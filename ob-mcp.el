;;; ob-mcp.el --- Org Babel support for the Model Context Protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Niall FitzGibbon

;; Author: Niall FitzGibbon <niall.fitzgibbon@finitestate.io>
;; Maintainer: Niall FitzGibbon <niall.fitzgibbon@finitestate.io>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (mcp "0"))
;; Keywords: tools, org, mcp, languages
;; URL: https://github.com/nfitzgibbon/ob-mcp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ob-mcp adds Model Context Protocol (MCP) support to Org Babel.  The
;; client itself is provided by mcp.el (https://github.com/lizqwerscott/mcp.el),
;; which handles the stdio and HTTP/SSE transports, OAuth bearer tokens
;; and capability negotiation.  ob-mcp is the thin Org Babel layer on top:
;; it translates source blocks into mcp.el calls and renders the results.
;;
;; Source blocks can:
;;
;;   * list configured MCP servers,
;;   * list and describe a server's tools/functions, and call them,
;;   * list and read a server's resources, and
;;   * list, describe and render a server's prompts.
;;
;; ob-mcp adds no configuration of its own: servers are the ones you have
;; already defined in mcp.el's `mcp-hub-servers', and everything else is
;; driven by source-block header arguments.  Just enable the backend:
;;
;;   (require 'ob-mcp)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages '((mcp . t)))
;;
;; See the mcp.el documentation for `mcp-hub-servers'; in brief a
;; `:command' entry uses the stdio transport and a `:url' entry uses HTTP
;; (with optional `:token'/`:headers' for authorization):
;;
;;   (setq mcp-hub-servers
;;         '(("filesystem"
;;            :command "npx"
;;            :args ("-y" "@modelcontextprotocol/server-filesystem" "/tmp"))
;;           ("remote"
;;            :url "https://api.example.com/mcp"
;;            :token "s3cr3t")))
;;
;; Source block grammar.  The first line is "COMMAND [ARGUMENT]"; any
;; remaining lines form a JSON object used as call/prompt arguments.
;; The `:server' header selects the server; `:async t' runs the request
;; without blocking Emacs and fills the result in when it arrives.
;;
;;   #+begin_src mcp
;;   list-servers
;;   #+end_src
;;
;;   #+begin_src mcp :server filesystem
;;   list-tools
;;   #+end_src
;;
;;   #+begin_src mcp :server filesystem :async t
;;   call read_file
;;   {"path": "/etc/hosts"}
;;   #+end_src
;;
;;   #+begin_src mcp :server filesystem
;;   read-resource file:///etc/hosts
;;   #+end_src
;;
;;   #+begin_src mcp :server assistant
;;   get-prompt summarize
;;   {"topic": "octopuses"}
;;   #+end_src

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mcp)
(require 'mcp-hub)

(declare-function org-babel-where-is-src-block-head "ob-core" (&optional src-block))
(declare-function org-babel-insert-result "ob-core"
                  (result &optional result-params info hash lang exec-time))

;;;; Connections
;;
;; ob-mcp has no configuration of its own.  Servers come from mcp.el's
;; `mcp-hub-servers' and connecting is delegated to `mcp-hub-start-all-server'.

(defun ob-mcp-connection (name)
  "Return a connected mcp.el connection for server NAME, connecting if needed.
Servers are configured in mcp.el's `mcp-hub-servers'."
  (unless (assoc name mcp-hub-servers)
    (error "ob-mcp: no server named %S in `mcp-hub-servers'" name))
  (unless (mcp--server-running-p name)
    (mcp-hub-start-all-server nil (list name) t))
  (let ((conn (gethash name mcp-server-connections)))
    (unless (and conn (eq (mcp--status conn) 'connected))
      (error "ob-mcp: could not connect to server %S" name))
    conn))

(defun ob-mcp-disconnect (name)
  "Terminate the connection to server NAME, if any."
  (interactive
   (list (completing-read "Disconnect MCP server: "
                          (hash-table-keys mcp-server-connections) nil t)))
  (mcp-stop-server name))

(defun ob-mcp-disconnect-all ()
  "Terminate every open MCP connection."
  (interactive)
  (dolist (name (hash-table-keys mcp-server-connections))
    (mcp-stop-server name)))

;;;; Public operations

(defun ob-mcp--as-list (sequence)
  "Coerce SEQUENCE (mcp.el returns JSON arrays as vectors) to a list."
  (append sequence nil))

(defun ob-mcp-list-tools (name)
  "Return the list of tool plists exposed by server NAME."
  (ob-mcp--as-list (mcp--tools (ob-mcp-connection name))))

(defun ob-mcp-list-resources (name)
  "Return the list of resource plists exposed by server NAME."
  (ob-mcp--as-list (mcp--resources (ob-mcp-connection name))))

(defun ob-mcp-list-prompts (name)
  "Return the list of prompt plists exposed by server NAME."
  (ob-mcp--as-list (mcp--prompts (ob-mcp-connection name))))

;;;; Result rendering

(defun ob-mcp--first-line (string)
  "Return the first line of STRING, or an empty string for nil."
  (if (and string (stringp string))
      (car (split-string string "\n"))
    ""))

(defun ob-mcp--content-item-string (item)
  "Render one MCP content ITEM plist as a string."
  (pcase (plist-get item :type)
    ("text" (plist-get item :text))
    ("image" (format "[image: %s]" (plist-get item :mimeType)))
    ("audio" (format "[audio: %s]" (plist-get item :mimeType)))
    ("resource" (format "[resource: %s]"
                        (plist-get (plist-get item :resource) :uri)))
    ("resource_link" (format "[resource: %s]" (plist-get item :uri)))
    (other (format "[%s]" other))))

(defun ob-mcp--content-to-string (result)
  "Render an MCP tool-call RESULT plist into a string, erroring if it failed."
  (let ((text (string-join
               (delq nil (mapcar #'ob-mcp--content-item-string
                                 (ob-mcp--as-list (plist-get result :content))))
               "\n")))
    (if (eq (plist-get result :isError) t)
        (error "ob-mcp: tool reported an error: %s" text)
      text)))

(defun ob-mcp--resource-to-string (result)
  "Render a resources/read RESULT plist into a string."
  (string-join
   (mapcar (lambda (c)
             (cond ((plist-get c :text) (plist-get c :text))
                   ((plist-get c :blob)
                    (format "[binary resource %s: %s]"
                            (plist-get c :uri)
                            (or (plist-get c :mimeType) "application/octet-stream")))
                   (t (format "[resource %s]" (plist-get c :uri)))))
           (ob-mcp--as-list (plist-get result :contents)))
   "\n"))

(defun ob-mcp--prompt-to-string (result)
  "Render a prompts/get RESULT plist into role-tagged text."
  (string-join
   (mapcar (lambda (m)
             (format "%s: %s"
                     (plist-get m :role)
                     (ob-mcp--content-item-string (plist-get m :content))))
           (ob-mcp--as-list (plist-get result :messages)))
   "\n\n"))

(defun ob-mcp--servers-table ()
  "Return an Org table describing all servers in `mcp-hub-servers'."
  (cons '("Server" "Transport" "Endpoint" "Connected")
        (cons 'hline
              (mapcar
               (lambda (entry)
                 (let* ((name (car entry))
                        (spec (cdr entry))
                        (url (plist-get spec :url)))
                   (list name
                         (if url "http" "stdio")
                         (or url (string-join (cons (plist-get spec :command)
                                                    (plist-get spec :args))
                                              " "))
                         (if (mcp--server-running-p name) "yes" "no"))))
               mcp-hub-servers))))

(defun ob-mcp--tools-table (tools)
  "Return an Org table of TOOLS (a list of tool plists)."
  (cons '("Tool" "Description")
        (cons 'hline
              (mapcar (lambda (tl)
                        (list (plist-get tl :name)
                              (ob-mcp--first-line (plist-get tl :description))))
                      tools))))

(defun ob-mcp--describe-table (tools tool)
  "Return an Org table describing TOOL's parameters, found in TOOLS."
  (let* ((spec (or (seq-find (lambda (tl) (equal (plist-get tl :name) tool)) tools)
                   (error "ob-mcp: no tool named %S" tool)))
         (schema (plist-get spec :inputSchema))
         (required (ob-mcp--as-list (plist-get schema :required)))
         (rows nil))
    (cl-loop for (key value) on (plist-get schema :properties) by #'cddr
             for pname = (substring (symbol-name key) 1)
             do (push (list pname
                            (or (plist-get value :type) "")
                            (if (member pname required) "yes" "no")
                            (ob-mcp--first-line (plist-get value :description)))
                      rows))
    (cons '("Parameter" "Type" "Required" "Description")
          (cons 'hline (nreverse rows)))))

(defun ob-mcp--resources-table (resources)
  "Return an Org table of RESOURCES (a list of resource plists)."
  (cons '("URI" "Name" "Description")
        (cons 'hline
              (mapcar (lambda (r)
                        (list (plist-get r :uri)
                              (or (plist-get r :name) "")
                              (ob-mcp--first-line (plist-get r :description))))
                      resources))))

(defun ob-mcp--prompts-table (prompts)
  "Return an Org table of PROMPTS (a list of prompt plists)."
  (cons '("Prompt" "Description")
        (cons 'hline
              (mapcar (lambda (p)
                        (list (plist-get p :name)
                              (ob-mcp--first-line (plist-get p :description))))
                      prompts))))

(defun ob-mcp--describe-prompt-table (prompts prompt)
  "Return an Org table describing PROMPT's arguments, found in PROMPTS."
  (let ((spec (or (seq-find (lambda (p) (equal (plist-get p :name) prompt)) prompts)
                  (error "ob-mcp: no prompt named %S" prompt))))
    (cons '("Argument" "Required" "Description")
          (cons 'hline
                (mapcar (lambda (a)
                          (list (plist-get a :name)
                                (if (eq (plist-get a :required) t) "yes" "no")
                                (ob-mcp--first-line (plist-get a :description))))
                        (ob-mcp--as-list (plist-get spec :arguments)))))))

;;;; Body parsing

(defun ob-mcp--parse-body (body)
  "Split source-block BODY into (COMMAND ARGUMENT PAYLOAD).
COMMAND is the down-cased first token, ARGUMENT the second token (or
nil), and PAYLOAD the trimmed remainder after the first line (or nil)."
  (let* ((trimmed (string-trim body))
         (newline (string-search "\n" trimmed))
         (first-line (if newline (substring trimmed 0 newline) trimmed))
         (rest (when newline (string-trim (substring trimmed (1+ newline)))))
         (tokens (split-string first-line "[ \t]+" t)))
    (when (null tokens)
      (error "ob-mcp: empty source block; expected a command"))
    (list (downcase (car tokens))
          (cadr tokens)
          (and rest (not (string-empty-p rest)) rest))))

(defun ob-mcp--require-server (server command)
  "Return SERVER or signal that COMMAND needs a `:server' header."
  (or server
      (error "ob-mcp: `%s' requires a :server header argument" command)))

(defun ob-mcp--need (value command what)
  "Signal an error unless VALUE is non-nil; COMMAND needs WHAT."
  (unless value
    (error "ob-mcp: `%s' requires %s" command what))
  value)

(defun ob-mcp--parse-arguments (payload args-header)
  "Decode call/prompt arguments from PAYLOAD or the ARGS-HEADER string.
Returns a plist (nil when empty; mcp.el then sends an empty object).
Booleans and null use mcp.el's JSON conventions so they re-serialize."
  (let ((json (or payload (and args-header (format "%s" args-header)))))
    (when (and json (not (string-empty-p (string-trim json))))
      (json-parse-string json
                         :object-type 'plist
                         :array-type 'array
                         :null-object :json-null
                         :false-object :json-false))))

(defun ob-mcp--async-requested-p (params)
  "Return non-nil when PARAMS request asynchronous execution."
  (let ((value (cdr (assq :async params))))
    (and value (not (member (downcase (format "%s" value))
                            '("no" "nil" "false" ""))))))

;;;; Asynchronous Org Babel results

(defun ob-mcp--async-insert (buffer marker result-params value)
  "Replace the placeholder result at MARKER in BUFFER with VALUE."
  (when (and (buffer-live-p buffer) (marker-buffer marker))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (org-babel-insert-result value result-params)))))

(defun ob-mcp--execute-async (starter render bparams)
  "Run STARTER asynchronously and insert (RENDER result) when it lands.
STARTER is (lambda (ON-RESULT ON-ERROR)) where ON-RESULT takes a result
plist and ON-ERROR takes (CODE MESSAGE).  BPARAMS are the block's header
arguments.  Returns a placeholder shown until the real result arrives."
  (let ((head (org-babel-where-is-src-block-head)))
    (unless head
      (error "ob-mcp: :async requires execution from within an Org source block"))
    (let ((buffer (current-buffer))
          (marker (copy-marker head t))
          (result-params (or (cdr (assq :result-params bparams)) '("replace"))))
      (funcall starter
               (lambda (result)
                 (ob-mcp--async-insert
                  buffer marker result-params
                  (condition-case e (funcall render result)
                    (error (concat "ob-mcp error: " (error-message-string e))))))
               (lambda (code message)
                 (ob-mcp--async-insert
                  buffer marker result-params
                  (format "ob-mcp error: %s %s" code message))))
      "ob-mcp: pending…")))

(defun ob-mcp--run (params sync-fn async-starter render)
  "Render an operation synchronously, or asynchronously when PARAMS ask.
SYNC-FN returns a result plist; ASYNC-STARTER is (lambda (ON-RESULT ON-ERROR))."
  (if (ob-mcp--async-requested-p params)
      (ob-mcp--execute-async async-starter render params)
    (funcall render (funcall sync-fn))))

;;;; Org Babel entry point

;;;###autoload
(defun org-babel-execute:mcp (body params)
  "Execute an `mcp' Org Babel source block.
BODY holds the command; PARAMS the header arguments.  See the
ob-mcp Commentary for the source-block grammar."
  (pcase-let* ((`(,command ,argument ,payload) (ob-mcp--parse-body body))
               (server (cdr (assq :server params)))
               (args-header (cdr (assq :args params))))
    (pcase command
      ((or "list-servers" "servers")
       (ob-mcp--servers-table))
      ((or "list-tools" "tools" "list-functions" "functions")
       (ob-mcp--tools-table
        (ob-mcp-list-tools (ob-mcp--require-server server command))))
      ((or "describe" "describe-tool")
       (ob-mcp--need argument command "a tool name")
       (ob-mcp--describe-table
        (ob-mcp-list-tools (ob-mcp--require-server server command)) argument))
      ((or "list-resources" "resources")
       (ob-mcp--resources-table
        (ob-mcp-list-resources (ob-mcp--require-server server command))))
      ((or "list-prompts" "prompts")
       (ob-mcp--prompts-table
        (ob-mcp-list-prompts (ob-mcp--require-server server command))))
      ((or "describe-prompt")
       (ob-mcp--need argument command "a prompt name")
       (ob-mcp--describe-prompt-table
        (ob-mcp-list-prompts (ob-mcp--require-server server command)) argument))
      ((or "call" "call-tool")
       (ob-mcp--need argument command "a tool name")
       (let ((conn (ob-mcp-connection (ob-mcp--require-server server command)))
             (args (ob-mcp--parse-arguments payload args-header)))
         (ob-mcp--run params
                      (lambda () (mcp-call-tool conn argument args))
                      (lambda (ok err) (mcp-async-call-tool conn argument args ok err))
                      #'ob-mcp--content-to-string)))
      ((or "read-resource" "read")
       (ob-mcp--need argument command "a resource URI")
       (let ((conn (ob-mcp-connection (ob-mcp--require-server server command))))
         (ob-mcp--run params
                      (lambda () (mcp-read-resource conn argument))
                      (lambda (ok err) (mcp-async-read-resource conn argument ok err))
                      #'ob-mcp--resource-to-string)))
      ((or "get-prompt" "prompt")
       (ob-mcp--need argument command "a prompt name")
       (let ((conn (ob-mcp-connection (ob-mcp--require-server server command)))
             (args (ob-mcp--parse-arguments payload args-header)))
         (ob-mcp--run params
                      (lambda () (mcp-get-prompt conn argument args))
                      (lambda (ok err) (mcp-async-get-prompt conn argument args ok err))
                      #'ob-mcp--prompt-to-string)))
      (_ (error (concat "ob-mcp: unknown command %S (try list-servers, "
                        "list-tools, describe, call, list-resources, "
                        "read-resource, list-prompts, get-prompt)")
                command)))))

;;;###autoload
(defun org-babel-prep-session:mcp (_session _params)
  "Sessions are not supported for `mcp' source blocks."
  (error "ob-mcp: source blocks do not support sessions"))

(provide 'ob-mcp)
;;; ob-mcp.el ends here
