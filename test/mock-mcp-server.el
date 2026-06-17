;;; mock-mcp-server.el --- A tiny stdio MCP server for tests -*- lexical-binding: t; -*-

;; A self-contained Model Context Protocol server used by the ob-mcp
;; test suite.  Run it in batch mode:
;;
;;   emacs --batch -l test/mcp-engine.el -l test/mock-mcp-server.el
;;
;; It speaks JSON-RPC 2.0 over stdio using newline-delimited messages and
;; delegates the actual method handling to `mcp-engine'.

;;; Code:

(require 'cl-lib)
(require 'mcp-engine (expand-file-name
                      "mcp-engine.el"
                      (file-name-directory (or load-file-name buffer-file-name))))

(defun mock-mcp--encode (value)
  "Encode VALUE to a single-line JSON string."
  (json-serialize value :null-object nil :false-object :false))

(defun mock-mcp--decode (string)
  "Decode JSON STRING into a plist."
  (json-parse-string string :object-type 'plist :array-type 'list
                     :null-object nil :false-object :false))

(defun mock-mcp--reply (id result)
  "Print a JSON-RPC success reply for ID carrying RESULT."
  (princ (concat (mock-mcp--encode (list :jsonrpc "2.0" :id id :result result))
                 "\n")))

(defun mock-mcp--error (id code message)
  "Print a JSON-RPC error reply for ID with CODE and MESSAGE."
  (princ (concat (mock-mcp--encode
                  (list :jsonrpc "2.0" :id id
                        :error (list :code code :message message)))
                 "\n")))

(defun mock-mcp--dispatch (msg)
  "Handle one decoded JSON-RPC MSG."
  (let ((id (plist-get msg :id))
        (method (plist-get msg :method))
        (params (plist-get msg :params)))
    (when id ; requests carry an id; notifications are ignored
      (condition-case _
          (mock-mcp--reply id (mcp-engine-result method params))
        (mcp-engine-unknown-method
         (mock-mcp--error id -32601 (format "method not found: %s" method)))))))

(defun mock-mcp-main ()
  "Read newline-delimited JSON-RPC from stdin until EOF."
  (let (line)
    (while (setq line (ignore-errors (read-string "")))
      (let ((trimmed (string-trim line)))
        (unless (string-empty-p trimmed)
          (condition-case err
              (mock-mcp--dispatch (mock-mcp--decode trimmed))
            (error (message "mock-mcp dispatch error: %s"
                            (error-message-string err)))))))))

(mock-mcp-main)

;;; mock-mcp-server.el ends here
