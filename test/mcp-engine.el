;;; mcp-engine.el --- Shared MCP server logic for tests -*- lexical-binding: t; -*-

;; A transport-independent MCP "engine" used by the test servers.  It
;; maps a JSON-RPC METHOD plus PARAMS to a result plist and exposes a
;; couple of tools, a resource, and a prompt.  Loading this file has no
;; side effects; the stdio and HTTP test servers drive it.

;;; Code:

(require 'cl-lib)

(define-error 'mcp-engine-unknown-method "Unknown MCP method")

(defconst mcp-engine-tools
  (list
   (list :name "echo"
         :description "Echo back the provided text."
         :inputSchema (list :type "object"
                            :properties (list :text (list :type "string"
                                                          :description "Text to echo."))
                            :required ["text"]))
   (list :name "add"
         :description "Add two numbers and return the sum."
         :inputSchema (list :type "object"
                            :properties (list :a (list :type "number")
                                              :b (list :type "number"))
                            :required ["a" "b"])))
  "Tools advertised by the test servers.")

(defconst mcp-engine-resources
  (list (list :uri "mem://greeting"
              :name "greeting"
              :description "A friendly greeting."
              :mimeType "text/plain"))
  "Resources advertised by the test servers.")

(defconst mcp-engine-prompts
  (list (list :name "greet"
              :description "Greet someone by name."
              :arguments (vector (list :name "who"
                                       :description "Who to greet."
                                       :required t))))
  "Prompts advertised by the test servers.")

(defun mcp-engine--text-result (string)
  "Wrap STRING as an MCP tool-call result."
  (list :content (vector (list :type "text" :text string)) :isError :false))

(defun mcp-engine--call (name arguments)
  "Dispatch a tools/call for NAME with ARGUMENTS (a plist)."
  (pcase name
    ("echo" (mcp-engine--text-result (or (plist-get arguments :text) "")))
    ("add" (mcp-engine--text-result
            (number-to-string (+ (plist-get arguments :a)
                                 (plist-get arguments :b)))))
    (_ (list :content (vector (list :type "text"
                                    :text (format "unknown tool: %s" name)))
             :isError t))))

(defun mcp-engine--read (uri)
  "Dispatch a resources/read for URI."
  (pcase uri
    ("mem://greeting"
     (list :contents (vector (list :uri uri :mimeType "text/plain"
                                   :text "Hello from MCP!"))))
    (_ (list :contents (vector (list :uri uri :mimeType "text/plain"
                                     :text (format "no such resource: %s" uri)))))))

(defun mcp-engine--prompt (name arguments)
  "Dispatch a prompts/get for NAME with ARGUMENTS (a plist)."
  (pcase name
    ("greet"
     (let ((who (or (plist-get arguments :who) "world")))
       (list :description "A greeting."
             :messages (vector (list :role "user"
                                     :content (list :type "text"
                                                    :text (format "Hello, %s!" who)))))))
    (_ (signal 'mcp-engine-unknown-method (list (format "prompt: %s" name))))))

(defun mcp-engine-result (method params)
  "Return the MCP result plist for METHOD with PARAMS.
Signal `mcp-engine-unknown-method' for unsupported methods."
  (pcase method
    ("initialize"
     ;; mcp.el only accepts protocol versions in `mcp--support-versions'.
     (list :protocolVersion "2025-03-26"
           :capabilities (list :tools (make-hash-table)
                               :resources (make-hash-table)
                               :prompts (make-hash-table))
           :serverInfo (list :name "mock-mcp" :version "0.1.0")))
    ("tools/list" (list :tools (apply #'vector mcp-engine-tools)))
    ("tools/call" (mcp-engine--call (plist-get params :name)
                                    (plist-get params :arguments)))
    ("resources/list" (list :resources (apply #'vector mcp-engine-resources)))
    ("resources/templates/list" (list :resourceTemplates []))
    ("resources/read" (mcp-engine--read (plist-get params :uri)))
    ("prompts/list" (list :prompts (apply #'vector mcp-engine-prompts)))
    ("prompts/get" (mcp-engine--prompt (plist-get params :name)
                                       (plist-get params :arguments)))
    (_ (signal 'mcp-engine-unknown-method (list method)))))

(provide 'mcp-engine)
;;; mcp-engine.el ends here
