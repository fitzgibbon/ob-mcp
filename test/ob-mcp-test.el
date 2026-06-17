;;; ob-mcp-test.el --- Tests for ob-mcp -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -Q -L . -l test/ob-mcp-test.el -f ert-run-tests-batch-and-exit
;;
;; The client layer is mcp.el; these tests drive ob-mcp end to end through
;; it, talking to a self-contained stdio mock server (test/mock-mcp-server.el
;; over test/mcp-engine.el).  mcp.el's HTTP/SSE transport is exercised by
;; mcp.el's own suite, so here we only unit-test the spec translation for it.

;;; Code:

(require 'ert)
(require 'org)
(require 'ob-mcp)

(defconst ob-mcp-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun ob-mcp-test--stdio-spec ()
  "Server spec that launches the batch-mode mock stdio server."
  (list :command (or (executable-find "emacs") "emacs")
        :args (list "--batch" "-Q"
                    "-l" (expand-file-name "mcp-engine.el" ob-mcp-test--dir)
                    "-l" (expand-file-name "mock-mcp-server.el" ob-mcp-test--dir))))

(defmacro ob-mcp-test--with-stdio (&rest body)
  "Evaluate BODY with a single stdio mock server configured and torn down.
Servers live in mcp.el's `mcp-hub-servers'.  The post-init settle delay
is zeroed to keep connects fast."
  (declare (indent 0))
  `(let ((mcp-hub-servers (list (cons "mock" (ob-mcp-test--stdio-spec))))
         (mcp-server-wait-initial-time 0))
     (unwind-protect (progn ,@body)
       (ob-mcp-disconnect-all))))

;;;; Unit tests (no connection)

(ert-deftest ob-mcp-test-parse-body ()
  (should (equal '("list-servers" nil nil) (ob-mcp--parse-body "list-servers")))
  (should (equal '("list-tools" nil nil) (ob-mcp--parse-body "  list-tools  ")))
  (should (equal '("describe" "read_file" nil) (ob-mcp--parse-body "describe read_file")))
  (should (equal '("call" "echo" "{\"text\": \"hi\"}")
                 (ob-mcp--parse-body "call echo\n{\"text\": \"hi\"}"))))

(ert-deftest ob-mcp-test-async-requested-p ()
  (should (ob-mcp--async-requested-p '((:async . "t"))))
  (should (ob-mcp--async-requested-p '((:async . yes))))
  (should-not (ob-mcp--async-requested-p '((:async . "no"))))
  (should-not (ob-mcp--async-requested-p nil)))

(ert-deftest ob-mcp-test-unknown-server-errors ()
  (let ((mcp-hub-servers nil))
    (should-error (ob-mcp-connection "nope"))))

;;;; Client layer through mcp.el

(ert-deftest ob-mcp-test-connect-and-tools ()
  (ob-mcp-test--with-stdio
    (let ((conn (ob-mcp-connection "mock")))
      (should (eq 'connected (mcp--status conn)))
      (should (equal "mock-mcp" (plist-get (mcp--server-info conn) :name))))
    (should (member "echo" (mapcar (lambda (tl) (plist-get tl :name))
                                   (ob-mcp-list-tools "mock"))))
    (let ((conn (ob-mcp-connection "mock")))
      (should (equal "hi" (ob-mcp--content-to-string
                           (mcp-call-tool conn "echo" '(:text "hi")))))
      (should (equal "7" (ob-mcp--content-to-string
                          (mcp-call-tool conn "add" '(:a 3 :b 4))))))))

(ert-deftest ob-mcp-test-resources-and-prompts ()
  (ob-mcp-test--with-stdio
    (should (member "mem://greeting"
                    (mapcar (lambda (r) (plist-get r :uri))
                            (ob-mcp-list-resources "mock"))))
    (should (equal "Hello from MCP!"
                   (ob-mcp--resource-to-string
                    (mcp-read-resource (ob-mcp-connection "mock") "mem://greeting"))))
    (should (member "greet" (mapcar (lambda (p) (plist-get p :name))
                                    (ob-mcp-list-prompts "mock"))))
    (should (string-match-p "Hello, Ada!"
                            (ob-mcp--prompt-to-string
                             (mcp-get-prompt (ob-mcp-connection "mock") "greet" '(:who "Ada")))))))

;;;; Org Babel dispatch (synchronous), reusing one connection

(ert-deftest ob-mcp-test-execute-commands ()
  (ob-mcp-test--with-stdio
    (let ((srv '((:server . "mock"))))
      (let ((table (org-babel-execute:mcp "list-servers" nil)))
        (should (equal '("Server" "Transport" "Endpoint" "Connected") (car table)))
        (should (assoc "mock" (cddr table))))
      (should (assoc "echo" (cddr (org-babel-execute:mcp "list-tools" srv))))
      (let ((d (org-babel-execute:mcp "describe add" srv)))
        (should (equal '("Parameter" "Type" "Required" "Description") (car d)))
        (should (equal "yes" (nth 2 (assoc "a" (cddr d))))))
      (should (equal "echoed!"
                     (org-babel-execute:mcp "call echo\n{\"text\": \"echoed!\"}" srv)))
      (should (assoc "mem://greeting"
                     (cddr (org-babel-execute:mcp "list-resources" srv))))
      (should (equal "Hello from MCP!"
                     (org-babel-execute:mcp "read-resource mem://greeting" srv)))
      (let ((p (org-babel-execute:mcp "describe-prompt greet" srv)))
        (should (equal '("Argument" "Required" "Description") (car p)))
        (should (equal "yes" (nth 1 (assoc "who" (cddr p))))))
      (should (string-match-p
               "Hello, Zoe!"
               (org-babel-execute:mcp "get-prompt greet\n{\"who\": \"Zoe\"}" srv))))))

(ert-deftest ob-mcp-test-execute-errors ()
  (ob-mcp-test--with-stdio
    (should-error (org-babel-execute:mcp "call echo\n{}" nil))      ; no :server
    (should-error (org-babel-execute:mcp "frobnicate" nil))))       ; unknown command

;;;; End-to-end blocks

(ert-deftest ob-mcp-test-execute-end-to-end-block ()
  (ob-mcp-test--with-stdio
    (with-temp-buffer
      (org-mode)
      (setq-local org-confirm-babel-evaluate nil)
      (insert "#+begin_src mcp :server mock :results silent\n"
              "call add\n{\"a\": 40, \"b\": 2}\n"
              "#+end_src\n")
      (goto-char (point-min))
      (should (equal "42" (org-babel-execute-src-block))))))

(defun ob-mcp-test--await-result (regexp &optional timeout)
  "Pump the event loop until REGEXP appears in the buffer or TIMEOUT."
  (let ((deadline (+ (float-time) (or timeout 15))))
    (while (and (not (save-excursion (goto-char (point-min))
                                     (re-search-forward regexp nil t)))
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (save-excursion (goto-char (point-min)) (re-search-forward regexp nil t))))

(ert-deftest ob-mcp-test-async-call ()
  (ob-mcp-test--with-stdio
    (with-temp-buffer
      (org-mode)
      (setq-local org-confirm-babel-evaluate nil)
      (insert "#+begin_src mcp :server mock :async t\n"
              "call add\n{\"a\": 20, \"b\": 22}\n"
              "#+end_src\n")
      (goto-char (point-min))
      (should (equal "ob-mcp: pending…" (org-babel-execute-src-block)))
      (should (ob-mcp-test--await-result "^: 42$")))))

(provide 'ob-mcp-test)
;;; ob-mcp-test.el ends here
