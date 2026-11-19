;;; code-insight-mcp-test.el --- Tests for code-insight-mcp  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'code-insight-mcp)

(ert-deftest code-insight-mcp-test-around-tool-binds-safe ()
  "Wrapped tools see only safe local variables applied."
  (should (eq (code-insight-mcp--around-tool
               (lambda () enable-local-variables))
              :safe)))

(ert-deftest code-insight-mcp-test-around-tool-skips-unsafe-file-locals ()
  "A file with an unsafe local variable opens without asking."
  ;; Split "Local Variables" so Emacs does not take it for this file's own
  (let* ((file (make-temp-file "code-insight-mcp" nil ".el"
                               (concat ";; Local " "Variables:\n"
                                       ";; code-insight-mcp-test-unsafe: 1\n"
                                       ";; End:\n")))
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'hack-local-variables-confirm)
                   (lambda (&rest _) (error "Asked about local variables"))))
          (setq buffer (code-insight-mcp--around-tool #'find-file-noselect file))
          (should-not (local-variable-p 'code-insight-mcp-test-unsafe buffer)))
      (when buffer (kill-buffer buffer))
      (delete-file file))))

(ert-deftest code-insight-mcp-test-drop-tools ()
  "The project info tool is removed and the others are served."
  (let ((claude-code-ide-mcp-server-tools nil))
    (claude-code-ide-emacs-tools-setup)
    (code-insight-mcp--drop-tools)
    (code-insight-mcp--setup-lsp-tools)
    (let ((functions (mapcar (lambda (spec) (plist-get spec :function))
                             claude-code-ide-mcp-server-tools)))
      (should-not (memq 'claude-code-ide-mcp-project-info functions))
      (dolist (f code-insight-mcp--tool-functions)
        (should (memq f functions))))))

(ert-deftest code-insight-mcp-test-rename-tools ()
  "The claude-code-ide tools get short names, once, even when set up again."
  (let ((claude-code-ide-mcp-server-tools nil))
    (dotimes (_ 2)
      (claude-code-ide-emacs-tools-setup)
      (code-insight-mcp--drop-tools)
      (code-insight-mcp--rename-tools)
      (code-insight-mcp--setup-lsp-tools))
    (let ((names (mapcar (lambda (spec) (plist-get spec :name))
                         claude-code-ide-mcp-server-tools)))
      (should (equal (sort (copy-sequence names) #'string<)
                     '("imenu_symbols" "lsp_call_hierarchy" "lsp_definition"
                       "lsp_diagnostics" "lsp_hover" "lsp_implementation"
                       "lsp_references" "lsp_workspace_symbol" "treesit_info"
                       "xref_apropos" "xref_references"))))))

(ert-deftest code-insight-mcp-test-setup-replaces-own-tools ()
  "Setting the tools up again replaces this package's tools only."
  (let ((claude-code-ide-mcp-server-tools nil))
    (claude-code-ide-emacs-tools-setup)
    (code-insight-mcp--setup-lsp-tools)
    ;; As if a description had changed since
    (push (list :function #'code-insight-mcp-lsp-hover :name "lsp_hover"
                :description "Old")
          claude-code-ide-mcp-server-tools)
    (code-insight-mcp--setup-lsp-tools)
    (should (= (seq-count (lambda (spec)
                            (equal (plist-get spec :name) "lsp_hover"))
                          claude-code-ide-mcp-server-tools)
               1))
    (should (seq-find (lambda (spec)
                        (eq (plist-get spec :function)
                            'claude-code-ide-mcp-imenu-list-symbols))
                      claude-code-ide-mcp-server-tools))))

(ert-deftest code-insight-mcp-test-strip-hover-source ()
  "jdtls's trailing Source link is removed, other text kept."
  (should (equal (code-insight-mcp--strip-hover-source
                  "```java\nint f()\n```\n\nDoc.\n\nSource: *[app](file:///a/B.java#44)*")
                 "```java\nint f()\n```\n\nDoc."))
  (should (equal (code-insight-mcp--strip-hover-source "Doc.\n") "Doc.\n"))
  (should-not (code-insight-mcp--strip-hover-source
               "Source: *[app](file:///a/B.java#44)*"))
  (should-not (code-insight-mcp--strip-hover-source nil)))

(ert-deftest code-insight-mcp-test-goto ()
  "Positions are found by symbol, by column or by indentation."
  (with-temp-buffer
    (insert "first\n    int foo = bar(foo);\n")
    (code-insight-mcp--goto 2 "foo" nil)
    (should (= (current-column) 8))
    (code-insight-mcp--goto 2 "bar" 1)
    (should (= (current-column) 14))
    (code-insight-mcp--goto 2 nil 3)
    (should (= (current-column) 2))
    (code-insight-mcp--goto 2 nil nil)
    (should (= (current-column) 4))
    (should-error (code-insight-mcp--goto 2 "fo" nil))
    ;; Case matters, whatever `case-fold-search' says
    (erase-buffer)
    (insert "Widget WIDGET = Widget.get();\n")
    (let ((case-fold-search t))
      (code-insight-mcp--goto 1 "WIDGET" nil))
    (should (= (current-column) 7))
    (should-error (code-insight-mcp--goto 9 nil nil))))

(ert-deftest code-insight-mcp-test-hover-text-strings ()
  "Plain strings and vectors of them are joined, empty ones dropped."
  (should (equal (code-insight-mcp--hover-text "doc") "doc"))
  (should (equal (code-insight-mcp--hover-text ["a" "" "b"]) "a\n\nb"))
  (should-not (code-insight-mcp--hover-text ""))
  (should-not (code-insight-mcp--hover-text [])))

(defun code-insight-mcp-test--rewrite (file text)
  "Write TEXT to FILE behind its buffer's back, with a later mtime."
  (with-temp-file file (insert text))
  (set-file-times file (time-add (current-time) 10)))

(ert-deftest code-insight-mcp-test-revert-stale-buffers ()
  "Unmodified stale buffers are reverted, modified ones are left alone."
  (let* ((clean-file (make-temp-file "code-insight-mcp" nil ".txt" "old\n"))
         (edited-file (make-temp-file "code-insight-mcp" nil ".txt" "old\n"))
         (clean (find-file-noselect clean-file))
         (edited (find-file-noselect edited-file)))
    (unwind-protect
        (progn
          (with-current-buffer edited (insert "mine "))
          (code-insight-mcp-test--rewrite clean-file "new\n")
          (code-insight-mcp-test--rewrite edited-file "new\n")
          (code-insight-mcp--revert-stale-buffers)
          (should (equal (with-current-buffer clean (buffer-string)) "new\n"))
          (should (equal (with-current-buffer edited (buffer-string)) "mine old\n")))
      (with-current-buffer edited (set-buffer-modified-p nil))
      (kill-buffer clean)
      (kill-buffer edited)
      (delete-file clean-file)
      (delete-file edited-file))))

(ert-deftest code-insight-mcp-test-location-list ()
  "Definition responses become lists of locations."
  (should-not (code-insight-mcp--location-list nil))
  (should (equal (code-insight-mcp--location-list '(:uri "a")) '((:uri "a"))))
  (should (equal (code-insight-mcp--location-list [(:uri "a") (:uri "b")])
                 '((:uri "a") (:uri "b"))))
  ;; Without `lsp-use-plists', arrays are lists of hash tables
  (let ((a (make-hash-table :test #'equal))
        (b (make-hash-table :test #'equal)))
    (should (equal (code-insight-mcp--location-list (list a b)) (list a b)))
    (should (equal (code-insight-mcp--location-list a) (list a)))))

(ert-deftest code-insight-mcp-test-line-text ()
  "A line is read from the file, trimmed."
  (let ((file (make-temp-file "code-insight-mcp" nil ".txt" "one\n   two  \n")))
    (unwind-protect
        (progn
          (should (equal (code-insight-mcp--line-text file 2) "two"))
          (should (equal (code-insight-mcp--line-text file 9) ""))
          ;; While a tool runs, a file is read once
          (let ((code-insight-mcp--file-lines (make-hash-table :test #'equal)))
            (should (equal (code-insight-mcp--line-text file 1) "one"))
            (with-temp-file file (insert "changed\n"))
            (should (equal (code-insight-mcp--line-text file 2) "two"))))
      (delete-file file))))

(ert-deftest code-insight-mcp-test-format-diagnostic ()
  "A diagnostic is one line with its position, severity and source."
  (require 'lsp-protocol)
  (let ((diagnostic (lsp-make-diagnostic
                     :range (lsp-make-range
                             :start (lsp-make-position :line 5 :character 15)
                             :end (lsp-make-position :line 5 :character 17))
                     :severity 1
                     :message "Type mismatch\n"
                     :source "Java"
                     :code "16777233")))
    (should (equal (code-insight-mcp--format-diagnostic "/a/B.java" diagnostic)
                   "/a/B.java:6:16: error: Type mismatch [Java 16777233]"))))

(ert-deftest code-insight-mcp-test-format-symbol ()
  "A symbol is one line with its position, kind, name and container."
  (require 'lsp-mode)
  (let ((range (lsp-make-range
                :start (lsp-make-position :line 2 :character 13)
                :end (lsp-make-position :line 2 :character 19))))
    (should (equal (code-insight-mcp--format-symbol
                    (lsp-make-symbol-information
                     :name "Widget" :kind 5 :containerName "com.example"
                     :location (lsp-make-location :uri "file:///a/Widget.java"
                                                  :range range)))
                   "/a/Widget.java:3:14: Class Widget (in com.example)"))
    (should (equal (code-insight-mcp--format-symbol
                    (lsp-make-symbol-information
                     :name "main" :kind 6 :containerName ""
                     :location (lsp-make-location :uri "file:///a/Main.java"
                                                  :range range)))
                   "/a/Main.java:3:14: Method main"))))

(ert-deftest code-insight-mcp-test-diagnostics-fresh-p ()
  "Diagnostics are fresh while the buffer is as it was when they came."
  (let ((code-insight-mcp--diagnostics-ticks (make-hash-table :test #'equal)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/code-insight-mcp-test.java")
      (should-not (code-insight-mcp--diagnostics-fresh-p))
      (puthash buffer-file-name (buffer-chars-modified-tick)
               code-insight-mcp--diagnostics-ticks)
      (should (code-insight-mcp--diagnostics-fresh-p))
      (insert "x")
      (should-not (code-insight-mcp--diagnostics-fresh-p))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))))

(ert-deftest code-insight-mcp-test-url ()
  "The URL is built from the port and the session ID."
  (let ((code-insight-mcp-port 7788)
        (code-insight-mcp-session-id "test"))
    (cl-letf (((symbol-function 'claude-code-ide-mcp-server-get-port)
               (lambda () nil)))
      (should (equal (code-insight-mcp-url) "http://localhost:7788/mcp/test")))))

(provide 'code-insight-mcp-test)
;;; code-insight-mcp-test.el ends here
