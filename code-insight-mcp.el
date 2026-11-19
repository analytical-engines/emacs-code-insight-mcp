;;; code-insight-mcp.el --- Lend Emacs's code insight to any agent over MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tomohiro Miyasaka

;; Author: Tomohiro Miyasaka <tomohiro.miyasaka@analytical-engines.com>
;; URL: https://github.com/analytical-engines/emacs-code-insight-mcp
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (claude-code-ide "0.3.0"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

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

;; Emacs knows a lot about code: definitions, references, the symbols
;; of a file, syntax trees, projects.  This package keeps an MCP server
;; running in Emacs so that any agent (Claude Code, Codex, Gemini CLI
;; and so on) can ask Emacs about code and get answers back.  Agents do
;; not move the cursor or change what is on screen.
;;
;; The server and its tools come from claude-code-ide.  claude-code-ide
;; starts its tools server only for the Claude sessions it launches, on
;; a random port; this package starts it on a fixed port and registers
;; one session that never ends, so agents started anywhere can use it.
;;
;; Setup:
;;
;;   (require 'code-insight-mcp)
;;   (code-insight-mcp-mode 1)
;;
;; Then register the server with each agent once, for example
;;
;;   claude mcp add -s user -t http emacs-code-insight \
;;     http://localhost:7777/mcp/emacs
;;
;; While a tool runs, file local variables that are not known to be
;; safe are skipped instead of asking, so a tool never waits for an
;; answer in the minibuffer.
;;
;; Limitations:
;;
;; - claude-code-ide's project info tool is not served: it tells about
;;   the Claude session's project, which agents know better themselves.
;;   It is removed from `claude-code-ide-mcp-server-tools', so Claude
;;   sessions started from claude-code-ide lose it too.  Likewise the
;;   other claude-code-ide tools get short names there, such as
;;   imenu_symbols, for those sessions too.
;; - Tools open the files they look at, and the files their results
;;   point to, and leave them open, without their unsafe local
;;   variables.  A file already open is used as it is, so agents see
;;   unsaved changes.
;; - If a claude-code-ide session already started the server on another
;;   port, the server keeps that port until it stops.
;; - Any program on this machine can reach the port.  The tools only read.

;;; Code:

(require 'seq)
(require 'subr-x)
;; The Emacs Lisp xref backend's apropos needs `apropos-parse-pattern',
;; which `xref-find-apropos' loads but the tools do not
(require 'apropos)
(require 'claude-code-ide-mcp-server)
(require 'claude-code-ide-emacs-tools)

(defgroup code-insight-mcp nil
  "Lend Emacs's code insight to any agent over MCP."
  :group 'tools
  :prefix "code-insight-mcp-")

;;;; Customization

(defcustom code-insight-mcp-port 7777
  "Port the MCP server listens on.
Agents reach the server at http://localhost:PORT/mcp/SESSION-ID, where
SESSION-ID is `code-insight-mcp-session-id'."
  :type 'integer)

(defcustom code-insight-mcp-session-id "emacs"
  "Session ID registered for agents, the last part of the server URL."
  :type 'string)

(defcustom code-insight-mcp-session-directory "~"
  "Directory of the registered session.
Tools resolve relative file names against it."
  :type 'directory)

(defcustom code-insight-mcp-max-locations 200
  "Most locations a tool returns; the rest are counted but not listed."
  :type 'integer)

(defcustom code-insight-mcp-lsp-start-timeout 60
  "Seconds to wait for a language server that is starting.
Tools may start the server themselves, and some servers, such as
jdtls, take a while to be ready."
  :type 'number)

(defcustom code-insight-mcp-diagnostics-timeout 10
  "Seconds to wait for the language server to check a changed file.
When it has not answered by then, the diagnostics known so far are
returned, with a note that they may be out of date."
  :type 'number)

;;;; Tools

(defconst code-insight-mcp--tool-functions
  '(claude-code-ide-mcp-xref-find-references
    claude-code-ide-mcp-xref-find-apropos
    claude-code-ide-mcp-imenu-list-symbols
    claude-code-ide-mcp-treesit-info
    code-insight-mcp-lsp-hover
    code-insight-mcp-lsp-definition
    code-insight-mcp-lsp-references
    code-insight-mcp-lsp-diagnostics
    code-insight-mcp-lsp-workspace-symbol
    code-insight-mcp-lsp-implementation
    code-insight-mcp-lsp-call-hierarchy)
  "Functions of the tools served, wrapped by `code-insight-mcp--around-tool'.")

(defconst code-insight-mcp--dropped-tool-functions
  '(claude-code-ide-mcp-project-info)
  "Functions of claude-code-ide tools that are not served.
The project info tool tells about the Claude session's project, and the
session registered for agents has none.")

(defconst code-insight-mcp--tool-names
  '((claude-code-ide-mcp-imenu-list-symbols . "imenu_symbols")
    (claude-code-ide-mcp-treesit-info . "treesit_info")
    (claude-code-ide-mcp-xref-find-apropos . "xref_apropos")
    (claude-code-ide-mcp-xref-find-references . "xref_references"))
  "Short names the claude-code-ide tools are served under.
They match the names of this package's own tools.")

(defun code-insight-mcp--drop-tools ()
  "Remove the tools in `code-insight-mcp--dropped-tool-functions'."
  (setq claude-code-ide-mcp-server-tools
        (seq-remove (lambda (spec)
                      (memq (plist-get spec :function)
                            code-insight-mcp--dropped-tool-functions))
                    claude-code-ide-mcp-server-tools)))

(defun code-insight-mcp--rename-tools ()
  "Give the claude-code-ide tools the names in `code-insight-mcp--tool-names'.
Setting the tools up again adds them under their long names once more,
so duplicates left by renaming are removed."
  (setq claude-code-ide-mcp-server-tools
        (delete-dups
         (mapcar (lambda (spec)
                   (let ((name (alist-get (plist-get spec :function)
                                          code-insight-mcp--tool-names)))
                     (if name
                         (plist-put (copy-sequence spec) :name name)
                       spec)))
                 claude-code-ide-mcp-server-tools))))

(defun code-insight-mcp--revert-stale-buffers ()
  "Revert unmodified buffers whose files changed on disk.
Agents often change a file and ask about it right away, before
auto-revert has caught up.  Remote files are left alone, so that a
tool never waits for a connection."
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (not (file-remote-p (buffer-file-name buffer)))
               (not (buffer-modified-p buffer))
               (not (verify-visited-file-modtime buffer))
               (file-exists-p (buffer-file-name buffer)))
      (with-current-buffer buffer
        (revert-buffer t t t)))))

(defun code-insight-mcp--around-tool (fn &rest args)
  "Call FN with ARGS on current file contents, without asking anything.
Stale buffers are reverted first.  Only safe local variables are
applied in files FN opens or reverts."
  (let ((enable-local-variables :safe))
    (code-insight-mcp--revert-stale-buffers)
    (apply fn args)))

;;;; LSP tools

(declare-function lsp "lsp-mode")
(declare-function lsp-request "lsp-mode")
(declare-function lsp-session "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")
(declare-function lsp-feature? "lsp-mode")
(declare-function lsp--workspace-status "lsp-mode")
(declare-function lsp-find-session-folder "lsp-mode")
(declare-function lsp--text-document-position-params "lsp-mode")
(declare-function lsp:hover-contents "lsp-protocol")
(declare-function lsp-markup-content? "lsp-protocol")
(declare-function lsp:markup-content-value "lsp-protocol")
(declare-function lsp-marked-string? "lsp-protocol")
(declare-function lsp:marked-string-value "lsp-protocol")
(declare-function lsp:marked-string-language "lsp-protocol")
(declare-function lsp--uri-to-path "lsp-mode")
(declare-function lsp--make-reference-params "lsp-mode")
(declare-function lsp-diagnostics "lsp-mode")
(declare-function lsp:symbol-information-location "lsp-protocol")
(declare-function lsp:call-hierarchy-item-uri "lsp-protocol")
(declare-function lsp:call-hierarchy-item-selection-range "lsp-protocol")
(declare-function lsp:call-hierarchy-item-name "lsp-protocol")
(declare-function lsp:call-hierarchy-item-kind "lsp-protocol")
(declare-function lsp:call-hierarchy-item-detail? "lsp-protocol")
(declare-function lsp:call-hierarchy-incoming-call-from "lsp-protocol")
(declare-function lsp:call-hierarchy-incoming-call-from-ranges "lsp-protocol")
(declare-function lsp:call-hierarchy-outgoing-call-to "lsp-protocol")
(declare-function lsp:call-hierarchy-outgoing-call-from-ranges "lsp-protocol")
(declare-function lsp:symbol-information-container-name? "lsp-protocol")
(declare-function lsp:symbol-information-kind "lsp-protocol")
(declare-function lsp:symbol-information-name "lsp-protocol")
(defvar lsp-symbol-kinds)
(declare-function lsp--text-document-did-save "lsp-mode")
(declare-function lsp--fix-path-casing "lsp-mode")
(declare-function lsp:publish-diagnostics-params-uri "lsp-protocol")
(declare-function lsp:diagnostic-range "lsp-protocol")
(declare-function lsp:diagnostic-severity? "lsp-protocol")
(declare-function lsp:diagnostic-message "lsp-protocol")
(declare-function lsp:diagnostic-source? "lsp-protocol")
(declare-function lsp:diagnostic-code? "lsp-protocol")
(declare-function lsp-location-link? "lsp-protocol")
(declare-function lsp:location-link-target-uri "lsp-protocol")
(declare-function lsp:location-link-target-selection-range "lsp-protocol")
(declare-function lsp:location-uri "lsp-protocol")
(declare-function lsp:location-range "lsp-protocol")
(declare-function lsp:range-start "lsp-protocol")
(declare-function lsp:position-line "lsp-protocol")
(declare-function lsp:position-character "lsp-protocol")
(defvar lsp-mode)

(defun code-insight-mcp--file-buffer (file-path)
  "Return a buffer visiting FILE-PATH, opening it when needed.
A relative FILE-PATH is taken from `code-insight-mcp-session-directory'."
  (let ((file (expand-file-name file-path code-insight-mcp-session-directory)))
    (unless (file-exists-p file)
      (error "No such file: %s" file))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defun code-insight-mcp--lsp-ready-p (method)
  "Return non-nil if the buffer's language servers are ready for METHOD.
They are when every one is initialized and, unless METHOD is nil, one
can handle METHOD.  Servers such as jdtls register what they can
handle only after they are initialized."
  (let ((workspaces (lsp-workspaces)))
    (and workspaces
         (seq-every-p (lambda (w) (eq (lsp--workspace-status w) 'initialized))
                      workspaces)
         (or (null method) (lsp-feature? method)))))

(defun code-insight-mcp--ensure-lsp (&optional method)
  "Make sure LSP is on and ready for METHOD in the buffer, or signal an error.
Without METHOD, ready means the language servers are initialized.
LSP is started only for files in a project LSP already knows, so that
starting it never asks which project the file belongs to.  A server
still starting is waited for, at most `code-insight-mcp-lsp-start-timeout'
seconds."
  ;; `lsp-mode' stays on in buffers whose server was shut down, so look
  ;; at the servers, not the mode
  (unless (and (bound-and-true-p lsp-mode) (lsp-workspaces))
    (unless (and (require 'lsp-mode nil t)
                 buffer-file-name
                 (lsp-find-session-folder (lsp-session) buffer-file-name))
      (error "No LSP in %s, and its project is not known to LSP"
             (buffer-name)))
    (lsp)
    (unless (bound-and-true-p lsp-mode)
      (error "Could not start LSP in %s" (buffer-name))))
  (let ((deadline (+ (float-time) code-insight-mcp-lsp-start-timeout)))
    (while (and (not (code-insight-mcp--lsp-ready-p method))
                (< (float-time) deadline))
      (accept-process-output nil 0.1)))
  (unless (code-insight-mcp--lsp-ready-p method)
    (error "The language server for %s was not ready%s in %s seconds; try again later"
           (buffer-name) (if method (format " for %s" method) "")
           code-insight-mcp-lsp-start-timeout)))

(defun code-insight-mcp--goto (line symbol column)
  "Move to LINE, then to SYMBOL on it, or to COLUMN.
LINE and COLUMN count from 1.  Without SYMBOL or COLUMN, move to the
first non-blank character of LINE."
  (goto-char (point-min))
  (unless (zerop (forward-line (1- line)))
    (error "Line %d is past the end of %s" line (buffer-name)))
  (cond
   ((and symbol (not (string-empty-p symbol)))
    (unless (let ((case-fold-search nil))
              (re-search-forward
               (concat "\\_<" (regexp-quote symbol) "\\_>") (line-end-position) t))
      (error "No `%s' on line %d of %s" symbol line (buffer-name)))
    (goto-char (match-beginning 0)))
   (column
    (move-to-column (1- column)))
   (t
    (back-to-indentation))))

(defun code-insight-mcp--hover-text (contents)
  "Return the text of hover CONTENTS, nil when there is none.
CONTENTS is a MarkupContent, a MarkedString or a vector of MarkedString."
  (cond
   ((null contents) nil)
   ((stringp contents) (and (not (string-empty-p contents)) contents))
   ((vectorp contents)
    (let ((parts (delq nil (mapcar #'code-insight-mcp--hover-text contents))))
      (and parts (string-join parts "\n\n"))))
   ((lsp-markup-content? contents)
    (code-insight-mcp--hover-text (lsp:markup-content-value contents)))
   ((lsp-marked-string? contents)
    (let ((value (lsp:marked-string-value contents))
          (language (lsp:marked-string-language contents)))
      (and value (not (string-empty-p value))
           (if language
               (format "```%s\n%s\n```" language value)
             value))))))

(defun code-insight-mcp--lsp-request-at (method line symbol column
                                                 &optional params)
  "Send LSP request METHOD for a position in the current buffer.
The position is found by `code-insight-mcp--goto' from LINE, SYMBOL
and COLUMN.  PARAMS is a function called there to make the request's
parameters, `lsp--text-document-position-params' by default.  Return
\(POSITION . RESPONSE), POSITION being a \"FILE:LINE:COLUMN\" string."
  (save-excursion
    (code-insight-mcp--goto line symbol column)
    (cons (format "%s:%d:%d" buffer-file-name
                  (line-number-at-pos) (1+ (current-column)))
          (lsp-request method (funcall (or params
                                           #'lsp--text-document-position-params))))))

(defun code-insight-mcp--strip-hover-source (text)
  "Return TEXT without a trailing \"Source: *[NAME](URI)*\" line.
jdtls ends hovers with such a link to the file the symbol is in, which
the agent asked about already."
  (let ((stripped
         (and text
              (replace-regexp-in-string
               "\\(?:\n\\s-*\\)*\n?Source: \\*\\[[^]\n]*\\]([^)\n]*)\\*\\s-*\\'" "" text))))
    (and stripped (not (string-empty-p stripped)) stripped)))

(defun code-insight-mcp-lsp-hover (file-path line &optional symbol column)
  "Return what LSP shows on hover at LINE of FILE-PATH.
The position is SYMBOL on LINE if given, else COLUMN, else the first
non-blank character.  LINE and COLUMN count from 1."
  (with-current-buffer (code-insight-mcp--file-buffer file-path)
    (code-insight-mcp--ensure-lsp "textDocument/hover")
    (pcase-let ((`(,position . ,hover)
                 (code-insight-mcp--lsp-request-at
                  "textDocument/hover" line symbol column)))
      (format "%s\n%s" position
              (or (and hover (code-insight-mcp--strip-hover-source
                              (code-insight-mcp--hover-text
                               (lsp:hover-contents hover))))
                  "No hover information")))))

(defun code-insight-mcp--location-list (response)
  "Return the locations in RESPONSE as a list.
RESPONSE is nil, a Location or LocationLink, or an array of them.
lsp-mode gives arrays as vectors or lists, and objects as hash tables
or, with `lsp-use-plists', as plists."
  (cond ((null response) nil)
        ((vectorp response) (append response nil))
        ((and (consp response) (not (keywordp (car response)))) response)
        (t (list response))))

(defvar code-insight-mcp--file-lines nil
  "Nil, or a hash table from file names to vectors of their lines.
Bound while a tool runs, so that each file is read once.")

(defun code-insight-mcp--read-lines (file)
  "Return the lines of FILE as a vector, from its buffer if it has one."
  (let ((buffer (find-buffer-visiting file)))
    (with-temp-buffer
      (if buffer
          (insert-buffer-substring buffer)
        (insert-file-contents file))
      (vconcat (split-string (buffer-string) "\n")))))

(defun code-insight-mcp--line-text (file line)
  "Return LINE of FILE, counting from 1, without surrounding blanks."
  (let ((lines (if code-insight-mcp--file-lines
                   (with-memoization (gethash file code-insight-mcp--file-lines)
                     (code-insight-mcp--read-lines file))
                 (code-insight-mcp--read-lines file))))
    (if (<= 1 line (length lines))
        (string-trim (aref lines (1- line)))
      "")))

(defun code-insight-mcp--location-position (location)
  "Return (FILE LINE COLUMN) of LOCATION, LINE and COLUMN counting from 1.
LOCATION is a Location or LocationLink.  Its URI is resolved in the
current buffer, so that URIs into libraries (for example jdtls's
\"jdt:\" ones) become files a client can read."
  (pcase-let* ((`(,uri ,range)
                (if (lsp-location-link? location)
                    (list (lsp:location-link-target-uri location)
                          (lsp:location-link-target-selection-range location))
                  (list (lsp:location-uri location)
                        (lsp:location-range location))))
               (start (lsp:range-start range)))
    (list (lsp--uri-to-path uri)
          (1+ (lsp:position-line start))
          (1+ (lsp:position-character start)))))

(defun code-insight-mcp--format-location (location)
  "Return LOCATION as \"FILE:LINE:COLUMN: TEXT\".
LOCATION is as in `code-insight-mcp--location-position'."
  (pcase-let ((`(,file ,line ,column)
               (code-insight-mcp--location-position location)))
    (format "%s:%d:%d: %s" file line column
            (if (file-readable-p file)
                (code-insight-mcp--line-text file line)
              ""))))

(defun code-insight-mcp--lsp-locations (method file-path line symbol column
                                               none &optional params)
  "Return the locations LSP request METHOD gives, one per line.
The position is found from FILE-PATH, LINE, SYMBOL and COLUMN as in
`code-insight-mcp-lsp-hover'; PARAMS is as in
`code-insight-mcp--lsp-request-at'.  NONE is the text when there are
no locations."
  (with-current-buffer (code-insight-mcp--file-buffer file-path)
    (code-insight-mcp--ensure-lsp method)
    (pcase-let* ((`(,position . ,response)
                  (code-insight-mcp--lsp-request-at
                   method line symbol column params))
                 (locations (code-insight-mcp--location-list response))
                 (shown (seq-take locations code-insight-mcp-max-locations))
                 (code-insight-mcp--file-lines (make-hash-table :test #'equal)))
      (concat position "\n"
              (if locations
                  (mapconcat #'code-insight-mcp--format-location shown "\n")
                none)
              (if (> (length locations) (length shown))
                  (format "\n... and %d more"
                          (- (length locations) (length shown)))
                "")))))

(defun code-insight-mcp-lsp-definition (file-path line &optional symbol column)
  "Return where the symbol at LINE of FILE-PATH is defined.
The position is found as in `code-insight-mcp-lsp-hover'."
  (code-insight-mcp--lsp-locations "textDocument/definition"
                                   file-path line symbol column
                                   "No definition found"))

(defun code-insight-mcp-lsp-references (file-path line &optional symbol column)
  "Return where the symbol at LINE of FILE-PATH is used.
The declaration itself is not included.  The position is found as in
`code-insight-mcp-lsp-hover'."
  (code-insight-mcp--lsp-locations "textDocument/references"
                                   file-path line symbol column
                                   "No references found"
                                   (lambda () (lsp--make-reference-params nil t))))

(defun code-insight-mcp-lsp-implementation (file-path line &optional symbol column)
  "Return what implements the symbol at LINE of FILE-PATH.
The position is found as in `code-insight-mcp-lsp-hover'."
  (code-insight-mcp--lsp-locations "textDocument/implementation"
                                   file-path line symbol column
                                   "No implementations found"))

;;;;; Call hierarchy

(defun code-insight-mcp--item-position (item range)
  "Return (FILE LINE COLUMN) of RANGE in the file of call hierarchy ITEM.
LINE and COLUMN count from 1.  RANGE defaults to ITEM's selection range."
  (let ((start (lsp:range-start
                (or range (lsp:call-hierarchy-item-selection-range item)))))
    (list (lsp--uri-to-path (lsp:call-hierarchy-item-uri item))
          (1+ (lsp:position-line start))
          (1+ (lsp:position-character start)))))

(defun code-insight-mcp--item-label (item)
  "Return \"KIND NAME\" of call hierarchy ITEM, with its detail if any."
  (let ((detail (lsp:call-hierarchy-item-detail? item)))
    (format "%s %s%s"
            (alist-get (lsp:call-hierarchy-item-kind item) lsp-symbol-kinds
                       "Symbol")
            (lsp:call-hierarchy-item-name item)
            (if (and detail (not (string-empty-p detail)))
                (format " (%s)" detail)
              ""))))

(defun code-insight-mcp--incoming-lines (calls)
  "Return a line per call site in incoming CALLS.
Each is \"FILE:LINE:COLUMN: TEXT [from CALLER]\"."
  (mapcan
   (lambda (call)
     (let ((from (lsp:call-hierarchy-incoming-call-from call)))
       (mapcar (lambda (range)
                 (pcase-let ((`(,file ,line ,column)
                              (code-insight-mcp--item-position from range)))
                   (format "%s:%d:%d: %s [from %s]" file line column
                           (if (file-readable-p file)
                               (code-insight-mcp--line-text file line)
                             "")
                           (code-insight-mcp--item-label from))))
               (code-insight-mcp--location-list
                (lsp:call-hierarchy-incoming-call-from-ranges call)))))
   calls))

(defun code-insight-mcp--outgoing-lines (calls)
  "Return a line per callee in outgoing CALLS.
Each is \"FILE:LINE:COLUMN: CALLEE (called on lines ...)\", the
lines being those of the calls in the caller's file."
  (mapcar
   (lambda (call)
     (let ((to (lsp:call-hierarchy-outgoing-call-to call)))
       (pcase-let ((`(,file ,line ,column)
                    (code-insight-mcp--item-position to nil)))
         (format "%s:%d:%d: %s (called on line %s)" file line column
                 (code-insight-mcp--item-label to)
                 (mapconcat (lambda (range)
                              (number-to-string
                               (1+ (lsp:position-line (lsp:range-start range)))))
                            (code-insight-mcp--location-list
                             (lsp:call-hierarchy-outgoing-call-from-ranges call))
                            ", ")))))
   calls))

(defun code-insight-mcp-lsp-call-hierarchy (file-path line direction
                                                      &optional symbol column)
  "Return the calls into or out of the function at LINE of FILE-PATH.
DIRECTION is \"incoming\" for its callers or \"outgoing\" for what it
calls.  The position is found as in `code-insight-mcp-lsp-hover'."
  (unless (member direction '("incoming" "outgoing"))
    (error "Direction must be \"incoming\" or \"outgoing\", not %S" direction))
  (with-current-buffer (code-insight-mcp--file-buffer file-path)
    (code-insight-mcp--ensure-lsp "textDocument/prepareCallHierarchy")
    (pcase-let* ((`(,position . ,response)
                  (code-insight-mcp--lsp-request-at
                   "textDocument/prepareCallHierarchy" line symbol column))
                 (item (car (code-insight-mcp--location-list response))))
      (if (not item)
          (format "%s\nNo function here to show calls of" position)
        (let* ((incoming (equal direction "incoming"))
               (calls (code-insight-mcp--location-list
                       (lsp-request (if incoming
                                        "callHierarchy/incomingCalls"
                                      "callHierarchy/outgoingCalls")
                                    (list :item item))))
               ;; jdtls repeats a call once per call site
               (lines (delete-dups
                       (if incoming
                           (code-insight-mcp--incoming-lines calls)
                         (code-insight-mcp--outgoing-lines calls))))
               (shown (seq-take lines code-insight-mcp-max-locations)))
          (concat position ": " (code-insight-mcp--item-label item)
                  (if lines
                      (mapconcat (lambda (l) (concat "\n" l)) shown "")
                    (if incoming "\nNo callers found" "\nNo calls found"))
                  (if (> (length lines) (length shown))
                      (format "\n... and %d more"
                              (- (length lines) (length shown)))
                    "")))))))

;;;;; Workspace symbols

(defun code-insight-mcp--format-symbol (symbol)
  "Return SYMBOL as \"FILE:LINE:COLUMN: KIND NAME (in CONTAINER)\".
SYMBOL is a SymbolInformation."
  (pcase-let ((`(,file ,line ,column)
               (code-insight-mcp--location-position
                (lsp:symbol-information-location symbol)))
              (container (lsp:symbol-information-container-name? symbol)))
    (format "%s:%d:%d: %s %s%s" file line column
            (alist-get (lsp:symbol-information-kind symbol) lsp-symbol-kinds
                       "Symbol")
            (lsp:symbol-information-name symbol)
            (if (and container (not (string-empty-p container)))
                (format " (in %s)" container)
              ""))))

(defun code-insight-mcp-lsp-workspace-symbol (file-path query)
  "Return the symbols matching QUERY in the project of FILE-PATH.
FILE-PATH only picks the language server to ask."
  (with-current-buffer (code-insight-mcp--file-buffer file-path)
    (code-insight-mcp--ensure-lsp "workspace/symbol")
    (let* ((symbols (code-insight-mcp--location-list
                     (lsp-request "workspace/symbol" (list :query query))))
           (shown (seq-take symbols code-insight-mcp-max-locations)))
      (concat (format "%d symbols matching %S" (length symbols) query)
              (mapconcat (lambda (symbol)
                           (concat "\n" (code-insight-mcp--format-symbol symbol)))
                         shown "")
              (if (> (length symbols) (length shown))
                  (format "\n... and %d more" (- (length symbols) (length shown)))
                "")))))

;;;;; Diagnostics

(defvar code-insight-mcp--diagnostics-ticks (make-hash-table :test #'equal)
  "Hash table from file names to the contents their diagnostics are for.
Each value is the `buffer-chars-modified-tick' of the file's buffer
when the language server last published diagnostics for it.")

(defun code-insight-mcp--note-diagnostics (_workspace params)
  "Record which contents the diagnostics in PARAMS are for.
Runs after `lsp--on-diagnostics', the handler of every
textDocument/publishDiagnostics notification."
  (let* ((file (lsp--uri-to-path (lsp:publish-diagnostics-params-uri params)))
         (buffer (find-buffer-visiting file)))
    (when buffer
      (puthash (buffer-file-name buffer)
               (buffer-chars-modified-tick buffer)
               code-insight-mcp--diagnostics-ticks))))

(defun code-insight-mcp--diagnostics-fresh-p ()
  "Return non-nil if the current buffer's diagnostics are for its contents."
  (eql (gethash buffer-file-name code-insight-mcp--diagnostics-ticks)
       (buffer-chars-modified-tick)))

(defconst code-insight-mcp--unseen-wait 2
  "Seconds to wait for diagnostics of a file never seen checked.
Such a file was opened before `code-insight-mcp-mode' started watching
and has not changed since; servers may well not check it again.")

(defun code-insight-mcp--wait-for-diagnostics ()
  "Wait until the current buffer's diagnostics are fresh, or time out.
Servers check a file again only when it changes, so when the buffer
is unmodified, tell the server it was saved, which may get it checked.
Return `fresh', `unseen' when the file was never seen checked and is
unchanged, or `timeout'."
  (let ((unseen (and (not (gethash buffer-file-name
                                   code-insight-mcp--diagnostics-ticks))
                     (not (buffer-modified-p)))))
    (unless (or (code-insight-mcp--diagnostics-fresh-p) (buffer-modified-p))
      (lsp--text-document-did-save))
    (let ((deadline (+ (float-time)
                       (if unseen
                           (min code-insight-mcp--unseen-wait
                                code-insight-mcp-diagnostics-timeout)
                         code-insight-mcp-diagnostics-timeout))))
      (while (and (not (code-insight-mcp--diagnostics-fresh-p))
                  (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (cond ((code-insight-mcp--diagnostics-fresh-p) 'fresh)
          (unseen 'unseen)
          (t 'timeout))))

(defconst code-insight-mcp--severities
  '((1 . "error") (2 . "warning") (3 . "info") (4 . "hint"))
  "LSP diagnostic severities and their names.")

(defun code-insight-mcp--format-diagnostic (file diagnostic)
  "Return DIAGNOSTIC of FILE as \"FILE:LINE:COLUMN: SEVERITY: MESSAGE\".
The source and code of DIAGNOSTIC follow in brackets when known."
  (let* ((start (lsp:range-start (lsp:diagnostic-range diagnostic)))
         (source (lsp:diagnostic-source? diagnostic))
         (code (lsp:diagnostic-code? diagnostic))
         (tag (string-join (delq nil (list source
                                           (and code (format "%s" code))))
                           " ")))
    (format "%s:%d:%d: %s: %s%s"
            file
            (1+ (lsp:position-line start))
            (1+ (lsp:position-character start))
            (alist-get (lsp:diagnostic-severity? diagnostic)
                       code-insight-mcp--severities "unknown")
            (string-trim (lsp:diagnostic-message diagnostic))
            (if (string-empty-p tag) "" (format " [%s]" tag)))))

(defun code-insight-mcp--diagnostic-key (diagnostic)
  "Return a sort key for DIAGNOSTIC: its line, then its column."
  (let ((start (lsp:range-start (lsp:diagnostic-range diagnostic))))
    (+ (* 100000 (lsp:position-line start)) (lsp:position-character start))))

(defun code-insight-mcp-lsp-diagnostics (file-path)
  "Return the errors and warnings the language server reports for FILE-PATH.
When the file changed since the server last checked it, wait for the
new check, at most `code-insight-mcp-diagnostics-timeout' seconds."
  (with-current-buffer (code-insight-mcp--file-buffer file-path)
    (code-insight-mcp--ensure-lsp)
    (let* ((state (code-insight-mcp--wait-for-diagnostics))
           (diagnostics (sort (copy-sequence
                               (gethash (lsp--fix-path-casing buffer-file-name)
                                        (lsp-diagnostics t)))
                              (lambda (a b)
                                (< (code-insight-mcp--diagnostic-key a)
                                   (code-insight-mcp--diagnostic-key b))))))
      (concat
       (format "%s: %s" buffer-file-name
               (if diagnostics
                   (format "%d diagnostics" (length diagnostics))
                 "no diagnostics"))
       (pcase state
         ('fresh "")
         ('unseen " (as the language server reported them before; the file has not changed since it was opened)")
         (_ (format " (the language server did not finish checking in %s seconds; these may be out of date)"
                    code-insight-mcp-diagnostics-timeout)))
       (mapconcat (lambda (d)
                    (concat "\n" (code-insight-mcp--format-diagnostic
                                   buffer-file-name d)))
                  diagnostics "")))))

(defconst code-insight-mcp--position-args
  '((:name "file_path"
           :type string
           :description "Absolute path of the file")
    (:name "line"
           :type integer
           :description "Line number, counting from 1")
    (:name "symbol"
           :type string
           :description "Name of the symbol on that line; its first occurrence is used"
           :optional t)
    (:name "column"
           :type integer
           :description "Column, counting from 1; used when symbol is not given"
           :optional t))
  "Arguments of the LSP tools that take a position in a file.")

(defun code-insight-mcp--setup-lsp-tools ()
  "Register the LSP tools of this package.
Tools registered before are removed first, so that a tool whose
description changed is not served twice."
  (setq claude-code-ide-mcp-server-tools
        (seq-remove (lambda (spec)
                      (let ((function (plist-get spec :function)))
                        (and (symbolp function)
                             (string-prefix-p "code-insight-mcp-"
                                              (symbol-name function)))))
                    claude-code-ide-mcp-server-tools))
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-hover
   :name "lsp_hover"
   :description "Show what the language server knows about a symbol: its type or signature and its documentation.  Uses the language server Emacs already runs for the file's project.  Give the line and, preferably, the symbol's name on that line instead of a column."
   :args code-insight-mcp--position-args)
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-definition
   :name "lsp_definition"
   :description "Find where a symbol is defined, using the language server Emacs already runs for the file's project.  Each result is FILE:LINE:COLUMN: and the text of that line.  Definitions inside libraries are given as files you can read, where the language server provides them (for example jdtls's sources or decompiled classes in jars).  Give the line and, preferably, the symbol's name on that line instead of a column."
   :args code-insight-mcp--position-args)
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-references
   :name "lsp_references"
   :description "Find where a symbol is used, using the language server Emacs already runs for the file's project.  Unlike a text search, it tells apart overloads and other symbols with the same name.  Each result is FILE:LINE:COLUMN: and the text of that line; the declaration itself is not included.  Give the line and, preferably, the symbol's name on that line instead of a column."
   :args code-insight-mcp--position-args)
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-diagnostics
   :name "lsp_diagnostics"
   :description "List the errors and warnings the language server reports for a file, as a compiler would, without building.  Use it after changing a file: when the file changed since the server last checked it, this waits for the new check.  Each result is FILE:LINE:COLUMN: SEVERITY: MESSAGE [SOURCE CODE].  Uses the language server Emacs already runs for the file's project."
   :args '((:name "file_path"
                  :type string
                  :description "Absolute path of the file")))
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-implementation
   :name "lsp_implementation"
   :description "Find what implements an interface, abstract class or method, using the language server Emacs already runs for the file's project.  Each result is FILE:LINE:COLUMN: and the text of that line.  Give the line and, preferably, the symbol's name on that line instead of a column."
   :args code-insight-mcp--position-args)
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-call-hierarchy
   :name "lsp_call_hierarchy"
   :description "Show one level of calls of a function or method, using the language server Emacs already runs for the file's project.  With direction \"incoming\", each result is a call site, FILE:LINE:COLUMN: TEXT [from CALLER]; with \"outgoing\", each is a function it calls, FILE:LINE:COLUMN: CALLEE (called on line ...).  Call again on a result to go further.  Give the line and, preferably, the function's name on that line instead of a column."
   :args (append (seq-take code-insight-mcp--position-args 2)
                 '((:name "direction"
                          :type string
                          :enum ["incoming" "outgoing"]
                          :description "\"incoming\" for callers, \"outgoing\" for what it calls"))
                 (seq-drop code-insight-mcp--position-args 2)))
  (claude-code-ide-make-tool
   :function #'code-insight-mcp-lsp-workspace-symbol
   :name "lsp_workspace_symbol"
   :description "Search a project's symbols (classes, methods, fields and so on) by name, including its libraries where the language server indexes them.  Each result is FILE:LINE:COLUMN: KIND NAME (in CONTAINER); symbols inside libraries are given as files you can read, where the language server provides them, though their line and column may only point at the top of the file.  Servers may cap the results (jdtls at 50), so prefer specific names, qualified ones where the server takes them.  Uses the language server Emacs already runs for the project."
   :args '((:name "file_path"
                  :type string
                  :description "Absolute path of any file in the project, to pick its language server")
           (:name "query"
                  :type string
                  :description "Name or part of a name; how it matches is up to the language server"))))

;;;; Server

(defun code-insight-mcp-url ()
  "Return the URL agents reach the server at."
  (format "http://localhost:%d/mcp/%s"
          (or (claude-code-ide-mcp-server-get-port) code-insight-mcp-port)
          code-insight-mcp-session-id))

(defun code-insight-mcp--start ()
  "Start the server and register the session for agents."
  (setq claude-code-ide-mcp-server-port code-insight-mcp-port
        claude-code-ide-enable-mcp-server t)
  (claude-code-ide-emacs-tools-setup)
  (code-insight-mcp--drop-tools)
  (code-insight-mcp--rename-tools)
  (code-insight-mcp--setup-lsp-tools)
  (dolist (f code-insight-mcp--tool-functions)
    (advice-add f :around #'code-insight-mcp--around-tool))
  (advice-add 'lsp--on-diagnostics :after #'code-insight-mcp--note-diagnostics)
  (unless (claude-code-ide-mcp-server-ensure-server)
    (error "Could not start the MCP server on port %d" code-insight-mcp-port))
  (claude-code-ide-mcp-server-register-session
   code-insight-mcp-session-id
   (expand-file-name code-insight-mcp-session-directory)
   nil))

(defun code-insight-mcp--stop ()
  "Unregister the session; the server stops when no sessions remain."
  (claude-code-ide-mcp-server-session-ended code-insight-mcp-session-id)
  (dolist (f code-insight-mcp--tool-functions)
    (advice-remove f #'code-insight-mcp--around-tool))
  (advice-remove 'lsp--on-diagnostics #'code-insight-mcp--note-diagnostics))

;;;###autoload
(define-minor-mode code-insight-mcp-mode
  "Keep an MCP server running that lends Emacs's code insight to agents.
The server URL is shown by `code-insight-mcp-url'."
  :global t
  :group 'code-insight-mcp
  (if code-insight-mcp-mode
      (condition-case err
          (progn
            (code-insight-mcp--start)
            (message "code-insight-mcp: serving at %s" (code-insight-mcp-url)))
        (error
         (code-insight-mcp--stop)
         (setq code-insight-mcp-mode nil)
         (signal (car err) (cdr err))))
    (code-insight-mcp--stop)))

(provide 'code-insight-mcp)
;;; code-insight-mcp.el ends here
