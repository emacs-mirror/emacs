;;; treesit-admin.el --- Tree-sitter related admin scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Maintainer: 付禹安 (Yuan Fu) <casouri@gmail.com>
;; Keywords: maint
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'rx)

;;; Manual coverage

(declare-function find-library-name "find-func.el")
(defun treesit-admin-check-manual-coverage ()
  "Print tree-sitter functions missing from the manual in message buffer."
  (interactive)
  (require 'find-func)
  (let ((functions-in-source
         (with-temp-buffer
           (insert-file-contents (find-library-name "treesit"))
           (cl-remove-if
            (lambda (name) (string-search "treesit--" name))
            (cl-sort
             (save-excursion
               (goto-char (point-min))
               (cl-loop while (re-search-forward
                               "^(defun \\([^ ]+\\)" nil t)
                        collect (match-string-no-properties 1)))
             #'string<))))
        (functions-in-manual
         (with-temp-buffer
           (insert-file-contents (expand-file-name
                                  "doc/lispref/parsing.texi"
                                  source-directory))
           (insert-file-contents (expand-file-name
                                  "doc/lispref/modes.texi"
                                  source-directory))
           (cl-sort
            (save-excursion
              (goto-char (point-min))
              (cl-loop while (re-search-forward
                              "^@defun \\([^ ]+\\)" nil t)
                       collect (match-string-no-properties 1)))
            #'string<))))
    (message "Missing: %s"
             (string-join
              (cl-remove-if
               (lambda (name) (member name functions-in-manual))
               functions-in-source)
              "\n"))))

;;; Query validation

(defvar treesit-admin--builtin-language-sources
  '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.4")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
    (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
    (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                "v0.23.2" "typescript/src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
         "v0.23.2" "tsx/src")
    (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
    (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.2")
    (php "https://github.com/tree-sitter/tree-sitter-php"
         "v0.23.11" "php/src")
    (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.1")
    (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
    (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen" "v1.1.0")
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.3.0")
    (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
    (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "v0.3.3")
    (heex "https://github.com/phoenixframework/tree-sitter-heex" "v0.7.0")
    (java "https://github.com/tree-sitter/tree-sitter-java" "v0.23.5")
    (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.23.2")
    (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.7.0")
    (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0"))
  "A list of sources for the builtin modes.
The source information are in the format of
`treesit-language-source-alist'.  This is for development only.")

(defvar treesit-admin--builtin-modes
  '( c-ts-mode c++-ts-mode cmake-ts-mode dockerfile-ts-mode
     go-ts-mode ruby-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode
     json-ts-mode rust-ts-mode php-ts-mode css-ts-mode lua-ts-mode
     html-ts-mode elixir-ts-mode heex-ts-mode java-ts-mode)
  "Builtin tree-sitter modes that we check.")

(defun treesit-admin--verify-major-mode-queries (modes source-alist grammar-dir)
  "Verify font-lock queries in MODES.

SOURCE-ALIST should have the same shape as
`treesit-language-source-alist'.  GRAMMAR-DIR is a temporary direction
in which grammars are installed.

If the font-lock queries work fine with the latest grammar, insert some
comments in the source file saying that the modes are known to work with
that version of grammar.  At the end of the process, show a list of
queries that has problems with latest grammar."
  (let ((treesit-extra-load-path (list grammar-dir))
        (treesit--install-language-grammar-full-clone t)
        (treesit--install-language-grammar-blobless t)
        (version-alist nil)
        (invalid-feature-list nil)
        (valid-modes nil)
        (mode-language-alist nil)
        (file-modes-alist nil)
        (langs (cl-remove-duplicates
                (mapcan #'treesit-admin--mode-languages modes))))
    (dolist (lang langs)
      (let* ((recipe (assoc lang source-alist))
             (ver (apply #'treesit--install-language-grammar-1
                         (cons grammar-dir recipe))))
        (if ver
            (push (cons lang ver) version-alist)
          (error "Cannot get version for %s" lang))))

    ;; Validate font-lock queries for each major mode.
    (dolist (mode modes)
      (let ((settings
             (with-temp-buffer
               (ignore-errors
                 (funcall mode)
                 (font-lock-mode -1)
                 treesit-font-lock-settings)))
            (all-queries-valid t))
        (dolist (setting settings)
          (let* ((query (treesit-font-lock-setting-query setting))
                 (language (treesit-query-language query))
                 (feature (treesit-font-lock-setting-feature setting)))
            ;; Record that MODE uses LANGUAGE.
            (unless (memq language (alist-get mode mode-language-alist))
              (push language (alist-get mode mode-language-alist)))
            ;; Validate query.
            (when (not (ignore-errors
                         (treesit-query-compile language query t)
                         t))
              (push (list mode language feature) invalid-feature-list)
              (setq all-queries-valid nil))))
        (when all-queries-valid
          (push mode valid-modes))))

    ;; Group modes by their source file.
    (dolist (mode valid-modes)
      (let ((source-file (replace-regexp-in-string
                          (rx ".elc" eos)
                          ".el"
                          (car (get mode 'function-history)))))
        (unless (member mode (alist-get source-file file-modes-alist
                                        nil nil #'equal))
          (push mode (alist-get source-file file-modes-alist
                                nil nil #'equal)))))

    ;; Update the "known-to-work" version comment for the modes.
    (pcase-dolist (`(,source-file . ,modes) file-modes-alist)
      (let (beg)
        (with-temp-buffer
          (insert-file-contents source-file)
          (goto-char (point-min))
          (when (not (search-forward
                      ";;; Tree-sitter language versions\n" nil t))
            (re-search-forward (rx (or ";;; Commentary:" ";;; Code:")))
            (forward-line -1)
            (insert "\n;;; Tree-sitter language versions\n\n")
            (forward-line -1))
          (setq beg (point))
          (search-forward "\n\n")
          (delete-region beg (point))
          (insert ";;\n")
          (dolist (mode modes)
            (insert (format ";; %s has been tested with the following grammars and version:\n" mode))
            (dolist (lang (alist-get mode mode-language-alist))
              (insert (format ";; - tree-sitter-%s: %s\n" lang (alist-get lang version-alist))))
            (insert ";;\n"))
          (insert
           ";; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.")
          (insert "\n\n")
          (write-file source-file))))

    (pop-to-buffer (get-buffer-create "*verify major mode queries*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Verified grammar and versions:\n")
      (pcase-dolist (`(,lang . ,version) version-alist)
        (insert (format "- %s: %s\n" lang version)))
      (insert "\n")
      (if (null invalid-feature-list)
          (insert "All the queries are valid with latest grammar.\n")
        (insert "The following modes has invalid queries:\n")
        (dolist (entry invalid-feature-list)
          (insert (format "mode: %s language: %s feature: %s"
                          (nth 0 entry)
                          (nth 1 entry)
                          (nth 2 entry)))))
      (special-mode))))

(defun treesit-admin-verify-major-mode-queries ()
  "Verify font-lock queries in builtin major modes.

If the font-lock queries work fine with the latest grammar, insert some
comments in the source file saying that the modes are known to work with
that version of grammar.  At the end of the process, show a list of
queries that has problems with latest grammar."
  (interactive)
  (treesit-admin--verify-major-mode-queries
   treesit-admin--builtin-modes
   treesit-admin--builtin-language-sources
   "/tmp/tree-sitter-grammars"))

;;; Compatibility report

(defvar treesit-admin-file-name (or load-file-name (buffer-file-name))
  "Filename of the source file treesit-admin.el.")

(defvar treesit-admin--compat-template-file-name
  (expand-file-name "compat-template.html"
                    (file-name-directory
                     (or load-file-name (buffer-file-name))))
  "Filename of the HTML template for the compatibility report.")

(defun treesit-admin--validate-mode-lang (mode lang)
  "Validate queries for LANG in MODE.

Return non-nil if all queries are valid, nil otherwise."
  (let ((settings
         (with-temp-buffer
           (ignore-errors
             (funcall mode)
             (font-lock-mode -1)
             treesit-font-lock-settings)))
        (all-queries-valid t))
    (dolist (setting settings)
      ;; `treesit-font-lock-setting-query' isn't available in Emacs 30.
      (let* ((query (car setting))
             (language (treesit-query-language query)))
        ;; Validate query.
        (when (and (eq lang language)
                   (not (ignore-errors
                          (treesit-query-compile language query t)
                          t)))
          (setq all-queries-valid nil))))
    all-queries-valid))

(defun treesit-admin--mode-languages (mode)
  "Return languages used by MODE in a list."
  (let ((settings
         (with-temp-buffer
           (ignore-errors
             ;; TODO: A more generic way to find all queries.
             (let ((c-ts-mode-enable-doxygen t)
                   (c-ts-mode-enable-doxygen t)
                   (java-ts-mode-enabel-doxygen t))
               (funcall mode))
             (font-lock-mode -1)
             treesit-font-lock-settings)))
        (all-queries-valid t))
    (cl-remove-duplicates
     (mapcar #'treesit-query-language
             (mapcar #'treesit-font-lock-setting-query
                     settings)))))

(defun treesit-admin--find-latest-compatible-revision
    (mode language source-alist grammar-dir &optional emacs-executable)
  "Find the latest revision for LANGUAGE that's compatible with MODE.

MODE, LANGUAGE, SOURCE-ALIST, GRAMMAR-DIR are the same as in
`treesit-admin--verify-major-mode-queries'.

By default, use the Emacs executable that spawned the current Emacs
session to validate grammars, but if EMACS-EXECUTABLE is non-nil, use it
instead.

Return a plist of the form

    (:version VERSION :head-version HEAD-VERSION :timestamp TIMESTAMP).

HEAD-VERSION is the version of the HEAD, VERSION is the latest
compatible version.  TIMESTAMP is the commit date of VERSION in UNIX
epoch format."
  (let ((treesit-extra-load-path (list grammar-dir))
        (treesit--install-language-grammar-full-clone t)
        (treesit--install-language-grammar-blobless t)
        (recipe (alist-get language source-alist))
        (workdir (make-temp-file "treesit-validate-workdir" t))
        (emacs-executable
         (or emacs-executable
             (expand-file-name invocation-name invocation-directory)))
        head-version version exit-code timestamp)
    (when (not recipe)
      (signal 'treesit-error `("Cannot find recipe" ,language)))
    (pcase-let ((`(,url ,revision ,source-dir ,cc ,c++ ,commit)
                 recipe))
      (with-temp-buffer
        (treesit--git-clone-repo url revision workdir)
        (when commit
          (treesit--git-checkout-branch workdir commit))
        (setq head-version (treesit--language-git-revision workdir))
        (treesit--build-grammar
         workdir grammar-dir language source-dir cc c++)
        (while (not (eq exit-code 0))
          (unless (null exit-code)
            (treesit--git-checkout-branch workdir "HEAD~")
            (treesit--build-grammar
             workdir grammar-dir language source-dir cc c++))
          (setq version (treesit--language-git-revision workdir))
          (setq timestamp (treesit--language-git-timestamp workdir))
          (message "Validateing version %s" version)
          (setq exit-code
                (call-process
                 emacs-executable nil t nil
                 "-Q" "--batch"
                 "--eval" (prin1-to-string
                           `(let ((treesit-extra-load-path
                                   '(,grammar-dir)))
                              (load ,treesit-admin-file-name)
                              (if (treesit-admin--validate-mode-lang
                                   ',mode ',language)
                                  (kill-emacs 0)
                                (kill-emacs -1)))))))))
    (list :version version :head-version head-version :timestamp timestamp)))

(defun treesit-admin--last-compatible-grammar-for-modes
    (modes source-alist grammar-dir &optional emacs-executable)
  "Generate an alist listing latest compatible grammar versions.

MODES, SOURCE-ALIST, GRAMMAR-DIR are the same as
`treesit-admin--verify-major-mode-queries'.  If EMACS-EXECUTABLE is
non-nil, use it for validating queries.

Return an alist of an alist of a plist:

    ((MODE . ((LANG . (:version VERSION :head-VERSION HEAD-VERSION)) ...)) ...)

VERSION and HEAD-VERSION in the plist are the same as in
`treesit-admin--find-latest-compatible-revision'."
  (mapcar
   (lambda (mode)
     (cons mode
           (mapcar
            (lambda (language)
              (cons language
                    (treesit-admin--find-latest-compatible-revision
                     mode language source-alist grammar-dir
                     emacs-executable)))
            (treesit-admin--mode-languages mode))))
   modes))

(defun treesit-admin--generate-compatibility-report
    (emacs-executables modes out-file)
  "Generate a table for language compatibility for MODES.

Note that this only works for Emacs 31 and later, because before Emacs
31 we can't validate a compiled query (because there's a bug preventing
us from eager compiling a compiled query that's already lazily
compiled).

EMACS-EXECUTABLES is a list of Emacs executables to check for."
  (let ((tables
         (mapcar
          (lambda (emacs)
            (cons (with-temp-buffer
                    (call-process emacs nil t nil
                                  "-Q" "--batch"
                                  "--eval" "(princ emacs-version)")
                    (buffer-string))
                  (treesit-admin--last-compatible-grammar-for-modes
                   modes
                   treesit-admin--builtin-language-sources
                   "/tmp/treesit-grammar"
                   emacs)))
          emacs-executables))
        (database (make-hash-table :test #'equal))
        languages)
    (dolist (table tables)
      (dolist (mode-entry (cdr table))
        (dolist (language-entry (cdr mode-entry))
          (let* ((lang (car language-entry))
                 (plist (cdr language-entry))
                 ;; KEY = (LANG . EMACS-VERSION)
                 (key (cons lang (car table)))
                 (existing-plist (gethash key database)))
            (push lang languages)
            ;; If there are two major modes that uses LANG, and they
            ;; have different compatible versions, use the older
            ;; version.
            (when (or (not existing-plist)
                      (< (plist-get plist :timestamp)
                         (plist-get existing-plist :timestamp)))
              (puthash key plist database))))))
    (setq languages (cl-sort (cl-remove-duplicates languages)
                             (lambda (a b)
                               (string< (symbol-name a) (symbol-name b)))))
    ;; Compose HTML table.
    (with-temp-buffer
      (insert "<tr><th>Language</th>")
      (dolist (emacs-version (mapcar #'car tables))
        (insert (format "<th>%s</th>" emacs-version)))
      (insert "</tr>\n")
      (dolist (lang languages)
        (insert "<tr>")
        (insert (format "<th>%s</th>" lang))
        (dolist (emacs-version (mapcar #'car tables))
          (let* ((key (cons lang emacs-version))
                 (plist (gethash key database))
                 (version (plist-get plist :version))
                 (head-version (plist-get plist :head-version))
                 (classname
                  (if (equal version head-version) "head" "")))
            (if (not plist)
                (insert "<td></td>")
              (insert (format "<td class=\"%s\">%s</td>"
                              classname version)))))
        (insert "</tr>\n"))

      ;; Compose table with template and write to out file.
      (let ((time (current-time-string nil t))
            (table-text (buffer-string)))
        (erase-buffer)
        (insert-file-contents treesit-admin--compat-template-file-name)
        (goto-char (point-min))
        (search-forward "___REPLACE_TIME___")
        (replace-match (format "%s UTC" time) t)
        (search-forward "___REPLACE_TABLE___")
        (replace-match table-text t)
        (write-region (point-min) (point-max) out-file)))))

(provide 'treesit-admin)

;;; treesit-admin.el ends here
