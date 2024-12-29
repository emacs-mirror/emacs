;;; treesit-admin.el --- Tree-sitter related admin scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

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
  '((c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby"))
  "A list of sources for the builtin modes.
The source information are in the format of
`treesit-language-source-alist'.  This is for development only.")

(defun treesit-admin--verify-major-mode-queries (modes langs source-alist grammar-dir)
  "Verify font-lock queries in MODES.

LANGS is a list of languages, it should cover all the languages used by
major modes in MODES.  SOURCE-ALIST should have the same shape as
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
        (file-modes-alist nil))
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
            (insert (format ";; %s is known to work with the following languages and version:\n" mode))
            (dolist (lang (alist-get mode mode-language-alist))
              (insert (format ";; - tree-sitter-%s: %s\n" lang (alist-get lang version-alist))))
            (insert ";;\n"))
          (insert
           ";; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar version has a good chance to work.
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

;;; Compatibility report

(defvar treesit-admin-file-name (or load-file-name (buffer-file-name))
  "Filename of the source file treesit-admin.el.")

(defvar treesit-admin--compat-template-file-name
  (expand-file-name "compat-template.html"
                    (file-name-directory
                     (or load-file-name (buffer-file-name))))
  "Filename of the HTML template for the compatibility report.")

(defun treesit-admin-verify-major-mode-queries ()
  "Varify font-lock queries in builtin major modes.

If the font-lock queries work fine with the latest grammar, insert some
comments in the source file saying that the modes are known to work with
that version of grammar.  At the end of the process, show a list of
queries that has problems with latest grammar."
  (interactive)
  (treesit-admin--verify-major-mode-queries
   '(cmake-ts-mode dockerfile-ts-mode go-ts-mode ruby-ts-mode)
   '(cmake dockerfile go ruby)
   treesit-admin--builtin-language-sources
   "/tmp/tree-sitter-grammars"))

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
      (let* ((query (treesit-font-lock-setting-query setting))
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
             (funcall mode)
             (font-lock-mode -1)
             treesit-font-lock-settings)))
        (all-queries-valid t))
    (cl-remove-duplicates
     (mapcar #'treesit-query-language
             (mapcar #'treesit-font-lock-setting-query
                     settings)))))

(defun treesit-admin--find-latest-compatible-revision
    (mode language source-alist grammar-dir)
  "Find the latest revision for LANGUAGE that's compatible with MODE.

MODE, LANGUAGE, SOURCE-ALIST, GRAMMAR-DIR are the same as in
`treesit-admin--verify-major-mode-queries'.

Return a plist (:version VERSION :head-version HEAD-VERSION).
HEAD-VERSION is the version of the HEAD, VERSION is the latest
compatible version."
  (let ((treesit-extra-load-path (list grammar-dir))
        (treesit--install-language-grammar-full-clone t)
        (treesit--install-language-grammar-blobless t)
        (recipe (alist-get language source-alist))
        (workdir (make-temp-file "treesit-validate-workdir" t))
        (emacs-executable
         (expand-file-name invocation-name invocation-directory))
        head-version version exit-code)
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
    (list :version version :head-version head-version)))

(defun treesit-admin--last-compatible-grammar-for-modes
    (modes source-alist grammar-dir)
  "Generate an HTML page listing latest compatible grammar versions.

MODES, SOURCE-ALIST, GRAMMAR-DIR are the same as
`treesit-admin--verify-major-mode-queries'.

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
                     mode language source-alist grammar-dir)))
            (treesit-admin--mode-languages mode))))
   modes))

(defun treesit-admin-generate-compatibility-report ()
  "Generate a language compatibility report."
  (interactive)
  )

(provide 'treesit-admin)

;;; treesit-admin.el ends here
