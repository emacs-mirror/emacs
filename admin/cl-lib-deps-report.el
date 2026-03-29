:;exec emacs -Q --batch -l "$0" -- "$@" # -*- lexical-binding: t -*-
;;; cl-lib-deps-report.el --- report cl-lib dependencies in lisp files -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;; Generate an Org report of cl-lib macro/function usage and missing
;; compile-time/runtime requires for files under lisp/.

;;; Code:

(require 'cl-lib)
(require 'org)

(setq debug-on-error nil)

(defun cl-lib-deps-report--scan-file (file symbol-re macros funcs)
  "Return cl-lib usage data for FILE using SYMBOL-RE, MACROS, and FUNCS.
Exclude tokens found in strings or comments, and return a list with
dependency flags, require kind, and sorted symbol lists."
  (with-temp-buffer
    (insert-file-contents file)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (let ((tokens '())
            (total-req 0)
            (eval-req 0))
        (goto-char (point-min))
        (while (re-search-forward symbol-re nil t)
          (let ((ppss (syntax-ppss)))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (push (match-string 0) tokens))))
        (setq tokens (cl-delete-duplicates tokens :test #'string=))
        (let* ((macro-toks (cl-remove-if-not (lambda (tok) (member tok macros)) tokens))
               (func-toks (cl-remove-if-not (lambda (tok) (member tok funcs)) tokens))
               (macro-dep (and macro-toks t))
               (func-dep (and func-toks t)))
          (goto-char (point-min))
          (while (re-search-forward "(require[[:space:]\n]*'cl-lib" nil t)
            (let ((ppss (syntax-ppss)))
              (unless (or (nth 3 ppss) (nth 4 ppss))
                (setq total-req (1+ total-req)))))
          (goto-char (point-min))
          (while (re-search-forward "(eval-when-compile[[:space:]\n]*(require[[:space:]\n]*'cl-lib" nil t)
            (let ((ppss (syntax-ppss)))
              (unless (or (nth 3 ppss) (nth 4 ppss))
                (setq eval-req (1+ eval-req)))))
          (let* ((runtime-req (> total-req eval-req))
                 (eval-req-present (> eval-req 0))
                 (require-kind
                  (cond
                   ((and (= total-req 0) (= eval-req 0)) "no")
                   ((> total-req eval-req) "runtime")
                   (t "compile-time"))))
            (list macro-dep func-dep require-kind runtime-req eval-req-present
                  (sort macro-toks #'string<)
                  (sort func-toks #'string<))))))))

(defun cl-lib-deps-report--main (args)
  "Generate an Org report of cl-lib dependencies under a Lisp directory.
ARGS should be `command-line-args-left', which starts with \"--\" when
invoked via the file's exec stub."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (default-root (expand-file-name "../lisp" script-dir))
         ;; `command-line-args-left' includes a \"--\" sentinel from the exec stub.
         (args (if (and args (string= (car args) "--")) (cdr args) args))
         (root (or (car args) default-root)))
    (unless (file-directory-p root)
      (princ (format "%s: Directory not found: %s\n" (or load-file-name "cl-lib-deps-report.el") root))
      (kill-emacs 1))
    (let* ((candidate-re "cl-[[:alnum:]-]+\\*?")
           (symbol-re "\\_<cl-[[:alnum:]-]+\\*?\\_>")
           (pattern (format "%s|\\(require[[:space:]]*'cl-lib|\\(eval-when-compile[[:space:]]*\\(require[[:space:]]*'cl-lib"
                            candidate-re))
           (files
            (let ((cmd (format "find %s -type f -name '*.el' -print0 | xargs -0 grep -l -E %s || true"
                               (shell-quote-argument root)
                               (shell-quote-argument pattern))))
              (with-temp-buffer
                (call-process "sh" nil t nil "-c" cmd)
                (split-string (buffer-string) "\n" t))))
           (macros '())
           (funcs '()))
      (mapatoms
       (lambda (sym)
         (when (and (symbolp sym)
                    (string-prefix-p "cl-" (symbol-name sym)))
           (cond
            ((macrop sym) (push (symbol-name sym) macros))
            ((fboundp sym) (push (symbol-name sym) funcs))))))
      (setq macros (sort macros #'string<))
      (setq funcs (sort funcs #'string<))
      (setq files (sort files #'string<))
      (with-temp-buffer
        (org-mode)
        (insert (format "* cl-lib dependency report (%s)\n" root))
        (insert "** files\n")
        (insert "| file | cl- macros used | cl- functions used | require |\n")
        (insert "|------|-----------------|--------------------|---------|\n")
        (let (runtime-missing compile-missing require-unneeded)
          (dolist (file files)
            (when (file-regular-p file)
              (cl-destructuring-bind (macro-dep func-dep require-kind runtime-req eval-req-present macro-toks func-toks)
                  (cl-lib-deps-report--scan-file file symbol-re macros funcs)
                (when (and func-dep (not runtime-req))
                  (push (list file func-toks) runtime-missing))
                (when (and macro-dep (not eval-req-present))
                  (push (list file macro-toks) compile-missing))
                (when (and (not func-dep) (not macro-dep)
                           (or runtime-req eval-req-present))
                  (push file require-unneeded))
                (let ((skip
                       (or (and (not macro-dep) (not func-dep)
                                (string= require-kind "no"))
                           (and func-dep (string= require-kind "runtime"))
                           (and macro-dep (not func-dep)
                                (string= require-kind "compile-time")))))
                  (unless skip
                    (insert (format "| %s | %s | %s | %s |\n"
                                    file
                                    (if macro-dep "yes" "no")
                                    (if func-dep "yes" "no")
                                    require-kind)))))))
          (org-table-align)
          (insert "** runtime dependency missing require\n")
          (dolist (entry (sort runtime-missing (lambda (a b) (string< (car a) (car b)))))
            (insert (format "- %s (%s)\n"
                            (car entry)
                            (mapconcat (lambda (s) (format "~%s~" s)) (cadr entry) ", "))))
          (insert "\n** compile-time dependency missing eval-when-compile require\n")
          (dolist (entry (sort compile-missing (lambda (a b) (string< (car a) (car b)))))
            (insert (format "- %s (%s)\n"
                            (car entry)
                            (mapconcat (lambda (s) (format "~%s~" s)) (cadr entry) ", "))))
          (insert "\n** no dependency but require present\n")
          (dolist (f (sort require-unneeded #'string<))
            (insert (format "- %s\n" f)))
          (insert "\n* Summary\n")
          (insert (format "- Total files audited: %d\n" (length files)))
          (insert (format "- Redundant requires found: %d\n" (length require-unneeded)))
          (insert (format "- Missing runtime requires: %d\n" (length runtime-missing)))
          (insert (format "- Missing compile-time requires: %d\n" (length compile-missing))))
        (princ (buffer-string))))))

(cl-lib-deps-report--main command-line-args-left)

;;; cl-lib-deps-report.el ends here
