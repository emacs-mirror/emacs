;;; ob-emacs-lisp.el --- Babel Functions for Emacs-lisp Code -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; Org-Babel support for evaluating emacs-lisp code

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob-core)

(declare-function org-babel--get-vars "ob" (params))
(declare-function org-babel-result-cond "ob" (result-params scalar-form &rest table-forms))
(declare-function org-babel-reassemble-table "ob" (table colnames rownames))
(declare-function org-babel-pick-name "ob" (names selector))

(defconst org-babel-header-args:emacs-lisp '((lexical . :any))
  "Emacs-lisp specific header arguments.")

(defvar org-babel-default-header-args:emacs-lisp '((:lexical . "no"))
  "Default arguments for evaluating an emacs-lisp source block.

A value of \"yes\" or t causes source blocks to be eval'd using
lexical scoping.  It can also be an alist mapping symbols to
their value.  It is used both as the optional LEXICAL argument to
`eval', and as the value for `lexical-binding' in buffers created
by `org-edit-src-code'.")

(defun org-babel-expand-body:emacs-lisp (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(print-level nil)
	(print-length nil)
        (prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (if (null vars) (concat body "\n")
      (format "(let (%s)\n%s%s%s\n)"
	      (mapconcat
	       (lambda (var)
		 (format "%S" `(,(car var) ',(cdr var))))
	       vars "\n      ")
              (if prologue (concat prologue "\n      ") "")
	      body
              (if epilogue (concat "\n      " epilogue "\n") "")))))

(defun org-babel-execute:emacs-lisp (body params)
  "Execute emacs-lisp code BODY according to PARAMS."
  (let* ((lexical (cdr (assq :lexical params)))
         (session (cdr (assq :session params)))
	 (result-params (cdr (assq :result-params params)))
	 (body (format (if (member "output" result-params)
			   "(with-output-to-string %s\n)"
			 "(progn %s\n)")
		       (org-babel-expand-body:emacs-lisp body params)))
	 (result (eval (read (if (or (member "code" result-params)
				     (member "pp" result-params))
				 (concat "(pp " body ")")
			       body))
		       (org-babel-emacs-lisp-lexical lexical))))
    (when (and session (not (equal session "none")))
      (error "ob-emacs-lisp backend does not support sessions"))
    (org-babel-result-cond result-params
      (let ((print-level nil)
            (print-length nil))
        (if (or (member "scalar" result-params)
                (member "verbatim" result-params))
            (format "%S" result)
          (format "%s" result)))
      (org-babel-reassemble-table
       result
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

(defun org-babel-emacs-lisp-lexical (lexical)
  "Interpret :lexical source block argument.
Convert LEXICAL into the form appropriate for `lexical-binding'
and the LEXICAL argument to `eval'."
  (if (listp lexical)
      lexical
    (not (null (member lexical '("yes" "t"))))))

(defun org-babel-edit-prep:emacs-lisp (info)
  "Set `lexical-binding' in Org edit buffer.
Set `lexical-binding' in Org edit buffer according to the
corresponding :lexical source block argument provide in the INFO
channel, as returned by `org-babel-get-src-block-info'."
  (setq lexical-binding
        (org-babel-emacs-lisp-lexical
         (org-babel-read
          (cdr (assq :lexical (nth 2 info)))))))

(defun org-babel-prep-session:emacs-lisp (_session _params)
  "Return an error because we do not support sessions."
  (error "ob-emacs-lisp backend does not support sessions"))

(org-babel-make-language-alias "elisp" "emacs-lisp")

(provide 'ob-emacs-lisp)

;;; ob-emacs-lisp.el ends here
