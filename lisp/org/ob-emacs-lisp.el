;;; ob-emacs-lisp.el --- org-babel functions for emacs-lisp code evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.01

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating emacs-lisp code

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:emacs-lisp
  '((:hlines . "yes") (:colnames . "no"))
  "Default arguments for evaluating an emacs-lisp source block.")

(declare-function org-babel-comint-with-output "ob-comint" (&rest body))
(declare-function org-babel-comint-buffer-livep "ob-comint" (buffer))
(declare-function org-babel-comint-wait-for-output "ob-comint" (buffer))
(declare-function org-babel-comint-in-buffer "ob-comint" (buffer &rest body))
(declare-function orgtbl-to-generic "org-table" (table params))

(defun org-babel-expand-body:emacs-lisp (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((processed-params (or processed-params (org-babel-process-params params)))
         (vars (nth 1 processed-params))
         (result-params (nth 2 processed-params))
         (print-level nil) (print-length nil)
         (body (if (> (length vars) 0)
		   (concat "(let ("
			 (mapconcat
			  (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
			  vars "\n      ")
			 ")\n" body ")")
		 body)))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(concat "(pp " body ")") body)))

(defun org-babel-execute:emacs-lisp (body params)
  "Execute a block of emacs-lisp code with Babel."
  (save-window-excursion
    (let ((processed-params (org-babel-process-params params)))
      (org-babel-reassemble-table
       (eval (read (format "(progn %s)"
			   (org-babel-expand-body:emacs-lisp
			    body params processed-params))))
       (org-babel-pick-name (nth 4 processed-params) (cdr (assoc :colnames params)))
       (org-babel-pick-name (nth 5 processed-params) (cdr (assoc :rownames params)))))))

(provide 'ob-emacs-lisp)

;; arch-tag: e9a3acca-dc84-472a-9f5a-23c35befbcd6

;;; ob-emacs-lisp.el ends here
