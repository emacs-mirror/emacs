;;; ob-lisp.el --- org-babel functions for Common Lisp

;; Copyright (C) 2010 Free Software Foundation

;; Author: David T. O'Toole <dto@gnu.org>, Eric Schulte
;; Keywords: literate programming, reproducible research, lisp
;; Homepage: http://orgmode.org
;; Version: 7.4

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; Requires SLIME (Superior Lisp Interaction Mode for Emacs.)
;; See http://common-lisp.net/project/slime/

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(declare-function slime-eval "ext:slime" (sexp &optional package))
(declare-function slime-process "ext:slime" (&optional connection))
(declare-function slime-connected-p "ext:slime" ())

(defvar org-babel-default-header-args:lisp '()
  "Default header arguments for lisp code blocks.")

(defcustom org-babel-lisp-cmd "sbcl --script"
  "Name of command used to evaluate lisp blocks."
  :group 'org-babel
  :type 'string)

(defun org-babel-expand-body:lisp (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (if (> (length vars) 0)
        (concat "(let ("
                (mapconcat
                 (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
                 vars "\n      ")
                ")\n" body ")")
      body)))

(defun org-babel-execute:lisp (body params)
  "Execute a block of Lisp code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (require 'slime)
  (message "executing Lisp source code block")
  (let* ((session (org-babel-lisp-initiate-session
		   (cdr (assoc :session params))))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:lisp body params)))
    (read
     (if session
         ;; session evaluation
         (save-window-excursion
           (cadr (slime-eval `(swank:eval-and-grab-output ,full-body))))
       ;; external evaluation
       (let ((script-file (org-babel-temp-file "lisp-script-")))
         (with-temp-file script-file
           (insert
            ;; return the value or the output
            (if (string= result-type "value")
                (format "(print %s)" full-body)
              full-body)))
         (org-babel-eval
	  (format "%s %s" org-babel-lisp-cmd
		  (org-babel-process-file-name script-file)) ""))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:lisp (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "not yet implemented"))

(defun org-babel-lisp-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (require 'slime)
  (unless (string= session "none")
    (save-window-excursion
      (or (slime-connected-p)
	  (slime-process)))))

(provide 'ob-lisp)

;; arch-tag: 18086168-009f-4947-bbb5-3532375d851d

;;; ob-lisp.el ends here
