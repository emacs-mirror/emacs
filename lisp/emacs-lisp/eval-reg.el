;;; eval-reg.el --- Redefine eval-region, and subrs that use it, in Lisp

;; Copyright (C) 1994 Daniel LaLiberte

;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>
;; Keywords: lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Commentary:

;;; eval-region, eval-buffer, and eval-current-buffer are redefined in
;;; Lisp to allow customizations by Lisp code.  eval-region calls
;;; `read', `eval', and `prin1', so Lisp replacements of these
;;; functions will affect eval-region and anything else that calls it.
;;; eval-buffer and eval-current-buffer are redefined in Lisp to call
;;; eval-region on the buffer.  

;;; Because of dynamic binding, all local variables are protected from
;;; being seen by eval by giving them funky names.  But variables in
;;; routines that call eval-region are similarly exposed.

;;; Perhaps this should be one of several files in an `elisp' package
;;; that replaces Emacs Lisp subroutines with Lisp versions of the
;;; same.

;;; Eval-region may be installed, after loading, by calling:
;;; (elisp-eval-region-install).  Installation can be undone with:
;;; (elisp-eval-region-uninstall).

'(defpackage "elisp-eval-region"
   (:nicknames "elisp")
   (:use "elisp")
   (:export
    elisp-eval-region-install
    elisp-eval-region-uninstall
    elisp-eval-region-level
    with-elisp-eval-region
    eval-region
    eval-buffer
    eval-current-buffer
    ))
'(in-package elisp-eval-region)

;; Save standard versions.
(if (not (fboundp 'original-eval-region))
    (defalias 'original-eval-region (symbol-function 'eval-region)))
(if (not (fboundp 'original-eval-buffer))
    (defalias 'original-eval-buffer 
	  (if (fboundp 'eval-buffer)  ;; only in Emacs 19
	      (symbol-function 'eval-buffer)
	    'undefined)))
(if (not (fboundp 'original-eval-current-buffer))
    (defalias 'original-eval-current-buffer
	  (symbol-function 'eval-current-buffer)))

(defvar elisp-eval-region-level 0
  "If the value is 0, use the original version of `elisp-eval-region'.
Callers of `elisp-eval-region' should increment `elisp-eval-region-level'
while the Lisp version should be used.  Installing `elisp-eval-region'
increments it once, and uninstalling decrements it.")

;; Installing and uninstalling should always be used in pairs, 
;; or just install once and never uninstall. 
(defun elisp-eval-region-install ()
  (interactive)
  (defalias 'eval-region 'elisp-eval-region)
  (defalias 'eval-buffer 'elisp-eval-buffer)
  (defalias 'eval-current-buffer 'elisp-eval-current-buffer)
  (setq elisp-eval-region-level (1+ elisp-eval-region-level)))

(defun elisp-eval-region-uninstall ()
  (interactive)
  (if (> 1 elisp-eval-region-level)
      (setq elisp-eval-region-level (1- elisp-eval-region-level))
    (setq elisp-eval-region-level 0)
    (defalias 'eval-region (symbol-function 'original-eval-region))
    (defalias 'eval-buffer (symbol-function 'original-eval-buffer))
    (defalias 'eval-current-buffer 
      (symbol-function 'original-eval-current-buffer))
    ))

(put 'with-elisp-eval-region 'lisp-indent-function 1)
(put 'with-elisp-eval-region 'lisp-indent-hook 1)
(put 'with-elisp-eval-region 'edebug-form-spec t)

(defmacro with-elisp-eval-region (flag &rest body)
  "If FLAG is nil, decrement `eval-region-level' while executing BODY.
The effect of decrementing all the way to zero is that `eval-region'
will use the original `eval-region', which may be the Emacs subr or some
previous redefinition.  Before calling this macro, this package should
already have been installed, using `elisp-eval-region-install', which
increments the count once.  So if another package still requires the
Lisp version of the code, the count will still be non-zero.

The count is not bound locally by this macro, so changes by BODY to
its value will not be lost."
  (` (let ((elisp-code (function (lambda () (,@ body)))))
       (if (not (, flag))
	   (unwind-protect
	       (progn
		 (setq elisp-eval-region-level (1- elisp-eval-region-level))
		 (funcall elisp-code))
	     (setq elisp-eval-region-level (1+ elisp-eval-region-level)))
	 (funcall elisp-code)))))


(defun elisp-eval-region (elisp-start elisp-end &optional elisp-output)
  "Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print.

This version, from `eval-reg.el', allows Lisp customization of read,
eval, and the printer."

  ;; Because this doesnt narrow to the region, one other difference 
  ;; concerns inserting whitespace after the expression being evaluated.

  (interactive "r")
  (if (= 0 elisp-eval-region-level)
      (original-eval-region elisp-start elisp-end elisp-output)
    (let ((elisp-pnt (point))
	  (elisp-buf (current-buffer));; Outside buffer
	  (elisp-inside-buf (current-buffer));; Buffer current while evaling
	  ;; Mark the end because it may move.
	  (elisp-end-marker (set-marker (make-marker) elisp-end))
	  elisp-form
	  elisp-val)
      (goto-char elisp-start)
      (elisp-skip-whitespace)
      (while (< (point) elisp-end-marker)
	(setq elisp-form (read elisp-buf))

	(let ((elisp-current-buffer (current-buffer)))
	  ;; Restore the inside current-buffer.
	  (set-buffer elisp-inside-buf)
	  (setq elisp-val (eval elisp-form))
	  ;; Remember current buffer for next time.
	  (setq elisp-inside-buf (current-buffer))
	  ;; Should this be protected?
	  (set-buffer elisp-current-buffer))

	(if elisp-output
	    (let ((standard-output (or elisp-output t)))
	      (setq values (cons elisp-val values))
	      (if (eq standard-output t)
		  (prin1 elisp-val)
		(princ "\n")
		(prin1 elisp-val)
		(princ "\n")
		)))
	(goto-char (min (max elisp-end-marker (point))
			(progn (elisp-skip-whitespace) (point))))
	)				; while
      (if elisp-output nil
	;; like save-excursion recovery, but done only if no error occurs
	;; but mark is not restored
	(set-buffer elisp-buf)
	(goto-char elisp-pnt))
      nil)))


(defun elisp-skip-whitespace ()
  ;; Leave point before the next token, skipping white space and comments.
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n\r")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))


(defun elisp-eval-current-buffer (&optional elisp-output)
  "Execute the current buffer as Lisp code.
Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print.

This version calls `eval-region' on the whole buffer."
  ;; The standard eval-current-buffer doesn't use eval-region.
  (interactive)
  (eval-region (point-min) (point-max) elisp-output))


(defun elisp-eval-buffer (&optional elisp-bufname elisp-printflag)
  "Execute BUFFER as Lisp code.  Use current buffer if BUFFER is nil.
Programs can pass argument PRINTFLAG which controls printing of
output: nil means discard it; anything else is stream for print.

This version calls `eval-region' on the whole buffer."
  (interactive)
  (if (null elisp-bufname)
      (setq elisp-bufname (current-buffer)))
  (save-excursion
    (set-buffer (or (get-buffer elisp-bufname) 
		    (error "No such buffer: %s" elisp-bufname)))
    (eval-region (point-min) (point-max) elisp-printflag)))


(provide 'eval-reg)

;;; eval-reg.el ends here
