;; pp.el --- pretty printer for Emacs Lisp
;; Copyright (C) 1989, 1993 Free Software Foundation, Inc.

;; Author: Randal Schwartz <merlyn@ora.com>

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

;;; Code:

(defvar pp-escape-newlines t 
  "*Value of print-escape-newlines used by pp-* functions.")

(defun pp-to-string (object)
  "Return a string containing the pretty-printed representation of OBJECT,
any Lisp object.  Quoting characters are used when needed to make output
that `read' can handle, whenever this is possible."
  (save-excursion
    (set-buffer (generate-new-buffer " pp-to-string"))
    (unwind-protect
	(progn
	  (lisp-mode-variables)
	  (set-syntax-table emacs-lisp-mode-syntax-table)
	  (let ((print-escape-newlines pp-escape-newlines))
	    (prin1 object (current-buffer)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    ;; (message "%06d" (- (point-max) (point)))
	    (cond
	     ((looking-at "\\s\(")
	      (while (looking-at "\\s(")
		(forward-char 1)))
	     ((and (looking-at "\\(quote[ \t]+\\)\\([^.)]\\)")
		   (> (match-beginning 1) 1)
		   (= ?\( (char-after (1- (match-beginning 1))))
		   ;; Make sure this is a two-element list.
		   (save-excursion
		     (goto-char (match-beginning 2))
		     (forward-sexp)
		     ;; (looking-at "[ \t]*\)")
		     ;; Avoid mucking with match-data; does this test work?
		     (char-equal ?\) (char-after (point)))))
	      ;; -1 gets the paren preceding the quote as well.
	      (delete-region (1- (match-beginning 1)) (match-end 1))
	      (insert "'")
	      (forward-sexp 1)
	      (if (looking-at "[ \t]*\)")
		  (delete-region (match-beginning 0) (match-end 0))
		(error "Malformed quote"))
	      (backward-sexp 1))	      
	     ((condition-case err-var
		  (prog1 t (down-list 1))
		(error nil))
	      (backward-char 1)
	      (skip-chars-backward " \t")
	      (delete-region
	       (point)
	       (progn (skip-chars-forward " \t") (point)))
	      (if (not (char-equal ?' (char-after (1- (point)))))
		  (insert ?\n)))
	     ((condition-case err-var
		  (prog1 t (up-list 1))
		(error nil))
	      (while (looking-at "\\s)")
		(forward-char 1))
	      (skip-chars-backward " \t")
	      (delete-region
	       (point)
	       (progn (skip-chars-forward " \t") (point)))
	      (if (not (char-equal ?' (char-after (1- (point)))))
		  (insert ?\n)))
	     (t (goto-char (point-max)))))
	  (goto-char (point-min))
	  (indent-sexp)
	  (buffer-string))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun pp (object &optional stream)
  "Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)."
  (princ (pp-to-string object) (or stream standard-output)))

;;;###autoload
(defun pp-eval-expression (expression)
  "Evaluate EXPRESSION and pretty-print value into a new display buffer.
If the pretty-printed value fits on one line, the message line is used
instead.  Value is also consed on to front of variable  values 's
value."
  (interactive "xPp-eval: ")
  (setq values (cons (eval expression) values))
  (let* ((old-show-hook
	  (or (let ((sym (if (> (string-to-int emacs-version) 18)
			     'temp-buffer-show-function
			   'temp-buffer-show-hook)))
		(and (boundp 'sym) (symbol-value sym)))
	      'display-buffer))
	 (temp-buffer-show-hook
	  (function
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (goto-char (point-min))
	       (end-of-line 1)
	       (if (or (< (1+ (point)) (point-max))
		       (>= (- (point) (point-min)) (screen-width)))
		   (progn
		     (goto-char (point-min)) ; expected by some hooks ...
		     (funcall old-show-hook buf))
		 (message "%s" (buffer-substring (point-min) (point)))
		 (delete-windows-on buf) ; no need to kill it
		 )))))
	 (temp-buffer-show-function temp-buffer-show-hook)) ; emacs19 name
    (with-output-to-temp-buffer "*Pp Eval Output*"
      (pp (car values)))
    (save-excursion
      (set-buffer "*Pp Eval Output*")
      (emacs-lisp-mode))))

;;;###autoload
(defun pp-eval-last-sexp (arg)
  "Run `pp-eval-expression' on sexp before point (which see).
With argument, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (let ((stab (syntax-table)) (pt (point)) start exp)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (save-excursion
      (forward-sexp -1)
      ;; If first line is commented, ignore all leading comments:
      (if (save-excursion (beginning-of-line) (looking-at "[ \t]*;"))
	  (progn
	    (setq exp (buffer-substring (point) pt))
	    (while (string-match "\n[ \t]*;+" exp start)
	      (setq start (1+ (match-beginning 0))
		    exp (concat (substring exp 0 start)
				(substring exp (match-end 0)))))
	    (setq exp (read exp)))
	(setq exp (read (current-buffer)))))
    (set-syntax-table stab)
    (if arg
	(insert (pp-to-string (eval exp)))
      (pp-eval-expression exp))))

;;; Test cases for quote
;; (pp-eval-expression ''(quote quote))
;; (pp-eval-expression ''((quote a) (quote b)))
;; (pp-eval-expression ''('a 'b))	; same as above
;; (pp-eval-expression ''((quote (quote quote)) (quote quote)))
;; These do not satisfy the quote test.
;; (pp-eval-expression ''quote)
;; (pp-eval-expression ''(quote))
;; (pp-eval-expression ''(quote . quote))
;; (pp-eval-expression ''(quote a b))
;; (pp-eval-expression ''(quotefoo))
;; (pp-eval-expression ''(a b))

(provide 'pp)				; so (require 'pp) works

;;; pp.el ends here.
