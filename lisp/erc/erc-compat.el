;;; erc-compat.el --- ERC compatibility code for XEmacs

;; Copyright (C) 2002, 2003, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ERC

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This mostly defines stuff that cannot be worked around easily.

;;; Code:

(require 'format-spec)

;;;###autoload (autoload 'erc-define-minor-mode "erc-compat")
(defalias 'erc-define-minor-mode 'define-minor-mode)
(put 'erc-define-minor-mode 'edebug-form-spec 'define-minor-mode)

(defun erc-decode-coding-string (s coding-system)
  "Decode S using CODING-SYSTEM."
  (decode-coding-string s coding-system t))

(defun erc-encode-coding-string (s coding-system)
  "Encode S using CODING-SYSTEM.
Return the same string, if the encoding operation is trivial.
See `erc-encoding-coding-alist'."
  (encode-coding-string s coding-system t))

(defalias 'erc-propertize 'propertize)
(defalias 'erc-view-mode-enter 'view-mode-enter)
(defalias 'erc-function-arglist 'help-function-arglist)
(defalias 'erc-delete-dups 'delete-dups)
(defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string)

(defvar erc-emacs-build-time
  (if (stringp emacs-build-time)
      emacs-build-time
    (format-time-string "%Y-%m-%d" emacs-build-time))
  "Time at which Emacs was dumped out.")

;; XEmacs' `replace-match' does not replace matching subexpressions in strings.
(defun erc-replace-match-subexpression-in-string
  (newtext string match subexp start &optional fixedcase literal)
  "Replace the subexpression SUBEXP of the last match in STRING with NEWTEXT.
MATCH is the text which matched the subexpression (see `match-string').
START is the beginning position of the last match (see `match-beginning').
See `replace-match' for explanations of FIXEDCASE and LITERAL."
  (cond ((featurep 'xemacs)
	 (string-match match string start)
	 (replace-match newtext fixedcase literal string))
	(t (replace-match newtext fixedcase literal string subexp))))

(defalias 'erc-cancel-timer 'cancel-timer)
(defalias 'erc-make-obsolete 'make-obsolete)
(defalias 'erc-make-obsolete-variable 'make-obsolete-variable)

;; Provde an equivalent of `assert', based on the code from cl-macs.el
(defun erc-const-expr-p (x)
  (cond ((consp x)
	 (or (eq (car x) 'quote)
	     (and (memq (car x) '(function function*))
		  (or (symbolp (nth 1 x))
		      (and (eq (and (consp (nth 1 x))
				    (car (nth 1 x))) 'lambda) 'func)))))
	((symbolp x) (and (memq x '(nil t)) t))
	(t t)))

(put 'erc-assertion-failed 'error-conditions '(error))
(put 'erc-assertion-failed 'error-message "Assertion failed")

(defun erc-list* (arg &rest rest)
  "Return a new list with specified args as elements, cons'd to last arg.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'."
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defmacro erc-assert (form &optional show-args string &rest args)
  "Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used."
  (let ((sargs
	 (and show-args
	      (delq nil (mapcar
			 (function
			  (lambda (x)
			    (and (not (erc-const-expr-p x)) x)))
			 (cdr form))))))
    (list 'progn
	  (list 'or form
		(if string
		    (erc-list* 'error string (append sargs args))
		  (list 'signal '(quote erc-assertion-failed)
			(erc-list* 'list (list 'quote form) sargs))))
	  nil)))

;; Provide a simpler replacement for `member-if'
(defun erc-member-if (predicate list)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches."
  (let ((ptr list))
    (catch 'found
      (while ptr
	(when (funcall predicate (car ptr))
	  (throw 'found ptr))
	(setq ptr (cdr ptr))))))

;; Provide a simpler replacement for `delete-if'
(defun erc-delete-if (predicate seq)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
  ;; remove from car
  (while (when (funcall predicate (car seq))
	   (setq seq (cdr seq))))
  ;; remove from cdr
  (let ((ptr seq)
	(next (cdr seq)))
    (while next
      (when (funcall predicate (car next))
	(setcdr ptr (if (consp next)
			(cdr next)
		      nil)))
      (setq ptr (cdr ptr))
      (setq next (cdr ptr))))
  seq)

;; Provide a simpler replacement for `remove-if-not'
(defun erc-remove-if-not (predicate seq)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ to
avoid corrupting the original SEQ."
  (let (newseq)
    (dolist (el seq)
      (when (funcall predicate el)
	(setq newseq (cons el newseq))))
    (nreverse newseq)))

;; Copied from cl-extra.el
(defun erc-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))

(provide 'erc-compat)

;;; erc-compat.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: 8948ffe0-aff8-4ad8-a196-368ebbfd58ff
