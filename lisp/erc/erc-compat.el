;;; erc-compat.el --- ERC compatibility code for older Emacsen  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2003, 2005-2022 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>
;; URL: https://www.emacswiki.org/emacs/ERC

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

;; This mostly defines stuff that cannot be worked around easily.

;;; Code:

(require 'format-spec)

;;;###autoload(autoload 'erc-define-minor-mode "erc-compat")
(define-obsolete-function-alias 'erc-define-minor-mode
  #'define-minor-mode "28.1")

(defun erc-decode-coding-string (s coding-system)
  "Decode S using CODING-SYSTEM."
  (declare (obsolete decode-coding-string "28.1"))
  (decode-coding-string s coding-system t))

(defun erc-encode-coding-string (s coding-system)
  "Encode S using CODING-SYSTEM.
Return the same string, if the encoding operation is trivial.
See `erc-encoding-coding-alist'."
  (declare (obsolete encode-coding-string "28.1"))
  (encode-coding-string s coding-system t))

(define-obsolete-function-alias 'erc-propertize #'propertize "28.1")
(define-obsolete-function-alias 'erc-view-mode-enter #'view-mode-enter "28.1")
(autoload 'help-function-arglist "help-fns")
(define-obsolete-function-alias 'erc-function-arglist #'help-function-arglist "28.1")
(define-obsolete-function-alias 'erc-delete-dups #'delete-dups "28.1")
(define-obsolete-function-alias 'erc-replace-regexp-in-string #'replace-regexp-in-string "28.1")

(defun erc-set-write-file-functions (new-val)
  (declare (obsolete nil "28.1"))
  (set (make-local-variable 'write-file-functions) new-val))

(defvar erc-emacs-build-time
  (if (or (stringp emacs-build-time) (not emacs-build-time))
      emacs-build-time
    (format-time-string "%Y-%m-%d" emacs-build-time))
  "Time at which Emacs was dumped out, or nil if not available.")
(make-obsolete-variable 'erc-emacs-build-time 'emacs-build-time "28.1")
(define-obsolete-variable-alias 'erc-user-emacs-directory 'user-emacs-directory "28.1")

(defun erc-replace-match-subexpression-in-string
  (newtext string _match subexp _start &optional fixedcase literal)
  "Replace the subexpression SUBEXP of the last match in STRING with NEWTEXT.
MATCH is the text which matched the subexpression (see `match-string').
START is the beginning position of the last match (see `match-beginning').
See `replace-match' for explanations of FIXEDCASE and LITERAL."
  (declare (obsolete replace-match "28.1"))
  (replace-match newtext fixedcase literal string subexp))

(define-obsolete-function-alias 'erc-with-selected-window
  #'with-selected-window "28.1")
(define-obsolete-function-alias 'erc-cancel-timer #'cancel-timer "28.1")
(define-obsolete-function-alias 'erc-make-obsolete #'make-obsolete "28.1")
(define-obsolete-function-alias 'erc-make-obsolete-variable
  #'make-obsolete-variable "28.1")

;; Provide a simpler replacement for `cl-member-if'
(defun erc-member-if (predicate list)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches."
  (declare (obsolete cl-member-if "28.1"))
  (let ((ptr list))
    (catch 'found
      (while ptr
	(when (funcall predicate (car ptr))
	  (throw 'found ptr))
	(setq ptr (cdr ptr))))))

;; Provide a simpler replacement for `cl-delete-if'
(defun erc-delete-if (predicate seq)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
  (declare (obsolete cl-delete-if "28.1"))
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

;; Provide a simpler replacement for `cl-remove-if-not'
(defun erc-remove-if-not (predicate seq)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ to
avoid corrupting the original SEQ."
  (declare (obsolete cl-remove-if-not "28.1"))
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
  (declare (obsolete cl-subseq "28.1"))
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
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
