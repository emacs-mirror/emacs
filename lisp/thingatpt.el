;;; thingatpt.el --- Get the `thing' at point

;; Copyright (C) 1991,1992,1993 Free Software Foundation, Inc.

;; Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;; Keywords: extensions, matching, mouse
;; Created: Thu Mar 28 13:48:23 1991

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; This file provides routines for getting the `thing' at the location of
;; point, whatever that `thing' happens to be.  The `thing' is defined by
;; it's beginning and end positions in the buffer.
;;
;; The function bounds-of-thing-at-point finds the beginning and end
;; positions by moving first forward to the end of the `thing', and then
;; backwards to the beginning.  By default, it uses the corresponding
;; forward-`thing' operator (eg. forward-word, forward-line).
;;
;; Special cases are allowed for using properties associated with the named
;; `thing': 
;;
;;   forward-op		Function to call to skip forward over a `thing' (or
;;                      with a negative argument, backward).
;;                      
;;   beginning-op	Function to call to skip to the beginning of a `thing'.
;;   end-op		Function to call to skip to the end of a `thing'.
;;
;; Reliance on existing operators means that many `things' can be accessed
;; without further code:  eg.
;;     (thing-at-point 'line)
;;     (thing-at-point 'page)

;;; Code: =================================================================

(provide 'thingatpt)

;;=== Basic movement ======================================================

;;;###autoload
(defun forward-thing (THING &optional N)
  "Move forward to the end of the next THING."
  (let ((forward-op (or (get THING 'forward-op)
			(intern-soft (format "forward-%s" THING)))))
    (if (fboundp forward-op)
	(funcall forward-op (or N 1))
      (error "Can't determine how to move over %ss" THING))))

;;=== General routines ====================================================

;;;###autoload
(defun bounds-of-thing-at-point (THING)
  "Determine the start and end buffer locations for the THING at point,
where THING is an entity for which there is a either a corresponding
forward-THING operation, or corresponding beginning-of-THING and
end-of-THING operations, eg. 'word, 'sentence, 'defun.
  Return a cons cell '(start . end) giving the start and end positions."
  (let ((orig (point)))
    (condition-case nil
	(save-excursion
	  (let ((end (progn 
		       (funcall 
			(or (get THING 'end-op) 
			    (function (lambda () (forward-thing THING 1)))))
		       (point)))
		(beg (progn 
		       (funcall 
			(or (get THING 'beginning-op) 
			    (function (lambda () (forward-thing THING -1)))))
		       (point))))
	    (if (and beg end (<= beg orig) (< orig end))
		(cons beg end))))
      (error nil))))

;;;###autoload
(defun thing-at-point (THING)
  "Return the THING at point, where THING is an entity defined by
bounds-of-thing-at-point."
  (let ((bounds (bounds-of-thing-at-point THING)))
    (if bounds 
	(buffer-substring (car bounds) (cdr bounds)))))

;;=== Go to beginning/end =================================================

(defun beginning-of-thing (THING)
  (let ((bounds (bounds-of-thing-at-point THING)))
    (or bounds (error "No %s here" THING))
    (goto-char (car bounds))))

(defun end-of-thing (THING)
  (let ((bounds (bounds-of-thing-at-point THING)))
    (or bounds (error "No %s here" THING))
    (goto-char (cdr bounds))))

;;=== Special cases =======================================================

;;--- Sexps ---

(defun in-string-p ()
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) orig)))))

(defun end-of-sexp ()
  (let ((char-syntax (char-syntax (char-after (point)))))
    (if (or (eq char-syntax ?\))
	    (and (eq char-syntax ?\") (in-string-p)))
	(forward-char 1)
      (forward-sexp 1))))

(put 'sexp 'end-op 'end-of-sexp)

;;--- Lists ---

(put 'list 'end-op (function (lambda () (up-list 1))))
(put 'list 'beginning-op 'backward-sexp)

;;--- Filenames ---

(defvar file-name-chars "~/A-Za-z0-9---_.${}#%,"
  "Characters allowable in filenames.")

(put 'filename 'end-op    
     (function (lambda () (skip-chars-forward file-name-chars))))
(put 'filename 'beginning-op
     (function (lambda () (skip-chars-backward file-name-chars (point-min)))))

;;--- Whitespace ---

(defun forward-whitespace (ARG)
  (interactive "p")
  (if (natnump ARG) 
      (re-search-forward "[ \t]+\\|\n" nil nil ARG)
    (while (< ARG 0)
      (if (re-search-backward "[ \t]+\\|\n" nil nil)
	  (or (eq (char-after (match-beginning 0)) 10)
	      (skip-chars-backward " \t")))
      (setq ARG (1+ ARG)))))

;;--- Buffer ---

(put 'buffer 'end-op 'end-of-buffer)
(put 'buffer 'beginning-op 'beginning-of-buffer)

;;--- Symbols ---

(defun forward-symbol (ARG)
  (interactive "p")
  (if (natnump ARG) 
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil nil ARG)
    (while (< ARG 0)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil nil)
	  (skip-syntax-backward "w_"))
      (setq ARG (1+ ARG)))))

;;=== Aliases =============================================================

(defun word-at-point () (thing-at-point 'word))
(defun sentence-at-point () (thing-at-point 'sentence))

(defun read-from-whole-string (STR)
  "Read a lisp expression from STR, signalling an error if the entire string
was not used."
  (let* ((read-data (read-from-string STR))
	 (more-left 
	  (condition-case nil
	      (progn (read-from-string (substring STR (cdr read-data)))
		     t)
	    (end-of-file nil))))
    (if more-left
	(error "Can't read whole string")
      (car read-data))))

(defun form-at-point (&optional THING PRED) 
  (let ((sexp (condition-case nil 
		  (read-from-whole-string (thing-at-point (or THING 'sexp)))
		(error nil))))
    (if (or (not PRED) (funcall PRED sexp)) sexp)))

(defun sexp-at-point ()   (form-at-point 'sexp))
(defun symbol-at-point () (form-at-point 'sexp 'symbolp))
(defun number-at-point () (form-at-point 'sexp 'numberp))
(defun list-at-point ()   (form-at-point 'list 'listp))

;; thingatpt.el ends here.
