;;; thingatpt.el --- Get the `thing' at point

;; Copyright (C) 1991,92,93,94,95,1996 Free Software Foundation, Inc.

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

;; This file provides routines for getting the "thing" at the location of
;; point, whatever that "thing" happens to be.  The "thing" is defined by
;; its beginning and end positions in the buffer.
;;
;; The function bounds-of-thing-at-point finds the beginning and end
;; positions by moving first forward to the end of the "thing", and then
;; backwards to the beginning.  By default, it uses the corresponding
;; forward-"thing" operator (eg. forward-word, forward-line).
;;
;; Special cases are allowed for using properties associated with the named
;; "thing": 
;;
;;   forward-op		Function to call to skip forward over a "thing" (or
;;                      with a negative argument, backward).
;;                      
;;   beginning-op	Function to call to skip to the beginning of a "thing".
;;   end-op		Function to call to skip to the end of a "thing".
;;
;; Reliance on existing operators means that many `things' can be accessed
;; without further code:  eg.
;;     (thing-at-point 'line)
;;     (thing-at-point 'page)

;;; Code:

(provide 'thingatpt)

;; Basic movement

;;;###autoload
(defun forward-thing (thing &optional n)
  "Move forward to the end of the next THING."
  (let ((forward-op (or (get thing 'forward-op)
			(intern-soft (format "forward-%s" thing)))))
    (if (fboundp forward-op)
	(funcall forward-op (or n 1))
      (error "Can't determine how to move over a %s" thing))))

;; General routines

;;;###autoload
(defun bounds-of-thing-at-point (thing)
  "Determine the start and end buffer locations for the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING.

The value is a cons cell (START . END) giving the start and end positions
of the textual entity that was found."
  (let ((orig (point)))
    (condition-case nil
	(save-excursion
	  (let ((end (progn 
		       (funcall 
			(or (get thing 'end-op) 
			    (function (lambda () (forward-thing thing 1)))))
		       (point)))
		(beg (progn 
		       (funcall 
			(or (get thing 'beginning-op) 
			    (function (lambda () (forward-thing thing -1)))))
		       (point))))
	    (if (and beg end (<= beg orig) (< orig end))
		(cons beg end))))
      (error nil))))

;;;###autoload
(defun thing-at-point (thing)
  "Return the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds 
	(buffer-substring (car bounds) (cdr bounds)))))

;; Go to beginning/end

(defun beginning-of-thing (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (car bounds))))

(defun end-of-thing (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (cdr bounds))))

;;  Special cases 

;;  Lines 

;; bolp will be false when you click on the last line in the buffer
;; and it has no final newline.

(put 'line 'beginning-op
     (function (lambda () (if (bolp) (forward-line -1) (beginning-of-line)))))

;;  Sexps 

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

;;  Lists 

(put 'list 'end-op (function (lambda () (up-list 1))))
(put 'list 'beginning-op 'backward-sexp)

;;  Filenames and URLs

(defvar thing-at-point-file-name-chars "~/A-Za-z0-9---_.${}#%,:"
  "Characters allowable in filenames.")

(put 'filename 'end-op    
     '(lambda () (skip-chars-forward thing-at-point-file-name-chars)))
(put 'filename 'beginning-op
     '(lambda () (skip-chars-backward thing-at-point-file-name-chars)))

(defvar thing-at-point-url-chars "~/A-Za-z0-9---_$%."
  "Characters allowable in a URL.")

(put 'url 'end-op    
     '(lambda () (skip-chars-forward thing-at-point-url-chars)
	(skip-chars-backward ".")))
(put 'url 'beginning-op
     '(lambda ()
	(skip-chars-backward thing-at-point-url-chars)
	(or (= (preceding-char) ?:)
	    (error "No URL here"))
	(forward-char -1)
	(skip-chars-backward "a-zA-Z")))

;;  Whitespace 

(defun forward-whitespace (arg)
  (interactive "p")
  (if (natnump arg) 
      (re-search-forward "[ \t]+\\|\n" nil nil arg)
    (while (< arg 0)
      (if (re-search-backward "[ \t]+\\|\n" nil nil)
	  (or (eq (char-after (match-beginning 0)) 10)
	      (skip-chars-backward " \t")))
      (setq arg (1+ arg)))))

;;  Buffer 

(put 'buffer 'end-op 'end-of-buffer)
(put 'buffer 'beginning-op 'beginning-of-buffer)

;;  Symbols 

(defun forward-symbol (arg)
  (interactive "p")
  (if (natnump arg) 
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil nil arg)
    (while (< arg 0)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil nil)
	  (skip-syntax-backward "w_"))
      (setq arg (1+ arg)))))

;;  Syntax blocks 

(defun forward-same-syntax (&optional arg)
  (interactive "p")
  (while (< arg 0)
    (skip-syntax-backward 
     (char-to-string (char-syntax (char-after (1- (point))))))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (skip-syntax-forward (char-to-string (char-syntax (char-after (point)))))
    (setq arg (1- arg))))

;;  Aliases 

(defun word-at-point () (thing-at-point 'word))
(defun sentence-at-point () (thing-at-point 'sentence))

(defun read-from-whole-string (str)
  "Read a lisp expression from STR.
Signal an error if the entire string was not used."
  (let* ((read-data (read-from-string str))
	 (more-left 
	  (condition-case nil
	      (progn (read-from-string (substring str (cdr read-data)))
		     t)
	    (end-of-file nil))))
    (if more-left
	(error "Can't read whole string")
      (car read-data))))

(defun form-at-point (&optional thing pred) 
  (let ((sexp (condition-case nil 
		  (read-from-whole-string (thing-at-point (or thing 'sexp)))
		(error nil))))
    (if (or (not pred) (funcall pred sexp)) sexp)))

(defun sexp-at-point ()   (form-at-point 'sexp))
(defun symbol-at-point () (form-at-point 'sexp 'symbolp))
(defun number-at-point () (form-at-point 'sexp 'numberp))
(defun list-at-point ()   (form-at-point 'list 'listp))

;; thingatpt.el ends here.
