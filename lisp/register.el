;;; register.el --- register commands for Emacs

;; Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package of functions emulates and somewhat extends the venerable
;; TECO's `register' feature, which permits you to save various useful
;; pieces of buffer state to named variables.  The entry points are
;; documented in the Emacs user's manual.

;;; Code:

(defvar register-alist nil
  "Alist of elements (NAME . CONTENTS), one for each Emacs register.
NAME is a character (a number).  CONTENTS is a string, number, marker or list.
A list of strings represents a rectangle.
A list of the form (file . NAME) represents the file named NAME.
A list of the form (file-query NAME POSITION) represents position POSITION
 in the file named NAME, but query before visiting it.
A list of the form (WINDOW-CONFIGURATION POSITION)
 represents a saved window configuration plus a saved value of point.
A list of the form (FRAME-CONFIGURATION POSITION)
 represents a saved frame configuration plus a saved value of point.")

(defun get-register (reg)
  "Return contents of Emacs register named REG, or nil if none."
  (cdr (assq reg register-alist)))

(defun set-register (register value)
  "Set contents of Emacs register named REGISTER to VALUE.  Returns VALUE.
See the documentation of the variable `register-alist' for possible VALUE."
  (let ((aelt (assq register register-alist)))
    (if aelt
	(setcdr aelt value)
      (setq aelt (cons register value))
      (setq register-alist (cons aelt register-alist)))
    value))

(defun point-to-register (register &optional arg)
  "Store current location of point in register REGISTER.
With prefix argument, store current frame configuration.
Use \\[jump-to-register] to go to that location or restore that configuration.
Argument is a character, naming the register."
  (interactive "cPoint to register: \nP")
  (set-register register
		(if arg (list (current-frame-configuration) (point-marker))
		  (point-marker))))

(defun window-configuration-to-register (register &optional arg)
  "Store the window configuration of the selected frame in register REGISTER.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register."
  (interactive "cWindow configuration to register: \nP")
  ;; current-window-configuration does not include the value
  ;; of point in the current buffer, so record that separately.
  (set-register register (list (current-window-configuration) (point-marker))))

(defun frame-configuration-to-register (register &optional arg)
  "Store the window configuration of all frames in register REGISTER.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register."
  (interactive "cFrame configuration to register: \nP")
  ;; current-frame-configuration does not include the value
  ;; of point in the current buffer, so record that separately.
  (set-register register (list (current-frame-configuration) (point-marker))))

(defalias 'register-to-point 'jump-to-register)
(defun jump-to-register (register &optional delete)
  "Move point to location stored in a register.
If the register contains a file name, find that file.
 \(To put a file name in a register, you must use `set-register'.)
If the register contains a window configuration (one frame) or a frame
configuration (all frames), restore that frame or all frames accordingly.
First argument is a character, naming the register.
Optional second arg non-nil (interactively, prefix argument) says to
delete any existing frames that the frame configuration doesn't mention.
\(Otherwise, these frames are iconified.)"
  (interactive "cJump to register: \nP")
  (let ((val (get-register register)))
    (cond
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val) (not delete))
      (goto-char (cadr val)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((markerp val)
      (or (marker-buffer val)
	  (error "That register's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
	  (y-or-n-p (format "Visit file %s again? " (nth 1 val)))
	  (error "Register access aborted"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     (t
      (error "Register doesn't contain a buffer position or configuration")))))

;; Turn markers into file-query references when a buffer is killed.
(defun register-swap-out ()
  (and buffer-file-name
       (let ((tail register-alist))
	 (while tail
	   (and (markerp (cdr (car tail)))
		(eq (marker-buffer (cdr (car tail))) (current-buffer))
		(setcdr (car tail)
			(list 'file-query
			      buffer-file-name
			      (marker-position (cdr (car tail))))))
	   (setq tail (cdr tail))))))

(add-hook 'kill-buffer-hook 'register-swap-out)

(defun number-to-register (number register)
  "Store a number in a register.
Two args, NUMBER and REGISTER (a character, naming the register).
If NUMBER is nil, a decimal number is read from the buffer starting
at point, and point moves to the end of that number.
Interactively, NUMBER is the prefix arg (none means nil)."
  (interactive "P\ncNumber to register: ")
  (set-register register 
		(if number
		    (prefix-numeric-value number)
		  (if (looking-at "\\s-*-?[0-9]+")
		      (progn
			(goto-char (match-end 0))
			(string-to-int (match-string 0)))
		    0))))

(defun increment-register (number register)
  "Add NUMBER to the contents of register REGISTER.
Interactively, NUMBER is the prefix arg."
  (interactive "p\ncIncrement register: ")
  (or (numberp (get-register register))
      (error "Register does not contain a number"))
  (set-register register (+ number (get-register register))))

(defun view-register (register)
  "Display what is contained in register named REGISTER.
The Lisp value REGISTER is a character."
  (interactive "cView register: ")
  (let ((val (get-register register)))
    (if (null val)
	(message "Register %s is empty" (single-key-description register))
      (with-output-to-temp-buffer "*Output*"
	(describe-register-1 register t)))))

(defun list-registers ()
  "Display a list of nonempty registers saying briefly what they contain."
  (interactive)
  (let ((list (copy-sequence register-alist)))
    (setq list (sort list (lambda (a b) (< (car a) (car b)))))
    (with-output-to-temp-buffer "*Output*"
      (dolist (elt list)
	(when (get-register (car elt))
	  (describe-register-1 (car elt))
	  (terpri))))))

(defun describe-register-1 (register &optional verbose)
  (princ "Register ")
  (princ (single-key-description register))
  (princ " contains ")
  (let ((val (get-register register)))
    (cond
     ((numberp val)
      (princ val))

     ((markerp val)
      (let ((buf (marker-buffer val)))
	(if (null buf)
	    (princ "a marker in no buffer")
	  (princ "a buffer position:\n    buffer ")
	  (princ (buffer-name buf))
	  (princ ", position ")
	  (princ (marker-position val)))))

     ((and (consp val) (window-configuration-p (car val)))
      (princ "a window configuration."))

     ((and (consp val) (frame-configuration-p (car val)))
      (princ "a frame configuration."))

     ((and (consp val) (eq (car val) 'file))
      (princ "the file ")
      (prin1 (cdr val))
      (princ "."))

     ((and (consp val) (eq (car val) 'file-query))
      (princ "a file-query reference:\n    file ")
      (prin1 (car (cdr val)))
      (princ ",\n    position ")
      (princ (car (cdr (cdr val))))
      (princ "."))

     ((consp val)
      (if verbose
	  (progn
	    (princ "the rectangle:\n")
	    (while val
	      (princ "    ")
	      (princ (car val))
	      (terpri)
	      (setq val (cdr val))))
	(princ "a rectangle starting with ")
	(princ (car val))))

     ((stringp val)
      (setq val
	    (remove-list-of-text-properties 0 (length val)
					    yank-excluded-properties val))
      (if verbose
	  (progn
	    (princ "the text:\n")
	    (princ val))
	(princ "text starting with\n    ")
	(string-match "[^ \t\n].\\{,20\\}" val)
	(princ (match-string 0 val))))
     (t
      (princ "Garbage:\n")
      (if verbose (prin1 val))))))

(defun insert-register (register &optional arg)
  "Insert contents of register REGISTER.  (REGISTER is a character.)
Normally puts point before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and point after.
Interactively, second arg is non-nil if prefix arg is supplied."
  (interactive "*cInsert register: \nP")
  (push-mark)
  (let ((val (get-register register)))
    (cond
     ((consp val)
      (insert-rectangle val))
     ((stringp val)
      (insert-for-yank val))
     ((numberp val)
      (princ val (current-buffer)))
     ((and (markerp val) (marker-position val))
      (princ (marker-position val) (current-buffer)))
     (t
      (error "Register does not contain text"))))
  (if (not arg) (exchange-point-and-mark)))

(defun copy-to-register (register start end &optional delete-flag)
  "Copy region into register REGISTER.  With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to copy."
  (interactive "cCopy to register: \nr\nP")
  (set-register register (buffer-substring start end))
  (if delete-flag (delete-region start end)))

(defun append-to-register (register start end &optional delete-flag)
  "Append region to text in register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to register: \nr\nP")
  (or (stringp (get-register register))
      (error "Register does not contain text"))
  (set-register register (concat (get-register register)
			    (buffer-substring start end)))
  (if delete-flag (delete-region start end)))

(defun prepend-to-register (register start end &optional delete-flag)
  "Prepend region to text in register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to prepend."
  (interactive "cPrepend to register: \nr\nP")
  (or (stringp (get-register register))
      (error "Register does not contain text"))
  (set-register register (concat (buffer-substring start end)
			    (get-register register)))
  (if delete-flag (delete-region start end)))

(defun copy-rectangle-to-register (register start end &optional delete-flag)
  "Copy rectangular region into register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle."
  (interactive "cCopy rectangle to register: \nr\nP")
  (set-register register
		(if delete-flag
		    (delete-extract-rectangle start end)
		  (extract-rectangle start end))))

;;; register.el ends here
