;;; register.el --- register commands for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 1985, 1993-1994, 2001-2018 Free Software Foundation,
;; Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; This package of functions emulates and somewhat extends the venerable
;; TECO's `register' feature, which permits you to save various useful
;; pieces of buffer state to named variables.  The entry points are
;; documented in the Emacs user's manual: (info "(emacs) Registers").

(eval-when-compile (require 'cl-lib))

;;; Code:

;; FIXME: Clean up namespace usage!

(cl-defstruct
  (registerv (:constructor nil)
	     (:constructor registerv--make (&optional data print-func
						      jump-func insert-func))
	     (:copier nil))
  (data        nil :read-only t)
  (print-func  nil :read-only t)
  (jump-func   nil :read-only t)
  (insert-func nil :read-only t))

(cl-defun registerv-make (data &key print-func jump-func insert-func)
  "Create a register value object.

DATA can be any value.
PRINT-FUNC if provided controls how `list-registers' and
`view-register' print the register.  It should be a function
receiving one argument DATA and print text that completes
this sentence:
  Register X contains [TEXT PRINTED BY PRINT-FUNC]
JUMP-FUNC if provided, controls how `jump-to-register' jumps to the register.
INSERT-FUNC if provided, controls how `insert-register' insert the register.
They both receive DATA as argument."
  (declare (obsolete "Use your own type with methods on register-val-(insert|describe|jump-to)" "27.1"))
  (registerv--make data print-func jump-func insert-func))

(defvar register-alist nil
  "Alist of elements (NAME . CONTENTS), one for each Emacs register.
NAME is a character (a number).  CONTENTS is a string, number, marker, list
or a struct returned by `registerv-make'.
A list of strings represents a rectangle.
A list of the form (file . FILE-NAME) represents the file named FILE-NAME.
A list of the form (file-query FILE-NAME POSITION) represents
 position POSITION in the file named FILE-NAME, but query before
 visiting it.
A list of the form (WINDOW-CONFIGURATION POSITION)
 represents a saved window configuration plus a saved value of point.
A list of the form (FRAME-CONFIGURATION POSITION)
 represents a saved frame configuration plus a saved value of point.")

(defgroup register nil
  "Register commands."
  :group 'convenience
  :version "24.3")

(defcustom register-separator nil
  "Register containing the text to put between collected texts, or nil if none.

When collecting text with \\[append-to-register] (or \\[prepend-to-register]),
contents of this register is added to the beginning (or end, respectively)
of the marked text."
  :group 'register
  :type '(choice (const :tag "None" nil)
		 (character :tag "Use register" :value ?+)))

(defcustom register-preview-delay 1
  "If non-nil, time to wait in seconds before popping up a preview window.
If nil, do not show register previews, unless `help-char' (or a member of
`help-event-list') is pressed."
  :version "24.4"
  :type '(choice number (const :tag "No preview unless requested" nil))
  :group 'register)

(defun get-register (register)
  "Return contents of Emacs register named REGISTER, or nil if none."
  (alist-get register register-alist))

(defun set-register (register value)
  "Set contents of Emacs register named REGISTER to VALUE.  Returns VALUE.
See the documentation of the variable `register-alist' for possible VALUEs."
  (setf (alist-get register register-alist) value))

(defun register-describe-oneline (c)
  "One-line description of register C."
  (let ((d (replace-regexp-in-string
            "\n[ \t]*" " "
            (with-output-to-string (describe-register-1 c)))))
    (if (string-match "Register.+? contains \\(?:an? \\|the \\)?" d)
        (substring d (match-end 0))
      d)))

(defun register-preview-default (r)
  "Default function for the variable `register-preview-function'."
  (format "%s: %s\n"
	  (single-key-description (car r))
	  (register-describe-oneline (car r))))

(defvar register-preview-function #'register-preview-default
  "Function to format a register for previewing.
Takes one argument, a cons (NAME . CONTENTS) as found in `register-alist'.
Returns a string.")

(defun register-preview (buffer &optional show-empty)
  "Pop up a window to show register preview in BUFFER.
If SHOW-EMPTY is non-nil show the window even if no registers.
Format of each entry is controlled by the variable `register-preview-function'."
  (when (or show-empty (consp register-alist))
    (with-current-buffer-window
     buffer
     (cons 'display-buffer-below-selected
	   '((window-height . fit-window-to-buffer)
	     (preserve-size . (nil . t))))
     nil
     (with-current-buffer standard-output
       (setq cursor-in-non-selected-windows nil)
       (insert (mapconcat register-preview-function register-alist ""))))))

(defun register-read-with-preview (prompt)
  "Read and return a register name, possibly showing existing registers.
Prompt with the string PROMPT.  If `register-alist' and
`register-preview-delay' are both non-nil, display a window
listing existing registers after `register-preview-delay' seconds.
If `help-char' (or a member of `help-event-list') is pressed,
display such a window regardless."
  (let* ((buffer "*Register Preview*")
	 (timer (when (numberp register-preview-delay)
		  (run-with-timer register-preview-delay nil
				  (lambda ()
				    (unless (get-buffer-window buffer)
				      (register-preview buffer))))))
	 (help-chars (cl-loop for c in (cons help-char help-event-list)
			      when (not (get-register c))
			      collect c)))
    (unwind-protect
	(progn
	  (while (memq (read-key (propertize prompt 'face 'minibuffer-prompt))
		       help-chars)
	    (unless (get-buffer-window buffer)
	      (register-preview buffer 'show-empty)))
          (when (or (eq ?\C-g last-input-event)
                    (eq 'escape last-input-event)
                    (eq ?\C-\[ last-input-event))
            (keyboard-quit))
	  (if (characterp last-input-event) last-input-event
	    (error "Non-character input-event")))
      (and (timerp timer) (cancel-timer timer))
      (let ((w (get-buffer-window buffer)))
        (and (window-live-p w) (delete-window w)))
      (and (get-buffer buffer) (kill-buffer buffer)))))

(defun point-to-register (register &optional arg)
  "Store current location of point in register REGISTER.
With prefix argument, store current frame configuration.
Use \\[jump-to-register] to go to that location or restore that configuration.
Argument is a character, naming the register.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview
                      (if current-prefix-arg
                          "Frame configuration to register: "
                        "Point to register: "))
                     current-prefix-arg))
  ;; Turn the marker into a file-ref if the buffer is killed.
  (add-hook 'kill-buffer-hook 'register-swap-out nil t)
  (set-register register
		(if arg (list (current-frame-configuration) (point-marker))
		  (point-marker))))

(defun window-configuration-to-register (register &optional _arg)
  "Store the window configuration of the selected frame in register REGISTER.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview
		      "Window configuration to register: ")
		     current-prefix-arg))
  ;; current-window-configuration does not include the value
  ;; of point in the current buffer, so record that separately.
  (set-register register (list (current-window-configuration) (point-marker))))

;; It has had the optional arg for ages, but never used it.
(set-advertised-calling-convention 'window-configuration-to-register
				   '(register) "24.4")

(defun frame-configuration-to-register (register &optional _arg)
  "Store the window configuration of all frames in register REGISTER.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview
		      "Frame configuration to register: ")
		     current-prefix-arg))
  ;; current-frame-configuration does not include the value
  ;; of point in the current buffer, so record that separately.
  (set-register register (list (current-frame-configuration) (point-marker))))

;; It has had the optional arg for ages, but never used it.
(set-advertised-calling-convention 'frame-configuration-to-register
				   '(register) "24.4")

(make-obsolete 'frame-configuration-to-register 'frameset-to-register "24.4")

(defalias 'register-to-point 'jump-to-register)
(defun jump-to-register (register &optional delete)
  "Move point to location stored in a register.
If the register contains a file name, find that file.
\(To put a file name in a register, you must use `set-register'.)
If the register contains a window configuration (one frame) or a frameset
\(all frames), restore that frame or all frames accordingly.
First argument is a character, naming the register.
Optional second arg non-nil (interactively, prefix argument) says to
delete any existing frames that the frameset doesn't mention.
\(Otherwise, these frames are iconified.)

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Jump to register: ")
		     current-prefix-arg))
  (let ((val (get-register register)))
    (register-val-jump-to val delete)))

(cl-defgeneric register-val-jump-to (_val _arg)
  "Execute the \"jump\" operation of VAL.
ARG is the value of the prefix argument or nil."
  (user-error "Register doesn't contain a buffer position or configuration"))

(cl-defmethod register-val-jump-to ((val registerv) _arg)
  (cl-assert (registerv-jump-func val) nil
             "Don't know how to jump to register value %S" val)
  (funcall (registerv-jump-func val) (registerv-data val)))

(cl-defmethod register-val-jump-to ((val marker) _arg)
  (or (marker-buffer val)
      (user-error "That register's buffer no longer exists"))
  (switch-to-buffer (marker-buffer val))
  (unless (or (= (point) (marker-position val))
              (eq last-command 'jump-to-register))
    (push-mark))
  (goto-char val))

(cl-defmethod register-val-jump-to ((val cons) delete)
  (cond
   ((frame-configuration-p (car val))
    (set-frame-configuration (car val) (not delete))
    (goto-char (cadr val)))
   ((window-configuration-p (car val))
    (set-window-configuration (car val))
    (goto-char (cadr val)))
   ((eq (car val) 'file)
    (find-file (cdr val)))
   ((eq (car val) 'file-query)
    (or (find-buffer-visiting (nth 1 val))
	(y-or-n-p (format "Visit file %s again? " (nth 1 val)))
	(user-error "Register access aborted"))
    (find-file (nth 1 val))
    (goto-char (nth 2 val)))
   (t (cl-call-next-method val delete))))

(defun register-swap-out ()
  "Turn markers into file-query references when a buffer is killed."
  (and buffer-file-name
       (dolist (elem register-alist)
	 (and (markerp (cdr elem))
	      (eq (marker-buffer (cdr elem)) (current-buffer))
	      (setcdr elem
		      (list 'file-query
			    buffer-file-name
			    (marker-position (cdr elem))))))))

(defun number-to-register (number register)
  "Store a number in a register.
Two args, NUMBER and REGISTER (a character, naming the register).
If NUMBER is nil, a decimal number is read from the buffer starting
at point, and point moves to the end of that number.
Interactively, NUMBER is the prefix arg (none means nil).

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list current-prefix-arg
		     (register-read-with-preview "Number to register: ")))
  (set-register register
		(if number
		    (prefix-numeric-value number)
		  (if (looking-at "\\s-*-?[0-9]+")
		      (progn
			(goto-char (match-end 0))
			(string-to-number (match-string 0)))
		    0))))

(defun increment-register (prefix register)
  "Augment contents of REGISTER.
Interactively, PREFIX is in raw form.

If REGISTER contains a number, add `prefix-numeric-value' of
PREFIX to it.

If REGISTER is empty or if it contains text, call
`append-to-register' with `delete-flag' set to PREFIX.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list current-prefix-arg
		     (register-read-with-preview "Increment register: ")))
  (let ((register-val (get-register register)))
    (cond
     ((numberp register-val)
      (let ((number (prefix-numeric-value prefix)))
	(set-register register (+ number register-val))))
     ((or (not register-val) (stringp register-val))
      (append-to-register register (region-beginning) (region-end) prefix))
     (t (user-error "Register does not contain a number or text")))))

(defun view-register (register)
  "Display what is contained in register named REGISTER.
The Lisp value REGISTER is a character.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "View register: ")))
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
    (register-val-describe val verbose)))

(cl-defgeneric register-val-describe (val verbose)
  "Print description of register value VAL to `standard-output'."
  (princ "Garbage:\n")
  (if verbose (prin1 val)))

(cl-defmethod register-val-describe ((val registerv) _verbose)
  (if (registerv-print-func val)
      (funcall (registerv-print-func val) (registerv-data val))
    (princ "[UNPRINTABLE CONTENTS].")))

(cl-defmethod register-val-describe ((val number) _verbose)
  (princ val))

(cl-defmethod register-val-describe ((val marker) _verbose)
  (let ((buf (marker-buffer val)))
    (if (null buf)
	(princ "a marker in no buffer")
      (princ "a buffer position:\n    buffer ")
      (princ (buffer-name buf))
      (princ ", position ")
      (princ (marker-position val)))))

(cl-defmethod register-val-describe ((val cons) verbose)
  (cond
   ((window-configuration-p (car val))
    (princ "a window configuration."))

   ((frame-configuration-p (car val))
    (princ "a frame configuration."))

   ((eq (car val) 'file)
    (princ "the file ")
    (prin1 (cdr val))
    (princ "."))

   ((eq (car val) 'file-query)
    (princ "a file-query reference:\n    file ")
    (prin1 (car (cdr val)))
    (princ ",\n    position ")
    (princ (car (cdr (cdr val))))
    (princ "."))

   (t
    (if verbose
	(progn
	  (princ "the rectangle:\n")
	  (while val
	    (princ "    ")
	    (princ (car val))
	    (terpri)
	    (setq val (cdr val))))
      (princ "a rectangle starting with ")
      (princ (car val))))))

(cl-defmethod register-val-describe ((val string) verbose)
  (setq val (copy-sequence val))
  (if (eq yank-excluded-properties t)
      (set-text-properties 0 (length val) nil val)
    (remove-list-of-text-properties 0 (length val)
				    yank-excluded-properties val))
  (if verbose
      (progn
	(princ "the text:\n")
	(princ val))
    (cond
     ;; Extract first N characters starting with first non-whitespace.
     ((string-match (format "[^ \t\n].\\{,%d\\}"
			    ;; Deduct 6 for the spaces inserted below.
			    (min 20 (max 0 (- (window-width) 6))))
		    val)
      (princ "text starting with\n    ")
      (princ (match-string 0 val)))
     ((string-match "^[ \t\n]+$" val)
      (princ "whitespace"))
     (t
      (princ "the empty string")))))

(defun insert-register (register &optional arg)
  "Insert contents of register REGISTER.  (REGISTER is a character.)
Normally puts point before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and point after.
Interactively, second arg is nil if prefix arg is supplied and t
otherwise.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (register-read-with-preview "Insert register: ")
		       (not current-prefix-arg))))
  (push-mark)
  (let ((val (get-register register)))
    (register-val-insert val))
  (if (not arg) (exchange-point-and-mark)))

(cl-defgeneric register-val-insert (_val)
  "Insert register value VAL."
  (user-error "Register does not contain text"))

(cl-defmethod register-val-insert ((val registerv))
  (cl-assert (registerv-insert-func val) nil
             "Don't know how to insert register value %S" val)
  (funcall (registerv-insert-func val) (registerv-data val)))

(cl-defmethod register-val-insert ((val cons))
  (insert-rectangle val))

(cl-defmethod register-val-insert ((val string))
  (insert-for-yank val))

(cl-defmethod register-val-insert ((val number))
  (princ val (current-buffer)))

(cl-defmethod register-val-insert ((val marker))
  (if (marker-position val)
      (princ (marker-position val) (current-buffer))
    (cl-call-next-method val)))

(defun copy-to-register (register start end &optional delete-flag region)
  "Copy region into register REGISTER.
With prefix arg, delete as well.
Called from program, takes five args: REGISTER, START, END, DELETE-FLAG,
and REGION.  START and END are buffer positions indicating what to copy.
The optional argument REGION if non-nil, indicates that we're not just
copying some text between START and END, but we're copying the region.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Copy to register: ")
		     (region-beginning)
		     (region-end)
		     current-prefix-arg
		     t))
  (set-register register (if region
			     (funcall region-extract-function delete-flag)
			   (prog1 (filter-buffer-substring start end)
			     (if delete-flag (delete-region start end)))))
  (setq deactivate-mark t)
  (cond (delete-flag)
	((called-interactively-p 'interactive)
	 (indicate-copied-region))))

(defun append-to-register (register start end &optional delete-flag)
  "Append region to text in register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Append to register: ")
		     (region-beginning)
		     (region-end)
		     current-prefix-arg))
  (let ((reg (get-register register))
        (text (filter-buffer-substring start end))
	(separator (and register-separator (get-register register-separator))))
    (set-register
     register (cond ((not reg) text)
                    ((stringp reg) (concat reg separator text))
                    (t (user-error "Register does not contain text")))))
  (setq deactivate-mark t)
  (cond (delete-flag
	 (delete-region start end))
	((called-interactively-p 'interactive)
	 (indicate-copied-region))))

(defun prepend-to-register (register start end &optional delete-flag)
  "Prepend region to text in register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to prepend.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Prepend to register: ")
		     (region-beginning)
		     (region-end)
		     current-prefix-arg))
  (let ((reg (get-register register))
        (text (filter-buffer-substring start end))
	(separator (and register-separator (get-register register-separator))))
    (set-register
     register (cond ((not reg) text)
                    ((stringp reg) (concat text separator reg))
                    (t (user-error "Register does not contain text")))))
  (setq deactivate-mark t)
  (cond (delete-flag
	 (delete-region start end))
	((called-interactively-p 'interactive)
	 (indicate-copied-region))))

(defun copy-rectangle-to-register (register start end &optional delete-flag)
  "Copy rectangular region into register REGISTER.
With prefix arg, delete as well.
To insert this register in the buffer, use \\[insert-register].

Called from a program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview
		      "Copy rectangle to register: ")
		     (region-beginning)
		     (region-end)
		     current-prefix-arg))
  (let ((rectangle (if delete-flag
		       (delete-extract-rectangle start end)
		     (extract-rectangle start end))))
    (set-register register rectangle)
    (when (and (null delete-flag)
	       (called-interactively-p 'interactive))
      (setq deactivate-mark t)
      (indicate-copied-region (length (car rectangle))))))

(provide 'register)
;;; register.el ends here
