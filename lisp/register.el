;;; register.el --- register commands for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 1985, 1993-1994, 2001-2023 Free Software Foundation,
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

(declare-function frameset-register-p "frameset")

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
A list of the form (buffer . BUFFER-NAME) represents the buffer BUFFER-NAME.
A list of the form (WINDOW-CONFIGURATION POSITION)
 represents a saved window configuration plus a saved value of point.
A list of the form (FRAME-CONFIGURATION POSITION)
 represents a saved frame configuration (a.k.a. \"frameset\") plus
 a saved value of point.")

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
  "If non-nil, time to wait in seconds before popping up register preview window.
If nil, do not show register previews, unless `help-char' (or a member of
`help-event-list') is pressed.

This variable has no effect when `register-use-preview' is set to any
value except \\='traditional."
  :version "24.4"
  :type '(choice number (const :tag "No preview unless requested" nil))
  :group 'register)

(defcustom register-preview-default-keys (mapcar #'string (number-sequence ?a ?z))
  "Default keys for setting a new register."
  :type '(repeat string)
  :version "30.1")

(defvar register--read-with-preview-function nil
  "Function to use for reading a register name with preview.
Two functions are provided, one that provide navigation and highlighting
of the selected register, filtering of register according to command in
use, defaults register to use when setting a new register, confirmation
and notification when you are about to overwrite a register, and generic
functions to configure how each existing command behaves.  Use the
function `register-read-with-preview-fancy' for this.  The other
provided function, `register-read-with-preview-traditional', behaves
the same as in Emacs 29 and before: no filtering, no navigation,
and no defaults.")

(defvar register-preview-function nil
  "Function to format a register for previewing.
Called with one argument, a cons (NAME . CONTENTS), as found
in `register-alist'.  The function should return a string, the
description of the argument.  The function to use is set according
to the value of `register--read-with-preview-function'.")

(defcustom register-use-preview 'traditional
  "Whether to show register preview when modifying registers.

When set to `t', show a preview buffer with navigation and highlighting.
When nil, show a preview buffer without navigation and highlighting, and
exit the minibuffer immediately after inserting response in minibuffer.
When set to \\='never, behave as with nil, but with no preview buffer at
all; the preview buffer is still accessible with `help-char' (C-h).
When set to \\='traditional (the default), provide a more basic preview
according to `register-preview-delay'; this preserves the traditional
behavior of Emacs 29 and before."
  :type '(choice
          (const :tag "Use preview" t)
          (const :tag "Use quick preview" nil)
          (const :tag "Never use preview" never)
          (const :tag "Basic preview like Emacs-29" traditional))
  :version "30.1"
  :set (lambda (var val)
         (set var val)
         (setq register--read-with-preview-function
               (if (eq val 'traditional)
                   #'register-read-with-preview-traditional
                 #'register-read-with-preview-fancy))
         (setq register-preview-function nil)))

(defun get-register (register)
  "Return contents of Emacs register named REGISTER, or nil if none."
  (alist-get register register-alist))

(defun set-register (register value)
  "Set contents of Emacs register named REGISTER to VALUE, return VALUE.
See the documentation of the variable `register-alist' for possible VALUEs."
  (setf (alist-get register register-alist) value))

(defun register-describe-oneline (c)
  "Return a one-line description of register C."
  (let ((d (replace-regexp-in-string
            "\n[ \t]*" " "
            (with-output-to-string (describe-register-1 c)))))
    (if (string-match "Register.+? contains \\(?:an? \\|the \\)?" d)
        (substring d (match-end 0))
      d)))

(defun register-preview-default-1 (r)
  "Function used to format a register for fancy previewing.
This is used as the value of the variable `register-preview-function'
when `register-use-preview' is set to t or nil."
  (format "%s: %s\n"
	  (propertize (string (car r))
                      'display (single-key-description (car r)))
	  (register-describe-oneline (car r))))

(defun register-preview-default (r)
  "Function used to format a register for traditional preview.
This is the default value of the variable `register-preview-function',
and is used when `register-use-preview' is set to \\='traditional."
  (format "%s: %s\n"
	  (single-key-description (car r))
	  (register-describe-oneline (car r))))

(cl-defgeneric register--preview-function (read-preview-function)
  "Return a function to format registers for previewing by READ-PREVIEW-FUNCTION.")
(cl-defmethod register--preview-function ((_read-preview-function
                                           (eql register-read-with-preview-traditional)))
  #'register-preview-default)
(cl-defmethod register--preview-function ((_read-preview-function
                                           (eql register-read-with-preview-fancy)))
  #'register-preview-default-1)

(cl-defstruct register-preview-info
  "Store data for a specific register command.
TYPES are the supported types of registers.
MSG is the minibuffer message to show when a register is selected.
ACT is the type of action the command is doing on register.
SMATCH accept a boolean value to say if the command accepts non-matching
registers.
If NOCONFIRM is non-nil, request confirmation of register name by RET."
  types msg act smatch noconfirm)

(cl-defgeneric register-command-info (command)
  "Return a `register-preview-info' object storing data for COMMAND."
  (ignore command))
(cl-defmethod register-command-info ((_command (eql insert-register)))
  (make-register-preview-info
   :types '(string number)
   :msg "Insert register `%s'"
   :act 'insert
   :smatch t
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info ((_command (eql jump-to-register)))
  (make-register-preview-info
   :types  '(window frame marker kmacro
             file buffer file-query)
   :msg "Jump to register `%s'"
   :act 'jump
   :smatch t
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info ((_command (eql view-register)))
  (make-register-preview-info
   :types '(all)
   :msg "View register `%s'"
   :act 'view
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))
(cl-defmethod register-command-info ((_command (eql append-to-register)))
  (make-register-preview-info
   :types '(string number)
   :msg "Append to register `%s'"
   :act 'modify
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))
(cl-defmethod register-command-info ((_command (eql prepend-to-register)))
  (make-register-preview-info
   :types '(string number)
   :msg "Prepend to register `%s'"
   :act 'modify
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))
(cl-defmethod register-command-info ((_command (eql increment-register)))
  (make-register-preview-info
   :types '(string number)
   :msg "Increment register `%s'"
   :act 'modify
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))
(cl-defmethod register-command-info ((_command (eql copy-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Copy to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info ((_command (eql point-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Point to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info ((_command (eql number-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Number to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info
    ((_command (eql window-configuration-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Window configuration to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info ((_command (eql frameset-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Frameset to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))))
(cl-defmethod register-command-info ((_command (eql copy-rectangle-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Copy rectangle to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))

(defun register-preview-forward-line (arg)
  "Move to next or previous line in register preview buffer.
If ARG is positive, go to next line; if negative, go to previous line.
Do nothing when defining or executing kmacros."
  ;; Ensure user enter manually key in minibuffer when recording a macro.
  (unless (or defining-kbd-macro executing-kbd-macro
              (not (get-buffer-window "*Register Preview*" 'visible)))
    (let ((fn (if (> arg 0) #'eobp #'bobp))
          (posfn (if (> arg 0)
                     #'point-min
                     (lambda () (1- (point-max)))))
          str)
      (with-current-buffer "*Register Preview*"
        (let ((ovs (overlays-in (point-min) (point-max)))
              pos)
          (goto-char (if ovs
                         (overlay-start (car ovs))
                         (point-min)))
          (setq pos (point))
          (and ovs (forward-line arg))
          (when (and (funcall fn)
                     (or (> arg 0) (eql pos (point))))
            (goto-char (funcall posfn)))
          (setq str (buffer-substring-no-properties
                     (pos-bol) (1+ (pos-bol))))
          (remove-overlays)
          (with-selected-window (minibuffer-window)
            (delete-minibuffer-contents)
            (insert str)))))))

(defun register-preview-next ()
  "Go to next line in the register preview buffer."
  (interactive)
  (register-preview-forward-line 1))

(defun register-preview-previous ()
  "Go to previous line in the register preview buffer."
  (interactive)
  (register-preview-forward-line -1))

(defun register-type (register)
  "Return REGISTER type.
Register type that can be returned is one of the following:
 - string
 - number
 - marker
 - buffer
 - file
 - file-query
 - window
 - frame
 - kmacro

One can add new types to a specific command by defining a new `cl-defmethod'
matching that command.  Predicates for type in new `cl-defmethod' should
satisfy `cl-typep', otherwise the new type should be defined with
`cl-deftype'."
  ;; Call register--type against the register value.
  (register--type (if (consp (cdr register))
                     (cadr register)
                   (cdr register))))

(cl-defgeneric register--type (regval)
  "Return the type of register value REGVAL."
  (ignore regval))

(cl-defmethod register--type ((_regval string)) 'string)
(cl-defmethod register--type ((_regval number)) 'number)
(cl-defmethod register--type ((_regval marker)) 'marker)
(cl-defmethod register--type ((_regval (eql buffer))) 'buffer)
(cl-defmethod register--type ((_regval (eql file))) 'file)
(cl-defmethod register--type ((_regval (eql file-query))) 'file-query)
(cl-defmethod register--type ((_regval window-configuration)) 'window)
(cl-deftype frame-register () '(satisfies frameset-register-p))
(cl-defmethod register--type :extra "frame-register" (_regval) 'frame)
(cl-deftype kmacro-register () '(satisfies kmacro-register-p))
(cl-defmethod register--type :extra "kmacro-register" (_regval) 'kmacro)

(defun register-of-type-alist (types)
  "Filter `register-alist' according to TYPES."
  (if (memq 'all types)
      register-alist
    (cl-loop for register in register-alist
             when (memq (register-type register) types)
             collect register)))

(defun register-preview (buffer &optional show-empty)
  "Pop up a window showing the preview of registers in BUFFER.
If SHOW-EMPTY is non-nil, show the preview window even if no registers.
Format of each entry is controlled by the variable `register-preview-function'."
  (unless register-preview-function
    (setq register-preview-function (register--preview-function
                                     register--read-with-preview-function)))
  (when (or show-empty (consp register-alist))
    (with-current-buffer-window
     buffer
     (cons 'display-buffer-below-selected
	   '((window-height . fit-window-to-buffer)
	     (preserve-size . (nil . t))))
     nil
     (with-current-buffer standard-output
       (setq cursor-in-non-selected-windows nil)
       (mapc (lambda (elem)
               (when (get-register (car elem))
                 (insert (funcall register-preview-function elem))))
             register-alist)))))

(defun register-preview-1 (buffer &optional show-empty types)
  "Pop up a window showing the preview of registers in BUFFER.

This is the preview function used with the `register-read-with-preview-fancy'
function.
If SHOW-EMPTY is non-nil, show the preview window even if no registers.
Optional argument TYPES (a list) specifies the types of register to show;
if it is nil, show all the registers.  See `register-type' for suitable types.
Format of each entry is controlled by the variable `register-preview-function'."
  (unless register-preview-function
    (setq register-preview-function (register--preview-function
                                     register--read-with-preview-function)))
  (let ((registers (register-of-type-alist (or types '(all)))))
    (when (or show-empty (consp registers))
      (with-current-buffer-window
        buffer
        (cons 'display-buffer-below-selected
	      '((window-height . fit-window-to-buffer)
	        (preserve-size . (nil . t))))
        nil
        (with-current-buffer standard-output
          (setq cursor-in-non-selected-windows nil)
          (mapc (lambda (elem)
                  (when (get-register (car elem))
                    (insert (funcall register-preview-function elem))))
                registers))))))

(cl-defgeneric register-preview-get-defaults (action)
  "Return default registers according to ACTION."
  (ignore action))
(cl-defmethod register-preview-get-defaults ((_action (eql set)))
  (cl-loop for s in register-preview-default-keys
           unless (assoc (string-to-char s) register-alist)
           collect s))

(defun register-read-with-preview (prompt)
  "Read register name, prompting with PROMPT; possibly show existing registers.
This reads and returns the name of a register.  PROMPT should be a string
to prompt the user for the name.
If `help-char' (or a member of `help-event-list') is pressed,
display preview window unconditionally.
This calls the function specified by `register--read-with-preview-function'."
  (funcall register--read-with-preview-function prompt))

(defun register-read-with-preview-traditional (prompt)
  "Read register name, prompting with PROMPT; possibly show existing registers.
This reads and returns the name of a register.  PROMPT should be a string
to prompt the user for the name.
If `register-alist' and `register-preview-delay' are both non-nil, display
a window listing existing registers after `register-preview-delay' seconds.
If `help-char' (or a member of `help-event-list') is pressed,
display preview window unconditionally.

This function is used as the value of `register--read-with-preview-function'
when `register-use-preview' is set to \\='traditional."
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

(defun register-read-with-preview-fancy (prompt)
  "Read register name, prompting with PROMPT; possibly show existing registers.
This reads and returns the name of a register.  PROMPT should be a string
to prompt the user for the name.
If `help-char' (or a member of `help-event-list') is pressed,
display preview window regardless.

This function is used as the value of `register--read-with-preview-function'
when `register-use-preview' is set to any value other than \\='traditional
or \\='never."
  (let* ((buffer "*Register Preview*")
         (buffer1 "*Register quick preview*")
         (buf (if register-use-preview buffer buffer1))
         (pat "")
         (map (let ((m (make-sparse-keymap)))
                (set-keymap-parent m minibuffer-local-map)
                m))
         (data (register-command-info this-command))
         (enable-recursive-minibuffers t)
         types msg result act win strs smatch noconfirm)
    (if data
        (setq types     (register-preview-info-types data)
              msg       (register-preview-info-msg   data)
              act       (register-preview-info-act   data)
              smatch    (register-preview-info-smatch data)
              noconfirm (register-preview-info-noconfirm data))
      (setq types '(all)
            msg   "Overwrite register `%s'"
            act   'set))
    (setq strs (mapcar (lambda (x)
                         (string (car x)))
                       (register-of-type-alist types)))
    (when (and (memq act '(insert jump view)) (null strs))
      (error "No register suitable for `%s'" act))
    (dolist (k (cons help-char help-event-list))
      (define-key map (vector k)
                  (lambda ()
                    (interactive)
                    ;; Do nothing when buffer1 is in use.
                    (unless (get-buffer-window buf)
                      (with-selected-window (minibuffer-selected-window)
                        (register-preview-1 buffer 'show-empty types))))))
    (define-key map (kbd "<down>") 'register-preview-next)
    (define-key map (kbd "<up>")   'register-preview-previous)
    (define-key map (kbd "C-n")    'register-preview-next)
    (define-key map (kbd "C-p")    'register-preview-previous)
    (unless (or executing-kbd-macro (eq register-use-preview 'never))
      (register-preview-1 buf nil types))
    (unwind-protect
        (let ((setup
               (lambda ()
                 (with-selected-window (minibuffer-window)
                   (let ((input (minibuffer-contents)))
                     (when (> (length input) 1)
                       (let ((new (substring input 1))
                             (old (substring input 0 1)))
                         (setq input (if (or (null smatch)
                                             (member new strs))
                                         new old))
                         (delete-minibuffer-contents)
                         (insert input)))
                     (when (and smatch (not (string= input ""))
                                (not (member input strs)))
                       (setq input "")
                       (delete-minibuffer-contents)
                       (minibuffer-message "Not matching"))
                     (when (not (string= input pat))
                       (setq pat input))))
                 (if (setq win (get-buffer-window buffer))
                     (with-selected-window win
                       (let ((ov (make-overlay
                                  (point-min) (point-min)))
                             ;; Allow upper-case and lower-case letters
                             ;; to refer to different registers.
                             (case-fold-search nil))
                         (goto-char (point-min))
                         (remove-overlays)
                         (unless (string= pat "")
                           (if (re-search-forward (concat "^" pat) nil t)
                               (progn (move-overlay
                                       ov
                                       (match-beginning 0) (pos-eol))
                                      (overlay-put ov 'face 'match)
                                      (when msg
                                        (with-selected-window
                                            (minibuffer-window)
                                          (minibuffer-message msg pat))))
                             (with-selected-window (minibuffer-window)
                               (minibuffer-message
                                "Register `%s' is empty" pat))))))
                   (unless (string= pat "")
                     (with-selected-window (minibuffer-window)
                       (if (and (member pat strs)
                                (null noconfirm))
                           (with-selected-window (minibuffer-window)
                             (minibuffer-message msg pat))
                         ;; `:noconfirm' is specified explicitly, don't ask for
                         ;; confirmation and exit immediately (bug#66394).
                         (setq result pat)
                         (exit-minibuffer))))))))
          (minibuffer-with-setup-hook
              (lambda () (add-hook 'post-command-hook setup nil 'local))
            (setq result (read-from-minibuffer
                          prompt nil map nil nil
                          (register-preview-get-defaults act))))
          (cl-assert (and result (not (string= result "")))
                     nil "No register specified")
          (string-to-char result))
      (let ((w (get-buffer-window buf)))
        (and (window-live-p w) (delete-window w)))
      (and (get-buffer buf) (kill-buffer buf)))))

(defun point-to-register (register &optional arg)
  "Store current location of point in REGISTER.
With prefix argument ARG, store current frame configuration (a.k.a. \"frameset\").
Use \\[jump-to-register] to go to that location or restore that configuration.
Argument is a character, the name of the register.

Interactively, prompt for REGISTER using `register-read-with-preview'."
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
  "Store the window configuration of the selected frame in REGISTER.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, the name of the register.

Interactively, prompt for REGISTER using `register-read-with-preview'."
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
  "Store the window configurations of all frames in REGISTER.
\(This window configuration is also known as \"frameset\").
Use \\[jump-to-register] to restore the configuration.
Argument is a character, the name of the register.

Interactively, prompt for REGISTER using `register-read-with-preview'."
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
  "Go to location stored in REGISTER, or restore configuration stored there.
Push the mark if going to the location moves point, unless called in succession.
If the register contains a file name, find that file.
If the register contains a buffer name, switch to that buffer.
\(To put a file or buffer name in a register, you must use `set-register'.)
If the register contains a window configuration (one frame) or a frameset
\(all frames), restore the configuration of that frame or of all frames
accordingly.
First argument REGISTER is a character, the name of the register.
Optional second arg DELETE non-nil (interactively, prefix argument) says
to delete any existing frames that the frameset doesn't mention.
\(Otherwise, these frames are iconified.)  This argument is currently
ignored if the register contains anything but a frameset.

Interactively, prompt for REGISTER using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Jump to register: ")
		     current-prefix-arg))
  (let ((val (get-register register)))
    (register-val-jump-to val delete)))

(cl-defgeneric register-val-jump-to (_val _arg)
  "Execute the \"jump\" operation of VAL.
VAL is the contents of a register as returned by `get-register'.
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
   ((eq (car val) 'buffer)
    (switch-to-buffer (cdr val)))
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
  "Store NUMBER in REGISTER.
REGISTER is a character, the name of the register.
If NUMBER is nil, a decimal number is read from the buffer
at point, and point moves to the end of that number.
Interactively, NUMBER is the prefix arg (none means nil).

Interactively, prompt for REGISTER using `register-read-with-preview'."
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
  "Augment contents of REGISTER using PREFIX.
Interactively, PREFIX is the raw prefix argument.

If REGISTER contains a number, add `prefix-numeric-value' of
PREFIX to it.

If REGISTER is empty or if it contains text, call
`append-to-register' with `delete-flag' set to PREFIX.

Interactively, prompt for REGISTER using `register-read-with-preview'."
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
  "Display the description of the contents of REGISTER.
REGISTER is a character, the name of the register.

Interactively, prompt for REGISTER using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "View register: ")))
  (let ((val (get-register register)))
    (if (null val)
	(message "Register %s is empty" (single-key-description register))
      (with-output-to-temp-buffer "*Output*"
	(describe-register-1 register t)))))

(defun list-registers ()
  "Display the list of nonempty registers with brief descriptions of contents."
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
  "Print description of register value VAL to `standard-output'.
Second argument VERBOSE means produce a more detailed description."
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
    (let* ((stored-window-config (car val))
           (window-config-frame (window-configuration-frame stored-window-config))
           (current-frame (selected-frame)))
      (princ (format "a window configuration: %s."
                     (if (frame-live-p window-config-frame)
                         (with-selected-frame window-config-frame
                           (save-window-excursion
                             (set-window-configuration stored-window-config)
                             (concat
                              (mapconcat (lambda (w) (buffer-name (window-buffer w)))
                                         (window-list (selected-frame)) ", ")
                              (unless (eq current-frame window-config-frame)
                                " in another frame"))))
                       "dead frame")))))

   ((frame-configuration-p (car val))
    (princ "a frame configuration."))

   ((eq (car val) 'file)
    (princ "the file ")
    (prin1 (cdr val))
    (princ "."))

   ((eq (car val) 'buffer)
    (princ "the buffer ")
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
  "Insert contents of REGISTER at point.
REGISTER is a character, the name of the register.
Normally puts point before and mark after the inserted text, but
if optional second argument ARG is non-nil, puts mark before and
point after.  Interactively, ARG is nil if prefix arg is supplied,
and t otherwise.

Interactively, prompt for REGISTER using `register-read-with-preview'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (register-read-with-preview "Insert register: ")
		       (not current-prefix-arg))))
  (push-mark)
  (let ((val (get-register register)))
    (register-val-insert val))
  (if (not arg) (exchange-point-and-mark)))

(cl-defgeneric register-val-insert (_val)
  "Insert register value VAL in current buffer at point."
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
  "Copy region of text between START and END into REGISTER.
If DELETE-FLAG is non-nil (interactively, prefix arg), delete the region
after copying.
Called from Lisp, takes five args: REGISTER, START, END, DELETE-FLAG,
and REGION.  START and END are buffer positions indicating what to copy.
The optional argument REGION, if non-nil, means START..END denotes the
region.

Interactively, prompt for REGISTER using `register-read-with-preview'
and use mark and point as START and END; REGION is always non-nil in
this case."
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
  "Append region of text between START and END to REGISTER.
If DELETE-FLAG is non-nil (interactively, prefix arg), delete the region
after appending.
Called from Lisp, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append.

Interactively, prompt for REGISTER using `register-read-with-preview',
and use mark and point as START and END."
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
  "Prepend region of text between START and END to REGISTER.
If DELETE-FLAG is non-nil (interactively, prefix arg), delete the region
after prepending.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to prepend.

Interactively, prompt for REGISTER using `register-read-with-preview',
and use mark and point as START and END."
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
  "Copy rectangular region of text between START and END into REGISTER.
If DELETE-FLAG is non-nil (interactively, prefix arg), delete the region
after copying.
To insert this register into a buffer, use \\[insert-register].

Called from Lisp, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle.

Interactively, prompt for REGISTER using `register-read-with-preview',
and use mark and point as START and END."
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
