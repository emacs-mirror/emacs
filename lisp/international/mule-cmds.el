;;; mule-cmds.el --- Commands for mulitilingual environment

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual

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

;;; Code:

;;; MULE related key bindings and menus.

(defvar mule-keymap (make-sparse-keymap)
  "Keymap for MULE (Multilingual environment) specific commands.")

;; Keep "C-x C-m ..." for mule specific commands.
(define-key ctl-x-map "\C-m" mule-keymap)

(define-key mule-keymap "m" 'toggle-enable-multibyte-characters)
(define-key mule-keymap "f" 'set-buffer-file-coding-system)
(define-key mule-keymap "t" 'set-terminal-coding-system)
(define-key mule-keymap "k" 'set-keyboard-coding-system)
(define-key mule-keymap "p" 'set-buffer-process-coding-system)
(define-key mule-keymap "\C-\\" 'select-input-method)
(define-key mule-keymap "c" 'universal-coding-system-argument)
(define-key mule-keymap "l" 'set-language-environment)

(define-key help-map "\C-L" 'describe-language-environment)
(define-key help-map "L" 'describe-language-environment)
(define-key help-map "\C-\\" 'describe-input-method)
(define-key help-map "I" 'describe-input-method)
(define-key help-map "C" 'describe-coding-system)
(define-key help-map "h" 'view-hello-file)

(defvar mule-menu-keymap (make-sparse-keymap "Mule")
  "Keymap for MULE (Multilingual environment) menu specific commands.")

(define-key global-map [menu-bar mule] (cons "Mule" mule-menu-keymap))

(setq menu-bar-final-items (cons 'mule menu-bar-final-items))

(defvar describe-language-environment-map nil)
(define-prefix-command 'describe-language-environment-map)

(defvar setup-language-environment-map nil)
(define-prefix-command 'setup-language-environment-map)

(defvar set-coding-system-map nil)
(define-prefix-command 'set-coding-system-map)

(define-key-after mule-menu-keymap [toggle-mule]
  '("Toggle Multibyte Characters" . toggle-enable-multibyte-characters)
  t)
(define-key-after mule-menu-keymap [describe-language-environment]
  '("Describe Language Environment" . describe-language-environment-map)
  t)
(define-key-after mule-menu-keymap [set-language-environment]
  '("Set Language Environment" . setup-language-environment-map)
  t)
(define-key-after mule-menu-keymap [mouse-set-font]
  '("Set Font/Fontset" . mouse-set-font)
  t)
(define-key-after mule-menu-keymap [separator-mule]
  '("--")
  t)
(define-key-after mule-menu-keymap [toggle-input-method]
  '("Toggle Input Method" . toggle-input-method)
  t)
(define-key-after mule-menu-keymap [select-input-method]
  '("Select Input Method" . select-input-method)
  t)
(define-key-after mule-menu-keymap [describe-input-method]
  '("Describe Input Method" . describe-input-method)
  t)
(define-key-after mule-menu-keymap [separator-input-method]
  '("--")
  t)
(define-key-after mule-menu-keymap [describe-coding-system]
  '("Describe Coding Systems" . describe-coding-system)
  t)
(define-key-after mule-menu-keymap [set-various-coding-system]
  '("Set Coding System" . set-coding-system-map)
  t)
(define-key-after mule-menu-keymap [separator-coding-system]
  '("--")
  t)
(define-key-after mule-menu-keymap [mule-diag]
  '("Show All of MULE Status" . mule-diag)
  t)
(define-key-after mule-menu-keymap [view-hello-file]
  '("Show Script Examples" . view-hello-file)
  t)

(define-key-after set-coding-system-map [set-buffer-file-coding-system]
  '("Buffer File" . set-buffer-file-coding-system)
  t)
(define-key-after set-coding-system-map [universal-coding-system-argument]
  '("Next Command" . universal-coding-system-argument)
  t)
(define-key-after set-coding-system-map [set-terminal-coding-system]
  '("Terminal" . set-terminal-coding-system)
  t)
(define-key-after set-coding-system-map [set-keyboard-coding-system]
  '("Keyboard" . set-keyboard-coding-system)
  t)
(define-key-after set-coding-system-map [set-buffer-process-coding-system]
  '("Buffer Process" . set-buffer-process-coding-system)
  t)

(define-key setup-language-environment-map
  [Default] '("Default" . setup-specified-language-environment))

;; These are meaningless when running under X.
(put 'set-terminal-coding-system 'menu-enable
     '(not (eq window-system 'x)))
(put 'set-keyboard-coding-system 'menu-enable
     '(not (eq window-system 'x)))
;; This is meaningless when the current buffer has no process.
(put 'set-buffer-process-coding-system 'menu-enable
     '(get-buffer-process (current-buffer)))

;; This should be a single character key binding because users use it
;; very frequently while editing multilingual text.  Now we can use
;; only two such keys: "\C-\\" and "\C-^", but the latter is not
;; convenient because it requires shifting on most keyboards.  An
;; alternative is "\C-\]" which is now bound to `abort-recursive-edit'
;; but it won't be used that frequently.
(define-key global-map "\C-\\" 'toggle-input-method)

;;; This is no good because people often type Shift-SPC
;;; meaning to type SPC.  -- rms.
;;; ;; Here's an alternative key binding for X users (Shift-SPACE).
;;; (define-key global-map [?\S- ] 'toggle-input-method)

(defun toggle-enable-multibyte-characters (&optional arg)
  "Change whether this buffer enables multibyte characters.
With arg, make them enable iff arg is positive."
  (interactive "P")
  (setq enable-multibyte-characters
	(if (null arg) (null enable-multibyte-characters)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defun view-hello-file ()
  "Display the HELLO file which list up many languages and characters."
  (interactive)
  ;; We have to decode the file in any environment.
  (let ((default-enable-multibyte-characters t)
	(coding-system-for-read 'iso-2022-7bit))
    (find-file-read-only (expand-file-name "HELLO" data-directory))))

(defun universal-coding-system-argument ()
  "Execute an I/O command using the specified coding system."
  (interactive)
  (let* ((coding-system (read-coding-system "Coding system for following command: "))
	 (keyseq (read-key-sequence
		  (format "Command to execute with %s:" coding-system)))
	 (cmd (key-binding keyseq)))
    (let ((coding-system-for-read coding-system)
	  (coding-system-for-write coding-system))
      (message "")
      (call-interactively cmd))))

(defun set-default-coding-systems (coding-system)
  "Set default value of various coding systems to CODING-SYSTEM.
The follwing coding systems are set:
  o coding system of a newly created buffer
  o default coding system for terminal output
  o default coding system for keyboard input
  o default coding system for subprocess I/O"
  (check-coding-system coding-system)
  (setq-default buffer-file-coding-system coding-system)
  (setq default-terminal-coding-system coding-system)
  (setq default-keyboard-coding-system coding-system)
  (setq default-process-coding-system (cons coding-system coding-system)))

(defun prefer-coding-system (coding-system)
  "Add CODING-SYSTEM at the front of the priority list for automatic detection.
This also sets the following coding systems to CODING-SYSTEM:
  o coding system of a newly created buffer
  o default coding system for terminal output
  o default coding system for keyboard input
  o default coding system for subprocess I/O"
  (interactive "zPrefer coding system: ")
  (if (not (and coding-system (coding-system-p coding-system)))
      (error "Invalid coding system `%s'" coding-system))
  (let ((coding-category (coding-system-category coding-system))
	(parent (coding-system-parent coding-system)))
    (if (not coding-category)
	;; CODING-SYSTEM is no-conversion or undecided.
	(error "Can't prefer the coding system `%s'" coding-system))
    (set coding-category (or parent coding-system))
    (if (not (eq coding-category (car coding-category-list)))
	;; We must change the order.
	(setq coding-category-list
	      (cons coding-category
		    (delq coding-category coding-category-list))))
    (if (and parent (interactive-p))
	(message "Highest priority is set to %s (parent of %s)"
		 parent coding-system))
    (set-default-coding-systems (or parent coding-system))))


;;; Language support staffs.

(defvar language-info-alist nil
  "Alist of language names vs the corresponding information of various kind.
Each element looks like:
	(LANGUAGE-NAME . ((KEY . INFO) ...))
where LANGUAGE-NAME is a string,
KEY is a symbol denoting the kind of information,
INFO is any Lisp object which contains the actual information related
to KEY.")

(defun get-language-info (language-name key)
  "Return the information for LANGUAGE-NAME of the kind KEY.
KEY is a symbol denoting the kind of required information."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let ((lang-slot (assoc-ignore-case language-name language-info-alist)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun set-language-info (language-name key info)
  "Set for LANGUAGE-NAME the information INFO under KEY.
KEY is a symbol denoting the kind of information.
INFO is any Lisp object which contains the actual information.

Currently, the following KEYs are used by Emacs:

charset: list of symbols whose values are charsets specific to the language.

coding-system: list of coding systems specific to the langauge.

tutorial: a tutorial file name written in the language.

sample-text: one line short text containing characters of the language.

documentation: t or a string describing how Emacs supports the language.
      If a string is specified, it is shown before any other information
      of the language by the command `describe-language-environment'.

setup-function: a function to call for setting up environment
       convenient for a user of the language.

If KEY is documentation or setup-function, you can also specify
a cons cell as INFO, in which case, the car part should be
a normal value as INFO for KEY (as described above),
and the cdr part should be a symbol whose value is a menu keymap
in which an entry for the language is defined.  But, only the car part
is actually set as the information.

We will define more KEYs in the future.  To avoid conflict,
if you want to use your own KEY values, make them start with `user-'."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let (lang-slot key-slot)
    (setq lang-slot (assoc language-name language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list language-name)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq key-slot (assq key lang-slot))
    (if (null key-slot)			; If no slot for the key, add it.
	(progn
	  (setq key-slot (list key))
	  (setcdr lang-slot (cons key-slot (cdr lang-slot)))))
    ;; Setup menu.
    (cond ((eq key 'documentation)
	   (define-key-after
	     (if (consp info)
		 (prog1 (symbol-value (cdr info))
		   (setq info (car info)))
	       describe-language-environment-map)
	     (vector (intern language-name))
	     (cons language-name 'describe-specified-language-support)
	     t))
	  ((eq key 'setup-function)
	   (define-key-after
	     (if (consp info)
		 (prog1 (symbol-value (cdr info))
		   (setq info (car info)))
	       setup-language-environment-map)
	     (vector (intern language-name))
	     (cons language-name 'setup-specified-language-environment)
	     t)))

    (setcdr key-slot info)
    ))

(defun set-language-info-alist (language-name alist)
  "Set for LANGUAGE-NAME the information in ALIST.
ALIST is an alist of KEY and INFO.  See the documentation of
`set-langauge-info' for the meanings of KEY and INFO."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (while alist
    (set-language-info language-name (car (car alist)) (cdr (car alist)))
    (setq alist (cdr alist))))

(defun read-language-name (key prompt &optional default)
  "Read language name which has information for KEY, prompting with PROMPT.
DEFAULT is the default choice of language.
This returns a language name as a string."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(function (lambda (elm) (assq key elm)))
				t nil nil default)))
    (if (and (> (length name) 0)
	     (get-language-info name key))
	name)))

;;; Multilingual input methods.

(defconst leim-list-file-name "leim-list.el"
  "Name of LEIM list file.
This file contains a list of libraries of Emacs input methods (LEIM)
in the format of Lisp expression for registering each input method.
Emacs loads this file at startup time.")

(defvar leim-list-header (format "\
;;; %s -- list of LEIM (Library of Emacs Input Method)
;;
;; This file contains a list of LEIM (Library of Emacs Input Method)
;; in the same directory as this file.  Loading this file registeres
;; the whole input methods in Emacs.
;;
;; Each entry has the form:
;;   (register-input-method
;;    INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC
;;    TITLE DESCRIPTION
;;    ARG ...)
;; See the function `register-input-method' for the meanings of arguments.
;;
;; If this directory is included in load-path, Emacs automatically
;; loads this file at startup time.

"
				 leim-list-file-name)
  "Header to be inserted in LEIM list file.")

(defvar leim-list-entry-regexp "^(register-input-method"
  "Regexp matching head of each entry in LEIM list file.
See also the variable `leim-list-header'")

(defvar update-leim-list-functions
  '(quail-update-leim-list-file)
  "List of functions to call to update LEIM list file.
Each function is called with one arg, LEIM directory name.")

(defun update-leim-list-file (&rest dirs)
  "Update LEIM list file in directories DIRS."
  (let ((functions update-leim-list-functions))
    (while functions
      (apply (car functions) dirs)
      (setq functions (cdr functions)))))

(defvar current-input-method nil
  "The current input method for multilingual text.
If nil, that means no input method is activated now.")
(make-variable-buffer-local 'current-input-method)
(put 'current-input-method 'permanent-local t)

(defvar current-input-method-title nil
  "Title string of the current input method shown in mode line.")
(make-variable-buffer-local 'current-input-method-title)
(put 'current-input-method-title 'permanent-local t)

(defcustom default-input-method nil
  "*Default input method for multilingual text.
This is the input method activated automatically by the command
`toggle-input-method' (\\[toggle-input-method])."
  :group 'mule)

(defvar input-method-history nil
  "History list for some commands that read input methods.")
(make-variable-buffer-local 'input-method-history)
(put 'input-method-history 'permanent-local t)

(defvar inactivate-current-input-method-function nil
  "Function to call for inactivating the current input method.
Every input method should set this to an appropriate value when activated.
This function is called with no argument.

This function should never change the value of `current-input-method'.
It is set to nil by the function `inactivate-input-method'.")
(make-variable-buffer-local 'inactivate-current-input-method-function)
(put 'inactivate-current-input-method-function 'permanent-local t)

(defvar describe-current-input-method-function nil
  "Function to call for describing the current input method.
This function is called with no argument.")
(make-variable-buffer-local 'describe-current-input-method-function)
(put 'describe-current-input-method-function 'permanent-local t)

(defvar input-method-alist nil
  "Alist of input method names vs the corresponding information to use it.
Each element has the form:
	(INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC TITLE DESCRIPTION ...)
See the function `register-input-method' for the meanings of each elements.")

(defun register-input-method (input-method language-name &rest args)
  "Register INPUT-METHOD as an input method for LANGUAGE-NAME.
INPUT-METHOD and LANGUAGE-NAME are symbols or strings.
The remaining arguments are:
	ACTIVATE-FUNC, TITLE, DESCRIPTION, and ARG ...
 where,
ACTIVATE-FUNC is a function to call for activating this method.
TITLE is a string shown in mode-line while this method is active,
DESCRIPTION is a string describing about this method,
Arguments to ACTIVATE-FUNC are INPUT-METHOD and ARGs."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method)))
  (let ((info (cons language-name args))
	(slot (assoc input-method input-method-alist)))
    (if slot
	(setcdr slot info)
      (setq slot (cons input-method info))
      (setq input-method-alist (cons slot input-method-alist)))))

(defun read-input-method-name (prompt &optional default inhibit-null)
  "Read a name of input method from a minibuffer prompting with PROMPT.
If DEFAULT is non-nil, use that as the default,
  and substitute it into PROMPT at the first `%s'.
If INHIBIT-NULL is non-nil, null input signals an error.

The return value is a string."
  (if default
      (setq prompt (format prompt default)))
  (let* ((completion-ignore-case t)
	 ;; This binding is necessary because input-method-history is
	 ;; buffer local.
	 (input-method (completing-read prompt input-method-alist
					nil t nil 'input-method-history
					default)))
    (if (> (length input-method) 0)
	input-method
      (if inhibit-null
	  (error "No valid input method is specified")))))

(defun activate-input-method (input-method)
  "Turn INPUT-METHOD on.
If some input method is already on, turn it off at first."
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method)))
  (if (and current-input-method
	   (not (string= current-input-method input-method)))
    (inactivate-input-method))
  (unless current-input-method
    (let ((slot (assoc input-method input-method-alist)))
      (if (null slot)
	  (error "Can't activate input method `%s'" input-method))
      (apply (nth 2 slot) input-method (nthcdr 5 slot))
      (setq current-input-method input-method)
      (setq current-input-method-title (nth 3 slot))
      (run-hooks 'input-method-activate-hook))))

(defun inactivate-input-method ()
  "Turn off the current input method."
  (when current-input-method
    (if input-method-history
	(unless (string= current-input-method (car input-method-history))
	  (setq input-method-history
		(cons current-input-method
		      (delete current-input-method input-method-history))))
      (setq input-method-history (list current-input-method)))
    (unwind-protect
	(funcall inactivate-current-input-method-function)
      (unwind-protect
	  (run-hooks 'input-method-inactivate-hook)
	(setq current-input-method nil
	      current-input-method-title nil)))))

(defun select-input-method (input-method)
  "Select and turn on INPUT-METHOD.
This sets the default input method to what you specify,
and turn it on for the current buffer."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t))))
  (activate-input-method input-method)
  (setq default-input-method input-method))

(defun toggle-input-method (&optional arg)
  "Turn on or off a multilingual text input method for the current buffer.

With arg, read an input method from minibuffer and turn it on.

Without arg, if some input method is currently activated, turn it off,
else turn on an input method selected last time
or the default input method (see `default-input-method').

When there's no input method to turn on, turn on what read from minibuffer."
  (interactive "P")
  (let* ((default (or (car input-method-history) default-input-method)))
    (if (and current-input-method (not arg))
	(inactivate-input-method)
      (activate-input-method
       (if (or arg (not default))
	   (read-input-method-name
	    (if default "Input method (default %s): " "Input method: " )
	    default t)  
	 default))
      (or default-input-method
	  (setq default-input-method current-input-method)))))

(defun describe-input-method (input-method)
  "Describe  input method INPUT-METHOD."
  (interactive
   (list (read-input-method-name
	  "Describe input method (default, current choice): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (null input-method)
      (describe-current-input-method)
    (with-output-to-temp-buffer "*Help*"
      (let ((elt (assoc input-method input-method-alist)))
	(princ (format "Input method: %s (`%s' in mode line) for %s\n  %s\n"
		       input-method (nth 3 elt) (nth 1 elt) (nth 4 elt)))))))

(defun describe-current-input-method ()
  "Describe the input method currently in use."
  (if current-input-method
      (if (and (symbolp describe-current-input-method-function)
	       (fboundp describe-current-input-method-function))
	  (funcall describe-current-input-method-function)
	(message "No way to describe the current input method `%s'"
		 (cdr current-input-method))
	(ding))
    (error "No input method is activated now")))

(defun read-multilingual-string (prompt &optional initial-input
					input-method)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second arg INITIAL-INPUT is non-nil, insert it in the minibuffer
initially.
Optional 3rd argument INPUT-METHOD specifies the input method
to be activated instead of the one selected last time.  It is a symbol
or a string."
  (setq input-method
	(or input-method
	    default-input-method
	    (read-input-method-name "Input method: " nil t)))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current-input-method input-method))
    (read-string prompt initial-input nil nil t)))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defcustom input-method-verbose-flag t
  "*If this flag is non-nil, input methods give extra guidance.

The extra guidance is done by showing list of available keys in echo
area.

For complex input methods such as `chinese-py' and `japanese',
when you use the input method in the minibuffer, the guidance is
shown at the bottom short window (split from the existing window).
For simple input methods, guidance is not shown
when you are in the minibuffer."
  :type 'boolean
  :group 'mule)

(defcustom input-method-highlight-flag t
  "*If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence."
  :type 'boolean
  :group 'mule)

(defvar input-method-activate-hook nil
  "Normal hook run just after an input method is activated.

The variable `current-input-method' keeps the input method name
just activated.")

(defvar input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.

The variable `current-input-method' still keeps the input method name
just inacitvated.")

(defvar input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text.")

(defvar input-method-exit-on-invalid-key nil
  "This flag controls the behaviour of an input method on invalid key input.
Usually, when a user types a key which doesn't start any character
handled by the input method, the key is handled by turning off the
input method temporalily.  After the key is handled, the input method is 
back on.
But, if this flag is non-nil, the input method is never back on.")


(defun setup-specified-language-environment ()
  "Set up multi-lingual environment convenient for the specified language."
  (interactive)
  (let (language-name)
    (if (and (symbolp last-command-event)
	     (or (not (eq last-command-event 'Default))
		 (setq last-command-event 'English))
	     (setq language-name (symbol-name last-command-event)))
	(set-language-environment language-name)
      (error "Bogus calling sequence"))))

(defvar current-language-environment "English"
  "The last language environment specified with `set-language-environment'.")

(defun set-language-environment (language-name)
  "Set up multi-lingual environment for using LANGUAGE-NAME.
This sets the coding system priority and the default input method
and sometimes other things."
  (interactive (list (read-language-name 'setup-function
					 "Set language environment: ")))
  (if language-name
      (if (symbolp language-name)
	  (setq language-name (symbol-name language-name)))
    (setq language-name "English"))
  (if (null (get-language-info language-name 'setup-function))
      (error "Language environment not defined: %S" language-name))
  (funcall (get-language-info language-name 'setup-function))
  (setq current-language-environment language-name)
  (force-mode-line-update t))

;; Print all arguments with `princ', then print "\n".
(defsubst princ-list (&rest args)
  (while args (princ (car args)) (setq args (cdr args)))
  (princ "\n"))

;; Print a language specific information such as input methods,
;; charsets, and coding systems.  This function is intended to be
;; called from the menu:
;;   [menu-bar mule describe-language-environment LANGUAGE]
;; and should not run it by `M-x describe-current-input-method-function'.
(defun describe-specified-language-support ()
  "Describe how Emacs supports the specified language environment."
  (interactive)
  (let (language-name)
    (if (not (and (symbolp last-command-event)
		  (setq language-name (symbol-name last-command-event))))
	(error "Bogus calling sequence"))
    (describe-language-environment language-name)))

(defun describe-language-environment (language-name)
  "Describe how Emacs supports language environment LANGUAGE-NAME."
  (interactive
   (list (read-language-name
	  'documentation
	  "Describe language environment (default, current choise): ")))
  (if (null language-name)
      (setq language-name current-language-environment))
  (if (or (null language-name)
	  (null (get-language-info language-name 'documentation)))
      (error "No documentation for the specified language"))
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let ((doc (get-language-info language-name 'documentation)))
    (with-output-to-temp-buffer "*Help*"
      (if (stringp doc)
	  (progn
	    (princ-list doc)
	    (terpri)))
      (let ((str (get-language-info language-name 'sample-text)))
	(if (stringp str)
	    (progn
	      (princ "Sample text:\n")
	      (princ-list "  " str)
	      (terpri))))
      (princ "Input methods:\n")
      (let ((l input-method-alist))
	(while l
	  (if (string= language-name (nth 1 (car l)))
	      (princ-list "  " (car (car l))
			  (format " (`%s' in mode line)" (nth 3 (car l)))))
	  (setq l (cdr l))))
      (terpri)
      (princ "Character sets:\n")
      (let ((l (get-language-info language-name 'charset)))
	(if (null l)
	    (princ-list "  nothing specific to " language-name)
	  (while l
	    (princ-list "  " (car l) ": "
			(charset-description (car l)))
	    (setq l (cdr l)))))
      (terpri)
      (princ "Coding systems:\n")
      (let ((l (get-language-info language-name 'coding-system)))
	(if (null l)
	    (princ-list "  nothing specific to " language-name)
	  (while l
	    (princ (format "  %s (`%c' in mode line):\n\t%s\n"
			   (car l)
			   (coding-system-mnemonic (car l))
			   (coding-system-doc-string (car l))))
	    (setq l (cdr l))))))))

;;; Charset property

(defsubst get-charset-property (charset propname)
  "Return the value of CHARSET's PROPNAME property.
This is the last value stored with
 (put-charset-property CHARSET PROPNAME VALUE)."
  (plist-get (charset-plist charset) propname))

(defsubst put-charset-property (charset propname value)
  "Store CHARSETS's PROPNAME property with value VALUE.
It can be retrieved with `(get-charset-property CHARSET PROPNAME)'."
  (set-charset-plist charset
		     (plist-put (charset-plist charset) propname value)))

;;; Character code property
(put 'char-code-property-table 'char-table-extra-slots 0)

(defvar char-code-property-table
  (make-char-table 'char-code-property-table)
  "Char-table containing a property list of each character code.

See also the documentation of `get-char-code-property' and
`put-char-code-property'.")

(defun get-char-code-property (char propname)
  "Return the value of CHAR's PROPNAME property in `char-code-property-table'."
  (let ((plist (aref char-code-property-table char)))
    (if (listp plist)
	(car (cdr (memq propname plist))))))

(defun put-char-code-property (char propname value)
  "Store CHAR's PROPNAME property with VALUE in `char-code-property-table'.
It can be retrieved with `(get-char-code-property CHAR PROPNAME)'."
  (let ((plist (aref char-code-property-table char)))
    (if plist
	(let ((slot (memq propname plist)))
	  (if slot
	      (setcar (cdr slot) value)
	    (nconc plist (list propname value))))
      (aset char-code-property-table char (list propname value)))))

;;; mule-cmds.el ends here
