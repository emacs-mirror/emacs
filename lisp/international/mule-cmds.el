;;; mule-cmds.el --- Commands for mulitilingual environment

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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

(defvar mule-keymap nil
  "Keymap for MULE (Multilingual environment) specific commands.")
(define-prefix-command 'mule-keymap)

;; Keep "C-x C-m ..." for mule specific commands.
(define-key ctl-x-map "\C-m" 'mule-keymap)

(define-key mule-keymap "m" 'toggle-enable-multibyte-characters)
(define-key mule-keymap "f" 'set-buffer-file-coding-system)
(define-key mule-keymap "t" 'set-terminal-coding-system)
(define-key mule-keymap "k" 'set-keyboard-coding-system)
(define-key mule-keymap "p" 'set-buffer-process-coding-system)
(define-key mule-keymap "\C-\\" 'select-input-method)
(define-key mule-keymap "c" 'universal-coding-system-argument)

(define-key help-map "\C-L" 'describe-language-environment)
(define-key help-map "\C-\\" 'describe-input-method)
(define-key help-map "C" 'describe-current-coding-system)
(define-key help-map "h" 'view-hello-file)

(defvar mule-menu-keymap nil
  "Keymap for MULE (Multilingual environment) menu specific commands.")
(define-prefix-command 'mule-menu-keymap)

(define-key global-map [menu-bar mule] (cons "Mule" mule-menu-keymap))

(setq menu-bar-final-items (cons 'mule menu-bar-final-items))

(defvar describe-language-environment-map nil)
(define-prefix-command 'describe-language-environment-map)

(defvar setup-language-environment-map nil)
(define-prefix-command 'setup-language-environment-map)

(defvar set-coding-system-map nil)
(define-prefix-command 'set-coding-system-map)

(define-key-after mule-menu-keymap [toggle-mule]
  '("Toggle MULE facility" . toggle-enable-multibyte-characters)
  t)
(define-key-after mule-menu-keymap [describe-language-environment]
  '("Describe language environment" . describe-language-environment-map)
  t)
(define-key-after mule-menu-keymap [set-language-environment]
  '("Set language environment" . setup-language-environment-map)
  t)
(define-key-after mule-menu-keymap [separator-mule]
  '("--")
  t)
(define-key-after mule-menu-keymap [toggle-input-method]
  '("Toggle input method" . toggle-input-method)
  t)
(define-key-after mule-menu-keymap [select-input-method]
  '("Select input method" . select-input-method)
  t)
(define-key-after mule-menu-keymap [describe-input-method]
  '("Describe input method" . describe-input-method)
  t)
(define-key-after mule-menu-keymap [separator-input-method]
  '("--")
  t)
(define-key-after mule-menu-keymap [describe-current-coding-system]
  '("Describe coding systems" . describe-current-coding-system)
  t)
(define-key-after mule-menu-keymap [set-various-coding-system]
  '("Set coding systems" . set-coding-system-map)
  t)
(define-key-after mule-menu-keymap [separator-coding-system]
  '("--")
  t)
(define-key-after mule-menu-keymap [mule-diag]
  '("Show diagnosis for MULE" . mule-diag)
  t)
(define-key-after mule-menu-keymap [view-hello-file]
  '("Show many languages" . view-hello-file)
  t)

(define-key-after set-coding-system-map [set-buffer-file-coding-system]
  '("Buffer file" . set-buffer-file-coding-system)
  t)
(define-key-after set-coding-system-map [set-terminal-coding-system]
  '("Terminal" . set-terminal-coding-system)
  t)
(define-key-after set-coding-system-map [set-keyboard-coding-system]
  '("Keyboard" . set-keyboard-coding-system)
  t)
(define-key-after set-coding-system-map [set-buffer-process-coding-system]
  '("Buffer process" . set-buffer-process-coding-system)
  t)

(define-key setup-language-environment-map
  [Default] '("Default" . setup-specified-language-environment))

;; These are meaningless when running under X.
(put 'set-terminal-coding-system 'menu-enable
     '(null window-system))
(put 'set-keyboard-coding-system 'menu-enable
     '(null window-system))

;; This should be a single character key binding because users use it
;; very frequently while editing multilingual text.  Now we can use
;; only two such keys: "\C-\\" and "\C-^", but the latter is not
;; convenient because it requires shifting on most keyboards.  An
;; alternative is "\C-\]" which is now bound to `abort-recursive-edit'
;; but it won't be used that frequently.
(define-key global-map "\C-\\" 'toggle-input-method)

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
	(coding-system-for-read 'iso-2022-7))
    (find-file-read-only (expand-file-name "HELLO" data-directory))))

(defun universal-coding-system-argument ()
  "Execute an I/O command using the specified coding system."
  (interactive)
  (let* ((coding-system (read-coding-system "Coding system: "))
	 (keyseq (read-key-sequence
		  (format "With coding system %s:" coding-system)))
	 (cmd (key-binding keyseq)))
    (let ((coding-system-for-read coding-system)
	  (coding-system-for-write coding-system))
      (message "")
      (call-interactively cmd))))


;;; Language support staffs.

(defvar primary-language "English"
  "Name of a user's primary language.
Emacs provide various language supports based on this variable.")

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
LANGUAGE-NAME is a string.
KEY is a symbol denoting the kind of required information."
  (let ((lang-slot (assoc-ignore-case language-name language-info-alist)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun set-language-info (language-name key info)
  "Set for LANGUAGE-NAME the information INFO under KEY.
LANGUAGE-NAME is a string
KEY is a symbol denoting the kind of information.
INFO is any Lisp object which contains the actual information.

Currently, the following KEYs are used by Emacs:

charset: list of symbols whose values are charsets specific to the language.

coding-system: list of coding systems specific to the langauge.

tutorial: a tutorial file name written in the language.

sample-text: one line short text containing characters of the language.

input-method: alist of input method names for the language vs information
      for activating them.  Use `register-input-method' (which see)
      to add a new input method to the alist.

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
  (while alist
    (set-language-info language-name (car (car alist)) (cdr (car alist)))
    (setq alist (cdr alist))))

(defun read-language-name (key prompt &optional initial-input)
  "Read language name which has information for KEY, prompting with PROMPT."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(function (lambda (elm) (assq key elm)))
				t
				initial-input)))
    (if (and (> (length name) 0)
	     (get-language-info name key))
	name)))

;;; Multilingual input methods.

(defvar current-input-method nil
  "The current input method for multilingual text.
The value is a cons of language name and input method name.
If nil, it means no input method is activated now.")
(make-variable-buffer-local 'current-input-method)
(put 'current-input-method 'permanent-local t)

(defvar current-input-method-title nil
  "Title string of the current input method shown in mode line.
Every input method should set this to an appropriate value when activated.")
(make-variable-buffer-local 'current-input-method-title)
(put 'current-input-method-title 'permanent-local t)

(defvar default-input-method nil
  "Default input method.
The default input method is the one activated automatically by the command
`toggle-input-method' (\\[toggle-input-method]).
The value is a cons of language name and input method name.")
(make-variable-buffer-local 'default-input-method)
(put 'default-input-method 'permanent-local t)

(defvar default-input-method-title nil
  "Title string of the default input method.")
(make-variable-buffer-local 'default-input-method-title)
(put 'default-input-method-title 'permanent-local t)

(defvar previous-input-method nil
  "Input method selected previously.
This is the one selected before the current input method is selected.
See also the documentation of `default-input-method'.")
(make-variable-buffer-local 'previous-input-method)
(put 'previous-input-method 'permanent-local t)

(defvar inactivate-current-input-method-function nil
  "Function to call for inactivating the current input method.
Every input method should set this to an appropriate value when activated.
This function is called with no argument.")
(make-variable-buffer-local 'inactivate-current-input-method-function)
(put 'inactivate-current-input-method-function 'permanent-local t)

(defvar describe-current-input-method-function nil
  "Function to call for describing the current input method.
This function is called with no argument.")
(make-variable-buffer-local 'describe-current-input-method-function)
(put 'describe-current-input-method-function 'permanent-local t)

(defun register-input-method (language-name input-method)
  "Register INPUT-METHOD as an input method of LANGUAGE-NAME.
LANGUAGE-NAME is a string.
INPUT-METHOD is a list of the form:
	(METHOD-NAME ACTIVATE-FUNC ARG ...)
where METHOD-NAME is the name of this method,
ACTIVATE-FUNC is the function to call for activating this method.
Arguments for the function are METHOD-NAME and ARGs."
  (let ((slot (get-language-info language-name 'input-method))
	method-slot)
    (if (null slot)
	(set-language-info language-name 'input-method (list input-method))
      (setq method-slot (assoc (car input-method) slot))
      (if method-slot
	  (setcdr method-slot (cdr input-method))
	(set-language-info language-name 'input-method
			   (cons input-method slot))))))

(defun read-language-and-input-method-name ()
  "Read a language name and the corresponding input method from a minibuffer.
Return a list of those names."
  (let* ((default-val (or previous-input-method default-input-method))
	 (language-name (read-language-name
			 'input-method "Language: "
			 (if default-val (cons (car default-val) 0)))))
    (if (null language-name)
	(error "No input method for the specified language"))
    (if (not (string= language-name (car default-val)))
	;; Now the default value has no meaning.
	(setq default-val nil))
    (let* ((completion-ignore-case t)
	   (key-slot (cdr (assq 'input-method
				(assoc language-name language-info-alist))))
	   (method-name
	    (completing-read "Input method: " key-slot nil t
			     (if default-val (cons (cdr default-val) 0)))))
      (if (= (length method-name) 0)
	  (error "No input method specified"))
      (list language-name
	    (car (assoc-ignore-case method-name key-slot))))))

;; Actvate input method METHOD-NAME for langauge LANGUAGE-NAME.
(defun activate-input-method (language-name method-name)
  (if (and current-input-method
	   (or (not (string= (car current-input-method) language-name))
	       (not (string= (cdr current-input-method) method-name))))
      (inactivate-input-method))
  (or current-input-method
      (let* ((key-slot (get-language-info language-name 'input-method))
	     (method-slot (cdr (assoc method-name key-slot))))
	(if (null method-slot)
	    (error "Invalid input method `%s' for  %s"
		   method-name language-name))
	(apply (car method-slot) method-name (cdr method-slot))
	(setq current-input-method (cons language-name method-name))
	(if (not (equal default-input-method current-input-method))
	    (progn
	      (setq previous-input-method default-input-method)
	      (setq default-input-method current-input-method)
	      (setq default-input-method-title current-input-method-title))))))

;; Inactivate the current input method.
(defun inactivate-input-method ()
  (if current-input-method
      (unwind-protect
	  (funcall inactivate-current-input-method-function)
	(setq current-input-method nil))))

(defun select-input-method (language-name method-name)
  "Select and activate input method METHOD-NAME for inputting LANGUAGE-NAME.
Both the default and local values of default-input-method are
set to the selected input method.

The information for activating METHOD-NAME is stored
in `language-info-alist' under the key 'input-method.
The format of the information has the form:
	((METHOD-NAME ACTIVATE-FUNC ARG ...) ...)
where ACTIVATE-FUNC is a function to call for activating this method.
Arguments for the function are METHOD-NAME and ARGs."
  (interactive (read-language-and-input-method-name))
  (activate-input-method language-name method-name)
  (setq-default default-input-method default-input-method)
  (setq-default default-input-method-title default-input-method-title))

(defun toggle-input-method (&optional arg)
  "Turn on or off a multilingual text input method for the current buffer.
With arg, turn on an input method specified interactively.
Without arg, if some input method is currently activated, turn it off,
else turn on default-input-method (which see).
In the latter case, if default-input-method is nil, select an input method
interactively."
  (interactive "P")
  (if arg
      (let ((input-method (read-language-and-input-method-name)))
	(activate-input-method (car input-method) (nth 1 input-method)))
    (if current-input-method
	(inactivate-input-method)
      (if default-input-method
	  (activate-input-method (car default-input-method)
				 (cdr default-input-method))
	(let ((input-method (read-language-and-input-method-name)))
	  (activate-input-method (car input-method) (nth 1 input-method)))))))

(defun describe-input-method ()
  "Describe the current input method."
  (interactive)
  (if current-input-method
      (if (and (symbolp describe-current-input-method-function)
	       (fboundp describe-current-input-method-function))
	  (funcall describe-current-input-method-function)
	(message "No way to describe the current input method `%s'"
		 (cdr current-input-method))
	(ding))
    (message "No input method is activated now")
    (ding)))

(defun read-multilingual-string (prompt &optional initial-input
					language-name method-name)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second arg INITIAL-INPUT is non-nil, insert it in the minibuffer
 initially
Optional 3rd and 4th arguments LANGUAGE-NAME and METHOD-NAME specify
 the input method to be activated instead of the one selected last time."
  (let ((default-input-method default-input-method))
    (if (and language-name method-name)
	(setq default-input-method (cons language-name method-name))
      (or default-input-method
	  (let ((lang-and-input-method (read-language-and-input-method-name)))
	    (setq default-input-method (cons (car lang-and-input-method)
					     (nth 1 lang-and-input-method))))))
    (let ((minibuffer-setup-hook '(toggle-input-method)))
      (read-string prompt initial-input))))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defvar input-method-tersely-flag nil
  "*If this flag is non-nil, input method works rather tersely.

For instance, Quail input method does not show guidance buffer while
inputting at minibuffer if this flag is t.")

(defvar input-method-activate-hook nil
  "Normal hook run just after an input method is activated.")

(defvar input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.")

(defvar input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text.")


(defun setup-specified-language-environment ()
  "Setup multi-lingual environment convenient for the specified language."
  (interactive)
  (let (language-name func)
    (if (and (symbolp last-command-event)
	     (or (not (eq last-command-event 'Default))
		 (setq last-command-event 'English))
	     (setq language-name (symbol-name last-command-event))
	     (setq func (get-language-info language-name 'setup-function)))
	(progn
	  (funcall func)
	  (force-mode-line-update t))
      (error "Bogus calling sequence"))))

;;;###autoload
(defun set-language-environment (language-name)
  "Set up multi-lingual environment for using LANGUAGE-NAME.
This sets the coding system priority and the default input method
and sometimes other things."
  (interactive (list (read-language-name 'setup-function "Language: ")))
  (if (or (null language-name)
	  (null (get-language-info language-name 'setup-function)))
      (error "No way to setup environment for the specified language"))
  (let ((last-command-event (intern language-name)))
    (setup-specified-language-environment)))

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
  "Describe how Emacs supports the specified langugage."
  (interactive)
  (let (language-name)
    (if (not (and (symbolp last-command-event)
		  (setq language-name (symbol-name last-command-event))))
	(error "Bogus calling sequence"))
    (describe-language-environment language-name)))

(defun describe-language-environment (language-name)
  "Describe how Emacs supports language environment LANGUAGE-NAME."
  (interactive (list (read-language-name 'documentation "Language: ")))
  (if (or (null language-name)
	  (null (get-language-info language-name 'documentation)))
      (error "No documentation for the specified language"))
  (let ((doc (get-language-info language-name 'documentation)))
    (with-output-to-temp-buffer "*Help*"
      (if (stringp doc)
	  (princ-list doc))
      (terpri)
      (let ((str (get-language-info language-name 'sample-text)))
	(if (stringp str)
	    (progn
	      (princ "Sample text:\n")
	      (princ-list "  " str))))
      (terpri)
      (princ "Input methods:\n")
      (let ((l (get-language-info language-name 'input-method)))
	(while l
	  (princ-list "  " (car (car l)))
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
`(put-charset-property CHARSET PROPNAME VALUE)'."
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
`put-char-code-property'")

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
