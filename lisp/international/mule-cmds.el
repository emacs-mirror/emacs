;;; mule-cmds.el --- commands for mulitilingual environment

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(eval-when-compile (defvar dos-codepage))

;;; MULE related key bindings and menus.

(defvar mule-keymap (make-sparse-keymap)
  "Keymap for Mule (Multilingual environment) specific commands.")

;; Keep "C-x C-m ..." for mule specific commands.
(define-key ctl-x-map "\C-m" mule-keymap)

(define-key mule-keymap "f" 'set-buffer-file-coding-system)
(define-key mule-keymap "t" 'set-terminal-coding-system)
(define-key mule-keymap "k" 'set-keyboard-coding-system)
(define-key mule-keymap "p" 'set-buffer-process-coding-system)
(define-key mule-keymap "x" 'set-selection-coding-system)
(define-key mule-keymap "X" 'set-next-selection-coding-system)
(define-key mule-keymap "\C-\\" 'set-input-method)
(define-key mule-keymap "c" 'universal-coding-system-argument)
(define-key mule-keymap "l" 'set-language-environment)

(define-key help-map "\C-L" 'describe-language-environment)
(define-key help-map "L" 'describe-language-environment)
(define-key help-map "\C-\\" 'describe-input-method)
(define-key help-map "I" 'describe-input-method)
(define-key help-map "C" 'describe-coding-system)
(define-key help-map "h" 'view-hello-file)

(defvar mule-menu-keymap
  (make-sparse-keymap "Mule (Multilingual Environment)")
  "Keymap for Mule (Multilingual environment) menu specific commands.")

(defvar describe-language-environment-map
  (make-sparse-keymap "Describe Language Environment"))

(defvar setup-language-environment-map
  (make-sparse-keymap "Set Language Environment"))

(defvar set-coding-system-map
  (make-sparse-keymap "Set Coding System"))

(define-key-after mule-menu-keymap [set-language-environment]
  (list 'menu-item  "Set Language Environment" setup-language-environment-map
	:help "Multilingual environment suitable for a specific language"))
(define-key-after mule-menu-keymap [mouse-set-font]
  '(menu-item "Set Font/Fontset" mouse-set-font
	       :visible (fboundp 'generate-fontset-menu)
	       :help "Select a font from list of known fonts/fontsets"))
(define-key-after mule-menu-keymap [separator-mule]
  '("--")
  t)
(define-key-after mule-menu-keymap [toggle-input-method]
  '(menu-item "Toggle Input Method" toggle-input-method)
  t)
(define-key-after mule-menu-keymap [set-input-method]
  '(menu-item "Select Input Method..." set-input-method)
  t)
(define-key-after mule-menu-keymap [describe-input-method]
  '(menu-item "Describe Input Method"  describe-input-method))
(define-key-after mule-menu-keymap [separator-input-method]
  '("--")
  t)
(define-key-after mule-menu-keymap [set-various-coding-system]
  (list 'menu-item "Set Coding Systems" set-coding-system-map
	:enable 'enable-multibyte-characters))
(define-key-after mule-menu-keymap [view-hello-file]
  '(menu-item "Show Multi-lingual Text" view-hello-file
	      :enable (file-readable-p
		       (expand-file-name "HELLO" data-directory))
	      :help "Display file which says HELLO in many languages")
  t)
(define-key-after mule-menu-keymap [separator-coding-system]
  '("--")
  t)
(define-key-after mule-menu-keymap [describe-language-environment]
  (list 'menu-item "Describe Language Environment"
	describe-language-environment-map
	:help "Show multilingual settings for a specific language")
  t)
(define-key-after mule-menu-keymap [describe-input-method]
  '(menu-item "Describe Input Method..." describe-input-method
	      :help "Keyboard layout for a specific input method")
  t)
(define-key-after mule-menu-keymap [describe-coding-system]
  '(menu-item "Describe Coding System..." describe-coding-system)
  t)
(define-key-after mule-menu-keymap [list-character-sets]
  '(menu-item "List Character Sets" list-character-sets
	      :help "Show table of available character sets"))
(define-key-after mule-menu-keymap [mule-diag]
  '(menu-item "Show All of Mule Status" mule-diag
	      :help "Display multilingual environment settings")
  t)

(define-key-after set-coding-system-map [set-buffer-file-coding-system]
  '(menu-item "For Saving this Buffer" set-buffer-file-coding-system
	      :help "How to encode this buffer on disk")
  t)
(define-key-after set-coding-system-map [universal-coding-system-argument]
  '(menu-item "For Next Command" universal-coding-system-argument
	      :help "Coding system to be used by next command")
  t)
(define-key-after set-coding-system-map [set-terminal-coding-system]
  '(menu-item "For Terminal" set-terminal-coding-system
	      :enable (null (memq window-system '(x w32 mac)))
	      :help "How to encode terminal output")
  t)
(define-key-after set-coding-system-map [set-keyboard-coding-system]
  '(menu-item "For Keyboard" set-keyboard-coding-system
	      :help "How to decode keyboard input")
  t)
(define-key-after set-coding-system-map [set-buffer-process-coding-system]
  '(menu-item "For I/O with Subprocess" set-buffer-process-coding-system
	      :visible (fboundp 'start-process)
	      :enable (get-buffer-process (current-buffer))
	      :help "How to en/decode I/O from/to subprocess connected to this buffer")
  t)
(define-key-after set-coding-system-map [set-selection-coding-system]
  '(menu-item "For X Selections/Clipboard" set-selection-coding-system
	      :visible (display-selections-p)
	      :help "How to en/decode data to/from selection/clipboard")
  t)
(define-key-after set-coding-system-map [set-next-selection-coding-system]
  '(menu-item "For Next X Selection" set-next-selection-coding-system
	      :visible (display-selections-p)
	      :help "How to en/decode next selection/clipboard operation")
  t)
(define-key setup-language-environment-map
  [Default] '(menu-item "Default" setup-specified-language-environment))

(define-key describe-language-environment-map
  [Default] '(menu-item "Default" describe-specified-language-support))

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

;;; Mule related hyperlinks.
(defconst help-xref-mule-regexp-template
  (purecopy (concat "\\(\\<\\("
		    "\\(coding system\\)\\|"
		    "\\(input method\\)\\|"
		    "\\(character set\\)\\|"
		    "\\(charset\\)"
		    "\\)\\s-+\\)?"
		    ;; Note starting with word-syntax character:
		    "`\\(\\sw\\(\\sw\\|\\s_\\)+\\)'")))

(defun coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system which differs from CODING-SYSTEM in eol conversion.
The returned coding system converts end-of-line by EOL-TYPE
but text as the same way as CODING-SYSTEM.
EOL-TYPE should be `unix', `dos', `mac', or nil.
If EOL-TYPE is nil, the returned coding system detects
how end-of-line is formatted automatically while decoding.

EOL-TYPE can be specified by an integer 0, 1, or 2.
They means `unix', `dos', and `mac' respectively."
  (if (symbolp eol-type)
      (setq eol-type (cond ((eq eol-type 'unix) 0)
			   ((eq eol-type 'dos) 1)
			   ((eq eol-type 'mac) 2)
			   (t eol-type))))
  (let ((orig-eol-type (coding-system-eol-type coding-system)))
    (if (vectorp orig-eol-type)
	(if (not eol-type)
	    coding-system
	  (aref orig-eol-type eol-type))
      (let ((base (coding-system-base coding-system)))
	(if (not eol-type)
	    base
	  (if (= eol-type orig-eol-type)
	      coding-system
	    (setq orig-eol-type (coding-system-eol-type base))
	    (if (vectorp orig-eol-type)
		(aref orig-eol-type eol-type))))))))

(defun coding-system-change-text-conversion (coding-system coding)
  "Return a coding system which differs from CODING-SYSTEM in text conversion.
The returned coding system converts text by CODING
but end-of-line as the same way as CODING-SYSTEM.
If CODING is nil, the returned coding system detects
how text is formatted automatically while decoding."
  (if (not coding)
      (coding-system-base coding-system)
    (let ((eol-type (coding-system-eol-type coding-system)))
      (coding-system-change-eol-conversion
       coding
       (if (numberp eol-type) (aref [unix dos mac] eol-type))))))

(defun toggle-enable-multibyte-characters (&optional arg)
  "Change whether this buffer uses multibyte characters.
With arg, use multibyte characters if the arg is positive.

Note that this command does not convert the byte contents of
the buffer; it only changes the way those bytes are interpreted.
In general, therefore, this command *changes* the sequence of
characters that the current buffer contains.

We suggest you avoid using use this command unless you know what you
are doing.  If you use it by mistake, and the buffer is now displayed
wrong, use this command again to toggle back to the right mode."
  (interactive "P")
  (let ((new-flag
	 (if (null arg) (null enable-multibyte-characters)
	   (> (prefix-numeric-value arg) 0))))
    (set-buffer-multibyte new-flag))
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
  (let* ((default (and buffer-file-coding-system
		       (not (eq (coding-system-type buffer-file-coding-system)
				t))
		       buffer-file-coding-system))
	 (coding-system (read-coding-system
			 (if default
			     (format "Coding system for following command (default, %s): " default)
			   "Coding system for following command: ")
			 default))
	 (keyseq (read-key-sequence
		  (format "Command to execute with %s:" coding-system)))
	 (cmd (key-binding keyseq))
	 prefix)

    (when (eq cmd 'universal-argument)
      (call-interactively cmd)
      
      ;; Process keys bound in `universal-argument-map'.
      (while (progn
	       (setq keyseq (read-key-sequence nil t)
		     cmd (key-binding keyseq t))
	       (not (eq cmd 'universal-argument-other-key)))
	(let ((current-prefix-arg prefix-arg)
	      ;; Have to bind `last-command-char' here so that 
	      ;; `digit-argument', for isntance, can compute the
	      ;; prefix arg.
	      (last-command-char (aref keyseq 0)))
	  (call-interactively cmd)))

      ;; This is the final call to `univeral-argument-other-key', which
      ;; set's the final `prefix-arg.
      (let ((current-prefix-arg prefix-arg))
	(call-interactively cmd))
	
      ;; Read the command to execute with the given prefix arg.
      (setq prefix prefix-arg
	    keyseq (read-key-sequence nil t)
	    cmd (key-binding keyseq)))

    (let ((coding-system-for-read coding-system)
	  (coding-system-for-write coding-system)
	  (current-prefix-arg prefix))
      (message "")
      (call-interactively cmd))))

(defun set-default-coding-systems (coding-system)
  "Set default value of various coding systems to CODING-SYSTEM.
This sets the following coding systems:
  o coding system of a newly created buffer
  o default coding system for subprocess I/O
This also sets the following values:
  o default value used as file-name-coding-system for converting file names.
  o default value for the command `set-terminal-coding-system' (not on MSDOS)
  o default value for the command `set-keyboard-coding-system'."
  (check-coding-system coding-system)
  (setq-default buffer-file-coding-system coding-system)
  (if default-enable-multibyte-characters
      (setq default-file-name-coding-system coding-system))
  ;; If coding-system is nil, honor that on MS-DOS as well, so
  ;; that they could reset the terminal coding system.
  (unless (and (eq window-system 'pc) coding-system)
    (setq default-terminal-coding-system coding-system))
  (setq default-keyboard-coding-system coding-system)
  (setq default-process-coding-system (cons coding-system coding-system)))

(defalias 'update-iso-coding-systems 'update-coding-systems-internal)
(make-obsolete 'update-iso-coding-systems 'update-coding-systems-internal "20.3")

(defun prefer-coding-system (coding-system)
  "Add CODING-SYSTEM at the front of the priority list for automatic detection.
This also sets the following coding systems:
  o coding system of a newly created buffer
  o default coding system for subprocess I/O
This also sets the following values:
  o default value used as `file-name-coding-system' for converting file names.
  o default value for the command `set-terminal-coding-system' (not on MSDOS)
  o default value for the command `set-keyboard-coding-system'

If CODING-SYSTEM specifies a certain type of EOL conversion, the coding
systems set by this function will use that type of EOL conversion.

This command does not change the default value of terminal coding system
for MS-DOS terminal, because DOS terminals only support a single coding
system, and Emacs automatically sets the default to that coding system at
startup.

A coding system that requires automatic detection of text
encoding (e.g. undecided, unix) can't be preferred.

See also `coding-category-list' and `coding-system-category'."
  (interactive "zPrefer coding system: ")
  (if (not (and coding-system (coding-system-p coding-system)))
      (error "Invalid coding system `%s'" coding-system))
  (let ((coding-category (coding-system-category coding-system))
	(base (coding-system-base coding-system))
	(eol-type (coding-system-eol-type coding-system)))
    (if (not coding-category)
	;; CODING-SYSTEM is no-conversion or undecided.
	(error "Can't prefer the coding system `%s'" coding-system))
    (set coding-category (or base coding-system))
    (update-coding-systems-internal)
    (or (eq coding-category (car coding-category-list))
	;; We must change the order.
	(set-coding-priority (list coding-category)))
    (if (and base (interactive-p))
	(message "Highest priority is set to %s (base of %s)"
		 base coding-system))
    ;; If they asked for specific EOL conversion, honor that.
    (if (memq eol-type '(0 1 2))
	(setq coding-system
	      (coding-system-change-eol-conversion base eol-type))
      (setq coding-system base))
    (set-default-coding-systems coding-system)))

(defvar sort-coding-systems-predicate nil
  "If non-nil, a predicate function to sort coding systems.

It is called with two coding systems, and should return t if the first
one is \"less\" than the second.

The function `sort-coding-systems' use it.")

(defun sort-coding-systems (codings)
  "Sort coding system list CODINGS by a priority of each coding system.

If a coding system is most preferred, it has the highest priority.
Otherwise, a coding system corresponds to some MIME charset has higher
priorities.  Among them, a coding system included in `coding-system'
key of the current language environment has higher priorities.  See
also the documentation of `language-info-alist'.

If the variable `sort-coding-systems-predicate' (which see) is
non-nil, it is used to sort CODINGS in the different way than above."
  (if sort-coding-systems-predicate
      (sort codings sort-coding-systems-predicate)
    (let* ((most-preferred (symbol-value (car coding-category-list)))
	   (lang-preferred (get-language-info current-language-environment
					      'coding-system))
	   (func (function
		  (lambda (x)
		    (let ((base (coding-system-base x)))
		      (+ (if (eq base most-preferred) 64 0)
			 (let ((mime (coding-system-get base 'mime-charset)))
			   (if mime
			       (if (string-match "^x-" (symbol-name mime))
				   16 32)
			     0))
			 (if (memq base lang-preferred) 8 0)
			 (if (string-match "-with-esc$" (symbol-name base))
			     0 4)
			 (if (eq (coding-system-type base) 2)
			     ;; For ISO based coding systems, prefer
			     ;; one that doesn't use escape sequences.
			     (let ((flags (coding-system-flags base)))
			       (if (or (consp (aref flags 0))
				       (consp (aref flags 1))
				       (consp (aref flags 2))
				       (consp (aref flags 3)))
				   (if (or (aref flags 8) (aref flags 9))
				       0
				     1)
				 2))
			   1)))))))
      (sort codings (function (lambda (x y)
				(> (funcall func x) (funcall func y))))))))

(defun find-coding-systems-region (from to)
  "Return a list of proper coding systems to encode a text between FROM and TO.
All coding systems in the list can safely encode any multibyte characters
in the text.

If the text contains no multibyte characters, return a list of a single
element `undecided'."
  (let ((codings (find-coding-systems-region-internal from to)))
    (if (eq codings t)
	;; The text contains only ASCII characters.  Any coding
	;; systems are safe.
	'(undecided)
      ;; We need copy-sequence because sorting will alter the argument.
      (sort-coding-systems (copy-sequence codings)))))

(defun find-coding-systems-string (string)
  "Return a list of proper coding systems to encode STRING.
All coding systems in the list can safely encode any multibyte characters
in STRING.

If STRING contains no multibyte characters, return a list of a single
element `undecided'."
  (find-coding-systems-region string nil))

(defun find-coding-systems-for-charsets (charsets)
  "Return a list of proper coding systems to encode characters of CHARSETS.
CHARSETS is a list of character sets."
  (cond ((or (null charsets)
	     (and (= (length charsets) 1)
		  (eq 'ascii (car charsets))))
	 '(undecided))
	((or (memq 'eight-bit-control charsets)
	     (memq 'eight-bit-graphic charsets))
	 '(raw-text emacs-mule))
	(t
	 (let ((codings t)
	       charset l ll)
	   (while (and codings charsets)
	     (setq charset (car charsets) charsets (cdr charsets))
	     (unless (eq charset 'ascii)
	       (setq l (aref char-coding-system-table (make-char charset)))
	       (if (eq codings t)
		   (setq codings l)
		 (let ((ll nil))
		   (while codings
		     (if (memq (car codings) l)
			 (setq ll (cons (car codings) ll)))
		     (setq codings (cdr codings)))
		   (setq codings ll)))))
	   (append codings
		   (char-table-extra-slot char-coding-system-table 0))))))

(defun find-multibyte-characters (from to &optional maxcount excludes)
  "Find multibyte characters in the region specified by FROM and TO.
If FROM is a string, find multibyte characters in the string.
The return value is an alist of the following format:
  ((CHARSET COUNT CHAR ...) ...)
where
  CHARSET is a character set,
  COUNT is a number of characters,
  CHARs are found characters of the character set.
Optional 3rd arg MAXCOUNT limits how many CHARs are put in the above list.
Optional 4th arg EXCLUDE is a list of character sets to be ignored.

For invalid characters, CHARs are actually strings."
  (let ((chars nil)
	charset char)
    (if (stringp from)
	(let ((idx 0))
	  (while (setq idx (string-match "[^\000-\177]" from idx))
	    (setq char (aref from idx)
		  charset (char-charset char))
	    (if (eq charset 'unknown)
		(setq char (match-string 0)))
	    (if (or (memq charset '(unknown
				    eight-bit-control eight-bit-graphic))
		    (not (or (eq excludes t) (memq charset excludes))))
		(let ((slot (assq charset chars)))
		  (if slot
		      (if (not (memq char (nthcdr 2 slot)))
			  (let ((count (nth 1 slot)))
			    (setcar (cdr slot) (1+ count))
			    (if (or (not maxcount) (< count maxcount))
				(nconc slot (list char)))))
		    (setq chars (cons (list charset 1 char) chars)))))
	    (setq idx (1+ idx))))
      (save-excursion
	(goto-char from)
	(while (re-search-forward "[^\000-\177]" to t)
	  (setq char (preceding-char)
		charset (char-charset char))
	  (if (eq charset 'unknown)
	      (setq char (match-string 0)))
	  (if (or (memq charset '(unknown eight-bit-control eight-bit-graphic))
		  (not (or (eq excludes t) (memq charset excludes))))
	      (let ((slot (assq charset chars)))
		(if slot
		    (if (not (member char (nthcdr 2 slot)))
			(let ((count (nth 1 slot)))
			  (setcar (cdr slot) (1+ count))
			  (if (or (not maxcount) (< count maxcount))
			      (nconc slot (list char)))))
		  (setq chars (cons (list charset 1 char) chars))))))))
    (nreverse chars)))

(defvar last-coding-system-specified nil
  "Most recent coding system explicitly specified by the user when asked.
This variable is set whenever Emacs asks the user which coding system
to use in order to write a file.  If you set it to nil explicitly,
then call `write-region', then afterward this variable will be non-nil
only if the user was explicitly asked and specified a coding system.")

(defvar select-safe-coding-system-accept-default-p nil
  "If non-nil, a function to control the behaviour of coding system selection.
The meaning is the same as the argument ACCEPT-DEFAULT-P of the
function `select-safe-coding-system' (which see).  This variable
overrides that argument.")

(defun select-safe-coding-system (from to &optional default-coding-system
				       accept-default-p)
  "Ask a user to select a safe coding system from candidates.
The candidates of coding systems which can safely encode a text
between FROM and TO are shown in a popup window.  Among them, the most
proper one is suggested as the default.

The list of `buffer-file-coding-system' of the current buffer and the
most preferred coding system (if it corresponds to a MIME charset) is
treated as the default coding system list.  Among them, the first one
that safely encodes the text is silently selected and returned without
any user interaction.  See also the command `prefer-coding-system'.

Optional 3rd arg DEFAULT-CODING-SYSTEM specifies a coding system or a
list of coding systems to be prepended to the default coding system
list.

Optional 4th arg ACCEPT-DEFAULT-P, if non-nil, is a function to
determine the acceptability of the silently selected coding system.
It is called with that coding system, and should return nil if it
should not be silently selected and thus user interaction is required.

The variable `select-safe-coding-system-accept-default-p', if
non-nil, overrides ACCEPT-DEFAULT-P.

Kludgy feature: if FROM is a string, the string is the target text,
and TO is ignored."
  (if (and default-coding-system
	   (not (listp default-coding-system)))
      (setq default-coding-system (list default-coding-system)))

  ;; Change elements of the list to (coding . base-coding).
  (setq default-coding-system
	(mapcar (function (lambda (x) (cons x (coding-system-base x))))
		default-coding-system))

  ;; If buffer-file-coding-system is not nil nor undecided, append it
  ;; to the defaults.
  (if buffer-file-coding-system
      (let ((base (coding-system-base buffer-file-coding-system)))
	(or (eq base 'undecided)
	    (assq buffer-file-coding-system default-coding-system)
	    (rassq base default-coding-system)
	    (setq default-coding-system
		  (append default-coding-system
			  (list (cons buffer-file-coding-system base)))))))

  ;; If the most preferred coding system has the property mime-charset,
  ;; append it to the defaults.
  (let ((tail coding-category-list)
	preferred base)
    (while (and tail
		(not (setq preferred (symbol-value (car tail)))))
      (setq tail (cdr tail)))
    (and (coding-system-p preferred)
	 (setq base (coding-system-base preferred))
	 (coding-system-get preferred 'mime-charset)
	 (not (assq preferred default-coding-system))
	 (not (rassq base default-coding-system))
	 (setq default-coding-system
	       (append default-coding-system (list (cons preferred base))))))

  (if select-safe-coding-system-accept-default-p
      (setq accept-default-p select-safe-coding-system-accept-default-p))

  (let ((codings (find-coding-systems-region from to))
	(coding-system nil)
	(l default-coding-system))
    (if (eq (car codings) 'undecided)
	;; Any coding system is ok.
	(setq coding-system t)
      ;; Try the defaults.
      (while (and l (not coding-system))
	(if (memq (cdr (car l)) codings)
	    (setq coding-system (car (car l)))
	  (setq l (cdr l))))
      (if (and coding-system accept-default-p)
	  (or (funcall accept-default-p coding-system)
	      (setq coding-system (list coding-system)))))

    ;; If all the defaults failed, ask a user.
    (when (or (not coding-system) (consp coding-system))
      ;; At first, change each coding system to the corresponding
      ;; mime-charset name if it is also a coding system.  Such a name
      ;; is more friendly to users.
      (let ((l codings)
	    mime-charset)
	(while l
	  (setq mime-charset (coding-system-get (car l) 'mime-charset))
	  (if (and mime-charset (coding-system-p mime-charset))
	      (setcar l mime-charset))
	  (setq l (cdr l))))

      ;; Then ask users to select one form CODINGS.
      (unwind-protect
	  (save-window-excursion
	    (with-output-to-temp-buffer "*Warning*"
	      (save-excursion
		(set-buffer standard-output)
		(if (not default-coding-system)
		    (insert "No default coding systems to try.")
		  (insert "These default coding systems were tried")
		  (if (stringp from)
		      (insert " to encode \""
			      (if (> (length from) 10)
				  (substring from 0 10)
				from)
			      "...\""))
		  (insert ":\n")
		  (let ((pos (point))
			(fill-prefix "  "))
		    (mapcar (function (lambda (x)
					(princ "  ") (princ (car x))))
			    default-coding-system)
		    (insert "\n")
		    (fill-region-as-paragraph pos (point)))
		  (insert
		   (if (consp coding-system)
		       (concat (format "%s safely encodes the target text,\n"
				       (car coding-system))
			       "but it is not recommended for encoding text in this context,\n"
			       "e.g., for sending an email message.\n")
		     "However, none of them safely encodes the target text.\n")))
		(insert (if (consp coding-system)
			    "\nSelect the above, or "
			  "\nSelect ")
			"one of the following safe coding systems:\n")
		(let ((pos (point))
		      (fill-prefix "  "))
		  (mapcar (function (lambda (x) (princ "  ") (princ x)))
			  codings)
		  (insert "\n")
		  (fill-region-as-paragraph pos (point)))))

	    ;; Read a coding system.
	    (if (consp coding-system)
		(setq codings (cons (car coding-system) codings)))
	    (let* ((safe-names (mapcar (lambda (x) (list (symbol-name x)))
				       codings))
		   (name (completing-read
			  (format "Select coding system (default %s): "
				  (car codings))
			  safe-names nil t nil nil
			  (car (car safe-names)))))
	      (setq last-coding-system-specified (intern name)
		    coding-system last-coding-system-specified)))
	(kill-buffer "*Warning*")))

    (if (vectorp (coding-system-eol-type coding-system))
	(let ((eol (coding-system-eol-type buffer-file-coding-system)))
	  (if (numberp eol)
	      (setq coding-system
		    (coding-system-change-eol-conversion coding-system eol)))))

    (if (eq coding-system t)
	(setq coding-system buffer-file-coding-system))
    coding-system))

(setq select-safe-coding-system-function 'select-safe-coding-system)

(defun select-message-coding-system ()
  "Return a coding system to encode the outgoing message of the current buffer.
It at first tries the first coding system found in these variables
in this order:
  (1) local value of `buffer-file-coding-system'
  (2) value of `sendmail-coding-system'
  (3) value of `default-sendmail-coding-system'
  (4) value of `default-buffer-file-coding-system'
If the found coding system can't encode the current buffer,
or none of them are bound to a coding system,
it asks the user to select a proper coding system."
  (let ((coding (or (and (local-variable-p 'buffer-file-coding-system)
			  buffer-file-coding-system)
		     sendmail-coding-system
		     default-sendmail-coding-system
		     default-buffer-file-coding-system)))
    (if (eq coding 'no-conversion)
	;; We should never use no-conversion for outgoing mails.
	(setq coding nil))
    (if (fboundp select-safe-coding-system-function)
	(funcall select-safe-coding-system-function
		 (point-min) (point-max) coding
		 (function (lambda (x) (coding-system-get x 'mime-charset))))
      coding)))

;;; Language support stuff.

(defvar language-info-alist nil
  "Alist of language environment definitions.
Each element looks like:
	(LANGUAGE-NAME . ((KEY . INFO) ...))
where LANGUAGE-NAME is a string, the name of the language environment,
KEY is a symbol denoting the kind of information, and
INFO is the data associated with KEY.
Meaningful values for KEY include

  documentation      value is documentation of what this language environment
			is meant for, and how to use it.
  charset	     value is a list of the character sets used by this
			language environment.
  sample-text	     value is one line of text,
			written using those character sets,
			appropriate for this language environment.
  setup-function     value is a function to call to switch to this
			language environment.
  exit-function      value is a function to call to leave this
		        language environment.
  coding-system      value is a list of coding systems that are good
			for saving text written in this language environment.
			This list serves as suggestions to the user;
			in effect, as a kind of documentation.
  coding-priority    value is a list of coding systems for this language
			environment, in order of decreasing priority.
			This is used to set up the coding system priority
			list when you switch to this language environment.
  nonascii-translation
		     value is a translation table to be set in the
			variable `nonascii-translation-table' in this
			language environment, or a character set from
			which `nonascii-insert-offset' is calculated.
  input-method       value is a default input method for this language
			environment.
  features           value is a list of features requested in this
			language environment.

The following keys take effect only when multibyte characters are
globally disabled, i.e. the value of `default-enable-multibyte-characters'
is nil.

  unibyte-syntax     value is a library name to load to set
			unibyte 8-bit character syntaxes for this
			language environment.

  unibyte-display    value is a coding system to encode characters
			for the terminal.  Characters in the range
			of 160 to 255 display not as octal escapes,
			but as non-ASCII characters in this language
			environment.")

(defun get-language-info (lang-env key)
  "Return information listed under KEY for language environment LANG-ENV.
KEY is a symbol denoting the kind of information.
For a list of useful values for KEY and their meanings,
see `language-info-alist'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((lang-slot (assoc-ignore-case lang-env language-info-alist)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun set-language-info (lang-env key info)
  "Modify part of the definition of language environment LANG-ENV.
Specifically, this stores the information INFO under KEY
in the definition of this language environment.
KEY is a symbol denoting the kind of information.
INFO is the value for that information.

For a list of useful values for KEY and their meanings,
see `language-info-alist'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let (lang-slot key-slot)
    (setq lang-slot (assoc lang-env language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list lang-env)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq key-slot (assq key lang-slot))
    (if (null key-slot)			; If no slot for the key, add it.
	(progn
	  (setq key-slot (list key))
	  (setcdr lang-slot (cons key-slot (cdr lang-slot)))))
    (setcdr key-slot (purecopy info))))

(defun set-language-info-alist (lang-env alist &optional parents)
  "Store ALIST as the definition of language environment LANG-ENV.
ALIST is an alist of KEY and INFO values.  See the documentation of
`language-info-alist' for the meanings of KEY and INFO.

Optional arg PARENTS is a list of parent menu names; it specifies
where to put this language environment in the 
Describe Language Environment and Set Language Environment menus.
For example, (\"European\") means to put this language environment
in the European submenu in each of those two menus."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((describe-map describe-language-environment-map)
	(setup-map setup-language-environment-map))
    (if parents
	(let ((l parents)
	      map parent-symbol parent prompt)
	  (while l
	    (if (symbolp (setq parent-symbol (car l)))
		(setq parent (symbol-name parent))
	      (setq parent parent-symbol parent-symbol (intern parent)))
	    (setq map (lookup-key describe-map (vector parent-symbol)))
	    ;; This prompt string is for define-prefix-command, so
	    ;; that the map it creates will be suitable for a menu.
	    (or map (setq prompt (format "%s Environment" parent)))
	    (if (not map)
		(progn
		  (setq map (intern (format "describe-%s-environment-map"
					    (downcase parent))))
		  (define-prefix-command map nil prompt)
		  (define-key-after describe-map (vector parent-symbol)
		    (cons parent map) t)))
	    (setq describe-map (symbol-value map))
	    (setq map (lookup-key setup-map (vector parent-symbol)))
	    (if (not map)
		(progn
		  (setq map (intern (format "setup-%s-environment-map"
					    (downcase parent))))
		  (define-prefix-command map nil prompt)
		  (define-key-after setup-map (vector parent-symbol)
		    (cons parent map) t)))
	    (setq setup-map (symbol-value map))
	    (setq l (cdr l)))))

    ;; Set up menu items for this language env.
    (let ((doc (assq 'documentation alist)))
      (when doc
	(define-key-after describe-map (vector (intern lang-env))
	  (cons lang-env 'describe-specified-language-support) t)))
    (define-key-after setup-map (vector (intern lang-env))
      (cons lang-env 'setup-specified-language-environment) t)

    (while alist
      (set-language-info lang-env (car (car alist)) (cdr (car alist)))
      (setq alist (cdr alist)))))

(defun read-language-name (key prompt &optional default)
  "Read a language environment name which has information for KEY.
If KEY is nil, read any language environment.
Prompt with PROMPT.  DEFAULT is the default choice of language environment.
This returns a language environment name as a string."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(and key
				     (function (lambda (elm) (assq key elm))))
				t nil nil default)))
    (if (and (> (length name) 0)
	     (or (not key)
		 (get-language-info name key)))
	name)))

;;; Multilingual input methods.
(defgroup leim nil 
  "LEIM: Libraries of Emacs Input Methods."
  :group 'mule)

(defconst leim-list-file-name "leim-list.el"
  "Name of LEIM list file.
This file contains a list of libraries of Emacs input methods (LEIM)
in the format of Lisp expression for registering each input method.
Emacs loads this file at startup time.")

(defvar leim-list-header (format
";;; %s -- list of LEIM (Library of Emacs Input Method)
;;
;; This file contains a list of LEIM (Library of Emacs Input Method)
;; methods in the same directory as this file.  Loading this file
;; registers all the input methods in Emacs.
;;
;; Each entry has the form:
;;   (register-input-method
;;    INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC
;;    TITLE DESCRIPTION
;;    ARG ...)
;; See the function `register-input-method' for the meanings of the arguments.
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
  "*Default input method for multilingual text (a string).
This is the input method activated automatically by the command
`toggle-input-method' (\\[toggle-input-method])."
  :group 'mule
  :type '(choice (const nil) string)
  :set-after '(current-language-environment))

(put 'input-method-function 'permanent-local t)

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
  "Alist of input method names vs how to use them.
Each element has the form:
   (INPUT-METHOD LANGUAGE-ENV ACTIVATE-FUNC TITLE DESCRIPTION ARGS...)
See the function `register-input-method' for the meanings of the elements.")

(defun register-input-method (input-method lang-env &rest args)
  "Register INPUT-METHOD as an input method for language environment LANG-ENV.
INPUT-METHOD and LANG-ENV are symbols or strings.

The remaining arguments are:
	ACTIVATE-FUNC, TITLE, DESCRIPTION, and ARGS...
ACTIVATE-FUNC is a function to call to activate this method.
TITLE is a string to show in the mode line when this method is active.
DESCRIPTION is a string describing this method and what it is good for.
The ARGS, if any, are passed as arguments to ACTIVATE-FUNC.
All told, the arguments to ACTIVATE-FUNC are INPUT-METHOD and the ARGS.

This function is mainly used in the file \"leim-list.el\" which is
created at Emacs build time, registering all Quail input methods
contained in the Emacs distribution.

In case you want to register a new Quail input method by yourself, be
careful to use the same input method title as given in the third
parameter of `quail-define-package'.  (If the values are different, the
string specified in this function takes precedence.)

The commands `describe-input-method' and `list-input-methods' need
these duplicated values to show some information about input methods
without loading the relevant Quail packages."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method)))
  (let ((info (cons lang-env args))
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
	 ;; As it is quite normal to change input method in the
	 ;; minibuffer, we must enable it even if
	 ;; enable-recursive-minibuffers is currently nil.
	 (enable-recursive-minibuffers t)
	 ;; This binding is necessary because input-method-history is
	 ;; buffer local.
	 (input-method (completing-read prompt input-method-alist
					nil t nil 'input-method-history
					default)))
    (if (and input-method (symbolp input-method))
	(setq input-method (symbol-name input-method)))
    (if (> (length input-method) 0)
	input-method
      (if inhibit-null
	  (error "No valid input method is specified")))))

(defun activate-input-method (input-method)
  "Switch to input method INPUT-METHOD for the current buffer.
If some other input method is already active, turn it off first.
If INPUT-METHOD is nil, deactivate any current input method."
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (and current-input-method
	   (not (string= current-input-method input-method)))
      (inactivate-input-method))
  (unless (or current-input-method (null input-method))
    (let ((slot (assoc input-method input-method-alist)))
      (if (null slot)
	  (error "Can't activate input method `%s'" input-method))
      (setq current-input-method-title nil)
      (let ((func (nth 2 slot)))
	(if (functionp func)
	    (apply (nth 2 slot) input-method (nthcdr 5 slot))
	  (if (and (consp func) (symbolp (car func)) (symbolp (cdr func)))
	      (progn
		(require (cdr func))
		(apply (car func) input-method (nthcdr 5 slot)))
	    (error "Can't activate input method `%s'" input-method))))
      (setq current-input-method input-method)
      (or (stringp current-input-method-title)
	  (setq current-input-method-title (nth 3 slot)))
      (unwind-protect
	  (run-hooks 'input-method-activate-hook)
	(force-mode-line-update)))))

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
	      input-method-function nil
	      current-input-method-title nil)
	(force-mode-line-update)))))

(defun set-input-method (input-method)
  "Select and activate input method INPUT-METHOD for the current buffer.
This also sets the default input method to the one you specify.
If INPUT-METHOD is nil, this function turns off the input method, and
also causes you to be prompted for a name of an input method the next
time you invoke \\[toggle-input-method].

To deactivate the input method interactively, use \\[toggle-input-method].
To deactivate it programmatically, use \\[inactivate-input-method]."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t))))
  (activate-input-method input-method)
  (setq default-input-method input-method))

(defun toggle-input-method (&optional arg)
  "Enable or disable multilingual text input method for the current buffer.
Only one input method can be enabled at any time in a given buffer.

The normal action is to enable an input method if none was
enabled, and disable the current one otherwise.  Which input method
to enable can be determined in various ways--either the one most
recently used, or the one specified by `default-input-method', or
as a last resort by reading the name of an input method in the
minibuffer.

With a prefix argument, read an input method name with the minibuffer
and enable that one.  The default is the most recent input method specified
\(not including the currently active input method, if any)."

  (interactive "P")
  (if (and current-input-method (not arg))
      (inactivate-input-method)
    (let ((default (or (car input-method-history) default-input-method)))
      (if (and arg default (equal current-input-method default)
	       (> (length input-method-history) 1))
	  (setq default (nth 1 input-method-history)))
      (activate-input-method
       (if (or arg (not default))
	   (progn
	     (read-input-method-name
	      (if default "Input method (default %s): " "Input method: " )
	      default t))
	 default))
      (or default-input-method
	  (setq default-input-method current-input-method)))))

(defun describe-input-method (input-method)
  "Describe input method INPUT-METHOD."
  (interactive
   (list (read-input-method-name
	  "Describe input method (default, current choice): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (null input-method)
      (describe-current-input-method)
    (let ((current current-input-method))
      (condition-case nil
	  (progn
	    (save-excursion
	      (activate-input-method input-method)
	      (describe-current-input-method))
	    (activate-input-method current))
	(error 
	 (activate-input-method current)
	 (with-output-to-temp-buffer "*Help*"
	   (let ((elt (assoc input-method input-method-alist)))
	     (princ (format
		     "Input method: %s (`%s' in mode line) for %s\n  %s\n"
		     input-method (nth 3 elt) (nth 1 elt) (nth 4 elt))))))))))

(defun describe-current-input-method ()
  "Describe the input method currently in use."
  (if current-input-method
      (if (and (symbolp describe-current-input-method-function)
	       (fboundp describe-current-input-method-function))
	  (funcall describe-current-input-method-function)
	(message "No way to describe the current input method `%s'"
		 current-input-method)
	(ding))
    (error "No input method is activated now")))

(defun read-multilingual-string (prompt &optional initial-input input-method)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second arg INITIAL-INPUT is non-nil, insert it in the minibuffer
initially.
Optional 3rd argument INPUT-METHOD specifies the input method
to be activated instead of the one selected last time.  It is a symbol
or a string."
  (setq input-method
	(or input-method
	    current-input-method
	    default-input-method
	    (read-input-method-name "Input method: " nil t)))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((prev-input-method current-input-method))
    (unwind-protect
	(progn
	  (activate-input-method input-method)
	  (read-string prompt initial-input nil nil t))
      (activate-input-method prev-input-method))))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defcustom input-method-verbose-flag 'default
  "*A flag to control extra guidance given by input methods.
The value should be nil, t, `complex-only', or `default'.

The extra guidance is done by showing list of available keys in echo
area.  When you use the input method in the minibuffer, the guidance
is shown at the bottom short window (split from the existing window).

If the value is t, extra guidance is always given, if the value is
nil, extra guidance is always suppressed.

If the value is `complex-only', only complex input methods such as
`chinese-py' and `japanese' give extra guidance.

If the value is `default', complex input methods always give extra
guidance, but simple input methods give it only when you are not in
the minibuffer.

See also the variable `input-method-highlight-flag'."
  :type '(choice (const t) (const nil) (const complex-only) (const default))
  :group 'mule)

(defcustom input-method-highlight-flag t
  "*If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence.
See also the variable `input-method-verbose-flag'."
  :type 'boolean
  :group 'mule)

(defvar input-method-activate-hook nil
  "Normal hook run just after an input method is activated.

The variable `current-input-method' keeps the input method name
just activated.")

(defvar input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.

The variable `current-input-method' still keeps the input method name
just inactivated.")

(defvar input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text.")

(defvar input-method-exit-on-first-char nil
  "This flag controls when an input method returns.
Usually, the input method does not return while there's a possibility
that it may find a different translation if a user types another key.
But, it this flag is non-nil, the input method returns as soon as
the current key sequence gets long enough to have some valid translation.")

(defvar input-method-use-echo-area nil
  "This flag controls how an input method shows an intermediate key sequence.
Usually, the input method inserts the intermediate key sequence,
or candidate translations corresponding to the sequence,
at point in the current buffer.
But, if this flag is non-nil, it displays them in echo area instead.")

(defvar input-method-exit-on-invalid-key nil
  "This flag controls the behaviour of an input method on invalid key input.
Usually, when a user types a key which doesn't start any character
handled by the input method, the key is handled by turning off the
input method temporarily.  After that key, the input method is re-enabled.
But, if this flag is non-nil, the input method is never back on.")


(defvar set-language-environment-hook nil
  "Normal hook run after some language environment is set.

When you set some hook function here, that effect usually should not
be inherited to another language environment.  So, you had better set
another function in `exit-language-environment-hook' (which see) to
cancel the effect.")

(defvar exit-language-environment-hook nil
  "Normal hook run after exiting from some language environment.
When this hook is run, the variable `current-language-environment'
is still bound to the language environment being exited.

This hook is mainly used for canceling the effect of
`set-language-environment-hook' (which-see).")

(put 'setup-specified-language-environment 'apropos-inhibit t)

(defun setup-specified-language-environment ()
  "Switch to a specified language environment."
  (interactive)
  (let (language-name)
    (if (and (symbolp last-command-event)
	     (or (not (eq last-command-event 'Default))
		 (setq last-command-event 'English))
	     (setq language-name (symbol-name last-command-event)))
	(set-language-environment language-name)
      (error "Bogus calling sequence"))))

(defcustom current-language-environment "English"
  "The last language environment specified with `set-language-environment'.
This variable should be set only with \\[customize], which is equivalent
to using the function `set-language-environment'."
  :link '(custom-manual "(emacs)Language Environments")
  :set (lambda (symbol value) (set-language-environment value))
  :get (lambda (x)
	 (or (car-safe (assoc-ignore-case
			(if (symbolp current-language-environment)
			    (symbol-name current-language-environment)
			  current-language-environment)
			language-info-alist))
	     "English"))
  :type (cons 'choice (mapcar (lambda (lang)
				(list 'const (car lang)))
			      language-info-alist))
  :initialize 'custom-initialize-default
  :group 'mule
  :type 'string)

(defun reset-language-environment ()
  "Reset multilingual environment of Emacs to the default status.

The default status is as follows:

  The default value of buffer-file-coding-system is nil.
  The default coding system for process I/O is nil.
  The default value for the command `set-terminal-coding-system' is nil.
  The default value for the command `set-keyboard-coding-system' is nil.

  The order of priorities of coding categories and the coding system
  bound to each category are as follows
	coding category			coding system
	--------------------------------------------------
	coding-category-iso-8-2		iso-latin-1
	coding-category-iso-8-1		iso-latin-1
	coding-category-iso-7-tight	iso-2022-jp
	coding-category-iso-7		iso-2022-7bit
	coding-category-iso-7-else	iso-2022-7bit-lock
	coding-category-iso-8-else	iso-2022-8bit-ss2
	coding-category-emacs-mule 	emacs-mule
	coding-category-raw-text	raw-text
	coding-category-sjis		japanese-shift-jis
	coding-category-big5		chinese-big5
	coding-category-ccl		nil
	coding-category-binary		no-conversion
	coding-category-utf-16-be	nil
	coding-category-utf-16-le	nil
	coding-category-utf-8		mule-utf-8"
  (interactive)
  ;; This function formerly set default-enable-multibyte-characters to t,
  ;; but that is incorrect.  It should not alter the unibyte/multibyte choice.

  (setq coding-category-iso-7-tight	'iso-2022-jp
	coding-category-iso-7		'iso-2022-7bit
	coding-category-iso-8-1		'iso-latin-1
	coding-category-iso-8-2		'iso-latin-1
	coding-category-iso-7-else	'iso-2022-7bit-lock
	coding-category-iso-8-else	'iso-2022-8bit-ss2
	coding-category-emacs-mule	'emacs-mule
	coding-category-raw-text	'raw-text
	coding-category-sjis		'japanese-shift-jis
	coding-category-big5		'chinese-big5
	coding-category-utf-16-be       nil
	coding-category-utf-16-le       nil
	coding-category-utf-8           'mule-utf-8
	coding-category-ccl		nil
	coding-category-binary		'no-conversion)

  (set-coding-priority
   '(coding-category-iso-8-1
     coding-category-iso-8-2
     coding-category-iso-7-tight
     coding-category-iso-7
     coding-category-iso-7-else
     coding-category-iso-8-else
     coding-category-emacs-mule 
     coding-category-raw-text
     coding-category-sjis
     coding-category-big5
     coding-category-ccl
     coding-category-binary
     coding-category-utf-16-be
     coding-category-utf-16-le
     coding-category-utf-8))

  (update-coding-systems-internal)

  (set-default-coding-systems nil)
  (setq default-sendmail-coding-system 'iso-latin-1)
  (setq default-process-coding-system '(undecided . iso-latin-1))

  ;; Don't alter the terminal and keyboard coding systems here.
  ;; The terminal still supports the same coding system
  ;; that it supported a minute ago.
;;;  (set-terminal-coding-system-internal nil)
;;;  (set-keyboard-coding-system-internal nil)

  (setq nonascii-translation-table nil
	nonascii-insert-offset 0))

(reset-language-environment)

(defun set-display-table-and-terminal-coding-system (language-name)
  "Set up the display table and terminal coding system for LANGUAGE-NAME."
  (let ((coding (get-language-info language-name 'unibyte-display)))
    (if coding
	(standard-display-european-internal)
      (standard-display-default (if (eq window-system 'pc) 128 160) 255)
      (aset standard-display-table 146 nil))
    (or (eq window-system 'pc)
      (set-terminal-coding-system coding))))

(defun set-language-environment (language-name)
  "Set up multi-lingual environment for using LANGUAGE-NAME.
This sets the coding system priority and the default input method
and sometimes other things.  LANGUAGE-NAME should be a string
which is the name of a language environment.  For example, \"Latin-1\"
specifies the character set for the major languages of Western Europe."
  (interactive (list (read-language-name
		      nil
		      "Set language environment (default, English): ")))
  (if language-name
      (if (symbolp language-name)
	  (setq language-name (symbol-name language-name)))
    (setq language-name "English"))
  (or (assoc-ignore-case language-name language-info-alist)
      (error "Language environment not defined: %S" language-name))
  (if current-language-environment
      (let ((func (get-language-info current-language-environment
				     'exit-function)))
	(run-hooks 'exit-language-environment-hook)
	(if (fboundp func) (funcall func))))
  (let ((default-eol-type (coding-system-eol-type
			   default-buffer-file-coding-system)))
    (reset-language-environment)

    (setq current-language-environment language-name)
    (set-language-environment-coding-systems language-name default-eol-type))
  (let ((input-method (get-language-info language-name 'input-method)))
    (when input-method
      (setq default-input-method input-method)
      (if input-method-history
	  (setq input-method-history
		(cons input-method
		      (delete input-method input-method-history))))))
  (let ((nonascii (get-language-info language-name 'nonascii-translation))
	(dos-table
	 (if (eq window-system 'pc)
	     (intern
	      (format "cp%d-nonascii-translation-table" dos-codepage)))))
    (cond
     ((char-table-p nonascii)
      (setq nonascii-translation-table nonascii))
     ((and (eq window-system 'pc) (boundp dos-table))
      ;; DOS terminals' default is to use a special non-ASCII translation
      ;; table as appropriate for the installed codepage.
      (setq nonascii-translation-table (symbol-value dos-table)))
     ((charsetp nonascii)
      (setq nonascii-insert-offset (- (make-char nonascii) 128)))))

  ;; Unibyte setups if necessary.
  (unless default-enable-multibyte-characters
    ;; Syntax and case table.
    (let ((syntax (get-language-info language-name 'unibyte-syntax)))
      (if syntax
	  (let ((set-case-syntax-set-multibyte nil))
	    (load syntax nil t))
	;; No information for syntax and case.  Reset to the defaults.
	(let ((syntax-table (standard-syntax-table))
	      (case-table (standard-case-table))
	      (ch (if (eq window-system 'pc) 128 160)))
	  (while (< ch 256)
	    (modify-syntax-entry ch " " syntax-table)
	    (aset case-table ch ch)
	    (setq ch (1+ ch)))
	  (set-char-table-extra-slot case-table 0 nil)
	  (set-char-table-extra-slot case-table 1 nil)
	  (set-char-table-extra-slot case-table 2 nil))
	(set-standard-case-table (standard-case-table))
	(let ((list (buffer-list)))
	  (while list
	    (with-current-buffer (car list)
	      (set-case-table (standard-case-table)))
	    (setq list (cdr list))))))
    (set-display-table-and-terminal-coding-system language-name))

  (let ((required-features (get-language-info language-name 'features)))
    (while required-features
      (require (car required-features))
      (setq required-features (cdr required-features))))
  (let ((func (get-language-info language-name 'setup-function)))
    (if (fboundp func)
	(funcall func)))
  (run-hooks 'set-language-environment-hook)
  (force-mode-line-update t))

(defun standard-display-european-internal ()
  ;; Actually set up direct output of non-ASCII characters.
  (standard-display-8bit (if (eq window-system 'pc) 128 160) 255)
  ;; Unibyte Emacs on MS-DOS wants to display all 8-bit characters with
  ;; the native font, and codes 160 and 146 stand for something very
  ;; different there.
  (or (and (eq window-system 'pc) (not default-enable-multibyte-characters))
      (progn
	;; Make non-line-break space display as a plain space.
	;; Most X fonts do the wrong thing for code 160.
	(aset standard-display-table 160 [32])
	;; With luck, non-Latin-1 fonts are more recent and so don't
	;; have this bug.
	(aset standard-display-table 2208 [32]) ; Latin-1 NBSP
	;; Most Windows programs send out apostrophes as \222.  Most X fonts
	;; don't contain a character at that position.  Map it to the ASCII
	;; apostrophe.  [This is actually RIGHT SINGLE QUOTATION MARK
	;; from the cp1252, aka Windows-1252 character set.  --fx]
	(aset standard-display-table 146 [39]))))

(defun set-language-environment-coding-systems (language-name
						&optional eol-type)
  "Do various coding system setups for language environment LANGUAGE-NAME.

The optional arg EOL-TYPE specifies the eol-type of the default value
of buffer-file-coding-system set by this function."
  (let* ((priority (get-language-info language-name 'coding-priority))
	 (default-coding (car priority)))
    (if priority
	(let ((categories (mapcar 'coding-system-category priority)))
	  (set-default-coding-systems
	   (if (memq eol-type '(0 1 2 unix dos mac))
	       (coding-system-change-eol-conversion default-coding eol-type)
	     default-coding))
	  (setq default-sendmail-coding-system default-coding)
	  (set-coding-priority categories)
	  (while priority
	    (set (car categories) (car priority))
	    (setq priority (cdr priority) categories (cdr categories)))
	  (update-coding-systems-internal)))))

;; Print all arguments with `princ', then print "\n".
(defsubst princ-list (&rest args)
  (while args (princ (car args)) (setq args (cdr args)))
  (princ "\n"))

(put 'describe-specified-language-support 'apropos-inhibit t)

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
		  (or (not (eq last-command-event 'Default))
		      (setq last-command-event 'English))
		  (setq language-name (symbol-name last-command-event))))
	(error "Bogus calling sequence"))
    (describe-language-environment language-name)))

(defun describe-language-environment (language-name)
  "Describe how Emacs supports language environment LANGUAGE-NAME."
  (interactive
   (list (read-language-name
	  'documentation
	  "Describe language environment (default, current choice): ")))
  (if (null language-name)
      (setq language-name current-language-environment))
  (if (or (null language-name)
	  (null (get-language-info language-name 'documentation)))
      (error "No documentation for the specified language"))
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let ((doc (get-language-info language-name 'documentation))
	pos)
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
	(set-buffer standard-output)
	(insert language-name " language environment\n\n")
	(if (stringp doc)
	    (insert doc "\n\n"))
	(condition-case nil
	    (let ((str (eval (get-language-info language-name 'sample-text))))
	      (if (stringp str)
		  (insert "Sample text:\n  " str "\n\n")))
	  (error nil))
	(let ((input-method (get-language-info language-name 'input-method))
	      (l (copy-sequence input-method-alist)))
	  (insert "Input methods")
	  (when input-method
	    (insert " (default, " input-method ")")
	    (setq input-method (assoc input-method input-method-alist))
	    (setq l (cons input-method (delete input-method l))))
	  (insert ":\n")
	  (while l
	    (when (string= language-name (nth 1 (car l)))
	      (insert "  " (car (car l)))
	      (search-backward (car (car l)))
	      (help-xref-button 0 #'describe-input-method (car (car l))
				"mouse-2, RET: describe this input method")
	      (goto-char (point-max))
	      (insert " (\""
		      (if (stringp (nth 3 (car l)))
			  (nth 3 (car l))
			(car (nth 3 (car l))))
		      "\" in mode line)\n"))
	    (setq l (cdr l)))
	  (insert "\n"))
	(insert "Character sets:\n")
	(let ((l (get-language-info language-name 'charset)))
	  (if (null l)
	      (insert "  nothing specific to " language-name "\n")
	    (while l
	      (insert "  " (symbol-name (car l)))
	      (search-backward (symbol-name (car l)))
	      (help-xref-button 0 #'describe-character-set (car l)
				"mouse-2, RET: describe this character set")
	      (goto-char (point-max))
	      (insert ": " (charset-description (car l)) "\n")
	      (setq l (cdr l)))))
	(insert "\n")
	(insert "Coding systems:\n")
	(let ((l (get-language-info language-name 'coding-system)))
	  (if (null l)
	      (insert "  nothing specific to " language-name "\n")
	    (while l
	      (insert "  " (symbol-name (car l)))
	      (search-backward (symbol-name (car l)))
	      (help-xref-button 0 #'describe-coding-system (car l)
				"mouse-2, RET: describe this coding system")
	      (goto-char (point-max))
	      (insert " (`"
		      (coding-system-mnemonic (car l))
		      "' in mode line):\n\t"
		      (coding-system-doc-string (car l))
		      "\n")
	      (let ((aliases (coding-system-get (car l)
						'alias-coding-systems)))
		(when aliases
		  (insert "\t(alias:")
		  (while aliases
		    (insert " " (symbol-name (car aliases)))
		    (setq aliases (cdr aliases)))
		  (insert ")\n")))
	      (setq l (cdr l)))))
	(help-setup-xref (list #'describe-language-environment language-name)
			 (interactive-p))))))

;;; Locales.

(defvar locale-translation-file-name nil
  "File name for the system's file of locale-name aliases, or nil if none.")

(defvar locale-language-names
  '(
    ;; UTF-8 is not yet implemented.
    ;; Put this first, so that e.g. "ko.UTF-8" does not match "ko" below.
    (".*[._]utf" . nil)

    ;; Locale names of the form LANGUAGE[_TERRITORY][.CODESET][@MODIFIER]
    ;; as specified in the Single Unix Spec, Version 2.
    ;; LANGUAGE is a language code taken from ISO 639:1988 (E/F)
    ;; with additions from ISO 639/RA Newsletter No.1/1989;
    ;; see Internet RFC 2165 (1997-06).
    ;; TERRITORY is a country code taken from ISO 3166.
    ;; CODESET and MODIFIER are implementation-dependent.
    ;;
    ; aa Afar
    ; ab Abkhazian
    ("af" . "Latin-1") ; Afrikaans
    ("am" . "Ethiopic") ; Amharic
    ; ar Arabic
    ; as Assamese
    ; ay Aymara
    ; az Azerbaijani
    ; ba Bashkir
    ("be" . "Latin-5") ; Byelorussian
    ("bg" . "Latin-5") ; Bulgarian
    ; bh Bihari
    ; bi Bislama
    ; bn Bengali, Bangla
    ("bo" . "Tibetan")
    ("br" . "Latin-1") ; Breton
    ("ca" . "Latin-1") ; Catalan
    ; co Corsican
    ("cs" . "Czech")
    ("cy" . "Latin-8") ; Welsh
    ("da" . "Latin-1") ; Danish
    ("de" . "German")
    ; dz Bhutani
    ("el" . "Greek")
    ;; Users who specify "en" explicitly typically want Latin-1, not ASCII.
    ("en" . "Latin-1") ; English
    ("eo" . "Latin-3") ; Esperanto
    ("es" . "Spanish")
    ("et" . "Latin-4") ; Estonian
    ("eu" . "Latin-1") ; Basque
    ; fa Persian
    ("fi" . "Latin-1") ; Finnish
    ; fj Fiji
    ("fo" . "Latin-1") ; Faroese
    ("fr" . "Latin-1") ; French
    ("fy" . "Latin-1") ; Frisian
    ("ga" . "Latin-1") ; Irish Gaelic (new orthography)
    ("gd" . "Latin-1") ; Scots Gaelic
    ("gl" . "Latin-1") ; Galician
    ; gn Guarani
    ; gu Gujarati
    ; ha Hausa
    ("he" . "Hebrew")
    ("hi" . "Devanagari") ; Hindi
    ("hr" . "Latin-2") ; Croatian
    ("hu" . "Latin-2") ; Hungarian
    ; hy Armenian
    ; ia Interlingua
    ("id" . "Latin-1") ; Indonesian
    ; ie Interlingue
    ; ik Inupiak
    ("is" . "Latin-1") ; Icelandic
    ("it" . "Latin-1") ; Italian
    ; iu Inuktitut
    ("ja" . "Japanese")
    ; jw Javanese
    ; ka Georgian
    ; kk Kazakh
    ("kl" . "Latin-1") ; Greenlandic
    ; km Cambodian
    ; kn Kannada
    ("ko" . "Korean")
    ; ks Kashmiri
    ; ku Kurdish
    ; ky Kirghiz
    ("la" . "Latin-1") ; Latin
    ; ln Lingala
    ("lo" . "Lao") ; Laothian
    ("lt" . "Latin-4") ; Lithuanian
    ("lv" . "Latin-4") ; Latvian, Lettish
    ; mg Malagasy
    ; mi Maori
    ("mk" . "Latin-5") ; Macedonian
    ; ml Malayalam
    ; mn Mongolian
    ; mo Moldavian
    ("mr" . "Devanagari") ; Marathi
    ; ms Malay
    ("mt" . "Latin-3") ; Maltese
    ; my Burmese
    ; na Nauru
    ("ne" . "Devanagari") ; Nepali
    ("nl" . "Dutch")
    ("no" . "Latin-1") ; Norwegian
    ; oc Occitan
    ; om (Afan) Oromo
    ; or Oriya
    ; pa Punjabi
    ("pl" . "Latin-2") ; Polish
    ; ps Pashto, Pushto
    ("pt" . "Latin-1") ; Portuguese
    ; qu Quechua
    ("rm" . "Latin-1") ; Rhaeto-Romanic
    ; rn Kirundi
    ("ro" . "Romanian")
    ("ru.*[_.]koi8" . "Cyrillic-KOI8") ; Russian
    ("ru" . "Latin-5") ; Russian
    ; rw Kinyarwanda
    ("sa" . "Devanagari") ; Sanskrit
    ; sd Sindhi
    ; sg Sangho
    ("sh" . "Latin-2") ; Serbo-Croatian
    ; si Sinhalese
    ("sk" . "Slovak")
    ("sl" . "Slovenian")
    ; sm Samoan
    ; sn Shona
    ; so Somali
    ("sq" . "Latin-1") ; Albanian
    ("sr" . "Latin-2") ; Serbian (Latin alphabet)
    ; ss Siswati
    ; st Sesotho
    ; su Sundanese
    ("sv" . "Latin-1") ; Swedish
    ("sw" . "Latin-1") ; Swahili
    ; ta Tamil
    ; te Telugu
    ; tg Tajik
    ("th" . "Thai")
    ; ti Tigrinya
    ; tk Turkmen
    ("tl" . "Latin-1") ; Tagalog
    ; tn Setswana
    ; to Tonga
    ("tr" . "Latin-5") ; Turkish
    ; ts Tsonga
    ; tt Tatar
    ; tw Twi
    ; ug Uighur
    ("uk" . "Latin-5") ; Ukrainian
    ; ur Urdu
    ; uz Uzbek
    ("vi" . "Vietnamese")
    ; vo Volapuk
    ; wo Wolof
    ; xh Xhosa
    ; yi Yiddish
    ; yo Yoruba
    ; za Zhuang
    ("zh.*[._]big5" . "Chinese-BIG5")
    ("zh.*[._]gbk" . nil) ; Solaris 2.7; has gbk-0 as well as GB 2312.1980-0
    ("zh_tw" . "Chinese-CNS")
    ("zh" . "Chinese-GB")
    ; zu Zulu

    ;; ISO standard locales
    ("c$" . "ASCII")
    ("posix$" . "ASCII")

    ;; The "IPA" Emacs language environment does not correspond
    ;; to any ISO 639 code, so let it stand for itself.
    ("ipa$" . "IPA")

    ;; Nonstandard or obsolete language codes
    ("cz" . "Czech") ; e.g. Solaris 2.6
    ("ee" . "Latin-4") ; Estonian, e.g. X11R6.4
    ("iw" . "Hebrew") ; e.g. X11R6.4
    ("sp" . "Latin-5") ; Serbian (Cyrillic alphabet), e.g. X11R6.4
    ("su" . "Latin-1") ; Finnish, e.g. Solaris 2.6
    ("jp" . "Japanese") ; e.g. MS Windows
    ("chs" . "Chinese-GB") ; MS Windows Chinese Simplified
    ("cht" . "Chinese-BIG5") ; MS Windows Chinese Traditional
    )
  "List of pairs of locale regexps and language names.
The first element whose locale regexp matches the start of a downcased locale
specifies the language name corresponding to that locale.
If the language name is nil, there is no corresponding language environment.")

(defvar locale-charset-language-names
  '((".*8859[-_]?1\\>" . "Latin-1")
    (".*8859[-_]?2\\>" . "Latin-2")
    (".*8859[-_]?3\\>" . "Latin-3")
    (".*8859[-_]?4\\>" . "Latin-4")
    (".*8859[-_]?9\\>" . "Latin-5")
    (".*8859[-_]?14\\>" . "Latin-8")
    (".*8859[-_]?15\\>" . "Latin-9")
    (".*@euro\\>" . "Latin-9")
    )
  "List of pairs of locale regexps and charset language names.
The first element whose locale regexp matches the start of a downcased locale
specifies the language name whose charsets corresponds to that locale.
This language name is used if its charsets disagree with the charsets of
the language name that would otherwise be used for this locale.")

(defvar locale-preferred-coding-systems
  '(("ja.*[._]euc" . japanese-iso-8bit)
    ("ja.*[._]jis7" . iso-2022-jp)
    ("ja.*[._]pck" . japanese-shift-jis)
    ("ja.*[._]sjis" . japanese-shift-jis)
    ("jpn" . japanese-shift-jis)   ; MS-Windows uses this.
    )
  "List of pairs of locale regexps and preferred coding systems.
The first element whose locale regexp matches the start of a downcased locale
specifies the coding system to prefer when using that locale.")

(defun locale-name-match (key alist)
  "Search for KEY in ALIST, which should be a list of regexp-value pairs.
Return the value corresponding to the first regexp that matches the
start of KEY, or nil if there is no match."
  (let (element)
    (while (and alist (not element))
      (if (string-match (concat "^\\(" (car (car alist)) "\\)") key)
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    (cdr element)))

(defun set-locale-environment (locale-name)
  "Set up multi-lingual environment for using LOCALE-NAME.
This sets the coding system priority and the default input method
and sometimes other things.  LOCALE-NAME should be a string
which is the name of a locale supported by the system;
often it is of the form xx_XX.CODE, where xx is a language,
XX is a country, and CODE specifies a character set and coding system.
For example, the locale name \"ja_JP.EUC\" might name a locale
for Japanese in Japan using the `japanese-iso-8bit' coding-system.

If LOCALE-NAME is nil, its value is taken from the environment.

The locale names supported by your system can typically be found in a
directory named `/usr/share/locale' or `/usr/lib/locale'.  LOCALE-NAME
may be translated according to the table specified by
`locale-translation-file-name'.

See also `locale-charset-language-names', `locale-language-names',
`locale-preferred-coding-systems' and `locale-coding-system'."

  ;; Do this at runtime for the sake of binaries possibly transported
  ;; to a system without X.
  (setq locale-translation-file-name
	(let ((files
	       '("/usr/lib/X11/locale/locale.alias" ; e.g. X11R6.4
		 "/usr/X11R6/lib/X11/locale/locale.alias" ; e.g. RedHat 4.2
		 "/usr/openwin/lib/locale/locale.alias" ; e.g. Solaris 2.6
		 ;;
		 ;; The following name appears after the X-related names above,
		 ;; since the X-related names are what X actually uses.
		 "/usr/share/locale/locale.alias" ; GNU/Linux sans X
		 )))
	  (while (and files (not (file-exists-p (car files))))
	    (setq files (cdr files)))
	  (car files)))

  (let ((locale locale-name))

    (unless locale
      ;; Use the first of these three environment variables
      ;; that has a nonempty value.
      (let ((vars '("LC_ALL" "LC_CTYPE" "LANG")))
	(while (and vars (not (setq locale (getenv (car vars)))))
	  (setq vars (cdr vars)))))

    (when locale

      ;; Translate "swedish" into "sv_SE.ISO8859-1", and so on,
      ;; using the translation file that many systems have.
      (when locale-translation-file-name
	(with-temp-buffer
	  (insert-file-contents locale-translation-file-name)
	  (when (re-search-forward
		 (concat "^" (regexp-quote locale) ":?[ \t]+") nil t)
	    (setq locale (buffer-substring (point) (line-end-position))))))

      ;; Leave the system locales alone if the caller did not specify
      ;; an explicit locale name, as their defaults are set from
      ;; LC_MESSAGES and LC_TIME, not LC_CTYPE, and the user might not
      ;; want to set them to the same value as LC_CTYPE.
      (when locale-name
	(setq system-messages-locale locale)
	(setq system-time-locale locale))

      (setq locale (downcase locale))

      (let ((language-name
	     (locale-name-match locale locale-language-names))
	    (charset-language-name
	     (locale-name-match locale locale-charset-language-names))
	    (coding-system
	     (locale-name-match locale locale-preferred-coding-systems)))

	(if (and charset-language-name
		 (not
		  (equal (get-language-info language-name 'charset)
			 (get-language-info charset-language-name 'charset))))
	    (setq language-name charset-language-name))

	(when language-name

	  ;; Set up for this character set.  This is now the right way
	  ;; to do it for both unibyte and multibyte modes.
	  (set-language-environment language-name)

	  ;; If default-enable-multibyte-characters is nil,
	  ;; we are using single-byte characters,
	  ;; so the display table and terminal coding system are irrelevant.
	  (when default-enable-multibyte-characters
	    (set-display-table-and-terminal-coding-system language-name))

	  (setq locale-coding-system
		(car (get-language-info language-name 'coding-priority))))

	(when coding-system
	  (prefer-coding-system coding-system)
	  (setq locale-coding-system coding-system))))))

;;; Charset property

(defun get-charset-property (charset propname)
  "Return the value of CHARSET's PROPNAME property.
This is the last value stored with
 (put-charset-property CHARSET PROPNAME VALUE)."
  (and (not (eq charset 'composition))
       (plist-get (charset-plist charset) propname)))

(defun put-charset-property (charset propname value)
  "Store CHARSETS's PROPNAME property with value VALUE.
It can be retrieved with `(get-charset-property CHARSET PROPNAME)'."
  (or (eq charset 'composition)
      (set-charset-plist charset
			 (plist-put (charset-plist charset) propname value))))

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


;; Pretty description of encoded string

;; Alist of ISO 2022 control code vs the corresponding mnemonic string.
(defvar iso-2022-control-alist
  '((?\x1b . "ESC")
    (?\x0e . "SO")
    (?\x0f . "SI")
    (?\x8e . "SS2")
    (?\x8f . "SS3")
    (?\x9b . "CSI")))

(defun encoded-string-description (str coding-system)
  "Return a pretty description of STR that is encoded by CODING-SYSTEM."
  (setq str (string-as-unibyte str))
  (mapconcat
   (if (and coding-system (eq (coding-system-type coding-system) 2))
       ;; Try to get a pretty description for ISO 2022 escape sequences.
       (function (lambda (x) (or (cdr (assq x iso-2022-control-alist))
				 (format "%02X" x))))
     (function (lambda (x) (format "0x%02X" x))))
   str " "))

(defun encode-coding-char (char coding-system)
  "Encode CHAR by CODING-SYSTEM and return the resulting string.
If CODING-SYSTEM can't safely encode CHAR, return nil."
  (let ((str1 (string-as-multibyte (char-to-string char)))
	(str2 (string-as-multibyte (make-string 2 char)))
	(safe-chars (and coding-system
			 (coding-system-get coding-system 'safe-chars)))
	(charset (char-charset char))
	enc1 enc2 i1 i2)
    (when (or (eq safe-chars t)
	      (eq charset 'ascii)
	      (and safe-chars (aref safe-chars char)))
      ;; We must find the encoded string of CHAR.  But, just encoding
      ;; CHAR will put extra control sequences (usually to designate
      ;; ASCII charaset) at the tail if type of CODING is ISO 2022.
      ;; To exclude such tailing bytes, we at first encode one-char
      ;; string and two-char string, then check how many bytes at the
      ;; tail of both encoded strings are the same.

      (setq enc1 (encode-coding-string str1 coding-system)
	    i1 (length enc1)
	    enc2 (encode-coding-string str2 coding-system)
	    i2 (length enc2))
      (while (and (> i1 0) (= (aref enc1 (1- i1)) (aref enc2 (1- i2))))
	(setq i1 (1- i1) i2 (1- i2)))

      ;; Now (substring enc1 i1) and (substring enc2 i2) are the same,
      ;; and they are the extra control sequences at the tail to
      ;; exclude.
      (substring enc2 0 i2))))


;;; mule-cmds.el ends here
