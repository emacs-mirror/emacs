;;; viper-init.el --- some common definitions for Viper

;; Copyright (C) 1997 Free Software Foundation, Inc.

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

;; Code

(provide 'viper-init)

;; compiler pacifier
(defvar mark-even-if-inactive)
(defvar viper-version)
(defvar viper-expert-level)
(defvar vip-expert-level)
;; end pacifier


;; Viper version
(defun viper-version ()
  (interactive)
  (message "Viper version is %s" viper-version)) 
(defalias 'vip-version 'viper-version)

;; Is it XEmacs?
(defconst vip-xemacs-p (string-match "\\(Lucid\\|XEmacs\\)" emacs-version))
;; Is it Emacs?
(defconst vip-emacs-p (not vip-xemacs-p))
;; Tell whether we are running as a window application or on a TTY
(defsubst vip-device-type ()
  (if vip-emacs-p
      window-system
    (device-type (selected-device))))
;; in XEmacs: device-type is tty on tty and stream in batch.
(defun vip-window-display-p ()
  (and (vip-device-type) (not (memq (vip-device-type) '(tty stream pc)))))

(defcustom vip-ms-style-os-p (memq system-type '(ms-dos windows-nt windows-95))
  "Tells if Emacs is running under an MS-style OS: ms-dos, windows-nt, W95."
  :type 'boolean
  :tag "Is it Microsoft-made OS?"
  :group 'viper)
(defcustom vip-vms-os-p (memq system-type '(vax-vms axp-vms))
  "Tells if Emacs is running under VMS."
  :type 'boolean
  :tag "Is it VMS?"
  :group 'viper)

(defcustom vip-force-faces nil
  "If t, Viper will think that it is running on a display that supports faces.
This is provided as a temporary relief for users of graphics-capable terminals
that Viper doesn't know about.
In all likelihood, you don't need to bother with this setting."
  :type 'boolean
  :group 'viper)

(defun vip-has-face-support-p ()
  (cond ((vip-window-display-p))
	(vip-force-faces)
	(vip-emacs-p (memq (vip-device-type) '(pc)))
	(vip-xemacs-p (memq (vip-device-type) '(tty pc)))))

(defun vip-convert-standard-file-name (fname)
  (if vip-emacs-p
      (convert-standard-filename fname)
    ;; hopefully, XEmacs adds this functionality
    fname))


;;; Macros

(defmacro vip-deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
	       (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
     )))

(defmacro vip-loop (count body)
  "(vip-loop COUNT BODY) Execute BODY COUNT times."
  (list 'let (list (list 'count count))
	(list 'while '(> count 0)
	      body
	      '(setq count (1- count))
	      )))

(defmacro vip-buffer-live-p (buf)
  (` (and (, buf) (get-buffer (, buf)) (buffer-name (get-buffer (, buf))))))
  
;; return buffer-specific macro definition, given a full macro definition
(defmacro vip-kbd-buf-alist (macro-elt)
  (` (nth 1 (, macro-elt))))
;; get a pair: (curr-buffer . macro-definition)
(defmacro vip-kbd-buf-pair (macro-elt)
  (` (assoc (buffer-name) (vip-kbd-buf-alist (, macro-elt)))))
;; get macro definition for current buffer
(defmacro vip-kbd-buf-definition (macro-elt)
  (` (cdr (vip-kbd-buf-pair (, macro-elt)))))
  
;; return mode-specific macro definitions, given a full macro definition
(defmacro vip-kbd-mode-alist (macro-elt)
  (` (nth 2 (, macro-elt))))
;; get a pair: (major-mode . macro-definition)
(defmacro vip-kbd-mode-pair (macro-elt)
  (` (assoc major-mode (vip-kbd-mode-alist (, macro-elt)))))
;; get macro definition for the current major mode
(defmacro vip-kbd-mode-definition (macro-elt)
  (` (cdr (vip-kbd-mode-pair (, macro-elt)))))
  
;; return global macro definition, given a full macro definition
(defmacro vip-kbd-global-pair (macro-elt)
  (` (nth 3 (, macro-elt))))
;; get global macro definition from an elt of macro-alist
(defmacro vip-kbd-global-definition (macro-elt)
  (` (cdr (vip-kbd-global-pair (, macro-elt)))))
  
;; last elt of a sequence
(defsubst vip-seq-last-elt (seq)
  (elt seq (1- (length seq))))
  

(defvar vip-minibuffer-overlay-priority 300)
(defvar vip-replace-overlay-priority 400)
(defvar vip-search-overlay-priority 500)
  

;;; Viper minor modes

;; Mode for vital things like \e, C-z.
(vip-deflocalvar vip-vi-intercept-minor-mode nil)

(vip-deflocalvar vip-vi-basic-minor-mode nil
  "Viper's minor mode for Vi bindings.")
  
(vip-deflocalvar vip-vi-local-user-minor-mode nil
  "Auxiliary minor mode for user-defined local bindings in Vi state.")

(vip-deflocalvar vip-vi-global-user-minor-mode nil
  "Auxiliary minor mode for user-defined global bindings in Vi state.")

(vip-deflocalvar vip-vi-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Vi state.")

(vip-deflocalvar vip-vi-diehard-minor-mode nil
  "This minor mode is in effect when the user wants Viper to be Vi.")

(vip-deflocalvar vip-vi-kbd-minor-mode nil
  "Minor mode for Ex command macros in Vi state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map.")

;; Mode for vital things like \e, C-z.
(vip-deflocalvar vip-insert-intercept-minor-mode nil)

(vip-deflocalvar vip-insert-basic-minor-mode nil
  "Viper's minor mode for bindings in Insert mode.")

(vip-deflocalvar vip-insert-local-user-minor-mode nil
  "Auxiliary minor mode for buffer-local user-defined bindings in Insert state.
This is a way to overshadow normal Insert mode bindings locally to certain
designated buffers.")

(vip-deflocalvar vip-insert-global-user-minor-mode nil
  "Auxiliary minor mode for global user-defined bindings in Insert state.")

(vip-deflocalvar vip-insert-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Insert state.")

(vip-deflocalvar vip-insert-diehard-minor-mode nil
  "Minor mode that simulates Vi very closely.
Not recommened, except for the novice user.")

(vip-deflocalvar vip-insert-kbd-minor-mode nil
"Minor mode for Ex command macros Insert state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map!.")

(vip-deflocalvar vip-replace-minor-mode nil
  "Minor mode in effect in replace state (cw, C, and the like commands).")

;; Mode for vital things like \C-z and \C-x)
;; This is t, by default. So, any new buffer will have C-z defined as
;; switch to Vi, unless we switched states in this buffer
(vip-deflocalvar vip-emacs-intercept-minor-mode t)
  
(vip-deflocalvar vip-emacs-local-user-minor-mode t
  "Minor mode for local user bindings effective in Emacs state.
Users can use it to override Emacs bindings when Viper is in its Emacs
state.")  
  
(vip-deflocalvar vip-emacs-global-user-minor-mode t
  "Minor mode for global user bindings in effect in Emacs state.
Users can use it to override Emacs bindings when Viper is in its Emacs
state.")  

(vip-deflocalvar vip-emacs-kbd-minor-mode t
  "Minor mode for Vi style macros in Emacs state.
The corresponding keymap stores key bindings of Vi macros defined with
`vip-record-kbd-macro' command. There is no Ex-level command to do this
interactively.")

(vip-deflocalvar vip-emacs-state-modifier-minor-mode t
  "Minor mode used to make major-mode-specific modification to Emacs state.
For instance, a Vi purist may want to bind `dd' in Dired mode to a function
that deletes a file.")

(vip-deflocalvar vip-vi-minibuffer-minor-mode nil
   "Minor mode that forces Vi-style when the Minibuffer is in Vi state.")

(vip-deflocalvar vip-insert-minibuffer-minor-mode nil
   "Minor mode that forces Vi-style when the Minibuffer is in Insert state.")
  


;; Some common error messages

(defconst vip-SpuriousText "Spurious text after command"  "")
(defconst vip-BadExCommand "Not an editor command"   "")
(defconst vip-InvalidCommandArgument "Invalid command argument"   "")
(defconst vip-NoPrevSearch "No previous search string"   "")
(defconst vip-EmptyRegister "`%c': Nothing in this register"   "")
(defconst vip-InvalidRegister "`%c': Invalid register"   "")
(defconst vip-EmptyTextmarker "`%c': Text marker doesn't point anywhere"   "")
(defconst vip-InvalidTextmarker "`%c': Invalid text marker"   "")
(defconst vip-InvalidViCommand "Invalid command"   "")
(defconst vip-BadAddress "Ill-formed address"   "")
(defconst vip-FirstAddrExceedsSecond "First address exceeds second"   "")
(defconst vip-NoFileSpecified "No file specified"   "")

;; Is t until viper-mode executes for the very first time. 
;; Prevents recursive descend into startup messages.
(defvar vip-first-time t)

(defvar viper-expert-level (if (boundp 'vip-expert-level) vip-expert-level 0)
  "User's expert level.
The minor mode vip-vi-diehard-minor-mode is in effect when
viper-expert-level is 1 or 2 or when vip-want-emacs-keys-in-vi is t.
The minor mode vip-insert-diehard-minor-mode is in effect when
viper-expert-level is 1 or 2 or if vip-want-emacs-keys-in-insert is t.
Use `M-x viper-set-expert-level' to change this.")

;; Max expert level supported by Viper. This is NOT a user option.
;; It is here to make it hard for the user from resetting it.
(defconst viper-max-expert-level 5)


;;; ISO characters
  
(vip-deflocalvar vip-automatic-iso-accents nil "")
(defcustom vip-automatic-iso-accents nil
  "*If non-nil, ISO accents will be turned on in insert/replace emacs states and turned off in vi-state. 
For some users, this behavior may be too primitive. In this case, use
insert/emacs/vi state hooks."
  :type 'boolean
  :group 'viper)
  

;; VI-style Undo

;; Used to 'undo' complex commands, such as replace and insert commands.
(vip-deflocalvar vip-undo-needs-adjustment nil)
(put 'vip-undo-needs-adjustment 'permanent-local t)

;; A mark that Viper puts on buffer-undo-list.  Marks the beginning of a
;; complex command that must be undone atomically. If inserted, it is
;; erased by vip-change-state-to-vi and vip-repeat.
(defconst vip-buffer-undo-list-mark 'viper)

(defcustom vip-keep-point-on-undo nil
  "*Non-nil means not to move point while undoing commands.
This style is different from Emacs and Vi. Try it to see if
it better fits your working style."
  :type 'boolean
  :tag "Preserve Position of Point After Undo"
  :group 'viper)  

;; Replace mode and changing text

;; Viper's own after/before change functions, which get vip-add-hook'ed to
;; Emacs's
(vip-deflocalvar vip-after-change-functions nil "")
(vip-deflocalvar vip-before-change-functions nil "")
(vip-deflocalvar vip-post-command-hooks nil "")
(vip-deflocalvar vip-pre-command-hooks nil "")

;; Can be used to pass global states around for short period of time
(vip-deflocalvar vip-intermediate-command nil "")

;; Indicates that the current destructive command has started in replace mode.
(vip-deflocalvar vip-began-as-replace nil "")

(defcustom vip-allow-multiline-replace-regions t
  "If non-nil, Viper will allow multi-line replace regions.
This is an extension to standard Vi.
If nil, commands that attempt to replace text spanning multiple lines first
delete the text being replaced, as in standard Vi."
  :type 'boolean
  :group 'viper)

(defcustom vip-replace-overlay-cursor-color "Red"
  "*Cursor color when Viper is in Replace state."
  :type 'string
  :group 'viper)
(defcustom vip-insert-state-cursor-color "Green"
  "Cursor color when Viper is in insert state."
  :type 'string
  :group 'viper)

;; place to save cursor colow when switching to insert mode
(vip-deflocalvar vip-saved-cursor-color nil "")
  
(vip-deflocalvar vip-replace-overlay nil "")
(put 'vip-replace-overlay 'permanent-local t)

(defcustom vip-replace-overlay-pixmap "gray3"
  "Pixmap to use for search face on non-color displays."
  :type 'string
  :group 'viper)
(defcustom vip-search-face-pixmap "gray3"
  "Pixmap to use for search face on non-color displays."
  :type 'string
  :group 'viper)


(defcustom vip-replace-region-end-delimiter "$"
  "A string marking the end of replacement regions.
It is used only with TTYs or if `vip-use-replace-region-delimiters'
is non-nil."
  :type 'string
  :group 'viper)
(defcustom vip-replace-region-start-delimiter ""
  "A string marking the beginning of replacement regions.
It is used only with TTYs or if `vip-use-replace-region-delimiters'
is non-nil."
  :type 'string
  :group 'viper)
(defcustom vip-use-replace-region-delimiters (not (vip-has-face-support-p))
  "*If non-nil, Viper will always use `vip-replace-region-end-delimiter' and
`vip-replace-region-start-delimiter' to delimit replacement regions, even on
color displays. By default, the delimiters are used only on TTYs."
  :type 'boolean
  :group 'viper)
  
;; XEmacs requires glyphs
(if vip-xemacs-p
    (progn
      (or (glyphp vip-replace-region-end-delimiter)
	  (setq vip-replace-region-end-delimiter
		(make-glyph vip-replace-region-end-delimiter)))
      (or (glyphp vip-replace-region-start-delimiter)
	  (setq vip-replace-region-start-delimiter
		(make-glyph vip-replace-region-start-delimiter)))
      ))
      
  
;; These are local marker that must be initialized to nil and moved with
;; `vip-move-marker-locally'
;;
;; Remember the last position inside the replace region.
(vip-deflocalvar vip-last-posn-in-replace-region nil)
;; Remember the last position while inserting
(vip-deflocalvar vip-last-posn-while-in-insert-state nil)
(put 'vip-last-posn-in-replace-region 'permanent-local t)
(put 'vip-last-posn-while-in-insert-state 'permanent-local t)

(vip-deflocalvar vip-sitting-in-replace nil "")
(put 'vip-sitting-in-replace 'permanent-local t)
  
;; Remember the number of characters that have to be deleted in replace
;; mode to compensate for the inserted characters.
(vip-deflocalvar vip-replace-chars-to-delete 0 "")
(vip-deflocalvar vip-replace-chars-deleted 0 "")

;; Insertion ring and command ring
(defcustom vip-insertion-ring-size 14
  "The size of history of inserted text.
This is a list where Viper keeps the history of previously inserted pieces of
text."
  :type 'integer
  :group 'viper)
;; The insertion ring.
(defvar vip-insertion-ring nil)
;; This is temp insertion ring. Used to do rotation for display purposes.
;; When rotation just started, it is initialized to vip-insertion-ring.
(defvar vip-temp-insertion-ring nil)
(defvar vip-last-inserted-string-from-insertion-ring "")

(defcustom vip-command-ring-size 14
  "The size of history of Vi commands repeatable with dot."
  :type 'integer
  :group 'viper)
;; The command ring.
(defvar vip-command-ring nil)
;; This is temp command ring. Used to do rotation for display purposes.
;; When rotation just started, it is initialized to vip-command-ring.
(defvar vip-temp-command-ring nil)

;; Fast keyseq and ESC keyseq timeouts
(defcustom vip-fast-keyseq-timeout 200
  "*Key sequence separated by no more than this many milliseconds is viewed as a Vi-style macro, if such a macro is defined.
Setting this too high may slow down your typing. Setting this value too low
will make it hard to use Vi-stile timeout macros."
  :type 'integer
  :group 'viper)

(defcustom vip-ESC-keyseq-timeout (if (vip-window-display-p)
				      0 vip-fast-keyseq-timeout)
  "*Key sequence beginning with ESC and separated by no more than this many milliseconds is considered to be generated by a keyboard function key.
Setting this too high may slow down switching from insert to vi state. Setting
this value too low will make it impossible to use function keys in insert mode
on a dumb terminal."
  :type 'integer
  :group 'viper)

;; Modes and related variables

;; Current mode.  One of: `emacs-state', `vi-state', `insert-state'
(vip-deflocalvar vip-current-state 'emacs-state)


;; Autoindent in insert

;; Variable that keeps track of whether C-t has been pressed.
(vip-deflocalvar vip-cted nil "")

;; Preserve the indent value, used by C-d in insert mode.
(vip-deflocalvar vip-current-indent 0)

;; Whether to preserve the indent, used by C-d in insert mode.
(vip-deflocalvar vip-preserve-indent nil)

(vip-deflocalvar vip-auto-indent nil "")
(defcustom vip-auto-indent nil
  "*Enable autoindent, if t.
This is a buffer-local variable."
  :type 'boolean
  :group 'viper)

(vip-deflocalvar vip-electric-mode t "")
(defcustom vip-electric-mode t
  "*If t, electrify Viper.
Currently, this only electrifies auto-indentation, making it appropriate to the
mode of the buffer.
This means that auto-indentation will depart from standard Vi and will indent
appropriate to the mode of the buffer. This is especially useful for editing
programs and LaTeX documents."
  :type 'boolean
  :group 'viper)

(defcustom vip-shift-width 8
  "*The shiftwidth variable."
  :type 'integer
  :group 'viper)

;; Variables for repeating destructive commands

(defcustom vip-keep-point-on-repeat t
  "*If t, don't move point when repeating previous command.
This is useful for doing repeated changes with the '.' key.
The user can change this to nil, if she likes when the cursor moves
to a new place after repeating previous Vi command."
  :type 'boolean
  :group 'viper) 

;; Remember insert point as a marker.  This is a local marker that must be
;; initialized to nil and moved with `vip-move-marker-locally'.
(vip-deflocalvar vip-insert-point nil)
(put 'vip-insert-point 'permanent-local t)

;; This remembers the point before dabbrev-expand was called.
;; If vip-insert-point turns out to be bigger than that, it is reset
;; back to vip-pre-command-point.
;; The reason this is needed is because dabbrev-expand (and possibly
;; others) may jump to before the insertion point, delete something and
;; then reinsert a bigger piece. For instance:  bla^blo
;; If dabbrev-expand is called after `blo' and ^ undicates vip-insert-point,
;; then point jumps to the beginning of `blo'. If expansion is found, `blablo'
;; is deleted, and we have |^, where | denotes point. Next, dabbrev-expand
;; will insert the expansion, and we get: blablo^
;; Whatever we insert next goes before the ^, i.e., before the
;; vip-insert-point marker. So, Viper will think that nothing was
;; inserted. Remembering the orig position of the marker circumvents the
;; problem.
;; We don't know of any command, except dabbrev-expand, that has the same
;; problem. However, the same trick can be used if such a command is
;; discovered later.
;;
(vip-deflocalvar vip-pre-command-point nil)
(put 'vip-pre-command-point 'permanent-local t) ; this is probably an overkill

;; This is used for saving inserted text.
(defvar vip-last-insertion  nil)
  
;; Remembers the last replaced region.
(defvar vip-last-replace-region "")
  
;; Remember com point as a marker.
;; This is a local marker. Should be moved with `vip-move-marker-locally'
(vip-deflocalvar vip-com-point nil)

;; If non-nil, the value is a list (M-COM VAL COM REG inserted-text cmd-keys)
;; It is used to re-execute last destructive command.
;; M-COM is a Lisp symbol representing the function to be executed.
;; VAL is the prefix argument that was used with that command.
;; COM is an internal descriptor, such as ?r, ?c, ?C, which contains
;; additional information on how the function in M-COM is to be handled.
;; REG is the register used by command
;; INSERTED-TEXT is text inserted by that command (in case of o, c, C, i, r
;; commands).
;; COMMAND-KEYS are the keys that were typed to invoke the command.
(defvar vip-d-com nil)

;; The character remembered by the Vi `r' command.
(defvar vip-d-char nil)

;; Name of register to store deleted or yanked strings
(defvar vip-use-register nil)



;; Variables for Moves and Searches

;; For use by `;' command.
(defvar vip-f-char nil)

;; For use by `.' command.
(defvar vip-F-char nil)

;; For use by `;' command.
(defvar vip-f-forward nil)

;; For use by `;' command.
(defvar vip-f-offset nil)

;; Last search string
(defvar vip-s-string "")

(defcustom vip-quote-string "> "
  "String inserted at the beginning of quoted region."
  :type 'string
  :group 'viper)

;; If t, search is forward.
(defvar vip-s-forward nil)

(defcustom vip-case-fold-search nil
  "*If not nil, search ignores cases."
  :type 'boolean
  :group 'viper)

(defcustom vip-re-search t
  "*If not nil, search is regexp search, otherwise vanilla search."
  :type 'boolean
  :tag "Regexp Search"
  :group 'viper)

(defcustom vip-search-scroll-threshold 2
  "*If search lands within this threshnold from the window top/bottom,
the window will be scrolled up or down appropriately, to reveal context.
If you want Viper search to behave as usual in Vi, set this variable to a
negative number."
  :type 'boolean
  :group 'viper)

(defcustom vip-re-query-replace t
  "*If t then do regexp replace, if nil then do string replace."
  :type 'boolean
  :tag "Regexp Query Replace"
  :group 'viper)

(defcustom vip-re-replace t
  "*If t, do regexp replace. nil means do string replace."
  :type 'boolean
  :tag "Regexp Replace"
  :group 'viper)

(defcustom vip-parse-sexp-ignore-comments t
  "*If t, `%' ignores the parentheses that occur inside comments."
  :type 'boolean
  :group 'viper)

(vip-deflocalvar vip-ex-style-motion t "")
(defcustom vip-ex-style-motion t
  "*If t, the commands l,h do not cross lines, etc (Ex-style).
If nil, these commands cross line boundaries."
  :type 'boolean
  :group 'viper)

(vip-deflocalvar vip-ex-style-editing-in-insert t "")
(defcustom vip-ex-style-editing-in-insert t
  "*If t, `Backspace' and `Delete' don't cross line boundaries in insert, etc.
Note: this doesn't preclude `Backspace' and `Delete' from deleting characters
by moving past the insertion point. This is a feature, not a bug."
  :type 'boolean
  :group 'viper)

(vip-deflocalvar vip-ESC-moves-cursor-back vip-ex-style-editing-in-insert "")
(defcustom vip-ESC-moves-cursor-back nil
  "*If t, ESC moves cursor back when changing from insert to vi state.
If nil, the cursor stays where it was."
  :type 'boolean
  :group 'viper)

(vip-deflocalvar vip-delete-backwards-in-replace nil "")
(defcustom vip-delete-backwards-in-replace nil
  "*If t, DEL key will delete characters while moving the cursor backwards.
If nil, the cursor will move backwards without deleting anything."
  :type 'boolean
  :group 'viper)

(defcustom vip-buffer-search-char nil
  "*Key used for buffer-searching. Must be a character type, e.g., ?g."
  :type '(choice (const nil) character)
  :group 'viper)

(defcustom vip-search-wrap-around-t t
  "*If t, search wraps around."
  :type 'boolean
  :tag "Search Wraps Around"
  :group 'viper)
  
(vip-deflocalvar vip-related-files-and-buffers-ring nil "")
(defcustom vip-related-files-and-buffers-ring nil
  "*List of file and buffer names that are considered to be related to the current buffer.
Related buffers can be cycled through via :R and :P commands."
  :type 'boolean
  :group 'viper)
(put 'vip-related-files-and-buffers-ring 'permanent-local t)

;; Used to find out if we are done with searching the current buffer.
(vip-deflocalvar vip-local-search-start-marker nil)
;; As above, but global
(defvar vip-search-start-marker (make-marker))

;; the search overlay
(vip-deflocalvar vip-search-overlay nil)


(defvar vip-heading-start 
  (concat "^\\s-*(\\s-*defun\\s-\\|"			        ; lisp
	  "^{\\s-*$\\|^[_a-zA-Z][^()]*[()].*{\\s-*$\\|"	        ; C/C++
	  "^\\s-*class.*{\\|^\\s-*struct.*{\\|^\\s-*enum.*{\\|"
	  "^\\\\[sb][a-z]*{.*}\\s-*$\\|"	    		; latex
	  "^@node\\|@table\\|^@m?enu\\|^@itemize\\|^@if\\|"	; texinfo
	  "^.+:-")			                        ; prolog
  "*Regexps for Headings. Used by \[\[ and \]\].")

(defvar vip-heading-end 
  (concat "^}\\|"						; C/C++
	  "^\\\\end{\\|"					; latex
	  "^@end \\|"						; texinfo
	  ")\n\n[ \t\n]*\\|"					; lisp
	  "\\.\\s-*$")						; prolog
      "*Regexps to end Headings/Sections. Used by \[\].")


;; These two vars control the interaction of jumps performed by ' and `.
;; In this new version, '' doesn't erase the marks set by ``, so one can
;; use both kinds of jumps interchangeably and without loosing positions
;; inside the lines.

;; Remembers position of the last jump done using ``'.
(vip-deflocalvar vip-last-jump  nil)
;; Remembers position of the last jump done using `''.
(vip-deflocalvar vip-last-jump-ignore 0)

;; History variables

;; History of search strings.
(defvar vip-search-history  (list ""))
;; History of query-replace strings used as a source.
(defvar vip-replace1-history nil)
;; History of query-replace strings used as replacement.
(defvar vip-replace2-history nil)
;; History of region quoting strings.
(defvar vip-quote-region-history (list vip-quote-string))
;; History of Ex-style commands.
(defvar vip-ex-history nil)
;; History of shell commands.
(defvar vip-shell-history nil)


;; Last shell command. There are two of these, one for Ex (in viper-ex)
;; and one for Vi.

;; Last shell command executed with ! command.
(defvar vip-last-shell-com nil)



;;; Miscellaneous

(defvar vip-inhibit-startup-message nil
  "Whether Viper startup message should be inhibited.")

(defcustom vip-spell-function 'ispell-region
  "Spell function used by #s<move> command to spell."
  :type 'function
  :group 'viper)

(defcustom vip-tags-file-name "TAGS"
  "The tags file used by Viper."
  :type 'string
  :group 'viper)

;; Indicates if we are in the middle of executing a command that takes another
;; command as an argument, e.g., cw, dw, etc.
(defvar vip-inside-command-argument-action nil)

;; Minibuffer

(defcustom vip-vi-style-in-minibuffer t
  "If t, use vi-style editing in minibuffer.
Should be set in `~/.vip' file."
  :type 'boolean
  :group 'viper)
  
;; overlay used in the minibuffer to indicate which state it is in
(vip-deflocalvar vip-minibuffer-overlay nil)

;; Hook, specific to Viper, which is run just *before* exiting the minibuffer.
;; Beginning with Emacs 19.26, the standard `minibuffer-exit-hook' is run
;; *after* exiting the minibuffer
(defvar vip-minibuffer-exit-hook nil)
       

;; Mode line
(defconst vip-vi-state-id  	"<V> "
  "Mode line tag identifying the Vi mode of Viper.")
(defconst vip-emacs-state-id	"<E> "
  "Mode line tag identifying the Emacs mode of Viper.")
(defconst vip-insert-state-id	"<I> "
  "Mode line tag identifying the Insert mode of Viper.")
(defconst vip-replace-state-id	"<R> "
  "Mode line tag identifying the Replace mode of Viper.")


(defcustom vip-vi-state-hook nil
  "*Hooks run just before the switch to Vi mode is completed."
  :type 'hook
  :group 'viper)
(defcustom vip-insert-state-hook nil
  "*Hooks run just before the switch to Insert mode is completed."
  :type 'hook
  :group 'viper)
(defcustom vip-replace-state-hook nil
  "*Hooks run just before the switch to Replace mode is completed."
  :type 'hook
  :group 'viper)
(defcustom vip-emacs-state-hook nil
  "*Hooks run just before the switch to Emacs mode is completed."
  :type 'hook
  :group 'viper)
  
(defcustom vip-load-hook nil
  "Hooks run just after loading Viper."
  :type 'hook
  :group 'viper)
  

;;; Local Variables:
;;; eval: (put 'vip-deflocalvar 'lisp-indent-hook 'defun)
;;; End:

;;;  viper-ex.el ends here
