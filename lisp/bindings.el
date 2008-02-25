;;; bindings.el --- define standard key bindings and some variables

;; Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994, 1995, 1996, 1999,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; Special formatting conventions are used in this file!
;;;
;;; A backslash-newline is used at the beginning of a documentation string
;;; when that string should be stored in the file etc/DOCnnn, not in core.
;;;
;;; Such strings read into Lisp as numbers (during the pure-loading phase).
;;;
;;; But you must obey certain rules to make sure the string is understood
;;; and goes into etc/DOCnnn properly.
;;;
;;; The doc string must appear in the standard place in a call to
;;; defun, autoload, defvar or defconst.  No Lisp macros are recognized.
;;; The open-paren starting the definition must appear in column 0.
;;;
;;; In defvar and defconst, there is an additional rule:
;;; The double-quote that starts the string must be on the same
;;; line as the defvar or defconst.
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; Code:

(defun make-mode-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))


(defun mode-line-toggle-read-only (event)
  "Like `toggle-read-only', for the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (toggle-read-only)
    (force-mode-line-update)))


(defun mode-line-toggle-modified (event)
  "Toggle the buffer-modified flag from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (set-buffer-modified-p (not (buffer-modified-p)))
    (force-mode-line-update)))


(defun mode-line-widen (event)
  "Widen a buffer from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (widen)
    (force-mode-line-update)))


(defun mode-line-abbrev-mode (event)
  "Turn off `abbrev-mode' from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (abbrev-mode)
    (force-mode-line-update)))


(defun mode-line-auto-fill-mode (event)
  "Turn off `auto-fill-mode' from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (auto-fill-mode)
    (force-mode-line-update)))


(defvar mode-line-input-method-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-2]
      (lambda (e)
	(interactive "e")
	(save-selected-window
	  (select-window
	   (posn-window (event-start e)))
	  (toggle-input-method)
	  (force-mode-line-update))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(save-selected-window
	  (select-window
	   (posn-window (event-start e)))
	  (describe-current-input-method))))
    (purecopy map)))


(defvar mode-line-coding-system-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
	(interactive "e")
	(save-selected-window
	  (select-window (posn-window (event-start e)))
	  (when (and enable-multibyte-characters
		     buffer-file-coding-system)
	    (describe-coding-system buffer-file-coding-system)))))
    (purecopy map))
  "Local keymap for the coding-system part of the mode line.")


(defun mode-line-change-eol (event)
  "Cycle through the various possible kinds of end-of-line styles."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let ((eol (coding-system-eol-type buffer-file-coding-system)))
      (set-buffer-file-coding-system
       (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))

(defvar mode-line-eol-desc-cache nil)

(defun mode-line-eol-desc ()
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
	 (mnemonic (coding-system-eol-type-mnemonic buffer-file-coding-system))
	 (desc (assq eol mode-line-eol-desc-cache)))
    (if (and desc (eq (cadr desc) mnemonic))
	(cddr desc)
      (if desc (setq mode-line-eol-desc-cache nil)) ;Flush the cache if stale.
      (setq desc
	    (propertize
	     mnemonic
	     'help-echo (format "End-of-line style: %s\nmouse-1 to cycle"
				(if (eq eol 0) "Unix-style LF"
				  (if (eq eol 1) "Dos-style CRLF"
				    (if (eq eol 2) "Mac-style CR"
				      "Undecided"))))
	     'keymap
	     (eval-when-compile
	       (let ((map (make-sparse-keymap)))
		 (define-key map [mode-line mouse-1] 'mode-line-change-eol)
		 map))
	     'mouse-face 'mode-line-highlight))
      (push (cons eol (cons mnemonic desc)) mode-line-eol-desc-cache)
      desc)))

(defvar mode-line-client
  `(""
    (:propertize ("" (:eval (if (frame-parameter nil 'client) "@" "")))
		 help-echo "Emacsclient frame"))
  "Mode-line control for identifying Emacsclient frames.")

(defvar mode-line-mule-info
  `(""
    (current-input-method
     (:propertize ("" current-input-method-title)
		  help-echo (concat
			     "Current input method: "
			     current-input-method
			     "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
		  local-map ,mode-line-input-method-map
		  mouse-face mode-line-highlight))
    ,(propertize
      "%z"
      'help-echo
      #'(lambda (window object point)
	  (with-current-buffer (window-buffer window)
	    ;; Don't show this tip if the coding system is nil,
	    ;; it reads like a bug, and is not useful anyway.
	    (when buffer-file-coding-system
	      (format "Buffer coding system %s\nmouse-1: describe coding system"
		      (if enable-multibyte-characters
			  (concat "(multi-byte): "
				  (symbol-name buffer-file-coding-system))
			(concat "(unibyte): "
				(symbol-name buffer-file-coding-system)))))))
      'mouse-face 'mode-line-highlight
      'local-map mode-line-coding-system-map)
    (:eval (mode-line-eol-desc)))
  "Mode-line control for displaying information of multilingual environment.
Normally it displays current input method (if any activated) and
mnemonics of the following coding systems:
  coding system for saving or writing the current buffer
  coding system for keyboard input (if Emacs is running on terminal)
  coding system for terminal output (if Emacs is running on terminal)"
  ;; Currently not:
  ;;  coding system for decoding output of buffer process (if any)
  ;;  coding system for encoding text to send to buffer process (if any)."
)

(make-variable-buffer-local 'mode-line-mule-info)

(defvar mode-line-frame-identification '(window-system "  " "-%F  ")
  "Mode-line control to describe the current frame.")

(defvar mode-line-process nil "\
Mode-line control for displaying info on process status.
Normally nil in most modes, since there is no process to display.")

(make-variable-buffer-local 'mode-line-process)

(defvar mode-line-modified
  (list (propertize
	 "%1*"
	 'help-echo (purecopy (lambda (window object point)
 				(format "Buffer is %s\nmouse-1 toggles"
					(save-selected-window
					  (select-window window)
					  (if buffer-read-only
					      "read-only"
					    "writable")))))
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-1
			       #'mode-line-toggle-read-only))
	 'mouse-face 'mode-line-highlight)
	(propertize
	 "%1+"
	 'help-echo  (purecopy (lambda (window object point)
				 (format "Buffer is %sodified\nmouse-1 toggles modified state"
					 (save-selected-window
					   (select-window window)
					   (if (buffer-modified-p)
					     "M"
					   "Not m")))))
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-1 #'mode-line-toggle-modified))
	 'mouse-face 'mode-line-highlight))
  "Mode-line control for displaying whether current buffer is modified.")

(make-variable-buffer-local 'mode-line-modified)

(defvar mode-line-remote
  (list (propertize
	 "%1@"
	 'help-echo (purecopy (lambda (window object point)
 				(format "%s"
					(save-selected-window
					  (select-window window)
					  (concat
					  (if (file-remote-p default-directory)
					      "Remote: "
					    "Local: ")
					  default-directory)))))))
  "Mode-line flag to show if default-directory for current buffer is remote.")

(make-variable-buffer-local 'mode-line-remote)

;; Actual initialization is below.
(defvar mode-line-position nil
  "Mode-line control for displaying the position in the buffer.
Normally displays the buffer percentage and, optionally, the
buffer size, the line number and the column number.")

(defvar mode-line-modes nil
  "Mode-line control for displaying major and minor modes.")

(defvar mode-line-major-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'mouse-major-mode-menu)
    (define-key map [mode-line mouse-2] 'describe-mode)
    (define-key map [mode-line down-mouse-3] 'mode-line-mode-menu-1)
    map) "\
Keymap to display on major mode.")

(defvar mode-line-minor-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'mouse-minor-mode-menu)
    (define-key map [mode-line mouse-2] 'mode-line-minor-mode-help)
    (define-key map [mode-line down-mouse-3] 'mode-line-mode-menu-1)
    (define-key map [header-line down-mouse-3] 'mode-line-mode-menu-1)
    map) "\
Keymap to display on minor modes.")

(let* ((help-echo
	;; The multi-line message doesn't work terribly well on the
	;; bottom mode line...  Better ideas?
	;; 	  "\
	;; mouse-1: select window, mouse-2: delete others, mouse-3: delete,
	;; drag-mouse-1: resize, C-mouse-2: split horizontally"
	"mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display")
       (dashes (propertize "--" 'help-echo help-echo))
       (standard-mode-line-format
	(list
	 "%e"
	 (propertize "-" 'help-echo help-echo)
	 'mode-line-mule-info
	 'mode-line-client
	 'mode-line-modified
	 'mode-line-remote
	 'mode-line-frame-identification
	 'mode-line-buffer-identification
	 (propertize "   " 'help-echo help-echo)
	 'mode-line-position
	 '(vc-mode vc-mode)
	 (propertize "  " 'help-echo help-echo)
	 'mode-line-modes
	 `(which-func-mode ("" which-func-format ,dashes))
	 `(global-mode-string (,dashes global-mode-string))
	 (propertize "-%-" 'help-echo help-echo)))
       (standard-mode-line-modes
	(list
	 (propertize "%[(" 'help-echo help-echo)
	 `(:propertize ("" mode-name)
		       help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
		       mouse-face mode-line-highlight
		       local-map ,mode-line-major-mode-keymap)
	 '("" mode-line-process)
	 `(:propertize ("" minor-mode-alist)
		       mouse-face mode-line-highlight
		       help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
		       local-map ,mode-line-minor-mode-keymap)
	 (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
		     'mouse-face 'mode-line-highlight
		     'local-map (make-mode-line-mouse-map
				 'mouse-2 #'mode-line-widen))
	 (propertize ")%]--" 'help-echo help-echo)))

       (standard-mode-line-position
	`((-3 ,(propertize "%p" 'help-echo help-echo))
	  (size-indication-mode
	   (8 ,(propertize
		" of %I"
		;; XXX needs better description
		'help-echo (format "Size indication mode\n%s" help-echo))))
	  (line-number-mode
	   ((column-number-mode
	     (10 ,(propertize
		   " (%l,%c)"
		   'help-echo
		   (format "Line number and Column number\n%s" help-echo)))
	     (6 ,(propertize
		  " L%l"
		  'help-echo
		  (format "Line number\n%s" help-echo)))))
	   ((column-number-mode
	     (5 ,(propertize
		  " C%c"
		  'help-echo
		  (format "Column number\n%s" help-echo)))))))))

  (setq-default mode-line-format standard-mode-line-format)
  (put 'mode-line-format 'standard-value
       (list `(quote ,standard-mode-line-format)))

  (setq-default mode-line-modes standard-mode-line-modes)
  (put 'mode-line-modes 'standard-value
       (list `(quote ,standard-mode-line-modes)))

  (setq-default mode-line-position standard-mode-line-position)
  (put 'mode-line-position 'standard-value
       (list `(quote ,standard-mode-line-position))))

(defvar mode-line-buffer-identification-keymap
  ;; Add menu of buffer operations to the buffer identification part
  ;; of the mode line.or header line.
  (let ((map (make-sparse-keymap)))
    ;; Bind down- events so that the global keymap won't ``shine
    ;; through''.
    (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
    (define-key map [header-line down-mouse-1] 'ignore)
    (define-key map [header-line mouse-1] 'mode-line-previous-buffer)
    (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
    (define-key map [header-line down-mouse-3] 'ignore)
    (define-key map [header-line mouse-3] 'mode-line-next-buffer)
    map) "\
Keymap for what is displayed by `mode-line-buffer-identification'.")

(defun propertized-buffer-identification (fmt)
  "Return a list suitable for `mode-line-buffer-identification'.
FMT is a format specifier such as \"%12b\".  This function adds
text properties for face, help-echo, and local-map to it."
  (list (propertize fmt
		    'face 'mode-line-buffer-id
		    'help-echo
		    (purecopy "Buffer name\n\
mouse-1: previous buffer\n\
mouse-3: next buffer")
		    'mouse-face 'mode-line-highlight
		    'local-map mode-line-buffer-identification-keymap)))

(defvar mode-line-buffer-identification (propertized-buffer-identification "%12b") "\
Mode-line control for identifying the buffer being displayed.
Its default value is (\"%12b\") with some text properties added.
Major modes that edit things other than ordinary files may change this
\(e.g. Info, Dired,...)")

(make-variable-buffer-local 'mode-line-buffer-identification)

(defun unbury-buffer () "\
Switch to the last buffer in the buffer list."
  (interactive)
  (switch-to-buffer (last-buffer)))

(defun mode-line-unbury-buffer (event) "\
Call `unbury-buffer' in this window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (unbury-buffer)))

(defun mode-line-bury-buffer (event) "\
Like `bury-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (bury-buffer)))

(defun mode-line-other-buffer () "\
Switch to the most recently selected buffer other than the current one."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun mode-line-next-buffer (event)
  "Like `next-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (next-buffer)))

(defun mode-line-previous-buffer (event)
  "Like `previous-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (previous-buffer)))

(defvar mode-line-mode-menu (make-sparse-keymap "Minor Modes") "\
Menu of mode operations in the mode line.")

(defun mode-line-mode-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (mode-line-mode-menu event))
	   (binding (and selection (lookup-key mode-line-mode-menu
					       (vector (car selection))))))
      (if binding
	  (call-interactively binding)))))

(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

;; Use mode-line-mode-menu for local minor-modes only.
;; Global ones can go on the menubar (Options --> Show/Hide).
(define-key mode-line-mode-menu [overwrite-mode]
  `(menu-item ,(purecopy "Overwrite (Ovwrt)") overwrite-mode
	      :help "Overwrite mode: typed characters replace existing text"
	      :button (:toggle . overwrite-mode)))
(define-key mode-line-mode-menu [outline-minor-mode]
  `(menu-item ,(purecopy "Outline (Outl)") outline-minor-mode
	      ;; XXX: This needs a good, brief description.
	      :help ""
	      :button (:toggle . (bound-and-true-p outline-minor-mode))))
(define-key mode-line-mode-menu [highlight-changes-mode]
  `(menu-item ,(purecopy "Highlight changes (Chg)") highlight-changes-mode
	      :help "Show changes in the buffer in a distinctive color"
	      :button (:toggle . (bound-and-true-p highlight-changes-mode))))
(define-key mode-line-mode-menu [hide-ifdef-mode]
  `(menu-item ,(purecopy "Hide ifdef (Ifdef)") hide-ifdef-mode
	      :help "Show/Hide code within #ifdef constructs"
	      :button (:toggle . (bound-and-true-p hide-ifdef-mode))))
(define-key mode-line-mode-menu [glasses-mode]
  `(menu-item ,(purecopy "Glasses (o^o)") glasses-mode
	      :help "Insert virtual separators to make long identifiers easy to read"
	      :button (:toggle . (bound-and-true-p glasses-mode))))
(define-key mode-line-mode-menu [font-lock-mode]
  `(menu-item ,(purecopy "Font Lock") font-lock-mode
	      :help "Syntax coloring"
	      :button (:toggle . font-lock-mode)))
(define-key mode-line-mode-menu [flyspell-mode]
  `(menu-item ,(purecopy "Flyspell (Fly)") flyspell-mode
	      :help "Spell checking on the fly"
	      :button (:toggle . (bound-and-true-p flyspell-mode))))
(define-key mode-line-mode-menu [auto-revert-tail-mode]
  `(menu-item ,(purecopy "Auto revert tail (Tail)") auto-revert-tail-mode
	      :help "Revert the tail of the buffer when buffer grows"
	      :enable (buffer-file-name)
	      :button (:toggle . (bound-and-true-p auto-revert-tail-mode))))
(define-key mode-line-mode-menu [auto-revert-mode]
  `(menu-item ,(purecopy "Auto revert (ARev)") auto-revert-mode
	      :help "Revert the buffer when the file on disk changes"
	      :button (:toggle . (bound-and-true-p auto-revert-mode))))
(define-key mode-line-mode-menu [auto-fill-mode]
  `(menu-item ,(purecopy "Auto fill (Fill)") auto-fill-mode
	      :help "Automatically insert new lines"
	      :button (:toggle . auto-fill-function)))
(define-key mode-line-mode-menu [abbrev-mode]
  `(menu-item ,(purecopy "Abbrev (Abbrev)") abbrev-mode
	      :help "Automatically expand abbreviations"
	      :button (:toggle . abbrev-mode)))

(defun mode-line-mode-menu (event)
  (interactive "@e")
  (x-popup-menu event mode-line-mode-menu))

(defun mode-line-minor-mode-help (event)
  "Describe minor mode for EVENT on minor modes area of the mode line."
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (describe-minor-mode-from-indicator indicator)))

(defvar minor-mode-alist nil "\
Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line if VARIABLE's value is non-nil.

Actually, STRING need not be a string; any possible mode-line element
is okay.  See `mode-line-format'.")
;; Don't use purecopy here--some people want to change these strings.
(setq minor-mode-alist
      (list
       (list 'abbrev-mode " Abbrev")
       '(overwrite-mode overwrite-mode)
       (list 'auto-fill-function " Fill")
       ;; not really a minor mode...
       '(defining-kbd-macro " Def")))

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      (append
       (cond ((memq system-type '(ms-dos windows-nt))
	      '(".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk"
		".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386"))
	     ((eq system-type 'vax-vms)
	      '(".obj" ".exe" ".bin" ".lbin" ".sbin"
		".brn" ".rnt" ".lni"
		".olb" ".tlb" ".mlb" ".hlb"))
	     (t
	      '(".o" "~" ".bin" ".lbin" ".so"
		".a" ".ln" ".blg" ".bbl")))
       '(".elc" ".lof"
	 ".glo" ".idx" ".lot"
	 ;; TeX-related
	 ".fmt" ".tfm"
	 ;; Java compiled
	 ".class"
	 ;; CLISP
	 ".fas" ".lib" ".mem"
	 ;; CMUCL
	 ".x86f" ".sparcf"
         ;; Other CL implementations (Allegro, LispWorks, OpenMCL)
         ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl"
	 ;; Libtool
	 ".lo" ".la"
	 ;; Gettext
	 ".gmo" ".mo"
	 ;; Texinfo-related
	 ;; This used to contain .log, but that's commonly used for log
	 ;; files you do want to see, not just TeX stuff.  -- fx
	 ".toc" ".aux"
	 ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
	 ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs"
	 ;; Python byte-compiled
	 ".pyc" ".pyo")))

;; Suffixes used for executables.
(setq exec-suffixes
      (cond
       ((memq system-type '(ms-dos windows-nt))
	'(".exe" ".com" ".bat" ".cmd" ".btm" ""))
       (t
	'(""))))

;; Packages should add to this list appropriately when they are
;; loaded, rather than listing everything here.
(setq debug-ignored-errors
      '(beginning-of-line beginning-of-buffer end-of-line
	end-of-buffer end-of-file buffer-read-only
	file-supersession
      	"^Previous command was not a yank$"
	"^Minibuffer window is not active$"
	"^No previous history search regexp$"
	"^No later matching history item$"
	"^No earlier matching history item$"
	"^End of history; no default available$"
	"^End of history; no next item$"
	"^Beginning of history; no preceding item$"
	"^No recursive edit is in progress$"
	"^Changes to be undone are outside visible portion of buffer$"
	"^No undo information in this buffer$"
	"^No further undo information"
	"^Save not confirmed$"
	"^Recover-file cancelled\\.$"
	"^Cannot switch buffers in a dedicated window$"
        ))


(make-variable-buffer-local 'indent-tabs-mode)

;; We have base64 and md5 functions built in now.
(provide 'base64)
(provide 'md5)
(provide 'overlay '(display syntax-table field))
(provide 'text-properties '(display syntax-table field point-entered))

(define-key esc-map "\t" 'complete-symbol)

(defun complete-symbol (arg) "\
Perform tags completion on the text around point.
Completes to the set of names listed in the current tags table.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see).

With a prefix argument, this command does completion within
the collection of symbols listed in the index of the manual for the
language you are using."
  (interactive "P")
  (if arg
      (info-complete-symbol)
    (if (fboundp 'complete-tag)
	(complete-tag)
      ;; Don't autoload etags if we have no tags table.
      (error "%s" (substitute-command-keys
	      "No tags table loaded; use \\[visit-tags-table] to load one")))))

;; Reduce total amount of space we must allocate during this function
;; that we will not need to keep permanently.
(garbage-collect)


(setq help-event-list '(help f1))

(make-variable-buffer-local 'minor-mode-overriding-map-alist)

;; From frame.c
(global-set-key [switch-frame] 'handle-switch-frame)
(global-set-key [select-window] 'handle-select-window)

;; FIXME: Do those 3 events really ever reach the global-map ?
;;        It seems that they can't because they're handled via
;;        special-event-map which is used at very low-level.  -stef
(global-set-key [delete-frame] 'handle-delete-frame)
(global-set-key [iconify-frame] 'ignore-event)
(global-set-key [make-frame-visible] 'ignore-event)


;These commands are defined in editfns.c
;but they are not assigned to keys there.
(put 'narrow-to-region 'disabled t)
(define-key ctl-x-map "nn" 'narrow-to-region)
(define-key ctl-x-map "nw" 'widen)
;; (define-key ctl-x-map "n" 'narrow-to-region)
;; (define-key ctl-x-map "w" 'widen)

;; Quitting
(define-key global-map "\e\e\e" 'keyboard-escape-quit)
(define-key global-map "\C-g" 'keyboard-quit)

;; Used to be in termdev.el: when using several terminals, make C-z
;; suspend only the relevant terminal.
(substitute-key-definition 'suspend-emacs 'suspend-frame global-map)

(define-key global-map "\C-j" 'newline-and-indent)
(define-key global-map "\C-m" 'newline)
(define-key global-map "\C-o" 'open-line)
(define-key esc-map "\C-o" 'split-line)
(define-key global-map "\C-q" 'quoted-insert)
(define-key esc-map "^" 'delete-indentation)
(define-key esc-map "\\" 'delete-horizontal-space)
(define-key esc-map "m" 'back-to-indentation)
(define-key ctl-x-map "\C-o" 'delete-blank-lines)
(define-key esc-map " " 'just-one-space)
(define-key esc-map "z" 'zap-to-char)
(define-key esc-map "=" 'count-lines-region)
(define-key ctl-x-map "=" 'what-cursor-position)
(define-key esc-map ":" 'eval-expression)
;; Define ESC ESC : like ESC : for people who type ESC ESC out of habit.
(define-key esc-map "\M-:" 'eval-expression)
;; Changed from C-x ESC so that function keys work following C-x.
(define-key ctl-x-map "\e\e" 'repeat-complex-command)
;; New binding analogous to M-:.
(define-key ctl-x-map "\M-:" 'repeat-complex-command)
(define-key ctl-x-map "u" 'advertised-undo)
;; Many people are used to typing C-/ on X terminals and getting C-_.
(define-key global-map [?\C-/] 'undo)
(define-key global-map "\C-_" 'undo)
;; Richard said that we should not use C-x <uppercase letter> and I have
;; no idea whereas to bind it.  Any suggestion welcome.  -stef
;; (define-key ctl-x-map "U" 'undo-only)

(define-key esc-map "!" 'shell-command)
(define-key esc-map "|" 'shell-command-on-region)

(define-key global-map [?\C-x right] 'next-buffer)
(define-key global-map [?\C-x C-right] 'next-buffer)
(define-key global-map [?\C-x left] 'previous-buffer)
(define-key global-map [?\C-x C-left] 'previous-buffer)

(let ((map minibuffer-local-map))
  (define-key map "\en"   'next-history-element)
  (define-key map [next]  'next-history-element)
  (define-key map [down]  'next-history-element)
  (define-key map "\ep"   'previous-history-element)
  (define-key map [prior] 'previous-history-element)
  (define-key map [up]    'previous-history-element)
  (define-key map "\es"   'next-matching-history-element)
  (define-key map "\er"   'previous-matching-history-element)
  ;; Override the global binding (which calls indent-relative via
  ;; indent-for-tab-command).  The alignment that indent-relative tries to
  ;; do doesn't make much sense here since the prompt messes it up.
  (define-key map "\t"    'self-insert-command)
  (define-key minibuffer-local-map [C-tab] 'file-cache-minibuffer-complete))

(define-key global-map "\C-u" 'universal-argument)
(let ((i ?0))
  (while (<= i ?9)
    (define-key esc-map (char-to-string i) 'digit-argument)
    (setq i (1+ i))))
(define-key esc-map "-" 'negative-argument)
;; Define control-digits.
(let ((i ?0))
  (while (<= i ?9)
    (define-key global-map (read (format "[?\\C-%c]" i)) 'digit-argument)
    (setq i (1+ i))))
(define-key global-map [?\C--] 'negative-argument)
;; Define control-meta-digits.
(let ((i ?0))
  (while (<= i ?9)
    (define-key esc-map (read (format "[?\\C-%c]" i)) 'digit-argument)
    (setq i (1+ i))))
(define-key global-map [?\C-\M--] 'negative-argument)

(define-key global-map "\C-k" 'kill-line)
(define-key global-map "\C-w" 'kill-region)
(define-key esc-map "w" 'kill-ring-save)
(define-key esc-map "\C-w" 'append-next-kill)
(define-key global-map "\C-y" 'yank)
(define-key esc-map "y" 'yank-pop)

;; (define-key ctl-x-map "a" 'append-to-buffer)

(define-key global-map "\C-@" 'set-mark-command)
;; Many people are used to typing C-SPC and getting C-@.
(define-key global-map [?\C- ] 'set-mark-command)
(define-key ctl-x-map "\C-x" 'exchange-point-and-mark)
(define-key ctl-x-map "\C-@" 'pop-global-mark)
(define-key ctl-x-map [?\C- ] 'pop-global-mark)

(define-key global-map "\C-n" 'next-line)
(define-key global-map "\C-p" 'previous-line)
(define-key ctl-x-map "\C-n" 'set-goal-column)
(define-key global-map "\C-a" 'move-beginning-of-line)
(define-key global-map "\C-e" 'move-end-of-line)
(define-key esc-map "g" (make-sparse-keymap))
(define-key esc-map "g\M-g" 'goto-line)
(define-key esc-map "gg" 'goto-line)

(define-key esc-map "gn" 'next-error)
(define-key esc-map "g\M-n" 'next-error)
(define-key ctl-x-map "`" 'next-error)

(define-key esc-map "gp" 'previous-error)
(define-key esc-map "g\M-p" 'previous-error)

;;(defun function-key-error ()
;;  (interactive)
;;  (error "That function key is not bound to anything"))

(define-key global-map [menu] 'execute-extended-command)
(define-key global-map [find] 'search-forward)

;; Don't do this.  We define <delete> in function-key-map instead.
;(define-key global-map [delete] 'backward-delete-char)

;; natural bindings for terminal keycaps --- defined in X keysym order
(define-key global-map [C-S-backspace]  'kill-whole-line)
(define-key global-map [home]		'move-beginning-of-line)
(define-key global-map [C-home]		'beginning-of-buffer)
(define-key global-map [M-home]		'beginning-of-buffer-other-window)
(define-key esc-map    [home]		'beginning-of-buffer-other-window)
(define-key global-map [left]		'backward-char)
(define-key global-map [up]		'previous-line)
(define-key global-map [right]		'forward-char)
(define-key global-map [down]		'next-line)
(define-key global-map [prior]		'scroll-down)
(define-key global-map [next]		'scroll-up)
(define-key global-map [C-up]		'backward-paragraph)
(define-key global-map [C-down]		'forward-paragraph)
(define-key global-map [C-prior]	'scroll-right)
(put 'scroll-left 'disabled t)
(define-key global-map [C-next]		'scroll-left)
(define-key global-map [M-next]		'scroll-other-window)
(define-key esc-map    [next]		'scroll-other-window)
(define-key global-map [M-prior]	'scroll-other-window-down)
(define-key esc-map    [prior]		'scroll-other-window-down)
(define-key esc-map [?\C-\S-v]		'scroll-other-window-down)
(define-key global-map [end]		'move-end-of-line)
(define-key global-map [C-end]		'end-of-buffer)
(define-key global-map [M-end]		'end-of-buffer-other-window)
(define-key esc-map    [end]		'end-of-buffer-other-window)
(define-key global-map [begin]		'beginning-of-buffer)
(define-key global-map [M-begin]	'beginning-of-buffer-other-window)
(define-key esc-map    [begin]		'beginning-of-buffer-other-window)
;; (define-key global-map [select]	'function-key-error)
;; (define-key global-map [print]	'function-key-error)
(define-key global-map [execute]	'execute-extended-command)
(define-key global-map [insert]		'overwrite-mode)
(define-key global-map [C-insert]	'kill-ring-save)
(define-key global-map [S-insert]	'yank)
;; `insertchar' is what term.c produces.  Should we change term.c
;; to produce `insert' instead?
(define-key global-map [insertchar]	'overwrite-mode)
(define-key global-map [C-insertchar]	'kill-ring-save)
(define-key global-map [S-insertchar]	'yank)
(define-key global-map [undo]		'undo)
(define-key global-map [redo]		'repeat-complex-command)
(define-key global-map [again]		'repeat-complex-command) ; Sun keyboard
(define-key global-map [open]		'find-file) ; Sun
;; The following wouldn't work to interrupt running code since C-g is
;; treated specially in the event loop.
;; (define-key global-map [stop]		'keyboard-quit) ; Sun
;; (define-key global-map [clearline]	'function-key-error)
(define-key global-map [insertline]	'open-line)
(define-key global-map [deleteline]	'kill-line)
(define-key global-map [deletechar]	'delete-char)
;; (define-key global-map [backtab]	'function-key-error)
;; (define-key global-map [f1]		'function-key-error)
;; (define-key global-map [f2]		'function-key-error)
;; (define-key global-map [f3]		'function-key-error)
;; (define-key global-map [f4]		'function-key-error)
;; (define-key global-map [f5]		'function-key-error)
;; (define-key global-map [f6]		'function-key-error)
;; (define-key global-map [f7]		'function-key-error)
;; (define-key global-map [f8]		'function-key-error)
;; (define-key global-map [f9]		'function-key-error)
;; (define-key global-map [f10]		'function-key-error)
;; (define-key global-map [f11]		'function-key-error)
;; (define-key global-map [f12]		'function-key-error)
;; (define-key global-map [f13]		'function-key-error)
;; (define-key global-map [f14]		'function-key-error)
;; (define-key global-map [f15]		'function-key-error)
;; (define-key global-map [f16]		'function-key-error)
;; (define-key global-map [f17]		'function-key-error)
;; (define-key global-map [f18]		'function-key-error)
;; (define-key global-map [f19]		'function-key-error)
;; (define-key global-map [f20]		'function-key-error)
;; (define-key global-map [f21]		'function-key-error)
;; (define-key global-map [f22]		'function-key-error)
;; (define-key global-map [f23]		'function-key-error)
;; (define-key global-map [f24]		'function-key-error)
;; (define-key global-map [f25]		'function-key-error)
;; (define-key global-map [f26]		'function-key-error)
;; (define-key global-map [f27]		'function-key-error)
;; (define-key global-map [f28]		'function-key-error)
;; (define-key global-map [f29]		'function-key-error)
;; (define-key global-map [f30]		'function-key-error)
;; (define-key global-map [f31]		'function-key-error)
;; (define-key global-map [f32]		'function-key-error)
;; (define-key global-map [f33]		'function-key-error)
;; (define-key global-map [f34]		'function-key-error)
;; (define-key global-map [f35]		'function-key-error)
;; (define-key global-map [kp-backtab]	'function-key-error)
;; (define-key global-map [kp-space]	'function-key-error)
;; (define-key global-map [kp-tab]		'function-key-error)
;; (define-key global-map [kp-enter]	'function-key-error)
;; (define-key global-map [kp-f1]		'function-key-error)
;; (define-key global-map [kp-f2]		'function-key-error)
;; (define-key global-map [kp-f3]		'function-key-error)
;; (define-key global-map [kp-f4]		'function-key-error)
;; (define-key global-map [kp-multiply]	'function-key-error)
;; (define-key global-map [kp-add]		'function-key-error)
;; (define-key global-map [kp-separator]	'function-key-error)
;; (define-key global-map [kp-subtract]	'function-key-error)
;; (define-key global-map [kp-decimal]	'function-key-error)
;; (define-key global-map [kp-divide]	'function-key-error)
;; (define-key global-map [kp-0]		'function-key-error)
;; (define-key global-map [kp-1]		'function-key-error)
;; (define-key global-map [kp-2]		'function-key-error)
;; (define-key global-map [kp-3]		'function-key-error)
;; (define-key global-map [kp-4]		'function-key-error)
;; (define-key global-map [kp-5]		'recenter)
;; (define-key global-map [kp-6]		'function-key-error)
;; (define-key global-map [kp-7]		'function-key-error)
;; (define-key global-map [kp-8]		'function-key-error)
;; (define-key global-map [kp-9]		'function-key-error)
;; (define-key global-map [kp-equal]	'function-key-error)

;; X11R6 distinguishes these keys from the non-kp keys.
;; Make them behave like the non-kp keys unless otherwise bound.
(define-key function-key-map [kp-home] [home])
(define-key function-key-map [kp-left] [left])
(define-key function-key-map [kp-up] [up])
(define-key function-key-map [kp-right] [right])
(define-key function-key-map [kp-down] [down])
(define-key function-key-map [kp-prior] [prior])
(define-key function-key-map [kp-next] [next])
(define-key function-key-map [M-kp-next] [M-next])
(define-key function-key-map [kp-end] [end])
(define-key function-key-map [kp-begin] [begin])
(define-key function-key-map [kp-insert] [insert])
(define-key function-key-map [backspace] [?\C-?])
(define-key function-key-map [delete] [?\C-?])
(define-key function-key-map [kp-delete] [?\C-?])
(define-key function-key-map [S-kp-end] [S-end])
(define-key function-key-map [S-kp-down] [S-down])
(define-key function-key-map [S-kp-next] [S-next])
(define-key function-key-map [S-kp-left] [S-left])
(define-key function-key-map [S-kp-right] [S-right])
(define-key function-key-map [S-kp-home] [S-home])
(define-key function-key-map [S-kp-up] [S-up])
(define-key function-key-map [S-kp-prior] [S-prior])
(define-key function-key-map [C-S-kp-end] [C-S-end])
(define-key function-key-map [C-S-kp-down] [C-S-down])
(define-key function-key-map [C-S-kp-next] [C-S-next])
(define-key function-key-map [C-S-kp-left] [C-S-left])
(define-key function-key-map [C-S-kp-right] [C-S-right])
(define-key function-key-map [C-S-kp-home] [C-S-home])
(define-key function-key-map [C-S-kp-up] [C-S-up])
(define-key function-key-map [C-S-kp-prior] [C-S-prior])
;; Don't bind shifted keypad numeric keys, they reportedly
;; interfere with the feature of some keyboards to produce
;; numbers when NumLock is off.
;(define-key function-key-map [S-kp-1] [S-end])
;(define-key function-key-map [S-kp-2] [S-down])
;(define-key function-key-map [S-kp-3] [S-next])
;(define-key function-key-map [S-kp-4] [S-left])
;(define-key function-key-map [S-kp-6] [S-right])
;(define-key function-key-map [S-kp-7] [S-home])
;(define-key function-key-map [S-kp-8] [S-up])
;(define-key function-key-map [S-kp-9] [S-prior])
(define-key function-key-map [C-S-kp-1] [C-S-end])
(define-key function-key-map [C-S-kp-2] [C-S-down])
(define-key function-key-map [C-S-kp-3] [C-S-next])
(define-key function-key-map [C-S-kp-4] [C-S-left])
(define-key function-key-map [C-S-kp-6] [C-S-right])
(define-key function-key-map [C-S-kp-7] [C-S-home])
(define-key function-key-map [C-S-kp-8] [C-S-up])
(define-key function-key-map [C-S-kp-9] [C-S-prior])

(define-key global-map [mouse-movement] 'ignore)

(define-key global-map "\C-t" 'transpose-chars)
(define-key esc-map "t" 'transpose-words)
(define-key esc-map "\C-t" 'transpose-sexps)
(define-key ctl-x-map "\C-t" 'transpose-lines)

(define-key esc-map ";" 'comment-dwim)
(define-key esc-map "j" 'indent-new-comment-line)
(define-key esc-map "\C-j" 'indent-new-comment-line)
(define-key ctl-x-map ";" 'comment-set-column)
(define-key ctl-x-map "f" 'set-fill-column)
(define-key ctl-x-map "$" 'set-selective-display)

(define-key esc-map "@" 'mark-word)
(define-key esc-map "f" 'forward-word)
(define-key esc-map "b" 'backward-word)
(define-key esc-map "d" 'kill-word)
(define-key esc-map "\177" 'backward-kill-word)

(define-key esc-map "<" 'beginning-of-buffer)
(define-key esc-map ">" 'end-of-buffer)
(define-key ctl-x-map "h" 'mark-whole-buffer)
(define-key esc-map "\\" 'delete-horizontal-space)

(defalias 'mode-specific-command-prefix (make-sparse-keymap))
(defvar mode-specific-map (symbol-function 'mode-specific-command-prefix)
  "Keymap for characters following C-c.")
(define-key global-map "\C-c" 'mode-specific-command-prefix)

(global-set-key [M-right]  'forward-word)
(define-key esc-map [right] 'forward-word)
(global-set-key [M-left]   'backward-word)
(define-key esc-map [left] 'backward-word)
;; ilya@math.ohio-state.edu says these bindings are standard on PC editors.
(global-set-key [C-right]  'forward-word)
(global-set-key [C-left]   'backward-word)
;; This is not quite compatible, but at least is analogous
(global-set-key [C-delete] 'backward-kill-word)
(global-set-key [C-backspace] 'kill-word)
;; This is "move to the clipboard", or as close as we come.
(global-set-key [S-delete] 'kill-region)

(global-set-key [C-M-left]    'backward-sexp)
(define-key esc-map [C-left]  'backward-sexp)
(global-set-key [C-M-right]   'forward-sexp)
(define-key esc-map [C-right] 'forward-sexp)
(global-set-key [C-M-up]      'backward-up-list)
(define-key esc-map [C-up]    'backward-up-list)
(global-set-key [C-M-down]    'down-list)
(define-key esc-map [C-down]  'down-list)
(global-set-key [C-M-home]    'beginning-of-defun)
(define-key esc-map [C-home]  'beginning-of-defun)
(global-set-key [C-M-end]     'end-of-defun)
(define-key esc-map [C-end]   'end-of-defun)

(define-key esc-map "\C-f" 'forward-sexp)
(define-key esc-map "\C-b" 'backward-sexp)
(define-key esc-map "\C-u" 'backward-up-list)
(define-key esc-map "\C-@" 'mark-sexp)
(define-key esc-map [?\C-\ ] 'mark-sexp)
(define-key esc-map "\C-d" 'down-list)
(define-key esc-map "\C-k" 'kill-sexp)
;;; These are dangerous in various situations,
;;; so let's not encourage anyone to use them.
;;;(define-key global-map [C-M-delete] 'backward-kill-sexp)
;;;(define-key global-map [C-M-backspace] 'backward-kill-sexp)
(define-key esc-map [C-delete] 'backward-kill-sexp)
(define-key esc-map [C-backspace] 'backward-kill-sexp)
(define-key esc-map "\C-n" 'forward-list)
(define-key esc-map "\C-p" 'backward-list)
(define-key esc-map "\C-a" 'beginning-of-defun)
(define-key esc-map "\C-e" 'end-of-defun)
(define-key esc-map "\C-h" 'mark-defun)
(define-key ctl-x-map "nd" 'narrow-to-defun)
(define-key esc-map "(" 'insert-parentheses)
(define-key esc-map ")" 'move-past-close-and-reindent)

(define-key ctl-x-map "\C-e" 'eval-last-sexp)

(define-key ctl-x-map "m" 'compose-mail)
(define-key ctl-x-4-map "m" 'compose-mail-other-window)
(define-key ctl-x-5-map "m" 'compose-mail-other-frame)

(define-key ctl-x-map "r\C-@" 'point-to-register)
(define-key ctl-x-map [?r ?\C-\ ] 'point-to-register)
(define-key ctl-x-map "r " 'point-to-register)
(define-key ctl-x-map "rj" 'jump-to-register)
(define-key ctl-x-map "rs" 'copy-to-register)
(define-key ctl-x-map "rx" 'copy-to-register)
(define-key ctl-x-map "ri" 'insert-register)
(define-key ctl-x-map "rg" 'insert-register)
(define-key ctl-x-map "rr" 'copy-rectangle-to-register)
(define-key ctl-x-map "rn" 'number-to-register)
(define-key ctl-x-map "r+" 'increment-register)
(define-key ctl-x-map "rc" 'clear-rectangle)
(define-key ctl-x-map "rk" 'kill-rectangle)
(define-key ctl-x-map "rd" 'delete-rectangle)
(define-key ctl-x-map "ry" 'yank-rectangle)
(define-key ctl-x-map "ro" 'open-rectangle)
(define-key ctl-x-map "rt" 'string-rectangle)
(define-key ctl-x-map "rw" 'window-configuration-to-register)
(define-key ctl-x-map "rf" 'frame-configuration-to-register)

(define-key esc-map "q" 'fill-paragraph)
(define-key ctl-x-map "." 'set-fill-prefix)

(define-key esc-map "{" 'backward-paragraph)
(define-key esc-map "}" 'forward-paragraph)
(define-key esc-map "h" 'mark-paragraph)
(define-key esc-map "a" 'backward-sentence)
(define-key esc-map "e" 'forward-sentence)
(define-key esc-map "k" 'kill-sentence)
(define-key ctl-x-map "\177" 'backward-kill-sentence)

(define-key ctl-x-map "[" 'backward-page)
(define-key ctl-x-map "]" 'forward-page)
(define-key ctl-x-map "\C-p" 'mark-page)
(define-key ctl-x-map "l" 'count-lines-page)
(define-key ctl-x-map "np" 'narrow-to-page)
;; (define-key ctl-x-map "p" 'narrow-to-page)

(define-key ctl-x-map "al" 'add-mode-abbrev)
(define-key ctl-x-map "a\C-a" 'add-mode-abbrev)
(define-key ctl-x-map "ag" 'add-global-abbrev)
(define-key ctl-x-map "a+" 'add-mode-abbrev)
(define-key ctl-x-map "aig" 'inverse-add-global-abbrev)
(define-key ctl-x-map "ail" 'inverse-add-mode-abbrev)
;; (define-key ctl-x-map "a\C-h" 'inverse-add-global-abbrev)
(define-key ctl-x-map "a-" 'inverse-add-global-abbrev)
(define-key ctl-x-map "ae" 'expand-abbrev)
(define-key ctl-x-map "a'" 'expand-abbrev)
;; (define-key ctl-x-map "\C-a" 'add-mode-abbrev)
;; (define-key ctl-x-map "\+" 'add-global-abbrev)
;; (define-key ctl-x-map "\C-h" 'inverse-add-mode-abbrev)
;; (define-key ctl-x-map "\-" 'inverse-add-global-abbrev)
(define-key esc-map "'" 'abbrev-prefix-mark)
(define-key ctl-x-map "'" 'expand-abbrev)

(define-key ctl-x-map "z" 'repeat)

(define-key esc-map "\C-l" 'reposition-window)

(define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)
(define-key ctl-x-4-map "c" 'clone-indirect-buffer-other-window)

;; Signal handlers
(define-key special-event-map [sigusr1] 'ignore)
(define-key special-event-map [sigusr2] 'ignore)

;; Don't look for autoload cookies in this file.
;; Local Variables:
;; no-update-autoloads: t
;; End:

;; arch-tag: 23b5c7e6-e47b-49ed-8c6c-ed213c5fffe0
;;; bindings.el ends here
