;;; bindings.el --- define standard key bindings and some variables

;; Copyright (C) 1985-1987, 1992-1996, 1999-2018 Free Software
;; Foundation, Inc.

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

;;; Code:

(defun make-mode-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))


(defun mode-line-toggle-read-only (event)
  "Like toggling `read-only-mode', for the mode-line."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (read-only-mode 'toggle)))

(defun mode-line-toggle-modified (event)
  "Toggle the buffer-modified flag from the mode-line."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (set-buffer-modified-p (not (buffer-modified-p)))
    (force-mode-line-update)))

(defun mode-line-widen (event)
  "Widen a buffer from the mode-line."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (widen)
    (force-mode-line-update)))

(defvar mode-line-input-method-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-2]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (toggle-input-method)
	  (force-mode-line-update))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (describe-current-input-method))))
    (purecopy map)))

(defvar mode-line-coding-system-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (when (and enable-multibyte-characters
		     buffer-file-coding-system)
	    (describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (call-interactively 'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for the coding-system part of the mode line.")

(defun mode-line-change-eol (event)
  "Cycle through the various possible kinds of end-of-line styles."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (let ((eol (coding-system-eol-type buffer-file-coding-system)))
      (set-buffer-file-coding-system
       (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))

(defvar mode-line-eol-desc-cache nil)

(defun mode-line-eol-desc ()
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
	 (mnemonic (coding-system-eol-type-mnemonic buffer-file-coding-system))
	 (desc (assoc eol mode-line-eol-desc-cache)))
    (if (and desc (eq (cadr desc) mnemonic))
	(cddr desc)
      (if desc (setq mode-line-eol-desc-cache nil)) ;Flush the cache if stale.
      (setq desc
	    (propertize
	     mnemonic
	     'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
				(if (eq eol 0) "Unix-style LF"
				  (if (eq eol 1) "DOS-style CRLF"
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


;;; Mode line contents

(defun mode-line-default-help-echo (window)
  "Return default help echo text for WINDOW's mode line."
  (let* ((frame (window-frame window))
         (line-1a
          ;; Show text to select window only if the window is not
          ;; selected.
          (not (eq window (frame-selected-window frame))))
         (line-1b
          ;; Show text to drag mode line if either the window is not
          ;; at the bottom of its frame or the minibuffer window of
          ;; this frame can be resized.  This matches a corresponding
          ;; check in `mouse-drag-mode-line'.
          (or (not (window-at-side-p window 'bottom))
              (let ((mini-window (minibuffer-window frame)))
                (and (eq frame (window-frame mini-window))
                     (or (minibuffer-window-active-p mini-window)
                         (not resize-mini-windows))))))
         (line-2
          ;; Show text make window occupy the whole frame
          ;; only if it doesn't already do that.
          (not (eq window (frame-root-window frame))))
         (line-3
          ;; Show text to delete window only if that's possible.
          (not (eq window (frame-root-window frame)))))
    (when (or line-1a line-1b line-2 line-3)
      (concat
       (when (or line-1a line-1b)
         (concat
          "mouse-1: "
          (when line-1a "Select window")
          (when line-1b
            (if line-1a " (drag to resize)" "Drag to resize"))
          (when (or line-2 line-3) "\n")))
       (when line-2
         (concat
          "mouse-2: Make window occupy whole frame"
          (when line-3 "\n")))
       (when line-3
         "mouse-3: Remove window from frame")))))

(defcustom mode-line-default-help-echo #'mode-line-default-help-echo
  "Default help text for the mode line.
If the value is a string, it specifies the tooltip or echo area
message to display when the mouse is moved over the mode line.
If the value is a function, call that function with one argument
- the window whose mode line to display.  If the text at the
mouse position has a `help-echo' text property, that overrides
this variable."
  :type '(choice
          (const :tag "No help" :value nil)
          function
          (string :value "mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display"))
  :version "27.1"
  :group 'mode-line)

(defvar mode-line-front-space '(:eval (if (display-graphic-p) " " "-"))
  "Mode line construct to put at the front of the mode line.
By default, this construct is displayed right at the beginning of
the mode line, except that if there is a memory-full message, it
is displayed first.")
(put 'mode-line-front-space 'risky-local-variable t)

(defun mode-line-mule-info-help-echo (window _object _point)
  "Return help text specifying WINDOW's buffer coding system."
  (with-current-buffer (window-buffer window)
    (if buffer-file-coding-system
	(format "Buffer coding system (%s): %s
mouse-1: Describe coding system
mouse-3: Set coding system"
		(if enable-multibyte-characters "multi-byte" "unibyte")
		(symbol-name buffer-file-coding-system))
      "Buffer coding system: none specified")))

(defvar mode-line-mule-info
  `(""
    (current-input-method
     (:propertize ("" current-input-method-title)
		  help-echo (concat
			     ,(purecopy "Current input method: ")
			     current-input-method
			     ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
		  local-map ,mode-line-input-method-map
		  mouse-face mode-line-highlight))
    ,(propertize
      "%z"
      'help-echo 'mode-line-mule-info-help-echo
      'mouse-face 'mode-line-highlight
      'local-map mode-line-coding-system-map)
    (:eval (mode-line-eol-desc)))
  "Mode line construct to report the multilingual environment.
Normally it displays current input method (if any activated) and
mnemonics of the following coding systems:
  coding system for saving or writing the current buffer
  coding system for keyboard input (on a text terminal)
  coding system for terminal output (on a text terminal)")
;;;###autoload
(put 'mode-line-mule-info 'risky-local-variable t)
(make-variable-buffer-local 'mode-line-mule-info)

(defvar mode-line-client
  `(""
    (:propertize ("" (:eval (if (frame-parameter nil 'client) "@" "")))
		 help-echo ,(purecopy "emacsclient frame")))
  "Mode line construct for identifying emacsclient frames.")
;;;###autoload
(put 'mode-line-client 'risky-local-variable t)

(defun mode-line-read-only-help-echo (window _object _point)
  "Return help text specifying WINDOW's buffer read-only status."
  (format "Buffer is %s\nmouse-1: Toggle"
	  (if (buffer-local-value 'buffer-read-only (window-buffer window))
	      "read-only"
	    "writable")))

(defun mode-line-modified-help-echo (window _object _point)
  "Return help text specifying WINDOW's buffer modification status."
  (format "Buffer is %smodified\nmouse-1: Toggle modification state"
	  (if (buffer-modified-p (window-buffer window)) "" "not ")))

(defvar mode-line-modified
  (list (propertize
	 "%1*"
	 'help-echo 'mode-line-read-only-help-echo
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-1
			       #'mode-line-toggle-read-only))
	 'mouse-face 'mode-line-highlight)
	(propertize
	 "%1+"
	 'help-echo 'mode-line-modified-help-echo
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-1 #'mode-line-toggle-modified))
	 'mouse-face 'mode-line-highlight))
  "Mode line construct for displaying whether current buffer is modified.")
;;;###autoload
(put 'mode-line-modified 'risky-local-variable t)
(make-variable-buffer-local 'mode-line-modified)

(defvar mode-line-remote
  (list (propertize
	 "%1@"
	 'mouse-face 'mode-line-highlight
	 'help-echo (purecopy (lambda (window _object _point)
 				(format "%s"
					(with-selected-window window
					  (if (stringp default-directory)
					      (concat
					       (if (file-remote-p default-directory)
						   "Current directory is remote: "
						 "Current directory is local: ")
					       default-directory)
					    "Current directory is nil")))))))
  "Mode line construct to indicate a remote buffer.")
;;;###autoload
(put 'mode-line-remote 'risky-local-variable t)
(make-variable-buffer-local 'mode-line-remote)

;; MSDOS frames have window-system, but want the Fn identification.
(defun mode-line-frame-control ()
  "Compute mode line construct for frame identification.
Value is used for `mode-line-frame-identification', which see."
  (if (or (null window-system)
	  (eq window-system 'pc))
      "-%F  "
    "  "))

;; We need to defer the call to mode-line-frame-control to the time
;; the mode line is actually displayed.
(defvar mode-line-frame-identification '(:eval (mode-line-frame-control))
  "Mode line construct to describe the current frame.")
;;;###autoload
(put 'mode-line-frame-identification 'risky-local-variable t)

(defvar mode-line-process nil
  "Mode line construct for displaying info on process status.
Normally nil in most modes, since there is no process to display.")
;;;###autoload
(put 'mode-line-process 'risky-local-variable t)
(make-variable-buffer-local 'mode-line-process)

(defun bindings--define-key (map key item)
  "Make as much as possible of the menus pure."
  (declare (indent 2))
  (define-key map key
    (cond
     ((not (consp item)) item)     ;Not sure that could be other than a symbol.
     ;; Keymaps can't be made pure otherwise users can't remove/add elements
     ;; from/to them any more.
     ((keymapp item) item)
     ((stringp (car item))
      (if (keymapp (cdr item))
          (cons (purecopy (car item)) (cdr item))
        (purecopy item)))
     ((eq 'menu-item (car item))
      (if (keymapp (nth 2 item))
          `(menu-item ,(purecopy (nth 1 item)) ,(nth 2 item)
                      ,@(purecopy (nthcdr 3 item)))
        (purecopy item)))
     (t (message "non-menu-item: %S" item) item))))

(defvar mode-line-mode-menu (make-sparse-keymap "Minor Modes") "\
Menu of mode operations in the mode line.")

(defvar mode-line-major-mode-keymap
  (let ((map (make-sparse-keymap)))
    (bindings--define-key map [mode-line down-mouse-1]
      `(menu-item "Menu Bar" ignore
        :filter ,(lambda (_) (mouse-menu-major-mode-map))))
    (define-key map [mode-line mouse-2] 'describe-mode)
    (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
    map) "\
Keymap to display on major mode.")

(defvar mode-line-minor-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'mouse-minor-mode-menu)
    (define-key map [mode-line mouse-2] 'mode-line-minor-mode-help)
    (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
    (define-key map [header-line down-mouse-3] mode-line-mode-menu)
    map) "\
Keymap to display on minor modes.")

(defvar mode-line-modes
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	  "("
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
	  (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		      'mouse-face 'mode-line-highlight
		      'local-map (make-mode-line-mouse-map
				  'mouse-2 #'mode-line-widen))
	  ")"
	  (propertize "%]" 'help-echo recursive-edit-help-echo)
	  " "))
  "Mode line construct for displaying major and minor modes.")
(put 'mode-line-modes 'risky-local-variable t)

(defvar mode-line-column-line-number-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Toggle Line and Column Number Display")))
    (bindings--define-key menu-map [size-indication-mode]
      '(menu-item "Display Size Indication" size-indication-mode
		  :help "Toggle displaying a size indication in the mode-line"
		  :button (:toggle . size-indication-mode)))
    (bindings--define-key menu-map [line-number-mode]
      '(menu-item "Display Line Numbers" line-number-mode
		  :help "Toggle displaying line numbers in the mode-line"
		  :button (:toggle . line-number-mode)))
    (bindings--define-key menu-map [column-number-mode]
      '(menu-item "Display Column Numbers" column-number-mode
		  :help "Toggle displaying column numbers in the mode-line"
		  :button (:toggle . column-number-mode)))
    (define-key map [mode-line down-mouse-1] menu-map)
    map) "\
Keymap to display on column and line numbers.")

(defcustom column-number-indicator-zero-based t
  "When non-nil, mode line displays column numbers zero-based.

This variable has effect only when Column Number mode is turned on,
which displays column numbers in the mode line.
If the value is non-nil, the displayed column numbers start from
zero, otherwise they start from one."
  :type 'boolean
  :group 'mode-line
  :version "26.1")

(defcustom mode-line-percent-position '(-3 "%p")
  "Specification of \"percentage offset\" of window through buffer.
This option specifies both the field width and the type of offset
displayed in `mode-line-position', a component of the default
`mode-line-format'."
  :type `(radio
          (const :tag "nil:  No offset is displayed" nil)
          (const :tag "\"%o\": Proportion of \"travel\" of the window through the buffer"
                 (-3 "%o"))
          (const :tag "\"%p\": Percentage offset of top of window"
                 (-3 "%p"))
          (const :tag "\"%P\": Percentage offset of bottom of window"
                 (-3 "%P"))
          (const :tag "\"%q\": Offsets of both top and bottom of window"
                 (6 "%q")))
  :version "26.1"
  :group 'mode-line)
(put 'mode-line-percent-position 'risky-local-variable t)

(defvar mode-line-position
  `((:propertize
     mode-line-percent-position
     local-map ,mode-line-column-line-number-mode-map
     mouse-face mode-line-highlight
     ;; XXX needs better description
     help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")
    (size-indication-mode
     (8 ,(propertize
	  " of %I"
	  'local-map mode-line-column-line-number-mode-map
	  'mouse-face 'mode-line-highlight
	  ;; XXX needs better description
	  'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")))
    (line-number-mode
     ((column-number-mode
       (column-number-indicator-zero-based
        (10 ,(propertize
              " (%l,%c)"
              'local-map mode-line-column-line-number-mode-map
              'mouse-face 'mode-line-highlight
              'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu"))
        (10 ,(propertize
              " (%l,%C)"
              'local-map mode-line-column-line-number-mode-map
              'mouse-face 'mode-line-highlight
              'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")))
       (6 ,(propertize
	    " L%l"
	    'local-map mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu"))))
     ((column-number-mode
       (column-number-indicator-zero-based
        (5 ,(propertize
             " C%c"
             'local-map mode-line-column-line-number-mode-map
             'mouse-face 'mode-line-highlight
             'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu"))
        (5 ,(propertize
             " C%C"
             'local-map mode-line-column-line-number-mode-map
             'mouse-face 'mode-line-highlight
             'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu")))))))
  "Mode line construct for displaying the position in the buffer.
Normally displays the buffer percentage and, optionally, the
buffer size, the line number and the column number.")
(put 'mode-line-position 'risky-local-variable t)

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
		    (purecopy "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer")
		    'mouse-face 'mode-line-highlight
		    'local-map mode-line-buffer-identification-keymap)))

(defvar mode-line-buffer-identification
  (propertized-buffer-identification "%12b")
  "Mode line construct for identifying the buffer being displayed.
Its default value is (\"%12b\") with some text properties added.
Major modes that edit things other than ordinary files may change this
\(e.g. Info, Dired,...)")
;;;###autoload
(put 'mode-line-buffer-identification 'risky-local-variable t)
(make-variable-buffer-local 'mode-line-buffer-identification)

(defvar mode-line-misc-info
  '((global-mode-string ("" global-mode-string " ")))
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'.")
(put 'mode-line-misc-info 'risky-local-variable t)

(defvar mode-line-end-spaces '(:eval (unless (display-graphic-p) "-%-"))
  "Mode line construct to put at the end of the mode line.")
(put 'mode-line-end-spaces 'risky-local-variable t)

;; Default value of the top-level `mode-line-format' variable:
(let ((standard-mode-line-format
       (list "%e"
	     'mode-line-front-space
	     'mode-line-mule-info
	     'mode-line-client
	     'mode-line-modified
	     'mode-line-remote
	     'mode-line-frame-identification
	     'mode-line-buffer-identification
	     "   "
	     'mode-line-position
	     '(vc-mode vc-mode)
	     "  "
	     'mode-line-modes
	     'mode-line-misc-info
	     'mode-line-end-spaces)))
  (setq-default mode-line-format standard-mode-line-format)
  (put 'mode-line-format 'standard-value
       (list `(quote ,standard-mode-line-format))))


(defun mode-line-unbury-buffer (event) "\
Call `unbury-buffer' in this window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (unbury-buffer)))

(defun mode-line-bury-buffer (event) "\
Like `bury-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (bury-buffer)))

(defun mode-line-other-buffer () "\
Switch to the most recently selected buffer other than the current one."
  (interactive)
  (switch-to-buffer (other-buffer) nil t))

(defun mode-line-next-buffer (event)
  "Like `next-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (next-buffer)))

(defun mode-line-previous-buffer (event)
  "Like `previous-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (previous-buffer)))

(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

;; Use mode-line-mode-menu for local minor-modes only.
;; Global ones can go on the menubar (Options --> Show/Hide).
(bindings--define-key mode-line-mode-menu [overwrite-mode]
  '(menu-item "Overwrite (Ovwrt)" overwrite-mode
	      :help "Overwrite mode: typed characters replace existing text"
	      :button (:toggle . overwrite-mode)))
(bindings--define-key mode-line-mode-menu [outline-minor-mode]
  '(menu-item "Outline (Outl)" outline-minor-mode
	      ;; XXX: This needs a good, brief description.
	      :help ""
	      :button (:toggle . (bound-and-true-p outline-minor-mode))))
(bindings--define-key mode-line-mode-menu [highlight-changes-mode]
  '(menu-item "Highlight changes (Chg)" highlight-changes-mode
	      :help "Show changes in the buffer in a distinctive color"
	      :button (:toggle . (bound-and-true-p highlight-changes-mode))))
(bindings--define-key mode-line-mode-menu [hide-ifdef-mode]
  '(menu-item "Hide ifdef (Ifdef)" hide-ifdef-mode
	      :help "Show/Hide code within #ifdef constructs"
	      :button (:toggle . (bound-and-true-p hide-ifdef-mode))))
(bindings--define-key mode-line-mode-menu [glasses-mode]
  '(menu-item "Glasses (o^o)" glasses-mode
	      :help "Insert virtual separators to make long identifiers easy to read"
	      :button (:toggle . (bound-and-true-p glasses-mode))))
(bindings--define-key mode-line-mode-menu [font-lock-mode]
  '(menu-item "Font Lock" font-lock-mode
	      :help "Syntax coloring"
	      :button (:toggle . font-lock-mode)))
(bindings--define-key mode-line-mode-menu [flyspell-mode]
  '(menu-item "Flyspell (Fly)" flyspell-mode
	      :help "Spell checking on the fly"
	      :button (:toggle . (bound-and-true-p flyspell-mode))))
(bindings--define-key mode-line-mode-menu [auto-revert-tail-mode]
  '(menu-item "Auto revert tail (Tail)" auto-revert-tail-mode
	      :help "Revert the tail of the buffer when the file on disk grows"
	      :enable (buffer-file-name)
	      :button (:toggle . (bound-and-true-p auto-revert-tail-mode))))
(bindings--define-key mode-line-mode-menu [auto-revert-mode]
  '(menu-item "Auto revert (ARev)" auto-revert-mode
	      :help "Revert the buffer when the file on disk changes"
	      :button (:toggle . (bound-and-true-p auto-revert-mode))))
(bindings--define-key mode-line-mode-menu [auto-fill-mode]
  '(menu-item "Auto fill (Fill)" auto-fill-mode
	      :help "Automatically insert new lines"
	      :button (:toggle . auto-fill-function)))
(bindings--define-key mode-line-mode-menu [abbrev-mode]
  '(menu-item "Abbrev (Abbrev)" abbrev-mode
	      :help "Automatically expand abbreviations"
	      :button (:toggle . abbrev-mode)))

(defun mode-line-minor-mode-help (event)
  "Describe minor mode for EVENT on minor modes area of the mode line."
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (describe-minor-mode-from-indicator indicator)))

(defvar minor-mode-alist nil "\
Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line if VARIABLE's value is non-nil.

Actually, STRING need not be a string; any mode-line construct is
okay.  See `mode-line-format'.")
;;;###autoload
(put 'minor-mode-alist 'risky-local-variable t)
;; Don't use purecopy here--some people want to change these strings.
(setq minor-mode-alist
      '((abbrev-mode " Abbrev")
        (overwrite-mode overwrite-mode)
        (auto-fill-function " Fill")
        ;; not really a minor mode...
        (defining-kbd-macro " Def")))

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      (append
       (cond ((memq system-type '(ms-dos windows-nt))
	      (mapcar 'purecopy
	      '(".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk"
		".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386")))
	     (t
	      (mapcar 'purecopy
	      '(".o" "~" ".bin" ".lbin" ".so"
		".a" ".ln" ".blg" ".bbl"))))
       (mapcar 'purecopy
       '(".elc" ".lof"
	 ".glo" ".idx" ".lot"
	 ;; VCS metadata directories
	 ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/"
	 ;; TeX-related
	 ".fmt" ".tfm"
	 ;; Java compiled
	 ".class"
	 ;; CLISP
	 ".fas" ".lib" ".mem"
	 ;; CMUCL
	 ".x86f" ".sparcf"
	 ;; OpenMCL / Clozure CL
	 ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl"
	 ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl"
	 ".sx32fsl" ".wx64fsl" ".wx32fsl"
         ;; Other CL implementations (Allegro, LispWorks)
         ".fasl" ".ufsl" ".fsl" ".dxl"
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
	 ".pyc" ".pyo"))))

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
      ;; FIXME: Maybe beginning-of-line, beginning-of-buffer, end-of-line,
      ;; end-of-buffer, end-of-file, buffer-read-only, and
      ;; file-supersession should all be user-errors!
      `(beginning-of-line beginning-of-buffer end-of-line
	end-of-buffer end-of-file buffer-read-only
	file-supersession mark-inactive
        user-error ;; That's the main one!
        ))

(make-variable-buffer-local 'indent-tabs-mode)

;; These per-buffer variables are never reset by
;; `kill-all-local-variables', because they have no default value.
;; For consistency, we give them the `permanent-local' property, even
;; though `kill-all-local-variables' does not actually consult it.
;; See init_buffer_once in buffer.c for the origins of this list.

(mapc (lambda (sym) (put sym 'permanent-local t))
      '(buffer-file-name default-directory buffer-backed-up
	buffer-saved-size buffer-auto-save-file-name
	buffer-read-only buffer-undo-list mark-active
	point-before-scroll buffer-file-truename
	buffer-file-format buffer-auto-save-file-format
	buffer-display-count buffer-display-time
	enable-multibyte-characters
	buffer-file-coding-system truncate-lines))

;; We have base64, md5 and sha1 functions built in now.
(provide 'base64)
(provide 'md5)
(provide 'sha1)
(provide 'overlay '(display syntax-table field))
(provide 'text-properties '(display syntax-table field point-entered))

(define-key esc-map "\t" 'complete-symbol)

(defun complete-symbol (arg)
  "Perform completion on the text around point.
The completion method is determined by `completion-at-point-functions'.

With a prefix argument, this command does completion within
the collection of symbols listed in the index of the manual for the
language you are using."
  (interactive "P")
  (if arg (info-complete-symbol) (completion-at-point)))

;; Reduce total amount of space we must allocate during this function
;; that we will not need to keep permanently.
(garbage-collect)


(setq help-event-list '(help f1 ?\?))

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

;; Moving with arrows in bidi-sensitive direction.
(defcustom visual-order-cursor-movement nil
  "If non-nil, moving cursor with arrow keys follows the visual order.

When this is non-nil, \\[right-char] will move to the character that is
to the right of point on display, and \\[left-char] will move to the left,
disregarding the surrounding bidirectional context.  Depending on the
bidirectional context of the surrounding characters, this can move point
many buffer positions away.

When the text is entirely left-to-right, logical-order and visual-order
cursor movements produce identical results."
  :type '(choice (const :tag "Logical-order cursor movement" nil)
		 (const :tag "Visual-order cursor movement" t))
  :group 'display
  :version "24.4")

(defun right-char (&optional n)
  "Move point N characters to the right (to the left if N is negative).
On reaching beginning or end of buffer, stop and signal error.

If `visual-order-cursor-movement' is non-nil, this always moves
to the right on display, wherever that is in the buffer.
Otherwise, depending on the bidirectional context, this may move
one position either forward or backward in the buffer.  This is
in contrast with \\[forward-char] and \\[backward-char], which
see."
  (interactive "^p")
  (if visual-order-cursor-movement
      (dotimes (i (if (numberp n) (abs n) 1))
	(move-point-visually (if (and (numberp n) (< n 0)) -1 1)))
    (if (eq (current-bidi-paragraph-direction) 'left-to-right)
	(forward-char n)
      (backward-char n))))

(defun left-char ( &optional n)
  "Move point N characters to the left (to the right if N is negative).
On reaching beginning or end of buffer, stop and signal error.

If `visual-order-cursor-movement' is non-nil, this always moves
to the left on display, wherever that is in the buffer.
Otherwise, depending on the bidirectional context, this may move
one position either backward or forward in the buffer.  This is
in contrast with \\[forward-char] and \\[backward-char], which
see."
  (interactive "^p")
  (if visual-order-cursor-movement
      (dotimes (i (if (numberp n) (abs n) 1))
	(move-point-visually (if (and (numberp n) (< n 0)) 1 -1)))
    (if (eq (current-bidi-paragraph-direction) 'left-to-right)
	(backward-char n)
      (forward-char n))))

(defun right-word (&optional n)
  "Move point N words to the right (to the left if N is negative).

Depending on the bidirectional context, this may move either forward
or backward in the buffer.  This is in contrast with \\[forward-word]
and \\[backward-word], which see.

Value is normally t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed
if `inhibit-field-text-motion' is non-nil."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (forward-word n)
    (backward-word n)))

(defun left-word (&optional n)
  "Move point N words to the left (to the right if N is negative).

Depending on the bidirectional context, this may move either backward
or forward in the buffer.  This is in contrast with \\[backward-word]
and \\[forward-word], which see.

Value is normally t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed
if `inhibit-field-text-motion' is non-nil."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (backward-word n)
    (forward-word n)))

(defvar narrow-map (make-sparse-keymap)
  "Keymap for narrowing commands.")
(define-key ctl-x-map "n" narrow-map)

(define-key narrow-map "n" 'narrow-to-region)
(define-key narrow-map "w" 'widen)

;; Quitting
(define-key global-map "\e\e\e" 'keyboard-escape-quit)
(define-key global-map "\C-g" 'keyboard-quit)

;; Used to be in termdev.el: when using several terminals, make C-z
;; suspend only the relevant terminal.
(substitute-key-definition 'suspend-emacs 'suspend-frame global-map)

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
(define-key esc-map "=" 'count-words-region)
(define-key ctl-x-map "=" 'what-cursor-position)
(define-key esc-map ":" 'eval-expression)
;; Define ESC ESC : like ESC : for people who type ESC ESC out of habit.
(define-key esc-map "\M-:" 'eval-expression)
;; Changed from C-x ESC so that function keys work following C-x.
(define-key ctl-x-map "\e\e" 'repeat-complex-command)
;; New binding analogous to M-:.
(define-key ctl-x-map "\M-:" 'repeat-complex-command)
(define-key ctl-x-map "u" 'undo)
(put 'undo :advertised-binding [?\C-x ?u])
;; Many people are used to typing C-/ on X terminals and getting C-_.
(define-key global-map [?\C-/] 'undo)
(define-key global-map "\C-_" 'undo)
;; Richard said that we should not use C-x <uppercase letter> and I have
;; no idea whereas to bind it.  Any suggestion welcome.  -stef
;; (define-key ctl-x-map "U" 'undo-only)

(define-key esc-map "!" 'shell-command)
(define-key esc-map "|" 'shell-command-on-region)
(define-key esc-map "&" 'async-shell-command)

(define-key ctl-x-map [right] 'next-buffer)
(define-key ctl-x-map [C-right] 'next-buffer)
(define-key global-map [XF86Forward] 'next-buffer)
(define-key ctl-x-map [left] 'previous-buffer)
(define-key ctl-x-map [C-left] 'previous-buffer)
(define-key global-map [XF86Back] 'previous-buffer)

(let ((map minibuffer-local-map))
  (define-key map "\en"   'next-history-element)
  (define-key map [next]  'next-history-element)
  (define-key map [down]  'next-line-or-history-element)
  (define-key map [XF86Forward] 'next-history-element)
  (define-key map "\ep"   'previous-history-element)
  (define-key map [prior] 'previous-history-element)
  (define-key map [up]    'previous-line-or-history-element)
  (define-key map [XF86Back] 'previous-history-element)
  (define-key map "\es"   'next-matching-history-element)
  (define-key map "\er"   'previous-matching-history-element)
  ;; Override the global binding (which calls indent-relative via
  ;; indent-for-tab-command).  The alignment that indent-relative tries to
  ;; do doesn't make much sense here since the prompt messes it up.
  (define-key map "\t"    'self-insert-command)
  (define-key map [C-tab] 'file-cache-minibuffer-complete))

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

;; Update tutorial--default-keys if you change these.
(define-key global-map "\177" 'delete-backward-char)
;; We explicitly want C-d to use `delete-char' instead of
;; `delete-forward-char' so that it ignores `delete-active-region':
;; Most C-d users are old-timers who don't expect
;; `delete-active-region' here, while newer users who expect
;; `delete-active-region' use C-d much less.
(define-key global-map "\C-d" 'delete-char)

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
(put 'set-mark-command :advertised-binding [?\C- ])

(define-key ctl-x-map "\C-x" 'exchange-point-and-mark)
(define-key ctl-x-map "\C-@" 'pop-global-mark)
(define-key ctl-x-map " " 'rectangle-mark-mode)
(define-key ctl-x-map [?\C- ] 'pop-global-mark)

(define-key global-map "\C-n" 'next-line)
(define-key global-map "\C-p" 'previous-line)
(define-key ctl-x-map "\C-n" 'set-goal-column)
(define-key global-map "\C-a" 'move-beginning-of-line)
(define-key global-map "\C-e" 'move-end-of-line)

(define-key ctl-x-map "`" 'next-error)

(defvar goto-map (make-sparse-keymap)
  "Keymap for navigation commands.")
(define-key esc-map "g" goto-map)

(define-key goto-map    "c" 'goto-char)
(define-key goto-map    "g" 'goto-line)
(define-key goto-map "\M-g" 'goto-line)
(define-key goto-map    "n" 'next-error)
(define-key goto-map "\M-n" 'next-error)
(define-key goto-map    "p" 'previous-error)
(define-key goto-map "\M-p" 'previous-error)
(define-key goto-map   "\t" 'move-to-column)

(defvar search-map (make-sparse-keymap)
  "Keymap for search related commands.")
(define-key esc-map "s" search-map)

(define-key search-map "o"    'occur)
(define-key search-map "\M-w" 'eww-search-words)
(define-key search-map "hr"   'highlight-regexp)
(define-key search-map "hp"   'highlight-phrase)
(define-key search-map "hl"   'highlight-lines-matching-regexp)
(define-key search-map "h."   'highlight-symbol-at-point)
(define-key search-map "hu"   'unhighlight-regexp)
(define-key search-map "hf"   'hi-lock-find-patterns)
(define-key search-map "hw"   'hi-lock-write-interactive-patterns)

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
(define-key global-map [left]		'left-char)
(define-key global-map [up]		'previous-line)
(define-key global-map [right]		'right-char)
(define-key global-map [down]		'next-line)
(define-key global-map [prior]		'scroll-down-command)
(define-key global-map [next]		'scroll-up-command)
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
(define-key global-map [deletechar]	'delete-forward-char)
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
;; FIXME: rather than list such mappings for every modifier-combination,
;;   we should come up with a way to do it generically, something like
;;   (define-key function-key-map [*-kp-home] [*-home])
;; Currently we add keypad key combinations with basic modifiers
;; (to complement plain bindings in "Keypad support" section in simple.el)
;; Until [*-kp-home] is implemented, for more modifiers we could also use:
;; (todo-powerset '(control meta shift hyper super alt))  (Bug#14397)
(let ((modifiers '(nil (control) (meta) (control meta) (shift)
		   (control shift) (meta shift) (control meta shift)))
      (keys '((kp-delete delete) (kp-insert insert)
	      (kp-end end) (kp-down down) (kp-next next)
	      (kp-left left) (kp-begin begin) (kp-right right)
	      (kp-home home) (kp-up up) (kp-prior prior)
	      (kp-enter enter) (kp-decimal ?.)
	      (kp-0 ?0) (kp-1 ?1) (kp-2 ?2) (kp-3 ?3) (kp-4 ?4)
	      (kp-5 ?5) (kp-6 ?6) (kp-7 ?7) (kp-8 ?8) (kp-9 ?9)
	      (kp-add ?+) (kp-subtract ?-) (kp-multiply ?*) (kp-divide ?/))))
  (dolist (pair keys)
    (let ((keypad (nth 0 pair))
	  (normal (nth 1 pair)))
      (when (characterp normal)
	(put keypad 'ascii-character normal))
      (dolist (mod modifiers)
	(define-key function-key-map
	  (vector (append mod (list keypad)))
	  (vector (append mod (list normal))))))))

(define-key function-key-map [backspace] [?\C-?])
(define-key function-key-map [delete] [?\C-?])
(define-key function-key-map [kp-delete] [?\C-?])

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
;(define-key function-key-map [C-S-kp-1] [C-S-end])
;(define-key function-key-map [C-S-kp-2] [C-S-down])
;(define-key function-key-map [C-S-kp-3] [C-S-next])
;(define-key function-key-map [C-S-kp-4] [C-S-left])
;(define-key function-key-map [C-S-kp-6] [C-S-right])
;(define-key function-key-map [C-S-kp-7] [C-S-home])
;(define-key function-key-map [C-S-kp-8] [C-S-up])
;(define-key function-key-map [C-S-kp-9] [C-S-prior])

;; Hitting C-SPC on text terminals, usually sends the ascii code 0 (aka C-@),
;; so we can't distinguish those two keys, but usually we consider C-SPC
;; (rather than C-@) as the "canonical" binding.
(define-key function-key-map [?\C-@] [?\C-\s])
;; Many keyboards don't have a `backtab' key, so by convention the user
;; can use S-tab instead to access that binding.
(define-key function-key-map [S-tab] [backtab])

(define-key global-map [mouse-movement] 'ignore)

(define-key global-map "\C-t" 'transpose-chars)
(define-key esc-map "t" 'transpose-words)
(define-key esc-map "\C-t" 'transpose-sexps)
(define-key ctl-x-map "\C-t" 'transpose-lines)

(define-key esc-map ";" 'comment-dwim)
(define-key esc-map "j" 'indent-new-comment-line)
(define-key esc-map "\C-j" 'indent-new-comment-line)
(define-key ctl-x-map ";" 'comment-set-column)
(define-key ctl-x-map [?\C-\;] 'comment-line)
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

(global-set-key [M-right]  'right-word)
(define-key esc-map [right] 'forward-word)
(global-set-key [M-left]   'left-word)
(define-key esc-map [left] 'backward-word)
;; ilya@math.ohio-state.edu says these bindings are standard on PC editors.
(global-set-key [C-right]  'right-word)
(global-set-key [C-left]   'left-word)
;; This is not quite compatible, but at least is analogous
(global-set-key [C-delete] 'kill-word)
(global-set-key [C-backspace] 'backward-kill-word)
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


(defvar ctl-x-r-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'clear-rectangle)
    (define-key map "k" 'kill-rectangle)
    (define-key map "d" 'delete-rectangle)
    (define-key map "y" 'yank-rectangle)
    (define-key map "o" 'open-rectangle)
    (define-key map "t" 'string-rectangle)
    (define-key map "N" 'rectangle-number-lines)
    (define-key map "\M-w" 'copy-rectangle-as-kill)
    (define-key map "\C-@" 'point-to-register)
    (define-key map [?\C-\ ] 'point-to-register)
    (define-key map " " 'point-to-register)
    (define-key map "j" 'jump-to-register)
    (define-key map "s" 'copy-to-register)
    (define-key map "x" 'copy-to-register)
    (define-key map "i" 'insert-register)
    (define-key map "g" 'insert-register)
    (define-key map "r" 'copy-rectangle-to-register)
    (define-key map "n" 'number-to-register)
    (define-key map "+" 'increment-register)
    (define-key map "w" 'window-configuration-to-register)
    (define-key map "f" 'frameset-to-register)
    map)
  "Keymap for subcommands of C-x r.")
(define-key ctl-x-map "r" ctl-x-r-map)

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

(defvar abbrev-map (make-sparse-keymap)
  "Keymap for abbrev commands.")
(define-key ctl-x-map "a" abbrev-map)

(define-key abbrev-map "l" 'add-mode-abbrev)
(define-key abbrev-map "\C-a" 'add-mode-abbrev)
(define-key abbrev-map "g" 'add-global-abbrev)
(define-key abbrev-map "+" 'add-mode-abbrev)
(define-key abbrev-map "ig" 'inverse-add-global-abbrev)
(define-key abbrev-map "il" 'inverse-add-mode-abbrev)
;; (define-key abbrev-map "\C-h" 'inverse-add-global-abbrev)
(define-key abbrev-map "-" 'inverse-add-global-abbrev)
(define-key abbrev-map "e" 'expand-abbrev)
(define-key abbrev-map "'" 'expand-abbrev)
;; (define-key ctl-x-map "\C-a" 'add-mode-abbrev)
;; (define-key ctl-x-map "+" 'add-global-abbrev)
;; (define-key ctl-x-map "\C-h" 'inverse-add-mode-abbrev)
;; (define-key ctl-x-map "-" 'inverse-add-global-abbrev)
(define-key esc-map "'" 'abbrev-prefix-mark)
(define-key ctl-x-map "'" 'expand-abbrev)
(define-key ctl-x-map "\C-b" 'list-buffers)

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

;;; bindings.el ends here
