;;; bindings.el --- define standard key bindings and some variables  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-2026 Free Software Foundation, Inc.

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

(declare-function tramp-revert-buffer-with-sudo "tramp-cmds")

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
    map))

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
    map)
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
the mode line, except that if there is a \"memory full\" message,
it is displayed first.")
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

(defvar-local mode-line-mule-info
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

(defvar mode-line-client
  `(:eval
    (if (frame-parameter nil 'client)
        ,(propertize "@" 'help-echo "emacsclient frame")))
  "Mode line construct for identifying emacsclient frames.")
;; Autoload if this file no longer dumped.
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

(defvar-local mode-line-modified
  (list (propertize
	 "%1*"
	 'help-echo 'mode-line-read-only-help-echo
         'local-map (make-mode-line-mouse-map
                     'mouse-1
                     #'mode-line-toggle-read-only)
	 'mouse-face 'mode-line-highlight)
	(propertize
	 "%1+"
	 'help-echo 'mode-line-modified-help-echo
         'local-map (make-mode-line-mouse-map
                     'mouse-1 #'mode-line-toggle-modified)
	 'mouse-face 'mode-line-highlight))
  "Mode line construct for displaying whether current buffer is modified.")
;;;###autoload
(put 'mode-line-modified 'risky-local-variable t)

(defvar-local mode-line-remote
  (list (propertize
	 "%1@"
	 'mouse-face 'mode-line-highlight
         'help-echo (lambda (window _object _point)
                      (format "%s"
                              (with-selected-window window
                                (if (stringp default-directory)
                                    (concat
                                     (if (file-remote-p default-directory)
                                         "Current directory is remote: "
                                       "Current directory is local: ")
                                     default-directory)
                                  "Current directory is nil"))))))
  "Mode line construct to indicate a remote buffer.")
;;;###autoload
(put 'mode-line-remote 'risky-local-variable t)

;; MSDOS frames have window-system, but want the Fn identification.
(defun mode-line-frame-control ()
  "Compute mode line construct for frame identification.
Value is used for `mode-line-frame-identification', which see."
  (if (or (null window-system)
	  (eq window-system 'pc))
      " %F  "
    "  "))

;; We need to defer the call to mode-line-frame-control to the time
;; the mode line is actually displayed.
(defvar mode-line-frame-identification '(:eval (mode-line-frame-control))
  "Mode line construct to describe the current frame.")
;;;###autoload
(put 'mode-line-frame-identification 'risky-local-variable t)

(defvar-keymap mode-line-window-dedicated-keymap
  :doc "Keymap for what is displayed by `mode-line-window-dedicated'."
  "<mode-line> <mouse-1>" #'toggle-window-dedicated)

(defun mode-line-window-control ()
  "Compute mode line construct for window dedicated state.
Value is used for `mode-line-window-dedicated', which see."
  (cond
   ((eq (window-dedicated-p) t)
    (propertize
     "D"
     'help-echo "Window strongly dedicated to its buffer\nmouse-1: Toggle"
     'local-map mode-line-window-dedicated-keymap
     'mouse-face 'mode-line-highlight))
   ((window-dedicated-p)
    (propertize
     "d"
     'help-echo "Window dedicated to its buffer\nmouse-1: Toggle"
     'local-map mode-line-window-dedicated-keymap
     'mouse-face 'mode-line-highlight))
   (t "")))

(defvar mode-line-window-dedicated '(:eval (mode-line-window-control))
  "Mode line construct to describe the current window.")
;;;###autoload
(put 'mode-line-window-dedicated 'risky-local-variable t)

(defvar-local mode-line-process nil
  "Mode line construct for displaying info on process status.
Normally nil in most modes, since there is no process to display.")
;;;###autoload
(put 'mode-line-process 'risky-local-variable t)

(defcustom mode-line-right-align-edge 'window
  "Where function `mode-line-format-right-align' should align to.
Internally, that function uses `:align-to' in a display property,
so aligns to the left edge of the given area.  See info node
`(elisp)Pixel Specification'.

Must be set to a symbol.  Acceptable values are:
- `window': align to extreme right of window, regardless of margins
  or fringes
- `right-fringe': align to right-fringe
- `right-margin': align to right-margin"
  :type '(choice (const right-margin)
                 (const right-fringe)
                 (const window))
  :group 'mode-line
  :version "30.1")

(defun mode--line-format-right-align ()
  "Right-align all following mode-line constructs.

When the symbol `mode-line-format-right-align' appears in
`mode-line-format', return a string of one space, with a display
property to make it appear long enough to align anything after
that symbol to the right of the rendered mode line.  Exactly how
far to the right is controlled by `mode-line-right-align-edge'.

It is important that the symbol `mode-line-format-right-align' be
included in `mode-line-format' (and not another similar construct
such as `(:eval (mode-line-format-right-align)').  This is because
the symbol `mode-line-format-right-align' is processed by
`format-mode-line' as a variable."
  (let* ((rest (cdr (memq 'mode-line-format-right-align
			  mode-line-format)))
	 (rest-str (format-mode-line `("" ,@rest)))
	 (rest-width (progn
                       (add-face-text-property
                        0 (length rest-str) 'mode-line t rest-str)
                       (string-pixel-width rest-str))))
    (propertize " " 'display
		;; The `right' spec doesn't work on TTY frames
		;; when windows are split horizontally (bug#59620)
		(if (and (display-graphic-p)
                         (not (eq mode-line-right-align-edge 'window)))
		    `(space :align-to (- ,mode-line-right-align-edge
                                         (,rest-width)))
		  `(space :align-to (,(- (window-pixel-width)
                                         (window-scroll-bar-width)
                                         (window-right-divider-width)
                                         (* (or (car (window-margins)) 0)
                                            (frame-char-width))
                                         (car (window-fringes))
                                         ;; Manually account for value of
                                         ;; `mode-line-right-align-edge' even
                                         ;; when display is non-graphical
                                         (pcase mode-line-right-align-edge
                                           ('right-margin
                                            (or (cdr (window-margins)) 0))
                                           ('right-fringe
                                            ;; what here?
                                            (or (cadr (window-fringes)) 0))
                                           (_ 0))
                                         rest-width)))))))

(defvar mode-line-format-right-align '(:eval (mode--line-format-right-align))
  "Mode line construct to right align all following constructs.")
;;;###autoload
(put 'mode-line-format-right-align 'risky-local-variable t)

(defvar mode-line-mode-menu (make-sparse-keymap "Minor Modes") "\
Menu of mode operations in the mode line.")

(defun bindings--menu-item-string (item)
  "Return the menu-item string for ITEM, or nil if not a menu-item."
  (pcase item
    (`(menu-item ,name . ,_) (eval name t))
    (`(,(and (pred stringp) name) . ,_) name)))

(defun bindings--sort-menu-keymap (map)
  "Sort the bindings in MAP in alphabetical order by menu-item string.
The order of bindings in a keymap matters only when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (let ((bindings nil)
        (prompt (keymap-prompt map)))
    (while (keymapp map)
      (setq map (map-keymap
                 (lambda (key item)
                   ;; FIXME: Handle char-ranges here?
                   (push (cons key item) bindings))
                 map)))
    ;; Sort the bindings and make a new keymap from them.
    (setq bindings
          (sort bindings
                (lambda (a b)
                  (string< (bindings--menu-item-string (cdr-safe a))
                           (bindings--menu-item-string (cdr-safe b))))))
    (nconc (make-sparse-keymap prompt) bindings)))

(defcustom mode-line-collapse-minor-modes nil
  "Minor modes for which mode line lighters are hidden.
Hidden lighters are collapsed into one, which latter is customizable
using the option `mode-line-collapse-minor-modes-to'.

The value could be a list (MODES ...) which means to collapse lighters
only for MODES, or a list (not MODES ...) which means to collapse all
lighters for minor modes not in MODES.  Other non-nil values make all
lighters hidden."
  :type '(choice (const :tag "No modes" nil)
                 (repeat :tag "Modes" symbol)
                 (cons :tag "All modes except"
                       (const not) (repeat symbol))
                 (const :tag "All modes" t))
  :group 'mode-line
  :version "31.1")

(defcustom mode-line-collapse-minor-modes-to
  (if (char-displayable-p ?…) " …" " ...")
  "Lighter for collapsed minor modes.
This is effective only when `mode-line-collapse-minor-modes' is non-nil."
  :type 'string
  :initialize #'custom-initialize-delay
  :group 'mode-line
  :version "31.1")

(defcustom mode-line-modes-delimiters '("(" . ")")
  "Strings placed around the modes displayed in the mode line.
These elements are placed around `mode-name' and `mode-line-modes'."
  :type '(choice (const :tag "No delimiters")
                 (cons (string :tag "Left delimiter")
                       (string :tag "Right delimiter")))
  :group 'mode-line
  :version "31.1")

(defvar mode-line-minor-modes '(:eval (mode-line--minor-modes))
  "Mode line construct for minor mode lighters.")
;;;###autoload
(put 'mode-line-minor-modes 'risky-local-variable t)

(defun mode-line--make-lighter-menu (alist)
  "Return a menu keymap for minor mode lighters in ALIST.
ALIST should be in the same format as `minor-mode-alist'.

Return nil if no lighters in ALIST should be visible, for example, there
are no active minor modes or non-empty lighters."
  (let ((menu (make-sparse-keymap "Minor Modes"))
        (empty t))
    (dolist (item alist)
      (when-let* ((variable (car item))
                  ((and (boundp variable)
                        (symbol-value variable)))
                  (lighter (format-mode-line `("" ,@(cdr-safe item))))
                  ((not (string= lighter "")))
                  (toggle (or (get variable :minor-mode-function) variable))
                  ;; Follow the format in `mouse-minor-mode-menu'
                  (name (format "%s - %s" lighter
                                (capitalize
                                 (string-replace
                                  "-" " " (symbol-name toggle))))))
        (when (eq ?  (aref name 0))
          (setq name (substring name 1)))
        (let* ((map (cdr-safe (assq variable minor-mode-map-alist)))
               (mm-menu (and (keymapp map)
                             (keymap-lookup map "<menu-bar>"))))
          (setq mm-menu
                (cond (mm-menu (mouse-menu-non-singleton mm-menu))
                      ((fboundp toggle)
                       (define-keymap :name name
                         "<help>" (list 'menu-item
                                        "Help for minor mode"
                                        (lambda () (interactive)
                                          (describe-function toggle)))
                         "<turn-off>" (list 'menu-item
                                            "Turn off minor mode"
                                            toggle)))
                      ;; No menu and not a minor mode function, so just
                      ;; display the label without a sub-menu.
                      (t nil)))
          (keymap-set menu (format "<%s>" toggle)
                      (list 'menu-item name mm-menu))
          (setq empty nil))))
    (and (not empty) menu)))

(defun mode-line--minor-modes ()
  "Compute mode line constructs for minor mode lighters."
  (let (visible hidden)
    (cond
     ((not mode-line-collapse-minor-modes)
      (setq visible minor-mode-alist
            hidden nil))
     ((eq 'not (car-safe mode-line-collapse-minor-modes))
      (let ((modes (cdr mode-line-collapse-minor-modes)))
        (dolist (item minor-mode-alist)
          (if (memq (car item) modes)
              (push item visible)
            (push item hidden)))
        (setq visible (nreverse visible)
              hidden (nreverse hidden))))
     ((listp mode-line-collapse-minor-modes)
      (let ((modes mode-line-collapse-minor-modes))
        (dolist (item minor-mode-alist)
          (if (memq (car item) modes)
              (push item hidden)
            (push item visible)))
        (setq visible (nreverse visible)
              hidden (nreverse hidden))))
     (t (setq visible nil
              hidden minor-mode-alist)))
    (list ""
          `(:propertize ("" ,visible)
                        mouse-face mode-line-highlight
                        help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                        local-map ,mode-line-minor-mode-keymap)
          (unless (string= "" (format-mode-line `("" ,hidden)))
            (let* ((menu
                    ;; FIXME: This is to defer the computation of the
                    ;; menu, but may not play well with touchscreen.
                    (lambda (e)
                      (interactive "@e")
                      (if-let* ((m (mode-line--make-lighter-menu hidden)))
                          (popup-menu m e)
                        (message "No menu available"))))
                   (keymap
                    (define-keymap
                      :parent mode-line-minor-mode-keymap
                      "<mode-line> <down-mouse-1>" menu
                      "<mode-line> <mouse-2>" #'describe-mode)))
              `(:propertize mode-line-collapse-minor-modes-to
                            mouse-face mode-line-highlight
                            help-echo "Hidden minor modes\n\
mouse-1: Display hidden minor modes\n\
mouse-2: Show help for enabled minor modes\n\
mouse-3: Toggle minor modes"
                            local-map ,keymap))))))

(defvar mode-line-major-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
      `(menu-item "Menu Bar" ignore
        :filter ,(lambda (_) (mouse-menu-major-mode-map))))
    (define-key map [mode-line mouse-2] 'describe-mode)
    (define-key map [mode-line down-mouse-3]
      `(menu-item "Minor Modes" ,mode-line-mode-menu
        :filter bindings--sort-menu-keymap))
    map) "\
Keymap to display on major mode.")

(defvar mode-line-minor-mode-keymap
  (let ((map (make-sparse-keymap))
        (mode-menu-binding
         `(menu-item "Menu Bar" ,mode-line-mode-menu
           :filter bindings--sort-menu-keymap)))
    (define-key map [mode-line down-mouse-1] 'mouse-minor-mode-menu)
    (define-key map [mode-line mouse-2] 'mode-line-minor-mode-help)
    (define-key map [mode-line down-mouse-3] mode-menu-binding)
    (define-key map [header-line down-mouse-3] mode-menu-binding)
    map) "\
Keymap to display on minor modes.")

(defvar mode-line-modes
  (let ((recursive-edit-help-echo
         "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
          '(:eval (car mode-line-modes-delimiters))
	  `(:propertize ("" mode-name)
			help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			mouse-face mode-line-highlight
			local-map ,mode-line-major-mode-keymap)
	  '("" mode-line-process)
	  (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		      'mouse-face 'mode-line-highlight
		      'local-map (make-mode-line-mouse-map
				  'mouse-2 #'mode-line-widen))
	  '("" mode-line-minor-modes)
          '(:eval (cdr mode-line-modes-delimiters))
	  (propertize "%]" 'help-echo recursive-edit-help-echo)
	  " "))
  "Mode line construct for displaying major and minor modes.")
(put 'mode-line-modes 'risky-local-variable t)

(defvar mode-line-column-line-number-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Toggle Line and Column Number Display")))
    (define-key menu-map [size-indication-mode]
      '(menu-item "Display Size Indication" size-indication-mode
		  :help "Toggle displaying a size indication in the mode-line"
		  :button (:toggle . size-indication-mode)))
    (define-key menu-map [line-number-mode]
      '(menu-item "Display Line Numbers" line-number-mode
		  :help "Toggle displaying line numbers in the mode-line"
		  :button (:toggle . line-number-mode)))
    (define-key menu-map [column-number-mode]
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
(make-obsolete-variable 'column-number-indicator-zero-based
                        'mode-line-position-column-format "28.1")

(defcustom mode-line-percent-position '(-3 "%p")
  "Specification of \"percentage offset\" of window through buffer.
This option specifies both the field width and the type of offset
displayed in `mode-line-position', a component of the default
`mode-line-format'."
  :type '(radio
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
  :risky t
  :group 'mode-line)

(defcustom mode-line-position-line-format '(" L%l")
  "Format used to display line numbers in the mode line.
This is used when `line-number-mode' is switched on.  The \"%l\"
format spec will be replaced by the line number.

Also see `mode-line-position-column-line-format'."
  :type '(list string)
  :version "28.1"
  :group 'mode-line)

(defcustom mode-line-position-column-format '(" C%c")
  "Format used to display column numbers in the mode line.
This is used when `column-number-mode' is switched on.  The
\"%c\" format spec is replaced by the zero-based column number,
and \"%C\" is replaced by the one-based column number.

Also see `mode-line-position-column-line-format'."
  :type '(list string)
  :version "28.1"
  :group 'mode-line)

(defcustom mode-line-position-column-line-format '(" (%l,%c)")
  "Format used to display combined line/column numbers in the mode line.
This is used when `column-number-mode' and `line-number-mode' are
switched on.  The \"%c\" format spec will be replaced by the
column number, which is zero-based if
`column-number-indicator-zero-based' is non-nil, and one-based if
`column-number-indicator-zero-based' is nil."
  :type '(list string)
  :version "28.1"
  :group 'mode-line)

(defconst mode-line-position--column-line-properties
  (list 'local-map mode-line-column-line-number-mode-map
        'mouse-face 'mode-line-highlight
        'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu"))

(defvar mode-line-position
  `((:propertize
     ("" mode-line-percent-position)
     local-map ,mode-line-column-line-number-mode-map
     display (min-width (5.0))
     mouse-face mode-line-highlight
     ;; XXX needs better description
     help-echo "Window Scroll Percentage
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
     (column-number-mode
      (column-number-indicator-zero-based
       (10
        (:propertize
         mode-line-position-column-line-format
         display (min-width (10.0))
         ,@mode-line-position--column-line-properties))
       (10
        (:propertize
         (:eval (string-replace
                 "%c" "%C" (car mode-line-position-column-line-format)))
         display (min-width (10.0))
         ,@mode-line-position--column-line-properties)))
      (6
       (:propertize
	mode-line-position-line-format
        display (min-width (6.0))
        ,@mode-line-position--column-line-properties)))
     (column-number-mode
      (column-number-indicator-zero-based
       (6
        (:propertize
         mode-line-position-column-format
         display (min-width (6.0))
         ,@mode-line-position--column-line-properties))
       (6
        (:propertize
         (:eval (string-replace
                 "%c" "%C" (car mode-line-position-column-format)))
         display (min-width (6.0))
         ,@mode-line-position--column-line-properties))))))
  "Mode line construct for displaying the position in the buffer.
Normally displays the buffer percentage and, optionally, the
buffer size, the line number and the column number.")
(put 'mode-line-position 'risky-local-variable t)

(defvar-keymap mode-line-buffer-identification-keymap
  :doc "Keymap for what is displayed by `mode-line-buffer-identification'."
  ;; Add menu of buffer operations to the buffer identification part
  ;; of the mode line.or header line.
  ;; Bind down- events so that the global keymap won't ``shine
  ;; through''.
  "<mode-line> <mouse-1>"        #'mode-line-previous-buffer
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>"      #'mode-line-previous-buffer
  "<mode-line> <mouse-3>"        #'mode-line-next-buffer
  "<header-line> <down-mouse-3>" #'ignore
  "<header-line> <mouse-3>"      #'mode-line-next-buffer)

(defun propertized-buffer-identification (fmt)
  "Return a list suitable for `mode-line-buffer-identification'.
FMT is a format specifier such as \"%12b\".  This function adds
text properties for face, help-echo, and local-map to it."
  (list (propertize fmt
		    'face 'mode-line-buffer-id
		    'help-echo
                    "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
		    'mouse-face 'mode-line-highlight
		    'local-map mode-line-buffer-identification-keymap)))

(defvar-local mode-line-buffer-identification
  (propertized-buffer-identification "%12b")
  "Mode line construct for identifying the buffer being displayed.
Its default value is (\"%12b\") with some text properties added.
Major modes that edit things other than ordinary files may change this
\(e.g. Info, Dired,...)")
;;;###autoload
(put 'mode-line-buffer-identification 'risky-local-variable t)

(defvar mode-line-misc-info
  '((global-mode-string ("" global-mode-string)))
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
             (list
              :propertize
              (list ""
	            'mode-line-mule-info
	            'mode-line-client
	            'mode-line-modified
		    'mode-line-remote
		    'mode-line-window-dedicated)
              'display '(min-width (6.0)))
	     'mode-line-frame-identification
	     'mode-line-buffer-identification
	     "   "
	     'mode-line-position
	     '(project-mode-line project-mode-line-format)
	     '(vc-mode vc-mode)
	     "  "
	     'mode-line-modes
	     'mode-line-misc-info
	     'mode-line-end-spaces)))
  (setq-default mode-line-format standard-mode-line-format)
  (put 'mode-line-format 'standard-value
       (list `(quote ,standard-mode-line-format))))


(defun mode-line-unbury-buffer (event)
  "Call `unbury-buffer' in this window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (unbury-buffer)))

(defun mode-line-bury-buffer (event)
  "Like `bury-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (bury-buffer)))

(defun mode-line-other-buffer ()
  "Switch to the most recently selected buffer other than the current one."
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

(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	(and (minibuffer-window-active-p (minibuffer-window))
	     (with-selected-window (minibuffer-window)
	       (eq window (minibuffer-selected-window)))))))

(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil.
Note that if `lexical-binding' is in effect, this function isn't
meaningful if it refers to a lexically bound variable."
  (unless (symbolp var)
    (signal 'wrong-type-argument (list 'symbolp var)))
  `(and (boundp (quote ,var)) ,var))

;; Use mode-line-mode-menu for local minor-modes only.
;; Global ones can go on the menubar (Options --> Show/Hide).
(define-key mode-line-mode-menu [overwrite-mode]
  '(menu-item "Overwrite (Ovwrt)" overwrite-mode
	      :help "Overwrite mode: typed characters replace existing text"
	      :button (:toggle . overwrite-mode)))
(define-key mode-line-mode-menu [outline-minor-mode]
  '(menu-item "Outline (Outl)" outline-minor-mode
	      ;; XXX: This needs a good, brief description.
	      :help ""
	      :button (:toggle . (bound-and-true-p outline-minor-mode))))
(define-key mode-line-mode-menu [highlight-changes-mode]
  '(menu-item "Highlight changes (Chg)" highlight-changes-mode
	      :help "Show changes in the buffer in a distinctive color"
	      :button (:toggle . (bound-and-true-p highlight-changes-mode))))
(define-key mode-line-mode-menu [hide-ifdef-mode]
  '(menu-item "Hide ifdef (Ifdef)" hide-ifdef-mode
	      :help "Show/Hide code within #ifdef constructs"
	      :button (:toggle . (bound-and-true-p hide-ifdef-mode))))
(define-key mode-line-mode-menu [glasses-mode]
  '(menu-item "Glasses (o^o)" glasses-mode
	      :help "Insert virtual separators to make long identifiers easy to read"
	      :button (:toggle . (bound-and-true-p glasses-mode))))
(define-key mode-line-mode-menu [font-lock-mode]
  '(menu-item "Font Lock" font-lock-mode
	      :help "Syntax coloring"
	      :button (:toggle . font-lock-mode)))
(define-key mode-line-mode-menu [flyspell-mode]
  '(menu-item "Flyspell (Fly)" flyspell-mode
	      :help "Spell checking on the fly"
	      :button (:toggle . (bound-and-true-p flyspell-mode))))
(define-key mode-line-mode-menu [completion-preview-mode]
  '(menu-item "Completion Preview (CP)" completion-preview-mode
              :help "Show preview of completion suggestions as you type"
              :enable completion-at-point-functions
              :button (:toggle . (bound-and-true-p completion-preview-mode))))
(define-key mode-line-mode-menu [auto-revert-tail-mode]
  '(menu-item "Auto revert tail (Tail)" auto-revert-tail-mode
	      :help "Revert the tail of the buffer when the file on disk grows"
	      :enable (buffer-file-name)
	      :button (:toggle . (bound-and-true-p auto-revert-tail-mode))))
(define-key mode-line-mode-menu [auto-revert-mode]
  '(menu-item "Auto revert (ARev)" auto-revert-mode
	      :help "Revert the buffer when the file on disk changes"
	      :button (:toggle . (bound-and-true-p auto-revert-mode))))
(define-key mode-line-mode-menu [auto-fill-mode]
  '(menu-item "Auto fill (Fill)" auto-fill-mode
	      :help "Automatically insert new lines"
	      :button (:toggle . auto-fill-function)))
(define-key mode-line-mode-menu [abbrev-mode]
  '(menu-item "Abbrev (Abbrev)" abbrev-mode
	      :help "Automatically expand abbreviations"
	      :button (:toggle . abbrev-mode)))

(defun mode-line-minor-mode-help (event)
  "Describe minor mode for EVENT on minor modes area of the mode line."
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (describe-minor-mode-from-indicator indicator event)))

(defvar mode-line-defining-kbd-macro (propertize " Def" 'face 'font-lock-warning-face)
  "String displayed in the mode line in keyboard macro recording mode.")
;;;###autoload
(put 'mode-line-defining-kbd-macro 'risky-local-variable t)

(defvar minor-mode-alist nil "\
Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line if VARIABLE's value is non-nil.

Actually, STRING need not be a string; any mode-line construct is
okay.  See `mode-line-format'.")
;;;###autoload
(put 'minor-mode-alist 'risky-local-variable t)

(setq minor-mode-alist
      '((abbrev-mode " Abbrev")
        (overwrite-mode overwrite-mode)
        (auto-fill-function " Fill")
        ;; not really a minor mode...
        (defining-kbd-macro mode-line-defining-kbd-macro)))

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
	     (t
              '(".o" "~" ".bin" ".lbin" ".so"
                ".a" ".ln" ".blg" ".bbl")))
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
      ;; FIXME: Maybe beginning-of-line, beginning-of-buffer, end-of-line,
      ;; end-of-buffer, end-of-file, buffer-read-only, and
      ;; file-supersession should all be user-errors!
      '(beginning-of-line beginning-of-buffer end-of-line
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
(global-set-key [iconify-frame] 'ignore)
(global-set-key [make-frame-visible] 'ignore)

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
      (dotimes (_ (if (numberp n) (abs n) 1))
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
      (dotimes (_ (if (numberp n) (abs n) 1))
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

The word boundaries are normally determined by the buffer's syntax
table and character script (according to `char-script-table'), but
`find-word-boundary-function-table', such as set up by `subword-mode',
can change that.  If a Lisp program needs to move by words determined
strictly by the syntax table, it should use `forward-word-strictly'
instead.  See Info node `(elisp) Word Motion' for details.

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

The word boundaries are normally determined by the buffer's syntax
table and character script (according to `char-script-table'), but
`find-word-boundary-function-table', such as set up by `subword-mode',
can change that.  If a Lisp program needs to move by words determined
strictly by the syntax table, it should use `forward-word-strictly'
instead.  See Info node `(elisp) Word Motion' for details.

If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed
if `inhibit-field-text-motion' is non-nil."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (backward-word n)
    (forward-word n)))

(defvar-keymap narrow-map
  :doc "Keymap for narrowing commands."
  "n" #'narrow-to-region
  "w" #'widen
  "g" #'goto-line-relative)
(define-key ctl-x-map "n" narrow-map)

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
(define-key esc-map " " 'cycle-spacing)
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
;; Many people are used to typing C-/ on GUI frames and getting C-_.
(define-key global-map [?\C-/] 'undo)
(define-key global-map "\C-_" 'undo)
;; Richard said that we should not use C-x <uppercase letter> and I have
;; no idea whereas to bind it.  Any suggestion welcome.  -stef
;; (define-key ctl-x-map "U" 'undo-only)
(defvar-keymap undo-repeat-map
  :doc "Keymap to repeat `undo' commands.  Used in `repeat-mode'."
  :repeat t
  "u" #'undo)

(define-key global-map '[(control ??)] 'undo-redo)
(define-key global-map [?\C-\M-_] 'undo-redo)

(define-key esc-map "!" 'shell-command)
(define-key esc-map "|" 'shell-command-on-region)
(define-key esc-map "&" 'async-shell-command)

(define-key ctl-x-map [right] 'next-buffer)
(define-key ctl-x-map [C-right] 'next-buffer)
(define-key global-map [XF86Forward] 'next-buffer)
(put 'next-buffer :advertised-binding [?\C-x right])
(define-key ctl-x-map [left] 'previous-buffer)
(define-key ctl-x-map [C-left] 'previous-buffer)
(define-key global-map [XF86Back] 'previous-buffer)
(put 'previous-buffer :advertised-binding [?\C-x left])

(defvar-keymap buffer-navigation-repeat-map
  :doc "Keymap to repeat `next-buffer' and `previous-buffer'.  Used in `repeat-mode'."
  :repeat t
  "<right>" #'next-buffer
  "<left>"  #'previous-buffer)

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

(defvar-keymap next-error-repeat-map
  :doc "Keymap to repeat `next-error' and `previous-error'.  Used in `repeat-mode'."
  :repeat t
  "n"   #'next-error
  "M-n" #'next-error
  "p"   #'previous-error
  "M-p" #'previous-error)

(defvar-keymap goto-map
  :doc "Keymap for navigation commands."
  "c"   #'goto-char
  "g"   #'goto-line
  "M-g" #'goto-line
  "n"   #'next-error
  "M-n" #'next-error
  "p"   #'previous-error
  "M-p" #'previous-error
  "TAB" #'move-to-column
  "i"   #'imenu)
(define-key esc-map "g" goto-map)

(defvar-keymap search-map
  :doc "Keymap for search related commands."
  "o"   #'occur
  "M-w" #'eww-search-words
  "h r" #'highlight-regexp
  "h p" #'highlight-phrase
  "h l" #'highlight-lines-matching-regexp
  "h ." #'highlight-symbol-at-point
  "h u" #'unhighlight-regexp
  "h f" #'hi-lock-find-patterns
  "h w" #'hi-lock-write-interactive-patterns)
(define-key esc-map "s" search-map)

(put 'highlight-regexp                   :advertised-binding [?\M-s ?h ?r])
(put 'highlight-phrase                   :advertised-binding [?\M-s ?h ?p])
(put 'highlight-lines-matching-regexp    :advertised-binding [?\M-s ?h ?l])
(put 'highlight-symbol-at-point          :advertised-binding [?\M-s ?h ?.])
(put 'unhighlight-regexp                 :advertised-binding [?\M-s ?h ?u])
(put 'hi-lock-find-patterns              :advertised-binding [?\M-s ?h ?f])
(put 'hi-lock-write-interactive-patterns :advertised-binding [?\M-s ?h ?w])

;;(defun function-key-error ()
;;  (interactive)
;;  (error "That function key is not bound to anything"))

(define-key global-map [menu] 'execute-extended-command)
(define-key global-map [find] 'search-forward)

;; Don't do this.  We define <delete> in function-key-map instead.
;(define-key global-map [delete] 'backward-delete-char)

;; natural bindings for terminal keycaps --- defined in X keysym order
(define-key global-map
            (if (eq system-type 'windows-nt) [scroll] [Scroll_Lock])
            #'scroll-lock-mode)
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
;; The next three keys are used on MS Windows and Android.
(define-key global-map [copy]		'kill-ring-save)
(define-key global-map [paste]		'yank)
(define-key global-map [cut]		'kill-region)
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

(define-key global-map [touch-end] 'ignore)

;; X11 distinguishes these keys from the non-kp keys.
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

(defun ignore-preserving-kill-region (&rest _)
  "Like `ignore', but don't overwrite `last-event' if it's `kill-region'."
  (declare (completion ignore))
  (interactive)
  (when (eq last-command 'kill-region)
    (setq this-command 'kill-region))
  nil)

(define-key global-map [mouse-movement] #'ignore-preserving-kill-region)

(define-key global-map "\C-t" 'transpose-chars)
(define-key esc-map "t" 'transpose-words)
(define-key esc-map "\C-t" 'transpose-sexps)
(define-key ctl-x-map "\C-t" 'transpose-lines)

(define-key esc-map ";" 'comment-dwim)
(define-key esc-map "j" 'default-indent-new-line)
(define-key esc-map "\C-j" 'default-indent-new-line)
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
  "Keymap for characters following \\`C-c'.")
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
(define-key global-map [C-M-delete] 'backward-kill-sexp)
(define-key global-map [C-M-backspace] 'backward-kill-sexp)
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


(defvar-keymap ctl-x-r-map
  :doc "Keymap for subcommands of \\`C-x r'."
  "c"     #'clear-rectangle
  "k"     #'kill-rectangle
  "d"     #'delete-rectangle
  "y"     #'yank-rectangle
  "o"     #'open-rectangle
  "t"     #'string-rectangle
  "N"     #'rectangle-number-lines
  "M-w"   #'copy-rectangle-as-kill
  "C-@"   #'point-to-register
  "C-SPC" #'point-to-register
  "SPC"   #'point-to-register
  "j"     #'jump-to-register
  "s"     #'copy-to-register
  "x"     #'copy-to-register
  "i"     #'insert-register
  "g"     #'insert-register
  "r"     #'copy-rectangle-to-register
  "n"     #'number-to-register
  "+"     #'increment-register
  "w"     #'window-configuration-to-register
  "f"     #'frameset-to-register
  "F"     #'file-to-register
  "B"     #'buffer-to-register)
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

(defvar-keymap page-navigation-repeat-map
  :doc "Keymap to repeat `forward-page' and `backward-page'.  Used in `repeat-mode'."
  :repeat t
  "]" #'forward-page
  "[" #'backward-page)

(define-key ctl-x-map "\C-p" 'mark-page)
(define-key ctl-x-map "l" 'count-lines-page)
(define-key ctl-x-map "np" 'narrow-to-page)

(defvar-keymap abbrev-map
  :doc "Keymap for abbrev commands."
  "l"   #'add-mode-abbrev
  "C-a" #'add-mode-abbrev
  "g"   #'add-global-abbrev
  "+"   #'add-mode-abbrev
  "i g" #'inverse-add-global-abbrev
  "i l" #'inverse-add-mode-abbrev
  "-"   #'inverse-add-global-abbrev
  "e"   #'expand-abbrev
  "'"   #'expand-abbrev)
(define-key ctl-x-map "a" abbrev-map)

(define-key esc-map "'" 'abbrev-prefix-mark)
(define-key ctl-x-map "'" 'expand-abbrev)
(define-key ctl-x-map "\C-b" 'list-buffers)

(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(define-key ctl-x-map "z" 'repeat)

(defvar-keymap ctl-x-x-map
  :doc "Keymap for subcommands of \\`C-x x'."
  "f" #'font-lock-update
  "g" #'revert-buffer-quick
  "r" #'rename-buffer
  "u" #'rename-uniquely
  "n" #'clone-buffer
  "i" #'insert-buffer
  "t" #'toggle-truncate-lines
  "@" #'tramp-revert-buffer-with-sudo)
(define-key ctl-x-map "x" ctl-x-x-map)

(define-key esc-map "\C-l" 'reposition-window)

(define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)
(define-key ctl-x-4-map "c" 'clone-indirect-buffer-other-window)

;; Signal handlers
(define-key special-event-map [sigusr1] 'ignore)
(define-key special-event-map [sigusr2] 'ignore)

;; Text conversion
(define-key global-map [text-conversion] 'analyze-text-conversion)

(define-obsolete-function-alias 'bindings--define-key #'define-key "31.1")

;; Don't look for autoload cookies in this file.
;; Local Variables:
;; no-update-autoloads: t
;; End:

;;; bindings.el ends here
