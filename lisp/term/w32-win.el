;;; win32-win.el --- parse switches controlling interface with win32

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Kevin Gallo
;; Keywords: terminals

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

;; win32-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that win32 windows are to be used.  Command line switches are parsed and those
;; pertaining to win32 are processed and removed from the command line.  The
;; win32 display is opened and hooks are set for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window (s).

;;; Code:


;; These are the standard X switches from the Xt Initialize.c file of
;; Release 4.

;; Command line		Resource Manager string

;; +rv			*reverseVideo
;; +synchronous		*synchronous
;; -background		*background
;; -bd			*borderColor
;; -bg			*background
;; -bordercolor		*borderColor
;; -borderwidth		.borderWidth
;; -bw			.borderWidth
;; -display		.display
;; -fg			*foreground
;; -fn			*font
;; -font		*font
;; -foreground		*foreground
;; -geometry		.geometry
;; -i			.iconType
;; -itype		.iconType
;; -iconic		.iconic
;; -name		.name
;; -reverse		*reverseVideo
;; -rv			*reverseVideo
;; -selectionTimeout    .selectionTimeout
;; -synchronous		*synchronous
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

(if (not (eq window-system 'win32))
    (error "%s: Loading win32-win.el but not compiled for win32" (invocation-name)))
	 
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)

;; Because Windows scrollbars look and act quite differently compared
;; with the standard X scroll-bars, we don't try to use the normal
;; scroll bar routines.

(defun win32-handle-scroll-bar-event (event)
  "Handle Win32 scroll bar events to do normal Window style scrolling."
  (interactive "e")
  (let ((old-window (selected-window)))
    (unwind-protect
	(let* ((position (event-start event))
	       (window (nth 0 position))
	       (portion-whole (nth 2 position))
	       (bar-part (nth 4 position)))
	  (save-excursion
	    (select-window window)
	    (cond
	     ((eq bar-part 'up)
	      (scroll-down 1))
	     ((eq bar-part 'above-handle)
	      (scroll-down))
	     ((eq bar-part 'handle)
	      (scroll-bar-maybe-set-window-start event))
	     ((eq bar-part 'below-handle)
	      (scroll-up))
	     ((eq bar-part 'down)
	      (scroll-up 1))
	     )))
      (select-window old-window))))

;; The following definition is used for debugging.
;(defun win32-handle-scroll-bar-event (event) (interactive "e") (princ event))

(global-set-key [vertical-scroll-bar mouse-1] 'win32-handle-scroll-bar-event)

;; (scroll-bar-mode nil)

(defvar x-invocation-args)

(defvar x-command-line-resources nil)

(defconst x-option-alist
  '(("-bw" .	x-handle-numeric-switch)
    ("-d" .		x-handle-display)
    ("-display" .	x-handle-display)
    ("-name" .	x-handle-name-rn-switch)
    ("-rn" .	x-handle-name-rn-switch)
    ("-T" .		x-handle-switch)
    ("-r" .		x-handle-switch)
    ("-rv" .	x-handle-switch)
    ("-reverse" .	x-handle-switch)
    ("-fn" .	x-handle-switch)
    ("-font" .	x-handle-switch)
    ("-ib" .	x-handle-numeric-switch)
    ("-g" .		x-handle-geometry)
    ("-geometry" .	x-handle-geometry)
    ("-fg" .	x-handle-switch)
    ("-foreground".	x-handle-switch)
    ("-bg" .	x-handle-switch)
    ("-background".	x-handle-switch)
    ("-ms" .	x-handle-switch)
    ("-itype" .	x-handle-switch)
    ("-i" 	.	x-handle-switch)
    ("-iconic" .	x-handle-iconic)
    ("-xrm" .       x-handle-xrm-switch)
    ("-cr" .	x-handle-switch)
    ("-vb" .	x-handle-switch)
    ("-hb" .	x-handle-switch)
    ("-bd" .	x-handle-switch)))

(defconst x-long-option-alist
  '(("--border-width" .	"-bw")
    ("--display" .	"-d")
    ("--name" .		"-name")
    ("--title" .	"-T")
    ("--reverse-video" . "-reverse")
    ("--font" .		"-font")
    ("--internal-border" . "-ib")
    ("--geometry" .	"-geometry")
    ("--foreground-color" . "-fg")
    ("--background-color" . "-bg")
    ("--mouse-color" .	"-ms")
    ("--icon-type" .	"-itype")
    ("--iconic" .	"-iconic")
    ("--xrm" .		"-xrm")
    ("--cursor-color" .	"-cr")
    ("--vertical-scroll-bars" . "-vb")
    ("--border-color" .	"-bd")))

(defconst x-switch-definitions
  '(("-name" name)
    ("-T" name)
    ("-r" reverse t)
    ("-rv" reverse t)
    ("-reverse" reverse t)
    ("-fn" font)
    ("-font" font)
    ("-ib" internal-border-width)
    ("-fg" foreground-color)
    ("-foreground" foreground-color)
    ("-bg" background-color)
    ("-background" background-color)
    ("-ms" mouse-color)
    ("-cr" cursor-color)
    ("-itype" icon-type t)
    ("-i" icon-type t)
    ("-vb" vertical-scroll-bars t)
    ("-hb" horizontal-scroll-bars t)
    ("-bd" border-color)
    ("-bw" border-width)))

;; Handler for switches of the form "-switch value" or "-switch".
(defun x-handle-switch (switch)
  (let ((aelt (assoc switch x-switch-definitions)))
    (if aelt
	(if (nth 2 aelt)
	    (setq default-frame-alist
		  (cons (cons (nth 1 aelt) (nth 2 aelt))
			default-frame-alist))
	  (setq default-frame-alist
		(cons (cons (nth 1 aelt)
			    (car x-invocation-args))
		      default-frame-alist)
		x-invocation-args (cdr x-invocation-args))))))

;; Make -iconic apply only to the initial frame!
(defun x-handle-iconic (switch)
  (setq initial-frame-alist
	(cons '(visibility . icon) initial-frame-alist)))

;; Handler for switches of the form "-switch n"
(defun x-handle-numeric-switch (switch)
  (let ((aelt (assoc switch x-switch-definitions)))
    (if aelt
	(setq default-frame-alist
	      (cons (cons (nth 1 aelt)
			  (string-to-int (car x-invocation-args)))
		    default-frame-alist)
	      x-invocation-args
	      (cdr x-invocation-args)))))

;; Handle the -xrm option.
(defun x-handle-xrm-switch (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-command-line-resources (car x-invocation-args))
  (setq x-invocation-args (cdr x-invocation-args)))

;; Handle the geometry option
(defun x-handle-geometry (switch)
  (let ((geo (x-parse-geometry (car x-invocation-args))))
    (setq initial-frame-alist
	  (append initial-frame-alist
		  (if (or (assq 'left geo) (assq 'top geo))
		      '((user-position . t)))
		  (if (or (assq 'height geo) (assq 'width geo))
		      '((user-size . t)))
		  geo)
	  x-invocation-args (cdr x-invocation-args))))

;; Handle the -name and -rn options.  Set the variable x-resource-name
;; to the option's operand; if the switch was `-name', set the name of
;; the initial frame, too.
(defun x-handle-name-rn-switch (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-resource-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args))
  (if (string= switch "-name")
      (setq initial-frame-alist (cons (cons 'name x-resource-name)
				      initial-frame-alist))))

(defvar x-display-name nil
  "The display name specifying server and frame.")

(defun x-handle-display (switch)
  (setq x-display-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args)))

(defvar x-invocation-args nil)

(defun x-handle-args (args)
  "Process the X-related command line options in ARGS.
This is done before the user's startup file is loaded.  They are copied to
x-invocation args from which the X-related things are extracted, first
the switch (e.g., \"-fg\") in the following code, and possible values
\(e.g., \"black\") in the option handler code (e.g., x-handle-switch).
This returns ARGS with the arguments that have been processed removed."
  (message "%s" args)
  (setq x-invocation-args args
	args nil)
  (while x-invocation-args
    (let* ((this-switch (car x-invocation-args))
	   (orig-this-switch this-switch)
	   completion argval aelt)
      (setq x-invocation-args (cdr x-invocation-args))
      ;; Check for long options with attached arguments
      ;; and separate out the attached option argument into argval.
      (if (string-match "^--[^=]*=" this-switch)
	  (setq argval (substring this-switch (match-end 0))
		this-switch (substring this-switch 0 (1- (match-end 0)))))
      (setq completion (try-completion this-switch x-long-option-alist))
      (if (eq completion t)
	  ;; Exact match for long option.
	  (setq this-switch (cdr (assoc this-switch x-long-option-alist)))
	(if (stringp completion)
	    (let ((elt (assoc completion x-long-option-alist)))
	      ;; Check for abbreviated long option.
	      (or elt
		  (error "Option `%s' is ambiguous" this-switch))
	      (setq this-switch (cdr elt)))
	  ;; Check for a short option.
	  (setq argval nil this-switch orig-this-switch)))
      (setq aelt (assoc this-switch x-option-alist))
      (if aelt
	  (if argval
	      (let ((x-invocation-args
		     (cons argval x-invocation-args)))
		(funcall (cdr aelt) this-switch))
	    (funcall (cdr aelt) this-switch))
	(setq args (cons this-switch args)))))
  (setq args (nreverse args)))



;;
;; Available colors
;;

(defvar x-colors '("aquamarine"
		   "Aquamarine"
		   "medium aquamarine"
		   "MediumAquamarine"
		   "black"
		   "Black"
		   "blue"
		   "Blue"
		   "cadet blue"
		   "CadetBlue"
		   "cornflower blue"
		   "CornflowerBlue"
		   "dark slate blue"
		   "DarkSlateBlue"
		   "light blue"
		   "LightBlue"
		   "light steel blue"
		   "LightSteelBlue"
		   "medium blue"
		   "MediumBlue"
		   "medium slate blue"
		   "MediumSlateBlue"
		   "midnight blue"
		   "MidnightBlue"
		   "navy blue"
		   "NavyBlue"
		   "navy"
		   "Navy"
		   "sky blue"
		   "SkyBlue"
		   "slate blue"
		   "SlateBlue"
		   "steel blue"
		   "SteelBlue"
		   "coral"
		   "Coral"
		   "cyan"
		   "Cyan"
		   "firebrick"
		   "Firebrick"
		   "brown"
		   "Brown"
		   "gold"
		   "Gold"
		   "goldenrod"
		   "Goldenrod"
		   "green"
		   "Green"
		   "dark green"
		   "DarkGreen"
		   "dark olive green"
		   "DarkOliveGreen"
		   "forest green"
		   "ForestGreen"
		   "lime green"
		   "LimeGreen"
		   "medium sea green"
		   "MediumSeaGreen"
		   "medium spring green"
		   "MediumSpringGreen"
		   "pale green"
		   "PaleGreen"
		   "sea green"
		   "SeaGreen"
		   "spring green"
		   "SpringGreen"
		   "yellow green"
		   "YellowGreen"
		   "dark slate grey"
		   "DarkSlateGrey"
		   "dark slate gray"
		   "DarkSlateGray"
		   "dim grey"
		   "DimGrey"
		   "dim gray"
		   "DimGray"
		   "light grey"
		   "LightGrey"
		   "light gray"
		   "LightGray"
		   "gray"
		   "grey"
		   "Gray"
		   "Grey"
		   "khaki"
		   "Khaki"
		   "magenta"
		   "Magenta"
		   "maroon"
		   "Maroon"
		   "orange"
		   "Orange"
		   "orchid"
		   "Orchid"
		   "dark orchid"
		   "DarkOrchid"
		   "medium orchid"
		   "MediumOrchid"
		   "pink"
		   "Pink"
		   "plum"
		   "Plum"
		   "red"
		   "Red"
		   "indian red"
		   "IndianRed"
		   "medium violet red"
		   "MediumVioletRed"
		   "orange red"
		   "OrangeRed"
		   "violet red"
		   "VioletRed"
		   "salmon"
		   "Salmon"
		   "sienna"
		   "Sienna"
		   "tan"
		   "Tan"
		   "thistle"
		   "Thistle"
		   "turquoise"
		   "Turquoise"
		   "dark turquoise"
		   "DarkTurquoise"
		   "medium turquoise"
		   "MediumTurquoise"
		   "violet"
		   "Violet"
		   "blue violet"
		   "BlueViolet"
		   "wheat"
		   "Wheat"
		   "white"
		   "White"
		   "yellow"
		   "Yellow"
		   "green yellow"
		   "GreenYellow")
  "The full list of X colors from the `rgb.text' file.")

(defun x-defined-colors (&optional frame)
  "Return a list of colors supported for a particular frame.
The argument FRAME specifies which frame to try.
The value may be different for frames on different X displays."
  (or frame (setq frame (selected-frame)))
  (let ((all-colors x-colors)
	(this-color nil)
	(defined-colors nil))
    (while all-colors
      (setq this-color (car all-colors)
	    all-colors (cdr all-colors))
      (and (face-color-supported-p frame this-color t)
	   (setq defined-colors (cons this-color defined-colors))))
    defined-colors))

;;;; Function keys

(defun iconify-or-deiconify-frame ()
  "Iconify the selected frame, or deiconify if it's currently an icon."
  (interactive)
  (if (eq (cdr (assq 'visibility (frame-parameters))) t)
      (iconify-frame)
    (make-frame-visible)))

(substitute-key-definition 'suspend-emacs 'iconify-or-deiconify-frame
			   global-map)

;; Map certain keypad keys into ASCII characters
;; that people usually expect.
(define-key function-key-map [tab] [?\t])
(define-key function-key-map [linefeed] [?\n])
(define-key function-key-map [clear] [11])
(define-key function-key-map [return] [13])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [M-linefeed] [?\M-\n])
(define-key function-key-map [M-clear] [?\M-\013])
(define-key function-key-map [M-return] [?\M-\015])
(define-key function-key-map [M-escape] [?\M-\e])

;; These don't do the right thing (voelker)
;(define-key function-key-map [backspace] [127])
;(define-key function-key-map [delete] [127])
;(define-key function-key-map [M-backspace] [?\M-\d])
;(define-key function-key-map [M-delete] [?\M-\d])

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)
;; These don't seem to be necessary (voelker)
;(put 'backspace 'ascii-character 127)
;(put 'delete 'ascii-character 127)


;;;; Selections and cut buffers

;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-cut-buffer-or-selection-value.
(defvar x-last-selected-text nil)

;;; It is said that overlarge strings are slow to put into the cut buffer.
;;; Note this value is overridden below.
(defvar x-cut-buffer-max 20000
  "Max number of characters to put in the cut buffer.")

(defvar x-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to the primary selection.")

(defun x-select-text (text &optional push)
  (if x-select-enable-clipboard 
      (win32-set-clipboard-data text))
  (setq x-last-selected-text text))
    
;;; Return the value of the current selection.
;;; Consult the selection, then the cut buffer.  Treat empty strings
;;; as if they were unset.
(defun x-get-selection-value ()
  (if x-select-enable-clipboard 
      (let (text)
	;; Don't die if x-get-selection signals an error.
	(condition-case c
	    (setq text (win32-get-clipboard-data))
	  (error (message "win32-get-clipboard-data:%s" c)))
	(if (string= text "") (setq text nil))
	(cond
	 ((not text) nil)
	 ((eq text x-last-selected-text) nil)
	 ((string= text x-last-selected-text)
	  ;; Record the newer string, so subsequent calls can use the 'eq' test.
	  (setq x-last-selected-text text)
	  nil)
	 (t
	  (setq x-last-selected-text text))))))

;;; Do the actual Windows setup here; the above code just defines
;;; functions and variables that we use now.

(setq command-line-args (x-handle-args command-line-args))

;;; Make sure we have a valid resource name.
(or (stringp x-resource-name)
    (let (i)
      (setq x-resource-name (invocation-name))

      ;; Change any . or * characters in x-resource-name to hyphens,
      ;; so as not to choke when we use it in X resource queries.
      (while (setq i (string-match "[.*]" x-resource-name))
	(aset x-resource-name i ?-))))

;; For the benefit of older Emacses (19.27 and earlier) that are sharing
;; the same lisp directory, don't pass the third argument unless we seem
;; to have the multi-display support.
(if (fboundp 'x-close-connection)
    (x-open-connection ""
		       x-command-line-resources
		       ;; Exit Emacs with fatal error if this fails.
		       t)
  (x-open-connection ""
		     x-command-line-resources))

(setq frame-creation-function 'x-create-frame-with-faces)

(setq x-cut-buffer-max (min (- (/ (x-server-max-request-size) 2) 100)
			    x-cut-buffer-max))

;; Win32 expects the menu bar cut and paste commands to use the clipboard.
;; This has ,? to match both on Sunos and on Solaris.
(menu-bar-enable-clipboard)

;; Apply a geometry resource to the initial frame.  Put it at the end
;; of the alist, so that anything specified on the command line takes
;; precedence.
(let* ((res-geometry (x-get-resource "geometry" "Geometry"))
       parsed)
  (if res-geometry
      (progn
	(setq parsed (x-parse-geometry res-geometry))
	;; If the resource specifies a position,
	;; call the position and size "user-specified".
	(if (or (assq 'top parsed) (assq 'left parsed))
	    (setq parsed (cons '(user-position . t)
			       (cons '(user-size . t) parsed))))
	;; All geometry parms apply to the initial frame.
	(setq initial-frame-alist (append initial-frame-alist parsed))
	;; The size parms apply to all frames.
	(if (assq 'height parsed)
	    (setq default-frame-alist
		  (cons (cons 'height (cdr (assq 'height parsed)))
			default-frame-alist)))
	(if (assq 'width parsed)
	    (setq default-frame-alist
		  (cons (cons 'width (cdr (assq 'width parsed)))
			default-frame-alist))))))

;; Check the reverseVideo resource.
(let ((case-fold-search t))
  (let ((rv (x-get-resource "reverseVideo" "ReverseVideo")))
    (if (and rv
	     (string-match "^\\(true\\|yes\\|on\\)$" rv))
	(setq default-frame-alist
	      (cons '(reverse . t) default-frame-alist)))))

;; Set x-selection-timeout, measured in milliseconds.
(let ((res-selection-timeout
       (x-get-resource "selectionTimeout" "SelectionTimeout")))
  (setq x-selection-timeout 20000)
  (if res-selection-timeout
      (setq x-selection-timeout (string-to-number res-selection-timeout))))

(defun x-win-suspend-error ()
  (error "Suspending an emacs running under Win32 makes no sense"))
(add-hook 'suspend-hook 'x-win-suspend-error)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)

;;; Turn off window-splitting optimization; win32 is usually fast enough
;;; that this is only annoying.
(setq split-window-keep-point t)

;; Don't show the frame name; that's redundant.
(setq-default mode-line-buffer-identification '("Emacs: %12b"))

;;; Set to a system sound if you want a fancy bell.
(set-message-beep 'ok)

;; Remap some functions to call win32 common dialogs

(defun internal-face-interactive (what &optional bool)
  (let* ((fn (intern (concat "face-" what)))
	 (prompt (concat "Set " what " of face"))
	 (face (read-face-name (concat prompt ": ")))
	 (default (if (fboundp fn)
		      (or (funcall fn face (selected-frame))
			  (funcall fn 'default (selected-frame)))))
	 (fn-win (intern (concat (symbol-name window-system) "-select-" what)))
	 (value 
	  (if (fboundp fn-win)
	      (funcall fn-win)
	    (if bool
		(y-or-n-p (concat "Should face " (symbol-name face)
				  " be " bool "? "))
	      (read-string (concat prompt " " (symbol-name face) " to: ")
			   default)))))
	 (list face (if (equal value "") nil value))))

;; Redefine the font selection to use the Win32 dialog

(defun mouse-set-font (&rest fonts)
  (interactive)
  (set-default-font (win32-select-font)))

;;; win32-win.el ends here
