;;; x-win.el --- parse switches controlling interface with X window system
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; X-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that X windows are to be used.  Command line switches are parsed and those
;; pertaining to X are processed and removed from the command line.  The
;; X display is opened and hooks are set for popping up the initial window.

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

(if (not (eq window-system 'x))
    (error "%s: Loading x-win.el but not compiled for X" (invocation-name)))
	 
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)

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
  "The X display name specifying server and X frame.")

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
;; Standard X cursor shapes, courtesy of Mr. Fox, who wanted ALL of them.
;;

(defconst x-pointer-X-cursor 0)
(defconst x-pointer-arrow 2)
(defconst x-pointer-based-arrow-down 4)
(defconst x-pointer-based-arrow-up 6)
(defconst x-pointer-boat 8)
(defconst x-pointer-bogosity 10)
(defconst x-pointer-bottom-left-corner 12)
(defconst x-pointer-bottom-right-corner 14)
(defconst x-pointer-bottom-side 16)
(defconst x-pointer-bottom-tee 18)
(defconst x-pointer-box-spiral 20)
(defconst x-pointer-center-ptr 22)
(defconst x-pointer-circle 24)
(defconst x-pointer-clock 26)
(defconst x-pointer-coffee-mug 28)
(defconst x-pointer-cross 30)
(defconst x-pointer-cross-reverse 32)
(defconst x-pointer-crosshair 34)
(defconst x-pointer-diamond-cross 36)
(defconst x-pointer-dot 38)
(defconst x-pointer-dotbox 40)
(defconst x-pointer-double-arrow 42)
(defconst x-pointer-draft-large 44)
(defconst x-pointer-draft-small 46)
(defconst x-pointer-draped-box 48)
(defconst x-pointer-exchange 50)
(defconst x-pointer-fleur 52)
(defconst x-pointer-gobbler 54)
(defconst x-pointer-gumby 56)
(defconst x-pointer-hand1 58)
(defconst x-pointer-hand2 60)
(defconst x-pointer-heart 62)
(defconst x-pointer-icon 64)
(defconst x-pointer-iron-cross 66)
(defconst x-pointer-left-ptr 68)
(defconst x-pointer-left-side 70)
(defconst x-pointer-left-tee 72)
(defconst x-pointer-leftbutton 74)
(defconst x-pointer-ll-angle 76)
(defconst x-pointer-lr-angle 78)
(defconst x-pointer-man 80)
(defconst x-pointer-middlebutton 82)
(defconst x-pointer-mouse 84)
(defconst x-pointer-pencil 86)
(defconst x-pointer-pirate 88)
(defconst x-pointer-plus 90)
(defconst x-pointer-question-arrow 92)
(defconst x-pointer-right-ptr 94)
(defconst x-pointer-right-side 96)
(defconst x-pointer-right-tee 98)
(defconst x-pointer-rightbutton 100)
(defconst x-pointer-rtl-logo 102)
(defconst x-pointer-sailboat 104)
(defconst x-pointer-sb-down-arrow 106)
(defconst x-pointer-sb-h-double-arrow 108)
(defconst x-pointer-sb-left-arrow 110)
(defconst x-pointer-sb-right-arrow 112)
(defconst x-pointer-sb-up-arrow 114)
(defconst x-pointer-sb-v-double-arrow 116)
(defconst x-pointer-shuttle 118)
(defconst x-pointer-sizing 120)
(defconst x-pointer-spider 122)
(defconst x-pointer-spraycan 124)
(defconst x-pointer-star 126)
(defconst x-pointer-target 128)
(defconst x-pointer-tcross 130)
(defconst x-pointer-top-left-arrow 132)
(defconst x-pointer-top-left-corner 134)
(defconst x-pointer-top-right-corner 136)
(defconst x-pointer-top-side 138)
(defconst x-pointer-top-tee 140)
(defconst x-pointer-trek 142)
(defconst x-pointer-ul-angle 144)
(defconst x-pointer-umbrella 146)
(defconst x-pointer-ur-angle 148)
(defconst x-pointer-watch 150)
(defconst x-pointer-xterm 152)

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
		   "medium goldenrod"
		   "MediumGoldenrod"
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
		   "medium forest green"
		   "MediumForestGreen"
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
  "The full list of X colors from the rgb.text file.")

(defun x-defined-colors ()
  "Return a list of colors supported by the current X-Display."
  (let ((all-colors x-colors)
	(this-color nil)
	(defined-colors nil))
    (while all-colors
      (setq this-color (car all-colors)
	    all-colors (cdr all-colors))
      (and (x-color-defined-p this-color)
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
(define-key function-key-map [backspace] [127])
(define-key function-key-map [delete] [127])
(define-key function-key-map [tab] [?\t])
(define-key function-key-map [linefeed] [?\n])
(define-key function-key-map [clear] [11])
(define-key function-key-map [return] [13])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-backspace] [?\M-\d])
(define-key function-key-map [M-delete] [?\M-\d])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [M-linefeed] [?\M-\n])
(define-key function-key-map [M-clear] [?\M-\013])
(define-key function-key-map [M-return] [?\M-\015])
(define-key function-key-map [M-escape] [?\M-\e])

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'backspace 'ascii-character 127)
(put 'delete 'ascii-character 127)
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)

;; Set up to recognize vendor-specific keysyms.
;; Unless/until there is a real conflict,
;; we need not try to make this list depend on
;; the type of X server in use.
(setq system-key-alist
      '(
	;; These are some HP keys.
	(  168 . mute-acute)
	(  169 . mute-grave)
	(  170 . mute-asciicircum)
	(  171 . mute-diaeresis)
	(  172 . mute-asciitilde)
	(  175 . lira)
	(  190 . guilder)
	(  252 . block)
	(  256 . longminus)
	(65388 . reset)
	(65389 . system)
	(65390 . user)
	(65391 . clearline)
	(65392 . insertline)
	(65393 . deleteline)
	(65394 . insertchar)
	(65395 . deletechar)
	(65396 . backtab)
	(65397 . kp-backtab)
	;; This is used by DEC's X server.
	(65280 . remove)
	;; These are for Sun.
 	(392963 . mute-acute)
	(392960 . mute-grave)
	(392964 . mute-diaeresis)
	(392961 . mute-asciicircum)
	(392976 . f35)
    	(392977 . f36)
	(393056 . req)
	;; These are for Sun under X11R6
	(393072 . props)
	(393073 . front)
	(393074 . copy)
	(393075 . open)
	(393076 . paste)
	(393077 . cut)
	))

;;;; Selections and cut buffers

;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-cut-buffer-or-selection-value.
(defvar x-last-selected-text nil)

;;; It is said that overlarge strings are slow to put into the cut buffer.
;;; Note this value is overridden below.
(defvar x-cut-buffer-max 20000
  "Max number of characters to put in the cut buffer.")

(defvar x-select-enable-clipboard nil
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to the primary selection.")

;;; Make TEXT, a string, the primary X selection.
;;; Also, set the value of X cut buffer 0, for backward compatibility
;;; with older X applications.
;;; gildea@lcs.mit.edu says it's not desirable to put kills
;;; in the clipboard.
(defun x-select-text (text &optional push)
  ;; Don't send the cut buffer too much text.
  ;; It becomes slow, and if really big it causes errors.
  (if (< (length text) x-cut-buffer-max)
      (x-set-cut-buffer text push)
    (x-set-cut-buffer "" push))
  (x-set-selection 'PRIMARY text)
  (if x-select-enable-clipboard
      (x-set-selection 'CLIPBOARD text))
  (setq x-last-selected-text text))

;;; Return the value of the current X selection.
;;; Consult the selection, then the cut buffer.  Treat empty strings
;;; as if they were unset.
(defun x-cut-buffer-or-selection-value ()
  (let (text)

    ;; Don't die if x-get-selection signals an error.
    (condition-case c
	(setq text (x-get-selection 'PRIMARY))
      (error (message "%s" c)))
    (if (string= text "") (setq text nil))

    (if x-select-enable-clipboard
	(condition-case c
	    (setq text (x-get-selection 'CLIPBOARD))
	  (error (message "%s" c))))
    (if (string= text "") (setq text nil))
    (or text (setq text (x-get-cut-buffer 0)))
    (if (string= text "") (setq text nil))

    (cond
     ((not text) nil)
     ((eq text x-last-selected-text) nil)
     ((string= text x-last-selected-text)
      ;; Record the newer string, so subsequent calls can use the `eq' test.
      (setq x-last-selected-text text)
      nil)
     (t
      (setq x-last-selected-text text)))))


;;; Do the actual X Windows setup here; the above code just defines
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
    (x-open-connection (or x-display-name
			   (setq x-display-name (getenv "DISPLAY")))
		       x-command-line-resources
		       ;; Exit Emacs with fatal error if this fails.
		       t)
  (x-open-connection (or x-display-name
			 (setq x-display-name (getenv "DISPLAY")))
		     x-command-line-resources))

(setq frame-creation-function 'x-create-frame-with-faces)

(setq x-cut-buffer-max (min (- (/ (x-server-max-request-size) 2) 100)
			    x-cut-buffer-max))

;; Sun expects the menu bar cut and paste commands to use the clipboard.
(if (string-match "X11/NeWS - Sun Microsystems Inc\\."
		  (x-server-vendor))
    (menu-bar-enable-clipboard))

;; Apply a geometry resource to the initial frame.  Put it at the end
;; of the alist, so that anything specified on the command line takes
;; precedence.
(let ((res-geometry (x-get-resource "geometry" "Geometry")))
  (if res-geometry
      (setq initial-frame-alist (append initial-frame-alist
					(x-parse-geometry res-geometry)))))

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
  (error "Suspending an emacs running under X makes no sense"))
(add-hook 'suspend-hook 'x-win-suspend-error)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; Turn off window-splitting optimization; X is usually fast enough
;;; that this is only annoying.
(setq split-window-keep-point t)

;;; x-win.el ends here
