;; Parse switches controlling how Emacs interfaces with X window system.
;; Copyright (C) 1990 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; X-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that X windows are to be used.  Command line switches are parsed and those
;; pertaining to X are processed and removed from the command line.  The
;; X display is opened and hooks are set for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window (s).

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
    (error "Loading x-win.el but not compiled for X"))
	 
(require 'frame)
(require 'mouse)
(require 'scroll-bar)

(setq command-switch-alist
      (append '(("-bw" .	x-handle-numeric-switch)
		("-d" .		x-handle-display)
		("-display" .	x-handle-display)
		("-name" .	x-handle-switch)
		("-T" .		x-handle-switch)
		("-r" .		x-handle-switch)
		("-rv" .	x-handle-switch)
		("-reverse" .	x-handle-switch)
		("-fn" .	x-handle-switch)
		("-font" .	x-handle-switch)
		("-ib" .	x-handle-switch)
		("-g" .		x-handle-geometry)
		("-geometry" .	x-handle-geometry)
		("-fg" .	x-handle-switch)
		("-foreground".	x-handle-switch)
		("-bg" .	x-handle-switch)
		("-background".	x-handle-switch)
		("-ms" .	x-handle-switch)
		("-itype" .	x-handle-switch)
		("-iconic" .	x-handle-switch)
		("-cr" .	x-handle-switch)
		("-vb" .	x-handle-switch)
		("-hb" .	x-handle-switch)
		("-bd" .	x-handle-switch))
	      command-switch-alist))

(defconst x-switch-definitions
  '(("-name" name)
    ("-T" name)
    ("-r" lose)
    ("-rv" lose)
    ("-reverse" lose)
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
    ("-iconic" iconic-startup t)
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

;; Handle the geometry option
(defun x-handle-geometry (switch)
  (setq initial-frame-alist (append initial-frame-alist
				     (x-geometry (car x-invocation-args)))
	x-invocation-args (cdr x-invocation-args)))

(defvar x-display-name nil
  "The X display name specifying server and X frame.")

(defun x-handle-display (switch)
  (setq x-display-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args)))

(defvar x-invocation-args nil)

(defun x-handle-args (args)
  "Here the X-related command line options in ARGS are processed,
before the user's startup file is loaded.  They are copied to
x-invocation args from which the X-related things are extracted, first
the switch (e.g., \"-fg\") in the following code, and possible values
(e.g., \"black\") in the option handler code (e.g., x-handle-switch).
This returns ARGS with the arguments that have been processed removed."
  (setq x-invocation-args args
	args nil)
  (while x-invocation-args
    (let* ((this-switch (car x-invocation-args))
	   (aelt (assoc this-switch command-switch-alist)))
      (setq x-invocation-args (cdr x-invocation-args))
      (if aelt
	  (funcall (cdr aelt) this-switch)
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
      (and (x-defined-color this-color)
	   (setq defined-colors (cons this-color defined-colors))))
    defined-colors))

;;;; Function keys

;;; Give some common function keys reasonable definitions.
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [left] 'backward-char)
(define-key global-map [up] 'previous-line)
(define-key global-map [right] 'forward-char)
(define-key global-map [down] 'next-line)
(define-key global-map [prior] 'scroll-down)
(define-key global-map [next] 'scroll-up)
(define-key global-map [M-next] 'scroll-other-window)
(define-key global-map [begin] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)

(define-key global-map "\C-z" 'iconify-frame)

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

;; Eventually these will tell read-char how to convert
;; these special chars to ASCII.
(put 'backspace 'ascii-character 127)
(put 'delete 'ascii-character 127)
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)

;;;; Selections and cut buffers

;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-cut-buffer-or-selection-value.
(defvar x-last-selected-text nil)

;;; Make TEXT, a string, the primary and clipboard X selections.
;;; If you are running xclipboard, this means you can effectively
;;; have a window on a copy of the kill-ring.
;;; Also, set the value of X cut buffer 0, for backward compatibility
;;; with older X applications.
(defun x-select-text (text)
  (x-set-cut-buffer 0 text)
  (x-set-selection 'clipboard text)
  (x-set-selection 'primary text)
  (setq x-last-selected-text text))

;;; Return the value of the current X selection.  For compatibility
;;; with older X applications, this checks cut buffer 0 before
;;; retrieving the value of the primary selection.
(defun x-cut-buffer-or-selection-value ()
  (let (text)

    ;; Consult the cut buffer, then the selection.  Treat empty strings
    ;; as if they were unset.
    (setq text (x-get-cut-buffer 0))
    (if (string= text "") (setq text nil))
    (or text (setq text (x-selection 'primary)))
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
(x-open-connection (or x-display-name
		       (setq x-display-name (getenv "DISPLAY"))))

(setq frame-creation-function 'x-create-frame)

(defun x-win-suspend-error ()
  (error "Suspending an emacs running under X makes no sense"))
(add-hook 'suspend-hooks 'x-win-suspend-error)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; Turn off window-splitting optimization; X is usually fast enough
;;; that this is only annoying.
(setq split-window-keep-point t)
