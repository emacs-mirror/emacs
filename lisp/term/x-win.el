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
;; -title		.title
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

(setq command-switch-alist
      (append '(("-dm" . x-establish-daemon-mode)
		("-bw" . x-handle-numeric-switch)
		("-d" . x-handle-display)
		("-display" . x-handle-display)
		("-name" . x-handle-switch)
		("-T" . x-handle-switch)
		("-r" . x-handle-switch)
		("-rv" . x-handle-switch)
		("-reverse" . x-handle-switch)
		("-fn" . x-handle-switch)
		("-font" . x-handle-switch)
		("-ib" . x-handle-switch)
		("-g" . x-handle-geometry)
		("-geometry" . x-handle-geometry)
		("-fg" . x-handle-switch)
		("-foreground" . x-handle-switch)
		("-bg" . x-handle-switch)
		("-background" . x-handle-switch)
		("-ms" . x-handle-switch)
		("-ib" . x-handle-switch)
		("-iconic" . x-handle-switch)
		("-cr" . x-handle-switch)
		("-vb" . x-handle-switch)
		("-hb" . x-handle-switch)
		("-bd" . x-handle-switch))
	      command-switch-alist))

(defvar x-switches-specified nil)

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
    ("-ib" icon-type t)
    ("-iconic" iconic-startup t)
    ("-vb" vertical-scroll-bar t)
    ("-hb" horizontal-scroll-bar t)
    ("-bd" border-color)
    ("-bw" border-width)))

;; Handler for switches of the form "-switch value" or "-switch".
(defun x-handle-switch (switch)
  (let ((aelt (assoc switch x-switch-definitions)))
    (if aelt
	(if (nth 2 aelt)
	    (setq x-switches-specified
		  (cons (cons (nth 1 aelt) (nth 2 aelt))
			x-switches-specified))
	  (setq x-switches-specified
		(cons (cons (nth 1 aelt)
			    (car x-invocation-args))
		      x-switches-specified)
		x-invocation-args (cdr x-invocation-args))))))

;; Handler for switches of the form "-switch n"
(defun x-handle-numeric-switch (switch)
  (let ((aelt (assoc switch x-switch-definitions)))
    (if aelt
	(setq x-switches-specified
	      (cons (cons (nth 1 aelt)
			  (string-to-int (car x-invocation-args)))
		    x-switches-specified)
	      x-invocation-args
	      (cdr x-invocation-args)))))

;; Handle the geometry option
(defun x-handle-geometry (switch)
  (setq x-switches-specified (append x-switches-specified
				     (x-geometry (car x-invocation-args)))
	x-invocation-args (cdr x-invocation-args)))

;; The daemon stuff isn't really useful at the moment.
(defvar x-daemon-mode nil
  "When set, means initially create just a minibuffer.")
	  
(defun x-establish-daemon-mode (switch)
  (setq x-daemon-mode t))

(defvar x-display-name nil
  "The X display name specifying server and X screen.")

(defun x-handle-display (switch)
  (setq x-display-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args)))

;; Here the X-related command line options are processed, before the user's
;; startup file is loaded.  These  are present in ARGS (see startup.el).
;; They are copied to x-invocation args from which the X-related things
;; are extracted, first the switch (e.g., "-fg") in the following code,
;; and possible values (e.g., "black") in the option handler code
;; (e.g., x-handle-switch).

;; When finished, only things not pertaining to X (e.g., "-q", filenames)
;; are left in ARGS

(defvar x-invocation-args nil)

(if (eq window-system 'x)
    (progn
      (setq window-setup-hook 'x-pop-initial-window
	    x-invocation-args args
	    args nil)
      (require 'x-mouse)
      (require 'screen)
      (setq suspend-hook
	    '(lambda ()
	       (error "Suspending an emacs running under X makes no sense")))
      (define-key global-map "" 'iconify-emacs)
      (while x-invocation-args
	(let* ((this-switch (car x-invocation-args))
	       (aelt (assoc this-switch command-switch-alist)))
	  (setq x-invocation-args (cdr x-invocation-args))
	  (if aelt
	      (funcall (cdr aelt) this-switch)
	    (setq args (cons this-switch args)))))
      (setq args (nreverse args))
      (x-open-connection (or x-display-name
			     (setq x-display-name (getenv "DISPLAY"))))
      ;;
      ;; This is the place to handle Xresources
      ;;
      )
  (error "Loading x-win.el but not compiled for X"))


;; This is the function which creates the first X window.  It is called
;; from startup.el after the user's init file is processed.

(defun x-pop-initial-window ()
  ;; xterm.c depends on using interrupt-driven input.
  (set-input-mode t nil t)
  ;; (setq mouse-motion-handler 'x-track-pointer)
  (setq x-switches-specified (append x-switches-specified
				     initial-screen-alist
				     screen-default-alist))
  (and (string-equal (user-real-login-name) "mtr")
       (setq x-switches-specified
	     (append (list '(name . "Die, Yuppie SCUM!!!"))
		     x-switches-specified)))
  ;; see screen.el for this function
  (pop-initial-screen x-switches-specified)
  (delete-screen terminal-screen))


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


;;
;; Convenience functions for dynamically changing screen parameters
;;

(defun x-set-default-font (font-name)
  (interactive "sFont name: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'font font-name))))

(defun x-set-background (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'background-color color-name))))

(defun x-set-foreground (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'foreground-color color-name))))

(defun x-set-cursor (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'cursor-color color-name))))

(defun x-set-mouse (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'mouse-color color-name))))

(defun x-set-mouse-shape (shape)
  (interactive "sShape: ")
  (setq x-pointer-shape (eval (intern shape)))
  (modify-screen-parameters (selected-screen)
			    (list (assoc 'mouse-color (screen-parameters)))))

(defun x-set-border (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'border-color color-name))))

(defun x-set-name (name)
  (interactive "sName: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'name name))))

(defun x-set-auto-raise (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'auto-raise toggle))))

(defun x-set-auto-lower (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'auto-lower toggle))))

(defun x-set-vertical-bar (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'vertical-scroll-bar toggle))))

(defun x-set-horizontal-bar (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'horizontal-scroll-bar toggle))))

;;
;; Function key processing under X.  Function keys are received through
;; in the input stream as Lisp symbols.
;;

(defun define-function-key (map sym definition)
  (let ((exist (assq sym (cdr map))))
    (if exist
	(setcdr exist definition)
      (setcdr map
	      (cons (cons sym definition)
		    (cdr map))))))

;; For unused keysyms.  If this happens, it's probably a server or
;; Xlib bug.

(defun weird-x-keysym ()
  (interactive)
  (error "Bizarre X keysym received."))
(define-function-key global-function-map 'xk-not-serious 'weird-x-keysym)

;; Keypad type things

(define-function-key global-function-map 'xk-home 'beginning-of-line)
(define-function-key global-function-map 'xk-left 'backward-char)
(define-function-key global-function-map 'xk-up 'previous-line)
(define-function-key global-function-map 'xk-right 'forward-char)
(define-function-key global-function-map 'xk-down 'next-line)
(define-function-key global-function-map 'xk-prior 'previous-line)
(define-function-key global-function-map 'xk-next 'next-line)
(define-function-key global-function-map 'xk-end 'end-of-line)
(define-function-key global-function-map 'xk-begin 'beginning-of-line)

 ;;  IsMiscFunctionKey 

(define-function-key global-function-map 'xk-select nil)
(define-function-key global-function-map 'xk-print nil)
(define-function-key global-function-map 'xk-execute nil)
(define-function-key global-function-map 'xk-insert nil)
(define-function-key global-function-map 'xk-undo nil)
(define-function-key global-function-map 'xk-redo nil)
(define-function-key global-function-map 'xk-menu nil)
(define-function-key global-function-map 'xk-find nil)
(define-function-key global-function-map 'xk-cancel nil)
(define-function-key global-function-map 'xk-help nil)
(define-function-key global-function-map 'xk-break nil)

 ;;  IsKeypadKey 

(define-function-key global-function-map 'xk-kp-space
  '(lambda nil (interactive)
     (insert " ")))
(define-function-key global-function-map 'xk-kp-tab
  '(lambda nil (interactive)
     (insert "\t")))
(define-function-key global-function-map 'xk-kp-enter
  '(lambda nil (interactive)
     (insert "\n")))

(define-function-key global-function-map 'xk-kp-f1 'rmail)
(define-function-key global-function-map 'xk-kp-f2 nil)
(define-function-key global-function-map 'xk-kp-f3 nil)
(define-function-key global-function-map 'xk-kp-f4 nil)

(define-function-key global-function-map 'xk-kp-equal
  '(lambda nil (interactive)
     (insert "=")))
(define-function-key global-function-map 'xk-kp-multiply
  '(lambda nil (interactive)
     (insert "*")))
(define-function-key global-function-map 'xk-kp-add
  '(lambda nil (interactive)
     (insert "+")))
(define-function-key global-function-map 'xk-kp-separator
  '(lambda nil (interactive)
     (insert ";")))
(define-function-key global-function-map 'xk-kp-subtract
  '(lambda nil (interactive)
     (insert "-")))
(define-function-key global-function-map 'xk-kp-decimal
  '(lambda nil (interactive)
     (insert ".")))
(define-function-key global-function-map 'xk-kp-divide
  '(lambda nil (interactive)
     (insert "/")))

(define-function-key global-function-map 'xk-kp-0
  '(lambda nil (interactive)
     (insert "0")))
(define-function-key global-function-map 'xk-kp-1
  '(lambda nil (interactive)
     (insert "1")))
(define-function-key global-function-map 'xk-kp-2
  '(lambda nil (interactive)
     (insert "2")))
(define-function-key global-function-map 'xk-kp-3
  '(lambda nil (interactive)
     (insert "3")))
(define-function-key global-function-map 'xk-kp-4
  '(lambda nil (interactive)
     (insert "4")))
(define-function-key global-function-map 'xk-kp-5
  '(lambda nil (interactive)
     (insert "5")))
(define-function-key global-function-map 'xk-kp-6
  '(lambda nil (interactive)
     (insert "6")))
(define-function-key global-function-map 'xk-kp-7
  '(lambda nil (interactive)
     (insert "7")))
(define-function-key global-function-map 'xk-kp-8
  '(lambda nil (interactive)
     (insert "8")))
(define-function-key global-function-map 'xk-kp-9
  '(lambda nil (interactive)
     (insert "9")))

 ;;  IsFunctionKey 

(define-function-key global-function-map 'xk-f1 'rmail)
(define-function-key global-function-map 'xk-f2 nil)
(define-function-key global-function-map 'xk-f3 nil)
(define-function-key global-function-map 'xk-f4 nil)
(define-function-key global-function-map 'xk-f5 nil)
(define-function-key global-function-map 'xk-f6 nil)
(define-function-key global-function-map 'xk-f7 nil)
(define-function-key global-function-map 'xk-f8 nil)
(define-function-key global-function-map 'xk-f9 nil)
(define-function-key global-function-map 'xk-f10 nil)
(define-function-key global-function-map 'xk-f11 nil)
(define-function-key global-function-map 'xk-f12 nil)
(define-function-key global-function-map 'xk-f13 nil)
(define-function-key global-function-map 'xk-f14 nil)
(define-function-key global-function-map 'xk-f15 nil)
(define-function-key global-function-map 'xk-f16 nil)
(define-function-key global-function-map 'xk-f17 nil)
(define-function-key global-function-map 'xk-f18 nil)
(define-function-key global-function-map 'xk-f19 nil)
(define-function-key global-function-map 'xk-f20 nil)
(define-function-key global-function-map 'xk-f21 nil)
(define-function-key global-function-map 'xk-f22 nil)
(define-function-key global-function-map 'xk-f23 nil)
(define-function-key global-function-map 'xk-f24 nil)
(define-function-key global-function-map 'xk-f25 nil)
(define-function-key global-function-map 'xk-f26 nil)
(define-function-key global-function-map 'xk-f27 nil)
(define-function-key global-function-map 'xk-f28 nil)
(define-function-key global-function-map 'xk-f29 nil)
(define-function-key global-function-map 'xk-f30 nil)
(define-function-key global-function-map 'xk-f31 nil)
(define-function-key global-function-map 'xk-f32 nil)
(define-function-key global-function-map 'xk-f33 nil)
(define-function-key global-function-map 'xk-f34 nil)
(define-function-key global-function-map 'xk-f35 nil)
