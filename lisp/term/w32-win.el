;;; w32-win.el --- parse switches controlling interface with W32 window system

;; Copyright (C) 1993, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Kevin Gallo
;; Keywords: terminals

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; w32-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that W32 windows are to be used.  Command line switches are parsed and those
;; pertaining to W32 are processed and removed from the command line.  The
;; W32 display is opened and hooks are set for popping up the initial window.

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

;; (if (not (eq window-system 'w32))
;;     (error "%s: Loading w32-win.el but not compiled for w32" (invocation-name)))

(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'dnd)
(require 'w32-vars)

;; Keep an obsolete alias for w32-focus-frame in case it is used by code
;; outside Emacs.
(define-obsolete-function-alias 'w32-focus-frame 'x-focus-frame "23.1")

(defvar xlfd-regexp-registry-subnum)
(defvar w32-color-map) ;; defined in w32fns.c

(declare-function w32-send-sys-command "w32fns.c")
(declare-function w32-select-font "w32fns.c")
(declare-function set-message-beep "w32console.c")

;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
(if (fboundp 'new-fontset)
    (require 'fontset))

;; The following definition is used for debugging scroll bar events.
;(defun w32-handle-scroll-bar-event (event) (interactive "e") (princ event))

(defun w32-drag-n-drop-debug (event)
  "Print the drag-n-drop EVENT in a readable form."
  (interactive "e")
  (princ event))

(defun w32-drag-n-drop (event)
  "Edit the files listed in the drag-n-drop EVENT.
Switch to a buffer editing the last file dropped."
  (interactive "e")
  (save-excursion
    ;; Make sure the drop target has positive co-ords
    ;; before setting the selected frame - otherwise it
    ;; won't work.  <skx@tardis.ed.ac.uk>
    (let* ((window (posn-window (event-start event)))
	   (coords (posn-x-y (event-start event)))
	   (x (car coords))
	   (y (cdr coords)))
      (if (and (> x 0) (> y 0))
	  (set-frame-selected-window nil window))
      (mapc (lambda (file-name)
		(let ((f (subst-char-in-string ?\\ ?/ file-name))
		      (coding (or file-name-coding-system
				  default-file-name-coding-system)))
		  (setq file-name
			(mapconcat 'url-hexify-string
				   (split-string (encode-coding-string f coding)
						 "/")
				   "/")))
		(dnd-handle-one-url window 'private
				    (concat "file:" file-name)))
		(car (cdr (cdr event)))))
  (raise-frame)))

(defun w32-drag-n-drop-other-frame (event)
  "Edit the files listed in the drag-n-drop EVENT, in other frames.
May create new frames, or reuse existing ones.  The frame editing
the last file dropped is selected."
  (interactive "e")
  (mapcar 'find-file-other-frame (car (cdr (cdr event)))))

;; Bind the drag-n-drop event.
(global-set-key [drag-n-drop] 'w32-drag-n-drop)
(global-set-key [C-drag-n-drop] 'w32-drag-n-drop-other-frame)

;; Keyboard layout/language change events
;; For now ignore language-change events; in the future
;; we should switch the Emacs Input Method to match the
;; new layout/language selected by the user.
(global-set-key [language-change] 'ignore)

(defvar x-invocation-args)

(defvar x-command-line-resources nil)

(defun x-handle-switch (switch)
  "Handle SWITCH of the form \"-switch value\" or \"-switch\"."
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(push (cons (nth 3 aelt) (or (nth 4 aelt) (pop x-invocation-args)))
	      default-frame-alist))))

(defun x-handle-numeric-switch (switch)
  "Handle SWITCH of the form \"-switch n\"."
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(push (cons (nth 3 aelt) (string-to-number (pop x-invocation-args)))
	      default-frame-alist))))

;; Handle options that apply to initial frame only
(defun x-handle-initial-switch (switch)
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(push (cons (nth 3 aelt) (or (nth 4 aelt) (pop x-invocation-args)))
	      initial-frame-alist))))

(defun x-handle-iconic (switch)
  "Make \"-iconic\" SWITCH apply only to the initial frame."
  (push '(visibility . icon) initial-frame-alist))

(defun x-handle-xrm-switch (switch)
  "Handle the \"-xrm\" SWITCH."
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-command-line-resources
	(if (null x-command-line-resources)
	    (car x-invocation-args)
	  (concat x-command-line-resources "\n" (car x-invocation-args))))
  (setq x-invocation-args (cdr x-invocation-args)))

(defun x-handle-geometry (switch)
  "Handle the \"-geometry\" SWITCH."
  (let* ((geo (x-parse-geometry (car x-invocation-args)))
	 (left (assq 'left geo))
	 (top (assq 'top geo))
	 (height (assq 'height geo))
	 (width (assq 'width geo)))
    (if (or height width)
	(setq default-frame-alist
	      (append default-frame-alist
		      '((user-size . t))
		      (if height (list height))
		      (if width (list width)))
	      initial-frame-alist
	      (append initial-frame-alist
		      '((user-size . t))
		      (if height (list height))
		      (if width (list width)))))
    (if (or left top)
	(setq initial-frame-alist
	      (append initial-frame-alist
		      '((user-position . t))
		      (if left (list left))
		      (if top (list top)))))
    (setq x-invocation-args (cdr x-invocation-args))))

(defun x-handle-name-switch (switch)
  "Handle the \"-name\" SWITCH."
;; Handle the -name option.  Set the variable x-resource-name
;; to the option's operand; set the name of the initial frame, too.
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-resource-name (pop x-invocation-args))
  (push (cons 'name x-resource-name) initial-frame-alist))

(defvar x-display-name nil
  "The display name specifying server and frame.")

(defun x-handle-display (switch)
  "Handle the \"-display\" SWITCH."
  (setq x-display-name (pop x-invocation-args)))

(defun x-handle-args (args)
  "Process the X-related command line options in ARGS.
This is done before the user's startup file is loaded.  They are copied to
`x-invocation args' from which the X-related things are extracted, first
the switch (e.g., \"-fg\") in the following code, and possible values
\(e.g., \"black\") in the option handler code (e.g., x-handle-switch).
This returns ARGS with the arguments that have been processed removed."
  ;; We use ARGS to accumulate the args that we don't handle here, to return.
  (setq x-invocation-args args
	args nil)
  (while (and x-invocation-args
	      (not (equal (car x-invocation-args) "--")))
    (let* ((this-switch (car x-invocation-args))
	   (orig-this-switch this-switch)
	   completion argval aelt handler)
      (setq x-invocation-args (cdr x-invocation-args))
      ;; Check for long options with attached arguments
      ;; and separate out the attached option argument into argval.
      (if (string-match "^--[^=]*=" this-switch)
	  (setq argval (substring this-switch (match-end 0))
		this-switch (substring this-switch 0 (1- (match-end 0)))))
      ;; Complete names of long options.
      (if (string-match "^--" this-switch)
	  (progn
	    (setq completion (try-completion this-switch command-line-x-option-alist))
	    (if (eq completion t)
		;; Exact match for long option.
		nil
	      (if (stringp completion)
		  (let ((elt (assoc completion command-line-x-option-alist)))
		    ;; Check for abbreviated long option.
		    (or elt
			(error "Option `%s' is ambiguous" this-switch))
		    (setq this-switch completion))))))
      (setq aelt (assoc this-switch command-line-x-option-alist))
      (if aelt (setq handler (nth 2 aelt)))
      (if handler
	  (if argval
	      (let ((x-invocation-args
		     (cons argval x-invocation-args)))
		(funcall handler this-switch))
	    (funcall handler this-switch))
	(push orig-this-switch args))))
  (nconc (nreverse args) x-invocation-args))

;;
;; Available colors
;;
;; The ordering of the colors is chosen for the user's convenience in
;; `list-colors-display', which displays the reverse of this list.
;; Roughly speaking, `list-colors-display' orders by (i) named shades
;; of grey with hue 0.0, sorted by value (ii) named colors with
;; saturation 1.0, sorted by hue, (iii) named non-white colors with
;; saturation less than 1.0, sorted by hue, (iv) other named shades of
;; white, (v) numbered colors sorted by hue, and (vi) numbered shades
;; of grey.

(defvar x-colors
  '("gray100" "gray99" "gray98" "gray97" "gray96" "gray95" "gray94" "gray93" "gray92"
    "gray91" "gray90" "gray89" "gray88" "gray87" "gray86" "gray85" "gray84" "gray83"
    "gray82" "gray81" "gray80" "gray79" "gray78" "gray77" "gray76" "gray75" "gray74"
    "gray73" "gray72" "gray71" "gray70" "gray69" "gray68" "gray67" "gray66" "gray65"
    "gray64" "gray63" "gray62" "gray61" "gray60" "gray59" "gray58" "gray57" "gray56"
    "gray55" "gray54" "gray53" "gray52" "gray51" "gray50" "gray49" "gray48" "gray47"
    "gray46" "gray45" "gray44" "gray43" "gray42" "gray41" "gray40" "gray39" "gray38"
    "gray37" "gray36" "gray35" "gray34" "gray33" "gray32" "gray31" "gray30" "gray29"
    "gray28" "gray27" "gray26" "gray25" "gray24" "gray23" "gray22" "gray21" "gray20"
    "gray19" "gray18" "gray17" "gray16" "gray15" "gray14" "gray13" "gray12" "gray11"
    "gray10" "gray9" "gray8" "gray7" "gray6" "gray5" "gray4" "gray3" "gray2" "gray1"
    "gray0" "LightPink1" "LightPink2" "LightPink3" "LightPink4" "pink1" "pink2" "pink3"
    "pink4" "PaleVioletRed1" "PaleVioletRed2" "PaleVioletRed3" "PaleVioletRed4"
    "LavenderBlush1" "LavenderBlush2" "LavenderBlush3" "LavenderBlush4" "VioletRed1"
    "VioletRed2" "VioletRed3" "VioletRed4" "HotPink1" "HotPink2" "HotPink3" "HotPink4"
    "DeepPink1" "DeepPink2" "DeepPink3" "DeepPink4" "maroon1" "maroon2" "maroon3"
    "maroon4" "orchid1" "orchid2" "orchid3" "orchid4" "plum1" "plum2" "plum3" "plum4"
    "thistle1" "thistle2" "thistle3" "thistle4" "MediumOrchid1" "MediumOrchid2"
    "MediumOrchid3" "MediumOrchid4" "DarkOrchid1" "DarkOrchid2" "DarkOrchid3"
    "DarkOrchid4" "purple1" "purple2" "purple3" "purple4" "MediumPurple1"
    "MediumPurple2" "MediumPurple3" "MediumPurple4" "SlateBlue1" "SlateBlue2"
    "SlateBlue3" "SlateBlue4" "RoyalBlue1" "RoyalBlue2" "RoyalBlue3" "RoyalBlue4"
    "LightSteelBlue1" "LightSteelBlue2" "LightSteelBlue3" "LightSteelBlue4" "SlateGray1"
    "SlateGray2" "SlateGray3" "SlateGray4" "DodgerBlue1" "DodgerBlue2" "DodgerBlue3"
    "DodgerBlue4" "SteelBlue1" "SteelBlue2" "SteelBlue3" "SteelBlue4" "SkyBlue1"
    "SkyBlue2" "SkyBlue3" "SkyBlue4" "LightSkyBlue1" "LightSkyBlue2" "LightSkyBlue3"
    "LightSkyBlue4" "LightBlue1" "LightBlue2" "LightBlue3" "LightBlue4" "CadetBlue1"
    "CadetBlue2" "CadetBlue3" "CadetBlue4" "azure1" "azure2" "azure3" "azure4"
    "LightCyan1" "LightCyan2" "LightCyan3" "LightCyan4" "PaleTurquoise1"
    "PaleTurquoise2" "PaleTurquoise3" "PaleTurquoise4" "DarkSlateGray1" "DarkSlateGray2"
    "DarkSlateGray3" "DarkSlateGray4" "aquamarine1" "aquamarine2" "aquamarine3"
    "aquamarine4" "SeaGreen1" "SeaGreen2" "SeaGreen3" "SeaGreen4" "honeydew1"
    "honeydew2" "honeydew3" "honeydew4" "DarkSeaGreen1" "DarkSeaGreen2" "DarkSeaGreen3"
    "DarkSeaGreen4" "PaleGreen1" "PaleGreen2" "PaleGreen3" "PaleGreen4"
    "DarkOliveGreen1" "DarkOliveGreen2" "DarkOliveGreen3" "DarkOliveGreen4" "OliveDrab1"
    "OliveDrab2" "OliveDrab3" "OliveDrab4" "ivory1" "ivory2" "ivory3" "ivory4"
    "LightYellow1" "LightYellow2" "LightYellow3" "LightYellow4" "khaki1" "khaki2"
    "khaki3" "khaki4" "LemonChiffon1" "LemonChiffon2" "LemonChiffon3" "LemonChiffon4"
    "LightGoldenrod1" "LightGoldenrod2" "LightGoldenrod3" "LightGoldenrod4" "cornsilk1"
    "cornsilk2" "cornsilk3" "cornsilk4" "goldenrod1" "goldenrod2" "goldenrod3"
    "goldenrod4" "DarkGoldenrod1" "DarkGoldenrod2" "DarkGoldenrod3" "DarkGoldenrod4"
    "wheat1" "wheat2" "wheat3" "wheat4" "NavajoWhite1" "NavajoWhite2" "NavajoWhite3"
    "NavajoWhite4" "burlywood1" "burlywood2" "burlywood3" "burlywood4" "AntiqueWhite1"
    "AntiqueWhite2" "AntiqueWhite3" "AntiqueWhite4" "bisque1" "bisque2" "bisque3"
    "bisque4" "tan1" "tan2" "tan3" "tan4" "PeachPuff1" "PeachPuff2" "PeachPuff3"
    "PeachPuff4" "seashell1" "seashell2" "seashell3" "seashell4" "chocolate1"
    "chocolate2" "chocolate3" "chocolate4" "sienna1" "sienna2" "sienna3" "sienna4"
    "LightSalmon1" "LightSalmon2" "LightSalmon3" "LightSalmon4" "salmon1" "salmon2"
    "salmon3" "salmon4" "coral1" "coral2" "coral3" "coral4" "tomato1" "tomato2"
    "tomato3" "tomato4" "MistyRose1" "MistyRose2" "MistyRose3" "MistyRose4" "snow1"
    "snow2" "snow3" "snow4" "RosyBrown1" "RosyBrown2" "RosyBrown3" "RosyBrown4"
    "IndianRed1" "IndianRed2" "IndianRed3" "IndianRed4" "firebrick1" "firebrick2"
    "firebrick3" "firebrick4" "brown1" "brown2" "brown3" "brown4" "magenta1" "magenta2"
    "magenta3" "magenta4" "blue1" "blue2" "blue3" "blue4" "DeepSkyBlue1" "DeepSkyBlue2"
    "DeepSkyBlue3" "DeepSkyBlue4" "turquoise1" "turquoise2" "turquoise3" "turquoise4"
    "cyan1" "cyan2" "cyan3" "cyan4" "SpringGreen1" "SpringGreen2" "SpringGreen3"
    "SpringGreen4" "green1" "green2" "green3" "green4" "chartreuse1" "chartreuse2"
    "chartreuse3" "chartreuse4" "yellow1" "yellow2" "yellow3" "yellow4" "gold1" "gold2"
    "gold3" "gold4" "orange1" "orange2" "orange3" "orange4" "DarkOrange1" "DarkOrange2"
    "DarkOrange3" "DarkOrange4" "OrangeRed1" "OrangeRed2" "OrangeRed3" "OrangeRed4"
    "red1" "red2" "red3" "red4" "lavender blush" "ghost white" "lavender" "alice blue"
    "azure" "light cyan" "mint cream" "honeydew" "ivory" "light goldenrod yellow"
    "light yellow" "beige" "floral white" "old lace" "blanched almond" "moccasin"
    "papaya whip" "bisque" "antique white" "linen" "peach puff" "seashell" "misty rose"
    "snow" "light pink" "pink" "hot pink" "deep pink" "maroon" "pale violet red"
    "violet red" "medium violet red" "violet" "plum" "thistle" "orchid" "medium orchid"
    "dark orchid" "purple" "blue violet" "medium purple" "light slate blue"
    "medium slate blue" "slate blue" "dark slate blue" "midnight blue" "navy"
    "dark blue" "light steel blue" "cornflower blue" "dodger blue" "royal blue"
    "light slate gray" "slate gray" "dark slate gray" "steel blue" "cadet blue"
    "light sky blue" "sky blue" "light blue" "powder blue" "pale turquoise" "turquoise"
    "medium turquoise" "dark cyan" "aquamarine" "medium aquamarine" "light sea green"
    "medium sea green" "sea green" "dark sea green" "pale green" "lime green"
    "forest green" "light green" "green yellow" "yellow green" "olive drab"
    "dark olive green" "lemon chiffon" "khaki" "dark khaki" "cornsilk"
    "pale goldenrod" "light goldenrod" "goldenrod" "dark goldenrod" "wheat"
    "navajo white" "tan" "burlywood" "sandy brown" "peru" "chocolate" "saddle brown"
    "sienna" "rosy brown" "dark salmon" "coral" "tomato" "light salmon" "salmon"
    "light coral" "indian red" "firebrick" "brown" "dark red" "magenta"
    "dark magenta" "dark violet" "medium blue" "blue" "deep sky blue"
    "cyan" "medium spring green" "spring green" "green" "lawn green" "chartreuse"
    "yellow" "gold" "orange" "dark orange" "orange red" "red" "white" "white smoke"
    "gainsboro" "light grey" "gray" "dark grey" "dim gray" "black" )
  "The list of X colors from the `rgb.txt' file.
XConsortium: rgb.txt,v 10.41 94/02/20 18:39:36 rws Exp")

(defun xw-defined-colors (&optional frame)
  "Internal function called by `defined-colors', which see."
  (or frame (setq frame (selected-frame)))
  (let ((defined-colors nil))
    (dolist (this-color (or (mapcar 'car w32-color-map) x-colors))
      (and (color-supported-p this-color frame t)
	   (push this-color defined-colors)))
    defined-colors))


;;;; Function keys

 ;;; make f10 activate the real menubar rather than the mini-buffer menu
 ;;; navigation feature.
 (defun menu-bar-open (&optional frame)
   "Start key navigation of the menu bar in FRAME.
 
 This initially activates the first menu-bar item, and you can then navigate
 with the arrow keys, select a menu entry with the Return key or cancel with
 the Escape key.  If FRAME has no menu bar, this function does nothing.
 
 If FRAME is nil or not given, use the selected frame."
   (interactive "i")
   (w32-send-sys-command ?\xf100 frame))


;; W32 systems have different fonts than commonly found on X, so
;; we define our own standard fontset here.
(defvar w32-standard-fontset-spec
 "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-fontset-standard"
 "String of fontset spec of the standard fontset.
This defines a fontset consisting of the Courier New variations for
European languages which are distributed with Windows as
\"Multilanguage Support\".

See the documentation of `create-fontset-from-fontset-spec' for the format.")

(defun x-win-suspend-error ()
  "Report an error when a suspend is attempted."
  (error "Suspending an Emacs running under W32 makes no sense"))


(defun mouse-set-font (&rest fonts)
  "Select an Emacs font from a list of known good fonts and fontsets.

If `w32-use-w32-font-dialog' is non-nil (the default), use the Windows
font dialog to display the list of possible fonts.  Otherwise use a
pop-up menu (like Emacs does on other platforms) initialized with
the fonts in `w32-fixed-font-alist'.
If `w32-list-proportional-fonts' is non-nil, add proportional fonts
to the list in the font selection dialog (the fonts listed by the
pop-up menu are unaffected by `w32-list-proportional-fonts')."
  (interactive
   (if w32-use-w32-font-dialog
       (let ((chosen-font (w32-select-font (selected-frame)
					   w32-list-proportional-fonts)))
	 (and chosen-font (list chosen-font)))
     (x-popup-menu
      last-nonmenu-event
      ;; Append list of fontsets currently defined.
      ;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
      (if (fboundp 'new-fontset)
          (append w32-fixed-font-alist (list (generate-fontset-menu)))))))
  (if fonts
      (let (font)
	(while fonts
	  (condition-case nil
	      (progn
                (setq font (car fonts))
		(set-default-font font)
                (setq fonts nil))
	    (error (setq fonts (cdr fonts)))))
	(if (null font)
	    (error "Font not found")))))

;;; Set default known names for image libraries
(setq image-library-alist
      '((xpm "libxpm.dll" "xpm4.dll" "libXpm-nox4.dll")
        (png "libpng12d.dll" "libpng12.dll" "libpng.dll"
	 ;; these are libpng 1.2.8 from GTK+
	 "libpng13d.dll" "libpng13.dll")
        (jpeg "jpeg62.dll" "libjpeg.dll" "jpeg-62.dll" "jpeg.dll")
        (tiff "libtiff3.dll" "libtiff.dll")
        (gif "giflib4.dll" "libungif4.dll" "libungif.dll")
        (svg "librsvg-2-2.dll")
        (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
        (glib "libglib-2.0-0.dll")))

;;; multi-tty support
(defvar w32-initialized nil
  "Non-nil if the w32 window system has been initialized.")

(defun w32-initialize-window-system ()
  "Initialize Emacs for W32 GUI frames."

  ;; Do the actual Windows setup here; the above code just defines
  ;; functions and variables that we use now.

  (setq command-line-args (x-handle-args command-line-args))

  ;; Make sure we have a valid resource name.
  (or (stringp x-resource-name)
      (setq x-resource-name
            ;; Change any . or * characters in x-resource-name to hyphens,
            ;; so as not to choke when we use it in X resource queries.
            (replace-regexp-in-string "[.*]" "-" (invocation-name))))

  (x-open-connection "" x-command-line-resources
                     ;; Exit with a fatal error if this fails and we
                     ;; are the initial display
                     (eq initial-window-system 'w32))

  ;; Setup the default fontset.
  (setup-default-fontset)

  ;; Enable Japanese fonts on Windows to be used by default.
  (set-fontset-font t (make-char 'katakana-jisx0201)
		    '("*" . "JISX0208-SJIS"))
  (set-fontset-font t (make-char 'latin-jisx0201)
		    '("*" . "JISX0208-SJIS"))
  (set-fontset-font t (make-char 'japanese-jisx0208)
		    '("*" . "JISX0208-SJIS"))
  (set-fontset-font t (make-char 'japanese-jisx0208-1978)
		    '("*" . "JISX0208-SJIS"))

  ;; Create the standard fontset.
  (create-fontset-from-fontset-spec w32-standard-fontset-spec t)
  ;; Create fontset specified in X resources "Fontset-N" (N is 0, 1,...).
  (create-fontset-from-x-resource)

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
              (push (cons 'height (cdr (assq 'height parsed)))
                    default-frame-alist))
          (if (assq 'width parsed)
              (push (cons 'width (cdr (assq 'width parsed)))
                    default-frame-alist)))))

  ;; Check the reverseVideo resource.
  (let ((case-fold-search t))
    (let ((rv (x-get-resource "reverseVideo" "ReverseVideo")))
      (if (and rv (string-match "^\\(true\\|yes\\|on\\)$" rv))
          (push '(reverse . t) default-frame-alist))))

  ;; Don't let Emacs suspend under w32 gui
  (add-hook 'suspend-hook 'x-win-suspend-error)

  ;; Turn off window-splitting optimization; w32 is usually fast enough
  ;; that this is only annoying.
  (setq split-window-keep-point t)

  ;; Turn on support for mouse wheels
  (mouse-wheel-mode 1)

  ;; W32 expects the menu bar cut and paste commands to use the clipboard.
  (menu-bar-enable-clipboard)

  ;; Don't show the frame name; that's redundant.
  (setq-default mode-line-frame-identification "  ")

  ;; Set to a system sound if you want a fancy bell.
  (set-message-beep 'ok)
  (setq w32-initialized t))

(add-to-list 'handle-args-function-alist '(w32 . x-handle-args))
(add-to-list 'frame-creation-function-alist '(w32 . x-create-frame-with-faces))
(add-to-list 'window-system-initialization-alist '(w32 . w32-initialize-window-system))

(provide 'w32-win)

;; arch-tag: 69fb1701-28c2-4890-b351-3d1fe4b4f166
;;; w32-win.el ends here
