;;; x-win.el --- parse relevant switches and set up for X  -*-coding: utf-8-emacs;-*-

;; Copyright (C) 1993, 1994, 2001, 2002 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals, i18n

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

;; X-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that X windows are to be used.  Command line switches are parsed and those
;; pertaining to X are processed and removed from the command line.  The
;; X display is opened and hooks are set for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window(s).

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
(require 'fontset)

(defvar x-invocation-args)

(defvar x-command-line-resources nil)

;; Handler for switches of the form "-switch value" or "-switch".
(defun x-handle-switch (switch)
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(let ((param (nth 3 aelt))
	      (value (nth 4 aelt)))
	  (if value
	      (setq default-frame-alist
		    (cons (cons param value)
			  default-frame-alist))
	    (setq default-frame-alist
		  (cons (cons param
			      (car x-invocation-args))
			default-frame-alist)
		  x-invocation-args (cdr x-invocation-args)))))))

;; Handler for switches of the form "-switch n"
(defun x-handle-numeric-switch (switch)
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(let ((param (nth 3 aelt)))
	  (setq default-frame-alist
		(cons (cons param
			    (string-to-int (car x-invocation-args)))
		      default-frame-alist)
		x-invocation-args
		(cdr x-invocation-args))))))

;; Handle options that apply to initial frame only
(defun x-handle-initial-switch (switch)
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(let ((param (nth 3 aelt))
	      (value (nth 4 aelt)))
	  (if value
	      (setq initial-frame-alist
		    (cons (cons param value)
			  initial-frame-alist))
	    (setq initial-frame-alist
		  (cons (cons param
			      (car x-invocation-args))
			initial-frame-alist)
		  x-invocation-args (cdr x-invocation-args)))))))

;; Make -iconic apply only to the initial frame!
(defun x-handle-iconic (switch)
  (setq initial-frame-alist
	(cons '(visibility . icon) initial-frame-alist)))

;; Handle the -xrm option.
(defun x-handle-xrm-switch (switch)
  (unless (consp x-invocation-args)
    (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-command-line-resources
	(if (null x-command-line-resources)
	    (car x-invocation-args)
	  (concat x-command-line-resources "\n" (car x-invocation-args))))
  (setq x-invocation-args (cdr x-invocation-args)))

;; Handle the geometry option
(defun x-handle-geometry (switch)
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

;; Handle the -name option.  Set the variable x-resource-name
;; to the option's operand; set the name of
;; the initial frame, too.
(defun x-handle-name-switch (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-resource-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args))
  (setq initial-frame-alist (cons (cons 'name x-resource-name)
				  initial-frame-alist)))

(defvar x-display-name nil
  "The X display name specifying server and X frame.")

(defun x-handle-display (switch)
  (setq x-display-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args))
  ;; Make subshell programs see the same DISPLAY value Emacs really uses.
  ;; Note that this isn't completely correct, since Emacs can use
  ;; multiple displays.  However, there is no way to tell an already
  ;; running subshell which display the user is currently typing on.
  (setenv "DISPLAY" x-display-name))

(defun x-handle-args (args)
  "Process the X-related command line options in ARGS.
This is done before the user's startup file is loaded.  They are copied to
`x-invocation-args', from which the X-related things are extracted, first
the switch (e.g., \"-fg\") in the following code, and possible values
\(e.g., \"black\") in the option handler code (e.g., x-handle-switch).
This function returns ARGS minus the arguments that have been processed."
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
	(setq args (cons orig-this-switch args)))))
  (nconc (nreverse args) x-invocation-args))

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

(defvar x-colors '("LightGreen"
		   "light green"
		   "DarkRed"
		   "dark red"
		   "DarkMagenta"
		   "dark magenta"
		   "DarkCyan"
		   "dark cyan"
		   "DarkBlue"
		   "dark blue"
		   "DarkGray"
		   "dark gray"
		   "DarkGrey"
		   "dark grey"
		   "grey100"
		   "gray100"
		   "grey99"
		   "gray99"
		   "grey98"
		   "gray98"
		   "grey97"
		   "gray97"
		   "grey96"
		   "gray96"
		   "grey95"
		   "gray95"
		   "grey94"
		   "gray94"
		   "grey93"
		   "gray93"
		   "grey92"
		   "gray92"
		   "grey91"
		   "gray91"
		   "grey90"
		   "gray90"
		   "grey89"
		   "gray89"
		   "grey88"
		   "gray88"
		   "grey87"
		   "gray87"
		   "grey86"
		   "gray86"
		   "grey85"
		   "gray85"
		   "grey84"
		   "gray84"
		   "grey83"
		   "gray83"
		   "grey82"
		   "gray82"
		   "grey81"
		   "gray81"
		   "grey80"
		   "gray80"
		   "grey79"
		   "gray79"
		   "grey78"
		   "gray78"
		   "grey77"
		   "gray77"
		   "grey76"
		   "gray76"
		   "grey75"
		   "gray75"
		   "grey74"
		   "gray74"
		   "grey73"
		   "gray73"
		   "grey72"
		   "gray72"
		   "grey71"
		   "gray71"
		   "grey70"
		   "gray70"
		   "grey69"
		   "gray69"
		   "grey68"
		   "gray68"
		   "grey67"
		   "gray67"
		   "grey66"
		   "gray66"
		   "grey65"
		   "gray65"
		   "grey64"
		   "gray64"
		   "grey63"
		   "gray63"
		   "grey62"
		   "gray62"
		   "grey61"
		   "gray61"
		   "grey60"
		   "gray60"
		   "grey59"
		   "gray59"
		   "grey58"
		   "gray58"
		   "grey57"
		   "gray57"
		   "grey56"
		   "gray56"
		   "grey55"
		   "gray55"
		   "grey54"
		   "gray54"
		   "grey53"
		   "gray53"
		   "grey52"
		   "gray52"
		   "grey51"
		   "gray51"
		   "grey50"
		   "gray50"
		   "grey49"
		   "gray49"
		   "grey48"
		   "gray48"
		   "grey47"
		   "gray47"
		   "grey46"
		   "gray46"
		   "grey45"
		   "gray45"
		   "grey44"
		   "gray44"
		   "grey43"
		   "gray43"
		   "grey42"
		   "gray42"
		   "grey41"
		   "gray41"
		   "grey40"
		   "gray40"
		   "grey39"
		   "gray39"
		   "grey38"
		   "gray38"
		   "grey37"
		   "gray37"
		   "grey36"
		   "gray36"
		   "grey35"
		   "gray35"
		   "grey34"
		   "gray34"
		   "grey33"
		   "gray33"
		   "grey32"
		   "gray32"
		   "grey31"
		   "gray31"
		   "grey30"
		   "gray30"
		   "grey29"
		   "gray29"
		   "grey28"
		   "gray28"
		   "grey27"
		   "gray27"
		   "grey26"
		   "gray26"
		   "grey25"
		   "gray25"
		   "grey24"
		   "gray24"
		   "grey23"
		   "gray23"
		   "grey22"
		   "gray22"
		   "grey21"
		   "gray21"
		   "grey20"
		   "gray20"
		   "grey19"
		   "gray19"
		   "grey18"
		   "gray18"
		   "grey17"
		   "gray17"
		   "grey16"
		   "gray16"
		   "grey15"
		   "gray15"
		   "grey14"
		   "gray14"
		   "grey13"
		   "gray13"
		   "grey12"
		   "gray12"
		   "grey11"
		   "gray11"
		   "grey10"
		   "gray10"
		   "grey9"
		   "gray9"
		   "grey8"
		   "gray8"
		   "grey7"
		   "gray7"
		   "grey6"
		   "gray6"
		   "grey5"
		   "gray5"
		   "grey4"
		   "gray4"
		   "grey3"
		   "gray3"
		   "grey2"
		   "gray2"
		   "grey1"
		   "gray1"
		   "grey0"
		   "gray0"
		   "thistle4"
		   "thistle3"
		   "thistle2"
		   "thistle1"
		   "MediumPurple4"
		   "MediumPurple3"
		   "MediumPurple2"
		   "MediumPurple1"
		   "purple4"
		   "purple3"
		   "purple2"
		   "purple1"
		   "DarkOrchid4"
		   "DarkOrchid3"
		   "DarkOrchid2"
		   "DarkOrchid1"
		   "MediumOrchid4"
		   "MediumOrchid3"
		   "MediumOrchid2"
		   "MediumOrchid1"
		   "plum4"
		   "plum3"
		   "plum2"
		   "plum1"
		   "orchid4"
		   "orchid3"
		   "orchid2"
		   "orchid1"
		   "magenta4"
		   "magenta3"
		   "magenta2"
		   "magenta1"
		   "VioletRed4"
		   "VioletRed3"
		   "VioletRed2"
		   "VioletRed1"
		   "maroon4"
		   "maroon3"
		   "maroon2"
		   "maroon1"
		   "PaleVioletRed4"
		   "PaleVioletRed3"
		   "PaleVioletRed2"
		   "PaleVioletRed1"
		   "LightPink4"
		   "LightPink3"
		   "LightPink2"
		   "LightPink1"
		   "pink4"
		   "pink3"
		   "pink2"
		   "pink1"
		   "HotPink4"
		   "HotPink3"
		   "HotPink2"
		   "HotPink1"
		   "DeepPink4"
		   "DeepPink3"
		   "DeepPink2"
		   "DeepPink1"
		   "red4"
		   "red3"
		   "red2"
		   "red1"
		   "OrangeRed4"
		   "OrangeRed3"
		   "OrangeRed2"
		   "OrangeRed1"
		   "tomato4"
		   "tomato3"
		   "tomato2"
		   "tomato1"
		   "coral4"
		   "coral3"
		   "coral2"
		   "coral1"
		   "DarkOrange4"
		   "DarkOrange3"
		   "DarkOrange2"
		   "DarkOrange1"
		   "orange4"
		   "orange3"
		   "orange2"
		   "orange1"
		   "LightSalmon4"
		   "LightSalmon3"
		   "LightSalmon2"
		   "LightSalmon1"
		   "salmon4"
		   "salmon3"
		   "salmon2"
		   "salmon1"
		   "brown4"
		   "brown3"
		   "brown2"
		   "brown1"
		   "firebrick4"
		   "firebrick3"
		   "firebrick2"
		   "firebrick1"
		   "chocolate4"
		   "chocolate3"
		   "chocolate2"
		   "chocolate1"
		   "tan4"
		   "tan3"
		   "tan2"
		   "tan1"
		   "wheat4"
		   "wheat3"
		   "wheat2"
		   "wheat1"
		   "burlywood4"
		   "burlywood3"
		   "burlywood2"
		   "burlywood1"
		   "sienna4"
		   "sienna3"
		   "sienna2"
		   "sienna1"
		   "IndianRed4"
		   "IndianRed3"
		   "IndianRed2"
		   "IndianRed1"
		   "RosyBrown4"
		   "RosyBrown3"
		   "RosyBrown2"
		   "RosyBrown1"
		   "DarkGoldenrod4"
		   "DarkGoldenrod3"
		   "DarkGoldenrod2"
		   "DarkGoldenrod1"
		   "goldenrod4"
		   "goldenrod3"
		   "goldenrod2"
		   "goldenrod1"
		   "gold4"
		   "gold3"
		   "gold2"
		   "gold1"
		   "yellow4"
		   "yellow3"
		   "yellow2"
		   "yellow1"
		   "LightYellow4"
		   "LightYellow3"
		   "LightYellow2"
		   "LightYellow1"
		   "LightGoldenrod4"
		   "LightGoldenrod3"
		   "LightGoldenrod2"
		   "LightGoldenrod1"
		   "khaki4"
		   "khaki3"
		   "khaki2"
		   "khaki1"
		   "DarkOliveGreen4"
		   "DarkOliveGreen3"
		   "DarkOliveGreen2"
		   "DarkOliveGreen1"
		   "OliveDrab4"
		   "OliveDrab3"
		   "OliveDrab2"
		   "OliveDrab1"
		   "chartreuse4"
		   "chartreuse3"
		   "chartreuse2"
		   "chartreuse1"
		   "green4"
		   "green3"
		   "green2"
		   "green1"
		   "SpringGreen4"
		   "SpringGreen3"
		   "SpringGreen2"
		   "SpringGreen1"
		   "PaleGreen4"
		   "PaleGreen3"
		   "PaleGreen2"
		   "PaleGreen1"
		   "SeaGreen4"
		   "SeaGreen3"
		   "SeaGreen2"
		   "SeaGreen1"
		   "DarkSeaGreen4"
		   "DarkSeaGreen3"
		   "DarkSeaGreen2"
		   "DarkSeaGreen1"
		   "aquamarine4"
		   "aquamarine3"
		   "aquamarine2"
		   "aquamarine1"
		   "DarkSlateGray4"
		   "DarkSlateGray3"
		   "DarkSlateGray2"
		   "DarkSlateGray1"
		   "cyan4"
		   "cyan3"
		   "cyan2"
		   "cyan1"
		   "turquoise4"
		   "turquoise3"
		   "turquoise2"
		   "turquoise1"
		   "CadetBlue4"
		   "CadetBlue3"
		   "CadetBlue2"
		   "CadetBlue1"
		   "PaleTurquoise4"
		   "PaleTurquoise3"
		   "PaleTurquoise2"
		   "PaleTurquoise1"
		   "LightCyan4"
		   "LightCyan3"
		   "LightCyan2"
		   "LightCyan1"
		   "LightBlue4"
		   "LightBlue3"
		   "LightBlue2"
		   "LightBlue1"
		   "LightSteelBlue4"
		   "LightSteelBlue3"
		   "LightSteelBlue2"
		   "LightSteelBlue1"
		   "SlateGray4"
		   "SlateGray3"
		   "SlateGray2"
		   "SlateGray1"
		   "LightSkyBlue4"
		   "LightSkyBlue3"
		   "LightSkyBlue2"
		   "LightSkyBlue1"
		   "SkyBlue4"
		   "SkyBlue3"
		   "SkyBlue2"
		   "SkyBlue1"
		   "DeepSkyBlue4"
		   "DeepSkyBlue3"
		   "DeepSkyBlue2"
		   "DeepSkyBlue1"
		   "SteelBlue4"
		   "SteelBlue3"
		   "SteelBlue2"
		   "SteelBlue1"
		   "DodgerBlue4"
		   "DodgerBlue3"
		   "DodgerBlue2"
		   "DodgerBlue1"
		   "blue4"
		   "blue3"
		   "blue2"
		   "blue1"
		   "RoyalBlue4"
		   "RoyalBlue3"
		   "RoyalBlue2"
		   "RoyalBlue1"
		   "SlateBlue4"
		   "SlateBlue3"
		   "SlateBlue2"
		   "SlateBlue1"
		   "azure4"
		   "azure3"
		   "azure2"
		   "azure1"
		   "MistyRose4"
		   "MistyRose3"
		   "MistyRose2"
		   "MistyRose1"
		   "LavenderBlush4"
		   "LavenderBlush3"
		   "LavenderBlush2"
		   "LavenderBlush1"
		   "honeydew4"
		   "honeydew3"
		   "honeydew2"
		   "honeydew1"
		   "ivory4"
		   "ivory3"
		   "ivory2"
		   "ivory1"
		   "cornsilk4"
		   "cornsilk3"
		   "cornsilk2"
		   "cornsilk1"
		   "LemonChiffon4"
		   "LemonChiffon3"
		   "LemonChiffon2"
		   "LemonChiffon1"
		   "NavajoWhite4"
		   "NavajoWhite3"
		   "NavajoWhite2"
		   "NavajoWhite1"
		   "PeachPuff4"
		   "PeachPuff3"
		   "PeachPuff2"
		   "PeachPuff1"
		   "bisque4"
		   "bisque3"
		   "bisque2"
		   "bisque1"
		   "AntiqueWhite4"
		   "AntiqueWhite3"
		   "AntiqueWhite2"
		   "AntiqueWhite1"
		   "seashell4"
		   "seashell3"
		   "seashell2"
		   "seashell1"
		   "snow4"
		   "snow3"
		   "snow2"
		   "snow1"
		   "thistle"
		   "MediumPurple"
		   "medium purple"
		   "purple"
		   "BlueViolet"
		   "blue violet"
		   "DarkViolet"
		   "dark violet"
		   "DarkOrchid"
		   "dark orchid"
		   "MediumOrchid"
		   "medium orchid"
		   "orchid"
		   "plum"
		   "violet"
		   "magenta"
		   "VioletRed"
		   "violet red"
		   "MediumVioletRed"
		   "medium violet red"
		   "maroon"
		   "PaleVioletRed"
		   "pale violet red"
		   "LightPink"
		   "light pink"
		   "pink"
		   "DeepPink"
		   "deep pink"
		   "HotPink"
		   "hot pink"
		   "red"
		   "OrangeRed"
		   "orange red"
		   "tomato"
		   "LightCoral"
		   "light coral"
		   "coral"
		   "DarkOrange"
		   "dark orange"
		   "orange"
		   "LightSalmon"
		   "light salmon"
		   "salmon"
		   "DarkSalmon"
		   "dark salmon"
		   "brown"
		   "firebrick"
		   "chocolate"
		   "tan"
		   "SandyBrown"
		   "sandy brown"
		   "wheat"
		   "beige"
		   "burlywood"
		   "peru"
		   "sienna"
		   "SaddleBrown"
		   "saddle brown"
		   "IndianRed"
		   "indian red"
		   "RosyBrown"
		   "rosy brown"
		   "DarkGoldenrod"
		   "dark goldenrod"
		   "goldenrod"
		   "LightGoldenrod"
		   "light goldenrod"
		   "gold"
		   "yellow"
		   "LightYellow"
		   "light yellow"
		   "LightGoldenrodYellow"
		   "light goldenrod yellow"
		   "PaleGoldenrod"
		   "pale goldenrod"
		   "khaki"
		   "DarkKhaki"
		   "dark khaki"
		   "OliveDrab"
		   "olive drab"
		   "ForestGreen"
		   "forest green"
		   "YellowGreen"
		   "yellow green"
		   "LimeGreen"
		   "lime green"
		   "GreenYellow"
		   "green yellow"
		   "MediumSpringGreen"
		   "medium spring green"
		   "chartreuse"
		   "green"
		   "LawnGreen"
		   "lawn green"
		   "SpringGreen"
		   "spring green"
		   "PaleGreen"
		   "pale green"
		   "LightSeaGreen"
		   "light sea green"
		   "MediumSeaGreen"
		   "medium sea green"
		   "SeaGreen"
		   "sea green"
		   "DarkSeaGreen"
		   "dark sea green"
		   "DarkOliveGreen"
		   "dark olive green"
		   "DarkGreen"
		   "dark green"
		   "aquamarine"
		   "MediumAquamarine"
		   "medium aquamarine"
		   "CadetBlue"
		   "cadet blue"
		   "LightCyan"
		   "light cyan"
		   "cyan"
		   "turquoise"
		   "MediumTurquoise"
		   "medium turquoise"
		   "DarkTurquoise"
		   "dark turquoise"
		   "PaleTurquoise"
		   "pale turquoise"
		   "PowderBlue"
		   "powder blue"
		   "LightBlue"
		   "light blue"
		   "LightSteelBlue"
		   "light steel blue"
		   "SteelBlue"
		   "steel blue"
		   "LightSkyBlue"
		   "light sky blue"
		   "SkyBlue"
		   "sky blue"
		   "DeepSkyBlue"
		   "deep sky blue"
		   "DodgerBlue"
		   "dodger blue"
		   "blue"
		   "RoyalBlue"
		   "royal blue"
		   "MediumBlue"
		   "medium blue"
		   "LightSlateBlue"
		   "light slate blue"
		   "MediumSlateBlue"
		   "medium slate blue"
		   "SlateBlue"
		   "slate blue"
		   "DarkSlateBlue"
		   "dark slate blue"
		   "CornflowerBlue"
		   "cornflower blue"
		   "NavyBlue"
		   "navy blue"
		   "navy"
		   "MidnightBlue"
		   "midnight blue"
		   "LightGray"
		   "light gray"
		   "LightGrey"
		   "light grey"
		   "grey"
		   "gray"
		   "LightSlateGrey"
		   "light slate grey"
		   "LightSlateGray"
		   "light slate gray"
		   "SlateGrey"
		   "slate grey"
		   "SlateGray"
		   "slate gray"
		   "DimGrey"
		   "dim grey"
		   "DimGray"
		   "dim gray"
		   "DarkSlateGrey"
		   "dark slate grey"
		   "DarkSlateGray"
		   "dark slate gray"
		   "black"
		   "white"
		   "MistyRose"
		   "misty rose"
		   "LavenderBlush"
		   "lavender blush"
		   "lavender"
		   "AliceBlue"
		   "alice blue"
		   "azure"
		   "MintCream"
		   "mint cream"
		   "honeydew"
		   "seashell"
		   "LemonChiffon"
		   "lemon chiffon"
		   "ivory"
		   "cornsilk"
		   "moccasin"
		   "NavajoWhite"
		   "navajo white"
		   "PeachPuff"
		   "peach puff"
		   "bisque"
		   "BlanchedAlmond"
		   "blanched almond"
		   "PapayaWhip"
		   "papaya whip"
		   "AntiqueWhite"
		   "antique white"
		   "linen"
		   "OldLace"
		   "old lace"
		   "FloralWhite"
		   "floral white"
		   "gainsboro"
		   "WhiteSmoke"
		   "white smoke"
		   "GhostWhite"
		   "ghost white"
		   "snow")
  "The list of X colors from the `rgb.txt' file.
XConsortium: rgb.txt,v 10.41 94/02/20 18:39:36 rws Exp")

(defun xw-defined-colors (&optional frame)
  "Internal function called by `defined-colors', which see."
  (or frame (setq frame (selected-frame)))
  (let ((all-colors x-colors)
	(this-color nil)
	(defined-colors nil))
    (while all-colors
      (setq this-color (car all-colors)
	    all-colors (cdr all-colors))
      (and (color-supported-p this-color frame t)
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
(define-key function-key-map [clear] [?\C-l])
(define-key function-key-map [return] [?\C-m])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-backspace] [?\M-\d])
(define-key function-key-map [M-delete] [?\M-\d])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [M-linefeed] [?\M-\n])
(define-key function-key-map [M-clear] [?\M-\C-l])
(define-key function-key-map [M-return] [?\M-\C-m])
(define-key function-key-map [M-escape] [?\M-\e])
(define-key function-key-map [iso-lefttab] [backtab])

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'backspace 'ascii-character 127)
(put 'delete 'ascii-character 127)
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)


;;;; Keysyms

(defun vendor-specific-keysyms (vendor)
  "Return the appropriate value of system-key-alist for VENDOR.
VENDOR is a string containing the name of the X Server's vendor,
as returned by (x-server-vendor)."
  (cond ((string-equal vendor "Apollo Computer Inc.")
	 '((65280 . linedel)
	   (65281 . chardel)
	   (65282 . copy)
	   (65283 . cut)
	   (65284 . paste)
	   (65285 . move)
	   (65286 . grow)
	   (65287 . cmd)
	   (65288 . shell)
	   (65289 . leftbar)
	   (65290 . rightbar)
	   (65291 . leftbox)
	   (65292 . rightbox)
	   (65293 . upbox)
	   (65294 . downbox)
	   (65295 . pop)
	   (65296 . read)
	   (65297 . edit)
	   (65298 . save)
	   (65299 . exit)
	   (65300 . repeat)))
	((or (string-equal vendor "Hewlett-Packard Incorporated")
	     (string-equal vendor "Hewlett-Packard Company"))
	 '((  168 . mute-acute)
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
	   (65397 . kp-backtab)))
	((or (string-equal vendor "X11/NeWS - Sun Microsystems Inc.")
	     (string-equal vendor "X Consortium"))
	 '((392976 . f36)
	   (392977 . f37)
	   (393056 . req)
	   ;; These are for Sun under X11R6
	   (393072 . props)
	   (393073 . front)
	   (393074 . copy)
	   (393075 . open)
	   (393076 . paste)
	   (393077 . cut)))
	(t
	 ;; This is used by DEC's X server.
	 '((65280 . remove)))))

;; Latin-1
(let ((i 160))
  (while (< i 256)
    (puthash i i x-keysym-table)
    (setq i (1+ i))))

;; Table from Kuhn's proposed additions to the `KEYSYM Encoding'
;; appendix to the X protocol definition.
(dolist
     (pair
      '((#x1a1 . ?Ą)
	(#x1a2 . ?˘)
	(#x1a3 . ?Ł)
	(#x1a5 . ?Ľ)
	(#x1a6 . ?Ś)
	(#x1a9 . ?Š)
	(#x1aa . ?Ş)
	(#x1ab . ?Ť)
	(#x1ac . ?Ź)
	(#x1ae . ?Ž)
	(#x1af . ?Ż)
	(#x1b1 . ?ą)
	(#x1b2 . ?˛)
	(#x1b3 . ?ł)
	(#x1b5 . ?ľ)
	(#x1b6 . ?ś)
	(#x1b7 . ?ˇ)
	(#x1b9 . ?š)
	(#x1ba . ?ş)
	(#x1bb . ?ť)
	(#x1bc . ?ź)
	(#x1bd . ?˝)
	(#x1be . ?ž)
	(#x1bf . ?ż)
	(#x1c0 . ?Ŕ)
	(#x1c3 . ?Ă)
	(#x1c5 . ?Ĺ)
	(#x1c6 . ?Ć)
	(#x1c8 . ?Č)
	(#x1ca . ?Ę)
	(#x1cc . ?Ě)
	(#x1cf . ?Ď)
	(#x1d0 . ?Đ)
	(#x1d1 . ?Ń)
	(#x1d2 . ?Ň)
	(#x1d5 . ?Ő)
	(#x1d8 . ?Ř)
	(#x1d9 . ?Ů)
	(#x1db . ?Ű)
	(#x1de . ?Ţ)
	(#x1e0 . ?ŕ)
	(#x1e3 . ?ă)
	(#x1e5 . ?ĺ)
	(#x1e6 . ?ć)
	(#x1e8 . ?č)
	(#x1ea . ?ę)
	(#x1ec . ?ě)
	(#x1ef . ?ď)
	(#x1f0 . ?đ)
	(#x1f1 . ?ń)
	(#x1f2 . ?ň)
	(#x1f5 . ?ő)
	(#x1f8 . ?ř)
	(#x1f9 . ?ů)
	(#x1fb . ?ű)
	(#x1fe . ?ţ)
	(#x1ff . ?˙)
	(#x2a1 . ?Ħ)
	(#x2a6 . ?Ĥ)
	(#x2a9 . ?İ)
	(#x2ab . ?Ğ)
	(#x2ac . ?Ĵ)
	(#x2b1 . ?ħ)
	(#x2b6 . ?ĥ)
	(#x2b9 . ?ı)
	(#x2bb . ?ğ)
	(#x2bc . ?ĵ)
	(#x2c5 . ?Ċ)
	(#x2c6 . ?Ĉ)
	(#x2d5 . ?Ġ)
	(#x2d8 . ?Ĝ)
	(#x2dd . ?Ŭ)
	(#x2de . ?Ŝ)
	(#x2e5 . ?ċ)
	(#x2e6 . ?ĉ)
	(#x2f5 . ?ġ)
	(#x2f8 . ?ĝ)
	(#x2fd . ?ŭ)
	(#x2fe . ?ŝ)
	(#x3a2 . ?ĸ)
	(#x3a3 . ?Ŗ)
	(#x3a5 . ?Ĩ)
	(#x3a6 . ?Ļ)
	(#x3aa . ?Ē)
	(#x3ab . ?Ģ)
	(#x3ac . ?Ŧ)
	(#x3b3 . ?ŗ)
	(#x3b5 . ?ĩ)
	(#x3b6 . ?ļ)
	(#x3ba . ?ē)
	(#x3bb . ?ģ)
	(#x3bc . ?ŧ)
	(#x3bd . ?Ŋ)
	(#x3bf . ?ŋ)
	(#x3c0 . ?Ā)
	(#x3c7 . ?Į)
	(#x3cc . ?Ė)
	(#x3cf . ?Ī)
	(#x3d1 . ?Ņ)
	(#x3d2 . ?Ō)
	(#x3d3 . ?Ķ)
	(#x3d9 . ?Ų)
	(#x3dd . ?Ũ)
	(#x3de . ?Ū)
	(#x3e0 . ?ā)
	(#x3e7 . ?į)
	(#x3ec . ?ė)
	(#x3ef . ?ī)
	(#x3f1 . ?ņ)
	(#x3f2 . ?ō)
	(#x3f3 . ?ķ)
	(#x3f9 . ?ų)
	(#x3fd . ?ũ)
	(#x3fe . ?ū)
	(#x47e . ?‾)
	(#x4a1 . ?。)
	(#x4a2 . ?\「)
	(#x4a3 . ?\」)
	(#x4a4 . ?、)
	(#x4a5 . ?・)
	(#x4a6 . ?ヲ)
	(#x4a7 . ?ァ)
	(#x4a8 . ?ィ)
	(#x4a9 . ?ゥ)
	(#x4aa . ?ェ)
	(#x4ab . ?ォ)
	(#x4ac . ?ャ)
	(#x4ad . ?ュ)
	(#x4ae . ?ョ)
	(#x4af . ?ッ)
	(#x4b0 . ?ー)
	(#x4b1 . ?ア)
	(#x4b2 . ?イ)
	(#x4b3 . ?ウ)
	(#x4b4 . ?エ)
	(#x4b5 . ?オ)
	(#x4b6 . ?カ)
	(#x4b7 . ?キ)
	(#x4b8 . ?ク)
	(#x4b9 . ?ケ)
	(#x4ba . ?コ)
	(#x4bb . ?サ)
	(#x4bc . ?シ)
	(#x4bd . ?ス)
	(#x4be . ?セ)
	(#x4bf . ?ソ)
	(#x4c0 . ?タ)
	(#x4c1 . ?チ)
	(#x4c2 . ?ツ)
	(#x4c3 . ?テ)
	(#x4c4 . ?ト)
	(#x4c5 . ?ナ)
	(#x4c6 . ?ニ)
	(#x4c7 . ?ヌ)
	(#x4c8 . ?ネ)
	(#x4c9 . ?ノ)
	(#x4ca . ?ハ)
	(#x4cb . ?ヒ)
	(#x4cc . ?フ)
	(#x4cd . ?ヘ)
	(#x4ce . ?ホ)
	(#x4cf . ?マ)
	(#x4d0 . ?ミ)
	(#x4d1 . ?ム)
	(#x4d2 . ?メ)
	(#x4d3 . ?モ)
	(#x4d4 . ?ヤ)
	(#x4d5 . ?ユ)
	(#x4d6 . ?ヨ)
	(#x4d7 . ?ラ)
	(#x4d8 . ?リ)
	(#x4d9 . ?ル)
	(#x4da . ?レ)
	(#x4db . ?ロ)
	(#x4dc . ?ワ)
	(#x4dd . ?ン)
	(#x4de . ?゛)
	(#x4df . ?゜)
	(#x5ac . ?،)
	(#x5bb . ?؛)
	(#x5bf . ?؟)
	(#x5c1 . ?ء)
	(#x5c2 . ?آ)
	(#x5c3 . ?أ)
	(#x5c4 . ?ؤ)
	(#x5c5 . ?إ)
	(#x5c6 . ?ئ)
	(#x5c7 . ?ا)
	(#x5c8 . ?ب)
	(#x5c9 . ?ة)
	(#x5ca . ?ت)
	(#x5cb . ?ث)
	(#x5cc . ?ج)
	(#x5cd . ?ح)
	(#x5ce . ?خ)
	(#x5cf . ?د)
	(#x5d0 . ?ذ)
	(#x5d1 . ?ر)
	(#x5d2 . ?ز)
	(#x5d3 . ?س)
	(#x5d4 . ?ش)
	(#x5d5 . ?ص)
	(#x5d6 . ?ض)
	(#x5d7 . ?ط)
	(#x5d8 . ?ظ)
	(#x5d9 . ?ع)
	(#x5da . ?غ)
	(#x5e0 . ?ـ)
	(#x5e1 . ?ف)
	(#x5e2 . ?ق)
	(#x5e3 . ?ك)
	(#x5e4 . ?ل)
	(#x5e5 . ?م)
	(#x5e6 . ?ن)
	(#x5e7 . ?ه)
	(#x5e8 . ?و)
	(#x5e9 . ?ى)
	(#x5ea . ?ي)
	(#x5eb . ?ً)
	(#x5ec . ?ٌ)
	(#x5ed . ?ٍ)
	(#x5ee . ?َ)
	(#x5ef . ?ُ)
	(#x5f0 . ?ِ)
	(#x5f1 . ?ّ)
	(#x5f2 . ?ْ)
	(#x6a1 . ?ђ)
	(#x6a2 . ?ѓ)
	(#x6a3 . ?ё)
	(#x6a4 . ?є)
	(#x6a5 . ?ѕ)
	(#x6a6 . ?і)
	(#x6a7 . ?ї)
	(#x6a8 . ?ј)
	(#x6a9 . ?љ)
	(#x6aa . ?њ)
	(#x6ab . ?ћ)
	(#x6ac . ?ќ)
	(#x6ae . ?ў)
	(#x6af . ?џ)
	(#x6b0 . ?№)
	(#x6b1 . ?Ђ)
	(#x6b2 . ?Ѓ)
	(#x6b3 . ?Ё)
	(#x6b4 . ?Є)
	(#x6b5 . ?Ѕ)
	(#x6b6 . ?І)
	(#x6b7 . ?Ї)
	(#x6b8 . ?Ј)
	(#x6b9 . ?Љ)
	(#x6ba . ?Њ)
	(#x6bb . ?Ћ)
	(#x6bc . ?Ќ)
	(#x6be . ?Ў)
	(#x6bf . ?Џ)
	(#x6c0 . ?ю)
	(#x6c1 . ?а)
	(#x6c2 . ?б)
	(#x6c3 . ?ц)
	(#x6c4 . ?д)
	(#x6c5 . ?е)
	(#x6c6 . ?ф)
	(#x6c7 . ?г)
	(#x6c8 . ?х)
	(#x6c9 . ?и)
	(#x6ca . ?й)
	(#x6cb . ?к)
	(#x6cc . ?л)
	(#x6cd . ?м)
	(#x6ce . ?н)
	(#x6cf . ?о)
	(#x6d0 . ?п)
	(#x6d1 . ?я)
	(#x6d2 . ?р)
	(#x6d3 . ?с)
	(#x6d4 . ?т)
	(#x6d5 . ?у)
	(#x6d6 . ?ж)
	(#x6d7 . ?в)
	(#x6d8 . ?ь)
	(#x6d9 . ?ы)
	(#x6da . ?з)
	(#x6db . ?ш)
	(#x6dc . ?э)
	(#x6dd . ?щ)
	(#x6de . ?ч)
	(#x6df . ?ъ)
	(#x6e0 . ?Ю)
	(#x6e1 . ?А)
	(#x6e2 . ?Б)
	(#x6e3 . ?Ц)
	(#x6e4 . ?Д)
	(#x6e5 . ?Е)
	(#x6e6 . ?Ф)
	(#x6e7 . ?Г)
	(#x6e8 . ?Х)
	(#x6e9 . ?И)
	(#x6ea . ?Й)
	(#x6eb . ?К)
	(#x6ec . ?Л)
	(#x6ed . ?М)
	(#x6ee . ?Н)
	(#x6ef . ?О)
	(#x6f0 . ?П)
	(#x6f1 . ?Я)
	(#x6f2 . ?Р)
	(#x6f3 . ?С)
	(#x6f4 . ?Т)
	(#x6f5 . ?У)
	(#x6f6 . ?Ж)
	(#x6f7 . ?В)
	(#x6f8 . ?Ь)
	(#x6f9 . ?Ы)
	(#x6fa . ?З)
	(#x6fb . ?Ш)
	(#x6fc . ?Э)
	(#x6fd . ?Щ)
	(#x6fe . ?Ч)
	(#x6ff . ?Ъ)
	(#x7a1 . ?Ά)
	(#x7a2 . ?Έ)
	(#x7a3 . ?Ή)
	(#x7a4 . ?Ί)
	(#x7a5 . ?Ϊ)
	(#x7a7 . ?Ό)
	(#x7a8 . ?Ύ)
	(#x7a9 . ?Ϋ)
	(#x7ab . ?Ώ)
	(#x7ae . ?΅)
	(#x7af . ?―)
	(#x7b1 . ?ά)
	(#x7b2 . ?έ)
	(#x7b3 . ?ή)
	(#x7b4 . ?ί)
	(#x7b5 . ?ϊ)
	(#x7b6 . ?ΐ)
	(#x7b7 . ?ό)
	(#x7b8 . ?ύ)
	(#x7b9 . ?ϋ)
	(#x7ba . ?ΰ)
	(#x7bb . ?ώ)
	(#x7c1 . ?Α)
	(#x7c2 . ?Β)
	(#x7c3 . ?Γ)
	(#x7c4 . ?Δ)
	(#x7c5 . ?Ε)
	(#x7c6 . ?Ζ)
	(#x7c7 . ?Η)
	(#x7c8 . ?Θ)
	(#x7c9 . ?Ι)
	(#x7ca . ?Κ)
	(#x7cb . ?Λ)
	(#x7cc . ?Μ)
	(#x7cd . ?Ν)
	(#x7ce . ?Ξ)
	(#x7cf . ?Ο)
	(#x7d0 . ?Π)
	(#x7d1 . ?Ρ)
	(#x7d2 . ?Σ)
	(#x7d4 . ?Τ)
	(#x7d5 . ?Υ)
	(#x7d6 . ?Φ)
	(#x7d7 . ?Χ)
	(#x7d8 . ?Ψ)
	(#x7d9 . ?Ω)
	(#x7e1 . ?α)
	(#x7e2 . ?β)
	(#x7e3 . ?γ)
	(#x7e4 . ?δ)
	(#x7e5 . ?ε)
	(#x7e6 . ?ζ)
	(#x7e7 . ?η)
	(#x7e8 . ?θ)
	(#x7e9 . ?ι)
	(#x7ea . ?κ)
	(#x7eb . ?λ)
	(#x7ec . ?μ)
	(#x7ed . ?ν)
	(#x7ee . ?ξ)
	(#x7ef . ?ο)
	(#x7f0 . ?π)
	(#x7f1 . ?ρ)
	(#x7f2 . ?σ)
	(#x7f3 . ?ς)
	(#x7f4 . ?τ)
	(#x7f5 . ?υ)
	(#x7f6 . ?φ)
	(#x7f7 . ?χ)
	(#x7f8 . ?ψ)
	(#x7f9 . ?ω)
	(#x8a1 . ?⎷)
	(#x8a2 . ?┌)
	(#x8a3 . ?─)
	(#x8a4 . ?⌠)
	(#x8a5 . ?⌡)
	(#x8a6 . ?│)
	(#x8a7 . ?⎡)
	(#x8a8 . ?⎣)
	(#x8a9 . ?⎤)
	(#x8aa . ?⎦)
	(#x8ab . ?⎛)
	(#x8ac . ?⎝)
	(#x8ad . ?⎞)
	(#x8ae . ?⎠)
	(#x8af . ?⎨)
	(#x8b0 . ?⎬)
	(#x8bc . ?≤)
	(#x8bd . ?≠)
	(#x8be . ?≥)
	(#x8bf . ?∫)
	(#x8c0 . ?∴)
	(#x8c1 . ?∝)
	(#x8c2 . ?∞)
	(#x8c5 . ?∇)
	(#x8c8 . ?∼)
	(#x8c9 . ?≃)
	(#x8cd . ?⇔)
	(#x8ce . ?⇒)
	(#x8cf . ?≡)
	(#x8d6 . ?√)
	(#x8da . ?⊂)
	(#x8db . ?⊃)
	(#x8dc . ?∩)
	(#x8dd . ?∪)
	(#x8de . ?∧)
	(#x8df . ?∨)
	(#x8ef . ?∂)
	(#x8f6 . ?ƒ)
	(#x8fb . ?←)
	(#x8fc . ?↑)
	(#x8fd . ?→)
	(#x8fe . ?↓)
	(#x9e0 . ?◆)
	(#x9e1 . ?▒)
	(#x9e2 . ?␉)
	(#x9e3 . ?␌)
	(#x9e4 . ?␍)
	(#x9e5 . ?␊)
	(#x9e8 . ?␤)
	(#x9e9 . ?␋)
	(#x9ea . ?┘)
	(#x9eb . ?┐)
	(#x9ec . ?┌)
	(#x9ed . ?└)
	(#x9ee . ?┼)
	(#x9ef . ?⎺)
	(#x9f0 . ?⎻)
	(#x9f1 . ?─)
	(#x9f2 . ?⎼)
	(#x9f3 . ?⎽)
	(#x9f4 . ?├)
	(#x9f5 . ?┤)
	(#x9f6 . ?┴)
	(#x9f7 . ?┬)
	(#x9f8 . ?│)
	(#xaa1 . ? )
	(#xaa2 . ? )
	(#xaa3 . ? )
	(#xaa4 . ? )
	(#xaa5 . ? )
	(#xaa6 . ? )
	(#xaa7 . ? )
	(#xaa8 . ? )
	(#xaa9 . ?—)
	(#xaaa . ?–)
	(#xaae . ?…)
	(#xaaf . ?‥)
	(#xab0 . ?⅓)
	(#xab1 . ?⅔)
	(#xab2 . ?⅕)
	(#xab3 . ?⅖)
	(#xab4 . ?⅗)
	(#xab5 . ?⅘)
	(#xab6 . ?⅙)
	(#xab7 . ?⅚)
	(#xab8 . ?℅)
	(#xabb . ?‒)
	(#xabc . ?〈)
	(#xabe . ?〉)
	(#xac3 . ?⅛)
	(#xac4 . ?⅜)
	(#xac5 . ?⅝)
	(#xac6 . ?⅞)
	(#xac9 . ?™)
	(#xaca . ?☓)
	(#xacc . ?◁)
	(#xacd . ?▷)
	(#xace . ?○)
	(#xacf . ?▯)
	(#xad0 . ?‘)
	(#xad1 . ?’)
	(#xad2 . ?“)
	(#xad3 . ?”)
	(#xad4 . ?℞)
	(#xad6 . ?′)
	(#xad7 . ?″)
	(#xad9 . ?✝)
	(#xadb . ?▬)
	(#xadc . ?◀)
	(#xadd . ?▶)
	(#xade . ?●)
	(#xadf . ?▮)
	(#xae0 . ?◦)
	(#xae1 . ?▫)
	(#xae2 . ?▭)
	(#xae3 . ?△)
	(#xae4 . ?▽)
	(#xae5 . ?☆)
	(#xae6 . ?•)
	(#xae7 . ?▪)
	(#xae8 . ?▲)
	(#xae9 . ?▼)
	(#xaea . ?☜)
	(#xaeb . ?☞)
	(#xaec . ?♣)
	(#xaed . ?♦)
	(#xaee . ?♥)
	(#xaf0 . ?✠)
	(#xaf1 . ?†)
	(#xaf2 . ?‡)
	(#xaf3 . ?✓)
	(#xaf4 . ?✗)
	(#xaf5 . ?♯)
	(#xaf6 . ?♭)
	(#xaf7 . ?♂)
	(#xaf8 . ?♀)
	(#xaf9 . ?☎)
	(#xafa . ?⌕)
	(#xafb . ?℗)
	(#xafc . ?‸)
	(#xafd . ?‚)
	(#xafe . ?„)
	(#xba3 . ?<)
	(#xba6 . ?>)
	(#xba8 . ?∨)
	(#xba9 . ?∧)
	(#xbc0 . ?¯)
	(#xbc2 . ?⊥)
	(#xbc3 . ?∩)
	(#xbc4 . ?⌊)
	(#xbc6 . ?_)
	(#xbca . ?∘)
	(#xbcc . ?⎕)
	(#xbce . ?⊤)
	(#xbcf . ?○)
	(#xbd3 . ?⌈)
	(#xbd6 . ?∪)
	(#xbd8 . ?⊃)
	(#xbda . ?⊂)
	(#xbdc . ?⊢)
	(#xbfc . ?⊣)
	(#xcdf . ?‗)
	(#xce0 . ?א)
	(#xce1 . ?ב)
	(#xce2 . ?ג)
	(#xce3 . ?ד)
	(#xce4 . ?ה)
	(#xce5 . ?ו)
	(#xce6 . ?ז)
	(#xce7 . ?ח)
	(#xce8 . ?ט)
	(#xce9 . ?י)
	(#xcea . ?ך)
	(#xceb . ?כ)
	(#xcec . ?ל)
	(#xced . ?ם)
	(#xcee . ?מ)
	(#xcef . ?ן)
	(#xcf0 . ?נ)
	(#xcf1 . ?ס)
	(#xcf2 . ?ע)
	(#xcf3 . ?ף)
	(#xcf4 . ?פ)
	(#xcf5 . ?ץ)
	(#xcf6 . ?צ)
	(#xcf7 . ?ק)
	(#xcf8 . ?ר)
	(#xcf9 . ?ש)
	(#xcfa . ?ת)
	(#xda1 . ?ก)
	(#xda2 . ?ข)
	(#xda3 . ?ฃ)
	(#xda4 . ?ค)
	(#xda5 . ?ฅ)
	(#xda6 . ?ฆ)
	(#xda7 . ?ง)
	(#xda8 . ?จ)
	(#xda9 . ?ฉ)
	(#xdaa . ?ช)
	(#xdab . ?ซ)
	(#xdac . ?ฌ)
	(#xdad . ?ญ)
	(#xdae . ?ฎ)
	(#xdaf . ?ฏ)
	(#xdb0 . ?ฐ)
	(#xdb1 . ?ฑ)
	(#xdb2 . ?ฒ)
	(#xdb3 . ?ณ)
	(#xdb4 . ?ด)
	(#xdb5 . ?ต)
	(#xdb6 . ?ถ)
	(#xdb7 . ?ท)
	(#xdb8 . ?ธ)
	(#xdb9 . ?น)
	(#xdba . ?บ)
	(#xdbb . ?ป)
	(#xdbc . ?ผ)
	(#xdbd . ?ฝ)
	(#xdbe . ?พ)
	(#xdbf . ?ฟ)
	(#xdc0 . ?ภ)
	(#xdc1 . ?ม)
	(#xdc2 . ?ย)
	(#xdc3 . ?ร)
	(#xdc4 . ?ฤ)
	(#xdc5 . ?ล)
	(#xdc6 . ?ฦ)
	(#xdc7 . ?ว)
	(#xdc8 . ?ศ)
	(#xdc9 . ?ษ)
	(#xdca . ?ส)
	(#xdcb . ?ห)
	(#xdcc . ?ฬ)
	(#xdcd . ?อ)
	(#xdce . ?ฮ)
	(#xdcf . ?ฯ)
	(#xdd0 . ?ะ)
	(#xdd1 . ?ั)
	(#xdd2 . ?า)
	(#xdd3 . ?ำ)
	(#xdd4 . ?ิ)
	(#xdd5 . ?ี)
	(#xdd6 . ?ึ)
	(#xdd7 . ?ื)
	(#xdd8 . ?ุ)
	(#xdd9 . ?ู)
	(#xdda . ?ฺ)
	(#xddf . ?฿)
	(#xde0 . ?เ)
	(#xde1 . ?แ)
	(#xde2 . ?โ)
	(#xde3 . ?ใ)
	(#xde4 . ?ไ)
	(#xde5 . ?ๅ)
	(#xde6 . ?ๆ)
	(#xde7 . ?็)
	(#xde8 . ?่)
	(#xde9 . ?้)
	(#xdea . ?๊)
	(#xdeb . ?๋)
	(#xdec . ?์)
	(#xded . ?ํ)
	(#xdf0 . ?๐)
	(#xdf1 . ?๑)
	(#xdf2 . ?๒)
	(#xdf3 . ?๓)
	(#xdf4 . ?๔)
	(#xdf5 . ?๕)
	(#xdf6 . ?๖)
	(#xdf7 . ?๗)
	(#xdf8 . ?๘)
	(#xdf9 . ?๙)
	(#xea1 . ?ㄱ)
	(#xea2 . ?ㄲ)
	(#xea3 . ?ㄳ)
	(#xea4 . ?ㄴ)
	(#xea5 . ?ㄵ)
	(#xea6 . ?ㄶ)
	(#xea7 . ?ㄷ)
	(#xea8 . ?ㄸ)
	(#xea9 . ?ㄹ)
	(#xeaa . ?ㄺ)
	(#xeab . ?ㄻ)
	(#xeac . ?ㄼ)
	(#xead . ?ㄽ)
	(#xeae . ?ㄾ)
	(#xeaf . ?ㄿ)
	(#xeb0 . ?ㅀ)
	(#xeb1 . ?ㅁ)
	(#xeb2 . ?ㅂ)
	(#xeb3 . ?ㅃ)
	(#xeb4 . ?ㅄ)
	(#xeb5 . ?ㅅ)
	(#xeb6 . ?ㅆ)
	(#xeb7 . ?ㅇ)
	(#xeb8 . ?ㅈ)
	(#xeb9 . ?ㅉ)
	(#xeba . ?ㅊ)
	(#xebb . ?ㅋ)
	(#xebc . ?ㅌ)
	(#xebd . ?ㅍ)
	(#xebe . ?ㅎ)
	(#xebf . ?ㅏ)
	(#xec0 . ?ㅐ)
	(#xec1 . ?ㅑ)
	(#xec2 . ?ㅒ)
	(#xec3 . ?ㅓ)
	(#xec4 . ?ㅔ)
	(#xec5 . ?ㅕ)
	(#xec6 . ?ㅖ)
	(#xec7 . ?ㅗ)
	(#xec8 . ?ㅘ)
	(#xec9 . ?ㅙ)
	(#xeca . ?ㅚ)
	(#xecb . ?ㅛ)
	(#xecc . ?ㅜ)
	(#xecd . ?ㅝ)
	(#xece . ?ㅞ)
	(#xecf . ?ㅟ)
	(#xed0 . ?ㅠ)
	(#xed1 . ?ㅡ)
	(#xed2 . ?ㅢ)
	(#xed3 . ?ㅣ)
	(#xed4 . ?ᆨ)
	(#xed5 . ?ᆩ)
	(#xed6 . ?ᆪ)
	(#xed7 . ?ᆫ)
	(#xed8 . ?ᆬ)
	(#xed9 . ?ᆭ)
	(#xeda . ?ᆮ)
	(#xedb . ?ᆯ)
	(#xedc . ?ᆰ)
	(#xedd . ?ᆱ)
	(#xede . ?ᆲ)
	(#xedf . ?ᆳ)
	(#xee0 . ?ᆴ)
	(#xee1 . ?ᆵ)
	(#xee2 . ?ᆶ)
	(#xee3 . ?ᆷ)
	(#xee4 . ?ᆸ)
	(#xee5 . ?ᆹ)
	(#xee6 . ?ᆺ)
	(#xee7 . ?ᆻ)
	(#xee8 . ?ᆼ)
	(#xee9 . ?ᆽ)
	(#xeea . ?ᆾ)
	(#xeeb . ?ᆿ)
	(#xeec . ?ᇀ)
	(#xeed . ?ᇁ)
	(#xeee . ?ᇂ)
	(#xeef . ?ㅭ)
	(#xef0 . ?ㅱ)
	(#xef1 . ?ㅸ)
	(#xef2 . ?ㅿ)
	(#xef3 . ?ㆁ)
	(#xef4 . ?ㆄ)
	(#xef5 . ?ㆆ)
	(#xef6 . ?ㆍ)
	(#xef7 . ?ㆎ)
	(#xef8 . ?ᇫ)
	(#xef9 . ?ᇰ)
	(#xefa . ?ᇹ)
	(#xeff . ?₩)
	(#x13bc . ?Œ)
	(#x13bd . ?œ)
	(#x13be . ?Ÿ)
	(#x20a0 . ?₠)
	(#x20a1 . ?₡)
	(#x20a2 . ?₢)
	(#x20a3 . ?₣)
	(#x20a4 . ?₤)
	(#x20a5 . ?₥)
	(#x20a6 . ?₦)
	(#x20a7 . ?₧)
	(#x20a8 . ?₨)
	(#x20aa . ?₪)
	(#x20ab . ?₫)
	(#x20ac . ?€)))
  (puthash (car pair) (cdr pair) x-keysym-table))

;; The following keysym codes for graphics are listed in the document
;; as not having unicodes available:

;; #x08b1	TOP LEFT SUMMATION	Technical
;; #x08b2	BOTTOM LEFT SUMMATION	Technical
;; #x08b3	TOP VERTICAL SUMMATION CONNECTOR	Technical
;; #x08b4	BOTTOM VERTICAL SUMMATION CONNECTOR	Technical
;; #x08b5	TOP RIGHT SUMMATION	Technical
;; #x08b6	BOTTOM RIGHT SUMMATION	Technical
;; #x08b7	RIGHT MIDDLE SUMMATION	Technical
;; #x0aac	SIGNIFICANT BLANK SYMBOL	Publish
;; #x0abd	DECIMAL POINT	Publish
;; #x0abf	MARKER	Publish
;; #x0acb	TRADEMARK SIGN IN CIRCLE	Publish
;; #x0ada	HEXAGRAM	Publish
;; #x0aff	CURSOR	Publish
;; #x0dde	THAI MAIHANAKAT	Thai


;;;; Selections and cut buffers

;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-cut-buffer-or-selection-value.
(defvar x-last-selected-text nil)

;;; It is said that overlarge strings are slow to put into the cut buffer.
;;; Note this value is overridden below.
(defvar x-cut-buffer-max 20000
  "Max number of characters to put in the cut buffer.")

(defcustom x-select-enable-clipboard nil
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection."
  :type 'boolean
  :group 'killing)

;;; Make TEXT, a string, the primary X selection.
;;; Also, set the value of X cut buffer 0, for backward compatibility
;;; with older X applications.
;;; gildea@stop.mail-abuse.org says it's not desirable to put kills
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
;;; If this function is called twice and finds the same text,
;;; it returns nil the second time.  This is so that a single
;;; selection won't be added to the kill ring over and over.
(defun x-cut-buffer-or-selection-value ()
  (let (text)
    (when x-select-enable-clipboard
      (if (null text) 
	  (condition-case c
	      (setq text (x-get-selection 'CLIPBOARD 'COMPOUND_TEXT))
	    (error nil)))
      (if (null text) 
	  (condition-case c
	      (setq text (x-get-selection 'CLIPBOARD 'STRING))
	    (error nil)))
      (if (string= text "") (setq text nil)))

    ;; Don't die if x-get-selection signals an error.
    (if (null text) 
	(condition-case c
	    (setq text (x-get-selection 'PRIMARY 'COMPOUND_TEXT))
	  (error nil)))
    (if (null text) 
	(condition-case c
	    (setq text (x-get-selection 'PRIMARY 'STRING))
	  (error nil)))
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

(x-open-connection (or x-display-name
		       (setq x-display-name (getenv "DISPLAY")))
		   x-command-line-resources
		   ;; Exit Emacs with fatal error if this fails.
		   t)

(setq frame-creation-function 'x-create-frame-with-faces)

(setq x-cut-buffer-max (min (- (/ (x-server-max-request-size) 2) 100)
			    x-cut-buffer-max))

;; Create the standard fontset.
(create-fontset-from-fontset-spec standard-fontset-spec t)

;; Create fontset specified in X resources "Fontset-N" (N is 0, 1, ...).
(create-fontset-from-x-resource)

;; Sun expects the menu bar cut and paste commands to use the clipboard.
;; This has ,? to match both on Sunos and on Solaris.
(if (string-match "Sun Microsystems,? Inc\\."
		  (x-server-vendor))
    (menu-bar-enable-clipboard))

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
  (error "Suspending an emacs running under X makes no sense"))
(add-hook 'suspend-hook 'x-win-suspend-error)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; Turn off window-splitting optimization; X is usually fast enough
;;; that this is only annoying.
(setq split-window-keep-point t)

;; Don't show the frame name; that's redundant with X.
(setq-default mode-line-frame-identification "  ")

;;; Motif direct handling of f10 wasn't working right,
;;; So temporarily we've turned it off in lwlib-Xm.c
;;; and turned the Emacs f10 back on.
;;; ;; Motif normally handles f10 itself, so don't try to handle it a second time.
;;; (if (featurep 'motif)
;;;     (global-set-key [f10] 'ignore))

;;; x-win.el ends here
