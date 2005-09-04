;;; mac-win.el --- parse switches controlling interface with Mac window system -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1999, 2000, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Andrew Choi <akochoi@mac.com>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Mac-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that Mac windows are to be used.  Command line switches are parsed and those
;; pertaining to Mac are processed and removed from the command line.  The
;; Mac display is opened and hooks are set for popping up the initial window.

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

(if (not (eq window-system 'mac))
    (error "%s: Loading mac-win.el but not compiled for Mac" (invocation-name)))

(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(defvar mac-charset-info-alist)
(defvar mac-services-selection)
(defvar mac-system-script-code)
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
			    (string-to-number (car x-invocation-args)))
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
  "The display name specifying server and frame.")

(defun x-handle-display (switch)
  (setq x-display-name (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args)))

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
;; Standard Mac cursor shapes
;;

(defconst mac-pointer-arrow 0)
(defconst mac-pointer-copy-arrow 1)
(defconst mac-pointer-alias-arrow 2)
(defconst mac-pointer-contextual-menu-arrow 3)
(defconst mac-pointer-I-beam 4)
(defconst mac-pointer-cross 5)
(defconst mac-pointer-plus 6)
(defconst mac-pointer-watch 7)
(defconst mac-pointer-closed-hand 8)
(defconst mac-pointer-open-hand 9)
(defconst mac-pointer-pointing-hand 10)
(defconst mac-pointer-counting-up-hand 11)
(defconst mac-pointer-counting-down-hand 12)
(defconst mac-pointer-counting-up-and-down-hand 13)
(defconst mac-pointer-spinning 14)
(defconst mac-pointer-resize-left 15)
(defconst mac-pointer-resize-right 16)
(defconst mac-pointer-resize-left-right 17)
;; Mac OS X 10.2 and later
(defconst mac-pointer-not-allowed 18)
;; Mac OS X 10.3 and later
(defconst mac-pointer-resize-up 19)
(defconst mac-pointer-resize-down 20)
(defconst mac-pointer-resize-up-down 21)
(defconst mac-pointer-poof 22)

;;
;; Standard X cursor shapes that have Mac counterparts
;;

(defconst x-pointer-left-ptr mac-pointer-arrow)
(defconst x-pointer-xterm mac-pointer-I-beam)
(defconst x-pointer-crosshair mac-pointer-cross)
(defconst x-pointer-plus mac-pointer-plus)
(defconst x-pointer-watch mac-pointer-watch)
(defconst x-pointer-hand2 mac-pointer-pointing-hand)
(defconst x-pointer-left-side mac-pointer-resize-left)
(defconst x-pointer-right-side mac-pointer-resize-right)
(defconst x-pointer-sb-h-double-arrow mac-pointer-resize-left-right)
(defconst x-pointer-top-side mac-pointer-resize-up)
(defconst x-pointer-bottom-side mac-pointer-resize-down)
(defconst x-pointer-sb-v-double-arrow mac-pointer-resize-up-down)


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

(substitute-key-definition 'suspend-emacs 'iconify-or-deiconify-frame
			   global-map)

;; Map certain keypad keys into ASCII characters
;; that people usually expect.
(let ((m (terminal-local-value 'local-function-key-map nil)))
  (define-key m [backspace] [?\d])
  (define-key m [delete] [?\d])
  (define-key m [tab] [?\t])
  (define-key m [linefeed] [?\n])
  (define-key m [clear] [?\C-l])
  (define-key m [return] [?\C-m])
  (define-key m [escape] [?\e])
  (define-key m [M-backspace] [?\M-\d])
  (define-key m [M-delete] [?\M-\d])
  (define-key m [M-tab] [?\M-\t])
  (define-key m [M-linefeed] [?\M-\n])
  (define-key m [M-clear] [?\M-\C-l])
  (define-key m [M-return] [?\M-\C-m])
  (define-key m [M-escape] [?\M-\e]))

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'backspace 'ascii-character ?\d)
(put 'delete 'ascii-character ?\d)
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character ?\C-l)
(put 'return 'ascii-character ?\C-m)
(put 'escape 'ascii-character ?\e)


;;;; Script codes and coding systems
(defconst mac-script-code-coding-systems
  '((0 . mac-roman)			; smRoman
    (1 . japanese-shift-jis)		; smJapanese
    (2 . chinese-big5)			; smTradChinese
    (3 . korean-iso-8bit)		; smKorean
    (7 . mac-cyrillic)			; smCyrillic
    (25 . chinese-iso-8bit)		; smSimpChinese
    (29 . mac-centraleurroman)		; smCentralEuroRoman
    )
  "Alist of Mac script codes vs Emacs coding systems.")

(defconst mac-system-coding-system
  (let ((base (or (cdr (assq mac-system-script-code
			     mac-script-code-coding-systems))
		  'mac-roman)))
    (if (eq system-type 'darwin)
	base
      (coding-system-change-eol-conversion base 'mac)))
  "Coding system derived from the system script code.")

(defun mac-add-charset-info (xlfd-charset mac-text-encoding)
  "Function to add character sets to display with Mac fonts.
Creates entries in `mac-charset-info-alist'.
XLFD-CHARSET is a string which will appear in the XLFD font name
to identify the character set.  MAC-TEXT-ENCODING is the
correspoinding TextEncodingBase value."
  (add-to-list 'mac-charset-info-alist
               (list xlfd-charset mac-text-encoding
		     (cdr (assq mac-text-encoding
				mac-script-code-coding-systems)))))

(setq mac-charset-info-alist nil)
(mac-add-charset-info "mac-roman" 0)
(mac-add-charset-info "jisx0208.1983-sjis" 1)
(mac-add-charset-info "jisx0201.1976-0" 1)
(mac-add-charset-info "big5-0" 2)
(mac-add-charset-info "ksc5601.1989-0" 3)
(mac-add-charset-info "mac-cyrillic" 7)
(mac-add-charset-info "gb2312.1980-0" 25)
(mac-add-charset-info "mac-centraleurroman" 29)
(mac-add-charset-info "mac-symbol" 33)
(mac-add-charset-info "adobe-fontspecific" 33) ; for X-Symbol
(mac-add-charset-info "mac-dingbats" 34)


;;;; Keyboard layout/language change events
(defun mac-handle-language-change (event)
  (interactive "e")
  (let ((coding-system
	 (cdr (assq (car (cadr event)) mac-script-code-coding-systems))))
    (set-keyboard-coding-system (or coding-system 'mac-roman))
    ;; MacJapanese maps reverse solidus to ?\x80.
    (if (eq coding-system 'japanese-shift-jis)
	(define-key key-translation-map [?\x80] "\\"))))

(define-key special-event-map [language-change] 'mac-handle-language-change)

;;;; Selections and Services menu

;; Setup to use the Mac clipboard.
(set-selection-coding-system mac-system-coding-system)

;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-get-selection-value.
(defvar x-last-selected-text-clipboard nil
  "The value of the CLIPBOARD selection last time we selected or
pasted text.")
(defvar x-last-selected-text-primary nil
  "The value of the PRIMARY X selection last time we selected or
pasted text.")

(defcustom x-select-enable-clipboard t
  "*Non-nil means cutting and pasting uses the clipboard.
This is in addition to the primary selection."
  :type 'boolean
  :group 'killing)

;;; Make TEXT, a string, the primary X selection.
(defun x-select-text (text &optional push)
  (x-set-selection 'PRIMARY text)
  (setq x-last-selected-text-primary text)
  (if (not x-select-enable-clipboard)
      (setq x-last-selected-text-clipboard nil)
    (x-set-selection 'CLIPBOARD text)
    (setq x-last-selected-text-clipboard text))
  )

(defun x-get-selection (&optional type data-type)
  "Return the value of a selection.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING') says
how to convert the data.

TYPE may be any symbol \(but nil stands for `PRIMARY').  However,
only a few symbols are commonly used.  They conventionally have
all upper-case names.  The most often used ones, in addition to
`PRIMARY', are `SECONDARY' and `CLIPBOARD'.

DATA-TYPE is usually `STRING', but can also be one of the symbols
in `selection-converter-alist', which see."
  (let ((data (x-get-selection-internal (or type 'PRIMARY)
					(or data-type 'STRING)))
	(coding (or next-selection-coding-system
		    selection-coding-system)))
    (when (and (stringp data)
	       (setq data-type (get-text-property 0 'foreign-selection data)))
      (cond ((eq data-type 'public.utf16-plain-text)
	     (let ((encoded (and (fboundp 'mac-code-convert-string)
				 (mac-code-convert-string data
							  'utf-16 coding))))
	       (if encoded
		   (let ((coding-save last-coding-system-used))
		     (setq data (decode-coding-string encoded coding))
		     (setq last-coding-system-used coding-save))
		 (setq data
		       (decode-coding-string data 'utf-16)))))
	    ((eq data-type 'com.apple.traditional-mac-plain-text)
	     (setq data (decode-coding-string data coding)))
	    ((eq data-type 'public.file-url)
	     (setq data (decode-coding-string data 'utf-8))
	     ;; Remove a trailing nul character.
	     (let ((len (length data)))
	       (if (and (> len 0) (= (aref data (1- len)) ?\0))
		   (setq data (substring data 0 (1- len)))))))
      (put-text-property 0 (length data) 'foreign-selection data-type data))
    data))

(defun x-selection-value (type)
  (let ((data-types '(public.utf16-plain-text
		      com.apple.traditional-mac-plain-text
		      public.file-url))
	text tiff-image)
    (while (and (null text) data-types)
      (setq text (condition-case nil
		     (x-get-selection type (car data-types))
		   (error nil)))
      (setq data-types (cdr data-types)))
    (if text
	(remove-text-properties 0 (length text) '(foreign-selection nil) text))
    (setq tiff-image (condition-case nil
			 (x-get-selection type 'public.tiff)
		       (error nil)))
    (when tiff-image
      (remove-text-properties 0 (length tiff-image)
			      '(foreign-selection nil) tiff-image)
      (setq tiff-image (create-image tiff-image 'tiff t))
      (or text (setq text " "))
      (put-text-property 0 (length text) 'display tiff-image text))
    text))

;;; Return the value of the current selection.
;;; Treat empty strings as if they were unset.
;;; If this function is called twice and finds the same text,
;;; it returns nil the second time.  This is so that a single
;;; selection won't be added to the kill ring over and over.
(defun x-get-selection-value ()
  (let (clip-text primary-text)
    (if (not x-select-enable-clipboard)
	(setq x-last-selected-text-clipboard nil)
      (setq clip-text (x-selection-value 'CLIPBOARD))
      (if (string= clip-text "") (setq clip-text nil))

      ;; Check the CLIPBOARD selection for 'newness', is it different
      ;; from what we remebered them to be last time we did a
      ;; cut/paste operation.
      (setq clip-text
	    (cond;; check clipboard
	     ((or (not clip-text) (string= clip-text ""))
	      (setq x-last-selected-text-clipboard nil))
	     ((eq      clip-text x-last-selected-text-clipboard) nil)
	     ((string= clip-text x-last-selected-text-clipboard)
	      ;; Record the newer string,
	      ;; so subsequent calls can use the `eq' test.
	      (setq x-last-selected-text-clipboard clip-text)
	      nil)
	     (t
	      (setq x-last-selected-text-clipboard clip-text))))
      )

    (setq primary-text (x-selection-value 'PRIMARY))
    ;; Check the PRIMARY selection for 'newness', is it different
    ;; from what we remebered them to be last time we did a
    ;; cut/paste operation.
    (setq primary-text
	  (cond;; check primary selection
	   ((or (not primary-text) (string= primary-text ""))
	    (setq x-last-selected-text-primary nil))
	   ((eq      primary-text x-last-selected-text-primary) nil)
	   ((string= primary-text x-last-selected-text-primary)
	    ;; Record the newer string,
	    ;; so subsequent calls can use the `eq' test.
	    (setq x-last-selected-text-primary primary-text)
	    nil)
	   (t
	    (setq x-last-selected-text-primary primary-text))))

    ;; As we have done one selection, clear this now.
    (setq next-selection-coding-system nil)

    ;; At this point we have recorded the current values for the
    ;; selection from clipboard (if we are supposed to) and primary,
    ;; So return the first one that has changed (which is the first
    ;; non-null one).
    (or clip-text primary-text)
    ))

(put 'CLIPBOARD 'mac-scrap-name "com.apple.scrap.clipboard")
(when (eq system-type 'darwin)
  (put 'FIND 'mac-scrap-name "com.apple.scrap.find")
  (put 'PRIMARY 'mac-scrap-name
       (format "org.gnu.Emacs.%d.selection.PRIMARY" (emacs-pid))))
(put 'com.apple.traditional-mac-plain-text 'mac-ostype "TEXT")
(put 'public.utf16-plain-text 'mac-ostype "utxt")
(put 'public.tiff 'mac-ostype "TIFF")
(put 'public.file-url 'mac-ostype "furl")

(defun mac-select-convert-to-string (selection type value)
  (let ((str (cdr (xselect-convert-to-string selection nil value)))
	coding)
    (setq coding (or next-selection-coding-system selection-coding-system))
    (if coding
	(setq coding (coding-system-base coding))
      (setq coding 'raw-text))
    (when str
      ;; If TYPE is nil, this is a local request, thus return STR as
      ;; is.  Otherwise, encode STR.
      (if (not type)
	  str
	(let ((inhibit-read-only t))
	  (remove-text-properties 0 (length str) '(composition nil) str)
	  (cond
	   ((eq type 'public.utf16-plain-text)
	    (let (s)
	      (when (and (fboundp 'mac-code-convert-string)
			 (memq coding (find-coding-systems-string str)))
		(setq coding (coding-system-change-eol-conversion coding 'mac))
		(setq s (mac-code-convert-string
			 (encode-coding-string str coding)
			 coding 'utf-16)))
	      (setq str (or s (encode-coding-string str 'utf-16-mac)))))
	   ((eq type 'com.apple.traditional-mac-plain-text)
	    (let ((encodables (find-coding-systems-string str))
		  (rest mac-script-code-coding-systems))
	      (unless (memq coding encodables)
		(while (and rest (not (memq (cdar rest) encodables)))
		  (setq rest (cdr rest)))
		(if rest
		    (setq coding (cdar rest)))))
	    (setq coding (coding-system-change-eol-conversion coding 'mac))
	    (setq str (encode-coding-string str coding)))
	   (t
	    (error "Unknown selection type: %S" type))
	   )))

      (setq next-selection-coding-system nil)
      (cons type str))))

(defun mac-select-convert-to-file-url (selection type value)
  (let ((filename (xselect-convert-to-filename selection type value))
	(coding (or file-name-coding-system default-file-name-coding-system)))
    (if (and filename coding)
	(setq filename (encode-coding-string filename coding)))
    (and filename
	 (concat "file://localhost"
		 (mapconcat 'url-hexify-string
			    (split-string filename "/") "/")))))

(setq selection-converter-alist
      (nconc
       '((public.utf16-plain-text . mac-select-convert-to-string)
	 (com.apple.traditional-mac-plain-text . mac-select-convert-to-string)
	 ;; This is not enabled by default because the `Import Image'
	 ;; menu makes Emacs crash or hang for unknown reasons.
	 ;; (public.tiff . nil)
	 (public.file-url . mac-select-convert-to-file-url)
	 )
       selection-converter-alist))

(defun mac-services-open-file ()
  (interactive)
  (find-file-existing (x-selection-value mac-services-selection)))

(defun mac-services-open-selection ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*untitled*"))
  (insert (x-selection-value mac-services-selection))
  (sit-for 0)
  (save-buffer) ; It pops up the save dialog.
  )

(defun mac-services-mail-selection ()
  (interactive)
  (compose-mail)
  (rfc822-goto-eoh)
  (forward-line 1)
  (insert (x-selection-value mac-services-selection) "\n"))

(defun mac-services-mail-to ()
  (interactive)
  (compose-mail (x-selection-value mac-services-selection)))

(defun mac-services-insert-text ()
  (interactive)
  (let ((text (x-selection-value mac-services-selection)))
    (if (not buffer-read-only)
	(insert text)
      (kill-new text)
      (message
       (substitute-command-keys
	"The text from the Services menu can be accessed with \\[yank]")))))

(defvar mac-application-menu-map (make-sparse-keymap))
(define-key mac-application-menu-map [quit] 'save-buffers-kill-emacs)
(define-key mac-application-menu-map [services perform open-file]
  'mac-services-open-file)
(define-key mac-application-menu-map [services perform open-selection]
  'mac-services-open-selection)
(define-key mac-application-menu-map [services perform mail-selection]
  'mac-services-mail-selection)
(define-key mac-application-menu-map [services perform mail-to]
  'mac-services-mail-to)
(define-key mac-application-menu-map [services paste]
  'mac-services-insert-text)
(define-key mac-application-menu-map [preferences] 'customize)
(define-key mac-application-menu-map [about] 'display-splash-screen)
(global-set-key [menu-bar application] mac-application-menu-map)

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

(if (x-display-list)
    ;; On Mac OS 8/9, Most coding systems used in code conversion for
    ;; font names are not ready at the time when the terminal frame is
    ;; created.  So we reconstruct font name table for the initial
    ;; frame.
    (mac-clear-font-name-table)
  (x-open-connection "Mac"
		     x-command-line-resources
		     ;; Exit Emacs with fatal error if this fails.
		     t))

(setq frame-creation-function 'x-create-frame-with-faces)

(cp-make-coding-system
 mac-centraleurroman
 [?\,AD(B ?\$,1  (B ?\$,1 !(B ?\,AI(B ?\$,1 $(B ?\,AV(B ?\,A\(B ?\,Aa(B ?\$,1 %(B ?\$,1 ,(B ?\,Ad(B ?\$,1 -(B ?\$,1 &(B ?\$,1 '(B ?\,Ai(B ?\$,1!9(B
  ?\$,1!:(B ?\$,1 .(B ?\,Am(B ?\$,1 /(B ?\$,1 2(B ?\$,1 3(B ?\$,1 6(B ?\,As(B ?\$,1 7(B ?\,At(B ?\,Av(B ?\,Au(B ?\,Az(B ?\$,1 :(B ?\$,1 ;(B ?\,A|(B
  ?\$,1s (B ?\,A0(B ?\$,1 8(B ?\,A#(B ?\,A'(B ?\$,1s"(B ?\,A6(B ?\,A_(B ?\,A.(B ?\,A)(B ?\$,1ub(B ?\$,1 9(B ?\,A((B ?\$,1y (B ?\$,1 C(B ?\$,1 N(B
  ?\$,1 O(B ?\$,1 J(B ?\$,1y$(B ?\$,1y%(B ?\$,1 K(B ?\$,1 V(B ?\$,1x"(B ?\$,1x1(B ?\$,1 b(B ?\$,1 [(B ?\$,1 \(B ?\$,1 ](B ?\$,1 ^(B ?\$,1 Y(B ?\$,1 Z(B ?\$,1 e(B
  ?\$,1 f(B ?\$,1 c(B ?\,A,(B ?\$,1x:(B ?\$,1 d(B ?\$,1 g(B ?\$,1x&(B ?\,A+(B ?\,A;(B ?\$,1s&(B ?\,A (B ?\$,1 h(B ?\$,1 p(B ?\,AU(B ?\$,1 q(B ?\$,1 l(B
  ?\$,1rs(B ?\$,1rt(B ?\$,1r|(B ?\$,1r}(B ?\$,1rx(B ?\$,1ry(B ?\,Aw(B ?\$,2"*(B ?\$,1 m(B ?\$,1 t(B ?\$,1 u(B ?\$,1 x(B ?\$,1s9(B ?\$,1s:(B ?\$,1 y(B ?\$,1 v(B
  ?\$,1 w(B ?\$,1! (B ?\$,1rz(B ?\$,1r~(B ?\$,1!!(B ?\$,1 z(B ?\$,1 {(B ?\,AA(B ?\$,1!$(B ?\$,1!%(B ?\,AM(B ?\$,1!=(B ?\$,1!>(B ?\$,1!*(B ?\,AS(B ?\,AT(B
  ?\$,1!+(B ?\$,1!.(B ?\,AZ(B ?\$,1!/(B ?\$,1!0(B ?\$,1!1(B ?\$,1!2(B ?\$,1!3(B ?\,A](B ?\,A}(B ?\$,1 W(B ?\$,1!;(B ?\$,1 a(B ?\$,1!<(B ?\$,1 B(B ?\$,1$g(B]
 "Mac Central European Roman Encoding (MIME:x-mac-centraleurroman).")
(coding-system-put 'mac-centraleurroman 'mime-charset 'x-mac-centraleurroman)

(cp-make-coding-system
 mac-cyrillic
 [?\$,1(0(B ?\$,1(1(B ?\$,1(2(B ?\$,1(3(B ?\$,1(4(B ?\$,1(5(B ?\$,1(6(B ?\$,1(7(B ?\$,1(8(B ?\$,1(9(B ?\$,1(:(B ?\$,1(;(B ?\$,1(<(B ?\$,1(=(B ?\$,1(>(B ?\$,1(?(B
  ?\$,1(@(B ?\$,1(A(B ?\$,1(B(B ?\$,1(C(B ?\$,1(D(B ?\$,1(E(B ?\$,1(F(B ?\$,1(G(B ?\$,1(H(B ?\$,1(I(B ?\$,1(J(B ?\$,1(K(B ?\$,1(L(B ?\$,1(M(B ?\$,1(N(B ?\$,1(O(B
  ?\$,1s (B ?\,A0(B ?\$,1)P(B ?\,A#(B ?\,A'(B ?\$,1s"(B ?\,A6(B ?\$,1(&(B ?\,A.(B ?\,A)(B ?\$,1ub(B ?\$,1("(B ?\$,1(r(B ?\$,1y (B ?\$,1(#(B ?\$,1(s(B
  ?\$,1x>(B ?\,A1(B ?\$,1y$(B ?\$,1y%(B ?\$,1(v(B ?\,A5(B ?\$,1)Q(B ?\$,1(((B ?\$,1($(B ?\$,1(t(B ?\$,1('(B ?\$,1(w(B ?\$,1()(B ?\$,1(y(B ?\$,1(*(B ?\$,1(z(B
  ?\$,1(x(B ?\$,1(%(B ?\,A,(B ?\$,1x:(B ?\$,1!R(B ?\$,1xh(B ?\$,1x&(B ?\,A+(B ?\,A;(B ?\$,1s&(B ?\,A (B ?\$,1(+(B ?\$,1({(B ?\$,1(,(B ?\$,1(|(B ?\$,1(u(B
  ?\$,1rs(B ?\$,1rt(B ?\$,1r|(B ?\$,1r}(B ?\$,1rx(B ?\$,1ry(B ?\,Aw(B ?\$,1r~(B ?\$,1(.(B ?\$,1(~(B ?\$,1(/(B ?\$,1((B ?\$,1uV(B ?\$,1(!(B ?\$,1(q(B ?\$,1(o(B
  ?\$,1(P(B ?\$,1(Q(B ?\$,1(R(B ?\$,1(S(B ?\$,1(T(B ?\$,1(U(B ?\$,1(V(B ?\$,1(W(B ?\$,1(X(B ?\$,1(Y(B ?\$,1(Z(B ?\$,1([(B ?\$,1(\(B ?\$,1(](B ?\$,1(^(B ?\$,1(_(B
  ?\$,1(`(B ?\$,1(a(B ?\$,1(b(B ?\$,1(c(B ?\$,1(d(B ?\$,1(e(B ?\$,1(f(B ?\$,1(g(B ?\$,1(h(B ?\$,1(i(B ?\$,1(j(B ?\$,1(k(B ?\$,1(l(B ?\$,1(m(B ?\$,1(n(B ?\$,1tL(B]
 "Mac Cyrillic Encoding (MIME:x-mac-cyrillic).")
(coding-system-put 'mac-cyrillic 'mime-charset 'x-mac-cyrillic)

(let
    ((encoding-vector
      (vconcat
       (make-vector 32 nil)
       ;; mac-symbol (32..126) -> emacs-mule mapping
       [?\  ?\! ?\$,1x (B ?\# ?\$,1x#(B ?\% ?\& ?\$,1x-(B ?\( ?\) ?\$,1x7(B ?\+ ?\, ?\$,1x2(B ?\. ?\/
	?\0 ?\1 ?\2 ?\3 ?\4 ?\5 ?\6 ?\7 ?\8 ?\9 ?\: ?\; ?\< ?\= ?\> ?\?
	?\$,1xe(B ?\$,1&q(B ?\$,1&r(B ?\$,1''(B ?\$,1&t(B ?\$,1&u(B ?\$,1'&(B ?\$,1&s(B ?\$,1&w(B ?\$,1&y(B ?\$,1'Q(B ?\$,1&z(B ?\$,1&{(B ?\$,1&|(B ?\$,1&}(B ?\$,1&(B
	?\$,1' (B ?\$,1&x(B ?\$,1'!(B ?\$,1'#(B ?\$,1'$(B ?\$,1'%(B ?\$,1'B(B ?\$,1')(B ?\$,1&~(B ?\$,1'((B ?\$,1&v(B ?\[ ?\$,1xT(B ?\] ?\$,1ye(B ?\_
	?\$,3bE(B ?\$,1'1(B ?\$,1'2(B ?\$,1'G(B ?\$,1'4(B ?\$,1'5(B ?\$,1'F(B ?\$,1'3(B ?\$,1'7(B ?\$,1'9(B ?\$,1'U(B ?\$,1':(B ?\$,1';(B ?\$,1'<(B ?\$,1'=(B ?\$,1'?(B
	?\$,1'@(B ?\$,1'8(B ?\$,1'A(B ?\$,1'C(B ?\$,1'D(B ?\$,1'E(B ?\$,1'V(B ?\$,1'I(B ?\$,1'>(B ?\$,1'H(B ?\$,1'6(B ?\{ ?\| ?\} ?\$,1x\(B]
       (make-vector (- 160 127) nil)
       ;; mac-symbol (160..254) -> emacs-mule mapping
       ;; Mapping of the following characters are changed from the
       ;; original one:
       ;; 0xE2	0x00AE+0xF87F -> 0x00AE # REGISTERED SIGN, alternate: sans serif
       ;; 0xE3	0x00A9+0xF87F -> 0x00A9	# COPYRIGHT SIGN, alternate: sans serif
       ;; 0xE4	0x2122+0xF87F -> 0x2122	# TRADE MARK SIGN, alternate: sans serif
       [?\$,1tL(B ?\$,1'R(B ?\$,1s2(B ?\$,1y$(B ?\$,1sD(B ?\$,1x>(B ?\$,1!R(B ?\$,2#c(B ?\$,2#f(B ?\$,2#e(B ?\$,2#`(B ?\$,1vt(B ?\$,1vp(B ?\$,1vq(B ?\$,1vr(B ?\$,1vs(B
	?\,A0(B ?\,A1(B ?\$,1s3(B ?\$,1y%(B ?\,AW(B ?\$,1x=(B ?\$,1x"(B ?\$,1s"(B ?\,Aw(B ?\$,1y (B ?\$,1y!(B ?\$,1xh(B ?\$,1s&(B ?\$,1|p(B ?\$,1|O(B ?\$,1w5(B
	?\$,1uu(B ?\$,1uQ(B ?\$,1u\(B ?\$,1uX(B ?\$,1yW(B ?\$,1yU(B ?\$,1x%(B ?\$,1xI(B ?\$,1xJ(B ?\$,1yC(B ?\$,1yG(B ?\$,1yD(B ?\$,1yB(B ?\$,1yF(B ?\$,1x((B ?\$,1x)(B
	?\$,1x@(B ?\$,1x'(B ?\,A.(B ?\,A)(B ?\$,1ub(B ?\$,1x/(B ?\$,1x:(B ?\$,1z%(B ?\,A,(B ?\$,1xG(B ?\$,1xH(B ?\$,1wT(B ?\$,1wP(B ?\$,1wQ(B ?\$,1wR(B ?\$,1wS(B
	?\$,2"*(B ?\$,2=H(B ?\,A.(B ?\,A)(B ?\$,1ub(B ?\$,1x1(B ?\$,1|;(B ?\$,1|<(B ?\$,1|=(B ?\$,1|A(B ?\$,1|B(B ?\$,1|C(B ?\$,1|G(B ?\$,1|H(B ?\$,1|I(B ?\$,1|J(B
	?\$,3b_(B ?\$,2=I(B ?\$,1xK(B ?\$,1{ (B ?\$,1|N(B ?\$,1{!(B ?\$,1|>(B ?\$,1|?(B ?\$,1|@(B ?\$,1|D(B ?\$,1|E(B ?\$,1|F(B ?\$,1|K(B ?\$,1|L(B ?\$,1|M(B
	nil]))
     translation-table)
  (setq translation-table
	(make-translation-table-from-vector encoding-vector))
;;  (define-translation-table 'mac-symbol-decoder translation-table)
  (define-translation-table 'mac-symbol-encoder
    (char-table-extra-slot translation-table 0)))

(let
    ((encoding-vector
      (vconcat
       (make-vector 32 nil)
       ;; mac-dingbats (32..126) -> emacs-mule mapping
       [?\  ?\$,2%A(B ?\$,2%B(B ?\$,2%C(B ?\$,2%D(B ?\$,2"n(B ?\$,2%F(B ?\$,2%G(B ?\$,2%H(B ?\$,2%I(B ?\$,2"{(B ?\$,2"~(B ?\$,2%L(B ?\$,2%M(B ?\$,2%N(B ?\$,2%O(B
	?\$,2%P(B ?\$,2%Q(B ?\$,2%R(B ?\$,2%S(B ?\$,2%T(B ?\$,2%U(B ?\$,2%V(B ?\$,2%W(B ?\$,2%X(B ?\$,2%Y(B ?\$,2%Z(B ?\$,2%[(B ?\$,2%\(B ?\$,2%](B ?\$,2%^(B ?\$,2%_(B
	?\$,2%`(B ?\$,2%a(B ?\$,2%b(B ?\$,2%c(B ?\$,2%d(B ?\$,2%e(B ?\$,2%f(B ?\$,2%g(B ?\$,2"e(B ?\$,2%i(B ?\$,2%j(B ?\$,2%k(B ?\$,2%l(B ?\$,2%m(B ?\$,2%n(B ?\$,2%o(B
	?\$,2%p(B ?\$,2%q(B ?\$,2%r(B ?\$,2%s(B ?\$,2%t(B ?\$,2%u(B ?\$,2%v(B ?\$,2%w(B ?\$,2%x(B ?\$,2%y(B ?\$,2%z(B ?\$,2%{(B ?\$,2%|(B ?\$,2%}(B ?\$,2%~(B ?\$,2%(B
	?\$,2& (B ?\$,2&!(B ?\$,2&"(B ?\$,2&#(B ?\$,2&$(B ?\$,2&%(B ?\$,2&&(B ?\$,2&'(B ?\$,2&((B ?\$,2&)(B ?\$,2&*(B ?\$,2&+(B ?\$,2"/(B ?\$,2&-(B ?\$,2!`(B ?\$,2&/(B
	?\$,2&0(B ?\$,2&1(B ?\$,2&2(B ?\$,2!r(B ?\$,2!|(B ?\$,2"&(B ?\$,2&6(B ?\$,2"7(B ?\$,2&8(B ?\$,2&9(B ?\$,2&:(B ?\$,2&;(B ?\$,2&<(B ?\$,2&=(B ?\$,2&>(B
       nil
       ;; mac-dingbats (128..141) -> emacs-mule mapping
       ?\$,2&H(B ?\$,2&I(B ?\$,2&J(B ?\$,2&K(B ?\$,2&L(B ?\$,2&M(B ?\$,2&N(B ?\$,2&O(B ?\$,2&P(B ?\$,2&Q(B ?\$,2&R(B ?\$,2&S(B ?\$,2&T(B ?\$,2&U(B]
       (make-vector (- 161 142) nil)
       ;; mac-dingbats (161..239) -> emacs-mule mapping
       [?\$,2&A(B ?\$,2&B(B ?\$,2&C(B ?\$,2&D(B ?\$,2&E(B ?\$,2&F(B ?\$,2&G(B ?\$,2#c(B ?\$,2#f(B ?\$,2#e(B ?\$,2#`(B ?\$,1~@(B ?\$,1~A(B ?\$,1~B(B ?\$,1~C(B
	?\$,1~D(B ?\$,1~E(B ?\$,1~F(B ?\$,1~G(B ?\$,1~H(B ?\$,1~I(B ?\$,2&V(B ?\$,2&W(B ?\$,2&X(B ?\$,2&Y(B ?\$,2&Z(B ?\$,2&[(B ?\$,2&\(B ?\$,2&](B ?\$,2&^(B ?\$,2&_(B
	?\$,2&`(B ?\$,2&a(B ?\$,2&b(B ?\$,2&c(B ?\$,2&d(B ?\$,2&e(B ?\$,2&f(B ?\$,2&g(B ?\$,2&h(B ?\$,2&i(B ?\$,2&j(B ?\$,2&k(B ?\$,2&l(B ?\$,2&m(B ?\$,2&n(B ?\$,2&o(B
	?\$,2&p(B ?\$,2&q(B ?\$,2&r(B ?\$,2&s(B ?\$,2&t(B ?\$,1vr(B ?\$,1vt(B ?\$,1vu(B ?\$,2&x(B ?\$,2&y(B ?\$,2&z(B ?\$,2&{(B ?\$,2&|(B ?\$,2&}(B ?\$,2&~(B ?\$,2&(B
	?\$,2' (B ?\$,2'!(B ?\$,2'"(B ?\$,2'#(B ?\$,2'$(B ?\$,2'%(B ?\$,2'&(B ?\$,2''(B ?\$,2'((B ?\$,2')(B ?\$,2'*(B ?\$,2'+(B ?\$,2',(B ?\$,2'-(B ?\$,2'.(B ?\$,2'/(B
	nil
       ;; mac-dingbats (241..254) -> emacs-mule mapping
	?\$,2'1(B ?\$,2'2(B ?\$,2'3(B ?\$,2'4(B ?\$,2'5(B ?\$,2'6(B ?\$,2'7(B ?\$,2'8(B ?\$,2'9(B ?\$,2':(B ?\$,2';(B ?\$,2'<(B ?\$,2'=(B ?\$,2'>(B
	nil]))
     translation-table)
  (setq translation-table
	(make-translation-table-from-vector encoding-vector))
;;  (define-translation-table 'mac-dingbats-decoder translation-table)
  (define-translation-table 'mac-dingbats-encoder
    (char-table-extra-slot translation-table 0)))

(defvar mac-font-encoder-list
  '(("mac-roman" mac-roman-encoder
     ccl-encode-mac-roman-font "%s")
    ("mac-centraleurroman" encode-mac-centraleurroman
     ccl-encode-mac-centraleurroman-font "%s ce")
    ("mac-cyrillic" encode-mac-cyrillic
     ccl-encode-mac-cyrillic-font "%s cy")
    ("mac-symbol" mac-symbol-encoder
     ccl-encode-mac-symbol-font "symbol")
    ("mac-dingbats" mac-dingbats-encoder
     ccl-encode-mac-dingbats-font "zapf dingbats")))

(let ((encoder-list
       (mapcar (lambda (lst) (nth 1 lst)) mac-font-encoder-list))
      (charset-list
       '(latin-iso8859-2
	 latin-iso8859-3 latin-iso8859-4
	 cyrillic-iso8859-5 greek-iso8859-7 hebrew-iso8859-8
	 latin-iso8859-9 latin-iso8859-14 latin-iso8859-15)))
  (dolist (encoder encoder-list)
    (let ((table (get encoder 'translation-table)))
      (dolist (charset charset-list)
	(dotimes (i 96)
	  (let* ((c (make-char charset (+ i 32)))
		 (mu (aref ucs-mule-to-mule-unicode c))
		 (mac-encoded (and mu (aref table mu))))
	    (if mac-encoded
		(aset table c mac-encoded))))))))

;; We assume none of official dim2 charsets (0x90..0x99) are encoded
;; to these fonts.

(define-ccl-program ccl-encode-mac-roman-font
  `(0
    (if (r0 <= ?\xef)
	(translate-character mac-roman-encoder r0 r1)
      ((r1 <<= 7)
       (r1 |= r2)
       (translate-character mac-roman-encoder r0 r1))))
  "CCL program for Mac Roman font")

(define-ccl-program ccl-encode-mac-centraleurroman-font
  `(0
    (if (r0 <= ?\xef)
	(translate-character encode-mac-centraleurroman r0 r1)
      ((r1 <<= 7)
       (r1 |= r2)
       (translate-character encode-mac-centraleurroman r0 r1))))
  "CCL program for Mac Central European Roman font")

(define-ccl-program ccl-encode-mac-cyrillic-font
  `(0
    (if (r0 <= ?\xef)
	(translate-character encode-mac-cyrillic r0 r1)
      ((r1 <<= 7)
       (r1 |= r2)
       (translate-character encode-mac-cyrillic r0 r1))))
  "CCL program for Mac Cyrillic font")

(define-ccl-program ccl-encode-mac-symbol-font
  `(0
    (if (r0 <= ?\xef)
	(translate-character mac-symbol-encoder r0 r1)
      ((r1 <<= 7)
       (r1 |= r2)
       (translate-character mac-symbol-encoder r0 r1))))
  "CCL program for Mac Symbol font")

(define-ccl-program ccl-encode-mac-dingbats-font
  `(0
    (if (r0 <= ?\xef)
	(translate-character mac-dingbats-encoder r0 r1)
      ((r1 <<= 7)
       (r1 |= r2)
       (translate-character mac-dingbats-encoder r0 r1))))
  "CCL program for Mac Dingbats font")


(setq font-ccl-encoder-alist
      (nconc
       (mapcar (lambda (lst) (cons (nth 0 lst) (nth 2 lst)))
	       mac-font-encoder-list)
       font-ccl-encoder-alist))

(defconst mac-char-fontspec-list
  ;; Directly operate on a char-table instead of a fontset so that it
  ;; may not create a dummy fontset.
  (let ((template (make-char-table 'fontset)))
    (dolist
	(font-encoder
	 (nreverse
	  (mapcar (lambda (lst)
		    (cons (cons (nth 3 lst) (nth 0 lst)) (nth 1 lst)))
		  mac-font-encoder-list)))
      (let ((font (car font-encoder))
	    (encoder (cdr font-encoder)))
	(map-char-table
	 (lambda (key val)
	   (or (null val)
	       (generic-char-p key)
	       (memq (char-charset key)
		     '(ascii eight-bit-control eight-bit-graphic))
	       (aset template key font)))
	 (get encoder 'translation-table))))

    ;; Like fontset-info, but extend a range only if its "to" part is
    ;; the predecessor of the current char.
    (let* ((last '((0 nil)))
	   (accumulator last)
	   last-char-or-range last-char last-elt)
      (map-char-table
       (lambda (char elt)
	 (when elt
	   (setq last-char-or-range (car (car last))
		 last-char (if (consp last-char-or-range)
			       (cdr last-char-or-range)
			     last-char-or-range)
		 last-elt (cdr (car last)))
	   (if (and (eq elt last-elt)
		    (= char (1+ last-char))
		    (eq (char-charset char) (char-charset last-char)))
	       (if (consp last-char-or-range)
		   (setcdr last-char-or-range char)
		 (setcar (car last) (cons last-char char)))
	     (setcdr last (list (cons char elt)))
	     (setq last (cdr last)))))
       template)
      (cdr accumulator))))

(defun fontset-add-mac-fonts (fontset &optional base-family)
  "Add font-specs for Mac fonts to FONTSET.
The added font-specs are determined by BASE-FAMILY and the value
of `mac-char-fontspec-list', which is a list
of (CHARACTER-OR-RANGE . (FAMILY-FORMAT . REGISTRY)).  If
BASE-FAMILY is nil, the font family in the added font-specs is
also nil.  If BASE-FAMILY is a string, `%s' in FAMILY-FORMAT is
replaced with the string.  Otherwise, `%s' in FAMILY-FORMAT is
replaced with the ASCII font family name in FONTSET."
  (if base-family
      (if (stringp base-family)
	  (setq base-family (downcase base-family))
	(let ((ascii-font (fontset-font fontset (charset-id 'ascii))))
	  (if ascii-font
	      (setq base-family
		    (aref (x-decompose-font-name
			   (downcase (x-resolve-font-name ascii-font)))
			  xlfd-regexp-family-subnum))))))
  (let (fontspec-cache fontspec)
    (dolist (char-fontspec mac-char-fontspec-list)
      (setq fontspec (cdr (assq (cdr char-fontspec) fontspec-cache)))
      (when (null fontspec)
	(setq fontspec
	      (cons (and base-family
			 (format (car (cdr char-fontspec)) base-family))
		    (cdr (cdr char-fontspec))))
	(setq fontspec-cache (cons (cons (cdr char-fontspec) fontspec)
				   fontspec-cache)))
      (set-fontset-font fontset (car char-fontspec) fontspec))))

(defun create-fontset-from-mac-roman-font (font &optional resolved-font
						fontset-name)
  "Create a fontset from a Mac roman font FONT.

Optional 1st arg RESOLVED-FONT is a resolved name of FONT.  If
omitted, `x-resolve-font-name' is called to get the resolved name.  At
this time, if FONT is not available, error is signaled.

Optional 2nd arg FONTSET-NAME is a string to be used in
`<CHARSET_ENCODING>' fields of a new fontset name.  If it is omitted,
an appropriate name is generated automatically.

It returns a name of the created fontset."
  (let ((fontset
	 (create-fontset-from-ascii-font font resolved-font fontset-name)))
    (fontset-add-mac-fonts fontset t)
    fontset))

;; Setup the default fontset.
(setup-default-fontset)
;; Add Mac-encoding fonts unless ETL fonts are installed.
(unless (x-list-fonts "*-iso8859-1")
  (fontset-add-mac-fonts "fontset-default"))

;; Create a fontset that uses mac-roman font.  With this fontset,
;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
(create-fontset-from-fontset-spec
 "-etl-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-mac,
ascii:-*-Monaco-*-*-*-*-12-*-*-*-*-*-mac-roman")
(fontset-add-mac-fonts "fontset-mac" t)

;; Create fontset specified in X resources "Fontset-N" (N is 0, 1, ...).
(create-fontset-from-x-resource)

;; Try to create a fontset from a font specification which comes
;; from initial-frame-alist, default-frame-alist, or X resource.
;; A font specification in command line argument (i.e. -fn XXXX)
;; should be already in default-frame-alist as a `font'
;; parameter.  However, any font specifications in site-start
;; library, user's init file (.emacs), and default.el are not
;; yet handled here.

(let ((font (or (cdr (assq 'font initial-frame-alist))
		(cdr (assq 'font default-frame-alist))
		(x-get-resource "font" "Font")))
      xlfd-fields resolved-name)
  (if (and font
	   (not (query-fontset font))
	   (setq resolved-name (x-resolve-font-name font))
	   (setq xlfd-fields (x-decompose-font-name font)))
      (if (string= "fontset" (aref xlfd-fields xlfd-regexp-registry-subnum))
	  (new-fontset font (x-complement-fontset-spec xlfd-fields nil))
	;; Create a fontset from FONT.  The fontset name is
	;; generated from FONT.
	(if (and (string= "mac" (aref xlfd-fields xlfd-regexp-registry-subnum))
		 (string= "roman" (aref xlfd-fields xlfd-regexp-encoding-subnum)))
	    (create-fontset-from-mac-roman-font font resolved-name "startup")
	  (create-fontset-from-ascii-font font resolved-name "startup")))))

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

(defun x-win-suspend-error ()
  (error "Suspending an Emacs running under Mac makes no sense"))
(add-hook 'suspend-hook 'x-win-suspend-error)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)

(defalias 'x-cut-buffer-or-selection-value 'x-get-selection-value)

;;; Turn off window-splitting optimization; Mac is usually fast enough
;;; that this is only annoying.
(setq split-window-keep-point t)

;; Don't show the frame name; that's redundant.
(setq-default mode-line-frame-identification "  ")

;; Turn on support for mouse wheels.
(mouse-wheel-mode 1)


;; Enable CLIPBOARD copy/paste through menu bar commands.
(menu-bar-enable-clipboard)

(defun mac-drag-n-drop (event)
  "Edit the files listed in the drag-n-drop EVENT.
Switch to a buffer editing the last file dropped."
  (interactive "e")
  ;; Make sure the drop target has positive co-ords
  ;; before setting the selected frame - otherwise it
  ;; won't work.  <skx@tardis.ed.ac.uk>
  (let* ((window (posn-window (event-start event)))
	 (coords (posn-x-y (event-start event)))
	 (x (car coords))
	 (y (cdr coords)))
    (if (and (> x 0) (> y 0))
	(set-frame-selected-window nil window))
    (mapcar (lambda (file-name)
	      (if (listp file-name)
		  (let ((line (car file-name))
			(start (car (cdr file-name)))
			(end (car (cdr (cdr file-name)))))
		    (if (> line 0)
			(goto-line line)
		      (if (and (> start 0) (> end 0))
			  (progn (set-mark start)
				 (goto-char end)))))
		(dnd-handle-one-url window 'private
				    (concat "file:" file-name))))
	    (car (cdr (cdr event)))))
  (raise-frame))

(global-set-key [drag-n-drop] 'mac-drag-n-drop)

;; By checking whether the variable mac-ready-for-drag-n-drop has been
;; defined, the event loop in macterm.c can be informed that it can
;; now receive Finder drag and drop events.  Files dropped onto the
;; Emacs application icon can only be processed when the initial frame
;; has been created: this is where the files should be opened.
(add-hook 'after-init-hook
	  '(lambda ()
	     (defvar mac-ready-for-drag-n-drop t)))

;;;; Non-toolkit Scroll bars

(unless x-toolkit-scroll-bars

;; for debugging
;; (defun mac-handle-scroll-bar-event (event) (interactive "e") (princ event))

;;(global-set-key [vertical-scroll-bar mouse-1] 'mac-handle-scroll-bar-event)

(global-set-key
 [vertical-scroll-bar down-mouse-1]
 'mac-handle-scroll-bar-event)

(global-unset-key [vertical-scroll-bar drag-mouse-1])
(global-unset-key [vertical-scroll-bar mouse-1])

(defun mac-handle-scroll-bar-event (event)
  "Handle scroll bar EVENT to emulate Mac Toolbox style scrolling."
  (interactive "e")
  (let* ((position (event-start event))
	 (window (nth 0 position))
	 (bar-part (nth 4 position)))
    (select-window window)
    (cond
     ((eq bar-part 'up)
      (goto-char (window-start window))
      (mac-scroll-down-line))
     ((eq bar-part 'above-handle)
      (mac-scroll-down))
     ((eq bar-part 'handle)
      (scroll-bar-drag event))
     ((eq bar-part 'below-handle)
      (mac-scroll-up))
     ((eq bar-part 'down)
      (goto-char (window-start window))
      (mac-scroll-up-line)))))

(defun mac-scroll-ignore-events ()
  ;; Ignore confusing non-mouse events
  (while (not (memq (car-safe (read-event))
		    '(mouse-1 double-mouse-1 triple-mouse-1))) nil))

(defun mac-scroll-down ()
  (track-mouse
    (mac-scroll-ignore-events)
    (scroll-down)))

(defun mac-scroll-down-line ()
  (track-mouse
    (mac-scroll-ignore-events)
    (scroll-down 1)))

(defun mac-scroll-up ()
  (track-mouse
    (mac-scroll-ignore-events)
    (scroll-up)))

(defun mac-scroll-up-line ()
  (track-mouse
    (mac-scroll-ignore-events)
    (scroll-up 1)))

)

;;;; Others

(unless (eq system-type 'darwin)
  ;; This variable specifies the Unix program to call (as a process) to
  ;; determine the amount of free space on a file system (defaults to
  ;; df).  If it is not set to nil, ls-lisp will not work correctly
  ;; unless an external application df is implemented on the Mac.
  (setq directory-free-space-program nil)

  ;; Set this so that Emacs calls subprocesses with "sh" as shell to
  ;; expand filenames Note no subprocess for the shell is actually
  ;; started (see run_mac_command in sysdep.c).
  (setq shell-file-name "sh")

  ;; Some system variables are encoded with the system script code.
  (dolist (v '(system-name
	       emacs-build-system	; Mac OS 9 version cannot dump
	       user-login-name user-real-login-name user-full-name))
    (set v (decode-coding-string (symbol-value v) mac-system-coding-system))))

;; If Emacs is started from the Finder, change the default directory
;; to the user's home directory.
(if (string= default-directory "/")
    (cd "~"))

;; Darwin 6- pty breakage is now controlled from the C code so that
;; it applies to all builds on darwin.  See s/darwin.h PTY_ITERATION.
;; (setq process-connection-type t)

;; Assume that fonts are always scalable on the Mac.  This sometimes
;; results in characters with jagged edges.  However, without it,
;; fonts with both truetype and bitmap representations but no italic
;; or bold bitmap versions will not display these variants correctly.
(setq scalable-fonts-allowed t)

;; arch-tag: 71dfcd14-cde8-4d66-b05c-85ec94fb23a6
;;; mac-win.el ends here
