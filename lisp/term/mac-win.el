;;; mac-win.el --- parse switches controlling interface with Mac window system

;; Copyright (C) 1999, 2000, 2002, 2003, 2004  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
;;(require 'select)
(require 'menu-bar)
(require 'fontset)
(require 'x-dnd)

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
(define-key function-key-map [return] [?\C-m])
(define-key function-key-map [M-return] [?\M-\C-m])
(define-key function-key-map [tab] [?\t])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [backspace] [127])
(define-key function-key-map [M-backspace] [?\M-\d])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-escape] [?\M-\e])

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'return 'ascii-character 13)
(put 'tab 'ascii-character ?\t)
(put 'backspace 'ascii-character 127)
(put 'escape 'ascii-character ?\e)


;;;; Keysyms

;; Define constant values to be set to mac-keyboard-text-encoding
(defconst kTextEncodingMacRoman 0)
(defconst kTextEncodingISOLatin1 513 "0x201")
(defconst kTextEncodingISOLatin2 514 "0x202")


;;;; Selections and cut buffers

;; Setup to use the Mac clipboard.  The functions mac-cut-function and
;; mac-paste-function are defined in mac.c.
(set-selection-coding-system 'compound-text-mac)

(setq interprogram-cut-function
      '(lambda (str push)
	 (mac-cut-function
	  (encode-coding-string str selection-coding-system t) push)))

(setq interprogram-paste-function
      '(lambda ()
	 (let ((clipboard (mac-paste-function)))
	   (if clipboard
	       (decode-coding-string clipboard selection-coding-system t)))))


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

(define-ccl-program ccl-encode-mac-roman-font
  `(0
    (if (r0 != ,(charset-id 'ascii))
	(if (r0 <= ?\x8f)
	    (translate-character mac-roman-encoder r0 r1)
	  ((r1 <<= 7)
	   (r1 |= r2)
	   (translate-character mac-roman-encoder r0 r1)))))
  "CCL program for Mac Roman font")

(let
    ((encoding-vector (make-vector 256 nil))
     (i 0)
     (vec	;; mac-centraleurroman (128..255) -> UCS mapping
      [	#x00C4	;; 128:LATIN CAPITAL LETTER A WITH DIAERESIS
	#x0100	;; 129:LATIN CAPITAL LETTER A WITH MACRON
	#x0101	;; 130:LATIN SMALL LETTER A WITH MACRON
	#x00C9	;; 131:LATIN CAPITAL LETTER E WITH ACUTE
	#x0104	;; 132:LATIN CAPITAL LETTER A WITH OGONEK
	#x00D6	;; 133:LATIN CAPITAL LETTER O WITH DIAERESIS
	#x00DC	;; 134:LATIN CAPITAL LETTER U WITH DIAERESIS
	#x00E1	;; 135:LATIN SMALL LETTER A WITH ACUTE
	#x0105	;; 136:LATIN SMALL LETTER A WITH OGONEK
	#x010C	;; 137:LATIN CAPITAL LETTER C WITH CARON
	#x00E4	;; 138:LATIN SMALL LETTER A WITH DIAERESIS
	#x010D	;; 139:LATIN SMALL LETTER C WITH CARON
	#x0106	;; 140:LATIN CAPITAL LETTER C WITH ACUTE
	#x0107	;; 141:LATIN SMALL LETTER C WITH ACUTE
	#x00E9	;; 142:LATIN SMALL LETTER E WITH ACUTE
	#x0179	;; 143:LATIN CAPITAL LETTER Z WITH ACUTE
	#x017A	;; 144:LATIN SMALL LETTER Z WITH ACUTE
	#x010E	;; 145:LATIN CAPITAL LETTER D WITH CARON
	#x00ED	;; 146:LATIN SMALL LETTER I WITH ACUTE
	#x010F	;; 147:LATIN SMALL LETTER D WITH CARON
	#x0112	;; 148:LATIN CAPITAL LETTER E WITH MACRON
	#x0113	;; 149:LATIN SMALL LETTER E WITH MACRON
	#x0116	;; 150:LATIN CAPITAL LETTER E WITH DOT ABOVE
	#x00F3	;; 151:LATIN SMALL LETTER O WITH ACUTE
	#x0117	;; 152:LATIN SMALL LETTER E WITH DOT ABOVE
	#x00F4	;; 153:LATIN SMALL LETTER O WITH CIRCUMFLEX
	#x00F6	;; 154:LATIN SMALL LETTER O WITH DIAERESIS
	#x00F5	;; 155:LATIN SMALL LETTER O WITH TILDE
	#x00FA	;; 156:LATIN SMALL LETTER U WITH ACUTE
	#x011A	;; 157:LATIN CAPITAL LETTER E WITH CARON
	#x011B	;; 158:LATIN SMALL LETTER E WITH CARON
	#x00FC	;; 159:LATIN SMALL LETTER U WITH DIAERESIS
	#x2020	;; 160:DAGGER
	#x00B0	;; 161:DEGREE SIGN
	#x0118	;; 162:LATIN CAPITAL LETTER E WITH OGONEK
	#x00A3	;; 163:POUND SIGN
	#x00A7	;; 164:SECTION SIGN
	#x2022	;; 165:BULLET
	#x00B6	;; 166:PILCROW SIGN
	#x00DF	;; 167:LATIN SMALL LETTER SHARP S
	#x00AE	;; 168:REGISTERED SIGN
	#x00A9	;; 169:COPYRIGHT SIGN
	#x2122	;; 170:TRADE MARK SIGN
	#x0119	;; 171:LATIN SMALL LETTER E WITH OGONEK
	#x00A8	;; 172:DIAERESIS
	#x2260	;; 173:NOT EQUAL TO
	#x0123	;; 174:LATIN SMALL LETTER G WITH CEDILLA
	#x012E	;; 175:LATIN CAPITAL LETTER I WITH OGONEK
	#x012F	;; 176:LATIN SMALL LETTER I WITH OGONEK
	#x012A	;; 177:LATIN CAPITAL LETTER I WITH MACRON
	#x2264	;; 178:LESS-THAN OR EQUAL TO
	#x2265	;; 179:GREATER-THAN OR EQUAL TO
	#x012B	;; 180:LATIN SMALL LETTER I WITH MACRON
	#x0136	;; 181:LATIN CAPITAL LETTER K WITH CEDILLA
	#x2202	;; 182:PARTIAL DIFFERENTIAL
	#x2211	;; 183:N-ARY SUMMATION
	#x0142	;; 184:LATIN SMALL LETTER L WITH STROKE
	#x013B	;; 185:LATIN CAPITAL LETTER L WITH CEDILLA
	#x013C	;; 186:LATIN SMALL LETTER L WITH CEDILLA
	#x013D	;; 187:LATIN CAPITAL LETTER L WITH CARON
	#x013E	;; 188:LATIN SMALL LETTER L WITH CARON
	#x0139	;; 189:LATIN CAPITAL LETTER L WITH ACUTE
	#x013A	;; 190:LATIN SMALL LETTER L WITH ACUTE
	#x0145	;; 191:LATIN CAPITAL LETTER N WITH CEDILLA
	#x0146	;; 192:LATIN SMALL LETTER N WITH CEDILLA
	#x0143	;; 193:LATIN CAPITAL LETTER N WITH ACUTE
	#x00AC	;; 194:NOT SIGN
	#x221A	;; 195:SQUARE ROOT
	#x0144	;; 196:LATIN SMALL LETTER N WITH ACUTE
	#x0147	;; 197:LATIN CAPITAL LETTER N WITH CARON
	#x2206	;; 198:INCREMENT
	#x00AB	;; 199:LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	#x00BB	;; 200:RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	#x2026	;; 201:HORIZONTAL ELLIPSIS
	#x00A0	;; 202:NO-BREAK SPACE
	#x0148	;; 203:LATIN SMALL LETTER N WITH CARON
	#x0150	;; 204:LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
	#x00D5	;; 205:LATIN CAPITAL LETTER O WITH TILDE
	#x0151	;; 206:LATIN SMALL LETTER O WITH DOUBLE ACUTE
	#x014C	;; 207:LATIN CAPITAL LETTER O WITH MACRON
	#x2013	;; 208:EN DASH
	#x2014	;; 209:EM DASH
	#x201C	;; 210:LEFT DOUBLE QUOTATION MARK
	#x201D	;; 211:RIGHT DOUBLE QUOTATION MARK
	#x2018	;; 212:LEFT SINGLE QUOTATION MARK
	#x2019	;; 213:RIGHT SINGLE QUOTATION MARK
	#x00F7	;; 214:DIVISION SIGN
	#x25CA	;; 215:LOZENGE
	#x014D	;; 216:LATIN SMALL LETTER O WITH MACRON
	#x0154	;; 217:LATIN CAPITAL LETTER R WITH ACUTE
	#x0155	;; 218:LATIN SMALL LETTER R WITH ACUTE
	#x0158	;; 219:LATIN CAPITAL LETTER R WITH CARON
	#x2039	;; 220:SINGLE LEFT-POINTING ANGLE QUOTATION MARK
	#x203A	;; 221:SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
	#x0159	;; 222:LATIN SMALL LETTER R WITH CARON
	#x0156	;; 223:LATIN CAPITAL LETTER R WITH CEDILLA
	#x0157	;; 224:LATIN SMALL LETTER R WITH CEDILLA
	#x0160	;; 225:LATIN CAPITAL LETTER S WITH CARON
	#x201A	;; 226:SINGLE LOW-9 QUOTATION MARK
	#x201E	;; 227:DOUBLE LOW-9 QUOTATION MARK
	#x0161	;; 228:LATIN SMALL LETTER S WITH CARON
	#x015A	;; 229:LATIN CAPITAL LETTER S WITH ACUTE
	#x015B	;; 230:LATIN SMALL LETTER S WITH ACUTE
	#x00C1	;; 231:LATIN CAPITAL LETTER A WITH ACUTE
	#x0164	;; 232:LATIN CAPITAL LETTER T WITH CARON
	#x0165	;; 233:LATIN SMALL LETTER T WITH CARON
	#x00CD	;; 234:LATIN CAPITAL LETTER I WITH ACUTE
	#x017D	;; 235:LATIN CAPITAL LETTER Z WITH CARON
	#x017E	;; 236:LATIN SMALL LETTER Z WITH CARON
	#x016A	;; 237:LATIN CAPITAL LETTER U WITH MACRON
	#x00D3	;; 238:LATIN CAPITAL LETTER O WITH ACUTE
	#x00D4	;; 239:LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	#x016B	;; 240:LATIN SMALL LETTER U WITH MACRON
	#x016E	;; 241:LATIN CAPITAL LETTER U WITH RING ABOVE
	#x00DA	;; 242:LATIN CAPITAL LETTER U WITH ACUTE
	#x016F	;; 243:LATIN SMALL LETTER U WITH RING ABOVE
	#x0170	;; 244:LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
	#x0171	;; 245:LATIN SMALL LETTER U WITH DOUBLE ACUTE
	#x0172	;; 246:LATIN CAPITAL LETTER U WITH OGONEK
	#x0173	;; 247:LATIN SMALL LETTER U WITH OGONEK
	#x00DD	;; 248:LATIN CAPITAL LETTER Y WITH ACUTE
	#x00FD	;; 249:LATIN SMALL LETTER Y WITH ACUTE
	#x0137	;; 250:LATIN SMALL LETTER K WITH CEDILLA
	#x017B	;; 251:LATIN CAPITAL LETTER Z WITH DOT ABOVE
	#x0141	;; 252:LATIN CAPITAL LETTER L WITH STROKE
	#x017C	;; 253:LATIN SMALL LETTER Z WITH DOT ABOVE
	#x0122	;; 254:LATIN CAPITAL LETTER G WITH CEDILLA
	#x02C7	;; 255:CARON
	])
     translation-table)
  (while (< i 128)
    (aset encoding-vector i i)
    (setq i (1+ i)))
  (while (< i 256)
    (aset encoding-vector i
	  (decode-char 'ucs (aref vec (- i 128))))
    (setq i (1+ i)))
  (setq translation-table
	(make-translation-table-from-vector encoding-vector))
;;  (define-translation-table 'mac-centraleurroman-decoder translation-table)
  (define-translation-table 'mac-centraleurroman-encoder
    (char-table-extra-slot translation-table 0)))

(let
    ((encoding-vector (make-vector 256 nil))
     (i 0)
     (vec	;; mac-cyrillic (128..255) -> UCS mapping
      [	#x0410	;; 128:CYRILLIC CAPITAL LETTER A
	#x0411	;; 129:CYRILLIC CAPITAL LETTER BE
	#x0412	;; 130:CYRILLIC CAPITAL LETTER VE
	#x0413	;; 131:CYRILLIC CAPITAL LETTER GHE
	#x0414	;; 132:CYRILLIC CAPITAL LETTER DE
	#x0415	;; 133:CYRILLIC CAPITAL LETTER IE
	#x0416	;; 134:CYRILLIC CAPITAL LETTER ZHE
	#x0417	;; 135:CYRILLIC CAPITAL LETTER ZE
	#x0418	;; 136:CYRILLIC CAPITAL LETTER I
	#x0419	;; 137:CYRILLIC CAPITAL LETTER SHORT I
	#x041A	;; 138:CYRILLIC CAPITAL LETTER KA
	#x041B	;; 139:CYRILLIC CAPITAL LETTER EL
	#x041C	;; 140:CYRILLIC CAPITAL LETTER EM
	#x041D	;; 141:CYRILLIC CAPITAL LETTER EN
	#x041E	;; 142:CYRILLIC CAPITAL LETTER O
	#x041F	;; 143:CYRILLIC CAPITAL LETTER PE
	#x0420	;; 144:CYRILLIC CAPITAL LETTER ER
	#x0421	;; 145:CYRILLIC CAPITAL LETTER ES
	#x0422	;; 146:CYRILLIC CAPITAL LETTER TE
	#x0423	;; 147:CYRILLIC CAPITAL LETTER U
	#x0424	;; 148:CYRILLIC CAPITAL LETTER EF
	#x0425	;; 149:CYRILLIC CAPITAL LETTER HA
	#x0426	;; 150:CYRILLIC CAPITAL LETTER TSE
	#x0427	;; 151:CYRILLIC CAPITAL LETTER CHE
	#x0428	;; 152:CYRILLIC CAPITAL LETTER SHA
	#x0429	;; 153:CYRILLIC CAPITAL LETTER SHCHA
	#x042A	;; 154:CYRILLIC CAPITAL LETTER HARD SIGN
	#x042B	;; 155:CYRILLIC CAPITAL LETTER YERU
	#x042C	;; 156:CYRILLIC CAPITAL LETTER SOFT SIGN
	#x042D	;; 157:CYRILLIC CAPITAL LETTER E
	#x042E	;; 158:CYRILLIC CAPITAL LETTER YU
	#x042F	;; 159:CYRILLIC CAPITAL LETTER YA
	#x2020	;; 160:DAGGER
	#x00B0	;; 161:DEGREE SIGN
	#x0490	;; 162:CYRILLIC CAPITAL LETTER GHE WITH UPTURN
	#x00A3	;; 163:POUND SIGN
	#x00A7	;; 164:SECTION SIGN
	#x2022	;; 165:BULLET
	#x00B6	;; 166:PILCROW SIGN
	#x0406	;; 167:CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
	#x00AE	;; 168:REGISTERED SIGN
	#x00A9	;; 169:COPYRIGHT SIGN
	#x2122	;; 170:TRADE MARK SIGN
	#x0402	;; 171:CYRILLIC CAPITAL LETTER DJE
	#x0452	;; 172:CYRILLIC SMALL LETTER DJE
	#x2260	;; 173:NOT EQUAL TO
	#x0403	;; 174:CYRILLIC CAPITAL LETTER GJE
	#x0453	;; 175:CYRILLIC SMALL LETTER GJE
	#x221E	;; 176:INFINITY
	#x00B1	;; 177:PLUS-MINUS SIGN
	#x2264	;; 178:LESS-THAN OR EQUAL TO
	#x2265	;; 179:GREATER-THAN OR EQUAL TO
	#x0456	;; 180:CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
	#x00B5	;; 181:MICRO SIGN
	#x0491	;; 182:CYRILLIC SMALL LETTER GHE WITH UPTURN
	#x0408	;; 183:CYRILLIC CAPITAL LETTER JE
	#x0404	;; 184:CYRILLIC CAPITAL LETTER UKRAINIAN IE
	#x0454	;; 185:CYRILLIC SMALL LETTER UKRAINIAN IE
	#x0407	;; 186:CYRILLIC CAPITAL LETTER YI
	#x0457	;; 187:CYRILLIC SMALL LETTER YI
	#x0409	;; 188:CYRILLIC CAPITAL LETTER LJE
	#x0459	;; 189:CYRILLIC SMALL LETTER LJE
	#x040A	;; 190:CYRILLIC CAPITAL LETTER NJE
	#x045A	;; 191:CYRILLIC SMALL LETTER NJE
	#x0458	;; 192:CYRILLIC SMALL LETTER JE
	#x0405	;; 193:CYRILLIC CAPITAL LETTER DZE
	#x00AC	;; 194:NOT SIGN
	#x221A	;; 195:SQUARE ROOT
	#x0192	;; 196:LATIN SMALL LETTER F WITH HOOK
	#x2248	;; 197:ALMOST EQUAL TO
	#x2206	;; 198:INCREMENT
	#x00AB	;; 199:LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	#x00BB	;; 200:RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	#x2026	;; 201:HORIZONTAL ELLIPSIS
	#x00A0	;; 202:NO-BREAK SPACE
	#x040B	;; 203:CYRILLIC CAPITAL LETTER TSHE
	#x045B	;; 204:CYRILLIC SMALL LETTER TSHE
	#x040C	;; 205:CYRILLIC CAPITAL LETTER KJE
	#x045C	;; 206:CYRILLIC SMALL LETTER KJE
	#x0455	;; 207:CYRILLIC SMALL LETTER DZE
	#x2013	;; 208:EN DASH
	#x2014	;; 209:EM DASH
	#x201C	;; 210:LEFT DOUBLE QUOTATION MARK
	#x201D	;; 211:RIGHT DOUBLE QUOTATION MARK
	#x2018	;; 212:LEFT SINGLE QUOTATION MARK
	#x2019	;; 213:RIGHT SINGLE QUOTATION MARK
	#x00F7	;; 214:DIVISION SIGN
	#x201E	;; 215:DOUBLE LOW-9 QUOTATION MARK
	#x040E	;; 216:CYRILLIC CAPITAL LETTER SHORT U
	#x045E	;; 217:CYRILLIC SMALL LETTER SHORT U
	#x040F	;; 218:CYRILLIC CAPITAL LETTER DZHE
	#x045F	;; 219:CYRILLIC SMALL LETTER DZHE
	#x2116	;; 220:NUMERO SIGN
	#x0401	;; 221:CYRILLIC CAPITAL LETTER IO
	#x0451	;; 222:CYRILLIC SMALL LETTER IO
	#x044F	;; 223:CYRILLIC SMALL LETTER YA
	#x0430	;; 224:CYRILLIC SMALL LETTER A
	#x0431	;; 225:CYRILLIC SMALL LETTER BE
	#x0432	;; 226:CYRILLIC SMALL LETTER VE
	#x0433	;; 227:CYRILLIC SMALL LETTER GHE
	#x0434	;; 228:CYRILLIC SMALL LETTER DE
	#x0435	;; 229:CYRILLIC SMALL LETTER IE
	#x0436	;; 230:CYRILLIC SMALL LETTER ZHE
	#x0437	;; 231:CYRILLIC SMALL LETTER ZE
	#x0438	;; 232:CYRILLIC SMALL LETTER I
	#x0439	;; 233:CYRILLIC SMALL LETTER SHORT I
	#x043A	;; 234:CYRILLIC SMALL LETTER KA
	#x043B	;; 235:CYRILLIC SMALL LETTER EL
	#x043C	;; 236:CYRILLIC SMALL LETTER EM
	#x043D	;; 237:CYRILLIC SMALL LETTER EN
	#x043E	;; 238:CYRILLIC SMALL LETTER O
	#x043F	;; 239:CYRILLIC SMALL LETTER PE
	#x0440	;; 240:CYRILLIC SMALL LETTER ER
	#x0441	;; 241:CYRILLIC SMALL LETTER ES
	#x0442	;; 242:CYRILLIC SMALL LETTER TE
	#x0443	;; 243:CYRILLIC SMALL LETTER U
	#x0444	;; 244:CYRILLIC SMALL LETTER EF
	#x0445	;; 245:CYRILLIC SMALL LETTER HA
	#x0446	;; 246:CYRILLIC SMALL LETTER TSE
	#x0447	;; 247:CYRILLIC SMALL LETTER CHE
	#x0448	;; 248:CYRILLIC SMALL LETTER SHA
	#x0449	;; 249:CYRILLIC SMALL LETTER SHCHA
	#x044A	;; 250:CYRILLIC SMALL LETTER HARD SIGN
	#x044B	;; 251:CYRILLIC SMALL LETTER YERU
	#x044C	;; 252:CYRILLIC SMALL LETTER SOFT SIGN
	#x044D	;; 253:CYRILLIC SMALL LETTER E
	#x044E	;; 254:CYRILLIC SMALL LETTER YU
	#x20AC	;; 255:EURO SIGN
	])
     translation-table)
  (while (< i 128)
    (aset encoding-vector i i)
    (setq i (1+ i)))
  (while (< i 256)
    (aset encoding-vector i
	  (decode-char 'ucs (aref vec (- i 128))))
    (setq i (1+ i)))
  (setq translation-table
	(make-translation-table-from-vector encoding-vector))
;;  (define-translation-table 'mac-cyrillic-decoder translation-table)
  (define-translation-table 'mac-cyrillic-encoder
    (char-table-extra-slot translation-table 0)))

(defvar mac-font-encoder-list
  '(("mac-roman" mac-roman-encoder
     ccl-encode-mac-roman-font "%s")
    ("mac-centraleurroman" mac-centraleurroman-encoder
     ccl-encode-mac-centraleurroman-font "%s ce")
    ("mac-cyrillic" mac-cyrillic-encoder
     ccl-encode-mac-cyrillic-font "%s cy")))

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

(define-ccl-program ccl-encode-mac-centraleurroman-font
  `(0
    (if (r0 != ,(charset-id 'ascii))
	(if (r0 <= ?\x8f)
	    (translate-character mac-centraleurroman-encoder r0 r1)
	  ((r1 <<= 7)
	   (r1 |= r2)
	   (translate-character mac-centraleurroman-encoder r0 r1)))))
  "CCL program for Mac Central European Roman font")

(define-ccl-program ccl-encode-mac-cyrillic-font
  `(0
    (if (r0 != ,(charset-id 'ascii))
	(if (r0 <= ?\x8f)
	    (translate-character mac-cyrillic-encoder r0 r1)
	  ((r1 <<= 7)
	   (r1 |= r2)
	   (translate-character mac-cyrillic-encoder r0 r1)))))
  "CCL program for Mac Cyrillic font")


(setq font-ccl-encoder-alist
      (nconc
       (mapcar (lambda (lst) (cons (nth 0 lst) (nth 2 lst)))
	       mac-font-encoder-list)
       font-ccl-encoder-alist))

(defun fontset-add-mac-fonts (fontset &optional base-family)
  (if base-family
      (setq base-family (downcase base-family))
    (let ((ascii-font
	   (downcase (x-resolve-font-name
		      (fontset-font fontset (charset-id 'ascii))))))
      (setq base-family (aref (x-decompose-font-name ascii-font)
			      xlfd-regexp-family-subnum))))
;;  (if (not (string-match "^fontset-" fontset))
;;      (setq fontset
;;	    (concat "fontset-" (aref (x-decompose-font-name fontset)
;;				     xlfd-regexp-encoding-subnum))))
  (dolist
      (font-encoder
       (nreverse
	(mapcar (lambda (lst)
		  (cons (cons (format (nth 3 lst) base-family) (nth 0 lst))
			(nth 1 lst)))
		mac-font-encoder-list)))
    (let ((font (car font-encoder))
	  (encoder (cdr font-encoder)))
      (map-char-table
       (lambda (key val)
	 (or (null val)
	     (generic-char-p key)
	     (memq (char-charset key)
		   '(ascii eight-bit-control eight-bit-graphic))
	     (set-fontset-font fontset key font)))
       (get encoder 'translation-table)))))
 
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
    (fontset-add-mac-fonts fontset)
    fontset))

;; Setup the default fontset.
(setup-default-fontset)

;; Create a fontset that uses mac-roman font.  With this fontset,
;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
(create-fontset-from-fontset-spec
 "-etl-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-mac,
ascii:-*-Monaco-*-*-*-*-12-*-*-*-*-*-mac-roman")
(fontset-add-mac-fonts "fontset-mac")

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
	(create-fontset-from-ascii-font font resolved-name "startup"))))

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

;; Don't show the frame name; that's redundant.
(setq-default mode-line-frame-identification "  ")

;; Turn on support for mouse wheels.
(mouse-wheel-mode 1)

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
		(x-dnd-handle-one-url window 'private
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

;;;; Scroll bars

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


;;;; Others

(unless (eq system-type 'darwin)
  ;; This variable specifies the Unix program to call (as a process) to
  ;; deteremine the amount of free space on a file system (defaults to
  ;; df).  If it is not set to nil, ls-lisp will not work correctly
  ;; unless an external application df is implemented on the Mac.
  (setq directory-free-space-program nil)

  ;; Set this so that Emacs calls subprocesses with "sh" as shell to
  ;; expand filenames Note no subprocess for the shell is actually
  ;; started (see run_mac_command in sysdep.c).
  (setq shell-file-name "sh"))

;; X Window emulation in macterm.c is not complete enough to start a
;; frame without a minibuffer properly.  Call this to tell ediff
;; library to use a single frame.
; (ediff-toggle-multiframe)

(if (eq system-type 'darwin)
    ;; On Darwin filenames are encoded in UTF-8
    (setq file-name-coding-system 'utf-8)
  ;; To display filenames in Chinese or Japanese, replace mac-roman with
  ;; big5 or sjis
  (setq file-name-coding-system 'mac-roman))

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

;; (prefer-coding-system 'mac-roman)

;;; arch-tag: 71dfcd14-cde8-4d66-b05c-85ec94fb23a6
;;; mac-win.el ends here
