;;; mac-win.el --- parse switches controlling interface with Mac window system -*-coding: utf-8-*-

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Andrew Choi <akochoi@mac.com>
;; Keywords: terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; -iconic		.iconic
;; -name		.name
;; -reverse		*reverseVideo
;; -rv			*reverseVideo
;; -selectionTimeout    .selectionTimeout
;; -synchronous		*synchronous
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

;; (if (not (eq window-system 'mac))
;;     (error "%s: Loading mac-win.el but not compiled for Mac" (invocation-name)))

(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(defvar mac-charset-info-alist)
(defvar mac-service-selection)
(defvar mac-system-script-code)
(defvar mac-apple-event-map)
(defvar mac-font-panel-mode)
(defvar mac-ts-active-input-overlay)
(defvar mac-ts-active-input-buf)
(defvar x-invocation-args)
(declare-function mac-code-convert-string "mac.c")
(declare-function mac-coerce-ae-data "mac.c")
(declare-function mac-resume-apple-event "macselect.c")
;; Suppress warning when compiling on non-Mac.
(declare-function mac-font-panel-mode "mac-win.el")
(declare-function mac-atsu-font-face-attributes "macfns.c")
(declare-function mac-ae-set-reply-parameter "macselect.c")
(declare-function mac-clear-font-name-table "macfns.c")

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

(defun x-setup-function-keys (frame)
  "Setup Function Keys for mac."
  ;; Don't do this twice on the same display, or it would break
  ;; normal-erase-is-backspace-mode.
  (unless (terminal-parameter frame 'x-setup-function-keys)
    (with-selected-frame frame
      ;; Map certain keypad keys into ASCII characters
      ;; that people usually expect.
      (define-key local-function-key-map [backspace] [?\d])
      (define-key local-function-key-map [delete] [?\d])
      (define-key local-function-key-map [tab] [?\t])
      (define-key local-function-key-map [linefeed] [?\n])
      (define-key local-function-key-map [clear] [?\C-l])
      (define-key local-function-key-map [return] [?\C-m])
      (define-key local-function-key-map [escape] [?\e])
      (define-key local-function-key-map [M-backspace] [?\M-\d])
      (define-key local-function-key-map [M-delete] [?\M-\d])
      (define-key local-function-key-map [M-tab] [?\M-\t])
      (define-key local-function-key-map [M-linefeed] [?\M-\n])
      (define-key local-function-key-map [M-clear] [?\M-\C-l])
      (define-key local-function-key-map [M-return] [?\M-\C-m])
      (define-key local-function-key-map [M-escape] [?\M-\e])
      (substitute-key-definition 'suspend-emacs 'iconify-or-deiconify-frame
				 local-function-key-map global-map))
    (set-terminal-parameter frame 'x-setup-function-keys t)))

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'backspace 'ascii-character ?\d)
(put 'delete 'ascii-character ?\d)
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character ?\C-l)
(put 'return 'ascii-character ?\C-m)
(put 'escape 'ascii-character ?\e)

;; Modifier name `ctrl' is an alias of `control'.
(put 'ctrl 'modifier-value (get 'control 'modifier-value))


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

(defun mac-add-charset-info (xlfd-charset mac-text-encoding)
  "Add a character set to display with Mac fonts.
Create an entry in `mac-charset-info-alist'.
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
(mac-add-charset-info "iso10646-1" 126) ; for ATSUI

(define-charset 'mac-centraleurroman
  "Mac Central European Roman"
  :short-name "Mac CE"
  :ascii-compatible-p t
  :code-space [0 255]
  :map
  (let ((tbl
	 [?\Ä ?\Ā ?\ā ?\É ?\Ą ?\Ö ?\Ü ?\á ?\ą ?\Č ?\ä ?\č ?\Ć ?\ć ?\é ?\Ź
	  ?\ź ?\Ď ?\í ?\ď ?\Ē ?\ē ?\Ė ?\ó ?\ė ?\ô ?\ö ?\õ ?\ú ?\Ě ?\ě ?\ü
	  ?\† ?\° ?\Ę ?\£ ?\§ ?\• ?\¶ ?\ß ?\® ?\© ?\™ ?\ę ?\¨ ?\≠ ?\ģ ?\Į
	  ?\į ?\Ī ?\≤ ?\≥ ?\ī ?\Ķ ?\∂ ?\∑ ?\ł ?\Ļ ?\ļ ?\Ľ ?\ľ ?\Ĺ ?\ĺ ?\Ņ
	  ?\ņ ?\Ń ?\¬ ?\√ ?\ń ?\Ň ?\∆ ?\« ?\» ?\… ?\  ?\ň ?\Ő ?\Õ ?\ő ?\Ō
	  ?\– ?\— ?\“ ?\” ?\‘ ?\’ ?\÷ ?\◊ ?\ō ?\Ŕ ?\ŕ ?\Ř ?\‹ ?\› ?\ř ?\Ŗ
	  ?\ŗ ?\Š ?\‚ ?\„ ?\š ?\Ś ?\ś ?\Á ?\Ť ?\ť ?\Í ?\Ž ?\ž ?\Ū ?\Ó ?\Ô
	  ?\ū ?\Ů ?\Ú ?\ů ?\Ű ?\ű ?\Ų ?\ų ?\Ý ?\ý ?\ķ ?\Ż ?\Ł ?\ż ?\Ģ ?\ˇ])
	(map (make-vector 512 nil)))
    (or (= (length tbl) 128)
	(error "Invalid vector length: %d" (length tbl)))
    (dotimes (i 128)
      (aset map (* i 2) i)
      (aset map (1+ (* i 2)) i))
    (dotimes (i 128)
      (aset map (+ 256 (* i 2)) (+ 128 i))
      (aset map (+ 256 (1+ (* i 2))) (aref tbl i)))
    map))

(define-coding-system 'mac-centraleurroman
  "Mac Central European Roman Encoding (MIME:x-mac-centraleurroman)."
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(mac-centraleurroman)
  :mime-charset 'x-mac-centraleurroman)

(define-charset 'mac-cyrillic
  "Mac Cyrillic"
  :short-name "Mac CYRILLIC"
  :ascii-compatible-p t
  :code-space [0 255]
  :map
  (let ((tbl
	 [?\А ?\Б ?\В ?\Г ?\Д ?\Е ?\Ж ?\З ?\И ?\Й ?\К ?\Л ?\М ?\Н ?\О ?\П
	  ?\Р ?\С ?\Т ?\У ?\Ф ?\Х ?\Ц ?\Ч ?\Ш ?\Щ ?\Ъ ?\Ы ?\Ь ?\Э ?\Ю ?\Я
	  ?\† ?\° ?\Ґ ?\£ ?\§ ?\• ?\¶ ?\І ?\® ?\© ?\™ ?\Ђ ?\ђ ?\≠ ?\Ѓ ?\ѓ
	  ?\∞ ?\± ?\≤ ?\≥ ?\і ?\µ ?\ґ ?\Ј ?\Є ?\є ?\Ї ?\ї ?\Љ ?\љ ?\Њ ?\њ
	  ?\ј ?\Ѕ ?\¬ ?\√ ?\ƒ ?\≈ ?\∆ ?\« ?\» ?\… ?\  ?\Ћ ?\ћ ?\Ќ ?\ќ ?\ѕ
	  ?\– ?\— ?\“ ?\” ?\‘ ?\’ ?\÷ ?\„ ?\Ў ?\ў ?\Џ ?\џ ?\№ ?\Ё ?\ё ?\я
	  ?\а ?\б ?\в ?\г ?\д ?\е ?\ж ?\з ?\и ?\й ?\к ?\л ?\м ?\н ?\о ?\п
	  ?\р ?\с ?\т ?\у ?\ф ?\х ?\ц ?\ч ?\ш ?\щ ?\ъ ?\ы ?\ь ?\э ?\ю ?\€])
	(map (make-vector 512 nil)))
    (or (= (length tbl) 128)
	(error "Invalid vector length: %d" (length tbl)))
    (dotimes (i 128)
      (aset map (* i 2) i)
      (aset map (1+ (* i 2)) i))
    (dotimes (i 128)
      (aset map (+ 256 (* i 2)) (+ 128 i))
      (aset map (+ 256 (1+ (* i 2))) (aref tbl i)))
    map))

(define-coding-system 'mac-cyrillic
  "Mac Cyrillic Encoding (MIME:x-mac-cyrillic)."
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(mac-cyrillic)
  :mime-charset 'x-mac-cyrillic)

(define-charset 'mac-symbol
  "Mac Symbol"
  :short-name "Mac SYMBOL"
  :code-space [32 254]
  :map
  (let ((tbl-32-126
	 [?\  ?\! ?\∀ ?\# ?\∃ ?\% ?\& ?\∍ ?\( ?\) ?\∗ ?\+ ?\, ?\− ?\. ?\/
	  ?\0 ?\1 ?\2 ?\3 ?\4 ?\5 ?\6 ?\7 ?\8 ?\9 ?\: ?\; ?\< ?\= ?\> ?\?
	  ?\≅ ?\Α ?\Β ?\Χ ?\Δ ?\Ε ?\Φ ?\Γ ?\Η ?\Ι ?\ϑ ?\Κ ?\Λ ?\Μ ?\Ν ?\Ο
	  ?\Π ?\Θ ?\Ρ ?\Σ ?\Τ ?\Υ ?\ς ?\Ω ?\Ξ ?\Ψ ?\Ζ ?\[ ?\∴ ?\] ?\⊥ ?\_
	  ?\ ?\α ?\β ?\χ ?\δ ?\ε ?\φ ?\γ ?\η ?\ι ?\ϕ ?\κ ?\λ ?\μ ?\ν ?\ο
	  ?\π ?\θ ?\ρ ?\σ ?\τ ?\υ ?\ϖ ?\ω ?\ξ ?\ψ ?\ζ ?\{ ?\| ?\} ?\∼])
	(map-32-126 (make-vector (* (1+ (- 126 32)) 2) nil))
	(tbl-160-254
	 ;; Mapping of the following characters are changed from the
	 ;; original one:
	 ;; 0xE2 0x00AE+0xF87F->0x00AE # REGISTERED SIGN, alternate: sans serif
	 ;; 0xE3 0x00A9+0xF87F->0x00A9 # COPYRIGHT SIGN, alternate: sans serif
	 ;; 0xE4 0x2122+0xF87F->0x2122 # TRADE MARK SIGN, alternate: sans serif
	 [?\€ ?\ϒ ?\′ ?\≤ ?\⁄ ?\∞ ?\ƒ ?\♣ ?\♦ ?\♥ ?\♠ ?\↔ ?\← ?\↑ ?\→ ?\↓
	  ?\° ?\± ?\″ ?\≥ ?\× ?\∝ ?\∂ ?\• ?\÷ ?\≠ ?\≡ ?\≈ ?\… ?\⏐ ?\⎯ ?\↵
	  ?\ℵ ?\ℑ ?\ℜ ?\℘ ?\⊗ ?\⊕ ?\∅ ?\∩ ?\∪ ?\⊃ ?\⊇ ?\⊄ ?\⊂ ?\⊆ ?\∈ ?\∉
	  ?\∠ ?\∇ ?\® ?\© ?\™ ?\∏ ?\√ ?\⋅ ?\¬ ?\∧ ?\∨ ?\⇔ ?\⇐ ?\⇑ ?\⇒ ?\⇓
	  ?\◊ ?\〈 ?\® ?\© ?\™ ?\∑ ?\⎛ ?\⎜ ?\⎝ ?\⎡ ?\⎢ ?\⎣ ?\⎧ ?\⎨ ?\⎩ ?\⎪
	  ?\ ?\〉 ?\∫ ?\⌠ ?\⎮ ?\⌡ ?\⎞ ?\⎟ ?\⎠ ?\⎤ ?\⎥ ?\⎦ ?\⎫ ?\⎬ ?\⎭])
	(map-160-254 (make-vector (* (1+ (- 254 160)) 2) nil)))
    (dotimes (i (1+ (- 126 32)))
      (aset map-32-126 (* i 2) (+ 32 i))
      (aset map-32-126 (1+ (* i 2)) (aref tbl-32-126 i)))
    (dotimes (i (1+ (- 254 160)))
      (aset map-160-254 (* i 2) (+ 160 i))
      (aset map-160-254 (1+ (* i 2)) (aref tbl-160-254 i)))
    (vconcat map-32-126 map-160-254)))

(define-charset 'mac-dingbats
  "Mac Dingbats"
  :short-name "Mac Dingbats"
  :code-space [32 254]
  :map
  (let ((tbl-32-126
	 [?\  ?\✁ ?\✂ ?\✃ ?\✄ ?\☎ ?\✆ ?\✇ ?\✈ ?\✉ ?\☛ ?\☞ ?\✌ ?\✍ ?\✎ ?\✏
	  ?\✐ ?\✑ ?\✒ ?\✓ ?\✔ ?\✕ ?\✖ ?\✗ ?\✘ ?\✙ ?\✚ ?\✛ ?\✜ ?\✝ ?\✞ ?\✟
	  ?\✠ ?\✡ ?\✢ ?\✣ ?\✤ ?\✥ ?\✦ ?\✧ ?\★ ?\✩ ?\✪ ?\✫ ?\✬ ?\✭ ?\✮ ?\✯
	  ?\✰ ?\✱ ?\✲ ?\✳ ?\✴ ?\✵ ?\✶ ?\✷ ?\✸ ?\✹ ?\✺ ?\✻ ?\✼ ?\✽ ?\✾ ?\✿
	  ?\❀ ?\❁ ?\❂ ?\❃ ?\❄ ?\❅ ?\❆ ?\❇ ?\❈ ?\❉ ?\❊ ?\❋ ?\● ?\❍ ?\■ ?\❏
	  ?\❐ ?\❑ ?\❒ ?\▲ ?\▼ ?\◆ ?\❖ ?\◗ ?\❘ ?\❙ ?\❚ ?\❛ ?\❜ ?\❝ ?\❞])
	(map-32-126 (make-vector (* (1+ (- 126 32)) 2) nil))
	(tbl-128-141
	 [?\❨ ?\❩ ?\❪ ?\❫ ?\❬ ?\❭ ?\❮ ?\❯ ?\❰ ?\❱ ?\❲ ?\❳ ?\❴ ?\❵])
	(map-128-141 (make-vector (* (1+ (- 141 128)) 2) nil))
	(tbl-161-239
	 [?\❡ ?\❢ ?\❣ ?\❤ ?\❥ ?\❦ ?\❧ ?\♣ ?\♦ ?\♥ ?\♠ ?\① ?\② ?\③ ?\④
	  ?\⑤ ?\⑥ ?\⑦ ?\⑧ ?\⑨ ?\⑩ ?\❶ ?\❷ ?\❸ ?\❹ ?\❺ ?\❻ ?\❼ ?\❽ ?\❾ ?\❿
	  ?\➀ ?\➁ ?\➂ ?\➃ ?\➄ ?\➅ ?\➆ ?\➇ ?\➈ ?\➉ ?\➊ ?\➋ ?\➌ ?\➍ ?\➎ ?\➏
	  ?\➐ ?\➑ ?\➒ ?\➓ ?\➔ ?\→ ?\↔ ?\↕ ?\➘ ?\➙ ?\➚ ?\➛ ?\➜ ?\➝ ?\➞ ?\➟
	  ?\➠ ?\➡ ?\➢ ?\➣ ?\➤ ?\➥ ?\➦ ?\➧ ?\➨ ?\➩ ?\➪ ?\➫ ?\➬ ?\➭ ?\➮ ?\➯])
	(map-161-239 (make-vector (* (1+ (- 239 161)) 2) nil))
	(tbl-241-254
	 [?\➱ ?\➲ ?\➳ ?\➴ ?\➵ ?\➶ ?\➷ ?\➸ ?\➹ ?\➺ ?\➻ ?\➼ ?\➽ ?\➾])
	(map-241-254 (make-vector (* (1+ (- 254 241)) 2) nil)))
    (dotimes (i (1+ (- 126 32)))
      (aset map-32-126 (* i 2) (+ 32 i))
      (aset map-32-126 (1+ (* i 2)) (aref tbl-32-126 i)))
    (dotimes (i (1+ (- 141 128)))
      (aset map-128-141 (* i 2) (+ 128 i))
      (aset map-128-141 (1+ (* i 2)) (aref tbl-128-141 i)))
    (dotimes (i (1+ (- 239 161)))
      (aset map-161-239 (* i 2) (+ 161 i))
      (aset map-161-239 (1+ (* i 2)) (aref tbl-161-239 i)))
    (dotimes (i (1+ (- 254 241)))
      (aset map-241-254 (* i 2) (+ 241 i))
      (aset map-241-254 (1+ (* i 2)) (aref tbl-241-254 i)))
    (vconcat map-32-126 map-128-141 map-161-239 map-241-254)))

(defconst mac-system-coding-system
  (let ((base (or (cdr (assq mac-system-script-code
			     mac-script-code-coding-systems))
		  'mac-roman)))
    (if (eq system-type 'darwin)
	base
      (coding-system-change-eol-conversion base 'mac)))
  "Coding system derived from the system script code.")

(set-selection-coding-system mac-system-coding-system)


;;;; Keyboard layout/language change events
(defun mac-handle-language-change (event)
  "Set keyboard coding system to what is specified in EVENT."
  (interactive "e")
  (let ((coding-system
	 (cdr (assq (car (cadr event)) mac-script-code-coding-systems))))
    (set-keyboard-coding-system (or coding-system 'mac-roman))
    ;; MacJapanese maps reverse solidus to ?\x80.
    (if (eq coding-system 'japanese-shift-jis)
	(define-key key-translation-map [?\x80] "\\"))))

(define-key special-event-map [language-change] 'mac-handle-language-change)


;;;; Conversion between common flavors and Lisp string.

(defconst mac-text-encoding-ascii #x600
  "ASCII text encoding.")

(defconst mac-text-encoding-mac-japanese-basic-variant #x20001
  "MacJapanese text encoding without Apple double-byte extensions.")

(defun mac-utxt-to-string (data &optional coding-system)
  (or coding-system (setq coding-system mac-system-coding-system))
  (let* ((encoding
	  (and (eq system-type 'darwin)
	       (eq (coding-system-base coding-system) 'japanese-shift-jis)
	       mac-text-encoding-mac-japanese-basic-variant))
	 (str (and (fboundp 'mac-code-convert-string)
		   (mac-code-convert-string data nil
					    (or encoding coding-system)))))
    (when str
      (setq str (decode-coding-string str coding-system))
      (if (eq encoding mac-text-encoding-mac-japanese-basic-variant)
	  ;; Does it contain Apple one-byte extensions other than
	  ;; reverse solidus?
	  (if (string-match "[\xa0\xfd-\xff]" str)
	      (setq str nil)
	    ;; ASCII-only?
	    (unless (mac-code-convert-string data nil mac-text-encoding-ascii)
	      (subst-char-in-string ?\x5c ?\¥ str t)
	      (subst-char-in-string ?\x80 ?\\ str t)))))
    (or str
	(decode-coding-string data
			      (if (eq (byteorder) ?B) 'utf-16be 'utf-16le)))))

(defun mac-string-to-utxt (string &optional coding-system)
  (or coding-system (setq coding-system mac-system-coding-system))
  (let (data encoding)
    (when (and (fboundp 'mac-code-convert-string)
	       (memq (coding-system-base coding-system)
		     (find-coding-systems-string string)))
      (setq coding-system
	    (coding-system-change-eol-conversion coding-system 'mac))
      (let ((str string))
	(when (and (eq system-type 'darwin)
		   (eq coding-system 'japanese-shift-jis-mac))
	  (setq encoding mac-text-encoding-mac-japanese-basic-variant)
	  (setq str (subst-char-in-string ?\\ ?\x80 str))
	  (subst-char-in-string ?\¥ ?\x5c str t)
	  ;; ASCII-only?
	  (if (string-match "\\`[\x00-\x7f]*\\'" str)
	      (setq str nil)))
	(and str
	     (setq data (mac-code-convert-string
			 (encode-coding-string str coding-system)
			 (or encoding coding-system) nil)))))
    (or data (encode-coding-string string (if (eq (byteorder) ?B)
					      'utf-16be-mac
					    'utf-16le-mac)))))

(defun mac-TEXT-to-string (data &optional coding-system)
  (or coding-system (setq coding-system mac-system-coding-system))
  (prog1 (setq data (decode-coding-string data coding-system))
    (when (eq (coding-system-base coding-system) 'japanese-shift-jis)
      ;; (subst-char-in-string ?\x5c ?\¥ data t)
      (subst-char-in-string ?\x80 ?\\ data t))))

(defun mac-string-to-TEXT (string &optional coding-system)
  (or coding-system (setq coding-system mac-system-coding-system))
  (let ((encodables (find-coding-systems-string string))
	(rest mac-script-code-coding-systems))
    (unless (memq (coding-system-base coding-system) encodables)
      (while (and rest (not (memq (cdar rest) encodables)))
	(setq rest (cdr rest)))
      (if rest
	  (setq coding-system (cdar rest)))))
  (setq coding-system
	(coding-system-change-eol-conversion coding-system 'mac))
  (when (eq coding-system 'japanese-shift-jis-mac)
    ;; (setq string (subst-char-in-string ?\\ ?\x80 string))
    (setq string (subst-char-in-string ?\¥ ?\x5c string)))
  (encode-coding-string string coding-system))

(defun mac-furl-to-string (data)
  ;; Remove a trailing nul character.
  (let ((len (length data)))
    (if (and (> len 0) (= (aref data (1- len)) ?\0))
	(substring data 0 (1- len))
      data)))

(defun mac-TIFF-to-string (data &optional text)
  (prog1 (or text (setq text (copy-sequence " ")))
    (put-text-property 0 (length text) 'display (create-image data 'tiff t)
		       text)))

;;;; Selections

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
	     (setq data (mac-utxt-to-string data coding)))
	    ((eq data-type 'com.apple.traditional-mac-plain-text)
	     (setq data (mac-TEXT-to-string data coding)))
	    ((eq data-type 'public.file-url)
	     (setq data (mac-furl-to-string data))))
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
      (setq text (mac-TIFF-to-string tiff-image text)))
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
	(coding (or next-selection-coding-system selection-coding-system)))
    (when str
      ;; If TYPE is nil, this is a local request, thus return STR as
      ;; is.  Otherwise, encode STR.
      (if (not type)
	  str
	(let ((inhibit-read-only t))
	  (remove-text-properties 0 (length str) '(composition nil) str)
	  (cond
	   ((eq type 'public.utf16-plain-text)
	    (setq str (mac-string-to-utxt str coding)))
	   ((eq type 'com.apple.traditional-mac-plain-text)
	    (setq str (mac-string-to-TEXT str coding)))
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

;;;; Apple events, HICommand events, and Services menu

;;; Event classes
(put 'core-event     'mac-apple-event-class "aevt") ; kCoreEventClass
(put 'internet-event 'mac-apple-event-class "GURL") ; kAEInternetEventClass

;;; Event IDs
;; kCoreEventClass
(put 'open-application     'mac-apple-event-id "oapp") ; kAEOpenApplication
(put 'reopen-application   'mac-apple-event-id "rapp") ; kAEReopenApplication
(put 'open-documents       'mac-apple-event-id "odoc") ; kAEOpenDocuments
(put 'print-documents      'mac-apple-event-id "pdoc") ; kAEPrintDocuments
(put 'open-contents        'mac-apple-event-id "ocon") ; kAEOpenContents
(put 'quit-application     'mac-apple-event-id "quit") ; kAEQuitApplication
(put 'application-died     'mac-apple-event-id "obit") ; kAEApplicationDied
(put 'show-preferences     'mac-apple-event-id "pref") ; kAEShowPreferences
(put 'autosave-now         'mac-apple-event-id "asav") ; kAEAutosaveNow
;; kAEInternetEventClass
(put 'get-url              'mac-apple-event-id "GURL") ; kAEGetURL
;; Converted HI command events
(put 'about                'mac-apple-event-id "abou") ; kHICommandAbout
(put 'show-hide-font-panel 'mac-apple-event-id "shfp") ; kHICommandShowHideFontPanel

(defmacro mac-event-spec (event)
  `(nth 1 ,event))

(defmacro mac-event-ae (event)
  `(nth 2 ,event))

(defun mac-ae-parameter (ae &optional keyword type)
  (or keyword (setq keyword "----")) ;; Direct object.
  (if (not (and (consp ae) (equal (car ae) "aevt")))
      (error "Not an Apple event: %S" ae)
    (let ((type-data (cdr (assoc keyword (cdr ae))))
	  data)
      (when (and type type-data (not (equal type (car type-data))))
	(setq data (mac-coerce-ae-data (car type-data) (cdr type-data) type))
	(setq type-data (if data (cons type data) nil)))
      type-data)))

(defun mac-ae-list (ae &optional keyword type)
  (or keyword (setq keyword "----")) ;; Direct object.
  (let ((desc (mac-ae-parameter ae keyword "list")))
    (cond ((null desc)
	   nil)
	  ((not (equal (car desc) "list"))
	   (error "Parameter for \"%s\" is not a list" keyword))
	  (t
	   (if (null type)
	       (cdr desc)
	     (mapcar
	      (lambda (type-data)
		(mac-coerce-ae-data (car type-data) (cdr type-data) type))
	      (cdr desc)))))))

(defun mac-ae-number (ae keyword)
  (let ((type-data (mac-ae-parameter ae keyword))
	str)
    (if (and type-data
	     (setq str (mac-coerce-ae-data (car type-data)
					   (cdr type-data) "TEXT")))
	(let ((num (string-to-number str)))
	  ;; Mac OS Classic may return "0e+0" as the coerced value for
	  ;; the type "magn" and the data "\000\000\000\000".
	  (if (= num 0.0) 0 num))
      nil)))

(defun mac-bytes-to-integer (bytes &optional from to)
  (or from (setq from 0))
  (or to (setq to (length bytes)))
  (let* ((len (- to from))
	 (extended-sign-len (- (1+ (ceiling (log most-positive-fixnum 2)))
			       (* 8 len)))
	 (result 0))
    (dotimes (i len)
      (setq result (logior (lsh result 8)
			   (aref bytes (+ from (if (eq (byteorder) ?B) i
						 (- len i 1)))))))
    (if (> extended-sign-len 0)
	(ash (lsh result extended-sign-len) (- extended-sign-len))
      result)))

(defun mac-ae-selection-range (ae)
;; #pragma options align=mac68k
;; typedef struct SelectionRange {
;;   short unused1; // 0 (not used)
;;   short lineNum; // line to select (<0 to specify range)
;;   long startRange; // start of selection range (if line < 0)
;;   long endRange; // end of selection range (if line < 0)
;;   long unused2; // 0 (not used)
;;   long theDate; // modification date/time
;; } SelectionRange;
;; #pragma options align=reset
  (let ((range-bytes (cdr (mac-ae-parameter ae "kpos" "TEXT"))))
    (and range-bytes
	 (list (mac-bytes-to-integer range-bytes 2 4)
	       (mac-bytes-to-integer range-bytes 4 8)
	       (mac-bytes-to-integer range-bytes 8 12)
	       (mac-bytes-to-integer range-bytes 16 20)))))

;; On Mac OS X 10.4 and later, the `open-document' event contains an
;; optional parameter keyAESearchText from the Spotlight search.
(defun mac-ae-text-for-search (ae)
  (let ((utf8-text (cdr (mac-ae-parameter ae "stxt" "utf8"))))
    (and utf8-text
	 (decode-coding-string utf8-text 'utf-8))))

(defun mac-ae-text (ae)
  (or (cdr (mac-ae-parameter ae nil "TEXT"))
      (error "No text in Apple event.")))

(defun mac-ae-frame (ae &optional keyword type)
  (let ((bytes (cdr (mac-ae-parameter ae keyword type))))
    (if (or (null bytes) (/= (length bytes) 4))
	(error "No window reference in Apple event.")
      (let ((window-id (mac-coerce-ae-data "long" bytes "TEXT"))
	    (rest (frame-list))
	    frame)
	(while (and (null frame) rest)
	  (if (string= (frame-parameter (car rest) 'window-id) window-id)
	      (setq frame (car rest)))
	  (setq rest (cdr rest)))
	frame))))

(defun mac-ae-script-language (ae keyword)
;; struct WritingCode {
;;   ScriptCode          theScriptCode;
;;   LangCode            theLangCode;
;; };
  (let ((bytes (cdr (mac-ae-parameter ae keyword "intl"))))
    (and bytes
	 (cons (mac-bytes-to-integer bytes 0 2)
	       (mac-bytes-to-integer bytes 2 4)))))

(defun mac-bytes-to-text-range (bytes &optional from to)
;; struct TextRange {
;;   long                fStart;
;;   long                fEnd;
;;   short               fHiliteStyle;
;; };
  (or from (setq from 0))
  (or to (setq to (length bytes)))
  (and (= (- to from) (+ 4 4 2))
       (list (mac-bytes-to-integer bytes from (+ from 4))
	     (mac-bytes-to-integer bytes (+ from 4) (+ from 8))
	     (mac-bytes-to-integer bytes (+ from 8) to))))

(defun mac-ae-text-range-array (ae keyword)
;; struct TextRangeArray {
;;   short               fNumOfRanges;
;;   TextRange           fRange[1];
;; };
  (let* ((bytes (cdr (mac-ae-parameter ae keyword "tray")))
	 (len (length bytes))
	 nranges result)
    (when (and bytes (>= len 2)
	       (progn
		 (setq nranges (mac-bytes-to-integer bytes 0 2))
		 (= len (+ 2 (* nranges 10)))))
      (setq result (make-vector nranges nil))
      (dotimes (i nranges)
	(aset result i
	      (mac-bytes-to-text-range bytes (+ (* i 10) 2)
				       (+ (* i 10) 12)))))
    result))

(defconst mac-keyboard-modifier-mask-alist
  (mapcar
   (lambda (modifier-bit)
     (cons (car modifier-bit) (lsh 1 (cdr modifier-bit))))
   '((command  . 8)			; cmdKeyBit
     (shift    . 9)			; shiftKeyBit
     (option   . 11)			; optionKeyBit
     (control  . 12)			; controlKeyBit
     (function . 17)))			; kEventKeyModifierFnBit
  "Alist of Mac keyboard modifier symbols vs masks.")

(defun mac-ae-keyboard-modifiers (ae)
  (let ((modifiers-value (mac-ae-number ae "kmod"))
	modifiers)
    (if modifiers-value
	(dolist (modifier-mask mac-keyboard-modifier-mask-alist)
	  (if (/= (logand modifiers-value (cdr modifier-mask)) 0)
	      (setq modifiers (cons (car modifier-mask) modifiers)))))
    modifiers))

(defun mac-ae-reopen-application (event)
  "Show some frame in response to the Apple event EVENT.
The frame to be shown is chosen from visible or iconified frames
if possible.  If there's no such frame, a new frame is created."
  (interactive "e")
  (unless (frame-visible-p (selected-frame))
    (let ((frame (or (car (visible-frame-list))
		     (car (filtered-frame-list 'frame-visible-p)))))
      (if frame
	  (select-frame frame)
	(switch-to-buffer-other-frame "*scratch*"))))
  (select-frame-set-input-focus (selected-frame)))

(defun mac-ae-open-documents (event)
  "Open the documents specified by the Apple event EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (dolist (file-name (mac-ae-list ae nil 'undecoded-file-name))
      (if file-name
	  (dnd-open-local-file
	   (concat "file://"
		   (mapconcat 'url-hexify-string
			      (split-string file-name "/") "/")) nil)))
    (let ((selection-range (mac-ae-selection-range ae))
	  (search-text (mac-ae-text-for-search ae)))
      (cond (selection-range
	     (let ((line (car selection-range))
		   (start (cadr selection-range))
		   (end (nth 2 selection-range)))
	       (if (>= line 0)
		   (goto-line (1+ line))
		 (if (and (>= start 0) (>= end 0))
		     (progn (set-mark (1+ start))
			    (goto-char (1+ end)))))))
	    ((stringp search-text)
	     (re-search-forward
	      (mapconcat 'regexp-quote (split-string search-text) "\\|")
	      nil t)))))
  (select-frame-set-input-focus (selected-frame)))

(defun mac-ae-quit-application (event)
  "Quit the application Emacs with the Apple event EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (unwind-protect
	(save-buffers-kill-emacs)
      ;; Reaches here if the user has canceled the quit.
      (mac-resume-apple-event ae -128)))) ; userCanceledErr

;; url-generic-parse-url is autoloaded from url-parse.
(declare-function url-type "url-parse" t t) ; defstruct

(defun mac-ae-get-url (event)
  "Open the URL specified by the Apple event EVENT.
Currently the `mailto' scheme is supported."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (parsed-url (url-generic-parse-url (mac-ae-text ae))))
    (if (string= (url-type parsed-url) "mailto")
	(progn
	  (url-mailto parsed-url)
	  (select-frame-set-input-focus (selected-frame)))
      (mac-resume-apple-event ae t))))

(setq mac-apple-event-map (make-sparse-keymap))

;; Received when Emacs is launched without associated documents.
;; Accept it as an Apple event, but no Emacs event is generated so as
;; not to erase the splash screen.
(define-key mac-apple-event-map [core-event open-application] 0)

;; Received when a dock or application icon is clicked and Emacs is
;; already running.
(define-key mac-apple-event-map [core-event reopen-application]
  'mac-ae-reopen-application)

(define-key mac-apple-event-map [core-event open-documents]
  'mac-ae-open-documents)
(define-key mac-apple-event-map [core-event show-preferences] 'customize)
(define-key mac-apple-event-map [core-event quit-application]
  'mac-ae-quit-application)

(define-key mac-apple-event-map [internet-event get-url] 'mac-ae-get-url)

(define-key mac-apple-event-map [hi-command about] 'about-emacs)

;;; Converted Carbon Events
(defun mac-handle-toolbar-switch-mode (event)
  "Toggle visibility of tool-bars in response to EVENT.
With no keyboard modifiers, it toggles the visibility of the
frame where the tool-bar toggle button was pressed.  With some
modifiers, it changes the global tool-bar visibility setting."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (if (mac-ae-keyboard-modifiers ae)
	;; Globally toggle tool-bar-mode if some modifier key is pressed.
	(tool-bar-mode 'toggle)
      (let ((frame (mac-ae-frame ae)))
	(set-frame-parameter frame 'tool-bar-lines
			     (if (= (frame-parameter frame 'tool-bar-lines) 0)
				 1 0))))))

;; kEventClassWindow/kEventWindowToolbarSwitchMode
(define-key mac-apple-event-map [window toolbar-switch-mode]
  'mac-handle-toolbar-switch-mode)

;;; Font panel
(when (fboundp 'mac-set-font-panel-visible-p)

(define-minor-mode mac-font-panel-mode
  "Toggle use of the font panel.
With numeric ARG, display the font panel if and only if ARG is positive."
  :init-value nil
  :global t
  :group 'mac
  (mac-set-font-panel-visible-p mac-font-panel-mode))

(defun mac-handle-font-panel-closed (event)
  "Update internal status in response to font panel closed EVENT."
  (interactive "e")
  ;; Synchronize with the minor mode variable.
  (mac-font-panel-mode 0))

(defun mac-handle-font-selection (event)
  "Change default face attributes according to font selection EVENT."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (fm-font-size (mac-ae-number ae "fmsz"))
	 (atsu-font-id (mac-ae-number ae "auid"))
	 (attribute-values (and atsu-font-id
				(mac-atsu-font-face-attributes atsu-font-id))))
    (if fm-font-size
	(setq attribute-values
	      `(:height ,(* 10 fm-font-size) ,@attribute-values)))
    (apply 'set-face-attribute 'default (selected-frame) attribute-values)))

;; kEventClassFont/kEventFontPanelClosed
(define-key mac-apple-event-map [font panel-closed]
  'mac-handle-font-panel-closed)
;; kEventClassFont/kEventFontSelection
(define-key mac-apple-event-map [font selection] 'mac-handle-font-selection)
(define-key mac-apple-event-map [hi-command show-hide-font-panel]
  'mac-font-panel-mode)

(define-key-after menu-bar-showhide-menu [mac-font-panel-mode]
  (menu-bar-make-mm-toggle mac-font-panel-mode
			   "Font Panel"
			   "Show the font panel as a floating dialog")
  'showhide-speedbar)

) ;; (fboundp 'mac-set-font-panel-visible-p)

;;; Text Services
(defvar mac-ts-update-active-input-area-seqno 0
  "Number of processed update-active-input-area events.")
(setq mac-ts-active-input-overlay (make-overlay 0 0))

(defface mac-ts-caret-position
  '((t :inverse-video t))
  "Face for caret position in Mac TSM active input area.
This is used when the active input area is displayed either in
the echo area or in a buffer where the cursor is not displayed."
  :group 'mac)

(defface mac-ts-raw-text
  '((t :underline t))
  "Face for raw text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-selected-raw-text
  '((t :underline t))
  "Face for selected raw text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-converted-text
  '((((background dark)) :underline "gray20")
    (t :underline "gray80"))
  "Face for converted text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-selected-converted-text
  '((t :underline t))
  "Face for selected converted text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-block-fill-text
  '((t :underline t))
  "Face for block fill text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-outline-text
  '((t :underline t))
  "Face for outline text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-selected-text
  '((t :underline t))
  "Face for selected text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-no-hilite
  '((t :inherit default))
  "Face for no hilite in Mac TSM active input area."
  :group 'mac)

(defconst mac-ts-hilite-style-faces
  '((2 . mac-ts-raw-text)		 ; kTSMHiliteRawText
    (3 . mac-ts-selected-raw-text)	 ; kTSMHiliteSelectedRawText
    (4 . mac-ts-converted-text)		 ; kTSMHiliteConvertedText
    (5 . mac-ts-selected-converted-text) ; kTSMHiliteSelectedConvertedText
    (6 . mac-ts-block-fill-text)	 ; kTSMHiliteBlockFillText
    (7 . mac-ts-outline-text)		 ; kTSMHiliteOutlineText
    (8 . mac-ts-selected-text)		 ; kTSMHiliteSelectedText
    (9 . mac-ts-no-hilite))		 ; kTSMHiliteNoHilite
  "Alist of Mac TSM hilite style vs Emacs face.")

(defun mac-ts-update-active-input-buf (text fix-len hilite-rng update-rng)
  (let ((buf-len (length mac-ts-active-input-buf))
	confirmed)
    (if (or (null update-rng)
	    (/= (% (length update-rng) 2) 0))
	;; The parameter is missing (or in a bad format).  The
	;; existing inline input session is completely replaced with
	;; the new text.
	(setq mac-ts-active-input-buf text)
      ;; Otherwise, the current subtext specified by the (2*j)-th
      ;; range is replaced with the new subtext specified by the
      ;; (2*j+1)-th range.
      (let ((tail buf-len)
	    (i (length update-rng))
	    segments rng)
	(while (> i 0)
	  (setq i (- i 2))
	  (setq rng (aref update-rng i))
	  (if (and (<= 0 (cadr rng)) (< (cadr rng) tail)
		   (<= tail buf-len))
	      (setq segments
		    (cons (substring mac-ts-active-input-buf (cadr rng) tail)
			  segments)))
	  (setq tail (car rng))
	  (setq rng (aref update-rng (1+ i)))
	  (if (and (<= 0 (car rng)) (< (car rng) (cadr rng))
		   (<= (cadr rng) (length text)))
	      (setq segments
		    (cons (substring text (car rng) (cadr rng))
			  segments))))
	(if (and (< 0 tail) (<= tail buf-len))
	    (setq segments
		  (cons (substring mac-ts-active-input-buf 0 tail)
			segments)))
	(setq mac-ts-active-input-buf (apply 'concat segments))))
    (setq buf-len (length mac-ts-active-input-buf))
    ;; Confirm (a part of) inline input session.
    (cond ((< fix-len 0)
	   ;; Entire inline session is being confirmed.
	   (setq confirmed mac-ts-active-input-buf)
	   (setq mac-ts-active-input-buf ""))
	  ((= fix-len 0)
	   ;; None of the text is being confirmed (yet).
	   (setq confirmed ""))
	  (t
	   (if (> fix-len buf-len)
	       (setq fix-len buf-len))
	   (setq confirmed (substring mac-ts-active-input-buf 0 fix-len))
	   (setq mac-ts-active-input-buf
		 (substring mac-ts-active-input-buf fix-len))))
    (setq buf-len (length mac-ts-active-input-buf))
    ;; Update highlighting and the caret position in the new inline
    ;; input session.
    (remove-text-properties 0 buf-len '(cursor nil) mac-ts-active-input-buf)
    (mapc (lambda (rng)
	    (cond ((and (= (nth 2 rng) 1) ; kTSMHiliteCaretPosition
			(<= 0 (car rng)) (< (car rng) buf-len))
		   (put-text-property (car rng) buf-len
				      'cursor t mac-ts-active-input-buf))
		  ((and (<= 0 (car rng)) (< (car rng) (cadr rng))
			(<= (cadr rng) buf-len))
		   (put-text-property (car rng) (cadr rng) 'face
				      (cdr (assq (nth 2 rng)
						 mac-ts-hilite-style-faces))
				      mac-ts-active-input-buf))))
	  hilite-rng)
    confirmed))

(defun mac-split-string-by-property-change (string)
  (let ((tail (length string))
	head result)
    (unless (= tail 0)
      (while (setq head (previous-property-change tail string)
		   result (cons (substring string (or head 0) tail) result)
		   tail head)))
    result))

(defun mac-replace-untranslated-utf-8-chars (string &optional to-string)
  (or to-string (setq to-string "$,3u=(B"))
  (mapconcat
   (lambda (str)
     (if (get-text-property 0 'untranslated-utf-8 str) to-string str))
   (mac-split-string-by-property-change string)
   ""))

(defun mac-keyboard-translate-char (ch)
  (if (and (characterp ch)
	   (or (char-table-p keyboard-translate-table)
	       (and (or (stringp keyboard-translate-table)
			(vectorp keyboard-translate-table))
		    (> (length keyboard-translate-table) ch))))
      (or (aref keyboard-translate-table ch) ch)
    ch))

(defun mac-unread-string (string)
  ;; Unread characters and insert them in a keyboard macro being
  ;; defined.
  (apply 'isearch-unread
	 (mapcar 'mac-keyboard-translate-char
		 (mac-replace-untranslated-utf-8-chars string))))

(defun mac-ts-update-active-input-area (event)
  "Update Mac TSM active input area according to EVENT.
The confirmed text is converted to Emacs input events and pushed
into `unread-command-events'.  The unconfirmed text is displayed
either in the current buffer or in the echo area."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (type-text (mac-ae-parameter ae "tstx"))
	 (text (or (cdr type-text) ""))
	 (decode-fun (if (equal (car type-text) "TEXT")
			 'mac-TEXT-to-string 'mac-utxt-to-string))
	 (script-language (mac-ae-script-language ae "tssl"))
	 (coding (or (cdr (assq (car script-language)
				mac-script-code-coding-systems))
		     'mac-roman))
	 (fix-len (mac-ae-number ae "tsfx"))
	 ;; Optional parameters
	 (hilite-rng (mac-ae-text-range-array ae "tshi"))
	 (update-rng (mac-ae-text-range-array ae "tsup"))
	 ;;(pin-rng (mac-bytes-to-text-range (cdr (mac-ae-parameter ae "tspn" "txrn"))))
	 ;;(clause-offsets (cdr (mac-ae-parameter ae "tscl" "ofay")))
	 (seqno (mac-ae-number ae "tsSn"))
	 confirmed)
    (unless (= seqno mac-ts-update-active-input-area-seqno)
      ;; Reset internal states if sequence number is out of sync.
      (setq mac-ts-active-input-buf ""))
    (setq confirmed
	  (mac-ts-update-active-input-buf text fix-len hilite-rng update-rng))
    (let ((use-echo-area
	   (or isearch-mode
	       (and cursor-in-echo-area (current-message))
	       ;; Overlay strings are not shown in some cases.
	       (get-char-property (point) 'invisible)
	       (and (not (bobp))
		    (or (and (get-char-property (point) 'display)
			     (eq (get-char-property (1- (point)) 'display)
				 (get-char-property (point) 'display)))
			(and (get-char-property (point) 'composition)
			     (eq (get-char-property (1- (point)) 'composition)
				 (get-char-property (point) 'composition)))))))
	  active-input-string caret-seen)
      ;; Decode the active input area text with inheriting faces and
      ;; the caret position.
      (setq active-input-string
	    (mapconcat
	     (lambda (str)
	       (let ((decoded (funcall decode-fun str coding)))
		 (put-text-property 0 (length decoded) 'face
				    (get-text-property 0 'face str) decoded)
		 (when (and (not caret-seen)
			    (get-text-property 0 'cursor str))
		   (setq caret-seen t)
		   (if (or use-echo-area (null cursor-type))
		       (put-text-property 0 1 'face 'mac-ts-caret-position
					  decoded)
		     (put-text-property 0 1 'cursor t decoded)))
		 decoded))
	     (mac-split-string-by-property-change mac-ts-active-input-buf)
	     ""))
      (put-text-property 0 (length active-input-string)
			 'mac-ts-active-input-string t active-input-string)
      (if use-echo-area
	  (let ((msg (current-message))
		message-log-max)
	    (if (and msg
		     ;; Don't get confused by previously displayed
		     ;; `active-input-string'.
		     (null (get-text-property 0 'mac-ts-active-input-string
					      msg)))
		(setq msg (propertize msg 'display
				      (concat msg active-input-string)))
	      (setq msg active-input-string))
	    (message "%s" msg)
	    (overlay-put mac-ts-active-input-overlay 'before-string nil))
	(move-overlay mac-ts-active-input-overlay
		      (point) (point) (current-buffer))
	(overlay-put mac-ts-active-input-overlay 'before-string
		     active-input-string))
      (mac-unread-string (funcall decode-fun confirmed coding)))
    ;; The event is successfully processed.  Sync the sequence number.
    (setq mac-ts-update-active-input-area-seqno (1+ seqno))))

(defun mac-ts-unicode-for-key-event (event)
  "Convert Unicode key EVENT to Emacs key events and unread them."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (text (cdr (mac-ae-parameter ae "tstx" "utxt")))
	 (script-language (mac-ae-script-language ae "tssl"))
	 (coding (or (cdr (assq (car script-language)
				mac-script-code-coding-systems))
		     'mac-roman)))
    (if text
	(mac-unread-string (mac-utxt-to-string text coding)))))

;; kEventClassTextInput/kEventTextInputUpdateActiveInputArea
(define-key mac-apple-event-map [text-input update-active-input-area]
  'mac-ts-update-active-input-area)
;; kEventClassTextInput/kEventTextInputUnicodeForKeyEvent
(define-key mac-apple-event-map [text-input unicode-for-key-event]
  'mac-ts-unicode-for-key-event)

;;; Services
(defun mac-service-open-file ()
  "Open the file specified by the selection value for Services."
  (interactive)
  ;; The selection seems not to contain the file name as
  ;; public.utf16-plain-text data on Mac OS X 10.4.
  (dnd-open-file (x-get-selection mac-service-selection 'public.file-url) nil))

(defun mac-service-open-selection ()
  "Create a new buffer containing the selection value for Services."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*untitled*"))
  (insert (x-selection-value mac-service-selection))
  (sit-for 0)
  (save-buffer) ; It pops up the save dialog.
  )

(defun mac-service-mail-selection ()
  "Prepare a mail buffer containing the selection value for Services."
  (interactive)
  (compose-mail)
  (rfc822-goto-eoh)
  (forward-line 1)
  (insert (x-selection-value mac-service-selection) "\n"))

(defun mac-service-mail-to ()
  "Prepare a mail buffer to be sent to the selection value for Services."
  (interactive)
  (compose-mail (x-selection-value mac-service-selection)))

(defun mac-service-insert-text ()
  "Insert the selection value for Services."
  (interactive)
  (let ((text (x-selection-value mac-service-selection)))
    (if (not buffer-read-only)
	(insert text)
      (kill-new text)
      (message "%s"
       (substitute-command-keys
	"The text from the Services menu can be accessed with \\[yank]")))))

;; kEventClassService/kEventServicePaste
(define-key mac-apple-event-map [service paste] 'mac-service-insert-text)
;; kEventClassService/kEventServicePerform
(define-key mac-apple-event-map [service perform open-file]
  'mac-service-open-file)
(define-key mac-apple-event-map [service perform open-selection]
  'mac-service-open-selection)
(define-key mac-apple-event-map [service perform mail-selection]
  'mac-service-mail-selection)
(define-key mac-apple-event-map [service perform mail-to]
  'mac-service-mail-to)

(defun mac-dispatch-apple-event (event)
  "Dispatch EVENT according to the keymap `mac-apple-event-map'."
  (interactive "e")
  (let* ((binding (lookup-key mac-apple-event-map (mac-event-spec event)))
	 (ae (mac-event-ae event))
	 (service-message (and (keymapp binding)
			       (cdr (mac-ae-parameter ae "svmg")))))
    (when service-message
      (setq service-message
	    (intern (decode-coding-string service-message 'utf-8)))
      (setq binding (lookup-key binding (vector service-message))))
    ;; Replace (cadr event) with a dummy position so that event-start
    ;; returns it.
    (setcar (cdr event) (list (selected-window) (point) '(0 . 0) 0))
    (if (null (mac-ae-parameter ae 'emacs-suspension-id))
	(command-execute binding nil (vector event) t)
      (condition-case err
	  (progn
	    (command-execute binding nil (vector event) t)
	    (mac-resume-apple-event ae))
	(error
	 (mac-ae-set-reply-parameter ae "errs"
				     (cons "TEXT" (error-message-string err)))
	 (mac-resume-apple-event ae -10000)))))) ; errAEEventFailed

(define-key special-event-map [mac-apple-event] 'mac-dispatch-apple-event)

;; Processing of Apple events are deferred at the startup time.  For
;; example, files dropped onto the Emacs application icon can only be
;; processed when the initial frame has been created: this is where
;; the files should be opened.
(add-hook 'after-init-hook 'mac-process-deferred-apple-events)

(run-with-idle-timer 5 t 'mac-cleanup-expired-apple-events)


;;;; Drag and drop

(defcustom mac-dnd-types-alist
  '(("furl" . mac-dnd-handle-furl)
    ("hfs " . mac-dnd-handle-hfs)
    ("utxt" . mac-dnd-insert-utxt)
    ("TEXT" . mac-dnd-insert-TEXT)
    ("TIFF" . mac-dnd-insert-TIFF))
  "Which function to call to handle a drop of that type.
The function takes three arguments, WINDOW, ACTION and DATA.
WINDOW is where the drop occurred, ACTION is always `private' on
Mac.  DATA is the drop data.  Unlike the x-dnd counterpart, the
return value of the function is not significant.

See also `mac-dnd-known-types'."
  :version "22.1"
  :type 'alist
  :group 'mac)

(defun mac-dnd-handle-furl (window action data)
  (dnd-handle-one-url window action (mac-furl-to-string data)))

(defun mac-dnd-handle-hfs (window action data)
;; struct HFSFlavor {
;;   OSType fileType;
;;   OSType fileCreator;
;;   UInt16 fdFlags;
;;   FSSpec fileSpec;
;; };
  (let* ((file-name (mac-coerce-ae-data "fss " (substring data 10)
					'undecoded-file-name))
	 (url (concat "file://"
		      (mapconcat 'url-hexify-string
				 (split-string file-name "/") "/"))))
    (dnd-handle-one-url window action url)))

(defun mac-dnd-insert-utxt (window action data)
  (dnd-insert-text window action (mac-utxt-to-string data)))

(defun mac-dnd-insert-TEXT (window action data)
  (dnd-insert-text window action (mac-TEXT-to-string data)))

(defun mac-dnd-insert-TIFF (window action data)
  (dnd-insert-text window action (mac-TIFF-to-string data)))

(defun mac-dnd-drop-data (event frame window data type &optional action)
  (or action (setq action 'private))
  (let* ((type-info (assoc type mac-dnd-types-alist))
	 (handler (cdr type-info))
	 (w (posn-window (event-start event))))
    (when handler
      (if (and (window-live-p w)
	       (not (window-minibuffer-p w))
	       (not (window-dedicated-p w)))
	  ;; If dropping in an ordinary window which we could use,
	  ;; let dnd-open-file-other-window specify what to do.
	  (progn
	    (when (not mouse-yank-at-point)
	      (goto-char (posn-point (event-start event))))
	    (funcall handler window action data))
	;; If we can't display the file here,
	;; make a new window for it.
	(let ((dnd-open-file-other-window t))
	  (select-frame frame)
	  (funcall handler window action data))))))

(defun mac-dnd-handle-drag-n-drop-event (event)
  "Receive drag and drop events."
  (interactive "e")
  (let ((window (posn-window (event-start event)))
	(ae (mac-event-ae event))
	action)
    (when (windowp window) (select-window window))
    (if (memq 'option (mac-ae-keyboard-modifiers ae))
	(setq action 'copy))
    (dolist (item (mac-ae-list ae))
      (if (not (equal (car item) "null"))
	  (mac-dnd-drop-data event (selected-frame) window
			     (cdr item) (car item) action)))))

(setq font-encoding-alist
      (append
       '(("mac-roman" . mac-roman)
	 ("mac-centraleurroman" . mac-centraleurroman)
	 ("mac-cyrillic" . mac-cyrillic)
	 ("mac-symbol" . mac-symbol)
	 ("mac-dingbats" . mac-dingbats))
       font-encoding-alist))

(defun fontset-add-mac-fonts (fontset &optional base-family)
  (dolist (elt `((latin . (,(or base-family "Monaco") . "mac-roman"))
		 (mac-roman . (,base-family . "mac-roman"))
		 (mac-centraleurroman . (,base-family . "mac-centraleurroman"))
		 (mac-cyrillic . (,base-family . "mac-cyrillic"))
		 (mac-symbol . (,base-family . "mac-symbol"))
		 (mac-dingbats . (,base-family . "mac-dingbats"))))
    (set-fontset-font fontset (car elt) (cdr elt))))

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
  (or resolved-font
      (setq resolved-font (x-resolve-font-name font)))
  (let ((base-family (aref (x-decompose-font-name resolved-font)
			   xlfd-regexp-family-subnum)))
    (if (string= base-family "*")
	(setq base-family nil))
    (new-fontset fontset-name (list (cons 'ascii resolved-font)))
    (fontset-add-mac-fonts fontset-name base-family)))

(defun x-win-suspend-error ()
  (error "Suspending an Emacs running under Mac makes no sense"))

(defalias 'x-cut-buffer-or-selection-value 'x-get-selection-value)

(defvar mac-initialized nil
  "Non-nil if the w32 window system has been initialized.")

(defun mac-initialize-window-system ()
  "Initialize Emacs for Mac GUI frames."

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

(add-hook 'suspend-hook 'x-win-suspend-error)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)




;;; Turn off window-splitting optimization; Mac is usually fast enough
;;; that this is only annoying.
(setq split-window-keep-point t)

;; Don't show the frame name; that's redundant.
(setq-default mode-line-frame-identification "  ")

;; Turn on support for mouse wheels.
(mouse-wheel-mode 1)


;; Enable CLIPBOARD copy/paste through menu bar commands.
(menu-bar-enable-clipboard)


;; Initiate drag and drop

(define-key special-event-map [drag-n-drop] 'mac-dnd-handle-drag-n-drop-event)


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

;; Adjust Courier font specifications in x-fixed-font-alist.
(let ((courier-fonts (assoc "Courier" x-fixed-font-alist)))
  (if courier-fonts
      (dolist (label-fonts (cdr courier-fonts))
	(setcdr label-fonts
		(mapcar
		 (lambda (font)
		   (if (string-match "\\`-adobe-courier-\\([^-]*\\)-\\(.\\)-\\(.*\\)-iso8859-1\\'" font)
		       (replace-match
			(if (string= (match-string 2 font) "o")
			    "-*-courier-\\1-i-\\3-*-*"
			  "-*-courier-\\1-\\2-\\3-*-*")
			t nil font)
		     font))
		 (cdr label-fonts))))))

;; Setup the default fontset.
(setup-default-fontset)

;; Create a fontset that uses mac-roman font.  With this fontset,
;; characters belonging to mac-roman charset (that contains ASCII and
;; more Latin characters) are displayed by a mac-roman font.
(create-fontset-from-mac-roman-font
 "-*-Monaco-*-*-*-*-12-*-*-*-*-*-mac-roman" nil
 "-apple-Monaco-normal-r-*-*-12-*-*-*-*-*-fontset-standard")

;; Create fontset specified in X resources "Fontset-N" (N is 0, 1, ...).
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
	;; The size parms apply to all frames.  Don't set it if there are
	;; sizes there already (from command line).
	(if (and (assq 'height parsed)
		 (not (assq 'height default-frame-alist)))
	    (setq default-frame-alist
		  (cons (cons 'height (cdr (assq 'height parsed)))
			default-frame-alist)))
	(if (and (assq 'width parsed)
		 (not (assq 'width default-frame-alist)))
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

(setq mac-initialized t)))

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

;; Now the default directory is changed to the user's home directory
;; in emacs.c if invoked from the WindowServer (with -psn_* option).
;; (if (string= default-directory "/")
;;     (cd "~"))

;; Darwin 6- pty breakage is now controlled from the C code so that
;; it applies to all builds on darwin.  See s/darwin.h PTY_ITERATION.
;; (setq process-connection-type t)

;; Assume that fonts are always scalable on the Mac.  This sometimes
;; results in characters with jagged edges.  However, without it,
;; fonts with both truetype and bitmap representations but no italic
;; or bold bitmap versions will not display these variants correctly.
(setq scalable-fonts-allowed t)

(add-to-list 'handle-args-function-alist '(mac . x-handle-args))
(add-to-list 'frame-creation-function-alist '(mac . x-create-frame-with-faces))
(add-to-list 'window-system-initialization-alist '(mac . mac-initialize-window-system))

(provide 'mac-win)

;; arch-tag: 71dfcd14-cde8-4d66-b05c-85ec94fb23a6
;;; mac-win.el ends here
