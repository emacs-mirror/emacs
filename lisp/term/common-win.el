;;; common-win.el --- common part of handling window systems

;; Copyright (C) 1993, 1994, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

;; Maintainer: FSF
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

;;; Code:


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

(declare-function x-parse-geometry "frame.c" (string))

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

(defvar x-resource-name)

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
  "The name of the window display on which Emacs was started.
On X, the display name of individual X frames is recorded in the
`display' frame parameter.")

(defun x-handle-display (switch)
  "Handle -display DISPLAY option."
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
  (purecopy 
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
    "light sky blue" "sky blue" "light blue" "powder blue" "pale turquoise"
    "turquoise" "medium turquoise" "dark turquoise"  "dark cyan" "aquamarine"
    "medium aquamarine" "light sea green"
    "medium sea green" "sea green" "dark sea green" "pale green" "lime green"
    "dark green" "forest green" "light green" "green yellow" "yellow green" "olive drab"
    "dark olive green" "lemon chiffon" "khaki" "dark khaki" "cornsilk"
    "pale goldenrod" "light goldenrod" "goldenrod" "dark goldenrod" "wheat"
    "navajo white" "tan" "burlywood" "sandy brown" "peru" "chocolate" "saddle brown"
    "sienna" "rosy brown" "dark salmon" "coral" "tomato" "light salmon" "salmon"
    "light coral" "indian red" "firebrick" "brown" "dark red" "magenta"
    "dark magenta" "dark violet" "medium blue" "blue" "deep sky blue"
    "cyan" "medium spring green" "spring green" "green" "lawn green" "chartreuse"
    "yellow" "gold" "orange" "dark orange" "orange red" "red" "white" "white smoke"
    "gainsboro" "light gray" "gray" "dark gray" "dim gray" "black" ))
  "List of basic colors available on color displays.
For X, the list comes from the `rgb.txt' file,v 10.41 94/02/20.
For Nextstep, this is a list of non-PANTONE colors returned by
the operating system.")

;; arch-tag: 2a128601-99cc-401e-9dff-0ee6a36102ef
;;; common-win.el ends here
