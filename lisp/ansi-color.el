;;; ansi-color.el -- translate ANSI into text-properties

;; Copyright (C) 1999  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.ch>
;; Maintainer: Alex Schroeder <alex@gnu.ch>
;; Version: 1.2.0
;; Keywords: comm processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; You can get the latest version of this file from my homepage
;; <URL:http://www.geocities.com/TimesSquare/6120/emacs.html>.
;;
;; This file provides a function that takes a string containing ANSI
;; control sequences and tries to replace these with text-properties.
;;
;; I was unable to extract this functionality from term.el for another
;; program I wanted to extend (the MUSH client TinyTalk.el), so I had to
;; rewrite this.

;; In order to install this with TinyMush.el, add the following to your
;; .emacs file:
;;
;; (setq tinymud-filter-line-hook 'my-tinymud-add-ansi-text-properties)
;; (autoload 'ansi-color-to-text-properties "ansi-color" 
;;   "Translates ANSI color control sequences into text-properties." t)
;; (defun my-tinymud-add-ansi-text-properties (conn line)
;;   "Call `ansi-color-to-text-properties' for LINE.
;; Ignores CONN and returns nil, so that `tinymud-filter-line' continues to
;; process triggers and everything else."
;;   (ansi-color-to-text-properties line)
;;   nil)

;; If the ANSI sequences assume that you have a black background, you'll
;; have to display the stuff in a frame with a black background.  You
;; can create such a frame like this (it still looks ugly!):
;;
;; (defun my-black-frame ()
;;   "Create a frame with black background."
;;   (interactive)
;;   (make-frame '((foreground-color . "white")
;; 		(background-color . "black"))))

;;; Testing:

;; If you want to test the setup, evaluate the following fragment in a
;; buffer without font-lock-mode.  This doesn't work in buffers that
;; have font-lock-mode!
;;
;; (progn
;;   (setq line "[1mbold[0m and [34mblue[0m, [1m[34mbold and blue[0m!!")
;;   (ansi-color-to-text-properties line)
;;   (insert line))
;;
;; Other test strings: (m-eating-bug) "[1mmold[0m should be mold"

;;; Bugs:

;; 1. Only supports the ANSI sequences that the MUSH I'm on uses (the
;;    MUSH is Elendor, see http://www.elendor.net).  To see the list of
;;    codes supported I did a `help ansi()'.  Based on this information,
;;    I used TinyTalk.el (without ANSI color support), gave myself the
;;    ANSI color flags using `@set me=ANSI' and `@set me=COLOR', and
;;    noted the ANSI escape sequences produced by the MUSH using `think
;;    ansi(r,red)' for example.
;;
;; 2. The code is spaghetti-code, I hate it.
;;
;; 3. If a squence of chars looks like the start of an ANSI sequence,
;;    the chars will be set invisible.  If the squence of chars turns
;;    out not to be an ANSI sequence, this is not undone.  Here is a
;;    teststring: "Is '[3' visible as ^[[3?"  This could be solved by
;;    using `state': it shows most of the time how many characters have
;;    been set invisible.



;;; Code:

(defvar ansi-color-faces-vector
  [default bold  default default underline bold  default modeline]
  "Faces used for ANSI control sequences determining a face.

Those are sequences like this one: [1m, where 1 could be one of the
following numbers: 0 (default), 1 (hilight, rendered as bold), 4
(underline), 5 (flashing, rendered as bold), 7 (inverse, rendered the
same as the modeline)")

(defvar ansi-color-names-vector
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Array of colors.

Used for sequences like this one: [31m, where 1 could be an index to a
foreground color (red, in this case), or [41m, where 1 could be an
index to a background color.

The default colors are: black, red, green, yellow, blue, magenta,
cyan, and white.

On a light background, I prefer: black, red, dark green, orange, blue,
magenta, turquoise, snow4")

;; The main function

(defun ansi-color-to-text-properties (str)
  "Translates ANSI color control sequences into text-properties.  

The ANSI control sequences are made invisible.  The text-properties are
added to the string given in the parameter STR."
  ;; ANSI code for highlighting, example:  boring[1mINTERESTING[0mboring
  ;; state: start with 0, "" -> 1, "[" -> 2, "[013457]" -> 3, 
  ;; "[013457]" -> 4, "m" -> back to 0!
  ;; param: stored when state is 3 (in the above example: 1)
  (let ((str-length (length str))
	(face '(default))
	(i 0) (char) (state 0) (param1) (param2))
    (while (< i str-length)
      (setq char (aref str i))
      (cond
       ;; When writing normal chars (state 0) and happening upon an ANSI sequence.
       ((and (= state 0) (= char ?))
	(setq state 1)); saw escape
       ((and (= state 1) (= char ?\[)); seen escape
	(setq state 2
	      param1 nil
	      param2 nil)); saw [, prepare for param1 and param2!
       ((and (or (= state 2) (= state 3)); reading first or second digit
	     (string-match "[01234567]" (substring str i (1+ i))))
	(if (= state 2); reading first digit
	    ;; [1m (hilight)
	    (setq param1 (string-to-number (substring str i (1+ i)))
		  state 3); prepare to read a second digit or quit.
	  ;; if reading second digit
	  ;; such as [32m (green foreground)
	  (setq param2 (string-to-number (substring str i (1+ i)))
		state 4))); read second digit, prepare to quit
       ((and (or (= state 3) (= state 4)) (= char ?m)); reading last char: m
	(setq state 5); state 5: m will be last invisible char.  Now
	;; reset face according to param1 and param2.
	(if (null param2); only param1 set: no color changes!
	    ;; [0m: default face
	    (if (= param1 0)
		(setq face '(default))
	      ;; [1m: hilight, [7m: inverse, [4m: underline, etc.
	      (add-to-list 'face (aref ansi-color-faces-vector param1)))
	  ;; If param2 is set, we are changing back- or foreground color.
	  (if (= param1 3); first digit told us to change foreground
	      ;; [31m: red foreground
	      (add-to-list 'face (cons 'foreground-color 
				       (aref ansi-color-names-vector param2)))
	    ;; [42m: green background
	    (add-to-list 'face (cons 'background-color 
				     (aref ansi-color-names-vector param2))))))
       (t (setq state 0))); all other cases, state is 0.

      ;; Set text-property for every char.
      (if (> state 0); if reading ANSI codes, state > 0: make them
		     ; invisible.
	  (put-text-property i (1+ i) 'invisible t str)
	;; if reading normal chars, state is 0, put them in the
	;; current face.
	(put-text-property i (1+ i) 'face face str))

      ;; Debug: (message "%c: %d" char state)

      ;; If we just finished reading an ANSI sequence (state 5), reset
      ;; state (state 0).
      (if (> state 4) (setq state 0))
      ;; Next char
      (setq i (1+ i)))))

(provide 'ansi-color)

;;; ansi-colors.el ends here


