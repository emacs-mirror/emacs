;;; ansi-color.el -- translate ANSI into text-properties

;; Copyright (C) 1999  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 2.1.1
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

;;; Testing:

;; If you want to test the setup, evaluate the following fragment in a
;; buffer without font-lock-mode.  This doesn't work in buffers that
;; have font-lock-mode!
;;
;; (insert (ansi-color-apply "\033[1mbold\033[0m and \033[34mblue\033[0m, \033[1m\033[34mbold and blue\033[0m!!"))

;; Usage with TinyMush.el:

;; In order to install this with TinyMush.el, add the following to your
;; .emacs file:
;;
;; (setq tinymud-filter-line-hook 'my-ansi-color-filter)
;; (autoload 'ansi-color-apply "ansi-color" 
;;   "Translates ANSI color control sequences into text-properties." t)
;; (defun my-ansi-color-filter (conn line)
;;   "Call `ansi-color-apply' and then processes things like `filter-line'."
;;   (setq line (ansi-color-apply line))
;;   (if (not (get-value conn 'trigger-disable))
;;       (progn
;; 	(check-triggers conn line
;; 			(get-value conn 'triggers))
;; 	(check-triggers conn line
;; 			(get-value (get-value conn 'world) 'triggers))
;; 	(check-triggers conn line
;; 			tinymud-global-triggers)))
;;   (display-line conn line)
;;   t)

;; Usage with shell-mode:

;; In order to enjoy the marvels of "ls --color=tty" you will have to
;; enter shell-mode using M-x shell, possibly disable font-lock-mode
;; using M-: (font-lock-mode 0), and add ansi-color-apply to
;; comint-preoutput-filter-functions using M-: (add-hook
;; 'comint-preoutput-filter-functions 'ansi-color-apply).



;;; Code:

;; Customization

(defvar ansi-color-faces-vector
  [default bold  default default underline bold  default modeline]
  "Faces used for ANSI control sequences determining a face.

Those are sequences like this one: \033[1m, where 1 could be one of the
following numbers: 0 (default), 1 (hilight, rendered as bold), 4
(underline), 5 (flashing, rendered as bold), 7 (inverse, rendered the
same as the modeline)")

(defvar ansi-color-names-vector
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Array of colors.

Used for sequences like this one: \033[31m, where 1 could be an index to a
foreground color (red, in this case), or \033[41m, where 1 could be an
index to a background color.

The default colors are: black, red, green, yellow, blue, magenta,
cyan, and white.

On a light background, I prefer: black, red, dark green, orange, blue,
magenta, turquoise, snow4")

;; Main function

(defun ansi-color-apply (string)
  "Translates ANSI color control sequences into text-properties.

Applies ANSI control sequences setting foreground and background colors
to STRING and returns the result.  The colors used are given in
`ansi-color-faces-vector' and `ansi-color-names-vector'.

This function can be added to `comint-preoutput-filter-functions'."
  (let ((face)
	(start 0) (end) (escape)
	(result)
	(params))
    ;; find the next escape sequence
    (while (setq end (string-match "\033\\[\\([01347][01234567]?;\\)*[01347][01234567]?m" string start))
      ;; store escape sequence
      (setq escape (match-string 0 string))
      ;; colorize the old block from start to end using old face
      (if face
	  (put-text-property start end 'face face string))
      (setq result (concat result (substring string start end)))
      ;; create new face by applying all the parameters in the escape sequence
      (let ((i 0))
	(while (setq i (string-match "[01347][01234567]?[;m]" escape i))
	  (setq face (ansi-color-make-face face
					   (aref escape i)
					   (aref escape (1+ i))))
	  (setq i (match-end 0))))
      (setq start (+ end (length escape))))
    (concat result (substring string start))))

;; Helper functions

(defun ansi-color-make-face (face param1 param2)
  "Return a face based on FACE and characters PARAM1 and PARAM2.

The face can be used in a call to `add-text-properties'.  The PARAM1 and
PARAM2 characters are the two numeric characters in ANSI control
sequences between ?[ and ?m.  Unless the ANSI control sequence specifies
a return to default face using PARAM1 ?0 and PARAM2 ?m (ie. \"\033[0m\"), the
properties specified by PARAM1 and PARAM2 are added to face."
  (cond ((= param1 ?0)
	 nil)
	((= param2 ?m)
	 (add-to-list 'face (aref ansi-color-faces-vector
				  (string-to-number (char-to-string param1)))))
	((= param1 ?3)
	 (add-to-list 'face (cons 'foreground-color
				  (aref ansi-color-names-vector
					(string-to-number (char-to-string param2))))))
	((= param1 ?4)
	 (add-to-list 'face (cons 'background-color
				  (aref ansi-color-names-vector
					(string-to-number (char-to-string param2))))))
	(t (add-to-list 'face (aref ansi-color-faces-vector
				  (string-to-number (char-to-string param1)))))))

(provide 'ansi-color)

;;; ansi-color.el ends here
