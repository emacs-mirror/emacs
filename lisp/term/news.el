;; keypad and function key bindings for the Sony NEWS keyboard.
;; Copyright (C) 1989 Free Software Foundation, Inc.

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

;; This file effects a mapping from the raw escape sequences of various
;; keypad and function keys to the symbols used by emacs to represent
;; those keys.  The mapping from key symbol to the function performed
;; when that key is pressed is handled keyboard-independently by the file
;; ../keypad.el.

;; Note that his file is also used under X11.  For this to work, the variable
;; names must not change from keyboard file to keyboard file, nor can the
;; structure of keypad-maps change.

(require 'keypad)

(defvar keypads nil
  "Keypad and function keys keymap for Sony News machine.")

(defvar keypad-maps nil
  "A list of strings sent by the keypad and function keys on the Sony News.
There is an element for each unique prefix.  Each element is of the form
(PREFIX map map ...), each map being (string . symbol).")

(setq keypad-maps '(("\eO"
			("P" . function-1)
			("Q" . function-2)
			("R" . function-3)
			("S" . function-4)
			("T" . function-5)
			("U" . function-6)
			("V" . function-7)
			("W" . function-8)
			("X" . function-9)
			("Y" . function-10)

			("m" . keypad-subtract)
			("k" . keypad-add)
			("l" . keypad-comma)
			("n" . keypad-period)
			("M" . keypad-enter)

			("p" . keypad-0)
			("q" . keypad-1)
			("r" . keypad-2)
			("s" . keypad-3)
			("t" . keypad-4)
			("u" . keypad-5)
			("v" . keypad-6)
			("w" . keypad-7)
			("x" . keypad-8)
			("y" . keypad-9)
	
			     ; These three strings are just made up.
			("a"	. execute)       ; enter
			("b"	. select)        ; nfer
			("c"	. cancel))))     ; xfer

(let ((pads keypad-maps))
  (while pads
    (unwind-protect
	(let* ((prefix (car (car pads)))
	       (stringmap (cdr (car pads)))
	       (padmap (if (lookup-key global-map prefix)
			   (error "Keymap entry for keypad prefix already exisists")
			 (make-sparse-keymap))))
	  (define-key global-map prefix padmap)
	  (setup-terminal-keymap padmap stringmap))
      (setq pads (cdr pads)))))
