;;; pc-win.el --- setup support for `PC windows' (whatever that is)

;; Copyright (C) 1994, 1996, 1997, 1999, 2001 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
;; Maintainer: FSF

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

;;; Code:

(load "term/internal" nil t)

;;; This is copied from etc/rgb.txt, except that some values were changed
;;; a bit to make them consistent with DOS console colors, and the RGB
;;; values were scaled up to 16 bits, as `tty-define-color' requires.
;;;
;;; The mapping between the 16 standard EGA/VGA colors and X color names
;;; was done by running a Unix version of Emacs inside an X client and a
;;; DJGPP-compiled Emacs on the same PC.  The names of X colors used to
;;; define the pixel values are shown as comments to each color below.
;;;
;;; If you want to change the RGB values, keep in mind that various pieces
;;; of Emacs think that a color whose RGB values add up to less than 0.6 of
;;; the values for WHITE (i.e. less than 117963) are ``dark'', otherwise the
;;; color is ``light''; see `frame-set-background-mode' in lisp/faces.el for
;;; an example.
(defvar msdos-color-values
  '(("black"          0     0     0     0)
    ("blue"           1     0     0 52480) ; MediumBlue
    ("green"          2  8704 35584  8704) ; ForestGreen
    ("cyan"           3     0 52736 53504) ; DarkTurquoise
    ("red"            4 45568  8704  8704) ; FireBrick
    ("magenta"        5 35584     0 35584) ; DarkMagenta
    ("brown"          6 40960 20992 11520) ; Sienna
    ("lightgray"      7 48640 48640 48640) ; Gray
    ("darkgray"       8 26112 26112 26112) ; Gray40
    ("lightblue"      9     0     0 65535) ; Blue
    ("lightgreen"    10     0 65535     0) ; Green
    ("lightcyan"     11     0 65535 65535) ; Cyan
    ("lightred"      12 65535     0     0) ; Red
    ("lightmagenta"  13 65535     0 65535) ; Magenta
    ("yellow"        14 65535 65535     0) ; Yellow
    ("white"         15 65535 65535 65535))
  "A list of MS-DOS console colors, their indices and 16-bit RGB values.")

;; ---------------------------------------------------------------------------
;; We want to delay setting frame parameters until the faces are setup
(defvar default-frame-alist nil)
(modify-frame-parameters terminal-frame default-frame-alist)
(tty-color-clear)

(defun msdos-face-setup ()
  (set-face-foreground 'bold "yellow" terminal-frame)
  (set-face-foreground 'italic "red" terminal-frame)
  (set-face-foreground 'bold-italic "lightred" terminal-frame)
  (set-face-foreground 'underline "white" terminal-frame)

  (make-face 'msdos-menu-active-face)
  (make-face 'msdos-menu-passive-face)
  (make-face 'msdos-menu-select-face)
  (set-face-foreground 'msdos-menu-active-face "white" terminal-frame)
  (set-face-foreground 'msdos-menu-passive-face "lightgray" terminal-frame)
  (set-face-background 'msdos-menu-active-face "blue" terminal-frame)
  (set-face-background 'msdos-menu-passive-face "blue" terminal-frame)
  (set-face-background 'msdos-menu-select-face "red" terminal-frame))

(add-hook 'before-init-hook 'msdos-face-setup)

(defun msdos-handle-reverse-video (frame parameters)
  "Handle the reverse-video frame parameter on MS-DOS frames."
  (when (cdr (or (assq 'reverse parameters)
		 (assq 'reverse default-frame-alist)))
      (let* ((params (frame-parameters frame))
	     (fg (cdr (assq 'foreground-color params)))
	     (bg (cdr (assq 'background-color params))))
	(if (equal fg (cdr (assq 'mouse-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'mouse-color bg))))
	(if (equal fg (cdr (assq 'cursor-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'cursor-color bg)))))))

;; This must run after all the default colors are inserted into
;; tty-color-alist, since msdos-handle-reverse-video needs to know the
;; actual frame colors.  tty-color-alist is set up by startup.el, but
;; only after it runs before-init-hook and after-init-hook.
(defun msdos-setup-initial-frame ()
  (modify-frame-parameters terminal-frame default-frame-alist)
  ;; This remembers the screen colors after applying default-frame-alist,
  ;; so that all subsequent frames could begin with those colors.
  (msdos-remember-default-colors terminal-frame)
  (modify-frame-parameters terminal-frame initial-frame-alist)
  (msdos-handle-reverse-video terminal-frame
			      (frame-parameters terminal-frame))

  (frame-set-background-mode terminal-frame)
  (face-set-after-frame-default terminal-frame))

(add-hook 'term-setup-hook 'msdos-setup-initial-frame)

;; We create frames as if we were a terminal, but with a twist.
(defun make-msdos-frame (&optional parameters)
  (let ((frame (make-terminal-frame parameters))
	success)
    (unwind-protect
	(progn
	  (msdos-handle-reverse-video frame (frame-parameters frame))
	  (frame-set-background-mode frame)
	  (face-set-after-frame-default frame)
	  (setq success t))
      (unless success (delete-frame frame)))
    frame))

(setq frame-creation-function 'make-msdos-frame)

;; ---------------------------------------------------------------------------
;; More or less useful imitations of certain X-functions.  A lot of the
;; values returned are questionable, but usually only the form of the
;; returned value matters.  Also, by the way, recall that `ignore' is
;; a useful function for returning 'nil regardless of argument.

;; From src/xfns.c
(defun x-list-fonts (pattern &optional face frame maximum width)
  (if (or (null width) (and (numberp width) (= width 1)))
      (list "ms-dos")
    (list "no-such-font")))
(defun x-display-pixel-width (&optional frame) (frame-width frame))
(defun x-display-pixel-height (&optional frame) (frame-height frame))
(defun x-display-planes (&optional frame) 4) ;bg switched to 16 colors as well
(defun x-display-color-cells (&optional frame) 16)
(defun x-server-max-request-size (&optional frame) 1000000) ; ???
(defun x-server-vendor (&optional frame) t "GNU")
(defun x-server-version (&optional frame) '(1 0 0))
(defun x-display-screens (&optional frame) 1)
(defun x-display-mm-height (&optional frame) 245) ; Guess the size of my
(defun x-display-mm-width (&optional frame) 322)  ; monitor, EZ...
(defun x-display-backing-store (&optional frame) 'not-useful)
(defun x-display-visual-class (&optional frame) 'static-color)
(fset 'x-display-save-under 'ignore)
(fset 'x-get-resource 'ignore)

;; From lisp/term/x-win.el
(defvar x-display-name "pc"
  "The display name specifying the MS-DOS display and frame type.")
(setq split-window-keep-point t)
(defvar x-colors (mapcar 'car msdos-color-values)
  "The list of colors available on a PC display under MS-DOS.")

;; From lisp/term/w32-win.el
;
;;;; Selections and cut buffers
;
;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-cut-buffer-or-selection-value.
(defvar x-last-selected-text nil)

(defcustom x-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is the default on this system, since MS-Windows does not
support other types of selections."
  :type 'boolean
  :group 'killing)

(defun x-select-text (text &optional push)
  (if x-select-enable-clipboard
      (w16-set-clipboard-data text))
  (setq x-last-selected-text text))

;;; Return the value of the current selection.
;;; Consult the selection, then the cut buffer.  Treat empty strings
;;; as if they were unset.
(defun x-get-selection-value ()
  (if x-select-enable-clipboard
      (let (text)
	;; Don't die if x-get-selection signals an error.
	(condition-case c
	    (setq text (w16-get-clipboard-data))
	  (error (message "w16-get-clipboard-data:%s" c)))
	(if (string= text "") (setq text nil))
	(cond
	 ((not text) nil)
	 ((eq text x-last-selected-text) nil)
	 ((string= text x-last-selected-text)
	  ;; Record the newer string, so subsequent calls can use the 'eq' test.
	  (setq x-last-selected-text text)
	  nil)
	 (t
	  (setq x-last-selected-text text))))))

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)

;; From lisp/faces.el: we only have one font, so always return
;; it, no matter which variety they've asked for.
(defun x-frob-font-slant (font which)
  font)
(make-obsolete 'x-frob-font-slant 'make-face-... "21.1")
(defun x-frob-font-weight (font which)
  font)
(make-obsolete 'x-frob-font-weight 'make-face-... "21.1")
(defun x-font-family-list ()
  "Return a list of available font families on FRAME.\n\
If FRAME is omitted or nil, use the selected frame.\n\
Value is a list of conses (FAMILY . FIXED-P) where FAMILY\n\
is a font family, and FIXED-P is non-nil if fonts of that family\n\
are fixed-pitch."
  '(("default" . t)))

;; From src/fontset.c:
(fset 'query-fontset 'ignore)

;; From lisp/term/x-win.el: make iconify-or-deiconify-frame a no-op.
(fset 'iconify-or-deiconify-frame 'ignore)

;; From lisp/frame.el
(fset 'set-default-font 'ignore)
(fset 'set-mouse-color 'ignore)		; We cannot, I think.
(fset 'set-cursor-color 'ignore)	; Hardware determined by char under.
(fset 'set-border-color 'ignore)	; Not useful.

;; ---------------------------------------------------------------------------

;;; arch-tag: 5cbdb455-b495-427b-95d0-e417d77d00b4
;;; pc-win.el ends here
