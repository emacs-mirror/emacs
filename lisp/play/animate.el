;;; animate.el --- make text dance

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Maintainer: Richard Stallman <rms@gnu.org>
;; Keywords: games

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

;; (animate-string STRING VPOS &optional HPOS)
;; makes the string STRING appear starting at VPOS, HPOS
;; by having each letter swoop into place from random starting position.

;;; Code:

;;; STRING is the string to be displayed,
;;; and DEST-X, DEST-Y say where on the screen
;;; it should end up.

;;; This function returns a list describing
;;; all the characters and the paths they should take.
;;; Each element has the form
;;;  (CHAR START-Y START-X DEST-Y DEST-X).

;;; The start position of each character is chosen randomly.
;;; The destination is chosen to put it in the right place
;;; in the string when the whole string finally reaches its
;;; specified position.

(defun animate-initialize (string vpos hpos)
  (let ((characters nil))
    (dotimes (i (length string))
      (setq characters
	    (cons (list (aref string i)
			;; Random starting positions.
			(random (window-height))
			(random (1- (window-width)))
			;; All the chars should end up
			;; on the specified line.
			vpos
			;; The Ith character in the string
			;; needs to end up I positions later.
			(+ hpos i))
		  characters)))
    characters))

;;; Display the characters in CHARACTERS,
;;; each one FRACTION of the way from its start to its destination.
;;; If FRACTION is 0, the characters appear in their starting positions.
;;; If FRACTION is 1, the characters appear in their destinations.

(defun animate-step (characters fraction)
  (let ((remains (- 1 fraction)))
    (dolist (item characters)
      (let ((vpos (+ (* remains (nth 1 item))
		     (* fraction (nth 3 item))))
	    (hpos (+ (* remains (nth 2 item))
		     (* fraction (nth 4 item)))))
	(animate-place-char (car item) vpos hpos)))))

;;; Place the character CHAR at position VPOS, HPOS in the current buffer.
(defun animate-place-char (char vpos hpos)
  (goto-char (window-start))
  (let ((next-line-add-newlines t))
    (dotimes (i vpos)
      (next-line 1)))
  (beginning-of-line)
  (move-to-column (floor hpos) t)
  (unless (eolp) (delete-char 1))
  (insert-char char 1))

(defvar animate-n-steps 10
  "Number of steps to use `animate-string'.")

;;;###autoload
(defun animate-string (string vpos &optional hpos)
  "Display STRING starting at position VPOS, HPOS, using animation.
The characters start at randomly chosen places,
and all slide in parallel to their final positions,
passing through `animate-n-steps' positions before the final ones.
If HPOS is nil (or omitted), center the string horizontally
in the current window."
  (let ((characters
	 (animate-initialize string vpos
			     (or hpos
				 ;; HPOS unspecified, so compute
				 ;; it so as to center the string.
				 (max 0 (/ (- (window-width) (length string)) 2))))))
    (dotimes (i animate-n-steps)
      ;; Bind buffer-undo-list so it will be unchanged when we are done.
      ;; (We're going to undo all our changes anyway.)
      (let (buffer-undo-list
	    list-to-undo)
	;; Display the characters at the Ith position.
	;; This inserts them in the buffer.
	(animate-step characters (/ i 1.0 animate-n-steps))
	;; Make sure buffer is displayed starting at the beginning.
	(set-window-start nil 1)
	;; Display it, and wait just a little while.
	(sit-for .05)
	;; Now undo the changes we made in the buffer.
	(setq list-to-undo buffer-undo-list)
	(while list-to-undo
	  (let ((undo-in-progress t))
	    (setq list-to-undo (primitive-undo 1 list-to-undo))))))
    ;; Insert the characters in their final positions.
    (animate-step characters 1)
    ;; Put the cursor at the end of the text on the line.
    (end-of-line)
    ;; Redisplay so they appear on the screen there.
    (sit-for 0)
    ;; This is so that the undo command, used afterwards,
    ;; will undo the "animate" calls one by one.
    (undo-boundary)))

;;; animate.el ends here
