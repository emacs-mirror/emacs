;;; paren.el --- highlight matching paren.
;; Copyright (C) 1993 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Load this and it will display highlighting on whatever
;; paren matches the one before or after point.

;;; Code:

(defvar blink-paren-overlay nil)

;; Find the place to blink, if there is one,
;; and blink it until input arrives.
(defun blink-paren-command-hook ()
  (let (pos dir mismatch (oldpos (point))
	    (face (if (face-equal 'highlight 'region)
		      'underline 'highlight)))
    (cond ((eq (char-syntax (following-char)) ?\()
	   (setq dir 1))
	  ((eq (char-syntax (preceding-char)) ?\))
	   (setq dir -1)))
    (save-excursion
      (save-restriction
	;; Determine the range within which to look for a match.
	(if blink-matching-paren-distance
	    (narrow-to-region (max (point-min)
				   (- (point) blink-matching-paren-distance))
			      (min (point-max)
				   (+ (point) blink-matching-paren-distance))))
	;; Scan across one sexp within that range.
	(condition-case ()
	    (setq pos (scan-sexps (point) dir))
	  (error nil))
	;; See if the "matching" paren is the right kind of paren
	;; to match the one we started at.
	(if pos
	    (let ((beg (min pos oldpos)) (end (max pos oldpos)))
	      (and (/= (char-syntax (char-after beg)) ?\$)
		   (setq mismatch
			 (/= (char-after (1- end))
			     (logand (lsh (aref (syntax-table)
						(char-after beg))
					  -8)
				     255))))))
	;; If they don't properly match, don't blink.
	(if mismatch
	    (setq pos nil))))
    (cond (pos
	   (if blink-paren-overlay
	       (move-overlay blink-paren-overlay (- pos dir) pos)
	     (setq blink-paren-overlay
		   (make-overlay (- pos dir) pos)))
	   (overlay-put blink-paren-overlay 'face face)
;;; This is code to blink the highlighting.
;;; It is desirable to avoid this because
;;; it would interfere with auto-save and gc when idle.
;;;	   (while (sit-for 1)
;;;	     (overlay-put blink-paren-overlay
;;;			  'face
;;;			  (if (overlay-get blink-paren-overlay
;;;					   'face)
;;;			      nil face)))
	   )
	  (t
	   (delete-overlay blink-paren-overlay)))))

(add-hook 'post-command-hook 'blink-paren-command-hook)

