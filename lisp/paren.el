;;; paren.el --- highlight matching paren.

;; Copyright (C) 1993, 1996 Free Software Foundation, Inc.

;; Author: rms@gnu.ai.mit.edu
;; Maintainer: FSF
;; Keywords: languages, faces

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

;; Load this and it will display highlighting on whatever
;; paren matches the one before or after point.

;;; Code:

;; This is the overlay used to highlight the matching paren.
(defvar show-paren-overlay nil)
;; This is the overlay used to highlight the closeparen right before point.
(defvar show-paren-overlay-1 nil)

(defvar show-paren-mode nil)
(defvar show-paren-idle-timer nil)

(defvar show-paren-mismatch-face nil)

(defvar show-paren-delay (if (featurep 'lisp-float-type) 0.125 1)
  "*Time in seconds to delay before showing the matching paren.")

(defvar show-paren-face 'region
  "*Name of the face to use for showing the matching paren.")

;;;###autoload
(defun show-paren-mode (&optional arg)
  "Toggle Show Paren mode.
With prefix ARG, turn Show Paren mode on if and only if ARG is positive.
Returns the new status of Show Paren mode (non-nil means on).

When Show Paren mode is enabled, any matching parenthesis is highlighted
after `show-paren-delay' seconds of Emacs idle time."
  (interactive "P")
  (if window-system
      (let ((on-p (if arg
		      (> (prefix-numeric-value arg) 0)
		    (not show-paren-mode))))
	(setq blink-matching-paren-on-screen (not on-p))
	(and show-paren-idle-timer (cancel-timer show-paren-idle-timer))
	(if on-p
	    (setq show-paren-idle-timer (run-with-idle-timer show-paren-delay t
					 'show-paren-function))
	  (and show-paren-overlay (overlay-buffer show-paren-overlay)
	       (delete-overlay show-paren-overlay))
	  (and show-paren-overlay-1 (overlay-buffer show-paren-overlay-1)
	       (delete-overlay show-paren-overlay-1)))
	(setq show-paren-mode on-p))))

;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun show-paren-function ()
  ;; Do nothing if no window system to display results with.
  ;; Do nothing if executing keyboard macro.
  ;; Do nothing if input is pending.
  (if window-system
      (let (pos dir mismatch (oldpos (point))
		(face show-paren-face))
	(cond ((eq (char-syntax (preceding-char)) ?\))
	       (setq dir -1))
	      ((eq (char-syntax (following-char)) ?\()
	       (setq dir 1)))
	(if dir
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
				 (not (eq (char-after (1- end))
					  ;; This can give nil.
					  (matching-paren (char-after beg))))))))
		;; If they don't properly match, use a different face,
		;; or print a message.
		(if mismatch
		    (progn
		      (and (null show-paren-mismatch-face)
			   (x-display-color-p)
			   (progn
			     (add-to-list 'facemenu-unlisted-faces 
					  'paren-mismatch)
			     (make-face 'paren-mismatch)
			     (or (face-nontrivial-p 'paren-mismatch t)
				 (progn
				   (set-face-background 'paren-mismatch
							"purple")
				   (set-face-foreground 'paren-mismatch
							"white")))
			     (setq show-paren-mismatch-face 'paren-mismatch)))
		      (if show-paren-mismatch-face
			  (setq face show-paren-mismatch-face)
			(message "Paren mismatch"))))
		)))
	(cond (pos
	       (if (= dir -1)
		   ;; If matching backwards, highlight the closeparen
		   ;; before point as well as its matching open.
		   (progn
		     (if show-paren-overlay-1
			 (move-overlay show-paren-overlay-1
				       (+ (point) dir) (point)
				       (current-buffer))
		       (setq show-paren-overlay-1
			     (make-overlay (+ (point) dir) (point))))
		     ;; Always set the overlay face, since it varies.
		     (overlay-put show-paren-overlay-1 'face face))
		 ;; Otherwise, turn off any such highlighting.
		 (and show-paren-overlay-1
		      (overlay-buffer show-paren-overlay-1)
		      (delete-overlay show-paren-overlay-1)))
	       ;; Turn on highlighting for the matching paren.
	       (if show-paren-overlay
		   (move-overlay show-paren-overlay (- pos dir) pos
				 (current-buffer))
		 (setq show-paren-overlay
		       (make-overlay (- pos dir) pos)))
	       ;; Always set the overlay face, since it varies.
	       (overlay-put show-paren-overlay 'face face))
	      (t
	       ;; If not at a paren that has a match,
	       ;; turn off any previous paren highlighting.
	       (and show-paren-overlay (overlay-buffer show-paren-overlay)
		    (delete-overlay show-paren-overlay))
	       (and show-paren-overlay-1 (overlay-buffer show-paren-overlay-1)
		    (delete-overlay show-paren-overlay-1)))))))

;;; For back compatibility we turn ourselves on if we're dumped or loaded.
(add-hook 'window-setup-hook '(lambda () (show-paren-mode t)))
(show-paren-mode t)

(provide 'paren)

;;; paren.el ends here
