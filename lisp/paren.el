;;; paren.el --- highlight matching paren.

;; Copyright (C) 1993, 1996 Free Software Foundation, Inc.

;; Author: rms@gnu.org
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

(defgroup paren-showing nil
  "Showing (un)matching of parens and expressions."
  :prefix "show-paren-"
  :group 'paren-matching)

;; This is the overlay used to highlight the matching paren.
(defvar show-paren-overlay nil)
;; This is the overlay used to highlight the closeparen right before point.
(defvar show-paren-overlay-1 nil)

(defcustom show-paren-mode nil
  "*Toggle Show Paren mode.
When Show Paren mode is enabled, any matching parenthesis is highlighted
after `show-paren-delay' seconds of Emacs idle time.
Setting this variable directly does not take effect;
use either \\[customize] or the function `show-paren-mode'."
  :set (lambda (symbol value)
	 (show-paren-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'paren-showing
  :require 'paren)

(defcustom show-paren-style 'parenthesis
  "*Style used when showing a matching paren.
Valid styles are `parenthesis' (meaning show the matching paren),
`expression' (meaning show the entire expression enclosed by the paren) and
`mixed' (meaning show the matching paren if it is visible, and the expression
otherwise)."
  :type '(choice (const parenthesis) (const expression) (const mixed))
  :group 'paren-showing)

(defcustom show-paren-delay
  (if (featurep 'lisp-float-type) (/ (float 1) (float 8)) 1)
  "*Time in seconds to delay before showing a matching paren."
  :type '(number :tag "seconds")
  :group 'paren-showing)

(defcustom show-paren-ring-bell-on-mismatch nil
  "*If non-nil, beep if mismatched paren is detected."
  :type 'boolean
  :group 'paren-showing
  :version "20.3")
  
(defface show-paren-match-face
  '((((class color)) (:background "turquoise"))
    (t (:background "gray")))
  "Show Paren mode face used for a matching paren."
  :group 'faces
  :group 'paren-showing)

(defface show-paren-mismatch-face
  '((((class color)) (:foreground "white" :background "purple"))
    (t (:reverse-video t)))
  "Show Paren mode face used for a mismatching paren."
  :group 'faces
  :group 'paren-showing)

(defvar show-paren-idle-timer nil)

;;;###autoload
(defun show-paren-mode (&optional arg)
  "Toggle Show Paren mode.
With prefix ARG, turn Show Paren mode on if and only if ARG is positive.
Returns the new status of Show Paren mode (non-nil means on).

When Show Paren mode is enabled, any matching parenthesis is highlighted
in `show-paren-style' after `show-paren-delay' seconds of Emacs idle time."
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
		(not show-paren-mode))))
    (setq show-paren-mode on-p)
    ;; Turn off the usual paren-matching method
    ;; when this one is turned on.
    (if (local-variable-p 'show-paren-mode)
	(make-local-variable 'blink-matching-paren-on-screen)
      (kill-local-variable 'blink-matching-paren-on-screen))
    (setq blink-matching-paren-on-screen (not on-p))

    ;; Now enable or disable the mechanism.
    ;; First get rid of the old idle timer.
    (if show-paren-idle-timer
	(cancel-timer show-paren-idle-timer))
    (setq show-paren-idle-timer nil)
    ;; If show-paren-mode is enabled in some buffer now,
    ;; set up a new timer.
    (when (memq t (mapcar (lambda (buffer)
			    (with-current-buffer buffer
			      show-paren-mode))
			  (buffer-list)))
      (setq show-paren-idle-timer (run-with-idle-timer
				   show-paren-delay t
				   'show-paren-function)))
    (unless on-p
      (and show-paren-overlay
	   (eq (overlay-buffer show-paren-overlay) (current-buffer))
	   (delete-overlay show-paren-overlay))
      (and show-paren-overlay-1
	   (eq (overlay-buffer show-paren-overlay-1) (current-buffer))
	   (delete-overlay show-paren-overlay-1)))))

;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun show-paren-function ()
  (if show-paren-mode
      (let (pos dir mismatch face (oldpos (point)))
	(cond ((eq (char-syntax (preceding-char)) ?\))
	       (setq dir -1))
	      ((eq (char-syntax (following-char)) ?\()
	       (setq dir 1)))
	;;
	;; Find the other end of the sexp.
	(when dir
	  (save-excursion
	    (save-restriction
	      ;; Determine the range within which to look for a match.
	      (when blink-matching-paren-distance
		(narrow-to-region
		 (max (point-min) (- (point) blink-matching-paren-distance))
		 (min (point-max) (+ (point) blink-matching-paren-distance))))
	      ;; Scan across one sexp within that range.
	      ;; Errors or nil mean there is a mismatch.
	      (condition-case ()
		  (setq pos (scan-sexps (point) dir))
		(error (setq pos t mismatch t)))
	      ;; If found a "matching" paren, see if it is the right
	      ;; kind of paren to match the one we started at.
	      (when (integerp pos)
		(let ((beg (min pos oldpos)) (end (max pos oldpos)))
		  (when (/= (char-syntax (char-after beg)) ?\$)
		    (setq mismatch
			  (not (eq (char-before end)
				   ;; This can give nil.
				   (matching-paren (char-after beg)))))))))))
	;;
	;; Highlight the other end of the sexp, or unhighlight if none.
	(if (not pos)
	    (progn
	      ;; If not at a paren that has a match,
	      ;; turn off any previous paren highlighting.
	      (and show-paren-overlay (overlay-buffer show-paren-overlay)
		   (delete-overlay show-paren-overlay))
	      (and show-paren-overlay-1 (overlay-buffer show-paren-overlay-1)
		   (delete-overlay show-paren-overlay-1)))
	  ;;
	  ;; Use the correct face.
	  (if mismatch
	      (progn
		(if show-paren-ring-bell-on-mismatch
		    (beep))
		(setq face 'show-paren-mismatch-face))
	    (setq face 'show-paren-match-face))
	  ;;
	  ;; If matching backwards, highlight the closeparen
	  ;; before point as well as its matching open.
	  ;; If matching forward, and the openparen is unbalanced,
	  ;; highlight the paren at point to indicate misbalance.
	  ;; Otherwise, turn off any such highlighting.
	  (if (and (= dir 1) (integerp pos))
	      (when (and show-paren-overlay-1
			 (overlay-buffer show-paren-overlay-1))
		(delete-overlay show-paren-overlay-1))
	    (let ((from (if (= dir 1)
			    (point)
			  (forward-point -1)))
		  (to (if (= dir 1)
			  (forward-point 1)
			(point))))
	      (if show-paren-overlay-1
		  (move-overlay show-paren-overlay-1 from to (current-buffer))
		(setq show-paren-overlay-1 (make-overlay from to)))
	      ;; Always set the overlay face, since it varies.
	      (overlay-put show-paren-overlay-1 'face face)))
	  ;;
	  ;; Turn on highlighting for the matching paren, if found.
	  ;; If it's an unmatched paren, turn off any such highlighting.
	  (unless (integerp pos)
	    (delete-overlay show-paren-overlay))
	  (let ((to (if (or (eq show-paren-style 'expression)
			    (and (eq show-paren-style 'mixed)
				 (not (pos-visible-in-window-p pos))))
			(point)
		      pos))
		(from (if (or (eq show-paren-style 'expression)
			      (and (eq show-paren-style 'mixed)
				   (not (pos-visible-in-window-p pos))))
			  pos
			(save-excursion
			  (goto-char pos)
			  (forward-point (- dir))))))
	    (if show-paren-overlay
		(move-overlay show-paren-overlay from to (current-buffer))
	      (setq show-paren-overlay (make-overlay from to))))
	  ;;
	  ;; Always set the overlay face, since it varies.
	  (overlay-put show-paren-overlay 'face face)))
    ;; show-paren-mode is nil in this buffer.
    (and show-paren-overlay
	 (delete-overlay show-paren-overlay))
    (and show-paren-overlay-1
	 (delete-overlay show-paren-overlay-1))))

(provide 'paren)

(if show-paren-mode
    (show-paren-mode t))

;;; paren.el ends here
