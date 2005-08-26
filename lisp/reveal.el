;;; reveal.el --- Automatically reveal hidden text at point

;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: outlines

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Reveal mode is a minor mode that makes sure that text around point
;; is always visible.  When point enters a region of hidden text,
;; `reveal-mode' temporarily makes it visible.
;;
;; This is normally used in conjunction with `outline-minor-mode',
;; `hs-minor-mode', `hide-ifdef-mode', ...
;;
;; It only works with packages that hide text using overlays.
;; Packages can provide special support for it by placing
;; a function in the `reveal-toggle-invisible' property on the symbol
;; used as the value of the `invisible' overlay property.
;; The function is called right after revealing (or re-hiding) the
;; text with two arguments: the overlay and a boolean that's non-nil
;; if we have just revealed the text.  When revealing, that function
;; may re-hide some of the text.

;;; Todo:

;; - find other hysteresis features.

;;; Code:

(require 'pcvs-util)

(defgroup reveal nil
  "Reveal hidden text on the fly."
  :group 'editing)

(defcustom reveal-around-mark t
  "Reveal text around the mark, if active."
  :type 'boolean
  :group 'reveal)

(defvar reveal-open-spots nil)
(make-variable-buffer-local 'reveal-open-spots)

(defvar reveal-last-tick nil)
(make-variable-buffer-local 'reveal-last-tick)

;; Actual code

(defun reveal-post-command ()
  ;; Refresh the spots that might have changed.
  ;; `Refreshing' here means to try and re-hide the corresponding text.
  ;; We don't refresh everything correctly:
  ;; - we only refresh spots in the current window.
  ;; FIXME: do we actually know that (current-buffer) = (window-buffer) ?
  (with-local-quit
  (condition-case err
   (let* ((spots (cvs-partition
		  (lambda (x)
		    ;; We refresh any spot in the current window as well
		    ;; as any spots associated with a dead window or a window
		    ;; which does not show this buffer any more.
		    (or (eq (car x) (selected-window))
			(not (window-live-p (car x)))
			(not (eq (window-buffer (car x))
				 (current-buffer)))))
		  reveal-open-spots))
	  (old-ols (mapcar 'cdr (car spots)))
	  (repeat t))
     (setq reveal-open-spots (cdr spots))
     ;; Open new overlays.
     (while repeat
       (setq repeat nil)
       (dolist (ol (nconc (when (and reveal-around-mark mark-active)
			    (overlays-at (mark)))
			  (overlays-at (point))))
	 (push (cons (selected-window) ol) reveal-open-spots)
	 (setq old-ols (delq ol old-ols))
	 (let ((inv (overlay-get ol 'invisible)) open)
	   (when (and inv
		      ;; There's an `invisible' property.  Make sure it's
		      ;; actually invisible.
		      (or (not (listp buffer-invisibility-spec))
			  (memq inv buffer-invisibility-spec)
			  (assq inv buffer-invisibility-spec))
		      (or (setq open
				(or (overlay-get ol 'reveal-toggle-invisible)
				    (and (symbolp inv)
					 (get inv 'reveal-toggle-invisible))
				    (overlay-get ol 'isearch-open-invisible-temporary)))
			  (overlay-get ol 'isearch-open-invisible)
			  (and (consp buffer-invisibility-spec)
			       (cdr (assq inv buffer-invisibility-spec))))
		      (overlay-put ol 'reveal-invisible inv))
	     (if (null open)
		 (overlay-put ol 'invisible nil)
	       ;; Use the provided opening function and repeat (since the
	       ;; opening function might have hidden a subpart around point).
	       (setq repeat t)
	       (condition-case err
		   (funcall open ol nil)
		 (error (message "!!Reveal-show (funcall %s %s nil): %s !!"
				 open ol err)
			;; Let's default to a meaningful behavior to avoid
			;; getting stuck in an infinite loop.
			(setq repeat nil)
			(overlay-put ol 'invisible nil))))))))
     ;; Close old overlays.
     (if (not (eq reveal-last-tick
		  (setq reveal-last-tick (buffer-modified-tick))))
	 ;; The buffer was modified since last command: let's refrain from
	 ;; closing any overlay because it tends to behave poorly when
	 ;; inserting text at the end of an overlay (basically the overlay
	 ;; should be rear-advance when it's open, but things like
	 ;; outline-minor-mode make it non-rear-advance because it's
	 ;; a better choice when it's closed).
	 (dolist (ol old-ols)
	   (push (cons (selected-window) ol) reveal-open-spots))
       ;; The last command was only a point motion or some such
       ;; non-buffer-modifying command.  Let's close whatever can be closed.
       (dolist (ol old-ols)
	 (when (and (eq (current-buffer) (overlay-buffer ol))
		    (not (rassq ol reveal-open-spots)))
	   (if (and (>= (point) (save-excursion
				  (goto-char (overlay-start ol))
				  (line-beginning-position 1)))
		    (<= (point) (save-excursion
				  (goto-char (overlay-end ol))
				  (line-beginning-position 2))))
	       ;; Still near the overlay: keep it open.
	       (push (cons (selected-window) ol) reveal-open-spots)
	     ;; Really close it.
	     (let ((open (overlay-get ol 'reveal-toggle-invisible)) inv)
	       (if (or open
		       (and (setq inv (overlay-get ol 'reveal-invisible))
			    (setq open (or (get inv 'reveal-toggle-invisible)
					   (overlay-get ol 'isearch-open-invisible-temporary)))))
		   (condition-case err
		       (funcall open ol t)
		     (error (message "!!Reveal-hide (funcall %s %s t): %s !!"
				     open ol err)))
		 (overlay-put ol 'invisible inv))))))))
   (error (message "Reveal: %s" err)))))

(defvar reveal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Override the default move-beginning-of-line and move-end-of-line
    ;; which skips valuable invisible text.
    (define-key map [remap move-beginning-of-line] 'beginning-of-line)
    (define-key map [remap move-end-of-line] 'end-of-line)
    map))

;;;###autoload
(define-minor-mode reveal-mode
  "Toggle Reveal mode on or off.
Reveal mode renders invisible text around point visible again.

Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  :group 'reveal
  :lighter (global-reveal-mode nil " Reveal")
  :keymap reveal-mode-map
  (if reveal-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'reveal-post-command nil t))
    (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook 'reveal-post-command t)))

;;;###autoload
(define-minor-mode global-reveal-mode
  "Toggle Reveal mode in all buffers on or off.
Reveal mode renders invisible text around point visible again.

Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  :global t :group 'reveal
  (setq-default reveal-mode global-reveal-mode)
  (if global-reveal-mode
      (progn
	(setq search-invisible t)
	(add-hook 'post-command-hook 'reveal-post-command))
    (setq search-invisible 'open)	;FIXME
    (remove-hook 'post-command-hook 'reveal-post-command)))

(provide 'reveal)

;; arch-tag: 96ba0242-2274-4ed7-8e10-26bc0707b4d8
;;; reveal.el ends here
