;;; reveal.el --- Automatically reveal hidden text at point

;; Copyright (C) 2000, 2001  Free Software Foundation, Inc.

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
  :type 'boolean)

(defvar reveal-open-spots nil)
(make-variable-buffer-local 'reveal-open-spots)

;; Actual code

(defun reveal-post-command ()
  ;; Refresh the spots that might have changed.
  ;; `Refreshing' here means to try and re-hide the corresponding text.
  ;; We don't refresh everything correctly:
  ;; - we only refresh spots in the current window.
  ;; FIXME: do we actually know that (current-buffer) = (window-buffer) ?
  (with-local-quit
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
	  (repeat t)
	  ;; `post-command-hook' binds it to t, but the user might want to
	  ;; interrupt our work if we somehow get stuck in an infinite loop.
	  (inhibit-quit nil)
	  inv open)
     (setq reveal-open-spots (cdr spots))
     ;; Open new overlays.
     (while repeat
       (setq repeat nil)
       (dolist (ol (nconc (when (and reveal-around-mark mark-active)
			    (overlays-at (mark)))
			  (overlays-at (point))))
	 (push (cons (selected-window) ol) reveal-open-spots)
	 (setq old-ols (delq ol old-ols))
	 (when (setq inv (overlay-get ol 'invisible))
	   (when (or (overlay-get ol 'isearch-open-invisible)
		     (and (consp buffer-invisibility-spec)
			  (assq inv buffer-invisibility-spec)))
	     (overlay-put ol 'reveal-invisible inv)
	     (overlay-put ol 'invisible nil)
	     (when (setq open (get inv 'reveal-toggle-invisible))
	       ;; Use the provided opening function and repeat (since the
	       ;; opening function might have hidden a subpart around point).
	       (setq repeat t)
	       (condition-case err
		   (funcall open ol t)
		 (error (message "!!Reveal-show: %s !!" err))))))))
     ;; Close old overlays.
     (dolist (ol old-ols)
       (when (and (setq inv (overlay-get ol 'reveal-invisible))
		  (eq (current-buffer) (overlay-buffer ol))
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
	   (overlay-put ol 'invisible inv)
	   (when (setq open (get inv 'reveal-toggle-invisible))
	     (condition-case err
		 (funcall open ol nil)
	       (error (message "!!Reveal-hide: %s !!" err))))))))))

;;;###autoload
(define-minor-mode reveal-mode
  "Toggle Reveal mode on or off.
Reveal mode renders invisible text around point visible again.

Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  :lighter (global-reveal-mode nil " Reveal")
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
  :global t
  (setq-default reveal-mode global-reveal-mode)
  (if global-reveal-mode
      (progn
	(setq search-invisible t)
	(add-hook 'post-command-hook 'reveal-post-command))
    (setq search-invisible 'open)	;FIXME
    (remove-hook 'post-command-hook 'reveal-post-command)))

(provide 'reveal)
;;; reveal.el ends here
