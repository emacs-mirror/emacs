;;; tpu-extras.el --- Scroll margins and free cursor mode for TPU-edt

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: emulations

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

;;  The functions contained in this file implement scroll margins and free
;;  cursor mode.  The following keys and commands are affected.

;;       key/command   function                        scroll   cursor

;;       Up-Arrow      previous line                     x        x
;;       Down-Arrow    next line                         x        x
;;       Right-Arrow   next character                             x
;;       Left-Arrow    previous character                         x
;;       KP0           next or previous line             x
;;       KP7           next or previous page             x
;;       KP8           next or previous screen           x
;;       KP2           next or previous end-of-line      x        x
;;       Control-e     current end-of-line                        x
;;       Control-h     previous beginning-of-line        x
;;       Next Scr      next screen                       x
;;       Prev Scr      previous screen                   x
;;       Search        find a string                     x
;;       Replace       find and replace a string         x
;;       Newline       insert a newline                  x
;;       Paragraph     next or previous paragraph        x
;;       Auto-Fill     break lines on spaces             x

;;  These functions are not part of the base TPU-edt for the following
;;  reasons:

;;  Free cursor mode is implemented with the emacs picture-mode functions.
;;  These functions support moving the cursor all over the screen, however,
;;  when the cursor is moved past the end of a line, spaces or tabs are
;;  appended to the line - even if no text is entered in that area.  In
;;  order for a free cursor mode to work exactly like TPU/edt, this trailing
;;  whitespace needs to be dealt with in every function that might encounter
;;  it.  Such global changes are impractical, however, free cursor mode is
;;  too valuable to abandon completely, so it has been implemented in those
;;  functions where it serves best.

;;  The implementation of scroll margins adds overhead to previously
;;  simple and often used commands.  These commands are now responsible
;;  for their normal operation and part of the display function.  There
;;  is a possibility that this display overhead could adversely affect the
;;  performance of TPU-edt on slower computers.  In order to support the
;;  widest range of computers, scroll margin support is optional.

;;  I don't know for a fact that the overhead associated with scroll
;;  margin support is significant.  If you find that it is, please send me
;;  a note describing the extent of the performance degradation.  Be sure
;;  to include a description of the platform where you're running TPU-edt.
;;  Send your note to the address provided by Gold-V.

;;  Even with these differences and limitations, these functions implement
;;  important aspects of the real TPU/edt.  Those who miss free cursor mode
;;  and/or scroll margins will appreciate these implementations.

;;; Usage:

;;  To use this file, simply load it after loading TPU-edt.  After that,
;;  customize TPU-edt to your tastes by setting scroll margins and/or
;;  turning on free cursor mode.  Here's an example for your .emacs file.

;;     (load "tpu-edt")                      ; Load the base TPU-edt
;;     (load "tpu-extras")                   ;   and the extras.
;;     (tpu-set-scroll-margins "10%" "15%")  ; Set scroll margins.

;;  Once the extras are loaded, scroll margins and cursor binding can be
;;  changed with the following commands:

;;     tpu-set-scroll-margins  or   set scroll margins
;;     tpu-set-cursor-bound    or   set cursor bound
;;     tpu-set-cursor-free     or   set cursor free

;;  Additionally, Gold-F toggles between bound and free cursor modes.

;;  Note that switching out of free cursor mode or exiting TPU-edt while in
;;  free cursor mode strips trailing whitespace from every line in the file.

;;; Code:


;;;  Gotta have tpu-edt

(require 'tpu-edt)


;;;  Customization variables

(defconst tpu-top-scroll-margin 0
  "*Scroll margin at the top of the screen.
Interpreted as a percent of the current window size.")
(defconst tpu-bottom-scroll-margin 0
  "*Scroll margin at the bottom of the screen.
Interpreted as a percent of the current window size.")

(defvar tpu-backward-char-like-tpu t
  "*If non-nil, in free cursor mode backward-char (left-arrow) works
just like TPU/edt.  Otherwise, backward-char will move to the end of
the previous line when starting from a line beginning.")


;;;  Global variables

(defvar tpu-cursor-free nil
  "If non-nil, let the cursor roam free.")


;;;  Hooks  --  Set cursor free in picture mode.
;;;             Clean up when writing a file from cursor free mode.

(add-hook 'picture-mode-hook 'tpu-set-cursor-free)

(defun tpu-write-file-hook nil
  "Eliminate whitespace at ends of lines, if the cursor is free."
  (if (and (buffer-modified-p) tpu-cursor-free) (picture-clean)))

(or (memq 'tpu-write-file-hook write-file-hooks)
    (setq write-file-hooks
	  (cons 'tpu-write-file-hook write-file-hooks)))


;;;  Utility routines for implementing scroll margins

(defun tpu-top-check (beg lines)
  "Enforce scroll margin at the top of screen."
  (let ((margin	 (/ (* (window-height) tpu-top-scroll-margin) 100)))
    (cond ((< beg margin) (recenter beg))
	  ((< (- beg lines) margin) (recenter margin)))))

(defun tpu-bottom-check (beg lines)
  "Enforce scroll margin at the bottom of screen."
  (let* ((height (window-height))
	 (margin (+ 1 (/ (* height tpu-bottom-scroll-margin) 100)))
	 ;; subtract 1 from height because it includes mode line
	 (difference (- height margin 1)))
    (cond ((> beg difference) (recenter beg))
	  ((> (+ beg lines) difference) (recenter (- margin))))))


;;;  Movement by character

(defun tpu-forward-char (num)
  "Move right ARG characters (left if ARG is negative)."
  (interactive "p")
  (if tpu-cursor-free (picture-forward-column num) (forward-char num)))

(defun tpu-backward-char (num)
  "Move left ARG characters (right if ARG is negative)."
  (interactive "p")
  (cond ((not tpu-cursor-free)
	 (backward-char num))
	(tpu-backward-char-like-tpu
	 (picture-backward-column num))
	((bolp)
	 (backward-char 1)
	 (picture-end-of-line)
	 (picture-backward-column (1- num)))
	(t
	 (picture-backward-column num))))


;;;  Movement by line

(defun tpu-next-line (num)
  "Move to next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (if tpu-cursor-free (or (eobp) (picture-move-down num))
      (next-line-internal num))
    (tpu-bottom-check beg num)
    (setq this-command 'next-line)))

(defun tpu-previous-line (num)
  "Move to previous line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (if tpu-cursor-free (picture-move-up num) (next-line-internal (- num)))
    (tpu-top-check beg num)
    (setq this-command 'previous-line)))

(defun tpu-next-beginning-of-line (num)
  "Move to beginning of line; if at beginning, move to beginning of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (backward-char 1)
    (forward-line (- 1 num))
    (tpu-top-check beg num)))

(defun tpu-next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (cond (tpu-cursor-free
	   (let ((beg (point)))
	     (if (< 1 num) (forward-line num))
	     (picture-end-of-line)
	     (if (<= (point) beg) (progn (forward-line) (picture-end-of-line)))))
	  (t
	   (forward-char)
	   (end-of-line num)))
    (tpu-bottom-check beg num)))

(defun tpu-previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (cond (tpu-cursor-free
	   (picture-end-of-line (- 1 num)))
	  (t
	   (end-of-line (- 1 num))))
    (tpu-top-check beg num)))

(defun tpu-current-end-of-line nil
  "Move point to end of current line."
  (interactive)
  (let ((beg (point)))
    (if tpu-cursor-free (picture-end-of-line) (end-of-line))
    (if (= beg (point)) (message "You are already at the end of a line."))))

(defun tpu-forward-line (num)
  "Move to beginning of next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (next-line-internal num)
    (tpu-bottom-check beg num)
    (beginning-of-line)))

(defun tpu-backward-line (num)
  "Move to beginning of previous line.
Prefix argument serves as repeat count."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (next-line-internal (- num))
    (tpu-top-check beg num)
    (beginning-of-line)))


;;;  Movement by paragraph

(defun tpu-paragraph (num)
  "Move to the next paragraph in the current direction.
A repeat count means move that many paragraphs."
  (interactive "p")
  (let* ((left nil)
	 (beg (tpu-current-line))
	 (height (window-height))
	 (top-percent
	  (if (= 0 tpu-top-scroll-margin) 10 tpu-top-scroll-margin))
	 (bottom-percent
	  (if (= 0 tpu-bottom-scroll-margin) 15 tpu-bottom-scroll-margin))
	 (top-margin (/ (* height top-percent) 100))
	 (bottom-up-margin (+ 1 (/ (* height bottom-percent) 100)))
	 (bottom-margin (max beg (- height bottom-up-margin 1)))
	 (top (save-excursion (move-to-window-line top-margin) (point)))
	 (bottom (save-excursion (move-to-window-line bottom-margin) (point)))
	 (far (save-excursion
		(goto-char bottom) (forward-line (- height 2)) (point))))
    (cond (tpu-advance
	   (tpu-next-paragraph num)
	   (cond((> (point) far)
		 (setq left (save-excursion (forward-line height)))
		 (if (= 0 left) (recenter top-margin)
		   (recenter (- left bottom-up-margin))))
		(t
		 (and (> (point) bottom) (recenter bottom-margin)))))
	  (t
	   (tpu-previous-paragraph num)
	   (and (< (point) top) (recenter (min beg top-margin)))))))


;;;  Movement by page

(defun tpu-page (num)
  "Move to the next page in the current direction.
A repeat count means move that many pages."
  (interactive "p")
  (let* ((left nil)
	 (beg (tpu-current-line))
	 (height (window-height))
	 (top-percent
	  (if (= 0 tpu-top-scroll-margin) 10 tpu-top-scroll-margin))
	 (bottom-percent
	  (if (= 0 tpu-bottom-scroll-margin) 15 tpu-bottom-scroll-margin))
	 (top-margin (/ (* height top-percent) 100))
	 (bottom-up-margin (+ 1 (/ (* height bottom-percent) 100)))
	 (bottom-margin (max beg (- height bottom-up-margin 1)))
	 (top (save-excursion (move-to-window-line top-margin) (point)))
	 (bottom (save-excursion (move-to-window-line bottom-margin) (point)))
	 (far (save-excursion
		(goto-char bottom) (forward-line (- height 2)) (point))))
    (cond (tpu-advance
	   (forward-page num)
	   (cond((> (point) far)
		 (setq left (save-excursion (forward-line height)))
		 (if (= 0 left) (recenter top-margin)
		   (recenter (- left bottom-up-margin))))
		(t
		 (and (> (point) bottom) (recenter bottom-margin)))))
	  (t
	   (backward-page num)
	   (and (< (point) top) (recenter (min beg top-margin)))))))


;;;  Scrolling

(defun tpu-scroll-window-down (num)
  "Scroll the display down to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (tpu-current-line))
	 (height (1- (window-height)))
	 (lines (* num (/ (* height tpu-percent-scroll) 100))))
    (next-line-internal (- lines))
    (tpu-top-check beg lines)))

(defun tpu-scroll-window-up (num)
  "Scroll the display up to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (tpu-current-line))
	 (height (1- (window-height)))
	 (lines (* num (/ (* height tpu-percent-scroll) 100))))
    (next-line-internal lines)
    (tpu-bottom-check beg lines)))


;;;  Replace the TPU-edt internal search function

(defun tpu-search-internal (pat &optional quiet)
  "Search for a string or regular expression."
  (let* ((left nil)
	 (beg (tpu-current-line))
	 (height (window-height))
	 (top-percent
	  (if (= 0 tpu-top-scroll-margin) 10 tpu-top-scroll-margin))
	 (bottom-percent
	  (if (= 0 tpu-bottom-scroll-margin) 15 tpu-bottom-scroll-margin))
	 (top-margin (/ (* height top-percent) 100))
	 (bottom-up-margin (+ 1 (/ (* height bottom-percent) 100)))
	 (bottom-margin (max beg (- height bottom-up-margin 1)))
	 (top (save-excursion (move-to-window-line top-margin) (point)))
	 (bottom (save-excursion (move-to-window-line bottom-margin) (point)))
	 (far (save-excursion
		(goto-char bottom) (forward-line (- height 2)) (point))))
    (tpu-search-internal-core pat quiet)
    (if tpu-searching-forward
	(cond((> (point) far)
	      (setq left (save-excursion (forward-line height)))
	      (if (= 0 left) (recenter top-margin)
		(recenter (- left bottom-up-margin))))
	     (t
	      (and (> (point) bottom) (recenter bottom-margin))))
      (and (< (point) top) (recenter (min beg top-margin))))))



;;;  Replace the newline, newline-and-indent, and do-auto-fill functions

(or (fboundp 'tpu-old-newline)
    (fset 'tpu-old-newline (symbol-function 'newline)))
(or (fboundp 'tpu-old-do-auto-fill)
    (fset 'tpu-old-do-auto-fill (symbol-function 'do-auto-fill)))
(or (fboundp 'tpu-old-newline-and-indent)
    (fset 'tpu-old-newline-and-indent (symbol-function 'newline-and-indent)))

(defun newline (&optional num)
  "Insert a newline.  With arg, insert that many newlines.
In Auto Fill mode, can break the preceding line if no numeric arg.
This is the TPU-edt version that respects the bottom scroll margin."
  (interactive "p")
  (let ((beg (tpu-current-line)))
    (or num (setq num 1))
    (tpu-old-newline num)
    (tpu-bottom-check beg num)))

(defun newline-and-indent nil
  "Insert a newline, then indent according to major mode.
Indentation is done using the current indent-line-function.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents
to the specified left-margin column.  This is the TPU-edt
version that respects the bottom scroll margin."
  (interactive)
  (let ((beg (tpu-current-line)))
    (tpu-old-newline-and-indent)
    (tpu-bottom-check beg 1)))

(defun do-auto-fill nil
  "TPU-edt version that respects the bottom scroll margin."
  (let ((beg (tpu-current-line)))
    (tpu-old-do-auto-fill)
    (tpu-bottom-check beg 1)))


;;;  Function to set scroll margins

;;;###autoload
(defun tpu-set-scroll-margins (top bottom)
  "Set scroll margins."
  (interactive
   "sEnter top scroll margin (N lines or N%% or RETURN for current value): \
\nsEnter bottom scroll margin (N lines or N%% or RETURN for current value): ")
  ;; set top scroll margin
  (or (string= top "")
      (if (string= "%" (substring top -1))
	  (setq tpu-top-scroll-margin (string-to-int top))
	(setq tpu-top-scroll-margin
	      (/ (1- (+ (* (string-to-int top) 100) (window-height)))
		 (window-height)))))
  ;; set bottom scroll margin
  (or (string= bottom "")
      (if (string= "%" (substring bottom -1))
	  (setq tpu-bottom-scroll-margin (string-to-int bottom))
	(setq tpu-bottom-scroll-margin
	      (/ (1- (+ (* (string-to-int bottom) 100) (window-height)))
		 (window-height)))))
  ;; report scroll margin settings if running interactively
  (and (interactive-p)
       (message "Scroll margins set.  Top = %s%%, Bottom = %s%%"
		tpu-top-scroll-margin tpu-bottom-scroll-margin)))


;;;  Functions to set cursor bound or free

;;;###autoload
(defun tpu-set-cursor-free nil
  "Allow the cursor to move freely about the screen."
  (interactive)
  (setq tpu-cursor-free t)
  (substitute-key-definition 'tpu-set-cursor-free
			     'tpu-set-cursor-bound
			     GOLD-map)
  (message "The cursor will now move freely about the screen."))

;;;###autoload
(defun tpu-set-cursor-bound nil
  "Constrain the cursor to the flow of the text."
  (interactive)
  (picture-clean)
  (setq tpu-cursor-free nil)
  (substitute-key-definition 'tpu-set-cursor-bound
			     'tpu-set-cursor-free
			     GOLD-map)
  (message "The cursor is now bound to the flow of your text."))

;;; tpu-extras.el ends here
