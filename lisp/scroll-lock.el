;;; scroll-lock.el --- Scroll lock scrolling.

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: FSF
;; Created: 2005-06-18

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; By activating Scroll Lock mode, keys for moving point by line or
;; paragraph will scroll the buffer by the respective amount of lines
;; instead.  Point will be kept vertically fixed relative to window
;; boundaries.

;;; Code:

(defvar scroll-lock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] 'scroll-lock-next-line)
    (define-key map [remap previous-line] 'scroll-lock-previous-line)
    (define-key map [remap forward-paragraph] 'scroll-lock-forward-paragraph)
    (define-key map [remap backward-paragraph] 'scroll-lock-backward-paragraph)
    map)
  "Keymap for Scroll Lock mode.")

(defvar scroll-lock-preserve-screen-pos-save scroll-preserve-screen-position
  "Used for saving the state of `scroll-preserve-screen-position'.")
(make-variable-buffer-local 'scroll-lock-preserve-screen-pos-save)

(defvar scroll-lock-temporary-goal-column 0
  "Like `temporary-goal-column' but for scroll-lock-* commands.")

;;;###autoload
(define-minor-mode scroll-lock-mode
  "Minor mode for pager-like scrolling.
Keys which normally move point by line or paragraph will scroll
the buffer by the respective amount of lines instead and point
will be kept vertically fixed relative to window boundaries
during scrolling."
  :lighter " ScrLck"
  :keymap scroll-lock-mode-map
  (if scroll-lock-mode
      (progn
	(setq scroll-lock-preserve-screen-pos-save
	      scroll-preserve-screen-position)
	(set (make-local-variable 'scroll-preserve-screen-position) 'always))
    (setq scroll-preserve-screen-position
	  scroll-lock-preserve-screen-pos-save)))

(defun scroll-lock-update-goal-column ()
  "Update `scroll-lock-temporary-goal-column' if necessary."
  (unless (memq last-command '(scroll-lock-next-line
			       scroll-lock-previous-line
			       scroll-lock-forward-paragraph
			       scroll-lock-backward-paragraph))
    (setq scroll-lock-temporary-goal-column (current-column))))

(defun scroll-lock-move-to-column (column)
  "Like `move-to-column' but cater for wrapped lines."
  (if (or (bolp)
	  ;; Start of a screen line.
	  (not (zerop (mod (- (point) (line-beginning-position))
			   (window-width)))))
      (move-to-column column)
    (forward-char (min column (- (line-end-position) (point))))))

(defun scroll-lock-next-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (if (pos-visible-in-window-p (point-max))
      (next-line arg)
    (scroll-up arg))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun scroll-lock-previous-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (condition-case nil
      (scroll-down arg)
    (beginning-of-buffer (previous-line arg)))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun scroll-lock-forward-paragraph (&optional arg)
  "Scroll down ARG paragraphs keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (scroll-up (count-screen-lines (point) (save-excursion
					   (forward-paragraph arg)
					   (point))))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun scroll-lock-backward-paragraph (&optional arg)
  "Scroll up ARG paragraphs keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (let ((goal (save-excursion (backward-paragraph arg) (point))))
    (condition-case nil
	(scroll-down (count-screen-lines goal (point)))
      (beginning-of-buffer (goto-char goal))))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(provide 'scroll-lock)

;;; scroll-lock.el ends here
