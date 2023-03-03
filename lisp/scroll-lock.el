;;; scroll-lock.el --- Scroll lock scrolling.  -*- lexical-binding:t -*-

;; Copyright (C) 2005-2023 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: emacs-devel@gnu.org
;; Created: 2005-06-18

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; By activating Scroll Lock mode, keys for moving point by line or
;; paragraph will scroll the buffer by the respective amount of lines
;; instead.  Point will be kept vertically fixed relative to window
;; boundaries.

;;; Code:

(defvar-keymap scroll-lock-mode-map
  :doc "Keymap for Scroll Lock mode."
  "<remap> <next-line>"          #'scroll-lock-next-line
  "<remap> <previous-line>"      #'scroll-lock-previous-line
  "<remap> <forward-paragraph>"  #'scroll-lock-forward-paragraph
  "<remap> <backward-paragraph>" #'scroll-lock-backward-paragraph
  "S-<down>"                     #'scroll-lock-next-line-always-scroll)

(defvar-local scroll-lock-preserve-screen-pos-save scroll-preserve-screen-position
  "Used for saving the state of `scroll-preserve-screen-position'.")

(defvar scroll-lock-temporary-goal-column 0
  "Like `temporary-goal-column' but for scroll-lock-* commands.")

;;;###autoload
(define-minor-mode scroll-lock-mode
  "Buffer-local minor mode for pager-like scrolling.

When enabled, keys that normally move point by line or paragraph
will scroll the buffer by the respective amount of lines instead
and point will be kept vertically fixed relative to window
boundaries during scrolling.

Note that the default key binding to `scroll' will not work on
MS-Windows systems if `w32-scroll-lock-modifier' is non-nil."
  :lighter " ScrLck"
  :keymap scroll-lock-mode-map
  (if scroll-lock-mode
      (progn
	(setq scroll-lock-preserve-screen-pos-save
	      scroll-preserve-screen-position)
        (setq-local scroll-preserve-screen-position 'always))
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

(defun scroll-lock-next-line-always-scroll (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (condition-case nil
      (scroll-up arg)
    (end-of-buffer (goto-char (point-max)) (recenter 1)))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun scroll-lock-next-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (if (pos-visible-in-window-p (point-max))
      (forward-line arg)
    (scroll-up arg))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun scroll-lock-previous-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (condition-case nil
      (scroll-down arg)
    (beginning-of-buffer (forward-line (- arg))))
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
