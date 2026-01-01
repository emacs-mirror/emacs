;;; tty-tip.el --- Display help in kind of tooltips on ttys  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

;; This uses tty child frames to display help which looks and feels much
;; like using tooltips (but they really aren't).

;; Use `tty-tip-mode' to activate or toggle this feature.
;;
;; You can customize face `tooltip', `tooltip-short-delay',
;; `tooltip-delay', `tooltip-recent-seconds'.

;;; Code:

(require 'tooltip)

(defvar tty-tip--frame nil)

(defun tty-tip--make-buffer (text)
  (with-current-buffer
      (get-buffer-create " *tty-tip*")
    ;; Redirect focus to parent.
    (add-hook 'pre-command-hook #'tty-tip--delete-frame nil t)
    ;; Use an empty keymap.
    (use-local-map (make-keymap))
    (dolist (var '((mode-line-format . nil)
                   (header-line-format . nil)
                   (tab-line-format . nil)
                   (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
                   (frame-title-format . "")
                   (truncate-lines . t)
                   (cursor-in-non-selected-windows . nil)
                   (cursor-type . nil)
                   (show-trailing-whitespace . nil)
                   (display-line-numbers . nil)
                   (left-fringe-width . nil)
                   (right-fringe-width . nil)
                   (left-margin-width . 0)
                   (right-margin-width . 0)
                   (fringes-outside-margins . 0)
                   (buffer-read-only . t)))
      (set (make-local-variable (car var)) (cdr var)))
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (current-buffer)))

(defvar tty-tip-frame-parameters
  `((visibility . nil)
    (background-color . "lightyellow")
    (foreground-color . "black")
    (width . 0) (height . 0)
    (min-width . t) (min-height . t)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (no-other-window . t)
    (no-delete-other-windows . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)))

(defun tty-tip--frame-parameters ()
  (let ((params (copy-sequence tty-tip-frame-parameters))
        (fg (face-attribute 'tooltip :foreground))
        (bg (face-attribute 'tooltip :background)))
    (when (stringp fg)
      (setf (alist-get 'foreground-color params) fg))
    (when (stringp bg)
      (setf (alist-get 'background-color params) bg))
    params))

(defvar tty-tip--help-message nil)
(defvar tty-tip--hide-time nil)
(defvar tty-tip--show-timer nil)
(defvar tty-tip--hide-timer nil)

(defun tty-tip--delete-frame ()
  (when tty-tip--frame
    (when tty-tip--hide-timer
      (cancel-timer tty-tip--hide-timer)
      (setq tty-tip--hide-timer nil))
    (delete-frame tty-tip--frame)
    (setq tty-tip--frame nil)
    t))

(defun tty-tip--compute-position ()
  (let* ((pos (mouse-position))
         (mouse-x (car (cdr pos)))
         (mouse-y (cdr (cdr pos)))
	 (x (+ mouse-x 1))
	 (y (+ mouse-y 1))
	 (tip-width (frame-width tty-tip--frame))
	 (tip-height (frame-height tty-tip--frame))
	 (tty-width (display-pixel-width))
	 (tty-height (display-pixel-height)))
    (when (> (+ x tip-width) tty-width)
      (setq x (max 0 (- x tip-width 1))))
    (when (> (+ y tip-height) tty-height)
      (setq y (max 0 (- y tip-height 1))))
    (cons x y)))

(defun tty-tip--create-frame (text)
  (let* ((minibuffer (minibuffer-window (window-frame)))
         (buffer (tty-tip--make-buffer text))
         (window-min-height 1)
         (window-min-width 1)
         after-make-frame-functions
	 (text-lines (string-lines text)))
    (setq tty-tip--frame
          (make-frame
           `((parent-frame . ,(car (mouse-position)))
             (minibuffer . ,minibuffer)
             ,@(tty-tip--frame-parameters))))
    (let ((win (frame-root-window tty-tip--frame)))
      (set-window-buffer win buffer)
      (set-window-dedicated-p win t)
      (set-frame-size tty-tip--frame
                      (apply #'max (mapcar #'string-width text-lines))
                      (length text-lines))
      (let* ((pos (tty-tip--compute-position))
             (x (car pos))
             (y (cdr pos)))
	(set-frame-position tty-tip--frame x y))
      (make-frame-visible tty-tip--frame)
      (setq tty-tip--hide-timer
            (run-with-timer tooltip-hide-delay nil
                            #'tty-tip--delete-frame)))))

(defun tty-tip--delay ()
  (if (and tty-tip--hide-time
	   (time-less-p (time-since tty-tip--hide-time)
			tooltip-recent-seconds))
      tooltip-short-delay
    tooltip-delay))

(defun tty-tip--cancel-delayed-tip ()
  (when tty-tip--show-timer
    (cancel-timer tty-tip--show-timer)
    (setq tty-tip--show-timer nil)))

(defun tty-tip--start-delayed-tip ()
  (setq tty-tip--show-timer
        (run-with-timer (tty-tip--delay) nil
                        (lambda ()
                          (tty-tip--create-frame
                           tty-tip--help-message)))))

(defun tty-tip--hide (&optional _ignored-arg)
  (tty-tip--cancel-delayed-tip)
  (when (tty-tip--delete-frame)
    (setq tty-tip--hide-time (float-time))))

(defun tty-tip--show-help (msg)
  (let ((previous-help tty-tip--help-message))
    (setq tty-tip--help-message msg)
    (cond ((null msg)
	   (tty-tip--hide))
	  ((equal previous-help msg)
	   nil)
	  (t
	   (tty-tip--hide)
	   (tty-tip--start-delayed-tip)))))

;;;###autoload
(define-minor-mode tty-tip-mode
  "Global minor mode for displaying help in tty child frames."
  :global t :group 'help
  (unless (display-graphic-p)
    (cond (tty-tip-mode
	   (setq show-help-function #'tty-tip--show-help)
           (add-hook 'pre-command-hook #'tty-tip--hide))
          (t
           (setq show-help-function nil)
           (remove-hook 'pre-command-hook #'tty-tip--hide)))))

(provide 'tty-tip)

;;; tty-tip.el ends here
