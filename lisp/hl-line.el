;;; hl-line.el --- highlight the current line  -*- lexical-binding:t -*-

;; Copyright (C) 1998, 2000-2022 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Created: 1998-09-13
;; Keywords: faces, frames, emulations

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

;;; Code:

(make-obsolete-variable 'hl-line-overlay nil "29.1")
(make-obsolete-variable 'global-hl-line-overlay nil "29.1")
(make-obsolete-variable 'global-hl-line-overlays nil "29.1")
(make-obsolete-variable 'global-hl-line-sticky-flag nil "29.1")
(make-obsolete-variable 'hl-line-overlay-buffer nil "29.1")
(make-obsolete-variable 'hl-line-range-function nil "29.1")

(defvar-local hl-line--overlay nil
  "Keep state else scan entire buffer in `post-command-hook'.")

;; 1. define-minor-mode creates buffer-local hl-line--overlay
;; 2. overlay wiped by kill-all-local-variables
;; 3. post-command-hook dupes overlay
;; Solution: prevent step 2.
(put 'hl-line--overlay 'permanent-local t)

(defgroup hl-line nil
  "Highlight the current line."
  :version "21.1"
  :group 'convenience)

(defface hl-line '((t :inherit highlight :extend t))
  "Default face for highlighting the current line in hl-line-mode."
  :version "22.1"
  :group 'hl-line)

(defcustom hl-line-face 'hl-line
  "Face with which to highlight the current line in Hl-Line mode."
  :type 'face
  :group 'hl-line
  :set (lambda (symbol value)
	 (set symbol value)
	 (dolist (buffer (buffer-list))
	   (with-current-buffer buffer
	     (when hl-line--overlay
	       (overlay-put hl-line--overlay 'face hl-line-face))))))

(defcustom hl-line-sticky-flag t
  "Non-nil to preserve highlighting overlay when focus leaves window."
  :type 'boolean
  :version "22.1"
  :group 'hl-line
  :set (lambda (symbol value)
         (set-default symbol value)
         (unless value
           (let ((selected (window-buffer (selected-window))))
             (dolist (buffer (buffer-list))
               (unless (eq buffer selected)
                 (with-current-buffer buffer
                   (hl-line-unhighlight))))))))

(defcustom hl-line-overlay-priority -50
  "Priority used on the overlay used by hl-line."
  :type 'integer
  :version "22.1"
  :group 'hl-line)

(defcustom hl-line-highlight-hook nil
  "After hook for `hl-line-highlight'.
Currently used in calendar/todo-mode."
  :type 'hook
  :group 'hl-line)

;;;###autoload
(define-minor-mode hl-line-mode
  "Toggle highlighting of the current line."
  :group 'hl-line
  (if hl-line-mode
      (progn
        (hl-line-highlight)
        (add-hook 'change-major-mode-hook #'hl-line-unhighlight nil t)
	(add-hook 'post-command-hook #'hl-line-highlight nil t))
    (remove-hook 'post-command-hook #'hl-line-highlight t)
    (remove-hook 'change-major-mode-hook #'hl-line-unhighlight t)
    (let (hl-line-sticky-flag)
      (hl-line-unhighlight))))

(defun hl-line-unhighlight ()
  (unless hl-line-sticky-flag
    (when hl-line--overlay
      (delete-overlay hl-line--overlay)
      (setq hl-line--overlay nil))))

(defun hl-line-highlight ()
  (unless (minibufferp)
    (unless hl-line--overlay
      (setq hl-line--overlay
            (let ((ol (make-overlay (point) (point))))
              (prog1 ol
                (overlay-put ol 'priority hl-line-overlay-priority)
                (overlay-put ol 'face hl-line-face)))))
    (move-overlay hl-line--overlay
                  (line-beginning-position)
                  (line-beginning-position 2))
    (run-hooks 'hl-line-highlight-hook)))

(defun hl-line-turn-on ()
  (unless (minibufferp)
    (let (inhibit-quit)
      (hl-line-mode 1))))

(defun hl-line-unload-function ()
  "Unload the Hl-Line library."
  (global-hl-line-mode -1)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when hl-line-mode (hl-line-mode -1))))
  ;; continue standard unloading
  nil)

;;;###autoload
(define-globalized-minor-mode global-hl-line-mode
  hl-line-mode hl-line-turn-on
  :group 'hl-line
  :version "29.1")

(provide 'hl-line)

;;; hl-line.el ends here
