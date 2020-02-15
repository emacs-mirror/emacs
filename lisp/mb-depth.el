;;; mb-depth.el --- Indicate minibuffer-depth in prompt -*- lexical-binding: t -*-
;;
;; Copyright (C) 2006-2020 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: convenience

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
;;
;; Defines the minor mode `minibuffer-depth-indicate-mode'.
;;
;; When active, any recursive use of the minibuffer will show
;; the recursion depth in the minibuffer prompt.  This is only
;; useful if `enable-recursive-minibuffers' is non-nil.

;;; Code:

(defvar minibuffer-depth-indicator-function nil
  "If non-nil, function to set up the minibuffer depth indicator.
It is called with one argument, the minibuffer depth,
and must return a string.")

;; An overlay covering the prompt.  This is a buffer-local variable in
;; each affected minibuffer.
;;
(defvar minibuffer-depth-overlay)
(make-variable-buffer-local 'minibuffer-depth-overlay)

;; This function goes on minibuffer-setup-hook
(defun minibuffer-depth-setup ()
  "Set up a minibuffer for `minibuffer-depth-indicate-mode'.
The prompt should already have been inserted."
  (let ((depth (minibuffer-depth)))
    (when (> depth 1)
      (let ((pos (point-min)))
        (setq minibuffer-depth-overlay (make-overlay pos (1+ pos))))
      (overlay-put minibuffer-depth-overlay 'before-string
                   (if minibuffer-depth-indicator-function
                       (funcall minibuffer-depth-indicator-function depth)
                     (propertize (format "[%d]" depth) 'face 'highlight)))
      (overlay-put minibuffer-depth-overlay 'evaporate t))))

;;;###autoload
(define-minor-mode minibuffer-depth-indicate-mode
  "Toggle Minibuffer Depth Indication mode.

Minibuffer Depth Indication mode is a global minor mode.  When
enabled, any recursive use of the minibuffer will show the
recursion depth in the minibuffer prompt.  This is only useful if
`enable-recursive-minibuffers' is non-nil."
  :global t
  :group 'minibuffer
  (if minibuffer-depth-indicate-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'minibuffer-depth-setup)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'minibuffer-depth-setup)))

(provide 'mb-depth)

;;; mb-depth.el ends here
