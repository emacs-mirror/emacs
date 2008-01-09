;;; mb-depth.el --- Indicate minibuffer-depth in prompt
;;
;; Copyright (C) 2006, 2007, 2008  Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Defines the minor mode `minibuffer-indicate-depth-mode'.
;;
;; When active, any recursive use of the minibuffer will show
;; the recursion depth in the minibuffer prompt.  This is only
;; useful if `enable-recursive-minibuffers' is non-nil.

;;; Code:

(defvar minibuf-depth-indicator-function nil
  "If non-nil, function to set up the minibuffer depth indicator.
It is called with one argument, the minibuffer depth,
and must return a string.")

;; An overlay covering the prompt.  This is a buffer-local variable in
;; each affected minibuffer.
;;
(defvar minibuf-depth-overlay)
(make-variable-buffer-local 'minibuf-depth-overlay)

;; This function goes on minibuffer-setup-hook
(defun minibuf-depth-setup-minibuffer ()
  "Set up a minibuffer for `minibuffer-indicate-depth-mode'.
The prompt should already have been inserted."
  (when (> (minibuffer-depth) 1)
    (setq minibuf-depth-overlay (make-overlay (point-min) (1+ (point-min))))
    (overlay-put minibuf-depth-overlay 'before-string
		 (if minibuf-depth-indicator-function
		     (funcall minibuf-depth-indicator-function (minibuffer-depth))
		   (propertize (format "[%d]" (minibuffer-depth)) 'face 'highlight)))
    (overlay-put minibuf-depth-overlay 'evaporate t)))

;;;###autoload
(define-minor-mode minibuffer-indicate-depth-mode
  "Toggle Minibuffer Indicate Depth mode.
When active, any recursive use of the minibuffer will show
the recursion depth in the minibuffer prompt.  This is only
useful if `enable-recursive-minibuffers' is non-nil.

With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'minibuffer
  (if minibuffer-indicate-depth-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'minibuf-depth-setup-minibuffer)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'minibuf-depth-setup-minibuffer)))

(provide 'mb-depth)

;; arch-tag: 50224089-5bf5-46f8-803d-18f018c5eacf
;;; mb-depth.el ends here
