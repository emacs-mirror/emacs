;;; hl-line.el --- highlight the current line

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author:  Dave Love <fx@gnu.org>
;; Created: 1998-09-13
;; Keywords: faces, frames

;; Hl-Line mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Hl-Line mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides a global minor mode (toggled by M-x hl-line-mode) to
;; highlight, in a windowing system, the line on which point is
;; (except in a minibuffer window) to satisfy a request for a feature
;; of Lesser Editors.

;; You probably don't really want this; if the cursor is difficult to
;; spot, try changing its colour or using a cursor blinking
;; <URL:http://www.wonderworks.com/download/blinking-cursor.el.gz> or
;; jiggling <URL:http://www.eskimo.com/%7Eseldon> package.  (Cursor
;; blinking will be built in to Emacs 21.)  The hookery involved here
;; might slow Emacs noticeably on a slow machine.

;; An overlay is used, active only on the selected window.  Hooks are
;; added to `pre-command-hook' and `post-command-hook' to activate and
;; deactivate (by deleting) the overlay.  `hl-line-unhighlight', on
;; `pre-command-hook', deactivates it unconditionally in case the
;; command changes the selected window.  (It does so rather than
;; keeping track of changes in the selected window).
;; `hl-line-highlight', on `post-command-hook', activates it again
;; across the window width.

;;; Code:

(defgroup hl-line nil
  "Highliight the current line."
  :version "20.5"
  :group 'editing)

;;;###autoload
(defcustom hl-line-mode nil
  "Non-nil if Hl-Line mode is enabled."
  :set (lambda (symbol value)
	 (hl-line-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'hl-line
  :require 'hl-line)

(defcustom hl-line-face 'highlight
  "Face with which to highlight the current line."
  :type 'face
  :group 'hl-line)

(defvar hl-line-overlay nil)
(make-variable-buffer-local 'hl-line-overlay)

(defun hl-line-highlight ()
  "Active the Hl-Line overlay on the current line in the current window.
\(Unless it's a minibuffer window.)"
  (unless (window-minibuffer-p (selected-window)) ; silly in minibuffer
    (unless hl-line-overlay		; new overlay for this buffer
      (setq hl-line-overlay (make-overlay 1 1))	; to be moved
      (overlay-put hl-line-overlay 'face hl-line-face))
    (overlay-put hl-line-overlay 'window (selected-window))
    (move-overlay hl-line-overlay
		  (line-beginning-position) (1+ (line-end-position)))))

(defun hl-line-unhighlight ()
  "Deactivate the Hl-Line overlay on the current line in the current window."
  (if hl-line-overlay
      (delete-overlay hl-line-overlay)))

;;;###autoload
(defun hl-line-mode (&optional arg)
  "Global minor mode to highlight the line about point.

With ARG, turn Hl-Line mode on if ARG is positive, off otherwise.
Only useful with a windowing system.
Uses functions `hl-line-unhighlight' and `hl-line-highlight' on
`pre-command-hook' and `post-command-hook'."
  (interactive "P")
  (setq hl-line-mode (if (null arg)
			 (not hl-line-mode)
		       (> (prefix-numeric-value arg) 0)))
  (cond (hl-line-mode
	 (add-hook 'pre-command-hook #'hl-line-unhighlight)
	 (add-hook 'post-command-hook #'hl-line-highlight))
	(t
	 (hl-line-unhighlight)
	 (remove-hook 'pre-command-hook #'hl-line-unhighlight)
	 (remove-hook 'post-command-hook #'hl-line-highlight)))
  (if (interactive-p)
      (message "Hl-Line mode %sabled" (if hl-line-mode "en" "dis"))))

(provide 'hl-line)

;;; hl-line.el ends here
