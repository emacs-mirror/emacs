;;; hl-line.el --- highlight the current line

;; Copyright (C) 1998, 2000, 2001 Free Software Foundation, Inc.

;; Author:  Dave Love <fx@gnu.org>
;; Created: 1998-09-13
;; Keywords: faces, frames, emulation

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides a minor mode (toggled by M-x hl-line-mode) and a global minor
;; mode (toggled by M-x global-hl-line-mode) to highlight, on a
;; suitable terminal, the line in the current window on which point is
;; (except in a minibuffer window).  Done to satisfy a request for a
;; feature of Lesser Editors.

;; You probably don't really want this; if the cursor is difficult to
;; spot, try changing its colour, relying on `blink-cursor-mode' or
;; both.  The hookery used might affect response noticeably on a slow
;; machine.  It may be useful in "non-text" buffers such as Gnus or
;; PCL-CVS though.

;; An overlay is used, active only on the selected window.  Hooks are
;; added to `pre-command-hook' and `post-command-hook' to activate and
;; deactivate (by deleting) the overlay.  `hl-line-unhighlight', on
;; `pre-command-hook', deactivates it unconditionally in case the
;; command changes the selected window.  (It does so rather than
;; keeping track of changes in the selected window).
;; `hl-line-highlight', on `post-command-hook', activates it again
;; across the window width.

;; You could make variable `hl-line-mode' buffer-local to avoid
;; highlighting specific buffers, when the global mode is used.

;;; Code:

(defgroup hl-line nil
  "Highlight the current line."
  :version "21.1"
  :group 'editing)

(defcustom hl-line-face 'highlight
  "Face with which to highlight the current line."
  :type 'face
  :group 'hl-line)

(defvar hl-line-overlay nil)

;;;###autoload
(define-minor-mode hl-line-mode
  "Minor mode to highlight the line about point in the current window.
With ARG, turn Hl-Line mode on if ARG is positive, off otherwise.
Uses functions `hl-line-unhighlight' and `hl-line-highlight' on
`pre-command-hook' and `post-command-hook'."
  nil nil nil
  (if hl-line-mode
      (progn
	(add-hook 'pre-command-hook #'hl-line-unhighlight)
	(add-hook 'post-command-hook #'hl-line-highlight))
    (hl-line-unhighlight)
    (remove-hook 'pre-command-hook #'hl-line-unhighlight)
    (remove-hook 'post-command-hook #'hl-line-highlight)))

;;;###autoload
(easy-mmode-define-global-mode
 global-hl-line-mode hl-line-mode hl-line-mode
 :group 'hl-line)

(defun hl-line-highlight ()
  "Active the Hl-Line overlay on the current line in the current window.
\(Unless it's a minibuffer window.)"
  (when hl-line-mode			; Could be made buffer-local.
    (unless (window-minibuffer-p (selected-window)) ; silly in minibuffer
      (unless hl-line-overlay
	(setq hl-line-overlay (make-overlay 1 1)) ; to be moved
	(overlay-put hl-line-overlay 'face hl-line-face))
      (overlay-put hl-line-overlay 'window (selected-window))
      (move-overlay hl-line-overlay
		    (line-beginning-position) (1+ (line-end-position))
		    (current-buffer)))))

(defun hl-line-unhighlight ()
  "Deactivate the Hl-Line overlay on the current line in the current window."
  (if hl-line-overlay
      (delete-overlay hl-line-overlay)))

(provide 'hl-line)

;;; hl-line.el ends here
