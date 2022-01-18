;;; info-edit.el --- Editing info files  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1986, 1992-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: help
;; Obsolete-since: 24.4

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

(require 'info)

(defvar Info-edit-mode-hook nil
  "Hook run when `Info-edit-mode' is activated.")

(make-obsolete-variable 'Info-edit-mode-hook
			"editing Info nodes by hand is not recommended." "24.4")

(define-obsolete-variable-alias 'Info-edit-map 'Info-edit-mode-map "24.1")
(defvar Info-edit-mode-map (let ((map (make-sparse-keymap)))
                             (set-keymap-parent map text-mode-map)
                             (define-key map "\C-c\C-c" #'Info-cease-edit)
                             map)
  "Local keymap used within `e' command of Info.")

(make-obsolete-variable 'Info-edit-mode-map
			"editing Info nodes by hand is not recommended."
			"24.4")

;; Info-edit mode is suitable only for specially formatted data.
(put 'Info-edit-mode 'mode-class 'special)

(define-derived-mode Info-edit-mode text-mode "Info Edit"
  "Major mode for editing the contents of an Info node.
Like text mode with the addition of `Info-cease-edit'
which returns to Info mode for browsing."
  (setq buffer-read-only nil)
  (force-mode-line-update)
  (buffer-enable-undo (current-buffer)))

(defun Info-edit ()
  "Edit the contents of this Info node."
  (interactive)
  (Info-edit-mode)
  (message "%s" (substitute-command-keys
		 "Editing: Type \\<Info-edit-mode-map>\\[Info-cease-edit] to return to info")))

(put 'Info-edit 'disabled "Editing Info nodes by hand is not recommended.
This feature will be removed in future.")

(defun Info-cease-edit ()
  "Finish editing Info node; switch back to Info proper."
  (interactive)
  ;; Do this first, so nothing has changed if user C-g's at query.
  (and (buffer-modified-p)
       (y-or-n-p "Save the file? ")
       (save-buffer))
  (Info-mode)
  (force-mode-line-update)
  (and (marker-position Info-tag-table-marker)
       (buffer-modified-p)
       (message "Tags may have changed.  Use Info-tagify if necessary")))

(defvar ibuffer-help-buffer-modes)
;; Moved here from definition of ibuffer-help-buffer-modes to make
;; that variable customizable even though this code is obsolete.  See
;; also Bug#30990.
(add-to-list 'ibuffer-help-buffer-modes 'Info-edit-mode)

(provide 'info-edit)

;;; info-edit.el ends here
