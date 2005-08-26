;;; hscroll.el --- automatically scroll truncated lines horizontally
;;; Copyright (C) 1992, 1993, 1995, 1996 Free Software Foundation, Inc.

;; Author: Wayne Mesard <wmesard@esd.sgi.com>
;; Keywords: display

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file contains dummy variables and functions only because Emacs
;; does hscrolling automatically, now.

;;; Code:

;;;
;;; PUBLIC VARIABLES
;;;

(defvar hscroll-version "0.0")

(defgroup hscroll nil
  "This customization group is kept for compatibility only.
Emacs now does hscrolling automatically.  Please remove references
to hscroll from your init file and code."
  :group 'editing)


(defcustom hscroll-global-mode nil
  "*Obsolete."
  :group 'hscroll
  :type 'boolean
  :require 'hscroll
  :version "20.3")

(defcustom hscroll-margin 5
  "*Obsolete."
  :group 'hscroll
  :type 'integer)

(defcustom hscroll-snap-threshold 30
  "*Obsolete."
  :group 'hscroll
  :type 'integer)

(defcustom hscroll-step-percent 25
  "*Obsolete."
  :group 'hscroll
  :type 'integer)

(defcustom hscroll-mode-name " Hscr"
  "*Obsolete."
  :group 'hscroll
  :type 'string)

;;;
;;; PUBLIC COMMANDS
;;;

;;;###autoload
(defun turn-on-hscroll ()
  "This function is obsolete.
Emacs now does hscrolling automatically, if `truncate-lines' is non-nil.
Also see `automatic-hscrolling'.")

;;;###autoload
(defun hscroll-mode (&optional arg)
  "This function is obsolete.
Emacs now does hscrolling automatically, if `truncate-lines' is non-nil.
Also see `automatic-hscrolling'."
  (interactive "P"))

;;;###autoload
(defun hscroll-global-mode  (&optional arg)
  "This function is obsolete.
Emacs now does hscrolling automatically, if `truncate-lines' is non-nil.
Also see `automatic-hscrolling'."
  (interactive "P"))

(defun hscroll-window-maybe ()
  "This function is obsolete.
Emacs now does hscrolling automatically, if `truncate-lines' is non-nil.
Also see `automatic-hscrolling'."
  (interactive))

(provide 'hscroll)

;;; arch-tag: 48377520-e5ca-401d-b360-3881b2d5a05a
;;; hscroll.el ends here
