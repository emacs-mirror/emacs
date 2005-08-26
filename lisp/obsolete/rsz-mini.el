;;; rsz-mini.el --- dynamically resize minibuffer to display entire contents

;; Copyright (C) 1990, 1993, 1994, 1995, 1997, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@splode.com>
;;         Roland McGrath <roland@gnu.org>
;; Maintainer: Noah Friedman <friedman@splode.com>
;; Keywords: minibuffer, window, frame, display

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

;; This package is obsolete.  Emacs now resizes mini-windows
;; automatically.

;;; Code:


(defgroup resize-minibuffer nil
  "This customization group is obsolete."
  :group 'frames)

;;;###autoload
(defcustom resize-minibuffer-mode nil
  "*This variable is obsolete."
  :type 'boolean
  :group 'resize-minibuffer
  :require 'rsz-mini)

;;;###autoload
(defcustom resize-minibuffer-window-max-height nil
  "*This variable is obsolete."
  :type '(choice (const nil) integer)
  :group 'resize-minibuffer)

;;;###autoload
(defcustom resize-minibuffer-window-exactly t
  "*This variable is obsolete."
  :type 'boolean
  :group 'resize-minibuffer)

;;;###autoload
(defcustom resize-minibuffer-frame nil
  "*This variable is obsolete."
  :type 'boolean
  :group 'resize-minibuffer)

;;;###autoload
(defcustom resize-minibuffer-frame-max-height nil
  "*This variable is obsolete."
  :group 'resize-minibuffer)

;;;###autoload
(defcustom resize-minibuffer-frame-exactly t
  "*This variable is obsolete."
  :type 'boolean
  :group 'resize-minibuffer)


;;;###autoload
(defun resize-minibuffer-mode (&optional prefix)
  "This function is obsolete."
  (interactive "P"))

(provide 'rsz-mini)

;;; arch-tag: 3cb85d51-ab33-4e46-8362-dd87a5d06c99
;;; rsz-mini.el ends here
