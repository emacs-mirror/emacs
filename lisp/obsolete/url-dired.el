;;; url-dired.el --- URL Dired minor mode  -*- lexical-binding: t -*-

;; Copyright (C) 1996-1999, 2004-2023 Free Software Foundation, Inc.

;; Keywords: comm, files
;; Obsolete-since: 29.1

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

(autoload 'dired-get-filename "dired")

(defvar-keymap url-dired-minor-mode-map
  :doc "Keymap used when browsing directories."
  "RET"       #'url-dired-find-file
  "<mouse-2>" #'url-dired-find-file-mouse)

(defun url-dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (let ((filename (dired-get-filename)))
    (find-file filename)))

(defun url-dired-find-file-mouse (event)
  "In dired, visit the file or directory name you click on."
  (interactive "@e")
  (mouse-set-point event)
  (url-dired-find-file))

(define-minor-mode url-dired-minor-mode
  "Minor mode for directory browsing."
  :lighter " URL" :keymap url-dired-minor-mode-map)

(defun url-find-file-dired (dir)
  "\"Edit\" directory DIR, but with additional URL-friendly bindings."
  (interactive "DURL Dired (directory): ")
  (find-file dir)
  (url-dired-minor-mode t))

(provide 'url-dired)

;;; url-dired.el ends here
