;;; m-buffer-at.el --- Stateless point functions -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2014, Phillip Lord, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides stateless equivalents to many core Emacs functions, that provide
;; information about a buffer. Most of these functions take either a buffer as
;; a parameter or a location, which is either a marker (with a non-nil buffer
;; and location) or a buffer and integer.

;; These functions are generally competitive with the originals in terms of
;; speed.

;;; Status:

;; There are lots more to do, but the interface should be stable.

;;; Code:

;; #+begin_src emacs-lisp

(require 'm-buffer-macro)

(defun m-buffer-at-point (buffer)
  "Return the location of point in BUFFER.
See also `point'."
  (with-current-buffer
      buffer
    (point)))

(defun m-buffer-at-eolp (&rest location)
  "Return t if LOCATION is at the end of a line.
See also `eolp'."
  (m-buffer-with-current-location
      location
    (eolp)))

(defun m-buffer-at-bolp (&rest location)
  "Return t if LOCATION is at the begining of a line.
See also `bolp'"
  (m-buffer-with-current-location
      location
    (bolp)))

(defun m-buffer-at-line-beginning-position (&rest location)
  "Return the start of the line of LOCATION."
  (m-buffer-with-current-location
      location
    (line-beginning-position)))

(defun m-buffer-at-line-end-position (&rest location)
  "Return the end of the line of LOCATION."
  (m-buffer-with-current-location
      location
    (line-end-position)))

(defun m-buffer-at-narrowed-p (buffer)
  (with-current-buffer
      buffer
    (buffer-narrowed-p)))

(defun m-buffer-at-string (buffer)
  (with-current-buffer
      buffer
    (buffer-string)))

(provide 'm-buffer-at)
;;; m-buffer-at.el ends here
;; #+end_src
