;;; obarray.el --- obarray functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: obarray functions
;; Package: emacs

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

;; This file provides function for working with obarrays.

;;; Code:

(defconst obarray-default-size 59
  "The value 59 is an arbitrary prime number that gives a good hash.")

(defun obarray-make (&optional size)
  "Return a new obarray of size SIZE or `obarray-default-size'."
  (let ((size (or size obarray-default-size)))
    (if (< 0 size)
        (make-vector size 0)
      (signal 'wrong-type-argument '(size 0)))))

(defun obarray-size (ob)
  "Return the number of slots of obarray OB."
  (length ob))

(defun obarrayp (object)
  "Return t if OBJECT is an obarray."
  (and (vectorp object)
       (< 0 (length object))))

;; Don’t use obarray as a variable name to avoid shadowing.
(defun obarray-get (ob name)
  "Return symbol named NAME if it is contained in obarray OB.
Return nil otherwise."
  (intern-soft name ob))

(defun obarray-put (ob name)
  "Return symbol named NAME from obarray OB.
Creates and adds the symbol if doesn't exist."
  (intern name ob))

(defun obarray-remove (ob name)
  "Remove symbol named NAME if it is contained in obarray OB.
Return t on success, nil otherwise."
  (unintern name ob))

(defun obarray-map (fn ob)
  "Call function FN on every symbol in obarray OB and return nil."
  (mapatoms fn ob))

(provide 'obarray)
;;; obarray.el ends here
