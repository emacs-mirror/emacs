;;; obarray.el --- obarray functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

(defconst obarray-default-size 4)
(make-obsolete-variable 'obarray-default-size
                        "obarrays now grow automatically." "30.1")

(defun obarray-size (_ob)
  (declare (obsolete "obarrays now grow automatically." "30.1"))
  obarray-default-size)

;; Don’t use obarray as a variable name to avoid shadowing.
(defun obarray-get (ob name)
  "Return symbol named NAME if it is contained in obarray OB.
Return nil otherwise."
  (intern-soft name ob))

(defun obarray-put (ob name)
  "Return symbol named NAME from obarray OB.
Creates and adds the symbol if it doesn't exist."
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
