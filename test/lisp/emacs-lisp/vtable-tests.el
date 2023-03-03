;;; vtable-tests.el --- Tests for vtable.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'vtable)
(require 'ert)
(require 'ert-x)

(ert-deftest test-vstable-compute-columns ()
  (should
   (equal (mapcar
           (lambda (column)
             (vtable-column-align column))
           (vtable--compute-columns
            (make-vtable :columns '("a" "b" "c")
                         :objects '(("foo" 1 2)
                                    ("bar" 3 :zot))
                         :insert nil)))
          '(left right left))))

;;; vtable-tests.el ends here
