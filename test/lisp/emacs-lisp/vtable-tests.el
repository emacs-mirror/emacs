;;; vtable-tests.el --- Tests for vtable.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

(ert-deftest test-vtable-insert-object ()
  (should
   (equal (let ((buffer (get-buffer-create " *vtable-test*")))
            (pop-to-buffer buffer)
            (erase-buffer)
            (let* ((object1 '("Foo" 3))
                   (object2 '("Gazonk" 8))
                   (table (make-vtable
                           :columns '("Name" (:name "Rank" :width 5))
                           :objects (list object1 object2))))
              (mapc (lambda (args)
                      (pcase-let ((`(,object ,location ,before) args))
                        (vtable-insert-object table object location before)))
                    `( ; Some correct inputs.
                      ;; object    location        before
                      (("Fizz" 4)  ,object1        nil)
                      (("Bop"  7)  ,object2        t)
                      (("Zat"  5)  2               nil)
                      (("Dib"  6)  3               t)
                      (("Wup"  9)  nil             nil)
                      (("Quam" 2)  nil             t)
                      ;; And some faulty inputs.
                      (("Yat"  1)  -1              nil) ; non-existing index, `before' is ignored.
                      (("Vop"  10) 100             t)   ; non-existing index, `before' is ignored.
                      (("Jib"  11) ("Bleh"  0)     nil) ; non-existing object.
                      (("Nix"  0)  ("Ugh"   0)     t)   ; non-existing object.
                      ))
              (mapcar #'cadr (vtable-objects table))))
          (number-sequence 0 11))))

;;; vtable-tests.el ends here
