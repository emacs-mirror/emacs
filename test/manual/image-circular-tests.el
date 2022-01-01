;;; image-circular-tests.el --- test image functions with circular objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2021-2022 Free Software Foundation, Inc.

;; Author: Pip Cet <pipcet@gmail.com>
;; Keywords:       internal
;; Human-Keywords: internal

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

(require 'ert)

(ert-deftest image-test-duplicate-keywords ()
  "Test that duplicate keywords in an image spec lead to rejection."
  (should-error (image-size `(image :type xbm :type xbm :width 1 :height 1
                                    :data ,(bool-vector t))
                            t)))

(ert-deftest image-test-circular-plist ()
  "Test that a circular image spec is rejected."
  (should-error
   (let ((l `(image :type xbm :width 1 :height 1 :data ,(bool-vector t))))
     (setcdr (last l) '#1=(:invalid . #1#))
     (image-size l t))))

(ert-deftest image-test-:type-property-value ()
  "Test that :type is allowed as a property value in an image spec."
  (should (equal (image-size `(image :dummy :type :type xbm :width 1 :height 1
                                        :data ,(bool-vector t))
                                t)
                 (cons 1 1))))

(ert-deftest image-test-circular-specs ()
  "Test that circular image spec property values do not cause infinite recursion."
  (should
   (let* ((circ1 (cons :dummy nil))
          (circ2 (cons :dummy nil))
          (spec1 `(image :type xbm :width 1 :height 1
                         :data ,(bool-vector 1) :ignored ,circ1))
          (spec2 `(image :type xbm :width 1 :height 1
                        :data ,(bool-vector 1) :ignored ,circ2)))
     (setcdr circ1 circ1)
     (setcdr circ2 circ2)
     (and (equal (image-size spec1 t) (cons 1 1))
          (equal (image-size spec2 t) (cons 1 1))))))

(provide 'image-circular-tests)
;;; image-circular-tests.el ends here.
