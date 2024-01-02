;;; image-circular-tests.el --- test image functions with circular objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2021-2024 Free Software Foundation, Inc.

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

(declare-function image-size "image.c" (spec &optional pixels frame))

(ert-deftest image-test-duplicate-keywords ()
  "Test that duplicate keywords in an image spec lead to rejection."
  (skip-unless (display-images-p))
  (should-error (image-size `(image :type xbm :type xbm
                                    :data-width 1 :data-height 1
                                    :data ,(bool-vector t))
                            t)))

(ert-deftest image-test-circular-plist ()
  "Test that a circular image spec is rejected."
  (skip-unless (display-images-p))
  (let ((spec `(image :type xbm :data-width 1 :data-height 1
                      :data ,(bool-vector t)
                      . ,'#1=(:invalid . #1#))))
    (should-error (image-size spec t))))

(ert-deftest image-test-:type-property-value ()
  "Test that :type is allowed as a property value in an image spec."
  (skip-unless (display-images-p))
  (should (equal (image-size `(image :dummy :type :type xbm
                                     :data-width 1 :data-height 1
                                     :data ,(bool-vector t))
                             t)
                 '(1 . 1))))

(ert-deftest image-test-circular-specs ()
  "Test with circular image spec property values.
In particular, test that they do not cause infinite recursion."
  :expected-result :failed ;; FIXME: bug#36403#63.
  (skip-unless (display-images-p))
  ;; Two copies needed to warm up image cache.
  (let* ((circ1 (list :dummy))
         (circ2 (list :dummy))
         (spec1 `(image :type xbm :data-width 1 :data-height 1
                        :data ,(bool-vector 1) :ignored ,circ1))
         (spec2 `(image :type xbm :data-width 1 :data-height 1
                        :data ,(bool-vector 1) :ignored ,circ2)))
    (setcdr circ1 circ1)
    (setcdr circ2 circ2)
    (should (equal (image-size spec1 t) '(1 . 1)))
    (should (equal (image-size spec2 t) '(1 . 1)))))

(provide 'image-circular-tests)
;;; image-circular-tests.el ends here.
