;;; cl-print-tests.el --- Test suite for the cl-print facility.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

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

(cl-defstruct cl-print--test a b)

(ert-deftest cl-print-tests-1 ()
  "Test cl-print code."
  (let ((x (make-cl-print--test :a 1 :b 2)))
    (let ((print-circle nil))
      (should (equal (cl-prin1-to-string `((x . ,x) (y . ,x)))
                     "((x . #s(cl-print--test :a 1 :b 2)) (y . #s(cl-print--test :a 1 :b 2)))")))
    (let ((print-circle t))
      (should (equal (cl-prin1-to-string `((x . ,x) (y . ,x)))
                     "((x . #1=#s(cl-print--test :a 1 :b 2)) (y . #1#))")))
    (should (string-match "\\`#f(compiled-function (x) \"[^\"]+\" [^\)]*)\\'"
                          (cl-prin1-to-string (symbol-function #'caar))))))

(ert-deftest cl-print-tests-2 ()
  (let ((x (record 'foo 1 2 3)))
    (should (equal
             x
             (car (read-from-string (with-output-to-string (prin1 x))))))
    (let ((print-circle t))
      (should (string-match
               "\\`(#1=#s(foo 1 2 3) #1#)\\'"
               (cl-prin1-to-string (list x x)))))))

(ert-deftest cl-print-circle ()
  (let ((x '(#1=(a . #1#) #1#)))
    (let ((print-circle nil))
      (should (string-match "\\`((a . #[0-9]) (a . #[0-9]))\\'"
                            (cl-prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(#1=(a . #1#) #1#)" (cl-prin1-to-string x))))))

;;; cl-print-tests.el ends here.
