;;; calculator-tests.el --- Test suite for calculator. -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;;; Code:
(require 'ert)
(require 'calculator)

(ert-deftest calculator-test-calculator-string-to-number ()
  (dolist (x '((""          0.0)
               ("+"         0.0)
               ("-"         0.0)
               ("."         0.0)
               ("+."        0.0)
               ("-."       -0.0)
               (".-"        0.0)
               ("--."       0.0)
               ("-0.0e"    -0.0)
               ("1e1"      10.0)
               ("1e+1"     10.0)
               ("1e-1"      0.1)
               ("+1e1"     10.0)
               ("-1e1"    -10.0)
               ("+1e-1"     0.1)
               ("-1e-1"    -0.1)
               (".1.e1"     0.1)
               (".1..e1"    0.1)
               ("1e+1.1"   10.0)
               ("-2e-1.1"  -0.2)))
    (pcase x
      (`(,str ,expected)
       (let ((calculator-input-radix nil))
         (should (equal (calculator-string-to-number str) expected)))))))

(provide 'calculator-tests)
;;; calculator-tests.el ends here
