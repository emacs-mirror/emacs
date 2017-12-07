;;; floatfns-tests.el --- tests for floating point operations

;; Copyright 2017 Free Software Foundation, Inc.

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

(require 'ert)

(ert-deftest divide-extreme-sign ()
  (should-error (ceiling most-negative-fixnum -1.0))
  (should-error (floor most-negative-fixnum -1.0))
  (should-error (round most-negative-fixnum -1.0))
  (should-error (truncate most-negative-fixnum -1.0)))

(ert-deftest logb-extreme-fixnum ()
  (should (= (logb most-negative-fixnum) (1+ (logb most-positive-fixnum)))))

(ert-deftest fround-fixnum ()
  (should-error (ffloor 0) :type 'wrong-type-argument)
  (should-error (fceiling 0) :type 'wrong-type-argument)
  (should-error (ftruncate 0) :type 'wrong-type-argument)
  (should-error (fround 0) :type 'wrong-type-argument))

(provide 'floatfns-tests)
