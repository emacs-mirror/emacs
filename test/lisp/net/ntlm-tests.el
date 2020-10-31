;;; ntlm-tests.el --- tests for ntlm.el            -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
(require 'ntlm)

;; This is the Lisp bignum implementation of `ntlm--time-to-timestamp',
;; for reference.
(defun ntlm-tests--time-to-timestamp (time)
  "Convert TIME to an NTLMv2 timestamp.
Return a unibyte string representing the number of tenths of a
microsecond since January 1, 1601 as a 64-bit little-endian
signed integer.  TIME must be on the form (HIGH LOW USEC PSEC)."
  (let* ((s (+ (ash (nth 0 time) 16) (nth 1 time)))
         (us (nth 2 time))
         (ps (nth 3 time))
         (tenths-of-us-since-jan-1-1601
          (+ (* s 10000000) (* us 10) (/ ps 100000)
	     ;; tenths of microseconds between 1601-01-01 and 1970-01-01
	     116444736000000000)))
    (apply #'unibyte-string
           (mapcar (lambda (i)
                     (logand (ash tenths-of-us-since-jan-1-1601 (* i -8))
                             #xff))
                   (number-sequence 0 7)))))

(ert-deftest ntlm-time-to-timestamp ()
  ;; Verify poor man's bignums in implementation that can run on Emacs < 27.1.
  (let ((time '(24471 63910 412962 0)))
    (should (equal (ntlm--time-to-timestamp time)
                   (ntlm-tests--time-to-timestamp time))))
  (let ((time '(397431 65535 999999 999999)))
    (should (equal (ntlm--time-to-timestamp time)
                   (ntlm-tests--time-to-timestamp time)))))

(provide 'ntlm-tests)
