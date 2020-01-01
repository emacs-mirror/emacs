;;; timefns-tests.el -- tests for timefns.c

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'ert)

(defun timefns-tests--decode-time (look zone decoded-time)
  (should (equal (decode-time look zone t) decoded-time))
  (should (equal (decode-time look zone 'integer)
		 (cons (time-convert (car decoded-time) 'integer)
		       (cdr decoded-time)))))

;;; Check format-time-string and decode-time with various TZ settings.
;;; Use only POSIX-compatible TZ values, since the tests should work
;;; even if tzdb is not in use.
(ert-deftest format-time-string-with-zone ()
  ;; Don’t use (0 0 0 0) as the test case, as there are too many bugs
  ;; in MS-Windows (and presumably other) C libraries when formatting
  ;; time stamps near the Epoch of 1970-01-01 00:00:00 UTC, and this
  ;; test is for GNU Emacs, not for C runtimes.  Instead, look before
  ;; you leap: "look" is the timestamp just before the first leap
  ;; second on 1972-06-30 23:59:60 UTC, so it should format to the
  ;; same string regardless of whether the underlying C library
  ;; ignores leap seconds, while avoiding circa-1970 glitches.
  ;;
  ;; Similarly, stick to the limited set of time zones that are
  ;; supported by both POSIX and MS-Windows: exactly 3 ASCII letters
  ;; in the abbreviation, and no DST.
  (let ((format "%Y-%m-%d %H:%M:%S.%3N %z (%Z)"))
    (dolist (look '((1202 22527 999999 999999)
		    (7879679999900 . 100000)
		    (78796799999999999999 . 1000000000000)))
      ;; UTC.
     (let* ((look-ticks-hz (time-convert look t))
	    (hz (cdr look-ticks-hz))
	    (look-integer (time-convert look 'integer))
	    (sec (time-add (time-convert 59 hz)
			   (time-subtract look-ticks-hz
					  (time-convert look-integer hz)))))
      (should (string-equal
	       (format-time-string "%Y-%m-%d %H:%M:%S.%3N %z" look t)
	       "1972-06-30 23:59:59.999 +0000"))
      (timefns-tests--decode-time look t
				  (list sec 59 23 30 6 1972 5 nil 0))
      ;; "UTC0".
      (should (string-equal
	       (format-time-string format look "UTC0")
	       "1972-06-30 23:59:59.999 +0000 (UTC)"))
      (timefns-tests--decode-time look "UTC0"
				  (list sec 59 23 30 6 1972 5 nil 0))
      ;; Negative UTC offset, as a Lisp list.
      (should (string-equal
	       (format-time-string format look '(-28800 "PST"))
	       "1972-06-30 15:59:59.999 -0800 (PST)"))
      (timefns-tests--decode-time look '(-28800 "PST")
				  (list sec 59 15 30 6 1972 5 nil -28800))
      ;; Negative UTC offset, as a Lisp integer.
      (should (string-equal
	       (format-time-string format look -28800)
	       ;; MS-Windows build replaces unrecognizable TZ values,
	       ;; such as "-08", with "ZZZ".
	       (if (eq system-type 'windows-nt)
		   "1972-06-30 15:59:59.999 -0800 (ZZZ)"
		 "1972-06-30 15:59:59.999 -0800 (-08)")))
      (timefns-tests--decode-time look -28800
				  (list sec 59 15 30 6 1972 5 nil -28800))
      ;; Positive UTC offset that is not an hour multiple, as a string.
      (should (string-equal
	       (format-time-string format look "IST-5:30")
	       "1972-07-01 05:29:59.999 +0530 (IST)"))
      (timefns-tests--decode-time look "IST-5:30"
				  (list sec 29 5 1 7 1972 6 nil 19800))))))

(ert-deftest decode-then-encode-time ()
  (let ((time-values (list 0 -2 1 0.0 -0.0 -2.0 1.0
			   most-negative-fixnum most-positive-fixnum
			   (1- most-negative-fixnum)
			   (1+ most-positive-fixnum)
			   1e+INF -1e+INF 1e+NaN -1e+NaN
			   '(0 1 0 0) '(1 0 0 0) '(-1 0 0 0)
			   '(123456789000000 . 1000000)
			   (cons (1+ most-positive-fixnum) 1000000000000)
			   (cons 1000000000000 (1+ most-positive-fixnum)))))
    (dolist (a time-values)
      (let* ((d (ignore-errors (decode-time a t t)))
             (d-integer (ignore-errors (decode-time a t 'integer)))
	     (e (if d (encode-time d)))
	     (e-integer (if d-integer (encode-time d-integer))))
	(should (or (not d) (time-equal-p a e)))
	(should (or (not d-integer) (time-equal-p (time-convert a 'integer)
                                                  e-integer)))))))

;;; This should not dump core.
(ert-deftest format-time-string-with-outlandish-zone ()
  (should (stringp
           (format-time-string "%Y-%m-%d %H:%M:%S.%3N %z" nil
                               (concat (make-string 2048 ?X) "0")))))

(defun timefns-tests--have-leap-seconds ()
  (string-equal (format-time-string "%Y-%m-%d %H:%M:%S" 78796800 t)
                "1972-06-30 23:59:60"))

(ert-deftest format-time-string-with-bignum-on-32-bit ()
  (should (or (string-equal
               (format-time-string "%Y-%m-%d %H:%M:%S" (- (ash 1 31) 3600) t)
               "2038-01-19 02:14:08")
              (timefns-tests--have-leap-seconds))))

;;; Tests of format-time-string padding

(ert-deftest format-time-string-padding-minimal-deletes-unneeded-zeros ()
  (let ((ref-time (append (encode-time 0 0 0 15 2 2000) '(123450))))
    (should (equal (format-time-string "%-:::z" ref-time "FJT-12") "+12"))
    (should (equal (format-time-string "%-N" ref-time) "12345"))
    (should (equal (format-time-string "%-6N" ref-time) "12345"))
    (should (equal (format-time-string "%-m" ref-time) "2")))) ;not "02"

(ert-deftest format-time-string-padding-minimal-retains-needed-zeros ()
  (let ((ref-time (append (encode-time 0 0 0 20 10 2000) '(3450))))
    (should (equal (format-time-string "%-z" ref-time "IST-5:30") "+530"))
    (should (equal (format-time-string "%-4z" ref-time "IST-5:30") "+530"))
    (should (equal (format-time-string "%4z" ref-time "IST-5:30") "+530"))
    (should (equal (format-time-string "%-N" ref-time) "00345"))
    (should (equal (format-time-string "%-3N" ref-time) "003"))
    (should (equal (format-time-string "%3N" ref-time) "003"))
    (should (equal (format-time-string "%-m" ref-time) "10")) ;not "1"
    (should (equal (format-time-string "%-1m" ref-time) "10")) ;not "1"
    (should (equal (format-time-string "%1m" ref-time) "10")))) ;not "1"

(ert-deftest format-time-string-padding-spaces ()
  (let ((ref-time (append (encode-time 0 0 0 10 12 2000) '(123000))))
    (should (equal (format-time-string "%_7z" ref-time "CHA-12:45") "  +1245"))
    (should (equal (format-time-string "%_6N" ref-time) "123   "))
    (should (equal (format-time-string "%_9N" ref-time) "123      "))
    (should (equal (format-time-string "%_12N" ref-time) "123         "))
    (should (equal (format-time-string "%_m" ref-time) "12"))
    (should (equal (format-time-string "%_2m" ref-time) "12"))
    (should (equal (format-time-string "%_3m" ref-time) " 12"))))

(ert-deftest format-time-string-padding-zeros-adds-on-insignificant-side ()
  "Fractional seconds have a fixed place on the left,
and any padding must happen on the right.  All other numbers have
a fixed place on the right and are padded on the left."
  (let ((ref-time (append (encode-time 0 0 0 10 12 2000) '(123000))))
    (should (equal (format-time-string "%3m" ref-time) "012"))
    (should (equal (format-time-string "%7z" ref-time "CHA-12:45") "+001245"))
    (should (equal (format-time-string "%12N" ref-time) "123000000000"))
    (should (equal (format-time-string "%9N" ref-time) "123000000"))
    (should (equal (format-time-string "%6N" ref-time) "123000"))))


(ert-deftest time-equal-p-nil-nil ()
  (should (time-equal-p nil nil)))

(ert-deftest time-arith-tests ()
  (let ((time-values (list 0 -1 1 0.0 -0.0 -1.0 1.0
			   most-negative-fixnum most-positive-fixnum
			   (1- most-negative-fixnum)
			   (1+ most-positive-fixnum)
			   1e1 -1e1 1e-1 -1e-1
			   1e8 -1e8 1e-8 -1e-8
			   1e9 -1e9 1e-9 -1e-9
			   1e10 -1e10 1e-10 -1e-10
			   1e16 -1e16 1e-16 -1e-16
			   1e37 -1e37 1e-37 -1e-37
			   1e+INF -1e+INF 1e+NaN -1e+NaN
			   '(0 0 0 1) '(0 0 1 0) '(0 1 0 0) '(1 0 0 0)
			   '(-1 0 0 0) '(1 2 3 4) '(-1 2 3 4)
			   '(-123456789 . 100000) '(123456789 . 1000000)
			   (cons (1+ most-positive-fixnum) 1000000000000)
			   (cons 1000000000000 (1+ most-positive-fixnum)))))
    (dolist (a time-values)
      (should-error (time-add a 'ouch))
      (should-error (time-add 'ouch a))
      (should-error (time-subtract a 'ouch))
      (should-error (time-subtract 'ouch a))
      (dolist (b time-values)
	(let ((aa (time-subtract (time-add a b) b)))
	  (should (or (time-equal-p a aa) (and (floatp aa) (isnan aa)))))
	(should (= 1 (+ (if (time-less-p a b) 1 0)
			(if (time-equal-p a b) 1 0)
			(if (time-less-p b a) 1 0)
			(if (or (and (floatp a) (isnan a))
				(and (floatp b) (isnan b)))
			    1 0))))
	(should (or (not (time-less-p 0 b))
		    (time-less-p a (time-add a b))
		    (time-equal-p a (time-add a b))
		    (and (floatp (time-add a b)) (isnan (time-add a b)))))
	(let ((x (float-time (time-add a b)))
	      (y (+ (float-time a) (float-time b))))
	  (should (or (and (isnan x) (isnan y))
		      (= x y)
		      (< 0.99 (/ x y) 1.01)
		      (< 0.99 (/ (- (float-time a)) (float-time b))
			 1.01))))))))

(ert-deftest time-rounding-tests ()
  (should (time-equal-p 1e-13 (time-add 0 1e-13))))

(ert-deftest encode-time-dst-numeric-zone ()
    "Check for Bug#35502."
    (should (time-equal-p
             (encode-time '(29 31 17 30 4 2019 2 t 7200))
             '(23752 27217))))

(ert-deftest float-time-precision ()
  (should (< 0 (float-time '(1 . 10000000000))))
  (should (< (float-time '(-1 . 10000000000)) 0))

  (let ((x 1.0))
    (while (not (zerop x))
      (dolist (multiplier '(-1.9 -1.5 -1.1 -1 1 1.1 1.5 1.9))
        (let ((xmult (* x multiplier)))
          (should (= xmult (float-time (time-convert xmult t))))))
      (setq x (/ x 2))))

  (let ((x 1.0))
    (while (ignore-errors (time-convert x t))
      (dolist (divisor '(-1.9 -1.5 -1.1 -1 1 1.1 1.5 1.9))
        (let ((xdiv (/ x divisor)))
          (should (= xdiv (float-time (time-convert xdiv t))))))
      (setq x (* x 2)))))
