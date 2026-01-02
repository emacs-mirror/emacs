;;; time-date-tests.el --- tests for calendar/time-date.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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
(require 'time-date)

(ert-deftest test-obsolete-with-decoded-time-value ()
  (with-suppressed-warnings ((obsolete with-decoded-time-value))
    (with-decoded-time-value ((high low micro pico type '(1 2 3 4 5 6 8 8)))
      (should (equal (list high low micro pico type) '(1 2 3 4 3))))))

(ert-deftest test-obsolete-encode-time-value ()
  (should (equal (with-suppressed-warnings ((obsolete encode-time-value))
                   (encode-time-value 1 2 3 4 0))
                 '(1 . 2)))
  (should (equal (with-suppressed-warnings ((obsolete encode-time-value))
                   (encode-time-value 1 2 3 4 1))
                 '(1 2)))
  (should (equal (with-suppressed-warnings ((obsolete encode-time-value))
                   (encode-time-value 1 2 3 4 2))
                 '(1 2 3)))
  (should (equal (with-suppressed-warnings ((obsolete encode-time-value))
                   (encode-time-value 1 2 3 4 3))
                 '(1 2 3 4))))

(ert-deftest test-date-to-time ()
  (let ((date-list
         '(("2021-12-04"                   (00 00 00 04 12 2021 nil -1 nil))
           ("2006-05-04T03:02:01Z"         (01 02 03 04 05 2006 nil nil 0))
           ;; Test cases from timezone-parse-date docstring
           ("14 Apr 89 03:20"              (00 20 03 14 04 1989 nil -1 nil))
           ("14 Apr 89 03:20:12 GMT"       (12 20 03 14 04 1989 nil nil 0))
           ("Fri, 17 Mar 89 4:01"          (00 01 04 17 03 1989 nil -1 nil))
           ("Fri, 17 Mar 89 4:01:33 GMT"   (33 01 04 17 03 1989 nil nil 0))
           ("Mon Jan 16 16:12 1989"        (00 12 16 16 01 1989 nil -1 nil))
           ("Mon Jan 16 16:12:37 GMT 1989" (37 12 16 16 01 1989 nil nil 0))
           ("Thu, 11 Apr 16:17:12 91"      (12 17 16 11 04 1991 nil -1 nil))
           ("Mon, 6  Jul 16:47:20 T 1992"  (20 47 16 06 07 1992 nil -1 nil))
           ("1996-06-24 21:13:12"          (12 13 21 24 06 1996 nil -1 nil))
           ("19960624t211312"              (12 13 21 24 06 1996 nil -1 nil))
           ;; These are parsed incorrectly:
           ;; "6 May 1992 1641-JST (Wednesday)"
           ;; "22-AUG-1993 10:59:12.82"
           ;; "1996-06-24 21:13-ZONE"
           )))
    (dolist (date date-list)
      (should (equal (date-to-time (car date))
                     (encode-time (cadr date)))))))

(ert-deftest test-days-between ()
  (should (equal (days-between "2021-10-22" "2020-09-29") 388)))

(ert-deftest test-leap-year ()
  (should-not (date-leap-year-p 1999))
  (should-not (date-leap-year-p 1900))
  (should (date-leap-year-p 2000))
  (should (date-leap-year-p 2004)))

(ert-deftest test-days-to-time ()
  (should (time-equal-p (days-to-time 0) '(0 0)))
  (should (time-equal-p (days-to-time 1) '(1 20864)))
  (should (time-equal-p (days-to-time 999) '(1317 2688)))
  (should (time-equal-p (days-to-time 0.0) '(0 0 0 0)))
  (should (time-equal-p (days-to-time 0.5) '(0 43200 0 0)))
  (should (time-equal-p (days-to-time 1.0) '(1 20864 0 0)))
  (should (time-equal-p (days-to-time 999.0) '(1317 2688 0 0))))

(ert-deftest test-seconds-to-string ()
  (should (equal (seconds-to-string 0) "0s"))
  (should (equal (seconds-to-string 9) "9.00s"))
  (should (equal (seconds-to-string 99) "99.00s"))
  (should (equal (seconds-to-string 999) "16.65m"))
  (should (equal (seconds-to-string 9999) "2.78h"))
  (should (equal (seconds-to-string 99999) "27.78h"))
  (should (equal (seconds-to-string 999999) "11.57d"))
  (should (equal (seconds-to-string 9999999) "115.74d"))
  (should (equal (seconds-to-string 99999999) "3.17y"))
  (should (equal (seconds-to-string 999999999) "31.69y")))

(ert-deftest test-days-in-month ()
  (should (= (date-days-in-month 2004 2) 29))
  (should (= (date-days-in-month 2004 3) 31))
  (should (= (date-days-in-month 2019 2) 28))
  (should (= (date-days-in-month 2020 12) 31))
  (should-not (= (date-days-in-month 1900 3) 28))
  (should-error (date-days-in-month 2020 0))
  (should-error (date-days-in-month 2020 15))
  (should-error (date-days-in-month 2020 'foo)))

(ert-deftest test-format-seconds ()
  (let ((format-seconds-list
	 '(("%y %d %h %m %s %%" 0 "0 0 0 0 0 %")
	   ("%y %d %h %m %s %%" 0 "0 0 0 0 0 %")
	   ("%y %d %h %m %s %%" 9999999 "0 115 17 46 39 %")
	   ("%y %d %h %m %z %s %%" 1 "1 %")
	   ("%mm %ss" 66 "1m 6s")
	   ("%mm %5ss" 66 "1m     6s")
	   ("%mm %.5ss" 66.4 "1m 00006s")
	   ("%mm %,1ss" 66.4 "1m 6.4s")
	   ("%mm %5,1ss" 66.4 "1m   6.4s")
	   ("%mm %.5,1ss" 66.4 "1m 006.4s")
	   ("%hh %z%x%mm %ss" 120 "2m")
	   ("%hh %z%mm %ss" 120 "2m 0s")
	   ("%hh %x%mm %ss" 120 "0h 2m")
	   ("%hh %x%mm %ss" 0 "0h 0m 0s")
	   ("%y %z%d %h %m %s %%" 9999999 "115 17 46 39 %")
	   ("%Y, %D, %H, %M, %z%S" 0 "0 seconds"))))
    (dolist (fs format-seconds-list)
      (let ((string (nth 0 fs))
	    (seconds (nth 1 fs))
	    (expected (nth 2 fs)))
      (should (equal (format-seconds string seconds) expected))
      (when (< 0 seconds)
	(should (equal (format-seconds string (- seconds))
		       (concat "-" expected))))))))

(ert-deftest test-ordinal ()
  (should (equal (date-ordinal-to-time 2008 271)
                 '(nil nil nil 27 9 2008 nil nil nil)))
  (should (equal (date-ordinal-to-time 2008 1)
                 '(nil nil nil 1 1 2008 nil nil nil)))
  (should (equal (date-ordinal-to-time 2008 32)
                 '(nil nil nil 1 2 2008 nil nil nil)))
  (should (equal (date-ordinal-to-time 1981 095)
                 '(nil nil nil 5 4 1981 nil nil nil))))

(cl-defmethod mdec (&key second minute hour
                         day month year
                         dst zone)
  (list second minute hour day month year nil dst zone))

(ert-deftest test-decoded-add ()
  (let ((time '(12 15 16 8 7 2019 1 t 7200)))
    (should (equal (decoded-time-add time (mdec :year 1))
                   '(12 15 16 8 7 2020 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :year -2))
                   '(12 15 16 8 7 2017 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month 1))
                   '(12 15 16 8 8 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month 10))
                   '(12 15 16 8 5 2020 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month -1))
                   '(12 15 16 8 6 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month -10))
                   '(12 15 16 8 9 2018 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month -14))
                   '(12 15 16 8 5 2018 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :month -24))
                   '(12 15 16 8 7 2017 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day 1))
                   '(12 15 16 9 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day -1))
                   '(12 15 16 7 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day 30))
                   '(12 15 16 7 8 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day -365))
                   '(12 15 16 8 7 2018 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :day 365))
                   '(12 15 16 7 7 2020 1 t 7200)))

    ;; 2020 is a leap year.
    (should (equal (decoded-time-add time (mdec :day 366))
                   '(12 15 16 8 7 2020 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :second 1))
                   '(13 15 16 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :second -1))
                   '(11 15 16 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :second 61))
                   '(13 16 16 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :hour 1 :minute 2 :second 3))
                   '(15 17 17 8 7 2019 1 t 7200)))

    (should (equal (decoded-time-add time (mdec :hour 24))
                   '(12 15 16 9 7 2019 1 t 7200)))
    ))

(ert-deftest test-decoded-add-zone ()
  (let ((time '(12 15 16 8 7 2019 1 t 7200)))
    (should (equal (decoded-time-add time (mdec :zone -3600))
                   '(12 15 15 8 7 2019 1 t 7200)))
    (should (equal (decoded-time-add time (mdec :zone -7200))
                   '(12 15 14 8 7 2019 1 t 7200)))))

(ert-deftest test-time-since ()
  (should (time-equal-p 0 (time-since nil)))
  (should (time-equal-p 1 (time-convert (time-since (time-subtract nil 1))
                                        'integer))))

(ert-deftest test-time-decoded-period ()
  (should (equal (decoded-time-period '(nil nil 1 nil nil nil nil nil nil))
                 3600))

  (should (equal (decoded-time-period '(1 0 0 0 0 0 nil nil nil)) 1))
  (should (equal (decoded-time-period '(0 1 0 0 0 0 nil nil nil)) 60))
  (should (equal (decoded-time-period '(0 0 1 0 0 0 nil nil nil)) 3600))
  (should (equal (decoded-time-period '(0 0 0 1 0 0 nil nil nil)) 86400))
  (should (equal (decoded-time-period '(0 0 0 0 1 0 nil nil nil)) 2592000))
  (should (equal (decoded-time-period '(0 0 0 0 0 1 nil nil nil)) 31536000))
  (should (equal (decoded-time-period '(1 2 3 4 5 6 nil nil nil)) 202532521))

  (should (equal (decoded-time-period '((135 . 10) 0 0 0 0 0 nil nil nil))
                 13.5)))

(ert-deftest test-time-wrap-addition ()
  (should (equal (decoded-time-add '(0 0 0 1 11 2008 nil nil nil)
                                   (make-decoded-time :month 1))
                 '(0 0 0 1 12 2008 nil nil nil)))
  (should (equal (decoded-time-add '(0 0 0 1 12 2008 nil nil nil)
                                   (make-decoded-time :month 1))
                 '(0 0 0 1 1 2009 nil nil nil)))
  (should (equal (decoded-time-add '(0 0 0 1 11 2008 nil nil nil)
                                   (make-decoded-time :month 12))
                 '(0 0 0 1 11 2009 nil nil nil)))
  (should (equal (decoded-time-add '(0 0 0 1 11 2008 nil nil nil)
                                   (make-decoded-time :month 13))
                 '(0 0 0 1 12 2009 nil nil nil)))
  (should (equal (decoded-time-add '(0 0 0 30 12 2008 nil nil nil)
                                   (make-decoded-time :day 1))
                 '(0 0 0 31 12 2008 nil nil nil)))
  (should (equal (decoded-time-add '(0 0 0 30 12 2008 nil nil nil)
                                   (make-decoded-time :day 2))
                 '(0 0 0 1 1 2009 nil nil nil))))

;;; time-date-tests.el ends here
