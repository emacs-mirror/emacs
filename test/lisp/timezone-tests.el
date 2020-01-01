;;; timezone-tests.el --- Tests for timezone.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

(require 'ert)
(require 'timezone)

(ert-deftest timezone-tests-make-date-arpa-standard ()
  (should (equal (timezone-make-date-arpa-standard "14 Apr 89 03:20" "PST" "GMT")
                 "14 Apr 1989 11:20:00 GMT"))
  (should (equal (timezone-make-date-arpa-standard "14 Apr 89 03:20" "GMT" "GMT")
                 "14 Apr 1989 03:20:00 GMT"))
  (should (equal (timezone-make-date-arpa-standard "14 Apr 89 03:20" nil "GMT") ; assume GMT
                 "14 Apr 1989 03:20:00 GMT")))

(ert-deftest timezone-tests-make-date-sortable ()
  (should (equal (timezone-make-date-sortable "14 Apr 89 03:20" "GMT" "GMT")
                 "1989041403:20:00"))
  (should (equal (timezone-make-date-sortable "14 Apr 89 03:20" nil "GMT") ; assume GMT
                 "1989041403:20:00")))

(ert-deftest timezone-tests-make-arpa-date ()
  (should (equal (timezone-make-arpa-date 2020 1 1 "00:00")
                 "01 Jan 2020 00:00 +0000"))
  (should (equal (timezone-make-arpa-date 2020 1 1 "00:00" "GMT")
                 "01 Jan 2020 00:00 GMT")))

(ert-deftest timezone-tests-make-sortable-date ()
  (should (equal (timezone-make-sortable-date 2020 1 1 "00:00")
                 "2020010100:00")))

(ert-deftest timezone-tests-make-time-string ()
  (should (equal (timezone-make-time-string 1 2 3) "01:02:03")))

(ert-deftest timezone-tests-parse-date ()
  ;; Accepted style 1 from docstring
  (should (equal (timezone-parse-date "14 Apr 89 03:20")
                 ["1989" "4" "14" "03:20" nil]))
  (should (equal (timezone-parse-date "14 Apr 89 03:20 GMT")
                 ["1989" "4" "14" "03:20" "GMT"]))
  (should (equal (timezone-parse-date "14 Apr 89 03:20:12")
                 ["1989" "4" "14" "03:20:12" nil]))
  (should (equal (timezone-parse-date "14 Apr 89 03:20:12 GMT")
                 ["1989" "4" "14" "03:20:12" "GMT"]))
  ;; Accepted style 2 from docstring
  (should (equal (timezone-parse-date "Fri, 17 Mar 89 4:01")
                 ["1989" "3" "17" "4:01" nil]))
  (should (equal (timezone-parse-date "Fri, 17 Mar 89 4:01 GMT")
                 ["1989" "3" "17" "4:01" "GMT"]))
  (should (equal (timezone-parse-date "Fri, 17 Mar 89 4:01:33")
                 ["1989" "3" "17" "4:01:33" nil]))
  (should (equal (timezone-parse-date "Fri, 17 Mar 89 4:01:33 GMT")
                 ["1989" "3" "17" "4:01:33" "GMT"]))
  ;; Accepted style 3 from docstring
  (should (equal (timezone-parse-date "Mon Jan 16 16:12 1989")
                 ["1989" "1" "16" "16:12" nil]))
  (should (equal (timezone-parse-date "Mon Jan 16 16:12 GMT 1989")
                 ["1989" "1" "16" "16:12" "GMT"]))
  (should (equal (timezone-parse-date "Mon Jan 16 16:12:37 1989")
                 ["1989" "1" "16" "16:12:37" nil]))
  (should (equal (timezone-parse-date "Mon Jan 16 16:12:37 GMT 1989")
                 ["1989" "1" "16" "16:12:37" "GMT"]))
  ;; Accepted style 4 from docstring
  (should (equal (timezone-parse-date "6 May 1992 1641-JST (Wednesday)")
                 ["1992" "5" "6" "1641" "-JST"]))
  ;; Accepted style 5 from docstring
  (should (equal (timezone-parse-date "22-AUG-1993 10:59:12.82")
                 ["1993" "8" "22" "10:59:12" nil]))
  ;; Accepted style 6 from docstring
  (should (equal (timezone-parse-date "Thu, 11 Apr 16:17:12 91")
                 ["1991" "4" "11" "16:17:12" nil]))
  (should (equal (timezone-parse-date "Thu, 11 Apr 16:17:12 91 MET")
                 ["1991" "4" "11" "16:17:12" "MET"]))
  ;; Accepted style 7 from docstring
  (should (equal (timezone-parse-date "Mon, 6  Jul 16:47:20 T 1992")
                 ["1992" "7" "6" "16:47:20" nil]))
  (should (equal (timezone-parse-date "Mon, 6  Jul 16:47:20 T 1992 MET")
                 ["1992" "7" "6" "16:47:20" "MET"]))
  ;; Accepted style 8 from docstring
  (should (equal (timezone-parse-date "1996-06-24 21:13")
                 ["1996" "06" "24" "21:13" nil]))
  (should (equal (timezone-parse-date "1996-06-24 21:13 GMT")
                 ["1996" "06" "24" "21:13" "GMT"]))
  (should (equal (timezone-parse-date "1996-06-24 21:13:12")
                 ["1996" "06" "24" "21:13:12" nil]))
  (should (equal (timezone-parse-date "1996-06-24 21:13:12 GMT")
                 ["1996" "06" "24" "21:13:12" "GMT"]))
  ;; Accepted style 9 from docstring
  (should (equal (timezone-parse-date "1996-06-24 21:13-ZONE")
                 ["1996" "06" "24" "21:13" "-ZONE"]))
  ;; Accepted style 10 from docstring
  (should (equal (timezone-parse-date "19960624T211312")
                 ["1996" "06" "24" "211312" nil]))
  (should (equal (timezone-parse-date "*invalid*") ["0" "0" "0" "0" nil])))

(ert-deftest timezone-tests-parse-date/windowed-dates ()
  (should (equal (timezone-parse-date "14 Apr 69 10:00")
                 ["1969" "4" "14" "10:00" nil]))
  (should (equal (timezone-parse-date "14 Apr 68 10:00")
                 ["2068" "4" "14" "10:00" nil]))
  (should (equal (timezone-parse-date "14 Apr 199 10:00")
                 ["2099" "4" "14" "10:00" nil])))

(ert-deftest timezone-tests-parse-time ()
  (should (equal (timezone-parse-time "1234")      ["12" "34" "0"]))
  (should (equal (timezone-parse-time "12:34")     ["12" "34" "0"]))
  (should (equal (timezone-parse-time "123456")    ["12" "34" "56"]))
  (should (equal (timezone-parse-time "12:34:56")  ["12" "34" "56"]))
  (should (equal (timezone-parse-time "*invalid*") ["0" "0" "0"])))

(ert-deftest timezone-tests-zone-to-minute ()
  (should (equal (timezone-zone-to-minute "GMT") 0))
  (should (equal (timezone-zone-to-minute "CDT") -300))
  (should (equal (timezone-zone-to-minute "+100") 60))
  (should (equal (timezone-zone-to-minute "-100") -60))
  (should (equal (timezone-zone-to-minute "*invalid*") 0)))

(ert-deftest timezone-tests-time-from-absolute ()
  (should (time-equal-p
	   (timezone-time-from-absolute (* 2020 365)  ; Jan 1 2020
					(* 12 60 60)) ; 12:00
	   '(23911 48704 0 0))))

;; TODO: Write tests for timezone-tests-time-zone-from-absolute, which is a
;;       bit tricky since the results depend on `current-time-zone'.

(ert-deftest timezone-tests-fix-time ()
  (should (equal (timezone-fix-time "20200501 20:20" "GMT" "GMT")
                 [2020 5 1 20 20 0 "GMT"]))
  (should (equal (timezone-fix-time "20200501 20:20" nil "GMT") ; assume GMT
                 [2020 5 1 20 20 0 "GMT"])))

(ert-deftest timezone-tests-last-day-of-month ()
  (should (equal (timezone-last-day-of-month 2 1999) 28))
  (should (equal (timezone-last-day-of-month 2 2000) 29))) ; leap year

(ert-deftest timezone-tests-leap-year-p ()
  (should (equal (timezone-leap-year-p 1999) nil))
  (should (equal (timezone-leap-year-p 2000) t))
  (should (equal (timezone-leap-year-p 2024) t))
  (should (equal (timezone-leap-year-p 2100) nil)))

(ert-deftest timezone-tests-day-number ()
  (should (equal (timezone-day-number 5 1 1999) 121))
  (should (equal (timezone-day-number 5 1 2000) 122))) ; leap year

(ert-deftest timezone-tests-absolute-from-gregorian ()
  (should (equal (timezone-absolute-from-gregorian 1 1 2020) 737425)))

(provide 'timezone-tests)
;;; timezone-tests.el ends here
