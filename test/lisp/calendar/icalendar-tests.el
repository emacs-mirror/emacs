;;; icalendar-tests.el --- Test suite for icalendar.el  -*- lexical-binding:t -*-

;; Copyright (C) 2005, 2008-2026 Free Software Foundation, Inc.

;; Author:         Ulf Jasper <ulf.jasper@web.de>
;; Created:        March 2005
;; Keywords:       calendar
;; Human-Keywords: calendar, diary, iCalendar, vCalendar

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

;; TODO:
;; - Add more unit tests for functions, timezone etc.

;; Note: Watch the trailing blank that is added on import.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'icalendar)

;; ======================================================================
;; Helpers
;; ======================================================================

(defun icalendar-tests--get-ical-event (ical-string)
  "Return iCalendar event for ICAL-STRING."
  (save-excursion
    (with-temp-buffer
      (insert ical-string)
      (goto-char (point-min))
      (car (icalendar--read-element nil nil)))))

(defun icalendar-tests--trim (string)
  "Remove leading and trailing whitespace from STRING."
  (replace-regexp-in-string "[ \t\n]+\\'" ""
                            (replace-regexp-in-string "\\`[ \t\n]+" "" string)))

(defun icalendar-tests--get-file-contents (filename)
  "Return contents of file in test data directory named FILENAME."
  (with-temp-buffer
    (let ((coding-system-for-read 'raw-text)
          (inhibit-eol-conversion t))
      (insert-file-contents-literally
       (ert-resource-file filename))
      (buffer-string))))

(defun icalendar-tests--get-error-string-for-export (diary-string)
  "Call icalendar-export for DIARY-STRING and return resulting error-string."
  (ert-with-temp-file file
    :suffix "-export.ics"
    (with-temp-buffer
      (insert diary-string)
      (icalendar-export-region (point-min) (point-max) file))
    (with-current-buffer "*icalendar-errors*"
      (buffer-string))))

;; ======================================================================
;; Tests of functions
;; ======================================================================

(ert-deftest icalendar--create-uid ()
  "Test for `icalendar--create-uid'."
  (let* ((icalendar-uid-format "xxx-%c-%h-%u-%s")
         (icalendar--uid-count 77)
         (entry-full "30.06.1964 07:01 blahblah")
         (hash (format "%d" (abs (sxhash entry-full))))
         (contents "DTSTART:19640630T070100\nblahblah")
         (username (or user-login-name "UNKNOWN_USER")))
    (should (= 77 icalendar--uid-count))
    (should (string= (concat "xxx-77-" hash "-" username "-19640630")
                     (icalendar--create-uid entry-full contents)))
    (should (= 78 icalendar--uid-count))
    (setq contents "blahblah")
    (setq icalendar-uid-format "yyy%syyy")
    (should (string=  (concat "yyyDTSTARTyyy")
                      (icalendar--create-uid entry-full contents)))))

(ert-deftest icalendar-convert-anniversary-to-ical ()
  "Test method for `icalendar--convert-anniversary-to-ical'."
  (let* ((calendar-date-style 'iso)
         result)
    (setq result (icalendar--convert-anniversary-to-ical
                  "" "%%(diary-anniversary 1963 6 30) g"))
    (should (consp result))
    (should (string= (concat
                      "\nDTSTART;VALUE=DATE:19640630"
                      "\nDTEND;VALUE=DATE:19640701"
                      "\nRRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=06;BYMONTHDAY=30")
                     (car result)))
    (should (string= "g" (cdr result)))))

(ert-deftest icalendar--convert-cyclic-to-ical ()
  "Test method for `icalendar--convert-cyclic-to-ical'."
  (let* ((calendar-date-style 'iso)
         result)
    (setq result (icalendar--convert-block-to-ical
                  "" "%%(diary-block 2004 7 19 2004 8 27) Sommerferien"))
    (should (consp result))
    (should (string= (concat
                      "\nDTSTART;VALUE=DATE:20040719"
                      "\nDTEND;VALUE=DATE:20040828")
                     (car result)))
    (should (string= "Sommerferien" (cdr result)))))

(ert-deftest icalendar--convert-block-to-ical ()
  "Test method for `icalendar--convert-block-to-ical'."
  (let* ((calendar-date-style 'iso)
         result)
    (setq result (icalendar--convert-block-to-ical
                  "" "%%(diary-block 2004 7 19 2004 8 27) Sommerferien"))
    (should (consp result))
    (should (string= (concat
                      "\nDTSTART;VALUE=DATE:20040719"
                      "\nDTEND;VALUE=DATE:20040828")
                     (car result)))
    (should (string= "Sommerferien" (cdr result)))))

(ert-deftest icalendar--convert-float-to-ical ()
  "Test method for `icalendar--convert-float-to-ical'."
  ;; See Bug#78085
  (let* ((calendar-date-style 'iso)
         (icalendar-recurring-start-year 2025)
         (first-saturday-date "20250104") ; first Sat. in 2025
         result)
    (setq result (icalendar--convert-float-to-ical
                  "" "%%(diary-float t 6 1) 1st Sat/month"))
    (should (consp result))
    (should (string= (concat
                      "\nDTSTART;VALUE=DATE:" first-saturday-date
                      "\nRRULE:FREQ=MONTHLY;BYDAY=1SA")
                     (car result)))
    (should (string= "1st Sat/month" (cdr result)))))

(ert-deftest icalendar--convert-yearly-to-ical ()
  "Test method for `icalendar--convert-yearly-to-ical'."
  (let* ((calendar-date-style 'iso)
         result
         (calendar-month-name-array
          ["January" "February" "March" "April" "May" "June" "July" "August"
           "September" "October" "November" "December"]))
    (setq result (icalendar--convert-yearly-to-ical "" "May 1 Tag der Arbeit"))
    (should (consp result))
    (should (string= (concat
                      "\nDTSTART;VALUE=DATE:19000501"
                      "\nDTEND;VALUE=DATE:19000502"
                      "\nRRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=5;BYMONTHDAY=1")
                     (car result)))
    (should (string= "Tag der Arbeit" (cdr result)))))

(ert-deftest icalendar--convert-weekly-to-ical ()
  "Test method for `icalendar--convert-weekly-to-ical'."
  (let* ((calendar-date-style 'iso)
         result
         (calendar-day-name-array
          ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
           "Saturday"]))
    (setq result (icalendar--convert-weekly-to-ical "" "Monday 8:30 subject"))
    (should (consp result))
    (should (string= (concat "\nDTSTART;VALUE=DATE-TIME:20050103T083000"
                             "\nDTEND;VALUE=DATE-TIME:20050103T093000"
                             "\nRRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO")
                     (car result)))
    (should (string= "subject" (cdr result)))))

(ert-deftest icalendar--convert-sexp-to-ical ()
  "Test method for `icalendar--convert-sexp-to-ical'."
  (let* (result
         (icalendar-export-sexp-enumeration-days 3))
    ;; test case %%(diary-hebrew-date)
    (setq result (icalendar--convert-sexp-to-ical "" "%%(diary-hebrew-date)"))
    (should (consp result))
    (should (eq icalendar-export-sexp-enumeration-days (length result)))
    (mapc (lambda (i)
            (should (consp i))
            (should (string-match "Hebrew date (until sunset): .*" (cdr i))))
          result)))

(ert-deftest icalendar--convert-to-ical ()
  "Test method for `icalendar--convert-to-ical'."
  (let* (result
         (icalendar-export-sexp-enumerate-all t)
         (icalendar-export-sexp-enumeration-days 3)
         (calendar-date-style 'iso))
    ;; test case: %%(diary-anniversary 1642 12 25) Newton
    ;; forced enumeration not matching the actual day --> empty
    (setq result (icalendar--convert-sexp-to-ical
                  "" "%%(diary-anniversary 1642 12 25) Newton's birthday"
                  (encode-time 1 1 1 6 12 2014)))
    (should (null result))
    ;; test case: %%(diary-anniversary 1642 12 25) Newton
    ;; enumeration does match the actual day -->
    (setq result (icalendar--convert-sexp-to-ical
                  "" "%%(diary-anniversary 1642 12 25) Newton's birthday"
                  (encode-time 1 1 1 24 12 2014)))
    (should (= 1 (length result)))
    (should (consp (car result)))
    (should (string-match
             "\nDTSTART;VALUE=DATE:20141225\nDTEND;VALUE=DATE:20141226"
             (car (car result))))
    (should (string-match "Newton's birthday" (cdr (car result))))))

(ert-deftest icalendar--parse-vtimezone ()
  "Test method for `icalendar--parse-vtimezone'."
  (let (vtimezone result)
    ;; testcase: valid timezone with rrule
    (setq vtimezone (icalendar-tests--get-ical-event "BEGIN:VTIMEZONE
TZID:thename
BEGIN:STANDARD
DTSTART:16010101T040000
TZOFFSETFROM:+0300
TZOFFSETTO:+0200
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=-1SU;BYMONTH=10
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:16010101T030000
TZOFFSETFROM:+0200
TZOFFSETTO:+0300
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=-1SU;BYMONTH=3
END:DAYLIGHT
END:VTIMEZONE
"))
    (setq result (icalendar--parse-vtimezone vtimezone))
    (should (string= "thename" (car result)))
    (message (cdr result))
    (should (string= "STD-02:00DST-03:00,M3.5.0/03:00:00,M10.5.0/04:00:00"
                     (cdr result)))

    ;; testcase: name of tz contains comma
    (setq vtimezone (icalendar-tests--get-ical-event "BEGIN:VTIMEZONE
TZID:anothername, with a comma
BEGIN:STANDARD
DTSTART:16010101T040000
TZOFFSETFROM:+0300
TZOFFSETTO:+0200
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=2MO;BYMONTH=10
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:16010101T030000
TZOFFSETFROM:+0200
TZOFFSETTO:+0300
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=2MO;BYMONTH=3
END:DAYLIGHT
END:VTIMEZONE
"))
    (setq result (icalendar--parse-vtimezone vtimezone))
    (should (string= "anothername, with a comma" (car result)))
    (message (cdr result))
    (should (string= "STD-02:00DST-03:00,M3.2.1/03:00:00,M10.2.1/04:00:00"
                     (cdr result)))

    ;; testcase: offsetfrom = offsetto
    (setq vtimezone (icalendar-tests--get-ical-event "BEGIN:VTIMEZONE
TZID:Kolkata, Chennai, Mumbai, New Delhi
X-MICROSOFT-CDO-TZID:23
BEGIN:STANDARD
DTSTART:16010101T000000
TZOFFSETFROM:+0530
TZOFFSETTO:+0530
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:16010101T000000
TZOFFSETFROM:+0530
TZOFFSETTO:+0530
END:DAYLIGHT
END:VTIMEZONE
"))
    (setq result (icalendar--parse-vtimezone vtimezone))
    (should (string= "Kolkata, Chennai, Mumbai, New Delhi" (car result)))
    (message (cdr result))
    (should (string= "STD-05:30DST-05:30,M1.1.1/00:00:00,M1.1.1/00:00:00"
                     (cdr result)))

    ;; FIXME: add testcase that covers changes for fix of bug#34315
    ))

(ert-deftest icalendar--convert-ordinary-to-ical ()
  "Test method for `icalendar--convert-ordinary-to-ical'."
  (let* ((calendar-date-style 'iso)
         result)
    ;; without time
    (setq result (icalendar--convert-ordinary-to-ical "&?" "2010 2 15 subject"))
    (should (consp result))
    (should (string=  "\nDTSTART;VALUE=DATE:20100215\nDTEND;VALUE=DATE:20100216"
                      (car result)))
    (should (string= "subject" (cdr result)))

    ;; with start time
    (setq result (icalendar--convert-ordinary-to-ical
                  "&?" "&2010 2 15 12:34 s"))
    (should (consp result))
    (should (string=  (concat "\nDTSTART;VALUE=DATE-TIME:20100215T123400"
                              "\nDTEND;VALUE=DATE-TIME:20100215T133400")
                      (car result)))
    (should (string= "s" (cdr result)))

    ;; with time
    (setq result (icalendar--convert-ordinary-to-ical
                  "&?" "&2010 2 15 12:34-23:45 s"))
    (should (consp result))
    (should (string=  (concat "\nDTSTART;VALUE=DATE-TIME:20100215T123400"
                              "\nDTEND;VALUE=DATE-TIME:20100215T234500")
                      (car result)))
    (should (string= "s" (cdr result)))

    ;; with time, again -- test bug#5549
    (setq result (icalendar--convert-ordinary-to-ical
                  "x?" "x2010 2 15 0:34-1:45 s"))
    (should (consp result))
    (should (string=  (concat "\nDTSTART;VALUE=DATE-TIME:20100215T003400"
                              "\nDTEND;VALUE=DATE-TIME:20100215T014500")
                      (car result)))
    (should (string= "s" (cdr result)))))

(ert-deftest icalendar--diarytime-to-isotime ()
  "Test method for `icalendar--diarytime-to-isotime'."
  (should (string= "T011500"
		   (icalendar--diarytime-to-isotime "01:15" "")))
  (should (string= "T011500"
		   (icalendar--diarytime-to-isotime "1:15" "")))
  (should (string= "T000100"
		   (icalendar--diarytime-to-isotime "0:01" "")))
  (should (string= "T010000"
		   (icalendar--diarytime-to-isotime "0100" "")))
  (should (string= "T010000"
		   (icalendar--diarytime-to-isotime "0100" "am")))
  (should (string= "T130000"
		   (icalendar--diarytime-to-isotime "0100" "pm")))
  (should (string= "T120000"
		   (icalendar--diarytime-to-isotime "1200" "")))
  (should (string= "T171700"
		   (icalendar--diarytime-to-isotime "17:17" "")))
  (should (string= "T000000"
		   (icalendar--diarytime-to-isotime "1200" "am")))
  (should (string= "T000100"
		   (icalendar--diarytime-to-isotime "1201" "am")))
  (should (string= "T005900"
		   (icalendar--diarytime-to-isotime "1259" "am")))
  (should (string= "T120000"
		   (icalendar--diarytime-to-isotime "1200" "pm")))
  (should (string= "T120100"
		   (icalendar--diarytime-to-isotime "1201" "pm")))
  (should (string= "T125900"
		   (icalendar--diarytime-to-isotime "1259" "pm")))
  (should (string= "T150000"
		   (icalendar--diarytime-to-isotime "3" "pm"))))

(ert-deftest icalendar--datetime-to-diary-date ()
  "Test method for `icalendar--datetime-to-diary-date'."
  (let* ((datetime '(59 59 23 31 12 2008))
         (calendar-date-style 'iso))
    (should (string= "2008 12 31"
		     (icalendar--datetime-to-diary-date datetime)))
    (setq calendar-date-style 'european)
    (should (string= "31 12 2008"
		     (icalendar--datetime-to-diary-date datetime)))
    (setq calendar-date-style 'american)
    (should (string= "12 31 2008"
		     (icalendar--datetime-to-diary-date datetime)))))

(ert-deftest icalendar--datestring-to-isodate ()
  "Test method for `icalendar--datestring-to-isodate'."
  (let ((calendar-date-style 'iso))
    ;; numeric iso
    (should (string= "20080511"
		     (icalendar--datestring-to-isodate "2008 05 11")))
    (should (string= "20080531"
		     (icalendar--datestring-to-isodate "2008 05 31")))
    (should (string= "20080602"
		     (icalendar--datestring-to-isodate "2008 05 31" 2)))
    ;; Bug#69894
    (should (string= "20240319"
		     (icalendar--datestring-to-isodate "2024-03-19")))

    ;; numeric european
    (setq calendar-date-style 'european)
    (should (string= "20080511"
		     (icalendar--datestring-to-isodate "11 05 2008")))
    (should (string= "20080531"
		     (icalendar--datestring-to-isodate "31 05 2008")))
    (should (string= "20080602"
		     (icalendar--datestring-to-isodate "31 05 2008" 2)))

    ;; numeric american
    (setq calendar-date-style 'american)
    (should (string= "20081105"
		     (icalendar--datestring-to-isodate "11 05 2008")))
    (should (string= "20081230"
		     (icalendar--datestring-to-isodate "12 30 2008")))
    (should (string= "20090101"
		     (icalendar--datestring-to-isodate "12 30 2008" 2)))

    ;; non-numeric
    (setq calendar-date-style nil)      ;not necessary for conversion
    (should (string= "20081105"
		     (icalendar--datestring-to-isodate "Nov 05 2008")))
    (should (string= "20081105"
		     (icalendar--datestring-to-isodate "05 Nov 2008")))
    (should (string= "20081105"
		     (icalendar--datestring-to-isodate "2008 Nov 05")))

    ;; non-numeric with day-shift and year-shift
    (setq calendar-date-style nil)      ;not necessary for conversion
    (should (string= "20210212"
		     (icalendar--datestring-to-isodate "2021 Feb 11" 1)))
    (should (string= "20210131"
		     (icalendar--datestring-to-isodate "2021 Feb 11" -11)))
    (should (string= "20200211"
		     (icalendar--datestring-to-isodate "2021 Feb 11" nil -1)))
    (should (string= "21010211"
		     (icalendar--datestring-to-isodate "2021 Feb 11" nil 80)))
    ))

(ert-deftest icalendar--first-weekday-of-year ()
  "Test method for `icalendar-first-weekday-of-year'."
  (should (eq 1 (icalendar-first-weekday-of-year "TU" 2008)))
  (should (eq 3 (icalendar-first-weekday-of-year "WE" 2007)))
  (should (eq 5 (icalendar-first-weekday-of-year "TH" 2006)))
  (should (eq 7 (icalendar-first-weekday-of-year "FR" 2005)))
  (should (eq 3 (icalendar-first-weekday-of-year "SA" 2004)))
  (should (eq 5 (icalendar-first-weekday-of-year "SU" 2003)))
  (should (eq 7 (icalendar-first-weekday-of-year "MO" 2002)))
  (should (eq 3 (icalendar-first-weekday-of-year "MO" 2000)))
  (should (eq 1 (icalendar-first-weekday-of-year "TH" 1970))))

(ert-deftest icalendar--import-format-sample ()
  "Test method for `icalendar-import-format-sample'."
  (should (string= (concat "SUMMARY='a' DESCRIPTION='b' LOCATION='c' "
                           "ORGANIZER='d' STATUS='' URL='' CLASS=''")
		   (icalendar-import-format-sample
                    (icalendar-tests--get-ical-event "BEGIN:VEVENT
DTSTAMP:20030509T043439Z
DTSTART:20030509T103000
SUMMARY:a
ORGANIZER:d
LOCATION:c
DTEND:20030509T153000
DESCRIPTION:b
END:VEVENT
")))))

(ert-deftest icalendar--format-ical-event ()
  "Test `icalendar--format-ical-event'."
  (let ((icalendar-import-format "%s%d%l%o%t%u%c")
        (icalendar-import-format-summary "SUM %s")
        (icalendar-import-format-location " LOC %s")
        (icalendar-import-format-description " DES %s")
        (icalendar-import-format-organizer " ORG %s")
        (icalendar-import-format-status " STA %s")
        (icalendar-import-format-url " URL %s")
        (icalendar-import-format-class " CLA %s")
        (event (icalendar-tests--get-ical-event "BEGIN:VEVENT
DTSTAMP:20030509T043439Z
DTSTART:20030509T103000
SUMMARY:sum
ORGANIZER:org
LOCATION:loc
DTEND:20030509T153000
DESCRIPTION:des
END:VEVENT
")))
    (should (string= "SUM sum DES des LOC loc ORG org"
		     (icalendar--format-ical-event event)))
    (setq icalendar-import-format (lambda (&rest _ignore)
                                    "helloworld"))
    (should (string= "helloworld"  (icalendar--format-ical-event event)))
    (setq icalendar-import-format
          (lambda (event)
            (format "-%s-%s-%s-%s-%s-%s-%s-"
                    (icalendar--get-event-property event 'SUMMARY)
                    (icalendar--get-event-property event 'DESCRIPTION)
                    (icalendar--get-event-property event 'LOCATION)
                    (icalendar--get-event-property event 'ORGANIZER)
                    (icalendar--get-event-property event 'STATUS)
                    (icalendar--get-event-property event 'URL)
                    (icalendar--get-event-property event 'CLASS))))
    (should (string= "-sum-des-loc-org-nil-nil-nil-"
		     (icalendar--format-ical-event event)))))

(ert-deftest icalendar--parse-summary-and-rest ()
  "Test `icalendar--parse-summary-and-rest'."
  (let ((icalendar-import-format "%s%d%l%o%t%u%c")
        (icalendar-import-format-summary "SUM %s")
        (icalendar-import-format-location " LOC %s")
        (icalendar-import-format-description " DES %s")
        (icalendar-import-format-organizer " ORG %s")
        (icalendar-import-format-status " STA %s")
        (icalendar-import-format-url " URL %s")
        (icalendar-import-format-class " CLA %s")
        (result))
    (setq result (icalendar--parse-summary-and-rest "SUM sum ORG org"))
    (should (string= "org"  (cdr (assoc 'org result))))

    (setq result (icalendar--parse-summary-and-rest
                  "SUM sum DES des LOC loc ORG org STA sta URL url CLA cla"))
    (should (string= "des" (cdr (assoc 'des result))))
    (should (string= "loc" (cdr (assoc 'loc result))))
    (should (string= "org" (cdr (assoc 'org result))))
    (should (string= "sta" (cdr (assoc 'sta result))))
    (should (string= "cla" (cdr (assoc 'cla result))))

    (setq icalendar-import-format (lambda () "Hello world"))
    (setq result (icalendar--parse-summary-and-rest
                  "blah blah "))
    (should (not result))
    ))

(ert-deftest icalendar--decode-isodatetime ()
  "Test `icalendar--decode-isodatetime'."
  (let ((tz (getenv "TZ")))
    (unwind-protect
	(progn
	  ;; Use Eastern European Time (UTC+2, UTC+3 daylight saving)
	  (setenv "TZ" "EET-2EEST,M3.5.0/3,M10.5.0/4")

          (message "%s" (current-time-zone (encode-time 0 0 10 1 1 2013 0)))
          (message "%s" (current-time-zone (encode-time 0 0 10 1 8 2013 0)))

          ;; testcase: no time zone in input -> keep time as is
          ;; 1 Jan 2013 10:00
          (should (equal '(0 0 10 1 1 2013 2 nil 7200)
                         (icalendar--decode-isodatetime "20130101T100000")))
          ;; 1 Aug 2013 10:00 (DST)
          (should (equal '(0 0 10 1 8 2013 4 t 10800)
                         (icalendar--decode-isodatetime "20130801T100000")))

          ;; testcase: no time zone in input, shift by -1 days
          ;; 1 Jan 2013 10:00 -> 31 Dec 2012
          (should (equal '(0 0 10 31 12 2012 1 nil 7200)
                         (icalendar--decode-isodatetime "20130101T100000" -1)))
          ;; 1 Aug 2013 10:00 (DST) -> 31 Jul 2012 (DST)
          (should (equal '(0 0 10 31 7 2013 3 t 10800)
                         (icalendar--decode-isodatetime "20130801T100000" -1)))


          ;; testcase: UTC time zone specifier in input -> convert to local time
          ;; 31 Dec 2013 23:00 UTC -> 1 Jan 2014 01:00 EET
          (should (equal '(0 0 1 1 1 2014 3 nil 7200)
                         (icalendar--decode-isodatetime "20131231T230000Z")))
          ;; 1 Aug 2013 10:00 UTC -> 1 Aug 2013 13:00 EEST
          (should (equal '(0 0 13 1 8 2013 4 t 10800)
                         (icalendar--decode-isodatetime "20130801T100000Z")))

          ;; testcase: override timezone with Central European Time, 1 Jan 2013 10:00 -> 1 Jan 2013 11:00
          (should (equal '(0 0 11 1 1 2013 2 nil 7200)
                         (icalendar--decode-isodatetime "20130101T100000" nil
                                                        '(3600 "CET"))))
          ;; testcase: override timezone (UTC-02:00), 1 Jan 2013 10:00 -> 1 Jan 2013 14:00
          (should (equal '(0 0 14 1 1 2013 2 nil 7200)
                         (icalendar--decode-isodatetime "20130101T100000" nil -7200)))

          ;; FIXME: add testcase that covers changes for fix of bug#34315

          )
      ;; restore time-zone even if something went terribly wrong
      (setenv "TZ" tz))))

(ert-deftest icalendar--convert-tz-offset ()
  "Test `icalendar--convert-tz-offset'."
  (let ((tz (getenv "TZ")))
    (unwind-protect
	(progn
	  ;; Use Eastern European Time (UTC+2, UTC+3 daylight saving)
	  (setenv "TZ" "EET-2EEST,M3.5.0/3,M10.5.0/4")

          ;; testcase: artificial input
          (should (equal '("DST-03:00" . "M5.1.1/01:23:45")
                         (icalendar--convert-tz-offset
                          '((DTSTART nil "________T012345") ;
                            (TZOFFSETFROM nil "+0200")
                            (TZOFFSETTO nil "+0300")
                            (RRULE nil "FREQ=YEARLY;INTERVAL=1;BYDAY=1MO;BYMONTH=5"))
                          t)))

          ;; testcase: Europe/Berlin Standard
          (should (equal '("STD-01:00" . "M10.5.0/03:00:00")
                         (icalendar--convert-tz-offset
                          '((TZOFFSETFROM nil "+0200")
                            (TZOFFSETTO nil "+0100")
                            (TZNAME nil CET)
                            (DTSTART nil "19701025T030000")
                            (RRULE nil "FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU"))
                          nil)))

          ;; testcase: Europe/Berlin DST
          (should (equal '("DST-02:00" . "M3.5.0/02:00:00")
                         (icalendar--convert-tz-offset
                          '((TZOFFSETFROM nil "+0100")
                            (TZOFFSETTO nil "+0200")
                            (TZNAME nil CEST)
                            (DTSTART nil "19700329T020000")
                            (RRULE nil "FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU"))
                          t)))

          ;; testcase: dtstart is mandatory
          (should (null (icalendar--convert-tz-offset
                         '((TZOFFSETFROM nil "+0100")
                           (TZOFFSETTO nil "+0200")
                           (RRULE nil "FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU"))
                         t)))

          ;; FIXME: rrule and rdate are NOT mandatory!  Must fix code
          ;; before activating these testcases
          ;; ;; testcase: no rrule and no rdate => no result
          ;; (should (null (icalendar--convert-tz-offset
          ;;                 '((TZOFFSETFROM nil "+0100")
          ;;                   (TZOFFSETTO nil "+0200")
          ;;                   (DTSTART nil "19700329T020000"))
          ;;                 t)))
          ;; ;; testcase: no rrule with rdate => no result
          ;; (should (null (icalendar--convert-tz-offset
          ;;                 '((TZOFFSETFROM nil "+0100")
          ;;                   (TZOFFSETTO nil "+0200")
          ;;                   (DTSTART nil "18840101T000000")
          ;;                   (RDATE nil "18840101T000000"))
          ;;                 t)))
          )
      ;; restore time-zone even if something went terribly wrong
      (setenv "TZ" tz))))

(ert-deftest icalendar--decode-isoduration ()
  "Test `icalendar--decode-isoduration'."

  ;; testcase: 7 days
  (should (equal '(0 0 0 7 0 0)
                 (icalendar--decode-isoduration "P7D")))

  ;; testcase: 7 days, one second -- see bug#34315
  (should (equal '(1 0 0 7 0 0)
                 (icalendar--decode-isoduration "P7DT1S")))

  ;; testcase: 3 hours, 2 minutes, one second
  (should (equal '(1 2 3 0 0 0)
                 (icalendar--decode-isoduration "PT3H2M1S")))

  ;; testcase: 99 days, 3 hours, 2 minutes, one second  -- see bug#34315
  (should (equal '(1 2 3 99 0 0)
                 (icalendar--decode-isoduration "P99DT3H2M1S")))

  ;; testcase: 2 weeks
  (should (equal '(0 0 0 14 0 0)
                 (icalendar--decode-isoduration "P2W")))

  ;; testcase: rfc2445, section 4.3.6: 15 days, 5 hours and 20 seconds  -- see bug#34315
  (should (equal '(20 0 5 15 0 0)
                 (icalendar--decode-isoduration "P15DT5H0M20S")))

  ;; testcase: rfc2445, section 4.3.6: 7 weeks
  (should (equal '(0 0 0 49 0 0)
                 (icalendar--decode-isoduration "P7W")))
  )


;; ======================================================================
;; Export tests
;; ======================================================================

(defun icalendar-tests--test-export (input-iso input-european input-american
                                               expected-output &optional alarms)
  "Perform an export test.
Argument INPUT-ISO iso style diary string.
Argument INPUT-EUROPEAN european style diary string.
Argument INPUT-AMERICAN american style diary string.
Argument EXPECTED-OUTPUT expected iCalendar result string.
Optional argument ALARMS the value of `icalendar-export-alarms' for this test.

European style input data must use German month names.  American
and ISO style input data must use English month names."
  (let ((tz (getenv "TZ"))
	(calendar-date-style 'iso)
	(icalendar-recurring-start-year 2000)
        (icalendar-export-alarms alarms))
    (unwind-protect
	(progn
;;;	  (message "Current time zone: %s" (current-time-zone))
	  ;; Use this form so as not to rely on system tz database.
	  ;; Eg hydra.nixos.org.
	  (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
;;;	  (message "Current time zone: %s" (current-time-zone))
	  (when input-iso
	    (let ((calendar-month-name-array
		   ["January" "February" "March" "April" "May" "June" "July" "August"
		    "September" "October" "November" "December"])
		  (calendar-day-name-array
		   ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
		    "Saturday"]))
	      (setq calendar-date-style 'iso)
	      (icalendar-tests--do-test-export input-iso expected-output)))
	  (when input-european
	    (let ((calendar-month-name-array
		   ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August"
		    "September" "Oktober" "November" "Dezember"])
		  (calendar-day-name-array
		   ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag"
		    "Samstag"]))
	      (setq calendar-date-style 'european)
	      (icalendar-tests--do-test-export input-european expected-output)))
	  (when input-american
	    (let ((calendar-month-name-array
		   ["January" "February" "March" "April" "May" "June" "July" "August"
		    "September" "October" "November" "December"])
		  (calendar-day-name-array
		   ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
		    "Saturday"]))
	      (setq calendar-date-style 'american)
	      (icalendar-tests--do-test-export input-american expected-output))))
      ;; restore time-zone even if something went terribly wrong
      (setenv "TZ" tz))))

(defun icalendar-tests--do-test-export (input expected-output)
  "Actually perform export test.
Argument INPUT input diary string.
Argument EXPECTED-OUTPUT expected iCalendar result string."
  (ert-with-temp-file temp-file
    :suffix "icalendar-tests-ics"
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert input)
            (icalendar-export-region (point-min) (point-max) temp-file))
          (save-excursion
            (find-file temp-file)
            (goto-char (point-min))
            (cond (expected-output
                   (should (re-search-forward "^\\s-*BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0
BEGIN:VEVENT
UID:emacs[0-9]+
\\(\\(.\\|\n\\)+\\)
END:VEVENT
END:VCALENDAR
\\s-*$"
                                              nil t))
                   (should (string-match
                            (concat "^\\s-*"
                                    (regexp-quote (buffer-substring-no-properties
                                                   (match-beginning 1) (match-end 1)))
                                    "\\s-*$")
                            expected-output)))
                  (t
                   (should (re-search-forward "^\\s-*BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0
END:VCALENDAR
\\s-*$"
                                              nil t))))))
      ;; cleanup!!
      (kill-buffer (find-buffer-visiting temp-file)))))

(ert-deftest icalendar-export-ordinary-no-time ()
  "Perform export test."

  (let ((icalendar-export-hidden-diary-entries nil))
    (icalendar-tests--test-export
     "&2000 Oct 3 ordinary no time "
     "&3 Okt 2000 ordinary no time "
     "&Oct 3 2000 ordinary no time "
     nil))

  (icalendar-tests--test-export
   "2000 Oct 3 ordinary no time "
   "3 Okt 2000 ordinary no time "
   "Oct 3 2000 ordinary no time "
   "DTSTART;VALUE=DATE:20001003
DTEND;VALUE=DATE:20001004
SUMMARY:ordinary no time
"))

(ert-deftest icalendar-export-ordinary ()
  "Perform export test."

  (icalendar-tests--test-export
   "2000 Oct 3 16:30 ordinary with time"
   "3 Okt 2000 16:30 ordinary with time"
   "Oct 3 2000 16:30 ordinary with time"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:ordinary with time
")
  (icalendar-tests--test-export
   "2000 10 3 16:30 ordinary with time 2"
   "3 10 2000 16:30 ordinary with time 2"
   "10 3 2000 16:30 ordinary with time 2"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:ordinary with time 2
")

  (icalendar-tests--test-export
   "2000/10/3 16:30 ordinary with time 3"
   "3/10/2000 16:30 ordinary with time 3"
   "10/3/2000 16:30 ordinary with time 3"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:ordinary with time 3
"))

(ert-deftest icalendar-export-multiline ()
  "Perform export test."

  ;; multiline -- FIXME!!!
  (icalendar-tests--test-export
   "2000 October 3 16:30 multiline
  17:30 multiline continued FIXME"
   "3 Oktober 2000 16:30 multiline
  17:30 multiline continued FIXME"
   "October 3 2000 16:30 multiline
  17:30 multiline continued FIXME"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:multiline
DESCRIPTION:
  17:30 multiline continued FIXME
"))

(ert-deftest icalendar-export-weekly-by-day ()
  "Perform export test."

  ;; weekly by day
  (icalendar-tests--test-export
   "Monday 1:30pm weekly by day with start time"
   "Montag 13:30 weekly by day with start time"
   "Monday 1:30pm weekly by day with start time"
   "DTSTART;VALUE=DATE-TIME:20000103T133000
DTEND;VALUE=DATE-TIME:20000103T143000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:weekly by day with start time
")

  (icalendar-tests--test-export
   "Monday 13:30-15:00 weekly by day with start and end time"
   "Montag 13:30-15:00 weekly by day with start and end time"
   "Monday 01:30pm-03:00pm weekly by day with start and end time"
   "DTSTART;VALUE=DATE-TIME:20000103T133000
DTEND;VALUE=DATE-TIME:20000103T150000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:weekly by day with start and end time
"))

(ert-deftest icalendar-export-yearly ()
  "Perform export test."
  ;; yearly
  (icalendar-tests--test-export
   "may 1 yearly no time"
   "1 Mai yearly no time"
   "may 1 yearly no time"
   "DTSTART;VALUE=DATE:19000501
DTEND;VALUE=DATE:19000502
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=5;BYMONTHDAY=1
SUMMARY:yearly no time
"))

(ert-deftest icalendar-export-anniversary ()
  "Perform export test."
  ;; anniversaries
  (icalendar-tests--test-export
   "%%(diary-anniversary 1988 10 3) anniversary no time"
   "%%(diary-anniversary 3 10 1988) anniversary no time"
   "%%(diary-anniversary 10 3 1988) anniversary no time"
   "DTSTART;VALUE=DATE:19891003
DTEND;VALUE=DATE:19891004
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=10;BYMONTHDAY=03
SUMMARY:anniversary no time
")
  (icalendar-tests--test-export
   "%%(diary-anniversary 1988 10 3) 19:00-20:00 anniversary with time"
   "%%(diary-anniversary 3 10 1988) 19:00-20:00 anniversary with time"
   "%%(diary-anniversary 10 3 1988) 19:00-20:00 anniversary with time"
   "DTSTART;VALUE=DATE-TIME:19891003T190000
DTEND;VALUE=DATE-TIME:19891004T200000
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=10;BYMONTHDAY=03
SUMMARY:anniversary with time
"))

(ert-deftest icalendar-export-block ()
  "Perform export test."
  ;; block
  (icalendar-tests--test-export
   "%%(diary-block 2001 6 18 2001 7 6) block no time"
   "%%(diary-block 18 6 2001 6 7 2001) block no time"
   "%%(diary-block 6 18 2001 7 6 2001) block no time"
   "DTSTART;VALUE=DATE:20010618
DTEND;VALUE=DATE:20010707
SUMMARY:block no time
")
  (icalendar-tests--test-export
   "%%(diary-block 2001 6 18 2001 7 6) 13:00-17:00 block with time"
   "%%(diary-block 18 6 2001 6 7 2001) 13:00-17:00 block with time"
   "%%(diary-block 6 18 2001 7 6 2001) 13:00-17:00 block with time"
   "DTSTART;VALUE=DATE-TIME:20010618T130000
DTEND;VALUE=DATE-TIME:20010618T170000
RRULE:FREQ=DAILY;INTERVAL=1;UNTIL=20010706
SUMMARY:block with time
")
  (icalendar-tests--test-export
   "%%(diary-block 2001 6 18 2001 7 6) 13:00 block no end time"
   "%%(diary-block 18 6 2001 6 7 2001) 13:00 block no end time"
   "%%(diary-block 6 18 2001 7 6 2001) 13:00 block no end time"
   "DTSTART;VALUE=DATE-TIME:20010618T130000
DTEND;VALUE=DATE-TIME:20010618T140000
RRULE:FREQ=DAILY;INTERVAL=1;UNTIL=20010706
SUMMARY:block no end time
"))

(ert-deftest icalendar-export-alarms ()
  "Perform export test with different settings for exporting alarms."
  ;; no alarm
  (icalendar-tests--test-export
   "2014 Nov 17 19:30 no alarm"
   "17 Nov 2014 19:30 no alarm"
   "Nov 17 2014 19:30 no alarm"
   "DTSTART;VALUE=DATE-TIME:20141117T193000
DTEND;VALUE=DATE-TIME:20141117T203000
SUMMARY:no alarm
"
   nil)

  ;; 10 minutes in advance, audio
  (icalendar-tests--test-export
   "2014 Nov 17 19:30 audio alarm"
   "17 Nov 2014 19:30 audio alarm"
   "Nov 17 2014 19:30 audio alarm"
   "DTSTART;VALUE=DATE-TIME:20141117T193000
DTEND;VALUE=DATE-TIME:20141117T203000
SUMMARY:audio alarm
BEGIN:VALARM
ACTION:AUDIO
TRIGGER:-PT10M
END:VALARM
"
   '(10 ((audio))))

  ;; 20 minutes in advance, display
  (icalendar-tests--test-export
   "2014 Nov 17 19:30 display alarm"
   "17 Nov 2014 19:30 display alarm"
   "Nov 17 2014 19:30 display alarm"
   "DTSTART;VALUE=DATE-TIME:20141117T193000
DTEND;VALUE=DATE-TIME:20141117T203000
SUMMARY:display alarm
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER:-PT20M
DESCRIPTION:display alarm
END:VALARM
"
   '(20 ((display))))

  ;; 66 minutes in advance, email
  (icalendar-tests--test-export
   "2014 Nov 17 19:30 email alarm"
   "17 Nov 2014 19:30 email alarm"
   "Nov 17 2014 19:30 email alarm"
   "DTSTART;VALUE=DATE-TIME:20141117T193000
DTEND;VALUE=DATE-TIME:20141117T203000
SUMMARY:email alarm
BEGIN:VALARM
ACTION:EMAIL
TRIGGER:-PT66M
DESCRIPTION:email alarm
SUMMARY:email alarm
ATTENDEE:MAILTO:att.one@email.com
ATTENDEE:MAILTO:att.two@email.com
END:VALARM
"
   '(66 ((email ("att.one@email.com" "att.two@email.com")))))

  ;; 2 minutes in advance, all alarms
  (icalendar-tests--test-export
   "2014 Nov 17 19:30 all alarms"
   "17 Nov 2014 19:30 all alarms"
   "Nov 17 2014 19:30 all alarms"
   "DTSTART;VALUE=DATE-TIME:20141117T193000
DTEND;VALUE=DATE-TIME:20141117T203000
SUMMARY:all alarms
BEGIN:VALARM
ACTION:EMAIL
TRIGGER:-PT2M
DESCRIPTION:all alarms
SUMMARY:all alarms
ATTENDEE:MAILTO:att.one@email.com
ATTENDEE:MAILTO:att.two@email.com
END:VALARM
BEGIN:VALARM
ACTION:AUDIO
TRIGGER:-PT2M
END:VALARM
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER:-PT2M
DESCRIPTION:all alarms
END:VALARM
"
   '(2 ((email ("att.one@email.com" "att.two@email.com")) (audio) (display)))))

;; ======================================================================
;; #bug56241
;; ======================================================================
(defun icalendar-tests--diary-float (&rest args)
  (apply #'diary-float args))

(ert-deftest icalendar-export-bug-56241-dotted-pair ()
  "See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56241#5"
  ;; This test started failing early July 2023 without any apparent change
  ;; to the underlying code, so is probably sensitive to the current date.
  :tags '(:unstable)
  (let ((icalendar-export-sexp-enumeration-days 366))
    (mapc (lambda (diary-string)
            (should (string= "" (icalendar-tests--get-error-string-for-export
                                 diary-string))))
          '("%%(diary-float 7 0 1) First Sunday in July 1"
            "%%(icalendar-tests--diary-float 7 0 1) First Sunday in July 2"))))


;; (ert-deftest icalendar-export-bug-56241-sexp-does-not-match ()
;;   "Reported in #bug56241 -- needs to be fixed!"
;;   (let ((icalendar-export-sexp-enumeration-days 0))
;;     (mapc (lambda (diary-string)
;;             (should (string= "" (icalendar-tests--get-error-string-for-export
;;                                  diary-string))))
;;           '("%%(diary-float 7 0 1) First Sunday in July 1"
;;             "%%(icalendar-tests--diary-float 7 0 1) First Sunday in July 2"))))

(ert-deftest icalendar-export-bug-56241-nested-sexps ()
  "Reported in #bug56241 -- needs to be fixed!"
  (let ((icalendar-export-sexp-enumeration-days 366))
    (mapc (lambda (diary-string)
            (should (string= "" (icalendar-tests--get-error-string-for-export
                                 diary-string))))
          '("%%(= (calendar-day-of-week date) 0) Sunday 1"
            "%%(= 0 (calendar-day-of-week date)) Sunday 2"))))

;; ======================================================================
;; Import tests
;; ======================================================================

(defun icalendar-tests--test-import (filename expected-iso expected-european
					      expected-american)
  "Perform import test.
Argument FILENAME ics file to import.
Argument EXPECTED-ISO diary-file containing expected
iso-calendar-style result.
Argument EXPECTED-EUROPEAN diary-file containing expected
european-calendar-style result.
Argument EXPECTED-AMERICAN diary-file containing expected
american-calendar-style result.
During import test the timezone is set to Central European Time."
  (let ((timezone (getenv "TZ")))
    (unwind-protect
	(progn
	  ;; Use this form so as not to rely on system tz database.
	  ;; Eg hydra.nixos.org.
	  (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
	  (with-temp-buffer
	    (insert (icalendar-tests--get-file-contents filename))
	    (let ((icalendar-import-format "%s%d%l%o%t%u%c%U")
		  (icalendar-import-format-summary "%s")
		  (icalendar-import-format-location "\n Location: %s")
		  (icalendar-import-format-description "\n Desc: %s")
		  (icalendar-import-format-organizer "\n Organizer: %s")
		  (icalendar-import-format-status "\n Status: %s")
		  (icalendar-import-format-url "\n URL: %s")
		  (icalendar-import-format-class "\n Class: %s")
		  (icalendar-import-format-uid "\n UID: %s")
		  calendar-date-style)
	      (when expected-iso
		(setq calendar-date-style 'iso)
		(icalendar-tests--do-test-import
                 (icalendar-tests--get-file-contents expected-iso)))
	      (when expected-european
		(setq calendar-date-style 'european)
		(icalendar-tests--do-test-import
                 (icalendar-tests--get-file-contents expected-european)))
	      (when expected-american
		(setq calendar-date-style 'american)
		(icalendar-tests--do-test-import
                 (icalendar-tests--get-file-contents expected-american))))))
      (setenv "TZ" timezone))))

(defun icalendar-tests--do-test-import (expected-output)
  "Actually perform import test.
Argument EXPECTED-OUTPUT file containing expected diary string."
  (ert-with-temp-file temp-file
    :suffix "icalendar-test-diary"
    ;; Test the Catch-the-mysterious-coding-header logic below.
    ;; Ruby-mode adds an after-save-hook which inserts the header!
    ;; (save-excursion
    ;;   (find-file temp-file)
    ;;   (ruby-mode))
    (let ((coding-system-for-write 'raw-text))
      (icalendar-import-buffer temp-file t t))
    (save-excursion
      (find-file temp-file)
      ;; Check for the mysterious "# coding: ..." header, remove it
      ;; and give a shout
      (goto-char (point-min))
      (when (re-search-forward "# coding: .*?\n" nil t)
        (message (concat "%s\n"
                         "Found mysterious \"# coding ...\" header!  Removing it.\n"
                         "Current Modes: %s, %s\n"
                         "Current test: %s\n"
                         "%s")
                 (make-string 70 ?*)
                 major-mode
                 minor-mode-list
                 (ert-running-test)
                 (make-string 70 ?*))
        (buffer-disable-undo)
        (replace-match "")
        (set-buffer-modified-p nil))

      (let ((result (buffer-substring-no-properties (point-min) (point-max))))
        (should (string= expected-output result)))
      (kill-buffer (find-buffer-visiting temp-file)))))

(ert-deftest icalendar-import-non-recurring ()
  "Perform standard import tests."
  (icalendar-tests--test-import "import-non-recurring-1.ics"
                                "import-non-recurring-1.diary-iso"
                                "import-non-recurring-1.diary-european"
                                "import-non-recurring-1.diary-american")
  (icalendar-tests--test-import "import-non-recurring-all-day.ics"
                                "import-non-recurring-all-day.diary-iso"
                                "import-non-recurring-all-day.diary-european"
                                "import-non-recurring-all-day.diary-american")
  (icalendar-tests--test-import "import-non-recurring-long-summary.ics"
                                "import-non-recurring-long-summary.diary-iso"
                                "import-non-recurring-long-summary.diary-european"
                                "import-non-recurring-long-summary.diary-american")
  (icalendar-tests--test-import "import-non-recurring-block.ics"
                                "import-non-recurring-block.diary-iso"
                                "import-non-recurring-block.diary-european"
                                "import-non-recurring-block.diary-american")
  (icalendar-tests--test-import "import-non-recurring-folded-summary.ics"
                                "import-non-recurring-folded-summary.diary-iso"
                                "import-non-recurring-folded-summary.diary-european"
                                "import-non-recurring-folded-summary.diary-american")
  (icalendar-tests--test-import "import-non-recurring-another-example.ics"
                                "import-non-recurring-another-example.diary-iso"
                                "import-non-recurring-another-example.diary-european"
                                "import-non-recurring-another-example.diary-american"))


(ert-deftest icalendar-import-rrule ()
  (icalendar-tests--test-import "import-rrule-daily.ics"
                                "import-rrule-daily.diary-iso"
                                "import-rrule-daily.diary-european"
                                "import-rrule-daily.diary-american")
  (icalendar-tests--test-import "import-rrule-daily-two-day.ics"
                                "import-rrule-daily-two-day.diary-iso"
                                "import-rrule-daily-two-day.diary-european"
                                "import-rrule-daily-two-day.diary-american")
  (icalendar-tests--test-import "import-rrule-daily-with-exceptions.ics"
                                "import-rrule-daily-with-exceptions.diary-iso"
                                "import-rrule-daily-with-exceptions.diary-european"
                                "import-rrule-daily-with-exceptions.diary-american")
  (icalendar-tests--test-import "import-rrule-weekly.ics"
                                "import-rrule-weekly.diary-iso"
                                "import-rrule-weekly.diary-european"
                                "import-rrule-weekly.diary-american")
  (icalendar-tests--test-import "import-rrule-monthly-no-end.ics"
                                "import-rrule-monthly-no-end.diary-iso"
                                "import-rrule-monthly-no-end.diary-european"
                                "import-rrule-monthly-no-end.diary-american")
  (icalendar-tests--test-import "import-rrule-monthly-with-end.ics"
                                "import-rrule-monthly-with-end.diary-iso"
                                "import-rrule-monthly-with-end.diary-european"
                                "import-rrule-monthly-with-end.diary-american")
  (icalendar-tests--test-import "import-rrule-anniversary.ics"
                                "import-rrule-anniversary.diary-iso"
                                "import-rrule-anniversary.diary-european"
                                "import-rrule-anniversary.diary-american")
  (icalendar-tests--test-import "import-rrule-yearly.ics"
                                "import-rrule-yearly.diary-iso"
                                "import-rrule-yearly.diary-european"
                                "import-rrule-yearly.diary-american")
  (icalendar-tests--test-import "import-rrule-count-daily-short.ics"
                                "import-rrule-count-daily-short.diary-iso"
                                "import-rrule-count-daily-short.diary-european"
                                "import-rrule-count-daily-short.diary-american")
  (icalendar-tests--test-import "import-rrule-count-daily-long.ics"
                                "import-rrule-count-daily-long.diary-iso"
                                "import-rrule-count-daily-long.diary-european"
                                "import-rrule-count-daily-long.diary-american")
  (icalendar-tests--test-import "import-rrule-count-monthly.ics"
                                "import-rrule-count-monthly.diary-iso"
                                "import-rrule-count-monthly.diary-european"
                                "import-rrule-count-monthly.diary-american")
  (icalendar-tests--test-import "import-rrule-count-every-second-month.ics"
                                "import-rrule-count-every-second-month.diary-iso"
                                "import-rrule-count-every-second-month.diary-european"
                                "import-rrule-count-every-second-month.diary-american")
  (icalendar-tests--test-import "import-rrule-count-yearly.ics"
                                "import-rrule-count-yearly.diary-iso"
                                "import-rrule-count-yearly.diary-european"
                                "import-rrule-count-yearly.diary-american")
  (icalendar-tests--test-import "import-rrule-count-every-second-year.ics"
                                "import-rrule-count-every-second-year.diary-iso"
                                "import-rrule-count-every-second-year.diary-european"
                                "import-rrule-count-every-second-year.diary-american")
  )

(ert-deftest icalendar-import-duration ()
  (icalendar-tests--test-import "import-duration.ics"
                                "import-duration.diary-iso"
                                "import-duration.diary-european"
                                "import-duration.diary-american")
  ;; duration-2: this is actually an rrule test
  (icalendar-tests--test-import "import-duration-2.ics"
                                "import-duration-2.diary-iso"
                                "import-duration-2.diary-european"
                                "import-duration-2.diary-american"))

(ert-deftest icalendar-import-bug-6766 ()
  ;;bug#6766 -- multiple byday values in a weekly rrule
  (icalendar-tests--test-import "import-bug-6766.ics"
                                "import-bug-6766.diary-iso"
                                "import-bug-6766.diary-european"
                                "import-bug-6766.diary-american"))

(ert-deftest icalendar-import-bug-24199 ()
  ;;bug#24199 -- monthly rule with byday-clause
  (icalendar-tests--test-import "import-bug-24199.ics"
                                "import-bug-24199.diary-iso"
                                "import-bug-24199.diary-european"
                                "import-bug-24199.diary-american"))

(ert-deftest icalendar-import-bug-33277 ()
  ;;bug#33277 -- start time equals end time
  (icalendar-tests--test-import "import-bug-33277.ics"
                                "import-bug-33277.diary-iso"
                                "import-bug-33277.diary-european"
                                "import-bug-33277.diary-american"))

(ert-deftest icalendar-import-multiple-vcalendars ()
  (icalendar-tests--test-import "import-multiple-vcalendars.ics"
                                "import-multiple-vcalendars.diary-iso"
                                "import-multiple-vcalendars.diary-european"
                                "import-multiple-vcalendars.diary-american"))

(ert-deftest icalendar-import-with-uid ()
  "Perform import test with uid."
  (icalendar-tests--test-import "import-with-uid.ics"
                                "import-with-uid.diary-iso"
                                "import-with-uid.diary-european"
                                "import-with-uid.diary-american"))

(ert-deftest icalendar-import-with-timezone ()
  ;; This is known to fail on MS-Windows, because the test assumes
  ;; Posix features of specifying DST rules.
  :expected-result (if (memq system-type '(windows-nt ms-dos))
                       :failed
                     :passed)
  ;; bug#11473
  ;; "standardtime" begins first sunday in january and is 4 hours behind CET
  ;; "daylightsavingtime" begins first sunday in november and is 1 hour before CET
  (icalendar-tests--test-import "import-with-timezone.ics"
                                "import-with-timezone.diary-iso"
                                nil
                                nil))

;; ======================================================================
;; Cycle
;; ======================================================================
(defun icalendar-tests--test-cycle (input)
  "Perform cycle test.
Argument INPUT icalendar event string."
  (with-temp-buffer
    (if (string-match "^BEGIN:VCALENDAR" input)
        (insert input)
      (insert "BEGIN:VCALENDAR\nPRODID:-//Emacs//NONSGML icalendar.el//EN\n")
      (insert "VERSION:2.0\nBEGIN:VEVENT\n")
      (insert input)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (insert "END:VEVENT\nEND:VCALENDAR\n"))
    (let ((icalendar-import-format "%s%d%l%o%t%u%c%U")
          (icalendar-import-format-summary "%s")
          (icalendar-import-format-location "\n Location: %s")
          (icalendar-import-format-description "\n Desc: %s")
          (icalendar-import-format-organizer "\n Organizer: %s")
          (icalendar-import-format-status "\n Status: %s")
          (icalendar-import-format-url "\n URL: %s")
          (icalendar-import-format-class "\n Class: %s")
          (icalendar-import-format-class "\n UID: %s")
          (icalendar-export-alarms nil))
      (dolist (calendar-date-style '(iso european american))
        (icalendar-tests--do-test-cycle)))))

(defun icalendar-tests--do-test-cycle ()
  "Actually perform import/export cycle test."
  (ert-with-temp-file temp-diary
    (ert-with-temp-file temp-ics
      (let ((org-input (buffer-substring-no-properties (point-min) (point-max))))

        (unwind-protect
            (progn
              ;; step 1: import
              (icalendar-import-buffer temp-diary t t)

              ;; step 2: export what was just imported
              (save-excursion
                (find-file temp-diary)
                (icalendar-export-region (point-min) (point-max) temp-ics))

              ;; compare the output of step 2 with the input of step 1
              (save-excursion
                (find-file temp-ics)
                (goto-char (point-min))
                ;;(when (re-search-forward "\nUID:.*\n" nil t)
                ;;(replace-match "\n"))
                (let ((cycled (buffer-substring-no-properties (point-min) (point-max))))
                  (should (string= org-input cycled)))))
          ;; clean up
          (kill-buffer (find-buffer-visiting temp-diary))
          (with-current-buffer (find-buffer-visiting temp-ics)
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer))))))))

(ert-deftest icalendar-cycle ()
  "Perform cycling tests.
Take care to avoid auto-generated UIDs here."
  (icalendar-tests--test-cycle
   "UID:dummyuid
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
SUMMARY:Cycletest
")
  (icalendar-tests--test-cycle
   "UID:blah
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
SUMMARY:Cycletest
DESCRIPTION:beschreibung!
LOCATION:nowhere
ORGANIZER:ulf
")
  (icalendar-tests--test-cycle
   "UID:4711
DTSTART;VALUE=DATE:19190909
DTEND;VALUE=DATE:19190910
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=09;BYMONTHDAY=09
SUMMARY:and diary-anniversary
"))

;; ======================================================================
;; Real world
;; ======================================================================
(ert-deftest icalendar-real-world ()
  "Perform real-world tests, as gathered from problem reports."
  ;; This is known to fail on MS-Windows, since it doesn't support DST
  ;; specification with month and day.
  :expected-result (if (memq system-type '(windows-nt ms-dos))
                       :failed
                     :passed)
  ;; 2003-05-29
  (icalendar-tests--test-import "import-real-world-2003-05-29.ics"
                                nil
                                "import-real-world-2003-05-29.diary-european"
                                "import-real-world-2003-05-29.diary-american")

  ;; created with https://apps.marudot.com/ical/
  (icalendar-tests--test-import "import-real-world-no-dst.ics"
                                nil
                                "import-real-world-no-dst.diary-european"
                                "import-real-world-no-dst.diary-american")

  ;; 2003-06-18 a
  (icalendar-tests--test-import "import-real-world-2003-06-18a.ics"
                                nil
                                "import-real-world-2003-06-18a.diary-european"
                                "import-real-world-2003-06-18a.diary-american")
  ;; 2003-06-18 b -- uses timezone
  (icalendar-tests--test-import "import-real-world-2003-06-18b.ics"
                                nil
                                "import-real-world-2003-06-18b.diary-european"
                                "import-real-world-2003-06-18b.diary-american")
  ;; export 2004-10-28 block entries
  (icalendar-tests--test-export
   nil
   nil
   "-*- mode: text; fill-column: 256;-*-

>>>  block entries:

%%(diary-block 11 8 2004 11 10 2004) Nov 8-10 aa
"
   "DTSTART;VALUE=DATE:20041108
DTEND;VALUE=DATE:20041111
SUMMARY:Nov 8-10 aa")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-block 12 13 2004 12 17 2004) Dec 13-17 bb"
   "DTSTART;VALUE=DATE:20041213
DTEND;VALUE=DATE:20041218
SUMMARY:Dec 13-17 bb")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-block 2 3 2005 2 4 2005) Feb 3-4 cc"
   "DTSTART;VALUE=DATE:20050203
DTEND;VALUE=DATE:20050205
SUMMARY:Feb 3-4 cc")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-block 4 24 2005 4 29 2005) April 24-29 dd"
   "DTSTART;VALUE=DATE:20050424
DTEND;VALUE=DATE:20050430
SUMMARY:April 24-29 dd
")
  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-block 5 30 2005 6 1 2005) may 30 - June 1: ee"
   "DTSTART;VALUE=DATE:20050530
DTEND;VALUE=DATE:20050602
SUMMARY:may 30 - June 1: ee")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-block 6 6 2005 6 8 2005) ff"
   "DTSTART;VALUE=DATE:20050606
DTEND;VALUE=DATE:20050609
SUMMARY:ff")

  ;; export 2004-10-28 anniversary entries
  (icalendar-tests--test-export
   nil
   nil
   "
>>> anniversaries:

%%(diary-anniversary 3 28 1990) aa birthday (%d years old)"
   "DTSTART;VALUE=DATE:19910328
DTEND;VALUE=DATE:19910329
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=03;BYMONTHDAY=28
SUMMARY:aa birthday (%d years old)
")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 5 17 1956) bb birthday (%d years old)"
   "DTSTART;VALUE=DATE:19570517
DTEND;VALUE=DATE:19570518
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=05;BYMONTHDAY=17
SUMMARY:bb birthday (%d years old)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 6 8 1996) cc birthday (%d years old)"
   "DTSTART;VALUE=DATE:19970608
DTEND;VALUE=DATE:19970609
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=06;BYMONTHDAY=08
SUMMARY:cc birthday (%d years old)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 7 22 1982) dd (%d years ago...!)"
   "DTSTART;VALUE=DATE:19830722
DTEND;VALUE=DATE:19830723
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=07;BYMONTHDAY=22
SUMMARY:dd (%d years ago...!)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 8 1 1987) ee birthday (%d years old)"
   "DTSTART;VALUE=DATE:19880801
DTEND;VALUE=DATE:19880802
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=08;BYMONTHDAY=01
SUMMARY:ee birthday (%d years old)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 9 21 1956) ff birthday (%d years old)"
   "DTSTART;VALUE=DATE:19570921
DTEND;VALUE=DATE:19570922
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=09;BYMONTHDAY=21
SUMMARY:ff birthday (%d years old)")

  ;; FIXME: this testcase verifies that icalendar-export fails to
  ;; export the nested sexp. After repairing bug56241 icalendar-export
  ;; works correctly for this sexp but now the testcase fails.
  ;; Therefore this testcase is disabled for the time being.
  ;;  (icalendar-tests--test-export
  ;;   nil
  ;;   nil
  ;;   "%%(diary-offset '(diary-float t 3 4) 1) asdf"
  ;;   nil)


  ;; FIXME!

  ;; export 2004-10-28 monthly, weekly entries

  ;;   (icalendar-tests--test-export
  ;;    nil
  ;;    "
  ;; >>> ------------ monthly:

  ;; */27/* 10:00 blah blah"
  ;; "xxx")

  (icalendar-tests--test-export
   nil
   nil
   ">>> ------------ my week:

Monday 13:00 MAC"
   "DTSTART;VALUE=DATE-TIME:20000103T130000
DTEND;VALUE=DATE-TIME:20000103T140000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:MAC")

  (icalendar-tests--test-export
   nil
   nil
   "Monday 15:00 a1"
   "DTSTART;VALUE=DATE-TIME:20000103T150000
DTEND;VALUE=DATE-TIME:20000103T160000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:a1")


  (icalendar-tests--test-export
   nil
   nil
   "Monday 16:00-17:00 a2"
   "DTSTART;VALUE=DATE-TIME:20000103T160000
DTEND;VALUE=DATE-TIME:20000103T170000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:a2")

  (icalendar-tests--test-export
   nil
   nil
   "Tuesday 11:30-13:00 a3"
   "DTSTART;VALUE=DATE-TIME:20000104T113000
DTEND;VALUE=DATE-TIME:20000104T130000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU
SUMMARY:a3")

  (icalendar-tests--test-export
   nil
   nil
   "Tuesday 15:00 a4"
   "DTSTART;VALUE=DATE-TIME:20000104T150000
DTEND;VALUE=DATE-TIME:20000104T160000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU
SUMMARY:a4")

  (icalendar-tests--test-export
   nil
   nil
   "Wednesday 13:00 a5"
   "DTSTART;VALUE=DATE-TIME:20000105T130000
DTEND;VALUE=DATE-TIME:20000105T140000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=WE
SUMMARY:a5")

  (icalendar-tests--test-export
   nil
   nil
   "Wednesday 11:30-13:30 a6"
   "DTSTART;VALUE=DATE-TIME:20000105T113000
DTEND;VALUE=DATE-TIME:20000105T133000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=WE
SUMMARY:a6")

  (icalendar-tests--test-export
   nil
   nil
   "Wednesday 15:00 s1"
   "DTSTART;VALUE=DATE-TIME:20000105T150000
DTEND;VALUE=DATE-TIME:20000105T160000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=WE
SUMMARY:s1")


  ;; export 2004-10-28 regular entries
  (icalendar-tests--test-export
   nil
   nil
   "
>>> regular diary entries:

Oct 12 2004, 14:00 Tue: [2004-10-12] q1"
   "DTSTART;VALUE=DATE-TIME:20041012T140000
DTEND;VALUE=DATE-TIME:20041012T150000
SUMMARY:Tue: [2004-10-12] q1")

  ;; 2004-11-19
  (icalendar-tests--test-import "import-real-world-2004-11-19.ics"
                                nil
                                "import-real-world-2004-11-19.diary-european"
                                "import-real-world-2004-11-19.diary-american")

  ;; 2004-09-09 pg
  (icalendar-tests--test-export
   "%%(diary-block 1 1 2004 4 1 2004) Urlaub"
   nil
   nil
   "DTSTART;VALUE=DATE:20040101
DTEND;VALUE=DATE:20040105
SUMMARY:Urlaub")

  ;; 2004-10-25 pg
  (icalendar-tests--test-export
   nil
   "5 11 2004 Bla Fasel"
   nil
   "DTSTART;VALUE=DATE:20041105
DTEND;VALUE=DATE:20041106
SUMMARY:Bla Fasel")

  ;; 2004-10-30 pg
  (icalendar-tests--test-export
   nil
   "2 Nov 2004 15:00-16:30 Zahnarzt"
   nil
   "DTSTART;VALUE=DATE-TIME:20041102T150000
DTEND;VALUE=DATE-TIME:20041102T163000
SUMMARY:Zahnarzt")

  ;; 2005-02-07 lt
  (icalendar-tests--test-import "import-real-world-2005-02-07.ics"
                                nil
                                "import-real-world-2005-02-07.diary-european"
                                "import-real-world-2005-02-07.diary-american")

  ;; 2005-03-01 lt
  (icalendar-tests--test-import "import-real-world-2005-03-01.ics"
                                nil
                                "import-real-world-2005-03-01.diary-european"
                                "import-real-world-2005-03-01.diary-american")

  ;; 2005-03-23 lt
  (icalendar-tests--test-export
   nil
   "&%%(diary-cyclic 7 8 2 2005) 16:00-16:45 [WORK] Pppp"
   nil
   "DTSTART;VALUE=DATE-TIME:20050208T160000
DTEND;VALUE=DATE-TIME:20050208T164500
RRULE:FREQ=DAILY;INTERVAL=7
SUMMARY:[WORK] Pppp
")

  ;; 2005-05-27 eu
  (icalendar-tests--test-export
   nil
   nil
   ;; FIXME: colon not allowed!
   ;;"Nov 1: NNN Wwwwwwww Wwwww - Aaaaaa Pppppppp rrrrrr ddd oo Nnnnnnnn 30"
   "Nov 1 NNN Wwwwwwww Wwwww - Aaaaaa Pppppppp rrrrrr ddd oo Nnnnnnnn 30"
   "DTSTART;VALUE=DATE:19001101
DTEND;VALUE=DATE:19001102
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=11;BYMONTHDAY=1
SUMMARY:NNN Wwwwwwww Wwwww - Aaaaaa Pppppppp rrrrrr ddd oo Nnnnnnnn 30
")

  ;; bug#11473
  (icalendar-tests--test-import "import-bug-11473.ics"
                                nil
                                "import-bug-11473.diary-european"
                                nil)

  ;; 2015-12-05, mixed line endings and empty lines, see Bug#22092.
  (icalendar-tests--test-import "import-bug-22092.ics"
                                "import-bug-22092.diary-iso"
                                "import-bug-22092.diary-european"
                                "import-bug-22092.diary-american"))

(defun icalendar-test--format (string &optional day zone)
  "Decode and format STRING with DAY and ZONE."
  (let ((time (icalendar--decode-isodatetime string day zone)))
    (format-time-string "%FT%T%z" (encode-time time) 0)))

(ert-deftest icalendar-tests--decode-isodatetime ()
  "Test `icalendar--decode-isodatetime'."
  (should (equal (icalendar-test--format "20040917T050910-02:00")
                 "2004-09-17T03:09:10+0000"))
  (let ((orig (icalendar-test--format "20040917T050910")))
    (unwind-protect
	(let ((zone "XXX-02"))
	  (should (equal (icalendar-test--format "20040917T050910" nil zone)
                         "2004-09-17T03:09:10+0000"))
	  (should (equal (icalendar-test--format "20040917T0509" nil zone)
                         "2004-09-17T03:09:00+0000"))
	  (should (equal (icalendar-test--format "20040917" nil zone)
                         "2004-09-16T22:00:00+0000"))
	  (should (equal (icalendar-test--format "20040917T050910" 1 zone)
                         "2004-09-18T03:09:10+0000"))
	  (should (equal (icalendar-test--format "20040917T050910" 30 zone)
                         "2004-10-17T03:09:10+0000")))
      (should (equal orig (icalendar-test--format "20040917T050910")))))
  (should (equal (icalendar-test--format "20040917T050910Z")
                 "2004-09-17T05:09:10+0000"))
  (should (equal (icalendar-test--format "20040917T050910" -1 0)
                 "2004-09-16T05:09:10+0000"))
  (should (equal (icalendar-test--format "20040917T050910" nil -3600)
                 "2004-09-17T06:09:10+0000")))

(provide 'icalendar-tests)
;;; icalendar-tests.el ends here
