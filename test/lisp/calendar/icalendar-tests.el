;; icalendar-tests.el --- Test suite for icalendar.el

;; Copyright (C) 2005, 2008-2017 Free Software Foundation, Inc.

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

;; ======================================================================
;; Tests of functions
;; ======================================================================

(ert-deftest icalendar--create-uid ()
  "Test for `icalendar--create-uid'."
  (let* ((icalendar-uid-format "xxx-%t-%c-%h-%u-%s")
         (icalendar--uid-count 77)
         (entry-full "30.06.1964 07:01 blahblah")
         (hash (format "%d" (abs (sxhash entry-full))))
         (contents "DTSTART:19640630T070100\nblahblah")
         (username (or user-login-name "UNKNOWN_USER")))
    (cl-letf (((symbol-function 'current-time) (lambda () '(1 2 3))))
      (should (= 77 icalendar--uid-count))
      (should (string=  (concat "xxx-123-77-" hash "-" username "-19640630")
                        (icalendar--create-uid entry-full contents)))
      (should (= 78 icalendar--uid-count)))
    (setq contents "blahblah")
    (setq icalendar-uid-format "yyy%syyy")
    (should (string=  (concat "yyyDTSTARTyyy")
                      (icalendar--create-uid entry-full contents)))))

(ert-deftest icalendar-convert-anniversary-to-ical ()
  "Test method for `icalendar--convert-anniversary-to-ical'."
  (let* ((calendar-date-style 'iso)
         result)
    (setq result (icalendar--convert-anniversary-to-ical
                  "" "%%(diary-anniversary 1964 6 30) g"))
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
    ;; offsetfrom = offsetto
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
                     (cdr result)))))

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
		     (icalendar--datestring-to-isodate "2008 Nov 05")))))

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
    (setq icalendar-import-format (lambda (&rest ignore)
                                    "helloworld"))
    (should (string= "helloworld"  (icalendar--format-ical-event event)))
    (setq icalendar-import-format
          (lambda (e)
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
  (let ((tz (getenv "TZ"))
	result)
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

          ;; testcase: UTC time zone specifier in input -> convert to local time
          ;; 31 Dec 2013 23:00 UTC -> 1 Jan 2013 01:00 EET
          (should (equal '(0 0 1 1 1 2014 3 nil 7200)
                         (icalendar--decode-isodatetime "20131231T230000Z")))
          ;; 1 Aug 2013 10:00 UTC -> 1 Aug 2013 13:00 EEST
          (should (equal '(0 0 13 1 8 2013 4 t 10800)
                         (icalendar--decode-isodatetime "20130801T100000Z")))

          )
      ;; restore time-zone even if something went terribly wrong
      (setenv "TZ" tz)))  )

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

European style input data must use german month names.  American
and ISO style input data must use english month names."
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
  (let ((temp-file (make-temp-file "icalendar-tests-ics")))
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
      (kill-buffer (find-buffer-visiting temp-file))
      (delete-file temp-file))))

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
   "%%(diary-anniversary 1989 10 3) anniversary no time"
   "%%(diary-anniversary 3 10 1989) anniversary no time"
   "%%(diary-anniversary 10 3 1989) anniversary no time"
   "DTSTART;VALUE=DATE:19891003
DTEND;VALUE=DATE:19891004
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=10;BYMONTHDAY=03
SUMMARY:anniversary no time
")
  (icalendar-tests--test-export
   "%%(diary-anniversary 1989 10 3) 19:00-20:00 anniversary with time"
   "%%(diary-anniversary 3 10 1989) 19:00-20:00 anniversary with time"
   "%%(diary-anniversary 10 3 1989) 19:00-20:00 anniversary with time"
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
;; Import tests
;; ======================================================================

(defun icalendar-tests--test-import (input expected-iso expected-european
					   expected-american)
  "Perform import test.
Argument INPUT icalendar event string.
Argument EXPECTED-ISO expected iso style diary string.
Argument EXPECTED-EUROPEAN expected european style diary string.
Argument EXPECTED-AMERICAN expected american style diary string.
During import test the timezone is set to Central European Time."
  (let ((timezone (getenv "TZ")))
    (unwind-protect
	(progn
	  ;; Use this form so as not to rely on system tz database.
	  ;; Eg hydra.nixos.org.
	  (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
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
		  (icalendar-import-format-uid "\n UID: %s")
		  calendar-date-style)
	      (when expected-iso
		(setq calendar-date-style 'iso)
		(icalendar-tests--do-test-import input expected-iso))
	      (when expected-european
		(setq calendar-date-style 'european)
		(icalendar-tests--do-test-import input expected-european))
	      (when expected-american
		(setq calendar-date-style 'american)
		(icalendar-tests--do-test-import input expected-american)))))
      (setenv "TZ" timezone))))

(defun icalendar-tests--do-test-import (input expected-output)
  "Actually perform import test.
Argument INPUT input icalendar string.
Argument EXPECTED-OUTPUT expected diary string."
  (let ((temp-file (make-temp-file "icalendar-test-diary")))
    ;; Test the Catch-the-mysterious-coding-header logic below.
    ;; Ruby-mode adds an after-save-hook which inserts the header!
    ;; (save-excursion
    ;;   (find-file temp-file)
    ;;   (ruby-mode))
    (icalendar-import-buffer temp-file t t)
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
      (kill-buffer (find-buffer-visiting temp-file))
      (delete-file temp-file))))

(ert-deftest icalendar-import-non-recurring ()
  "Perform standard import tests."
  (icalendar-tests--test-import
   "SUMMARY:non-recurring
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000"
   "&2003/9/19 09:00-11:30 non-recurring\n"
   "&19/9/2003 09:00-11:30 non-recurring\n"
   "&9/19/2003 09:00-11:30 non-recurring\n")
  (icalendar-tests--test-import
   "SUMMARY:non-recurring allday
DTSTART;VALUE=DATE-TIME:20030919"
   "&2003/9/19 non-recurring allday\n"
   "&19/9/2003 non-recurring allday\n"
   "&9/19/2003 non-recurring allday\n")
  (icalendar-tests--test-import
   ;; Checkdoc removes trailing blanks.  Therefore: format!
   (format "%s\n%s\n%s" "SUMMARY:long " " summary"
           "DTSTART;VALUE=DATE:20030919")
   "&2003/9/19 long summary\n"
   "&19/9/2003 long summary\n"
   "&9/19/2003 long summary\n")
  (icalendar-tests--test-import
   "UID:748f2da0-0d9b-11d8-97af-b4ec8686ea61
SUMMARY:Sommerferien
STATUS:TENTATIVE
CLASS:PRIVATE
X-MOZILLA-ALARM-DEFAULT-UNITS:Minuten
X-MOZILLA-RECUR-DEFAULT-INTERVAL:0
DTSTART;VALUE=DATE:20040719
DTEND;VALUE=DATE:20040828
DTSTAMP:20031103T011641Z
"
   "&%%(and (diary-block 2004 7 19 2004 8 27)) Sommerferien
 Status: TENTATIVE
 Class: PRIVATE
 UID: 748f2da0-0d9b-11d8-97af-b4ec8686ea61
"
   "&%%(and (diary-block 19 7 2004 27 8 2004)) Sommerferien
 Status: TENTATIVE
 Class: PRIVATE
 UID: 748f2da0-0d9b-11d8-97af-b4ec8686ea61
"
   "&%%(and (diary-block 7 19 2004 8 27 2004)) Sommerferien
 Status: TENTATIVE
 Class: PRIVATE
 UID: 748f2da0-0d9b-11d8-97af-b4ec8686ea61
")
  (icalendar-tests--test-import
   "UID
 :04979712-3902-11d9-93dd-8f9f4afe08da
SUMMARY
 :folded summary
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
DTSTART
 :20041123T140000
DTEND
 :20041123T143000
DTSTAMP
 :20041118T013430Z
LAST-MODIFIED
 :20041118T013640Z
"
   "&2004/11/23 14:00-14:30 folded summary
 Status: TENTATIVE
 Class: PRIVATE
 UID: 04979712-3902-11d9-93dd-8f9f4afe08da\n"
   "&23/11/2004 14:00-14:30 folded summary
 Status: TENTATIVE
 Class: PRIVATE
 UID: 04979712-3902-11d9-93dd-8f9f4afe08da\n"
   "&11/23/2004 14:00-14:30 folded summary
 Status: TENTATIVE
 Class: PRIVATE
 UID: 04979712-3902-11d9-93dd-8f9f4afe08da\n")

  (icalendar-tests--test-import
   "UID
 :6161a312-3902-11d9-b512-f764153bb28b
SUMMARY
 :another example
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
DTSTART
 :20041123T144500
DTEND
 :20041123T154500
DTSTAMP
 :20041118T013641Z
"
   "&2004/11/23 14:45-15:45 another example
 Status: TENTATIVE
 Class: PRIVATE
 UID: 6161a312-3902-11d9-b512-f764153bb28b\n"
   "&23/11/2004 14:45-15:45 another example
 Status: TENTATIVE
 Class: PRIVATE
 UID: 6161a312-3902-11d9-b512-f764153bb28b\n"
   "&11/23/2004 14:45-15:45 another example
 Status: TENTATIVE
 Class: PRIVATE
 UID: 6161a312-3902-11d9-b512-f764153bb28b\n"))

(ert-deftest icalendar-import-rrule ()
  (icalendar-tests--test-import
   "SUMMARY:rrule daily
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;
"
   "&%%(and (diary-cyclic 1 2003 9 19)) 09:00-11:30 rrule daily\n"
   "&%%(and (diary-cyclic 1 19 9 2003)) 09:00-11:30 rrule daily\n"
   "&%%(and (diary-cyclic 1 9 19 2003)) 09:00-11:30 rrule daily\n")
  ;; RRULE examples
  (icalendar-tests--test-import
   "SUMMARY:rrule daily
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;INTERVAL=2
"
   "&%%(and (diary-cyclic 2 2003 9 19)) 09:00-11:30 rrule daily\n"
   "&%%(and (diary-cyclic 2 19 9 2003)) 09:00-11:30 rrule daily\n"
   "&%%(and (diary-cyclic 2 9 19 2003)) 09:00-11:30 rrule daily\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule daily with exceptions
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;INTERVAL=2
EXDATE:20030921,20030925
"
   "&%%(and (not (diary-date 2003 9 25)) (not (diary-date 2003 9 21)) (diary-cyclic 2 2003 9 19)) 09:00-11:30 rrule daily with exceptions\n"
   "&%%(and (not (diary-date 25 9 2003)) (not (diary-date 21 9 2003)) (diary-cyclic 2 19 9 2003)) 09:00-11:30 rrule daily with exceptions\n"
   "&%%(and (not (diary-date 9 25 2003)) (not (diary-date 9 21 2003)) (diary-cyclic 2 9 19 2003)) 09:00-11:30 rrule daily with exceptions\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule weekly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=WEEKLY;
"
   "&%%(and (diary-cyclic 7 2003 9 19)) 09:00-11:30 rrule weekly\n"
   "&%%(and (diary-cyclic 7 19 9 2003)) 09:00-11:30 rrule weekly\n"
   "&%%(and (diary-cyclic 7 9 19 2003)) 09:00-11:30 rrule weekly\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule monthly no end
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;
"
   "&%%(and (diary-date t t 19) (diary-block 2003 9 19 9999 1 1)) 09:00-11:30 rrule monthly no end\n"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 1 1 9999)) 09:00-11:30 rrule monthly no end\n"
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 1 1 9999)) 09:00-11:30 rrule monthly no end\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule monthly with end
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;UNTIL=20050819;
"
   "&%%(and (diary-date t t 19) (diary-block 2003 9 19 2005 8 19)) 09:00-11:30 rrule monthly with end\n"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 19 8 2005)) 09:00-11:30 rrule monthly with end\n"
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 8 19 2005)) 09:00-11:30 rrule monthly with end\n")
  (icalendar-tests--test-import
   "DTSTART;VALUE=DATE:20040815
DTEND;VALUE=DATE:20040816
SUMMARY:Maria Himmelfahrt
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=8
"
   "&%%(and (diary-anniversary 2004 8 15))  Maria Himmelfahrt\n"
   "&%%(and (diary-anniversary 15 8 2004))  Maria Himmelfahrt\n"
   "&%%(and (diary-anniversary 8 15 2004))  Maria Himmelfahrt\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule yearly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=YEARLY;INTERVAL=2
"
   "&%%(and (diary-anniversary 2003 9 19)) 09:00-11:30 rrule yearly\n" ;FIXME
   "&%%(and (diary-anniversary 19 9 2003)) 09:00-11:30 rrule yearly\n" ;FIXME
   "&%%(and (diary-anniversary 9 19 2003)) 09:00-11:30 rrule yearly\n") ;FIXME
  (icalendar-tests--test-import
   "SUMMARY:rrule count daily short
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;COUNT=1;INTERVAL=1
"
   "&%%(and (diary-cyclic 1 2003 9 19) (diary-block 2003 9 19 2003 9 19)) 09:00-11:30 rrule count daily short\n"
   "&%%(and (diary-cyclic 1 19 9 2003) (diary-block 19 9 2003 19 9 2003)) 09:00-11:30 rrule count daily short\n"
   "&%%(and (diary-cyclic 1 9 19 2003) (diary-block 9 19 2003 9 19 2003)) 09:00-11:30 rrule count daily short\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule count daily long
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;COUNT=14;INTERVAL=1
"
   "&%%(and (diary-cyclic 1 2003 9 19) (diary-block 2003 9 19 2003 10 2)) 09:00-11:30 rrule count daily long\n"
   "&%%(and (diary-cyclic 1 19 9 2003) (diary-block 19 9 2003 2 10 2003)) 09:00-11:30 rrule count daily long\n"
   "&%%(and (diary-cyclic 1 9 19 2003) (diary-block 9 19 2003 10 2 2003)) 09:00-11:30 rrule count daily long\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule count bi-weekly 3 times
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=WEEKLY;COUNT=3;INTERVAL=2
"
   "&%%(and (diary-cyclic 14 2003 9 19) (diary-block 2003 9 19 2003 10 31)) 09:00-11:30 rrule count bi-weekly 3 times\n"
   "&%%(and (diary-cyclic 14 19 9 2003) (diary-block 19 9 2003 31 10 2003)) 09:00-11:30 rrule count bi-weekly 3 times\n"
   "&%%(and (diary-cyclic 14 9 19 2003) (diary-block 9 19 2003 10 31 2003)) 09:00-11:30 rrule count bi-weekly 3 times\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule count monthly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;INTERVAL=1;COUNT=5
"
   "&%%(and (diary-date t t 19) (diary-block 2003 9 19 2004 1 19)) 09:00-11:30 rrule count monthly\n"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 19 1 2004)) 09:00-11:30 rrule count monthly\n"
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 1 19 2004)) 09:00-11:30 rrule count monthly\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule count every second month
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=5
"
   "&%%(and (diary-date t t 19) (diary-block 2003 9 19 2004 5 19)) 09:00-11:30 rrule count every second month\n" ;FIXME
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 19 5 2004)) 09:00-11:30 rrule count every second month\n" ;FIXME
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 5 19 2004)) 09:00-11:30 rrule count every second month\n") ;FIXME
  (icalendar-tests--test-import
   "SUMMARY:rrule count yearly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=YEARLY;INTERVAL=1;COUNT=5
"
   "&%%(and (diary-date t 9 19) (diary-block 2003 9 19 2007 9 19)) 09:00-11:30 rrule count yearly\n"
   "&%%(and (diary-date 19 9 t) (diary-block 19 9 2003 19 9 2007)) 09:00-11:30 rrule count yearly\n"
   "&%%(and (diary-date 9 19 t) (diary-block 9 19 2003 9 19 2007)) 09:00-11:30 rrule count yearly\n")
  (icalendar-tests--test-import
   "SUMMARY:rrule count every second year
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=5
"
   "&%%(and (diary-date t 9 19) (diary-block 2003 9 19 2011 9 19)) 09:00-11:30 rrule count every second year\n" ;FIXME!!!
   "&%%(and (diary-date 19 9 t) (diary-block 19 9 2003 19 9 2011)) 09:00-11:30 rrule count every second year\n" ;FIXME!!!
   "&%%(and (diary-date 9 19 t) (diary-block 9 19 2003 9 19 2011)) 09:00-11:30 rrule count every second year\n") ;FIXME!!!
)

(ert-deftest icalendar-import-duration ()
  ;; duration
  (icalendar-tests--test-import
   "DTSTART;VALUE=DATE:20050217
SUMMARY:duration
DURATION:P7D
"
   "&%%(and (diary-block 2005 2 17 2005 2 23)) duration\n"
   "&%%(and (diary-block 17 2 2005 23 2 2005)) duration\n"
   "&%%(and (diary-block 2 17 2005 2 23 2005)) duration\n")
  (icalendar-tests--test-import
   "UID:20041127T183329Z-18215-1001-4536-49109@andromeda
DTSTAMP:20041127T183315Z
LAST-MODIFIED:20041127T183329
SUMMARY:Urlaub
DTSTART;VALUE=DATE:20011221
DTEND;VALUE=DATE:20011221
RRULE:FREQ=DAILY;UNTIL=20011229;INTERVAL=1;WKST=SU
CLASS:PUBLIC
SEQUENCE:1
CREATED:20041127T183329
"
   "&%%(and (diary-cyclic 1 2001 12 21) (diary-block 2001 12 21 2001 12 29))  Urlaub
 Class: PUBLIC
 UID: 20041127T183329Z-18215-1001-4536-49109@andromeda\n"
   "&%%(and (diary-cyclic 1 21 12 2001) (diary-block 21 12 2001 29 12 2001))  Urlaub
 Class: PUBLIC
 UID: 20041127T183329Z-18215-1001-4536-49109@andromeda\n"
   "&%%(and (diary-cyclic 1 12 21 2001) (diary-block 12 21 2001 12 29 2001))  Urlaub
 Class: PUBLIC
 UID: 20041127T183329Z-18215-1001-4536-49109@andromeda\n"))

(ert-deftest icalendar-import-bug-6766 ()
  ;;bug#6766 -- multiple byday values in a weekly rrule
  (icalendar-tests--test-import
"CLASS:PUBLIC
DTEND;TZID=America/New_York:20100421T120000
DTSTAMP:20100525T141214Z
DTSTART;TZID=America/New_York:20100421T113000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO,WE,TH,FR
SEQUENCE:1
STATUS:CONFIRMED
SUMMARY:Scrum
TRANSP:OPAQUE
UID:8814e3f9-7482-408f-996c-3bfe486a1262
END:VEVENT
BEGIN:VEVENT
CLASS:PUBLIC
DTSTAMP:20100525T141214Z
DTSTART;VALUE=DATE:20100422
DTEND;VALUE=DATE:20100423
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU,TH
SEQUENCE:1
SUMMARY:Tues + Thurs thinking
TRANSP:OPAQUE
UID:8814e3f9-7482-408f-996c-3bfe486a1263
"
"&%%(and (memq (calendar-day-of-week date) '(1 3 4 5)) (diary-cyclic 1 2010 4 21)) 11:30-12:00 Scrum
 Status: CONFIRMED
 Class: PUBLIC
 UID: 8814e3f9-7482-408f-996c-3bfe486a1262
&%%(and (memq (calendar-day-of-week date) '(2 4)) (diary-cyclic 1 2010 4 22)) Tues + Thurs thinking
 Class: PUBLIC
 UID: 8814e3f9-7482-408f-996c-3bfe486a1263
"
"&%%(and (memq (calendar-day-of-week date) '(1 3 4 5)) (diary-cyclic 1 21 4 2010)) 11:30-12:00 Scrum
 Status: CONFIRMED
 Class: PUBLIC
 UID: 8814e3f9-7482-408f-996c-3bfe486a1262
&%%(and (memq (calendar-day-of-week date) '(2 4)) (diary-cyclic 1 22 4 2010)) Tues + Thurs thinking
 Class: PUBLIC
 UID: 8814e3f9-7482-408f-996c-3bfe486a1263
"
"&%%(and (memq (calendar-day-of-week date) '(1 3 4 5)) (diary-cyclic 1 4 21 2010)) 11:30-12:00 Scrum
 Status: CONFIRMED
 Class: PUBLIC
 UID: 8814e3f9-7482-408f-996c-3bfe486a1262
&%%(and (memq (calendar-day-of-week date) '(2 4)) (diary-cyclic 1 4 22 2010)) Tues + Thurs thinking
 Class: PUBLIC
 UID: 8814e3f9-7482-408f-996c-3bfe486a1263
"))

(ert-deftest icalendar-import-bug-24199 ()
  ;;bug#24199 -- monthly rule with byday-clause
  (icalendar-tests--test-import
"
SUMMARY:Summary
DESCRIPTION:Desc
LOCATION:Loc
DTSTART:20151202T124600
DTEND:20151202T160000
RRULE:FREQ=MONTHLY;BYDAY=1WE;INTERVAL=1
EXDATE:20160106T114600Z
EXDATE:20160203T114600Z
EXDATE:20160302T114600Z
EXDATE:20160504T104600Z
EXDATE:20160601T104600Z
CLASS:DEFAULT
TRANSP:OPAQUE
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER;VALUE=DURATION:-PT3H
END:VALARM
LAST-MODIFIED:20160805T191040Z
UID:9188710a-08a7-4061-bae3-d4cf4972599a
"
"&%%(and (not (diary-date 2016 1 6)) (not (diary-date 2016 2 3)) (not (diary-date 2016 3 2)) (not (diary-date 2016 5 4)) (not (diary-date 2016 6 1)) (diary-float t 3 1) (diary-block 2015 12 2 9999 1 1)) 12:46-16:00 Summary
 Desc: Desc
 Location: Loc
 Class: DEFAULT
 UID: 9188710a-08a7-4061-bae3-d4cf4972599a
"
"&%%(and (not (diary-date 6 1 2016)) (not (diary-date 3 2 2016)) (not (diary-date 2 3 2016)) (not (diary-date 4 5 2016)) (not (diary-date 1 6 2016)) (diary-float t 3 1) (diary-block 2 12 2015 1 1 9999)) 12:46-16:00 Summary
 Desc: Desc
 Location: Loc
 Class: DEFAULT
 UID: 9188710a-08a7-4061-bae3-d4cf4972599a
"
"&%%(and (not (diary-date 1 6 2016)) (not (diary-date 2 3 2016)) (not (diary-date 3 2 2016)) (not (diary-date 5 4 2016)) (not (diary-date 6 1 2016)) (diary-float t 3 1) (diary-block 12 2 2015 1 1 9999)) 12:46-16:00 Summary
 Desc: Desc
 Location: Loc
 Class: DEFAULT
 UID: 9188710a-08a7-4061-bae3-d4cf4972599a
"
))

(ert-deftest icalendar-import-multiple-vcalendars ()
  (icalendar-tests--test-import
   "DTSTART;VALUE=DATE:20110723
SUMMARY:event-1
"
   "&2011/7/23 event-1\n"
   "&23/7/2011 event-1\n"
   "&7/23/2011 event-1\n")

  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0\nBEGIN:VEVENT
DTSTART;VALUE=DATE:20110723
SUMMARY:event-1
END:VEVENT
END:VCALENDAR
BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0
BEGIN:VEVENT
DTSTART;VALUE=DATE:20110724
SUMMARY:event-2
END:VEVENT
END:VCALENDAR
BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0
BEGIN:VEVENT
DTSTART;VALUE=DATE:20110725
SUMMARY:event-3a
END:VEVENT
BEGIN:VEVENT
DTSTART;VALUE=DATE:20110725
SUMMARY:event-3b
END:VEVENT
END:VCALENDAR
"
   "&2011/7/23 event-1\n&2011/7/24 event-2\n&2011/7/25 event-3a\n&2011/7/25 event-3b\n"
   "&23/7/2011 event-1\n&24/7/2011 event-2\n&25/7/2011 event-3a\n&25/7/2011 event-3b\n"
   "&7/23/2011 event-1\n&7/24/2011 event-2\n&7/25/2011 event-3a\n&7/25/2011 event-3b\n"))

(ert-deftest icalendar-import-with-uid ()
  "Perform import test with uid."
  (icalendar-tests--test-import
   "UID:1234567890uid
SUMMARY:non-recurring
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000"
   "&2003/9/19 09:00-11:30 non-recurring\n UID: 1234567890uid\n"
   "&19/9/2003 09:00-11:30 non-recurring\n UID: 1234567890uid\n"
   "&9/19/2003 09:00-11:30 non-recurring\n UID: 1234567890uid\n"))

(ert-deftest icalendar-import-with-timezone ()
  ;; This is known to fail on MS-Windows, because the test assumes
  ;; Posix features of specifying DST rules.
  :expected-result (if (memq system-type '(windows-nt ms-dos))
                       :failed
                     :passed)
  ;; bug#11473
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
BEGIN:VTIMEZONE
TZID:fictional, nonexistent, arbitrary
BEGIN:STANDARD
DTSTART:20100101T000000
TZOFFSETFROM:+0200
TZOFFSETTO:-0200
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=1SU;BYMONTH=01
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20101201T000000
TZOFFSETFROM:-0200
TZOFFSETTO:+0200
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=1SU;BYMONTH=11
END:DAYLIGHT
END:VTIMEZONE
BEGIN:VEVENT
SUMMARY:standardtime
DTSTART;TZID=\"fictional, nonexistent, arbitrary\":20120115T120000
DTEND;TZID=\"fictional, nonexistent, arbitrary\":20120115T123000
END:VEVENT
BEGIN:VEVENT
SUMMARY:daylightsavingtime
DTSTART;TZID=\"fictional, nonexistent, arbitrary\":20121215T120000
DTEND;TZID=\"fictional, nonexistent, arbitrary\":20121215T123000
END:VEVENT
END:VCALENDAR"
   ;; "standardtime" begins first sunday in january and is 4 hours behind CET
   ;; "daylightsavingtime" begins first sunday in november and is 1 hour before CET
   "&2012/1/15 15:00-15:30 standardtime
&2012/12/15 11:00-11:30 daylightsavingtime
"
   nil
   nil)
  )
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
  (let ((temp-diary (make-temp-file "icalendar-test-diary"))
        (temp-ics (make-temp-file "icalendar-test-ics"))
        (org-input (buffer-substring-no-properties (point-min) (point-max))))

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
	(kill-buffer (current-buffer)))
      (delete-file temp-diary)
      (delete-file temp-ics))))

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
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
METHOD:REQUEST
PRODID:Microsoft CDO for Microsoft Exchange
VERSION:2.0
BEGIN:VTIMEZONE
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
BEGIN:VEVENT
DTSTAMP:20030509T043439Z
DTSTART;TZID=\"Kolkata, Chennai, Mumbai, New Delhi\":20030509T103000
SUMMARY:On-Site Interview
UID:040000008200E00074C5B7101A82E0080000000080B6DE661216C301000000000000000
 010000000DB823520692542408ED02D7023F9DFF9
ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=TRUE;CN=\"Xxxxx
 xxx Xxxxxxxxxxxx\":MAILTO:xxxxxxxx@xxxxxxx.com
ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=TRUE;CN=\"Yyyyyyy Y
 yyyy\":MAILTO:yyyyyyy@yyyyyyy.com
ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=TRUE;CN=\"Zzzz Zzzz
 zz\":MAILTO:zzzzzz@zzzzzzz.com
ORGANIZER;CN=\"Aaaaaa Aaaaa\":MAILTO:aaaaaaa@aaaaaaa.com
LOCATION:Cccc
DTEND;TZID=\"Kolkata, Chennai, Mumbai, New Delhi\":20030509T153000
DESCRIPTION:10:30am - Blah
SEQUENCE:0
PRIORITY:5
CLASS:
CREATED:20030509T043439Z
LAST-MODIFIED:20030509T043459Z
STATUS:CONFIRMED
TRANSP:OPAQUE
X-MICROSOFT-CDO-BUSYSTATUS:BUSY
X-MICROSOFT-CDO-INSTTYPE:0
X-MICROSOFT-CDO-INTENDEDSTATUS:BUSY
X-MICROSOFT-CDO-ALLDAYEVENT:FALSE
X-MICROSOFT-CDO-IMPORTANCE:1
X-MICROSOFT-CDO-OWNERAPPTID:126441427
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:REMINDER
TRIGGER;RELATED=START:-PT00H15M00S
END:VALARM
END:VEVENT
END:VCALENDAR"
   nil
   "&9/5/2003 07:00-12:00 On-Site Interview
 Desc: 10:30am - Blah
 Location: Cccc
 Organizer: MAILTO:aaaaaaa@aaaaaaa.com
 Status: CONFIRMED
 UID: 040000008200E00074C5B7101A82E0080000000080B6DE661216C301000000000000000010000000DB823520692542408ED02D7023F9DFF9
"
   "&5/9/2003 07:00-12:00 On-Site Interview
 Desc: 10:30am - Blah
 Location: Cccc
 Organizer: MAILTO:aaaaaaa@aaaaaaa.com
 Status: CONFIRMED
 UID: 040000008200E00074C5B7101A82E0080000000080B6DE661216C301000000000000000010000000DB823520692542408ED02D7023F9DFF9
")

  ;; created with http://apps.marudot.com/ical/
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//www.marudot.com//iCal Event Maker
X-WR-CALNAME:Test
CALSCALE:GREGORIAN
BEGIN:VTIMEZONE
TZID:Asia/Tehran
TZURL:http://tzurl.org/zoneinfo-outlook/Asia/Tehran
X-LIC-LOCATION:Asia/Tehran
BEGIN:STANDARD
TZOFFSETFROM:+0330
TZOFFSETTO:+0330
TZNAME:IRST
DTSTART:19700101T000000
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTAMP:20141116T171439Z
UID:20141116T171439Z-678877132@marudot.com
DTSTART;TZID=\"Asia/Tehran\":20141116T070000
DTEND;TZID=\"Asia/Tehran\":20141116T080000
SUMMARY:NoDST
DESCRIPTION:Test event from timezone without DST
LOCATION:Everywhere
END:VEVENT
END:VCALENDAR"
   nil
   "&16/11/2014 04:30-05:30 NoDST
 Desc: Test event from timezone without DST
 Location: Everywhere
 UID: 20141116T171439Z-678877132@marudot.com
"
   "&11/16/2014 04:30-05:30 NoDST
 Desc: Test event from timezone without DST
 Location: Everywhere
 UID: 20141116T171439Z-678877132@marudot.com
")


  ;; 2003-06-18 a
  (icalendar-tests--test-import
   "DTSTAMP:20030618T195512Z
DTSTART;TZID=\"Mountain Time (US & Canada)\":20030623T110000
SUMMARY:Dress Rehearsal for XXXX-XXXX
UID:040000008200E00074C5B7101A82E00800000000608AA7DA9835C301000000000000000
 0100000007C3A6D65EE726E40B7F3D69A23BD567E
ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=TRUE;CN=\"AAAAA,AAA
 AA (A-AAAAAAA,ex1)\":MAILTO:aaaaa_aaaaa@aaaaa.com
ORGANIZER;CN=\"ABCD,TECHTRAINING
 (A-Americas,exgen1)\":MAILTO:xxx@xxxxx.com
LOCATION:555 or TN 555-5555 ID 5555 & NochWas (see below)
DTEND;TZID=\"Mountain Time (US & Canada)\":20030623T120000
DESCRIPTION:753 Zeichen hier radiert
SEQUENCE:0
PRIORITY:5
CLASS:
CREATED:20030618T195518Z
LAST-MODIFIED:20030618T195527Z
STATUS:CONFIRMED
TRANSP:OPAQUE
X-MICROSOFT-CDO-BUSYSTATUS:BUSY
X-MICROSOFT-CDO-INSTTYPE:0
X-MICROSOFT-CDO-INTENDEDSTATUS:BUSY
X-MICROSOFT-CDO-ALLDAYEVENT:FALSE
X-MICROSOFT-CDO-IMPORTANCE:1
X-MICROSOFT-CDO-OWNERAPPTID:1022519251
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:REMINDER
TRIGGER;RELATED=START:-PT00H15M00S
END:VALARM"
   nil
   "&23/6/2003 11:00-12:00 Dress Rehearsal for XXXX-XXXX
 Desc: 753 Zeichen hier radiert
 Location: 555 or TN 555-5555 ID 5555 & NochWas (see below)
 Organizer: MAILTO:xxx@xxxxx.com
 Status: CONFIRMED
 UID: 040000008200E00074C5B7101A82E00800000000608AA7DA9835C3010000000000000000100000007C3A6D65EE726E40B7F3D69A23BD567E
"
   "&6/23/2003 11:00-12:00 Dress Rehearsal for XXXX-XXXX
 Desc: 753 Zeichen hier radiert
 Location: 555 or TN 555-5555 ID 5555 & NochWas (see below)
 Organizer: MAILTO:xxx@xxxxx.com
 Status: CONFIRMED
 UID: 040000008200E00074C5B7101A82E00800000000608AA7DA9835C3010000000000000000100000007C3A6D65EE726E40B7F3D69A23BD567E
")
  ;; 2003-06-18 b -- uses timezone
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
METHOD:REQUEST
PRODID:Microsoft CDO for Microsoft Exchange
VERSION:2.0
BEGIN:VTIMEZONE
TZID:Mountain Time (US & Canada)
X-MICROSOFT-CDO-TZID:12
BEGIN:STANDARD
DTSTART:16010101T020000
TZOFFSETFROM:-0600
TZOFFSETTO:-0700
RRULE:FREQ=YEARLY;WKST=MO;INTERVAL=1;BYMONTH=10;BYDAY=-1SU
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:16010101T020000
TZOFFSETFROM:-0700
TZOFFSETTO:-0600
RRULE:FREQ=YEARLY;WKST=MO;INTERVAL=1;BYMONTH=4;BYDAY=1SU
END:DAYLIGHT
END:VTIMEZONE
BEGIN:VEVENT
DTSTAMP:20030618T230323Z
DTSTART;TZID=\"Mountain Time (US & Canada)\":20030623T090000
SUMMARY:Updated: Dress Rehearsal for ABC01-15
UID:040000008200E00074C5B7101A82E00800000000608AA7DA9835C301000000000000000
 0100000007C3A6D65EE726E40B7F3D69A23BD567E
ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;X-REPLYTIME=20030618T20
 0700Z;RSVP=TRUE;CN=\"AAAAA,AAAAAA
\(A-AAAAAAA,ex1)\":MAILTO:aaaaaa_aaaaa@aaaaa
 .com
ORGANIZER;CN=\"ABCD,TECHTRAINING
\(A-Americas,exgen1)\":MAILTO:bbb@bbbbb.com
LOCATION:123 or TN 123-1234 ID abcd & SonstWo (see below)
DTEND;TZID=\"Mountain Time (US & Canada)\":20030623T100000
DESCRIPTION:Viele Zeichen standen hier früher
SEQUENCE:0
PRIORITY:5
CLASS:
CREATED:20030618T230326Z
LAST-MODIFIED:20030618T230335Z
STATUS:CONFIRMED
TRANSP:OPAQUE
X-MICROSOFT-CDO-BUSYSTATUS:BUSY
X-MICROSOFT-CDO-INSTTYPE:0
X-MICROSOFT-CDO-INTENDEDSTATUS:BUSY
X-MICROSOFT-CDO-ALLDAYEVENT:FALSE
X-MICROSOFT-CDO-IMPORTANCE:1
X-MICROSOFT-CDO-OWNERAPPTID:1022519251
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:REMINDER
TRIGGER;RELATED=START:-PT00H15M00S
END:VALARM
END:VEVENT
END:VCALENDAR"
   nil
   "&23/6/2003 17:00-18:00 Updated: Dress Rehearsal for ABC01-15
 Desc: Viele Zeichen standen hier früher
 Location: 123 or TN 123-1234 ID abcd & SonstWo (see below)
 Organizer: MAILTO:bbb@bbbbb.com
 Status: CONFIRMED
 UID: 040000008200E00074C5B7101A82E00800000000608AA7DA9835C3010000000000000000100000007C3A6D65EE726E40B7F3D69A23BD567E
"
   "&6/23/2003 17:00-18:00 Updated: Dress Rehearsal for ABC01-15
 Desc: Viele Zeichen standen hier früher
 Location: 123 or TN 123-1234 ID abcd & SonstWo (see below)
 Organizer: MAILTO:bbb@bbbbb.com
 Status: CONFIRMED
 UID: 040000008200E00074C5B7101A82E00800000000608AA7DA9835C3010000000000000000100000007C3A6D65EE726E40B7F3D69A23BD567E
")
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

%%(diary-anniversary 3 28 1991) aa birthday (%d years old)"
   "DTSTART;VALUE=DATE:19910328
DTEND;VALUE=DATE:19910329
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=03;BYMONTHDAY=28
SUMMARY:aa birthday (%d years old)
")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 5 17 1957) bb birthday (%d years old)"
   "DTSTART;VALUE=DATE:19570517
DTEND;VALUE=DATE:19570518
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=05;BYMONTHDAY=17
SUMMARY:bb birthday (%d years old)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 6 8 1997) cc birthday (%d years old)"
   "DTSTART;VALUE=DATE:19970608
DTEND;VALUE=DATE:19970609
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=06;BYMONTHDAY=08
SUMMARY:cc birthday (%d years old)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 7 22 1983) dd (%d years ago...!)"
   "DTSTART;VALUE=DATE:19830722
DTEND;VALUE=DATE:19830723
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=07;BYMONTHDAY=22
SUMMARY:dd (%d years ago...!)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 8 1 1988) ee birthday (%d years old)"
   "DTSTART;VALUE=DATE:19880801
DTEND;VALUE=DATE:19880802
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=08;BYMONTHDAY=01
SUMMARY:ee birthday (%d years old)")

  (icalendar-tests--test-export
   nil
   nil
   "%%(diary-anniversary 9 21 1957) ff birthday (%d years old)"
   "DTSTART;VALUE=DATE:19570921
DTEND;VALUE=DATE:19570922
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=09;BYMONTHDAY=21
SUMMARY:ff birthday (%d years old)")


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
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
VERSION
 :2.0
PRODID
 :-//Mozilla.org/NONSGML Mozilla Calendar V1.0//EN
BEGIN:VEVENT
SUMMARY
 :Jjjjj & Wwwww
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
DTSTART
 :20041123T140000
DTEND
 :20041123T143000
DTSTAMP
 :20041118T013430Z
LAST-MODIFIED
 :20041118T013640Z
END:VEVENT
BEGIN:VEVENT
SUMMARY
 :BB Aaaaaaaa Bbbbb
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
DTSTART
 :20041123T144500
DTEND
 :20041123T154500
DTSTAMP
 :20041118T013641Z
END:VEVENT
BEGIN:VEVENT
SUMMARY
 :Hhhhhhhh
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
DTSTART
 :20041123T110000
DTEND
 :20041123T120000
DTSTAMP
 :20041118T013831Z
END:VEVENT
BEGIN:VEVENT
SUMMARY
 :MMM Aaaaaaaaa
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
X-MOZILLA-RECUR-DEFAULT-INTERVAL
 :2
RRULE
 :FREQ=WEEKLY;INTERVAL=2;BYDAY=FR
DTSTART
 :20041112T140000
DTEND
 :20041112T183000
DTSTAMP
 :20041118T014117Z
END:VEVENT
BEGIN:VEVENT
SUMMARY
 :Rrrr/Cccccc ii Aaaaaaaa
DESCRIPTION
 :Vvvvv Rrrr aaa Cccccc
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
DTSTART
 ;VALUE=DATE
 :20041119
DTEND
 ;VALUE=DATE
 :20041120
DTSTAMP
 :20041118T013107Z
LAST-MODIFIED
 :20041118T014203Z
END:VEVENT
BEGIN:VEVENT
SUMMARY
 :Wwww aa hhhh
STATUS
 :TENTATIVE
CLASS
 :PRIVATE
X-MOZILLA-ALARM-DEFAULT-LENGTH
 :0
RRULE
 :FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
DTSTART
 ;VALUE=DATE
 :20041101
DTEND
 ;VALUE=DATE
 :20041102
DTSTAMP
 :20041118T014045Z
LAST-MODIFIED
 :20041118T023846Z
END:VEVENT
END:VCALENDAR
"
   nil
   "&23/11/2004 14:00-14:30 Jjjjj & Wwwww
 Status: TENTATIVE
 Class: PRIVATE
&23/11/2004 14:45-15:45 BB Aaaaaaaa Bbbbb
 Status: TENTATIVE
 Class: PRIVATE
&23/11/2004 11:00-12:00 Hhhhhhhh
 Status: TENTATIVE
 Class: PRIVATE
&%%(and (diary-cyclic 14 12 11 2004)) 14:00-18:30 MMM Aaaaaaaaa
 Status: TENTATIVE
 Class: PRIVATE
&%%(and (diary-block 19 11 2004 19 11 2004)) Rrrr/Cccccc ii Aaaaaaaa
 Desc: Vvvvv Rrrr aaa Cccccc
 Status: TENTATIVE
 Class: PRIVATE
&%%(and (diary-cyclic 7 1 11 2004)) Wwww aa hhhh
 Status: TENTATIVE
 Class: PRIVATE
"
   "&11/23/2004 14:00-14:30 Jjjjj & Wwwww
 Status: TENTATIVE
 Class: PRIVATE
&11/23/2004 14:45-15:45 BB Aaaaaaaa Bbbbb
 Status: TENTATIVE
 Class: PRIVATE
&11/23/2004 11:00-12:00 Hhhhhhhh
 Status: TENTATIVE
 Class: PRIVATE
&%%(and (diary-cyclic 14 11 12 2004)) 14:00-18:30 MMM Aaaaaaaaa
 Status: TENTATIVE
 Class: PRIVATE
&%%(and (diary-block 11 19 2004 11 19 2004)) Rrrr/Cccccc ii Aaaaaaaa
 Desc: Vvvvv Rrrr aaa Cccccc
 Status: TENTATIVE
 Class: PRIVATE
&%%(and (diary-cyclic 7 11 1 2004)) Wwww aa hhhh
 Status: TENTATIVE
 Class: PRIVATE
")

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
  (icalendar-tests--test-import
   "UID
 :b60d398e-1dd1-11b2-a159-cf8cb05139f4
SUMMARY
 :Waitangi Day
DESCRIPTION
 :abcdef
CATEGORIES
 :Public Holiday
STATUS
 :CONFIRMED
CLASS
 :PRIVATE
DTSTART
 ;VALUE=DATE
 :20050206
DTEND
 ;VALUE=DATE
 :20050207
DTSTAMP
 :20050128T011209Z"
   nil
   "&%%(and (diary-block 6 2 2005 6 2 2005)) Waitangi Day
 Desc: abcdef
 Status: CONFIRMED
 Class: PRIVATE
 UID: b60d398e-1dd1-11b2-a159-cf8cb05139f4
"
   "&%%(and (diary-block 2 6 2005 2 6 2005)) Waitangi Day
 Desc: abcdef
 Status: CONFIRMED
 Class: PRIVATE
 UID: b60d398e-1dd1-11b2-a159-cf8cb05139f4
")

  ;; 2005-03-01 lt
  (icalendar-tests--test-import
   "DTSTART;VALUE=DATE:20050217
SUMMARY:Hhhhhh Aaaaa ii Aaaaaaaa
UID:6AFA7558-6994-11D9-8A3A-000A95A0E830-RID
DTSTAMP:20050118T210335Z
DURATION:P7D"
   nil
   "&%%(and (diary-block 17 2 2005 23 2 2005)) Hhhhhh Aaaaa ii Aaaaaaaa
 UID: 6AFA7558-6994-11D9-8A3A-000A95A0E830-RID\n"
   "&%%(and (diary-block 2 17 2005 2 23 2005)) Hhhhhh Aaaaa ii Aaaaaaaa
 UID: 6AFA7558-6994-11D9-8A3A-000A95A0E830-RID\n")

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
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR
METHOD:REQUEST
PRODID:Microsoft Exchange Server 2007
VERSION:2.0
BEGIN:VTIMEZONE
TZID:(UTC+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna
BEGIN:STANDARD
DTSTART:16010101T030000
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=-1SU;BYMONTH=10
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:16010101T020000
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=-1SU;BYMONTH=3
END:DAYLIGHT
END:VTIMEZONE
BEGIN:VEVENT
ORGANIZER;CN=\"A. Luser\":MAILTO:a.luser@foo.com
ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=TRUE;CN=\"Luser, Oth
 er\":MAILTO:other.luser@foo.com
DESCRIPTION;LANGUAGE=en-US:\nWhassup?\n\n
SUMMARY;LANGUAGE=en-US:Query
DTSTART;TZID=\"(UTC+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna\"
 :20120515T150000
DTEND;TZID=\"(UTC+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna\":2
 0120515T153000
UID:040000008200E00074C5B7101A82E0080000000020FFAED0CFEFCC01000000000000000
 010000000575268034ECDB649A15349B1BF240F15
RECURRENCE-ID;TZID=\"(UTC+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, V
 ienna\":20120515T170000
CLASS:PUBLIC
PRIORITY:5
DTSTAMP:20120514T153645Z
TRANSP:OPAQUE
STATUS:CONFIRMED
SEQUENCE:15
LOCATION;LANGUAGE=en-US:phone
X-MICROSOFT-CDO-APPT-SEQUENCE:15
X-MICROSOFT-CDO-OWNERAPPTID:1907632092
X-MICROSOFT-CDO-BUSYSTATUS:TENTATIVE
X-MICROSOFT-CDO-INTENDEDSTATUS:BUSY
X-MICROSOFT-CDO-ALLDAYEVENT:FALSE
X-MICROSOFT-CDO-IMPORTANCE:1
X-MICROSOFT-CDO-INSTTYPE:3
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:REMINDER
TRIGGER;RELATED=START:-PT15M
END:VALARM
END:VEVENT
END:VCALENDAR"
   nil
   "&15/5/2012 15:00-15:30 Query
 Location: phone
 Organizer: MAILTO:a.luser@foo.com
 Status: CONFIRMED
 Class: PUBLIC
 UID: 040000008200E00074C5B7101A82E0080000000020FFAED0CFEFCC01000000000000000010000000575268034ECDB649A15349B1BF240F15
"     nil)

  ;; 2015-12-05, mixed line endings and empty lines, see Bug#22092.
  (icalendar-tests--test-import
   "BEGIN:VCALENDAR\r
PRODID:-//www.norwegian.no//iCalendar MIMEDIR//EN\r
VERSION:2.0\r
METHOD:REQUEST\r
BEGIN:VEVENT\r
UID:RFCALITEM1\r
SEQUENCE:1512040950\r
DTSTAMP:20141204T095043Z\r
ORGANIZER:noreply@norwegian.no\r
DTSTART:20141208T173000Z\r

DTEND:20141208T215500Z\r

LOCATION:Stavanger-Sola\r

DESCRIPTION:Fly med Norwegian, reservasjon. Fra Stavanger til Troms&#248; 8. des 2014 18:30, DY545Fly med Norwegian, reservasjon . Fra Stavanger til Troms&#248; 8. des 2014 21:00, DY390\r

X-ALT-DESC;FMTTYPE=text/html:<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\"><html><head><META NAME=\"Generator\" CONTENT=\"MS Exchange Server version 08.00.0681.000\"><title></title></head><body><b><font face=\"Calibri\" size=\"3\">Reisereferanse</p></body></html>
SUMMARY:Norwegian til Tromsoe-Langnes -\r

CATEGORIES:Appointment\r


PRIORITY:5\r

CLASS:PUBLIC\r

TRANSP:OPAQUE\r
END:VEVENT\r
END:VCALENDAR
"
"&2014/12/8 18:30-22:55 Norwegian til Tromsoe-Langnes -
 Desc: Fly med Norwegian, reservasjon. Fra Stavanger til Troms&#248; 8. des 2014 18:30, DY545Fly med Norwegian, reservasjon . Fra Stavanger til Troms&#248; 8. des 2014 21:00, DY390
 Location: Stavanger-Sola
 Organizer: noreply@norwegian.no
 Class: PUBLIC
 UID: RFCALITEM1
"
"&8/12/2014 18:30-22:55 Norwegian til Tromsoe-Langnes -
 Desc: Fly med Norwegian, reservasjon. Fra Stavanger til Troms&#248; 8. des 2014 18:30, DY545Fly med Norwegian, reservasjon . Fra Stavanger til Troms&#248; 8. des 2014 21:00, DY390
 Location: Stavanger-Sola
 Organizer: noreply@norwegian.no
 Class: PUBLIC
 UID: RFCALITEM1
"
"&12/8/2014 18:30-22:55 Norwegian til Tromsoe-Langnes -
 Desc: Fly med Norwegian, reservasjon. Fra Stavanger til Troms&#248; 8. des 2014 18:30, DY545Fly med Norwegian, reservasjon . Fra Stavanger til Troms&#248; 8. des 2014 21:00, DY390
 Location: Stavanger-Sola
 Organizer: noreply@norwegian.no
 Class: PUBLIC
 UID: RFCALITEM1
"
)
  )

(provide 'icalendar-tests)
;;; icalendar-tests.el ends here
