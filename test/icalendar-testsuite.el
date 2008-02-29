;; icalendar-testsuite.el --- Test suite for icalendar.el

;; Copyright (C) 2005, 2008  Free Software Foundation, Inc.

;; Author:         Ulf Jasper <ulf.jasper@web.de>
;; Created:        March 2005
;; Keywords:       calendar
;; Human-Keywords: calendar, diary, iCalendar, vCalendar

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; TODO:
;; - Add more unit tests for functions, timezone etc.

;;; Code:
(defun icalendar-testsuite-run ()
  "Run icalendar test suite."
  (interactive)
  (icalendar-testsuite--run-function-tests)
  (icalendar-testsuite--run-import-tests)
  (icalendar-testsuite--run-export-tests)
  (icalendar-testsuite--run-cycle-tests)
  (icalendar-testsuite--run-real-world-tests)
  (message "All icalendar tests finished successfully."))

;; ======================================================================
;; Test methods for functions
;; ======================================================================
(defun icalendar-testsuite--run-function-tests ()
  "Perform tests for single icalendar functions."
  (icalendar-testsuite--test-parse-summary-and-rest)
  (icalendar-testsuite--test-format-ical-event)
  (icalendar-testsuite--test-import-format-sample)
  (icalendar-testsuite--test-first-weekday-of-year))

(defun icalendar-testsuite--test-format-ical-event ()
  "Test icalendar--format-ical-event"
  (let ((icalendar-import-format "%s%d%l%o%t%u%c")
        (icalendar-import-format-summary "SUM %s")
        (icalendar-import-format-location " LOC %s")
        (icalendar-import-format-description " DES %s")
        (icalendar-import-format-organizer " ORG %s")
        (icalendar-import-format-status " STA %s")
        (icalendar-import-format-url " URL %s")
        (icalendar-import-format-class " CLA %s")
        (was-european-calendar european-calendar-style)
        (event (icalendar-testsuite--get-ical-event "BEGIN:VEVENT
DTSTAMP:20030509T043439Z
DTSTART:20030509T103000
SUMMARY:sum
ORGANIZER:org
LOCATION:loc
DTEND:20030509T153000
DESCRIPTION:des
END:VEVENT
")))
    (assert (string= (icalendar--format-ical-event event)
                     "SUM sum DES des LOC loc ORG org") t)
    (setq icalendar-import-format (lambda (&rest ignore)
                                    "helloworld"))
    (assert (string= (icalendar--format-ical-event event)
                     "helloworld") t)
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
    (assert (string= (icalendar--format-ical-event event)
                     "-sum-des-loc-org-nil-nil-nil-") t)))

(defun icalendar-testsuite--test-parse-summary-and-rest ()
  "Test icalendar--parse-summary-and-rest."
  (let ((icalendar-import-format "%s%d%l%o%t%u%c")
        (icalendar-import-format-summary "SUM %s")
        (icalendar-import-format-location " LOC %s")
        (icalendar-import-format-description " DES %s")
        (icalendar-import-format-organizer " ORG %s")
        (icalendar-import-format-status " STA %s")
        (icalendar-import-format-url " URL %s")
        (icalendar-import-format-class " CLA %s")
        (was-european-calendar european-calendar-style)
        (result))
    ;; FIXME: need a trailing blank char!
    (setq result (icalendar--parse-summary-and-rest "SUM sum ORG org "))
    (assert (string= (cdr (assoc 'org result)) "org"))

    (setq result (icalendar--parse-summary-and-rest
                  "SUM sum DES des LOC loc ORG org STA sta URL url CLA cla "))
    (assert (string= (cdr (assoc 'des result)) "des"))
    (assert (string= (cdr (assoc 'loc result)) "loc"))
    (assert (string= (cdr (assoc 'org result)) "org"))
    (assert (string= (cdr (assoc 'sta result)) "sta"))
    (assert (string= (cdr (assoc 'cla result)) "cla"))

    (setq icalendar-import-format (lambda () "Hello world"))
    (setq result (icalendar--parse-summary-and-rest
                  "blah blah "))
    (assert (not result))
    ))

(defun icalendar-testsuite--get-ical-event (ical-string)
  "Helper function for testing `icalendar-testsuite--test-format-ical-event'."
  (save-excursion
    (with-temp-buffer
      (insert ical-string)
      (goto-char (point-min))
      (car (icalendar--read-element nil nil)))))

(defun icalendar-testsuite--test-import-format-sample ()
  "Test method for `icalendar-import-format-sample'."
  (assert (string= (icalendar-import-format-sample
                    (icalendar-testsuite--get-ical-event "BEGIN:VEVENT
DTSTAMP:20030509T043439Z
DTSTART:20030509T103000
SUMMARY:a
ORGANIZER:d
LOCATION:c
DTEND:20030509T153000
DESCRIPTION:b
END:VEVENT
"))
                   (concat "SUMMARY=`a' DESCRIPTION=`b' LOCATION=`c' "
                           "ORGANIZER=`d' STATUS=`' URL=`' CLASS=`'"))))

(defun icalendar-testsuite--test-first-weekday-of-year ()
  (assert (eq 1 (icalendar-first-weekday-of-year "TU" 2008)))
  (assert (eq 3 (icalendar-first-weekday-of-year "WE" 2007)))
  (assert (eq 5 (icalendar-first-weekday-of-year "TH" 2006)))
  (assert (eq 7 (icalendar-first-weekday-of-year "FR" 2005)))
  (assert (eq 3 (icalendar-first-weekday-of-year "SA" 2004)))
  (assert (eq 5 (icalendar-first-weekday-of-year "SU" 2003)))
  (assert (eq 7 (icalendar-first-weekday-of-year "MO" 2002)))
  (assert (eq 3 (icalendar-first-weekday-of-year "MO" 2000)))
  (assert (eq 1 (icalendar-first-weekday-of-year "TH" 1970))))

;; ======================================================================
;; Test methods for exporting from diary to icalendar
;; ======================================================================

(defun icalendar-testsuite--test-export (input-european input-american
                                                        expected-output)
  "Perform an export test.
Argument INPUT-EUROPEAN european style diary string.
Argument INPUT-AMERICAN american style diary string.
Argument EXPECTED-OUTPUT expected icalendar result string."
  (message "--- icalendar-testsuite--test-export ---")
  (let ((was-european-calendar european-calendar-style)
        (icalendar-recurring-start-year 2000))
    (set-time-zone-rule "CET") ;;FIXME: reset timezone!
    (when input-european
      (let ((calendar-month-name-array
             ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August"
              "September" "Oktober" "November" "Dezember"])
            (calendar-day-name-array
             ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag"
              "Samstag"]))
        (european-calendar)
        (icalendar-testsuite--do-test-export input-european expected-output)))
    (when input-american
      (let ((calendar-month-name-array
             ["January" "February" "March" "April" "May" "June" "July" "August"
              "September" "October" "November" "December"])
            (calendar-day-name-array
             ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
              "Saturday"]))
        (american-calendar)
        (icalendar-testsuite--do-test-export input-american expected-output)))
    (if was-european-calendar
        (european-calendar)
      (american-calendar))))

(defun icalendar-testsuite--do-test-export (input expected-output)
  "Actually perform export test.
Argument INPUT input diary string.
Argument EXPECTED-OUTPUT expected icalendar result string."
  (let ((temp-file (make-temp-file "icalendar-testsuite-ics")))
    (with-temp-buffer
      (insert input)
      (icalendar-export-region (point-min) (point-max) temp-file))
    (save-excursion
      (find-file temp-file)
      (goto-char (point-min))
      (unless
          (cond (expected-output
                 (and (re-search-forward "^\\s-*BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0
BEGIN:VEVENT
UID:emacs[0-9]+
\\(\\(.\\|\n\\)+\\)
END:VEVENT
END:VCALENDAR
\\s-*$"
                                         nil t)
                      (string-match
                       (concat "^\\s-*"
                               (regexp-quote (buffer-substring-no-properties
                                              (match-beginning 1) (match-end 1)))
                               "\\s-*$")
                       expected-output)))
                (t
                 (re-search-forward "^\\s-*BEGIN:VCALENDAR
PRODID:-//Emacs//NONSGML icalendar.el//EN
VERSION:2.0
END:VCALENDAR
\\s-*$"
                                    nil t)))
        (error
         "Export test failed! Input: `%s'\nFound:\n\n%s\n\nbut expected\n\n%s"
         input
         (or (and (match-beginning 1)
                  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
             "<nil>")
         (or expected-output "<nil>"))))
    (kill-buffer (find-buffer-visiting temp-file))
    (delete-file temp-file)))

;; ======================================================================
;; Test methods for importing from icalendar to diary
;; ======================================================================

(defun icalendar-testsuite--test-import (input expected-european
                                               expected-american)
  "Perform import test.
Argument INPUT icalendar event string.
Argument EXPECTED-EUROPEAN expected european style diary string.
Argument EXPECTED-AMERICAN expected american style diary string."
  (message "--- icalendar-testsuite--test-import ---")
  (let ((timezone (cadr (current-time-zone))))
    (set-time-zone-rule "CET")
    (with-temp-buffer
      (if (string-match "^BEGIN:VCALENDAR" input)
          (insert input)
        (insert "BEGIN:VCALENDAR\nPRODID:-//Emacs//NONSGML icalendar.el//EN\n")
        (insert "VERSION:2.0\nBEGIN:VEVENT\n")
        (insert input)
        (unless (eq (char-before) ?\n)
          (insert "\n"))
        (insert "END:VEVENT\nEND:VCALENDAR\n"))
      (let ((icalendar-import-format "%s%d%l%o%t%u%c")
            (icalendar-import-format-summary "%s")
            (icalendar-import-format-location "\n Location: %s")
            (icalendar-import-format-description "\n Desc: %s")
            (icalendar-import-format-organizer "\n Organizer: %s")
            (icalendar-import-format-status "\n Status: %s")
            (icalendar-import-format-url "\n URL: %s")
            (icalendar-import-format-class "\n Class: %s")
            (was-european-calendar european-calendar-style))
        (when expected-european
          (european-calendar)
          (icalendar-testsuite--do-test-import input expected-european))
        (when expected-american
          (american-calendar)
          (icalendar-testsuite--do-test-import input expected-american))
        (if was-european-calendar
            (european-calendar)
          (american-calendar))))
    (set-time-zone-rule timezone)))

(defun icalendar-testsuite--do-test-import (input expected-output)
  "Actually perform import test.
Argument INPUT input icalendar string.
Argument EXPECTED-OUTPUT expected diary string."
  (let ((temp-file (make-temp-file "icalendar-test-diary")))
    (icalendar-import-buffer temp-file t t)
    (save-excursion
      (find-file temp-file)
      (let ((result (buffer-substring-no-properties (point-min) (point-max))))
        (unless (string-match (concat "^\\s-*" expected-output "\\s-*$")
                              result)
          (error "Import test failed! Found `%s'\nbut expected `%s'" result
                 expected-output)))
      (kill-buffer (find-buffer-visiting temp-file))
      (delete-file temp-file))))

;; ======================================================================
;; Test methods for cycle...
;; ======================================================================
(defun icalendar-testsuite--test-cycle (input)
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
    (let ((icalendar-import-format "%s%d%l%o%t%u%c")
          (icalendar-import-format-summary "%s")
          (icalendar-import-format-location "\n Location: %s")
          (icalendar-import-format-description "\n Desc: %s")
          (icalendar-import-format-organizer "\n Organizer: %s")
          (icalendar-import-format-status "\n Status: %s")
          (icalendar-import-format-url "\n URL: %s")
          (icalendar-import-format-class "\n Class: %s")
          (was-european-calendar european-calendar-style))
      (european-calendar)
      (icalendar-testsuite--do-test-cycle)
      (american-calendar)
      (icalendar-testsuite--do-test-cycle)
      (if was-european-calendar
          (european-calendar)
        (american-calendar)))))

(defun icalendar-testsuite--do-test-cycle ()
  "Actually perform import/export cycle test."
  (let ((temp-diary (make-temp-file "icalendar-test-diary"))
        (temp-ics (make-temp-file "icalendar-test-ics"))
        (org-input (buffer-substring-no-properties (point-min) (point-max))))
    (icalendar-import-buffer temp-diary t t)
    (save-excursion
      (find-file temp-diary)
      (icalendar-export-region (point-min) (point-max) temp-ics))
    (save-excursion
      (find-file temp-ics)
      (goto-char (point-min))
      (when (re-search-forward "\nUID:.*\n" nil t)
        (replace-match "\n"))
      (let ((cycled (buffer-substring-no-properties (point-min) (point-max))))
        (unless (string-equal org-input cycled)
          (error "Import test failed! Found `%s'\nbut expected `%s'" cycled
                 org-input))))
    (kill-buffer (find-buffer-visiting temp-diary))
    (save-excursion
      (set-buffer (find-buffer-visiting temp-ics))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (delete-file temp-diary)
    (delete-file temp-ics)))

;; ======================================================================
;; Import tests
;; ======================================================================
(defun icalendar-testsuite--run-import-tests ()
  "Perform standard import tests."
  (icalendar-testsuite--test-import
   "SUMMARY:non-recurring
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000"
   "&19/9/2003 09:00-11:30 non-recurring"
   "&9/19/2003 09:00-11:30 non-recurring")

  (icalendar-testsuite--test-import
   "SUMMARY:non-recurring allday
DTSTART;VALUE=DATE-TIME:20030919"
   "&19/9/2003 non-recurring allday"
   "&9/19/2003 non-recurring allday")

  (icalendar-testsuite--test-import
   "SUMMARY:long 
 summary
DTSTART;VALUE=DATE:20030919"
   "&19/9/2003 long summary"
   "&9/19/2003 long summary")

  (icalendar-testsuite--test-import
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
   "&%%(and (diary-block 19 7 2004 27 8 2004)) Sommerferien"
   "&%%(and (diary-block 7 19 2004 8 27 2004)) Sommerferien")
  (icalendar-testsuite--test-import
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
   "&23/11/2004 14:00-14:30 folded summary"
   "&11/23/2004 14:00-14:30 folded summary")
  (icalendar-testsuite--test-import
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
   "&23/11/2004 14:45-15:45 another example"
   "&11/23/2004 14:45-15:45 another example")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule daily
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;
"
   "&%%(and (diary-cyclic 1 19 9 2003)) 09:00-11:30 rrule daily"
   "&%%(and (diary-cyclic 1 9 19 2003)) 09:00-11:30 rrule daily")

  ;; RRULE examples
  (icalendar-testsuite--test-import
   "SUMMARY:rrule daily
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;INTERVAL=2
"
   "&%%(and (diary-cyclic 2 19 9 2003)) 09:00-11:30 rrule daily"
   "&%%(and (diary-cyclic 2 9 19 2003)) 09:00-11:30 rrule daily")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule daily with exceptions
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;INTERVAL=2
EXDATE:20030921,20030925
"
   "&%%(and (not (diary-date 25 9 2003)) (not (diary-date 21 9 2003)) (diary-cyclic 2 19 9 2003)) 09:00-11:30 rrule daily with exceptions"
   "&%%(and (not (diary-date 9 25 2003)) (not (diary-date 9 21 2003)) (diary-cyclic 2 9 19 2003)) 09:00-11:30 rrule daily with exceptions")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule weekly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=WEEKLY;
"
   "&%%(and (diary-cyclic 7 19 9 2003)) 09:00-11:30 rrule weekly"
   "&%%(and (diary-cyclic 7 9 19 2003)) 09:00-11:30 rrule weekly")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule monthly no end
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;
"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 1 1 9999)) 09:00-11:30 rrule monthly no end"
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 1 1 9999)) 09:00-11:30 rrule monthly no end")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule monthly with end
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;UNTIL=20050819;
"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 19 8 2005)) 09:00-11:30 rrule monthly with end"
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 8 19 2005)) 09:00-11:30 rrule monthly with end")
  (icalendar-testsuite--test-import
   "DTSTART;VALUE=DATE:20040815
DTEND;VALUE=DATE:20040816
SUMMARY:Maria Himmelfahrt
UID:CC56BEA6-49D2-11D8-8833-00039386D1C2-RID
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=8
"
   "&%%(and (diary-anniversary 15 8 2004))  Maria Himmelfahrt"
   "&%%(and (diary-anniversary 8 15 2004))  Maria Himmelfahrt")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule yearly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=YEARLY;INTERVAL=2
"
   "&%%(and (diary-anniversary 19 9 2003)) 09:00-11:30 rrule yearly" ;FIXME
   "&%%(and (diary-anniversary 9 19 2003)) 09:00-11:30 rrule yearly") ;FIXME
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count daily short
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;COUNT=1;INTERVAL=1
"
   "&%%(and (diary-cyclic 1 19 9 2003) (diary-block 19 9 2003 19 9 2003)) 09:00-11:30 rrule count daily short"
   "&%%(and (diary-cyclic 1 9 19 2003) (diary-block 9 19 2003 9 19 2003)) 09:00-11:30 rrule count daily short")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count daily long
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=DAILY;COUNT=14;INTERVAL=1
"
   "&%%(and (diary-cyclic 1 19 9 2003) (diary-block 19 9 2003 2 10 2003)) 09:00-11:30 rrule count daily long"
   "&%%(and (diary-cyclic 1 9 19 2003) (diary-block 9 19 2003 10 2 2003)) 09:00-11:30 rrule count daily long")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count bi-weekly 3 times
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=WEEKLY;COUNT=3;INTERVAL=2
"
   "&%%(and (diary-cyclic 14 19 9 2003) (diary-block 19 9 2003 31 10 2003)) 09:00-11:30 rrule count bi-weekly 3 times"
   "&%%(and (diary-cyclic 14 9 19 2003) (diary-block 9 19 2003 10 31 2003)) 09:00-11:30 rrule count bi-weekly 3 times")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count monthly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;INTERVAL=1;COUNT=5
"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 19 1 2004)) 09:00-11:30 rrule count monthly"
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 1 19 2004)) 09:00-11:30 rrule count monthly")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count every second month
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=5
"
   "&%%(and (diary-date 19 t t) (diary-block 19 9 2003 19 5 2004)) 09:00-11:30 rrule count every second month" ;FIXME
   "&%%(and (diary-date t 19 t) (diary-block 9 19 2003 5 19 2004)) 09:00-11:30 rrule count every second month") ;FIXME
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count yearly
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=YEARLY;INTERVAL=1;COUNT=5
"
   "&%%(and (diary-date 19 9 t) (diary-block 19 9 2003 19 9 2007)) 09:00-11:30 rrule count yearly"
   "&%%(and (diary-date 9 19 t) (diary-block 9 19 2003 9 19 2007)) 09:00-11:30 rrule count yearly")
  (icalendar-testsuite--test-import
   "SUMMARY:rrule count every second year
DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=5
"
   "&%%(and (diary-date 19 9 t) (diary-block 19 9 2003 19 9 2011)) 09:00-11:30 rrule count every second year" ;FIXME!!!
   "&%%(and (diary-date 9 19 t) (diary-block 9 19 2003 9 19 2011)) 09:00-11:30 rrule count every second year") ;FIXME!!!

  ;; duration
  (icalendar-testsuite--test-import
   "DTSTART;VALUE=DATE:20050217
SUMMARY:duration
DURATION:P7D
"
   "&%%(and (diary-block 17 2 2005 23 2 2005)) duration"
   "&%%(and (diary-block 2 17 2005 2 23 2005)) duration")

  (icalendar-testsuite--test-import
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
   "&%%(and (diary-cyclic 1 21 12 2001) (diary-block 21 12 2001 29 12 2001))  Urlaub"
   "&%%(and (diary-cyclic 1 12 21 2001) (diary-block 12 21 2001 12 29 2001))  Urlaub")
  )

;; ======================================================================
;; Export tests
;; ======================================================================
(defun icalendar-testsuite--run-export-tests ()
  "Perform standard export tests."

  (let ((icalendar-export-hidden-diary-entries nil))
    (icalendar-testsuite--test-export
     "&3 Okt 2000 ordinary no time "
     "&Oct 3 2000 ordinary no time "
     nil))

  ;; "ordinary" events
  (icalendar-testsuite--test-export
   "3 Okt 2000 ordinary no time "
   "Oct 3 2000 ordinary no time "
   "DTSTART;VALUE=DATE:20001003
DTEND;VALUE=DATE:20001004
SUMMARY:ordinary no time 
")
  (icalendar-testsuite--test-export
   "3 Okt 2000 16:30 ordinary with time"
   "Oct 3 2000 16:30 ordinary with time"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:ordinary with time
")
  (icalendar-testsuite--test-export
   "3 10 2000 16:30 ordinary with time 2"
   "10 3 2000 16:30 ordinary with time 2"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:ordinary with time 2
")

  (icalendar-testsuite--test-export
   "3/10/2000 16:30 ordinary with time 3"
   "10/3/2000 16:30 ordinary with time 3"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:ordinary with time 3
")

  ;; multiline -- FIXME!!!
  (icalendar-testsuite--test-export
   "3 Oktober 2000 16:30 multiline
  17:30 multiline continued FIXME"
   "October 3 2000 16:30 multiline
  17:30 multiline continued FIXME"
   "DTSTART;VALUE=DATE-TIME:20001003T163000
DTEND;VALUE=DATE-TIME:20001003T173000
SUMMARY:multiline
DESCRIPTION:
  17:30 multiline continued FIXME
")

  ;; weekly by day
  (icalendar-testsuite--test-export
   "Montag 13:30 weekly by day with start time"
   "Monday 1:30pm weekly by day with start time"
   "DTSTART;VALUE=DATE-TIME:20000103T133000
DTEND;VALUE=DATE-TIME:20000103T143000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:weekly by day with start time
")

  (icalendar-testsuite--test-export
   "Montag 13:30-15:00 weekly by day with start and end time"
   "Monday 01:30pm-03:00pm weekly by day with start and end time"
   "DTSTART;VALUE=DATE-TIME:20000103T133000
DTEND;VALUE=DATE-TIME:20000103T150000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:weekly by day with start and end time
")

  ;; yearly
  (icalendar-testsuite--test-export
   "1 Mai yearly no time"
   "may 1 yearly no time"
   "DTSTART;VALUE=DATE:19000501
DTEND;VALUE=DATE:19000502
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=5;BYMONTHDAY=1
SUMMARY:yearly no time
")

  ;; anniversaries
  (icalendar-testsuite--test-export
   "%%(diary-anniversary 3 10 1989) anniversary no time"
   "%%(diary-anniversary 10 3 1989) anniversary no time"
   "DTSTART;VALUE=DATE:19891003
DTEND;VALUE=DATE:19891004
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=10;BYMONTHDAY=03
SUMMARY:anniversary no time
")
  (icalendar-testsuite--test-export
   "%%(diary-anniversary 3 10 1989) 19:00-20:00 anniversary with time"
   "%%(diary-anniversary 10 3 1989) 19:00-20:00 anniversary with time"
   "DTSTART;VALUE=DATE-TIME:19891003T190000
DTEND;VALUE=DATE-TIME:19891004T200000
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=10;BYMONTHDAY=03
SUMMARY:anniversary with time
")

  ;; block
  (icalendar-testsuite--test-export
   "%%(diary-block 18 6 2001 6 7 2001) block no time"
   "%%(diary-block 6 18 2001 7 6 2001) block no time"
   "DTSTART;VALUE=DATE:20010618
DTEND;VALUE=DATE:20010707
SUMMARY:block no time
")
  (icalendar-testsuite--test-export
   "%%(diary-block 18 6 2001 6 7 2001) 13:00-17:00 block with time"
   "%%(diary-block 6 18 2001 7 6 2001) 13:00-17:00 block with time"
   "DTSTART;VALUE=DATE-TIME:20010618T130000
DTEND;VALUE=DATE-TIME:20010618T170000
RRULE:FREQ=DAILY;INTERVAL=1;UNTIL=20010706
SUMMARY:block with time
")
  (icalendar-testsuite--test-export
   "%%(diary-block 18 6 2001 6 7 2001) 13:00 block no end time"
   "%%(diary-block 6 18 2001 7 6 2001) 13:00 block no end time"
   "DTSTART;VALUE=DATE-TIME:20010618T130000
DTEND;VALUE=DATE-TIME:20010618T140000
RRULE:FREQ=DAILY;INTERVAL=1;UNTIL=20010706
SUMMARY:block no end time
")
  )

;; ======================================================================
;; Real world
;; ======================================================================
(defun icalendar-testsuite--run-real-world-tests ()
  "Perform real-world tests, as gathered from problem reports."
  ;; 2003-05-29
  (icalendar-testsuite--test-import
   "BEGIN:VCALENDAR
METHOD:REQUEST
PRODID:Microsoft CDO for Microsoft Exchange
VERSION:2.0
BEGIN:VTIMEZONE
TZID:Kolkata\, Chennai\, Mumbai\, New Delhi
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
   "&9/5/2003 10:30-15:30 On-Site Interview
 Desc: 10:30am - Blah
 Location: Cccc
 Organizer: MAILTO:aaaaaaa@aaaaaaa.com"
   "&5/9/2003 10:30-15:30 On-Site Interview
 Desc: 10:30am - Blah
 Location: Cccc
 Organizer: MAILTO:aaaaaaa@aaaaaaa.com")

  ;; 2003-06-18 a
  (icalendar-testsuite--test-import
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
   "&23/6/2003 11:00-12:00 Dress Rehearsal for XXXX-XXXX
 Desc: 753 Zeichen hier radiert
 Location: 555 or TN 555-5555 ID 5555 & NochWas (see below)
 Organizer: MAILTO:xxx@xxxxx.com"
   "&6/23/2003 11:00-12:00 Dress Rehearsal for XXXX-XXXX
 Desc: 753 Zeichen hier radiert
 Location: 555 or TN 555-5555 ID 5555 & NochWas (see below)
 Organizer: MAILTO:xxx@xxxxx.com")

  ;; 2003-06-18 b -- uses timezone
  (icalendar-testsuite--test-import
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
   "&23/6/2003 17:00-18:00 Updated: Dress Rehearsal for ABC01-15
 Desc: Viele Zeichen standen hier früher
 Location: 123 or TN 123-1234 ID abcd & SonstWo (see below)
 Organizer: MAILTO:bbb@bbbbb.com
 Status: CONFIRMED"
   "&6/23/2003 17:00-18:00 Updated: Dress Rehearsal for ABC01-15
 Desc: Viele Zeichen standen hier früher
 Location: 123 or TN 123-1234 ID abcd & SonstWo (see below)
 Organizer: MAILTO:bbb@bbbbb.com
 Status: CONFIRMED")

  ;; export 2004-10-28 block entries
  (icalendar-testsuite--test-export
   nil
   "-*- mode: text; fill-column: 256;-*-

>>>  block entries:

%%(diary-block 11 8 2004 11 10 2004) Nov 8-10 aa
"
   "DTSTART;VALUE=DATE:20041108
DTEND;VALUE=DATE:20041111
SUMMARY:Nov 8-10 aa")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-block 12 13 2004 12 17 2004) Dec 13-17 bb"
   "DTSTART;VALUE=DATE:20041213
DTEND;VALUE=DATE:20041218
SUMMARY:Dec 13-17 bb")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-block 2 3 2005 2 4 2005) Feb 3-4 cc"
   "DTSTART;VALUE=DATE:20050203
DTEND;VALUE=DATE:20050205
SUMMARY:Feb 3-4 cc")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-block 4 24 2005 4 29 2005) April 24-29 dd"
   "DTSTART;VALUE=DATE:20050424
DTEND;VALUE=DATE:20050430
SUMMARY:April 24-29 dd
")
  (icalendar-testsuite--test-export
   nil
   "%%(diary-block 5 30 2005 6 1 2005) may 30 - June 1: ee"
   "DTSTART;VALUE=DATE:20050530
DTEND;VALUE=DATE:20050602
SUMMARY:may 30 - June 1: ee")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-block 6 6 2005 6 8 2005) ff"
   "DTSTART;VALUE=DATE:20050606
DTEND;VALUE=DATE:20050609
SUMMARY:ff")

  ;; export 2004-10-28 anniversary entries
  (icalendar-testsuite--test-export
   nil
   "
>>> anniversaries:

%%(diary-anniversary 3 28 1991) aa birthday (%d years old)"
   "DTSTART;VALUE=DATE:19910328
DTEND;VALUE=DATE:19910329
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=03;BYMONTHDAY=28
SUMMARY:aa birthday (%d years old)
")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-anniversary 5 17 1957) bb birthday (%d years old)"
   "DTSTART;VALUE=DATE:19570517
DTEND;VALUE=DATE:19570518
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=05;BYMONTHDAY=17
SUMMARY:bb birthday (%d years old)")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-anniversary 6 8 1997) cc birthday (%d years old)"
   "DTSTART;VALUE=DATE:19970608
DTEND;VALUE=DATE:19970609
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=06;BYMONTHDAY=08
SUMMARY:cc birthday (%d years old)")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-anniversary 7 22 1983) dd (%d years ago...!)"
   "DTSTART;VALUE=DATE:19830722
DTEND;VALUE=DATE:19830723
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=07;BYMONTHDAY=22
SUMMARY:dd (%d years ago...!)")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-anniversary 8 1 1988) ee birthday (%d years old)"
   "DTSTART;VALUE=DATE:19880801
DTEND;VALUE=DATE:19880802
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=08;BYMONTHDAY=01
SUMMARY:ee birthday (%d years old)")

  (icalendar-testsuite--test-export
   nil
   "%%(diary-anniversary 9 21 1957) ff birthday (%d years old)"
   "DTSTART;VALUE=DATE:19570921
DTEND;VALUE=DATE:19570922
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=09;BYMONTHDAY=21
SUMMARY:ff birthday (%d years old)")


  ;; FIXME!

  ;; export 2004-10-28 monthly, weekly entries

  ;;   (icalendar-testsuite--test-export
  ;;    nil
  ;;    "
  ;; >>> ------------ monthly:

  ;; */27/* 10:00 blah blah"
  ;; "xxx")

  (icalendar-testsuite--test-export
   nil
   ">>> ------------ my week:

Monday 13:00 MAC"
   "DTSTART;VALUE=DATE-TIME:20000103T130000
DTEND;VALUE=DATE-TIME:20000103T140000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:MAC")

  (icalendar-testsuite--test-export
   nil
   "Monday 15:00 a1"
   "DTSTART;VALUE=DATE-TIME:20000103T150000
DTEND;VALUE=DATE-TIME:20000103T160000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:a1")


  (icalendar-testsuite--test-export
   nil
   "Monday 16:00-17:00 a2"
   "DTSTART;VALUE=DATE-TIME:20000103T160000
DTEND;VALUE=DATE-TIME:20000103T170000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO
SUMMARY:a2")

  (icalendar-testsuite--test-export
   nil
   "Tuesday 11:30-13:00 a3"
   "DTSTART;VALUE=DATE-TIME:20000104T113000
DTEND;VALUE=DATE-TIME:20000104T130000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU
SUMMARY:a3")

  (icalendar-testsuite--test-export
   nil
   "Tuesday 15:00 a4"
   "DTSTART;VALUE=DATE-TIME:20000104T150000
DTEND;VALUE=DATE-TIME:20000104T160000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU
SUMMARY:a4")

  (icalendar-testsuite--test-export
   nil
   "Wednesday 13:00 a5"
   "DTSTART;VALUE=DATE-TIME:20000105T130000
DTEND;VALUE=DATE-TIME:20000105T140000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=WE
SUMMARY:a5")

  (icalendar-testsuite--test-export
   nil
   "Wednesday 11:30-13:30 a6"
   "DTSTART;VALUE=DATE-TIME:20000105T113000
DTEND;VALUE=DATE-TIME:20000105T133000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=WE
SUMMARY:a6")

  (icalendar-testsuite--test-export
   nil
   "Wednesday 15:00 s1"
   "DTSTART;VALUE=DATE-TIME:20000105T150000
DTEND;VALUE=DATE-TIME:20000105T160000
RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=WE
SUMMARY:s1")


  ;; export 2004-10-28 regular entries
  (icalendar-testsuite--test-export
   nil
   "
>>> regular diary entries:

Oct 12 2004, 14:00 Tue: [2004-10-12] q1"
   "DTSTART;VALUE=DATE-TIME:20041012T140000
DTEND;VALUE=DATE-TIME:20041012T150000
SUMMARY:Tue: [2004-10-12] q1")

  ;; 2004-11-19
  (icalendar-testsuite--test-import
   "BEGIN:VCALENDAR
VERSION
 :2.0
PRODID
 :-//Mozilla.org/NONSGML Mozilla Calendar V1.0//EN
BEGIN:VEVENT
UID
 :04979712-3902-11d9-93dd-8f9f4afe08da
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
UID
 :6161a312-3902-11d9-b512-f764153bb28b
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
UID
 :943a4d7e-3902-11d9-9ce7-c9addeadf928
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
UID
 :fe53615e-3902-11d9-9dd8-9d38a155bf41
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
UID
 :87c928ee-3901-11d9-b21f-b45042155024
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
UID
 :e8f331ae-3902-11d9-9948-dfdcb66a2872
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
 Class: PRIVATE "
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
 Class: PRIVATE ")

  ;; 2004-09-09 pg
  (icalendar-testsuite--test-export
   "%%(diary-block 1 1 2004 4 1 2004) Urlaub"
   nil
   "DTSTART;VALUE=DATE:20040101
DTEND;VALUE=DATE:20040105
SUMMARY:Urlaub")

  ;; 2004-10-25 pg
  (icalendar-testsuite--test-export
   "5 11 2004 Bla Fasel"
   nil
   "DTSTART;VALUE=DATE:20041105
DTEND;VALUE=DATE:20041106
SUMMARY:Bla Fasel")

  ;; 2004-10-30 pg
  (icalendar-testsuite--test-export
   "2 Nov 2004 15:00-16:30 Zahnarzt"
   nil
   "DTSTART;VALUE=DATE-TIME:20041102T150000
DTEND;VALUE=DATE-TIME:20041102T163000
SUMMARY:Zahnarzt")

  ;; 2005-02-07 lt
  (icalendar-testsuite--test-import
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
   "&%%(and (diary-block 6 2 2005 6 2 2005)) Waitangi Day
 Desc: abcdef"
   "&%%(and (diary-block 2 6 2005 2 6 2005)) Waitangi Day
 Desc: abcdef")

  ;; 2005-03-01 lt
  (icalendar-testsuite--test-import
   "DTSTART;VALUE=DATE:20050217
SUMMARY:Hhhhhh Aaaaa ii Aaaaaaaa
UID:6AFA7558-6994-11D9-8A3A-000A95A0E830-RID
DTSTAMP:20050118T210335Z
DURATION:P7D"
   "&%%(and (diary-block 17 2 2005 23 2 2005)) Hhhhhh Aaaaa ii Aaaaaaaa"
   "&%%(and (diary-block 2 17 2005 2 23 2005)) Hhhhhh Aaaaa ii Aaaaaaaa")

  ;; 2005-03-23 lt
  (icalendar-testsuite--test-export
   "&%%(diary-cyclic 7 8 2 2005) 16:00-16:45 [WORK] Pppp"
   nil
   "DTSTART;VALUE=DATE-TIME:20050208T160000
DTEND;VALUE=DATE-TIME:20050208T164500
RRULE:FREQ=DAILY;INTERVAL=7
SUMMARY:[WORK] Pppp
")

  ;; 2005-05-27 eu
  (icalendar-testsuite--test-export
   nil
   ;; FIXME: colon not allowed!
   ;;"Nov 1: NNN Wwwwwwww Wwwww - Aaaaaa Pppppppp rrrrrr ddd oo Nnnnnnnn 30"
   "Nov 1 NNN Wwwwwwww Wwwww - Aaaaaa Pppppppp rrrrrr ddd oo Nnnnnnnn 30"
   "DTSTART;VALUE=DATE:19001101
DTEND;VALUE=DATE:19001102
RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=11;BYMONTHDAY=1
SUMMARY:NNN Wwwwwwww Wwwww - Aaaaaa Pppppppp rrrrrr ddd oo Nnnnnnnn 30
")
  )

(defun icalendar-testsuite--run-cycle-tests ()
  (icalendar-testsuite--test-cycle
   "DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
SUMMARY:Cycletest
")

  (icalendar-testsuite--test-cycle
   "DTSTART;VALUE=DATE-TIME:20030919T090000
DTEND;VALUE=DATE-TIME:20030919T113000
SUMMARY:Cycletest
DESCRIPTION:beschreibung!
LOCATION:nowhere
ORGANIZER:ulf
")

  ;; FIXME: does not work
  ;;  (icalendar-testsuite--test-cycle
  ;;   "DTSTART;VALUE=DATE:19190909
  ;;DTEND;VALUE=DATE:19190910
  ;;RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=09;BYMONTHDAY=09
  ;;SUMMARY:and diary-anniversary
  ;;")
  )


(provide 'icalendar-testsuite)

;; arch-tag: 33a98396-90e9-49c8-b0e9-b606386d6e8c
;;; icalendar-testsuite.el ends here
