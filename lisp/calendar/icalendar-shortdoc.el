;;; icalendar-shortdoc.el --- Short documentation for iCalendar  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: April 2026
;; Keywords: calendar, help
;; Human-Keywords: calendar, iCalendar

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines a short documentation group for the main functions
;; of the iCalendar API.  It does not aim to cover every function in the
;; library; it is a guide to the high-level functions for Lisp
;; programmers who want to interpret or produce iCalendar data.

;; To read this documentation, do:
;; M-x load-libraray RET icalendar-shortdoc RET
;; M-x shortdoc RET icalendar RET


;;; Code:
(require 'shortdoc)
(require 'icalendar-macs)
(require 'icalendar-parser)
(require 'icalendar-utils)
(require 'icalendar-ast)
(require 'icalendar-recur)

(define-short-documentation-group icalendar
  "Parsing and printing VCALENDAR objects"
  (icalendar-parse
   :no-manual t
   :no-eval* (icalendar-parse)
   :result-string "(icalendar-vcalendar ...)")
  (icalendar-parse-and-index
   :no-manual t
   :no-eval* (icalendar-parse-and-index)
   :result (vcalendar index))
  (icalendar-index-get
   :args (index &rest kwargs)
   :no-manual t
   :no-eval (icalendar-index-get index :uid "some-event-UID")
   :eg-result-string "(icalendar-vevent ...)"
   :no-eval (icalendar-index-get index :date '(5 28 2026))
   :eg-result-string "((icalendar-vevent ...) ...)"
   :no-eval (icalendar-index-get index :tzid "Europe/Vienna")
   :eg-result-string "(icalendar-vtimezone ...)")
  (icalendar-parse-from-string
   :no-eval
   (icalendar-parse-from-string 'icalendar-rrule "RRULE:FREQ=WEEKLY\n")
   :result-string "(icalendar-rrule ...)")
  (icalendar-print-calendar-node
   :no-manual t
   :no-eval (icalendar-print-calendar-node vcalendar)
   :result "BEGIN:VCALENDAR...")
  "Constructing syntax nodes"
  (icalendar-make-vcalendar
   :no-manual t
   :no-eval
   "(icalendar-make-vcalendar
   some-vtimezone
   (@ list-of-vevents)
   ...)"
   :result-string "(icalendar-vcalendar ...)")
  (icalendar-make-vevent
   :no-manual t
   :no-eval
   "(icalendar-make-vevent
   (icalendar-summary \"Party\")
   (icalendar-location \"Robot House\")
   (icalendar-organizer \"mailto:bender@mars.edu\"
      (icalendar-cnparam \"Bender B. Rodriguez\"))
   (icalendar-attendee  \"mailto:philip.j.fry@mars.edu\"
      (icalendar-partstatparam \"ACCEPTED\"))
   (icalendar-dtstart '(3 13 3003))
   (icalendar-rrule '((FREQ MONTHLY))))"
   :result-string "(icalendar-vevent ...)")
  (icalendar-make-vtodo
   :no-manual t
   :no-eval
   "(icalendar-make-vtodo
   (icalendar-summary \"Sell another doomsday device\")
   (icalendar-description \"Need funds for electronium hat experiment.\")
   (icalendar-due '(6 6 3002)))"
   :result-string "(icalendar-vtodo ...)")
  (icalendar-make-vjournal
   :no-manual t
   :no-eval
   "(icalendar-make-vjournal
   (icalendar-summary \"Results: Electronium Hat Experiment\")
   (icalendar-description \"How do you like them bananas?\")
   (icalendar-dtstart '(8 6 3002)))"
   :result-string "(icalendar-vjournal ...)")
  "Binding values in syntax nodes"
  (icalendar-with-component
      :args (node bindings &rest body)
    :no-manual t
    :no-eval
    "(icalendar-with-component vevent
     ((icalendar-summary :value summary)
      (icalendar-location :value location))
    (format \"%s @ %s\" summary location))"
    :eg-result "Party @ Robot House")
  (icalendar-with-property
      :args (node bindings &rest body)
    :no-manual t
    :no-eval "(icalendar-with-property location-node nil
    (downcase value))"
    :eg-result "robot house")
  (icalendar-with-property-of
      :args (component property-type bindings &rest body)
      :no-manual t
      :no-eval
      "(icalendar-with-property-of vevent 'icalendar-location nil
    (downcase value))"
      :eg-result "robot house")
  (icalendar-with-param
      :args (parameter &rest body)
      :no-manual t
      :no-eval "(icalendar-with-param cnparam-node
    (upcase value))"
      :eg-result "BENDER B. RODRIGUEZ")
  (icalendar-with-param-of
      :args (property param-type &rest body)
    :no-manual t
    :no-eval "(icalendar-with-param-of organizer-node 'icalendar-cnparam
    (upcase value))"
    :eg-result "BENDER B. RODRIGUEZ")
  "Time zones"
  (icalendar-recur-current-tz-to-vtimezone
   :no-manual t
   :no-eval (icalendar-recur-current-tz-to-vtimezone)
   :result-string "(icalendar-vtimezone ...)")
  (icalendar-recur-tz-decode-time
   :no-manual t
   :no-eval (icalendar-recur-tz-decode-time timestamp vtimezone)
   :eg-result (0 0 11 11 11 2024 1 nil 3600))
  (icalendar-recur-tz-set-zone
   :no-eval (icalendar-recur-tz-set-zone '(0 0 11 11 11 2024 1 -1 nil) vtimezone)
   :eg-result (0 0 11 11 11 2024 1 nil 3600))
  "Dates and date-times"
  (icalendar-make-date-time
   :no-manual t
   :args (&rest kwargs)
   :no-eval
   "(icalendar-make-date-time :year 2024 :month 12 :day 11
                            :hour 10 :minute 0 :second 0
                            :tz vtimezone)"
   :eg-result (0 0 10 11 12 2024 3 -1 3600))
  (icalendar-date/time<=
   :eval (icalendar-date/time<= '(12 15 2024) '(12 11 2024))
   :eval "(icalendar-date/time<= '(0 0 8 11 11 2024 1 nil 3600)
                         '(0 0 10 11 11 2024 3 nil 3600))"
   :no-manual t)
  (icalendar-date/time<
   :no-manual t
   :eval (icalendar-date/time< '(12 15 2024) '(12 15 2024)))
  (icalendar-date/time-min
   :no-manual t
   :eval (icalendar-date/time-min '(12 15 2024) '(11 15 2024) '(12 1 2024))
   :eval
   "(icalendar-date/time-min '(0 0 8 11 12 2024 3 nil 3600)
                           '(0 0 9 11 12 2024 3 nil 3600)
                           '(0 0 10 11 12 2024 3 nil 3600))")
  (icalendar-date/time-max
   :no-manual t
   :eval (icalendar-date/time-max '(12 15 2024) '(11 15 2024) '(12 1 2024))
   :eval
   "(icalendar-date/time-max '(0 0 8 11 12 2024 3 nil 3600)
                           '(0 0 9 11 12 2024 3 nil 3600)
                           '(0 0 10 11 12 2024 3 nil 3600))")
  (icalendar-date/time-add
   :no-manual t
   :eval (icalendar-date/time-add '(11 11 2024) :month 2)
   :eval (icalendar-date/time-add '(0 0 10 11 11 2024 1 -1 nil) :hour -3))
  (icalendar-date/time-add-duration
   :no-manual t
   :eval "(icalendar-date/time-add-duration '(12 11 2024)
    (make-decoded-time :day 4))"
   :eval "(icalendar-date/time-add-duration '(0 0 10 11 12 2024 3 -1 nil)
    (make-decoded-time :hour 2 :minute 30))")
  (icalendar-date/time-year
   :no-manual t
   :eval (icalendar-date/time-year '(12 11 2024))
   :eval (icalendar-date/time-year '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-month
   :no-manual t
   :eval (icalendar-date/time-month '(12 11 2024))
   :eval (icalendar-date/time-month '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-monthday
   :no-manual t
   :eval (icalendar-date/time-monthday '(12 11 2024))
   :eval (icalendar-date/time-monthday '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-weekday
   :no-manual t
   :eval (icalendar-date/time-weekday '(12 11 2024))
   :eval (icalendar-date/time-weekday '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-weekno
   :no-manual t
   :eval (icalendar-date/time-weekno '(12 11 2024))
   :eval (icalendar-date/time-weekno '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-hour
   :no-manual t
   :eval (icalendar-date/time-hour '(12 11 2024))
   :eval (icalendar-date/time-hour '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-minute
   :no-manual t
   :eval (icalendar-date/time-minute '(12 11 2024))
   :eval (icalendar-date/time-minute '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-second
   :no-manual t
   :eval (icalendar-date/time-second '(12 11 2024))
   :eval (icalendar-date/time-second '(0 0 10 11 12 2024 3 nil 3600)))
  (icalendar-date/time-zone
   :no-manual t
   :eval (icalendar-date/time-zone '(12 11 2024))
   :eval (icalendar-date/time-zone '(0 0 10 11 12 2024 3 nil 3600)))
  "Recurrence rules and recurrences"
  (icalendar-recur-recurrences-in-window
   :no-manual t
   :no-eval
   "(icalendar-recur-recurrences-in-window '(1 1 2026) '(12 31 2026)
                                          vevent)"
   :eg-result-string "((1 10 2026) (2 10 2026) ...)")
  (icalendar-recur-recurrences-in-window-w/end-times
   :no-manual t
   :no-eval
   "(icalendar-recur-recurrences-in-window-w/end-times
     '(0 0 0 1 1 2026 4 -1 nil) '(0 0 0 1 2 2026 1 -1 nil)
     vevent)"
   :eg-result-string
   "(((0 0 10 10 1 2026 4 -1 nil) (0 0 11 10 1 2026 4 -1 nil)) ...)")
  (icalendar-recur-recurrences-to-count
   :no-manual t
   :no-eval
   "(icalendar-recur-recurrences-to-count '(1 1 2026) '(12 31 2026)
                                          vevent)"
   :eg-result-string "((1 10 2026) (2 10 2026) (3 10 2026))")
  (icalendar-recur-freq
   :eval
   (icalendar-recur-freq '((FREQ MONTHLY) (INTERVAL 3) (BYDAY ((5 . -1))))))
  (icalendar-recur-interval-size
   :eval (icalendar-recur-interval-size '((FREQ MONTHLY) (BYDAY ((5 . -1)))))
   :eval (icalendar-recur-interval-size '((FREQ MONTHLY) (INTERVAL 3))))
  (icalendar-recur-count
   :eval (icalendar-recur-count '((FREQ MONTHLY) (INTERVAL 2) (COUNT 6))))
  (icalendar-recur-until
   :eval (icalendar-recur-until '((FREQ WEEKLY) (UNTIL (12 31 2026)))))
  (icalendar-recur-by*
   :eval (icalendar-recur-by* 'BYDAY '((FREQ MONTHLY) (BYDAY ((5 . -1))))))
  (icalendar-recur-weekstart
   :eval
   (icalendar-recur-weekstart '((FREQ WEEKLY) (UNTIL (12 31 2026)) (WKST 0)))
   :eval
   (icalendar-recur-weekstart '((FREQ WEEKLY) (UNTIL (12 31 2026))))))

(provide 'icalendar-shortdoc)
