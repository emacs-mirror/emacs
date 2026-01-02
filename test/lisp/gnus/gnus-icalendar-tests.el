;;; gnus-icalendar-tests.el --- tests                -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Jan Tatarik <jan.tatarik@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'ert)
(require 'gnus-icalendar)


(defun gnus-icalendar-tests--get-ical-event (ical-string &optional participant)
  "Return gnus-icalendar event for ICAL-STRING."
  (let (event)
    (with-temp-buffer
      (insert ical-string)
      (setq event (gnus-icalendar-event-from-buffer (buffer-name) participant)))
    event))

(ert-deftest gnus-icalendar-parse ()
  "test"
  (let ((tz (getenv "TZ"))
        (event (gnus-icalendar-tests--get-ical-event "\
BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VTIMEZONE
TZID:America/New_York
X-LIC-LOCATION:America/New_York
BEGIN:DAYLIGHT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
DTSTART:19700308T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
DTSTART:19701101T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTART;TZID=America/New_York:20201208T090000
DTEND;TZID=America/New_York:20201208T100000
DTSTAMP:20200728T182853Z
ORGANIZER;CN=Company Events:mailto:anoncompany.com_3bm6fh805bme9uoeliqcle1sa
 g@group.calendar.google.com
UID:iipdt88slddpeu7hheuu09sfmd@google.com
X-MICROSOFT-CDO-OWNERAPPTID:-362490173
RECURRENCE-ID;TZID=America/New_York:20201208T091500
CREATED:20200309T134939Z
DESCRIPTION:In this meeting\\, we will cover topics from product and enginee
 ring presentations and demos to new hire announcements to watching the late
LAST-MODIFIED:20200728T182852Z
LOCATION:New York-22-Town Hall Space (250) [Chrome Box]
SEQUENCE:4
STATUS:CONFIRMED
SUMMARY:Townhall | All Company Meeting
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR
")))

    (unwind-protect
        (progn
          ;; Use this form so as not to rely on system tz database.
	  ;; Eg hydra.nixos.org.
          (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
          (should (eq (eieio-object-class event) 'gnus-icalendar-event-request))
          (should (not (gnus-icalendar-event:recurring-p event)))
          (should (string= (gnus-icalendar-event:start event) "2020-12-08 15:00"))
          (with-slots (organizer summary description location end-time uid rsvp participation-type) event
                      (should (string= organizer "anoncompany.com_3bm6fh805bme9uoeliqcle1sag@group.calendar.google.com"))
                      (should (string= summary "Townhall | All Company Meeting"))
                      (should (string= description "In this meeting, we will cover topics from product and engineering presentations and demos to new hire announcements to watching the late"))
                      (should (string= location "New York-22-Town Hall Space (250) [Chrome Box]"))
                      (should (string= (format-time-string "%Y-%m-%d %H:%M" end-time) "2020-12-08 16:00"))
                      (should (string= uid "iipdt88slddpeu7hheuu09sfmd@google.com"))
                      (should (not rsvp))
                      (should (eq participation-type 'non-participant))))
      (setenv "TZ" tz))))

(ert-deftest gnus-icalendary-byday ()
  ""
  (let ((tz (getenv "TZ"))
        (event (gnus-icalendar-tests--get-ical-event "\
BEGIN:VCALENDAR
PRODID:Zimbra-Calendar-Provider
VERSION:2.0
METHOD:REQUEST
BEGIN:VTIMEZONE
TZID:America/New_York
BEGIN:STANDARD
DTSTART:16010101T020000
TZOFFSETTO:-0500
TZOFFSETFROM:-0400
RRULE:FREQ=YEARLY;WKST=MO;INTERVAL=1;BYMONTH=11;BYDAY=1SU
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:16010101T020000
TZOFFSETTO:-0400
TZOFFSETFROM:-0500
RRULE:FREQ=YEARLY;WKST=MO;INTERVAL=1;BYMONTH=3;BYDAY=2SU
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
BEGIN:VEVENT
UID:903a5415-9067-4f63-b499-1b6205f49c88
RRULE:FREQ=DAILY;UNTIL=20200825T035959Z;INTERVAL=1;BYDAY=MO,TU,WE,TH,FR
SUMMARY:appointment every weekday\\, start jul 24\\, 2020\\, end aug 24\\, 2020
ATTENDEE;CN=Mark Hershberger;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP
 =TRUE:mailto:hexmode <at> gmail.com
ORGANIZER;CN=Mark A. Hershberger:mailto:mah <at> nichework.com
DTSTART;TZID=\"America/New_York\":20200724T090000
DTEND;TZID=\"America/New_York\":20200724T093000
STATUS:CONFIRMED
CLASS:PUBLIC
X-MICROSOFT-CDO-INTENDEDSTATUS:BUSY
TRANSP:OPAQUE
LAST-MODIFIED:20200719T150815Z
DTSTAMP:20200719T150815Z
SEQUENCE:0
DESCRIPTION:The following is a new meeting request:
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER;RELATED=START:-PT5M
DESCRIPTION:Reminder
END:VALARM
END:VEVENT
END:VCALENDAR" (list "Mark Hershberger"))))

    (unwind-protect
        (progn
          ;; Use this form so as not to rely on system tz database.
	  ;; Eg hydra.nixos.org.
          (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
    (should (eq (eieio-object-class event) 'gnus-icalendar-event-request))
    (should (gnus-icalendar-event:recurring-p event))
    (should (string= (gnus-icalendar-event:recurring-interval event) "1"))
    (should (string= (gnus-icalendar-event:start event) "2020-07-24 15:00"))
    (with-slots (organizer summary description location end-time uid rsvp participation-type) event
      (should (string= organizer "mah <at> nichework.com"))
      (should (string= summary "appointment every weekday, start jul 24, 2020, end aug 24, 2020"))
      (should (string= description "The following is a new meeting request:"))
      (should (null location))
      (should (string= (format-time-string "%Y-%m-%d %H:%M" end-time) "2020-07-24 15:30"))
      (should (string= uid "903a5415-9067-4f63-b499-1b6205f49c88"))
      (should rsvp)
      (should (eq participation-type 'required)))
    (should (equal (gnus-icalendar-event:recurring-days event) '(1 2 3 4 5)))
    (should (string= (gnus-icalendar-event:org-timestamp event) "<2020-07-24 15:00-15:30 +1w>
<2020-07-27 15:00-15:30 +1w>
<2020-07-28 15:00-15:30 +1w>
<2020-07-29 15:00-15:30 +1w>
<2020-07-30 15:00-15:30 +1w>")))
      (setenv "TZ" tz))))

(ert-deftest gnus-icalendary-weekly-byday ()
  ""
  (let ((tz (getenv "TZ"))
        (event (gnus-icalendar-tests--get-ical-event "\
BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VTIMEZONE
TZID:Europe/Berlin
X-LIC-LOCATION:Europe/Berlin
BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
TZNAME:CEST
DTSTART:19700329T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
TZNAME:CET
DTSTART:19701025T030000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTART;TZID=Europe/Berlin:20200915T140000
DTEND;TZID=Europe/Berlin:20200915T143000
RRULE:FREQ=WEEKLY;BYDAY=FR,MO,TH,TU,WE
DTSTAMP:20200915T120627Z
ORGANIZER;CN=anon@anoncompany.com:mailto:anon@anoncompany.com
UID:7b6g3m7iftuo90ei4ul00feqn_R20200915T120000@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;PARTSTAT=ACCEPTED;RSVP=TRUE
 ;CN=participant@anoncompany.com;X-NUM-GUESTS=0:mailto:participant@anoncompany.com
CREATED:20200325T095723Z
DESCRIPTION:Coffee talk
LAST-MODIFIED:20200915T120623Z
LOCATION:
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:Casual coffee talk
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR" (list "participant@anoncompany.com"))))

    (unwind-protect
        (progn
          ;; Use this form so as not to rely on system tz database.
	  ;; Eg hydra.nixos.org.
          (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
          (should (eq (eieio-object-class event) 'gnus-icalendar-event-request))
          (should (gnus-icalendar-event:recurring-p event))
          (should (string= (gnus-icalendar-event:recurring-interval event) "1"))
          (should (string= (gnus-icalendar-event:start event) "2020-09-15 14:00"))
          (with-slots (organizer summary description location end-time uid rsvp participation-type) event
            (should (string= organizer "anon@anoncompany.com"))
            (should (string= summary "Casual coffee talk"))
            (should (string= description "Coffee talk"))
            (should (string= location ""))
            (should (string= (format-time-string "%Y-%m-%d %H:%M" end-time) "2020-09-15 14:30"))
            (should (string= uid "7b6g3m7iftuo90ei4ul00feqn_R20200915T120000@google.com"))
            (should rsvp)
            (should (eq participation-type 'required)))
          (should (equal (sort (gnus-icalendar-event:recurring-days event) #'<) '(1 2 3 4 5)))
          (should (string= (gnus-icalendar-event:org-timestamp event) "<2020-09-15 14:00-14:30 +1w>
<2020-09-16 14:00-14:30 +1w>
<2020-09-17 14:00-14:30 +1w>
<2020-09-18 14:00-14:30 +1w>
<2020-09-21 14:00-14:30 +1w>")))
      (setenv "TZ" tz))))

(ert-deftest gnus-icalendar-accept-with-comment ()
  ""
  (let ((event "\
BEGIN:VEVENT
DTSTART;TZID=Europe/Berlin:20200915T140000
DTEND;TZID=Europe/Berlin:20200915T143000
RRULE:FREQ=WEEKLY;BYDAY=FR,MO,TH,TU,WE
DTSTAMP:20200915T120627Z
ORGANIZER;CN=anon@anoncompany.com:mailto:anon@anoncompany.com
UID:7b6g3m7iftuo90ei4ul00feqn_R20200915T120000@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;PARTSTAT=NEEDS-ACTION;RSVP=TRUE
 ;CN=participant@anoncompany.com;X-NUM-GUESTS=0:mailto:participant@anoncompany.com
CREATED:20200325T095723Z
DESCRIPTION:Coffee talk
LAST-MODIFIED:20200915T120623Z
LOCATION:
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:Casual coffee talk
TRANSP:OPAQUE
END:VEVENT")
        (icalendar-identities '("participant@anoncompany.com")))
    (let* ((reply (with-temp-buffer
                    (insert event)
                    (gnus-icalendar-event-reply-from-buffer
                     (current-buffer)
                     'accepted
                     icalendar-identities
                     "Can not stay long."))))
      (should (string-match "^ATTENDEE;.*?\\(PARTSTAT=[^;]+\\)" reply))
      (should (string-equal (match-string 1 reply) "PARTSTAT=ACCEPTED"))
      (should (string-match "^COMMENT:\\(.*\\)$" reply))
      (should (string-equal (match-string 1 reply) "Can not stay long.")))))

(ert-deftest gnus-icalendar-decline-without-changing-comment ()
  ""
  (let ((event "\
BEGIN:VEVENT
DTSTART;TZID=Europe/Berlin:20200915T140000
DTEND;TZID=Europe/Berlin:20200915T143000
RRULE:FREQ=WEEKLY;BYDAY=FR,MO,TH,TU,WE
DTSTAMP:20200915T120627Z
ORGANIZER;CN=anon@anoncompany.com:mailto:anon@anoncompany.com
UID:7b6g3m7iftuo90ei4ul00feqn_R20200915T120000@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;PARTSTAT=NEEDS-ACTION;RSVP=TRUE
 ;CN=participant@anoncompany.com;X-NUM-GUESTS=0:mailto:participant@anoncompany.com
CREATED:20200325T095723Z
DESCRIPTION:Coffee talk
LAST-MODIFIED:20200915T120623Z
COMMENT:Only available at 2pm
LOCATION:
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:Casual coffee talk
TRANSP:OPAQUE
END:VEVENT")
        (icalendar-identities '("participant@anoncompany.com")))
    (let* ((reply (with-temp-buffer
                    (insert event)
                    (gnus-icalendar-event-reply-from-buffer
                     (current-buffer)
                     'declined
                     icalendar-identities
                     nil))))
      (should (string-match "^ATTENDEE;.*?\\(PARTSTAT=[^;]+\\)" reply))
      (should (string-equal (match-string 1 reply) "PARTSTAT=DECLINED"))
      (should (string-match "^COMMENT:\\(.*\\)$" reply))
      (should (string-equal (match-string 1 reply) "Only available at 2pm"))
      )))

(provide 'gnus-icalendar-tests)
;;; gnus-icalendar-tests.el ends here
