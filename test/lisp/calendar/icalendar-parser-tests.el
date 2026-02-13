;;; tests/icalendar-parser.el --- Tests for icalendar-parser  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.

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

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'icalendar-macs))
(require 'ert)
(require 'ert-x)
(require 'icalendar-parser)
(require 'icalendar-utils)

(cl-defmacro ipt:parse/print-test (string &key expected parser type printer source)
  "Create a test which parses STRING, prints the resulting parse
tree, and compares the printed version with STRING (or with
EXPECTED, if given).  If they are the same, the test passes.
PARSER and PRINTER should be the parser and printer functions
appropriate to STRING.  TYPE, if given, should be the type of
object PARSER is expected to parse; it will be passed as PARSER's
first argument.  SOURCE should be a symbol; it is used to name the
test."
  (let ((parser-form
         (if type
             `(funcall (function ,parser) (quote ,type) (point-max))
           `(funcall (function ,parser) (point-max)))))
    `(ert-deftest ,(intern (concat "ipt:parse/print-" (symbol-name source))) ()
       ,(format "Parse and reprint example from `%s'; pass if they match" source)
       (let* ((parse-buf (get-buffer-create "*iCalendar Parse Test*"))
              (print-buf (get-buffer-create "*iCalendar Print Test*"))
              (unparsed ,string)
              (expected (or ,expected unparsed))
              (printed nil))
         (set-buffer parse-buf)
         (erase-buffer)
         (insert unparsed)
         (goto-char (point-min))
         (let ((parsed ,parser-form))
           (should (icalendar-ast-node-valid-p parsed))
           (set-buffer print-buf)
           (erase-buffer)
           (insert (funcall (function ,printer) parsed))
           ;; this may need adjusting if printers become coding-system aware:
           (decode-coding-region (point-min) (point-max) 'utf-8-dos)
           (setq printed (buffer-substring-no-properties (point-min) (point-max)))
           (should (equal expected printed)))))))

(ipt:parse/print-test
"ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.1.1/1)

(ipt:parse/print-test
"RDATE;VALUE=DATE:19970304,19970504,19970704,19970904\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.1.1/2)

(ipt:parse/print-test
"ATTACH:http://example.com/public/quarterly-report.doc\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.1.3/1)

(ipt:parse/print-test
;; Corrected.  The original contains invalid base64 data; it was
;; missing the final "=", as noted in errata ID 5602.
;; The decoded string should read:
;; The quick brown fox jumps over the lazy dog.
"ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.1.3/2)

(ipt:parse/print-test
"DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2/1)

(ipt:parse/print-test
"DESCRIPTION;ALTREP=\"CID:part3.msg.970415T083000@example.com\": Project XYZ Review Meeting will include the following agenda items: (a) Market Overview\\, (b) Finances\\, (c) Project Management\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.1/1)

(ipt:parse/print-test
"ORGANIZER;CN=\"John Smith\":mailto:jsmith@example.com\n"
;; CN param value does not require quotes, so they're missing when
;; re-printed:
:expected "ORGANIZER;CN=John Smith:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.2/1)

(ipt:parse/print-test
"ATTENDEE;CUTYPE=GROUP:mailto:ietf-calsch@example.org\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.3/1)

(ipt:parse/print-test
"ATTENDEE;DELEGATED-FROM=\"mailto:jsmith@example.com\":mailto:jdoe@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.4/1)

(ipt:parse/print-test
"ATTENDEE;DELEGATED-TO=\"mailto:jdoe@example.com\",\"mailto:jqpublic@example.com\":mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.5/1)

(ipt:parse/print-test
"ORGANIZER;DIR=\"ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)\":mailto:jimdo@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.6/1)

(ipt:parse/print-test
"ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.7/1)

(ipt:parse/print-test
"ATTACH;FMTTYPE=application/msword:ftp://example.com/pub/docs/agenda.doc\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.8/1)

(ipt:parse/print-test
"FREEBUSY;FBTYPE=BUSY:19980415T133000Z/19980415T170000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.9/1)

(ipt:parse/print-test
"SUMMARY;LANGUAGE=en-US:Company Holiday Party\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.10/1)

(ipt:parse/print-test
"LOCATION;LANGUAGE=en:Germany\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.10/2)

(ipt:parse/print-test
"LOCATION;LANGUAGE=no:Tyskland\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.10/3)

(ipt:parse/print-test
"ATTENDEE;MEMBER=\"mailto:ietf-calsch@example.org\":mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.11/1)

(ipt:parse/print-test
"ATTENDEE;MEMBER=\"mailto:projectA@example.com\",\"mailto:projectB@example.com\":mailto:janedoe@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.11/2)

(ipt:parse/print-test
"ATTENDEE;PARTSTAT=DECLINED:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.12/1)

(ipt:parse/print-test
"RECURRENCE-ID;RANGE=THISANDFUTURE:19980401T133000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.13/1)

(ipt:parse/print-test
"TRIGGER;RELATED=END:PT5M\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.14/1)

(ipt:parse/print-test
"RELATED-TO;RELTYPE=SIBLING:19960401-080045-4000F192713@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.15/1)

(ipt:parse/print-test
"ATTENDEE;ROLE=CHAIR:mailto:mrbig@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.16/1)

(ipt:parse/print-test
"ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.17/1)

(ipt:parse/print-test
"ORGANIZER;SENT-BY=\"mailto:sray@example.com\":mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.18/1)

(ipt:parse/print-test
"DTSTART;TZID=America/New_York:19980119T020000\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.19/1)

(ipt:parse/print-test
"DTEND;TZID=America/New_York:19980119T030000\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.2.19/2)

(ipt:parse/print-test
"ATTACH;FMTTYPE=image/vnd.microsoft.icon;ENCODING=BASE64;VALUE=BINARY:AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.3.1/1)

(ipt:parse/print-test
"TRUE"
:type icalendar-boolean
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.2/1)

(ipt:parse/print-test
"mailto:jane_doe@example.com"
:type icalendar-cal-address
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.3/1)

(ipt:parse/print-test
"19970714"
:type icalendar-date
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.4/1)

(ipt:parse/print-test
;; 'Floating' time:
"19980118T230000"
:type icalendar-date-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.5/1)

(ipt:parse/print-test
;; UTC time:
"19980119T070000Z"
:type icalendar-date-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.5/2)

(ipt:parse/print-test
;; Leap second (seconds = 60)
"19970630T235960Z"
:type icalendar-date-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.5/3)

(ipt:parse/print-test
;; Local time:
"DTSTART:19970714T133000\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.3.5/4)

(ipt:parse/print-test
;; UTC time:
"DTSTART:19970714T173000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.3.5/5)

(ipt:parse/print-test
;; Local time with TZ identifier:
"DTSTART;TZID=America/New_York:19970714T133000\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.3.5/6)

(ipt:parse/print-test
"P15DT5H0M20S"
:expected "P15DT5H20S"
:type icalendar-dur-value
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.6/1)

(ipt:parse/print-test
"P7W"
:type icalendar-dur-value
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.6/2)

(ipt:parse/print-test
"1000000.0000001"
:type icalendar-float
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.7/1)

(ipt:parse/print-test
"1.333"
:type icalendar-float
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.7/2)

(ipt:parse/print-test
"-3.14"
:type icalendar-float
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.7/3)

(ipt:parse/print-test
"1234567890"
:type icalendar-integer
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.8/1)

(ipt:parse/print-test
"-1234567890"
:type icalendar-integer
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.8/2)

(ipt:parse/print-test
"+1234567890"
;; "+" sign isn't required, so it's not re-printed:
:expected "1234567890"
:type icalendar-integer
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.8/3)

(ipt:parse/print-test
"432109876"
:type icalendar-integer
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.8/4)

(ipt:parse/print-test
"19970101T180000Z/19970102T070000Z"
:type icalendar-period
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.9/1)

(ipt:parse/print-test
"19970101T180000Z/PT5H30M"
:type icalendar-period
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.9/2)

(ipt:parse/print-test
"FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1"
:type icalendar-recur
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.10/1)

(ipt:parse/print-test
"FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30"
:type icalendar-recur
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.10/2)

(ipt:parse/print-test
"FREQ=DAILY;COUNT=10;INTERVAL=2"
:type icalendar-recur
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.10/3)

(ipt:parse/print-test
"Project XYZ Final Review\\nConference Room - 3B\\nCome Prepared."
:type icalendar-text
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.11/1)

(ipt:parse/print-test
;; Local time:
"230000"
:type icalendar-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.12/1)

(ipt:parse/print-test
;; UTC time:
"070000Z"
:type icalendar-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.12/2)

(ipt:parse/print-test
;; Local time:
"083000"
:type icalendar-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.12/3)

(ipt:parse/print-test
;; UTC time:
"133000Z"
:type icalendar-time
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.12/4)

(ipt:parse/print-test
;; Local time with TZ identifier:
"SOMETIMEPROP;TZID=America/New_York;VALUE=TIME:083000\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.3.12/5)

(ipt:parse/print-test
"http://example.com/my-report.txt"
:type icalendar-uri
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.13/1)

(ipt:parse/print-test
"-0500"
:type icalendar-utc-offset
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc5545-sec3.3.14/1)

(ipt:parse/print-test
"+0100"
:type icalendar-utc-offset
:parser icalendar-parse-value-node
:printer icalendar-print-value-node
:source rfc55453.3.14/1)

(ipt:parse/print-test
"BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec3.4/1)

(ipt:parse/print-test
"DTSTART:19960415T133000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.5/1)

(ipt:parse/print-test
"BEGIN:VEVENT
UID:19970901T130000Z-123401@example.com
DTSTAMP:19970901T130000Z
DTSTART:19970903T163000Z
DTEND:19970903T190000Z
SUMMARY:Annual Employee Review
CLASS:PRIVATE
CATEGORIES:BUSINESS,HUMAN RESOURCES
END:VEVENT
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.1/1)

(ipt:parse/print-test
"BEGIN:VEVENT
UID:19970901T130000Z-123402@example.com
DTSTAMP:19970901T130000Z
DTSTART:19970401T163000Z
DTEND:19970402T010000Z
SUMMARY:Laurel is in sensitivity awareness class.
CLASS:PUBLIC
CATEGORIES:BUSINESS,HUMAN RESOURCES
TRANSP:TRANSPARENT
END:VEVENT
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.1/2)

(ipt:parse/print-test
"BEGIN:VEVENT
UID:19970901T130000Z-123403@example.com
DTSTAMP:19970901T130000Z
DTSTART;VALUE=DATE:19971102
SUMMARY:Our Blissful Anniversary
TRANSP:TRANSPARENT
CLASS:CONFIDENTIAL
CATEGORIES:ANNIVERSARY,PERSONAL,SPECIAL OCCASION
RRULE:FREQ=YEARLY
END:VEVENT
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.1/3)

(ipt:parse/print-test
"BEGIN:VEVENT
UID:20070423T123432Z-541111@example.com
DTSTAMP:20070423T123432Z
DTSTART;VALUE=DATE:20070628
DTEND;VALUE=DATE:20070709
SUMMARY:Festival International de Jazz de Montreal
TRANSP:TRANSPARENT
END:VEVENT
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.1/4)

(ipt:parse/print-test
"BEGIN:VTODO
UID:20070313T123432Z-456553@example.com
DTSTAMP:20070313T123432Z
DUE;VALUE=DATE:20070501
SUMMARY:Submit Quebec Income Tax Return for 2006
CLASS:CONFIDENTIAL
CATEGORIES:FAMILY,FINANCE
STATUS:NEEDS-ACTION
END:VTODO
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.2/1)

(ipt:parse/print-test
"BEGIN:VTODO
UID:20070514T103211Z-123404@example.com
DTSTAMP:20070514T103211Z
DTSTART:20070514T110000Z
DUE:20070709T130000Z
COMPLETED:20070707T100000Z
SUMMARY:Submit Revised Internet-Draft
PRIORITY:1
STATUS:NEEDS-ACTION
END:VTODO
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.2/2)

(ipt:parse/print-test
"BEGIN:VJOURNAL
UID:19970901T130000Z-123405@example.com
DTSTAMP:19970901T130000Z
DTSTART;VALUE=DATE:19970317
SUMMARY:Staff meeting minutes
DESCRIPTION:1.  Staff meeting: Participants include Joe\\,Lisa\\, and Bob.  Aurora project plans were reviewed.  There is currently no budget reserves for this project.  Lisa will escalate to management.  Next meeting on Tuesday.\\n 2.  Telephone Conference: ABC Corp. sales representative called to discuss new printer.  Promised to get us a demo by Friday.\\n3.  Henry Miller (Handsoff Insurance): Car was totaled by tree.  Is looking into a loaner car. 555-2323 (tel).
END:VJOURNAL
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.3/1)

(ipt:parse/print-test
"BEGIN:VFREEBUSY
UID:19970901T082949Z-FA43EF@example.com
ORGANIZER:mailto:jane_doe@example.com
ATTENDEE:mailto:john_public@example.com
DTSTART:19971015T050000Z
DTEND:19971016T050000Z
DTSTAMP:19970901T083000Z
END:VFREEBUSY
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.4/1)

(ipt:parse/print-test
"BEGIN:VFREEBUSY
UID:19970901T095957Z-76A912@example.com
ORGANIZER:mailto:jane_doe@example.com
ATTENDEE:mailto:john_public@example.com
DTSTAMP:19970901T100000Z
FREEBUSY:19971015T050000Z/PT8H30M,19971015T160000Z/PT5H30M,19971015T223000Z/PT6H30M
URL:http://example.com/pub/busy/jpublic-01.ifb
COMMENT:This iCalendar file contains busy time information for the next three months.
END:VFREEBUSY
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.4/2)

(ipt:parse/print-test
;; Corrected.  Original has invalid value in ORGANIZER
"BEGIN:VFREEBUSY
UID:19970901T115957Z-76A912@example.com
DTSTAMP:19970901T120000Z
ORGANIZER:mailto:jsmith@example.com
DTSTART:19980313T141711Z
DTEND:19980410T141711Z
FREEBUSY:19980314T233000Z/19980315T003000Z
FREEBUSY:19980316T153000Z/19980316T163000Z
FREEBUSY:19980318T030000Z/19980318T040000Z
URL:http://www.example.com/calendar/busytime/jsmith.ifb
END:VFREEBUSY
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.4/3)

(ipt:parse/print-test
"BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:DAYLIGHT
DTSTART:19670430T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19740106T020000
RDATE:19750223T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19760425T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
END:VTIMEZONE
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.5/1)

(ipt:parse/print-test
"BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:STANDARD
DTSTART:20071104T020000
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20070311T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.5/2)

(ipt:parse/print-test
"BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
TZURL:http://zones.example.com/tz/America-New_York.ics
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.5/3)

(ipt:parse/print-test
"BEGIN:VTIMEZONE
TZID:Fictitious
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.5/4)

(ipt:parse/print-test
"BEGIN:VTIMEZONE
TZID:Fictitious
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19990424T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=4
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.5/5)

(ipt:parse/print-test
"BEGIN:VALARM
TRIGGER;VALUE=DATE-TIME:19970317T133000Z
REPEAT:4
DURATION:PT15M
ACTION:AUDIO
ATTACH;FMTTYPE=audio/basic:ftp://example.com/pub/sounds/bell-01.aud
END:VALARM
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.6/1)

(ipt:parse/print-test
"BEGIN:VALARM
TRIGGER:-PT30M
REPEAT:2
DURATION:PT15M
ACTION:DISPLAY
DESCRIPTION:Breakfast meeting with executive\\nteam at 8:30 AM EST.
END:VALARM
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.6/2)

(ipt:parse/print-test
"BEGIN:VALARM
TRIGGER;RELATED=END:-P2D
ACTION:EMAIL
ATTENDEE:mailto:john_doe@example.com
SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***
DESCRIPTION:A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST).  Attached is a pointer the document template for the agenda file.
ATTACH;FMTTYPE=application/msword:http://example.com/templates/agenda.doc
END:VALARM
"
:parser icalendar-parse-component
:printer icalendar-print-component-node
:source rfc5545-sec3.6.6/3)

(ipt:parse/print-test
"CALSCALE:GREGORIAN\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.7.1/1)

(ipt:parse/print-test
"METHOD:REQUEST\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.7.2/1)

(ipt:parse/print-test
"PRODID:-//ABC Corporation//NONSGML My Product//EN\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.7.3/1)

(ipt:parse/print-test
"VERSION:2.0\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.7./1)

(ipt:parse/print-test
"ATTACH:CID:jsmith.part3.960817T083000.xyzMail@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.1/1)

(ipt:parse/print-test
"ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/reports/r-960812.ps\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.1/2)

(ipt:parse/print-test
"CATEGORIES:APPOINTMENT,EDUCATION\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.2/1)

(ipt:parse/print-test
"CATEGORIES:MEETING\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.2/2)

(ipt:parse/print-test
"CLASS:PUBLIC\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.3/1)

(ipt:parse/print-test
"COMMENT:The meeting really needs to include both ourselves and the customer.  We can't hold this meeting without them.  As a matter of fact\\, the venue for the meeting ought to be at their site. - - John\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.4/1)

(ipt:parse/print-test
"DESCRIPTION:Meeting to provide technical review for \"Phoenix\" design.\\nHappy Face Conference Room.  Phoenix design team MUST attend this meeting.\\nRSVP to team leader.\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.5/1)

(ipt:parse/print-test
"GEO:37.386013;-122.082932\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.6/1)

(ipt:parse/print-test
"LOCATION:Conference Room - F123\\, Bldg. 002\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.7/1)

(ipt:parse/print-test
"LOCATION;ALTREP=\"http://xyzcorp.com/conf-rooms/f123.vcf\":Conference Room - F123\\, Bldg. 002\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.7/2)

(ipt:parse/print-test
"PERCENT-COMPLETE:39\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.8/1)

(ipt:parse/print-test
"PRIORITY:1\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.9/1)

(ipt:parse/print-test
"PRIORITY:2\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.9/2)

(ipt:parse/print-test
"PRIORITY:0\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.9/3)

(ipt:parse/print-test
"RESOURCES:EASEL,PROJECTOR,VCR\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.10/1)

(ipt:parse/print-test
"RESOURCES;LANGUAGE=fr:Nettoyeur haute pression\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.10/2)

(ipt:parse/print-test
"STATUS:TENTATIVE\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.11/1)

(ipt:parse/print-test
"STATUS:NEEDS-ACTION\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.11/2)

(ipt:parse/print-test
"STATUS:DRAFT\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.11/3)

(ipt:parse/print-test
"SUMMARY:Department Party\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.1.12/1)

(ipt:parse/print-test
"COMPLETED:19960401T150000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.1/1)

(ipt:parse/print-test
"DTEND:19960401T150000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.2/1)

(ipt:parse/print-test
"DTEND;VALUE=DATE:19980704\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.2/2)

(ipt:parse/print-test
"DUE:19980430T000000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.3/1)

(ipt:parse/print-test
"DTSTART:19980118T073000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.4/1)

(ipt:parse/print-test
"DURATION:PT1H0M0S\n"
;; 0M and 0S are not re-printed because they don't contribute to the duration:
:expected "DURATION:PT1H\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.5/1)

(ipt:parse/print-test
"DURATION:PT15M\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.5/2)

(ipt:parse/print-test
"FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:19970308T160000Z/PT8H30M\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.6/1)

(ipt:parse/print-test
"FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.6/2)

(ipt:parse/print-test
"FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H,19970308T230000Z/19970309T000000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.6/3)

(ipt:parse/print-test
"TRANSP:TRANSPARENT\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.7/1)

(ipt:parse/print-test
"TRANSP:OPAQUE\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.2.7/2)

(ipt:parse/print-test
"TZID:America/New_York\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.1/1)

(ipt:parse/print-test
"TZID:America/New_York\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.1/2)

(ipt:parse/print-test
"TZID:/example.org/America/New_York\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.1/3)

(ipt:parse/print-test
"TZNAME:EST\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.2/1)

(ipt:parse/print-test
"TZNAME;LANGUAGE=fr-CA:HNE\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.2/2)

(ipt:parse/print-test
"TZOFFSETFROM:-0500\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.3/1)

(ipt:parse/print-test
"TZOFFSETFROM:+1345\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.3/2)

(ipt:parse/print-test
"TZOFFSETTO:-0400\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.4/1)

(ipt:parse/print-test
"TZOFFSETTO:+1245\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.4/2)

(ipt:parse/print-test
"TZURL:http://timezones.example.org/tz/America-Los_Angeles.ics\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.3.5/1)

(ipt:parse/print-test
"ATTENDEE;MEMBER=\"mailto:DEV-GROUP@example.com\":mailto:joecool@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/1)

(ipt:parse/print-test
"ATTENDEE;DELEGATED-FROM=\"mailto:immud@example.com\":mailto:ildoit@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/2)

(ipt:parse/print-test
"ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry Cabot:mailto:hcabot@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/3)

(ipt:parse/print-test
"ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM=\"mailto:bob@example.com\";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/4)

(ipt:parse/print-test
"ATTENDEE;CN=John Smith;DIR=\"ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)\":mailto:jimdo@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/5)

(ipt:parse/print-test
"ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;DELEGATED-FROM=\"mailto:iamboss@example.com\";CN=Henry Cabot:mailto:hcabot@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/6)

(ipt:parse/print-test
"ATTENDEE;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO=\"mailto:hcabot@example.com\";CN=The Big Cheese:mailto:iamboss@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/7)

(ipt:parse/print-test
"ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/8)

(ipt:parse/print-test
;; Corrected.  Original lacks quotes around SENT-BY address.
"ATTENDEE;SENT-BY=\"mailto:jan_doe@example.com\";CN=John Smith:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.1/9)

(ipt:parse/print-test
"CONTACT:Jim Dolittle\\, ABC Industries\\, +1-919-555-1234\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.2/1)

(ipt:parse/print-test
;; Corrected.  Original contained unallowed backslash in ldap: URI
"CONTACT;ALTREP=\"ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)\":Jim Dolittle\\, ABC Industries\\,+1-919-555-1234\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.2/2)

(ipt:parse/print-test
"CONTACT;ALTREP=\"CID:part3.msg970930T083000SILVER@example.com\":Jim Dolittle\\, ABC Industries\\, +1-919-555-1234\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.2/3)

(ipt:parse/print-test
"CONTACT;ALTREP=\"http://example.com/pdi/jdoe.vcf\":Jim Dolittle\\, ABC Industries\\, +1-919-555-1234\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.2/4)

(ipt:parse/print-test
"ORGANIZER;CN=John Smith:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.3/1)

(ipt:parse/print-test
"ORGANIZER;CN=JohnSmith;DIR=\"ldap://example.com:6666/o=DC%20Associates,c=US???(cn=John%20Smith)\":mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.3/2)

(ipt:parse/print-test
"ORGANIZER;SENT-BY=\"mailto:jane_doe@example.com\":mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.3/3)

(ipt:parse/print-test
"RECURRENCE-ID;VALUE=DATE:19960401\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.4/1)

(ipt:parse/print-test
"RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.4/2)

(ipt:parse/print-test
"RELATED-TO:jsmith.part7.19960817T083000.xyzMail@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.5/1)

(ipt:parse/print-test
"RELATED-TO:19960401-080045-4000F192713-0052@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.5/2)

(ipt:parse/print-test
"URL:http://example.com/pub/calendars/jsmith/mytime.ics\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.6/1)

(ipt:parse/print-test
"UID:19960401T080045Z-4000F192713-0052@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.4.7/1)

(ipt:parse/print-test
"EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.1/1)

(ipt:parse/print-test
"RDATE:19970714T123000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.2/1)

(ipt:parse/print-test
"RDATE;TZID=America/New_York:19970714T083000\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.2/2)

(ipt:parse/print-test
"RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,19960404T010000Z/PT3H\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.2/3)

(ipt:parse/print-test
"RDATE;VALUE=DATE:19970101,19970120,19970217,19970421,19970526,19970704,19970901,19971014,19971128,19971129,19971225\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.2/4)

(ipt:parse/print-test
"RRULE:FREQ=DAILY;COUNT=10\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/1)

(ipt:parse/print-test
"RRULE:FREQ=DAILY;UNTIL=19971224T000000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/2)

(ipt:parse/print-test
"RRULE:FREQ=DAILY;INTERVAL=2\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/3)

(ipt:parse/print-test
"RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/4)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/5)

(ipt:parse/print-test
"RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/6)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;COUNT=10\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/7)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/8)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/9)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/10)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/11)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/12)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/13)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/14)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/15)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/16)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/17)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;BYMONTHDAY=-3\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/18)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/19)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/20)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/21)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/22)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/23)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/24)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/25)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;BYDAY=20MO\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/26)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/27)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/28)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/29)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/30)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/31)

(ipt:parse/print-test
"RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/32)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/33)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/34)

(ipt:parse/print-test
"RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/35)

(ipt:parse/print-test
"RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/36)

(ipt:parse/print-test
"RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/37)

(ipt:parse/print-test
"RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/38)

(ipt:parse/print-test
"RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/39)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/40)

(ipt:parse/print-test
"RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/41)

(ipt:parse/print-test
"RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.5.3/42)

(ipt:parse/print-test
"ACTION:AUDIO\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.6.1/1)

(ipt:parse/print-test
"ACTION:DISPLAY\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.6.1/2)

(ipt:parse/print-test
"REPEAT:4\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.6.2/1)

(ipt:parse/print-test
"TRIGGER:-PT15M\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.6.3/1)

(ipt:parse/print-test
"TRIGGER;RELATED=END:PT5M\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.6.3/2)

(ipt:parse/print-test
"TRIGGER;VALUE=DATE-TIME:19980101T050000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.6.3/3)

(ipt:parse/print-test
"CREATED:19960329T133000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.7.1/1)

(ipt:parse/print-test
"DTSTAMP:19971210T080000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.7.2/1)

(ipt:parse/print-test
"LAST-MODIFIED:19960817T133000Z\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.7.3/1)

(ipt:parse/print-test
"SEQUENCE:0\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.7.4/1)

(ipt:parse/print-test
"SEQUENCE:2\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.7.4/2)

(ipt:parse/print-test
"DRESSCODE:CASUAL\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.1/1)

(ipt:parse/print-test
"NON-SMOKING;VALUE=BOOLEAN:TRUE\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.1/2)

(ipt:parse/print-test
"X-ABC-MMSUBJ;VALUE=URI;FMTTYPE=audio/basic:http://www.example.org/mysubj.au\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.2/1)

(ipt:parse/print-test
"REQUEST-STATUS:2.0;Success\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.3/1)

(ipt:parse/print-test
"REQUEST-STATUS:3.1;Invalid property value;DTSTART:96-Apr-01\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.3/2)

(ipt:parse/print-test
"REQUEST-STATUS:2.8; Success\\, repeating event ignored.  Scheduled as a single event.;RRULE:FREQ=WEEKLY\\;INTERVAL=2\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.3/3)

(ipt:parse/print-test
"REQUEST-STATUS:4.1;Event conflict.  Date-time is busy.\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.3/4)

(ipt:parse/print-test
"REQUEST-STATUS:3.7;Invalid calendar user;ATTENDEE:mailto:jsmith@example.com\n"
:parser icalendar-parse-property
:printer icalendar-print-property-node
:source rfc5545-sec3.8.8.3/5)

(ipt:parse/print-test
"BEGIN:VCALENDAR
PRODID:-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN
VERSION:2.0
BEGIN:VEVENT
DTSTAMP:19960704T120000Z
UID:uid1@example.com
ORGANIZER:mailto:jsmith@example.com
DTSTART:19960918T143000Z
DTEND:19960920T220000Z
STATUS:CONFIRMED
CATEGORIES:CONFERENCE
SUMMARY:Networld+Interop Conference
DESCRIPTION:Networld+Interop Conference and Exhibit\\nAtlanta World Congress Center\\nAtlanta\\, Georgia
END:VEVENT
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec4/1)

(ipt:parse/print-test
"BEGIN:VCALENDAR
PRODID:-//RDU Software//NONSGML HandCal//EN
VERSION:2.0
BEGIN:VTIMEZONE
TZID:America/New_York
BEGIN:STANDARD
DTSTART:19981025T020000
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19990404T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
BEGIN:VEVENT
DTSTAMP:19980309T231000Z
UID:guid-1.example.com
ORGANIZER:mailto:mrbig@example.com
ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT;CUTYPE=GROUP:mailto:employee-A@example.com
DESCRIPTION:Project XYZ Review Meeting
CATEGORIES:MEETING
CLASS:PUBLIC
CREATED:19980309T130000Z
SUMMARY:XYZ Project Review
DTSTART;TZID=America/New_York:19980312T083000
DTEND;TZID=America/New_York:19980312T093000
LOCATION:1CP Conference Room 4350
END:VEVENT
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec4/2)

(ipt:parse/print-test
"BEGIN:VCALENDAR
METHOD:xyz
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VEVENT
DTSTAMP:19970324T120000Z
SEQUENCE:0
UID:uid3@example.com
ORGANIZER:mailto:jdoe@example.com
ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com
DTSTART:19970324T123000Z
DTEND:19970324T210000Z
CATEGORIES:MEETING,PROJECT
CLASS:PUBLIC
SUMMARY:Calendaring Interoperability Planning Meeting
DESCRIPTION:Discuss how we can test c&s interoperability\\nusing iCalendar and other IETF standards.
LOCATION:LDB Lobby
ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/conf/bkgrnd.ps
END:VEVENT
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec4/3)

(ipt:parse/print-test
;; Corrected.  The TRIGGER property originally did not specify
;; VALUE=DATE-TIME, which is required since it is not the default type.
;; See https://www.rfc-editor.org/errata/eid2039
"BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VTODO
DTSTAMP:19980130T134500Z
SEQUENCE:2
UID:uid4@example.com
ORGANIZER:mailto:unclesam@example.com
ATTENDEE;PARTSTAT=ACCEPTED:mailto:jqpublic@example.com
DUE:19980415T000000
STATUS:NEEDS-ACTION
SUMMARY:Submit Income Taxes
BEGIN:VALARM
ACTION:AUDIO
TRIGGER;VALUE=DATE-TIME:19980403T120000Z
ATTACH;FMTTYPE=audio/basic:http://example.com/pub/audio-files/ssbanner.aud
REPEAT:4
DURATION:PT1H
END:VALARM
END:VTODO
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec4/4)

(ipt:parse/print-test
"BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VJOURNAL
DTSTAMP:19970324T120000Z
UID:uid5@example.com
ORGANIZER:mailto:jsmith@example.com
STATUS:DRAFT
CLASS:PUBLIC
CATEGORIES:Project Report,XYZ,Weekly Meeting
DESCRIPTION:Project xyz Review Meeting Minutes\\nAgenda\\n1.  Review of project version 1.0 requirements.\\n2.Definitionof project processes.\\n3.  Review of project schedule.\\nParticipants: John Smith\\, Jane Doe\\, Jim Dandy\\n-It was decided that the requirements need to be signed off byproduct marketing.\\n-P roject processes were accepted.\\n-Project schedule needs to account for scheduled holidaysand employee vacation time.  Check with HR for specificdates.\\n-New schedule will be distributed by Friday.\\n-Next weeks meeting is cancelled.  No meeting until 3/23.
END:VJOURNAL
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec4/5)

(ipt:parse/print-test
;; Corrected.  Original text in the standard is missing UID and DTSTAMP.
;; See https://www.rfc-editor.org/errata/eid4149
"BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//RDU Software//NONSGML HandCal//EN
BEGIN:VFREEBUSY
UID:19970901T115957Z-76A912@example.com
DTSTAMP:19970901T120000Z
ORGANIZER:mailto:jsmith@example.com
DTSTART:19980313T141711Z
DTEND:19980410T141711Z
FREEBUSY:19980314T233000Z/19980315T003000Z
FREEBUSY:19980316T153000Z/19980316T163000Z
FREEBUSY:19980318T030000Z/19980318T040000Z
URL:http://www.example.com/calendar/busytime/jsmith.ifb
END:VFREEBUSY
END:VCALENDAR
"
:parser icalendar-parse-calendar
:printer icalendar-print-calendar-node
:source rfc5545-sec4/6)


;; Tests from real world data:
(ert-deftest ipt:bad-organizer-params ()
  "Real example: bad ORGANIZER property with params introduced by colon"
  (let ((bad "ORGANIZER:CN=ORGANIZER:mailto:anonymized@domain.example\n")
        (ok  "ORGANIZER;CN=ORGANIZER:mailto:anonymized@domain.example\n"))
    (should-error (ical:parse-from-string 'ical:organizer bad))
    (should (ical:ast-node-p (ical:parse-from-string 'ical:organizer ok)))))

(ert-deftest ipt:bad-attendee ()
  "Real example: bad ATTENDEE property missing mailto: prefix"
  (let ((bad "ATTENDEE;ROLE=REQ-PARTICIPANT;CN=TRAVELLER:anonymized@domain.example\n")
        (ok  "ATTENDEE;ROLE=REQ-PARTICIPANT;CN=TRAVELLER:mailto:anonymized@domain.example\n"))
    (should-error (ical:parse-from-string 'ical:attendee bad))
    (should (ical:ast-node-p (ical:parse-from-string 'ical:attendee ok)))))

(ert-deftest ipt:bad-attach ()
  "Real example: bad ATTACH property containing broken URI"
  (let ((bad "ATTACH;VALUE=URI:Glass\n")
        (ok  "ATTACH;VALUE=URI:https://example.com\n"))
    (should-error (ical:parse-from-string 'ical:attach bad))
    (should (ical:ast-node-p (ical:parse-from-string 'ical:attach ok)))))

(ert-deftest ipt:bad-cnparam ()
  "Real example: bad unquoted CN parameter containing a comma"
  (let ((bad "ORGANIZER;CN=Hartlauer Geschäft Wien, Taborstr. 18:mailto:anonymized@domain.example\n")
        (ok  "ORGANIZER;CN=\"Hartlauer Geschäft Wien, Taborstr. 18\":mailto:anonymized@domain.example\n"))
    ;; strict parser should reject bad but accept ok:
    (let ((ical:parse-strictly t))
      (should (ical:ast-node-p (ical:parse-from-string 'ical:organizer ok)))
      (should-error (ical:parse-from-string 'ical:organizer bad)))
    ;; relaxed parser should accept bad:
    (let ((ical:parse-strictly nil))
      (should (ical:ast-node-p (ical:parse-from-string 'ical:organizer bad))))))

(ert-deftest ipt:fix-bad-description ()
  "Real example: bad DESCRIPTION property containing blank lines,
fixed by `icalendar-fix-blank-lines'."
  (let ((bad "BEGIN:VCALENDAR
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VEVENT
UID:45dd7698-5c53-47e3-9280-19c5dff62571
PRIORITY:1
DTSTART:20210721T175200
DTEND:20210721T192400
LOCATION:Verona Porta Nuova
DESCRIPTION:Verona Porta Nuova-Firenze S. M. Novella;Train: Frecciarossa 8527, departing from Verona Porta Nuova Hours: 17:52; arriving at Firenze S. M. Novella Hours: 19:24 Coach 8, Position 7A; pnr code CLS345


SUMMARY:Trip Verona Porta Nuova-Firenze S. M. Novella, Train Frecciarossa 8527, Coach 8, Position 7A, PNR CLS345,
ORGANIZER;CN=ORGANIZER:mailto:anonymized@domain.example
ATTENDEE;ROLE=REQ-PARTICIPANT;CN=BUYER:mailto:anonymized@domain.example
ATTENDEE;ROLE=REQ-PARTICIPANT;CN=TRAVELLER:mailto:anonymized@domain.example
END:VEVENT
END:VCALENDAR
"))
    ;; The default parser should produce an error on the blank lines in
    ;; DESCRIPTION:
    (let ((ical:pre-parsing-hook nil))
      (with-temp-buffer
        (ical:init-error-buffer)
        (insert bad)
        (goto-char (point-min))
        (ical:parse)
        ;; Parsing should produce error at the bad description property:
        (should (ical:errors-p))))
    ;; cleaning up the blank lines before parsing should correct this:
    (let ((ical:pre-parsing-hook '(ical:fix-blank-lines)))
      (with-temp-buffer
        (ical:init-error-buffer)
        (insert bad)
        (goto-char (point-min))
        (let ((vcal (ical:parse)))
          (should (not (ical:errors-p)))
          (ical:with-component vcal
              ((ical:vevent vevent))
            (ical:with-component vevent
                ((ical:description :value description))
              (let* ((expected "CLS345")
                     (end (length description))
                     (start (- end (length expected))))
              (should (equal expected
                             (substring description start end)))))))))))

(ert-deftest ipt:bad-hyphenated-dates ()
  "Real example: bad date values containing hyphens, fixed by
`icalendar-fix-hyphenated-dates'."
  (let ((bad "BEGIN:VCALENDAR
X-LOTUS-CHARSET:UTF-8
VERSION:2.0
PRODID:http://www.bahn.de
METHOD:PUBLISH
BEGIN:VTIMEZONE
TZID:Europe/Berlin
X-LIC-LOCATION:Europe/Berlin
BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
TZNAME:CEST
DTSTART:19700329T020000
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=-1SU;BYMONTH=3
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
TZNAME:CET
DTSTART:19701025T030000
RRULE:FREQ=YEARLY;INTERVAL=1;BYDAY=-1SU;BYMONTH=10
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:bahn2023-08-29141400
CLASS:PUBLIC
SUMMARY:Frankfurt(Main)Hbf -> Hamburg Hbf
DTSTART;TZID=Europe/Berlin:2023-08-29T141400
DTEND;TZID=Europe/Berlin:2023-08-29T183600
DTSTAMP:2023-07-30T194700Z
END:VEVENT
END:VCALENDAR
"))
    ;; default parser should skip the invalid DTSTART, DTEND, and DTSTAMP values:
    (let ((ical:pre-parsing-hook nil))
      (with-temp-buffer
        (ical:init-error-buffer)
        (insert bad)
        (goto-char (point-min))
        (let ((vcal (ical:parse)))
          ;; Parsing should produce errors as the bad properties are
          ;; skipped:
          (should (ical:errors-p))
          ;; The resulting calendar is invalid because the VEVENT
          ;; contains no DTSTAMP:
          (should-error (ical:ast-node-valid-p vcal t)))))
    ;; cleaning up the hyphenated dates before parsing should correct
    ;; these problems:
    (let ((ical:pre-parsing-hook '(ical:fix-hyphenated-dates)))
      (with-temp-buffer
        (ical:init-error-buffer)
        (insert bad)
        (goto-char (point-min))
        (let ((vcal (ical:parse))
              (expected-dtstamp
               (ical:make-date-time :year 2023 :month 7 :day 30
                                    :hour 19 :minute 47 :second 0
                                    :zone 0)))
          (should (not (ical:errors-p)))
          (should (ical:ast-node-valid-p vcal t))
          (ical:with-component vcal
              ((ical:vevent vevent))
            (ical:with-component vevent
                ((ical:dtstamp :value dtstamp))
              (should (equal dtstamp expected-dtstamp)))))))))

(ert-deftest ipt:bad-user-addresses ()
  "Real example: bad calendar user addresses missing \"mailto:\", fixed by
`icalendar-fix-missing-mailtos'."
  (let ((bad "BEGIN:VCALENDAR
VERSION:2.0
PRODID:missing
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VEVENT
UID:45dd7698-5c53-47e3-9280-19c5dff62571
PRIORITY:1
DTSTART:20210721T175200
DTEND:20210721T192400
LOCATION:Verona Porta Nuova
SUMMARY:Trip Verona Porta Nuova-Firenze S. M. Novella
ORGANIZER;SENT-BY=\"other@domain.example\":anonymized@domain.example
ATTENDEE;ROLE=REQ-PARTICIPANT;CN=TRAVELER:traveler@domain.example
END:VEVENT
END:VCALENDAR
"))
    (let ((ical:pre-parsing-hook nil))
      (with-temp-buffer
        (ical:init-error-buffer)
        (insert bad)
        (goto-char (point-min))
        (ical:parse)
        ;; Parsing should produce errors as the bad properties are
        ;; skipped:
        (should (ical:errors-p))))
    ;; cleaning up the addresses before parsing should correct
    ;; these problems:
    (let ((ical:pre-parsing-hook '(ical:fix-missing-mailtos)))
      (with-temp-buffer
        (ical:init-error-buffer)
        (insert bad)
        (goto-char (point-min))
        (let ((vcal (ical:parse))
              (expected-attendee "mailto:traveler@domain.example")
              (expected-organizer "mailto:anonymized@domain.example")
              (expected-sender "mailto:other@domain.example"))
          (should (not (ical:errors-p)))
          (ical:with-component vcal
              ((ical:vevent vevent))
            (ical:with-component vevent
                ((ical:attendee :value attendee)
                 (ical:organizer :value organizer))
              (should (equal attendee expected-attendee))
              (should (equal organizer expected-organizer))
              (ical:with-property organizer
                  ((ical:sentbyparam :value sent-by))
                  (should (equal sent-by expected-sender))))))))))




;; Local Variables:
;; read-symbol-shorthands: (("ipt:" . "icalendar-parser-test-") ("ical:" . "icalendar-"))
;; End:
;;; icalendar-parser-tests.el ends here
