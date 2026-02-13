;;; diary-icalendar-tests.el --- Tests for diary-icalendar -*- lexical-binding: t; -*-
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
(require 'diary-icalendar)
(require 'icalendar-parser)
(require 'icalendar-utils)
(require 'icalendar)
(require 'ert)
(require 'ert-x)
(require 'seq)


;; Tests for diary import functions
(defconst icalendar-resources-directory
  (expand-file-name "test/lisp/calendar/icalendar-resources"
                    source-directory))

(defconst diary-icalendar-resources-directory
  (expand-file-name "test/lisp/calendar/diary-icalendar-resources"
                    source-directory))

(defun dit:icalendar-resource-file (filename)
  ;; Return a filename from the ./icalendar-resources directory:
  (file-name-concat icalendar-resources-directory filename))

(defun dit:resource-file (filename)
  ;; Return a filename from the ./diary-icalendar-resources directory:
  (file-name-concat diary-icalendar-resources-directory filename))

(defun dit:file-contents (filename)
  "Return literal contents of FILENAME."
  (with-temp-buffer
    (let ((coding-system-for-read 'raw-text)
          (inhibit-eol-conversion t))
      (insert-file-contents-literally filename)
      (buffer-string))))

(defmacro dit:with-tz (tz &rest body)
  "Evaluate BODY with time zone TZ in effect."
  `(let ((old-tz (getenv "TZ")))
     (unwind-protect
         (progn
           (setenv "TZ" ,tz)
           ,@body)
       (setenv "TZ" old-tz))))

(defun dit:import-file (ics-file)
  "Test diary import of ICS-FILE.

ICS-FILE names a .ics file in icalendar-resources directory.  The
calendar in ICS-FILE is parsed and imported in ISO, European, and
American date styles.  The output of each import is compared against the
contents of any diary files with the same base name as ICS-FILE and
extensions \".diary-all\", \".diary-american\", \".diary-european\", or
\".diary-iso\"."
  (let* ((basename (file-name-base ics-file))
         (ics-file (dit:icalendar-resource-file ics-file))
         (import-buffer (icalendar-unfolded-buffer-from-file ics-file))
         (all-file (dit:resource-file (concat basename ".diary-all")))
         (iso-file (dit:resource-file (concat basename ".diary-iso")))
         (european-file (dit:resource-file (concat basename ".diary-european")))
         (american-file (dit:resource-file (concat basename ".diary-american"))))
    (with-current-buffer import-buffer
      (when (file-exists-p all-file)
        (calendar-set-date-style 'american) ; because it's the default
        (dit:-do-test-import all-file))
      (when (file-exists-p iso-file)
        (calendar-set-date-style 'iso)
        (dit:-do-test-import iso-file))
      (when (file-exists-p european-file)
        (calendar-set-date-style 'european)
        (dit:-do-test-import european-file))
      (when (file-exists-p american-file)
        (calendar-set-date-style 'american)
        (dit:-do-test-import american-file))
      (set-buffer-modified-p nil)) ; so we can kill it without being asked
    (kill-buffer import-buffer)))

(defun dit:-do-test-import (diary-filename)
  "Import iCalendar in current buffer and compare the result with DIARY-FILENAME."
  (ert-with-temp-file temp-file
    :suffix "icalendar-test-diary"
    (dit:with-tz "Europe/Vienna"
      ;; There's no way to make the test data independent of the system
      ;; time zone unless diary gains time zone awareness/syntax, so we have
      ;; to choose some time zone or other to standardize on for the import
      ;; tests.  "Europe/Vienna" is an arbitrary choice; it's simply the one
      ;; I originally generated the test data files in.
      ;; N.B.  "Europe/Vienna" = "CET-1CEST,M3.5.0/02:00,M10.5.0/03:00"
      (di:import-buffer temp-file t t))
    (save-excursion
      (find-file temp-file)
      (let ((result (buffer-substring-no-properties (point-min) (point-max)))
            (expected (dit:file-contents diary-filename)))
        ;; Trim the result so that whitespace produced by the importer
        ;; need not be committed in the test data files:
        (should (equal (string-trim result)
                       (string-trim expected)))
        ;; This is useful for debugging differences when tests are failing:
        ;; (unless (equal (string-trim result)
        ;;                (string-trim expected))
        ;;   (let ((result-buf (current-buffer))
        ;;         (diary-buf (find-file diary-filename)))
        ;;     (ediff-buffers result-buf ; actual output
        ;;                    diary-buf)
        ;;     (switch-to-buffer-other-frame "*Ediff Control Panel*")
        ;;     (error "Unexpected result; see ediff")))
          ))
    (kill-buffer (find-buffer-visiting temp-file))))

(ert-deftest dit:import-non-recurring ()
  "Import tests for standard, non-recurring events."
  (dit:import-file "import-non-recurring-1.ics")
  (dit:import-file "import-non-recurring-all-day.ics")
  (dit:import-file "import-non-recurring-long-summary.ics")
  (dit:import-file "import-non-recurring-block.ics")
  (dit:import-file "import-non-recurring-folded-summary.ics")
  (dit:import-file "import-non-recurring-another-example.ics"))

(ert-deftest dit:import-w/legacy-vars ()
  "Import tests using legacy import variables"
  (let ((icalendar-import-format "%s%c%d%l%o%t%u%U")
        (icalendar-import-format-summary "%s")
        (icalendar-import-format-class "\n CLASS=%s")
        (icalendar-import-format-description "\n DESCRIPTION=%s")
        (icalendar-import-format-location "\n LOCATION=%s")
        (icalendar-import-format-organizer "\n ORGANIZER=%s")
        (icalendar-import-format-status "\n STATUS=%s")
        (icalendar-import-format-url "\n URL=%s")
        (icalendar-import-format-uid "\n UID=%s"))
    (dit:import-file "import-legacy-vars.ics")))

(defun dit:legacy-import-function (vevent)
  "Example function value for `icalendar-import-format'"
  (let ((props (nth 2 (car vevent))))
    (mapconcat
     (lambda (prop)
       (format " %s: %s\n"
               (symbol-name (nth 0 prop))
               (nth 2 prop)))
     props)))

(ert-deftest dit:import-w/legacy-function ()
  "Import tests using legacy import variables"
  (let ((icalendar-import-format 'dit:legacy-import-function))
    (dit:import-file "import-legacy-function.ics")))

(ert-deftest dit:import-w/time-format ()
  "Import tests for customized `diary-icalendar-time-format'"
  (let ((diary-icalendar-time-format "%l.%Mh"))
    (dit:import-file "import-time-format-12hr-blank.ics")))

(ert-deftest dit:import-rrule ()
  "Import tests for recurring events."
  (dit:import-file "import-rrule-daily.ics")
  (dit:import-file "import-rrule-daily-two-day.ics")
  (dit:import-file "import-rrule-daily-with-exceptions.ics")
  (dit:import-file "import-rrule-weekly.ics")
  (dit:import-file "import-rrule-monthly-no-end.ics")
  (dit:import-file "import-rrule-monthly-with-end.ics")
  (dit:import-file "import-rrule-anniversary.ics")
  (dit:import-file "import-rrule-yearly.ics")
  (dit:import-file "import-rrule-count-bi-weekly.ics")
  (dit:import-file "import-rrule-count-daily-short.ics")
  (dit:import-file "import-rrule-count-daily-long.ics")
  (dit:import-file "import-rrule-count-monthly.ics")
  (dit:import-file "import-rrule-count-every-second-month.ics")
  (dit:import-file "import-rrule-count-yearly.ics")
  (dit:import-file "import-rrule-count-every-second-year.ics"))

(ert-deftest dit:import-duration ()
  (dit:import-file "import-duration.ics")
  ;; duration-2: this is actually an rrule test
  (dit:import-file "import-duration-2.ics"))

(ert-deftest dit:import-multiple-vcalendars ()
  (dit:import-file "import-multiple-vcalendars.ics"))

(ert-deftest dit:import-with-uid ()
  "Perform import test with uid."
  (dit:import-file "import-with-uid.ics"))

(ert-deftest dit:import-with-attachment ()
  "Test importing an attached file to `icalendar-attachment-directory'"
  (ert-with-temp-directory temp-dir
    (let ((di:attachment-directory temp-dir)
          (uid-dir (file-name-concat temp-dir
                                     ;; Event's UID:
                                     "f9fee9a0-1231-4984-9078-f1357db352db")))
      (dit:import-file "import-with-attachment.ics")
      (should (file-directory-p uid-dir))
      (let ((files (directory-files uid-dir t
                                    ;; First 4 chars of base64-string:
                                    "R3Jl")))
        (should (length= files 1))
        (with-temp-buffer
          (insert-file-contents (car files))
          (should (equal "Greetings! I am a base64-encoded file"
                         (buffer-string))))))))

(ert-deftest dit:import-with-timezone ()
  (dit:import-file "import-with-timezone.ics"))

(ert-deftest dit:import-real-world ()
  "Import tests of other real world data"
  ;; N.B.  Not all data from these files is expected to be imported
  ;; without any pre-parsing cleanup, since they are in some cases
  ;; malformed.  The test data matches what the importer should produce
  ;; in its default configuration.
  (dit:with-tz "Asia/Kolkata"
     ;; Indian Standard Time, used in this file, does not adjust for
     ;; daylight savings; so we use that time zone to keep this test
     ;; from failing on systems in a time zone that does:
     (dit:import-file "import-real-world-2003-05-29.ics"))
  (dit:with-tz "Asia/Tehran"
     ;; For the same reason, we use "Asia/Tehran" here:
     (dit:import-file "import-real-world-no-dst.ics"))
  (dit:import-file "import-real-world-2003-06-18a.ics")
  ;; FIXME: this test seems to be failing due to an invisible unicode
  ;; error of some sort.  The import result and the expected output are
  ;; visually identical and ediff shows no differences in the buffers,
  ;; but the strings are apparently not `equal', and comparing them
  ;; character-by-character shows that they somehow differ at the "ü" in
  ;; "früher".  But `describe-char' there shows no differences so far as
  ;; I can see.
  ;(dit:import-file "import-real-world-2003-06-18b.ics")
  (dit:import-file "import-real-world-2004-11-19.ics")
  (dit:import-file "import-real-world-2005-02-07.ics")
  (dit:import-file "import-real-world-2005-03-01.ics"))

(ert-deftest dit:import-bug-6766 ()
  ;;bug#6766 -- multiple byday values in a weekly rrule
  (dit:import-file "import-bug-6766.ics"))

(ert-deftest dit:import-bug-11473 ()
  ;; bug#11473 -- illegal tzid
  (dit:import-file "import-bug-11473.ics"))

(ert-deftest dit:import-bug-22092 ()
  ;; bug#22092 -- mixed line endings
  (let ((ical:pre-unfolding-hook '(ical:fix-line-endings)))
    (dit:import-file "import-bug-22092.ics")))

(ert-deftest dit:import-bug-24199 ()
  ;;bug#24199 -- monthly rule with byday-clause
  (dit:import-file "import-bug-24199.ics"))

(ert-deftest dit:import-bug-33277 ()
  ;;bug#33277 -- start time equals end time
  (dit:import-file "import-bug-33277.ics"))




;; Tests for diary export functions
(cl-defmacro dit:parse-test (entry &key parser type number
                                   bindings tests
                                   source)
  "Create a test which parses data from ENTRY.

PARSER should be a zero-argument function which parses data of TYPE in a
buffer containing ENTRY.  The defined test passes if PARSER returns a
list of NUMBER objects which satisfy TYPE.  If NUMBER is nil, the return
value of parser must be a single value satisfying TYPE.

BINDINGS, if given, will be evaluated and made available in the lexical
environment where PARSER is called; this can be used to temporarily set
variables that affect parsing.

TESTS, if given, is an additional test form that will be evaluated after
the main tests.  The variable `parsed' will be bound to the return value
of PARSER when TESTS are evaluated.

SOURCE, if given, should be a symbol; it is used to name the test."
  (let ((parser-form `(funcall (function ,parser))))
    `(ert-deftest
         ,(intern (concat "diary-icalendar-test-"
                          (string-replace "diary-icalendar-" ""
                                          (symbol-name parser))
                          (if source (concat "/" (symbol-name source)) "")))
         ()
       ,(format "Does `%s' correctly parse `%s' in diary entries?" parser type)
       (let* ((parse-buf (get-buffer-create "*iCalendar Parse Test*"))
              (unparsed ,entry))
         (set-buffer parse-buf)
         (erase-buffer)
         (insert unparsed)
         (goto-char (point-min))
         (let* (,@bindings
               (parsed ,parser-form))
           (when ,number
               (should (length= parsed ,number))
               (should (seq-every-p (lambda (val) (cl-typep val ,type))
                                    parsed)))
           (unless ,number
             (should (cl-typep parsed ,type)))
           ,tests)))))

(dit:parse-test
 "2025-04-01 A basic entry
    Other data"
:parser di:parse-entry-type
:type 'symbol
:source vevent
:tests (should (eq parsed 'ical:vevent)))

(dit:parse-test
 "&2025-04-01 A nonmarking journal entry
     Other data"
:parser di:parse-entry-type
:bindings ((di:export-nonmarking-as-vjournal t))
:type 'symbol
:source vjournal
:tests (should (eq parsed 'ical:vjournal)))

(dit:parse-test
 "2025-04-01 Due: some task
     Other data"
:parser di:parse-entry-type
:bindings ((di:todo-regexp "Due: "))
:type 'symbol
:source vtodo
:tests (should (eq parsed 'ical:vtodo)))

(defun dit:parse-vevent-transparency ()
  "Call `di:parse-transparency' with \\='icalendar-vevent"
  (di:parse-transparency 'ical:vevent))

(dit:parse-test
 "&%%(diary-anniversary 7 28 1985) A transparent anniversary"
 :parser dit:parse-vevent-transparency
 :type 'ical:transp
 :number 1
 :source nonmarking
 :tests
 (ical:with-property (car parsed) nil
   (should (equal value "TRANSPARENT"))))

(dit:parse-test
 "2025-04-01 Team Meeting
   Some data
   Organizer: Mr. Foo <foo@example.com>
   Attendees: Baz Bar <baz@example.com>
              Alice Unternehmer <alice@example.com> (some other data)
   Other data"
:parser di:parse-attendees-and-organizer
:number 3
:type '(or ical:attendee ical:organizer)
:tests
(dolist (p parsed)
  (ical:with-property p
    ((ical:cnparam :value name))
    (cond ((equal value "mailto:foo@example.com")
           (should (equal name "Mr. Foo"))
           (should (ical:organizer-property-p p)))
          ((equal value "mailto:baz@example.com")
           (should (equal name "Baz Bar"))
           (should (ical:attendee-property-p p)))
          ((equal value "mailto:alice@example.com")
           (should (equal name "Alice Unternehmer"))
           (should (ical:attendee-property-p p)))
          (t (error "Incorrectly parsed attendee address: %s" value))))))

(dit:parse-test
 "2025-04-01 An event with a UID
    Some data
    UID: emacs174560213714413195191
    Other data"
:parser di:parse-uid
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:uid
:tests
(ical:with-property (car parsed) nil
  (should (equal "emacs174560213714413195191" value))))

(dit:parse-test
 "2025-04-01 An event with a different style of UID
    Some data
    UID: 197846d7-51be-4d8e-837f-7e132286e7cf
    Other data"
:parser di:parse-uid
:source with-org-id-uuid
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:uid
:tests
(ical:with-property (car parsed) nil
  (should (equal "197846d7-51be-4d8e-837f-7e132286e7cf" value))))

(dit:parse-test
 "2025-04-01 An event with a status
    Some data
    Status: confirmed
    Other data"
:parser di:parse-status
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:status
:tests
(ical:with-property (car parsed) nil
  (should (equal "CONFIRMED" value))))

(dit:parse-test
 "2025-04-01 An event with an access classification
    Some data
    Class: private
    Other data"
:parser di:parse-class
:source private
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:class
:tests
(ical:with-property (car parsed) nil
  (should (equal "PRIVATE" value))))

(dit:parse-test
 "2025-04-01 An event with an access classification
    Some data
    Access: public
    Other data"
:parser di:parse-class
:source public
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:class
:tests
(ical:with-property (car parsed) nil
  (should (equal "PUBLIC" value))))

(dit:parse-test
 "2025-04-01 An event with a location
    Some data
    Location: Sesamstraße 13
    Other data"
:parser di:parse-location
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:location
:tests
(ical:with-property (car parsed) nil
  (should (equal "Sesamstraße 13" value))))

(dit:parse-test
 "2025-04-01 An event with an URL
    Some data
    URL: http://example.com/foo/bar?q=baz
    Other data"
:parser di:parse-url
:bindings ((diary-date-forms diary-iso-date-forms))
:type 'ical:url
:tests
(ical:with-property (car parsed) nil
  (should (equal "http://example.com/foo/bar?q=baz" value))))


;; N.B.  There is no date at the start of the entry in the following two
;; tests because di:parse-summary-and-description assumes that the date
;; parsing functions have already moved the start of the restriction
;; beyond it.
(dit:parse-test
 "Event summary
    Some data
    Other data"
:parser di:parse-summary-and-description
:number 2
:type '(or ical:summary ical:description)
:bindings ((diary-date-forms diary-iso-date-forms))
:tests
(ical:with-property (car parsed) nil (should (equal "Event summary" value))))

(dit:parse-test
 "Some data
    Summary: Event summary
    Other data"
:parser di:parse-summary-and-description
:number 2
:bindings ((di:summary-regexp "^[[:space:]]+Summary: \\(.*\\)$"))
:type '(or ical:summary ical:description)
:bindings ((diary-date-forms diary-iso-date-forms))
:source with-summary-regexp
:tests
(ical:with-property (car parsed) nil (should (equal "Event summary" value))))

(dit:parse-test
 "2025/04/01 Some entry"
 :parser di:parse-date-form
 :type 'ical:date
 :bindings ((diary-date-forms diary-iso-date-forms))
 :source iso-date
 :tests
 (progn
   (should (= 2025 (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "2025-04-01 Some entry"
 :parser di:parse-date-form
 :type 'ical:date
 :bindings ((diary-date-forms diary-iso-date-forms))
 :source iso-date-dashes
 :tests
 (progn
   (should (= 2025 (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "1/4/2025 Some entry"
 :parser di:parse-date-form
 :type 'ical:date
 :bindings ((diary-date-forms diary-european-date-forms))
 :source european-date
 :tests
 (progn
   (should (= 2025 (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "4/1/2025 Some entry"
 :parser di:parse-date-form
 :type 'ical:date
 :bindings ((diary-date-forms diary-american-date-forms))
 :source american-date
 :tests
 (progn
   (should (= 2025 (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "4/1 April Fool's"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source generic-year-american
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "1/5 Tag der Arbeit"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-european-date-forms))
 :source generic-year-european
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (= 5 (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "1/*/2025 Rent due"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-european-date-forms))
 :source generic-month
 :tests
 (progn
   (should (= 2025 (calendar-extract-year parsed)))
   (should (eq t (calendar-extract-month parsed)))
   (should (= 1 (calendar-extract-day parsed)))))

(dit:parse-test
 "*/2/2025 Every day in February: go running"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-european-date-forms))
 :source generic-day
 :tests
 (progn
   (should (= 2025 (calendar-extract-year parsed)))
   (should (= 2 (calendar-extract-month parsed)))
   (should (eq t (calendar-extract-day parsed)))))

(dit:parse-test
 "Friday
    Lab meeting
    Backup data"
 :parser di:parse-weekday-name
 :type 'integer
 :tests
 (should (= 5 parsed)))

;;; Examples from the Emacs manual:
(dit:parse-test
 "12/22/2015  Twentieth wedding anniversary!"
 :parser di:parse-date-form
 :type 'ical:date
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/1
 :tests
 (progn
   (should (= 2015 (calendar-extract-year parsed)))
   (should (= 12 (calendar-extract-month parsed)))
   (should (= 22 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Generic date via unspecified year:
 "10/22       Ruth's birthday."
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/2
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (= 10 (calendar-extract-month parsed)))
   (should (= 22 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Generic date via unspecified year:
 "4/30  Results for April are due"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.3/3
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 30 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Generic date with asterisks:
 "* 21, *:    Payday"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/3
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (eq t (calendar-extract-month parsed)))
   (should (= 21 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Generic date with asterisks:
 "*/25  Monthly cycle finishes"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.3/4
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (eq t (calendar-extract-month parsed)))
   (should (= 25 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Weekday name:
 "Tuesday--weekly meeting with grad students at 10am
           Supowit, Shen, Bitner, and Kapoor to attend."
 :parser di:parse-weekday-name
 :type 'integer
 :source emacs-manual-sec33.10.1/4
 :tests
 (should (= 2 parsed)))

(dit:parse-test
 ;; Weekday name:
 "Friday  Don't leave without backing up files"
 :parser di:parse-weekday-name
 :type 'integer
 :source emacs-manual-sec33.10.3/5
 :tests
 (should (= 5 parsed)))

(dit:parse-test
 ;; Date with two-digit year:
 "1/13/89     Friday the thirteenth!!"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/5
 :tests
 (progn
   (should (= 1989 (calendar-extract-year parsed)))
   (should (= 1 (calendar-extract-month parsed)))
   (should (= 13 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Date with two-digit year:
 "4/20/12  Switch-over to new tabulation system"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.3/1
 :tests
 (progn
   (should (= 2012 (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 20 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Abbreviated weekday name:
 "thu 4pm     squash game with Lloyd."
 :parser di:parse-weekday-name
 :type 'integer
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/6
 :tests
 (should (= 4 parsed)))

(dit:parse-test
 ;; Abbreviated month name:
 "mar 16      Dad's birthday"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/7
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (= 3 (calendar-extract-month parsed)))
   (should (= 16 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Abbreviated month name with following period:
 "apr. 25  Start tabulating annual results"
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.3/2
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 25 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Long form date:
 "April 15, 2016 Income tax due."
 :parser di:parse-date-form
 :type 'ical:date
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/8
 :tests
 (progn
   (should (= 2016 (calendar-extract-year parsed)))
   (should (= 4 (calendar-extract-month parsed)))
   (should (= 15 (calendar-extract-day parsed)))))

(dit:parse-test
 ;; Generic monthly date:
 "* 15        time cards due."
 :parser di:parse-date-form
 :type 'list
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/9
 :tests
 (progn
   (should (eq t (calendar-extract-year parsed)))
   (should (eq t (calendar-extract-month parsed)))
   (should (= 15 (calendar-extract-day parsed)))))

(dit:parse-test
 "%%(diary-anniversary 5 28 1995) A birthday"
 :parser di:parse-sexp
 :type 'list
 :tests (should (eq 'diary-anniversary (car parsed))))

(dit:parse-test
 "%%(diary-time-block :start (0 0 13 2 4 2025 6 t 7200)
                      :end (0 0 11 4 4 2025 6 t 7200))
    A multiday event with different start and end times"
 :parser di:parse-sexp
 :type 'list
 :source multiline-sexp
 :tests (should (eq 'diary-time-block (car parsed))))

(defun dit:entry-parser ()
  "Call `di:parse-entry' on the full test buffer"
  (let ((tz
         (cond
          ((eq 'local di:time-zone-export-strategy)
           (di:current-tz-to-vtimezone))
          ((listp di:time-zone-export-strategy)
           (di:current-tz-to-vtimezone di:time-zone-export-strategy)))))

    (di:parse-entry (point-min) (point-max) tz)))

(dit:parse-test
 ;; Weekly event, abbreviated weekday name:
 "thu 4pm     squash game with Lloyd."
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source emacs-manual-sec33.10.1/6
 :tests
 (ical:with-component (car parsed)
   ((ical:dtstart :value dtstart)
    (ical:rrule :value rrule)
    (ical:summary :value summary))
   (should (equal summary "squash game with Lloyd."))
   (should (equal (ical:date-time-to-date dtstart)
                  (calendar-nth-named-day 1 4 1 di:recurring-start-year)))
   (should (= 16 (decoded-time-hour dtstart)))
   (should (eq (ical:recur-freq rrule) 'WEEKLY))
   (should (equal (ical:recur-by* 'BYDAY rrule) (list 4)))))

(dit:parse-test
 ;; Multiline entry, parsed as one event:
 "2025-05-03
    9AM Lab meeting
      Gunther to present on new assay
    12:30-1:30PM Lunch with Phil
    16:00 Experiment A finishes; move to freezer"
 :parser dit:entry-parser
 :source multiline-single
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-iso-date-forms)))

(dit:parse-test
 ;; Multiline entry, parsed linewise as three events:
 "2025-05-03
    9AM Lab meeting
      Gunther to present on new assay
    12:30-1:30PM Lunch with Phil
    16:00 Experiment A finishes; move to freezer"
 :parser dit:entry-parser
 :source multiline-linewise
 :type 'ical:vevent
 :number 3
 :bindings ((diary-date-forms diary-iso-date-forms)
            (diary-icalendar-export-linewise t))
 :tests
 (progn
   (dolist (event parsed)
     (ical:with-component event
       ((ical:dtstart :value-type start-type :value dtstart)
        (ical:dtend :value-type end-type :value dtend)
        (ical:summary :value summary))
       (should (eq start-type 'ical:date-time))
       (should (= 2025 (decoded-time-year dtstart)))
       (should (= 5 (decoded-time-month dtstart)))
       (should (= 3 (decoded-time-day dtstart)))
       (when dtend
         (should (eq end-type 'ical:date-time))
         (should (= 2025 (decoded-time-year dtend)))
         (should (= 5 (decoded-time-month dtend)))
         (should (= 3 (decoded-time-day dtend))))
       (cond ((equal summary "Lab meeting")
              (should (= 9 (decoded-time-hour dtstart))))
             ((equal summary "Lunch with Phil")
              (should (= 12 (decoded-time-hour dtstart)))
              (should (= 30 (decoded-time-minute dtstart)))
              (should (= 13 (decoded-time-hour dtend)))
              (should (= 30 (decoded-time-minute dtend))))
             ((equal summary "Experiment A finishes; move to freezer")
              (should (= 16 (decoded-time-hour dtstart))))
             (t (error "Unknown event: %s" summary)))))))

(dit:parse-test
 ;; Multiline entry from the manual, parsed linewise:
 ;; TODO: I've left the times verbatim in the example
 ;; and in the tests, even though "2:30", "5:30" and "8:00"
 ;; would most naturally be understood as PM times.
 ;; Should probably fix the manual, then revise here.
 "02/11/2012
           Bill B. visits Princeton today
           2pm Cognitive Studies Committee meeting
           2:30-5:30 Liz at Lawrenceville
           4:00pm Dentist appt
           7:30pm Dinner at George's
           8:00-10:00pm concert"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 6
 :bindings ((diary-date-forms diary-american-date-forms)
            (diary-icalendar-export-linewise t))
 :source emacs-manual-sec33.10.1/10
 :tests
 (progn
   (dolist (event parsed)
     (ical:with-component event
       ((ical:dtstart :value-type start-type :value dtstart)
        (ical:dtend :value-type end-type :value dtend)
        (ical:summary :value summary))
       (when (eq start-type 'ical:date)
         (should (= 2012 (calendar-extract-year dtstart)))
         (should (= 2 (calendar-extract-month dtstart)))
         (should (= 11 (calendar-extract-day dtstart))))
       (when (eq start-type 'ical:date-time)
         (should (= 2012 (decoded-time-year dtstart)))
         (should (= 2 (decoded-time-month dtstart)))
         (should (= 11 (decoded-time-day dtstart))))
       (when dtend
         (should (eq end-type 'ical:date-time))
         (should (= 2012 (decoded-time-year dtend)))
         (should (= 2 (decoded-time-month dtend)))
         (should (= 11 (decoded-time-day dtend))))
       (cond ((equal summary "Bill B. visits Princeton today")
              (should (eq start-type 'ical:date)))
             ((equal summary "Cognitive Studies Committee meeting")
              (should (= 14 (decoded-time-hour dtstart)))
              (should (= 0 (decoded-time-minute dtstart))))
             ((equal summary "Liz at Lawrenceville")
              (should (= 2 (decoded-time-hour dtstart)))
              (should (= 30 (decoded-time-minute dtstart)))
              (should (= 5 (decoded-time-hour dtend)))
              (should (= 30 (decoded-time-minute dtend))))
             ((equal summary "Dentist appt")
              (should (= 16 (decoded-time-hour dtstart)))
              (should (= 0 (decoded-time-minute dtstart))))
             ((equal summary "Dinner at George's")
              (should (= 19 (decoded-time-hour dtstart)))
              (should (= 30 (decoded-time-minute dtstart))))
             ((equal summary "concert")
              (should (= 8 (decoded-time-hour dtstart)))
              (should (= 0 (decoded-time-minute dtstart)))
              (should (= 22 (decoded-time-hour dtend)))
              (should (= 0 (decoded-time-minute dtend))))
             (t (error "Unknown event: %s" summary)))))))

(dit:parse-test
 ;; Same as the last, but with ignored data on the same line as the date
 "02/11/2012 Ignored
             2pm Cognitive Studies Committee meeting
             2:30-5:30 Liz at Lawrenceville
             4:00pm Dentist appt
             7:30pm Dinner at George's
             8:00-10:00pm concert"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 5
 :bindings ((diary-date-forms diary-american-date-forms)
            (diary-icalendar-export-linewise t))
 :source emacs-manual-sec33.10.1/10-first-line)

(dit:parse-test
 "%%(diary-anniversary 5 28 1995) H's birthday"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms)
            (calendar-date-style 'american))
 :source diary-anniversary-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:summary :value summary))
   (should (equal dtstart '(5 28 1995)))
   (should (eq (ical:recur-freq recur-value) 'YEARLY))
   (should (equal summary "H's birthday"))))

(dit:parse-test
 "%%(diary-block 6 24 2012 7 10 2012) Vacation"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source diary-block-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:summary :value summary))
   (should (equal dtstart '(6 24 2012)))
   (should (equal (ical:recur-freq recur-value) 'DAILY))
   (should (equal (ical:recur-until recur-value) '(7 10 2012)))
   (should (equal summary "Vacation"))))

(dit:parse-test
 "%%(diary-cyclic 50 3 1 2012) Renew medication"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source diary-cyclic-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:summary :value summary))
   (should (equal dtstart '(3 1 2012)))
   (should (eq (ical:recur-freq recur-value) 'DAILY))
   (should (eq (ical:recur-interval-size recur-value) 50))
   (should (equal summary "Renew medication"))))

(dit:parse-test
 "%%(diary-float 11 4 4) American Thanksgiving"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source diary-float-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:summary :value summary))
   (should (equal dtstart
                  (calendar-nth-named-day 4 4 11 di:recurring-start-year)))
   (should (eq (ical:recur-freq recur-value) 'MONTHLY))
   (should (equal (ical:recur-by* 'BYMONTH recur-value) (list 11)))
   (should (equal (ical:recur-by* 'BYDAY recur-value) (list '(4 . 4))))
   (should (equal summary "American Thanksgiving"))))

(dit:parse-test
 "%%(diary-offset '(diary-float t 3 4) 2) Monthly committee meeting"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source diary-offset-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:summary :value summary))
   (should (equal dtstart
                  (calendar-nth-named-day 4 5 1 di:recurring-start-year)))
   (should (eq (ical:recur-freq recur-value) 'MONTHLY))
   ;; day 3 is Wednesday, so offset of 2 means Friday (=5):
   (should (equal (ical:recur-by* 'BYDAY recur-value) (list '(5 . 4))))
   (should (equal summary "Monthly committee meeting"))))

(dit:parse-test
 "%%(diary-rrule :start '(11 11 2024)
                 :rule '((FREQ WEEKLY))
                 :exclude '((12 23 2024) (12 30 2024))
     ) Reading group"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source diary-rrule-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:exdate :values exdates)
      (ical:summary :value summary))
   (should (equal dtstart '(11 11 2024)))
   (should (eq (ical:recur-freq recur-value) 'WEEKLY))
   (should (equal exdates '((12 23 2024) (12 30 2024))))
   (should (equal summary "Reading group"))))

(dit:parse-test
 "%%(diary-date '(10 11 12) 22 t) Rake leaves"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms))
 :source diary-date-recurrence
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rrule :value recur-value)
      (ical:summary :value summary))
   (should (equal dtstart (list 10 22 di:recurring-start-year)))
   (should (eq (ical:recur-freq recur-value) 'YEARLY))
   (should (equal (ical:recur-by* 'BYMONTH recur-value) (list 10 11 12)))
   (should (equal (ical:recur-by* 'BYMONTHDAY recur-value) (list 22)))
   (should (equal summary "Rake leaves"))))

(dit:parse-test
 ;; From the manual: "Suppose you get paid on the 21st of the month if
 ;; it is a weekday, and on the Friday before if the 21st is on a
 ;; weekend..."
 "%%(let ((dayname (calendar-day-of-week date))
          (day (cadr date)))
          (or (and (= day 21) (memq dayname '(1 2 3 4 5)))
              (and (memq day '(19 20)) (= dayname 5)))
              ) Pay check deposited"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-date-forms diary-american-date-forms)
            (di:export-sexp-enumeration-days 366))
 :source emacs-manual-sec33.13.10.7
 :tests
 (ical:with-component (car parsed)
     ((ical:dtstart :value dtstart)
      (ical:rdate :values rdates)
      (ical:summary :value summary))
   (should (equal summary "Pay check deposited"))
   (mapc
    (lambda (date)
      (should (or (and (= 21 (calendar-extract-day date))
                       (memq (calendar-day-of-week date) (list 1 2 3 4 5)))
                  (and (memq (calendar-extract-day date) (list 19 20))
                       (= 5 (calendar-day-of-week date))))))
    (cons dtstart rdates))))

(dit:parse-test
 "02/11/2012 4:00pm Exported with 'local strategy"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((tz (getenv "TZ"))
            ;; Refresh output from `calendar-current-time-zone':
            (calendar-current-time-zone-cache nil)
	    ;; Assume Eastern European Time (UTC+2, UTC+3 daylight saving)
            (_ (setenv "TZ" "EET-2EEST,M3.5.0/3,M10.5.0/4"))
            ;; ...and use this TZ when exporting:
            (diary-icalendar-time-zone-export-strategy 'local)
            (diary-date-forms diary-european-date-forms))
 :source tz-strategy-local
 :tests
 (unwind-protect
     (let ((vtimezone (di:current-tz-to-vtimezone)))
       (ical:with-component vtimezone
         ((ical:standard :first std)
          (ical:daylight :first dst))
         (should (= (* 2 60 60) (ical:with-property-of std 'ical:tzoffsetto)))
         (should (= (* 3 60 60) (ical:with-property-of dst 'ical:tzoffsetto))))
       (ical:with-component (car parsed)
         ((ical:dtstart :first start-node :value start))
         (should (= (* 2 60 60) (decoded-time-zone start)))
         (should (= 16 (decoded-time-hour start)))
         (should (ical:with-param-of start-node 'ical:tzidparam))))
   ;; restore time zone
   (setenv "TZ" tz)))

(dit:parse-test
 "02/11/2012 4:00pm Exported with 'to-utc strategy"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((tz (getenv "TZ"))
	    ;; Assume Eastern European Time (UTC+2, UTC+3 daylight saving)
            (_ (setenv "TZ" "EET-2EEST,M3.5.0/3,M10.5.0/4"))
            ;; ...and convert times to UTC on export:
            (diary-icalendar-time-zone-export-strategy 'to-utc)
            (diary-date-forms diary-european-date-forms))
 :source tz-strategy-to-utc
 :tests
 (unwind-protect
     (ical:with-component (car parsed)
       ((ical:dtstart :first start-node :value start))
       (should (= 0 (decoded-time-zone start)))
       (should (= (- 16 2) (decoded-time-hour start)))
       (should-not (ical:with-param-of start-node 'ical:tzidparam)))
   ;; restore time zone
   (setenv "TZ" tz)))

(dit:parse-test
 "02/11/2012 4:00pm Exported with 'floating strategy"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((tz (getenv "TZ"))
	    ;; Assume Eastern European Time (UTC+2, UTC+3 daylight saving)
            (_ (setenv "TZ" "EET-2EEST,M3.5.0/3,M10.5.0/4"))
            ;; ...but use floating times:
            (diary-icalendar-time-zone-export-strategy 'floating)
            (diary-date-forms diary-european-date-forms))
 :source tz-strategy-floating
 :tests
 (unwind-protect
     (ical:with-component (car parsed)
         ((ical:dtstart :first start-node :value start))
       (should (null (decoded-time-zone start)))
       (should (= 16 (decoded-time-hour start)))
       (should-not (ical:with-param-of start-node 'ical:tzidparam)))

     ;; restore time zone
     (setenv "TZ" tz)))

(dit:parse-test
 "02/11/2012 4:00pm Exported with tz info list"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings (;; Encode Eastern European Time (UTC+2, UTC+3 daylight saving)
            ;; directly in the variable:
            (diary-icalendar-time-zone-export-strategy
             '(120 60 "EET" "EEST"
               (calendar-nth-named-day -1 0 3 year) ; last Sunday of March
               (calendar-nth-named-day -1 0 10 year) ; last Sunday of October
               240 180))
            (diary-date-forms diary-european-date-forms))
 :source tz-strategy-sexp
 :tests
 (let ((vtimezone (di:current-tz-to-vtimezone
                   diary-icalendar-time-zone-export-strategy
                   "EET")))
   (ical:with-component vtimezone
       ((ical:standard :first std)
        (ical:daylight :first dst))
     (should (= (* 2 60 60) (ical:with-property-of std 'ical:tzoffsetto)))
     (should (= (* 3 60 60) (ical:with-property-of dst 'ical:tzoffsetto))))
   (ical:with-component (car parsed)
       ((ical:dtstart :first start-node :value start))
     (should (= 7200 (decoded-time-zone start)))
     (should (= 16 (decoded-time-hour start)))
     (should (ical:with-param-of start-node 'ical:tzidparam)))))

(defun dit:parse-@-location (type properties)
  "Example user function for parsing additional properties.
Parses anything following \"@\" to end of line as the entry's LOCATION."
  (ignore type properties)
  (goto-char (point-min))
  (when (re-search-forward "@\\([^\n]+\\)" nil t)
    (list (ical:make-property ical:location
                              (string-trim (match-string 1))))))

(dit:parse-test
 "2025/08/02 BBQ @ John's"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-icalendar-other-properties-parser #'dit:parse-@-location)
            (diary-date-forms diary-iso-date-forms))
 :source other-properties-parser
 :tests
 (ical:with-component (car parsed)
   ((ical:location :value location))
   (should (equal location "John's"))))

(dit:parse-test
 "2025/05/15 11AM Department meeting
  Attendee: <mydept@example.com>"
 :parser dit:entry-parser
 :type 'ical:vevent
 :number 1
 :bindings ((diary-icalendar-export-alarms
             '((audio 10)
               (display 20 "In %t minutes: %s")
               (email 60 "In %t minutes: %s" ("myemail@example.com" from-entry))))
            (diary-date-forms diary-iso-date-forms))
 :source alarms-export
 :tests
 (ical:with-component (car parsed)
   ((ical:valarm :all valarms))
   (should (length= valarms 3))
   (dolist (valarm valarms)
     (ical:with-component valarm
       ((ical:action :value action)
        (ical:trigger :value trigger)
        (ical:summary :value summary)
        (ical:attendee :all attendee-nodes))
       (cond ((equal action "AUDIO")
              (should (eql -10 (decoded-time-minute trigger))))
             ((equal action "DISPLAY")
              (should (eql -20 (decoded-time-minute trigger)))
              (should (equal summary "In 20 minutes: Department meeting")))
             ((equal action "EMAIL")
              (should (eql -60 (decoded-time-minute trigger)))
              (should (equal summary "In 60 minutes: Department meeting"))
              (should (length= attendee-nodes 2))
              (let ((addrs (mapcar (lambda (n) (ical:with-node-value n))
                                   attendee-nodes)))
                (should (member "mailto:myemail@example.com" addrs))
                (should (member "mailto:mydept@example.com" addrs))))
             (t (error "Unknown alarm action %s" action)))))))



;; Local Variables:
;; read-symbol-shorthands: (("dit:" . "diary-icalendar-test-") ("di:" . "diary-icalendar-") ("ical:" . "icalendar-"))
;; byte-compile-warnings: (not obsolete)
;; End:
;;; diary-icalendar-tests.el ends here
