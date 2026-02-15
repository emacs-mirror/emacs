;;; diary-icalendar.el --- Display iCalendar data in diary  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Free Software Foundation, Inc.

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: January 2025
;; Keywords: calendar
;; Human-Keywords: diary, calendar, iCalendar

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

;; This file is a replacement for icalendar.el that uses a new parser
;; and offers more features.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'icalendar-macs))
(require 'icalendar)
(require 'icalendar-parser)
(require 'icalendar-utils)
(require 'icalendar-recur)
(require 'icalendar-ast)
(require 'calendar)
(require 'cal-dst)
(require 'diary-lib)
(require 'skeleton)
(require 'seq)
(require 'rx)
(require 'pp)

;; Customization
(defgroup diary-icalendar nil
  "iCalendar import, export, and display in diary."
  :version "31.1"
  :group 'diary
  :prefix 'diary-icalendar)

;;; Import customizations
(defgroup diary-icalendar-import nil
  "iCalendar import into diary."
  :version "31.1"
  :group 'diary-icalendar
  :prefix 'diary-icalendar)

(defcustom di:always-import-quietly nil
  "When non-nil, diary will never ask for confirmations when importing events.

`diary-icalendar-import-file' and `diary-icalendar-import-buffer' both
accept an optional argument, QUIETLY, which determines whether these
functions ask for confirmation when importing individual events and
saving the diary file.  If you set this variable to t, you will never be
asked to confirm."
  :version "31.1"
  :type '(choice (const :tag "Ask for confirmations" nil)
                 (const :tag "Never ask for confirmations" t)))

(defcustom di:after-mailcap-viewer-hook nil
  "Hook run after `diary-icalendar-mailcap-viewer'.

The functions in this hook will be run in a temporary buffer after
formatting the contents of iCalendar data as diary entries in that
buffer.  You can add functions to this hook if you want, for example, to
copy these entries somewhere else."
  :version "31.1"
  :type '(hook))

(defcustom di:attachment-directory nil
  "Directory in which to save iCalendar attachments when importing.

If the value is nil, binary attachments encoded in an ATTACH property
are never saved.  If it is the name of a directory, attachments will be
saved in per-component subdirectories of this directory, with each
subdirectory named by the component's UID value."
  :version "31.1"
  :type '(choice
          (const :tag "Do not save attachments" nil)
          directory))

(defcustom di:time-format "%H:%M"
  "Format string to use for event times.

The value must be a valid format string for `format-time-string'; see
its docstring for more information.  The value only needs to format clock
times, and should format them in a way that will be recognized by
`diary-time-regexp'.  (Date information is formatted separately at the
start of the imported entry.)  Examples:

  \"%H:%M\" - 24-hour, 0-padded: 09:00 or 21:00
  \"%k.%Mh\" - 24-hour, blank-padded: 9.00h or 21.00h
  \"%I:%M%p\" - 12-hour, 0-padded, with AM/PM: 09:00AM or 09:00PM
  \"%l.%M%p\" - 12-hour, blank-padded, with AM/PM: 9.00AM or 9.00PM"
  :version "31.1"
  :type '(string))

(defcustom di:attendee-format-function #'di:attendee-skeleton
  "Function to format ATTENDEE properties during diary import.

This should be a function which inserts information about an
`icalendar-attendee' into the current buffer.  It is convenient to
express such a function as a skeleton; see `define-skeleton' and
`skeleton-insert' for more information.

The function will be called with one argument, ATTENDEE, which will be
an `icalendar-attendee' syntax node.  It should insert information about
the attendee into the current buffer.  See `icalendar-with-property' for
a convenient way to bind the data in ATTENDEE.

For convenience when writing this function as a skeleton, the following
variables will also be (dynamically) bound when the function is called.
All values will be strings (unless another type is noted), or nil:

`attendee-address' - address, with \"mailto:\" removed
`attendee-cn' - common name (`icalendar-cnparam')
`attendee-cutype' - calendar user type (`icalendar-cutypeparam')
`attendee-role' - role in the event (`icalendar-roleparam')
`attendee-partstat' - participation status (`icalendar-partstatparam')
`attendee-rsvp' - whether an RSVP is requested (`icalendar-rsvpparam')"
  :version "31.1"
  :type '(radio (function-item di:attendee-skeleton)
                (function :tag "Other function")))

(defcustom di:skip-addresses-regexp
  (concat "\\<" (regexp-quote user-mail-address) "\\'")
  "Regular expression matching addresses to skip when importing.

This regular expression should match calendar addresses (which are
typically \"mailto:\" URIs) which should be skipped when importing
ATTENDEE, ORGANIZER, and other iCalendar properties that identify a
contact.

You can make this match your own email address(es) to prevent them from
being formatted by `diary-icalendar-attendee-format-function' and
listed in diary entries."
  :version "31.1"
  :type '(regexp))

(defcustom di:vevent-format-function #'di:vevent-skeleton
  "Function to format VEVENT components for the diary.

This function is called with one argument VEVENT, an `icalendar-vevent'.
It should insert formatted data from this event into the current buffer.
It is convenient to express such a function as a skeleton; see
`define-skeleton' and `skeleton-insert' for more information.  See
`icalendar-with-component' for a convenient way to bind the data in
VEVENT.

For convenience when writing this function as a skeleton, the following
variables will be (dynamically) bound when the function is called.  All
values will be strings unless another type is noted, or nil:

`ical-as-alarm' (symbol) - non-nil when the event should be formatted for an
  alarm notification in advance of the event.  The symbol indicates the
  type of alarm: `email' means to format the event as the body of an email.
  (Currently only used for EMAIL alarms; see `diary-icalendar-export-alarms'.)
`ical-attachments' (list of strings) - URLs or filenames of attachments
  in the event
`ical-attendees' (list of strings) - the participants of the event,
  formatted by `diary-icalendar-attendee-format-function'
`ical-categories' (list of strings) - categories specified in the event
`ical-access' - the event's access classification
`ical-comments' (list of strings) - comments specified in the event
`ical-description' - the event's description
`ical-start' - start date and time in a single string.  When importing,
  includes the date, otherwise just the (local) time.
`ical-end' - end date and time in a single string.  When importing,
  includes the date, otherwise just the (local) time.
`ical-start-to-end' - a single string containing both start and end date and
  (local) time.  If the event starts and ends on the same day, the date
  is not repeated.  When importing, dates are included, and the string
  may contain a diary s-exp; when displaying, the string contains only
  the times for the displayed date.  If there is no end date, same as
  `ical-start'.
`ical-importing' (a boolean) - t if the event should be formatted for import.
  When nil, the event should be formatted for display rather than import.
  When importing it is important to include all information from the event
  that you want to be saved in the diary; when displaying, information like
  the date (or date-related S-expressions) and UID can be left out.
`ical-location' - the event's location, or geographical coordinates
`ical-nonmarking' (a boolean) - if non-nil, the diary entry should be nonmarking
`ical-organizer' - the event's organizer, formatted by
  `diary-icalendar-attendee-format-function'
`ical-priority' (a number) - the event's priority (1 = highest priority,
  9 = lowest; 0 = undefined)
`ical-rrule-sexp' - a string containing a diary S-expression for a
  recurring event.  If this is non-nil, you should normally use it
  instead of the start-* and end-* variables to form the date of the
  entry.
`ical-status' - overall status specified by the organizer (e.g. \"confirmed\")
`ical-summary' - a summary of the event
`ical-transparency' - the event's time transparency status, either
  \"OPAQUE\" (busy) or \"TRANSPARENT\" (free); see `icalendar-transp'
`ical-uid' - the unique identifier of the event
`ical-url' - a URL for the event"
  :version "31.1"
  :type '(radio (function-item di:vevent-skeleton)
                (function :tag "Other function")))

(defcustom di:vjournal-format-function #'di:vjournal-skeleton
  "Function to format VJOURNAL components for the diary.

This function is called with one argument VJOURNAL, an
`icalendar-vjournal'.  It should insert formatted data from this journal
entry into the current buffer.  It is convenient to express such a
function as a skeleton; see `define-skeleton' and `skeleton-insert' for
more information, and see `diary-icalendar-vjournal-skeleton' for an
example.  See `icalendar-with-component' for a convenient way to bind
the data in VJOURNAL.

For convenience when writing this function as a skeleton, the following
variables will be (dynamically) bound when the function is called.  All
values will be strings unless another type is noted, or nil:

`ical-attachments' (list of strings) - URLs or filenames of attachments
  in the journal entry
`ical-attendees' (list of strings) - the participants of the journal entry,
  formatted by `diary-icalendar-attendee-format-function'
`ical-categories' (list of strings) - categories specified in the journal entry
`ical-access' - the journal entry's access classification
`ical-comments' (list of strings) - comments specified in the journal entry
`ical-description' - the journal entry's description(s) as a single
  string, separated by newlines (more than one description is allowed in
  VJOURNAL components)
`ical-start' - start date and time in a single string.  When importing,
  includes the date, otherwise just the (local) time.
`ical-importing' (a boolean) - t if the journal entry should be
  formatted for import.  When nil, the entry should be formatted for
  display rather than import.  When importing it is important to include
  all information from the entry that you want to be saved in the diary;
  when displaying, information like the date (or date-related
  S-expressions) and UID can be left out.
`ical-nonmarking' (a boolean) - if non-nil, the diary entry should be nonmarking
`ical-organizer' - the journal entry's organizer, formatted by
  `diary-icalendar-attendee-format-function'
`ical-rrule-sexp' - a string containing a diary S-expression for a recurring
  journal entry.  If this is non-nil, you should normally use it instead
  of the start-* variables to form the date of the entry.
`ical-status' - overall status specified by the organizer (e.g. \"draft\")
`ical-summary' - a summary of the journal entry
`ical-uid' - the unique identifier of the journal entry
`ical-url' - a URL for the journal entry"
  :version "31.1"
  :type '(radio (function-item di:vjournal-skeleton)
                (function :tag "Other function")))

(defcustom di:import-vjournal-as-nonmarking t
  "Whether to import VJOURNAL components as nonmarking diary entries.

If this variable is non-nil, VJOURNAL components will be imported into
the diary as \"nonmarking\" entries by prefixing
`diary-nonmarking-symbol'.  This means they will not cause their date to
be marked in the calendar when the command `diary-mark-entries' is
called.  See Info node `(emacs)Displaying the Diary' for more
information."
  :version "31.1"
  :type '(choice (const :tag "Import as nonmarking entries" t)
                 (const :tag "Import as normal (marking) entries" nil)))

(defcustom di:vtodo-format-function #'di:vtodo-skeleton
  "Function to format VTODO components for the diary.

This function is called with one argument VTODO, an `icalendar-vtodo'.
It should insert formatted data from this task into the current buffer.
It is convenient to express such a function as a skeleton; see
`define-skeleton' and `skeleton-insert' for more information.  See
`icalendar-with-component' for a convenient way to bind the data in
VTODO.

For convenience when writing this function as a skeleton, the following
variables will be (dynamically) bound when the function is called.  All
values will be strings unless another type is noted, or nil:

`ical-as-alarm' (symbol) - non-nil when the task should be formatted for
  an alarm notification in advance of the task.  The symbol indicates
  the type of alarm: `email' means to format the task as the body of an
  email.  (Currently only used for EMAIL alarms; see
  `diary-icalendar-export-alarms'.)
`ical-attachments' (list of strings) - URLs or filenames of attachments
  in the task
`ical-attendees' (list of strings) - the participants of the task,
  formatted by `diary-icalendar-attendee-format-function'
`ical-categories' (list of strings) - categories specified in the task
`ical-access' - the task's access classification
`ical-comments' (list of strings) - comments specified in the task
`ical-completed' - when the task was completed, formatted as a local
  date-time string
`ical-description' - the task's description
`ical-start' - start-date and time in a single string.  When importing,
  includes the date, otherwise just the (local) time
`ical-start-to-end' - a single string containing both start and due date
  and time.  If the task starts and ends on the same day, the date is
  not repeated.  When importing, dates are included, and the string may
  contain a diary s-exp; when displaying, the string contains only the
  times for the displayed date.  If there is no end date, same as
  `ical-start'.
`ical-due' - due date and time in a single string
`ical-end' - same as `ical-due'
`ical-work-time-sexp' - when the task has both a start date and a due date,
  this is a %%(diary-time-block ...) diary S-expression representing the
  time from the start date to the due date (only non-nil when
  importing).  You can use this e.g. to make a separate entry for the
  task's work time, so that it shows up every day in the diary until it
  is due.
`ical-importing' (a boolean) - t if the task should be formatted for import.
  When nil, the task should be formatted for display rather than import.
  When importing it is important to include all information from the task
  that you want to be saved in the diary; when displaying, information like
  the date (or date-related S-expressions) and UID can be left out.
`ical-location' - the task's location, or geographical coordinates
`ical-nonmarking' (a boolean) - if non-nil, the diary entry should be nonmarking
`ical-organizer' - the task's organizer, formatted by
  `diary-icalendar-attendee-format-function'
`ical-percent-complete' (a number between 0 and 100) - the percentage of
  the task which has already been completed
`ical-priority' (a number) - the task's priority (1 = highest priority,
  9 = lowest; 0 = undefined)
`ical-rrule-sexp' - a string containing a diary S-expression for a
  recurring task (only non-nil when importing).  When this is non-nil,
  you should normally use it instead of the start and end variables to
  form the date of the entry.
`ical-status' - overall status specified by the organizer (e.g. \"confirmed\")
`ical-summary' - a summary of the task
`ical-uid' - the unique identifier of the task
`ical-url' - a URL for the task"
  :version "31.1"
  :type '(radio (function-item di:vjournal-skeleton)
                (function :tag "Other function")))

(defcustom di:import-predicate #'identity
  "Predicate to filter iCalendar components before importing.

This function must accept one argument, which will be an
`icalendar-vevent', `icalendar-vtodo', or `icalendar-vjournal'
component.  It should return non-nil if this component should be
formatted for import, or nil if it should be skipped.

The default value will format all the events, todos, and journal entries
in a given calendar."
  :version "31.1"
  :type '(radio (function-item identity)
                (function :tag "Other predicate")))

;;; Export customization
(defgroup diary-icalendar-export nil
  "iCalendar export from diary."
  :version "31.1"
  :group 'diary-icalendar
  :prefix 'diary-icalendar)

(defcustom di:address-regexp
  (rx line-start
      (one-or-more space)
      (zero-or-one ;; property prefix, e.g. "Attendee:" or "Organizer:"
       (seq (one-or-more word) ":"))
      (group-n 2 (zero-or-more (not (any "<" "\n"))))
      "<"
      (group-n 1 (one-or-more (not (any "@" "\n")))
                 "@"
                 (one-or-more (not (any ">" "\n"))))
      ">")
  "Regular expression to match calendar user (email) addresses.

The full address should match group 1; \"mailto:\" will be prepended to
the full address during export, unless it or another URI scheme is
present.  If there is a match in group 2, it will be used as the
common name associated with the address (see `icalendar-cnparam').

The default value matches names and addresses on lines like:

  Ms. Baz <baz@foo.com>

as well as on lines like:

  Property: Ms. Baz <baz@foo.com> other data...

Any matching address within a diary entry will be exported as an
iCalendar ATTENDEE property, unless the line on which it appears is also
a match for `diary-icalendar-organizer-regexp', in which case it will be
exported as the ORGANIZER property."
  :version "31.1"
  :type '(regexp))

(defcustom di:description-regexp nil
  "Regular expression to match description in an entry.

If this is nil, the entire entry (after the date and time specification)
is used as the description.  Thus, it is only necessary to set this
variable if you want to export diary entries where the text to be used
as the description should not include the full entry body.  In that case,
the description should match group 1 of this regexp."
  :version "31.1"
  :type '(radio
          (const :tag "Use full entry body" nil)
          (regexp :tag "Regexp")))

(defcustom di:organizer-regexp
  (rx line-start
      (one-or-more space)
      "Organizer:")
  "Regular expression to match line of an entry specifying the ORGANIZER.

This regular expression need *not* match the name and address of the
organizer (`diary-icalendar-address-regexp' is responsible for that).
It only needs to match a line on which the organizer's address appears,
to distinguish the organizer's address from other addresses."
  :version "31.1"
  :type '(regexp))

(defcustom di:class-regexp
  (rx line-start
      (one-or-more space)
      (or "Class:" ; for backward compatibility
          "Access:")
      (zero-or-more space)
      (group-n 1 (or "public" "private" "confidential")))
  "Regular expression to match access classification.

The access classification value should be matched by group 1.  The default
regexp matches access classifications like:
  Access: C
or
  Class: C
where C can be any of:
  public
  private
  confidential"
  :version "31.1"
  :type '(regexp))

(defcustom di:location-regexp
  (rx line-start
      (one-or-more space)
      "Location:"
      (zero-or-more space)
      (group-n 1 (one-or-more not-newline)))
  "Regular expression to match location of an event.

The location value should be matched by group 1.  The default regexp
matches lines like:

  Location: Some place"
  :version "31.1"
  :type '(regexp))

(defcustom di:status-regexp
  (rx line-start
      (one-or-more space)
      "Status:"
      (zero-or-more space)
      (group-n 1 (or "tentative" "confirmed" "cancelled" "needs-action" "completed"
                     "in-process" "draft" "final")))
  "Regular expression to match status of an event.

The status value should be matched by group 1.  The default regexp
matches statuses on lines like:

  Status: S

where S can be any of:

  tentative
  confirmed
  cancelled
  needs-action
  completed
  in-process
  draft
  final"
  :version "31.1"
  :type '(regexp))

(defcustom di:summary-regexp nil
  "Regular expression to match summary in an entry.

If this is nil, the first line of the entry (after the date and time
specification) is used as the summary.  Thus, it is only necessary to set
this variable if you want to export diary entries where the text to be
used as the summary does not appear on the first line of the entry.  In
that case, the summary should match group 1 of this regexp."
  :version "31.1"
  :type '(choice (const nil) regexp))

(defcustom di:todo-regexp nil
  "Regular expression that identifies an entry as a task (VTODO).

If this is non-nil, any diary entry that matches this regexp will be
exported as an iCalendar VTODO component (instead of VEVENT), with its
due date equal to the entry date."
  :version "31.1"
  :type '(radio (const :tag "Do not export VTODO tasks" nil)
                (regexp :tag "Regexp for tasks")))

(defcustom di:uid-regexp
  (rx line-start
      (one-or-more space)
      "UID:"
      (zero-or-more space)
      (group-n 1 (one-or-more not-newline)))
  "Regular expression to match UID of an entry.

The UID value should be matched by group 1.  The default regexp matches
UIDs on lines like:

  UID: some-unique-identifier"
  :version "31.1"
  :type '(regexp))

(defcustom di:url-regexp
  (rx line-start
      (one-or-more space)
      "URL:"
      (zero-or-more space)
      (group-n 1 (eval 'ical:uri)))
  "Regular expression to match URL of an entry.

The full URL should be matched by group 1.  The default regexp matches
URLs on lines like:

  URL: http://example.com/foo/bar"
  :version "31.1"
  :type '(regexp))

(defcustom di:export-nonmarking-entries t
  "Whether to export nonmarking diary entries.

If this variable is nil, nonmarking diary entries (those prefixed with
`diary-nonmarking-symbol') are never exported.  If it is non-nil,
nonmarking diary entries are exported; see also
`diary-icalendar-export-nonmarking-as-vjournal' for more control over
how they are exported."
  :version "31.1"
  :type '(choice (const :tag "Export nonmarking entries" t)
                 (const :tag "Do not export nonmarking entries" nil)))

(defcustom di:export-nonmarking-as-vjournal nil
  "Whether to export nonmarking diary entries as VJOURNAL components.

If this variable is non-nil, nonmarking diary entries (those prefixed
with `diary-nonmarking-symbol') will be exported as iCalendar VJOURNAL
components, rather than VEVENT components.  VJOURNAL components are
intended to represent notes, documents, or other data associated with a
date.  External calendar applications may treat VJOURNAL components
differently than VEVENTs, so consult your application's documentation
before setting this variable to t.

If this variable is nil, nonmarking entries will be exported as VEVENT
components which do not take up busy time in the calendar (i.e., with
the TRANSP property set to \"TRANSPARENT\"; see `icalendar-transp')."
  :version "31.1"
  :type '(choice (const :tag "Export nonmarking entries as VEVENT" nil)
                 (const :tag "Export nonmarking entries as VJOURNAL" t))
  :link '(url-link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.3"))

(defcustom di:export-alarms
  nil
  "Determine whether and how alarms are included in exported diary events.

If this variable is nil, no alarms are created during export.
If it is non-nil, it should be a list of lists like:

\((TYPE LEAD-TIME [OPTIONS]) ...)

In each inner list, the first element TYPE should be a symbol indicating
an alarm type to generate: one of \\='audio, \\='display, or \\='email.
The second element LEAD-TIME should be an integer specifying the amount
of time before the event, in minutes, when the alarm should be
triggered.  For audio alarms, there are currently no other
OPTIONS.

For display and email alarms, the next OPTION is a format string for the
displayed alarm, or the email subject line.  In this string, \"%t\" will
be replaced with LEAD-TIME and \"%s\" with the event's summary.

If TYPE is \\='email, the next OPTION should be a list whose members
specify the email addresses to which email alarms should be sent.  These
can either be email addresses (as strings), or the symbol
\\='from-entry, meaning that these addresses should be taken from the
exported diary entry (see `diary-icalendar-address-regexp')."
  :version "31.1"
  :type
  '(choice (const :tag "Do not include alarms when exporting diary entries" nil)
           (set :tag "Create alarms of these types"
                (list :tag "Audio alarms"
                      (const :tag "Options" audio)
                      (integer :tag "Advance time (in minutes)"
                               :value 10)
                      ;; TODO: specify an audio file to attach?
                      ;; TODO: other options we could have here and below:
                      ;; - whether alarm is before event start or end
                      ;; - repetitions and delays between repetitions
                      )
                (list :tag "Display alarms"
                      (const :tag "Options" display)
                      (integer :tag "Advance time (minutes)"
                               :value 10)
                      (string :tag "Display format"
                              :value "In %t minutes: %s")
                      ;; TODO: other options?
                      )
                (list :tag "Email alarms"
                      (const :tag "Options" email)
                      (integer :tag "Advance time (minutes)"
                               :value 10)
                      ;; TODO: other options?
                      (string :tag "Subject line format"
                              :value "In %t minutes: %s")
                      (set
                       :tag "Attendees"
                       (const :tag "Parse addresses from entry"
                              from-entry)
                       (repeat :tag "Other addresses"
                               (string :tag "Email address")))))))

(defcustom di:export-sexp-enumeration-days
  14
  "Number of days over which an S-expression diary entry is enumerated.

Some S-expression entries cannot be translated to iCalendar format.
They are therefore enumerated, i.e., explicitly evaluated for a
certain number of days, and then exported.  The enumeration starts
on the current day and continues for the number of days given here.

See `icalendar-export-sexp-enumerate-all' for a list of sexp
entries which by default are NOT enumerated."
  :version "31.1"
  :type 'integer)

(defcustom di:export-sexp-enumerate-all
  nil
  "Whether all S-expression diary entries are enumerated.

If this variable is non-nil, all S-expression diary entries are
enumerated for `diary-icalendar-export-sexp-enumeration-days' days
instead of translating them into an iCalendar equivalent.
This causes the following S-expression entries to be enumerated
instead of translated to a recurrence rule:
 `diary-anniversary'
 `diary-block'
 `diary-cyclic'
 `diary-date'
 `diary-float'
 `diary-remind'
 `diary-rrule'
 `diary-time-block'
All other S-expression entries are enumerated in any case."
  :version "31.1"
  :type '(choice (const :tag "Export without enumeration when possible" nil)
                 (const :tag "Always enumerate S-expression entries" t)))

(defcustom di:recurring-start-year
  (1- (decoded-time-year (decode-time)))
  "Start year for recurring events.

Set this to a year just before the start of your personal calendar.
It is needed when exporting certain diary S-expressions to iCalendar
recurring events, and because some calendar browsers only propagate
recurring events for several years beyond the start time."
  :version "31.1"
  :type 'integer)

(defcustom di:time-zone-export-strategy
  'local
  "Strategy to use for exporting clock times in diary files.

The symbol `local' (the default) means to assume that times are in the
time zone determined by `calendar-current-time-zone'.  The time zone
information returned by that function will be exported as an iCalendar
VTIMEZONE component, and clock times in the diary file will be exported
with a reference to that time zone definition.

On some systems, `calendar-current-time-zone' cannot determine time zone
information for the local time zone.  In that case, you can set this
variable to a list in the format returned by that function:

 (UTC-DIFF DST-OFFSET STD-ZONE DST-ZONE
  DST-STARTS DST-ENDS DST-STARTS-TIME DST-ENDS-TIME)

This list describes the time zone you would like to use for export.  See
the docstring of `calendar-current-time-zone' for details.  Times in the
diary file will be exported like with `local' for this time zone.

The other possible values for this variable avoid the need to include
any time zone information in the exported iCalendar data:

The symbol `to-utc' means to re-encode all exported times to UTC
time.  In this case, export will assume that times are in Emacs local
time, and rely on `encode-time' and `decode-time' to convert them to UTC
times.

The symbol `floating' means to export clock times without any time
zone identifier, which the iCalendar standard (RFC5545) calls
\"floating\" times.  RFC5545 specifies that floating times should be
interpreted as local to whichever time zone the recipient of the
iCalendar data is currently in (which might be different from your local
time zone).  You should only use this if that behavior makes sense for
the events you are exporting."
  :version "31.1"
  :type
  '(radio (const :tag "Use TZ from `calendar-current-time-zone'" local)
          (const :tag "Convert local times to UTC" to-utc)
          (const :tag "Use floating times" floating)
          (sexp :tag "User-provided TZ information"
                :match icr:-tz-info-sexp-p
                :type-error
                "See `calendar-current-time-zone' for format"))
  :link '(url-link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.5"))

(defcustom di:export-linewise
  nil
  "Export entries with multiple lines to distinct events.

If this is non-nil, each line of a diary entry will be exported as a
separate iCalendar event.

If you write your diary entries in a one-entry-per-day style, with
multiple events or appointments per day, you can use this variable to
export these individual events to iCalendar format.  For example, an
entry like:

2025-05-03
  9AM Lab meeting
    Günter to present on new assay
  Start experiment A
  12:30-1:30PM Lunch with Phil
  16:00 Experiment A finishes; move to freezer

will be exported as four events, each on 2025-05-03 but with different
start times (except for the second event, \"Start experiment A\", which
has no start time).  An event line can be continued onto subsequent lines
via additional indentation, as in the first event in this entry.

If this variable is non-nil, each distinct event must begin on a
continuation line of the entry (below the date); any text on the same
line as the date is ignored.  A time specification can only appear at
the beginning of each continuation line of the entry, immediately after
the leading whitespace.

If this variable is nil, each entry will be exported as exactly one
event, and only a time specification immediately following the date will
determine the start and end times for that event.  Thus, in the example
above, the exported event would have a start date but no start time or
end time.  The times in the entry would be preserved as text in the
event description."
  :version "31.1"
  :type '(radio (const :tag "Do not export linewise" nil)
                (const :tag "Export linewise" t)))

(defcustom di:other-properties-parser nil
  "Function to parse additional iCalendar properties from diary entries.

If you like to keep your diary entries in a particular format, you can
set this to a function which parses that format to iCalendar properties
during iCalendar export, so that other calendar applications can use
them.

The parsing function will be called with the current restriction set to
the boundaries of a diary entry.  If `diary-icalendar-export-linewise'
is non-nil, the restriction will correspond to a single event in a
multi-line diary entry.

The function should accept two arguments, TYPE and PROPERTIES.  TYPE is
the iCalendar type symbol (one of \\='icalendar-vevent,
\\='icalendar-vjournal, or \\='icalendar-vtodo) for the component being
generated for the entry.  PROPERTIES is the list of property nodes that
`diary-icalendar-parse-entry' has already parsed from the entry and will
be included in the exported component.

The function should return a list of iCalendar property nodes, which
(in addition to PROPERTIES) will be incorporated into the
`icalendar-vevent', `icalendar-vjournal', or `icalendar-vtodo' component
node created from the current entry.  See the docstrings of those
symbols for more information on the properties they can contain, and the
`icalendar-make-property' macro for a simple way to create property
nodes from values parsed from the entry."
  :version "31.1"
  :type '(radio (const :tag "Do not parse additional properties" nil)
                (function :tag "Parsing function")))


;; Utilities for display and import

;;; Error handling
(define-error 'ical:diary-import-error "Unable to import iCalendar data"
              'ical:error)

(cl-defun di:signal-import-error (msg &key (diary-buffer (current-buffer))
                                           (position (point))
                                           line
                                           (severity 2))
  (let ((err-data
          (list :message msg
                :buffer diary-buffer
                :position position
                :line line
                :severity severity)))
    (signal 'ical:diary-import-error err-data)))

;;; Backward compatibility with icalendar.el

;; icalendar.el provided the following customization variables:
;; `icalendar-import-format'
;; `icalendar-import-format-class'
;; `icalendar-import-format-description'
;; `icalendar-import-format-location'
;; `icalendar-import-format-organizer'
;; `icalendar-import-format-summary'
;; `icalendar-import-format-status'
;; `icalendar-import-format-url'
;; `icalendar-import-format-uid'
;; These were all format strings: `icalendar-import-format' was the
;; top-level format string, which would potentially incorporate the
;; formatted output from the others.  This approach to customization
;; isn't very flexible, though, and doing it right requires a
;; separate defcustom variable for each iCalendar property.  (The above
;; list is not nearly exhaustive.) I have abandoned this approach in
;; what follows in favor of skeleton.el templates, but the following two
;; functions provide backward compatibility for anyone who had
;; customized the values of the above variables:
(defun di:-use-legacy-vars-p ()
  "Return non-nil if user has set `icalendar-import-format*' variables.
If any of these variables have non-default values, they will be used by
`diary-icalendar-import-format-entry' to import events.  This function
is for backward compatibility; please do not rely on it in new code."
  (declare (obsolete nil "31.1"))
  (with-suppressed-warnings
      ((obsolete ical:import-format
                 ical:import-format-class
                 ical:import-format-description
                 ical:import-format-location
                 ical:import-format-organizer
                 ical:import-format-summary
                 ical:import-format-status
                 ical:import-format-url
                 ical:import-format-uid))
    (or
     (and (boundp 'ical:import-format)
          (not (equal ical:import-format
                      (custom--standard-value 'ical:import-format))))
     (and (boundp 'ical:import-format-class)
          (not (equal ical:import-format-class
                      (custom--standard-value 'ical:import-format-class))))
     (and (boundp 'ical:import-format-description)
          (not (equal ical:import-format-description
                      (custom--standard-value
                       'ical:import-format-description))))
     (and (boundp 'ical:import-format-location)
          (not (equal ical:import-format-location
                      (custom--standard-value 'ical:import-format-location))))
     (and (boundp 'ical:import-format-organizer)
          (not (equal ical:import-format-organizer
                      (custom--standard-value 'ical:import-format-organizer))))
     (and (boundp 'ical:import-format-summary)
          (not (equal ical:import-format-summary
                      (custom--standard-value 'ical:import-format-summary))))
     (and (boundp 'ical:import-format-status)
          (not (equal ical:import-format-status
                      (custom--standard-value 'ical:import-format-status))))
     (and (boundp 'ical:import-format-url)
          (not (equal ical:import-format-url
                      (custom--standard-value 'ical:import-format-url))))
     (and (boundp 'ical:import-format-uid)
          (not (equal ical:import-format-uid
                      (custom--standard-value 'ical:import-format-uid)))))))

(defun di:-format-vevent-legacy (date class desc location organizer
                                 summary status url uid)
  "Format an entry on DATE using the values of obsolete import variables.
This function is for backward compatibility; please do not rely on it in
new code."
  (declare (obsolete nil "31.1"))
  (with-suppressed-warnings
      ((obsolete ical:import-format
                 ical:import-format-class
                 ical:import-format-description
                 ical:import-format-location
                 ical:import-format-organizer
                 ical:import-format-summary
                 ical:import-format-status
                 ical:import-format-url
                 ical:import-format-uid))

    (insert ical:import-format)
    (replace-regexp-in-region "%c"
                              (format ical:import-format-class class)
                              (point-min) (point-max))
    (replace-regexp-in-region "%d"
                              (format ical:import-format-description desc)
                              (point-min) (point-max))
    (replace-regexp-in-region "%l"
                              (format ical:import-format-location location)
                              (point-min) (point-max))
    (replace-regexp-in-region "%o"
                              (format ical:import-format-organizer organizer)
                              (point-min) (point-max))
    (replace-regexp-in-region "%s"
                              (format ical:import-format-summary summary)
                              (point-min) (point-max))
    (replace-regexp-in-region "%t"
                              (format ical:import-format-status status)
                              (point-min) (point-max))
    (replace-regexp-in-region "%u"
                              (format ical:import-format-url url)
                              (point-min) (point-max))
    (replace-regexp-in-region "%U"
                              (format ical:import-format-uid uid)
                              (point-min) (point-max))
    (goto-char (point-min))
    (insert date " ")))

(defun di:-vevent-to-legacy-alist (vevent)
  "Convert an `icalendar-vevent' to an alist of the kind used by icalendar.el.
This function is for backward compatibility; please do not rely on it in
new code."
  (declare (obsolete nil "31.1"))
  ;; function values of `icalendar-import-format' expect a list like:
  ;; ((VEVENT nil
  ;;   ((PROP1 params val)
  ;;    (PROP2 params val)
  ;;    ...)))
  (let ((vevent-children (ical:ast-node-children vevent))
        children)
    (dolist (p vevent-children)
      (let* ((type (ical:ast-node-type p))
             (list-sep (get type 'ical:list-sep))
             (name (intern (car (rassq type ical:property-types))))
             ;; icalendar.el did not interpret values when parsing, so we
             ;; convert back to string representation:
             (value (ical:ast-node-value p))
             (value-str
              (or (ical:ast-node-meta-get :original-value p)
                  (if list-sep
                      (string-join (mapcar #'ical:default-value-printer value)
                                   list-sep)
                    (ical:default-value-printer value))))
             params)
        (when (ical:ast-node-children p)
          (dolist (param (ical:ast-node-children p))
            (let* ((par-str (ical:print-param-node param))
                   (split (string-split par-str "[;=]"))
                   (parname (intern (nth 1 split)))
                   (parval (nth 2 split)))
              (push `(,parname nil ,parval) params)))
          (setq params (nreverse params)))
        (push `(,name ,params ,value-str) children)))
    (setq children (nreverse children))
    ;; Return the legacy alist:
    `((VEVENT nil ,children))))

;;; Other utilities

(defconst di:entry-regexp
  (rx line-start
      (group-n 1 ; first line of entry
        (or (group-n 2 (regexp diary-nonmarking-symbol))
            (not (any "\t\n #")))
        (one-or-more not-newline))
      (group-n 3 ; continuation lines of entry
        (zero-or-more "\n" (any space) (zero-or-more not-newline))))
  "Regular expression to match a full diary entry.

Group 1 matches the first line of the entry.  Group 2 contains
`diary-nonmarking-symbol', if it was present at the start of the first
line.  Group 3 contains any continuation lines of the entry.")

;; TODO: move to diary-lib.el?
(defun di:entry-bounds ()
  "Return markers (START END) bounding the diary entry around point.
If point is not in an entry, return nil."
  (save-excursion
    (let* ((pt (point))
           (bound (point-min))
           (start (make-marker))
           (end (make-marker)))
      (when (re-search-backward "^[[:space:]]*$" nil t)
        (setq bound (match-end 0)))
      (goto-char pt)
      (cond ((looking-at di:entry-regexp)
             (set-marker start (match-beginning 0))
             (set-marker end (match-end 0)))
            ((re-search-backward di:entry-regexp bound t)
             (set-marker start (match-beginning 0))
             ;; match again forward, to ensure we get the full entry;
             ;; see `re-search-backward':
             (goto-char start)
             (when (looking-at di:entry-regexp)
               (set-marker end (match-end 0))))
            (t nil))
      (when (and (marker-position start) (marker-position end))
        (list start end)))))

(defun di:find-entry-with-uid (uid &optional diary-filename)
  "Search DIARY-FILENAME (default: `diary-file') for an entry containing UID.

The UID must occur on a line matching `diary-icalendar-uid-regexp'.  If
such an entry exists, return markers (START END) bounding it.
Otherwise, return nil."
  (let* ((diary-file (or diary-filename diary-file))
         (diary-buffer (or (find-buffer-visiting diary-file)
                           (find-file-noselect diary-file))))
    (with-current-buffer diary-buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (catch 'found
            (while (re-search-forward di:uid-regexp nil t)
              (when (equal uid (match-string 1))
                (throw 'found (di:entry-bounds))))
            ;; continue search in included files:
            ;; TODO: is this a good idea?
            ;; (goto-char (point-min))
            ;; (while (re-search-forward
            ;;         (rx line-start (regexp diary-include-string)
            ;;             ?\" (group-n 1 (one-or-more (not ?\")) ?\"))
            ;;         nil t)
            ;;   (let ((entry (di:find-entry-with-uid uid (match-string 1))))
            ;;     (when entry
            ;;       (throw 'found entry))))
            ;; nothing to return:
            nil))))))

(defun di:y-or-n-or-edit-p (prompt)
  "Like `y-or-n-p', but with the option to enter a recursive edit.
Adds a message to current binding of `help-form' explaining how."
  (let* ((allow-edits-map
          (let ((map (make-sparse-keymap)))
            (define-key map [remap edit]
                        (lambda ()
                          (interactive)
			  (save-excursion
			    (save-window-excursion
			      (recursive-edit)))))
            map))
         (y-or-n-p-map (make-composed-keymap allow-edits-map
                                             y-or-n-p-map))
         (help-form
          (concat (when (stringp help-form) (concat help-form "\n\n"))
                  ;; FIXME: should use substitute-command-keys here, but
                  ;; for some reason, even with \<y-or-n-p-map>, it
                  ;; doesn't find the C-r and C-M-c bindings and only
                  ;; suggests M-x ...
                  "Type C-r to enter recursive edit before answering "
                  "(C-M-c to exit).")))
    (save-excursion
      (save-restriction
        (y-or-n-p prompt)))))

;;; Skeletons
;;
;; We use skeleton.el's templating facilities to make formatting of
;; different iCalendar elements in the diary simple and easy to
;; customize.  There are default skeletons for each major type of
;; iCalendar component (`di:vevent-skeleton', `di:vtodo-skeleton',
;; `di:vjournal-skeleton'), and a corresponding defcustom pointing to
;; each of these skeletons (`di:vevent-format-function', etc.).
;; `di:format-entry' calls these skeletons, or user-provided functions,
;; to format individual components as diary entries.  Since properties
;; representing people (`icalendar-attendee', `icalendar-organizer') are
;; important and relatively complex, another skeleton
;; (`di:attendee-skeleton') takes care of formatting these for the
;; top-level component skeletons.
(defun di:attendee-skeleton (attendee)
  "Default skeleton to format an `icalendar-attendee' for the diary.

Includes any data from the attendee's `icalendar-cnparam' and
`icalendar-partstatparam', and does not insert any data if its
`icalendar-cutypeparam' is non-nil and anything other than
\"INDIVIDUAL\" or \"GROUP\".

The result looks like:
  <foo@example.com>
or
  Baz Foo <foo@example.com>
or
  Baz Foo <foo@example.com> (declined)"
  (ignore attendee) ; we only need the `attendee-' vars below
  (with-suppressed-warnings ((free-vars attendee-cutype))
    ;; skip non-human "attendees":
    (when (or (not attendee-cutype)
              (equal attendee-cutype "INDIVIDUAL")
              (equal attendee-cutype "GROUP"))
      (skeleton-insert
       '(nil
         attendee-cn
         (format " <%s>" attendee-address)
         (when attendee-partstat
           (format " (%s)" (downcase attendee-partstat))))))))

(defun di:format-attendee (attendee)
  "Format ATTENDEE for the diary.

ATTENDEE should be an `icalendar-attendee' or `icalendar-organizer'
property node.  Returns a string representing an entry for the attendee,
formatted by `diary-icalendar-attendee-format-function', unless the
attendee's address matches the regexp in
`diary-icalendar-skip-addresses-regexp'; in that case, nil is returned."
  (ical:with-property attendee
    ((ical:cutypeparam :value cutype)
     (ical:cnparam :value cn)
     (ical:roleparam :value role)
     (ical:partstatparam :value partstat)
     (ical:rsvpparam :value rsvp))
    (unless (and di:skip-addresses-regexp
                 (string-match-p di:skip-addresses-regexp value))
      (dlet ((attendee-address (ical:strip-mailto value))
             (attendee-cn (when cn (string-trim cn)))
             (attendee-cutype cutype)
             (attende-role role)
             (attendee-partstat partstat)
             (attendee-rsvp rsvp))
        (with-temp-buffer
          (funcall di:attendee-format-function attendee)
          (buffer-string))))))

(defun di:vevent-skeleton (vevent)
  "Default skeleton to format an `icalendar-vevent' for the diary."
  (ignore vevent)  ; we only need the dynamic `ical-*' variables here
  (skeleton-insert
   '(nil
     (when (or ical-nonmarking (equal ical-transparency "TRANSPARENT"))
       diary-nonmarking-symbol)
     (or ical-rrule-sexp ical-start-to-end ical-start) & " "
     ical-summary "\n"
     @ ; start of body (for indentation)
     (when ical-location "Location: ") ical-location
     & "\n" (when ical-url "URL: ") & ical-url
     & "\n" (when ical-status "Status: ") & ical-status
     & "\n" (when ical-organizer "Organizer: ") & ical-organizer
     & "\n" (di:format-list ical-attendees "Attendee")
     & "\n" (di:format-list ical-categories "Category" "Categories")
     & "\n" (di:format-list ical-comments "Comment")
     & "\n" (di:format-list ical-contacts "Contact")
     & "\n" (di:format-list ical-attachments "Attachment")
     & "\n" (when (and ical-importing ical-access) "Access: ") & ical-access
     & "\n" (when (and ical-importing ical-uid) "UID: ") & ical-uid
     & "\n" (when ical-description "Description: ") & ical-description
     & "\n"
     @ ; end of body
     (let* ((end (pop skeleton-positions))
            (start (pop skeleton-positions)))
       ;; TODO: should diary define a customizable indentation level?
       ;; For now, we use 1 because that's what icalendar.el chose
       (indent-code-rigidly start end 1)
       nil) ; Don't insert return value
     (when ical-importing "\n"))))

(defun di:vjournal-skeleton (vjournal)
  "Default skeleton to format an `icalendar-vjournal' for the diary."
  (ignore vjournal)  ; we only need the dynamic `ical-*' variables here
  (skeleton-insert
   '(nil
     (when (or ical-nonmarking di:import-vjournal-as-nonmarking)
       diary-nonmarking-symbol)
     (or ical-rrule-sexp ical-start) & " "
     ical-summary "\n"
     @ ; start of body (for indentation)
     & "\n" (when ical-url "URL: ") & ical-url
     & "\n" (when ical-status "Status: ") & ical-status
     & "\n" (when ical-organizer "Organizer: ") & ical-organizer
     & "\n" (di:format-list ical-attendees "Attendee")
     & "\n" (di:format-list ical-categories "Category" "Categories")
     & "\n" (di:format-list ical-comments "Comment")
     & "\n" (di:format-list ical-contacts "Contact")
     & "\n" (di:format-list ical-attachments "Attachment")
     & "\n" (when (and ical-importing ical-access) "Access: ") & ical-access
     & "\n" (when (and ical-importing ical-uid) "UID: ") & ical-uid
     ;; In a vjournal, multiple `icalendar-description's are allowed:
     & "\n" (di:format-list ical-descriptions "Description")
     & "\n"
     @ ; end of body
     (let* ((end (pop skeleton-positions))
            (start (pop skeleton-positions)))
       (indent-code-rigidly start end 1)
       nil) ; Don't insert return value
     (when ical-importing "\n"))))

(defun di:vtodo-skeleton (vtodo)
  "Default skeleton to format an `icalendar-vtodo' for the diary."
  (ignore vtodo)  ; we only need the dynamic `ical-*' variables here
  (skeleton-insert
   '(nil
     (when ical-nonmarking diary-nonmarking-symbol)
     (or ical-rrule-sexp ical-due) & " "
     (when ical-due "Due: ") summary
     (when start (concat " (Start: " ical-start ")"))
     "\n"
     @ ; start of body (for indentation)
     & "\n" (when ical-url "URL: ") & ical-url
     & "\n" (when ical-status "Status: ") & ical-status
     & "\n" (when ical-completed "Completed: ") & ical-completed
     & "\n" (when ical-percent-complete
              (format "Progress: %d%%" ical-percent-complete))
     & "\n" (when ical-organizer "Organizer: ") & ical-organizer
     & "\n" (di:format-list ical-attendees "Attendee")
     & "\n" (di:format-list ical-categories "Category" "Categories")
     & "\n" (di:format-list ical-comments "Comment")
     & "\n" (di:format-list ical-contacts "Contact")
     & "\n" (di:format-list ical-attachments "Attachment")
     & "\n" (when (and ical-importing ical-access) "Access: ") & ical-access
     & "\n" (when (and ical-importing ical-uid) "UID: ") & ical-uid
     & "\n" (when ical-description "Description: ") & ical-description
     & "\n"
     @ ; end of body
     (let* ((end (pop skeleton-positions))
            (start (pop skeleton-positions)))
       (indent-code-rigidly start end 1)
       nil) ; Don't insert return value
     (when ical-importing "\n"))))

;;; Further utilities for formatting/importing special kinds of values:
(defun di:format-geo-coordinates (geo)
  "Format an `icalendar-geo-coordinates' value as degrees N/S and E/W."
  (format "%.6f°%s %.6f°%s" ; RFC5545 says we may truncate after 6 decimal places
          (abs (car geo)) (if (< 0 (car geo)) "N" "S")
          (abs (cdr geo)) (if (< 0 (cdr geo)) "E" "W")))

(defun di:save-binary-attachment (base64-data dir &optional mimetype)
  "Decode and save BASE64-DATA to a new file in DIR.

The file will be named based on a unique prefix of BASE64-DATA with an
extension based on MIMETYPE.  It will be saved in a subdirectory named
DIR of `diary-icalendar-attachment-directory', which will be created if
necessary.  Returns the (non-directory part of) the saved filename."
  (require 'mailcap) ; for `mailcap-mime-type-to-extension'
  ;; Create the subdirectory for the attachment if necessary:
  (unless (and (directory-name-p di:attachment-directory)
               (file-writable-p di:attachment-directory))
    (di:signal-import-error
     (format "Cannot write to directory: %s" di:attachment-directory)))
  (make-directory (file-name-concat di:attachment-directory dir) t)
  ;; Create a unique filename for the attachment.  Unfortunately RFC5545
  ;; has no mechanism for suggesting a filename, so we just use a unique
  ;; prefix of BASE64-DATA, or a random number as a fallback.
  (let* ((nchars 4)
         (max-chars (length base64-data))
         (prefix (substring base64-data 0 nchars))
         (extn (when mimetype
                 (concat "." (symbol-name
                              (mailcap-mime-type-to-extension mimetype)))))
         (path (file-name-concat di:attachment-directory dir
                                 (concat prefix extn))))
    (while (file-exists-p path)
      (cl-incf nchars)
      (setq prefix (if (< nchars max-chars)
                       (substring base64-data 0 nchars)
                     (number-to-string (random max-chars))))
      (setq path (file-name-concat di:attachment-directory dir
                                   (concat prefix extn))))
    ;; Save the file and return its name:
    (let ((data (base64-decode-string base64-data))
          (coding-system-for-write 'no-conversion))
      (write-region data nil path)
      (file-name-nondirectory path))))

(defun di:save-attachments-from (attachment-nodes uid)
  "Save attachments in ATTACHMENT-NODES and return a list of attachments.

If these nodes contain binary data, rather than an URL, save the data to
a file in `diary-icalendar-attachment-directory' (unless this variable
is nil).  UID should be the universal ID of the component containing
ATTACHMENT-NODES; the attachments will be saved in a subdirectory of the
same name.  The returned list is a list of strings, which are either
URLs or filenames."
  (let (entry-attachments)
    (dolist (node attachment-nodes)
      (ical:with-property node
        ((ical:fmttypeparam :value fmttype))
        (when (and (eq 'ical:binary value-type)
                   di:attachment-directory)
          (let ((filename (di:save-binary-attachment value uid fmttype)))
            (push filename entry-attachments)))
        (when (eq 'ical:url value-type)
          (push value entry-attachments))))
    ;; Return the list of filenames and URLs:
    entry-attachments))

(defun di:format-list (values &optional title plural-form sep indent)
  "Smartly format VALUES for the diary.

VALUES should be a list of strings.  nil elements will be ignored, and an
empty list will return nil.

TITLE is a string to add to the beginning of the list; a colon will be
appended.  PLURAL-FORM is the plural of TITLE, to be used when VALUES
contains more than one element (default: TITLE+\"s\").

The strings in VALUES are first joined with SEP (default: \", \"), with
\"TITLE: \" prepended.  If the result is longer than the current value of
`fill-column', the values are instead formatted one per line, with the
title on its own line at the beginning, and the whole list indented
relative to the title by INDENT spaces (default: 2).  Thus, in the first
case, the result looks like:
  TITLE(s): VAL1, VAL2, ...
and in the second:
  TITLE(s):
    VAL1
    VAL2
    ..."
  (when (cdr values)
    (setq title (when title (or plural-form (concat title "s")))))
  (unless indent
    (setq indent 2))
  ;; Remove nil values and extra whitespace:
  (setq values (mapcar #'string-trim (delq nil values)))
  (when values
    (let ((line (concat
                 (when title (concat title ": "))
                 (string-join values (or sep ", ")))))
      (if (< (length line) fill-column)
          line
        ;; Otherwise, one value per line:
        (with-temp-buffer
          (insert (string-join values "\n"))
          (indent-code-rigidly (point-min) (point-max) indent)
          (goto-char (point-min))
          (when title
            (insert title ":\n"))
          (buffer-string))))))

(defun di:format-time (dt &optional tzname)
  "Format the `icalendar-date-time' DT for the diary.
The time is formatted according to `diary-icalendar-time-format', which see.
TZNAME, if specified, should be a string naming the time zone observance
in which DT occurs."
  ;; Diary does not support seconds, so silently truncate:
  (let ((time (format-time-string di:time-format (encode-time dt))))
    (if tzname
        (concat time " " tzname)
      time)))

(defun di:format-time-as-local (dt &optional original-tzname)
  "Format the time in `icalendar-date-time' DT for the diary.

DT is translated to the system local time zone if necessary, and the
original time specification is preserved in parentheses if it was given
in a different zone.  ORIGINAL-TZNAME, if specified, should be a string
naming the time zone observance in which DT was originally encoded in
the iCalendar data."
  (cl-typecase dt
    (ical:date "")
    (ical:date-time
     (let* ((ts (encode-time dt))
            (original-offset (decoded-time-zone dt))
            (local-tz (current-time-zone ts))
            (local-offset (car local-tz))
            (local-dt (decode-time ts local-tz))
            (local-str (di:format-time local-dt)))
       (if (and original-tzname original-offset
                (not (= original-offset local-offset)))
           (format "%s (%s)" local-str (di:format-time dt original-tzname))
         local-str)))))

(defun di:format-date (dt)
  "Format the `icalendar-date' or `icalendar-date-time' DT for the diary.
If DT is a date-time, only the date part is considered.  The date is
formatted with `calendar-date-string' according to the pattern in
`diary-date-insertion-form'."
  (dlet ((calendar-date-display-form diary-date-insertion-form))
    (cl-typecase dt
      (ical:date (calendar-date-string dt t t))
      (ical:date-time (calendar-date-string (ical:date-time-to-date dt) t t)))))

(defun di:format-date/time-as-local (dt &optional original-tzname)
  "Format the `icalendar-date' or `icalendar-date-time' DT for the diary.

If DT is a plain date, only the date will be formatted.  If DT is a
date-time, both the date and the time will formatted, after translating
DT into a date and time into the system local time.

If specified, ORIGINAL-TZNAME should be a string naming the time zone
observance in which DT was originally encoded in the iCalendar data.  In
this case, the original clock time in DT will also be added in
parentheses, with date if necessary.  For example:
  2025/05/01 09:00 (08:00 GMT)
or
  2025/05/01 18:00 (2025/05/02 08:00 JST)"
  (let ((local-dt (ical:date/time-to-local dt)))
    (cl-typecase local-dt
      (ical:date (di:format-date local-dt))
      (ical:date-time
       (let ((date (di:format-date local-dt))
             (time (di:format-time local-dt))
             (orig-date (di:format-date dt))
             (orig-time (di:format-time dt original-tzname)))
         (if original-tzname
             (format "%s %s (%s)" date time
                     (if (equal date orig-date)
                         orig-time
                       (format "%s %s" orig-date orig-time)))
           (format "%s %s" date time)))))))

(defun di:format-time-range (start end &optional omit-start-date)
  "Format a time range for the diary.

START and END should be `icalendar-date-time' values where the date part
is the same.  (If they are not on the same date, nil is returned; use
`diary-icalendar-format-time-block-sexp' to make a diary S-exp for this
range instead.)

The date is only formatted once, and the time is formatted as a range, like:
  STARTDATE STARTTIME-ENDTIME
If OMIT-START-DATE is non-nil, STARTDATE will be omitted."
  (when (equal (ical:date/time-to-date start) (ical:date/time-to-date end))
    (format "%s%s-%s"
            (if omit-start-date ""
              (concat (di:format-date start) " "))
            (di:format-time-as-local start)
            (di:format-time-as-local end))))

(defun di:format-block-sexp (start end)
  "Format a `diary-block' diary S-expression between START and END.

START and END may be `icalendar-date' or `icalendar-date-time'
values.  If they are date-times, only the date parts will be considered.
Returns a string like \"%%(diary-block ...)\" with the arguments properly
ordered for the current value of `calendar-date-style'."
  (unless (cl-typep start 'ical:date)
    (setq start (ical:date-time-to-date start)))
  (unless (cl-typep end 'ical:date)
    (setq end (ical:date-time-to-date end)))
  (concat
   diary-sexp-entry-symbol
   (apply #'format "(diary-block %d %d %d %d %d %d)"
          (cl-case calendar-date-style
            ;; M/D/Y
            (american (list (calendar-extract-month start)
                            (calendar-extract-day start)
                            (calendar-extract-year start)
                            (calendar-extract-month end)
                            (calendar-extract-day end)
                            (calendar-extract-year end)))
            ;; D/M/Y
            (european (list (calendar-extract-day start)
                            (calendar-extract-month start)
                            (calendar-extract-year start)
                            (calendar-extract-day end)
                            (calendar-extract-month end)
                            (calendar-extract-year end)))
            ;; Y/M/D
            (iso      (list (calendar-extract-year start)
                            (calendar-extract-month start)
                            (calendar-extract-day start)
                            (calendar-extract-year end)
                            (calendar-extract-month end)
                            (calendar-extract-day end)))))))

(defun di:format-time-block-sexp (start end)
  "Format a `diary-time-block' diary S-expression for times between START and END."
  (concat
   diary-sexp-entry-symbol
   (format "(diary-time-block :start '%s :end '%s)" start end)))

(defun di:format-rrule-sexp (component)
  "Format the recurrence rule data in COMPONENT as a diary S-expression.

The returned string looks like \"%%(diary-rrule ...)\", and contains the
necessary data from COMPONENT for the calendar to compute recurrences of
the event."
  (ical:with-component component
      ((ical:dtstart :value dtstart)
       (ical:dtend :value dtend)
       (ical:duration :value duration)
       (ical:rrule :value rrule)
       (ical:rdate :all rdate-nodes)
       (ical:exdate :all exdate-nodes))
    (unless (or rrule rdate-nodes)
      (di:signal-import-error "No recurrence data in component"))
    (let ((exdates
           (mapcar #'ical:ast-node-value
                   (apply #'append
                          (mapcar #'ical:ast-node-value exdate-nodes))))
          (rdates
           (mapcar #'ical:ast-node-value
                   (apply #'append
                          (mapcar #'ical:ast-node-value rdate-nodes))))
          ;; N.B. we intentionally *don't* add any clock times to the
          ;; imported diary entry, since they could conflict with the
          ;; times generated by the recurrence rule, e.g. if the rule is
          ;; an 'HOURLY rule.  Instead we always specify the end time
          ;; (if any) via a duration, and take care of displaying the
          ;; correct clocks times after computing recurrences during
          ;; diary display (see `diary-rrule').
          (dur-value (cond (duration duration)
                           (dtend (unless (equal dtstart dtend)
                                    (ical:duration-between dtstart dtend)))
                           (t nil)))
          (arg-plist nil))

      (when exdates
        (setq arg-plist (plist-put arg-plist :exclude `(quote ,exdates))))
      (when rdates
        (setq arg-plist (plist-put arg-plist :include `(quote ,rdates))))
      (when dtstart
        (setq arg-plist (plist-put arg-plist :start `(quote ,dtstart))))
      (when dur-value
        (setq arg-plist (plist-put arg-plist :duration `(quote ,dur-value))))
      (when rrule
        ;; TODO: make this prettier to look at?
        (setq arg-plist (append (list :rule `(quote ,rrule)) arg-plist)))
      ;; TODO: timezones??

      (setq arg-plist (cons 'diary-rrule arg-plist))
      (string-trim ; removing trailing \n added by pp
       (concat diary-sexp-entry-symbol
               (with-output-to-string (pp arg-plist)))))))

;; This function puts all of the above together to format individual
;; iCalendar components as diary entries.  The final formatting is done
;; by the appropriate skeleton command for the component, or by
;; `di:-format-vevent-legacy' if the legacy format string variables from
;; icalendar.el are set.
(defun di:format-entry (component index &optional nonmarking)
  "Format an iCalendar component for the diary.

COMPONENT should be an `icalendar-vevent', `icalendar-vtodo', or
`icalendar-vjournal'.  INDEX should be an index into the calendar where
COMPONENT occurs, as returned by `icalendar-parse-and-index'.

Depending on the type of COMPONENT, the body will be formatted by one of:
`diary-icalendar-vevent-format-function'
`diary-icalendar-vtodo-format-function'
`diary-icalendar-vjournal-format-function'
which see.

The variable `ical-nonmarking' will be bound to the value of NONMARKING in
the relevant skeleton command.  If it is non-nil, the user requested the
entry to be nonmarking.

Returns a string containing the diary entry."
  (ical:with-component component
      ((ical:attach :all attach-nodes)
       (ical:attendee :all attendee-nodes)
       (ical:categories :all categories-nodes)
       (ical:class :value access)
       (ical:comment :all comment-nodes)
       (ical:completed :value completed-dt)
       (ical:contact :all contact-nodes)
       (ical:description :value description)
       ;; in `icalendar-vjournal', multiple `icalendar-description'
       ;; nodes are allowed:
       (ical:description :all description-nodes)
       (ical:dtend :first dtend-node :value dtend)
       (ical:dtstart :first dtstart-node :value dtstart)
       (ical:duration :value duration)
       (ical:due :first due-node :value due-dt)
       (ical:geo :value geo)
       (ical:location :value location)
       (ical:organizer :first organizer-node  ; for skeleton formatting
                       :value organizer-addr) ; for legacy formatting
       (ical:percent-complete :value percent-complete)
       (ical:priority :value priority)
       (ical:rrule :value rrule)
       (ical:rdate :all rdate-nodes)
       (ical:status :value status)
       (ical:summary :value summary)
       (ical:transp :value transp)
       (ical:uid :value uid)
       (ical:url :value url))
    (let* ((is-recurring (or rdate-nodes rrule))
           (start-tz (when dtstart-node
                       (ical:with-property dtstart-node
                         ((ical:tzidparam :value tzid))
                         (when tzid (ical:index-get index :tzid tzid)))))
           (start-tzname (when start-tz (icr:tzname-on dtstart start-tz)))
           (dtstart-local (ical:date/time-to-local dtstart))
           (due-tz (when due-node
                     (ical:with-property due-node
                       ((ical:tzidparam :value tzid))
                       (when tzid (ical:index-get index :tzid tzid)))))
           (due-tzname (when due-tz (icr:tzname-on due-dt due-tz)))
           (dtend
            (cond (dtend dtend)
                  ;; DTEND and DUE never occur in the same component,
                  ;; so we alias dtend to due:
                  (due-dt due-dt)
                  (duration
                   (ical:date/time-add-duration dtstart duration start-tz))))
           (dtend-local (ical:date/time-to-local dtend))
           (end-tz
            (cond (dtend-node
                   (ical:with-property dtend-node
                     ((ical:tzidparam :value tzid))
                     (when tzid (ical:index-get index :tzid tzid))))
                  (due-node due-tz)
                  (duration start-tz)))
           (end-tzname (when end-tz (icr:tzname-on dtend end-tz)))
           (component-type (ical:ast-node-type component)))
      (dlet
          ;; We use "ical-" rather than "icalendar-" as prefix for these
          ;; vars because (a) it's shorter and (b) to avoid shadowing
          ;; any library symbols:
          ((ical-attachments
            (when attach-nodes
              (di:save-attachments-from attach-nodes uid)))
           (ical-attendees (mapcar #'di:format-attendee attendee-nodes))
           (ical-categories
            (mapcan
             (lambda (node)
               (mapcar #'ical:text-to-string (ical:ast-node-value node)))
             categories-nodes))
           (ical-access (when access (downcase access)))
           (ical-comments
            (mapcar
             (lambda (node) (ical:text-to-string (ical:ast-node-value node)))
             comment-nodes))
           (ical-contacts
            (mapcar
             (lambda (node) (ical:text-to-string (ical:ast-node-value node)))
             contact-nodes))
           (ical-completed
            (when completed-dt (di:format-date/time-as-local completed-dt)))
           (ical-description
            (if (eq 'icalendar-vjournal component-type)
              (mapconcat
               (lambda (node)
                 (ical:trimp (ical:text-to-string (ical:ast-node-value node))))
               description-nodes
               "\n\n")
              (ical:trimp description)))
           (ical-start
            (when dtstart
              (if (bound-and-true-p ical-importing)
                  (di:format-date/time-as-local dtstart start-tzname)
                (di:format-time-as-local dtstart start-tzname))))
           (ical-end
            (when dtend
              (if (bound-and-true-p ical-importing)
                  (di:format-date/time-as-local dtend end-tzname)
                (di:format-time-as-local dtend end-tzname))))
           (ical-start-to-end
            (with-suppressed-warnings ((lexical date) (free-vars date))
              (cond
               ((not dtstart) nil)
               ((or (not dtend) (equal dtstart dtend))
                ;; without a distinct DTEND/DUE, same as start:
                (if (bound-and-true-p ical-importing)
                    (di:format-date/time-as-local dtstart start-tzname)
                  (di:format-time-as-local dtstart start-tzname)))
               ((and (bound-and-true-p ical-importing)
                     (cl-typep dtstart 'ical:date)
                     (cl-typep dtend 'ical:date))
                ;; Importing two dates:
                ;; %%(diary-block ...)
                (di:format-block-sexp
                 dtstart
                 ;; DTEND is an exclusive bound, while
                 ;; diary-block needs an inclusive bound, so
                 ;; subtract a day:
                 (ical:date-add dtend :day -1)))
               ((and (bound-and-true-p ical-importing)
                     (equal (ical:date/time-to-date dtstart-local)
                            (ical:date/time-to-date dtend-local)))
                ;; Importing, start and end times on same day:
                ;; DATE HH:MM-HH:MM
                (di:format-time-range dtstart-local dtend-local))
               ((bound-and-true-p ical-importing)
                ;; Importing at least one date-time, on different days:
                ;; %%(diary-time-block :start ... :end ...)
                (di:format-time-block-sexp dtstart-local dtend-local))
               ((and (boundp 'date) ; bound when displaying diary
                     (cl-typep dtstart-local 'ical:date-time)
                     (cl-typep dtend-local 'ical:date-time)
                     (equal date (ical:date-time-to-date dtstart-local))
                     (equal date (ical:date-time-to-date dtend-local)))
                ;; Displaying, start and end times on the day displayed:
                ;; HH:MM-HH:MM
                (di:format-time-range dtstart-local dtend-local t))
               ((and (boundp 'date) ; bound when displaying diary
                     (cl-typep dtstart-local 'ical:date-time)
                     (cl-typep dtend-local 'ical:date-time))
                ;; Displaying, start and/or end time on other days:
                ;; HH:MM-HH:MM for just the times on `date'
                (di:format-time-range
                 (ical:date/time-max dtstart-local
                                     (ical:make-date-time
                                      :year (calendar-extract-year date)
                                      :month (calendar-extract-month date)
                                      :day (calendar-extract-day date)
                                      :hour 0 :minute 0 :second 0
                                      :zone
                                      (decoded-time-zone dtstart-local)))
                 (ical:date/time-min dtend-local
                                     (ical:make-date-time
                                      :year (calendar-extract-year date)
                                      :month (calendar-extract-month date)
                                      :day (calendar-extract-day date)
                                      :hour 23 :minute 59 :second 59
                                      :zone
                                      (decoded-time-zone dtend-local)))))
               (t
                ;; That's all the cases we care about here.
                nil))))
           (ical-due
            (when (eq component-type 'ical:vtodo)
              (if due-node
                  (di:format-date/time-as-local due-dt due-tzname)
                ;; here we use start-tzname because due/dtend is calculated from
                ;; dtstart, not its own node with a tzid:
                (di:format-date/time-as-local dtend start-tzname))))
           (ical-work-time-sexp
            (when (and dtstart due-dt (bound-and-true-p ical-importing))
              (di:format-time-block-sexp dtstart-local due-dt)))
           (ical-importing (bound-and-true-p ical-importing))
           (ical-location (or (ical:trimp location)
                              (when geo (di:format-geo-coordinates geo))))
           (ical-nonmarking nonmarking)
           (ical-organizer (di:format-attendee organizer-node))
           (ical-percent-complete percent-complete)
           (ical-priority priority)
           (ical-rrule-sexp
            (when (and is-recurring (bound-and-true-p ical-importing))
              (di:format-rrule-sexp component)))
           (ical-status (when status (ical:trimp (downcase status))))
           (ical-summary (ical:trimp summary))
           (ical-transparency transp)
           (ical-uid (ical:trimp uid))
           (ical-url (ical:trimp url)))
        (with-temp-buffer
          (cl-case (ical:ast-node-type component)
            (ical:vevent
             (with-suppressed-warnings
                 ((obsolete ical:import-format
                            di:-use-legacy-vars-p
                            di:-vevent-to-legacy-alist
                            di:-format-vevent-legacy))
               ;; N.B. icalendar.el *only* imported VEVENT components
               (if (di:-use-legacy-vars-p)
                   (if (functionp ical:import-format)
                       (insert (funcall ical:import-format
                                        (di:-vevent-to-legacy-alist component)))
                     (di:-format-vevent-legacy (or ical-rrule-sexp
                                                   ical-start-to-end
                                                   ical-start)
                                               ical-access ical-description
                                               ical-location organizer-addr
                                               ical-summary ical-status
                                               ical-url ical-uid))
                 (funcall di:vevent-format-function component))))
            (ical:vtodo (funcall di:vtodo-format-function component))
            (ical:vjournal (funcall di:vjournal-format-function component)))
          (buffer-string))))))


;; Import to Diary
;;
;; `di:import-file' and `di:import-buffer' are the main user commands
;; for import.  (These replace `icalendar-import-file' and
;; `icalendar-import-buffer' defined by icalendar.el, which are now
;; obsolete aliases to these commands.) `di:import-buffer-to-buffer' is
;; the function underlying these commands; it is the main import
;; function available for external Lisp code.

;; `di:import-buffer-to-buffer' is the underlying function that formats
;; a complete `icalendar-vcalendar' as diary entries.  This function runs
;; `di:post-entry-format-hook' after formatting each component as an
;; entry, and it runs `di:post-calendar-format-hook' after all entries
;; have been formatted.  These hooks enable e.g. user review and
;; confirmation of each imported entry and of the whole imported
;; calendar.
(defvar di:post-entry-format-hook nil
  "Hook run after formatting a single iCalendar component as a diary entry.

The functions in this hook are run by `diary-icalendar-import-buffer-to-buffer'
\(which see) after each component it formats.  Each function will be
called in a (narrowed) buffer whose contents represent a single diary
entry.")

(defvar di:post-calendar-format-hook nil
  "Hook run after formatting a complete `icalendar-vcalendar' as diary entries.

The functions in this hook are run by `diary-icalendar-import-buffer-to-buffer'
\(which see) after formatting all the diary entries created from the
calendar.  Each function will be called in a buffer containing all the
diary entries.")

(defun di:sort-by-start-ascending (c1 c2)
  "Sort iCalendar component C1 before C2 if C1 starts strictly before C2.
Components with no start date/time are sorted after components that do."
  (let ((c1start (ical:with-property-of c1 'ical:dtstart nil value))
        (c2start (ical:with-property-of c2 'ical:dtstart nil value)))
    (cond ((and c1start c2start)
           (ical:date/time< c1start c2start))
          ;; order anything with a start before anything without:
          (c1start t)
          (c2start nil)
          ;; otherwise they can stay as-is:
          (t t))))

(defcustom di:import-comparison-function #'di:sort-by-start-ascending
  "Comparison function for sorting imported iCalendar components.
See the :lessp argument of `sort' for more information."
  :version "31.1"
  :type '(radio (function-item di:sort-by-start-ascending)
                (function :tag "Other comparison function")))

(defun di:import-buffer-to-buffer (&optional all-nonmarking)
  "Format iCalendar data in current buffer as diary entries.

This function parses the first iCalendar VCALENDAR in the current buffer
and formats its VEVENT, VJOURNAL, and VTODO components as diary entries.
It returns a new buffer containing those diary entries.  The caller
should kill this buffer when it is no longer needed.

If ALL-NONMARKING is non-nil, all diary entries will be non-marking.

The list of components to import can be filtered by binding
`diary-icalendar-import-predicate'.  After each component is formatted as
a diary entry, `diary-icalendar-post-entry-format-hook' is run in a (narrowed)
buffer containing that entry.  After all components have been formatted,
`diary-icalendar-post-calendar-format-hook' is run in the (widened) buffer
containing all the entries.

The formatting of imported entries depends on a number of
user-customizable variables, including: `diary-date-forms',
`calendar-date-style', `calendar-date-display-form' and customizations
in the `diary-icalendar' group."
  (unless (ical:contains-vcalendar-p (current-buffer))
    (di:signal-import-error (format "No VCALENDAR object in buffer %s"
                                    (buffer-name))))
  (save-excursion
    (goto-char (point-min))
    (let (vcalendar index)
      (ical:init-error-buffer)
      (let ((vcal/idx (ical:parse-and-index (current-buffer))))
        (when vcal/idx
          (setq vcalendar (car vcal/idx))
          (setq index (cadr vcal/idx))
          (let* ((import-buf (generate-new-buffer " *diary-import*"))
                 (to-import
                  (sort
                   (seq-filter
                    (lambda (c)
                      (and (or (ical:vevent-component-p c)
                               (ical:vjournal-component-p c)
                               (ical:vtodo-component-p c))
                           (funcall di:import-predicate c)))
                    (ical:ast-node-children vcalendar))
                  :lessp di:import-comparison-function
                  :in-place t))
                 ;; prevent point from being reset from window-point
                 ;; when narrowed buffer is displayed for confirmation:
                 (window-point-insertion-type t)
                 ;; position at start of each entry:
                 entry-start)

            (with-current-buffer import-buf
              (dlet ((ical-importing t)) ; inform skeletons we're importing
                (dolist (component to-import)
                  (setq entry-start (point))
                  (insert (di:format-entry component index all-nonmarking))
                  (with-restriction entry-start (point)
                    (save-excursion
                      (run-hooks 'di:post-entry-format-hook)))
                  (unless (bolp) (insert "\n"))))
              (save-excursion
                (run-hooks 'di:post-calendar-format-hook))
              import-buf)))))))

;; Internal variables needed by `di:-entry-import'.  They are dynamically
;; bound in `di:import-buffer'.
(defvar di:-no-queries nil)
(defvar di:-entry-count nil)

(defun di:-entry-import ()
  ;; Adds the formatted entry in the current restriction to the diary,
  ;; after getting confirmation from the user.
  ;; Used via `di:post-entry-format-hook' in `di:import-buffer', below.
  (unless di:-no-queries
    (display-buffer (current-buffer)))
  (when (or di:-no-queries
            (let ((help-form
                   "Type y to add this entry to the diary, n to skip to next."))
              (di:y-or-n-or-edit-p "Add this entry to the diary?")))
    (ical:condition-case err
       (let* ((uid (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward di:uid-regexp nil t)
                       (match-string 1))))
              (other-entry (di:find-entry-with-uid uid))
              (entry (buffer-string)))
         (if (and other-entry
                  (not di:-no-queries)
                  (y-or-n-p "Replace existing entry with same UID?"))
             (with-current-buffer (marker-buffer (car other-entry))
               (replace-region-contents
                (car other-entry) (cadr other-entry) entry))
           ;; Otherwise, diary-make-entry inserts the new entry at the end
           ;; of the main diary file:
           (diary-make-entry
            entry
            nil ; skeleton has already interpreted nonmarking
            nil ; use dynamic value of `diary-file'
            t   ; skeleton responsible for final spaces
            t))  ; no need to show diary file while importing
         (when other-entry
           (set-marker (car other-entry) nil)
           (set-marker (cadr other-entry) nil))
         (cl-incf di:-entry-count)))))

;;;###autoload
(defun di:import-buffer (&optional diary-filename quietly all-nonmarking)
  "Import iCalendar events from current buffer into diary.

This function parses the first iCalendar VCALENDAR in the current buffer
and imports VEVENT, VJOURNAL, and VTODO components to the diary file
DIARY-FILENAME (default: `diary-file').

For each entry, you are asked whether to add it to the diary unless
QUIETLY is non-nil.  After all entries are imported, you are also asked
if you want to save the diary file unless QUIETLY is non-nil.  When
called interactively, you are asked if you want to confirm each entry
individually; answer No to make QUIETLY non-nil.

ALL-NONMARKING determines whether all diary events are created as
non-marking entries.  When called interactively, you are asked whether
you want to make all entries non-marking.

The formatting of imported entries in the diary depends on a number of
user-customizable variables.  Before running this command for the first
time, you may especially wish to check the values of:
`diary-file'
`diary-date-forms'
`diary-date-insertion-form'
`calendar-date-style'
`calendar-date-display-form'
as well as variables in the customization group `diary-icalendar-import'."
  (interactive
   (list (read-file-name "Diary file: "
                         (when diary-file (file-name-directory diary-file))
                         (cons diary-file diary-included-files))
         (or di:always-import-quietly
             (not (y-or-n-p "Confirm entries individually?")))
         (y-or-n-p "Make all entries non-marking?")))

  (let* ((diary-file diary-filename) ; dynamically bound for `di:-entry-import',
         (di:-entry-count 0)         ; see above
         (di:-no-queries quietly)    ;
         (di:post-entry-format-hook
          (append di:post-entry-format-hook (list #'di:-entry-import)))
         (diary-buffer (or (find-buffer-visiting diary-filename)
                           (find-file-noselect diary-filename)))
         import-buffer)
    (unwind-protect
        (setq import-buffer (di:import-buffer-to-buffer all-nonmarking))
      (when (bufferp import-buffer)
        (kill-buffer import-buffer)))
    (display-buffer diary-buffer)
    (when (or quietly
              (y-or-n-p (format "%d entries imported.  Save diary file?"
                                di:-entry-count)))
      (with-current-buffer diary-buffer
        (goto-char (point-max))
        (save-buffer)))))

;;;###autoload
(defun di:import-file (filename &optional diary-filename quietly nonmarking)
  "Import iCalendar diary entries from FILENAME into DIARY-FILENAME.

This function parses the first iCalendar VCALENDAR in FILENAME and
imports VEVENT, VJOURNAL, and VTODO components to the diary
DIARY-FILENAME (default: `diary-file').

For each entry, you are asked whether to add it to the diary unless
QUIETLY is non-nil.  After all entries are imported, you are also asked
if you want to save the diary file unless QUIETLY is non-nil.  When
called interactively, you are asked if you want to confirm each entry
individually; answer No to make QUIETLY non-nil.

NONMARKING determines whether all diary events are created as
non-marking entries.  When called interactively, you are asked whether
you want to make all entries non-marking.

The formatting of imported entries in the diary depends on a number of
user-customizable variables.  Before running this command for the first
time, you may especially wish to check the values of:
`diary-file'
`diary-date-forms'
`diary-date-insertion-form'
`calendar-date-style'
`calendar-date-display-form'
as well as variables in the customization group `diary-icalendar-import'."
  (interactive
   (list (read-file-name "iCalendar file: " nil nil 'confirm)
         (read-file-name "Diary file: "
                         (when diary-file (file-name-directory diary-file))
                         (cons diary-file diary-included-files))
         (or di:always-import-quietly
             (not (y-or-n-p "Confirm entries individually?")))
         (y-or-n-p "Make all entries non-marking?")))
  (let ((parse-buf (ical:find-unfolded-buffer-visiting filename)))
    (unless parse-buf
      (ical:condition-case err
        (setq parse-buf
              (ical:unfolded-buffer-from-file (expand-file-name filename)))))
    ;; Hand off to `di:import-buffer' for the actual import:
    (if parse-buf
        (with-current-buffer parse-buf
          (di:import-buffer diary-filename quietly nonmarking))
      ;; If we get here, we weren't able to open the file for parsing:
      (warn "Unable to open file %s; see %s"
            filename (buffer-name (ical:error-buffer))))))

;; Some simple support for viewing iCalendar data in MIME message
;; parts.  Mail readers may want to build their own viewer using the
;; import functions above, but this is a good starting point:
(defun di:mailcap-viewer ()
  "View iCalendar data in the current message part as diary entries.

This function is a suitable viewer for text/calendar parts in MIME
messages, such as email attachments.  To use this function as a viewer,
customize the variable `mailcap-user-mime-data' and add an entry
containing this function for the MIME type \"text/calendar\".

To extend the behavior of this function, see
`diary-icalendar-after-mailcap-viewer-hook'."
  (let ((entries-buf (diary-icalendar-import-buffer-to-buffer)))
    (unwind-protect
        (progn
          ;; Since this is already a temporary viewer buffer, we replace
          ;; its contents with the imported entries, so we can (a) keep
          ;; the window configuration setup by the calling mailcap code
          ;; and (b) already kill the import buffer here.
          (erase-buffer)
          (insert-buffer-substring entries-buf)
          (diary-mode)
          (run-hooks di:after-mailcap-viewer-hook))
      (kill-buffer entries-buf))))


;; Export

;;; Error handling
(define-error 'ical:diary-export-error "Unable to export diary data" 'ical:error)

(cl-defun di:signal-export-error (msg &key (diary-buffer (current-buffer))
                                           (position (point))
                                           line
                                           (severity 2))
  (let ((err-data
          (list :message msg
                :buffer diary-buffer
                :position position
                :line line
                :severity severity)))
    (signal 'ical:diary-export-error err-data)))

;;; Export utility functions
(defun di:parse-attendees-and-organizer ()
  "Parse `icalendar-attendee' and `icalendar-organizer' nodes from entry.

Searches the entry in the current restriction for addresses matching
`diary-icalendar-address-regexp'.  If an address is found on a
line that also matches `diary-icalendar-organizer-regexp', it will be
parsed as an `icalendar-organizer' node, or otherwise as an
`icalendar-attendee'.  Returns the list of nodes for all addresses found."
  (goto-char (point-min))
  (let (attendees organizer)
    (while (re-search-forward di:address-regexp nil t)
      (let ((addr (match-string 1))
            (cn (match-string 2)))
        (unless (string-match ":" addr) ; URI scheme already present
          (setq addr (concat "mailto:" addr)))
        (when cn
          (setq cn (ical:trimp cn)))
        (if (string-match di:organizer-regexp
                          (buffer-substring (line-beginning-position)
                                            (line-end-position)))
            (setq organizer
                  (ical:make-property ical:organizer addr (ical:cnparam cn)))
          (push (ical:make-property ical:attendee addr (ical:cnparam cn))
                attendees))))
    (if organizer
        (cons organizer attendees)
      attendees)))

(defun di:parse-location ()
  "Parse `icalendar-location' node from entry.

Searches the entry in the current restriction for a location matching
`diary-icalendar-location-regexp'.  If a location is found, it will be
parsed as an `icalendar-location' node.  Returns a list containing just
this node, or nil."
  (goto-char (point-min))
  (when (and di:location-regexp
             (re-search-forward di:location-regexp nil t))
    (ical:make-property ical:location (ical:trimp (match-string 1)))))

(defun di:parse-class ()
  "Parse `icalendar-class' node from entry.

Searches the entry in the current restriction for an access
classification matching `diary-icalendar-class-regexp'.  If a
classification is found, it will be parsed as an `icalendar-class'
node.  Return this node, or nil."
  (goto-char (point-min))
  (when (and di:class-regexp
             (re-search-forward di:class-regexp nil t))
    (ical:make-property ical:class
        (upcase (string-trim (match-string 1))))))

(defun di:parse-status ()
  "Parse `icalendar-status' node from entry.

Searches the entry in the current restriction for a status matching
`diary-icalendar-status-regexp'.  If a status is found, it will be parsed
as an `icalendar-status' node.  Return this node, or nil."
  (goto-char (point-min))
  (when (and di:status-regexp
             (re-search-forward di:status-regexp nil t))
    (ical:make-property ical:status
        (upcase (string-trim (match-string 1))))))

(defun di:parse-url ()
  "Parse `icalendar-url' node from entry.

Searches the entry in the current restriction for an URL matching
`diary-icalendar-url-regexp'.  If an URL is found, it will be parsed as an
`icalendar-url' node.  Return this node, or nil."
  (goto-char (point-min))
  (when (and di:url-regexp
             (re-search-forward di:url-regexp nil t))
    (ical:make-property ical:url (ical:trimp (match-string 1)))))

(defun di:parse-uid ()
  "Parse `icalendar-uid' node from entry.

Searches the entry in the current restriction for a UID matching
`diary-icalendar-uid-regexp'.  If a UID is found, it will be parsed as an
`icalendar-uid' node.  Return this node, or nil."
  (goto-char (point-min))
  (when (and di:uid-regexp
             (re-search-forward di:uid-regexp nil t))
    (ical:make-property ical:uid (ical:trimp (match-string 1)))))

(defun di:parse-summary-and-description ()
  "Parse summary and description nodes from current restriction.

When `diary-icalendar-summary-regexp' or
`diary-icalendar-description-regexp' are non-nil, and the entry matches
them, the matches will be used to generate the summary and description.

Otherwise, the first line of the entry (after any nonmarking symbol and
date and time specification) is used as the summary.  The description is
the full body of the entry, excluding the nonmarking symbol, date and
time, but including the summary.

Returns a list containing an `icalendar-summary' node and
`icalendar-description' node, or nil."
  (goto-char (point-min))
  (let (summary description)
    (when (and di:summary-regexp
               (re-search-forward di:summary-regexp nil t))
      (setq summary (match-string 1)))
    (goto-char (point-min))
    (when (and di:description-regexp
               (re-search-forward di:description-regexp nil t))
      (setq description (match-string 1)))
    ;; Fall back to using first line and entire entry:
    (goto-char (point-min))
    (while (looking-at-p "[[:space:]]")
      (forward-char))
    (unless summary
      (setq summary (buffer-substring (point) (line-end-position))))
    (unless description
      (setq description (buffer-substring (point) (point-max))))
    ;; Remove any indentation on subsequent lines from description:
    (setq description (replace-regexp-in-string "^[[:space:]]+" "" description))

    (list (ical:make-property ical:summary summary)
          (ical:make-property ical:description description))))

(defun di:parse-entry-type ()
  "Return the type symbol for the component type used to export an entry.

Default is `icalendar-vevent'.  If the entry is nonmarking and
`diary-icalendar-export-nonmarking-as-vjournal' is non-nil,
`icalendar-vjournal' is returned.  If `diary-icalendar-todo-regexp' is
non-nil and the entry matches it, `icalendar-vtodo' is returned.

If the entry is nonmarking and `diary-icalendar-export-nonmarking-entries'
is nil, nil is returned, indicating that the entry should not be
exported."
  (let (type)
    (goto-char (point-min))
    (unless (and (looking-at-p diary-nonmarking-symbol)
                 (not di:export-nonmarking-entries))
      (setq type 'ical:vevent)
      (when (and (looking-at-p diary-nonmarking-symbol)
                 di:export-nonmarking-as-vjournal)
        (setq type 'ical:vjournal))
      (when (and di:todo-regexp (re-search-forward di:todo-regexp nil t))
        (setq type 'ical:vtodo)))
    type))

(defun di:parse-transparency (type)
  "Return the iCalendar time transparency of an entry.

TYPE should be the type symbol for the component to be exported, as
returned by `diary-icalendar-parse-entry-type'.  If the entry is
non-marking (i.e., begins with `diary-nonmarking-symbol'), and it is to
be exported as an `icalendar-vevent' (according to TYPE), then this
function returns a list containing the appropriate `icalendar-transp'
property node to mark the event as transparent, and moves the current
restriction past the non-marking symbol.  Otherwise it returns nil."
  (save-excursion
    (goto-char (point-min))
    (when (and (eq type 'ical:vevent)
               (re-search-forward (concat "^" diary-nonmarking-symbol)
                                  (line-end-position) t))
      (narrow-to-region (point) (point-max))
      (list
       (ical:make-property ical:transp "TRANSPARENT")))))

;; TODO: move to diary-lib?
(defun di:parse-date-form ()
  "Parse a date matching `diary-date-forms' on the current line.

If a date is found, moves the current restriction past the end of the
date and returns a list (MONTH DAY YEAR), where each value is an integer
or t if the date is generic in that unit.  Otherwise returns nil."
  (goto-char (point-min))
  (catch 'date
    (let (date-regexp backup)
      (dolist (date-sexp diary-date-forms)
        (when (eq 'backup (car date-sexp))
          (setq date-sexp (cdr date-sexp))
          (setq backup t))
        (setq date-regexp (di:date-form-to-regexp date-sexp))
        (when backup (beginning-of-line))
        (when (let ((case-fold-search t))
                (re-search-forward date-regexp nil t))
          (let ((year
                 (let ((match (match-string 1)))
                   (if (or (null match) (equal match "*"))
                       t
                     (if (and diary-abbreviated-year-flag (length= match 2))
                         ;; from diary-lib.el:
                         ;; Add 2-digit year to current century.
                         ;; If more than 50 years in the future,
                         ;; assume last century.  If more than 50
                         ;; years in the past, assume next century.
                         (let* ((current-y
                                 (calendar-extract-year (calendar-current-date)))
                                (y (+ (string-to-number match)
                                      ;; Current century, eg 2000.
                                      (* 100 (/ current-y 100))))
                                (offset (- y current-y)))
                           (cond ((> offset 50)
                                  (- y 100))
                                 ((< offset -50)
                                  (+ y 100))
                                 (t y)))
                       (string-to-number match)))))
                (month
                 (let ((month-num (match-string 2))
                       (month-name (match-string 4)))
                   (cond ((or (equal month-name "*") (equal month-num "*")) t)
                         (month-num (string-to-number month-num))
                         (month-name
                          (alist-get
                           (capitalize month-name)
                           (calendar-make-alist
                            calendar-month-name-array
                            1 nil
                            calendar-month-abbrev-array
                            (mapcar (lambda (e) (format "%s." e))
                                    calendar-month-abbrev-array))
                           nil nil #'equal)))))
                (day
                 (let ((day-num (match-string 3))
                       (day-name (match-string 5)))
                   (cond
                    ;; We don't care about the day name here, unless it
                    ;; is "*", since it won't help us identify a day of
                    ;; the month.  Weekly entries under a weekday name
                    ;; are parsed by `di:parse-weekday-name', below.
                    ((or (equal day-name "*") (equal day-num "*")) t)
                    (day-num (string-to-number day-num))))))
            (when (and year month day)
              (narrow-to-region (match-end 0) (point-max))
              (throw 'date (list month day year)))))))))

(defun di:date-form-to-regexp (date-sexp)
  "Convert DATE-SEXP to a regular expression.

DATE-SEXP should be an S-expression in the variables `year', `month',
`day', `monthname', and `dayname', as found e.g. in `diary-date-forms'.
The returned regular expression matches dates of this form, including
generic dates specified with \"*\", and abbreviated and long-form month
and day names (based on `calendar-month-name-array' and
`calendar-month-abbrev-array', and similarly for day names).  The match
groups contain the following data:

Group 1: the 2-4 digit year, or a literal *
Group 2: the 1-2 digit month number, or a literal *
Group 3: the 1-2 digit day number, or a literal *
Group 4: the (long-form or abbreviated) month name, or a literal *
Group 5: the (long-form or abbreviated) day name, or a literal *"
  (when (eq 'backup (car date-sexp))
    (setq date-sexp (cdr date-sexp)))
  (let ((month-names-regexp
         (rx
          (group-n 4
            (or (regexp (diary-name-pattern calendar-month-name-array
                                            calendar-month-abbrev-array))
                "*"))))
        (day-names-regexp
         (rx
          (group-n 5
            (or (regexp (diary-name-pattern calendar-day-name-array
                                            calendar-day-abbrev-array))
                "*"))))
          date-regexp)
    (calendar-dlet
        ((prefix (rx line-start
                     (zero-or-one (regexp diary-nonmarking-symbol))))
         (year (rx (group-n 1 (or (** 2 4 digit) "*"))))
         (month (rx (group-n 2 (or (** 1 2 digit) "*"))))
         (day (rx (group-n 3 (or (** 1 2 digit) "*"))))
         (monthname month-names-regexp)
         (dayname day-names-regexp))
      (setq date-regexp (apply #'concat (cons prefix (mapcar #'eval date-sexp)))))
  date-regexp))

(defun di:parse-weekday-name ()
  "Parse a weekday name on the current line.

The day name must appear in `calendar-day-name-array' or
`calendar-day-abbrev-array'.  If a day name is found, move the current
restriction past it, and return a day number between 0 (=Sunday) and
6 (=Saturday).  Otherwise, return nil."
  (goto-char (point-min))
  (let ((day-names-regexp
         (rx line-start
             (zero-or-one (regexp diary-nonmarking-symbol))
             (group-n 1
               (regexp (diary-name-pattern calendar-day-name-array
                                           calendar-day-abbrev-array))))))
    (when (re-search-forward day-names-regexp (line-end-position) t)
      (let ((day-name (capitalize (match-string 1))))
        (narrow-to-region (match-end 0) (point-max))
        (alist-get
         day-name
         (calendar-make-alist calendar-day-name-array 0 nil
                              calendar-day-abbrev-array
                              (mapcar (lambda (e) (format "%s." e))
                                      calendar-day-abbrev-array))
         nil nil #'equal)))))

(defun di:weekday-to-recurrence (weekday)
  "Convert WEEKDAY to a WEEKLY iCalendar recurrence rule.

WEEKDAY must be an integer between 0 (=Sunday) and 6 (=Saturday).
Returns a list (START RRULE), with START being an `icalendar-dtstart'
property and RRULE an `icalendar-rrule'."
  (let ((dtstart (calendar-nth-named-day 1 weekday 1 di:recurring-start-year))
        (rrule `((FREQ WEEKLY)
                 (BYDAY (,weekday)))))
    (list (ical:make-property ical:dtstart dtstart)
          (ical:make-property ical:rrule rrule))))

;; TODO: give this value to diary-time-regexp?
(defconst di:time-regexp
  (rx-let ((hours (or (seq (any "0-2") (any "0-9"))
                      (any "0-9")))
           (minutes (seq (any "0-5") (any "0-9")))
           (am/pm (seq (any "ap") "m"))) ;; am, pm
    (rx
     (group-n 1 ;; START
       (group-n 11 hours) ;; start hour
       (or
        ;; 10:00 or 10h00:
        (seq (or ":" "h") (group-n 12 minutes) (opt (group-n 13 am/pm)))
        ;; 10.00h or 10.00am: (a bare "10.00" should not match)
        (seq "." (group-n 12 minutes) (or (group-n 13 am/pm) "h"))
        ;; 10am
        (group-n 13 am/pm)
        ;; 10h
        "h"))
     (zero-or-one
      (one-or-more "-")
      (group-n 2 ;; END
        (group-n 21 hours) ;; end hour
        (or
         ;; 10:00 or 10h00:
         (seq (or ":" "h") (group-n 22 minutes) (opt (group-n 23 am/pm)))
         ;; 10.00h or 10.00am:
         (seq "." (group-n 22 minutes) (or "h" (group-n 23 am/pm)))
         ;; 10am
         (group-n 23 am/pm)
         ;; 10h
         "h")))
     (one-or-more space)))
  "Regular expression to match diary appointment times.

Accepted time formats look like e.g.:
  9AM 9:00 09:00 9h 9h00 9.00am 9.00h
  9PM 9:00pm 21:00 21h00 21.00pm 21.00h
  9AM-1PM 09:00-13:00

Group 1 matches the start time:
  Group 11 matches the hours digits
  Group 12 matches the minutes digits
  Group 13 matches an AM/PM specification

Group 2 matches the end time:
  Group 21 matches the hours digits
  Group 22 matches the minutes digits
  Group 23 matches an AM/PM specification")

(defun di:parse-time ()
  "Parse diary time string in the current restriction.

If a time specification is found, move the current restriction past it,
and return a list (START END), where START and END are decoded-time
values containing the hours and minutes slots parsed from the time
specification.  END may be nil if no end time was specified."
  (goto-char (point-min))
  (let ((regexp di:time-regexp)
        (case-fold-search t))
    (when di:export-linewise
      ;; In this case, only look for a time following whitespace,
      ;; at the beginning of a continuation line of the full entry:
      (setq regexp (concat "^[[:space:]]+" di:time-regexp)))

    (when (re-search-forward regexp (line-end-position) t)
      (let* ((start-hh (string-to-number (match-string 11)))
             (start-am/pm (when (match-string 13)
                            (upcase (match-string 13))))
             (start-hours (if (and (equal start-am/pm "PM") (< start-hh 12))
                              (+ 12 start-hh)
                            start-hh))
             (start-minutes (string-to-number (or (match-string 12) "0")))
             (start
              (when (and start-hours start-minutes)
                (make-decoded-time :hour start-hours
                                   :minute start-minutes
                                   :second 0)))
             (end-hh (when (match-string 21)
                       (string-to-number (match-string 21))))
             (end-am/pm (when (match-string 23)
                          (upcase (match-string 23))))
             (end-hours (if (and end-hh (equal end-am/pm "PM") (< end-hh 12))
                            (+ 12 end-hh)
                          end-hh))
             (end-minutes (when end-hours
                            (string-to-number (or (match-string 22) "0"))))
             (end (when (and end-hours end-minutes)
                    (make-decoded-time :hour end-hours
                                       :minute end-minutes
                                       :second 0))))
        (narrow-to-region (match-end 0) (point-max))
        ;; Return the times:
        (list start end)))))

(defun di:-tz-is-utc-p ()
  "Return non-nil if the current time zone is UTC."
  (equal (current-time-zone) '(0 "UTC")))

(defun di:convert-time-via-strategy (dt &optional vtimezone)
  "Reinterpret the local time DT per the time zone export strategy.

The export strategy is determined by
`diary-icalendar-time-zone-export-strategy', which see.

DT may be an `icalendar-date' or `icalendar-date-time'.  If it is a
date, it is returned unmodified.  If it is a date-time, depending on the
strategy and any existing zone information in DT, it will be converted
to a correct local, UTC, or floating time.  VTIMEZONE should be the
`icalendar-vtimezone' which defines the local time zone, if the time
zone export strategy requires it."
  (cl-typecase dt
    (ical:date dt)
    (ical:date-time
     (cond
       ((or (and (eq 'local di:time-zone-export-strategy)
                 (not (di:-tz-is-utc-p)))
            (listp di:time-zone-export-strategy))
        (unless (ical:vtimezone-component-p vtimezone)
          (di:signal-export-error
           (format
            "%s time export strategy requires a time zone definition;\n%s"
            (if (eq 'local di:time-zone-export-strategy) "`local'" "list-based")
            (concat
             "check the value of `diary-icalendar-time-zone-export-strategy'\n"
             "and the output of `calendar-current-time-zone'"))))
        (if (decoded-time-zone dt)
            (icr:tz-decode-time (encode-time dt) vtimezone)
          (icr:tz-set-zone dt vtimezone :error)))
       ((or (eq 'to-utc di:time-zone-export-strategy)
            (di:-tz-is-utc-p)) ; we're already in UTC, so mark dt as such
        (decode-time (encode-time dt) t))
       ((eq 'floating di:time-zone-export-strategy)
        (setf (decoded-time-zone dt) nil)
        dt)))))

(defun di:parse-sexp ()
  "Parse a diary S-expression at the beginning of the current restriction.

The S-expression must appear at the start of line, immediately after
`diary-sexp-entry-symbol'.  If an S-expression is found, move the
current restriction past it, and return the S-expression.  Otherwise,
return nil."
  (goto-char (point-min))
  (let ((regexp (rx line-start
                    (regexp diary-sexp-entry-symbol))))
    (when (re-search-forward regexp (line-end-position) t)
      (let ((sexp (read (current-buffer))))
        (narrow-to-region (point) (point-max))
        sexp))))

(defun di:anniversary-sexp-to-recurrence (sexp)
  "Convert `diary-anniversary' SEXP to `icalendar-dtstart' and `icalendar-rrule'.
Returns a pair of nodes (START RRULE)."
  (let* ((d1 (nth 1 sexp))
         (d2 (nth 2 sexp))
         (d3 (nth 3 sexp))
         (dtstart (diary-make-date d1 d2 (or d3 di:recurring-start-year)))
         (rrule '((FREQ YEARLY))))
    (list
     (ical:make-property ical:dtstart dtstart (ical:valuetypeparam 'ical:date))
     (ical:make-property ical:rrule rrule))))

(defun di:block-sexp-to-recurrence (sexp)
  "Convert `diary-block' SEXP to `icalendar-dtstart' and `icalendar-rrule' nodes.
Returns a pair of nodes (START RRULE)."
  (let* ((dtstart (diary-make-date (nth 1 sexp) (nth 2 sexp) (nth 3 sexp)))
         (end (diary-make-date (nth 4 sexp) (nth 5 sexp) (nth 6 sexp)))
         (rrule `((FREQ DAILY)
                  (UNTIL ,end))))
    (list (ical:make-property ical:dtstart dtstart
            (ical:valuetypeparam 'ical:date))
          (ical:make-property ical:rrule rrule))))

(defun di:time-block-sexp-to-start-end (sexp &optional vtimezone)
  "Convert `diary-time-block' SEXP to `icalendar-dtstart' and `icalendar-dtend'.
Returns a pair of nodes (START END).

VTIMEZONE, if specified, should be an `icalendar-vtimezone'.  Times in
SEXP will be reinterpreted as local to VTIMEZONE, as UTC, or as floating
times according to `diary-icalendar-time-zone-export-strategy'."
  (let* ((start (plist-get sexp :start))
         (dtstart (di:convert-time-via-strategy start vtimezone))
         (end (plist-get sexp :end))
         (dtend (di:convert-time-via-strategy end vtimezone))
         (tzid (ical:with-property-of vtimezone 'ical:tzid)))
    (list (ical:make-property ical:dtstart dtstart (ical:tzidparam tzid))
          (ical:make-property ical:dtend dtend (ical:tzidparam tzid)))))

(defun di:cyclic-sexp-to-recurrence (sexp)
  "Convert `diary-cyclic' SEXP to `icalendar-dtstart' and `icalendar-rrule'.
Returns a pair of nodes (START RRULE)."
  (let* ((ndays (nth 1 sexp))
         (dtstart (diary-make-date (nth 2 sexp) (nth 3 sexp) (nth 4 sexp)))
         (rrule `((FREQ DAILY)
                  (INTERVAL ,ndays))))
    (list
     (ical:make-property ical:dtstart dtstart (ical:valuetypeparam 'ical:date))
     (ical:make-property ical:rrule rrule))))

(defun di:float-sexp-to-recurrence (sexp)
  "Convert `diary-float' SEXP to `icalendar-dtstart' and `icalendar-rrule'.
Returns a pair of nodes (START RRULE)."
  (let* ((month-exp (nth 1 sexp))
         (months (cond ((eq month-exp t) nil) ; don't add a BYMONTH clause
                       ((integerp month-exp) (list month-exp))
                       ((and (listp month-exp) (eq 'quote (car month-exp)))
                        (eval month-exp nil)) ; unquote a literal list of ints
                       (t month-exp)))
         (_ (unless (seq-every-p #'integerp months)
              (di:signal-export-error
               (format "Malformed month(s) in `diary-float' S-expression:\n%s"
                       sexp))))
         (dow (nth 2 sexp))
         (n (nth 3 sexp))
         (day (or (nth 4 sexp)
                  (if (< 0 n) 1
                    'last))) ; = "last day of the month" for any month
         ;; Calculate the offset within the month from day, n:
         (offset
          (cond ((eq day 'last) n)
                ((and (< 0 day) (< 0 n))
                 ;; In this case, to get the offset relative to
                 ;; the start of the month, we need to add to n
                 ;; the number of weeks in the month before day:
                 ;; e.g. if day = 8, n = 2, then we are looking
                 ;; for the second DOW after the 8th of the
                 ;; month, which is the 3rd DOW after the 1st of
                 ;; the month
                 (+ n (/ (1- day) 7)))
                ((and (< 0 day) (< n 0) (< day (* 7 (abs n))))
                 ;; In this case, we need to cross into the
                 ;; previous month and adjust the offset
                 ;; accordingly to reflect the correct number of
                 ;; weeks before the end of the month.
                 ;; e.g. if day = 15, n = -3, we're looking for the
                 ;; 3rd DOW before the 15th of the month,
                 ;; which is the 1st DOW "before" the end of the
                 ;; previous month (where "before" is inclusive,
                 ;; e.g offset = -1 will work when DOW is the last
                 ;; day of the month)
                 (when months
                   (setq months
                         (sort
                          :in-place t
                          (mapcar
                           (lambda (m) (if (eql m 1) 12 (1- m)))
                           months))))
                 (+ n (/ (1- day) 7)))))
         (rrule (delq nil
                      `((FREQ MONTHLY)
                        ,(when months
                           (list 'BYMONTH months))
                        (BYDAY ((,dow . ,offset))))))
         (dtstart
          (calendar-nth-named-day n dow
                                  (if months (apply #'min months) 1)
                                  di:recurring-start-year
                                  (unless (eq day 'last) day))))

    ;; if at this point we have an offset which could put us outside the
    ;; month boundaries, warn the user that this may not be supported:
    (when (< 4 (abs offset))
      (ical:warn
       (format
        "`diary-float' with large N=%d may not be supported on other systems" n)))

    (list (ical:make-property ical:dtstart dtstart
            (ical:valuetypeparam 'ical:date))
          (ical:make-property ical:rrule rrule))))

(defun di:offset-sexp-to-nodes (sexp)
  "Convert a `diary-offset' SEXP to a list of property nodes.

SEXP must have the form (diary-offset INNER-SEXP NDAYS).  The conversion
is only possible for relatively simple cases of INNER-SEXP.  The
INNER-SEXP is first converted to a list of property nodes (see
`diary-icalendar-export-sexp'), and then any date, time, period, and
recurrence rule values in these nodes are adjusted NDAYS forward."
  (let* ((arg1 (nth 1 sexp))
         (inner-sexp (if (eq (car arg1) 'quote)
                         (eval arg1 nil) ; unquote a quoted inner sexp
                       arg1))
         (nodes (di:sexp-to-nodes inner-sexp))
         (ndays (nth 2 sexp)))
    (dolist (node nodes)
      (ical:with-property node nil
       (cl-case (ical:ast-node-type node)
         ((ical:dtstart ical:dtend)
          (ical:ast-node-set-value
           value-node
           (ical:date/time-add value :day ndays)))
         (ical:exdate
          (dolist (val-node value-nodes)
            (ical:with-node-value val-node nil
              (ical:ast-node-set-value
               val-node
               (ical:date/time-add value :day ndays)))))
         (ical:rdate
          (dolist (val-node value-nodes)
            (ical:ast-node-set-value
              val-node
              (ical:with-node-value val-node nil
               (cl-typecase value
                (ical:period
                 (ical:make-period
                  (ical:date/time-add (ical:period-start value) :day ndays)
                  :end (when (ical:period--defined-end value)
                         (ical:date/time-add
                          (ical:period--defined-end value) :day ndays))
                  :duration (ical:period-dur-value value)))
                (t (ical:date/time-add value :day ndays)))))))
         (ical:rrule
          (let ((mdays (ical:recur-by* 'BYMONTHDAY value))
                (ydays (ical:recur-by* 'BYYEARDAY value))
                (dows (ical:recur-by* 'BYDAY value))
                (bad-clause
                 (cond ((ical:recur-by* 'BYSETPOS value) 'BYSETPOS)
                       ((ical:recur-by* 'BYWEEKNO value) 'BYWEEKNO))))
            ;; We can't reliably subtract days in the following cases, so bail:
            (when (< 28 ndays)
              (di:signal-export-error
               (format "Cannot export `diary-offset' with large offset %d" ndays)))
            (when bad-clause
              (di:signal-export-error
               (format "Cannot export `diary-offset': inner SEXP %s contains %s"
                       sexp bad-clause)))
            (when (seq-some (lambda (md)
                              (or (and (< 0 md) (< 28 (+ md ndays)))
                                  (and (< md 0) (< 0 (+ md ndays)))))
                            mdays)
              (di:signal-export-error
               (format "Cannot export `diary-offset': inner SEXP %s contains %s"
                       inner-sexp
                       "BYMONTHDAY clause that could cross month bounds")))
            (when (seq-some (lambda (yd)
                              (or (and (< 0 yd) (< 365 (+ yd ndays)))
                                  (and (< yd 0) (< 0 (+ yd ndays)))))
                            ydays)
              (di:signal-export-error
               (format "Cannot export `diary-offset': inner SEXP %s contains %s"
                       inner-sexp
                       "BYYEARDAY clause that could cross year bounds")))
            ;; Adjust the rule's clauses to account for the offset:
            (when mdays
              (setf (alist-get 'BYMONTHDAY value)
                    (list
                     (mapcar (apply-partially #'+ ndays) mdays))))
            (when ydays
              (setf (alist-get 'BYYEARDAY value)
                    (list
                     (mapcar (apply-partially #'+ ndays) ydays))))
            (when dows
              (setf (alist-get 'BYDAY value)
                    (list
                     (mapcar
                      (lambda (dow)
                        (if (integerp dow)
                            (mod (+ dow ndays) 7)
                          (let* ((wkday (car dow))
                                 (shifted (+ wkday ndays))
                                 (new-wkday (mod shifted 7))
                                 (new-offs
                                  (cond
                                   ;; if shifted is not between 0 and 7,
                                   ;; we moved into another week, so we need
                                   ;; to modify the offset within the month/year
                                   ;; by the number of weeks moved:
                                   ((< 7 shifted)
                                    (+ (/ shifted 7) (cdr dow)))
                                   ((< shifted 0)
                                    (+ -1 (/ shifted 7) (cdr dow)))
                                   ;; otherwise it stays the same:
                                   (t (cdr dow)))))
                            (cons new-wkday new-offs))))
                      dows)))))))))
    ;; Return the modified nodes:
    nodes))

;; Converts a legacy value of `icalendar-export-alarms' to new format of
;; `diary-icalendar-export-alarms':
(defun di:-convert-legacy-alarm-options (alarm-options)
  (declare (obsolete nil "31.1"))
  (let ((lead-time (car alarm-options))
        (by-types (cadr alarm-options)))
    (mapcar
     (lambda (l)
       (cl-case (car l)
         (audio `(audio ,lead-time))
         (display `(display ,lead-time "%s"))
         (email `(email ,lead-time "%s" ,(cadr l)))))
     by-types)))

(defun di:add-valarms (component &optional vtimezone)
  "Add VALARMs to COMPONENT according to `diary-icalendar-export-alarms'.

COMPONENT should be an `icalendar-vevent' or `icalendar-vtodo'.  The
generated VALARM components will be added to this node's children.
VTIMEZONE should define the local timezone; it is required when
formatting alarms as mail messages.  Returns the modified COMPONENT."
  (let* ((alarm-options
         (if (and (bound-and-true-p icalendar-export-alarms)
                  (null di:export-alarms))
             ;; For backward compatibility with icalendar.el:
             (with-suppressed-warnings
                 ((obsolete ical:export-alarms
                            di:-convert-legacy-alarm-options))
               (di:-convert-legacy-alarm-options ical:export-alarms))
           di:export-alarms))
         valarms)
    (dolist (opts alarm-options)
      (let* ((type (nth 0 opts))
             (minutes (nth 1 opts)))
        (cl-case type
          (audio
           (push (ical:make-valarm
                  (ical:action "AUDIO")
                  (ical:trigger (make-decoded-time :minute (* -1 minutes))))
                 valarms))
          (display
           (ical:with-component component
             ((ical:summary :value summary)
              (ical:description :value description))
             (let* ((displayed-summary
                     (replace-regexp-in-string
                      "%t" (number-to-string minutes)
                      (replace-regexp-in-string
                       "%s" summary
                       (nth 2 opts)))))
               (push (ical:make-valarm
                      (ical:action "DISPLAY")
                      (ical:trigger (make-decoded-time :minute (* -1 minutes)))
                      (ical:summary displayed-summary)
                      (ical:description description))
                     valarms))))
          (email
           (ical:with-component component
             ((ical:summary :value summary)
              (ical:attendee :all entry-attendees))
             (let* ((subject
                     (replace-regexp-in-string
                      "%t" (number-to-string minutes)
                      (replace-regexp-in-string
                       "%s" summary
                       (nth 2 opts))))
                    (index (ical:index-insert-tz (ical:make-index) vtimezone))
                    (body
                     (dlet ((ical-as-alarm 'email))
                       (di:format-entry component index)))
                    (addresses (nth 3 opts))
                    all-attendees)
               (dolist (address addresses)
                 (cond
                  ((eq address 'from-entry)
                   (setq all-attendees (append entry-attendees all-attendees)))
                  ((stringp address)
                   (push (ical:make-property ical:attendee
                             (concat "mailto:" address))
                         all-attendees))))
               (push (ical:make-valarm
                      (ical:action "EMAIL")
                      (ical:trigger (make-decoded-time :minute (* -1 minutes)))
                      (ical:summary subject)
                      (ical:description body)
                      (@ all-attendees))
                     valarms)))))))
    (apply #'ical:ast-node-adopt-children component valarms)
    component))

(defun di:rrule-sexp-to-recurrence (sexp &optional vtimezone)
  "Convert a `diary-rrule' SEXP to iCalendar recurrence rule properties.
Returns a list containing at least `icalendar-dtstart' and
`icalendar-rrule' nodes, and zero or more `icalendar-rdate',
`icalendar-exdate', and `icalendar-duration' nodes.

VTIMEZONE, if specified, should be an `icalendar-vtimezone'.  Times in
SEXP will be reinterpreted as local to VTIMEZONE, as UTC, or as floating
times according to `diary-icalendar-time-zone-export-strategy'."
  (let* ((args (cdr sexp))
         (start (plist-get args :start))
         (dtstart (di:convert-time-via-strategy
                   (if (eq 'quote (car start)) (eval start nil) start)
                   vtimezone))
         (rule (plist-get args :rule))
         (rrule (if (eq 'quote (car rule)) (eval rule nil) rule))
         (included (plist-get args :include))
         (rdates (mapcar
                  (lambda (dt) (di:convert-time-via-strategy dt vtimezone))
                  (if (eq 'quote (car included)) (eval included nil) included)))
         (excluded (plist-get args :exclude))
         (exdates (mapcar
                   (lambda (dt) (di:convert-time-via-strategy dt vtimezone))
                   (if (eq 'quote (car excluded)) (eval excluded nil) excluded)))
         (duration (eval (plist-get args :duration) t))
         (dur-value
          (if (eq 'quote (car duration)) (eval duration nil) duration))
         (tzid
          (when (cl-typep dtstart 'ical:date-time)
            (ical:with-property-of vtimezone 'ical:tzid)))
         nodes)
    (push (ical:make-property ical:rrule rrule) nodes)
    (push (ical:make-property ical:dtstart dtstart (ical:tzidparam tzid))
          nodes)
    (when rdates
      (push (ical:make-property ical:rdate rdates (ical:tzidparam tzid))
            nodes))
    (when exdates
      (push (ical:make-property ical:exdate exdates (ical:tzidparam tzid))
            nodes))
    (when duration
      (push (ical:make-property ical:duration dur-value) nodes))
    nodes))

(defun di:dates-to-recurrence (months days years)
  "Convert values representing one or more dates to iCalendar recurrences.

MONTHS, DAYS, and YEARS should either be integers, lists of integers, or
the symbol t.

Returns a pair of nodes (START R), where START is an `icalendar-dtstart'
node and R is an `icalendar-rrule' node or `icalendar-rdate' node (or
nil, if MONTHS, DAYS and YEARS are all integers)."
  (if (and (integerp months) (integerp days) (integerp years))
      ;; just a regular date, without recurrence data:
      (list
       (ical:make-property ical:dtstart (list months days years))
       nil)

    (when (integerp months) (setq months (list months)))
    (when (integerp days) (setq days (list days)))
    (when (integerp years) (setq years (list years)))
    (let (dtstart freq bymonth bymonthday rdates rdate-type)
      (cond ((and (eq days t) (eq months t) (eq years t))
             (setq freq 'DAILY
                   dtstart (list 1 1 di:recurring-start-year)))
            ((and (eq months t) (eq years t))
             (setq freq 'MONTHLY
                   bymonthday days
                   dtstart (list 1 (car days) di:recurring-start-year)))
            ((and (eq years t) (eq days t))
             (setq freq 'DAILY
                   bymonth months
                   dtstart (list (apply #'min months)
                                 1
                                 di:recurring-start-year)))
            ((eq years t)
             (setq freq 'YEARLY
                   bymonth months
                   bymonthday days
                   dtstart
                   (list (apply #'min months)
                         (apply #'min days)
                         di:recurring-start-year)))
            ;; The remaining cases are not representable as RRULEs,
            ;; because there is no BYYEAR clause.  So we generate an RDATE
            ;; covering each specified date.
            ((and (eq months t) (eq days t))
             ;; In this case we represent each of the specified years as a period:
             (setq rdate-type 'ical:period
                   rdates
                   (mapcar
                    (lambda (y)
                      (ical:make-period
                       (ical:make-date-time :year y :month 1 :day 1
                                            :hour 0 :minute 0 :second 0)
                       :end
                       (ical:make-date-time :year (1+ y) :month 1 :day 1
                                            :hour 0 :minute 0 :second 0)))
                    years)
                   dtstart (ical:date-time-to-date
                            (ical:period-start (car rdates)))))
            (t
             ;; Otherwise, represent each date individually:
             (setq rdate-type 'ical:date
                   rdates
                   (mapcan
                    (lambda (y)
                      (mapcan
                       (lambda (m)
                         (mapcar
                          (lambda (d) (list m d y))
                          (if (listp days) days
                            ;; days = t:
                            (number-sequence 1 (calendar-last-day-of-month m y)))))
                       (if (listp months) months
                         ;; months = t:
                         (number-sequence 1 12))))
                    years)
                   ;; ensure dtstart is the earliest recurrence:
                   dtstart (apply #'ical:date/time-min rdates)
                   rdates (seq-remove (apply-partially #'equal dtstart) rdates))))

      ;; Return the pair of nodes (DTSTART RRULE) or (DTSTART RDATE):
      (let* ((recur-value
              (delq nil
                    `((FREQ ,freq)
                      ,(when bymonth (list 'BYMONTH bymonth))
                      ,(when bymonthday (list 'BYMONTHDAY bymonthday)))))
           (rrule-node (when freq (ical:make-property ical:rrule recur-value)))
           (rdate-node (when rdates
                         (ical:make-property ical:rdate rdates
                           (ical:valuetypeparam rdate-type))))
           (dtstart-node (ical:make-property ical:dtstart dtstart)))
        (list dtstart-node (or rrule-node rdate-node))))))

(defun di:date-sexp-to-recurrence (sexp)
  "Convert a `diary-date' SEXP to an `icalendar-rrule' or `icalendar-rdate' node.
Returns a pair of nodes (START R), where START is an `icalendar-dtstart'
node and R is the RRULE or RDATE node."
  (let* ((d1 (nth 1 sexp))
         (d2 (nth 2 sexp))
         (d3 (nth 3 sexp))
         years months days)
    (cl-case calendar-date-style
      (iso (setq years (if (integerp d1) (list d1) d1)
                 months (if (integerp d2) (list d2) d2)
                 days (if (integerp d3) (list d3) d3)))
      (american (setq months (if (integerp d1) (list d1) d1)
                      days (if (integerp d2) (list d2) d2)
                      years (if (integerp d3) (list d3) d3)))
      (european (setq days (if (integerp d1) (list d1) d1)
                      months (if (integerp d2) (list d2) d2)
                      years (if (integerp d3) (list d3) d3))))

    ;; unquote lists of integers read as quoted lists:
    (when (and (listp months) (eq 'quote (car months)))
      (setq months (eval months nil)))
    (when (and (listp days) (eq 'quote (car days)))
      (setq days (eval days nil)))
    (when (and (listp years) (eq 'quote (car years)))
      (setq years (eval years nil)))

    ;; if at this point we don't have lists of integers or "t", user
    ;; entered a malformed diary-date sexp:
    (unless (or (eq months t) (seq-every-p #'integerp months))
      (di:signal-export-error
       (format "Malformed months in `diary-date' S-expression:\n%s" sexp)))
    (unless (or (eq days t) (seq-every-p #'integerp days))
      (di:signal-export-error
       (format "Malformed days in `diary-date' S-expression:\n%s" sexp)))
    (unless (or (eq years t) (seq-every-p #'integerp years))
      (di:signal-export-error
       (format "Malformed years in `diary-date' S-expression:\n%s" sexp)))

    (di:dates-to-recurrence months days years)))

(defun di:other-sexp-to-recurrence (sexp)
  "Convert diary SEXP to `icalendar-rdate' by enumerating its recurrences.

The enumeration starts on the current date and includes recurrences in
the next `diary-icalendar-export-sexp-enumeration-days' days.  Returns a
list (START COMMENT RDATE), where START is an `icalendar-dtstart',
COMMENT is an `icalendar-comment' containing SEXP, and RDATE is an
`icalendar-rdate' containing the enumerated recurrences.  If there are
no recurrences, (START COMMENT EXDATE) is returned, where START is the
current date, and EXDATE is an `icalendar-exdate' excluding that start
date as a recurrence.  (This is because `icalendar-dtstart' is a required
property and must be present even if the recurrence set is empty.)"
  (let* ((today (calendar-absolute-from-gregorian (calendar-current-date)))
         (end (+ today (1- di:export-sexp-enumeration-days)))
        dtstart rdates exdates)
    (dolist (absdate (number-sequence today end))
      (calendar-dlet ((date (calendar-gregorian-from-absolute absdate)))
        (when (eval sexp t)
          (push date rdates))))
    (if rdates
        (progn
          (setq rdates (nreverse rdates))
          (setq dtstart (car rdates)
                rdates (cdr rdates)))
      (ical:warn
       (format "No recurrences in the next %d days: %s"
               di:export-sexp-enumeration-days
               sexp)
       :severity 0)
      ;; When there are no recurrences, we still need a DTSTART, but we
      ;; can exclude it via an EXDATE:
      (setq dtstart (calendar-current-date)
            exdates (list dtstart)))

    (append
     (list
      (ical:make-property ical:dtstart dtstart
        (ical:valuetypeparam 'ical:date))
      ;; TODO: should we maybe use an X-name property for this?
      (ical:make-property ical:comment (format "%s" sexp)))
     (if rdates
         (list
          (ical:make-property ical:rdate rdates
            (ical:valuetypeparam 'ical:date)))
       (list
        (ical:make-property ical:exdate exdates
          (ical:valuetypeparam 'ical:date)))))))

(defun di:sexp-to-nodes (sexp &optional vtimezone)
  "Convert a diary S-expression SEXP to a list of iCalendar property nodes.

The fully supported S-expressions are:
`diary-anniversary'
`diary-block'
`diary-cyclic'
`diary-date'
`diary-float'
`diary-remind'
`diary-rrule'
`diary-time-block'

There is partial support for `diary-offset' S-expressions; see
`diary-icalendar-offset-to-nodes'.

Other S-expressions are only supported via enumeration.  Their
recurrences are enumerated for
`diary-icalendar-export-sexp-enumeration-days' starting from the current
date; see `diary-icalendar-other-sexp-to-recurrence'.  If
`diary-icalendar-export-sexp-enumerate-all' is non-nil, all
S-expressions are enumerated rather than converted to recurrence rules.

VTIMEZONE, if specified, should be an `icalendar-vtimezone'.  Times in
SEXP will be reinterpreted as local to VTIMEZONE, as UTC, or as floating
times according to `diary-icalendar-time-zone-export-strategy'."
  (if di:export-sexp-enumerate-all ;; see Bug#7911 for motivation
      (di:other-sexp-to-recurrence sexp)
    (cl-case (car sexp)
      (diary-anniversary (di:anniversary-sexp-to-recurrence sexp))
      (diary-block (di:block-sexp-to-recurrence sexp))
      (diary-cyclic (di:cyclic-sexp-to-recurrence sexp))
      (diary-date (di:date-sexp-to-recurrence sexp))
      (diary-float (di:float-sexp-to-recurrence sexp))
      (diary-offset (di:offset-sexp-to-nodes sexp))
      (diary-rrule (di:rrule-sexp-to-recurrence sexp vtimezone))
      (diary-time-block (di:time-block-sexp-to-start-end sexp vtimezone))
      ;; For `diary-remind' we only handle the inner sexp:
      (diary-remind (di:sexp-to-nodes (nth 1 sexp) vtimezone))
      (t (di:other-sexp-to-recurrence sexp)))))

;;; Time zone handling during export:

(defun di:current-tz-to-vtimezone (&optional tz tzid start-year)
  "Convert TZ to an `icalendar-vtimezone'.

See `icalendar-recur-current-tz-to-vtimezone' for arguments' meanings.
This function wraps that one, but signals `icalendar-diary-export-error'
instead if TZ cannot be converted."
  (condition-case _
      (icr:current-tz-to-vtimezone tz tzid start-year)
    ((ical:tz-insufficient-data ical:tz-unsupported)
     (di:signal-export-error
      (format "Unable to export time zone data: %s.\n%s." tz
              "Check the value of `diary-icalendar-time-zone-export-strategy'")))))

;;; Parsing complete diary entries:

(defun di:parse-entry-linewise (begin end vtimezone type date-nodes)
  "Convert the entry between BEGIN and END linewise to iCalendar components.

\"Linewise\" means each line of a diary entry will be exported as a
distinct event; see `diary-icalendar-export-linewise'.
Returns a list of component nodes representing the events.

VTIMEZONE must be the `icalendar-vtimezone' in which times in the entry
appear (or nil).  TYPE and DATE-NODES must contain the iCalendar component
type and date information parsed from the beginning of the entry which
apply to all of the events.  These arguments are passed on in recursive
calls to `diary-icalendar-parse-entry'."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (let ((subentry-regexp
           ;; match to the end of lines which have indentation equal to
           ;; or greater than the current one:
           (rx line-start
               (group-n 1 (+ space))
               (* not-newline)
               (* "\n" (backref 1) (+ space) (* not-newline))))
          components)

      (while (re-search-forward subentry-regexp end t)
        (let ((next-pos (1+ (match-end 0))))
          (setq components
                (append
                 (di:parse-entry (match-beginning 0) (match-end 0)
                                  vtimezone type date-nodes)
                 components))
          (goto-char next-pos)))
      components)))

(defun di:parse-entry (begin end &optional vtimezone type date-nodes)
  "Convert the entry between BEGIN and END to a list of iCalendar components.

The region between BEGIN and END will be parsed for a date, time,
summary, description, attendees, and UID.  This information will be
combined into an `icalendar-vevent' (or `icalendar-vjournal' or
`icalendar-vtodo', depending on the values of
`diary-icalendar-export-nonmarking-entries',
`diary-icalendar-export-nonmarking-as-vjournal' and
`diary-icalendar-todo-regexp') and that component will be returned
wrapped in a list.  Returns nil if the entry should not be exported
according to `diary-icalendar-export-nonmarking-entries'.

If `diary-icalendar-export-linewise' is non-nil, then a top-level call
to this function will return a list of several such components.  (Thus,
the function always returns a list of components.)

VTIMEZONE, if specified, should be the `icalendar-vtimezone' in which
times in the entry appear.  If
`diary-icalendar-time-zone-export-strategy' is not either \\='to-utc or
\\='floating, VTIMEZONE must be provided.

DATE-NODES and TYPE should be nil in a top-level call; they are used in
recursive calls to this function made by
`diary-icalendar-parse-entry-linewise'."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (let (sexp dateform weekday tzid transparency all-props should-recurse)
      (setq should-recurse (and di:export-linewise (not date-nodes) (not type)))
      (when (ical:vtimezone-component-p vtimezone)
        (setq tzid (ical:with-property-of vtimezone 'ical:tzid)))
      (unless date-nodes
        ;; If we don't already have date information, we are in a
        ;; top-level call and need to collect the date and type
        ;; information from the start of the entry:
        (setq type (di:parse-entry-type))
        ;; N.B. the following four parsing functions successively
        ;; narrow the current restriction past anything they parse:
        (setq transparency (di:parse-transparency type))
        (setq sexp (di:parse-sexp))
        (setq dateform (di:parse-date-form))
        (setq weekday (di:parse-weekday-name))
        (setq date-nodes
              (append
               transparency
               (when sexp (di:sexp-to-nodes sexp vtimezone))
               (when dateform
                 (apply #'di:dates-to-recurrence dateform))
               (when (and weekday (not dateform))
                 (di:weekday-to-recurrence weekday)))))

      (when type ; nil means entry should not be exported
        (if should-recurse
            ;; If we are in a top level call and should export linewise,
            ;; do that recursively now:
            (di:parse-entry-linewise (point) end vtimezone type date-nodes)

          ;; Otherwise, we are either in a recursive call with a
          ;; narrower restriction, or don't need to export linewise.  In
          ;; both cases, we gather the remaining data from the current
          ;; restriction and combine everything into a component node:
          (let* ((times (di:parse-time))
                 (start-time (when times (car times)))
                 (end-time (when times (cadr times))))
            ;; Combine clock time values in the current restriction with
            ;; date information parsed at the top level.  Doing this here
            ;; allows us to combine a different time on each line of an
            ;; entry exported linewise with the date information for the
            ;; whole entry:
            (dolist (node date-nodes)
              (ical:with-property node nil
                (cond
                 ((and (ical:dtstart-property-p node)
                       (eq 'ical:date value-type)
                       start-time)
                  (let ((dtstart
                         (di:convert-time-via-strategy
                          (ical:date-time-variant
                           start-time
                           :year (calendar-extract-year value)
                           :month (calendar-extract-month value)
                           :day (calendar-extract-day value))
                          vtimezone)))
                    (push (ical:make-property ical:dtstart dtstart
                            (ical:tzidparam tzid))
                          all-props)
                    (when end-time
                      ;; an end time parsed from a time specification
                      ;; in the entry is always on the same day as
                      ;; DTSTART.
                      (let* ((dtend
                              (di:convert-time-via-strategy
                               (ical:date-time-variant
                                end-time
                                :year (calendar-extract-year value)
                                :month (calendar-extract-month value)
                                :day (calendar-extract-day value))
                               vtimezone))
                             (is-recurring
                              (seq-find
                               (lambda (n) (or (ical:rrule-property-p n)
                                               (ical:rdate-property-p n)))
                               date-nodes)))
                        (if is-recurring
                            ;; If the entry is recurring, we interpret
                            ;; the end time as giving us a duration for all
                            ;; recurrences:
                            (progn
                              (when (seq-find #'ical:duration-property-p
                                              date-nodes)
                                (ical:warn
                                 (concat "Parsed both duration and end time; "
                                         "ignoring end time specification")
                                 :buffer (current-buffer)
                                 :position (point)))
                              (push (ical:make-property ical:duration
                                        (ical:duration-between dtstart dtend))
                                    all-props))
                          ;; Otherwise we make a normal DTEND:
                          (push (ical:make-property ical:dtend dtend)
                                all-props))))))

                 ((and (ical:rdate-property-p node)
                       start-time
                       (seq-every-p (apply-partially #'eq 'ical:date)
                                    value-types))
                  (let ((rdates
                         (mapcar
                          (lambda (dt)
                            (if end-time
                                (ical:make-period
                                 (di:convert-time-via-strategy
                                  (ical:date-time-variant
                                   start-time
                                   :year (calendar-extract-year dt)
                                   :month (calendar-extract-month dt)
                                   :day (calendar-extract-day dt))
                                  vtimezone)
                                 :end
                                 (di:convert-time-via-strategy
                                  (ical:date-time-variant
                                   end-time
                                   :year (calendar-extract-year dt)
                                   :month (calendar-extract-month dt)
                                   :day (calendar-extract-day dt))
                                  vtimezone))
                              (di:convert-time-via-strategy
                               (ical:date-time-variant
                                start-time
                                :year (calendar-extract-year dt)
                                :month (calendar-extract-month dt)
                                :day (calendar-extract-day dt))
                               vtimezone)))
                          values)))
                    (push (ical:make-property ical:rdate rdates
                            (ical:tzidparam tzid))
                          all-props)))

                   ;; preserve any other node read from date, e.g. RRULE, as is:
                   (node (push node all-props))))))

          ;; In a VTODO, entry date must become the DUE date; either
          ;; DTEND becomes DUE, or if there is no DTEND, then DTSTART:
          (when (eq type 'ical:vtodo)
            (unless (catch 'found-dtend
                      (dolist (node all-props)
                        (when (ical:dtend-property-p node)
                          (ical:ast-node-set-type node 'ical:due)
                          (throw 'found-dtend t))))
              (dolist (node all-props)
                (when (ical:dtstart-property-p node)
                  (ical:ast-node-set-type node 'ical:due)))))

          ;; Collect the remaining properties:
          (setq all-props (append (di:parse-summary-and-description) all-props))
          (setq all-props (append (di:parse-attendees-and-organizer) all-props))
          (push (ical:make-property ical:dtstamp (decode-time nil t)) all-props)
          (let ((class (di:parse-class))
                (location (di:parse-location))
                (status (di:parse-status))
                (url (di:parse-url)))
            (when class (push class all-props))
            (when location (push location all-props))
            (when status (push status all-props))
            (when url (push url all-props)))
          (push (or (di:parse-uid)
                     (ical:make-property ical:uid
                         (ical:make-uid all-props)))
                all-props)

          ;; Allow users to add to the properties parsed:
          (when (functionp di:other-properties-parser)
            (let ((others (funcall di:other-properties-parser type all-props)))
              (dolist (p others)
                (condition-case nil
                    (push (ical:ast-node-valid-p p)
                          all-props)
                  (ical:validation-error
                   (ical:warn
                    (format "`%s' returned invalid `%s' property; ignoring"
                            di:other-properties-parser
                            (ical:ast-node-type p))
                    :buffer (current-buffer)
                    :position (point)))))))

          ;; Construct, validate and return a component of the appropriate type:
          (let ((component
                 (ical:ast-node-valid-p
                  (ical:make-ast-node type nil all-props))))

            ;; Add alarms per `diary-icalendar-export-alarms', except for
            ;; in VJOURNAL, where alarms are not allowed:
            ;; TODO: should we also add alarms for `diary-remind' sexps?
            (when (not (eq type 'ical:vjournal))
              (di:add-valarms component vtimezone))

            ;; Return the component wrapped in a list (for type consistency):
            (list component)))))))

;;;###autoload
(defun di:export-region (begin end filename &optional erase)
  "Export diary entries between BEGIN and END to iCalendar format in FILENAME.

If FILENAME exists and is not empty, this function asks whether to erase
its contents first.  If ERASE is non-nil, the contents of FILENAME will
always be erased without asking.  Otherwise the exported data will be
appended to the end of FILENAME.

The export depends on a number of user-customizable variables.  Before
running this command for the first time, you may especially wish to
check the values of:
`diary-file'
`diary-date-forms'
`calendar-date-style'
as well as variables in the customization group `diary-icalendar-export'."
  (interactive (list (region-beginning)
                     (region-end)
                     (expand-file-name
                      (read-file-name "iCalendar file: "))))

  (ical:init-error-buffer)
  (let (output-buffer local-tz components vcalendar)
    (when (and (null erase)
               (file-exists-p filename)
               (< 0 (file-attribute-size (file-attributes filename)))
               (y-or-n-p (format "Delete existing contents of %s?" filename)))
      (setq erase t))
    (ical:condition-case err
      (setq output-buffer (find-file-noselect filename)))
    (when output-buffer
      (save-excursion
        (save-restriction
          (narrow-to-region begin end)
          (goto-char (point-min))
          (cond ((listp di:time-zone-export-strategy)
                 (setq local-tz (di:current-tz-to-vtimezone
                                 di:time-zone-export-strategy)))
                ((and (eq 'local di:time-zone-export-strategy)
                      (not (di:-tz-is-utc-p))) ; don't generate a VTIMEZONE in UTC
                 (setq local-tz (di:current-tz-to-vtimezone))))
          (while (re-search-forward di:entry-regexp nil t)
            (let ((entry-start (match-beginning 0))
                  (entry-end (match-end 0))
                  (first-line (match-string 1)))
              (ical:condition-case err-data
                 (setq components
                       (append (di:parse-entry entry-start entry-end local-tz)
                               components))
                (ical:export-error
                 (ical:warn
                  (concat
                   (format "Unable to export entry \"%s...\"; skipping" first-line)
                   "\nError was:\n"
                   (plist-get err-data :message))
                  :position entry-start
                  :buffer (current-buffer))))
              (goto-char (1+ entry-end))))
          (setq components (nreverse components))
          (when (ical:vtimezone-component-p local-tz)
            (push local-tz components))
          (ical:condition-case err-data
             (setq vcalendar (ical:make-vcalendar (@ components))))

          (when vcalendar
            (with-current-buffer output-buffer
              (when erase (erase-buffer))
              (goto-char (point-max)) ; append, if user chose not to erase
              (unless (bolp) (insert "\n"))
              (ical:condition-case err-data
                 (insert (ical:print-calendar-node vcalendar)))
              (let ((coding-system-for-write 'utf-8-dos))
                (save-buffer))))))))

  (message
   (if (ical:errors-p)
       (format "iCalendar export completed with errors; see buffer %s"
               (buffer-name (ical:error-buffer)))
     "iCalendar export completed successfully.")))

;;;###autoload
(defun di:export-file (diary-filename filename &optional erase)
  "Export DIARY-FILENAME to iCalendar format in FILENAME.

The diary entries in DIARY-FILENAME will be exported to iCalendar format
and the resulting calendar will be saved to FILENAME.

If FILENAME exists and is not empty, this function asks whether to erase
its contents first.  If ERASE is non-nil, the contents of FILENAME will
always be erased without asking.  Otherwise the exported data will be
appended to the end of FILENAME.

The export depends on a number of user-customizable variables.  Before
running this command for the first time, you may especially wish to
check the values of:
`diary-file'
`diary-date-forms'
`calendar-date-style'
as well as variables in the customization group `diary-icalendar-export'."
  (interactive (list
                (read-file-name "Diary file: "
                                (when diary-file (file-name-directory diary-file))
                                (cons diary-file diary-included-files)
                                'confirm)
                (read-file-name "iCalendar file: "
                                (when diary-file (file-name-directory diary-file))
                                (when diary-file
                                  (concat
                                   (file-name-sans-extension diary-file)
                                   ".ics")))))
  (when (and (null erase)
             (file-exists-p filename)
             (< 0 (file-attribute-size (file-attributes filename)))
             (y-or-n-p (format "Delete existing contents of %s?" filename)))
      (setq erase t))
  (with-current-buffer (find-file-noselect diary-filename)
    (di:export-region (point-min) (point-max) filename erase)))


;; Display in Diary

;;; Functions implementing diary-icalendar sexps.
;;; TODO: move these to diary-lib.el?

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(cl-defun diary-time-block (&key start end)
  "Diary S-expression for time blocks.

Entry applies if the queried date occurs between START and END,
inclusive.  START and END may be `icalendar-date' or
`icalendar-date-time' values."
  (with-no-warnings (defvar date) (defvar entry))
  (when (and (ical:date/time<= start date) (ical:date/time<= date end))
    entry))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(cl-defun diary-rrule (&key rule start duration include exclude)
  "Diary S-expression for iCalendar recurrence rules.

Entry applies if the queried date matches the recurrence rule.

The keyword arguments RULE, START, INCLUDE and EXCLUDE should contain
the recurrence data from an iCalendar component.  RULE should be an
`icalendar-recur' value, START an `icalendar-date' or
`icalendar-date-time', DURATION an `icalendar-dur-value', and INCLUDE
and EXCLUDE should be lists of `icalendar-date' or `icalendar-date-time'
values (of the same type as START)."
  ;; TODO: also support a format that is nicer to read and type by hand.
  ;; e.g. just letting a rule be specified in a recur-value string like
  ;;   :rule "FREQ=MONTHLY;BYDAY=1SU"
  ;; is perhaps already better than the raw Lisp format.  We could at least
  ;; support specifying the clauses with keywords, e.g.
  ;;   :freq :monthly :byday '("Sunday" . 1)
  ;; would be better than the current
  ;;   :rule '((FREQ MONTHLY) (BYDAY ((0 . 1))))
  (with-no-warnings (defvar date) (defvar entry))
  (when (ical:date<= start date)
    (let* ((vevent (ical:make-vevent
                    (ical:rrule rule)
                    (ical:dtstart start)
                    (ical:rdate include)
                    (ical:exdate exclude)))
           (interval (icr:find-interval date start rule)))
      (cl-typecase start
        (ical:date
         (when (member date (icr:recurrences-in-interval interval vevent))
           entry))
        (ical:date-time
         ;; TODO.  If start is a date-time, it was probably imported from
         ;; an iCalendar file, but in order to calculate recurrences, we
         ;; really need all the time zone information from that file,
         ;; not just the rule, start, include and exclude.  But encoding
         ;; all that tz info in a diary s-exp is cumbersome and ugly and
         ;; probably not worth the trouble.  Since this is the diary, we
         ;; assume that all we really care about here is whether there
         ;; are recurrences on a particular day.  Thus we convert
         ;; HOURLY/MINUTELY/SECONDLY rules to a DAILY rule, and all
         ;; values to plain dates.  This keeps things simple (and
         ;; hopefully quicker) but means that information gets lost.  I
         ;; hope this can be changed to do things right at some point,
         ;; but that will require first adding more robust time zone
         ;; support to the diary somehow -- perhaps via #included
         ;; iCalendar files?
         (let* ((date-rule (copy-sequence rule))
                (start-date (ical:date-time-to-date start))
                (include-dates (mapcar #'ical:date-time-to-date include))
                (exclude-dates (mapcar #'ical:date-time-to-date exclude))
                ;; Preserve the clock times in the entry:
                (entry-time
                 (if duration
                     (di:format-time-range
                      start
                      (ical:date/time-add-duration start duration))
                   (di:format-time-as-local start)))
                (date-entry (concat entry-time " " entry)))
           (when (memq (ical:recur-freq date-rule) '(HOURLY MINUTELY SECONDLY))
             (setf (alist-get 'FREQ date-rule) 'DAILY)
             (setf (alist-get 'INTERVAL date-rule) 1)
             (setf (alist-get 'BYHOUR date-rule nil t) nil)
             (setf (alist-get 'BYMINUTE date-rule nil t) nil)
             (setf (alist-get 'BYSECOND date-rule nil t) nil))
           ;; Recurse with the plain date values:
           (calendar-dlet
               ((date date)
                (entry date-entry))
             (diary-rrule :rule date-rule :start start-date
                          :include include-dates :exclude exclude-dates))))))))

(defun di:display-entries ()
  "Display iCalendar data from a file in the diary.

This function allows you to display the data in an iCalendar-formatted
file in the diary without importing it.  The data is read directly from
the currently value of `diary-file'.  If this file contains iCalendar
data, any events, tasks, and journal entries in the file which occur on
`original-date' and `number' of days after are formatted for display in
the diary.  (All three of these variables are dynamically bound by the
diary when this function is called.)

To use this function, add an '#include \"FILE\"' entry in your diary
file for each iCalendar file you want to display (see
`diary-include-string').  Then add `diary-include-other-diary-files' to
`diary-list-entries-hook'.  (Consider also adding `diary-sort-entries' at
the end of this hook if you want entries to be displayed in order.)
Finally, add this function to `diary-nongregorian-listing-hook', so that
it is called once for each included file when the diary is displayed."
  (with-no-warnings (defvar original-date) ; the start date
                    (defvar number) ; number of days to generate entries for
                    (defvar diary-file)) ; dyn. bound to included file name
  (let ((diary-buffer (or (find-buffer-visiting diary-file)
                          (find-file-noselect diary-file))))
    (when (ical:contains-vcalendar-p diary-buffer)
      (let ((vcal/idx (ical:parse-and-index diary-file)))
        (when vcal/idx
          (let* ((index (cadr vcal/idx))
                 (absstart (calendar-absolute-from-gregorian original-date))
                 (absend (+ absstart (1- number))))

            (dolist (absdate (number-sequence absstart absend))
              (let* ((date (calendar-gregorian-from-absolute absdate))
                     (to-format (ical:index-get index :date date)))
                (dolist (component to-format)
                  ;; Format the entry, with a pointer back to its location
                  ;; in the parsed buffer:
                  (let ((marker (make-marker)))
                    (set-marker marker
                                (ical:ast-node-meta-get :begin component)
                                (ical:ast-node-meta-get :buffer component))
                    (diary-add-to-list
                     date
                     (di:format-entry component index)
                     ""
                     marker)))))))))))

(defun di:marking-dates-of (component index)
  "Return the dates in COMPONENT that should be marked in the calendar.

INDEX should be a parse tree index containing the time zone definition
relevant to COMPONENT; see `icalendar-parse-and-index'.  The dates to
mark are derived from COMPONENT's start and end date and time, and any
recurrences it has within the year currently displayed by the calendar.

No dates are returned if COMPONENT's `icalendar-transp' property has the
value \"TRANSPARENT\" (which means the component does not form a block
of busy time on a schedule), or if COMPONENT is an `icalendar-vjournal'
and `diary-icalendar-import-vjournal-as-nonmarking' is non-nil."
  (ical:with-component component
    ((ical:dtstart :first dtstart-node :value dtstart)
     (ical:dtend :first dtend-node :value dtend)
     (ical:due :value due)
     (ical:duration :value duration)
     (ical:rdate :first rdate)
     (ical:rrule :first rrule)
     (ical:transp :value transparency))
    (let* ((start-tz (ical:with-param-of dtstart-node 'ical:tzidparam
                                         (ical:index-get index :tzid value)))
           (end
            (cond
             (dtend dtend)
             (due due)
             (duration (ical:date/time-add-duration dtstart duration start-tz))))
           dates)

      (unless (or (equal transparency "TRANSPARENT")
                  (and di:import-vjournal-as-nonmarking
                       (ical:vjournal-component-p component)))
        ;; Mark the start date(s) for every (marking) entry:
        (setq dates (if end
                        (ical:dates-until dtstart end t)
                      (list (ical:date/time-to-date
                             (ical:date/time-to-local dtstart)))))
        ;; Mark the dates for any recurrences in the displayed calendar year:
        (let ((year (when (boundp 'displayed-year) ; bound by calendar
                      displayed-year)))
          (when (and year (or rdate rrule))
            (let* ((low (list 1 1 year))
                   (high (list 12 31 year))
                   (recs (icr:recurrences-in-window-w/end-times
                          low high component start-tz)))
              (dolist (rec recs)
                (setq dates (append (ical:dates-until (car rec) (cadr rec) t)
                                    dates)))))))
      dates)))

(defun di:mark-entries ()
  "Mark calendar dates for iCalendar data from a file.

This function allows you to mark the dates in an iCalendar-formatted
file in the calendar without importing it.  The data is read directly
from the current value of `diary-file' (which is dynamically bound by
the diary when this function is called).

To use this function, add an '#include \"FILE\"' entry in your diary
file for each iCalendar file you want to display (see
`diary-include-string').  Then add `diary-mark-included-diary-files' to
`diary-mark-entries-hook'.  Finally, add this function to
`diary-nongregorian-marking-hook', so that it is called once for each
included file when dates are marked in the calendar."
  (with-no-warnings (defvar diary-file)) ; dyn. bound to included file name
  (let ((diary-buffer (or (find-buffer-visiting diary-file)
                          (find-file-noselect diary-file))))
    (when (ical:contains-vcalendar-p diary-buffer)
      (let ((vcal/idx (ical:parse-and-index diary-buffer)))
        (when vcal/idx
          (let* ((index (cadr vcal/idx))
                 (vcalendar (car vcal/idx))
                 (to-mark
                  (append (ical:ast-node-children-of 'ical:vevent vcalendar)
                          (ical:ast-node-children-of 'ical:vjournal vcalendar)
                          (ical:ast-node-children-of 'ical:vtodo vcalendar)))
                 (all-dates (mapcan (lambda (c) (di:marking-dates-of c index))
                                    to-mark))
                 (dates (seq-uniq
                         (sort all-dates :lessp #'ical:date< :in-place t))))

            (dolist (date dates)
              (let ((month (calendar-extract-month date))
                    (year (calendar-extract-year date)))
                ;; avoid marking outside the displayed months,
                ;; to speed things up:
                (with-current-buffer calendar-buffer
                  (with-suppressed-warnings
                      ((free-vars displayed-year
                                  displayed-month))
                    (when (and (= year displayed-year)
                               (<= (1- displayed-month) month)
                               (<= month (1+ displayed-month)))
                      (calendar-mark-visible-date date))))))))))))



(provide 'diary-icalendar)

;; Local Variables:
;; read-symbol-shorthands: (("ical:" . "icalendar-") ("icr:" . "icalendar-recur-") ("di:" . "diary-icalendar-"))
;; End:
;;; diary-icalendar.el ends here
