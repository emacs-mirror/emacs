;;; calendar.el --- Calendar functions.  -*-byte-compile-dynamic: t;-*-

;;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994 Free Software
;;; Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: calendar, Gregorian calendar, Julian calendar, 
;;	Hebrew calendar, Islamic calendar, ISO calendar, Julian day number,
;;	diary, holidays

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This collection of functions implements a calendar window.  It
;; generates a calendar for the current month, together with the previous
;; and coming months, or for any other three-month period.  The calendar
;; can be scrolled forward and backward in the window to show months in
;; the past or future; the cursor can move forward and backward by days,
;; weeks, or months, making it possible, for instance, to jump to the
;; date a specified number of days, weeks, or months from the date under
;; the cursor.  The user can display a list of holidays and other notable
;; days for the period shown; the notable days can be marked on the
;; calendar, if desired.  The user can also specify that dates having
;; corresponding diary entries (in a file that the user specifies) be
;; marked; the diary entries for any date can be viewed in a separate
;; window.  The diary and the notable days can be viewed independently of
;; the calendar.  Dates can be translated from the (usual) Gregorian
;; calendar to the day of the year/days remaining in year, to the ISO
;; commercial calendar, to the Julian (old style) calendar, to the Hebrew
;; calendar, to the Islamic calendar, to the French Revolutionary calendar,
;; to the Mayan calendar, and to the astronomical (Julian) day number.
;; When floating point is available, times of sunrise/sunset can be displayed,
;; as can the phases of the moon.  Appointment notification for diary entries
;; is available.

;; The following files are part of the calendar/diary code:

;;       cal-menu.el                   Menu support
;;       cal-x.el                      X-windows dedicated frame functions
;;       diary-lib.el, diary-ins.el    Diary functions
;;       holidays.el                   Holiday functions
;;       cal-french.el                 French Revolutionary calendar
;;       cal-mayan.el                  Mayan calendars
;;       cal-dst.el                    Daylight savings time rules
;;       solar.el                      Sunrise/sunset, equinoxes/solstices
;;       lunar.el                      Phases of the moon
;;       appt.el                       Appointment notification

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;; GNU Emacs users too numerous to list pointed out a variety of problems
;; with earlier forms of the `infinite' sliding calendar and suggested some
;; of the features included in this package.  Especially significant in this
;; regard was the suggestion of mark-diary-entries and view-diary-entries,
;; together ideas for their implementation, by
;;  Michael S. Littman		     Cognitive Science Research Group
;;  (201) 829-5155                   Bell Communications Research
;;  mlittman@wind.bellcore.com       445 South St. Box 1961 (2L-331)
;;                                   Morristown, NJ  07960

;; The algorithms for the Hebrew calendar are those of the Rambam (Rabbi Moses
;; Maimonides), from his Mishneh Torah, as implemented by
;;  Nachum Dershowitz                Department of Computer Science
;;  (217) 333-4219                   University of Illinois at Urbana-Champaign
;;  nachum@cs.uiuc.edu               1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;; Technical details of all the calendrical calculations can be found in

;; ``Calendrical Calculations'' by Nachum Dershowitz and Edward M. Reingold,
;; Software--Practice and Experience, Volume 20, Number 9 (September, 1990),
;; pages 899-928.  ``Calendrical Calculations, Part II: Three Historical
;; Calendars'' by E. M. Reingold,  N. Dershowitz, and S. M. Clamen,
;; Software--Practice and Experience, Volume 23, Number 4 (April, 1993),
;; pages 383-404.

;; Hard copies of these two papers can be obtained by sending email to
;; reingold@cs.uiuc.edu with the SUBJECT "send-paper-cal" (no quotes) and
;; the message BODY containing your mailing address (snail).

;;; Code:

(defun calendar-version ()
  (interactive)
  (message "Version 5.3, January 25, 1994"))

;;;###autoload
(defvar calendar-week-start-day 0
  "*The day of the week on which a week in the calendar begins.
0 means Sunday (default), 1 means Monday, and so on.")

;;;###autoload
(defvar calendar-offset 0
  "*The offset of the principal month from the center of the calendar window.
0 means the principal month is in the center (default), -1 means on the left,
+1 means on the right.  Larger (or smaller) values push the principal month off
the screen.")

;;;###autoload
(defvar view-diary-entries-initially nil
  "*Non-nil means display current date's diary entries on entry.
The diary is displayed in another window when the calendar is first displayed,
if the current date is visible.  The number of days of diary entries displayed
is governed by the variable `number-of-diary-entries'.")

;;;###autoload
(defvar number-of-diary-entries 1
  "*Specifies how many days of diary entries are to be displayed initially.
This variable affects the diary display when the command M-x diary is used,
or if the value of the variable `view-diary-entries-initially' is t.  For
example, if the default value 1 is used, then only the current day's diary
entries will be displayed.  If the value 2 is used, then both the current
day's and the next day's entries will be displayed.

The value can also be a vector such as [0 2 2 2 2 4 1]; this value
says to display no diary entries on Sunday, the display the entries
for the current date and the day after on Monday through Thursday,
display Friday through Monday's entries on Friday, and display only
Saturday's entries on Saturday.

This variable does not affect the diary display with the `d' command
from the calendar; in that case, the prefix argument controls the
number of days of diary entries displayed.")

;;;###autoload
(defvar mark-diary-entries-in-calendar nil
  "*Non-nil means mark dates with diary entries, in the calendar window.
The marking symbol is specified by the variable `diary-entry-marker'.")

(defvar diary-entry-marker
  (if (not window-system)
      "+"
    (require 'faces)
    (make-face 'diary-face)
    (cond ((face-differs-from-default-p 'diary-face))
          ((x-display-color-p) (set-face-foreground 'diary-face "red"))
          (t (copy-face 'bold 'diary-face)))
    'diary-face)
  "*Used to mark dates that have diary entries.
Can be either a single-character string or a face.")

(defvar calendar-today-marker
  (if (not window-system)
      "="
    (require 'faces)
    (make-face 'calendar-today-face)
    (if (not (face-differs-from-default-p 'calendar-today-face))
        (set-face-underline-p 'calendar-today-face t))
    'calendar-today-face)
  "*Used to mark today's date.
Can be either a single-character string or a face.")

(defvar calendar-holiday-marker
  (if (not window-system)
      "*"
    (require 'faces)
    (make-face 'holiday-face)
    (cond ((face-differs-from-default-p 'holiday-face))
          ((x-display-color-p) (set-face-background 'holiday-face "pink"))
          (t (set-face-background 'holiday-face "black")
             (set-face-foreground 'holiday-face "white")))
    'holiday-face)
  "*Used to mark notable dates in the calendar.
Can be either a single-character string or a face.")

;;;###autoload
(defvar view-calendar-holidays-initially nil
  "*Non-nil means display holidays for current three month period on entry.
The holidays are displayed in another window when the calendar is first
displayed.")

;;;###autoload
(defvar mark-holidays-in-calendar nil
  "*Non-nil means mark dates of holidays in the calendar window.
The marking symbol is specified by the variable `calendar-holiday-marker'.")

;;;###autoload
(defvar all-hebrew-calendar-holidays nil
  "*If nil, show only major holidays from the Hebrew calendar.
This means only those Jewish holidays that appear on secular calendars.

If t, show all the holidays that would appear in a complete Hebrew calendar.")

;;;###autoload
(defvar all-christian-calendar-holidays nil
  "*If nil, show only major holidays from the Christian calendar.
This means only those Christian holidays that appear on secular calendars.

If t, show all the holidays that would appear in a complete Christian
calendar.")

;;;###autoload
(defvar all-islamic-calendar-holidays nil
  "*If nil, show only major holidays from the Islamic calendar.
This means only those Islamic holidays that appear on secular calendars.

If t, show all the holidays that would appear in a complete Islamic
calendar.")

;;;###autoload
(defvar calendar-load-hook nil
  "*List of functions to be called after the calendar is first loaded.
This is the place to add key bindings to `calendar-mode-map'.")

;;;###autoload
(defvar initial-calendar-window-hook nil
  "*List of functions to be called when the calendar window is first opened.
The functions invoked are called after the calendar window is opened, but
once opened is never called again.  Leaving the calendar with the `q' command
and reentering it will cause these functions to be called again.")

;;;###autoload
(defvar today-visible-calendar-hook nil
  "*List of functions called whenever the current date is visible.
This can be used, for example, to replace today's date with asterisks; a
function `calendar-star-date' is included for this purpose:
    (setq today-visible-calendar-hook 'calendar-star-date)
It can also be used to mark the current date with `calendar-today-marker';
a function is also provided for this:
    (setq today-visible-calendar-hook 'calendar-mark-today)

The corresponding variable `today-invisible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is not visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks.")

;;;###autoload
(defvar today-invisible-calendar-hook nil
  "*List of functions called whenever the current date is not visible.

The corresponding variable `today-visible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks.")

;;;###autoload
(defvar diary-file "~/diary"
  "*Name of the file in which one's personal diary of dates is kept.

The file's entries are lines in any of the forms

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR
            DAYNAME

at the beginning of the line; the remainder of the line is the diary entry
string for that date.  MONTH and DAY are one or two digit numbers, YEAR is
a number and may be written in full or abbreviated to the final two digits.
If the date does not contain a year, it is generic and applies to any year.
DAYNAME entries apply to any date on which is on that day of the week.
MONTHNAME and DAYNAME can be spelled in full, abbreviated to three
characters (with or without a period), capitalized or not.  Any of DAY,
MONTH, or MONTHNAME, YEAR can be `*' which matches any day, month, or year,
respectively.

The European style (in which the day precedes the month) can be used
instead, if you execute `european-calendar' when in the calendar, or set
`european-calendar-style' to t in your .emacs file.  The European forms are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

To revert to the default American style from the European style, execute
`american-calendar' in the calendar.

A diary entry can be preceded by the character
`diary-nonmarking-symbol' (ordinarily `&') to make that entry
nonmarking--that is, it will not be marked on dates in the calendar
window but will appear in a diary window.

Multiline diary entries are made by indenting lines after the first with
either a TAB or one or more spaces.

Lines not in one the above formats are ignored.  Here are some sample diary
entries (in the default American style):

     12/22/1988 Twentieth wedding anniversary!!
     &1/1. Happy New Year!
     10/22 Ruth's birthday.
     21: Payday
     Tuesday--weekly meeting with grad students at 10am
              Supowit, Shen, Bitner, and Kapoor to attend.
     1/13/89 Friday the thirteenth!!
     &thu 4pm squash game with Lloyd.
     mar 16 Dad's birthday
     April 15, 1989 Income tax due.
     &* 15 time cards due.

If the first line of a diary entry consists only of the date or day name with
no trailing blanks or punctuation, then that line is not displayed in the
diary window; only the continuation lines is shown.  For example, the
single diary entry

     02/11/1989
      Bill Blattner visits Princeton today
      2pm Cognitive Studies Committee meeting
      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
      4:00pm Jamie Tappenden
      7:30pm Dinner at George and Ed's for Alan Ryan
      7:30-10:00pm dance at Stewart Country Day School

will appear in the diary window without the date line at the beginning.  This
facility allows the diary window to look neater, but can cause confusion if
used with more than one day's entries displayed.

Diary entries can be based on Lisp sexps.  For example, the diary entry

      %%(diary-block 11 1 1990 11 10 1990) Vacation

causes the diary entry \"Vacation\" to appear from November 1 through November
10, 1990.  Other functions available are `diary-float', `diary-anniversary',
`diary-cyclic', `diary-day-of-year', `diary-iso-date', `diary-french-date',
`diary-hebrew-date', `diary-islamic-date', `diary-mayan-date',
`diary-yahrzeit', `diary-sunrise-sunset', `diary-phases-of-moon',
`diary-parasha', `diary-omer', `diary-rosh-hodesh', and
`diary-sabbath-candles'.  See the documentation for the function
`list-sexp-diary-entries' for more details.

Diary entries based on the Hebrew and/or the Islamic calendar are also
possible, but because these are somewhat slow, they are ignored
unless you set the `nongregorian-diary-listing-hook' and the
`nongregorian-diary-marking-hook' appropriately.  See the documentation
for these functions for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `list-diary-entries-hook'.")

;;;###autoload
(defvar diary-nonmarking-symbol "&"
  "*Symbol indicating that a diary entry is not to be marked in the calendar.")

;;;###autoload
(defvar hebrew-diary-entry-symbol "H"
  "*Symbol indicating a diary entry according to the Hebrew calendar.")

;;;###autoload
(defvar islamic-diary-entry-symbol "I"
  "*Symbol indicating a diary entry according to the Islamic calendar.")

;;;###autoload
(defvar diary-include-string "#include"
  "*The string indicating inclusion of another file of diary entries.
See the documentation for the function `include-other-diary-files'.")

;;;###autoload
(defvar sexp-diary-entry-symbol "%%"
  "*The string used to indicate a sexp diary entry in diary-file.
See the documentation for the function `list-sexp-diary-entries'.")

;;;###autoload
(defvar abbreviated-calendar-year t
  "*Interpret a two-digit year DD in a diary entry as either 19DD or 20DD.
For the Gregorian calendar; similarly for the Hebrew and Islamic calendars.
If this variable is nil, years must be written in full.")

;;;###autoload
(defvar european-calendar-style nil
  "*Use the European style of dates in the diary and in any displays.
If this variable is t, a date 1/2/1990 would be interpreted as February 1,
1990.  The accepted European date styles are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

Names can be capitalized or not, written in full, or abbreviated to three
characters with or without a period.")

;;;###autoload
(defvar american-date-diary-pattern
  '((month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W"))
  "*List of pseudo-patterns describing the American patterns of date used.
See the documentation of `diary-date-forms' for an explanation.")

;;;###autoload
(defvar european-date-diary-pattern
  '((day "/" month "[^/0-9]")
    (day "/" month "/" year "[^0-9]")
    (backup day " *" monthname "\\W+\\<[^*0-9]")
    (day " *" monthname " *" year "[^0-9]")
    (dayname "\\W"))
  "*List of pseudo-patterns describing the European patterns of date used.
See the documentation of `diary-date-forms' for an explanation.")

(defvar diary-date-forms
  (if european-calendar-style
      european-date-diary-pattern
    american-date-diary-pattern)
  "*List of pseudo-patterns describing the forms of date used in the diary.
The patterns on the list must be MUTUALLY EXCLUSIVE and must should not match
any portion of the diary entry itself, just the date component.

A pseudo-pattern is a list of regular expressions and the keywords `month',
`day', `year', `monthname', and `dayname'.  The keyword `monthname' will
match the name of the month, capitalized or not, or its three-letter
abbreviation, followed by a period or not; it will also match `*'.
Similarly, `dayname' will match the name of the day, capitalized or not, or
its three-letter abbreviation, followed by a period or not.  The keywords
`month', `day', and `year' will match those numerical values, preceded by
arbitrarily many zeros; they will also match `*'.

The matching of the diary entries with the date forms is done with the
standard syntax table from Fundamental mode, but with the `*' changed so
that it is a word constituent.

If, to be mutually exclusive, a pseudo-pattern must match a portion of the
diary entry itself, the first element of the pattern MUST be `backup'.  This
directive causes the date recognizer to back up to the beginning of the
current word of the diary entry, so in no case can the pattern match more than
a portion of the first word of the diary entry.")

;;;###autoload
(defvar european-calendar-display-form
  '((if dayname (concat dayname ", ")) day " " monthname " " year)
  "*Pseudo-pattern governing the way a date appears in the European style.
See the documentation of calendar-date-display-form for an explanation.")

;;;###autoload
(defvar american-calendar-display-form
  '((if dayname (concat dayname ", ")) monthname " " day ", " year)
  "*Pseudo-pattern governing the way a date appears in the American style.
See the documentation of `calendar-date-display-form' for an explanation.")

(defvar calendar-date-display-form
  (if european-calendar-style
      european-calendar-display-form
    american-calendar-display-form)
  "*Pseudo-pattern governing the way a date appears.

Used by the function `calendar-date-string', a pseudo-pattern is a list of
expressions that can involve the keywords `month', `day', and `year', all
numbers in string form, and `monthname' and `dayname', both alphabetic
strings.  For example, the ISO standard would use the pseudo- pattern

       '(year \"-\" month \"-\" day)

while a typical American form would be

       '(month \"/\" day \"/\" (substring year -2))

and

       '((format \"%9s, %9s %2s, %4s\" dayname monthname day year))

would give the usual American style in fixed-length fields.

See the documentation of the function `calendar-date-string'.")

(defun european-calendar ()
  "Set the interpretation and display of dates to the European style."
  (interactive)
  (setq european-calendar-style t)
  (setq calendar-date-display-form european-calendar-display-form)
  (setq diary-date-forms european-date-diary-pattern)
  (update-calendar-mode-line))

(defun american-calendar ()
  "Set the interpretation and display of dates to the American style."
  (interactive)
  (setq european-calendar-style nil)
  (setq calendar-date-display-form american-calendar-display-form)
  (setq diary-date-forms american-date-diary-pattern)
  (update-calendar-mode-line))

;;;###autoload
(defvar print-diary-entries-hook 'lpr-buffer
  "*List of functions called after a temporary diary buffer is prepared.
The buffer shows only the diary entries currently visible in the diary
buffer.  The default just does the printing.  Other uses might include, for
example, rearranging the lines into order by day and time, saving the buffer
instead of deleting it, or changing the function used to do the printing.")

;;;###autoload
(defvar list-diary-entries-hook nil
  "*List of functions called after diary file is culled for relevant entries.
It is to be used for diary entries that are not found in the diary file.

A function `include-other-diary-files' is provided for use as the value of
this hook.  This function enables you to use shared diary files together
with your own.  The files included are specified in the diary file by lines
of the form

        #include \"filename\"

This is recursive; that is, #include directives in files thus included are
obeyed.  You can change the \"#include\" to some other string by changing
the variable `diary-include-string'.  When you use `include-other-diary-files'
as part of the list-diary-entries-hook, you will probably also want to use the
function `mark-included-diary-files' as part of `mark-diary-entries-hook'.

For example, you could use

     (setq list-diary-entries-hook
       '(include-other-diary-files sort-diary-entries))
     (setq diary-display-hook 'fancy-diary-display)

in your `.emacs' file to cause the fancy diary buffer to be displayed with
diary entries from various included files, each day's entries sorted into
lexicographic order.")

;;;###autoload
(defvar diary-hook nil
  "*List of functions called after the display of the diary.
Can be used for appointment notification.")

;;;###autoload
(defvar diary-display-hook nil
  "*List of functions that handle the display of the diary.
If nil (the default), `simple-diary-display' is used.  Use `ignore' for no
diary display.

Ordinarily, this just displays the diary buffer (with holidays indicated in
the mode line), if there are any relevant entries.  At the time these
functions are called, the variable `diary-entries-list' is a list, in order
by date, of all relevant diary entries in the form of ((MONTH DAY YEAR)
STRING), where string is the diary entry for the given date.  This can be
used, for example, a different buffer for display (perhaps combined with
holidays), or produce hard copy output.

A function `fancy-diary-display' is provided as an alternative
choice for this hook; this function prepares a special noneditable diary
buffer with the relevant diary entries that has neat day-by-day arrangement
with headings.  The fancy diary buffer will show the holidays unless the
variable `holidays-in-diary-buffer' is set to nil.  Ordinarily, the fancy
diary buffer will not show days for which there are no diary entries, even
if that day is a holiday; if you want such days to be shown in the fancy
diary buffer, set the variable `diary-list-include-blanks' to t.")

;;;###autoload
(defvar nongregorian-diary-listing-hook nil
  "*List of functions called for listing diary file and included files.
As the files are processed for diary entries, these functions are used to cull
relevant entries.  You can use either or both of `list-hebrew-diary-entries'
and `list-islamic-diary-entries'.  The documentation for these functions
describes the style of such diary entries.")

;;;###autoload
(defvar mark-diary-entries-hook nil
  "*List of functions called after marking diary entries in the calendar.

A function `mark-included-diary-files' is also provided for use as the
mark-diary-entries-hook; it enables you to use shared diary files together
with your own.  The files included are specified in the diary file by lines
of the form
        #include \"filename\"
This is recursive; that is, #include directives in files thus included are
obeyed.  You can change the \"#include\" to some other string by changing the
variable `diary-include-string'.  When you use `mark-included-diary-files' as
part of the mark-diary-entries-hook, you will probably also want to use the
function `include-other-diary-files' as part of `list-diary-entries-hook'.")

;;;###autoload
(defvar nongregorian-diary-marking-hook nil
  "*List of functions called for marking diary file and included files.
As the files are processed for diary entries, these functions are used to cull
relevant entries.  You can use either or both of `mark-hebrew-diary-entries'
and `mark-islamic-diary-entries'.  The documentation for these functions
describes the style of such diary entries.")

;;;###autoload
(defvar diary-list-include-blanks nil
  "*If nil, do not include days with no diary entry in the list of diary entries.
Such days will then not be shown in the the fancy diary buffer, even if they
are holidays.")

;;;###autoload
(defvar holidays-in-diary-buffer t
  "*Non-nil means include holidays in the diary display.
The holidays appear in the mode line of the diary buffer, or in the
fancy diary buffer next to the date.  This slows down the diary functions
somewhat; setting it to nil makes the diary display faster.")

(defvar calendar-mark-ring nil)

;;;###autoload
(put 'general-holidays 'risky-local-variable t)
;;;###autoload
(defvar general-holidays
  '((holiday-fixed 1 1 "New Year's Day")
    (holiday-float 1 1 3 "Martin Luther King Day")
    (holiday-fixed 2 2 "Ground Hog Day")
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-float 2 1 3 "President's Day")
    (holiday-fixed 3 17 "St. Patrick's Day")
    (holiday-fixed 4 1 "April Fool's Day")
    (holiday-float 5 0 2 "Mother's Day")
    (holiday-float 5 1 -1 "Memorial Day")
    (holiday-fixed 6 14 "Flag Day")
    (holiday-float 6 0 3 "Father's Day")
    (holiday-fixed 7 4 "Independence Day")
    (holiday-float 9 1 1 "Labor Day")
    (holiday-float 10 1 2 "Columbus Day")
    (holiday-fixed 10 31 "Halloween")
    (holiday-fixed 11 11 "Veteran's Day")
    (holiday-float 11 4 4 "Thanksgiving"))
  "*General holidays.  Default value is for the United States.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'local-holidays 'risky-local-variable t)
;;;###autoload
(defvar local-holidays nil
  "*Local holidays.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'other-holidays 'risky-local-variable t)
;;;###autoload
(defvar other-holidays nil
  "*User defined holidays.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'hebrew-holidays-1 'risky-local-variable t)
;;;###autoload
(defvar hebrew-holidays-1
  '((holiday-rosh-hashanah-etc)
    (if all-hebrew-calendar-holidays
        (holiday-julian
         11
         (let* ((m displayed-month)
                (y displayed-year)
                (year))
           (increment-calendar-month m y -1)
           (let ((year (extract-calendar-year
                        (calendar-julian-from-absolute
                         (calendar-absolute-from-gregorian
                          (list m 1 y))))))
             (if (zerop (% (1+ year) 4))
                 22
               21))) "\"Tal Umatar\" (evening)"))))

;;;###autoload
(put 'hebrew-holidays-2 'risky-local-variable t)
;;;###autoload
(defvar hebrew-holidays-2
  '((if all-hebrew-calendar-holidays
        (holiday-hanukkah)
      (holiday-hebrew 9 25 "Hanukkah"))
    (if all-hebrew-calendar-holidays
      (holiday-hebrew
       10
       (let ((h-year (extract-calendar-year
                      (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (list displayed-month 28 displayed-year))))))
         (if (= (% (calendar-absolute-from-hebrew (list 10 10 h-year))
                   7)
                6)
             11 10))
       "Tzom Teveth"))
    (if all-hebrew-calendar-holidays
        (holiday-hebrew 11 15 "Tu B'Shevat"))))

;;;###autoload
(put 'hebrew-holidays-3 'risky-local-variable t)
;;;###autoload
(defvar hebrew-holidays-3
  '((if all-hebrew-calendar-holidays
        (holiday-hebrew
         11
         (let ((m displayed-month)
               (y displayed-year))
           (increment-calendar-month m y 1)
           (let* ((h-year (extract-calendar-year
                           (calendar-hebrew-from-absolute
                            (calendar-absolute-from-gregorian
                             (list m
                                   (calendar-last-day-of-month m y)
                                   y)))))
                  (s-s
                   (calendar-hebrew-from-absolute
                    (if (=
                         (% (calendar-absolute-from-hebrew
                             (list 7 1 h-year))
                            7)
                         6)
                        (calendar-dayname-on-or-before
                         6 (calendar-absolute-from-hebrew
                            (list 11 17 h-year)))
                      (calendar-dayname-on-or-before
                       6 (calendar-absolute-from-hebrew
                          (list 11 16 h-year))))))
                  (day (extract-calendar-day s-s)))
             day))
         "Shabbat Shirah"))))

;;;###autoload
(put 'hebrew-holidays-4 'risky-local-variable t)
;;;###autoload
(defvar hebrew-holidays-4
  '((holiday-passover-etc)
    (if (and all-hebrew-calendar-holidays
             (let* ((m displayed-month)
                    (y displayed-year)
                    (year))
               (increment-calendar-month m y -1)
               (let ((year (extract-calendar-year
                            (calendar-julian-from-absolute
                             (calendar-absolute-from-gregorian
                              (list m 1 y))))))
                 (= 21 (% year 28)))))
        (holiday-julian 3 26 "Kiddush HaHamah"))
    (if all-hebrew-calendar-holidays
        (holiday-tisha-b-av-etc))))

;;;###autoload
(put 'hebrew-holidays 'risky-local-variable t)
;;;###autoload
(defvar hebrew-holidays (append hebrew-holidays-1 hebrew-holidays-2
				hebrew-holidays-3 hebrew-holidays-4)
  "*Jewish holidays.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'christian-holidays 'risky-local-variable t)
;;;###autoload
(defvar christian-holidays
  '((if all-christian-calendar-holidays
        (holiday-fixed 1 6 "Epiphany"))
    (holiday-easter-etc)
    (if all-christian-calendar-holidays
        (holiday-greek-orthodox-easter))
    (if all-christian-calendar-holidays
        (holiday-fixed 8 15 "Assumption"))
    (if all-christian-calendar-holidays
        (holiday-advent))
    (holiday-fixed 12 25 "Christmas")
    (if all-christian-calendar-holidays
        (holiday-julian 12 25 "Eastern Orthodox Christmas")))
  "*Christian holidays.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'islamic-holidays 'risky-local-variable t)
;;;###autoload
(defvar islamic-holidays
  '((holiday-islamic
     1 1
     (format "Islamic New Year %d"
             (let ((m displayed-month)
                   (y displayed-year))
               (increment-calendar-month m y 1)
               (extract-calendar-year
                (calendar-islamic-from-absolute
                 (calendar-absolute-from-gregorian
                  (list
                   m (calendar-last-day-of-month m y) y)))))))
    (if all-islamic-calendar-holidays
        (holiday-islamic 1 10 "Ashura"))
    (if all-islamic-calendar-holidays
        (holiday-islamic 3 12 "Mulad-al-Nabi"))
    (if all-islamic-calendar-holidays
        (holiday-islamic 7 26 "Shab-e-Mi'raj"))
    (if all-islamic-calendar-holidays
        (holiday-islamic 8 15 "Shab-e-Bara't"))
    (holiday-islamic 9 1 "Ramadan Begins")
    (if all-islamic-calendar-holidays
        (holiday-islamic 9 27 "Shab-e Qadr"))
    (if all-islamic-calendar-holidays
        (holiday-islamic 10 1 "Id-al-Fitr"))
    (if all-islamic-calendar-holidays
        (holiday-islamic 12 10 "Id-al-Adha")))
  "*Islamic holidays.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'solar-holidays 'risky-local-variable t)
;;;###autoload
(defvar solar-holidays
  '((if (fboundp 'atan)
	(solar-equinoxes-solstices))
    (if (progn
	  (require 'cal-dst)
	  t)
      (funcall
       'holiday-sexp
        calendar-daylight-savings-starts
        '(format "Daylight Savings Time Begins %s"
                  (if (fboundp 'atan)
                      (solar-time-string
                       (/ calendar-daylight-savings-starts-time (float 60))
                       calendar-standard-time-zone-name)
                    ""))))
    (funcall
     'holiday-sexp
     calendar-daylight-savings-ends
     '(format "Daylight Savings Time Ends %s"
              (if (fboundp 'atan)
                  (solar-time-string
                   (/ calendar-daylight-savings-ends-time (float 60))
                   calendar-daylight-time-zone-name)
                ""))))
  "*Sun-related holidays.
See the documentation for `calendar-holidays' for details.")

;;;###autoload
(put 'calendar-holidays 'risky-local-variable t)
(defvar calendar-holidays
  (append general-holidays local-holidays other-holidays
          christian-holidays hebrew-holidays islamic-holidays
          solar-holidays)
  "*List of notable days for the command M-x holidays.

Additional holidays are easy to add to the list, just put them in the list
`other-holidays' in your .emacs file.  Similarly, by setting any of
`general-holidays', `local-holidays' `christian-holidays', `hebrew-holidays',
`islamic-holidays', or `solar-holidays' to nil in your .emacs file, you can
eliminate unwanted categories of holidays.  The intention is that (in the US)
`local-holidays' be set in site-init.el and `other-holidays' be set by the
user.

Entries on the list are expressions that return (possibly empty) lists of
items of the form ((month day year) string) of a holiday in the in the
three-month period centered around `displayed-month' of `displayed-year'.
Several basic functions are provided for this purpose:

    (holiday-fixed MONTH DAY STRING) is a fixed date on the Gregorian calendar
    (holiday-float MONTH DAYNAME K STRING &optional day) is the Kth DAYNAME in
                               MONTH on the Gregorian calendar (0 for Sunday,
                               etc.); K<0 means count back from the end of the
                               month. An optional parameter DAY means the Kth
                               DAYNAME after/before MONTH DAY.
    (holiday-hebrew MONTH DAY STRING)  a fixed date on the Hebrew calendar
    (holiday-islamic MONTH DAY STRING) a fixed date on the Islamic calendar
    (holiday-julian MONTH DAY STRING)  a fixed date on the Julian calendar
    (holiday-sexp SEXP STRING) SEXP is a Gregorian-date-valued expression
                               in the variable `year'; if it evaluates to
                               a visible date, that's the holiday; if it
                               evaluates to nil, there's no holiday.  STRING
                               is an expression in the variable `date'.

For example, to add Bastille Day, celebrated in France on July 14, add

     (holiday-fixed 7 14 \"Bastille Day\")

to the list.  To add Hurricane Supplication Day, celebrated in the Virgin
Islands on the fourth Monday in August, add

     (holiday-float 8 1 4 \"Hurricane Supplication Day\")

to the list (the last Monday would be specified with `-1' instead of `4').
To add the last day of Hanukkah to the list, use

     (holiday-hebrew 10 2 \"Last day of Hanukkah\")

since the Hebrew months are numbered with 1 starting from Nisan, while to
add the Islamic feast celebrating Mohammed's birthday use

     (holiday-islamic 3 12 \"Mohammed's Birthday\")

since the Islamic months are numbered from 1 starting with Muharram.  To
add Thomas Jefferson's birthday, April 2, 1743 (Julian), use

     (holiday-julian 4 2 \"Jefferson's Birthday\")

To include a holiday conditionally, use the sexp form or a conditional.  For
example, to include American presidential elections, which occur on the first
Tuesday after the first Monday in November of years divisible by 4, add

     (holiday-sexp
       (if (zerop (% year 4))
           (calendar-gregorian-from-absolute
             (1+ (calendar-dayname-on-or-before
                   1 (+ 6 (calendar-absolute-from-gregorian
                            (list 11 1 year)))))))
       \"US Presidential Election\")

or

     (if (zerop (% displayed-year 4))
         (holiday-fixed 11
                (extract-calendar-day
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-dayname-on-or-before
                       1 (+ 6 (calendar-absolute-from-gregorian
                               (list 11 1 displayed-year)))))))
                \"US Presidential Election\"))

to the list.  To include the phases of the moon, add

     (lunar-phases)

to the holiday list, where `lunar-phases' is an Emacs-Lisp function that
you've written to return a (possibly empty) list of the relevant VISIBLE dates
with descriptive strings such as

     (((2 6 1989) \"New Moon\") ((2 12 1989) \"First Quarter Moon\") ... ).")

(defconst calendar-buffer "*Calendar*"
  "Name of the buffer used for the calendar.")

(defconst holiday-buffer "*Holidays*"
  "Name of the buffer used for the displaying the holidays.")

(defconst fancy-diary-buffer "*Fancy Diary Entries*"
  "Name of the buffer used for the optional fancy display of the diary.")

(defconst lunar-phases-buffer "*Phases of Moon*"
  "Name of the buffer used for the lunar phases.")

(defmacro increment-calendar-month (mon yr n)
  "Move the variables MON and YR to the month and year by N months.
Forward if N is positive or backward if N is negative."
  (` (let (( macro-y (+ (* (, yr) 12) (, mon) -1 (, n) )))
       (setq (, mon) (1+ (% macro-y 12) ))
       (setq (, yr) (/ macro-y 12)))))

(defmacro calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop."
  (` (let (( (, var) (1- (, init)) ))
       (while (>= (, final) (setq (, var) (1+ (, var))))
         (,@ body)))))

(defmacro calendar-sum (index initial condition expression)
  "For INDEX = INITIAL et seq, as long as CONDITION holds, sum EXPRESSION."
  (` (let (( (, index) (, initial))
             (sum 0))
       (while (, condition)
         (setq sum (+ sum (, expression) ))
         (setq (, index) (1+ (, index))))
       sum)))

;; The following are in-line for speed; they can be called thousands of times
;; when looking up holidays or processing the diary.  Here, for example, are
;; the numbers of calls to calendar/diary/holiday functions in preparing the
;; fancy diary display, for a moderately complex diary file, with functions
;; used instead of macros.  There were a total of 10000 such calls:
;;
;;  1934   extract-calendar-month
;;  1852   extract-calendar-year
;;  1819   extract-calendar-day
;;   845   calendar-leap-year-p
;;   837   calendar-day-number
;;   775   calendar-absolute-from-gregorian
;;   346   calendar-last-day-of-month
;;   286   hebrew-calendar-last-day-of-month
;;   188   hebrew-calendar-leap-year-p
;;   180   hebrew-calendar-elapsed-days
;;   163   hebrew-calendar-last-month-of-year
;;    66   calendar-date-compare
;;    65   hebrew-calendar-days-in-year
;;    60   calendar-absolute-from-julian
;;    50   calendar-absolute-from-hebrew
;;    43   calendar-date-equal
;;    38   calendar-gregorian-from-absolute
;;     .
;;     .
;;     .
;;
;; The use of these seven macros eliminates the overhead of 92% of the function
;; calls; it's faster this way.

(defsubst extract-calendar-month (date)
  "Extract the month part of DATE which has the form (month day year)."
  (car date))

(defsubst extract-calendar-day (date)
  "Extract the day part of DATE which has the form (month day year)."
  (car (cdr date)))

(defsubst extract-calendar-year (date)
  "Extract the year part of DATE which has the form (month day year)."
  (car (cdr (cdr date))))

(defsubst calendar-leap-year-p (year)
  "Returns t if YEAR is a Gregorian leap year."
  (and (zerop (% year 4))
       (or (not (zerop (% year 100)))
           (zerop (% year 400)))))

;; The foregoing is a bit faster, but not as clear as the following:
;;
;;(defsubst calendar-leap-year-p (year)
;;  "Returns t if YEAR is a Gregorian leap year."
;;  (or
;;    (and (=  (% year   4) 0)
;;         (/= (% year 100) 0))
;;    (= (% year 400) 0)))

(defsubst calendar-last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (= month 2) (calendar-leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

;; An explanation of the calculation can be found in PascAlgorithms by
;; Edward and Ruth Reingold, Scott-Foresman/Little, Brown, 1988.

(defsubst calendar-day-number (date)
  "Return the day number within the year of the date DATE.
For example, (calendar-day-number '(1 1 1987)) returns the value 1,
while (calendar-day-number '(12 31 1980)) returns 366."
    (let* ((month (extract-calendar-month date))
           (day (extract-calendar-day date))
           (year (extract-calendar-year date))
         (day-of-year (+ day (* 31 (1- month)))))
      (if (> month 2)
          (progn
            (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
            (if (calendar-leap-year-p year)
                (setq day-of-year (1+ day-of-year)))))
      day-of-year))

(defsubst calendar-absolute-from-gregorian (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((prior-years (1- (extract-calendar-year date))))
    (+ (calendar-day-number date);; Days this year
       (* 365 prior-years);;        + Days in prior years
       (/ prior-years 4);;          + Julian leap years
       (- (/ prior-years 100));;    - century years
       (/ prior-years 400))));;     + Gregorian leap years

;;;###autoload
(defun calendar (&optional arg)
  "Display a three-month calendar in another window.
The three months appear side by side, with the current month in the middle
surrounded by the previous and next months.  The cursor is put on today's date.

If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file; appropriate setting
of the variable `view-diary-entries-initially' will cause the diary entries for
the current date to be displayed in another window.  The value of the variable
`number-of-diary-entries' controls the number of days of diary entries
displayed upon initial display of the calendar.

An optional prefix argument ARG causes the calendar displayed to be ARG
months in the future if ARG is positive or in the past if ARG is negative;
in this case the cursor goes on the first day of the month.

Once in the calendar window, future or past months can be moved into view.
Arbitrary months can be displayed, or the calendar can be scrolled forward
or backward.

The cursor can be moved forward or backward by one day, one week, one month,
or one year.  All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the digit keys
and the minus sign are automatically prefixes.  The window is replotted as
necessary to display the desired date.

Diary entries can be marked on the calendar or displayed in another window.

Use M-x describe-mode for details of the key bindings in the calendar window.

The Gregorian calendar is assumed.

After loading the calendar, the hooks given by the variable
`calendar-load-hook' are run.  This is the place to add key bindings to the
calendar-mode-map.

After preparing the calendar window initially, the hooks given by the variable
`initial-calendar-window-hook' are run.

The hooks given by the variable `today-visible-calendar-hook' are run
everytime the calendar window gets scrolled, if the current date is visible
in the window.  If it is not visible, the hooks given by the variable
`today-invisible-calendar-hook' are run.  Thus, for example, setting
`today-visible-calendar-hook' to 'calendar-star-date will cause today's date
to be replaced by asterisks to highlight it whenever it is in the window."
  (interactive "P")
  (set-buffer (get-buffer-create calendar-buffer))
  (calendar-mode)
  (let* ((pop-up-windows t)
         (split-height-threshold 1000)
         (date (if arg
                   (calendar-read-date t)
                 (calendar-current-date)))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date)))
    (pop-to-buffer calendar-buffer)
    (increment-calendar-month month year (- calendar-offset))
    (generate-calendar-window month year)
    (if (and view-diary-entries-initially (calendar-date-is-visible-p date))
        (view-diary-entries
         (if (vectorp number-of-diary-entries)
             (aref number-of-diary-entries (calendar-day-of-week date))
           number-of-diary-entries))))
  (let* ((diary-buffer (get-file-buffer diary-file))
         (diary-window (if diary-buffer (get-buffer-window diary-buffer)))
         (split-height-threshold (if diary-window 2 1000)))
    (if view-calendar-holidays-initially
        (list-calendar-holidays)))
  (run-hooks 'initial-calendar-window-hook))

(autoload 'view-diary-entries "diary-lib"
  "Prepare and display a buffer with diary entries.
Searches your diary file for entries that match ARG days starting with
the date indicated by the cursor position in the displayed three-month
calendar."
  t)

(autoload 'calendar-sunrise-sunset "solar"
  "Local time of sunrise and sunset for date under cursor."
  t)

(autoload 'calendar-phases-of-moon "lunar"
  "Create a buffer of the phases of the moon for the current calendar window."
  t)

(autoload 'calendar-print-french-date "cal-french"
  "Show the French Revolutionary calendar equivalent of the date under the cursor."
  t)

(autoload 'calendar-goto-french-date "cal-french"
 "Move cursor to French Revolutionary date."
  t)

(autoload 'calendar-french-date-string "cal-french"
  "String of French Revolutionary date of Gregorian DATE."
  t)

(autoload 'calendar-mayan-date-string "cal-mayan"
  "String of Mayan date of Gregorian DATE."
  t)

(autoload 'calendar-print-mayan-date "cal-mayan"
  "Show the Mayan long count, Tzolkin, and Haab equivalents of the date under the cursor."
  t)

(autoload 'calendar-goto-mayan-long-count-date "cal-mayan"
 "Move cursor to Mayan long count date."
  t)

(autoload 'calendar-next-haab-date "cal-mayan"
  "Move cursor to next instance of Mayan Haab date."
  t)

(autoload 'calendar-previous-haab-date "cal-mayan"
  "Move cursor to previous instance of Mayan Haab date."
  t)

(autoload 'calendar-next-tzolkin-date "cal-mayan"
  "Move cursor to next instance of Mayan Tzolkin date."
  t)

(autoload 'calendar-previous-tzolkin-date "cal-mayan"
  "Move cursor to previous instance of Mayan Tzolkin date."
  t)

(autoload 'calendar-next-calendar-round-date "cal-mayan"
  "Move cursor to next instance of Mayan Haab/Tzoklin combination."
  t)

(autoload 'calendar-previous-calendar-round-date "cal-mayan"
  "Move cursor to previous instance of Mayan Haab/Tzoklin combination."
  t)

(autoload 'show-all-diary-entries "diary-lib"
  "Show all of the diary entries in the diary file.
This function gets rid of the selective display of the diary file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created."
  t)

(autoload 'mark-diary-entries "diary-lib"
  "Mark days in the calendar window that have diary entries.
Each entry in diary file visible in the calendar window is marked."
  t)

(autoload 'insert-diary-entry "diary-ins"
  "Insert a diary entry for the date indicated by point."
  t)

(autoload 'insert-weekly-diary-entry "diary-ins"
  "Insert a weekly diary entry for the day of the week indicated by point."
  t)


(autoload 'insert-monthly-diary-entry "diary-ins"
  "Insert a monthly diary entry for the day of the month indicated by point."
  t)

(autoload 'insert-yearly-diary-entry "diary-ins"
  "Insert an annual diary entry for the day of the year indicated by point."
  t)

(autoload 'insert-anniversary-diary-entry "diary-ins"
  "Insert an anniversary diary entry for the date indicated by point."
  t)

(autoload 'insert-block-diary-entry "diary-ins"
  "Insert a block diary entry for the dates indicated by point and mark."
  t)

(autoload 'insert-cyclic-diary-entry "diary-ins"
  "Insert a cyclic diary entry starting at the date indicated by point."
  t)

(autoload 'insert-hebrew-diary-entry "diary-ins"
  "Insert a diary entry for the Hebrew date corresponding to the date
indicated by point."
  t)

(autoload 'insert-monthly-hebrew-diary-entry "diary-ins"
  "Insert a monthly diary entry for the day of the Hebrew month corresponding
to the date indicated by point."
  t)

(autoload 'insert-yearly-hebrew-diary-entry "diary-ins"
  "Insert an annual diary entry for the day of the Hebrew year corresponding
to the date indicated by point."
  t)

(autoload 'insert-islamic-diary-entry "diary-ins"
  "Insert a diary entry for the Islamic date corresponding to the date
indicated by point."
  t)

(autoload 'insert-monthly-islamic-diary-entry "diary-ins"
  "Insert a monthly diary entry for the day of the Islamic month corresponding
to the date indicated by point."
  t)

(autoload 'insert-yearly-islamic-diary-entry "diary-ins"
  "Insert an annual diary entry for the day of the Islamic year corresponding
to the date indicated by point."
  t)

(autoload 'list-calendar-holidays "holidays"
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.  Returns t if any
holidays are found, nil if not."
  t)

(autoload 'mark-calendar-holidays "holidays"
  "Mark notable days in the calendar window."
  t)

(autoload 'calendar-cursor-holidays "holidays"
  "Find holidays for the date specified by the cursor in the calendar window."
  t)

(defun generate-calendar-window (&optional mon yr)
  "Generate the calendar window for the current date.
Or, for optional MON, YR."
  (let* ((buffer-read-only nil)
         (today (calendar-current-date))
         (month (extract-calendar-month today))
         (day (extract-calendar-day today))
         (year (extract-calendar-year today))
         (today-visible
          (or (not mon)
              (let ((offset (calendar-interval mon yr month year)))
                (and (<= offset 1) (>= offset -1)))))
         (day-in-week (calendar-day-of-week today)))
    (update-calendar-mode-line)
    (if mon
        (generate-calendar mon yr)
        (generate-calendar month year))
    (calendar-cursor-to-visible-date
     (if today-visible today (list displayed-month 1 displayed-year)))
    (set-buffer-modified-p nil)
    (or (one-window-p t)
        (/= (frame-width) (window-width))
        (shrink-window (- (window-height) 9)))
    (sit-for 0)
    (and mark-holidays-in-calendar
         (mark-calendar-holidays)
         (sit-for 0))
    (unwind-protect
        (if mark-diary-entries-in-calendar (mark-diary-entries))
      (if today-visible
          (run-hooks 'today-visible-calendar-hook)
        (run-hooks 'today-invisible-calendar-hook)))))

(defun generate-calendar (month year)
  "Generate a three-month Gregorian calendar centered around MONTH, YEAR."
  (if (< (+ month (* 12 (1- year))) 2)
      (error "Months before February, 1 AD are not available."))
  (setq displayed-month month)
  (setq displayed-year year)
  (erase-buffer)
  (increment-calendar-month month year -1)
  (calendar-for-loop i from 0 to 2 do
       (generate-calendar-month month year (+ 5 (* 25 i)))
       (increment-calendar-month month year 1)))

(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (let* ((blank-days;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
	 (last (calendar-last-day-of-month month year)))
   (goto-char (point-min))
   (calendar-insert-indented
    (calendar-string-spread
     (list (format "%s %d" (calendar-month-name month) year)) ?  20)
    indent t)
   (calendar-insert-indented "" indent);; Go to proper spot
   (calendar-for-loop i from 0 to 6 do
      (insert (substring (aref calendar-day-name-array 
                               (mod (+ calendar-week-start-day i) 7))
                         0 2))
      (insert " "))
   (calendar-insert-indented "" 0 t);; Force onto following line
   (calendar-insert-indented "" indent);; Go to proper spot
   ;; Add blank days before the first of the month
   (calendar-for-loop i from 1 to blank-days do (insert "   "))
   ;; Put in the days of the month
   (calendar-for-loop i from 1 to last do
      (insert (format "%2d " i))
      (put-text-property (- (point) (if (< i 10) 2 3)) (1- (point))
			 'mouse-face 'highlight)
      (and (zerop (mod (+ i blank-days) 7))
           (/= i last)
           (calendar-insert-indented "" 0 t)    ;; Force onto following line
           (calendar-insert-indented "" indent)))));; Go to proper spot

(defun calendar-insert-indented (string indent &optional newline)
  "Insert STRING at column INDENT.
If the optional parameter NEWLINE is t, leave point at start of next line,
inserting a newline if there was no next line; otherwise, leave point after
the inserted text.  Value is always t."
  ;; Try to move to that column.
  (move-to-column indent)
  ;; If line is too short, indent out to that column.
  (if (< (current-column) indent)
      (indent-to indent))
  (insert string)
  ;; Advance to next line, if requested.
  (if newline
      (progn
	(end-of-line)
	(if (eobp)
            (newline)
	  (forward-line 1))))
  t)

(defun redraw-calendar ()
  "Redraw the calendar display."
  (interactive)
  (let ((cursor-date (calendar-cursor-to-date)))
    (generate-calendar-window displayed-month displayed-year)
    (calendar-cursor-to-visible-date cursor-date)))

(defvar calendar-debug-sexp nil
  "*Turn debugging on when evaluating a sexp in the diary or holiday list.")

(defvar calendar-mode-map nil)
(if calendar-mode-map
    nil
  (setq calendar-mode-map (make-sparse-keymap))
  (if window-system (require 'cal-menu))
  (calendar-for-loop i from 0 to 9 do
       (define-key calendar-mode-map (int-to-string i) 'digit-argument))
  (let ((l (list 'narrow-to-region 'mark-word 'mark-sexp 'mark-paragraph
                 'mark-defun 'mark-whole-buffer 'mark-page
                 'downcase-region 'upcase-region 'kill-region
                 'copy-region-as-kill 'capitalize-region 'write-region)))
    (while l
      (substitute-key-definition (car l) 'calendar-not-implemented
				 calendar-mode-map global-map)
      (setq l (cdr l))))
  (define-key calendar-mode-map "-"     'negative-argument)
  (define-key calendar-mode-map "\C-x>" 'scroll-calendar-right)
  (define-key calendar-mode-map [prior] 'scroll-calendar-right-three-months)
  (define-key calendar-mode-map "\ev"   'scroll-calendar-right-three-months)
  (define-key calendar-mode-map "\C-x<" 'scroll-calendar-left)
  (define-key calendar-mode-map [next]  'scroll-calendar-left-three-months)
  (define-key calendar-mode-map "\C-v"  'scroll-calendar-left-three-months)
  (define-key calendar-mode-map "\C-b"  'calendar-backward-day)
  (define-key calendar-mode-map "\C-p"  'calendar-backward-week)
  (define-key calendar-mode-map "\e{"   'calendar-backward-month)
  (define-key calendar-mode-map "\C-x[" 'calendar-backward-year)
  (define-key calendar-mode-map "\C-f"  'calendar-forward-day)
  (define-key calendar-mode-map "\C-n"  'calendar-forward-week)
  (define-key calendar-mode-map [left]  'calendar-backward-day)
  (define-key calendar-mode-map [up]    'calendar-backward-week)
  (define-key calendar-mode-map [right] 'calendar-forward-day)
  (define-key calendar-mode-map [down]  'calendar-forward-week)
  (define-key calendar-mode-map "\e}"   'calendar-forward-month)
  (define-key calendar-mode-map "\C-x]" 'calendar-forward-year)
  (define-key calendar-mode-map "\C-a"  'calendar-beginning-of-week)
  (define-key calendar-mode-map "\C-e"  'calendar-end-of-week)
  (define-key calendar-mode-map "\ea"   'calendar-beginning-of-month)
  (define-key calendar-mode-map "\ee"   'calendar-end-of-month)
  (define-key calendar-mode-map "\e<"   'calendar-beginning-of-year)
  (define-key calendar-mode-map "\e>"   'calendar-end-of-year)
  (define-key calendar-mode-map "\C-@"  'calendar-set-mark)
  ;; Many people are used to typing C-SPC and getting C-@.
  (define-key calendar-mode-map [?\C-\ ] 'calendar-set-mark)
  (define-key calendar-mode-map "\C-x\C-x" 'calendar-exchange-point-and-mark)
  (define-key calendar-mode-map "\e=" 'calendar-count-days-region)
  (define-key calendar-mode-map "gd"  'calendar-goto-date)
  (define-key calendar-mode-map "gj"  'calendar-goto-julian-date)
  (define-key calendar-mode-map "ga"  'calendar-goto-astro-day-number)
  (define-key calendar-mode-map "gh"  'calendar-goto-hebrew-date)
  (define-key calendar-mode-map "gi"  'calendar-goto-islamic-date)
  (define-key calendar-mode-map "gc"  'calendar-goto-iso-date)
  (define-key calendar-mode-map "gf"  'calendar-goto-french-date)
  (define-key calendar-mode-map "gml"  'calendar-goto-mayan-long-count-date)
  (define-key calendar-mode-map "gmpc" 'calendar-previous-calendar-round-date)
  (define-key calendar-mode-map "gmnc" 'calendar-next-calendar-round-date)
  (define-key calendar-mode-map "gmph" 'calendar-previous-haab-date)
  (define-key calendar-mode-map "gmnh" 'calendar-next-haab-date)
  (define-key calendar-mode-map "gmpt" 'calendar-previous-tzolkin-date)
  (define-key calendar-mode-map "gmnt" 'calendar-next-tzolkin-date)
  (define-key calendar-mode-map "S"   'calendar-sunrise-sunset)
  (define-key calendar-mode-map "M"   'calendar-phases-of-moon)
  (define-key calendar-mode-map " "   'scroll-other-window)
  (define-key calendar-mode-map "\C-c\C-l" 'redraw-calendar)
  (define-key calendar-mode-map "."   'calendar-goto-today)
  (define-key calendar-mode-map "o"   'calendar-other-month)
  (define-key calendar-mode-map "q"   'exit-calendar)
  (define-key calendar-mode-map "a"   'list-calendar-holidays)
  (define-key calendar-mode-map "h"   'calendar-cursor-holidays)
  (define-key calendar-mode-map "x"   'mark-calendar-holidays)
  (define-key calendar-mode-map "u"   'calendar-unmark)
  (define-key calendar-mode-map "m"   'mark-diary-entries)
  (define-key calendar-mode-map "d"   'view-diary-entries)
  (define-key calendar-mode-map "D"   'view-other-diary-entries)
  (define-key calendar-mode-map "s"   'show-all-diary-entries)
  (define-key calendar-mode-map "pd"  'calendar-print-day-of-year)
  (define-key calendar-mode-map "pc"  'calendar-print-iso-date)
  (define-key calendar-mode-map "pj"  'calendar-print-julian-date)
  (define-key calendar-mode-map "pa"  'calendar-print-astro-day-number)
  (define-key calendar-mode-map "ph"  'calendar-print-hebrew-date)
  (define-key calendar-mode-map "pi"  'calendar-print-islamic-date)
  (define-key calendar-mode-map "pf"  'calendar-print-french-date)
  (define-key calendar-mode-map "pm"  'calendar-print-mayan-date)
  (define-key calendar-mode-map "id"  'insert-diary-entry)
  (define-key calendar-mode-map "iw"  'insert-weekly-diary-entry)
  (define-key calendar-mode-map "im"  'insert-monthly-diary-entry)
  (define-key calendar-mode-map "iy"  'insert-yearly-diary-entry)
  (define-key calendar-mode-map "ia"  'insert-anniversary-diary-entry)
  (define-key calendar-mode-map "ib"  'insert-block-diary-entry)
  (define-key calendar-mode-map "ic"  'insert-cyclic-diary-entry)
  (define-key calendar-mode-map "ihd" 'insert-hebrew-diary-entry)
  (define-key calendar-mode-map "ihm" 'insert-monthly-hebrew-diary-entry)
  (define-key calendar-mode-map "ihy" 'insert-yearly-hebrew-diary-entry)
  (define-key calendar-mode-map "iid" 'insert-islamic-diary-entry)
  (define-key calendar-mode-map "iim" 'insert-monthly-islamic-diary-entry)
  (define-key calendar-mode-map "iiy" 'insert-yearly-islamic-diary-entry)
  (define-key calendar-mode-map "?"   'calendar-goto-info-node))

(defun describe-calendar-mode ()
  "Create a help buffer with a brief description of the calendar-mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ
     (format
      "Calendar Mode:\nFor a complete description, type %s\n%s\n"
      (substitute-command-keys
       "\\<calendar-mode-map>\\[describe-mode] from within the calendar")
      (substitute-command-keys "\\{calendar-mode-map}")))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

;; Calendar mode is suitable only for specially formatted data.
(put 'calendar-mode 'mode-class 'special)

(defvar calendar-mode-line-format
  (list
   (substitute-command-keys "\\<calendar-mode-map>\\[scroll-calendar-left]")
   "Calendar"
   (substitute-command-keys "\\<calendar-mode-map>\\[calendar-goto-info-node] info/\\[calendar-other-month] other/\\[calendar-goto-today] today")
   '(calendar-date-string (calendar-current-date) t)
   (substitute-command-keys "\\<calendar-mode-map>\\[scroll-calendar-right]"))
  "The mode line of the calendar buffer.")

(defun calendar-goto-info-node ()
  "Go to the info node for the calendar."
  (interactive)
  (require 'info)
  (let ((where (Info-find-emacs-command-nodes 'calendar)))
    (if (not where)
        (error "Couldn't find documentation for the calendar.")
      (save-window-excursion (info))
      (pop-to-buffer "*info*")
      (Info-find-node (car (car where)) (car (cdr (car where)))))))

(defun calendar-mode ()
  "A major mode for the calendar window.

For a complete description, type \
\\<calendar-mode-map>\\[calendar-goto-info-node] from within the calendar.

\\<calendar-mode-map>\\{calendar-mode-map}"

  (kill-all-local-variables)
  (setq major-mode 'calendar-mode)
  (setq mode-name "Calendar")
  (use-local-map calendar-mode-map)
  (setq buffer-read-only t)
  (setq indent-tabs-mode nil)
  (update-calendar-mode-line)
  (make-local-variable 'calendar-mark-ring)
  (make-local-variable 'displayed-month);;  Month in middle of window.
  (make-local-variable 'displayed-year));;  Year in middle of window.

(defun calendar-string-spread (strings char length)
  "Concatenate list of STRINGS separated with copies of CHAR to fill LENGTH.
The effect is like mapconcat but the separating pieces are as balanced as
possible.  Each item of STRINGS is evaluated before concatenation so it can
actually be an expression that evaluates to a string.  If LENGTH is too short,
the STRINGS are just concatenated and the result truncated."
;; The algorithm is based on equation (3.25) on page 85 of Concrete
;; Mathematics by Ronald L. Graham, Donald E. Knuth, and Oren Patashnik,
;; Addison-Wesley, Reading, MA, 1989
  (let* ((strings (mapcar 'eval
                          (if (< (length strings) 2)
                              (append (list "") strings (list ""))
                            strings)))
         (n (- length (length (apply 'concat strings))))
         (m (1- (length strings)))
         (s (car strings))
         (strings (cdr strings))
         (i 0))
    (while strings
      (setq s (concat s
                      (make-string (max 0 (/ (+ n i) m)) char)
                      (car strings)))
      (setq i (1+ i))
      (setq strings (cdr strings)))
    (substring s 0 length)))

(defun update-calendar-mode-line ()
  "Update the calendar mode line with the current date and date style."
  (if (bufferp (get-buffer calendar-buffer))
      (save-excursion
        (set-buffer calendar-buffer)
        (setq mode-line-format
              (calendar-string-spread
               calendar-mode-line-format ?  (frame-width))))))

(defun calendar-window-list ()
  "List of all calendar-related windows."
  (let ((calendar-buffers (calendar-buffer-list))
        list)
    (walk-windows '(lambda (w)
                     (if (memq (window-buffer w) calendar-buffers)
                         (setq list (cons w list))))
                  nil t)
    list))

(defun calendar-buffer-list ()
  "List of all calendar-related buffers."
  (let* ((diary-buffer (get-file-buffer diary-file))
         (buffers (list "*Yahrzeits*" lunar-phases-buffer holiday-buffer
                        fancy-diary-buffer diary-buffer calendar-buffer))
         (buffer-list nil)
         b)
    (while buffers
      (setq b (car buffers))
      (setq b (cond ((stringp b) (get-buffer b))
                    ((bufferp b) b)
                    (t nil)))
      (if b (setq buffer-list (cons b buffer-list)))
      (setq buffers (cdr buffers)))
    buffer-list))

(defun exit-calendar ()
  "Get out of the calendar window and hide it and related buffers."
  (interactive)
  (let* ((diary-buffer (get-file-buffer diary-file)))
    (if (and diary-buffer (buffer-modified-p diary-buffer)
	     (not
	      (yes-or-no-p
	       "Diary modified; do you really want to exit the calendar? ")))
	(error)
      ;; Need to do this multiple times because one time can replace some
      ;; calendar-related buffers with other calendar-related buffers
      (mapcar (lambda (x)
                (mapcar 'calendar-hide-window (calendar-window-list)))
              (calendar-window-list)))))

(defun calendar-hide-window (window)
  "Hide WINDOW if it is calendar-related."
  (let ((buffer (if (window-live-p window) (window-buffer window))))
    (if (memq buffer (calendar-buffer-list))
        (cond
         ((and window-system
               (eq 'icon (cdr (assoc 'visibility
                                     (frame-parameters
                                      (window-frame window))))))
          nil)
         ((and window-system (window-dedicated-p window))
          (iconify-frame (window-frame window)))
         ((not (and (select-window window) (one-window-p window)))
          (delete-window window))
         (t (set-buffer buffer)
            (bury-buffer))))))

(defun calendar-goto-today ()
  "Reposition the calendar window so the current date is visible."
  (interactive)
  (let ((today (calendar-current-date)));; The date might have changed.
    (if (not (calendar-date-is-visible-p today))
        (generate-calendar-window)
      (update-calendar-mode-line)
      (calendar-cursor-to-visible-date today))))

(defun calendar-forward-month (arg)
  "Move the cursor forward ARG months.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((cursor-date (calendar-cursor-to-date t))
         (month (extract-calendar-month cursor-date))
         (day (extract-calendar-day cursor-date))
         (year (extract-calendar-year cursor-date)))
    (increment-calendar-month month year arg)
    (let ((last (calendar-last-day-of-month month year)))
      (if (< last day)
        (setq day last)))
    ;; Put the new month on the screen, if needed, and go to the new date.
    (let ((new-cursor-date (list month day year)))
      (if (not (calendar-date-is-visible-p new-cursor-date))
          (calendar-other-month month year))
      (calendar-cursor-to-visible-date new-cursor-date))))

(defun calendar-forward-year (arg)
  "Move the cursor forward by ARG years.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (* 12 arg)))

(defun calendar-backward-month (arg)
  "Move the cursor backward by ARG months.
Movement is forward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (- arg)))

(defun calendar-backward-year (arg)
  "Move the cursor backward ARG years.
Movement is forward is ARG is negative."
  (interactive "p")
  (calendar-forward-month (* -12 arg)))

(defun scroll-calendar-left (arg)
  "Scroll the displayed calendar left by ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((old-date (calendar-cursor-to-date))
        (today (calendar-current-date)))
    (if (/= arg 0)
        (progn
          (increment-calendar-month displayed-month displayed-year arg)
          (generate-calendar-window displayed-month displayed-year)
          (calendar-cursor-to-visible-date
           (cond
            ((calendar-date-is-visible-p old-date) old-date)
            ((calendar-date-is-visible-p today) today)
            (t (list displayed-month 1 displayed-year))))))))

(defun scroll-calendar-right (arg)
  "Scroll the displayed calendar window right by ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (scroll-calendar-left (- arg)))

(defun scroll-calendar-left-three-months (arg)
  "Scroll the displayed calendar window left by 3*ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (scroll-calendar-left (* 3 arg)))

(defun scroll-calendar-right-three-months (arg)
  "Scroll the displayed calendar window right by 3*ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (scroll-calendar-left (* -3 arg)))

(defun calendar-current-date ()
  "Returns the current date in a list (month day year)."
  (let ((s (current-time-string)))
    (list (length (member (substring s 4 7)
                          '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
                            "Jun" "May" "Apr" "Mar" "Feb" "Jan")))
          (string-to-number (substring s 8 10))
          (string-to-number (substring s 20 24)))))

(defun calendar-cursor-to-date (&optional error)
  "Returns a list (month day year) of current cursor position.
If cursor is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (let* ((segment (/ (current-column) 25))
         (month (% (+ displayed-month segment -1) 12))
         (month (if (= 0 month) 12 month))
         (year
          (cond
           ((and (=  12 month) (= segment 0)) (1- displayed-year))
           ((and (=   1 month) (= segment 2)) (1+ displayed-year))
           (t displayed-year))))
    (if (and (looking-at "[0-9]")
             (< 2 (count-lines (point-min) (point))))
        (save-excursion
          (re-search-backward "[^0-9]")
          (list month
                (string-to-int (buffer-substring (1+ (point)) (+ 4 (point))))
                year))
      (if (looking-at "\\*")
          (save-excursion
            (re-search-backward "[^*]")
            (if (looking-at ".\\*\\*")
                (list month calendar-starred-day year)
              (if error (error "Not on a date!"))))
        (if error (error "Not on a date!"))))))

(defun calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (let ((date (calendar-cursor-to-date))
        (column (current-column)))
    (if date
        date
      (if (> 3 (count-lines (point-min) (point)))
          (progn
            (goto-line 3)
            (move-to-column column)))
      (if (not (looking-at "[0-9]"))
          (if (and (not (looking-at " *$"))
                   (or (< column 25)
                       (and (> column 27)
                            (< column 50))
                       (and (> column 52)
                            (< column 75))))
              (progn
                (re-search-forward "[0-9]" nil t)
                (backward-char 1))
            (re-search-backward "[0-9]" nil t)))
      (calendar-cursor-to-date))))

(defun calendar-forward-day (arg)
  "Move the cursor forward ARG days.
Moves backward if ARG is negative."
  (interactive "p")
  (if (/= 0 arg)
      (let*
          ((cursor-date (calendar-cursor-to-date))
           (cursor-date (if cursor-date
                            cursor-date
                          (if (> arg 0) (setq arg (1- arg)))
                          (calendar-cursor-to-nearest-date)))
           (new-cursor-date
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian cursor-date) arg)))
           (new-display-month (extract-calendar-month new-cursor-date))
           (new-display-year (extract-calendar-year new-cursor-date)))
        ;; Put the new month on the screen, if needed, and go to the new date.
        (if (not (calendar-date-is-visible-p new-cursor-date))
            (calendar-other-month new-display-month new-display-year))
        (calendar-cursor-to-visible-date new-cursor-date))))

(defun calendar-backward-day (arg)
  "Move the cursor back ARG days.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (- arg)))

(defun calendar-forward-week (arg)
  "Move the cursor forward ARG weeks.
Moves backward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg 7)))

(defun calendar-backward-week (arg)
  "Move the cursor back ARG weeks.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg -7)))

(defun calendar-beginning-of-week (arg)
  "Move the cursor back ARG calendar-week-start-day's."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-backward-day
     (if (= day calendar-week-start-day)
         (* 7 arg)
       (+ (mod (- day calendar-week-start-day) 7)
          (* 7 (1- arg)))))))

(defun calendar-end-of-week (arg)
  "Move the cursor forward ARG calendar-week-start-day+6's."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-forward-day
     (if (= day (mod (1- calendar-week-start-day) 7))
         (* 7 arg)
       (+ (- 6 (mod (- day calendar-week-start-day) 7))
          (* 7 (1- arg)))))))

(defun calendar-beginning-of-month (arg)
  "Move the cursor backward ARG month beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date)))
    (if (= day 1)
        (calendar-backward-month arg)
      (calendar-cursor-to-visible-date (list month 1 year))
      (calendar-backward-month (1- arg)))))

(defun calendar-end-of-month (arg)
  "Move the cursor forward ARG month ends."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (last-day (calendar-last-day-of-month month year)))
    (if (/= day last-day)
        (progn
          (calendar-cursor-to-visible-date (list month last-day year))
          (setq arg (1- arg))))
    (increment-calendar-month month year arg)
    (let ((last-day (list
                     month
                     (calendar-last-day-of-month month year)
                     year)))
      (if (not (calendar-date-is-visible-p last-day))
          (calendar-other-month month year)
      (calendar-cursor-to-visible-date last-day)))))

(defun calendar-beginning-of-year (arg)
  "Move the cursor backward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (jan-first (list 1 1 year)))
    (if (and (= day 1) (= 1 month))
        (calendar-backward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p jan-first))
          (calendar-cursor-to-visible-date jan-first)
        (calendar-other-month 1 (- year (1- arg)))))))

(defun calendar-end-of-year (arg)
  "Move the cursor forward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (dec-31 (list 12 31 year)))
    (if (and (= day 31) (= 12 month))
        (calendar-forward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p dec-31))
          (calendar-cursor-to-visible-date dec-31)
        (calendar-other-month 12 (- year (1- arg)))
        (calendar-cursor-to-visible-date (list 12 31 displayed-year))))))

;; The following version of calendar-gregorian-from-absolute is preferred for
;; reasons of clarity, BUT it's much slower than the version that follows it.

;;(defun calendar-gregorian-from-absolute (date)
;;  "Compute the list (month day year) corresponding to the absolute DATE.
;;The absolute date is the number of days elapsed since the (imaginary)
;;Gregorian date Sunday, December 31, 1 BC."
;;  (let* ((approx (/ date 366));; Approximation from below.
;;         (year                ;; Search forward from the approximation.
;;          (+ approx
;;             (calendar-sum y approx
;;                 (>= date (calendar-absolute-from-gregorian (list 1 1 (1+ y))))
;;                  1)))
;;         (month         ;; Search forward from January.
;;          (1+ (calendar-sum m 1
;;                   (> date
;;                      (calendar-absolute-from-gregorian
;;                       (list m (calendar-last-day-of-month m year) year)))
;;                   1)))
;;         (day           ;; Calculate the day by subtraction.
;;          (- date
;;             (1- (calendar-absolute-from-gregorian (list month 1 year))))))
;;    (list month day year)))

(defun calendar-gregorian-from-absolute (date)
  "Compute the list (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
;; See the footnote on page 384 of ``Calendrical Calculations, Part II:
;; Three Historical Calendars'' by E. M. Reingold,  N. Dershowitz, and S. M.
;; Clamen, Software--Practice and Experience, Volume 23, Number 4
;; (April, 1993), pages 383-404 for an explanation.
  (let* ((d0 (1- date))
         (n400 (/ d0 146097))
         (d1 (% d0 146097))
         (n100 (/ d1 36524))
         (d2 (% d1 36524))
         (n4 (/ d2 1461))
         (d3 (% d2 1461))
         (n1 (/ d3 365))
         (day (1+ (% d3 365)))
         (year (+ (* 400 n400) (* 100 n100) (* n4 4) n1)))
    (if (or (= n100 4) (= n1 4))
        (list 12 31 year)
      (let ((year (1+ year))
            (month 1))
        (while (let ((mdays (calendar-last-day-of-month month year)))
                 (and (< mdays day)
                      (setq day (- day mdays))))
          (setq month (1+ month)))
        (list month day year)))))

(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let* ((month (extract-calendar-month date))
	 (day (extract-calendar-day date))
	 (year (extract-calendar-year date))
	 (first-of-month-weekday (calendar-day-of-week (list month 1 year))))
    (goto-line (+ 3
		  (/ (+ day  -1
                        (mod
                         (- (calendar-day-of-week (list month 1 year))
                            calendar-week-start-day)
                         7))
                     7)))
    (move-to-column (+ 6
		       (* 25
			  (1+ (calendar-interval
			       displayed-month displayed-year month year)))
		       (* 3 (mod
                             (- (calendar-day-of-week date)
                                calendar-week-start-day)
                             7))))))

(defun calendar-other-month (month year)
  "Display a three-month calendar centered around MONTH and YEAR."
  (interactive (calendar-read-date 'noday))
  (if (and (= month displayed-month)
           (= year displayed-year))
      nil
    (let ((old-date (calendar-cursor-to-date))
          (today (calendar-current-date)))
      (generate-calendar-window month year)
      (calendar-cursor-to-visible-date
       (cond
        ((calendar-date-is-visible-p old-date) old-date)
        ((calendar-date-is-visible-p today) today)
        (t (list month 1 year)))))))

(defun calendar-set-mark (arg)
  "Mark the date under the cursor, or jump to marked date.
With no prefix argument, push current date onto marked date ring.
With argument, jump to mark, pop it, and put point at end of ring."
  (interactive "P")
  (let ((date (calendar-cursor-to-date t)))
    (if (null arg)
        (progn
          (setq calendar-mark-ring (cons date calendar-mark-ring))
          ;; Since the top of the mark ring is the marked date in the
          ;; calendar, the mark ring in the calendar is one longer than
          ;; in other buffers to get the same effect.
          (if (> (length calendar-mark-ring) (1+ mark-ring-max))
              (setcdr (nthcdr mark-ring-max calendar-mark-ring) nil))
          (message "Mark set"))
      (if (null calendar-mark-ring)
          (error "No mark set in this buffer")
        (calendar-goto-date (car calendar-mark-ring))
        (setq calendar-mark-ring
              (cdr (nconc calendar-mark-ring (list date))))))))

(defun calendar-exchange-point-and-mark ()
  "Exchange the current cursor position with the marked date."
  (interactive)
  (let ((mark (car calendar-mark-ring))
        (date (calendar-cursor-to-date t)))
    (if (null mark)
        (error "No mark set in this buffer")
      (setq calendar-mark-ring (cons date (cdr calendar-mark-ring)))
      (calendar-goto-date mark))))

(defun calendar-count-days-region ()
  "Count the number of days (inclusive) between point and the mark."
  (interactive)
  (let* ((days (- (calendar-absolute-from-gregorian
                   (calendar-cursor-to-date t))
                  (calendar-absolute-from-gregorian
                   (or (car calendar-mark-ring)
                       (error "No mark set in this buffer")))))
         (days (1+ (if (> days 0) days (- days)))))
    (message "Region has %d day%s (inclusive)"
             days (if (> days 1) "s" ""))))

(defun calendar-not-implemented ()
  "Not implemented."
  (interactive)
  (error "%s not available in the calendar"
         (global-key-binding (this-command-keys))))

(defun calendar-read (prompt acceptable &optional initial-contents)
  "Return an object read from the minibuffer.
Prompt with the string PROMPT and use the function ACCEPTABLE to decide if
entered item is acceptable.  If non-nil, optional third arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading."
  (let ((value (read-minibuffer prompt initial-contents)))
    (while (not (funcall acceptable value))
      (setq value (read-minibuffer prompt initial-contents)))
    value))

(defun calendar-read-date (&optional noday)
  "Prompt for Gregorian date.  Returns a list (month day year).
If optional NODAY is t, does not ask for day, but just returns
(month nil year); if NODAY is any other non-nil value the value returned is
(month year) "
  (let* ((year (calendar-read
                "Year (>0): "
                '(lambda (x) (> x 0))
                (int-to-string (extract-calendar-year
                                (calendar-current-date)))))
         (month-array calendar-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc
                      (capitalize
                       (completing-read
                        "Month name: "
                        (mapcar 'list (append month-array nil))
                        nil t))
                      (calendar-make-alist month-array 1 'capitalize))))
         (last (calendar-last-day-of-month month year)))
    (if noday
        (if (eq noday t)
            (list month nil year)
          (list month year))
      (list month
            (calendar-read (format "Day (1-%d): " last)
                                   '(lambda (x) (and (< 0 x) (<= x last))))
            year))))

(defun calendar-goto-date (date)
  "Move cursor to DATE."
  (interactive (list (calendar-read-date)))
  (let ((month (extract-calendar-month date))
        (year (extract-calendar-year date)))
    (if (not (calendar-date-is-visible-p date))
        (calendar-other-month
         (if (and (= month 1) (= year 1))
             2
           month)
         year)))
  (calendar-cursor-to-visible-date date))

(defun calendar-goto-julian-date (date &optional noecho)
  "Move cursor to Julian DATE; echo Julian date unless NOECHO is t."
  (interactive
   (let* ((today (calendar-current-date))
          (year (calendar-read
                 "Julian calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-julian-from-absolute
                    (calendar-absolute-from-gregorian
                     today))))))
          (month-array calendar-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Julian calendar month name: "
                         (mapcar 'list (append month-array nil))
                         nil t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last 
           (if (and (zerop (% year 4)) (= month 2))
               29
             (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))
          (day (calendar-read
                (format "Julian calendar day (%d-%d): "
                        (if (and (= year 1) (= month 1)) 3 1) last)
                '(lambda (x) 
                   (and (< (if (and (= year 1) (= month 1)) 2 0) x)
                        (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-julian date)))
  (or noecho (calendar-print-julian-date)))

(defun calendar-goto-hebrew-date (date &optional noecho)
  "Move cursor to Hebrew DATE; echo Hebrew date unless NOECHO is t."
  (interactive
   (let* ((today (calendar-current-date))
          (year (calendar-read
                 "Hebrew calendar year (>3760): "
                 '(lambda (x) (> x 3760))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-hebrew-from-absolute
                    (calendar-absolute-from-gregorian today))))))
          (month-array (if (hebrew-calendar-leap-year-p year)
                           calendar-hebrew-month-name-array-leap-year
                         calendar-hebrew-month-name-array-common-year))
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Hebrew calendar month name: "
                         (mapcar 'list (append month-array nil))
                         (if (= year 3761)
                             '(lambda (x)
                                (let ((m (cdr
                                          (assoc
                                           (car x)
                                           (calendar-make-alist
                                            month-array)))))
                                  (< 0
                                     (calendar-absolute-from-hebrew
                                      (list m
                                            (hebrew-calendar-last-day-of-month
                                             m year)
                                            year))))))
                                 
                         t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last (hebrew-calendar-last-day-of-month month year))
          (first (if (and (= year 3761) (= month 10))
                     18 1))
          (day (calendar-read
                (format "Hebrew calendar day (%d-%d): "
                        first last)
                '(lambda (x) (and (<= first x) (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-hebrew date)))
  (or noecho (calendar-print-hebrew-date)))

(defun calendar-goto-islamic-date (date &optional noecho)
  "Move cursor to Islamic DATE; echo Islamic date unless NOECHO is t."
  (interactive
   (let* ((today (calendar-current-date))
          (year (calendar-read
                 "Islamic calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-islamic-from-absolute
                    (calendar-absolute-from-gregorian today))))))
          (month-array calendar-islamic-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Islamic calendar month name: "
                         (mapcar 'list (append month-array nil))
                         nil t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last (islamic-calendar-last-day-of-month month year))
          (day (calendar-read
                (format "Islamic calendar day (1-%d): " last)
                '(lambda (x) (and (< 0 x) (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic date)))
  (or noecho (calendar-print-islamic-date)))

(defun calendar-goto-iso-date (date &optional noecho)
  "Move cursor to ISO DATE; echo ISO date unless NOECHO is t."
  (interactive
   (let* ((today (calendar-current-date))
          (year (calendar-read
                 "ISO calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string (extract-calendar-year today))))
          (no-weeks (extract-calendar-month
                     (calendar-iso-from-absolute
                      (1-
                       (calendar-dayname-on-or-before
                        1 (calendar-absolute-from-gregorian
                           (list 1 4 (1+ year))))))))
          (week (calendar-read
                 (format "ISO calendar week (1-%d): " no-weeks)
                 '(lambda (x) (and (> x 0) (<= x no-weeks)))))
          (day (calendar-read
                "ISO day (1-7): "
                '(lambda (x) (and (<= 1 x) (<= x 7))))))
     (list (list week day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-iso date)))
  (or noecho (calendar-print-iso-date)))

(defun calendar-interval (mon1 yr1 mon2 yr2)
  "The number of months difference between MON1, YR1 and MON2, YR2."
  (+ (* 12 (- yr2 yr1))
     (- mon2 mon1)))

(defun calendar-day-name (date)
  "Returns a string with the name of the day of the week of DATE."
  (aref calendar-day-name-array (calendar-day-of-week date)))

(defvar calendar-day-name-array
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(defvar calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])

(defun calendar-make-alist (sequence &optional start-index filter)
  "Make an assoc list corresponding to SEQUENCE.
Start at index 1, unless optional START-INDEX is provided.
If FILTER is provided, apply it to each item in the list."
  (let ((index (if start-index (1- start-index) 0)))
    (mapcar
     '(lambda (x)
        (setq index (1+ index))
        (cons (if filter (funcall filter x) x)
              index))
     (append sequence nil))))

(defun calendar-month-name (month)
  "The name of MONTH."
  (aref calendar-month-name-array (1- month)))

(defun calendar-day-of-week (date)
  "Returns the day-of-the-week index of DATE, 0 for Sunday, 1 for Monday, etc."
  (% (calendar-absolute-from-gregorian date) 7))

(defun calendar-unmark ()
  "Delete all diary/holiday marks/highlighting from the calendar."
  (interactive)
  (setq mark-holidays-in-calendar nil)
  (setq mark-diary-entries-in-calendar nil)
  (redraw-calendar))

(defun calendar-date-is-visible-p (date)
  "Returns t if DATE is legal and is visible in the calendar window."
  (let ((gap (calendar-interval
              displayed-month displayed-year
              (extract-calendar-month date) (extract-calendar-year date))))
    (and (calendar-date-is-legal-p date) (> 2 gap) (< -2 gap))))

(defun calendar-date-is-legal-p (date)
  "Returns t if DATE is a legal date."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (and (<= 1 month) (<= month 12)
         (<= 1 day) (<= day (calendar-last-day-of-month month year))
         (<= 1 year))))

(defun calendar-date-equal (date1 date2)
  "Returns t if the DATE1 and DATE2 are the same."
  (and
   (= (extract-calendar-month date1) (extract-calendar-month date2))
   (= (extract-calendar-day date1) (extract-calendar-day date2))
   (= (extract-calendar-year date1) (extract-calendar-year date2))))

(defun mark-visible-calendar-date (date &optional mark)
  "Mark DATE in the calendar window with MARK.
MARK is either a single-character string or a face.
MARK defaults to diary-entry-marker."
  (if (calendar-date-is-legal-p date)
      (save-excursion
        (set-buffer calendar-buffer)
        (calendar-cursor-to-visible-date date)
        (let ((mark (or mark diary-entry-marker)))
          (if (stringp mark)
              (let ((buffer-read-only nil))
                (forward-char 1)
                (delete-char 1)
                (insert mark)
                (forward-char -2))
	    (overlay-put
             (make-overlay (1-(point)) (1+ (point))) 'face mark))))))

(defun calendar-star-date ()
  "Replace the date under the cursor in the calendar window with asterisks.
This function can be used with the today-visible-calendar-hook run after the
calendar window has been prepared."
  (let ((buffer-read-only nil))
    (make-variable-buffer-local 'calendar-starred-day)
    (forward-char 1)
    (setq calendar-starred-day
          (string-to-int
           (buffer-substring (point) (- (point) 2))))
    (delete-char -2)
    (insert "**")
    (backward-char 1)
    (set-buffer-modified-p nil)))

(defun calendar-mark-today ()
  "Mark the date under the cursor in the calendar window.
The date is marked with calendar-today-marker.  This function can be used with
the today-visible-calendar-hook run after the calendar window has been
prepared."
  (mark-visible-calendar-date
   (calendar-cursor-to-date)
   calendar-today-marker))

(defun calendar-date-compare (date1 date2)
  "Returns t if DATE1 is before DATE2, nil otherwise.
The actual dates are in the car of DATE1 and DATE2."
  (< (calendar-absolute-from-gregorian (car date1))
     (calendar-absolute-from-gregorian (car date2))))

(defun calendar-date-string (date &optional abbreviate nodayname)
  "A string form of DATE, driven by the variable `calendar-date-display-form'.
An optional parameter ABBREVIATE, when t, causes the month and day names to be
abbreviated to three characters.  An optional parameter NODAYNAME, when t,
omits the name of the day of the week."
  (let* ((dayname
          (if nodayname
              nil
            (if abbreviate
                (substring (calendar-day-name date) 0 3)
              (calendar-day-name date))))
         (month (extract-calendar-month date))
         (monthname
          (if abbreviate
              (substring
               (calendar-month-name month) 0 3)
            (calendar-month-name month)))
         (day (int-to-string (extract-calendar-day date)))
         (month (int-to-string month))
         (year (int-to-string (extract-calendar-year date))))
    (mapconcat 'eval calendar-date-display-form "")))

(defun calendar-dayname-on-or-before (dayname date)
  "Returns the absolute date of the DAYNAME on or before absolute DATE.
DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.

Note: Applying this function to d+6 gives us the DAYNAME on or after an
absolute day d.  Similarly, applying it to d+3 gives the DAYNAME nearest to
absolute date d, applying it to d-1 gives the DAYNAME previous to absolute
date d, and applying it to d+7 gives the DAYNAME following absolute date d."
  (- date (% (- date dayname) 7)))

(defun calendar-nth-named-absday (n dayname month year &optional day)
  "The absolute date of Nth DAYNAME in MONTH, YEAR before/after optional DAY.
A DAYNAME of 0 means Sunday, 1 means Monday, and so on.  If N<0,
return the Nth DAYNAME before MONTH DAY, YEAR (inclusive).
If N>0, return the Nth DAYNAME after MONTH DAY, YEAR (inclusive).

If DAY is omitted, it defaults to 1 if N>0, and MONTH's last day otherwise."
  (if (> n 0)
      (+ (* 7 (1- n))
	 (calendar-dayname-on-or-before
	  dayname
	  (+ 6 (calendar-absolute-from-gregorian
		(list month (or day 1) year)))))
    (+ (* 7 (1+ n))
       (calendar-dayname-on-or-before
	dayname
	(calendar-absolute-from-gregorian
	 (list month
	       (or day (calendar-last-day-of-month month year))
	       year))))))

(defun calendar-nth-named-day (n dayname month year &optional day)
  "The date of Nth DAYNAME in MONTH, YEAR before/after optional DAY.
A DAYNAME of 0 means Sunday, 1 means Monday, and so on.  If N<0,
return the Nth DAYNAME before MONTH DAY, YEAR (inclusive).
If N>0, return the Nth DAYNAME after MONTH DAY, YEAR (inclusive).

If DAY is omitted, it defaults to 1 if N>0, and MONTH's last day otherwise."
  (calendar-gregorian-from-absolute
   (calendar-nth-named-absday n dayname month year day)))

(defun calendar-day-of-year-string (&optional date)
  "String of day number of year of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((d (or date (calendar-current-date)))
         (year (extract-calendar-year d))
         (day (calendar-day-number d))
         (days-remaining (- (calendar-day-number (list 12 31 year)) day)))
    (format "Day %d of %d; %d day%s remaining in the year"
            day year days-remaining (if (= days-remaining 1) "" "s"))))

(defun calendar-print-day-of-year ()
  "Show day number in year/days remaining in year for date under the cursor."
  (interactive)
  (message (calendar-day-of-year-string (calendar-cursor-to-date t))))

(defun calendar-absolute-from-iso (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The `ISO year' corresponds approximately to the Gregorian year, but
weeks start on Monday and end on Sunday.  The first week of the ISO year is
the first such week in which at least 4 days are in a year.  The ISO
commercial DATE has the form (week day year) in which week is in the range
1..52 and day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 =
Sunday).  The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((week (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date)))
    (+ (calendar-dayname-on-or-before
        1 (+ 3 (calendar-absolute-from-gregorian (list 1 1 year))))
       (* 7 (1- week))
       (if (= day 0) 6 (1- day)))))

(defun calendar-iso-from-absolute (date)
  "Compute the `ISO commercial date' corresponding to the absolute DATE.
The ISO year corresponds approximately to the Gregorian year, but weeks
start on Monday and end on Sunday.  The first week of the ISO year is the
first such week in which at least 4 days are in a year.  The ISO commercial
date has the form (week day year) in which week is in the range 1..52 and
day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 = Sunday).  The
absolute date is the number of days elapsed since the (imaginary) Gregorian
date Sunday, December 31, 1 BC."
  (let* ((approx (extract-calendar-year
                  (calendar-gregorian-from-absolute (- date 3))))
         (year (+ approx
                  (calendar-sum y approx
                      (>= date (calendar-absolute-from-iso (list 1 1 (1+ y))))
                      1))))
    (list
     (1+ (/ (- date (calendar-absolute-from-iso (list 1 1 year))) 7))
     (% date 7)
     year)))

(defun calendar-iso-date-string (&optional date)
  "String of ISO date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((d (calendar-absolute-from-gregorian 
             (or date (calendar-current-date))))
         (day (% d 7))
         (iso-date (calendar-iso-from-absolute d)))
    (format "Day %s of week %d of %d"
            (if (zerop day) 7 day)
            (extract-calendar-month iso-date)
            (extract-calendar-year iso-date))))

(defun calendar-print-iso-date ()
  "Show equivalent ISO date for the date under the cursor."
  (interactive)
  (message "ISO date: %s"
           (calendar-iso-date-string (calendar-cursor-to-date t))))

(defun calendar-julian-from-absolute (date)
  "Compute the Julian (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((approx (/ (+ date 2) 366));; Approximation from below.
         (year        ;; Search forward from the approximation.
          (+ approx
             (calendar-sum y approx
                (>= date (calendar-absolute-from-julian (list 1 1 (1+ y))))
                1)))
         (month       ;; Search forward from January.
          (1+ (calendar-sum m 1
                 (> date
                    (calendar-absolute-from-julian
                     (list m
                           (if (and (= m 2) (= (% year 4) 0))
                               29
                             (aref [31 28 31 30 31 30 31 31 30 31 30 31]
                                   (1- m)))
                           year)))
                 1)))
         (day         ;; Calculate the day by subtraction.
          (- date (1- (calendar-absolute-from-julian (list month 1 year))))))
    (list month day year)))

(defun calendar-absolute-from-julian (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ (calendar-day-number date)
       (if (and (= (% year 100) 0)
                (/= (% year 400) 0)
                (> month 2))
           1 0);; Correct for Julian but not Gregorian leap year.
       (* 365 (1- year))
       (/ (1- year) 4)
       -2)))

(defun calendar-julian-date-string (&optional date)
  "String of Julian date of Gregorian DATE.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'."
  (calendar-date-string
   (calendar-julian-from-absolute
    (calendar-absolute-from-gregorian
     (or date (calendar-current-date))))
   nil t))

(defun calendar-print-julian-date ()
  "Show the Julian calendar equivalent of the date under the cursor."
  (interactive)
  (message "Julian date: %s"
           (calendar-julian-date-string (calendar-cursor-to-date t))))

(defun islamic-calendar-leap-year-p (year)
  "Returns t if YEAR is a leap year on the Islamic calendar."
  (memq (% year 30)
        (list 2 5 7 10 13 16 18 21 24 26 29)))

(defun islamic-calendar-last-day-of-month (month year)
  "The last day in MONTH during YEAR on the Islamic calendar."
  (cond
   ((memq month (list 1 3 5 7 9 11)) 30)
   ((memq month (list 2 4 6 8 10)) 29)
   (t (if (islamic-calendar-leap-year-p year) 30 29))))

(defun islamic-calendar-day-number (date)
  "Return the day number within the year of the Islamic date DATE."
    (let* ((month (extract-calendar-month date))
           (day (extract-calendar-day date)))
      (+ (* 30 (/ month 2))
         (* 29 (/ (1- month) 2))
         day)))

(defun calendar-absolute-from-islamic (date)
  "Absolute date of Islamic DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (y (% year 30))
         (leap-years-in-cycle
          (cond
           ((< y 3) 0)  ((< y 6) 1)  ((< y 8) 2)  ((< y 11) 3) ((< y 14) 4)
           ((< y 17) 5) ((< y 19) 6) ((< y 22) 7) ((< y 25) 8) ((< y 27) 9)
           (t 10))))
    (+ (islamic-calendar-day-number date);; days so far this year
       (* (1- year) 354)                 ;; days in all non-leap years
       (* 11 (/ year 30))                ;; leap days in complete cycles
       leap-years-in-cycle               ;; leap days this cycle
       227014)))                         ;; days before start of calendar

(defun calendar-islamic-from-absolute (date)
  "Compute the Islamic date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (if (< date 227015)
      (list 0 0 0);; pre-Islamic date
    (let* ((approx (/ (- date 227014) 355));; Approximation from below.
           (year           ;; Search forward from the approximation.
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-absolute-from-islamic
                                       (list 1 1 (1+ y))))
                             1)))
           (month          ;; Search forward from Muharram.
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-absolute-from-islamic
                                  (list m
                                        (islamic-calendar-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day            ;; Calculate the day by subtraction.
            (- date
               (1- (calendar-absolute-from-islamic (list month 1 year))))))
      (list month day year))))

(defvar calendar-islamic-month-name-array
  ["Muharram" "Safar" "Rabi I" "Rabi II" "Jumada I" "Jumada II"
   "Rajab" "Sha'ban" "Ramadan" "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"])

(defun calendar-islamic-date-string (&optional date)
  "String of Islamic date before sunset of Gregorian DATE.
Returns the empty string if DATE is pre-Islamic.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'."
  (let ((calendar-month-name-array calendar-islamic-month-name-array)
        (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date))))))
    (if (< (extract-calendar-year islamic-date) 1)
        ""
      (calendar-date-string islamic-date nil t))))

(defun calendar-print-islamic-date ()
  "Show the Islamic calendar equivalent of the date under the cursor."
  (interactive)
  (let ((i (calendar-islamic-date-string (calendar-cursor-to-date t))))
    (if (string-equal i "")
        (message "Date is pre-Islamic")
      (message "Islamic date (until sunset): %s" i))))

(defun calendar-hebrew-from-absolute (date)
  "Compute the Hebrew date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((greg-date (calendar-gregorian-from-absolute date))
         (month (aref [9 10 11 12 1 2 3 4 7 7 7 8]
                 (1- (extract-calendar-month greg-date))))
         (day)
         (year (+ 3760 (extract-calendar-year greg-date))))
    (while (>= date (calendar-absolute-from-hebrew (list 7 1 (1+ year))))
        (setq year (1+ year)))
    (let ((length (hebrew-calendar-last-month-of-year year)))
      (while (> date
                (calendar-absolute-from-hebrew
                 (list month
                       (hebrew-calendar-last-day-of-month month year)
                       year)))
        (setq month (1+ (% month length)))))
    (setq day (1+
               (- date (calendar-absolute-from-hebrew (list month 1 year)))))
    (list month day year)))

(defun hebrew-calendar-leap-year-p (year)
  "t if YEAR is a Hebrew calendar leap year."
  (< (% (1+ (* 7 year)) 19) 7))

(defun hebrew-calendar-last-month-of-year (year)
  "The last month of the Hebrew calendar YEAR."
  (if (hebrew-calendar-leap-year-p year)
      13
    12))

(defun hebrew-calendar-last-day-of-month (month year)
  "The last day of MONTH in YEAR."
  (if (or (memq month (list 2 4 6 10 13))
          (and (= month 12) (not (hebrew-calendar-leap-year-p year)))
          (and (= month 8) (not (hebrew-calendar-long-heshvan-p year)))
          (and (= month 9) (hebrew-calendar-short-kislev-p year)))
      29
    30))

(defun hebrew-calendar-elapsed-days (year)
  "Days from Sun. prior to start of Hebrew calendar to mean conjunction of Tishri of Hebrew YEAR."
  (let* ((months-elapsed
          (+ (* 235 (/ (1- year) 19));; Months in complete cycles so far.
             (* 12 (% (1- year) 19))      ;; Regular months in this cycle
             (/ (1+ (* 7 (% (1- year) 19))) 19)));; Leap months this cycle
         (parts-elapsed (+ 204 (* 793 (% months-elapsed 1080))))
         (hours-elapsed (+ 5
                           (* 12 months-elapsed)
                           (* 793 (/ months-elapsed 1080))
                           (/ parts-elapsed 1080)))
         (parts                                  ;; Conjunction parts
          (+ (* 1080 (% hours-elapsed 24)) (% parts-elapsed 1080)))
         (day                                    ;; Conjunction day
          (+ 1 (* 29 months-elapsed) (/ hours-elapsed 24)))
         (alternative-day
          (if (or (>= parts 19440)    ;; If the new moon is at or after midday,
                  (and (= (% day 7) 2);; ...or is on a Tuesday...
                       (>= parts 9924)  ;;    at 9 hours, 204 parts or later...
                       (not (hebrew-calendar-leap-year-p year)));; of a
                                                                ;; common year,
                  (and (= (% day 7) 1);; ...or is on a Monday...
                       (>= parts 16789) ;;   at 15 hours, 589 parts or later...
                       (hebrew-calendar-leap-year-p (1- year))));; at the end
                                                     ;; of a leap year
       ;; Then postpone Rosh HaShanah one day
              (1+ day)
       ;; Else
            day)))
    (if ;; If Rosh HaShanah would occur on Sunday, Wednesday, or Friday
        (memq (% alternative-day 7) (list 0 3 5))
  ;; Then postpone it one (more) day and return        
        (1+ alternative-day)
  ;; Else return        
      alternative-day)))

(defun hebrew-calendar-days-in-year (year)
  "Number of days in Hebrew YEAR."
  (- (hebrew-calendar-elapsed-days (1+ year))
     (hebrew-calendar-elapsed-days year)))

(defun hebrew-calendar-long-heshvan-p (year)
  "t if Heshvan is long in Hebrew YEAR."
  (= (% (hebrew-calendar-days-in-year year) 10) 5))

(defun hebrew-calendar-short-kislev-p (year)
  "t if Kislev is short in Hebrew YEAR."
  (= (% (hebrew-calendar-days-in-year year) 10) 3))

(defun calendar-absolute-from-hebrew (date)
  "Absolute date of Hebrew DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date)))
    (+ day                            ;; Days so far this month.
       (if (< month 7);; before Tishri
     ;; Then add days in prior months this year before and after Nisan
           (+ (calendar-sum
               m 7 (<= m (hebrew-calendar-last-month-of-year year))
               (hebrew-calendar-last-day-of-month m year))
              (calendar-sum
               m 1 (< m month)
               (hebrew-calendar-last-day-of-month m year)))
     ;; Else add days in prior months this year
         (calendar-sum
          m 7 (< m month)
          (hebrew-calendar-last-day-of-month m year)))
    (hebrew-calendar-elapsed-days year);; Days in prior years.
    -1373429)))                        ;; Days elapsed before absolute date 1.

(defvar calendar-hebrew-month-name-array-common-year
  ["Nisan" "Iyar" "Sivan" "Tammuz" "Av" "Elul" "Tishri"
   "Heshvan" "Kislev" "Teveth" "Shevat" "Adar"])

(defvar calendar-hebrew-month-name-array-leap-year
  ["Nisan" "Iyar" "Sivan" "Tammuz" "Av" "Elul" "Tishri"
   "Heshvan" "Kislev" "Teveth" "Shevat" "Adar I" "Adar II"])

(defun calendar-hebrew-date-string (&optional date)
  "String of Hebrew date before sunset of Gregorian DATE.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'."
  (let* ((hebrew-date (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (calendar-month-name-array
          (if (hebrew-calendar-leap-year-p (extract-calendar-year hebrew-date))
              calendar-hebrew-month-name-array-leap-year
            calendar-hebrew-month-name-array-common-year)))
    (calendar-date-string hebrew-date nil t)))

(defun calendar-print-hebrew-date ()
  "Show the Hebrew calendar equivalent of the date under the cursor."
  (interactive)
  (message "Hebrew date (until sunset): %s"
           (calendar-hebrew-date-string (calendar-cursor-to-date t))))

(defun hebrew-calendar-yahrzeit (death-date year)
  "Absolute date of the anniversary of Hebrew DEATH-DATE in Hebrew YEAR."
  (let* ((death-day (extract-calendar-day death-date))
         (death-month (extract-calendar-month death-date))
         (death-year (extract-calendar-year death-date)))
    (cond
     ;; If it's Heshvan 30 it depends on the first anniversary; if
     ;; that was not Heshvan 30, use the day before Kislev 1.
     ((and (= death-month 8)
           (= death-day 30)
           (not (hebrew-calendar-long-heshvan-p (1+ death-year))))
      (1- (calendar-absolute-from-hebrew (list 9 1 year))))
     ;; If it's Kislev 30 it depends on the first anniversary; if
     ;; that was not Kislev 30, use the day before Teveth 1.
     ((and (= death-month 9)
           (= death-day 30)
           (hebrew-calendar-short-kislev-p (1+ death-year)))
      (1- (calendar-absolute-from-hebrew (list 10 1 year))))
     ;; If it's Adar II, use the same day in last month of
     ;; year (Adar or Adar II).
     ((= death-month 13)
      (calendar-absolute-from-hebrew
       (list (hebrew-calendar-last-month-of-year year) death-day year)))
     ;; If it's the 30th in Adar I and year is not a leap year
     ;; (so Adar has only 29 days), use the last day in Shevat.
     ((and (= death-day 30)
           (= death-month 12)
           (not (hebrew-calendar-leap-year-p year)))
      (calendar-absolute-from-hebrew (list 11 30 year)))
     ;; In all other cases, use the normal anniversary of the date of death.
     (t (calendar-absolute-from-hebrew
         (list death-month death-day year))))))

(defun calendar-set-mode-line (str)
  "Set mode line to STR, centered, surrounded by dashes."
  (setq mode-line-format
        (calendar-string-spread (list str) ?- (frame-width))))

;;;###autoload
(defun list-yahrzeit-dates (death-date start-year end-year)
  "List Yahrzeit dates for *Gregorian* DEATH-DATE from START-YEAR to END-YEAR.
When called interactively from the calendar window, the date of death is taken
from the cursor position."
  (interactive
   (let* ((death-date
           (if (equal (current-buffer) (get-buffer calendar-buffer))
               (calendar-cursor-to-date)
             (let* ((today (calendar-current-date))
                    (year (calendar-read
                           "Year of death (>0): "
                           '(lambda (x) (> x 0))
                           (int-to-string (extract-calendar-year today))))
                    (month-array calendar-month-name-array)
                    (completion-ignore-case t)
                    (month (cdr (assoc
                                 (capitalize
                                  (completing-read
                                   "Month of death (name): "
                                   (mapcar 'list (append month-array nil))
                                   nil t))
                                 (calendar-make-alist
                                  month-array 1 'capitalize))))
                    (last (calendar-last-day-of-month month year))
                    (day (calendar-read
                          (format "Day of death (1-%d): " last)
                          '(lambda (x) (and (< 0 x) (<= x last))))))
               (list month day year))))
          (death-year (extract-calendar-year death-date))
          (start-year (calendar-read
                       (format "Starting year of Yahrzeit table (>%d): "
                               death-year)
                       '(lambda (x) (> x death-year))
                       (int-to-string (1+ death-year))))
          (end-year (calendar-read
                     (format "Ending year of Yahrzeit table (>=%d): "
                             start-year)
                       '(lambda (x) (>= x start-year)))))
   (list death-date start-year end-year)))
  (message "Computing yahrzeits...")
  (let* ((yahrzeit-buffer "*Yahrzeits*")
         (h-date (calendar-hebrew-from-absolute
                  (calendar-absolute-from-gregorian death-date)))
         (h-month (extract-calendar-month h-date))
         (h-day (extract-calendar-day h-date))
         (h-year (extract-calendar-year h-date)))
    (set-buffer (get-buffer-create yahrzeit-buffer))
    (setq buffer-read-only nil)
    (calendar-set-mode-line
     (format "Yahrzeit dates for %s = %s"
             (calendar-date-string death-date)
             (let ((calendar-month-name-array
                    (if (hebrew-calendar-leap-year-p h-year)
                        calendar-hebrew-month-name-array-leap-year
                      calendar-hebrew-month-name-array-common-year)))
               (calendar-date-string h-date nil t))))
    (erase-buffer)
    (goto-char (point-min))
    (calendar-for-loop i from start-year to end-year do
        (insert
         (calendar-date-string
          (calendar-gregorian-from-absolute
           (hebrew-calendar-yahrzeit
            h-date
            (extract-calendar-year
             (calendar-hebrew-from-absolute
              (calendar-absolute-from-gregorian (list 1 1 i))))))) "\n"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (display-buffer yahrzeit-buffer)
    (message "Computing yahrzeits...done")))

(defun calendar-astro-date-string (&optional date)
  "String of astronomical (Julian) day number of afternoon of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (int-to-string
   (+ 1721425 (calendar-absolute-from-gregorian
               (or date (calendar-current-date))))))

(defun calendar-print-astro-day-number ()
  "Show astronomical (Julian) day number of afternoon on date shown by cursor."
  (interactive)
  (message
   "Astronomical (Julian) day number after noon UTC: %s"
   (calendar-astro-date-string (calendar-cursor-to-date t))))

(defun calendar-goto-astro-day-number (daynumber &optional noecho)
  "Move cursor to astronomical (Julian) DAYNUMBER.
Echo astronomical (Julian) day number unless NOECHO is t."
  (interactive (list (calendar-read
                      "Astronomical (Julian) day number (>1721425): "
                      '(lambda (x) (> x 1721425)))))
  (calendar-goto-date (calendar-gregorian-from-absolute (- daynumber 1721425)))
  (or noecho (calendar-print-astro-day-number)))

(run-hooks 'calendar-load-hook)

(provide 'calendar)

;;; calendar.el ends here
