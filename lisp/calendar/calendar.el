;;; calendar.el --- calendar functions

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1997,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, Gregorian calendar, diary, holidays

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

;; This collection of functions implements a calendar window.  It
;; generates a calendar for the current month, together with the
;; previous and coming months, or for any other three-month period.
;; The calendar can be scrolled forward and backward in the window to
;; show months in the past or future; the cursor can move forward and
;; backward by days, weeks, or months, making it possible, for
;; instance, to jump to the date a specified number of days, weeks, or
;; months from the date under the cursor.  The user can display a list
;; of holidays and other notable days for the period shown; the
;; notable days can be marked on the calendar, if desired.  The user
;; can also specify that dates having corresponding diary entries (in
;; a file that the user specifies) be marked; the diary entries for
;; any date can be viewed in a separate window.  The diary and the
;; notable days can be viewed independently of the calendar.  Dates
;; can be translated from the (usual) Gregorian calendar to the day of
;; the year/days remaining in year, to the ISO commercial calendar, to
;; the Julian (old style) calendar, to the Hebrew calendar, to the
;; Islamic calendar, to the Baha'i calendar, to the French
;; Revolutionary calendar, to the Mayan calendar, to the Chinese
;; calendar, to the Coptic calendar, to the Ethiopic calendar, and to
;; the astronomical (Julian) day number.  When floating point is
;; available, times of sunrise/sunset can be displayed, as can the
;; phases of the moon.  Appointment notification for diary entries is
;; available.  Calendar printing via LaTeX is available.

;; The following files are part of the calendar/diary code:

;;       appt.el                       Appointment notification
;;       cal-china.el                  Chinese calendar
;;       cal-coptic.el                 Coptic/Ethiopic calendars
;;       cal-dst.el                    Daylight saving time rules
;;       cal-hebrew.el                 Hebrew calendar
;;       cal-islam.el                  Islamic calendar
;;       cal-bahai.el                  Baha'i calendar
;;       cal-iso.el                    ISO calendar
;;       cal-julian.el                 Julian/astronomical calendars
;;       cal-mayan.el                  Mayan calendars
;;       cal-menu.el                   Menu support
;;       cal-move.el                   Movement in the calendar
;;       cal-persia.el                 Persian calendar
;;       cal-tex.el                    Calendars in LaTeX
;;       cal-x.el                      X-windows dedicated frame functions
;;       diary-lib.el                  Diary functions
;;       holidays.el                   Holiday functions
;;       lunar.el                      Phases of the moon
;;       solar.el                      Sunrise/sunset, equinoxes/solstices

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;; An earlier version of the technical details appeared in
;; ``Calendrical Calculations'' by Nachum Dershowitz and Edward M. Reingold,
;; Software--Practice and Experience, Volume 20, Number 9 (September, 1990),
;; pages 899-928, and in ``Calendrical Calculations, Part II: Three Historical
;; Calendars'' by E. M. Reingold,  N. Dershowitz, and S. M. Clamen,
;; Software--Practice and Experience, Volume 23, Number 4 (April, 1993),
;; pages 383-404.

;; Hard copies of these two papers can be obtained by sending email to
;; reingold@cs.uiuc.edu with the SUBJECT "send-paper-cal" (no quotes) and
;; the message BODY containing your mailing address (snail).

;;; Code:

(defvar displayed-month)
(defvar displayed-year)

(require 'cal-loaddefs)
(require 'cal-menu)


(defgroup calendar nil
  "Calendar and time management support."
  :group 'applications)

(defgroup calendar-hooks nil
  "Calendar hooks."
  :prefix "calendar-"
  :group 'calendar)

(defgroup diary nil
  "Emacs diary."
  :group 'calendar)

(defgroup holidays nil
  "Holidays support in calendar."
  :group 'calendar
  :prefix "calendar-"
  :group 'local)


(defcustom calendar-offset 0
  "The offset of the principal month from the center of the calendar window.
0 means the principal month is in the center (default), -1 means on the left,
+1 means on the right.  Larger (or smaller) values push the principal month off
the screen."
  :type 'integer
  :group 'calendar)

(defcustom calendar-setup nil
  "The frame setup of the calendar.
The choices are: `one-frame' (calendar and diary together in one separate,
dedicated frame); `two-frames' (calendar and diary in separate, dedicated
frames); `calendar-only' (calendar in a separate, dedicated frame); with
any other value the current frame is used.  Using any of the first
three options overrides the value of `view-diary-entries-initially'."
  :type '(choice
          (const :tag "calendar and diary in separate frame" one-frame)
          (const :tag "calendar and diary each in own frame" two-frames)
          (const :tag "calendar in separate frame" calendar-only)
          (const :tag "use current frame" nil))
  :group 'calendar)

(defcustom calendar-minimum-window-height 8
  "Minimum height `generate-calendar-window' should use for calendar window."
  :type 'integer
  :version "22.1"
  :group 'calendar)

(defcustom calendar-week-start-day 0
  "The day of the week on which a week in the calendar begins.
0 means Sunday (default), 1 means Monday, and so on.

If you change this variable directly (without using customize)
after starting `calendar', you should call `redraw-calendar' to
update the calendar display to reflect the change, otherwise
movement commands will not work correctly."
  :type 'integer
  ;; Change the initialize so that if you reload calendar.el, it will not
  ;; cause a redraw (which may fail, e.g. with "invalid byte-code in
  ;; calendar.elc" because of the "byte-compile-dynamic").
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set sym val)
         (redraw-calendar))
  :group 'calendar)

(defcustom view-diary-entries-initially nil
  "Non-nil means display current date's diary entries on entry to calendar.
The diary is displayed in another window when the calendar is first displayed,
if the current date is visible.  The number of days of diary entries displayed
is governed by the variable `number-of-diary-entries'.  This variable can
be overridden by the value of `calendar-setup'."
  :type 'boolean
  :group 'diary)

(defcustom mark-diary-entries-in-calendar nil
  "Non-nil means mark dates with diary entries, in the calendar window.
The marking symbol is specified by the variable `diary-entry-marker'."
  :type 'boolean
  :group 'diary)

(defcustom calendar-remove-frame-by-deleting nil
  "Determine how the calendar mode removes a frame no longer needed.
If nil, make an icon of the frame.  If non-nil, delete the frame."
  :type 'boolean
  :group 'view)

(defface calendar-today
  '((t (:underline t)))
  "Face for indicating today's date."
  :group 'diary)
;; Backward-compatibility alias.  FIXME make obsolete.
(put 'calendar-today-face 'face-alias 'calendar-today)

(defface diary
  '((((min-colors 88) (class color) (background light))
     :foreground "red1")
    (((class color) (background light))
     :foreground "red")
    (((min-colors 88) (class color) (background dark))
     :foreground "yellow1")
    (((class color) (background dark))
     :foreground "yellow")
    (t
     :weight bold))
  "Face for highlighting diary entries."
  :group 'diary)
;; Backward-compatibility alias. FIXME make obsolete.
(put 'diary-face 'face-alias 'diary)

(defface holiday
  '((((class color) (background light))
     :background "pink")
    (((class color) (background dark))
     :background "chocolate4")
    (t
     :inverse-video t))
  "Face for indicating dates that have holidays."
  :group 'diary)
;; Backward-compatibility alias.  FIXME make obsolete.
(put 'holiday-face 'face-alias 'holiday)

(defcustom diary-entry-marker (if (display-color-p) 'diary "+")
  "How to mark dates that have diary entries.
The value can be either a single-character string or a face."
  :type '(choice string face)
  :group 'diary)

(defcustom calendar-today-marker (if (display-color-p) 'calendar-today "=")
  "How to mark today's date in the calendar.
The value can be either a single-character string or a face.
Marking today's date is done only if you set up `today-visible-calendar-hook'
to request that."
  :type '(choice string face)
  :group 'calendar)

(defcustom calendar-holiday-marker (if (display-color-p) 'holiday "*")
  "How to mark notable dates in the calendar.
The value can be either a single-character string or a face."
  :type '(choice string face)
  :group 'calendar)

(defcustom view-calendar-holidays-initially nil
  "Non-nil means display holidays for current three month period on entry.
The holidays are displayed in another window when the calendar is first
displayed."
  :type 'boolean
  :group 'holidays)

(defcustom mark-holidays-in-calendar nil
  "Non-nil means mark dates of holidays in the calendar window.
The marking symbol is specified by the variable `calendar-holiday-marker'."
  :type 'boolean
  :group 'holidays)

(defcustom calendar-mode-hook nil
  "Hook run when entering `calendar-mode'."
  :type 'hook
  :group 'calendar-hooks)

(defcustom calendar-load-hook nil
  "List of functions to be called after the calendar is first loaded.
This is the place to add key bindings to `calendar-mode-map'."
  :type 'hook
  :group 'calendar-hooks)

(defcustom initial-calendar-window-hook nil
  "List of functions to be called when the calendar window is first opened.
The functions invoked are called after the calendar window is opened, but
once opened is never called again.  Leaving the calendar with the `q' command
and reentering it will cause these functions to be called again."
  :type 'hook
  :group 'calendar-hooks)

(defcustom today-visible-calendar-hook nil
  "List of functions called whenever the current date is visible.
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
functions that move by days and weeks."
  :type 'hook
  :group 'calendar-hooks)

(defcustom today-invisible-calendar-hook nil
  "List of functions called whenever the current date is not visible.

The corresponding variable `today-visible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks."
  :type 'hook
  :group 'calendar-hooks)

(defcustom calendar-move-hook nil
  "List of functions called whenever the cursor moves in the calendar.

For example,

  (add-hook 'calendar-move-hook (lambda () (diary-view-entries 1)))

redisplays the diary for whatever date the cursor is moved to."
  :type 'hook
  :group 'calendar-hooks)

;;;###autoload
(defcustom diary-file "~/diary"
  "Name of the file in which one's personal diary of dates is kept.

The file's entries are lines beginning with any of the forms
specified by the variable `american-date-diary-pattern', by default:

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR
            DAYNAME

with the remainder of the line being the diary entry string for
that date.  MONTH and DAY are one or two digit numbers, YEAR is a
number and may be written in full or abbreviated to the final two
digits (if `abbreviated-calendar-year' is non-nil).  MONTHNAME
and DAYNAME can be spelled in full (as specified by the variables
`calendar-month-name-array' and `calendar-day-name-array'),
abbreviated (as specified by `calendar-month-abbrev-array' and
`calendar-day-abbrev-array') with or without a period,
capitalized or not.  Any of DAY, MONTH, or MONTHNAME, YEAR can be
`*' which matches any day, month, or year, respectively. If the
date does not contain a year, it is generic and applies to any
year.  A DAYNAME entry applies to the appropriate day of the week
in every week.

The European style (in which the day precedes the month) can be
used instead, if you execute `european-calendar' when in the
calendar, or set `european-calendar-style' to t in your .emacs
file.  The European forms (see `european-date-diary-pattern') are

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

causes the diary entry \"Vacation\" to appear from November 1 through
November 10, 1990.  Other functions available are `diary-float',
`diary-anniversary', `diary-cyclic', `diary-day-of-year',
`diary-iso-date', `diary-french-date', `diary-hebrew-date',
`diary-islamic-date', `diary-bahai-date', `diary-mayan-date',
`diary-chinese-date', `diary-coptic-date', `diary-ethiopic-date',
`diary-persian-date', `diary-yahrzeit', `diary-sunrise-sunset',
`diary-phases-of-moon', `diary-parasha', `diary-omer',
`diary-rosh-hodesh', and `diary-sabbath-candles'.  See the
documentation for the function `list-sexp-diary-entries' for more
details.

Diary entries based on the Hebrew, the Islamic and/or the Baha'i
calendar are also possible, but because these are somewhat slow, they
are ignored unless you set the `nongregorian-diary-listing-hook' and
the `nongregorian-diary-marking-hook' appropriately.  See the
documentation for these functions for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `list-diary-entries-hook'."
  :type 'file
  :group 'diary)

(defcustom diary-nonmarking-symbol "&"
  "Symbol indicating that a diary entry is not to be marked in the calendar."
  :type 'string
  :group 'diary)

(defcustom hebrew-diary-entry-symbol "H"
  "Symbol indicating a diary entry according to the Hebrew calendar."
  :type 'string
  :group 'diary)

(defcustom islamic-diary-entry-symbol "I"
  "Symbol indicating a diary entry according to the Islamic calendar."
  :type 'string
  :group 'diary)

(defcustom bahai-diary-entry-symbol "B"
  "Symbol indicating a diary entry according to the Baha'i calendar."
  :type 'string
  :group 'diary)

(defcustom abbreviated-calendar-year t
  "Interpret a two-digit year DD in a diary entry as either 19DD or 20DD.
For the Gregorian calendar; similarly for the Hebrew, Islamic and
Baha'i calendars.  If this variable is nil, years must be written in
full."
  :type 'boolean
  :group 'diary)

;;;###autoload
(defcustom european-calendar-style nil
  "Use the European style of dates in the diary and in any displays.
If this variable is t, a date 1/2/1990 would be interpreted as February 1,
1990.  The default European date styles (see `european-date-diary-pattern')
are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

Names can be capitalized or not, written in full (as specified by the
variable `calendar-day-name-array'), or abbreviated (as specified by
`calendar-day-abbrev-array') with or without a period.

Setting this variable directly does not take effect (if the
calendar package is already loaded).  Rather, use either
\\[customize] or the functions `european-calendar' and
`american-calendar'."
  :type 'boolean
  ;; Without :initialize (require 'calendar) throws an error because
  ;; american-calendar is undefined at this point.
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (if value
             (european-calendar)
           (american-calendar)))
  :group 'diary)

(defcustom american-date-diary-pattern
  '((month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W"))
  "List of pseudo-patterns describing the American patterns of date used.
See the documentation of `diary-date-forms' for an explanation."
  :type '(repeat (choice (cons :tag "Backup"
			       :value (backup . nil)
			       (const backup)
			       (repeat (list :inline t :format "%v"
					     (symbol :tag "Keyword")
					     (choice symbol regexp))))
			 (repeat (list :inline t :format "%v"
				       (symbol :tag "Keyword")
				       (choice symbol regexp)))))
  :group 'diary)

(defcustom european-date-diary-pattern
  '((day "/" month "[^/0-9]")
    (day "/" month "/" year "[^0-9]")
    (backup day " *" monthname "\\W+\\<\\([^*0-9]\\|\\([0-9]+[:aApP]\\)\\)")
    (day " *" monthname " *" year "[^0-9]")
    (dayname "\\W"))
  "List of pseudo-patterns describing the European patterns of date used.
See the documentation of `diary-date-forms' for an explanation."
  :type '(repeat (choice (cons :tag "Backup"
			       :value (backup . nil)
			       (const backup)
			       (repeat (list :inline t :format "%v"
					     (symbol :tag "Keyword")
					     (choice symbol regexp))))
			 (repeat (list :inline t :format "%v"
				       (symbol :tag "Keyword")
				       (choice symbol regexp)))))
  :group 'diary)

(autoload 'diary-font-lock-keywords "diary-lib")
(autoload 'diary-live-p "diary-lib")
(defvar diary-font-lock-keywords)

(defcustom diary-date-forms
  (if european-calendar-style
      european-date-diary-pattern
    american-date-diary-pattern)
  "List of pseudo-patterns describing the forms of date used in the diary.
The patterns on the list must be MUTUALLY EXCLUSIVE and should not match
any portion of the diary entry itself, just the date component.

A pseudo-pattern is a list of regular expressions and the keywords `month',
`day', `year', `monthname', and `dayname'.  The keyword `monthname' will
match the name of the month (see `calendar-month-name-array'), capitalized
or not, or its user-specified abbreviation (see `calendar-month-abbrev-array'),
followed by a period or not; it will also match `*'.  Similarly, `dayname'
will match the name of the day (see `calendar-day-name-array'), capitalized or
not, or its user-specified abbreviation (see `calendar-day-abbrev-array'),
followed by a period or not.  The keywords `month', `day', and `year' will
match those numerical values, preceded by arbitrarily many zeros; they will
also match `*'.

The matching of the diary entries with the date forms is done with the
standard syntax table from Fundamental mode, but with the `*' changed so
that it is a word constituent.

If, to be mutually exclusive, a pseudo-pattern must match a portion of the
diary entry itself, the first element of the pattern MUST be `backup'.  This
directive causes the date recognizer to back up to the beginning of the
current word of the diary entry, so in no case can the pattern match more than
a portion of the first word of the diary entry."
  :type '(repeat (choice (cons :tag "Backup"
			       :value (backup . nil)
			       (const backup)
			       (repeat (list :inline t :format "%v"
					     (symbol :tag "Keyword")
					     (choice symbol regexp))))
			 (repeat (list :inline t :format "%v"
				       (symbol :tag "Keyword")
				       (choice symbol regexp)))))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (unless (equal value (eval symbol))
           (custom-set-default symbol value)
           (setq diary-font-lock-keywords (diary-font-lock-keywords))
           ;; Need to redraw not just to get new font-locking, but also
           ;; to pick up any newly recognized entries.
           (and (diary-live-p)
                (diary))))
  :group 'diary)

(defcustom european-calendar-display-form
  '((if dayname (concat dayname ", ")) day " " monthname " " year)
  "Pseudo-pattern governing the way a date appears in the European style.
See the documentation of `calendar-date-display-form' for an explanation."
  :type 'sexp
  :group 'calendar)

(defcustom american-calendar-display-form
  '((if dayname (concat dayname ", ")) monthname " " day ", " year)
  "Pseudo-pattern governing the way a date appears in the American style.
See the documentation of `calendar-date-display-form' for an explanation."
  :type 'sexp
  :group 'calendar)

(defcustom calendar-date-display-form
  (if european-calendar-style
      european-calendar-display-form
    american-calendar-display-form)
  "Pseudo-pattern governing the way a date appears.

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

See the documentation of the function `calendar-date-string'."
  :type 'sexp
  :group 'calendar)

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

;; FIXME move to diary-lib and adjust appt.
(defcustom diary-hook nil
  "List of functions called after the display of the diary.
Can be used for appointment notification."
  :type 'hook
  :group 'diary)

(autoload 'diary-set-maybe-redraw "diary-lib")

(defcustom diary-display-hook nil
  "List of functions that handle the display of the diary.
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
diary buffer, set the variable `diary-list-include-blanks' to t."
  :type 'hook
  :options '(fancy-diary-display)
  :initialize 'custom-initialize-default
  :set 'diary-set-maybe-redraw
  :group 'diary)

(defcustom holidays-in-diary-buffer t
  "Non-nil means include holidays in the diary display.
The holidays appear in the mode line of the diary buffer, or in the
fancy diary buffer next to the date.  This slows down the diary functions
somewhat; setting it to nil makes the diary display faster."
  :type 'boolean
  :group 'holidays)

(defcustom calendar-debug-sexp nil
  "Turn debugging on when evaluating a sexp in the diary or holiday list."
  :type 'boolean
  :group 'calendar)

;;;###autoload
(defcustom general-holidays
  '((holiday-fixed 1 1 "New Year's Day")
    (holiday-float 1 1 3 "Martin Luther King Day")
    (holiday-fixed 2 2 "Groundhog Day")
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-float 2 1 3 "President's Day")
    (holiday-fixed 3 17 "St. Patrick's Day")
    (holiday-fixed 4 1 "April Fools' Day")
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
  "General holidays.  Default value is for the United States.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'general-holidays 'risky-local-variable t)

;;;###autoload
(defcustom oriental-holidays
  '((if (fboundp 'atan)
	(holiday-chinese-new-year)))
  "Oriental holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'oriental-holidays 'risky-local-variable t)

;;;###autoload
(defcustom local-holidays nil
  "Local holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'local-holidays 'risky-local-variable t)

;;;###autoload
(defcustom other-holidays nil
  "User defined holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'other-holidays 'risky-local-variable t)

(defcustom all-hebrew-calendar-holidays nil
  "If nil, show only major holidays from the Hebrew calendar.
This means only those Jewish holidays that appear on secular calendars.
Otherwise, show all the holidays that would appear in a complete Hebrew
calendar."
  :type 'boolean
  :group 'holidays)

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
(put 'hebrew-holidays-1 'risky-local-variable t)

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
(put 'hebrew-holidays-2 'risky-local-variable t)

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
(put 'hebrew-holidays-3 'risky-local-variable t)

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
(put 'hebrew-holidays-4 'risky-local-variable t)

;;;###autoload
(defcustom hebrew-holidays (append hebrew-holidays-1 hebrew-holidays-2
				hebrew-holidays-3 hebrew-holidays-4)
  "Jewish holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'hebrew-holidays 'risky-local-variable t)

(defcustom all-christian-calendar-holidays nil
  "If nil, show only major holidays from the Christian calendar.
This means only those Christian holidays that appear on secular calendars.
Otherwise, show all the holidays that would appear in a complete Christian
calendar."
  :type 'boolean
  :group 'holidays)

;;;###autoload
(defcustom christian-holidays
  '((if all-christian-calendar-holidays
        (holiday-fixed 1 6 "Epiphany"))
    (holiday-easter-etc 0 "Easter Sunday")
    (holiday-easter-etc -2 "Good Friday")
    (holiday-easter-etc -46 "Ash Wednesday")
    (if all-christian-calendar-holidays
        (holiday-easter-etc -63 "Septuagesima Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -56 "Sexagesima Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -49 "Shrove Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -48 "Shrove Monday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -47 "Shrove Tuesday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -14 "Passion Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -7 "Palm Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc -3 "Maundy Thursday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc 35 "Rogation Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc 39 "Ascension Day"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc 49 "Pentecost (Whitsunday)"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc 50 "Whitmonday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc 56 "Trinity Sunday"))
    (if all-christian-calendar-holidays
        (holiday-easter-etc 60 "Corpus Christi"))
    (if all-christian-calendar-holidays
        (holiday-greek-orthodox-easter))
    (if all-christian-calendar-holidays
        (holiday-fixed 8 15 "Assumption"))
    (if all-christian-calendar-holidays
        (holiday-advent 0 "Advent"))
    (holiday-fixed 12 25 "Christmas")
    (if all-christian-calendar-holidays
        (holiday-julian 12 25 "Eastern Orthodox Christmas")))
  "Christian holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'christian-holidays 'risky-local-variable t)

(defcustom all-islamic-calendar-holidays nil
  "If nil, show only major holidays from the Islamic calendar.
This means only those Islamic holidays that appear on secular calendars.
Otherwise, show all the holidays that would appear in a complete Islamic
calendar."
  :type 'boolean
  :group 'holidays)

;;;###autoload
(defcustom islamic-holidays
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
  "Islamic holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'islamic-holidays 'risky-local-variable t)

(defcustom all-bahai-calendar-holidays nil
  "If nil, show only major holidays from the Baha'i calendar.
These are the days on which work and school must be suspended.
Otherwise, show all the holidays that would appear in a complete Baha'i
calendar."
  :type 'boolean
  :group 'holidays)

;;;###autoload
(defcustom bahai-holidays
  '((holiday-fixed
     3 21
     (format "Baha'i New Year (Naw-Ruz) %d" (- displayed-year (1- 1844))))
    (holiday-fixed  4 21 "First Day of Ridvan")
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 22 "Second Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 23 "Third Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 24 "Fourth Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 25 "Fifth Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 26 "Sixth Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 27 "Seventh Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 28 "Eighth Day of Ridvan"))
    (holiday-fixed  4 29 "Ninth Day of Ridvan")
    (if all-bahai-calendar-holidays
	(holiday-fixed  4 30 "Tenth Day of Ridvan"))
    (if all-bahai-calendar-holidays
	(holiday-fixed  5  1 "Eleventh Day of Ridvan"))
    (holiday-fixed  5  2 "Twelfth Day of Ridvan")
    (holiday-fixed  5 23 "Declaration of the Bab")
    (holiday-fixed  5 29 "Ascension of Baha'u'llah")
    (holiday-fixed  7  9 "Martyrdom of the Bab")
    (holiday-fixed 10 20 "Birth of the Bab")
    (holiday-fixed 11 12 "Birth of Baha'u'llah")
    (if all-bahai-calendar-holidays
	(holiday-fixed 11 26 "Day of the Covenant"))
    (if all-bahai-calendar-holidays
	(holiday-fixed 11 28 "Ascension of `Abdu'l-Baha")))
  "Baha'i holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'bahai-holidays 'risky-local-variable t)

;;;###autoload
(defcustom solar-holidays
  '((if (fboundp 'atan)
	(solar-equinoxes-solstices))
    (if (require 'cal-dst)
      (funcall
       'holiday-sexp
        calendar-daylight-savings-starts
        '(format "Daylight Saving Time Begins %s"
                  (if (fboundp 'atan)
                      (solar-time-string
                       (/ calendar-daylight-savings-starts-time (float 60))
                       calendar-standard-time-zone-name)
                    ""))))
    (funcall
     'holiday-sexp
     calendar-daylight-savings-ends
     '(format "Daylight Saving Time Ends %s"
              (if (fboundp 'atan)
                  (solar-time-string
                   (/ calendar-daylight-savings-ends-time (float 60))
                   calendar-daylight-time-zone-name)
                ""))))
  "Sun-related holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'solar-holidays 'risky-local-variable t)

;;;###autoload
(defcustom calendar-holidays
  (append general-holidays local-holidays other-holidays
          christian-holidays hebrew-holidays islamic-holidays
          bahai-holidays oriental-holidays solar-holidays)
  "List of notable days for the command \\[holidays].

Additional holidays are easy to add to the list, just put them in the
list `other-holidays' in your .emacs file.  Similarly, by setting any
of `general-holidays', `local-holidays' `christian-holidays',
`hebrew-holidays', `islamic-holidays', `bahai-holidays',
`oriental-holidays', or `solar-holidays' to nil in your .emacs file,
you can eliminate unwanted categories of holidays.

The aforementioned variables control the holiday choices offered
by the function `holiday-list' when it is called interactively.

They also initialize the default value of `calendar-holidays',
which is the default list of holidays used by the function
`holiday-list' in the non-interactive case.  Note that these
variables have no effect on `calendar-holidays' after it has been
set (e.g. after the calendar is loaded).  In that case, customize
`calendar-holidays' directly.

The intention is that (in the US) `local-holidays' be set in
site-init.el and `other-holidays' be set by the user.

Entries on the list are expressions that return (possibly empty) lists of
items of the form ((month day year) string) of a holiday in the in the
three-month period centered around `displayed-month' of `displayed-year'.
Several basic functions are provided for this purpose:

    (holiday-fixed MONTH DAY STRING) is a fixed date on the Gregorian calendar
    (holiday-float MONTH DAYNAME K STRING &optional day) is the Kth DAYNAME in
                               MONTH on the Gregorian calendar (0 for Sunday,
                               etc.); K<0 means count back from the end of the
                               month.  An optional parameter DAY means the Kth
                               DAYNAME after/before MONTH DAY.
    (holiday-hebrew MONTH DAY STRING)  a fixed date on the Hebrew calendar
    (holiday-islamic MONTH DAY STRING) a fixed date on the Islamic calendar
    (holiday-bahai MONTH DAY STRING)   a fixed date on the Baha'i calendar
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
add an entry for the Baha'i festival of Ridvan, use

     (holiday-bahai 2 13 \"Festival of Ridvan\")

since the Baha'i months are numbered from 1 starting with Baha.  To
add Thomas Jefferson's birthday, April 2, 1743 (Julian), use

     (holiday-julian 4 2 \"Jefferson's Birthday\")

To include a holiday conditionally, use the sexp form or a conditional.  For
example, to include American presidential elections, which occur on the first
Tuesday after the first Monday in November of years divisible by 4, add

     (holiday-sexp
       '(if (zerop (% year 4))
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

     (((2 6 1989) \"New Moon\") ((2 12 1989) \"First Quarter Moon\") ... )."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'calendar-holidays 'risky-local-variable t)

;;; End of user options.

(defconst calendar-buffer "*Calendar*"
  "Name of the buffer used for the calendar.")

(defconst holiday-buffer "*Holidays*"
  "Name of the buffer used for the displaying the holidays.")

(defconst fancy-diary-buffer "*Fancy Diary Entries*"
  "Name of the buffer used for the optional fancy display of the diary.")

(defconst other-calendars-buffer "*Other Calendars*"
  "Name of the buffer used for the display of date on other calendars.")

(defconst lunar-phases-buffer "*Phases of Moon*"
  "Name of the buffer used for the lunar phases.")

(defmacro increment-calendar-month (mon yr n)
  "Increment the variables MON and YR by N months.
Forward if N is positive or backward if N is negative.
A negative YR is interpreted as BC; -1 being 1 BC, and so on."
  `(let (macro-y)
     (if (< ,yr 0) (setq ,yr (1+ ,yr))) ; -1 BC -> 0 AD, etc
     (setq macro-y (+ (* ,yr 12) ,mon -1 ,n)
           ,mon (1+ (mod macro-y 12))
           ,yr (/ macro-y 12))
     (and (< macro-y 0) (> ,mon 1) (setq ,yr (1- ,yr)))
     (if (< ,yr 1) (setq ,yr (1- ,yr))))) ; 0 AD -> -1 BC, etc

(defun calendar-increment-month (n &optional mon yr)
  "Return the Nth month after MON/YR.
The return value is a pair (MONTH . YEAR).
MON defaults to `displayed-month'.  YR defaults to `displayed-year'."
  (unless mon (setq mon displayed-month))
  (unless yr (setq yr displayed-year))
  (increment-calendar-month mon yr n)
  (cons mon yr))

(defmacro calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive."
  (declare (debug (symbolp "from" form "to" form "do" body)))
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

(defmacro calendar-sum (index initial condition expression)
  "For INDEX = INITIAL et seq, as long as CONDITION holds, sum EXPRESSION."
  (declare (debug (symbolp form form form)))
  `(let ((,index ,initial)
         (sum 0))
    (while ,condition
      (setq sum (+ sum ,expression))
      (setq ,index (1+ ,index)))
    sum))

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

;; Note gives wrong answer for result of (calendar-read-date 'noday).
(defsubst extract-calendar-day (date)
  "Extract the day part of DATE which has the form (month day year)."
  (car (cdr date)))

(defsubst extract-calendar-year (date)
  "Extract the year part of DATE which has the form (month day year)."
  (car (cdr (cdr date))))

(defsubst calendar-leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year.
A negative year is interpreted as BC; -1 being 1 BC, and so on."
  ;; 1 BC = 0 AD, 2 BC acts like 1 AD, etc.
  (if (< year 0) (setq year (1- (abs year))))
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
The Gregorian date Sunday, December 31, 1 BC is imaginary.
DATE is a list of the form (month day year).  A negative year is
interpreted as BC; -1 being 1 BC, and so on.  Dates before 12/31/1 BC
return negative results."
  (let ((year (extract-calendar-year date))
        offset-years)
    (cond ((= year 0)
           (error "There was no year zero"))
          ((> year 0)
           (setq offset-years (1- year))
           (+ (calendar-day-number date) ; Days this year
              (* 365 offset-years)       ; + Days in prior years
              (/ offset-years 4)         ; + Julian leap years
              (- (/ offset-years 100))   ; - century years
              (/ offset-years 400)))     ; + Gregorian leap years
          (t
           ;; Years between date and 1 BC, excluding 1 BC (1 for 2 BC, etc).
           (setq offset-years (abs (1+ year)))
           (- (calendar-day-number date)
              (* 365 offset-years)
              (/ offset-years 4)
              (- (/ offset-years 100))
              (/ offset-years 400)
              (calendar-day-number '(12 31 -1))))))) ; days in year 1 BC

;;;###autoload
(defun calendar (&optional arg)
  "Choose between the one frame, two frame, or basic calendar displays.
If called with an optional prefix argument, prompts for month and year.

The original function `calendar' has been renamed `calendar-basic-setup'.
See the documentation of that function for more information."
  (interactive "P")
  (cond ((equal calendar-setup 'one-frame) (calendar-one-frame-setup arg))
        ((equal calendar-setup 'two-frames) (calendar-two-frame-setup arg))
        ((equal calendar-setup 'calendar-only)
         (calendar-only-one-frame-setup arg))
        (t (calendar-basic-setup arg))))

(autoload 'diary-view-entries "diary-lib"
  "Prepare and display a buffer with diary entries.
Searches your diary file for entries that match ARG days starting with
the date indicated by the cursor position in the displayed three-month
calendar."
  t)

(autoload 'calendar-list-holidays "holidays"
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.  Returns t if any
holidays are found, nil if not."
  t)

(defun calendar-basic-setup (&optional arg)
  "Display a three-month calendar in another window.
The three months appear side by side, with the current month in the middle
surrounded by the previous and next months.  The cursor is put on today's date.

If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file; appropriate setting
of the variable `view-diary-entries-initially' will cause the diary entries for
the current date to be displayed in another window.  The value of the variable
`number-of-diary-entries' controls the number of days of diary entries
displayed upon initial display of the calendar.

Once in the calendar window, future or past months can be moved into view.
Arbitrary months can be displayed, or the calendar can be scrolled forward
or backward.

The cursor can be moved forward or backward by one day, one week, one month,
or one year.  All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the digit keys
and the minus sign are automatically prefixes.  The window is replotted as
necessary to display the desired date.

Diary entries can be marked on the calendar or displayed in another window.

Use \\[describe-mode] for details of the key bindings in the calendar window.

The Gregorian calendar is assumed.

After loading the calendar, the hooks given by the variable
`calendar-load-hook' are run.  This is the place to add key bindings to the
calendar-mode-map.

After preparing the calendar window initially, the hooks given by the variable
`initial-calendar-window-hook' are run.

The hooks given by the variable `today-visible-calendar-hook' are run
every time the calendar window gets scrolled, if the current date is visible
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
    ;; (calendar-read-date t) returns a date with day = nil, which is
    ;; not a valid date for the visible test in the diary section.
    (if arg (setcar (cdr date) 1))
    (increment-calendar-month month year (- calendar-offset))
    ;; Display the buffer before calling generate-calendar-window so that it
    ;; can get a chance to adjust the window sizes to the frame size.
    (pop-to-buffer calendar-buffer)
    (generate-calendar-window month year)
    (if (and view-diary-entries-initially (calendar-date-is-visible-p date))
        (diary-view-entries)))
  (let* ((diary-buffer (get-file-buffer diary-file))
         (diary-window (if diary-buffer (get-buffer-window diary-buffer)))
         (split-height-threshold (if diary-window 2 1000)))
    (if view-calendar-holidays-initially
        (calendar-list-holidays)))
  (run-hooks 'initial-calendar-window-hook))

(autoload 'view-other-diary-entries "diary-lib"
  "Prepare and display buffer of diary entries from an alternative diary file.
Searches for entries that match ARG days, starting with the date indicated
by the cursor position in the displayed three-month calendar.
D-FILE specifies the file to use as the diary file."
  t)

(autoload 'calendar-sunrise-sunset "solar"
  "Local time of sunrise and sunset for date under cursor."
  t)

(autoload 'calendar-phases-of-moon "lunar"
  "Create a buffer of the phases of the moon for the current calendar window."
  t)

(autoload 'calendar-goto-hebrew-date "cal-hebrew"
  "Move cursor to Hebrew date."
  t)

(autoload 'calendar-print-hebrew-date "cal-hebrew"
  "Show the Hebrew date equivalents of date."
  t)

(autoload 'calendar-hebrew-date-string "cal-hebrew"
  "String of Hebrew date of Gregorian date.")

(autoload 'diary-show-all-entries "diary-lib"
  "Show all of the diary entries in the diary file.
This function gets rid of the selective display of the diary file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created."
  t)

(autoload 'mark-diary-entries "diary-lib"
  "Mark days in the calendar window that have diary entries.
Each entry in diary file visible in the calendar window is marked."
  t)

(autoload 'make-diary-entry "diary-lib"
  "Insert a diary entry STRING which may be NONMARKING in FILE.")

(autoload 'insert-diary-entry "diary-lib"
  "Insert a diary entry for the date indicated by point."
  t)

(autoload 'insert-weekly-diary-entry "diary-lib"
  "Insert a weekly diary entry for the day of the week indicated by point."
  t)

(autoload 'insert-monthly-diary-entry "diary-lib"
  "Insert a monthly diary entry for the day of the month indicated by point."
  t)

(autoload 'insert-yearly-diary-entry "diary-lib"
  "Insert an annual diary entry for the day of the year indicated by point."
  t)

(autoload 'insert-anniversary-diary-entry "diary-lib"
  "Insert an anniversary diary entry for the date indicated by point."
  t)

(autoload 'insert-block-diary-entry "diary-lib"
  "Insert a block diary entry for the dates indicated by point and mark."
  t)

(autoload 'insert-cyclic-diary-entry "diary-lib"
  "Insert a cyclic diary entry starting at the date indicated by point."
  t)

(autoload 'insert-hebrew-diary-entry "cal-hebrew"
  "Insert a diary entry for the Hebrew date corresponding to the date
indicated by point."
  t)

(autoload 'insert-monthly-hebrew-diary-entry "cal-hebrew"
  "Insert a monthly diary entry for the day of the Hebrew month corresponding
to the date indicated by point."
  t)

(autoload 'insert-yearly-hebrew-diary-entry "cal-hebrew"
  "Insert an annual diary entry for the day of the Hebrew year corresponding
to the date indicated by point."
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
  (let* ((inhibit-read-only t)
         (today (calendar-current-date))
         (month (extract-calendar-month today))
         (day (extract-calendar-day today))
         (year (extract-calendar-year today))
         (today-visible
          (or (not mon)
              (let ((offset (calendar-interval mon yr month year)))
                (and (<= offset 1) (>= offset -1)))))
         (day-in-week (calendar-day-of-week today))
         (in-calendar-window (eq (window-buffer (selected-window))
                                 (get-buffer calendar-buffer))))
    (generate-calendar (or mon month) (or yr year))
    (update-calendar-mode-line)
    (calendar-cursor-to-visible-date
     (if today-visible today (list displayed-month 1 displayed-year)))
    (set-buffer-modified-p nil)
    ;; Don't do any window-related stuff if we weren't called from a
    ;; window displaying the calendar
    (when in-calendar-window
      (if (or (one-window-p t) (not (window-full-width-p)))
          ;; Don't mess with the window size, but ensure that the first
          ;; line is fully visible
          (set-window-vscroll nil 0)
        ;; Adjust the window to exactly fit the displayed calendar
        (fit-window-to-buffer nil nil calendar-minimum-window-height))
      (sit-for 0))
    (if (and (boundp 'font-lock-mode)
	     font-lock-mode)
	(font-lock-fontify-buffer))
    (and mark-holidays-in-calendar
;;;         (calendar-date-is-valid-p today) ; useful for BC dates
         (mark-calendar-holidays)
         (and in-calendar-window (sit-for 0)))
    (unwind-protect
        (if mark-diary-entries-in-calendar (mark-diary-entries))
      (if today-visible
          (run-hooks 'today-visible-calendar-hook)
        (run-hooks 'today-invisible-calendar-hook)))))

(defun generate-calendar (month year)
  "Generate a three-month Gregorian calendar centered around MONTH, YEAR."
  ;; A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
  ;; Note that while calendars for years BC could be displayed as it
  ;; stands, almost all other calendar functions (eg holidays) would
  ;; at best have unpredictable results for such dates.
  (if (< (+ month (* 12 (1- year))) 2)
      (error "Months before January, 1 AD cannot be displayed"))
  (setq displayed-month month
        displayed-year year)
  (erase-buffer)
  (increment-calendar-month month year -1)
  (dotimes (i 3)
    (generate-calendar-month month year (+ 5 (* 25 i)))
    (increment-calendar-month month year 1)))

(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted at the top of the buffer in which point is currently
located, but indented INDENT spaces.  The indentation is done from the first
character on the line and does not disturb the first INDENT characters on the
line."
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
   ;; Use the first two characters of each day to head the columns.
   (dotimes (i 7)
     (insert
      (let ((string
             (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t)))
        (if enable-multibyte-characters
            (truncate-string-to-width string 2)
          (substring string 0 2)))
      " "))
   (calendar-insert-indented "" 0 t);; Force onto following line
   (calendar-insert-indented "" indent);; Go to proper spot
   ;; Add blank days before the first of the month
   (dotimes (idummy blank-days) (insert "   "))
   ;; Put in the days of the month
   (calendar-for-loop i from 1 to last do
      (insert (format "%2d " i))
      (add-text-properties
       (- (point) 3) (1- (point))
       '(mouse-face highlight
	 help-echo "mouse-2: menu of operations for this date"))
      (and (zerop (mod (+ i blank-days) 7))
           (/= i last)
           (calendar-insert-indented "" 0 t)    ;; Force onto following line
           (calendar-insert-indented "" indent)))));; Go to proper spot

(defun calendar-insert-indented (string indent &optional newline)
  "Insert STRING at column INDENT.
If the optional parameter NEWLINE is t, leave point at start of next line,
inserting a newline if there was no next line; otherwise, leave point after
the inserted text.  Returns t."
  ;; Try to move to that column.
  (move-to-column indent)
  ;; If line is too short, indent out to that column.
  (if (< (current-column) indent)
      (indent-to indent))
  (insert string)
  ;; Advance to next line, if requested.
  (when newline
    (end-of-line)
    (if (eobp)
        (newline)
      (forward-line 1)))
  t)

(defun redraw-calendar ()
  "Redraw the calendar display, if `calendar-buffer' is live."
  (interactive)
  (if (get-buffer calendar-buffer)
      (with-current-buffer calendar-buffer
        (let ((cursor-date (calendar-cursor-to-nearest-date)))
          (generate-calendar-window displayed-month displayed-year)
          (calendar-cursor-to-visible-date cursor-date)))))

(defvar calendar-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (dolist (c '(narrow-to-region mark-word mark-sexp mark-paragraph
                 mark-defun mark-whole-buffer mark-page
                 downcase-region upcase-region kill-region
                 copy-region-as-kill capitalize-region write-region))
      (define-key map (vector 'remap c) 'calendar-not-implemented))
    (define-key map "<"     'calendar-scroll-right)
    (define-key map "\C-x<" 'calendar-scroll-right)
    (define-key map [prior] 'calendar-scroll-right-three-months)
    (define-key map "\ev"   'calendar-scroll-right-three-months)
    (define-key map ">"     'calendar-scroll-left)
    (define-key map "\C-x>" 'calendar-scroll-left)
    (define-key map [next]  'calendar-scroll-left-three-months)
    (define-key map "\C-v"  'calendar-scroll-left-three-months)
    (define-key map "\C-b"  'calendar-backward-day)
    (define-key map "\C-p"  'calendar-backward-week)
    (define-key map "\e{"   'calendar-backward-month)
    (define-key map "\C-x[" 'calendar-backward-year)
    (define-key map "\C-f"  'calendar-forward-day)
    (define-key map "\C-n"  'calendar-forward-week)
    (define-key map [left]  'calendar-backward-day)
    (define-key map [up]    'calendar-backward-week)
    (define-key map [right] 'calendar-forward-day)
    (define-key map [down]  'calendar-forward-week)
    (define-key map "\e}"   'calendar-forward-month)
    (define-key map "\C-x]" 'calendar-forward-year)
    (define-key map "\C-a"  'calendar-beginning-of-week)
    (define-key map "\C-e"  'calendar-end-of-week)
    (define-key map "\ea"   'calendar-beginning-of-month)
    (define-key map "\ee"   'calendar-end-of-month)
    (define-key map "\e<"   'calendar-beginning-of-year)
    (define-key map "\e>"   'calendar-end-of-year)
    (define-key map "\C-@"  'calendar-set-mark)
    ;; Many people are used to typing C-SPC and getting C-@.
    (define-key map [?\C-\s] 'calendar-set-mark)
    (define-key map "\C-x\C-x" 'calendar-exchange-point-and-mark)
    (define-key map "\e=" 'calendar-count-days-region)
    (define-key map "gd"  'calendar-goto-date)
    (define-key map "gD"  'calendar-goto-day-of-year)
    (define-key map "gj"  'calendar-goto-julian-date)
    (define-key map "ga"  'calendar-goto-astro-day-number)
    (define-key map "gh"  'calendar-goto-hebrew-date)
    (define-key map "gi"  'calendar-goto-islamic-date)
    (define-key map "gb"  'calendar-bahai-goto-date)
    (define-key map "gC"  'calendar-goto-chinese-date)
    (define-key map "gk"  'calendar-goto-coptic-date)
    (define-key map "ge"  'calendar-goto-ethiopic-date)
    (define-key map "gp"  'calendar-goto-persian-date)
    (define-key map "gc"  'calendar-goto-iso-date)
    (define-key map "gw"  'calendar-goto-iso-week)
    (define-key map "gf"  'calendar-goto-french-date)
    (define-key map "gml"  'calendar-goto-mayan-long-count-date)
    (define-key map "gmpc" 'calendar-previous-calendar-round-date)
    (define-key map "gmnc" 'calendar-next-calendar-round-date)
    (define-key map "gmph" 'calendar-previous-haab-date)
    (define-key map "gmnh" 'calendar-next-haab-date)
    (define-key map "gmpt" 'calendar-previous-tzolkin-date)
    (define-key map "gmnt" 'calendar-next-tzolkin-date)
    (define-key map "Aa"   'appt-add)
    (define-key map "Ad"   'appt-delete)
    (define-key map "S"   'calendar-sunrise-sunset)
    (define-key map "M"   'calendar-phases-of-moon)
    (define-key map " "   'scroll-other-window)
    (define-key map (kbd "DEL") 'scroll-other-window-down)
    (define-key map "\C-c\C-l" 'redraw-calendar)
    (define-key map "."   'calendar-goto-today)
    (define-key map "o"   'calendar-other-month)
    (define-key map "q"   'exit-calendar)
    (define-key map "a"   'calendar-list-holidays)
    (define-key map "h"   'calendar-cursor-holidays)
    (define-key map "x"   'mark-calendar-holidays)
    (define-key map "u"   'calendar-unmark)
    (define-key map "m"   'mark-diary-entries)
    (define-key map "d"   'diary-view-entries)
    (define-key map "D"   'view-other-diary-entries)
    (define-key map "s"   'diary-show-all-entries)
    (define-key map "pd"  'calendar-print-day-of-year)
    (define-key map "pC"  'calendar-print-chinese-date)
    (define-key map "pk"  'calendar-print-coptic-date)
    (define-key map "pe"  'calendar-print-ethiopic-date)
    (define-key map "pp"  'calendar-print-persian-date)
    (define-key map "pc"  'calendar-print-iso-date)
    (define-key map "pj"  'calendar-print-julian-date)
    (define-key map "pa"  'calendar-print-astro-day-number)
    (define-key map "ph"  'calendar-print-hebrew-date)
    (define-key map "pi"  'calendar-print-islamic-date)
    (define-key map "pb"  'calendar-bahai-print-date)
    (define-key map "pf"  'calendar-print-french-date)
    (define-key map "pm"  'calendar-print-mayan-date)
    (define-key map "po"  'calendar-print-other-dates)
    (define-key map "id"  'insert-diary-entry)
    (define-key map "iw"  'insert-weekly-diary-entry)
    (define-key map "im"  'insert-monthly-diary-entry)
    (define-key map "iy"  'insert-yearly-diary-entry)
    (define-key map "ia"  'insert-anniversary-diary-entry)
    (define-key map "ib"  'insert-block-diary-entry)
    (define-key map "ic"  'insert-cyclic-diary-entry)
    (define-key map "ihd" 'insert-hebrew-diary-entry)
    (define-key map "ihm" 'insert-monthly-hebrew-diary-entry)
    (define-key map "ihy" 'insert-yearly-hebrew-diary-entry)
    (define-key map "iid" 'insert-islamic-diary-entry)
    (define-key map "iim" 'insert-monthly-islamic-diary-entry)
    (define-key map "iiy" 'insert-yearly-islamic-diary-entry)
    (define-key map "iBd" 'diary-bahai-insert-entry)
    (define-key map "iBm" 'diary-bahai-insert-monthly-entry)
    (define-key map "iBy" 'diary-bahai-insert-yearly-entry)
    (define-key map "?"   'calendar-goto-info-node)
    (define-key map "Hm" 'cal-html-cursor-month)
    (define-key map "Hy" 'cal-html-cursor-year)
    (define-key map "tm" 'cal-tex-cursor-month)
    (define-key map "tM" 'cal-tex-cursor-month-landscape)
    (define-key map "td" 'cal-tex-cursor-day)
    (define-key map "tw1" 'cal-tex-cursor-week)
    (define-key map "tw2" 'cal-tex-cursor-week2)
    (define-key map "tw3" 'cal-tex-cursor-week-iso)
    (define-key map "tw4" 'cal-tex-cursor-week-monday)
    (define-key map "tfd" 'cal-tex-cursor-filofax-daily)
    (define-key map "tfw" 'cal-tex-cursor-filofax-2week)
    (define-key map "tfW" 'cal-tex-cursor-filofax-week)
    (define-key map "tfy" 'cal-tex-cursor-filofax-year)
    (define-key map "ty" 'cal-tex-cursor-year)
    (define-key map "tY" 'cal-tex-cursor-year-landscape)

    (define-key map [menu-bar edit] 'undefined)
    (define-key map [menu-bar search] 'undefined)
    ;; This ignores the mouse-up event after the mouse-down that pops up the
    ;; context menu.  It should not be necessary because the mouse-up event
    ;; should be eaten up by the menu-handling toolkit.
    ;; (define-key map [mouse-2] 'ignore)

    (easy-menu-define nil map nil cal-menu-moon-menu)
    (easy-menu-define nil map nil cal-menu-diary-menu)
    (easy-menu-define nil map nil cal-menu-holidays-menu)
    (easy-menu-define nil map nil cal-menu-goto-menu)
    (easy-menu-define nil map nil cal-menu-scroll-menu)

    (define-key map [down-mouse-3]
      (easy-menu-binding cal-menu-context-mouse-menu))
    (define-key map [down-mouse-2]
      (easy-menu-binding cal-menu-global-mouse-menu))

    map))

(defun describe-calendar-mode ()
  "Create a help buffer with a brief description of the `calendar-mode'."
  (interactive)
  (help-setup-xref (list #'describe-calendar-mode) (interactive-p))
  (with-output-to-temp-buffer (help-buffer)
    (princ
     (format
      "Calendar Mode:\nFor a complete description, type %s\n%s\n"
      (substitute-command-keys
       "\\<calendar-mode-map>\\[describe-mode] from within the calendar")
      (substitute-command-keys "\\{calendar-mode-map}")))
    (print-help-return-message)))

;; Calendar mode is suitable only for specially formatted data.
(put 'calendar-mode 'mode-class 'special)

(defvar calendar-mode-line-format
  (list
   (propertize "<"
	       'help-echo "mouse-1: previous month"
	       'mouse-face 'mode-line-highlight
	       'keymap (make-mode-line-mouse-map 'mouse-1
						 'calendar-scroll-right))
   "Calendar"
   (concat
    (propertize
     (substitute-command-keys
      "\\<calendar-mode-map>\\[calendar-goto-info-node] info")
     'help-echo "mouse-1: read Info on Calendar"
     'mouse-face 'mode-line-highlight
     'keymap (make-mode-line-mouse-map 'mouse-1 'calendar-goto-info-node))
    " / "
    (propertize
     (substitute-command-keys
     " \\<calendar-mode-map>\\[calendar-other-month] other")
     'help-echo "mouse-1: choose another month"
     'mouse-face 'mode-line-highlight
     'keymap (make-mode-line-mouse-map
	      'mouse-1 'mouse-calendar-other-month))
    " / "
    (propertize
     (substitute-command-keys
     "\\<calendar-mode-map>\\[calendar-goto-today] today")
     'help-echo "mouse-1: go to today's date"
     'mouse-face 'mode-line-highlight
     'keymap (make-mode-line-mouse-map 'mouse-1 #'calendar-goto-today)))
   '(calendar-date-string (calendar-current-date) t)
   (propertize ">"
	       'help-echo "mouse-1: next month"
	       'mouse-face 'mode-line-highlight
	       'keymap (make-mode-line-mouse-map
			'mouse-1 'calendar-scroll-left)))
  "The mode line of the calendar buffer.

This must be a list of items that evaluate to strings--those strings are
evaluated and concatenated together, evenly separated by blanks.  The variable
`date' is available for use as the date under (or near) the cursor; `date'
defaults to the current date if it is otherwise undefined.  Here is an example
value that has the Hebrew date, the day number/days remaining in the year,
and the ISO week/year numbers in the mode.  When `calendar-move-hook' is set
to `update-calendar-mode-line', these mode line shows these values for the date
under the cursor:

      (list
       \"\"
       '(calendar-hebrew-date-string date)
       '(let* ((year (extract-calendar-year date))
               (d (calendar-day-number date))
               (days-remaining
                (- (calendar-day-number (list 12 31 year)) d)))
          (format \"%d/%d\" d days-remaining))
       '(let* ((d (calendar-absolute-from-gregorian date))
               (iso-date (calendar-iso-from-absolute d)))
          (format \"ISO week %d of %d\"
            (extract-calendar-month iso-date)
            (extract-calendar-year iso-date)))
       \"\"))")

(defun mouse-calendar-other-month (event)
  "Display a three-month calendar centered around a specified month and year."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (call-interactively 'calendar-other-month)))

(defun calendar-goto-info-node ()
  "Go to the info node for the calendar."
  (interactive)
  (info "(emacs)Calendar/Diary"))

(defvar calendar-mark-ring nil
  "Used by `calendar-set-mark'.")

(defvar calendar-starred-day nil
  "Stores the value of the last date that `calendar-star-date' replaced.")

(defun calendar-mode ()
  "A major mode for the calendar window.

For a complete description, type \
\\<calendar-mode-map>\\[calendar-goto-info-node] from within the calendar.

\\<calendar-mode-map>\\{calendar-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'calendar-mode
        mode-name "Calendar"
        buffer-read-only t
        indent-tabs-mode nil)
  (use-local-map calendar-mode-map)
  (update-calendar-mode-line)
  (make-local-variable 'calendar-mark-ring)
  (make-local-variable 'calendar-starred-day)
  (make-local-variable 'displayed-month) ;; Month in middle of window.
  (make-local-variable 'displayed-year)  ;; Year in middle of window.
  ;; Most functions only work if displayed-month and displayed-year are set,
  ;; so let's make sure they're always set.  Most likely, this will be reset
  ;; soon in generate-calendar, but better safe than sorry.
  (unless (boundp 'displayed-month) (setq displayed-month 1))
  (unless (boundp 'displayed-year)  (setq displayed-year  2001))
  (set (make-local-variable 'font-lock-defaults)
       '(calendar-font-lock-keywords t))
  (run-mode-hooks 'calendar-mode-hook))

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
    (dolist (string strings)
      (setq s (concat s
                      (make-string (max 0 (/ (+ n i) m)) char)
                      string))
      (setq i (1+ i)))
    (substring s 0 length)))

(defun update-calendar-mode-line ()
  "Update the calendar mode line with the current date and date style."
  (if (bufferp (get-buffer calendar-buffer))
      (with-current-buffer calendar-buffer
        (setq mode-line-format
              (calendar-string-spread
               (let ((date (condition-case nil
                               (calendar-cursor-to-nearest-date)
                             (error (calendar-current-date)))))
                 (mapcar 'eval  calendar-mode-line-format))
               ?  (frame-width)))
        (force-mode-line-update))))

(defun calendar-window-list ()
  "List of all calendar-related windows."
  (let ((calendar-buffers (calendar-buffer-list))
        list)
    (walk-windows (lambda (w)
		    (if (memq (window-buffer w) calendar-buffers)
			(push w list)))
                  nil t)
    list))

(defun calendar-buffer-list ()
  "List of all calendar-related buffers."
  (let* ((diary-buffer (get-file-buffer diary-file))
         (buffers (list "*Yahrzeits*" lunar-phases-buffer holiday-buffer
                        fancy-diary-buffer diary-buffer calendar-buffer
                        other-calendars-buffer))
         (buffer-list nil))
    (dolist (b buffers)
      (setq b (cond ((stringp b) (get-buffer b))
                    ((bufferp b) b)
                    (t nil)))
      (if b (push b buffer-list)))
    buffer-list))

(defun exit-calendar ()
  "Get out of the calendar window and hide it and related buffers."
  (interactive)
  (let* ((diary-buffer (get-file-buffer diary-file)))
    (if (or (not diary-buffer)
            (not (buffer-modified-p diary-buffer))
            (yes-or-no-p
             "Diary modified; do you really want to exit the calendar? "))
        ;; Need to do this multiple times because one time can replace some
        ;; calendar-related buffers with other calendar-related buffers
        (mapc (lambda (x)
                (mapc 'calendar-hide-window (calendar-window-list)))
              (calendar-window-list)))))

(defun calendar-hide-window (window)
  "Hide WINDOW if it is calendar-related."
  (let ((buffer (if (window-live-p window) (window-buffer window))))
    (if (memq buffer (calendar-buffer-list))
        (cond
         ((and (display-multi-frame-p)
               (eq 'icon (cdr (assoc 'visibility
                                     (frame-parameters
                                      (window-frame window))))))
          nil)
         ((and (display-multi-frame-p) (window-dedicated-p window))
          (if calendar-remove-frame-by-deleting
              (delete-frame (window-frame window))
              (iconify-frame (window-frame window))))
         ((not (and (select-window window) (one-window-p window)))
          (delete-window window))
         (t (set-buffer buffer)
            (bury-buffer))))))

(defun calendar-current-date ()
  "Return the current date in a list (month day year)."
  (let ((now (decode-time)))
    (list (nth 4 now) (nth 3 now) (nth 5 now))))

(defun calendar-cursor-to-date (&optional error)
  "Return a list (month day year) of current cursor position.
If cursor is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (let* ((segment (/ (current-column) 25))
         (month (% (+ displayed-month segment -1) 12))
         (month (if (zerop month) 12 month))
         (year
          (cond
           ((and (=  12 month) (zerop segment)) (1- displayed-year))
           ((and (=   1 month) (= segment 2)) (1+ displayed-year))
           (t displayed-year))))
    (if (and (looking-at "[ 0-9]?[0-9][^0-9]")
             (< 2 (count-lines (point-min) (point))))
        (save-excursion
          (if (not (looking-at " "))
              (re-search-backward "[^0-9]"))
          (list month
                (string-to-number (buffer-substring (1+ (point)) (+ 4 (point))))
                year))
      (if (and (looking-at "\\*")
               (save-excursion
                 (re-search-backward "[^*]")
                 (looking-at ".\\*\\*")))
          (list month calendar-starred-day year)
        (if error (error "Not on a date!"))))))

(add-to-list 'debug-ignored-errors "Not on a date!")

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
Gregorian date Sunday, December 31, 1 BC.  This function does not
handle dates in years BC."
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
          (push date calendar-mark-ring)
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


(defvar calendar-abbrev-length 3
  "*Length of abbreviations to be used for day and month names.
See also `calendar-day-abbrev-array' and `calendar-month-abbrev-array'.")

(defvar calendar-day-name-array
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "*Array of capitalized strings giving, in order, the day names.
The first two characters of each string will be used to head the
day columns in the calendar.  See also the variable
`calendar-day-abbrev-array'.")

(defvar calendar-day-abbrev-array
  [nil nil nil nil nil nil nil]
  "*Array of capitalized strings giving the abbreviated day names.
The order should be the same as that of the full names specified
in `calendar-day-name-array'.  These abbreviations may be used
instead of the full names in the diary file.  Do not include a
trailing `.' in the strings specified in this variable, though
you may use such in the diary file.  If any element of this array
is nil, then the abbreviation will be constructed as the first
`calendar-abbrev-length' characters of the corresponding full name.")

(defvar calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"]
  "*Array of capitalized strings giving, in order, the month names.
See also the variable `calendar-month-abbrev-array'.")

(defvar calendar-month-abbrev-array
  [nil nil nil nil nil nil nil nil nil nil nil nil]
 "*Array of capitalized strings giving the abbreviated month names.
The order should be the same as that of the full names specified
in `calendar-month-name-array'.  These abbreviations are used in
the calendar menu entries, and can also be used in the diary
file.  Do not include a trailing `.' in the strings specified in
this variable, though you may use such in the diary file.  If any
element of this array is nil, then the abbreviation will be
constructed as the first `calendar-abbrev-length' characters of the
corresponding full name.")


(defun calendar-read-date (&optional noday)
  "Prompt for Gregorian date.  Return a list (month day year).
If optional NODAY is t, does not ask for day, but just returns
\(month nil year); if NODAY is any other non-nil value the value returned is
\(month year)"
  (let* ((year (calendar-read
                "Year (>0): "
                (lambda (x) (> x 0))
                (int-to-string (extract-calendar-year
                                (calendar-current-date)))))
         (month-array calendar-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc-string
                       (completing-read
                        "Month name: "
                        (mapcar 'list (append month-array nil))
                        nil t)
                      (calendar-make-alist month-array 1) t)))
         (last (calendar-last-day-of-month month year)))
    (if noday
        (if (eq noday t)
            (list month nil year)
          (list month year))
      (list month
            (calendar-read (format "Day (1-%d): " last)
			   (lambda (x) (and (< 0 x) (<= x last))))
            year))))

(defun calendar-interval (mon1 yr1 mon2 yr2)
  "The number of months difference between MON1, YR1 and MON2, YR2.
The result is positive if the second date is later than the first.
Negative years are interpreted as years BC; -1 being 1 BC, and so on."
  (if (< yr1 0) (setq yr1 (1+ yr1)))      ; -1 BC -> 0 AD, etc
  (if (< yr2 0) (setq yr2 (1+ yr2)))
  (+ (* 12 (- yr2 yr1))
     (- mon2 mon1)))

(defun calendar-abbrev-construct (abbrev full &optional period)
  "Internal calendar function to return a complete abbreviation array.
ABBREV is an array of abbreviations, FULL the corresponding array
of full names.  The return value is the ABBREV array, with any nil
elements replaced by the first three characters taken from the
corresponding element of FULL.  If optional argument PERIOD is non-nil,
each element returned has a final `.' character."
  (let (elem array name)
    (dotimes (i (length full))
      (setq name (aref full i)
            elem (or (aref abbrev i)
                     (substring name 0
                                (min calendar-abbrev-length (length name))))
            elem (format "%s%s" elem (if period "." ""))
            array (append array (list elem))))
    (vconcat array)))

(defvar calendar-font-lock-keywords
  `((,(concat (regexp-opt (mapcar 'identity calendar-month-name-array) t)
	      " -?[0-9]+")
     . font-lock-function-name-face) ; month and year
    (,(regexp-opt
       (list (substring (aref calendar-day-name-array 6) 0 2)
	     (substring (aref calendar-day-name-array 0) 0 2)))
     ;; Saturdays and Sundays are hilited differently.
     . font-lock-comment-face)
    ;; First two chars of each day are used in the calendar.
    (,(regexp-opt (mapcar (lambda (x) (substring x 0 2)) calendar-day-name-array))
     . font-lock-reference-face))
  "Default keywords to highlight in Calendar mode.")

(defun calendar-day-name (date &optional abbrev absolute)
  "Return a string with the name of the day of the week of DATE.
DATE should be a list in the format (MONTH DAY YEAR), unless the
optional argument ABSOLUTE is non-nil, in which case DATE should
be an integer in the range 0 to 6 corresponding to the day of the
week.  Day names are taken from the variable `calendar-day-name-array',
unless the optional argument ABBREV is non-nil, in which case
the variable `calendar-day-abbrev-array' is used."
  (aref (if abbrev
            (calendar-abbrev-construct calendar-day-abbrev-array
                                       calendar-day-name-array)
          calendar-day-name-array)
        (if absolute date (calendar-day-of-week date))))

(defun calendar-make-alist (sequence &optional start-index filter abbrevs)
  "Make an assoc list corresponding to SEQUENCE.
Each element of sequence will be associated with an integer, starting
from 1, or from START-INDEX if that is non-nil.  If a sequence ABBREVS
is supplied, the function `calendar-abbrev-construct' is used to
construct abbreviations corresponding to the elements in SEQUENCE.
Each abbreviation is entered into the alist with the same
association index as the full name it represents.
If FILTER is provided, apply it to each key in the alist."
  (let ((index 0)
        (offset (or start-index 1))
        (aseq (if abbrevs (calendar-abbrev-construct abbrevs sequence)))
        (aseqp (if abbrevs (calendar-abbrev-construct abbrevs sequence
                                                      'period)))
        alist elem)
    (dotimes (i (length sequence) (reverse alist))
      (setq index (+ i offset)
            elem (elt sequence i)
            alist
            (cons (cons (if filter (funcall filter elem) elem) index) alist))
      (if aseq
          (setq elem (elt aseq i)
                alist (cons (cons (if filter (funcall filter elem) elem)
                                  index) alist)))
      (if aseqp
          (setq elem (elt aseqp i)
                alist (cons (cons (if filter (funcall filter elem) elem)
                                  index) alist))))))

(defun calendar-month-name (month &optional abbrev)
  "Return a string with the name of month number MONTH.
Months are numbered from one.  Month names are taken from the
variable `calendar-month-name-array', unless the optional
argument ABBREV is non-nil, in which case
`calendar-month-abbrev-array' is used."
  (aref (if abbrev
            (calendar-abbrev-construct calendar-month-abbrev-array
                                       calendar-month-name-array)
          calendar-month-name-array)
        (1- month)))

(defun calendar-day-of-week (date)
  "Return the day-of-the-week index of DATE, 0 for Sunday, 1 for Monday, etc.
DATE is a list of the form (month day year).  A negative year is
interpreted as BC; -1 being 1 BC, and so on."
  (mod (calendar-absolute-from-gregorian date) 7))

(defun calendar-unmark ()
  "Delete all diary/holiday marks/highlighting from the calendar."
  (interactive)
  (setq mark-holidays-in-calendar nil)
  (setq mark-diary-entries-in-calendar nil)
  (redraw-calendar))

(defun calendar-date-is-visible-p (date)
  "Return t if DATE is valid and is visible in the calendar window."
  (let ((gap (calendar-interval
              displayed-month displayed-year
              (extract-calendar-month date) (extract-calendar-year date))))
    (and (calendar-date-is-valid-p date) (> 2 gap) (< -2 gap))))

(defun calendar-date-is-valid-p (date)
  "Return t if DATE is a valid date."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (and (<= 1 month) (<= month 12)
         ;; (calendar-read-date t) returns a date with day = nil.
         ;; Should not be valid (?), since many funcs prob assume integer.
         ;; (calendar-read-date 'noday) returns (month year), which
         ;; currently results in extract-calendar-year returning nil.
         day year (<= 1 day) (<= day (calendar-last-day-of-month month year))
         ;; BC dates left as non-valid, to suppress errors from
         ;; complex holiday algorithms not suitable for years BC.
         ;; Note there are side effects on calendar navigation.
         (<= 1 year))))

(define-obsolete-function-alias 'calendar-date-is-legal-p
    'calendar-date-is-valid-p "23.1")

(defun calendar-date-equal (date1 date2)
  "Return t if the DATE1 and DATE2 are the same."
  (and
   (= (extract-calendar-month date1) (extract-calendar-month date2))
   (= (extract-calendar-day date1) (extract-calendar-day date2))
   (= (extract-calendar-year date1) (extract-calendar-year date2))))

(defun mark-visible-calendar-date (date &optional mark)
  "Mark DATE in the calendar window with MARK.
MARK is a single-character string, a list of face attributes/values, or a face.
MARK defaults to `diary-entry-marker'."
  (if (calendar-date-is-valid-p date)
      (with-current-buffer calendar-buffer
        (save-excursion
          (calendar-cursor-to-visible-date date)
          (setq mark
                (or (and (stringp mark) (= (length mark) 1) mark) ; single-char
                    (and (listp mark) (> (length mark) 0) mark)   ; attr list
                    (and (facep mark) mark)                       ; face-name
                    diary-entry-marker))
          (cond
           ;; face or an attr-list that contained a face
           ((facep mark)
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face mark))
           ;; single-char
           ((and (stringp mark) (= (length mark) 1))
            (let ((inhibit-read-only t))
              (forward-char 1)
              ;; Insert before delete so as to better preserve markers.
              (insert mark)
              (delete-char 1)
              (forward-char -2)))
           (t ;; attr list
            (let ((temp-face
                   (make-symbol
                    (apply 'concat "temp-"
                           (mapcar (lambda (sym)
                                     (cond
                                      ((symbolp sym) (symbol-name sym))
                                      ((numberp sym) (number-to-string sym))
                                      (t sym)))
                                   mark))))
                  (faceinfo mark))
              (make-face temp-face)
              ;; Remove :face info from the mark, copy the face info into
              ;; temp-face
              (while (setq faceinfo (memq :face faceinfo))
                (copy-face (read (nth 1 faceinfo)) temp-face)
                (setcar faceinfo nil)
                (setcar (cdr faceinfo) nil))
              (setq mark (delq nil mark))
              ;; Apply the font aspects
              (apply 'set-face-attribute temp-face nil mark)
              (overlay-put
               (make-overlay (1- (point)) (1+ (point))) 'face temp-face))))))))

(defun calendar-star-date ()
  "Replace the date under the cursor in the calendar window with asterisks.
This function can be used with the `today-visible-calendar-hook' run after the
calendar window has been prepared."
  (let ((inhibit-read-only t)
        (modified (buffer-modified-p)))
    (forward-char 1)
    (setq calendar-starred-day
          (string-to-number (buffer-substring (point) (- (point) 2))))
    ;; Insert before deleting, to better preserve markers.
    (insert "**")
    (forward-char -2)
    (delete-char -2)
    (forward-char 1)
    (restore-buffer-modified-p modified)))

(defun calendar-mark-today ()
  "Mark the date under the cursor in the calendar window.
The date is marked with `calendar-today-marker'.  This function can be used with
the `today-visible-calendar-hook' run after the calendar window has been
prepared."
  (mark-visible-calendar-date
   (calendar-cursor-to-date)
   calendar-today-marker))

(defun calendar-date-compare (date1 date2)
  "Return t if DATE1 is before DATE2, nil otherwise.
The actual dates are in the car of DATE1 and DATE2."
  (< (calendar-absolute-from-gregorian (car date1))
     (calendar-absolute-from-gregorian (car date2))))

(defun calendar-date-string (date &optional abbreviate nodayname)
  "A string form of DATE, driven by the variable `calendar-date-display-form'.
An optional parameter ABBREVIATE, when non-nil, causes the month
and day names to be abbreviated as specified by
`calendar-month-abbrev-array' and `calendar-day-abbrev-array',
respectively.  An optional parameter NODAYNAME, when t, omits the
name of the day of the week."
  (let* ((dayname
          (unless nodayname
            (calendar-day-name date abbreviate)))
         (month (extract-calendar-month date))
         (monthname (calendar-month-name month abbreviate))
         (day (int-to-string (extract-calendar-day date)))
         (month (int-to-string month))
         (year (int-to-string (extract-calendar-year date))))
    (mapconcat 'eval calendar-date-display-form "")))

(defun calendar-dayname-on-or-before (dayname date)
  "Return the absolute date of the DAYNAME on or before absolute DATE.
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

(defun calendar-print-other-dates ()
  "Show dates on other calendars for date under the cursor."
  (interactive)
  (let* ((date (calendar-cursor-to-date t)))
    (with-current-buffer (get-buffer-create other-calendars-buffer)
      (let ((inhibit-read-only t)
            (modified (buffer-modified-p)))
        (calendar-set-mode-line
         (concat (calendar-date-string date) " (Gregorian)"))
        (erase-buffer)
        (apply
         'insert
         (delq nil
               (list
                (calendar-day-of-year-string date) "\n"
                (format "ISO date: %s\n" (calendar-iso-date-string date))
                (format "Julian date: %s\n"
                        (calendar-julian-date-string date))
                (format "Astronomical (Julian) day number (at noon UTC): %s.0\n"
                        (calendar-astro-date-string date))
                (format "Fixed (RD) date: %s\n"
                        (calendar-absolute-from-gregorian date))
                (format "Hebrew date (before sunset): %s\n"
                        (calendar-hebrew-date-string date))
                (format "Persian date: %s\n"
                        (calendar-persian-date-string date))
                (let ((i (calendar-islamic-date-string date)))
                  (if (not (string-equal i ""))
                      (format "Islamic date (before sunset): %s\n" i)))
                (let ((b (calendar-bahai-date-string date)))
                  (if (not (string-equal b ""))
                      (format "Baha'i date (before sunset): %s\n" b)))
                (format "Chinese date: %s\n"
                        (calendar-chinese-date-string date))
                (let ((c (calendar-coptic-date-string date)))
                  (if (not (string-equal c ""))
                      (format "Coptic date: %s\n" c)))
                (let ((e (calendar-ethiopic-date-string date)))
                  (if (not (string-equal e ""))
                      (format "Ethiopic date: %s\n" e)))
                (let ((f (calendar-french-date-string date)))
                  (if (not (string-equal f ""))
                      (format "French Revolutionary date: %s\n" f)))
                (format "Mayan date: %s\n"
                        (calendar-mayan-date-string date)))))
        (goto-char (point-min))
        (restore-buffer-modified-p modified))
      (display-buffer other-calendars-buffer))))

(defun calendar-print-day-of-year ()
  "Show day number in year/days remaining in year for date under the cursor."
  (interactive)
  (message "%s" (calendar-day-of-year-string (calendar-cursor-to-date t))))

(defun calendar-set-mode-line (str)
  "Set mode line to STR, centered, surrounded by dashes."
  (let* ((edges (window-edges))
         ;; As per doc of window-width, total visible mode-line length.
         (width (- (nth 2 edges) (nth 0 edges))))
    (setq mode-line-format
          (if buffer-file-name
              `("-" mode-line-modified
                ,(calendar-string-spread (list str) ?- (- width 6))
                "---")
            (calendar-string-spread (list str) ?- width)))))

(defun calendar-mod (m n)
  "Non-negative remainder of M/N with N instead of 0."
  (1+ (mod (1- m) n)))


(defun calendar-version ()
  (interactive)
  (message "GNU Emacs %s" emacs-version))

(make-obsolete 'calendar-version 'emacs-version "23.1")


(run-hooks 'calendar-load-hook)

(provide 'calendar)

;; Local variables:
;; byte-compile-dynamic: t
;; End:

;; arch-tag: 19c61596-c8fb-4c69-bcf1-7dd739919cd8
;;; calendar.el ends here
