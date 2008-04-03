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
;; the astronomical (Julian) day number.  Times of sunrise/sunset can
;; be displayed, as can the phases of the moon.  Appointment
;; notification for diary entries is available.  Calendar printing via
;; LaTeX is available.

;; The following files are part of the calendar/diary code:

;;    appt.el                    Appointment notification
;;    cal-bahai.el               Baha'i calendar
;;    cal-china.el               Chinese calendar
;;    cal-coptic.el              Coptic/Ethiopic calendars
;;    cal-dst.el                 Daylight saving time rules
;;    cal-french.el              French revolutionary calendar
;;    cal-hebrew.el              Hebrew calendar
;;    cal-html.el                Calendars in HTML
;;    cal-islam.el               Islamic calendar
;;    cal-iso.el                 ISO calendar
;;    cal-julian.el              Julian/astronomical calendars
;;    cal-mayan.el               Mayan calendars
;;    cal-menu.el                Menu support
;;    cal-move.el                Movement in the calendar
;;    cal-persia.el              Persian calendar
;;    cal-tex.el                 Calendars in LaTeX
;;    cal-x.el                   Dedicated frame functions
;;    calendar.el                This file
;;    diary-lib.el               Diary functions
;;    holidays.el                Holiday functions
;;    lunar.el                   Phases of the moon
;;    solar.el                   Sunrise/sunset, equinoxes/solstices

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


;; A note on free variables:

;; The calendar passes around a few dynamically bound variables, which
;; unfortunately have rather common names.  They are meant to be
;; available for external functions, so the names can't be changed.

;; displayed-month, displayed-year: bound in generate-calendar, the
;;   central month of the 3 month calendar window
;; original-date, number: bound in diary-list-entries, the arguments
;;   with which that function was called.
;; date, entry: bound in list-sexp-diary-entries (qv)

;; Bound in diary-list-entries:
;; diary-entries-list: use in d-l, appt.el, and by add-to-diary-list
;; diary-saved-point: only used in diary-lib.el, passed to the display func
;; date-string: only used in diary-lib.el
;; list-only: don't modify the diary-buffer, just return a list of entries
;; file-glob-attrs: yuck

;;; Code:

;; (elisp) Eval During Compile: "Effectively `require' is
;; automatically `eval-and-compile'" [but `load' is not]
(eval-and-compile
  (load "cal-loaddefs" nil 'quiet))

;; Avoid recursive load of calendar when loading cal-menu.
(provide 'calendar)
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

(defcustom calendar-remove-frame-by-deleting t
  "Determine how the calendar mode removes a frame no longer needed.
If nil, make an icon of the frame.  If non-nil, delete the frame."
  :type 'boolean
  :version "23.1"                       ; changed from nil to t
  :group 'view
  :group 'calendar)

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
;; Backward-compatibility alias.  FIXME make obsolete.
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

;; These don't respect changes in font-lock-mode after loading.
(defcustom diary-entry-marker (if (and font-lock-mode (display-color-p))
                                  'diary
                                "+")
  "How to mark dates that have diary entries.
The value can be either a single-character string or a face."
  :type '(choice string face)
  :group 'diary)

(defcustom calendar-today-marker (if (and font-lock-mode (display-color-p))
                                     'calendar-today
                                   "=")
  "How to mark today's date in the calendar.
The value can be either a single-character string or a face.
Used by `calendar-mark-today'."
  :type '(choice string face)
  :group 'calendar)

(defcustom calendar-holiday-marker (if (and font-lock-mode (display-color-p))
                                       'holiday
                                     "*")
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
  "List of functions to be called when the calendar window is created.
Quitting the calendar and re-entering it will cause these functions
to be called again."
  :type 'hook
  :group 'calendar-hooks)

(defcustom today-visible-calendar-hook nil
  "List of functions called whenever the current date is visible.
To mark today's date, add the function `calendar-mark-today'.
To replace the date with asterisks, add the function `calendar-star-date'.
See also `today-invisible-calendar-hook'.

In general, be careful about changing characters in the calendar buffer,
since it may cause the movement commands to fail."
  :type 'hook
  :options '(calendar-mark-today calendar-star-date)
  :group 'calendar-hooks)

(defcustom today-invisible-calendar-hook nil
  "List of functions called whenever the current date is not visible.
See also `today-visible-calendar-hook'."
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
specified by the variable `diary-date-forms', which by default
uses the forms of `american-date-diary-pattern':

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
`calendar-month-name-array' and `calendar-day-name-array'), or
abbreviated (as specified by `calendar-month-abbrev-array' and
`calendar-day-abbrev-array') with or without a period.  Case is
ignored.  Any of DAY, MONTH, or MONTHNAME, YEAR can be `*' which
matches any day, month, or year, respectively.  If the date does
not contain a year, it is generic and applies to any year.  A
DAYNAME entry applies to the appropriate day of the week in every week.

You can customize `diary-date-forms' to your preferred format.
Three default styles are provided: `american-date-diary-pattern',
`european-date-diary-pattern', and `iso-date-diary-pattern'.
You can choose between these by setting `calendar-date-style' in your
.emacs file, or by using `calendar-set-date-style' when in the calendar.

A diary entry can be preceded by the character `diary-nonmarking-symbol'
\(ordinarily `&') to make that entry nonmarking--that is, it will not be
marked on dates in the calendar window but will appear in a diary window.

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
November 10, 1990.  See the documentation for the function
`list-sexp-diary-entries' for more details.

Diary entries based on the Hebrew, the Islamic and/or the Baha'i
calendar are also possible, but because these are somewhat slow, they
are ignored unless you set the `nongregorian-diary-listing-hook' and
the `nongregorian-diary-marking-hook' appropriately.  See the
documentation of these hooks for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `list-diary-entries-hook'."
  :type 'file
  :group 'diary)

;; FIXME do these have to be single characters?
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

;;;###autoload
(defcustom european-calendar-style nil
  "Non-nil means use the European style of dates in the diary and display.
In this case, a date like 1/2/1990 would be interpreted as
February 1, 1990.  See `european-date-diary-pattern' for the
default European diary date styles.

Setting this variable directly does not take effect (if the
calendar package is already loaded).  Rather, use either
\\[customize] or the function `calendar-set-date-style'."
  :type 'boolean
  ;; Without :initialize (require 'calendar) throws an error because
  ;; calendar-set-date-style is undefined at this point.
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (if value
             (calendar-set-date-style 'european)
           (calendar-set-date-style 'american)))
  :group 'diary)

;;;###autoload
(make-obsolete-variable 'european-calendar-style 'calendar-date-style "23.1")

;; Used by various other packages.
;;;###autoload
(defcustom calendar-date-style (if european-calendar-style 'european
                                 'american)
  "Your preferred style for writing dates.
The options are:
`american' - month/day/year
`european' - day/month/year
`iso'      - year/month/day
This affects how dates written in your diary are interpreted.
It also affects date display, as well as those calendar and diary
functions that take a date as an argument, e.g. `diary-date', by
changing the order in which the arguments are interpreted.

Setting this variable directly does not take effect (if the
calendar package is already loaded).  Rather, use either
\\[customize] or the function `calendar-set-date-style'."
  :version "23.1"
  :type '(choice (const american :tag "Month/Day/Year")
                 (const european :tag "Day/Month/Year")
                 (const iso      :tag "Year/Month/Day"))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (calendar-set-date-style value))
  :group 'calendar)

;; Next three are provided to aid in setting diary-date-forms.
(defcustom iso-date-diary-pattern
  '((month "[-/]" day "[^-/0-9]")
    (year "[-/]" month "[-/]" day "[^0-9]")
    (monthname "-" day "[^-0-9]")
    (year "-" monthname "-" day "[^0-9]")
    (dayname "\\W"))
    "List of pseudo-patterns describing the ISO style of dates.
The defaults are: MONTH[-/]DAY; YEAR[-/]MONTH[-/]DAY; MONTHNAME-DAY;
YEAR-MONTHNAME-DAY; DAYNAME.  Normally you should not customize this,
but `diary-date-forms' (which see)."
    :version "23.1"
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

(defcustom american-date-diary-pattern
  '((month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W"))
  "List of pseudo-patterns describing the American style of dates.
The defaults are: MONTH/DAY; MONTH/DAY/YEAR; MONTHNAME DAY;
MONTHNAME DAY, YEAR; DAYNAME.  Normally you should not customize this,
but `diary-date-forms' (which see)."
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
  "List of pseudo-patterns describing the European style of dates.
The defaults are: DAY/MONTH; DAY/MONTH/YEAR; DAY MONTHNAME;
DAY MONTHNAME YEAR; DAYNAME.  Normally you should not customize this, but
`diary-date-forms' (which see)."
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

(defvar diary-font-lock-keywords)

(defcustom diary-date-forms (cond ((eq calendar-date-style 'iso)
                                   iso-date-diary-pattern)
                                  ((eq calendar-date-style 'european)
                                   european-date-diary-pattern)
                                  (t american-date-diary-pattern))
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
a portion of the first word of the diary entry.

For examples of three common styles, see `american-date-diary-pattern',
`european-date-diary-pattern', and `iso-date-diary-pattern'."
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

;; Next three are provided to aid in setting calendar-date-display-form.
(defcustom iso-calendar-display-form '((format "%s-%.2d-%.2d" year
                                               (string-to-number month)
                                               (string-to-number day)))
  "Pseudo-pattern governing the way a date appears in the ISO style.
Normally you should not customize this, but `calendar-date-display-form'
\(which see)."
  :type 'sexp
  :version "23.1"
  :group 'calendar)

(defcustom european-calendar-display-form
  '((if dayname (concat dayname ", ")) day " " monthname " " year)
  "Pseudo-pattern governing the way a date appears in the European style.
Normally you should not customize this, but `calendar-date-display-form'
\(which see)."
  :type 'sexp
  :group 'calendar)

(defcustom american-calendar-display-form
  '((if dayname (concat dayname ", ")) monthname " " day ", " year)
  "Pseudo-pattern governing the way a date appears in the American style.
Normally you should not customize this, but `calendar-date-display-form'
\(which see)."
  :type 'sexp
  :group 'calendar)

(defcustom calendar-date-display-form (cond ((eq calendar-date-style 'iso)
                                             iso-calendar-display-form)
                                            ((eq calendar-date-style 'european)
                                             european-calendar-display-form)
                                            (t american-calendar-display-form))
  "Pseudo-pattern governing the way a calendar date appears.
Used by the function `calendar-date-string' (which see), a pseudo-pattern
is a list of expressions that can involve the keywords `month', `day',
and `year' (all numbers in string form), and `monthname' and `dayname'
\(both alphabetic strings).  For example, a typical American form would be

       '(month \"/\" day \"/\" (substring year -2))

whereas

       '((format \"%9s, %9s %2s, %4s\" dayname monthname day year))

would give the usual American style in fixed-length fields.  The variables
`iso-calendar-display-form', `european-calendar-display-form', and
`american-calendar-display-form' provide some defaults for three common
styles."
  :type 'sexp
  :group 'calendar)

(defun calendar-set-date-style (style)
  "Set the style of calendar and diary dates to STYLE (a symbol).
The valid styles are described in the documentation of `calendar-date-style'."
  (interactive (list (intern
                      (completing-read "Date style: "
                                       '("american" "european" "iso") nil t
                                       nil nil "american"))))
  (or (memq style '(american european iso))
      (setq style 'american))
  (setq calendar-date-style style
        calendar-date-display-form
        (symbol-value (intern-soft (format "%s-calendar-display-form" style)))
        diary-date-forms
        (symbol-value (intern-soft (format "%s-date-diary-pattern" style))))
  (update-calendar-mode-line))

(defun european-calendar ()
  "Set the interpretation and display of dates to the European style."
  (interactive)
  (calendar-set-date-style 'european))

(make-obsolete 'european-calendar 'calendar-set-date-style "23.1")

(defun american-calendar ()
  "Set the interpretation and display of dates to the American style."
  (interactive)
  (calendar-set-date-style 'american))

(make-obsolete 'american-calendar 'calendar-set-date-style "23.1")

;; FIXME move to diary-lib and adjust appt.
;; Add appt-make-list as an option?
(defcustom diary-hook nil
  "List of functions called after the display of the diary.
Can be used for appointment notification."
  :type 'hook
  :group 'diary)

(defcustom diary-display-hook nil
  "List of functions that handle the display of the diary.
If nil (the default), `simple-diary-display' is used.  Use
`ignore' for no diary display.

Ordinarily, this just displays the diary buffer (with holidays
indicated in the mode line), if there are any relevant entries.
At the time these functions are called, the variable
`diary-entries-list' is a list, in order by date, of all relevant
diary entries in the form of ((MONTH DAY YEAR) STRING), where
string is the diary entry for the given date.  This can be used,
for example, a different buffer for display (perhaps combined
with holidays), or produce hard copy output.

A function `fancy-diary-display' is provided for use with this
hook; this function prepares a special noneditable diary buffer
with the relevant diary entries that has neat day-by-day
arrangement with headings.  The fancy diary buffer will show the
holidays unless the variable `holidays-in-diary-buffer' is set to
nil.  Ordinarily, the fancy diary buffer will not show days for
which there are no diary entries, even if that day is a holiday;
if you want such days to be shown in the fancy diary buffer, set
the variable `diary-list-include-blanks' non-nil."
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

;; The various holiday variables are autoloaded because people
;; are used to using them to set calendar-holidays without having to
;; explicitly load this file.

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
  '((holiday-chinese-new-year))
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
         (let ((m displayed-month)
               (y displayed-year)
               year)
           (increment-calendar-month m y -1)
           (setq year (extract-calendar-year
                       (calendar-julian-from-absolute
                        (calendar-absolute-from-gregorian (list m 1 y)))))
           (if (zerop (% (1+ year) 4))
               22
             21)) "\"Tal Umatar\" (evening)")))
  "Component of the default value of `hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-1 'risky-local-variable t)
;;;###autoload
(make-obsolete-variable 'hebrew-holidays-1 'hebrew-holidays "23.1")

;;;###autoload
(defvar hebrew-holidays-2
  '((holiday-hanukkah)         ; respects all-hebrew-calendar-holidays
    (if all-hebrew-calendar-holidays
      (holiday-hebrew
       10
       (let ((h-year (extract-calendar-year
                      (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (list displayed-month 28 displayed-year))))))
         (if (= 6 (% (calendar-absolute-from-hebrew (list 10 10 h-year))
                     7))
             11 10))
       "Tzom Teveth"))
    (if all-hebrew-calendar-holidays
        (holiday-hebrew 11 15 "Tu B'Shevat")))
  "Component of the default value of `hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-2 'risky-local-variable t)
;;;###autoload
(make-obsolete-variable 'hebrew-holidays-2 'hebrew-holidays "23.1")

;;;###autoload
(defvar hebrew-holidays-3
  '((if all-hebrew-calendar-holidays
        (holiday-hebrew
         11
         (let* ((m displayed-month)
                (y displayed-year)
                (h-year (progn
                          (increment-calendar-month m y 1)
                          (extract-calendar-year
                           (calendar-hebrew-from-absolute
                            (calendar-absolute-from-gregorian
                             (list m (calendar-last-day-of-month m y) y))))))
                (s-s
                 (calendar-hebrew-from-absolute
                  (if (= 6
                         (% (calendar-absolute-from-hebrew
                             (list 7 1 h-year))
                            7))
                      (calendar-dayname-on-or-before
                       6 (calendar-absolute-from-hebrew
                          (list 11 17 h-year)))
                    (calendar-dayname-on-or-before
                     6 (calendar-absolute-from-hebrew
                        (list 11 16 h-year))))))
                (day (extract-calendar-day s-s)))
           day)
         "Shabbat Shirah")))
  "Component of the default value of `hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-3 'risky-local-variable t)
;;;###autoload
(make-obsolete-variable 'hebrew-holidays-3 'hebrew-holidays "23.1")

;;;###autoload
(defvar hebrew-holidays-4
  '((holiday-passover-etc)
    (and all-hebrew-calendar-holidays
         (let* ((m displayed-month)
                (y displayed-year)
                (year (progn
                        (increment-calendar-month m y -1)
                        (extract-calendar-year
                         (calendar-julian-from-absolute
                          (calendar-absolute-from-gregorian (list m 1 y)))))))
           (= 21 (% year 28)))
         (holiday-julian 3 26 "Kiddush HaHamah"))
    (if all-hebrew-calendar-holidays
        (holiday-tisha-b-av-etc)))
    "Component of the default value of `hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-4 'risky-local-variable t)
;;;###autoload
(make-obsolete-variable 'hebrew-holidays-4 'hebrew-holidays "23.1")

;;;###autoload
(defcustom hebrew-holidays
  '((holiday-passover-etc)
    (holiday-rosh-hashanah-etc)
    (holiday-hanukkah)
    (if all-hebrew-calendar-holidays
        (append
         (holiday-tisha-b-av-etc)
         (holiday-hebrew-misc))))
  "Jewish holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :version "23.1"            ; removed dependency on hebrew-holidays-N
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
  '((holiday-easter-etc)    ; respects all-christian-calendar-holidays
    (holiday-fixed 12 25 "Christmas")
    (if all-christian-calendar-holidays
        (append
         (holiday-fixed 1 6 "Epiphany")
         (holiday-julian 12 25 "Eastern Orthodox Christmas")
         (holiday-greek-orthodox-easter)
         (holiday-fixed 8 15 "Assumption")
         (holiday-advent 0 "Advent"))))
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
  '((holiday-islamic-new-year)
    (holiday-islamic 9 1 "Ramadan Begins")
    (if all-islamic-calendar-holidays
        (append
         (holiday-islamic 1 10 "Ashura")
         (holiday-islamic 3 12 "Mulad-al-Nabi")
         (holiday-islamic 7 26 "Shab-e-Mi'raj")
         (holiday-islamic 8 15 "Shab-e-Bara't")
         (holiday-islamic 9 27 "Shab-e Qadr")
         (holiday-islamic 10 1 "Id-al-Fitr")
         (holiday-islamic 12 10 "Id-al-Adha"))))
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
  '((holiday-bahai-new-year)
    (holiday-bahai-ridvan)      ; respects all-bahai-calendar-holidays
    (if all-bahai-calendar-holidays
        (append
         (holiday-fixed 11 26 "Day of the Covenant")
         (holiday-fixed 11 28 "Ascension of `Abdu'l-Baha")))
    (holiday-fixed  5 23 "Declaration of the Bab")
    (holiday-fixed  5 29 "Ascension of Baha'u'llah")
    (holiday-fixed  7  9 "Martyrdom of the Bab")
    (holiday-fixed 10 20 "Birth of the Bab")
    (holiday-fixed 11 12 "Birth of Baha'u'llah"))
  "Baha'i holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'bahai-holidays 'risky-local-variable t)

;;;###autoload
(defcustom solar-holidays
  '((solar-equinoxes-solstices)
    (holiday-sexp calendar-daylight-savings-starts
                  (format "Daylight Saving Time Begins %s"
                          (solar-time-string
                           (/ calendar-daylight-savings-starts-time (float 60))
                           calendar-standard-time-zone-name)))
    (holiday-sexp calendar-daylight-savings-ends
                  (format "Daylight Saving Time Ends %s"
                          (solar-time-string
                           (/ calendar-daylight-savings-ends-time (float 60))
                           calendar-daylight-time-zone-name))))
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

(defconst cal-hebrew-yahrzeit-buffer "*Yahrzeits*"
  "Name of the buffer used by `list-yahrzeit-dates'.")

(defmacro increment-calendar-month (mon yr n &optional nmonths)
  "Increment the variables MON and YR by N months.
Forward if N is positive or backward if N is negative.
A negative YR is interpreted as BC; -1 being 1 BC, and so on.
Optional NMONTHS is the number of months per year (default 12)."
  ;; Can view this as a form of base-nmonths arithmetic, in which "a
  ;; year" = "ten", and we never bother to use hundreds.
  `(let ((nmonths (or ,nmonths 12))
         macro-y)
     (if (< ,yr 0) (setq ,yr (1+ ,yr))) ; -1 BC -> 0 AD, etc
     (setq macro-y (+ (* ,yr nmonths) ,mon -1 ,n)
           ,mon (1+ (mod macro-y nmonths))
           ,yr (/ macro-y nmonths))
     ;; Alternative:
;;;      (setq macro-y (+ (* ,yr nmonths) ,mon -1 ,n)
;;;            ,yr (/ macro-y nmonths)
;;;            ,mon (- macro-y (* ,yr nmonths)))
     (and (< macro-y 0) (> ,mon 1) (setq ,yr (1- ,yr)))
     (if (< ,yr 1) (setq ,yr (1- ,yr))))) ; 0 AD -> -1 BC, etc

(defvar displayed-month)
(defvar displayed-year)

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
inclusive.  The standard macro `dotimes' is preferable in most cases."
  (declare (debug (symbolp "from" form "to" form "do" body))
           (indent defun))
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

(make-obsolete 'calendar-for-loop "use `dotimes' or `while' instead." "23.1")

(defmacro calendar-sum (index initial condition expression)
  "For INDEX = INITIAL, +1, ... (as long as CONDITION holds), sum EXPRESSION."
  (declare (debug (symbolp form form form)))
  `(let ((,index ,initial)
         (sum 0))
    (while ,condition
      (setq sum (+ sum ,expression)
            ,index (1+ ,index)))
    sum))

;; FIXME bind q to bury-buffer?
(defmacro calendar-in-read-only-buffer (buffer &rest body)
  "Switch to BUFFER and executes the forms in BODY.
First creates or erases BUFFER as needed.  Leaves BUFFER read-only,
with disabled undo.  Leaves point at point-min, displays BUFFER."
  (declare (indent 1) (debug t))
  `(progn
     (set-buffer (get-buffer-create ,buffer))
     (setq buffer-read-only nil
           buffer-undo-list t)
     (erase-buffer)
     ,@body
     (goto-char (point-min))
     (set-buffer-modified-p nil)
     (setq buffer-read-only t)
     (display-buffer ,buffer)))

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
;;
;; The use of these seven macros eliminates the overhead of 92% of the function
;; calls; it's faster this way.

(defsubst extract-calendar-month (date)
  "Extract the month part of DATE which has the form (month day year)."
  (car date))

;; Note gives wrong answer for result of (calendar-read-date 'noday),
;; but that is only used by `calendar-other-month'.
(defsubst extract-calendar-day (date)
  "Extract the day part of DATE which has the form (month day year)."
  (cadr date))

(defsubst extract-calendar-year (date)
  "Extract the year part of DATE which has the form (month day year)."
  (nth 2 date))

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
;;  "Return t if YEAR is a Gregorian leap year."
;;  (or
;;   (and (zerop (% year 4))
;;        (not (zerop (% year 100))))
;;   (zerop (% year 400)))

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
    (when (> month 2)
      (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
      (if (calendar-leap-year-p year)
          (setq day-of-year (1+ day-of-year))))
    day-of-year))

(defsubst calendar-absolute-from-gregorian (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary.
DATE is a list of the form (month day year).  A negative year is
interpreted as BC; -1 being 1 BC, and so on.  Dates before 12/31/1 BC
return negative results."
  (let ((year (extract-calendar-year date))
        offset-years)
    (cond ((zerop year)
           (error "There was no year zero"))
          ((> year 0)
           (setq offset-years (1- year))
           (+ (calendar-day-number date) ; days this year
              (* 365 offset-years)       ; + days in prior years
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
  "Display a three-month Gregorian calendar.
The three months appear side by side, with the current month in
the middle surrounded by the previous and next months.  The
cursor is put on today's date.  If optional prefix argument ARG
is non-nil, prompts for the central month and year.

Once in the calendar window, future or past months can be moved
into view.  Arbitrary months can be displayed, or the calendar
can be scrolled forward or backward.  The cursor can be moved
forward or backward by one day, one week, one month, or one year.
All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the
digit keys and the minus sign are automatically prefixes.  Use
\\[describe-mode] for details of the key bindings in the calendar
window.

Displays the calendar in a separate window, or optionally in a
separate frame, depending on the value of `calendar-setup'.

If `view-diary-entries-initially' is non-nil, also displays the
diary entries for the current date (or however many days
`number-of-diary-entries' specifies).  This variable can be
overridden by `calendar-setup'.  As well as being displayed,
diary entries can also be marked on the calendar (see
`mark-diary-entries-in-calendar').

Runs the following hooks:

`calendar-load-hook' - after loading calendar.el
`today-visible-calendar-hook', `today-invisible-calendar-hook' - after
   generating a calendar, if today's date is visible or not, respectively
`initial-calendar-window-hook' - after first creating a calendar

This function is suitable for execution in a .emacs file."
  (interactive "P")
  ;; Avoid loading cal-x unless it will be used.
  (if (and (memq calendar-setup '(one-frame two-frames calendar-only))
           (display-multi-frame-p))
      (calendar-frame-setup calendar-setup arg)
    (calendar-basic-setup arg)))

(defun calendar-basic-setup (&optional arg nodisplay)
  "Create a three-month calendar.
If optional prefix argument ARG is non-nil, prompts for the month
and year, else uses the current date.  If NODISPLAY is non-nil, don't
display the generated calendar."
  (interactive "P")
  (set-buffer (get-buffer-create calendar-buffer))
  (calendar-mode)
  (let* ((pop-up-windows t)
         (split-height-threshold 1000)
         (date (if arg (calendar-read-date t)
                 (calendar-current-date)))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date)))
    (increment-calendar-month month year (- calendar-offset))
    ;; Display the buffer before calling generate-calendar-window so that it
    ;; can get a chance to adjust the window sizes to the frame size.
    (or nodisplay (pop-to-buffer calendar-buffer))
    (generate-calendar-window month year)
    (if (and view-diary-entries-initially (calendar-date-is-visible-p date))
        (diary-view-entries)))
  (if view-calendar-holidays-initially
      (let* ((diary-buffer (get-file-buffer diary-file))
             (diary-window (if diary-buffer (get-buffer-window diary-buffer)))
             (split-height-threshold (if diary-window 2 1000)))
        ;; FIXME display buffer?
        (calendar-list-holidays)))
  (run-hooks 'initial-calendar-window-hook))

(defun generate-calendar-window (&optional mon yr)
  "Generate the calendar window for the current date.
Optional integers MON and YR are used instead of today's date."
  (let* ((inhibit-read-only t)
         (today (calendar-current-date))
         (month (extract-calendar-month today))
         (day (extract-calendar-day today))
         (year (extract-calendar-year today))
         (today-visible (or (not mon)
                            (<= (abs (calendar-interval mon yr month year)) 1)))
         (day-in-week (calendar-day-of-week today))
         (in-calendar-window (eq (window-buffer (selected-window))
                                 (get-buffer calendar-buffer))))
    (generate-calendar (or mon month) (or yr year))
    (update-calendar-mode-line)
    (calendar-cursor-to-visible-date
     (if today-visible today (list displayed-month 1 displayed-year)))
    (set-buffer-modified-p nil)
    ;; Don't do any window-related stuff if we weren't called from a
    ;; window displaying the calendar.
    (when in-calendar-window
      (if (or (one-window-p t) (not (window-full-width-p)))
          ;; Don't mess with the window size, but ensure that the first
          ;; line is fully visible.
          (set-window-vscroll nil 0)
        ;; Adjust the window to exactly fit the displayed calendar.
        (fit-window-to-buffer nil nil calendar-minimum-window-height))
      (sit-for 0))
    (and (bound-and-true-p font-lock-mode)
         (font-lock-fontify-buffer))
    (and mark-holidays-in-calendar
;;;         (calendar-date-is-valid-p today) ; useful for BC dates
         (calendar-mark-holidays)
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
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
         (last (calendar-last-day-of-month month year))
         string)
   (goto-char (point-min))
   (calendar-insert-indented
    (calendar-string-spread
     (list (format "%s %d" (calendar-month-name month) year)) ?  20)
    indent t)
   (calendar-insert-indented "" indent) ; go to proper spot
   ;; Use the first two characters of each day to head the columns.
   (dotimes (i 7)
     (insert
      (progn
        (setq string
              (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t))
        (if enable-multibyte-characters
            (truncate-string-to-width string 2)
          (substring string 0 2)))
      " "))
   (calendar-insert-indented "" 0 t)    ; force onto following line
   (calendar-insert-indented "" indent) ; go to proper spot
   ;; Add blank days before the first of the month.
   (dotimes (idummy blank-days) (insert "   "))
   ;; Put in the days of the month.
   (dotimes (i last)
     (insert (format "%2d " (1+ i)))
     (add-text-properties
      (- (point) 3) (1- (point))
      '(mouse-face highlight
                   help-echo "mouse-2: menu of operations for this date"))
     (and (zerop (mod (+ i 1 blank-days) 7))
          (/= i (1- last))
          (calendar-insert-indented "" 0 t) ; force onto following line
          (calendar-insert-indented "" indent))))) ; go to proper spot

(defun calendar-insert-indented (string indent &optional newline)
  "Insert STRING at column INDENT.
If the optional parameter NEWLINE is non-nil, leave point at start of next
line, inserting a newline if there was no next line; otherwise, leave point
after the inserted text.  Returns t."
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
    (define-key map "x"   'calendar-mark-holidays)
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

    map)
  "Keymap for `calendar-mode'.")

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

;; After calendar-mode-map.
(defcustom calendar-mode-line-format
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
to `update-calendar-mode-line', the mode line shows these values for the date
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
       \"\"))"
  :type 'sexp
  :group 'calendar)

(defun mouse-calendar-other-month (event)
  "Display a three-month calendar centered around a specified month and year.
EVENT is the last mouse event."
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

(defun calendar-mode ()
  "A major mode for the calendar window.

For a complete description, type \
\\<calendar-mode-map>\\[calendar-goto-info-node] from within the calendar.

\\<calendar-mode-map>\\{calendar-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'calendar-mode
        mode-name "Calendar"
        buffer-read-only t
        buffer-undo-list t
        indent-tabs-mode nil)
  (use-local-map calendar-mode-map)
  (update-calendar-mode-line)
  (make-local-variable 'calendar-mark-ring)
  (make-local-variable 'displayed-month) ; month in middle of window
  (make-local-variable 'displayed-year)  ; year in middle of window
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
;; Addison-Wesley, Reading, MA, 1989.
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
                      string)
            i (1+ i)))
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
  "List of all calendar-related buffers (as buffers, not strings)."
  (let (buffs)
    (dolist (b (list cal-hebrew-yahrzeit-buffer lunar-phases-buffer
                     holiday-buffer fancy-diary-buffer
                     (get-file-buffer diary-file)
                     calendar-buffer other-calendars-buffer))
      (and b (setq b (get-buffer b))
           (push b buffs)))
    buffs))

(defun exit-calendar ()
  "Get out of the calendar window and hide it and related buffers."
  (interactive)
  (let ((diary-buffer (get-file-buffer diary-file)))
    (if (or (not diary-buffer)
            (not (buffer-modified-p diary-buffer))
            (yes-or-no-p
             "Diary modified; do you really want to exit the calendar? "))
        ;; Need to do this multiple times because one time can replace some
        ;; calendar-related buffers with other calendar-related buffers.
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
ERROR is non-nil, otherwise just returns nil."
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
      (if error (error "Not on a date!")))))

(add-to-list 'debug-ignored-errors "Not on a date!")

;; The following version of calendar-gregorian-from-absolute is preferred for
;; reasons of clarity, BUT it's much slower than the version that follows it.

;;(defun calendar-gregorian-from-absolute (date)
;;  "Compute the list (month day year) corresponding to the absolute DATE.
;;The absolute date is the number of days elapsed since the (imaginary)
;;Gregorian date Sunday, December 31, 1 BC."
;;  (let* ((approx (/ date 366)) ; approximation from below
;;         (year                ; search forward from the approximation
;;          (+ approx
;;             (calendar-sum y approx
;;                 (>= date (calendar-absolute-from-gregorian (list 1 1 (1+ y))))
;;                  1)))
;;         (month                         ; search forward from January
;;          (1+ (calendar-sum m 1
;;                   (> date
;;                      (calendar-absolute-from-gregorian
;;                       (list m (calendar-last-day-of-month m year) year)))
;;                   1)))
;;         (day                      ; calculate the day by subtraction
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
         (year (+ (* 400 n400) (* 100 n100) (* n4 4) n1))
         (month 1)
         mdays)
    (if (or (= n100 4) (= n1 4))
        (list 12 31 year)
      (setq year (1+ year))
      (while (< (setq mdays (calendar-last-day-of-month month year)) day)
        (setq day (- day mdays)
              month (1+ month)))
      (list month day year))))

(defun calendar-other-month (month year)
  "Display a three-month calendar centered around MONTH and YEAR."
  (interactive (calendar-read-date 'noday))
  (unless (and (= month displayed-month)
               (= year displayed-year))
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
With argument ARG, jump to mark, pop it, and put point at end of ring."
  (interactive "P")
  (let ((date (calendar-cursor-to-date t)))
    (if arg
        (if (null calendar-mark-ring)
            (error "No mark set in this buffer")
          (calendar-goto-date (car calendar-mark-ring))
          (setq calendar-mark-ring
                (cdr (nconc calendar-mark-ring (list date)))))
      (push date calendar-mark-ring)
      ;; Since the top of the mark ring is the marked date in the
      ;; calendar, the mark ring in the calendar is one longer than
      ;; in other buffers to get the same effect.
      (if (> (length calendar-mark-ring) (1+ mark-ring-max))
          (setcdr (nthcdr mark-ring-max calendar-mark-ring) nil))
      (message "Mark set"))))

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

;; FIXME does it have to start from Sunday?
(defcustom calendar-day-name-array
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "Array of capitalized strings giving, in order, the day names.
The first two characters of each string will be used to head the
day columns in the calendar.  See also the variable
`calendar-day-abbrev-array'."
  :group 'calendar
  :type '(vector (string :tag "Sunday")
                 (string :tag "Monday")
                 (string :tag "Tuesday")
                 (string :tag "Wednesday")
                 (string :tag "Thursday")
                 (string :tag "Friday")
                 (string :tag "Saturday")))

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

(defcustom calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"]
  "Array of capitalized strings giving, in order, the month names.
See also the variable `calendar-month-abbrev-array'."
  :group 'calendar
  :type '(vector (string :tag "January")
                 (string :tag "February")
                 (string :tag "March")
                 (string :tag "April")
                 (string :tag "May")
                 (string :tag "June")
                 (string :tag "July")
                 (string :tag "August")
                 (string :tag "September")
                 (string :tag "October")
                 (string :tag "November")
                 (string :tag "December")))

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

(defun calendar-read-date (&optional noday)
  "Prompt for Gregorian date.  Return a list (month day year).
If optional NODAY is t, does not ask for day, but just returns
\(month 1 year); if NODAY is any other non-nil value the value returned is
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
            (list month 1 year)
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
     ;; Saturdays and Sundays are highlighted differently.
     . font-lock-comment-face)
    ;; First two chars of each day are used in the calendar.
    (,(regexp-opt (mapcar (lambda (x) (substring x 0 2))
                          calendar-day-name-array))
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
  (setq mark-holidays-in-calendar nil
        mark-diary-entries-in-calendar nil)
  (with-current-buffer calendar-buffer
    (mapc 'delete-overlay (overlays-in (point-min) (point-max)))))

(defun calendar-date-is-visible-p (date)
  "Return non-nil if DATE is valid and is visible in the calendar window."
  (and (calendar-date-is-valid-p date)
       (< (abs (calendar-interval
                displayed-month displayed-year
                (extract-calendar-month date) (extract-calendar-year date)))
          2)))

(defun calendar-nongregorian-visible-p (month day toabs fromabs switch)
  "Return non-nil if MONTH, DAY is visible in the calendar window.
MONTH and DAY are in some non-Gregorian calendar system.  The
functions TOABS and FROMABS convert that system to and from
absolute, respectively.  SWITCH is a function that takes a single
argument (a local month number).  It applies when the local year
changes across the calendar window, and returns non-nil if the
specified month should be associated with the higher year.
Returns the corresponding Gregorian date."
  ;; We need to choose the local year associated with month and day
  ;; that might make them visible.
  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         ;; Absolute date of first/last dates in calendar window.
         (start-date (progn
                       (increment-calendar-month m1 y1 -1)
                       (calendar-absolute-from-gregorian (list m1 1 y1))))
         (end-date (progn
                     (increment-calendar-month m2 y2 1)
                     (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2))))
         ;; Local date of first/last date in calendar window.
         (local-start (funcall fromabs start-date))
         (local-end (funcall fromabs end-date))
         ;; Local year of first/last dates.
         ;; Can only differ if displayed-month = 12, 1, 2.
         (local-y1 (extract-calendar-year local-start))
         (local-y2 (extract-calendar-year local-end))
         ;; Choose which year might be visible in the window.
         ;; Obviously it only matters when y1 and y2 differ, ie
         ;; when the _local_ new year is visible.
         (year (if (funcall switch month) local-y2 local-y1))
         (date (calendar-gregorian-from-absolute
                (funcall toabs (list month day year)))))
    (if (calendar-date-is-visible-p date)
        date)))

(defun calendar-date-is-valid-p (date)
  "Return t if DATE is a valid date."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (and (<= 1 month) (<= month 12)
         ;; (calendar-read-date t) used to return a date with day = nil.
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

(defun calendar-make-temp-face (attrlist)
  "Return a temporary face based on the attributes in ATTRLIST.
ATTRLIST is a list with elements of the form :face face :foreground color."
  (let ((attrs attrlist)
        faceinfo face temp-face)
    ;; Separate :face from the other attributes.  Use the last :face
    ;; if there are more than one.  FIXME is merging meaningful?
    (while attrs
      (if (eq (car attrs) :face)
          (setq face (intern-soft (cadr attrs))
                attrs (cddr attrs))
        (push (car attrs) faceinfo)
        (setq attrs (cdr attrs))))
    (or (facep face) (setq face 'default))
    (if (not faceinfo)
        ;; No attributes to apply, so just use an existing-face.
        face
      ;; FIXME should we be using numbered temp-faces, re-using where poss?
      (setq temp-face
            (make-symbol
             (concat ":caltemp"
                     (mapconcat (lambda (sym)
                                  (cond
                                   ((symbolp sym) (symbol-name sym))
                                   ((numberp sym) (number-to-string sym))
                                   (t sym)))
                                attrlist ""))))
      (make-face temp-face)
      (copy-face face temp-face)
      ;; Apply the font aspects.
      (apply 'set-face-attribute temp-face nil (nreverse faceinfo))
      temp-face)))

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
                    (and font-lock-mode
                         (or
                          (and (listp mark) (> (length mark) 0) mark) ; attrs
                          (and (facep mark) mark))) ; face-name
                    diary-entry-marker))
          (cond
           ;; Face or an attr-list that contained a face.
           ((facep mark)
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face mark))
           ;; Single-character mark, goes after the date.
           ((and (stringp mark) (= (length mark) 1))
            (overlay-put
             (make-overlay (1+ (point)) (+ 2 (point))) 'display mark))
           (t                           ; attr list
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face
             (calendar-make-temp-face mark))))))))

(defun calendar-star-date ()
  "Replace the date under the cursor in the calendar window with asterisks.
You might want to add this function to `today-visible-calendar-hook'."
  (unless (catch 'found
            (dolist (ol (overlays-at (point)))
              (and (overlay-get ol 'calendar-star)
                   (throw 'found t))))
    (let ((ol (make-overlay (1- (point)) (point))))
      (overlay-put ol 'display "*")
      (overlay-put ol 'calendar-star t)
      ;; Use copy-sequence to avoid merging of identical 'display props.
      ;; Use two overlays so as not to mess up
      ;; calendar-cursor-to-nearest-date (and calendar-forward-day).
      (overlay-put (setq ol (make-overlay (point) (1+ (point))))
                   'display (copy-sequence "*"))
      (overlay-put ol 'calendar-star t))))

(defun calendar-mark-today ()
  "Mark the date under the cursor in the calendar window.
The date is marked with `calendar-today-marker'.  You might want to add
this function to `today-visible-calendar-hook'."
  (mark-visible-calendar-date (calendar-cursor-to-date) calendar-today-marker))

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
  (let* ((dayname (unless nodayname (calendar-day-name date abbreviate)))
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
Like `calendar-nth-named-absday', but returns a Gregorian date."
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

(defun calendar-other-dates (date)
  "Return a list of strings giving Gregorian DATE in other calendars.
DATE is (month day year).  Calendars that do not apply are omitted."
  (let (odate)
    (delq nil
          (list
           (calendar-day-of-year-string date)
           (format "ISO date: %s" (calendar-iso-date-string date))
           (format "Julian date: %s"
                   (calendar-julian-date-string date))
           (format "Astronomical (Julian) day number (at noon UTC): %s.0"
                   (calendar-astro-date-string date))
           (format "Fixed (RD) date: %s"
                   (calendar-absolute-from-gregorian date))
           (format "Hebrew date (before sunset): %s"
                   (calendar-hebrew-date-string date))
           (format "Persian date: %s"
                   (calendar-persian-date-string date))
           (unless (string-equal
                    (setq odate (calendar-islamic-date-string date))
                    "")
             (format "Islamic date (before sunset): %s" odate))
           (unless (string-equal
                    (setq odate (calendar-bahai-date-string date))
                    "")
             (format "Baha'i date: %s" odate))
           (format "Chinese date: %s"
                   (calendar-chinese-date-string date))
           (unless (string-equal
                    (setq odate (calendar-coptic-date-string date))
                    "")
             (format "Coptic date: %s" odate))
           (unless (string-equal
                    (setq odate (calendar-ethiopic-date-string date))
                    "")
             (format "Ethiopic date: %s" odate))
           (unless (string-equal
                    (setq odate (calendar-french-date-string date))
                    "")
             (format "French Revolutionary date: %s" odate))
           (format "Mayan date: %s"
                   (calendar-mayan-date-string date))))))

(defun calendar-print-other-dates ()
  "Show dates on other calendars for date under the cursor."
  (interactive)
  (let ((date (calendar-cursor-to-date t)))
    (calendar-in-read-only-buffer other-calendars-buffer
      (calendar-set-mode-line (format "%s (Gregorian)"
                                      (calendar-date-string date)))
      (insert (mapconcat 'identity (calendar-other-dates date) "\n")))))

(defun calendar-print-day-of-year ()
  "Show day number in year/days remaining in year for date under the cursor."
  (interactive)
  (message "%s" (calendar-day-of-year-string (calendar-cursor-to-date t))))

(defun calendar-set-mode-line (str)
  "Set mode line to STR, centered, surrounded by dashes."
  (let* ((edges (window-edges))
         ;; As per doc of window-width, total visible mode-line length.
         (width (- (nth 2 edges) (car edges))))
    (setq mode-line-format
          (if buffer-file-name
              `("-" mode-line-modified
                ,(calendar-string-spread (list str) ?- (- width 6))
                "---")
            (calendar-string-spread (list str) ?- width)))))

(defun calendar-version ()
  "Display the Calendar version."
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
