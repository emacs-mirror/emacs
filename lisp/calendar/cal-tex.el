;;; cal-tex.el --- calendar functions for printing calendars with LaTeX

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Steve Fisk <fisk@bowdoin.edu>
;;      Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: Calendar, LaTeX

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This collection of functions implements the creation of LaTeX calendars
;; based on the user's holiday choices and diary file.

;; TO DO
;;
;;     (*)  Add holidays and diary entries to daily calendar.
;;
;;     (*)  Add diary entries to weekly calendar functions.
;;
;;     (*)  Make calendar styles for A4 paper.
;;
;;     (*)  Make monthly styles Filofax paper.

;;; Code:

(require 'calendar)

(autoload 'list-diary-entries "diary-lib" nil t)
(autoload 'calendar-holiday-list "holidays" nil t)
(autoload 'calendar-iso-from-absolute "cal-iso" nil t)

;;;
;;; Customizable variables 
;;;

(defcustom cal-tex-which-days '(0 1 2 3 4 5 6)
  "*The days of the week that are displayed on the portrait monthly calendar.
Sunday is 0, Monday is 1, and so on.  The default is to print from Sunday to
Saturday.  For example, use

                    (setq cal-tex-which-days '(1 3 5))

to only print Monday, Wednesday, Friday."
  :type '(repeat integer)
  :group 'calendar-tex)

(defcustom cal-tex-holidays t
  "*If t (default), then the holidays are also printed.
If finding the holidays is too slow, set this to nil."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-diary nil
  "*If t, the diary entries are printed in the calendar."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-rules nil
  "*If t, pages will be ruled in some styles."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-daily-string
  '(let* ((year (extract-calendar-year date))
          (day  (calendar-day-number date))
          (days-remaining (- (calendar-day-number (list 12 31 year)) day)))
     (format "%d/%d" day  days-remaining))
  "*An expression in the variable `date' whose value is placed on date.
The string resulting from evaluating this expression is placed at the bottom
center of `date' on the monthly calendar, next to the date in the weekly
calendars, and in the top center of daily calendars.

Default is ordinal day number of the year and the number of days remaining.
As an example of what you do, setting this to

    '(progn
       (require 'cal-hebrew)
       (calendar-hebrew-date-string date))

will put the Hebrew date at the bottom of each day."
  :type 'sexp
  :group 'calendar-tex)

(defcustom cal-tex-buffer "calendar.tex"
  "*The name for the tex-ed calendar."
  :type 'string
  :group 'calendar-tex)

(defcustom cal-tex-24 nil
  "*If t, use a 24 hour clock in the daily calendar."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-daily-start 8
  "*The first hour of the daily calendar page."
  :type 'integer
  :group 'calendar-tex)

(defcustom cal-tex-daily-end 20
  "*The last hour of the daily calendar page."
  :type 'integer
  :group 'calendar-tex)

(defcustom cal-tex-hook nil
  "*List of functions called after any LaTeX calendar buffer is generated.
You can use this to do postprocessing on the buffer.  For example, to change
characters with diacritical marks to their LaTeX equivalents, use
     (add-hook 'cal-tex-hook
               '(lambda () (iso-iso2tex (point-min) (point-max))))"
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-year-hook nil
  "*List of functions called after a LaTeX year calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-month-hook nil
  "*List of functions called after a LaTeX month calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-week-hook nil
  "*List of functions called after a LaTeX week calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-daily-hook nil
  "*List of functions called after a LaTeX daily calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

;;;
;;; Definitions for LaTeX code
;;;

(defvar  cal-tex-day-prefix "\\caldate{%s}{%s}"
  "The initial LaTeX code for a day.  
The holidays, diary entries, bottom string, and the text follow.")
 
(defvar cal-tex-day-name-format "\\myday{%s}%%"
  "The format for LaTeX code for a day name.  The names are taken from
calendar-day-name-array.")

(defvar cal-tex-cal-one-month
"\\def\\calmonth#1#2%
{\\begin{center}%
\\Huge\\bf\\uppercase{#1} #2 \\\\[1cm]%
\\end{center}}% 
\\vspace*{-1.5cm}%
%
"
  "LaTeX code for the month header")

(defvar cal-tex-cal-multi-month
"\\def\\calmonth#1#2#3#4%
{\\begin{center}%
\\Huge\\bf #1 #2---#3 #4\\\\[1cm]%
\\end{center}}% 
\\vspace*{-1.5cm}%
%
"
  "LaTeX code for the month header")

(defvar cal-tex-myday
"\\renewcommand{\\myday}[1]%
{\\makebox[\\cellwidth]{\\hfill\\large\\bf#1\\hfill}}
%
"
  "LaTeX code for a day heading")

(defvar cal-tex-caldate
"\\fboxsep=0pt
\\long\\def\\caldate#1#2#3#4#5#6{%
    \\fbox{\\hbox to\\cellwidth{%
     \\vbox to\\cellheight{%
       \\hbox to\\cellwidth{%
          {\\hspace*{1mm}\\Large \\bf \\strut #2}\\hspace{.05\\cellwidth}%
          \\raisebox{\\holidaymult\\cellheight}%
                   {\\parbox[t]{.75\\cellwidth}{\\tiny \\raggedright #4}}}
       \\hbox to\\cellwidth{%
           \\hspace*{1mm}\\parbox{.95\\cellwidth}{\\tiny \\raggedright #3}}
       \\hspace*{1mm}%
       \\hbox to\\cellwidth{#6}%
       \\vfill%
       \\hbox to\\cellwidth{\\hfill \\tiny #5 \\hfill}%
       \\vskip 1.4pt}%
     \\hskip -0.4pt}}}
"
  "LaTeX code to insert one box with date info in calendar.
This definition is the heart of the calendar!")

(defun cal-tex-list-holidays (d1 d2)
  "Generate a list of all holidays from absolute date D1 to D2."
  (let* ((start (calendar-gregorian-from-absolute d1))
         (displayed-month (extract-calendar-month start))
         (displayed-year (extract-calendar-year start))
         (end (calendar-gregorian-from-absolute d2))
         (end-month (extract-calendar-month end))
         (end-year (extract-calendar-year end))
         (number-of-intervals
          (1+ (/ (calendar-interval displayed-month displayed-year
                                    end-month end-year)
                 3)))
         (holidays nil)
         (in-range))
    (increment-calendar-month displayed-month displayed-year 1)
    (calendar-for-loop i from 1 to number-of-intervals do
      (setq holidays (append holidays (calendar-holiday-list)))
      (increment-calendar-month displayed-month displayed-year 3))
    (while holidays
      (and (car (car holidays))
           (let ((a (calendar-absolute-from-gregorian (car (car holidays)))))
             (and (<= d1 a) (<= a d2)))
           (setq in-range (append (list (car holidays)) in-range)))
      (setq holidays (cdr holidays)))
    in-range))

(defun cal-tex-list-diary-entries (d1 d2)
  "Generate a list of all diary-entries from absolute date D1 to D2."
  (let ((diary-list-include-blanks nil)
        (diary-display-hook 'ignore))
    (list-diary-entries
     (calendar-gregorian-from-absolute d1)
     (1+ (- d2 d1)))))

(defun cal-tex-preamble (&optional args)
  "Insert the LaTeX preamble.
Preamble Includes initial definitions for various LaTeX commands.
Optional ARGS are included."
  (set-buffer (get-buffer-create cal-tex-buffer))
  (erase-buffer)
  (insert "\\documentclass")
  (if args
      (insert "[" args "]"))
  (insert "{article}\n"
          "\\hbadness 20000
\\hfuzz=1000pt
\\vbadness 20000
\\lineskip 0pt
\\marginparwidth 0pt
\\oddsidemargin  -2cm
\\evensidemargin -2cm
\\marginparsep   0pt
\\topmargin      0pt
\\textwidth      7.5in
\\textheight     9.5in
\\newlength{\\cellwidth}
\\newlength{\\cellheight}
\\newlength{\\boxwidth}
\\newlength{\\boxheight}
\\newlength{\\cellsize}
\\newcommand{\\myday}[1]{}
\\newcommand{\\caldate}[6]{}
\\newcommand{\\nocaldate}[6]{}
\\newcommand{\\calsmall}[6]{}
%
"))

;;;
;;;  Yearly calendars
;;;

(defun cal-tex-cursor-year (&optional arg)
  "Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument specifies number of years."
  (interactive "p")
  (cal-tex-year (extract-calendar-year (calendar-cursor-to-date t))
                (if arg arg 1)))

(defun cal-tex-cursor-year-landscape (&optional arg)
  "Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument specifies number of years."
  (interactive "p")
  (cal-tex-year (extract-calendar-year (calendar-cursor-to-date t))
                (if arg arg 1)
                t))

(defun cal-tex-year (year n &optional landscape)
  "Make a one page yearly calendar of YEAR; do this for N years.
There are four rows of three months each, unless optional LANDSCAPE is t,
in which case the calendar isprinted in landscape mode with three rows of
four months each."
  (cal-tex-insert-preamble 1 landscape "12pt")
  (if landscape
      (cal-tex-vspace "-.6cm")
    (cal-tex-vspace "-3.1cm"))
  (calendar-for-loop j from 1 to n do
     (insert "\\vfill%\n")
     (cal-tex-b-center)
     (cal-tex-Huge (number-to-string year))
     (cal-tex-e-center)
     (cal-tex-vspace "1cm")
     (cal-tex-b-center)
     (cal-tex-b-parbox "l" (if landscape "5.9in" "4.3in"))
     (insert "\n")
     (cal-tex-noindent)
     (cal-tex-nl)
     (calendar-for-loop i from 1 to 12 do
        (insert (cal-tex-mini-calendar i year "month" "1.1in" "1in"))
        (insert "\\month")
        (cal-tex-hspace "0.5in")
        (if (zerop (mod i (if landscape 4 3)))
            (cal-tex-nl "0.5in")))
     (cal-tex-e-parbox)
     (cal-tex-e-center)
     (insert "\\vfill%\n")
     (setq year (1+ year))
     (if (/= j n)
         (cal-tex-newpage)
       (cal-tex-end-document))
     (run-hooks 'cal-tex-year-hook))
  (run-hooks 'cal-tex-hook))

(defun cal-tex-cursor-filofax-year (&optional arg)
  "Make a Filofax one page yearly calendar of year indicated by cursor.
Optional parameter specifies number of years."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (year (extract-calendar-year (calendar-cursor-to-date t))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.675in")
    (cal-tex-cmd "\\evensidemargin 1.675in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\fboxsep 0.5mm")
    (cal-tex-cmd "\\pagestyle{empty}")
    (cal-tex-b-document)
    (cal-tex-cmd "\\vspace*{0.25in}")
    (calendar-for-loop j from 1 to n do
       (insert (format "\\hfil {\\Large \\bf %s} \\hfil\\\\\n" year))
       (cal-tex-b-center)
       (cal-tex-b-parbox "l" "\\textwidth")
       (insert "\n")
       (cal-tex-noindent)
       (cal-tex-nl)
       (let ((month-names; don't use default in case user changed it
              ["January" "February" "March" "April" "May" "June"
               "July" "August" "September" "October" "November" "December"]))
         (calendar-for-loop i from 1 to 12 do
            (insert (cal-tex-mini-calendar i year
                                           (aref month-names (1- i))
                                           "1in" ".9in" "tiny" "0.6mm"))))
       (insert
"\\noindent\\fbox{\\January}\\fbox{\\February}\\fbox{\\March}\\\\
\\noindent\\fbox{\\April}\\fbox{\\May}\\fbox{\\June}\\\\
\\noindent\\fbox{\\July}\\fbox{\\August}\\fbox{\\September}\\\\
\\noindent\\fbox{\\October}\\fbox{\\November}\\fbox{\\December}
")
       (cal-tex-e-parbox)
       (cal-tex-e-center)
       (setq year (1+ year))
       (if (= j n)
           (cal-tex-end-document)
         (cal-tex-newpage)
         (cal-tex-cmd "\\vspace*{0.25in}"))
       (run-hooks 'cal-tex-year-hook))
    (run-hooks 'cal-tex-hook)))

;;;
;;;  Monthly calendars
;;;

(defun cal-tex-cursor-month-landscape (&optional arg)
  "Make a buffer with LaTeX commands for the month cursor is on.
Optional prefix argument specifies number of months to be produced.
The output is in landscape format, one month to a page."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-cursor-to-date t))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (end-month month)
         (end-year year)
         (cal-tex-which-days '(0 1 2 3 4 5 6)))
    (increment-calendar-month end-month end-year (1- n))
    (let ((diary-list (if cal-tex-diary
                          (cal-tex-list-diary-entries
                           (calendar-absolute-from-gregorian
                            (list month 1 year))
                           (calendar-absolute-from-gregorian
                            (list end-month
                                  (calendar-last-day-of-month
                                   end-month end-year)
                                  end-year)))))
          (holidays (if cal-tex-holidays
                        (cal-tex-list-holidays
                         (calendar-absolute-from-gregorian
                          (list month 1 year))
                         (calendar-absolute-from-gregorian
                          (list end-month
                                (calendar-last-day-of-month end-month end-year)
                                end-year)))))
          (other-month)
          (other-year)
          (small-months-at-start))
      (cal-tex-insert-preamble (cal-tex-number-weeks month year 1) t "12pt")
      (cal-tex-cmd cal-tex-cal-one-month)
      (calendar-for-loop i from 1 to n do
         (setq other-month month)
         (setq other-year year)
         (increment-calendar-month other-month other-year -1)
         (insert (cal-tex-mini-calendar other-month other-year "lastmonth"
                                        "\\cellwidth" "\\cellheight"))
         (increment-calendar-month other-month other-year 2)
         (insert (cal-tex-mini-calendar other-month other-year "nextmonth"
                                        "\\cellwidth" "\\cellheight"))
         (cal-tex-insert-month-header 1 month year month year)
         (cal-tex-insert-day-names)
         (cal-tex-nl ".2cm")
         (setq small-months-at-start
               (< 1 (mod (- (calendar-day-of-week (list month 1 year))
                            calendar-week-start-day)
                         7)))
         (if small-months-at-start
             (insert "\\lastmonth\\nextmonth\\hspace*{-2\\cellwidth}"))
         (cal-tex-insert-blank-days month year cal-tex-day-prefix)
         (cal-tex-insert-days month year diary-list holidays
                              cal-tex-day-prefix)
         (cal-tex-insert-blank-days-at-end month year cal-tex-day-prefix)
         (if (and (not small-months-at-start)
                  (< 1 (mod (- (1- calendar-week-start-day)
                               (calendar-day-of-week
                                (list month
                                      (calendar-last-day-of-month month year)
                                      year)))
                                 7)))
             (insert "\\vspace*{-\\cellwidth}\\hspace*{-2\\cellwidth}"
                     "\\lastmonth\\nextmonth%
"))
         (if (/= i n)
             (progn
               (run-hooks 'cal-tex-month-hook)
               (cal-tex-newpage)
               (increment-calendar-month month year 1)
               (cal-tex-vspace "-2cm")
               (cal-tex-insert-preamble
                (cal-tex-number-weeks month year 1) t "12pt" t))))
      (cal-tex-end-document)
      (run-hooks 'cal-tex-hook))))

(defun cal-tex-cursor-month (arg)
  "Make a buffer with LaTeX commands for the month cursor is on.
Optional prefix argument specifies number of months to be produced.
Calendar is condensed onto one page."
  (interactive "p")
  (let* ((date (calendar-cursor-to-date t))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (end-month month)
         (end-year year)
         (n (if arg arg 1)))
    (increment-calendar-month end-month end-year (1- n))
    (let ((diary-list (if cal-tex-diary
                          (cal-tex-list-diary-entries
                           (calendar-absolute-from-gregorian
                            (list month 1 year))
                           (calendar-absolute-from-gregorian
                            (list end-month
                                  (calendar-last-day-of-month
                                   end-month end-year)
                                  end-year)))))
          (holidays (if cal-tex-holidays
                        (cal-tex-list-holidays
                         (calendar-absolute-from-gregorian
                          (list month 1 year))
                         (calendar-absolute-from-gregorian
                          (list end-month
                                (calendar-last-day-of-month end-month end-year)
                                end-year)))))
          (other-month)
          (other-year))
      (cal-tex-insert-preamble (cal-tex-number-weeks month year n) nil"12pt")
      (if (> n 1)
          (cal-tex-cmd cal-tex-cal-multi-month)
        (cal-tex-cmd cal-tex-cal-one-month))
      (cal-tex-insert-month-header n month year end-month end-year)
      (cal-tex-insert-day-names)
      (cal-tex-nl ".2cm")
      (cal-tex-insert-blank-days month year cal-tex-day-prefix)
      (calendar-for-loop i from 1 to n do
         (setq other-month month)
         (setq other-year year)
         (cal-tex-insert-days month year diary-list holidays
                              cal-tex-day-prefix)
         (if (= (mod (calendar-absolute-from-gregorian
                          (list month
                                (calendar-last-day-of-month month year)
                                year))
                         7)
                6); last day of month was Saturday
           (progn
             (cal-tex-hfill)
             (cal-tex-nl)))
         (increment-calendar-month month year 1))
      (cal-tex-insert-blank-days-at-end end-month end-year cal-tex-day-prefix)
      (cal-tex-end-document)))
  (run-hooks 'cal-tex-hook))

(defun cal-tex-insert-days (month year diary-list holidays day-format) 
  "Insert LaTeX commands for a range of days in monthly calendars.
LaTeX commands are inserted for the days of the MONTH in YEAR.
Diary entries on DIARY-LIST are included. Holidays on HOLIDAYS are included.
Each day is formatted using format DAY-FORMAT."
  (let* ((blank-days;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
         (date)
         (last (calendar-last-day-of-month month year)))
    (calendar-for-loop i from 1 to last do
       (setq date (list month i year))
       (if (memq (calendar-day-of-week date) cal-tex-which-days)
           (progn 
             (insert (format day-format (cal-tex-month-name month) i))
             (cal-tex-arg (cal-tex-latexify-list diary-list date))
             (cal-tex-arg (cal-tex-latexify-list holidays date))
             (cal-tex-arg (eval cal-tex-daily-string))
             (cal-tex-arg)
             (cal-tex-comment)))
       (if (and (zerop (mod (+ i blank-days) 7))
                (/= i last))
           (progn
             (cal-tex-hfill)
             (cal-tex-nl))))))

(defun cal-tex-insert-day-names ()
  "Insert the names of the days at top of a monthly calendar."
  (calendar-for-loop i from 0 to 6 do
     (if (memq i cal-tex-which-days)
         (insert (format cal-tex-day-name-format 
                         (cal-tex-LaTeXify-string
                          (aref calendar-day-name-array 
                                (mod (+ calendar-week-start-day i) 7))))))
     (cal-tex-comment)))

(defun cal-tex-insert-month-header (n month year end-month end-year)
  "Create a title for a calendar.
A title is inserted for a calendar with N months starting with 
MONTH YEAR and ending with END-MONTH END-YEAR."
  (let ((month-name (cal-tex-month-name  month))
         (end-month-name (cal-tex-month-name  end-month)))
    (if (= 1 n)
        (insert (format "\\calmonth{%s}{%s}\n\\vspace*{-0.5cm}"
                month-name year) )       
        (insert (format "\\calmonth{%s}{%s}{%s}{%s}\n\\vspace*{-0.5cm}"
                month-name year end-month-name end-year))))
  (cal-tex-comment))

(defun cal-tex-insert-blank-days (month year day-format)
  "Insert code for initial days not in calendar.
Insert LaTeX code for the blank days at the beginning of the MONTH in
YEAR.  The entry is formatted using DAY-FORMAT.  If the entire week is
blank, no days are inserted."
  (if (cal-tex-first-blank-p month year)
      (let* ((blank-days;; at start of month
              (mod
               (- (calendar-day-of-week (list month 1 year))
                  calendar-week-start-day)
               7)))
        (calendar-for-loop i from 0 to (1- blank-days) do 
            (if (memq i cal-tex-which-days)
                (insert (format day-format " " " ") "{}{}{}{}%\n"))))))

(defun cal-tex-insert-blank-days-at-end (month year day-format)
  "Insert code for final days not in calendar.
Insert LaTeX code for the blank days at the end of the MONTH in YEAR.
The entry is formatted using DAY-FORMAT."
  (if (cal-tex-last-blank-p month year)
      (let* ((last-day (calendar-last-day-of-month month year))
             (blank-days;; at end of month
              (mod
               (- (calendar-day-of-week (list month last-day year))
                  calendar-week-start-day)
               7)))
        (calendar-for-loop i from (1+ blank-days) to 6 do 
           (if (memq i cal-tex-which-days)
               (insert (format day-format "" "") "{}{}{}{}%\n"))))))

(defun cal-tex-first-blank-p (month year)
  "Determine if any days of the first week will be printed.
Return t if there will there be any days of the first week printed
in the calendar starting in MONTH YEAR."
  (let ((any-days nil)
        (the-saturday))                 ;the day of week of 1st Saturday
    (calendar-for-loop i from 1 to 7 do
       (if (= 6 (calendar-day-of-week (list month i year)))
           (setq the-saturday i)))
    (calendar-for-loop i from 1 to the-saturday do
       (if (memq (calendar-day-of-week (list month i year))
                 cal-tex-which-days)
           (setq any-days t)))
    any-days))

(defun cal-tex-last-blank-p (month year)
  "Determine if any days of the last week will be printed.
Return t if there will there be any days of the last week printed
in the calendar starting in MONTH YEAR."
  (let ((any-days nil)
        (last-day (calendar-last-day-of-month month year))
        (the-sunday))                   ;the day of week of last Sunday
    (calendar-for-loop i from (- last-day 6) to last-day do
       (if (= 0 (calendar-day-of-week (list month i year)))
           (setq the-sunday i)))
    (calendar-for-loop i from the-sunday to last-day do
       (if (memq (calendar-day-of-week (list month i year))
                 cal-tex-which-days)
           (setq any-days t)))
    any-days))

(defun cal-tex-number-weeks (month year n)
  "Determine the number of weeks in a range of dates.
Compute the number of  weeks in the calendar starting with MONTH and YEAR, 
and lasting N months, including only the days in WHICH-DAYS. As it stands, 
this is only an upper bound."
  (let ((d (list month 1 year)))
    (increment-calendar-month month year (1- n))
    (/ (- (calendar-dayname-on-or-before
           calendar-week-start-day
           (+ 7 (calendar-absolute-from-gregorian
                   (list month (calendar-last-day-of-month month year) year))))
          (calendar-dayname-on-or-before
           calendar-week-start-day
           (calendar-absolute-from-gregorian d)))
       7)))

;;;
;;; Weekly calendars
;;;

(defun cal-tex-cursor-week (&optional arg)
  "Make a buffer with LaTeX commands for a two-page one-week calendar.
It applies to the week that point is in.
Optional prefix argument specifies number of weeks.
Holidays are included if `cal-tex-holidays' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 calendar-week-start-day
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays
                        (calendar-absolute-from-gregorian date)
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date))))))
    (cal-tex-preamble "11pt")
    (cal-tex-cmd "\\textwidth   6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (insert cal-tex-LaTeX-hourbox)
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (calendar-for-loop i from 1 to n do
       (cal-tex-vspace "-1.5in")
       (cal-tex-b-center)
       (cal-tex-Huge-bf (format "\\uppercase{%s}"
                                (cal-tex-month-name month)))
       (cal-tex-hspace "2em")
       (cal-tex-Huge-bf (number-to-string year))
       (cal-tex-nl ".5cm")
       (cal-tex-e-center)
       (cal-tex-hspace "-.2in")
       (cal-tex-b-parbox "l" "7in")
       (calendar-for-loop j from 1 to 7 do 
          (cal-tex-week-hours date holidays "3.1")
          (setq date (cal-tex-incr-date date)))
       (cal-tex-e-parbox)
       (setq month (extract-calendar-month date))
       (setq year (extract-calendar-year date))
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-cursor-week2 (&optional arg)
  "Make a buffer with LaTeX commands for a two-page one-week calendar.
It applies to the week that point is in.
Optional prefix argument specifies number of weeks.
Holidays are included if `cal-tex-holidays' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 calendar-week-start-day
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (d date)
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays
                        (calendar-absolute-from-gregorian date)
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date))))))
    (cal-tex-preamble "12pt")
    (cal-tex-cmd "\\textwidth   6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (insert cal-tex-LaTeX-hourbox)
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (calendar-for-loop i from 1 to n do
       (cal-tex-vspace "-1.5in")
       (cal-tex-b-center)
       (cal-tex-Huge-bf (format "\\uppercase{%s}"
                                (cal-tex-month-name month)))
       (cal-tex-hspace "2em")
       (cal-tex-Huge-bf (number-to-string year))
       (cal-tex-nl ".5cm")
       (cal-tex-e-center)
       (cal-tex-hspace "-.2in")
       (cal-tex-b-parbox "l" "\\textwidth")
       (calendar-for-loop j from 1 to 3 do 
          (cal-tex-week-hours date holidays "5")
          (setq date (cal-tex-incr-date date)))
       (cal-tex-e-parbox)
       (cal-tex-nl)
       (insert (cal-tex-mini-calendar
                (extract-calendar-month (cal-tex-previous-month date))
                (extract-calendar-year (cal-tex-previous-month date))
                "lastmonth" "1.1in" "1in"))
       (insert (cal-tex-mini-calendar
                (extract-calendar-month date)
                (extract-calendar-year date)
                "thismonth" "1.1in" "1in"))
       (insert (cal-tex-mini-calendar
                (extract-calendar-month (cal-tex-next-month date))
                (extract-calendar-year (cal-tex-next-month date))
                "nextmonth" "1.1in" "1in"))
       (insert "\\hbox to \\textwidth{")
       (cal-tex-hfill)
       (insert "\\lastmonth")
       (cal-tex-hfill)
       (insert "\\thismonth")
       (cal-tex-hfill)
       (insert "\\nextmonth")
       (cal-tex-hfill)
       (insert "}")
       (cal-tex-nl)
       (cal-tex-b-parbox "l" "\\textwidth")
       (calendar-for-loop j from 4 to 7 do 
                          (cal-tex-week-hours date holidays "5")
                          (setq date (cal-tex-incr-date date)))
       (cal-tex-e-parbox)
       (setq month (extract-calendar-month date))
       (setq year (extract-calendar-year date))
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-cursor-week-iso (&optional arg)
  "Make a buffer with LaTeX commands for a one page ISO-style weekly calendar.
Optional prefix argument specifies number of weeks.
Diary entries are included if `cal-tex-diary' is t.
Holidays are included if `cal-tex-holidays' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 1
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (day (extract-calendar-day date))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays
                        (calendar-absolute-from-gregorian date)
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date)))))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          (calendar-absolute-from-gregorian
                           (list month 1 year))
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date))))))
    (cal-tex-preamble "11pt")
    (cal-tex-cmd "\\textwidth 6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (calendar-for-loop i from 1 to n do
       (cal-tex-vspace "-1.5in")
       (cal-tex-b-center)
       (cal-tex-Huge-bf
        (let* ((d (calendar-iso-from-absolute
                   (calendar-absolute-from-gregorian date))))
          (format "Week %d of %d"
                  (extract-calendar-month d)
                  (extract-calendar-year d))))
       (cal-tex-nl ".5cm")
       (cal-tex-e-center)
       (cal-tex-b-parbox "l" "\\textwidth")
       (calendar-for-loop j from 1 to 7 do 
          (cal-tex-b-parbox "t" "\\textwidth")
          (cal-tex-b-parbox "t" "\\textwidth")
          (cal-tex-rule "0pt" "\\textwidth" ".2mm")
          (cal-tex-nl)
          (cal-tex-b-parbox "t" "\\textwidth")
          (cal-tex-large-bf (cal-tex-LaTeXify-string (calendar-day-name date)))
          (insert ", ")
          (cal-tex-large-bf (cal-tex-month-name month))
          (insert " ")
          (cal-tex-large-bf (number-to-string day))
          (if (not (string= "" (cal-tex-latexify-list holidays date)))
              (progn
                (insert ": ")
                (cal-tex-large-bf (cal-tex-latexify-list holidays date "; "))))
          (cal-tex-hfill)
          (insert " " (eval cal-tex-daily-string))
          (cal-tex-e-parbox)
          (cal-tex-nl)
          (cal-tex-noindent)
          (cal-tex-b-parbox "t" "\\textwidth")
          (if (not (string= "" (cal-tex-latexify-list diary-list date)))
              (progn
                (insert "\\vbox to 0pt{")
                (cal-tex-large-bf
                 (cal-tex-latexify-list diary-list date))
                (insert "}")))
          (cal-tex-e-parbox)
          (cal-tex-nl)
          (setq date (cal-tex-incr-date date))
          (setq month (extract-calendar-month date))
          (setq day (extract-calendar-day date))
          (cal-tex-e-parbox)
          (cal-tex-e-parbox "2cm")
          (cal-tex-nl)
          (setq month (extract-calendar-month date))
          (setq year (extract-calendar-year date)))
       (cal-tex-e-parbox)
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defvar cal-tex-LaTeX-hourbox 
  "\\newcommand{\\hourbox}[2]%
{\\makebox[2em]{\\rule{0cm}{#2ex}#1}\\rule{3in}{.15mm}}\n"
  "One hour and a line on the right.")

(defun cal-tex-week-hours (date holidays height)
  "Insert hourly entries for DATE with HOLIDAYS, with line height HEIGHT." 
  (let ((month (extract-calendar-month date))
        (day   (extract-calendar-day date))
        (year  (extract-calendar-year date))
        (afternoon))
  (cal-tex-comment "begin cal-tex-week-hours")
  (cal-tex-cmd  "\\ \\\\[-.2cm]")
  (cal-tex-cmd "\\noindent")
  (cal-tex-b-parbox "l" "6.8in")
  (cal-tex-large-bf (cal-tex-LaTeXify-string (calendar-day-name date)))
  (insert ", ")
  (cal-tex-large-bf (cal-tex-month-name month))
  (insert " ")
  (cal-tex-large-bf (number-to-string day))
  (if (not (string= "" (cal-tex-latexify-list holidays date)))
      (progn
        (insert ": ")
        (cal-tex-large-bf (cal-tex-latexify-list holidays date "; "))))
  (cal-tex-hfill)
  (insert " " (eval cal-tex-daily-string))
  (cal-tex-e-parbox)
  (cal-tex-nl "-.3cm")
  (cal-tex-rule "0pt" "6.8in" ".2mm")
  (cal-tex-nl "-.1cm")
  (calendar-for-loop i from 8 to 12 do
     (if cal-tex-24
         (setq afternoon (+ i 5))
       (setq afternoon (- i 7)))
     (cal-tex-cmd "\\hourbox"  (number-to-string i))
     (cal-tex-arg height)
     (cal-tex-hspace ".4cm")
     (cal-tex-cmd "\\hourbox"  (number-to-string afternoon))
     (cal-tex-arg height)
     (cal-tex-nl))))

(defun cal-tex-cursor-week-monday (&optional arg)
  "Make a buffer with LaTeX commands for a two-page one-week calendar.
It applies to the week that point is in, and starts on Monday.
Optional prefix argument specifies number of weeks.
Holidays are included if `cal-tex-holidays' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 0
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t))))))
    (cal-tex-preamble "11pt")
    (cal-tex-cmd "\\textwidth   6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (cal-tex-b-document)
    (calendar-for-loop i from 1 to n do
       (cal-tex-vspace "-1cm")
       (insert "\\noindent ")
       (cal-tex-weekly4-box (cal-tex-incr-date date) nil)
       (cal-tex-weekly4-box (cal-tex-incr-date date 4) nil)
       (cal-tex-nl ".2cm")
       (cal-tex-weekly4-box (cal-tex-incr-date date 2) nil)
       (cal-tex-weekly4-box (cal-tex-incr-date date 5) nil)
       (cal-tex-nl ".2cm")
       (cal-tex-weekly4-box (cal-tex-incr-date date 3) nil)
       (cal-tex-weekly4-box (cal-tex-incr-date date 6) t)
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
	     (setq date (cal-tex-incr-date date 7))
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-weekly4-box (date weekend)
  "Make one box for DATE, different if WEEKEND."
  (let* (
	(day (extract-calendar-day date))
	(month (extract-calendar-month date))
	(year (extract-calendar-year date))
	(dayname (cal-tex-LaTeXify-string (calendar-day-name date)))
	(date1 (cal-tex-incr-date date))
	(day1 (extract-calendar-day date1))
	(month1 (extract-calendar-month date1))
	(year1 (extract-calendar-year date1))
	(dayname1 (cal-tex-LaTeXify-string (calendar-day-name date1)))
	)
    (cal-tex-b-framebox "8cm" "l")
    (cal-tex-b-parbox "b" "7.5cm")
    (insert (format "{\\Large\\bf %s,} %s/%s/%s\\\\\n" dayname month day year))
    (cal-tex-rule "0pt" "7.5cm" ".5mm")
    (cal-tex-nl)
    (if (not weekend)
	(progn
	  (calendar-for-loop i from 8 to 12 do
	     (insert (format "{\\large\\sf %d}\\\\\n" i)))
	  (calendar-for-loop i from 1 to 5 do
	     (insert (format "{\\large\\sf %d}\\\\\n" 
			     (if cal-tex-24 (+ i 12) i))))))
    (cal-tex-nl ".5cm")
    (if weekend
	(progn
	  (cal-tex-vspace "1cm")
	  (insert "\\ \\vfill")
	  (insert (format "{\\Large\\bf %s,} %s/%s/%s\\\\\n" 
			  dayname1 month1 day1 year1))
	  (cal-tex-rule "0pt" "7.5cm" ".5mm")
	  (cal-tex-nl "1.5cm")
	  (cal-tex-vspace "1cm")))
     (cal-tex-e-parbox)
     (cal-tex-e-framebox)
     (cal-tex-hspace "1cm")))

(defun cal-tex-cursor-filofax-2week (&optional arg)
  "Two-weeks-at-a-glance Filofax style calendar for week indicated by cursor.
Optional prefix argument specifies number of weeks.
Diary entries are included if `cal-tex-diary' is t.
Holidays are included if `cal-tex-holidays' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 calendar-week-start-day
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (day (extract-calendar-day date))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays
                        (calendar-absolute-from-gregorian date)
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date)))))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          (calendar-absolute-from-gregorian
                           (list month 1 year))
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date))))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.75in")
    (cal-tex-cmd "\\evensidemargin 1.5in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\headsep 0.125in")
    (cal-tex-cmd "\\footskip .125in")
    (insert "\\def\\righthead#1{\\hfill {\\normalsize \\bf #1}\\\\[-6pt]}
\\long\\def\\rightday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 0.7in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\small #5 \\hfill #1 {\\normalsize \\bf #2}}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
\\def\\lefthead#1{\\noindent {\\normalsize \\bf #1}\\hfill\\\\[-6pt]}
\\long\\def\\leftday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 0.7in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\noindent {\\normalsize \\bf #2} \\small #1 \\hfill #5}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (calendar-for-loop i from 1 to n do
       (if (= (mod i 2) 1)
           (insert "\\righthead")
         (insert "\\lefthead"))
       (cal-tex-arg
        (let ((d (cal-tex-incr-date date 6)))
          (if (= (extract-calendar-month date)
                 (extract-calendar-month d))
              (format "%s %s"
                      (cal-tex-month-name
                       (extract-calendar-month date))
                      (extract-calendar-year date))
            (if (=  (extract-calendar-year date)
                    (extract-calendar-year d))
                (format "%s---%s %s"
                        (cal-tex-month-name
                         (extract-calendar-month date))
                        (cal-tex-month-name
                         (extract-calendar-month d))
                        (extract-calendar-year date))
              (format "%s %s---%s %s"
                      (cal-tex-month-name
                       (extract-calendar-month date))
                      (extract-calendar-year date)
                      (cal-tex-month-name (extract-calendar-month d))
                      (extract-calendar-year d))))))
       (insert "%\n")
       (calendar-for-loop j from 1 to 7 do 
          (if (= (mod i 2) 1)
              (insert "\\rightday")
            (insert "\\leftday"))
          (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
          (cal-tex-arg (int-to-string (extract-calendar-day date)))
          (cal-tex-arg (cal-tex-latexify-list diary-list date))
          (cal-tex-arg (cal-tex-latexify-list holidays date))
          (cal-tex-arg (eval cal-tex-daily-string))
          (insert "%\n")
          (setq date (cal-tex-incr-date date)))
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-cursor-filofax-week (&optional arg)
  "One-week-at-a-glance Filofax style calendar for week indicated by cursor.
Optional prefix argument specifies number of weeks.
Weeks start on Monday. 
Diary entries are included if `cal-tex-diary' is t.
Holidays are included if `cal-tex-holidays' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 1
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (day (extract-calendar-day date))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays
                        (calendar-absolute-from-gregorian date)
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date)))))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          (calendar-absolute-from-gregorian
                           (list month 1 year))
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date))))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.75in")
    (cal-tex-cmd "\\evensidemargin 1.5in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\headsep 0.125in")
    (cal-tex-cmd "\\footskip .125in")
    (insert "\\def\\righthead#1{\\hfill {\\normalsize \\bf #1}\\\\[-6pt]}
\\long\\def\\rightday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 1.85in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\small #5 \\hfill #1 {\\normalsize \\bf #2}}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
\\long\\def\\weekend#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to .8in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\small #5 \\hfill #1 {\\normalsize \\bf #2}}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
\\def\\lefthead#1{\\noindent {\\normalsize \\bf #1}\\hfill\\\\[-6pt]}
\\long\\def\\leftday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 1.85in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\noindent {\\normalsize \\bf #2} \\small #1 \\hfill #5}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}\\ ")
    (cal-tex-newpage)
    (calendar-for-loop i from 1 to n do
       (insert "\\lefthead")
       (cal-tex-arg
        (let ((d (cal-tex-incr-date date 2)))
          (if (= (extract-calendar-month date)
                 (extract-calendar-month d))
              (format "%s %s"
                      (cal-tex-month-name
                       (extract-calendar-month date))
                      (extract-calendar-year date))
            (if (=  (extract-calendar-year date)
                    (extract-calendar-year d))
                (format "%s---%s %s"
                        (cal-tex-month-name
                         (extract-calendar-month date))
                        (cal-tex-month-name
                         (extract-calendar-month d))
                        (extract-calendar-year date))
              (format "%s %s---%s %s"
                      (cal-tex-month-name
                       (extract-calendar-month date))
                      (extract-calendar-year date)
                      (cal-tex-month-name (extract-calendar-month d))
                      (extract-calendar-year d))))))
       (insert "%\n")
       (calendar-for-loop j from 1 to 3 do 
          (insert "\\leftday")
          (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
          (cal-tex-arg (int-to-string (extract-calendar-day date)))
          (cal-tex-arg (cal-tex-latexify-list diary-list date))
          (cal-tex-arg (cal-tex-latexify-list holidays date))
          (cal-tex-arg (eval cal-tex-daily-string))
          (insert "%\n")
          (setq date (cal-tex-incr-date date)))
       (insert "\\noindent\\rule{\\textwidth}{0.3pt}\\\\%\n")
       (cal-tex-newpage)
       (insert "\\righthead")
       (cal-tex-arg
        (let ((d (cal-tex-incr-date date 3)))
          (if (= (extract-calendar-month date)
                 (extract-calendar-month d))
              (format "%s %s"
                      (cal-tex-month-name
                       (extract-calendar-month date))
                      (extract-calendar-year date))
            (if (=  (extract-calendar-year date)
                    (extract-calendar-year d))
                (format "%s---%s %s"
                        (cal-tex-month-name
                         (extract-calendar-month date))
                        (cal-tex-month-name
                         (extract-calendar-month d))
                        (extract-calendar-year date))
              (format "%s %s---%s %s"
                      (cal-tex-month-name
                       (extract-calendar-month date))
                      (extract-calendar-year date)
                      (cal-tex-month-name (extract-calendar-month d))
                      (extract-calendar-year d))))))
       (insert "%\n")
       (calendar-for-loop j from 1 to 2 do 
          (insert "\\rightday")
          (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
          (cal-tex-arg (int-to-string (extract-calendar-day date)))
          (cal-tex-arg (cal-tex-latexify-list diary-list date))
          (cal-tex-arg (cal-tex-latexify-list holidays date))
          (cal-tex-arg (eval cal-tex-daily-string))
          (insert "%\n")
          (setq date (cal-tex-incr-date date)))
       (calendar-for-loop j from 1 to 2 do 
          (insert "\\weekend")
          (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
          (cal-tex-arg (int-to-string (extract-calendar-day date)))
          (cal-tex-arg (cal-tex-latexify-list diary-list date))
          (cal-tex-arg (cal-tex-latexify-list holidays date))
          (cal-tex-arg (eval cal-tex-daily-string))
          (insert "%\n")
          (setq date (cal-tex-incr-date date)))
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-cursor-filofax-daily (&optional arg)
  "Day-per-page Filofax style calendar for week indicated by cursor.
Optional prefix argument specifies number of weeks.  Weeks start on Monday. 
Diary entries are included if `cal-tex-diary' is t.
Holidays are included if `cal-tex-holidays' is t.
Pages are ruled if `cal-tex-rules' is t."
  (interactive "p")
  (let* ((n (if arg arg 1))
         (date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 1
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date))
         (day (extract-calendar-day date))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays
                        (calendar-absolute-from-gregorian date)
                        (+ (* 7 n)
                           (calendar-absolute-from-gregorian date)))))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          (calendar-absolute-from-gregorian
                           (list month 1 year))
			  (+ (* 7 n)
			     (calendar-absolute-from-gregorian date))))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.75in")
    (cal-tex-cmd "\\evensidemargin 1.5in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\headsep 0.125in")
    (cal-tex-cmd "\\footskip .125in")
    (insert "\\def\\righthead#1{\\hfill {\\normalsize \\bf #1}\\\\[-6pt]}
\\long\\def\\rightday#1#2#3{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox {%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\hfill \\small #3 \\hfill}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\em #2}}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize #1}}}}}
\\long\\def\\weekend#1#2#3{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox {%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\hfill \\small #3 \\hfill}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\em #2}}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize #1}}}}}
\\def\\lefthead#1{\\noindent {\\normalsize \\bf #1}\\hfill\\\\[-6pt]}
\\long\\def\\leftday#1#2#3{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox {%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\hfill \\small #3 \\hfill}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\em #2}}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize #1}}}}}
\\newbox\\LineBox
\\setbox\\LineBox=\\hbox to\\textwidth{%
\\vrule height.2in width0pt\\leaders\\hrule\\hfill}
\\def\\linesfill{\\par\\leaders\\copy\\LineBox\\vfill}
")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (calendar-for-loop i from 1 to n do
       (calendar-for-loop j from 1 to 5 do 
	  (let ((odd (/= 0 (% j 2))))
	    (insert (if odd "\\righthead" "\\lefthead"))
	    (cal-tex-arg (calendar-date-string date))
	    (insert "%\n")
	    (insert (if odd "\\rightday"  "\\leftday")))
	  (cal-tex-arg (cal-tex-latexify-list diary-list date))
	  (cal-tex-arg (cal-tex-latexify-list holidays date "\\\\" t))
	  (cal-tex-arg (eval cal-tex-daily-string))
	  (insert "%\n")
	  (if cal-tex-rules
	      (insert "\\linesfill\n")
	    (insert "\\vfill\\noindent\\rule{\\textwidth}{0.3pt}\\\\%\n"))
	  (cal-tex-newpage)
	  (setq date (cal-tex-incr-date date)))
       (insert "%\n")
       (calendar-for-loop j from 1 to 2 do 
	  (insert "\\lefthead")
          (cal-tex-arg (calendar-date-string date))
          (insert "\\weekend")
          (cal-tex-arg (cal-tex-latexify-list diary-list date))
          (cal-tex-arg (cal-tex-latexify-list holidays date "\\\\" t))
          (cal-tex-arg (eval cal-tex-daily-string))
          (insert "%\n")
          (if cal-tex-rules
              (insert "\\linesfill\n")
            (insert "\\vfill"))
          (setq date (cal-tex-incr-date date)))
       (if (not cal-tex-rules)
	   (insert "\\noindent\\rule{\\textwidth}{0.3pt}\\\\%\n"))
       (if (/= i n)
           (progn
             (run-hooks 'cal-tex-week-hook)
             (cal-tex-newpage))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))


;;;
;;;  Daily calendars
;;;

(defun cal-tex-cursor-day (&optional arg)
  "Make a buffer with LaTeX commands for the day cursor is on.
Optional prefix argument specifies number of days."
  (interactive "p")
  (let ((n (if arg arg 1))
        (date (calendar-absolute-from-gregorian (calendar-cursor-to-date t))))
    (cal-tex-preamble "12pt")
    (cal-tex-cmd "\\textwidth       6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (calendar-for-loop i from 1 to n do
       (cal-tex-vspace "-1.7in")
       (cal-tex-daily-page (calendar-gregorian-from-absolute date))
       (setq date (1+ date))
       (if (/= i n)
          (progn
            (cal-tex-newpage)
            (run-hooks 'cal-tex-daily-hook))))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-daily-page (date)
  "Make a calendar page for Gregorian DATE on 8.5 by 11 paper."
  (let* ((hour)
         (month-name (cal-tex-month-name (extract-calendar-month date))))
    (cal-tex-banner "cal-tex-daily-page")
    (cal-tex-b-makebox "4cm" "l")
    (cal-tex-b-parbox "b" "3.8cm")
    (cal-tex-rule "0mm" "0mm" "2cm")
    (cal-tex-Huge (number-to-string (extract-calendar-day date)))
    (cal-tex-nl ".5cm")
    (cal-tex-bf month-name )
    (cal-tex-e-parbox)
    (cal-tex-hspace "1cm")
    (cal-tex-scriptsize (eval cal-tex-daily-string))
    (cal-tex-hspace "3.5cm")
    (cal-tex-e-makebox)
    (cal-tex-hfill)
    (cal-tex-b-makebox "4cm" "r")
    (cal-tex-bf (cal-tex-LaTeXify-string (calendar-day-name date)))
    (cal-tex-e-makebox)
    (cal-tex-nl)
    (cal-tex-hspace ".4cm")
    (cal-tex-rule "0mm" "16.1cm" "1mm")
    (cal-tex-nl ".1cm")
    (calendar-for-loop i from cal-tex-daily-start to cal-tex-daily-end do
       (cal-tex-cmd "\\noindent")
       (setq hour (if cal-tex-24
                      i
                    (mod i 12)))
       (if (= 0 hour) (setq hour 12))
       (cal-tex-b-makebox "1cm" "c")
       (cal-tex-arg (number-to-string hour))
       (cal-tex-e-makebox)
       (cal-tex-rule "0mm" "15.5cm" ".2mm")
       (cal-tex-nl ".2cm")
       (cal-tex-b-makebox "1cm" "c")
       (cal-tex-arg "$\\diamond$" )
       (cal-tex-e-makebox)
       (cal-tex-rule "0mm" "15.5cm" ".2mm")
       (cal-tex-nl ".2cm"))
    (cal-tex-hfill)
    (insert (cal-tex-mini-calendar
             (extract-calendar-month (cal-tex-previous-month date))
             (extract-calendar-year (cal-tex-previous-month date))
             "lastmonth" "1.1in" "1in"))
    (insert (cal-tex-mini-calendar
             (extract-calendar-month date)
             (extract-calendar-year date)
             "thismonth" "1.1in" "1in"))
    (insert (cal-tex-mini-calendar
             (extract-calendar-month (cal-tex-next-month date))
             (extract-calendar-year (cal-tex-next-month date))
             "nextmonth" "1.1in" "1in"))
    (insert "\\hbox to \\textwidth{")
    (cal-tex-hfill)
    (insert "\\lastmonth")
    (cal-tex-hfill)
    (insert "\\thismonth")
    (cal-tex-hfill)
    (insert "\\nextmonth")
    (cal-tex-hfill)
    (insert "}")
    (cal-tex-banner "end of cal-tex-daily-page")))
    
;;;
;;;  Mini calendars
;;;

(defun cal-tex-mini-calendar (month year name width height &optional ptsize colsep)
  "Produce mini-calendar for MONTH, YEAR in macro NAME with WIDTH and HEIGHT.
Optional PTSIZE gives the point ptsize; scriptsize is the default.  Optional
COLSEP gives the column separation; 1mm is the default."
  (let* ((blank-days;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
          (last (calendar-last-day-of-month month year))
         (colsep (if colsep colsep "1mm"))
         (str (concat "\\def\\" name "{\\hbox to" width "{%\n"
                      "\\vbox to" height "{%\n"
                      "\\vfil  \\hbox to" width "{%\n"
                      "\\hfil\\"
                      (if ptsize ptsize "scriptsize")
                      "\\begin{tabular}"
                      "{@{\\hspace{0mm}}r@{\\hspace{" colsep
                      "}}r@{\\hspace{" colsep "}}r@{\\hspace{" colsep
                      "}}r@{\\hspace{" colsep "}}r@{\\hspace{" colsep
                      "}}r@{\\hspace{" colsep "}}r@{\\hspace{0mm}}}%\n"
                      "\\multicolumn{7}{c}{"
                      (cal-tex-month-name month)
                      " "
                      (int-to-string year)
                      "}\\\\[1mm]\n")))
    (calendar-for-loop i from 0 to 6 do
      (setq str
            (concat str
                    (cal-tex-LaTeXify-string
                     (substring (aref calendar-day-name-array 
                                      (mod (+ calendar-week-start-day i) 7))
                                0 2))
                    (if (/= i 6)
                        " & "
                      "\\\\[0.7mm]\n"))))
    (calendar-for-loop i from 1 to blank-days do
      (setq str (concat str " & ")))
    (calendar-for-loop i from 1 to last do
      (setq str (concat str (int-to-string i)))
      (setq str (concat str (if (zerop (mod (+ i blank-days) 7))
                                (if (/= i last) "\\\\[0.5mm]\n" "")
                              " & "))))
    (setq str (concat str "\n\\end{tabular}\\hfil}\\vfil}}}%\n"))
   str))

;;;
;;;  Various calendar functions
;;;

(defun cal-tex-incr-date (date &optional n)
  "The date of the day following DATE.
If optional N is given, the date of N days after DATE."
  (calendar-gregorian-from-absolute
   (+ (if n n 1) (calendar-absolute-from-gregorian date))))

(defun cal-tex-latexify-list (date-list date &optional separator final-separator)
  "Return string with concatenated, LaTeXified entries in DATE-LIST for DATE.
Use double backslash as a separator unless optional SEPARATOR is given.
If resulting string is not empty, put separator at end if optional
FINAL-SEPARATOR is t."
  (let* ((sep (if separator separator "\\\\"))
         (result
          (mapconcat '(lambda (x) (cal-tex-LaTeXify-string  x))
                     (let ((result)
                           (p date-list))
                       (while p
                         (and (car (car p))
                              (calendar-date-equal date (car (car p)))
                              (setq result (cons (car (cdr (car p))) result)))
                         (setq p (cdr p)))
                       (reverse result))
                     sep)))
    (if (and final-separator (not (string-equal result "")))
          (concat result sep)
        result)))

(defun cal-tex-previous-month (date)
  "Return the date of the first day in the month previous to DATE."
  (let* ((month (extract-calendar-month date))
         (year (extract-calendar-year date)))
    (increment-calendar-month month year -1)
    (list month 1 year)))

(defun cal-tex-next-month (date)
  "Return the date of the first day in the  month following DATE."
  (let* ((month (extract-calendar-month date))
         (year (extract-calendar-year date)))
    (increment-calendar-month month year 1)
    (list month 1 year)))

;;;
;;;  LaTeX Code
;;;

(defun cal-tex-end-document ()
  "Finish the LaTeX document.
Insert the trailer to LaTeX document, pop to LaTeX buffer, add 
informative header, and run HOOK."
  (cal-tex-e-document)
  (latex-mode)
  (pop-to-buffer cal-tex-buffer)
  (goto-char (point-min))    
  (cal-tex-comment "       This buffer was produced by cal-tex.el.")
  (cal-tex-comment "       To print a calendar, type")
  (cal-tex-comment "          M-x tex-buffer RET")
  (cal-tex-comment "          M-x tex-print  RET")
  (goto-char (point-min)))

(defun cal-tex-insert-preamble (weeks landscape size &optional append)
  "Initialize the output buffer.
Select the output buffer, and insert the preamble for a calendar of 
WEEKS weeks.  Insert code for landscape mode if LANDSCAPE is true. 
Use pointsize SIZE.  Optional argument APPEND, if t, means add to end of
without erasing current contents."
  (let ((width "18cm")
        (height "24cm"))
    (if landscape
        (progn
          (setq width "24cm")
          (setq height "18cm")))
    (if (not append)
        (progn
          (cal-tex-preamble size)
          (if (not landscape) 
            (progn
              (cal-tex-cmd "\\oddsidemargin -1.75cm")
              (cal-tex-cmd "\\def\\holidaymult{.06}"))
            (cal-tex-cmd "\\special{landscape}")
            (cal-tex-cmd "\\textwidth  9.5in")
            (cal-tex-cmd "\\textheight 7in")
            (cal-tex-comment)
            (cal-tex-cmd "\\def\\holidaymult{.08}"))
          (cal-tex-cmd  cal-tex-caldate)
          (cal-tex-cmd  cal-tex-myday)
          (cal-tex-b-document)
          (cal-tex-cmd "\\pagestyle{empty}")))
    (cal-tex-cmd "\\setlength{\\cellwidth}" width)
    (insert (format "\\setlength{\\cellwidth}{%f\\cellwidth}\n"
                    (/ 1.1 (length cal-tex-which-days))))
    (cal-tex-cmd "\\setlength{\\cellheight}" height)
    (insert (format "\\setlength{\\cellheight}{%f\\cellheight}\n"
                    (/ 1.0 weeks)))              
    (cal-tex-cmd "\\ \\par")
    (cal-tex-vspace "-3cm")))

(defvar cal-tex-LaTeX-subst-list
  '(("\"". "``")
    ("\"". "''");; Quote changes meaning when list is reversed.
    ("@" . "\\verb|@|") 
    ("&" . "\\&") 
    ("%" . "\\%") 
    ("$" . "\\$")
    ("#" . "\\#")
    ("_" . "\\_")
    ("{" . "\\{")
    ("}" . "\\}")
    ("<" . "$<$")
    (">" . "$>$")
    ("\n" . "\\ \\\\"))  ;\\ needed for e.g \begin{center}\n AA\end{center}
  "List of symbols and their replacements.")

(defun cal-tex-LaTeXify-string (string)
  "Protect special characters in STRING from LaTeX."
  (if (not string)
      ""
    (let ((head "")
          (tail string)
          (list cal-tex-LaTeX-subst-list))
      (while (not (string-equal tail ""))
        (let* ((ch (substring tail 0 1))
               (pair (assoc ch list)))
          (if (and pair (string-equal ch "\""))
              (setq list (reverse list)));; Quote changes meaning each time.
          (setq tail (substring tail 1))
          (setq head (concat head (if pair (cdr pair) ch)))))
      head)))

(defun cal-tex-month-name (month)
  "The name of MONTH, LaTeXified."
  (cal-tex-LaTeXify-string (calendar-month-name month)))

(defun cal-tex-hfill () "Insert hfill." (insert "\\hfill"))

(defun cal-tex-newpage () "Insert newpage." (insert "\\newpage%\n"))

(defun cal-tex-noindent () "Insert noindent." (insert "\\noindent"))

(defun cal-tex-vspace (space)
  "Insert vspace command to move SPACE vertically."
  (insert "\\vspace*{" space "}")
  (cal-tex-comment))

(defun cal-tex-hspace (space)
  "Insert hspace command to move SPACE horizontally."
  (insert "\\hspace*{" space "}")
  (cal-tex-comment))

(defun cal-tex-comment (&optional comment)
  "Insert % at end of line, include COMMENT if present, and move
   to next line."
  (insert "% ")
  (if comment
      (insert comment))
  (insert "\n"))

(defun cal-tex-banner (comment)
  "Insert the COMMENT separated by blank lines."
  (cal-tex-comment)
  (cal-tex-comment)
  (cal-tex-comment (concat "\t\t\t" comment))
  (cal-tex-comment))


(defun cal-tex-nl (&optional skip comment)
  "End a line with \\.  If SKIP, then add that much spacing.
   Add COMMENT if present"
  (insert "\\\\")       
  (if skip 
      (insert "[" skip "]"))
  (cal-tex-comment comment))
    
(defun cal-tex-arg (&optional text)
  "Insert optional TEXT surrounded by braces."
  (insert "{")
  (if text (insert text))
  (insert "}"))

(defun cal-tex-cmd (cmd &optional arg)
  "Insert LaTeX CMD, with optional ARG, and end with %"
  (insert cmd)
  (cal-tex-arg arg)
  (cal-tex-comment))

;;;
;;;   Environments
;;;

(defun cal-tex-b-document ()
  "Insert beginning of document."
  (cal-tex-cmd "\\begin{document}"))

(defun cal-tex-e-document ()
  "Insert end of document."
  (cal-tex-cmd "\\end{document}"))

(defun cal-tex-b-center ()
  "Insert beginning of centered block."
  (cal-tex-cmd "\\begin{center}"))

(defun cal-tex-e-center ()
  "Insert end of centered block."
  (cal-tex-comment)
  (cal-tex-cmd "\\end{center}"))


;;;
;;;  Boxes
;;;


(defun cal-tex-b-parbox (position width)
  "Insert parbox with parameters POSITION and WIDTH."
  (insert "\\parbox[" position "]{" width "}{")
  (cal-tex-comment))

(defun cal-tex-e-parbox (&optional height)
  "Insert end of parbox. Force it to be a given HEIGHT."
  (cal-tex-comment)
  (if height
      (cal-tex-rule "0mm" "0mm" height))
  (insert "}")
  (cal-tex-comment "end parbox"))

(defun cal-tex-b-framebox ( width position )
  "Insert  framebox with parameters WIDTH and POSITION (clr)."
  (insert "\\framebox[" width "][" position "]{" )
  (cal-tex-comment))

(defun cal-tex-e-framebox ()
  "Insert end of framebox."
  (cal-tex-comment)
  (insert "}")
  (cal-tex-comment "end framebox"))


(defun cal-tex-b-makebox ( width position )
  "Insert  makebox with parameters WIDTH and POSITION (clr)."
  (insert "\\makebox[" width "][" position "]{" )
  (cal-tex-comment))

(defun cal-tex-e-makebox ()
  "Insert end of makebox."
  (cal-tex-comment)
  (insert "}")
  (cal-tex-comment "end makebox"))


(defun cal-tex-rule (lower width height)
  "Insert a rule with parameters LOWER WIDTH HEIGHT."
  (insert "\\rule[" lower "]{" width "}{" height "}"))

;;;
;;;     Fonts
;;;

(defun cal-tex-em (string)
  "Insert STRING in bf font."
  (insert "{\\em " string "}"))

(defun cal-tex-bf (string)
  "Insert STRING in bf font."
  (insert "{\\bf " string "}"))

(defun cal-tex-scriptsize (string)
  "Insert STRING in scriptsize font."
  (insert "{\\scriptsize " string "}"))

(defun cal-tex-huge (string)
  "Insert STRING in huge size."
  (insert "{\\huge " string "}"))

(defun cal-tex-Huge (string)
  "Insert STRING in Huge size."
  (insert "{\\Huge " string "}"))

(defun cal-tex-Huge-bf (string)
  "Insert STRING in Huge bf size."
  (insert "{\\Huge\\bf " string "}"))

(defun cal-tex-large (string)
  "Insert STRING in large size."
  (insert "{\\large " string "}"))

(defun cal-tex-large-bf (string)
  "Insert STRING in large bf size."
  (insert "{\\large\\bf  " string "}"))

(provide 'cal-tex)

;;; cal-tex.el ends here
