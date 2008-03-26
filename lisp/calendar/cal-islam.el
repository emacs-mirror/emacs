;;; cal-islam.el --- calendar functions for the Islamic calendar

;; Copyright (C) 1995, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Islamic calendar, calendar, diary

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

;; This collection of functions implements the features of calendar.el and
;; diary.el that deal with the Islamic calendar.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;;; Code:

(require 'calendar)

(defconst calendar-islamic-month-name-array
  ["Muharram" "Safar" "Rabi I" "Rabi II" "Jumada I" "Jumada II"
   "Rajab" "Sha'ban" "Ramadan" "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"]
"Array of strings giving the names of the Islamic months.")

(eval-and-compile
  (autoload 'calendar-absolute-from-julian "cal-julian"))

(defconst calendar-islamic-epoch
  (eval-when-compile (calendar-absolute-from-julian '(7 16 622)))
  "Absolute date of start of Islamic calendar = July 16, 622 AD (Julian).")

(defun islamic-calendar-leap-year-p (year)
  "Return t if YEAR is a leap year on the Islamic calendar."
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
  (let ((month (extract-calendar-month date)))
    (+ (* 30 (/ month 2))
       (* 29 (/ (1- month) 2))
       (extract-calendar-day date))))

(defun calendar-absolute-from-islamic (date)
  "Absolute date of Islamic DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (y (% year 30))
         (leap-years-in-cycle (cond ((< y 3) 0)
                                    ((< y 6) 1)
                                    ((< y 8) 2)
                                    ((< y 11) 3)
                                    ((< y 14) 4)
                                    ((< y 17) 5)
                                    ((< y 19) 6)
                                    ((< y 22) 7)
                                    ((< y 25) 8)
                                    ((< y 27) 9)
                                    (t 10))))
    (+ (islamic-calendar-day-number date) ; days so far this year
       (* (1- year) 354)                  ; days in all non-leap years
       (* 11 (/ year 30))             ; leap days in complete cycles
       leap-years-in-cycle            ; leap days this cycle
       (1- calendar-islamic-epoch)))) ; days before start of calendar

(defun calendar-islamic-from-absolute (date)
  "Compute the Islamic date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (if (< date calendar-islamic-epoch)
      (list 0 0 0)                      ; pre-Islamic date
    (let* ((approx (/ (- date calendar-islamic-epoch)
                      355))  ; approximation from below
           (year             ; search forward from the approximation
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-absolute-from-islamic
                                       (list 1 1 (1+ y))))
                             1)))
           (month                       ; search forward from Muharram
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-absolute-from-islamic
                                  (list m
                                        (islamic-calendar-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day                    ; calculate the day by subtraction
            (- date
               (1- (calendar-absolute-from-islamic (list month 1 year))))))
      (list month day year))))

;;;###cal-autoload
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

;;;###cal-autoload
(defun calendar-print-islamic-date ()
  "Show the Islamic calendar equivalent of the date under the cursor."
  (interactive)
  (let ((i (calendar-islamic-date-string (calendar-cursor-to-date t))))
    (if (string-equal i "")
        (message "Date is pre-Islamic")
      (message "Islamic date (until sunset): %s" i))))

(defun calendar-islamic-read-date ()
  "Interactively read the arguments for an Islamic date command.
Reads a year, month, and day."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                "Islamic calendar year (>0): "
                (lambda (x) (> x 0))
                (int-to-string
                 (extract-calendar-year
                  (calendar-islamic-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (month-array calendar-islamic-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc-string
                      (completing-read
                       "Islamic calendar month name: "
                       (mapcar 'list (append month-array nil))
                       nil t)
                      (calendar-make-alist month-array 1) t)))
         (last (islamic-calendar-last-day-of-month month year))
         (day (calendar-read
               (format "Islamic calendar day (1-%d): " last)
               (lambda (x) (and (< 0 x) (<= x last))))))
    (list (list month day year))))

;;;###cal-autoload
(defun calendar-goto-islamic-date (date &optional noecho)
  "Move cursor to Islamic DATE; echo Islamic date unless NOECHO is non-nil."
  (interactive (calendar-islamic-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic date)))
  (or noecho (calendar-print-islamic-date)))

(defvar displayed-month)                ; from generate-calendar
(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-islamic (month day string)
  "Holiday on MONTH, DAY (Islamic) called STRING.
If MONTH, DAY (Islamic) is visible, the value returned is corresponding
Gregorian date in the form of the list (((month day year) STRING)).  Returns
nil if it is not visible in the current calendar window."
  (let* ((islamic-date (calendar-islamic-from-absolute
                        (calendar-absolute-from-gregorian
                         (list displayed-month 15 displayed-year))))
         (m (extract-calendar-month islamic-date))
         (y (extract-calendar-year islamic-date))
        (date))
    (unless (< m 1)                   ; Islamic calendar doesn't apply
      (increment-calendar-month m y (- 10 month))
      (if (> m 7)                      ; Islamic date might be visible
          (let ((date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic (list month day y)))))
            (if (calendar-date-is-visible-p date)
                (list (list date string))))))))

(autoload 'diary-list-entries-1 "diary-lib")

;;;###diary-autoload
(defun list-islamic-diary-entries ()
  "Add any Islamic date entries from the diary file to `diary-entries-list'.
Islamic date diary entries must be prefaced by `islamic-diary-entry-symbol'
\(normally an `I').  The same `diary-date-forms' govern the style
of the Islamic calendar entries, except that the Islamic month
names must be spelled in full.  The Islamic months are numbered
from 1 to 12 with Muharram being 1 and 12 being Dhu al-Hijjah.
If an Islamic date diary entry begins with `diary-nonmarking-symbol',
the entry will appear in the diary listing, but will not be
marked in the calendar.  This function is provided for use with
`nongregorian-diary-listing-hook'."
  (diary-list-entries-1 calendar-islamic-month-name-array
                        islamic-diary-entry-symbol
                        'calendar-islamic-from-absolute))

(autoload 'calendar-mark-1 "diary-lib")

;;;###diary-autoload
(defun mark-islamic-calendar-date-pattern (month day year &optional color)
  "Mark dates in calendar window that conform to Islamic date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `mark-visible-calendar-date' as MARK."
  (calendar-mark-1 month day year 'calendar-islamic-from-absolute
                   'calendar-absolute-from-islamic color))

(autoload 'diary-mark-entries-1 "diary-lib")

;;;###diary-autoload
(defun mark-islamic-diary-entries ()
  "Mark days in the calendar window that have Islamic date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `list-islamic-diary-entries' for more information."
  (diary-mark-entries-1 calendar-islamic-month-name-array
                        islamic-diary-entry-symbol
                        'calendar-islamic-from-absolute
                        'mark-islamic-calendar-date-pattern))

;;;###cal-autoload
(defun insert-islamic-diary-entry (arg)
  "Insert a diary entry.
For the Islamic date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-month-name-array calendar-islamic-month-name-array))
    (make-diary-entry
     (concat islamic-diary-entry-symbol
             (calendar-date-string
              (calendar-islamic-from-absolute
               (calendar-absolute-from-gregorian (calendar-cursor-to-date t)))
              nil t))
     arg)))

;;;###cal-autoload
(defun insert-monthly-islamic-diary-entry (arg)
  "Insert a monthly diary entry.
For the day of the Islamic month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (if european-calendar-style
                                        '(day " * ")
                                      '("* " day )))
        (calendar-month-name-array calendar-islamic-month-name-array))
    (make-diary-entry
     (concat islamic-diary-entry-symbol
             (calendar-date-string
              (calendar-islamic-from-absolute
               (calendar-absolute-from-gregorian (calendar-cursor-to-date t)))))
     arg)))

;;;###cal-autoload
(defun insert-yearly-islamic-diary-entry (arg)
  "Insert an annual diary entry.
For the day of the Islamic year corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (if european-calendar-style
                                        '(day " " monthname)
                                      '(monthname " " day)))
        (calendar-month-name-array calendar-islamic-month-name-array))
    (make-diary-entry
     (concat islamic-diary-entry-symbol
             (calendar-date-string
              (calendar-islamic-from-absolute
               (calendar-absolute-from-gregorian (calendar-cursor-to-date t)))))
     arg)))

(defvar date)

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
;;;###diary-autoload
(defun diary-islamic-date ()
  "Islamic calendar equivalent of date diary entry."
  (let ((i (calendar-islamic-date-string date)))
    (if (string-equal i "")
        "Date is pre-Islamic"
      (format "Islamic date (until sunset): %s" i))))

(provide 'cal-islam)

;; arch-tag: a951b6c1-6f47-48d5-bac3-1b505cd719f7
;;; cal-islam.el ends here
