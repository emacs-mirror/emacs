;;; cal-julian.el --- calendar functions for the Julian calendar

;; Copyright (C) 1995, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Julian calendar, Julian day number, calendar, diary

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
;; diary.el that deal with the Julian calendar.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;;; Code:

(defvar displayed-month)
(defvar displayed-year)

(require 'calendar)

;;;###autoload
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
       (if (and (zerop (% year 100))
                (/= (% year 400) 0)
                (> month 2))
           1 0);; Correct for Julian but not Gregorian leap year.
       (* 365 (1- year))
       (/ (1- year) 4)
       -2)))

;;;###autoload
(defun calendar-julian-date-string (&optional date)
  "String of Julian date of Gregorian DATE.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'."
  (calendar-date-string
   (calendar-julian-from-absolute
    (calendar-absolute-from-gregorian
     (or date (calendar-current-date))))
   nil t))

;;;###autoload
(defun calendar-print-julian-date ()
  "Show the Julian calendar equivalent of the date under the cursor."
  (interactive)
  (message "Julian date: %s"
           (calendar-julian-date-string (calendar-cursor-to-date t))))

;;;###autoload
(defun calendar-goto-julian-date (date &optional noecho)
  "Move cursor to Julian DATE; echo Julian date unless NOECHO is t."
  (interactive
   (let* ((today (calendar-current-date))
          (year (calendar-read
                 "Julian calendar year (>0): "
                 (lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-julian-from-absolute
                    (calendar-absolute-from-gregorian
                     today))))))
          (month-array calendar-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc-string
                        (completing-read
                         "Julian calendar month name: "
                         (mapcar 'list (append month-array nil))
                         nil t)
                       (calendar-make-alist month-array 1) t)))
          (last
           (if (and (zerop (% year 4)) (= month 2))
               29
             (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))
          (day (calendar-read
                (format "Julian calendar day (%d-%d): "
                        (if (and (= year 1) (= month 1)) 3 1) last)
                (lambda (x)
                   (and (< (if (and (= year 1) (= month 1)) 2 0) x)
                        (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-julian date)))
  (or noecho (calendar-print-julian-date)))

(defun holiday-julian (month day string)
  "Holiday on MONTH, DAY  (Julian) called STRING.
If MONTH, DAY (Julian) is visible, the value returned is corresponding
Gregorian date in the form of the list (((month day year) STRING)).  Returns
nil if it is not visible in the current calendar window."
  (let ((m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year)
        (year))
    (increment-calendar-month m1 y1 -1)
    (increment-calendar-month m2 y2 1)
    (let* ((start-date (calendar-absolute-from-gregorian
                        (list m1 1 y1)))
           (end-date (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2)))
           (julian-start (calendar-julian-from-absolute start-date))
           (julian-end (calendar-julian-from-absolute end-date))
           (julian-y1 (extract-calendar-year julian-start))
           (julian-y2 (extract-calendar-year julian-end)))
      (setq year (if (< 10 month) julian-y1 julian-y2))
      (let ((date (calendar-gregorian-from-absolute
                   (calendar-absolute-from-julian
                    (list month day year)))))
        (if (calendar-date-is-visible-p date)
            (list (list date string)))))))

(defvar date)

;; To be called from list-sexp-diary-entries, where DATE is bound.
(defun diary-julian-date ()
  "Julian calendar equivalent of date diary entry."
  (format "Julian date: %s" (calendar-julian-date-string date)))

;;;###autoload
(defun calendar-absolute-from-astro (d)
  "Absolute date of astronomical (Julian) day number D."
  (- d 1721424.5))

;;;###autoload
(defun calendar-astro-from-absolute (d)
  "Astronomical (Julian) day number of absolute date D."
  (+ d 1721424.5))

;;;###autoload
(defun calendar-astro-date-string (&optional date)
  "String of astronomical (Julian) day number after noon UTC of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (int-to-string
   (ceiling
    (calendar-astro-from-absolute
     (calendar-absolute-from-gregorian
      (or date (calendar-current-date)))))))

;;;###autoload
(defun calendar-print-astro-day-number ()
  "Show astronomical (Julian) day number after noon UTC on date shown by cursor."
  (interactive)
  (message
   "Astronomical (Julian) day number (at noon UTC): %s.0"
   (calendar-astro-date-string (calendar-cursor-to-date t))))

;;;###autoload
(defun calendar-goto-astro-day-number (daynumber &optional noecho)
  "Move cursor to astronomical (Julian) DAYNUMBER.
Echo astronomical (Julian) day number unless NOECHO is t."
  (interactive (list (calendar-read
                      "Astronomical (Julian) day number (>1721425): "
                      (lambda (x) (> x 1721425)))))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (floor
     (calendar-absolute-from-astro daynumber))))
  (or noecho (calendar-print-astro-day-number)))

;; To be called from list-sexp-diary-entries, where DATE is bound.
(defun diary-astro-day-number ()
  "Astronomical (Julian) day number diary entry."
  (format "Astronomical (Julian) day number at noon UTC: %s.0"
          (calendar-astro-date-string date)))

(provide 'cal-julian)

;; Local Variables:
;; generated-autoload-file: "cal-loaddefs.el"
;; End:

;; arch-tag: 0520acdd-1c60-4188-9aa8-9b8c24d856ae
;;; cal-julian.el ends here
