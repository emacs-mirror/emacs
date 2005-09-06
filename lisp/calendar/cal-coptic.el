;;; cal-coptic.el --- calendar functions for the Coptic/Ethiopic calendars

;; Copyright (C) 1995, 1997 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: Coptic calendar, Ethiopic calendar, calendar, diary

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This collection of functions implements the features of calendar.el and
;; diary.el that deal with the Coptic and Ethiopic calendars.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(defvar date)

(require 'cal-julian)

(defvar coptic-calendar-month-name-array
  ["Tut" "Babah" "Hatur" "Kiyahk" "Tubah" "Amshir" "Baramhat" "Barmundah"
   "Bashans" "Baunah" "Abib" "Misra" "al-Nasi"])

(defvar coptic-calendar-epoch (calendar-absolute-from-julian '(8 29 284))
  "Absolute date of start of Coptic calendar = August 29, 284 A.D. (Julian).")

(defvar coptic-name "Coptic")

(defun coptic-calendar-leap-year-p (year)
  "True if YEAR is a leap year on the Coptic calendar."
  (zerop (mod (1+ year) 4)))

(defun coptic-calendar-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the Coptic calendar.
The 13th month is not really a month, but the 5 (6 in leap years) day period of
Nisi (Kebus)  at the end of the year."
  (if (< month 13)
      30
    (if (coptic-calendar-leap-year-p year)
        6
      5)))

(defun calendar-absolute-from-coptic (date)
  "Compute absolute date from Coptic date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ (1- coptic-calendar-epoch);; Days before start of calendar
       (* 365 (1- year))         ;; Days in prior years
       (/ year 4)                ;; Leap days in prior years
       (* 30 (1- month))         ;; Days in prior months this year
       day)))                    ;; Days so far this month


(defun calendar-coptic-from-absolute (date)
  "Compute the Coptic equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (if (< date coptic-calendar-epoch)
      (list 0 0 0);; pre-Coptic date
    (let* ((approx (/ (- date coptic-calendar-epoch)
                      366))   ;; Approximation from below.
           (year              ;; Search forward from the approximation.
            (+ approx
               (calendar-sum y approx
                 (>= date (calendar-absolute-from-coptic (list 1 1 (1+ y))))
                 1)))
           (month             ;; Search forward from Tot.
            (1+ (calendar-sum m 1
                  (> date
                     (calendar-absolute-from-coptic
                      (list m
                            (coptic-calendar-last-day-of-month m year)
                            year)))
                  1)))
           (day                ;; Calculate the day by subtraction.
            (- date
               (1- (calendar-absolute-from-coptic (list month 1 year))))))
    (list month day year))))

(defun calendar-coptic-date-string (&optional date)
  "String of Coptic date of Gregorian DATE.
Returns the empty string if DATE is pre-Coptic calendar.
Defaults to today's date if DATE is not given."
  (let* ((coptic-date (calendar-coptic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (y (extract-calendar-year coptic-date))
         (m (extract-calendar-month coptic-date)))
    (if (< y 1)
        ""
      (let ((monthname (aref coptic-calendar-month-name-array (1- m)))
            (day (int-to-string (extract-calendar-day coptic-date)))
            (dayname nil)
            (month (int-to-string m))
            (year (int-to-string y)))
        (mapconcat 'eval calendar-date-display-form "")))))

(defun calendar-print-coptic-date ()
  "Show the Coptic calendar equivalent of the selected date."
  (interactive)
  (let ((f (calendar-coptic-date-string (calendar-cursor-to-date t))))
    (if (string-equal f "")
        (message "Date is pre-%s calendar" coptic-name)
      (message "%s date: %s" coptic-name f))))

(defun calendar-goto-coptic-date (date &optional noecho)
  "Move cursor to Coptic date DATE.
Echo Coptic date unless NOECHO is t."
  (interactive (coptic-prompt-for-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-coptic date)))
  (or noecho (calendar-print-coptic-date)))

(defun coptic-prompt-for-date ()
  "Ask for a Coptic date."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                (format "%s calendar year (>0): " coptic-name)
                '(lambda (x) (> x 0))
                (int-to-string
                 (extract-calendar-year
                  (calendar-coptic-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (completion-ignore-case t)
         (month (cdr (assoc-string
                      (completing-read
                       (format "%s calendar month name: " coptic-name)
                       (mapcar 'list
                               (append coptic-calendar-month-name-array nil))
                       nil t)
                      (calendar-make-alist coptic-calendar-month-name-array
                                           1) t)))
         (last (coptic-calendar-last-day-of-month month year))
         (day (calendar-read
               (format "%s calendar day (1-%d): " coptic-name last)
               '(lambda (x) (and (< 0 x) (<= x last))))))
    (list (list month day year))))

(defun diary-coptic-date ()
  "Coptic calendar equivalent of date diary entry."
  (let ((f (calendar-coptic-date-string date)))
    (if (string-equal f "")
        (format "Date is pre-%s calendar" coptic-name)
      (format "%s date: %s" coptic-name f))))

(defconst ethiopic-calendar-month-name-array
  ["Maskaram" "Teqemt" "Khedar" "Takhsas" "Ter" "Yakatit" "Magabit" "Miyazya"
   "Genbot" "Sane" "Hamle" "Nahas" "Paguem"])

(defconst ethiopic-calendar-epoch 2796
  "Absolute date of start of Ethiopic calendar = August 29, 8 C.E. (Julian).")

(defconst ethiopic-name "Ethiopic")

(defun calendar-absolute-from-ethiopic (date)
  "Compute absolute date from Ethiopic date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((coptic-calendar-epoch ethiopic-calendar-epoch))
    (calendar-absolute-from-coptic date)))

(defun calendar-ethiopic-from-absolute (date)
  "Compute the Ethiopic equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (let ((coptic-calendar-epoch ethiopic-calendar-epoch))
    (calendar-coptic-from-absolute date)))

(defun calendar-ethiopic-date-string (&optional date)
  "String of Ethiopic date of Gregorian DATE.
Returns the empty string if DATE is pre-Ethiopic calendar.
Defaults to today's date if DATE is not given."
  (let ((coptic-calendar-epoch ethiopic-calendar-epoch)
        (coptic-name ethiopic-name)
        (coptic-calendar-month-name-array ethiopic-calendar-month-name-array))
    (calendar-coptic-date-string date)))

(defun calendar-print-ethiopic-date ()
  "Show the Ethiopic calendar equivalent of the selected date."
  (interactive)
  (let ((coptic-calendar-epoch ethiopic-calendar-epoch)
        (coptic-name ethiopic-name)
        (coptic-calendar-month-name-array ethiopic-calendar-month-name-array))
    (call-interactively 'calendar-print-coptic-date)))

(defun calendar-goto-ethiopic-date (date &optional noecho)
  "Move cursor to Ethiopic date DATE.
Echo Ethiopic date unless NOECHO is t."
  (interactive
   (let ((coptic-calendar-epoch ethiopic-calendar-epoch)
         (coptic-name ethiopic-name)
         (coptic-calendar-month-name-array ethiopic-calendar-month-name-array))
     (coptic-prompt-for-date)))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-ethiopic date)))
  (or noecho (calendar-print-ethiopic-date)))

(defun diary-ethiopic-date ()
  "Ethiopic calendar equivalent of date diary entry."
  (let ((coptic-calendar-epoch ethiopic-calendar-epoch)
        (coptic-name ethiopic-name)
        (coptic-calendar-month-name-array ethiopic-calendar-month-name-array))
    (diary-coptic-date)))

(provide 'cal-coptic)

;;; arch-tag: 72d49161-25df-4072-9312-b182cdca7627
;;; cal-coptic.el ends here
