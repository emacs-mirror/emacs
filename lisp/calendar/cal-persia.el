;;; cal-persia.el --- calendar functions for the Persian calendar

;; Copyright (C) 1996, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Persian calendar, calendar, diary

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
;; diary.el that deal with the Persian calendar.

;;; Code:

(defvar date)

(require 'cal-julian)

(defvar persian-calendar-month-name-array
  ["Farvardin" "Ordibehest" "Xordad" "Tir" "Mordad" "Sahrivar" "Mehr" "Aban"
   "Azar" "Dey" "Bahman" "Esfand"])

(defvar persian-calendar-epoch (calendar-absolute-from-julian '(3 19 622))
  "Absolute date of start of Persian calendar = March 19, 622 A.D. (Julian).")

(defun persian-calendar-leap-year-p (year)
  "True if YEAR is a leap year on the Persian calendar."
  (< (mod (* (mod (mod (if (<= 0 year)
                           ; No year zero
                           (+ year 2346)
                         (+ year 2347))
                       2820)
                  768)
              683)
           2820)
      683))

(defun persian-calendar-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the Persian calendar."
  (cond
   ((< month 7) 31)
   ((or (< month 12) (persian-calendar-leap-year-p year)) 30)
   (t 29)))

(defun calendar-absolute-from-persian (date)
  "Compute absolute date from Persian date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (if (< year 0)
        (+ (calendar-absolute-from-persian
            (list month day (1+ (mod year 2820))))
           (* 1029983 (floor year 2820)))
      (+ (1- persian-calendar-epoch); Days before epoch
         (* 365 (1- year)) ; Days in prior years.
         (* 683        ; Leap days in prior 2820-year cycles
            (floor (+ year 2345) 2820))
         (* 186        ; Leap days in prior 768 year cycles
            (floor (mod (+ year 2345) 2820) 768))
         (floor; Leap years in current 768 or 516 year cycle
          (* 683 (mod (mod (+ year 2345) 2820) 768))
          2820)
         -568          ; Leap years in Persian years -2345...-1
         (calendar-sum ; Days in prior months this year.
          m 1 (< m month)
          (persian-calendar-last-day-of-month m year))
         day))))        ; Days so far this month.

(defun calendar-persian-year-from-absolute (date)
  "Persian year corresponding to the absolute DATE."
  (let* ((d0        ; Prior days since start of 2820 cycles
          (- date (calendar-absolute-from-persian (list 1 1 -2345))))
         (n2820     ; Completed 2820-year cycles
          (floor d0 1029983))
         (d1        ; Prior days not in n2820
          (mod d0 1029983))
         (n768      ; 768-year cycles not in n2820
          (floor d1 280506))
         (d2        ; Prior days not in n2820 or n768
          (mod d1 280506))
         (n1        ; Years not in n2820 or n768
          ; we want is
          ; (floor (+ (* 2820 d2) (* 2820 366)) 1029983))
          ; but that causes overflow, so we use
          (let ((a (floor d2 366)); we use 366 as the divisor because
                                  ; (2820*366 mod 1029983) is small
                (b (mod d2 366)))
            (+ 1 a (floor (+ (* 2137 a) (* 2820 b) 2137) 1029983))))
         (year (+ (* 2820 n2820); Complete 2820 year cycles
                  (* 768 n768)  ; Complete 768 year cycles
                  (if           ; Remaining years
                      ; Last day of 2820 year cycle
                      (= d1 1029617)
                      (1- n1)
                    n1)
                  -2345)))      ; Years before year 1
    (if (< year 1)
        (1- year); No year zero
      year)))

(defun calendar-persian-from-absolute (date)
  "Compute the Persian equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (let* ((year (calendar-persian-year-from-absolute date))
         (month         ; Search forward from Farvardin
          (1+ (calendar-sum m 1
                            (> date
                               (calendar-absolute-from-persian
                                (list
                                 m
                                 (persian-calendar-last-day-of-month m year)
                                 year)))
                            1)))
         (day           ; Calculate the day by subtraction
          (- date (1- (calendar-absolute-from-persian
                       (list month 1 year))))))
    (list month day year)))

;;;###autoload
(defun calendar-persian-date-string (&optional date)
  "String of Persian date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((persian-date (calendar-persian-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (y (extract-calendar-year persian-date))
         (m (extract-calendar-month persian-date)))
    (let ((monthname (aref persian-calendar-month-name-array (1- m)))
          (day (int-to-string (extract-calendar-day persian-date)))
          (dayname nil)
          (month (int-to-string m))
          (year (int-to-string y)))
      (mapconcat 'eval calendar-date-display-form ""))))

;;;###autoload
(defun calendar-print-persian-date ()
  "Show the Persian calendar equivalent of the selected date."
  (interactive)
  (message "Persian date: %s"
           (calendar-persian-date-string (calendar-cursor-to-date t))))

;;;###autoload
(defun calendar-goto-persian-date (date &optional noecho)
  "Move cursor to Persian date DATE.
Echo Persian date unless NOECHO is t."
  (interactive (persian-prompt-for-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-persian date)))
  (or noecho (calendar-print-persian-date)))

(defun persian-prompt-for-date ()
  "Ask for a Persian date."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                "Persian calendar year (not 0): "
                (lambda (x) (/= x 0))
                (int-to-string
                 (extract-calendar-year
                  (calendar-persian-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (completion-ignore-case t)
         (month (cdr (assoc
                       (completing-read
                        "Persian calendar month name: "
                        (mapcar 'list
                                (append persian-calendar-month-name-array nil))
                        nil t)
                      (calendar-make-alist persian-calendar-month-name-array
                                           1))))
         (last (persian-calendar-last-day-of-month month year))
         (day (calendar-read
               (format "Persian calendar day (1-%d): " last)
               (lambda (x) (and (< 0 x) (<= x last))))))
    (list (list month day year))))

(defun diary-persian-date ()
  "Persian calendar equivalent of date diary entry."
  (format "Persian date: %s" (calendar-persian-date-string date)))

(provide 'cal-persia)

;; Local Variables:
;; generated-autoload-file: "cal-loaddefs.el"
;; End:

;; arch-tag: 2832383c-e4b4-4dc2-8ee9-cfbdd53e5e2d
;;; cal-persia.el ends here
