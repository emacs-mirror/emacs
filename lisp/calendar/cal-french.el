;;; cal-french.el --- calendar functions for the French Revolutionary calendar.

;; Copyright (C) 1988, 1989, 1992 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: French Revolutionary calendar, calendar, diary

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;; This collection of functions implements the features of calendar.el and
;; diary.el that deal with the French Revolutionary calendar.

;; Technical details of the Mayan calendrical calculations can be found in
;; ``Calendrical Calculations, Part II: Three Historical Calendars''
;; by E. M. Reingold,  N. Dershowitz, and S. M. Clamen,
;; Software--Practice and Experience, Volume 23, Number 4 (April, 1993),
;; pages 383-404.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'calendar)

(defconst french-calendar-month-name-array
  ["Vende'miaire" "Brumaire" "Frimaire" "Nivo^se" "Pluvio^se" "Vento^se"
   "Germinal" "Flore'al" "Prairial" "Messidor" "Thermidor" "Fructidor"])

(defconst french-calendar-day-name-array
  ["Primidi" "Duodi" "Tridi" "Quartidi" "Quintidi" "Sextidi" "Septidi"
   "Octidi" "Nonidi" "Decadi"])

(defconst french-calendar-special-days-array
  ["de la Vertu" "du Genie" "du Labour" "de la Raison" "de la Recompense"
   "de la Revolution"])

(defun french-calendar-leap-year-p (year)
  "True if YEAR is a leap year on the French Revolutionary calendar.
For Gregorian years 1793 to 1805, the years of actual operation of the
calendar, uses historical practice based on equinoxes is followed (years 3, 7,
and 11 were leap years; 15 and 20 would have been leap years).  For later
years uses the proposed rule of Romme (never adopted)--leap years fall every
four years except century years not divisible 400 and century years that are
multiples of 4000."
  (or (memq year '(3 7 11));; Actual practice--based on equinoxes
      (memq year '(15 20)) ;; Anticipated practice--based on equinoxes
      (and (> year 20)     ;; Romme's proposal--never adopted
           (zerop (% year 4))
           (not (memq (% year 400) '(100 200 300)))
           (not (zerop (% year 4000))))))

(defun french-calendar-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the French Revolutionary calendar.
The 13th month is not really a month, but the 5 (6 in leap years) day period of
`sansculottides' at the end of the year."
  (if (< month 13)
      30
    (if (french-calendar-leap-year-p year)
        6
      5)))

(defun calendar-absolute-from-french (date)
  "Compute absolute date from French Revolutionary date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ (* 365 (1- year));; Days in prior years
       ;; Leap days in prior years
       (if (< year 20)
           (/ year 4);; Actual and anticipated practice (years 3, 7, 11, 15)
         ;; Romme's proposed rule (using the Principle of Inclusion/Exclusion)
         (+ (/ (1- year) 4);; Luckily, there were 4 leap years before year 20
            (- (/ (1- year) 100))
            (/ (1- year) 400)
            (- (/ (1- year) 4000))))
       (* 30 (1- month));; Days in prior months this year
       day;; Days so far this month
       654414)));; Days before start of calendar (September 22, 1792).

(defun calendar-french-from-absolute (date)
  "Compute the French Revolutionary equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the
(imaginary) Gregorian date Sunday, December 31, 1 BC."
  (if (< date 654415)
      (list 0 0 0);; pre-French Revolutionary date
    (let* ((approx (/ (- date 654414) 366));; Approximation from below.
           (year                ;; Search forward from the approximation.
            (+ approx
               (calendar-sum y approx
                 (>= date (calendar-absolute-from-french (list 1 1 (1+ y))))
                 1)))
           (month               ;; Search forward from Vendemiaire.
            (1+ (calendar-sum m 1
                  (> date
                     (calendar-absolute-from-french
                      (list m
                            (french-calendar-last-day-of-month m year)
                            year)))
                  1)))
           (day                   ;; Calculate the day by subtraction.
            (- date
               (1- (calendar-absolute-from-french (list month 1 year))))))
    (list month day year))))

(defun calendar-print-french-date ()
  "Show the French Revolutionary calendar equivalent of the selected date."
  (interactive)
  (let* ((french-date (calendar-french-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (y (extract-calendar-year french-date))
         (m (extract-calendar-month french-date))
         (d (extract-calendar-day french-date)))
    (if (< y 1)
        (message "Date is pre-French Revolution")
      (if (= m 13)
          (message "Jour %s de l'Anne'e %d de la Revolution"
                   (aref french-calendar-special-days-array (1- d))
                   y)
        (message "Decade %s, %s de %s de l'Anne'e %d de la Revolution"
                 (make-string (1+ (/ (1- d) 10)) ?I)
                 (aref french-calendar-day-name-array (% (1- d) 10))
                 (aref french-calendar-month-name-array (1- m))
                 y)))))

(defun calendar-goto-french-date (date &optional noecho)
  "Move cursor to French Revolutionary date DATE.
Echo French Revolutionary date unless NOECHO is t."
  (interactive
   (let* ((year (calendar-read
                 "Anne'e de la Revolution (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-french-from-absolute
                    (calendar-absolute-from-gregorian
                     (calendar-current-date)))))))
          (month-list
           (mapcar 'list
                   (append french-calendar-month-name-array
                           (if (french-calendar-leap-year-p year)
                               (mapcar
                                '(lambda (x) (concat "Jour " x))
                                french-calendar-special-days-array)
                              (cdr;; we don't want rev. day in a non-leap yr.
                               (nreverse
                                (mapcar
                                 '(lambda (x) (concat "Jour " x))
                                 french-calendar-special-days-array)))))))
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Mois ou Sansculottide: "
                         month-list
                         nil t))
                       (calendar-make-alist
                        month-list
                        1
                        '(lambda (x) (capitalize (car x)))))))
          (decade (if (> month 12)
                      1
                    (calendar-read
                     "De'cade (1-3): "
                     '(lambda (x) (memq x '(1 2 3))))))
          (day (if (> month 12)
                   (- month 12)
                 (calendar-read
                  "Jour (1-10)): "
                  '(lambda (x) (and (<= 1 x) (<= x 10))))))
          (month (if (> month 12) 13 month))
          (day (+ day (* 10 (1- decade)))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-french date)))
  (or noecho (calendar-print-french-date)))

(defun diary-french-date ()
  "French calendar equivalent of date diary entry."
  (let* ((french-date (calendar-french-from-absolute
                       (calendar-absolute-from-gregorian date)))
         (y (extract-calendar-year french-date))
         (m (extract-calendar-month french-date))
         (d (extract-calendar-day french-date)))
    (if (> y 0)
      (if (= m 13)
          (format "Jour %s de l'Anne'e %d de la Revolution"
                   (aref french-calendar-special-days-array (1- d))
                   y)
        (format "Decade %s, %s de %s de l'Anne'e %d de la Revolution"
                 (make-string (1+ (/ (1- d) 10)) ?I)
                 (aref french-calendar-day-name-array (% (1- d) 10))
                 (aref french-calendar-month-name-array (1- m))
                 y)))))

(provide 'cal-french)

;;; cal-french.el ends here
