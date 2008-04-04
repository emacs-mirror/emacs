;;; cal-hebrew.el --- calendar functions for the Hebrew calendar

;; Copyright (C) 1995, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008  Free Software Foundation, Inc.

;; Author: Nachum Dershowitz <nachum@cs.uiuc.edu>
;;         Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Hebrew calendar, calendar, diary

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

;; See calendar.el.

;;; Code:

(require 'calendar)

(defun hebrew-calendar-leap-year-p (year)
  "Non-nil if YEAR is a Hebrew calendar leap year."
  (< (% (1+ (* 7 year)) 19) 7))

(defun hebrew-calendar-last-month-of-year (year)
  "The last month of the Hebrew calendar YEAR."
  (if (hebrew-calendar-leap-year-p year)
      13
    12))

(defun hebrew-calendar-elapsed-days (year)
  "Days to mean conjunction of Tishri of Hebrew YEAR.
Measured from Sunday before start of Hebrew calendar."
  (let* ((months-elapsed
          (+ (* 235 (/ (1- year) 19)) ; months in complete cycles so far
             (* 12 (% (1- year) 19))  ; regular months in this cycle
             (/ (1+ (* 7 (% (1- year) 19))) 19))) ; leap months this cycle
         (parts-elapsed (+ 204 (* 793 (% months-elapsed 1080))))
         (hours-elapsed (+ 5
                           (* 12 months-elapsed)
                           (* 793 (/ months-elapsed 1080))
                           (/ parts-elapsed 1080)))
         (parts                         ; conjunction parts
          (+ (* 1080 (% hours-elapsed 24)) (% parts-elapsed 1080)))
         (day                           ; conjunction day
          (+ 1 (* 29 months-elapsed) (/ hours-elapsed 24)))
         (alternative-day
          (if (or (>= parts 19440) ; if the new moon is at or after midday
                  (and (= (% day 7) 2)  ; ...or is on a Tuesday...
                       (>= parts 9924) ; at 9 hours, 204 parts or later...
                       ;; of a common year...
                       (not (hebrew-calendar-leap-year-p year)))
                  (and (= (% day 7) 1)  ; ...or is on a Monday...
                       (>= parts 16789) ; at 15 hours, 589 parts or later...
                       ;; at the end of a leap year.
                       (hebrew-calendar-leap-year-p (1- year))))
              ;; Then postpone Rosh HaShanah one day.
              (1+ day)
            ;; Else:
            day)))
    ;; If Rosh HaShanah would occur on Sunday, Wednesday, or Friday
    (if (memq (% alternative-day 7) (list 0 3 5))
        ;; Then postpone it one (more) day and return.
        (1+ alternative-day)
      ;; Else return.
      alternative-day)))

(defun hebrew-calendar-days-in-year (year)
  "Number of days in Hebrew YEAR."
  (- (hebrew-calendar-elapsed-days (1+ year))
     (hebrew-calendar-elapsed-days year)))

(defun hebrew-calendar-long-heshvan-p (year)
  "Non-nil if Heshvan is long in Hebrew YEAR."
  (= (% (hebrew-calendar-days-in-year year) 10) 5))

(defun hebrew-calendar-short-kislev-p (year)
  "Non-nil if Kislev is short in Hebrew YEAR."
  (= (% (hebrew-calendar-days-in-year year) 10) 3))

(defun hebrew-calendar-last-day-of-month (month year)
  "The last day of MONTH in YEAR."
  (if (or (memq month (list 2 4 6 10 13))
          (and (= month 12) (not (hebrew-calendar-leap-year-p year)))
          (and (= month 8) (not (hebrew-calendar-long-heshvan-p year)))
          (and (= month 9) (hebrew-calendar-short-kislev-p year)))
      29
    30))

(defun calendar-absolute-from-hebrew (date)
  "Absolute date of Hebrew DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ day                              ; days so far this month
       (if (< month 7)                  ; before Tishri
           ;; Then add days in prior months this year before and after Nisan.
           (+ (calendar-sum
               m 7 (<= m (hebrew-calendar-last-month-of-year year))
               (hebrew-calendar-last-day-of-month m year))
              (calendar-sum
               m 1 (< m month)
               (hebrew-calendar-last-day-of-month m year)))
         ;; Else add days in prior months this year.
         (calendar-sum
          m 7 (< m month)
          (hebrew-calendar-last-day-of-month m year)))
       (hebrew-calendar-elapsed-days year) ; days in prior years
       -1373429)))               ; days elapsed before absolute date 1

(defun calendar-hebrew-from-absolute (date)
  "Compute the Hebrew date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((greg-date (calendar-gregorian-from-absolute date))
         (year (+ 3760 (extract-calendar-year greg-date)))
         (month (aref [9 10 11 12 1 2 3 4 7 7 7 8]
                      (1- (extract-calendar-month greg-date))))
         (length (progn
                   (while (>= date (calendar-absolute-from-hebrew
                                    (list 7 1 (1+ year))))
                     (setq year (1+ year)))
                   (hebrew-calendar-last-month-of-year year)))
         day)
    (while (> date
              (calendar-absolute-from-hebrew
               (list month
                     (hebrew-calendar-last-day-of-month month year)
                     year)))
      (setq month (1+ (% month length))))
    (setq day (1+
               (- date (calendar-absolute-from-hebrew (list month 1 year)))))
    (list month day year)))

(defconst calendar-hebrew-month-name-array-common-year
  ["Nisan" "Iyar" "Sivan" "Tammuz" "Av" "Elul" "Tishri"
   "Heshvan" "Kislev" "Teveth" "Shevat" "Adar"]
  "Array of strings giving the names of the Hebrew months in a common year.")

(defconst calendar-hebrew-month-name-array-leap-year
  ["Nisan" "Iyar" "Sivan" "Tammuz" "Av" "Elul" "Tishri"
   "Heshvan" "Kislev" "Teveth" "Shevat" "Adar I" "Adar II"]
  "Array of strings giving the names of the Hebrew months in a leap year.")

;;;###cal-autoload
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

;;;###cal-autoload
(defun calendar-print-hebrew-date ()
  "Show the Hebrew calendar equivalent of the date under the cursor."
  (interactive)
  (message "Hebrew date (until sunset): %s"
           (calendar-hebrew-date-string (calendar-cursor-to-date t))))

(defun hebrew-calendar-yahrzeit (death-date year)
  "Absolute date of the anniversary of Hebrew DEATH-DATE in Hebrew YEAR."
  (let ((death-day (extract-calendar-day death-date))
        (death-month (extract-calendar-month death-date))
        (death-year (extract-calendar-year death-date)))
    (cond
     ;; If it's Heshvan 30 it depends on the first anniversary; if
     ;; that was not Heshvan 30, use the day before Kislev 1.
     ((and (= death-month 8)
           (= death-day 30)
           (not (hebrew-calendar-long-heshvan-p (1+ death-year))))
      (1- (calendar-absolute-from-hebrew (list 9 1 year))))
     ;; If it's Kislev 30 it depends on the first anniversary; if that
     ;; was not Kislev 30, use the day before Teveth 1.
     ((and (= death-month 9)
           (= death-day 30)
           (hebrew-calendar-short-kislev-p (1+ death-year)))
      (1- (calendar-absolute-from-hebrew (list 10 1 year))))
     ;; If it's Adar II, use the same day in last month of year (Adar
     ;; or Adar II).
     ((= death-month 13)
      (calendar-absolute-from-hebrew
       (list (hebrew-calendar-last-month-of-year year) death-day year)))
     ;; If it's the 30th in Adar I and year is not a leap year (so
     ;; Adar has only 29 days), use the last day in Shevat.
     ((and (= death-day 30)
           (= death-month 12)
           (not (hebrew-calendar-leap-year-p year)))
      (calendar-absolute-from-hebrew (list 11 30 year)))
     ;; In all other cases, use the normal anniversary of the date of death.
     (t (calendar-absolute-from-hebrew
         (list death-month death-day year))))))

(defun calendar-hebrew-read-date ()
  "Interactively read the arguments for a Hebrew date command.
Reads a year, month, and day."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                "Hebrew calendar year (>3760): "
                (lambda (x) (> x 3760))
                (int-to-string
                 (extract-calendar-year
                  (calendar-hebrew-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (month-array (if (hebrew-calendar-leap-year-p year)
                          calendar-hebrew-month-name-array-leap-year
                        calendar-hebrew-month-name-array-common-year))
         (completion-ignore-case t)
         (month (cdr (assoc-string
                      (completing-read
                       "Hebrew calendar month name: "
                       (mapcar 'list (append month-array nil))
                       (if (= year 3761)
                           (lambda (x)
                             (let ((m (cdr
                                       (assoc-string
                                        (car x)
                                        (calendar-make-alist month-array)
                                        t))))
                               (< 0
                                  (calendar-absolute-from-hebrew
                                   (list m
                                         (hebrew-calendar-last-day-of-month
                                          m year)
                                         year))))))
                       t)
                      (calendar-make-alist month-array 1) t)))
         (last (hebrew-calendar-last-day-of-month month year))
         (first (if (and (= year 3761) (= month 10))
                    18 1))
         (day (calendar-read
               (format "Hebrew calendar day (%d-%d): "
                       first last)
               (lambda (x) (and (<= first x) (<= x last))))))
    (list (list month day year))))

;;;###cal-autoload
(defun calendar-goto-hebrew-date (date &optional noecho)
  "Move cursor to Hebrew DATE; echo Hebrew date unless NOECHO is non-nil."
  (interactive (calendar-hebrew-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-hebrew date)))
  (or noecho (calendar-print-hebrew-date)))

(defvar displayed-month)                ; from generate-calendar

(defun calendar-hebrew-date-is-visible-p (month day)
  "Return non-nil if Hebrew MONTH DAY is visible in the calendar window.
Returns the corresponding Gregorian date."
  ;; This test is only to speed things up a bit; it works fine without it.
  (if (memq displayed-month
            ;; What this is doing is equivalent to +1,2,3,4,5 modulo 12, ie:
            ;;  (mapcar (lambda (n) (let ((x (mod n 12)))
            ;;                        (if (zerop x) 12
            ;;                          x)))
            ;;          (number-sequence (1+ month) (+ 5 month)))
            ;; Ie it makes a list:
            ;;  2  3  4  5  6 when month = 1
            ;;  3  4  5  6  7 when month = 2
            ;; ...
            ;;  8  9 10 11 12 when month = 7
            ;;  9 10 11 12  1 when month = 8
            ;; ...
            ;; 12  1  2  3  4 when month = 11
            ;;  1  2  3  4  5 when month = 12
            ;; This implies that hebrew month N cannot occur outside
            ;; Gregorian months N:N+6 (the calendar shows
            ;; displayed-month +/- 1 at any time).
            ;; So to put it another way:
            ;;  (calendar-interval month 1 displayed-month
            ;;                    (if (> month displayed-month) 2 1))
            ;; must be >= 1 and <= 5.  This could be expanded to:
            ;;  (if (> month displayed-month) (+ 12 (- displayed-month month))
            ;;    (- displayed-month month)
            (list
             (if (< 11 month) (- month 11) (+ month 1))
             (if (< 10 month) (- month 10) (+ month 2))
             (if (<  9 month) (- month  9) (+ month 3))
             (if (<  8 month) (- month  8) (+ month 4))
             (if (<  7 month) (- month  7) (+ month 5))))
      (calendar-nongregorian-visible-p
       month day 'calendar-absolute-from-hebrew
       'calendar-hebrew-from-absolute
       ;; Hebrew new year is start of month 7.
       ;; If hmonth >= 7, choose the higher year.
       (lambda (m) (> m 6)))))

;;;###holiday-autoload
(defun holiday-hebrew (month day string)
  "Holiday on MONTH, DAY (Hebrew) called STRING.
If MONTH, DAY (Hebrew) is visible, the value returned is corresponding
Gregorian date in the form of the list (((month day year) STRING)).  Returns
nil if it is not visible in the current calendar window."
  (let ((gdate (calendar-hebrew-date-is-visible-p month day)))
    (if gdate (list (list gdate string)))))

;; h-r-h-e should be called from holidays code.
(declare-function holiday-filter-visible-calendar "holidays" (l))

(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-rosh-hashanah-etc (&optional all)
  "List of dates related to Rosh Hashanah, as visible in calendar window.
Shows only the major holidays, unless `calendar-hebrew-all-holidays-flag'
or ALL is non-nil."
  (when (memq displayed-month '(8 9 10 11))
    (let ((abs-r-h (calendar-absolute-from-hebrew
                    (list 7 1 (+ displayed-year 3761)))))
      (holiday-filter-visible-calendar
       (append
        (list
         (list (calendar-gregorian-from-absolute abs-r-h)
               (format "Rosh HaShanah %d" (+ 3761 displayed-year)))
         (list (calendar-gregorian-from-absolute (+ abs-r-h 9))
               "Yom Kippur")
         (list (calendar-gregorian-from-absolute (+ abs-r-h 14))
               "Sukkot")
         (list (calendar-gregorian-from-absolute (+ abs-r-h 21))
               "Shemini Atzeret")
         (list (calendar-gregorian-from-absolute (+ abs-r-h 22))
               "Simchat Torah"))
        (when (or all calendar-hebrew-all-holidays-flag)
          (list
           (list (calendar-gregorian-from-absolute
                  (calendar-dayname-on-or-before 6 (- abs-r-h 4)))
                 "Selichot (night)")
           (list (calendar-gregorian-from-absolute (1- abs-r-h))
                 "Erev Rosh HaShanah")
           (list (calendar-gregorian-from-absolute (1+ abs-r-h))
                 "Rosh HaShanah (second day)")
           (list (calendar-gregorian-from-absolute
                  (if (= (% abs-r-h 7) 4) (+ abs-r-h 3) (+ abs-r-h 2)))
                 "Tzom Gedaliah")
           (list (calendar-gregorian-from-absolute
                  (calendar-dayname-on-or-before 6 (+ 7 abs-r-h)))
                 "Shabbat Shuvah")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 8))
                 "Erev Yom Kippur")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 13))
                 "Erev Sukkot")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 15))
                 "Sukkot (second day)")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 16))
                 "Hol Hamoed Sukkot (first day)")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 17))
                 "Hol Hamoed Sukkot (second day)")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 18))
                 "Hol Hamoed Sukkot (third day)")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 19))
                 "Hol Hamoed Sukkot (fourth day)")
           (list (calendar-gregorian-from-absolute (+ abs-r-h 20))
                   "Hoshanah Rabbah"))))))))

;;;###holiday-autoload
(defun holiday-hanukkah (&optional all)
  "List of dates related to Hanukkah, as visible in calendar window.
Shows only Hanukkah, unless `calendar-hebrew-all-holidays-flag' or ALL
is non-nil."
  ;; This test is only to speed things up a bit, it works fine without it.
  (when (memq displayed-month '(10 11 12 1 2))
    (let* ((m displayed-month)
           (y displayed-year)
           (h-y (progn
                  (increment-calendar-month m y 1)
                  (extract-calendar-year
                   (calendar-hebrew-from-absolute
                    (calendar-absolute-from-gregorian
                     (list m (calendar-last-day-of-month m y) y))))))
           (abs-h (calendar-absolute-from-hebrew (list 9 25 h-y)))
           (ord ["first" "second" "third" "fourth" "fifth" "sixth"
                 "seventh" "eighth"])
           han)
      (holiday-filter-visible-calendar
       (if (or all calendar-hebrew-all-holidays-flag)
           (append
            (list
             (list (calendar-gregorian-from-absolute (1- abs-h))
                   "Erev Hanukkah"))
            (dotimes (i 8 (nreverse han))
              (push (list
                     (calendar-gregorian-from-absolute (+ abs-h i))
                     (format "Hanukkah (%s day)" (aref ord i)))
                    han)))
         (list (list (calendar-gregorian-from-absolute abs-h) "Hanukkah")))))))

;;;###holiday-autoload
(defun holiday-passover-etc (&optional all)
  "List of dates related to Passover, as visible in calendar window.
Shows only the major holidays, unless `calendar-hebrew-all-holidays-flag'
or ALL is non-nil."
  (when (< displayed-month 8)
    (let ((abs-p (calendar-absolute-from-hebrew
                  (list 1 15 (+ displayed-year 3760)))))
      (holiday-filter-visible-calendar
       ;; The first two are out of order when the others are added.
       (append
        (list
         (list (calendar-gregorian-from-absolute abs-p) "Passover")
         (list (calendar-gregorian-from-absolute (+ abs-p 50))
                    "Shavuot"))
        (when (or all calendar-hebrew-all-holidays-flag)
          (list
           (list (calendar-gregorian-from-absolute
                  (calendar-dayname-on-or-before 6 (- abs-p 43)))
                 "Shabbat Shekalim")
           (list (calendar-gregorian-from-absolute
                  (calendar-dayname-on-or-before 6 (- abs-p 30)))
                 "Shabbat Zachor")
           (list (calendar-gregorian-from-absolute
                  (if (= (% abs-p 7) 2) (- abs-p 33) (- abs-p 31)))
                 "Fast of Esther")
           (list (calendar-gregorian-from-absolute (- abs-p 31))
                 "Erev Purim")
           (list (calendar-gregorian-from-absolute (- abs-p 30))
                 "Purim")
           (list (calendar-gregorian-from-absolute
                  (if (zerop (% abs-p 7)) (- abs-p 28) (- abs-p 29)))
                 "Shushan Purim")
           (list (calendar-gregorian-from-absolute
                  (- (calendar-dayname-on-or-before 6 (- abs-p 14)) 7))
                 "Shabbat Parah")
           (list (calendar-gregorian-from-absolute
                  (calendar-dayname-on-or-before 6 (- abs-p 14)))
                 "Shabbat HaHodesh")
           (list (calendar-gregorian-from-absolute
                  (calendar-dayname-on-or-before 6 (1- abs-p)))
                 "Shabbat HaGadol")
           (list (calendar-gregorian-from-absolute (1- abs-p))
                 "Erev Passover")
           (list (calendar-gregorian-from-absolute (1+ abs-p))
                 "Passover (second day)")
           (list (calendar-gregorian-from-absolute (+ abs-p 2))
                 "Hol Hamoed Passover (first day)")
           (list (calendar-gregorian-from-absolute (+ abs-p 3))
                 "Hol Hamoed Passover (second day)")
           (list (calendar-gregorian-from-absolute (+ abs-p 4))
                 "Hol Hamoed Passover (third day)")
           (list (calendar-gregorian-from-absolute (+ abs-p 5))
                 "Hol Hamoed Passover (fourth day)")
           (list (calendar-gregorian-from-absolute (+ abs-p 6))
                 "Passover (seventh day)")
           (list (calendar-gregorian-from-absolute (+ abs-p 7))
                 "Passover (eighth day)")
           (list (calendar-gregorian-from-absolute
                  (if (zerop (% (+ abs-p 12) 7))
                      (+ abs-p 13)
                    (+ abs-p 12)))
                 "Yom HaShoah")
           (list (calendar-gregorian-from-absolute
                  (if (zerop (% abs-p 7))
                      (+ abs-p 18)
                    (if (= (% abs-p 7) 6)
                        (+ abs-p 19)
                      (+ abs-p 20))))
                 "Yom HaAtzma'ut")
           (list (calendar-gregorian-from-absolute (+ abs-p 33))
                 "Lag BaOmer")
           (list (calendar-gregorian-from-absolute (+ abs-p 43))
                 "Yom Yerushalaim")
           (list (calendar-gregorian-from-absolute (+ abs-p 49))
                 "Erev Shavuot")
           (list (calendar-gregorian-from-absolute (+ abs-p 51))
                 "Shavuot (second day)"))))))))

;;;###holiday-autoload
(defun holiday-tisha-b-av-etc ()
  "List of dates around Tisha B'Av, as visible in calendar window."
  (when (memq displayed-month '(5 6 7 8 9))
    (let ((abs-t-a (calendar-absolute-from-hebrew
                    (list 5 9 (+ displayed-year 3760)))))
      (holiday-filter-visible-calendar
       (list
        (list (calendar-gregorian-from-absolute
               (if (= (% abs-t-a 7) 6) (- abs-t-a 20) (- abs-t-a 21)))
              "Tzom Tammuz")
        (list (calendar-gregorian-from-absolute
               (calendar-dayname-on-or-before 6 abs-t-a))
              "Shabbat Hazon")
        (list (calendar-gregorian-from-absolute
               (if (= (% abs-t-a 7) 6) (1+ abs-t-a) abs-t-a))
              "Tisha B'Av")
        (list (calendar-gregorian-from-absolute
               (calendar-dayname-on-or-before 6 (+ abs-t-a 7)))
              "Shabbat Nahamu"))))))

(autoload 'holiday-julian "cal-julian")

;;;###holiday-autoload
(defun holiday-hebrew-misc ()
  "Miscellaneous Hebrew holidays, if visible in calendar window.
Includes: Tal Umatar, Tzom Teveth, Tu B'Shevat, Shabbat Shirah, and
Kiddush HaHamah."
  (let ((m displayed-month)
        (y displayed-year)
        year h-year s-s)
    (append
     (holiday-julian
      11
      (progn
        (increment-calendar-month m y -1)
        (setq year (extract-calendar-year
                    (calendar-julian-from-absolute
                     (calendar-absolute-from-gregorian (list m 1 y)))))
        (if (zerop (% (1+ year) 4))
            22
          21)) "\"Tal Umatar\" (evening)")
     (holiday-hebrew
      10
      (progn
        (setq h-year (extract-calendar-year
                      (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (list displayed-month 28 displayed-year)))))
        (if (= 6 (% (calendar-absolute-from-hebrew (list 10 10 h-year))
                    7))
            11 10))
      "Tzom Teveth")
     (holiday-hebrew 11 15 "Tu B'Shevat")
     (holiday-hebrew
      11
      (progn
        (setq m displayed-month
              y displayed-year
              h-year (progn
                       (increment-calendar-month m y 1)
                       (extract-calendar-year
                        (calendar-hebrew-from-absolute
                         (calendar-absolute-from-gregorian
                          (list m (calendar-last-day-of-month m y) y)))))
              s-s
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
        (extract-calendar-day s-s))
      "Shabbat Shirah")
     (and (progn
            (setq m displayed-month
                  y displayed-year
                  year (progn
                         (increment-calendar-month m y -1)
                         (extract-calendar-year
                          (calendar-julian-from-absolute
                           (calendar-absolute-from-gregorian (list m 1 y))))))
            (= 21 (% year 28)))
          (holiday-julian 3 26 "Kiddush HaHamah")))))


(autoload 'diary-list-entries-1 "diary-lib")

;;;###diary-autoload
(defun list-hebrew-diary-entries ()
  "Add any Hebrew date entries from the diary file to `diary-entries-list'.
Hebrew date diary entries must be prefaced by `hebrew-diary-entry-symbol'
\(normally an `H').  The same diary date forms govern the style
of the Hebrew calendar entries, except that the Hebrew month
names cannot be abbreviated.  The Hebrew months are numbered
from 1 to 13 with Nisan being 1, 12 being Adar I and 13 being
Adar II; you must use `Adar I' if you want Adar of a common
Hebrew year.  If a Hebrew date diary entry begins with
`diary-nonmarking-symbol', the entry will appear in the diary
listing, but will not be marked in the calendar.  This function
is provided for use with `nongregorian-diary-listing-hook'."
  (diary-list-entries-1 calendar-hebrew-month-name-array-leap-year
                        hebrew-diary-entry-symbol
                        'calendar-hebrew-from-absolute))

(autoload 'calendar-mark-complex "diary-lib")

;;;###diary-autoload
(defun mark-hebrew-calendar-date-pattern (month day year &optional color)
  "Mark dates in calendar window that conform to Hebrew date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `mark-visible-calendar-date' as MARK."
  ;; FIXME not the same as the Bahai and Islamic cases, so can't use
  ;; calendar-mark-1.
  (save-excursion
    (set-buffer calendar-buffer)
    (if (and (not (zerop month)) (not (zerop day)))
        (if (not (zerop year))
            ;; Fully specified Hebrew date.
            (let ((date (calendar-gregorian-from-absolute
                         (calendar-absolute-from-hebrew
                          (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (mark-visible-calendar-date date color)))
          ;; Month and day in any year.
          (let ((gdate (calendar-hebrew-date-is-visible-p month day)))
            (if gdate (mark-visible-calendar-date gdate color))))
      (calendar-mark-complex month day year
                             'calendar-hebrew-from-absolute color))))

(autoload 'diary-mark-entries-1 "diary-lib")

;;;###diary-autoload
(defun mark-hebrew-diary-entries ()
  "Mark days in the calendar window that have Hebrew date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `list-hebrew-diary-entries' for more information."
  (diary-mark-entries-1 'mark-hebrew-calendar-date-pattern
                        calendar-hebrew-month-name-array-leap-year
                        hebrew-diary-entry-symbol
                        'calendar-hebrew-from-absolute))


(autoload 'diary-insert-entry-1 "diary-lib")

;;;###cal-autoload
(defun insert-hebrew-diary-entry (arg)
  "Insert a diary entry.
For the Hebrew date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 nil arg calendar-hebrew-month-name-array-leap-year
                        hebrew-diary-entry-symbol
                        'calendar-hebrew-from-absolute))

;;;###cal-autoload
(defun insert-monthly-hebrew-diary-entry (arg)
  "Insert a monthly diary entry.
For the day of the Hebrew month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'monthly arg calendar-hebrew-month-name-array-leap-year
                        hebrew-diary-entry-symbol
                        'calendar-hebrew-from-absolute))

;;;###cal-autoload
(defun insert-yearly-hebrew-diary-entry (arg)
  "Insert an annual diary entry.
For the day of the Hebrew year corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'yearly arg calendar-hebrew-month-name-array-leap-year
                        hebrew-diary-entry-symbol
                        'calendar-hebrew-from-absolute))

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
                           (lambda (x) (> x 0))
                           (int-to-string (extract-calendar-year today))))
                    (month-array calendar-month-name-array)
                    (completion-ignore-case t)
                    (month (cdr (assoc-string
                                 (completing-read
                                  "Month of death (name): "
                                  (mapcar 'list (append month-array nil))
                                  nil t)
                                 (calendar-make-alist month-array 1) t)))
                    (last (calendar-last-day-of-month month year))
                    (day (calendar-read
                          (format "Day of death (1-%d): " last)
                          (lambda (x) (and (< 0 x) (<= x last))))))
               (list month day year))))
          (death-year (extract-calendar-year death-date))
          (start-year (calendar-read
                       (format "Starting year of Yahrzeit table (>%d): "
                               death-year)
                       (lambda (x) (> x death-year))
                       (int-to-string (1+ death-year))))
          (end-year (calendar-read
                     (format "Ending year of Yahrzeit table (>=%d): "
                             start-year)
                     (lambda (x) (>= x start-year)))))
     (list death-date start-year end-year)))
  (message "Computing Yahrzeits...")
  (let* ((h-date (calendar-hebrew-from-absolute
                  (calendar-absolute-from-gregorian death-date)))
         (h-month (extract-calendar-month h-date))
         (h-day (extract-calendar-day h-date))
         (h-year (extract-calendar-year h-date))
         (i (1- start-year)))
    (calendar-in-read-only-buffer cal-hebrew-yahrzeit-buffer
      (calendar-set-mode-line
       (format "Yahrzeit dates for %s = %s"
               (calendar-date-string death-date)
               (let ((calendar-month-name-array
                      (if (hebrew-calendar-leap-year-p h-year)
                          calendar-hebrew-month-name-array-leap-year
                        calendar-hebrew-month-name-array-common-year)))
                 (calendar-date-string h-date nil t))))
      (while (<= (setq i (1+ i)) end-year)
        (insert
         (calendar-date-string
          (calendar-gregorian-from-absolute
           (hebrew-calendar-yahrzeit
            h-date
            (extract-calendar-year
             (calendar-hebrew-from-absolute
              (calendar-absolute-from-gregorian (list 1 1 i))))))) "\n"))))
  (message "Computing Yahrzeits...done"))

(defvar date)

;; To be called from list-sexp-diary-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-hebrew-date ()
  "Hebrew calendar equivalent of date diary entry."
  (format "Hebrew date (until sunset): %s" (calendar-hebrew-date-string date)))

;;;###diary-autoload
(defun diary-omer (&optional mark)
  "Omer count diary entry.
Entry applies if date is within 50 days after Passover.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((passover
          (calendar-absolute-from-hebrew
           (list 1 15 (+ (extract-calendar-year date) 3760))))
         (omer (- (calendar-absolute-from-gregorian date) passover))
         (week (/ omer 7))
         (day (% omer 7)))
    (if (and (> omer 0) (< omer 50))
        (cons mark
              (format "Day %d%s of the omer (until sunset)"
                      omer
                      (if (zerop week)
                          ""
                        (format ", that is, %d week%s%s"
                                week
                                (if (= week 1) "" "s")
                                (if (zerop day)
                                    ""
                                  (format " and %d day%s"
                                          day (if (= day 1) "" "s"))))))))))

(defvar entry)

(autoload 'diary-make-date "diary-lib")

;;;###diary-autoload
(defun diary-yahrzeit (death-month death-day death-year &optional mark)
  "Yahrzeit diary entry--entry applies if date is Yahrzeit or the day before.
Parameters are DEATH-MONTH, DEATH-DAY, DEATH-YEAR; the diary
entry is assumed to be the name of the person.  Although the date
of death is specified by the civil calendar, the proper Hebrew
calendar Yahrzeit is determined.

The order of the input parameters changes according to `calendar-date-style'
\(e.g. to DEATH-DAY, DEATH-MONTH, DEATH-YEAR in the European style).

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((h-date (calendar-hebrew-from-absolute
                  (calendar-absolute-from-gregorian
                   (diary-make-date death-month death-day death-year))))
         (h-month (extract-calendar-month h-date))
         (h-day (extract-calendar-day h-date))
         (h-year (extract-calendar-year h-date))
         (d (calendar-absolute-from-gregorian date))
         (yr (extract-calendar-year (calendar-hebrew-from-absolute d)))
         (diff (- yr h-year))
         (y (hebrew-calendar-yahrzeit h-date yr)))
    (if (and (> diff 0) (or (= y d) (= y (1+ d))))
        (cons mark
              (format "Yahrzeit of %s%s: %d%s anniversary"
                      entry
                      (if (= y d) "" " (evening)")
                      diff
                      (cond ((= (% diff 10) 1) "st")
                            ((= (% diff 10) 2) "nd")
                            ((= (% diff 10) 3) "rd")
                            (t "th")))))))

;;;###diary-autoload
(defun diary-rosh-hodesh (&optional mark)
  "Rosh Hodesh diary entry.
Entry applies if date is Rosh Hodesh, the day before, or the Saturday before.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((d (calendar-absolute-from-gregorian date))
         (h-date (calendar-hebrew-from-absolute d))
         (h-month (extract-calendar-month h-date))
         (h-day (extract-calendar-day h-date))
         (h-year (extract-calendar-year h-date))
         (leap-year (hebrew-calendar-leap-year-p h-year))
         (last-day (hebrew-calendar-last-day-of-month h-month h-year))
         (h-month-names
          (if leap-year
              calendar-hebrew-month-name-array-leap-year
            calendar-hebrew-month-name-array-common-year))
         (this-month (aref h-month-names (1- h-month)))
         (h-yesterday (extract-calendar-day
                       (calendar-hebrew-from-absolute (1- d)))))
    (if (or (= h-day 30) (and (= h-day 1) (/= h-month 7)))
        (cons mark
              (format
               "Rosh Hodesh %s"
               (if (= h-day 30)
                   (format
                    "%s (first day)"
                    ;; Next month must be in the same year since this
                    ;; month can't be the last month of the year since
                    ;; it has 30 days
                    (aref h-month-names h-month))
                 (if (= h-yesterday 30)
                     (format "%s (second day)" this-month)
                   this-month))))
      (if (= (% d 7) 6)        ; Saturday--check for Shabbat Mevarchim
          (cons mark
                (cond ((and (> h-day 22) (/= h-month 6) (= 29 last-day))
                       (format "Mevarchim Rosh Hodesh %s (%s)"
                               (aref h-month-names
                                     (if (= h-month
                                            (hebrew-calendar-last-month-of-year
                                             h-year))
                                         0 h-month))
                               (aref calendar-day-name-array (- 29 h-day))))
                      ((and (< h-day 30) (> h-day 22) (= 30 last-day))
                       (format "Mevarchim Rosh Hodesh %s (%s-%s)"
                               (aref h-month-names h-month)
                               (if (= h-day 29)
                                   "tomorrow"
                                 (aref calendar-day-name-array (- 29 h-day)))
                               (aref calendar-day-name-array
                                     (% (- 30 h-day) 7))))))
        (if (and (= h-day 29) (/= h-month 6))
            (cons mark
                  (format "Erev Rosh Hodesh %s"
                          (aref h-month-names
                                (if (= h-month
                                       (hebrew-calendar-last-month-of-year
                                        h-year))
                                    0 h-month)))))))))

(defconst hebrew-calendar-parashiot-names
  ["Bereshith"   "Noah"      "Lech L'cha" "Vayera"    "Hayei Sarah" "Toledoth"
   "Vayetze"     "Vayishlah" "Vayeshev"   "Mikketz"   "Vayiggash"   "Vayhi"
   "Shemoth"     "Vaera"     "Bo"         "Beshallah" "Yithro"      "Mishpatim"
   "Terumah"     "Tetzavveh" "Ki Tissa"   "Vayakhel"  "Pekudei"     "Vayikra"
   "Tzav"        "Shemini"   "Tazria"     "Metzora"   "Aharei Moth" "Kedoshim"
   "Emor"        "Behar"     "Behukkotai" "Bemidbar"  "Naso"      "Behaalot'cha"
   "Shelah L'cha" "Korah"    "Hukkath"    "Balak"     "Pinhas"      "Mattoth"
   "Masei"       "Devarim"   "Vaethanan"  "Ekev"      "Reeh"        "Shofetim"
   "Ki Tetze"    "Ki Tavo"   "Nitzavim"   "Vayelech"  "Haazinu"]
  "The names of the parashiot in the Torah.")

(defun hebrew-calendar-parasha-name (p)
  "Name(s) corresponding to parasha P."
  (if (arrayp p)                        ; combined parasha
      (format "%s/%s"
              (aref hebrew-calendar-parashiot-names (aref p 0))
              (aref hebrew-calendar-parashiot-names (aref p 1)))
    (aref hebrew-calendar-parashiot-names p)))

;; Following 14 constants are used in diary-parasha (intern).

;; The seven ordinary year types (keviot).
(defconst hebrew-calendar-year-Saturday-incomplete-Sunday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
       23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
       43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year starts on Saturday, is `incomplete' (Heshvan and Kislev each have
29 days), and has Passover start on Sunday.")

(defconst hebrew-calendar-year-Saturday-complete-Tuesday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
       23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
       43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Saturday, is `complete' (Heshvan and Kislev each
have 30 days), and has Passover start on Tuesday.")

(defconst hebrew-calendar-year-Monday-incomplete-Tuesday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
      23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
      43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `incomplete' (Heshvan and Kislev each
have 29 days), and has Passover start on Tuesday.")

(defconst hebrew-calendar-year-Monday-complete-Thursday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
      23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 (nil . 34) (34 . 35) (35 . 36)
      (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `complete' (Heshvan and Kislev each have
30 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Tuesday-regular-Thursday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
      23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 (nil . 34) (34 . 35) (35 . 36)
      (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Tuesday, is `regular' (Heshvan has 29 days and
Kislev has 30 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Thursday-regular-Saturday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22] 23
      24 nil (nil . 25) (25 . [26 27]) ([26 27] . [28 29]) ([28 29] . 30)
      (30 . 31) ([31 32] . 32) 33 34 35 36 37 38 39 40 [41 42] 43 44 45 46 47 48
      49 50]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `regular' (Heshvan has 29 days and
Kislev has 30 days), and has Passover start on Saturday.")

(defconst hebrew-calendar-year-Thursday-complete-Sunday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
      23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
      43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `complete' (Heshvan and Kislev each
have 30 days), and has Passover start on Sunday.")

;; The seven leap year types (keviot).
(defconst hebrew-calendar-year-Saturday-incomplete-Tuesday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
       23 24 25 26 27 nil 28 29 30 31 32 33 34 35 36 37 38 39 40 [41 42]
       43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Saturday, is `incomplete' (Heshvan and Kislev each
have 29 days), and has Passover start on Tuesday.")

(defconst hebrew-calendar-year-Saturday-complete-Thursday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
       23 24 25 26 27 nil 28 29 30 31 32 33 (nil . 34) (34 . 35) (35 . 36)
       (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Saturday, is `complete' (Heshvan and Kislev each
have 30 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Monday-incomplete-Thursday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
      23 24 25 26 27 nil 28 29 30 31 32 33 (nil . 34) (34 . 35) (35 . 36)
      (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `incomplete' (Heshvan and Kislev each
have 29 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Monday-complete-Saturday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
      23 24 25 26 27 nil (nil . 28) (28 . 29) (29 . 30) (30 . 31) (31 . 32)
      (32 . 33) (33 . 34) (34 . 35) (35 . 36) (36 . 37) (37 . 38) (38 . 39)
      (39 . 40) (40 . 41) ([41 42] . 42) 43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `complete' (Heshvan and Kislev each have
30 days), and has Passover start on Saturday.")

(defconst hebrew-calendar-year-Tuesday-regular-Saturday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
      23 24 25 26 27 nil (nil . 28) (28 . 29) (29 . 30) (30 . 31) (31 . 32)
      (32 . 33) (33 . 34) (34 . 35) (35 . 36) (36 . 37) (37 . 38) (38 . 39)
      (39 . 40) (40 . 41) ([41 42] . 42) 43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Tuesday, is `regular' (Heshvan has 29 days and
Kislev has 30 days), and has Passover start on Saturday.")

(defconst hebrew-calendar-year-Thursday-incomplete-Sunday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
      23 24 25 26 27 28 nil 29 30 31 32 33 34 35 36 37 38 39 40 41 42
      43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `incomplete' (Heshvan and Kislev both
have 29 days), and has Passover start on Sunday.")

(defconst hebrew-calendar-year-Thursday-complete-Tuesday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
      23 24 25 26 27 28 nil 29 30 31 32 33 34 35 36 37 38 39 40 41 42
      43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `complete' (Heshvan and Kislev both
have 30 days), and has Passover start on Tuesday.")

;;;###diary-autoload
(defun diary-parasha (&optional mark)
  "Parasha diary entry--entry applies if date is a Saturday.
An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let ((d (calendar-absolute-from-gregorian date)))
    (if (= (% d 7) 6)                   ; Saturday
        (let* ((h-year (extract-calendar-year
                        (calendar-hebrew-from-absolute d)))
               (rosh-hashanah
                (calendar-absolute-from-hebrew (list 7 1 h-year)))
               (passover
                (calendar-absolute-from-hebrew (list 1 15 h-year)))
               (rosh-hashanah-day
                (aref calendar-day-name-array (% rosh-hashanah 7)))
               (passover-day
                (aref calendar-day-name-array (% passover 7)))
               (long-h (hebrew-calendar-long-heshvan-p h-year))
               (short-k (hebrew-calendar-short-kislev-p h-year))
               (type (cond ((and long-h (not short-k)) "complete")
                           ((and (not long-h) short-k) "incomplete")
                           (t "regular")))
               (year-format
                (symbol-value
                 (intern (format "hebrew-calendar-year-%s-%s-%s" ; keviah
                                 rosh-hashanah-day type passover-day))))
               (first-saturday            ; of Hebrew year
                (calendar-dayname-on-or-before 6 (+ 6 rosh-hashanah)))
               (saturday             ; which Saturday of the Hebrew year
                (/ (- d first-saturday) 7))
               (parasha (aref year-format saturday)))
          (if parasha
              (cons mark
                    (format
                     "Parashat %s"
                     (if (listp parasha) ; Israel differs from diaspora
                         (if (car parasha)
                             (format "%s (diaspora), %s (Israel)"
                                     (hebrew-calendar-parasha-name
                                      (car parasha))
                                     (hebrew-calendar-parasha-name
                                      (cdr parasha)))
                           (format "%s (Israel)"
                                   (hebrew-calendar-parasha-name
                                    (cdr parasha))))
                       (hebrew-calendar-parasha-name parasha)))))))))

(provide 'cal-hebrew)

;; arch-tag: aaab6718-7712-42ac-a32d-28fe1f944f3c
;;; cal-hebrew.el ends here
