;;; cal-chinese.el --- calendar functions for the Chinese calendar.

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: Chinese calendar, calendar, holidays, diary

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

;; This collection of functions implements the features of calendar.el,
;; diary.el, and holidays.el that deal with the Chinese calendar.  It was
;; written by

;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'lunar)

(defvar chinese-calendar-terrestrial-branch
  ["Zi" "Chou" "Yin" "Mao" "Chen" "Si" "Wu" "Wei" "Shen" "You" "Xu" "Hai"])

(defvar chinese-calendar-celestial-stem
  ["Jia" "Yi" "Bing" "Ding" "Wu" "Ji" "Geng" "Xin" "Ren" "Gui"])

(defvar chinese-calendar-time-zone 
  '(if (< year 1928)
       (+ 465 (/ 40.0 60.0))
     480)
  "*Number of minutes difference between local standard time for Chinese
calendar and Coordinated Universal (Greenwich) Time.  Default is for Beijing.
This is an expression in `year' since it changed at 1928-01-01 00:00:00 from
UT+7:45:40 to UT+8.")

(defvar chinese-calendar-location-name "Beijing"
  "*Name of location used for calculation of Chinese calendar.")

(defvar chinese-calendar-daylight-time-offset 0
; The correct value is as follows, but I don't believe the Chinese calendrical
; authorities would use DST in determining astronomical events:
;  60
  "*Number of minutes difference between daylight savings and standard time
for Chinese calendar.  Default is for no daylight savings time.")

(defvar chinese-calendar-standard-time-zone-name
  '(if (< year 1928)
       "PMT"
     "CST")
  "*Abbreviated name of standard time zone used for Chinese calendar.")

(defvar chinese-calendar-daylight-time-zone-name "CDT"
  "*Abbreviated name of daylight-savings time zone used for Chinese calendar.")

(defvar chinese-calendar-daylight-savings-starts nil
; The correct value is as follows, but I don't believe the Chinese calendrical
; authorities would use DST in determining astronomical events:
;  '(cond ((< 1986 year) (calendar-nth-named-day 1 0 4 year 10))
;         ((= 1986 year) '(5 4 1986))
;         (t nil))
  "*Sexp giving the date on which daylight savings time starts for Chinese
calendar.  Default is for no daylight savings time.  See documentation of
`calendar-daylight-savings-starts'.")

(defvar chinese-calendar-daylight-savings-ends nil
; The correct value is as follows, but I don't believe the Chinese calendrical
; authorities would use DST in determining astronomical events:
;  '(if (<= 1986 year) (calendar-nth-named-day 1 0 9 year 11))
  "*Sexp giving the date on which daylight savings time ends for Chinese
calendar.  Default is for no daylight savings time.  See documentation of
`calendar-daylight-savings-ends'.")

(defvar chinese-calendar-daylight-savings-starts-time 0
  "*Number of minutes after midnight that daylight savings time starts for
Chinese calendar.  Default is for no daylight savings time.")

(defvar chinese-calendar-daylight-savings-ends-time 0
  "*Number of minutes after midnight that daylight savings time ends for
Chinese calendar.  Default is for no daylight savings time.")

(defun chinese-zodiac-sign-on-or-after (d)
  "Absolute date of first new Zodiac sign on or after absolute date d.
The Zodiac signs begin when the sun's longitude is a multiple of 30 degrees."
 (let* ((year (extract-calendar-year
                (calendar-gregorian-from-absolute
                 (floor (calendar-absolute-from-astro d)))))
         (calendar-time-zone (eval chinese-calendar-time-zone))
         (calendar-daylight-time-offset
          chinese-calendar-daylight-time-offset)
         (calendar-standard-time-zone-name
          chinese-calendar-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          chinese-calendar-daylight-time-zone-name)
         (calendar-calendar-daylight-savings-starts
          chinese-calendar-daylight-savings-starts)
         (calendar-daylight-savings-ends
          chinese-calendar-daylight-savings-ends)
         (calendar-daylight-savings-starts-time
          chinese-calendar-daylight-savings-starts-time)
         (calendar-daylight-savings-ends-time
          chinese-calendar-daylight-savings-ends-time))
   (floor
    (calendar-absolute-from-astro
     (solar-date-next-longitude
      (calendar-astro-from-absolute d)
      30)))))

(defun chinese-new-moon-on-or-after (d)
  "Absolute date of first new moon on or after absolute date d."
  (let* ((year (extract-calendar-year
                (calendar-gregorian-from-absolute d)))
         (calendar-time-zone (eval chinese-calendar-time-zone))
         (calendar-daylight-time-offset
          chinese-calendar-daylight-time-offset)
         (calendar-standard-time-zone-name
          chinese-calendar-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          chinese-calendar-daylight-time-zone-name)
         (calendar-calendar-daylight-savings-starts
          chinese-calendar-daylight-savings-starts)
         (calendar-daylight-savings-ends
          chinese-calendar-daylight-savings-ends)
         (calendar-daylight-savings-starts-time
          chinese-calendar-daylight-savings-starts-time)
         (calendar-daylight-savings-ends-time
          chinese-calendar-daylight-savings-ends-time))
    (floor
     (calendar-absolute-from-astro
      (lunar-new-moon-on-or-after
       (calendar-astro-from-absolute d))))))

(defun calendar-absolute-from-chinese (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((cycle (car date))
         (year (car (cdr date)))
         (month (car (cdr (cdr date))))
         (day (car (cdr (cdr (cdr date)))))
         (g-year (+ (* (1- cycle) 60);; years in prior cycles
                    (1- year);;         prior years this cycle
                    -2636));;           years before absolute date 0
         (new-year (chinese-new-year g-year))
         (current-month new-year)
         (current-month-number 1)
         (next-month (chinese-new-moon-on-or-after (1+ new-year)))
         (next-sign (chinese-zodiac-sign-on-or-after
                     (1+ (chinese-zodiac-sign-on-or-after current-month))))
         (had-leap-month nil))
    (while (< current-month-number month)
      ;; current-month < next-month <= next-sign
      (setq current-month next-month)
      (setq next-month (chinese-new-moon-on-or-after (1+ current-month)))
      (if (and (<= next-month next-sign) (not had-leap-month))
          (progn;; leap month
            (setq current-month-number (+ current-month-number 0.5))
            (setq had-leap-month t))
        (setq current-month-number (floor (1+ current-month-number)))
        (setq next-sign (chinese-zodiac-sign-on-or-after (1+ next-sign)))))
    (+ current-month (1- day))))

(defun calendar-chinese-from-absolute (date)
  "Compute Chinese date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((greg-date (calendar-gregorian-from-absolute date))
         (greg-year (1- (extract-calendar-year greg-date)))
         (greg-year
          (+ greg-year
             (calendar-sum y greg-year
                           (>= date (chinese-new-year (1+ y))) 1)) )
         (chinese-year (+ greg-year 2697))
         (cycle (/ (1- chinese-year) 60))              ;; previous cycles
         (year (calendar-mod chinese-year 60));; years this cycle
         (current-month (chinese-new-year greg-year))
         (month 1)
         (next-month (chinese-new-moon-on-or-after (1+ current-month)))
         (next-sign (chinese-zodiac-sign-on-or-after
                     (1+ (chinese-zodiac-sign-on-or-after current-month))))
         (had-leap-month nil))
    (while (<= next-month date)
      ;; current-month < next-month <= next-sign
      (setq current-month next-month)
      (setq next-month (chinese-new-moon-on-or-after (1+ current-month)))
      (if (and (<= next-month next-sign) (not had-leap-month))
          (progn;; leap month
            (setq month (+ month 0.5))
            (setq had-leap-month t))
        (setq month (floor (1+ month)))
        (setq next-sign (chinese-zodiac-sign-on-or-after (1+ next-sign)))))
    (list cycle year month (1+ (- date current-month)))))

(defun chinese-new-year (year)
  "The absolute date of Chinese New Year in Gregorian YEAR."
  (let* ((last-solstice (chinese-zodiac-sign-on-or-after
                         (calendar-absolute-from-gregorian
                          (list 12 15 (1- year)))))
         (twelfth-new-moon;;       twelfth month of previous year
          (chinese-new-moon-on-or-after (1+ last-solstice)))
         (thirteenth-new-moon;;    maybe leap month, maybe New Year
          (chinese-new-moon-on-or-after (1+ twelfth-new-moon)))
         (fourteenth-new-moon;;    maybe New Year, maybe second month
          (chinese-new-moon-on-or-after (1+ thirteenth-new-moon)))
         (next-solstice (chinese-zodiac-sign-on-or-after
                         (calendar-absolute-from-gregorian (list 12 15 year))))
         (new-moons (+ 3 (calendar-sum m 0
                                       (< (chinese-new-moon-on-or-after
                                           (+ fourteenth-new-moon (* 29 m)))
                                          next-solstice)
                                       1))))
    (if (and (= new-moons 14)
             (< (chinese-zodiac-sign-on-or-after
                 (calendar-absolute-from-gregorian (list 2 15 year)))
                thirteenth-new-moon)
             (<= fourteenth-new-moon
                 (chinese-zodiac-sign-on-or-after
                  (calendar-absolute-from-gregorian (list 3 15 year)))))
        fourteeth-new-moon
      thirteenth-new-moon)))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (chinese-new-year y))))
          (if (calendar-date-is-visible-p chinese-new-year)
          (list (list chinese-new-year
                      (format "Chinese New Year (%s-%s)"
                              (aref chinese-calendar-celestial-stem
                                    (% (+ y 6) 10))
                              (aref chinese-calendar-terrestrial-branch
                                    (% (+ y 8) 12))))))))))

(defun calendar-chinese-date-string (&optional date)
  "String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian
                  (or date (calendar-current-date))))
         (c-date (calendar-chinese-from-absolute a-date))
         (cycle (car c-date))
         (year (car (cdr c-date)))
         (month (car (cdr (cdr c-date))))
         (day (car (cdr (cdr (cdr c-date)))))
         (this-month (calendar-absolute-from-chinese
                      (list cycle year month 1)))
         (next-month (calendar-absolute-from-chinese
                      (list cycle year (1+ month) 1)))
         (month (floor month))
         (m-cycle (% (+ (* year 5) month) 60)))
    (format "Cycle %s, year %s (%s-%s), %smonth %s, day %s (%s-%s)"
            cycle
            year
            (aref chinese-calendar-celestial-stem (% (+ year 9) 10))
            (aref chinese-calendar-terrestrial-branch (% (+ year 11) 12))
            (if (not (integerp month))
                "second "
              (if (< 30 (- next-month this-month))
                  "first "
                ""))
            month
            day
            (aref chinese-calendar-celestial-stem (% (+ a-date 4) 10))
            (aref chinese-calendar-terrestrial-branch (% (+ a-date 2) 12)))))

(defun calendar-print-chinese-date ()
  "Show the Chinese date equivalents of date."
  (interactive)
  (message "Computing Chinese date...")
  (message "Chinese date: %s"
           (calendar-chinese-date-string (calendar-cursor-to-date t))))

(defun calendar-goto-chinese-date (date &optional noecho)
  "Move cursor to Chinese date DATE.
Echo Chinese date unless NOECHO is t."
  (interactive
   (let* ((c (calendar-chinese-from-absolute
              (calendar-absolute-from-gregorian
               (calendar-current-date))))
          (cycle (calendar-read
                  "Cycle number (>44): "
                  '(lambda (x) (> x 44))
                  (int-to-string (car c))))
          (year (calendar-read
                 "Year in cycle (1..60): "
                 '(lambda (x) (and (<= 1 x) (<= x 60)))
                 (int-to-string (car (cdr c)))))
          (month (read-minibuffer "Month: "))
          (day (read-minibuffer "Day: ")))
     (list (list cycle year month day))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-chinese date)))
  (or noecho (calendar-print-chinese-date)))

(defun diary-chinese-date ()
  "Chinese calendar equivalent of date diary entry."
  (format "Chinese date: %s" (calendar-chinese-date-string date)))

(provide 'cal-chinese)

;;; cal-chinese ends here
