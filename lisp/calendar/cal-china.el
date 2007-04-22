;;; cal-china.el --- calendar functions for the Chinese calendar

;; Copyright (C) 1995, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This collection of functions implements the features of calendar.el,
;; diary.el, and holidays.el that deal with the Chinese calendar.  The rules
;; used for the Chinese calendar are those of Baolin Liu (see L. E. Doggett's
;; article "Calendars" in the Explanatory Supplement to the Astronomical
;; Almanac, second edition, 1992) for the calendar as revised at the beginning
;; of the Qing dynasty in 1644.  The nature of the astronomical calculations
;; is such that precise calculations cannot be made without great expense in
;; time, so that the calendars produced may not agree perfectly with published
;; tables--but no two pairs of published tables agree perfectly either!  Liu's
;; rules produce a calendar for 2033 which is not accepted by all authorities.
;; The date of Chinese New Year is correct from 1644-2051.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;;; Code:

(defvar date)
(defvar displayed-month)
(defvar displayed-year)

(require 'lunar)

(defvar chinese-calendar-celestial-stem
  ["Jia" "Yi" "Bing" "Ding" "Wu" "Ji" "Geng" "Xin" "Ren" "Gui"])

(defvar chinese-calendar-terrestrial-branch
  ["Zi" "Chou" "Yin" "Mao" "Chen" "Si" "Wu" "Wei" "Shen" "You" "Xu" "Hai"])

(defcustom chinese-calendar-time-zone
  '(if (< year 1928)
       (+ 465 (/ 40.0 60.0))
     480)
  "*Number of minutes difference between local standard time for Chinese
calendar and Coordinated Universal (Greenwich) Time.  Default is for Beijing.
This is an expression in `year' since it changed at 1928-01-01 00:00:00 from
UT+7:45:40 to UT+8."
  :type 'sexp
  :group 'chinese-calendar)

(defcustom chinese-calendar-location-name "Beijing"
  "*Name of location used for calculation of Chinese calendar."
  :type 'string
  :group 'chinese-calendar)

(defcustom chinese-calendar-daylight-time-offset 0
; The correct value is as follows, but the Chinese calendrical
; authorities do NOT use DST in determining astronomical events:
;  60
  "*Number of minutes difference between daylight saving and standard time
for Chinese calendar.  Default is for no daylight saving time."
  :type 'integer
  :group 'chinese-calendar)

(defcustom chinese-calendar-standard-time-zone-name
  '(if (< year 1928)
       "PMT"
     "CST")
  "*Abbreviated name of standard time zone used for Chinese calendar.
This is an expression depending on `year' because it changed
at 1928-01-01 00:00:00 from `PMT' to `CST'."
  :type 'sexp
  :group 'chinese-calendar)

(defcustom chinese-calendar-daylight-time-zone-name "CDT"
  "*Abbreviated name of daylight saving time zone used for Chinese calendar."
  :type 'string
  :group 'chinese-calendar)

(defcustom chinese-calendar-daylight-savings-starts nil
; The correct value is as follows, but the Chinese calendrical
; authorities do NOT use DST in determining astronomical events:
;  '(cond ((< 1986 year) (calendar-nth-named-day 1 0 4 year 10))
;         ((= 1986 year) '(5 4 1986))
;         (t nil))
  "*Sexp giving the date on which daylight saving time starts for Chinese
calendar.  Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-starts'."
  :type 'sexp
  :group 'chinese-calendar)

(defcustom chinese-calendar-daylight-savings-ends nil
; The correct value is as follows, but the Chinese calendrical
; authorities do NOT use DST in determining astronomical events:
;  '(if (<= 1986 year) (calendar-nth-named-day 1 0 9 year 11))
  "*Sexp giving the date on which daylight saving time ends for Chinese
calendar.  Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-ends'."
  :type 'sexp
  :group 'chinese-calendar)

(defcustom chinese-calendar-daylight-savings-starts-time 0
  "*Number of minutes after midnight that daylight saving time starts for
Chinese calendar.  Default is for no daylight saving time."
  :type 'integer
  :group 'chinese-calendar)

(defcustom chinese-calendar-daylight-savings-ends-time 0
  "*Number of minutes after midnight that daylight saving time ends for
Chinese calendar.  Default is for no daylight saving time."
  :type 'integer
  :group 'chinese-calendar)

(defun chinese-zodiac-sign-on-or-after (d)
  "Absolute date of first new Zodiac sign on or after absolute date d.
The Zodiac signs begin when the sun's longitude is a multiple of 30 degrees."
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

(defvar chinese-year-cache
  '((1990 (12 726464) (1 726494) (2 726523) (3 726553) (4 726582) (5 726611)
          (5.5 726641) (6 726670) (7 726699) (8 726729) (9 726758) (10 726788)
          (11 726818))
    (1991 (12 726848) (1 726878) (2 726907) (3 726937) (4 726966) (5 726995)
          (6 727025) (7 727054) (8 727083) (9 727113) (10 727142) (11 727172))
    (1992 (12 727202) (1 727232) (2 727261) (3 727291) (4 727321) (5 727350)
          (6 727379) (7 727409) (8 727438) (9 727467) (10 727497) (11 727526))
    (1993 (12 727556) (1 727586) (2 727615) (3 727645) (3.5 727675) (4 727704)
          (5 727734) (6 727763) (7 727793) (8 727822) (9 727851) (10 727881)
          (11 727910))
    (1994 (12 727940) (1 727969) (2 727999) (3 728029) (4 728059) (5 728088)
          (6 728118) (7 728147) (8 728177) (9 728206) (10 728235) (11 728265))
    (1995 (12 728294) (1 728324) (2 728353) (3 728383) (4 728413) (5 728442)
          (6 728472) (7 728501) (8 728531) (8.5 728561) (9 728590) (10 728619)
          (11 728649))
    (1996 (12 728678) (1 728708) (2 728737) (3 728767) (4 728796) (5 728826)
          (6 728856) (7 728885) (8 728915) (9 728944) (10 728974) (11 729004))
    (1997 (12 729033) (1 729062) (2 729092) (3 729121) (4 729151) (5 729180)
          (6 729210) (7 729239) (8 729269) (9 729299) (10 729328) (11 729358))
    (1998 (12 729388) (1 729417) (2 729447) (3 729476) (4 729505) (5 729535)
          (5.5 729564) (6 729593) (7 729623) (8 729653) (9 729682) (10 729712)
          (11 729742))
    (1999 (12 729771) (1 729801) (2 729831) (3 729860) (4 729889) (5 729919)
          (6 729948) (7 729977) (8 730007) (9 730036) (10 730066) (11 730096))
    (2000 (12 730126) (1 730155) (2 730185) (3 730215) (4 730244) (5 730273)
          (6 730303) (7 730332) (8 730361) (9 730391) (10 730420) (11 730450))
    (2001 (12 730480) (1 730509) (2 730539) (3 730569) (4 730598) (4.5 730628)
          (5 730657) (6 730687) (7 730716) (8 730745) (9 730775) (10 730804)
          (11 730834))
    (2002 (12 730863) (1 730893) (2 730923) (3 730953) (4 730982) (5 731012)
          (6 731041) (7 731071) (8 731100) (9 731129) (10 731159) (11 731188))
    (2003 (12 731218) (1 731247) (2 731277) (3 731307) (4 731336) (5 731366)
          (6 731396) (7 731425) (8 731455) (9 731484) (10 731513) (11 731543))
    (2004 (12 731572) (1 731602) (2 731631) (2.5 731661) (3 731690) (4 731720)
          (5 731750) (6 731779) (7 731809) (8 731838) (9 731868) (10 731897)
          (11 731927))
    (2005 (12 731956) (1 731986) (2 732015) (3 732045) (4 732074) (5 732104)
          (6 732133) (7 732163) (8 732193) (9 732222) (10 732252) (11 732281))
    (2006 (12 732311) (1 732340) (2 732370) (3 732399) (4 732429) (5 732458)
          (6 732488) (7 732517) (7.5 732547) (8 732576) (9 732606) (10 732636)
          (11 732665))
    (2007 (12 732695) (1 732725) (2 732754) (3 732783) (4 732813) (5 732842)
          (6 732871) (7 732901) (8 732930) (9 732960) (10 732990) (11 733020))
    (2008 (12 733049) (1 733079) (2 733109) (3 733138) (4 733167) (5 733197)
          (6 733226) (7 733255) (8 733285) (9 733314) (10 733344) (11 733374))
    (2009 (12 733403) (1 733433) (2 733463) (3 733493) (4 733522) (5 733551)
          (5.5 733581) (6 733610) (7 733639) (8 733669) (9 733698) (10 733728)
          (11 733757))
    (2010 (12 733787) (1 733817) (2 733847) (3 733876) (4 733906) (5 733935)
          (6 733965) (7 733994) (8 734023) (9 734053) (10 734082) (11 734112)))
  "An assoc list of Chinese year structures as determined by `chinese-year'.

Values are computed as needed, but to save time, the initial value consists
of the precomputed years 1990-2010.  The code works just as well with this
set to nil initially (which is how the value for 1990-2010 was computed).")

(defun chinese-year (y)
  "The structure of the Chinese year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Chinese months from the Chinese month following the solstice in
Gregorian year Y-1 to the Chinese month of the solstice of Gregorian year Y.

The list is cached for further use."
  (let ((list (cdr (assoc y chinese-year-cache))))
    (if (not list)
        (progn
          (setq list (compute-chinese-year y))
          (setq chinese-year-cache
                (append chinese-year-cache (list (cons y list))))))
    list))

(defun number-chinese-months (list start)
  "Assign month numbers to the lunar months in LIST, starting with START.
Numbers are assigned sequentially, START, START+1, ..., 11, with half
numbers used for leap months.

First month of list will never be a leap month, nor will the last."
  (if list
      (if (zerop (- 12 start (length list)))
          ;; List is too short for a leap month
          (cons (list start (car list))
                (number-chinese-months (cdr list) (1+ start)))
        (cons
         ;; First month
         (list start (car list))
         ;; Remaining months
         (if (and (cdr (cdr list));; at least two more months...
                  (<= (car (cdr (cdr list)))
                      (chinese-zodiac-sign-on-or-after (car (cdr list)))))
             ;; Next month is a leap month
             (cons (list (+ start 0.5) (car (cdr list)))
                   (number-chinese-months (cdr (cdr list)) (1+ start)))
           ;; Next month is not a leap month
           (number-chinese-months (cdr list) (1+ start)))))))

(defun chinese-month-list (start end)
  "List of starting dates of Chinese months from START to END."
  (if (<= start end)
      (let ((new-moon (chinese-new-moon-on-or-after start)))
        (if (<= new-moon end)
            (cons new-moon
                  (chinese-month-list (1+ new-moon) end))))))

(defun compute-chinese-year (y)
  "Compute the structure of the Chinese year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Chinese months from the Chinese month following the solstice in
Gregorian year Y-1 to the Chinese month of the solstice of Gregorian year Y."
  (let* ((next-solstice (chinese-zodiac-sign-on-or-after
                         (calendar-absolute-from-gregorian
                          (list 12 15 y))))
         (list (chinese-month-list (1+ (chinese-zodiac-sign-on-or-after
                                        (calendar-absolute-from-gregorian
                                         (list 12 15 (1- y)))))
                                   next-solstice))
         (next-sign (chinese-zodiac-sign-on-or-after (car list))))
    (if (= (length list) 12)
        ;; No room for a leap month, just number them 12, 1, 2, ..., 11
        (cons (list 12 (car list))
              (number-chinese-months (cdr list) 1))
      ;; Now we can assign numbers to the list for y
      ;; The first month or two are special
      (if (or (> (car list) next-sign) (>= next-sign (car (cdr list))))
          ;; First month on list is a leap month, second is not
          (append (list (list 11.5 (car list))
                        (list 12 (car (cdr list))))
                  (number-chinese-months (cdr (cdr list)) 1))
        ;; First month on list is not a leap month
        (append (list (list 12 (car list)))
                (if (>= (chinese-zodiac-sign-on-or-after (car (cdr list)))
                        (car (cdr (cdr list))))
                    ;; Second month on list is a leap month
                    (cons (list 12.5 (car (cdr list)))
                          (number-chinese-months (cdr (cdr list)) 1))
                  ;; Second month on list is not a leap month
                  (number-chinese-months (cdr list) 1)))))))

(defun calendar-absolute-from-chinese (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((cycle (car date))
         (year (car (cdr date)))
         (month (car (cdr (cdr date))))
         (day (car (cdr (cdr (cdr date)))))
         (g-year (+ (* (1- cycle) 60);; years in prior cycles
                    (1- year)        ;; prior years this cycle
                    -2636)))         ;; years before absolute date 0
    (+ (1- day);; prior days this month
       (car
        (cdr    ;; absolute date of start of this month
         (assoc month (append (memq (assoc 1 (chinese-year g-year))
                                    (chinese-year g-year))
                              (chinese-year (1+ g-year)))))))))

(defun calendar-chinese-from-absolute (date)
  "Compute Chinese date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (extract-calendar-year
                  (calendar-gregorian-from-absolute date)))
         (c-year (+ g-year 2695))
         (list (append (chinese-year (1- g-year))
                       (chinese-year g-year)
                       (chinese-year (1+ g-year)))))
    (while (<= (car (cdr (car (cdr list)))) date)
      ;; the first month on the list is in Chinese year c-year
      ;; date is on or after start of second month on list...
      (if (= 1 (car (car (cdr list))))
          ;; second month on list is a new Chinese year
          (setq c-year (1+ c-year)))
      ;; ...so first month on list is of no interest
      (setq list (cdr list)))
    (list (/ (1- c-year) 60)
          (calendar-mod c-year 60)
          (car (car list))
          (1+ (- date (car (cdr (car list))))))))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (chinese-year y)))))))
          (if (calendar-date-is-visible-p chinese-new-year)
          (list
           (list chinese-new-year
                 (format "Chinese New Year (%s)"
                         (calendar-chinese-sexagesimal-name (+ y 57))))))))))

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
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            (calendar-mod (1+ (floor month)) 12)
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
    (format "Cycle %s, year %s (%s), %smonth %s%s, day %s (%s)"
            cycle
            year (calendar-chinese-sexagesimal-name year)
            (if (not (integerp month))
                "second "
              (if (< 30 (- next-month this-month))
                  "first "
                ""))
            (floor month)
            (if (integerp month)
                (format " (%s)" (calendar-chinese-sexagesimal-name
                                 (+ (* 12 year) month 50)))
              "")
            day (calendar-chinese-sexagesimal-name (+ a-date 15)))))

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s-%s"
          (aref chinese-calendar-celestial-stem (% (1- n) 10))
          (aref chinese-calendar-terrestrial-branch (% (1- n) 12))))

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
                  "Chinese calendar cycle number (>44): "
                  '(lambda (x) (> x 44))
                  (int-to-string (car c))))
          (year (calendar-read
                 "Year in Chinese cycle (1..60): "
                 '(lambda (x) (and (<= 1 x) (<= x 60)))
                 (int-to-string (car (cdr c)))))
          (month-list (make-chinese-month-assoc-list
                       (chinese-months cycle year)))
          (month (cdr (assoc
                       (completing-read "Chinese calendar month: "
                                        month-list nil t)
                       month-list)))
          (last (if (= month
                       (car (cdr (cdr
                                  (calendar-chinese-from-absolute
                                   (+ 29
                                      (calendar-absolute-from-chinese
                                       (list cycle year month 1))))))))
                    30
                  29))
          (day (calendar-read
                (format "Chinese calendar day (1-%d): " last)
                '(lambda (x) (and (<= 1 x) (<= x last))))))
     (list (list cycle year month day))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-chinese date)))
  (or noecho (calendar-print-chinese-date)))

(defun chinese-months (c y)
  "A list of the months in cycle C, year Y of the Chinese calendar."
  (let* ((l (memq 1 (append
                     (mapcar '(lambda (x)
                                (car x))
                             (chinese-year (extract-calendar-year
                                            (calendar-gregorian-from-absolute
                                             (calendar-absolute-from-chinese
                                              (list c y 1 1))))))
                     (mapcar '(lambda (x)
                                (if (> (car x) 11) (car x)))
                             (chinese-year (extract-calendar-year
                                            (calendar-gregorian-from-absolute
                                             (calendar-absolute-from-chinese
                                              (list (if (= y 60) (1+ c) c)
                                                    (if (= y 60) 1 y)
                                                    1 1))))))))))
    l))

(defun make-chinese-month-assoc-list (l)
  "Make list of months L into an assoc list."
  (if (and l (car l))
      (if (and (cdr l) (car (cdr l)))
          (if (= (car l) (floor (car (cdr l))))
              (append
               (list (cons (format "%s (first)" (car l)) (car l))
                     (cons (format "%s (second)" (car l)) (car (cdr l))))
               (make-chinese-month-assoc-list (cdr (cdr l))))
            (append
             (list (cons (int-to-string (car l)) (car l)))
             (make-chinese-month-assoc-list (cdr l))))
        (list (cons (int-to-string (car l)) (car l))))))

(defun diary-chinese-date ()
  "Chinese calendar equivalent of date diary entry."
  (format "Chinese date: %s" (calendar-chinese-date-string date)))

(provide 'cal-china)

;;; arch-tag: 7e5b7e0d-676c-47e3-8696-93e7ea0ab644
;;; cal-china.el ends here
