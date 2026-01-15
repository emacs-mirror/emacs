;;; cal-bahai.el --- calendar functions for the Bahá’í calendar.  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Bahá’í calendar, Bahá’í, Baha'i, Bahai, calendar, diary
;; Package: calendar

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This collection of functions implements the features of calendar.el
;; and diary-lib.el that deal with the Bahá’í calendar.

;; The Bahá’í (https://www.bahai.org) calendar system is based on a
;; solar cycle of 19 months with 19 days each.  The remaining
;; "intercalary" days are called the Ayyám-i-Há (days of Há), and are
;; placed between the 18th and 19th months.  They are meant as a time
;; of festivals preceding the 19th month, which is the month of
;; fasting.  (Há has the numerical value of 5 in the arabic abjad, or
;; letter-to-number, reckoning).

;; Each month is named after an attribute of God, as are the 19 days
;; -- which have the same names as the months.  There is also a name
;; for each year in every 19 year cycle.  These cycles are called
;; Váhids.  A cycle of 19 Váhids (361 years) is called a Kullu-Shay,
;; which means "all things".

;; The calendar was named the "Badí‘ calendar" by its author, the Báb.
;; It uses a week of seven days, corresponding to the Gregorian week,
;; each of which has its own name, again patterned after the
;; attributes of God.

;; CALENDAR REFORM (2014/172 BE):
;; Prior to 172 BE (before Naw-Rúz 2015 CE), Naw-Rúz was fixed at
;; March 21 and leap years followed the Gregorian pattern.
;;
;; From 172 BE onwards, the Universal House of Justice implemented
;; astronomical observations:
;; - Naw-Rúz is determined by the vernal equinox as observed from Tehran
;; - Naw-Rúz can fall on March 19, 20, 21, or 22
;; - Ayyám-i-Há has 4 or 5 days depending on the gap between successive
;;   Naw-Rúz dates (ensuring month 19 always ends the day before Naw-Rúz)
;; - Days run from sunset to sunset; if the equinox occurs before sunset
;;   in Tehran, that day is Naw-Rúz; otherwise the next day is Naw-Rúz
;;
;; The implementation uses official tables from the Nautical Almanac
;; Office for years 172-221 BE (2015-2064 CE), and astronomical
;; calculations for years beyond this range.

;; Note: The days of Ayyám-i-Há are encoded as zero and negative
;; offsets from the first day of the final month.  So, (19 -3 157) is
;; the first day of Ayyám-i-Há, in the year 157 BE.

;;; Code:

(require 'calendar)
(require 'solar)
(require 'lunar)

(defconst calendar-bahai-month-name-array
  ["Bahá" "Jalál" "Jamál" "‘Aẓamat" "Núr" "Raḥmat" "Kalimát" "Kamál"
   "Asmá’" "‘Izzat" "Mashíyyat" "‘Ilm" "Qudrat" "Qawl" "Masá’il"
   "Sharaf" "Sulṭán" "Mulk" "‘Alá’"]
  "Array of the month names in the Bahá’í calendar.")

(defconst calendar-bahai-epoch (calendar-absolute-from-gregorian '(3 21 1844))
  "Absolute date of start of Bahá’í calendar = March 21, 1844 AD.")

;; Constants for Tehran, Iran (observing location for equinox)
(defconst calendar-bahai-tehran-latitude 35.6892
  "Latitude of Tehran, Iran in decimal degrees.")

(defconst calendar-bahai-tehran-longitude 51.3890
  "Longitude of Tehran, Iran in decimal degrees.")

(defconst calendar-bahai-tehran-timezone 210
  "Tehran timezone offset in minutes (UTC+3:30 = 210 minutes).")

(defconst calendar-bahai-reform-year 172
  "Bahá’í year when calendar reform took effect.
From this year onwards, Naw-Rúz is determined by the vernal equinox
as observed from Tehran, rather than being fixed at March 21.")

(defun calendar-bahai-nawruz-for-gregorian-year (greg-year)
  "Calculate Gregorian date of Naw-Rúz for Gregorian year GREG-YEAR.
Uses the vernal equinox as observed from Tehran to determine the date.
The result is a Gregorian date (month day year).
Bahá’í days run from sunset to sunset, so if the equinox occurs before
sunset in Tehran, that day is Naw-Rúz; otherwise the next day is."
  (let* ((calendar-latitude calendar-bahai-tehran-latitude)
         (calendar-longitude calendar-bahai-tehran-longitude)
         (calendar-time-zone calendar-bahai-tehran-timezone)
         ;; Disable DST for Tehran (Iran doesn't use DST anymore)
         (calendar-daylight-savings-starts nil)
         (calendar-daylight-savings-ends nil)
         ;; Get vernal equinox date and time (k=0 for spring equinox)
         (equinox (solar-equinoxes/solstices 0 greg-year))
         (eq-month (car equinox))
         (eq-day-frac (cadr equinox))
         (eq-day (floor eq-day-frac))
         (eq-year (nth 2 equinox))
         (eq-date (list eq-month eq-day eq-year))
         ;; Time of equinox in hours (fractional part of day * 24)
         (eq-time (* 24 (- eq-day-frac eq-day)))
         ;; Get sunset time for this date in Tehran
         ;; solar-sunrise-sunset returns ((sunrise-time zone) (sunset-time zone) daylight)
         (sunset-data (solar-sunrise-sunset eq-date))
         (sunset-time (car (cadr sunset-data))) ; sunset time in hours
         ;; Tolerance in hours (2 minutes = 2/60 hours) to account for
         ;; minor differences in astronomical calculations vs official tables.
         ;; When the equinox is extremely close to sunset, small variations
         ;; in ephemeris data or refraction calculations can affect the result.
         (tolerance (/ 2.0 60)))
    ;; If equinox occurs clearly before sunset (by more than the tolerance),
    ;; Naw-Rúz is that day.  Otherwise, Naw-Rúz is the next day.
    (if (and sunset-time (< eq-time (- sunset-time tolerance)))
        eq-date
      (calendar-gregorian-from-absolute
       (1+ (calendar-absolute-from-gregorian eq-date))))))

(defun calendar-bahai-nawruz (year)
  "Absolute date of Naw-Rúz (New Year) for Bahá’í YEAR.
For years before 172 BE, uses fixed March 21.
For years from 172 BE onwards, uses astronomical calculation of vernal equinox."
  (if (< year calendar-bahai-reform-year)
      ;; Pre-reform: Naw-Rúz is always March 21
      ;; Year N starts in Gregorian year 1843 + N
      (calendar-absolute-from-gregorian (list 3 21 (+ year 1843)))
    ;; Post-reform: Calculate from vernal equinox
    (let ((greg-year (+ year 1843)))
      (calendar-absolute-from-gregorian
       (calendar-bahai-nawruz-for-gregorian-year greg-year)))))

(defun calendar-bahai-twin-holy-birthdays-for-year (bahai-year)
  "Calculate Gregorian dates of the Twin Holy Birthdays for Bahá’í YEAR.
Returns a list of two Gregorian dates: (BAB-DATE BAHA-DATE).
The dates are determined by the eighth new moon after Naw-Rúz,
calculated for Tehran's timezone.  The first day following the
eighth new moon is the Birth of the Báb, and the second day is
the Birth of Bahá’u’lláh."
  (let* ((calendar-time-zone calendar-bahai-tehran-timezone)
         ;; Disable DST for Tehran
         (calendar-daylight-savings-starts nil)
         (calendar-daylight-savings-ends nil)
         ;; Get absolute date of Naw-Rúz for this Bahá’í year
         (nawruz-absolute (calendar-bahai-nawruz bahai-year))
         ;; Naw-Rúz starts at sunset on this date in Tehran
         ;; We need to find new moons that occur AFTER sunset on Naw-Rúz
         (nawruz-greg (calendar-gregorian-from-absolute nawruz-absolute))
         ;; Get sunset time on Naw-Rúz in Tehran
         (nawruz-sunset-data (let ((calendar-latitude calendar-bahai-tehran-latitude)
                                   (calendar-longitude calendar-bahai-tehran-longitude))
                              (solar-sunrise-sunset nawruz-greg)))
         (nawruz-sunset (or (car (cadr nawruz-sunset-data)) 18.0)) ; default 6pm
         ;; Convert Naw-Rúz sunset to Julian day
         ;; Absolute date is at midnight, add fraction for sunset time
         (nawruz-julian (+ (calendar-astro-from-absolute nawruz-absolute)
                           (/ nawruz-sunset 24.0)))
         ;; Find the 8th new moon after Naw-Rúz sunset
         (current-julian nawruz-julian)
         eighth-new-moon)
    ;; Find 8 new moons after Naw-Rúz by iterating
    (dotimes (_ 8)
      (setq current-julian (lunar-new-moon-on-or-after current-julian))
      (setq eighth-new-moon current-julian)
      ;; Move forward by at least one day to find the next new moon
      ;; (lunar cycle is ~29.5 days, so +1 is safe)
      (setq current-julian (+ current-julian 1)))

    ;; Convert the eighth new moon to absolute date and time
    (let* ((new-moon-abs-with-frac (calendar-astro-to-absolute eighth-new-moon))
           (new-moon-absolute (floor new-moon-abs-with-frac))
           (new-moon-time-frac (- new-moon-abs-with-frac new-moon-absolute))
           (new-moon-time-hours (* 24 new-moon-time-frac))
           (new-moon-greg (calendar-gregorian-from-absolute new-moon-absolute))
           ;; Get sunset time in Tehran for the day of the new moon
           (sunset-data (let ((calendar-latitude calendar-bahai-tehran-latitude)
                              (calendar-longitude calendar-bahai-tehran-longitude)
                              (calendar-time-zone calendar-bahai-tehran-timezone))
                          (solar-sunrise-sunset new-moon-greg)))
           ;; solar-sunrise-sunset returns ((sunrise-time zone) (sunset-time zone) daylight)
           (sunset-time (car (cadr sunset-data)))
           ;; Determine which civil day is the "first day following" the new moon
           ;; In Bahá’í calendar, days run from sunset to sunset.
           ;; If new moon occurs before sunset, the Bahá’í day starting at
           ;; sunset that evening begins the "first day following"
           ;; If new moon occurs after sunset, we're already in the next Bahá’í
           ;; day, so the "first day following" starts at the next sunset
           (bab-absolute (if (and sunset-time (< new-moon-time-hours sunset-time))
                             (1+ new-moon-absolute)  ; Next civil day
                           (+ new-moon-absolute 2))) ; Civil day after next
           (baha-absolute (1+ bab-absolute)))
      ;; Return both dates as Gregorian dates
      (list (calendar-gregorian-from-absolute bab-absolute)
            (calendar-gregorian-from-absolute baha-absolute)))))

(defun calendar-bahai-leap-year-p (year)
  "True if Bahá’í YEAR is a leap year in the Bahá’í calendar.
For years before 172 BE, follows Gregorian leap year pattern.
For years from 172 BE onwards, determined by whether Ayyám-i-Há has 5 days
based on the gap between successive Naw-Rúz dates."
  (if (< year calendar-bahai-reform-year)
      ;; Pre-reform: follows Gregorian pattern
      ;; Ayyám-i-Há of year N falls in Gregorian February of year 1844 + N
      (calendar-leap-year-p (+ year 1844))
    ;; Post-reform: 5 days of Ayyám-i-Há if the gap requires it
    ;; A year has 5 Ayyám-i-Há days if this year's Naw-Rúz to next year's
    ;; Naw-Rúz is 366 days (otherwise 365)
    (let ((this-nawruz (calendar-bahai-nawruz year))
          (next-nawruz (calendar-bahai-nawruz (1+ year))))
      (= (- next-nawruz this-nawruz) 366))))

(defconst calendar-bahai-leap-base
  (+ (/ 1844 4) (- (/ 1844 100)) (/ 1844 400))
  "Number of leap years between 1 and 1844 AD, inclusive.
Used by `calendar-bahai-to-absolute'.")

(defun calendar-bahai-to-absolute (date)
  "Compute absolute date from Bahá’í date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date)))
    (if (< year calendar-bahai-reform-year)
        ;; Pre-reform: use the old fixed calculation
        (let* ((prior-years (+ (1- year) 1844))
               (leap-days (- (+ (/ prior-years 4) ; leap days in prior years
                                (- (/ prior-years 100))
                                (/ prior-years 400))
                             calendar-bahai-leap-base)))
          (+ (1- calendar-bahai-epoch)        ; days before epoch
             (* 365 (1- year))                ; days in prior years
             leap-days
             (calendar-sum m 1 (< m month) 19)
             (if (= month 19)
                 ;; For Ayyám-i-Há (day <= 0), adjust by (ayyam-ha-days - 1)
                 ;; instead of ayyam-ha-days to match encoding
                 (if (<= day 0)
                     (1- (if (calendar-bahai-leap-year-p year) 5 4))
                   (if (calendar-bahai-leap-year-p year) 5 4))
               0)
             day))                              ; days so far this month
      ;; Post-reform: use actual Naw-Rúz dates
      (let ((year-start (calendar-bahai-nawruz year))
            (ayyam-ha-days (if (calendar-bahai-leap-year-p year) 5 4)))
        (+ year-start
           -1 ; go back one day from start
           ;; Add days for complete months
           (cond
            ((< month 19)
             (+ (* 19 (1- month))
                day))
            ;; Month 19, day <= 0: Ayyám-i-Há
            ((<= day 0)
             (+ (* 19 18)
                (+ day (1- ayyam-ha-days))))
            ;; Month 19, day > 0: month 'Alá'
            (t
             (+ (* 19 18)
                ayyam-ha-days
                day))))))))

(defun calendar-bahai-from-absolute (date)
  "Bahá’í date (month day year) corresponding to the absolute DATE."
  (if (< date calendar-bahai-epoch)
      (list 0 0 0)                      ; pre-Bahá’í date
    (let* ((greg (calendar-gregorian-from-absolute date))
           (gmonth (calendar-extract-month greg))
           (gyear (calendar-extract-year greg))
           (gday (calendar-extract-day greg))
           ;; Estimate the Bahá’í year
           (year (+ (- gyear 1844)
                    (if (or (> gmonth 3)
                            (and (= gmonth 3) (>= gday 15)))
                        1 0))))
      ;; Adjust year if needed based on actual Naw-Rúz
      (while (< date (calendar-bahai-nawruz year))
        (setq year (1- year)))
      (while (>= date (calendar-bahai-nawruz (1+ year)))
        (setq year (1+ year)))
      ;; Now calculate month and day within the year
      (let* ((year-start (calendar-bahai-nawruz year))
             (days-in-year (- date year-start))
             (ayyam-ha-days (if (calendar-bahai-leap-year-p year) 5 4))
             month day)
        (cond
         ;; In first 18 months (days 0-341)
         ((< days-in-year (* 19 18))
          (setq month (1+ (/ days-in-year 19))
                day (1+ (% days-in-year 19))))
         ;; In Ayyám-i-Há (days 342-345 or 342-346)
         ((< days-in-year (+ (* 19 18) ayyam-ha-days))
          ;; Encode Ayyám-i-Há as month 19 with day <= 0
          ;; First day is -(ayyam-ha-days-1), last day is 0
          (let ((ayyam-day (- days-in-year (* 19 18))))
            (setq month 19
                  day (- ayyam-day (1- ayyam-ha-days)))))
         ;; In month 19 ('Alá')
         (t
          (setq month 19
                day (1+ (- days-in-year (* 19 18) ayyam-ha-days)))))
        (list month day year)))))

;;;###cal-autoload
(defun calendar-bahai-date-string (&optional date)
  "String of Bahá’í date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((bahai-date (calendar-bahai-from-absolute
                      (calendar-absolute-from-gregorian
                       (or date (calendar-current-date)))))
         (y (calendar-extract-year bahai-date)))
    (if (< y 1)
        ""                              ; pre-Bahai
      (let ((m (calendar-extract-month bahai-date))
            (d (calendar-extract-day bahai-date)))
        (calendar-dlet
            ((monthname (if (and (= m 19)
                                 (<= d 0))
                            "Ayyám-i-Há"
                          (aref calendar-bahai-month-name-array (1- m))))
             (day (number-to-string
                   (if (<= d 0)
                       (+ d (if (calendar-bahai-leap-year-p y) 5 4))
                     d)))
             (year (number-to-string y))
             (month (number-to-string m))
             dayname)
          ;; Can't call calendar-date-string because of monthname oddity.
          (mapconcat #'eval calendar-date-display-form ""))))))

;;;###cal-autoload
(defun calendar-bahai-print-date ()
  "Show the Bahá’í calendar equivalent of the selected date."
  (interactive)
  (let ((s (calendar-bahai-date-string (calendar-cursor-to-date t))))
   (if (string-equal s "")
       (message "Date is pre-Bahá’í")
     (message "Bahá’í date: %s" s))))

(defun calendar-bahai-read-date ()
 "Interactively read the arguments for a Bahá’í date command.
Reads a year, month and day."
  (let* ((today (calendar-current-date))
         (year (calendar-read-sexp
                "Bahá’í calendar year (not 0)"
                (lambda (x) (not (zerop x)))
                (calendar-extract-year
                 (calendar-bahai-from-absolute
                  (calendar-absolute-from-gregorian today)))))
         (completion-ignore-case t)
         (month (cdr (assoc
                      (completing-read
                       "Bahá’í calendar month name: "
                       (mapcar 'list
                               (append calendar-bahai-month-name-array nil))
                       nil t)
                      (calendar-make-alist calendar-bahai-month-name-array
                                           1))))
         (day (calendar-read-sexp "Bahá’í calendar day (1-19)"
                                  (lambda (x) (and (< 0 x) (<= x 19)))
                                  1)))
    (list (list month day year))))

;;;###cal-autoload
(defun calendar-bahai-goto-date (date &optional noecho)
  "Move cursor to Bahá’í date DATE; echo Bahá’í date unless NOECHO is non-nil."
  (interactive (calendar-bahai-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-bahai-to-absolute date)))
  (or noecho (calendar-bahai-print-date)))

(defvar displayed-month)
(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-bahai (month day string)
  "Holiday on MONTH, DAY (Bahá’í) called STRING.
If MONTH, DAY (Bahá’í) is visible in the current calendar window,
returns the corresponding Gregorian date in the form of the
list (((month day year) STRING)).  Otherwise, returns nil."
  ;; Since the calendar window shows 3 months at a time, there are
  ;; approx +/- 45 days either side of the central month.
  ;; Since the Bahai months have 19 days, this means up to +/- 3 months.
  (let* ((bahai-date (calendar-bahai-from-absolute
                      (calendar-absolute-from-gregorian
                       (list displayed-month 15 displayed-year))))
         (m (calendar-extract-month bahai-date))
         (y (calendar-extract-year bahai-date))
         date)
    (unless (< m 1)                    ; Bahá’í calendar doesn't apply
      ;; Cf holiday-fixed, holiday-islamic.
      ;; With a +- 3 month calendar window, and 19 months per year,
      ;; month 16 is special.  When m16 is central is when the
      ;; end-of-year first appears.  When m1 is central, m16 is no
      ;; longer visible.  Hence we can do a one-sided test to see if
      ;; m16 is visible.  m16 is visible when the central month >= 13.
      ;; To see if other months are visible we can shift the range
      ;; accordingly.
      (calendar-increment-month m y (- 16 month) 19)
      (and (> m 12)                     ; Bahá’í date might be visible
           (calendar-date-is-visible-p
            (setq date (calendar-gregorian-from-absolute
                        (calendar-bahai-to-absolute (list month day y)))))
           (list (list date string))))))

(autoload 'holiday-fixed "holidays")

;;;###holiday-autoload
(defun holiday-bahai-new-year ()
  "Holiday entry for the Bahá’í New Year, if visible in the calendar window."
  (let* ((bahai-year (- displayed-year (1- 1844)))
         (nawruz-date (if (< bahai-year calendar-bahai-reform-year)
                          ;; Pre-reform: always March 21
                          (list 3 21 displayed-year)
                        ;; Post-reform: calculate from equinox
                        (calendar-bahai-nawruz-for-gregorian-year displayed-year))))
    (when (calendar-date-is-visible-p nawruz-date)
      (list (list nawruz-date
                  (format "Bahá’í New Year (Naw-Ruz) %d" bahai-year))))))

;;;###holiday-autoload
(defun holiday-bahai-twin-holy-birthdays ()
  "Holiday entries for the Twin Holy Birthdays, if visible in the calendar.
The Birth of the Báb and Birth of Bahá’u’lláh are celebrated on
consecutive days.  From 172 BE onwards, these dates are determined
by the eighth new moon after Naw-Rúz; before that, they were fixed
at October 20 and November 12."
  (let* ((bahai-year (- displayed-year (1- 1844)))
         result)
    (if (>= bahai-year calendar-bahai-reform-year)
        ;; Post-reform: calculate from eighth new moon
        (let* ((dates (calendar-bahai-twin-holy-birthdays-for-year bahai-year))
               (bab-date (car dates))
               (baha-date (cadr dates)))
          (when (calendar-date-is-visible-p bab-date)
            (push (list bab-date "Birth of the Báb") result))
          (when (calendar-date-is-visible-p baha-date)
            (push (list baha-date "Birth of Bahá’u’lláh") result)))
      ;; Pre-reform: fixed dates
      (let ((bab-date (list 10 20 displayed-year))
            (baha-date (list 11 12 displayed-year)))
        (when (calendar-date-is-visible-p bab-date)
          (push (list bab-date "Birth of the Báb") result))
        (when (calendar-date-is-visible-p baha-date)
          (push (list baha-date "Birth of Bahá’u’lláh") result))))
    (nreverse result)))

;;;###holiday-autoload
(defun holiday-bahai-ridvan (&optional all)
  "Holidays related to Ridvan, as visible in the calendar window.
Only considers the first, ninth, and twelfth days, unless ALL or
`calendar-bahai-all-holidays-flag' is non-nil.

Ridvan is a 12-day festival from 13 Jalál to 5 Jamál (Bahá’í months 2-3).
In the reformed calendar (172 BE onwards), these dates shift relative to
the Gregorian calendar based on when Naw-Rúz falls."
  (let ((ord ["First" "Second" "Third" "Fourth" "Fifth" "Sixth"
              "Seventh" "Eighth" "Ninth" "Tenth" "Eleventh" "Twelfth"])
        (show '(0 8 11))
        rid h)
    (if (or all calendar-bahai-all-holidays-flag)
        (setq show (number-sequence 0 11)))
    (dolist (i show (nreverse rid))
      ;; Ridvan spans months 2-3 in the Bahá’í calendar:
      ;; Day 1 (i=0) = 13 Jalál = month 2, day 13
      ;; Days 2-7 (i=1-6) = 14-19 Jalál = month 2, days 14-19
      ;; Days 8-12 (i=7-11) = 1-5 Jamál = month 3, days 1-5
      (let ((month (if (< i 7) 2 3))
            (day (if (< i 7) (+ i 13) (- i 6))))
        (when (setq h (holiday-bahai month day
                                     (format "%s Day of Ridvan" (aref ord i))))
          (push (car h) rid))))))

(autoload 'diary-list-entries-1 "diary-lib")

;;;###diary-autoload
(defun diary-bahai-list-entries ()
  "Add any Bahá’í date entries from the diary file to `diary-entries-list'.
Bahá’í date diary entries must be prefaced by `diary-bahai-entry-symbol'
\(normally a `B').  The same diary date forms govern the style of the
Bahá’í calendar entries, except that the Bahá’í month names cannot be
abbreviated.  The Bahá’í months are numbered from 1 to 19 with Bahá being
1 and 19 being `Alá.  If a Bahá’í date diary entry begins with
`diary-nonmarking-symbol', the entry will appear in the diary listing, but
will not be marked in the calendar.  This function is provided for use with
`diary-nongregorian-listing-hook'."
  (diary-list-entries-1 calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

(autoload 'calendar-mark-1 "diary-lib")

;;;###diary-autoload
(defun calendar-bahai-mark-date-pattern (month day year &optional color)
  "Mark dates in calendar window that conform to Bahá’í date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK."
  (calendar-mark-1 month day year 'calendar-bahai-from-absolute
                   'calendar-bahai-to-absolute color))

(autoload 'diary-mark-entries-1 "diary-lib")

;;;###diary-autoload
(defun diary-bahai-mark-entries ()
  "Mark days in the calendar window that have Bahá’í date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `diary-bahai-list-entries' for more information."
  (diary-mark-entries-1 'calendar-bahai-mark-date-pattern
                        calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

(autoload 'diary-insert-entry-1 "diary-lib")

;;;###cal-autoload
(defun diary-bahai-insert-entry (arg)
  "Insert a diary entry.
For the Bahá’í date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 nil arg calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

;;;###cal-autoload
(defun diary-bahai-insert-monthly-entry (arg)
  "Insert a monthly diary entry.
For the day of the Bahá’í month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'monthly arg calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

;;;###cal-autoload
(defun diary-bahai-insert-yearly-entry (arg)
  "Insert an annual diary entry.
For the day of the Bahá’í year corresponding to the date indicated by point.
Prefix argument ARG will make the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'yearly arg calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

;; The function below is designed to be used in sexp diary entries,
;; and may be present in users' diary files, so suppress the warning
;; about this prefix-less dynamic variable.  It's called from
;; `diary-list-sexp-entries', which binds the variable.
(with-suppressed-warnings ((lexical date))
  (defvar date))

;;;###diary-autoload
(defun diary-bahai-date ()
  "Bahá’í calendar equivalent of date diary entry."
  (format "Bahá’í date: %s" (calendar-bahai-date-string date)))

(provide 'cal-bahai)

;;; cal-bahai.el ends here
