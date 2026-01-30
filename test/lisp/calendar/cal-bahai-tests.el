;;; cal-bahai-tests.el --- tests for the Bahá’í calendar. -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

;; The following code verifies the astronomical calculations against
;; official dates published by the Bahá’í World Centre.
;;
;; BACKGROUND: 2014 Calendar Reform
;; --------------------------------
;; On 10 July 2014, the Universal House of Justice announced provisions
;; for the uniform implementation of the Badí' calendar, effective from
;; Naw-Rúz 172 BE (March 2015).  The key provisions are:
;;
;; 1. NAW-RÚZ DETERMINATION:
;;    "The Festival of Naw-Rúz falleth on the day that the sun entereth
;;    the sign of Aries, even should this occur no more than one minute
;;    before sunset."  Tehran is the reference point for determining the
;;    moment of the vernal equinox.  If the equinox occurs before sunset
;;    in Tehran, that day is Naw-Rúz; otherwise, the following day is.
;;
;; 2. TWIN HOLY BIRTHDAYS:
;;    "They will now be observed on the first and the second day
;;    following the occurrence of the eighth new moon after Naw-Rúz,
;;    as determined in advance by astronomical tables using Ṭihrán as
;;    the point of reference."
;;
;; VERIFICATION APPROACH
;; ---------------------
;; The functions below compare calculated dates against official data
;; from the Bahá’í World Centre, covering the 50-year period from
;; 172 BE (2015 CE) to 221 BE (2064 CE).  This data was extracted from
;; the official ICS calendar file distributed by the Bahá’í World Centre.
;;
;; The verification confirms:
;; - Naw-Rúz dates: Calculated using `solar-equinoxes/solstices' for the
;;   vernal equinox and `solar-sunrise-sunset' for Tehran sunset times.
;; - Twin Holy Birthdays: Calculated using `lunar-new-moon-on-or-after'
;;   to find the eighth new moon after Naw-Rúz.

;;; Code:

(require 'ert)
(require 'cal-bahai)

(defconst calendar-bahai--nawruz-reference-dates
  '((2015 3 21) (2016 3 20) (2017 3 20) (2018 3 21) (2019 3 21)
    (2020 3 20) (2021 3 20) (2022 3 21) (2023 3 21) (2024 3 20)
    (2025 3 20) (2026 3 21) (2027 3 21) (2028 3 20) (2029 3 20)
    (2030 3 20) (2031 3 21) (2032 3 20) (2033 3 20) (2034 3 20)
    (2035 3 21) (2036 3 20) (2037 3 20) (2038 3 20) (2039 3 21)
    (2040 3 20) (2041 3 20) (2042 3 20) (2043 3 21) (2044 3 20)
    (2045 3 20) (2046 3 20) (2047 3 21) (2048 3 20) (2049 3 20)
    (2050 3 20) (2051 3 21) (2052 3 20) (2053 3 20) (2054 3 20)
    (2055 3 21) (2056 3 20) (2057 3 20) (2058 3 20) (2059 3 20)
    (2060 3 20) (2061 3 20) (2062 3 20) (2063 3 20) (2064 3 20))
  "Official Naw-Rúz dates from the Bahá’í World Centre (2015-2064).
Each entry is (GREGORIAN-YEAR MONTH DAY).  These dates are extracted
from the official ICS calendar file and serve as the authoritative
reference for verifying the astronomical calculations.

The dates show that Naw-Rúz falls on March 20 or 21, depending on
when the vernal equinox occurs relative to sunset in Tehran.")

(defconst calendar-bahai--twin-birthdays-reference-dates
  '(;; (GREG-YEAR BAB-MONTH BAB-DAY BAHA-MONTH BAHA-DAY)
    (2015 11 13 11 14) (2016 11  1 11  2) (2017 10 21 10 22)
    (2018 11  9 11 10) (2019 10 29 10 30) (2020 10 18 10 19)
    (2021 11  6 11  7) (2022 10 26 10 27) (2023 10 16 10 17)
    (2024 11  2 11  3) (2025 10 22 10 23) (2026 11 10 11 11)
    (2027 10 30 10 31) (2028 10 19 10 20) (2029 11  7 11  8)
    (2030 10 28 10 29) (2031 10 17 10 18) (2032 11  4 11  5)
    (2033 10 24 10 25) (2034 11 12 11 13) (2035 11  1 11  2)
    (2036 10 20 10 21) (2037 11  8 11  9) (2038 10 29 10 30)
    (2039 10 19 10 20) (2040 11  6 11  7) (2041 10 26 10 27)
    (2042 10 15 10 16) (2043 11  3 11  4) (2044 10 22 10 23)
    (2045 11 10 11 11) (2046 10 30 10 31) (2047 10 20 10 21)
    (2048 11  7 11  8) (2049 10 28 10 29) (2050 10 17 10 18)
    (2051 11  5 11  6) (2052 10 24 10 25) (2053 11 11 11 12)
    (2054 11  1 11  2) (2055 10 21 10 22) (2056 11  8 11  9)
    (2057 10 29 10 30) (2058 10 18 10 19) (2059 11  6 11  7)
    (2060 10 25 10 26) (2061 10 14 10 15) (2062 11  2 11  3)
    (2063 10 23 10 24) (2064 11 10 11 11))
  "Official Twin Holy Birthday dates from the Bahá’í World Centre (2015-2064).
Each entry is (GREGORIAN-YEAR BAB-MONTH BAB-DAY BAHA-MONTH BAHA-DAY).

The Birth of the Báb and the Birth of Bahá’u’lláh are celebrated on
consecutive days, determined by the eighth new moon after Naw-Rúz.
These dates move through the Gregorian calendar, typically falling
between mid-October and mid-November (Bahá’í months of Mashíyyat,
\\='Ilm, and Qudrat).")

(ert-deftest calendar-bahai-verify-nawruz ()
  "Verify Naw-Rúz calculations against official reference dates."
  (pcase-dolist (`(,greg-year ,expected-month ,expected-day)
                 calendar-bahai--nawruz-reference-dates)
    (let* ((expected (list expected-month expected-day greg-year))
           (computed (calendar-bahai-nawruz-for-gregorian-year greg-year)))
      (should (equal computed expected)))))

(ert-deftest calendar-bahai-verify-twin-birthdays ()
  "Verify Twin Holy Birthday calculations against official reference dates."
  (pcase-dolist (`(,greg-year ,bab-month ,bab-day ,baha-month ,baha-day)
                 calendar-bahai--twin-birthdays-reference-dates)
    (let* ((bahai-year (- greg-year (1- 1844)))
           (expected-bab (list bab-month bab-day greg-year))
           (expected-baha (list baha-month baha-day greg-year)))
      ;; Only verify from reform year onwards
      (when (>= bahai-year calendar-bahai-reform-year)
        (pcase-let* ((`(,computed-bab ,computed-baha)
                      (calendar-bahai-twin-holy-birthdays-for-year bahai-year)))
          (should (equal computed-bab expected-bab))
          (should (equal computed-baha expected-baha)))))))

(provide 'cal-bahai-tests)
;;; cal-bahai-tests.el ends here
