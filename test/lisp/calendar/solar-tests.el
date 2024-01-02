;;; solar-tests.el --- tests for solar.el            -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'solar)

(ert-deftest solar-sunrise-sunset ()
  ;; Bug#44237: wrong sunrise time on Dec 30 and 31, 2020 for Jaipur.
  (let ((calendar-latitude 26.9)
        (calendar-longitude 75.8)
        (calendar-time-zone +330)
        (calendar-standard-time-zone-name "IST")
        ;; Make sure our clockwork isn't confused by daylight saving rules
        ;; in effect for any other time zone (bug#45818).
        (calendar-daylight-savings-starts nil)
        (epsilon (/ 60.0)))             ; Minute accuracy is good enough.
    (let* ((sunrise-sunset (solar-sunrise-sunset '(12 30 2020)))
           (sunrise (car (nth 0 sunrise-sunset)))
           (sunset (car (nth 1 sunrise-sunset))))
      (should (< (abs (- sunrise 7.27)) epsilon))
      (should (< (abs (- sunset 17.72)) epsilon)))
    (let* ((sunrise-sunset (solar-sunrise-sunset '(12 31 2020)))
           (sunrise (car (nth 0 sunrise-sunset)))
           (sunset (car (nth 1 sunrise-sunset))))
      (should (< (abs (- sunrise 7.28)) epsilon))
      (should (< (abs (- sunset 17.72)) epsilon)))))

(provide 'solar-tests)

;;; solar-tests.el ends here
