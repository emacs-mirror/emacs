;;; battery-tests.el --- tests for battery.el -*- lexical-binding: t -*-

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

(require 'battery)

(ert-deftest battery-linux-proc-apm-regexp ()
  "Test `rx' definition `battery--linux-proc-apm'."
  (let ((str "1.16 1.2 0x07 0x01 0xff 0x80 -1% -1 ?"))
    (should (string-match (rx battery--linux-proc-apm) str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "1.16"))
    (should (equal (match-string 2 str) "1.2"))
    (should (equal (match-string 3 str) "07"))
    (should (equal (match-string 4 str) "01"))
    (should (equal (match-string 5 str) "ff"))
    (should (equal (match-string 6 str) "80"))
    (should (equal (match-string 7 str) "-1"))
    (should (equal (match-string 8 str) "-1"))
    (should (equal (match-string 9 str) "?")))
  (let ((str "1.16 1.2 0x03 0x00 0x00 0x01 99% 1792 min"))
    (should (string-match (rx battery--linux-proc-apm) str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "1.16"))
    (should (equal (match-string 2 str) "1.2"))
    (should (equal (match-string 3 str) "03"))
    (should (equal (match-string 4 str) "00"))
    (should (equal (match-string 5 str) "00"))
    (should (equal (match-string 6 str) "01"))
    (should (equal (match-string 7 str) "99"))
    (should (equal (match-string 8 str) "1792"))
    (should (equal (match-string 9 str) "min"))))

(ert-deftest battery-acpi-rate-regexp ()
  "Test `rx' definition `battery--acpi-rate'."
  (let ((str "01 mA"))
    (should (string-match (rx (battery--acpi-rate)) str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "01"))
    (should (equal (match-string 2 str) "mA")))
  (let ((str "23 mW"))
    (should (string-match (rx (battery--acpi-rate)) str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "23"))
    (should (equal (match-string 2 str) "mW")))
  (let ((str "23 mWh"))
    (should (string-match (rx (battery--acpi-rate)) str))
    (should (equal (match-string 0 str) "23 mW"))
    (should (equal (match-string 1 str) "23"))
    (should (equal (match-string 2 str) "mW")))
  (should-not (string-match (rx (battery--acpi-rate) eos) "45 mWh")))

(ert-deftest battery-acpi-capacity-regexp ()
  "Test `rx' definition `battery--acpi-capacity'."
  (let ((str "01 mAh"))
    (should (string-match (rx battery--acpi-capacity) str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "01"))
    (should (equal (match-string 2 str) "mAh")))
  (let ((str "23 mWh"))
    (should (string-match (rx battery--acpi-capacity) str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "23"))
    (should (equal (match-string 2 str) "mWh")))
  (should-not (string-match (rx battery--acpi-capacity eos) "45 mW")))

(ert-deftest battery-upower-state ()
  "Test `battery--upower-state'."
  ;; Charging.
  (dolist (total '(nil charging discharging empty fully-charged
                       pending-charge pending-discharge))
    (should (eq (battery--upower-state '(("State" . 1)) total) 'charging)))
  (dolist (state '(nil 0 1 2 3 4 5 6))
    (should (eq (battery--upower-state `(("State" . ,state)) 'charging)
                'charging)))
  ;; Discharging.
  (dolist (total '(nil discharging empty fully-charged
                       pending-charge pending-discharge))
    (should (eq (battery--upower-state '(("State" . 2)) total) 'discharging)))
  (dolist (state '(nil 0 2 3 4 5 6))
    (should (eq (battery--upower-state `(("State" . ,state)) 'discharging)
                'discharging)))
  ;; Pending charge.
  (dolist (total '(nil empty fully-charged pending-charge pending-discharge))
    (should (eq (battery--upower-state '(("State" . 5)) total)
                'pending-charge)))
  (dolist (state '(nil 0 3 4 5 6))
    (should (eq (battery--upower-state `(("State" . ,state)) 'pending-charge)
                'pending-charge)))
  ;; Pending discharge.
  (dolist (total '(nil empty fully-charged pending-discharge))
    (should (eq (battery--upower-state '(("State" . 6)) total)
                'pending-discharge)))
  (dolist (state '(nil 0 3 4 6))
    (should (eq (battery--upower-state `(("State" . ,state)) 'pending-discharge)
                'pending-discharge)))
  ;; Empty.
  (dolist (total '(nil empty))
    (should (eq (battery--upower-state '(("State" . 3)) total) 'empty)))
  (dolist (state '(nil 0 3))
    (should (eq (battery--upower-state `(("State" . ,state)) 'empty) 'empty)))
  ;; Fully charged.
  (dolist (total '(nil fully-charged))
    (should (eq (battery--upower-state '(("State" . 4)) total) 'fully-charged)))
  (dolist (state '(nil 0 4))
    (should (eq (battery--upower-state `(("State" . ,state)) 'fully-charged)
                'fully-charged))))

(ert-deftest battery-upower-state-unknown ()
  "Test `battery--upower-state' with unknown states."
  ;; Unknown running total retains new state.
  (should-not (battery--upower-state () nil))
  (should-not (battery--upower-state '(("State" . state)) nil))
  (should-not (battery--upower-state '(("State" . 0)) nil))
  (should (eq (battery--upower-state '(("State" . 1)) nil) 'charging))
  (should (eq (battery--upower-state '(("State" . 2)) nil) 'discharging))
  (should (eq (battery--upower-state '(("State" . 3)) nil) 'empty))
  (should (eq (battery--upower-state '(("State" . 4)) nil) 'fully-charged))
  (should (eq (battery--upower-state '(("State" . 5)) nil) 'pending-charge))
  (should (eq (battery--upower-state '(("State" . 6)) nil) 'pending-discharge))
  ;; Unknown new state retains running total.
  (dolist (props '(() (("State" . state)) (("State" . 0))))
    (dolist (total '(nil charging discharging empty fully-charged
                         pending-charge pending-discharge))
      (should (eq (battery--upower-state props total) total))))
  ;; Conflicting empty and fully-charged.
  (should-not (battery--upower-state '(("State" . 3)) 'fully-charged))
  (should-not (battery--upower-state '(("State" . 4)) 'empty)))

(ert-deftest battery-format ()
  "Test `battery-format'."
  (should (equal (battery-format "" ()) ""))
  (should (equal (battery-format "" '((?b . "-"))) ""))
  (should (equal (battery-format "%2a%-3b%.1p%%" '((?b . "-") (?p . "99")))
                 "-  9%")))

;;; battery-tests.el ends here
