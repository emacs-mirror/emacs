;;; lunar.el --- calendar functions for phases of the moon.

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: moon, lunar phases, calendar, diary

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

;; This collection of functions implements lunar phases for calendar.el and
;; diary.el.

;; Based on ``Astronomical Formulae for Calculators,'' 3rd ed., by Jean Meeus,
;; Willmann-Bell, Inc., 1985.
;;
;; WARNING: The calculations will be accurate only to within a few minutes.

;; The author would be delighted to have an astronomically more sophisticated
;; person rewrite the code for the lunar calculations in this file!

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(if (fboundp 'atan)
    (require 'lisp-float-type)
  (error "Lunar calculations impossible since floating point is unavailable."))

(require 'solar)

(defun lunar-phase-list (month year)
  "List of lunar phases for three months starting with Gregorian MONTH, YEAR."
  (let ((end-month month)
        (end-year year)
        (start-month month)
        (start-year year))
    (increment-calendar-month end-month end-year 3)
    (increment-calendar-month start-month start-year -1)
    (let* ((end-date (list (list end-month 1 end-year)))
           (start-date (list (list start-month 
                                   (calendar-last-day-of-month
                                    start-month start-year)
                                   start-year)))
           (index (* 4
                     (truncate
                      (* 12.3685
                         (+ year
                            ( / (calendar-day-number (list month 1 year))
                                366.0)
                            -1900)))))
           (new-moon (lunar-phase index))
           (list))
      (while (calendar-date-compare new-moon end-date)
        (if (calendar-date-compare start-date new-moon)
            (setq list (append list (list new-moon))))
        (setq index (1+ index))
        (setq new-moon (lunar-phase index)))
      list)))

(defun lunar-phase (index)
  "Local date and time of lunar phase INDEX.
Integer below INDEX/4 gives the lunation number, counting from Jan 1, 1900;
remainder mod 4 gives the phase: 0 new moon, 1 first quarter, 2 full moon,
3 last quarter."
  (let* ((phase (mod index 4))
         (index (/ index 4.0))
         (time (/ index 1236.85))
         (date (+ (calendar-absolute-from-gregorian '(1 0.5 1900))
                  0.75933
                  (* 29.53058868 index)
                  (* 0.0001178 time time)
                  (* -0.000000155 time time time)
                  (* 0.00033
                     (solar-sin-degrees (+ 166.56
                                           (* 132.87 time)
                                           (* -0.009173 time time))))))
         (sun-anomaly (mod
                       (+ 359.2242
                          (* 29.105356 index)
                          (* -0.0000333 time time)
                          (* -0.00000347 time time time))
                       360.0))
         (moon-anomaly (mod
                        (+ 306.0253
                           (* 385.81691806 index)
                           (* 0.0107306 time time)
                           (* 0.00001236 time time time))
                        360.0))
         (moon-lat (mod
                    (+ 21.2964
                       (* 390.67050646 index)
                       (* -0.0016528 time time)
                       (* -0.00000239 time time time))
                    360.0))
         (adjustment
          (if (memq phase '(0 2))
              (+ (* (- 0.1734 (* 0.000393 time))
                    (solar-sin-degrees sun-anomaly))
                 (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
                 (* -0.4068 (solar-sin-degrees moon-anomaly))
                 (* 0.0161 (solar-sin-degrees (* 2 moon-anomaly)))
                 (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
                 (* 0.0104 (solar-sin-degrees (* 2 moon-lat)))
                 (* -0.0051 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
                 (* -0.0074 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
                 (* 0.0004 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
                 (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
                 (* -0.0006 (solar-sin-degrees
                             (+ (* 2 moon-lat) moon-anomaly)))
                 (* 0.0010 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
                 (* 0.0005 (solar-sin-degrees
                            (+ (* 2 moon-anomaly) sun-anomaly))))
            (+ (* (- 0.1721 (* 0.0004 time))
                  (solar-sin-degrees sun-anomaly))
               (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
               (* -0.6280 (solar-sin-degrees moon-anomaly))
               (* 0.0089 (solar-sin-degrees (* 2 moon-anomaly)))
               (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
               (* 0.0079 (solar-sin-degrees (* 2 moon-lat)))
               (* -0.0119 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
               (* -0.0047 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
               (* 0.0003 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
               (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
               (* -0.0006 (solar-sin-degrees (+ (* 2 moon-lat) moon-anomaly)))
               (* 0.0021 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
               (* 0.0003 (solar-sin-degrees
                          (+ (* 2 moon-anomaly) sun-anomaly)))
               (* 0.0004 (solar-sin-degrees
                          (- sun-anomaly (* 2 moon-anomaly))))
               (* -0.0003 (solar-sin-degrees
                          (+ (* 2 sun-anomaly) moon-anomaly))))))
         (adj (+ 0.0028
                 (* -0.0004 (solar-cosine-degrees
                             sun-anomaly))
                 (* 0.0003 (solar-cosine-degrees
                            moon-anomaly))))
         (adjustment (cond ((= phase 1) (+ adjustment adj))
                           ((= phase 2) (- adjustment adj))
                           (t adjustment)))
         (date (+ date adjustment))
	 (date (+ date (/ (- calendar-time-zone
			     (solar-ephemeris-correction
                              (extract-calendar-year
                               (calendar-gregorian-from-absolute
                                (truncate date)))))
			  60.0 24.0)))
         (time (* 24 (- date (truncate date))))
	 (date (calendar-gregorian-from-absolute (truncate date)))
         (adj (solar-adj-time-for-dst date time)))
    (list (car adj) (apply 'solar-time-string (cdr adj)) phase)))

(defun lunar-phase-name (phase)
  "Name of lunar PHASE.
0 = new moon, 1 = first quarter, 2 = full moon, 3 = last quarter."
  (cond ((= 0 phase) "New Moon")
        ((= 1 phase) "First Quarter Moon")
        ((= 2 phase) "Full Moon")
        ((= 3 phase) "Last Quarter Moon")))

(defun calendar-phases-of-moon ()
  "Create a buffer with the lunar phases for the current calendar window."
  (interactive)
  (message "Computing phases of the moon...")
  (let ((m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year)
        (lunar-phases-buffer "*Phases of Moon*"))
    (increment-calendar-month m1 y1 -1)
    (increment-calendar-month m2 y2 1)
    (set-buffer (get-buffer-create lunar-phases-buffer))
    (setq buffer-read-only nil)
    (calendar-set-mode-line
     (if (= y1 y2)
         (format "Phases of the Moon from %s to %s, %d%%-"
                 (calendar-month-name m1) (calendar-month-name m2) y2)
       (format "Phases of the Moon from %s, %d to %s, %d%%-"
               (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
    (erase-buffer)
    (insert
     (mapconcat
      '(lambda (x)
         (let ((date (car x))
               (time (car (cdr x)))
               (phase (car (cdr (cdr x)))))
           (concat (calendar-date-string date)
                   ": "
                   (lunar-phase-name phase)
                   " "
                   time)))
      (lunar-phase-list m1 y1) "\n"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (display-buffer lunar-phases-buffer)
    (message "Computing phases of the moon...done")))

;;;###autoload
(defun phases-of-moon (&optional arg)
  "Display the quarters of the moon for last month, this month, and next month.
If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file."
  (interactive "P")
  (save-excursion
    (let* ((completion-ignore-case t)
           (date (calendar-current-date))
           (displayed-month
            (if arg
                (cdr (assoc
                      (capitalize
                       (completing-read
                        "Month name: "
                        (mapcar 'list (append calendar-month-name-array nil))
                        nil t))
                      (calendar-make-alist calendar-month-name-array)))
              (extract-calendar-month date)))
           (displayed-year
            (if arg
                (calendar-read
                 "Year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year (calendar-current-date))))
              (extract-calendar-year date))))
      (calendar-phases-of-moon))))

(defun diary-phases-of-moon ()
  "Moon phases diary entry."
  (let* ((index (* 4
                   (truncate
                    (* 12.3685
                       (+ (extract-calendar-year date)
                          ( / (calendar-day-number date)
                              366.0)
                          -1900)))))
         (phase (lunar-phase index)))
    (while (calendar-date-compare phase (list date))
      (setq index (1+ index))
      (setq phase (lunar-phase index)))
    (if (calendar-date-equal (car phase) date)
        (concat (lunar-phase-name (car (cdr (cdr phase)))) " "
                (car (cdr phase))))))

(provide 'lunar)

;;; lunar.el ends here
