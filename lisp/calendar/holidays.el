;;; holidays.el --- holiday functions for the calendar package

;; Copyright (C) 1989, 1990, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: holidays, calendar

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This collection of functions implements the holiday features as described
;; in calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations'' by Nachum Dershowitz and Edward M. Reingold,
;; Software--Practice and Experience, Volume 20, Number 9 (September, 1990),
;; pages 899-928.  ``Calendrical Calculations, Part II: Three Historical
;; Calendars'' by E. M. Reingold,  N. Dershowitz, and S. M. Clamen,
;; Software--Practice and Experience, Volume 23, Number 4 (April, 1993),
;; pages 383-404.

;; Hard copies of these two papers can be obtained by sending email to
;; reingold@cs.uiuc.edu with the SUBJECT "send-paper-cal" (no quotes) and
;; the message BODY containing your mailing address (snail).

;;; Code:

(require 'calendar)

(autoload 'holiday-julian "cal-julian"
  "Holiday on MONTH, DAY  (Julian) called STRING."
  t)

(autoload 'holiday-hebrew "cal-hebrew"
  "Holiday on MONTH, DAY (Hebrew) called STRING."
  t)

(autoload 'holiday-rosh-hashanah-etc "cal-hebrew"
  "List of dates related to Rosh Hashanah, as visible in calendar window."
  t)

(autoload 'holiday-hanukkah "cal-hebrew"
  "List of dates related to Hanukkah, as visible in calendar window."
  t)

(autoload 'holiday-passover-etc "cal-hebrew"
  "List of dates related to Passover, as visible in calendar window."
  t)

(autoload 'holiday-tisha-b-av-etc "cal-hebrew"
  "List of dates around Tisha B'Av, as visible in calendar window."
  t)

(autoload 'holiday-islamic "cal-islam"
  "Holiday on MONTH, DAY (Islamic) called STRING."
  t)

(autoload 'holiday-chinese-new-year "cal-china"
  "Date of Chinese New Year."
  t)

(autoload 'solar-equinoxes-solstices "solar"
  "Date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point."
  t)

(defun holidays (&optional arg)
  "Display the holidays for last month, this month, and next month.
If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file."
  (interactive "P")
  (save-excursion
    (let* ((completion-ignore-case t)
           (date (if arg
                     (calendar-read-date t)
                   (calendar-current-date)))
           (displayed-month (extract-calendar-month date))
           (displayed-year (extract-calendar-year date)))
      (list-calendar-holidays))))

(defun check-calendar-holidays (date)
  "Check the list of holidays for any that occur on DATE.
The value returned is a list of strings of relevant holiday descriptions.
The holidays are those in the list calendar-holidays."
  (let* ((displayed-month (extract-calendar-month date))
         (displayed-year (extract-calendar-year date))
         (h (calendar-holiday-list))
         (holiday-list))
    (while h
      (if (calendar-date-equal date (car (car h)))
          (setq holiday-list (append holiday-list (cdr (car h)))))
      (setq h (cdr h)))
    holiday-list))

(defun calendar-cursor-holidays ()
  "Find holidays for the date specified by the cursor in the calendar window."
  (interactive)
  (message "Checking holidays...")
  (let* ((date (calendar-cursor-to-date t))
         (date-string (calendar-date-string date))
         (holiday-list (check-calendar-holidays date))
         (holiday-string (mapconcat 'identity holiday-list ";  "))
         (msg (format "%s:  %s" date-string holiday-string)))
    (if (not holiday-list)
        (message "No holidays known for %s" date-string)
      (if (<= (length msg) (frame-width))
          (message "%s" msg)
        (set-buffer (get-buffer-create holiday-buffer))
        (setq buffer-read-only nil)
        (calendar-set-mode-line date-string)
        (erase-buffer)
        (insert (mapconcat 'identity holiday-list "\n"))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (display-buffer holiday-buffer)
        (message "Checking holidays...done")))))

(defun mark-calendar-holidays ()
  "Mark notable days in the calendar window."
  (interactive)
  (setq mark-holidays-in-calendar t)
  (message "Marking holidays...")
  (let ((holiday-list (calendar-holiday-list)))
    (while holiday-list
      (mark-visible-calendar-date
       (car (car holiday-list)) calendar-holiday-marker)
      (setq holiday-list (cdr holiday-list))))
  (message "Marking holidays...done"))

(defun list-calendar-holidays ()
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list calendar-notable-days.  Returns t if any
holidays are found, nil if not."
  (interactive)
  (message "Looking up holidays...")
  (let ((holiday-list (calendar-holiday-list))
        (m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year))
    (if (not holiday-list)
        (progn
          (message "Looking up holidays...none found")
          nil)
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (increment-calendar-month m1 y1 -1)
      (increment-calendar-month m2 y2 1)
      (calendar-set-mode-line
       (if (= y1 y2)
           (format "Notable Dates from %s to %s, %d%%-"
                   (calendar-month-name m1) (calendar-month-name m2) y2)
         (format "Notable Dates from %s, %d to %s, %d%%-"
                 (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
      (erase-buffer)
      (insert
       (mapconcat
        '(lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Looking up holidays...done")
      t)))

(defun calendar-holiday-list ()
  "Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list calendar-holidays."
  (let ((p calendar-holidays)
        (holiday-list))
    (while p
      (let* ((holidays
              (if calendar-debug-sexp
                  (let ((stack-trace-on-error t))
                    (eval (car p)))
                (condition-case nil
                    (eval (car p))
                  (error (beep)
                         (message "Bad holiday list item: %s" (car p))
                         (sleep-for 2))))))
        (if holidays
            (setq holiday-list (append holidays holiday-list))))
      (setq p (cdr p)))
    (setq holiday-list (sort holiday-list 'calendar-date-compare))))

;; Below are the functions that calculate the dates of holidays; these
;; are eval'ed in the function calendar-holiday-list.  If you
;; write other such functions, be sure to imitate the style used below.
;; Remember that each function must return a list of items of the form
;; ((month day year) string) of VISIBLE dates in the calendar window.

(defun holiday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (list month day y) string)))))

(defun holiday-float (month dayname n string &optional day)
  "Holiday on MONTH, DAYNAME (Nth occurrence, Gregorian) called STRING.
If the Nth DAYNAME in MONTH is visible, the value returned is the list
\(((MONTH DAY year) STRING)).

If N<0, count backward from the end of MONTH.

An optional parameter DAY means the Nth DAYNAME after/before MONTH DAY.

Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (calendar-nth-named-day n dayname month y day) string)))))

(defun holiday-sexp (sexp string)
  "Sexp holiday for dates in the calendar window.
SEXP is an expression in variable `year' evaluates to `date'.

STRING is an expression in `date' that evaluates to the holiday description
of `date'.

If `date' is visible in the calendar window, the holiday STRING is on that
date.  If date is nil, or if the date is not visible, there is no holiday."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y -1)
    (filter-visible-calendar-holidays
     (append
      (let* ((year y)
             (date (eval sexp))
             (string (if date (eval string))))
        (list (list date string)))
      (let* ((year (1+ y))
             (date (eval sexp))
             (string (if date (eval string))))
        (list (list date string)))))))

(defun holiday-advent ()
  "Date of Advent, if visible in calendar window."
  (let ((year displayed-year)
        (month displayed-month))
    (increment-calendar-month month year -1)
    (let ((advent (calendar-gregorian-from-absolute
                   (calendar-dayname-on-or-before 0
                    (calendar-absolute-from-gregorian
                     (list 12 3 year))))))
      (if (calendar-date-is-visible-p advent)
          (list (list advent "Advent"))))))

(defun holiday-easter-etc ()
  "List of dates related to Easter, as visible in calendar window."
 (if (and (> displayed-month 5) (not all-christian-calendar-holidays))
     nil;; Ash Wednesday, Good Friday, and Easter are not visible.
   (let* ((century (1+ (/ displayed-year 100)))
          (shifted-epact        ;; Age of moon for April 5...
           (% (+ 14 (* 11 (% displayed-year 19));;     ...by Nicaean rule
                 (-           ;; ...corrected for the Gregorian century rule
                  (/ (* 3 century) 4))
                 (/    ;; ...corrected for Metonic cycle inaccuracy.
                  (+ 5 (* 8 century)) 25)
                 (* 30 century));;              Keeps value positive.
              30))
          (adjusted-epact       ;;  Adjust for 29.5 day month.
           (if (or (= shifted-epact 0)
                   (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
               (1+ shifted-epact)
             shifted-epact))
          (paschal-moon       ;; Day after the full moon on or after March 21.
           (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
              adjusted-epact))
          (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))
          (mandatory
           (list
            (list (calendar-gregorian-from-absolute abs-easter)
                  "Easter Sunday")
            (list (calendar-gregorian-from-absolute (- abs-easter 2))
                  "Good Friday")
            (list (calendar-gregorian-from-absolute (- abs-easter 46))
                  "Ash Wednesday")))
          (optional
           (list
            (list (calendar-gregorian-from-absolute (- abs-easter 63))
                  "Septuagesima Sunday")
            (list (calendar-gregorian-from-absolute (- abs-easter 56))
                  "Sexagesima Sunday")
            (list (calendar-gregorian-from-absolute (- abs-easter 49))
                  "Shrove Sunday")
            (list (calendar-gregorian-from-absolute (- abs-easter 48))
                  "Shrove Monday")
            (list (calendar-gregorian-from-absolute (- abs-easter 47))
                  "Shrove Tuesday")
            (list (calendar-gregorian-from-absolute (- abs-easter 14))
                  "Passion Sunday")
            (list (calendar-gregorian-from-absolute (- abs-easter 7))
                  "Palm Sunday")
            (list (calendar-gregorian-from-absolute (- abs-easter 3))
                  "Maundy Thursday")
            (list (calendar-gregorian-from-absolute (+ abs-easter 35))
                  "Rogation Sunday")
            (list (calendar-gregorian-from-absolute (+ abs-easter 39))
                  "Ascension Day")
            (list (calendar-gregorian-from-absolute (+ abs-easter 49))
                  "Pentecost (Whitsunday)")
            (list (calendar-gregorian-from-absolute (+ abs-easter 50))
                  "Whitmonday")
            (list (calendar-gregorian-from-absolute (+ abs-easter 56))
                  "Trinity Sunday")
            (list (calendar-gregorian-from-absolute (+ abs-easter 60))
                  "Corpus Christi")))
          (output-list
           (filter-visible-calendar-holidays mandatory)))
     (if all-christian-calendar-holidays
         (setq output-list
               (append 
                (filter-visible-calendar-holidays optional)
                output-list)))
     output-list)))

(defun holiday-greek-orthodox-easter ()
  "Date of Easter according to the rule of the Council of Nicaea."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (let* ((julian-year
            (extract-calendar-year
             (calendar-julian-from-absolute
              (calendar-absolute-from-gregorian
               (list m (calendar-last-day-of-month m y) y)))))
           (shifted-epact ;; Age of moon for April 5.
            (% (+ 14
                  (* 11 (% julian-year 19)))
               30))
           (paschal-moon  ;; Day after full moon on or after March 21.
            (- (calendar-absolute-from-julian (list 4 19 julian-year))
               shifted-epact))
           (nicaean-easter;; Sunday following the Paschal moon
            (calendar-gregorian-from-absolute
             (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))))
      (if (calendar-date-is-visible-p nicaean-easter)
          (list (list nicaean-easter "Pascha (Greek Orthodox Easter)"))))))

(defun filter-visible-calendar-holidays (l)
  "Return a list of all visible holidays of those on L."
  (let ((visible)
        (p l))
    (while p
      (and (car (car p))
           (calendar-date-is-visible-p (car (car p)))
           (setq visible (append (list (car p)) visible)))
      (setq p (cdr p)))
    visible))

(provide 'holidays)

;;; holidays.el ends here
