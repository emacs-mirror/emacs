;;; holidays.el --- holiday functions for the calendar package

;;; Copyright (C) 1989, 1990, 1992 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: holidays, calendar

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

(autoload 'calendar-holiday-function-solar-equinoxes-solstices "solar"
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
  (let* ((date (or (calendar-cursor-to-date)
                   (error "Cursor is not on a date!")))
         (date-string (calendar-date-string date))
         (holiday-list (check-calendar-holidays date))
         (holiday-string (mapconcat 'identity holiday-list ";  "))
         (msg (format "%s:  %s" date-string holiday-string)))
    (if (not holiday-list)
        (message "No holidays known for %s" date-string)
      (if (<= (length msg) (screen-width))
          (message msg)
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
            (format "Notable Dates from %s, %d to %s, %d%%-"
                    (calendar-month-name m1) y1 (calendar-month-name m2) y2))
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
      (let* ((function-name
              (intern (format "calendar-holiday-function-%s" (car (car p)))))
             (holidays
	      (condition-case nil
		  (if (cdr (car p));; optional arguments
		      (funcall function-name (cdr (car p)))
		    (funcall function-name))
		(error
		 (beep)
		 (message "Bad holiday list item: %s" (car p))
		 (sleep-for 2)))))
        (if holidays
            (setq holiday-list (append holidays holiday-list))))
      (setq p (cdr p)))
    (setq holiday-list (sort holiday-list 'calendar-date-compare))))

;; Below are the functions that calculate the dates of holidays; these
;; are called by the funcall in the function calendar-holiday-list.  If you
;; write other such functions, be sure to imitate the style used below,
;; including the evaluation of each element in the list that constitutes
;; the argument to the function.  If you don't do this evaluation, the
;; list calendar-holidays cannot contain expressions (as, for example, in
;; the entry for the Islamic new year.)  Also remember that each function
;; must return a list of items of the form ((month day year) string);
;; the date (month day year) should be visible in the calendar window.

(defun calendar-holiday-function-fixed (x)
  "Returns the corresponding Gregorian date, if visible in the window, to
(month day) where month is (car X) and day is (car (cdr X)).  If it is
visible, the value returned is the list (((month day year) string)) where
string is (car (nthcdr 2 X)).  Returns nil if it is not visible in the
current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x))))
         (m displayed-month)
         (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (list month day y) string)))))

(defun calendar-holiday-function-float (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
n-th occurrence (negative counts from the end of the month) of dayname in
month where month is (car X), dayname is (car (cdr X)), and n is
(car (nthcdr 2 X)).  If it is visible, the value returned is the list
(((month day year) string)) where string is (car (nthcdr 3 X)).
Returns nil if it is not visible in the current calendar window."
  (let* ((month (eval (car x)))
         (dayname (eval (car (cdr x))))
         (n (eval (car (nthcdr 2 x))))
         (string (eval (car (nthcdr 3 x))))
         (m displayed-month)
         (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (calendar-nth-named-day n dayname month y) string)))))

(defun calendar-holiday-function-julian (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
Julian date (month day) where month is (car X) and day is (car (cdr X)).
If it is visible, the value returned is the list (((month day year) string))
where string is (car (nthcdr 2 X)).  Returns nil if it is not visible in the
current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x))))
         (m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         (year))
    (increment-calendar-month m1 y1 -1)
    (increment-calendar-month m2 y2 1)
    (let* ((start-date (calendar-absolute-from-gregorian
                        (list m1 1 y1)))
           (end-date (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2)))
           (julian-start (calendar-julian-from-absolute start-date))
           (julian-end (calendar-julian-from-absolute end-date))
           (julian-y1 (extract-calendar-year julian-start))
           (julian-y2 (extract-calendar-year julian-end)))
      (setq year (if (< 10 month) julian-y1 julian-y2))
      (let ((date (calendar-gregorian-from-absolute
                   (calendar-absolute-from-julian
                    (list month day year)))))
        (if (calendar-date-is-visible-p date)
            (list (list date string)))))))

(defun calendar-holiday-function-islamic (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
Islamic date (month day) where month is (car X) and day is (car (cdr X)).
If it is visible, the value returned is the list (((month day year) string))
where string is (car (nthcdr 2 X)).  Returns nil if it is not visible in
the current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x))))
         (islamic-date (calendar-islamic-from-absolute
                        (calendar-absolute-from-gregorian
                         (list displayed-month 15 displayed-year))))
         (m (extract-calendar-month islamic-date))
         (y (extract-calendar-year islamic-date))
        (date))
    (if (< m 1)
        nil;;   Islamic calendar doesn't apply.
      (increment-calendar-month m y (- 10 month))
      (if (> m 7);;  Islamic date might be visible
          (let ((date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic (list month day y)))))
            (if (calendar-date-is-visible-p date)
                (list (list date string))))))))

(defun calendar-holiday-function-hebrew (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
Hebrew date (month day) where month is (car X) and day is (car (cdr X)).
If it is visible, the value returned is the list (((month day year) string))
where string is (car (nthcdr 2 X)).  Returns nil if it is not visible in
the current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x)))))
    (if (memq displayed-month;;  This test is only to speed things up a bit;
              (list          ;;  it works fine without the test too.
               (if (< 11 month) (- month 11) (+ month 1))
               (if (< 10 month) (- month 10) (+ month 2))
               (if (<  9 month) (- month  9) (+ month 3))
               (if (<  8 month) (- month  8) (+ month 4))
               (if (<  7 month) (- month  7) (+ month 5))))
        (let ((m1 displayed-month)
              (y1 displayed-year)
              (m2 displayed-month)
              (y2 displayed-year)
              (year))
          (increment-calendar-month m1 y1 -1)
          (increment-calendar-month m2 y2 1)
          (let* ((start-date (calendar-absolute-from-gregorian
                              (list m1 1 y1)))
                 (end-date (calendar-absolute-from-gregorian
                            (list m2 (calendar-last-day-of-month m2 y2) y2)))
                 (hebrew-start (calendar-hebrew-from-absolute start-date))
                 (hebrew-end (calendar-hebrew-from-absolute end-date))
                 (hebrew-y1 (extract-calendar-year hebrew-start))
                 (hebrew-y2 (extract-calendar-year hebrew-end)))
            (setq year (if (< 6 month) hebrew-y2 hebrew-y1))
            (let ((date (calendar-gregorian-from-absolute
                         (calendar-absolute-from-hebrew
                          (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (list (list date string)))))))))

(defun calendar-holiday-function-if (x)
  "Conditional holiday for dates in the calendar window.
The boolean condition is (car X).  If t, the holiday (car (cdr X)) is
checked.  If nil, the holiday (car (cdr (cdr X))), if there, is checked."
  (let* ((boolean (eval (car x)))
         (h (if boolean (car (cdr x)) (car (cdr (cdr x))))))
    (if h
     (let* ((function-name
              (intern (format "calendar-holiday-function-%s" (car h))))
            (holidays
              (if (cdr h);; optional arguments
                  (funcall function-name (cdr h))
                (funcall function-name))))
       holidays))))

(defun calendar-holiday-function-sexp (x)
  "Sexp holiday for dates in the calendar window.
The sexp (in `year') is (car X).  If the sexp evals to a date visible in the
calendar window, the holiday (car (cdr X)) is on that date.  If the sexp evals
to nil, or if the date is not visible, there is no holiday."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y -1)
    (filter-visible-calendar-holidays
     (append
      (let ((year y))
        (list (list (eval (car x)) (eval (car (cdr x))))))
      (let ((year (1+ y)))
        (list (list (eval (car x)) (eval (car (cdr x))))))))))

(defun calendar-holiday-function-advent ()
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

(defun calendar-holiday-function-easter-etc ()
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
                  "Ascension Sunday")
            (list (calendar-gregorian-from-absolute (+ abs-easter 49))
                  "Pentecost (Whitsunday)")
            (list (calendar-gregorian-from-absolute (+ abs-easter 50))
                  "Whitmunday")
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

(defun calendar-holiday-function-greek-orthodox-easter ()
  "Date of Easter according to the rule of the Council of Nicaea, if visible
in the calendar window."
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

(defun calendar-holiday-function-rosh-hashanah-etc ()
  "List of dates related to Rosh Hashanah, as visible in calendar window."
  (if (or (< displayed-month 8)
          (> displayed-month 11))
      nil;; None of the dates is visible
    (let* ((abs-r-h (calendar-absolute-from-hebrew
                      (list 7 1 (+ displayed-year 3761))))
            (mandatory
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
                    "Simchat Torah")))
           (optional
            (list 
             (list (calendar-gregorian-from-absolute
                    (calendar-dayname-on-or-before 6 (- abs-r-h 4)))
                   "Selichot (night)")
             (list (calendar-gregorian-from-absolute (1- abs-r-h))
                   "Erev Rosh HaShannah")
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
                   "Hoshannah Rabbah")))
            (output-list
             (filter-visible-calendar-holidays mandatory)))
      (if all-hebrew-calendar-holidays
          (setq output-list
                (append 
                 (filter-visible-calendar-holidays optional)
                 output-list)))
      output-list)))

(defun calendar-holiday-function-hanukkah ()
  "List of dates related to Hanukkah, as visible in calendar window."
    (if (memq displayed-month;;  This test is only to speed things up a bit;
              '(10 11 12 1 2));; it works fine without the test too.
        (let ((m displayed-month)
              (y displayed-year))
          (increment-calendar-month m y 1)
          (let* ((h-y (extract-calendar-year
                         (calendar-hebrew-from-absolute
                          (calendar-absolute-from-gregorian
                           (list m (calendar-last-day-of-month m y) y)))))
                 (abs-h (calendar-absolute-from-hebrew (list 9 25 h-y))))
            (filter-visible-calendar-holidays
             (list
              (list (calendar-gregorian-from-absolute (1- abs-h))
                    "Erev Hanukkah")
              (list (calendar-gregorian-from-absolute abs-h)
                    "Hanukkah (first day)")
              (list (calendar-gregorian-from-absolute (1+ abs-h))
                    "Hanukkah (second day)")
              (list (calendar-gregorian-from-absolute (+ abs-h 2))
                    "Hanukkah (third day)")
              (list (calendar-gregorian-from-absolute (+ abs-h 3))
                    "Hanukkah (fourth day)")
              (list (calendar-gregorian-from-absolute (+ abs-h 4))
                    "Hanukkah (fifth day)")
              (list (calendar-gregorian-from-absolute (+ abs-h 5))
                    "Hanukkah (sixth day)")
              (list (calendar-gregorian-from-absolute (+ abs-h 6))
                    "Hanukkah (seventh day)")
              (list (calendar-gregorian-from-absolute (+ abs-h 7))
                    "Hanukkah (eighth day)")))))))

(defun calendar-holiday-function-passover-etc ()
  "List of dates related to Passover, as visible in calendar window."
 (if (< 7 displayed-month)
      nil;; None of the dates is visible
    (let* ((abs-p (calendar-absolute-from-hebrew
                      (list 1 15 (+ displayed-year 3760))))
           (mandatory
            (list
             (list (calendar-gregorian-from-absolute abs-p)
                   "Passover")
             (list (calendar-gregorian-from-absolute (+ abs-p 50))
                   "Shavuot")))
           (optional
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
             (list (calendar-gregorian-from-absolute (+ abs-p 12))
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
                   "Yom Yerushalim")
             (list (calendar-gregorian-from-absolute (+ abs-p 49))
                   "Erev Shavuot")
             (list (calendar-gregorian-from-absolute (+ abs-p 51))
                   "Shavuot (second day)")))
           (output-list
             (filter-visible-calendar-holidays mandatory)))
      (if all-hebrew-calendar-holidays
          (setq output-list
                (append 
                 (filter-visible-calendar-holidays optional)
                 output-list)))
      output-list)))

(defun calendar-holiday-function-tisha-b-av-etc ()
  "List of dates around Tisha B'Av, as visible in calendar window."
  (if (or (< displayed-month 5)
          (> displayed-month 9))
      nil;; None of the dates is visible
    (let* ((abs-t-a (calendar-absolute-from-hebrew
                      (list 5 9 (+ displayed-year 3760)))))

      (filter-visible-calendar-holidays
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
