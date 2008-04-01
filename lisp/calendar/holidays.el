;;; holidays.el --- holiday functions for the calendar package

;; Copyright (C) 1989, 1990, 1992, 1993, 1994, 1997, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: holidays, calendar

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

(eval-and-compile
  (load "hol-loaddefs" nil 'quiet))

;;;###diary-autoload
(defun calendar-holiday-list ()
  "Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list `calendar-holidays'."
  (sort (delq nil
              (mapcar (lambda (p)
                        (car
                         (if calendar-debug-sexp
                             (let ((stack-trace-on-error t))
                               (eval p))
                           (condition-case nil
                               (eval p)
                             (error (beep)
                                    (message "Bad holiday list item: %s" p)
                                    (sleep-for 2))))))
                      calendar-holidays))
        'calendar-date-compare))

(defvar displayed-month)                ; from generate-calendar
(defvar displayed-year)

;;;###cal-autoload
(defun calendar-list-holidays ()
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.
Returns non-nil if any holidays are found."
  (interactive)
  (message "Looking up holidays...")
  (let ((holiday-list (calendar-holiday-list))
        (m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year))
    (if (not holiday-list)
        (message "Looking up holidays...none found")
      (calendar-in-read-only-buffer holiday-buffer
        (increment-calendar-month m1 y1 -1)
        (increment-calendar-month m2 y2 1)
        (calendar-set-mode-line
         (if (= y1 y2)
             (format "Notable Dates from %s to %s, %d%%-"
                     (calendar-month-name m1) (calendar-month-name m2) y2)
           (format "Notable Dates from %s, %d to %s, %d%%-"
                   (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
        (insert
         (mapconcat
          (lambda (x) (concat (calendar-date-string (car x))
                              ": " (cadr x)))
          holiday-list "\n")))
      (message "Looking up holidays...done"))
    holiday-list))

(define-obsolete-function-alias
  'list-calendar-holidays 'calendar-list-holidays "23.1")

;;;###autoload
(defun holidays (&optional arg)
  "Display the holidays for last month, this month, and next month.
If called with an optional prefix argument ARG, prompts for month and year.
This function is suitable for execution in a .emacs file."
  (interactive "P")
  (save-excursion
    (let* ((completion-ignore-case t)
           (date (if arg (calendar-read-date t)
                   (calendar-current-date)))
           (displayed-month (extract-calendar-month date))
           (displayed-year (extract-calendar-year date)))
      (calendar-list-holidays))))

;; rms: "Emacs commands to display a list of something generally start
;; with `list-'.  Please make `list-holidays' the principal name."
;;;###autoload
(defun list-holidays (y1 &optional y2 l label)
  "Display holidays for years Y1 to Y2 (inclusive).
Y2 defaults to Y1.  The optional list of holidays L defaults to
`calendar-holidays'.  If you want to control what holidays are
displayed, use a different list.  For example,

  (list-holidays 2006 2006
    (append general-holidays local-holidays other-holidays))

will display holidays for the year 2006 defined in the 3
mentioned lists, and nothing else.

When called interactively, this command offers a choice of
holidays, based on the variables `solar-holidays' etc.  See the
documentation of `calendar-holidays' for a list of the variables
that control the choices, as well as a description of the format
of a holiday list.

The optional LABEL is used to label the buffer created."
  (interactive
   (let* ((start-year (calendar-read
                       "Starting year of holidays (>0): "
                       (lambda (x) (> x 0))
                       (int-to-string (extract-calendar-year
                                       (calendar-current-date)))))
          (end-year (calendar-read
                     (format "Ending year (inclusive) of holidays (>=%s): "
                             start-year)
                     (lambda (x) (>= x start-year))
                     (int-to-string start-year)))
          (completion-ignore-case t)
          (lists
           (list
            (cons "All" calendar-holidays)
            (cons "Equinoxes/Solstices"
                  (list (list 'solar-equinoxes-solstices)))
            (if general-holidays (cons "General" general-holidays))
            (if local-holidays (cons "Local" local-holidays))
            (if other-holidays (cons "Other" other-holidays))
            (if christian-holidays (cons "Christian" christian-holidays))
            (if hebrew-holidays (cons "Hebrew" hebrew-holidays))
            (if islamic-holidays (cons "Islamic" islamic-holidays))
            (if bahai-holidays (cons "Baha'i" bahai-holidays))
            (if oriental-holidays (cons "Oriental" oriental-holidays))
            (if solar-holidays (cons "Solar" solar-holidays))
            (cons "Ask" nil)))
          (choice (capitalize
                   (completing-read "List (TAB for choices): " lists nil t)))
          (which (if (string-equal choice "Ask")
                     (eval (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays"
                    (format "%s Holidays" choice)))))
     (list start-year end-year which name)))
  (unless y2 (setq y2 y1))
  (message "Computing holidays...")
  (let ((calendar-holidays (or l calendar-holidays))
        (title (or label "Holidays"))
        (s (calendar-absolute-from-gregorian (list 2 1 y1)))
        (e (calendar-absolute-from-gregorian (list 11 1 y2)))
        (displayed-month 2)
        (displayed-year y1)
        holiday-list)
    (while (<= s e)
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (increment-calendar-month displayed-month displayed-year 3)
      (setq s (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    (save-excursion
      (calendar-in-read-only-buffer holiday-buffer
        (calendar-set-mode-line
         (if (= y1 y2)
             (format "%s for %s" title y1)
           (format "%s for %s-%s" title y1 y2)))
        (insert
         (mapconcat
          (lambda (x) (concat (calendar-date-string (car x))
                              ": " (cadr x)))
          holiday-list "\n")))
      (message "Computing holidays...done"))))

;;;###autoload
(defalias 'holiday-list 'list-holidays)

;;;###diary-autoload
(defun calendar-check-holidays (date)
  "Check the list of holidays for any that occur on DATE.
The value returned is a list of strings of relevant holiday descriptions.
The holidays are those in the list `calendar-holidays'."
  (let ((displayed-month (extract-calendar-month date))
        (displayed-year (extract-calendar-year date))
        holiday-list)
    (dolist (h (calendar-holiday-list) holiday-list)
      (if (calendar-date-equal date (car h))
          (setq holiday-list (append holiday-list (cdr h)))))))

(define-obsolete-function-alias
  'check-calendar-holidays 'calendar-check-holidays "23.1")

;;;###cal-autoload
(defun calendar-cursor-holidays ()
  "Find holidays for the date specified by the cursor in the calendar window."
  (interactive)
  (message "Checking holidays...")
  (let* ((date (calendar-cursor-to-date t))
         (date-string (calendar-date-string date))
         (holiday-list (calendar-check-holidays date))
         (holiday-string (mapconcat 'identity holiday-list ";  "))
         (msg (format "%s:  %s" date-string holiday-string)))
    (if (not holiday-list)
        (message "No holidays known for %s" date-string)
      (if (<= (length msg) (frame-width))
          (message "%s" msg)
        (calendar-in-read-only-buffer holiday-buffer
          (calendar-set-mode-line date-string)
          (insert (mapconcat 'identity holiday-list "\n")))
        (message "Checking holidays...done")))))

;;;###cal-autoload
(defun calendar-mark-holidays ()
  "Mark notable days in the calendar window."
  (interactive)
  (setq mark-holidays-in-calendar t)
  (message "Marking holidays...")
  (dolist (holiday (calendar-holiday-list))
    (mark-visible-calendar-date (car holiday) calendar-holiday-marker))
  (message "Marking holidays...done"))

(define-obsolete-function-alias
  'mark-calendar-holidays 'calendar-mark-holidays "23.1")

;; Below are the functions that calculate the dates of holidays; these
;; are eval'ed in the function calendar-holiday-list.  If you
;; write other such functions, be sure to imitate the style used below.
;; Remember that each function must return a list of items of the form
;; ((month day year) string) of VISIBLE dates in the calendar window.

(defun holiday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  ;; This determines whether a given month is visible in the calendar.
  ;; cf calendar-date-is-visible-p (which also checks the year part).
  ;; The day is irrelevant since only full months are displayed.
  ;; Since the calendar displays three months at a time, month N
  ;; is visible if displayed-month = N-1, N, N+1.
  ;; In particular, November is visible if d-m = 10, 11, 12.
  ;; This is useful, because we can do a one-sided test:
  ;; November is visible if d-m > 9. (Similarly, February is visible if
  ;; d-m < 4.)
  ;; To determine if December is visible, we can shift the calendar
  ;; back a month and ask if November is visible; to determine if
  ;; October is visible, we can shift it forward a month and ask if
  ;; November is visible; etc.
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)                         ; is november visible?
        (list (list (list month day y) string)))))

(defun holiday-float (month dayname n string &optional day)
  "Holiday on MONTH, DAYNAME (Nth occurrence) called STRING.
If the Nth DAYNAME in MONTH is visible, the value returned is the list
\(((MONTH DAY year) STRING)).

If N<0, count backward from the end of MONTH.

An optional parameter DAY means the Nth DAYNAME on or after/before MONTH DAY.

Returns nil if it is not visible in the current calendar window."
  ;; This is messy because the holiday may be visible, while the date
  ;; on which it is based is not.  For example, the first Monday after
  ;; December 30 may be visible when January is not.  For large values
  ;; of |n| the problem is more grotesque.  If we didn't have to worry
  ;; about such cases, we could just use the original version of this
  ;; function:
  ;;  (let ((m displayed-month)
  ;;        (y displayed-year))
  ;;    (increment-calendar-month m y (- 11 month))
  ;;    (if (> m 9); month in year y is visible
  ;;      (list (list (calendar-nth-named-day n dayname month y day) string)))))
  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         (d1 (progn             ; first possible base date for holiday
               (increment-calendar-month m1 y1 -1)
               (+ (calendar-nth-named-absday 1 dayname m1 y1)
                  (* -7 n)
                  (if (> n 0) 1 -7))))
         (d2                     ; last possible base date for holiday
          (progn
            (increment-calendar-month m2 y2 1)
            (+ (calendar-nth-named-absday -1 dayname m2 y2)
               (* -7 n)
               (if (> n 0) 7 -1))))
         (y1 (extract-calendar-year (calendar-gregorian-from-absolute d1)))
         (y2 (extract-calendar-year (calendar-gregorian-from-absolute d2)))
         (y                             ; year of base date
          (if (or (= y1 y2) (> month 9))
              y1
            y2))
         (d                             ; day of base date
          (or day (if (> n 0)
                      1
                    (calendar-last-day-of-month month y))))
         (date                        ; base date for holiday
          (calendar-absolute-from-gregorian (list month d y))))
    (and (<= d1 date) (<= date d2)
         (list (list (calendar-nth-named-day n dayname month y d)
                     string)))))

(defun holiday-filter-visible-calendar (l)
  "Return a list of all visible holidays of those on L."
  (let (visible)
    (dolist (p l visible)
      (and (car p)
           (calendar-date-is-visible-p (car p))
           (push p visible)))))

(define-obsolete-function-alias
  'filter-visible-calendar-holidays 'holiday-filter-visible-calendar "23.1")

(defun holiday-sexp (sexp string)
  "Sexp holiday for dates in the calendar window.
SEXP is an expression in variable `year' that is evaluated to
give `date'.  STRING is an expression in `date' that evaluates to
the holiday description of `date'.  If `date' is visible in the
calendar window, the holiday STRING is on that date.  If date is
nil, or if the date is not visible, there is no holiday."
  (let ((m displayed-month)
        (y displayed-year)
        year date)
    (increment-calendar-month m y -1)
    (holiday-filter-visible-calendar
     (list
      (progn
        (setq year y
              date (eval sexp))
        (list date (if date (eval string))))
      (progn
        (setq year (1+ y)
              date (eval sexp))
        (list date (if date (eval string))))))))


(defun holiday-advent (&optional n string)
  "Date of Nth day after advent (named STRING), if visible in calendar window.
Negative values of N are interpreted as days before advent.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after advent.

For backwards compatibility, if this function is called with no
arguments, then it returns the value appropriate for advent itself."
  ;; Backwards compatibility layer.
  (if (not n)
      (holiday-advent 0 "Advent")
    (let* ((year displayed-year)
           (month displayed-month)
           (advent (progn
                     (increment-calendar-month month year -1)
                     (calendar-gregorian-from-absolute
                      (+ n
                         (calendar-dayname-on-or-before
                          0
                          (calendar-absolute-from-gregorian
                           (list 12 3 year))))))))
      (if (calendar-date-is-visible-p advent)
          (list (list advent string))))))

(defun holiday-easter-etc (&optional n string)
  "Date of Nth day after Easter (named STRING), if visible in calendar window.
Negative values of N are interpreted as days before Easter.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after Easter.

For backwards compatibility, if this function is called with no
arguments, then it returns a list of \"standard\" Easter-related
holidays (with more entries if `all-christian-calendar-holidays'
is non-nil)."
  ;; Backwards compatibility layer.
  (if (not n)
      (delq nil                   ; filter out nil (not visible) dates
            (mapcar (lambda (e)
                      (apply 'holiday-easter-etc e))
                    (append
                     (if all-christian-calendar-holidays
                         '((-63 "Septuagesima Sunday")
                           (-56 "Sexagesima Sunday")
                           (-49 "Shrove Sunday")
                           (-48 "Shrove Monday")
                           (-47 "Shrove Tuesday")
                           (-14 "Passion Sunday")
                           (-7 "Palm Sunday")
                           (-3 "Maundy Thursday")
                           (35 "Rogation Sunday")
                           (39 "Ascension Day")
                           (49 "Pentecost (Whitsunday)")
                           (50 "Whitmonday")
                           (56 "Trinity Sunday")
                           (60 "Corpus Christi")))
                     '((0 "Easter Sunday")
                       (-2 "Good Friday")
                       (-46 "Ash Wednesday")))))
    (let* ((century (1+ (/ displayed-year 100)))
           (shifted-epact               ; age of moon for April 5...
            (% (+ 14 (* 11 (% displayed-year 19)) ; ...by Nicaean rule
                  (-     ; ...corrected for the Gregorian century rule
                   (/ (* 3 century) 4))
                  (/       ; ...corrected for Metonic cycle inaccuracy
                   (+ 5 (* 8 century)) 25)
                  (* 30 century))       ; keeps value positive
               30))
           (adjusted-epact              ; adjust for 29.5 day month
            (if (or (zerop shifted-epact)
                    (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
                (1+ shifted-epact)
              shifted-epact))
           (paschal-moon ; day after the full moon on or after March 21
            (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
               adjusted-epact))
           (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
      (holiday-filter-visible-calendar
       (list (list (calendar-gregorian-from-absolute (+ abs-easter n))
                   string))))))

;; Prior call to calendar-julian-from-absolute will autoload cal-julian.
(declare-function calendar-absolute-from-julian "cal-julian" (date))

(defun holiday-greek-orthodox-easter ()
  "Date of Easter according to the rule of the Council of Nicaea."
  (let* ((m displayed-month)
         (y displayed-year)
         (julian-year (progn
                        (increment-calendar-month m y 1)
                        (extract-calendar-year
                         (calendar-julian-from-absolute
                          (calendar-absolute-from-gregorian
                           (list m (calendar-last-day-of-month m y) y))))))
         (shifted-epact                 ; age of moon for April 5
          (% (+ 14
                (* 11 (% julian-year 19)))
             30))
         (paschal-moon      ; day after full moon on or after March 21
          (- (calendar-absolute-from-julian (list 4 19 julian-year))
             shifted-epact))
         (nicaean-easter           ; Sunday following the Paschal moon
          (calendar-gregorian-from-absolute
           (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))))
    (if (calendar-date-is-visible-p nicaean-easter)
        (list (list nicaean-easter "Pascha (Greek Orthodox Easter)")))))

(provide 'holidays)

;; arch-tag: 48eb3117-75a7-4dbe-8fd9-873c3cbb0d37
;;; holidays.el ends here
