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

;; This collection of functions implements the holiday features as described
;; in calendar.el.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;; An earlier version of the technical details appeared in
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

(defvar displayed-month)
(defvar displayed-year)

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

(autoload 'holiday-bahai "cal-bahai"
  "Holiday on MONTH, DAY (Baha'i) called STRING."
  t)

(autoload 'holiday-chinese-new-year "cal-china"
  "Date of Chinese New Year."
  t)

(autoload 'solar-equinoxes-solstices "solar"
  "Date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point."
  t)

;;;###autoload
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
      (calendar-list-holidays))))

;; rms: "Emacs commands to display a list of something generally start
;; with `list-'.  Please make `list-holidays' the principal name."
;;;###autoload
(defun list-holidays (y1 y2 &optional l label)
  "Display holidays for years Y1 to Y2 (inclusive).

The optional list of holidays L defaults to `calendar-holidays'.
If you want to control what holidays are displayed, use a
different list.  For example,

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
            (if (fboundp 'atan)
                (cons "Equinoxes/Solstices"
                      (list (list 'solar-equinoxes-solstices))))
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
  (message "Computing holidays...")
  (let* ((holiday-buffer "*Holidays*")
         (calendar-holidays (if l l calendar-holidays))
         (title (or label "Holidays"))
         (holiday-list nil)
         (s (calendar-absolute-from-gregorian (list 2 1 y1)))
         (e (calendar-absolute-from-gregorian (list 11 1 y2)))
         (d s)
         (never t)
         (displayed-month 2)
         (displayed-year y1))
    (while (or never (<= d e))
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (setq never nil)
      (increment-calendar-month displayed-month displayed-year 3)
      (setq d (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    (save-excursion
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (calendar-set-mode-line
       (if (= y1 y2)
           (format "%s for %s" title y1)
         (format "%s for %s-%s" title y1 y2)))
      (erase-buffer)
      (goto-char (point-min))
      (insert
       (mapconcat
        (lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Computing holidays...done"))))

;;;###autoload
(defalias 'holiday-list 'list-holidays)

(defun calendar-check-holidays (date)
  "Check the list of holidays for any that occur on DATE.
The value returned is a list of strings of relevant holiday descriptions.
The holidays are those in the list `calendar-holidays'."
  (let ((displayed-month (extract-calendar-month date))
        (displayed-year (extract-calendar-year date))
        (holiday-list))
    (dolist (h (calendar-holiday-list))
      (if (calendar-date-equal date (car h))
          (setq holiday-list (append holiday-list (cdr h)))))
    holiday-list))

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

(defun calendar-mark-holidays ()
  "Mark notable days in the calendar window."
  (interactive)
  (setq mark-holidays-in-calendar t)
  (message "Marking holidays...")
  (dolist (holiday (calendar-holiday-list))
    (mark-visible-calendar-date
     (car holiday) calendar-holiday-marker))
  (message "Marking holidays...done"))

(defun calendar-list-holidays ()
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.  Returns t if any
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
        (lambda (x) (concat (calendar-date-string (car x))
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
The holidays are those in the list `calendar-holidays'."
  (let ((holiday-list ()))
    (dolist (p calendar-holidays)
      (let* ((holidays
              (if calendar-debug-sexp
                  (let ((stack-trace-on-error t))
                    (eval p))
                (condition-case nil
                    (eval p)
                  (error (beep)
                         (message "Bad holiday list item: %s" p)
                         (sleep-for 2))))))
        (if holidays
            (setq holiday-list (append holidays holiday-list)))))
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
  "Holiday on MONTH, DAYNAME (Nth occurrence) called STRING.
If the Nth DAYNAME in MONTH is visible, the value returned is the list
\(((MONTH DAY year) STRING)).

If N<0, count backward from the end of MONTH.

An optional parameter DAY means the Nth DAYNAME on or after/before MONTH DAY.

Returns nil if it is not visible in the current calendar window."
;; This is messy because the holiday may be visible, while the date on which
;; it is based is not.  For example, the first Monday after December 30 may be
;; visible when January is not.  For large values of |n| the problem is more
;; grotesque.  If we didn't have to worry about such cases, we could just use

;;  (let ((m displayed-month)
;;        (y displayed-year))
;;    (increment-calendar-month m y (- 11 month))
;;    (if (> m 9); month in year y is visible
;;      (list (list (calendar-nth-named-day n dayname month y day) string)))))

;; which is the way the function was originally written.

  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 m1)
         (y2 y1))
    (increment-calendar-month m1 y1 -1)
    (increment-calendar-month m2 y2 1)
    (let* ((d1;  first possible base date for holiday
            (+ (calendar-nth-named-absday 1 dayname m1 y1)
               (* -7 n)
               (if (> n 0) 1 -7)))
           (d2;  last possible base date for holiday
            (+ (calendar-nth-named-absday -1 dayname m2 y2)
               (* -7 n)
               (if (> n 0) 7 -1)))
           (y1 (extract-calendar-year (calendar-gregorian-from-absolute d1)))
           (y2 (extract-calendar-year (calendar-gregorian-from-absolute d2)))
           (y; year of base date
            (if (or (= y1 y2) (> month 9))
                  y1
                y2))
           (d; day of base date
            (or day (if (> n 0)
                        1
                      (calendar-last-day-of-month month y))))
           (date; base date for holiday
            (calendar-absolute-from-gregorian (list month d y))))
      (if (and (<= d1 date) (<= date d2))
          (list (list (calendar-nth-named-day n dayname month y d)
                      string))))))

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
    (holiday-filter-visible-calendar
     (list
      (let* ((year y)
             (date (eval sexp))
             (string (if date (eval string))))
        (list date string))
      (let* ((year (1+ y))
             (date (eval sexp))
             (string (if date (eval string))))
        (list date string))))))

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
    (let ((year displayed-year)
          (month displayed-month))
      (increment-calendar-month month year -1)
      (let ((advent (calendar-gregorian-from-absolute
                     (+ n
                        (calendar-dayname-on-or-before
                         0
                         (calendar-absolute-from-gregorian
                          (list 12 3 year)))))))
        (if (calendar-date-is-visible-p advent)
            (list (list advent string)))))))

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
      (let (res-list res)
        (dolist (elem (append
                       (if all-christian-calendar-holidays
                           '((-63 . "Septuagesima Sunday")
                             (-56 . "Sexagesima Sunday")
                             (-49 . "Shrove Sunday")
                             (-48 . "Shrove Monday")
                             (-47 . "Shrove Tuesday")
                             (-14 . "Passion Sunday")
                             (-7 . "Palm Sunday")
                             (-3 . "Maundy Thursday")
                             (35 . "Rogation Sunday")
                             (39 . "Ascension Day")
                             (49 . "Pentecost (Whitsunday)")
                             (50 . "Whitmonday")
                             (56 . "Trinity Sunday")
                             (60 . "Corpus Christi")))
                       '((0 . "Easter Sunday")
                         (-2 . "Good Friday")
                         (-46 . "Ash Wednesday")))
                      res-list)
          ;; Filter out nil (not visible) values.
          (if (setq res (holiday-easter-etc (car elem) (cdr elem)))
              (setq res-list (append res res-list)))))
    (let* ((century (1+ (/ displayed-year 100)))
           (shifted-epact ;; Age of moon for April 5...
            (% (+ 14 (* 11 (% displayed-year 19)) ;;     ...by Nicaean rule
                  (- ;; ...corrected for the Gregorian century rule
                   (/ (* 3 century) 4))
                  (/ ;; ...corrected for Metonic cycle inaccuracy.
                   (+ 5 (* 8 century)) 25)
                  (* 30 century)) ;;              Keeps value positive.
               30))
           (adjusted-epact ;;  Adjust for 29.5 day month.
            (if (or (zerop shifted-epact)
                    (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
                (1+ shifted-epact)
              shifted-epact))
           (paschal-moon ;; Day after the full moon on or after March 21.
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

(defun holiday-filter-visible-calendar (l)
  "Return a list of all visible holidays of those on L."
  (let ((visible ()))
    (dolist (p l)
      (and (car p)
           (calendar-date-is-visible-p (car p))
           (push p visible)))
    visible))

;; Backward compatibility.
(define-obsolete-function-alias
  'filter-visible-calendar-holidays 'holiday-filter-visible-calendar "23.1")
(define-obsolete-function-alias
  'list-calendar-holidays 'calendar-list-holidays "23.1")
(define-obsolete-function-alias
  'mark-calendar-holidays 'calendar-mark-holidays "23.1")
(define-obsolete-function-alias
  'check-calendar-holidays 'calendar-check-holidays "23.1")

(provide 'holidays)

;; arch-tag: 48eb3117-75a7-4dbe-8fd9-873c3cbb0d37
;;; holidays.el ends here
