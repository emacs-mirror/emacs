;;; cal-islam.el --- calendar functions for the Islamic calendar

;; Copyright (C) 1995, 1997, 2001, 2003 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: Islamic calendar, calendar, diary

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

;; This collection of functions implements the features of calendar.el and
;; diary.el that deal with the Islamic calendar.

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(defvar displayed-month)
(defvar displayed-year)

(require 'cal-julian)

(defvar calendar-islamic-month-name-array
  ["Muharram" "Safar" "Rabi I" "Rabi II" "Jumada I" "Jumada II"
   "Rajab" "Sha'ban" "Ramadan" "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"]
"Array of strings giving the names of the Islamic months.")

(defvar calendar-islamic-epoch (calendar-absolute-from-julian '(7 16 622))
  "Absolute date of start of Islamic calendar = August 29, 284 A.D. (Julian).")

(defun islamic-calendar-leap-year-p (year)
  "Returns t if YEAR is a leap year on the Islamic calendar."
  (memq (% year 30)
        (list 2 5 7 10 13 16 18 21 24 26 29)))

(defun islamic-calendar-last-day-of-month (month year)
  "The last day in MONTH during YEAR on the Islamic calendar."
  (cond
   ((memq month (list 1 3 5 7 9 11)) 30)
   ((memq month (list 2 4 6 8 10)) 29)
   (t (if (islamic-calendar-leap-year-p year) 30 29))))

(defun islamic-calendar-day-number (date)
  "Return the day number within the year of the Islamic date DATE."
    (let* ((month (extract-calendar-month date))
           (day (extract-calendar-day date)))
      (+ (* 30 (/ month 2))
         (* 29 (/ (1- month) 2))
         day)))

(defun calendar-absolute-from-islamic (date)
  "Absolute date of Islamic DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (y (% year 30))
         (leap-years-in-cycle
          (cond
           ((< y 3) 0)  ((< y 6) 1)  ((< y 8) 2)  ((< y 11) 3) ((< y 14) 4)
           ((< y 17) 5) ((< y 19) 6) ((< y 22) 7) ((< y 25) 8) ((< y 27) 9)
           (t 10))))
    (+ (islamic-calendar-day-number date);; days so far this year
       (* (1- year) 354)                 ;; days in all non-leap years
       (* 11 (/ year 30))                ;; leap days in complete cycles
       leap-years-in-cycle               ;; leap days this cycle
       (1- calendar-islamic-epoch))))    ;; days before start of calendar

(defun calendar-islamic-from-absolute (date)
  "Compute the Islamic date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (if (< date calendar-islamic-epoch)
      (list 0 0 0);; pre-Islamic date
    (let* ((approx (/ (- date calendar-islamic-epoch)
                      355));; Approximation from below.
           (year           ;; Search forward from the approximation.
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-absolute-from-islamic
                                       (list 1 1 (1+ y))))
                             1)))
           (month          ;; Search forward from Muharram.
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-absolute-from-islamic
                                  (list m
                                        (islamic-calendar-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day            ;; Calculate the day by subtraction.
            (- date
               (1- (calendar-absolute-from-islamic (list month 1 year))))))
      (list month day year))))

(defun calendar-islamic-date-string (&optional date)
  "String of Islamic date before sunset of Gregorian DATE.
Returns the empty string if DATE is pre-Islamic.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'."
  (let ((calendar-month-name-array calendar-islamic-month-name-array)
        (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date))))))
    (if (< (extract-calendar-year islamic-date) 1)
        ""
      (calendar-date-string islamic-date nil t))))

(defun calendar-print-islamic-date ()
  "Show the Islamic calendar equivalent of the date under the cursor."
  (interactive)
  (let ((i (calendar-islamic-date-string (calendar-cursor-to-date t))))
    (if (string-equal i "")
        (message "Date is pre-Islamic")
      (message "Islamic date (until sunset): %s" i))))

(defun calendar-goto-islamic-date (date &optional noecho)
  "Move cursor to Islamic DATE; echo Islamic date unless NOECHO is t."
  (interactive
   (let* ((today (calendar-current-date))
          (year (calendar-read
                 "Islamic calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-islamic-from-absolute
                    (calendar-absolute-from-gregorian today))))))
          (month-array calendar-islamic-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc-string
                        (completing-read
                         "Islamic calendar month name: "
                         (mapcar 'list (append month-array nil))
                         nil t)
                       (calendar-make-alist month-array 1) t)))
          (last (islamic-calendar-last-day-of-month month year))
          (day (calendar-read
                (format "Islamic calendar day (1-%d): " last)
                '(lambda (x) (and (< 0 x) (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic date)))
  (or noecho (calendar-print-islamic-date)))

(defun diary-islamic-date ()
  "Islamic calendar equivalent of date diary entry."
  (let ((i (calendar-islamic-date-string date)))
    (if (string-equal i "")
        "Date is pre-Islamic"
      (format "Islamic date (until sunset): %s" i))))

(defun holiday-islamic (month day string)
  "Holiday on MONTH, DAY (Islamic) called STRING.
If MONTH, DAY (Islamic) is visible, the value returned is corresponding
Gregorian date in the form of the list (((month day year) STRING)).  Returns
nil if it is not visible in the current calendar window."
  (let* ((islamic-date (calendar-islamic-from-absolute
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

(defun list-islamic-diary-entries ()
  "Add any Islamic date entries from the diary file to `diary-entries-list'.
Islamic date diary entries must be prefaced by an `islamic-diary-entry-symbol'
\(normally an `I').  The same diary date forms govern the style of the Islamic
calendar entries, except that the Islamic month names must be spelled in full.
The Islamic months are numbered from 1 to 12 with Muharram being 1 and 12 being
Dhu al-Hijjah.  If an Islamic date diary entry begins with a
`diary-nonmarking-symbol', the entry will appear in the diary listing, but will
not be marked in the calendar.  This function is provided for use with the
`nongregorian-diary-listing-hook'."
  (if (< 0 number)
      (let ((buffer-read-only nil)
            (diary-modified (buffer-modified-p))
            (gdate original-date)
            (mark (regexp-quote diary-nonmarking-symbol)))
        (calendar-for-loop i from 1 to number do
           (let* ((d diary-date-forms)
                  (idate (calendar-islamic-from-absolute
                          (calendar-absolute-from-gregorian gdate)))
                  (month (extract-calendar-month idate))
                  (day (extract-calendar-day idate))
                  (year (extract-calendar-year idate)))
             (while d
               (let*
                   ((date-form (if (equal (car (car d)) 'backup)
                                   (cdr (car d))
                                 (car d)))
                    (backup (equal (car (car d)) 'backup))
                    (dayname
                     (format "%s\\|%s\\.?"
                      (calendar-day-name gdate)
                      (calendar-day-name gdate 'abbrev)))
                    (calendar-month-name-array
                     calendar-islamic-month-name-array)
                    (monthname
                     (concat
                      "\\*\\|"
                      (calendar-month-name month)))
                    (month (concat "\\*\\|0*" (int-to-string month)))
                    (day (concat "\\*\\|0*" (int-to-string day)))
                    (year
                     (concat
                      "\\*\\|0*" (int-to-string year)
                      (if abbreviated-calendar-year
                          (concat "\\|" (int-to-string (% year 100)))
                        "")))
                    (regexp
                     (concat
                      "\\(\\`\\|\^M\\|\n\\)" mark "?"
                      (regexp-quote islamic-diary-entry-symbol)
                      "\\("
                      (mapconcat 'eval date-form "\\)\\(")
                      "\\)"))
                    (case-fold-search t))
                 (goto-char (point-min))
                 (while (re-search-forward regexp nil t)
                   (if backup (re-search-backward "\\<" nil t))
                   (if (and (or (char-equal (preceding-char) ?\^M)
                                (char-equal (preceding-char) ?\n))
                            (not (looking-at " \\|\^I")))
                       ;;  Diary entry that consists only of date.
                       (backward-char 1)
                     ;;  Found a nonempty diary entry--make it visible and
                     ;;  add it to the list.
                     (let ((entry-start (point))
                           (date-start))
                       (re-search-backward "\^M\\|\n\\|\\`")
                       (setq date-start (point))
                       (re-search-forward "\^M\\|\n" nil t 2)
                       (while (looking-at " \\|\^I")
                         (re-search-forward "\^M\\|\n" nil t))
                       (backward-char 1)
                       (subst-char-in-region date-start (point) ?\^M ?\n t)
                       (add-to-diary-list
                        gdate
                        (buffer-substring-no-properties entry-start (point))
                        (buffer-substring-no-properties
                         (1+ date-start) (1- entry-start))
                        (copy-marker entry-start))))))
               (setq d (cdr d))))
           (setq gdate
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-absolute-from-gregorian gdate)))))
           (set-buffer-modified-p diary-modified))
        (goto-char (point-min))))

(defun mark-islamic-diary-entries ()
  "Mark days in the calendar window that have Islamic date diary entries.
Each entry in diary-file (or included files) visible in the calendar window
is marked.  Islamic date entries are prefaced by a islamic-diary-entry-symbol
\(normally an `I').  The same diary-date-forms govern the style of the Islamic
calendar entries, except that the Islamic month names must be spelled in full.
The Islamic months are numbered from 1 to 12 with Muharram being 1 and 12 being
Dhu al-Hijjah.  Islamic date diary entries that begin with a
diary-nonmarking-symbol will not be marked in the calendar.  This function is
provided for use as part of the nongregorian-diary-marking-hook."
  (let ((d diary-date-forms))
    (while d
      (let*
          ((date-form (if (equal (car (car d)) 'backup)
                          (cdr (car d))
                        (car d)));; ignore 'backup directive
           (dayname (diary-name-pattern calendar-day-name-array
                                        calendar-day-abbrev-array))
           (monthname
            (format "%s\\|\\*"
                    (diary-name-pattern calendar-islamic-month-name-array)))
           (month "[0-9]+\\|\\*")
           (day "[0-9]+\\|\\*")
           (year "[0-9]+\\|\\*")
           (l (length date-form))
           (d-name-pos (- l (length (memq 'dayname date-form))))
           (d-name-pos (if (/= l d-name-pos) (+ 2 d-name-pos)))
           (m-name-pos (- l (length (memq 'monthname date-form))))
           (m-name-pos (if (/= l m-name-pos) (+ 2 m-name-pos)))
           (d-pos (- l (length (memq 'day date-form))))
           (d-pos (if (/= l d-pos) (+ 2 d-pos)))
           (m-pos (- l (length (memq 'month date-form))))
           (m-pos (if (/= l m-pos) (+ 2 m-pos)))
           (y-pos (- l (length (memq 'year date-form))))
           (y-pos (if (/= l y-pos) (+ 2 y-pos)))
           (regexp
            (concat
             "\\(\\`\\|\^M\\|\n\\)"
             (regexp-quote islamic-diary-entry-symbol)
             "\\("
             (mapconcat 'eval date-form "\\)\\(")
             "\\)"))
           (case-fold-search t))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let* ((dd-name
                  (if d-name-pos
                      (buffer-substring
                       (match-beginning d-name-pos)
                       (match-end d-name-pos))))
                 (mm-name
                  (if m-name-pos
                      (buffer-substring
                       (match-beginning m-name-pos)
                       (match-end m-name-pos))))
                 (mm (string-to-number
                      (if m-pos
                          (buffer-substring
                           (match-beginning m-pos)
                           (match-end m-pos))
                        "")))
                 (dd (string-to-number
                      (if d-pos
                          (buffer-substring
                           (match-beginning d-pos)
                           (match-end d-pos))
                        "")))
                 (y-str (if y-pos
                            (buffer-substring
                             (match-beginning y-pos)
                             (match-end y-pos))))
                 (yy (if (not y-str)
                         0
                       (if (and (= (length y-str) 2)
                                abbreviated-calendar-year)
                           (let* ((current-y
                                   (extract-calendar-year
                                    (calendar-islamic-from-absolute
                                     (calendar-absolute-from-gregorian
                                      (calendar-current-date)))))
                                  (y (+ (string-to-number y-str)
                                        (* 100 (/ current-y 100)))))
                             (if (> (- y current-y) 50)
                                 (- y 100)
                               (if (> (- current-y y) 50)
                                   (+ y 100)
                                 y)))
                         (string-to-number y-str)))))
            (if dd-name
                (mark-calendar-days-named
                 (cdr (assoc-string dd-name
                                         (calendar-make-alist
                                          calendar-day-name-array
                                          0 nil calendar-day-abbrev-array) t)))
              (if mm-name
                  (setq mm (if (string-equal mm-name "*") 0
                             (cdr (assoc-string
                                   mm-name
                                   (calendar-make-alist
                                    calendar-islamic-month-name-array) t)))))
              (mark-islamic-calendar-date-pattern mm dd yy)))))
      (setq d (cdr d)))))

(defun mark-islamic-calendar-date-pattern (month day year)
  "Mark dates in calendar window that conform to Islamic date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (if (and (/= 0 month) (/= 0 day))
        (if (/= 0 year)
            ;; Fully specified Islamic date.
            (let ((date (calendar-gregorian-from-absolute
                         (calendar-absolute-from-islamic
                          (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (mark-visible-calendar-date date)))
          ;; Month and day in any year--this taken from the holiday stuff.
          (let* ((islamic-date (calendar-islamic-from-absolute
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
                               (calendar-absolute-from-islamic
                                (list month day y)))))
                    (if (calendar-date-is-visible-p date)
                        (mark-visible-calendar-date date)))))))
      ;; Not one of the simple cases--check all visible dates for match.
      ;; Actually, the following code takes care of ALL of the cases, but
      ;; it's much too slow to be used for the simple (common) cases.
      (let ((m displayed-month)
            (y displayed-year)
            (first-date)
            (last-date))
        (increment-calendar-month m y -1)
        (setq first-date
              (calendar-absolute-from-gregorian
               (list m 1 y)))
        (increment-calendar-month m y 2)
        (setq last-date
              (calendar-absolute-from-gregorian
               (list m (calendar-last-day-of-month m y) y)))
        (calendar-for-loop date from first-date to last-date do
          (let* ((i-date (calendar-islamic-from-absolute date))
                 (i-month (extract-calendar-month i-date))
                 (i-day (extract-calendar-day i-date))
                 (i-year (extract-calendar-year i-date)))
            (and (or (zerop month)
                     (= month i-month))
                 (or (zerop day)
                     (= day i-day))
                 (or (zerop year)
                     (= year i-year))
                 (mark-visible-calendar-date
                  (calendar-gregorian-from-absolute date)))))))))

(defun insert-islamic-diary-entry (arg)
  "Insert a diary entry.
For the Islamic date corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-month-name-array calendar-islamic-month-name-array))
    (make-diary-entry
     (concat
      islamic-diary-entry-symbol
      (calendar-date-string
       (calendar-islamic-from-absolute
        (calendar-absolute-from-gregorian
         (calendar-cursor-to-date t)))
       nil t))
     arg)))

(defun insert-monthly-islamic-diary-entry (arg)
  "Insert a monthly diary entry.
For the day of the Islamic month corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style '(day " * ") '("* " day )))
         (calendar-month-name-array calendar-islamic-month-name-array))
    (make-diary-entry
     (concat
      islamic-diary-entry-symbol
      (calendar-date-string
       (calendar-islamic-from-absolute
        (calendar-absolute-from-gregorian
         (calendar-cursor-to-date t)))))
     arg)))

(defun insert-yearly-islamic-diary-entry (arg)
  "Insert an annual diary entry.
For the day of the Islamic year corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day)))
         (calendar-month-name-array calendar-islamic-month-name-array))
    (make-diary-entry
     (concat
      islamic-diary-entry-symbol
      (calendar-date-string
       (calendar-islamic-from-absolute
        (calendar-absolute-from-gregorian
         (calendar-cursor-to-date t)))))
     arg)))

(provide 'cal-islam)

;;; arch-tag: a951b6c1-6f47-48d5-bac3-1b505cd719f7
;;; cal-islam.el ends here
