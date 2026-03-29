;;; cal-move.el --- calendar functions for movement in the calendar  -*- lexical-binding: t; -*-

;; Copyright (C) 1995, 2001-2026 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: calendar
;; Human-Keywords: calendar
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

;; See calendar.el.

;;; Code:

;; FIXME should calendar just require this?
(require 'calendar)


;; Note that this is not really the "closest" date.
;; In most cases, it just searches forwards for the next day.
;;;###cal-autoload
(defun calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (or (calendar-cursor-to-date)
      (let* ((col (current-column))
             (edges (cdr (assoc (calendar-column-to-segment)
                                calendar-month-edges)))
             (last (nth 2 edges))
             (right (nth 3 edges)))
        (when (< (count-lines (point-min) (point)) calendar-first-date-row)
          (goto-char (point-min))
          (forward-line (1- calendar-first-date-row))
          (move-to-column col))
        ;; The date positions are fixed and computable, but searching
        ;; is probably more flexible.  Need to consider blank days at
        ;; start and end of month if computing positions.
        ;; 'date text-property is used to exclude intermonth text.
        (unless (and (looking-at "[0-9]")
                     (get-text-property (point) 'date))
          ;; We search forwards for a number, except close to the RH
          ;; margin of a month, where we search backwards.
          ;; Note that the searches can go to other lines.
          (if (or (looking-at " *$")
                  (and (> col last) (< col right)))
              (while (and (re-search-backward "[0-9]" nil t)
                          (not (get-text-property (point) 'date))))
            (while (and (re-search-forward "[0-9]" nil t)
                        (not (get-text-property (1- (point)) 'date))))
            (backward-char 1)))
        (calendar-cursor-to-date))))

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

;;;###cal-autoload
(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (goto-char (point-min))
    (forward-line (+ calendar-first-date-row -1
                     (/ (+ day -1
                           (mod
                            (- (calendar-day-of-week (list month 1 year))
                               calendar-week-start-day)
                            7))
                        7)))
    (move-to-column (+ calendar-left-margin (1- calendar-day-digit-width)
                       (* calendar-month-width
                          (1+ (calendar-interval
                               displayed-month displayed-year month year)))
                       (* calendar-column-width
                          (mod
                           (- (calendar-day-of-week date)
                              calendar-week-start-day)
                           7))))))

;;;###cal-autoload
(defun calendar-goto-today ()
  "Reposition the calendar window so the current date is visible."
  (interactive)
  (let ((today (calendar-current-date))) ; the date might have changed
    (if (not (calendar-date-is-visible-p today))
        (calendar-generate-window))
    (calendar-cursor-to-visible-date today))
  (run-hooks 'calendar-move-hook))

(defun calendar--show-month-at-edge (month year arg)
  "Regenerate the calendar with MONTH, YEAR at the left or right edge.
This function is mainly used when MONTH, YEAR is invisible.  The new
month is placed at the right edge if ARG is positive, and the left edge
otherwise."
  (pcase-let* ((`(,m1 ,y1 ,m2 ,y2) (calendar-get-month-range))
               (offset (if (> arg 0)
                           (calendar-interval m2 y2 month year)
                         (calendar-interval m1 y1 month year))))
    (calendar-increment-month m1 y1 (1+ offset))
    (calendar-generate-window m1 y1)))

;;;###cal-autoload
(defun calendar-forward-month (arg)
  "Move the cursor forward ARG months.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((cursor-date (calendar-cursor-to-date t))
         (month (calendar-extract-month cursor-date))
         (day (calendar-extract-day cursor-date))
         (year (calendar-extract-year cursor-date))
         (last (progn
                 (calendar-increment-month month year arg)
                 (calendar-last-day-of-month month year)))
         (day (min last day))
         ;; Put the new month on the screen, if needed, and go to the new date.
         (new-cursor-date (list month day year)))
    (if (not (calendar-date-is-visible-p new-cursor-date))
        (calendar--show-month-at-edge month year arg))
    (calendar-cursor-to-visible-date new-cursor-date))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-forward-year (arg)
  "Move the cursor forward by ARG years.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (* 12 arg)))

;;;###cal-autoload
(defun calendar-backward-month (arg)
  "Move the cursor backward by ARG months.
Movement is forward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (- arg)))

;;;###cal-autoload
(defun calendar-backward-year (arg)
  "Move the cursor backward ARG years.
Movement is forward is ARG is negative."
  (interactive "p")
  (calendar-forward-month (* -12 arg)))

;;;###cal-autoload
(defun calendar-scroll-left (&optional arg event)
  "Scroll the displayed calendar left by ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-current-buffer
    (when (event-start event)
      (set-buffer (calendar-event-buffer event)))
    (calendar-cursor-to-nearest-date)
    (unless (zerop arg)
      (let* ((old-date (calendar-cursor-to-date))
             (today (calendar-current-date))
             (month displayed-month)
             (year displayed-year)
             (offset (calendar-interval month year
                                        (calendar-extract-month old-date)
                                        (calendar-extract-year old-date))))
        (calendar-increment-month month year arg)
        (calendar-generate-window month year)
        (calendar-cursor-to-visible-date
         (cond
          ((calendar-date-is-visible-p old-date) old-date)
          ((calendar-date-is-visible-p today) today)
          (t (calendar-increment-month month year offset)
             (list month 1 year))))))
    (run-hooks 'calendar-move-hook)))

;;;###cal-autoload
(defun calendar-scroll-right (&optional arg event)
  "Scroll the displayed calendar window right by ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (calendar-scroll-left (- (or arg 1)) event))

;;;###cal-autoload
(defun calendar-scroll-calendar-left (arg &optional event)
  "Scroll the displayed calendar window left ARG times.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (calendar-scroll-left (* calendar-total-months arg) event))

;;;###cal-autoload
(define-obsolete-function-alias 'calendar-scroll-left-three-months
  'calendar-scroll-calendar-left "31.1")

;; cf scroll-bar-toolkit-scroll
;;;###cal-autoload
(defun calendar-scroll-toolkit-scroll (event)
  "Function to scroll the calendar after a toolkit scroll-bar click."
  (interactive "e")
  (let ((part (nth 4 (event-end event))))
    ;; Not bothering with drag events (handle, end-scroll).
    (cond ((memq part '(above-handle up top))
           (calendar-scroll-right nil event))
          ((memq part '(below-handle down bottom))
           (calendar-scroll-left nil event)))))

;;;###cal-autoload
(defun calendar-scroll-calendar-right (arg &optional event)
  "Scroll the displayed calendar window right ARG times.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (calendar-scroll-left (* -1 calendar-total-months arg) event))

;;;###cal-autoload
(define-obsolete-function-alias 'calendar-scroll-right-three-months
  'calendar-scroll-calendar-right "31.1")

(defvar calendar-recenter-last-op nil
  "Last calendar recenter operation performed.")

;;;###cal-autoload
(defun calendar-recenter ()
  "Scroll the calendar so that the month of the date at point is centered.
Next invocation puts this month on the leftmost position, and another
invocation puts this month on the rightmost position.  Subsequent
invocations reuse the same order in a cyclical manner."
  (interactive)
  (pcase-let ((positions '(center first last))
              (cursor-date (calendar-cursor-to-nearest-date))
              (`(,m1 ,y1 ,m2 ,y2) (calendar-get-month-range)))
    ;; Update global last position upon repeat.
    (setq calendar-recenter-last-op
          (if (eq this-command last-command)
              (car (or (cdr (memq calendar-recenter-last-op positions))
                       positions))
            (car positions)))
    (cond ((eq calendar-recenter-last-op 'center)
           (calendar-increment-month
            m1 y1 (/ (1- calendar-total-months) 2)))
          ;; Other sub-cases should not happen as we should be centered
          ;; from here.
          ((eq calendar-recenter-last-op 'first))
          ((eq calendar-recenter-last-op 'last)
           (setq m1 m2 y1 y2)))
    (calendar-scroll-left
     (calendar-interval m1 y1
                        (calendar-extract-month cursor-date)
                        (calendar-extract-year cursor-date)))))

;;;###cal-autoload
(defun calendar-forward-day (arg)
  "Move the cursor forward ARG days.
Moves backward if ARG is negative."
  (interactive "p")
  (unless (zerop arg)
    (let* ((cursor-date (or (calendar-cursor-to-date)
                            (progn
                              (if (> arg 0) (setq arg (1- arg)))
                              (calendar-cursor-to-nearest-date))))
           (new-cursor-date
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian cursor-date) arg)))
           (month (calendar-extract-month new-cursor-date))
           (year (calendar-extract-year new-cursor-date)))
      ;; Put the new month on the screen, if needed.
      (unless (calendar-date-is-visible-p new-cursor-date)
        ;; The next line gives smoother scrolling (i.e. making the new
        ;; month appear at the edge).
        (calendar--show-month-at-edge month year arg))
      ;; Go to the new date.
      (calendar-cursor-to-visible-date new-cursor-date)))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-backward-day (arg)
  "Move the cursor back ARG days.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (- arg)))

;;;###cal-autoload
(defun calendar-forward-week (arg)
  "Move the cursor forward ARG weeks.
Moves backward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg 7)))

;;;###cal-autoload
(defun calendar-backward-week (arg)
  "Move the cursor back ARG weeks.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg -7)))

;;;###cal-autoload
(defun calendar-beginning-of-week (arg)
  "Move the cursor back ARG calendar-week-start-day's."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-backward-day
     (if (= day calendar-week-start-day)
         (* 7 arg)
       (+ (mod (- day calendar-week-start-day) 7)
          (* 7 (1- arg)))))))

;;;###cal-autoload
(defun calendar-end-of-week (arg)
  "Move the cursor forward ARG calendar-week-start-day+6's."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-forward-day
     (if (= day (mod (1- calendar-week-start-day) 7))
         (* 7 arg)
       (+ (- 6 (mod (- day calendar-week-start-day) 7))
          (* 7 (1- arg)))))))

;;;###cal-autoload
(defun calendar-beginning-of-month (arg)
  "Move the cursor backward ARG month beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date)))
    (if (= day 1)
        (calendar-backward-month arg)
      (calendar-cursor-to-visible-date (list month 1 year))
      (calendar-backward-month (1- arg)))))

;;;###cal-autoload
(defun calendar-end-of-month (arg)
  "Move the cursor forward ARG month ends."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (last-day (calendar-last-day-of-month month year))
         (last-day (progn
                     (unless (= day last-day)
                       (calendar-cursor-to-visible-date
                        (list month last-day year))
                       (setq arg (1- arg)))
                     (calendar-increment-month month year arg)
                     (list month
                           (calendar-last-day-of-month month year)
                           year))))
    (if (not (calendar-date-is-visible-p last-day))
        (calendar--show-month-at-edge month year arg))
    (calendar-cursor-to-visible-date last-day))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-beginning-of-year (arg)
  "Move the cursor backward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (jan-first (list 1 1 year))
         (calendar-move-hook nil))
    (if (and (= day 1) (= 1 month))
        (calendar-backward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p jan-first))
          (calendar-cursor-to-visible-date jan-first)
        (setq year (- year (1- arg)))
        (calendar--show-month-at-edge 1 year (- arg))
        (calendar-cursor-to-visible-date (list 1 1 year)))))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-end-of-year (arg)
  "Move the cursor forward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (dec-31 (list 12 31 year))
         (calendar-move-hook nil))
    (if (and (= day 31) (= 12 month))
        (calendar-forward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p dec-31))
          (calendar-cursor-to-visible-date dec-31)
        (setq year (+ year (1- arg)))
        (calendar--show-month-at-edge 12 year arg)
        (calendar-cursor-to-visible-date (list 12 31 year)))))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-goto-date (date)
  "Move cursor to DATE."
  (interactive (list (calendar-read-date)))
  (let ((month (calendar-extract-month date))
        (year (calendar-extract-year date)))
    (if (not (calendar-date-is-visible-p date))
        (calendar-other-month
         (if (and (= month 1) (= year 1))
             2
           month)
         year)))
  (calendar-cursor-to-visible-date date)
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-goto-day-of-year (year day &optional noecho)
  "Move cursor to YEAR, DAY number; echo DAY/YEAR unless NOECHO is non-nil.
Negative DAY counts backward from end of year.
Interactively, prompt for YEAR and DAY number."
  (interactive
   (let* ((year (calendar-read-sexp
                 "Year (>0)"
                 (lambda (x) (> x 0))
                 (calendar-extract-year (calendar-current-date))))
          (last (if (calendar-leap-year-p year) 366 365))
          (day (calendar-read-sexp
                "Day number (+/- 1-%d)"
                (lambda (x) (and (<= 1 (abs x)) (<= (abs x) last)))
                (calendar-day-number (calendar-current-date))
                last)))
     (list year day)))
  (calendar-goto-date (calendar-date-from-day-of-year year day))
  (or noecho (calendar-print-day-of-year)))

;;;###cal-autoload
(defun calendar-show-more-months (&optional arg)
  "Show ARG more months on the right in the calendar.
The calendar shows at most 12 months and at least 3 months."
  (interactive "p" calendar-mode)
  (cond
   ((= arg 0) nil)
   ((> arg 0)
    (if (>= calendar-total-months 12)
        (error "The calendar shows at most 12 months."))
    (let ((avail (floor (/ (- (window-body-width) calendar-right-margin)
                           (+ calendar-month-width
                              calendar-intermonth-spacing)))))
      (if (< avail 1)
          (message "No space left to display more months.")
        (incf calendar-total-months (min avail arg))
        (calendar-recompute-layout-variables)
        (calendar-redraw))))
   (t
    (if (<= calendar-total-months 3)
        (error "The calendar shows at least 3 months."))
    (calendar-cursor-to-nearest-date)
    (let ((cursor-date (calendar-cursor-to-date t)))
      (setq calendar-total-months (max 3 (+ calendar-total-months arg)))
      (calendar-recompute-layout-variables)
      (pcase-let* ((`(,m1 ,y1 ,_ ,_) (calendar-get-month-range))
                   (offset (- (calendar-interval
                               m1 y1
                               (calendar-extract-month cursor-date)
                               (calendar-extract-year cursor-date))
                              calendar-total-months)))
        (if (< offset 0)
            (calendar-redraw)
          (calendar-increment-month m1 y1 (+ 2 offset))
          (calendar-generate-window m1 y1)
          (calendar-cursor-to-visible-date cursor-date)
          (calendar-update-mode-line)))))))

;;;###cal-autoload
(defun calendar-show-fewer-months (&optional arg)
  "Show ARG fewer months on the right in the calendar.
If the date corresponding to current cursor position is beyond the new
calendar range, scroll the calendar left so that the date remains in the
view."
  (interactive "p" calendar-mode)
  (calendar-show-more-months (- arg)))

(provide 'cal-move)

;;; cal-move.el ends here
