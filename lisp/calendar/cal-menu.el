;;; cal-menu.el --- calendar functions for menu bar and popup menu support

;; Copyright (C) 1994, 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;	Lara Rios <lrios@coewl.cen.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, popup menus, menu bar

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

;; This collection of functions implements menu bar and popup menu support for
;; calendar.el.

;;; Code:

;; The code in this file is only called from calendar.el, but can't
;; require it (to supress undefined function warnings from compiler)
;; without a recursive require.
;; All these functions are either autoloaded, or autoloaded or defined
;; in calendar.el.
(declare-function calendar-increment-month "calendar" (n &optional mon yr))
(declare-function calendar-month-name      "calendar" (month &optional abbrev))
(declare-function extract-calendar-year    "calendar" (date))
(declare-function calendar-cursor-to-date  "calendar" (&optional error))
(declare-function holiday-list             "holidays" (y1 y2 &optional l label))
(declare-function calendar-sunrise-sunset  "solar"    nil)
(declare-function calendar-current-date    "calendar" nil)
(declare-function calendar-cursor-holidays "holidays" nil)
(declare-function calendar-date-string     "calendar"
                  (date &optional abbreviate nodayname))
(declare-function insert-diary-entry       "diary-lib" (arg))
(declare-function calendar-set-mark        "calendar"  (arg))
(declare-function cal-tex-cursor-day       "cal-tex"   (&optional arg))
(declare-function cal-tex-cursor-week      "cal-tex"   (&optional arg))
(declare-function cal-tex-cursor-week2     "cal-tex"   (&optional arg))
(declare-function cal-tex-cursor-week-iso  "cal-tex"   (&optional arg))
(declare-function cal-tex-cursor-week-monday     "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-filofax-daily   "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-filofax-2week   "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-filofax-week    "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-month           "cal-tex"    (arg))
(declare-function cal-tex-cursor-month-landscape "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-year            "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-filofax-year    "cal-tex"    (&optional arg))
(declare-function cal-tex-cursor-year-landscape  "cal-tex"    (&optional arg))
(declare-function calendar-day-of-year-string    "calendar"   (&optional date))
(declare-function calendar-iso-date-string       "cal-iso"    (&optional date))
(declare-function calendar-julian-date-string    "cal-julian" (&optional date))
(declare-function calendar-astro-date-string     "cal-julian" (&optional date))
(declare-function calendar-absolute-from-gregorian "calendar" (date))
(declare-function calendar-hebrew-date-string    "cal-hebrew" (&optional date))
(declare-function calendar-persian-date-string   "cal-persia" (&optional date))
(declare-function calendar-bahai-date-string     "cal-bahai"  (&optional date))
(declare-function calendar-islamic-date-string   "cal-islam"  (&optional date))
(declare-function calendar-chinese-date-string   "cal-china"  (&optional date))
(declare-function calendar-coptic-date-string    "cal-coptic" (&optional date))
(declare-function calendar-ethiopic-date-string  "cal-coptic" (&optional date))
(declare-function calendar-french-date-string    "cal-french" (&optional date))
(declare-function calendar-mayan-date-string     "cal-mayan"  (&optional date))
(declare-function calendar-print-chinese-date    "cal-china"  nil)
(declare-function calendar-goto-date             "cal-move"   (date))

(defvar displayed-year)

(defconst cal-menu-moon-menu
  '("Moon"
    ["Lunar Phases" calendar-phases-of-moon]))

(defconst cal-menu-diary-menu
  '("Diary"
    ["Other File" view-other-diary-entries]
    ["Cursor Date" diary-view-entries]
    ["Mark All" mark-diary-entries]
    ["Show All" diary-show-all-entries]
    ["Insert Diary Entry" insert-diary-entry]
    ["Insert Weekly" insert-weekly-diary-entry]
    ["Insert Monthly" insert-monthly-diary-entry]
    ["Insert Yearly" insert-yearly-diary-entry]
    ["Insert Anniversary" insert-anniversary-diary-entry]
    ["Insert Block" insert-block-diary-entry]
    ["Insert Cyclic" insert-cyclic-diary-entry]
    ("Insert Baha'i"
     [" " nil :suffix (calendar-bahai-date-string (calendar-cursor-to-date))]
     ["One time" diary-bahai-insert-entry]
     ["Monthly" diary-bahai-insert-monthly-entry]
     ["Yearly" diary-bahai-insert-yearly-entry])
    ("Insert Islamic"
     [" " nil :suffix (calendar-islamic-date-string (calendar-cursor-to-date))]
     ["One time" insert-islamic-diary-entry]
     ["Monthly" insert-monthly-islamic-diary-entry]
     ["Yearly" insert-yearly-islamic-diary-entry])
    ("Insert Hebrew"
     [" " nil :suffix (calendar-hebrew-date-string (calendar-cursor-to-date))]
     ["One time" insert-hebrew-diary-entry]
     ["Monthly" insert-monthly-hebrew-diary-entry]
     ["Yearly" insert-yearly-hebrew-diary-entry])))

(defun cal-menu-holiday-window-suffix ()
  "Return a string suffix for the \"Window\" entry in `cal-menu-holidays-menu'."
  (let ((my1 (calendar-increment-month -1))
        (my2 (calendar-increment-month 1)))
    (if (= (cdr my1) (cdr my2))
        (format "%s-%s, %d"
                (calendar-month-name (car my1) 'abbrev)
                (calendar-month-name (car my2) 'abbrev)
                (cdr my2))
      (format "%s, %d-%s, %d"
              (calendar-month-name (car my1) 'abbrev)
              (cdr my1)
              (calendar-month-name (car my2) 'abbrev)
              (cdr my2)))))

(defconst cal-menu-holidays-menu
  `("Holidays"
    ["For Cursor Date -" calendar-cursor-holidays
     :suffix (calendar-date-string (calendar-cursor-to-date) t t)
     :visible (calendar-cursor-to-date)]
    ["For Window -" calendar-list-holidays
     :suffix (cal-menu-holiday-window-suffix)]
    ["For Today -" cal-menu-today-holidays
     :suffix (calendar-date-string (calendar-current-date) t t)]
    "--"
    ,@(let ((l ()))
        ;; Show 11 years--5 before, 5 after year of middle month.
        ;; We used to use :suffix rather than :label and bumped into
        ;; an easymenu bug:
        ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01813.html
        ;; The bug has since been fixed.
        (dotimes (i 11)
          (push (vector (format "hol-year-%d" i)
                        `(lambda ()
                           (interactive)
                           (holiday-list (+ displayed-year ,(- i 5))
                                         (+ displayed-year ,(- i 5))))
                        :label `(format "For Year %d"
                                       (+ displayed-year ,(- i 5))))
                l))
        (nreverse l))
    "--"
    ["Unmark Calendar" calendar-unmark]
    ["Mark Holidays" mark-calendar-holidays]))

(defconst cal-menu-goto-menu
  '("Goto"
    ["Today" calendar-goto-today]
    ["Beginning of Week" calendar-beginning-of-week]
    ["End of Week" calendar-end-of-week]
    ["Beginning of Month" calendar-beginning-of-month]
    ["End of Month" calendar-end-of-month]
    ["Beginning of Year" calendar-beginning-of-year]
    ["End of Year" calendar-end-of-year]
    ["Other Date" calendar-goto-date]
    ["Day of Year" calendar-goto-day-of-year]
    ["ISO Week" calendar-goto-iso-week]
    ["ISO Date" calendar-goto-iso-date]
    ["Astronomical Date" calendar-goto-astro-day-number]
    ["Hebrew Date" calendar-goto-hebrew-date]
    ["Persian Date" calendar-goto-persian-date]
    ["Baha'i Date" calendar-bahai-goto-date]
    ["Islamic Date" calendar-goto-islamic-date]
    ["Julian Date" calendar-goto-julian-date]
    ["Chinese Date" calendar-goto-chinese-date]
    ["Coptic Date" calendar-goto-coptic-date]
    ["Ethiopic Date" calendar-goto-ethiopic-date]
    ("Mayan Date"
     ["Next Tzolkin" calendar-next-tzolkin-date]
     ["Previous Tzolkin" calendar-previous-tzolkin-date]
     ["Next Haab" calendar-next-haab-date]
     ["Previous Haab" calendar-previous-haab-date]
     ["Next Round" calendar-next-calendar-round-date]
     ["Previous Round" calendar-previous-calendar-round-date])
    ["French Date" calendar-goto-french-date]))

(defconst cal-menu-scroll-menu
  '("Scroll"
    ["Forward 1 Month" calendar-scroll-left]
    ["Forward 3 Months" calendar-scroll-left-three-months]
    ["Forward 1 Year" "4\C-v"]
    ["Backward 1 Month" calendar-scroll-right]
    ["Backward 3 Months" calendar-scroll-right-three-months]
    ["Backward 1 Year" "4\ev"]))

(defun cal-menu-x-popup-menu (position menu)
  "Like `x-popup-menu', but print an error message if popups are unavailable.
POSITION and MENU are passed to `x-popup-menu'."
  (if (display-popup-menus-p)
      (x-popup-menu position menu)
    (error "Popup menus are not available on this system")))

(defun cal-menu-list-holidays-year ()
  "Display a list of the holidays of the selected date's year."
  (interactive)
  (let ((year (extract-calendar-year (calendar-cursor-to-date))))
    (holiday-list year year)))

(defun cal-menu-list-holidays-following-year ()
  "Display a list of the holidays of the following year."
  (interactive)
  (let ((year (1+ (extract-calendar-year (calendar-cursor-to-date)))))
    (holiday-list year year)))

(defun cal-menu-list-holidays-previous-year ()
  "Display a list of the holidays of the previous year."
  (interactive)
  (let ((year (1- (extract-calendar-year (calendar-cursor-to-date)))))
    (holiday-list year year)))

(defun calendar-event-to-date (&optional error)
  "Date of last event.
If event is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (with-current-buffer
      (window-buffer (posn-window (event-start last-input-event)))
    (goto-char (posn-point (event-start last-input-event)))
    (calendar-cursor-to-date error)))

(defun calendar-mouse-sunrise/sunset ()
  "Show sunrise/sunset times for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (calendar-sunrise-sunset)))

(defun cal-menu-today-holidays ()
  "Show holidays for today's date."
  (interactive)
  (save-excursion
    (calendar-cursor-to-date (calendar-current-date))
    (calendar-cursor-holidays)))

(autoload 'calendar-check-holidays "holidays")
(autoload 'diary-list-entries "diary-lib")

(defun calendar-mouse-holidays (&optional event)
  "Pop up menu of holidays for mouse selected date.
EVENT is the event that invoked this command."
  (interactive "e")
  (let* ((date (calendar-event-to-date))
         (l (mapcar 'list (calendar-check-holidays date)))
         (selection
          (cal-menu-x-popup-menu
           event
           (list
            (format "Holidays for %s" (calendar-date-string date))
            (append
             (list (format "Holidays for %s" (calendar-date-string date)))
             (if l l '("None")))))))
    (and selection (call-interactively selection))))

(defvar holidays-in-diary-buffer)       ; only called from calendar.el

(defun calendar-mouse-view-diary-entries (&optional date diary event)
  "Pop up menu of diary entries for mouse-selected date.
Use optional DATE and alternative file DIARY.  EVENT is the event
that invoked this command.  Shows holidays if `holidays-in-diary-buffer'
is non-nil."
  (interactive "i\ni\ne")
  (let* ((date (or date (calendar-event-to-date)))
         (diary-file (if diary diary diary-file))
         (diary-list-include-blanks nil)
         (diary-display-hook 'ignore)
         (diary-entries
          (mapcar (lambda (x) (split-string (cadr x) "\n"))
                  (diary-list-entries date 1 'list-only)))
         (holidays (if holidays-in-diary-buffer
                       (calendar-check-holidays date)))
         (title (concat "Diary entries "
                        (if diary (format "from %s " diary) "")
                        "for "
                        (calendar-date-string date)))
         (selection
          (cal-menu-x-popup-menu
           event
           (list title
                 (append
                  (list title)
                  (mapcar (lambda (x) (list (concat "     " x))) holidays)
                  (if holidays
                      (list "--shadow-etched-in" "--shadow-etched-in"))
                  (if diary-entries
                      (mapcar 'list (apply 'append diary-entries))
                    '("None")))))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-other-diary-entries ()
  "Pop up menu of diary entries from alternative file on mouse-selected date."
  (interactive)
  (calendar-mouse-view-diary-entries
   (calendar-event-to-date)
   (read-file-name "Enter diary file name: " default-directory nil t)))

(defun calendar-mouse-insert-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (insert-diary-entry nil)))

(defun calendar-mouse-set-mark ()
  "Mark the date under the cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (calendar-set-mark nil)))

(defun cal-tex-mouse-day ()
  "Make a buffer with LaTeX commands for the day mouse is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-day nil)))

(defun cal-tex-mouse-week ()
  "One page calendar for week indicated by cursor.
Holidays are included if `cal-tex-holidays' is t."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week nil)))

(defun cal-tex-mouse-week2 ()
  "Make a buffer with LaTeX commands for the week cursor is on.
The printed output will be on two pages."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week2 nil)))

(defun cal-tex-mouse-week-iso ()
  "One page calendar for week indicated by cursor.
Holidays are included if `cal-tex-holidays' is t."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week-iso nil)))

(defun cal-tex-mouse-week-monday ()
  "One page calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-week-monday nil)))

(defun cal-tex-mouse-filofax-daily ()
  "Day-per-page Filofax calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-daily nil)))

(defun cal-tex-mouse-filofax-2week ()
  "One page Filofax calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-2week nil)))

(defun cal-tex-mouse-filofax-week ()
  "Two page Filofax calendar for week indicated by cursor."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-week nil)))

(defun cal-tex-mouse-month ()
  "Make a buffer with LaTeX commands for the month cursor is on.
Calendar is condensed onto one page."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-month nil)))

(defun cal-tex-mouse-month-landscape ()
  "Make a buffer with LaTeX commands for the month cursor is on.
The output is in landscape format, one month to a page."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-month-landscape nil)))

(defun cal-tex-mouse-year ()
  "Make a buffer with LaTeX commands for the year cursor is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-year nil)))

(defun cal-tex-mouse-filofax-year ()
  "Make a buffer with LaTeX commands for Filofax calendar of year cursor is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-filofax-year nil)))

(defun cal-tex-mouse-year-landscape ()
  "Make a buffer with LaTeX commands for the year cursor is on."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (cal-tex-cursor-year-landscape nil)))

(defun calendar-mouse-print-dates (&optional event)
  "Pop up menu of equivalent dates to mouse selected date.
EVENT is the event that invoked this command."
  (interactive "e")
  (let* ((date (calendar-event-to-date))
        (selection
         (cal-menu-x-popup-menu
          event
          (list
           (concat (calendar-date-string date) " (Gregorian)")
           (append
            (list
             (concat (calendar-date-string date) " (Gregorian)")
             (list (calendar-day-of-year-string date))
             (list (format "ISO date: %s" (calendar-iso-date-string date)))
             (list (format "Julian date: %s"
                           (calendar-julian-date-string date)))
             (list
              (format "Astronomical (Julian) day number (at noon UTC): %s.0"
                           (calendar-astro-date-string date)))
             (list
              (format "Fixed (RD) date: %s"
                      (calendar-absolute-from-gregorian date)))
             (list (format "Hebrew date (before sunset): %s"
                           (calendar-hebrew-date-string date)))
             (list (format "Persian date: %s"
                           (calendar-persian-date-string date)))
             (list (format "Baha'i date (before sunset): %s"
                           (calendar-bahai-date-string date))))
            (let ((i (calendar-islamic-date-string date)))
              (if (not (string-equal i ""))
                  (list (list (format "Islamic date (before sunset): %s" i)))))
            (list
             (list (format "Chinese date: %s"
                           (calendar-chinese-date-string date))))
            ;; (list '("Chinese date (select to echo Chinese date)"
            ;;         . calendar-mouse-chinese-date))
            (let ((c (calendar-coptic-date-string date)))
              (if (not (string-equal c ""))
                  (list (list (format "Coptic date: %s" c)))))
            (let ((e (calendar-ethiopic-date-string date)))
              (if (not (string-equal e ""))
                  (list (list (format "Ethiopic date: %s" e)))))
            (let ((f (calendar-french-date-string date)))
              (if (not (string-equal f ""))
                  (list (list (format "French Revolutionary date: %s" f)))))
            (list
             (list
              (format "Mayan date: %s"
                      (calendar-mayan-date-string date)))))))))
        (and selection (call-interactively selection))))

(defun calendar-mouse-chinese-date ()
  "Show Chinese equivalent for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-mouse-goto-date (calendar-event-to-date))
    (calendar-print-chinese-date)))

(defun calendar-mouse-goto-date (date)
  "Goto DATE in the buffer specified by `last-input-event'."
  (set-buffer (window-buffer (posn-window (event-start last-input-event))))
  (calendar-goto-date date))

(defun cal-menu-set-date-title (menu)
  "Convert date of last event to title suitable for MENU."
  (easy-menu-filter-return
   menu (calendar-date-string (calendar-event-to-date t) t nil)))

(easy-menu-define cal-menu-context-mouse-menu nil
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  '("foo" :filter cal-menu-set-date-title
    "--"
    ["Holidays" calendar-mouse-holidays]
    ["Mark date" calendar-mouse-set-mark]
    ["Sunrise/sunset" calendar-mouse-sunrise/sunset]
    ["Other calendars" calendar-mouse-print-dates]
    ("Prepare LaTeX buffer"
     ["Daily (1 page)" cal-tex-mouse-day]
     ["Weekly (1 page)" cal-tex-mouse-week]
     ["Weekly (2 pages)" cal-tex-mouse-week2]
     ["Weekly (other style; 1 page)" cal-tex-mouse-week-iso]
     ["Weekly (yet another style; 1 page)" cal-tex-mouse-week-monday]
     ["Monthly" cal-tex-mouse-month]
     ["Monthly (landscape)" cal-tex-mouse-month-landscape]
     ["Yearly" cal-tex-mouse-year]
     ["Yearly (landscape)" cal-tex-mouse-year-landscape]
     ("Filofax styles"
      ["Filofax Daily (one-day-per-page)" cal-tex-mouse-filofax-daily]
      ["Filofax Weekly (2-weeks-at-a-glance)" cal-tex-mouse-filofax-2week]
      ["Filofax Weekly (week-at-a-glance)" cal-tex-mouse-filofax-week]
      ["Filofax Yearly" cal-tex-mouse-filofax-year]))
    ["Diary entries" calendar-mouse-view-diary-entries]
    ["Insert diary entry" calendar-mouse-insert-diary-entry]
    ["Other diary file entries" calendar-mouse-view-other-diary-entries]))

(easy-menu-define cal-menu-global-mouse-menu nil
  "Menu bound to a mouse event, not specific to the mouse-click location."
  '("Calendar"
    ["Scroll forward" calendar-scroll-left-three-months]
    ["Scroll backward" calendar-scroll-right-three-months]
    ["Mark diary entries" mark-diary-entries]
    ["List holidays" calendar-list-holidays]
    ["Mark holidays" calendar-mark-holidays]
    ["Unmark" calendar-unmark]
    ["Lunar phases" calendar-phases-of-moon]
    ["Show diary" diary-show-all-entries]
    ["Exit calendar" exit-calendar]))

(run-hooks 'cal-menu-load-hook)

(provide 'cal-menu)

;; arch-tag: aa81cf73-ce89-48a4-97ec-9ef861e87fe9
;;; cal-menu.el ends here
