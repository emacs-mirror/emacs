;;; cal-menu.el --- calendar functions for menu bar and popup menu support

;; Copyright (C) 1994, 1995, 2001 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;	Lara Rios <lrios@coewl.cen.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: calendar, popup menus, menu bar

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

;; This collection of functions implements menu bar and popup menu support for
;; calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(eval-when-compile (require 'calendar))
(require 'easymenu)

(define-key calendar-mode-map [menu-bar edit] 'undefined)
(define-key calendar-mode-map [menu-bar search] 'undefined)

(define-key calendar-mode-map [down-mouse-2] 'calendar-mouse-2-date-menu)
(define-key calendar-mode-map [mouse-2] 'ignore)

(defvar calendar-mouse-3-map (make-sparse-keymap "Calendar"))
(define-key calendar-mode-map [down-mouse-3] calendar-mouse-3-map)
(define-key calendar-mode-map [C-down-mouse-3] calendar-mouse-3-map)

(define-key calendar-mode-map [menu-bar moon]
  (cons "Moon" (make-sparse-keymap "Moon")))

(define-key calendar-mode-map [menu-bar moon moon]
  '("Lunar Phases" . calendar-phases-of-moon))

(define-key calendar-mode-map [menu-bar diary]
  (cons "Diary" (make-sparse-keymap "Diary")))

(define-key calendar-mode-map [menu-bar diary heb]
  '("Insert Hebrew" . calendar-mouse-insert-hebrew-diary-entry))
(define-key calendar-mode-map [menu-bar diary isl]
  '("Insert Islamic" . calendar-mouse-insert-islamic-diary-entry))
(define-key calendar-mode-map [menu-bar diary cyc]
  '("Insert Cyclic" . insert-cyclic-diary-entry))
(define-key calendar-mode-map [menu-bar diary blk]
  '("Insert Block" . insert-block-diary-entry))
(define-key calendar-mode-map [menu-bar diary ann]
  '("Insert Anniversary" . insert-anniversary-diary-entry))
(define-key calendar-mode-map [menu-bar diary yr]
  '("Insert Yearly" . insert-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar diary mon]
  '("Insert Monthly" . insert-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar diary wk]
  '("Insert Weekly" . insert-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar diary ent]
  '("Insert Diary Entry" . insert-diary-entry))
(define-key calendar-mode-map [menu-bar diary all]
  '("Show All" . show-all-diary-entries))
(define-key calendar-mode-map [menu-bar diary mark]
 '("Mark All" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Cursor Date" . view-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Other File" . view-other-diary-entries))

(define-key calendar-mode-map [menu-bar Holidays]
  (cons "Holidays" (make-sparse-keymap "Holidays")))

(define-key calendar-mode-map [menu-bar goto]
  (cons "Goto" (make-sparse-keymap "Goto")))

(define-key calendar-mode-map [menu-bar goto french]
  '("French Date" . calendar-goto-french-date))
(define-key calendar-mode-map [menu-bar goto mayan]
  (cons "Mayan Date" (make-sparse-keymap "Mayan")))
(define-key calendar-mode-map [menu-bar goto ethiopic]
  '("Ethiopic Date" . calendar-goto-ethiopic-date))
(define-key calendar-mode-map [menu-bar goto coptic]
  '("Coptic Date" . calendar-goto-coptic-date))
(define-key calendar-mode-map [menu-bar goto chinese]
  '("Chinese Date" . calendar-goto-chinese-date))
(define-key calendar-mode-map [menu-bar goto julian]
  '("Julian Date" . calendar-goto-julian-date))
(define-key calendar-mode-map [menu-bar goto islamic]
  '("Islamic Date" . calendar-goto-islamic-date))
(define-key calendar-mode-map [menu-bar goto persian]
  '("Persian Date" . calendar-goto-persian-date))
(define-key calendar-mode-map [menu-bar goto hebrew]
  '("Hebrew Date" . calendar-goto-hebrew-date))
(define-key calendar-mode-map [menu-bar goto astro]
  '("Astronomical Date" . calendar-goto-astro-day-number))
(define-key calendar-mode-map [menu-bar goto iso]
  '("ISO Date" . calendar-goto-iso-date))
(define-key calendar-mode-map [menu-bar goto gregorian]
  '("Other Date" . calendar-goto-date))
(define-key calendar-mode-map [menu-bar goto end-of-year]
  '("End of Year" . calendar-end-of-year))
(define-key calendar-mode-map [menu-bar goto beginning-of-year]
  '("Beginning of Year" . calendar-beginning-of-year))
(define-key calendar-mode-map [menu-bar goto end-of-month]
  '("End of Month" . calendar-end-of-month))
(define-key calendar-mode-map [menu-bar goto beginning-of-month]
  '("Beginning of Month" . calendar-beginning-of-month))
(define-key calendar-mode-map [menu-bar goto end-of-week]
  '("End of Week" . calendar-end-of-week))
(define-key calendar-mode-map [menu-bar goto beginning-of-week]
  '("Beginning of Week" . calendar-beginning-of-week))
(define-key calendar-mode-map [menu-bar goto today]
  '("Today" . calendar-goto-today))


(define-key calendar-mode-map [menu-bar goto mayan prev-rnd]
  '("Previous Round" . calendar-previous-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto mayan nxt-rnd]
  '("Next Round" . calendar-next-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto mayan prev-haab]
  '("Previous Haab" . calendar-previous-haab-date))
(define-key calendar-mode-map [menu-bar goto mayan next-haab]
  '("Next Haab" . calendar-next-haab-date))
(define-key calendar-mode-map [menu-bar goto mayan prev-tzol]
  '("Previous Tzolkin" . calendar-previous-tzolkin-date))
(define-key calendar-mode-map [menu-bar goto mayan next-tzol]
  '("Next Tzolkin" . calendar-next-tzolkin-date))

(define-key calendar-mode-map [menu-bar scroll]
  (cons "Scroll" (make-sparse-keymap "Scroll")))

(define-key calendar-mode-map [menu-bar scroll bk-12]
  '("Backward 1 Year" . "4\ev"))
(define-key calendar-mode-map [menu-bar scroll bk-3]
  '("Backward 3 Months" . scroll-calendar-right-three-months))
(define-key calendar-mode-map [menu-bar scroll bk-1]
  '("Backward 1 Month" . scroll-calendar-right))
(define-key calendar-mode-map [menu-bar scroll fwd-12]
  '("Forward 1 Year" . "4\C-v"))
(define-key calendar-mode-map [menu-bar scroll fwd-3]
  '("Forward 3 Months" . scroll-calendar-left-three-months))
(define-key calendar-mode-map [menu-bar scroll fwd-1]
  '("Forward 1 Month" . scroll-calendar-left))

(defun cal-menu-x-popup-menu (position menu)
  "Like `x-popup-menu', but prints an error message if popup menus are
not available."
  (if (display-popup-menus-p)
      (x-popup-menu position menu)
    (error "Popup menus are not available on this system")))

(defun cal-menu-list-holidays-year ()
  "Display a list of the holidays of the selected date's year."
  (interactive)
  (let ((year (extract-calendar-year (calendar-cursor-to-date))))
    (list-holidays year year)))

(defun cal-menu-list-holidays-following-year ()
  "Display a list of the holidays of the following year."
  (interactive)
  (let ((year (1+ (extract-calendar-year (calendar-cursor-to-date)))))
    (list-holidays year year)))

(defun cal-menu-list-holidays-previous-year ()
  "Display a list of the holidays of the previous year."
  (interactive)
  (let ((year (1- (extract-calendar-year (calendar-cursor-to-date)))))
    (list-holidays year year)))

(defun cal-menu-update ()
  ;; Update the holiday part of calendar menu bar for the current display.
  (condition-case nil
      (if (eq major-mode 'calendar-mode)
          (let ((l))
            (calendar-for-loop;; Show 11 years--5 before, 5 after year of
                   ;; middle month
             i from (- displayed-year 5) to (+ displayed-year 5) do
             (setq l (cons (vector (format "For Year %s" i)
                                   (list (list 'lambda 'nil '(interactive)
                                               (list 'list-holidays i i)))
                                   t)
                           l)))
            (setq l (cons ["Mark Holidays" mark-calendar-holidays t]
                          (cons ["Unmark Calendar" calendar-unmark t]
                                (cons ["--" '("--") t] l))))
            (easy-menu-change nil "Holidays" (nreverse l))
            (define-key calendar-mode-map [menu-bar Holidays separator]
              '("--"))
            (define-key calendar-mode-map [menu-bar Holidays today]
                `(,(format "For Today (%s)"
                           (calendar-date-string (calendar-current-date) t t))
                  . cal-menu-today-holidays))
            (let ((title
                   (let ((m1 displayed-month)
                         (y1 displayed-year)
                         (m2 displayed-month)
                         (y2 displayed-year))
                     (increment-calendar-month m1 y1 -1)
                     (increment-calendar-month m2 y2 1)
                     (if (= y1 y2)
                         (format "%s-%s, %d"
                                 (calendar-month-name m1 3)
                                 (calendar-month-name m2 3)
                                 y2)
                       (format "%s, %d-%s, %d"
                               (calendar-month-name m1 3)
                               y1
                               (calendar-month-name m2 3)
                               y2)))))
              (define-key  calendar-mode-map [menu-bar Holidays 3-month]
                `(,(format "For Window (%s)" title)
                  . list-calendar-holidays)))
            (let ((date (calendar-cursor-to-date)))
              (if date
                  (define-key calendar-mode-map [menu-bar Holidays 1-day]
                    `(,(format "For Cursor Date (%s)"
                               (calendar-date-string date t t))
                      . calendar-cursor-holidays))))))
    ;; Try to avoid entering infinite beep mode in case of errors.
    (error (ding))))

(defun calendar-event-to-date (&optional error)
  "Date of last event.
If event is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-start last-input-event))))
    (goto-char (posn-point (event-start last-input-event)))
    (calendar-cursor-to-date error)))

(defun calendar-mouse-insert-hebrew-diary-entry (event)
  "Pop up menu to insert a Hebrew-date diary entry."
  (interactive "e")
  (let ((hebrew-selection
         (cal-menu-x-popup-menu
          event
          (list "Hebrew insert menu"
                (list (calendar-hebrew-date-string (calendar-cursor-to-date))
                      '("One time" . insert-hebrew-diary-entry)
                      '("Monthly" . insert-monthly-hebrew-diary-entry)
                      '("Yearly" . insert-yearly-hebrew-diary-entry))))))
    (and hebrew-selection (call-interactively hebrew-selection))))

(defun calendar-mouse-insert-islamic-diary-entry (event)
  "Pop up menu to insert an Islamic-date diary entry."
  (interactive "e")
  (let ((islamic-selection
         (cal-menu-x-popup-menu
          event
          (list "Islamic insert menu"
                (list (calendar-islamic-date-string (calendar-cursor-to-date))
                      '("One time" . insert-islamic-diary-entry)
                      '("Monthly" . insert-monthly-islamic-diary-entry)
                      '("Yearly" . insert-yearly-islamic-diary-entry))))))
    (and islamic-selection (call-interactively islamic-selection))))

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

(defun calendar-mouse-holidays ()
  "Pop up menu of holidays for mouse selected date."
  (interactive)
  (let* ((date (calendar-event-to-date))
         (l (mapcar '(lambda (x) (list x))
                    (check-calendar-holidays date)))
         (selection
          (cal-menu-x-popup-menu
           event
           (list
            (format "Holidays for %s" (calendar-date-string date))
            (append
             (list (format "Holidays for %s" (calendar-date-string date)))
             (if l l '("None")))))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-diary-entries ()
  "Pop up menu of diary entries for mouse selected date."
  (interactive)
  (let* ((date (calendar-event-to-date))
         (l (mapcar '(lambda (x) (list (car (cdr x))))
                    (let ((diary-list-include-blanks nil)
                          (diary-display-hook 'ignore))
                      (list-diary-entries date 1))))
         (selection
          (cal-menu-x-popup-menu
           event
           (list
            (format "Diary entries for %s" (calendar-date-string date))
            (append
             (list (format "Diary entries for %s" (calendar-date-string date)))
             (if l l '("None")))))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-other-diary-entries ()
  "Pop up menu of diary entries from alternative file on mouse-selected date."
  (interactive)
  (let* ((date (calendar-event-to-date))
         (diary-list-include-blanks nil)
         (diary-display-hook 'ignore)
         (diary-file (read-file-name
                      "Enter diary file name: "
                      default-directory nil t))
         ; The following doesn't really do the right thing.  The problem is
         ; that a newline in the diary entry does not give a newline in a
         ; pop-up menu; for that you need a separate list item.  When the (car
         ; (cdr x)) contains newlines, the item should be split into a list of
         ; items.  Too minor and messy to worry about.
         (l (mapcar '(lambda (x) (list (car (cdr x))))
                    (list-diary-entries date 1)))
         (selection
          (cal-menu-x-popup-menu
           event
           (list
            (format "Diary entries from %s for %s"
                    diary-file
                    (calendar-date-string date))
            (append
             (list (format "Diary entries from %s for %s"
                            diary-file
                           (calendar-date-string date)))
             (if l l '("None")))))))
    (and selection (call-interactively selection))))

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

(defun calendar-mouse-print-dates ()
  "Pop up menu of equivalent dates to mouse selected date."
  (interactive)
  (let ((date (calendar-event-to-date))
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
                           (calendar-persian-date-string date))))
            (let ((i (calendar-islamic-date-string date)))
              (if (not (string-equal i ""))
                  (list (list (format "Islamic date (before sunset): %s" i)))))
            (list
             (list (format "Chinese date: %s"
                           (calendar-chinese-date-string date))))
;            (list '("Chinese date (select to echo Chinese date)"
;                    . calendar-mouse-chinese-date))
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
  (set-buffer (window-buffer (posn-window (event-start last-input-event))))
  (calendar-goto-date date))

(defun calendar-mouse-2-date-menu (event)
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  (interactive "e")
  (let* ((date (calendar-event-to-date t))
         (selection
          (cal-menu-x-popup-menu
           event
           (list (calendar-date-string date t nil)
                 (list
                  ""
                  '("Holidays" . calendar-mouse-holidays)
                  '("Mark date" . calendar-mouse-set-mark)
                  '("Sunrise/sunset" . calendar-mouse-sunrise/sunset)
                  '("Other calendars" . calendar-mouse-print-dates)
                  '("Prepare LaTeX buffer" . calendar-mouse-cal-tex-menu)
                  '("Diary entries" . calendar-mouse-view-diary-entries)
                  '("Insert diary entry" . calendar-mouse-insert-diary-entry)
                  '("Other diary file entries"
                    . calendar-mouse-view-other-diary-entries)
                  )))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-cal-tex-menu (event)
  "Pop up submenu for Mouse-2 for cal-tex commands for selected date in the calendar window."
  (interactive "e")
  (let* ((selection
          (cal-menu-x-popup-menu
           event
           (list (calendar-date-string date t nil)
                 (list
                  ""
                  '("Daily (1 page)" . cal-tex-mouse-day)
                  '("Weekly (1 page)" . cal-tex-mouse-week)
                  '("Weekly (2 pages)" . cal-tex-mouse-week2)
                  '("Weekly (other style; 1 page)" . cal-tex-mouse-week-iso)
                  '("Weekly (yet another style; 1 page)" .
                    cal-tex-mouse-week-monday)
                  '("Monthly" . cal-tex-mouse-month)
                  '("Monthly (landscape)" . cal-tex-mouse-month-landscape)
                  '("Yearly" . cal-tex-mouse-year)
                  '("Yearly (landscape)" . cal-tex-mouse-year-landscape)
                  '("Filofax styles" . cal-tex-mouse-filofax)
                  )))))
    (and selection (call-interactively selection))))

(defun cal-tex-mouse-filofax (event)
  "Pop up sub-submenu for Mouse-2 for Filofax cal-tex commands for selected date."
  (interactive "e")
  (let* ((selection
          (cal-menu-x-popup-menu
           event
           (list (calendar-date-string date t nil)
                 (list
                  ""
                  '("Filofax Daily (one-day-per-page)" .
                    cal-tex-mouse-filofax-daily)
                  '("Filofax Weekly (2-weeks-at-a-glance)" .
                    cal-tex-mouse-filofax-2week)
                  '("Filofax Weekly (week-at-a-glance)" .
                    cal-tex-mouse-filofax-week)
                  '("Filofax Yearly" . cal-tex-mouse-filofax-year)
                  )))))
    (and selection (call-interactively selection))))

(define-key calendar-mouse-3-map [exit-calendar]
  '("Exit calendar" . exit-calendar))
(define-key calendar-mouse-3-map [show-diary]
  '("Show diary" . show-all-diary-entries))
(define-key calendar-mouse-3-map [lunar-phases]
  '("Lunar phases" . calendar-phases-of-moon))
(define-key calendar-mouse-3-map [unmark]
  '("Unmark" . calendar-unmark))
(define-key calendar-mouse-3-map [mark-holidays]
  '("Mark holidays" . mark-calendar-holidays))
(define-key calendar-mouse-3-map [list-holidays]
  '("List holidays" . list-calendar-holidays))
(define-key calendar-mouse-3-map [mark-diary-entries]
  '("Mark diary entries" . mark-diary-entries))
(define-key calendar-mouse-3-map [scroll-backward]
  '("Scroll backward" . scroll-calendar-right-three-months))
(define-key calendar-mouse-3-map [scroll-forward]
  '("Scroll forward" . scroll-calendar-left-three-months))

(run-hooks 'cal-menu-load-hook)

(provide 'cal-menu)

;;; cal-menu.el ends here
