;;; cal-menu.el --- calendar functions for menu bar and popup menu support

;; Copyright (C) 1994 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This collection of functions implements menu bar and popup menu support for
;; calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(define-key calendar-mode-map [menu-bar edit] 'undefined)

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
  '("Insert Daily". insert-diary-entry))
(define-key calendar-mode-map [menu-bar diary all]
  '("Show All" . show-all-diary-entries))
(define-key calendar-mode-map [menu-bar diary mark]
 '("Mark All" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Cursor Date" . view-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Other File" . view-other-diary-entries))

(define-key calendar-mode-map [menu-bar holidays]
  (cons "Holidays" (make-sparse-keymap "Holidays")))

(define-key calendar-mode-map [menu-bar holidays unmark]
  '("Unmark" . calendar-unmark))
(define-key calendar-mode-map [menu-bar holidays mark]
  '("Mark" . mark-calendar-holidays))
(define-key calendar-mode-map [menu-bar holidays 3-mon]
  '("3 Months" . list-calendar-holidays))
(define-key calendar-mode-map [menu-bar holidays 1-day]
  '("One Day" . calendar-cursor-holidays))

(define-key calendar-mode-map [menu-bar goto]
  (cons "Goto" (make-sparse-keymap "Goto")))

(define-key calendar-mode-map [menu-bar goto french]
  '("French Date" . calendar-goto-french-date))
(define-key calendar-mode-map [menu-bar goto mayan]
  (cons "Mayan Date" (make-sparse-keymap "Mayan")))
(define-key calendar-mode-map [menu-bar goto julian]
  '("Julian Date" . calendar-goto-julian-date))
(define-key calendar-mode-map [menu-bar goto islamic]
  '("Islamic Date" . calendar-goto-islamic-date))
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

(put 'calendar-forward-day 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-backward-day 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-forward-week 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-backward-week 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-forward-month 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-backward-month 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-forward-year 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-backward-year 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-beginning-of-year 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-end-of-year 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-beginning-of-month 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-end-of-month 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-end-of-week 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-beginning-of-week 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-mouse-print-dates 'menu-enable '(calendar-event-to-date))
(put 'calendar-sunrise-sunset 'menu-enable '(calendar-event-to-date))
(put 'calendar-cursor-holidays 'menu-enable '(calendar-cursor-to-date))
(put 'view-diary-entries 'menu-enable '(calendar-cursor-to-date))
(put 'view-other-diary-entries 'menu-enable '(calendar-cursor-to-date))
(put 'calendar-mouse-insert-hebrew-diary-entry
     'menu-enable
     '(calendar-cursor-to-date))
(put 'calendar-mouse-insert-islamic-diary-entry
     'menu-enable
     '(calendar-cursor-to-date))
(put 'insert-cyclic-diary-entry 'menu-enable '(calendar-cursor-to-date))
(put 'insert-block-diary-entry 'menu-enable '(calendar-cursor-to-date))
(put 'insert-anniversary-diary-entry 'menu-enable '(calendar-cursor-to-date))
(put 'insert-yearly-diary-entry 'menu-enable '(calendar-cursor-to-date))
(put 'insert-monthly-diary-entry 'menu-enable '(calendar-cursor-to-date))
(put 'insert-weekly-diary-entry 'menu-enable '(calendar-cursor-to-date))

(defun calendar-event-to-date (&optional error)
  "Date of last event.
If event is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (save-excursion
    (goto-char (posn-point (event-start last-input-event)))
    (calendar-cursor-to-date error)))

(defun calendar-mouse-insert-hebrew-diary-entry (event)
  "Pop up menu to insert a Hebrew-date diary entry."
  (interactive "e")
  (let ((hebrew-selection
         (x-popup-menu
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
         (x-popup-menu
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
    (calendar-goto-date (calendar-event-to-date))
    (calendar-sunrise-sunset)))

(defun calendar-mouse-holidays ()
  "Show holidays for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date (calendar-event-to-date))
    (calendar-cursor-holidays)))

(defun calendar-mouse-view-diary-entries ()
  "View diary entries on mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date (calendar-event-to-date))
    (view-diary-entries 1)))

(defun calendar-mouse-view-other-diary-entries ()
  "View diary entries from alternative file on mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date (calendar-event-to-date))
    (call-interactively 'view-other-diary-entries)))

(defun calendar-mouse-insert-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date (calendar-event-to-date))
    (insert-diary-entry nil)))

(defun calendar-mouse-set-mark ()
  "Mark the date under the cursor."
  (interactive)
  (save-excursion
    (calendar-goto-date (calendar-event-to-date))
    (calendar-set-mark nil)))

(defun calendar-mouse-print-dates ()
  "Pop up menu of equivalent dates to mouse selected date."
  (interactive)
  (let ((date (calendar-event-to-date)))
    (x-popup-menu
     event
     (list
      "Date Menu"
      (append
       (list
        (concat (calendar-date-string date) " (Gregorian)")
        (list (calendar-day-of-year-string date))
        (list (format "ISO date: %s" (calendar-iso-date-string date)))
        (list (format "Julian date: %s" (calendar-julian-date-string date)))
        (list (format "Astronomical (Julian) date (before noon): %s"
                      (calendar-astro-date-string date)))
        (list (format "Hebrew date (before sunset): %s"
                      (calendar-hebrew-date-string date))))
       (let ((i (calendar-islamic-date-string date)))
         (if (not (string-equal i ""))
             (list (list (format "Islamic date (before sunset): %s" i)))))
       (let ((f (calendar-french-date-string date)))
         (if (not (string-equal f ""))
             (list (list (format "French Revolutionary date: %s" f)))))
       (list
	(list
         (format "Mayan date: %s" (calendar-mayan-date-string date)))))))))

(defun calendar-mouse-2-date-menu (event)
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  (interactive "e")
  (let* ((date (calendar-event-to-date t))
         (selection
          (x-popup-menu
           event
           (list "Menu"
                 (list
                  (calendar-date-string date t t)
                  '("Holidays" . calendar-mouse-holidays)
                  '("Mark date" . calendar-mouse-set-mark)
                  '("Sunrise/sunset" . calendar-mouse-sunrise/sunset)
                  '("Other calendars" . calendar-mouse-print-dates)
                  '("Diary entries" . calendar-mouse-view-diary-entries)
                  '("Insert diary entry" . calendar-mouse-insert-diary-entry)
                  '("Other diary file entries"
                    . calendar-mouse-view-other-diary-entries)
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
