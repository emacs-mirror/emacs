;;; cal-menu.el --- calendar functions for menu bar and popup menu support

;; Copyright (C) 1994, 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;         Lara Rios <lrios@coewl.cen.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, popup menus, menu bar

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See calendar.el.

;;; Code:

(require 'calendar)

(defconst cal-menu-sunmoon-menu
  '("Sun/Moon"
    ["Lunar Phases" calendar-lunar-phases]
    ["Sunrise/sunset for cursor date" calendar-sunrise-sunset]
    ["Sunrise/sunset for cursor month" calendar-sunrise-sunset-month])
  "Key map for \"Sun/Moon\" menu in the calendar.")

(defconst cal-menu-diary-menu
  '("Diary"
    ["Other File" diary-view-other-diary-entries]
    ["Cursor Date" diary-view-entries]
    ["Mark All" diary-mark-entries]
    ["Show All" diary-show-all-entries]
    ["Insert Diary Entry" diary-insert-entry]
    ["Insert Weekly" diary-insert-weekly-entry]
    ["Insert Monthly" diary-insert-monthly-entry]
    ["Insert Yearly" diary-insert-yearly-entry]
    ["Insert Anniversary" diary-insert-anniversary-entry]
    ["Insert Block" diary-insert-block-entry]
    ["Insert Cyclic" diary-insert-cyclic-entry]
    ("Insert Baha'i"
     ["One time" diary-bahai-insert-entry]
     ["Monthly" diary-bahai-insert-monthly-entry]
     ["Yearly" diary-bahai-insert-yearly-entry])
    ("Insert Islamic"
     ["One time" diary-islamic-insert-entry]
     ["Monthly" diary-islamic-insert-monthly-entry]
     ["Yearly" diary-islamic-insert-yearly-entry])
    ("Insert Hebrew"
     ["One time" diary-hebrew-insert-entry]
     ["Monthly" diary-hebrew-insert-monthly-entry]
     ["Yearly" diary-hebrew-insert-yearly-entry]))
    "Key map for \"Diary\" menu in the calendar.")

(defun cal-menu-holiday-window-suffix ()
  "Return a string suffix for the \"Window\" entry in `cal-menu-holidays-menu'."
  (let ((my1 (calendar-increment-month-cons -1))
        (my2 (calendar-increment-month-cons 1)))
    ;; Mon1-Mon2, Year  or  Mon1, Year1-Mon2, Year2.
    (format "%s%s-%s, %d"
            (calendar-month-name (car my1) 'abbrev)
            (if (= (cdr my1) (cdr my2))
                ""
              (format ", %d" (cdr my1)))
            (calendar-month-name (car my2) 'abbrev)
            (cdr my2))))

(defvar displayed-year)                 ; from calendar-generate

(defconst cal-menu-holidays-menu
  `("Holidays"
    ["For Cursor Date -" calendar-cursor-holidays
     :suffix (calendar-date-string (calendar-cursor-to-date) t t)
     :visible (calendar-cursor-to-date)]
    ["For Window -" calendar-list-holidays
     :suffix (cal-menu-holiday-window-suffix)]
    ["For Today -" (calendar-cursor-holidays (calendar-current-date))
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
                           (holiday-list (+ displayed-year ,(- i 5))))
                        :label `(format "For Year %d"
                                       (+ displayed-year ,(- i 5))))
                l))
        (nreverse l))
    "--"
    ["Unmark Calendar" calendar-unmark]
    ["Mark Holidays" calendar-mark-holidays])
  "Key map for \"Holidays\" menu in the calendar.")

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
    ["ISO Week" calendar-iso-goto-week]
    ["ISO Date" calendar-iso-goto-date]
    ["Astronomical Date" calendar-astro-goto-day-number]
    ["Hebrew Date" calendar-hebrew-goto-date]
    ["Persian Date" calendar-persian-goto-date]
    ["Baha'i Date" calendar-bahai-goto-date]
    ["Islamic Date" calendar-islamic-goto-date]
    ["Julian Date" calendar-julian-goto-date]
    ["Chinese Date" calendar-chinese-goto-date]
    ["Coptic Date" calendar-coptic-goto-date]
    ["Ethiopic Date" calendar-ethiopic-goto-date]
    ("Mayan Date"
     ["Next Tzolkin" calendar-mayan-next-tzolkin-date]
     ["Previous Tzolkin" calendar-mayan-previous-tzolkin-date]
     ["Next Haab" calendar-mayan-next-haab-date]
     ["Previous Haab" calendar-mayan-previous-haab-date]
     ["Next Round" calendar-mayan-next-round-date]
     ["Previous Round" calendar-mayan-previous-round-date])
    ["French Date" calendar-french-goto-date])
  "Key map for \"Goto\" menu in the calendar.")

(defconst cal-menu-scroll-menu
  '("Scroll"
    ["Forward 1 Month" calendar-scroll-left]
    ["Forward 3 Months" calendar-scroll-left-three-months]
    ["Forward 1 Year" (calendar-scroll-left 12) :keys "4 C-v"]
    ["Backward 1 Month" calendar-scroll-right]
    ["Backward 3 Months" calendar-scroll-right-three-months]
    ["Backward 1 Year" (calendar-scroll-right 12) :keys "4 M-v"])
  "Key map for \"Scroll\" menu in the calendar.")

(declare-function x-popup-menu "xmenu.c" (position menu))

(defmacro cal-menu-x-popup-menu (event title &rest body)
  "Call `x-popup-menu' at position EVENT, with TITLE and contents BODY.
Signals an error if popups are unavailable."
  (declare (indent 2))
  `(if (display-popup-menus-p)
       (x-popup-menu ,event (list ,title (append (list ,title) ,@body)))
     (error "Popup menus are not available on this system")))

(autoload 'diary-list-entries "diary-lib")
;; Autoloaded in diary-lib.
(declare-function calendar-check-holidays "holidays" (date))

(defun calendar-mouse-view-diary-entries (&optional date diary event)
  "Pop up menu of diary entries for mouse-selected date.
Use optional DATE and alternative file DIARY.  EVENT is the event
that invoked this command.  Shows holidays if `diary-show-holidays-flag'
is non-nil."
  (interactive "i\ni\ne")
  (let* ((date (or date (calendar-cursor-to-date nil event)))
         (diary-file (or diary diary-file))
         (diary-list-include-blanks nil)
         (diary-entries (mapcar (lambda (x) (split-string (cadr x) "\n"))
                                (diary-list-entries date 1 'list-only)))
         (holidays (if diary-show-holidays-flag
                       (calendar-check-holidays date)))
         (title (format "Diary entries%s for %s"
                        (if diary (format " from %s" diary) "")
                        (calendar-date-string date)))
         (selection (cal-menu-x-popup-menu event title
                      (mapcar (lambda (x) (list (concat "     " x))) holidays)
                      (if holidays
                          (list "--shadow-etched-in" "--shadow-etched-in"))
                      (if diary-entries
                          (mapcar 'list (apply 'append diary-entries))
                        '("None")))))
    (and selection (call-interactively selection))))

(defun calendar-mouse-view-other-diary-entries (&optional event)
  "Pop up menu of diary entries from alternative file on mouse-selected date."
  (interactive "e")
  (calendar-mouse-view-diary-entries
   (calendar-cursor-to-date nil event)
   (read-file-name "Enter diary file name: " default-directory nil t)
   event))

(defun cal-menu-set-date-title (menu)
  "Convert date of last event to title suitable for MENU."
  (easy-menu-filter-return
   menu (calendar-date-string (calendar-cursor-to-date t last-input-event)
                              t nil)))

(easy-menu-define cal-menu-context-mouse-menu nil
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  '("cal-menu-mouse2" :filter cal-menu-set-date-title
    "--"
    ["Holidays" calendar-cursor-holidays]
    ["Mark date" calendar-set-mark]
    ["Sunrise/sunset" calendar-sunrise-sunset]
    ["Other calendars" calendar-print-other-dates]
    ;; FIXME there is a bug (#447) with last-nonmenu-event and submenus.
    ;; These currently don't work if called without calendar window selected.
    ("Prepare LaTeX buffer"
     ["Daily (1 page)" cal-tex-cursor-day]
     ["Weekly (1 page)" cal-tex-cursor-week]
     ["Weekly (2 pages)" cal-tex-cursor-week2]
     ["Weekly (other style; 1 page)" cal-tex-cursor-week-iso]
     ["Weekly (yet another style; 1 page)" cal-tex-cursor-week-monday]
     ["Monthly" cal-tex-cursor-month]
     ["Monthly (landscape)" cal-tex-cursor-month-landscape]
     ["Yearly" cal-tex-cursor-year]
     ["Yearly (landscape)" cal-tex-cursor-year-landscape]
     ("Filofax styles"
      ["Filofax Daily (one-day-per-page)" cal-tex-cursor-filofax-daily]
      ["Filofax Weekly (2-weeks-at-a-glance)" cal-tex-cursor-filofax-2week]
      ["Filofax Weekly (week-at-a-glance)" cal-tex-cursor-filofax-week]
      ["Filofax Yearly" cal-tex-cursor-filofax-year]))
    ("Write HTML calendar"
     ["For selected month" cal-html-cursor-month]
     ["For selected year" cal-html-cursor-year])
    ["Diary entries" calendar-mouse-view-diary-entries :keys "d"]
    ["Insert diary entry" diary-insert-entry]
    ["Other diary file entries" calendar-mouse-view-other-diary-entries
     :keys "D"]))

(easy-menu-define cal-menu-global-mouse-menu nil
  "Menu bound to a mouse event, not specific to the mouse-click location."
  '("Calendar"
    ["Scroll forward" calendar-scroll-left-three-months]
    ["Scroll backward" calendar-scroll-right-three-months]
    ["Mark diary entries" diary-mark-entries]
    ["List holidays" calendar-list-holidays]
    ["Mark holidays" calendar-mark-holidays]
    ["Unmark" calendar-unmark]
    ["Lunar phases" calendar-lunar-phases]
    ["Sunrise times for month" calendar-sunrise-sunset-month]
    ["Show diary" diary-show-all-entries]
    ["Exit calendar" calendar-exit]))

;; Undocumented and probably useless.
(defvar cal-menu-load-hook nil
  "Hook run on loading of the `cal-menu' package.")
(make-obsolete-variable 'cal-menu-load-hook
                        "it will be removed in future." "23.1")

(run-hooks 'cal-menu-load-hook)

(provide 'cal-menu)

;; arch-tag: aa81cf73-ce89-48a4-97ec-9ef861e87fe9
;;; cal-menu.el ends here
