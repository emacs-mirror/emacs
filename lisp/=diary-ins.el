;;; diary-ins.el --- calendar functions for adding diary entries.

;; Copyright (C) 1990 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: diary, calendar

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

;; This collection of functions implements the diary insertion features as
;; described in calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'diary)

(defun make-diary-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to diary-file."
  (find-file-other-window
   (substitute-in-file-name (if file file diary-file)))
  (goto-char (point-max))
  (insert
   (if (bolp) "" "\n")
   (if nonmarking diary-nonmarking-symbol "")
   string " "))

(defun insert-diary-entry (arg)
  "Insert a diary entry for the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (make-diary-entry
   (calendar-date-string
    (or (calendar-cursor-to-date)
        (error "Cursor is not on a date!"))
    t t)
   arg))

(defun insert-weekly-diary-entry (arg)
  "Insert a weekly diary entry for the day of the week indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (make-diary-entry
   (calendar-day-name
    (or (calendar-cursor-to-date)
        (error "Cursor is not on a date!")))
   arg))

(defun insert-monthly-diary-entry (arg)
  "Insert a monthly diary entry for the day of the month indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " * ")
            '("* " day))))
    (make-diary-entry
     (calendar-date-string
      (or (calendar-cursor-to-date)
          (error "Cursor is not on a date!"))
      t)
     arg)))

(defun insert-yearly-diary-entry (arg)
  "Insert an annual diary entry for the day of the year indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day))))
    (make-diary-entry
     (calendar-date-string
      (or (calendar-cursor-to-date)
          (error "Cursor is not on a date!"))
      t)
     arg)))

(defun insert-anniversary-diary-entry (arg)
  "Insert an anniversary diary entry for the date given by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " month " " year)
            '(month " " day " " year))))
    (make-diary-entry
     (format "%s(diary-anniversary %s)"
             sexp-diary-entry-symbol
             (calendar-date-string
              (or (calendar-cursor-to-date)
                  (error "Cursor is not on a date!"))
              nil t))
     arg)))

(defun insert-block-diary-entry (arg)
  "Insert a block diary entry for the days between the point and marked date.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " month " " year)
            '(month " " day " " year)))
         (cursor (or (calendar-cursor-to-date)
                     (error "Cursor is not on a date!")))
         (mark (or (car calendar-mark-ring)
                   (error "No mark set in this buffer")))
         (start)
         (end))
    (if (< (calendar-absolute-from-gregorian mark)
           (calendar-absolute-from-gregorian cursor))
        (setq start mark
              end cursor)
      (setq start cursor
              end mark))
    (make-diary-entry
     (format "%s(diary-block %s %s)"
      sexp-diary-entry-symbol
      (calendar-date-string start nil t)
      (calendar-date-string end nil t))
     arg)))

(defun insert-cyclic-diary-entry (arg)
  "Insert a cyclic diary entry starting at the date given by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " month " " year)
            '(month " " day " " year))))
    (make-diary-entry
     (format "%s(diary-cyclic %d %s)"
             sexp-diary-entry-symbol
             (calendar-read "Repeat every how many days: "
                            '(lambda (x) (> x 0)))
             (calendar-date-string
              (or (calendar-cursor-to-date)
                  (error "Cursor is not on a date!"))
              nil t))
     arg)))

(defun insert-hebrew-diary-entry (arg)
  "Insert a diary entry.
For the Hebrew date corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-month-name-array
          calendar-hebrew-month-name-array-leap-year))
    (make-diary-entry
     (concat
      hebrew-diary-entry-symbol
      (calendar-date-string 
       (calendar-hebrew-from-absolute
        (calendar-absolute-from-gregorian
         (or (calendar-cursor-to-date)
             (error "Cursor is not on a date!"))))
       nil t))
     arg)))

(defun insert-monthly-hebrew-diary-entry (arg)
  "Insert a monthly diary entry.
For the day of the Hebrew month corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style '(day " * ") '("* " day )))
         (calendar-month-name-array
          calendar-hebrew-month-name-array-leap-year))
    (make-diary-entry
     (concat
      hebrew-diary-entry-symbol
      (calendar-date-string 
       (calendar-hebrew-from-absolute
        (calendar-absolute-from-gregorian
         (or (calendar-cursor-to-date)
             (error "Cursor is not on a date!"))))))
     arg)))

(defun insert-yearly-hebrew-diary-entry (arg)
  "Insert an annual diary entry.
For the day of the Hebrew year corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day)))
         (calendar-month-name-array
          calendar-hebrew-month-name-array-leap-year))
    (make-diary-entry
     (concat
      hebrew-diary-entry-symbol
      (calendar-date-string 
       (calendar-hebrew-from-absolute
        (calendar-absolute-from-gregorian
         (or (calendar-cursor-to-date)
             (error "Cursor is not on a date!"))))))
     arg)))

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
         (or (calendar-cursor-to-date)
             (error "Cursor is not on a date!"))))
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
         (or (calendar-cursor-to-date)
             (error "Cursor is not on a date!"))))))
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
         (or (calendar-cursor-to-date)
             (error "Cursor is not on a date!"))))))
     arg)))

(provide 'diary-ins)

;;; diary-ins.el ends here
