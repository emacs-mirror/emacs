;;; cal-x.el --- calendar windows in dedicated frames in X

;; Copyright (C) 1994, 1995, 2005  Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;;      Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, dedicated frames, X Window System

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

;; This collection of functions implements dedicated frames in X for
;; calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'calendar)

(defvar calendar-frame nil "Frame in which to display the calendar.")

(defvar diary-frame nil "Frame in which to display the diary.")

;; This should not specify the font.  That's up to the user.
;; Certainly it should not specify auto-lower and auto-raise
;; since most users won't like that.
(defvar diary-frame-parameters
  '((name . "Diary") (title . "Diary") (height . 10) (width . 80)
    (unsplittable . t) (minibuffer . nil))
  "Parameters of the diary frame, if the diary is in its own frame.
Location and color should be set in .Xdefaults.")

(defvar calendar-frame-parameters
  '((name . "Calendar") (title . "Calendar") (minibuffer . nil)
    (height . 10) (width . 80) (unsplittable . t) (vertical-scroll-bars . nil))
  "Parameters of the calendar frame, if the calendar is in a separate frame.
Location and color should be set in .Xdefaults.")

(defvar calendar-and-diary-frame-parameters
  '((name . "Calendar") (title . "Calendar") (height . 28) (width . 80)
    (minibuffer . nil))
  "Parameters of the frame that displays both the calendar and the diary.
Location and color should be set in .Xdefaults.")

(defvar calendar-after-frame-setup-hooks nil
  "Hooks to be run just after setting up a calendar frame.
Can be used to change frame parameters, such as font, color, location, etc.")

(defun calendar-one-frame-setup (&optional arg)
  "Start calendar and display it in a dedicated frame together with the diary.
This function requires a display capable of multiple frames, else
`calendar-basic-setup' is used instead."
  (if (not (display-multi-frame-p))
      (calendar-basic-setup arg)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    (if (frame-live-p diary-frame) (delete-frame diary-frame))
    (let ((special-display-buffer-names nil)
          (view-diary-entries-initially t))
      (save-window-excursion
        (save-excursion
          (setq calendar-frame
		(make-frame calendar-and-diary-frame-parameters))
          (run-hooks 'calendar-after-frame-setup-hooks)
          (select-frame calendar-frame)
          (if (eq 'icon (cdr (assoc 'visibility
                                     (frame-parameters calendar-frame))))
              (iconify-or-deiconify-frame))
          (calendar-basic-setup arg)
          (set-window-dedicated-p (selected-window) t)
          (set-window-dedicated-p
           (display-buffer
            (if (not (memq 'fancy-diary-display diary-display-hook))
                (get-file-buffer diary-file)
              (if (not (bufferp (get-buffer fancy-diary-buffer)))
                  (make-fancy-diary-buffer))
              fancy-diary-buffer))
           t))))))

(defun calendar-only-one-frame-setup (&optional arg)
  "Start calendar and display it in a dedicated frame.
This function requires a display capable of multiple frames, else
`calendar-basic-setup' is used instead."
  (if (not (display-multi-frame-p))
      (calendar-basic-setup arg)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    (let ((special-display-buffer-names nil)
          (view-diary-entries-initially nil))
      (save-window-excursion
        (save-excursion
          (setq calendar-frame
		(make-frame calendar-frame-parameters))
          (run-hooks 'calendar-after-frame-setup-hooks)
          (select-frame calendar-frame)
          (if (eq 'icon (cdr (assoc 'visibility
                                     (frame-parameters calendar-frame))))
              (iconify-or-deiconify-frame))
          (calendar-basic-setup arg)
          (set-window-dedicated-p (selected-window) t))))))

(defun calendar-two-frame-setup (&optional arg)
  "Start calendar and diary in separate, dedicated frames.
This function requires a display capable of multiple frames, else
`calendar-basic-setup' is used instead."
  (if (not (display-multi-frame-p))
      (calendar-basic-setup arg)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    (if (frame-live-p diary-frame) (delete-frame diary-frame))
    (let ((pop-up-windows nil)
          (view-diary-entries-initially nil)
          (special-display-buffer-names nil))
      (save-window-excursion
        (save-excursion (calendar-basic-setup arg))
        (setq calendar-frame (make-frame calendar-frame-parameters))
        (run-hooks 'calendar-after-frame-setup-hooks)
        (select-frame calendar-frame)
        (if (eq 'icon (cdr (assoc 'visibility
                                  (frame-parameters calendar-frame))))
            (iconify-or-deiconify-frame))
        (display-buffer calendar-buffer)
        (set-window-dedicated-p (selected-window) t)
        (setq diary-frame (make-frame diary-frame-parameters))
        (run-hooks 'calendar-after-frame-setup-hooks)
        (select-frame diary-frame)
        (if (eq 'icon (cdr (assoc 'visibility
                                  (frame-parameters diary-frame))))
            (iconify-or-deiconify-frame))
        (save-excursion (diary))
        (set-window-dedicated-p
         (display-buffer
          (if (not (memq 'fancy-diary-display diary-display-hook))
              (get-file-buffer diary-file)
            (if (not (bufferp (get-buffer fancy-diary-buffer)))
                (make-fancy-diary-buffer))
            fancy-diary-buffer))
         t)))))

;; Formerly (get-file-buffer diary-file) was added to the list here,
;; but that isn't clean, and the value could even be nil.
(setq special-display-buffer-names
      (append special-display-buffer-names
              (list "*Yahrzeits*" lunar-phases-buffer holiday-buffer
                    fancy-diary-buffer
                    other-calendars-buffer calendar-buffer)))

(run-hooks 'cal-x-load-hook)

(provide 'cal-x)

;;; arch-tag: c6dbddca-ae84-442d-87fc-244b76e38e17
;;; cal-x.el ends here
