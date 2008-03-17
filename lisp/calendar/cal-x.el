;;; cal-x.el --- calendar windows in dedicated frames in X

;; Copyright (C) 1994, 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008  Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;;         Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, dedicated frames, X Window System

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

;; This collection of functions implements dedicated frames in X for
;; calendar.el.

;;; Code:

(require 'calendar)

(defcustom diary-frame-parameters
  '((name . "Diary") (title . "Diary") (height . 10) (width . 80)
    (unsplittable . t) (minibuffer . nil))
  "Parameters of the diary frame, if the diary is in its own frame.
Location and color should be set in .Xdefaults." ; why?
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (unsplittable boolean) (minibuffer boolean)
             (vertical-scroll-bars boolean))
  :group 'calendar)

(defcustom calendar-frame-parameters
  '((name . "Calendar") (title . "Calendar") (height . 10) (width . 80)
    (unsplittable . t) (minibuffer . nil) (vertical-scroll-bars . nil))
  "Parameters of the calendar frame, if the calendar is in a separate frame.
Location and color should be set in .Xdefaults."
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (unsplittable boolean) (minibuffer boolean)
             (vertical-scroll-bars boolean))
  :group 'calendar)

(defcustom calendar-and-diary-frame-parameters
  '((name . "Calendar") (title . "Calendar") (height . 28) (width . 80)
    (minibuffer . nil))
  "Parameters of the frame that displays both the calendar and the diary.
Location and color should be set in .Xdefaults."
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (unsplittable boolean) (minibuffer boolean)
             (vertical-scroll-bars boolean))
  :group 'calendar)

(defcustom calendar-after-frame-setup-hooks nil
  "Hooks to be run just after setting up a calendar frame.
Can be used to change frame parameters, such as font, color, location, etc."
  :type 'hook
  :group 'calendar-hooks)

;;; End of user options.

(defvar calendar-frame nil
  "Frame in which the calendar was last displayed.")

(defvar diary-frame nil
  "Frame in which the diary was last displayed.")

(defun calendar-frame-1 (frame)
  "Subroutine used by `calendar-one-frame-setup' etc."
  (run-hooks 'calendar-after-frame-setup-hooks)
  (select-frame frame)
  (if (eq 'icon (cdr (assoc 'visibility (frame-parameters frame))))
      (iconify-or-deiconify-frame)))

;; calendar-basic-setup is called first, and will autoload diary-lib.
(declare-function make-fancy-diary-buffer "diary-lib" nil)

(defun calendar-dedicate-diary ()
  "Dedicate the window associated with the diary buffer."
  (set-window-dedicated-p
   (display-buffer
    (if (not (memq 'fancy-diary-display diary-display-hook))
        (get-file-buffer diary-file)
      (or (buffer-live-p fancy-diary-buffer)
          (make-fancy-diary-buffer))
      fancy-diary-buffer))
   t))

;;; FIXME ../../src/emacs -Q  --eval "(setq calendar-setup 'calendar-only)"  -f calendar
;;;###cal-autoload
(defun calendar-one-frame-setup (&optional arg only)
  "Start calendar and display it in a dedicated frame.
Also show the diary in that frame, unless ONLY is non-nil.  The optional
argument ARG is passed to `calendar-basic-setup'.  If the display
is not capable of multiple frames, `calendar-basic-setup' is all
that is used."
  (if (not (display-multi-frame-p))
      (calendar-basic-setup arg)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    (unless only
      (if (frame-live-p diary-frame) (delete-frame diary-frame)))
    (let ((special-display-buffer-names nil)
          (view-diary-entries-initially (not only)))
      (save-window-excursion
        (save-excursion
          (calendar-frame-1
           (setq calendar-frame
                 (make-frame (if only
                                 calendar-frame-parameters
                               calendar-and-diary-frame-parameters))))
          (calendar-basic-setup arg)    ; FIXME move?
          ;; FIXME display-buffer?
          (set-window-dedicated-p (selected-window) t)
          (unless only (calendar-dedicate-diary)))))))

;;;###cal-autoload
(defun calendar-only-one-frame-setup (&optional arg)
  "Start calendar and display it in a dedicated frame.
The optional argument ARG is passed to `calendar-basic-setup'.
If the display is not capable of multiple frames, `calendar-basic-setup'
is all that is used."
  (calendar-one-frame-setup arg t))

;;;###cal-autoload
(defun calendar-two-frame-setup (&optional arg)
  "Start calendar and diary in separate, dedicated frames.
The optional argument ARG is passed to `calendar-basic-setup'.
If the display is not capable of multiple frames, `calendar-basic-setup'
is all that is used."
  (if (not (display-multi-frame-p))
      (calendar-basic-setup arg)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    (if (frame-live-p diary-frame) (delete-frame diary-frame))
    (let ((pop-up-windows nil)
          (view-diary-entries-initially nil)
          (special-display-buffer-names nil))
      (save-window-excursion
        ;; FIXME why does this do things in a slightly different order
        ;; to calendar-one-frame-setup?
        (save-excursion (calendar-basic-setup arg))
        (calendar-frame-1
         (setq calendar-frame (make-frame calendar-frame-parameters)))
        (display-buffer calendar-buffer)
        (set-window-dedicated-p (selected-window) t)
        (calendar-frame-1
         (setq diary-frame (make-frame diary-frame-parameters)))
        (save-excursion (diary))
        (calendar-dedicate-diary)))))

;; Formerly (get-file-buffer diary-file) was added to the list here,
;; but that isn't clean, and the value could even be nil.
;; FIXME is this really our business?
(setq special-display-buffer-names
      (append special-display-buffer-names
              (list cal-hebrew-yahrzeit-buffer
                    lunar-phases-buffer holiday-buffer fancy-diary-buffer
                    other-calendars-buffer calendar-buffer)))

(run-hooks 'cal-x-load-hook)

(provide 'cal-x)

;; arch-tag: c6dbddca-ae84-442d-87fc-244b76e38e17
;;; cal-x.el ends here
