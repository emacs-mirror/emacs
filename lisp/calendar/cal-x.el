;;; cal-x.el --- calendar windows in dedicated frames in x-windows

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;;      Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: calendar, dedicated frames, x-windows

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

;; This collection of functions implements dedicated frames in x-windows for
;; calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'calendar)
(if (not (fboundp 'calendar-basic-setup))
    (fset 'calendar-basic-setup (symbol-function 'calendar)))
 
(defvar calendar-setup 'one-frame
  "The frame set up of the calendar.
The choices are `one-frame' (calendar and diary together in one separate,
dediciated frame) or `two-frames' (calendar and diary in separate, dedicated
frames); with any other value the current frame is used.")

(defun calendar (&optional arg)
  "Choose between the one frame, two frame, or basic calendar displays.
The original function `calendar' has been renamed `calendar-basic-setup'."
  (interactive "P")
  (cond ((equal calendar-setup 'one-frame) (calendar-one-frame-setup arg))
        ((equal calendar-setup 'two-frames) (calendar-two-frame-setup arg))
        (t (calendar-basic-setup arg))))

(defvar calendar-frame nil "Frame in which to display the calendar.")

(defvar diary-frame nil "Frame in which to display the diary.")
  
(defvar diary-frame-parameters
  '((name . "Diary") (height . 10) (width . 80) (unsplittable . t)
    (font . "6x13") (auto-lower . t) (auto-raise . t) (minibuffer . nil))
  "Parameters of the diary frame, if the diary is in its own frame.
Location and color should be set in .Xdefaults.")
                                 
(defvar calendar-frame-parameters
  '((name . "Calendar") (minibuffer . nil) (height . 10) (width . 80)
    (auto-raise . t) (auto-lower . t) (font . "6x13") (unsplittable . t)
    (vertical-scroll-bars . nil))
  "Parameters of the calendar frame, if the calendar is in a separate frame.
Location and color should be set in .Xdefaults.")

(defvar calendar-and-diary-frame-parameters
  '((name . "Calendar") (height . 28) (width . 80) (minibuffer . nil)
    (font . "6x13") (auto-raise . t) (auto-lower . t))
  "Parameters of the frame that displays both the calendar and the diary.
Location and color should be set in .Xdefaults.")
  
(defvar calendar-after-frame-setup-hooks nil
  "Hooks to be run just after setting up a calendar frame.
Can be used to change frame parameters, such as font, color, location, etc.")
  
(defun calendar-one-frame-setup (&optional arg)
  "Start calendar and display it in a dedicated frame together with the diary."
  (if (not window-system)
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
          (set-window-dedicated-p (selected-window) 'calendar)
          (set-window-dedicated-p
           (display-buffer
            (if (memq 'fancy-diary-display diary-display-hook)
                fancy-diary-buffer
              (get-file-buffer diary-file)))
           'diary))))))
    
(defun calendar-two-frame-setup (&optional arg)
  "Start calendar and diary in separate, dedicated frames."
  (if (not window-system)
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
        (set-window-dedicated-p (selected-window) 'calendar)
        (setq diary-frame (make-frame diary-frame-parameters))
        (run-hooks 'calendar-after-frame-setup-hooks)
        (select-frame diary-frame)
        (if (eq 'icon (cdr (assoc 'visibility
                                  (frame-parameters diary-frame))))
            (iconify-or-deiconify-frame))
        (save-excursion (diary))
        (set-window-dedicated-p
         (display-buffer
          (if (memq 'fancy-diary-display diary-display-hook)
              fancy-diary-buffer
            (get-file-buffer diary-file)))
         'diary)))))

(setq special-display-buffer-names
      (append special-display-buffer-names
              (list "*Yahrzeits*" lunar-phases-buffer holiday-buffer
                    fancy-diary-buffer (get-file-buffer diary-file)
                    calendar-buffer)))

(run-hooks 'cal-x-load-hook)

(provide 'cal-x)

;;; cal-x.el ends here
