;;; type-break.el --- take breaks from typing

;;; Copyright (C) 1994 Roland McGrath
;;; Copyright (C) 1994 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;;         Roland McGrath <roland@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions, timer, RSI, CTS, tendinitis, suffering, pain
;;; Created: 1994-07-13

;;; $Id$

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; Based on Roland McGrath's hanoi-break.el (unreleased).
;;; The idea for keystroke recording was suggested by
;;; Mark Ashton <mpashston@gnu.ai.mit.edu>

;;; Code:


(require 'timer)

;;;###autoload
(defvar type-break-interval (* 60 30)
  "*Number of seconds between typing breaks.")

;;;###autoload
(defvar type-break-delay-interval 60
  "*Number of seconds between queries to take a break, if put off.
The user will continue to be prompted at this interval until he or she
finally submits to taking a typing break.")

;; Assuming average typing speed is 45wpm, default this to the average
;; number of keystrokes one is likely to type in a break interval.
;; That way if the user goes through a furious burst of typing activity,
;; cause a typing break to be required sooner than originally scheduled.
;;;###autoload
(defvar type-break-keystroke-interval (* 45 (/ type-break-interval 60))
  "*Number of keystrokes to record before querying for a typing break.
If not a number, don't keep track of keystrokes.

Actually, this is not the number of keystrokes per se, but the number of
interactive commands (including self-inserting characters typed).
Compound key bindings like C-x C-f count as a single command even though
that consists of multiple keystrokes.")
  
;;;###autoload
(defvar type-break-demo-function-vector
  [type-break-life type-break-hanoi]
  "*Vector consisting of functions to run as demos during typing breaks.
When a typing break begins, one of these functions is selected randomly
to have emacs do something interesting.

Any function in this vector should start a demo which ceases as soon as a
key is pressed.")

;; The original motivation for this variable was that in emacs 19.25 (or
;; perhaps just in unreleased versions of emacs 19.26), the function
;; keyboard.c:safe_run_hooks wasn't reentrant, so that running yes-or-no-p
;; from a post-command-hook caused the inferior command loop to wipe out
;; the original value of the hook.  That was fixed, but it occured to me
;; that some people might prefer y-or-n-p.
;;;###autoload
(defvar type-break-query-function 'yes-or-no-p
  "*Function to use for making query for a typing break.
Usually this will be `yes-or-no-p' or `y-or-n-p'.")

;; The rest are internal variables.  Do not set them yourself.

;; Number of commands (roughly # of keystrokes) recorded since last break.
(defvar type-break-keystroke-count 0)

;; Non-nil if we need a typing break soon.
(defvar type-break-p nil)


;;;###autoload
(defun type-break ()
  "Take a typing break.

If `type-break-delay-interval' seconds have passed since the last typing
break, or `type-break-keystroke-interval' keystrokes have been recorded
since the last typing break, ask the user to take a break now.

The user can refuse by answering \"no\", in which case another query will
be made in `type-break-delay-interval' seconds.

During the typing break, a demo selected from the functions listed in
`type-break-demo-function-vector' is run."
  (interactive)
  (setq type-break-p nil)
  (setq type-break-keystroke-count 0)
  (cancel-type-break)
  (save-window-excursion
    (condition-case ()
        (cond
         ((funcall type-break-query-function "Take a break from typing now? ")
          ;; Eat the screen.
          (and (eq (selected-window) (minibuffer-window))
               (other-window 1))
          (delete-other-windows)
          (scroll-right (window-width))
          (message "Take a break from typing.")
          (type-break-select)
          (type-break-schedule))
         (t
          (type-break-schedule type-break-delay-interval)))
      (quit
       (type-break-schedule type-break-delay-interval)))))

(defun type-break-select ()
  (random t)
  (let* ((len (length type-break-demo-function-vector))
         (idx (random len))
         (fn (aref type-break-demo-function-vector idx)))
  (condition-case ()
      (funcall fn)
    (error nil))))


;;;###autoload
(defun type-break-schedule (&optional time)
  "Schedule a typing break TIME seconds from now.
If time is not specified, default to type-break-interval."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (or time (setq time type-break-interval))
  ;; Remove any old scheduled break
  (cancel-type-break)
  (run-at-time time nil 'type-break-soon))

(defun cancel-type-break ()
  "Cancel scheduled typing breaks."
  (interactive)
  (let ((timer-dont-exit t))
    (cancel-function-timers 'type-break-soon)))

(defun type-break-soon ()
  "Take a typing break very soon."
  (setq type-break-p t))

(defun type-break-check ()
  "Take a typing break if the time has come."
  (setq type-break-keystroke-count (1+ type-break-keystroke-count))
  (cond
   ((input-pending-p))
   ((or type-break-p
        (and (natnump type-break-keystroke-interval)
             (> type-break-keystroke-count type-break-keystroke-interval)))
    (type-break))))


;; This is a wrapper around hanoi that calls it with an arg large enough to
;; make the largest discs possible that will fit in the window.
;; Also, clean up the *Hanoi* buffer after we're done.
(defun type-break-hanoi ()
  "Take a hanoiing typing break."
  (and (get-buffer "*Hanoi*")
       (kill-buffer "*Hanoi*"))
  (condition-case ()
      (progn
        (hanoi (/ (window-width) 8))
        ;; Wait for user to come back.
        (read-char)
        (kill-buffer "*Hanoi*"))
    (quit 
     (and (get-buffer "*Hanoi*")
          (kill-buffer "*Hanoi*")))))

;; This is a wrapper around life that calls it with a `sleep' arg to make
;; it run a little more leisurely.
;; Also, clean up the *Life* buffer after we're done.
(defun type-break-life ()
  "Take a typing break and get a life."
  (and (get-buffer "*Life*")
       (kill-buffer "*Life*"))
  (condition-case ()
      (progn
        (life 3)
        ;; Wait for user to come back.
        (read-char)
        (kill-buffer "*Life*"))
    (quit 
     (and (get-buffer "*Life*")
          (kill-buffer "*Life*")))))


(provide 'type-break)

(add-hook 'post-command-hook 'type-break-check 'append)

;;; type-break.el ends here
