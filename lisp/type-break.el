;;; type-break.el --- take breaks from typing at appropriate intervals

;;; Copyright (C) 1994 Roland McGrath
;;; Copyright (C) 1994 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;;         Roland McGrath <roland@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions, timers
;;; Status: works in GNU Emacs 19
;;; Created: 1994-07-13

;;; LCD Archive Entry:
;;; type-break|Noah Friedman|friedman@prep.ai.mit.edu|
;;; take breaks from typing at appropriate intervals|
;;; $Date$|$Revision$||

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
;;; Code:


(require 'timer)

;;;###autoload
(defvar type-break-mode t
  "*Non-`nil' means typing break mode is enabled.
See the docstring for the `type-break-mode' command for more information.")

;;;###autoload
(defvar type-break-interval (* 60 60)
  "*Number of seconds between scheduled typing breaks.")

;;;###autoload
(defvar type-break-query-interval 60
  "*Number of seconds between queries to take a break, if put off.
The user will continue to be prompted at this interval until he or she
finally submits to taking a typing break.")

;;;###autoload
(defvar type-break-keystroke-threshold
  ;; Assuming average typing speed is 45wpm and the average word length is
  ;; about 5 letters, default upper threshold to the average number of
  ;; keystrokes one is likely to type in a break interval.  That way if the
  ;; user goes through a furious burst of typing activity, cause a typing
  ;; break to be required sooner than originally scheduled.
  ;; Conversely, the minimum threshold should be about a quarter of this.
  (let* ((wpm 45)
         (avg-word-length 5)
         (upper (* wpm avg-word-length (/ type-break-interval 60)))
         (lower (/ upper 4)))
    (cons lower upper))
  "*Upper and lower bound on number of keystrokes for considering typing break.

This structure is a pair of numbers.  The first number is the minimum
number of keystrokes that must have been entered since the last typing
break before considering another one, even if the scheduled time has
elapsed; the break is simply rescheduled until later if the minimum
threshold hasn't been reached.

The second number is the maximum number of keystrokes that can be entered
before a typing break is requested immediately, pre-empting the originally
scheduled break.

Keys with bucky bits (shift, control, meta, etc) are counted as only one
keystroke even though they really require multiple keys to generate them.")
  
;;;###autoload
(defvar type-break-query-function 'yes-or-no-p
  "*Function to use for making query for a typing break.
It should take a string as an argument, the prompt.
Usually this should be set to `yes-or-no-p' or `y-or-n-p'.")

(defvar type-break-demo-function-vector
  [type-break-demo-life type-break-demo-hanoi]
  "*Vector consisting of functions to run as demos during typing breaks.
When a typing break begins, one of these functions is selected randomly
to have emacs do something interesting.

Any function in this vector should start a demo which ceases as soon as a
key is pressed.")

;; These are internal variables.  Do not set them yourself.

;; Number of commands (roughly # of keystrokes) recorded since last break.
(defvar type-break-keystroke-count 0)

;; Non-nil when a scheduled typing break is due.
(defvar type-break-alarm-p nil)


;;;###autoload
(defun type-break-mode (&optional prefix)
  "Enable or disable typing-break mode.
This is a minor mode, but it is global to all buffers by default.

When this mode is enabled, the user is encouraged to take typing breaks at
appropriate intervals; either after a specified amount of time or when the
user has exceeded a keystroke threshold.  When the time arrives, the user
is asked to take a break.  If the user refuses at that time, emacs will ask
again in a short period of time.  The idea is to give the user enough time
to find a good breaking point in his or her work, but be sufficiently
annoying to discourage putting typing breaks off indefinitely.

Calling this command with no prefix argument toggles this mode.
A negative prefix argument disables this mode.
A non-negative prefix argument or any other non-`nil' argument enables it.

The user may enable or disable this mode by setting the variable of the
same name, though setting it in that way doesn't reschedule a break or
reset the keystroke counter.

When this function enables the mode, it schedules a break with
`type-break-schedule' to make sure one occurs (the user can call that
command to reschedule the break at any time).  It also initializes the
keystroke counter.

The variable `type-break-interval' specifies the number of seconds to
schedule between regular typing breaks.  This variable doesn't directly
affect the time schedule; it simply provides a default for the
`type-break-schedule' command.

The variable `type-break-query-interval' specifies the number of seconds to
schedule between repeated queries for breaks when the user answers \"no\"
to the previous query.

The variable `type-break-keystroke-theshold' is used to determine the
thresholds at which typing breaks should be considered.

The variable `type-break-query-function' should contain a function (or the
symbolic name of a function) to be used to query the user for typing
breaks."
  (interactive "P")
  ;; make sure it's there.
  (add-hook 'post-command-hook 'type-break-check 'append)

  (cond
   ((null prefix)
    (setq type-break-mode (not type-break-mode)))
   ((numberp (prefix-numeric-value prefix))
    (setq type-break-mode (>= (prefix-numeric-value prefix) 0)))
   (prefix
    (setq type-break-mode t))
   (t
    (setq type-break-mode nil)))

  (cond
   (type-break-mode
    (setq type-break-keystroke-count 0)
    (type-break-schedule)
    (and (interactive-p)
         (message "type-break-mode is enabled and reset")))
   ((interactive-p)
    (message "type-break-mode is disabled")))

  type-break-mode)

;;;###autoload
(defun type-break ()
  "Take a typing break.

During the break, a demo selected from the functions listed in
`type-break-demo-function-vector' is run.

After the typing break is finished, the next break is scheduled
as per the function `type-break-schedule', and the keystroke counter is
reset."
  (interactive)
  (save-window-excursion
    ;; Eat the screen.
    (and (eq (selected-window) (minibuffer-window))
         (other-window 1))
    (delete-other-windows)
    (scroll-right (window-width))
    (message "Press any key to finish typing break.")

    (random t)
    (let* ((len (length type-break-demo-function-vector))
           (idx (random len))
           (fn (aref type-break-demo-function-vector idx)))
      (condition-case ()
          (funcall fn)
        (error nil)))

    (setq type-break-keystroke-count 0)
    (type-break-schedule)))


;;;###autoload
(defun type-break-schedule (&optional time)
  "Schedule a typing break for TIME seconds from now.
If time is not specified, default to `type-break-interval'."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (or time (setq time type-break-interval))
  ;; Remove any old scheduled break
  (type-break-cancel-schedule)
  (run-at-time time nil 'type-break-alarm))

(defun type-break-cancel-schedule ()
  "Cancel scheduled typing breaks.
This does not prevent queries for typing breaks when the keystroke
threshold has been reached; to turn off typing breaks altogether, turn off
type-break-mode."
  (interactive)
  (let ((timer-dont-exit t))
    (cancel-function-timers 'type-break-alarm))
  (setq type-break-alarm-p nil))

(defun type-break-alarm ()
  "This function is run when a scheduled typing break is due."
  (setq type-break-alarm-p t))

(defun type-break-check ()
  "Ask to take a typing break if appropriate.
This may be the case either because the scheduled time has come \(and the
minimum keystroke threshold has been reached\) or because the maximum
keystroke threshold has been exceeded."
  (cond
   (type-break-mode
    (let* ((pair (and (consp type-break-keystroke-threshold)
                      type-break-keystroke-threshold))
           (min-threshold (car pair))
           (max-threshold (cdr pair)))
      (and pair
           (stringp (this-command-keys))
           (setq type-break-keystroke-count
                 (+ type-break-keystroke-count (length (this-command-keys)))))
      (cond
       ((input-pending-p))
       ((and (numberp max-threshold)
             (> type-break-keystroke-count max-threshold))
        (setq type-break-keystroke-count 0)
        (type-break-query))
       (type-break-alarm-p
        (cond
         ((and (numberp min-threshold)
               (< type-break-keystroke-count min-threshold)))
         (t
          (setq type-break-keystroke-count 0)
          (type-break-query)))))))))

(defun type-break-query ()
  (condition-case ()
      (cond
       ((funcall type-break-query-function "Take a break from typing now? ")
        (type-break))
       (t
        (type-break-schedule type-break-query-interval)))
    (quit
     (type-break-schedule type-break-query-interval))))


;; This is a wrapper around hanoi that calls it with an arg large enough to
;; make the largest discs possible that will fit in the window.
;; Also, clean up the *Hanoi* buffer after we're done.
(defun type-break-demo-hanoi ()
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
(defun type-break-demo-life ()
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

(type-break-mode t)

;;; type-break.el ends here
