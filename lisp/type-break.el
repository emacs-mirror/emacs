;;; type-break.el --- encourage rests from typing at appropriate intervals

;;; Copyright (C) 1994 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions, timers
;;; Status: known to work in GNU Emacs 19.25 or later.
;;; Created: 1994-07-13

;;; LCD Archive Entry:
;;; type-break|Noah Friedman|friedman@prep.ai.mit.edu|
;;; encourage rests from typing at appropriate intervals|
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

;;; The docstring for the function `type-break-mode' summarizes most of the
;;; details of the interface.

;;; This package relies on the assumption that you live entirely in emacs,
;;; as the author does.  If that's not the case for you (e.g. you often
;;; suspend emacs or work in other windows) then this won't help very much;
;;; it will depend on just how often you switch back to emacs.  At the very
;;; least, you will want to turn off the keystroke thresholds and rest
;;; interval tracking.

;;; Setting type-break-good-rest-interval makes emacs cons like a maniac
;;; because of repeated calls to `current-time'.  There's not really any
;;; good way to avoid this without disabling the variable.

;;; This package was inspired by Roland McGrath's hanoi-break.el.

;;; Code:


(require 'timer)

;; Make this nil initially so that the call to type-break-mode at the end
;; will cause scheduling and so forth to happen.
;;;###autoload
(defvar type-break-mode nil
  "*Non-`nil' means typing break mode is enabled.
See the docstring for the `type-break-mode' command for more information.")

;;;###autoload
(defvar type-break-interval (* 60 60)
  "*Number of seconds between scheduled typing breaks.")

;;;###autoload
(defvar type-break-good-rest-interval (/ type-break-interval 6)
  "*Number of seconds of idle time considered to be an adequate typing rest.

When this variable is non-`nil', emacs checks the idle time between
keystrokes.  If this idle time is long enough to be considered a "good"
rest from typing, then the next typing break is simply rescheduled for later.

The user will also be admonished if a forced break isn't at least as long
as this time, to remind them to rest longer next time.")

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
This structure is a pair of numbers.

The first number is the minimum number of keystrokes that must have been
entered since the last typing break before considering another one, even if
the scheduled time has elapsed; the break is simply rescheduled until later
if the minimum threshold hasn't been reached.  If this first value is nil,
then there is no minimum threshold; as soon as the scheduled time has
elapsed, the user will always be queried.

The second number is the maximum number of keystrokes that can be entered
before a typing break is requested immediately, pre-empting the originally
scheduled break.  If this second value is nil, then no pre-emptive breaks
will occur; only scheduled ones will.

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

;; Non-nil when a scheduled typing break is due.
(defvar type-break-alarm-p nil)

(defvar type-break-keystroke-count 0)

(defvar type-break-time-last-break nil)
(defvar type-break-time-next-break nil)
(defvar type-break-time-last-command (current-time))


;; Compute the difference, in seconds, between a and b, two structures
;; similar to those returned by `current-time'.
;; Use addition rather than logand since I found it convenient to add
;; seconds to the cdr of some of my stored time values, which may throw off
;; the number of bits in the cdr.
(defsubst type-break-time-difference (a b)
  (+ (lsh (- (car b) (car a)) 16)
     (- (car (cdr b)) (car (cdr a)))))

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

If the mode was previously disabled and is enabled as a consequence of
calling this function, it schedules a break with `type-break-schedule' to
make sure one occurs (the user can call that command to reschedule the
break at any time).  It also initializes the keystroke counter.

The variable `type-break-interval' specifies the number of seconds to
schedule between regular typing breaks.  This variable doesn't directly
affect the time schedule; it simply provides a default for the
`type-break-schedule' command.

The variable `type-break-query-interval' specifies the number of seconds to
schedule between repeated queries for breaks when the user answers \"no\"
to the previous query.

The variable `type-break-good-rest-interval' specifies the minimum amount
of time which is considered a reasonable typing break.  Whenever that time
has elapsed, typing breaks are automatically rescheduled for later even if
emacs didn't prompt you to take one first.  You can disable this behavior.

The variable `type-break-keystroke-threshold' is used to determine the
thresholds at which typing breaks should be considered.  You can use
the command `type-break-guestimate-keystroke-threshold' to try to
approximate good values for this.

The variable `type-break-query-function' should contain a function (or the
symbolic name of a function) to be used to query the user for typing
breaks.

Finally, the command `type-break-statistics' prints interesting things."
  (interactive "P")
  ;; make sure it's there.
  (add-hook 'post-command-hook 'type-break-check 'append)

  (let ((already-enabled type-break-mode))
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
     ((and already-enabled type-break-mode)
      (and (interactive-p)
           (message "type-break-mode was already enabled")))
     (type-break-mode
      (setq type-break-keystroke-count 0)
      (type-break-schedule)
      (and (interactive-p)
           (message "type-break-mode is enabled and reset")))
     ((interactive-p)
      (message "type-break-mode is disabled"))))
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
  (setq type-break-time-last-break (current-time))
  (save-window-excursion
    ;; Eat the screen.
    (and (eq (selected-window) (minibuffer-window))
         (other-window 1))
    (delete-other-windows)
    (scroll-right (window-width))
    (message "Press any key to resume from typing break.")

    (random t)
    (let* ((len (length type-break-demo-function-vector))
           (idx (random len))
           (fn (aref type-break-demo-function-vector idx)))
      (condition-case ()
          (funcall fn)
        (error nil))))

  (and type-break-good-rest-interval
       (< (type-break-time-difference type-break-time-last-command
                                      (current-time))
          type-break-good-rest-interval)
       (message "That typing break wasn't really long enough.  Rest more next time."))

  (setq type-break-keystroke-count 0)
  (type-break-schedule))


;;;###autoload
(defun type-break-schedule (&optional time)
  "Schedule a typing break for TIME seconds from now.
If time is not specified, default to `type-break-interval'."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (or time (setq time type-break-interval))
  ;; Remove any old scheduled break
  (type-break-cancel-schedule)
  (run-at-time time nil 'type-break-alarm)

  (setq type-break-time-next-break (current-time))
  (setcar (cdr type-break-time-next-break)
          (+ time (car (cdr type-break-time-next-break)))))

(defun type-break-cancel-schedule ()
  "Cancel scheduled typing breaks.
This does not prevent queries for typing breaks when the keystroke
threshold has been reached; to turn off typing breaks altogether, turn off
type-break-mode."
  (interactive)
  (let ((timer-dont-exit t))
    (cancel-function-timers 'type-break-alarm))
  (setq type-break-alarm-p nil)
  (setq type-break-time-next-break nil))

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
    (let* ((threshold-pair (and (consp type-break-keystroke-threshold)
                                type-break-keystroke-threshold))
           (min-threshold (car threshold-pair))
           (max-threshold (cdr threshold-pair)))

      ;; Reset schedule and keystroke count if user has been idle longer
      ;; than a normal resting period.
      (cond
       (type-break-good-rest-interval
         (and (> (type-break-time-difference type-break-time-last-command
                                            (current-time))
                type-break-good-rest-interval)
             (progn
               (setq type-break-keystroke-count 0)
               (type-break-schedule)))
        (setq type-break-time-last-command (current-time))))

      (and threshold-pair
           (setq type-break-keystroke-count
                 (+ type-break-keystroke-count (length (this-command-keys)))))

      (cond
       ((input-pending-p))
       (type-break-alarm-p
        (cond
         ((and min-threshold
               (< type-break-keystroke-count min-threshold)))
         (t
          ;; If the keystroke count is within min-threshold characters of
          ;; the maximum threshold, set the count to min-threshold.  That
          ;; way, if the count was really close the threshold and the user
          ;; doesn't choose to take a break now, s/he won't be pestered
          ;; almost immediately after saying "no"; that's what the query
          ;; interval delay is for.
          ;; On the other hand, don't set it too small (make it at least
          ;; min-threshold); that way we can be sure the user will be asked
          ;; again to take a break after the query interval has elapsed.
          ;; If the user chooses to take a break now, the break function
          ;; will reset the keystroke count anyway.
          (and max-threshold
               min-threshold
               (> (- max-threshold type-break-keystroke-count) min-threshold)
               (setq type-break-keystroke-count min-threshold))
          (type-break-query))))
       ((and max-threshold
             (> type-break-keystroke-count max-threshold))
        ;; Set it to the min threshold if possible, to be sure the user
        ;; will be pestered again in at least a minute.
        (setq type-break-keystroke-count (or min-threshold 0))
        (type-break-query)))))))

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
     ;; eat char
     (read-char)
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


;;;###autoload
(defun type-break-statistics ()
  "Print statistics about typing breaks in a temporary buffer.
This includes the last time a typing break was taken, when the next one is
scheduled, the keystroke thresholds and the current keystroke count, etc."
  (interactive)
  (with-output-to-temp-buffer "*Typing Break Statistics*"
    (princ (format "Typing break statistics\n-----------------------\n
Last typing break           : %s
Next scheduled typing break : %s\n
Minimum keystroke threshold : %s
Maximum keystroke threshold : %s
Current keystroke count     : %s"
                   (if type-break-time-last-break
                       (current-time-string type-break-time-last-break)
                     "never")
                   (if (and type-break-mode type-break-time-next-break)
                       (format "%s\t(%d minutes from now)"
                               (current-time-string type-break-time-next-break)
                               (/ (type-break-time-difference
                                   (current-time) type-break-time-next-break)
                                  60))
                     "none scheduled")
                   (or (car type-break-keystroke-threshold) "none")
                   (or (cdr type-break-keystroke-threshold) "none")
                   type-break-keystroke-count))))

;;;###autoload
(defun type-break-guestimate-keystroke-threshold (wpm &optional wordlen frac)
  "Guess values for the minimum/maximum keystroke threshold for typing breaks.
If called interactively, the user is prompted for their guess as to how
many words per minute they usually type.  From that, the command sets the
values in `type-break-keystroke-threshold' based on a fairly simple
algorithm involving assumptions about the average length of words (5).
For the minimum threshold, it uses about a quarter of the computed maximum
threshold.

When called from lisp programs, the optional args WORDLEN and FRAC can be
used to override the default assumption about average word length and the
fraction of the maximum threshold to which to set the minimum threshold.
FRAC should be the inverse of the fractional value; for example, a value of
2 would mean to use one half, a value of 4 would mean to use one quarter, etc."
  (interactive "nHow many words per minute do you type? ")
  (let* ((upper (* wpm (or wordlen 5) (/ type-break-interval 60)))
         (lower (/ upper (or frac 4))))
    (or type-break-keystroke-threshold
        (setq type-break-keystroke-threshold (cons nil nil)))
    (setcar type-break-keystroke-threshold lower)
    (setcdr type-break-keystroke-threshold upper)
    (if (interactive-p)
        (message "min threshold: %d\tmax threshold: %d" lower upper)
      type-break-keystroke-threshold)))


(provide 'type-break)

(type-break-mode t)

;;; type-break.el ends here
