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

;;; This package was inspired by Roland McGrath's hanoi-break.el.
;;; Thanks to both Roland McGrath <roland@gnu.ai.mit.edu> and Mark Ashton
;;; <mpashton@gnu.ai.mit.edu> for feedback and ideas.

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
keystrokes.  If this idle time is long enough to be considered a \"good\"
rest from typing, then the next typing break is simply rescheduled for later.

If a break is interrupted before this much time elapses, the user will be
asked whether or not really to interrupt the break.")

;;;###autoload
(defvar type-break-keystroke-threshold
  ;; Assuming typing speed is 35wpm (on the average, do you really
  ;; type more than that in a minute?  I spend a lot of time reading mail
  ;; and simply studying code in buffers) and average word length is
  ;; about 5 letters, default upper threshold to the average number of
  ;; keystrokes one is likely to type in a break interval.  That way if the
  ;; user goes through a furious burst of typing activity, cause a typing
  ;; break to be required sooner than originally scheduled.
  ;; Conversely, the minimum threshold should be about a fifth of this.
  (let* ((wpm 35)
         (avg-word-length 5)
         (upper (* wpm avg-word-length (/ type-break-interval 60)))
         (lower (/ upper 5)))
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

(defvar type-break-time-warning-intervals '(300 120 60 30)
  "*List of time intervals for warnings about upcoming typing break.
At each of the intervals (specified in seconds) away from a scheduled
typing break, print a warning in the echo area.")

(defvar type-break-keystroke-warning-intervals '(300 200 100 50)
  "*List of keystroke measurements for warnings about upcoming typing break.
At each of the intervals (specified in keystrokes) away from the upper
keystroke threshold, print a warning in the echo area.
If either this variable or the upper threshold is set, then no warnings
Will occur.")

(defvar type-break-query-interval 60
  "*Number of seconds between queries to take a break, if put off.
The user will continue to be prompted at this interval until he or she
finally submits to taking a typing break.")

(defvar type-break-warning-repeat 40
  "*Number of keystrokes for which warnings should be repeated.
That is, for each of this many keystrokes the warning is redisplayed
in the echo area to make sure it's really seen.")

(defvar type-break-query-function 'yes-or-no-p
  "Function to use for making query for a typing break.
It should take a string as an argument, the prompt.
Usually this should be set to `yes-or-no-p' or `y-or-n-p'.")

(defvar type-break-demo-functions
  '(type-break-demo-boring type-break-demo-life type-break-demo-hanoi)
  "*List of functions to consider running as demos during typing breaks.
When a typing break begins, one of these functions is selected randomly
to have emacs do something interesting.

Any function in this list should start a demo which ceases as soon as a
key is pressed.")

(defvar type-break-post-command-hook nil
  "Hook run indirectly by post-command-hook for typing break functions.")

;; These are internal variables.  Do not set them yourself.

(defvar type-break-alarm-p nil)
(defvar type-break-keystroke-count 0)
(defvar type-break-time-last-break nil)
(defvar type-break-time-next-break nil)
(defvar type-break-time-last-command (current-time))
(defvar type-break-current-time-warning-interval nil)
(defvar type-break-current-keystroke-warning-interval nil)
(defvar type-break-time-warning-count 0)
(defvar type-break-keystroke-warning-count 0)


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

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

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

If set, the variable `type-break-good-rest-interval' specifies the minimum
amount of time which is considered a reasonable typing break.  Whenever
that time has elapsed, typing breaks are automatically rescheduled for
later even if emacs didn't prompt you to take one first.  Also, if a break
is ended before this much time has elapsed, the user will be asked whether
or not to continue.

The variable `type-break-keystroke-threshold' is used to determine the
thresholds at which typing breaks should be considered.  You can use
the command `type-break-guestimate-keystroke-threshold' to try to
approximate good values for this.

Finally, the command `type-break-statistics' prints interesting things."
  (interactive "P")
  ;; make sure it's there.
  (add-hook 'post-command-hook 'type-break-run-tb-post-command-hook 'append)
  (add-hook 'type-break-post-command-hook 'type-break-check)

  (let ((already-enabled type-break-mode))
    (setq type-break-mode (>= (prefix-numeric-value prefix) 0))

    (cond
     ((and already-enabled type-break-mode)
      (and (interactive-p)
           (message "type-break-mode is enabled")))
     (type-break-mode
      (type-break-keystroke-reset)
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
`type-break-demo-functions' is run.

After the typing break is finished, the next break is scheduled
as per the function `type-break-schedule'."
  (interactive)
  (type-break-cancel-schedule)
  (let ((continue t)
        (start-time (current-time)))
    (setq type-break-time-last-break start-time)
    (while continue
      (save-window-excursion
        ;; Eat the screen.
        (and (eq (selected-window) (minibuffer-window))
             (other-window 1))
        (delete-other-windows)
        (scroll-right (window-width))
        (message "Press any key to resume from typing break.")

        (random t)
        (let* ((len (length type-break-demo-functions))
               (idx (random len))
               (fn (nth idx type-break-demo-functions)))
          (condition-case ()
              (funcall fn)
            (error nil))))

      (cond
       (type-break-good-rest-interval
        (let ((break-secs (type-break-time-difference
                           start-time (current-time))))
          (cond
           ((>= break-secs type-break-good-rest-interval)
            (setq continue nil))
           ;; Don't be pedantic; if user's rest was only a minute short,
           ;; why bother?
           ((> 60 (abs (- break-secs type-break-good-rest-interval)))
            (setq continue nil))
           ((funcall
             type-break-query-function
             (format "You really ought to rest %s more.  Continue break? "
                     (type-break-format-time (- type-break-good-rest-interval
                                                break-secs)))))
           (t
            (setq continue nil)))))
       (t (setq continue nil)))))

  (type-break-keystroke-reset)
  (type-break-schedule))


(defun type-break-schedule (&optional time)
  "Schedule a typing break for TIME seconds from now.
If time is not specified, default to `type-break-interval'."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (or time (setq time type-break-interval))
  (type-break-cancel-schedule)
  (type-break-time-warning-schedule time 'reset)
  (run-at-time time nil 'type-break-alarm)
  (setq type-break-time-next-break
        (type-break-time-sum (current-time) time)))

(defun type-break-cancel-schedule ()
  (type-break-cancel-time-warning-schedule)
  (let ((timer-dont-exit t))
    (cancel-function-timers 'type-break-alarm))
  (setq type-break-alarm-p nil)
  (setq type-break-time-next-break nil))

(defun type-break-time-warning-schedule (&optional time resetp)
  (let (type-break-current-time-warning-interval)
    (type-break-cancel-time-warning-schedule))
  (cond
   (type-break-time-warning-intervals
    (and resetp
         (setq type-break-current-time-warning-interval
               type-break-time-warning-intervals))

    (or time
        (setq time (type-break-time-difference (current-time)
                                               type-break-time-next-break)))

    (while (and type-break-current-time-warning-interval
                (> (car type-break-current-time-warning-interval) time))
      (setq type-break-current-time-warning-interval
            (cdr type-break-current-time-warning-interval)))

    (cond
     (type-break-current-time-warning-interval
      (setq time (- time (car type-break-current-time-warning-interval)))
      (setq type-break-current-time-warning-interval
            (cdr type-break-current-time-warning-interval))

      (let (type-break-current-time-warning-interval)
        (type-break-cancel-time-warning-schedule))
      (run-at-time time nil 'type-break-time-warning-alarm))))))

(defun type-break-cancel-time-warning-schedule ()
  (let ((timer-dont-exit t))
    (cancel-function-timers 'type-break-time-warning-alarm))
  (remove-hook 'type-break-post-command-hook 'type-break-time-warning)
  (setq type-break-current-time-warning-interval
        type-break-time-warning-intervals))

(defun type-break-alarm ()
  (setq type-break-alarm-p t))

(defun type-break-time-warning-alarm ()
  (type-break-time-warning-schedule)
  (setq type-break-time-warning-count type-break-warning-repeat)
  (add-hook 'type-break-post-command-hook 'type-break-time-warning 'append))


(defun type-break-run-tb-post-command-hook ()
  (and type-break-mode
       (run-hooks 'type-break-post-command-hook)))

(defun type-break-check ()
  "Ask to take a typing break if appropriate.
This may be the case either because the scheduled time has come \(and the
minimum keystroke threshold has been reached\) or because the maximum
keystroke threshold has been exceeded."
  (let* ((min-threshold (car type-break-keystroke-threshold))
         (max-threshold (cdr type-break-keystroke-threshold)))
    (and type-break-good-rest-interval
         (progn
           (and (> (type-break-time-difference
                    type-break-time-last-command (current-time))
                   type-break-good-rest-interval)
                (progn
                  (type-break-keystroke-reset)
                  (setq type-break-time-last-break (current-time))
                  (type-break-schedule)))
           (setq type-break-time-last-command (current-time))))

    (and type-break-keystroke-threshold
         (setq type-break-keystroke-count
               (+ type-break-keystroke-count (length (this-command-keys)))))

    ;; This has been optimized for speed; calls to input-pending-p and
    ;; checking for the minibuffer window are only done if it would
    ;; matter for the sake of querying user.
    (cond
     (type-break-alarm-p
      (cond
       ((input-pending-p))
       ((eq (selected-window) (minibuffer-window)))
       ((and min-threshold
             (< type-break-keystroke-count min-threshold))
        (type-break-schedule))
       (t
        ;; If keystroke count is within min-threshold of
        ;; max-threshold, lower it to reduce the liklihood of an
        ;; immediate subsequent query.
        (and max-threshold
             min-threshold
             (< (- max-threshold type-break-keystroke-count) min-threshold)
             (progn
               (type-break-keystroke-reset)
               (setq type-break-keystroke-count min-threshold)))
        (type-break-query))))
     ((and type-break-keystroke-warning-intervals
           max-threshold
           (= type-break-keystroke-warning-count 0)
           (type-break-check-keystroke-warning)))
     ((and max-threshold
           (> type-break-keystroke-count max-threshold)
           (not (input-pending-p))
           (not (eq (selected-window) (minibuffer-window))))
      (type-break-keystroke-reset)
      (setq type-break-keystroke-count (or min-threshold 0))
      (type-break-query)))))

;; This should return t if warnings were enabled, nil otherwise.
(defsubst type-break-check-keystroke-warning ()
  (let ((left (- (cdr type-break-keystroke-threshold)
                 type-break-keystroke-count)))
    (cond
     ((null (car type-break-current-keystroke-warning-interval))
      nil)
     ((> left (car type-break-current-keystroke-warning-interval))
      nil)
     (t
      (while (and (car type-break-current-keystroke-warning-interval)
                  (< left (car type-break-current-keystroke-warning-interval)))
        (setq type-break-current-keystroke-warning-interval
              (cdr type-break-current-keystroke-warning-interval)))
      (setq type-break-keystroke-warning-count type-break-warning-repeat)
      (add-hook 'type-break-post-command-hook 'type-break-keystroke-warning)
      t))))

(defun type-break-query ()
  (condition-case ()
      (cond
       ((let ((type-break-mode nil))
          (funcall type-break-query-function "Take a break from typing now? "))
        (type-break))
       (t
        (type-break-schedule type-break-query-interval)))
    (quit
     (type-break-schedule type-break-query-interval))))

(defun type-break-time-warning ()
  (cond
   ((and (car type-break-keystroke-threshold)
         (< type-break-keystroke-count (car type-break-keystroke-threshold))))
   ((> type-break-time-warning-count 0)
    (cond
     ((eq (selected-window) (minibuffer-window)))
     (t
      ;; Pause for a moment so previous messages can be seen.
      (sit-for 2)
      (message "Warning: typing break due in %s."
               (type-break-format-time
                (type-break-time-difference (current-time)
                                            type-break-time-next-break)))
      (setq type-break-time-warning-count
            (1- type-break-time-warning-count)))))
   (t
    (remove-hook 'type-break-post-command-hook 'type-break-time-warning))))

(defun type-break-keystroke-warning ()
  (cond
   ((> type-break-keystroke-warning-count 0)
    (cond
     ((eq (selected-window) (minibuffer-window)))
     (t
      (sit-for 2)
      (message "Warning: typing break due in %s keystrokes."
               (- (cdr type-break-keystroke-threshold)
                  type-break-keystroke-count))
      (setq type-break-keystroke-warning-count
            (1- type-break-keystroke-warning-count)))))
   (t
    (remove-hook 'type-break-post-command-hook
                 'type-break-keystroke-warning))))


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
                       (format "%s\t(%s from now)"
                               (current-time-string type-break-time-next-break)
                               (type-break-format-time
                                (type-break-time-difference
                                (current-time)
                                type-break-time-next-break)))
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
  (interactive "NHow many words per minute do you type? ")
  (let* ((upper (* wpm (or wordlen 5) (/ type-break-interval 60)))
         (lower (/ upper (or frac 5))))
    (or type-break-keystroke-threshold
        (setq type-break-keystroke-threshold (cons nil nil)))
    (setcar type-break-keystroke-threshold lower)
    (setcdr type-break-keystroke-threshold upper)
    (if (interactive-p)
        (message "min threshold: %d\tmax threshold: %d" lower upper)
      type-break-keystroke-threshold)))


;;; misc functions

;; Compute the difference, in seconds, between a and b, two structures
;; similar to those returned by `current-time'.
;; Use addition rather than logand since that is more robust; the low 16
;; bits of the seconds might have been incremented, making it more than 16
;; bits wide.
(defsubst type-break-time-difference (a b)
  (+ (lsh (- (car b) (car a)) 16)
     (- (car (cdr b)) (car (cdr a)))))

;; Return (in a new list the same in structure to that returned by
;; `current-time') the sum of the arguments.  Each argument may be a time
;; list or a single integer, a number of seconds.
;; This function keeps the high and low 16 bits of the seconds properly
;; balanced so that the lower value never exceeds 16 bits.  Otherwise, when
;; the result is passed to `current-time-string' it will toss some of the
;; "low" bits and return the wrong value.
(defun type-break-time-sum (&rest tmlist)
  (let ((high 0)
        (low 0)
        (micro 0)
        tem)
    (while tmlist
      (setq tem (car tmlist))
      (setq tmlist (cdr tmlist))
      (cond
       ((numberp tem)
        (setq low (+ low tem)))
       (t
        (setq high  (+ high  (or (car tem) 0)))
        (setq low   (+ low   (or (car (cdr tem)) 0)))
        (setq micro (+ micro (or (car (cdr (cdr tem))) 0))))))

    (and (>= micro 1000000)
         (progn
           (setq tem (/ micro 1000000))
           (setq low (+ low tem))
           (setq micro (- micro (* tem 1000000)))))

    (setq tem (lsh low -16))
    (and (> tem 0)
         (progn
           (setq low (logand low 65535))
           (setq high (+ high tem))))

    (list high low micro)))

(defsubst type-break-format-time (secs)
  (let ((mins (/ secs 60)))
    (cond
     ((= mins 1) (format "%d minute" mins))
     ((> mins 0) (format "%d minutes" mins))
     ((= secs 1) (format "%d second" secs))
     (t (format "%d seconds" secs)))))

(defun type-break-keystroke-reset ()
  (setq type-break-keystroke-count 0)
  (setq type-break-keystroke-warning-count 0)
  (setq type-break-current-keystroke-warning-interval
        type-break-keystroke-warning-intervals)
  (remove-hook 'type-break-post-command-hook 'type-break-keystroke-warning))


;;; Demo wrappers

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
  (let ((continue t))
    (while continue
      (setq continue nil)
      (and (get-buffer "*Life*")
           (kill-buffer "*Life*"))
      (condition-case ()
          (progn
            (life 3)
            ;; wait for user to return
            (read-char)
            (kill-buffer "*Life*"))
        (life-extinct
         (message (get 'life-extinct 'error-message))
         (sit-for 3)
         ;; restart demo
         (setq continue t))
        (quit
         (and (get-buffer "*Life*")
              (kill-buffer "*Life*")))))))

;; Boring demo, but doesn't use many cycles
(defun type-break-demo-boring ()
  "Boring typing break demo."
  (let ((rmsg "Press any key to resume from typing break")
        (buffer-name "*Typing Break Buffer*")
        line col pos
        elapsed timeleft tmsg)
    (condition-case ()
        (progn
          (switch-to-buffer (get-buffer-create buffer-name))
          (buffer-disable-undo (current-buffer))
          (erase-buffer)
          (setq line (1+ (/ (window-height) 2)))
          (setq col (/ (- (window-width) (length rmsg)) 2))
          (insert (make-string line ?\C-j)
                  (make-string col ?\ )
                  rmsg)
          (forward-line -1)
          (beginning-of-line)
          (setq pos (point))
          (while (not (input-pending-p))
            (delete-region pos (progn
                                 (goto-char pos)
                                 (end-of-line)
                                 (point)))
            (setq elapsed (type-break-time-difference
                           type-break-time-last-break
                           (current-time)))
            (cond
             (type-break-good-rest-interval
              (setq timeleft (- type-break-good-rest-interval elapsed))
              (if (> timeleft 0)
                  (setq tmsg (format "You should rest for %s more"
                                     (type-break-format-time timeleft)))
                (setq tmsg (format "Typing break has lasted %s"
                                   (type-break-format-time elapsed)))))
             (t
              (setq tmsg (format "Typing break has lasted %s"
                                 (type-break-format-time elapsed)))))
            (setq col (/ (- (window-width) (length tmsg)) 2))
            (insert (make-string col ?\ ) tmsg)
            (goto-char (point-min))
            (sit-for 60))
          (read-char)
          (kill-buffer buffer-name))
      (quit
       (and (get-buffer buffer-name)
            (kill-buffer buffer-name))))))


(provide 'type-break)

;; Do not do this at load time because it makes it impossible to load this
;; file into temacs and then dump it.
;(type-break-mode t)

;; local variables:
;; vc-make-backup-files: t
;; end:

;;; type-break.el ends here
