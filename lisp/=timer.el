;;; timers.el --- run a function with args at some time in future

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Maintainer: FSF

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package gives you the capability to run Emacs Lisp commands at
;; specified times in the future, either as one-shots or periodically.
;; The single entry point is `run-at-time'.

;;; Code:

;; Layout of a timer vector:
;; [triggered-p trigger-high trigger-low delta-secs function args]

(defun timer-create ()
  "Create a timer object."
  (let ((timer (make-vector 7 nil)))
    (aset timer 0 (make-vector 1 'timer-event))
    timer))

(defun timerp (object)
  "Return t if OBJECT is a timer."
  (and (vectorp object) (= (length object) 7)))

(defun timer-set-time (timer time &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'
If optional third argument DELTA is a non-zero integer make the timer
fire repeatedly that menu seconds apart."
  (or (timerp timer)
      (error "Invalid timer"))
  (aset timer 1 (car time))
  (aset timer 2 (if (consp (cdr time)) (car (cdr time)) (cdr time)))
  (aset timer 3 (if (consp (cdr time)) (nth 2 time) 0))
  (aset timer 4 (and (integerp delta) (> delta 0) delta))
  timer)


(defun timer-inc-time (timer secs &optional usecs)
  "Increment the time set in TIMER by SECS seconds and USECS microseconds.
SECS may be a fraction."
  (or usecs (setq usecs 0))
  (if (floatp secs)
      (let* ((integer (floor secs))
	     (fraction (floor (* 1000000 (- secs integer)))))
	(setq usecs fraction secs integer)))
  (let ((newusecs (+ (aref timer 3) usecs)))
    (aset timer 3 (mod newusecs 1000000))
    (setq secs (+ secs (/ newusecs 1000000))))
  (let ((newlow (+ (aref timer 2) secs))
	(newhigh (aref timer 1)))
    (setq newhigh (+ newhigh (/ newlow 65536))
	  newlow (logand newlow 65535))
    (aset timer 1 newhigh)
    (aset timer 2 newlow)))

(defun timer-set-time-with-usecs (timer time usecs &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'
If optional third argument DELTA is a non-zero integer make the timer
fire repeatedly that menu seconds apart."
  (or (timerp timer)
      (error "Invalid timer"))
  (aset timer 1 (car time))
  (aset timer 2 (if (consp (cdr time)) (car (cdr time)) (cdr time)))
  (aset timer 3 usecs)
  (aset timer 4 (and (integerp delta) (> delta 0) delta))
  timer)

(defun timer-set-function (timer function &optional args)
  "Make TIMER call FUNCTION with optional ARGS when triggering."
  (or (timerp timer)
      (error "Invalid timer"))
  (aset timer 5 function)
  (aset timer 6 args)
  timer)

(defun timer-activate (timer)
  "Put TIMER on the list of active timers."
  (if (and (timerp timer)
	   (integerp (aref timer 1))
	   (integerp (aref timer 2))
	   (integerp (aref timer 3))
	   (aref timer 5))
      (let ((timers timer-list)
	    last)
	;; Skip all timers to trigger before the new one.
	(while (and timers
		    (or (> (aref timer 1) (aref (car timers) 1))
			(and (= (aref timer 1) (aref (car timers) 1))
			     (> (aref timer 2) (aref (car timers) 2)))
			(and (= (aref timer 1) (aref (car timers) 1))
			     (= (aref timer 2) (aref (car timers) 2))
			     (> (aref timer 3) (aref (car timers) 3)))))
	  (setq last timers
		timers (cdr timers)))
	;; Insert new timer after last which possibly means in front of queue.
	(if last
	    (setcdr last (cons timer timers))
	  (setq timer-list (cons timer timers)))
	(aset timer 0 nil)
	nil)
    (error "Invalid or uninitialized timer")))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (or (timerp timer)
      (error "Invalid timer"))
  (setq timer-list (delq timer timer-list))
  nil)

(defun cancel-function-timers (function)
  "Cancel all timers scheduled by `run-at-time' which would run FUNCTION."
  (interactive "aCancel timers of function: ")
  (let ((tail timer-list))
    (while tail
      (if (eq (aref (car tail) 5) function)
          (setq timer-list (delq (car tail) timer-list)))
      (setq tail (cdr tail)))))

;; Set up the common handler for all timer events.  Since the event has
;; the timer as parameter we can still distinguish.  Note that using
;; special-event-map ensures that event timer events that arrive in the
;; middle of a key sequence being entered are still handled correctly.
(define-key special-event-map [timer-event] 'timer-event-handler)
(defun timer-event-handler (event)
  "Call the handler for the timer in the event EVENT."
  (interactive "e")
  (let ((timer (cdr-safe event)))
    (if (timerp timer)
	(progn
	  ;; Delete from queue.
	  (cancel-timer timer)
	  ;; Run handler
	  (apply (aref timer 5) (aref timer 6))
	  ;; Re-schedule if requested.
	  (if (aref timer 4)
	      (progn
		(timer-inc-time timer (aref timer 4) 0)
		(timer-activate timer))))
      (error "Bogus timer event"))))

;;;###autoload
(defun run-at-time (time repeat function &rest args)
  "Run a function at a time, and optionally on a regular interval.
Arguments are TIME, REPEAT, FUNCTION &rest ARGS.
TIME is a string like \"11:23pm\" or a value from `encode-time'.
REPEAT, an integer number of seconds, is the interval on which to repeat
the call to the function.  If REPEAT is nil or 0, call it just once."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  ;; Handle "11:23pm" and the like.  Interpret it as meaning today
  ;; which admittedly is rather stupid if we have passed that time
  ;; already.  Unfortunately we don't have a `parse-time' function
  ;; to do the right thing.
  (if (stringp time)
      (progn
	(require 'diary-lib)
	(let ((hhmm (diary-entry-time time))
	      (now (decode-time)))
	  (if (< hhmm 0)
	      (setq time 'bad)
	    (setq time
		  (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
			       (nth 4 now) (nth 5 now) (nth 8 now)))))))

  ;; Special case: nil means "now" and is useful when repeting.
  (if (null time)
      (setq time (current-time)))

  (or (consp time)
      (error "Invalid time format"))

  (or (null repeat)
      (natnump repeat)
      (error "Invalid repetition interval"))

  (let ((timer (timer-create)))
    (timer-set-time timer time repeat)
    (timer-set-function timer function args)
    (timer-activate timer)))

(defun run-after-delay (secs usecs repeat function &rest args)
  "Perform an action after a delay of SECS seconds and USECS microseconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
The action is to call FUNCTION with arguments ARGS."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")

  (or (null repeat)
      (natnump repeat)
      (error "Invalid repetition interval"))

  (let ((timer (timer-create)))
    (timer-set-time timer (current-time))
    (timer-inc-time timer secs usecs)
    (timer-set-function timer function args)
    (timer-activate timer)))

(provide 'timers)

;;; timers.el ends here
