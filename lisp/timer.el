;;; timer.el --- run a function with args at some time in future.

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

;;; Code:

;; Layout of a timer vector:
;; [triggered-p high-seconds low-seconds usecs repeat-delay
;;  function args idle-delay]

(defun timer-create ()
  "Create a timer object."
  (let ((timer (make-vector 8 nil)))
    (aset timer 0 t)
    timer))

(defun timerp (object)
  "Return t if OBJECT is a timer."
  (and (vectorp object) (= (length object) 8)))

(defun timer-set-time (timer time &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'.
If optional third argument DELTA is a non-zero integer, make the timer
fire repeatedly that many seconds apart."
  (or (timerp timer)
      (error "Invalid timer"))
  (aset timer 1 (car time))
  (aset timer 2 (if (consp (cdr time)) (car (cdr time)) (cdr time)))
  (aset timer 3 (or (and (consp (cdr time)) (consp (cdr (cdr time)))
			 (nth 2 time))
		    0))
  (aset timer 4 (and (numberp delta) (> delta 0) delta))
  timer)

(defun timer-set-idle-time (timer secs &optional repeat)
  "Set the trigger idle time of TIMER to SECS.
If optional third argument REPEAT is non-nil, make the timer
fire each time Emacs is idle for that many seconds."
  (or (timerp timer)
      (error "Invalid timer"))
  (aset timer 1 0)
  (aset timer 2 0)
  (aset timer 3 0)
  (timer-inc-time timer secs)
  (aset timer 4 repeat)
  timer)

(defun timer-relative-time (time secs &optional usecs)
  "Advance TIME by SECS seconds and optionally USECS microseconds.
SECS may be a fraction."
  (let ((high (car time))
	(low (if (consp (cdr time)) (nth 1 time) (cdr time)))
	(micro (if (numberp (car-safe (cdr-safe (cdr time))))
		   (nth 2 time)
		 0)))
    ;; Add
    (if usecs (setq micro (+ micro usecs)))
    (if (floatp secs)
	(setq micro (+ micro (floor (* 1000000 (- secs (floor secs)))))))
    (setq low (+ low (floor secs)))

    ;; Normalize
    (setq low (+ low (/ micro 1000000)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536)))
    (setq low (logand low 65535))

    (list high low (and (/= micro 0) micro))))

(defun timer-inc-time (timer secs &optional usecs)
  "Increment the time set in TIMER by SECS seconds and USECS microseconds.
SECS may be a fraction."
  (let ((time (timer-relative-time
	       (list (aref timer 1) (aref timer 2) (aref timer 3))
	       secs
	       usecs)))
    (aset timer 1 (nth 0 time))
    (aset timer 2 (nth 1 time))
    (aset timer 3 (or (nth 2 time) 0))))

(defun timer-set-time-with-usecs (timer time usecs &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'.
If optional third argument DELTA is a non-zero integer, make the timer
fire repeatedly that many seconds apart."
  (or (timerp timer)
      (error "Invalid timer"))
  (aset timer 1 (car time))
  (aset timer 2 (if (consp (cdr time)) (car (cdr time)) (cdr time)))
  (aset timer 3 usecs)
  (aset timer 4 (and (numberp delta) (> delta 0) delta))
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
	(aset timer 7 nil)
	nil)
    (error "Invalid or uninitialized timer")))

(defun timer-activate-when-idle (timer)
  "Arrange to activate TIMER whenever Emacs is next idle."
  (if (and (timerp timer)
	   (integerp (aref timer 1))
	   (integerp (aref timer 2))
	   (integerp (aref timer 3))
	   (aref timer 5))
      (let ((timers timer-idle-list)
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
	  (setq timer-idle-list (cons timer timers)))
	(aset timer 0 t)
	(aset timer 7 t)
	nil)
    (error "Invalid or uninitialized timer")))

(defalias 'disable-timeout 'cancel-timer)
(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (or (timerp timer)
      (error "Invalid timer"))
  (setq timer-list (delq timer timer-list))
  (setq timer-idle-list (delq timer timer-idle-list))
  nil)

(defun cancel-function-timers (function)
  "Cancel all timers scheduled by `run-at-time' which would run FUNCTION."
  (interactive "aCancel timers of function: ")
  (let ((tail timer-list))
    (while tail
      (if (eq (aref (car tail) 5) function)
          (setq timer-list (delq (car tail) timer-list)))
      (setq tail (cdr tail))))
  (let ((tail timer-idle-list))
    (while tail
      (if (eq (aref (car tail) 5) function)
          (setq timer-idle-list (delq (car tail) timer-idle-list)))
      (setq tail (cdr tail)))))

;; Set up the common handler for all timer events.  Since the event has
;; the timer as parameter we can still distinguish.  Note that using
;; special-event-map ensures that event timer events that arrive in the
;; middle of a key sequence being entered are still handled correctly.
(define-key special-event-map [timer-event] 'timer-event-handler)

;; Record the last few events, for debugging.
(defvar timer-event-last-2 nil)
(defvar timer-event-last-1 nil)
(defvar timer-event-last nil)

(defun timer-event-handler (event)
  "Call the handler for the timer in the event EVENT."
  (interactive "e")
  (setq timer-event-last-2 timer-event-last-1)
  (setq timer-event-last-1 timer-event-last)
  (setq timer-event-last (cons event (copy-sequence event)))
  (let ((inhibit-quit t)
	(timer (car-safe (cdr-safe event))))
    (if (timerp timer)
	(progn
	  ;; Delete from queue.
	  (cancel-timer timer)
	  ;; Run handler
	  (condition-case nil
	      (apply (aref timer 5) (aref timer 6))
	    (error nil))
	  ;; Re-schedule if requested.
	  (if (aref timer 4)
	      (if (aref timer 7)
		  (timer-activate-when-idle timer)
		(timer-inc-time timer (aref timer 4) 0)
		(timer-activate timer))))
      (error "Bogus timer event"))))

;; This function is incompatible with the one in levents.el.
(defun timeout-event-p (event)
  "Non-nil if EVENT is a timeout event."
  (and (listp event) (eq (car event) 'timer-event)))

;;;###autoload
(defun run-at-time (time repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be a string like \"11:23pm\", nil meaning now, a number of seconds
from now, or a value from `encode-time'.
REPEAT may be an integer or floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  ;; Special case: nil means "now" and is useful when repeating.
  (if (null time)
      (setq time (current-time)))

  ;; Handle numbers as relative times in seconds.
  (if (numberp time)
      (setq time (timer-relative-time (current-time) time)))

  ;; Handle relative times like "2 hours and 35 minutes"
  (if (stringp time)
      (let ((secs (timer-duration time)))
	(if secs
	    (setq time (timer-relative-time (current-time) secs)))))

  ;; Handle "11:23pm" and the like.  Interpret it as meaning today
  ;; which admittedly is rather stupid if we have passed that time
  ;; already.  (Though only Emacs hackers hack Emacs at that time.)
  (if (stringp time)
      (progn
	(require 'diary-lib)
	(let ((hhmm (diary-entry-time time))
	      (now (decode-time)))
	  (if (>= hhmm 0)
	      (setq time
		    (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
				 (nth 4 now) (nth 5 now) (nth 8 now)))))))

  (or (consp time)
      (error "Invalid time format"))

  (or (null repeat)
      (numberp repeat)
      (error "Invalid repetition interval"))

  (let ((timer (timer-create)))
    (timer-set-time timer time repeat)
    (timer-set-function timer function args)
    (timer-activate timer)
    timer))

;;;###autoload
(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")
  (apply 'run-at-time secs repeat function args))

;;;###autoload
(defun add-timeout (secs function object &optional repeat)
  "Add a timer to run SECS seconds from now, to call FUNCTION on OBJECT.
If REPEAT is non-nil, repeat the timer every REPEAT seconds.
This function is for compatibility; see also `run-with-timer'."
  (run-with-timer secs repeat function object))

;;;###autoload
(defun run-with-idle-timer (secs repeat function &rest args)
  "Perform an action the next time Emacs is idle for SECS seconds.
If REPEAT is non-nil, do this each time Emacs is idle for SECS seconds.
SECS may be an integer or a floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive
   (list (read-from-minibuffer "Run after idle (seconds): " nil nil t)
	 (y-or-n-p "Repeat each time Emacs is idle? ")
	 (intern (completing-read "Function: " obarray 'fboundp t))))
  (let ((timer (timer-create)))
    (timer-set-function timer function args)
    (timer-set-idle-time timer secs repeat)
    (timer-activate-when-idle timer)
    timer))

(defun with-timeout-handler (tag)
  (throw tag 'timeout))

;;;###autoload (put 'with-timeout 'lisp-indent-function 1)

;;;###autoload
(defmacro with-timeout (list &rest body)
  "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The call should look like:
 (with-timeout (SECONDS TIMEOUT-FORMS...) BODY...)
The timeout is checked whenever Emacs waits for some kind of external
event \(such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected."
  (let ((seconds (car list))
	(timeout-forms (cdr list)))
    `(let ((with-timeout-tag (cons nil nil))
	   with-timeout-value with-timeout-timer)
       (if (catch with-timeout-tag
	     (progn
	       (setq with-timeout-timer
		     (run-with-timer ,seconds nil
				      'with-timeout-handler
				      with-timeout-tag))
	       (setq with-timeout-value (progn . ,body))
	       nil))
	   (progn . ,timeout-forms)
	 (cancel-timer with-timeout-timer)
	 with-timeout-value))))

(defun y-or-n-p-with-timeout (prompt seconds default-value)
  "Like (y-or-n-p PROMPT), with a timeout.
If the user does not answer after SECONDS seconds, return DEFAULT-VALUE."
  (with-timeout (seconds default-value)
    (y-or-n-p prompt)))

(defvar timer-duration-words
  (list (cons "microsec" 0.000001)
	(cons "microsecond" 0.000001)
        (cons "millisec" 0.001)
	(cons "millisecond" 0.001)
        (cons "sec" 1)
	(cons "second" 1)
	(cons "min" 60)
	(cons "minute" 60)
	(cons "hour" (* 60 60))
	(cons "day" (* 24 60 60))
	(cons "week" (* 7 24 60 60))
	(cons "fortnight" (* 14 24 60 60))
	(cons "month" (* 30 24 60 60))	  ; Approximation
	(cons "year" (* 365.25 24 60 60)) ; Approximation
	)
  "Alist mapping temporal words to durations in seconds")

(defun timer-duration (string)
  "Return number of seconds specified by STRING, or nil if parsing fails."
  (let ((secs 0)
	(start 0)
	(case-fold-search t))
    (while (string-match
	    "[ \t]*\\([0-9.]+\\)?[ \t]*\\([a-z]+[a-rt-z]\\)s?[ \t]*"
	    string start)
      (let ((count (if (match-beginning 1)
		       (string-to-number (match-string 1 string))
		     1))
	    (itemsize (cdr (assoc (match-string 2 string)
				  timer-duration-words))))
	(if itemsize
	    (setq start (match-end 0)
		  secs (+ secs (* count itemsize)))
	  (setq secs nil
		start (length string)))))
    (if (= start (length string))
	secs
      (if (string-match "\\`[0-9.]+\\'" string)
	  (string-to-number string)))))

(provide 'timer)

;;; timer.el ends here
