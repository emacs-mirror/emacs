;;; timer.el --- run a function with args at some time in future

;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package gives you the capability to run Emacs Lisp commands at
;; specified times in the future, either as one-shots or periodically.
;; The single entry point is `run-at-time'.

;;; Code:

(defvar timer-program (expand-file-name "timer" exec-directory)
  "The name of the program to run as the timer subprocess.
It should normally be in the exec-directory.")

(defvar timer-process nil)
(defvar timer-alist ())
(defvar timer-out "")
(defvar timer-dont-exit nil
  ;; this is useful for functions which will be doing their own erratic
  ;; rescheduling or people who otherwise expect to use the process frequently
  "If non-nil, don't exit the timer process when no more events are pending.")

;; Error symbols for timers
(put 'timer-error 'error-conditions '(error timer-error))
(put 'timer-error 'error-message "Timer error")

(put 'timer-abnormal-termination 
     'error-conditions 
     '(error timer-error timer-abnormal-termination))
(put 'timer-abnormal-termination 
     'error-message 
     "Timer exited abnormally--all events cancelled")

(put 'timer-filter-error
     'error-conditions
     '(error timer-error timer-filter-error))
(put 'timer-filter-error
     'error-message 
     "Error in timer process filter")


;; This should not be necessary, but on some systems, we get
;; unkillable processes without this.
;; It may be a kernel bug, but that's not certain.
(defun timer-kill-emacs-hook ()
  (if timer-process
      (progn
	(set-process-sentinel timer-process nil)
	(set-process-filter timer-process nil)
	(delete-process timer-process))))
(add-hook 'kill-emacs-hook 'timer-kill-emacs-hook)

;;;###autoload
(defun run-at-time (time repeat function &rest args)
  "Run a function at a time, and optionally on a regular interval.
Arguments are TIME, REPEAT, FUNCTION &rest ARGS.
TIME, a string, can be specified absolutely or relative to now.
TIME can also be an integer, a number of seconds.
REPEAT, an integer number of seconds, is the interval on which to repeat
the call to the function.  If REPEAT is nil or 0, call it just once.

Absolute times may be specified in a wide variety of formats;
Something of the form `HOUR:MIN:SEC TIMEZONE MONTH/DAY/YEAR', where
all fields are numbers, works; the format used by the Unix `date'
command works too.

Relative times may be specified as a series of numbers followed by units:
  1 min         	denotes one minute from now.
  min			does too.
  1 min 5 sec		denotes 65 seconds from now.
  1 min 2 sec 3 hour 4 day 5 week 6 fortnight 7 month 8 year
			denotes the sum of all the given durations from now."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")
  (if (equal repeat 0)
      (setq repeat nil))
  ;; Make TIME a string.
  (if (integerp time)
      (setq time (format "%d sec" time)))
  (cond ((or (not timer-process) 
             (memq (process-status timer-process) '(exit signal nil)))
         (if timer-process (delete-process timer-process))
         (setq timer-process
	       (let ((process-connection-type nil))
		 (start-process "timer" nil timer-program))
               timer-alist nil)
         (set-process-filter   timer-process 'timer-process-filter)
         (set-process-sentinel timer-process 'timer-process-sentinel)
         (process-kill-without-query timer-process))
        ((eq (process-status timer-process) 'stop)
         (continue-process timer-process)))
  ;; There should be a living, breathing timer process now
  (let* ((token (concat (current-time-string) "-" (length timer-alist)))
	 (elt (list token repeat function args)))
    (process-send-string timer-process (concat time "@" token "\n"))
    (setq timer-alist (cons elt timer-alist))
    elt))

(defun cancel-timer (elt)
  "Cancel a timer previously made with `run-at-time'.
The argument should be a value previously returned by `run-at-time'.
Cancelling the timer means that nothing special 
will happen at the specified time."
  (setcar (cdr elt) nil)
  (setcar (cdr (cdr elt)) 'ignore))

(defun timer-process-filter (proc str)
  (setq timer-out (concat timer-out str))
  (let (do token error)
    (while (string-match "\n" timer-out)
      (setq token (substring timer-out 0 (match-beginning 0))
	    do (assoc token timer-alist)
	    timer-out (substring timer-out (match-end 0)))
      (cond
       (do
	(apply (nth 2 do) (nth 3 do))	; do it
	(if (natnump (nth 1 do))	; reschedule it
	    (send-string proc (concat (nth 1 do) " sec@" (car do) "\n"))
	  (setq timer-alist (delq do timer-alist))))
       ((string-match "timer: \\([^:]+\\): \\([^@]*\\)@\\(.*\\)$" token)
	(setq error (substring token (match-beginning 1) (match-end 1))
	      do    (substring token (match-beginning 2) (match-end 2))
	      token (assoc (substring token (match-beginning 3) (match-end 3))
			   timer-alist)
	      timer-alist (delq token timer-alist))
	(or timer-alist 
	    timer-dont-exit
	    (process-send-eof proc))
	;; Update error message for this particular instance
	(put 'timer-filter-error
	     'error-message
	     (format "%s for %s; couldn't set at \"%s\"" 
		     error (nth 2 token) do))
	(signal 'timer-filter-error (list proc str)))))
    (or timer-alist timer-dont-exit (process-send-eof proc))))

(defun timer-process-sentinel (proc str)
  (let ((stat (process-status proc)))
    (if (eq stat 'stop)
	(continue-process proc)
      ;; if it exited normally, presumably it was intentional.
      ;; if there were no pending events, who cares that it exited?
      (or (null timer-alist)
          (eq stat 'exit)
          (let ((alist timer-alist))
            (setq timer-process nil timer-alist nil)
            (signal 'timer-abnormal-termination (list proc stat str alist))))
      ;; Used to set timer-scratch to "", but nothing uses that var.
      (setq timer-process nil timer-alist nil))))

(defun cancel-function-timers (function)
  "Cancel all events scheduled by `run-at-time' which would run FUNCTION."
  (interactive "aCancel timers of function: ")
  (let ((alist timer-alist))
    (while alist
      (if (eq (nth 2 (car alist)) function)
          (setq timer-alist (delq (car alist) timer-alist)))
      (setq alist (cdr alist))))
  (or timer-alist timer-dont-exit (process-send-eof timer-process)))

(provide 'timer)

;;; timer.el ends here
