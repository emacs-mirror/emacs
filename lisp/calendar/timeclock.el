;;; timeclock.el --- mode for keeping track of how much you work

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 25 Mar 1999
;; Version: 2.2
;; Keywords: calendar data

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

;; This mode is for keeping track of time intervals.  You can use it
;; for whatever purpose you like, but the typical scenario is to keep
;; track of how much time you spend working on certain projects.
;;
;; Use `timeclock-in' when you start on a project, and `timeclock-out'
;; when you're done.  Once you've collected some data, you can use
;; `timeclock-workday-remaining' to see how much time is left to be
;; worked today (assuming a typical average of 8 hours a day), and
;; `timeclock-when-to-leave' which will calculate when you're free.

;; You'll probably want to bind the timeclock commands to some handy
;; keystrokes.  At the moment, C-x t is unused in Emacs 20:
;;
;;   (require 'timeclock)
;;
;;   (define-key ctl-x-map "ti" 'timeclock-in)
;;   (define-key ctl-x-map "to" 'timeclock-out)
;;   (define-key ctl-x-map "tc" 'timeclock-change)
;;   (define-key ctl-x-map "tr" 'timeclock-reread-log)
;;   (define-key ctl-x-map "tu" 'timeclock-update-modeline)
;;   (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

;; If you want Emacs to display the amount of time "left" to your
;; workday in the modeline, you can either set the value of
;; `timeclock-modeline-display' to t using M-x customize, or you
;; can add this code to your .emacs file:
;;
;;   (require 'timeclock)
;;   (timeclock-modeline-display)
;;
;; To cancel this modeline display at any time, just call
;; `timeclock-modeline-display' again.

;; You may also want Emacs to ask you before exiting, if you are
;; current working on a project.  This can be done either by setting
;; `timeclock-ask-before-exiting' to t using M-x customize (this is
;; the default), or by adding the following to your .emacs file:
;;
;;   (add-hook 'kill-emacs-hook 'timeclock-query-out)

;; NOTE: If you change your .timelog file without using timeclock's
;; functions, or if you change the value of any of timeclock's
;; customizable variables, you should run the command
;; `timeclock-reread-log'.  This will recompute any discrepancies in
;; your average working time, and will make sure that the various
;; display functions return the correct value.

;;; History:

;;; Code:

(defgroup timeclock nil
  "Keeping track time of the time that gets spent."
  :group 'data)

;;; User Variables:

(defcustom timeclock-file (convert-standard-filename "~/.timelog")
  "*The file used to store timeclock data in."
  :type 'file
  :group 'timeclock)

(defcustom timeclock-workday (* 8 60 60)
  "*The length of a work period."
  :type 'integer
  :group 'timeclock)

(defcustom timeclock-relative t
  "*When reporting time, make it relative to `timeclock-workday'?
For example, if the length of a normal workday is eight hours, and you
work four hours on Monday, then the amount of time \"remaining\" on
Tuesday is twelve hours -- relative to an averaged work period of
eight hours -- or eight hours, non-relative.  So relative time takes
into account any discrepancy of time under-worked or overworked on
previous days."
  :type 'boolean
  :group 'timeclock)

(defcustom timeclock-get-project-function 'timeclock-ask-for-project
  "*The function used to determine the name of the current project.
When clocking in, and no project is specified, this function will be
called to determine what the current project to be worked on is.
If this variable is nil, no questions will be asked."
  :type 'function
  :group 'timeclock)

(defcustom timeclock-get-reason-function 'timeclock-ask-for-reason
  "*A function used to determine the reason for clocking out.
When clocking out, and no reason is specified, this function will be
called to determine what the reason is.
If this variable is nil, no questions will be asked."
  :type 'function
  :group 'timeclock)

(defcustom timeclock-get-workday-function nil
  "*A function used to determine the length of today's workday.
The first time that a user clocks in each day, this function will be
called to determine what the length of the current workday is.  If
nil, or equal to `timeclock-workday', nothing special will be done.
If it is a quantity different from `timeclock-workday', however, a
record will be output to the timelog file to note the fact that that
day has a different length from the norm."
  :type 'function
  :group 'timeclock)

(defcustom timeclock-ask-before-exiting t
  "*If non-nil, ask if the user wants to clock out before exiting Emacs."
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'kill-emacs-hook 'timeclock-query-out)
	   (remove-hook 'kill-emacs-hook 'timeclock-query-out))
	 (setq timeclock-ask-before-exiting value))
  :type 'boolean
  :group 'timeclock)

(defvar timeclock-update-timer nil
  "The timer used to update `timeclock-mode-string'.")

(defcustom timeclock-use-display-time t
  "*If non-nil, use `display-time-hook' for doing modeline updates.
The advantage to this is that it means one less timer has to be set
running amok in Emacs' process space.  The disadvantage is that it
requires you to have `display-time' running.  If you don't want to use
`display-time', but still want the modeline to show how much time is
left, set this variable to nil.  You will need to restart Emacs (or
toggle the value of `timeclock-modeline-display') for the change to
take effect."
  :set (lambda (symbol value)
	 (let ((currently-displaying
		(and (boundp 'timeclock-modeline-display)
		     timeclock-modeline-display)))
	   ;; if we're changing to the state that
	   ;; `timeclock-modeline-display' is already using, don't
	   ;; bother toggling it.  This happens on the initial loading
	   ;; of timeclock.el.
	   (if (and currently-displaying
		    (or (and value
			     (boundp 'display-time-hook)
			     (memq 'timeclock-update-modeline
				   display-time-hook))
			(and (not value)
			     timeclock-update-timer)))
	       (setq currently-displaying nil))
	   (and currently-displaying
		(set-variable timeclock-modeline-display nil))
	   (setq timeclock-use-display-time value)
	   (and currently-displaying
		(set-variable timeclock-modeline-display t))
	   timeclock-use-display-time))
  :type 'boolean
  :group 'timeclock
  :require 'time)

(defcustom timeclock-first-in-hook nil
  "*A hook run for the first \"in\" event each day.
Note that this hook is run before recording any events.  Thus the
value of `timeclock-hours-today', `timeclock-last-event' and the
return value of function `timeclock-last-period' are relative previous
to today."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-load-hook nil
  "*Hook that gets run after timeclock has been loaded."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-in-hook nil
  "*A hook run every time an \"in\" event is recorded."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-day-over-hook nil
  "*A hook that is run when the workday has been completed.
This hook is only run if the current time remaining is being display
in the modeline.  See the variable `timeclock-modeline-display'."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-out-hook nil
  "*A hook run every time an \"out\" event is recorded."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-done-hook nil
  "*A hook run every time a project is marked as completed."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-event-hook nil
  "*A hook run every time any event is recorded."
  :type 'hook
  :group 'timeclock)

(defvar timeclock-last-event nil
  "A list containing the last event that was recorded.
The format of this list is (CODE TIME PROJECT).  PROJECT will be
non-nil only if CODE is \"o\" or \"O\".")

(defvar timeclock-last-event-workday nil
  "The number of seconds in the workday of `timeclock-last-event'.")

;;; Internal Variables:

(defvar timeclock-discrepancy nil
  "A variable containing the time discrepancy before the last event.
Normally, timeclock assumes that you intend to work for
`timeclock-workday' seconds every day.  Any days in which you work
more or less than this amount is considered either a positive or
negative discrepancy.  If you work in such a manner that the
discrepancy is always brought back to zero, then you will by
definition have worked an average amount equal to `timeclock-workday'
each day.")

(defvar timeclock-elapsed nil
  "A variable containing the time elapsed for complete periods today.
This value is not accurate enough to be useful by itself.  Rather,
call `timeclock-workday-elapsed', to determine how much time has been
worked so far today.  Also, if `timeclock-relative' is nil, this value
will be the same as `timeclock-discrepancy'.")

(defvar timeclock-last-period nil
  "Integer representing the number of seconds in the last period.
Note that you shouldn't access this value, but should use the function
`timeclock-last-period' instead.")

(defvar timeclock-mode-string nil
  "The timeclock string (optionally) displayed in the modeline.")

(defvar timeclock-day-over nil
  "The date of the last day when notified \"day over\" for.")

;;; User Functions:

;;;###autoload
(defun timeclock-modeline-display (&optional arg)
  "Toggle display of the amount of time left today in the modeline.
If `timeclock-use-display-time' is non-nil, the modeline will be
updated whenever the time display is updated.  Otherwise, the
timeclock will use its own sixty second timer to do its updating.
With prefix ARG, turn modeline display on if and only if ARG is
positive.  Returns the new status of timeclock modeline display
\(non-nil means on)."
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
		(not timeclock-modeline-display))))
    (if on-p
	(let ((list-entry (memq 'global-mode-string
				mode-line-format)))
	  (unless (memq 'timeclock-mode-string mode-line-format)
	    (setcdr list-entry
		    (cons 'timeclock-mode-string
			  (cdr list-entry))))
	  (unless (memq 'timeclock-update-modeline timeclock-event-hook)
	    (add-hook 'timeclock-event-hook 'timeclock-update-modeline))
	  (when timeclock-update-timer
	    (cancel-timer timeclock-update-timer)
	    (setq timeclock-update-timer nil))
	  (if (boundp 'display-time-hook)
	      (remove-hook 'display-time-hook 'timeclock-update-modeline))
	  (if timeclock-use-display-time
	      (add-hook 'display-time-hook 'timeclock-update-modeline)
	    (setq timeclock-update-timer
		  (run-at-time nil 60 'timeclock-update-modeline))))
      (setq mode-line-format
	    (delq 'timeclock-mode-string mode-line-format))
      (remove-hook 'timeclock-event-hook 'timeclock-update-modeline)
      (if (boundp 'display-time-hook)
	  (remove-hook 'display-time-hook
		       'timeclock-update-modeline))
      (when timeclock-update-timer
	(cancel-timer timeclock-update-timer)
	(setq timeclock-update-timer nil)))
    (force-mode-line-update)
    on-p))

;; This has to be here so that the function definition of
;; `timeclock-modeline-display' is known to the "set" function.
(defcustom timeclock-modeline-display nil
  "Toggle modeline display of time remaining.
You must modify via \\[customize] for this variable to have an effect."
  :set (lambda (symbol value)
	 (setq timeclock-modeline-display
	       (timeclock-modeline-display (or value 0))))
  :type 'boolean
  :group 'timeclock
  :require 'timeclock)

;;;###autoload
(defun timeclock-in (&optional arg project find-project)
  "Clock in, recording the current time moment in the timelog.
With a numeric prefix ARG, record the fact that today has only that
many hours in it to be worked.  If arg is a non-numeric prefix arg
\(non-nil, but not a number), 0 is assumed (working on a holiday or
weekend).  *If not called interactively, ARG should be the number of
_seconds_ worked today*.  This feature only has effect the first time
this function is called within a day.

PROJECT as the project being clocked into.  If PROJECT is nil, and
FIND-PROJECT is non-nil -- or the user calls `timeclock-in'
interactively -- call the function `timeclock-get-project-function' to
discover the name of the project."
  (interactive
   (list (and current-prefix-arg
	      (if (numberp current-prefix-arg)
		  (* current-prefix-arg 60 60)
		0))))
  (if (equal (car timeclock-last-event) "i")
      (error "You've already clocked in!")
    (unless timeclock-last-event
      (timeclock-reread-log))
    (unless (equal (timeclock-time-to-date
		    (cadr timeclock-last-event))
		   (timeclock-time-to-date (current-time)))
      (let ((workday (or (and (numberp arg) arg)
			 (and arg 0)
			 (and timeclock-get-workday-function
			      (funcall timeclock-get-workday-function))
			 timeclock-workday)))
	(run-hooks 'timeclock-first-in-hook)
	;; settle the discrepancy for the new day
	(setq timeclock-discrepancy
	      (- timeclock-discrepancy workday))
	(if (not (= workday timeclock-workday))
	    (timeclock-log "h" (and (numberp arg)
				    (number-to-string arg))))))
    (timeclock-log "i" (or project
			   (and timeclock-get-project-function
				(or find-project (interactive-p))
				(funcall timeclock-get-project-function))))
    (run-hooks 'timeclock-in-hook)))

;;;###autoload
(defun timeclock-out (&optional arg reason find-reason)
  "Clock out, recording the current time moment in the timelog.
If a prefix ARG is given, the user has completed the project that was
begun during the last time segment.

REASON is the user's reason for clocking out.  If REASON is nil, and
FIND-REASON is non-nil -- or the user calls `timeclock-out'
interactively -- call the function `timeclock-get-reason-function' to
discover the reason."
  (interactive "P")
  (if (equal (downcase (car timeclock-last-event)) "o")
      (error "You've already clocked out!")
    (timeclock-log
     (if arg "O" "o")
     (or reason
	 (and timeclock-get-reason-function
	      (or find-reason (interactive-p))
	      (funcall timeclock-get-reason-function))))
    (run-hooks 'timeclock-out-hook)
    (if arg
	(run-hooks 'timeclock-done-hook))))

;;;###autoload
(defun timeclock-status-string (&optional show-seconds today-only)
  "Report the overall timeclock status at the present moment."
  (interactive "P")
  (let* ((remainder (timeclock-workday-remaining))
	 (last-in (equal (car timeclock-last-event) "i"))
	 status)
    (setq status
	  (format "Currently %s since %s (%s), %s %s, leave at %s"
		  (if last-in "IN" "OUT")
		  (if show-seconds
		      (format-time-string "%-I:%M:%S %p"
					  (nth 1 timeclock-last-event))
		    (format-time-string "%-I:%M %p"
					(nth 1 timeclock-last-event)))
		  (or (nth 2 timeclock-last-event)
		      (if last-in "**UNKNOWN**" "workday over"))
		  (timeclock-seconds-to-string remainder show-seconds t)
		  (if (> remainder 0)
		      "remaining" "over")
		  (timeclock-when-to-leave-string show-seconds today-only)))
    (if (interactive-p)
	(message status)
      status)))

;;;###autoload
(defun timeclock-change (&optional arg project)
  "Change to working on a different project, by clocking in then out.
With a prefix ARG, consider the previous project as having been
finished at the time of changeover.  PROJECT is the name of the last
project you were working on."
  (interactive "P")
  (timeclock-out arg)
  (timeclock-in nil project (interactive-p)))

;;;###autoload
(defun timeclock-query-out ()
  "Ask the user before clocking out.
This is a useful function for adding to `kill-emacs-hook'."
  (if (and (equal (car timeclock-last-event) "i")
	   (y-or-n-p "You're currently clocking time, clock out? "))
      (timeclock-out)))

;;;###autoload
(defun timeclock-reread-log ()
  "Re-read the timeclock, to account for external changes.
Returns the new value of `timeclock-discrepancy'."
  (interactive)
  (setq timeclock-discrepancy nil)
  (timeclock-find-discrep)
  (if timeclock-modeline-display
      (timeclock-update-modeline))
  timeclock-discrepancy)

(defun timeclock-seconds-to-string (seconds &optional show-seconds
					    reverse-leader)
  "Convert SECONDS into a compact time string.
If SHOW-SECONDS is non-nil, make the resolution of the return string
include the second count.  If REVERSE-LEADER is non-nil, it means to
output a \"+\" if the time value is negative, rather than a \"-\".
This is used when negative time values have an inverted meaning (such
as with time remaining, where negative time really means overtime)."
  (if show-seconds
      (format "%s%d:%02d:%02d"
	      (if (< seconds 0) (if reverse-leader "+" "-") "")
	      (truncate (/ (abs seconds) 60 60))
	      (% (truncate (/ (abs seconds) 60)) 60)
	      (% (truncate (abs seconds)) 60))
    (format "%s%d:%02d"
	    (if (< seconds 0) (if reverse-leader "+" "-") "")
	    (truncate (/ (abs seconds) 60 60))
	    (% (truncate (/ (abs seconds) 60)) 60))))

(defun timeclock-workday-remaining (&optional today-only)
  "Return a the number of seconds until the workday is complete.
The amount returned is relative to the value of `timeclock-workday'.
If TODAY-ONLY is non-nil, the value returned will be relative only to
the time worked today, and not to past time.  This argument only makes
a difference if `timeclock-relative' is non-nil."
  (- (timeclock-find-discrep today-only)))

(defun timeclock-currently-in-p ()
  "Return non-nil if the user is currently clocked in."
  (equal (car timeclock-last-event) "i"))

;;;###autoload
(defun timeclock-workday-remaining-string (&optional show-seconds
						     today-only)
  "Return a string representing the amount of time left today.
Display second resolution if SHOW-SECONDS is non-nil.  If TODAY-ONLY
is non-nil, the display will be relative only to time worked today.
See `timeclock-relative' for more information about the meaning of
\"relative to today\"."
  (interactive)
  (let ((string (timeclock-seconds-to-string
		 (timeclock-workday-remaining today-only)
		 show-seconds t)))
    (if (interactive-p)
	(message string)
      string)))

(defun timeclock-workday-elapsed (&optional relative)
  "Return a the number of seconds worked so far today.
If RELATIVE is non-nil, the amount returned will be relative to past
time worked.  The default is to return only the time that has elapsed
so far today."
  (+ timeclock-workday
     (timeclock-find-discrep (not relative))))

;;;###autoload
(defun timeclock-workday-elapsed-string (&optional show-seconds
						   relative)
  "Return a string representing the amount of time worked today.
Display seconds resolution if SHOW-SECONDS is non-nil.  If RELATIVE is
non-nil, the amount returned will be relative to past time worked."
  (interactive)
  (let ((string (timeclock-seconds-to-string
		 (timeclock-workday-elapsed relative)
		 show-seconds)))
    (if (interactive-p)
	(message string)
      string)))

(defun timeclock-when-to-leave (&optional today-only)
  "Return a time value representing at when the workday ends today.
If TODAY-ONLY is non-nil, the value returned will be relative only to
the time worked today, and not to past time.  This argument only makes
a difference if `timeclock-relative' is non-nil."
  (timeclock-seconds-to-time
   (- (timeclock-time-to-seconds (current-time))
      (timeclock-find-discrep today-only))))

;;;###autoload
(defun timeclock-when-to-leave-string (&optional show-seconds
						 today-only)
  "Return a string representing at what time the workday ends today.
This string is relative to the value of `timeclock-workday'.  If
NO-MESSAGE is non-nil, no messages will be displayed in the
minibuffer.  If SHOW-SECONDS is non-nil, the value printed/returned
will include seconds.  If TODAY-ONLY is non-nil, the value returned
will be relative only to the time worked today, and not to past time.
This argument only makes a difference if `timeclock-relative' is
non-nil."
  (interactive)
  (let* ((then (timeclock-when-to-leave today-only))
	 (string
	  (if show-seconds
	      (format-time-string "%-I:%M:%S %p" then)
	    (format-time-string "%-I:%M %p" then))))
    (if (interactive-p)
	(message string)
      string)))

;;; Internal Functions:

(defvar timeclock-project-list nil)
(defvar timeclock-last-project nil)

(defun timeclock-ask-for-project ()
  "Ask the user for the project they are clocking into."
  (completing-read (format "Clock into which project (default \"%s\"): "
			   (or timeclock-last-project
			       (car timeclock-project-list)))
		   (mapcar 'list timeclock-project-list)
		   nil nil nil nil (or timeclock-last-project
				       (car timeclock-project-list))))

(defvar timeclock-reason-list nil)

(defun timeclock-ask-for-reason ()
  "Ask the user for the reason they are clocking out."
  (completing-read "Reason for clocking out: "
		   (mapcar 'list timeclock-reason-list)))

(defun timeclock-update-modeline ()
  "Update the `timeclock-mode-string' displayed in the modeline."
  (interactive)
  (let* ((remainder (timeclock-workday-remaining))
	 (last-in (equal (car timeclock-last-event) "i")))
    (when (and (< remainder 0)
	       (not (and timeclock-day-over
			 (equal timeclock-day-over
				(timeclock-time-to-date
				 (current-time))))))
      (setq timeclock-day-over
	    (timeclock-time-to-date (current-time)))
      (run-hooks 'timeclock-day-over-hook))
    (setq timeclock-mode-string
	  (format "   %c%s%c"
		  (if last-in ?< ?[)
		  (timeclock-seconds-to-string remainder nil t)
		  (if last-in ?> ?])))))

(defun timeclock-log (code &optional project)
  "Log the event CODE to the timeclock log, at the time of call.
If PROJECT is a string, it represents the project which the event is
being logged for.  Normally only \"out\" events specify a project."
  (save-excursion
    (set-buffer (find-file-noselect timeclock-file))
    (goto-char (point-max))
    (if (not (bolp))
	(insert "\n"))
    (let ((now (current-time)))
      (insert code " "
	      (format-time-string "%Y/%m/%d %H:%M:%S" now)
	      (or (and project
		       (stringp project)
		       (> (length project) 0)
		       (concat " " project))
		  "")
	      "\n")
      (if (equal (downcase code) "o")
	  (setq timeclock-last-period
		(- (timeclock-time-to-seconds now)
		   (timeclock-time-to-seconds
		    (cadr timeclock-last-event)))
		timeclock-discrepancy
		(+ timeclock-discrepancy
		   timeclock-last-period)))
      (setq timeclock-last-event (list code now project)))
    (save-buffer)
    (run-hooks 'timeclock-event-hook)))

(defun timeclock-read-moment ()
  "Read the moment under point from the timelog."
  (save-excursion
    (beginning-of-line)
    (let ((eol (save-excursion (end-of-line) (point))))
      (if (re-search-forward
	   (concat "^\\(.\\)\\s-+"
		   "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)\\s-+"
		   "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\s-*"
		   "\\(.*\\)") eol t)
	  (let ((code (match-string 1))
		(year (string-to-number (match-string 2)))
		(mon  (string-to-number (match-string 3)))
		(mday (string-to-number (match-string 4)))
		(hour (string-to-number (match-string 5)))
		(min  (string-to-number (match-string 6)))
		(sec  (string-to-number (match-string 7)))
		(project (match-string 8)))
	    (list code (encode-time sec min hour mday mon year)
		  project))))))

(defun timeclock-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defun timeclock-seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to an Emacs time structure."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun timeclock-time-to-date (time)
  "Convert the TIME value to a textual date string."
  (format-time-string "%Y/%m/%d" time))

(defun timeclock-last-period (&optional moment)
  "Return the value of the last event period.
If the last event was a clock-in, the period will be open ended, and
growing every second.  Otherwise, it is a fixed amount which has been
recorded to disk.  If MOMENT is non-nil, use that as the current time.
This is only provided for coherency when used by
`timeclock-discrepancy'."
  (if (equal (car timeclock-last-event) "i")
      (- (timeclock-time-to-seconds (or moment (current-time)))
	 (timeclock-time-to-seconds
	  (cadr timeclock-last-event)))
    timeclock-last-period))

(defun timeclock-find-discrep (&optional today-only)
  "Find overall discrepancy from `timeclock-workday' (in seconds).
If TODAY-ONLY is non-nil, the discrepancy will be not be relative, and
will correspond only to the amount of time elapsed today.  This is
identical to what would be return if `timeclock-relative' were nil."
  (let* ((now (current-time)) (first t)
	 (todays-date (timeclock-time-to-date now))
	 accum event beg last-date
	 last-date-limited last-date-seconds avg)
    (unless timeclock-discrepancy
      (setq timeclock-project-list nil
	    timeclock-last-project nil
	    timeclock-reason-list nil)
      (save-excursion
	(set-buffer (find-file-noselect timeclock-file))
	(goto-char (point-min))
	(setq accum 0)
	(setq timeclock-elapsed 0)
	(while (setq event (timeclock-read-moment))
	  (cond ((equal (car event) "h")
		 (setq last-date-limited
		       (timeclock-time-to-date (cadr event))
		       last-date-seconds
		       (* (string-to-number (nth 2 event)) 3600)))
		((equal (car event) "i")
		 (when (and (nth 2 event)
			    (> (length (nth 2 event)) 0))
		   (add-to-list 'timeclock-project-list (nth 2 event))
		   (setq timeclock-last-project (nth 2 event)))
		 (let ((date (timeclock-time-to-date (cadr event))))
		   (if (and last-date
			    timeclock-relative
			    (not (equal date last-date)))
		       (setq accum (- accum
				      (if last-date-limited
					  last-date-seconds
					timeclock-workday)))
		     (unless (or last-date (not first))
		       (setq first nil
			     accum (- accum
				      (if last-date-limited
					  last-date-seconds
					timeclock-workday)))))
		   (setq last-date date
			 last-date-limited nil)
		   (if beg
		       (error "Error in format of timelog file!")
		     (setq beg (timeclock-time-to-seconds (cadr event))))))
		((equal (downcase (car event)) "o")
		 (if (and (nth 2 event)
			  (> (length (nth 2 event)) 0))
		     (add-to-list 'timeclock-reason-list (nth 2 event)))
		 (if (or timeclock-relative
			 (equal last-date todays-date))
		     (if (not beg)
			 (error "Error in format of timelog file!")
		       (setq timeclock-last-period
			     (- (timeclock-time-to-seconds (cadr event))
				beg)
			     accum (+ timeclock-last-period accum)
			     beg nil)))
		 (if (equal last-date todays-date)
		     (setq timeclock-elapsed
			   (+ timeclock-last-period timeclock-elapsed)))))
	  (setq timeclock-last-event event
		timeclock-last-event-workday
		(if (equal (timeclock-time-to-date now)
			   last-date-limited)
		    last-date-seconds
		  timeclock-workday))
	  (forward-line))
	(setq timeclock-discrepancy accum)))
    (setq accum (if today-only
		    timeclock-elapsed
		  timeclock-discrepancy))
    (if timeclock-last-event
	(if (equal (car timeclock-last-event) "i")
	    (setq accum (+ accum (timeclock-last-period now)))
	  (if (not (equal (timeclock-time-to-date
			   (cadr timeclock-last-event))
			  (timeclock-time-to-date now)))
	      (setq accum (- accum timeclock-last-event-workday)))))
    (setq accum
	  (- accum
	     (if (and timeclock-last-event
		      (equal (timeclock-time-to-date
			      (cadr timeclock-last-event))
			     (timeclock-time-to-date now)))
		 timeclock-last-event-workday
	       timeclock-workday)))))

(provide 'timeclock)

(run-hooks 'timeclock-load-hook)

;; make sure we know the list of reasons, projects, and have computed
;; the last event and current discrepancy.
(if (file-readable-p timeclock-file)
    (timeclock-reread-log))

;;; timeclock.el ends here
