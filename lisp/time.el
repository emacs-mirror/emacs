;;; time.el --- display time and load in mode line of Emacs.

;; Copyright (C) 1985, 86, 87, 93, 94, 96, 2000 Free Software Foundation, Inc.

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

;; Facilities to display current time/date and a new-mail indicator
;; in the Emacs mode line.  The single entry point is `display-time'.

;;; Code:

(defgroup display-time nil
  "Display time and load in mode line of Emacs."
  :group 'modeline
  :group 'mail)


;;;###autoload
(defcustom display-time-mode nil
  "Toggle display of time, load level, and mail flag in mode lines.
Setting this variable directly does not take effect;
use either \\[customize] or the function `display-time-mode'."
  :set (lambda (symbol value)
	 (display-time-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'display-time
  :require 'time
  :version "20.3")


(defcustom display-time-mail-file nil
  "*File name of mail inbox file, for indicating existence of new mail.
Non-nil and not a string means don't check for mail.  nil means use
default, which is system-dependent, and is the same as used by Rmail."
  :type '(choice (const :tag "Default" nil)
		 (file :format "%v"))
  :group 'display-time)

;;;###autoload
(defcustom display-time-day-and-date nil "\
*Non-nil means \\[display-time] should display day and date as well as time."
  :type 'boolean
  :group 'display-time)

(defvar display-time-timer nil)

(defcustom display-time-interval 60
  "*Seconds between updates of time in the mode line."
  :type 'integer
  :group 'display-time)

(defcustom display-time-24hr-format nil
  "*Non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23.
Nil means 1 <= hh <= 12, and an AM/PM suffix is used."
  :type 'boolean
  :group 'display-time)

(defvar display-time-string nil)

(defcustom display-time-hook nil
  "*List of functions to be called when the time is updated on the mode line."
  :type 'hook
  :group 'display-time)

(defvar display-time-server-down-time nil
   "Time when mail file's file system was recorded to be down.
If that file system seems to be up, the value is nil.")

;;;###autoload
(defun display-time ()
  "Enable display of time, load level, and mail flag in mode lines.
This display updates automatically every minute.
If `display-time-day-and-date' is non-nil, the current day and date
are displayed as well.
This runs the normal hook `display-time-hook' after each update."
  (interactive)
  (display-time-mode 1))

;;;###autoload
(defun display-time-mode (arg)
  "Toggle display of time, load level, and mail flag in mode lines.
With a numeric arg, enable this display if arg is positive.

When this display is enabled, it updates automatically every minute.
If `display-time-day-and-date' is non-nil, the current day and date
are displayed as well.
This runs the normal hook `display-time-hook' after each update."
  (interactive "P")
  (let ((on (if (null arg)
		(not display-time-timer)
	      (> (prefix-numeric-value arg) 0))))
    (setq display-time-mode on)
    (and display-time-timer (cancel-timer display-time-timer))
    (setq display-time-timer nil)
    (setq display-time-string "")
    (or global-mode-string (setq global-mode-string '("")))
    (if on
	(progn
	  (or (memq 'display-time-string global-mode-string)
	      (setq global-mode-string
		    (append global-mode-string '(display-time-string))))
	  ;; Set up the time timer.
	  (setq display-time-timer
		(run-at-time t display-time-interval
			     'display-time-event-handler))
	  ;; Make the time appear right away.
	  (display-time-update)
	  ;; When you get new mail, clear "Mail" from the mode line.
	  (add-hook 'rmail-after-get-new-mail-hook
		    'display-time-event-handler))
      (remove-hook 'rmail-after-get-new-mail-hook
		   'display-time-event-handler))))


(defcustom display-time-format nil
  "*A string specifying the format for displaying the time in the mode line.
See the function `format-time-string' for an explanation of
how to write this string.  If this is nil, the defaults
depend on `display-time-day-and-date' and `display-time-24hr-format'."
  :type '(choice (const :tag "Default" nil)
		 string)
  :group 'display-time)

(defcustom display-time-string-forms
  `((if (and (not display-time-format) display-time-day-and-date)
	(format-time-string "%a %b %e " now)
      "")
    (format-time-string (or display-time-format
			    (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
			now)
    load
    (if mail ,(propertize " Mail"
			  'help-echo "mouse-2: Read mail"
			  'local-map (make-mode-line-mouse2-map
				      (lambda (e)
					(interactive "e")
					(funcall read-mail-command))))
      ""))
  "*A list of expressions governing display of the time in the mode line.
For most purposes, you can control the time format using `display-time-format'
which is a more standard interface.

This expression is a list of expressions that can involve the keywords
`load', `day', `month', and `year', `12-hours', `24-hours', `minutes',
`seconds', all numbers in string form, and `monthname', `dayname', `am-pm',
and `time-zone' all alphabetic strings, and `mail' a true/nil value.

For example, the form

  '((substring year -2) \"/\" month \"/\" day
    \" \" 24-hours \":\" minutes \":\" seconds
    (if time-zone \" (\") time-zone (if time-zone \")\")
    (if mail \" Mail\" \"\"))

would give mode line times like `94/12/30 21:07:48 (UTC)'."
  :type 'sexp
  :group 'display-time)

(defun display-time-event-handler ()
  (display-time-update)
  ;; Do redisplay right now, if no input pending.
  (sit-for 0)
  (let* ((current (current-time))
	 (timer display-time-timer)
	 ;; Compute the time when this timer will run again, next.
	 (next-time (timer-relative-time
		     (list (aref timer 1) (aref timer 2) (aref timer 3))
		     (* 5 (aref timer 4)) 0)))
    ;; If the activation time is far in the past,
    ;; skip executions until we reach a time in the future.
    ;; This avoids a long pause if Emacs has been suspended for hours.
    (or (> (nth 0 next-time) (nth 0 current))
	(and (= (nth 0 next-time) (nth 0 current))
	     (> (nth 1 next-time) (nth 1 current)))
	(and (= (nth 0 next-time) (nth 0 current))
	     (= (nth 1 next-time) (nth 1 current))
	     (> (nth 2 next-time) (nth 2 current)))
	(progn
	  (timer-set-time timer (timer-next-integral-multiple-of-time
				 current display-time-interval)
			  display-time-interval)
	  (timer-activate timer)))))

;; Update the display-time info for the mode line
;; but don't redisplay right now.  This is used for
;; things like Rmail `g' that want to force an update
;; which can wait for the next redisplay.
(defun display-time-update ()
  (let* ((now (current-time))
	 (time (current-time-string now))
         (load (condition-case ()
                   (if (zerop (car (load-average))) ""
		     ;; The load average number is myterious, so
		     ;; propvide some help.
                     (let ((str (format " %03d" (car (load-average)))))
		       (propertize
			(concat (substring str 0 -2) "." (substring str -2))
			'help-echo "System load average")))
                 (error "")))
         (mail-spool-file (or display-time-mail-file
                              (getenv "MAIL")
                              (concat rmail-spool-directory
                                      (user-login-name))))
	 (mail (and (stringp mail-spool-file)
		    (or (null display-time-server-down-time)
			;; If have been down for 20 min, try again.
			(> (- (nth 1 now) display-time-server-down-time)
			   1200)
			(and (< (nth 1 now) display-time-server-down-time)
			     (> (- (nth 1 now) display-time-server-down-time)
				-64336)))
		    (let ((start-time (current-time)))
		      (prog1
			  (display-time-file-nonempty-p mail-spool-file)
			(if (> (- (nth 1 (current-time)) (nth 1 start-time))
			       20)
			    ;; Record that mail file is not accessible.
			    (setq display-time-server-down-time
				  (nth 1 (current-time)))
			  ;; Record that mail file is accessible.
			  (setq display-time-server-down-time nil))))))
         (24-hours (substring time 11 13))
         (hour (string-to-int 24-hours))
         (12-hours (int-to-string (1+ (% (+ hour 11) 12))))
         (am-pm (if (>= hour 12) "pm" "am"))
         (minutes (substring time 14 16))
         (seconds (substring time 17 19))
         (time-zone (car (cdr (current-time-zone now))))
         (day (substring time 8 10))
         (year (substring time 20 24))
         (monthname (substring time 4 7))
         (month
          (cdr
           (assoc
            monthname
            '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4")
              ("May" . "5") ("Jun" . "6") ("Jul" . "7") ("Aug" . "8")
              ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
         (dayname (substring time 0 3)))
    (setq display-time-string
          (mapconcat 'eval display-time-string-forms ""))
    ;; This is inside the let binding, but we are not going to document
    ;; what variables are available.
    (run-hooks 'display-time-hook))
  (force-mode-line-update))

(defun display-time-file-nonempty-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes (file-chase-links file))))))

(if display-time-mode
    (display-time-mode t))

(provide 'time)

;;; time.el ends here
