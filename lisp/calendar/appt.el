;;; appt.el --- appointment notification functions

;; Copyright (C) 1989, 1990, 1994, 1998 Free Software Foundation, Inc.

;; Author: Neil Mager <neilm@juliet.ll.mit.edu>
;; Maintainer: FSF
;; Keywords: calendar

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

;;
;; appt.el - visible and/or audible notification of
;;           appointments from ~/diary file.
;;
;;;
;;; Thanks to  Edward M. Reingold for much help and many suggestions,
;;; And to many others for bug fixes and suggestions.
;;;
;;;
;;; This functions in this file will alert the user of a 
;;; pending appointment based on their diary file.
;;;
;;; A message will be displayed in the mode line of the Emacs buffer
;;; and (if you request) the terminal will beep and display a message
;;; from the diary in the mini-buffer, or you can choose to 
;;; have a message displayed in a new buffer.
;;;
;;; The variable `appt-message-warning-time' allows the
;;; user to specify how much notice they want before the appointment. The 
;;; variable `appt-issue-message' specifies whether the user wants
;;; to to be notified of a pending appointment.
;;; 
;;; In order to use the appt package, you only need
;;; to load it---provided you have appointments.
;;;
;;; Before that, you can also set some options if you want
;;;   (setq view-diary-entries-initially t)
;;;   (setq appt-issue-message t)
;;; 
;;;  This is an example of what can be in your diary file:
;;; Monday
;;;   9:30am Coffee break
;;;  12:00pm Lunch        
;;; 
;;; Based upon the above lines in your .emacs and diary files, 
;;; the calendar and diary will be displayed when you enter
;;; Emacs and your appointments list will automatically be created.
;;; You will then be reminded at 9:20am about your coffee break
;;; and at 11:50am to go to lunch. 
;;;
;;; Use describe-function on appt-check for a description of other variables
;;; that can be used to personalize the notification system.
;;;
;;;  In order to add or delete items from todays list, use appt-add
;;;  and appt-delete.
;;;
;;;  Additionally, the appointments list is recreated automatically
;;;  at 12:01am for those who do not logout every day or are programming
;;;  late.
;;;
;;; Brief internal description - Skip this if you are not interested!
;;;
;;; The function appt-make-list creates the appointments list which appt-check
;;; reads. This is all done automatically.
;;; It is invoked from the function list-diary-entries.
;;;
;;; You can change the way the appointment window is created/deleted by
;;; setting  the variables
;;;
;;;	     appt-disp-window-function
;;; and
;;; 	     appt-delete-window-function
;;;
;;; For instance, these variables can be set to functions that display
;;; appointments in pop-up frames, which are lowered or iconified after
;;; appt-display-interval minutes.
;;;

;;; Code:

;; Make sure calendar is loaded when we compile this.
(require 'calendar)

(provide 'appt)

;;;###autoload
(defcustom appt-issue-message t
  "*Non-nil means check for appointments in the diary buffer.
To be detected, the diary entry must have the time
as the first thing on a line."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-message-warning-time 12
  "*Time in minutes before an appointment that the warning begins."
  :type 'integer
  :group 'appt)

;;;###autoload
(defcustom appt-audible t
  "*Non-nil means beep to indicate appointment."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-visible t
  "*Non-nil means display appointment message in echo area."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-display-mode-line t
  "*Non-nil means display minutes to appointment and time on the mode line."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-msg-window t
  "*Non-nil means display appointment message in another window."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-display-duration 10
  "*The number of seconds an appointment message is displayed."
  :type 'integer
  :group 'appt)

;;;###autoload
(defcustom appt-display-diary t
  "*Non-nil means to display the next days diary on the screen. 
This will occur at midnight when the appointment list is updated."
  :type 'boolean
  :group 'appt)

(defvar appt-time-msg-list nil
  "The list of appointments for today.
Use `appt-add' and `appt-delete' to add and delete appointments from list.
The original list is generated from the today's `diary-entries-list'.
The number before each time/message is the time in minutes from midnight.")

(defconst appt-max-time 1439
  "11:59pm in minutes - number of minutes in a day minus 1.")

(defcustom appt-display-interval 3
  "*Number of minutes to wait between checking the appointment list."
  :type 'integer
  :group 'appt)
  
(defvar appt-buffer-name " *appt-buf*"
  "Name of the appointments buffer.")
  
(defvar appt-disp-window-function 'appt-disp-window
  "Function called to display appointment window.")
  
(defvar appt-delete-window-function 'appt-delete-window
  "Function called to remove appointment window and buffer.")

(defvar appt-mode-string nil
  "String being displayed in the mode line saying you have an appointment.
The actual string includes the amount of time till the appointment.")

(defvar appt-prev-comp-time nil
  "Time of day (mins since midnight) at which we last checked appointments.")

(defvar appt-now-displayed nil
  "Non-nil when we have started notifying about a appointment that is near.")

(defvar appt-display-count nil)

(defun appt-check ()
  "Check for an appointment and update the mode line.
Note: the time must be the first thing in the line in the diary
for a warning to be issued.

The format of the time can be either 24 hour or am/pm.
Example: 

               02/23/89
                 18:00 Dinner
            
              Thursday
                11:45am Lunch meeting.

Appointments are checked every `appt-display-interval' minutes.
The following variables control appointment notification:

`appt-issue-message'
	If t, the diary buffer is checked for appointments.

`appt-message-warning-time'
	Variable used to determine if appointment message
	should be displayed.

`appt-audible'
	Variable used to determine if appointment is audible.
	Default is t.

`appt-visible'
	Variable used to determine if appointment message should be
	displayed in the mini-buffer.  Default is t.

`appt-msg-window'
	Variable used to determine if appointment message
	should temporarily appear in another window.  Mutually exclusive
	to `appt-visible'.

`appt-display-duration'
	The number of seconds an appointment message
	is displayed in another window.

`appt-disp-window-function'
    	Function called to display appointment window.  You can customize
	appt.el by setting this variable to a function different from the
	one provided with this package.
  
`appt-delete-window-function'
    	Function called to remove appointment window and buffer.  You can
	customize appt.el by setting this variable to a function different
	from the one provided with this package."

  (let* ((min-to-app -1)
	 (new-time "")
	 (prev-appt-mode-string appt-mode-string)
	 (prev-appt-display-count (or appt-display-count 0))
	 ;; Non-nil means do a full check for pending appointments
	 ;; and display in whatever ways the user has selected.
	 ;; When no appointment is being displayed,
	 ;; we always do a full check.
	 (full-check
	  (or (not appt-now-displayed)
	      ;; This is true every appt-display-interval minutes.
	      (= 0 (mod prev-appt-display-count appt-display-interval))))
	 ;; Non-nil means only update the interval displayed in the mode line.
	 (mode-line-only
	  (and (not full-check) appt-now-displayed)))

    (when (or full-check mode-line-only)
      (save-excursion

	;; Get the current time and convert it to minutes
	;; from midnight. ie. 12:01am = 1, midnight = 0.

	(let* ((now (decode-time))
	       (cur-hour (nth 2 now))
	       (cur-min (nth 1 now))
	       (cur-comp-time (+ (* cur-hour 60) cur-min)))

	  ;; At the first check in any given day, update our 
	  ;; appointments to today's list.

	  (if (or (null appt-prev-comp-time)
		  (< cur-comp-time appt-prev-comp-time))
	      (condition-case nil
		  (progn
		    (if (and view-diary-entries-initially appt-display-diary)
			(diary)
		      (let ((diary-display-hook 'appt-make-list))
			(diary))))
		(error nil)))
	  (setq appt-prev-comp-time cur-comp-time)

	  (setq appt-mode-string nil)
	  (setq appt-display-count nil)

	  ;; If there are entries in the list, and the
	  ;; user wants a message issued,
	  ;; get the first time off of the list
	  ;; and calculate the number of minutes until the appointment.

	  (if (and appt-issue-message appt-time-msg-list)
	      (let ((appt-comp-time (car (car (car appt-time-msg-list)))))
		(setq min-to-app (- appt-comp-time cur-comp-time))

		(while (and appt-time-msg-list 
			    (< appt-comp-time cur-comp-time))
		  (setq appt-time-msg-list (cdr appt-time-msg-list)) 
		  (if appt-time-msg-list
		      (setq appt-comp-time 
			    (car (car (car appt-time-msg-list))))))

		;; If we have an appointment between midnight and
		;; 'appt-message-warning-time' minutes after midnight,
		;; we must begin to issue a message before midnight.
		;; Midnight is considered 0 minutes and 11:59pm is
		;; 1439 minutes. Therefore we must recalculate the minutes
		;; to appointment variable. It is equal to the number of 
		;; minutes before midnight plus the number of 
		;; minutes after midnight our appointment is.

		(if (and (< appt-comp-time appt-message-warning-time)
			 (> (+ cur-comp-time appt-message-warning-time)
			    appt-max-time))
		    (setq min-to-app (+ (- (1+ appt-max-time) cur-comp-time))
			  appt-comp-time))

		;; issue warning if the appointment time is 
		;; within appt-message-warning time

		(when (and (<= min-to-app appt-message-warning-time)
			   (>= min-to-app 0))
		  (setq appt-now-displayed t)
		  (setq appt-display-count
			(1+ prev-appt-display-count))
		  (unless mode-line-only
		    (if appt-msg-window
			(progn
			  (setq new-time (format-time-string "%a %b %e "
							     (current-time)))
			  (funcall
			   appt-disp-window-function
			   (number-to-string min-to-app) new-time
			   (car (cdr (car appt-time-msg-list))))

			  (run-at-time
			   (format "%d sec" appt-display-duration)
			   nil
			   appt-delete-window-function))
			      ;;; else

		      (if appt-visible
			  (message "%s" 
				   (car (cdr (car appt-time-msg-list)))))

		      (if appt-audible
			  (beep 1))))

		  (when appt-display-mode-line
		    (setq appt-mode-string
			  (concat  " App't in "
				   (number-to-string min-to-app)
				   " min. ")))

		  ;; When an appointment is reached,
		  ;; delete it from the list.
		  ;; Reset the count to 0 in case we display another
		  ;; appointment on the next cycle.
		  (if (= min-to-app 0)
		      (setq appt-time-msg-list
			    (cdr appt-time-msg-list)
			    appt-display-count nil)))))

	  ;; If we have changed the mode line string,
	  ;; redisplay all mode lines.
	  (and appt-display-mode-line
	       (not (equal appt-mode-string
			   prev-appt-mode-string))
	       (progn
		 (force-mode-line-update t)
		 ;; If the string now has a notification,
		 ;; redisplay right now.
		 (if appt-mode-string
		     (sit-for 0)))))))))


(defun appt-disp-window (min-to-app new-time appt-msg)
  "Display appointment message APPT-MSG in a separate buffer."
  (require 'electric)

  ;; Make sure we're not in the minibuffer
  ;; before splitting the window.

  (if (equal (selected-window) (minibuffer-window))
      (if (other-window 1) 
	  (select-window (other-window 1))
	(if (display-multi-frame-p)
	    (select-frame (other-frame 1)))))
      
  (let* ((this-buffer (current-buffer))
	 (this-window (selected-window))
	 (appt-disp-buf (set-buffer (get-buffer-create appt-buffer-name))))

    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer appt-disp-buf)
      (unless (or (special-display-p (buffer-name appt-disp-buf))
		  (same-window-p (buffer-name appt-disp-buf)))
	;; By default, split the bottom window and use the lower part.
	(appt-select-lowest-window)
	(split-window))
      (pop-to-buffer appt-disp-buf))
    (setq mode-line-format 
	  (concat "-------------------- Appointment in "
		  min-to-app " minutes. " new-time " %-"))
    (erase-buffer)
    (insert appt-msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    (raise-frame (selected-frame))
    (select-window this-window)
    (if appt-audible
	(beep 1))))
      
(defun appt-delete-window ()
  "Function called to undisplay appointment messages.
Usually just deletes the appointment buffer."
  (let ((window (get-buffer-window appt-buffer-name t)))
    (and window
	 (or (eq window (frame-root-window (window-frame window)))
	     (delete-window window))))
  (kill-buffer appt-buffer-name)
  (if appt-audible
      (beep 1)))

(defun appt-select-lowest-window ()
"Select the lowest window on the frame."
  (let ((lowest-window (selected-window))
	(bottom-edge (nth 3 (window-edges))))
    (walk-windows (lambda (w)
		    (let ((next-bottom-edge (nth 3 (window-edges w))))
		      (when (< bottom-edge next-bottom-edge)
			(setq bottom-edge next-bottom-edge
			      lowest-window w)))))
    (select-window lowest-window)))

;;;###autoload
(defun appt-add (new-appt-time new-appt-msg)
  "Add an appointment for the day at NEW-APPT-TIME and issue message NEW-APPT-MSG.
The time should be in either 24 hour format or am/pm format."

  (interactive "sTime (hh:mm[am/pm]): \nsMessage: ")
  (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?" new-appt-time)
      nil
    (error "Unacceptable time-string"))
  
  (let* ((appt-time-string (concat new-appt-time " " new-appt-msg))
         (appt-time (list (appt-convert-time new-appt-time)))
         (time-msg (cons appt-time (list appt-time-string))))
    (setq appt-time-msg-list (nconc appt-time-msg-list (list time-msg)))
    (setq appt-time-msg-list (appt-sort-list appt-time-msg-list)))) 

;;;###autoload
(defun appt-delete ()
  "Delete an appointment from the list of appointments."
  (interactive)
  (let* ((tmp-msg-list appt-time-msg-list))
    (while tmp-msg-list
      (let* ((element (car tmp-msg-list))
             (prompt-string (concat "Delete " 
                                    (prin1-to-string (car (cdr element))) 
                                    " from list? "))
             (test-input (y-or-n-p prompt-string)))
        (setq tmp-msg-list (cdr tmp-msg-list))
        (if test-input
            (setq appt-time-msg-list (delq element appt-time-msg-list)))))
    (appt-check)
    (message "")))
                 

(eval-when-compile (defvar number)
		   (defvar original-date)
		   (defvar diary-entries-list))
;;;###autoload
(defun appt-make-list ()
  "Create the appointments list from todays diary buffer.
The time must be at the beginning of a line for it to be
put in the appointments list.
  02/23/89
    12:00pm lunch
   Wednesday
     10:00am group meeting
We assume that the variables DATE and NUMBER
hold the arguments that `list-diary-entries' received.
They specify the range of dates that the diary is being processed for."

  ;; We have something to do if the range of dates that the diary is
  ;; considering includes the current date.
  (if (and (not (calendar-date-compare
		 (list (calendar-current-date))
		 (list original-date)))
	   (calendar-date-compare
	    (list (calendar-current-date))
            (list (calendar-gregorian-from-absolute
		   (+ (calendar-absolute-from-gregorian original-date)
		      number)))))
      (save-excursion
	;; Clear the appointments list, then fill it in from the diary.
	(setq appt-time-msg-list nil)
	(if diary-entries-list

	    ;; Cycle through the entry-list (diary-entries-list)
	    ;; looking for entries beginning with a time. If 
	    ;; the entry begins with a time, add it to the
	    ;; appt-time-msg-list. Then sort the list.

	    (let ((entry-list diary-entries-list)
		  (new-time-string ""))
	      ;; Skip diary entries for dates before today.
	      (while (and entry-list
			  (calendar-date-compare
			   (car entry-list) (list (calendar-current-date))))
		(setq entry-list (cdr entry-list)))
	      ;; Parse the entries for today.
	      (while (and entry-list 
			  (calendar-date-equal 
			   (calendar-current-date) (car (car entry-list))))
		(let ((time-string (substring (prin1-to-string 
					       (cadr (car entry-list))) 1 -1)))

		  (while (string-match
			  "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?\\(.*\n\\)*.*"
			  time-string)
		    (let* ((appt-time-string (match-string 0 time-string)))

		      (if (< (match-end 0) (length time-string))
			  (setq new-time-string (substring time-string 
							   (+ (match-end 0) 1)
							   nil))
			(setq new-time-string ""))

		      (string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?"
				    time-string)

		      (let* ((appt-time (list (appt-convert-time 
					       (match-string 0 time-string))))
			     (time-msg (cons appt-time
					     (list appt-time-string))))
			(setq time-string new-time-string)
			(setq appt-time-msg-list (nconc appt-time-msg-list
							(list time-msg)))))))
		(setq entry-list (cdr entry-list)))))
	(setq appt-time-msg-list (appt-sort-list appt-time-msg-list))

	;; Get the current time and convert it to minutes
	;; from midnight. ie. 12:01am = 1, midnight = 0,
	;; so that the elements in the list
	;; that are earlier than the present time can
	;; be removed.

	(let* ((now (decode-time))
	       (cur-hour (nth 2 now))
	       (cur-min (nth 1 now))
	       (cur-comp-time (+ (* cur-hour 60) cur-min))
	       (appt-comp-time (car (car (car appt-time-msg-list)))))

	  (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
	    (setq appt-time-msg-list (cdr appt-time-msg-list)) 
	    (if appt-time-msg-list
		(setq appt-comp-time (car (car (car appt-time-msg-list))))))))))
  

(defun appt-sort-list (appt-list)
  "Simple sort to put the appointments list APPT-LIST in order.
Scan the list for the smallest element left in the list.
Append the smallest element left into the new list, and remove
it from the original list."
  (let ((order-list nil))
    (while appt-list
      (let* ((element (car appt-list))
             (element-time (car (car element)))
             (tmp-list (cdr appt-list)))
        (while tmp-list
          (if (< element-time (car (car (car tmp-list))))
              nil
            (setq element (car tmp-list))
            (setq element-time (car (car element))))
          (setq tmp-list (cdr tmp-list)))
        (setq order-list (nconc order-list (list element)))
        (setq appt-list (delq element appt-list))))
    order-list))


(defun appt-convert-time (time2conv)
  "Convert hour:min[am/pm] format to minutes from midnight."

  (let ((conv-time 0)
        (hr 0)
        (min 0))

    (string-match ":\\([0-9][0-9]\\)" time2conv)
    (setq min (string-to-int 
               (match-string 1 time2conv)))
  
    (string-match "[0-9]?[0-9]:" time2conv)
    (setq hr (string-to-int 
              (match-string 0 time2conv)))
  
    ;; convert the time appointment time into 24 hour time
  
    (cond ((and (string-match "pm" time2conv) (< hr 12))
	   (setq hr (+ 12 hr)))
	  ((and (string-match "am" time2conv) (= hr 12))
           (setq hr 0)))
  
    ;; convert the actual time
    ;; into minutes for comparison
    ;; against the actual time.
  
    (setq conv-time (+ (* hr 60) min))
    conv-time))

(defvar appt-timer nil
  "Timer used for diary appointment notifications (`appt-check').")

(unless appt-timer
  (setq appt-timer (run-at-time t 60 'appt-check)))

(or global-mode-string (setq global-mode-string '("")))
(or (memq 'appt-mode-string global-mode-string)
    (setq global-mode-string
	  (append global-mode-string '(appt-mode-string))))

;;; appt.el ends here
