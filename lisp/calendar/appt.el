;;; appt.el --- appointment notification functions

;; Copyright (C) 1989, 1990, 1994, 1998, 2004  Free Software Foundation, Inc.

;; Author: Neil Mager <neilm@juliet.ll.mit.edu>
;; Maintainer: Glenn Morris <gmorris@ast.cam.ac.uk>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; appt.el - visible and/or audible notification of
;;           appointments from diary file.
;;
;;;
;;; Thanks to  Edward M. Reingold for much help and many suggestions,
;;; And to many others for bug fixes and suggestions.
;;;
;;;
;;; This functions in this file will alert the user of a
;;; pending appointment based on his/her diary file.  This package
;;; is documented in the Emacs manual.
;;;
;;; To activate this package, simply use (appt-activate 1).
;;; A `diary-file' with appointments of the format described in the
;;; documentation of the function `appt-check' is required.
;;; Relevant customizable variables are also listed in the
;;; documentation of that function.
;;;
;;; Today's appointment list is initialized from the diary when this
;;; package is activated. Additionally, the appointments list is
;;; recreated automatically at 12:01am for those who do not logout
;;; every day or are programming late. It is also updated when the
;;; `diary-file' is saved. Calling `appt-check' with an argument forces
;;; a re-initialization at any time.
;;;
;;; In order to add or delete items from today's list, without
;;; changing the diary file, use `appt-add' and `appt-delete'.
;;;

;;; Brief internal description - Skip this if you are not interested!
;;;
;;; The function `appt-make-list' creates the appointments list which
;;; `appt-check' reads.
;;;
;;; You can change the way the appointment window is created/deleted by
;;; setting the variables
;;;
;;;	     appt-disp-window-function
;;; and
;;; 	     appt-delete-window-function
;;;
;;; For instance, these variables could be set to functions that display
;;; appointments in pop-up frames, which are lowered or iconified after
;;; `appt-display-interval' minutes.
;;;

;;; Code:

;; Make sure calendar is loaded when we compile this.
(require 'calendar)


;;;###autoload
(defcustom appt-issue-message t
  "*Non-nil means check for appointments in the diary buffer.
To be detected, the diary entry must have the format described in the
documentation of the function `appt-check'."
  :type 'boolean
  :group 'appt)

(make-obsolete-variable 'appt-issue-message
                        "use the function `appt-activate', and the \
variable `appt-display-format' instead." "22.1")

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
  "*Non-nil means display appointment message in echo area.
This variable is only relevant if `appt-msg-window' is nil."
  :type 'boolean
  :group 'appt)

(make-obsolete-variable 'appt-visible 'appt-display-format "22.1")

;;;###autoload
(defcustom appt-msg-window t
  "*Non-nil means display appointment message in another window.
If non-nil, this variable overrides `appt-visible'."
  :type 'boolean
  :group 'appt)

(make-obsolete-variable 'appt-msg-window 'appt-display-format "22.1")

;; TODO - add popup.
(defcustom appt-display-format 'ignore
  "How appointment reminders should be displayed.
The options are:
   window - use a separate window
   echo   - use the echo area
   nil    - no visible reminder.
See also `appt-audible' and `appt-display-mode-line'.

The default value is 'ignore, which means to fall back on the value
of the (obsolete) variables `appt-msg-window' and `appt-visible'."
  :type '(choice
          (const :tag "Separate window" window)
          (const :tag "Echo-area" echo)
          (const :tag "No visible display" nil))
  :group 'appt
  :version "22.1")

;;;###autoload
(defcustom appt-display-mode-line t
  "*Non-nil means display minutes to appointment and time on the mode line.
This is in addition to any other display of appointment messages."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-display-duration 10
  "*The number of seconds an appointment message is displayed.
Only relevant if reminders are to be displayed in their own window."
  :type 'integer
  :group 'appt)

;;;###autoload
(defcustom appt-display-diary t
  "*Non-nil displays the diary when the appointment list is first initialized.
This will occur at midnight when the appointment list is updated."
  :type 'boolean
  :group 'appt)

(defcustom appt-display-interval 3
  "*Number of minutes to wait between checking the appointment list."
  :type 'integer
  :group 'appt)

(defcustom appt-disp-window-function 'appt-disp-window
  "Function called to display appointment window.
Only relevant if reminders are being displayed in a window."
  :type '(choice (const appt-disp-window)
                 function)
  :group 'appt)

(defcustom appt-delete-window-function 'appt-delete-window
  "Function called to remove appointment window and buffer.
Only relevant if reminders are being displayed in a window."
  :type '(choice (const appt-delete-window)
                 function)
  :group 'appt)


;;; Internal variables below this point.

(defconst appt-buffer-name " *appt-buf*"
  "Name of the appointments buffer.")

(defvar appt-time-msg-list nil
  "The list of appointments for today.
Use `appt-add' and `appt-delete' to add and delete appointments.
The original list is generated from today's `diary-entries-list', and
can be regenerated using the function `appt-check'.
Each element of the generated list has the form (MINUTES STRING [FLAG]); where
MINUTES is the time in minutes of the appointment after midnight, and
STRING is the description of the appointment.
FLAG, if non-nil, says that the element was made with `appt-add'
so calling `appt-make-list' again should preserve it.")

(defconst appt-max-time (1- (* 24 60))
  "11:59pm in minutes - number of minutes in a day minus 1.")

(defvar appt-mode-string nil
  "String being displayed in the mode line saying you have an appointment.
The actual string includes the amount of time till the appointment.
Only used if `appt-display-mode-line' is non-nil.")

(defvar appt-prev-comp-time nil
  "Time of day (mins since midnight) at which we last checked appointments.
A nil value forces the diary file to be (re-)checked for appointments.")

(defvar appt-now-displayed nil
  "Non-nil when we have started notifying about a appointment that is near.")

(defvar appt-display-count nil
  "Internal variable used to count number of consecutive reminders.")

(defvar appt-timer nil
  "Timer used for diary appointment notifications (`appt-check').
If this is non-nil, appointment checking is active.")


;;; Functions.

(defun appt-display-message (string mins)
  "Display a reminder about an appointment.
The string STRING describes the appointment, due in integer MINS minutes.
The format of the visible reminder is controlled by `appt-display-format'.
The variable `appt-audible' controls the audible reminder."
  ;; let binding for backwards compatability. Remove when obsolete
  ;; vars appt-msg-window and appt-visible are dropped.
  (let ((appt-display-format
         (if (eq appt-display-format 'ignore)
             (cond (appt-msg-window 'window)
                   (appt-visible 'echo))
           appt-display-format)))
    (cond ((eq appt-display-format 'window)
           (funcall appt-disp-window-function
                    (number-to-string mins)
                    (format-time-string "%a %b %e " (current-time))
                    string)
           (run-at-time (format "%d sec" appt-display-duration)
                        nil
                        appt-delete-window-function))
          ((eq appt-display-format 'echo)
           (message "%s" string)))
    (if appt-audible (beep 1))))


(defun appt-check (&optional force)
  "Check for an appointment and update any reminder display.
If optional argument FORCE is non-nil, reparse the diary file for
appointments.  Otherwise the diary file is only parsed once per day,
and when saved.

Note: the time must be the first thing in the line in the diary
for a warning to be issued.  The format of the time can be either
24 hour or am/pm.  For example:

              02/23/89
                18:00 Dinner

              Thursday
                11:45am Lunch meeting.

Appointments are checked every `appt-display-interval' minutes.
The following variables control appointment notification:

`appt-display-format'
        Controls the format in which reminders are displayed.

`appt-audible'
	Variable used to determine if reminder is audible.
	Default is t.

`appt-message-warning-time'
	Variable used to determine when appointment message
	should first be displayed.

`appt-display-mode-line'
        If non-nil, a generic message giving the time remaining
        is shown in the mode-line when an appointment is due.

`appt-display-interval'
        Interval in minutes at which to check for pending appointments.

`appt-display-diary'
        Display the diary buffer when the appointment list is
        initialized for the first time in a day.

The following variables are only relevant if reminders are being
displayed in a window:

`appt-display-duration'
	The number of seconds an appointment message is displayed.

`appt-disp-window-function'
    	Function called to display appointment window.

`appt-delete-window-function'
    	Function called to remove appointment window and buffer."

  (let* ((min-to-app -1)
	 (prev-appt-mode-string appt-mode-string)
	 (prev-appt-display-count (or appt-display-count 0))
	 ;; Non-nil means do a full check for pending appointments
	 ;; and display in whatever ways the user has selected.
	 ;; When no appointment is being displayed,
	 ;; we always do a full check.
	 (full-check
	  (or (not appt-now-displayed)
	      ;; This is true every appt-display-interval minutes.
	      (zerop (mod prev-appt-display-count appt-display-interval))))
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

	  (if (or force                 ; eg initialize, diary save
                  (null appt-prev-comp-time)             ; first check
		  (< cur-comp-time appt-prev-comp-time)) ; new day
	      (condition-case nil
                  (if appt-display-diary
                      (let ((diary-hook
                             (if (assoc 'appt-make-list diary-hook)
                                 diary-hook
                               (cons 'appt-make-list diary-hook))))
                        (diary))
                    (let ((diary-display-hook 'appt-make-list)
                          (d-buff (find-buffer-visiting
                                   (substitute-in-file-name diary-file)))
                          selective)
                      (if d-buff        ; diary buffer exists
                          (with-current-buffer d-buff
                            (setq selective selective-display)))
                      (diary)
                      ;; If the diary buffer existed before this command,
                      ;; restore its display state. Otherwise, kill it.
                      (if d-buff
                          ;; Displays the diary buffer.
                          (or selective (show-all-diary-entries))
                        (and
                         (setq d-buff (find-buffer-visiting
                                       (substitute-in-file-name diary-file)))
                         (kill-buffer d-buff)))))
		(error nil)))

	  (setq appt-prev-comp-time cur-comp-time
                appt-mode-string nil
                appt-display-count nil)

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
		  (setq appt-now-displayed t
                        appt-display-count (1+ prev-appt-display-count))
		  (unless mode-line-only
                    (appt-display-message (cadr (car appt-time-msg-list))
                                          min-to-app))
		  (when appt-display-mode-line
		    (setq appt-mode-string
                          (format " App't in %s min." min-to-app)))

		  ;; When an appointment is reached,
		  ;; delete it from the list.
		  ;; Reset the count to 0 in case we display another
		  ;; appointment on the next cycle.
		  (if (zerop min-to-app)
		      (setq appt-time-msg-list (cdr appt-time-msg-list)
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
  "Display appointment message APPT-MSG in a separate buffer.
The appointment is due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the date."
  (require 'electric)

  ;; Make sure we're not in the minibuffer
  ;; before splitting the window.

  (if (equal (selected-window) (minibuffer-window))
      (if (other-window 1)
	  (select-window (other-window 1))
	(if (display-multi-frame-p)
	    (select-frame (other-frame 1)))))

  (let ((this-window (selected-window))
        (appt-disp-buf (set-buffer (get-buffer-create appt-buffer-name))))

    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer appt-disp-buf)
      (unless (or (special-display-p (buffer-name appt-disp-buf))
		  (same-window-p (buffer-name appt-disp-buf)))
	;; By default, split the bottom window and use the lower part.
	(appt-select-lowest-window)
        (select-window (split-window)))
      (switch-to-buffer appt-disp-buf))
    (calendar-set-mode-line
     (format " Appointment in %s minutes. %s " min-to-app new-time))
    (erase-buffer)
    (insert appt-msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    (raise-frame (selected-frame))
    (select-window this-window)))

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

(defconst appt-time-regexp
  "[0-9]?[0-9]\\(h\\([0-9][0-9]\\)?\\|[:.][0-9][0-9]\\)\\(am\\|pm\\)?")

;;;###autoload
(defun appt-add (new-appt-time new-appt-msg)
  "Add an appointment for today at NEW-APPT-TIME with message NEW-APPT-MSG.
The time should be in either 24 hour format or am/pm format."
  (interactive "sTime (hh:mm[am/pm]): \nsMessage: ")
  (unless (string-match appt-time-regexp new-appt-time)
    (error "Unacceptable time-string"))
  (let* ((appt-time-string (concat new-appt-time " " new-appt-msg))
         (appt-time (list (appt-convert-time new-appt-time)))
         (time-msg (list appt-time appt-time-string t)))
    (setq appt-time-msg-list (nconc appt-time-msg-list (list time-msg)))
    (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))))

;;;###autoload
(defun appt-delete ()
  "Delete an appointment from the list of appointments."
  (interactive)
  (let ((tmp-msg-list appt-time-msg-list))
    (while tmp-msg-list
      (let* ((element (car tmp-msg-list))
             (prompt-string (concat "Delete "
				    ;; We want to quote any doublequotes
				    ;; in the string, as well as put
				    ;; doublequotes around it.
                                    (prin1-to-string
				     (substring-no-properties
				      (car (cdr element)) 0))
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
  "Update the appointments list from today's diary buffer.
The time must be at the beginning of a line for it to be
put in the appointments list (see examples in documentation of
the function `appt-check').  We assume that the variables DATE and
NUMBER hold the arguments that `list-diary-entries' received.
They specify the range of dates that the diary is being processed for.

Any appointments made with `appt-add' are not affected by this
function.

For backwards compatibility, this function activates the
appointment package (if it is not already active)."
  ;; See comments above appt-activate defun.
  (if (not appt-timer)
      (appt-activate 1)
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
          (dolist (elt appt-time-msg-list)
            ;; Delete any entries that were not made with appt-add.
            (unless (nth 2 elt)
              (setq appt-time-msg-list
                    (delq elt appt-time-msg-list))))
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
                  (let ((time-string (cadr (car entry-list))))
                    (while (string-match appt-time-regexp time-string)
                      (let* ((beg (match-beginning 0))
                             ;; Get just the time for this appointment.
                             (only-time (match-string 0 time-string))
                             ;; Find the end of this appointment
                             ;; (the start of the next).
                             (end (string-match
                                   (concat "\n[ \t]*" appt-time-regexp)
                                   time-string
                                   (match-end 0)))
                             ;; Get the whole string for this appointment.
                             (appt-time-string
                              (substring time-string beg (if end (1- end)))))

                        ;; Add this appointment to appt-time-msg-list.
                        (let* ((appt-time (list (appt-convert-time only-time)))
                               (time-msg (list appt-time appt-time-string)))
                          (setq appt-time-msg-list
                                (nconc appt-time-msg-list (list time-msg))))

                        ;; Discard this appointment from the string.
                        (setq time-string
                              (if end (substring time-string end) "")))))
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
                 (appt-comp-time (car (caar appt-time-msg-list))))

            (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
              (setq appt-time-msg-list (cdr appt-time-msg-list))
              (if appt-time-msg-list
                  (setq appt-comp-time (car (caar appt-time-msg-list))))))))))


(defun appt-sort-list (appt-list)
  "Sort an appointment list, putting earlier items at the front.
APPT-LIST is a list of the same format as `appt-time-msg-list'."
(sort appt-list (lambda (e1 e2) (< (caar e1) (caar e2)))))


(defun appt-convert-time (time2conv)
  "Convert hour:min[am/pm] format to minutes from midnight.
A period (.) can be used instead of a colon (:) to separate the
hour and minute parts."
  ;; Formats that should be accepted:
  ;;   10:00 10.00 10h00 10h 10am 10:00am 10.00am
  (let ((min (if (string-match "[h:.]\\([0-9][0-9]\\)" time2conv)
                 (string-to-number (match-string 1 time2conv))
               0))
        (hr (if (string-match "[0-9]*[0-9]" time2conv)
                (string-to-number (match-string 0 time2conv))
              0)))

    ;; convert the time appointment time into 24 hour time
    (cond ((and (string-match "pm" time2conv) (< hr 12))
	   (setq hr (+ 12 hr)))
	  ((and (string-match "am" time2conv) (= hr 12))
           (setq hr 0)))

    ;; convert the actual time into minutes.
    (+ (* hr 60) min)))


(defun appt-update-list ()
  "If the current buffer is visiting the diary, update appointments.
This function is intended for use with `write-file-functions'."
  (and (string-equal buffer-file-name (expand-file-name diary-file))
       appt-timer
       (let ((appt-display-diary nil))
         (appt-check t)))
  nil)


;; In Emacs-21.3, the manual documented the following procedure to
;; activate this package:
;;     (display-time)
;;     (add-hook 'diary-hook 'appt-make-list)
;;     (diary 0)
;; The display-time call was not necessary, AFAICS.
;; What was really needed was to add the hook and load this file.
;; Calling (diary 0) once the hook had been added was in some sense a
;; roundabout way of loading this file. This file used to have code at
;; the top-level that set up the appt-timer and global-mode-string.
;; One way to maintain backwards compatibility would be to call
;; (appt-activate 1) at top-level. However, this goes against the
;; convention that just loading an Emacs package should not activate
;; it. Instead, we make appt-make-list activate the package (after a
;; suggestion from rms). This means that one has to call diary in
;; order to get it to work, but that is in line with the old (weird,
;; IMO) documented behavior for activating the package.
;; Actually, since (diary 0) does not run diary-hook, I don't think
;; the documented behavior in Emacs-21.3 would ever have worked.
;; Oh well, at least with the changes to appt-make-list it will now
;; work as well as it ever did.
;; The new method is just to use (appt-activate 1).
;; -- gmorris

;;;###autoload
(defun appt-activate (&optional arg)
"Toggle checking of appointments.
With optional numeric argument ARG, turn appointment checking on if
ARG is positive, otherwise off."
  (interactive "P")
  (let ((appt-active appt-timer))
    (setq appt-active (if arg (> (prefix-numeric-value arg) 0)
                        (not appt-active)))
    (remove-hook 'write-file-functions 'appt-update-list)
    (or global-mode-string (setq global-mode-string '("")))
    (delq 'appt-mode-string global-mode-string)
    (when appt-timer
      (cancel-timer appt-timer)
      (setq appt-timer nil))
    (when appt-active
      (add-hook 'write-file-functions 'appt-update-list)
      (setq appt-timer (run-at-time t 60 'appt-check)
            global-mode-string
            (append global-mode-string '(appt-mode-string)))
      (appt-check t))))


(provide 'appt)

;; arch-tag: bf5791c4-8921-499e-a26f-772b1788d347
;;; appt.el ends here
