;;; time.el --- display time, load and mail indicator in mode line of Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1985-1987, 1993-1994, 1996, 2000-2022 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Facilities to display current time/date and a new-mail indicator
;; in the Emacs mode line.  The entry point is `display-time'.

;; Use `world-clock' to display world clock in a buffer.

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup display-time nil
  "Display time and load in mode line of Emacs."
  :group 'mode-line
  :group 'mail)

(defcustom display-time-mail-file nil
  "File name of mail inbox file, for indicating existence of new mail.
Non-nil and not a string means don't check for mail; nil means use
default, which is system-dependent, and is the same as used by Rmail."
  :type '(choice (const :tag "None" none)
		 (const :tag "Default" nil)
		 (file :format "%v")))

(defcustom display-time-mail-directory nil
  "Name of mail inbox directory, for indicating existence of new mail.
Any nonempty regular file in the directory is regarded as newly arrived mail.
If nil, do not check a directory for arriving mail."
  :type '(choice (const :tag "None" nil)
		 (directory :format "%v")))

(defcustom display-time-mail-function nil
  "Function to call, for indicating existence of new mail.
If nil, that means use the default method: check that the file
specified by `display-time-mail-file' is nonempty or that the
directory `display-time-mail-directory' contains nonempty files."
  :type '(choice (const :tag "Default" nil)
		 (function)))

(defcustom display-time-default-load-average 0
  "Which load average value will be shown in the mode line.
Almost every system can provide values of load for the past 1 minute,
past 5 or past 15 minutes.  The default is to display 1-minute load average.
The value can be one of:

  0   => 1 minute load
  1   => 5 minutes load
  2   => 15 minutes load
  nil => None (do not display the load average)"
  :type '(choice (const :tag "1 minute load" 0)
		 (const :tag "5 minutes load" 1)
		 (const :tag "15 minutes load" 2)
		 (const :tag "None" nil)))

(defvar display-time-load-average nil
  "Value of the system's load average currently shown on the mode line.
See `display-time-default-load-average'.

This is an internal variable; setting it has no effect.")

(defcustom display-time-load-average-threshold 0.1
  "Load-average values below this value won't be shown in the mode line."
  :type 'number)

;;;###autoload
(defcustom display-time-day-and-date nil "\
Non-nil means \\[display-time] should display day and date as well as time."
  :type 'boolean)

(defvar display-time-timer nil)

(defcustom display-time-interval 60
  "Seconds between updates of time in the mode line."
  :type 'integer)

(defcustom display-time-24hr-format nil
  "Non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23.
A value of nil means 1 <= hh <= 12, and an AM/PM suffix is used."
  :type 'boolean)

(defvar display-time-string nil
  "String used in mode lines to display a time string.
It should not be set directly, but is instead updated by the
`display-time' function.")
;;;###autoload(put 'display-time-string 'risky-local-variable t)

(defcustom display-time-hook nil
  "List of functions to be called when the time is updated on the mode line."
  :type 'hook)

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

;; This business used to be simpler when all mode lines had the same
;; face and the image could just be pbm.  Now we try to rely on an xpm
;; image with a transparent background.  Otherwise, set the background
;; for pbm.

(defcustom display-time-mail-face nil
  "Face to use for `display-time-mail-string'.
If `display-time-use-mail-icon' is non-nil, the image's
background color is the background of this face.  Set this to
make the mail indicator stand out on a color display."
  :group 'mode-line-faces
  :group 'display-time
  :version "22.1"
  :type '(choice (const :tag "None" nil) face))

(defvar display-time-mail-icon
  (find-image '((:type xpm :file "letter.xpm" :ascent center)
		(:type pbm :file "letter.pbm" :ascent center)))
  "Image specification to offer as the mail indicator on a graphic display.
See `display-time-use-mail-icon' and `display-time-mail-face'.")

;; Fixme: Default to icon on graphical display?
(defcustom display-time-use-mail-icon nil
  "Non-nil means use an icon as mail indicator on a graphic display.
Otherwise use `display-time-mail-string'.  The icon may consume less
of the mode line.  It is specified by `display-time-mail-icon'."
  :type 'boolean)

;; Fixme: maybe default to the character if we can display Unicode.
(defcustom display-time-mail-string "Mail"
  "String to use as the mail indicator in `display-time-string-forms'.
This can use the Unicode letter character if you can display it."
  :version "22.1"
  :type '(choice (const "Mail")
		 ;; Use :tag here because the Lucid menu won't display
		 ;; multibyte text.
		 (const :tag "Unicode letter character" "✉")
		 string))

(defcustom display-time-format nil
  "String specifying format for displaying the time in the mode line.
See the function `format-time-string' for an explanation of
how to write this string.  If this is nil, the defaults
depend on `display-time-day-and-date' and `display-time-24hr-format'."
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom display-time-string-forms
  '((if (and (not display-time-format) display-time-day-and-date)
	(format-time-string "%a %b %e " now)
      "")
    (propertize
     (format-time-string (or display-time-format
			     (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
			 now)
     'help-echo (format-time-string "%a %b %e, %Y" now))
    load
    (if mail
	;; Build the string every time to act on customization.
	;; :set-after doesn't help for `customize-option'.  I think it
	;; should.
	(concat
	 " "
	 (propertize
	  display-time-mail-string
	  'display `(when (and display-time-use-mail-icon
			       (display-graphic-p))
		      ,@display-time-mail-icon
		      ,@(if (and display-time-mail-face
				 (memq (plist-get (cdr display-time-mail-icon)
						  :type)
				       '(pbm xbm)))
			    (let ((bg (face-attribute display-time-mail-face
						      :background)))
			      (if (stringp bg)
				  (list :background bg)))))
	  'face display-time-mail-face
	  'help-echo "You have new mail; mouse-2: Read mail"
	  'mouse-face 'mode-line-highlight
	  'local-map (make-mode-line-mouse-map 'mouse-2
					       read-mail-command)))
      "")
    " ")
  "List of expressions governing display of the time in the mode line.
For most purposes, you can control the time format using `display-time-format'
which is a more standard interface.

This expression is a list of expressions that can involve the keywords
`load', `day', `month', and `year', `12-hours', `24-hours', `minutes',
`seconds', all numbers in string form, and `monthname', `dayname', `am-pm',
and `time-zone' all alphabetic strings, and `mail' a true/nil value.

For example:

   ((substring year -2) \"/\" month \"/\" day
    \" \" 24-hours \":\" minutes \":\" seconds
    (if time-zone \" (\") time-zone (if time-zone \")\")
    (if mail \" Mail\" \"\"))

would give mode line times like `94/12/30 21:07:48 (UTC)'."
  :type '(repeat sexp))

(defun display-time-event-handler ()
  (display-time-update)
  (let* ((current (current-time))
	 (timer display-time-timer)
	 ;; Compute the time when this timer will run again, next.
	 (next-time (timer-relative-time
		     (list (aref timer 1) (aref timer 2) (aref timer 3))
		     (* 5 (aref timer 4)) 0)))
    ;; If the activation time is not in the future,
    ;; skip executions until we reach a time in the future.
    ;; This avoids a long pause if Emacs has been suspended for hours.
    (or (time-less-p current next-time)
	(progn
	  (timer-set-time timer (timer-next-integral-multiple-of-time
				 current display-time-interval)
			  display-time-interval)
	  (timer-activate timer)))))

(defun display-time-next-load-average ()
  "Switch between different load averages in the mode line.
Switches from the 1 to 5 to 15 minute load average, and then back to 1."
  (interactive)
  (if (= 3 (setq display-time-load-average (1+ display-time-load-average)))
      (setq display-time-load-average 0))
  (display-time-update))

(defun display-time-mail-check-directory ()
  (let ((mail-files (directory-files display-time-mail-directory t))
	(size 0))
    (while (and mail-files (= size 0))
      ;; Count size of regular files only.
      (setq size (+ size (or (and (file-regular-p (car mail-files))
				  (file-attribute-size
				   (file-attributes (car mail-files))))
			     0)))
      (setq mail-files (cdr mail-files)))
    (if (> size 0)
	size
      nil)))

(with-no-warnings
  ;; Warnings are suppressed to avoid "global/dynamic var `X' lacks a prefix".
  (defvar now)
  (defvar time)
  (defvar load)
  (defvar mail)
  (defvar 24-hours)
  (defvar hour)
  (defvar 12-hours)
  (defvar am-pm)
  (defvar minutes)
  (defvar seconds)
  (defvar time-zone)
  (defvar day)
  (defvar year)
  (defvar monthname)
  (defvar month)
  (defvar dayname))

(defun display-time-update--load ()
  (if (null display-time-load-average)
      ""
    (condition-case ()
        ;; Do not show values less than
        ;; `display-time-load-average-threshold'.
        (if (> (* display-time-load-average-threshold 100)
               (nth display-time-load-average (load-average)))
            ""
          ;; The load average number is mysterious, so
          ;; provide some help.
          (let ((str (format " %03d"
                             (nth display-time-load-average
                                  (load-average)))))
            (propertize
             (concat (substring str 0 -2) "." (substring str -2))
             'local-map (make-mode-line-mouse-map
                         'mouse-2 'display-time-next-load-average)
             'mouse-face 'mode-line-highlight
             'help-echo (concat
                         "System load average for past "
                         (if (= 0 display-time-load-average)
                             "1 minute"
                           (if (= 1 display-time-load-average)
                               "5 minutes"
                             "15 minutes"))
                         "; mouse-2: next"))))
      (error ""))))

(defun display-time-update--mail ()
  (let ((mail-spool-file (or display-time-mail-file
                             (getenv "MAIL")
                             (concat rmail-spool-directory
                                     (user-login-name)))))
    (cond
     (display-time-mail-function
      (funcall display-time-mail-function))
     (display-time-mail-directory
      (display-time-mail-check-directory))
     ((and (stringp mail-spool-file)
           (or (null display-time-server-down-time)
               ;; If have been down for 20 min, try again.
               (time-less-p 1200 (time-since
                              display-time-server-down-time))))
      (let ((start-time (current-time)))
        (prog1
            (display-time-file-nonempty-p mail-spool-file)
          ;; Record whether mail file is accessible.
          (setq display-time-server-down-time
                (let ((end-time (current-time)))
                  (and (time-less-p 20 (time-subtract
                                    end-time start-time))
                       (float-time end-time))))))))))

(defun display-time-update ()
  "Update the `display-time' info for the mode line.
However, don't redisplay right now.

This is used for things like Rmail `g' that want to force an
update which can wait for the next redisplay."
  (let* ((now (current-time))
         (time (current-time-string now))
         (load (display-time-update--load))
         (mail (display-time-update--mail))
         (24-hours (substring time 11 13))
         (hour (string-to-number 24-hours))
         (12-hours (int-to-string (1+ (% (+ hour 11) 12))))
         (am-pm (if (>= hour 12) "pm" "am"))
         (minutes (substring time 14 16))
         (seconds (substring time 17 19))
         (time-zone (car (cdr (current-time-zone now))))
         (day (substring time 8 10))
         (year (format-time-string "%Y" now))
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
  (force-mode-line-update 'all))

(defun display-time-file-nonempty-p (file)
  (let ((remote-file-name-inhibit-cache (- display-time-interval 5)))
    (and (file-exists-p file)
	 (< 0 (file-attribute-size
	       (file-attributes (file-chase-links file)))))))

;;;###autoload
(define-minor-mode display-time-mode
  "Toggle display of time, load level, and mail flag in mode lines.

When Display Time mode is enabled, it updates every minute (you
can control the number of seconds between updates by customizing
`display-time-interval').  If `display-time-day-and-date' is
non-nil, the current day and date are displayed as well.  This
runs the normal hook `display-time-hook' after each update."
  :global t :group 'display-time
  (and display-time-timer (cancel-timer display-time-timer))
  (setq display-time-timer nil)
  (setq display-time-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (setq display-time-load-average display-time-default-load-average)
  (if display-time-mode
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
		 'display-time-event-handler)))


;;; Obsolete names

(define-obsolete-variable-alias 'display-time-world-list
  'world-clock-list "28.1")
(define-obsolete-variable-alias 'display-time-world-time-format
  'world-clock-time-format "28.1")
(define-obsolete-variable-alias 'display-time-world-buffer-name
  'world-clock-buffer-name "28.1")
(define-obsolete-variable-alias 'display-time-world-timer-enable
  'world-clock-timer-enable "28.1")
(define-obsolete-variable-alias 'display-time-world-timer-second
  'world-clock-timer-second "28.1")

(define-obsolete-function-alias 'display-time-world-mode
  #'world-clock-mode "28.1")
(define-obsolete-function-alias 'display-time-world-display
  #'world-clock-display "28.1")
(define-obsolete-function-alias 'display-time-world-timer
  #'world-clock-update "28.1")


;;; World clock

(defgroup world-clock nil
  "Display a world clock."
  :group 'display-time)

(defcustom zoneinfo-style-world-list
  '(("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("Europe/London" "London")
    ("Europe/Paris" "Paris")
    ("Asia/Calcutta" "Bangalore")
    ("Asia/Tokyo" "Tokyo"))
  "Alist of zoneinfo-style time zones and places for `world-clock'.
Each element has the form (TIMEZONE LABEL).
TIMEZONE should be a string of the form AREA/LOCATION, where AREA is
the name of a region -- a continent or ocean, and LOCATION is the name
of a specific location, e.g., a city, within that region.
LABEL is a string to display as the label of that TIMEZONE's time."
  :type '(repeat (list string string))
  :version "23.1")

(defcustom legacy-style-world-list
  '(("PST8PDT" "Seattle")
    ("EST5EDT" "New York")
    ("GMT0BST" "London")
    ("CET-1CDT" "Paris")
    ("IST-5:30" "Bangalore")
    ("JST-9" "Tokyo"))
  "Alist of traditional-style time zones and places for `world-clock'.
Each element has the form (TIMEZONE LABEL).
TIMEZONE should be a string of the form:

     std[+|-]offset[dst[offset][,date[/time],date[/time]]]

See the documentation of the TZ environment variable on your system,
for more details about the format of TIMEZONE.
LABEL is a string to display as the label of that TIMEZONE's time."
  :type '(repeat (list string string))
  :version "23.1")

(defcustom world-clock-list t
  "Alist of time zones and places for `world-clock' to display.
Each element has the form (TIMEZONE LABEL).
TIMEZONE should be in a format supported by your system.  See the
documentation of `zoneinfo-style-world-list' and
`legacy-style-world-list' for two widely used formats.  LABEL is
a string to display as the label of that TIMEZONE's time.

If the value is t instead of an alist, use the value of
`zoneinfo-style-world-list' if it works on this platform, and of
`legacy-style-world-list' otherwise."
  :type '(choice (const :tag "Default" t)
                 (repeat :tag "List of zones and labels"
                         (list (string :tag "Zone") (string :tag "Label"))))
  :version "28.1")

(defun time--display-world-list ()
  (if (listp world-clock-list)
      world-clock-list
    ;; Determine if zoneinfo style timezones are supported by testing that
    ;; America/New York and Europe/London return different timezones.
    (let ((nyt (format-time-string "%z" nil "America/New_York"))
	  (gmt (format-time-string "%z" nil "Europe/London")))
      (if (string-equal nyt gmt)
	  legacy-style-world-list
	zoneinfo-style-world-list))))

(defcustom world-clock-time-format "%A %d %B %R %Z"
  "Time format for `world-clock', see `format-time-string'."
  :type 'string
  :version "28.1")

(defcustom world-clock-buffer-name "*wclock*"
  "Name of the `world-clock' buffer."
  :type 'string
  :version "28.1")

(defcustom world-clock-timer-enable t
  "If non-nil, a timer will update the `world-clock' buffer."
  :type 'boolean
  :version "28.1")

(defcustom world-clock-timer-second 60
  "Interval in seconds for updating the `world-clock' buffer."
  :type 'integer
  :version "28.1")

(defface world-clock-label
  '((t :inherit font-lock-variable-name-face))
  "Face for time zone label in `world-clock' buffer.")

(defvar world-clock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(define-derived-mode world-clock-mode special-mode "World clock"
  "Major mode for buffer that displays times in various time zones.
See `world-clock'."
  :interactive nil
  (setq-local revert-buffer-function #'world-clock-update)
  (setq show-trailing-whitespace nil))

(defvar world-clock--timer nil
  "The current world clock timer.")

(defun world-clock-display (alist)
  "Replace current buffer text with times in various zones, based on ALIST."
  (let ((inhibit-read-only t)
	(buffer-undo-list t)
	(now (current-time))
	(max-width 0)
	result fmt)
    (erase-buffer)
    (dolist (zone alist)
      (let* ((label (cadr zone))
	     (width (string-width label)))
	(push (cons label
		    (format-time-string world-clock-time-format
					now (car zone)))
	      result)
	(when (> width max-width)
	  (setq max-width width))))
    (setq fmt (concat "%-" (int-to-string max-width) "s %s\n"))
    (dolist (timedata (nreverse result))
      (insert (format fmt
                      (propertize (car timedata)
                                  'face 'world-clock-label)
                      (cdr timedata))))
    (delete-char -1))
  (goto-char (point-min)))

;;;###autoload
(define-obsolete-function-alias 'display-time-world
  #'world-clock "28.1")

;;;###autoload
(defun world-clock ()
  "Display a world clock buffer with times in various time zones.
The variable `world-clock-list' specifies which time zones to use.
To turn off the world time display, go to the window and type `\\[quit-window]'."
  (interactive)
  (if-let ((buffer (get-buffer world-clock-buffer-name)))
      (pop-to-buffer buffer)
    (pop-to-buffer world-clock-buffer-name)
    (when world-clock-timer-enable
      (setq world-clock--timer
            (run-at-time t world-clock-timer-second #'world-clock-update))
      (add-hook 'kill-buffer-hook #'world-clock-cancel-timer nil t)))
  (world-clock-display (time--display-world-list))
  (world-clock-mode)
  (fit-window-to-buffer))

(defun world-clock-cancel-timer ()
  "Cancel the world clock timer."
  (when world-clock--timer
    (cancel-timer world-clock--timer)
    (setq world-clock--timer nil)))

(defun world-clock-update (&optional _arg _noconfirm)
  "Update the `world-clock' buffer."
  (if (get-buffer world-clock-buffer-name)
      (with-current-buffer (get-buffer world-clock-buffer-name)
        (let ((op (point)))
          (world-clock-display (time--display-world-list))
          (goto-char op)))
    (world-clock-cancel-timer)))

;;;###autoload
(defun emacs-uptime (&optional format here)
  "Return a string giving the uptime of this instance of Emacs.
FORMAT is a string to format the result, using `format-seconds'.
For example, the Unix uptime command format is \"%D, %z%2h:%.2m\".
If the optional argument HERE is non-nil, insert string at
point."
  (interactive "i\nP")
  (let ((str
         (format-seconds (or format "%Y, %D, %H, %M, %z%S")
			 (time-convert
			  (time-since before-init-time)
			  'integer))))
    (if here
        (insert str)
      (if (called-interactively-p 'interactive)
          (message "%s" str)
        str))))

;;;###autoload
(defun emacs-init-time (&optional format)
  "Return a string giving the duration of the Emacs initialization.
FORMAT is a string to format the result, using `format'.  If nil,
the default format \"%f seconds\" is used."
  (interactive)
  (let ((str (format (or format "%f seconds")
                     (float-time (time-subtract after-init-time
                                                before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str)
      str)))

(provide 'time)

;;; time.el ends here
