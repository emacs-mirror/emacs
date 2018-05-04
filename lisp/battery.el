;;; battery.el --- display battery status information

;; Copyright (C) 1997-1998, 2000-2018 Free Software Foundation, Inc.

;; Author: Ralph Schleicher <rs@nunatak.allgaeu.org>
;; Keywords: hardware

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

;; There is at present support for GNU/Linux, macOS and Windows.  This
;; library supports both the `/proc/apm' file format of Linux version
;; 1.3.58 or newer and the `/proc/acpi/' directory structure of Linux
;; 2.4.20 and 2.6.  Darwin (macOS) is supported by using the `pmset'
;; program.  Windows is supported by the GetSystemPowerStatus API call.

;;; Code:

(require 'timer)
(eval-when-compile (require 'cl-lib))

(defgroup battery nil
  "Display battery status information."
  :prefix "battery-"
  :group 'hardware)

(defcustom battery-linux-sysfs-regexp "[bB][aA][tT][0-9]?$"
  "Regexp for folder names to be searched under
  /sys/class/power_supply/ that contain battery information."
  :version "26.1"
  :type 'regexp
  :group 'battery)

(defcustom battery-upower-device "battery_BAT1"
  "Upower battery device name."
  :version "26.1"
  :type 'string
  :group 'battery)

(defcustom battery-status-function
  (cond ((and (eq system-type 'gnu/linux)
	      (file-readable-p "/proc/apm"))
	 #'battery-linux-proc-apm)
	((and (eq system-type 'gnu/linux)
	      (file-directory-p "/proc/acpi/battery"))
	 #'battery-linux-proc-acpi)
	((and (eq system-type 'gnu/linux)
	      (file-directory-p "/sys/class/power_supply/")
	      (directory-files "/sys/class/power_supply/" nil
                               battery-linux-sysfs-regexp))
	 #'battery-linux-sysfs)
	((and (eq system-type 'berkeley-unix)
	      (file-executable-p "/usr/sbin/apm"))
	 #'battery-bsd-apm)
	((and (eq system-type 'darwin)
	      (condition-case nil
		  (with-temp-buffer
		    (and (eq (call-process "pmset" nil t nil "-g" "ps") 0)
			 (> (buffer-size) 0)))
		(error nil)))
	 #'battery-pmset)
	((fboundp 'w32-battery-status)
	 #'w32-battery-status))
  "Function for getting battery status information.
The function has to return an alist of conversion definitions.
Its cons cells are of the form

    (CONVERSION . REPLACEMENT-TEXT)

CONVERSION is the character code of a \"conversion specification\"
introduced by a `%' character in a control string."
  :type '(choice (const nil) function)
  :group 'battery)

(defcustom battery-echo-area-format
  "Power %L, battery %B (%p%% load, remaining time %t)"
  "Control string formatting the string to display in the echo area.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'.  Here are the ones generally available:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%L AC line status (verbose)
%p Battery load percentage
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  :type '(choice string (const nil))
  :group 'battery)

(defvar battery-mode-line-string nil
  "String to display in the mode line.")
;;;###autoload (put 'battery-mode-line-string 'risky-local-variable t)

(defcustom battery-mode-line-limit 100
  "Percentage of full battery load below which display battery status"
  :version "24.1"
  :type 'integer
  :group 'battery)

(defcustom battery-mode-line-format
  (cond ((eq battery-status-function 'battery-linux-proc-acpi)
	 "[%b%p%%,%d°C]")
	(battery-status-function
	 "[%b%p%%]"))
  "Control string formatting the string to display in the mode line.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'.  Here are the ones generally available:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%L AC line status (verbose)
%p Battery load percentage
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  :type '(choice string (const nil))
  :group 'battery)

(defcustom battery-update-interval 60
  "Seconds after which the battery status will be updated."
  :type 'integer
  :group 'battery)

(defcustom battery-load-low 25
  "Upper bound of low battery load percentage.
A battery load percentage below this number is considered low."
  :type 'integer
  :group 'battery)

(defcustom battery-load-critical 10
  "Upper bound of critical battery load percentage.
A battery load percentage below this number is considered critical."
  :type 'integer
  :group 'battery)

(defvar battery-update-timer nil
  "Interval timer object.")

;;;###autoload
(defun battery ()
  "Display battery status information in the echo area.
The text being displayed in the echo area is controlled by the variables
`battery-echo-area-format' and `battery-status-function'."
  (interactive)
  (message "%s" (if (and battery-echo-area-format battery-status-function)
		    (battery-format battery-echo-area-format
				    (funcall battery-status-function))
		  "Battery status not available")))

;;;###autoload
(define-minor-mode display-battery-mode
  "Toggle battery status display in mode line (Display Battery mode).
With a prefix argument ARG, enable Display Battery mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

The text displayed in the mode line is controlled by
`battery-mode-line-format' and `battery-status-function'.
The mode line is be updated every `battery-update-interval'
seconds."
  :global t :group 'battery
  (setq battery-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and battery-update-timer (cancel-timer battery-update-timer))
  (if (and battery-status-function battery-mode-line-format)
      (if (not display-battery-mode)
	  (setq global-mode-string
		(delq 'battery-mode-line-string global-mode-string))
	(add-to-list 'global-mode-string 'battery-mode-line-string t)
	(setq battery-update-timer (run-at-time nil battery-update-interval
						'battery-update-handler))
	(battery-update))
    (message "Battery status not available")
    (setq display-battery-mode nil)))

(defun battery-update-handler ()
  (battery-update)
  (sit-for 0))

(defun battery-update ()
  "Update battery status information in the mode line."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data))))))
    (setq battery-mode-line-string
	  (propertize (if (and battery-mode-line-format
			       (numberp percentage)
                               (<= percentage battery-mode-line-limit))
			  (battery-format battery-mode-line-format data)
			"")
		      'face
                      (and (numberp percentage)
                           (<= percentage battery-load-critical)
                           'error)
		      'help-echo "Battery status information")))
  (force-mode-line-update))

;;; `/proc/apm' interface for Linux.

(defconst battery-linux-proc-apm-regexp
  (concat "^\\([^ ]+\\)"		; Driver version.
	  " \\([^ ]+\\)"		; APM BIOS version.
	  " 0x\\([0-9a-f]+\\)"		; APM BIOS flags.
	  " 0x\\([0-9a-f]+\\)"		; AC line status.
	  " 0x\\([0-9a-f]+\\)"		; Battery status.
	  " 0x\\([0-9a-f]+\\)"		; Battery flags.
	  " \\(-?[0-9]+\\)%"		; Load percentage.
	  " \\(-?[0-9]+\\)"		; Remaining time.
	  " \\(.*\\)"			; Time unit.
	  "$")
  "Regular expression matching contents of `/proc/apm'.")

(defun battery-linux-proc-apm ()
  "Get APM status information from Linux (the kernel).
This function works only with the new `/proc/apm' format introduced
in Linux version 1.3.58.

The following %-sequences are provided:
%v Linux driver version
%V APM BIOS version
%I APM BIOS status (verbose)
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let (driver-version bios-version bios-interface line-status
	battery-status battery-status-symbol load-percentage
	seconds minutes hours remaining-time tem)
    (with-temp-buffer
      (ignore-errors (insert-file-contents "/proc/apm"))
      (when (re-search-forward battery-linux-proc-apm-regexp)
	(setq driver-version (match-string 1))
	(setq bios-version (match-string 2))
	(setq tem (string-to-number (match-string 3) 16))
	(if (not (logand tem 2))
	    (setq bios-interface "not supported")
	  (setq bios-interface "enabled")
	  (cond ((logand tem 16) (setq bios-interface "disabled"))
		((logand tem 32) (setq bios-interface "disengaged")))
	  (setq tem (string-to-number (match-string 4) 16))
	  (cond ((= tem 0) (setq line-status "off-line"))
		((= tem 1) (setq line-status "on-line"))
		((= tem 2) (setq line-status "on backup")))
	  (setq tem (string-to-number (match-string 6) 16))
	  (if (= tem 255)
	      (setq battery-status "N/A")
	    (setq tem (string-to-number (match-string 5) 16))
	    (cond ((= tem 0) (setq battery-status "high"
				   battery-status-symbol ""))
		  ((= tem 1) (setq battery-status "low"
				   battery-status-symbol "-"))
		  ((= tem 2) (setq battery-status "critical"
				   battery-status-symbol "!"))
		  ((= tem 3) (setq battery-status "charging"
				   battery-status-symbol "+")))
	    (setq load-percentage (match-string 7))
	    (setq seconds (string-to-number (match-string 8)))
	    (and (string-equal (match-string 9) "min")
		 (setq seconds (* 60 seconds)))
	    (setq minutes (/ seconds 60)
		  hours (/ seconds 3600))
	    (setq remaining-time
		  (format "%d:%02d" hours (- minutes (* 60 hours))))))))
    (list (cons ?v (or driver-version "N/A"))
	  (cons ?V (or bios-version "N/A"))
	  (cons ?I (or bios-interface "N/A"))
	  (cons ?L (or line-status "N/A"))
	  (cons ?B (or battery-status "N/A"))
	  (cons ?b (or battery-status-symbol ""))
	  (cons ?p (or load-percentage "N/A"))
	  (cons ?s (or (and seconds (number-to-string seconds)) "N/A"))
	  (cons ?m (or (and minutes (number-to-string minutes)) "N/A"))
	  (cons ?h (or (and hours (number-to-string hours)) "N/A"))
	  (cons ?t (or remaining-time "N/A")))))


;;; `/proc/acpi/' interface for Linux.

(defun battery-linux-proc-acpi ()
  "Get ACPI status information from Linux (the kernel).
This function works only with the `/proc/acpi/' format introduced
in Linux version 2.4.20 and 2.6.0.

The following %-sequences are provided:
%c Current capacity (mAh)
%r Current rate
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%L AC line status (verbose)
%p Battery load percentage
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let ((design-capacity 0)
	(last-full-capacity 0)
	full-capacity
	(warn 0)
	(low 0)
	capacity rate rate-type charging-state minutes hours)
    ;; ACPI provides information about each battery present in the system in
    ;; a separate subdirectory.  We are going to merge the available
    ;; information together since displaying for a variable amount of
    ;; batteries seems overkill for format-strings.
    (with-temp-buffer
      (dolist (dir (ignore-errors (directory-files "/proc/acpi/battery/"
						   t "\\`[^.]")))
	(erase-buffer)
	(ignore-errors (insert-file-contents (expand-file-name "state" dir)))
	(when (re-search-forward "present: +yes$" nil t)
	  (and (re-search-forward "charging state: +\\(.*\\)$" nil t)
	       (member charging-state '("unknown" "charged" nil))
	       ;; On most multi-battery systems, most of the time only one
	       ;; battery is "charging"/"discharging", the others are
	       ;; "unknown".
	       (setq charging-state (match-string 1)))
	  (when (re-search-forward "present rate: +\\([0-9]+\\) \\(m[AW]\\)$"
				   nil t)
	    (setq rate (+ (or rate 0) (string-to-number (match-string 1))))
	    (when (> rate 0)
	      (setq rate-type (or (and rate-type
				     (if (string= rate-type (match-string 2))
					 rate-type
				       (error
					"Inconsistent rate types (%s vs. %s)"
					rate-type (match-string 2))))
				  (match-string 2)))))
	  (when (re-search-forward "remaining capacity: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (setq capacity
		  (+ (or capacity 0) (string-to-number (match-string 1))))))
	(goto-char (point-max))
	(ignore-errors (insert-file-contents (expand-file-name "info" dir)))
	(when (re-search-forward "present: +yes$" nil t)
	  (when (re-search-forward "design capacity: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (cl-incf design-capacity (string-to-number (match-string 1))))
	  (when (re-search-forward "last full capacity: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (cl-incf last-full-capacity (string-to-number (match-string 1))))
	  (when (re-search-forward
		 "design capacity warning: +\\([0-9]+\\) m[AW]h$" nil t)
	    (cl-incf warn (string-to-number (match-string 1))))
	  (when (re-search-forward "design capacity low: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (cl-incf low (string-to-number (match-string 1)))))))
    (setq full-capacity (if (> last-full-capacity 0)
			    last-full-capacity design-capacity))
    (and capacity rate
	 (setq minutes (if (zerop rate) 0
			 (floor (* (/ (float (if (string= charging-state
							  "charging")
						 (- full-capacity capacity)
					       capacity))
				      rate)
				   60)))
	       hours (/ minutes 60)))
    (list (cons ?c (or (and capacity (number-to-string capacity)) "N/A"))
	  (cons ?L (or (battery-search-for-one-match-in-files
			(mapcar (lambda (e) (concat e "/state"))
				(ignore-errors
				  (directory-files "/proc/acpi/ac_adapter/"
						   t "\\`[^.]")))
			"state: +\\(.*\\)$" 1)

		       "N/A"))
	  (cons ?d (or (battery-search-for-one-match-in-files
			(mapcar (lambda (e) (concat e "/temperature"))
				(ignore-errors
				  (directory-files "/proc/acpi/thermal_zone/"
						   t "\\`[^.]")))
			"temperature: +\\([0-9]+\\) C$" 1)

		       "N/A"))
	  (cons ?r (or (and rate (concat (number-to-string rate) " "
					 rate-type)) "N/A"))
	  (cons ?B (or charging-state "N/A"))
	  (cons ?b (or (and (string= charging-state "charging") "+")
		       (and capacity (< capacity low) "!")
		       (and capacity (< capacity warn) "-")
		       ""))
	  (cons ?h (or (and hours (number-to-string hours)) "N/A"))
	  (cons ?m (or (and minutes (number-to-string minutes)) "N/A"))
	  (cons ?t (or (and minutes
			    (format "%d:%02d" hours (- minutes (* 60 hours))))
		       "N/A"))
	  (cons ?p (or (and full-capacity capacity
			    (> full-capacity 0)
			    (number-to-string
			     (floor (/ capacity
				       (/ (float full-capacity) 100)))))
		       "N/A")))))


;;; `/sys/class/power_supply/BATN' interface for Linux.

(defun battery-linux-sysfs ()
  "Get ACPI status information from Linux kernel.
This function works only with the new `/sys/class/power_supply/'
format introduced in Linux version 2.4.25.

The following %-sequences are provided:
%c Current capacity (mAh or mWh)
%r Current rate
%B Battery status (verbose)
%d Temperature (in degrees Celsius)
%p Battery load percentage
%L AC line status (verbose)
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let (charging-state temperature hours
        ;; Some batteries report charges and current, other energy and power.
        ;; In order to reliably be able to combine those data, we convert them
        ;; all to energy/power (since we can't combine different charges if
        ;; they're not at the same voltage).
	(energy-full 0.0)
	(energy-now 0.0)
	(power-now 0.0)
	(voltage-now 10.8))    ;Arbitrary default, in case the info is missing.
    ;; SysFS provides information about each battery present in the
    ;; system in a separate subdirectory.  We are going to merge the
    ;; available information together.
    (with-temp-buffer
      (dolist (dir (ignore-errors
		    (directory-files
		     "/sys/class/power_supply/" t
                     battery-linux-sysfs-regexp)))
	(erase-buffer)
	(ignore-errors (insert-file-contents
			(expand-file-name "uevent" dir)))
	(goto-char (point-min))
	(when (re-search-forward
	       "POWER_SUPPLY_VOLTAGE_NOW=\\([0-9]*\\)$" nil t)
	  (setq voltage-now (/ (string-to-number (match-string 1)) 1000000.0)))
	(goto-char (point-min))
	(when (re-search-forward "POWER_SUPPLY_PRESENT=1$" nil t)
	  (goto-char (point-min))
	  (and (re-search-forward "POWER_SUPPLY_STATUS=\\(.*\\)$" nil t)
	       (member charging-state '("Unknown" "Full" nil))
	       (setq charging-state (match-string 1)))
	  (goto-char (point-min))
	  (when (re-search-forward
                 "POWER_SUPPLY_\\(CURRENT\\|POWER\\)_NOW=\\([0-9]*\\)$"
                 nil t)
	    (cl-incf power-now
		     (* (float (string-to-number (match-string 2)))
			(if (eq (char-after (match-beginning 1)) ?C)
			    voltage-now 1.0))))
	  (goto-char (point-min))
	  (when (re-search-forward "POWER_SUPPLY_TEMP=\\([0-9]*\\)$" nil t)
	    (setq temperature (match-string 1)))
	  (goto-char (point-min))
	  (let (full-string now-string)
	    ;; Sysfs may list either charge (mAh) or energy (mWh).
	    ;; Keep track of both, and choose which to report later.
	    (cond ((and (re-search-forward
			 "POWER_SUPPLY_CHARGE_FULL=\\([0-9]*\\)$" nil t)
			(setq full-string (match-string 1))
			(re-search-forward
			 "POWER_SUPPLY_CHARGE_NOW=\\([0-9]*\\)$" nil t)
			(setq now-string (match-string 1)))
		   (cl-incf energy-full (* (string-to-number full-string)
                                           voltage-now))
		   (cl-incf energy-now  (* (string-to-number now-string)
                                           voltage-now)))
		  ((and (progn (goto-char (point-min)) t)
			(re-search-forward
			 "POWER_SUPPLY_ENERGY_FULL=\\([0-9]*\\)$" nil t)
			(setq full-string (match-string 1))
			(re-search-forward
			 "POWER_SUPPLY_ENERGY_NOW=\\([0-9]*\\)$" nil t)
			(setq now-string (match-string 1)))
		   (cl-incf energy-full (string-to-number full-string))
		   (cl-incf energy-now  (string-to-number now-string)))))
	  (goto-char (point-min))
	  (unless (zerop power-now)
	    (let ((remaining (if (string= charging-state "Discharging")
				 energy-now
			       (- energy-full energy-now))))
	      (setq hours (/ remaining power-now)))))))
    (list (cons ?c (cond ((or (> energy-full 0) (> energy-now 0))
			  (number-to-string (/ energy-now voltage-now)))
			 (t "N/A")))
	  (cons ?r (if (> power-now 0.0)
		       (format "%.1f" (/ power-now 1000000.0))
		     "N/A"))
	  (cons ?m (if hours (format "%d" (* hours 60)) "N/A"))
	  (cons ?h (if hours (format "%d" hours) "N/A"))
	  (cons ?t (if hours
		       (format "%d:%02d" hours (* (- hours (floor hours)) 60))
		     "N/A"))
	  (cons ?d (or temperature "N/A"))
	  (cons ?B (or charging-state "N/A"))
	  (cons ?p (cond ((and (> energy-full 0) (> energy-now 0))
			  (format "%.1f"
				  (/ (* 100 energy-now) energy-full)))
			 (t "N/A")))
	  (cons ?L (cond
                    ((battery-search-for-one-match-in-files
                      (list "/sys/class/power_supply/AC/online"
                            "/sys/class/power_supply/ACAD/online"
                            "/sys/class/power_supply/ADP1/online")
                      "1" 0)
                     "AC")
                    ((battery-search-for-one-match-in-files
                      (list "/sys/class/power_supply/AC/online"
                            "/sys/class/power_supply/ACAD/online"
                            "/sys/class/power_supply/ADP1/online")
                      "0" 0)
                     "BAT")
                    (t "N/A"))))))


(declare-function dbus-get-property "dbus.el"
                  (bus service path interface property))

;;; `upowerd' interface.
(defsubst battery-upower-prop (pname &optional device)
  (dbus-get-property
   :system
   "org.freedesktop.UPower"
   (concat "/org/freedesktop/UPower/devices/" (or device battery-upower-device))
   "org.freedesktop.UPower"
   pname))

(defun battery-upower ()
  "Get battery status from dbus Upower interface.
This function works only in systems with `upowerd' daemon
running.

The following %-sequences are provided:
%c Current capacity (mWh)
%p Battery load percentage
%r Current rate
%B Battery status (verbose)
%L AC line status (verbose)
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let ((percents (battery-upower-prop "Percentage"))
        (time-to-empty (battery-upower-prop "TimeToEmpty"))
        (time-to-full (battery-upower-prop "TimeToFull"))
        (state (battery-upower-prop "State"))
        (online (battery-upower-prop "Online" "line_power_ACAD"))
        (energy (battery-upower-prop "Energy"))
        (energy-rate (battery-upower-prop "EnergyRate"))
        (battery-states '((0 . "unknown") (1 . "charging")
                          (2 . "discharging") (3 . "empty")
                          (4 . "fully-charged") (5 . "pending-charge")
                          (6 . "pending-discharge")))
        seconds minutes hours remaining-time)
    (cond ((and online time-to-full)
           (setq seconds time-to-full))
          ((and (not online) time-to-empty)
           (setq seconds time-to-empty)))
    (when seconds
      (setq minutes (/ seconds 60)
            hours (/ minutes 60)
            remaining-time
            (format "%d:%02d" (truncate hours)
                    (- (truncate minutes) (* 60 (truncate hours))))))
    (list (cons ?c (or (and energy
                            (number-to-string (round (* 1000 energy))))
                       "N/A"))
          (cons ?p (or (and percents (number-to-string (round percents)))
                       "N/A"))
          (cons ?r (or (and energy-rate
                            (concat (number-to-string energy-rate) " W"))
                       "N/A"))
          (cons ?B (or (and state (cdr (assoc state battery-states)))
                       "unknown"))
          (cons ?L (or (and online "on-line") "off-line"))
          (cons ?s (or (and seconds (number-to-string seconds)) "N/A"))
          (cons ?m (or (and minutes (number-to-string minutes)) "N/A"))
          (cons ?h (or (and hours (number-to-string hours)) "N/A"))
          (cons ?t (or remaining-time "N/A")))))


;;; `apm' interface for BSD.
(defun battery-bsd-apm ()
  "Get APM status information from BSD apm binary.
The following %-sequences are provided:
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
 `!' means critical, and `+' means charging
%P Advanced power saving mode state (verbose)
%p Battery charge percentage
%s Remaining battery charge time in seconds
%m Remaining battery charge time in minutes
%h Remaining battery charge time in hours
%t Remaining battery charge time in the form `h:min'"
  (let* ((os-name (car (split-string
			(shell-command-to-string "/usr/bin/uname"))))
	 (apm-flag (if (equal os-name "OpenBSD") "P" "s"))
	 (apm-cmd (concat "/usr/sbin/apm -ablm" apm-flag))
	 (apm-output (split-string (shell-command-to-string apm-cmd)))
	 ;; Battery status
	 (battery-status
	  (let ((stat (string-to-number (nth 0 apm-output))))
	    (cond ((eq stat 0) '("high" . ""))
		  ((eq stat 1) '("low" . "-"))
		  ((eq stat 2) '("critical" . "!"))
		  ((eq stat 3) '("charging" . "+"))
		  ((eq stat 4) '("absent" . nil)))))
	 ;; Battery percentage
	 (battery-percentage (nth 1 apm-output))
	 ;; Battery life
	 (battery-life (nth 2 apm-output))
	 ;; AC status
	 (line-status
	  (let ((ac (string-to-number (nth 3 apm-output))))
	    (cond ((eq ac 0) "disconnected")
		  ((eq ac 1) "connected")
		  ((eq ac 2) "backup power"))))
	 ;; Advanced power savings mode
	 (apm-mode
	  (let ((apm (string-to-number (nth 4 apm-output))))
	    (if (string= os-name "OpenBSD")
		(cond ((eq apm 0) "manual")
		      ((eq apm 1) "automatic")
		      ((eq apm 2) "cool running"))
	      (if (eq apm 1) "on" "off"))))
	 seconds minutes hours remaining-time)
    (unless (member battery-life '("unknown" "-1"))
      (if (member os-name '("OpenBSD" "NetBSD"))
	  (setq minutes (string-to-number battery-life)
		seconds (* 60 minutes))
	(setq seconds (string-to-number battery-life)
	      minutes (truncate (/ seconds 60))))
      (setq hours (truncate (/ minutes 60))
	    remaining-time (format "%d:%02d" hours
				   (- minutes (* 60 hours)))))
    (list (cons ?L (or line-status "N/A"))
	  (cons ?B (or (car battery-status) "N/A"))
	  (cons ?b (or (cdr battery-status) "N/A"))
	  (cons ?p (if (string= battery-percentage "255")
		       "N/A"
		     battery-percentage))
	  (cons ?P (or apm-mode "N/A"))
	  (cons ?s (or (and seconds (number-to-string seconds)) "N/A"))
	  (cons ?m (or (and minutes (number-to-string minutes)) "N/A"))
	  (cons ?h (or (and hours (number-to-string hours)) "N/A"))
	  (cons ?t (or remaining-time "N/A")))))


;;; `pmset' interface for Darwin (macOS).

(defun battery-pmset ()
  "Get battery status information using `pmset'.

The following %-sequences are provided:
%L Power source (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%h Remaining time in hours
%m Remaining time in minutes
%t Remaining time in the form `h:min'"
  (let (power-source load-percentage battery-status battery-status-symbol
	remaining-time hours minutes)
    (with-temp-buffer
      (ignore-errors (call-process "pmset" nil t nil "-g" "ps"))
      (goto-char (point-min))
      (when (re-search-forward "\\(?:Currentl?y\\|Now\\) drawing from '\\(AC\\|Battery\\) Power'" nil t)
	(setq power-source (match-string 1))
	(when (re-search-forward "^ -InternalBattery-0\\([ \t]+(id=[0-9]+)\\)*[ \t]+" nil t)
	  (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
	    (setq load-percentage (match-string 1))
	    (goto-char (match-end 0))
	    (cond ((looking-at "; charging")
		   (setq battery-status "charging"
			 battery-status-symbol "+"))
		  ((< (string-to-number load-percentage) battery-load-critical)
		   (setq battery-status "critical"
			 battery-status-symbol "!"))
		  ((< (string-to-number load-percentage) battery-load-low)
		   (setq battery-status "low"
			 battery-status-symbol "-"))
		  (t
		   (setq battery-status "high"
			 battery-status-symbol "")))
	    (when (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\) remaining"  nil t)
	      (setq remaining-time (match-string 1))
	      (let ((h (string-to-number (match-string 2)))
		    (m (string-to-number (match-string 3))))
		(setq hours (number-to-string (+ h (if (< m 30) 0 1)))
		      minutes (number-to-string (+ (* h 60) m)))))))))
    (list (cons ?L (or power-source "N/A"))
	  (cons ?p (or load-percentage "N/A"))
	  (cons ?B (or battery-status "N/A"))
	  (cons ?b (or battery-status-symbol ""))
	  (cons ?h (or hours "N/A"))
	  (cons ?m (or minutes "N/A"))
	  (cons ?t (or remaining-time "N/A")))))


;;; Private functions.

(defun battery-format (format alist)
  "Substitute %-sequences in FORMAT."
  (replace-regexp-in-string
   "%."
   (lambda (str)
     (let ((char (aref str 1)))
       (if (eq char ?%) "%"
	 (or (cdr (assoc char alist)) ""))))
   format t t))

(defun battery-search-for-one-match-in-files (files regexp match-num)
  "Search REGEXP in the content of the files listed in FILES.
If a match occurred, return the parenthesized expression numbered by
MATCH-NUM in the match.  Otherwise, return nil."
  (with-temp-buffer
    (catch 'found
      (dolist (file files)
	(and (ignore-errors (insert-file-contents file nil nil nil 'replace))
	     (re-search-forward regexp nil t)
	     (throw 'found (match-string match-num)))))))


(provide 'battery)

;;; battery.el ends here
