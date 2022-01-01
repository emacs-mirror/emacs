;;; battery.el --- display battery status information  -*- lexical-binding:t -*-

;; Copyright (C) 1997-1998, 2000-2022 Free Software Foundation, Inc.

;; Author: Ralph Schleicher <rs@ralph-schleicher.de>
;; Maintainer: emacs-devel@gnu.org
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

;; There is at present support for GNU/Linux, BSD, macOS, and Windows.
;; This library supports:
;; - UPower (https://upower.freedesktop.org) via D-Bus API.
;; - The `/sys/class/power_supply/' files of Linux >= 2.6.39.
;; - The `/proc/acpi/' directory structure of Linux 2.4.20 and 2.6.
;; - The `/proc/apm' file format of Linux version 1.3.58 or newer.
;; - BSD by using the `apm' program.
;; - Darwin (macOS) by using the `pmset' program.
;; - Windows via the GetSystemPowerStatus API call.

;;; Code:

(require 'dbus)
(eval-when-compile (require 'cl-lib))

(defgroup battery nil
  "Display battery status information."
  :prefix "battery-"
  :group 'hardware)

(defcustom battery-upower-device nil
  "Preferred UPower device name(s).
When `battery-status-function' is set to `battery-upower', this
user option specifies which power sources to query for status
information and merge into a single report.

When nil (the default), `battery-upower' queries all present
battery and line power devices as determined by the UPower
EnumerateDevices method.  A string or a nonempty list of strings
names particular devices to query instead.  UPower battery and
line power device names typically follow the patterns
\"battery_BATN\" and \"line_power_ACN\", respectively, with N
starting at 0 when present.  Device names should not include the
leading D-Bus path \"/org/freedesktop/UPower/devices/\"."
  :version "28.1"
  :type '(choice (const :tag "Autodetect all devices" nil)
                 (string :tag "Device")
                 (repeat :tag "Devices" string)))

(defcustom battery-upower-subscribe t
  "Whether to subscribe to UPower device change signals.
When nil, battery status information is polled every
`battery-update-interval' seconds.  When non-nil (the default),
the battery status is also updated whenever a power source is
added or removed, or when the system starts or stops running on
battery power.

This only takes effect when `battery-status-function' is set to
`battery-upower' before enabling `display-battery-mode'."
  :version "28.1"
  :type 'boolean)

(defconst battery-upower-service "org.freedesktop.UPower"
  "Well-known name of the UPower D-Bus service.
See URL `https://upower.freedesktop.org/docs/ref-dbus.html'.")

(defun battery--files (dir)
  "Return a list of absolute file names in DIR or nil on error.
Value does not include \".\" or \"..\"."
  (ignore-errors (directory-files dir t directory-files-no-dot-files-regexp)))

(defun battery--find-linux-sysfs-batteries ()
  "Return a list of all sysfs battery directories."
  (let (dirs)
    (dolist (dir (battery--files "/sys/class/power_supply/"))
      (when (file-exists-p (expand-file-name "capacity" dir))
        (push dir dirs)))
    (nreverse dirs)))

(defcustom battery-status-function
  (cond ((member battery-upower-service (dbus-list-activatable-names))
         #'battery-upower)
        ((and (eq system-type 'gnu/linux)
              (battery--find-linux-sysfs-batteries))
         #'battery-linux-sysfs)
	((and (eq system-type 'gnu/linux)
	      (file-directory-p "/proc/acpi/battery"))
	 #'battery-linux-proc-acpi)
	((and (eq system-type 'gnu/linux)
              (file-readable-p "/proc/apm"))
         #'battery-linux-proc-apm)
	((and (eq system-type 'berkeley-unix)
	      (file-executable-p "/usr/sbin/apm"))
	 #'battery-bsd-apm)
	((and (eq system-type 'darwin)
              (ignore-errors
                (with-temp-buffer
                  (and (eq (call-process "pmset" nil t nil "-g" "ps") 0)
                       (not (bobp))))))
	 #'battery-pmset)
	((fboundp 'w32-battery-status)
	 #'w32-battery-status))
  "Function for getting battery status information.
The function has to return an alist of conversion definitions.
Its cons cells are of the form

    (CONVERSION . REPLACEMENT-TEXT)

CONVERSION is the character code of a \"conversion specification\"
introduced by a `%' character in a control string."
  :version "28.1"
  :type '(choice (const nil) function))

(defcustom battery-echo-area-format
  "Power %L, battery %B (%p%% load, remaining time %t)"
  "Control string formatting the string to display in the echo area.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'.  Here are the ones generally available:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'

The full `format-spec' formatting syntax is supported."
  :link '(info-link "(elisp) Custom Format Strings")
  :type '(choice string (const nil)))

(defvar battery-mode-line-string nil
  "String to display in the mode line.")
;;;###autoload (put 'battery-mode-line-string 'risky-local-variable t)

(defcustom battery-mode-line-limit 100
  "Percentage of full battery load below which display battery status."
  :version "24.1"
  :type 'integer)

(defcustom battery-mode-line-format
  (cond ((eq battery-status-function #'battery-linux-proc-acpi)
	 "[%b%p%%,%d°C] ")
	(battery-status-function
	 "[%b%p%%] "))
  "Control string formatting the string to display in the mode line.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'.  Here are the ones generally available:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'

The full `format-spec' formatting syntax is supported."
  :link '(info-link "(elisp) Custom Format Strings")
  :type '(choice string (const nil)))

(defcustom battery-update-interval 60
  "Seconds after which the battery status will be updated."
  :type 'integer)

(defcustom battery-load-low 25
  "Upper bound of low battery load percentage.
A battery load percentage below this number is considered low."
  :type 'integer)

(defcustom battery-load-critical 10
  "Upper bound of critical battery load percentage.
A battery load percentage below this number is considered critical."
  :type 'integer)

(defface battery-load-low
  '((t :inherit warning))
  "Face used in mode line string when battery load is low.
See the option `battery-load-low'."
  :version "28.1")

(defface battery-load-critical
  '((t :inherit error))
  "Face used in mode line string when battery load is critical.
See the option `battery-load-critical'."
  :version "28.1")

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

The text displayed in the mode line is controlled by
`battery-mode-line-format' and `battery-status-function'.
The mode line is be updated every `battery-update-interval'
seconds."
  :global t
  (setq battery-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and battery-update-timer (cancel-timer battery-update-timer))
  (battery--upower-unsubscribe)
  (if (and battery-status-function battery-mode-line-format)
      (if (not display-battery-mode)
	  (setq global-mode-string
		(delq 'battery-mode-line-string global-mode-string))
	(add-to-list 'global-mode-string 'battery-mode-line-string t)
        (and (eq battery-status-function #'battery-upower)
             battery-upower-subscribe
             (battery--upower-subscribe))
	(setq battery-update-timer (run-at-time nil battery-update-interval
                                                #'battery-update-handler))
	(battery-update))
    (message "Battery status not available")
    (setq display-battery-mode nil)))

(defun battery-update-handler ()
  (battery-update)
  (sit-for 0))

(defun battery-update ()
  "Update battery status information in the mode line."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data)))))
         (res (and battery-mode-line-format
                   (or (not (numberp percentage))
                       (<= percentage battery-mode-line-limit))
                   (battery-format battery-mode-line-format data)))
         (len (length res)))
    (unless (zerop len)
      (cond ((not (numberp percentage)))
            ((< percentage battery-load-critical)
             (add-face-text-property 0 len 'battery-load-critical t res))
            ((< percentage battery-load-low)
             (add-face-text-property 0 len 'battery-load-low t res)))
      (put-text-property 0 len 'help-echo "Battery status information" res))
    (setq battery-mode-line-string (or res "")))
  (force-mode-line-update t))


;;; `/proc/apm' interface for Linux.

;; Regular expression matching contents of `/proc/apm'.
(rx-define battery--linux-proc-apm
  (: bol   (group (+ (not ?\s)))        ; Driver version.
     " "   (group (+ (not ?\s)))        ; APM BIOS version.
     " 0x" (group (+ xdigit))           ; APM BIOS flags.
     " 0x" (group (+ xdigit))           ; AC line status.
     " 0x" (group (+ xdigit))           ; Battery status.
     " 0x" (group (+ xdigit))           ; Battery flags.
     " "   (group (? ?-) (+ digit)) ?%  ; Load percentage.
     " "   (group (? ?-) (+ digit))     ; Remaining time.
     " "   (group (* nonl))             ; Time unit
     eol))

(defconst battery-linux-proc-apm-regexp (rx battery--linux-proc-apm)
  "Regular expression matching contents of `/proc/apm'.")
(make-obsolete-variable 'battery-linux-proc-apm-regexp
                        "it is no longer used." "28.1")

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
  (let ( driver-version bios-version bios-interface line-status
         battery-status battery-status-symbol load-percentage
         seconds minutes hours remaining-time tem )
    (with-temp-buffer
      (ignore-errors (insert-file-contents "/proc/apm"))
      (when (re-search-forward (rx battery--linux-proc-apm) nil t)
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
          (unless (= (string-to-number (match-string 6) 16) 255)
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
                  (format "%d:%02d" hours (% minutes 60)))))))
    (list (cons ?v (or driver-version "N/A"))
	  (cons ?V (or bios-version "N/A"))
	  (cons ?I (or bios-interface "N/A"))
	  (cons ?L (or line-status "N/A"))
	  (cons ?B (or battery-status "N/A"))
	  (cons ?b (or battery-status-symbol ""))
	  (cons ?p (or load-percentage "N/A"))
          (cons ?s (if seconds (number-to-string seconds) "N/A"))
          (cons ?m (if minutes (number-to-string minutes) "N/A"))
          (cons ?h (if hours (number-to-string hours) "N/A"))
	  (cons ?t (or remaining-time "N/A")))))


;;; `/proc/acpi/' interface for Linux.

(rx-define battery--acpi-rate (&rest hour)
  (: (group (+ digit)) " " (group ?m (in "AW") hour)))
(rx-define battery--acpi-capacity (battery--acpi-rate ?h))

(defun battery-linux-proc-acpi ()
  "Get ACPI status information from Linux (the kernel).
This function works only with the `/proc/acpi/' interface
introduced in Linux version 2.4.20 and 2.6.0.

The following %-sequences are provided:
%c Current capacity (mAh)
%r Current rate of charge or discharge
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
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
      (dolist (dir (battery--files "/proc/acpi/battery/"))
        (ignore-errors
          (insert-file-contents (expand-file-name "state" dir) nil nil nil t))
        (goto-char (point-min))
        (when (re-search-forward (rx "present:" (+ space) "yes" eol) nil t)
          (and (re-search-forward (rx "charging state:" (+ space)
                                      (group (not space) (* nonl)) eol)
                                  nil t)
	       (member charging-state '("unknown" "charged" nil))
	       ;; On most multi-battery systems, most of the time only one
	       ;; battery is "charging"/"discharging", the others are
	       ;; "unknown".
	       (setq charging-state (match-string 1)))
          (when (re-search-forward (rx "present rate:" (+ space)
                                       (battery--acpi-rate) eol)
				   nil t)
	    (setq rate (+ (or rate 0) (string-to-number (match-string 1))))
	    (when (> rate 0)
              (cond ((not rate-type)
                     (setq rate-type (match-string 2)))
                    ((not (string= rate-type (match-string 2)))
                     (error "Inconsistent rate types (%s vs. %s)"
                            rate-type (match-string 2))))))
          (when (re-search-forward (rx "remaining capacity:" (+ space)
                                       battery--acpi-capacity eol)
				   nil t)
	    (setq capacity
		  (+ (or capacity 0) (string-to-number (match-string 1))))))
	(goto-char (point-max))
	(ignore-errors (insert-file-contents (expand-file-name "info" dir)))
        (when (re-search-forward (rx "present:" (+ space) "yes" eol) nil t)
          (when (re-search-forward (rx "design capacity:" (+ space)
                                       battery--acpi-capacity eol)
				   nil t)
	    (cl-incf design-capacity (string-to-number (match-string 1))))
          (when (re-search-forward (rx "last full capacity:" (+ space)
                                       battery--acpi-capacity eol)
				   nil t)
	    (cl-incf last-full-capacity (string-to-number (match-string 1))))
          (when (re-search-forward (rx "design capacity warning:" (+ space)
                                       battery--acpi-capacity eol)
                                   nil t)
	    (cl-incf warn (string-to-number (match-string 1))))
          (when (re-search-forward (rx "design capacity low:" (+ space)
                                       battery--acpi-capacity eol)
				   nil t)
	    (cl-incf low (string-to-number (match-string 1)))))))
    (setq full-capacity (if (> last-full-capacity 0)
			    last-full-capacity design-capacity))
    (and capacity rate
	 (setq minutes (if (zerop rate) 0
			 (floor (* (if (string= charging-state
						"charging")
				       (- full-capacity capacity)
				     capacity)
				   60)
				rate))
	       hours (/ minutes 60)))
    (list (cons ?c (if capacity (number-to-string capacity) "N/A"))
	  (cons ?L (or (battery-search-for-one-match-in-files
                        (mapcar (lambda (d) (expand-file-name "state" d))
                                (battery--files "/proc/acpi/ac_adapter/"))
                        (rx "state:" (+ space) (group (not space) (* nonl)) eol)
                        1)
		       "N/A"))
	  (cons ?d (or (battery-search-for-one-match-in-files
                        (mapcar (lambda (d) (expand-file-name "temperature" d))
                                (battery--files "/proc/acpi/thermal_zone/"))
                        (rx "temperature:" (+ space) (group (+ digit)) " C" eol)
                        1)
		       "N/A"))
          (cons ?r (if rate
                       (concat (number-to-string rate) " " rate-type)
                     "N/A"))
	  (cons ?B (or charging-state "N/A"))
          (cons ?b (cond ((string= charging-state "charging") "+")
                         ((and capacity (< capacity low)) "!")
                         ((and capacity (< capacity warn)) "-")
                         ("")))
          (cons ?h (if hours (number-to-string hours) "N/A"))
          (cons ?m (if minutes (number-to-string minutes) "N/A"))
          (cons ?t (if minutes (format "%d:%02d" hours (% minutes 60)) "N/A"))
          (cons ?p (if (and full-capacity capacity (> full-capacity 0))
                       (number-to-string (floor (* 100 capacity) full-capacity))
                     "N/A")))))


;;; `/sys/class/power_supply/BATN' interface for Linux.

(defun battery-linux-sysfs ()
  "Get sysfs status information from Linux kernel.
This function works only with the new `/sys/class/power_supply/'
interface introduced in Linux version 2.4.25.

The following %-sequences are provided:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%L Power source (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%p Battery load percentage
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let (;; Some batteries report charges and current, others energy and power.
        ;; In order to reliably be able to combine those data, we convert them
        ;; all to energy/power (since we can't combine different charges if
        ;; they're not at the same voltage).
	(energy-full 0.0)
	(energy-now 0.0)
	(power-now 0.0)
        (voltage-now 10.8) ; Arbitrary default, in case the info is missing.
        charging-state temperature hours percentage-now)
    ;; SysFS provides information about each battery present in the
    ;; system in a separate subdirectory.  We are going to merge the
    ;; available information together.
    (with-temp-buffer
      (dolist (dir (battery--find-linux-sysfs-batteries))
        (ignore-errors
          (insert-file-contents (expand-file-name "uevent" dir) nil nil nil t))
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
		     (* (string-to-number (match-string 2))
			(if (eq (char-after (match-beginning 1)) ?C)
			    voltage-now 1))))
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
                  ((and (goto-char (point-min))
			(re-search-forward
			 "POWER_SUPPLY_ENERGY_FULL=\\([0-9]*\\)$" nil t)
			(setq full-string (match-string 1))
			(re-search-forward
			 "POWER_SUPPLY_ENERGY_NOW=\\([0-9]*\\)$" nil t)
			(setq now-string (match-string 1)))
		   (cl-incf energy-full (string-to-number full-string))
		   (cl-incf energy-now  (string-to-number now-string)))))
	  (unless (zerop power-now)
	    (let ((remaining (if (string= charging-state "Discharging")
				 energy-now
			       (- energy-full energy-now))))
	      (setq hours (/ remaining power-now)))))))
    (when (and (> energy-full 0) (> energy-now 0))
      (setq percentage-now (/ (* 100 energy-now) energy-full)))
    (list (cons ?c (if (or (> energy-full 0) (> energy-now 0))
                       (number-to-string (/ energy-now voltage-now))
                     "N/A"))
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
          (cons ?b (cond ((string= charging-state "Charging") "+")
                         ((not percentage-now) "")
                         ((< percentage-now battery-load-critical) "!")
                         ((< percentage-now battery-load-low) "-")
                         ("")))
          (cons ?p (if percentage-now (format "%.1f" percentage-now) "N/A"))
          (cons ?L (pcase (battery-search-for-one-match-in-files
                           '("/sys/class/power_supply/AC/online"
                             "/sys/class/power_supply/ACAD/online"
                             "/sys/class/power_supply/ADP1/online")
                           (rx (in "01")) 0)
                     ("0" "BAT")
                     ("1" "AC")
                     (_ "N/A"))))))


;;; UPower interface.

(defconst battery-upower-interface "org.freedesktop.UPower"
  "Name of the UPower D-Bus interface.
See URL `https://upower.freedesktop.org/docs/UPower.html'.")

(defconst battery-upower-path "/org/freedesktop/UPower"
  "D-Bus object providing `battery-upower-interface'.")

(defconst battery-upower-device-interface "org.freedesktop.UPower.Device"
  "Name of the UPower Device D-Bus interface.
See URL `https://upower.freedesktop.org/docs/Device.html'.")

(defconst battery-upower-device-path "/org/freedesktop/UPower/devices"
  "D-Bus object providing `battery-upower-device-interface'.")

(defvar battery--upower-signals nil
  "Handles for UPower signal subscriptions.")

(defun battery--upower-signal-handler (&rest _)
  "Update battery status on receiving a UPower D-Bus signal."
  (timer-event-handler battery-update-timer))

(defun battery--upower-props-changed (_interface changed _invalidated)
  "Update status when system starts/stops running on battery.
Intended as a UPower PropertiesChanged signal handler."
  (when (assoc "OnBattery" changed)
    (battery--upower-signal-handler)))

(defun battery--upower-unsubscribe ()
  "Unsubscribe from UPower device change signals."
  (mapc #'dbus-unregister-object battery--upower-signals)
  (setq battery--upower-signals ()))

(defun battery--upower-subscribe ()
  "Subscribe to UPower device change signals."
  (push (dbus-register-signal :system battery-upower-service
                              battery-upower-path
                              dbus-interface-properties
                              "PropertiesChanged"
                              #'battery--upower-props-changed)
        battery--upower-signals)
  (dolist (method '("DeviceAdded" "DeviceRemoved"))
    (push (dbus-register-signal :system battery-upower-service
                                battery-upower-path
                                battery-upower-interface
                                method #'battery--upower-signal-handler)
          battery--upower-signals)))

(defun battery--upower-device-properties (device)
  "Return value for all available properties for the UPower DEVICE."
  (dbus-get-all-properties
   :system battery-upower-service
   (expand-file-name device battery-upower-device-path)
   battery-upower-device-interface))

(defun battery--upower-devices ()
  "List all UPower devices according to `battery-upower-device'."
  (cond ((stringp battery-upower-device)
         (list battery-upower-device))
        (battery-upower-device)
        ((dbus-ignore-errors
           (dbus-call-method :system battery-upower-service
                             battery-upower-path
                             battery-upower-interface
                             "EnumerateDevices"
                             :timeout 1000)))))

(defun battery--upower-state (props state)
  "Merge the UPower battery state in PROPS with STATE.
This is an extension of the UPower DisplayDevice algorithm for
merging multiple battery states into one.  PROPS is an alist of
battery properties from `battery-upower-device-interface', and
STATE is a symbol representing the state to merge with."
  ;; Map UPower enum into our printable symbols.
  (let* ((new (pcase (cdr (assoc "State" props))
                (1 'charging)
                (2 'discharging)
                (3 'empty)
                (4 'fully-charged)
                (5 'pending-charge)
                (6 'pending-discharge)))
         ;; Unknown state represented by nil.
         (either (delq nil (list new state))))
    ;; Earlier states override later ones.
    (car (cond ((memq 'charging either))
               ((memq 'discharging either))
               ((memq 'pending-charge either))
               ((memq 'pending-discharge either))
               ;; Only options left are full or empty,
               ;; but if they conflict return nil.
               ((null (cdr either)) either)
               ((apply #'eq either) either)))))

(defun battery-upower ()
  "Get battery status from UPower D-Bus interface.
This function works only in systems that provide a UPower D-Bus
service.

The following %-sequences are provided:
%c Current capacity (mWh)
%r Current rate of charge or discharge
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let ((count 0) props type line-status state load temperature
        secs mins hrs total-energy total-rate total-tte total-ttf)
    ;; Merge information from all available or specified UPower
    ;; devices like other `battery-status-function's.
    (dolist (device (battery--upower-devices))
      (setq props (battery--upower-device-properties device))
      (setq type (cdr (assoc "Type" props)))
      (cond
       ((and (eq type 1) (not (eq line-status 'online)))
        ;; It's a line power device: `online' if currently providing
        ;; power, any other non-nil value if simply present.
        (setq line-status (if (cdr (assoc "Online" props)) 'online t)))
       ((and (eq type 2) (cdr (assoc "IsPresent" props)))
        ;; It's a battery.
        (setq count (1+ count))
        (setq state (battery--upower-state props state))
        (let ((energy  (cdr (assoc "Energy"      props)))
              (rate    (cdr (assoc "EnergyRate"  props)))
              (percent (cdr (assoc "Percentage"  props)))
              (temp    (cdr (assoc "Temperature" props)))
              (tte     (cdr (assoc "TimeToEmpty" props)))
              (ttf     (cdr (assoc "TimeToFull"  props))))
          (when energy  (setq total-energy (+ (or total-energy 0) energy)))
          (when rate    (setq total-rate   (+ (or total-rate   0) rate)))
          (when percent (setq load         (+ (or load         0) percent)))
          (when temp    (setq temperature  (+ (or temperature  0) temp)))
          (when tte     (setq total-tte    (+ (or total-tte    0) tte)))
          (when ttf     (setq total-ttf    (+ (or total-ttf    0) ttf)))))))
    (when (> count 1)
      ;; Averages over multiple batteries.
      (when load (setq load (/ load count)))
      (when temperature (setq temperature (/ temperature count))))
    (when (setq secs (if (eq line-status 'online) total-ttf total-tte))
      (setq mins (/ secs 60))
      (setq hrs (/ secs 3600)))
    (list (cons ?c (if total-energy
                       (format "%.0f" (* total-energy 1000))
                     "N/A"))
          (cons ?r (if total-rate (format "%.1f W" total-rate) "N/A"))
          (cons ?L (cond ((eq line-status 'online) "on-line")
                         (line-status "off-line")
                         ("N/A")))
          (cons ?B (format "%s" (or state 'unknown)))
          (cons ?b (cond ((eq state 'charging) "+")
                         ((and load (< load battery-load-critical)) "!")
                         ((and load (< load battery-load-low)) "-")
                         ("")))
          ;; Zero usually means unknown.
          (cons ?d (if (and temperature (/= temperature 0))
                       (format "%.0f" temperature)
                     "N/A"))
          (cons ?p (if load (format "%.0f" load) "N/A"))
          (cons ?s (if secs (number-to-string secs) "N/A"))
          (cons ?m (if mins (number-to-string mins) "N/A"))
          (cons ?h (if hrs (number-to-string hrs) "N/A"))
          (cons ?t (if hrs (format "%d:%02d" hrs (% mins 60)) "N/A")))))


;;; `apm' interface for BSD.

(defun battery-bsd-apm ()
  "Get APM status information from BSD apm binary.
The following %-sequences are provided:
%P Advanced power saving mode state (verbose)
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let* ((os-name (car (split-string
                        ;; FIXME: Can't we use something like `system-type'?
                        (shell-command-to-string "/usr/bin/uname"))))
         (apm-flag (pcase os-name
                     ("OpenBSD" "mP")
                     ("FreeBSD" "st")
                     (_         "ms")))
         (apm-cmd (concat "/usr/sbin/apm -abl" apm-flag))
         (apm-output (split-string (shell-command-to-string apm-cmd)))
         (indices (pcase os-name
                    ;; FreeBSD's manpage documents that multiple
                    ;; outputs are ordered by "the order in which
                    ;; they're listed in the manpage", which is alphabetical
                    ;; and is also the order in which we pass them.
                    ("FreeBSD" '((ac . 0)
                                 (battery-status . 1)
                                 (battery-percent . 2)
                                 (apm-mode . 3)
                                 (battery-life . 4)))
                    ;; For NetBSD and OpenBSD, the manpage doesn't document
                    ;; the order.  The previous code used this order, so let's
                    ;; assume it's right.
                    (_         '((ac . 3)
                                 (battery-status . 0)
                                 (battery-percent . 1)
                                 (apm-mode . 4)
                                 (battery-life . 2)))))
         ;; Battery status
         (battery-status
          (pcase (string-to-number
                  (nth (alist-get 'battery-status indices) apm-output))
            (0 '("high" . ""))
            (1 '("low" . "-"))
            (2 '("critical" . "!"))
            (3 '("charging" . "+"))
            (4 '("absent" . nil))))
         ;; Battery percentage
         (battery-percentage
          (nth (alist-get 'battery-percent indices) apm-output))
         ;; Battery life
         (battery-life (nth (alist-get 'battery-life indices) apm-output))
         ;; AC status
         (line-status
          (pcase (string-to-number (nth (alist-get 'ac indices) apm-output))
            (0 "disconnected")
            (1 "connected")
            (2 "backup power")))
         ;; Advanced power savings mode
         (apm-mode
          (let ((apm (string-to-number
                      (nth (alist-get 'apm-mode indices) apm-output))))
            (if (string= os-name "OpenBSD")
                (pcase apm
                  (0 "manual")
                  (1 "automatic")
		  (2 "cool running"))
	      (if (eql apm 1) "on" "off"))))
	 seconds minutes hours remaining-time)
    (unless (member battery-life '("unknown" "-1"))
      (if (member os-name '("OpenBSD" "NetBSD"))
	  (setq minutes (string-to-number battery-life)
		seconds (* 60 minutes))
	(setq seconds (string-to-number battery-life)
	      minutes (truncate seconds 60)))
      (setq hours (truncate minutes 60)
            remaining-time (format "%d:%02d" hours (% minutes 60))))
    (list (cons ?L (or line-status "N/A"))
	  (cons ?B (or (car battery-status) "N/A"))
	  (cons ?b (or (cdr battery-status) "N/A"))
	  (cons ?p (if (string= battery-percentage "255")
		       "N/A"
		     battery-percentage))
	  (cons ?P (or apm-mode "N/A"))
          (cons ?s (if seconds (number-to-string seconds) "N/A"))
          (cons ?m (if minutes (number-to-string minutes) "N/A"))
          (cons ?h (if hours (number-to-string hours) "N/A"))
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
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let ( power-source load-percentage battery-status battery-status-symbol
         remaining-time hours minutes )
    (with-temp-buffer
      (ignore-errors (call-process "pmset" nil t nil "-g" "ps"))
      (goto-char (point-min))
      (when (re-search-forward ;; Handle old typo in output.
             "\\(?:Currentl?y\\|Now\\) drawing from '\\(AC\\|Battery\\) Power'"
             nil t)
	(setq power-source (match-string 1))
        (when (re-search-forward (rx bol " -InternalBattery-0" (+ space)
                                     (* "(id=" (+ digit) ")" (+ space)))
                                 nil t)
	  (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
	    (setq load-percentage (match-string 1))
	    (goto-char (match-end 0))
            (cond ((looking-at-p "; charging")
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
  (format-spec format alist 'delete))

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
