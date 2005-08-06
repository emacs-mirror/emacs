;;; battery.el --- display battery status information

;; Copyright (C) 1997, 1998, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Ralph Schleicher <rs@nunatak.allgaeu.org>
;; Keywords: hardware

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

;; There is at present support for GNU/Linux and OS X.  This library
;; supports both the `/proc/apm' file format of Linux version 1.3.58
;; or newer and the `/proc/acpi/' directory structure of Linux 2.4.20
;; and 2.6.  Darwin (OS X) is supported by using the `pmset' program.

;;; Code:

(require 'timer)
(eval-when-compile (require 'cl))


(defgroup battery nil
  "Display battery status information."
  :prefix "battery-"
  :group 'hardware)

(defcustom battery-status-function
  (cond ((and (eq system-type 'gnu/linux)
	      (file-readable-p "/proc/apm"))
	 'battery-linux-proc-apm)
	((and (eq system-type 'gnu/linux)
	      (file-directory-p "/proc/acpi/battery"))
	 'battery-linux-proc-acpi)
	((and (eq system-type 'darwin)
	      (ignore-errors 
		(with-temp-buffer 
		  (and (eq (call-process "pmset" nil t nil "-g" "ps") 0)
		       (> (buffer-size) 0)))))
	 'battery-pmset))
  "*Function for getting battery status information.
The function has to return an alist of conversion definitions.
Its cons cells are of the form

    (CONVERSION . REPLACEMENT-TEXT)

CONVERSION is the character code of a \"conversion specification\"
introduced by a `%' character in a control string."
  :type '(choice (const nil) function)
  :group 'battery)

(defcustom battery-echo-area-format
  (cond ((eq battery-status-function 'battery-linux-proc-apm)
	 "Power %L, battery %B (%p%% load, remaining time %t)")
	((eq battery-status-function 'battery-linux-proc-acpi)
	 "Power %L, battery %B at %r (%p%% load, remaining time %t)")
	((eq battery-status-function 'battery-pmset)
	 "%L power, battery %B (%p%% load, remaining time %t)"))
  "*Control string formatting the string to display in the echo area.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'."
  :type '(choice string (const nil))
  :group 'battery)

(defvar battery-mode-line-string nil
  "String to display in the mode line.")
;;;###autoload (put 'battery-mode-line-string 'risky-local-variable t)

(defcustom battery-mode-line-format
  (cond ((eq battery-status-function 'battery-linux-proc-apm)
	 "[%b%p%%]")
	((eq battery-status-function 'battery-linux-proc-acpi)
	 "[%b%p%%,%d°C]")
	((eq battery-status-function 'battery-pmset)
	 "[%b%p%%]"))
  "*Control string formatting the string to display in the mode line.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'."
  :type '(choice string (const nil))
  :group 'battery)

(defcustom battery-update-interval 60
  "*Seconds after which the battery status will be updated."
  :type 'integer
  :group 'battery)

(defcustom battery-load-low 25
  "*Upper bound of low battery load percentage.
A battery load percentage below this number is considered low."
  :type 'integer
  :group 'battery)

(defcustom battery-load-critical 10
  "*Upper bound of critical battery load percentage.
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
  "Display battery status information in the mode line.
The text being displayed in the mode line is controlled by the variables
`battery-mode-line-format' and `battery-status-function'.
The mode line will be updated automatically every `battery-update-interval'
seconds."
  :global t :group 'battery
  (setq battery-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and battery-update-timer (cancel-timer battery-update-timer))
  (if (not display-battery-mode)
      (setq global-mode-string
	    (delq 'battery-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'battery-mode-line-string t)
    (setq battery-update-timer (run-at-time nil battery-update-interval
					    'battery-update-handler))
    (battery-update)))

(defun battery-update-handler ()
  (battery-update)
  (sit-for 0))

(defun battery-update ()
  "Update battery status information in the mode line."
  (setq battery-mode-line-string
	(propertize (if (and battery-mode-line-format
			     battery-status-function)
			(battery-format
			 battery-mode-line-format
			 (funcall battery-status-function))
		      "")
		    'help-echo "Battery status information"))
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
  "Get APM status information from Linux kernel.
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
%s Remaining time in seconds
%m Remaining time in minutes
%h Remaining time in hours
%t Remaining time in the form `h:min'"
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
  "Get ACPI status information from Linux kernel.
This function works only with the new `/proc/acpi/' format introduced
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
%m Remaining time in minutes
%h Remaining time in hours
%t Remaining time in the form `h:min'"
  (let ((design-capacity 0)
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
	       (member charging-state '("unknown" nil))
	       ;; On most multi-battery systems, most of the time only one
	       ;; battery is "charging"/"discharging", the others are
	       ;; "unknown".
	       (setq charging-state (match-string 1)))
	  (when (re-search-forward "present rate: +\\([0-9]+\\) \\(m[AW]\\)$"
				   nil t)
	    (setq rate (+ (or rate 0) (string-to-number (match-string 1)))
		  rate-type (or (and rate-type
				     (if (string= rate-type (match-string 2))
					 rate-type
				       (error
					"Inconsistent rate types (%s vs. %s)"
					rate-type (match-string 2))))
				(match-string 2))))
	  (when (re-search-forward "remaining capacity: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (setq capacity
		  (+ (or capacity 0) (string-to-number (match-string 1))))))
	(goto-char (point-max))
	(ignore-errors (insert-file-contents (expand-file-name "info" dir)))
	(when (re-search-forward "present: +yes$" nil t)
	  (when (re-search-forward "design capacity: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (incf design-capacity (string-to-number (match-string 1))))
	  (when (re-search-forward
		 "design capacity warning: +\\([0-9]+\\) m[AW]h$" nil t)
	    (incf warn (string-to-number (match-string 1))))
	  (when (re-search-forward "design capacity low: +\\([0-9]+\\) m[AW]h$"
				   nil t)
	    (incf low (string-to-number (match-string 1)))))))
    (and capacity rate
	 (setq minutes (if (zerop rate) 0
			 (floor (* (/ (float (if (string= charging-state
							  "charging")
						 (- design-capacity capacity)
					       capacity)) rate) 60)))
	       hours (/ minutes 60)))
    (list (cons ?c (or (and capacity (number-to-string capacity)) "N/A"))
	  (cons ?L (or (when (file-exists-p "/proc/acpi/ac_adapter/AC/state")
			 (with-temp-buffer
			   (insert-file-contents
			    "/proc/acpi/ac_adapter/AC/state")
			   (when (re-search-forward "state: +\\(.*\\)$" nil t)
			     (match-string 1))))
		       "N/A"))
	  (cons ?d (or (when (file-exists-p
			      "/proc/acpi/thermal_zone/THRM/temperature")
			 (with-temp-buffer
			   (insert-file-contents
			    "/proc/acpi/thermal_zone/THRM/temperature")
			   (when (re-search-forward
				  "temperature: +\\([0-9]+\\) C$" nil t)
			     (match-string 1))))
		       (when (file-exists-p
			      "/proc/acpi/thermal_zone/THM/temperature")
			 (with-temp-buffer
			   (insert-file-contents
			    "/proc/acpi/thermal_zone/THM/temperature")
			   (when (re-search-forward
				  "temperature: +\\([0-9]+\\) C$" nil t)
			     (match-string 1))))
		       "N/A"))
	  (cons ?r (or (and rate (concat (number-to-string rate) " "
					 rate-type)) "N/A"))
	  (cons ?B (or charging-state "N/A"))
	  (cons ?b (or (and (string= charging-state "charging") "+")
		       (and (< capacity low) "!")
		       (and (< capacity warn) "-")
		       ""))
	  (cons ?h (or (and hours (number-to-string hours)) "N/A"))
	  (cons ?m (or (and minutes (number-to-string minutes)) "N/A"))
	  (cons ?t (or (and minutes
			    (format "%d:%02d" hours (- minutes (* 60 hours))))
		       "N/A"))
	  (cons ?p (or (and design-capacity capacity
			    (number-to-string
			     (floor (/ capacity
				       (/ (float design-capacity) 100)))))
		       "N/A")))))


;;; `pmset' interface for Darwin (OS X).

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
      (when (re-search-forward "Currentl?y drawing from '\\(AC\\|Battery\\) Power'" nil t)
	(setq power-source (match-string 1))
	(when (re-search-forward "^ -InternalBattery-0[ \t]+" nil t)
	  (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
	    (setq load-percentage (match-string 1))
	    (goto-char (match-end 0))
	    (cond ((looking-at "; charging")
		   (setq battery-status "charging"
			 battery-status-symbol "+"))
		  ((< (string-to-number load-percentage) battery-load-low)
		   (setq battery-status "low"
			 battery-status-symbol "-"))
		  ((< (string-to-number load-percentage) battery-load-critical)
		   (setq battery-status "critical"
			 battery-status-symbol "!"))
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


(provide 'battery)

;; arch-tag: 65916f50-4754-4b6b-ac21-0b510f545a37
;;; battery.el ends here
