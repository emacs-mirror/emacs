;;; battery.el --- display battery status information.

;; Copyright (C) 1997, 1998 Free Software Foundation, Inc.

;; Author: Ralph Schleicher <rs@purple.UL.BaWue.DE>
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; There is at present only a function interpreting the new `/proc/apm'
;; file format of Linux version 1.3.58 or newer.  That is, what a lucky
;; coincidence, exactly the interface provided by the author's labtop.

;;; Code:

(require 'timer)



(defgroup battery nil
  "Display battery status information."
  :prefix "battery-"
  :group 'hardware)

(defcustom battery-status-function
  (cond ((and (eq system-type 'gnu/linux)
	      (file-readable-p "/proc/apm"))
	 'battery-linux-proc-apm))  
  "*Function for getting battery status information.
The function have to return an alist of conversion definitions.
Cons cells are of the form

    (CONVERSION . REPLACEMENT-TEXT)

CONVERSION is the character code of a \"conversion specification\"
introduced by a `%' character in a control string."
  :type 'function
  :group 'battery)

(defcustom battery-echo-area-format
  (cond ((eq battery-status-function 'battery-linux-proc-apm)
	 "Power %L, battery %B (%p%% load, remaining time %t)"))
  "*Control string formatting the string to display in the echo area.
Ordinary characters in the control string are printed as-is, while
conversion specifications introduced by a `%' character in the control
string are substituted as defined by the current value of the variable
`battery-status-function'."
  :type '(choice string (const nil))
  :group 'battery)

(defvar battery-mode-line-string nil
  "String to display in the mode line.")

(defcustom battery-mode-line-format
  (cond ((eq battery-status-function 'battery-linux-proc-apm)
	 " [%b%p%%]"))
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
(defun display-battery ()
  "Display battery status information in the mode line.
The text beeing displayed in the mode line is controlled by the variables
`battery-mode-line-format' and `battery-status-function'.
The mode line will be updated automatically every `battery-update-interval'
seconds."
  (interactive)
  (setq battery-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'battery-mode-line-string global-mode-string)
      (setq global-mode-string (append global-mode-string
				       '(battery-mode-line-string))))
  (and battery-update-timer (cancel-timer battery-update-timer))
  (setq battery-update-timer (run-at-time nil battery-update-interval
					  'battery-update-handler))
  (battery-update))

(defun battery-update-handler ()
  (battery-update)
  (sit-for 0))

(defun battery-update ()
  "Update battery status information in the mode line."
  (setq battery-mode-line-string (if (and battery-mode-line-format
					  battery-status-function)
				     (battery-format
				      battery-mode-line-format
				      (funcall battery-status-function))
				   ""))
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
%p battery load percentage
%s Remaining time in seconds
%m Remaining time in minutes
%h Remaining time in hours
%t Remaining time in the form `h:min'"
  (let (driver-version bios-version bios-interface line-status
	battery-status battery-status-symbol load-percentage
	seconds minutes hours remaining-time buffer tem)
    (unwind-protect
	(save-excursion
	  (setq buffer (get-buffer-create " *battery*"))
	  (set-buffer buffer)
	  (erase-buffer)
	  (battery-insert-file-contents "/proc/apm")
	  (re-search-forward battery-linux-proc-apm-regexp)
	  (setq driver-version (match-string 1))
	  (setq bios-version (match-string 2))
	  (setq tem (battery-hex-to-int-2 (match-string 3)))
	  (if (not (logand tem 2))
	      (setq bios-interface "not supported")
	    (setq bios-interface "enabled")
	    (cond ((logand tem 16) (setq bios-interface "disabled"))
		  ((logand tem 32) (setq bios-interface "disengaged")))
	    (setq tem (battery-hex-to-int-2 (match-string 4)))
	    (cond ((= tem 0) (setq line-status "off-line"))
		  ((= tem 1) (setq line-status "on-line"))
		  ((= tem 2) (setq line-status "on backup")))
	    (setq tem (battery-hex-to-int-2 (match-string 6)))
	    (if (= tem 255)
		(setq battery-status "N/A")
	      (setq tem (battery-hex-to-int-2 (match-string 5)))
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


;;; Private functions.

(defun battery-format (format alist)
  "Substitute %-sequences in FORMAT."
  (let ((index 0)
	(length (length format))
	(result "")
	char flag elem)
    (while (< index length)
      (setq char (aref format index))
      (if (not flag)
	  (if (char-equal char ?%)
	      (setq flag t)
	    (setq result (concat result (char-to-string char))))
	(cond ((char-equal char ?%)
	       (setq result (concat result "%")))
	      ((setq elem (assoc char alist))
	       (setq result (concat result (cdr elem)))))
	(setq flag nil))
      (setq index (1+ index)))
    (or (null flag)
	(setq result (concat result "%")))
    result))

(defun battery-insert-file-contents (file-name)
  "Insert contents of file FILE-NAME after point.
FILE-NAME can be a non-ordinary file, for example, a named pipe.
Return t if file exists."
  (let ((load-read-function 'battery-read-function)
	(load-source-file-function nil)
	(load-path '("."))
	(load-history nil))
    (save-excursion
      (load file-name nil t t))))

(defun battery-read-function (&optional stream)
  "Function for reading expressions from STREAM.
Value is always nil."
  (let (char)
    (while (not (< (setq char (get-file-char)) 0))
      (insert char))))

(defconst battery-hex-map '((?0 .  0) (?1 .  1) (?2 .  2) (?3 .  3)
			    (?4 .  4) (?5 .  5) (?6 .  6) (?7 .  7)
			    (?8 .  8) (?9 .  9) (?a . 10) (?b . 11)
			    (?c . 12) (?d . 13) (?e . 14) (?f . 15)))

(defun battery-hex-to-int (string)
  "Convert a hexadecimal number (a string) into a number."
  (save-match-data
    (and (string-match "^[ \t]+" string)
	 (setq string (substring string (match-end 0))))
    (and (string-match "^0[xX]" string)
	 (setq string (substring string (match-end 0)))))
  (battery-hex-to-int-2 string))

(defun battery-hex-to-int-2 (string)
  (let ((index 0)
	(length (length string))
	(value 0)
	(elem nil))
    (while (and (< index length)
		(setq elem (assoc (downcase (aref string index))
				  battery-hex-map)))
      (setq value (+ (* 16 value) (cdr elem))
	    index (1+ index)))
    value))


(provide 'battery)

;;; battery.el ends here
