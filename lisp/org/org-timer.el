;;; org-clock.el --- The time clocking code for Org-mode

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.16
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the relative timer code for Org-mode

(require 'org)

(defvar org-timer-start-time nil
  "t=0 for the running timer.")

(defconst org-timer-re "\\([-+]?[0-9]+\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
  "Regular expression used to match timer stamps.")

(defcustom org-timer-format "%s "
  "The format to insert the time of the timer.
This format must contain one instance of \"%s\" which will be replaced by
the value of the relative timer."
  :group 'org-time
  :type 'string)

;;;###autoload
(defun org-timer-start (&optional offset)
  "Set the starting time for the relative timer to now.
When called with prefix argument OFFSET, prompt the user for an offset time,
with the default taken from a timer stamp at point, if any.
If OFFSET is a string or an integer, it is directly taken to be the offset
without user interaction.
When called with a double prefix arg, all timer strings in the active
region will be shifted by a specific amount.  You will be prompted for
the amount, with the default to make the first timer string in
the region 0:00:00."
  (interactive "P")
  (if (equal offset '(16))
      (call-interactively 'org-timer-change-times-in-region)
    (let (delta def s)
      (if (not offset)
	  (setq org-timer-start-time (current-time))
	(cond
	 ((integerp offset) (setq delta offset))
	 ((stringp offset) (setq delta (org-timer-hms-to-secs offset)))
	 (t
	  (setq def (if (org-in-regexp org-timer-re)
			(match-string 0)
		      "0:00:00")
		s (read-string
		   (format "Restart timer with offset [%s]: " def)))
	  (unless (string-match "\\S-" s) (setq s def))
	  (setq delta (org-timer-hms-to-secs (org-timer-fix-incomplete s)))))
	(setq org-timer-start-time
	      (seconds-to-time
	       (-
		(time-to-seconds (current-time))
		(org-timer-hms-to-secs s)))))
      (message "Timer start time set to %s, current value is %s"
	       (format-time-string "%T" org-timer-start-time)
	       (org-timer-secs-to-hms (or delta 0))))))

;;;###autoload
(defun org-timer (&optional restart)
  "Insert a H:MM:SS string from the timer into the buffer.
The first time this command is used, the timer is started.  When used with
a `C-u' prefix, force restarting the timer.
When used with a double prefix arg `C-u C-u', change all the timer string
in the region by a fixed amount.  This can be used to recalibrate a timer
that was not started at the correct moment."
  (interactive "P")
  (if (equal restart '(4)) (org-timer-start))
  (or org-timer-start-time (org-timer-start))
  (insert (format
	   org-timer-format
	   (org-timer-secs-to-hms
	    (floor
	     (- (time-to-seconds (current-time))
		(time-to-seconds org-timer-start-time)))))))

;;;###autoload
(defun org-timer-change-times-in-region (beg end delta)
  "Change all h:mm:ss time in region by a DELTA."
  (interactive
   "r\nsEnter time difference like \"-1:08:26\". Default is first time to zero: ")
  (let ((re "[-+]?[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}") p)
    (unless (string-match "\\S-" delta)
      (save-excursion
	(goto-char beg)
	(when (re-search-forward re end t)
	  (setq delta (match-string 0))
	  (if (equal (string-to-char delta) ?-)
	      (setq delta (substring delta 1))
	    (setq delta (concat "-" delta))))))
    (setq delta (org-timer-hms-to-secs (org-timer-fix-incomplete delta)))
    (when (= delta 0) (error "No change"))
    (save-excursion
      (goto-char end)
      (while (re-search-backward re beg t)
	(setq p (point))
	(replace-match
	 (save-match-data
	   (org-timer-secs-to-hms (+ (org-timer-hms-to-secs (match-string 0)) delta)))
	 t t)
	(goto-char p)))))

;;;###autoload
(defun org-timer-item (&optional arg)
  "Insert a description-type item with the current timer value."
  (interactive "P")
  (let ((ind 0))
    (save-excursion
      (skip-chars-backward " \n\t")
      (condition-case nil
	  (progn
	    (org-beginning-of-item)
	    (setq ind (org-get-indentation)))
	(error nil)))
    (or (bolp) (newline))
    (org-indent-line-to ind)
    (insert "- ")
    (org-timer (if arg '(4)))
    (insert ":: ")))

(defun org-timer-fix-incomplete (hms)
  "If hms is a H:MM:SS string with missing hour or hour and minute, fix it."
  (if (string-match "\\(?:\\([0-9]+:\\)?\\([0-9]+:\\)\\)?\\([0-9]+\\)" hms)
      (replace-match
       (format "%d:%02d:%02d"
	       (if (match-end 1) (string-to-number (match-string 1 hms)) 0)
	       (if (match-end 2) (string-to-number (match-string 2 hms)) 0)
	       (string-to-number (match-string 3 hms)))
       t t hms)
    (error "Cannot parse HMS string \"%s\"" hms)))

(defun org-timer-hms-to-secs (hms)
  "Convert h:mm:ss string to an integer time.
If the string starts with a minus sign, the integer will be negative."
  (if (not (string-match
	    "\\([-+]?[0-9]+\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
	    hms))
      0
    (let* ((h (string-to-number (match-string 1 hms)))
	   (m (string-to-number (match-string 2 hms)))
	   (s (string-to-number (match-string 3 hms)))
	   (sign (equal (substring (match-string 1 hms) 0 1) "-")))
      (setq h (abs h))
      (* (if sign -1 1) (+ s (* 60 (+ m (* 60 h))))))))

(defun org-timer-secs-to-hms (s)
  "Convert integer S into h:mm:ss.
If the integer is negative, the string will start with \"-\"."
  (let (sign m h)
    (setq sign (if (< s 0) "-" "")
	  s (abs s)
	  m (/ s 60) s (- s (* 60 m))
	  h (/ m 60) m (- m (* 60 h)))
    (format "%s%d:%02d:%02d" sign h m s)))

;; arch-tag: 97538f8c-3871-4509-8f23-1e7b3ff3d107

;;; org-timer.el ends here
