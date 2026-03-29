;;; Receive and execute Lisp code submitted by a test controller.  -*- lexical-binding: t; -*-
;;; $Id: ats-driver.el,v 1.9 2025/03/02 12:52:57 jw Exp $

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file establishes a connection to a controlling device, executes
;; Lisp expressions received from the same, and responds with any
;; results available.
;;
;; There were anciently many more facilities in this file but they are
;; in the process of being moved to `test-controller.el' (now in Lisp).

;;; Code:



;; Connection establishment and management.

(defvar ats-process nil
  "Connection to the test controller.")

(defvar ats-connection-established nil
  "Whether `ats-process' has been initialized.")

(defface ats-header '((t :height 1.3 :weight bold
			 :inherit variable-pitch))
  "Face of ATS header elements.")

(defvar ats-in-eval nil
  "Whether an `-eval' command is being processed and the form's size.")

(defvar ats-eval-as-printed nil
  "Whether to return the values of the submitted form as a string.")

(defvar ats-eval-serial nil
  "Serial number identifying this result.")

(defvar ats-eval-do-decode nil
  "Whether to decode the form provided as utf-8-emacs.")

(defvar ats-executing-form nil
  "Bound to `true' when executing a submitted form.")

(defun ats-process-filter (process string)
  "Filter input from `ats-process'.
Insert STRING into the connection buffer, till a full command is
read."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((marker (process-mark process)))
	(unless (marker-position marker)
	  (set-marker marker (point)))
	(save-excursion
	  (goto-char marker)
	  (insert string)
	  (set-marker marker (point))))
      (let ((firstchar (char-after (point-min)))
	    (inhibit-quit nil)
	    (in-eval ats-in-eval))
	(while (or (eq firstchar ?-) in-eval)
	  (unless ats-in-eval
	    (when (eq firstchar ?-)
	      ;; A command is being delivered.  Search for a newline.
	      (save-excursion
		(when-let* ((newline (search-forward "\n" nil t))
			    (command (buffer-substring
				      (point-min) (1- newline))))
		  (delete-region (point-min) newline)
		  (cond
		   ((equal command "-ok")
		    (setq ats-connection-established t)
		    (ats-display-status-buffer))
		   ((equal command "-not-accepting-connections")
		    (error
		     "The server is not accepting connections"))
		   ((string-match
		     "^-incorrect-uuid \\([[:alnum:]-]\\) \\([[:alnum:]-]\\)$"
		     command)
		    (error "Connection rejected; wanted ID=%s, received ID=%s"
			   (match-string 2 command) (match-string 1 command)))
		   ((string-match
		     "^-eval \\([[:digit:]]+\\) \\([[:digit:]]+\\) \\(t\\|nil\\) \\(t\\|nil\\)$"
		     command)
		    (setq ats-eval-serial (string-to-number
					   (match-string 1 command))
			  ats-in-eval (string-to-number
				       (match-string 2 command))
			  ats-eval-as-printed (equal
					       (match-string 3 command)
					       "t")
			  ats-eval-do-decode (equal
					      (match-string 4 command)
					      "t")))
		   (t (error (concat "Unknown command: " command))))))))
	  (when ats-in-eval
	    ;; Proceed till `ats-in-eval' characters are read.
	    (when (>= (- (point-max) (point-min)) ats-in-eval)
	      (unwind-protect
		  (let ((value
			 (save-restriction
			   (narrow-to-region (point-min) (1+ ats-in-eval))
			   (condition-case err
			       (let* ((str (buffer-string)))
				 (with-current-buffer "*ATS*"
				   (goto-char (point-max))
				   (let ((inhibit-read-only t))
				     (insert "--> " (truncate-string-to-width
						     str 256)
					     "\n")))
				 (let* ((str (if ats-eval-do-decode
						 (decode-coding-string
						  str 'utf-8-emacs t)
					       str))
					(expr (car (read-from-string str)))
					(value (let ((ats-executing-form t))
						 (eval expr))))
				   (cons 'ok value)))
			     (t (cons 'error err))))))
		    (let* ((print-escape-control-characters t)
			   (print-escape-newlines t)
			   (str (encode-coding-string
				 (prin1-to-string value) 'utf-8-emacs t)))
		      (if ats-eval-as-printed
			  (let* ((quoted (prin1-to-string str)))
			    (process-send-string
			     process (format "\fats-request:%d %d\n"
					     ats-eval-serial
					     (length quoted)))
			    (process-send-string process quoted))
			(process-send-string
			 process (format "\fats-request:%d %d\n"
					 ats-eval-serial
					 (length str)))
			(process-send-string process str)))
		    (process-send-string process "\n"))
		(delete-region (point-min)
			       (+ (point-min) ats-in-eval))
		(setq ats-in-eval nil))))
	  ;; Don't loop if the form data is yet to arrive.
	  (setq firstchar (char-after (point-min))
		in-eval nil))))))

(defun ats-display-status-buffer ()
  "Replace the splash screen with text announcing connection establishment."
  (with-current-buffer (get-buffer-create "*ATS*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize "ATS ready\n" 'face 'ats-header))
    (insert (propertize "
If you are reading this message, this instance of Emacs has\
 successfully established a connection with a controlling\
 machine and is patiently awaiting instructions.
"
			'face 'variable-pitch))
    (special-mode)
    (setq-local truncate-lines nil)
    (visual-line-mode 1))
  (pop-to-buffer "*ATS*" '(display-buffer-full-frame)))

(defun ats-establish-connection (host port id)
  "Connect to the test controller instance at HOST:PORT.
ID is the identifier assigned to this client.  Establish a
connection to a test controller instance through an address or a
Unix domain socket provided as aforesaid.  Signal an error upon
failure."
  (message "; Connecting to %s:%d..." host port)
  (setq ats-process (make-network-process
		     :name (format "*ats connection to %s:%d*" host port)
		     :buffer "*ats connection*"
		     :host host
		     :service port
		     :coding 'no-conversion
		     :filter #'ats-process-filter))
  (process-send-string ats-process (concat id "\n")))

(defun ats-driver-log (_ connection _)
  "Log function for ATS driver processes."
  (if ats-process
      (delete-process connection)
    (setq ats-process connection)
    (set-process-filter connection #'ats-process-filter)
    (pop-to-buffer (process-buffer connection))))

(defun ats-initiate-connection (commfile)
  "Open a network server locally to which the controller may connect.
Write its port number to COMMFILE, and await a connection from
the controller."
  (let* ((process (make-network-process :name " *ats driver*"
					:server t
					:host 'local
					:service t
					:family 'ipv4
					:coding 'no-conversion
					:log #'ats-driver-log))
	 (service (process-contact process :service)))
    (with-temp-buffer
      (insert (format "%d\n" service))
      (write-region (point-min) (point-max) commfile t))
    (message "; Listening for connection from controller at localhost:%d"
	     service)))



;; `kill-emacs' interception.

(defun ats-kill-emacs-function ()
  "Print a message announcing that Emacs is exiting.
Also, if executing a Lisp form, reply to the controller with the
backtrace of the exit before really exiting."
  (when-let* ((standard-output #'external-debugging-output)
	      (process ats-process))
    (princ (if ats-executing-form
	       "Emacs is attempting to exit while evaluating a form...\n"
	     "Emacs is exiting...\n"))
    (backtrace)
    (when ats-in-eval
      (with-temp-buffer
	(let ((standard-output (current-buffer)))
	  (backtrace)
	  (let ((err (cons 'exit (buffer-string))))
	    (let* ((print-escape-control-characters t)
		   (print-escape-newlines t)
		   (str (encode-coding-string
			 (prin1-to-string err) 'utf-8-emacs t)))
	      (if ats-eval-as-printed
		  (let* ((quoted (prin1-to-string str)))
		    (process-send-string
		     process (format "\fats-request:%d %d\n"
				     ats-eval-serial
				     (length quoted)))
		    (process-send-string process quoted))
		(process-send-string
		 process (format "\fats-request:%d %d\n"
				 ats-eval-serial
				 (length str)))
		(process-send-string process str)))))))))
(add-hook 'kill-emacs-hook #'ats-kill-emacs-function)

(provide 'test-driver)

;;; test-driver.el ends here

;; Local Variables:
;; emacs-lisp-docstring-fill-column: 64
;; indent-tabs-mode: t
;; no-byte-compile: t
;; End:
