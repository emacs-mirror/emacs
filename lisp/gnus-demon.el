;;; gnus-demon.el --- daemonic Gnus behaviour
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

;;; Code:

(require 'gnus)

(eval-when-compile (require 'cl))

(defvar gnus-demon-handlers nil
  "Alist of daemonic handlers to be run at intervals.
Each handler is a list on the form

\(FUNCTION TIME IDLE)

FUNCTION is the function to be called. 
TIME is the number of `gnus-demon-timestep's between each call.  
If nil, never call.  If t, call each `gnus-demon-timestep'.
If IDLE is t, only call if Emacs has been idle for a while.  If IDLE
is a number, only call when Emacs has been idle more than this number
of `gnus-demon-timestep's.  If IDLE is nil, don't care about
idleness.  If IDLE is a number and TIME is nil, then call once each
time Emacs has been idle for IDLE `gnus-demon-timestep's.")

(defvar gnus-demon-timestep 60
  "*Number of seconds in each demon timestep.")

;;; Internal variables.

(defvar gnus-demon-timer nil)
(defvar gnus-demon-idle-has-been-called nil)
(defvar gnus-demon-idle-time 0)
(defvar gnus-demon-handler-state nil)
(defvar gnus-demon-is-idle nil)
(defvar gnus-demon-last-keys nil) 

(eval-and-compile
  (autoload 'timezone-parse-date "timezone")
  (autoload 'timezone-make-arpa-date "timezone"))

;;; Functions.

(defun gnus-demon-add-handler (function time idle)
  "Add the handler FUNCTION to be run at TIME and IDLE."
  ;; First remove any old handlers that use this function.
  (gnus-demon-remove-handler function)
  ;; Then add the new one.
  (push (list function time idle) gnus-demon-handlers)
  (gnus-demon-init))

(defun gnus-demon-remove-handler (function &optional no-init)
  "Remove the handler FUNCTION from the list of handlers."
  (setq gnus-demon-handlers 
	(delq (assq function gnus-demon-handlers)
	      gnus-demon-handlers))
  (or no-init (gnus-demon-init)))

(defun gnus-demon-init ()
  "Initialize the Gnus daemon."
  (interactive)
  (gnus-demon-cancel)
  (if (null gnus-demon-handlers)
      () ; Nothing to do.
    ;; Set up timer.
    (setq gnus-demon-timer 
	  (nnheader-run-at-time 
	   gnus-demon-timestep gnus-demon-timestep 'gnus-demon))
    ;; Reset control variables.
    (setq gnus-demon-handler-state
	  (mapcar 
	   (lambda (handler)
	     (list (car handler) (gnus-demon-time-to-step (nth 1 handler))
		   (nth 2 handler)))
	   gnus-demon-handlers))
    (setq gnus-demon-idle-time 0)
    (setq gnus-demon-idle-has-been-called nil)
    (setq gnus-use-demon t)))

(gnus-add-shutdown 'gnus-demon-cancel 'gnus)

(defun gnus-demon-cancel ()
  "Cancel any Gnus daemons."
  (interactive)
  (and gnus-demon-timer
       (nnheader-cancel-timer gnus-demon-timer))
  (setq gnus-demon-timer nil
	gnus-use-demon nil))

(defun gnus-demon-is-idle-p ()
  "Whether Emacs is idle or not."
  ;; We do this simply by comparing the 100 most recent keystrokes
  ;; with the ones we had last time.  If they are the same, one might
  ;; guess that Emacs is indeed idle.  This only makes sense if one
  ;; calls this function seldom -- like once a minute, which is what
  ;; we do here.
  (let ((keys (recent-keys)))
    (or (equal keys gnus-demon-last-keys)
	(progn
	  (setq gnus-demon-last-keys keys)
	  nil))))

(defun gnus-demon-time-to-step (time)
  "Find out how many seconds to TIME, which is on the form \"17:43\"."
  (if (not (stringp time))
      time
    (let* ((date (current-time-string))
	   (dv (timezone-parse-date date))
	   (tdate (timezone-make-arpa-date 
		   (string-to-number (aref dv 0))
		   (string-to-number (aref dv 1))
		   (string-to-number (aref dv 2)) time
		   (or (aref dv 4) "UT")))
	   (nseconds (gnus-time-minus
		      (gnus-encode-date tdate) (gnus-encode-date date))))
      (round
       (/ (if (< nseconds 0)
	      (+ nseconds (* 60 60 24))
	    nseconds) gnus-demon-timestep)))))

(defun gnus-demon ()
  "The Gnus daemon that takes care of running all Gnus handlers."
  ;; Increase or reset the time Emacs has been idle.
  (if (gnus-demon-is-idle-p)
      (incf gnus-demon-idle-time)
    (setq gnus-demon-idle-time 0)
    (setq gnus-demon-idle-has-been-called nil))
  ;; Then we go through all the handler and call those that are
  ;; sufficiently ripe.
  (let ((handlers gnus-demon-handler-state)
	handler time idle)
    (while handlers
      (setq handler (pop handlers))
      (cond 
       ((numberp (setq time (nth 1 handler)))
	;; These handlers use a regular timeout mechanism.  We decrease
	;; the timer if it hasn't reached zero yet.
	(or (zerop time)
	    (setcar (nthcdr 1 handler) (decf time)))
	(and (zerop time)		; If the timer now is zero...
	     (or (not (setq idle (nth 2 handler))) ; Don't care about idle.
		 (and (numberp idle)	; Numerical idle...
		      (< idle gnus-demon-idle-time)) ; Idle timed out.
		 gnus-demon-is-idle)	; Or just need to be idle.
	     ;; So we call the handler.
	     (progn
	       (funcall (car handler))
	       ;; And reset the timer.
	       (setcar (nthcdr 1 handler)
		       (gnus-demon-time-to-step
			(nth 1 (assq (car handler) gnus-demon-handlers)))))))
       ;; These are only supposed to be called when Emacs is idle. 
       ((null (setq idle (nth 2 handler)))
	;; We do nothing.
	)
       ((not (numberp idle))
	;; We want to call this handler each and every time that
	;; Emacs is idle. 
	(funcall (car handler)))
       (t
	;; We want to call this handler only if Emacs has been idle
	;; for a specified number of timesteps.
	(and (not (memq (car handler) gnus-demon-idle-has-been-called))
	     (< idle gnus-demon-idle-time)
	     (progn
	       (funcall (car handler))
	       ;; Make sure the handler won't be called once more in
	       ;; this idle-cycle.
	       (push (car handler) gnus-demon-idle-has-been-called))))))))

(defun gnus-demon-add-nocem ()
  "Add daemonic NoCeM handling to Gnus."
  (gnus-demon-add-handler 'gnus-demon-scan-nocem 60 t))

(defun gnus-demon-scan-nocem ()
  "Scan NoCeM groups for NoCeM messages."
  (gnus-nocem-scan-groups))

(defun gnus-demon-add-disconnection ()
  "Add daemonic server disconnection to Gnus."
  (gnus-demon-add-handler 'gnus-demon-close-connections nil 30))

(defun gnus-demon-close-connections ()
  (gnus-close-backends))

(defun gnus-demon-add-scanmail ()
  "Add daemonic scanning of mail from the mail backends."
  (gnus-demon-add-handler 'gnus-demon-scan-mail 120 60))

(defun gnus-demon-scan-mail ()
  (let ((servers gnus-opened-servers)
	server)
    (while (setq server (car (pop servers)))
      (and (gnus-check-backend-function 'request-scan (car server))
	   (or (gnus-server-opened server)
	       (gnus-open-server server))
	   (gnus-request-scan nil server)))))

(provide 'gnus-demon)

;;; gnus-demon.el ends here
