;;; gnus-demon.el --- daemonic Gnus behaviour
;; Copyright (C) 1995,96,97 Free Software Foundation, Inc.

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

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-int)
(require 'nnheader)
(eval-and-compile
  (if (string-match "XEmacs" (emacs-version))
      (require 'itimer)
    (require 'timer)))

(defgroup gnus-demon nil
  "Demonic behaviour."
  :group 'gnus)

(defcustom gnus-demon-handlers nil
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
time Emacs has been idle for IDLE `gnus-demon-timestep's."
  :group 'gnus-demon
  :type '(repeat (list function
		       (choice :tag "Time"
			       (const :tag "never" nil)
			       (const :tag "one" t)
			       (integer :tag "steps" 1))
		       (choice :tag "Idle"
			       (const :tag "don't care" nil)
			       (const :tag "for a while" t)
			       (integer :tag "steps" 1)))))

(defcustom gnus-demon-timestep 60
  "*Number of seconds in each demon timestep."
  :group 'gnus-demon
  :type 'integer)

;;; Internal variables.

(defvar gnus-demon-timer nil)
(defvar gnus-demon-idle-has-been-called nil)
(defvar gnus-demon-idle-time 0)
(defvar gnus-demon-handler-state nil)
(defvar gnus-demon-last-keys nil)
(defvar gnus-inhibit-demon nil
  "*If non-nil, no daemonic function will be run.")

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
  (unless no-init
    (gnus-demon-init)))

(defun gnus-demon-init ()
  "Initialize the Gnus daemon."
  (interactive)
  (gnus-demon-cancel)
  (if (null gnus-demon-handlers)
      ()				; Nothing to do.
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
  (when gnus-demon-timer
    (nnheader-cancel-timer gnus-demon-timer))
  (setq gnus-demon-timer nil
	gnus-use-demon nil)
  (condition-case ()
      (nnheader-cancel-function-timers 'gnus-demon)
    (error t)))

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
    (let* ((now (current-time))
           ;; obtain NOW as discrete components -- make a vector for speed
           (nowParts (apply 'vector (decode-time now)))
           ;; obtain THEN as discrete components
           (thenParts (timezone-parse-time time))
           (thenHour (string-to-int (elt thenParts 0)))
           (thenMin (string-to-int (elt thenParts 1)))
           ;; convert time as elements into number of seconds since EPOCH.
           (then (encode-time 0
                              thenMin
                              thenHour
                              ;; If THEN is earlier than NOW, make it
                              ;; same time tomorrow. Doc for encode-time
                              ;; says that this is OK.
                              (+ (elt nowParts 3)
                                 (if (or (< thenHour (elt nowParts 2))
                                         (and (= thenHour (elt nowParts 2))
                                              (<= thenMin (elt nowParts 1))))
                                     1 0))
                              (elt nowParts 4)
                              (elt nowParts 5)
                              (elt nowParts 6)
                              (elt nowParts 7)
                              (elt nowParts 8)))
           ;; calculate number of seconds between NOW and THEN
           (diff (+ (* 65536 (- (car then) (car now)))
                    (- (cadr then) (cadr now)))))
      ;; return number of timesteps in the number of seconds
      (round (/ diff gnus-demon-timestep)))))

(defun gnus-demon ()
  "The Gnus daemon that takes care of running all Gnus handlers."
  ;; Increase or reset the time Emacs has been idle.
  (if (gnus-demon-is-idle-p)
      (incf gnus-demon-idle-time)
    (setq gnus-demon-idle-time 0)
    (setq gnus-demon-idle-has-been-called nil))
  ;; Disable all daemonic stuff if we're in the minibuffer
  (when (and (not (window-minibuffer-p (selected-window)))
	     (not gnus-inhibit-demon))
    ;; Then we go through all the handler and call those that are
    ;; sufficiently ripe.
    (let ((handlers gnus-demon-handler-state)
	  (gnus-inhibit-demon t)
	  handler time idle)
      (while handlers
	(setq handler (pop handlers))
	(cond
	 ((numberp (setq time (nth 1 handler)))
	  ;; These handlers use a regular timeout mechanism.  We decrease
	  ;; the timer if it hasn't reached zero yet.
	  (unless (zerop time)
	    (setcar (nthcdr 1 handler) (decf time)))
	  (and (zerop time)		; If the timer now is zero...
	       ;; Test for appropriate idleness
	       (progn
		 (setq idle (nth 2 handler))
		 (cond
		  ((null idle) t)	; Don't care about idle.
		  ((numberp idle)	; Numerical idle...
		   (< idle gnus-demon-idle-time)) ; Idle timed out.
		  (t (< 0 gnus-demon-idle-time)))) ; Or just need to be idle.
	       ;; So we call the handler.
	       (progn
		 (ignore-errors (funcall (car handler)))
		 ;; And reset the timer.
		 (setcar (nthcdr 1 handler)
			 (gnus-demon-time-to-step
			  (nth 1 (assq (car handler) gnus-demon-handlers)))))))
	 ;; These are only supposed to be called when Emacs is idle.
	 ((null (setq idle (nth 2 handler)))
	  ;; We do nothing.
	  )
	 ((and (not (numberp idle))
	       (gnus-demon-is-idle-p))
	  ;; We want to call this handler each and every time that
	  ;; Emacs is idle.
	  (ignore-errors (funcall (car handler))))
	 (t
	  ;; We want to call this handler only if Emacs has been idle
	  ;; for a specified number of timesteps.
	  (and (not (memq (car handler) gnus-demon-idle-has-been-called))
	       (< idle gnus-demon-idle-time)
	       (gnus-demon-is-idle-p)
	       (progn
		 (ignore-errors (funcall (car handler)))
		 ;; Make sure the handler won't be called once more in
		 ;; this idle-cycle.
		 (push (car handler) gnus-demon-idle-has-been-called)))))))))

(defun gnus-demon-add-nocem ()
  "Add daemonic NoCeM handling to Gnus."
  (gnus-demon-add-handler 'gnus-demon-scan-nocem 60 30))

(defun gnus-demon-scan-nocem ()
  "Scan NoCeM groups for NoCeM messages."
  (save-window-excursion
    (gnus-nocem-scan-groups)))

(defun gnus-demon-add-disconnection ()
  "Add daemonic server disconnection to Gnus."
  (gnus-demon-add-handler 'gnus-demon-close-connections nil 30))

(defun gnus-demon-close-connections ()
  (save-window-excursion
    (gnus-close-backends)))

(defun gnus-demon-add-scanmail ()
  "Add daemonic scanning of mail from the mail backends."
  (gnus-demon-add-handler 'gnus-demon-scan-mail 120 60))

(defun gnus-demon-scan-mail ()
  (save-window-excursion
    (let ((servers gnus-opened-servers)
	  server)
      (while (setq server (car (pop servers)))
	(and (gnus-check-backend-function 'request-scan (car server))
	     (or (gnus-server-opened server)
		 (gnus-open-server server))
	     (gnus-request-scan nil server))))))

(defun gnus-demon-add-rescan ()
  "Add daemonic scanning of new articles from all backends."
  (gnus-demon-add-handler 'gnus-demon-scan-news 120 60))

(defun gnus-demon-scan-news ()
  (save-window-excursion
    (when (gnus-alive-p)
      (save-excursion
	(set-buffer gnus-group-buffer)
	(gnus-group-get-new-news)))))

(defun gnus-demon-add-scan-timestamps ()
  "Add daemonic updating of timestamps in empty newgroups."
  (gnus-demon-add-handler 'gnus-demon-scan-timestamps nil 30))

(defun gnus-demon-scan-timestamps ()
  "Set the timestamp on all newsgroups with no unread and no ticked articles."
  (when (gnus-alive-p)
    (let ((cur-time (current-time))
	  (newsrc (cdr gnus-newsrc-alist))
	  info group unread has-ticked)
      (while (setq info (pop newsrc))
	(setq group (gnus-info-group info)
	      unread (gnus-group-unread group)
	      has-ticked (cdr (assq 'tick (gnus-info-marks info))))
	(when (and (numberp unread)
		   (= unread 0)
		   (not has-ticked))
	  (gnus-group-set-parameter group 'timestamp cur-time))))))

(provide 'gnus-demon)

;;; gnus-demon.el ends here
