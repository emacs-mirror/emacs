;;; erc-notify.el --- Online status change notification  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2004, 2006-2026 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@lexx.delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; URL: https://www.emacswiki.org/emacs/ErcNotify
;; Keywords: comm

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

;; This module defines a new command, /NOTIFY
;; See the docstring of `erc-cmd-NOTIFY' for details.

;;; Code:

(require 'erc)
(eval-when-compile (require 'pcomplete))

;;;; Customizable variables

(defgroup erc-notify nil
  "Track online status of certain nicknames."
  :group 'erc)

(defcustom erc-notify-list nil
  "List of nicknames you want to be notified about online/offline status change."
  :type '(repeat string))

(defcustom erc-notify-interval 60
  "Time interval (in seconds) for checking online status of notified people."
  :type 'integer)

(defcustom erc-notify-signon-hook nil
  "Hook run after someone on `erc-notify-list' has signed on.
Two arguments are passed to the function, SERVER and NICK, both
strings."
  :type 'hook
  :options '(erc-notify-signon))

(defcustom erc-notify-signoff-hook nil
  "Hook run after someone on `erc-notify-list' has signed off.
Two arguments are passed to the function, SERVER and NICK, both
strings."
  :type 'hook
  :options '(erc-notify-signoff))

(defun erc-notify-signon (server nick)
  (message "%s signed on at %s" nick server))

(defun erc-notify-signoff (server nick)
  (message "%s signed off from %s" nick server))

;;;; Internal variables

(defvar-local erc-last-ison nil
  "Last ISON information received through `erc-notify-timer'.")

(defvar-local erc-last-ison-time 0
  "Last time ISON was sent to the server in `erc-notify-timer'.")

;;;; Setup

(defun erc-notify-install-message-catalogs ()
  (declare (obsolete "defined at top level in erc-notify.el" "30.1"))
  (with-suppressed-warnings ((obsolete erc-define-catalog))
    (erc-define-catalog
     'english
     '((notify-current . "Notified people online: %l")
       (notify-list    . "Current notify list: %l")
       (notify-on      . "Detected %n on IRC network %m")
       (notify-off     . "%n has left IRC network %m")))))

;;;###autoload(autoload 'erc-notify-mode "erc-notify" nil t)
(define-erc-module notify nil
  "Periodically check for the online status of certain users and report
changes."
  ((add-hook 'erc-timer-hook #'erc-notify-timer)
   (add-hook 'erc-server-JOIN-functions #'erc-notify-JOIN)
   (add-hook 'erc-server-NICK-functions #'erc-notify-NICK)
   (add-hook 'erc-server-QUIT-functions #'erc-notify-QUIT))
  ((remove-hook 'erc-timer-hook #'erc-notify-timer)
   (remove-hook 'erc-server-JOIN-functions #'erc-notify-JOIN)
   (remove-hook 'erc-server-NICK-functions #'erc-notify-NICK)
   (remove-hook 'erc-server-QUIT-functions #'erc-notify-QUIT)))

;;;; Timer handler

(defun erc-notify-timer (now)
  (when (and erc-server-connected
	     erc-notify-list
	     (> (erc-time-diff
		 erc-last-ison-time now)
		erc-notify-interval))
    (erc-once-with-server-event
     303
     (lambda (proc parsed)
       (let* ((server (erc-response.sender parsed))
	      (ison-list (delete "" (split-string
				     (erc-response.contents parsed))))
	      (new-list ison-list)
	      (old-list (erc-with-server-buffer erc-last-ison)))
	 (while new-list
	   (when (not (erc-member-ignore-case (car new-list) old-list))
	     (run-hook-with-args 'erc-notify-signon-hook server (car new-list))
	     (erc-display-message
	      parsed 'notice proc
              'notify-on ?n (car new-list) ?m (erc-network-name)))
	   (setq new-list (cdr new-list)))
	 (while old-list
	   (when (not (erc-member-ignore-case (car old-list) ison-list))
	     (run-hook-with-args 'erc-notify-signoff-hook server (car old-list))
	     (erc-display-message
	      parsed 'notice proc
              'notify-off ?n (car old-list) ?m (erc-network-name)))
	   (setq old-list (cdr old-list)))
	 (setq erc-last-ison ison-list)
	 t)))
    (erc-server-send
     (concat "ISON " (mapconcat #'identity erc-notify-list " ")))
    (setq erc-last-ison-time now)))

(defun erc-notify-JOIN (proc parsed)
  "Check if channel joiner is on `erc-notify-list' and not on `erc-last-ison'.
When that's the case, produce a `notify-on' message and add the
nick to `erc-last-ison' to prevent any further notifications."
  (let ((nick (erc-extract-nick (erc-response.sender parsed))))
    (when (and (erc-member-ignore-case nick erc-notify-list)
	       (not (erc-member-ignore-case nick erc-last-ison)))
      (add-to-list 'erc-last-ison nick)
      (run-hook-with-args 'erc-notify-signon-hook
			  (or erc-server-announced-name erc-session-server)
			  nick)
      (erc-display-message
       parsed 'notice proc
       'notify-on ?n nick ?m (erc-network-name)))
    nil))

(defun erc-notify-NICK (proc parsed)
  "Check if new nick is on `erc-notify-list' and not on `erc-last-ison'.
When that's the case, produce a `notify-on' message and add the
nick to `erc-last-ison' to prevent any further notifications."
  (let ((nick (erc-response.contents parsed)))
    (when (and (erc-member-ignore-case nick erc-notify-list)
	       (not (erc-member-ignore-case nick erc-last-ison)))
      (add-to-list 'erc-last-ison nick)
      (run-hook-with-args 'erc-notify-signon-hook
			  (or erc-server-announced-name erc-session-server)
			  nick)
      (erc-display-message
       parsed 'notice proc
       'notify-on ?n nick ?m (erc-network-name)))
    nil))

(defun erc-notify-QUIT (proc parsed)
  "Check if quitter is on `erc-notify-list' and on `erc-last-ison'.
When that's the case, insert a `notify-off' message and remove
the nick from `erc-last-ison' to prevent further notifications."
  (let ((nick (erc-extract-nick (erc-response.sender parsed))))
    (when (and (erc-member-ignore-case nick erc-notify-list)
	       (erc-member-ignore-case nick erc-last-ison))
      (setq erc-last-ison (cl-delete-if
			   (let ((nick-down (erc-downcase nick)))
			     (lambda (el)
			       (string= nick-down (erc-downcase el))))
			   erc-last-ison))
      (run-hook-with-args 'erc-notify-signoff-hook
			  (or erc-server-announced-name erc-session-server)
			  nick)
      (erc-display-message
       parsed 'notice proc
       'notify-off ?n nick ?m (erc-network-name)))
    nil))

;;;; User level command

;;;###autoload
(defun erc-cmd-NOTIFY (&rest args)
  "Change `erc-notify-list' or list current notify-list members online.
Without args, list the current list of notified people online,
with args, toggle notify status of people."
  (unless erc-notify-mode
    (erc-notify-mode +1)
    (erc-button--display-error-notice-with-keys
     (current-buffer)
     "Command /NOTIFY requires the `notify' module. Enabling now. Add `notify'"
     " to `erc-modules' before next starting ERC to silence this message."))
  (cond
   ((null args)
    ;; Print current notified people (online)
    (let ((ison (erc-with-server-buffer erc-last-ison)))
      (if (not ison)
	  (erc-display-message
	   nil 'notice 'active "No ison-list yet!")
	(erc-display-message
	 nil 'notice 'active
         'notify-current ?l ison))))
   ((string= (car args) "-l")
    (let ((list (if erc-notify-list
                    (mapconcat #'identity erc-notify-list " ")
                  "(empty)")))
      (erc-display-message nil 'notice 'active 'notify-list ?l list)))
   (t
    (while args
      (if (erc-member-ignore-case (car args) erc-notify-list)
	  (progn
	    (setq erc-notify-list (delete (car args) erc-notify-list))
	    ;; Remove the nick from the value of erc-last-ison in
	    ;; every server buffer.  This prevents seeing a signoff
	    ;; notification for a nick that you have just _removed_
	    ;; from your notify list.
	    (dolist (buf (erc-buffer-list))
	      (with-current-buffer buf
                ;; FIXME replace with `erc--server-buffer-p' or
                ;; explain why that's unwise.
                (if (erc-server-or-unjoined-channel-buffer-p)
		    (setq erc-last-ison (delete (car args) erc-last-ison))))))
	(setq erc-notify-list (cons (erc-string-no-properties (car args))
				    erc-notify-list)))
      (setq args (cdr args)))
    (erc-cmd-NOTIFY "-l")))
  t)

;; "--" is not a typo.
(declare-function pcomplete--here "pcomplete"
		  (&optional form stub paring form-only))
(declare-function pcomplete-erc-all-nicks "erc-pcomplete"
                  (&optional postfix))

;;;###autoload
(defun pcomplete/erc-mode/NOTIFY ()
  (require 'erc-pcomplete)
  (pcomplete-here (append erc-notify-list (pcomplete-erc-all-nicks))))

(define-obsolete-variable-alias 'erc-message-english-notify_on
  'erc-message-english-notify-on "30.1")
(define-obsolete-variable-alias 'erc-message-english-notify_off
  'erc-message-english-notify-off "30.1")
(define-obsolete-variable-alias 'erc-message-english-notify_list
  'erc-message-english-notify-list "30.1")
(define-obsolete-variable-alias 'erc-message-english-notify_current
  'erc-message-english-notify-current "30.1")

(erc-define-message-format-catalog english
  (notify-current . "Notified people online: %l")
  (notify-list . "Current notify list: %l")
  (notify-on . "Detected %n on IRC network %m")
  (notify-off . "%n has left IRC network %m"))


;;;; Module `querypoll'

;; This module is similar to `notify' in that it periodically tries to
;; discover whether certain users are online.  Unlike that module, it's
;; not really configurable.  Rather, it only selects users you've
;; corresponded with in a query buffer, and it keeps `erc-server-users'
;; entries for them updated.

(declare-function ring-empty-p "ring" (ring))
(declare-function ring-insert "ring" (ring item))
(declare-function ring-insert+extend "ring" (ring item))
(declare-function ring-length "ring" (ring))
(declare-function ring-member "ring" (ring item))
(declare-function ring-ref "ring" (ring index))
(declare-function ring-remove "ring" (ring &optional index))

(defvar-local erc--querypoll-ring nil)
(defvar-local erc--querypoll-timer nil)

(defcustom erc-querypoll-exclude-regexp
  (rx bot (or (: "*" (+ nonl)) (: (+ (in "A-Za-z")) "Serv")) eot)
  "Pattern to skip polling for bots and services you regularly query."
  :group 'erc
  :package-version '(ERC . "5.6")
  :type 'regexp)

;;;###autoload(autoload 'erc-querypoll-mode "erc-notify" nil t)
(define-erc-module querypoll nil
  "Send periodic \"WHO\" requests for each query buffer.
Omit query participants who are currently present in some channel.
Instead of announcing arrivals and departures, rely on other modules,
like `nickbar', to provide UI feedback when changes occur.

Once ERC implements the `monitor' extension, this module will serve as
an optional fallback for keeping query-participant rolls up to date on
servers that lack support or are stingy with their allotments.  Until
such time, this module should be considered experimental and only really
useful for bots and other non-interactive Lisp programs.  Please note
that reporting is unreliable for short periods while a query participant
is parting, joining, quitting, or logging in.

This is a local ERC module, so selectively polling only a subset of
query targets is possible but cumbersome.  To do so, ensure
`erc-querypoll-mode' is enabled in the server buffer, and then toggle it
as appropriate in desired query buffers.  To stop polling for the
current connection, toggle off the command \\[erc-querypoll-mode] from a
server buffer, or run \\`M-x C-u erc-querypoll-disable RET' from a
target buffer.  Note that this module's minor mode must remain active in
at least the server buffer."
  ((if erc--target
       (if (erc-query-buffer-p)
           (progn ; accommodate those who eschew `erc-modules'
             (erc-with-server-buffer
               (unless erc-querypoll-mode
                 (erc-querypoll-mode +1)))
             (add-function :override (local 'erc--query-table-synced-predicate)
                           #'erc--querypoll-active-p)
             (erc--querypoll-subscribe (current-buffer)))
         (erc-querypoll-mode -1))
     (cl-assert (not erc--decouple-query-and-channel-membership-p))
     (setq-local erc--querypoll-ring (make-ring 5))
     (erc-with-all-buffers-of-server erc-server-process nil
       (unless erc-querypoll-mode
         (erc-querypoll-mode +1)))))
  ((when erc--querypoll-timer
     (cancel-timer erc--querypoll-timer))
   (if erc--target
       (when-let* (((erc-query-buffer-p))
                   (ring (erc-with-server-buffer erc--querypoll-ring))
                   (index (ring-member ring (current-buffer)))
                   ((not (erc--querypoll-target-in-chan-p (current-buffer)))))
         (ring-remove ring index)
         (remove-function (local 'erc--query-table-synced-predicate)
                          #'erc--querypoll-active-p)
         (unless (erc-current-nick-p (erc-target))
           (erc-remove-current-channel-member (erc-target))))
     (erc-with-all-buffers-of-server erc-server-process #'erc-query-buffer-p
       (erc-querypoll-mode -1)))
   (kill-local-variable 'erc--querypoll-ring)
   (kill-local-variable 'erc--querypoll-timer))
  localp)

(defun erc--querypoll-active-p ()
  "Return non-nil if `erc-querypoll-mode' is active in the current buffer."
  erc-querypoll-mode)

(defvar erc-querypoll-period-params '(10 10 1)
  "Parameters affecting the delay with respect to the number of buffers.
The elements represent some parameters of an exponential decay function,
a(e)^{-x/b}+c.  The first number (a) affects the overall scaling.  A
higher value means longer delays for all query buffers relative to queue
length.  The second number (b) determines how quickly the delay
decreases as the queue length increases.  Larger values make the delay
taper off more gradually.  The last number (c) sets the minimum delay
between updates regardless of queue length.")

(defun erc--querypoll-compute-period (queue-size)
  "Calculate delay based on QUEUE-SIZE."
  (let ((scale (nth 0 erc-querypoll-period-params))
        (rate (* 1.0 (nth 1 erc-querypoll-period-params)))
        (min (nth 2 erc-querypoll-period-params)))
    (+ (* scale (exp (/ (- queue-size) rate))) min)))

(defun erc--querypoll-target-in-chan-p (buffer)
  "Determine whether buffer's target, as a user, is joined to any channels."
  (and-let*
      ((target (erc--target-string (buffer-local-value 'erc--target buffer)))
       (user (erc-get-server-user target))
       (buffers (erc-server-user-buffers user))
       ((seq-some #'erc-channel-p buffers)))))

(defun erc--querypoll-get-length (ring)
  "Return the effective length of RING, discounting chan members."
  (let ((count 0))
    (dotimes (i (ring-length ring))
      (unless (erc--querypoll-target-in-chan-p (ring-ref ring i))
        (cl-incf count 1)))
    count))

(defun erc--querypoll-get-next (ring)
  (let ((n (ring-length ring)))
    (catch 'found
      (while (natnump (cl-decf n))
        (when-let* ((buffer (ring-remove ring))
                    ((buffer-live-p buffer)))
          ;; Push back buffers for users joined to some chan.
          (if (erc--querypoll-target-in-chan-p buffer)
              (ring-insert ring buffer)
            (throw 'found buffer)))))))

(defun erc--querypoll-subscribe (query-buffer &optional penalty)
  "Add QUERY-BUFFER to FIFO and ensure timer is running."
  (when query-buffer
    (cl-assert (erc-query-buffer-p query-buffer)))
  (erc-with-server-buffer
    (when (and query-buffer
               (not (with-current-buffer query-buffer
                      (or (erc-current-nick-p (erc-target))
                          (string-match erc-querypoll-exclude-regexp
                                        (erc-target)))))
               (not (ring-member erc--querypoll-ring query-buffer)))
      (ring-insert+extend erc--querypoll-ring query-buffer))
    (unless erc--querypoll-timer
      (setq erc--querypoll-timer
            (let* ((length (erc--querypoll-get-length erc--querypoll-ring))
                   (period (erc--querypoll-compute-period length)))
              (run-at-time (+ (or penalty 0) period)
                           nil #'erc--querypoll-send (current-buffer)))))))

(defun erc--querypoll-on-352 (target-nick args)
  "Add or update `erc-server-users' data for TARGET-NICK from ARGS.
Then add user to participant rolls in any existing query buffers."
  (pcase-let
      ((`(,_ ,channel ,login ,host ,_server ,nick ,_flags, hop-real) args))
    (when (and (string= channel "*") (erc-nick-equal-p nick target-nick))
      (if-let* ((user (erc-get-server-user nick)))
          (erc-update-user user nick host login
                           (erc--extract-352-full-name hop-real))
        ;; Don't add unless target is already known.
        (when (erc-get-buffer nick erc-server-process)
          (erc-add-server-user
           nick (make-erc-server-user
                 :nickname nick :login login :host host
                 :full-name (erc--extract-352-full-name hop-real)))))
      (erc--ensure-query-member nick)
      t)))

;; This uses heuristics to associate replies to the initial request
;; because ERC does not yet support `labeled-response'.
(defun erc--querypoll-send (server-buffer)
  "Send a captive \"WHO\" in SERVER-BUFFER."
  (when (and (buffer-live-p server-buffer)
             (buffer-local-value 'erc-server-connected server-buffer))
    (with-current-buffer server-buffer
      (setq erc--querypoll-timer nil)
      (if-let* ((buffer (erc--querypoll-get-next erc--querypoll-ring)))
          (letrec
              ((target (erc--target-string
                        (buffer-local-value 'erc--target buffer)))
               (penalty 0)
               (here-fn (erc-once-with-server-event
                         "352" (lambda (_ parsed)
                                 (erc--querypoll-on-352
                                  target (erc-response.command-args parsed)))))
               (done-fn (erc-once-with-server-event
                         "315"
                         (lambda (_ parsed)
                           (if (memq here-fn erc-server-352-functions)
                               (erc-remove-user
                                (nth 1 (erc-response.command-args parsed)))
                             (remove-hook 'erc-server-352-functions here-fn t))
                           (remove-hook 'erc-server-263-functions fail-fn t)
                           (remove-hook 'erc-server-315-functions done-fn t)
                           (erc--querypoll-subscribe buffer penalty)
                           t)))
               (fail-fn (erc-once-with-server-event
                         "263"
                         (lambda (proc parsed)
                           (setq penalty 60)
                           (funcall done-fn proc parsed)
                           t))))
            (erc-server-send (concat "WHO " target)))
        (unless (ring-empty-p erc--querypoll-ring)
          (erc--querypoll-subscribe nil 30))))))

(provide 'erc-notify)

;;; erc-notify.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
