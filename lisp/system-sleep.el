;;; system-sleep.el --- System sleep/wake event management -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Stephane Marks <shipmints@gmail.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package-Requires: ((emacs "31.1"))

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

;; Call `system-sleep-block-sleep' to inhibit system-wide idle sleep.
;; Idle sleep is typically triggered when the system does not detect
;; user activity and is independent of any processing that may be on
;; going.  This function is useful to block idle sleep for long-running
;; operations, for example, when a compilation is running.  You have the
;; option of keeping the system active while letting the display sleep.
;; This function returns a token which you must use to unblock this
;; request.
;;
;; Call `system-sleep-unblock-sleep' with the token from
;; `system-sleep-block-sleep' to unblock system-wide idle sleep for this
;; request.  There may be other active requests which will prevent the
;; system from sleeping.
;;
;; The function `system-sleep-sleep-blocked-p' will tell you if
;; `system-sleep' has any active system sleep blocks.
;;
;; Note: When the Emacs process dies, blocks are released on all
;; platforms.
;;
;; You can register functions on the abnormal hook
;; `system-sleep-event-functions'.  Each function will be called when
;; the system is preparing for sleep and when the system wakes from
;; sleep.  These functions are useful when you want to close (and
;; potentially reopen) external connections or serial ports.
;;
;; On supported GNU/Linux systems, the implementation is via D-Bus to
;; inhibit idle sleep, keep the display active, and forward events from
;; logind for system sleep events.
;;
;; On macOS and MS-Windows, native APIs are used to block idle sleep,
;; keep the display active, and provide sleep event notifications.
;;
;; On MS-Windows, an idle sleep block that keeps the display active may
;; not inhibit the screen saver.
;;
;; Externally to Emacs, there are system utility functions that you can
;; use to inspect all processes on your system that might be blocking it
;; from sleeping.
;;
;; On D-Bus systems, you can use the commands:
;;
;;   systemd-inhibit --list
;; or
;;   dbus-send --system --print-reply --dest=org.freedesktop.login1 \
;;   /org/freedesktop/login1 \
;;   org.freedesktop.login1.Manager.ListInhibitors
;;
;; Note: You can find the sleep/shutdown delay InhibitDelayMaxUSec in
;; the file logind.conf(5) which typically defaults to 5 seconds.
;;
;; On macOS, you can use the command:
;;
;;   pmset -g assertions
;;
;; On MS-Windows, you can use the following command which may need to be
;; run as an administrator:
;;
;;   powercfg -requests

;;; Code:

(require 'cl-lib)

;; Pacify the byte compiler.
(declare-function dbus--fd-close "dbusbind.c")
(declare-function dbus-unregister-object "dbus.el")
(declare-function dbus-register-signal "dbus.el")
(declare-function dbus-call-method "dbus.el")
(declare-function dbus-list-activatable-names "dbus.el")
(defvar dbus-service-emacs)

(defgroup system-sleep nil
  "System sleep/wake blocking and event management."
  :group 'system-interface
  :version "31.1")

(defvar system-sleep--back-end nil
  "Generic sleep-wake method system dispatcher.")

(defvar system-sleep--sleep-block-tokens nil
  "A list of active sleep-block tokens.
If non-nil, idle sleep is inhibited by `system-sleep'.")

(cl-defstruct
    (sleep-event (:type list) :named
                 (:constructor nil)
                 (:constructor make-sleep-event (state)))
  state)

;;;###autoload (autoload 'sleep-event-state "system-sleep.el")

;;;###autoload
(defcustom system-sleep-event-functions nil
  "Abnormal hook invoked on system sleep events.
Each function is called with one argument EVENT, a sleep event.  EVENT
state can be retrieved via \\+`(sleep-event-state EVENT)'.  It will be
one of the symbols \\+`pre-sleep' or \\+`post-wake'.

Handling \\+`pre-sleep' events should be done as fast as possible, do as
little as possible and avoid user prompts.  Systems often grant a very
short pre-sleep processing interval, typically ranging between 2 and 5
seconds.  The system may sleep even if your processing is not complete.
For example, your function could close active connections or serial
ports.

Handling \\+`post-wake' events offers more leeway.  Your function could
reestablish connections.

Note: Your code, or the functions it calls, should not raise any signals
or all hooks will be halted preventing other hook functions from
cleaning up or waking up.  You can wrap your code in a `condition-case'
block."
  :type 'hook
  :version "31.1")

;;;###autoload
(defun system-sleep-block-sleep (&optional why allow-display-sleep)
  "Inhibit system idle sleep.
Optional WHY is a string that identifies a sleep block to system utility
commands that inspect system-wide blocks.  WHY defaults to \"Emacs\".

Optional ALLOW-DISPLAY-SLEEP, when non-nil, allows the display to sleep
or a screen saver to run while the system idle sleep is blocked.  The
default is to keep the display active.

Return a sleep blocking token.  You must retain this value and provide
it to `system-sleep-unblock-sleep' to unblock its associated block.

Return nil if system sleep cannot be inhibited.

Note: All active blocks are released when the Emacs process dies.
Despite this, you should unblock your blocks when your processing is
complete.  See `with-system-sleep-block' for an easy way to do that."
  (when system-sleep--back-end
    (system-sleep--block-sleep (or why "Emacs") allow-display-sleep)))

;;;###autoload
(defun system-sleep-unblock-sleep (token)
  "Unblock the system sleep block associated with TOKEN.
Return non-nil TOKEN was unblocked, or nil if not.
In the unlikely event that unblock fails, the block will be released
when the Emacs process dies."
  (when system-sleep--back-end
    (system-sleep--unblock-sleep token)))

;;;###autoload
(defmacro with-system-sleep-block (&optional why allow-display-sleep &rest body)
  "Execute the forms in BODY while blocking system sleep.
The optional arguments WHY and ALLOW-DISPLAY-SLEEP have the same meaning
as in `system-sleep-block-sleep', which see.
The block is unblocked when BODY completes."
  (declare (indent 1) (debug t))
  (let ((token (make-symbol "--sleep-token--")))
    `(let ((,token (system-sleep-block-sleep ,why ,allow-display-sleep)))
       (unwind-protect
           (progn
             ,@body)
         (system-sleep-unblock-sleep ,token)))))

(defun system-sleep-unblock-all-sleep-blocks ()
  "Unblock all `system-sleep' blocks."
  (while system-sleep--sleep-block-tokens
    (system-sleep-unblock-sleep (car system-sleep--sleep-block-tokens))))

;;;###autoload
(defun system-sleep-sleep-blocked-p ()
  "Return non-nil if there are active sleep blocks."
  (and system-sleep--back-end
       system-sleep--sleep-block-tokens))


;; Internal implementation.

(defun system-sleep--set-back-end ()
  "Determine sleep/wake host system type."
  ;; Order matters to accommodate the cases where an NS or MS-Windows
  ;; build have the dbus feature.
  (setq system-sleep--back-end
        (cond ((featurep 'ns) 'ns)
              ((featurep 'w32) 'w32)
              ((and (require 'dbus)
                    (featurep 'dbusbind))
               'dbus)
              (t nil))))

(defvar system-sleep--event-after-hook-functions nil)
(defvar system-sleep--event-in-progress nil)
(defvar system-sleep--event-queue nil)

(defun system-sleep--sleep-event-handler (event)
  "Handle <sleep-event> special events and avoid races."
  (declare (completion ignore))
  (interactive "e")
  ;; Queue incoming event.
  (setq system-sleep--event-queue
        (append system-sleep--event-queue (list event)))
  ;; If an event is already in progress, return right away.
  ;; Otherwise, process queued events.
  (while (and (not system-sleep--event-in-progress)
              system-sleep--event-queue)
    (let ((current-event (pop system-sleep--event-queue)))
      (setq system-sleep--event-in-progress current-event)
      (unwind-protect
          (progn
            (run-hook-with-args 'system-sleep-event-functions
                                current-event)
            (run-hook-with-args 'system-sleep--event-after-hook-functions
                                current-event))
        (setq system-sleep--event-in-progress nil)))))

(defun system-sleep-enable ()
  "Enable `system-sleep'."
  (unless system-sleep--back-end
    (if (and (system-sleep--set-back-end)
             (system-sleep--enable))
        (keymap-set special-event-map "<sleep-event>"
                    #'system-sleep--sleep-event-handler)
      (warn "`system-sleep' could not be initialized"))))

(defun system-sleep-disable ()
  "Disable `system-sleep'."
  (when system-sleep--back-end
    (keymap-set special-event-map "<sleep-event>" #'ignore)
    (system-sleep-unblock-all-sleep-blocks)
    (system-sleep--disable)
    (setq system-sleep--back-end nil)))

(cl-defgeneric system-sleep--enable ()
  "Enable the `system-sleep' back end.
Return t if the back end is initialized, or nil.")

(cl-defgeneric system-sleep--disable ()
  "Disable the sleep/wake back end.")

(cl-defgeneric system-sleep--block-sleep (why allow-display-sleep)
  "Inhibit system idle sleep.
WHY is a string that identifies a sleep block to system utility commands
that inspect system-wide blocks.
When non-nil, ALLOW-DISPLAY-SLEEP allows the display to sleep or a
screen saver to run while the system idle sleep is blocked.  The default
is to keep the display active.
Return a sleep-block token.")

(cl-defgeneric system-sleep--unblock-sleep (token)
  "Unblock the system sleep block associated with TOKEN.
Return non-nil TOKEN was unblocked, or nil if not.")


;; D-Bus support.

(defvar system-sleep--dbus-sleep-inhibitor-types "sleep"
  "This is a colon-separated list of options.
The default is \"sleep\" which is compatible with the other supported
`system-sleep' platforms.  This could also be
\"sleep:shutdown\". Shutdown is available only on D-Bus systems.")

(defvar system-sleep--dbus-delay-lock nil)
(defvar system-sleep--dbus-pre-sleep-signal nil)
(defvar system-sleep--dbus-has-screensaver nil)

(defun system-sleep--dbus-delay-lock (make-or-close)
  (cond (make-or-close
         (if system-sleep--dbus-delay-lock
             (error "Delay lock should be nil")
           (setq system-sleep--dbus-delay-lock
                 (dbus-call-method
                  :system
                  "org.freedesktop.login1"
                  "/org/freedesktop/login1"
                  "org.freedesktop.login1.Manager"
                  "Inhibit"
                  :keep-fd
                  system-sleep--dbus-sleep-inhibitor-types
                  dbus-service-emacs
                  "Emacs sleep event watcher"
                  "delay"))))
        (t
         (when system-sleep--dbus-delay-lock
           (dbus--fd-close system-sleep--dbus-delay-lock)
           (setq system-sleep--dbus-delay-lock nil)))))

(defun system-sleep--dbus-prepare-for-sleep-callback (sleep-or-wake)
  (cond (sleep-or-wake
         (insert-special-event (make-sleep-event 'pre-sleep)))
        (t
         (insert-special-event (make-sleep-event 'post-wake)))))

(defun system-sleep--dbus-prepare-for-sleep-watcher (make-or-close)
  (cond (make-or-close
         (if system-sleep--dbus-pre-sleep-signal
             (error "PrepareForSleep watcher should be nil")
           (setq system-sleep--dbus-pre-sleep-signal
	         (dbus-register-signal
                  :system
	          "org.freedesktop.login1"
	          "/org/freedesktop/login1"
	          "org.freedesktop.login1.Manager"
	          "PrepareForSleep"
	          #'system-sleep--dbus-prepare-for-sleep-callback))))
        (t
         (dbus-unregister-object system-sleep--dbus-pre-sleep-signal)
         (setq system-sleep--dbus-pre-sleep-signal nil))))

(defun system-sleep--dbus-prepare-for-sleep-function (event)
  (pcase (sleep-event-state event)
    ('pre-sleep
     (system-sleep--dbus-delay-lock nil))
    ('post-wake
     (system-sleep--dbus-delay-lock t))))

(cl-defmethod system-sleep--enable (&context
                                    (system-sleep--back-end (eql 'dbus)))
  (when (member "org.freedesktop.login1"
                (dbus-list-activatable-names :system))
    (setq system-sleep--dbus-has-screensaver
          (member "org.freedesktop.ScreenSaver"
                   (dbus-list-activatable-names :session)))
    (add-hook 'system-sleep--event-after-hook-functions
              #'system-sleep--dbus-prepare-for-sleep-function)
    (system-sleep--dbus-delay-lock t)
    (system-sleep--dbus-prepare-for-sleep-watcher t)
    t))

(cl-defmethod system-sleep--disable (&context
                                     (system-sleep--back-end (eql 'dbus)))
  (system-sleep--dbus-prepare-for-sleep-watcher nil)
  (system-sleep--dbus-delay-lock nil)
  (remove-hook 'system-sleep--event-after-hook-functions
               #'system-sleep--dbus-prepare-for-sleep-function))

(cl-defmethod system-sleep--block-sleep (why
                                         allow-display-sleep
                                         &context
                                         (system-sleep--back-end (eql 'dbus)))
  (let ((subtokens))
    (if-let* ((sleep-cookie (dbus-call-method
                             :system
                             "org.freedesktop.login1"
                             "/org/freedesktop/login1"
                             "org.freedesktop.login1.Manager"
                             "Inhibit"
                             :keep-fd
                             system-sleep--dbus-sleep-inhibitor-types
                             dbus-service-emacs
                             why
                             "block")))
        (progn
          (let ((inhibit-quit t))
            (push (cons 'dbus-inhibitor-lock sleep-cookie) subtokens))
          (unless (or allow-display-sleep
                      (not system-sleep--dbus-has-screensaver))
            (if-let* ((screen-cookie
                       (dbus-call-method
                        :session
                        "org.freedesktop.ScreenSaver"
                        "/org/freedesktop/ScreenSaver"
                        "org.freedesktop.ScreenSaver"
                        "Inhibit"
                        dbus-service-emacs
                        "Screen Saver Block")))
                (let ((inhibit-quit t))
                  (push (cons 'dbus-screensaver-lock screen-cookie) subtokens))
              (warn "Unable to block the screen saver")))
          (let ((inhibit-quit t))
            (let ((token (list :system 'dbus :why why :subtokens subtokens)))
              (push token system-sleep--sleep-block-tokens)
              token)))
      (warn "Unable to block system sleep"))))

(cl-defmethod system-sleep--unblock-sleep (token
                                           &context
                                           (system-sleep--back-end (eql 'dbus)))

  (if (memq token system-sleep--sleep-block-tokens)
      (progn
        (let ((inhibit-quit t))
          (setq system-sleep--sleep-block-tokens
                (remq token system-sleep--sleep-block-tokens)))
        (dolist (subtoken (plist-get token :subtokens))
          (pcase (car subtoken)
            ('dbus-inhibitor-lock
             (dbus--fd-close (cdr subtoken)))
            ('dbus-screensaver-lock
             (dbus-call-method
              :session
              "org.freedesktop.ScreenSaver"
              "/org/freedesktop/ScreenSaver"
              "org.freedesktop.ScreenSaver"
              "UnInhibit"
              (cdr subtoken)))))
        t)
    (warn "Unknown `system-sleep' sleep token")
    nil))


;; macOS/GNUstep NS support.

(declare-function ns-block-system-sleep "nsfns.m")
(declare-function ns-unblock-system-sleep "nsfns.m")

(cl-defmethod system-sleep--enable (&context
                                    (system-sleep--back-end (eql 'ns)))
  t)

(cl-defmethod system-sleep--disable (&context
                                     (system-sleep--back-end (eql 'ns)))
  (ignore))

(cl-defmethod system-sleep--block-sleep (why
                                         allow-display-sleep
                                         &context
                                         (system-sleep--back-end (eql 'ns)))
  (if-let* ((cookie (ns-block-system-sleep why allow-display-sleep))
            (token (list :system 'ns :why why
                         :token (cons 'ns-sleep-block cookie))))
      (progn
        (let ((inhibit-quit t))
          (push token system-sleep--sleep-block-tokens))
        token)
    (warn "Unable to block system sleep")))

(cl-defmethod system-sleep--unblock-sleep (token
                                           &context
                                           (system-sleep--back-end (eql 'ns)))
  (if (memq token system-sleep--sleep-block-tokens)
      (progn
        (let ((inhibit-quit t))
          (setq system-sleep--sleep-block-tokens
                (remq token system-sleep--sleep-block-tokens)))
        (if (ns-unblock-system-sleep (cdr (plist-get token :token)))
            t
          (warn "Unable to unblock system sleep (blocks are released when Emacs dies)")
          nil))
    (warn "Unknown `system-sleep' sleep token")
    nil))


;; MS-Windows support.

(declare-function w32-block-system-sleep "w32fns.c")
(declare-function w32-unblock-system-sleep "w32fns.c")
(declare-function w32-system-sleep-block-count "w32fns.c")

(defvar system-sleep--w32-sleep-block-count 0)

(cl-defmethod system-sleep--enable (&context
                                    (system-sleep--back-end (eql 'w32)))
  t)

(cl-defmethod system-sleep--disable (&context
                                     (system-sleep--back-end (eql 'w32)))
  (ignore))

(cl-defmethod system-sleep--block-sleep (why
                                         allow-display-sleep
                                         &context
                                         (system-sleep--back-end (eql 'w32)))
  (if-let* ((cookie (w32-block-system-sleep allow-display-sleep))
            (token (list :system 'w32 :why why
                         :token (cons 'w32-sleep-block cookie))))
      (progn
        (let ((inhibit-quit t))
          (push token system-sleep--sleep-block-tokens))
        token)
    (warn "Unable to block system sleep")))

(cl-defmethod system-sleep--unblock-sleep (token
                                           &context
                                           (system-sleep--back-end (eql 'w32)))
  (if (memq token system-sleep--sleep-block-tokens)
      (progn
        (let ((inhibit-quit t))
          (setq system-sleep--sleep-block-tokens
                (remq token system-sleep--sleep-block-tokens)))
        (if (eq 0 (w32-system-sleep-block-count))
            (warn "Unable to unblock system sleep (no active tokens)")
          (if (w32-unblock-system-sleep)
              t
            (warn "Unable to unblock system sleep (blocks are released when Emacs dies)")
            nil)))
    (warn "Unknown `system-sleep' sleep token")
    nil))


;; Initialize system-sleep.

(system-sleep-enable)

(provide 'system-sleep)

;;; system-sleep.el ends here
