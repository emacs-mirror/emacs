;;; gnus-dbus.el --- DBUS integration for Gnus       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Obsolete-since: 31.1

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

;; This library is obsolete.
;;
;; Use `gnus-close-on-sleep' instead.

;; This library contains some Gnus integration for systems using DBUS.
;; At present it registers a signal to close all Gnus servers before
;; system sleep or hibernation.

;;; Code:

(require 'gnus)
(require 'dbus)
(declare-function gnus-close-all-servers "gnus-start")

;; (defcustom gnus-dbus-close-on-sleep nil
;;   "When non-nil, close Gnus servers on system sleep."
;;   :group 'gnus-dbus
;;   :type 'boolean)

;; It is suggested in the elisp documention that we create variable
;; alias's before executing the `defcustom'.  To reliably accomplish
;; that in this case we would have to edit gnus-start.el which I don't
;; want to do.  So I've done this instead.
(require 'gnus-start) ;; Run defcustom for `gnus-close-on-sleep'
(when (bound-and-true-p gnus-dbus-close-on-sleep)
  (setq gnus-close-on-sleep gnus-dbus-close-on-sleep))
(with-suppressed-warnings ((suspicious nil)) ;; Doesn't like aliasing bound variables
  (define-obsolete-variable-alias 'gnus-dbus-close-on-sleep 'gnus-close-on-sleep "31.1"))

(defvar gnus-dbus-sleep-registration-object nil
  "Object returned from `dbus-register-signal'.
Used to unregister the signal.")
(make-obsolete-variable 'gnus-dbus-sleep-registration-object nil "31.1")

(defun gnus-dbus-register-sleep-signal ()
  "Use `dbus-register-signal' to close servers on sleep."
  (declare (obsolete nil "31.1"))
  (when (featurep 'dbusbind)
    (setq gnus-dbus-sleep-registration-object
	  (dbus-register-signal :system
				"org.freedesktop.login1"
				"/org/freedesktop/login1"
				"org.freedesktop.login1.Manager"
				"PrepareForSleep"
				#'gnus-dbus-sleep-handler))
    (gnus-add-shutdown #'gnus-dbus-unregister-sleep-signal 'gnus)))

(defun gnus-dbus-sleep-handler (sleep-start)
  ;; Sleep-start is t before sleeping.
  (declare (obsolete nil "31.1"))
  (when (and sleep-start
	     (gnus-alive-p))
    (condition-case nil
	(gnus-close-all-servers)
      (error nil))))

(defun gnus-dbus-unregister-sleep-signal ()
  (declare (obsolete gnus-sleep-handler "31.1"))
  (condition-case nil
      (dbus-unregister-object
       gnus-dbus-sleep-registration-object)
    (wrong-type-argument nil)))

(provide 'gnus-dbus)
;;; gnus-dbus.el ends here
