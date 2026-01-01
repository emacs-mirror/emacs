;;; system-taskbar.el --- System GUI taskbar/dock/launcher status display -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Stephane Marks
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

;; Use this package to display a taskbar icon "badge" overlay, a
;; progress bar report overlay, or alert the user that an Emacs session
;; needs attention, often by flashing or bouncing the Emacs application
;; icon.
;;
;; Note: The term taskbar is inclusive of dock or launcher or some other
;; terminology as may be used for your system.
;;
;; On GNU/Linux, the visible effects will appear on the destinations
;; determined by your shell extension, most often the application
;; launcher or dock panel, or the top panel.  Effects are global for an
;; Emacs instance.
;;
;; On macOS/GNUstep, the effects will appear on the Dock and in the App
;; Switcher.  Effects are global for an Emacs instance.
;;
;; On MS-Windows, the effects appear on the taskbar.  Effects are
;; associated with the frame from which they are initiated.

;;; Usage:

;; The global minor mode `system-taskbar-mode' initializes the GUI
;; platform back-end and must be enabled before using the functions
;; below.
;;
;; `system-taskbar-badge' overlays a count, which is an integer, on the
;; Emacs taskbar icon.  You can use this, for example, to indicate the
;; number of unread email messages.  On GNU/Linux, the count must be an
;; integer or nil.  On macOS/GNUstep, the count may be an integer or a
;; string, which the operating system will abbreviate if too long.  On
;; MS-Windows, the taskbar badge will be abbreviated to three
;; characters; if the count is an integer outside the range -99 to 99,
;; it is shown as "-99" or "99+", if count is a string longer than 3
;; characters it is truncated.
;;
;; `system-taskbar-attention' flashes or bounces the Emacs taskbar icon
;; to indicate that your Emacs session wants attention.  Its behaviors
;; are back-end specific.
;;
;; `system-taskbar-progress' overlays a graphical progress bar on the
;; Emacs taskbar icon to illustrate progress of a potentially
;; long-running operation.
;;
;; When `system-taskbar-mode' is enabled, Emacs progress reporters will
;; be enhanced to display taskbar GUI progress bars.  Customize
;; `system-taskbar-use-progress-reporter' if you want to disable this
;; before enabling `system-taskbar-mode'.
;;
;; On GNU/Linux systems, taskbar effects will appear on the GUI
;; window-system destinations determined by your shell extension, most
;; often the application launcher or dock panel, or the top panel.
;; Taskbar effects are global for an Emacs instance.  The GNU/Linux
;; implementation sends taskbar messages to the system GUI using D-Bus.
;; You may need to install or configure shell extensions such as
;; https://extensions.gnome.org/extension/307/dash-to-dock/ that
;; implement Ubuntu's Unity D-Bus launcher spec which you can read more
;; about here https://wiki.ubuntu.com/Unity/LauncherAPI.
;;
;; Your Linux Emacs instance should be launched via an appropriate shell
;; "desktop" file such as those distributed with Emacs; e.g.,
;; "etc/emacsclient.desktop" as documented here
;; https://specifications.freedesktop.org/desktop-entry/latest/ and
;; which your GUI system should implement.
;;
;; On macOS/GNUstep 10.5+, taskbar effects appear on the Dock and in the
;; App Switcher.  Effects are global for an Emacs instance.
;; macOS/GNUstep is implemented via its native API and needs no special
;; configuration.
;;
;; On MS-Windows 7+, taskbar effects appear on the Windows taskbar.
;; Effects are associated with the frame from which they are initiated.
;; MS-Windows is implemented via its native API and needs no special
;; configuration.
;;
;; To add support for additional systems, provide a back end that
;; implements the cl-generic functions below.

;;; Code:

(require 'dbus)

(defgroup system-taskbar nil
  "System GUI taskbar icon badge, progress report, alerting."
  :group 'convenience
  :version "31.1")

(defcustom system-taskbar-use-progress-reporter t
  "Supplement progress-reporters with GUI taskbar icon progress bars.
Set this before enabling `system-taskbar-mode'."
  :type 'boolean
  :version "31.1")

(defcustom system-taskbar-clear-attention-on-frame-focus t
  "Clear the icon attention indicator when any GUI frame is focused.
Back ends that automatically clear the attention indicator, such as
macOS/GNUstep and MS-Windows, ignore this option."
  :type 'boolean
  :version "31.1")

(defcustom system-taskbar-dbus-desktop-file-name "emacsclient"
  "D-Bus desktop file base name for the system taskbar destination.
This should be the base name of the desktop file used to launch an Emacs
instance.  For example, if your launcher desktop file is called
\"emacs.desktop\", this option should be \"emacs\"."
  :type 'string
  :version "31.1")

(defcustom system-taskbar-dbus-timeout nil
  "Number of milliseconds to wait for D-Bus responses.
If nil, use the D-Bus default timeout which is 25,000 (i.e., 25s).

If your D-Bus desktop extension needs extra time to respond, in which
case `system-taskbar-mode' might not initialize or related functions
might not take visible effect, bind this to a value higher than 25,000
to find what works for your system."
  :type '(choice (const :tag "Default" nil) natnum)
  :version "31.1")

(defun system-taskbar-progress-reporter-install ()
  "Install system taskbar progress reporter."
  (add-hook 'progress-reporter-update-functions
            #'system-taskbar--progress-reporter-update))

(defun system-taskbar-progress-reporter-remove ()
  "Remove system taskbar progress reporter."
  (remove-hook 'progress-reporter-update-functions
               #'system-taskbar--progress-reporter-update))

(defvar system-taskbar--back-end nil
  "Generic taskbar method system dispatcher.")

;;;###autoload
(define-minor-mode system-taskbar-mode
  "System GUI taskbar icon badge, progress report, alerting."
  :global t
  (when noninteractive
    (warn "Batch mode does not support `system-taskbar'"))
  (cond (system-taskbar-mode
         (if (and (system-taskbar--set-back-end)
                  (system-taskbar--enable))
             (when system-taskbar-use-progress-reporter
               (system-taskbar-progress-reporter-install))
           (setq system-taskbar-mode nil)
           (warn "`system-taskbar' could not be initialized")))
        (t
         (system-taskbar-progress-reporter-remove)
         (when system-taskbar--back-end
           (system-taskbar--badge nil)
           (system-taskbar--attention nil)
           (system-taskbar--progress nil)
           (system-taskbar--disable)
           (setq system-taskbar--back-end nil)))))

(defun system-taskbar-badge (&optional count)
  "Display COUNT as an overlay on the system taskbar Emacs icon.
If COUNT is an integer, display that.
If COUNT is a string on back ends that support strings, display that.
The string should be short.
On back ends which do not support strings, convert COUNT to an integer
using `string-to-number' and testing `integerp', or nil if that fails.
If COUNT is nil or an empty string, remove the counter."
  (when system-taskbar-mode
    (system-taskbar--badge count)))

(defun system-taskbar-attention (&optional urgency timeout)
  "Flash the system taskbar icon and/or frame to alert the user.
URGENCY can be one of the symbols `informational', or `critical'.
If URGENCY is nil, clear the attention indicator.

The attention indicator is cleared by the earliest of bringing the Emacs
GUI into focus, or after TIMEOUT seconds.  If TIMEOUT is nil, the system
GUI behavior has priority.

On some back ends, `critical' has the same effect as `informational'."
  (when system-taskbar-mode
    (system-taskbar--attention urgency timeout)))

(defun system-taskbar-progress (&optional progress)
  "Display a progress indicator overlay on the system taskbar icon.
PROGRESS is a float in the range 0.0 to 1.0.
If PROGRESS is nil, remove the progress indicator."
  (when system-taskbar-mode
    (system-taskbar--progress progress)))


;; Internal implementation.

(defun system-taskbar--set-back-end ()
  "Determine taskbar host system type."
  ;; Order matters to accommodate the cases where an NS or MS-Windows
  ;; build have the dbus feature.
  (setq system-taskbar--back-end
        (cond ((boundp 'ns-version-string) 'ns)
              ((bound-and-true-p w32-initialized) 'w32)
              ((and (featurep 'dbusbind)
                    (member "org.freedesktop.login1"
                            (dbus-list-activatable-names :system)))
               'dbus)
              (t nil))))

(cl-defgeneric system-taskbar--enable ()
  "Enable the system taskbar back end.
Return t if the back end is initialized, or nil.")

(cl-defgeneric system-taskbar--disable ()
  "Disable the system taskbar back end.")

(cl-defgeneric system-taskbar--badge (&optional count)
  "Display COUNT as an overlay on the system taskbar Emacs icon.
If COUNT is an integer, display that.
If COUNT is a string on back ends that support strings, display that.
The string should be short.
On back ends which do not support strings, convert COUNT to an integer
using `string-to-number' and testing `integerp', or nil if that fails.
If COUNT is nil or an empty string, remove the counter.")

(cl-defgeneric system-taskbar--attention (&optional urgency timeout)
  "Flash the system taskbar icon and/or frame to alert the user.
URGENCY can be one of the symbols `informational', or `critical'.
If URGENCY is nil, clear the attention indicator.

The attention indicator is cleared by the earliest of bringing the Emacs
GUI into focus, or after TIMEOUT seconds.  If TIMEOUT is nil, the system
GUI behavior has priority.

On some back ends, `critical' has the same effect as `informational'.

On some back ends, attention will be displayed only if Emacs is not the
currently focused application.")

(cl-defgeneric system-taskbar--progress (&optional progress)
  "Display a progress indicator overlay on the system taskbar icon.
PROGRESS is a float in the range 0.0 to 1.0.
If PROGRESS is nil, remove the progress indicator.")

(defun system-taskbar--validate-progress (progress)
  "Return PROGRESS as a float in the range 0.0 to 1.0, or nil."
  (when (natnump progress)
    (setq progress (float progress)))
  (when (and progress (>= progress 0.0) (<= progress 1.0))
    progress))


;; `progress-reporter' support.

(defun system-taskbar--progress-reporter-update (_reporter state)
  "Progress reporter system taskbar update function.
REPORTER and STATE are the same as in
`progress-reporter-update-functions'."
  (when system-taskbar-mode
    (pcase state
      ((pred floatp)
       (system-taskbar--progress state))
      ((pred integerp)
       (system-taskbar--progress (/ (1+ state) 4.0)))
      ('done
       (system-taskbar--progress nil)))))


;; D-Bus support.

(defconst system-taskbar--dbus-service "com.canonical.Unity")
(defconst system-taskbar--dbus-interface "com.canonical.Unity.LauncherEntry")

(defvar system-taskbar--dbus-attention nil
  "Non-nil when attention is requested.")

(defun system-taskbar--dbus-send-signal (message)
  "Send MESSAGE to the D-Bus system taskbar service."
  (let ((app-uri
         (format "application://%s.desktop"
                 system-taskbar-dbus-desktop-file-name)))
    (dbus-send-signal
     :session
     system-taskbar--dbus-service
     "/"
     system-taskbar--dbus-interface
     "Update"
     app-uri
     message)))

(defun system-taskbar--dbus-clear-attention-on-frame-focus ()
  "Clear an active D-Bus attention request if any frame is focused."
  (when (and system-taskbar--dbus-attention
             (catch :clear
               (dolist (frame (frame-list))
                 (when (eq (frame-focus-state frame) t)
                   (throw :clear t)))))
    (system-taskbar-attention nil)))

(defun system-taskbar-dbus-ping-service ()
  "Return non-nil if `system-taskbar--dbus-service' responds.
Return nil if no response within `system-taskbar-dbus-timeout'."
  (dbus-ping
   :session
   system-taskbar--dbus-service
   system-taskbar-dbus-timeout))

(cl-defmethod system-taskbar--enable (&context
                                      (system-taskbar--back-end (eql 'dbus)))
  (if (system-taskbar-dbus-ping-service)
      (progn
        (when system-taskbar-clear-attention-on-frame-focus
          (add-function :after after-focus-change-function
                        #'system-taskbar--dbus-clear-attention-on-frame-focus))
        t)
    (display-warning 'system-taskbar-dbus
                     (format-message "D-Bus service `%s' unavailable"
                                     system-taskbar--dbus-service))
    nil))

(cl-defmethod system-taskbar--disable (&context
                                       (system-taskbar--back-end (eql 'dbus)))
  (remove-function after-focus-change-function
                   #'system-taskbar--dbus-clear-attention-on-frame-focus))

(cl-defmethod system-taskbar--badge (&context
                                     (system-taskbar--back-end (eql 'dbus))
                                     &optional count)
  "Display COUNT as an overlay on the system taskbar Emacs icon.
If COUNT is an integer, display that.  If COUNT is a string, convert it
to an integer, or nil if that fails.  If COUNT is any other type, use
nil.  If COUNT is nil or an empty string, remove the badge.
Note: The Unity D-Bus protocol supports only integer badges."
  (cond ((stringp count)
         (if (string-empty-p count)
             (setq count nil)
           (let ((count-1 (string-to-number count)))
             (setq count (if (integerp count-1) count-1 nil)))))
        ((not (integerp count))
         (setq count nil)))
  (system-taskbar--dbus-send-signal
   `((:dict-entry "count-visible"
                  (:variant :boolean ,(not (null count))))
     (:dict-entry "count"
                  (:variant :uint32 ,(if (null count) 0
                                       count))))))

(cl-defmethod system-taskbar--attention (&context
                                         (system-taskbar--back-end (eql 'dbus))
                                         &optional urgency timeout)
  "Request URGENCY user attention on the system taskbar Emacs icon.
The request will time out within the TIMEOUT seconds interval.
The Unity D-Bus protocol does not support differentiated urgency levels."
  (setq system-taskbar--dbus-attention urgency)
  (system-taskbar--dbus-send-signal
   `((:dict-entry "urgent"
                  (:variant :boolean ,(not (null urgency))))))
  (when (and urgency timeout)
    (run-with-timer
     timeout
     nil
     #'system-taskbar-attention nil)))

(cl-defmethod system-taskbar--progress (&context
                                        (system-taskbar--back-end (eql 'dbus))
                                        &optional progress)
  "Display a progress bar overlay on the system taskbar icon.
PROGRESS is a float in the range 0.0 to 1.0.
If PROGRESS is nil, remove the progress bar."
  (setq progress (system-taskbar--validate-progress progress))
  (system-taskbar--dbus-send-signal
   `((:dict-entry "progress-visible"
                  (:variant :boolean ,(not (null progress))))
     (:dict-entry "progress"
                  (:variant :double ,(if (null progress) 0 progress))))))


;; macOS/GNUstep NS support.

(declare-function ns-badge "nsfns.m")
(declare-function ns-request-user-attention "nsfns.m")
(declare-function ns-progress-indicator "nsfns.m")

(cl-defmethod system-taskbar--enable (&context
                                      (system-taskbar--back-end (eql 'ns)))
  t)

(cl-defmethod system-taskbar--disable (&context
                                       (system-taskbar--back-end (eql 'ns)))
  (ignore))

(cl-defmethod system-taskbar--badge (&context
                                     (system-taskbar--back-end (eql 'ns))
                                     &optional count)
  "Display COUNT as an overlay on the Dock badge.
If COUNT is an integer or a non-empty string, display that.  If COUNT is
nil or an empty string, clear the badge overlay.
Note: NS will abbreviate long strings to fit the badge's allocated
space."
  (cond ((stringp count)
         (when (string-empty-p count)
           (setq count nil)))
        ((integerp count)
         (setq count (number-to-string count)))
        (t (setq count nil)))
  (ns-badge count))

(cl-defmethod system-taskbar--attention (&context
                                         (system-taskbar--back-end (eql 'ns))
                                         &optional urgency timeout)
  "Request URGENCY user attention on the Dock.
The attention indicator will be cleared after TIMEOUT seconds."
  (ns-request-user-attention urgency)
  (when (and urgency timeout)
    (run-with-timer
     timeout
     nil
     #'system-taskbar-attention nil)))

(cl-defmethod system-taskbar--progress (&context
                                        (system-taskbar--back-end (eql 'ns))
                                        &optional progress)
  "Display a progress bar overlay on the Dock and App Switcher.
PROGRESS is a float in the range 0.0 to 1.0.
If PROGRESS is nil, remove the progress bar."
  (ns-progress-indicator (system-taskbar--validate-progress progress)))


;; MS-Windows support.

(declare-function w32-badge "w32fns.c")
(declare-function w32-request-user-attention "w32fns.c")
(declare-function w32-progress-indicator "w32fns.c")

;; The background color should be similar to the color of the Emacs icon
;; shown on the taskbar, as that seems to be the convention on
;; MS-Windows.
(defvar system-taskbar-w32-badge-background "#ab82ff" ; MediumPurple1
  "w32 badge background RGB triple string.")

;; It looks like the convention is to use the black foreground.
(defvar system-taskbar-w32-badge-foreground "#000000" ; black
  "w32 badge foreground RGB triple string.")

(defun system-taskbar--w32-clear-frame-indicators (frame)
  ;; NOTE: Update the below if adding new w32 system taskbar functions.
  (with-selected-frame frame
    (system-taskbar-badge nil)
    (system-taskbar-attention nil)
    (system-taskbar-progress nil)))

(cl-defmethod system-taskbar--enable (&context
                                      (system-taskbar--back-end (eql 'w32)))
  ;; Clear system taskbar indicators for a frame when it is deleted.
  (add-hook 'delete-frame-functions
            #'system-taskbar--w32-clear-frame-indicators)
  t)

(cl-defmethod system-taskbar--disable (&context
                                       (system-taskbar--back-end (eql 'w32)))
  (remove-hook 'delete-frame-functions
               #'system-taskbar--w32-clear-frame-indicators))

(cl-defmethod system-taskbar--badge (&context
                                     (system-taskbar--back-end (eql 'w32))
                                     &optional count)
  "Display a COUNT overlay on the system taskbar icon.
The taskbar icon target is associated with the selected frame.

If COUNT is an integer or a non-empty string, display that.  If COUNT is
nil or an empty string, clear the badge.

Due to MS-Windows icon overlay size limitations, if COUNT is an integer
and is outside the range -99 to 99, display \"-99\" and \"99+\",
respectively, if COUNT is a string longer than 2 characters truncate it
using `truncate-string-to-width'.

Consult `system-taskbar-w32-badge-background' and
`system-taskbar-w32-badge-foreground' for the background and foreground
colors for the painted overlay."
  (cond ((stringp count)
         (if (string-empty-p count)
             (setq count nil)
           (when (length> count 2)
             (setq count (truncate-string-to-width count 3 0 nil t)))))
        ((integerp count)
         (if (and (> count -100)
                  (< count 100))
             (setq count (number-to-string count))
           (if (< count 0)
               (setq count "-99")
             (setq count "99+"))))
        (t (setq count nil)))
  (w32-badge count
             system-taskbar-w32-badge-background
             system-taskbar-w32-badge-foreground))

(cl-defmethod system-taskbar--attention (&context
                                         (system-taskbar--back-end (eql 'w32))
                                         &optional urgency timeout)
  "Request URGENCY user attention on the system taskbar icon.
Indicate the icon associated with the selected frame.
If URGENCY is the symbol `informational', flash the taskbar icon.
If URGENCY is the symbol `critical', flash the taskbar icon and the
MS-Windows window frame.
Clear attention indicator after TIMEOUT seconds.  If TIMEOUT is nil,
default to MS-Windows default behavior."
  (w32-request-user-attention urgency)
  (when (and urgency timeout)
    (run-with-timer
     timeout
     nil
     #'system-taskbar-attention nil)))

(cl-defmethod system-taskbar--progress (&context
                                        (system-taskbar--back-end (eql 'w32))
                                        &optional progress)
  "Display a progress bar on the system taskbar icon.
PROGRESS is a float in the range 0.0 to 1.0.
If PROGRESS is nil, remove the progress bar."
  (w32-progress-indicator (system-taskbar--validate-progress progress)))



(provide 'system-taskbar)

;;; system-taskbar.el ends here
