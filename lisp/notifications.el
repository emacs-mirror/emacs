;;; notifications.el --- Client interface to desktop notifications.

;; Copyright (C) 2010-2018 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm desktop notifications

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

;; This package provides an implementation of the Desktop Notifications
;; <http://developer.gnome.org/notification-spec/>.

;; In order to activate this package, you must add the following code
;; into your .emacs:
;;
;;   (require 'notifications)

;; For proper usage, Emacs must be started in an environment with an
;; active D-Bus session bus.

;;; Code:
(require 'dbus)

(defconst notifications-specification-version "1.2"
  "The version of the Desktop Notifications Specification implemented.")

(defconst notifications-application-name "Emacs"
  "Default application name.")

(defconst notifications-application-icon
  (expand-file-name
   "images/icons/hicolor/scalable/apps/emacs.svg"
   data-directory)
  "Default application icon.")

(defconst notifications-service "org.freedesktop.Notifications"
  "D-Bus notifications service name.")

(defconst notifications-path "/org/freedesktop/Notifications"
  "D-Bus notifications service path.")

(defconst notifications-interface "org.freedesktop.Notifications"
  "D-Bus notifications service interface.")

(defconst notifications-notify-method "Notify"
  "D-Bus notifications notify method.")

(defconst notifications-close-notification-method "CloseNotification"
  "D-Bus notifications close notification method.")

(defconst notifications-get-capabilities-method "GetCapabilities"
  "D-Bus notifications get capabilities method.")

(defconst notifications-get-server-information-method "GetServerInformation"
  "D-Bus notifications get server information method.")

(defconst notifications-action-signal "ActionInvoked"
  "D-Bus notifications action signal.")

(defconst notifications-closed-signal "NotificationClosed"
  "D-Bus notifications closed signal.")

(defconst notifications-closed-reason
  '((1 expired)
    (2 dismissed)
    (3 close-notification)
    (4 undefined))
  "List of reasons why a notification has been closed.")

(defvar notifications-on-action-map nil
  "Mapping between notification and action callback functions.")

(defvar notifications-on-action-object nil
  "Object for registered on-action signal.")

(defvar notifications-on-close-map nil
  "Mapping between notification and close callback functions.")

(defvar notifications-on-close-object nil
  "Object for registered on-close signal.")

(defun notifications-on-action-signal (id action)
  "Dispatch signals to callback functions from `notifications-on-action-map'."
  (let* ((bus (dbus-event-bus-name last-input-event))
	 (unique-name (dbus-event-service-name last-input-event))
	 (entry (assoc (list bus unique-name id) notifications-on-action-map)))
    (when entry
      (funcall (cadr entry) id action)
      (when (and (not (setq notifications-on-action-map
			    (remove entry notifications-on-action-map)))
		 notifications-on-action-object)
	(dbus-unregister-object notifications-on-action-object)
	(setq notifications-on-action-object nil)))))

(defun notifications-on-closed-signal (id &optional reason)
  "Dispatch signals to callback functions from `notifications-on-closed-map'."
  ;; notification-daemon prior 0.4.0 does not send a reason.  So we
  ;; make it optional, and assume `undefined' as default.
  (let* ((bus (dbus-event-bus-name last-input-event))
	 (unique-name (dbus-event-service-name last-input-event))
	 (entry (assoc (list bus unique-name id) notifications-on-close-map))
	 (reason (or reason 4)))
    (when entry
      (funcall (cadr entry)
	       id (cadr (assoc reason notifications-closed-reason)))
      (when (and (not (setq notifications-on-close-map
			    (remove entry notifications-on-close-map)))
		 notifications-on-close-object)
	(dbus-unregister-object notifications-on-close-object)
	(setq notifications-on-close-object nil)))))

(defun notifications-notify (&rest params)
  "Send notification via D-Bus using the Freedesktop notification protocol.
Various PARAMS can be set:

 :bus            The D-Bus bus, if different from `:session'.
 :title          The notification title.
 :body           The notification body text.
 :app-name       The name of the application sending the notification.
                 Default to `notifications-application-name'.
 :replaces-id    The notification ID that this notification replaces.
 :app-icon       The notification icon.
                 Default is `notifications-application-icon'.
                 Set to nil if you do not want any icon displayed.
 :actions        A list of actions in the form:
                   (KEY TITLE KEY TITLE ...)
                 where KEY and TITLE are both strings.
                 The default action (usually invoked by clicking the
                 notification) should have a key named \"default\".
                 The title can be anything, though implementations are free
                 not to display it.
 :timeout        The timeout time in milliseconds since the display
                 of the notification at which the notification should
                 automatically close.
                 If -1, the notification's expiration time is dependent
                 on the notification server's settings, and may vary for
                 the type of notification.
                 If 0, the notification never expires.
                 Default value is -1.
 :urgency        The urgency level.
                 Either `low', `normal' or `critical'.
 :action-items   Whether the TITLE of the actions is interpreted as
                 a named icon.
 :category       The type of notification this is.
 :desktop-entry  This specifies the name of the desktop filename representing
                 the calling program.
 :image-data     This is a raw data image format which describes the width,
                 height, rowstride, has alpha, bits per sample, channels and
                 image data respectively.
 :image-path     This is represented either as a URI (file:// is the
                 only URI schema supported right now) or a name
                 in a freedesktop.org-compliant icon theme.
 :sound-file     The path to a sound file to play when the notification pops up.
 :sound-name     A themable named sound from the freedesktop.org sound naming
                 specification to play when the notification pops up.
                 Similar to icon-name,only for sounds. An example would
                 be \"message-new-instant\".
 :suppress-sound Causes the server to suppress playing any sounds, if it has
                 that ability.
 :resident       When set the server will not automatically remove the
                 notification when an action has been invoked.
 :transient      When set the server will treat the notification as transient
                 and by-pass the server's persistence capability, if it
                 should exist.
 :x              Specifies the X location on the screen that the notification
                 should point to.  The \"y\" hint must also be specified.
 :y              Specifies the Y location on the screen that the notification
                 should point to.  The \"x\" hint must also be specified.
 :on-action      Function to call when an action is invoked.
                 The notification id and the key of the action are passed
                 as arguments to the function.
 :on-close       Function to call when the notification has been closed
                 by timeout or by the user.
                 The function receive the notification id and the closing
                 reason as arguments:
                   - `expired' if the notification has expired
                   - `dismissed' if the notification was dismissed by the user
                   - `close-notification' if the notification was closed
                     by a call to CloseNotification
                   - `undefined' if the notification server hasn't provided
                     a reason

Which parameters are accepted by the notification server can be
checked via `notifications-get-capabilities'.

This function returns a notification id, an integer, which can be
used to manipulate the notification item with
`notifications-close-notification' or the `:replaces-id' argument
of another `notifications-notify' call."
  (with-demoted-errors
    (let ((bus (or (plist-get params :bus) :session))
	  (title (plist-get params :title))
	  (body (plist-get params :body))
	  (app-name (plist-get params :app-name))
	  (replaces-id (plist-get params :replaces-id))
	  (app-icon (plist-get params :app-icon))
	  (actions (plist-get params :actions))
	  (timeout (plist-get params :timeout))
	  ;; Hints
	  (hints '())
	  (urgency (plist-get params :urgency))
	  (category (plist-get params :category))
	  (desktop-entry (plist-get params :desktop-entry))
	  (image-data (plist-get params :image-data))
	  (image-path (plist-get params :image-path))
	  (action-items (plist-get params :action-items))
	  (sound-file (plist-get params :sound-file))
	  (sound-name (plist-get params :sound-name))
	  (suppress-sound (plist-get params :suppress-sound))
	  (resident (plist-get params :resident))
	  (transient (plist-get params :transient))
	  (x (plist-get params :x))
	  (y (plist-get params :y))
	  id)
      ;; Build hints array
      (when urgency
	(add-to-list 'hints `(:dict-entry
			      "urgency"
			      (:variant :byte ,(pcase urgency
						 (`low 0)
						 (`critical 2)
						 (_ 1)))) t))
      (when category
	(add-to-list 'hints `(:dict-entry
			      "category"
			      (:variant :string ,category)) t))
      (when desktop-entry
	(add-to-list 'hints `(:dict-entry
			      "desktop-entry"
			      (:variant :string ,desktop-entry)) t))
      (when image-data
	(add-to-list 'hints `(:dict-entry
			      "image-data"
			      (:variant :struct ,image-data)) t))
      (when image-path
	(add-to-list 'hints `(:dict-entry
			      "image-path"
			      (:variant :string ,image-path)) t))
      (when action-items
	(add-to-list 'hints `(:dict-entry
			      "action-items"
			      (:variant :boolean ,action-items)) t))
      (when sound-file
	(add-to-list 'hints `(:dict-entry
			      "sound-file"
			      (:variant :string ,sound-file)) t))
      (when sound-name
	(add-to-list 'hints `(:dict-entry
			      "sound-name"
			      (:variant :string ,sound-name)) t))
      (when suppress-sound
	(add-to-list 'hints `(:dict-entry
			      "suppress-sound"
			      (:variant :boolean ,suppress-sound)) t))
      (when resident
	(add-to-list 'hints `(:dict-entry
			      "resident"
			      (:variant :boolean ,resident)) t))
      (when transient
	(add-to-list 'hints `(:dict-entry
			      "transient"
			      (:variant :boolean ,transient)) t))
      (when x
	(add-to-list 'hints `(:dict-entry "x" (:variant :int32 ,x)) t))
      (when y
	(add-to-list 'hints `(:dict-entry "y" (:variant :int32 ,y)) t))

      ;; Call Notify method.
      (setq id
	    (dbus-call-method bus
			      notifications-service
			      notifications-path
			      notifications-interface
			      notifications-notify-method
			      :string (or app-name
					  notifications-application-name)
			      :uint32 (or replaces-id 0)
			      :string (if app-icon
					  (expand-file-name app-icon)
					;; If app-icon is nil because user
					;; requested it to be so, send the
					;; empty string
					(if (plist-member params :app-icon)
					    ""
					  ;; Otherwise send the
					  ;; default icon path
					  notifications-application-icon))
			      :string (or title "")
			      :string (or body "")
			      `(:array ,@actions)
			      (or hints '(:array :signature "{sv}"))
			      :int32 (or timeout -1)))

      ;; Register close/action callback function.  We must also
      ;; remember the daemon's unique name, because the daemon could
      ;; have restarted.
      (let ((on-action (plist-get params :on-action))
	    (on-close (plist-get params :on-close))
	    (unique-name (dbus-get-name-owner bus notifications-service)))
	(when on-action
	  (add-to-list 'notifications-on-action-map
		       (list (list bus unique-name id) on-action))
	  (unless notifications-on-action-object
	    (setq notifications-on-action-object
		  (dbus-register-signal
		   bus
		   nil
		   notifications-path
		   notifications-interface
		   notifications-action-signal
		   'notifications-on-action-signal))))

	(when on-close
	  (add-to-list 'notifications-on-close-map
		       (list (list bus unique-name id) on-close))
	  (unless notifications-on-close-object
	    (setq notifications-on-close-object
		  (dbus-register-signal
		   bus
		   nil
		   notifications-path
		   notifications-interface
		   notifications-closed-signal
		   'notifications-on-closed-signal)))))

      ;; Return notification id
      id)))

(defun notifications-close-notification (id &optional bus)
  "Close a notification with identifier ID.
BUS can be a string denoting a D-Bus connection, the default is `:session'."
  (dbus-call-method (or bus :session)
                    notifications-service
                    notifications-path
                    notifications-interface
                    notifications-close-notification-method
                    :uint32 id))

(defvar dbus-debug) ; used in the macroexpansion of dbus-ignore-errors

(defun notifications-get-capabilities (&optional bus)
  "Return the capabilities of the notification server, a list of symbols.
BUS can be a string denoting a D-Bus connection, the default is `:session'.
The following capabilities can be expected:

  :actions         The server will provide the specified actions
                   to the user.
  :action-icons    Supports using icons instead of text for
                   displaying actions.
  :body            Supports body text.
  :body-hyperlinks The server supports hyperlinks in the notifications.
  :body-images     The server supports images in the notifications.
  :body-markup     Supports markup in the body text.
  :icon-multi      The server will render an animation of all the
                   frames in a given image array.
  :icon-static     Supports display of exactly 1 frame of any
                   given image array.  This value is mutually exclusive
                   with `:icon-multi'.
  :persistence     The server supports persistence of notifications.
  :sound           The server supports sounds on notifications.

Further vendor-specific caps start with `:x-vendor', like `:x-gnome-foo-cap'."
  (dbus-ignore-errors
    (mapcar
     (lambda (x) (intern (concat ":" x)))
     (dbus-call-method (or bus :session)
		       notifications-service
		       notifications-path
		       notifications-interface
		       notifications-get-capabilities-method))))

(defun notifications-get-server-information (&optional bus)
  "Return information on the notification server, a list of strings.
BUS can be a string denoting a D-Bus connection, the default is `:session'.
The returned list is (NAME VENDOR VERSION SPEC-VERSION).

  NAME         The product name of the server.
  VENDOR       The vendor name.  For example, \"KDE\", \"GNOME\".
  VERSION      The server's version number.
  SPEC-VERSION The specification version the server is compliant with.

If SPEC_VERSION is missing, the server supports a specification
prior to \"1.0\".

See `notifications-specification-version' for the specification
version this library is compliant with."
  (dbus-ignore-errors
    (dbus-call-method (or bus :session)
		      notifications-service
		      notifications-path
		      notifications-interface
		      notifications-get-server-information-method)))

(provide 'notifications)
