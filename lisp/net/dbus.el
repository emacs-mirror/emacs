;;; -*- no-byte-compile: t; -*-
;;; dbus.el --- Elisp bindings for D-Bus.

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hardware

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides language bindings for the D-Bus API.  D-Bus
;; is a message bus system, a simple way for applications to talk to
;; one another.  See <http://dbus.freedesktop.org/> for details.

;; Low-level language bindings are implemented in src/dbusbind.c.

;;; Code:

(require 'xml)

(defconst dbus-service-dbus "org.freedesktop.DBus"
  "The bus name used to talk to the bus itself.")

(defconst dbus-path-dbus "/org/freedesktop/DBus"
  "The object path used to talk to the bus itself.")

(defconst dbus-interface-dbus "org.freedesktop.DBus"
  "The interface exported by the object with `dbus-service-dbus' and `dbus-path-dbus'.")

(defconst dbus-interface-introspectable "org.freedesktop.DBus.Introspectable"
  "The interface supported by introspectable objects.")


;;; Hash table of registered functions.

;; We create it here.  So we have a simple test in dbusbind.c, whether
;; the Lisp code has been loaded.
(setq dbus-registered-functions-table (make-hash-table :test 'equal))

(defun dbus-list-hash-table ()
  "Returns all registered signal registrations to D-Bus.
The return value is a list, with elements of kind (KEY . VALUE).
See `dbus-registered-functions-table' for a description of the
hash table."
  (let (result)
    (maphash
     '(lambda (key value) (add-to-list 'result (cons key value) 'append))
     dbus-registered-functions-table)
    result))

(defun dbus-name-owner-changed-handler (service old-owner new-owner)
  "Reapplies all signal registrations to D-Bus.
This handler is applied when a \"NameOwnerChanged\" signal has
arrived.  SERVICE is the object name for which the name owner has
been changed.  OLD-OWNER is the previous owner of SERVICE, or the
empty string if SERVICE was not owned yet.  NEW-OWNER is the new
owner of SERVICE, or the empty string if SERVICE looses any name owner."
  (save-match-data
    ;; Check whether SERVICE is a known name.
    (when (and (stringp service) (not (string-match "^:" service))
	       (stringp old-owner) (stringp new-owner))
      (maphash
       '(lambda (key value)
	  (dolist (elt value)
	    ;; key has the structure (BUS INTERFACE SIGNAL).
	    ;; elt has the structure (UNAME SERVICE PATH HANDLER).
	    (when (string-equal old-owner (car elt))
	      ;; Remove old key, and add new entry with changed name.
	      (dbus-unregister-signal (list key (cdr elt)))
	      ;; Maybe we could arrange the lists a little bit better
	      ;; that we don't need to extract every single element?
	      (dbus-register-signal
	       ;; BUS      SERVICE     PATH
	       (nth 0 key) (nth 1 elt) (nth 2 elt)
	       ;; INTERFACE SIGNAL     HANDLER
	       (nth 1 key) (nth 2 key) (nth 3 elt)))))
       (copy-hash-table dbus-registered-functions-table)))))

;; Register the handler.
(condition-case nil
    (progn
      (dbus-register-signal
       :system dbus-service-dbus dbus-path-dbus dbus-interface-dbus
       "NameOwnerChanged" 'dbus-name-owner-changed-handler)
      (dbus-register-signal
       :session dbus-service-dbus dbus-path-dbus dbus-interface-dbus
       "NameOwnerChanged" 'dbus-name-owner-changed-handler))
  (dbus-error))


;;; D-Bus events.

(defun dbus-check-event (event)
  "Checks whether EVENT is a well formed D-Bus event.
EVENT is a list which starts with symbol `dbus-event':

     (dbus-event BUS SERVICE PATH INTERFACE MEMBER HANDLER &rest ARGS)

BUS identifies the D-Bus the signal is coming from.  It is either
the symbol `:system' or the symbol `:session'.  SERVICE and PATH
are the unique name and the object path of the D-Bus object
emitting the signal.  INTERFACE and MEMBER denote the signal
which has been sent.  HANDLER is the function which has been
registered for this signal.  ARGS are the arguments passed to
HANDLER, when it is called during event handling in
`dbus-handle-event'.

This function raises a `dbus-error' signal in case the event is
not well formed."
  (when dbus-debug (message "DBus-Event %s" event))
  (unless (and (listp event)
	       (eq (car event) 'dbus-event)
	       ;; Bus symbol.
	       (symbolp (nth 1 event))
	       ;; Service.
	       (stringp (nth 2 event))
	       ;; Object path.
	       (stringp (nth 3 event))
	       ;; Interface.
	       (stringp (nth 4 event))
	       ;; Member.
	       (stringp (nth 5 event))
	       ;; Handler.
	       (functionp (nth 6 event)))
    (signal 'dbus-error (list "Not a valid D-Bus event" event))))

;;;###autoload
(defun dbus-handle-event (event)
  "Handle events from the D-Bus.
EVENT is a D-Bus event, see `dbus-check-event'.  HANDLER, being
part of the event, is called with arguments ARGS."
  (interactive "e")
  ;; We don't want to raise an error, because this function is called
  ;; in the event handling loop.
  (condition-case nil
      (progn
	(dbus-check-event event)
	(apply (nth 6 event) (nthcdr 7 event)))
    (dbus-error)))

(defun dbus-event-bus-name (event)
  "Return the bus name the event is coming from.
The result is either the symbol `:system' or the symbol `:session'.
EVENT is a D-Bus event, see `dbus-check-event'.  This function
raises a `dbus-error' signal in case the event is not well
formed."
  (dbus-check-event event)
  (nth 1 event))

(defun dbus-event-service-name (event)
  "Return the name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 2 event))

(defun dbus-event-path-name (event)
  "Return the object path of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 3 event))

(defun dbus-event-interface-name (event)
  "Return the interface name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 4 event))

(defun dbus-event-member-name (event)
  "Return the member name the event is coming from.
It is either a signal name or a method name. The result is is a
string.  EVENT is a D-Bus event, see `dbus-check-event'.  This
function raises a `dbus-error' signal in case the event is not
well formed."
  (dbus-check-event event)
  (nth 5 event))


;;; D-Bus registered names.

(defun dbus-list-activatable-names ()
  "Return the D-Bus service names which can be activated as list.
The result is a list of strings, which is nil when there are no
activatable service names at all."
  (condition-case nil
      (dbus-call-method
       :system dbus-service-dbus
       dbus-path-dbus dbus-interface-dbus "ListActivatableNames")
    (dbus-error)))

(defun dbus-list-names (bus)
  "Return the service names registered at D-Bus BUS.
The result is a list of strings, which is nil when there are no
registered service names at all.  Well known names are strings like
\"org.freedesktop.DBus\".  Names starting with \":\" are unique names
for services."
  (condition-case nil
      (dbus-call-method
       bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus "ListNames")
    (dbus-error)))

(defun dbus-list-known-names (bus)
  "Retrieve all services which correspond to a known name in BUS.
A service has a known name if it doesn't start with \":\"."
  (let (result)
    (dolist (name (dbus-list-names bus) result)
      (unless (string-equal ":" (substring name 0 1))
	(add-to-list 'result name 'append)))))

(defun dbus-list-queued-owners (bus service)
"Return the unique names registered at D-Bus BUS and queued for SERVICE.
The result is a list of strings, or nil when there are no queued name
owners service names at all."
  (condition-case nil
      (dbus-call-method
       bus dbus-service-dbus dbus-path-dbus
       dbus-interface-dbus "ListQueuedOwners" service)
    (dbus-error)))

(defun dbus-get-name-owner (bus service)
  "Return the name owner of SERVICE registered at D-Bus BUS.
The result is either a string, or nil if there is no name owner."
  (condition-case nil
      (dbus-call-method
       bus dbus-service-dbus dbus-path-dbus
       dbus-interface-dbus "GetNameOwner" service)
    (dbus-error)))

(defun dbus-introspect (bus service path)
  "Return the introspection data of SERVICE in D-Bus BUS at object path PATH.
The data are in XML format.

Example:

\(dbus-introspect
  :system \"org.freedesktop.Hal\"
  \"/org/freedesktop/Hal/devices/computer\")"
  (condition-case nil
      (dbus-call-method
       bus service path dbus-interface-introspectable "Introspect")
    (dbus-error)))

(if nil ;; Must be reworked.  Shall we offer D-Bus signatures at all?
(defun dbus-get-signatures (bus interface signal)
  "Retrieve SIGNAL's type signatures from D-Bus.
The result is a list of SIGNAL's type signatures.  Example:

  \(\"s\" \"b\" \"ai\"\)

This list represents 3 parameters of SIGNAL.  The first parameter
is of type string, the second parameter is of type boolean, and
the third parameter is of type array of integer.

If INTERFACE or SIGNAL do not exist, or if they do not support
the D-Bus method org.freedesktop.DBus.Introspectable.Introspect,
the function returns nil."
  (condition-case nil
      (let ((introspect-xml
	     (with-temp-buffer
	       (insert (dbus-introspect bus interface))
	       (xml-parse-region (point-min) (point-max))))
	    node interfaces signals args result)
	;; Get the root node.
	(setq node (xml-node-name introspect-xml))
	;; Get all interfaces.
	(setq interfaces (xml-get-children node 'interface))
	(while interfaces
	  (when (string-equal (xml-get-attribute (car interfaces) 'name)
			      interface)
	    ;; That's the requested interface.  Check for signals.
	    (setq signals (xml-get-children (car interfaces) 'signal))
	    (while signals
	      (when (string-equal (xml-get-attribute (car signals) 'name)
				  signal)
		;; The signal we are looking for.
		(setq args (xml-get-children (car signals) 'arg))
		(while args
		  (unless (xml-get-attribute (car args) 'type)
		    ;; This shouldn't happen, let's escape.
		    (signal 'dbus-error ""))
		  ;; We append the signature.
		  (setq
		   result (append result
				  (list (xml-get-attribute (car args) 'type))))
		  (setq args (cdr args)))
		(setq signals nil))
	      (setq signals (cdr signals)))
	    (setq interfaces nil))
	  (setq interfaces (cdr interfaces)))
	result)
    ;; We ignore `dbus-error'.  There might be no introspectable interface.
    (dbus-error nil)))
) ;; (if nil ...

(provide 'dbus)

;; arch-tag: a47caf84-9162-4811-90cc-5d388e37b9bd
;;; dbus.el ends here
