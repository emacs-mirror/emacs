;;; -*- no-byte-compile: t; -*-
;;; dbus.el --- Elisp bindings for D-Bus.

;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.

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

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Check it.
(assert (featurep 'dbusbind) nil "D-Bus is not supported")

(require 'xml)

(defconst dbus-service-dbus "org.freedesktop.DBus"
  "The bus name used to talk to the bus itself.")

(defconst dbus-path-dbus "/org/freedesktop/DBus"
  "The object path used to talk to the bus itself.")

(defconst dbus-interface-dbus "org.freedesktop.DBus"
  "The interface exported by the object with `dbus-service-dbus' and `dbus-path-dbus'.")

(defconst dbus-interface-peer (concat dbus-interface-dbus ".Peer")
  "The interface for peer objects.")

(defconst dbus-interface-introspectable
  (concat dbus-interface-dbus ".Introspectable")
  "The interface supported by introspectable objects.")

(defmacro dbus-ignore-errors (&rest body)
  "Execute BODY; signal D-Bus error when `dbus-debug' is non-nil.
Otherwise, return result of last form in BODY, or all other errors."
  `(condition-case err
       (progn ,@body)
     (dbus-error (when dbus-debug (signal (car err) (cdr err))))))

(put 'dbus-ignore-errors 'lisp-indent-function 0)
(put 'dbus-ignore-errors 'edebug-form-spec '(form symbolp body))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<dbus-ignore-errors\\>"))


;;; Hash table of registered functions.

;; We create it here.  So we have a simple test in dbusbind.c, whether
;; the Lisp code has been loaded.
(setq dbus-registered-functions-table (make-hash-table :test 'equal))

(defun dbus-list-hash-table ()
  "Returns all registered member registrations to D-Bus.
The return value is a list, with elements of kind (KEY . VALUE).
See `dbus-registered-functions-table' for a description of the
hash table."
  (let (result)
    (maphash
     '(lambda (key value) (add-to-list 'result (cons key value) 'append))
     dbus-registered-functions-table)
    result))

(defun dbus-unregister-object (object)
  "Unregister OBJECT from D-Bus.
OBJECT must be the result of a preceding `dbus-register-method'
or `dbus-register-signal' call.  It returns t if OBJECT has been
unregistered, nil otherwise."
  ;; Check parameter.
  (unless (and (consp object) (not (null (car object))) (consp (cdr object)))
    (signal 'wrong-type-argument (list 'D-Bus object)))

  ;; Find the corresponding entry in the hash table.
  (let* ((key (car object))
	 (value (gethash key dbus-registered-functions-table)))
    ;; Loop over the registered functions.
    (while (consp value)
      ;; (car value) has the structure (UNAME SERVICE PATH HANDLER).
      ;; (cdr object) has the structure ((SERVICE PATH HANDLER) ...).
      (if (not (equal (cdr (car value)) (car (cdr object))))
	  (setq value (cdr value))
	;; Compute new hash value.  If it is empty, remove it from
	;; hash table.
	(unless
	    (puthash
	     key
	     (delete (car value) (gethash key dbus-registered-functions-table))
	     dbus-registered-functions-table)
	  (remhash key dbus-registered-functions-table))
	(setq value t)))
    value))

(defun dbus-name-owner-changed-handler (&rest args)
  "Reapplies all member registrations to D-Bus.
This handler is applied when a \"NameOwnerChanged\" signal has
arrived.  SERVICE is the object name for which the name owner has
been changed.  OLD-OWNER is the previous owner of SERVICE, or the
empty string if SERVICE was not owned yet.  NEW-OWNER is the new
owner of SERVICE, or the empty string if SERVICE looses any name owner.

usage: (dbus-name-owner-changed-handler service old-owner new-owner)"
  (save-match-data
    ;; Check the arguments.  We should silently ignore it when they
    ;; are wrong.
    (if (and (= (length args) 3)
	     (stringp (car args))
	     (stringp (cadr args))
	     (stringp (caddr args)))
	(let ((service (car args))
	      (old-owner (cadr args))
	      (new-owner (caddr args)))
	  ;; Check whether SERVICE is a known name.
	  (when (not (string-match "^:" service))
	    (maphash
	     '(lambda (key value)
		(dolist (elt value)
		  ;; key has the structure (BUS INTERFACE MEMBER).
		  ;; elt has the structure (UNAME SERVICE PATH HANDLER).
		  (when (string-equal old-owner (car elt))
		    ;; Remove old key, and add new entry with changed name.
		    (dbus-unregister-object (list key (cdr elt)))
		    ;; Maybe we could arrange the lists a little bit better
		    ;; that we don't need to extract every single element?
		    (dbus-register-signal
		     ;; BUS      SERVICE     PATH
		     (nth 0 key) (nth 1 elt) (nth 2 elt)
		     ;; INTERFACE MEMBER     HANDLER
		     (nth 1 key) (nth 2 key) (nth 3 elt)))))
	     (copy-hash-table dbus-registered-functions-table))))
      ;; The error is reported only in debug mode.
      (when  dbus-debug
	(signal
	 'dbus-error
	 (cons
	  (format "Wrong arguments of %s.NameOwnerChanged" dbus-interface-dbus)
	  args))))))

;; Register the handler.
(dbus-ignore-errors
  (dbus-register-signal
   :system dbus-service-dbus dbus-path-dbus dbus-interface-dbus
   "NameOwnerChanged" 'dbus-name-owner-changed-handler)
  (dbus-register-signal
   :session dbus-service-dbus dbus-path-dbus dbus-interface-dbus
   "NameOwnerChanged" 'dbus-name-owner-changed-handler))


;;; D-Bus events.

(defun dbus-check-event (event)
  "Checks whether EVENT is a well formed D-Bus event.
EVENT is a list which starts with symbol `dbus-event':

     (dbus-event BUS SERIAL SERVICE PATH INTERFACE MEMBER HANDLER &rest ARGS)

BUS identifies the D-Bus the message is coming from.  It is
either the symbol `:system' or the symbol `:session'.  SERIAL is
the serial number of the received D-Bus message if it is a method
call, or nil.  SERVICE and PATH are the unique name and the
object path of the D-Bus object emitting the message.  INTERFACE
and MEMBER denote the message which has been sent.  HANDLER is
the function which has been registered for this message.  ARGS
are the arguments passed to HANDLER, when it is called during
event handling in `dbus-handle-event'.

This function raises a `dbus-error' signal in case the event is
not well formed."
  (when dbus-debug (message "DBus-Event %s" event))
  (unless (and (listp event)
	       (eq (car event) 'dbus-event)
	       ;; Bus symbol.
	       (symbolp (nth 1 event))
	       ;; Serial.
	       (or (natnump (nth 2 event)) (null (nth 2 event)))
	       ;; Service.
	       (stringp (nth 3 event))
	       ;; Object path.
	       (stringp (nth 4 event))
	       ;; Interface.
	       (stringp (nth 5 event))
	       ;; Member.
	       (stringp (nth 6 event))
	       ;; Handler.
	       (functionp (nth 7 event)))
    (signal 'dbus-error (list "Not a valid D-Bus event" event))))

;;;###autoload
(defun dbus-handle-event (event)
  "Handle events from the D-Bus.
EVENT is a D-Bus event, see `dbus-check-event'.  HANDLER, being
part of the event, is called with arguments ARGS."
  (interactive "e")
  ;; We don't want to raise an error, because this function is called
  ;; in the event handling loop.
  (dbus-ignore-errors
    (let (result)
      (dbus-check-event event)
      (setq result (apply (nth 7 event) (nthcdr 8 event)))
      (unless (consp result) (setq result (cons result nil)))
      ;; Return a message when serial is not nil.
      (when (not (null (nth 2 event)))
	(apply 'dbus-method-return-internal
	       (nth 1 event) (nth 2 event) (nth 3 event) result)))))

(defun dbus-event-bus-name (event)
  "Return the bus name the event is coming from.
The result is either the symbol `:system' or the symbol `:session'.
EVENT is a D-Bus event, see `dbus-check-event'.  This function
raises a `dbus-error' signal in case the event is not well
formed."
  (dbus-check-event event)
  (nth 1 event))

(defun dbus-event-serial-number (event)
  "Return the serial number of the corresponding D-Bus message.
The result is a number in case the D-Bus message is a method
call, or nil for all other mesage types.  The serial number is
needed for generating a reply message.  EVENT is a D-Bus event,
see `dbus-check-event'.  This function raises a `dbus-error'
signal in case the event is not well formed."
  (dbus-check-event event)
  (nth 2 event))

(defun dbus-event-service-name (event)
  "Return the name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 3 event))

(defun dbus-event-path-name (event)
  "Return the object path of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 4 event))

(defun dbus-event-interface-name (event)
  "Return the interface name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 5 event))

(defun dbus-event-member-name (event)
  "Return the member name the event is coming from.
It is either a signal name or a method name. The result is is a
string.  EVENT is a D-Bus event, see `dbus-check-event'.  This
function raises a `dbus-error' signal in case the event is not
well formed."
  (dbus-check-event event)
  (nth 6 event))


;;; D-Bus registered names.

(defun dbus-list-activatable-names ()
  "Return the D-Bus service names which can be activated as list.
The result is a list of strings, which is nil when there are no
activatable service names at all."
  (dbus-ignore-errors
    (dbus-call-method
     :system dbus-service-dbus
     dbus-path-dbus dbus-interface-dbus "ListActivatableNames")))

(defun dbus-list-names (bus)
  "Return the service names registered at D-Bus BUS.
The result is a list of strings, which is nil when there are no
registered service names at all.  Well known names are strings like
\"org.freedesktop.DBus\".  Names starting with \":\" are unique names
for services."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus "ListNames")))

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
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus
     dbus-interface-dbus "ListQueuedOwners" service)))

(defun dbus-get-name-owner (bus service)
  "Return the name owner of SERVICE registered at D-Bus BUS.
The result is either a string, or nil if there is no name owner."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus
     dbus-interface-dbus "GetNameOwner" service)))

(defun dbus-ping (bus service)
  "Check whether SERVICE is registered for D-Bus BUS."
  ;; "Ping" raises a D-Bus error if SERVICE does not exist.
  ;; Otherwise, it returns silently.
  (condition-case nil
      (progn
	(dbus-call-method bus service dbus-path-dbus dbus-interface-peer "Ping")
	t)
    (dbus-error nil)))

(defun dbus-introspect (bus service path)
  "Return the introspection data of SERVICE in D-Bus BUS at object path PATH.
The data are in XML format.

Example:

\(dbus-introspect
  :system \"org.freedesktop.Hal\"
  \"/org/freedesktop/Hal/devices/computer\")"
  (dbus-ignore-errors
    (dbus-call-method
     bus service path dbus-interface-introspectable "Introspect")))

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
  (dbus-ignore-errors
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
	    (when (string-equal (xml-get-attribute (car signals) 'name) signal)
	      ;; The signal we are looking for.
	      (setq args (xml-get-children (car signals) 'arg))
	      (while args
		(unless (xml-get-attribute (car args) 'type)
		  ;; This shouldn't happen, let's escape.
		  (signal 'dbus-error nil))
		;; We append the signature.
		(setq
		 result (append result
				(list (xml-get-attribute (car args) 'type))))
		(setq args (cdr args)))
	      (setq signals nil))
	    (setq signals (cdr signals)))
	  (setq interfaces nil))
	(setq interfaces (cdr interfaces)))
      result)))
) ;; (if nil ...

(provide 'dbus)

;; arch-tag: a47caf84-9162-4811-90cc-5d388e37b9bd
;;; dbus.el ends here
