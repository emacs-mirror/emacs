;;; dbus.el --- Elisp bindings for D-Bus.

;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hardware

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides language bindings for the D-Bus API.  D-Bus
;; is a message bus system, a simple way for applications to talk to
;; one another.  See <http://dbus.freedesktop.org/> for details.

;; Low-level language bindings are implemented in src/dbusbind.c.

;;; Code:

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Declare used subroutines and variables.
(declare-function dbus-call-method "dbusbind.c")
(declare-function dbus-register-signal "dbusbind.c")
(defvar dbus-debug)
(defvar dbus-registered-functions-table)

;; Pacify byte compiler.
(eval-when-compile
  (require 'cl))

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

(defconst dbus-interface-properties (concat dbus-interface-dbus ".Properties")
  "The interface for property objects.")

(defconst dbus-message-type-invalid 0
  "This value is never a valid message type.")

(defconst dbus-message-type-method-call 1
  "Message type of a method call message.")

(defconst dbus-message-type-method-return 2
  "Message type of a method return message.")

(defconst dbus-message-type-error 3
  "Message type of an error reply message.")

(defconst dbus-message-type-signal 4
  "Message type of a signal message.")

(defmacro dbus-ignore-errors (&rest body)
  "Execute BODY; signal D-Bus error when `dbus-debug' is non-nil.
Otherwise, return result of last form in BODY, or all other errors."
  `(condition-case err
       (progn ,@body)
     (dbus-error (when dbus-debug (signal (car err) (cdr err))))))

(put 'dbus-ignore-errors 'lisp-indent-function 0)
(put 'dbus-ignore-errors 'edebug-form-spec '(form body))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<dbus-ignore-errors\\>"))


;;; Hash table of registered functions.

;; We create it here.  So we have a simple test in dbusbind.c, whether
;; the Lisp code has been loaded.
(setq dbus-registered-functions-table (make-hash-table :test 'equal))

(defvar dbus-return-values-table (make-hash-table :test 'equal)
  "Hash table for temporary storing arguments of reply messages.
A key in this hash table is a list (BUS SERIAL).  BUS is either the
symbol `:system' or the symbol `:session'.  SERIAL is the serial number
of the reply message.  See `dbus-call-method-non-blocking-handler' and
`dbus-call-method-non-blocking'.")

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
or `dbus-register-signal' call.  It returns `t' if OBJECT has
been unregistered, `nil' otherwise."
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

(defun dbus-call-method-non-blocking-handler (&rest args)
  "Handler for reply messages of asynchronous D-Bus message calls.
It calls the function stored in `dbus-registered-functions-table'.
The result will be made available in `dbus-return-values-table'."
  (puthash (list (dbus-event-bus-name last-input-event)
		 (dbus-event-serial-number last-input-event))
	   (if (= (length args) 1) (car args) args)
	   dbus-return-values-table))

(defun dbus-call-method-non-blocking
  (bus service path interface method &rest args)
  "Call METHOD on the D-Bus BUS, but don't block the event queue.
This is necessary for communicating to registered D-Bus methods,
which are running in the same Emacs process.

The arguments are the same as in `dbus-call-method'.

usage: (dbus-call-method-non-blocking
         BUS SERVICE PATH INTERFACE METHOD
         &optional :timeout TIMEOUT &rest ARGS)"

  (let ((key
	 (apply
	  'dbus-call-method-asynchronously
	  bus service path interface method
	  'dbus-call-method-non-blocking-handler args)))
    ;; Wait until `dbus-call-method-non-blocking-handler' has put the
    ;; result into `dbus-return-values-table'.
    (while (not (gethash key dbus-return-values-table nil))
      (read-event nil nil 0.1))

    ;; Cleanup `dbus-return-values-table'.  Return the result.
    (prog1
	(gethash key dbus-return-values-table nil)
      (remhash key dbus-return-values-table))))

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
(when nil ;ignore-errors
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

  (dbus-event BUS TYPE SERIAL SERVICE PATH INTERFACE MEMBER HANDLER &rest ARGS)

BUS identifies the D-Bus the message is coming from.  It is
either the symbol `:system' or the symbol `:session'.  TYPE is
the D-Bus message type which has caused the event, SERIAL is the
serial number of the received D-Bus message.  SERVICE and PATH
are the unique name and the object path of the D-Bus object
emitting the message.  INTERFACE and MEMBER denote the message
which has been sent.  HANDLER is the function which has been
registered for this message.  ARGS are the arguments passed to
HANDLER, when it is called during event handling in
`dbus-handle-event'.

This function raises a `dbus-error' signal in case the event is
not well formed."
  (when dbus-debug (message "DBus-Event %s" event))
  (unless (and (listp event)
	       (eq (car event) 'dbus-event)
	       ;; Bus symbol.
	       (symbolp (nth 1 event))
	       ;; Type.
	       (and (natnump (nth 2 event))
		    (< dbus-message-type-invalid (nth 2 event)))
	       ;; Serial.
	       (natnump (nth 3 event))
	       ;; Service.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 4 event)))
	       ;; Object path.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 5 event)))
	       ;; Interface.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 6 event)))
	       ;; Member.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 7 event)))
	       ;; Handler.
	       (functionp (nth 8 event)))
    (signal 'dbus-error (list "Not a valid D-Bus event" event))))

;;;###autoload
(defun dbus-handle-event (event)
  "Handle events from the D-Bus.
EVENT is a D-Bus event, see `dbus-check-event'.  HANDLER, being
part of the event, is called with arguments ARGS.
If the HANDLER returns an `dbus-error', it is propagated as return message."
  (interactive "e")
  (condition-case err
      (let (result)
	;; We ignore not well-formed events.
	(dbus-check-event event)
	;; Error messages must be propagated.
	(when (= dbus-message-type-error (nth 2 event))
	  (signal 'dbus-error (nthcdr 9 event)))
	;; Apply the handler.
	(setq result (apply (nth 8 event) (nthcdr 9 event)))
	;; Return a message when it is a message call.
	(when (= dbus-message-type-method-call (nth 2 event))
	  (dbus-ignore-errors
	    (dbus-method-return-internal
	     (nth 1 event) (nth 3 event) (nth 4 event) result))))
    ;; Error handling.
    (dbus-error
     ;; Return an error message when it is a message call.
     (when (= dbus-message-type-method-call (nth 2 event))
       (dbus-ignore-errors
	 (dbus-method-error-internal
	  (nth 1 event) (nth 3 event) (nth 4 event) (cadr err))))
     ;; Propagate D-Bus error messages.
     (when (or dbus-debug (= dbus-message-type-error (nth 2 event)))
       (signal (car err) (cdr err))))))

(defun dbus-event-bus-name (event)
  "Return the bus name the event is coming from.
The result is either the symbol `:system' or the symbol `:session'.
EVENT is a D-Bus event, see `dbus-check-event'.  This function
raises a `dbus-error' signal in case the event is not well
formed."
  (dbus-check-event event)
  (nth 1 event))

(defun dbus-event-message-type (event)
  "Return the message type of the corresponding D-Bus message.
The result is a number.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function raises a `dbus-error' signal
in case the event is not well formed."
  (dbus-check-event event)
  (nth 2 event))

(defun dbus-event-serial-number (event)
  "Return the serial number of the corresponding D-Bus message.
The result is a number.  The serial number is needed for
generating a reply message.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function raises a `dbus-error' signal
in case the event is not well formed."
  (dbus-check-event event)
  (nth 3 event))

(defun dbus-event-service-name (event)
  "Return the name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 4 event))

(defun dbus-event-path-name (event)
  "Return the object path of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 5 event))

(defun dbus-event-interface-name (event)
  "Return the interface name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 6 event))

(defun dbus-event-member-name (event)
  "Return the member name the event is coming from.
It is either a signal name or a method name. The result is is a
string.  EVENT is a D-Bus event, see `dbus-check-event'.  This
function raises a `dbus-error' signal in case the event is not
well formed."
  (dbus-check-event event)
  (nth 7 event))


;;; D-Bus registered names.

(defun dbus-list-activatable-names ()
  "Return the D-Bus service names which can be activated as list.
The result is a list of strings, which is `nil' when there are no
activatable service names at all."
  (dbus-ignore-errors
    (dbus-call-method
     :system dbus-service-dbus
     dbus-path-dbus dbus-interface-dbus "ListActivatableNames")))

(defun dbus-list-names (bus)
  "Return the service names registered at D-Bus BUS.
The result is a list of strings, which is `nil' when there are no
registered service names at all.  Well known names are strings
like \"org.freedesktop.DBus\".  Names starting with \":\" are
unique names for services."
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
The result is a list of strings, or `nil' when there are no
queued name owners service names at all."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus
     dbus-interface-dbus "ListQueuedOwners" service)))

(defun dbus-get-name-owner (bus service)
  "Return the name owner of SERVICE registered at D-Bus BUS.
The result is either a string, or `nil' if there is no name owner."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus
     dbus-interface-dbus "GetNameOwner" service)))

(defun dbus-ping (bus service)
  "Check whether SERVICE is registered for D-Bus BUS."
  ;; "Ping" raises a D-Bus error if SERVICE does not exist.
  ;; Otherwise, it returns silently with `nil'.
  (condition-case nil
      (not
       (dbus-call-method bus service dbus-path-dbus dbus-interface-peer "Ping"))
    (dbus-error nil)))


;;; D-Bus introspection.

(defun dbus-introspect (bus service path)
  "This function returns all interfaces and sub-nodes of SERVICE,
registered at object path PATH at bus BUS.

BUS must be either the symbol `:system' or the symbol `:session'.
SERVICE must be a known service name, and PATH must be a valid
object path.  The last two parameters are strings.  The result,
the introspection data, is a string in XML format."
  ;; We don't want to raise errors.
  (dbus-ignore-errors
    (dbus-call-method
     bus service path dbus-interface-introspectable "Introspect")))

(defun dbus-introspect-xml (bus service path)
  "Return the introspection data of SERVICE in D-Bus BUS at object path PATH.
The data are a parsed list.  The root object is a \"node\",
representing the object path PATH.  The root object can contain
\"interface\" and further \"node\" objects."
  ;; We don't want to raise errors.
  (xml-node-name
   (ignore-errors
     (with-temp-buffer
       (insert (dbus-introspect bus service path))
       (xml-parse-region (point-min) (point-max))))))

(defun dbus-introspect-get-attribute (object attribute)
  "Return the ATTRIBUTE value of D-Bus introspection OBJECT.
ATTRIBUTE must be a string according to the attribute names in
the D-Bus specification."
  (xml-get-attribute-or-nil object (intern attribute)))

(defun dbus-introspect-get-node-names (bus service path)
  "Return all node names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings.  The node names stand for further
object paths of the D-Bus service."
  (let ((object (dbus-introspect-xml bus service path))
	result)
    (dolist (elt (xml-get-children object 'node) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-all-nodes (bus service path)
  "Return all node names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings, which are further object paths of SERVICE."
  (let ((result (list path)))
    (dolist (elt
             (dbus-introspect-get-node-names bus service path)
             result)
      (setq elt (expand-file-name elt path))
      (setq result
            (append result (dbus-introspect-get-all-nodes bus service elt))))))

(defun dbus-introspect-get-interface-names (bus service path)
  "Return all interface names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings.

There will be always the default interface
\"org.freedesktop.DBus.Introspectable\".  Another default
interface is \"org.freedesktop.DBus.Properties\".  If present,
\"interface\" objects can also have \"property\" objects as
children, beside \"method\" and \"signal\" objects."
  (let ((object (dbus-introspect-xml bus service path))
	result)
    (dolist (elt (xml-get-children object 'interface) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-interface (bus service path interface)
  "Return the INTERFACE of SERVICE in D-Bus BUS at object path PATH.
The return value is an XML object.  INTERFACE must be a string,
element of the list returned by
`dbus-introspect-get-interface-names'.  The resulting
\"interface\" object can contain \"method\", \"signal\",
\"property\" and \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-xml bus service path) 'interface)))
    (while (and elt
		(not (string-equal
		      interface
		      (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-method-names (bus service path interface)
  "Return a list of strings of all method names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (let ((object (dbus-introspect-get-interface bus service path interface))
	result)
    (dolist (elt (xml-get-children object 'method) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-method (bus service path interface method)
  "Return method METHOD of interface INTERFACE as XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
METHOD must be a string, element of the list returned by
`dbus-introspect-get-method-names'.  The resulting \"method\"
object can contain \"arg\" and \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-get-interface bus service path interface)
	      'method)))
    (while (and elt
		(not (string-equal
		      method (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-signal-names (bus service path interface)
  "Return a list of strings of all signal names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (let ((object (dbus-introspect-get-interface bus service path interface))
	result)
    (dolist (elt (xml-get-children object 'signal) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-signal (bus service path interface signal)
  "Return signal SIGNAL of interface INTERFACE as XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
SIGNAL must be a string, element of the list returned by
`dbus-introspect-get-signal-names'.  The resulting \"signal\"
object can contain \"arg\" and \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-get-interface bus service path interface)
	      'signal)))
    (while (and elt
		(not (string-equal
		      signal (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-property-names (bus service path interface)
  "Return a list of strings of all property names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (let ((object (dbus-introspect-get-interface bus service path interface))
	result)
    (dolist (elt (xml-get-children object 'property) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-property (bus service path interface property)
  "This function returns PROPERTY of INTERFACE as XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
PROPERTY must be a string, element of the list returned by
`dbus-introspect-get-property-names'.  The resulting PROPERTY
object can contain \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-get-interface bus service path interface)
	      'property)))
    (while (and elt
		(not (string-equal
		      property
		      (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-annotation-names
  (bus service path interface &optional name)
  "Return all annotation names as list of strings.
If NAME is `nil', the annotations are children of INTERFACE,
otherwise NAME must be a \"method\", \"signal\", or \"property\"
object, where the annotations belong to."
  (let ((object
	 (if name
	     (or (dbus-introspect-get-method bus service path interface name)
		 (dbus-introspect-get-signal bus service path interface name)
		 (dbus-introspect-get-property bus service path interface name))
	   (dbus-introspect-get-interface bus service path interface)))
	result)
    (dolist (elt (xml-get-children object 'annotation) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-annotation
  (bus service path interface name annotation)
  "Return ANNOTATION as XML object.
If NAME is `nil', ANNOTATION is a child of INTERFACE, otherwise
NAME must be the name of a \"method\", \"signal\", or
\"property\" object, where the ANNOTATION belongs to."
  (let ((elt (xml-get-children
	      (if name
		  (or (dbus-introspect-get-method
		       bus service path interface name)
		      (dbus-introspect-get-signal
		       bus service path interface name)
		      (dbus-introspect-get-property
		       bus service path interface name))
		(dbus-introspect-get-interface bus service path interface))
	      'annotation)))
    (while (and elt
		(not (string-equal
		      annotation
		      (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-argument-names (bus service path interface name)
  "Return a list of all argument names as list of strings.
NAME must be a \"method\" or \"signal\" object.

Argument names are optional, the function can return `nil'
therefore, even if the method or signal has arguments."
  (let ((object
	 (or (dbus-introspect-get-method bus service path interface name)
	     (dbus-introspect-get-signal bus service path interface name)))
	result)
    (dolist (elt (xml-get-children object 'arg) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-argument (bus service path interface name arg)
  "Return argument ARG as XML object.
NAME must be a \"method\" or \"signal\" object.  ARG must be a
string, element of the list returned by `dbus-introspect-get-argument-names'."
  (let ((elt (xml-get-children
	      (or (dbus-introspect-get-method bus service path interface name)
		  (dbus-introspect-get-signal bus service path interface name))
	      'arg)))
    (while (and elt
		(not (string-equal
		      arg (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-signature
  (bus service path interface name &optional direction)
  "Return signature of a `method' or `signal', represented by NAME, as string.
If NAME is a `method', DIRECTION can be either \"in\" or \"out\".
If DIRECTION is `nil', \"in\" is assumed.

If NAME is a `signal', and DIRECTION is non-`nil', DIRECTION must
be \"out\"."
  ;; For methods, we use "in" as default direction.
  (let ((object (or (dbus-introspect-get-method
		     bus service path interface name)
		    (dbus-introspect-get-signal
		     bus service path interface name))))
    (when (and (string-equal
		"method" (dbus-introspect-get-attribute object "name"))
	       (not (stringp direction)))
      (setq direction "in"))
    ;; In signals, no direction is given.
    (when (string-equal "signal" (dbus-introspect-get-attribute object "name"))
      (setq direction nil))
    ;; Collect the signatures.
    (mapconcat
     '(lambda (x)
	(let ((arg (dbus-introspect-get-argument
		    bus service path interface name x)))
	  (if (or (not (stringp direction))
		  (string-equal
		   direction
		   (dbus-introspect-get-attribute arg "direction")))
	      (dbus-introspect-get-attribute arg "type")
	    "")))
     (dbus-introspect-get-argument-names bus service path interface name)
     "")))


;;; D-Bus properties.

(defun dbus-get-property (bus service path interface property)
  "Return the value of PROPERTY of INTERFACE.
It will be checked at BUS, SERVICE, PATH.  The result can be any
valid D-Bus value, or `nil' if there is no PROPERTY."
  (dbus-ignore-errors
    ;; We must check, whether the "org.freedesktop.DBus.Properties"
    ;; interface is supported; otherwise the call blocks.
    (when
	(member
	 "Get"
	 (dbus-introspect-get-method-names
	  bus service path "org.freedesktop.DBus.Properties"))
      ;; "Get" returns a variant, so we must use the car.
      (car
       (dbus-call-method
	bus service path dbus-interface-properties
	"Get" interface property)))))

(defun dbus-set-property (bus service path interface property value)
  "Set value of PROPERTY of INTERFACE to VALUE.
It will be checked at BUS, SERVICE, PATH.  When the value has
been set successful, the result is VALUE.  Otherwise, `nil' is
returned."
  (dbus-ignore-errors
    (when
	(and
	 ;; We must check, whether the
	 ;; "org.freedesktop.DBus.Properties" interface is supported;
	 ;; otherwise the call blocks.
	 (member
	  "Set"
	  (dbus-introspect-get-method-names
	   bus service path "org.freedesktop.DBus.Properties"))
	 ;; PROPERTY must be writable.
	 (string-equal
	  "readwrite"
	  (dbus-introspect-get-attribute
	   (dbus-introspect-get-property bus service path interface property)
	   "access")))
      ;; "Set" requires a variant.
      (dbus-call-method
       bus service path dbus-interface-properties
       "Set" interface property (list :variant value))
      ;; Return VALUE.
      (dbus-get-property bus service path interface property))))

(defun dbus-get-all-properties (bus service path interface)
  "Return all properties of INTERFACE at BUS, SERVICE, PATH.
The result is a list of entries.  Every entry is a cons of the
name of the property, and its value.  If there are no properties,
`nil' is returned."
  ;; "org.freedesktop.DBus.Properties.GetAll" is not supported at
  ;; all interfaces.  Therefore, we do it ourselves.
  (dbus-ignore-errors
    (let (result)
      (dolist (property
	       (dbus-introspect-get-property-names
		bus service path interface)
	       result)
	(add-to-list
	 'result
	 (cons property (dbus-get-property bus service path interface property))
	 'append)))))

(provide 'dbus)

;; arch-tag: a47caf84-9162-4811-90cc-5d388e37b9bd
;;; dbus.el ends here
