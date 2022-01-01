;;; dbus.el --- Elisp bindings for D-Bus. -*- lexical-binding: t -*-

;; Copyright (C) 2007-2022 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides language bindings for the D-Bus API.  D-Bus
;; is a message bus system, a simple way for applications to talk to
;; one another.  See <https://dbus.freedesktop.org/> for details.

;; Low-level language bindings are implemented in src/dbusbind.c.

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".

;;; Code:

;; Declare used subroutines and variables.
(declare-function dbus-message-internal "dbusbind.c")
(declare-function dbus--init-bus "dbusbind.c")
(defvar dbus-message-type-invalid)
(defvar dbus-message-type-method-call)
(defvar dbus-message-type-method-return)
(defvar dbus-message-type-error)
(defvar dbus-message-type-signal)
(defvar dbus-registered-objects-table)

;; The following symbols are defined in dbusbind.c.  We need them also
;; when Emacs is compiled without D-Bus support.
(unless (boundp 'dbus-error)
  (define-error 'dbus-error "D-Bus error"))

(unless (boundp 'dbus-debug)
  (defvar dbus-debug nil))

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xml)

;;; D-Bus constants.

(defconst dbus-compound-types '(:array :variant :struct :dict-entry)
  "D-Bus compound types, represented as list.")

(defconst dbus-service-dbus "org.freedesktop.DBus"
  "The bus name used to talk to the bus itself.")

(defconst dbus-path-dbus "/org/freedesktop/DBus"
  "The object path used to talk to the bus itself.")

(defconst dbus-path-local (concat dbus-path-dbus "/Local")
  "The object path used in local/in-process-generated messages.")


;;; Default D-Bus interfaces.

(defconst dbus-interface-dbus "org.freedesktop.DBus"
  "The interface exported by the service `dbus-service-dbus'.")

(defconst dbus-interface-peer (concat dbus-interface-dbus ".Peer")
  "The interface for peer objects.
See URL `https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-peer'.")

;; <interface name="org.freedesktop.DBus.Peer">
;;   <method name="Ping">
;;   </method>
;;   <method name="GetMachineId">
;;     <arg name="machine_uuid" type="s" direction="out"/>
;;   </method>
;; </interface>

(defconst dbus-interface-introspectable
  (concat dbus-interface-dbus ".Introspectable")
  "The interface supported by introspectable objects.
See URL `https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-introspectable'.")

;; <interface name="org.freedesktop.DBus.Introspectable">
;;   <method name="Introspect">
;;     <arg name="data" type="s" direction="out"/>
;;   </method>
;; </interface>

(defconst dbus-interface-properties (concat dbus-interface-dbus ".Properties")
  "The interface for property objects.
See URL `https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties'.")

;; <interface name="org.freedesktop.DBus.Properties">
;;   <method name="Get">
;;     <arg name="interface" type="s" direction="in"/>
;;     <arg name="propname"  type="s" direction="in"/>
;;     <arg name="value"     type="v" direction="out"/>
;;   </method>
;;   <method name="Set">
;;     <arg name="interface" type="s" direction="in"/>
;;     <arg name="propname"  type="s" direction="in"/>
;;     <arg name="value"     type="v" direction="in"/>
;;   </method>
;;   <method name="GetAll">
;;     <arg name="interface" type="s" direction="in"/>
;;     <arg name="props"     type="a{sv}" direction="out"/>
;;   </method>
;;   <signal name="PropertiesChanged">
;;     <arg name="interface" type="s"/>
;;     <arg name="changed_properties"     type="a{sv}"/>
;;     <arg name="invalidated_properties" type="as"/>
;;   </signal>
;; </interface>

(defconst dbus-interface-objectmanager
  (concat dbus-interface-dbus ".ObjectManager")
  "The object manager interface.
See URL `https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-objectmanager'.")

;; <interface name="org.freedesktop.DBus.ObjectManager">
;;   <method name="GetManagedObjects">
;;     <arg name="object_paths_interfaces_and_properties"
;;          type="a{oa{sa{sv}}}" direction="out"/>
;;   </method>
;;   <signal name="InterfacesAdded">
;;     <arg name="object_path"               type="o"/>
;;     <arg name="interfaces_and_properties" type="a{sa{sv}}"/>
;;   </signal>
;;   <signal name="InterfacesRemoved">
;;     <arg name="object_path"               type="o"/>
;;     <arg name="interfaces"                type="as"/>
;;   </signal>
;; </interface>

(defconst dbus-interface-monitoring (concat dbus-interface-dbus ".Monitoring")
  "The monitoring interface.
See URL `https://dbus.freedesktop.org/doc/dbus-specification.html#bus-messages-become-monitor'.")

;; <interface name="org.freedesktop.DBus.Monitoring">
;;   <method name="BecomeMonitor">
;;     <arg name="rule" type="as" direction="in"/>
;;     <arg name="flags" type="u" direction="in"/> ;; Not used, must be 0.
;;   </method>
;; </interface>

(defconst dbus-interface-local (concat dbus-interface-dbus ".Local")
  "An interface whose methods can only be invoked by the local implementation.")

;; <interface name="org.freedesktop.DBus.Local">
;;   <signal name="Disconnected">
;;     <arg name="object_path"               type="o"/>
;;   </signal>
;; </interface>

(defconst dbus-annotation-deprecated (concat dbus-interface-dbus ".Deprecated")
  "An annotation indicating a deprecated interface, method, signal, or property.")


;;; Default D-Bus errors.

(defgroup dbus nil
  "Elisp bindings for D-Bus."
  :group 'comm
  :link '(custom-manual "(dbus)Top")
  :version "28.1")

(defconst dbus-error-dbus "org.freedesktop.DBus.Error"
  "The namespace for default error names.
See /usr/include/dbus-1.0/dbus/dbus-protocol.h.")

(defconst dbus-error-access-denied (concat dbus-error-dbus ".AccessDenied")
  "Security restrictions don't allow doing what you're trying to do.")

(defconst dbus-error-disconnected (concat dbus-error-dbus ".Disconnected")
  "The connection is disconnected and you're trying to use it.")

(defconst dbus-error-failed (concat dbus-error-dbus ".Failed")
  "A generic error; \"something went wrong\" - see the error message for more.")

(defconst dbus-error-invalid-args (concat dbus-error-dbus ".InvalidArgs")
  "Invalid arguments passed to a method call.")

(defconst dbus-error-no-reply (concat dbus-error-dbus ".NoReply")
  "No reply to a message expecting one, usually means a timeout occurred.")

(defconst dbus-error-property-read-only
  (concat dbus-error-dbus ".PropertyReadOnly")
  "Property you tried to set is read-only.")

(defconst dbus-error-service-unknown (concat dbus-error-dbus ".ServiceUnknown")
  "The bus doesn't know how to launch a service to supply the bus name you wanted.")

(defconst dbus-error-unknown-interface
  (concat dbus-error-dbus ".UnknownInterface")
  "Interface you invoked a method on isn't known by the object.")

(defconst dbus-error-unknown-method (concat dbus-error-dbus ".UnknownMethod")
  "Method name you invoked isn't known by the object you invoked it on.")

(defconst dbus-error-unknown-object (concat dbus-error-dbus ".UnknownObject")
  "Object you invoked a method on isn't known.")

(defconst dbus-error-unknown-property (concat dbus-error-dbus ".UnknownProperty")
  "Property you tried to access isn't known by the object.")


;;; Emacs defaults.

(defconst dbus-service-emacs "org.gnu.Emacs"
  "The well known service name of Emacs.")

(defconst dbus-path-emacs "/org/gnu/Emacs"
  "The object path namespace used by Emacs.
All object paths provided by the service `dbus-service-emacs'
shall be subdirectories of this path.")

(defconst dbus-interface-emacs "org.gnu.Emacs"
  "The interface namespace used by Emacs.")


;;; Basic D-Bus message functions.

(defmacro dbus-ignore-errors (&rest body)
  "Execute BODY; signal D-Bus error when `dbus-debug' is non-nil.
Otherwise, return result of last form in BODY, or all other errors."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (dbus-error (when dbus-debug (signal (car err) (cdr err))))))

(defvar dbus-event-error-functions '(dbus-notice-synchronous-call-errors)
  "Functions to be called when a D-Bus error happens in the event handler.
Every function must accept two arguments, the event and the error variable
caught in `condition-case' by `dbus-error'.")

(defvar dbus-return-values-table (make-hash-table :test #'equal)
  "Hash table for temporarily storing arguments of reply messages.
A key in this hash table is a list (:serial BUS SERIAL), like in
`dbus-registered-objects-table'.  BUS is either a Lisp keyword,
`:system' or `:session', or a string denoting the bus address.
SERIAL is the serial number of the reply message.

The value of an entry is a cons (STATE . RESULT).  STATE can be
either `:pending' (we are still waiting for the result),
`:complete' (the result is available) or `:error' (the reply
message was an error message).")

(defun dbus-call-method-handler (&rest args)
  "Handler for reply messages of asynchronous D-Bus message calls.
It calls the function stored in `dbus-registered-objects-table'.
The result will be made available in `dbus-return-values-table'."
  (let* ((key (list :serial
		    (dbus-event-bus-name last-input-event)
		    (dbus-event-serial-number last-input-event)))
         (result (gethash key dbus-return-values-table)))
    (when (consp result)
      (setcar result :complete)
      (setcdr result (if (= (length args) 1) (car args) args)))))

(defun dbus-notice-synchronous-call-errors (ev er)
  "Detect errors resulting from pending synchronous calls."
  (let* ((key (list :serial
		    (dbus-event-bus-name ev)
		    (dbus-event-serial-number ev)))
         (result (gethash key dbus-return-values-table)))
    (when (consp result)
      (setcar result :error)
      (setcdr result er))))

(defun dbus-call-method (bus service path interface method &rest args)
  "Call METHOD on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide METHOD.

If the parameter `:timeout' is given, the following integer
TIMEOUT specifies the maximum number of milliseconds before the
method call must return.  The default value is 25,000.  If the
method call doesn't return in time, a D-Bus error is raised.

All other arguments ARGS are passed to METHOD as arguments.  They are
converted into D-Bus types via the following rules:

  t and nil => DBUS_TYPE_BOOLEAN
  number    => DBUS_TYPE_UINT32
  integer   => DBUS_TYPE_INT32
  float     => DBUS_TYPE_DOUBLE
  string    => DBUS_TYPE_STRING
  list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type keyword.  For details
about type keywords, see Info node `(dbus)Type Conversion'.

`dbus-call-method' returns the resulting values of METHOD as a list of
Lisp objects.  The type conversion happens the other direction as for
input arguments.  It follows the mapping rules:

  DBUS_TYPE_BOOLEAN     => t or nil
  DBUS_TYPE_BYTE        => natural number
  DBUS_TYPE_UINT16      => natural number
  DBUS_TYPE_INT16       => integer
  DBUS_TYPE_UINT32      => natural number
  DBUS_TYPE_UNIX_FD     => natural number
  DBUS_TYPE_INT32       => integer
  DBUS_TYPE_UINT64      => natural number
  DBUS_TYPE_INT64       => integer
  DBUS_TYPE_DOUBLE      => float
  DBUS_TYPE_STRING      => string
  DBUS_TYPE_OBJECT_PATH => string
  DBUS_TYPE_SIGNATURE   => string
  DBUS_TYPE_ARRAY       => list
  DBUS_TYPE_VARIANT     => list
  DBUS_TYPE_STRUCT      => list
  DBUS_TYPE_DICT_ENTRY  => list

Example:

\(dbus-call-method
 :session \"org.gnome.seahorse\" \"/org/gnome/seahorse/keys/openpgp\"
 \"org.gnome.seahorse.Keys\" \"GetKeyField\"
 \"openpgp:657984B8C7A966DD\" \"simple-name\")

  => (t (\"Philip R. Zimmermann\"))

If the result of the METHOD call is just one value, the converted Lisp
object is returned instead of a list containing this single Lisp object.

\(dbus-call-method
 :system \"org.freedesktop.Hal\" \"/org/freedesktop/Hal/devices/computer\"
 \"org.freedesktop.Hal.Device\" \"GetPropertyString\"
 \"system.kernel.machine\")

  => \"i686\""

  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (or (memq bus '(:system :session :system-private :session-private))
      (stringp bus)
      (signal 'wrong-type-argument (list 'keywordp bus)))
  (or (stringp service)
      (signal 'wrong-type-argument (list 'stringp service)))
  (or (stringp path)
      (signal 'wrong-type-argument (list 'stringp path)))
  (or (stringp interface)
      (signal 'wrong-type-argument (list 'stringp interface)))
  (or (stringp method)
      (signal 'wrong-type-argument (list 'stringp method)))

  (let ((timeout (plist-get args :timeout))
        (check-interval 0.001)
	(key
	 (apply
          #'dbus-message-internal dbus-message-type-method-call
          bus service path interface method #'dbus-call-method-handler args))
        (result (cons :pending nil)))

    ;; Wait until `dbus-call-method-handler' has put the result into
    ;; `dbus-return-values-table'.  If no timeout is given, use the
    ;; default 25".  Events which are not from D-Bus must be restored.
    ;; `read-event' performs a redisplay.  This must be suppressed; it
    ;; hurts when reading D-Bus events asynchronously.

    ;; Work around bug#16775 by busy-waiting with gradual backoff for
    ;; dbus calls to complete.  A better approach would involve either
    ;; adding arbitrary wait condition support to read-event or
    ;; restructuring dbus as a kind of process object.  Poll at most
    ;; about once per second for completion.

    (puthash key result dbus-return-values-table)
    (unwind-protect
        (progn
          (with-timeout
              ((if timeout (/ timeout 1000.0) 25)
               (signal 'dbus-error `(,dbus-error-no-reply "Call timed out")))
            (while (eq (car result) :pending)
              (let ((event (let ((inhibit-redisplay t) unread-command-events)
                             (read-event nil nil check-interval))))
		(when event
		  (if (ignore-errors (dbus-check-event event))
		      (setf result (gethash key dbus-return-values-table))
		    (setf unread-command-events
			  (nconc unread-command-events
				 (cons event nil)))))
                (when (< check-interval 1)
                  (setf check-interval (* check-interval 1.05))))))
          (when (eq (car result) :error)
            (signal (cadr result) (cddr result)))
          (cdr result))
      (remhash key dbus-return-values-table))))

(defun dbus-call-method-asynchronously
 (bus service path interface method handler &rest args)
 "Call METHOD on the D-Bus BUS asynchronously.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide METHOD.

HANDLER is a Lisp function, which is called when the corresponding
return message has arrived.  If HANDLER is nil, no return message
will be expected.

If the parameter `:timeout' is given, the following integer
TIMEOUT specifies the maximum number of milliseconds before the
method call must return.  The default value is 25,000.  If the
method call doesn't return in time, a D-Bus error is raised.

All other arguments ARGS are passed to METHOD as arguments.  They are
converted into D-Bus types via the following rules:

  t and nil => DBUS_TYPE_BOOLEAN
  number    => DBUS_TYPE_UINT32
  integer   => DBUS_TYPE_INT32
  float     => DBUS_TYPE_DOUBLE
  string    => DBUS_TYPE_STRING
  list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type keyword.  For details
about type keywords, see Info node `(dbus)Type Conversion'.

If HANDLER is a Lisp function, the function returns a key into the
hash table `dbus-registered-objects-table'.  The corresponding entry
in the hash table is removed, when the return message arrives,
and HANDLER is called.

Example:

\(dbus-call-method-asynchronously
 :system \"org.freedesktop.Hal\" \"/org/freedesktop/Hal/devices/computer\"
 \"org.freedesktop.Hal.Device\" \"GetPropertyString\" #\\='message
 \"system.kernel.machine\")

  -| i686

  => (:serial :system 2)"

  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (or (memq bus '(:system :session :system-private :session-private))
      (stringp bus)
      (signal 'wrong-type-argument (list 'keywordp bus)))
  (or (stringp service)
      (signal 'wrong-type-argument (list 'stringp service)))
  (or (stringp path)
      (signal 'wrong-type-argument (list 'stringp path)))
  (or (stringp interface)
      (signal 'wrong-type-argument (list 'stringp interface)))
  (or (stringp method)
      (signal 'wrong-type-argument (list 'stringp method)))
  (or (null handler) (functionp handler)
      (signal 'wrong-type-argument (list 'functionp handler)))

  (apply #'dbus-message-internal dbus-message-type-method-call
	 bus service path interface method handler args))

(defun dbus-send-signal (bus service path interface signal &rest args)
  "Send signal SIGNAL on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.  The signal is sent from the
D-Bus object Emacs is registered at BUS.

SERVICE is the D-Bus name SIGNAL is sent to.  It can be either a known
name or a unique name.  If SERVICE is nil, the signal is sent as
broadcast message.  PATH is the D-Bus object path SIGNAL is sent from.
INTERFACE is an interface available at PATH.  It must provide signal
SIGNAL.

All other arguments ARGS are passed to SIGNAL as arguments.  They are
converted into D-Bus types via the following rules:

  t and nil => DBUS_TYPE_BOOLEAN
  number    => DBUS_TYPE_UINT32
  integer   => DBUS_TYPE_INT32
  float     => DBUS_TYPE_DOUBLE
  string    => DBUS_TYPE_STRING
  list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type keyword.  For details
about type keywords, see Info node `(dbus)Type Conversion'.

Example:

\(dbus-send-signal
 :session nil \"/org/gnu/Emacs\" \"org.gnu.Emacs.FileManager\"
 \"FileModified\" \"/home/albinus/.emacs\")"

  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (or (memq bus '(:system :session :system-private :session-private))
      (stringp bus)
      (signal 'wrong-type-argument (list 'keywordp bus)))
  (or (null service) (stringp service)
      (signal 'wrong-type-argument (list 'stringp service)))
  (or (stringp path)
      (signal 'wrong-type-argument (list 'stringp path)))
  (or (stringp interface)
      (signal 'wrong-type-argument (list 'stringp interface)))
  (or (stringp signal)
      (signal 'wrong-type-argument (list 'stringp signal)))

  (apply #'dbus-message-internal dbus-message-type-signal
	 bus service path interface signal args))

(defun dbus-method-return-internal (bus service serial &rest args)
  "Return for message SERIAL on the D-Bus BUS.
This is an internal function, it shall not be used outside dbus.el."

  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (or (memq bus '(:system :session :system-private :session-private))
      (stringp bus)
      (signal 'wrong-type-argument (list 'keywordp bus)))
  (or (stringp service)
      (signal 'wrong-type-argument (list 'stringp service)))
  (or (natnump serial)
      (signal 'wrong-type-argument (list 'natnump serial)))

  (apply #'dbus-message-internal dbus-message-type-method-return
	 bus service serial args))

(defun dbus-method-error-internal (bus service serial error-name &rest args)
  "Return error message for message SERIAL on the D-Bus BUS.
ERROR-NAME must belong to the \"org.freedesktop.DBus.Error\" namespace.
This is an internal function, it shall not be used outside dbus.el."

  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (or (memq bus '(:system :session :system-private :session-private))
      (stringp bus)
      (signal 'wrong-type-argument (list 'keywordp bus)))
  (or (stringp service)
      (signal 'wrong-type-argument (list 'stringp service)))
  (or (natnump serial)
      (signal 'wrong-type-argument (list 'natnump serial)))

  (apply #'dbus-message-internal dbus-message-type-error
	 bus service serial error-name args))

(defun dbus-check-arguments (bus service &rest args)
  "Check arguments ARGS by side effect.
BUS, SERVICE and ARGS have the same format as in `dbus-call-method'.
Any wrong argument triggers a D-Bus error.  Otherwise, return t.
This is an internal function, it shall not be used outside dbus.el."

  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (or (memq bus '(:system :session :system-private :session-private))
      (stringp bus)
      (signal 'wrong-type-argument (list 'keywordp bus)))
  (or (stringp service)
      (signal 'wrong-type-argument (list 'stringp service)))

  (apply #'dbus-message-internal dbus-message-type-invalid bus service args))


;;; Hash table of registered functions.

(defun dbus-list-hash-table ()
  "Return all registered member registrations to D-Bus.
The return value is a list, with elements of kind (KEY . VALUE).
See `dbus-registered-objects-table' for a description of the
hash table."
  (let (result)
    (maphash
     (lambda (key value) (push (cons key value) result))
     dbus-registered-objects-table)
    result))

(defun dbus-setenv (bus variable value)
  "Set the value of the BUS environment variable named VARIABLE to VALUE.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.  Both VARIABLE and VALUE should
be strings.

Normally, services inherit the environment of the BUS daemon.  This
function adds to or modifies that environment when activating services.

Some bus instances, such as `:system', may disable setting the environment."
  (dbus-call-method
   bus dbus-service-dbus dbus-path-dbus
   dbus-interface-dbus "UpdateActivationEnvironment"
   `(:array (:dict-entry ,variable ,value))))

(defun dbus-register-service (bus service &rest flags)
  "Register known name SERVICE on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

SERVICE is the D-Bus service name that should be registered.  It must
be a known name.

FLAGS are keywords, which control how the service name is registered.
The following keywords are recognized:

`:allow-replacement': Allow another service to become the primary
owner if requested.

`:replace-existing': Request to replace the current primary owner.

`:do-not-queue': If we can not become the primary owner do not place
us in the queue.

The function returns a keyword, indicating the result of the
operation.  One of the following keywords is returned:

`:primary-owner': Service has become the primary owner of the
requested name.

`:in-queue': Service could not become the primary owner and has been
placed in the queue.

`:exists': Service is already in the queue.

`:already-owner': Service is already the primary owner."

  ;; Add Peer handler.
  (dbus-register-method
   bus service nil dbus-interface-peer "Ping"
   #'dbus-peer-handler 'dont-register)

  ;; Add ObjectManager handler.
  (dbus-register-method
   bus service nil dbus-interface-objectmanager "GetManagedObjects"
   #'dbus-managed-objects-handler 'dont-register)

  (let ((arg 0)
	reply)
    (dolist (flag flags)
      (setq arg
	    (+ arg
	       (pcase flag
		 (:allow-replacement 1)
		 (:replace-existing 2)
		 (:do-not-queue 4)
		 (_ (signal 'wrong-type-argument (list flag)))))))
    (setq reply (dbus-call-method
		 bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
		 "RequestName" service arg))
    (pcase reply
      (1 :primary-owner)
      (2 :in-queue)
      (3 :exists)
      (4 :already-owner)
      (_ (signal 'dbus-error (list "Could not register service" service))))))

(defun dbus-unregister-service (bus service)
  "Unregister all objects related to SERVICE from D-Bus BUS.
BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.  SERVICE must be a known service
name.

The function returns a keyword, indicating the result of the
operation.  One of the following keywords is returned:

`:released': We successfully released the service.

`:non-existent': Service name does not exist on this bus.

`:not-owner': We are neither the primary owner nor waiting in the
queue of this service."

  (maphash
   (lambda (key value)
     (unless (eq :serial (car key))
       (dolist (elt value)
	 (ignore-errors
	   (when (and (equal bus (cadr key)) (string-equal service (cadr elt)))
	     (unless
		 (puthash key (delete elt value) dbus-registered-objects-table)
	       (remhash key dbus-registered-objects-table)))))))
   dbus-registered-objects-table)
  (let ((reply (dbus-call-method
		bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
		"ReleaseName" service)))
    (pcase reply
      (1 :released)
      (2 :non-existent)
      (3 :not-owner)
      (_ (signal 'dbus-error (list "Could not unregister service" service))))))

(defun dbus-register-signal
  (bus service path interface signal handler &rest args)
  "Register for a signal on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

SERVICE is the D-Bus service name used by the sending D-Bus object.
It can be either a known name or the unique name of the D-Bus object
sending the signal.

PATH is the D-Bus object path SERVICE is registered at.
INTERFACE is an interface offered by SERVICE.  It must provide
SIGNAL.  HANDLER is a Lisp function to be called when the signal
is received.  It must accept as arguments the values SIGNAL is
sending.

SERVICE, PATH, INTERFACE and SIGNAL can be nil.  This is
interpreted as a wildcard for the respective argument.

The remaining arguments ARGS can be keywords or keyword string pairs.
Their meaning is as follows:

`:argN' STRING:
`:pathN' STRING: This stands for the Nth argument of the
signal.  `:pathN' arguments can be used for object path wildcard
matches as specified by D-Bus, while an `:argN' argument
requires an exact match.

`:arg-namespace' STRING: Register for those signals, whose first
argument names a service or interface within the namespace
STRING.

`:path-namespace' STRING: Register for the object path namespace
STRING.  All signals sent from an object path, which has STRING as
the preceding string, are matched.  This requires PATH to be nil.

`:eavesdrop': Register for unicast signals which are not directed
to the D-Bus object Emacs is registered at D-Bus BUS, if the
security policy of BUS allows this.

Example:

\(defun my-signal-handler (device)
  (message \"Device %s added\" device))

\(dbus-register-signal
 :system \"org.freedesktop.Hal\" \"/org/freedesktop/Hal/Manager\"
 \"org.freedesktop.Hal.Manager\" \"DeviceAdded\" #\\='my-signal-handler)

  => ((:signal :system \"org.freedesktop.Hal.Manager\" \"DeviceAdded\")
      (\"org.freedesktop.Hal\" \"/org/freedesktop/Hal/Manager\" my-signal-handler))

`dbus-register-signal' returns an object, which can be used in
`dbus-unregister-object' for removing the registration."

  (let ((counter 0)
	(rule "type='signal'")
	uname key key1 value)

    ;; Retrieve unique name of service.  If service is a known name,
    ;; we will register for the corresponding unique name, if any.
    ;; Signals are sent always with the unique name as sender.  Note:
    ;; the unique name of `dbus-service-dbus' is that string itself.
    (if (and (stringp service)
	     (not (zerop (length service)))
	     (not (string-equal service dbus-service-dbus))
             (/= (string-to-char service) ?:))
	(setq uname (dbus-get-name-owner bus service))
      (setq uname service))

    (setq rule (concat rule
		       (when uname (format ",sender='%s'" uname))
		       (when interface (format ",interface='%s'" interface))
		       (when signal (format ",member='%s'" signal))
		       (when path (format ",path='%s'" path))))

    ;; Add arguments to the rule.
    (if (or (stringp (car args)) (null (car args)))
	;; As backward compatibility option, we allow just strings.
	(dolist (arg args)
	  (if (stringp arg)
	      (setq rule (concat rule (format ",arg%d='%s'" counter arg)))
	    (if arg (signal 'wrong-type-argument (list "Wrong argument" arg))))
	  (setq counter (1+ counter)))

      ;; Parse keywords.
      (while args
	(setq
	 key (car args)
	 rule (concat
	       rule
	       (cond
		;; `:arg0' .. `:arg63', `:path0' .. `:path63'.
		((and (keywordp key)
		      (string-match
                       "\\`:\\(arg\\|path\\)\\([[:digit:]]+\\)\\'"
		       (symbol-name key)))
		 (setq counter (match-string 2 (symbol-name key))
		       args (cdr args)
		       value (car args))
		 (unless (and (<= (string-to-number counter) 63)
			      (stringp value))
		   (signal 'wrong-type-argument
			   (list "Wrong argument" key value)))
		 (format
		  ",arg%s%s='%s'"
		  counter
		  (if (string-equal (match-string 1 (symbol-name key)) "path")
		      "path" "")
		  value))
		;; `:arg-namespace', `:path-namespace'.
                ((memq key '(:arg-namespace :path-namespace))
		 (setq args (cdr args)
		       value (car args))
		 (unless (stringp value)
		   (signal 'wrong-type-argument
			   (list "Wrong argument" key value)))
		 (format
		  ",%s='%s'"
                  (if (eq key :path-namespace) "path_namespace" "arg0namespace")
		  value))
		;; `:eavesdrop'.
		((eq key :eavesdrop)
		 ",eavesdrop='true'")
		(t (signal 'wrong-type-argument (list "Wrong argument" key)))))
	 args (cdr args))))

    ;; Add the rule to the bus.
    (condition-case err
	(dbus-call-method
	 bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
	 "AddMatch" rule)
      (dbus-error
       (if (not (string-match-p "eavesdrop" rule))
	   (signal (car err) (cdr err))
	 ;; The D-Bus spec says we shall fall back to a rule without eavesdrop.
	 (when dbus-debug (message "Removing eavesdrop from rule %s" rule))
         (setq rule (replace-regexp-in-string ",eavesdrop='true'" "" rule t t))
	 (dbus-call-method
	  bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
	  "AddMatch" rule))))

    (when dbus-debug (message "Matching rule \"%s\" created" rule))

    ;; Create a hash table entry.
    (setq key (list :signal bus interface signal)
	  key1 (list uname service path handler rule)
	  value (gethash key dbus-registered-objects-table))
    (unless  (member key1 value)
      (puthash key (cons key1 value) dbus-registered-objects-table))

    ;; Return the object.
    (list key (list service path handler))))

(defun dbus-register-method
  (bus service path interface method handler &optional dont-register-service)
  "Register METHOD on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

SERVICE is the D-Bus service name of the D-Bus object METHOD is
registered for.  It must be a known name (see discussion of
DONT-REGISTER-SERVICE below).

PATH is the D-Bus object path SERVICE is registered at (see
discussion of DONT-REGISTER-SERVICE below).  INTERFACE is the
interface offered by SERVICE.  It must provide METHOD.

HANDLER is a Lisp function to be called when a method call is
received.  It must accept the input arguments of METHOD.  The
return value of HANDLER is used for composing the returning D-Bus
message.  If HANDLER returns a reply message with an empty
argument list, HANDLER must return the keyword `:ignore' in order
to distinguish it from nil (the boolean false).

If HANDLER detects an error, it shall return the list `(:error
ERROR-NAME ERROR-MESSAGE)'.  ERROR-NAME is a namespaced string
which characterizes the error type, and ERROR-MESSAGE is a free
text string.  Alternatively, any Emacs signal `dbus-error' in
HANDLER raises a D-Bus error message with the error name
\"org.freedesktop.DBus.Error.Failed\".

When DONT-REGISTER-SERVICE is non-nil, the known name SERVICE is not
registered.  This means that other D-Bus clients have no way of
noticing the newly registered method.  When interfaces are constructed
incrementally by adding single methods or properties at a time,
DONT-REGISTER-SERVICE can be used to prevent other clients from
discovering the still incomplete interface."

  ;; Register SERVICE.
  (unless (or dont-register-service
	      (member service (dbus-list-names bus)))
    (dbus-register-service bus service))

  ;; Create a hash table entry.  We use nil for the unique name,
  ;; because the method might be called from anybody.
  (let* ((key (list :method bus interface method))
	 (key1 (list nil service path handler))
	 (value (gethash key dbus-registered-objects-table)))

    (unless  (member key1 value)
      (puthash key (cons key1 value) dbus-registered-objects-table))

    ;; Return the object.
    (list key (list service path handler))))

(defun dbus-unregister-object (object)
  "Unregister OBJECT from D-Bus.
OBJECT must be the result of a preceding `dbus-register-method',
`dbus-register-signal', `dbus-register-property' or
`dbus-register-monitor' call.  The function returns t if OBJECT
has been unregistered, nil otherwise.

When OBJECT identifies the last method or property, which is
registered for the respective service, Emacs releases its
association to the service from D-Bus."
  ;; Check parameter.
  (unless (and (consp object) (not (null (car object))) (consp (cdr object)))
    (signal 'wrong-type-argument (list 'D-Bus object)))

  ;; Find the corresponding entry in the hash table.
  (let* ((key (car object))
	 (type (car key))
	 (bus (cadr key))
	 (value (cadr object))
	 (service (car value))
	 (entry (gethash key dbus-registered-objects-table))
	 ret)
    ;; key has the structure (TYPE BUS INTERFACE MEMBER).
    ;; value has the structure (SERVICE PATH [HANDLER]).
    ;; entry has the structure ((UNAME SERVICE PATH MEMBER [RULE]) ...).
    ;; MEMBER is either a string (the handler), or a cons cell (a
    ;; property value).  UNAME and property values are not taken into
    ;; account for comparison.

    ;; Loop over the registered functions.
    (dolist (elt entry)
      (when (equal
	     value
	     (butlast (cdr elt) (- (length (cdr elt)) (length value))))
	(setq ret t)
	;; Compute new hash value.  If it is empty, remove it from the
	;; hash table.
	(unless (puthash key (delete elt entry) dbus-registered-objects-table)
	  (remhash key dbus-registered-objects-table))
	;; Remove match rule of signals.
	(when (eq type :signal)
	  (dbus-call-method
	   bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
	   "RemoveMatch" (nth 4 elt)))
        ;; Delete monitor connection by reestablishing private bus.
        (when (eq type :monitor)
          (dbus-init-bus bus 'private))))

    ;; Check, whether there is still a registered function or property
    ;; for the given service.  If not, unregister the service from the
    ;; bus.
    (when (and service (memq type '(:method :property))
	       (not (catch :found
		      (progn
			(maphash
			 (lambda (k v)
                           (when (consp v)
			     (dolist (e v)
			       (ignore-errors
			         (and
                                  ;; Type.
                                  (eq type (car k))
				  ;; Bus.
				  (equal bus (cadr k))
				  ;; Service.
				  (string-equal service (cadr e))
				  ;; Non-empty object path.
				  (nth 2 e)
				  (throw :found t))))))
			 dbus-registered-objects-table)
			nil))))
      (dbus-unregister-service bus service))
    ;; Return.
    ret))


;;; D-Bus type conversion.

(defun dbus-string-to-byte-array (string)
  "Transform STRING to list (:array :byte C1 :byte C2 ...).
STRING shall be UTF-8 coded."
  (if (zerop (length string))
      '(:array :signature "y")
    (cons :array (mapcan (lambda (c) (list :byte c)) string))))

(defun dbus-byte-array-to-string (byte-array &optional multibyte)
  "Transform BYTE-ARRAY into UTF-8 coded string.
BYTE-ARRAY must be a list of structure (c1 c2 ...), or a byte
array as produced by `dbus-string-to-byte-array'.  The resulting
string is unibyte encoded, unless MULTIBYTE is non-nil."
  (apply
   (if multibyte #'string #'unibyte-string)
   (unless (equal byte-array '(:array :signature "y"))
     (seq-filter #'characterp byte-array))))

(defun dbus-escape-as-identifier (string)
  "Escape an arbitrary STRING so it follows the rules for a C identifier.
The escaped string can be used as object path component, interface element
component, bus name component or member name in D-Bus.

The escaping consists of replacing all non-alphanumerics, and the
first character if it's a digit, with an underscore and two
lower-case hex digits:

   \"0123abc_xyz\\x01\\xff\" -> \"_30123abc_5fxyz_01_ff\"

i.e. similar to URI encoding, but with \"_\" taking the role of
\"%\", and a smaller allowed set.  As a special case, \"\" is
escaped to \"_\".

Returns the escaped string.  Algorithm taken from
telepathy-glib's `tp_escape_as_identifier'."
  (if (zerop (length string))
      "_"
    (replace-regexp-in-string
     "\\`[0-9]\\|[^A-Za-z0-9]"
     (lambda (x) (format "_%2x" (aref x 0)))
     string nil t)))

(defun dbus-unescape-from-identifier (string)
  "Retrieve the original string from the encoded STRING as unibyte string.
STRING must have been encoded with `dbus-escape-as-identifier'."
  (if (string-equal string "_")
      ""
    (replace-regexp-in-string
     "_.."
     (lambda (x) (byte-to-string (string-to-number (substring x 1) 16)))
     string nil t)))


;;; D-Bus events.

(defun dbus-check-event (event)
  "Check whether EVENT is a well formed D-Bus event.
EVENT is a list which starts with symbol `dbus-event':

  (dbus-event BUS TYPE SERIAL SERVICE DESTINATION PATH
              INTERFACE MEMBER HANDLER &rest ARGS)

BUS identifies the D-Bus the message is coming from.  It is
either a Lisp keyword, `:system', `:session', `:systemp-private'
or `:session-private', or a string denoting the bus address.

TYPE is the D-Bus message type which has caused the event, SERIAL
is the serial number of the received D-Bus message when TYPE is
equal `dbus-message-type-method-return' or `dbus-message-type-error'.

SERVICE and PATH are the unique name and the object path of the
D-Bus object emitting the message.  DESTINATION is the D-Bus name
the message is dedicated to, or nil in case the message is a
broadcast signal.

INTERFACE and MEMBER denote the message which has been sent.
When TYPE is `dbus-message-type-error', MEMBER is the error name.

HANDLER is the function which has been registered for this
message.  ARGS are the typed arguments as returned from the
message.  They are passed to HANDLER without type information,
when it is called during event handling in `dbus-handle-event'.

This function signals a `dbus-error' if the event is not well
formed."
  (when dbus-debug (message "DBus-Event %s" event))
  (unless (and (listp event)
	       (eq (car event) 'dbus-event)
	       ;; Bus keyword.
	       (or (keywordp (nth 1 event))
		   (stringp (nth 1 event)))
	       ;; Type.
	       (and (natnump (nth 2 event))
		    (< dbus-message-type-invalid (nth 2 event)))
	       ;; Serial.
	       (natnump (nth 3 event))
	       ;; Service.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
                   (or (stringp (nth 4 event))
                       (null (nth 4 event))))
	       ;; Destination.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
                   (or (stringp (nth 5 event))
                       (null (nth 5 event))))
	       ;; Object path.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 6 event)))
	       ;; Interface.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 7 event)))
	       ;; Member.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (stringp (nth 8 event)))
	       ;; Handler.
	       (functionp (nth 9 event))
               ;; Arguments.
               (listp (nthcdr 10 event)))
    (signal 'dbus-error (list "Not a valid D-Bus event" event))))

(defun dbus-delete-types (&rest args)
  "Delete type information from arguments retrieved via `dbus-handle-event'.
Basic type arguments (TYPE VALUE) will be transformed into VALUE, and
compound type arguments (TYPE VALUE) will be transformed into (VALUE)."
  (car
   (mapcar
    (lambda (elt)
      (cond
       ((atom elt) elt)
       ((memq (car elt) dbus-compound-types)
        (mapcar #'dbus-delete-types (cdr elt)))
       (t (cadr elt))))
    args)))

(defun dbus-flatten-types (arg)
  "Flatten type information from argument retrieved via `dbus-handle-event'.
Basic type arguments (TYPE VALUE) will be transformed into TYPE VALUE, and
compound type arguments (TYPE VALUE) will be kept as is."
  (let (result)
    (dolist (elt arg)
      (cond
       ((atom elt) (push elt result))
       ((and (not (memq (car elt) dbus-compound-types)))
	(push (car elt) result)
	(push (cadr elt) result))
       (t
	(push (cons (car elt) (dbus-flatten-types (cdr elt))) result))))
    (nreverse result)))

;;;###autoload
(defun dbus-handle-event (event)
  "Handle events from the D-Bus.
EVENT is a D-Bus event, see `dbus-check-event'.  HANDLER, being
part of the event, is called with arguments ARGS (without type information).
If the HANDLER returns a `dbus-error', it is propagated as return message."
  (declare (completion ignore))
  (interactive "e")
  (condition-case err
      (let (monitor args result)
	;; We ignore not well-formed events.
	(dbus-check-event event)
        ;; Remove type information.
        (setq args (mapcar #'dbus-delete-types (nthcdr 10 event)))
        (setq monitor
              (gethash
               (list :monitor (nth 1 event)) dbus-registered-objects-table))
        (if monitor
            ;; A monitor event shall not trigger other operations, and
            ;; it shall not trigger D-Bus errors.
            (setq result (dbus-ignore-errors (apply (nth 9 event) args)))
	  ;; Error messages must be propagated.  The error name is in
	  ;; the member slot.
	  (when (= dbus-message-type-error (nth 2 event))
	    (signal 'dbus-error (cons (nth 8 event) args)))
	  ;; Apply the handler.
	  (setq result (apply (nth 9 event) args))
	  ;; Return an (error) message when it is a message call.
	  (when (= dbus-message-type-method-call (nth 2 event))
	    (dbus-ignore-errors
              (if (eq (car-safe result) :error)
                  (apply #'dbus-method-error-internal
	                 (nth 1 event) (nth 4 event) (nth 3 event) (cdr result))
	        (if (eq result :ignore)
		    (dbus-method-return-internal
		     (nth 1 event) (nth 4 event) (nth 3 event))
                  (apply #'dbus-method-return-internal
		         (nth 1 event) (nth 4 event) (nth 3 event)
		         (if (consp result) result (list result)))))))))
    ;; Error handling.
    (dbus-error
     ;; Return an error message when it is a message call.
     (when (= dbus-message-type-method-call (nth 2 event))
       (dbus-ignore-errors
	 (dbus-method-error-internal
	  (nth 1 event) (nth 4 event) (nth 3 event) dbus-error-failed
          (error-message-string err))))
     ;; Propagate D-Bus error messages.
     (run-hook-with-args 'dbus-event-error-functions event err)
     (when dbus-debug
       (signal (car err) (cdr err))))))

(defun dbus-event-bus-name (event)
  "Return the bus name the event is coming from.
The result is either a Lisp keyword, `:system' or `:session', or
a string denoting the bus address.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function signals a `dbus-error' if the
event is not well formed."
  (dbus-check-event event)
  (nth 1 event))

(defun dbus-event-message-type (event)
  "Return the message type of the corresponding D-Bus message.
The result is a number.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function signals a `dbus-error' if the
event is not well formed."
  (dbus-check-event event)
  (nth 2 event))

(defun dbus-event-serial-number (event)
  "Return the serial number of the corresponding D-Bus message.
The result is a number.  The serial number is needed for
generating a reply message.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function signals a `dbus-error' if the
event is not well formed."
  (dbus-check-event event)
  (nth 3 event))

(defun dbus-event-service-name (event)
  "Return the name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function signals a `dbus-error' if the event is not well
formed."
  (dbus-check-event event)
  (nth 4 event))

(defun dbus-event-destination-name (event)
  "Return the name of the D-Bus object the event is dedicated to.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function signals a `dbus-error' if the event is not well
formed."
  (dbus-check-event event)
  (nth 5 event))

(defun dbus-event-path-name (event)
  "Return the object path of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function signals a `dbus-error' if the event is not well
formed."
  (dbus-check-event event)
  (nth 6 event))

(defun dbus-event-interface-name (event)
  "Return the interface name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function signals a `dbus-error' if the event is not well
formed."
  (dbus-check-event event)
  (nth 7 event))

(defun dbus-event-member-name (event)
  "Return the member name the event is coming from.
It is either a signal name, a method name or an error name.  The
result is a string.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function signals a `dbus-error' if the
event is not well formed."
  (dbus-check-event event)
  (nth 8 event))

(defun dbus-event-handler (event)
  "Return the handler the event is applied with.
The result is a function.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function signals a `dbus-error' if the
event is not well formed."
  (dbus-check-event event)
  (nth 9 event))

(defun dbus-event-arguments (event)
  "Return the arguments the event is carrying on.
The result is a list of arguments.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function signals a `dbus-error' if the
event is not well formed."
  (dbus-check-event event)
  (nthcdr 10 event))


;;; D-Bus registered names.

(defun dbus-list-activatable-names (&optional bus)
  "Return a list of the D-Bus service names which can be activated.
BUS defaults to `:system' when nil or omitted.  The result is a
list of strings, which is nil when there are no activatable
service names at all."
  (let (dbus-debug)
    (dbus-ignore-errors
      (dbus-call-method
       (or bus :system) dbus-service-dbus
       dbus-path-dbus dbus-interface-dbus "ListActivatableNames"))))

(defun dbus-list-names (bus)
  "Return the service names registered at D-Bus BUS.
The result is a list of strings, which is nil when there are no
registered service names at all.  Well known names are strings
like \"org.freedesktop.DBus\".  Names starting with \":\" are
unique names for services."
  (let (dbus-debug)
    (dbus-ignore-errors
      (dbus-call-method
       bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus "ListNames"))))

(defun dbus-list-known-names (bus)
  "Retrieve all services which correspond to a known name in BUS.
A service has a known name if it doesn't start with \":\"."
  (seq-remove (lambda (name)
                (= (string-to-char name) ?:))
              (dbus-list-names bus)))

(defun dbus-list-queued-owners (bus service)
  "Return the unique names registered at D-Bus BUS and queued for SERVICE.
The result is a list of strings, or nil when there are no queued
name owner service names at all."
  (let (dbus-debug)
    (dbus-ignore-errors
      (dbus-call-method
       bus dbus-service-dbus dbus-path-dbus
       dbus-interface-dbus "ListQueuedOwners" service))))

(defun dbus-get-name-owner (bus service)
  "Return the name owner of SERVICE registered at D-Bus BUS.
The result is either a string, or nil if there is no name owner."
  (let (dbus-debug)
    (dbus-ignore-errors
      (dbus-call-method
       bus dbus-service-dbus dbus-path-dbus
       dbus-interface-dbus "GetNameOwner" service))))

(defun dbus-ping (bus service &optional timeout)
  "Check whether SERVICE is registered for D-Bus BUS.
TIMEOUT, a nonnegative integer, specifies the maximum number of
milliseconds before `dbus-ping' must return.  The default value
is 25,000.

Note, that this autoloads SERVICE if it is not running yet.  To
check whether SERVICE is already running, you can instead write

  (member service (dbus-list-known-names bus))"
  ;; "Ping" raises a D-Bus error if SERVICE does not exist.
  ;; Otherwise, it returns silently with nil.
  (condition-case nil
      (not
       (if (natnump timeout)
	   (dbus-call-method
	    bus service dbus-path-dbus dbus-interface-peer
	    "Ping" :timeout timeout)
	 (dbus-call-method
	  bus service dbus-path-dbus dbus-interface-peer "Ping")))
    (dbus-error nil)))

(defun dbus-peer-handler ()
  "Default handler for the \"org.freedesktop.DBus.Peer\" interface.
It will be registered for all objects created by `dbus-register-service'."
  (let* ((last-input-event last-input-event)
	 (method (dbus-event-member-name last-input-event))
	 (path (dbus-event-path-name last-input-event)))
    (cond
     ;; "Ping" does not return an output parameter.
     ((string-equal method "Ping")
      :ignore)
     ;; "GetMachineId" returns "s".
     ((string-equal method "GetMachineId")
      (signal
       'dbus-error
       (list
	(format "%s.GetMachineId not implemented" dbus-interface-peer))))
     (t `(:error ,dbus-error-unknown-method
          ,(format-message
            "No such method \"%s.%s\" at path \"%s\""
            dbus-interface-peer method path))))))


;;; D-Bus introspection.

(defsubst dbus--introspect-names (object tag)
  "Return the names of the children of OBJECT with TAG."
  (mapcar (lambda (elt)
            (dbus-introspect-get-attribute elt "name"))
          (xml-get-children object tag)))

(defsubst dbus--introspect-name (object tag name)
  "Return the first child of OBJECT with TAG, whose name is NAME."
  (seq-find (lambda (elt)
              (string-equal (dbus-introspect-get-attribute elt "name") name))
            (xml-get-children object tag)))

(defun dbus-introspect (bus service path)
  "Return all interfaces and sub-nodes of SERVICE,
registered at object path PATH at bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.  SERVICE must be a known service
name, and PATH must be a valid object path.  The last two
parameters are strings.  The result, the introspection data, is a
string in XML format."
  ;; We don't want to raise errors.
  (let (dbus-debug)
    (dbus-ignore-errors
      (dbus-call-method
       bus service path dbus-interface-introspectable "Introspect"
       :timeout 1000))))

(defalias 'dbus--parse-xml-buffer
  (if (libxml-available-p)
      (lambda ()
        (xml-remove-comments (point-min) (point-max))
        (libxml-parse-xml-region (point-min) (point-max)))
    (lambda ()
      (car (xml-parse-region (point-min) (point-max)))))
  "Compatibility shim for `libxml-parse-xml-region'.")

(defun dbus-introspect-xml (bus service path)
  "Return the introspection data of SERVICE in D-Bus BUS at object path PATH.
The data are a parsed list.  The root object is a \"node\",
representing the object path PATH.  The root object can contain
\"interface\" and further \"node\" objects."
  (with-temp-buffer
    ;; We don't want to raise errors.
    (ignore-errors
      (insert (dbus-introspect bus service path))
      (dbus--parse-xml-buffer))))

(defun dbus-introspect-get-attribute (object attribute)
  "Return the ATTRIBUTE value of D-Bus introspection OBJECT.
ATTRIBUTE must be a string according to the attribute names in
the D-Bus specification."
  (xml-get-attribute-or-nil object (intern attribute)))

(defun dbus-introspect-get-node-names (bus service path)
  "Return all node names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings.  The node names stand for further
object paths of the D-Bus service."
  (dbus--introspect-names (dbus-introspect-xml bus service path) 'node))

(defun dbus-introspect-get-all-nodes (bus service path)
  "Return all node names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings, which are further object paths of SERVICE."
  (cons path (mapcan (lambda (elt)
                       (setq elt (expand-file-name elt path))
                       (dbus-introspect-get-all-nodes bus service elt))
                     (dbus-introspect-get-node-names bus service path))))

(defun dbus-introspect-get-interface-names (bus service path)
  "Return all interface names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings.

The default interface \"org.freedesktop.DBus.Introspectable\" is
always present.  Another default interface is
\"org.freedesktop.DBus.Properties\".  If present, \"interface\"
objects can also have \"property\" objects as children, beside
\"method\" and \"signal\" objects."
  (dbus--introspect-names (dbus-introspect-xml bus service path) 'interface))

(defun dbus-introspect-get-interface (bus service path interface)
  "Return the INTERFACE of SERVICE in D-Bus BUS at object path PATH.
The return value is an XML object.  INTERFACE must be a string
and a member of the list returned by
`dbus-introspect-get-interface-names'.  The resulting
\"interface\" object can contain \"method\", \"signal\",
\"property\" and \"annotation\" children."
  (dbus--introspect-name (dbus-introspect-xml bus service path)
                         'interface interface))

(defun dbus-introspect-get-method-names (bus service path interface)
  "Return a list of strings of all method names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (dbus--introspect-names
   (dbus-introspect-get-interface bus service path interface) 'method))

(defun dbus-introspect-get-method (bus service path interface method)
  "Return method METHOD of interface INTERFACE as an XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
METHOD must be a string and a member of the list returned by
`dbus-introspect-get-method-names'.  The resulting \"method\"
object can contain \"arg\" and \"annotation\" children."
  (dbus--introspect-name
   (dbus-introspect-get-interface bus service path interface)
   'method method))

(defun dbus-introspect-get-signal-names (bus service path interface)
  "Return a list of strings of all signal names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (dbus--introspect-names
   (dbus-introspect-get-interface bus service path interface) 'signal))

(defun dbus-introspect-get-signal (bus service path interface signal)
  "Return signal SIGNAL of interface INTERFACE as an XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
SIGNAL must be a string, element of the list returned by
`dbus-introspect-get-signal-names'.  The resulting \"signal\"
object can contain \"arg\" and \"annotation\" children."
  (dbus--introspect-name
   (dbus-introspect-get-interface bus service path interface)
   'signal signal))

(defun dbus-introspect-get-property-names (bus service path interface)
  "Return a list of strings of all property names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (dbus--introspect-names
   (dbus-introspect-get-interface bus service path interface) 'property))

(defun dbus-introspect-get-property (bus service path interface property)
  "Return PROPERTY of INTERFACE as an XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
PROPERTY must be a string and a member of the list returned by
`dbus-introspect-get-property-names'.  The resulting PROPERTY
object can contain \"annotation\" children."
  (dbus--introspect-name
   (dbus-introspect-get-interface bus service path interface)
   'property property))

(defun dbus-introspect-get-annotation-names
  (bus service path interface &optional name)
  "Return all annotation names as a list of strings.
If NAME is nil, the annotations are children of INTERFACE,
otherwise NAME must be a \"method\", \"signal\", or \"property\"
object, where the annotations belong to."
  (dbus--introspect-names
   (if name
       (or (dbus-introspect-get-method bus service path interface name)
           (dbus-introspect-get-signal bus service path interface name)
           (dbus-introspect-get-property bus service path interface name))
     (dbus-introspect-get-interface bus service path interface))
   'annotation))

(defun dbus-introspect-get-annotation
  (bus service path interface name annotation)
  "Return ANNOTATION as an XML object.
If NAME is nil, ANNOTATION is a child of INTERFACE, otherwise
NAME must be the name of a \"method\", \"signal\", or
\"property\" object, where the ANNOTATION belongs to."
  (dbus--introspect-name
   (if name
       (or (dbus-introspect-get-method bus service path interface name)
           (dbus-introspect-get-signal bus service path interface name)
           (dbus-introspect-get-property bus service path interface name))
     (dbus-introspect-get-interface bus service path interface))
   'annotation annotation))

(defun dbus-introspect-get-argument-names (bus service path interface name)
  "Return a list of all argument names as a list of strings.
NAME must be a \"method\" or \"signal\" object.

Argument names are optional, the function can return nil
therefore, even if the method or signal has arguments."
  (dbus--introspect-names
   (or (dbus-introspect-get-method bus service path interface name)
       (dbus-introspect-get-signal bus service path interface name))
   'arg))

(defun dbus-introspect-get-argument (bus service path interface name arg)
  "Return argument ARG as XML object.
NAME must be a \"method\" or \"signal\" object.  ARG must be a
string and a member of the list returned by
`dbus-introspect-get-argument-names'."
  (dbus--introspect-name
   (or (dbus-introspect-get-method bus service path interface name)
       (dbus-introspect-get-signal bus service path interface name))
   'arg arg))

(defun dbus-introspect-get-signature
  (bus service path interface name &optional direction)
  "Return signature of a `method', `property' or `signal' represented by NAME.
If NAME is a `method', DIRECTION can be either \"in\" or \"out\".
If DIRECTION is nil, \"in\" is assumed.

If NAME is a `signal' or a `property', DIRECTION is ignored."
  ;; For methods, we use "in" as default direction.
  (let ((object (or (dbus-introspect-get-method
		     bus service path interface name)
		    (dbus-introspect-get-signal
		     bus service path interface name)
		    (dbus-introspect-get-property
		     bus service path interface name))))
    (when (and (eq 'method (car object)) (not (stringp direction)))
      (setq direction "in"))
    ;; In signals, no direction is given.
    (when (eq 'signal (car object))
      (setq direction nil))
    ;; Collect the signatures.
    (if (eq 'property (car object))
        (dbus-introspect-get-attribute object "type")
      (mapconcat
       (lambda (x)
         (let ((arg (dbus-introspect-get-argument
                     bus service path interface name x)))
           (if (or (not (stringp direction))
                   (string-equal
                    direction
                    (dbus-introspect-get-attribute arg "direction")))
               (dbus-introspect-get-attribute arg "type")
             "")))
       (dbus-introspect-get-argument-names bus service path interface name)
       ""))))


;;; D-Bus properties.

(defun dbus-get-property (bus service path interface property)
  "Return the value of PROPERTY of INTERFACE.
It will be checked at BUS, SERVICE, PATH.  The result can be any
valid D-Bus value, or nil if there is no PROPERTY, or PROPERTY cannot be read."
  ;; "Get" returns a variant, so we must use the `car'.
  (car
   (dbus-call-method
    bus service path dbus-interface-properties
    "Get" :timeout 500 interface property)))

(defun dbus-set-property (bus service path interface property &rest args)
  "Set value of PROPERTY of INTERFACE to VALUE.
It will be checked at BUS, SERVICE, PATH.  VALUE can be preceded
by a TYPE keyword.  When the value is successfully set, and the
property's access type is not `:write', return VALUE.  Otherwise,
return nil.

\(dbus-set-property BUS SERVICE PATH INTERFACE PROPERTY [TYPE] VALUE)"
  ;; "Set" requires a variant.
  (dbus-call-method
   bus service path dbus-interface-properties
   "Set" :timeout 500 interface property (cons :variant args))
  ;; Return VALUE.
  (condition-case err
      (dbus-get-property bus service path interface property)
    (dbus-error
     (if (string-equal dbus-error-access-denied (cadr err))
         (car args)
       (signal (car err) (cdr err))))))

(defun dbus-get-all-properties (bus service path interface)
  "Return all properties of INTERFACE at BUS, SERVICE, PATH.
The result is a list of entries.  Every entry is a cons of the
name of the property, and its value.  If there are no properties,
nil is returned."
  (let (dbus-debug)
    (dbus-ignore-errors
      ;; "GetAll" returns "a{sv}".
      (mapcar (lambda (dict)
                (cons (car dict) (caadr dict)))
              (dbus-call-method bus service path dbus-interface-properties
                                "GetAll" :timeout 500 interface)))))

(defun dbus-get-this-registered-property (bus _service path interface property)
  "Return PROPERTY entry of `dbus-registered-objects-table'.
Filter out not matching PATH."
  ;; Remove entries not belonging to this case.
  (seq-filter
   (lambda (item)
     (string-equal path (nth 2 item)))
   (gethash (list :property bus interface property)
            dbus-registered-objects-table)))

(defun dbus-get-other-registered-properties
    (bus _service path interface property)
  "Return PROPERTY entry of `dbus-registered-objects-table'.
Filter out matching PATH."
  ;; Remove matching entries.
  (seq-remove
   (lambda (item)
     (string-equal path (nth 2 item)))
   (gethash (list :property bus interface property)
            dbus-registered-objects-table)))

(defun dbus-register-property
    (bus service path interface property access &rest args)
  "Register PROPERTY on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

SERVICE is the D-Bus service name of the D-Bus.  It must be a
known name (see discussion of DONT-REGISTER-SERVICE below).

PATH is the D-Bus object path SERVICE is registered at (see
discussion of DONT-REGISTER-SERVICE below).  INTERFACE is the
name of the interface used at PATH, PROPERTY is the name of the
property of INTERFACE.  ACCESS indicates, whether the property
can be changed by other services via D-Bus.  It must be either
the keyword `:read', `:write' or `:readwrite'.

VALUE is the initial value of the property, it can be of any
valid type (see `dbus-call-method' for details).  VALUE can be
preceded by a TYPE keyword.

If PROPERTY already exists on PATH, it will be overwritten.  For
properties with access type `:read' this is the only way to
change their values.  Properties with access type `:write' or
`:readwrite' can be changed by `dbus-set-property'.

The interface \"org.freedesktop.DBus.Properties\" is added to
PATH, including a default handler for the \"Get\", \"GetAll\" and
\"Set\" methods of this interface.  When EMITS-SIGNAL is non-nil,
the signal \"PropertiesChanged\" is sent when the property is
changed by `dbus-set-property'.

When DONT-REGISTER-SERVICE is non-nil, the known name SERVICE is
not registered.  This means that other D-Bus clients have no way
of noticing the newly registered property.  When interfaces are
constructed incrementally by adding single methods or properties
at a time, DONT-REGISTER-SERVICE can be used to prevent other
clients from discovering the still incomplete interface.

\(dbus-register-property BUS SERVICE PATH INTERFACE PROPERTY ACCESS \
[TYPE] VALUE &optional EMITS-SIGNAL DONT-REGISTER-SERVICE)"
  (let (;; Read basic type keyword.
        (type (when (keywordp (car args)) (pop args)))
        (value (pop args))
        (emits-signal (pop args))
        (dont-register-service (pop args)))
    (unless (member access '(:read :write :readwrite))
      (signal 'wrong-type-argument (list "Access type invalid" access)))
    (unless (or type (consp value))
      (setq type
            (cond
             ((memq value '(t nil)) :boolean)
             ((natnump value) :uint32)
             ((fixnump value) :int32)
             ((floatp value) :double)
             ((stringp value) :string)
             (t
              (signal 'wrong-type-argument (list "Value type invalid" value))))))
    (unless (consp value)
      (setq value (list type value)))
    (setq value (if (member (car value) dbus-compound-types)
                    (list :variant value) (cons :variant value)))
    (dbus-check-arguments bus service value)

    ;; Add handlers for the three property-related methods.
    (dbus-register-method
     bus service path dbus-interface-properties "Get"
     #'dbus-property-handler 'dont-register)
    (dbus-register-method
     bus service path dbus-interface-properties "GetAll"
     #'dbus-property-handler 'dont-register)
    (dbus-register-method
     bus service path dbus-interface-properties "Set"
     #'dbus-property-handler 'dont-register)

    ;; Register SERVICE.
    (unless (or dont-register-service (member service (dbus-list-names bus)))
      (dbus-register-service bus service))

    ;; Send the PropertiesChanged signal.
    (when emits-signal
      (dbus-send-signal
       bus service path dbus-interface-properties "PropertiesChanged"
       interface
       ;; changed_properties.
       (if (eq access :write)
           '(:array: :signature "{sv}")
         `(:array (:dict-entry ,property ,value)))
       ;; invalidated_properties.
       (if (eq access :write)
           `(:array ,property)
         '(:array))))

    ;; Create a hash table entry.  We use nil for the unique name,
    ;; because the property might be accessed from anybody.
    (let ((key (list :property bus interface property))
	  (val
           (cons
	    (list nil service path (list access emits-signal value))
            (dbus-get-other-registered-properties
             bus service path interface property))))
      (puthash key val dbus-registered-objects-table)

      ;; Return the object.
      (list key (list service path)))))

(defun dbus-property-handler (&rest args)
  "Default handler for the \"org.freedesktop.DBus.Properties\" interface.
It will be registered for all objects created by `dbus-register-property'."
  (let* ((last-input-event last-input-event)
         (bus (dbus-event-bus-name last-input-event))
	 (service (dbus-event-service-name last-input-event))
	 (path (dbus-event-path-name last-input-event))
	 (method (dbus-event-member-name last-input-event))
	 (interface (car args))
	 (property (cadr args)))
    (cond
     ;; "Get" returns a variant.
     ((string-equal method "Get")
      (let* ((entry (dbus-get-this-registered-property
                     bus service path interface property))
             (object (car (last (car entry)))))
        (cond
         ((not (consp object))
          `(:error ,dbus-error-unknown-property
            ,(format-message
              "No such property \"%s\" at path \"%s\"" property path)))
         ((eq :write (car object))
          `(:error ,dbus-error-access-denied
            ,(format-message
              "Property \"%s\" at path \"%s\" is not readable" property path)))
	 ;; Return the result.  Since variant is a list, we must embed
	 ;; it into another list.
         (t (list (nth 2 object))))))

     ;; "Set" needs the third typed argument from `last-input-event'.
     ((string-equal method "Set")
      (let* ((value (dbus-flatten-types (nth 12 last-input-event)))
	     (entry (dbus-get-this-registered-property
                     bus service path interface property))
	     (object (car (last (car entry)))))
        (cond
         ((not (consp object))
          `(:error ,dbus-error-unknown-property
            ,(format-message
              "No such property \"%s\" at path \"%s\"" property path)))
         ((eq :read (car object))
          `(:error ,dbus-error-property-read-only
            ,(format-message
              "Property \"%s\" at path \"%s\" is not writable" property path)))
         (t (puthash (list :property bus interface property)
		     (cons (append
                            (butlast (car entry))
                            ;; Reuse ACCESS and EMITS-SIGNAL.
			    (list (append (butlast object) (list value))))
                           (dbus-get-other-registered-properties
                            bus service path interface property))
		     dbus-registered-objects-table)
	    ;; Send the "PropertiesChanged" signal.
	    (when (nth 1 object)
	      (dbus-send-signal
	       bus service path dbus-interface-properties "PropertiesChanged"
               interface
               ;; changed_properties.
	       (if (eq :write (car object))
                   '(:array: :signature "{sv}")
                 `(:array (:dict-entry ,property ,value)))
               ;; invalidated_properties.
               (if (eq :write (car object))
                   `(:array ,property)
                 '(:array))))
            ;; Return empty reply.
	    :ignore))))

     ;; "GetAll" returns "a{sv}".
     ((string-equal method "GetAll")
      (let (result)
	(maphash
	 (lambda (key val)
           (when (consp val)
             (dolist (item val)
               (let ((object (car (last item))))
	         (when (and (equal (butlast key) (list :property bus interface))
		            (string-equal path (nth 2 item))
		            (consp object)
                            (not (eq :write (car object))))
	           (push
	            (list :dict-entry (car (last key)) (nth 2 object))
                    result))))))
	 dbus-registered-objects-table)
	;; Return the result, or an empty array.  An array must be
	;; embedded in a list.
	(list (cons :array (or result '(:signature "{sv}"))))))

     (t `(:error ,dbus-error-unknown-method
          ,(format-message
            "No such method \"%s.%s\" at path \"%s\""
            dbus-interface-properties method path))))))


;;; D-Bus object manager.

(defun dbus-get-all-managed-objects (bus service path)
  "Return all objects at BUS, SERVICE, PATH, and the children of PATH.
The result is a list of objects.  Every object is a cons of an
existing path name, and the list of available interface objects.
An interface object is another cons, whose car is the interface
name and cdr is the list of properties as returned by
`dbus-get-all-properties' for that path and interface.  Example:

\(dbus-get-all-managed-objects :session \"org.gnome.SettingsDaemon\" \"/\")

  => ((\"/org/gnome/SettingsDaemon/MediaKeys\"
       (\"org.gnome.SettingsDaemon.MediaKeys\")
       (\"org.freedesktop.DBus.Peer\")
       (\"org.freedesktop.DBus.Introspectable\")
       (\"org.freedesktop.DBus.Properties\")
       (\"org.freedesktop.DBus.ObjectManager\"))
      (\"/org/gnome/SettingsDaemon/Power\"
       (\"org.gnome.SettingsDaemon.Power.Keyboard\")
       (\"org.gnome.SettingsDaemon.Power.Screen\")
       (\"org.gnome.SettingsDaemon.Power\"
        (\"Icon\" . \". GThemedIcon battery-full-charged-symbolic \")
        (\"Tooltip\" . \"Laptop battery is charged\"))
       (\"org.freedesktop.DBus.Peer\")
       (\"org.freedesktop.DBus.Introspectable\")
       (\"org.freedesktop.DBus.Properties\")
       (\"org.freedesktop.DBus.ObjectManager\"))
      ...)

If possible, \"org.freedesktop.DBus.ObjectManager.GetManagedObjects\"
is used for retrieving the information.  Otherwise, the information
is collected via \"org.freedesktop.DBus.Introspectable.Introspect\"
and \"org.freedesktop.DBus.Properties.GetAll\", which is slow."
    (let ((result
	   ;; Direct call.  Fails, if the target does not support the
	   ;; object manager interface.
           (let (dbus-debug)
	     (dbus-ignore-errors
	       (dbus-call-method
	        bus service path dbus-interface-objectmanager
	        "GetManagedObjects" :timeout 1000)))))

      (if result
	  ;; Massage the returned structure.
	  (dolist (entry result result)
	    ;; "a{oa{sa{sv}}}".
	    (dolist (entry1 (cdr entry))
	      ;; "a{sa{sv}}".
	      (dolist (entry2 entry1)
		;; "a{sv}".
		(if (cadr entry2)
		    ;; "sv".
		    (dolist (entry3 (cadr entry2))
                      (setcdr entry3 (caadr entry3)))
		  (setcdr entry2 nil)))))

	;; Fallback: collect the information.  Slooow!
	(dolist (object
		 (dbus-introspect-get-all-nodes bus service path)
		 result)
	  (let (result1)
	    (dolist
		(interface
		 (dbus-introspect-get-interface-names bus service object)
		 result1)
	      (push
	       (cons interface
		     (dbus-get-all-properties bus service object interface))
               result1))
	    (when result1
	      (push (cons object result1) result)))))))

(defun dbus-managed-objects-handler ()
  "Default handler for the \"org.freedesktop.DBus.ObjectManager\" interface.
It will be registered for all objects created by `dbus-register-service'."
  (let* ((last-input-event last-input-event)
	 (bus (dbus-event-bus-name last-input-event))
	 (path (dbus-event-path-name last-input-event)))
    ;; "GetManagedObjects" returns "a{oa{sa{sv}}}".
    (let (interfaces result)

      ;; Check for object path wildcard interfaces.
      (maphash
       (lambda (key val)
	 (when (equal (butlast key 2) (list :property bus))
           (dolist (item val)
	     (unless (nth 2 item) ; Path.
	       (push (nth 2 key) interfaces)))))
       dbus-registered-objects-table)

      ;; Check all registered object paths.
      (maphash
       (lambda (key val)
	 (when (equal (butlast key 2) (list :property bus))
           (dolist (item val)
	     (let ((object (or (nth 2 item) ""))) ; Path.
	       (when (string-prefix-p path object)
	         (dolist (interface (cons (nth 2 key) (delete-dups interfaces)))
	           (unless (assoc object result)
		     (push (list object) result))
	           (unless (assoc interface (cdr (assoc object result)))
		     (setcdr
		      (assoc object result)
		      (append
		       (list (cons
		              interface
		              ;; We simulate
		              ;; "org.freedesktop.DBus.Properties.GetAll"
		              ;; by using an appropriate D-Bus event.
		              (let ((last-input-event
			             (append
			              (butlast last-input-event 4)
			              (list object dbus-interface-properties
                                            "GetAll" #'dbus-property-handler))))
		                (dbus-property-handler interface))))
		       (cdr (assoc object result)))))))))))
       dbus-registered-objects-table)

      ;; Return the result, or an empty array.
      (list
       :array
       (or
	(mapcar
	 (lambda (x)
	   (list
	    :dict-entry :object-path (car x)
	    (cons :array (mapcar (lambda (y) (cons :dict-entry y)) (cdr x)))))
	 result)
	'(:signature "{oa{sa{sv}}}"))))))

(cl-defun dbus-register-monitor
    (bus &optional handler &key type sender destination path interface member)
  "Register HANDLER for monitor events on the D-Bus BUS.

BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.

HANDLER is the function to be called when a monitor event
arrives.  It is called with the `args' slot of the monitor event,
which are stripped off the type keywords.  If HANDLER is nil, the
default handler `dbus-monitor-handler' is applied.

The other arguments are keyword-value pairs.  `:type TYPE'
defines the message type to be monitored.  If given, it must be
equal one of the strings \"method_call\", \"method_return\",
\"error\" or \"signal\".

`:sender SENDER' and `:destination DESTINATION' are D-Bus names.
They can be unique names, or well-known service names.

`:path PATH' is the D-Bus object to be monitored.  `:interface
INTERFACE' is the name of an interface, and `:member MEMBER' is
either a method name, a signal name, or an error name."
  (let ((bus-private (if (eq bus :system) :system-private
                       (if (eq bus :session) :session-private bus)))
        rule key key1 value)
    (unless handler (setq handler #'dbus-monitor-handler))
    ;; Compose rule.
    (setq rule
          (string-join
           (delq nil (mapcar
                      (lambda (item)
                        (when (cdr item)
                          (format "%s='%s'" (car item) (cdr item))))
                      `(("type" . ,type) ("sender" . ,sender)
                        ("destination" . ,destination) ("path" . ,path)
                        ("interface" . ,interface) ("member" . ,member))))
           ",")
          rule (or rule ""))

    (when (fboundp 'dbus-get-unique-name)
      (unless (ignore-errors (dbus-get-unique-name bus-private))
        (dbus-init-bus bus 'private)))
    (dbus-call-method
     bus-private dbus-service-dbus dbus-path-dbus dbus-interface-monitoring
     "BecomeMonitor" `(:array :string ,rule) :uint32 0)

    (when dbus-debug (message "Matching rule \"%s\" created" rule))

    ;; Create a hash table entry.
    (setq key (list :monitor bus-private)
	  key1 (list nil nil nil handler rule)
	  value (gethash key dbus-registered-objects-table))
    (unless  (member key1 value)
      (puthash key (cons key1 value) dbus-registered-objects-table))

    (when dbus-debug (message "%s" dbus-registered-objects-table))

    ;; Return the object.
    (list key (list nil nil handler))))

(defconst dbus-monitor-method-call
  (propertize "method-call" 'face 'font-lock-function-name-face)
  "Text to be inserted for D-Bus method-call in monitor.")

(defconst dbus-monitor-method-return
  (propertize "method-return" 'face 'font-lock-function-name-face)
  "Text to be inserted for D-Bus method-return in monitor.")

(defconst dbus-monitor-error (propertize "error" 'face 'font-lock-warning-face)
  "Text to be inserted for D-Bus error in monitor.")

(defconst dbus-monitor-signal
  (propertize "signal" 'face 'font-lock-type-face)
  "Text to be inserted for D-Bus signal in monitor.")

(defun dbus-monitor-goto-serial ()
  "Goto D-Bus message with the same serial number."
  (interactive)
  (when (mouse-event-p last-input-event) (mouse-set-point last-input-event))
  (when-let ((point (get-text-property (point) 'dbus-serial)))
    (goto-char point)))

(defun dbus-monitor-handler (&rest _args)
  "Default handler for the \"Monitoring.BecomeMonitor\" interface.
Its full name is \"org.freedesktop.DBus.Monitoring.BecomeMonitor\".
It will be applied for all objects created by `dbus-register-monitor'
which don't declare an own handler.  The printed timestamps do
not reflect the time the D-Bus message has passed the D-Bus
daemon, it is rather the timestamp the corresponding D-Bus event
has been handled by this function."
  (with-current-buffer (get-buffer-create "*D-Bus Monitor*")
    (special-mode)
    (buffer-disable-undo)
    ;; Move forward and backward between messages.
    (local-set-key [?n] #'forward-paragraph)
    (local-set-key [?p] #'backward-paragraph)
    ;; Follow serial links.
    (local-set-key  (kbd "RET") #'dbus-monitor-goto-serial)
    (local-set-key  [mouse-2] #'dbus-monitor-goto-serial)
    (let* ((inhibit-read-only t)
           (text-quoting-style 'grave)
           (point (point))
           (eobp (eobp))
           (event last-input-event)
           (type (dbus-event-message-type event))
	   (sender (dbus-event-service-name event))
	   (destination (dbus-event-destination-name event))
           (serial (dbus-event-serial-number event))
	   (path (dbus-event-path-name event))
	   (interface (dbus-event-interface-name event))
	   (member (dbus-event-member-name event))
           (arguments (dbus-event-arguments event))
           (time (time-to-seconds (current-time))))
      (save-excursion
        ;; Check for matching method-call.
        (goto-char (point-max))
        (when (and (or (= type dbus-message-type-method-return)
                       (= type dbus-message-type-error))
                   (re-search-backward
                    (format
                     (concat
                      "^method-call time=\\(\\S-+\\) "
                      ".*sender=%s .*serial=\\(%d\\) ")
                     destination serial)
                    nil 'noerror))
          (setq serial
                (propertize
                 (match-string 2) 'dbus-serial (match-beginning 0)
                 'help-echo "RET, mouse-1, mouse-2: goto method-call"
                 'face 'link 'follow-link 'mouse-face 'mouse-face 'highlight)
                time (format "%f (%f)" time (- time (read (match-string 1)))))
          (set-text-properties
           (match-beginning 2) (match-end 2)
           `(dbus-serial ,(point-max)
             help-echo
             ,(format
               "RET, mouse-1, mouse-2: goto %s"
               (if (= type dbus-message-type-error) "error" "method-return"))
             face link follow-link mouse-face mouse-face highlight)))
        ;; Insert D-Bus message.
        (goto-char (point-max))
        (insert
         (format
          (concat
           "%s time=%s sender=%s -> destination=%s serial=%s "
           "path=%s interface=%s member=%s\n")
          (cond
           ((= type dbus-message-type-method-call) dbus-monitor-method-call)
           ((= type dbus-message-type-method-return) dbus-monitor-method-return)
           ((= type dbus-message-type-error) dbus-monitor-error)
           ((= type dbus-message-type-signal) dbus-monitor-signal))
          time sender destination serial path interface member))
        (dolist (arg arguments)
          (pp (dbus-flatten-types arg) (current-buffer)))
        (insert "\n")
        ;; Show byte arrays as string.
        (goto-char point)
        (while (re-search-forward
                "(:array\\( :byte [[:digit:]]+\\)+)" nil 'noerror)
          (put-text-property
           (match-beginning 0) (match-end 0)
           'help-echo (dbus-byte-array-to-string (read (match-string 0)))))
        ;; Show fixed numbers.
        (goto-char point)
        (while (re-search-forward
                (concat
                 (regexp-opt
                  '(":int16" ":uint16" ":int32" ":uint32" ":int64" ":uint64"))
                 " \\([-+[:digit:]]+\\)")
                nil 'noerror)
          (put-text-property
           (match-beginning 1) (match-end 1)
           'help-echo
           (format
            "#o%o, #x%X" (read (match-string 1)) (read (match-string 1)))))
        ;; Show floating numbers.
        (goto-char point)
        (while (re-search-forward ":double \\([-+.[:digit:]]+\\)" nil 'noerror)
          (put-text-property
           (match-beginning 1) (match-end 1)
           'help-echo (format "%e" (read (match-string 1))))))
      (when eobp
        (goto-char (point-max))))))

;;;###autoload
(defun dbus-monitor (&optional bus)
  "Invoke `dbus-register-monitor' interactively, and switch to the buffer.
BUS is either a Lisp keyword, `:system' or `:session', or a
string denoting the bus address.  The value nil defaults to `:session'."
  (interactive
   (list
    (let ((input
           (completing-read
            (format-prompt "Enter bus symbol or name" :session)
            '(:system :session) nil nil nil nil :session)))
      (if (and (stringp input)
               (string-match-p "^\\(:session\\|:system\\)$" input))
          (intern input) input))))
  (dbus-register-monitor (or bus :session))
  (switch-to-buffer (get-buffer-create "*D-Bus Monitor*")))

(defun dbus-handle-bus-disconnect ()
  "React to a bus disconnection.
BUS is the bus that disconnected.  This routine unregisters all
handlers on the given bus and causes all synchronous calls
pending at the time of disconnect to fail."
  (let ((bus (dbus-event-bus-name last-input-event))
        keys-to-remove)
    (maphash
     (lambda (key value)
       (when (and (eq (nth 0 key) :serial)
                  (eq (nth 1 key) bus))
         (run-hook-with-args
          'dbus-event-error-functions
          (list 'dbus-event
                bus
                dbus-message-type-error
                (nth 2 key) ; serial
                nil         ; service
                nil         ; destination
                nil         ; path
                nil         ; interface
                nil         ; member
                value)      ; handler
          (list 'dbus-error dbus-error-disconnected  "Bus disconnected" bus))
         (push key keys-to-remove)))
     dbus-registered-objects-table)
    (dolist (key keys-to-remove)
      (remhash key dbus-registered-objects-table))))

(defun dbus-init-bus (bus &optional private)
  "Establish the connection to D-Bus BUS.

BUS can be either the keyword `:system' or the keyword
`:session', or it can be a string denoting the address of the
corresponding bus.  For the system and session buses, this
function is called when loading `dbus.el', there is no need to
call it again.

The function returns the number of connections this Emacs session
has established to the BUS under the same unique name (see
`dbus-get-unique-name').  It depends on the libraries Emacs is
linked with, and on the environment Emacs is running.  For
example, if Emacs is linked with the GTK+ toolkit, and it runs in
a GTK+-aware environment like GNOME, another connection might
already be established.

When PRIVATE is non-nil, a new connection is established instead
of reusing an existing one.  It results in a new unique name at
the bus.  This can be used, if it is necessary to distinguish
from another connection used in the same Emacs process, like the
one established by GTK+.  If BUS is the keyword `:system' or the
keyword `:session', the new connection is identified by the
keywords `:system-private' or `:session-private', respectively."
  (or (featurep 'dbusbind)
      (signal 'dbus-error (list "Emacs not compiled with dbus support")))
  (prog1
      (dbus--init-bus bus private)
    (dbus-register-signal
     bus nil dbus-path-local dbus-interface-local
     "Disconnected" #'dbus-handle-bus-disconnect)))

 
;; Initialize `:system' and `:session' buses.  This adds their file
;; descriptors to input_wait_mask, in order to detect incoming
;; messages immediately.
(when (featurep 'dbusbind)
  (dbus-ignore-errors
    (dbus-init-bus :system))
  (dbus-ignore-errors
    (dbus-init-bus :session)))

(provide 'dbus)

;;; TODO:

;; * Implement org.freedesktop.DBus.ObjectManager.InterfacesAdded and
;;   org.freedesktop.DBus.ObjectManager.InterfacesRemoved.
;;
;; * Cache introspection data.
;;
;; * Run handlers in own threads.

;;; dbus.el ends here
