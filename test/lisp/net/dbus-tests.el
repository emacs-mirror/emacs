;;; dbus-tests.el --- Tests of D-Bus integration into Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'dbus)

(defvar dbus-debug nil)
(declare-function dbus-get-unique-name "dbusbind.c" (bus))

(defconst dbus--test-enabled-session-bus
  (and (featurep 'dbusbind)
       (dbus-ignore-errors (dbus-get-unique-name :session)))
  "Check, whether we are registered at the session bus.")

(defconst dbus--test-enabled-system-bus
  (and (featurep 'dbusbind)
       (dbus-ignore-errors (dbus-get-unique-name :system)))
  "Check, whether we are registered at the system bus.")

(defconst dbus--test-service "org.gnu.Emacs.TestDBus"
  "Test service.")

(defconst dbus--test-path "/org/gnu/Emacs/TestDBus"
  "Test object path.")

(defconst dbus--test-interface "org.gnu.Emacs.TestDBus.Interface"
  "Test interface.")

(defun dbus--test-availability (bus)
  "Test availability of D-Bus BUS."
  (should (dbus-list-names bus))
  (should (dbus-list-activatable-names bus))
  (should (dbus-list-known-names bus))
  (should (dbus-get-unique-name bus)))

(ert-deftest dbus-test00-availability-session ()
  "Test availability of D-Bus `:session'."
  :expected-result (if dbus--test-enabled-session-bus :passed :failed)
  (dbus--test-availability :session))

(ert-deftest dbus-test00-availability-system ()
  "Test availability of D-Bus `:system'."
  :expected-result (if dbus--test-enabled-system-bus :passed :failed)
  (dbus--test-availability :system))

(ert-deftest dbus-test01-type-conversion ()
  "Check type conversion functions."
  (let ((ustr "0123abc_xyz\x01\xff")
	(mstr "Grüß Göttin"))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array "")) ""))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array ustr)) ustr))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array mstr) 'multibyte)
      mstr))
    ;; Should not work for multibyte strings.
    (should-not
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array mstr)) mstr))

    (should
     (string-equal
      (dbus-unescape-from-identifier (dbus-escape-as-identifier "")) ""))
    (should
     (string-equal
      (dbus-unescape-from-identifier (dbus-escape-as-identifier ustr)) ustr))
    ;; Should not work for multibyte strings.
    (should-not
     (string-equal
      (dbus-unescape-from-identifier (dbus-escape-as-identifier mstr)) mstr))))

(ert-deftest dbus-test01-basic-types ()
  "Check basic D-Bus type arguments."
  ;; Unknown keyword.
  (should-error
   (dbus-check-arguments :session dbus--test-service :keyword)
   :type 'wrong-type-argument)

  ;; `:string'.
  (should (dbus-check-arguments :session dbus--test-service "string"))
  (should (dbus-check-arguments :session dbus--test-service :string "string"))
  (should-error
   (dbus-check-arguments :session dbus--test-service :string 0.5)
   :type 'wrong-type-argument)

  ;; `:object-path'.
  (should
   (dbus-check-arguments
    :session dbus--test-service :object-path "/object/path"))
  (should-error
   (dbus-check-arguments :session dbus--test-service :object-path "string")
   :type 'dbus-error)
  (should-error
   (dbus-check-arguments :session dbus--test-service :object-path 0.5)
   :type 'wrong-type-argument)

  ;; `:signature'.
  (should (dbus-check-arguments :session dbus--test-service :signature "as"))
  (should-error
   (dbus-check-arguments :session dbus--test-service :signature "string")
   :type 'dbus-error)
  (should-error
   (dbus-check-arguments :session dbus--test-service :signature 0.5)
   :type 'wrong-type-argument)

  ;; `:boolean'.
  (should (dbus-check-arguments :session dbus--test-service nil))
  (should (dbus-check-arguments :session dbus--test-service t))
  (should (dbus-check-arguments :session dbus--test-service :boolean nil))
  (should (dbus-check-arguments :session dbus--test-service :boolean t))
  ;; Will be handled as `nil'.
  (should (dbus-check-arguments :session dbus--test-service :boolean))
  ;; Will be handled as `t'.
  (should (dbus-check-arguments :session dbus--test-service :boolean 'whatever))

  ;; `:byte'.
  (should (dbus-check-arguments :session dbus--test-service :byte 0))
  ;; Only the least significant byte is taken into account.
  (should
   (dbus-check-arguments :session dbus--test-service :byte most-positive-fixnum))
  (should-error
   (dbus-check-arguments :session dbus--test-service :byte -1)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :byte 0.5)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :byte "string")
   :type 'wrong-type-argument)

  ;; `:int16'.
  (should (dbus-check-arguments :session dbus--test-service :int16 0))
  (should (dbus-check-arguments :session dbus--test-service :int16 #x7fff))
  (should (dbus-check-arguments :session dbus--test-service :int16 #x-8000))
  (should-error
   (dbus-check-arguments :session dbus--test-service :int16 #x8000)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int16 #x-8001)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int16 0.5)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int16 "string")
   :type 'wrong-type-argument)

  ;; `:uint16'.
  (should (dbus-check-arguments :session dbus--test-service :uint16 0))
  (should (dbus-check-arguments :session dbus--test-service :uint16 #xffff))
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint16 #x10000)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint16 -1)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint16 0.5)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint16 "string")
   :type 'wrong-type-argument)

  ;; `:int32'.
  (should (dbus-check-arguments :session dbus--test-service :int32 0))
  (should (dbus-check-arguments :session dbus--test-service :int32 #x7fffffff))
  (should (dbus-check-arguments :session dbus--test-service :int32 #x-80000000))
  (should-error
   (dbus-check-arguments :session dbus--test-service :int32 #x80000000)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int32 #x-80000001)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int32 0.5)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int32 "string")
   :type 'wrong-type-argument)

  ;; `:uint32'.
  (should (dbus-check-arguments :session dbus--test-service 0))
  (should (dbus-check-arguments :session dbus--test-service :uint32 0))
  (should (dbus-check-arguments :session dbus--test-service :uint32 #xffffffff))
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint32 #x100000000)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint32 -1)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint32 0.5)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint32 "string")
   :type 'wrong-type-argument)

  ;; `:int64'.
  (should (dbus-check-arguments :session dbus--test-service :int64 0))
  (should
   (dbus-check-arguments :session dbus--test-service :int64 #x7fffffffffffffff))
  (should
   (dbus-check-arguments :session dbus--test-service :int64 #x-8000000000000000))
  (should-error
   (dbus-check-arguments :session dbus--test-service :int64 #x8000000000000000)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int64 #x-8000000000000001)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int64 0.5)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :int64 "string")
   :type 'wrong-type-argument)

  ;; `:uint64'.
  (should (dbus-check-arguments :session dbus--test-service :uint64 0))
  (should
   (dbus-check-arguments :session dbus--test-service :uint64 #xffffffffffffffff))
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint64 #x10000000000000000)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint64 -1)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint64 0.5)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :uint64 "string")
   :type 'wrong-type-argument)

  ;; `:double'.
  (should (dbus-check-arguments :session dbus--test-service :double 0))
  (should (dbus-check-arguments :session dbus--test-service :double 0.5))
  (should (dbus-check-arguments :session dbus--test-service :double -0.5))
  (should (dbus-check-arguments :session dbus--test-service :double -1))
  ;; Shall both be supported?
  (should (dbus-check-arguments :session dbus--test-service :double 1.0e+INF))
  (should (dbus-check-arguments :session dbus--test-service :double 0.0e+NaN))
  (should-error
   (dbus-check-arguments :session dbus--test-service :double "string")
   :type 'wrong-type-argument)

  ;; `:unix-fd'.  Value range 0 .. 9.
  (should (dbus-check-arguments :session dbus--test-service :unix-fd 0))
  (should (dbus-check-arguments :session dbus--test-service :unix-fd 9))
  (should-error
   (dbus-check-arguments :session dbus--test-service :unix-fd 10)
   :type 'dbus-error)
  (should-error
   (dbus-check-arguments :session dbus--test-service :unix-fd -1)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :unix-fd 0.5)
   :type 'args-out-of-range)
  (should-error
   (dbus-check-arguments :session dbus--test-service :unix-fd "string")
   :type 'wrong-type-argument))

(ert-deftest dbus-test01-compound-types ()
  "Check basic D-Bus type arguments."
  ;; `:array'.  It contains several elements of the same type.
  (should (dbus-check-arguments :session dbus--test-service '("string")))
  (should (dbus-check-arguments :session dbus--test-service '(:array "string")))
  (should
   (dbus-check-arguments :session dbus--test-service '(:array :string "string")))
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:array :string "string1" "string2")))
  ;; Empty array.
  (should (dbus-check-arguments :session dbus--test-service '(:array)))
  (should
   (dbus-check-arguments :session dbus--test-service '(:array :signature "o")))
  ;; Different element types.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array :string "string" :object-path "/object/path"))
   :type 'wrong-type-argument)

  ;; `:variant'.  It contains exactly one element.
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:variant :string "string")))
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:variant (:array "string"))))
  ;; More than one element.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:variant :string "string" :object-path "/object/path"))
   :type 'wrong-type-argument)

  ;; `:dict-entry'.  It must contain two elements; the first one must
  ;; be of a basic type.  It must be an element of an array.
  (should
   (dbus-check-arguments
    :session dbus--test-service
    '(:array (:dict-entry :string "string" :boolean t))))
  ;; The second element is `nil' (implicitly).  FIXME: Is this right?
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:array (:dict-entry :string "string"))))
  ;; Not two elements.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array (:dict-entry :string "string" :boolean t :boolean t)))
   :type 'wrong-type-argument)
  ;; The first element ist not of a basic type.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array (:dict-entry (:array :string "string") :boolean t)))
   :type 'wrong-type-argument)
  ;; It is not an element of an array.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service '(:dict-entry :string "string" :boolean t))
   :type 'wrong-type-argument)
  ;; Different dict entry types can be part of an array.
  (should
   (dbus-check-arguments
    :session dbus--test-service
    '(:array
      (:dict-entry :string "string1" :boolean t)
      (:dict-entry :string "string2" :object-path "/object/path"))))

  ;; `:struct'.  There is no restriction what could be an element of a struct.
  (should
   (dbus-check-arguments
    :session dbus--test-service
    '(:struct
      :string "string"
      :object-path "/object/path"
      (:variant (:array :unix-fd 1 :unix-fd 2 :unix-fd 3 :unix-fd 4))))))

(defun dbus--test-register-service (bus)
  "Check service registration at BUS."
  ;; Cleanup.
  (dbus-ignore-errors (dbus-unregister-service bus dbus--test-service))

  ;; Register an own service.
  (should (eq (dbus-register-service bus dbus--test-service) :primary-owner))
  (should (member dbus--test-service (dbus-list-known-names bus)))
  (should (eq (dbus-register-service bus dbus--test-service) :already-owner))
  (should (member dbus--test-service (dbus-list-known-names bus)))

  ;; Unregister the service.
  (should (eq (dbus-unregister-service bus dbus--test-service) :released))
  (should-not (member dbus--test-service (dbus-list-known-names bus)))
  (should (eq (dbus-unregister-service bus dbus--test-service) :non-existent))
  (should-not (member dbus--test-service (dbus-list-known-names bus)))

  ;; `dbus-service-dbus' is reserved for the BUS itself.
  (should
   (equal
    (butlast
     (should-error (dbus-register-service bus dbus-service-dbus)))
    `(dbus-error ,dbus-error-invalid-args)))
  (should
   (equal
    (butlast
     (should-error (dbus-unregister-service bus dbus-service-dbus)))
    `(dbus-error ,dbus-error-invalid-args))))

(ert-deftest dbus-test02-register-service-session ()
  "Check service registration at `:session' bus."
  (skip-unless (and dbus--test-enabled-session-bus
		    (dbus-register-service :session dbus--test-service)))
  (dbus--test-register-service :session)

  (let ((service "org.freedesktop.Notifications"))
    (when (member service (dbus-list-known-names :session))
      ;; Cleanup.
      (dbus-ignore-errors (dbus-unregister-service :session service))

      (should (eq (dbus-register-service :session service) :in-queue))
      (should (eq (dbus-unregister-service :session service) :released))

      (should
       (eq (dbus-register-service :session service :do-not-queue) :exists))
      (should (eq (dbus-unregister-service :session service) :not-owner)))))

(ert-deftest dbus-test02-register-service-system ()
  "Check service registration at `:system' bus."
  (skip-unless (and dbus--test-enabled-system-bus
		    (dbus-register-service :system dbus--test-service)))
  (dbus--test-register-service :system))

(ert-deftest dbus-test02-register-service-own-bus ()
  "Check service registration with an own bus.
This includes initialization and closing the bus."
  ;; Start bus.
  (let ((output
	 (ignore-errors
	   (shell-command-to-string "env DISPLAY= dbus-launch --sh-syntax")))
	bus pid)
    (skip-unless (stringp output))
    (when (string-match "DBUS_SESSION_BUS_ADDRESS='\\(.+\\)';" output)
      (setq bus (match-string 1 output)))
    (when (string-match "DBUS_SESSION_BUS_PID=\\([[:digit:]]+\\);" output)
      (setq pid (match-string 1 output)))
    (unwind-protect
	(progn
	  (skip-unless
	   (dbus-ignore-errors
	     (and bus pid
		  (featurep 'dbusbind)
		  (dbus-init-bus bus)
		  (dbus-get-unique-name bus)
		  (dbus-register-service bus dbus--test-service))))
	  ;; Run the test.
	  (dbus--test-register-service bus))

      ;; Save exit.
      (when pid (call-process "kill" nil nil nil pid)))))

(ert-deftest dbus-test03-peer-interface ()
  "Check `dbus-interface-peer' methods."
  (skip-unless
   (and dbus--test-enabled-session-bus
	(dbus-register-service :session dbus--test-service)
	;; "GetMachineId" is not implemented (yet).  When it returns a
	;; value, another D-Bus client like dbus-monitor is reacting
	;; on `dbus-interface-peer'.  We cannot test then.
	(not
	 (dbus-ignore-errors
	   (dbus-call-method
	    :session dbus--test-service dbus-path-dbus
	    dbus-interface-peer "GetMachineId" :timeout 100)))))

  (should (dbus-ping :session dbus--test-service 100))
  (dbus-unregister-service :session dbus--test-service)
  (should-not (dbus-ping :session dbus--test-service 100)))

(defun dbus--test-method-handler (&rest args)
  "Method handler for `dbus-test04-register-method'."
  (cond
   ;; No argument.
   ((null args)
    :ignore)
   ;; One argument.
   ((= 1 (length args))
    (car args))
   ;; Two arguments.
   ((= 2 (length args))
    `(:error ,dbus-error-invalid-args
             ,(format-message "Wrong arguments %s" args)))
   ;; More than two arguments.
   (t (signal 'dbus-error (cons "D-Bus signal" args)))))

(ert-deftest dbus-test04-register-method ()
  "Check method registration for an own service."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((method1 "Method1")
            (method2 "Method2")
            (handler #'dbus--test-method-handler)
            registered)

        ;; The service is not registered yet.
        (should
         (equal
          (butlast
           (should-error
            (dbus-call-method
             :session dbus--test-service dbus--test-path
             dbus--test-interface method1 :timeout 10 "foo")))
           `(dbus-error ,dbus-error-service-unknown)))

        ;; Register.
        (should
         (equal
          (setq
           registered
           (dbus-register-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method1 handler))
          `((:method :session ,dbus--test-interface ,method1)
            (,dbus--test-service ,dbus--test-path ,handler))))
        (should
         (equal
          (dbus-register-method
           :session dbus--test-service dbus--test-path
           dbus--test-interface method2 handler)
          `((:method :session ,dbus--test-interface ,method2)
            (,dbus--test-service ,dbus--test-path ,handler))))

        ;; No argument, returns nil.
        (should-not
         (dbus-call-method
          :session dbus--test-service dbus--test-path
          dbus--test-interface method1))
        ;; One argument, returns the argument.
        (should
         (string-equal
          (dbus-call-method
           :session dbus--test-service dbus--test-path
           dbus--test-interface method1 "foo")
          "foo"))
        ;; Two arguments, D-Bus error activated as `(:error ...)' list.
        (should
         (equal
          (should-error
           (dbus-call-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method1 "foo" "bar"))
          `(dbus-error ,dbus-error-invalid-args "Wrong arguments (foo bar)")))
        ;; Three arguments, D-Bus error activated by `dbus-error' signal.
        (should
         (equal
          (should-error
           (dbus-call-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method1 "foo" "bar" "baz"))
          `(dbus-error
            ,dbus-error-failed
            "D-Bus error: \"D-Bus signal\", \"foo\", \"bar\", \"baz\"")))

        ;; Unregister method.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered))
        (should
         (equal
          (butlast
           (should-error
            (dbus-call-method
             :session dbus--test-service dbus--test-path
             dbus--test-interface method1 :timeout 10 "foo")))
          `(dbus-error ,dbus-error-no-reply))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defvar dbus--test-signal-received nil
  "Received signal value in `dbus--test-signal-handler'.")

(defun dbus--test-signal-handler (&rest args)
  "Signal handler for `dbus-test*-signal'."
  (setq dbus--test-signal-received args))

(defun dbus--test-timeout-handler (&rest _ignore)
  "Timeout handler, reporting a failed test."
  (ert-fail (format "`%s' timed out" (ert-test-name (ert-running-test)))))

(ert-deftest dbus-test05-register-signal ()
  "Check signal registration for an own service."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((member "Member")
            (handler #'dbus--test-signal-handler)
            registered)

        ;; Register signal handler.
        (should
         (equal
          (setq
           registered
           (dbus-register-signal
            :session dbus--test-service dbus--test-path
            dbus--test-interface member handler))
          `((:signal :session ,dbus--test-interface ,member)
            (,dbus--test-service ,dbus--test-path ,handler))))

        ;; Send one argument, basic type.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member "foo")
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should (equal dbus--test-signal-received '("foo")))

        ;; Send two arguments, compound types.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member
         '(:array :byte 1 :byte 2 :byte 3) '(:variant :string "bar"))
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should (equal dbus--test-signal-received '((1 2 3) ("bar"))))

        ;; Unregister signal.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered)))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test06-register-property ()
  "Check property registration for an own service."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((property1 "Property1")
            (property2 "Property2")
            (property3 "Property3")
            (property4 "Property4")
            registered)

        ;; `:read' property.
        (should
         (equal
          (setq
           registered
           (dbus-register-property
            :session dbus--test-service dbus--test-path
            dbus--test-interface property1 :read "foo"))
          `((:property :session ,dbus--test-interface ,property1)
            (,dbus--test-service ,dbus--test-path))))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1)
          "foo"))
        ;; Due to `:read' access type, we don't get a proper reply
        ;; from `dbus-set-property'.
        (should
         (equal
          (butlast
           (should-error
            (dbus-set-property
             :session dbus--test-service dbus--test-path
             dbus--test-interface property1 "foofoo")))
          `(dbus-error ,dbus-error-property-read-only)))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1)
          "foo"))

        ;; `:write' property.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 :write "bar")
          `((:property :session ,dbus--test-interface ,property2)
            (,dbus--test-service ,dbus--test-path))))
        ;; Due to `:write' access type, we don't get a proper reply
        ;; from `dbus-get-property'.
        (should
         (equal
          (butlast
           (should-error
            (dbus-get-property
             :session dbus--test-service dbus--test-path
             dbus--test-interface property2)))
          `(dbus-error ,dbus-error-access-denied)))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 "barbar")
          "barbar"))
        ;; Still `:write' access type.
        (should
         (equal
          (butlast
           (should-error
            (dbus-get-property
             :session dbus--test-service dbus--test-path
             dbus--test-interface property2)))
          `(dbus-error ,dbus-error-access-denied)))

        ;; `:readwrite' property, typed value (Bug#43252).
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3 :readwrite :object-path "/baz")
          `((:property :session ,dbus--test-interface ,property3)
            (,dbus--test-service ,dbus--test-path))))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3)
          "/baz"))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3 :object-path "/baz/baz")
          "/baz/baz"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3)
          "/baz/baz"))

        ;; Not registered property.
        (should
         (equal
          (butlast
           (should-error
            (dbus-get-property
             :session dbus--test-service dbus--test-path
             dbus--test-interface property4)))
          `(dbus-error ,dbus-error-unknown-property)))
        (should
         (equal
          (butlast
           (should-error
            (dbus-set-property
             :session dbus--test-service dbus--test-path
             dbus--test-interface property4 "foobarbaz")))
          `(dbus-error ,dbus-error-unknown-property)))

        ;; `dbus-get-all-properties'.  We cannot retrieve a value for
        ;; the property with `:write' access type.
        (let ((result
               (dbus-get-all-properties
                :session dbus--test-service dbus--test-path
                dbus--test-interface)))
          (should (string-equal (cdr (assoc property1 result)) "foo"))
          (should (string-equal (cdr (assoc property3 result)) "/baz/baz"))
          (should-not (assoc property2 result)))

        ;; `dbus-get-all-managed-objects'.  We cannot retrieve a value for
        ;; the property with `:write' access type.
        (let ((result
               (dbus-get-all-managed-objects
                :session dbus--test-service dbus--test-path)))
          (should (setq result (cadr (assoc dbus--test-path result))))
          (should (setq result (cadr (assoc dbus--test-interface result))))
          (should (string-equal (cdr (assoc property1 result)) "foo"))
          (should (string-equal (cdr (assoc property3 result)) "/baz/baz"))
          (should-not (assoc property2 result)))

        ;; Unregister property.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered))
        (should
         (equal
          (butlast
           (should-error
            (dbus-get-property
             :session dbus--test-service dbus--test-path
             dbus--test-interface property1)))
          `(dbus-error ,dbus-error-unknown-property))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

;; The following test is inspired by Bug#43146.
(ert-deftest dbus-test06-register-property-several-paths ()
  "Check property registration for an own service at several paths."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((property1 "Property1")
            (property2 "Property2")
            (property3 "Property3"))

        ;; First path.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1 :readwrite "foo")
          `((:property :session ,dbus--test-interface ,property1)
            (,dbus--test-service ,dbus--test-path))))
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 :readwrite "bar")
          `((:property :session ,dbus--test-interface ,property2)
            (,dbus--test-service ,dbus--test-path))))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1)
          "foo"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2)
          "bar"))

        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1 "foofoo")
          "foofoo"))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 "barbar")
          "barbar"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1)
          "foofoo"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2)
          "barbar"))

        ;; Second path.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property2 :readwrite "foo")
          `((:property :session ,dbus--test-interface ,property2)
            (,dbus--test-service ,(concat dbus--test-path dbus--test-path)))))
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property3 :readwrite "bar")
          `((:property :session ,dbus--test-interface ,property3)
            (,dbus--test-service ,(concat dbus--test-path dbus--test-path)))))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property2)
          "foo"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property3)
          "bar"))

        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property2 "foofoo")
          "foofoo"))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property3 "barbar")
          "barbar"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property2)
          "foofoo"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service (concat dbus--test-path dbus--test-path)
           dbus--test-interface property3)
          "barbar"))

        ;; Everything is still fine, tested with `dbus-get-all-properties'.
        (let ((result
               (dbus-get-all-properties
                :session dbus--test-service dbus--test-path
                dbus--test-interface)))
          (should (string-equal (cdr (assoc property1 result)) "foofoo"))
          (should (string-equal (cdr (assoc property2 result)) "barbar"))
          (should-not (assoc property3 result)))

        (let ((result
               (dbus-get-all-properties
                :session dbus--test-service
                (concat dbus--test-path dbus--test-path) dbus--test-interface)))
          (should (string-equal (cdr (assoc property2 result)) "foofoo"))
          (should (string-equal (cdr (assoc property3 result)) "barbar"))
          (should-not (assoc property1 result)))

        ;; Final check with `dbus-get-all-managed-objects'.
        (let ((result
               (dbus-get-all-managed-objects :session dbus--test-service "/"))
              result1)
          (should (setq result1 (cadr (assoc dbus--test-path result))))
          (should (setq result1 (cadr (assoc dbus--test-interface result1))))
          (should (string-equal (cdr (assoc property1 result1)) "foofoo"))
          (should (string-equal (cdr (assoc property2 result1)) "barbar"))
          (should-not (assoc property3 result1))

          (should
           (setq
            result1
            (cadr (assoc (concat dbus--test-path dbus--test-path) result))))
          (should (setq result1 (cadr (assoc dbus--test-interface result1))))
          (should (string-equal (cdr (assoc property2 result1)) "foofoo"))
          (should (string-equal (cdr (assoc property3 result1)) "barbar"))
          (should-not (assoc property1 result1))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test06-register-property-emits-signal ()
  "Check property registration for an own service, including signalling."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((property "Property")
            (handler #'dbus--test-signal-handler))

        ;; Register signal handler.
        (should
         (equal
          (dbus-register-signal
           :session dbus--test-service dbus--test-path
           dbus-interface-properties "PropertiesChanged" handler)
          `((:signal :session ,dbus-interface-properties "PropertiesChanged")
            (,dbus--test-service ,dbus--test-path ,handler))))

        ;; Register property.
        (setq dbus--test-signal-received nil)
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property :readwrite "foo" 'emits-signal)
          `((:property :session ,dbus--test-interface ,property)
            (,dbus--test-service ,dbus--test-path))))
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        ;; It returns two arguments, "changed_properties" (an array of
        ;; dict entries) and "invalidated_properties" (an array of
        ;; strings).
        (should (equal dbus--test-signal-received `(((,property ("foo"))) ())))

        (should
         (equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property)
          "foo"))

        ;; Set property.  The new value shall be signalled.
        (setq dbus--test-signal-received nil)
        (should
         (equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property
           '(:array :byte 1 :byte 2 :byte 3))
          '(1 2 3)))
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should
         (equal
          dbus--test-signal-received `(((,property ((1 2 3)))) ())))

        (should
         (equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property)
          '(1 2 3))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defun dbus-test-all (&optional interactive)
  "Run all tests for \\[dbus]."
  (interactive "p")
  (funcall (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
           "^dbus"))

(provide 'dbus-tests)
;;; dbus-tests.el ends here
