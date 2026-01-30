;;; dbus-tests.el --- Tests of D-Bus integration into Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'dbus)

(defvar dbus-debug)
(defvar dbus-message-type-signal)
(declare-function dbus-get-unique-name "dbusbind.c" (bus))
(declare-function dbus-close-inhibitor-lock "dbusbind.c" (lock))
(declare-function dbus-registered-inhibitor-locks "dbusbind.c" ())
(declare-function dbus-make-inhibitor-lock "dbusbind.c"
                  (what why &optional block))

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

(defconst dbus--test-systemd-service "org.freedesktop.login1"
  "Systemd service.")

(defconst dbus--test-systemd-path "/org/freedesktop/login1"
  "Systemd object path.")

(defconst dbus--test-systemd-manager-interface "org.freedesktop.login1.Manager"
  "Systemd Manager interface.")

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
  (skip-unless dbus--test-enabled-session-bus)

  (let ((ustr (string-to-unibyte "0123abc_xyz\x01\xff"))
	(mstr (string-to-multibyte "Grüß Göttin"))
        (kstr (encode-coding-string "парола" 'koi8)))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array "")) ""))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array nil)) ""))
    (should
     (string-equal
      ;; The conversion could return a multibyte string, so we make it unibyte.
      (string-to-unibyte
       (dbus-byte-array-to-string (dbus-string-to-byte-array ustr)))
      ustr))
    (should
     (string-equal
      ;; The conversion could return a multibyte string, so we make it unibyte.
      (string-to-unibyte (dbus-byte-array-to-string (mapcar 'identity ustr)))
      ustr))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array mstr)) mstr))
    (should
     (string-equal
      ;; The conversion could return a multibyte string, so we make it unibyte.
      (string-to-unibyte
       (dbus-byte-array-to-string (dbus-string-to-byte-array kstr)))
      kstr))

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
  (skip-unless dbus--test-enabled-session-bus)

  ;; No argument or unknown keyword.
  (should-error
   (dbus-check-arguments :session dbus--test-service)
   :type 'wrong-number-of-arguments)
  (should-error
   (dbus-check-arguments :session dbus--test-service :keyword)
   :type 'wrong-type-argument)

  ;; `:string'.
  (should (dbus-check-arguments :session dbus--test-service "string"))
  (should (dbus-check-arguments :session dbus--test-service :string "string"))
  (should-error
   (dbus-check-arguments :session dbus--test-service :string)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :string 0.5)
   :type 'wrong-type-argument)

  ;; `:object-path'.
  (should
   (dbus-check-arguments
    :session dbus--test-service :object-path "/object/path"))
  (should-error
   (dbus-check-arguments :session dbus--test-service :object-path)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :object-path "string")
   :type 'dbus-error)
  (should-error
   (dbus-check-arguments :session dbus--test-service :object-path 0.5)
   :type 'wrong-type-argument)

  ;; `:signature'.
  (should (dbus-check-arguments :session dbus--test-service :signature "as"))
  (should-error
   (dbus-check-arguments :session dbus--test-service :signature)
   :type 'wrong-type-argument)
  ;; Raises an error on stderr.
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
  (should (dbus-check-arguments :session dbus--test-service :boolean 'whatever))
  (should-error
   (dbus-check-arguments :session dbus--test-service :boolean)
   :type 'wrong-type-argument)

  ;; `:byte'.
  (should (dbus-check-arguments :session dbus--test-service :byte 0))
  ;; Only the least significant byte is taken into account.
  (should
   (dbus-check-arguments :session dbus--test-service :byte most-positive-fixnum))
  (should-error
   (dbus-check-arguments :session dbus--test-service :byte)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :int16)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :uint16)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :int32)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :uint32)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :int64)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :uint64)
   :type 'wrong-type-argument)
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
   (dbus-check-arguments :session dbus--test-service :double)
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments :session dbus--test-service :double "string")
   :type 'wrong-type-argument)

  ;; `:unix-fd'.  UNIX file descriptors are transferred out-of-band.
  ;; We do not support this, and so we cannot do much testing here for
  ;; `:unix-fd' being an argument (which is an index to the file
  ;; descriptor in the array of file descriptors that accompany the
  ;; D-Bus message).  Mainly testing, that values out of `:uint32'
  ;; type range fail.
  (should (dbus-check-arguments :session dbus--test-service :unix-fd 0))
  (should-error
   (dbus-check-arguments :session dbus--test-service :unix-fd)
   :type 'wrong-type-argument)
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
  (skip-unless dbus--test-enabled-session-bus)

  ;; `:array'.  It contains several elements of the same type.
  (should (dbus-check-arguments :session dbus--test-service '("string")))
  (should (dbus-check-arguments :session dbus--test-service '(:array "string")))
  (should
   (dbus-check-arguments :session dbus--test-service '(:array :string "string")))
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:array :string "string1" "string2")))
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:array :signature "s" :signature "ao")))
  ;; Empty array (of strings).
  (should (dbus-check-arguments :session dbus--test-service '(:array)))
  ;; Empty array (of object paths).
  (should
   (dbus-check-arguments :session dbus--test-service '(:array :signature "o")))
  ;; Different element types.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array :string "string" :object-path "/object/path"))
   :type 'wrong-type-argument)
  ;; Different variant types in array don't matter.
  (should
   (dbus-check-arguments
    :session dbus--test-service
    '(:array
      (:variant :string "string1")
      (:variant (:struct :string "string2" :object-path "/object/path")))))

  ;; `:variant'.  It contains exactly one element.
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:variant :string "string")))
  (should
   (dbus-check-arguments
    :session dbus--test-service '(:variant (:array "string"))))
  ;; Empty variant.
  (should-error
   (dbus-check-arguments :session dbus--test-service '(:variant))
   :type 'wrong-type-argument)
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
    '(:array (:dict-entry :string "string" :boolean nil))))
  ;; This is an alternative syntax.
  (should
   (dbus-check-arguments
    :session dbus--test-service
    '(:array :dict-entry (:string "string" :boolean t))))
  ;; Empty dict-entry.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service '(:array (:dict-entry)))
   :type 'wrong-type-argument)
  ;; One element.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service '(:array (:dict-entry :string "string")))
   :type 'wrong-type-argument)
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array (:dict-entry :string "string" :boolean t :boolean t)))
   :type 'wrong-type-argument)
  ;; The first element is not of a basic type.
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
  ;; Different dict entry types in array.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array
      (:dict-entry :string "string1" :boolean t)
      (:dict-entry :string "string2" :object-path "/object/path")))
   :type 'wrong-type-argument)

  ;; `:struct'.  There is no restriction what could be an element of a struct.
  (should
   (dbus-check-arguments
    :session dbus--test-service
    '(:struct
      :string "string"
      :object-path "/object/path"
      (:variant (:array :unix-fd 1 :unix-fd 2 :unix-fd 3 :unix-fd 4)))))
  ;; Empty struct.
  (should-error
   (dbus-check-arguments :session dbus--test-service '(:struct))
   :type 'wrong-type-argument)
  ;; Different struct types in array.
  (should-error
   (dbus-check-arguments
    :session dbus--test-service
    '(:array
      (:struct :string "string1" :boolean t)
      (:struct :object-path "/object/path")))
   :type 'wrong-type-argument))

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

  ;; Unregistering a unique name returns nil.
  (should-not (dbus-unregister-service bus ":1.1"))

  ;; A service name is a string, constructed of at least two words
  ;; separated by ".".
  (should
   (equal
    (butlast
     (should-error (dbus-register-service bus "s")))
    `(dbus-error ,dbus-error-invalid-args)))

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
  (ert-with-temp-file tmpfile
    (let (bus pid)
      (with-temp-buffer
        (insert-file-contents (ert-resource-file "session.conf.in"))
        (search-forward "@testdir@")
        (replace-match (file-name-directory tmpfile))
        (write-file tmpfile))

      ;; Start bus.
      (with-temp-buffer
        (skip-unless
         (zerop
          (call-process
           "dbus-daemon" nil t nil "--fork"
           "--config-file" tmpfile "--print-address" "--print-pid")))
        (goto-char (point-min))
        (setq bus (buffer-substring (point) (line-end-position)))
        (forward-line)
        (setq pid (buffer-substring (point) (line-end-position))))

      ;; Run the test.
      (unwind-protect
	  (progn
	    (skip-unless
	     (dbus-ignore-errors
	       (and bus pid
		    (featurep 'dbusbind)
		    (dbus-init-bus bus)
		    (dbus-get-unique-name bus)
		    (dbus-register-service bus dbus--test-service))))
	    (dbus--test-register-service bus))

        ;; Save exit.
        (when pid (call-process "kill" nil nil nil pid))))))

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
   ((length= args 1)
    (car args))
   ;; Two arguments.
   ((length= args 2)
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
            dbus-debug ; There would be errors otherwise.
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
        ;; Three arguments, D-Bus error activated by `dbus-error'
        ;; signal.  On CentOS, it is not guaranteed which format the
        ;; error message arises.  (Bug#51369)
        (should
         (member
          (should-error
           (dbus-call-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method1 "foo" "bar" "baz"))
          `((dbus-error "D-Bus signal" "foo" "bar" "baz")
            (dbus-error
             ,dbus-error-failed
             "D-Bus error: \"D-Bus signal\", \"foo\", \"bar\", \"baz\""))))

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

(defun dbus--test-method-reentry-handler (&rest _args)
  "Method handler for `dbus-test04-method-reentry'."
  (dbus-get-all-managed-objects :session dbus--test-service dbus--test-path)
  42)

(ert-deftest dbus-test04-method-reentry ()
  "Check receiving method call while awaiting response.
Ensure that incoming method calls are handled when call to `dbus-call-method'
is in progress."
  :tags '(:expensive-test)
  ;; Simulate application registration.  (Bug#43251)
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((method "Reentry"))
        (should
         (equal
          (dbus-register-method
           :session dbus--test-service dbus--test-path
           dbus--test-interface method #'dbus--test-method-reentry-handler)
          `((:method :session ,dbus--test-interface ,method)
            (,dbus--test-service ,dbus--test-path
             dbus--test-method-reentry-handler))))

        (should
         (=
          (dbus-call-method
           :session dbus--test-service dbus--test-path
           dbus--test-interface method)
          42)))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test04-call-method-timeout ()
  "Verify `dbus-call-method' request timeout."
  :tags '(:expensive-test)
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))
  (dbus-register-service :session dbus--test-service)

  (unwind-protect
      (let ((start (current-time)))
        ;; Test timeout override for method call.
        (should-error
         (dbus-call-method
          :session dbus--test-service dbus--test-path
          dbus-interface-introspectable "Introspect" :timeout 2500)
         :type 'dbus-error)

        (should
         (< 2.4 (float-time (time-since start)) 2.7)))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defun dbus--test-method-authorizable-handler (&rest _args)
  "Method handler for `dbus-test04-call-method-authorizable'.
Returns the respective error."
  `(:error ,dbus-error-interactive-authorization-required
           "Interactive authentication required."))

(ert-deftest dbus-test04-call-method-authorizable ()
  "Verify `dbus-call-method' request authorizable."
  :tags '(:expensive-test)
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))
  (dbus-register-service :session dbus--test-service)

  (unwind-protect
      (let ((method "Method")
            (handler #'dbus--test-method-authorizable-handler)
            dbus-debug ; There would be errors otherwise.
            registered)

        ;; Register.
        (should
         (equal
          (setq
           registered
           (dbus-register-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method handler))
          `((:method :session ,dbus--test-interface ,method)
            (,dbus--test-service ,dbus--test-path ,handler))))

        ;; The error isn't seen, because it is transformed into a
        ;; warning.  So we check, whether a warning has arrived in the
        ;; respective buffer.
        (ignore-errors (kill-buffer "*Warnings*"))
        (should-not
         (dbus-call-method
          :session dbus--test-service dbus--test-path
          dbus--test-interface method "foo"))
        (should (get-buffer "*Warnings*"))

        ;; The same for asynchronous calls.
        (ignore-errors (kill-buffer "*Warnings*"))
        (dbus-call-method-asynchronously
         :session dbus--test-service dbus--test-path
         dbus--test-interface method #'ignore "foo")
        (with-timeout (1 (dbus--test-timeout-handler))
          (while (null (get-buffer "*Warnings*"))
            (read-event nil nil 0.1)))
        (should (get-buffer "*Warnings*"))

        ;; Unregister method.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered)))

    ;; Cleanup.
    (ignore-errors (kill-buffer "*Warnings*"))
    (dbus-unregister-service :session dbus--test-service))

  ;; Check parsing.  "org.freedesktop.DBus.ListNames" is agnostic to
  ;; :authorizable, so we can use it as test method.
  (when (dbus-ignore-errors
          (dbus-call-method
           :session dbus-service-dbus dbus-path-dbus
           dbus-interface-dbus "ListNames"))
    (should
     (dbus-call-method
      :session dbus-service-dbus dbus-path-dbus
      dbus-interface-dbus "ListNames" :authorizable t))

    (should
     (dbus-call-method
      :session dbus-service-dbus dbus-path-dbus
      dbus-interface-dbus "ListNames" :authorizable nil))

    (should
     (dbus-call-method
      :session dbus-service-dbus dbus-path-dbus
      dbus-interface-dbus "ListNames" :authorizable 'something))

    ;; Only method calls are allowed for :authorizable.
    (should-error
     (dbus-send-signal
      :session dbus--test-service dbus--test-path
      dbus--test-interface "Foo" :authorizable t "foo")
     :type 'dbus-error)))

(defvar dbus--test-event-expected nil
  "The expected event in `dbus--test-signal-handler'.")

(defvar dbus--test-signal-received nil
  "Received signal value in `dbus--test-signal-handler'.")

(defun dbus--test-signal-handler (&rest args)
  "Signal handler for `dbus-test*-signal' and `dbus-test08-register-monitor'."
  (ignore-error dbus-error
    ;; (message "%S" last-input-event)
    (let ((last-input-event last-input-event))
      (when (or (null dbus--test-event-expected)
                (and (equal (dbus-event-bus-name last-input-event)
                            (dbus-event-bus-name dbus--test-event-expected))
                     (equal (dbus-event-message-type last-input-event)
                            (dbus-event-message-type dbus--test-event-expected))
                     (equal (dbus-event-service-name last-input-event)
                            (dbus-event-service-name dbus--test-event-expected))
                     (equal (dbus-event-path-name last-input-event)
                            (dbus-event-path-name dbus--test-event-expected))
                     (equal (dbus-event-member-name last-input-event)
                            (dbus-event-member-name dbus--test-event-expected))))
        (push args dbus--test-signal-received)))))

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
        (should (equal dbus--test-signal-received '(("foo"))))

        ;; Send two arguments, compound types.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member
         '(:array :byte 1 :byte 2 :byte 3) '(:variant :string "bar"))
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should (equal dbus--test-signal-received '(((1 2 3) ("bar")))))

        ;; Unregister signal.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered)))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defun dbus--test-signal-handler1 (&rest args)
  "Signal handler for `dbus-test05-register-signal-several-handlers'."
  ;; (message "dbus--test-signal-handler1 %S" last-input-event)
  (dbus--test-signal-handler (cons "dbus--test-signal-handler1" args)))

(defun dbus--test-signal-handler2 (&rest args)
  "Signal handler for `dbus-test05-register-signal-several-handlers'."
  ;; (message "dbus--test-signal-handler2 %S" last-input-event)
  (dbus--test-signal-handler (cons "dbus--test-signal-handler2" args)))

(ert-deftest dbus-test05-register-signal-several-handlers ()
  "Check signal registration for an own service.
It shall call several handlers per received signal."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((member "Member")
            (handler1 #'dbus--test-signal-handler1)
            (handler2 #'dbus--test-signal-handler2)
            registered1 registered2)

        ;; Register signal handlers.
        (should
         (equal
          (setq
           registered1
           (dbus-register-signal
            :session dbus--test-service dbus--test-path
            dbus--test-interface member handler1))
          `((:signal :session ,dbus--test-interface ,member)
            (,dbus--test-service ,dbus--test-path ,handler1))))
        (should
         (equal
          (setq
           registered2
           (dbus-register-signal
            :session dbus--test-service dbus--test-path
            dbus--test-interface member handler2))
          `((:signal :session ,dbus--test-interface ,member)
            (,dbus--test-service ,dbus--test-path ,handler2))))

        ;; Send one argument, basic type.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member "foo")
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (length< dbus--test-signal-received 2)
            (read-event nil nil 0.1)))
        (should
         (member
          '(("dbus--test-signal-handler1" "foo")) dbus--test-signal-received))
        (should
         (member
          '(("dbus--test-signal-handler2" "foo")) dbus--test-signal-received))

        ;; Unregister one signal.
        (should (dbus-unregister-object registered1))
        (should-not (dbus-unregister-object registered1))

        ;; Send one argument, basic type.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member "foo")
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should
         (equal
          dbus--test-signal-received '((("dbus--test-signal-handler2" "foo")))))

        ;; Unregister the other signal.
        (should (dbus-unregister-object registered2))
        (should-not (dbus-unregister-object registered2)))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test05-register-signal-with-nils ()
  "Check signal registration for an own service.
SERVICE, PATH, INTERFACE and SIGNAL are ‘nil’.  This is interpreted as a
wildcard for the respective argument."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((member "Member")
            (handler #'dbus--test-signal-handler)
            registered)

        ;; Filter received signals in signal handler.
        (setq dbus--test-event-expected
              `(dbus-event :session ,dbus-message-type-signal
                0 ;; Serial number doesn't matter.
	        ,(dbus-get-unique-name :session)
                nil ;; Destination doesn't matter.
	        ,dbus--test-path ,dbus--test-interface ,member ,handler))

        ;; Register signal handler.
        (should
         (equal
          (setq
           registered
           (dbus-register-signal
            :session nil nil nil nil handler))
          `((:signal :session nil nil)
            (nil nil ,handler))))

        (dbus-register-signal
         :session nil dbus--test-path
         dbus--test-interface member handler)
        (dbus-register-signal
         :session dbus--test-service nil
         dbus--test-interface member handler)
        (dbus-register-signal
         :session dbus--test-service dbus--test-path
         nil member handler)
        (dbus-register-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface nil handler)

        ;; Send one argument, basic type.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member "foo")
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should (equal dbus--test-signal-received '(("foo"))))

        ;; Unregister signal.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered)))

    ;; Cleanup.
    (setq dbus--test-event-expected nil)
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
  "Check property registration for an own service, including signaling."
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
        ;; It returns three arguments, "interface" (a string),
        ;; "changed_properties" (an array of dict entries) and
        ;; "invalidated_properties" (an array of strings).
        (should
         (equal dbus--test-signal-received
                `((,dbus--test-interface ((,property ("foo"))) ()))))

        (should
         (equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property)
          "foo"))

        ;; Set property.  The new value shall be signaled.
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
          dbus--test-signal-received
          `((,dbus--test-interface ((,property ((1 2 3)))) ()))))

        (should
         (equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property)
          '(1 2 3))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defsubst dbus--test-run-property-test (selector name value expected)
  "Generate a property test: register, set, get, getall sequence.
This is a helper function for the macro `dbus--test-property'.
The argument SELECTOR indicates whether the test should expand to
`dbus-register-property' (if SELECTOR is `register') or
`dbus-set-property' (if SELECTOR is `set').
The argument NAME is the property name.
The argument VALUE is the value to register or set.
The argument EXPECTED is a transformed VALUE representing the
form `dbus-get-property' should return."
  (cond
   ((eq selector 'register)
    (should
     (equal
      (dbus-register-property
       :session dbus--test-service dbus--test-path dbus--test-interface name
       :readwrite value)
      `((:property :session ,dbus--test-interface ,name)
        (,dbus--test-service ,dbus--test-path)))))

   ((eq selector 'set)
    (should
     (equal
      (dbus-set-property
       :session dbus--test-service dbus--test-path dbus--test-interface name
       value)
      expected)))

   (t (signal 'wrong-type-argument "Selector should be 'register or 'set.")))

  (should
   (equal
    (dbus-get-property
     :session dbus--test-service dbus--test-path dbus--test-interface name)
    expected))

  (let ((result
         (dbus-get-all-properties
          :session dbus--test-service dbus--test-path dbus--test-interface)))
    (should (equal (cdr (assoc name result)) expected)))

  (let ((result
         (dbus-get-all-managed-objects :session dbus--test-service "/"))
        result1)
    (should (setq result1 (cadr (assoc dbus--test-path result))))
    (should (setq result1 (cadr (assoc dbus--test-interface result1))))
    (should (equal (cdr (assoc name result1)) expected))))

(defsubst dbus--test-property (name &rest value-list)
  "Test a D-Bus property named by string argument NAME.
The argument VALUE-LIST is a sequence of pairs, where each pair
represents a value form and an expected returned value form.  The
first pair in VALUES is used for `dbus-register-property'.
Subsequent pairs of the list are tested with `dbus-set-property'."
  (let ((values (car value-list)))
    (dbus--test-run-property-test
     'register name (car values) (cdr values)))
  (dolist (values (cdr value-list))
    (dbus--test-run-property-test
     'set name (car values) (cdr values))))

(ert-deftest dbus-test06-property-types ()
  "Check property access and mutation for an own service."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))
  (dbus-register-service :session dbus--test-service)

  (unwind-protect
      (progn
        (dbus--test-property
         "ByteArray"
         '((:array :byte 1 :byte 2 :byte 3) . (1 2 3))
         '((:array :byte 4 :byte 5 :byte 6) . (4 5 6)))

        (dbus--test-property
         "StringArray"
         '((:array "one" "two" :string "three") . ("one" "two" "three"))
         '((:array :string "four" :string "five" "six") . ("four" "five" "six")))

        (dbus--test-property
         "ObjectArray"
         '((:array
            :object-path "/node00"
            :object-path "/node01"
            :object-path "/node0/node02")
           . ("/node00" "/node01" "/node0/node02"))
         '((:array
            :object-path "/node10"
            :object-path "/node11"
            :object-path "/node0/node12")
           . ("/node10" "/node11" "/node0/node12")))

        (dbus--test-property
         "Dictionary"
         '((:array
            :dict-entry (:string "four" (:variant :string "value of four"))
            :dict-entry ("five" (:variant :object-path "/node0"))
            :dict-entry ("six"  (:variant (:array :byte 4 :byte 5 :byte 6))))
           . (("four"
               ("value of four"))
              ("five"
               ("/node0"))
              ("six"
               ((4 5 6)))))
         '((:array
            :dict-entry
            (:string "key0" (:variant (:array :byte 7 :byte 8 :byte 9)))
            :dict-entry ("key1" (:variant :string "value"))
            :dict-entry ("key2" (:variant :object-path "/node0/node1")))
           . (("key0"
               ((7 8 9)))
              ("key1"
               ("value"))
              ("key2"
               ("/node0/node1")))))

        (dbus--test-property            ; Syntax emphasizing :dict compound type.
         "Dictionary"
         '((:array
            (:dict-entry :string "seven" (:variant :string "value of seven"))
            (:dict-entry "eight" (:variant :object-path "/node8"))
            (:dict-entry "nine"  (:variant (:array :byte 9 :byte 27 :byte 81))))
           . (("seven"
               ("value of seven"))
              ("eight"
               ("/node8"))
              ("nine"
               ((9 27 81)))))
         '((:array
            (:dict-entry
             :string "key4" (:variant (:array :byte 7 :byte 49 :byte 125)))
            (:dict-entry "key5" (:variant :string "obsolete"))
            (:dict-entry "key6" (:variant :object-path "/node6/node7")))
           . (("key4"
               ((7 49 125)))
              ("key5"
               ("obsolete"))
              ("key6"
               ("/node6/node7")))))

        (dbus--test-property
         "ByteDictionary"
         '((:array
            (:dict-entry :byte  8 (:variant :string "byte-eight"))
            (:dict-entry :byte 16 (:variant :object-path "/byte/sixteen"))
            (:dict-entry :byte 48 (:variant (:array :byte 8 :byte 9 :byte 10))))
           . (( 8 ("byte-eight"))
              (16 ("/byte/sixteen"))
              (48 ((8 9 10))))))

        (dbus--test-property
         "Variant"
         '((:variant "Variant string") . ("Variant string"))
         '((:variant :byte 42) . (42))
         '((:variant :uint32 1000000) . (1000000))
         '((:variant :object-path "/variant/path") . ("/variant/path"))
         '((:variant :signature "a{sa{sv}}") . ("a{sa{sv}}"))
         '((:variant
            (:struct
             42 "string" (:object-path "/structure/path") (:variant "last")))
           . ((42 "string" ("/structure/path") ("last")))))

        ;; Test that :read prevents writes.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "StringArray" :read '(:array "one" "two" :string "three"))
          `((:property :session ,dbus--test-interface "StringArray")
	    (,dbus--test-service ,dbus--test-path))))

        (should-error          ; Cannot set property with :read access.
         (dbus-set-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "StringArray" '(:array "seven" "eight" :string "nine"))
         :type 'dbus-error)

        (should                    ; Property value preserved on error.
         (equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "StringArray")
          '("one" "two" "three")))

        ;; Test mismatched types in array.
        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "MixedArray" :readwrite
          '(:array
            :object-path "/node00"
            :string "/node01"
            :object-path "/node0/node02"))
         :type 'wrong-type-argument)

        ;; Test in-range integer values.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue" :readwrite :byte 255)
          `((:property :session ,dbus--test-interface "ByteValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue")
          255))

        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ShortValue" :readwrite :int16 32767)
          `((:property :session ,dbus--test-interface "ShortValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ShortValue")
          32767))

        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "UShortValue" :readwrite :uint16 65535)
          `((:property :session ,dbus--test-interface "UShortValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "UShortValue")
          65535))

        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "IntValue" :readwrite :int32 2147483647)
          `((:property :session ,dbus--test-interface "IntValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface "IntValue")
          2147483647))

        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "UIntValue" :readwrite :uint32 4294967295)
          `((:property :session ,dbus--test-interface "UIntValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "UIntValue")
          4294967295))

        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "LongValue" :readwrite :int64 9223372036854775807)
          `((:property :session ,dbus--test-interface "LongValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "LongValue")
          9223372036854775807))

        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ULongValue" :readwrite :uint64 18446744073709551615)
          `((:property :session ,dbus--test-interface "ULongValue")
	    (,dbus--test-service ,dbus--test-path))))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ULongValue")
          18446744073709551615))

        ;; Test integer overflow.
        (should
         (=
          (dbus-set-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue" :byte 520)
          8))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue")
          8))

        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "ShortValue" :readwrite :int16 32800)
         :type 'args-out-of-range)

        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "UShortValue" :readwrite :uint16 65600)
         :type 'args-out-of-range)

        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "IntValue" :readwrite :int32 2147483700)
         :type 'args-out-of-range)

        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "UIntValue" :readwrite :uint32 4294967300)
         :type 'args-out-of-range)

        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "LongValue" :readwrite :int64 9223372036854775900)
         :type 'args-out-of-range)

        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "ULongValue" :readwrite :uint64 18446744073709551700)
         :type 'args-out-of-range)

        ;; dbus-set-property may change property type.
        (should
         (=
          (dbus-set-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue" 1024)
          1024))

        (should
         (=
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue")
          1024))

        (should                         ; Another change property type test.
         (equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue" :boolean t)
          t))

        (should
         (eq
          (dbus-get-property
           :session dbus--test-service dbus--test-path dbus--test-interface
           "ByteValue")
          t))

        ;; Test invalid type specification.
        (should-error
         (dbus-register-property
          :session dbus--test-service dbus--test-path dbus--test-interface
          "InvalidType" :readwrite :keyword 128)
         :type 'wrong-type-argument))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defun dbus--test-introspect ()
  "Return test introspection string."
  (when (string-equal dbus--test-path (dbus-event-path-name last-input-event))
    (with-temp-buffer
      (insert-file-contents-literally
       (ert-resource-file "org.gnu.Emacs.TestDBus.xml"))
      (buffer-string))))

(defsubst dbus--test-validate-interface
  (iface-name expected-properties expected-methods expected-signals
              expected-annotations)
  "Validate an interface definition for `dbus-test07-introspection'.
The argument IFACE-NAME is a string naming the interface to validate.
The arguments EXPECTED-PROPERTIES, EXPECTED-METHODS, EXPECTED-SIGNALS, and
EXPECTED-ANNOTATIONS represent the names of the interface's properties,
methods, signals, and annotations, respectively."

  (let ((interface
         (dbus-introspect-get-interface
          :session dbus--test-service dbus--test-path iface-name)))
    (pcase-let ((`(interface ((name . ,name)) . ,rest) interface))
      (should
       (string-equal name iface-name))
      (should
       (string-equal name (dbus-introspect-get-attribute interface "name")))

      (let (properties methods signals annotations)
        (mapc (lambda (x)
                (let ((name (dbus-introspect-get-attribute x "name")))
                  (cond
                   ((eq 'property (car x))   (push name properties))
                   ((eq 'method (car x))     (push name methods))
                   ((eq 'signal (car x))     (push name signals))
                   ((eq 'annotation (car x)) (push name annotations)))))
              rest)

        (should
         (equal
          (nreverse properties)
          expected-properties))
        (should
         (equal
          (nreverse methods)
          expected-methods))
        (should
         (equal
          (nreverse signals)
          expected-signals))
        (should
         (equal
          (nreverse annotations)
          expected-annotations))))))

(defsubst dbus--test-validate-annotations (annotations expected-annotations)
  "Validate a list of D-Bus ANNOTATIONS.
Ensure each string in EXPECTED-ANNOTATIONS names an element of ANNOTATIONS.
And ensure each ANNOTATIONS has a value attribute marked \"true\"."
  (mapc
   (lambda (annotation)
     (let ((name (dbus-introspect-get-attribute annotation "name"))
           (value (dbus-introspect-get-attribute annotation "value")))
       (should
        (member name expected-annotations))
       (should
        (equal value "true"))))
   annotations))

(defsubst dbus--test-validate-property
  (interface property-name _expected-annotations &rest expected-args)
  "Validate a property definition for `dbus-test07-introspection'.

The argument INTERFACE is a string naming the interface owning PROPERTY-NAME.
The argument PROPERTY-NAME is a string naming the property to validate.
The arguments EXPECTED-ANNOTATIONS is a list of strings matching
the annotation names defined for the method or signal.
The argument EXPECTED-ARGS is a list of expected arguments for the property."
  (let* ((property
          (dbus-introspect-get-property
           :session dbus--test-service dbus--test-path interface property-name))
         (name (dbus-introspect-get-attribute property "name"))
         (type (dbus-introspect-get-attribute property "type"))
         (access (dbus-introspect-get-attribute property "access"))
         (expected (assoc-string name expected-args)))
    (should expected)

    (should
     (string-equal name property-name))

    (should
     (string-equal
      (nth 0 expected)
      name))

    (should
     (string-equal
      (nth 1 expected)
      type))

    (should
     (string-equal
      (nth 2 expected)
      access))))

(defsubst dbus--test-validate-m-or-s (tree expected-annotations expected-args)
  "Validate a method or signal definition for `dbus-test07-introspection'.
The argument TREE is an sexp returned from either `dbus-introspect-get-method'
or `dbus-introspect-get-signal'
The arguments EXPECTED-ANNOTATIONS is a list of strings matching
the annotation names defined for the method or signal.
The argument EXPECTED-ARGS is a list of expected arguments for
the method or signal."
  (let (args annotations)
    (mapc (lambda (elem)
            (cond
             ((eq 'arg (car elem)) (push elem args))
             ((eq 'annotation (car elem)) (push elem annotations))))
          tree)
    (should
     (equal
      (nreverse args)
      expected-args))
    (dbus--test-validate-annotations annotations expected-annotations)))

(defsubst dbus--test-validate-signal
  (interface signal-name expected-annotations &rest expected-args)
  "Validate a signal definition for `dbus-test07-introspection'.

The argument INTERFACE is a string naming the interface owning SIGNAL-NAME.
The argument SIGNAL-NAME is a string naming the signal to validate.
The arguments EXPECTED-ANNOTATIONS is a list of strings matching
the annotation names defined for the signal.
The argument EXPECTED-ARGS is a list of expected arguments for the signal."
  (let ((signal
         (dbus-introspect-get-signal
          :session dbus--test-service dbus--test-path interface signal-name)))
    (pcase-let ((`(signal ((name . ,name)) . ,rest) signal))
      (should
       (string-equal name signal-name))
      (should
       (string-equal name (dbus-introspect-get-attribute signal "name")))
      (dbus--test-validate-m-or-s rest expected-annotations expected-args))))

(defsubst dbus--test-validate-method
  (interface method-name expected-annotations &rest expected-args)
  "Validate a method definition for `dbus-test07-introspection'.

The argument INTERFACE is a string naming the interface owning METHOD-NAME.
The argument METHOD-NAME is a string naming the method to validate.
The arguments EXPECTED-ANNOTATIONS is a list of strings matching
the annotation names defined for the method.
The argument EXPECTED-ARGS is a list of expected arguments for the method."
  (let ((method
         (dbus-introspect-get-method
          :session dbus--test-service dbus--test-path interface method-name)))
    (pcase-let ((`(method ((name . ,name)) . ,rest) method))
      (should
       (string-equal name method-name))
      (should
       (string-equal name (dbus-introspect-get-attribute method "name")))
      (dbus--test-validate-m-or-s rest expected-annotations expected-args))))

(ert-deftest dbus-test07-introspection ()
  "Register an Introspection interface then query it."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))
  (dbus-register-service :session dbus--test-service)

  ;; Prepare introspection response.
  (dbus-register-method
   :session dbus--test-service dbus--test-path dbus-interface-introspectable
   "Introspect" 'dbus--test-introspect)
  (dbus-register-method
   :session dbus--test-service (concat dbus--test-path "/node0")
   dbus-interface-introspectable
   "Introspect" 'dbus--test-introspect)
  (dbus-register-method
   :session dbus--test-service (concat dbus--test-path "/node1")
   dbus-interface-introspectable
   "Introspect" 'dbus--test-introspect)
  (unwind-protect
      (let ((start (current-time)))
        ;; dbus-introspect-get-node-names
        (should
         (equal
          (dbus-introspect-get-node-names
           :session dbus--test-service dbus--test-path)
          '("node0" "node1")))

        ;; dbus-introspect-get-all-nodes
        (should
         (equal
          (dbus-introspect-get-all-nodes
           :session dbus--test-service dbus--test-path)
          (list dbus--test-path
                (concat dbus--test-path "/node0")
                (concat dbus--test-path "/node1"))))

        ;; dbus-introspect-get-interface-names
        (let ((interfaces
               (dbus-introspect-get-interface-names
                :session dbus--test-service dbus--test-path)))

          (should
           (equal
            interfaces
            `(,dbus-interface-introspectable
              ,dbus-interface-properties
              ,dbus--test-interface)))

          (dbus--test-validate-interface
           dbus-interface-introspectable nil '("Introspect") nil nil)

          ;; dbus-introspect-get-interface via `dbus--test-validate-interface'.
          (dbus--test-validate-interface
           dbus-interface-properties nil
           '("Get" "Set" "GetAll") '("PropertiesChanged") nil)

          (dbus--test-validate-interface
           dbus--test-interface '("Connected" "Player")
           '("Connect" "DeprecatedMethod0" "DeprecatedMethod1") nil
           `(,dbus-annotation-deprecated)))

        ;; dbus-introspect-get-method-names
        (let ((methods
               (dbus-introspect-get-method-names
                :session dbus--test-service dbus--test-path
                dbus--test-interface)))
          (should
           (equal
            methods
            '("Connect" "DeprecatedMethod0" "DeprecatedMethod1")))

          ;; dbus-introspect-get-method via `dbus--test-validate-method'.
          (dbus--test-validate-method
           dbus--test-interface "Connect" nil
           '(arg ((name . "uuid")      (type . "s")     (direction . "in")))
           '(arg ((name . "mode")      (type . "y")     (direction . "in")))
           '(arg ((name . "options")   (type . "a{sv}") (direction . "in")))
           '(arg ((name . "interface") (type . "s")     (direction . "out"))))

          (dbus--test-validate-method
           dbus--test-interface "DeprecatedMethod0"
           `(,dbus-annotation-deprecated))

          (dbus--test-validate-method
           dbus--test-interface "DeprecatedMethod1"
           `(,dbus-annotation-deprecated)))

        ;; dbus-introspect-get-signal-names
        (let ((signals
               (dbus-introspect-get-signal-names
                :session dbus--test-service dbus--test-path
                dbus-interface-properties)))
          (should
           (equal
            signals
            '("PropertiesChanged")))

          ;; dbus-introspect-get-signal via `dbus--test-validate-signal'.
          (dbus--test-validate-signal
           dbus-interface-properties "PropertiesChanged" nil
           '(arg ((name . "interface")              (type . "s")))
           '(arg ((name . "changed_properties")     (type . "a{sv}")))
           '(arg ((name . "invalidated_properties") (type . "as")))))

        ;; dbus-intropct-get-property-names
        (let ((properties
               (dbus-introspect-get-property-names
                :session dbus--test-service dbus--test-path
                dbus--test-interface)))
          (should
           (equal
            properties
            '("Connected" "Player")))

          ;; dbus-introspect-get-property via `dbus--test-validate-property'.
          (dbus--test-validate-property
           dbus--test-interface "Connected" nil
           '("Connected" "b" "read")
           '("Player" "o" "read")))

        ;; Elapsed time over a second suggests timeouts.
        (should
         (< 0.0 (float-time (time-since start)) 1.0)))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test07-introspection-timeout ()
  "Verify introspection request timeouts."
  :tags '(:expensive-test)
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))
  (dbus-register-service :session dbus--test-service)

  (unwind-protect
      (let ((start (current-time)))
        (dbus-introspect-xml :session dbus--test-service dbus--test-path)
        ;; Introspection internal timeout is one second.
        (should
         (< 1.0 (float-time (time-since start)))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test08-register-monitor ()
  "Check monitor registration."
  :tags '(:expensive-test)
  (skip-unless dbus--test-enabled-session-bus)

  (unwind-protect
      (let ((member "Member")
            (handler #'dbus--test-signal-handler)
            registered)

        ;; Filter received signals in signal handler.
        (setq dbus--test-event-expected
              `(dbus-event :session-private ,dbus-message-type-signal
                0 ;; Serial number doesn't matter.
	        ,(dbus-get-unique-name :session)
                nil ;; Destination doesn't matter.
	        ,dbus--test-path ,dbus--test-interface ,member ,handler))

        ;; Register monitor.
        (should
         (equal
          (setq
           registered
           (dbus-register-monitor :session handler))
          `((:monitor :session-private)
	    (nil nil ,handler))))

        ;; Send a signal, shall be traced.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member "foo")
	(with-timeout (1 (dbus--test-timeout-handler))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))

        ;; Unregister monitor.
        (should (dbus-unregister-object registered))
        (should-not (dbus-unregister-object registered))

        ;; Send a signal, shall not be traced.
        (setq dbus--test-signal-received nil)
        (dbus-send-signal
         :session dbus--test-service dbus--test-path
         dbus--test-interface member "foo")
	(with-timeout (1 (ignore))
          (while (null dbus--test-signal-received)
            (read-event nil nil 0.1)))
        (should-not dbus--test-signal-received)

        ;; Unregister monitor.
        ;; TODO: This seems to be a noop.  And it returns nil.
        (dbus-unregister-object registered))

    ;; Cleanup.
    (setq dbus--test-event-expected nil)
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test09-get-managed-objects ()
  "Check `dbus-get-all-managed-objects'."
  :tags '(:expensive-test)
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))
  (dbus-register-service :session dbus--test-service)

  (unwind-protect
      (let ((interfaces
             `((,(concat dbus--test-interface ".I0")
                ((,(concat dbus--test-path "/obj1")
                  (("I0Property1" . "Zero one one")
                   ("I0Property2" . "Zero one two")
                   ("I0Property3" . "Zero one three")))
                 (,(concat dbus--test-path "/obj0/obj2")
                  (("I0Property1" . "Zero two one")
                   ("I0Property2" . "Zero two two")
                   ("I0Property3" . "Zero two three")))
                 (,(concat dbus--test-path "/obj0/obj3")
                  (("I0Property1" . "Zero three one")
                   ("I0Property2" . "Zero three two")
                   ("I0Property3" . "Zero three three")))))
               (,(concat dbus--test-interface ".I1")
                ((,(concat dbus--test-path "/obj0/obj2")
                  (("I1Property1" . "One one one")
                   ("I1Property2" . "One one two")))
                 (,(concat dbus--test-path "/obj0/obj3")
                  (("I1Property1" . "One two one")
                   ("I1Property2" . "One two two"))))))))

        (should-not
         (dbus-get-all-managed-objects
          :session dbus--test-service dbus--test-path))

        (dolist (interface interfaces)
          (pcase-let ((`(,iname ,objs) interface))
            (dolist (obj objs)
              (pcase-let ((`(,path ,props) obj))
                (dolist (prop props)
                  (should
                   (equal
                    (dbus-register-property
                     :session dbus--test-service path iname
                     (car prop) :readwrite (cdr prop))
                    `((:property :session ,iname ,(car prop))
                      (,dbus--test-service ,path)))))))))

        (let ((result (dbus-get-all-managed-objects
                       :session dbus--test-service dbus--test-path)))
          (should
           (length= result 3))

          (dolist (interface interfaces)
            (pcase-let ((`(,iname ,objs) interface))
              (dolist (obj objs)
                (pcase-let ((`(,path ,props) obj))
                  (let* ((object (cadr (assoc-string path result)))
                         (iface  (cadr (assoc-string iname object))))
                    (should object)
                    (should iface)
                    (dolist (prop props)
                      (should (equal (cdr (assoc-string (car prop) iface))
                                     (cdr prop))))))))))

        (let ((result (dbus-get-all-managed-objects
                       :session dbus--test-service
                       (concat dbus--test-path "/obj0"))))
          (should
           (length= result 2))

          (dolist (interface interfaces)
            (pcase-let ((`(,iname ,objs) interface))
              (dolist (obj objs)
                (pcase-let ((`(,path ,props) obj))
                  (when (string-prefix-p (concat dbus--test-path "/obj0/") path)
                    (let* ((object (cadr (assoc-string path result)))
                           (iface  (cadr (assoc-string iname object))))
                      (should object)
                      (should iface)
                      (dolist (prop props)
                        (should (equal (cdr (assoc-string (car prop) iface))
                                       (cdr prop)))))))))))

        (let ((result (dbus-get-all-managed-objects
                       :session dbus--test-service
                       (concat dbus--test-path "/obj0/obj2"))))
          (should
           (length= result 1))

          (dolist (interface interfaces)
            (pcase-let ((`(,iname ,objs) interface))
              (dolist (obj objs)
                (pcase-let ((`(,path ,props) obj))
                  (when (string-prefix-p
                         (concat dbus--test-path "/obj0/obj2") path)
                    (let* ((object (cadr (assoc-string path result)))
                           (iface  (cadr (assoc-string iname object))))
                      (should object)
                      (should iface)
                      (dolist (prop props)
                        (should (equal (cdr (assoc-string (car prop) iface))
                                       (cdr prop))))))))))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test10-inhibitor-locks ()
  "Check `dbus-*-inhibitor-locks'."
  :tags '(:expensive-test)
  (skip-unless dbus--test-enabled-system-bus)
  (skip-unless (dbus-ping :system dbus--test-systemd-service 1000))

  (let (lock1 lock2)
    ;; Create inhibitor lock.
    (setq lock1 (dbus-make-inhibitor-lock "sleep" "Test delay"))
    (should (natnump lock1))
    ;; The lock is reported by systemd.
    (should
     (member
      (list "sleep" "Emacs" "Test delay" "delay" (user-uid) (emacs-pid))
      (dbus-call-method
       :system dbus--test-systemd-service dbus--test-systemd-path
       dbus--test-systemd-manager-interface "ListInhibitors")))
    ;; The lock is registered internally.
    (should
     (member
      (list lock1 "sleep" "Test delay" nil)
      (dbus-registered-inhibitor-locks)))
    ;; There exist a file descriptor.
    (when (file-directory-p (format "/proc/%d/fd" (emacs-pid)))
      (should (file-symlink-p (format "/proc/%d/fd/%d" (emacs-pid) lock1))))

    ;; It is not possible to modify registered inhibitor locks on Lisp level.
    (setcar (assoc lock1 (dbus-registered-inhibitor-locks)) 'malicious)
    (should (assoc lock1 (dbus-registered-inhibitor-locks)))
    (should-not (assoc 'malicious (dbus-registered-inhibitor-locks)))

    ;; Creating it again returns the same inhibitor lock.
    (should (= lock1 (dbus-make-inhibitor-lock "sleep" "Test delay")))

    ;; Create another inhibitor lock.
    (setq lock2 (dbus-make-inhibitor-lock "sleep" "Test block" 'block))
    (should (natnump lock2))
    (should-not (= lock1 lock2))
    ;; The lock is reported by systemd.
    (should
     (member
      (list "sleep" "Emacs" "Test block" "block" (user-uid) (emacs-pid))
      (dbus-call-method
       :system dbus--test-systemd-service dbus--test-systemd-path
       dbus--test-systemd-manager-interface "ListInhibitors")))
    ;; The lock is registered internally.
    (should
     (member
      (list lock2 "sleep" "Test block" t)
      (dbus-registered-inhibitor-locks)))
    ;; There exist a file descriptor.
    (when (file-directory-p (format "/proc/%d/fd" (emacs-pid)))
      (should (file-symlink-p (format "/proc/%d/fd/%d" (emacs-pid) lock2))))

    ;; Close the first inhibitor lock.
    (should (dbus-close-inhibitor-lock lock1))
    ;; The internal registration has gone.
    (should-not
     (member
      (list lock1 "sleep" "Test delay" nil)
      (dbus-registered-inhibitor-locks)))
    ;; The file descriptor has been deleted.
    (when (file-directory-p (format "/proc/%d/fd" (emacs-pid)))
      (should-not (file-symlink-p (format "/proc/%d/fd/%d" (emacs-pid) lock1))))

    ;; Closing it again is a noop.
    (should-not (dbus-close-inhibitor-lock lock1))

    ;; Creating it again returns (another?) inhibitor lock.
    (setq lock1 (dbus-make-inhibitor-lock "sleep" "Test delay"))
    (should (natnump lock1))
    ;; The lock is registered internally.
    (should
     (member
      (list lock1 "sleep" "Test delay" nil)
      (dbus-registered-inhibitor-locks)))
    ;; There exist a file descriptor.
    (when (file-directory-p (format "/proc/%d/fd" (emacs-pid)))
      (should (file-symlink-p (format "/proc/%d/fd/%d" (emacs-pid) lock1))))

    ;; Close the inhibitor locks.
    (should (dbus-close-inhibitor-lock lock1))
    (should (dbus-close-inhibitor-lock lock2))))

(defun dbus-test-all (&optional interactive)
  "Run all tests for \\[dbus]."
  (interactive "p")
  (funcall (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
           "^dbus"))

(provide 'dbus-tests)
;;; dbus-tests.el ends here
