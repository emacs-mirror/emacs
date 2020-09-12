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

(setq dbus-show-dbus-errors nil)

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
          ;; We don't care the error message text.
          (butlast
           (should-error
            (dbus-call-method
             :session dbus--test-service dbus--test-path
             dbus--test-interface method1 :timeout 10 "foo")))
          `(dbus-error ,dbus-error-no-reply))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

;; TODO: Test emits-signal.
(ert-deftest dbus-test05-register-property ()
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
        (should-not
         (dbus-set-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property1 "foofoo"))
        (let ((dbus-show-dbus-errors t))
          (should
           (equal
            ;; We don't care the error message text.
            (butlast
             (should-error
              (dbus-set-property
               :session dbus--test-service dbus--test-path
               dbus--test-interface property1 "foofoo")))
            `(dbus-error ,dbus-error-property-read-only))))
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
        (should-not
         (dbus-get-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property2))
        (let ((dbus-show-dbus-errors t))
          (should
           (equal
            ;; We don't care the error message text.
            (butlast
             (should-error
              (dbus-get-property
               :session dbus--test-service dbus--test-path
               dbus--test-interface property2)))
            `(dbus-error ,dbus-error-access-denied))))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 "barbar")
          "barbar"))
        (should-not ;; Due to `:write' access type.
         (dbus-get-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property2))

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
        (should-not
         (dbus-get-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property4))
        (let ((dbus-show-dbus-errors t))
          (should
           (equal
            ;; We don't care the error message text.
            (butlast
             (should-error
              (dbus-get-property
               :session dbus--test-service dbus--test-path
               dbus--test-interface property4)))
            `(dbus-error ,dbus-error-unknown-property))))
        (should-not
         (dbus-set-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property4 "foobarbaz"))
        (let ((dbus-show-dbus-errors t))
          (should
           (equal
            ;; We don't care the error message text.
            (butlast
             (should-error
              (dbus-set-property
               :session dbus--test-service dbus--test-path
               dbus--test-interface property4 "foobarbaz")))
            `(dbus-error ,dbus-error-unknown-property))))

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
        (should-not
         (dbus-get-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property1))
        (let ((dbus-show-dbus-errors t))
          (should
           (equal
            ;; We don't care the error message text.
            (butlast
             (should-error
              (dbus-get-property
               :session dbus--test-service dbus--test-path
               dbus--test-interface property1)))
            `(dbus-error ,dbus-error-unknown-property)))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

;; The following test is inspired by Bug#43146.
(ert-deftest dbus-test05-register-property-several-paths ()
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

(defun dbus-test-all (&optional interactive)
  "Run all tests for \\[dbus]."
  (interactive "p")
  (funcall (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
           "^dbus"))

(provide 'dbus-tests)
;;; dbus-tests.el ends here
