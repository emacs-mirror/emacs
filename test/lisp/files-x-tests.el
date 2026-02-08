;;; files-x-tests.el --- tests for files-x.el.  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

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
(require 'files-x)
(require 'tramp-integration)

(defconst files-x-test--variables1
  '((remote-shell-file-name . "/bin/bash")
    (remote-shell-command-switch . "-c")
    (remote-shell-interactive-switch . "-i")
    (remote-shell-login-switch . "-l")))
(defconst files-x-test--variables2
  '((remote-shell-file-name . "/bin/ksh")))
(defconst files-x-test--variables3
  '((remote-null-device . "/dev/null")))
(defconst files-x-test--variables4
  '((remote-null-device . "null")))
(defconst files-x-test--variables5
  '((remote-lazy-var . nil)
    (remote-null-device . "/dev/null")))
(defvar remote-shell-file-name)
(defvar remote-null-device)
(defvar remote-lazy-var nil)
(put 'remote-shell-file-name 'safe-local-variable #'identity)
(put 'remote-shell-command-switch 'safe-local-variable #'identity)
(put 'remote-shell-interactive-switch 'safe-local-variable #'identity)
(put 'remote-shell-login-switch 'safe-local-variable #'identity)
(put 'remote-null-device 'safe-local-variable #'identity)

(defconst files-x-test--application '(:application my-application))
(defconst files-x-test--another-application
  '(:application another-application))
(defconst files-x-test--protocol '(:protocol "my-protocol"))
(defconst files-x-test--user '(:user "my-user"))
(defconst files-x-test--machine '(:machine "my-machine"))

(defvar files-x-test--criteria nil)
(defconst files-x-test--criteria1
  (append files-x-test--application files-x-test--protocol
          files-x-test--user files-x-test--machine))
(defconst files-x-test--criteria2
  (append files-x-test--another-application files-x-test--protocol
          files-x-test--user files-x-test--machine))

(ert-deftest files-x-test-connection-local-set-profile-variables ()
  "Test setting connection-local profile variables."

  ;; Declare (PROFILE VARIABLES) objects.
  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist)
        connection-local-profile-alist connection-local-criteria-alist)
    (connection-local-set-profile-variables
     'remote-bash files-x-test--variables1)
    (should
     (equal
      (connection-local-get-profile-variables 'remote-bash)
      files-x-test--variables1))

    (connection-local-set-profile-variables
     'remote-ksh files-x-test--variables2)
    (should
     (equal
      (connection-local-get-profile-variables 'remote-ksh)
      files-x-test--variables2))

    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables3)
    (should
     (equal
      (connection-local-get-profile-variables 'remote-nullfile)
      files-x-test--variables3))

    ;; A redefinition overwrites existing values.
    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables4)
    (should
     (equal
      (connection-local-get-profile-variables 'remote-nullfile)
      files-x-test--variables4))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(ert-deftest files-x-test-connection-local-update-profile-variables ()
  "Test updating connection-local profile variables."

  ;; Declare (PROFILE VARIABLES) objects.
  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist)
        connection-local-profile-alist connection-local-criteria-alist)
    (connection-local-set-profile-variables
     'remote-bash (copy-alist files-x-test--variables1))
    (should
     (equal
      (connection-local-get-profile-variables 'remote-bash)
      files-x-test--variables1))

    ;; Updating overwrites only the values specified in this call, but
    ;; retains all the other values from previous calls.
    (connection-local-update-profile-variables
     'remote-bash files-x-test--variables2)
    (should
     (equal
      (connection-local-get-profile-variables 'remote-bash)
      (cons (car files-x-test--variables2)
            (cdr files-x-test--variables1))))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(ert-deftest files-x-test-connection-local-set-profiles ()
  "Test setting connection-local profiles."

  ;; Declare (CRITERIA PROFILES) objects.
  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist)
        connection-local-profile-alist connection-local-criteria-alist)
    (connection-local-set-profile-variables
     'remote-bash files-x-test--variables1)
    (connection-local-set-profile-variables
     'remote-ksh files-x-test--variables2)
    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables3)

    ;; Use a criteria with all properties.
    (setq files-x-test--criteria
          (append files-x-test--application files-x-test--protocol
                  files-x-test--user files-x-test--machine))

    ;; An empty variable list is accepted (but makes no sense).
    (connection-local-set-profiles files-x-test--criteria)
    (should-not (connection-local-get-profiles files-x-test--criteria))

    ;; First test, all declared properties.
    (connection-local-set-profiles
     files-x-test--criteria 'remote-bash 'remote-ksh)
    (should
     (equal
      (connection-local-get-profiles files-x-test--criteria)
      '(remote-bash remote-ksh)))

    ;; Changing the order of properties doesn't matter.
    (setq files-x-test--criteria
          (append files-x-test--protocol files-x-test--application
                  files-x-test--machine files-x-test--user))
    (should
     (equal
      (connection-local-get-profiles files-x-test--criteria)
      '(remote-bash remote-ksh)))

    ;; A further call adds profiles.
    (connection-local-set-profiles files-x-test--criteria 'remote-nullfile)
    (should
     (equal
      (connection-local-get-profiles files-x-test--criteria)
      '(remote-bash remote-ksh remote-nullfile)))

    ;; Adding existing profiles doesn't matter.
    (connection-local-set-profiles
     files-x-test--criteria 'remote-bash 'remote-nullfile)
    (should
     (equal
      (connection-local-get-profiles files-x-test--criteria)
      '(remote-bash remote-ksh remote-nullfile)))

    ;; Use different properties.
    (dolist (criteria
             `(;; All properties.
               ,(append files-x-test--application files-x-test--protocol
                        files-x-test--user files-x-test--machine)
               ;; Without :application.
               ,(append files-x-test--protocol
                        files-x-test--user files-x-test--machine)
               ;; Without :protocol.
               ,(append files-x-test--application
                        files-x-test--user files-x-test--machine)
               ;; Without :user.
               ,(append files-x-test--application files-x-test--protocol
                        files-x-test--machine)
               ;; Without :machine.
               ,(append files-x-test--application files-x-test--protocol
                        files-x-test--user)
               ;; No property at all.
               nil))
      (should
       (equal
        (connection-local-get-profiles criteria)
        '(remote-bash remote-ksh remote-nullfile))))

    ;; Using a nil criteria also works.  Duplicate profiles are trashed.
    (connection-local-set-profiles
     nil 'remote-bash 'remote-ksh 'remote-ksh 'remote-bash)
    ;; This matches also the existing profiles from other criteria.
    (should
     (equal
      (connection-local-get-profiles nil)
      '(remote-bash remote-ksh remote-nullfile)))

    ;; A criteria other than plist is wrong.
    (should-error (connection-local-set-profiles 'dummy))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(ert-deftest files-x-test-hack-connection-local-variables-apply ()
  "Test setting connection-local variables."

  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist)
        connection-local-profile-alist connection-local-criteria-alist)

    (connection-local-set-profile-variables
     'remote-bash files-x-test--variables1)
    (connection-local-set-profile-variables
     'remote-ksh files-x-test--variables2)
    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables3)

    (connection-local-set-profiles
     files-x-test--criteria1 'remote-bash 'remote-ksh)
    (connection-local-set-profiles
     files-x-test--criteria2 'remote-ksh 'remote-nullfile)

    ;; Apply the variables.
    (with-temp-buffer
      (let ((enable-connection-local-variables t))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply files-x-test--criteria1)
        ;; All connection-local variables are set.  They apply in
        ;; reverse order in `connection-local-variables-alist'.  The
        ;; settings from `remote-ksh' are not contained, because they
        ;; declare same variables as in `remote-bash'.
        (should
         (equal connection-local-variables-alist
                (nreverse (copy-tree files-x-test--variables1))))
        ;; The variables exist also as local variables.
        (should (local-variable-p 'remote-shell-file-name))
        ;; The proper variable value is set.
        (should
         (string-equal (symbol-value 'remote-shell-file-name) "/bin/bash"))))

    ;; The second test case.
    (with-temp-buffer
      (let ((enable-connection-local-variables t))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply files-x-test--criteria2)
        ;; All connection-local variables are set.  They apply in
        ;; reverse order in `connection-local-variables-alist'.
        (should
         (equal connection-local-variables-alist
                (append
                 (nreverse (copy-tree files-x-test--variables3))
                 (nreverse (copy-tree files-x-test--variables2)))))
        ;; The variables exist also as local variables.
        (should (local-variable-p 'remote-shell-file-name))
        (should (local-variable-p 'remote-null-device))
        ;; The proper variable value is set.
        (should
         (string-equal (symbol-value 'remote-shell-file-name) "/bin/ksh"))
        (should
         (string-equal (symbol-value 'remote-null-device) "/dev/null"))))

    ;; The third test case.  Both criteria `files-x-test--criteria1'
    ;; and `files-x-test--criteria2' apply, but there are no double
    ;; entries.
    (connection-local-set-profiles
     nil 'remote-bash 'remote-ksh)
    (with-temp-buffer
      (let ((enable-connection-local-variables t))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply nil)
        ;; All connection-local variables are set.  They apply in
        ;; reverse order in `connection-local-variables-alist'.  The
        ;; settings from `remote-ksh' are not contained, because they
        ;; declare same variables as in `remote-bash'.
        (should
         (equal connection-local-variables-alist
                (append
                 (nreverse (copy-tree files-x-test--variables3))
                 (nreverse (copy-tree files-x-test--variables1)))))
        ;; The variables exist also as local variables.
        (should (local-variable-p 'remote-shell-file-name))
        ;; The proper variable value is set.
        (should
         (string-equal (symbol-value 'remote-shell-file-name) "/bin/bash"))))

    ;; When `enable-connection-local-variables' is nil, nothing happens.
    (with-temp-buffer
      (let ((enable-connection-local-variables nil))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply nil)
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(ert-deftest files-x-test-with-connection-local-variables ()
  "Test setting connection-local variables."

  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist))
    (connection-local-set-profile-variables
     'remote-bash files-x-test--variables1)
    (connection-local-set-profile-variables
     'remote-ksh files-x-test--variables2)
    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables3)

    (connection-local-set-profiles
     nil 'remote-ksh 'remote-nullfile)

    (with-temp-buffer
      ;; Use the macro.  We need a remote `default-directory'.
      (let ((enable-connection-local-variables t)
	    (default-directory "/method:host:")
	    (remote-null-device "null"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (local-variable-p 'remote-null-device))
        (should-not (boundp 'remote-shell-file-name))
        (should (string-equal (symbol-value 'remote-null-device) "null"))

        (connection-local-set-profiles
         files-x-test--application 'remote-bash)

	(with-connection-local-variables
	 ;; All connection-local variables are set.  They apply in
	 ;; reverse order in `connection-local-variables-alist'.
	 ;; Since we have a remote default directory, Tramp's settings
	 ;; are appended as well.
         (should
          (equal
           connection-local-variables-alist
	   (append
            (nreverse
             (copy-tree tramp-connection-local-default-shell-variables))
            (nreverse
             (copy-tree tramp-connection-local-default-system-variables))
	    (nreverse (copy-tree files-x-test--variables3))
	    (nreverse (copy-tree files-x-test--variables2)))))
         ;; The variables exist also as local variables.
         (should (local-variable-p 'remote-shell-file-name))
         (should (local-variable-p 'remote-null-device))
         ;; The proper variable values are set.
         (should
          (string-equal (symbol-value 'remote-shell-file-name) "/bin/ksh"))
         (should
          (string-equal (symbol-value 'remote-null-device) "/dev/null"))

         ;; Run `with-connection-local-application-variables' to use a
         ;; different application.
	 (with-connection-local-application-variables
             (cadr files-x-test--application)
         (should
          (equal
           connection-local-variables-alist
	   (append
	    (nreverse (copy-tree files-x-test--variables3))
	    (nreverse (copy-tree files-x-test--variables1)))))
           ;; The proper variable values are set.
           (should
            (string-equal (symbol-value 'remote-shell-file-name) "/bin/bash"))
           (should
            (string-equal (symbol-value 'remote-null-device) "/dev/null")))
         ;; The variable values are reset.
         (should
          (string-equal (symbol-value 'remote-shell-file-name) "/bin/ksh"))
         (should
          (string-equal (symbol-value 'remote-null-device) "/dev/null")))

	;; Everything is rewound.  The old variable values are reset.
	(should-not connection-local-variables-alist)
	;; The variables don't exist as local variables.
	(should-not (local-variable-p 'remote-shell-file-name))
	(should-not (local-variable-p 'remote-null-device))
	;; The variable values are reset.
	(should-not (boundp 'remote-shell-file-name))
	(should (string-equal (symbol-value 'remote-null-device) "null"))))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(defun files-x-test--get-lazy-var ()
  "Get the connection-local value of `remote-lazy-var'.
If it's not initialized yet, initialize it."
  (with-connection-local-application-variables
      (cadr files-x-test--application)
    (or remote-lazy-var
        (setq-connection-local remote-lazy-var
                               (or (file-remote-p default-directory 'host)
                                   "local")))))

(defun files-x-test--set-lazy-var (value)
  "Set the connection-local value of `remote-lazy-var'"
  (with-connection-local-application-variables
      (cadr files-x-test--application)
    (setq-connection-local remote-lazy-var value)))

(ert-deftest files-x-test-setq-connection-local ()
  "Test dynamically setting connection local variables."
  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist)
        connection-local-profile-alist connection-local-criteria-alist)
    (connection-local-set-profile-variables
     'remote-lazy files-x-test--variables5)
    (connection-local-set-profiles
     files-x-test--application
     'remote-lazy)

    ;; Test the initial local value.
    (should (equal (files-x-test--get-lazy-var) "local"))

    ;; Set the local value and make sure it retains the value we set.
    (should (equal (files-x-test--set-lazy-var "here") "here"))
    (should (equal (files-x-test--get-lazy-var) "here"))

    (let ((default-directory "/method:host:"))
      ;; Test the initial remote value.
      (should (equal (files-x-test--get-lazy-var) "host"))

      ;; Set the remote value and make sure it retains the value we set.
      (should (equal (files-x-test--set-lazy-var "there") "there"))
      (should (equal (files-x-test--get-lazy-var) "there"))
      ;; Set another connection-local variable.
      (with-connection-local-application-variables
          (cadr files-x-test--application)
        (setq-connection-local remote-null-device "null")))

    ;; Make sure we get the local value we set above.
    (should (equal (files-x-test--get-lazy-var) "here"))
    (should-not (boundp 'remote-null-device))

    ;; Make sure we get the remote values we set above.
    (let ((default-directory "/method:host:"))
      (should (equal (files-x-test--get-lazy-var) "there"))
      (with-connection-local-application-variables
          (cadr files-x-test--application)
        (should (equal remote-null-device "null"))))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(ert-deftest files-x-test-connection-local-value ()
  "Test getting connection-local values."

  (let ((clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist))
    (connection-local-set-profile-variables
     'remote-bash files-x-test--variables1)
    (connection-local-set-profile-variables
     'remote-ksh files-x-test--variables2)
    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables3)

    (connection-local-set-profiles
     nil 'remote-ksh 'remote-nullfile)

    (connection-local-set-profile-variables
     'remote-lazy files-x-test--variables5)
    (connection-local-set-profiles
     files-x-test--application 'remote-lazy 'remote-bash)

    (with-temp-buffer
      ;; We need a remote `default-directory'.
      (let ((enable-connection-local-variables t)
	    (default-directory "/method:host:")
	    (remote-null-device "null"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (local-variable-p 'remote-null-device))
        (should-not (boundp 'remote-shell-file-name))
        (should (string-equal (symbol-value 'remote-null-device) "null"))

        ;; The proper variable values are set.
        (should (connection-local-p remote-shell-file-name))
        (should
         (string-equal
          (connection-local-value remote-shell-file-name) "/bin/ksh"))
        (should (connection-local-p remote-null-device))
        (should
         (string-equal
          (connection-local-value remote-null-device) "/dev/null"))
        (should-not (connection-local-p remote-lazy-var))

        ;; Run with a different application.
        (should
         (connection-local-p
          remote-shell-file-name (cadr files-x-test--application)))
        (should
         (string-equal
          (connection-local-value
           remote-shell-file-name (cadr files-x-test--application))
          "/bin/bash"))
        (should
         (connection-local-p
          remote-null-device (cadr files-x-test--application)))
        (should
         (string-equal
          (connection-local-value
           remote-null-device (cadr files-x-test--application))
          "/dev/null"))
        (should
         (connection-local-p
          remote-lazy-var (cadr files-x-test--application)))

        ;; The previous bindings haven't changed.
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (local-variable-p 'remote-null-device))
        (should-not (boundp 'remote-shell-file-name))
        (should (string-equal (symbol-value 'remote-null-device) "null"))))

    ;; `connection-local-value' and `connection-local-p' care about a
    ;; local default directory.
    (with-temp-buffer
      (let ((enable-connection-local-variables t)
	    (default-directory temporary-file-directory)
	    (remote-null-device "null"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (local-variable-p 'remote-null-device))
        (should-not (boundp 'remote-shell-file-name))
        (should (string-equal (symbol-value 'remote-null-device) "null"))

        ;; The recent variable values are used.
        (should-not (connection-local-p remote-shell-file-name))
        ;; `remote-shell-file-name' is not defined, so we get an error.
        (should-error
         (connection-local-value remote-shell-file-name) :type 'void-variable)
        (should-not (connection-local-p remote-null-device))
        (should
         (string-equal
          (connection-local-value remote-null-device) remote-null-device))
        (should-not (connection-local-p remote-lazy-var))

        ;; Run with a different application.
        (should-not
         (connection-local-p
          remote-shell-file-name (cadr files-x-test--application)))
        ;; `remote-shell-file-name' is not defined, so we get an error.
        (should-error
         (connection-local-value
          remote-shell-file-name (cadr files-x-test--application))
         :type 'void-variable)
        (should-not
         (connection-local-p
          remote-null-device (cadr files-x-test--application)))
        (should
         (string-equal
          (connection-local-value
           remote-null-device (cadr files-x-test--application))
          remote-null-device))
        (should-not
         (connection-local-p remote-lazy-var (cadr files-x-test--application)))))

    ;; Cleanup.
    (custom-set-variables
     `(connection-local-profile-alist ',clpa now)
     `(connection-local-criteria-alist ',clca now))))

(provide 'files-x-tests)
;;; files-x-tests.el ends here
