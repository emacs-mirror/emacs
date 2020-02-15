;;; files-x-tests.el --- tests for files-x.el.

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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
(put 'remote-shell-file-name 'safe-local-variable #'identity)
(put 'remote-shell-command-switch 'safe-local-variable #'identity)
(put 'remote-shell-interactive-switch 'safe-local-variable #'identity)
(put 'remote-shell-login-switch 'safe-local-variable #'identity)
(put 'remote-null-device 'safe-local-variable #'identity)

(defconst files-x-test--application '(:application 'my-application))
(defconst files-x-test--another-application
  '(:application 'another-application))
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
  (let (connection-local-profile-alist connection-local-criteria-alist)
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
      files-x-test--variables4))))

(ert-deftest files-x-test-connection-local-set-profiles ()
  "Test setting connection-local profiles."

  ;; Declare (CRITERIA PROFILES) objects.
  (let (connection-local-profile-alist connection-local-criteria-alist)
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
    (should-error (connection-local-set-profiles 'dummy))))

(ert-deftest files-x-test-hack-connection-local-variables-apply ()
  "Test setting connection-local variables."

  (let (connection-local-profile-alist connection-local-criteria-alist)

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
        ;; The proper variable value is set.
        (should
         (string-equal (symbol-value 'remote-shell-file-name) "/bin/ksh"))))

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
        (should-not (boundp 'remote-shell-file-name))))))

(defvar tramp-connection-local-default-profile)

(ert-deftest files-x-test-with-connection-local-variables ()
  "Test setting connection-local variables."

  (let (connection-local-profile-alist connection-local-criteria-alist)
    (connection-local-set-profile-variables
     'remote-bash files-x-test--variables1)
    (connection-local-set-profile-variables
     'remote-ksh files-x-test--variables2)
    (connection-local-set-profile-variables
     'remote-nullfile files-x-test--variables3)

    (connection-local-set-profiles
     nil 'remote-ksh 'remote-nullfile)

    (with-temp-buffer
      (let ((enable-connection-local-variables t))
        (hack-connection-local-variables-apply nil)

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
        ;; The proper variable values are set.
        (should
         (string-equal (symbol-value 'remote-shell-file-name) "/bin/ksh"))
        (should
         (string-equal (symbol-value 'remote-null-device) "/dev/null"))

	;; A candidate connection-local variable is not bound yet.
        (should-not (local-variable-p 'remote-shell-command-switch))))

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

	(with-connection-local-variables
	 ;; All connection-local variables are set.  They apply in
	 ;; reverse order in `connection-local-variables-alist'.
	 ;; Since we ha a remote default directory, Tramp's settings
	 ;; are appended as well.
         (should
          (equal
           connection-local-variables-alist
	   (append
	    (nreverse (copy-tree files-x-test--variables3))
	    (nreverse (copy-tree files-x-test--variables2))
            (nreverse (copy-tree tramp-connection-local-default-profile)))))
         ;; The variables exist also as local variables.
         (should (local-variable-p 'remote-shell-file-name))
         (should (local-variable-p 'remote-null-device))
         ;; The proper variable values are set.
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
	(should (string-equal (symbol-value 'remote-null-device) "null"))))))

(provide 'files-x-tests)
;;; files-x-tests.el ends here
