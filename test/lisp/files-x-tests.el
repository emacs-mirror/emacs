;;; files-x-tests.el --- tests for files-x.el.

;; Copyright (C) 2016 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'files-x)

(defvar files-x-test--criteria1 "my-user@my-remote-host")
(defvar files-x-test--criteria2
  (lambda (identification)
    (string-match "another-user@my-remote-host" identification)))
(defvar files-x-test--criteria3 nil)

(defvar files-x-test--variables1
  '((remote-shell-file-name . "/bin/bash")
    (remote-shell-command-switch . "-c")
    (remote-shell-interactive-switch . "-i")
    (remote-shell-login-switch . "-l")))
(defvar files-x-test--variables2
  '((remote-shell-file-name . "/bin/ksh")))
(defvar files-x-test--variables3
  '((remote-null-device . "/dev/null")))
(defvar files-x-test--variables4
  '((remote-null-device . "null")))

(ert-deftest files-x-test-connection-local-set-class-variables ()
  "Test setting connection-local class variables."

  ;; Declare (CLASS VARIABLES) objects.
  (let (connection-local-class-alist connection-local-criteria-alist)
    (connection-local-set-class-variables 'remote-bash files-x-test--variables1)
    (should
     (equal
      (connection-local-get-class-variables 'remote-bash)
      files-x-test--variables1))

    (connection-local-set-class-variables 'remote-ksh files-x-test--variables2)
    (should
     (equal
      (connection-local-get-class-variables 'remote-ksh)
      files-x-test--variables2))

    (connection-local-set-class-variables
     'remote-nullfile files-x-test--variables3)
    (should
     (equal
      (connection-local-get-class-variables 'remote-nullfile)
      files-x-test--variables3))

    ;; A redefinition overwrites existing values.
    (connection-local-set-class-variables
     'remote-nullfile files-x-test--variables4)
    (should
     (equal
      (connection-local-get-class-variables 'remote-nullfile)
      files-x-test--variables4))))

(ert-deftest files-x-test-connection-local-set-classes ()
  "Test setting connection-local classes."

  ;; Declare (CRITERIA CLASSES) objects.
  (let (connection-local-class-alist connection-local-criteria-alist)
    (connection-local-set-class-variables 'remote-bash files-x-test--variables1)
    (connection-local-set-class-variables 'remote-ksh files-x-test--variables2)
    (connection-local-set-class-variables
     'remote-nullfile files-x-test--variables3)

    (connection-local-set-classes
     files-x-test--criteria1 'remote-bash 'remote-ksh)
    (should
     (equal
      (connection-local-get-classes files-x-test--criteria1)
      '(remote-bash remote-ksh)))

    (connection-local-set-classes files-x-test--criteria2 'remote-ksh)
    (should
     (equal
      (connection-local-get-classes files-x-test--criteria2)
      '(remote-ksh)))
    ;; A further call adds classes.
    (connection-local-set-classes files-x-test--criteria2 'remote-nullfile)
    (should
     (equal
      (connection-local-get-classes files-x-test--criteria2)
      '(remote-ksh remote-nullfile)))
    ;; Adding existing classes doesn't matter.
    (connection-local-set-classes
     files-x-test--criteria2 'remote-bash 'remote-nullfile)
    (should
     (equal
      (connection-local-get-classes files-x-test--criteria2)
      '(remote-ksh remote-nullfile remote-bash)))

    ;; An empty variable list is accepted (but makes no sense).
    (connection-local-set-classes files-x-test--criteria3)
    (should-not (connection-local-get-classes files-x-test--criteria3))

    ;; Using a nil criteria also works.  Duplicate classes are trashed.
    (connection-local-set-classes
     files-x-test--criteria3 'remote-bash 'remote-ksh 'remote-ksh 'remote-bash)
    (should
     (equal
      (connection-local-get-classes files-x-test--criteria3)
      '(remote-bash remote-ksh)))

    ;; A criteria other than nil, regexp or lambda function is wrong.
    (should-error (connection-local-set-classes 'dummy))))

(ert-deftest files-x-test-hack-connection-local-variables-apply ()
  "Test setting connection-local variables."

  (let (connection-local-class-alist connection-local-criteria-alist)

    (connection-local-set-class-variables 'remote-bash files-x-test--variables1)
    (connection-local-set-class-variables 'remote-ksh files-x-test--variables2)
    (connection-local-set-class-variables
     'remote-nullfile files-x-test--variables3)

    (connection-local-set-classes
     files-x-test--criteria1 'remote-bash 'remote-ksh)
    (connection-local-set-classes
     files-x-test--criteria2 'remote-ksh 'remote-nullfile)

    ;; Apply the variables.
    (with-temp-buffer
      (let ((enable-connection-local-variables t)
            (default-directory "/sudo:my-user@my-remote-host:"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply)
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
      (let ((enable-connection-local-variables t)
            (default-directory "/ssh:another-user@my-remote-host:"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply)
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

    ;; The third test case.  Both `files-x-test--criteria1' and
    ;; `files-x-test--criteria3' apply, but there are no double
    ;; entries.
    (connection-local-set-classes
     files-x-test--criteria3 'remote-bash 'remote-ksh)
    (with-temp-buffer
      (let ((enable-connection-local-variables t)
            (default-directory "/sudo:my-user@my-remote-host:"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply)
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

    ;; When `enable-connection-local-variables' is nil, nothing happens.
    (with-temp-buffer
      (let ((enable-connection-local-variables nil)
            (default-directory "/ssh:another-user@my-remote-host:"))
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))
        (hack-connection-local-variables-apply)
        (should-not connection-local-variables-alist)
        (should-not (local-variable-p 'remote-shell-file-name))
        (should-not (boundp 'remote-shell-file-name))))))

(ert-deftest files-x-test-with-connection-local-classes ()
  "Test setting connection-local variables."

  (let (connection-local-class-alist connection-local-criteria-alist)
    (connection-local-set-class-variables 'remote-bash files-x-test--variables1)
    (connection-local-set-class-variables 'remote-ksh files-x-test--variables2)
    (connection-local-set-class-variables
     'remote-nullfile files-x-test--variables3)
    (connection-local-set-classes
     files-x-test--criteria3 'remote-ksh 'remote-nullfile)

    (with-temp-buffer
      (let ((enable-connection-local-variables t)
            (default-directory "/sudo:my-user@my-remote-host:"))
        (hack-connection-local-variables-apply)

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
        (should-not (local-variable-p 'remote-shell-command-switch))

	;; Use the macro.
        (with-connection-local-classes '(remote-bash remote-ksh)
          ;; All connection-local variables are set.  They apply in
          ;; reverse order in `connection-local-variables-alist'.
          ;; This variable keeps only the variables to be set inside
          ;; the macro.
          (should
           (equal connection-local-variables-alist
                  (nreverse (copy-tree files-x-test--variables1))))
          ;; The variables exist also as local variables.
          (should (local-variable-p 'remote-shell-file-name))
          (should (local-variable-p 'remote-shell-command-switch))
          ;; The proper variable values are set.  The settings from
          ;; `remote-bash' overwrite the same variables as in
          ;; `remote-ksh'.
          (should
           (string-equal (symbol-value 'remote-shell-file-name) "/bin/bash"))
          (should
           (string-equal (symbol-value 'remote-shell-command-switch) "-c")))

        ;; Everything is rewound.  The old variable values are reset.
        (should
         (equal connection-local-variables-alist
		(append
		 (nreverse (copy-tree files-x-test--variables3))
		 (nreverse (copy-tree files-x-test--variables2)))))
        ;; The variables exist also as local variables.
        (should (local-variable-p 'remote-shell-file-name))
        (should (local-variable-p 'remote-null-device))
        ;; The proper variable values are set.  The settings from
	;; `remote-ksh' are back.
        (should
         (string-equal (symbol-value 'remote-shell-file-name) "/bin/ksh"))
        (should
         (string-equal (symbol-value 'remote-null-device) "/dev/null"))

	;; The variable set temporarily is not unbound, again.
        (should-not (local-variable-p 'remote-shell-command-switch))))))

(provide 'files-x-tests)
;;; files-x-tests.el ends here
