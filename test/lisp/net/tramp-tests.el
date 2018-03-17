;;; tramp-tests.el --- Tests of remote file access  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

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

;;; Commentary:

;; The tests require a recent ert.el from Emacs 24.4.

;; Some of the tests require access to a remote host files.  Since
;; this could be problematic, a mock-up connection method "mock" is
;; used.  Emulating a remote connection, it simply calls "sh -i".
;; Tramp's file name handlers still run, so this test is sufficient
;; except for connection establishing.

;; If you want to test a real Tramp connection, set
;; $REMOTE_TEMPORARY_FILE_DIRECTORY to a suitable value in order to
;; overwrite the default value.  If you want to skip tests accessing a
;; remote host, set this environment variable to "/dev/null" or
;; whatever is appropriate on your system.

;; For slow remote connections, `tramp-test41-asynchronous-requests'
;; might be too heavy.  Setting $REMOTE_PARALLEL_PROCESSES to a proper
;; value less than 10 could help.

;; A whole test run can be performed calling the command `tramp-test-all'.

;;; Code:

(require 'dired)
(require 'ert)
(require 'ert-x)
(require 'tramp)
(require 'vc)
(require 'vc-bzr)
(require 'vc-git)
(require 'vc-hg)

(declare-function tramp-find-executable "tramp-sh")
(declare-function tramp-get-remote-path "tramp-sh")
(declare-function tramp-get-remote-stat "tramp-sh")
(declare-function tramp-get-remote-perl "tramp-sh")
(defvar auto-save-file-name-transforms)
(defvar tramp-copy-size-limit)
(defvar tramp-persistency-file-name)
(defvar tramp-remote-process-environment)

;; Beautify batch mode.
(when noninteractive
  ;; Suppress nasty messages.
  (fset 'shell-command-sentinel 'ignore)
  ;; We do not want to be interrupted.
  (eval-after-load 'tramp-gvfs
    '(fset 'tramp-gvfs-handler-askquestion
	   (lambda (_message _choices) '(t nil 0)))))

;; There is no default value on w32 systems, which could work out of the box.
(defconst tramp-test-temporary-file-directory
  (cond
   ((getenv "REMOTE_TEMPORARY_FILE_DIRECTORY"))
   ((eq system-type 'windows-nt) null-device)
   (t (add-to-list
       'tramp-methods
       '("mock"
	 (tramp-login-program        "sh")
	 (tramp-login-args           (("-i")))
	 (tramp-remote-shell         "/bin/sh")
	 (tramp-remote-shell-args    ("-c"))
	 (tramp-connection-timeout   10)))
      (add-to-list
       'tramp-default-host-alist
       `("\\`mock\\'" nil ,(system-name)))
      ;; Emacs' Makefile sets $HOME to a nonexistent value.  Needed in
      ;; batch mode only, therefore.
      (unless (and (null noninteractive) (file-directory-p "~/"))
        (setenv "HOME" temporary-file-directory))
      (format "/mock::%s" temporary-file-directory)))
  "Temporary directory for Tramp tests.")

(setq password-cache-expiry nil
      tramp-verbose 0
      tramp-cache-read-persistent-data t ;; For auth-sources.
      tramp-copy-size-limit nil
      tramp-message-show-message nil
      tramp-persistency-file-name nil)

;; This should happen on hydra only.
(when (getenv "EMACS_HYDRA_CI")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defvar tramp--test-enabled-checked nil
  "Cached result of `tramp--test-enabled'.
If the function did run, the value is a cons cell, the `cdr'
being the result.")

(defun tramp--test-enabled ()
  "Whether remote file access is enabled."
  (unless (consp tramp--test-enabled-checked)
    (setq
     tramp--test-enabled-checked
     (cons
      t (ignore-errors
	  (and
	   (file-remote-p tramp-test-temporary-file-directory)
	   (file-directory-p tramp-test-temporary-file-directory)
	   (file-writable-p tramp-test-temporary-file-directory))))))

  (when (cdr tramp--test-enabled-checked)
    ;; Cleanup connection.
    (ignore-errors
      (tramp-cleanup-connection
       (tramp-dissect-file-name tramp-test-temporary-file-directory)
       nil 'keep-password)))

  ;; Return result.
  (cdr tramp--test-enabled-checked))

(defsubst tramp--test-expensive-test ()
  "Whether expensive tests are run."
  (ert-select-tests
   (ert--stats-selector ert--current-run-stats)
   (list (make-ert-test :body nil :tags '(:expensive-test)))))

(defun tramp--test-make-temp-name (&optional local quoted)
  "Return a temporary file name for test.
If LOCAL is non-nil, a local file name is returned.
If QUOTED is non-nil, the local part of the file name is quoted.
The temporary file is not created."
  (funcall
   (if quoted 'tramp-compat-file-name-quote 'identity)
   (expand-file-name
    (make-temp-name "tramp-test")
    (if local temporary-file-directory tramp-test-temporary-file-directory))))

;; Don't print messages in nested `tramp--test-instrument-test-case' calls.
(defvar tramp--test-instrument-test-case-p nil
  "Whether `tramp--test-instrument-test-case' run.
This shall used dynamically bound only.")

(defmacro tramp--test-instrument-test-case (verbose &rest body)
  "Run BODY with `tramp-verbose' equal VERBOSE.
Print the content of the Tramp debug buffer, if BODY does not
eval properly in `should' or `should-not'.  `should-error' is not
handled properly.  BODY shall not contain a timeout."
  (declare (indent 1) (debug (natnump body)))
  `(let ((tramp-verbose (max (or ,verbose 0) (or tramp-verbose 0)))
	 (tramp-message-show-message t)
	 (tramp-debug-on-error t)
	 (debug-ignored-errors
	  (cons "^make-symbolic-link not supported$" debug-ignored-errors))
	 inhibit-message)
     (unwind-protect
	 (let ((tramp--test-instrument-test-case-p t)) ,@body)
       ;; Unwind forms.
       (when (and (null tramp--test-instrument-test-case-p) (> tramp-verbose 3))
	 (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
	   (with-current-buffer (tramp-get-connection-buffer v)
	     (message "%s" (buffer-string)))
	   (with-current-buffer (tramp-get-debug-buffer v)
	     (message "%s" (buffer-string))))))))

(defsubst tramp--test-message (fmt-string &rest arguments)
  "Emit a message into ERT *Messages*."
  (tramp--test-instrument-test-case 0
    (apply
     'tramp-message
     (tramp-dissect-file-name tramp-test-temporary-file-directory) 0
     fmt-string arguments)))

(defsubst tramp--test-backtrace ()
  "Dump a backtrace into ERT *Messages*."
  (tramp--test-instrument-test-case 10
    (tramp-backtrace
     (tramp-dissect-file-name tramp-test-temporary-file-directory))))

(defmacro tramp--test-print-duration (message &rest body)
  "Run BODY and print a message with duration, prompted by MESSAGE."
  (declare (indent 1) (debug (stringp body)))
  `(let ((start (current-time)))
     (unwind-protect
	 (progn ,@body)
       (tramp--test-message
	"%s %f sec"
	,message (float-time (time-subtract (current-time) start))))))

(ert-deftest tramp-test00-availability ()
  "Test availability of Tramp functions."
  :expected-result (if (tramp--test-enabled) :passed :failed)
  (tramp--test-message
   "Remote directory: `%s'" tramp-test-temporary-file-directory)
  (should (ignore-errors
	    (and
	     (file-remote-p tramp-test-temporary-file-directory)
	     (file-directory-p tramp-test-temporary-file-directory)
	     (file-writable-p tramp-test-temporary-file-directory)))))

(ert-deftest tramp-test01-file-name-syntax ()
  "Check remote file name syntax."
  ;; Simple cases.
  (should (tramp-tramp-file-p "/method::"))
  (should (tramp-tramp-file-p "/method:host:"))
  (should (tramp-tramp-file-p "/method:user@:"))
  (should (tramp-tramp-file-p "/method:user@host:"))
  (should (tramp-tramp-file-p "/method:user@email@host:"))

  ;; Using a port.
  (should (tramp-tramp-file-p "/method:host#1234:"))
  (should (tramp-tramp-file-p "/method:user@host#1234:"))

  ;; Using an IPv4 address.
  (should (tramp-tramp-file-p "/method:1.2.3.4:"))
  (should (tramp-tramp-file-p "/method:user@1.2.3.4:"))

  ;; Using an IPv6 address.
  (should (tramp-tramp-file-p "/method:[::1]:"))
  (should (tramp-tramp-file-p "/method:user@[::1]:"))

  ;; Local file name part.
  (should (tramp-tramp-file-p "/method:::"))
  (should (tramp-tramp-file-p "/method::/:"))
  (should (tramp-tramp-file-p "/method::/path/to/file"))
  (should (tramp-tramp-file-p "/method::/:/path/to/file"))
  (should (tramp-tramp-file-p "/method::file"))
  (should (tramp-tramp-file-p "/method::/:file"))

  ;; Multihop.
  (should (tramp-tramp-file-p "/method1:|method2::"))
  (should (tramp-tramp-file-p "/method1:host1|method2:host2:"))
  (should (tramp-tramp-file-p "/method1:user1@host1|method2:user2@host2:"))
  (should (tramp-tramp-file-p
	   "/method1:user1@host1|method2:user2@host2|method3:user3@host3:"))

  ;; No strings.
  (should-not (tramp-tramp-file-p nil))
  (should-not (tramp-tramp-file-p 'symbol))
  ;; Ange-ftp syntax.
  (should-not (tramp-tramp-file-p "/host:"))
  (should-not (tramp-tramp-file-p "/user@host:"))
  (should-not (tramp-tramp-file-p "/1.2.3.4:"))
  (should-not (tramp-tramp-file-p "/[]:"))
  (should-not (tramp-tramp-file-p "/[::1]:"))
  (should-not (tramp-tramp-file-p "/host:/:"))
  (should-not (tramp-tramp-file-p "/host1|host2:"))
  (should-not (tramp-tramp-file-p "/user1@host1|user2@host2:"))
  ;; Quote with "/:" suppresses file name handlers.
  (should-not (tramp-tramp-file-p "/::"))
  (should-not (tramp-tramp-file-p "/:@:"))
  (should-not (tramp-tramp-file-p "/:[]:"))
  ;; Methods shall be at least two characters on MS Windows, except
  ;; the default method.
  (let ((system-type 'windows-nt))
    (should-not (tramp-tramp-file-p "/c:/path/to/file"))
    (should-not (tramp-tramp-file-p "/c::/path/to/file"))
    (should (tramp-tramp-file-p "/-::/path/to/file")))
  (let ((system-type 'gnu/linux))
    (should (tramp-tramp-file-p "/-:h:/path/to/file"))
    (should (tramp-tramp-file-p "/m::/path/to/file"))))

(ert-deftest tramp-test01-file-name-syntax-simplified ()
  "Check simplified file name syntax."
  :tags '(:expensive-test)
  (let ((syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'simplified)
	  ;; Simple cases.
	  (should (tramp-tramp-file-p "/host:"))
	  (should (tramp-tramp-file-p "/user@:"))
	  (should (tramp-tramp-file-p "/user@host:"))
	  (should (tramp-tramp-file-p "/user@email@host:"))

	  ;; Using a port.
	  (should (tramp-tramp-file-p "/host#1234:"))
	  (should (tramp-tramp-file-p "/user@host#1234:"))

	  ;; Using an IPv4 address.
	  (should (tramp-tramp-file-p "/1.2.3.4:"))
	  (should (tramp-tramp-file-p "/user@1.2.3.4:"))

	  ;; Using an IPv6 address.
	  (should (tramp-tramp-file-p "/[::1]:"))
	  (should (tramp-tramp-file-p "/user@[::1]:"))

	  ;; Local file name part.
	  (should (tramp-tramp-file-p "/host::"))
	  (should (tramp-tramp-file-p "/host:/:"))
	  (should (tramp-tramp-file-p "/host:/path/to/file"))
	  (should (tramp-tramp-file-p "/host:/:/path/to/file"))
	  (should (tramp-tramp-file-p "/host:file"))
	  (should (tramp-tramp-file-p "/host:/:file"))

	  ;; Multihop.
	  (should (tramp-tramp-file-p "/host1|host2:"))
	  (should (tramp-tramp-file-p "/user1@host1|user2@host2:"))
	  (should (tramp-tramp-file-p "/user1@host1|user2@host2|user3@host3:"))

	  ;; No strings.
	  (should-not (tramp-tramp-file-p nil))
	  (should-not (tramp-tramp-file-p 'symbol))
	  ;; Quote with "/:" suppresses file name handlers.
	  (should-not (tramp-tramp-file-p "/::"))
	  (should-not (tramp-tramp-file-p "/:@:"))
	  (should-not (tramp-tramp-file-p "/:[]:")))

      ;; Exit.
      (tramp-change-syntax syntax))))

(ert-deftest tramp-test01-file-name-syntax-separate ()
  "Check separate file name syntax."
  :tags '(:expensive-test)
  (let ((syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'separate)
	  ;; Simple cases.
	  (should (tramp-tramp-file-p "/[method/]"))
	  (should (tramp-tramp-file-p "/[method/host]"))
	  (should (tramp-tramp-file-p "/[method/user@]"))
	  (should (tramp-tramp-file-p "/[method/user@host]"))
	  (should (tramp-tramp-file-p "/[method/user@email@host]"))

	  ;; Using a port.
	  (should (tramp-tramp-file-p "/[method/host#1234]"))
	  (should (tramp-tramp-file-p "/[method/user@host#1234]"))

	  ;; Using an IPv4 address.
	  (should (tramp-tramp-file-p "/[method/1.2.3.4]"))
	  (should (tramp-tramp-file-p "/[method/user@1.2.3.4]"))

	  ;; Using an IPv6 address.
	  (should (tramp-tramp-file-p "/[method/::1]"))
	  (should (tramp-tramp-file-p "/[method/user@::1]"))

	  ;; Local file name part.
	  (should (tramp-tramp-file-p "/[method/]"))
	  (should (tramp-tramp-file-p "/[method/]/:"))
	  (should (tramp-tramp-file-p "/[method/]/path/to/file"))
	  (should (tramp-tramp-file-p "/[method/]/:/path/to/file"))
	  (should (tramp-tramp-file-p "/[method/]file"))
	  (should (tramp-tramp-file-p "/[method/]/:file"))

	  ;; Multihop.
	  (should (tramp-tramp-file-p "/[method1/|method2/]"))
	  (should (tramp-tramp-file-p "/[method1/host1|method2/host2]"))
	  (should
	   (tramp-tramp-file-p
	    "/[method1/user1@host1|method2/user2@host2]"))
	  (should
	   (tramp-tramp-file-p
	    "/[method1/user1@host1|method2/user2@host2|method3/user3@host3]"))

	  ;; No strings.
	  (should-not (tramp-tramp-file-p nil))
	  (should-not (tramp-tramp-file-p 'symbol))
	  ;; Ange-ftp syntax.
	  (should-not (tramp-tramp-file-p "/host:"))
	  (should-not (tramp-tramp-file-p "/user@host:"))
	  (should-not (tramp-tramp-file-p "/1.2.3.4:"))
	  (should-not (tramp-tramp-file-p "/host:/:"))
	  (should-not (tramp-tramp-file-p "/host1|host2:"))
	  (should-not (tramp-tramp-file-p "/user1@host1|user2@host2:"))
	  ;; Quote with "/:" suppresses file name handlers.
	  (should-not (tramp-tramp-file-p "/:[]")))

      ;; Exit.
      (tramp-change-syntax syntax))))

(ert-deftest tramp-test02-file-name-dissect ()
  "Check remote file name components."
  (let ((tramp-default-method "default-method")
	(tramp-default-user "default-user")
	(tramp-default-host "default-host")
	tramp-default-method-alist
	tramp-default-user-alist
	tramp-default-host-alist)
    ;; Expand `tramp-default-user' and `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/method::")
	     (format "/%s:%s@%s:" "method" "default-user" "default-host")))
    (should (string-equal (file-remote-p "/method::" 'method) "method"))
    (should (string-equal (file-remote-p "/method::" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method::" 'host) "default-host"))
    (should (string-equal (file-remote-p "/method::" 'localname) ""))
    (should (string-equal (file-remote-p "/method::" 'hop) nil))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/-:host:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "host")))
    (should (string-equal (file-remote-p "/-:host:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:host:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/-:host:" 'host) "host"))
    (should (string-equal (file-remote-p "/-:host:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:host:" 'hop) nil))

    ;; Expand `tramp-default-method' and `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/-:user@:")
	     (format "/%s:%s@%s:" "default-method" "user" "default-host")))
    (should (string-equal (file-remote-p "/-:user@:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:user@:" 'user) "user"))
    (should (string-equal (file-remote-p "/-:user@:" 'host) "default-host"))
    (should (string-equal (file-remote-p "/-:user@:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:user@:" 'hop) nil))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/-:user@host:")
	     (format "/%s:%s@%s:" "default-method" "user" "host")))
    (should (string-equal
	     (file-remote-p "/-:user@host:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:user@host:" 'user) "user"))
    (should (string-equal (file-remote-p "/-:user@host:" 'host) "host"))
    (should (string-equal (file-remote-p "/-:user@host:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:user@host:" 'hop) nil))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:host:")
	     (format "/%s:%s@%s:" "method" "default-user" "host")))
    (should (string-equal (file-remote-p "/method:host:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:host:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method:host:" 'host) "host"))
    (should (string-equal (file-remote-p "/method:host:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:host:" 'hop) nil))

    ;; Expand `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/method:user@:")
	     (format "/%s:%s@%s:" "method" "user" "default-host")))
    (should (string-equal (file-remote-p "/method:user@:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:user@:" 'user) "user"))
    (should (string-equal (file-remote-p "/method:user@:" 'host)
			  "default-host"))
    (should (string-equal (file-remote-p "/method:user@:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:user@:" 'hop) nil))

    ;; No expansion.
    (should (string-equal
	     (file-remote-p "/method:user@host:")
	     (format "/%s:%s@%s:" "method" "user" "host")))
    (should (string-equal
	     (file-remote-p "/method:user@host:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:user@host:" 'user) "user"))
    (should (string-equal (file-remote-p "/method:user@host:" 'host) "host"))
    (should (string-equal (file-remote-p "/method:user@host:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:user@host:" 'hop) nil))

    ;; No expansion.
    (should (string-equal
	     (file-remote-p "/method:user@email@host:")
	     (format "/%s:%s@%s:" "method" "user@email" "host")))
    (should (string-equal
	     (file-remote-p "/method:user@email@host:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:user@email@host:" 'user) "user@email"))
    (should (string-equal
	     (file-remote-p "/method:user@email@host:" 'host) "host"))
    (should (string-equal
	     (file-remote-p "/method:user@email@host:" 'localname) ""))
    (should (string-equal
	     (file-remote-p "/method:user@email@host:" 'hop) nil))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/-:host#1234:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "host#1234")))
    (should (string-equal
	     (file-remote-p "/-:host#1234:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:host#1234:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/-:host#1234:" 'host) "host#1234"))
    (should (string-equal (file-remote-p "/-:host#1234:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:host#1234:" 'hop) nil))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/-:user@host#1234:")
	     (format "/%s:%s@%s:" "default-method" "user" "host#1234")))
    (should (string-equal
	     (file-remote-p "/-:user@host#1234:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:user@host#1234:" 'user) "user"))
    (should (string-equal (file-remote-p "/-:user@host#1234:" 'host) "host#1234"))
    (should (string-equal (file-remote-p "/-:user@host#1234:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:user@host#1234:" 'hop) nil))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:host#1234:")
	     (format "/%s:%s@%s:" "method" "default-user" "host#1234")))
    (should (string-equal
	     (file-remote-p "/method:host#1234:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:host#1234:" 'user) "default-user"))
    (should (string-equal
	     (file-remote-p "/method:host#1234:" 'host) "host#1234"))
    (should (string-equal (file-remote-p "/method:host#1234:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:host#1234:" 'hop) nil))

    ;; No expansion.
    (should (string-equal
	     (file-remote-p "/method:user@host#1234:")
	     (format "/%s:%s@%s:" "method" "user" "host#1234")))
    (should (string-equal
	     (file-remote-p "/method:user@host#1234:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:user@host#1234:" 'user) "user"))
    (should (string-equal
	     (file-remote-p "/method:user@host#1234:" 'host) "host#1234"))
    (should (string-equal
	     (file-remote-p "/method:user@host#1234:" 'localname) ""))
    (should (string-equal
	     (file-remote-p "/method:user@host#1234:" 'hop) nil))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/-:1.2.3.4:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "1.2.3.4")))
    (should (string-equal (file-remote-p "/-:1.2.3.4:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:1.2.3.4:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/-:1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal (file-remote-p "/-:1.2.3.4:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:1.2.3.4:" 'hop) nil))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/-:user@1.2.3.4:")
	     (format "/%s:%s@%s:" "default-method" "user" "1.2.3.4")))
    (should (string-equal
	     (file-remote-p "/-:user@1.2.3.4:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:user@1.2.3.4:" 'user) "user"))
    (should (string-equal (file-remote-p "/-:user@1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal (file-remote-p "/-:user@1.2.3.4:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:user@1.2.3.4:" 'hop) nil))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:1.2.3.4:")
	     (format "/%s:%s@%s:" "method" "default-user" "1.2.3.4")))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:1.2.3.4:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'hop) nil))

    ;; No expansion.
    (should (string-equal
	     (file-remote-p "/method:user@1.2.3.4:")
	     (format "/%s:%s@%s:" "method" "user" "1.2.3.4")))
    (should (string-equal
	     (file-remote-p "/method:user@1.2.3.4:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:user@1.2.3.4:" 'user) "user"))
    (should (string-equal
	     (file-remote-p "/method:user@1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal
	     (file-remote-p "/method:user@1.2.3.4:" 'localname) ""))
    (should (string-equal
	     (file-remote-p "/method:user@1.2.3.4:" 'hop) nil))

    ;; Expand `tramp-default-method', `tramp-default-user' and
    ;; `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/-:[]:")
	     (format
	      "/%s:%s@%s:" "default-method" "default-user" "default-host")))
    (should (string-equal (file-remote-p "/-:[]:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:[]:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/-:[]:" 'host) "default-host"))
    (should (string-equal (file-remote-p "/-:[]:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:[]:" 'hop) nil))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (let ((tramp-default-host "::1"))
      (should (string-equal
	       (file-remote-p "/-:[]:")
	       (format "/%s:%s@%s:" "default-method" "default-user" "[::1]")))
      (should (string-equal (file-remote-p "/-:[]:" 'method) "default-method"))
      (should (string-equal (file-remote-p "/-:[]:" 'user) "default-user"))
      (should (string-equal (file-remote-p "/-:[]:" 'host) "::1"))
      (should (string-equal (file-remote-p "/-:[]:" 'localname) ""))
      (should (string-equal (file-remote-p "/-:[]:" 'hop) nil)))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/-:[::1]:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "[::1]")))
    (should (string-equal (file-remote-p "/-:[::1]:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:[::1]:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/-:[::1]:" 'host) "::1"))
    (should (string-equal (file-remote-p "/-:[::1]:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:[::1]:" 'hop) nil))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/-:user@[::1]:")
	     (format "/%s:%s@%s:" "default-method" "user" "[::1]")))
    (should (string-equal
	     (file-remote-p "/-:user@[::1]:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/-:user@[::1]:" 'user) "user"))
    (should (string-equal (file-remote-p "/-:user@[::1]:" 'host) "::1"))
    (should (string-equal (file-remote-p "/-:user@[::1]:" 'localname) ""))
    (should (string-equal (file-remote-p "/-:user@[::1]:" 'hop) nil))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:[::1]:")
	     (format "/%s:%s@%s:" "method" "default-user" "[::1]")))
    (should (string-equal (file-remote-p "/method:[::1]:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:[::1]:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method:[::1]:" 'host) "::1"))
    (should (string-equal (file-remote-p "/method:[::1]:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:[::1]:" 'hop) nil))

    ;; No expansion.
    (should (string-equal
	     (file-remote-p "/method:user@[::1]:")
	     (format "/%s:%s@%s:" "method" "user" "[::1]")))
    (should (string-equal
	     (file-remote-p "/method:user@[::1]:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:user@[::1]:" 'user) "user"))
    (should (string-equal (file-remote-p "/method:user@[::1]:" 'host) "::1"))
    (should (string-equal
	     (file-remote-p "/method:user@[::1]:" 'localname) ""))
    (should (string-equal (file-remote-p "/method:user@[::1]:" 'hop) nil))

    ;; Local file name part.
    (should (string-equal (file-remote-p "/-:host:/:" 'localname) "/:"))
    (should (string-equal (file-remote-p "/method:::" 'localname) ":"))
    (should (string-equal (file-remote-p "/method:: " 'localname) " "))
    (should (string-equal (file-remote-p "/method::file" 'localname) "file"))
    (should (string-equal
	     (file-remote-p "/method::/path/to/file" 'localname)
	     "/path/to/file"))

    ;; Multihop.
    (should
     (string-equal
      (file-remote-p "/method1:user1@host1|method2:user2@host2:/path/to/file")
      (format "/%s:%s@%s|%s:%s@%s:"
	      "method1" "user1" "host1" "method2" "user2" "host2")))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2:/path/to/file" 'method)
      "method2"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2:/path/to/file" 'user)
      "user2"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2:/path/to/file" 'host)
      "host2"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2:/path/to/file" 'localname)
      "/path/to/file"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2:/path/to/file" 'hop)
      (format "%s:%s@%s|"
	      "method1" "user1" "host1")))

    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@host1"
	"|method2:user2@host2"
	"|method3:user3@host3:/path/to/file"))
      (format "/%s:%s@%s|%s:%s@%s|%s:%s@%s:"
	      "method1" "user1" "host1"
	      "method2" "user2" "host2"
	      "method3" "user3" "host3")))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@host1"
	"|method2:user2@host2"
	"|method3:user3@host3:/path/to/file")
       'method)
      "method3"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@host1"
	"|method2:user2@host2"
	"|method3:user3@host3:/path/to/file")
       'user)
      "user3"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@host1"
	"|method2:user2@host2"
	"|method3:user3@host3:/path/to/file")
       'host)
      "host3"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@host1"
	"|method2:user2@host2"
	"|method3:user3@host3:/path/to/file")
       'localname)
      "/path/to/file"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@host1"
	"|method2:user2@host2"
	"|method3:user3@host3:/path/to/file")
       'hop)
      (format "%s:%s@%s|%s:%s@%s|"
	      "method1" "user1" "host1" "method2" "user2" "host2")))

    ;; Expand `tramp-default-method-alist'.
    (add-to-list 'tramp-default-method-alist '("host1" "user1" "method1"))
    (add-to-list 'tramp-default-method-alist '("host2" "user2" "method2"))
    (add-to-list 'tramp-default-method-alist '("host3" "user3" "method3"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/-:user1@host1"
	"|-:user2@host2"
	"|-:user3@host3:/path/to/file"))
      (format "/%s:%s@%s|%s:%s@%s|%s:%s@%s:"
	      "-" "user1" "host1"
	      "-" "user2" "host2"
	      "method3" "user3" "host3")))

    ;; Expand `tramp-default-user-alist'.
    (add-to-list 'tramp-default-user-alist '("method1" "host1" "user1"))
    (add-to-list 'tramp-default-user-alist '("method2" "host2" "user2"))
    (add-to-list 'tramp-default-user-alist '("method3" "host3" "user3"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:host1"
	"|method2:host2"
	"|method3:host3:/path/to/file"))
      (format "/%s:%s|%s:%s|%s:%s@%s:"
	      "method1" "host1"
	      "method2" "host2"
	      "method3" "user3" "host3")))

    ;; Expand `tramp-default-host-alist'.
    (add-to-list 'tramp-default-host-alist '("method1" "user1" "host1"))
    (add-to-list 'tramp-default-host-alist '("method2" "user2" "host2"))
    (add-to-list 'tramp-default-host-alist '("method3" "user3" "host3"))
    (should
     (string-equal
      (file-remote-p
       (concat
	"/method1:user1@"
	"|method2:user2@"
	"|method3:user3@:/path/to/file"))
      (format "/%s:%s@|%s:%s@|%s:%s@%s:"
	      "method1" "user1"
	      "method2" "user2"
	      "method3" "user3" "host3")))))

(ert-deftest tramp-test02-file-name-dissect-simplified ()
  "Check simplified file name components."
  :tags '(:expensive-test)
  (let ((tramp-default-method "default-method")
	(tramp-default-user "default-user")
	(tramp-default-host "default-host")
	tramp-default-user-alist
	tramp-default-host-alist
	(syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'simplified)
	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/host:")
		   (format "/%s@%s:" "default-user" "host")))
	  (should (string-equal
		   (file-remote-p "/host:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/host:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/host:" 'host) "host"))
	  (should (string-equal (file-remote-p "/host:" 'localname) ""))
	  (should (string-equal (file-remote-p "/host:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/user@:")
		   (format "/%s@%s:" "user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/user@:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/user@:" 'user) "user"))
	  (should (string-equal (file-remote-p "/user@:" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/user@:" 'localname) ""))
	  (should (string-equal (file-remote-p "/user@:" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/user@host:")
		   (format "/%s@%s:" "user" "host")))
	  (should (string-equal
		   (file-remote-p "/user@host:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/user@host:" 'user) "user"))
	  (should (string-equal (file-remote-p "/user@host:" 'host) "host"))
	  (should (string-equal (file-remote-p "/user@host:" 'localname) ""))
	  (should (string-equal (file-remote-p "/user@host:" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/user@email@host:")
		   (format "/%s@%s:" "user@email" "host")))
	  (should (string-equal
		   (file-remote-p
		    "/user@email@host:" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/user@email@host:" 'user) "user@email"))
	  (should (string-equal
		   (file-remote-p "/user@email@host:" 'host) "host"))
	  (should (string-equal
		   (file-remote-p "/user@email@host:" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/user@email@host:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/host#1234:")
		   (format "/%s@%s:" "default-user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/host#1234:" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/host#1234:" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/host#1234:" 'host) "host#1234"))
	  (should (string-equal (file-remote-p "/host#1234:" 'localname) ""))
	  (should (string-equal (file-remote-p "/host#1234:" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/user@host#1234:")
		   (format "/%s@%s:" "user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/user@host#1234:" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/user@host#1234:" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/user@host#1234:" 'host) "host#1234"))
	  (should (string-equal
		   (file-remote-p "/user@host#1234:" 'localname) ""))
	  (should (string-equal (file-remote-p "/user@host#1234:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/1.2.3.4:")
		   (format "/%s@%s:" "default-user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/1.2.3.4:" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/1.2.3.4:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/1.2.3.4:" 'host) "1.2.3.4"))
	  (should (string-equal (file-remote-p "/1.2.3.4:" 'localname) ""))
	  (should (string-equal (file-remote-p "/1.2.3.4:" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/user@1.2.3.4:")
		   (format "/%s@%s:" "user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/user@1.2.3.4:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/user@1.2.3.4:" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/user@1.2.3.4:" 'host) "1.2.3.4"))
	  (should (string-equal (file-remote-p "/user@1.2.3.4:" 'localname) ""))
	  (should (string-equal (file-remote-p "/user@1.2.3.4:" 'hop) nil))

	  ;; Expand `tramp-default-method', `tramp-default-user' and
	  ;; `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[]:")
		   (format
		    "/%s@%s:" "default-user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/[]:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[]:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[]:" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/[]:" 'localname) ""))
	  (should (string-equal (file-remote-p "/[]:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (let ((tramp-default-host "::1"))
	    (should (string-equal
		     (file-remote-p "/[]:")
		     (format "/%s@%s:" "default-user" "[::1]")))
	    (should (string-equal
		     (file-remote-p "/[]:" 'method) "default-method"))
	    (should (string-equal (file-remote-p "/[]:" 'user) "default-user"))
	    (should (string-equal (file-remote-p "/[]:" 'host) "::1"))
	    (should (string-equal (file-remote-p "/[]:" 'localname) ""))
	    (should (string-equal (file-remote-p "/[]:" 'hop) nil)))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[::1]:")
		   (format "/%s@%s:" "default-user" "[::1]")))
	  (should (string-equal
		   (file-remote-p "/[::1]:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[::1]:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[::1]:" 'host) "::1"))
	  (should (string-equal (file-remote-p "/[::1]:" 'localname) ""))
	  (should (string-equal (file-remote-p "/[::1]:" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/user@[::1]:")
		   (format "/%s@%s:" "user" "[::1]")))
	  (should (string-equal
		   (file-remote-p "/user@[::1]:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/user@[::1]:" 'user) "user"))
	  (should (string-equal (file-remote-p "/user@[::1]:" 'host) "::1"))
	  (should (string-equal (file-remote-p "/user@[::1]:" 'localname) ""))
	  (should (string-equal (file-remote-p "/user@[::1]:" 'hop) nil))

	  ;; Local file name part.
	  (should (string-equal (file-remote-p "/host:/:" 'localname) "/:"))
	  (should (string-equal (file-remote-p "/host::" 'localname) ":"))
	  (should (string-equal (file-remote-p "/host: " 'localname) " "))
	  (should (string-equal (file-remote-p "/host:file" 'localname) "file"))
	  (should (string-equal
		   (file-remote-p "/host:/path/to/file" 'localname)
		   "/path/to/file"))

	  ;; Multihop.
	  (should
	   (string-equal
	    (file-remote-p "/user1@host1|user2@host2:/path/to/file")
	    (format "/%s@%s|%s@%s:" "user1" "host1" "user2" "host2")))
	  (should
	   (string-equal
	    (file-remote-p
	     "/user1@host1|user2@host2:/path/to/file" 'method)
	    "default-method"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/user1@host1|user2@host2:/path/to/file" 'user)
	    "user2"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/user1@host1|user2@host2:/path/to/file" 'host)
	    "host2"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/user1@host1|user2@host2:/path/to/file" 'localname)
	    "/path/to/file"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/user1@host1|user2@host2:/path/to/file" 'hop)
	    (format "%s@%s|" "user1" "host1")))

	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@host1"
	      "|user2@host2"
	      "|user3@host3:/path/to/file"))
	    (format "/%s@%s|%s@%s|%s@%s:"
		    "user1" "host1"
		    "user2" "host2"
		    "user3" "host3")))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@host1"
	      "|user2@host2"
	      "|user3@host3:/path/to/file")
	     'method)
	    "default-method"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@host1"
	      "|user2@host2"
	      "|user3@host3:/path/to/file")
	     'user)
	    "user3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@host1"
	      "|user2@host2"
	      "|user3@host3:/path/to/file")
	     'host)
	    "host3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@host1"
	      "|user2@host2"
	      "|user3@host3:/path/to/file")
	     'localname)
	    "/path/to/file"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@host1"
	      "|user2@host2"
	      "|user3@host3:/path/to/file")
	     'hop)
	    (format "%s@%s|%s@%s|"
		    "user1" "host1" "user2" "host2")))

	  ;; Expand `tramp-default-user-alist'.
	  (add-to-list 'tramp-default-user-alist '(nil "host1" "user1"))
	  (add-to-list 'tramp-default-user-alist '(nil "host2" "user2"))
	  (add-to-list 'tramp-default-user-alist '(nil "host3" "user3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/host1"
	      "|host2"
	      "|host3:/path/to/file"))
	    (format "/%s|%s|%s@%s:"
		    "host1"
		    "host2"
		    "user3" "host3")))

	  ;; Expand `tramp-default-host-alist'.
	  (add-to-list 'tramp-default-host-alist '(nil "user1" "host1"))
	  (add-to-list 'tramp-default-host-alist '(nil "user2" "host2"))
	  (add-to-list 'tramp-default-host-alist '(nil "user3" "host3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/user1@"
	      "|user2@"
	      "|user3@:/path/to/file"))
	    (format "/%s@|%s@|%s@%s:"
		    "user1"
		    "user2"
		    "user3" "host3"))))

      ;; Exit.
      (tramp-change-syntax syntax))))

(ert-deftest tramp-test02-file-name-dissect-separate ()
  "Check separate file name components."
  :tags '(:expensive-test)
  (let ((tramp-default-method "default-method")
	(tramp-default-user "default-user")
	(tramp-default-host "default-host")
	tramp-default-method-alist
	tramp-default-user-alist
	tramp-default-host-alist
	(syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'separate)
	  ;; Expand `tramp-default-user' and `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[method/]")
		   (format
		    "/[%s/%s@%s]" "method" "default-user" "default-host")))
	  (should (string-equal (file-remote-p "/[method/]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[method/]" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/[method/]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[/host]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "host")))
	  (should (string-equal
		   (file-remote-p "/[/host]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[/host]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[/host]" 'host) "host"))
	  (should (string-equal (file-remote-p "/[/host]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/host]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[/user@]")
		   (format
		    "/[%s/%s@%s]" "default-method" "user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/[/user@]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[/user@]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[/user@]" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/[/user@]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/user@]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[/user@host]")
		   (format "/[%s/%s@%s]" "default-method" "user" "host")))
	  (should (string-equal
		   (file-remote-p "/[/user@host]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[/user@host]" 'user) "user"))
	  (should (string-equal (file-remote-p "/[/user@host]" 'host) "host"))
	  (should (string-equal (file-remote-p "/[/user@host]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/user@host]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[-/host]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "host")))
	  (should (string-equal
		   (file-remote-p "/[-/host]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[-/host]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[-/host]" 'host) "host"))
	  (should (string-equal (file-remote-p "/[-/host]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/host]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[-/user@]")
		   (format
		    "/[%s/%s@%s]" "default-method" "user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/[-/user@]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[-/user@]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[-/user@]" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/[-/user@]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/user@]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[-/user@host]")
		   (format "/[%s/%s@%s]" "default-method" "user" "host")))
	  (should (string-equal
		   (file-remote-p "/[-/user@host]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[-/user@host]" 'user) "user"))
	  (should (string-equal (file-remote-p "/[-/user@host]" 'host) "host"))
	  (should (string-equal (file-remote-p "/[-/user@host]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/user@host]" 'hop) nil))

	  ;; Expand `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[method/host]")
		   (format "/[%s/%s@%s]" "method" "default-user" "host")))
	  (should (string-equal
		   (file-remote-p "/[method/host]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/host]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[method/host]" 'host) "host"))
	  (should (string-equal (file-remote-p "/[method/host]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/host]" 'hop) nil))

	  ;; Expand `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[method/user@]")
		   (format "/[%s/%s@%s]" "method" "user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/[method/user@]" 'method) "method"))
	  (should (string-equal (file-remote-p "/[method/user@]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[method/user@]" 'host) "default-host"))
	  (should (string-equal
		   (file-remote-p "/[method/user@]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/user@]" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/[method/user@host]")
		   (format "/[%s/%s@%s]" "method" "user" "host")))
	  (should (string-equal
		   (file-remote-p "/[method/user@host]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/user@host]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[method/user@host]" 'host) "host"))
	  (should (string-equal
		   (file-remote-p "/[method/user@host]" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/[method/user@host]" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/[method/user@email@host]")
		   (format "/[%s/%s@%s]" "method" "user@email" "host")))
	  (should (string-equal
		   (file-remote-p
		    "/[method/user@email@host]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p
		    "/[method/user@email@host]" 'user) "user@email"))
	  (should (string-equal
		   (file-remote-p "/[method/user@email@host]" 'host) "host"))
	  (should (string-equal
		   (file-remote-p "/[method/user@email@host]" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/[method/user@email@host]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[/host#1234]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/[/host#1234]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[/host#1234]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[/host#1234]" 'host) "host#1234"))
	  (should (string-equal (file-remote-p "/[/host#1234]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/host#1234]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[/user@host#1234]")
		   (format "/[%s/%s@%s]" "default-method" "user" "host#1234")))
	  (should (string-equal
		   (file-remote-p
		    "/[/user@host#1234]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p
		    "/[/user@host#1234]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[/user@host#1234]" 'host) "host#1234"))
	  (should (string-equal
		   (file-remote-p "/[/user@host#1234]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/user@host#1234]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[-/host#1234]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/[-/host#1234]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[-/host#1234]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[-/host#1234]" 'host) "host#1234"))
	  (should (string-equal (file-remote-p "/[-/host#1234]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/host#1234]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[-/user@host#1234]")
		   (format "/[%s/%s@%s]" "default-method" "user" "host#1234")))
	  (should (string-equal
		   (file-remote-p
		    "/[-/user@host#1234]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p
		    "/[-/user@host#1234]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[-/user@host#1234]" 'host) "host#1234"))
	  (should (string-equal
		   (file-remote-p "/[-/user@host#1234]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/user@host#1234]" 'hop) nil))

	  ;; Expand `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[method/host#1234]")
		   (format "/[%s/%s@%s]" "method" "default-user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/[method/host#1234]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/host#1234]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[method/host#1234]" 'host) "host#1234"))
	  (should (string-equal
		   (file-remote-p "/[method/host#1234]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/host#1234]" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/[method/user@host#1234]")
		   (format "/[%s/%s@%s]" "method" "user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/[method/user@host#1234]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/user@host#1234]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p
		    "/[method/user@host#1234]" 'host) "host#1234"))
	  (should (string-equal
		   (file-remote-p "/[method/user@host#1234]" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/[method/user@host#1234]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[/1.2.3.4]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/[/1.2.3.4]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[/1.2.3.4]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[/1.2.3.4]" 'host) "1.2.3.4"))
	  (should (string-equal (file-remote-p "/[/1.2.3.4]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/1.2.3.4]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[/user@1.2.3.4]")
		   (format "/[%s/%s@%s]" "default-method" "user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p
		    "/[/user@1.2.3.4]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[/user@1.2.3.4]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[/user@1.2.3.4]" 'host) "1.2.3.4"))
	  (should (string-equal
		   (file-remote-p "/[/user@1.2.3.4]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/user@1.2.3.4]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[-/1.2.3.4]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/[-/1.2.3.4]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[-/1.2.3.4]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[-/1.2.3.4]" 'host) "1.2.3.4"))
	  (should (string-equal (file-remote-p "/[-/1.2.3.4]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/1.2.3.4]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[-/user@1.2.3.4]")
		   (format "/[%s/%s@%s]" "default-method" "user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p
		    "/[-/user@1.2.3.4]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[-/user@1.2.3.4]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[-/user@1.2.3.4]" 'host) "1.2.3.4"))
	  (should (string-equal
		   (file-remote-p "/[-/user@1.2.3.4]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/user@1.2.3.4]" 'hop) nil))

	  ;; Expand `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[method/1.2.3.4]")
		   (format "/[%s/%s@%s]" "method" "default-user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/[method/1.2.3.4]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/1.2.3.4]" 'user) "default-user"))
	  (should (string-equal
		   (file-remote-p "/[method/1.2.3.4]" 'host) "1.2.3.4"))
	  (should (string-equal
		   (file-remote-p "/[method/1.2.3.4]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/1.2.3.4]" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/[method/user@1.2.3.4]")
		   (format "/[%s/%s@%s]" "method" "user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/[method/user@1.2.3.4]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/user@1.2.3.4]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[method/user@1.2.3.4]" 'host) "1.2.3.4"))
	  (should (string-equal
		   (file-remote-p "/[method/user@1.2.3.4]" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/[method/user@1.2.3.4]" 'hop) nil))

	  ;; Expand `tramp-default-method', `tramp-default-user' and
	  ;; `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[/]")
		   (format
		    "/[%s/%s@%s]"
		    "default-method" "default-user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/[/]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[/]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[/]" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/[/]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (let ((tramp-default-host "::1"))
	    (should (string-equal
		     (file-remote-p "/[/]")
		     (format
		      "/[%s/%s@%s]"
		      "default-method" "default-user" "::1")))
	    (should (string-equal
		     (file-remote-p "/[/]" 'method) "default-method"))
	    (should (string-equal (file-remote-p "/[/]" 'user) "default-user"))
	    (should (string-equal (file-remote-p "/[/]" 'host) "::1"))
	    (should (string-equal (file-remote-p "/[/]" 'localname) ""))
	    (should (string-equal (file-remote-p "/[/]" 'hop) nil)))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[/::1]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "::1")))
	  (should (string-equal
		   (file-remote-p "/[/::1]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[/::1]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[/::1]" 'host) "::1"))
	  (should (string-equal (file-remote-p "/[/::1]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/::1]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[/user@::1]")
		   (format "/[%s/%s@%s]" "default-method" "user" "::1")))
	  (should (string-equal
		   (file-remote-p "/[/user@::1]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[/user@::1]" 'user) "user"))
	  (should (string-equal (file-remote-p "/[/user@::1]" 'host) "::1"))
	  (should (string-equal (file-remote-p "/[/user@::1]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[/user@::1]" 'hop) nil))

	  ;; Expand `tramp-default-method', `tramp-default-user' and
	  ;; `tramp-default-host'.
	  (should (string-equal
		   (file-remote-p "/[-/]")
		   (format
		    "/[%s/%s@%s]"
		    "default-method" "default-user" "default-host")))
	  (should (string-equal
		   (file-remote-p "/[-/]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[-/]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[-/]" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/[-/]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/]" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (let ((tramp-default-host "::1"))
	    (should (string-equal
		     (file-remote-p "/[-/]")
		     (format
		      "/[%s/%s@%s]"
		      "default-method" "default-user" "::1")))
	    (should (string-equal
		     (file-remote-p "/[-/]" 'method) "default-method"))
	    (should (string-equal (file-remote-p "/[-/]" 'user) "default-user"))
	    (should (string-equal (file-remote-p "/[-/]" 'host) "::1"))
	    (should (string-equal (file-remote-p "/[-/]" 'localname) ""))
	    (should (string-equal (file-remote-p "/[-/]" 'hop) nil)))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[-/::1]")
		   (format
		    "/[%s/%s@%s]" "default-method" "default-user" "::1")))
	  (should (string-equal
		   (file-remote-p "/[-/::1]" 'method) "default-method"))
	  (should (string-equal
		   (file-remote-p "/[-/::1]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[-/::1]" 'host) "::1"))
	  (should (string-equal (file-remote-p "/[-/::1]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/::1]" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/[-/user@::1]")
		   (format "/[%s/%s@%s]" "default-method" "user" "::1")))
	  (should (string-equal
		   (file-remote-p "/[-/user@::1]" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/[-/user@::1]" 'user) "user"))
	  (should (string-equal (file-remote-p "/[-/user@::1]" 'host) "::1"))
	  (should (string-equal (file-remote-p "/[-/user@::1]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[-/user@::1]" 'hop) nil))

	  ;; Expand `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/[method/::1]")
		   (format "/[%s/%s@%s]" "method" "default-user" "::1")))
	  (should (string-equal
		   (file-remote-p "/[method/::1]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/::1]" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/[method/::1]" 'host) "::1"))
	  (should (string-equal (file-remote-p "/[method/::1]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/::1]" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/[method/user@::1]")
		   (format "/[%s/%s@%s]" "method" "user" "::1")))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1]" 'host) "::1"))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1]" 'localname) ""))
	  (should (string-equal (file-remote-p "/[method/user@::1]" 'hop) nil))

	  ;; Local file name part.
	  (should (string-equal (file-remote-p "/[/host]/:" 'localname) "/:"))
	  (should (string-equal (file-remote-p "/[-/host]/:" 'localname) "/:"))
	  (should (string-equal (file-remote-p "/[method/]:" 'localname) ":"))
	  (should (string-equal (file-remote-p "/[method/] " 'localname) " "))
	  (should (string-equal
		   (file-remote-p "/[method/]file" 'localname) "file"))
	  (should (string-equal
		   (file-remote-p "/[method/]/path/to/file" 'localname)
		   "/path/to/file"))

	  ;; Multihop.
	  (should
	   (string-equal
	    (file-remote-p
	     "/[method1/user1@host1|method2/user2@host2]/path/to/file")
	    (format "/[%s/%s@%s|%s/%s@%s]"
		    "method1" "user1" "host1" "method2" "user2" "host2")))
	  (should
	   (string-equal
	    (file-remote-p
	     "/[method1/user1@host1|method2/user2@host2]/path/to/file" 'method)
	    "method2"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/[method1/user1@host1|method2/user2@host2]/path/to/file" 'user)
	    "user2"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/[method1/user1@host1|method2/user2@host2]/path/to/file" 'host)
	    "host2"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/[method1/user1@host1|method2/user2@host2]/path/to/file"
	     'localname)
	    "/path/to/file"))
	  (should
	   (string-equal
	    (file-remote-p
	     "/[method1/user1@host1|method2/user2@host2]/path/to/file" 'hop)
	    (format "%s/%s@%s|"
		    "method1" "user1" "host1")))

	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@host1"
	      "|method2/user2@host2"
	      "|method3/user3@host3]/path/to/file"))
	    (format "/[%s/%s@%s|%s/%s@%s|%s/%s@%s]"
		    "method1" "user1" "host1"
		    "method2" "user2" "host2"
		    "method3" "user3" "host3")))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@host1"
	      "|method2/user2@host2"
	      "|method3/user3@host3]/path/to/file")
	     'method)
	    "method3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@host1"
	      "|method2/user2@host2"
	      "|method3/user3@host3]/path/to/file")
	     'user)
	    "user3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@host1"
	      "|method2/user2@host2"
	      "|method3/user3@host3]/path/to/file")
	     'host)
	    "host3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@host1"
	      "|method2/user2@host2"
	      "|method3/user3@host3]/path/to/file")
	     'localname)
	    "/path/to/file"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@host1"
	      "|method2/user2@host2"
	      "|method3/user3@host3]/path/to/file")
	     'hop)
	    (format "%s/%s@%s|%s/%s@%s|"
		    "method1" "user1" "host1" "method2" "user2" "host2")))

	  ;; Expand `tramp-default-method-alist'.
	  (add-to-list 'tramp-default-method-alist '("host1" "user1" "method1"))
	  (add-to-list 'tramp-default-method-alist '("host2" "user2" "method2"))
	  (add-to-list 'tramp-default-method-alist '("host3" "user3" "method3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[/user1@host1"
	      "|/user2@host2"
	      "|/user3@host3]/path/to/file"))
	    (format "/[/%s@%s|/%s@%s|%s/%s@%s]"
		    "user1" "host1"
		    "user2" "host2"
		    "method3" "user3" "host3")))

	  ;; Expand `tramp-default-user-alist'.
	  (add-to-list 'tramp-default-user-alist '("method1" "host1" "user1"))
	  (add-to-list 'tramp-default-user-alist '("method2" "host2" "user2"))
	  (add-to-list 'tramp-default-user-alist '("method3" "host3" "user3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/host1"
	      "|method2/host2"
	      "|method3/host3]/path/to/file"))
	    (format "/[%s/%s|%s/%s|%s/%s@%s]"
		    "method1" "host1"
		    "method2" "host2"
		    "method3" "user3" "host3")))

	  ;; Expand `tramp-default-host-alist'.
	  (add-to-list 'tramp-default-host-alist '("method1" "user1" "host1"))
	  (add-to-list 'tramp-default-host-alist '("method2" "user2" "host2"))
	  (add-to-list 'tramp-default-host-alist '("method3" "user3" "host3"))
	  (should
	   (string-equal
	    (file-remote-p
	     (concat
	      "/[method1/user1@"
	      "|method2/user2@"
	      "|method3/user3@]/path/to/file"))
	    (format "/[%s/%s@|%s/%s@|%s/%s@%s]"
		    "method1" "user1"
		    "method2" "user2"
		    "method3" "user3" "host3"))))

      ;; Exit.
      (tramp-change-syntax syntax))))

(ert-deftest tramp-test03-file-name-defaults ()
  "Check default values for some methods."
  ;; Default values in tramp-adb.el.
  (should (string-equal (file-remote-p "/adb::" 'host) ""))
  ;; Default values in tramp-ftp.el.
  (should (string-equal (file-remote-p "/-:ftp.host:" 'method) "ftp"))
  (dolist (u '("ftp" "anonymous"))
    (should (string-equal (file-remote-p (format "/-:%s@:" u) 'method) "ftp")))
  ;; Default values in tramp-gvfs.el.
  (when (and (load "tramp-gvfs" 'noerror 'nomessage)
	     (symbol-value 'tramp-gvfs-enabled))
    (should (string-equal (file-remote-p "/synce::" 'user) nil)))
  ;; Default values in tramp-sh.el.
  (dolist (h `("127.0.0.1" "[::1]" "localhost" "localhost6" ,(system-name)))
    (should
     (string-equal (file-remote-p (format "/-:root@%s:" h) 'method) "su")))
  (dolist (m '("su" "sudo" "ksu"))
    (should (string-equal (file-remote-p (format "/%s::" m) 'user) "root")))
  (dolist (m '("rcp" "remcp" "rsh" "telnet" "krlogin" "fcp"))
    (should
     (string-equal (file-remote-p (format "/%s::" m) 'user) (user-login-name))))
  ;; Default values in tramp-smb.el.
  (should (string-equal (file-remote-p "/smb::" 'user) nil)))

(ert-deftest tramp-test04-substitute-in-file-name ()
  "Check `substitute-in-file-name'."
  (should (string-equal (substitute-in-file-name "/method:host:///foo") "/foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host://foo") "/method:host:/foo"))
  (should
   (string-equal (substitute-in-file-name "/method:host:/path///foo") "/foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/path//foo") "/method:host:/foo"))
  ;; Quoting local part.
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/:///foo") "/method:host:/:///foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/://foo") "/method:host:/://foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/:/path///foo")
    "/method:host:/:/path///foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/:/path//foo")
    "/method:host:/:/path//foo"))

  (should
   (string-equal (substitute-in-file-name "/method:host://~foo") "/~foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/~foo") "/method:host:/~foo"))
  (should
   (string-equal (substitute-in-file-name "/method:host:/path//~foo") "/~foo"))
  ;; (substitute-in-file-name "/path/~foo") expands only for a local
  ;; user "foo" to "/~foo"".  Otherwise, it doesn't expand.
  (should
   (string-equal
    (substitute-in-file-name
     "/method:host:/path/~foo") "/method:host:/path/~foo"))
  ;; Quoting local part.
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/://~foo") "/method:host:/://~foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/:/~foo") "/method:host:/:/~foo"))
  (should
   (string-equal
    (substitute-in-file-name
     "/method:host:/:/path//~foo") "/method:host:/:/path//~foo"))
  (should
   (string-equal
    (substitute-in-file-name
     "/method:host:/:/path/~foo") "/method:host:/:/path/~foo"))

  (let (process-environment)
    (should
     (string-equal
      (substitute-in-file-name "/method:host:/path/$FOO")
      "/method:host:/path/$FOO"))
    (setenv "FOO" "bla")
    (should
     (string-equal
      (substitute-in-file-name "/method:host:/path/$FOO")
      "/method:host:/path/bla"))
    (should
     (string-equal
      (substitute-in-file-name "/method:host:/path/$$FOO")
      "/method:host:/path/$FOO"))
    ;; Quoting local part.
    (should
     (string-equal
      (substitute-in-file-name "/method:host:/:/path/$FOO")
      "/method:host:/:/path/$FOO"))
    (setenv "FOO" "bla")
    (should
     (string-equal
      (substitute-in-file-name "/method:host:/:/path/$FOO")
      "/method:host:/:/path/$FOO"))
    (should
     (string-equal
      (substitute-in-file-name "/method:host:/:/path/$$FOO")
      "/method:host:/:/path/$$FOO"))))

(ert-deftest tramp-test05-expand-file-name ()
  "Check `expand-file-name'."
  (should
   (string-equal
    (expand-file-name "/method:host:/path/./file") "/method:host:/path/file"))
  (should
   (string-equal
    (expand-file-name "/method:host:/path/../file") "/method:host:/file"))
  ;; Quoting local part.
  (should
   (string-equal
    (expand-file-name "/method:host:/:/path/./file")
    "/method:host:/:/path/file"))
  (should
   (string-equal
    (expand-file-name "/method:host:/:/path/../file") "/method:host:/:/file"))
  (should
   (string-equal
    (expand-file-name "/method:host:/:/~/path/./file")
    "/method:host:/:/~/path/file")))

;; The following test is inspired by Bug#26911.  It is rather a bug in
;; `expand-file-name', and it fails for all Emacs versions.  Test
;; added for later, when it is fixed.
(ert-deftest tramp-test05-expand-file-name-relative ()
  "Check `expand-file-name'."
  ;; Mark as failed until bug has been fixed.
  :expected-result :failed
  (skip-unless (tramp--test-enabled))
  ;; These are the methods the test doesn't fail.
  (when (or (tramp--test-adb-p) (tramp--test-gvfs-p)
	    (tramp-smb-file-name-p tramp-test-temporary-file-directory))
    (setf (ert-test-expected-result-type
	   (ert-get-test 'tramp-test05-expand-file-name-relative))
	  :passed))

  (should
   (string-equal
    (let ((default-directory
	    (concat
	     (file-remote-p tramp-test-temporary-file-directory) "/path")))
      (expand-file-name ".." "./"))
    (concat (file-remote-p tramp-test-temporary-file-directory) "/"))))

(ert-deftest tramp-test06-directory-file-name ()
  "Check `directory-file-name'.
This checks also `file-name-as-directory', `file-name-directory',
`file-name-nondirectory' and `unhandled-file-name-directory'."
  (should
   (string-equal
    (directory-file-name "/method:host:/path/to/file")
    "/method:host:/path/to/file"))
  (should
   (string-equal
    (directory-file-name "/method:host:/path/to/file/")
    "/method:host:/path/to/file"))
  (should
   (string-equal
    (directory-file-name "/method:host:/path/to/file//")
    "/method:host:/path/to/file"))
  (should
   (string-equal
    (file-name-as-directory "/method:host:/path/to/file")
    "/method:host:/path/to/file/"))
  (should
   (string-equal
    (file-name-as-directory "/method:host:/path/to/file/")
    "/method:host:/path/to/file/"))
  (should
   (string-equal
    (file-name-directory "/method:host:/path/to/file")
    "/method:host:/path/to/"))
  (should
   (string-equal
    (file-name-directory "/method:host:/path/to/file/")
    "/method:host:/path/to/file/"))
  (should
   (string-equal (file-name-nondirectory "/method:host:/path/to/file") "file"))
  (should
   (string-equal (file-name-nondirectory "/method:host:/path/to/file/") ""))
  (should-not
   (unhandled-file-name-directory "/method:host:/path/to/file"))

  ;; Bug#10085.
  (when (tramp--test-enabled) ;; Packages like tramp-gvfs.el might be disabled.
    (dolist (n-e '(nil t))
      ;; We must clear `tramp-default-method'.  On hydra, it is "ftp",
      ;; which ruins the tests.
      (let ((non-essential n-e)
	    tramp-default-method)
	(dolist
	    (file
	     `(,(format
		 "/%s::"
		 (file-remote-p tramp-test-temporary-file-directory 'method))
	       ,(format
		  "/-:%s:"
		  (file-remote-p tramp-test-temporary-file-directory 'host))))
	  (should (string-equal (directory-file-name file) file))
	  (should
	   (string-equal
	    (file-name-as-directory file)
	    (if (tramp-completion-mode-p)
		file (concat file "./"))))
	  (should (string-equal (file-name-directory file) file))
	  (should (string-equal (file-name-nondirectory file) "")))))))

(ert-deftest tramp-test07-file-exists-p ()
  "Check `file-exist-p', `write-region' and `delete-file'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (should-not (file-exists-p tmp-name))
      (write-region "foo" nil tmp-name)
      (should (file-exists-p tmp-name))
      (delete-file tmp-name)
      (should-not (file-exists-p tmp-name)))))

(ert-deftest tramp-test08-file-local-copy ()
  "Check `file-local-copy'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  tmp-name2)
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name1)
	    (should (setq tmp-name2 (file-local-copy tmp-name1)))
	    (with-temp-buffer
	      (insert-file-contents tmp-name2)
	      (should (string-equal (buffer-string) "foo")))
	    ;; Check also that a file transfer with compression works.
	    (let ((default-directory tramp-test-temporary-file-directory)
		  (tramp-copy-size-limit 4)
		  (tramp-inline-compress-start-size 2))
	      (delete-file tmp-name2)
	      (should (setq tmp-name2 (file-local-copy tmp-name1))))
	    ;; Error case.
	    (delete-file tmp-name1)
	    (delete-file tmp-name2)
	    (should-error
	     (setq tmp-name2 (file-local-copy tmp-name1))
	     :type tramp-file-missing))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2))))))

(ert-deftest tramp-test09-insert-file-contents ()
  "Check `insert-file-contents'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (insert-file-contents tmp-name)
	    (should (string-equal (buffer-string) "foo"))
	    (insert-file-contents tmp-name)
	    (should (string-equal (buffer-string) "foofoo"))
	    ;; Insert partly.
	    (insert-file-contents tmp-name nil 1 3)
	    (should (string-equal (buffer-string) "oofoofoo"))
	    ;; Replace.
	    (insert-file-contents tmp-name nil nil nil 'replace)
	    (should (string-equal (buffer-string) "foo"))
	    ;; Error case.
	    (delete-file tmp-name)
	    (should-error
	     (insert-file-contents tmp-name)
	     :type tramp-file-missing))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

(ert-deftest tramp-test10-write-region ()
  "Check `write-region'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
            ;; Write buffer.  Use absolute and relative file name.
	    (with-temp-buffer
	      (insert "foo")
	      (write-region nil nil tmp-name))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "foo")))
	    (delete-file tmp-name)
	    (with-temp-buffer
	      (insert "foo")
	      (should-not (file-exists-p tmp-name))
	      (let ((default-directory (file-name-directory tmp-name)))
		(should-not (file-exists-p (file-name-nondirectory tmp-name)))
		(write-region nil nil (file-name-nondirectory tmp-name))
		(should (file-exists-p (file-name-nondirectory tmp-name))))
	      (should (file-exists-p tmp-name)))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "foo")))

	    ;; Append.
	    (with-temp-buffer
	      (insert "bla")
	      (write-region nil nil tmp-name 'append))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "foobla")))
	    (with-temp-buffer
	      (insert "baz")
	      (write-region nil nil tmp-name 3))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "foobaz")))

	    ;; Write string.
	    (write-region "foo" nil tmp-name)
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "foo")))

	    ;; Write partly.
	    (with-temp-buffer
	      (insert "123456789")
	      (write-region 3 5 tmp-name))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "34")))

	    ;; Check message.
	    ;; Macro `ert-with-message-capture' was introduced in Emacs 26.1.
	    (with-no-warnings (when (symbol-plist 'ert-with-message-capture)
	      (let ((tramp-message-show-message t))
		(dolist (noninteractive '(nil t))
		  (dolist (visit '(nil t "string" no-message))
		    (ert-with-message-capture tramp--test-messages
		      (write-region "foo" nil tmp-name nil visit)
		      ;; We must check the last line.  There could be
		      ;; other messages from the progress reporter.
		      (should
		       (string-match
			(if (and (null noninteractive)
				 (or (eq visit t) (null visit) (stringp visit)))
			    (format "^Wrote %s\n\\'" tmp-name) "^\\'")
			tramp--test-messages))))))))

	    ;; Do not overwrite if excluded.
	    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
	      (write-region "foo" nil tmp-name nil nil nil 'mustbenew))
	    ;; `mustbenew' is passed to Tramp since Emacs 26.1.
	    (when (tramp--test-emacs26-p)
	      (should-error
	       (cl-letf (((symbol-function 'y-or-n-p) 'ignore))
		 (write-region "foo" nil tmp-name nil nil nil 'mustbenew))
               :type 'file-already-exists)
	      (should-error
	       (write-region "foo" nil tmp-name nil nil nil 'excl)
	       :type 'file-already-exists)))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

(ert-deftest tramp-test11-copy-file ()
  "Check `copy-file'."
  (skip-unless (tramp--test-enabled))

  ;; `filename-non-special' has been fixed in Emacs 27.1, see Bug#29579.
  (dolist (quoted (if (and (tramp--test-expensive-test) (tramp--test-emacs27-p))
		      '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (tmp-name3 (tramp--test-make-temp-name 'local quoted)))
      (dolist (source-target
	       `(;; Copy on remote side.
		 (,tmp-name1 . ,tmp-name2)
		 ;; Copy from remote side to local side.
		 (,tmp-name1 . ,tmp-name3)
		 ;; Copy from local side to remote side.
		 (,tmp-name3 . ,tmp-name1)))
	(let ((source (car source-target))
	      (target (cdr source-target)))

	  ;; Copy simple file.
	  (unwind-protect
	      (progn
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(copy-file source target)
		(should (file-exists-p target))
		(with-temp-buffer
		  (insert-file-contents target)
		  (should (string-equal (buffer-string) "foo")))
		(when (tramp--test-expensive-test)
		  (should-error
		   (copy-file source target)
		   :type 'file-already-exists))
		(copy-file source target 'ok))

	    ;; Cleanup.
	    (ignore-errors (delete-file source))
	    (ignore-errors (delete-file target)))

	  ;; Copy file to directory.
	  (unwind-protect
	      ;; FIXME: This fails on my QNAP server, see
	      ;; /share/Web/owncloud/data/owncloud.log
	      (unless (tramp--test-owncloud-p)
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(make-directory target)
		(should (file-directory-p target))
		;; This has been changed in Emacs 26.1.
		(when (and (tramp--test-expensive-test) (tramp--test-emacs26-p))
		  (should-error
		   (copy-file source target)
		   :type 'file-already-exists))
		(copy-file source (file-name-as-directory target))
		(should
		 (file-exists-p
		  (expand-file-name (file-name-nondirectory source) target))))

	    ;; Cleanup.
	    (ignore-errors (delete-file source))
	    (ignore-errors (delete-directory target 'recursive)))

	  ;; Copy directory to existing directory.
	  (unwind-protect
	      ;; FIXME: This fails on my QNAP server, see
	      ;; /share/Web/owncloud/data/owncloud.log
	      (unless (and (tramp--test-owncloud-p)
			   (or (not (file-remote-p source))
			       (not (file-remote-p target))))
		(make-directory source)
		(should (file-directory-p source))
		(write-region "foo" nil (expand-file-name "foo" source))
		(should (file-exists-p (expand-file-name "foo" source)))
		(make-directory target)
		(should (file-directory-p target))
		;; Directory `target' exists already, so we must use
		;; `file-name-as-directory'.
		(copy-file source (file-name-as-directory target))
		(should
		 (file-exists-p
		  (expand-file-name
		   (concat (file-name-nondirectory source) "/foo") target))))

	    ;; Cleanup.
	    (ignore-errors (delete-directory source 'recursive))
	    (ignore-errors (delete-directory target 'recursive)))

	  ;; Copy directory/file to non-existing directory.
	  (unwind-protect
	      ;; FIXME: This fails on my QNAP server, see
	      ;; /share/Web/owncloud/data/owncloud.log
	      (unless
		  (and (tramp--test-owncloud-p) (not (file-remote-p source)))
		(make-directory source)
		(should (file-directory-p source))
		(write-region "foo" nil (expand-file-name "foo" source))
		(should (file-exists-p (expand-file-name "foo" source)))
		(make-directory target)
		(should (file-directory-p target))
		(copy-file
		 source
		 (expand-file-name (file-name-nondirectory source) target))
		(should
		 (file-exists-p
		  (expand-file-name
		   (concat (file-name-nondirectory source) "/foo") target))))

	    ;; Cleanup.
	    (ignore-errors (delete-directory source 'recursive))
	    (ignore-errors (delete-directory target 'recursive))))))))

(ert-deftest tramp-test12-rename-file ()
  "Check `rename-file'."
  (skip-unless (tramp--test-enabled))

  ;; `filename-non-special' has been fixed in Emacs 27.1, see Bug#29579.
  (dolist (quoted (if (and (tramp--test-expensive-test) (tramp--test-emacs27-p))
		      '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (tmp-name3 (tramp--test-make-temp-name 'local quoted)))
      (dolist (source-target
	       `(;; Rename on remote side.
		 (,tmp-name1 . ,tmp-name2)
		 ;; Rename from remote side to local side.
		 (,tmp-name1 . ,tmp-name3)
		 ;; Rename from local side to remote side.
		 (,tmp-name3 . ,tmp-name1)))
	(let ((source (car source-target))
	      (target (cdr source-target)))

	  ;; Rename simple file.
	  (unwind-protect
	      (progn
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(rename-file source target)
		(should-not (file-exists-p source))
		(should (file-exists-p target))
		(with-temp-buffer
		  (insert-file-contents target)
		  (should (string-equal (buffer-string) "foo")))
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(when (tramp--test-expensive-test)
		  (should-error
		   (rename-file source target)
		   :type 'file-already-exists))
		(rename-file source target 'ok)
		(should-not (file-exists-p source)))

	    ;; Cleanup.
	    (ignore-errors (delete-file source))
	    (ignore-errors (delete-file target)))

	  ;; Rename file to directory.
	  (unwind-protect
	      (progn
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(make-directory target)
		(should (file-directory-p target))
		;; This has been changed in Emacs 26.1.
		(when (and (tramp--test-expensive-test) (tramp--test-emacs26-p))
		  (should-error
		   (rename-file source target)
		   :type 'file-already-exists))
		(rename-file source (file-name-as-directory target))
		(should-not (file-exists-p source))
		(should
		 (file-exists-p
		  (expand-file-name (file-name-nondirectory source) target))))

	    ;; Cleanup.
	    (ignore-errors (delete-file source))
	    (ignore-errors (delete-directory target 'recursive)))

	  ;; Rename directory to existing directory.
	  (unwind-protect
	      ;; FIXME: This fails on my QNAP server, see
	      ;; /share/Web/owncloud/data/owncloud.log
	      (unless (tramp--test-owncloud-p)
		(make-directory source)
		(should (file-directory-p source))
		(write-region "foo" nil (expand-file-name "foo" source))
		(should (file-exists-p (expand-file-name "foo" source)))
		(make-directory target)
		(should (file-directory-p target))
		;; Directory `target' exists already, so we must use
		;; `file-name-as-directory'.
		(rename-file source (file-name-as-directory target))
		(should-not (file-exists-p source))
		(should
		 (file-exists-p
		  (expand-file-name
		   (concat (file-name-nondirectory source) "/foo") target))))

	    ;; Cleanup.
	    (ignore-errors (delete-directory source 'recursive))
	    (ignore-errors (delete-directory target 'recursive)))

	  ;; Rename directory/file to non-existing directory.
	  (unwind-protect
	      ;; FIXME: This fails on my QNAP server, see
	      ;; /share/Web/owncloud/data/owncloud.log
	      (unless (tramp--test-owncloud-p)
		(make-directory source)
		(should (file-directory-p source))
		(write-region "foo" nil (expand-file-name "foo" source))
		(should (file-exists-p (expand-file-name "foo" source)))
		(make-directory target)
		(should (file-directory-p target))
		(rename-file
		 source
		 (expand-file-name (file-name-nondirectory source) target))
		(should-not (file-exists-p source))
		(should
		 (file-exists-p
		  (expand-file-name
		   (concat (file-name-nondirectory source) "/foo") target))))

	    ;; Cleanup.
	    (ignore-errors (delete-directory source 'recursive))
	    (ignore-errors (delete-directory target 'recursive))))))))

(ert-deftest tramp-test13-make-directory ()
  "Check `make-directory'.
This tests also `file-directory-p' and `file-accessible-directory-p'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "foo/bar" tmp-name1)))
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (should (file-directory-p tmp-name1))
	    (should (file-accessible-directory-p tmp-name1))
	    (should-error (make-directory tmp-name2) :type 'file-error)
	    (make-directory tmp-name2 'parents)
	    (should (file-directory-p tmp-name2))
	    (should (file-accessible-directory-p tmp-name2))
	    ;; If PARENTS is non-nil, `make-directory' shall not
	    ;; signal an error when DIR exists already.
	    (make-directory tmp-name2 'parents))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test14-delete-directory ()
  "Check `delete-directory'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      ;; Delete empty directory.
      (make-directory tmp-name)
      (should (file-directory-p tmp-name))
      (delete-directory tmp-name)
      (should-not (file-directory-p tmp-name))
      ;; Delete non-empty directory.
      (make-directory tmp-name)
      (should (file-directory-p tmp-name))
      (write-region "foo" nil (expand-file-name "bla" tmp-name))
      (should (file-exists-p (expand-file-name "bla" tmp-name)))
      (should-error (delete-directory tmp-name) :type 'file-error)
      (delete-directory tmp-name 'recursive)
      (should-not (file-directory-p tmp-name)))))

(ert-deftest tramp-test15-copy-directory ()
  "Check `copy-directory'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name nil quoted))
	   (tmp-name3 (expand-file-name
		       (file-name-nondirectory tmp-name1) tmp-name2))
	   (tmp-name4 (expand-file-name "foo" tmp-name1))
	   (tmp-name5 (expand-file-name "foo" tmp-name2))
	   (tmp-name6 (expand-file-name "foo" tmp-name3)))

      ;; Copy complete directory.
      (unwind-protect
	  (progn
	    ;; Copy empty directory.
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name4)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name4))
	    (copy-directory tmp-name1 tmp-name2)
	    (should (file-directory-p tmp-name2))
	    (should (file-exists-p tmp-name5))
	    ;; Target directory does exist already.
	    ;; This has been changed in Emacs 26.1.
	    (when (tramp--test-emacs26-p)
	      (should-error
	       (copy-directory tmp-name1 tmp-name2)
	       :type 'file-error))
	    (copy-directory tmp-name1 (file-name-as-directory tmp-name2))
	    (should (file-directory-p tmp-name3))
	    (should (file-exists-p tmp-name6)))

	;; Cleanup.
	(ignore-errors
	  (delete-directory tmp-name1 'recursive)
	  (delete-directory tmp-name2 'recursive)))

      ;; Copy directory contents.
      (unwind-protect
	  (progn
	    ;; Copy empty directory.
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name4)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name4))
	    (copy-directory tmp-name1 tmp-name2 nil 'parents 'contents)
	    (should (file-directory-p tmp-name2))
	    (should (file-exists-p tmp-name5))
	    ;; Target directory does exist already.
	    (delete-file tmp-name5)
	    (should-not (file-exists-p tmp-name5))
	    (copy-directory
	     tmp-name1 (file-name-as-directory tmp-name2)
	     nil 'parents 'contents)
	    (should (file-directory-p tmp-name2))
	    (should (file-exists-p tmp-name5))
	    (should-not (file-directory-p tmp-name3))
	    (should-not (file-exists-p tmp-name6)))

	;; Cleanup.
	(ignore-errors
	  (delete-directory tmp-name1 'recursive)
	  (delete-directory tmp-name2 'recursive))))))

(ert-deftest tramp-test16-directory-files ()
  "Check `directory-files'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "bla" tmp-name1))
	   (tmp-name3 (expand-file-name "foo" tmp-name1)))
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name2)
	    (write-region "bla" nil tmp-name3)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name2))
	    (should (file-exists-p tmp-name3))
	    (should (equal (directory-files tmp-name1) '("." ".." "bla" "foo")))
	    (should (equal (directory-files tmp-name1 'full)
			   `(,(concat tmp-name1 "/.")
			     ,(concat tmp-name1 "/..")
			     ,tmp-name2 ,tmp-name3)))
	    (should (equal (directory-files
			    tmp-name1 nil directory-files-no-dot-files-regexp)
			   '("bla" "foo")))
	    (should (equal (directory-files
			    tmp-name1 'full directory-files-no-dot-files-regexp)
			   `(,tmp-name2 ,tmp-name3))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

;; This is not a file name handler test.  But Tramp needed to apply an
;; advice for older Emacs versions, so we check that this has been fixed.
(ert-deftest tramp-test16-file-expand-wildcards ()
  "Check `file-expand-wildcards'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "foo" tmp-name1))
	   (tmp-name3 (expand-file-name "bar" tmp-name1))
	   (tmp-name4 (expand-file-name "baz" tmp-name1))
	   (default-directory tmp-name1))
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name2)
	    (write-region "bar" nil tmp-name3)
	    (write-region "baz" nil tmp-name4)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name2))
	    (should (file-exists-p tmp-name3))
	    (should (file-exists-p tmp-name4))

	    ;; `sort' works destructive.
	    (should
	     (equal (file-expand-wildcards "*")
		    (sort (copy-sequence '("foo" "bar" "baz")) 'string<)))
	    (should
	     (equal (file-expand-wildcards "ba?")
		    (sort (copy-sequence '("bar" "baz")) 'string<)))
	    (should
	     (equal (file-expand-wildcards "ba[rz]")
		    (sort (copy-sequence '("bar" "baz")) 'string<)))

	    (should
	     (equal
	      (file-expand-wildcards "*" 'full)
	      (sort
	       (copy-sequence `(,tmp-name2 ,tmp-name3 ,tmp-name4)) 'string<)))
	    (should
	     (equal
	      (file-expand-wildcards "ba?" 'full)
	      (sort (copy-sequence `(,tmp-name3 ,tmp-name4)) 'string<)))
	    (should
	     (equal
	      (file-expand-wildcards "ba[rz]" 'full)
	      (sort (copy-sequence `(,tmp-name3 ,tmp-name4)) 'string<)))

	    (should
	     (equal
	      (file-expand-wildcards (concat tmp-name1 "/" "*"))
	      (sort
	       (copy-sequence `(,tmp-name2 ,tmp-name3 ,tmp-name4)) 'string<)))
	    (should
	     (equal
	      (file-expand-wildcards (concat tmp-name1 "/" "ba?"))
	      (sort (copy-sequence `(,tmp-name3 ,tmp-name4)) 'string<)))
	    (should
	     (equal
	      (file-expand-wildcards (concat tmp-name1 "/" "ba[rz]"))
	      (sort (copy-sequence `(,tmp-name3 ,tmp-name4)) 'string<))))

	;; Cleanup.
	(ignore-errors
	  (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test17-insert-directory ()
  "Check `insert-directory'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name1
            (expand-file-name (tramp--test-make-temp-name nil quoted)))
	   (tmp-name2 (expand-file-name "foo" tmp-name1))
	   ;; We test for the summary line.  Keyword "total" could be localized.
	   (process-environment
	    (append '("LANG=C" "LANGUAGE=C" "LC_ALL=C") process-environment)))
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name2)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name2))
	    (with-temp-buffer
	      (insert-directory tmp-name1 nil)
	      (goto-char (point-min))
	      (should (looking-at-p (regexp-quote tmp-name1))))
	    ;; This has been fixed in Emacs 26.1.  See Bug#29423.
	    (when (tramp--test-emacs26-p)
	      (with-temp-buffer
	        (insert-directory (file-name-as-directory tmp-name1) nil)
	        (goto-char (point-min))
	        (should
                 (looking-at-p
                  (regexp-quote (file-name-as-directory tmp-name1))))))
	    (with-temp-buffer
	      (insert-directory tmp-name1 "-al")
	      (goto-char (point-min))
	      (should
	       (looking-at-p (format "^.+ %s$" (regexp-quote tmp-name1)))))
	    (with-temp-buffer
	      (insert-directory (file-name-as-directory tmp-name1) "-al")
	      (goto-char (point-min))
	      (should
	       (looking-at-p (format "^.+ %s/$" (regexp-quote tmp-name1)))))
	    (with-temp-buffer
	      (insert-directory
	       (file-name-as-directory tmp-name1) "-al" nil 'full-directory-p)
	      (goto-char (point-min))
	      (should
	       (looking-at-p
		(concat
		 ;; There might be a summary line.
		 "\\(total.+[[:digit:]]+\n\\)?"
		 ;; We don't know in which order ".", ".." and "foo" appear.
		 (format
		  "\\(.+ %s\\( ->.+\\)?\n\\)\\{%d\\}"
		  (regexp-opt (directory-files tmp-name1))
		  (length (directory-files tmp-name1))))))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test17-dired-with-wildcards ()
  "Check `dired' with wildcards."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-rsync-p)))
  ;; Since Emacs 26.1.
  (skip-unless (fboundp 'insert-directory-wildcard-in-dir-p))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name1
	    (expand-file-name (tramp--test-make-temp-name nil quoted)))
	   (tmp-name2
            (expand-file-name (tramp--test-make-temp-name nil quoted)))
	   (tmp-name3 (expand-file-name "foo" tmp-name1))
	   (tmp-name4 (expand-file-name "bar" tmp-name2))
	   (tramp-test-temporary-file-directory
	    (funcall
	     (if quoted 'tramp-compat-file-name-quote 'identity)
	     tramp-test-temporary-file-directory))
	   buffer)
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name3)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name3))
	    (make-directory tmp-name2)
	    (write-region "foo" nil tmp-name4)
	    (should (file-directory-p tmp-name2))
	    (should (file-exists-p tmp-name4))

	    ;; Check for expanded directory names.
	    (with-current-buffer
		(setq buffer
		      (dired-noselect
		       (expand-file-name
			"tramp-test*" tramp-test-temporary-file-directory)))
	      (goto-char (point-min))
	      (should
	       (re-search-forward
		(regexp-quote
		 (file-relative-name
		  tmp-name1 tramp-test-temporary-file-directory))))
	      (goto-char (point-min))
	      (should
	       (re-search-forward
		(regexp-quote
		 (file-relative-name
		  tmp-name2 tramp-test-temporary-file-directory)))))
	    (kill-buffer buffer)

	    ;; Check for expanded directory and file names.
	    (with-current-buffer
		(setq buffer
		      (dired-noselect
		       (expand-file-name
			"tramp-test*/*" tramp-test-temporary-file-directory)))
	      (goto-char (point-min))
	      (should
	       (re-search-forward
		(regexp-quote
		 (file-relative-name
		  tmp-name3 tramp-test-temporary-file-directory))))
	      (goto-char (point-min))
	      (should
	       (re-search-forward
		(regexp-quote
		 (file-relative-name
		  tmp-name4
		  tramp-test-temporary-file-directory)))))
	    (kill-buffer buffer)

	    ;; Check for special characters.
	    (setq tmp-name3 (expand-file-name "*?" tmp-name1))
	    (setq tmp-name4 (expand-file-name "[a-z0-9]" tmp-name2))
	    (write-region "foo" nil tmp-name3)
	    (should (file-exists-p tmp-name3))
	    (write-region "foo" nil tmp-name4)
	    (should (file-exists-p tmp-name4))

	    (with-current-buffer
		(setq buffer
		      (dired-noselect
		       (expand-file-name
			"tramp-test*/*" tramp-test-temporary-file-directory)))
	      (goto-char (point-min))
	      (should
	       (re-search-forward
		(regexp-quote
		 (file-relative-name
		  tmp-name3 tramp-test-temporary-file-directory))))
	      (goto-char (point-min))
	      (should
	       (re-search-forward
		(regexp-quote
		 (file-relative-name
		  tmp-name4
		  tramp-test-temporary-file-directory)))))
	    (kill-buffer buffer))

	;; Cleanup.
	(ignore-errors (kill-buffer buffer))
	(ignore-errors (delete-directory tmp-name1 'recursive))
	(ignore-errors (delete-directory tmp-name2 'recursive))))))

;; Method "smb" supports `make-symbolic-link' only if the remote host
;; has CIFS capabilities.  tramp-adb.el and tramp-gvfs.el do not
;; support symbolic links at all.
(defmacro tramp--test-ignore-make-symbolic-link-error (&rest body)
  "Run BODY, ignoring \"make-symbolic-link not supported\" file error."
  (declare (indent defun) (debug t))
  `(condition-case err
       (progn ,@body)
     ((error quit debug)
      (unless (and (eq (car err) 'file-error)
		   (string-equal (error-message-string err)
				 "make-symbolic-link not supported"))
	(signal (car err) (cdr err))))))

(ert-deftest tramp-test18-file-attributes ()
  "Check `file-attributes'.
This tests also `file-readable-p', `file-regular-p' and
`file-ownership-preserved-p'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory,
    ;; because it could be located on a symlinked directory.  This
    ;; would let the test fail.
    (let* ((tramp-test-temporary-file-directory
	    (file-truename tramp-test-temporary-file-directory))
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name nil quoted))
	   ;; File name with "//".
	   (tmp-name3
	    (format
	     "%s%s"
	     (file-remote-p tmp-name1)
	     (replace-regexp-in-string
	      "/" "//" (file-remote-p tmp-name1 'localname))))
	   attr)
      (unwind-protect
	  (progn
	    ;; `file-ownership-preserved-p' should return t for
	    ;; non-existing files.  It is implemented only in tramp-sh.el.
	    (when (tramp--test-sh-p)
	      (should (file-ownership-preserved-p tmp-name1 'group)))
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-readable-p tmp-name1))
	    (should (file-regular-p tmp-name1))
	    (when (tramp--test-sh-p)
	      (should (file-ownership-preserved-p tmp-name1 'group)))

	    ;; We do not test inodes and device numbers.
	    (setq attr (file-attributes tmp-name1))
	    (should (consp attr))
	    (should (null (car attr)))
	    (should (numberp (nth 1 attr))) ;; Link.
	    (should (numberp (nth 2 attr))) ;; Uid.
	    (should (numberp (nth 3 attr))) ;; Gid.
	    ;; Last access time.
	    (should (stringp (current-time-string (nth 4 attr))))
	    ;; Last modification time.
	    (should (stringp (current-time-string (nth 5 attr))))
	    ;; Last status change time.
	    (should (stringp (current-time-string (nth 6 attr))))
	    (should (numberp (nth 7 attr))) ;; Size.
	    (should (stringp (nth 8 attr))) ;; Modes.

	    (setq attr (file-attributes tmp-name1 'string))
	    (should (stringp (nth 2 attr))) ;; Uid.
	    (should (stringp (nth 3 attr))) ;; Gid.

	    (tramp--test-ignore-make-symbolic-link-error
	      (when (tramp--test-sh-p)
		(should (file-ownership-preserved-p tmp-name2 'group)))
	      (make-symbolic-link tmp-name1 tmp-name2)
	      (should (file-exists-p tmp-name2))
	      (should (file-symlink-p tmp-name2))
	      (when (tramp--test-sh-p)
		(should (file-ownership-preserved-p tmp-name2 'group)))
	      (setq attr (file-attributes tmp-name2))
	      (should
	       (string-equal
		(funcall
		 (if quoted 'tramp-compat-file-name-quote 'identity)
		 (car attr))
		(file-remote-p (file-truename tmp-name1) 'localname)))
	      (delete-file tmp-name2))

	    ;; Check, that "//" in symlinks are handled properly.
	    (with-temp-buffer
	      (let ((default-directory tramp-test-temporary-file-directory))
		(shell-command
		 (format
		  "ln -s %s %s"
		  (tramp-file-name-localname
		   (tramp-dissect-file-name tmp-name3))
		  (tramp-file-name-localname
		   (tramp-dissect-file-name tmp-name2)))
		 t)))
	    (when (file-symlink-p tmp-name2)
	      (setq attr (file-attributes tmp-name2))
	      (should
	       (string-equal
		(car attr)
		(tramp-file-name-localname
		 (tramp-dissect-file-name tmp-name3))))
	      (delete-file tmp-name2))

	    (when (tramp--test-sh-p)
	      (should (file-ownership-preserved-p tmp-name1 'group)))
	    (delete-file tmp-name1)
	    (make-directory tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-readable-p tmp-name1))
	    (should-not (file-regular-p tmp-name1))
	    (when (tramp--test-sh-p)
	      (should (file-ownership-preserved-p tmp-name1 'group)))
	    (setq attr (file-attributes tmp-name1))
	    (should (eq (car attr) t)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1))
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name2))))))

(ert-deftest tramp-test19-directory-files-and-attributes ()
  "Check `directory-files-and-attributes'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    ;; `directory-files-and-attributes' contains also values for
    ;; "../".  Ensure that this doesn't change during tests, for
    ;; example due to handling temporary files.
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "bla" tmp-name1))
	   attr)
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (should (file-directory-p tmp-name1))
	    (make-directory tmp-name2)
	    (should (file-directory-p tmp-name2))
	    (write-region "foo" nil (expand-file-name "foo" tmp-name2))
	    (write-region "bar" nil (expand-file-name "bar" tmp-name2))
	    (write-region "boz" nil (expand-file-name "boz" tmp-name2))
	    (setq attr (directory-files-and-attributes tmp-name2))
	    (should (consp attr))
	    ;; Dumb remote shells without perl(1) or stat(1) are not
	    ;; able to return the date correctly.  They say "don't know".
	    (dolist (elt attr)
	      (unless
		  (equal
		   (nth
		    5 (file-attributes (expand-file-name (car elt) tmp-name2)))
		   '(0 0))
		(should
		 (equal (file-attributes (expand-file-name (car elt) tmp-name2))
			(cdr elt)))))
	    (setq attr (directory-files-and-attributes tmp-name2 'full))
	    (dolist (elt attr)
	      (unless (equal (nth 5 (file-attributes (car elt))) '(0 0))
		(should
		 (equal (file-attributes (car elt)) (cdr elt)))))
	    (setq attr (directory-files-and-attributes tmp-name2 nil "^b"))
	    (should (equal (mapcar 'car attr) '("bar" "boz"))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test20-file-modes ()
  "Check `file-modes'.
This tests also `file-executable-p', `file-writable-p' and `set-file-modes'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (set-file-modes tmp-name #o777)
	    (should (= (file-modes tmp-name) #o777))
	    (should (file-executable-p tmp-name))
	    (should (file-writable-p tmp-name))
	    (set-file-modes tmp-name #o444)
	    (should (= (file-modes tmp-name) #o444))
	    (should-not (file-executable-p tmp-name))
	    ;; A file is always writable for user "root".
	    (unless (zerop (nth 2 (file-attributes tmp-name)))
	      (should-not (file-writable-p tmp-name))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

(ert-deftest tramp-test21-file-links ()
  "Check `file-symlink-p'.
This tests also `make-symbolic-link', `file-truename' and `add-name-to-file'."
  (skip-unless (tramp--test-enabled))
  ;; The semantics has changed heavily in Emacs 26.1.  We cannot test
  ;; older Emacsen, therefore.
  (skip-unless (tramp--test-emacs26-p))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory,
    ;; because it could be located on a symlinked directory.  This
    ;; would let the test fail.
    (let* ((tramp-test-temporary-file-directory
	    (file-truename tramp-test-temporary-file-directory))
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name nil quoted))
	   (tmp-name3 (tramp--test-make-temp-name 'local quoted))
	   (tmp-name4 (tramp--test-make-temp-name nil quoted))
	   (tmp-name5
	    (expand-file-name (file-name-nondirectory tmp-name1) tmp-name4)))
      ;; Check `make-symbolic-link'.
      (unwind-protect
	  (tramp--test-ignore-make-symbolic-link-error
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (make-symbolic-link tmp-name1 tmp-name2)
	    (should
	     (string-equal
	      (funcall
	       (if quoted 'tramp-compat-file-name-unquote 'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name2)))
	    (when (tramp--test-expensive-test)
	      (should-error
	       (make-symbolic-link tmp-name1 tmp-name2)
	       :type 'file-already-exists))
	    (when (tramp--test-expensive-test)
	      ;; A number means interactive case.
	      (cl-letf (((symbol-function 'yes-or-no-p) 'ignore))
		(should-error
		 (make-symbolic-link tmp-name1 tmp-name2 0)
		 :type 'file-already-exists)))
	    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
	      (make-symbolic-link tmp-name1 tmp-name2 0)
	      (should
	       (string-equal
		(funcall
		 (if quoted 'tramp-compat-file-name-unquote 'identity)
		 (file-remote-p tmp-name1 'localname))
		(file-symlink-p tmp-name2))))
	    (make-symbolic-link tmp-name1 tmp-name2 'ok-if-already-exists)
	    (should
	     (string-equal
	      (funcall
	       (if quoted 'tramp-compat-file-name-unquote 'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name2)))
	    ;; If we use the local part of `tmp-name1', it shall still work.
	    (make-symbolic-link
	     (file-remote-p tmp-name1 'localname)
	     tmp-name2 'ok-if-already-exists)
	    (should
	     (string-equal
	      (funcall
	       (if quoted 'tramp-compat-file-name-unquote 'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name2)))
	    ;; `tmp-name3' is a local file name.  Therefore, the link
	    ;; target remains unchanged, even if quoted.
	    ;; `make-symbolic-link' might not be permitted on w32 systems.
	    (unless (tramp--test-windows-nt)
	      (make-symbolic-link tmp-name1 tmp-name3)
	      (should
	       (string-equal tmp-name1 (file-symlink-p tmp-name3))))
	    ;; Check directory as newname.
	    (make-directory tmp-name4)
	    (when (tramp--test-expensive-test)
	      (should-error
	       (make-symbolic-link tmp-name1 tmp-name4)
	       :type 'file-already-exists))
	    (make-symbolic-link tmp-name1 (file-name-as-directory tmp-name4))
	    (should
	     (string-equal
	      (funcall
	       (if quoted 'tramp-compat-file-name-unquote 'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name5)))
	    ;; `smbclient' does not show symlinks in directories, so
	    ;; we cannot delete a non-empty directory.  We delete the
	    ;; file explicitly.
	    (delete-file tmp-name5))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2)
	  (delete-file tmp-name3)
	  (delete-directory tmp-name4 'recursive)))

      ;; Check `add-name-to-file'.
      (unwind-protect
	  (when (tramp--test-expensive-test)
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (add-name-to-file tmp-name1 tmp-name2)
	    (should (file-regular-p tmp-name2))
	    (should-error
	     (add-name-to-file tmp-name1 tmp-name2)
	     :type 'file-already-exists)
	    ;; A number means interactive case.
	    (cl-letf (((symbol-function 'yes-or-no-p) 'ignore))
	      (should-error
	       (add-name-to-file tmp-name1 tmp-name2 0)
	       :type 'file-already-exists))
	    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
	       (add-name-to-file tmp-name1 tmp-name2 0)
	       (should (file-regular-p tmp-name2)))
	    (add-name-to-file tmp-name1 tmp-name2 'ok-if-already-exists)
	    (should-not (file-symlink-p tmp-name2))
	    (should (file-regular-p tmp-name2))
	    ;; `tmp-name3' is a local file name.
	    (should-error
	     (add-name-to-file tmp-name1 tmp-name3)
	     :type 'file-error)
	    ;; Check directory as newname.
	    (make-directory tmp-name4)
	    (should-error
	     (add-name-to-file tmp-name1 tmp-name4)
	     :type 'file-already-exists)
	    (add-name-to-file tmp-name1 (file-name-as-directory tmp-name4))
	    (should
	     (file-regular-p
	      (expand-file-name (file-name-nondirectory tmp-name1) tmp-name4))))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2)
	  (delete-directory tmp-name4 'recursive)))

      ;; Check `file-truename'.
      (unwind-protect
	  (tramp--test-ignore-make-symbolic-link-error
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (string-equal tmp-name1 (file-truename tmp-name1)))
	    (make-symbolic-link tmp-name1 tmp-name2)
	    (should (file-symlink-p tmp-name2))
	    (should-not (string-equal tmp-name2 (file-truename tmp-name2)))
	    (should
	     (string-equal (file-truename tmp-name1) (file-truename tmp-name2)))
	    (should (file-equal-p tmp-name1 tmp-name2))
	    ;; Check relative symlink file name.
	    (delete-file tmp-name2)
	    (let ((default-directory tramp-test-temporary-file-directory))
	      (make-symbolic-link (file-name-nondirectory tmp-name1) tmp-name2))
	    (should (file-symlink-p tmp-name2))
	    (should-not (string-equal tmp-name2 (file-truename tmp-name2)))
	    (should
	     (string-equal (file-truename tmp-name1) (file-truename tmp-name2)))
	    (should (file-equal-p tmp-name1 tmp-name2))
	    ;; Symbolic links could look like a remote file name.
	    ;; They must be quoted then.
	    (delete-file tmp-name2)
	    (make-symbolic-link
	     (funcall
	      (if quoted 'tramp-compat-file-name-unquote 'identity)
	      "/penguin:motd:")
	     tmp-name2)
	    (should (file-symlink-p tmp-name2))
	    (should
	     (string-equal
	      (file-truename tmp-name2)
	      (tramp-compat-file-name-quote
	       (concat (file-remote-p tmp-name2) "/penguin:motd:"))))
	    ;; `tmp-name3' is a local file name.
	    ;; `make-symbolic-link' might not be permitted on w32 systems.
	    (unless (tramp--test-windows-nt)
	      (make-symbolic-link tmp-name1 tmp-name3)
	      (should (file-symlink-p tmp-name3))
              (should-not (string-equal tmp-name3 (file-truename tmp-name3)))
	      ;; `file-truename' returns a quoted file name for `tmp-name3'.
	      ;; We must unquote it.
	      (should
	       (string-equal
		(funcall
		 (if (tramp--test-emacs27-p)
		     'tramp-compat-file-name-unquote 'identity)
		 (file-truename tmp-name1))
		(tramp-compat-file-name-unquote (file-truename tmp-name3))))))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2)
	  (delete-file tmp-name3)))

      ;; Symbolic links could be nested.
      (unwind-protect
	  (tramp--test-ignore-make-symbolic-link-error
	    (make-directory tmp-name1)
	    (should (file-directory-p tmp-name1))
	    (let* ((tramp-test-temporary-file-directory
		    (file-truename tmp-name1))
		   (tmp-name2 (tramp--test-make-temp-name nil quoted))
		   (tmp-name3 tmp-name2)
		   (number-nesting 15))
	      (dotimes (_ number-nesting)
		(make-symbolic-link
		 tmp-name3
		 (setq tmp-name3 (tramp--test-make-temp-name nil quoted))))
	      (should
	       (string-equal
		(file-truename tmp-name2)
		(file-truename tmp-name3)))
	      (when (tramp--test-expensive-test)
		(should-error
		 (with-temp-buffer (insert-file-contents tmp-name2))
		 :type tramp-file-missing))
	      (when (tramp--test-expensive-test)
		(should-error
		 (with-temp-buffer (insert-file-contents tmp-name3))
		 :type tramp-file-missing))
	      ;; `directory-files' does not show symlinks to
	      ;; non-existing targets in the "smb" case.  So we remove
	      ;; the symlinks manually.
	      (while (stringp (setq tmp-name2 (file-symlink-p tmp-name3)))
		(delete-file tmp-name3)
		(setq tmp-name3 (concat (file-remote-p tmp-name3) tmp-name2)))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive)))

      ;; Detect cyclic symbolic links.
      (unwind-protect
	  (when (tramp--test-expensive-test)
	    (tramp--test-ignore-make-symbolic-link-error
	     (make-symbolic-link tmp-name2 tmp-name1)
	     (should (file-symlink-p tmp-name1))
	     (if (tramp-smb-file-name-p tramp-test-temporary-file-directory)
		 ;; The symlink command of `smbclient' detects the
		 ;; cycle already.
		 (should-error
		  (make-symbolic-link tmp-name1 tmp-name2)
		  :type 'file-error)
	       (make-symbolic-link tmp-name1 tmp-name2)
	       (should (file-symlink-p tmp-name2))
	       (should-error (file-truename tmp-name1) :type 'file-error))))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2)))

      ;; `file-truename' shall preserve trailing link of directories.
      (unless (file-symlink-p tramp-test-temporary-file-directory)
	(let* ((dir1 (directory-file-name tramp-test-temporary-file-directory))
	       (dir2 (file-name-as-directory dir1)))
	  (should (string-equal (file-truename dir1) (expand-file-name dir1)))
	  (should
	   (string-equal (file-truename dir2) (expand-file-name dir2))))))))

(ert-deftest tramp-test22-file-times ()
  "Check `set-file-times' and `file-newer-than-file-p'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (or (tramp--test-adb-p) (tramp--test-sh-p)))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (tmp-name3 (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (consp (nth 5 (file-attributes tmp-name1))))
	    ;; '(0 0) means don't know, and will be replaced by
	    ;; `current-time'.  Therefore, we use '(0 1).  We skip the
	    ;; test, if the remote handler is not able to set the
	    ;; correct time.
	    (skip-unless (set-file-times tmp-name1 '(0 1)))
	    ;; Dumb remote shells without perl(1) or stat(1) are not
	    ;; able to return the date correctly.  They say "don't know".
	    (unless (equal (nth 5 (file-attributes tmp-name1)) '(0 0))
	      (should (equal (nth 5 (file-attributes tmp-name1)) '(0 1)))
	      (write-region "bla" nil tmp-name2)
	      (should (file-exists-p tmp-name2))
	      (should (file-newer-than-file-p tmp-name2 tmp-name1))
	      ;; `tmp-name3' does not exist.
	      (should (file-newer-than-file-p tmp-name2 tmp-name3))
	      (should-not (file-newer-than-file-p tmp-name3 tmp-name1))))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2))))))

(ert-deftest tramp-test23-visited-file-modtime ()
  "Check `set-visited-file-modtime' and `verify-visited-file-modtime'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (verify-visited-file-modtime))
	      (set-visited-file-modtime '(0 1))
	      (should (verify-visited-file-modtime))
	      (should (equal (visited-file-modtime) '(0 1 0 0)))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

;; This test is inspired by Bug#29149.
(ert-deftest tramp-test24-file-acl ()
  "Check that `file-acl' and `set-file-acl' work proper."
  (skip-unless (tramp--test-enabled))
  (skip-unless (file-acl tramp-test-temporary-file-directory))

  ;; `filename-non-special' has been fixed in Emacs 27.1, see Bug#29579.
  (dolist (quoted (if (and (tramp--test-expensive-test) (tramp--test-emacs27-p))
		      '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (tmp-name3 (tramp--test-make-temp-name 'local quoted)))
      ;; Both files are remote.
      (unwind-protect
	  (progn
	    ;; Two files with same ACLs.
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-acl tmp-name1))
	    (copy-file tmp-name1 tmp-name2 nil nil nil 'preserve-permissions)
	    (should (file-acl tmp-name2))
	    (should (string-equal (file-acl tmp-name1) (file-acl tmp-name2)))
	    ;; Different permissions mean different ACLs.
	    (when (not (tramp--test-windows-nt-or-smb-p))
	      (set-file-modes tmp-name1 #o777)
	      (set-file-modes tmp-name2 #o444)
	      (should-not
	       (string-equal (file-acl tmp-name1) (file-acl tmp-name2))))
	    ;; Copy ACL.  Not all remote handlers support it, so we test.
	    (when (set-file-acl tmp-name2 (file-acl tmp-name1))
	      (should (string-equal (file-acl tmp-name1) (file-acl tmp-name2))))
	    ;; An invalid ACL does not harm.
	    (should-not (set-file-acl tmp-name2 "foo")))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name2)))

      ;; Remote and local file.
      (unwind-protect
	  (when (and (file-acl temporary-file-directory)
		     (not (tramp--test-windows-nt-or-smb-p)))
	    ;; Two files with same ACLs.
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-acl tmp-name1))
	    (copy-file tmp-name1 tmp-name3 nil nil nil 'preserve-permissions)
	    (should (file-acl tmp-name3))
	    (should (string-equal (file-acl tmp-name1) (file-acl tmp-name3)))
	    ;; Different permissions mean different ACLs.
	    (set-file-modes tmp-name1 #o777)
	    (set-file-modes tmp-name3 #o444)
	    (should-not
	     (string-equal (file-acl tmp-name1) (file-acl tmp-name3)))
	    ;; Copy ACL.  Since we don't know whether Emacs is built
	    ;; with local ACL support, we must check it.
	    (when (set-file-acl tmp-name3 (file-acl tmp-name1))
	      (should (string-equal (file-acl tmp-name1) (file-acl tmp-name3))))

	    ;; Two files with same ACLs.
	    (delete-file tmp-name1)
	    (copy-file tmp-name3 tmp-name1 nil nil nil 'preserve-permissions)
	    (should (file-acl tmp-name1))
	    (should (string-equal (file-acl tmp-name1) (file-acl tmp-name3)))
	    ;; Different permissions mean different ACLs.
	    (set-file-modes tmp-name1 #o777)
	    (set-file-modes tmp-name3 #o444)
	    (should-not
	     (string-equal (file-acl tmp-name1) (file-acl tmp-name3)))
	    ;; Copy ACL.
	    (set-file-acl tmp-name1 (file-acl tmp-name3))
	    (should (string-equal (file-acl tmp-name1) (file-acl tmp-name3))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name3))))))

(ert-deftest tramp-test25-file-selinux ()
  "Check `file-selinux-context' and `set-file-selinux-context'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not (equal (file-selinux-context tramp-test-temporary-file-directory)
	       '(nil nil nil nil))))

  ;; `filename-non-special' has been fixed in Emacs 27.1, see Bug#29579.
  (dolist (quoted (if (and (tramp--test-expensive-test) (tramp--test-emacs27-p))
		      '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (tmp-name3 (tramp--test-make-temp-name 'local quoted)))
      ;; Both files are remote.
      (unwind-protect
	  (progn
	    ;; Two files with same SELinux context.
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-selinux-context tmp-name1))
	    (copy-file tmp-name1 tmp-name2)
	    (should (file-selinux-context tmp-name2))
	    (should
	     (equal
	      (file-selinux-context tmp-name1)
	      (file-selinux-context tmp-name2)))
	    ;; Check different SELinux context.  We cannot support
	    ;; different ranges in this test; let's assume the most
	    ;; likely one.
	    (let ((context (file-selinux-context tmp-name1)))
	      (when (and (string-equal (nth 3 context) "s0")
			 (setcar (nthcdr 3 context) "s0:c0")
			 (set-file-selinux-context tmp-name1 context))
		(should-not
		 (equal
		  (file-selinux-context tmp-name1)
		  (file-selinux-context tmp-name2)))))
	    ;; Copy SELinux context.
	    (should
	     (set-file-selinux-context
	      tmp-name2 (file-selinux-context tmp-name1)))
	    (should
	     (equal
	      (file-selinux-context tmp-name1)
	      (file-selinux-context tmp-name2)))
	    ;; An invalid SELinux context does not harm.
	    (should-not (set-file-selinux-context tmp-name2 "foo")))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name2)))

      ;; Remote and local file.
      (unwind-protect
	  (when (and (not
		      (or (equal (file-selinux-context temporary-file-directory)
				 '(nil nil nil nil))
			  (tramp--test-windows-nt-or-smb-p)))
		     ;; Both users shall use the same SELinux context.
		     (string-equal
		      (let ((default-directory temporary-file-directory))
			(shell-command-to-string "id -Z"))
		      (let ((default-directory
			      tramp-test-temporary-file-directory))
			(shell-command-to-string "id -Z"))))

	    ;; Two files with same SELinux context.
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-selinux-context tmp-name1))
	    (copy-file tmp-name1 tmp-name3)
	    (should (file-selinux-context tmp-name3))
	    ;; We cannot expect that copying over file system
	    ;; boundaries keeps SELinux context.  So we copy it
	    ;; explicitly.
	    (should
	     (set-file-selinux-context
	      tmp-name3 (file-selinux-context tmp-name1)))
	    (should
	     (equal
	      (file-selinux-context tmp-name1)
	      (file-selinux-context tmp-name3)))
	    ;; Check different SELinux context.  We cannot support
	    ;; different ranges in this test; let's assume the most
	    ;; likely one.
	    (let ((context (file-selinux-context tmp-name1)))
	      (when (and (string-equal (nth 3 context) "s0")
			 (setcar (nthcdr 3 context) "s0:c0")
			 (set-file-selinux-context tmp-name1 context))
		(should-not
		 (equal
		  (file-selinux-context tmp-name1)
		  (file-selinux-context tmp-name3)))))
	    ;; Copy SELinux context.
	    (should
	     (set-file-selinux-context
	      tmp-name3 (file-selinux-context tmp-name1)))
	    (should
	     (equal
	      (file-selinux-context tmp-name1)
	      (file-selinux-context tmp-name3)))

	    ;; Two files with same SELinux context.
	    (delete-file tmp-name1)
	    (copy-file tmp-name3 tmp-name1)
	    (should (file-selinux-context tmp-name1))
	    ;; We cannot expect that copying over file system
	    ;; boundaries keeps SELinux context.  So we copy it
	    ;; explicitly.
	    (should
	     (set-file-selinux-context
	      tmp-name1 (file-selinux-context tmp-name3)))
	    (should
	     (equal
	      (file-selinux-context tmp-name1)
	      (file-selinux-context tmp-name3)))
	    ;; Check different SELinux context.  We cannot support
	    ;; different ranges in this test; let's assume the most
	    ;; likely one.
	    (let ((context (file-selinux-context tmp-name3)))
	      (when (and (string-equal (nth 3 context) "s0")
			 (setcar (nthcdr 3 context) "s0:c0")
			 (set-file-selinux-context tmp-name3 context))
		(should-not
		 (equal
		  (file-selinux-context tmp-name1)
		  (file-selinux-context tmp-name3)))))
	    ;; Copy SELinux context.
	    (should
	     (set-file-selinux-context
	      tmp-name1 (file-selinux-context tmp-name3)))
	    (should
	     (equal
	      (file-selinux-context tmp-name1)
	      (file-selinux-context tmp-name3))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name3))))))

(ert-deftest tramp-test26-file-name-completion ()
  "Check `file-name-completion' and `file-name-all-completions'."
  (skip-unless (tramp--test-enabled))

  ;; Method and host name in completion mode.  This kind of completion
  ;; does not work on MS Windows.
  (when (not (memq system-type '(cygwin windows-nt)))
    (let ((method (file-remote-p tramp-test-temporary-file-directory 'method))
	  (host (file-remote-p tramp-test-temporary-file-directory 'host))
          (orig-syntax tramp-syntax))
      (when (and (stringp host) (string-match tramp-host-with-port-regexp host))
	(setq host (match-string 1 host)))

      (unwind-protect
          (dolist
	      (syntax
	       (if (tramp--test-expensive-test)
		   (tramp-syntax-values) `(,orig-syntax)))
            (tramp-change-syntax syntax)
            (let ;; This is needed for the `simplified' syntax.
                ((method-marker
                  (if (zerop (length tramp-method-regexp))
                      "" tramp-default-method-marker))
                 ;; This is needed for the `separate' syntax.
                 (prefix-format (substring tramp-prefix-format 1)))
              ;; Complete method name.
	      (unless (or (zerop (length method))
                          (zerop (length tramp-method-regexp)))
	        (should
	         (member
		  (concat prefix-format method tramp-postfix-method-format)
		  (file-name-all-completions
                   (concat prefix-format (substring method 0 1)) "/"))))
              ;; Complete host name for default method.  With gvfs
              ;; based methods, host name will be determined as
              ;; host.local, so we omit the test.
	      (let ((tramp-default-method (or method tramp-default-method)))
		(unless (or (zerop (length host))
			    (tramp--test-gvfs-p tramp-default-method))
		  (should
		   (member
		    (concat
                     prefix-format method-marker tramp-postfix-method-format
                     host tramp-postfix-host-format)
		    (file-name-all-completions
		     (concat
                      prefix-format method-marker tramp-postfix-method-format
                      (substring host 0 1))
                     "/")))))
              ;; Complete host name.
	      (unless (or (zerop (length method))
                          (zerop (length tramp-method-regexp))
                          (zerop (length host))
			  (tramp--test-gvfs-p method))
	        (should
	         (member
		  (concat
                   prefix-format method tramp-postfix-method-format
                   host tramp-postfix-host-format)
		  (file-name-all-completions
		   (concat prefix-format method tramp-postfix-method-format)
                   "/"))))))

	;; Cleanup.
        (tramp-change-syntax orig-syntax))))

  (dolist (n-e '(nil t))
    (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
      (let ((non-essential n-e)
	    (tmp-name (tramp--test-make-temp-name nil quoted)))

	(unwind-protect
	    (progn
	      ;; Local files.
	      (make-directory tmp-name)
	      (should (file-directory-p tmp-name))
	      (write-region "foo" nil (expand-file-name "foo" tmp-name))
	      (should (file-exists-p (expand-file-name "foo" tmp-name)))
	      (write-region "bar" nil (expand-file-name "bold" tmp-name))
	      (should (file-exists-p (expand-file-name "bold" tmp-name)))
	      (make-directory (expand-file-name "boz" tmp-name))
	      (should (file-directory-p (expand-file-name "boz" tmp-name)))
	      (should (equal (file-name-completion "fo" tmp-name) "foo"))
	      (should (equal (file-name-completion "foo" tmp-name) t))
	      (should (equal (file-name-completion "b" tmp-name) "bo"))
	      (should-not (file-name-completion "a" tmp-name))
	      (should
	       (equal
		(file-name-completion "b" tmp-name 'file-directory-p) "boz/"))
	      (should
	       (equal (file-name-all-completions "fo" tmp-name) '("foo")))
	      (should
	       (equal
		(sort (file-name-all-completions "b" tmp-name) 'string-lessp)
		'("bold" "boz/")))
	      (should-not (file-name-all-completions "a" tmp-name))
	      ;; `completion-regexp-list' restricts the completion to
	      ;; files which match all expressions in this list.
	      (let ((completion-regexp-list
		     `(,directory-files-no-dot-files-regexp "b")))
		(should
		 (equal (file-name-completion "" tmp-name) "bo"))
		(should
		 (equal
		  (sort (file-name-all-completions "" tmp-name) 'string-lessp)
		  '("bold" "boz/"))))
	      ;; `file-name-completion' ignores file names that end in
	      ;; any string in `completion-ignored-extensions'.
	      (let ((completion-ignored-extensions '(".ext")))
		(write-region "foo" nil (expand-file-name "foo.ext" tmp-name))
		(should (file-exists-p (expand-file-name "foo.ext" tmp-name)))
		(should (equal (file-name-completion "fo" tmp-name) "foo"))
		(should (equal (file-name-completion "foo" tmp-name) t))
		(should
		 (equal (file-name-completion "foo." tmp-name) "foo.ext"))
		(should (equal (file-name-completion "foo.ext" tmp-name) t))
		;; `file-name-all-completions' is not affected.
		(should
		 (equal
		  (sort (file-name-all-completions "" tmp-name) 'string-lessp)
		  '("../" "./" "bold" "boz/" "foo" "foo.ext")))))

	  ;; Cleanup.
	  (ignore-errors (delete-directory tmp-name 'recursive)))))))

(ert-deftest tramp-test27-load ()
  "Check `load'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    (load tmp-name 'noerror 'nomessage)
	    (should-not (featurep 'tramp-test-load))
	    (write-region "(provide 'tramp-test-load)" nil tmp-name)
	    ;; `load' in lread.c does not pass `must-suffix'.  Why?
	    ;;(should-error
	    ;; (load tmp-name nil 'nomessage 'nosuffix 'must-suffix)
	    ;; :type 'file-error)
	    (load tmp-name nil 'nomessage 'nosuffix)
	    (should (featurep 'tramp-test-load)))

	;; Cleanup.
	(ignore-errors
	  (and (featurep 'tramp-test-load) (unload-feature 'tramp-test-load))
	  (delete-file tmp-name))))))

(ert-deftest tramp-test28-process-file ()
  "Check `process-file'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (or (tramp--test-adb-p) (tramp--test-sh-p)))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((tmp-name (tramp--test-make-temp-name nil quoted))
	   (fnnd (file-name-nondirectory tmp-name))
	   (default-directory tramp-test-temporary-file-directory)
	   kill-buffer-query-functions)
      (unwind-protect
	  (progn
	    ;; We cannot use "/bin/true" and "/bin/false"; those paths
	    ;; do not exist on hydra.
	    (should (zerop (process-file "true")))
	    (should-not (zerop (process-file "false")))
	    (should-not (zerop (process-file "binary-does-not-exist")))
	    (with-temp-buffer
	      (write-region "foo" nil tmp-name)
	      (should (file-exists-p tmp-name))
	      (should (zerop (process-file "ls" nil t nil fnnd)))
	      ;; `ls' could produce colorized output.
	      (goto-char (point-min))
	      (while
		  (re-search-forward tramp-display-escape-sequence-regexp nil t)
		(replace-match "" nil nil))
	      (should (string-equal (format "%s\n" fnnd) (buffer-string)))
	      (should-not (get-buffer-window (current-buffer) t))

	      ;; Second run. The output must be appended.
	      (goto-char (point-max))
	      (should (zerop (process-file "ls" nil t t fnnd)))
	      ;; `ls' could produce colorized output.
	      (goto-char (point-min))
	      (while
		  (re-search-forward tramp-display-escape-sequence-regexp nil t)
		(replace-match "" nil nil))
	      (should
	       (string-equal (format "%s\n%s\n" fnnd fnnd) (buffer-string)))
	      ;; A non-nil DISPLAY must not raise the buffer.
	      (should-not (get-buffer-window (current-buffer) t))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

(ert-deftest tramp-test29-start-file-process ()
  "Check `start-file-process'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((default-directory tramp-test-temporary-file-directory)
	  (tmp-name (tramp--test-make-temp-name nil quoted))
	  kill-buffer-query-functions proc)
      (unwind-protect
	  (with-temp-buffer
	    (setq proc (start-file-process "test1" (current-buffer) "cat"))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (process-send-string proc "foo")
	    (process-send-eof proc)
	    ;; Read output.
	    (with-timeout (10 (ert-fail "`start-file-process' timed out"))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(accept-process-output proc 0.1)))
	    (should (string-equal (buffer-string) "foo")))

	;; Cleanup.
	(ignore-errors (delete-process proc)))

      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (setq proc
		  (start-file-process
		   "test2" (current-buffer)
		   "cat" (file-name-nondirectory tmp-name)))
	    (should (processp proc))
	    ;; Read output.
	    (with-timeout (10 (ert-fail "`start-file-process' timed out"))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(accept-process-output proc 0.1)))
	    (should (string-equal (buffer-string) "foo")))

	;; Cleanup.
	(ignore-errors
	  (delete-process proc)
	  (delete-file tmp-name)))

      (unwind-protect
	  (with-temp-buffer
	    (setq proc (start-file-process "test3" (current-buffer) "cat"))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (set-process-filter
	     proc
	     (lambda (p s) (with-current-buffer (process-buffer p) (insert s))))
	    (process-send-string proc "foo")
	    (process-send-eof proc)
	    ;; Read output.
	    (with-timeout (10 (ert-fail "`start-file-process' timed out"))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(accept-process-output proc 0.1)))
	    (should (string-equal (buffer-string) "foo")))

	;; Cleanup.
	(ignore-errors (delete-process proc))))))

(ert-deftest tramp-test30-interrupt-process ()
  "Check `interrupt-process'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  ;; Since Emacs 26.1.
  (skip-unless (boundp 'interrupt-process-functions))

  (let ((default-directory tramp-test-temporary-file-directory)
	kill-buffer-query-functions proc)
    (unwind-protect
	(with-temp-buffer
	  (setq proc (start-file-process "test" (current-buffer) "sleep" "10"))
	  (should (processp proc))
	  (should (process-live-p proc))
	  (should (equal (process-status proc) 'run))
	  (should (numberp (process-get proc 'remote-pid)))
	  (should (interrupt-process proc))
	  ;; Let the process accept the interrupt.
          (accept-process-output proc 1 nil 0)
	  (should-not (process-live-p proc))
	  ;; An interrupted process cannot be interrupted, again.
	  (should-error (interrupt-process proc) :type 'error))

      ;; Cleanup.
      (ignore-errors (delete-process proc)))))

(ert-deftest tramp-test31-shell-command ()
  "Check `shell-command'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted))
	  (default-directory tramp-test-temporary-file-directory)
	  ;; Suppress nasty messages.
	  (inhibit-message t)
	  kill-buffer-query-functions)
      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (shell-command
	     (format "ls %s" (file-name-nondirectory tmp-name))
	     (current-buffer))
	    ;; `ls' could produce colorized output.
	    (goto-char (point-min))
	    (while
		(re-search-forward tramp-display-escape-sequence-regexp nil t)
	      (replace-match "" nil nil))
	    (should
	     (string-equal
	      (format "%s\n" (file-name-nondirectory tmp-name))
	      (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name)))

      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (async-shell-command
	     (format "ls %s" (file-name-nondirectory tmp-name))
	     (current-buffer))
	    ;; Read output.
	    (with-timeout (10 (ert-fail "`async-shell-command' timed out"))
	      (while (< (- (point-max) (point-min))
			(1+ (length (file-name-nondirectory tmp-name))))
		(accept-process-output
		 (get-buffer-process (current-buffer)) 0.1)))
	    ;; `ls' could produce colorized output.
	    (goto-char (point-min))
	    (while
		(re-search-forward tramp-display-escape-sequence-regexp nil t)
	      (replace-match "" nil nil))
	    ;; There might be a nasty "Process *Async Shell* finished" message.
	    (goto-char (point-min))
	    (forward-line)
	    (narrow-to-region (point-min) (point))
	    (should
	     (string-equal
	      (format "%s\n" (file-name-nondirectory tmp-name))
	      (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name)))

      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (async-shell-command "read line; ls $line" (current-buffer))
	    (process-send-string
	     (get-buffer-process (current-buffer))
	     (format "%s\n" (file-name-nondirectory tmp-name)))
	    ;; Read output.
	    (with-timeout (10 (ert-fail "`async-shell-command' timed out"))
	      (while (< (- (point-max) (point-min))
			(1+ (length (file-name-nondirectory tmp-name))))
		(accept-process-output
		 (get-buffer-process (current-buffer)) 0.1)))
	    ;; `ls' could produce colorized output.
	    (goto-char (point-min))
	    (while
		(re-search-forward tramp-display-escape-sequence-regexp nil t)
	      (replace-match "" nil nil))
	    ;; There might be a nasty "Process *Async Shell* finished" message.
	    (goto-char (point-min))
	    (forward-line)
	    (narrow-to-region (point-min) (point))
	    (should
	     (string-equal
	      (format "%s\n" (file-name-nondirectory tmp-name))
	      (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

(defun tramp--test-shell-command-to-string-asynchronously (command)
  "Like `shell-command-to-string', but for asynchronous processes."
  (with-temp-buffer
    (async-shell-command command (current-buffer))
    (with-timeout (10)
      (while (get-buffer-process (current-buffer))
	(accept-process-output (get-buffer-process (current-buffer)) 0.1)))
    (accept-process-output nil 0.1)
    (buffer-substring-no-properties (point-min) (point-max))))

;; This test is inspired by Bug#23952.
(ert-deftest tramp-test32-environment-variables ()
  "Check that remote processes set / unset environment variables properly."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  (dolist (this-shell-command-to-string
	   '(;; Synchronously.
	     shell-command-to-string
	     ;; Asynchronously.
	     tramp--test-shell-command-to-string-asynchronously))

    (let ((default-directory tramp-test-temporary-file-directory)
	  (shell-file-name "/bin/sh")
	  (envvar (concat "VAR_" (upcase (md5 (current-time-string)))))
	  kill-buffer-query-functions)

      (unwind-protect
	  ;; Set a value.
	  (let ((process-environment
		 (cons (concat envvar "=foo") process-environment)))
	    ;; Default value.
	    (should
	     (string-match
	      "foo"
	      (funcall
	       this-shell-command-to-string
	       (format "echo -n ${%s:?bla}" envvar))))))

      (unwind-protect
	  ;; Set the empty value.
	  (let ((process-environment
		 (cons (concat envvar "=") process-environment)))
	    ;; Value is null.
	    (should
	     (string-match
	      "bla"
	      (funcall
	       this-shell-command-to-string
	       (format "echo -n ${%s:?bla}" envvar))))
	    ;; Variable is set.
	    (should
	     (string-match
	      (regexp-quote envvar)
	      (funcall this-shell-command-to-string "set")))))

      ;; We force a reconnect, in order to have a clean environment.
      (tramp-cleanup-connection
       (tramp-dissect-file-name tramp-test-temporary-file-directory)
       'keep-debug 'keep-password)
      (unwind-protect
	  ;; Unset the variable.
	  (let ((tramp-remote-process-environment
		 (cons (concat envvar "=foo")
		       tramp-remote-process-environment)))
	    ;; Set the initial value, we want to unset below.
	    (should
	     (string-match
	      "foo"
	      (funcall
	       this-shell-command-to-string
	       (format "echo -n ${%s:?bla}" envvar))))
	    (let ((process-environment
		   (cons envvar process-environment)))
	      ;; Variable is unset.
	      (should
	       (string-match
		"bla"
		(funcall
		 this-shell-command-to-string
		 (format "echo -n ${%s:?bla}" envvar))))
	      ;; Variable is unset.
	      (should-not
	       (string-match
		(regexp-quote envvar)
		(funcall this-shell-command-to-string "set")))))))))

;; This test is inspired by Bug#27009.
(ert-deftest tramp-test32-environment-variables-and-port-numbers ()
  "Check that two connections with separate ports are different."
  (skip-unless (tramp--test-enabled))
  ;; We test it only for the mock-up connection; otherwise there might
  ;; be problems with the used ports.
  (skip-unless (and (eq tramp-syntax 'default)
                    (tramp--test-mock-p)))

  ;; We force a reconnect, in order to have a clean environment.
  (dolist (dir `(,tramp-test-temporary-file-directory
		 "/mock:localhost#11111:" "/mock:localhost#22222:"))
    (tramp-cleanup-connection
     (tramp-dissect-file-name dir) 'keep-debug 'keep-password))

  (unwind-protect
      (dolist (port '(11111 22222))
	(let* ((default-directory
		 (format "/mock:localhost#%d:%s" port temporary-file-directory))
	       (shell-file-name "/bin/sh")
	       (envvar (concat "VAR_" (upcase (md5 (current-time-string)))))
	       ;; We cannot use `process-environment', because this
	       ;; would be applied in `process-file'.
	       (tramp-remote-process-environment
		(cons
		 (format "%s=%d" envvar port)
		 tramp-remote-process-environment)))
	  (should
	   (string-match
	    (number-to-string port)
	    (shell-command-to-string (format "echo -n $%s" envvar))))))

    ;; Cleanup.
    (dolist (dir '("/mock:localhost#11111:" "/mock:localhost#22222:"))
      (tramp-cleanup-connection (tramp-dissect-file-name dir)))))

;; The functions were introduced in Emacs 26.1.
(ert-deftest tramp-test33-explicit-shell-file-name ()
  "Check that connection-local `explicit-shell-file-name' is set."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  ;; Since Emacs 26.1.
  (skip-unless (and (fboundp 'connection-local-set-profile-variables)
		    (fboundp 'connection-local-set-profiles)))

  ;; `connection-local-set-profile-variables' and
  ;; `connection-local-set-profiles' exist since Emacs 26.1.  We don't
  ;; want to see compiler warnings for older Emacsen.
  (let ((default-directory tramp-test-temporary-file-directory)
	explicit-shell-file-name kill-buffer-query-functions)
    (unwind-protect
	(progn
	  ;; `shell-mode' would ruin our test, because it deletes all
	  ;; buffer local variables.
	  (put 'explicit-shell-file-name 'permanent-local t)
	  ;; Declare connection-local variable `explicit-shell-file-name'.
	  (with-no-warnings
	    (connection-local-set-profile-variables
	     'remote-sh
	     '((explicit-shell-file-name . "/bin/sh")
	       (explicit-sh-args . ("-i"))))
	    (connection-local-set-profiles
	     `(:application tramp
	       :protocol ,(file-remote-p default-directory 'method)
	       :user ,(file-remote-p default-directory 'user)
	       :machine ,(file-remote-p default-directory 'host))
	     'remote-sh))

	  ;; Run interactive shell.  Since the default directory is
	  ;; remote, `explicit-shell-file-name' shall be set in order
	  ;; to avoid a question.
	  (with-current-buffer (get-buffer-create "*shell*")
	    (ignore-errors (kill-process (current-buffer)))
	    (should-not explicit-shell-file-name)
	    (call-interactively 'shell)
	    (should explicit-shell-file-name)))

      (put 'explicit-shell-file-name 'permanent-local nil)
      (kill-buffer "*shell*"))))

(ert-deftest tramp-test34-vc-registered ()
  "Check `vc-registered'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let* ((default-directory tramp-test-temporary-file-directory)
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "foo" tmp-name1))
	   (tramp-remote-process-environment tramp-remote-process-environment)
	   (vc-handled-backends
	    (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
	      (cond
	       ((tramp-find-executable
		 v vc-git-program (tramp-get-remote-path v))
		'(Git))
	       ((tramp-find-executable
		 v vc-hg-program (tramp-get-remote-path v))
		'(Hg))
	       ((tramp-find-executable
		 v vc-bzr-program (tramp-get-remote-path v))
		(setq tramp-remote-process-environment
		      (cons (format "BZR_HOME=%s"
				    (file-remote-p tmp-name1 'localname))
			    tramp-remote-process-environment))
		;; We must force a reconnect, in order to activate $BZR_HOME.
		(tramp-cleanup-connection
		 (tramp-dissect-file-name tramp-test-temporary-file-directory)
		 'keep-debug 'keep-password)
		'(Bzr))
	       (t nil))))
	   ;; Suppress nasty messages.
	   (inhibit-message t))
      (skip-unless vc-handled-backends)
      (unless quoted (tramp--test-message "%s" vc-handled-backends))

      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name2)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name2))
	    (should-not (vc-registered tmp-name1))
	    (should-not (vc-registered tmp-name2))

	    (let ((default-directory tmp-name1))
	      ;; Create empty repository, and register the file.
	      ;; Sometimes, creation of repository fails (bzr!); we
	      ;; skip the test then.
	      (condition-case nil
		  (vc-create-repo (car vc-handled-backends))
		(error (skip-unless nil)))
	      ;; The structure of VC-FILESET is not documented.  Let's
	      ;; hope it won't change.
	      (condition-case nil
		  (vc-register
		   (list (car vc-handled-backends)
			 (list (file-name-nondirectory tmp-name2))))
		;; `vc-register' has changed its arguments in Emacs
		;; 25.1.  Let's skip it for older Emacsen.
		(error (skip-unless (>= emacs-major-version 25))))
	      ;; vc-git uses an own process sentinel, Tramp's sentinel
	      ;; for flushing the cache isn't used.
	      (dired-uncache (concat (file-remote-p default-directory) "/"))
	      (should (vc-registered (file-name-nondirectory tmp-name2)))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test35-make-auto-save-file-name ()
  "Check `make-auto-save-file-name'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted)))

      (unwind-protect
	  (progn
	    ;; Use default `auto-save-file-name-transforms' mechanism.
	    (let (tramp-auto-save-directory)
	      (with-temp-buffer
		(setq buffer-file-name tmp-name1)
		(should
		 (string-equal
		  (make-auto-save-file-name)
		  ;; This is taken from original `make-auto-save-file-name'.
		  ;; We call `convert-standard-filename', because on
		  ;; MS Windows the (local) colons must be replaced by
		  ;; exclamation marks.
		  (convert-standard-filename
		   (expand-file-name
		    (format
		     "#%s#"
		     (subst-char-in-string
		      ?/ ?! (replace-regexp-in-string "!" "!!" tmp-name1)))
		    temporary-file-directory))))))

	    ;; No mapping.
	    (let (tramp-auto-save-directory auto-save-file-name-transforms)
	      (with-temp-buffer
		(setq buffer-file-name tmp-name1)
		(should
		 (string-equal
		  (make-auto-save-file-name)
		  (funcall
		   (if quoted 'tramp-compat-file-name-quote 'identity)
		   (expand-file-name
		    (format "#%s#" (file-name-nondirectory tmp-name1))
		    tramp-test-temporary-file-directory))))))

	    ;; Use default `tramp-auto-save-directory' mechanism.
	    (let ((tramp-auto-save-directory tmp-name2))
	      (with-temp-buffer
		(setq buffer-file-name tmp-name1)
		(should
		 (string-equal
		  (make-auto-save-file-name)
		  ;; This is taken from Tramp.
		  (expand-file-name
		   (format
		    "#%s#"
		    (tramp-subst-strs-in-string
		     '(("_" . "|")
		       ("/" . "_a")
		       (":" . "_b")
		       ("|" . "__")
		       ("[" . "_l")
		       ("]" . "_r"))
		     (tramp-compat-file-name-unquote tmp-name1)))
		   tmp-name2)))
		(should (file-directory-p tmp-name2))))

	    ;; Relative file names shall work, too.
	    (let ((tramp-auto-save-directory "."))
	      (with-temp-buffer
		(setq buffer-file-name tmp-name1
		      default-directory tmp-name2)
		(should
		 (string-equal
		  (make-auto-save-file-name)
		  ;; This is taken from Tramp.
		  (expand-file-name
		   (format
		    "#%s#"
		    (tramp-subst-strs-in-string
		     '(("_" . "|")
		       ("/" . "_a")
		       (":" . "_b")
		       ("|" . "__")
		       ("[" . "_l")
		       ("]" . "_r"))
		     (tramp-compat-file-name-unquote tmp-name1)))
		   tmp-name2)))
		(should (file-directory-p tmp-name2)))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-directory tmp-name2 'recursive))))))

(ert-deftest tramp-test36-find-backup-file-name ()
  "Check `find-backup-file-name'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  ;; These settings are not used by Tramp, so we ignore them.
	  version-control delete-old-versions
	  (kept-old-versions (default-toplevel-value 'kept-old-versions))
	  (kept-new-versions (default-toplevel-value 'kept-new-versions)))

      (unwind-protect
	  ;; Use default `backup-directory-alist' mechanism.
	  (let (backup-directory-alist tramp-backup-directory-alist)
	    (should
	     (equal
	      (find-backup-file-name tmp-name1)
	      (list
	       (funcall
		(if quoted 'tramp-compat-file-name-quote 'identity)
		(expand-file-name
		 (format "%s~" (file-name-nondirectory tmp-name1))
		 tramp-test-temporary-file-directory)))))))

      (unwind-protect
	  ;; Map `backup-directory-alist'.
	  (let ((backup-directory-alist `(("." . ,tmp-name2)))
		tramp-backup-directory-alist)
	    (should
	     (equal
	      (find-backup-file-name tmp-name1)
	      (list
	       (funcall
		(if quoted 'tramp-compat-file-name-quote 'identity)
		(expand-file-name
		 (format
		  "%s~"
		  ;; This is taken from `make-backup-file-name-1'.  We
		  ;; call `convert-standard-filename', because on MS
		  ;; Windows the (local) colons must be replaced by
		  ;; exclamation marks.
		  (subst-char-in-string
		   ?/ ?!
		   (replace-regexp-in-string
		    "!" "!!" (convert-standard-filename tmp-name1))))
		 tmp-name2)))))
	    ;; The backup directory is created.
	    (should (file-directory-p tmp-name2)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name2 'recursive)))

      (unwind-protect
	  ;; Map `tramp-backup-directory-alist'.
	  (let ((tramp-backup-directory-alist `(("." . ,tmp-name2)))
		backup-directory-alist)
	    (should
	     (equal
	      (find-backup-file-name tmp-name1)
	      (list
	       (funcall
		(if quoted 'tramp-compat-file-name-quote 'identity)
		(expand-file-name
		 (format
		  "%s~"
		  ;; This is taken from `make-backup-file-name-1'.  We
		  ;; call `convert-standard-filename', because on MS
		  ;; Windows the (local) colons must be replaced by
		  ;; exclamation marks.
		  (subst-char-in-string
		   ?/ ?!
		   (replace-regexp-in-string
		    "!" "!!" (convert-standard-filename tmp-name1))))
		 tmp-name2)))))
	    ;; The backup directory is created.
	    (should (file-directory-p tmp-name2)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name2 'recursive)))

      (unwind-protect
	  ;; Map `tramp-backup-directory-alist' with local file name.
	  (let ((tramp-backup-directory-alist
		 `(("." . ,(file-remote-p tmp-name2 'localname))))
		backup-directory-alist)
	    (should
	     (equal
	      (find-backup-file-name tmp-name1)
	      (list
	       (funcall
		(if quoted 'tramp-compat-file-name-quote 'identity)
		(expand-file-name
		 (format
		  "%s~"
		  ;; This is taken from `make-backup-file-name-1'.  We
		  ;; call `convert-standard-filename', because on MS
		  ;; Windows the (local) colons must be replaced by
		  ;; exclamation marks.
		  (subst-char-in-string
		   ?/ ?!
		   (replace-regexp-in-string
		    "!" "!!" (convert-standard-filename tmp-name1))))
		 tmp-name2)))))
	    ;; The backup directory is created.
	    (should (file-directory-p tmp-name2)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name2 'recursive))))))

;; The functions were introduced in Emacs 26.1.
(ert-deftest tramp-test37-make-nearby-temp-file ()
  "Check `make-nearby-temp-file' and `temporary-file-directory'."
  (skip-unless (tramp--test-enabled))
  ;; Since Emacs 26.1.
  (skip-unless
   (and (fboundp 'make-nearby-temp-file) (fboundp 'temporary-file-directory)))

  ;; `make-nearby-temp-file' and `temporary-file-directory' exists
  ;; since Emacs 26.1.  We don't want to see compiler warnings for
  ;; older Emacsen.
  (let ((default-directory tramp-test-temporary-file-directory)
	tmp-file)
    ;; The remote host shall know a temporary file directory.
    (should (stringp (with-no-warnings (temporary-file-directory))))
    (should
     (string-equal
      (file-remote-p default-directory)
      (file-remote-p (with-no-warnings (temporary-file-directory)))))

    ;; The temporary file shall be located on the remote host.
    (setq tmp-file (with-no-warnings (make-nearby-temp-file "tramp-test")))
    (should (file-exists-p tmp-file))
    (should (file-regular-p tmp-file))
    (should
     (string-equal
      (file-remote-p default-directory)
      (file-remote-p tmp-file)))
    (delete-file tmp-file)
    (should-not (file-exists-p tmp-file))

    (setq tmp-file (with-no-warnings (make-nearby-temp-file "tramp-test" 'dir)))
    (should (file-exists-p tmp-file))
    (should (file-directory-p tmp-file))
    (delete-directory tmp-file)
    (should-not (file-exists-p tmp-file))))

(defun tramp--test-emacs26-p ()
  "Check for Emacs version >= 26.1.
Some semantics has been changed for there, w/o new functions or
variables, so we check the Emacs version directly."
  (>= emacs-major-version 26))

(defun tramp--test-emacs27-p ()
  "Check for Emacs version >= 27.1.
Some semantics has been changed for there, w/o new functions or
variables, so we check the Emacs version directly."
  (>= emacs-major-version 27))

(defun tramp--test-adb-p ()
  "Check, whether the remote host runs Android.
This requires restrictions of file name syntax."
  (tramp-adb-file-name-p tramp-test-temporary-file-directory))

(defun tramp--test-docker-p ()
  "Check, whether the docker method is used.
This does not support some special file names."
  (string-equal
   "docker" (file-remote-p tramp-test-temporary-file-directory 'method)))

(defun tramp--test-ftp-p ()
  "Check, whether an FTP-like method is used.
This does not support globbing characters in file names (yet)."
  ;; Globbing characters are ??, ?* and ?\[.
  (string-match
   "ftp$" (file-remote-p tramp-test-temporary-file-directory 'method)))

(defun tramp--test-gvfs-p (&optional method)
  "Check, whether the remote host runs a GVFS based method.
This requires restrictions of file name syntax."
  (or (member method tramp-gvfs-methods)
      (tramp-gvfs-file-name-p tramp-test-temporary-file-directory)))

(defun tramp--test-hpux-p ()
  "Check, whether the remote host runs HP-UX.
Several special characters do not work properly there."
  ;; We must refill the cache.  `file-truename' does it.
  (with-parsed-tramp-file-name
      (file-truename tramp-test-temporary-file-directory) nil
    (string-match "^HP-UX" (tramp-get-connection-property v "uname" ""))))

(defun tramp--test-mock-p ()
  "Check, whether the mock method is used.
This does not support external Emacs calls."
  (string-equal
   "mock" (file-remote-p tramp-test-temporary-file-directory 'method)))

(defun tramp--test-owncloud-p ()
  "Check, whether the owncloud method is used."
  (string-equal
   "owncloud" (file-remote-p tramp-test-temporary-file-directory 'method)))

(defun tramp--test-rsync-p ()
  "Check, whether the rsync method is used.
This does not support special file names."
  (string-equal
   "rsync" (file-remote-p tramp-test-temporary-file-directory 'method)))

(defun tramp--test-sh-p ()
  "Check, whether the remote host runs a based method from tramp-sh.el."
  (eq
   (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
   'tramp-sh-file-name-handler))

(defun tramp--test-windows-nt ()
  "Check, whether the locale host runs MS Windows."
  (eq system-type 'windows-nt))

(defun tramp--test-windows-nt-and-batch ()
  "Check, whether the locale host runs MS Windows in batch mode.
This does not support special characters."
  (and (eq system-type 'windows-nt) noninteractive))

(defun tramp--test-windows-nt-and-pscp-psftp-p ()
  "Check, whether the locale host runs MS Windows, and ps{cp,ftp} is used.
This does not support utf8 based file transfer."
  (and (eq system-type 'windows-nt)
       (string-match
	(regexp-opt '("pscp" "psftp"))
	(file-remote-p tramp-test-temporary-file-directory 'method))))

(defun tramp--test-windows-nt-or-smb-p ()
  "Check, whether the locale or remote host runs MS Windows.
This requires restrictions of file name syntax."
  (or (eq system-type 'windows-nt)
      (tramp-smb-file-name-p tramp-test-temporary-file-directory)))

(defun tramp--test-check-files (&rest files)
  "Run a simple but comprehensive test over every file in FILES."
  ;; `filename-non-special' has been fixed in Emacs 27.1, see Bug#29579.
  (dolist (quoted (if (and (tramp--test-expensive-test) (tramp--test-emacs27-p))
		      '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory,
    ;; because it could be located on a symlinked directory.  This
    ;; would let the test fail.
    (let* ((tramp-test-temporary-file-directory
	    (file-truename tramp-test-temporary-file-directory))
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name 'local quoted))
	   (files (delq nil files))
	   (process-environment process-environment))
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (make-directory tmp-name2)

	    (dolist (elt files)
	      (let* ((file1 (expand-file-name elt tmp-name1))
		     (file2 (expand-file-name elt tmp-name2))
		     (file3 (expand-file-name (concat elt "foo") tmp-name1)))
		(write-region elt nil file1)
		(should (file-exists-p file1))

		;; Check file contents.
		(with-temp-buffer
		  (insert-file-contents file1)
		  (should (string-equal (buffer-string) elt)))

		;; Copy file both directions.
		(copy-file file1 (file-name-as-directory tmp-name2))
		(should (file-exists-p file2))
		(delete-file file1)
		(should-not (file-exists-p file1))
		(copy-file file2 (file-name-as-directory tmp-name1))
		(should (file-exists-p file1))

		(tramp--test-ignore-make-symbolic-link-error
		  (make-symbolic-link file1 file3)
		  (should (file-symlink-p file3))
		  (should
		   (string-equal
		    (expand-file-name file1) (file-truename file3)))
		  (should
		   (string-equal
		    (funcall
		     (if quoted 'tramp-compat-file-name-quote 'identity)
		     (car (file-attributes file3)))
		    (file-remote-p (file-truename file1) 'localname)))
		  ;; Check file contents.
		  (with-temp-buffer
		    (insert-file-contents file3)
		    (should (string-equal (buffer-string) elt)))
		  (delete-file file3))))

	    ;; Check file names.
	    (should (equal (directory-files
			    tmp-name1 nil directory-files-no-dot-files-regexp)
			   (sort (copy-sequence files) 'string-lessp)))
	    (should (equal (directory-files
			    tmp-name2 nil directory-files-no-dot-files-regexp)
			   (sort (copy-sequence files) 'string-lessp)))

	    ;; `substitute-in-file-name' could return different
	    ;; values.  For `adb', there could be strange file
	    ;; permissions preventing overwriting a file.  We don't
	    ;; care in this testcase.
	    (dolist (elt files)
	      (let ((file1
		     (substitute-in-file-name (expand-file-name elt tmp-name1)))
		    (file2
		     (substitute-in-file-name
		      (expand-file-name elt tmp-name2))))
		(ignore-errors (write-region elt nil file1))
		(should (file-exists-p file1))
		(ignore-errors (write-region elt nil file2 nil 'nomessage))
		(should (file-exists-p file2))))

	    (should (equal (directory-files
			    tmp-name1 nil directory-files-no-dot-files-regexp)
			   (directory-files
			    tmp-name2 nil directory-files-no-dot-files-regexp)))

	    ;; Check directory creation.  We use a subdirectory "foo"
	    ;; in order to avoid conflicts with previous file name tests.
	    (dolist (elt files)
	      (let* ((elt1 (concat elt "foo"))
		     (file1 (expand-file-name (concat "foo/" elt) tmp-name1))
		     (file2 (expand-file-name elt file1))
		     (file3 (expand-file-name elt1 file1)))
		(make-directory file1 'parents)
		(should (file-directory-p file1))
		(write-region elt nil file2)
		(should (file-exists-p file2))
		(should
		 (equal
		  (directory-files
		   file1 nil directory-files-no-dot-files-regexp)
		  `(,elt)))
		(should
		 (equal
		  (caar (directory-files-and-attributes
			 file1 nil directory-files-no-dot-files-regexp))
		  elt))

		;; Check symlink in `directory-files-and-attributes'.
		;; It does not work in the "smb" case, only relative
		;; symlinks to existing files are shown there.
		(tramp--test-ignore-make-symbolic-link-error
		  (unless
		     (tramp-smb-file-name-p tramp-test-temporary-file-directory)
		    (make-symbolic-link file2 file3)
		    (should (file-symlink-p file3))
		    (should
		     (string-equal
		      (caar (directory-files-and-attributes
			     file1 nil (regexp-quote elt1)))
		      elt1))
		    (should
		     (string-equal
		      (funcall
		       (if quoted 'tramp-compat-file-name-quote 'identity)
		       (cadr (car (directory-files-and-attributes
				   file1 nil (regexp-quote elt1)))))
		      (file-remote-p (file-truename file2) 'localname)))
		    (delete-file file3)
		    (should-not (file-exists-p file3))))

		(delete-file file2)
		(should-not (file-exists-p file2))
		(delete-directory file1)
		(should-not (file-exists-p file1))))

	    ;; Check, that environment variables are set correctly.
	    (when (and (tramp--test-expensive-test) (tramp--test-sh-p))
	      (dolist (elt files)
		(let ((envvar (concat "VAR_" (upcase (md5 elt))))
		      (default-directory tramp-test-temporary-file-directory)
		      (process-environment process-environment))
		  (setenv envvar elt)
		  ;; The value of PS1 could confuse Tramp's detection
		  ;; of process output.  So we unset it temporarily.
		  (setenv "PS1")
		  (with-temp-buffer
		    (should (zerop (process-file "env" nil t nil)))
		    (goto-char (point-min))
		    (should
		     (re-search-forward
		      (format
		       "^%s=%s$"
		       (regexp-quote envvar)
		       (regexp-quote (getenv envvar))))))))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))
	(ignore-errors (delete-directory tmp-name2 'recursive))))))

(defun tramp--test-special-characters ()
  "Perform the test in `tramp-test38-special-characters*'."
  ;; Newlines, slashes and backslashes in file names are not
  ;; supported.  So we don't test.  And we don't test the tab
  ;; character on Windows or Cygwin, because the backslash is
  ;; interpreted as a path separator, preventing "\t" from being
  ;; expanded to <TAB>.
  (let ((files
	 (list
	  (if (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	      "foo bar baz"
	    (if (or (tramp--test-adb-p)
		    (tramp--test-docker-p)
		    (eq system-type 'cygwin))
		" foo bar baz "
	      " foo\tbar baz\t"))
	  "$foo$bar$$baz$"
	  "-foo-bar-baz-"
	  "%foo%bar%baz%"
	  "&foo&bar&baz&"
	  (unless (or (tramp--test-ftp-p)
		      (tramp--test-gvfs-p)
		      (tramp--test-windows-nt-or-smb-p))
	    "?foo?bar?baz?")
	  (unless (or (tramp--test-ftp-p)
		      (tramp--test-gvfs-p)
		      (tramp--test-windows-nt-or-smb-p))
	    "*foo*bar*baz*")
	  (if (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	      "'foo'bar'baz'"
	    "'foo\"bar'baz\"")
	  "#foo~bar#baz~"
	  (if (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	      "!foo!bar!baz!"
	    "!foo|bar!baz|")
	  (if (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	      ";foo;bar;baz;"
	    ":foo;bar:baz;")
	  (unless (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	    "<foo>bar<baz>")
	  "(foo)bar(baz)"
	  (unless (or (tramp--test-ftp-p) (tramp--test-gvfs-p)) "[foo]bar[baz]")
	  "{foo}bar{baz}")))
    ;; Simplify test in order to speed up.
    (apply 'tramp--test-check-files
	   (if (tramp--test-expensive-test)
	       files (list (mapconcat 'identity files ""))))))

;; These tests are inspired by Bug#17238.
(ert-deftest tramp-test38-special-characters ()
  "Check special characters in file names."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))

  (tramp--test-special-characters))

(ert-deftest tramp-test38-special-characters-with-stat ()
  "Check special characters in file names.
Use the `stat' command."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))
  (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
    (skip-unless (tramp-get-remote-stat v)))

  (let ((tramp-connection-properties
	 (append
	  `((,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "perl" nil))
	  tramp-connection-properties)))
    (tramp--test-special-characters)))

(ert-deftest tramp-test38-special-characters-with-perl ()
  "Check special characters in file names.
Use the `perl' command."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))
  (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
    (skip-unless (tramp-get-remote-perl v)))

  (let ((tramp-connection-properties
	 (append
	  `((,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "stat" nil)
	    ;; See `tramp-sh-handle-file-truename'.
	    (,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "readlink" nil))
	  tramp-connection-properties)))
    (tramp--test-special-characters)))

(ert-deftest tramp-test38-special-characters-with-ls ()
  "Check special characters in file names.
Use the `ls' command."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-batch)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))

  (let ((tramp-connection-properties
	 (append
	  `((,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "perl" nil)
	    (,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "stat" nil)
	    ;; See `tramp-sh-handle-file-truename'.
	    (,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "readlink" nil))
	  tramp-connection-properties)))
    (tramp--test-special-characters)))

(defun tramp--test-utf8 ()
  "Perform the test in `tramp-test39-utf8*'."
  (let* ((utf8 (if (and (eq system-type 'darwin)
			(memq 'utf-8-hfs (coding-system-list)))
		   'utf-8-hfs 'utf-8))
	 (coding-system-for-read utf8)
	 (coding-system-for-write utf8)
	 (file-name-coding-system
	  (coding-system-change-eol-conversion utf8 'unix)))
    (tramp--test-check-files
     (unless (tramp--test-hpux-p) "Γυρίστε το Γαλαξία με Ώτο Στοπ")
     (unless (tramp--test-hpux-p)
       "أصبح بوسعك الآن تنزيل نسخة كاملة من موسوعة ويكيبيديا العربية لتصفحها بلا اتصال بالإنترنت")
     "银河系漫游指南系列"
     "Автостопом по гала́ктике")))

(ert-deftest tramp-test39-utf8 ()
  "Check UTF8 encoding in file names and file contents."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-docker-p)))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-batch)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))

  (tramp--test-utf8))

(ert-deftest tramp-test39-utf8-with-stat ()
  "Check UTF8 encoding in file names and file contents.
Use the `stat' command."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-docker-p)))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-batch)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))
  (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
    (skip-unless (tramp-get-remote-stat v)))

  (let ((tramp-connection-properties
	 (append
	  `((,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "perl" nil))
	  tramp-connection-properties)))
    (tramp--test-utf8)))

(ert-deftest tramp-test39-utf8-with-perl ()
  "Check UTF8 encoding in file names and file contents.
Use the `perl' command."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-docker-p)))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-batch)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))
  (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
    (skip-unless (tramp-get-remote-perl v)))

  (let ((tramp-connection-properties
	 (append
	  `((,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "stat" nil)
	    ;; See `tramp-sh-handle-file-truename'.
	    (,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "readlink" nil))
	  tramp-connection-properties)))
    (tramp--test-utf8)))

(ert-deftest tramp-test39-utf8-with-ls ()
  "Check UTF8 encoding in file names and file contents.
Use the `ls' command."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-docker-p)))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-batch)))
  (skip-unless (not (tramp--test-windows-nt-and-pscp-psftp-p)))

  (let ((tramp-connection-properties
	 (append
	  `((,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "perl" nil)
	    (,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "stat" nil)
	    ;; See `tramp-sh-handle-file-truename'.
	    (,(regexp-quote (file-remote-p tramp-test-temporary-file-directory))
	     "readlink" nil))
	  tramp-connection-properties)))
    (tramp--test-utf8)))

(ert-deftest tramp-test40-file-system-info ()
  "Check that `file-system-info' returns proper values."
  (skip-unless (tramp--test-enabled))
  ;; Since Emacs 27.1.
  (skip-unless (fboundp 'file-system-info))

  ;; `file-system-info' exists since Emacs 27.  We don't want to see
  ;; compiler warnings for older Emacsen.
  (let ((fsi (with-no-warnings
	       (file-system-info tramp-test-temporary-file-directory))))
    (skip-unless fsi)
    (should (and (consp fsi)
		 (= (length fsi) 3)
		 (numberp (nth 0 fsi))
		 (numberp (nth 1 fsi))
		 (numberp (nth 2 fsi))))))

(defun tramp--test-timeout-handler ()
  (interactive)
  (ert-fail (format "`%s' timed out" (ert-test-name (ert-running-test)))))

;; This test is inspired by Bug#16928.
(ert-deftest tramp-test41-asynchronous-requests ()
  "Check parallel asynchronous requests.
Such requests could arrive from timers, process filters and
process sentinels.  They shall not disturb each other."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  ;; This test could be blocked on hydra.  So we set a timeout of 300
  ;; seconds, and we send a SIGUSR1 signal after 300 seconds.
  ;; This clearly doesn't work though, because the test not
  ;; infrequently hangs for hours until killed by the infrastructure.
  (with-timeout (300 (tramp--test-timeout-handler))
    (define-key special-event-map [sigusr1] 'tramp--test-timeout-handler)
    (tramp--test-instrument-test-case (if (getenv "EMACS_HYDRA_CI") 10 0)
    (let* (;; For the watchdog.
	   (default-directory (expand-file-name temporary-file-directory))
	   (watchdog
            (start-process
             "*watchdog*" nil shell-file-name shell-command-switch
             (format "sleep 300; kill -USR1 %d" (emacs-pid))))
           (tmp-name (tramp--test-make-temp-name))
           (default-directory tmp-name)
           ;; Do not cache Tramp properties.
           (remote-file-name-inhibit-cache t)
           (process-file-side-effects t)
           ;; Suppress nasty messages.
           (inhibit-message t)
	   ;; Do not run delayed timers.
	   (timer-max-repeats 0)
	   ;; Number of asynchronous processes for test.  Tests on
	   ;; some machines handle less parallel processes.
           (number-proc
            (cond
             ((ignore-errors
               (string-to-number (getenv "REMOTE_PARALLEL_PROCESSES"))))
	     ((getenv "EMACS_HYDRA_CI") 5)
             (t 10)))
           ;; On hydra, timings are bad.
           (timer-repeat
            (cond
             ((getenv "EMACS_HYDRA_CI") 10)
             (t 1)))
           ;; We must distinguish due to performance reasons.
           (timer-operation
            (cond
             ((tramp--test-mock-p) 'vc-registered)
             (t 'file-attributes)))
           timer buffers kill-buffer-query-functions)

      (unwind-protect
          (progn
            (make-directory tmp-name)

            ;; Setup a timer in order to raise an ordinary command
            ;; again and again.  `vc-registered' is well suited,
            ;; because there are many checks.
            (setq
             timer
             (run-at-time
              0 timer-repeat
              (lambda ()
                (when buffers
                  (let ((time (float-time))
                        (default-directory tmp-name)
                        (file
                         (buffer-name (nth (random (length buffers)) buffers))))
                    (tramp--test-message
                     "Start timer %s %s" file (current-time-string))
                    (funcall timer-operation file)
                    ;; Adjust timer if it takes too much time.
                    (when (> (- (float-time) time) timer-repeat)
                      (setq timer-repeat (* 1.5 timer-repeat))
                      (setf (timer--repeat-delay timer) timer-repeat)
                      (tramp--test-message "Increase timer %s" timer-repeat))
                    (tramp--test-message
                     "Stop timer %s %s" file (current-time-string)))))))

            ;; Create temporary buffers.  The number of buffers
            ;; corresponds to the number of processes; it could be
            ;; increased in order to make pressure on Tramp.
            (dotimes (_ number-proc)
              (setq buffers (cons (generate-new-buffer "foo") buffers)))

            ;; Open asynchronous processes.  Set process filter and sentinel.
            (dolist (buf buffers)
	      ;; Activate timer.
	      (sit-for 0.01 'nodisp)
              (let ((proc
                     (start-file-process-shell-command
                      (buffer-name buf) buf
                      (concat
                       "(read line && echo $line >$line);"
                       "(read line && cat $line);"
                       "(read line && rm $line)")))
                    (file (expand-file-name (buffer-name buf))))
                ;; Remember the file name.  Add counter.
                (process-put proc 'foo file)
                (process-put proc 'bar 0)
                ;; Add process filter.
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (tramp--test-message
                    "Process filter %s %s %s" proc string (current-time-string))
                   (with-current-buffer (process-buffer proc)
                     (insert string))
                   (unless (zerop (length string))
		     (dired-uncache (process-get proc 'foo))
                     (should (file-attributes (process-get proc 'foo))))))
                ;; Add process sentinel.
                (set-process-sentinel
                 proc
                 (lambda (proc _state)
                   (tramp--test-message
                    "Process sentinel %s %s" proc (current-time-string))
		   (dired-uncache (process-get proc 'foo))
                   (should-not (file-attributes (process-get proc 'foo)))))))

            ;; Send a string.  Use a random order of the buffers.  Mix
            ;; with regular operation.
            (let ((buffers (copy-sequence buffers)))
              (while buffers
		;; Activate timer.
		(sit-for 0.01 'nodisp)
                (let* ((buf (nth (random (length buffers)) buffers))
                       (proc (get-buffer-process buf))
                       (file (process-get proc 'foo))
                       (count (process-get proc 'bar)))
                  (tramp--test-message
                   "Start action %d %s %s" count buf (current-time-string))
                  ;; Regular operation prior process action.
		  (dired-uncache file)
                  (if (= count 0)
                      (should-not (file-attributes file))
                    (should (file-attributes file)))
                  ;; Send string to process.
                  (process-send-string proc (format "%s\n" (buffer-name buf)))
                  (accept-process-output proc 0.1 nil 0)
                  ;; Give the watchdog a chance.
                  (read-event nil nil 0.01)
                  (tramp--test-message
                   "Continue action %d %s %s" count buf (current-time-string))
                  ;; Regular operation post process action.
		  (dired-uncache file)
                  (if (= count 2)
                      (should-not (file-attributes file))
                    (should (file-attributes file)))
                  (tramp--test-message
                   "Stop action %d %s %s" count buf (current-time-string))
                  (process-put proc 'bar (1+ count))
                  (unless (process-live-p proc)
                    (setq buffers (delq buf buffers))))))

            ;; Checks.  All process output shall exists in the
            ;; respective buffers.  All created files shall be
            ;; deleted.
            (tramp--test-message "Check %s" (current-time-string))
            (dolist (buf buffers)
              (with-current-buffer buf
                (should (string-equal (format "%s\n" buf) (buffer-string)))))
            (should-not
             (directory-files
              tmp-name nil directory-files-no-dot-files-regexp)))

        ;; Cleanup.
        (define-key special-event-map [sigusr1] 'ignore)
        (ignore-errors (quit-process watchdog))
        (dolist (buf buffers)
          (ignore-errors (delete-process (get-buffer-process buf)))
          (ignore-errors (kill-buffer buf)))
        (ignore-errors (cancel-timer timer))
        (ignore-errors (delete-directory tmp-name 'recursive)))))))

;; This test is inspired by Bug#29163.
(ert-deftest tramp-test42-auto-load ()
  "Check that Tramp autoloads properly."
  (let ((default-directory (expand-file-name temporary-file-directory))
	(code
	 (format
	  "(message \"Tramp loaded: %%s\" (and (file-remote-p %S) t))"
	  tramp-test-temporary-file-directory)))
    (should
     (string-match
      "Tramp loaded: t[\n\r]+"
      (shell-command-to-string
       (format
	"%s -batch -Q -L %s --eval %s"
	(shell-quote-argument
	 (expand-file-name invocation-name invocation-directory))
	(mapconcat 'shell-quote-argument load-path " -L ")
	(shell-quote-argument code)))))))

(ert-deftest tramp-test42-delay-load ()
  "Check that Tramp is loaded lazily, only when needed."
  ;; The autoloaded Tramp objects are different since Emacs 26.1.  We
  ;; cannot test older Emacsen, therefore.
  (skip-unless (tramp--test-emacs26-p))

  ;; Tramp is neither loaded at Emacs startup, nor when completing a
  ;; non-Tramp file name like "/foo".  Completing a Tramp-alike file
  ;; name like "/foo:" autoloads Tramp, when `tramp-mode' is t.
  (let ((default-directory (expand-file-name temporary-file-directory))
	(code
	 "(progn \
           (setq tramp-mode %s) \
	   (message \"Tramp loaded: %%s\" (featurep 'tramp)) \
	   (file-name-all-completions \"/foo\" \"/\") \
	   (message \"Tramp loaded: %%s\" (featurep 'tramp)) \
	   (file-name-all-completions \"/foo:\" \"/\") \
	   (message \"Tramp loaded: %%s\" (featurep 'tramp)))"))
    ;; Tramp doesn't load when `tramp-mode' is nil.
    (dolist (tm '(t nil))
      (should
       (string-match
	(format
       "Tramp loaded: nil[\n\r]+Tramp loaded: nil[\n\r]+Tramp loaded: %s[\n\r]+"
	 tm)
	(shell-command-to-string
	 (format
	  "%s -batch -Q -L %s --eval %s"
	  (shell-quote-argument
	   (expand-file-name invocation-name invocation-directory))
	  (mapconcat 'shell-quote-argument load-path " -L ")
	  (shell-quote-argument (format code tm)))))))))

(ert-deftest tramp-test42-recursive-load ()
  "Check that Tramp does not fail due to recursive load."
  (skip-unless (tramp--test-enabled))

  (let ((default-directory (expand-file-name temporary-file-directory)))
    (dolist (code
	     (list
	      (format
	       "(expand-file-name %S)" tramp-test-temporary-file-directory)
	      (format
	       "(let ((default-directory %S)) (expand-file-name %S))"
	       tramp-test-temporary-file-directory
	       temporary-file-directory)))
      (should-not
       (string-match
	"Recursive load"
	(shell-command-to-string
	 (format
	  "%s -batch -Q -L %s --eval %s"
	  (shell-quote-argument
	   (expand-file-name invocation-name invocation-directory))
	  (mapconcat 'shell-quote-argument load-path " -L ")
	  (shell-quote-argument code))))))))

(ert-deftest tramp-test42-remote-load-path ()
  "Check that Tramp autoloads its packages with remote `load-path'."
  ;; The autoloaded Tramp objects are different since Emacs 26.1.  We
  ;; cannot test older Emacsen, therefore.
  (skip-unless (tramp--test-emacs26-p))

  ;; `tramp-cleanup-all-connections' is autoloaded from tramp-cmds.el.
  ;; It shall still work, when a remote file name is in the
  ;; `load-path'.
  (let ((default-directory (expand-file-name temporary-file-directory))
	(code
	 "(let ((force-load-messages t) \
		(load-path (cons \"/foo:bar:\" load-path))) \
	    (tramp-cleanup-all-connections))"))
    (should
     (string-match
      (format
       "Loading %s"
       (expand-file-name
	"tramp-cmds" (file-name-directory (locate-library "tramp"))))
      (shell-command-to-string
       (format
	"%s -batch -Q -L %s -l tramp-sh --eval %s"
	(shell-quote-argument
	 (expand-file-name invocation-name invocation-directory))
	(mapconcat 'shell-quote-argument load-path " -L ")
	(shell-quote-argument code)))))))

(ert-deftest tramp-test43-unload ()
  "Check that Tramp and its subpackages unload completely.
Since it unloads Tramp, it shall be the last test to run."
  :tags '(:expensive-test)
  (skip-unless noninteractive)
  ;; The autoloaded Tramp objects are different since Emacs 26.1.  We
  ;; cannot test older Emacsen, therefore.
  (skip-unless (tramp--test-emacs26-p))

  (when (featurep 'tramp)
    (unload-feature 'tramp 'force)
    ;; No Tramp feature must be left.
    (should-not (featurep 'tramp))
    (should-not (all-completions "tramp" (delq 'tramp-tests features)))
    ;; `file-name-handler-alist' must be clean.
    (should-not (all-completions "tramp" (mapcar 'cdr file-name-handler-alist)))
    ;; There shouldn't be left a bound symbol, except buffer-local
    ;; variables, and autoload functions.  We do not regard our test
    ;; symbols, and the Tramp unload hooks.
    (mapatoms
     (lambda (x)
       (and (or (and (boundp x) (null (local-variable-if-set-p x)))
		(and (functionp x) (null (autoloadp (symbol-function x)))))
	    (string-match "^tramp" (symbol-name x))
	    (not (string-match "^tramp--?test" (symbol-name x)))
	    (not (string-match "unload-hook$" (symbol-name x)))
	    (ert-fail (format "`%s' still bound" x)))))
    ;; The defstruct `tramp-file-name' and all its internal functions
    ;; shall be purged.
    (should-not (cl--find-class 'tramp-file-name))
    (mapatoms
     (lambda (x)
       (and (functionp x)
            (string-match "tramp-file-name" (symbol-name x))
            (ert-fail (format "Structure function `%s' still exists" x)))))
    ;; There shouldn't be left a hook function containing a Tramp
    ;; function.  We do not regard the Tramp unload hooks.
    (mapatoms
     (lambda (x)
       (and (boundp x)
	    (string-match "-\\(hook\\|function\\)s?$" (symbol-name x))
	    (not (string-match "unload-hook$" (symbol-name x)))
	    (consp (symbol-value x))
	    (ignore-errors (all-completions "tramp" (symbol-value x)))
	    (ert-fail (format "Hook `%s' still contains Tramp function" x)))))))

(defun tramp-test-all (&optional interactive)
  "Run all tests for \\[tramp]."
  (interactive "p")
  (funcall
   (if interactive 'ert-run-tests-interactively 'ert-run-tests-batch) "^tramp"))

;; TODO:

;; * dired-compress-file
;; * dired-uncache
;; * file-equal-p (partly done in `tramp-test21-file-links')
;; * file-in-directory-p
;; * file-name-case-insensitive-p

;; * Work on skipped tests.  Make a comment, when it is impossible.
;; * Fix `tramp-test05-expand-file-name-relative' in `expand-file-name'.
;; * Fix `tramp-test06-directory-file-name' for `ftp'.
;; * Investigate, why `tramp-test11-copy-file' and `tramp-test12-rename-file'
;;   do not work properly for `owncloud'.
;; * Fix `tramp-test29-start-file-process' on MS Windows (`process-send-eof'?).
;; * Fix `tramp-test30-interrupt-process', timeout doesn't work reliably.
;; * Fix Bug#16928 in `tramp-test41-asynchronous-requests'.

(provide 'tramp-tests)
;;; tramp-tests.el ends here
