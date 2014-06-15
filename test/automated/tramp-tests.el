;;; tramp-tests.el --- Tests of remote file access

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

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
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

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

;; A whole test run can be performed calling the command `tramp-test-all'.

;;; Code:

(require 'ert)
(require 'tramp)
(require 'vc)
(require 'vc-bzr)
(require 'vc-git)
(require 'vc-hg)

(declare-function tramp-find-executable "tramp-sh")
(declare-function tramp-get-remote-path "tramp-sh")
(defvar tramp-copy-size-limit)
(defvar tramp-remote-process-environment)

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
      (format "/mock::%s" temporary-file-directory)))
  "Temporary directory for Tramp tests.")

(setq password-cache-expiry nil
      tramp-verbose 0
      tramp-copy-size-limit nil
      tramp-message-show-message nil)

;; This shall happen on hydra only.
(when (getenv "NIX_STORE")
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

(defun tramp--test-make-temp-name (&optional local)
  "Create a temporary file name for test."
  (expand-file-name
   (make-temp-name "tramp-test")
   (if local temporary-file-directory tramp-test-temporary-file-directory)))

(defmacro tramp--instrument-test-case (verbose &rest body)
  "Run BODY with `tramp-verbose' equal VERBOSE.
Print the the content of the Tramp debug buffer, if BODY does not
eval properly in `should', `should-not' or `should-error'.  BODY
shall not contain a timeout."
  (declare (indent 1) (debug (natnump body)))
  `(let ((tramp-verbose ,verbose)
	 (tramp-message-show-message t)
	 (tramp-debug-on-error t))
     (condition-case err
	 (progn ,@body)
       (ert-test-skipped
	(signal (car err) (cdr err)))
       ((error quit)
	(with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (message "%s" (buffer-string)))
	  (with-current-buffer (tramp-get-debug-buffer v)
	    (message "%s" (buffer-string))))
	(message "%s" err)
	(signal (car err) (cdr err))))))

(ert-deftest tramp-test00-availability ()
  "Test availability of Tramp functions."
  :expected-result (if (tramp--test-enabled) :passed :failed)
  (message "Remote directory: `%s'" tramp-test-temporary-file-directory)
  (should (ignore-errors
	    (and
	     (file-remote-p tramp-test-temporary-file-directory)
	     (file-directory-p tramp-test-temporary-file-directory)
	     (file-writable-p tramp-test-temporary-file-directory)))))

(ert-deftest tramp-test01-file-name-syntax ()
  "Check remote file name syntax."
  ;; Simple cases.
  (should (tramp-tramp-file-p "/method::"))
  (should (tramp-tramp-file-p "/host:"))
  (should (tramp-tramp-file-p "/user@:"))
  (should (tramp-tramp-file-p "/user@host:"))
  (should (tramp-tramp-file-p "/method:host:"))
  (should (tramp-tramp-file-p "/method:user@:"))
  (should (tramp-tramp-file-p "/method:user@host:"))
  (should (tramp-tramp-file-p "/method:user@email@host:"))

  ;; Using a port.
  (should (tramp-tramp-file-p "/host#1234:"))
  (should (tramp-tramp-file-p "/user@host#1234:"))
  (should (tramp-tramp-file-p "/method:host#1234:"))
  (should (tramp-tramp-file-p "/method:user@host#1234:"))

  ;; Using an IPv4 address.
  (should (tramp-tramp-file-p "/1.2.3.4:"))
  (should (tramp-tramp-file-p "/user@1.2.3.4:"))
  (should (tramp-tramp-file-p "/method:1.2.3.4:"))
  (should (tramp-tramp-file-p "/method:user@1.2.3.4:"))

  ;; Using an IPv6 address.
  (should (tramp-tramp-file-p "/[]:"))
  (should (tramp-tramp-file-p "/[::1]:"))
  (should (tramp-tramp-file-p "/user@[::1]:"))
  (should (tramp-tramp-file-p "/method:[::1]:"))
  (should (tramp-tramp-file-p "/method:user@[::1]:"))

  ;; Local file name part.
  (should (tramp-tramp-file-p "/host:/:"))
  (should (tramp-tramp-file-p "/method:::"))
  (should (tramp-tramp-file-p "/method::/path/to/file"))
  (should (tramp-tramp-file-p "/method::file"))

  ;; Multihop.
  (should (tramp-tramp-file-p "/method1:|method2::"))
  (should (tramp-tramp-file-p "/method1:host1|host2:"))
  (should (tramp-tramp-file-p "/method1:host1|method2:host2:"))
  (should (tramp-tramp-file-p "/method1:user1@host1|method2:user2@host2:"))
  (should (tramp-tramp-file-p
	   "/method1:user1@host1|method2:user2@host2|method3:user3@host3:"))

  ;; No strings.
  (should-not (tramp-tramp-file-p nil))
  (should-not (tramp-tramp-file-p 'symbol))
  ;; "/:" suppresses file name handlers.
  (should-not (tramp-tramp-file-p "/::"))
  (should-not (tramp-tramp-file-p "/:@:"))
  (should-not (tramp-tramp-file-p "/:[]:"))
  ;; Multihops require a method.
  (should-not (tramp-tramp-file-p "/host1|host2:"))
  ;; Methods or hostnames shall be at least two characters on MS Windows.
  (when (memq system-type '(cygwin windows-nt))
      (should-not (tramp-tramp-file-p "/c:/path/to/file"))
      (should-not (tramp-tramp-file-p "/c::/path/to/file"))))

(ert-deftest tramp-test02-file-name-dissect ()
  "Check remote file name components."
  (let ((tramp-default-method "default-method")
	(tramp-default-user "default-user")
	(tramp-default-host "default-host"))
    ;; Expand `tramp-default-user' and `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/method::")
	     (format "/%s:%s@%s:" "method" "default-user" "default-host")))
    (should (string-equal (file-remote-p "/method::" 'method) "method"))
    (should (string-equal (file-remote-p "/method::" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method::" 'host) "default-host"))
    (should (string-equal (file-remote-p "/method::" 'localname) ""))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/host:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "host")))
    (should (string-equal (file-remote-p "/host:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/host:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/host:" 'host) "host"))
    (should (string-equal (file-remote-p "/host:" 'localname) ""))

    ;; Expand `tramp-default-method' and `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/user@:")
	     (format "/%s:%s@%s:" "default-method""user" "default-host")))
    (should (string-equal (file-remote-p "/user@:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/user@:" 'user) "user"))
    (should (string-equal (file-remote-p "/user@:" 'host) "default-host"))
    (should (string-equal (file-remote-p "/user@:" 'localname) ""))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/user@host:")
	     (format "/%s:%s@%s:" "default-method" "user" "host")))
    (should (string-equal
	     (file-remote-p "/user@host:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/user@host:" 'user) "user"))
    (should (string-equal (file-remote-p "/user@host:" 'host) "host"))
    (should (string-equal (file-remote-p "/user@host:" 'localname) ""))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:host:")
	     (format "/%s:%s@%s:" "method" "default-user" "host")))
    (should (string-equal (file-remote-p "/method:host:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:host:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method:host:" 'host) "host"))
    (should (string-equal (file-remote-p "/method:host:" 'localname) ""))

    ;; Expand `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/method:user@:")
	     (format "/%s:%s@%s:" "method" "user" "default-host")))
    (should (string-equal (file-remote-p "/method:user@:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:user@:" 'user) "user"))
    (should (string-equal (file-remote-p "/method:user@:" 'host)
			  "default-host"))
    (should (string-equal (file-remote-p "/method:user@:" 'localname) ""))

    ;; No expansion.
    (should (string-equal
	     (file-remote-p "/method:user@host:")
	     (format "/%s:%s@%s:" "method" "user" "host")))
    (should (string-equal
	     (file-remote-p "/method:user@host:" 'method) "method"))
    (should (string-equal (file-remote-p "/method:user@host:" 'user) "user"))
    (should (string-equal (file-remote-p "/method:user@host:" 'host) "host"))
    (should (string-equal (file-remote-p "/method:user@host:" 'localname) ""))

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

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/host#1234:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "host#1234")))
    (should (string-equal
	     (file-remote-p "/host#1234:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/host#1234:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/host#1234:" 'host) "host#1234"))
    (should (string-equal (file-remote-p "/host#1234:" 'localname) ""))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/user@host#1234:")
	     (format "/%s:%s@%s:" "default-method" "user" "host#1234")))
    (should (string-equal
	     (file-remote-p "/user@host#1234:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/user@host#1234:" 'user) "user"))
    (should (string-equal (file-remote-p "/user@host#1234:" 'host) "host#1234"))
    (should (string-equal (file-remote-p "/user@host#1234:" 'localname) ""))

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

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/1.2.3.4:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "1.2.3.4")))
    (should (string-equal (file-remote-p "/1.2.3.4:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/1.2.3.4:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal (file-remote-p "/1.2.3.4:" 'localname) ""))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/user@1.2.3.4:")
	     (format "/%s:%s@%s:" "default-method" "user" "1.2.3.4")))
    (should (string-equal
	     (file-remote-p "/user@1.2.3.4:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/user@1.2.3.4:" 'user) "user"))
    (should (string-equal (file-remote-p "/user@1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal (file-remote-p "/user@1.2.3.4:" 'localname) ""))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:1.2.3.4:")
	     (format "/%s:%s@%s:" "method" "default-user" "1.2.3.4")))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:1.2.3.4:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'host) "1.2.3.4"))
    (should (string-equal (file-remote-p "/method:1.2.3.4:" 'localname) ""))

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

    ;; Expand `tramp-default-method', `tramp-default-user' and
    ;; `tramp-default-host'.
    (should (string-equal
	     (file-remote-p "/[]:")
	     (format
	      "/%s:%s@%s:" "default-method" "default-user" "default-host")))
    (should (string-equal (file-remote-p "/[]:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/[]:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/[]:" 'host) "default-host"))
    (should (string-equal (file-remote-p "/[]:" 'localname) ""))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (let ((tramp-default-host "::1"))
      (should (string-equal
	       (file-remote-p "/[]:")
	       (format "/%s:%s@%s:" "default-method" "default-user" "[::1]")))
      (should (string-equal (file-remote-p "/[]:" 'method) "default-method"))
      (should (string-equal (file-remote-p "/[]:" 'user) "default-user"))
      (should (string-equal (file-remote-p "/[]:" 'host) "::1"))
      (should (string-equal (file-remote-p "/[]:" 'localname) "")))

    ;; Expand `tramp-default-method' and `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/[::1]:")
	     (format "/%s:%s@%s:" "default-method" "default-user" "[::1]")))
    (should (string-equal (file-remote-p "/[::1]:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/[::1]:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/[::1]:" 'host) "::1"))
    (should (string-equal (file-remote-p "/[::1]:" 'localname) ""))

    ;; Expand `tramp-default-method'.
    (should (string-equal
	     (file-remote-p "/user@[::1]:")
	     (format "/%s:%s@%s:" "default-method" "user" "[::1]")))
    (should (string-equal
	     (file-remote-p "/user@[::1]:" 'method) "default-method"))
    (should (string-equal (file-remote-p "/user@[::1]:" 'user) "user"))
    (should (string-equal (file-remote-p "/user@[::1]:" 'host) "::1"))
    (should (string-equal (file-remote-p "/user@[::1]:" 'localname) ""))

    ;; Expand `tramp-default-user'.
    (should (string-equal
	     (file-remote-p "/method:[::1]:")
	     (format "/%s:%s@%s:" "method" "default-user" "[::1]")))
    (should (string-equal (file-remote-p "/method:[::1]:" 'method) "method"))
    (should (string-equal
	     (file-remote-p "/method:[::1]:" 'user) "default-user"))
    (should (string-equal (file-remote-p "/method:[::1]:" 'host) "::1"))
    (should (string-equal (file-remote-p "/method:[::1]:" 'localname) ""))

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

    ;; Local file name part.
    (should (string-equal (file-remote-p "/host:/:" 'localname) "/:"))
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
      (format "/%s:%s@%s:" "method2" "user2" "host2")))
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
       "/method1:user1@host1|method2:user2@host2|method3:user3@host3:/path/to/file")
      (format "/%s:%s@%s:" "method3" "user3" "host3")))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2|method3:user3@host3:/path/to/file"
       'method)
      "method3"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2|method3:user3@host3:/path/to/file"
       'user)
      "user3"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2|method3:user3@host3:/path/to/file"
       'host)
      "host3"))
    (should
     (string-equal
      (file-remote-p
       "/method1:user1@host1|method2:user2@host2|method3:user3@host3:/path/to/file"
       'localname)
      "/path/to/file"))))

(ert-deftest tramp-test03-file-name-defaults ()
  "Check default values for some methods."
  ;; Default values in tramp-adb.el.
  (should (string-equal (file-remote-p "/adb::" 'host) ""))
  ;; Default values in tramp-ftp.el.
  (should (string-equal (file-remote-p "/ftp.host:" 'method) "ftp"))
  (dolist (u '("ftp" "anonymous"))
    (should (string-equal (file-remote-p (format "/%s@:" u) 'method) "ftp")))
  ;; Default values in tramp-gvfs.el.
  (when (and (load "tramp-gvfs" 'noerror 'nomessage)
	     (symbol-value 'tramp-gvfs-enabled))
    (should (string-equal (file-remote-p "/synce::" 'user) nil)))
  ;; Default values in tramp-gw.el.
  (dolist (m '("tunnel" "socks"))
    (should
     (string-equal (file-remote-p (format "/%s::" m) 'user) (user-login-name))))
  ;; Default values in tramp-sh.el.
  (dolist (h `("127.0.0.1" "[::1]" "localhost" "localhost6" ,(system-name)))
    (should (string-equal (file-remote-p (format "/root@%s:" h) 'method) "su")))
  (dolist (m '("su" "sudo" "ksu"))
    (should (string-equal (file-remote-p (format "/%s::" m) 'user) "root")))
  (dolist (m '("rcp" "remcp" "rsh" "telnet" "krlogin" "fcp"))
    (should
     (string-equal (file-remote-p (format "/%s::" m) 'user) (user-login-name))))
  ;; Default values in tramp-smb.el.
  (should (string-equal (file-remote-p "/user%domain@host:" 'method) "smb"))
  (should (string-equal (file-remote-p "/smb::" 'user) nil)))

(ert-deftest tramp-test04-substitute-in-file-name ()
  "Check `substitute-in-file-name'."
  (should (string-equal (substitute-in-file-name "/method:host://foo") "/foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/path//foo") "/method:host:/foo"))
  (should
   (string-equal (substitute-in-file-name "/method:host:/path///foo")	"/foo"))
  (should
   (string-equal
    (substitute-in-file-name "/method:host:/path/~/foo") "/method:host:~/foo"))
  (should
   (string-equal (substitute-in-file-name "/method:host:/path//~/foo") "~/foo"))
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
      "/method:host:/path/$FOO"))))

(ert-deftest tramp-test05-expand-file-name ()
  "Check `expand-file-name'."
  (should
   (string-equal
    (expand-file-name "/method:host:/path/./file") "/method:host:/path/file"))
  (should
   (string-equal
    (expand-file-name "/method:host:/path/../file") "/method:host:/file")))

(ert-deftest tramp-test06-directory-file-name ()
  "Check `directory-file-name'.
This checks also `file-name-as-directory', `file-name-directory'
and `file-name-nondirectory'."
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
   (file-remote-p
    (unhandled-file-name-directory "/method:host:/path/to/file"))))

(ert-deftest tramp-test07-file-exists-p ()
  "Check `file-exist-p', `write-region' and `delete-file'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    (should-not (file-exists-p tmp-name))
    (write-region "foo" nil tmp-name)
    (should (file-exists-p tmp-name))
    (delete-file tmp-name)
    (should-not (file-exists-p tmp-name))))

(ert-deftest tramp-test08-file-local-copy ()
  "Check `file-local-copy'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name1 (tramp--test-make-temp-name))
	tmp-name2)
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (should (setq tmp-name2 (file-local-copy tmp-name1)))
	  (with-temp-buffer
	    (insert-file-contents tmp-name2)
	    (should (string-equal (buffer-string) "foo"))))
      (ignore-errors
	(delete-file tmp-name1)
	(delete-file tmp-name2)))))

(ert-deftest tramp-test09-insert-file-contents ()
  "Check `insert-file-contents'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name)
	  (with-temp-buffer
	    (insert-file-contents tmp-name)
	    (should (string-equal (buffer-string) "foo"))
	    (insert-file-contents tmp-name)
	    (should (string-equal (buffer-string) "foofoo"))
	    ;; Insert partly.
	    (insert-file-contents tmp-name nil 1 3)
	    (should (string-equal (buffer-string) "oofoofoo"))
	    ;; Replace.
	    (insert-file-contents tmp-name nil nil nil 'replace)
	    (should (string-equal (buffer-string) "foo"))))
      (ignore-errors (delete-file tmp-name)))))

(ert-deftest tramp-test10-write-region ()
  "Check `write-region'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    (unwind-protect
	(progn
	  (with-temp-buffer
	    (insert "foo")
	    (write-region nil nil tmp-name))
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
	    (should (string-equal (buffer-string) "34"))))
      (ignore-errors (delete-file tmp-name)))))

(ert-deftest tramp-test11-copy-file ()
  "Check `copy-file'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name1 (tramp--test-make-temp-name))
	(tmp-name2 (tramp--test-make-temp-name))
	(tmp-name3 (tramp--test-make-temp-name))
	(tmp-name4 (tramp--test-make-temp-name 'local))
	(tmp-name5 (tramp--test-make-temp-name 'local)))

    ;; Copy on remote side.
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (copy-file tmp-name1 tmp-name2)
	  (should (file-exists-p tmp-name2))
	  (with-temp-buffer
	    (insert-file-contents tmp-name2)
	    (should (string-equal (buffer-string) "foo")))
	  (should-error (copy-file tmp-name1 tmp-name2))
	  (copy-file tmp-name1 tmp-name2 'ok)
	  (make-directory tmp-name3)
 	  (copy-file tmp-name1 tmp-name3)
	  (should
	   (file-exists-p
	    (expand-file-name (file-name-nondirectory tmp-name1) tmp-name3))))
      (ignore-errors (delete-file tmp-name1))
      (ignore-errors (delete-file tmp-name2))
      (ignore-errors (delete-directory tmp-name3 'recursive)))

    ;; Copy from remote side to local side.
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (copy-file tmp-name1 tmp-name4)
	  (should (file-exists-p tmp-name4))
	  (with-temp-buffer
	    (insert-file-contents tmp-name4)
	    (should (string-equal (buffer-string) "foo")))
	  (should-error (copy-file tmp-name1 tmp-name4))
	  (copy-file tmp-name1 tmp-name4 'ok)
	  (make-directory tmp-name5)
 	  (copy-file tmp-name1 tmp-name5)
	  (should
	   (file-exists-p
	    (expand-file-name (file-name-nondirectory tmp-name1) tmp-name5))))
      (ignore-errors (delete-file tmp-name1))
      (ignore-errors (delete-file tmp-name4))
      (ignore-errors (delete-directory tmp-name5 'recursive)))

    ;; Copy from local side to remote side.
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name4 nil 'nomessage)
	  (copy-file tmp-name4 tmp-name1)
	  (should (file-exists-p tmp-name1))
	  (with-temp-buffer
	    (insert-file-contents tmp-name1)
	    (should (string-equal (buffer-string) "foo")))
	  (should-error (copy-file tmp-name4 tmp-name1))
	  (copy-file tmp-name4 tmp-name1 'ok)
	  (make-directory tmp-name3)
 	  (copy-file tmp-name4 tmp-name3)
	  (should
	   (file-exists-p
	    (expand-file-name (file-name-nondirectory tmp-name4) tmp-name3))))
      (ignore-errors (delete-file tmp-name1))
      (ignore-errors (delete-file tmp-name4))
      (ignore-errors (delete-directory tmp-name3 'recursive)))))

(ert-deftest tramp-test12-rename-file ()
  "Check `rename-file'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name1 (tramp--test-make-temp-name))
	(tmp-name2 (tramp--test-make-temp-name))
	(tmp-name3 (tramp--test-make-temp-name))
	(tmp-name4 (tramp--test-make-temp-name 'local))
	(tmp-name5 (tramp--test-make-temp-name 'local)))

    ;; Rename on remote side.
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (rename-file tmp-name1 tmp-name2)
	  (should-not (file-exists-p tmp-name1))
	  (should (file-exists-p tmp-name2))
	  (with-temp-buffer
	    (insert-file-contents tmp-name2)
	    (should (string-equal (buffer-string) "foo")))
	  (write-region "foo" nil tmp-name1)
	  (should-error (rename-file tmp-name1 tmp-name2))
	  (rename-file tmp-name1 tmp-name2 'ok)
	  (should-not (file-exists-p tmp-name1))
	  (write-region "foo" nil tmp-name1)
	  (make-directory tmp-name3)
 	  (rename-file tmp-name1 tmp-name3)
	  (should-not (file-exists-p tmp-name1))
	  (should
	   (file-exists-p
	    (expand-file-name (file-name-nondirectory tmp-name1) tmp-name3))))
      (ignore-errors (delete-file tmp-name1))
      (ignore-errors (delete-file tmp-name2))
      (ignore-errors (delete-directory tmp-name3 'recursive)))

    ;; Rename from remote side to local side.
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (rename-file tmp-name1 tmp-name4)
	  (should-not (file-exists-p tmp-name1))
	  (should (file-exists-p tmp-name4))
	  (with-temp-buffer
	    (insert-file-contents tmp-name4)
	    (should (string-equal (buffer-string) "foo")))
	  (write-region "foo" nil tmp-name1)
	  (should-error (rename-file tmp-name1 tmp-name4))
	  (rename-file tmp-name1 tmp-name4 'ok)
	  (should-not (file-exists-p tmp-name1))
	  (write-region "foo" nil tmp-name1)
	  (make-directory tmp-name5)
 	  (rename-file tmp-name1 tmp-name5)
	  (should-not (file-exists-p tmp-name1))
	  (should
	   (file-exists-p
	    (expand-file-name (file-name-nondirectory tmp-name1) tmp-name5))))
      (ignore-errors (delete-file tmp-name1))
      (ignore-errors (delete-file tmp-name4))
      (ignore-errors (delete-directory tmp-name5 'recursive)))

    ;; Rename from local side to remote side.
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name4 nil 'nomessage)
	  (rename-file tmp-name4 tmp-name1)
	  (should-not (file-exists-p tmp-name4))
	  (should (file-exists-p tmp-name1))
	  (with-temp-buffer
	    (insert-file-contents tmp-name1)
	    (should (string-equal (buffer-string) "foo")))
	  (write-region "foo" nil tmp-name4 nil 'nomessage)
	  (should-error (rename-file tmp-name4 tmp-name1))
	  (rename-file tmp-name4 tmp-name1 'ok)
	  (should-not (file-exists-p tmp-name4))
	  (write-region "foo" nil tmp-name4 nil 'nomessage)
	  (make-directory tmp-name3)
 	  (rename-file tmp-name4 tmp-name3)
	  (should-not (file-exists-p tmp-name4))
	  (should
	   (file-exists-p
	    (expand-file-name (file-name-nondirectory tmp-name4) tmp-name3))))
      (ignore-errors (delete-file tmp-name1))
      (ignore-errors (delete-file tmp-name4))
      (ignore-errors (delete-directory tmp-name3 'recursive)))))

(ert-deftest tramp-test13-make-directory ()
  "Check `make-directory'.
This tests also `file-directory-p' and `file-accessible-directory-p'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    (unwind-protect
	(progn
	  (make-directory tmp-name)
	  (should (file-directory-p tmp-name))
	  (should (file-accessible-directory-p tmp-name)))
      (ignore-errors (delete-directory tmp-name)))))

(ert-deftest tramp-test14-delete-directory ()
  "Check `delete-directory'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    ;; Delete empty directory.
    (make-directory tmp-name)
    (should (file-directory-p tmp-name))
    (delete-directory tmp-name)
    (should-not (file-directory-p tmp-name))
    ;; Delete non-empty directory.
    (make-directory tmp-name)
    (write-region "foo" nil (expand-file-name "bla" tmp-name))
    (should-error (delete-directory tmp-name) :type 'file-error)
    (delete-directory tmp-name 'recursive)
    (should-not (file-directory-p tmp-name))))

(ert-deftest tramp-test15-copy-directory ()
  "Check `copy-directory'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (eq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     'tramp-smb-file-name-handler)))

  (let* ((tmp-name1 (tramp--test-make-temp-name))
	 (tmp-name2 (tramp--test-make-temp-name))
	 (tmp-name3 (expand-file-name
		     (file-name-nondirectory tmp-name1) tmp-name2))
	 (tmp-name4 (expand-file-name "foo" tmp-name1))
	 (tmp-name5 (expand-file-name "foo" tmp-name2))
	 (tmp-name6 (expand-file-name "foo" tmp-name3)))
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
	  (copy-directory tmp-name1 tmp-name2)
	  (should (file-directory-p tmp-name3))
	  (should (file-exists-p tmp-name6)))
      (ignore-errors
	(delete-directory tmp-name1 'recursive)
	(delete-directory tmp-name2 'recursive)))))

(ert-deftest tramp-test16-directory-files ()
  "Check `directory-files'."
  (skip-unless (tramp--test-enabled))

  (let* ((tmp-name1 (tramp--test-make-temp-name))
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
      (ignore-errors (delete-directory tmp-name1 'recursive)))))

(ert-deftest tramp-test17-insert-directory ()
  "Check `insert-directory'."
  (skip-unless (tramp--test-enabled))

  (let* ((tmp-name1 (tramp--test-make-temp-name))
	 (tmp-name2 (expand-file-name "foo" tmp-name1)))
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
	  (with-temp-buffer
	    (insert-directory tmp-name1 "-al")
	    (goto-char (point-min))
	    (should (looking-at-p (format "^.+ %s$" (regexp-quote tmp-name1)))))
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
	      "\\(total.+[[:digit:]]+\n\\)?.+ \\.\n.+ \\.\\.\n.+ foo$"))))
      (ignore-errors (delete-directory tmp-name1 'recursive)))))

(ert-deftest tramp-test18-file-attributes ()
  "Check `file-attributes'.
This tests also `file-readable-p' and `file-regular-p'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name))
	attr)
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name)
	  (should (file-exists-p tmp-name))
	  (setq attr (file-attributes tmp-name))
	  (should (consp attr))
	  (should (file-exists-p tmp-name))
	  (should (file-readable-p tmp-name))
	  (should (file-regular-p tmp-name))
	  ;; We do not test inodes and device numbers.
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

	  (setq attr (file-attributes tmp-name 'string))
          (should (stringp (nth 2 attr))) ;; Uid.
          (should (stringp (nth 3 attr))) ;; Gid.
	  (delete-file tmp-name)

	  (make-directory tmp-name)
	  (should (file-exists-p tmp-name))
	  (should (file-readable-p tmp-name))
	  (should-not (file-regular-p tmp-name))
	  (setq attr (file-attributes tmp-name))
	  (should (eq (car attr) t)))
      (ignore-errors (delete-directory tmp-name)))))

(ert-deftest tramp-test19-directory-files-and-attributes ()
  "Check `directory-files-and-attributes'."
  (skip-unless (tramp--test-enabled))

  ;; `directory-files-and-attributes' contains also values for "../".
  ;; Ensure that this doesn't change during tests, for
  ;; example due to handling temporary files.
  (let* ((tmp-name1 (tramp--test-make-temp-name))
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
		 (nth 5
		      (file-attributes (expand-file-name (car elt) tmp-name2)))
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
      (ignore-errors (delete-directory tmp-name1 'recursive)))))

(ert-deftest tramp-test20-file-modes ()
  "Check `file-modes'.
This tests also `file-executable-p', `file-writable-p' and `set-file-modes'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (memq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     '(tramp-adb-file-name-handler
       tramp-gvfs-file-name-handler
       tramp-smb-file-name-handler))))

  (let ((tmp-name (tramp--test-make-temp-name)))
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
      (ignore-errors (delete-file tmp-name)))))

(ert-deftest tramp-test21-file-links ()
  "Check `file-symlink-p'.
This tests also `make-symbolic-link', `file-truename' and `add-name-to-file'."
  (skip-unless (tramp--test-enabled))

  ;; We must use `file-truename' for the temporary directory, because
  ;; it could be located on a symlinked directory.  This would let the
  ;; test fail.
  (let* ((tramp-test-temporary-file-directory
	  (file-truename tramp-test-temporary-file-directory))
	 (tmp-name1 (tramp--test-make-temp-name))
	 (tmp-name2 (tramp--test-make-temp-name))
	 (tmp-name3 (tramp--test-make-temp-name 'local)))
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (should (file-exists-p tmp-name1))
	  ;; Method "smb" supports `make-symbolic-link' only if the
	  ;; remote host has CIFS capabilities.  tramp-adb.el and
	  ;; tramp-gvfs.el do not support symbolic links at all.
	  (condition-case err
	      (make-symbolic-link tmp-name1 tmp-name2)
	    (file-error
	     (skip-unless
	      (not (string-equal (error-message-string err)
				 "make-symbolic-link not supported")))))
	  (should (file-symlink-p tmp-name2))
	  (should-error (make-symbolic-link tmp-name1 tmp-name2))
	  (make-symbolic-link tmp-name1 tmp-name2 'ok-if-already-exists)
	  (should (file-symlink-p tmp-name2))
	  ;; `tmp-name3' is a local file name.
	  (should-error (make-symbolic-link tmp-name1 tmp-name3)))
      (ignore-errors
	(delete-file tmp-name1)
	(delete-file tmp-name2)))

    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (should (file-exists-p tmp-name1))
	  (add-name-to-file tmp-name1 tmp-name2)
	  (should-not (file-symlink-p tmp-name2))
	  (should-error (add-name-to-file tmp-name1 tmp-name2))
	  (add-name-to-file tmp-name1 tmp-name2 'ok-if-already-exists)
	  (should-not (file-symlink-p tmp-name2))
	  ;; `tmp-name3' is a local file name.
	  (should-error (add-name-to-file tmp-name1 tmp-name3)))
      (ignore-errors
	(delete-file tmp-name1)
	(delete-file tmp-name2)))

    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (should (file-exists-p tmp-name1))
	  (make-symbolic-link tmp-name1 tmp-name2)
	  (should (file-symlink-p tmp-name2))
	  (should-not (string-equal tmp-name2 (file-truename tmp-name2)))
	  (should
	   (string-equal (file-truename tmp-name1) (file-truename tmp-name2)))
	  (should (file-equal-p tmp-name1 tmp-name2)))
      (ignore-errors
	(delete-file tmp-name1)
	(delete-file tmp-name2)))

    ;; `file-truename' shall preserve trailing link of directories.
    (unless (file-symlink-p tramp-test-temporary-file-directory)
      (let* ((dir1 (directory-file-name tramp-test-temporary-file-directory))
	     (dir2 (file-name-as-directory dir1)))
	(should (string-equal (file-truename dir1) (expand-file-name dir1)))
	(should (string-equal (file-truename dir2) (expand-file-name dir2)))))))

(ert-deftest tramp-test22-file-times ()
  "Check `set-file-times' and `file-newer-than-file-p'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (memq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     '(tramp-gvfs-file-name-handler tramp-smb-file-name-handler))))

  (let ((tmp-name1 (tramp--test-make-temp-name))
	(tmp-name2 (tramp--test-make-temp-name))
	(tmp-name3 (tramp--test-make-temp-name)))
    (unwind-protect
	(progn
	  (write-region "foo" nil tmp-name1)
	  (should (file-exists-p tmp-name1))
	  (should (consp (nth 5 (file-attributes tmp-name1))))
	  ;; '(0 0) means don't know, and will be replaced by
	  ;; `current-time'.  Therefore, we use '(0 1).
	  ;; We skip the test, if the remote handler is not able to
	  ;; set the correct time.
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
      (ignore-errors
	(delete-file tmp-name1)
	(delete-file tmp-name2)))))

(ert-deftest tramp-test23-visited-file-modtime ()
  "Check `set-visited-file-modtime' and `verify-visited-file-modtime'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
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
      (ignore-errors (delete-file tmp-name)))))

(ert-deftest tramp-test24-file-name-completion ()
  "Check `file-name-completion' and `file-name-all-completions'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    (unwind-protect
	(progn
	  (make-directory tmp-name)
	  (should (file-directory-p tmp-name))
	  (write-region "foo" nil (expand-file-name "foo" tmp-name))
	  (write-region "bar" nil (expand-file-name "bold" tmp-name))
	  (make-directory (expand-file-name "boz" tmp-name))
	  (should (equal (file-name-completion "fo" tmp-name) "foo"))
	  (should (equal (file-name-completion "b" tmp-name) "bo"))
	  (should
	   (equal (file-name-completion "b" tmp-name 'file-directory-p) "boz/"))
	  (should (equal (file-name-all-completions "fo" tmp-name) '("foo")))
	  (should
	   (equal (sort (file-name-all-completions "b" tmp-name) 'string-lessp)
		  '("bold" "boz/"))))
      (ignore-errors (delete-directory tmp-name 'recursive)))))

(ert-deftest tramp-test25-load ()
  "Check `load'."
  (skip-unless (tramp--test-enabled))

  (let ((tmp-name (tramp--test-make-temp-name)))
    (unwind-protect
	(progn
	  (load tmp-name 'noerror 'nomessage)
	  (should-not (featurep 'tramp-test-load))
	  (write-region "(provide 'tramp-test-load)" nil tmp-name)
	  ;; `load' in lread.c does not pass `must-suffix'.  Why?
	  ;(should-error (load tmp-name nil 'nomessage 'nosuffix 'must-suffix))
	  (load tmp-name nil 'nomessage 'nosuffix)
	  (should (featurep 'tramp-test-load)))
      (ignore-errors
	(and (featurep 'tramp-test-load) (unload-feature 'tramp-test-load))
	(delete-file tmp-name)))))

(ert-deftest tramp-test26-process-file ()
  "Check `process-file'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (memq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     '(tramp-gvfs-file-name-handler tramp-smb-file-name-handler))))

  (let ((tmp-name (tramp--test-make-temp-name))
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
	    (should
	     (zerop
	      (process-file "ls" nil t nil (file-name-nondirectory tmp-name))))
	    ;; `ls' could produce colorized output.
	    (goto-char (point-min))
	    (while (re-search-forward tramp-color-escape-sequence-regexp nil t)
	      (replace-match "" nil nil))
	    (should
	     (string-equal
	      (format "%s\n" (file-name-nondirectory tmp-name))
	      (buffer-string)))))
      (ignore-errors (delete-file tmp-name)))))

(ert-deftest tramp-test27-start-file-process ()
  "Check `start-file-process'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (memq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     '(tramp-adb-file-name-handler
       tramp-gvfs-file-name-handler
       tramp-smb-file-name-handler))))

  (let ((default-directory tramp-test-temporary-file-directory)
	(tmp-name (tramp--test-make-temp-name))
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
	      (accept-process-output proc 1)))
	  (should (string-equal (buffer-string) "foo")))
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
	      (accept-process-output proc 1)))
	  (should (string-equal (buffer-string) "foo")))
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
	      (accept-process-output proc 1)))
	  (should (string-equal (buffer-string) "foo")))
      (ignore-errors (delete-process proc)))))

(ert-deftest tramp-test28-shell-command ()
  "Check `shell-command'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (memq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     '(tramp-adb-file-name-handler
       tramp-gvfs-file-name-handler
       tramp-smb-file-name-handler))))

  (let ((tmp-name (tramp--test-make-temp-name))
	(default-directory tramp-test-temporary-file-directory)
	kill-buffer-query-functions)
    (unwind-protect
	(with-temp-buffer
	  (write-region "foo" nil tmp-name)
	  (should (file-exists-p tmp-name))
	  (shell-command
	   (format "ls %s" (file-name-nondirectory tmp-name)) (current-buffer))
	  ;; `ls' could produce colorized output.
	  (goto-char (point-min))
	  (while (re-search-forward tramp-color-escape-sequence-regexp nil t)
	    (replace-match "" nil nil))
	  (should
	   (string-equal
	    (format "%s\n" (file-name-nondirectory tmp-name)) (buffer-string))))
      (ignore-errors (delete-file tmp-name)))

    (unwind-protect
        (with-temp-buffer
          (write-region "foo" nil tmp-name)
	  (should (file-exists-p tmp-name))
          (async-shell-command
	   (format "ls %s" (file-name-nondirectory tmp-name)) (current-buffer))
	  (set-process-sentinel (get-buffer-process (current-buffer)) nil)
	  ;; Read output.
	  (with-timeout (10 (ert-fail "`async-shell-command' timed out"))
	    (while (< (- (point-max) (point-min))
		      (1+ (length (file-name-nondirectory tmp-name))))
	      (accept-process-output (get-buffer-process (current-buffer)) 1)))
	  ;; `ls' could produce colorized output.
	  (goto-char (point-min))
	  (while (re-search-forward tramp-color-escape-sequence-regexp nil t)
	    (replace-match "" nil nil))
	  ;; There might be a nasty "Process *Async Shell* finished" message.
	  (goto-char (point-min))
	  (forward-line)
	  (narrow-to-region (point-min) (point))
	  (should
	   (string-equal
	    (format "%s\n" (file-name-nondirectory tmp-name)) (buffer-string))))
      (ignore-errors (delete-file tmp-name)))

    (unwind-protect
	(with-temp-buffer
          (write-region "foo" nil tmp-name)
	  (should (file-exists-p tmp-name))
	  (async-shell-command "read line; ls $line" (current-buffer))
	  (set-process-sentinel (get-buffer-process (current-buffer)) nil)
	  (process-send-string
	   (get-buffer-process (current-buffer))
	   (format "%s\n" (file-name-nondirectory tmp-name)))
	  ;; Read output.
	  (with-timeout (10 (ert-fail "`async-shell-command' timed out"))
	    (while (< (- (point-max) (point-min))
		      (1+ (length (file-name-nondirectory tmp-name))))
	      (accept-process-output (get-buffer-process (current-buffer)) 1)))
	  ;; `ls' could produce colorized output.
	  (goto-char (point-min))
	  (while (re-search-forward tramp-color-escape-sequence-regexp nil t)
	    (replace-match "" nil nil))
	  ;; There might be a nasty "Process *Async Shell* finished" message.
	  (goto-char (point-min))
	  (forward-line)
	  (narrow-to-region (point-min) (point))
	  (should
	   (string-equal
	    (format "%s\n" (file-name-nondirectory tmp-name)) (buffer-string))))
      (ignore-errors (delete-file tmp-name)))))

(ert-deftest tramp-test29-vc-registered ()
  "Check `vc-registered'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (eq
    (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
    'tramp-sh-file-name-handler))

  (let* ((default-directory tramp-test-temporary-file-directory)
	 (tmp-name1 (tramp--test-make-temp-name))
	 (tmp-name2 (expand-file-name "foo" tmp-name1))
	 (tramp-remote-process-environment tramp-remote-process-environment)
	 (vc-handled-backends
	  (with-parsed-tramp-file-name tramp-test-temporary-file-directory nil
	    (cond
	     ((tramp-find-executable v vc-bzr-program (tramp-get-remote-path v))
	      (setq tramp-remote-process-environment
		    (cons (format "BZR_HOME=%s"
				  (file-remote-p tmp-name1 'localname))
			  tramp-remote-process-environment))
	      ;; We must force a reconnect, in order to activate $BZR_HOME.
	      (tramp-cleanup-connection
	       (tramp-dissect-file-name tramp-test-temporary-file-directory)
	       nil 'keep-password)
	      '(Bzr))
	     ((tramp-find-executable v vc-git-program (tramp-get-remote-path v))
	      '(Git))
	     ((tramp-find-executable v vc-hg-program (tramp-get-remote-path v))
	      '(Hg))
	     (t nil)))))
    (skip-unless vc-handled-backends)
    (message "%s" vc-handled-backends)

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
	    (vc-create-repo (car vc-handled-backends))
	    ;; The structure of VC-FILESET is not documented.  Let's
	    ;; hope it won't change.
	    (vc-register
	     nil (list (car vc-handled-backends)
		       (list (file-name-nondirectory tmp-name2)))))
	  (should (vc-registered tmp-name2)))

	(ignore-errors (delete-directory tmp-name1 'recursive)))))

(defun tramp--test-check-files (&rest files)
  "Runs a simple but comprehensive test over every file in FILES."
  (let ((tmp-name1 (tramp--test-make-temp-name))
	(tmp-name2 (tramp--test-make-temp-name 'local)))
    (unwind-protect
	(progn
	  (make-directory tmp-name1)
	  (make-directory tmp-name2)
	  (dolist (elt files)
	    (let ((file1 (expand-file-name elt tmp-name1))
		  (file2 (expand-file-name elt tmp-name2)))
	      (write-region elt nil file1)
	      (should (file-exists-p file1))
	      ;; Check file contents.
	      (with-temp-buffer
		(insert-file-contents file1)
		(should (string-equal (buffer-string) elt)))
	      ;; Copy file both directions.
	      (copy-file file1 tmp-name2)
	      (should (file-exists-p file2))
	      (delete-file file1)
	      (should-not (file-exists-p file1))
	      (copy-file file2 tmp-name1)
	      (should (file-exists-p file1))))

	  ;; Check file names.
	  (should (equal (directory-files
			  tmp-name1 nil directory-files-no-dot-files-regexp)
			 (sort (copy-sequence files) 'string-lessp)))
	  (should (equal (directory-files
			  tmp-name2 nil directory-files-no-dot-files-regexp)
			 (sort (copy-sequence files) 'string-lessp)))

	  ;; `substitute-in-file-name' could return different values.
	  ;; For `adb', there could be strange file permissions
	  ;; preventing overwriting a file.  We don't care in this
	  ;; testcase.
	  (dolist (elt files)
	    (let ((file1
		   (substitute-in-file-name (expand-file-name elt tmp-name1)))
		  (file2
		   (substitute-in-file-name (expand-file-name elt tmp-name2))))
	      (ignore-errors (write-region elt nil file1))
	      (should (file-exists-p file1))
	      (ignore-errors (write-region elt nil file2 nil 'nomessage))
	      (should (file-exists-p file2))))

	  (should (equal (directory-files
			  tmp-name1 nil directory-files-no-dot-files-regexp)
			 (directory-files
			  tmp-name2 nil directory-files-no-dot-files-regexp))))

      (ignore-errors (delete-directory tmp-name1 'recursive))
      (ignore-errors (delete-directory tmp-name2 'recursive)))))

;; This test is inspired by Bug#17238.
(ert-deftest tramp-test30-special-characters ()
  "Check special characters in file names."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (not
    (memq
     (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
     '(tramp-adb-file-name-handler
       tramp-gvfs-file-name-handler
       tramp-smb-file-name-handler))))

  ;; Newlines, slashes and backslashes in file names are not supported.
  ;; So we don't test.
  (tramp--test-check-files
   " foo\tbar baz\t"
   "$foo$bar$$baz$"
   "-foo-bar-baz-"
   "%foo%bar%baz%"
   "&foo&bar&baz&"
   "?foo?bar?baz?"
   "*foo*bar*baz*"
   "'foo\"bar'baz\""
   "#foo~bar#baz~"
   "!foo|bar!baz|"
   ":foo;bar:baz;"
   "<foo>bar<baz>"
   "(foo)bar(baz)"
   "[foo]bar[baz]"
   "{foo}bar{baz}"))

(ert-deftest tramp-test31-utf8 ()
  "Check UTF8 encoding in file names and file contents."
  (skip-unless (tramp--test-enabled))

  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(file-name-coding-system 'utf-8))
    (tramp--test-check-files
     "أصبح بوسعك الآن تنزيل نسخة كاملة من موسوعة ويكيبيديا العربية لتصفحها بلا اتصال بالإنترنت"
     "银河系漫游指南系列"
     "Автостопом по гала́ктике")))

;; This test is inspired by Bug#16928.
(ert-deftest tramp-test32-asynchronous-requests ()
  "Check parallel asynchronous requests.
Such requests could arrive from timers, process filters and
process sentinels.  They shall not disturb each other."
  ;; Mark as failed until bug has been fixed.
  :expected-result :failed
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (eq
    (tramp-find-foreign-file-name-handler tramp-test-temporary-file-directory)
    'tramp-sh-file-name-handler))

  ;; Keep instrumentation verbosity 0 until Tramp bug is fixed.  This
  ;; has the side effect, that this test fails instead to abort.  Good
  ;; for hydra.
  (tramp--instrument-test-case 0
  (let* ((tmp-name (tramp--test-make-temp-name))
	 (default-directory tmp-name)
	 (remote-file-name-inhibit-cache t)
	 timer buffers kill-buffer-query-functions)

    (unwind-protect
	(progn
	  (make-directory tmp-name)

	  ;; Setup a timer in order to raise an ordinary command again
	  ;; and again.  `vc-registered' is well suited, because there
	  ;; are many checks.
	  (setq
	   timer
	   (run-at-time
	    0 1
	    (lambda ()
	      (when buffers
		(vc-registered
		 (buffer-name (nth (random (length buffers)) buffers)))))))

	  ;; Create temporary buffers.  The number of buffers
	  ;; corresponds to the number of processes; it could be
	  ;; increased in order to make pressure on Tramp.
	  (dotimes (i 5)
	    (add-to-list 'buffers (generate-new-buffer "*temp*")))

	  ;; Open asynchronous processes.  Set process sentinel.
	  (dolist (buf buffers)
	    (async-shell-command "read line; touch $line; echo $line" buf)
	    (set-process-sentinel
	     (get-buffer-process buf)
	     (lambda (proc _state)
	       (delete-file (buffer-name (process-buffer proc))))))

	  ;; Send a string.  Use a random order of the buffers.  Mix
	  ;; with regular operation.
	  (let ((buffers (copy-sequence buffers))
		buf)
	    (while buffers
	      (setq buf (nth (random (length buffers)) buffers))
	      (process-send-string
	       (get-buffer-process buf) (format "'%s'\n" buf))
	      (file-attributes (buffer-name buf))
	      (setq buffers (delq buf buffers))))

	  ;; Wait until the whole output has been read.
	  (with-timeout ((* 10 (length buffers))
			 (ert-fail "`async-shell-command' timed out"))
	    (let ((buffers (copy-sequence buffers))
		  buf)
	      (while buffers
		(setq buf (nth (random (length buffers)) buffers))
		(if (ignore-errors
		      (memq (process-status (get-buffer-process buf))
			    '(run open)))
		    (accept-process-output (get-buffer-process buf) 0.1)
		  (setq buffers (delq buf buffers))))))

	  ;; Check.
	  (dolist (buf buffers)
	    (with-current-buffer buf
	      (should
	       (string-equal (format "'%s'\n" buf) (buffer-string)))))
	  (should-not
	   (directory-files tmp-name nil directory-files-no-dot-files-regexp)))

      ;; Cleanup.
      (ignore-errors (cancel-timer timer))
      (ignore-errors (delete-directory tmp-name 'recursive))
      (dolist (buf buffers)
	(ignore-errors (kill-buffer buf)))))))

(ert-deftest tramp-test33-recursive-load ()
  "Check that Tramp does not fail due to recursive load."
  (skip-unless (tramp--test-enabled))

  (dolist (code
	   (list
	    (format
	     "(expand-file-name %S)"
	     tramp-test-temporary-file-directory)
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
	(expand-file-name invocation-name invocation-directory)
	(mapconcat 'shell-quote-argument load-path " -L ")
	(shell-quote-argument code)))))))

(ert-deftest tramp-test34-unload ()
  "Check that Tramp and its subpackages unload completely.
Since it unloads Tramp, it shall be the last test to run."
  ;; Mark as failed until all symbols are unbound.
  :expected-result (if (featurep 'tramp) :failed :passed)
  (when (featurep 'tramp)
    (unload-feature 'tramp 'force)
    ;; No Tramp feature must be left.
    (should-not (featurep 'tramp))
    (should-not (all-completions "tramp" (delq 'tramp-tests features)))
    ;; `file-name-handler-alist' must be clean.
    (should-not (all-completions "tramp" (mapcar 'cdr file-name-handler-alist)))
    ;; There shouldn't be left a bound symbol.  We do not regard our
    ;; test symbols, and the Tramp unload hooks.
    (mapatoms
     (lambda (x)
       (and (or (boundp x) (functionp x))
	    (string-match "^tramp" (symbol-name x))
	    (not (string-match "^tramp--?test" (symbol-name x)))
	    (not (string-match "unload-hook$" (symbol-name x)))
	    (ert-fail (format "`%s' still bound" x)))))
;	    (progn (message "`%s' still bound" x)))))
    ;; There shouldn't be left a hook function containing a Tramp
    ;; function.  We do not regard the Tramp unload hooks.
    (mapatoms
     (lambda (x)
       (and (boundp x)
	    (string-match "-hooks?$" (symbol-name x))
	    (not (string-match "unload-hook$" (symbol-name x)))
	    (consp (symbol-value x))
	    (ignore-errors (all-completions "tramp" (symbol-value x)))
	    (ert-fail (format "Hook `%s' still contains Tramp function" x)))))))

;; TODO:

;; * dired-compress-file
;; * dired-uncache
;; * file-acl
;; * file-ownership-preserved-p
;; * file-selinux-context
;; * find-backup-file-name
;; * make-auto-save-file-name
;; * set-file-acl
;; * set-file-selinux-context

;; * Work on skipped tests.  Make a comment, when it is impossible.
;; * Fix `tramp-test15-copy-directory' for `smb'.  Using tar in a pipe
;;   doesn't work well when an interactive password must be provided.
;; * Fix `tramp-test27-start-file-process' for `nc' and on MS
;;   Windows (`process-send-eof'?).
;; * Fix `tramp-test28-shell-command' on MS Windows (nasty plink message).
;; * Fix `tramp-test30-special-characters' for `adb', `nc' and `smb'.
;; * Fix `tramp-test31-utf8' for MS Windows and `nc'/`telnet' (when
;;   target is a dumb busybox).  Seems to be in `directory-files'.
;; * Fix Bug#16928.  Set expected error of `tramp-test32-asynchronous-requests'.
;; * Fix `tramp-test34-unload' (Not all symbols are unbound).  Set
;;   expected error.

(defun tramp-test-all (&optional interactive)
  "Run all tests for \\[tramp]."
  (interactive "p")
  (funcall
   (if interactive 'ert-run-tests-interactively 'ert-run-tests-batch) "^tramp"))

(provide 'tramp-tests)
;;; tramp-tests.el ends here
