;;; tramp-tests.el --- Tests of remote file access  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

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

;; All temporary Tramp test files are removed prior test run.
;; Therefore, two test runs cannot be performed in parallel.

;; The environment variable $TRAMP_TEST_CLEANUP_TEMP_FILES, when set,
;; forces the removal of all temporary Tramp files prior test run.
;; This shouldn't be set if the test suite runs in parallel using
;; Tramp on a production system.

;; For slow remote connections, `tramp-test45-asynchronous-requests'
;; might be too heavy.  Setting $REMOTE_PARALLEL_PROCESSES to a proper
;; value less than 10 could help.

;; This test suite obeys the environment variables $EMACS_HYDRA_CI and
;; $EMACS_EMBA_CI, used on the Emacs CI/CD platforms.

;; The following test tags are used: `:expensive-test',
;; `:tramp-asynchronous-processes' and `:unstable'.

;; A whole test run can be performed calling the command `tramp-test-all'.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'dired-aux)
(require 'tramp)
(require 'ert-x)
(require 'tar-mode)
(require 'trace)
(require 'vc)
(require 'vc-bzr)
(require 'vc-git)
(require 'vc-hg)

(declare-function tramp-check-remote-uname "tramp-sh")
(declare-function tramp-find-executable "tramp-sh")
(declare-function tramp-get-remote-chmod-h "tramp-sh")
(declare-function tramp-get-remote-path "tramp-sh")
(declare-function tramp-get-remote-perl "tramp-sh")
(declare-function tramp-get-remote-stat "tramp-sh")
(declare-function tramp-list-tramp-buffers "tramp-cmds")
(declare-function tramp-method-out-of-band-p "tramp-sh")
(declare-function tramp-smb-get-localname "tramp-smb")
(defvar ange-ftp-make-backup-files)
(defvar comp-warn-primitives)
(defvar tramp-connection-properties)
(defvar tramp-copy-size-limit)
(defvar tramp-fuse-remove-hidden-files)
(defvar tramp-fuse-unmount-on-cleanup)
(defvar tramp-inline-compress-start-size)
(defvar tramp-persistency-file-name)
(defvar tramp-remote-path)
(defvar tramp-remote-process-environment)
(defvar tramp-use-connection-share)

;; Declared in Emacs 30.1.
(defvar remote-file-name-access-timeout)
(defvar remote-file-name-inhibit-delete-by-moving-to-trash)

;; `ert-remote-temporary-file-directory' was introduced in Emacs 29.1.
;; Adapting `tramp-remote-path' happens also there.
(unless (boundp 'ert-remote-temporary-file-directory)
  (eval-and-compile
    ;; There is no default value on w32 systems, which could work out
    ;; of the box.
    (defvar ert-remote-temporary-file-directory
      (cond
       ((getenv "REMOTE_TEMPORARY_FILE_DIRECTORY"))
       ((eq system-type 'windows-nt) null-device)
       (t (add-to-list
           'tramp-methods
           `("mock"
	     (tramp-login-program	,tramp-default-remote-shell)
	     (tramp-login-args		(("-i")))
             (tramp-direct-async	("-c"))
	     (tramp-remote-shell	,tramp-default-remote-shell)
	     (tramp-remote-shell-args	("-c"))
	     (tramp-connection-timeout	10)))
          (add-to-list
           'tramp-default-host-alist
           `("\\`mock\\'" nil ,(system-name)))
          ;; Emacs's Makefile sets $HOME to a nonexistent value.
          ;; Needed in batch mode only, therefore.
          (unless (and (null noninteractive) (file-directory-p "~/"))
            (setenv "HOME" temporary-file-directory))
          (format "/mock::%s" temporary-file-directory)))
      "Temporary directory for remote file tests.")

    ;; This should happen on hydra only.
    (when (getenv "EMACS_HYDRA_CI")
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path))))

;; Beautify batch mode.
(when noninteractive
  ;; Suppress nasty messages.
  (fset #'shell-command-sentinel #'ignore)
  ;; We do not want to be interrupted.
  (fset #'tramp-action-yesno
	(lambda (_proc vec)
	  (tramp-send-string vec (concat "yes" tramp-local-end-of-line)) t))
  (eval-after-load 'tramp-gvfs
    '(fset 'tramp-gvfs-handler-askquestion
	   (lambda (_message _choices) '(t nil 0)))))

(defconst tramp-test-vec
  (and (file-remote-p ert-remote-temporary-file-directory)
       (tramp-dissect-file-name ert-remote-temporary-file-directory))
  "The used `tramp-file-name' structure.")

(setq auth-source-cache-expiry nil
      auth-source-save-behavior nil
      ert-batch-backtrace-right-margin nil
      password-cache-expiry nil
      remote-file-name-inhibit-cache nil
      tramp-allow-unsafe-temporary-files t
      tramp-cache-read-persistent-data t ;; For auth-sources.
      tramp-copy-size-limit nil
      tramp-error-show-message-timeout nil
      tramp-persistency-file-name nil
      tramp-verbose 0)

(defconst tramp-test-name-prefix "tramp-test"
  "Prefix to use for temporary test files.")

(defun tramp--test-make-temp-name (&optional local quoted)
  "Return a temporary file name for test.
If LOCAL is non-nil, a local file name is returned.
If QUOTED is non-nil, the local part of the file name is quoted.
The temporary file is not created."
  (funcall
   (if quoted #'file-name-quote #'identity)
   (expand-file-name
    (make-temp-name tramp-test-name-prefix)
    (if local temporary-file-directory ert-remote-temporary-file-directory))))

;; Method "smb" supports `make-symbolic-link' only if the remote host
;; has CIFS capabilities.  tramp-adb.el, tramp-gvfs.el, tramp-rclone.el
;; and tramp-sshfs.el do not support symbolic links at all.
(defmacro tramp--test-ignore-make-symbolic-link-error (&rest body)
  "Run BODY, ignoring \"make-symbolic-link not supported\" file error."
  (declare (indent defun) (debug (body)))
  `(condition-case err
       (progn ,@body)
     (file-error
      (unless (string-equal (error-message-string err)
			    "make-symbolic-link not supported")
	(signal (car err) (cdr err))))))

;; Don't print messages in nested `tramp--test-instrument-test-case' calls.
(defvar tramp--test-instrument-test-case-p nil
  "Whether `tramp--test-instrument-test-case' run.
This shall used dynamically bound only.")

;; When `tramp-verbose' is greater than 10, and you want to trace
;; other functions as well, do something like
;; (let ((tramp-trace-functions '(file-name-non-special)))
;;   (tramp--test-instrument-test-case 11
;;     ...))
(defmacro tramp--test-instrument-test-case (verbose &rest body)
  "Run BODY with `tramp-verbose' equal VERBOSE.
Print the content of the Tramp connection and debug buffers, if
`tramp-verbose' is greater than 3.  Print traces if `tramp-verbose'
is greater than 10.
`should-error' is not handled properly.  BODY shall not contain a timeout."
  (declare (indent 1) (debug (natnump body)))
  `(let* ((tramp-verbose (max (or ,verbose 0) (or tramp-verbose 0)))
	  (debug-ignored-errors
	   (append
	    '("\\`make-symbolic-link not supported\\'"
	      "\\`error with add-name-to-file")
	    debug-ignored-errors))
	  inhibit-message)
     (unwind-protect
	 (let ((tramp--test-instrument-test-case-p t)) ,@body)
       ;; Unwind forms.
       (when (and (null tramp--test-instrument-test-case-p) (> tramp-verbose 3))
	 (untrace-all)
	 (dolist (buf (tramp-list-tramp-buffers))
	   (message ";; %s\n%s" buf (tramp-get-buffer-string buf))
	   (kill-buffer buf))))))

(defsubst tramp--test-message (fmt-string &rest arguments)
  "Emit a message into ERT *Messages*."
  (tramp--test-instrument-test-case 0
    (apply #'tramp-message tramp-test-vec 0 fmt-string arguments)))

(defsubst tramp--test-backtrace ()
  "Dump a backtrace into ERT *Messages*."
  (tramp--test-instrument-test-case 10
    (tramp-backtrace tramp-test-vec)))

(defmacro tramp--test-print-duration (message &rest body)
  "Run BODY and print a message with duration, prompted by MESSAGE."
  (declare (indent 1) (debug (stringp body)))
  `(let ((start (current-time)))
     (unwind-protect
	 (progn ,@body)
       (tramp--test-message
	"%s %f sec" ,message (float-time (time-subtract nil start))))))

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
	   (file-remote-p ert-remote-temporary-file-directory)
	   (file-directory-p ert-remote-temporary-file-directory)
	   (file-writable-p ert-remote-temporary-file-directory))))))

  (when (cdr tramp--test-enabled-checked)
    ;; Remove old test files.
    (dolist (dir `(,temporary-file-directory
		   ,tramp-compat-temporary-file-directory
		   ,ert-remote-temporary-file-directory))
      (dolist (file (directory-files
		     dir 'full
		     (rx-to-string
		      `(: bos (? ".#")
			  (| ,tramp-test-name-prefix
			     ,(if (getenv "TRAMP_TEST_CLEANUP_TEMP_FILES")
				  tramp-temp-name-prefix 'unmatchable))))))

	;; Exclude sockets and FUSE mount points.
	(ignore-errors
	  (unless
	      (or (string-prefix-p
		   "srw" (file-attribute-modes (file-attributes file)))
		  ;; Prior Emacs 31.1, the FUSE mount points where
		  ;; "tramp.rclone.*" and "tramp.sshfs.*".  We should
		  ;; exclude them as well, in order to avoid trouble.
		  (string-match-p
		   (rx bos (| (literal tramp-fuse-name-prefix)
			      (literal tramp-temp-name-prefix))
		       (| "rclone" "sshfs") ".")
		   (file-name-nondirectory file)))
	    (tramp--test-message "Delete %s" file)
	    (if (file-directory-p file)
		(delete-directory file 'recursive)
	      (delete-file file))))))
    ;; Cleanup connection.
    (ignore-errors
      (tramp-cleanup-connection tramp-test-vec nil 'keep-password)))

  ;; Return result.
  (cdr tramp--test-enabled-checked))

(ert-deftest tramp-test00-availability ()
  "Test availability of Tramp functions."
  :expected-result (if (tramp--test-enabled) :passed :failed)
  (tramp--test-message
   "Remote directory: `%s'" ert-remote-temporary-file-directory)
  (should (ignore-errors
	    (and
	     (file-remote-p ert-remote-temporary-file-directory)
	     (file-directory-p ert-remote-temporary-file-directory)
	     (file-writable-p ert-remote-temporary-file-directory)))))

(ert-deftest tramp-test01-file-name-syntax ()
  "Check remote file name syntax."
  (let ((syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'default)
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

	  ;; Using an IPv4 address with port.
	  (should (tramp-tramp-file-p "/method:1.2.3.4#1234:"))
	  (should (tramp-tramp-file-p "/method:user@1.2.3.4#1234:"))

	  ;; Using an IPv6 address.
	  (should (tramp-tramp-file-p "/method:[::1]:"))
	  (should (tramp-tramp-file-p "/method:user@[::1]:"))

	  ;; Using an IPv4 mapped IPv6 address.
	  (should (tramp-tramp-file-p "/method:[::ffff:1.2.3.4]:"))
	  (should (tramp-tramp-file-p "/method:user@[::ffff:1.2.3.4]:"))

	  ;; Using an IPv6 address with port.
	  (should (tramp-tramp-file-p "/method:[::1]#1234:"))
	  (should (tramp-tramp-file-p "/method:user@[::1]#1234:"))

	  ;; Local file name part.
	  (should (tramp-tramp-file-p "/method:::"))
	  (should (tramp-tramp-file-p "/method::/:"))
	  (should (tramp-tramp-file-p "/method::/path/to/file"))
	  (should (tramp-tramp-file-p "/method::/:/path/to/file"))
	  (should (tramp-tramp-file-p "/method::file"))
	  (should (tramp-tramp-file-p "/method::/:file"))

	  ;; Multihop.
	  (should (tramp-tramp-file-p "/method1:|method2::"))
	  (should
	   (tramp-tramp-file-p "/method1:host1|method2:host2:"))
	  (should
	   (tramp-tramp-file-p "/method1:user1@host1|method2:user2@host2:"))
	  (should
	   (tramp-tramp-file-p
	    "/method1:user1@host1|method2:user2@host2|method3:user3@host3:"))

	  ;; No strings.
	  (should-not (tramp-tramp-file-p nil))
	  (should-not (tramp-tramp-file-p 'symbol))
	  ;; No newline or linefeed.
	  (should-not (tramp-tramp-file-p "/method::file\nname"))
	  (should-not (tramp-tramp-file-p "/method::file\rname"))
	  ;; Ange-FTP syntax.
	  (should-not (tramp-tramp-file-p "/host:"))
	  (should-not (tramp-tramp-file-p "/user@host:"))
	  (should-not (tramp-tramp-file-p "/1.2.3.4:"))
	  (should-not (tramp-tramp-file-p "/[]:"))
	  (should-not (tramp-tramp-file-p "/[::1]:"))
	  (should-not (tramp-tramp-file-p "/[::ffff:1.2.3.4]:"))
	  (should-not (tramp-tramp-file-p "/host:/:"))
	  (should-not (tramp-tramp-file-p "/host1|host2:"))
	  (should-not (tramp-tramp-file-p "/user1@host1|user2@host2:"))
	  ;; Quote with "/:" suppresses file name handlers.
	  (should-not (tramp-tramp-file-p "/::"))
	  (should-not (tramp-tramp-file-p "/:@:"))
	  (should-not (tramp-tramp-file-p "/:[]:"))
	  ;; When `tramp-mode' is nil, Tramp is not activated.
	  (let (tramp-mode)
	    (should-not (tramp-tramp-file-p "/method:user@host:")))
	  ;; `tramp-ignored-file-name-regexp' suppresses Tramp.
	  (let ((tramp-ignored-file-name-regexp "\\`/method:user@host:"))
	    (should-not (tramp-tramp-file-p "/method:user@host:")))
	  ;; Methods shall be at least two characters, except the
	  ;; default method.
	  (let ((system-type 'windows-nt))
	    (should-not (tramp-tramp-file-p "/c:/path/to/file"))
	    (should-not (tramp-tramp-file-p "/c::/path/to/file"))
	    (should (tramp-tramp-file-p "/-::/path/to/file"))
	    (should (tramp-tramp-file-p "/mm::/path/to/file")))
	  (let ((system-type 'gnu/linux))
	    (should-not (tramp-tramp-file-p "/m::/path/to/file"))
	    (should (tramp-tramp-file-p "/-:h:/path/to/file"))
	    (should (tramp-tramp-file-p "/mm::/path/to/file"))))

      ;; Exit.
      (tramp-change-syntax syntax))))

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

	  ;; Using an IPv4 address with port.
	  (should (tramp-tramp-file-p "/1.2.3.4#1234:"))
	  (should (tramp-tramp-file-p "/user@1.2.3.4#1234:"))

	  ;; Using an IPv6 address.
	  (should (tramp-tramp-file-p "/[::1]:"))
	  (should (tramp-tramp-file-p "/user@[::1]:"))

	  ;; Using an IPv4 mapped IPv6 address.
	  (should (tramp-tramp-file-p "/[::ffff:1.2.3.4]:"))
	  (should (tramp-tramp-file-p "/user@[::ffff:1.2.3.4]:"))

	  ;; Using an IPv6 address with port.
	  (should (tramp-tramp-file-p "/[::1]#1234:"))
	  (should (tramp-tramp-file-p "/user@[::1]#1234:"))

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

	  ;; Using an IPv4 address with port.
	  (should (tramp-tramp-file-p "/[method/1.2.3.4#1234]"))
	  (should (tramp-tramp-file-p "/[method/user@1.2.3.4#1234]"))

	  ;; Using an IPv6 address.
	  (should (tramp-tramp-file-p "/[method/::1]"))
	  (should (tramp-tramp-file-p "/[method/user@::1]"))

	  ;; Using an IPv4 mapped IPv6 address.
	  (should (tramp-tramp-file-p "/[method/::ffff:1.2.3.4]"))
	  (should (tramp-tramp-file-p "/[method/user@::ffff:1.2.3.4]"))

	  ;; Using an IPv6 address with port.
	  (should (tramp-tramp-file-p "/[method/::1#1234]"))
	  (should (tramp-tramp-file-p "/[method/user@::1#1234]"))

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
	  ;; Ange-FTP syntax.
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
	tramp-default-host-alist
	tramp-default-proxies-alist
	;; Suppress method name check.
	(non-essential t)
	;; Suppress check for multihops.
	(tramp-cache-data (make-hash-table :test #'equal))
	(tramp-connection-properties '((nil "login-program" t)))
	(syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'default)
	  ;; An unknown method shall raise an error.
	  (let (non-essential)
	    (should-error
	     (expand-file-name "/method:user@host:")
	     :type 'user-error))

	  ;; Expand `tramp-default-user' and `tramp-default-host'.
	  (should
	   (string-equal
	    (file-remote-p "/method::")
	    (format "/%s:%s@%s:" "method" "default-user" "default-host")))
	  (should (string-equal (file-remote-p "/method::" 'method) "method"))
	  (should
	   (string-equal (file-remote-p "/method::" 'user) "default-user"))
	  (should
	   (string-equal (file-remote-p "/method::" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/method::" 'localname) ""))
	  (should (string-equal (file-remote-p "/method::" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should
	   (string-equal
	    (file-remote-p "/-:host:")
	    (format "/%s:%s@%s:" "default-method" "default-user" "host")))
	  (should
	   (string-equal (file-remote-p "/-:host:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/-:host:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/-:host:" 'host) "host"))
	  (should (string-equal (file-remote-p "/-:host:" 'localname) ""))
	  (should (string-equal (file-remote-p "/-:host:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-host'.
	  (should
	   (string-equal
	    (file-remote-p "/-:user@:")
	    (format "/%s:%s@%s:" "default-method" "user" "default-host")))
	  (should
	   (string-equal (file-remote-p "/-:user@:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/-:user@:" 'user) "user"))
	  (should
	   (string-equal (file-remote-p "/-:user@:" 'host) "default-host"))
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
	  (should
	   (string-equal (file-remote-p "/method:host:" 'method) "method"))
	  (should
	   (string-equal (file-remote-p "/method:host:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/method:host:" 'host) "host"))
	  (should (string-equal (file-remote-p "/method:host:" 'localname) ""))
	  (should (string-equal (file-remote-p "/method:host:" 'hop) nil))

	  ;; Expand `tramp-default-host'.
	  (should
	   (string-equal
	    (file-remote-p "/method:user@:")
	    (format "/%s:%s@%s:" "method" "user" "default-host")))
	  (should
	   (string-equal (file-remote-p "/method:user@:" 'method) "method"))
	  (should (string-equal (file-remote-p "/method:user@:" 'user) "user"))
	  (should
	   (string-equal (file-remote-p "/method:user@:" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/method:user@:" 'localname) ""))
	  (should (string-equal (file-remote-p "/method:user@:" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/method:user@host:")
		   (format "/%s:%s@%s:" "method" "user" "host")))
	  (should (string-equal
		   (file-remote-p "/method:user@host:" 'method) "method"))
	  (should
	   (string-equal (file-remote-p "/method:user@host:" 'user) "user"))
	  (should
	   (string-equal (file-remote-p "/method:user@host:" 'host) "host"))
	  (should
	   (string-equal (file-remote-p "/method:user@host:" 'localname) ""))
	  (should (string-equal (file-remote-p "/method:user@host:" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/method:user@email@host:")
		   (format "/%s:%s@%s:" "method" "user@email" "host")))
	  (should (string-equal
		   (file-remote-p "/method:user@email@host:" 'method) "method"))
	  (should
	   (string-equal
	    (file-remote-p "/method:user@email@host:" 'user) "user@email"))
	  (should (string-equal
		   (file-remote-p "/method:user@email@host:" 'host) "host"))
	  (should (string-equal
		   (file-remote-p "/method:user@email@host:" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/method:user@email@host:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should
	   (string-equal
	    (file-remote-p "/-:host#1234:")
	    (format "/%s:%s@%s:" "default-method" "default-user" "host#1234")))
	  (should (string-equal
		   (file-remote-p "/-:host#1234:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/-:host#1234:" 'user) "default-user"))
	  (should
	   (string-equal (file-remote-p "/-:host#1234:" 'host) "host#1234"))
	  (should (string-equal (file-remote-p "/-:host#1234:" 'localname) ""))
	  (should (string-equal (file-remote-p "/-:host#1234:" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/-:user@host#1234:")
		   (format "/%s:%s@%s:" "default-method" "user" "host#1234")))
	  (should
	   (string-equal
	    (file-remote-p "/-:user@host#1234:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/-:user@host#1234:" 'user) "user"))
	  (should
	   (string-equal
	    (file-remote-p "/-:user@host#1234:" 'host) "host#1234"))
	  (should
	   (string-equal (file-remote-p "/-:user@host#1234:" 'localname) ""))
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
	  (should
	   (string-equal (file-remote-p "/method:host#1234:" 'localname) ""))
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
	  (should
	   (string-equal
	    (file-remote-p "/-:1.2.3.4:")
	    (format "/%s:%s@%s:" "default-method" "default-user" "1.2.3.4")))
	  (should
	   (string-equal
	    (file-remote-p "/-:1.2.3.4:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/-:1.2.3.4:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/-:1.2.3.4:" 'host) "1.2.3.4"))
	  (should (string-equal (file-remote-p "/-:1.2.3.4:" 'localname) ""))
	  (should (string-equal (file-remote-p "/-:1.2.3.4:" 'hop) nil))

	  ;; Expand `tramp-default-method'.
	  (should (string-equal
		   (file-remote-p "/-:user@1.2.3.4:")
		   (format "/%s:%s@%s:" "default-method" "user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/-:user@1.2.3.4:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/-:user@1.2.3.4:" 'user) "user"))
	  (should
	   (string-equal (file-remote-p "/-:user@1.2.3.4:" 'host) "1.2.3.4"))
	  (should
	   (string-equal (file-remote-p "/-:user@1.2.3.4:" 'localname) ""))
	  (should
	   (string-equal (file-remote-p "/-:user@1.2.3.4:" 'hop) nil))

	  ;; Expand `tramp-default-user'.
	  (should (string-equal
		   (file-remote-p "/method:1.2.3.4:")
		   (format "/%s:%s@%s:" "method" "default-user" "1.2.3.4")))
	  (should
	   (string-equal (file-remote-p "/method:1.2.3.4:" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/method:1.2.3.4:" 'user) "default-user"))
	  (should
	   (string-equal (file-remote-p "/method:1.2.3.4:" 'host) "1.2.3.4"))
	  (should
	   (string-equal (file-remote-p "/method:1.2.3.4:" 'localname) ""))
	  (should (string-equal (file-remote-p "/method:1.2.3.4:" 'hop) nil))

	  ;; No expansion.
	  (should (string-equal
		   (file-remote-p "/method:user@1.2.3.4:")
		   (format "/%s:%s@%s:" "method" "user" "1.2.3.4")))
	  (should (string-equal
		   (file-remote-p "/method:user@1.2.3.4:" 'method) "method"))
	  (should
	   (string-equal (file-remote-p "/method:user@1.2.3.4:" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/method:user@1.2.3.4:" 'host) "1.2.3.4"))
	  (should (string-equal
		   (file-remote-p "/method:user@1.2.3.4:" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/method:user@1.2.3.4:" 'hop) nil))

	  ;; Expand `tramp-default-method', `tramp-default-user' and
	  ;; `tramp-default-host'.
	  (should
	   (string-equal
	    (file-remote-p "/-:[]:")
	    (format
	     "/%s:%s@%s:" "default-method" "default-user" "default-host")))
	  (should
	   (string-equal (file-remote-p "/-:[]:" 'method) "default-method"))
	  (should (string-equal (file-remote-p "/-:[]:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/-:[]:" 'host) "default-host"))
	  (should (string-equal (file-remote-p "/-:[]:" 'localname) ""))
	  (should (string-equal (file-remote-p "/-:[]:" 'hop) nil))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (let ((tramp-default-host "::1"))
	    (should
	     (string-equal
	      (file-remote-p "/-:[]:")
	      (format "/%s:%s@%s:" "default-method" "default-user" "[::1]")))
	    (should
	     (string-equal (file-remote-p "/-:[]:" 'method) "default-method"))
	    (should
	     (string-equal (file-remote-p "/-:[]:" 'user) "default-user"))
	    (should (string-equal (file-remote-p "/-:[]:" 'host) "::1"))
	    (should (string-equal (file-remote-p "/-:[]:" 'localname) ""))
	    (should (string-equal (file-remote-p "/-:[]:" 'hop) nil)))

	  ;; Expand `tramp-default-method' and `tramp-default-user'.
	  (should
	   (string-equal
	    (file-remote-p "/-:[::1]:")
	    (format "/%s:%s@%s:" "default-method" "default-user" "[::1]")))
	  (should
	   (string-equal (file-remote-p "/-:[::1]:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/-:[::1]:" 'user) "default-user"))
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
	  (should
	   (string-equal (file-remote-p "/method:[::1]:" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/method:[::1]:" 'user) "default-user"))
	  (should (string-equal (file-remote-p "/method:[::1]:" 'host) "::1"))
	  (should (string-equal (file-remote-p "/method:[::1]:" 'localname) ""))
	  (should (string-equal (file-remote-p "/method:[::1]:" 'hop) nil))

	  ;; No expansion.  Hop.
	  (should (string-equal
		   (file-remote-p "/method:user@[::1]#1234:")
		   (format "/%s:%s@%s#%s:" "method" "user" "[::1]" "1234")))
	  (should (string-equal
		   (file-remote-p "/method:user@[::1]#1234:" 'method) "method"))
	  (should (string-equal (file-remote-p "/method:user@[::1]#1234:" 'user)
				"user"))
	  (should (string-equal
		   (file-remote-p "/method:user@[::1]#1234:" 'host) "::1#1234"))
	  (should (string-equal
		   (file-remote-p "/method:user@[::1]#1234:" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/method:user@[::1]#1234:" 'hop) nil))

	  ;; Local file name part.
	  (should (string-equal (file-remote-p "/-:host:/:" 'localname) "/:"))
	  (should (string-equal (file-remote-p "/method:::" 'localname) ":"))
	  (should (string-equal (file-remote-p "/method:: " 'localname) " "))
	  (should
	   (string-equal (file-remote-p "/method::file" 'localname) "file"))
	  (should (string-equal
		   (file-remote-p "/method::/path/to/file" 'localname)
		   "/path/to/file"))

	  ;; Multihop.
	  (dolist (tramp-show-ad-hoc-proxies '(nil t))

	    ;; Explicit settings in `tramp-default-proxies-alist'
	    ;; shouldn't show hops.
	    (setq tramp-default-proxies-alist
		  '(("^host2$" "^user2$" "/method1:user1@host1:")))
	    (should
	     (string-equal
	      (file-remote-p "/method2:user2@host2:/path/to/file")
	      "/method2:user2@host2:"))
	    (setq tramp-default-proxies-alist nil)

	    ;; Ad-hoc settings.
	    (should
	     (string-equal
	      (file-remote-p
	       "/method1:user1@host1|method2:user2@host2:/path/to/file")
	      (if tramp-show-ad-hoc-proxies
		  "/method1:user1@host1|method2:user2@host2:"
		"/method2:user2@host2:")))
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
	       "/method1:user1@host1|method2:user2@host2:/path/to/file"
	       'localname)
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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/method1:user1@host1"
		   "|method2:user2@host2"
		   "|method3:user3@host3:")
		"/method3:user3@host3:")))
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
	    (add-to-list
	     'tramp-default-method-alist '("host1" "user1" "method1"))
	    (add-to-list
	     'tramp-default-method-alist '("host2" "user2" "method2"))
	    (add-to-list
	     'tramp-default-method-alist '("host3" "user3" "method3"))
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/-:user1@host1"
		"|-:user2@host2"
		"|-:user3@host3:/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/method1:user1@host1"
		   "|method2:user2@host2"
		   "|method3:user3@host3:")
		"/method3:user3@host3:")))

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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/method1:user1@host1"
		   "|method2:user2@host2"
		   "|method3:user3@host3:")
		"/method3:user3@host3:")))

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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/method1:user1@host1"
		   "|method2:user2@host2"
		   "|method3:user3@host3:")
		"/method3:user3@host3:")))

	    ;; Ad-hoc user name and host name expansion.
	    (setq tramp-default-method-alist nil
		  tramp-default-user-alist nil
		  tramp-default-host-alist nil)
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/method1:user1@host1"
		"|method2:user2@"
		"|method3:user3@:/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/method1:user1@host1"
		   "|method2:user2@host1"
		   "|method3:user3@host1:")
		"/method3:user3@host1:")))
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/method1:%u@%h"
		"|method2:user2@host2"
		"|method3:%u@%h"
		"|method4:user4%domain4@host4#1234:/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/method1:user2@host2"
		   "|method2:user2@host2"
		   "|method3:user4@host4"
		   "|method4:user4%domain4@host4#1234:")
		"/method4:user4%domain4@host4#1234:")))))

      ;; Exit.
      (tramp-change-syntax syntax))))

(ert-deftest tramp-test02-file-name-dissect-simplified ()
  "Check simplified file name components."
  :tags '(:expensive-test)
  (let ((tramp-default-method "default-method")
	(tramp-default-user "default-user")
	(tramp-default-host "default-host")
	tramp-default-user-alist
	tramp-default-host-alist
	tramp-default-proxies-alist
	;; Suppress method name check.
	(non-essential t)
	;; Suppress check for multihops.
	(tramp-cache-data (make-hash-table :test #'equal))
	(tramp-connection-properties '((nil "login-program" t)))
	(syntax tramp-syntax))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'simplified)
	  ;; An unknown default method shall raise an error.
	  (let (non-essential)
	    (should-error
	     (expand-file-name "/user@host:")
	     :type 'user-error))

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
		   (file-remote-p "/user@email@host:" 'method) "default-method"))
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

	  ;; No expansion.  Hop.
	  (should (string-equal
		   (file-remote-p "/user@[::1]#1234:")
		   (format "/%s@%s#%s:" "user" "[::1]" "1234")))
	  (should (string-equal
		   (file-remote-p "/user@[::1]#1234:" 'method) "default-method"))
	  (should
	   (string-equal (file-remote-p "/user@[::1]#1234:" 'user) "user"))
	  (should
	   (string-equal (file-remote-p "/user@[::1]#1234:" 'host) "::1#1234"))
	  (should
	   (string-equal (file-remote-p "/user@[::1]#1234:" 'localname) ""))
	  (should (string-equal (file-remote-p "/user@[::1]#1234:" 'hop) nil))

	  ;; Local file name part.
	  (should (string-equal (file-remote-p "/host:/:" 'localname) "/:"))
	  (should (string-equal (file-remote-p "/host::" 'localname) ":"))
	  (should (string-equal (file-remote-p "/host: " 'localname) " "))
	  (should (string-equal (file-remote-p "/host:file" 'localname) "file"))
	  (should (string-equal
		   (file-remote-p "/host:/path/to/file" 'localname)
		   "/path/to/file"))

	  ;; Multihop.
	  (dolist (tramp-show-ad-hoc-proxies '(nil t))

	    ;; Explicit settings in `tramp-default-proxies-alist'
	    ;; shouldn't show hops.
	    (setq tramp-default-proxies-alist
		  '(("^host2$" "^user2$" "/user1@host1:")))
	    (should
	     (string-equal
	      (file-remote-p "/user2@host2:/path/to/file")
	      "/user2@host2:"))
	    (setq tramp-default-proxies-alist nil)

	    ;; Ad-hoc settings.
	    (should
	     (string-equal
	      (file-remote-p "/user1@host1|user2@host2:/path/to/file")
	      (if tramp-show-ad-hoc-proxies
		  "/user1@host1|user2@host2:"
		"/user2@host2:")))
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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/user1@host1"
		   "|user2@host2"
		   "|user3@host3:")
		"/user3@host3:")))
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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/user1@host1"
		   "|user2@host2"
		   "|user3@host3:")
		"/user3@host3:")))

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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/user1@host1"
		   "|user2@host2"
		   "|user3@host3:")
		"/user3@host3:")))

	    ;; Ad-hoc user name and host name expansion.
	    (setq tramp-default-user-alist nil
		  tramp-default-host-alist nil)
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/user1@host1"
		"|user2@"
		"|user3@:/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/user1@host1"
		   "|user2@host1"
		   "|user3@host1:")
		"/user3@host1:")))
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/%u@%h"
		"|user2@host2"
		"|%u@%h"
		"|user4%domain4@host4#1234:/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/user2@host2"
		   "|user2@host2"
		   "|user4@host4"
		   "|user4%domain4@host4#1234:")
		"/user4%domain4@host4#1234:")))))

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
	tramp-default-proxies-alist
	;; Suppress method name check.
	(non-essential t)
	;; Suppress check for multihops.
	(tramp-cache-data (make-hash-table :test #'equal))
	(tramp-connection-properties '((nil "login-program" t)))
	(syntax tramp-syntax)
	;; We must transform `tramp-crypt-directories'.
	(tramp-crypt-directories
	 (mapcar #'tramp-dissect-file-name tramp-crypt-directories)))
    (unwind-protect
	(progn
	  (tramp-change-syntax 'separate)
	  ;; We must transform `tramp-crypt-directories'.
	  (setq tramp-crypt-directories
		(mapcar
		 (lambda (vec)
		   (tramp-make-tramp-file-name
		    vec (tramp-file-name-localname vec)))
		 tramp-crypt-directories))
	  ;; An unknown method shall raise an error.
	  (let (non-essential)
	    (should-error
	     (expand-file-name "/[method/user@host]")
	     :type 'user-error))

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
		   (file-remote-p "/[method/user@email@host]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/user@email@host]" 'user)
		   "user@email"))
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
		   (file-remote-p "/[/user@host#1234]" 'method)
		   "default-method"))
	  (should (string-equal
		   (file-remote-p "/[/user@host#1234]" 'user) "user"))
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
		   (file-remote-p "/[-/user@host#1234]" 'method)
		   "default-method"))
	  (should (string-equal
		   (file-remote-p "/[-/user@host#1234]" 'user) "user"))
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
		   (file-remote-p "/[method/user@host#1234]" 'host) "host#1234"))
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
		   (file-remote-p "/[/user@1.2.3.4]" 'method) "default-method"))
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
		   (file-remote-p "/[-/user@1.2.3.4]" 'method) "default-method"))
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

	  ;; No expansion.  Hop.
	  (should (string-equal
		   (file-remote-p "/[method/user@::1#1234]")
		   (format "/[%s/%s@%s#%s]" "method" "user" "::1" "1234")))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1#1234]" 'method) "method"))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1#1234]" 'user) "user"))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1#1234]" 'host) "::1#1234"))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1#1234]" 'localname) ""))
	  (should (string-equal
		   (file-remote-p "/[method/user@::1#1234]" 'hop) nil))

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
	  (dolist (tramp-show-ad-hoc-proxies '(nil t))

	    ;; Explicit settings in `tramp-default-proxies-alist'
	    ;; shouldn't show hops.
	    (setq tramp-default-proxies-alist
		  '(("^host2$" "^user2$" "/[method1/user1@host1]")))
	    (should
	     (string-equal
	      (file-remote-p "/[method2/user2@host2]/path/to/file")
	      "/[method2/user2@host2]"))
	    (setq tramp-default-proxies-alist nil)

	    ;; Ad-hoc settings.
	    (should
	     (string-equal
	      (file-remote-p
	       "/[method1/user1@host1|method2/user2@host2]/path/to/file")
	      (if tramp-show-ad-hoc-proxies
		  "/[method1/user1@host1|method2/user2@host2]"
		"/[method2/user2@host2]")))
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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/[method1/user1@host1"
		   "|method2/user2@host2"
		   "|method3/user3@host3]")
		"/[method3/user3@host3]")))
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
	    (add-to-list
	     'tramp-default-method-alist '("host1" "user1" "method1"))
	    (add-to-list
	     'tramp-default-method-alist '("host2" "user2" "method2"))
	    (add-to-list
	     'tramp-default-method-alist '("host3" "user3" "method3"))
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/[/user1@host1"
		"|/user2@host2"
		"|/user3@host3]/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/[method1/user1@host1"
		   "|method2/user2@host2"
		   "|method3/user3@host3]")
		"/[method3/user3@host3]")))

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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/[method1/user1@host1"
		   "|method2/user2@host2"
		   "|method3/user3@host3]")
		"/[method3/user3@host3]")))

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
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/[method1/user1@host1"
		   "|method2/user2@host2"
		   "|method3/user3@host3]")
		"/[method3/user3@host3]")))

	    ;; Ad-hoc user name and host name expansion.
	    (setq tramp-default-method-alist nil
		  tramp-default-user-alist nil
		  tramp-default-host-alist nil)
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/[method1/user1@host1"
		"|method2/user2@"
		"|method3/user3@]/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/[method1/user1@host1"
		   "|method2/user2@host1"
		   "|method3/user3@host1]")
		"/[method3/user3@host1]")))
	    (should
	     (string-equal
	      (file-remote-p
	       (concat
		"/[method1/%u@%h"
		"|method2/user2@host2"
		"|method3/%u@%h"
		"|method4/user4%domain4@host4#1234]/path/to/file"))
	      (if tramp-show-ad-hoc-proxies
		  (concat
		   "/[method1/user2@host2"
		   "|method2/user2@host2"
		   "|method3/user4@host4"
		   "|method4/user4%domain4@host4#1234]")
		"/[method4/user4%domain4@host4#1234]")))))

      ;; Exit.
      (tramp-change-syntax syntax))))

(ert-deftest tramp-test03-file-name-defaults ()
  "Check default values for some methods."
  (skip-unless (eq tramp-syntax 'default))

  ;; Default values in tramp-adb.el.
  (when (assoc "adb" tramp-methods)
    (should (string-equal (file-remote-p "/adb::" 'host) "")))
  ;; Default values in tramp-ftp.el.
  (when (assoc "ftp" tramp-methods)
    (should (string-equal (file-remote-p "/-:ftp.host:" 'method) "ftp"))
    (dolist (u '("ftp" "anonymous"))
      (should
       (string-equal (file-remote-p (format "/-:%s@:" u) 'method) "ftp"))))
  ;; Default values in tramp-sh.el and tramp-sudoedit.el.
  (when (assoc "su" tramp-methods)
    (dolist
	(h `("127.0.0.1" "[::1]" "localhost" "localhost4" "localhost6"
	     "ip6-localhost" "ip6-loopback" ,(system-name)))
      (should
       (string-equal (file-remote-p (format "/-:root@%s:" h) 'method) "su"))))
  (dolist (m '("su" "sudo" "ksu" "doas" "sudoedit"))
    (when (assoc m tramp-methods)
      (should (string-equal (file-remote-p (format "/%s::" m) 'user) "root"))
      (should
       (string-equal (file-remote-p (format "/%s::" m) 'host) (system-name)))))
  (dolist (m '("rcp" "remcp" "rsh" "telnet" "krlogin" "fcp" "nc"))
    (when (assoc m tramp-methods)
      (should
       (string-equal
	(file-remote-p (format "/%s::" m) 'user) (user-login-name)))))
  ;; Default values in tramp-smb.el.
  (when (assoc "smb" tramp-methods)
    (should (string-equal (file-remote-p "/smb::" 'user) nil))))

;; The following test is inspired by Bug#30946.
(ert-deftest tramp-test03-file-name-host-rules ()
  "Check host name rules for host-less methods."
  (skip-unless (eq tramp-syntax 'default))
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  ;; Host names must match rules in case the command template of a
  ;; method doesn't use them.
  (dolist (m '("su" "sg" "sudo" "doas" "ksu"))
    (when (assoc m tramp-methods)
      (let (tramp-connection-properties tramp-default-proxies-alist)
	(ignore-errors
	  (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password))
	;; Single hop.  The host name must match `tramp-local-host-regexp'.
	(should-error
	 (find-file (format "/%s:foo:" m))
	 :type 'user-error)
	;; Multi hop.  The host name must match the previous hop.
	(should-error
	 (find-file
	  (format
	   "%s|%s:foo:"
	   (substring (file-remote-p ert-remote-temporary-file-directory) 0 -1)
	   m))
	 :type 'user-error)))))

(ert-deftest tramp-test03-file-name-method-rules ()
  "Check file name rules for some methods."
  (skip-unless (eq tramp-syntax 'default))
  (skip-unless (tramp--test-enabled))

  ;; Multi hops are allowed for inline methods only.
  (let (non-essential)
    (should-error
     (expand-file-name "/ssh:user1@host1|method:user2@host2:/path/to/file")
     :type 'user-error)
    (should-error
     (expand-file-name "/method:user1@host1|ssh:user2@host2:/path/to/file")
     :type 'user-error)))

(ert-deftest tramp-test04-substitute-in-file-name ()
  "Check `substitute-in-file-name'."
  (skip-unless (eq tramp-syntax 'default))

  ;; Suppress method name check.  We cannot use the string "foo" as
  ;; user name, because (substitute-in-string "/~foo") returns
  ;; different values depending on the existence of user "foo" (see
  ;; Bug#43052).
  (let ((tramp-methods (cons '("method") tramp-methods))
        (foo (downcase (md5 (current-time-string)))))
    (should
     (string-equal (substitute-in-file-name "/method:host:///foo") "/foo"))
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
      (substitute-in-file-name "/method:host:/:///foo")
      "/method:host:/:///foo"))
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
     (string-equal
      (substitute-in-file-name (concat "/method:host://~" foo))
      (concat "/~" foo)))
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/~" foo))
      (concat "/method:host:/~" foo)))
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/path//~" foo))
      (concat "/~" foo)))
    ;; (substitute-in-file-name "/path/~foo") expands only for a local
    ;; user "foo" to "/~foo"".  Otherwise, it doesn't expand.
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/path/~" foo))
      (concat "/method:host:/path/~" foo)))
    ;; Quoting local part.
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/://~" foo))
      (concat "/method:host:/://~" foo)))
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/:/~" foo))
      (concat "/method:host:/:/~" foo)))
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/:/path//~" foo))
      (concat "/method:host:/:/path//~" foo)))
    (should
     (string-equal
      (substitute-in-file-name (concat "/method:host:/:/path/~" foo))
      (concat "/method:host:/:/path/~" foo)))

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
	"/method:host:/:/path/$$FOO")))))

(ert-deftest tramp-test05-expand-file-name ()
  "Check `expand-file-name'."
  (skip-unless (eq tramp-syntax 'default))

  ;; Suppress method name check.
  (let ((tramp-methods (cons '("method") tramp-methods)))
    (should
     (string-equal
      (expand-file-name "/method:host:/path/./file") "/method:host:/path/file"))
    (should
     (string-equal
      (expand-file-name "/method:host:/path/../file") "/method:host:/file"))
    (should
     (string-equal
      (expand-file-name "/method:host:/path/.") "/method:host:/path"))
    (should
     (string-equal
      (expand-file-name "/method:host:/path/..") "/method:host:/"))
    (should
     (string-equal
      (expand-file-name "." "/method:host:/path/") "/method:host:/path"))
    (should
     (string-equal
      (expand-file-name "" "/method:host:/path/") "/method:host:/path"))
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
      "/method:host:/:/~/path/file"))))

;; The following test is inspired by Bug#26911 and Bug#34834.  They
;; were bugs in `expand-file-name'.
(ert-deftest tramp-test05-expand-file-name-relative ()
  "Check `expand-file-name'."
  (skip-unless (tramp--test-enabled))
  ;; Methods with a share do not expand "/path/..".
  (skip-unless (not (tramp--test-share-p)))

  (should
   (string-equal
    (let ((default-directory
	    (concat
	     (file-remote-p ert-remote-temporary-file-directory) "/path")))
      (expand-file-name ".." "./"))
    (concat (file-remote-p ert-remote-temporary-file-directory) "/"))))

(ert-deftest tramp-test05-expand-file-name-top ()
  "Check `expand-file-name'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))

  (let ((dir (concat (file-remote-p ert-remote-temporary-file-directory) "/")))
    (dolist (local '("." ".."))
      (should (string-equal (expand-file-name local dir) dir))
      (should (string-equal (expand-file-name (concat dir local)) dir)))))

;; The following test is inspired by Bug#65685.
(ert-deftest tramp-test05-expand-file-name-tilde ()
  "Check `expand-file-name'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))

  (let ((dir (file-remote-p ert-remote-temporary-file-directory))
	(tramp-tolerate-tilde t))
    (should (string-equal (expand-file-name (concat dir "~"))
			  (expand-file-name (concat dir "/:~"))))))

(ert-deftest tramp-test06-directory-file-name ()
  "Check `directory-file-name'.
This checks also `file-name-as-directory', `file-name-directory',
`file-name-nondirectory' and `unhandled-file-name-directory'."
  (skip-unless (eq tramp-syntax 'default))

  ;; Suppress method name check.
  (let ((tramp-methods (cons '("method") tramp-methods)))
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
     (string-equal (file-name-directory "/method:host:file") "/method:host:"))
    (should
     (string-equal
      (file-name-directory "/method:host:path/") "/method:host:path/"))
    (should
     (string-equal
      (file-name-directory "/method:host:path/to") "/method:host:path/"))
    (should
     (string-equal
      (file-name-nondirectory "/method:host:/path/to/file") "file"))
    (should
     (string-equal (file-name-nondirectory "/method:host:/path/to/file/") ""))
    (should-not
     (unhandled-file-name-directory "/method:host:/path/to/file")))

  ;; Bug#10085.
  (when (tramp--test-enabled) ;; Packages like tramp-gvfs.el might be disabled.
    (dolist (non-essential '(nil t))
      ;; We must clear `tramp-default-method'.  On hydra, it is "ftp",
      ;; which ruins the tests.
      (let ((tramp-default-method
	     (file-remote-p ert-remote-temporary-file-directory 'method))
	    (host-port
	     (file-remote-p ert-remote-temporary-file-directory 'host)))
	(dolist
	    (file
	     `(,(format "/%s::" tramp-default-method)
	       ,(format
		 "/-:%s:"
		 ;; `(file-remote-p ... 'host)' eliminates IPv6
		 ;; delimiters.  Add them.
		 (if (string-match tramp-ipv6-regexp host-port)
		     (replace-match
		      (format
		       "%s\\&%s"
		       tramp-prefix-ipv6-format tramp-postfix-ipv6-format)
		      nil nil host-port)
		   host-port))))
	  (should (string-equal (directory-file-name file) file))
	  (should
	   (string-equal
	    (file-name-as-directory file)
	    (if non-essential
		file (concat file (if (tramp--test-ange-ftp-p) "/" "./")))))
	  (should (string-equal (file-name-directory file) file))
	  (should (string-equal (file-name-nondirectory file) "")))))))

(ert-deftest tramp-test07-abbreviate-file-name ()
  "Check that Tramp abbreviates file names correctly."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))
  ;; `abbreviate-file-name' is supported since Emacs 29.1.
  (skip-unless (tramp--test-emacs29-p))

  ;; We must refill the cache.  `file-truename' does it.
  (file-truename ert-remote-temporary-file-directory)
  (let* ((remote-host (file-remote-p ert-remote-temporary-file-directory))
	 (remote-host-nohop
	  (tramp-make-tramp-file-name (tramp-dissect-file-name remote-host)))
	 ;; Not all methods can expand "~".
         (home-dir (ignore-errors (expand-file-name (concat remote-host "~"))))
	 home-dir-nohop)
    (skip-unless home-dir)

    ;; Check home-dir abbreviation.
    (unless (string-suffix-p "~" home-dir)
      (should (equal (abbreviate-file-name (concat home-dir "/foo/bar"))
                     (concat remote-host-nohop "~/foo/bar")))
      (should (equal (abbreviate-file-name
		      (concat remote-host "/nowhere/special"))
                     (concat remote-host-nohop "/nowhere/special"))))

    ;; Check `directory-abbrev-alist' abbreviation.
    (let ((directory-abbrev-alist
           `((,(rx bos (literal home-dir) "/foo") . ,(concat home-dir "/f"))
             (,(rx bos (literal remote-host) "/nowhere")
	      . ,(concat remote-host "/nw")))))
      (should (equal (abbreviate-file-name (concat home-dir "/foo/bar"))
                     (concat remote-host-nohop "~/f/bar")))
      (should (equal (abbreviate-file-name
		      (concat remote-host "/nowhere/special"))
                     (concat remote-host-nohop "/nw/special"))))

    ;; Check that home-dir abbreviation doesn't occur when home-dir is just "/".
    (setq home-dir (concat remote-host "/")
	  home-dir-nohop
	  (tramp-make-tramp-file-name (tramp-dissect-file-name home-dir)))
    ;; The remote home directory is kept in the connection property "~".
    ;; We fake this setting.
    (tramp-set-connection-property tramp-test-vec "~" (file-local-name home-dir))
    (should (equal (abbreviate-file-name (concat home-dir "foo/bar"))
		   (concat home-dir-nohop "foo/bar")))
    (tramp-flush-connection-property tramp-test-vec "~")))

(ert-deftest tramp-test07-file-exists-p ()
  "Check `file-exist-p', `write-region' and `delete-file'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (should-not (file-exists-p tmp-name))
      (write-region "foo" nil tmp-name)
      (should (file-exists-p tmp-name))
      (delete-file tmp-name)
      (should-not (file-exists-p tmp-name))

      ;; Trashing files doesn't work when `system-move-file-to-trash'
      ;; is defined (on MS-Windows and macOS), and for encrypted
      ;; remote files.
      (unless (or (fboundp 'system-move-file-to-trash) (tramp--test-crypt-p))
	(let ((trash-directory (tramp--test-make-temp-name 'local quoted))
	      (delete-by-moving-to-trash t))
	  (make-directory trash-directory)
	  (should-not (file-exists-p tmp-name))
	  (write-region "foo" nil tmp-name)
	  (should (file-exists-p tmp-name))
	  (delete-file tmp-name 'trash)
	  (should-not (file-exists-p tmp-name))
	  (should
	   (or (file-exists-p
		(expand-file-name
		 (file-name-nondirectory tmp-name) trash-directory))
	       ;; Gdrive.
	       (file-symlink-p
		(expand-file-name
		 (file-name-nondirectory tmp-name) trash-directory))))
	  (delete-directory trash-directory 'recursive)
	  (should-not (file-exists-p trash-directory))))

      ;; Setting `remote-file-name-inhibit-delete-by-moving-to-trash'
      ;; prevents trashing remote files.
      (let ((trash-directory (tramp--test-make-temp-name 'local quoted))
	    (delete-by-moving-to-trash t)
	    (remote-file-name-inhibit-delete-by-moving-to-trash t))
	(make-directory trash-directory)
	(should-not (file-exists-p tmp-name))
	(write-region "foo" nil tmp-name)
	(should (file-exists-p tmp-name))
	(delete-file tmp-name 'trash)
	(should-not (file-exists-p tmp-name))
	(should-not
	 (file-exists-p
	  (expand-file-name (file-name-nondirectory tmp-name) trash-directory)))
	(delete-directory trash-directory 'recursive)
	(should-not (file-exists-p trash-directory))))))

(ert-deftest tramp-test08-file-local-copy ()
  "Check `file-local-copy'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
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
	    (let ((default-directory ert-remote-temporary-file-directory)
		  (tramp-copy-size-limit 4)
		  (tramp-inline-compress-start-size 2))
	      (delete-file tmp-name2)
	      (should (setq tmp-name2 (file-local-copy tmp-name1))))
	    ;; Error case.
	    (delete-file tmp-name1)
	    (delete-file tmp-name2)
	    (should-error
	     (setq tmp-name2 (file-local-copy tmp-name1))
	     :type 'file-missing))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2))))))

(ert-deftest tramp-test09-insert-file-contents ()
  "Check `insert-file-contents'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (let ((point (point)))
	      (should
	       (equal
		(insert-file-contents tmp-name)
		`(,(expand-file-name tmp-name) 3)))
	      (should (string-equal (buffer-string) "foo"))
	      (should (= point (point))))
	    (goto-char (1+ (point)))
	    (let ((point (point)))
	      (should
	       (equal
		(insert-file-contents tmp-name)
		`(,(expand-file-name tmp-name) 3)))
	      (should (string-equal (buffer-string) "ffoooo"))
	      (should (= point (point))))
	    ;; Insert partly.
	    (let ((point (point)))
	      (should
	       (equal
		(insert-file-contents tmp-name nil 1 3)
		`(,(expand-file-name tmp-name) 2)))
	      (should (string-equal (buffer-string) "foofoooo"))
	      (should (= point (point))))
	    (let ((point (point)))
	      (should
	       (equal
		(insert-file-contents tmp-name nil 2 5)
		`(,(expand-file-name tmp-name) 1)))
	      (should (string-equal (buffer-string) "fooofoooo"))
	      (should (= point (point))))
	    ;; Replace.
	    (let ((point (point)))
	      ;; 0 characters replaced, because "foo" is already there.
	      (should
	       (equal
		(insert-file-contents tmp-name nil nil nil 'replace)
		`(,(expand-file-name tmp-name) 0)))
	      (should (string-equal (buffer-string) "foo"))
	      (should (= point (point))))
	    ;; Insert another string.
	    (let ((point (point)))
	      (replace-string-in-region "foo" "bar" (point-min) (point-max))
	      (goto-char point)
	      (should
	       (equal
		(insert-file-contents tmp-name nil nil nil 'replace)
		`(,(expand-file-name tmp-name) 3)))
	      (should (string-equal (buffer-string) "foo"))
	      (should (= point (point))))
	    ;; Error case.
	    (delete-file tmp-name)
	    (should-error
	     (insert-file-contents tmp-name)
	     :type 'file-missing))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

(ert-deftest tramp-test10-write-region ()
  "Check `write-region'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted))
          (inhibit-message t))
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
	    (unless (tramp--test-ange-ftp-p)
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
	      (delete-file tmp-name)
	      (with-temp-buffer
		(insert "foo")
		(write-region nil nil tmp-name 'append))
	      (with-temp-buffer
		(insert-file-contents tmp-name)
		(should (string-equal (buffer-string) "foo"))))

	    ;; Write string.
	    (write-region "foo" nil tmp-name)
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "foo")))

	    ;; Write empty string.  Used for creation of temporary files.
	    (should-error
	     (make-empty-file tmp-name)
	     :type 'file-already-exists)
	    (delete-file tmp-name)
	    (make-empty-file tmp-name)
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "")))

	    ;; Write partly.
	    (with-temp-buffer
	      (insert "123456789")
	      (write-region 3 5 tmp-name))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (string-equal (buffer-string) "34")))

	    ;; Check message.
	    (let (inhibit-message)
	      (dolist (noninteractive (unless (tramp--test-ange-ftp-p) '(nil t)))
		(dolist (visit '(nil t "string" no-message))
		  (ert-with-message-capture tramp--test-messages
		    (write-region "foo" nil tmp-name nil visit)
		    ;; We must check the last line.  There could be
		    ;; other messages from the progress reporter.
		    (should
		     (string-match-p
		      (if (and (null noninteractive)
			       (or (eq visit t) (null visit) (stringp visit)))
			  (rx bol "Wrote " (literal tmp-name) "\n" eos)
			(rx bos))
		      tramp--test-messages))))))

	    ;; We do not test the lock file here.  See
	    ;; `tramp-test39-make-lock-file-name'.

	    ;; Do not overwrite if excluded.
	    (cl-letf (((symbol-function #'y-or-n-p) #'always)
		      ;; Ange-FTP.
		      ((symbol-function #'yes-or-no-p) #'always))
	      (write-region "foo" nil tmp-name nil nil nil 'mustbenew))
	    (should-error
	     (cl-letf (((symbol-function #'y-or-n-p) #'ignore)
		       ;; Ange-FTP.
		       ((symbol-function #'yes-or-no-p) #'ignore))
	       (write-region "foo" nil tmp-name nil nil nil 'mustbenew))
             :type 'file-already-exists)
	    (should-error
	     (write-region "foo" nil tmp-name nil nil nil 'excl)
	     :type 'file-already-exists)
	    (delete-file tmp-name)

	    ;; Check `buffer-file-coding-system'.  Bug#65022.
	    (with-temp-buffer
	      (setq buffer-file-name tmp-name)
	      (insert "foo")
	      (set-buffer-file-coding-system 'cp1251)
	      (let ((bfcs buffer-file-coding-system))
		(should (buffer-modified-p))
		(should (null (save-buffer)))
		(should
                 (eq (coding-system-get buffer-file-coding-system :mime-charset)
                     (coding-system-get bfcs :mime-charset))))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

;; The following test is inspired by Bug#35497.
(ert-deftest tramp-test10-write-region-file-precious-flag ()
  "Check that `file-precious-flag' is respected with Tramp in use."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))

  (let* ((tmp-name (tramp--test-make-temp-name))
         (inhibit-message t)
         written-files
         (advice (lambda (_start _end filename &rest _r)
                   (push filename written-files))))

    (unwind-protect
        (with-current-buffer (find-file-noselect tmp-name)
          ;; Write initial contents.  Adapt `visited-file-modtime'
          ;; in order to suppress confirmation.
          (insert "foo")
          (write-region nil nil tmp-name)
          (set-visited-file-modtime)
          ;; Run the test.
          (advice-add 'write-region :before advice)
          (setq-local file-precious-flag t)
          (setq-local backup-inhibited t)
          (insert "bar")
	  (should (buffer-modified-p))
          (should (null (save-buffer)))
	  (should (not (buffer-modified-p)))
          (should-not (cl-member tmp-name written-files :test #'string=)))

      ;; Cleanup.
      (ignore-errors (advice-remove 'write-region advice))
      (ignore-errors (delete-file tmp-name)))))

;; The following test is inspired by Bug#55166.
(ert-deftest tramp-test10-write-region-other-file-name-handler ()
  "Check that another file name handler in VISIT is acknowledged."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))
  (skip-unless (executable-find "gzip"))

  (let* ((default-directory ert-remote-temporary-file-directory)
	 (archive (ert-resource-file "foo.tar.gz"))
	 (tmp-file (expand-file-name (file-name-nondirectory archive)))
	 (require-final-newline t)
	 (inhibit-message t)
         (backup-inhibited t)
	 create-lockfiles buffer1 buffer2)
    (unwind-protect
	(progn
	  (copy-file archive tmp-file 'ok)
	  ;; Read archive.  Check contents of foo.txt, and modify it.  Save.
	  (with-current-buffer (setq buffer1 (find-file-noselect tmp-file))
	    (should (tar-goto-file "foo.txt"))
	    (save-current-buffer
	      (setq buffer2 (tar-extract))
	      (should (string-equal (buffer-string) "foo\n"))
	      (goto-char (point-max))
	      (insert "bar")
	      (should (buffer-modified-p))
              (should (null (save-buffer)))
	      (should-not (buffer-modified-p)))
	    (should (buffer-modified-p))
            (should (null (save-buffer)))
	    (should-not (buffer-modified-p)))

	  (kill-buffer buffer1)
	  (kill-buffer buffer2)
	  ;; Read archive.  Check contents of modified foo.txt.
	  (with-current-buffer (setq buffer1 (find-file-noselect tmp-file))
	    (should (tar-goto-file "foo.txt"))
	    (save-current-buffer
	      (setq buffer2 (tar-extract))
	      (should (string-equal (buffer-string) "foo\nbar\n")))))

      ;; Cleanup.
      (ignore-errors (kill-buffer buffer1))
      (ignore-errors (kill-buffer buffer2))
      (ignore-errors (delete-file tmp-file)))))

(ert-deftest tramp-test11-copy-file ()
  "Check `copy-file'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
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
		(should-error
		 (copy-file source target)
		 :type 'file-missing)
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(copy-file source target)
		(should (file-exists-p target))
		(with-temp-buffer
		  (insert-file-contents target)
		  (should (string-equal (buffer-string) "foo")))
		(when (tramp--test-expensive-test-p)
		  (should-error
		   (copy-file source target)
		   :type 'file-already-exists))
		(copy-file source target 'ok))

	    ;; Cleanup.
	    (ignore-errors (delete-file source))
	    (ignore-errors (delete-file target)))

	  ;; Copy file to directory.
	  (unwind-protect
	      ;; This doesn't work on FTP.
	      (unless (tramp--test-ange-ftp-p)
		(write-region "foo" nil source)
		(should (file-exists-p source))
		(make-directory target)
		(should (file-directory-p target))
		(when (tramp--test-expensive-test-p)
		  (should-error
		   (copy-file source target)
		   :type 'file-already-exists)
		  (should-error
		   (copy-file source target 'ok)
		   :type 'file-error))
		(copy-file source (file-name-as-directory target))
		(should
		 (file-exists-p
		  (expand-file-name (file-name-nondirectory source) target))))

	    ;; Cleanup.
	    (ignore-errors (delete-file source))
	    (ignore-errors (delete-directory target 'recursive)))

	  ;; Copy directory to existing directory.
	  (unwind-protect
	      ;; This doesn't work on FTP.
	      (unless (tramp--test-ange-ftp-p)
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
	      ;; This doesn't work on FTP.
	      (unless (tramp--test-ange-ftp-p)
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

  (dolist (quoted  (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
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
		(should-error
		 (rename-file source target)
		 :type 'file-missing)
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
		(when (tramp--test-expensive-test-p)
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
		(when (tramp--test-expensive-test-p)
		  (should-error
		   (rename-file source target)
		   :type 'file-already-exists)
		  (should-error
		   (rename-file source target 'ok)
		   :type 'file-error))
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
	      ;; This doesn't work on FTP.
	      (unless (tramp--test-ange-ftp-p)
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
	      ;; This doesn't work on FTP.
	      (unless (tramp--test-ange-ftp-p)
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

  ;; Since Emacs 29.1, `make-directory' has defined return values.
  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "foo/bar" tmp-name1))
	   (unusual-file-mode-1 #o740)
	   (unusual-file-mode-2 #o710))
      (unwind-protect
	  (progn
	    (with-file-modes unusual-file-mode-1
	      (if (tramp--test-emacs29-p)
		  (should-not (make-directory tmp-name1))
		(make-directory tmp-name1)))
	    (should-error
	     (make-directory tmp-name1)
	     :type 'file-already-exists)
	    (should (file-directory-p tmp-name1))
	    (should (file-accessible-directory-p tmp-name1))
	    (when (tramp--test-supports-set-file-modes-p)
	      (should (equal (format "%#o" unusual-file-mode-1)
			     (format "%#o" (file-modes tmp-name1)))))
	    (should-error
	     (make-directory tmp-name2)
	     :type 'file-error)
	    (with-file-modes unusual-file-mode-2
	      (if (tramp--test-emacs29-p)
		  (should-not (make-directory tmp-name2 'parents))
		(make-directory tmp-name2 'parents)))
	    (should (file-directory-p tmp-name2))
	    (should (file-accessible-directory-p tmp-name2))
	    (when (tramp--test-supports-set-file-modes-p)
	      (should (equal (format "%#o" unusual-file-mode-2)
			     (format "%#o" (file-modes tmp-name2)))))
	    ;; If PARENTS is non-nil, `make-directory' shall not
	    ;; signal an error when DIR exists already.  It returns t.
	    (if (tramp--test-emacs29-p)
		(should (make-directory tmp-name2 'parents))
	      (make-directory tmp-name2 'parents)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test14-delete-directory ()
  "Check `delete-directory'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "foo" tmp-name1)))
      ;; Delete empty directory.
      (make-directory tmp-name1)
      (should (file-directory-p tmp-name1))
      (delete-directory tmp-name1)
      (should-not (file-directory-p tmp-name1))
      ;; Delete non-empty directory.
      (make-directory tmp-name1)
      (should (file-directory-p tmp-name1))
      (write-region "foo" nil (expand-file-name "bla" tmp-name1))
      (should (file-exists-p (expand-file-name "bla" tmp-name1)))
      (make-directory tmp-name2)
      (should (file-directory-p tmp-name2))
      (write-region "foo" nil (expand-file-name "bla" tmp-name2))
      (should (file-exists-p (expand-file-name "bla" tmp-name2)))
      (should-error
       (delete-directory tmp-name1)
       :type 'file-error)
      (delete-directory tmp-name1 'recursive)
      (should-not (file-directory-p tmp-name1))

      ;; Trashing directories doesn't work when
      ;; `system-move-file-to-trash' is defined (on MS Windows and
      ;; macOS), for encrypted remote directories and for ange-ftp.
      (when (and (not (fboundp 'system-move-file-to-trash))
		 (not (tramp--test-crypt-p)) (not (tramp--test-ftp-p)))
	(let ((trash-directory (tramp--test-make-temp-name 'local quoted))
	      (delete-by-moving-to-trash t))
	  (make-directory trash-directory)
	  ;; Delete empty directory.
	  (make-directory tmp-name1)
	  (should (file-directory-p tmp-name1))
	  (delete-directory tmp-name1 nil 'trash)
	  (should-not (file-directory-p tmp-name1))
	  (should
	   (file-exists-p
	    (expand-file-name
	     (file-name-nondirectory tmp-name1) trash-directory)))
	  (delete-directory trash-directory 'recursive)
	  (should-not (file-exists-p trash-directory))
	  ;; Delete non-empty directory.
	  (make-directory tmp-name1)
	  (should (file-directory-p tmp-name1))
	  (write-region "foo" nil (expand-file-name "bla" tmp-name1))
	  (should (file-exists-p (expand-file-name "bla" tmp-name1)))
	  (make-directory tmp-name2)
	  (should (file-directory-p tmp-name2))
	  (write-region "foo" nil (expand-file-name "bla" tmp-name2))
	  (should (file-exists-p (expand-file-name "bla" tmp-name2)))
	  (should-error
	   (delete-directory tmp-name1 nil 'trash)
	   ;; tramp-rclone.el and tramp-sshfs.el call the local
	   ;; `delete-directory'.  This raises another error.
	   :type (if (tramp--test-fuse-p) 'error 'file-error))
	  (delete-directory tmp-name1 'recursive 'trash)
	  (should-not (file-directory-p tmp-name1))
	  (should
	   (file-exists-p
	    (format
	     "%s/%s/bla" trash-directory (file-name-nondirectory tmp-name1))))
	  (should
	   (file-exists-p
	    (format
	     "%s/%s/%s/bla" trash-directory (file-name-nondirectory tmp-name1)
	     (file-name-nondirectory tmp-name2))))
	  (delete-directory trash-directory 'recursive)
	  (should-not (file-exists-p trash-directory))))

      ;; Setting `remote-file-name-inhibit-delete-by-moving-to-trash'
      ;; prevents trashing remote files.
      (let ((trash-directory (tramp--test-make-temp-name 'local quoted))
	    (delete-by-moving-to-trash t)
	    (remote-file-name-inhibit-delete-by-moving-to-trash t))
	(make-directory trash-directory)
	(make-directory tmp-name1)
	(should (file-directory-p tmp-name1))
	(delete-directory tmp-name1 nil 'trash)
	(should-not (file-exists-p tmp-name1))
	(should-not
	 (file-exists-p
	  (expand-file-name (file-name-nondirectory tmp-name1) trash-directory)))
	(delete-directory trash-directory 'recursive)
	(should-not (file-exists-p trash-directory))))))

(ert-deftest tramp-test15-copy-directory ()
  "Check `copy-directory'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-rclone-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name nil quoted))
	   (tmp-name3 (expand-file-name
		       (file-name-nondirectory tmp-name1) tmp-name2))
	   (tmp-name4 (expand-file-name "foo" tmp-name1))
	   (tmp-name5 (expand-file-name "foo" tmp-name2))
	   (tmp-name6 (expand-file-name "foo" tmp-name3))
	   (tmp-name7 (tramp--test-make-temp-name nil quoted)))

      ;; Copy complete directory.
      (unwind-protect
	  (progn
	    (should-error
	     (copy-directory tmp-name1 tmp-name2)
	     :type 'file-missing)
	    ;; Copy empty directory.
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name4)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name4))
	    (copy-directory tmp-name1 tmp-name2)
	    (should (file-directory-p tmp-name2))
	    (should (file-exists-p tmp-name5))
	    ;; Target directory does exist already.
	    (should-error
	     (copy-directory tmp-name1 tmp-name2)
	     :type 'file-already-exists)
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
	  (delete-directory tmp-name2 'recursive)))

      ;; Copy symlink to directory.
      (dolist (copy-directory-create-symlink '(nil t))
	(unwind-protect
	    (tramp--test-ignore-make-symbolic-link-error
	     ;; Copy to file name.
	     (make-directory tmp-name1)
	     (write-region "foo" nil tmp-name4)
	     (make-symbolic-link tmp-name1 tmp-name7)
	     (should (file-directory-p tmp-name1))
	     (should (file-exists-p tmp-name4))
	     (should (file-symlink-p tmp-name7))
	     (copy-directory tmp-name7 tmp-name2)
	     (if copy-directory-create-symlink
		 (should
		  (string-equal
		   (file-symlink-p tmp-name2) (file-symlink-p tmp-name7)))
	       (should (file-directory-p tmp-name2)))
	     ;; Copy to directory name.
	     (delete-directory tmp-name2 'recursive)
	     (make-directory tmp-name2)
	     (should (file-directory-p tmp-name2))
	     (copy-directory tmp-name7 (file-name-as-directory tmp-name2))
	     (if copy-directory-create-symlink
		 (should
		  (string-equal
		   (file-symlink-p
		    (expand-file-name
		     (file-name-nondirectory tmp-name7) tmp-name2))
		   (file-symlink-p tmp-name7)))
	       (should
		(file-directory-p
		 (expand-file-name
		  (file-name-nondirectory tmp-name7) tmp-name2)))))

	  ;; Cleanup.
	  (ignore-errors
	    (delete-directory tmp-name1 'recursive)
	    (delete-directory tmp-name2 'recursive)
	    (delete-directory tmp-name7 'recursive)))))))

(ert-deftest tramp-test16-directory-files ()
  "Check `directory-files'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tramp-fuse-remove-hidden-files t)
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "bla" tmp-name1))
	   (tmp-name3 (expand-file-name "foo" tmp-name1)))
      (unwind-protect
	  (progn
	    (should-error
	     (directory-files tmp-name1)
	     :type 'file-missing)
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
			   `(,tmp-name2 ,tmp-name3)))
	    ;; Check the COUNT arg.
	    (should
	     (equal
	      (directory-files
	       tmp-name1 nil directory-files-no-dot-files-regexp nil 1)
	      '("bla"))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

;; This is not a file name handler test.  But Tramp needed to apply an
;; advice for older Emacs versions, so we check that this has been fixed.
(ert-deftest tramp-test16-file-expand-wildcards ()
  "Check `file-expand-wildcards'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tramp-fuse-remove-hidden-files t)
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
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
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test17-insert-directory ()
  "Check `insert-directory'."
  (skip-unless (tramp--test-enabled))
  ;; Ange-FTP is very special.  It does not include the header line
  ;; (this is performed by `dired').  If FULL is nil, it shows just
  ;; one file.  So we refrain from testing.
  (skip-unless (not (tramp--test-ange-ftp-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
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
	      (should (looking-at-p (rx (literal tmp-name1)))))
	    (with-temp-buffer
	      (insert-directory (file-name-as-directory tmp-name1) nil)
	      (goto-char (point-min))
	      (should
               (looking-at-p (rx (literal (file-name-as-directory tmp-name1))))))
	    (with-temp-buffer
	      (insert-directory tmp-name1 "-al")
	      (goto-char (point-min))
	      (should
	       (looking-at-p (rx bol (+ nonl) blank (literal tmp-name1) eol))))
	    (with-temp-buffer
	      (insert-directory (file-name-as-directory tmp-name1) "-al")
	      (goto-char (point-min))
	      (should
	       (looking-at-p
		(rx bol (+ nonl) blank (literal tmp-name1) "/" eol))))
	    (with-temp-buffer
	      (insert-directory
	       (file-name-as-directory tmp-name1) "-al" nil 'full-directory-p)
	      (goto-char (point-min))
	      (should
	       (looking-at-p
		(rx-to-string
		 `(:
		   ;; There might be a summary line.
		   (? (* blank) "total" (+ nonl) (+ digit) (? blank)
		      (? (any "EGKMPTYZk")) (? "i") (? "B") "\n")
		   ;; We don't know in which order ".", ".." and "foo" appear.
		   (= ,(length (directory-files tmp-name1))
		      (+ nonl) blank
		      (| . ,(directory-files tmp-name1))
		      (? " ->" (+ nonl)) "\n"))))))

	    ;; Check error cases.
	    (when (and (tramp--test-supports-set-file-modes-p)
		       ;; With "sshfs", directories with zero file
		       ;; modes are still "accessible".
		       (not (tramp--test-sshfs-p))
		       ;; A directory is always accessible for user "root".
		       (not (zerop (file-attribute-user-id
				    (file-attributes tmp-name1)))))
	      (set-file-modes tmp-name1 0)
	      (with-temp-buffer
		(should-error
		 (insert-directory tmp-name1 nil)
		 :type 'file-error))
	      (set-file-modes tmp-name1 #o777))
	    (delete-directory tmp-name1 'recursive)
	    (with-temp-buffer
	      (should-error
	       (insert-directory tmp-name1 nil)
	       :type 'file-missing)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test17-dired-with-wildcards ()
  "Check `dired' with wildcards."
  ;; `separate' syntax and IPv6 host name syntax do not work.
  (skip-unless
   (not (string-match-p (rx "[") ert-remote-temporary-file-directory)))
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-rsync-p)))
  ;; Wildcards are not supported in tramp-crypt.el.
  (skip-unless (not (tramp--test-crypt-p)))
  ;; Wildcards are not supported with "docker cp ..." or "podman cp ...".
  (skip-unless (not (tramp--test-container-oob-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tmp-name1
	    (expand-file-name (tramp--test-make-temp-name nil quoted)))
	   (tmp-name2
            (expand-file-name (tramp--test-make-temp-name nil quoted)))
	   (tmp-name3 (expand-file-name "foo" tmp-name1))
	   (tmp-name4 (expand-file-name "bar" tmp-name2))
	   (ert-remote-temporary-file-directory
	    (funcall
	     (if quoted #'file-name-quote #'identity)
	     ert-remote-temporary-file-directory))
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
			"tramp-test*" ert-remote-temporary-file-directory)))
	      (goto-char (point-min))
	      (should
	       (search-forward-regexp
		(rx
		 (literal
		  (file-relative-name
		   tmp-name1 ert-remote-temporary-file-directory)))))
	      (goto-char (point-min))
	      (should
	       (search-forward-regexp
		(rx
		 (literal
		  (file-relative-name
		   tmp-name2 ert-remote-temporary-file-directory))))))
	    (kill-buffer buffer)

	    ;; Check for expanded directory and file names.
	    (with-current-buffer
		(setq buffer
		      (dired-noselect
		       (expand-file-name
			"tramp-test*/*" ert-remote-temporary-file-directory)))
	      (goto-char (point-min))
	      (should
	       (search-forward-regexp
		(rx
		 (literal
		  (file-relative-name
		   tmp-name3 ert-remote-temporary-file-directory)))))
	      (goto-char (point-min))
	      (should
	       (search-forward-regexp
		(rx
		 (literal
		  (file-relative-name
		   tmp-name4
		   ert-remote-temporary-file-directory))))))
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
			"tramp-test*/*" ert-remote-temporary-file-directory)))
	      (goto-char (point-min))
	      (should
	       (search-forward-regexp
		(rx
		 (literal
		  (file-relative-name
		   tmp-name3 ert-remote-temporary-file-directory)))))
	      (goto-char (point-min))
	      (should
	       (search-forward-regexp
		(rx
		 (literal
		  (file-relative-name
		   tmp-name4
		   ert-remote-temporary-file-directory))))))
	    (kill-buffer buffer))

	;; Cleanup.
	(ignore-errors (kill-buffer buffer))
	(ignore-errors (delete-directory tmp-name1 'recursive))
	(ignore-errors (delete-directory tmp-name2 'recursive))))))

;; The following test is inspired by Bug#45691.
(ert-deftest tramp-test17-insert-directory-one-file ()
  "Check `insert-directory' inside directory listing."
  (skip-unless (tramp--test-enabled))
  ;; Relative file names in dired are not supported in tramp-crypt.el.
  (skip-unless (not (tramp--test-crypt-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tmp-name1
	    (expand-file-name (tramp--test-make-temp-name nil quoted)))
	   (tmp-name2 (expand-file-name "foo" tmp-name1))
	   (tmp-name3 (expand-file-name "bar" tmp-name1))
	   (dired-copy-preserve-time t)
	   (dired-recursive-copies 'top)
	   dired-copy-dereference
	   buffer)
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (write-region "foo" nil tmp-name2)
	    (should (file-directory-p tmp-name1))
	    (should (file-exists-p tmp-name2))

	    ;; Check, that `insert-directory' works properly.
	    (with-current-buffer
		(setq buffer (dired-noselect tmp-name1 "--dired -al"))
	      (read-only-mode -1)
	      (goto-char (point-min))
	      (while (not (or (eobp)
			      (string-equal
			       (dired-get-filename 'no-dir 'no-error)
			       (file-name-nondirectory tmp-name2))))
		(forward-line 1))
	      (should-not (eobp))
	      (copy-file tmp-name2 tmp-name3)
	      (insert-directory
	       (file-name-nondirectory tmp-name3) "--dired -al -d")
	      ;; Point shall still be the recent file.
	      (should
	       (string-equal
		(dired-get-filename 'no-dir 'no-error)
		(file-name-nondirectory tmp-name2)))
	      (should-not (search-forward "dired" nil t))
	      ;; The copied file has been inserted the line before.
	      (forward-line -1)
	      (should
	       (string-equal
		(dired-get-filename 'no-dir 'no-error)
		(file-name-nondirectory tmp-name3))))
	    (kill-buffer buffer))

	;; Cleanup.
	(ignore-errors (kill-buffer buffer))
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test18-file-attributes ()
  "Check `file-attributes'.
This tests also `access-file', `file-readable-p',
`file-regular-p' and `file-ownership-preserved-p'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory,
    ;; because it could be located on a symlinked directory.  This
    ;; would let the test fail.
    (let* ((ert-remote-temporary-file-directory
	    (file-truename ert-remote-temporary-file-directory))
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name nil quoted))
	   ;; File name with "//".
	   (tmp-name3
	    (format
	     "%s%s"
	     (file-remote-p tmp-name1)
	     (replace-regexp-in-string
	      "/" "//" (file-remote-p tmp-name1 'localname))))
	   ;; `file-ownership-preserved-p' is implemented only in tramp-sh.el.
	   (test-file-ownership-preserved-p (tramp--test-sh-p))
	   attr)
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name1)
	    ;; `access-file' returns nil in case of success.
	    (should-not (access-file tmp-name1 "error"))
	    ;; `access-file' could use a timeout.
	    (let ((remote-file-name-access-timeout 1)
		  comp-warn-primitives)
	      (cl-letf (((symbol-function #'file-exists-p)
			 (lambda (_filename) (sleep-for 5))))
		(should-error
		 (access-file tmp-name1 "error")
		 :type 'file-error)))
	    (delete-file tmp-name1)

	    ;; A sticky bit could damage the `file-ownership-preserved-p' test.
	    (when
		(and test-file-ownership-preserved-p
		     (zerop (logand
			     #o1000
			     (file-modes ert-remote-temporary-file-directory))))
	      (write-region "foo" nil tmp-name1)
	      (setq test-file-ownership-preserved-p
		    (= (file-attribute-group-id (file-attributes tmp-name1))
		       (tramp-get-remote-gid tramp-test-vec 'integer)))
	      (delete-file tmp-name1))

	    (when (tramp--test-supports-set-file-modes-p)
	      (write-region "foo" nil tmp-name1)
	      ;; A file is always accessible for user "root".
	      (unless
		  (zerop (file-attribute-user-id (file-attributes tmp-name1)))
		(set-file-modes tmp-name1 0)
		(should-error
		 (access-file tmp-name1 "error")
		 :type tramp-permission-denied)
		(set-file-modes tmp-name1 #o777))
	      (delete-file tmp-name1))
	    (should-error
	     (access-file tmp-name1 "error")
	     :type 'file-missing)

	    (should-not (file-exists-p tmp-name1))
	    (should-not (file-readable-p tmp-name1))
	    (should-not (file-regular-p tmp-name1))
	    ;; `file-ownership-preserved-p' should return t for
	    ;; non-existing files.
	    (when test-file-ownership-preserved-p
	      (should (file-ownership-preserved-p tmp-name1 'group)))
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-readable-p tmp-name1))
	    (should (file-regular-p tmp-name1))
	    (should-not (access-file tmp-name1 "error"))
	    (when test-file-ownership-preserved-p
	      (should (file-ownership-preserved-p tmp-name1 'group)))

	    ;; We do not test inodes and device numbers.
	    (setq attr (file-attributes tmp-name1))
	    (should (consp attr))
	    (should (null (file-attribute-type attr)))
	    (should (numberp (file-attribute-link-number attr)))
	    (should (numberp (file-attribute-user-id attr)))
	    (should (numberp (file-attribute-group-id attr)))
	    (should
	     (stringp (current-time-string (file-attribute-access-time attr))))
	    (should
	     (stringp
	      (current-time-string (file-attribute-modification-time attr))))
	    (should
	     (stringp
	      (current-time-string (file-attribute-status-change-time attr))))
	    (should (numberp (file-attribute-size attr)))
	    (should (stringp (file-attribute-modes attr)))

	    (setq attr (file-attributes tmp-name1 'string))
	    (should (stringp (file-attribute-user-id attr)))
	    (should (stringp (file-attribute-group-id attr)))

	    (tramp--test-ignore-make-symbolic-link-error
	      (should-error
	       (access-file tmp-name2 "error")
	       :type 'file-missing)
	      (when test-file-ownership-preserved-p
		(should (file-ownership-preserved-p tmp-name2 'group)))
	      (make-symbolic-link tmp-name1 tmp-name2)
	      (should (file-exists-p tmp-name2))
	      (should (file-symlink-p tmp-name2))
	      (should-not (access-file tmp-name2 "error"))
	      (when test-file-ownership-preserved-p
		(should (file-ownership-preserved-p tmp-name2 'group)))
	      (setq attr (file-attributes tmp-name2))
	      (should
	       (string-equal
		(funcall
		 (if quoted #'file-name-quote #'identity)
		 (file-attribute-type attr))
		(file-remote-p (file-truename tmp-name1) 'localname)))
	      (delete-file tmp-name2))

	    ;; Check, that "//" in symlinks are handled properly.
	    (with-temp-buffer
	      (let ((default-directory ert-remote-temporary-file-directory))
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
		(file-attribute-type attr)
		(funcall
		 (if (tramp--test-sshfs-p) #'file-name-nondirectory #'identity)
		 (tramp-file-name-localname
		  (tramp-dissect-file-name tmp-name3)))))
	      (delete-file tmp-name2))

	    (when test-file-ownership-preserved-p
	      (should (file-ownership-preserved-p tmp-name1 'group)))
	    (delete-file tmp-name1)
	    (make-directory tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-readable-p tmp-name1))
	    (should-not (file-regular-p tmp-name1))
	    (should-not (access-file tmp-name1 "error"))
	    (when test-file-ownership-preserved-p
	      (should (file-ownership-preserved-p tmp-name1 'group)))
	    (setq attr (file-attributes tmp-name1))
	    (should (eq (file-attribute-type attr) t)))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name2))))))

(defun tramp--test-set-ert-test-documentation (test command)
  "Set the documentation string for a derived test.
The test is derived from TEST and COMMAND."
  (let ((test-doc
	 (split-string (ert-test-documentation (get test 'ert--test)) "\n")))
    ;; The first line must be extended.
    (setcar
     test-doc (format "%s  Use the \"%s\" command." (car test-doc) command))
    (setf (ert-test-documentation
	   (get (intern (format "%s-with-%s" test command)) 'ert--test))
	  (string-join test-doc "\n"))))

(defmacro tramp--test-deftest-with-stat (test)
  "Define ert `TEST-with-stat'."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-with-stat")) ()
     :tags '(:expensive-test)
     (tramp--test-set-ert-test-documentation ',test "stat")
     (skip-unless (tramp--test-enabled))
     (skip-unless (tramp--test-sh-p))
     (skip-unless (tramp-get-remote-stat tramp-test-vec))
     (if-let* ((default-directory ert-remote-temporary-file-directory)
	       (ert-test (ert-get-test ',test))
	       (result (ert-test-most-recent-result ert-test))
	       (tramp-connection-properties
		(cons '(nil "perl" nil)
		      tramp-connection-properties)))
	 (progn
	   (skip-unless (< (ert-test-result-duration result) 300))
	   (funcall (ert-test-body ert-test)))
       (ert-skip (format "Test `%s' must run before" ',test)))))

(defmacro tramp--test-deftest-with-perl (test)
  "Define ert `TEST-with-perl'."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-with-perl")) ()
     :tags '(:expensive-test)
     (tramp--test-set-ert-test-documentation ',test "perl")
     (skip-unless (tramp--test-enabled))
     (skip-unless (tramp--test-sh-p))
     (skip-unless (tramp-get-remote-perl tramp-test-vec))
     (if-let* ((default-directory ert-remote-temporary-file-directory)
	       (ert-test (ert-get-test ',test))
	       (result (ert-test-most-recent-result ert-test))
	       (tramp-connection-properties
		(append
		 '((nil "stat" nil)
		   ;; See `tramp-sh-handle-file-truename'.
		   (nil "readlink" nil)
		   ;; See `tramp-sh-handle-get-remote-*'.
		   (nil "id" nil))
		 tramp-connection-properties)))
	 (progn
	   (skip-unless (< (ert-test-result-duration result) 300))
	   (funcall (ert-test-body ert-test)))
       (ert-skip (format "Test `%s' must run before" ',test)))))

(defmacro tramp--test-deftest-with-ls (test)
  "Define ert `TEST-with-ls'."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-with-ls")) ()
     :tags '(:expensive-test)
     (tramp--test-set-ert-test-documentation ',test "ls")
     (skip-unless (tramp--test-enabled))
     (skip-unless (tramp--test-sh-p))
     (if-let* ((default-directory ert-remote-temporary-file-directory)
	       (ert-test (ert-get-test ',test))
	       (result (ert-test-most-recent-result ert-test))
	       (tramp-connection-properties
		(append
		 '((nil "perl" nil)
		   (nil "stat" nil)
		   ;; See `tramp-sh-handle-file-truename'.
		   (nil "readlink" nil))
		 tramp-connection-properties)))
	 (progn
	   (skip-unless (< (ert-test-result-duration result) 300))
	   (funcall (ert-test-body ert-test)))
       (ert-skip (format "Test `%s' must run before" ',test)))))

(defmacro tramp--test-deftest-without-file-attributes (test)
  "Define ert `TEST-without-file-attributes'."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (concat (symbol-name test) "-without-file-attributes")) ()
     :tags '(:expensive-test)
     (let ((test-doc
	    (split-string
	     (ert-test-documentation (get ',test 'ert--test)) "\n")))
       ;; The first line must be extended.
       (setcar
	test-doc
	(format "%s  Don't Use the \"file-attributes\" cache." (car test-doc)))
       (setf (ert-test-documentation
	      (get
	       (intern (format "%s-without-file-attributes" ',test))
	       'ert--test))
	     (string-join test-doc "\n")))
     (skip-unless (tramp--test-enabled))
     (skip-unless
      (or (tramp--test-adb-p) (tramp--test-sh-p) (tramp--test-sudoedit-p)))
     (if-let* ((default-directory ert-remote-temporary-file-directory)
	       (ert-test (ert-get-test ',test))
	       (result (ert-test-most-recent-result ert-test)))
	 (progn
	   (skip-unless (< (ert-test-result-duration result) 300))
	   (let (tramp-use-file-attributes)
	     (funcall (ert-test-body ert-test))))
       (ert-skip (format "Test `%s' must run before" ',test)))))

(tramp--test-deftest-with-stat tramp-test18-file-attributes)

(tramp--test-deftest-with-perl tramp-test18-file-attributes)

(tramp--test-deftest-with-ls tramp-test18-file-attributes)

(tramp--test-deftest-without-file-attributes tramp-test18-file-attributes)

(defvar tramp--test-start-time nil
  "Keep the start time of the current test, a float number.")

(defsubst tramp--test-file-attributes-equal-p (attr1 attr2)
  "Check, whether file attributes ATTR1 and ATTR2 are equal.
They might differ only in time attributes or directory size."
  (let ((attr1 (copy-sequence attr1))
	(attr2 (copy-sequence attr2))
	(start-time (- tramp--test-start-time 10)))
    ;; Link number.  For directories, it includes the number of
    ;; subdirectories.  Set it to 1.
    (when (eq (file-attribute-type attr1) t)
      (setcar (nthcdr 1 attr1) 1))
    (when (eq (file-attribute-type attr2) t)
      (setcar (nthcdr 1 attr2) 1))
    ;; Access time.
    (setcar (nthcdr 4 attr1) tramp-time-dont-know)
    (setcar (nthcdr 4 attr2) tramp-time-dont-know)
    ;; Modification time.  If any of the time values is "don't know",
    ;; we cannot compare, and we normalize the time stamps.  If the
    ;; time value is newer than the test start time, normalize it,
    ;; because due to caching the time stamps could differ slightly (a
    ;; few seconds).  We use a test start time minus 10 seconds, in
    ;; order to compensate a possible timestamp resolution higher than
    ;; a second on the remote machine.
    (when (or (time-equal-p
	       (file-attribute-modification-time attr1) tramp-time-dont-know)
	      (time-equal-p
	       (file-attribute-modification-time attr2) tramp-time-dont-know))
      (setcar (nthcdr 5 attr1) tramp-time-dont-know)
      (setcar (nthcdr 5 attr2) tramp-time-dont-know))
    (when (< start-time
	     (float-time (file-attribute-modification-time attr1)))
      (setcar (nthcdr 5 attr1) tramp-time-dont-know))
    (when (< start-time
	     (float-time (file-attribute-modification-time attr2)))
      (setcar (nthcdr 5 attr2) tramp-time-dont-know))
    ;; Status change time.  Ditto.
    (when (or (time-equal-p
	       (file-attribute-status-change-time attr1) tramp-time-dont-know)
	      (time-equal-p
	       (file-attribute-status-change-time attr2) tramp-time-dont-know))
      (setcar (nthcdr 6 attr1) tramp-time-dont-know)
      (setcar (nthcdr 6 attr2) tramp-time-dont-know))
    (when (< start-time (float-time (file-attribute-status-change-time attr1)))
      (setcar (nthcdr 6 attr1) tramp-time-dont-know))
    (when (< start-time (float-time (file-attribute-status-change-time attr2)))
      (setcar (nthcdr 6 attr2) tramp-time-dont-know))
    ;; Size.  Set it to 0 for directories, because it might have
    ;; changed.  For example the upper directory "../".
    (when (eq (file-attribute-type attr1) t)
      (setcar (nthcdr 7 attr1) 0))
    (when (eq (file-attribute-type attr2) t)
      (setcar (nthcdr 7 attr2) 0))
    ;; The check.
    (unless (equal attr1 attr2) (tramp--test-message "%S\n%S" attr1 attr2))
    (equal attr1 attr2)))

;; This isn't 100% correct, but better than no explainer at all.
(put #'tramp--test-file-attributes-equal-p 'ert-explainer #'ert--explain-equal)

(ert-deftest tramp-test19-directory-files-and-attributes ()
  "Check `directory-files-and-attributes'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    ;; `directory-files-and-attributes' contains also values for
    ;; "../".  Ensure that this doesn't change during tests, for
    ;; example due to handling temporary files.
    (let* ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "bla" tmp-name1))
	   attr)
      (unwind-protect
	  (progn
	    (should-error
	     (directory-files-and-attributes tmp-name1)
	     :type 'file-missing)
	    (make-directory tmp-name1)
	    (should (file-directory-p tmp-name1))
	    (setq tramp--test-start-time
		  (float-time
		   (file-attribute-modification-time
		    (file-attributes tmp-name1))))
	    (make-directory tmp-name2)
	    (should (file-directory-p tmp-name2))
	    (write-region "foo" nil (expand-file-name "foo" tmp-name2))
	    (write-region "bar" nil (expand-file-name "bar" tmp-name2))
	    (write-region "boz" nil (expand-file-name "boz" tmp-name2))

	    (setq attr (directory-files-and-attributes tmp-name2))
	    (should (consp attr))
	    (dolist (elt attr)
	      (should
	       (tramp--test-file-attributes-equal-p
		(file-attributes (expand-file-name (car elt) tmp-name2))
		(cdr elt))))

	    (setq attr (directory-files-and-attributes tmp-name2 'full))
	    (should (consp attr))
	    (dolist (elt attr)
	      (should
	       (tramp--test-file-attributes-equal-p
		(file-attributes (car elt)) (cdr elt))))

	    (setq attr (directory-files-and-attributes
			tmp-name2 nil (rx bos "b")))
	    (should (equal (mapcar #'car attr) '("bar" "boz")))

	    ;; Check the COUNT arg.
	    (setq attr (directory-files-and-attributes
			tmp-name2 nil (rx bos "b") nil nil 1))
	    (should (equal (mapcar #'car attr) '("bar"))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(tramp--test-deftest-with-stat tramp-test19-directory-files-and-attributes)

(tramp--test-deftest-with-perl tramp-test19-directory-files-and-attributes)

(tramp--test-deftest-with-ls tramp-test19-directory-files-and-attributes)

(tramp--test-deftest-without-file-attributes
 tramp-test19-directory-files-and-attributes)

(ert-deftest tramp-test20-file-modes ()
  "Check `file-modes'.
This tests also `file-executable-p', `file-writable-p' and `set-file-modes'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-set-file-modes-p))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted)))

      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (set-file-modes tmp-name1 #o777)
	    (should (= (file-modes tmp-name1) #o777))
	    (should (file-executable-p tmp-name1))
	    (should (file-writable-p tmp-name1))
	    (set-file-modes tmp-name1 #o444)
	    (should (= (file-modes tmp-name1) #o444))
	    (should-not (file-executable-p tmp-name1))
	    ;; A file is always writable for user "root".
	    (unless
		(or (zerop (file-attribute-user-id (file-attributes tmp-name1)))
		    (tramp--test-sshfs-p))
	      (should-not (file-writable-p tmp-name1)))
	    ;; Check the NOFOLLOW arg.  For regular files, there
	    ;; shouldn't be a difference.
	    (set-file-modes tmp-name1 #o222 'nofollow)
	    (should (= (file-modes tmp-name1 'nofollow) #o222))
	    ;; Setting the mode for not existing files shall fail.
	    (should-error
	     (set-file-modes tmp-name2 #o777)
	     :type 'file-missing))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1)))

      ;; Check the NOFOLLOW arg.  It is implemented for tramp-gvfs.el
      ;; and tramp-sh.el.  However, tramp-gvfs,el does not support
      ;; creating symbolic links.  And in tramp-sh.el, we must ensure
      ;; that the remote chmod command supports the "-h" argument.
      (when (and (tramp--test-sh-p) (tramp-get-remote-chmod-h tramp-test-vec))
	(unwind-protect
	    (progn
	      (write-region "foo" nil tmp-name1)
	      (should (file-exists-p tmp-name1))
	      (make-symbolic-link tmp-name1 tmp-name2)
	      (should
	       (string-equal
		(funcall
		 (if quoted #'file-name-unquote #'identity)
		 (file-remote-p tmp-name1 'localname))
		(file-symlink-p tmp-name2)))
	      ;; Both report the modes of `tmp-name1'.
	      (should
	       (= (file-modes tmp-name1) (file-modes tmp-name2)))
	      ;; `tmp-name1' is a regular file.  NOFOLLOW doesn't matter.
	      (should
	       (= (file-modes tmp-name1) (file-modes tmp-name1 'nofollow)))
	      ;; `tmp-name2' is a symbolic link.  It has different permissions.
	      (should-not
	       (= (file-modes tmp-name2) (file-modes tmp-name2 'nofollow)))
	      (should-not
	       (= (file-modes tmp-name1 'nofollow)
		  (file-modes tmp-name2 'nofollow)))
	      ;; Change permissions.
	      (set-file-modes tmp-name1 #o200)
	      (set-file-modes tmp-name2 #o200)
	      (should
	       (= (file-modes tmp-name1) (file-modes tmp-name2) #o200))
	      ;; Change permissions with NOFOLLOW.
	      (set-file-modes tmp-name1 #o300 'nofollow)
	      (set-file-modes tmp-name2 #o300 'nofollow)
	      (should
	       (= (file-modes tmp-name1 'nofollow)
		  (file-modes tmp-name2 'nofollow)))
	      (should-not (= (file-modes tmp-name1) (file-modes tmp-name2))))

	  ;; Cleanup.
	  (ignore-errors (delete-file tmp-name1))
	  (ignore-errors (delete-file tmp-name2)))))))

;; Method "smb" could run into "NT_STATUS_REVISION_MISMATCH" error.
(defmacro tramp--test-ignore-add-name-to-file-error (&rest body)
  "Run BODY, ignoring \"error with add-name-to-file\" file error."
  (declare (indent defun) (debug (body)))
  `(condition-case err
       (progn ,@body)
     (file-error
      (unless (string-prefix-p "error with add-name-to-file"
			       (error-message-string err))
	(signal (car err) (cdr err))))))

(ert-deftest tramp-test21-file-links ()
  "Check `file-symlink-p'.
This tests also `make-symbolic-link', `file-truename' and `add-name-to-file'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory,
    ;; because it could be located on a symlinked directory.  This
    ;; would let the test fail.
    (let* ((ert-remote-temporary-file-directory
	    (file-truename ert-remote-temporary-file-directory))
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name nil quoted))
	   (tmp-name3 (tramp--test-make-temp-name 'local quoted))
	   (tmp-name4 (tramp--test-make-temp-name nil quoted))
	   (tmp-name5
	    (expand-file-name (file-name-nondirectory tmp-name1) tmp-name4))
	   (tmp-name6 (tramp--test-make-temp-name nil quoted)))
      ;; Check `make-symbolic-link'.
      (unwind-protect
	  (tramp--test-ignore-make-symbolic-link-error
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (file-regular-p tmp-name1))
	    (make-symbolic-link tmp-name1 tmp-name2)
	    (should (file-exists-p tmp-name2))
	    (should (file-regular-p tmp-name2))
	    (should
	     (string-equal
	      (funcall
	       (if quoted #'file-name-unquote #'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name2)))
	    (when (tramp--test-expensive-test-p)
	      (should-error
	       (make-symbolic-link tmp-name1 tmp-name2)
	       :type 'file-already-exists))
	    (when (tramp--test-expensive-test-p)
	      ;; A number means interactive case.
	      (cl-letf (((symbol-function #'yes-or-no-p) #'ignore))
		(should-error
		 (make-symbolic-link tmp-name1 tmp-name2 0)
		 :type 'file-already-exists)))
	    (cl-letf (((symbol-function #'yes-or-no-p) #'always))
	      (make-symbolic-link tmp-name1 tmp-name2 0)
	      (should
	       (string-equal
		(funcall
		 (if quoted #'file-name-unquote #'identity)
		 (file-remote-p tmp-name1 'localname))
		(file-symlink-p tmp-name2))))
	    (make-symbolic-link tmp-name1 tmp-name2 'ok-if-already-exists)
	    (should
	     (string-equal
	      (funcall
	       (if quoted #'file-name-unquote #'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name2)))
	    ;; If we use the local part of `tmp-name1', it shall still work.
	    (make-symbolic-link
	     (file-remote-p tmp-name1 'localname)
	     tmp-name2 'ok-if-already-exists)
	    (should
	     (string-equal
	      (funcall
	       (if quoted #'file-name-unquote #'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name2)))
	    ;; `tmp-name3' is a local file name.  Therefore, the link
	    ;; target remains unchanged, even if quoted.
	    ;; `make-symbolic-link' might not be permitted on w32 systems.
	    (unless (tramp--test-windows-nt-p)
	      (make-symbolic-link tmp-name1 tmp-name3)
	      (should
	       (string-equal tmp-name1 (file-symlink-p tmp-name3))))
	    ;; Check directory as newname.
	    (make-directory tmp-name4)
	    (should (file-directory-p tmp-name4))
	    (should-not (file-regular-p tmp-name4))
	    (when (tramp--test-expensive-test-p)
	      (should-error
	       (make-symbolic-link tmp-name1 tmp-name4)
	       :type 'file-already-exists))
	    (make-symbolic-link tmp-name1 (file-name-as-directory tmp-name4))
	    (should
	     (string-equal
	      (funcall
	       (if quoted #'file-name-unquote #'identity)
	       (file-remote-p tmp-name1 'localname))
	      (file-symlink-p tmp-name5)))
	    ;; Check, that files in symlinked directories still work.
	    (make-symbolic-link tmp-name4 tmp-name6)
	    (should (file-symlink-p tmp-name6))
	    (should-not (file-regular-p tmp-name6))
	    (write-region "foo" nil (expand-file-name "foo" tmp-name6))
	    (delete-file (expand-file-name "foo" tmp-name6))
	    (should-not (file-exists-p (expand-file-name "foo" tmp-name4)))
	    (should-not (file-exists-p (expand-file-name "foo" tmp-name6))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-file tmp-name2))
	(ignore-errors (delete-file tmp-name3))
	(ignore-errors (delete-file tmp-name5))
	(ignore-errors (delete-file tmp-name6))
	(ignore-errors (delete-directory tmp-name4 'recursive)))

      ;; Check `add-name-to-file'.
      (unwind-protect
	  (when (tramp--test-expensive-test-p)
	    (tramp--test-ignore-add-name-to-file-error
	     (write-region "foo" nil tmp-name1)
	     (should (file-exists-p tmp-name1))
	     (add-name-to-file tmp-name1 tmp-name2)
	     (should (file-regular-p tmp-name2))
	     (should-error
	      (add-name-to-file tmp-name1 tmp-name2)
	      :type 'file-already-exists)
	     ;; A number means interactive case.
	     (cl-letf (((symbol-function #'yes-or-no-p) #'ignore))
	       (should-error
		(add-name-to-file tmp-name1 tmp-name2 0)
		:type 'file-already-exists))
	     (cl-letf (((symbol-function #'yes-or-no-p) #'always))
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
	       (expand-file-name
		(file-name-nondirectory tmp-name1) tmp-name4)))))

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
	    (should (file-regular-p tmp-name1))
	    (should (string-equal tmp-name1 (file-truename tmp-name1)))
	    (make-symbolic-link tmp-name1 tmp-name2)
	    (should (file-symlink-p tmp-name2))
	    (should (file-regular-p tmp-name2))
	    (should-not (string-equal tmp-name2 (file-truename tmp-name2)))
	    (should
	     (string-equal (file-truename tmp-name1) (file-truename tmp-name2)))
	    (should (file-equal-p tmp-name1 tmp-name2))
	    ;; Check relative symlink file name.
	    (delete-file tmp-name2)
	    (let ((default-directory ert-remote-temporary-file-directory))
	      (make-symbolic-link (file-name-nondirectory tmp-name1) tmp-name2))
	    (should (file-symlink-p tmp-name2))
	    (should (file-regular-p tmp-name2))
	    (should-not (string-equal tmp-name2 (file-truename tmp-name2)))
	    (should
	     (string-equal (file-truename tmp-name1) (file-truename tmp-name2)))
	    (should (file-equal-p tmp-name1 tmp-name2))
	    ;; Symbolic links could look like a remote file name.
	    ;; They must be quoted then.
	    (let ((penguin
		   (if (eq tramp-syntax 'separate)
		       "/[penguin/motd]" "/penguin:motd:")))
	      (delete-file tmp-name2)
	      (make-symbolic-link
	       (funcall (if quoted #'file-name-unquote #'identity) penguin)
	       tmp-name2)
	      (should (file-symlink-p tmp-name2))
	      (should-not (file-regular-p tmp-name2))
	      (should
	       (string-equal
		(file-truename tmp-name2)
		(file-name-quote (concat (file-remote-p tmp-name2) penguin)))))
	    ;; `tmp-name3' is a local file name.
	    ;; `make-symbolic-link' might not be permitted on w32 systems.
	    (unless (tramp--test-windows-nt-p)
	      (make-symbolic-link tmp-name1 tmp-name3)
	      (should (file-symlink-p tmp-name3))
	      (should-not (file-regular-p tmp-name3))
              (should-not (string-equal tmp-name3 (file-truename tmp-name3)))
	      ;; `file-truename' returns a quoted file name for `tmp-name3'.
	      ;; We must unquote it.
	      (should
	       (string-equal
		(file-truename tmp-name1)
		(file-name-unquote (file-truename tmp-name3))))))

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
	    (let* ((ert-remote-temporary-file-directory
		    (file-truename tmp-name1))
		   (tmp-name2 (tramp--test-make-temp-name nil quoted))
		   (tmp-name3 tmp-name2)
		   (number-nesting 15))
	      (dotimes (_ number-nesting)
		(make-symbolic-link
		 tmp-name3
		 (setq tmp-name3 (tramp--test-make-temp-name nil quoted))))
	      (should-not (file-regular-p tmp-name2))
	      (should-not (file-regular-p tmp-name3))
	      (should
	       (string-equal
		(file-truename tmp-name2)
		(file-truename tmp-name3)))
	      (when (tramp--test-expensive-test-p)
		(should-error
		 (with-temp-buffer (insert-file-contents tmp-name2))
		 :type 'file-missing))
	      (when (tramp--test-expensive-test-p)
		(should-error
		 (with-temp-buffer (insert-file-contents tmp-name3))
		 :type 'file-missing))
	      ;; `directory-files' does not show symlinks to
	      ;; non-existing targets in the "smb" case.  So we remove
	      ;; the symlinks manually.
	      (while (stringp (setq tmp-name2 (file-symlink-p tmp-name3)))
		(delete-file tmp-name3)
		(setq tmp-name3 (concat (file-remote-p tmp-name3) tmp-name2)))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name2))
	(ignore-errors (delete-file tmp-name3))
	(ignore-errors (delete-directory tmp-name1 'recursive)))

      ;; Detect cyclic symbolic links.
      (unwind-protect
	  (when (tramp--test-expensive-test-p)
	    (tramp--test-ignore-make-symbolic-link-error
	     (make-symbolic-link tmp-name2 tmp-name1)
	     (should (file-symlink-p tmp-name1))
	     (should-not (file-regular-p tmp-name1))
	     (should-not (file-regular-p tmp-name2))
	     (should
	      (string-equal
	       (file-truename tmp-name1)
	       (file-truename tmp-name2)))
	     (if (tramp--test-smb-p)
		 ;; The symlink command of "smbclient" detects the
		 ;; cycle already.
		 (should-error
		  (make-symbolic-link tmp-name1 tmp-name2)
		  :type 'file-error)
	       (make-symbolic-link tmp-name1 tmp-name2)
	       (should (file-symlink-p tmp-name1))
	       (should (file-symlink-p tmp-name2))
	       (should-not (file-regular-p tmp-name1))
	       (should-not (file-regular-p tmp-name2))
	       (should-error
		(file-truename tmp-name1)
		:type 'file-error)
	       (should-error
		(file-truename tmp-name2)
		:type 'file-error))))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2)))

      ;; `file-truename' shall preserve trailing slash of directories.
      (let* ((dir1
	      (directory-file-name
	       (funcall
		(if quoted #'file-name-quote #'identity)
		ert-remote-temporary-file-directory)))
	     (dir2 (file-name-as-directory dir1)))
	(should (string-equal (file-truename dir1) (expand-file-name dir1)))
	(should (string-equal (file-truename dir2) (expand-file-name dir2)))))))

(ert-deftest tramp-test22-file-times ()
  "Check `set-file-times' and `file-newer-than-file-p'."
  (skip-unless (tramp--test-enabled))
  (skip-unless
   (or (tramp--test-adb-p) (tramp--test-gvfs-p)
       (tramp--test-sh-p) (tramp--test-sudoedit-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (tmp-name3 (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name1)
	    (should (file-exists-p tmp-name1))
	    (should (consp (file-attribute-modification-time
			    (file-attributes tmp-name1))))
	    ;; Skip the test, if the remote handler is not able to set
	    ;; the correct time.
	    ;; Some remote machines cannot resolve seconds.  So we use a minute.
	    (skip-unless (set-file-times tmp-name1 (seconds-to-time 60)))
	    ;; Dumb remote shells without perl(1) or stat(1) are not
	    ;; able to return the date correctly.  They say "don't know".
	    (unless (time-equal-p
		     (file-attribute-modification-time
		      (file-attributes tmp-name1))
		     tramp-time-dont-know)
	      (should
	       (time-equal-p
                (file-attribute-modification-time (file-attributes tmp-name1))
		(seconds-to-time 60)))
	      ;; Setting the time for not existing files shall fail.
	      (should-error
	       (set-file-times tmp-name2)
	       :type 'file-missing)
	      (write-region "bla" nil tmp-name2)
	      (should (file-exists-p tmp-name2))
	      (should (file-newer-than-file-p tmp-name2 tmp-name1))
	      ;; `tmp-name3' does not exist.
	      (should (file-newer-than-file-p tmp-name2 tmp-name3))
	      (should-not (file-newer-than-file-p tmp-name3 tmp-name1))
	      ;; Check the NOFOLLOW arg.  For regular files, there
	      ;; shouldn't be a difference.
	      (set-file-times tmp-name1 (seconds-to-time 60) 'nofollow)
	      (should
	       (time-equal-p
                (file-attribute-modification-time (file-attributes tmp-name1))
		(seconds-to-time 60)))))

	;; Cleanup.
	(ignore-errors
	  (delete-file tmp-name1)
	  (delete-file tmp-name2))))))

(ert-deftest tramp-test23-visited-file-modtime ()
  "Check `set-visited-file-modtime' and `verify-visited-file-modtime'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (with-temp-buffer
	      (insert-file-contents tmp-name)
	      (should (verify-visited-file-modtime))
              (set-visited-file-modtime (seconds-to-time 1))
	      (should (verify-visited-file-modtime))
	      (should (= 1 (float-time (visited-file-modtime))))

	      ;; Checks with deleted file.
	      (delete-file tmp-name)
	      (dired-uncache tmp-name)
	      (should (verify-visited-file-modtime))
              (set-visited-file-modtime (seconds-to-time 1))
	      (should (verify-visited-file-modtime))
	      (should (= 1 (float-time (visited-file-modtime))))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name))))))

;; This test is inspired by Bug#29149.
(ert-deftest tramp-test24-file-acl ()
  "Check that `file-acl' and `set-file-acl' work proper."
  (skip-unless (tramp--test-enabled))
  ;; The following test checks also whether `set-file-modes' will work.
  (skip-unless (file-acl ert-remote-temporary-file-directory))
  (skip-unless (not (tramp--test-crypt-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
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
	    (unless (tramp--test-windows-nt-or-smb-p)
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
   (not (equal (file-selinux-context ert-remote-temporary-file-directory)
	       '(nil nil nil nil))))
  (skip-unless (not (tramp--test-crypt-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
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
	    (copy-file tmp-name1 tmp-name2 nil nil nil 'preserve-permissions)
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
			      ert-remote-temporary-file-directory))
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

  ;; Method and host name in completion mode.
  (let ((tramp-fuse-remove-hidden-files t)
	(method (file-remote-p ert-remote-temporary-file-directory 'method))
	(host (file-remote-p ert-remote-temporary-file-directory 'host))
        (orig-syntax tramp-syntax)
        (minibuffer-completing-file-name t))
    (when (and (stringp host)
	       (string-match
		(rx (regexp tramp-prefix-port-regexp) (regexp tramp-port-regexp))
		host))
      (setq host (replace-match "" nil nil host)))

    (unwind-protect
        (dolist (syntax (if (tramp--test-expensive-test-p)
		            (tramp-syntax-values) `(,orig-syntax)))
          (tramp-change-syntax syntax)
	  ;; This has cleaned up all connection data, which are used
	  ;; for completion.  We must refill the cache.
	  (tramp-set-connection-property tramp-test-vec "property" nil)

          (let (;; This is needed for the `separate' syntax.
                (prefix-format (substring tramp-prefix-format 1))
		;; This is needed for the IPv6 host name syntax.
		(ipv6-prefix
		 (and (string-match-p tramp-ipv6-regexp host)
		      tramp-prefix-ipv6-format))
		(ipv6-postfix
		 (and (string-match-p tramp-ipv6-regexp host)
		      tramp-postfix-ipv6-format)))
            ;; Complete method name.
	    (unless (or (tramp-string-empty-or-nil-p method)
                        (string-empty-p tramp-method-regexp))
	      (should
	       (member
		(concat prefix-format method tramp-postfix-method-format)
		(file-name-all-completions
                 (concat prefix-format (substring method 0 1)) "/"))))
            ;; Complete host name.
	    (unless (or (tramp-string-empty-or-nil-p method)
                        (string-empty-p tramp-method-regexp)
                        (tramp-string-empty-or-nil-p host))
	      (should
	       (member
		(concat
                 prefix-format method tramp-postfix-method-format
		 ipv6-prefix host ipv6-postfix tramp-postfix-host-format)
		(file-name-all-completions
		 (concat prefix-format method tramp-postfix-method-format)
                 "/"))))))

      ;; Cleanup.
      (tramp-change-syntax orig-syntax)))

  (dolist (non-essential '(nil t))
    (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
      (let ((tramp-fuse-remove-hidden-files t)
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
	      ;; `file-name-completion' should not err out if
	      ;; directory does not exist.  (Bug#61890)
	      ;; Ange-FTP does not support this.
	      (unless (tramp--test-ange-ftp-p)
		(should-not
		 (file-name-completion "a" (file-name-concat tmp-name "fuzz"))))
	      ;; Ange-FTP does not support predicates.
	      (unless (tramp--test-ange-ftp-p)
		(should
		 (equal
		  (file-name-completion "b" tmp-name #'file-directory-p)
		  "boz/")))
	      (should
	       (equal (file-name-all-completions "fo" tmp-name) '("foo")))
	      (should
	       (equal
		(sort (file-name-all-completions "b" tmp-name) #'string-lessp)
		'("bold" "boz/")))
	      (should-not (file-name-all-completions "a" tmp-name))
	      ;; `completion-regexp-list' restricts the completion to
	      ;; files which match all expressions in this list.
	      ;; Ange-FTP does not complete "".
	      (unless (tramp--test-ange-ftp-p)
		(let ((completion-regexp-list
		       `(,directory-files-no-dot-files-regexp "b")))
		  (should
		   (equal (file-name-completion "" tmp-name) "bo"))
		  (should
		   (equal
		    (sort
		     (file-name-all-completions "" tmp-name) #'string-lessp)
		    '("bold" "boz/")))))
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
		  (sort (file-name-all-completions "" tmp-name) #'string-lessp)
		  '("../" "./" "bold" "boz/" "foo" "foo.ext")))))

	  ;; Cleanup.
	  (ignore-errors (delete-directory tmp-name 'recursive)))))))

(tramp--test-deftest-with-perl tramp-test26-file-name-completion)

(tramp--test-deftest-with-ls tramp-test26-file-name-completion)

;; This test is inspired by Bug#51386, Bug#52758, Bug#53513, Bug#54042
;; and Bug#60505.
(ert-deftest tramp-test26-interactive-file-name-completion ()
  "Check interactive completion with different `completion-styles'."
  ;; Method, user and host name in completion mode.
  (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)

  (let ((method (file-remote-p ert-remote-temporary-file-directory 'method))
	(user (file-remote-p ert-remote-temporary-file-directory 'user))
	(host (file-remote-p ert-remote-temporary-file-directory 'host))
	(hop (file-remote-p ert-remote-temporary-file-directory 'hop))
        (orig-syntax tramp-syntax)
        (non-essential t)
	(inhibit-message t))
    (when (and (stringp host)
	       (string-match
		(rx (regexp tramp-prefix-port-regexp) (regexp tramp-port-regexp))
		host))
      (setq host (replace-match "" nil nil host)))

    ;; (trace-function #'tramp-completion-file-name-handler)
    ;; (trace-function #'completion-file-name-table)
    (unwind-protect
        (dolist (syntax (if (tramp--test-expensive-test-p)
		            (tramp-syntax-values) `(,orig-syntax)))
          (tramp-change-syntax syntax)
	  ;; This has cleaned up all connection data, which are used
	  ;; for completion.  We must refill the cache.
	  (tramp-set-connection-property tramp-test-vec "property" nil)

          (dolist
              (style
               (if (tramp--test-expensive-test-p)
                   ;; It doesn't work for `initials' and `shorthand'
                   ;; completion styles.  Should it?
		   ;; `orderless' passes the tests, but it is an ELPA package.
                   '(emacs21 emacs22 basic partial-completion substring flex)
		 '(basic)))

	    (when (assoc style completion-styles-alist)
	      (let* (;; Force the real minibuffer in batch mode.
                     (executing-kbd-macro noninteractive)
                     (completion-styles `(,style))
                     completion-category-defaults
                     completion-category-overrides
                     ;; This is needed for the `simplified' syntax,
                     (tramp-default-method method)
                     (method-string
		      (unless (string-empty-p tramp-method-regexp)
			(concat method tramp-postfix-method-format)))
		     (user-string
		      (unless (tramp-string-empty-or-nil-p user)
			(concat user tramp-postfix-user-format)))
		     ;; This is needed for the IPv6 host name syntax.
		     (ipv6-prefix
		      (and (string-match-p tramp-ipv6-regexp host)
		           tramp-prefix-ipv6-format))
		     (ipv6-postfix
		      (and (string-match-p tramp-ipv6-regexp host)
		           tramp-postfix-ipv6-format))
		     (host-string
		      (unless (tramp-string-empty-or-nil-p host)
			(concat
			 ipv6-prefix host
			 ipv6-postfix tramp-postfix-host-format)))
		     ;; The hop string fits only the initial syntax.
		     (hop (and (eq tramp-syntax orig-syntax) hop))
                     test result completions)

		(dolist
		    (test-and-result
		     ;; These are triples of strings (TEST-STRING
		     ;; RESULT-CHECK COMPLETION-CHECK).  RESULT-CHECK
		     ;; could be not unique, in this case it is a list
		     ;; (RESULT1 RESULT2 ...).
		     (append
		      ;; Complete method name.
		      (unless (string-empty-p tramp-method-regexp)
			`((,(concat
                             tramp-prefix-format hop
                             (substring-no-properties
			      method 0 (min 2 (length method))))
			   ,(concat tramp-prefix-format hop method-string)
			   ,method-string)))
		      ;; Complete user name.
		      (unless (tramp-string-empty-or-nil-p user)
			`((,(concat
                             tramp-prefix-format hop method-string
                             (substring-no-properties
			      user 0 (min 2 (length user))))
			   ,(concat
                             tramp-prefix-format hop method-string user-string)
			   ,user-string)))
		      ;; Complete host name.
		      (unless (tramp-string-empty-or-nil-p host)
			`((,(concat
                             tramp-prefix-format hop method-string
			     ipv6-prefix
			     (substring-no-properties
			      host 0 (min 2 (length host))))
			   (,(concat
			      tramp-prefix-format hop method-string host-string)
			    ,(concat
			      tramp-prefix-format hop method-string
			      user-string host-string))
			   ,host-string)))
		      ;; Complete user and host name.
		      (unless (or (tramp-string-empty-or-nil-p user)
				  (tramp-string-empty-or-nil-p host))
			`((,(concat
                             tramp-prefix-format hop method-string user-string
			     ipv6-prefix
			     (substring-no-properties
			      host 0 (min 2 (length host))))
			   ,(concat
                             tramp-prefix-format hop method-string
	                     user-string host-string)
			   ,host-string)))))

                  (ignore-errors (kill-buffer "*Completions*"))
                  ;; (and (bufferp trace-buffer) (kill-buffer trace-buffer))
                  (discard-input)
                  (setq test (car test-and-result)
                        unread-command-events
                        (mapcar #'identity (concat test "\t\t\n"))
                        completions nil
                        result (read-file-name "Prompt: "))

                  (if (or (not (get-buffer "*Completions*"))
			  (string-match-p
			   (if (string-empty-p tramp-method-regexp)
			       (rx
				(| (regexp tramp-postfix-user-regexp)
				   (regexp tramp-postfix-host-regexp))
				eos)
			     (rx
			      (| (regexp tramp-postfix-method-regexp)
				 (regexp tramp-postfix-user-regexp)
				 (regexp tramp-postfix-host-regexp))
			      eos))
			   result))
		      (progn
                        ;; (tramp--test-message
                        ;;  "syntax: %s style: %s test: %s result: %s"
                        ;;  syntax style test result)
			(if (stringp (cadr test-and-result))
			    (should
			     (string-prefix-p (cadr test-and-result) result))
			  (should
			   (let (res)
			     (dolist (elem (cadr test-and-result) res)
			       (setq
				res (or res (string-prefix-p elem result))))))))

                    (with-current-buffer "*Completions*"
		      ;; We must remove leading `default-directory'.
		      (goto-char (point-min))
		      (let ((inhibit-read-only t))
			(while (search-forward-regexp "//" nil 'noerror)
			  (delete-region (line-beginning-position) (point))))
		      (goto-char (point-min))
		      (search-forward-regexp
		       (rx bol (0+ nonl)
			   (any "Pp") "ossible completions"
			   (0+ nonl) eol))
		      (forward-line 1)
		      (setq completions
                            (split-string
                             (buffer-substring-no-properties (point) (point-max))
                             (rx (any "\r\n\t ")) 'omit)))

                    ;; (tramp--test-message
                    ;;  "syntax: %s style: %s test: %s result: %s completions: %S"
                    ;;  syntax style test result completions)
                    (should (member (caddr test-and-result) completions))))))))

      ;; Cleanup.
      ;; (tramp--test-message "%s" (tramp-get-buffer-string trace-buffer))
      ;; (untrace-function #'tramp-completion-file-name-handler)
      ;; (untrace-function #'completion-file-name-table)
      (tramp-change-syntax orig-syntax))))

(ert-deftest tramp-test27-load ()
  "Check `load'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted)))
      (unwind-protect
	  (progn
	    ;; Ange-FTP does not tolerate a missing file, even with `noerror'.
	    (unless (tramp--test-ange-ftp-p)
	      (load tmp-name 'noerror 'nomessage))
	    (should-not (featurep 'tramp-test-load))
	    (write-region "(provide 'tramp-test-load)" nil tmp-name)
	    ;; `load' in lread.c passes `must-suffix' since Emacs 29.
	    ;; In Ange-FTP, `must-suffix' is ignored.
	    (when (and (tramp--test-emacs29-p)
                       (not (tramp--test-ange-ftp-p)))
	      (should-error
	       (load tmp-name nil 'nomessage 'nosuffix 'must-suffix)
	       :type 'file-error))
	    (load tmp-name nil 'nomessage 'nosuffix)
	    (should (featurep 'tramp-test-load)))

	;; Cleanup.
	(ignore-errors
	  (and (featurep 'tramp-test-load) (unload-feature 'tramp-test-load))
	  (delete-file tmp-name))))))

(defun tramp--test-shell-file-name ()
  "Return default remote shell."
  (let ((default-directory ert-remote-temporary-file-directory))
    (tramp-compat-connection-local-value shell-file-name)))

(defun tramp--test-shell-command-switch ()
  "Return default remote shell command switch."
  (let ((default-directory ert-remote-temporary-file-directory))
    (tramp-compat-connection-local-value shell-command-switch)))

(ert-deftest tramp-test28-process-file ()
  "Check `process-file'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let* ((tmp-name (tramp--test-make-temp-name nil quoted))
	   (fnnd (file-name-nondirectory tmp-name))
	   (default-directory ert-remote-temporary-file-directory)
	   (buffer (get-buffer-create "*tramp-tests*"))
	   kill-buffer-query-functions)
      (unwind-protect
	  (progn
	    ;; We cannot use "/bin/true" and "/bin/false"; those paths
	    ;; do not exist on hydra and on MS Windows.
	    (should (zerop (process-file "true")))
	    (should-not (zerop (process-file "false")))
	    (should-not (zerop (process-file "binary-does-not-exist")))
	    ;; Return exit code.
	    (should (= 42 (process-file
			   (tramp--test-shell-file-name) nil nil nil
			   (tramp--test-shell-command-switch) "exit 42")))
	    ;; Return exit code in case the process is interrupted,
	    ;; and there's no indication for a signal describing string.
	    (unless (tramp--test-sshfs-p)
	      (let (process-file-return-signal-string)
		(should
		 (= (+ 128 2)
		    (process-file
		     (tramp--test-shell-file-name) nil nil nil
		     (tramp--test-shell-command-switch) "kill -2 $$")))))
	    ;; Return string in case the process is interrupted and
	    ;; there's an indication for a signal describing string.
	    (unless (tramp--test-sshfs-p)
	      (let ((process-file-return-signal-string t))
		(should
		 (string-match-p
		  (rx (| "Interrupt" "Signal 2"))
		  (process-file
		   (tramp--test-shell-file-name) nil nil nil
		   (tramp--test-shell-command-switch) "kill -2 $$")))))

	    ;; Check DESTINATION.
	    (dolist (destination `(nil t ,buffer))
	      (when (bufferp destination)
		(with-current-buffer destination
		  (delete-region (point-min) (point-max))))
	      (with-temp-buffer
		(write-region "foo" nil tmp-name)
		(should (file-exists-p tmp-name))
		(should (zerop (process-file "ls" nil destination nil fnnd)))
		(with-current-buffer
		    (if (bufferp destination) destination (current-buffer))
		  ;; "ls" could produce colorized output.
		  (goto-char (point-min))
		  (while (search-forward-regexp
			  ansi-color-control-seq-regexp nil t)
		    (replace-match ""))
		  (should
		   (string-equal (if destination (format "%s\n" fnnd) "")
				 (buffer-string)))
		  (should-not (get-buffer-window (current-buffer) t))
		  (goto-char (point-max)))

		;; Second run.  The output must be appended.
		(should (zerop (process-file "ls" nil destination t fnnd)))
		(with-current-buffer
		    (if (bufferp destination) destination (current-buffer))
		  ;; "ls" could produce colorized output.
		  (goto-char (point-min))
		  (while (search-forward-regexp
			  ansi-color-control-seq-regexp nil t)
		    (replace-match ""))
		  (should
		   (string-equal
		    (if destination (format "%s\n%s\n" fnnd fnnd) "")
		    (buffer-string))))

		(unless (eq destination t)
		  (should (string-empty-p (buffer-string))))
		;; A non-nil DISPLAY must not raise the buffer.
		(should-not (get-buffer-window (current-buffer) t))
		(delete-file tmp-name)))

	    ;; Check remote and local INFILE.
	    (dolist (local '(nil t))
	      (with-temp-buffer
		(setq tmp-name (tramp--test-make-temp-name local quoted))
		(write-region "foo" nil tmp-name)
		(should (file-exists-p tmp-name))
		(should (zerop (process-file "cat" tmp-name t)))
		(should (string-equal "foo" (buffer-string)))
		(should-not (get-buffer-window (current-buffer) t))
		(delete-file tmp-name)))

	    ;; Check remote and local DESTINATION file.  This isn't
	    ;; implemented yet in all file name handler backends.
	    ;; (dolist (local '(nil t))
	    ;;   (setq tmp-name (tramp--test-make-temp-name local quoted))
	    ;;   (should
	    ;;    (zerop (process-file "echo" nil `(:file ,tmp-name) nil "foo")))
	    ;;   (with-temp-buffer
	    ;; 	(insert-file-contents tmp-name)
	    ;; 	(should (string-equal "foo" (buffer-string)))
	    ;; 	(should-not (get-buffer-window (current-buffer) t))
	    ;; 	(delete-file tmp-name)))

	    ;; Check remote and local STDERR.
	    (dolist (local '(nil t))
	      (setq tmp-name (tramp--test-make-temp-name local quoted))
	      (should-not
	       (zerop
		(process-file "cat" nil `(t ,tmp-name) nil "/does-not-exist")))
	      (with-temp-buffer
		(insert-file-contents tmp-name)
		(should
		 (string-match-p
		  (rx "cat:" (* nonl) " No such file or directory")
		  (buffer-string)))
		(should-not (get-buffer-window (current-buffer) t))
		(delete-file tmp-name))))

	;; Cleanup.
	(ignore-errors (kill-buffer buffer))
	(ignore-errors (delete-file tmp-name))))))

;; Must be a command, because used as `sigusr1' handler.
(defun tramp--test-timeout-handler (&rest _ignore)
  "Timeout handler, reporting a failed test."
  (interactive)
  (tramp--test-message "proc: %s" (get-buffer-process (current-buffer)))
  (when-let* ((proc (get-buffer-process (current-buffer)))
	      ((processp proc)))
    (tramp--test-message "cmd: %s" (process-command proc)))
  (tramp--test-message "buf: %s\n%s\n---" (current-buffer) (buffer-string))
  (ert-fail (format "`%s' timed out" (ert-test-name (ert-running-test)))))

(ert-deftest tramp-test29-start-file-process ()
  "Check `start-file-process'."
  :tags '(:expensive-test :tramp-asynchronous-processes)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((default-directory ert-remote-temporary-file-directory)
	  (tmp-name (tramp--test-make-temp-name nil quoted))
	  kill-buffer-query-functions command proc)

      ;; Simple process.
      (unwind-protect
	  (with-temp-buffer
	    (setq command '("cat")
		  proc
		  (apply #'start-file-process "test1" (current-buffer) command))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (should (equal (process-get proc 'remote-command) command))
	    (process-send-string proc "foo\n")
	    (process-send-eof proc)
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(while (accept-process-output proc 0 nil t))))
	    (should (string-match-p "foo" (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-process proc)))

      ;; Simple process using a file.
      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (setq command `("cat" ,(file-name-nondirectory tmp-name))
		  proc
		  (apply #'start-file-process "test2" (current-buffer) command))
	    (should (processp proc))
	    (should (equal (process-get proc 'remote-command) command))
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(while (accept-process-output proc 0 nil t))))
	    (should (string-match-p "foo" (buffer-string))))

	;; Cleanup.
	(ignore-errors
	  (delete-process proc)
	  (delete-file tmp-name)))

      ;; Process filter.
      (unwind-protect
	  (with-temp-buffer
	    (setq command '("cat")
		  proc
		  (apply #'start-file-process "test3" (current-buffer) command))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (should (equal (process-get proc 'remote-command) command))
	    (set-process-filter
	     proc
	     (lambda (p s) (with-current-buffer (process-buffer p) (insert s))))
	    (process-send-string proc "foo\n")
	    (process-send-eof proc)
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(while (accept-process-output proc 0 nil t))))
	    (should (string-match-p "foo" (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-process proc)))

      ;; Disabled process filter.  It doesn't work reliable.
      (unless t
	(unwind-protect
	    (with-temp-buffer
	      (setq command '("cat")
		    proc
		    (apply
		     #'start-file-process "test4" (current-buffer) command))
	      (should (processp proc))
	      (should (equal (process-status proc) 'run))
	      (should (equal (process-get proc 'remote-command) command))
	      (set-process-filter proc t)
	      (process-send-string proc "foo\n")
	      (process-send-eof proc)
	      ;; Read output.  There shouldn't be any.
	      (with-timeout (10)
		(while (process-live-p proc)
		  (while (accept-process-output proc 0 nil t))))
	      ;; No output due to process filter.
	      (should (= (point-min) (point-max))))

	  ;; Cleanup.
	  (ignore-errors (delete-process proc))))

      ;; Process connection type.
      (when (and (tramp--test-sh-p)
		 (not (tramp-direct-async-process-p))
		 (executable-find "hexdump" 'remote))
	(dolist (process-connection-type '(nil pipe t pty))
	  (unwind-protect
	      (with-temp-buffer
		(setq command '("hexdump" "-v" "-e" "/1 \"%02X\n\"")
		      proc
		      (apply #'start-file-process
			     (format "test5-%s" process-connection-type)
			     (current-buffer) command))
		(should (processp proc))
		(should (equal (process-status proc) 'run))
		(should (equal (process-get proc 'remote-command) command))
		;; Give the pipe process a chance to start.
		(when (memq process-connection-type '(nil pipe))
		  (sit-for 0.1 'nodisp))
		(process-send-string proc "foo\r\n")
		(process-send-eof proc)
		;; Read output.  On macOS, there is always newline
                ;; conversion.  "telnet" converts \r to <CR><NUL> if
                ;; `crlf' flag is FALSE.  See telnet(1) man page.
		(let ((expected
		       (rx "66\n" "6F\n" "6F\n"
			   (| "0D\n" "0A\n") (? "00\n") "0A\n")))
		  (with-timeout (10 (tramp--test-timeout-handler))
		    (while (not (string-match-p expected (buffer-string)))
		      (while (accept-process-output proc 0 nil t))))
		  (should (string-match-p expected (buffer-string)))))

	    ;; Cleanup.
	    (ignore-errors (delete-process proc)))))

      ;; PTY.
      (unwind-protect
	  (with-temp-buffer
	    ;; It works only for tramp-sh.el, and not direct async processes.
	    (if (or (not (tramp--test-sh-p)) (tramp-direct-async-process-p))
		(should-error
		 (start-file-process "test6" (current-buffer) nil)
		 :type 'wrong-type-argument)

	      (setq proc (start-file-process "test6" (current-buffer) nil))
	      (should (processp proc))
	      (should (equal (process-status proc) 'run))
	      (should-not (process-get proc 'remote-command))
	      ;; On MS Windows, `process-tty-name' returns nil.
	      (unless (tramp--test-windows-nt-p)
		(should (stringp (process-tty-name proc))))))

	;; Cleanup.
	(ignore-errors (delete-process proc))))))

(defmacro tramp--test-deftest-direct-async-process (test &optional unstable)
  "Define ert test `TEST-direct-async' for direct async processes.
If UNSTABLE is non-nil, the test is tagged as `:unstable'."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-direct-async")) ()
     ;; This is the docstring.  However, it must be expanded to a
     ;; string inside the macro.  No idea.
     ;; (concat (ert-test-documentation (get ',test 'ert--test))
     ;;         "\nUse direct async process.")
     :tags (append '(:expensive-test :tramp-asynchronous-processes)
		   (and ,unstable '(:unstable)))
     (skip-unless (tramp--test-enabled))
     (let* ((default-directory ert-remote-temporary-file-directory)
	    (ert-test (ert-get-test ',test))
	    (connection-local-profile-alist
	     (cons
	      '(direct-async-process-profile (tramp-direct-async-process . t))
	      connection-local-profile-alist))
	    (connection-local-criteria-alist
	     (cons
	      `((:application tramp
		 :machine ,(file-remote-p default-directory 'host))
		direct-async-process-profile)
	      connection-local-criteria-alist)))
       (skip-unless (tramp-direct-async-process-p))
       (when-let* ((result (ert-test-most-recent-result ert-test)))
	 (skip-unless (< (ert-test-result-duration result) 300)))
       ;; We do expect an established connection already,
       ;; `file-truename' does it by side-effect.  Suppress
       ;; `tramp--test-enabled', in order to keep the connection.
       (cl-letf (((symbol-function #'tramp--test-enabled) #'always))
	 (file-truename ert-remote-temporary-file-directory)
	 (funcall (ert-test-body ert-test))))))

(tramp--test-deftest-direct-async-process tramp-test29-start-file-process)

(ert-deftest tramp-test30-make-process ()
  "Check `make-process'."
  :tags (append '(:expensive-test :tramp-asynchronous-processes)
		(and (getenv "EMACS_EMBA_CI")
                     '(:unstable)))
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((default-directory ert-remote-temporary-file-directory)
	  (tmp-name (tramp--test-make-temp-name nil quoted))
	  kill-buffer-query-functions command proc)
      (should-not (apply #'make-process nil)) ; Use `apply' to avoid warnings.

      ;; Simple process.
      (unwind-protect
	  (with-temp-buffer
	    (setq command '("cat")
		  proc
		  (make-process
		   :name "test1" :buffer (current-buffer) :command command
		   :file-handler t))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (should (equal (process-get proc 'remote-command) command))
	    (process-send-string proc "foo\n")
	    (process-send-eof proc)
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(while (accept-process-output proc 0 nil t))))
	    (should (string-match-p "foo" (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-process proc)))

      ;; Simple process using a file.
      (unwind-protect
	  (with-temp-buffer
	    (write-region "foo" nil tmp-name)
	    (should (file-exists-p tmp-name))
	    (setq command `("cat" ,(file-name-nondirectory tmp-name))
		  proc
		  (make-process
		   :name "test2" :buffer (current-buffer) :command command
		   :file-handler t))
	    (should (processp proc))
	    (should (equal (process-get proc 'remote-command) command))
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (< (- (point-max) (point-min)) (length "foo"))
		(while (accept-process-output proc 0 nil t))))
	    (should (string-match-p "foo" (buffer-string))))

	;; Cleanup.
	(ignore-errors
	  (delete-process proc)
	  (delete-file tmp-name)))

      ;; Process filter.
      (unwind-protect
	  (with-temp-buffer
	    (setq command '("cat")
		  proc
		  (make-process
		   :name "test3" :buffer (current-buffer) :command command
		   :filter
		   (lambda (p s)
		     (with-current-buffer (process-buffer p) (insert s)))
		   :file-handler t))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (should (equal (process-get proc 'remote-command) command))
	    (process-send-string proc "foo\n")
	    (process-send-eof proc)
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (not (string-match-p "foo" (buffer-string)))
		(while (accept-process-output proc 0 nil t))))
	    (should (string-match-p "foo" (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-process proc)))

      ;; Disabled process filter.  It doesn't work reliable.
      (unless t
	(unwind-protect
	    (with-temp-buffer
	      (setq command '("cat")
		    proc
		    (make-process
		     :name "test4" :buffer (current-buffer) :command command
		     :filter t :file-handler t))
	      (should (processp proc))
	      (should (equal (process-status proc) 'run))
	      (should (equal (process-get proc 'remote-command) command))
	      (process-send-string proc "foo\n")
	      (process-send-eof proc)
	      ;; Read output.  There shouldn't be any.
	      (with-timeout (10)
		(while (process-live-p proc)
		  (while (accept-process-output proc 0 nil t))))
	      ;; No output due to process filter.
	      (should (= (point-min) (point-max))))

	  ;; Cleanup.
	  (ignore-errors (delete-process proc))))

      ;; Process sentinel.
      (unwind-protect
	  (with-temp-buffer
	    (setq command '("cat")
		  proc
		  (make-process
		   :name "test5" :buffer (current-buffer) :command command
		   :sentinel
		   (lambda (p s)
		     (with-current-buffer (process-buffer p) (insert s)))
		   :file-handler t))
	    (should (processp proc))
	    (should (equal (process-status proc) 'run))
	    (should (equal (process-get proc 'remote-command) command))
	    (process-send-string proc "foo\n")
	    (process-send-eof proc)
	    (delete-process proc)
	    ;; Read output.
	    (with-timeout (10 (tramp--test-timeout-handler))
	      (while (accept-process-output proc 0 nil t)))
	    ;; On some MS Windows systems, it returns "unknown signal".
	    (should
	     (string-match-p
	      (rx (| "unknown signal" "killed")) (buffer-string))))

	;; Cleanup.
	(ignore-errors (delete-process proc)))

      ;; Process with stderr buffer.  "telnet" does not cooperate with
      ;; three processes.
      (unless (or (tramp--test-telnet-p) (tramp-direct-async-process-p))
	(let ((stderr (generate-new-buffer "*stderr*")))
	  (unwind-protect
	      (with-temp-buffer
		(setq command '("cat" "/does-not-exist")
		      proc
		      (make-process
		       :name "test6" :buffer (current-buffer) :command  command
		       :stderr stderr :file-handler t))
		(should (processp proc))
		(should (equal (process-get proc 'remote-command) command))
		;; Read output.
		(with-timeout (10 (tramp--test-timeout-handler))
		  (while (accept-process-output proc 0 nil t)))
		;; Read stderr.
		(with-current-buffer stderr
		  (with-timeout (10 (tramp--test-timeout-handler))
		    (while (not (string-match-p
				 "No such file or directory" (buffer-string)))
		      (while (accept-process-output
			      (get-buffer-process stderr) 0 nil t))))
		  (delete-process proc)
		  (should
		   (string-match-p
		    (rx "cat:" (* nonl) " No such file or directory")
		    (buffer-string)))))

	    ;; Cleanup.
	    (ignore-errors (delete-process proc))
	    (ignore-errors (kill-buffer stderr)))))

      ;; Process with stderr file.
      (unless (tramp-direct-async-process-p)
	(unwind-protect
	    (with-temp-buffer
	      (setq command '("cat" "/does-not-exist")
		    proc
		    (make-process
		     :name "test7" :buffer (current-buffer) :command command
		     :stderr tmp-name :file-handler t))
	      (should (processp proc))
	      (should (equal (process-get proc 'remote-command) command))
	      ;; Read stderr.
	      (with-timeout (10 (tramp--test-timeout-handler))
		(while (accept-process-output proc nil nil t)))
	      (delete-process proc)
	      (with-temp-buffer
		(insert-file-contents tmp-name)
		(should
		 (string-match-p
		  (rx "cat:" (* nonl) " No such file or directory")
		  (buffer-string)))))

	  ;; Cleanup.
	  (ignore-errors (delete-process proc))
	  (ignore-errors (delete-file tmp-name))))

      ;; Process connection type.
      (when (and (tramp--test-sh-p)
		 (not (tramp-direct-async-process-p))
		 (executable-find "hexdump" 'remote))
	(dolist (connection-type '(nil pipe t pty))
	  ;; `process-connection-type' is taken when
	  ;; `:connection-type' is nil.
	  (dolist (process-connection-type
		   (if connection-type '(nil pipe t pty) '(nil)))
	    (unwind-protect
		(with-temp-buffer
		  (setq command '("hexdump" "-v" "-e" "/1 \"%02X\n\"")
			proc
			(make-process
			 :name
			 (format "test8-%s-%s"
				 connection-type process-connection-type)
			 :buffer (current-buffer)
			 :connection-type connection-type
			 :command command
			 :file-handler t))
		  (should (processp proc))
		  (should (equal (process-status proc) 'run))
		  (should (equal (process-get proc 'remote-command) command))
		  ;; Give the pipe process a chance to start.
		  (when (or (eq connection-type 'pipe)
			    (memq process-connection-type '(nil pipe)))
		    (sit-for 0.1 'nodisp))
		  (process-send-string proc "foo\r\n")
		  (process-send-eof proc)
		  ;; Read output.  On macOS, there is always newline
                  ;; conversion.  "telnet" converts \r to <CR><NUL> if
                  ;; `crlf' flag is FALSE.  See telnet(1) man page.
		  (let ((expected
			 (rx "66\n" "6F\n" "6F\n"
			     (| "0D\n" "0A\n") (? "00\n") "0A\n")))
		    (with-timeout (10 (tramp--test-timeout-handler))
		      (while (not (string-match-p expected (buffer-string)))
			(while (accept-process-output proc 0 nil t))))
		    (should (string-match-p expected (buffer-string)))))

	      ;; Cleanup.
	      (ignore-errors (delete-process proc)))))))))

(tramp--test-deftest-direct-async-process tramp-test30-make-process)

(ert-deftest tramp-test31-interrupt-process ()
  "Check `interrupt-process'."
  ;; The final `process-live-p' check does not run sufficiently.
  :tags '(:expensive-test :tramp-asynchronous-processes :unstable)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-windows-nt-p)))
  (skip-unless (not (tramp--test-crypt-p)))

  ;; We must use `file-truename' for the temporary directory, in
  ;; order to establish the connection prior running an asynchronous
  ;; process.
  (let ((default-directory (file-truename ert-remote-temporary-file-directory))
	(delete-exited-processes t)
	kill-buffer-query-functions command proc)
    (unwind-protect
	(with-temp-buffer
	  (setq command "trap 'echo boom; exit 1' 2; sleep 100"
		proc (start-file-process-shell-command
		      "test" (current-buffer) command))
	  (should (processp proc))
	  (should (process-live-p proc))
	  (should (equal (process-status proc) 'run))
	  (should (numberp (process-get proc 'remote-pid)))
	  (should (equal (process-get proc 'remote-command)
			 (with-connection-local-variables
			  `(,shell-file-name ,shell-command-switch ,command))))
	  (should (interrupt-process proc))
	  ;; Let the process accept the interrupt.
	  (with-timeout (10 (tramp--test-timeout-handler))
	    (while (process-live-p proc)
	      (while (accept-process-output proc 0 nil t))))
	  (should-not (process-live-p proc))
	  ;; An interrupted process cannot be interrupted, again.
	  (should-error
	   (interrupt-process proc)
	   :type 'error))

      ;; Cleanup.
      (ignore-errors (delete-process proc)))))

(ert-deftest tramp-test31-signal-process ()
  "Check `signal-process'."
  ;; The final `process-live-p' check does not run sufficiently.
  :tags '(:expensive-test :tramp-asynchronous-processes :unstable)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-windows-nt-p)))
  (skip-unless (not (tramp--test-crypt-p)))
  ;; Since Emacs 29.1.
  (skip-unless (boundp 'signal-process-functions))

  ;; We must use `file-truename' for the temporary directory, in
  ;; order to establish the connection prior running an asynchronous
  ;; process.
  (let ((default-directory (file-truename ert-remote-temporary-file-directory))
	(delete-exited-processes t)
	kill-buffer-query-functions command proc)

    ;; If PROCESS is a string, it must be a process name or a process
    ;; number.  Check error handling.
    (should-error
     (signal-process (md5 (current-time-string)) 0)
     :type 'wrong-type-argument)

    ;; The PROCESS argument of `signal-process' can be a string.  Test
    ;; this as well.
    (dolist
	(func '(identity
		(lambda (x) (format "%s" (if (processp x) (process-name x) x)))))
      (dolist (sigcode '(2 INT))
	(unwind-protect
	    (with-temp-buffer
	      (setq command "trap 'echo boom; exit 1' 2; sleep 100"
		    proc (start-file-process-shell-command
		          (format "test1-%s" sigcode) (current-buffer) command))
	      (should (processp proc))
	      (should (process-live-p proc))
	      (should (equal (process-status proc) 'run))
	      (should (numberp (process-get proc 'remote-pid)))
	      (should
	       (equal (process-get proc 'remote-command)
		      (with-connection-local-variables
		       `(,shell-file-name ,shell-command-switch ,command))))
	      (should (zerop (signal-process (funcall func proc) sigcode)))
	      ;; Let the process accept the signal.
	      (with-timeout (10 (tramp--test-timeout-handler))
		(while (accept-process-output proc 0 nil t)))
              (should-not (process-live-p proc)))

          ;; Cleanup.
          (ignore-errors (kill-process proc))
          (ignore-errors (delete-process proc)))

	(unwind-protect
	    (with-temp-buffer
	      (setq command "trap 'echo boom; exit 1' 2; sleep 100"
		    proc (start-file-process-shell-command
		          (format "test2-%s" sigcode) (current-buffer) command))
	      (should (processp proc))
	      (should (process-live-p proc))
	      (should (equal (process-status proc) 'run))
	      (should (numberp (process-get proc 'remote-pid)))
	      (should
	       (equal (process-get proc 'remote-command)
		      (with-connection-local-variables
		       `(,shell-file-name ,shell-command-switch ,command))))
	      ;; `signal-process' has argument REMOTE since Emacs 29.
	      (with-no-warnings
		(should
		 (zerop
		  (signal-process
		   (funcall func (process-get proc 'remote-pid))
		   sigcode default-directory))))
	      ;; Let the process accept the signal.
	      (with-timeout (10 (tramp--test-timeout-handler))
		(while (accept-process-output proc 0 nil t)))
              (should-not (process-live-p proc)))

          ;; Cleanup.
          (ignore-errors (kill-process proc))
          (ignore-errors (delete-process proc)))))))

(ert-deftest tramp-test31-list-system-processes ()
  "Check `list-system-processes'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))
  ;; `list-system-processes' is supported since Emacs 29.1.
  (skip-unless (tramp--test-emacs29-p))

  (let ((default-directory ert-remote-temporary-file-directory))
    (skip-unless (consp (list-system-processes)))
    (should (not (equal (list-system-processes)
			(let ((default-directory temporary-file-directory))
			  (list-system-processes)))))))

(ert-deftest tramp-test31-process-attributes ()
  "Check `process-attributes'."
  :tags '(:expensive-test :tramp-asynchronous-processes)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))
  ;; `process-attributes' is supported since Emacs 29.1.
  (skip-unless (tramp--test-emacs29-p))

  ;; We must use `file-truename' for the temporary directory, in
  ;; order to establish the connection prior running an asynchronous
  ;; process.
  (let ((default-directory (file-truename ert-remote-temporary-file-directory))
	(delete-exited-processes t)
	kill-buffer-query-functions command proc)
    (skip-unless (consp (list-system-processes)))

    (unwind-protect
	(progn
	  (setq command '("sleep" "100")
		proc (apply #'start-file-process "test" nil command))
	  (while (accept-process-output proc 0))
	  (when-let* ((pid (process-get proc 'remote-pid))
		      (attributes (process-attributes pid)))
	    ;; (tramp--test-message "%s" attributes)
	    (should (equal (cdr (assq 'comm attributes)) (car command)))
	    (should (equal (cdr (assq 'args attributes))
			   (string-join command " ")))))

      ;; Cleanup.
      (ignore-errors (delete-process proc)))))

(ert-deftest tramp-test31-memory-info ()
  "Check `memory-info'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))
  ;; `memory-info' is supported since Emacs 29.1.
  (skip-unless (tramp--test-emacs29-p))

  (when-let* ((default-directory ert-remote-temporary-file-directory)
              (mi (memory-info)))
    (should (consp mi))
    (should (length= mi 4))
    (dotimes (i (length mi))
      (should (natnump (nth i mi))))))

(defun tramp--test-async-shell-command
    (command output-buffer &optional error-buffer input)
  "Like `async-shell-command', reading the output.
INPUT, if non-nil, is a string sent to the process."
  (let ((proc (async-shell-command command output-buffer error-buffer))
	(delete-exited-processes t))
    (should (equal (process-get proc 'remote-command)
		   (with-connection-local-variables
		    `(,shell-file-name ,shell-command-switch ,command))))
    (cl-letf (((symbol-function #'shell-command-sentinel) #'ignore))
      (when (stringp input)
	(process-send-string proc input))
      (with-timeout
	  ((if (getenv "EMACS_EMBA_CI") 30 10) (tramp--test-timeout-handler))
	(while
	    (or (accept-process-output proc nil nil t) (process-live-p proc))))
      (accept-process-output proc nil nil t))))

(defun tramp--test-shell-command-to-string-asynchronously (command)
  "Like `shell-command-to-string', but for asynchronous processes."
  (with-temp-buffer
    (tramp--test-async-shell-command command (current-buffer))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest tramp-test32-shell-command ()
  "Check `shell-command'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name (tramp--test-make-temp-name nil quoted))
	  (default-directory ert-remote-temporary-file-directory)
	  ;; Suppress nasty messages.
	  (inhibit-message t)
	  kill-buffer-query-functions)

      (dolist (this-shell-command
	       (append
		;; Synchronously.
		'(shell-command)
		;; Asynchronously.
		(and (tramp--test-asynchronous-processes-p)
		     '(tramp--test-async-shell-command))))

	;; Test ordinary `{async-}shell-command'.
	(unwind-protect
	    (with-temp-buffer
	      (write-region "foo" nil tmp-name)
	      (should (file-exists-p tmp-name))
	      (funcall
	       this-shell-command
	       (format "ls %s" (file-name-nondirectory tmp-name))
	       (current-buffer))
	      ;; "ls" could produce colorized output.
	      (goto-char (point-min))
	      (while (search-forward-regexp ansi-color-control-seq-regexp nil t)
		(replace-match ""))
	      (should
	       (string-equal
		(format "%s\n" (file-name-nondirectory tmp-name))
		(buffer-string))))

	  ;; Cleanup.
	  (ignore-errors (delete-file tmp-name)))

	;; Test `{async-}shell-command' with error buffer.
	(unless (tramp-direct-async-process-p)
	  (let ((stderr (generate-new-buffer "*stderr*")))
	    (unwind-protect
		(with-temp-buffer
		  (funcall
		   this-shell-command
		   "echo foo >&2; echo bar" (current-buffer) stderr)
		  (should (string-equal "bar\n" (buffer-string)))
		  ;; Check stderr.
		  (should
		   (string-equal "foo\n" (tramp-get-buffer-string stderr))))

	      ;; Cleanup.
	      (ignore-errors (kill-buffer stderr))))))

      ;; Test sending string to `async-shell-command'.
      (when (tramp--test-asynchronous-processes-p)
	(unwind-protect
	    (with-temp-buffer
	      (write-region "foo" nil tmp-name)
	      (should (file-exists-p tmp-name))
	      (tramp--test-async-shell-command
	       "read line; ls $line" (current-buffer) nil
	       ;; String to be sent.
	       (format "%s\n" (file-name-nondirectory tmp-name)))
	      (should
	       (string-match-p
		;; Some shells echo, for example the "adb" or container methods.
		(rx
		 bos (** 1 2 (literal (file-name-nondirectory tmp-name)) "\n")
		 eos)
		(buffer-string))))

	  ;; Cleanup.
	  (ignore-errors (delete-file tmp-name))))))

  ;; Test `async-shell-command-width'.
  (when (and (tramp--test-asynchronous-processes-p) (tramp--test-sh-p))
    (let* (;; Since Fedora 41, this seems to be the upper limit.  Used
	   ;; to be 1024 before.
	   (async-shell-command-width 512)
	   (default-directory ert-remote-temporary-file-directory)
	   (cols (ignore-errors
		   (read (tramp--test-shell-command-to-string-asynchronously
			  "tput cols")))))
      (when (natnump cols)
	(should (= cols async-shell-command-width))))))

(tramp--test-deftest-direct-async-process tramp-test32-shell-command)

;; This test is inspired by Bug#39067.
(ert-deftest tramp-test32-shell-command-dont-erase-buffer ()
  "Check `shell-command-dont-erase-buffer'."
  ;; As long as Bug#40896 is not solved both in simple.el and Tramp,
  ;; this test cannot run properly.
  :tags '(:expensive-test :unstable)
  (skip-unless (tramp--test-enabled))
  (skip-unless nil)
  (skip-unless (tramp--test-supports-processes-p))

  ;; (message "   s-c-d-e-b current-buffer buffer-string point")
  ;; (message "===============================================")

  ;;    s-c-d-e-b current-buffer buffer-string point
  ;; ===============================================
  ;;          nil              t    foobazzbar     4 x
  ;;          nil            nil          bazz     5
  ;; -----------------------------------------------
  ;;        erase              t          bazz     1 x
  ;;        erase            nil          bazz     5
  ;; -----------------------------------------------
  ;; beg-last-out              t    foobazzbar     4 x
  ;; beg-last-out            nil    foobarbazz     7
  ;; -----------------------------------------------
  ;; end-last-out              t    foobazzbar     4
  ;; end-last-out            nil    foobazzbar    11
  ;; -----------------------------------------------
  ;;   save-point              t    foobazzbar     4 x
  ;;   save-point            nil    foobarbazz     4 x
  ;; -----------------------------------------------
  ;;       random              t    foobazzbar     4
  ;;       random            nil    foobazzbar    11
  ;; -----------------------------------------------

  (let (;; Suppress nasty messages.
	(inhibit-message t)
	buffer kill-buffer-query-functions)
    ;; We check both the local and remote case, in order to guarantee
    ;; that they behave similar.
    (dolist (default-directory
	      `(,temporary-file-directory ,ert-remote-temporary-file-directory))
      ;; These are the possible values of `shell-command-dont-erase-buffer'.
      ;; `random' is taken as non-nil value without special meaning.
      (dolist (shell-command-dont-erase-buffer
	       '(nil erase beg-last-out end-last-out save-point random))
	;; `shell-command' might work over the current buffer, or not.
	(dolist (current '(t nil))
	  (with-temp-buffer
	    ;; We insert the string "foobar" into an empty buffer.
	    ;; Point is set between "foo" and "bar".
	    (setq buffer (current-buffer))
	    (insert "foobar")
	    (goto-char (- (point) 3))
	    (should (string-equal "foobar" (buffer-string)))
	    (should (string-equal "foo" (buffer-substring (point-min) (point))))
	    (should (string-equal "bar" (buffer-substring (point) (point-max))))

	    ;; Apply `shell-command'.  It shall output the string
	    ;; "bazz".  Messages in the *Messages* buffer are
	    ;; suppressed.
            (let (message-log-max)
	      (if current
		  (shell-command "echo -n bazz" (current-buffer))
		(with-temp-buffer (shell-command "echo -n bazz" buffer))))

	  ;;   (message
	  ;;    "%12s %14s %13s %5d"
	  ;;    shell-command-dont-erase-buffer current (buffer-string) (point))))
	  ;; (message "-----------------------------------------------")))))

	    ;; Check result.
	    (cond
	     (current
	      ;; String is inserted at point, and point is preserved
	      ;; unless dictated otherwise.
	      (cond
	       ((null shell-command-dont-erase-buffer)
		(should (string-equal "foobazzbar" (buffer-string)))
		(should (= 4 (point))))
	       ((eq shell-command-dont-erase-buffer 'erase)
		(should (string-equal "bazz" (buffer-string)))
		(should (= 1 (point))))
	       ((eq shell-command-dont-erase-buffer 'beg-last-out)
		(should (string-equal "foobazzbar" (buffer-string)))
		(should (= 4 (point))))
	       ;; Bug#40896
	       ;; ((eq shell-command-dont-erase-buffer 'end-last-out)
	       ;; 	(should (string-equal "foobazzbar" (buffer-string)))
	       ;; 	(should (= 7 (point))))
	       ((eq shell-command-dont-erase-buffer 'save-point)
		(should (string-equal "foobazzbar" (buffer-string)))
		(should (= 4 (point))))
	       ;; Bug#40896
	       ;; ((eq shell-command-dont-erase-buffer 'random)
	       ;; 	(should (string-equal "foobazzbar" (buffer-string)))
	       ;; 	(should (= 7 (point))))))
	       ))

	     (t ;; not current buffer
	      ;; String is appended, and point is at point-max unless
	      ;; dictated otherwise.
	      (cond
	       ((null shell-command-dont-erase-buffer)
		(should (string-equal "bazz" (buffer-string)))
		(should (= 5 (point))))
	       ((eq shell-command-dont-erase-buffer 'erase)
		(should (string-equal "bazz" (buffer-string)))
		(should (= 5 (point))))
	       ((eq shell-command-dont-erase-buffer 'beg-last-out)
		(should (string-equal "foobarbazz" (buffer-string)))
		(should (= 7 (point))))
	       ;; ;; Bug#40896
	       ;; ((eq shell-command-dont-erase-buffer 'end-last-out)
	       ;; 	(should (string-equal "foobarbazz" (buffer-string)))
	       ;; 	(should (= 11 (point))))
	       ((eq shell-command-dont-erase-buffer 'save-point)
		(should (string-equal "foobarbazz" (buffer-string)))
		(should (= 4 (point))))
	       ;; ;; Bug#40896
	       ;; ((eq shell-command-dont-erase-buffer 'random)
	       ;; 	(should (string-equal "foobarbazz" (buffer-string)))
	       ;; 	(should (= 11 (point)))))))))))))
	       )))))))))

;; This test is inspired by Bug#23952.
(ert-deftest tramp-test33-environment-variables ()
  "Check that remote processes set / unset environment variables properly."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-crypt-p)))

  (dolist (this-shell-command-to-string
	   (append
	    ;; Synchronously.
	    '(shell-command-to-string)
	    ;; Asynchronously.
	    (and (tramp--test-asynchronous-processes-p)
		 '(tramp--test-shell-command-to-string-asynchronously))))

    (let ((default-directory ert-remote-temporary-file-directory)
	  (shell-file-name "/bin/sh")
	  (envvar (concat "VAR_" (upcase (md5 (current-time-string)))))
	  kill-buffer-query-functions)

      ;; Check INSIDE_EMACS.
      (setenv "INSIDE_EMACS")
      (should
       (string-equal
	(format "%s,tramp:%s\n" emacs-version tramp-version)
	(funcall this-shell-command-to-string "echo \"${INSIDE_EMACS:-bla}\"")))
      (let ((process-environment
	     (cons (format "INSIDE_EMACS=%s,foo" emacs-version)
		   process-environment)))
	(should
	 (string-equal
	  (format "%s,foo,tramp:%s\n" emacs-version tramp-version)
	  (funcall
	   this-shell-command-to-string "echo \"${INSIDE_EMACS:-bla}\""))))

      ;; Set a value.
      (let ((process-environment
	     (cons (concat envvar "=foo") process-environment)))
	;; Default value.
	(should
	 (string-match-p
	  "foo"
	  (funcall
	   this-shell-command-to-string
	   (format "echo \"${%s:-bla}\"" envvar)))))

      ;; Set the empty value.
      (let ((process-environment
	     (cons (concat envvar "=") process-environment)))
	;; Value is null.
	(should
	 (string-match-p
	  "bla"
	  (funcall
	   this-shell-command-to-string (format "echo \"${%s:-bla}\"" envvar))))
	;; Variable is set.
	(should
	 (string-match-p
	  (rx (literal envvar)) (funcall this-shell-command-to-string "set"))))

      (unless (tramp-direct-async-process-p)
	;; We force a reconnect, in order to have a clean environment.
	(tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	;; Unset the variable.
	(let ((tramp-remote-process-environment
	       (cons (concat envvar "=foo") tramp-remote-process-environment)))
	  ;; Refill the cache; we don't want to run into timeouts.
	  (file-truename default-directory)
	  ;; Check the initial value, we want to unset below.
	  (should
	   (string-match-p
	    "foo"
	    (funcall
	     this-shell-command-to-string
	     (format "echo \"${%s:-bla}\"" envvar))))
	  (let ((process-environment (cons envvar process-environment)))
	    ;; Variable is unset.
	    (should
	     (string-match-p
	      "bla"
	      (funcall
	       this-shell-command-to-string
	       (format "echo \"${%s:-bla}\"" envvar))))
	    ;; Variable is unset.
	    (should-not
	     (string-match-p
	      (rx (literal envvar))
	      ;; We must remove PS1, the output is truncated otherwise.
	      ;; We must suppress "_=VAR...".
	      (funcall
	       this-shell-command-to-string
	       "printenv | grep -v PS1 | grep -v _=")))))))))

(tramp--test-deftest-direct-async-process tramp-test33-environment-variables)

;; This test is inspired by Bug#27009.
(ert-deftest tramp-test33-environment-variables-and-port-numbers ()
  "Check that two connections with separate ports are different."
  (skip-unless (tramp--test-enabled))
  ;; We test it only for the mock-up connection; otherwise there might
  ;; be problems with the used ports.
  (skip-unless (and (eq tramp-syntax 'default) (tramp--test-mock-p)))
  (skip-unless (not (tramp--test-crypt-p)))

  ;; We force a reconnect, in order to have a clean environment.
  (dolist (dir `(,ert-remote-temporary-file-directory
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
	   (string-match-p
	    (number-to-string port)
	    (shell-command-to-string (format "echo $%s" envvar))))))

    ;; Cleanup.
    (dolist (dir '("/mock:localhost#11111:" "/mock:localhost#22222:"))
      (tramp-cleanup-connection (tramp-dissect-file-name dir)))))

(ert-deftest tramp-test34-connection-local-variables ()
  "Check that connection-local variables are enabled."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))

  (let* ((default-directory ert-remote-temporary-file-directory)
	 (tmp-name1 (tramp--test-make-temp-name))
	 (tmp-name2 (expand-file-name "foo" tmp-name1))
	 (enable-local-variables :all)
	 (enable-remote-dir-locals t)
         (inhibit-message t)
	 kill-buffer-query-functions
	 (clpa connection-local-profile-alist)
	 (clca connection-local-criteria-alist))
    (unwind-protect
	(progn
	  (make-directory tmp-name1)
          (should (file-directory-p tmp-name1))

	  ;; `local-variable' is buffer-local due to explicit setting.
	  ;; We need `with-no-warnings', because `defvar-local' is not
	  ;; called at toplevel.
	  (with-no-warnings (defvar-local local-variable 'buffer))
	  (with-temp-buffer
	    (should (eq local-variable 'buffer)))

	  ;; `local-variable' is connection-local due to Tramp.
	  (write-region "foo" nil tmp-name2)
	  (should (file-exists-p tmp-name2))
	  (connection-local-set-profile-variables
	   'local-variable-profile
	   '((local-variable . connect)))
	  (connection-local-set-profiles
	   `(:application tramp
	     :protocol ,(file-remote-p default-directory 'method)
	     :user ,(file-remote-p default-directory 'user)
	     :machine ,(file-remote-p default-directory 'host))
	   'local-variable-profile)
	  (with-current-buffer (find-file-noselect tmp-name2)
	    (should (eq local-variable 'connect))
	    (kill-buffer (current-buffer)))

	  ;; `local-variable' is still connection-local due to Tramp.
	  ;; `find-file-hook' overrides dir-local settings.
	  (write-region
	   "((nil . ((local-variable . dir))))" nil
	   (expand-file-name ".dir-locals.el" tmp-name1))
	  (should (file-exists-p (expand-file-name ".dir-locals.el" tmp-name1)))
	  (when (memq #'tramp-set-connection-local-variables-for-buffer
		      find-file-hook)
	    (with-current-buffer (find-file-noselect tmp-name2)
	      (should (eq local-variable 'connect))
	      (kill-buffer (current-buffer))))
	  ;; `local-variable' is dir-local due to existence of .dir-locals.el.
	  (let ((find-file-hook
		 (remq #'tramp-set-connection-local-variables-for-buffer
		       find-file-hook)))
	    (with-current-buffer (find-file-noselect tmp-name2)
	      (should (eq local-variable 'dir))
	      (kill-buffer (current-buffer))))

	  ;; `local-variable' is still connection-local due to Tramp.
	  ;; `find-file-hook' overrides dir-local settings.
	  (write-region
	   "-*- mode: comint; local-variable: file; -*-" nil tmp-name2)
          (should (file-exists-p tmp-name2))
	  (when (memq #'tramp-set-connection-local-variables-for-buffer
		      find-file-hook)
	    (with-current-buffer (find-file-noselect tmp-name2)
	      (should (eq local-variable 'connect))
	      (kill-buffer (current-buffer))))
	  ;; `local-variable' is file-local due to specifying as file variable.
	  (let ((find-file-hook
		 (remq #'tramp-set-connection-local-variables-for-buffer
		       find-file-hook)))
	    (with-current-buffer (find-file-noselect tmp-name2)
	      (should (eq local-variable 'file))
	      (kill-buffer (current-buffer)))))

      ;; Cleanup.
      (custom-set-variables
       `(connection-local-profile-alist ',clpa now)
       `(connection-local-criteria-alist ',clca now))
      (ignore-errors (delete-directory tmp-name1 'recursive)))))

(ert-deftest tramp-test34-explicit-shell-file-name ()
  "Check that connection-local `explicit-shell-file-name' is set."
  :tags '(:expensive-test :tramp-asynchronous-processes)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))

  (let ((default-directory ert-remote-temporary-file-directory)
	explicit-shell-file-name kill-buffer-query-functions
	(clpa connection-local-profile-alist)
	(clca connection-local-criteria-alist))
    (unwind-protect
	(progn
	  (connection-local-set-profile-variables
	   'remote-sh
	   `((explicit-shell-file-name . ,(tramp--test-shell-file-name))
	     (explicit-sh-args
	      . (,(tramp--test-shell-command-switch) "echo foo"))))
	  (connection-local-set-profiles
	   `(:application tramp
	     :protocol ,(file-remote-p default-directory 'method)
	     :user ,(file-remote-p default-directory 'user)
	     :machine ,(file-remote-p default-directory 'host))
	   'remote-sh)
	  (put 'explicit-shell-file-name 'safe-local-variable #'identity)
	  (put 'explicit-sh-args 'safe-local-variable #'identity)

	  ;; Run `shell' interactively.  Since the default directory
	  ;; is remote, `explicit-shell-file-name' shall be set in
	  ;; order to avoid a question.  `explicit-sh-args' echoes the
	  ;; test data.
	  (with-current-buffer (get-buffer-create "*shell*")
	    (ignore-errors (kill-process (get-buffer-process (current-buffer))))
	    (should-not explicit-shell-file-name)
	    (call-interactively #'shell)
	    (with-timeout (10)
	      (while (accept-process-output
		      (get-buffer-process (current-buffer)) nil nil t)))
	    (should (string-match-p (rx bol "foo" eol) (buffer-string)))))

      ;; Cleanup.
      (put 'explicit-shell-file-name 'permanent-local nil)
      (custom-set-variables
       `(connection-local-profile-alist ',clpa now)
       `(connection-local-criteria-alist ',clca now))
      (kill-buffer "*shell*"))))

(ert-deftest tramp-test35-exec-path ()
  "Check `exec-path' and `executable-find'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))
  (skip-unless (tramp--test-supports-set-file-modes-p))

  (let ((tmp-name (tramp--test-make-temp-name))
	(default-directory ert-remote-temporary-file-directory))
    (unwind-protect
	(progn
	  (should (consp (exec-path)))
	  ;; Last element is the `exec-directory'.
	  (should
	   (string-equal
	    (car (last (exec-path)))
	    (file-remote-p default-directory 'localname)))
	  ;; The shell "sh" shall always exist.
	  (should (executable-find "sh" 'remote))
	  ;; Since the last element in `exec-path' is the current
	  ;; directory, an executable file in that directory will be
	  ;; found.
	  (write-region "foo" nil tmp-name)
	  (should (file-exists-p tmp-name))

	  (set-file-modes tmp-name #o777)
	  (should (file-executable-p tmp-name))
	  (should
	   (string-equal
	    (executable-find (file-name-nondirectory tmp-name) 'remote)
	    (file-remote-p tmp-name 'localname)))
	  (should-not
	   (executable-find
	    (concat (file-name-nondirectory tmp-name) "foo") 'remote)))

      ;; Cleanup.
      (ignore-errors (delete-file tmp-name)))))

(tramp--test-deftest-direct-async-process tramp-test35-exec-path)

;; This test is inspired by Bug#33781.
(ert-deftest tramp-test35-remote-path ()
  "Check loooong `tramp-remote-path'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-crypt-p)))

  (let* ((tmp-name (tramp--test-make-temp-name))
	 (default-directory ert-remote-temporary-file-directory)
         (orig-exec-path (exec-path))
         (tramp-remote-path tramp-remote-path)
	 (orig-tramp-remote-path tramp-remote-path)
	 path)
    ;; The "flatpak" method modifies `tramp-remote-path'.
    (skip-unless (not (tramp-compat-connection-local-p tramp-remote-path)))
    (unwind-protect
	(progn
          ;; Non existing directories are removed.
          (setq tramp-remote-path
                (cons (file-remote-p tmp-name 'localname) tramp-remote-path))
          (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
          (should (equal (exec-path) orig-exec-path))
          (setq tramp-remote-path orig-tramp-remote-path)

          ;; Double entries are removed.
          (setq tramp-remote-path (append '("/" "/") tramp-remote-path))
          (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
          (should (equal (exec-path) (cons "/" orig-exec-path)))
          (setq tramp-remote-path orig-tramp-remote-path)

          ;; We make a super long `tramp-remote-path'.
	  (unless (tramp--test-container-oob-p)
            (make-directory tmp-name)
            (should (file-directory-p tmp-name))
            (while (length< (string-join orig-exec-path ":") 5000)
              (let ((dir (make-temp-file
			  (file-name-as-directory tmp-name) 'dir)))
		(should (file-directory-p dir))
		(setq tramp-remote-path
                      (append
		       tramp-remote-path `(,(file-remote-p dir 'localname)))
                      orig-exec-path
                      (append
		       (butlast orig-exec-path)
		       `(,(file-remote-p dir 'localname))
		       (last orig-exec-path)))))
            (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
            (should (equal (exec-path) orig-exec-path))
	    ;; Ignore trailing newline.
	    (setq path (substring (shell-command-to-string "echo $PATH") nil -1))
	    ;; The shell doesn't handle such long strings.
	    (unless (length>
		     path
		     (tramp-get-connection-property
		      tramp-test-vec "pipe-buf" 4096))
	      ;; The last element of `exec-path' is `exec-directory'.
              (should
	       (string-equal path (string-join (butlast orig-exec-path) ":"))))
	    ;; The shell "sh" shall always exist.
	    (should (executable-find "sh" 'remote))))

      ;; Cleanup.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
      (setq tramp-remote-path orig-tramp-remote-path)
      (ignore-errors (delete-directory tmp-name 'recursive)))))

(tramp--test-deftest-direct-async-process tramp-test35-remote-path)

(ert-deftest tramp-test36-vc-registered ()
  "Check `vc-registered'."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-crypt-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory, in
    ;; order to establish the connection prior running an asynchronous
    ;; process.
    (let* ((default-directory
	     (file-truename ert-remote-temporary-file-directory))
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (expand-file-name "foo" tmp-name1))
	   (tramp-remote-process-environment tramp-remote-process-environment)
	   ;; Suppress nasty messages.
           (inhibit-message t)
	   (vc-handled-backends
	    (cond
	     ((tramp-find-executable
	       tramp-test-vec vc-git-program
	       (tramp-get-remote-path tramp-test-vec))
	      '(Git))
	     ((tramp-find-executable
	       tramp-test-vec vc-hg-program
	       (tramp-get-remote-path tramp-test-vec))
	      '(Hg))
	     ((tramp-find-executable
	       tramp-test-vec vc-bzr-program
	       (tramp-get-remote-path tramp-test-vec))
	      (setq tramp-remote-process-environment
		    (cons (format "BZR_HOME=%s"
				  (file-remote-p tmp-name1 'localname))
			  tramp-remote-process-environment))
	      ;; We must force a reconnect, in order to activate $BZR_HOME.
	      (tramp-cleanup-connection
	       tramp-test-vec 'keep-debug 'keep-password)
	      '(Bzr))
	     (t nil))))
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
		(error (ert-skip "`vc-create-repo' not supported")))
	      ;; The structure of VC-FILESET is not documented.  Let's
	      ;; hope it won't change.
	      (vc-register
	       (list (car vc-handled-backends)
		     (list (file-name-nondirectory tmp-name2))))
	      ;; vc-git uses an own process sentinel, Tramp's sentinel
	      ;; for flushing the cache isn't used.
	      (dired-uncache (concat (file-remote-p default-directory) "/"))
	      (should (vc-registered (file-name-nondirectory tmp-name2)))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name1 'recursive))))))

(ert-deftest tramp-test37-make-auto-save-file-name ()
  "Check `make-auto-save-file-name'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  tramp-allow-unsafe-temporary-files)

      (unwind-protect
	  (progn
	    ;; Use default `auto-save-file-name-transforms' mechanism.
	    ;; It isn't prepared for `separate' syntax.
	    (unless (eq tramp-syntax 'separate)
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
		      temporary-file-directory)))))))

	    ;; No mapping.
	    (let (tramp-auto-save-directory auto-save-file-name-transforms)
	      (with-temp-buffer
		(setq buffer-file-name tmp-name1)
		(should
		 (string-equal
		  (make-auto-save-file-name)
		  (funcall
		   (if quoted #'file-name-quote #'identity)
		   (expand-file-name
		    (format "#%s#" (file-name-nondirectory tmp-name1))
		    ert-remote-temporary-file-directory))))))

	    ;; Use default `tramp-auto-save-directory' mechanism.
	    ;; Ange-FTP doesn't care.
	    (unless (tramp--test-ange-ftp-p)
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
		       (file-name-unquote tmp-name1)))
		     tmp-name2)))
		  (should (file-directory-p tmp-name2)))))

	    ;; Relative file names shall work, too.  Ange-FTP doesn't care.
	    (unless (tramp--test-ange-ftp-p)
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
		       (file-name-unquote tmp-name1)))
		     tmp-name2)))
		  (should (file-directory-p tmp-name2)))))

	    ;; Create temporary file.  This shall check for sensible
	    ;; files, owned by root.
	    (let ((tramp-auto-save-directory temporary-file-directory))
	      (write-region "foo" nil tmp-name1)
	      (when (zerop (or (file-attribute-user-id
				(file-attributes tmp-name1))
			       tramp-unknown-id-integer))
		(with-temp-buffer
		  (setq buffer-file-name tmp-name1)
		  (tramp-cleanup-connection
		   tramp-test-vec 'keep-debug 'keep-password)
		  (let ((tramp-allow-unsafe-temporary-files t))
		    (should (stringp (make-auto-save-file-name))))
		  (tramp-cleanup-connection
		   tramp-test-vec 'keep-debug 'keep-password)
		  (cl-letf (((symbol-function #'yes-or-no-p) #'ignore))
		    (should-error
		     (make-auto-save-file-name)
		     :type 'file-error))
		  (tramp-cleanup-connection
		   tramp-test-vec 'keep-debug 'keep-password)
		  (cl-letf (((symbol-function #'yes-or-no-p) #'always))
		    (should (stringp (make-auto-save-file-name))))))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(ignore-errors (delete-directory tmp-name2 'recursive))
	(tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)))))

(ert-deftest tramp-test38-find-backup-file-name ()
  "Check `find-backup-file-name'."
  (skip-unless (tramp--test-enabled))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (ange-ftp-make-backup-files t)
	  tramp-allow-unsafe-temporary-files
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
		(if quoted #'file-name-quote #'identity)
		(expand-file-name
		 (format "%s~" (file-name-nondirectory tmp-name1))
		 ert-remote-temporary-file-directory))))))

	;; Cleanup.  Nothing to do yet.
	nil)

      (unwind-protect
	  ;; Map `backup-directory-alist'.
	  (let ((backup-directory-alist `(("." . ,tmp-name2)))
		tramp-backup-directory-alist)
	    (should
	     (equal
	      (find-backup-file-name tmp-name1)
	      (list
	       (funcall
		(if quoted #'file-name-quote #'identity)
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
	  ;; Map `tramp-backup-directory-alist'.  Ange-FTP doesn't care.
	  (unless (tramp--test-ange-ftp-p)
	    (let ((tramp-backup-directory-alist `(("." . ,tmp-name2)))
		  backup-directory-alist)
	      (should
	       (equal
		(find-backup-file-name tmp-name1)
		(list
		 (funcall
		  (if quoted #'file-name-quote #'identity)
		  (expand-file-name
		   (format
		    "%s~"
		    ;; This is taken from `make-backup-file-name-1'.
		    ;; We call `convert-standard-filename', because on
		    ;; MS Windows the (local) colons must be replaced
		    ;; by exclamation marks.
		    (subst-char-in-string
		     ?/ ?!
		     (replace-regexp-in-string
		      "!" "!!" (convert-standard-filename tmp-name1))))
		   tmp-name2)))))
	      ;; The backup directory is created.
	      (should (file-directory-p tmp-name2))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name2 'recursive)))

      (unwind-protect
	  ;; Map `tramp-backup-directory-alist' with local file name.
	  ;; Ange-FTP doesn't care.
	  (unless (tramp--test-ange-ftp-p)
	    (let ((tramp-backup-directory-alist
		   `(("." . ,(file-remote-p tmp-name2 'localname))))
		  backup-directory-alist)
	      (should
	       (equal
		(find-backup-file-name tmp-name1)
		(list
		 (funcall
		  (if quoted #'file-name-quote #'identity)
		  (expand-file-name
		   (format
		    "%s~"
		    ;; This is taken from `make-backup-file-name-1'.
		    ;; We call `convert-standard-filename', because on
		    ;; MS Windows the (local) colons must be replaced
		    ;; by exclamation marks.
		    (subst-char-in-string
		     ?/ ?!
		     (replace-regexp-in-string
		      "!" "!!" (convert-standard-filename tmp-name1))))
		   tmp-name2)))))
	      ;; The backup directory is created.
	      (should (file-directory-p tmp-name2))))

	;; Cleanup.
	(ignore-errors (delete-directory tmp-name2 'recursive)))

      (unwind-protect
	  ;; Create temporary file.  This shall check for sensible
	  ;; files, owned by root.
	  (let ((backup-directory-alist `(("." . ,temporary-file-directory)))
		tramp-backup-directory-alist)
	    (write-region "foo" nil tmp-name1)
	    (when (zerop (or (file-attribute-user-id (file-attributes tmp-name1))
			     tramp-unknown-id-integer))
	      (tramp-cleanup-connection
	       tramp-test-vec 'keep-debug 'keep-password)
	      (let ((tramp-allow-unsafe-temporary-files t))
		(should (stringp (car (find-backup-file-name tmp-name1)))))
	      (tramp-cleanup-connection
	       tramp-test-vec 'keep-debug 'keep-password)
	      (cl-letf (((symbol-function #'yes-or-no-p) #'ignore))
		(should-error
		 (find-backup-file-name tmp-name1)
		 :type 'file-error))
	      (tramp-cleanup-connection
	       tramp-test-vec 'keep-debug 'keep-password)
	      (cl-letf (((symbol-function #'yes-or-no-p) #'always))
		(should (stringp (car (find-backup-file-name tmp-name1)))))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)))))

(ert-deftest tramp-test39-make-lock-file-name ()
  "Check `make-lock-file-name', `lock-file', `unlock-file' and `file-locked-p'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (let ((tmp-name1 (tramp--test-make-temp-name nil quoted))
	  (tmp-name2 (tramp--test-make-temp-name nil quoted))
	  (remote-file-name-inhibit-cache t)
	  (remote-file-name-inhibit-locks nil)
	  (create-lockfiles t)
	  tramp-allow-unsafe-temporary-files
          (inhibit-message t)
	  ;; tramp-rclone.el and tramp-sshfs.el cache the mounted files.
	  (tramp-fuse-unmount-on-cleanup t)
          auto-save-default
	  noninteractive)

      (unwind-protect
	  (progn
	    ;; A simple file lock.
	    (should-not (file-locked-p tmp-name1))
	    (lock-file tmp-name1)
	    (should (eq (file-locked-p tmp-name1) t))

	    ;; If it is locked already, nothing changes.
	    (lock-file tmp-name1)
	    (should (eq (file-locked-p tmp-name1) t))

            ;; `save-buffer' removes the lock.
            (with-temp-buffer
              (set-visited-file-name tmp-name1)
              (insert "foo")
	      (should (buffer-modified-p))
              (save-buffer)
	      (should-not (buffer-modified-p)))
            (should-not (file-locked-p tmp-name1))

            ;; `kill-buffer' removes the lock.
	    ;; `kill-buffer--possibly-save' exists since Emacs 29.1.
	    (when (fboundp 'kill-buffer--possibly-save)
	      (lock-file tmp-name1)
	      (should (eq (file-locked-p tmp-name1) t))
              (with-temp-buffer
		(set-visited-file-name tmp-name1)
		(insert "foo")
		(should (buffer-modified-p))
		;; Modifying `read-from-minibuffer' doesn't work on MS Windows.
		(cl-letf (((symbol-function #'kill-buffer--possibly-save)
			   #'always))
                  (kill-buffer)))
	      (should-not (file-locked-p tmp-name1)))

            ;; `kill-buffer' should not remove the lock when the
            ;; connection is broken.  See Bug#61663.
	    ;; `kill-buffer--possibly-save' exists since Emacs 29.1.
	    (when (fboundp 'kill-buffer--possibly-save)
	      (lock-file tmp-name1)
	      (should (eq (file-locked-p tmp-name1) t))
              (with-temp-buffer
		(set-visited-file-name tmp-name1)
		(insert "foo")
		(should (buffer-modified-p))
		(tramp-cleanup-connection
		 tramp-test-vec 'keep-debug 'keep-password)
		;; Modifying `read-from-minibuffer' doesn't work on MS Windows.
		(cl-letf (((symbol-function #'kill-buffer--possibly-save)
			   #'always))
                  (kill-buffer)))
	      ;; A new connection changes process id, and also the
	      ;; lock file contents.  But it still exists.
	      (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	      (should (stringp (file-locked-p tmp-name1))))

	    ;; When `remote-file-name-inhibit-locks' is set, nothing happens.
	    (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	    (let ((remote-file-name-inhibit-locks t))
	      (lock-file tmp-name1)
	      (should-not (file-locked-p tmp-name1)))

	    ;; When `lock-file-name-transforms' is set, another lock
	    ;; file is used.
	    (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	    (let ((lock-file-name-transforms `((,(rx (* nonl)) ,tmp-name2))))
	      (should
	       (string-equal
		 (make-lock-file-name tmp-name1)
		 (make-lock-file-name tmp-name2)))
	      (lock-file tmp-name1)
	      (should (eq (file-locked-p tmp-name1) t))
	      (unlock-file tmp-name1)
	      (should-not (file-locked-p tmp-name1)))

	    ;; Steal the file lock.
	    (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	    ;; Modifying `read-char' doesn't work on MS Windows.
	    (cl-letf (((symbol-function #'ask-user-about-lock) #'always))
	      (lock-file tmp-name1))
	    (should (eq (file-locked-p tmp-name1) t))

	    ;; Ignore the file lock.
	    (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	    ;; Modifying `read-char' doesn't work on MS Windows.
	    (cl-letf (((symbol-function #'ask-user-about-lock) #'ignore))
	      (lock-file tmp-name1))
	    (should (stringp (file-locked-p tmp-name1)))

	    ;; Quit the file lock machinery.  There are problems with
	    ;; "sftp" and "podman", so we test on Emacs 29.1 only.
	    (when (tramp--test-emacs29-p )
	      (tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)
	      ;; Modifying `read-char' doesn't work on MS Windows.
	      (cl-letf (((symbol-function #'ask-user-about-lock)
			 (lambda (&rest args)
			   (signal 'file-locked args))))
		(should-error
		 (lock-file tmp-name1)
		 :type 'file-locked)
		;; The same for `write-region'.
		(should-error
		 (write-region "foo" nil tmp-name1)
		 :type 'file-locked)
		(should-error
		 (write-region "foo" nil tmp-name1 nil nil tmp-name1)
		 :type 'file-locked)
		;; The same for `set-visited-file-name'.
		(with-temp-buffer
	          (should-error
                   (set-visited-file-name tmp-name1)
		   :type 'file-locked))))
	    (should (stringp (file-locked-p tmp-name1))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(unlock-file tmp-name1)
	(unlock-file tmp-name2)
	(should-not (file-locked-p tmp-name1))
	(should-not (file-locked-p tmp-name2)))

      (unwind-protect
	  ;; Create temporary file.  This shall check for sensible
	  ;; files, owned by root.
	  (let ((lock-file-name-transforms auto-save-file-name-transforms))
	    (write-region "foo" nil tmp-name1)
	    (when (zerop (or (file-attribute-user-id (file-attributes tmp-name1))
			     tramp-unknown-id-integer))
	      (tramp-cleanup-connection
	       tramp-test-vec 'keep-debug 'keep-password)
	      (cl-letf (((symbol-function #'yes-or-no-p) #'ignore))
		(should-error
		 (write-region "foo" nil tmp-name1)
		 :type 'file-error))
	      (tramp-cleanup-connection
	       tramp-test-vec 'keep-debug 'keep-password)
	      (cl-letf (((symbol-function #'yes-or-no-p) #'always))
		(write-region "foo" nil tmp-name1))))

	;; Cleanup.
	(ignore-errors (delete-file tmp-name1))
	(tramp-cleanup-connection tramp-test-vec 'keep-debug 'keep-password)))))

(ert-deftest tramp-test39-detect-external-change ()
  "Check that an external file modification is reported."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))

  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    (dolist (create-lockfiles '(nil t))
      (let ((tmp-name (tramp--test-make-temp-name nil quoted))
	    (remote-file-name-inhibit-cache t)
	    (remote-file-name-inhibit-locks nil)
	    tramp-allow-unsafe-temporary-files
            (inhibit-message t)
	    ;; tramp-rclone.el and tramp-sshfs.el cache the mounted files.
	    (tramp-fuse-unmount-on-cleanup t)
            auto-save-default
	    (backup-inhibited t)
	    noninteractive)
        (with-temp-buffer
          (unwind-protect
	      (progn
		(setq buffer-file-name tmp-name
		      buffer-file-truename tmp-name)
		(insert "foo")
		;; Bug#53207: with `create-lockfiles' nil, saving the
		;; buffer results in a prompt.
		(cl-letf (((symbol-function #'read-from-minibuffer)
			   (lambda (&rest _)
			     (ert-fail "Test failed unexpectedly"))))
		  (should (buffer-modified-p))
		  (save-buffer)
		  (should-not (buffer-modified-p)))
		(should-not (file-locked-p tmp-name))

		;; For local files, just changing the file
		;; modification on disk doesn't hurt, because file
		;; contents in buffer and on disk are equal.  For
		;; remote files, file contents is not compared.  We
		;; mock an older modification time in buffer, because
		;; Tramp regards modification times equal if they
		;; differ for less than 2 seconds.
		(set-visited-file-modtime (time-add (current-time) -60))
		;; Some Tramp methods cannot check the file
		;; modification time properly, for them it doesn't
		;; make sense to test.
		(when (not (verify-visited-file-modtime))
		  (cl-letf (((symbol-function #'read-char-choice)
			     (lambda (prompt &rest _) (message "%s" prompt) ?y)))
		    (ert-with-message-capture captured-messages
		      (insert "bar")
		      (when create-lockfiles
			(should (string-match-p
				 (rx-to-string
				  `(: bol
				      ,(if (tramp--test-crypt-p)
					   '(+ nonl)
					 (file-name-nondirectory tmp-name))
				      " changed on disk; really edit the buffer?"))
				 captured-messages))
			(should (file-locked-p tmp-name)))))

		  ;; `save-buffer' removes the file lock.
		  (cl-letf (((symbol-function #'yes-or-no-p) #'always)
			    ((symbol-function #'read-char-choice)
			     (lambda (&rest _) ?y)))
		    (should (buffer-modified-p))
		    (save-buffer)
		    (should-not (buffer-modified-p)))
		  (should-not (file-locked-p tmp-name))))

	    ;; Cleanup.
	    (set-buffer-modified-p nil)
	    (ignore-errors (delete-file tmp-name))
	    (tramp-cleanup-connection
	     tramp-test-vec 'keep-debug 'keep-password)))))))

(ert-deftest tramp-test40-make-nearby-temp-file ()
  "Check `make-nearby-temp-file' and `temporary-file-directory'."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (tramp--test-ange-ftp-p)))

  (let ((default-directory ert-remote-temporary-file-directory)
	tmp-file)
    ;; The remote host shall know a temporary file directory.
    (should (stringp (temporary-file-directory)))
    (should
     (string-equal
      (file-remote-p default-directory)
      (file-remote-p (temporary-file-directory))))

    ;; The temporary file shall be located on the remote host.
    (setq tmp-file (make-nearby-temp-file tramp-test-name-prefix))
    (should (file-exists-p tmp-file))
    (should (file-regular-p tmp-file))
    (should
     (string-equal
      (file-remote-p default-directory)
      (file-remote-p tmp-file)))
    (delete-file tmp-file)
    (should-not (file-exists-p tmp-file))

    (setq tmp-file (make-nearby-temp-file tramp-test-name-prefix 'dir))
    (should (file-exists-p tmp-file))
    (should (file-directory-p tmp-file))
    (delete-directory tmp-file)
    (should-not (file-exists-p tmp-file))))

(defun tramp--test-emacs29-p ()
  "Check for Emacs version >= 29.1.
Some semantics has been changed for there, without new functions
or variables, so we check the Emacs version directly."
  (>= emacs-major-version 29))

(defun tramp--test-adb-p ()
  "Check, whether the remote host runs Android.
This requires restrictions of file name syntax."
  (tramp-adb-file-name-p ert-remote-temporary-file-directory))

(defun tramp--test-ange-ftp-p ()
  "Check, whether Ange-FTP is used."
  (eq
   (tramp-find-foreign-file-name-handler tramp-test-vec)
   'tramp-ftp-file-name-handler))

(defun tramp--test-asynchronous-processes-p ()
  "Whether asynchronous processes tests are run.
This is used in tests which we don't want to tag
`:tramp-asynchronous-processes' completely."
  (and
   (ert-select-tests
    (ert--stats-selector ert--current-run-stats)
    (list (make-ert-test :name (ert-test-name (ert-running-test))
                         :body nil :tags '(:tramp-asynchronous-processes))))
   ;; tramp-adb.el cannot apply multibyte commands.
   (not (and (tramp--test-adb-p)
	     (string-match-p (rx multibyte) default-directory)))))

(defun tramp--test-box-p ()
  "Check, whether the toolbox or distrobox method is used.
This does not support `tramp-test45-asynchronous-requests'."
  (string-match-p
   (rx bol (| "toolbox" "distrobox") eol)
   (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-container-p ()
  "Check, whether a container method is used.
This does not support some special file names."
  (string-match-p
   (rx bol (| "docker" "podman" "kubernetes" "apptainer" "run0" "nspawn"))
   (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-container-oob-p ()
  "Check, whether the dockercp or podmancp method is used.
They does not support wildcard copy."
  (string-match-p
   (rx bol (| "dockercp" "podmancp") eol)
   (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-crypt-p ()
  "Check, whether the remote directory is encrypted."
  (tramp-crypt-file-name-p ert-remote-temporary-file-directory))

(defun tramp--test-expensive-test-p ()
  "Whether expensive tests are run.
This is used in tests which we don't want to tag `:expensive'
completely."
  (ert-select-tests
   (ert--stats-selector ert--current-run-stats)
   (list (make-ert-test :name (ert-test-name (ert-running-test))
                        :body nil :tags '(:expensive-test)))))

(defun tramp--test-ftp-p ()
  "Check, whether an FTP-like method is used.
This does not support globbing characters in file names (yet)."
  ;; Globbing characters are ??, ?* and ?\[.
  (string-suffix-p
   "ftp" (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-fuse-p ()
  "Check, whether an FUSE file system isused."
  (or (tramp--test-rclone-p) (tramp--test-sshfs-p)))

(defun tramp--test-gdrive-p ()
  "Check, whether the gdrive method is used."
  (string-equal
   "gdrive" (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-gvfs-p (&optional method)
  "Check, whether the remote host runs a GVFS based method.
This requires restrictions of file name syntax.
If optional METHOD is given, it is checked first."
  (or (member method tramp-gvfs-methods)
      (tramp-gvfs-file-name-p ert-remote-temporary-file-directory)))

(defun tramp--test-hpux-p ()
  "Check, whether the remote host runs HP-UX.
Several special characters do not work properly there."
  ;; We must refill the cache.  `file-truename' does it.
  (file-truename ert-remote-temporary-file-directory)
  (ignore-errors (tramp-check-remote-uname tramp-test-vec (rx bol "HP-UX"))))

(defun tramp--test-ksh-p ()
  "Check, whether the remote shell is ksh.
ksh93 makes some strange conversions of non-latin characters into
a $'' syntax."
  ;; We must refill the cache.  `file-truename' does it.
  (file-truename ert-remote-temporary-file-directory)
  (string-suffix-p
   "ksh"
   (tramp-get-connection-property tramp-test-vec "remote-shell" "")))

(defun tramp--test-macos-p ()
  "Check, whether the remote host runs macOS."
  ;; We must refill the cache.  `file-truename' does it.
  (file-truename ert-remote-temporary-file-directory)
  (ignore-errors (tramp-check-remote-uname tramp-test-vec "Darwin")))

(defun tramp--test-mock-p ()
  "Check, whether the mock method is used.
This does not support external Emacs calls."
  (string-equal
   "mock" (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-netbsd-p ()
  "Check, whether the remote host runs NetBSD."
  ;; We must refill the cache.  `file-truename' does it.
  (file-truename ert-remote-temporary-file-directory)
  (ignore-errors (tramp-check-remote-uname tramp-test-vec "NetBSD")))

(defun tramp--test-openbsd-p ()
  "Check, whether the remote host runs OpenBSD."
  ;; We must refill the cache.  `file-truename' does it.
  (file-truename ert-remote-temporary-file-directory)
  (ignore-errors (tramp-check-remote-uname tramp-test-vec "OpenBSD")))

(defun tramp--test-out-of-band-p ()
  "Check, whether an out-of-band method is used."
  (tramp-method-out-of-band-p tramp-test-vec 1))

(defun tramp--test-putty-p ()
  "Check, whether the method method usaes PuTTY.
This does not support connection share for more than two connections."
  (member
   (file-remote-p ert-remote-temporary-file-directory 'method)
   '("plink" "plinkx" "pscp" "psftp")))

(defun tramp--test-rclone-p ()
  "Check, whether the remote host is offered by rclone.
This requires restrictions of file name syntax."
  (tramp-rclone-file-name-p ert-remote-temporary-file-directory))

(defun tramp--test-rsync-p ()
  "Check, whether the rsync method is used.
This does not support special file names."
  (string-equal
   "rsync" (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-sh-p ()
  "Check, whether the remote host runs a based method from tramp-sh.el."
  (tramp-sh-file-name-handler-p tramp-test-vec))

(defun tramp--test-sh-no-ls--dired-p ()
  "Check, whether the remote host runs a based method from tramp-sh.el.
Additionally, ls does not support \"--dired\"."
  (and (tramp--test-sh-p)
       (with-temp-buffer
	 ;; We must refill the cache.  `insert-directory' does it.
	 ;; This fails for tramp-crypt.el, so we ignore that.
	 (ignore-errors
	   (insert-directory ert-remote-temporary-file-directory "-al"))
	 (not (tramp-get-connection-property tramp-test-vec "ls--dired")))))

(defun tramp--test-share-p ()
  "Check, whether the method needs a share."
  (and (tramp--test-gvfs-p)
       (string-match-p
	(rx bol (| "afp" (: "dav" (? "s")) "smb") eol)
	(file-remote-p ert-remote-temporary-file-directory 'method))))

(defun tramp--test-sshfs-p ()
  "Check, whether the remote host is offered by sshfs.
This requires restrictions of file name syntax."
  (tramp-sshfs-file-name-p ert-remote-temporary-file-directory))

(defun tramp--test-sudoedit-p ()
  "Check, whether the sudoedit method is used."
  (tramp-sudoedit-file-name-p ert-remote-temporary-file-directory))

(defun tramp--test-telnet-p ()
  "Check, whether the telnet method is used.
This does not support special file names."
  (string-equal
   "telnet" (file-remote-p ert-remote-temporary-file-directory 'method)))

(defun tramp--test-windows-nt-p ()
  "Check, whether the locale host runs MS Windows."
  (eq system-type 'windows-nt))

(defun tramp--test-windows-nt-and-out-of-band-p ()
  "Check, whether the locale host runs MS Windows and an out-of-band method.
This does not support utf8 based file transfer."
  (and (tramp--test-windows-nt-p)
       (tramp--test-out-of-band-p)))

(defun tramp--test-windows-nt-or-smb-p ()
  "Check, whether the locale or remote host runs MS Windows.
This requires restrictions of file name syntax."
  (or (tramp--test-windows-nt-p)
      (tramp--test-smb-p)))

(defun tramp--test-smb-p ()
  "Check, whether the locale or remote host runs MS Windows.
This requires restrictions of file name syntax."
  (tramp-smb-file-name-p ert-remote-temporary-file-directory))

(defun tramp--test-supports-processes-p ()
  "Return whether the method under test supports external processes."
  (unless (tramp--test-crypt-p)
    ;; We use it to enable/disable tests in a given test run, for
    ;; example for remote processes on MS Windows.
    (if (tramp-connection-property-p
         tramp-test-vec "tramp--test-supports-processes-p")
	(tramp-get-connection-property
	 tramp-test-vec "tramp--test-supports-processes-p")
      (or (tramp--test-adb-p) (tramp--test-sh-p) (tramp--test-sshfs-p)))))

(defun tramp--test-supports-set-file-modes-p ()
  "Return whether the method under test supports setting file modes."
  ;; "smb" does not unless the SMB server supports "posix" extensions.
  ;; "adb" does not unless the Android device is rooted.
  (or (tramp--test-sh-p) (tramp--test-sshfs-p) (tramp--test-sudoedit-p)
      ;; Not all tramp-gvfs.el methods support changing the file mode.
      (and
       (tramp--test-gvfs-p)
       (string-suffix-p
	"ftp" (file-remote-p ert-remote-temporary-file-directory 'method)))))

(defun tramp--test-check-files (&rest files)
  "Run a simple but comprehensive test over every file in FILES."
  (dolist (quoted (if (tramp--test-expensive-test-p) '(nil t) '(nil)))
    ;; We must use `file-truename' for the temporary directory,
    ;; because it could be located on a symlinked directory.  This
    ;; would let the test fail.
    (let* ((ert-remote-temporary-file-directory
	    (file-truename ert-remote-temporary-file-directory))
	   (tramp-fuse-remove-hidden-files t)
	   (tmp-name1 (tramp--test-make-temp-name nil quoted))
	   (tmp-name2 (tramp--test-make-temp-name 'local quoted))
	   (files
            (tramp-compat-seq-keep
	     (lambda (x) (unless (string-empty-p x) x)) files))
	   (process-environment process-environment)
	   (sorted-files (sort (copy-sequence files) #'string-lessp))
	   buffer)
      (unwind-protect
	  (progn
	    (make-directory tmp-name1)
	    (make-directory tmp-name2)

	    (dolist (elt files)
              ;(tramp--test-message "'%s'" elt)
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
		     (if quoted #'file-name-quote #'identity)
		     (file-attribute-type (file-attributes file3)))
		    (file-remote-p (file-truename file1) 'localname)))
		  ;; Check file contents.
		  (with-temp-buffer
		    (insert-file-contents file3)
		    (should (string-equal (buffer-string) elt)))
		  (delete-file file3))))

	    ;; Check file names.
	    (should (equal (directory-files
			    tmp-name1 nil directory-files-no-dot-files-regexp)
			   sorted-files))
	    (should (equal (directory-files
			    tmp-name2 nil directory-files-no-dot-files-regexp)
			   sorted-files))
	    (should (equal (mapcar
			    #'car
			    (directory-files-and-attributes
			     tmp-name1 nil directory-files-no-dot-files-regexp))
			   sorted-files))
	    (should (equal (mapcar
			    #'car
			    (directory-files-and-attributes
			     tmp-name2 nil directory-files-no-dot-files-regexp))
			   sorted-files))

	    ;; Check, that `insert-directory' works properly.
	    (with-current-buffer
		(setq buffer (dired-noselect tmp-name1 "--dired -al"))
	      (goto-char (point-min))
	      (while (not (eobp))
		(when-let* ((name (dired-get-filename 'no-dir 'no-error)))
		  (unless
		      (string-match-p name directory-files-no-dot-files-regexp)
		    (should (member name files))))
		(forward-line 1)))
	    (kill-buffer buffer)

	    ;; `substitute-in-file-name' could return different
	    ;; values.  For "adb", there could be strange file
	    ;; permissions preventing overwriting a file.  We don't
	    ;; care in this test case.
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
		;; symlinks to existing files are shown there.  On
		;; NetBSD, there are problems with loooong file names,
		;; see Bug#65324.
		(tramp--test-ignore-make-symbolic-link-error
		  (unless (or (tramp--test-netbsd-p) (tramp--test-smb-p))
		    (make-symbolic-link file2 file3)
		    (should (file-symlink-p file3))
		    (should
		     (string-equal
		      (caar (directory-files-and-attributes
			     file1 nil (rx (literal elt1))))
		      elt1))
		    (should
		     (string-equal
		      (funcall
		       (if quoted #'file-name-quote #'identity)
		       (cadr (car (directory-files-and-attributes
				   file1 nil (rx (literal elt1))))))
		      (file-remote-p (file-truename file2) 'localname)))
		    (delete-file file3)
		    (should-not (file-exists-p file3))))

		;; Check, that a process runs on a remote
		;; `default-directory' with special characters.  See
		;; Bug#53846.
		(when (and (tramp--test-expensive-test-p)
			   (tramp--test-supports-processes-p))
		  (let ((default-directory file1))
		    (dolist (this-shell-command
			     (append
			      ;; Synchronously.
			      '(shell-command)
			      ;; Asynchronously.
			      (and (tramp--test-asynchronous-processes-p)
				   '(tramp--test-async-shell-command))))
		      (with-temp-buffer
			(funcall this-shell-command "cat -- *" (current-buffer))
			(should
			 (string-match-p
			  (rx (literal elt) eol) (buffer-string)))))))

		(delete-file file2)
		(should-not (file-exists-p file2))
		(delete-directory file1 'recursive)
		(should-not (file-exists-p file1))))

	    ;; Check, that environment variables are set correctly.
            ;; We do not run on macOS due to encoding problems.  See
            ;; Bug#36940.
	    (when (and (tramp--test-expensive-test-p) (tramp--test-sh-p)
		       (not (tramp--test-crypt-p))
		       (not (eq system-type 'darwin)))
	      (dolist (elt files)
		(let ((envvar (concat "VAR_" (upcase (md5 elt))))
		      (elt (encode-coding-string elt coding-system-for-read))
		      (default-directory ert-remote-temporary-file-directory)
		      (process-environment process-environment))
		  (setenv envvar elt)
		  ;; The value of PS1 could confuse Tramp's detection
		  ;; of process output.  So we unset it temporarily.
		  (setenv "PS1")
		  (with-temp-buffer
		    (when (zerop (process-file "printenv" nil t nil))
		      (goto-char (point-min))
		      (should
		       (search-forward-regexp
			(rx
			 bol (literal envvar)
			 "=" (literal (getenv envvar)) eol)))))))))

	;; Cleanup.
	(ignore-errors (kill-buffer buffer))
	(ignore-errors (delete-directory tmp-name1 'recursive))
	(ignore-errors (delete-directory tmp-name2 'recursive))))))

;; These tests are inspired by Bug#17238.
(ert-deftest tramp-test41-special-characters ()
  "Check special characters in file names."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (getenv "EMACS_HYDRA_CI"))) ; SLOW ~ 245s
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-rclone-p)))
  (skip-unless (not (or (eq system-type 'darwin) (tramp--test-macos-p))))

  ;; Newlines, slashes and backslashes in file names are not
  ;; supported.  So we don't test.  And we don't test the tab
  ;; character on Windows or Cygwin, because the backslash is
  ;; interpreted as a path separator, preventing "\t" from being
  ;; expanded to <TAB>.
  (let ((files
	 (list
	  (cond ((or (tramp--test-ange-ftp-p)
		     (tramp--test-container-p)
		     (tramp--test-gvfs-p)
		     (tramp--test-openbsd-p)
		     (tramp--test-rclone-p)
		     (tramp--test-sudoedit-p)
		     (tramp--test-windows-nt-or-smb-p))
		 "foo bar baz")
		((or (tramp--test-adb-p)
		     (eq system-type 'cygwin))
		 " foo bar baz ")
		((tramp--test-sh-no-ls--dired-p)
		 "\tfoo bar baz\t")
		(t " foo\tbar baz\t"))
	  "@foo@bar@baz@"
	  (unless (tramp--test-windows-nt-and-out-of-band-p) "$foo$bar$$baz$")
	  "-foo-bar-baz-"
	  (unless (tramp--test-windows-nt-and-out-of-band-p) "%foo%bar%baz%")
	  "&foo&bar&baz&"
	  (unless (or (tramp--test-ftp-p)
		      (tramp--test-gvfs-p)
		      (tramp--test-windows-nt-or-smb-p))
	    "?foo?bar?baz?")
	  (unless (or (tramp--test-container-oob-p)
		      (tramp--test-ftp-p)
		      (tramp--test-gvfs-p)
		      (tramp--test-windows-nt-or-smb-p))
	    "*foo+bar*baz+")
	  (if (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	      "'foo'bar'baz'"
	    "'foo\"bar'baz\"")
	  "#foo~bar#baz~"
	  (unless (tramp--test-windows-nt-and-out-of-band-p)
	    (if (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
		"!foo!bar!baz!"
	      "!foo|bar!baz|"))
	  (if (or (tramp--test-gvfs-p)
		  (tramp--test-rclone-p)
		  (tramp--test-windows-nt-or-smb-p))
	      ";foo;bar;baz;"
	    ":foo;bar:baz;")
	  (unless (or (tramp--test-gvfs-p) (tramp--test-windows-nt-or-smb-p))
	    "<foo>bar<baz>")
	  "(foo)bar(baz)"
	  (unless (or (tramp--test-container-oob-p)
		      (tramp--test-ftp-p)
		      (tramp--test-gvfs-p))
	    "[foo]bar[baz]")
	  "{foo}bar{baz}")))
    ;; Simplify test in order to speed up.
    (apply #'tramp--test-check-files
	   (if (tramp--test-expensive-test-p)
	       files (list (string-join files ""))))))

(tramp--test-deftest-with-stat tramp-test41-special-characters)

(tramp--test-deftest-with-perl tramp-test41-special-characters)

(tramp--test-deftest-with-ls tramp-test41-special-characters)

(tramp--test-deftest-without-file-attributes tramp-test41-special-characters)

(tramp--test-deftest-direct-async-process tramp-test41-special-characters)

(ert-deftest tramp-test42-utf8 ()
  "Check UTF8 encoding in file names and file contents."
  (skip-unless (tramp--test-enabled))
  (skip-unless (not (getenv "EMACS_HYDRA_CI"))) ; SLOW ~ 620s
  (skip-unless (not (tramp--test-container-p)))
  (skip-unless (not (tramp--test-rsync-p)))
  (skip-unless (not (tramp--test-windows-nt-and-out-of-band-p)))
  (skip-unless (not (tramp--test-ksh-p)))
  (skip-unless (not (tramp--test-gdrive-p)))
  (skip-unless (not (tramp--test-crypt-p)))
  (skip-unless (not (tramp--test-rclone-p)))
  (skip-unless (not (or (eq system-type 'darwin) (tramp--test-macos-p))))

  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(file-name-coding-system
	 (coding-system-change-eol-conversion 'utf-8 'unix)))
    (apply
     #'tramp--test-check-files
     (append
      (list
       (unless (tramp--test-hpux-p) "Γυρίστε το Γαλαξία με Ώτο Στοπ")
       (unless (tramp--test-hpux-p)
	 "أصبح بوسعك الآن تنزيل نسخة كاملة من موسوعة ويكيبيديا العربية لتصفحها بلا اتصال بالإنترنت")
       "银河系漫游指南系列"
       "Автостопом по гала́ктике"
       ;; Use codepoints without a name.  See Bug#31272.
       ;; Works on some Android systems only.
       (unless (or (tramp--test-adb-p) (tramp--test-openbsd-p))
	 "bung")
       ;; Use codepoints from Supplementary Multilingual Plane (U+10000
       ;; to U+1FFFF).
       "🌈🍒👋")

      (when (and (tramp--test-expensive-test-p) (not (tramp--test-windows-nt-p)))
	(delete-dups
	 (mapcar
	  ;; Use all available language specific snippets.
	  (lambda (x)
	    (and
	     ;; The "Oriya" and "Odia" languages use some problematic
	     ;; composition characters.
	     (not (member (car x) '("Oriya" "Odia")))
             (stringp (setq x (eval (get-language-info (car x) 'sample-text) t)))
	     ;; Filter out strings which use unencodable characters.
	     (not (and (or (tramp--test-gvfs-p) (tramp--test-smb-p))
		       (unencodable-char-position
			0 (length x) file-name-coding-system nil x)))
	     ;; Filter out not displayable characters.
	     (setq x (mapconcat
		      (lambda (y)
			(and (char-displayable-p y) (char-to-string y)))
		      x ""))
             (not (string-empty-p x))
	     ;; ?\n and ?/ shouldn't be part of any file name.  ?\t,
	     ;; ?. and ?? do not work for "smb" method.  " " does not
	     ;; work at begin or end of the string for MS Windows.
	     (replace-regexp-in-string (rx (any " \t\n/.?")) "" x)))
	  language-info-alist)))))))

(tramp--test-deftest-with-stat tramp-test42-utf8)

(tramp--test-deftest-with-perl tramp-test42-utf8)

(tramp--test-deftest-with-ls tramp-test42-utf8)

(tramp--test-deftest-without-file-attributes tramp-test42-utf8)

(tramp--test-deftest-direct-async-process tramp-test42-utf8)

(ert-deftest tramp-test43-file-system-info ()
  "Check that `file-system-info' returns proper values."
  (skip-unless (tramp--test-enabled))

  (when-let* ((fsi (file-system-info ert-remote-temporary-file-directory)))
    (should (consp fsi))
    (should (length= fsi 3))
    (dotimes (i (length fsi))
      (should (natnump (or (nth i fsi) 0))))))

;; `file-user-uid' and `file-group-gid' were introduced in Emacs 30.1.
(ert-deftest tramp-test44-file-user-group-ids ()
  "Check results of user/group functions.
`file-user-uid', `file-group-gid', and `tramp-get-remote-*'
should all return proper values."
  (skip-unless (tramp--test-enabled))

  (let ((default-directory ert-remote-temporary-file-directory))
    ;; `file-user-uid' and `file-group-gid' exist since Emacs 30.1.
    ;; We don't want to see compiler warnings for older Emacsen.
    (when (fboundp 'file-user-uid)
      (should (integerp (with-no-warnings (file-user-uid)))))
    (when (fboundp 'file-group-gid)
      (should (integerp (with-no-warnings (file-group-gid)))))

    (with-parsed-tramp-file-name default-directory nil
      (should (or (integerp (tramp-get-remote-uid v 'integer))
		  (null (tramp-get-remote-uid v 'integer))))
      (should (or (stringp (tramp-get-remote-uid v 'string))
		  (null (tramp-get-remote-uid v 'string))))

      (should (or (integerp (tramp-get-remote-gid v 'integer))
		  (null (tramp-get-remote-gid v 'integer))))
      (should (or (stringp (tramp-get-remote-gid v 'string))
		  (null (tramp-get-remote-gid v 'string))))

      (when-let* ((groups (tramp-get-remote-groups v 'integer)))
	(should (consp groups))
	(dolist (group groups) (should (integerp group))))
      (when-let* ((groups (tramp-get-remote-groups v 'string)))
	(should (consp groups))
	(dolist (group groups) (should (stringp group)))))))

;; `tramp-test45-asynchronous-requests' could be blocked.  So we set a
;; timeout of 300 seconds, and we send a SIGUSR1 signal after 300
;; seconds.  Similar check is performed in the timer function.
(defconst tramp--test-asynchronous-requests-timeout 300
  "Timeout for `tramp-test45-asynchronous-requests'.")

;; This test is inspired by Bug#16928.
(ert-deftest tramp-test45-asynchronous-requests ()
  "Check parallel asynchronous requests.
Such requests could arrive from timers, process filters and
process sentinels.  They shall not disturb each other."
  :tags '(:expensive-test :tramp-asynchronous-processes)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-supports-processes-p))
  (skip-unless (not (tramp--test-container-p)))
  (skip-unless (not (tramp--test-sshfs-p)))
  (skip-unless (not (tramp--test-telnet-p)))
  (skip-unless (not (tramp--test-box-p)))
  (skip-unless (not (tramp--test-windows-nt-p)))

  (with-timeout
      (tramp--test-asynchronous-requests-timeout (tramp--test-timeout-handler))
    (define-key special-event-map [sigusr1] #'tramp--test-timeout-handler)
    (let* (;; For the watchdog.
	   (default-directory (expand-file-name temporary-file-directory))
	   ;; It doesn't work on w32 systems.
	   (watchdog
            (start-process-shell-command
             "*watchdog*" nil
             (format
	      "sleep %d; kill -USR1 %d"
	      tramp--test-asynchronous-requests-timeout (emacs-pid))))
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
	   ;; PuTTY-based methods can only share up to 10 connections.
	   (tramp-use-connection-share
	    (if (and (tramp--test-putty-p) (>= number-proc 10))
		'suppress (bound-and-true-p tramp-use-connection-share)))
           ;; On hydra, timings are bad.
           (timer-repeat
            (cond
             ((getenv "EMACS_HYDRA_CI") 10)
             (t 1)))
	   ;; This is when all timers start.  We check inside the
	   ;; timer function, that we don't exceed timeout.
	   (timer-start (current-time))
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
                (when (> (- (time-to-seconds) (time-to-seconds timer-start))
                         tramp--test-asynchronous-requests-timeout)
                  (tramp--test-timeout-handler))
                (when buffers
                  (let ((time (float-time))
                        (default-directory tmp-name)
                        (file (buffer-name (seq-random-elt buffers))))
                    (tramp--test-message
		     "Start timer %s %s" file (current-time-string))
		    (dired-uncache file)
		    (tramp--test-message
		     "Continue timer %s %s" file (file-attributes file))
		    (vc-registered file)
                    (tramp--test-message
                     "Stop timer %s %s" file (current-time-string))
                    ;; Adjust timer if it takes too much time.
                    (when (> (- (float-time) time) timer-repeat)
                      (setq timer-repeat (* 1.1 (- (float-time) time)))
                      (setf (timer--repeat-delay timer) timer-repeat)
                      (tramp--test-message
		       "Increase timer %s" timer-repeat)))))))

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
		       "(read line && echo $line >$line && echo $line);"
		       "(read line && cat $line);"
		       "(read line && rm -f $line)")))
                    (file (expand-file-name (buffer-name buf))))
                ;; Remember the file name.  Add counter.
                (process-put proc 'foo file)
                (process-put proc 'bar 0)
                ;; Add process filter.
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (tramp--test-message
                    "Process filter %s %s %s"
		    proc string (current-time-string))
                   (with-current-buffer (process-buffer proc)
                     (insert string))
                   (when (< (process-get proc 'bar) 2)
		     (dired-uncache (process-get proc 'foo))
                     (should (file-attributes (process-get proc 'foo))))))
                ;; Add process sentinel.  It shall not perform remote
                ;; operations, triggering Tramp processes.  This blocks.
                (set-process-sentinel
                 proc
                 (lambda (proc _state)
                   (tramp--test-message
                    "Process sentinel %s %s" proc (current-time-string))))
		(tramp--test-message "Process started %s" proc)))

            ;; Send a string to the processes.  Use a random order of
            ;; the buffers.  Mix with regular operation.
            (let ((buffers (copy-sequence buffers))
		  buf)
              (while buffers
		(setq buf (seq-random-elt buffers))
                (if-let* ((proc (get-buffer-process buf))
			  (file (process-get proc 'foo))
			  (count (process-get proc 'bar)))
		    (progn
                      (tramp--test-message
                       "Start action %d %s %s" count buf (current-time-string))
                      ;; Regular operation prior process action.
		      (dired-uncache file)
                      (if (= count 0)
			  (should-not (file-attributes file))
			(should (file-attributes file)))
                      ;; Send string to process.
                      (process-send-string
		       proc (format "%s\n" (buffer-name buf)))
                      (while (accept-process-output nil 0))
                      (tramp--test-message
                       "Continue action %d %s %s"
		       count buf (current-time-string))
                      ;; Regular operation post process action.
		      (dired-uncache file)
                      (if (= count 2)
			  (should-not (file-attributes file))
			(should (file-attributes file)))
                      (tramp--test-message
                       "Stop action %d %s %s" count buf (current-time-string))
                      (process-put proc 'bar (1+ count))
                      (unless (process-live-p proc)
			(setq buffers (delq buf buffers))))
		  (setq buffers (delq buf buffers)))))

            ;; Checks.  All process output shall exist in the
            ;; respective buffers.  All created files shall be
            ;; deleted.
            (tramp--test-message "Check %s" (current-time-string))
            (dolist (buf buffers)
              (with-current-buffer buf
                (should
		 (string-equal
		  ;; tramp-adb.el echoes, so we must add the three strings.
		  (if (tramp--test-adb-p)
		      (format "%s\n%s\n%s\n%s\n%s\n" buf buf buf buf buf)
		    (format "%s\n%s\n" buf buf))
		  (buffer-string)))))
            (should-not
             (directory-files
              tmp-name nil directory-files-no-dot-files-regexp)))

        ;; Cleanup.
        (define-key special-event-map [sigusr1] #'ignore)
        (ignore-errors (quit-process watchdog))
        (dolist (buf buffers)
          (ignore-errors (delete-process (get-buffer-process buf)))
          (ignore-errors (kill-buffer buf)))
        (ignore-errors (cancel-timer timer))
        (ignore-errors (delete-directory tmp-name 'recursive))))))

;; (tramp--test-deftest-direct-async-process tramp-test45-asynchronous-requests
;;   'unstable)

(ert-deftest tramp-test46-dired-compress-file ()
  "Check that Tramp (un)compresses normal files."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-crypt-p)))
  ;; Starting with Emacs 29.1, `dired-compress-file' is performed by
  ;; default handler.
  (skip-unless (not (tramp--test-emacs29-p)))

  (let ((default-directory ert-remote-temporary-file-directory)
        (tmp-name (tramp--test-make-temp-name)))
    (write-region "foo" nil tmp-name)
    (dired default-directory)
    (dired-revert)
    (dired-goto-file tmp-name)
    (should-not (dired-compress))
    (should (string= (concat tmp-name ".gz") (dired-get-filename)))
    (should-not (dired-compress))
    (should (string= tmp-name (dired-get-filename)))
    (delete-file tmp-name)))

(ert-deftest tramp-test46-dired-compress-dir ()
  "Check that Tramp (un)compresses directories."
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-sh-p))
  (skip-unless (not (tramp--test-crypt-p)))
  ;; Starting with Emacs 29.1, `dired-compress-dir' is performed by
  ;; default handler.
  (skip-unless (not (tramp--test-emacs29-p)))

  (let ((default-directory ert-remote-temporary-file-directory)
        (tmp-name (tramp--test-make-temp-name)))
    (make-directory tmp-name)
    (dired default-directory)
    (dired-revert)
    (dired-goto-file tmp-name)
    (should-not (dired-compress))
    (should (string= (concat tmp-name ".tar.gz") (dired-get-filename)))
    (should-not (dired-compress))
    (should (string= tmp-name (dired-get-filename)))
    (delete-directory tmp-name)
    (delete-file (concat tmp-name ".tar.gz"))))

(ert-deftest tramp-test47-read-password ()
  "Check Tramp password handling."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-enabled))
  (skip-unless (tramp--test-mock-p))
  ;; Not all read commands understand argument "-s" or "-p".
  (skip-unless
   (string-empty-p
    (let ((shell-file-name tramp-default-remote-shell))
      (shell-command-to-string "read -s -p Password: pass"))))

  (let* ((pass "secret")
	 (tramp-connection-properties
	  `((nil "login-args"
		 (("-c")
		  (,(tramp-shell-quote-argument
		     (concat
		      "read -s -p 'Password: ' pass; echo; "
		      "(test \"pass$pass\" != \"pass" pass "\" && "
		      "echo \"Login incorrect\" || "
		      tramp-default-remote-shell " -i)")))))))
	 mocked-input auth-sources)
    ;; We must mock `read-string', in order to avoid interactive
    ;; arguments.
    (cl-letf* (((symbol-function #'read-string)
		(lambda (&rest _args) (pop mocked-input))))

      ;; Reading password from stdin works.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug)
      ;; We don't want to invalidate the password.
      (setq mocked-input `(,(copy-sequence pass)))
      (should (file-exists-p ert-remote-temporary-file-directory))

      ;; Don't entering a password returns in error.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug)
      (setq mocked-input nil)
      (should-error (file-exists-p ert-remote-temporary-file-directory))

      ;; A wrong password doesn't work either.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug)
      (setq mocked-input `(,(concat pass pass)))
      (should-error (file-exists-p ert-remote-temporary-file-directory))

      ;; Reading password from auth-source works.  We use the netrc
      ;; backend; the other backends shall behave similar.
      ;; Macro `ert-with-temp-file' was introduced in Emacs 29.1.
      (with-no-warnings (when (symbol-plist 'ert-with-temp-file)
	(tramp-cleanup-connection tramp-test-vec 'keep-debug)
	(setq mocked-input nil)
	(auth-source-forget-all-cached)
	(ert-with-temp-file netrc-file
	  :prefix tramp-test-name-prefix :suffix ""
	  :text (format
		 "machine %s port mock password %s"
		 (file-remote-p ert-remote-temporary-file-directory 'host) pass)
	  (let ((auth-sources `(,netrc-file)))
	    (should (file-exists-p ert-remote-temporary-file-directory))))))

      ;; Checking session-timeout.
      (with-no-warnings (when (symbol-plist 'ert-with-temp-file)
	(tramp-cleanup-connection tramp-test-vec 'keep-debug)
	(let ((tramp-connection-properties
	       (cons '(nil "session-timeout" 1)
		     tramp-connection-properties)))
	  (setq mocked-input nil)
	  (auth-source-forget-all-cached)
	  (ert-with-temp-file netrc-file
	    :prefix tramp-test-name-prefix :suffix ""
	    :text (format
		   "machine %s port mock password %s"
		   (file-remote-p ert-remote-temporary-file-directory 'host)
		   pass)
	    (let ((auth-sources `(,netrc-file)))
	      (should (file-exists-p ert-remote-temporary-file-directory))))
	  ;; Session established, password cached.
	  (should
	   (password-in-cache-p
	    (auth-source-format-cache-entry
	     (tramp-get-connection-property tramp-test-vec " pw-spec"))))
	  ;; We want to see the timeout message.
	  (tramp--test-instrument-test-case 3
	    (sleep-for 2))
	  ;; Session canceled, no password in cache.
	  (should-not
	   (password-in-cache-p
	    (auth-source-format-cache-entry
	     (tramp-get-connection-property tramp-test-vec " pw-spec")))))))))

  ;; Cleanup.
  (tramp-cleanup-connection tramp-test-vec 'keep-debug))

(ert-deftest tramp-test47-read-otp-password ()
  "Check Tramp one-time password handling."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-mock-p))
  ;; Not all read commands understand argument "-s" or "-p".
  (skip-unless
   (string-empty-p
    (let ((shell-file-name tramp-default-remote-shell))
      (shell-command-to-string "read -s -p Password: pass"))))

  (let* ((pass "secret")
	 (tramp-connection-properties
	  `((nil "login-args"
		 (("-c")
		  (,(tramp-shell-quote-argument
		     (concat
		      "read -s -p 'Verification code: ' pass; echo; "
		      "(test \"pass$pass\" != \"pass" pass "\" && "
		      "echo \"Login incorrect\" || "
		      tramp-default-remote-shell " -i)")))))))
	 mocked-input auth-sources)
    ;; We must mock `read-string', in order to avoid interactive
    ;; arguments.
    (cl-letf* (((symbol-function #'read-string)
		(lambda (&rest _args) (pop mocked-input))))

      ;; Reading password from stdin works.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug)
      ;; We don't want to invalidate the password.
      (setq mocked-input `(,(copy-sequence pass)))
      (should (file-exists-p ert-remote-temporary-file-directory))

      ;; Don't entering a password returns in error.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug)
      (setq mocked-input nil)
      (should-error (file-exists-p ert-remote-temporary-file-directory))

      ;; A wrong password doesn't work either.
      (tramp-cleanup-connection tramp-test-vec 'keep-debug)
      (setq mocked-input `(,(concat pass pass)))
      (should-error (file-exists-p ert-remote-temporary-file-directory))

      ;; The password shouldn't be read from auth-source.
      ;; Macro `ert-with-temp-file' was introduced in Emacs 29.1.
      (with-no-warnings (when (symbol-plist 'ert-with-temp-file)
	(tramp-cleanup-connection tramp-test-vec 'keep-debug)
	(setq mocked-input nil)
	(auth-source-forget-all-cached)
	(ert-with-temp-file netrc-file
	  :prefix tramp-test-name-prefix :suffix ""
	  :text (format
		 "machine %s port mock password %s"
		 (file-remote-p ert-remote-temporary-file-directory 'host)
		 pass)
	  (let ((auth-sources `(,netrc-file)))
	    (should-error
	     (file-exists-p ert-remote-temporary-file-directory))))))))

  ;; Cleanup.
  (tramp-cleanup-connection tramp-test-vec 'keep-debug))

(ert-deftest tramp-test47-read-security-key ()
  "Check Tramp security key handling."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-mock-p))
  ;; Not all read commands understand argument "-s" or "-p".
  (skip-unless
   (string-empty-p
    (let ((shell-file-name tramp-default-remote-shell))
      (shell-command-to-string "read -s -p Password: pass"))))

  (let (;; Suppress "exec".
	(tramp-restricted-shell-hosts-alist `(,tramp-system-name)))

    ;; Reading security key works.
    (tramp-cleanup-connection tramp-test-vec 'keep-debug)
    (let ((tramp-connection-properties
	   `((nil "login-args"
		  (("-c")
		   (,(tramp-shell-quote-argument
		      "echo Confirm user presence for key XXX"))
		   (";") ("sleep" "1")
		   (";") ("echo" "User presence confirmed")
		   (";") ("sleep" "1")
		   (";") (,tramp-default-remote-shell "-i"))))))
      (should (file-exists-p ert-remote-temporary-file-directory)))

    (let* ((pin "1234")
	   (tramp-connection-properties
	    `((nil "login-args"
		   (("-c")
		    (,(tramp-shell-quote-argument
		       (concat
			"echo Confirm user presence for key XXX; sleep 1; "
			"read -s -p 'Enter PIN for XXX ' pin; echo; "
			"(test \"pin$pin\" != \"pin" pin "\" && "
			"echo \"Login incorrect\" || "
			"(echo User presence confirmed; "
			tramp-default-remote-shell " -i))")))))))
	   mocked-input auth-sources)
      ;; We must mock `read-string', in order to avoid interactive
      ;; arguments.
      (cl-letf* (((symbol-function #'read-string)
		  (lambda (&rest _args) (pop mocked-input))))

	;; Reading security PIN works.
	(tramp-cleanup-connection tramp-test-vec 'keep-debug)
	;; We don't want to invalidate the pin.
	(setq mocked-input `(,(copy-sequence pin)))
	(should (file-exists-p ert-remote-temporary-file-directory))

	;; Don't entering a security PIN returns in error.
	(tramp-cleanup-connection tramp-test-vec 'keep-debug)
	(setq mocked-input nil)
	(should-error (file-exists-p ert-remote-temporary-file-directory))

	;; A wrong security PIN doesn't work either.
	(tramp-cleanup-connection tramp-test-vec 'keep-debug)
	(setq mocked-input `(,(concat pin pin)))
	(should-error (file-exists-p ert-remote-temporary-file-directory))))

    ;; Timeout is detected.
    (tramp-cleanup-connection tramp-test-vec 'keep-debug)
    (let ((tramp-connection-properties
	   `((nil "login-args"
		  (("-c")
		   (,(tramp-shell-quote-argument
		      "echo Confirm user presence for key XXX"))
		   (";") ("sleep" "1")
		   (";") ("echo" "sign_and_send_pubkey: signing failed for XXX")
		   (";") ("sleep" "1")
		   (";") ("exit" "1"))))))
      (should-error (file-exists-p ert-remote-temporary-file-directory))))

  ;; Cleanup.
  (tramp-cleanup-connection tramp-test-vec 'keep-debug))

(ert-deftest tramp-test47-read-fingerprint ()
  "Check Tramp fingerprint handling."
  :tags '(:expensive-test)
  (skip-unless (tramp--test-mock-p))

  (let (;; Suppress "exec".
	(tramp-restricted-shell-hosts-alist `(,tramp-system-name)))

    ;; Reading fingerprint works.
    (tramp-cleanup-connection tramp-test-vec 'keep-debug)
    (let ((tramp-connection-properties
	   `((nil "login-args"
		  (("-c")
		   (,(tramp-shell-quote-argument
		      "echo Place your finger on the fingerprint reader"))
		   (";") ("sleep" "1")
		   (";") (,tramp-default-remote-shell "-i"))))))
      (should (file-exists-p ert-remote-temporary-file-directory)))

    ;; Falling back after a timeout works.
    (tramp-cleanup-connection tramp-test-vec 'keep-debug)
    (let ((tramp-connection-properties
	   `((nil "login-args"
		  (("-c")
		   (,(tramp-shell-quote-argument
		      "echo Place your finger on the fingerprint reader"))
		   (";") ("sleep" "1")
		   (";") ("echo" "Failed to match fingerprint")
		   (";") (,tramp-default-remote-shell "-i"))))))
      (should (file-exists-p ert-remote-temporary-file-directory)))

    ;; Interrupting the fingerprint handshaking works.
    (tramp-cleanup-connection tramp-test-vec 'keep-debug)
    (let ((tramp-connection-properties
	   `((nil "login-args"
		  (("-c")
		   (,(tramp-shell-quote-argument
		      "echo Place your finger on the fingerprint reader"))
		   (";") ("sleep" "1")
		   (";") (,tramp-default-remote-shell "-i")))))
	  tramp-use-fingerprint)
      (should (file-exists-p ert-remote-temporary-file-directory))))

  ;; Cleanup.
  (tramp-cleanup-connection tramp-test-vec 'keep-debug))

;; This test is inspired by Bug#29163.
(ert-deftest tramp-test48-auto-load ()
  "Check that Tramp autoloads properly."
  ;; If we use another syntax but `default', Tramp is already loaded
  ;; due to the `tramp-change-syntax' call.
  (skip-unless (eq tramp-syntax 'default))
  (skip-unless (tramp--test-enabled))

  (let ((default-directory (expand-file-name temporary-file-directory))
	(code
	 (format
	  ;; Suppress method name check.
	  "(let ((non-essential t)) \
             (message \"Tramp loaded: %%s\" (and (file-remote-p %S) t)))"
	  ert-remote-temporary-file-directory)))
    (should
     (string-match-p
      (rx "Tramp loaded: t" (+ (any "\r\n")))
      (shell-command-to-string
       (format
	"%s -batch -Q -L %s --eval %s"
	(shell-quote-argument
	 (expand-file-name invocation-name invocation-directory))
	(mapconcat #'shell-quote-argument load-path " -L ")
	(shell-quote-argument code)))))))

(ert-deftest tramp-test48-delay-load ()
  "Check that Tramp is loaded lazily, only when needed."
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
       (string-match-p
	(rx
	 "Tramp loaded: nil" (+ (any "\r\n"))
	 "Tramp loaded: nil" (+ (any "\r\n"))
	 "Tramp loaded: " (literal (symbol-name tm)) (+ (any "\r\n")))
	(shell-command-to-string
	 (format
	  "%s -batch -Q -L %s --eval %s"
	  (shell-quote-argument
	   (expand-file-name invocation-name invocation-directory))
	  (mapconcat #'shell-quote-argument load-path " -L ")
	  (shell-quote-argument (format code tm)))))))))

(ert-deftest tramp-test48-recursive-load ()
  "Check that Tramp does not fail due to recursive load."
  (skip-unless (tramp--test-enabled))

  (let ((default-directory (expand-file-name temporary-file-directory)))
    (dolist (code
	     (list
	      (format
	       "(expand-file-name %S)" ert-remote-temporary-file-directory)
	      (format
	       "(let ((default-directory %S)) (expand-file-name %S))"
	       ert-remote-temporary-file-directory
	       temporary-file-directory)))
      (should-not
       (string-match-p
	"Recursive load"
	(shell-command-to-string
	 (format
	  "%s -batch -Q -L %s --eval %s"
	  (shell-quote-argument
	   (expand-file-name invocation-name invocation-directory))
	  (mapconcat #'shell-quote-argument load-path " -L ")
	  (shell-quote-argument code))))))))

(ert-deftest tramp-test48-remote-load-path ()
  "Check that Tramp autoloads its packages with remote `load-path'."
  ;; `tramp-cleanup-all-connections' is autoloaded from tramp-cmds.el.
  ;; It shall still work, when a remote file name is in the
  ;; `load-path'.
  (let ((default-directory (expand-file-name temporary-file-directory))
	(code
	 "(let ((force-load-messages t) \
		(load-path (cons \"/foo:bar:\" load-path))) \
	    (tramp-cleanup-all-connections))"))
    (should
     (string-match-p
      (rx
       "Loading "
       (literal
        (expand-file-name
         "tramp-cmds" (file-name-directory (locate-library "tramp")))))
      (shell-command-to-string
       (format
	"%s -batch -Q -L %s -l tramp-sh --eval %s"
	(shell-quote-argument
	 (expand-file-name invocation-name invocation-directory))
	(mapconcat #'shell-quote-argument load-path " -L ")
	(shell-quote-argument code)))))))

(ert-deftest tramp-test49-without-remote-files ()
  "Check that Tramp can be suppressed."
  (skip-unless (tramp--test-enabled))

  (should (file-remote-p ert-remote-temporary-file-directory))
  (should-not
   (without-remote-files (file-remote-p ert-remote-temporary-file-directory)))
  (should (file-remote-p ert-remote-temporary-file-directory))

  (inhibit-remote-files)
  (should-not (file-remote-p ert-remote-temporary-file-directory))
  (tramp-register-file-name-handlers)
  (setq tramp-mode t)
  (should (file-remote-p ert-remote-temporary-file-directory)))

(ert-deftest tramp-test50-unload ()
  "Check that Tramp and its subpackages unload completely.
Since it unloads Tramp, it shall be the last test to run."
  :tags '(:expensive-test)
  ;; We have autoloaded objects from tramp.el and tramp-archive.el.
  ;; In order to remove them, we first need to load both packages.
  (require 'tramp)
  (require 'tramp-archive)
  (should (featurep 'tramp))
  (should (featurep 'tramp-archive))
  ;; This unloads also tramp-archive.el and tramp-theme.el if needed.
  (unload-feature 'tramp 'force)

  ;; No Tramp feature must be left except the test packages.
  (should-not (featurep 'tramp))
  (should-not (featurep 'tramp-archive))
  (should-not (featurep 'tramp-theme))
  (should-not
   (all-completions
    "tramp" (delq 'tramp-tests (delq 'tramp-archive-tests features))))

  ;; `file-name-handler-alist' must be clean.
  (should-not (all-completions "tramp" (mapcar #'cdr file-name-handler-alist)))

  ;; There shouldn't be left a bound symbol, except buffer-local
  ;; variables, and autoloaded functions.  We do not regard our test
  ;; symbols, and the Tramp unload hooks.
  (mapatoms
   (lambda (x)
     (and (or (and (boundp x) (null (local-variable-if-set-p x)))
	      (and (functionp x) (null (autoloadp (symbol-function x))))
	      (macrop x))
	  (string-prefix-p "tramp" (symbol-name x))
	  (string-match-p (rx bol "with" (| "tramp" "parsed")) (symbol-name x))
	  ;; `tramp-register-archive-file-name-handler' is autoloaded
	  ;; in Emacs < 29.1.
	  (not (eq 'tramp-register-archive-file-name-handler x))
	  ;; `tramp-compat-rx' is autoloaded in Emacs 29.1.
	  (not (eq 'tramp-compat-rx x))
	  (not (string-match-p
		(rx bol "tramp" (? "-archive") (** 1 2 "-") "test")
		(symbol-name x)))
	  (not (string-suffix-p "unload-hook" (symbol-name x)))
	  (not (get x 'tramp-autoload))
	  (ert-fail (format "`%s' still bound" x)))))

  ;; The defstruct `tramp-file-name' and all its internal functions
  ;; shall be purged.
  (should-not (cl--find-class 'tramp-file-name))
  (mapatoms
   (lambda (x)
     (and (functionp x) (null (autoloadp (symbol-function x)))
          (string-prefix-p "tramp-file-name" (symbol-name x))
          (ert-fail (format "Structure function `%s' still exists" x)))))

  ;; There shouldn't be left a hook function containing a Tramp
  ;; function.  We do not regard the Tramp unload hooks.
  (mapatoms
   (lambda (x)
     (and (boundp x)
	  (string-match-p
	   (rx "-" (| "hook" "function") (? "s") eol) (symbol-name x))
	  (not (string-suffix-p "unload-hook" (symbol-name x)))
	  (consp (symbol-value x))
	  (ignore-errors (all-completions "tramp" (symbol-value x)))
	  (ert-fail (format "Hook `%s' still contains Tramp function" x)))))

  ;; There shouldn't be left an advice function from Tramp.
  (mapatoms
   (lambda (x)
     (and (functionp x)
	  (advice-mapc
	   (lambda (fun _symbol)
	     (and (string-prefix-p "tramp" (symbol-name fun))
		  (ert-fail
		   (format "Function `%s' still contains Tramp advice" x))))
	   x))))

  ;; Reload.
  (require 'tramp)
  (require 'tramp-archive)
  (should (featurep 'tramp))
  (should (featurep 'tramp-archive)))

(defun tramp-test-all (&optional interactive)
  "Run all tests for \\[tramp].
If INTERACTIVE is non-nil, the tests are run interactively."
  (interactive "p")
  (funcall
   (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
   (rx bol "tramp")))

;; TODO:

;; * dired-uncache (partly done in other test functions)
;; * file-equal-p (partly done in `tramp-test21-file-links')
;; * file-in-directory-p
;; * file-name-case-insensitive-p
;; * tramp-get-home-directory
;; * tramp-set-file-uid-gid

;; * Work on skipped tests.  Make a comment, when it is impossible.
;; * Use `skip-when' starting with Emacs 30.1.
;; * Revisit expensive tests, once problems in `tramp-error' are solved.
;; * Fix `tramp-test06-directory-file-name' for "ftp".
;; * Check, why a process filter t doesn't work in
;;   `tramp-test29-start-file-process' and
;;   `tramp-test30-make-process'.
;; * Implement `tramp-test31-interrupt-process' and
;;   `tramp-test31-signal-process' for "adb", "sshfs" and for direct
;;   async processes.  Check, why they don't run stable.
;; * Check, why `tramp-test45-asynchronous-requests' often fails.  The
;;   famous reentrant error?
;; * Check, why direct async processes do not work for
;;   `tramp-test45-asynchronous-requests'.

(provide 'tramp-tests)

;;; tramp-tests.el ends here
