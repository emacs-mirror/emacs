;;; file-notify-tests.el --- Tests of file notifications

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

;; A whole test run can be performed calling the command `file-notify-test-all'.

;;; Code:

(require 'ert)
(require 'filenotify)
(require 'tramp)

;; There is no default value on w32 systems, which could work out of the box.
(defconst file-notify-test-remote-temporary-file-directory
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

(defvar file-notify--test-tmpfile nil)
(defvar file-notify--test-tmpfile1 nil)
(defvar file-notify--test-results nil)
(defvar file-notify--test-event nil)

(setq password-cache-expiry nil
      tramp-verbose 0
      tramp-message-show-message nil)

;; This shall happen on hydra only.
(when (getenv "NIX_STORE")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; We do not want to try and fail `file-notify-add-watch'.
(defun file-notify--test-local-enabled ()
  "Whether local file notification is enabled.
This is needed for local `temporary-file-directory' only, in the
remote case we return always `t'."
  (or file-notify--library
      (file-remote-p temporary-file-directory)))

(defvar file-notify--test-remote-enabled-checked nil
  "Cached result of `file-notify--test-remote-enabled'.
If the function did run, the value is a cons cell, the `cdr'
being the result.")

(defun file-notify--test-remote-enabled ()
  "Whether remote file notification is enabled."
  (unless (consp file-notify--test-remote-enabled-checked)
    (let (desc)
      (unwind-protect
	  (ignore-errors
	    (and
	     (file-remote-p file-notify-test-remote-temporary-file-directory)
	     (file-directory-p file-notify-test-remote-temporary-file-directory)
	     (file-writable-p file-notify-test-remote-temporary-file-directory)
	     (setq desc
		   (file-notify-add-watch
		    file-notify-test-remote-temporary-file-directory
		    '(change) 'ignore))))
	;; Unwind forms.
	(setq file-notify--test-remote-enabled-checked (cons t desc))
	(when desc (file-notify-rm-watch desc)))))
  ;; Return result.
  (cdr file-notify--test-remote-enabled-checked))

(defmacro file-notify--deftest-remote (test docstring)
  "Define ert `TEST-remote' for remote files."
  `(ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
     ,docstring
     (let* ((temporary-file-directory
	     file-notify-test-remote-temporary-file-directory)
	    (ert-test (ert-get-test ',test)))
       (skip-unless (file-notify--test-remote-enabled))
       (tramp-cleanup-connection
	(tramp-dissect-file-name temporary-file-directory) nil 'keep-password)
       (funcall (ert-test-body ert-test)))))

(ert-deftest file-notify-test00-availability ()
  "Test availability of `file-notify'."
  (skip-unless (file-notify--test-local-enabled))
  (let (desc)
    ;; Check, that different valid parameters are accepted.
    (should (setq desc (file-notify-add-watch
			temporary-file-directory '(change) 'ignore)))
    (file-notify-rm-watch desc)))

(file-notify--deftest-remote file-notify-test00-availability
  "Test availability of `file-notify' for remote files.")

(ert-deftest file-notify-test01-add-watch ()
  "Check `file-notify-add-watch'."
  (skip-unless (file-notify--test-local-enabled))
  (let (desc)
    ;; Check, that different valid parameters are accepted.
    (should (setq desc (file-notify-add-watch
			temporary-file-directory '(change) 'ignore)))
    (file-notify-rm-watch desc)
    (should (setq desc (file-notify-add-watch
			temporary-file-directory
			'(attribute-change) 'ignore)))
    (file-notify-rm-watch desc)
    (should (setq desc (file-notify-add-watch
			temporary-file-directory
			'(change attribute-change) 'ignore)))
    (file-notify-rm-watch desc)

    ;; Check error handling.
    (should-error (file-notify-add-watch 1 2 3 4)
		  :type 'wrong-number-of-arguments)
    (should
     (equal (should-error (file-notify-add-watch 1 2 3))
	    '(wrong-type-argument 1)))
    (should
     (equal (should-error (file-notify-add-watch
			   temporary-file-directory 2 3))
	    '(wrong-type-argument 2)))
    (should
     (equal (should-error (file-notify-add-watch
			   temporary-file-directory '(change) 3))
	    '(wrong-type-argument 3)))))

(file-notify--deftest-remote file-notify-test01-add-watch
  "Check `file-notify-add-watch' for remote files.")

(defun file-notify--test-event-test ()
  "Ert test function to be called by `file-notify--test-event-handler'.
We cannot pass arguments, so we assume that `file-notify--test-event'
is bound somewhere."
  ;(message "Event %S" file-notify--test-event)
  ;; Check the file name.
  (should
   (string-equal (file-notify--event-file-name file-notify--test-event)
		 file-notify--test-tmpfile))
  ;; Check the second file name if exists.
  (when (eq (nth 1 file-notify--test-event) 'renamed)
    (should
     (string-equal
      (file-notify--event-file1-name file-notify--test-event)
      file-notify--test-tmpfile1))))

(defun file-notify--test-event-handler (file-notify--test-event)
  "Run a test over FILE-NOTIFY--TEST-EVENT.
Save the result in `file-notify--test-results', for later analysis."
  (let ((result
	 (ert-run-test (make-ert-test :body 'file-notify--test-event-test))))
    (setq file-notify--test-results
	  (append file-notify--test-results `(,result)))))

(defun file-notify--test-make-temp-name ()
  "Create a temporary file name for test."
  (expand-file-name
   (make-temp-name "file-notify-test") temporary-file-directory))

(defmacro file-notify--wait-for-events (timeout until)
  "Wait for file notification events until form UNTIL is true.
TIMEOUT is the maximum time to wait for, in seconds."
  `(with-timeout (,timeout (ignore))
     (while (null ,until)
       (read-event nil nil 0.1))))

(ert-deftest file-notify-test02-events ()
  "Check file creation/removal notifications."
  (skip-unless (file-notify--test-local-enabled))
  (let (desc)
    (unwind-protect
	(progn
	  (setq file-notify--test-results nil
		file-notify--test-tmpfile (file-notify--test-make-temp-name)
		file-notify--test-tmpfile1 (file-notify--test-make-temp-name)
		desc
		(file-notify-add-watch
		 file-notify--test-tmpfile
		 '(change) 'file-notify--test-event-handler))

	  ;; Check creation and removal.
	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (delete-file file-notify--test-tmpfile)
	  (sleep-for 0.1)

	  ;; Check copy and rename.
	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (copy-file file-notify--test-tmpfile file-notify--test-tmpfile1)
	  (delete-file file-notify--test-tmpfile)
	  (delete-file file-notify--test-tmpfile1)
	  (sleep-for 0.1)

	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (rename-file file-notify--test-tmpfile file-notify--test-tmpfile1)
	  (delete-file file-notify--test-tmpfile1)
	  (sleep-for 0.1))

      ;; Wait for events, and exit.
      (file-notify--wait-for-events 5 file-notify--test-results)
      (file-notify-rm-watch desc)
      (ignore-errors (delete-file file-notify--test-tmpfile))
      (ignore-errors (delete-file file-notify--test-tmpfile1))))

  (should file-notify--test-results)
  (dolist (result file-notify--test-results)
    ;(message "%s" (ert-test-result-messages result))
    (when (ert-test-failed-p result)
      (ert-fail (cadr (ert-test-result-with-condition-condition result))))))

(file-notify--deftest-remote file-notify-test02-events
  "Check file creation/removal notifications for remote files.")

(defvar auto-revert-remote-files)
(defvar auto-revert-stop-on-user-input)
(setq auto-revert-remote-files t
      auto-revert-stop-on-user-input nil)
(require 'autorevert)

(ert-deftest file-notify-test03-autorevert ()
  "Check autorevert via file notification.
This test is skipped in batch mode."
  (skip-unless (file-notify--test-local-enabled))
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let* ((remote (file-remote-p temporary-file-directory))
	 (timeout (if remote 60 10))
	 buf)
    (unwind-protect
	(progn
	  (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))

	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (setq buf (find-file-noselect file-notify--test-tmpfile))
	  (with-current-buffer buf
	    (should (string-equal (buffer-string) "any text"))
	    (auto-revert-mode 1)

	    ;; `auto-revert-buffers' runs every 5".
	    (with-timeout (timeout (ignore))
	      (while (null auto-revert-notify-watch-descriptor)
		(sleep-for 1)))

	    ;; Check, that file notification has been used.
	    (should auto-revert-mode)
	    (should auto-revert-use-notify)
	    (should auto-revert-notify-watch-descriptor)

	    ;; Modify file.  We wait for a second, in order to
	    ;; have another timestamp.
	    (sleep-for 1)
	    (shell-command
	     (format "echo -n 'another text' >%s"
		     (or (file-remote-p file-notify--test-tmpfile 'localname)
			 file-notify--test-tmpfile)))

	    ;; Check, that the buffer has been reverted.
	    (with-current-buffer (get-buffer-create "*Messages*")
	      (file-notify--wait-for-events
	       timeout
	       (string-match (format "Reverting buffer `%s'." (buffer-name buf))
			     (buffer-string))))
	    (should (string-match "another text" (buffer-string)))))

      ;; Exit.
      (ignore-errors (kill-buffer buf))
      (ignore-errors (delete-file file-notify--test-tmpfile)))))

(file-notify--deftest-remote file-notify-test03-autorevert
  "Check autorevert via file notification for remote files.
This test is skipped in batch mode.")

(defun file-notify-test-all (&optional interactive)
  "Run all tests for \\[file-notify]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^file-notify-")
    (ert-run-tests-batch "^file-notify-")))

(provide 'file-notify-tests)
;;; file-notify-tests.el ends here
