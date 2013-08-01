;;; file-notify-tests.el --- Tests of file notifications

;; Copyright (C) 2013 Free Software Foundation, Inc.

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

;; Some of the tests are intended to run over remote files.  Set
;; `file-notify-test-remote-temporary-file-directory' to a suitable
;; value.  It must NOT require an interactive password prompt, when
;; running the tests in batch mode.

;; If you want to skip tests for remote files, set this variable to
;; `null-device'.

;;; Code:

(require 'ert)
(require 'filenotify)

;; There is no default value on w32 systems, which could work out of the box.
(defconst file-notify-test-remote-temporary-file-directory
  (if (eq system-type 'windows-nt) null-device "/ssh::/tmp")
  "Temporary directory for Tramp tests.")

(defvar file-notify--test-tmpfile nil)
(defvar file-notify--test-tmpfile1 nil)
(defvar file-notify--test-results nil)
(defvar file-notify--test-event nil)

(require 'tramp)
(require 'tramp-sh)
(setq tramp-verbose 0
      tramp-message-show-message nil)
(when noninteractive (defalias 'tramp-read-passwd 'ignore))

;; We do not want to try and fail `file-notify-add-watch'.
(defconst file-notify--test-local-enabled file-notify--library
  "Whether local file notification is enabled.")

;; We need also a check on the remote side, w/o adding a file monitor.
(defun file-notify--test-remote-enabled ()
  "Whether remote file notification is enabled."
  (ignore-errors
    (and (file-remote-p file-notify-test-remote-temporary-file-directory)
	 (file-directory-p file-notify-test-remote-temporary-file-directory)
	 (file-writable-p file-notify-test-remote-temporary-file-directory)
	 ;; Extracted from tramp-sh-handle-file-notify-add-watch.
	 ;; Even though the "remote" system is just ssh@localhost,
	 ;; the PATH might not be the same as the "local" PATH.
	 ;; Eg this seems to be the case on hydra.nixos.org.
	 ;; Without this, tests fail with:
	 ;; "No file notification program found on /ssh:localhost:"
	 ;; Try to fix PATH instead?
	 (with-parsed-tramp-file-name
	     file-notify-test-remote-temporary-file-directory nil
	     (or (tramp-get-remote-gvfs-monitor-dir v)
		 (tramp-get-remote-inotifywait v))))))

(defmacro file-notify--deftest-remote (test docstring)
  "Define ert `TEST-remote' for remote files."
  `(when (and (file-notify--test-remote-enabled) (ert-get-test ',test))
     ;; Define the test.
     (ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
       ,docstring
       (let* ((temporary-file-directory
	       file-notify-test-remote-temporary-file-directory)
	      (ert-test (ert-get-test ',test))
	      (most-recent-result (ert-test-most-recent-result ert-test))
	      result)
	 (unwind-protect
	     (progn
	       (setq result
		     (condition-case err
			 (ert-run-test ert-test)
		       ((error quit)
			(ert-fail err))))
	       (when (ert-test-failed-p result)
		 (ert-fail
		  (cadr (ert-test-result-with-condition-condition result)))))
	   ;; Reset status of TEST.
	   (setf (ert-test-most-recent-result ert-test) most-recent-result))))))

(ert-deftest file-notify-test00-availability ()
  "Test availability of `file-notify'."
  (let (desc)
    ;; Check, that different valid parameters are accepted.
    (should (setq desc (file-notify-add-watch
			temporary-file-directory '(change) 'ignore)))
    (file-notify-rm-watch desc)))

(file-notify--deftest-remote file-notify-test00-availability
  "Test availability of `file-notify' for remote files.")

(when file-notify--test-local-enabled

  (ert-deftest file-notify-test01-add-watch ()
    "Check `file-notify-add-watch'."
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
  ) ;; file-notify--test-local-enabled

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

(when file-notify--test-local-enabled

  (ert-deftest file-notify-test02-events ()
    "Check file creation/removal notifications."
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
	    (write-region "any text" nil file-notify--test-tmpfile)
	    (delete-file file-notify--test-tmpfile)

	    ;; Check copy and rename.
	    (write-region "any text" nil file-notify--test-tmpfile)
	    (copy-file file-notify--test-tmpfile file-notify--test-tmpfile1)
	    (delete-file file-notify--test-tmpfile)
	    (delete-file file-notify--test-tmpfile1)

	    (write-region "any text" nil file-notify--test-tmpfile)
	    (rename-file file-notify--test-tmpfile file-notify--test-tmpfile1)
	    (delete-file file-notify--test-tmpfile1))

	;; Wait for events, and exit.
	(sit-for 5 'nodisplay)
	(file-notify-rm-watch desc)
	(ignore-errors (delete-file file-notify--test-tmpfile))
	(ignore-errors (delete-file file-notify--test-tmpfile1))))

    (dolist (result file-notify--test-results)
      ;(message "%s" (ert-test-result-messages result))
      (when (ert-test-failed-p result)
	(ert-fail (cadr (ert-test-result-with-condition-condition result))))))

  (file-notify--deftest-remote file-notify-test02-events
    "Check file creation/removal notifications for remote files.")
  ) ;; file-notify--test-local-enabled

;; autorevert runs only in interactive mode.
(defvar auto-revert-remote-files)
(setq auto-revert-remote-files t)
(require 'autorevert)
(when (and file-notify--test-local-enabled (null noninteractive))

  (ert-deftest file-notify-test03-autorevert ()
    "Check autorevert via file notification.
This test is skipped in batch mode."
    ;; `auto-revert-buffers' runs every 5".  And we must wait, until
    ;; the file has been reverted.
    (let ((timeout 10)
	  buf)
      (unwind-protect
	  (progn
	    (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))

	    (write-region "any text" nil file-notify--test-tmpfile)
	    (setq buf (find-file-noselect file-notify--test-tmpfile))
	    (with-current-buffer buf
	      (should (string-equal (buffer-string) "any text"))
	      (auto-revert-mode 1)

	      ;; `auto-revert-buffers' runs every 5".
	      (with-timeout (timeout (ignore))
		(while (null auto-revert-notify-watch-descriptor)
		  (sit-for 0.1 'nodisplay)))

	      ;; Check, that file notification has been used.
	      (should auto-revert-mode)
	      (should auto-revert-use-notify)
	      (should auto-revert-notify-watch-descriptor)

	      ;; Modify file.  We wait for a second, in order to
	      ;; have another timestamp.
	      (sit-for 1)
	      (shell-command
	       (format "echo -n 'another text' >%s"
		       (or (file-remote-p file-notify--test-tmpfile 'localname)
			   file-notify--test-tmpfile)))

	      ;; Check, that the buffer has been reverted.
	      (with-current-buffer (get-buffer-create "*Messages*")
		(with-timeout (timeout (ignore))
		  (while
		      (null (string-match
			     (format "Reverting buffer `%s'." (buffer-name buf))
			     (buffer-string)))
		    (sit-for 0.1 'nodisplay))))
	      (should (string-equal (buffer-string) "another text"))))

	;; Exit.
	(ignore-errors (kill-buffer buf))
	(ignore-errors (delete-file file-notify--test-tmpfile)))))

  (file-notify--deftest-remote file-notify-test03-autorevert
    "Check autorevert via file notification for remote files.
This test is skipped in batch mode.")
  ) ;; (and file-notify--test-local-enabled (null noninteractive))

(defun file-notify-test-all (&optional interactive)
  "Run all tests for \\[file-notify]."
  (interactive "p")
  (when file-notify--test-local-enabled
    (if interactive
	(ert-run-tests-interactively "^file-notify-")
      (ert-run-tests-batch "^file-notify-"))))

(provide 'file-notify-tests)
;;; file-notify-tests.el ends here
