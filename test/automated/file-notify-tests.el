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
;; value.  The remote host must also provide the `inotifywait' program.

;;; Code:

(require 'ert)
(require 'filenotify)

(ert-deftest file-notify-test0 ()
  "Test availability of \\[file-notify]."
  (should (memq file-notify-support '(gfilenotify inotify w32notify))))

(ert-deftest file-notify-test1 ()
  "Add watch via \\[file-notify-add-watch]."
  (let (desc)
    ;; Check, that different valid parameters are accepted.
    (should (setq desc (file-notify-add-watch "/" '(change) 'ignore)))
    (file-notify-rm-watch desc)
    (should (setq desc (file-notify-add-watch "/" '(attribute-change) 'ignore)))
    (file-notify-rm-watch desc)
    (should (setq desc (file-notify-add-watch
			"/" '(change attribute-change) 'ignore)))
    (file-notify-rm-watch desc)

    ;; Check error handling.
    (should
     (equal (car (should-error (file-notify-add-watch 1 2 3 4)))
	    'wrong-number-of-arguments))
    (should
     (equal (should-error (file-notify-add-watch 1 2 3))
	    '(wrong-type-argument 1)))
    (should
     (equal (should-error (file-notify-add-watch "/" 2 3))
	    '(wrong-type-argument 2)))
    (should
     (equal (should-error (file-notify-add-watch "/" '(change) 3))
	    '(wrong-type-argument 3)))))

(defvar file-notify-test-tmpfile nil)
(defvar file-notify-test-tmpfile1 nil)
(defvar file-notify-test-results nil)
(defconst file-notify-test-remote-temporary-file-directory "/ssh::/tmp"
  "Temporary directory for Tramp tests.")

(defvar tramp-verbose)
(setq tramp-verbose 0)
(defvar file-notify--event)

(defun file-notify-test-run-remote-test ()
  "Check, whether the remote tests can be run."
  (ignore-errors
    (and
     (file-directory-p file-notify-test-remote-temporary-file-directory)
     (file-writable-p file-notify-test-remote-temporary-file-directory))))

(defun file-notify-event-test ()
  "Ert test function to be called by `file-notify-test-event-handler'.
We cannot pass arguments, so we assume that `file-notify--event'
is bound somewhere."
  (message "Event %S" file-notify--event)
  ;; Check the file name.
  (should
   (string-equal (file-notify--event-file-name file-notify--event)
		 file-notify-test-tmpfile))
  ;; Check the second file name if exists.
  (when (eq (nth 1 file-notify--event) 'renamed)
    (should
     (string-equal
      (file-notify--event-file1-name file-notify--event)
      file-notify-test-tmpfile1))))

(defun file-notify-test-event-handler (file-notify--event)
  "Run a test over FILE-NOTIFY--EVENT.
Save the result in `file-notify-test-results', for later analysis."
  (let ((result (ert-run-test (make-ert-test :body 'file-notify-event-test))))
    (setq file-notify-test-results
	  (append file-notify-test-results `(,result)))))

(defun file-notify-test-make-temp-name ()
  "Create a temporary file name for test."
  (expand-file-name
   (make-temp-name "file-notify-test") temporary-file-directory))

(ert-deftest file-notify-test2 ()
  "Check file creation/removal notifications."
  (let (desc)
    (unwind-protect
	(progn
	  (setq file-notify-test-results nil
		file-notify-test-tmpfile (file-notify-test-make-temp-name)
		file-notify-test-tmpfile1 (file-notify-test-make-temp-name)
		desc
		(file-notify-add-watch
		 file-notify-test-tmpfile
		 '(change) 'file-notify-test-event-handler))

	  ;; Check creation and removal.
	  (write-region "any text" nil file-notify-test-tmpfile)
	  (delete-file file-notify-test-tmpfile)

	  ;; Check copy and rename.
	  (write-region "any text" nil file-notify-test-tmpfile)
	  (copy-file file-notify-test-tmpfile file-notify-test-tmpfile1)
	  (delete-file file-notify-test-tmpfile)
	  (delete-file file-notify-test-tmpfile1)

	  (write-region "any text" nil file-notify-test-tmpfile)
	  (rename-file file-notify-test-tmpfile file-notify-test-tmpfile1)
	  (delete-file file-notify-test-tmpfile1))

      ;; Wait for events, and exit.
      (sit-for 5 'nodisplay)
      (file-notify-rm-watch desc)
      (ignore-errors (delete-file file-notify-test-tmpfile))
      (ignore-errors (delete-file file-notify-test-tmpfile1))))

  (dolist (result file-notify-test-results)
    ;(message "%s" (ert-test-result-messages result))
    (when (ert-test-failed-p result)
      (ert-fail (cadr (ert-test-result-with-condition-condition result))))))

;; TODO: When the remote test fails, suppress FAILED indication for TEST.
(defmacro file-notify-test-remote (test)
  "Run ert TEST for remote files."
  `(let* ((temporary-file-directory
	   file-notify-test-remote-temporary-file-directory)
	  (ert-test (ert-get-test ,test))
	  (most-recent-result (ert-test-most-recent-result ert-test))
	  result)
     (unwind-protect
	 (progn
	   (setq result
		 (condition-case err
		     (ert-run-test (ert-get-test ,test))
		   ((error quit)
		    (ert-fail err))))
	   (when (ert-test-failed-p result)
	     (ert-fail
	      (cadr (ert-test-result-with-condition-condition result)))))
       ;; Reset status of TEST.
       (setf (ert-test-most-recent-result ert-test) most-recent-result))))

(when (file-notify-test-run-remote-test)
  (ert-deftest file-notify-test3 ()
    "Check file creation/removal notification for remote files."
    (file-notify-test-remote 'file-notify-test2))
) ;; (file-notify-test-run-remote-test)

;; autorevert runs only in interactive mode.
(defvar auto-revert-remote-files)
(setq auto-revert-remote-files t)
(require 'autorevert)
(when (null noninteractive)

  (ert-deftest file-notify-test4 ()
    "Check autorevert via file notification.
This test is skipped in batch mode."
    ;; `auto-revert-buffers' runs every 5".  And we must wait, until
    ;; the file has been reverted.
    (let ((wait 10)
	  buf)
      (unwind-protect
	  (progn
	    (setq file-notify-test-tmpfile (file-notify-test-make-temp-name))

	    (write-region "any text" nil file-notify-test-tmpfile)
	    (setq buf (find-file-noselect file-notify-test-tmpfile))
	    (with-current-buffer buf
	      (should (string-equal (buffer-string) "any text"))
	      (auto-revert-mode 1)
	      ;; `auto-revert-buffers' runs every 5".
	      (sit-for wait)

	      ;; Check, that file notification has been used.
	      (should auto-revert-mode)
	      (should auto-revert-use-notify)
	      (should auto-revert-notify-watch-descriptor)

	      ;; Modify file.
	      (shell-command
	       (format "echo -n 'another text' >%s"
		       (or (file-remote-p file-notify-test-tmpfile 'localname)
			   file-notify-test-tmpfile)))
	      (sit-for wait)

	      ;; Check, that the buffer has been reverted.
	      (should (string-equal (buffer-string) "another text"))))

	;; Exit.
	(ignore-errors (kill-buffer buf))
	(ignore-errors (delete-file file-notify-test-tmpfile)))))

  (when (file-notify-test-run-remote-test)
    (ert-deftest file-notify-test5 ()
      "Check autorevert via file notification for remote files.
This test is skipped in batch mode."
      (file-notify-test-remote 'file-notify-test4))
    ) ;; (file-notify-test-run-remote-test)
  ) ;; (null noninteractive)

(defun file-notify-test-all (&optional interactive)
  "Run all tests for \\[file-notify]."
  (interactive "p")
  (when file-notify-support
    (if interactive
	(ert-run-tests-interactively "^file-notify-")
      (ert-run-tests-batch "^file-notify-"))))

(provide 'file-notify-tests)
;;; file-notify-tests.el ends here
