;;; file-notify-tests.el --- Tests of file notifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

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
(defvar file-notify--test-desc nil)
(defvar file-notify--test-results nil)
(defvar file-notify--test-event nil)
(defvar file-notify--test-events nil)
(defun file-notify--test-timeout ()
  (if (file-remote-p temporary-file-directory) 6 3))

(defun file-notify--test-cleanup ()
  "Cleanup after a test."
  (file-notify-rm-watch file-notify--test-desc)

  (when (and file-notify--test-tmpfile
             (file-exists-p file-notify--test-tmpfile))
    (if (directory-name-p file-notify--test-tmpfile)
        (delete-directory file-notify--test-tmpfile 'recursive)
      (delete-file file-notify--test-tmpfile)))
  (when (and file-notify--test-tmpfile1
             (file-exists-p file-notify--test-tmpfile1))
    (if (directory-name-p file-notify--test-tmpfile1)
        (delete-directory file-notify--test-tmpfile1 'recursive)
      (delete-file file-notify--test-tmpfile1)))
  (when (file-remote-p temporary-file-directory)
    (tramp-cleanup-connection
     (tramp-dissect-file-name temporary-file-directory) nil 'keep-password))

  (setq file-notify--test-tmpfile nil)
  (setq file-notify--test-tmpfile1 nil)
  (setq file-notify--test-desc nil)
  (setq file-notify--test-results nil)
  (setq file-notify--test-events nil)
  (when file-notify--test-event
    (error "file-notify--test-event should not be set but bound dynamically")))

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
remote case we return always t."
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
      (ignore-errors
        (and
         (file-remote-p file-notify-test-remote-temporary-file-directory)
         (file-directory-p file-notify-test-remote-temporary-file-directory)
         (file-writable-p file-notify-test-remote-temporary-file-directory)
         (setq desc
               (file-notify-add-watch
                file-notify-test-remote-temporary-file-directory
                '(change) 'ignore))))
      (setq file-notify--test-remote-enabled-checked (cons t desc))
      (when desc (file-notify-rm-watch desc))))
  ;; Return result.
  (cdr file-notify--test-remote-enabled-checked))

(defmacro file-notify--deftest-remote (test docstring)
  "Define ert `TEST-remote' for remote files."
  (declare (indent 1))
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
  ;; Report the native library which has been used.
  (if (null (file-remote-p temporary-file-directory))
      (message "Local library: `%s'" file-notify--library)
    (message "Remote command: `%s'"
             (replace-regexp-in-string
              "<[[:digit:]]+>\\'" ""
              (process-name (cdr file-notify--test-remote-enabled-checked)))))
  (should
   (setq file-notify--test-desc
         (file-notify-add-watch temporary-file-directory '(change) 'ignore)))

  ;; Cleanup.
  (file-notify--test-cleanup))

(file-notify--deftest-remote file-notify-test00-availability
  "Test availability of `file-notify' for remote files.")

(ert-deftest file-notify-test01-add-watch ()
  "Check `file-notify-add-watch'."
  (skip-unless (file-notify--test-local-enabled))
  ;; Check, that different valid parameters are accepted.
  (should
   (setq file-notify--test-desc
         (file-notify-add-watch temporary-file-directory '(change) 'ignore)))
  (file-notify-rm-watch file-notify--test-desc)
  (should
   (setq file-notify--test-desc
         (file-notify-add-watch
          temporary-file-directory '(attribute-change) 'ignore)))
  (file-notify-rm-watch file-notify--test-desc)
  (should
   (setq file-notify--test-desc
         (file-notify-add-watch
          temporary-file-directory '(change attribute-change) 'ignore)))
  (file-notify-rm-watch file-notify--test-desc)

  ;; Check error handling.
  (should-error (file-notify-add-watch 1 2 3 4)
                :type 'wrong-number-of-arguments)
  (should
   (equal (should-error
           (file-notify-add-watch 1 2 3))
          '(wrong-type-argument 1)))
  (should
   (equal (should-error
           (file-notify-add-watch temporary-file-directory 2 3))
          '(wrong-type-argument 2)))
  (should
   (equal (should-error
           (file-notify-add-watch temporary-file-directory '(change) 3))
          '(wrong-type-argument 3)))

  ;; Cleanup.
  (file-notify--test-cleanup))

(file-notify--deftest-remote file-notify-test01-add-watch
  "Check `file-notify-add-watch' for remote files.")

(defun file-notify--test-event-test ()
  "Ert test function to be called by `file-notify--test-event-handler'.
We cannot pass arguments, so we assume that `file-notify--test-event'
is bound somewhere."
  ;;(message "Event %S" file-notify--test-event)
  ;; Check the descriptor.
  (should (equal (car file-notify--test-event) file-notify--test-desc))
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

(defun file-notify--test-event-handler (event)
  "Run a test over FILE-NOTIFY--TEST-EVENT.
For later analysis, append the test result to `file-notify--test-results'
and the event to `file-notify--test-events'."
  (let* ((file-notify--test-event event)
         (result
          (ert-run-test (make-ert-test :body 'file-notify--test-event-test))))
    (setq file-notify--test-events
          (append file-notify--test-events `(,file-notify--test-event)))
    (setq file-notify--test-results
	  (append file-notify--test-results `(,result)))))

(defun file-notify--test-make-temp-name ()
  "Create a temporary file name for test."
  (expand-file-name
   (make-temp-name "file-notify-test") temporary-file-directory))

(defmacro file-notify--wait-for-events (timeout until)
  "Wait for and return file notification events until form UNTIL is true.
TIMEOUT is the maximum time to wait for, in seconds."
  `(with-timeout (,timeout (ignore))
     (while (null ,until)
       (read-event nil nil 0.1))))

(defmacro file-notify--test-with-events (timeout events &rest body)
  "Run BODY collecting events and then compare with EVENTS.
Don't wait longer than TIMEOUT seconds for the events to be delivered."
  (declare (indent 2))
  (let ((outer (make-symbol "outer")))
    `(let ((,outer file-notify--test-events))
       (let (file-notify--test-events)
         ,@body
         (file-notify--wait-for-events
          ,timeout (= (length ,events) (length file-notify--test-events)))
         (should (equal ,events (mapcar #'cadr file-notify--test-events)))
         (setq ,outer (append ,outer file-notify--test-events)))
       (setq file-notify--test-events ,outer))))

(ert-deftest file-notify-test02-events ()
  "Check file creation/change/removal notifications."
  (skip-unless (file-notify--test-local-enabled))
  (unwind-protect
      (progn
        ;; Check creation, change, and deletion.
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
              file-notify--test-tmpfile1 (file-notify--test-make-temp-name)
              file-notify--test-desc
              (file-notify-add-watch
               file-notify--test-tmpfile
               '(change) 'file-notify--test-event-handler))
        (file-notify--test-with-events
            (file-notify--test-timeout) '(created changed deleted)
          (write-region
           "any text" nil file-notify--test-tmpfile nil 'no-message)
          (delete-file file-notify--test-tmpfile))
        (file-notify-rm-watch file-notify--test-desc)

        ;; Check copy.
        (setq file-notify--test-desc
              (file-notify-add-watch
               file-notify--test-tmpfile
               '(change) 'file-notify--test-event-handler))
        (should file-notify--test-desc)
        (file-notify--test-with-events
            (file-notify--test-timeout)
            ;; w32notify does not distinguish between `changed' and
            ;; `attribute-changed'.
            (if (eq file-notify--library 'w32notify)
                '(created changed changed deleted)
              '(created changed deleted))
          (write-region
           "any text" nil file-notify--test-tmpfile nil 'no-message)
          (copy-file file-notify--test-tmpfile file-notify--test-tmpfile1)
          ;; The next two events shall not be visible.
          (set-file-modes file-notify--test-tmpfile 000)
          (read-event nil nil 0.1) ; In order to distinguish the events.
          (set-file-times file-notify--test-tmpfile '(0 0))
          (delete-file file-notify--test-tmpfile)
          (delete-file file-notify--test-tmpfile1))
        (file-notify-rm-watch file-notify--test-desc)

        ;; Check rename.
        (setq file-notify--test-desc
              (file-notify-add-watch
               file-notify--test-tmpfile
               '(change) 'file-notify--test-event-handler))
        (should file-notify--test-desc)
        (file-notify--test-with-events
            (file-notify--test-timeout) '(created changed renamed)
          (write-region
           "any text" nil file-notify--test-tmpfile nil 'no-message)
          (rename-file file-notify--test-tmpfile file-notify--test-tmpfile1)
          ;; After the rename, we won't get events anymore.
          (delete-file file-notify--test-tmpfile1))
        (file-notify-rm-watch file-notify--test-desc)

        ;; Check attribute change.  It doesn't work for w32notify.
        (unless (eq file-notify--library 'w32notify)
          (setq file-notify--test-desc
                (file-notify-add-watch
                 file-notify--test-tmpfile
                 '(attribute-change) 'file-notify--test-event-handler))
          (file-notify--test-with-events
              (file-notify--test-timeout) '(attribute-changed attribute-changed)
            (write-region
             "any text" nil file-notify--test-tmpfile nil 'no-message)
            (set-file-modes file-notify--test-tmpfile 000)
            (read-event nil nil 0.1) ; In order to distinguish the events.
            (set-file-times file-notify--test-tmpfile '(0 0))
            (delete-file file-notify--test-tmpfile))
          (file-notify-rm-watch file-notify--test-desc))

        ;; Check the global sequence again just to make sure that
        ;; `file-notify--test-events' has been set correctly.
        (should (equal (mapcar #'cadr file-notify--test-events)
                       (if (eq file-notify--library 'w32notify)
                           '(created changed deleted
                                     created changed changed deleted
                                     created changed renamed)
                         '(created changed deleted
                                   created changed deleted
                                   created changed renamed
                                   attribute-changed attribute-changed))))
        (should file-notify--test-results)
        (dolist (result file-notify--test-results)
          ;;(message "%s" (ert-test-result-messages result))
          (when (ert-test-failed-p result)
            (ert-fail
             (cadr (ert-test-result-with-condition-condition result))))))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test02-events
  "Check file creation/change/removal notifications for remote files.")

(require 'autorevert)
(setq auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-remote-files t
      auto-revert-stop-on-user-input nil)

(ert-deftest file-notify-test03-autorevert ()
  "Check autorevert via file notification."
  (skip-unless (file-notify--test-local-enabled))
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((timeout (if (file-remote-p temporary-file-directory) 60 10))
        buf)
    (unwind-protect
	(progn
	  (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))

	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (setq buf (find-file-noselect file-notify--test-tmpfile))
	  (with-current-buffer buf
	    (should (string-equal (buffer-string) "any text"))
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
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
            (write-region
             "another text" nil file-notify--test-tmpfile nil 'no-message)

	    ;; Check, that the buffer has been reverted.
	    (with-current-buffer (get-buffer-create "*Messages*")
	      (file-notify--wait-for-events
	       timeout
	       (string-match
                (format-message "Reverting buffer `%s'." (buffer-name buf))
                (buffer-string))))
	    (should (string-match "another text" (buffer-string)))))

      ;; Cleanup.
      (ignore-errors (kill-buffer buf))
      (file-notify--test-cleanup))))

(file-notify--deftest-remote file-notify-test03-autorevert
  "Check autorevert via file notification for remote files.")

(ert-deftest file-notify-test04-file-validity ()
  "Check `file-notify-valid-p' for files."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
        (setq file-notify--test-desc
              (file-notify-add-watch
               file-notify--test-tmpfile
               '(change) #'file-notify--test-event-handler))
        (file-notify--test-with-events
            (file-notify--test-timeout) '(created changed)
          (should (file-notify-valid-p file-notify--test-desc))
          (write-region
           "any text" nil file-notify--test-tmpfile nil 'no-message)
          (should (file-notify-valid-p file-notify--test-desc)))
        ;; After removing the watch, the descriptor must not be valid
        ;; anymore.
        (file-notify-rm-watch file-notify--test-desc)
        (should-not (file-notify-valid-p file-notify--test-desc)))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; The batch-mode operation of w32notify is fragile (there's no
      ;; input threads to send the message to).
      (unless (and noninteractive (eq file-notify--library 'w32notify))
        (let ((temporary-file-directory (make-temp-file
                                         "file-notify-test-parent" t)))
          (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
          (setq file-notify--test-desc
                (file-notify-add-watch
                 file-notify--test-tmpfile
                 '(change) #'file-notify--test-event-handler))
          (file-notify--test-with-events
              (file-notify--test-timeout) '(created changed)
            (should (file-notify-valid-p file-notify--test-desc))
            (write-region
             "any text" nil file-notify--test-tmpfile nil 'no-message)
            (should (file-notify-valid-p file-notify--test-desc)))
          ;; After deleting the parent, the descriptor must not be valid
          ;; anymore.
          (delete-directory temporary-file-directory t)
          (file-notify--wait-for-events
           (file-notify--test-timeout)
           (not (file-notify-valid-p file-notify--test-desc)))
          (should-not (file-notify-valid-p file-notify--test-desc))))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test04-file-validity
  "Check `file-notify-valid-p' via file notification for remote files.")

(ert-deftest file-notify-test05-dir-validity ()
  "Check `file-notify-valid-p' for directories."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
        (setq file-notify--test-tmpfile (file-name-as-directory
                                         (file-notify--test-make-temp-name)))
        (make-directory file-notify--test-tmpfile)
        (setq file-notify--test-desc
              (file-notify-add-watch
               file-notify--test-tmpfile
               '(change) #'file-notify--test-event-handler))
        (should (file-notify-valid-p file-notify--test-desc))
        ;; After removing the watch, the descriptor must not be valid
        ;; anymore.
        (file-notify-rm-watch file-notify--test-desc)
        (should-not (file-notify-valid-p file-notify--test-desc)))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; The batch-mode operation of w32notify is fragile (there's no
      ;; input threads to send the message to).
      (unless (and noninteractive (eq file-notify--library 'w32notify))
        (setq file-notify--test-tmpfile (file-name-as-directory
                                         (file-notify--test-make-temp-name)))
        (make-directory file-notify--test-tmpfile)
        (setq file-notify--test-desc
              (file-notify-add-watch
               file-notify--test-tmpfile
               '(change) #'file-notify--test-event-handler))
        (should (file-notify-valid-p file-notify--test-desc))
        ;; After deleting the directory, the descriptor must not be
        ;; valid anymore.
        (delete-directory file-notify--test-tmpfile t)
        (file-notify--wait-for-events
         (file-notify--test-timeout)
	 (not (file-notify-valid-p file-notify--test-desc)))
        (should-not (file-notify-valid-p file-notify--test-desc)))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test05-dir-validity
  "Check `file-notify-valid-p' via file notification for remote directories.")

(defun file-notify-test-all (&optional interactive)
  "Run all tests for \\[file-notify]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^file-notify-")
    (ert-run-tests-batch "^file-notify-")))

(provide 'file-notify-tests)
;;; file-notify-tests.el ends here
