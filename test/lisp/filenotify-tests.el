;;; filenotify-tests.el --- Tests of file notifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

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

;; For the remote file-notify library, Tramp checks for the existence
;; of a respective command.  The first command found is used.  In
;; order to use a dedicated one, the environment variable
;; $REMOTE_FILE_NOTIFY_LIBRARY shall be set, possible values are
;; "inotifywait", "gio-monitor" and "gvfs-monitor-dir".

;; Local file-notify libraries are auto-detected during Emacs
;; configuration.  This can be changed with a respective configuration
;; argument, like
;;
;;   --with-file-notification=inotify
;;   --with-file-notification=kqueue
;;   --with-file-notification=gfile
;;   --with-file-notification=w32

;; A whole test run can be performed calling the command `file-notify-test-all'.

;;; Code:

(require 'ert)
(require 'ert-x)
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
      (add-to-list
       'tramp-default-host-alist
       `("\\`mock\\'" nil ,(system-name)))
      ;; Emacs' Makefile sets $HOME to a nonexistent value.  Needed in
      ;; batch mode only, therefore.  `temporary-file-directory' might
      ;; be quoted, so we unquote it just in case.
      (unless (and (null noninteractive) (file-directory-p "~/"))
        (setenv "HOME" (file-name-unquote temporary-file-directory)))
      (format "/mock::%s" temporary-file-directory)))
  "Temporary directory for Tramp tests.")

;; Filter suppressed remote file-notify libraries.
(when (stringp (getenv "REMOTE_FILE_NOTIFY_LIBRARY"))
  (dolist (lib '("inotifywait" "gio-monitor" "gvfs-monitor-dir"))
    (unless (string-equal (getenv "REMOTE_FILE_NOTIFY_LIBRARY") lib)
      (add-to-list 'tramp-connection-properties `(nil ,lib nil)))))

(defvar file-notify--test-tmpdir nil)
(defvar file-notify--test-tmpfile nil)
(defvar file-notify--test-tmpfile1 nil)
(defvar file-notify--test-desc nil)
(defvar file-notify--test-desc1 nil)
(defvar file-notify--test-desc2 nil)
(defvar file-notify--test-results nil)
(defvar file-notify--test-event nil)
(defvar file-notify--test-file nil)
(defvar file-notify--test-events nil)
(defvar file-notify--test-monitors nil)

(defun file-notify--test-read-event ()
  "Read one event.
There are different timeouts for local and remote file notification libraries."
  (read-event
   nil nil
   (cond
    ;; gio/gpollfilemonitor.c declares POLL_TIME_SECS 5.  So we must
    ;; wait at least this time in the GPollFileMonitor case.  A
    ;; similar timeout seems to be needed in the GFamFileMonitor case,
    ;; at least on Cygwin.
    ((and (string-equal (file-notify--test-library) "gfilenotify")
          (memq (file-notify--test-monitor)
                '(GFamFileMonitor GPollFileMonitor)))
     7)
    ((string-equal (file-notify--test-library) "gvfs-monitor-dir.exe") 1)
    ((file-remote-p temporary-file-directory) 0.1)
    (t 0.01))))

(defun file-notify--test-timeout ()
  "Timeout to wait for arriving a bunch of events, in seconds."
  (cond
   ((file-remote-p temporary-file-directory) 6)
   ((string-equal (file-notify--test-library) "w32notify") 4)
   ((eq system-type 'cygwin) 6)
   (t 3)))

(defmacro file-notify--test-wait-for-events (timeout until)
  "Wait for and return file notification events until form UNTIL is true.
TIMEOUT is the maximum time to wait for, in seconds."
  `(with-timeout (,timeout (ignore))
     (while (null ,until)
       (file-notify--test-read-event))))

(defun file-notify--test-no-descriptors ()
  "Check that `file-notify-descriptors' is an empty hash table.
Return nil when any other file notification watch is still active."
  ;; Give read events a last chance.
  (file-notify--test-wait-for-events
   (file-notify--test-timeout)
   (zerop (hash-table-count file-notify-descriptors)))
  ;; Now check.
  (zerop (hash-table-count file-notify-descriptors)))

(defun file-notify--test-no-descriptors-explainer ()
  "Explain why `file-notify--test-no-descriptors' fails."
  (let ((result (list "Watch descriptor(s) existent:")))
    (maphash
     (lambda (key value) (push (cons key value) result))
     file-notify-descriptors)
    (nreverse result)))

(put 'file-notify--test-no-descriptors 'ert-explainer
     'file-notify--test-no-descriptors-explainer)

(defun file-notify--test-cleanup-p ()
  "Check, that the test has cleaned up the environment as much as needed."
  ;; `file-notify--test-event' should not be set but bound
  ;; dynamically.
  (should-not file-notify--test-event)
  ;; The test should have cleaned up this already.  Let's check
  ;; nevertheless.
  (should (file-notify--test-no-descriptors)))

(defun file-notify--test-cleanup ()
  "Cleanup after a test."
  (file-notify-rm-watch file-notify--test-desc)
  (file-notify-rm-watch file-notify--test-desc1)
  (file-notify-rm-watch file-notify--test-desc2)

  (ignore-errors
    (delete-file (file-newest-backup file-notify--test-tmpfile)))
  (ignore-errors
    (if (file-directory-p file-notify--test-tmpfile)
        (delete-directory file-notify--test-tmpfile 'recursive)
      (delete-file file-notify--test-tmpfile)))
  (ignore-errors
    (if (file-directory-p file-notify--test-tmpfile1)
        (delete-directory file-notify--test-tmpfile1 'recursive)
      (delete-file file-notify--test-tmpfile1)))
  (ignore-errors
    (delete-directory file-notify--test-tmpdir 'recursive))
  (ignore-errors
    (when (file-remote-p temporary-file-directory)
      (tramp-cleanup-connection
       (tramp-dissect-file-name temporary-file-directory) nil 'keep-password)))

  (when (hash-table-p file-notify-descriptors)
    (clrhash file-notify-descriptors))

  (setq file-notify--test-tmpdir nil
        file-notify--test-tmpfile nil
        file-notify--test-tmpfile1 nil
        file-notify--test-desc nil
        file-notify--test-desc1 nil
        file-notify--test-desc2 nil
        file-notify--test-results nil
        file-notify--test-event nil
        file-notify--test-file nil
        file-notify--test-events nil
        file-notify--test-monitors nil))

(setq file-notify-debug nil
      password-cache-expiry nil
      tramp-verbose 0
      tramp-message-show-message nil)

;; This should happen on hydra only.
(when (getenv "EMACS_HYDRA_CI")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun file-notify--test-add-watch (file flags callback)
  "Like `file-notify-add-watch', but also passing FILE to CALLBACK."
  (file-notify-add-watch file flags
                         (lambda (event) (funcall callback event file))))

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
                '(change) #'ignore))))
      (setq file-notify--test-remote-enabled-checked (cons t desc))
      (when desc (file-notify-rm-watch desc))))
  ;; Return result.
  (cdr file-notify--test-remote-enabled-checked))

(defun file-notify--test-library ()
  "The used library for the test, as a string.
In the remote case, it is the process name which runs on the
remote host, or nil."
  (if (null (file-remote-p temporary-file-directory))
      (symbol-name file-notify--library)
    (and (consp file-notify--test-remote-enabled-checked)
	 (processp (cdr file-notify--test-remote-enabled-checked))
	 (replace-regexp-in-string
	  "<[[:digit:]]+>\\'" ""
	  (process-name (cdr file-notify--test-remote-enabled-checked))))))

(defun file-notify--test-monitor ()
  "The used monitor for the test, as a symbol.
This returns only for the local case and gfilenotify; otherwise it is nil.
`file-notify--test-desc' must be a valid watch descriptor."
  ;; We cache the result, because after `file-notify-rm-watch',
  ;; `gfile-monitor-name' does not return a proper result anymore.
  ;; But we still need this information.
  (unless (file-remote-p temporary-file-directory)
    (or (cdr (assq file-notify--test-desc file-notify--test-monitors))
        (when (functionp 'gfile-monitor-name)
          (add-to-list 'file-notify--test-monitors
                       (cons file-notify--test-desc
                             (gfile-monitor-name file-notify--test-desc)))
          (cdr (assq file-notify--test-desc file-notify--test-monitors))))))

(defmacro file-notify--deftest-remote (test docstring &optional unstable)
  "Define ert `TEST-remote' for remote files.
If UNSTABLE is non-nil, the test is tagged as `:unstable'."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
     ,docstring
     :tags (if ,unstable '(:expensive-test :unstable) '(:expensive-test))
     (let* ((temporary-file-directory
	     file-notify-test-remote-temporary-file-directory)
	    (ert-test (ert-get-test ',test))
            vc-handled-backends)
       (skip-unless (file-notify--test-remote-enabled))
       (tramp-cleanup-connection
	(tramp-dissect-file-name temporary-file-directory) nil 'keep-password)
       (funcall (ert-test-body ert-test)))))

(ert-deftest file-notify-test00-availability ()
  "Test availability of `file-notify'."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
        ;; Report the native library which has been used.
        (message "Library: `%s'" (file-notify--test-library))
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                temporary-file-directory '(change) #'ignore)))
        (when (file-notify--test-monitor)
          (message "Monitor: `%s'" (file-notify--test-monitor)))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test00-availability
  "Test availability of `file-notify' for remote files.")

(defun file-notify--test-make-temp-name ()
  "Create a temporary file name for test."
  (unless (stringp file-notify--test-tmpdir)
    (setq file-notify--test-tmpdir
          (expand-file-name
           (make-temp-name "file-notify-test") temporary-file-directory)))
  (unless (file-directory-p file-notify--test-tmpdir)
    (make-directory file-notify--test-tmpdir))
  (expand-file-name
   (make-temp-name "file-notify-test") file-notify--test-tmpdir))

(ert-deftest file-notify-test01-add-watch ()
  "Check `file-notify-add-watch'."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
        (setq file-notify--test-tmpfile  (file-notify--test-make-temp-name)
              file-notify--test-tmpfile1
              (format
               "%s/%s" file-notify--test-tmpfile (md5 (current-time-string))))

        ;; Check, that different valid parameters are accepted.
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                file-notify--test-tmpdir '(change) #'ignore)))
        (file-notify-rm-watch file-notify--test-desc)
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                file-notify--test-tmpdir '(attribute-change) #'ignore)))
        (file-notify-rm-watch file-notify--test-desc)
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                file-notify--test-tmpdir '(change attribute-change) #'ignore)))
        (file-notify-rm-watch file-notify--test-desc)

        ;; File monitors like kqueue insist, that the watched file
        ;; exists.  Directory monitors are not bound to this
        ;; restriction.
        (when (string-equal (file-notify--test-library) "kqueue")
          (write-region
           "any text" nil file-notify--test-tmpfile nil 'no-message))
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                file-notify--test-tmpfile '(change attribute-change) #'ignore)))
        (file-notify-rm-watch file-notify--test-desc)
        (when (string-equal (file-notify--test-library) "kqueue")
          (delete-file file-notify--test-tmpfile))

        ;; Check error handling.
        (should-error (file-notify-add-watch 1 2 3 4)
                      :type 'wrong-number-of-arguments)
        (should
         (equal (should-error
                 (file-notify-add-watch 1 2 3))
                '(wrong-type-argument 1)))
        (should
         (equal (should-error
                 (file-notify-add-watch file-notify--test-tmpdir 2 3))
                '(wrong-type-argument 2)))
        (should
         (equal (should-error
                 (file-notify-add-watch file-notify--test-tmpdir '(change) 3))
                '(wrong-type-argument 3)))
        ;; The upper directory of a file must exist.
        (should
         (equal (should-error
                 (file-notify-add-watch
                  file-notify--test-tmpfile1
                  '(change attribute-change) #'ignore))
                `(file-notify-error
                  "Directory does not exist" ,file-notify--test-tmpfile)))

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test01-add-watch
  "Check `file-notify-add-watch' for remote files.")

;; This test is inspired by Bug#26126 and Bug#26127.
(ert-deftest file-notify-test02-rm-watch ()
  "Check `file-notify-rm-watch'."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      ;; Check, that `file-notify-rm-watch' works.
      (progn
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                temporary-file-directory '(change) #'ignore)))
        (file-notify-rm-watch file-notify--test-desc)
        ;; Check, that any parameter is accepted.
        (condition-case err
            (progn
              (file-notify-rm-watch nil)
              (file-notify-rm-watch 0)
              (file-notify-rm-watch "foo")
              (file-notify-rm-watch 'foo))
          (error (ert-fail err)))

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; Check, that no error is returned removing a watch descriptor twice.
      (progn
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
              file-notify--test-tmpfile1 (file-notify--test-make-temp-name))
        (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
        (write-region "any text" nil file-notify--test-tmpfile1 nil 'no-message)
        (should
         (setq file-notify--test-desc
               (file-notify-add-watch
                file-notify--test-tmpfile '(change) #'ignore)))
        (should
         (setq file-notify--test-desc1
               (file-notify-add-watch
                file-notify--test-tmpfile1 '(change) #'ignore)))
        ;; Remove `file-notify--test-desc' twice.
        (file-notify-rm-watch file-notify--test-desc)
        (file-notify-rm-watch file-notify--test-desc)
        (file-notify-rm-watch file-notify--test-desc1)
        (delete-file file-notify--test-tmpfile)
        (delete-file file-notify--test-tmpfile1)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; Check, that removing watch descriptors out of order do not
      ;; harm.  This fails on Cygwin because of timing issues unless a
      ;; long `sit-for' is added before the call to
      ;; `file-notify--test-read-event'.
      (unless (eq system-type 'cygwin)
        (let (results)
          (cl-flet ((first-callback (event)
                     (when (eq (file-notify--test-event-action event) 'deleted)
                       (push 1 results)))
                    (second-callback (event)
                     (when (eq (file-notify--test-event-action event) 'deleted)
                       (push 2 results))))
            (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
            (write-region
             "any text" nil file-notify--test-tmpfile nil 'no-message)
            (should
             (setq file-notify--test-desc
                   (file-notify-add-watch
                    file-notify--test-tmpfile
                    '(change) #'first-callback)))
            (should
             (setq file-notify--test-desc1
                   (file-notify-add-watch
                    file-notify--test-tmpfile
                    '(change) #'second-callback)))
            ;; Remove first watch.
            (file-notify-rm-watch file-notify--test-desc)
            ;; Only the second callback shall run.
	    (file-notify--test-read-event)
            (delete-file file-notify--test-tmpfile)
            (file-notify--test-wait-for-events
             (file-notify--test-timeout) results)
            (should (equal results (list 2)))

            ;; The environment shall be cleaned up.
            (file-notify--test-cleanup-p))))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test02-rm-watch
  "Check `file-notify-rm-watch' for remote files.")

;; Accessors for the callback argument.
(defun file-notify--test-event-desc (event) (car event))
(defun file-notify--test-event-action (event) (nth 1 event))
(defun file-notify--test-event-file (event) (nth 2 event))
(defun file-notify--test-event-file1 (event) (nth 3 event))

(defun file-notify--test-event-test ()
  "Ert test function to be called by `file-notify--test-event-handler'.
We cannot pass arguments, so we assume that `file-notify--test-event'
and `file-notify--test-file' are bound somewhere."
  ;; Check the descriptor.
  (should (equal (file-notify--test-event-desc file-notify--test-event)
                 file-notify--test-desc))
  ;; Check the file name.
  (should
   (string-prefix-p
    file-notify--test-file
    (file-notify--test-event-file file-notify--test-event)))
  ;; Check the second file name if exists.
  (when (eq (file-notify--test-event-action file-notify--test-event) 'renamed)
    (should
     (string-prefix-p
      file-notify--test-file
      (file-notify--test-event-file1 file-notify--test-event)))))

(defun file-notify--test-event-handler (event file)
  "Run a test over FILE-NOTIFY--TEST-EVENT.
For later analysis, append the test result to `file-notify--test-results'
and the event to `file-notify--test-events'."
  (let* ((file-notify--test-event event)
         (file-notify--test-file file)
         (result
          (ert-run-test (make-ert-test :body 'file-notify--test-event-test))))
    ;; Do not add lock files, this would confuse the checks.
    (unless (string-match
	     (regexp-quote ".#")
	     (file-notify--test-event-file file-notify--test-event))
      (when file-notify-debug
        (message "file-notify--test-event-handler result: %s event: %S"
                 (null (ert-test-failed-p result)) file-notify--test-event))
      (setq file-notify--test-events
	    (append file-notify--test-events `(,file-notify--test-event))
	    file-notify--test-results
	    (append file-notify--test-results `(,result))))))

(defun file-notify--test-with-actions-check (actions)
  "Check whether received actions match one of the ACTIONS alternatives."
  (let (result)
    (dolist (elt actions result)
      (setq result
            (or result
                (if (eq (car elt) :random)
                    (equal (sort (cdr elt) 'string-lessp)
                           (sort (mapcar #'file-notify--test-event-action
                                         file-notify--test-events)
                                 'string-lessp))
                  (equal elt (mapcar #'file-notify--test-event-action
                                     file-notify--test-events))))))))

(defun file-notify--test-with-actions-explainer (actions)
  "Explain why `file-notify--test-with-actions-check' fails."
  (if (null (cdr actions))
      (format "Received actions do not match expected actions\n%s\n%s"
              (mapcar #'file-notify--test-event-action file-notify--test-events)
              (car actions))
    (format
     "Received actions do not match any sequence of expected actions\n%s\n%s"
     (mapcar #'file-notify--test-event-action file-notify--test-events)
     actions)))

(put 'file-notify--test-with-actions-check 'ert-explainer
     'file-notify--test-with-actions-explainer)

(defmacro file-notify--test-with-actions (actions &rest body)
  "Run BODY collecting actions and then compare with ACTIONS.
ACTIONS is either a simple list of actions, or a list of lists of
actions, which represent different possible results.  The first
event of a list could be the pseudo event `:random', which is
just an indicator for comparison.

Don't wait longer than timeout seconds for the actions to be
delivered."
  (declare (indent 1) (debug (form body)))
  `(let* ((actions (if (consp (car ,actions)) ,actions (list ,actions)))
          (max-length
           (apply
            'max
            (mapcar
             (lambda (x) (length (if (eq (car x) :random) (cdr x) x)))
             actions)))
          create-lockfiles)
     ;; Flush pending actions.
     (file-notify--test-read-event)
     (file-notify--test-wait-for-events
      (file-notify--test-timeout)
      (not (input-pending-p)))
     (setq file-notify--test-events nil
           file-notify--test-results nil)
     ,@body
     (file-notify--test-wait-for-events
      ;; More actions need more time.  Use some fudge factor.
      (* (ceiling max-length 100) (file-notify--test-timeout))
      (= max-length (length file-notify--test-events)))
     ;; Check the result sequence just to make sure that all actions
     ;; are as expected.
     (dolist (result file-notify--test-results)
       (when (ert-test-failed-p result)
         (ert-fail
          (cadr (ert-test-result-with-condition-condition result)))))
     ;; One of the possible event sequences shall match.
     (should (file-notify--test-with-actions-check actions))))

(ert-deftest file-notify-test03-events ()
  "Check file creation/change/removal notifications."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      ;; Check file creation, change and deletion.  It doesn't work
      ;; for kqueue, because we don't use an implicit directory
      ;; monitor.
      (unless (string-equal (file-notify--test-library) "kqueue")
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
        (should
         (setq file-notify--test-desc
               (file-notify--test-add-watch
                file-notify--test-tmpfile
                '(change) #'file-notify--test-event-handler)))
        (file-notify--test-with-actions
            (cond
             ;; gvfs-monitor-dir on cygwin does not detect the
             ;; `created' event reliably.
	     ((string-equal
	       (file-notify--test-library) "gvfs-monitor-dir.exe")
	      '((deleted stopped)
	        (created deleted stopped)))
             ;; cygwin does not raise a `changed' event.
             ((eq system-type 'cygwin)
              '(created deleted stopped))
             (t '(created changed deleted stopped)))
          (write-region
           "another text" nil file-notify--test-tmpfile nil 'no-message)
          (file-notify--test-read-event)
          (delete-file file-notify--test-tmpfile))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      (progn
        ;; Check file change and deletion.
	(setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
        (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
	(should
	 (setq file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpfile
		'(change) #'file-notify--test-event-handler)))
        (file-notify--test-with-actions
	    (cond
             ;; gvfs-monitor-dir on cygwin does not detect the
             ;; `changed' event reliably.
	     ((string-equal (file-notify--test-library) "gvfs-monitor-dir.exe")
	      '((deleted stopped)
		(changed deleted stopped)))
	     ;; There could be one or two `changed' events.
	     (t '((changed deleted stopped)
		  (changed changed deleted stopped))))
          (write-region
           "another text" nil file-notify--test-tmpfile nil 'no-message)
          (file-notify--test-read-event)
          (delete-file file-notify--test-tmpfile))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; Check file creation, change and deletion when watching a
      ;; directory.  There must be a `stopped' event when deleting the
      ;; directory.
      (let ((file-notify--test-tmpdir
             (make-temp-file "file-notify-test-parent" t)))
	(should
	 (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
	       file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpdir
		'(change) #'file-notify--test-event-handler)))
	(file-notify--test-with-actions
	    (cond
	     ;; w32notify does not raise `deleted' and `stopped'
	     ;; events for the watched directory.
	     ((string-equal (file-notify--test-library) "w32notify")
	      '(created changed deleted))
             ;; gvfs-monitor-dir on cygwin does not detect the
             ;; `created' event reliably.
	     ((string-equal
	       (file-notify--test-library) "gvfs-monitor-dir.exe")
	      '((deleted stopped)
		(created deleted stopped)))
             ;; On emba, `deleted' and `stopped' events of the
             ;; directory are not detected.
             ((getenv "EMACS_EMBA_CI")
              '(created changed deleted))
	     ;; There are two `deleted' events, for the file and for
	     ;; the directory.  Except for cygwin and kqueue.  And
	     ;; cygwin does not raise a `changed' event.
	     ((eq system-type 'cygwin)
	      '(created deleted stopped))
	     ((string-equal (file-notify--test-library) "kqueue")
	      '(created changed deleted stopped))
	     (t '(created changed deleted deleted stopped)))
	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (file-notify--test-read-event)
          (delete-directory file-notify--test-tmpdir 'recursive))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; Check copy of files inside a directory.
      (let ((file-notify--test-tmpdir
	     (make-temp-file "file-notify-test-parent" t)))
	(should
	 (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
	       file-notify--test-tmpfile1 (file-notify--test-make-temp-name)
	       file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpdir
		'(change) #'file-notify--test-event-handler)))
	(file-notify--test-with-actions
	    (cond
	     ;; w32notify does not distinguish between `changed' and
	     ;; `attribute-changed'.  It does not raise `deleted' and
	     ;; `stopped' events for the watched directory.
	     ((string-equal (file-notify--test-library) "w32notify")
	      '(created changed created changed
		changed changed changed
		deleted deleted))
             ;; gvfs-monitor-dir on cygwin does not detect the
             ;; `created' event reliably.
	     ((string-equal
	       (file-notify--test-library) "gvfs-monitor-dir.exe")
	      '((deleted stopped)
		(created created deleted stopped)))
	     ;; There are three `deleted' events, for two files and
	     ;; for the directory.  Except for cygwin and kqueue.
	     ((eq system-type 'cygwin)
	      '(created created changed changed deleted stopped))
	     ((string-equal (file-notify--test-library) "kqueue")
	      '(created changed created changed deleted stopped))
             ;; On emba, `deleted' and `stopped' events of the
             ;; directory are not detected.
             ((getenv "EMACS_EMBA_CI")
              '(created changed created changed deleted deleted))
	     (t '(created changed created changed
		  deleted deleted deleted stopped)))
	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (file-notify--test-read-event)
	  (copy-file file-notify--test-tmpfile file-notify--test-tmpfile1)
	  ;; The next two events shall not be visible.
	  (file-notify--test-read-event)
	  (set-file-modes file-notify--test-tmpfile 000)
	  (file-notify--test-read-event)
	  (set-file-times file-notify--test-tmpfile '(0 0))
	  (file-notify--test-read-event)
          (delete-directory file-notify--test-tmpdir 'recursive))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; Check rename of files inside a directory.
      (let ((file-notify--test-tmpdir
	     (make-temp-file "file-notify-test-parent" t)))
	(should
	 (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
	       file-notify--test-tmpfile1 (file-notify--test-make-temp-name)
	       file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpdir
		'(change) #'file-notify--test-event-handler)))
	(file-notify--test-with-actions
	    (cond
	     ;; w32notify does not raise `deleted' and `stopped'
	     ;; events for the watched directory.
	     ((string-equal (file-notify--test-library) "w32notify")
	      '(created changed renamed deleted))
             ;; gvfs-monitor-dir on cygwin does not detect the
             ;; `created' event reliably.
	     ((string-equal
	       (file-notify--test-library) "gvfs-monitor-dir.exe")
	      '((deleted stopped)
		(created deleted stopped)))
             ;; On emba, `deleted' and `stopped' events of the
             ;; directory are not detected.
             ((getenv "EMACS_EMBA_CI")
              '(created changed renamed deleted))
	     ;; There are two `deleted' events, for the file and for
	     ;; the directory.  Except for cygwin and kqueue.  And
	     ;; cygwin raises `created' and `deleted' events instead
	     ;; of a `renamed' event.
	     ((eq system-type 'cygwin)
	      '(created created deleted deleted stopped))
	     ((string-equal (file-notify--test-library) "kqueue")
	      '(created changed renamed deleted stopped))
	     (t '(created changed renamed deleted deleted stopped)))
	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (file-notify--test-read-event)
	  (rename-file file-notify--test-tmpfile file-notify--test-tmpfile1)
	  ;; After the rename, we won't get events anymore.
	  (file-notify--test-read-event)
          (delete-directory file-notify--test-tmpdir 'recursive))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; Check attribute change.  Does not work for cygwin.
      (unless (eq system-type 'cygwin)
	(setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
	(write-region
	 "any text" nil file-notify--test-tmpfile nil 'no-message)
	(should
	 (setq file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpfile
		'(attribute-change) #'file-notify--test-event-handler)))
	(file-notify--test-with-actions
	    (cond
	     ;; w32notify does not distinguish between `changed' and
	     ;; `attribute-changed'.  Under MS Windows 7, we get four
	     ;; `changed' events, and under MS Windows 10 just two.
	     ;; Strange.
	     ((string-equal (file-notify--test-library) "w32notify")
	      '((changed changed)
		(changed changed changed changed)))
	     ;; For kqueue and in the remote case, `write-region'
	     ;; raises also an `attribute-changed' event.
	     ((or (string-equal (file-notify--test-library) "kqueue")
		  (file-remote-p temporary-file-directory))
	      '(attribute-changed attribute-changed attribute-changed))
	     (t '(attribute-changed attribute-changed)))
	  (write-region
	   "any text" nil file-notify--test-tmpfile nil 'no-message)
	  (file-notify--test-read-event)
	  (set-file-modes file-notify--test-tmpfile 000)
	  (file-notify--test-read-event)
	  (set-file-times file-notify--test-tmpfile '(0 0))
	  (file-notify--test-read-event)
	  (delete-file file-notify--test-tmpfile))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test03-events
  "Check file creation/change/removal notifications for remote files.")

(require 'autorevert)
(setq auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-remote-files t
      auto-revert-stop-on-user-input nil)

(ert-deftest file-notify-test04-autorevert ()
  "Check autorevert via file notification."
  (skip-unless (file-notify--test-local-enabled))

  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((timeout (if (file-remote-p temporary-file-directory) 60 10))
        buf)
    (unwind-protect
	(progn
          ;; In the remote case, `vc-refresh-state' returns undesired
          ;; error messages.  Let's suppress them.
          (advice-add 'vc-refresh-state :around 'ignore)
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

            ;; `file-notify--test-monitor' needs to know
            ;; `file-notify--test-desc' in order to compute proper
            ;; timeouts.
            (setq file-notify--test-desc auto-revert-notify-watch-descriptor)

	    ;; Check, that file notification has been used.
	    (should auto-revert-mode)
	    (should auto-revert-use-notify)
	    (should auto-revert-notify-watch-descriptor)

	    ;; Modify file.  We wait for a second, in order to have
            ;; another timestamp.
            (ert-with-message-capture captured-messages
              (sleep-for 1)
              (write-region
               "another text" nil file-notify--test-tmpfile nil 'no-message)

              ;; Check, that the buffer has been reverted.
              (file-notify--test-wait-for-events
               timeout
               (string-match
                (format-message "Reverting buffer `%s'." (buffer-name buf))
                captured-messages))
              (should (string-match "another text" (buffer-string))))

            ;; Stop file notification.  Autorevert shall still work via polling.
	    (file-notify-rm-watch auto-revert-notify-watch-descriptor)
	    (file-notify--test-wait-for-events
	     timeout (null auto-revert-notify-watch-descriptor))
	    (should auto-revert-use-notify)
	    (should-not
             (file-notify-valid-p auto-revert-notify-watch-descriptor))

	    ;; Modify file.  We wait for two seconds, in order to
	    ;; have another timestamp.  One second seems to be too
            ;; short.  And Cygwin sporadically requires more than two.
            (ert-with-message-capture captured-messages
              (sleep-for (if (eq system-type 'cygwin) 3 2))
              (write-region
               "foo bla" nil file-notify--test-tmpfile nil 'no-message)

              ;; Check, that the buffer has been reverted.
              (file-notify--test-wait-for-events
               timeout
               (string-match
                (format-message "Reverting buffer `%s'." (buffer-name buf))
                captured-messages))
              (should (string-match "foo bla" (buffer-string))))

            ;; Stop autorevert, in order to cleanup descriptor.
            (auto-revert-mode -1))

          ;; The environment shall be cleaned up.
          (file-notify--test-cleanup-p))

      ;; Cleanup.
      (advice-remove 'vc-refresh-state 'ignore)
      (ignore-errors (kill-buffer buf))
      (file-notify--test-cleanup))))

(file-notify--deftest-remote file-notify-test04-autorevert
  "Check autorevert via file notification for remote files.")

(ert-deftest file-notify-test05-file-validity ()
  "Check `file-notify-valid-p' for files."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
	(write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
	(should
	 (setq file-notify--test-desc
	       (file-notify-add-watch
                file-notify--test-tmpfile '(change) #'ignore)))
        (should (file-notify-valid-p file-notify--test-desc))
	;; After calling `file-notify-rm-watch', the descriptor is not
	;; valid anymore.
        (file-notify-rm-watch file-notify--test-desc)
        (should-not (file-notify-valid-p file-notify--test-desc))
	(delete-file file-notify--test-tmpfile)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      (progn
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
	(write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
	(should
	 (setq file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpfile
		'(change) #'file-notify--test-event-handler)))
	(should (file-notify-valid-p file-notify--test-desc))
        (file-notify--test-with-actions
	    (cond
             ;; gvfs-monitor-dir on cygwin does not detect the
             ;; `changed' event reliably.
	     ((string-equal (file-notify--test-library) "gvfs-monitor-dir.exe")
	      '((deleted stopped)
		(changed deleted stopped)))
	     ;; There could be one or two `changed' events.
	     (t '((changed deleted stopped)
		  (changed changed deleted stopped))))
          (write-region
           "another text" nil file-notify--test-tmpfile nil 'no-message)
	  (file-notify--test-read-event)
	  (delete-file file-notify--test-tmpfile))
	;; After deleting the file, the descriptor is not valid anymore.
        (should-not (file-notify-valid-p file-notify--test-desc))
        (file-notify-rm-watch file-notify--test-desc)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; On emba, `deleted' and `stopped' events of the directory are
      ;; not detected.
      (unless (getenv "EMACS_EMBA_CI")
        (let ((file-notify--test-tmpdir
	       (make-temp-file "file-notify-test-parent" t)))
	  (should
	   (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
	         file-notify--test-desc
	         (file-notify--test-add-watch
		  file-notify--test-tmpdir
		  '(change) #'file-notify--test-event-handler)))
	  (should (file-notify-valid-p file-notify--test-desc))
	  (file-notify--test-with-actions
	      (cond
	       ;; w32notify does not raise `deleted' and `stopped'
	       ;; events for the watched directory.
	       ((string-equal (file-notify--test-library) "w32notify")
	        '(created changed deleted))
               ;; gvfs-monitor-dir on cygwin does not detect the
               ;; `created' event reliably.
	       ((string-equal
                 (file-notify--test-library) "gvfs-monitor-dir.exe")
	        '((deleted stopped)
	          (created deleted stopped)))
	       ;; There are two `deleted' events, for the file and for
	       ;; the directory.  Except for cygwin and kqueue.  And
	       ;; cygwin does not raise a `changed' event.
	       ((eq system-type 'cygwin)
	        '(created deleted stopped))
	       ((string-equal (file-notify--test-library) "kqueue")
	        '(created changed deleted stopped))
	       (t '(created changed deleted deleted stopped)))
	    (write-region
	     "any text" nil file-notify--test-tmpfile nil 'no-message)
	    (file-notify--test-read-event)
	    (delete-directory file-notify--test-tmpdir 'recursive))
	  ;; After deleting the parent directory, the descriptor must
	  ;; not be valid anymore.
	  (should-not (file-notify-valid-p file-notify--test-desc))
          ;; w32notify doesn't generate `stopped' events when the
          ;; parent directory is deleted, which doesn't provide a
          ;; chance for filenotify.el to remove the descriptor from
          ;; the internal hash table it maintains.  So we must remove
          ;; the descriptor manually.
          (if (string-equal (file-notify--test-library) "w32notify")
              (file-notify--rm-descriptor file-notify--test-desc))

          ;; The environment shall be cleaned up.
          (file-notify--test-cleanup-p)))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test05-file-validity
  "Check `file-notify-valid-p' via file notification for remote files.")

(ert-deftest file-notify-test06-dir-validity ()
  "Check `file-notify-valid-p' for directories."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
	(should
	 (setq file-notify--test-tmpfile
	       (make-temp-file "file-notify-test-parent" t)))
	(should
	 (setq file-notify--test-desc
	       (file-notify-add-watch
                file-notify--test-tmpfile '(change) #'ignore)))
        (should (file-notify-valid-p file-notify--test-desc))
        ;; After removing the watch, the descriptor must not be valid
        ;; anymore.
        (file-notify-rm-watch file-notify--test-desc)
        (file-notify--test-wait-for-events
         (file-notify--test-timeout)
	 (not (file-notify-valid-p file-notify--test-desc)))
        (should-not (file-notify-valid-p file-notify--test-desc))
        (delete-directory file-notify--test-tmpfile 'recursive)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; On emba, `deleted' and `stopped' events of the directory are
      ;; not detected.
      (unless (getenv "EMACS_EMBA_CI")
	(should
	 (setq file-notify--test-tmpfile
	       (make-temp-file "file-notify-test-parent" t)))
	(should
	 (setq file-notify--test-desc
	       (file-notify-add-watch
		file-notify--test-tmpfile '(change) #'ignore)))
        (should (file-notify-valid-p file-notify--test-desc))
        ;; After deleting the directory, the descriptor must not be
        ;; valid anymore.
        (delete-directory file-notify--test-tmpfile 'recursive)
        (file-notify--test-wait-for-events
	 (file-notify--test-timeout)
	 (not (file-notify-valid-p file-notify--test-desc)))
        (should-not (file-notify-valid-p file-notify--test-desc))
        (if (string-equal (file-notify--test-library) "w32notify")
            (file-notify--rm-descriptor file-notify--test-desc))

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test06-dir-validity
  "Check `file-notify-valid-p' via file notification for remote directories.")

(ert-deftest file-notify-test07-many-events ()
  "Check that events are not dropped."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  (should
   (setq file-notify--test-tmpfile
	 (make-temp-file "file-notify-test-parent" t)))
  (should
   (setq file-notify--test-desc
	 (file-notify--test-add-watch
	  file-notify--test-tmpfile
	  '(change) #'file-notify--test-event-handler)))
  (unwind-protect
      (let ((n 1000)
            source-file-list target-file-list
            (default-directory file-notify--test-tmpfile))
        (dotimes (i n)
	  ;; It matters which direction we rename, at least for
	  ;; kqueue.  This backend parses directories in alphabetic
	  ;; order (x%d before y%d).  So we rename into both directions.
	  (if (zerop (mod i 2))
	      (progn
		(push (expand-file-name (format "x%d" i)) source-file-list)
		(push (expand-file-name (format "y%d" i)) target-file-list))
	    (push (expand-file-name (format "y%d" i)) source-file-list)
	    (push (expand-file-name (format "x%d" i)) target-file-list)))
        (file-notify--test-with-actions (make-list (+ n n) 'created)
          (let ((source-file-list source-file-list)
                (target-file-list target-file-list))
            (while (and source-file-list target-file-list)
              (file-notify--test-read-event)
              (write-region "" nil (pop source-file-list) nil 'no-message)
              (file-notify--test-read-event)
              (write-region "" nil (pop target-file-list) nil 'no-message))))
        (file-notify--test-with-actions
	    (cond
	     ;; w32notify fires both `deleted' and `renamed' events.
	     ((string-equal (file-notify--test-library) "w32notify")
	      (let (r)
		(dotimes (_i n)
		  (setq r (append '(deleted renamed) r)))
		r))
	     ;; cygwin fires `changed' and `deleted' events, sometimes
	     ;; in random order.
	     ((eq system-type 'cygwin)
	      (let (r)
		(dotimes (_i n)
		  (setq r (append '(changed deleted) r)))
		(cons :random r)))
	     (t (make-list n 'renamed)))
          (let ((source-file-list source-file-list)
                (target-file-list target-file-list))
            (while (and source-file-list target-file-list)
              (file-notify--test-read-event)
              (rename-file (pop source-file-list) (pop target-file-list) t))))
        (file-notify--test-with-actions (make-list n 'deleted)
          (dolist (file target-file-list)
            (file-notify--test-read-event)
            (delete-file file)))
        (delete-directory file-notify--test-tmpfile)
        (if (or (string-equal (file-notify--test-library) "w32notify")
                (getenv "EMACS_EMBA_CI"))
            (file-notify--rm-descriptor file-notify--test-desc))

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

;; Unpredictable failures, eg https://hydra.nixos.org/build/86016286
(file-notify--deftest-remote file-notify-test07-many-events
  "Check that events are not dropped for remote directories."
  (getenv "EMACS_HYDRA_CI"))

(ert-deftest file-notify-test08-backup ()
  "Check that backup keeps file notification."
  (skip-unless (file-notify--test-local-enabled))

  (unwind-protect
      (progn
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
	(write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
	(should
	 (setq file-notify--test-desc
	       (file-notify--test-add-watch
		file-notify--test-tmpfile
		'(change) #'file-notify--test-event-handler)))
        (should (file-notify-valid-p file-notify--test-desc))
        (file-notify--test-with-actions
            ;; There could be one or two `changed' events.
            '((changed)
              (changed changed))
          ;; There shouldn't be any problem, because the file is kept.
          (with-temp-buffer
            (let ((buffer-file-name file-notify--test-tmpfile)
                  (make-backup-files t)
                  (backup-by-copying t)
                  (kept-new-versions 1)
                  (delete-old-versions t))
              (insert "another text")
              (save-buffer))))
        ;; After saving the buffer, the descriptor is still valid.
        (should (file-notify-valid-p file-notify--test-desc))
	(delete-file file-notify--test-tmpfile)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup))

  (unwind-protect
      ;; It doesn't work for kqueue, because we don't use an implicit
      ;; directory monitor.
      (unless (string-equal (file-notify--test-library) "kqueue")
        (setq file-notify--test-tmpfile (file-notify--test-make-temp-name))
        (write-region
         "any text" nil file-notify--test-tmpfile nil 'no-message)
        (should
         (setq file-notify--test-desc
               (file-notify--test-add-watch
                file-notify--test-tmpfile
                '(change) #'file-notify--test-event-handler)))
        (should (file-notify-valid-p file-notify--test-desc))
        (file-notify--test-with-actions
            (cond
             ;; On cygwin we only get the `changed' event.
             ((eq system-type 'cygwin)
              '(changed))
             (t '(renamed created changed)))
          ;; The file is renamed when creating a backup.  It shall
          ;; still be watched.
          (with-temp-buffer
            (let ((buffer-file-name file-notify--test-tmpfile)
                  (make-backup-files t)
                  (backup-by-copying nil)
                  (backup-by-copying-when-mismatch nil)
                  (kept-new-versions 1)
                  (delete-old-versions t))
              (insert "another text")
              (save-buffer))))
        ;; After saving the buffer, the descriptor is still valid.
        (should (file-notify-valid-p file-notify--test-desc))
        (delete-file file-notify--test-tmpfile)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test08-backup
  "Check that backup keeps file notification for remote files.")

(ert-deftest file-notify-test09-watched-file-in-watched-dir ()
  "Watches a directory and a file in that directory separately.
Checks that the callbacks are only called with events with
descriptors that were issued when registering the watches.  This
test caters for the situation in bug#22736 where the callback for
the directory received events for the file with the descriptor of
the file watch."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  ;; A directory to be watched.
  (should
   (setq file-notify--test-tmpfile
         (make-temp-file "file-notify-test-parent" t)))
  ;; A file to be watched.
  (should
   (setq file-notify--test-tmpfile1
         (let ((file-notify--test-tmpdir file-notify--test-tmpfile))
           (file-notify--test-make-temp-name))))
  (write-region "any text" nil file-notify--test-tmpfile1 nil 'no-message)
  (unwind-protect
      (cl-flet (;; Directory monitor.
                (dir-callback (event file)
                 (let ((file-notify--test-desc file-notify--test-desc1))
                   (file-notify--test-event-handler event file)))
                ;; File monitor.
                (file-callback (event file)
                 (let ((file-notify--test-desc file-notify--test-desc2))
                   (file-notify--test-event-handler event file))))
        (should
         (setq file-notify--test-desc1
               (file-notify--test-add-watch
                file-notify--test-tmpfile
                '(change) #'dir-callback)
               ;; This is needed for `file-notify--test-monitor'.
               file-notify--test-desc file-notify--test-desc1))
        (should
         (setq file-notify--test-desc2
               (file-notify--test-add-watch
                file-notify--test-tmpfile1
                '(change) #'file-callback)))
        (should (file-notify-valid-p file-notify--test-desc1))
        (should (file-notify-valid-p file-notify--test-desc2))
        (should-not (equal file-notify--test-desc1 file-notify--test-desc2))
        (let ((n 100))
          ;; Run the test.
          (file-notify--test-with-actions
              ;; There could be one or two `changed' events.
              (list
	       ;; cygwin.
               (append
                '(:random)
                (make-list (/ n 2) 'changed)
                (make-list (/ n 2) 'created)
                (make-list (/ n 2) 'changed))
               (append
                '(:random)
                ;; Directory monitor and file monitor.
                (make-list (/ n 2) 'changed)
                (make-list (/ n 2) 'changed)
                ;; Just the directory monitor.
                (make-list (/ n 2) 'created)
                (make-list (/ n 2) 'changed))
               (append
                '(:random)
                ;; Directory monitor and file monitor.
                (make-list (/ n 2) 'changed)
                (make-list (/ n 2) 'changed)
                (make-list (/ n 2) 'changed)
                (make-list (/ n 2) 'changed)
                ;; Just the directory monitor.
                (make-list (/ n 2) 'created)
                (make-list (/ n 2) 'changed)))
            (dotimes (i n)
              (file-notify--test-read-event)
              (if (zerop (mod i 2))
                  (write-region
                   "any text" nil file-notify--test-tmpfile1 t 'no-message)
                (let ((file-notify--test-tmpdir file-notify--test-tmpfile))
                  (write-region
                   "any text" nil
                   (file-notify--test-make-temp-name) nil 'no-message))))))

        ;; If we delete the file, the directory monitor shall still be
        ;; active.  We receive the `deleted' event from both the
        ;; directory and the file monitor.  The `stopped' event is
        ;; from the file monitor.  It's undecided in which order the
        ;; directory and the file monitor are triggered.
        (file-notify--test-with-actions '(:random deleted deleted stopped)
          (delete-file file-notify--test-tmpfile1))
        (should (file-notify-valid-p file-notify--test-desc1))
        (should-not (file-notify-valid-p file-notify--test-desc2))

        ;; Now we delete the directory.
        (file-notify--test-with-actions
            (cond
             ;; In kqueue and for cygwin, just one `deleted' event for
             ;; the directory is received.
             ((or (eq system-type 'cygwin)
		  (string-equal (file-notify--test-library) "kqueue"))
              '(deleted stopped))
             (t (append
                 ;; The directory monitor raises a `deleted' event for
                 ;; every file contained in the directory, we must
                 ;; count them.
                 (make-list
                  (length
                   (directory-files
                    file-notify--test-tmpfile nil
                    directory-files-no-dot-files-regexp 'nosort))
                  'deleted)
                 ;; The events of the directory itself.
                 (cond
		  ;; w32notify does not raise `deleted' and `stopped'
		  ;; events for the watched directory.
                  ((string-equal (file-notify--test-library) "w32notify")
                   '())
                  ;; On emba, `deleted' and `stopped' events of the
                  ;; directory are not detected.
                  ((getenv "EMACS_EMBA_CI")
                   '())
                  (t '(deleted stopped))))))
          (delete-directory file-notify--test-tmpfile 'recursive))
        (unless (getenv "EMACS_EMBA_CI")
          (should-not (file-notify-valid-p file-notify--test-desc1))
          (should-not (file-notify-valid-p file-notify--test-desc2)))
        (when (or (string-equal (file-notify--test-library) "w32notify")
                  (getenv "EMACS_EMBA_CI"))
          (file-notify--rm-descriptor file-notify--test-desc1)
          (file-notify--rm-descriptor file-notify--test-desc2))

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test09-watched-file-in-watched-dir
  "Check `file-notify-test09-watched-file-in-watched-dir' for remote files." t)

(ert-deftest file-notify-test10-sufficient-resources ()
  "Check that file notification does not use too many resources."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))
  ;; This test is intended for kqueue only.
  (skip-unless (string-equal (file-notify--test-library) "kqueue"))

  (should
   (setq file-notify--test-tmpfile
	 (make-temp-file "file-notify-test-parent" t)))
  (unwind-protect
      (let ((file-notify--test-tmpdir file-notify--test-tmpfile)
	    descs)
	(should-error
	 (while t
	   ;; We watch directories, because we want to reach the upper
	   ;; limit.  Watching a file might not be sufficient, because
	   ;; most of the libraries implement this as watching the
	   ;; upper directory.
	   (setq file-notify--test-tmpfile1
		 (make-temp-file "file-notify-test-parent" t)
		 descs
		 (cons
		  (should
		   (file-notify-add-watch
		    file-notify--test-tmpfile1 '(change) #'ignore))
		  descs)))
	 :type 'file-notify-error)
	;; Remove watches.  If we don't do it prior removing
	;; directories, Emacs crashes in batch mode.
	(dolist (desc descs)
	 (file-notify-rm-watch desc))
	;; Remove directories.
        (delete-directory file-notify--test-tmpfile 'recursive)

        ;; The environment shall be cleaned up.
        (file-notify--test-cleanup-p))

    ;; Cleanup.
    (file-notify--test-cleanup)))

(file-notify--deftest-remote file-notify-test10-sufficient-resources
  "Check `file-notify-test10-sufficient-resources' for remote files.")

(defun file-notify-test-all (&optional interactive)
  "Run all tests for \\[file-notify]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^file-notify-")
    (ert-run-tests-batch "^file-notify-")))

;; TODO:

;; * kqueue does not send all expected `deleted' events.  Maybe due to
;;   the missing directory monitor.
;; * For w32notify, no `deleted' and `stopped' events arrive when a
;;   directory is removed.
;; * For cygwin and w32notify, no `attribute-changed' events arrive.
;;   They send `changed' events instead.
;; * cygwin does not send all expected `changed' and `deleted' events.
;;   Probably due to timing issues.

(provide 'file-notify-tests)
;;; filenotify-tests.el ends here
