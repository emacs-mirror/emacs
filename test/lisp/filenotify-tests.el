;;; filenotify-tests.el --- Tests of file notifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

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

;; For the remote file-notify library, Tramp checks for the existence of
;; a respective command.  The first command found is used.  In order to
;; use a dedicated one, the environment variable
;; $REMOTE_FILE_NOTIFY_LIBRARY shall be set, possible values are
;; "inotifywait", "gio", and "smb-notify".

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

(require 'tramp)
(require 'ert-x)
(require 'filenotify)

(defvar auto-revert-buffer-list)

;; Filter suppressed remote file-notify libraries.
(when (stringp (getenv "REMOTE_FILE_NOTIFY_LIBRARY"))
  (dolist (lib '("inotifywait" "gio" "smb-notify"))
    (unless (string-equal (getenv "REMOTE_FILE_NOTIFY_LIBRARY") lib)
      (add-to-list 'tramp-connection-properties `(nil ,lib nil)))))

(defvar file-notify--test-rootdir temporary-file-directory)
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

(defun file-notify--test-wait-event ()
  "Wait for one event.
There are different timeouts for local and remote file notification libraries."
  (read-event
   nil nil
   (cond
    ;; gio/gpollfilemonitor.c declares POLL_TIME_SECS 5.  So we must
    ;; wait at least this time in the GPollFileMonitor case.  A
    ;; similar timeout seems to be needed in the
    ;; GFam{File,Directory}Monitor case.  So we use a large timeout
    ;; for any monitor.
    ((file-notify--test-monitor) 7)
    ((file-remote-p file-notify--test-rootdir) 0.1)
    (t 0.01)))
  nil)

(defun file-notify--test-timeout ()
  "Timeout to wait for arriving a bunch of events, in seconds."
  (cond
   ((file-remote-p file-notify--test-rootdir) 20)
   ((eq system-type 'cygwin) 10)
   ((getenv "EMACS_EMBA_CI") 10)
   ((string-equal (file-notify--test-library) "w32notify") 4)
   (t 3)))

(defmacro file-notify--test-wait-for-events (timeout until)
  "Wait for and return file notification events until form UNTIL is true.
TIMEOUT is the maximum time to wait for, in seconds."
  `(with-timeout (,timeout (ignore))
     (while (null ,until)
       (when file-notify-debug
         (message "file-notify--test-wait-for-events received: %s"
                  (file-notify--test-event-actions)))
       (file-notify--test-wait-event))))

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
  "Cleanup before and after a test."
  (file-notify-rm-all-watches)

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
    (when (file-remote-p file-notify--test-rootdir)
      (tramp-cleanup-connection
       (tramp-dissect-file-name file-notify--test-rootdir) t 'keep-password)))

  (when (hash-table-p file-notify-descriptors)
    (clrhash file-notify-descriptors))

  (setq auto-revert-buffer-list nil
        file-notify--test-tmpdir nil
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

(setq auth-source-cache-expiry nil
      auth-source-save-behavior nil
      ert-temp-file-suffix ""
      file-notify-debug nil
      password-cache-expiry nil
      remote-file-name-inhibit-cache nil
      tramp-allow-unsafe-temporary-files t
      tramp-cache-read-persistent-data t ;; For auth-sources.
      tramp-verbose 0
      ;; When the remote user id is 0, Tramp refuses unsafe temporary files.
      tramp-allow-unsafe-temporary-files
      (or tramp-allow-unsafe-temporary-files noninteractive))

(defun file-notify--test-add-watch (file flags callback)
  "Like `file-notify-add-watch', but also passing FILE to CALLBACK."
  (file-notify-add-watch
   file flags (lambda (event) (funcall callback event file))))

;; We do not want to try and fail `file-notify-add-watch'.
(defun file-notify--test-local-enabled ()
  "Whether local file notification is enabled.
This is needed for local `file-notify--test-rootdir' only, in the
remote case we return always t."
  (or file-notify--library
      (file-remote-p file-notify--test-rootdir)))

(defvar file-notify--test-remote-enabled-checked
  (if (getenv "EMACS_HYDRA_CI") '(t . nil))
  "Cached result of `file-notify--test-remote-enabled'.
If the function did run, the value is a cons cell, the `cdr'
being the result.")

(defun file-notify--test-remote-enabled ()
  "Whether remote file notification is enabled."
  (unless (consp file-notify--test-remote-enabled-checked)
    (let (desc)
      (ignore-errors
        (and
         (file-remote-p ert-remote-temporary-file-directory)
         (file-directory-p ert-remote-temporary-file-directory)
         (file-writable-p ert-remote-temporary-file-directory)
         (setq desc
               (file-notify-add-watch
                ert-remote-temporary-file-directory
                '(change) #'ignore))))
      (setq file-notify--test-remote-enabled-checked (cons t desc))
      (when desc (file-notify-rm-watch desc))))
  ;; Return result.
  (cdr file-notify--test-remote-enabled-checked))

(defun file-notify--test-library ()
  "The used library for the test, as a string.
In the remote case, it is the process name which runs on the
remote host, or nil."
  (if (null (file-remote-p file-notify--test-rootdir))
      (symbol-name file-notify--library)
    (and (consp file-notify--test-remote-enabled-checked)
	 (processp (cdr file-notify--test-remote-enabled-checked))
	 (replace-regexp-in-string
	  "<[[:digit:]]+>\\'" ""
	  (process-name (cdr file-notify--test-remote-enabled-checked))))))

(defun file-notify--test-monitor ()
  "The used monitor for the test, as a symbol.
This returns only for (local) gfilenotify, (remote) gio or (remote)
smb-notify libraries; otherwise it is nil.  `file-notify--test-desc'
must be a valid watch descriptor."
  ;; We cache the result, because after `file-notify-rm-watch',
  ;; `gfile-monitor-name' does not return a proper result anymore.
  ;; But we still need this information.  So far, we know the monitors
  ;; - GFamFileMonitor (gfilenotify on cygwin)
  ;; - GFamDirectoryMonitor (gfilenotify on Solaris)
  ;; - GInotifyFileMonitor (gfilenotify and gio on GNU/Linux)
  ;; - GKqueueFileMonitor (gfilenotify and gio on FreeBSD)
  ;; - GPollFileMonitor (gio on cygwin)
  ;; - SMBSamba (smb-notify on Samba server)
  ;; - SMBWindows (smb-notify on MS Windows).
  (if file-notify--test-desc
      (or (alist-get file-notify--test-desc file-notify--test-monitors)
          (when (member
                 (file-notify--test-library) '("gfilenotify" "gio" "smb-notify"))
	    (add-to-list
	     'file-notify--test-monitors
	     (cons file-notify--test-desc
	           (if (file-remote-p file-notify--test-rootdir)
                       ;; `file-notify--test-desc' is the connection process.
                       (progn
                         (while
                             (and (process-live-p file-notify--test-desc)
                                  (not (tramp-connection-property-p
		                        file-notify--test-desc "file-monitor")))
                           (accept-process-output file-notify--test-desc 0))
		         (tramp-get-connection-property
		          file-notify--test-desc "file-monitor"))
		     (and (functionp 'gfile-monitor-name)
		          (gfile-monitor-name file-notify--test-desc)))))
            ;; If we don't know the monitor, there are good chances the
            ;; test will fail.  We skip it.
            (unless (alist-get file-notify--test-desc file-notify--test-monitors)
              (ert-skip "Cannot determine test monitor")))
	  (alist-get file-notify--test-desc file-notify--test-monitors))
    (ert-skip "`file-notify--test-desc' is nil when checking for test monitor")))

(defmacro file-notify--deftest-remote (test docstring &optional unstable)
  "Define ert `TEST-remote' for remote files.
If UNSTABLE is non-nil, the test is tagged as `:unstable'."
  (declare (indent 1) (debug (symbolp stringp &optional form)))
  `(ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
     ,docstring
     :tags (if ,unstable '(:expensive-test :unstable) '(:expensive-test))
     (let* ((file-notify--test-rootdir ert-remote-temporary-file-directory)
	    (ert-test (ert-get-test ',test))
            vc-handled-backends)
       (skip-unless (file-notify--test-remote-enabled))
       (file-notify--test-cleanup)
       (funcall (ert-test-body ert-test)))))

(defmacro with-file-notify-test (&rest body)
  "Setup test environment.
It creates `file-notify--test-tmpdir' and `file-notify--test-tmpfile'.
When returning, they are deleted."
  (declare (debug t))
  `(ert-with-temp-directory file-notify--test-tmpdir
     :prefix
     (expand-file-name "file-notify-test-parent" file-notify--test-rootdir)
     (let ((ert-temp-file-prefix
            (expand-file-name "file-notify-test" file-notify--test-tmpdir)))
       (ert-with-temp-file file-notify--test-tmpfile
         :prefix ert-temp-file-prefix
         (unwind-protect
             (progn ,@body)
           (file-notify--test-cleanup))))))

(ert-deftest file-notify-test00-availability ()
  "Test availability of `file-notify'."
  (skip-unless (file-notify--test-local-enabled))

  (with-file-notify-test
   ;; Report the native library which has been used.
   (message "Library: `%s'" (file-notify--test-library))
   (should
    (setq file-notify--test-desc
          (file-notify-add-watch file-notify--test-tmpdir '(change) #'ignore)))
   (when (file-notify--test-monitor)
     (message "Monitor: `%s'" (file-notify--test-monitor)))
   (file-notify-rm-watch file-notify--test-desc)

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p)))

(file-notify--deftest-remote file-notify-test00-availability
  "Test availability of `file-notify' for remote files.")

(defun file-notify--test-make-temp-name ()
  "Create a temporary file name for test."
  (make-temp-name
   (expand-file-name "file-notify-test" file-notify--test-tmpdir)))

(ert-deftest file-notify-test01-add-watch ()
  "Check `file-notify-add-watch'."
  (skip-unless (file-notify--test-local-enabled))

  (with-file-notify-test
   (setq file-notify--test-tmpfile1
         (format "%s/%s" file-notify--test-tmpfile (md5 (current-time-string))))
   (delete-file file-notify--test-tmpfile)

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

   ;; File monitors like kqueue insist, that the watched file exists.
   ;; Directory monitors are not bound to this restriction.
   (when (string-equal (file-notify--test-library) "kqueue")
     (write-region "any text" nil file-notify--test-tmpfile nil 'no-message))
   (should
    (setq file-notify--test-desc
          (file-notify-add-watch
           file-notify--test-tmpfile '(change attribute-change) #'ignore)))
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
   (file-notify--test-cleanup-p)))

(file-notify--deftest-remote file-notify-test01-add-watch
  "Check `file-notify-add-watch' for remote files.")

;; This test is inspired by Bug#26126 and Bug#26127.
(ert-deftest file-notify-test02-rm-watch ()
  "Check `file-notify-rm-watch' and `file-notify-rm-all-watches'."
  (skip-unless (file-notify--test-local-enabled))

  (with-file-notify-test
   ;; Check, that `file-notify-rm-watch' works.
   (should
    (setq file-notify--test-desc
          (file-notify-add-watch
           file-notify--test-tmpdir '(change) #'ignore)))
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

  (with-file-notify-test
   (ert-with-temp-file file-notify--test-tmpfile1
     :prefix ert-temp-file-prefix
     ;; Check, that no error is returned removing a watch descriptor twice.
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

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p)))

  (with-file-notify-test
   ;; Check, that removing watch descriptors out of order do not harm.
   ;; This fails on cygwin because of timing issues unless a long
   ;; `sit-for' is added before the call to
   ;; `file-notify--test-wait-event'.
   (unless (eq system-type 'cygwin)
     (let (results)
       (cl-flet ((first-callback (event)
                   (when (eq (file-notify--test-event-action event) 'deleted)
                     (push 1 results)))
                 (second-callback (event)
                   (when (eq (file-notify--test-event-action event) 'deleted)
                     (push 2 results))))
         (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
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
         ;; `file-notify-rm-watch' confuses `file-notify--test-monitor'.
         ;; Initialize it in time.
         (file-notify--test-monitor)
         ;; Remove first watch.
         (file-notify-rm-watch file-notify--test-desc)
         ;; Only the second callback shall run.
	 (file-notify--test-wait-event)
         (delete-file file-notify--test-tmpfile)
         (file-notify--test-wait-for-events
          (file-notify--test-timeout) results)
         (should (equal results (list 2)))

         ;; The environment shall be cleaned up.
         (file-notify--test-cleanup-p)))))

  (with-file-notify-test
   (ert-with-temp-file file-notify--test-tmpfile1
     :prefix ert-temp-file-prefix
     ;; Check `file-notify-rm-all-watches'.
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
     (file-notify-rm-all-watches)

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p))))

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
  (unless (eq (file-notify--test-event-action file-notify--test-event) 'stopped)
    (should (equal (file-notify--test-event-desc file-notify--test-event)
                   file-notify--test-desc)))
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
         (file-notify--test-file (directory-file-name file))
         (result
          (ert-run-test (make-ert-test :body 'file-notify--test-event-test))))
    ;; Do not add lock files, this would confuse the checks.
    (unless (string-match-p
	     (regexp-quote ".#")
	     (file-notify--test-event-file file-notify--test-event))
      (when file-notify-debug
        (message "file-notify--test-event-handler result: %s event: %S"
                 (null (ert-test-failed-p result)) file-notify--test-event))
      (setq file-notify--test-events
	    (append file-notify--test-events `(,file-notify--test-event))
	    file-notify--test-results
	    (append file-notify--test-results `(,result))))))

(defun file-notify--test-event-actions ()
  "Helper function to return retrieved actions, as list."
  (mapcar #'file-notify--test-event-action file-notify--test-events))

(defun file-notify--test-with-actions-check (actions)
  "Check whether received actions match one of the ACTIONS alternatives."
  (when file-notify-debug
    (message "file-notify--test-with-actions-check"))
  (let (result)
    (dolist (elt actions result)
      (setq result
            (or result
                (if (eq (car elt) :random)
                    (equal (sort (cdr elt) 'string-lessp)
                           (sort (file-notify--test-event-actions)
                                 'string-lessp))
                  (equal elt (file-notify--test-event-actions))))))
    ;; Do not report result in case we debug.  Write messages instead.
    (if file-notify-debug
        (prog1 t
          (if result
              (message "Success\n%s" (file-notify--test-event-actions))
            (message (file-notify--test-with-actions-explainer actions))))
      result)))

(defun file-notify--test-with-actions-explainer (actions)
  "Explain why `file-notify--test-with-actions-check' fails."
  (if (null (cdr actions))
      (format "Received actions do not match expected actions\n%s\n%s"
              (file-notify--test-event-actions) (car actions))
    (format
     "Received actions do not match any sequence of expected actions\n%s\n%s"
     (file-notify--test-event-actions) actions)))

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
  (declare (indent 1) (debug (form &rest body)))
  `(let* ((actions (if (consp (car ,actions)) ,actions (list ,actions)))
          (max-length
           (apply
            'max
            (mapcar
             (lambda (x) (length (if (eq (car x) :random) (cdr x) x)))
             actions)))
          ;; Don't stop while debugging.
          (while-no-input-ignore-events
           (cons 'file-notify while-no-input-ignore-events))
          create-lockfiles)
     ;; Flush pending actions.
     (file-notify--test-wait-event)
     (file-notify--test-wait-for-events
      (file-notify--test-timeout)
      (not (input-pending-p)))
     (setq file-notify--test-events nil
           file-notify--test-results nil)
     (when file-notify-debug
        (message "file-notify--test-with-actions expected: %s" actions))
     ,@body
     (file-notify--test-wait-for-events
      ;; More actions need more time.  Use some fudge factor.
      (* (ceiling max-length 100) (file-notify--test-timeout))
      (or (= max-length (length file-notify--test-events))
          (memq 'stopped (file-notify--test-event-actions))))
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
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  ;; It doesn't work for kqueue, because we don't use an implicit
  ;; directory monitor.
  (unless (string-equal (file-notify--test-library) "kqueue")
    (with-file-notify-test
     ;; Check file creation, change and deletion.
     (delete-file file-notify--test-tmpfile)
     (should
      (setq file-notify--test-desc
            (file-notify--test-add-watch
             file-notify--test-tmpfile
             '(change) #'file-notify--test-event-handler)))
     (file-notify--test-with-actions
         (cond
          ;; SMBSamba reports three `changed' events.
	  ((eq (file-notify--test-monitor) 'SMBSamba)
           '(created changed changed changed deleted stopped))
	  ;; GFam{File,Directory}Monitor, GKqueueFileMonitor and
	  ;; GPollFileMonitor do not report the `changed' event.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor
                   GKqueueFileMonitor GPollFileMonitor))
	   '(created deleted stopped))
          (t '(created changed deleted stopped)))
       (write-region
        "another text" nil file-notify--test-tmpfile nil 'no-message)
       (file-notify--test-wait-event)
       (delete-file file-notify--test-tmpfile))
     (file-notify-rm-watch file-notify--test-desc)

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p)))

  (with-file-notify-test
   ;; Check file change and deletion.
   (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpfile
	   '(change) #'file-notify--test-event-handler)))
   (file-notify--test-with-actions
       (cond
        ;; SMBSamba reports four `changed' events.
	((eq (file-notify--test-monitor) 'SMBSamba)
         '(changed changed changed changed deleted stopped))
	;; GFam{File,Directory}Monitor and GPollFileMonitor do not
        ;; detect the `changed' event reliably.
	((memq (file-notify--test-monitor)
               '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	 '((deleted stopped)
	   (changed deleted stopped)))
	;; GKqueueFileMonitor does not report the `changed' event.
	((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	 '(deleted stopped))
	;; There could be one or two `changed' events.
	(t '((changed deleted stopped)
	     (changed changed deleted stopped))))
     (write-region "another text" nil file-notify--test-tmpfile nil 'no-message)
     (file-notify--test-wait-event)
     (delete-file file-notify--test-tmpfile))
   (file-notify-rm-watch file-notify--test-desc)

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p))

  (with-file-notify-test
   (delete-file file-notify--test-tmpfile)
   ;; Check file creation, change and deletion when watching a
   ;; directory.  There must be a `stopped' event when deleting the
   ;; directory.
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpdir
	   '(change) #'file-notify--test-event-handler)))
   (file-notify--test-with-actions
       (cond
	;; w32notify does not raise `deleted' and `stopped' events for
	;; the watched directory.
	((string-equal (file-notify--test-library) "w32notify")
	 '(created changed deleted))
        ;; SMBSamba reports three `changed' events.
	((eq (file-notify--test-monitor) 'SMBSamba)
         '(created changed changed changed deleted deleted stopped))
	;; There are two `deleted' events, for the file and for the
	;; directory.  Except for GFam{File,Directory}Monitor,
	;; GPollFileMonitor and kqueue.  And GFam{File,Directory}Monitor
	;; and GPollFileMonitor do not raise a `changed' event.
	((memq (file-notify--test-monitor)
               '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	 '(created deleted stopped))
	((string-equal (file-notify--test-library) "kqueue")
	 '(created changed deleted stopped))
	;; GKqueueFileMonitor does not report the `changed' event.
	((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	 '(created deleted deleted stopped))
	(t '(created changed deleted deleted stopped)))
     (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
     (file-notify--test-wait-event)
     (delete-directory file-notify--test-tmpdir 'recursive))
   (file-notify-rm-watch file-notify--test-desc)

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p))

  (with-file-notify-test
   (ert-with-temp-file file-notify--test-tmpfile1
     :prefix ert-temp-file-prefix
     ;; Check copy of files inside a directory.
     (delete-file file-notify--test-tmpfile)
     (delete-file file-notify--test-tmpfile1)
     (should
      (setq file-notify--test-desc
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
          ;; SMBWindows does not distinguish between `changed' and
	  ;; `attribute-changed'.
	  ((eq (file-notify--test-monitor) 'SMBWindows)
	   '(created changed created changed changed changed
             deleted deleted deleted stopped))
          ;; SMBSamba reports three `changed' events.
	  ((eq (file-notify--test-monitor) 'SMBSamba)
           '(created changed changed changed created changed changed changed
             changed changed deleted deleted deleted stopped))
	  ;; There are three `deleted' events, for two files and for the
	  ;; directory.  Except for GFam{File,Directory}Monitor,
	  ;; GPollFileMonitor and kqueue.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	   '(created created changed changed deleted stopped))
	  ((string-equal (file-notify--test-library) "kqueue")
	   '(created changed created changed deleted stopped))
	  ;; GKqueueFileMonitor does not report the `changed' event.
	  ((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	   '(created created deleted deleted deleted stopped))
	  (t '(created changed created changed
	       deleted deleted deleted stopped)))
       (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
       (file-notify--test-wait-event)
       (copy-file file-notify--test-tmpfile file-notify--test-tmpfile1)
       ;; The next two events shall not be visible.
       (file-notify--test-wait-event)
       (set-file-modes file-notify--test-tmpfile 000 'nofollow)
       (file-notify--test-wait-event)
       (set-file-times file-notify--test-tmpfile '(0 0) 'nofollow)
       (file-notify--test-wait-event)
       (delete-directory file-notify--test-tmpdir 'recursive))
     (file-notify-rm-watch file-notify--test-desc)

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p)))

  (with-file-notify-test
   (ert-with-temp-file file-notify--test-tmpfile1
     :prefix ert-temp-file-prefix
     ;; Check rename of files inside a directory.
     (delete-file file-notify--test-tmpfile)
     (delete-file file-notify--test-tmpfile1)
     (should
      (setq file-notify--test-tmpfile (file-notify--test-make-temp-name)
	    file-notify--test-tmpfile1 (file-notify--test-make-temp-name)
	    file-notify--test-desc
	    (file-notify--test-add-watch
	     file-notify--test-tmpdir
	     '(change) #'file-notify--test-event-handler)))
     (file-notify--test-with-actions
	 (cond
	  ;; w32notify does not raise `deleted' and `stopped' events for
	  ;; the watched directory.
	  ((string-equal (file-notify--test-library) "w32notify")
	   '(created changed renamed deleted))
          ;; SMBSamba reports three `changed' events.
	  ((eq (file-notify--test-monitor) 'SMBSamba)
           '(created changed changed changed
             renamed changed changed deleted deleted stopped))
	  ;; There are two `deleted' events, for the file and for the
	  ;; directory.  Except for GFam{File,Directory}Monitor,
	  ;; GPollfileMonitor and kqueue.  And
	  ;; GFam{File,Directory}Monitor and GPollFileMonitor raise
	  ;; `created' and `deleted' events instead of a `renamed'
	  ;; event.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	   '(created created deleted deleted stopped))
	  ((string-equal (file-notify--test-library) "kqueue")
	   '(created changed renamed deleted stopped))
	  ;; GKqueueFileMonitor does not report the `changed' event.
	  ((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	   '(created renamed deleted deleted stopped))
	  (t '(created changed renamed deleted deleted stopped)))
       (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
       (file-notify--test-wait-event)
       (rename-file file-notify--test-tmpfile file-notify--test-tmpfile1)
       ;; After the rename, we won't get events anymore.
       (file-notify--test-wait-event)
       (delete-directory file-notify--test-tmpdir 'recursive))
     (file-notify-rm-watch file-notify--test-desc)

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p)))

  (with-file-notify-test
   ;; Check attribute change.
   (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpfile
	   '(attribute-change) #'file-notify--test-event-handler)))
   (file-notify--test-with-actions
       (cond
	;; w32notify does not distinguish between `changed' and
	;; `attribute-changed'.  Under MS Windows 7, we get four
	;; `changed' events, and under MS Windows 10 just two.  Strange.
	((string-equal (file-notify--test-library) "w32notify")
	 '((changed changed)
	   (changed changed changed changed)))
        ;; SMBWindows does not distinguish between `changed' and
	;; `attribute-changed'.
	((eq (file-notify--test-monitor) 'SMBWindows)
	 '(changed changed changed))
        ;; SMBSamba does not distinguish between `changed' and
	;; `attribute-changed'.
	((eq (file-notify--test-monitor) 'SMBSamba)
         '(changed changed changed changed changed))
	;; GFam{File,Directory}Monitor, GKqueueFileMonitor and
	;; GPollFileMonitor do not report the `attribute-changed' event.
	((memq (file-notify--test-monitor)
               '(GFamFileMonitor GFamDirectoryMonitor
                 GKqueueFileMonitor GPollFileMonitor))
         '())
	;; For GInotifyFileMonitor, `write-region' raises also an
	;; `attribute-changed' event on gio.
	((and (string-equal (file-notify--test-library) "gio")
              (eq (file-notify--test-monitor) 'GInotifyFileMonitor))
	 '(attribute-changed attribute-changed attribute-changed))
	;; For kqueue, `write-region' raises also an `attribute-changed'
	;; event.
        ((string-equal (file-notify--test-library) "kqueue")
	 '(attribute-changed attribute-changed attribute-changed))
	;; For inotifywait, `write-region' raises also an
	;; `attribute-changed' event.
        ((string-equal (file-notify--test-library) "inotifywait")
	 '(attribute-changed attribute-changed attribute-changed))
	(t '(attribute-changed attribute-changed)))
     (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
     (file-notify--test-wait-event)
     (set-file-modes file-notify--test-tmpfile 000 'nofollow)
     (file-notify--test-wait-event)
     (set-file-times file-notify--test-tmpfile '(0 0) 'nofollow)
     (file-notify--test-wait-event)
     (delete-file file-notify--test-tmpfile))
   (file-notify-rm-watch file-notify--test-desc)

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p)))

(file-notify--deftest-remote file-notify-test03-events
  "Check file creation/change/removal notifications for remote files.")

(require 'autorevert)
(setq auto-revert-debug nil
      auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-remote-files t
      auto-revert-stop-on-user-input nil)

(ert-deftest file-notify-test04-autorevert ()
  "Check autorevert via file notification."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  ;; Run with shortened `auto-revert-interval' for a faster test.
  (let* ((auto-revert-interval 1)
         (timeout (if (file-remote-p file-notify--test-rootdir)
                      60   ; FIXME: can this be shortened?
                    (* auto-revert-interval 2.5)))
         (text-quoting-style 'grave)
         buf)
    (auto-revert-set-timer)

    (with-file-notify-test
     (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
     (setq buf (find-file-noselect file-notify--test-tmpfile))
     (with-current-buffer buf
       ;; In the remote case, `vc-refresh-state' returns undesired error
       ;; messages.  Let's suppress them.
       (add-function :around (local #'vc-refresh-state) #'ignore)
       (should (string-equal (buffer-string) "any text"))
       ;; `buffer-stale--default-function' checks for
       ;; `verify-visited-file-modtime'.  We must ensure that it returns
       ;; nil.
       (sleep-for 1)
       (auto-revert-mode 1)

       (file-notify--test-wait-for-events
        timeout auto-revert-notify-watch-descriptor)

       ;; `file-notify--test-monitor' needs to know
       ;; `file-notify--test-desc' in order to compute proper timeouts.
       (setq file-notify--test-desc auto-revert-notify-watch-descriptor)

       ;; GKqueueFileMonitor does not report the `changed' event.
       (skip-when (eq (file-notify--test-monitor) 'GKqueueFileMonitor))

       ;; Check, that file notification has been used.
       (should auto-revert-mode)
       (should auto-revert-use-notify)
       (should auto-revert-notify-watch-descriptor)

       ;; Modify file.  We wait for a second, in order to have another
       ;; timestamp.
       (ert-with-message-capture captured-messages
         (let ((inhibit-message t))
           (sleep-for 1)
           (write-region
            "another text" nil file-notify--test-tmpfile nil 'no-message)

           ;; Check, that the buffer has been reverted.
           (file-notify--test-wait-for-events
            timeout
            (string-match-p
             (rx bol "Reverting buffer `"
                 (literal (buffer-name buf)) "'" eol)
             captured-messages))
           (should (string-match-p "another text" (buffer-string)))))

       ;; Stop file notification.  Autorevert shall still work via polling.
       (file-notify-rm-watch auto-revert-notify-watch-descriptor)
       (file-notify--test-wait-for-events
	timeout
        (or (null auto-revert-notify-watch-descriptor)
            (not (file-notify-valid-p auto-revert-notify-watch-descriptor))))
       (should auto-revert-use-notify)

       ;; Modify file.  We wait for two seconds, in order to have
       ;; another timestamp.  One second seems to be too short.  And
       ;; cygwin sporadically requires more than two.
       (ert-with-message-capture captured-messages
         (let ((inhibit-message t))
           (sleep-for (if (eq system-type 'cygwin) 3 2))
           (write-region "foo bla" nil file-notify--test-tmpfile nil 'no-message)

           ;; Check, that the buffer has been reverted.
           (file-notify--test-wait-for-events
            timeout
            (string-match-p
             (rx bol "Reverting buffer `"
                 (literal (buffer-name buf)) "'" eol)
             captured-messages))
           (should (string-match-p "foo bla" (buffer-string)))))

       ;; Stop autorevert, in order to cleanup descriptor.
       (auto-revert-mode -1)
       (remove-function (local #'vc-refresh-state) #'ignore))

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p)

     ;; Cleanup.
     (ignore-errors (kill-buffer buf)))))

(file-notify--deftest-remote file-notify-test04-autorevert
  "Check autorevert via file notification for remote files.")

(ert-deftest file-notify-test05-file-validity ()
  "Check `file-notify-valid-p' for files."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  (with-file-notify-test
   (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
   (should
    (setq file-notify--test-desc
	  (file-notify-add-watch
           file-notify--test-tmpfile '(change) #'ignore)))
   (should (file-notify-valid-p file-notify--test-desc))
   ;; After calling `file-notify-rm-watch', the descriptor is not valid
   ;; anymore.
   (file-notify-rm-watch file-notify--test-desc)
   (should-not (file-notify-valid-p file-notify--test-desc))

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p))

  (with-file-notify-test
   (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpfile
	   '(change) #'file-notify--test-event-handler)))
   (should (file-notify-valid-p file-notify--test-desc))
   (file-notify--test-with-actions
       (cond
        ;; SMBSamba reports three `changed' events.
	((eq (file-notify--test-monitor) 'SMBSamba)
         '(changed changed changed changed deleted stopped))
	;; GFam{File,Directory}Monitor do not detect the `changed' event
        ;; reliably.
	((memq (file-notify--test-monitor)
               '(GFamFileMonitor GFamDirectoryMonitor))
	 '((deleted stopped)
	   (changed deleted stopped)))
	;; GKqueueFileMonitor does not report the `changed' event.
	((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	 '(deleted stopped))
	;; There could be one or two `changed' events.
	(t '((changed deleted stopped)
	     (changed changed deleted stopped))))
     (write-region "another text" nil file-notify--test-tmpfile nil 'no-message)
     (file-notify--test-wait-event)
     (delete-file file-notify--test-tmpfile))
   (file-notify--test-wait-event)
   ;; After deleting the file, the descriptor is not valid anymore.
   (should-not (file-notify-valid-p file-notify--test-desc))
   (file-notify-rm-watch file-notify--test-desc)

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p))

  (with-file-notify-test
   (delete-file file-notify--test-tmpfile)
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpdir
	   '(change) #'file-notify--test-event-handler)))
   (should (file-notify-valid-p file-notify--test-desc))
   (file-notify--test-with-actions
       (cond
	;; w32notify does not raise `deleted' and `stopped' events for
	;; the watched directory.
	((string-equal (file-notify--test-library) "w32notify")
	 '(created changed deleted))
        ;; SMBSamba reports three `changed' events.
	((eq (file-notify--test-monitor) 'SMBSamba)
         '(created changed changed changed deleted deleted stopped))
	;; There are two `deleted' events, for the file and for the
	;; directory.  Except for GFam{File,Directory}Monitor,
	;; GPollFileMonitor and kqueue.  And GFam{File,Directory}Monitor
	;; and GPollfileMonitor do not raise a `changed' event.
	((memq (file-notify--test-monitor)
               '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	 '(created deleted stopped))
	((string-equal (file-notify--test-library) "kqueue")
	 '(created changed deleted stopped))
	;; GKqueueFileMonitor does not report the `changed' event.
	((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	 '(created deleted deleted stopped))
	(t '(created changed deleted deleted stopped)))
     (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
     (file-notify--test-wait-event)
     (delete-directory file-notify--test-tmpdir 'recursive))
   ;; After deleting the parent directory, the descriptor must not be
   ;; valid anymore.
   (should-not (file-notify-valid-p file-notify--test-desc))
   ;; w32notify doesn't generate `stopped' events when the parent
   ;; directory is deleted, which doesn't provide a chance for
   ;; filenotify.el to remove the descriptor from the internal hash
   ;; table it maintains.  So we must remove the descriptor manually.
   (if (string-equal (file-notify--test-library) "w32notify")
       (file-notify--rm-descriptor file-notify--test-desc))

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p)))

(file-notify--deftest-remote file-notify-test05-file-validity
  "Check `file-notify-valid-p' via file notification for remote files.")

(ert-deftest file-notify-test06-dir-validity ()
  "Check `file-notify-valid-p' for directories."
  (skip-unless (file-notify--test-local-enabled))

  (with-file-notify-test
   (should
    (setq file-notify--test-desc
	  (file-notify-add-watch
           file-notify--test-tmpdir '(change) #'ignore)))
   (should (file-notify-valid-p file-notify--test-desc))
   ;; After removing the watch, the descriptor must not be valid
   ;; anymore.
   (file-notify-rm-watch file-notify--test-desc)
   (file-notify--test-wait-for-events
    (file-notify--test-timeout)
    (not (file-notify-valid-p file-notify--test-desc)))
   (should-not (file-notify-valid-p file-notify--test-desc))

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p))

  (with-file-notify-test
   (should
    (setq file-notify--test-desc
	  (file-notify-add-watch
	   file-notify--test-tmpdir '(change) #'ignore)))
   (should (file-notify-valid-p file-notify--test-desc))
   ;; After deleting the directory, the descriptor must not be valid
   ;; anymore.
   (delete-directory file-notify--test-tmpdir 'recursive)
   (file-notify--test-wait-for-events
    (file-notify--test-timeout)
    (not (file-notify-valid-p file-notify--test-desc)))
   (should-not (file-notify-valid-p file-notify--test-desc))
   (if (string-equal (file-notify--test-library) "w32notify")
       (file-notify--rm-descriptor file-notify--test-desc))

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p)))

(file-notify--deftest-remote file-notify-test06-dir-validity
  "Check `file-notify-valid-p' via file notification for remote directories.")

(ert-deftest file-notify-test07-many-events ()
  "Check that events are not dropped."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  (with-file-notify-test
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpdir
	   '(change) #'file-notify--test-event-handler)))
   (let ((file-notify-debug ;; Temporarily.
         (or file-notify-debug
             (and (getenv "EMACS_EMBA_CI")
                  (string-equal (file-notify--test-library) "gio")
                  (eq (file-notify--test-monitor) 'GInotifyFileMonitor))))
         (n 10);00)
         source-file-list target-file-list
         (default-directory file-notify--test-tmpdir))
     (dotimes (i n)
       ;; It matters which direction we rename, at least for kqueue.
       ;; This backend parses directories in alphabetic order (x%d
       ;; before y%d).  So we rename into both directions.
       (if (evenp i)
	   (progn
	     (push (expand-file-name (format "x%d" i)) source-file-list)
	     (push (expand-file-name (format "y%d" i)) target-file-list))
	 (push (expand-file-name (format "y%d" i)) source-file-list)
	 (push (expand-file-name (format "x%d" i)) target-file-list)))
     (file-notify--test-with-actions
	 (cond
          ;; SMBSamba fires both `created' and `changed' events.
	  ((eq (file-notify--test-monitor) 'SMBSamba)
	   (let (r)
	     (dotimes (_i (+ n n) r)
	       (setq r (append '(created changed) r)))))
	  (t (make-list (+ n n) 'created)))
       (let ((source-file-list source-file-list)
             (target-file-list target-file-list))
         (while (and source-file-list target-file-list)
           (file-notify--test-wait-event)
           (write-region "" nil (pop source-file-list) nil 'no-message)
           (file-notify--test-wait-event)
           (write-region "" nil (pop target-file-list) nil 'no-message))))
     (file-notify--test-with-actions
	 (cond
	  ;; w32notify fires both `deleted' and `renamed' events.
	  ((string-equal (file-notify--test-library) "w32notify")
	   (let (r)
	     (dotimes (_i n r)
	       (setq r (append '(deleted renamed) r)))))
          ;; SMBWindows fires both `changed' and `deleted' events.
	  ((eq (file-notify--test-monitor) 'SMBWindows)
	   (let (r)
	     (dotimes (_i n r)
	       (setq r (append '(changed changed changed deleted) r)))))
          ;; SMBSamba fires both `changed' and `deleted' events.
	  ((eq (file-notify--test-monitor) 'SMBSamba)
	   (let (r)
	     (dotimes (_i n r)
	       (setq r (append '(changed changed changed changed deleted) r)))))
          ;; GFam{File,Directory}Monitor and GPollFileMonitor fire
	  ;; `changed' and `deleted' events, sometimes in random order.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	   (let (r)
	     (dotimes (_i n (cons :random r))
	       (setq r (append '(changed deleted) r)))))
	  (t (make-list n 'renamed)))
       (let ((source-file-list source-file-list)
             (target-file-list target-file-list))
         (while (and source-file-list target-file-list)
           (file-notify--test-wait-event)
           (rename-file (pop source-file-list) (pop target-file-list) t))))
     (file-notify--test-with-actions (make-list n 'deleted)
       (dolist (file target-file-list)
         (file-notify--test-wait-event)
         (delete-file file)))
     (file-notify--rm-descriptor file-notify--test-desc)

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p))))

(file-notify--deftest-remote file-notify-test07-many-events
  "Check that events are not dropped for remote directories.")

(ert-deftest file-notify-test08-backup ()
  "Check that backup keeps file notification."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))

  (let ((file-notify-debug ;; Temporarily.
         (or file-notify-debug
             (getenv "EMACS_EMBA_CI"))))

  (with-file-notify-test
   (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
	   file-notify--test-tmpfile
	   '(change) #'file-notify--test-event-handler)))
   (should (file-notify-valid-p file-notify--test-desc))
   (file-notify--test-with-actions
       (cond
        ;; SMBSamba reports four `changed' events.
	((eq (file-notify--test-monitor) 'SMBSamba)
         '(changed changed changed changed))
	;; GKqueueFileMonitor does not report the `changed' event.
	((eq (file-notify--test-monitor) 'GKqueueFileMonitor) '())
        ;; There could be one or two `changed' events.
	(t '((changed)
	     (changed changed))))
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
   (file-notify--rm-descriptor file-notify--test-desc)

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p))

  (with-file-notify-test
   ;; It doesn't work for kqueue, because we don't use an implicit
   ;; directory monitor.
   (unless (string-equal (file-notify--test-library) "kqueue")
     (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
     (should
      (setq file-notify--test-desc
            (file-notify--test-add-watch
             file-notify--test-tmpfile
             '(change) #'file-notify--test-event-handler)))
     (should (file-notify-valid-p file-notify--test-desc))
     (file-notify--test-with-actions
         (cond
          ;; SMBWindows reports two `changed' events.
	  ((eq (file-notify--test-monitor) 'SMBWindows)
           '(changed changed))
          ;; SMBSamba reports four `changed' events.
	  ((eq (file-notify--test-monitor) 'SMBSamba)
           '(changed changed changed changed))
	  ;; GFam{File,Directory}Monitor and GPollFileMonitor
	  ;; report only the `changed' event.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
	   '(changed))
	  ;; GKqueueFileMonitor does not report the `changed' event.
	  ((eq (file-notify--test-monitor) 'GKqueueFileMonitor)
	   '(renamed created))
          (t '(renamed created changed)))
       ;; The file is renamed when creating a backup.  It shall still be
       ;; watched.
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
     (file-notify--rm-descriptor file-notify--test-desc)

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p)))))

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

  (with-file-notify-test
   (write-region "any text" nil file-notify--test-tmpfile nil 'no-message)
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
             file-notify--test-tmpdir
             '(change) #'dir-callback)
            ;; This is needed for `file-notify--test-monitor'.
            file-notify--test-desc file-notify--test-desc1))
     (should
      (setq file-notify--test-desc2
            (file-notify--test-add-watch
             file-notify--test-tmpfile
             '(change) #'file-callback)))
     (should (file-notify-valid-p file-notify--test-desc1))
     (should (file-notify-valid-p file-notify--test-desc2))
     (should-not (equal file-notify--test-desc1 file-notify--test-desc2))
     (let ((n 10));0))
       ;; Run the test.
       (file-notify--test-with-actions
           ;; There could be one or two `changed' events.
           (list
            ;; SMBSamba.  Sometimes, tha last `changed' event is
            ;; missing, so we add two alternatives.
            (append
             '(:random)
             ;; Just the file monitor.
             (make-list (* (/ n 2) 5) 'changed)
             ;; Just the directory monitor.  Strange, not all `changed'
             ;; events do arrive.
             (make-list (1- (* (/ n 2) 10)) 'changed)
             (make-list (/ n 2) 'created)
             (make-list (/ n 2) 'created))
            (append
             '(:random)
             ;; Just the file monitor.
             (make-list (* (/ n 2) 5) 'changed)
             ;; Just the directory monitor.  This is the alternative
             ;; with all `changed' events.
             (make-list (* (/ n 2) 10) 'changed)
             (make-list (/ n 2) 'created)
             (make-list (/ n 2) 'created))
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
             (make-list (/ n 2) 'changed))
	    (append
             '(:random)
	     ;; Just the directory monitor.  GKqueueFileMonitor does not
	     ;; report the `changed' event.
             (make-list (/ n 2) 'created)))
         (dotimes (i n)
           (file-notify--test-wait-event)
           (if (evenp i)
               (write-region
                "any text" nil file-notify--test-tmpfile t 'no-message)
             (write-region
              "any text" nil
              (file-notify--test-make-temp-name) nil 'no-message)))))

     ;; If we delete the file, the directory monitor shall still be
     ;; active.  We receive the `deleted' event from both the directory
     ;; and the file monitor.  The `stopped' event is from the file
     ;; monitor.  It's undecided in which order the directory and the
     ;; file monitor are triggered.
     (file-notify--test-with-actions
         '((:random deleted deleted stopped)
           (:random deleted deleted deleted stopped))
       (delete-file file-notify--test-tmpfile))
     (should (file-notify-valid-p file-notify--test-desc1))
     (unless (string-equal (file-notify--test-library) "w32notify")
       (should-not (file-notify-valid-p file-notify--test-desc2)))

     ;; Now we delete the directory.
     (file-notify--test-with-actions
         (cond
          ;; GFam{File,Directory}Monitor, GPollFileMonitor and kqueue
          ;; raise just one `deleted' event for the directory.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor GPollFileMonitor))
           '(deleted stopped))
	  ((string-equal (file-notify--test-library) "kqueue")
           '(deleted stopped))
          (t (append
              ;; The directory monitor raises a `deleted' event for
              ;; every file contained in the directory, we must count
              ;; them.
              (make-list
               (length
                (directory-files
                 file-notify--test-tmpdir nil
                 directory-files-no-dot-files-regexp 'nosort))
               'deleted)
              ;; The events of the directory itself.
              (cond
	       ;; w32notify does not raise `deleted' and `stopped'
	       ;; events for the watched directory.
               ((string-equal (file-notify--test-library) "w32notify")
                '())
               (t '(deleted stopped))))))
       (delete-directory file-notify--test-tmpdir 'recursive))
     (unless (getenv "EMACS_EMBA_CI")
       (should-not (file-notify-valid-p file-notify--test-desc1))
       (should-not (file-notify-valid-p file-notify--test-desc2)))
     (when (string-equal (file-notify--test-library) "w32notify")
       (file-notify--rm-descriptor file-notify--test-desc1)
       (file-notify--rm-descriptor file-notify--test-desc2))

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p))))

(file-notify--deftest-remote file-notify-test09-watched-file-in-watched-dir
  "Check `file-notify-test09-watched-file-in-watched-dir' for remote files.")

(ert-deftest file-notify-test10-sufficient-resources ()
  "Check that file notification does not use too many resources."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))
  ;; This test is intended for kqueue only.  We cannot check for
  ;; GKqueueFileMonitor, because `file-notify--test-desc' is not set yet.
  (skip-unless (string-equal (file-notify--test-library) "kqueue"))

  (with-file-notify-test
   (let (descs tmpfile)
     (should-error
      (while t
	;; We watch directories, because we want to reach the upper
	;; limit.  Watching a file might not be sufficient, because most
	;; of the libraries implement this as watching the upper
	;; directory.
	(setq tmpfile
	      (make-temp-file
               (expand-file-name
                "file-notify-test-parent" file-notify--test-tmpdir)
               t)
	      descs
	      (cons
	       (should (file-notify-add-watch tmpfile '(change) #'ignore))
	       descs)))
      :type 'file-notify-error)
     ;; Remove watches.  If we don't do it prior removing directories,
     ;; Emacs crashes in batch mode.
     (dolist (desc descs)
       (file-notify-rm-watch desc))

     ;; The environment shall be cleaned up.
     (file-notify--test-cleanup-p))))

(file-notify--deftest-remote file-notify-test10-sufficient-resources
  "Check `file-notify-test10-sufficient-resources' for remote files.")

(ert-deftest file-notify-test11-symlinks ()
  "Check that file notification do not follow symbolic links."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))
  ;; This test does not work for kqueue (yet).
  (skip-when (string-equal (file-notify--test-library) "kqueue"))

  (with-file-notify-test
   (ert-with-temp-file file-notify--test-tmpfile1
     :prefix ert-temp-file-prefix
     (delete-file file-notify--test-tmpfile)
     ;; Symlink a file.
     (write-region "any text" nil file-notify--test-tmpfile1 nil 'no-message)
     ;; Some systems, like MS Windows without sufficient privileges, do
     ;; not allow creation of symbolic links.
     (condition-case nil
         (make-symbolic-link
          file-notify--test-tmpfile1 file-notify--test-tmpfile)
       (error (ert-skip "`make-symbolic-link' not supported")))
     (should
      (setq file-notify--test-desc
	    (file-notify--test-add-watch
             file-notify--test-tmpfile
             '(attribute-change change) #'file-notify--test-event-handler)))
     (should (file-notify-valid-p file-notify--test-desc))

     ;; Writing to either the symlink or the target should not raise any
     ;; event.
     (file-notify--test-with-actions nil
       (write-region
        "another text" nil file-notify--test-tmpfile nil 'no-message)
       (write-region
        "another text" nil file-notify--test-tmpfile1 nil 'no-message))
     ;; Sanity check.
     (file-notify--test-wait-for-events
      (file-notify--test-timeout)
      (not (input-pending-p)))
     (should-not file-notify--test-events)

     ;; Changing timestamp of the target should not raise any event.  We
     ;; don't use `nofollow'.
     (file-notify--test-with-actions nil
       (set-file-times file-notify--test-tmpfile1 '(0 0))
       (set-file-times file-notify--test-tmpfile '(0 0)))
     ;; Sanity check.
     (file-notify--test-wait-for-events
      (file-notify--test-timeout)
      (not (input-pending-p)))
     (should-not file-notify--test-events)

     ;; Changing timestamp of the symlink shows the event.
     (file-notify--test-with-actions
	 (cond
	  ;; w32notify does not distinguish between `changed' and
	  ;; `attribute-changed'.
	  ((string-equal (file-notify--test-library) "w32notify")
	   '(changed))
	  ;; GFam{File,Directory}Monitor, GKqueueFileMonitor and
	  ;; GPollFileMonitor do not report the `attribute-changed'
	  ;; event.
	  ((memq (file-notify--test-monitor)
                 '(GFamFileMonitor GFamDirectoryMonitor
                   GKqueueFileMonitor GPollFileMonitor))
           '())
          (t '(attribute-changed)))
       (set-file-times file-notify--test-tmpfile '(0 0) 'nofollow))

     ;; Deleting the target should not raise any event.
     (file-notify--test-with-actions nil
       (delete-file file-notify--test-tmpfile1)
       (delete-file file-notify--test-tmpfile))
     ;; Sanity check.
     (file-notify--test-wait-for-events
      (file-notify--test-timeout)
      (not (input-pending-p)))
     (should-not file-notify--test-events)

     ;; The environment shall be cleaned up.
     (file-notify-rm-watch file-notify--test-desc)
     (file-notify--test-cleanup-p)))

  (with-file-notify-test
   (ert-with-temp-directory file-notify--test-tmpfile1
     :prefix (concat ert-temp-file-prefix "-parent")
     (delete-file file-notify--test-tmpfile)
     ;; Symlink a directory.
     (let ((tmpfile (expand-file-name "foo" file-notify--test-tmpfile))
           (tmpfile1 (expand-file-name "foo" file-notify--test-tmpfile1)))
       (make-symbolic-link file-notify--test-tmpfile1 file-notify--test-tmpfile)
       (write-region "any text" nil tmpfile1 nil 'no-message)
       (should
	(setq file-notify--test-desc
	      (file-notify--test-add-watch
               file-notify--test-tmpfile
               '(attribute-change change) #'file-notify--test-event-handler)))
       (should (file-notify-valid-p file-notify--test-desc))

       ;; None of the actions on a file in the symlinked directory
       ;; will be reported.
       (file-notify--test-with-actions nil
         (write-region "another text" nil tmpfile nil 'no-message)
         (write-region "another text" nil tmpfile1 nil 'no-message)
         (set-file-times tmpfile '(0 0))
         (set-file-times tmpfile '(0 0) 'nofollow)
         (set-file-times tmpfile1 '(0 0))
         (set-file-times tmpfile1 '(0 0) 'nofollow)
         (delete-file tmpfile)
         (delete-file tmpfile1))
       ;; Sanity check.
       (file-notify--test-wait-for-events
        (file-notify--test-timeout)
        (not (input-pending-p)))
       (should-not file-notify--test-events)

       ;; The environment shall be cleaned up.
       (delete-directory file-notify--test-tmpdir 'recursive)
       (file-notify-rm-watch file-notify--test-desc)
       (file-notify--test-cleanup-p)))))

(file-notify--deftest-remote file-notify-test11-symlinks
  "Check `file-notify-test11-symlinks' for remote files.")

(ert-deftest file-notify-test12-unmount ()
  "Check that file notification stop after unmounting the filesystem."
  :tags '(:expensive-test)
  (skip-unless (file-notify--test-local-enabled))
  ;; This test does not work for w32notify snd smb-notify.
  (skip-when (member (file-notify--test-library) '("w32notify" "smb-notify")))

  (with-file-notify-test
   (should
    (setq file-notify--test-desc
	  (file-notify--test-add-watch
           file-notify--test-tmpfile
           '(attribute-change change) #'file-notify--test-event-handler)))
   (should (file-notify-valid-p file-notify--test-desc))

   ;; Unmounting the filesystem should stop watching.
   (file-notify--test-with-actions '(stopped)
     ;; We emulate unmounting by inserting a corresponding event.
     (insert-special-event
      (make-file-notify
       :-event
       (list file-notify--test-desc
             (pcase (file-notify--test-library)
               ((or "inotify" "inotifywait") '(unmount isdir))
               ((or "gfilenotify" "gio") '(unmounted))
               ("kqueue" '(revoke))
               (err (ert-fail (format "Library %s not supported" err))))
             (pcase (file-notify--test-library)
               ("kqueue" (file-local-name file-notify--test-tmpfile))
               (_ (file-local-name file-notify--test-tmpdir)))
             ;; In the inotify case, there is a 4th slot `cookie'.
             ;; Since it is unused for `unmount', we ignore it.
             )
       :-callback
       (pcase (file-notify--test-library)
         ("inotify" #'file-notify--callback-inotify)
         ("gfilenotify" #'file-notify--callback-gfilenotify)
         ("kqueue" #'file-notify--callback-kqueue)
         ((or "inotifywait" "gio") #'file-notify-callback)
         (err (ert-fail (format "Library %s not supported" err)))))))

   ;; The watch has been stopped.
   (should-not (file-notify-valid-p file-notify--test-desc))

   ;; The environment shall be cleaned up.
   (file-notify--test-cleanup-p)))

(file-notify--deftest-remote file-notify-test12-unmount
  "Check `file-notify-test12-unmount' for remote files.")

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
;; * For cygwin, w32notify, and smb-notify, no `attribute-changed'
;;   events arrive.  They send `changed' events instead.
;; * cygwin does not send all expected `changed' and `deleted' events.
;;   Probably due to timing issues.

(provide 'filenotify-tests)
;;; filenotify-tests.el ends here
