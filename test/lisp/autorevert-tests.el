;;; autorevert-tests.el --- Tests of auto-revert   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

;; For the remote file-notify library, Tramp checks for the existence
;; of a respective command.  The first command found is used.  In
;; order to use a dedicated one, the environment variable
;; $REMOTE_FILE_NOTIFY_LIBRARY shall be set, possible values are
;; "inotifywait", "gio" and "smb-notify".

;; Local file-notify libraries are auto-detected during Emacs
;; configuration.  This can be changed with a respective configuration
;; argument, like
;;
;;   --with-file-notification=inotify
;;   --with-file-notification=kqueue
;;   --with-file-notification=gfile
;;   --with-file-notification=w32

;; A whole test run can be performed calling the command `auto-revert-test-all'.

;;; Code:

(require 'tramp)
(require 'ert-x)
(require 'autorevert)

(setq auth-source-cache-expiry nil
      auth-source-save-behavior nil
      auto-revert-debug nil
      auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-stop-on-user-input nil
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

(defvar auto-revert--test-rootdir temporary-file-directory)
(defvar auto-revert--test-monitors nil)

(defun auto-revert--timeout ()
  "Time to wait for a message."
  (+ auto-revert-interval 1))

(defvar auto-revert--messages nil
  "Used to collect messages issued during a section of a test.")

;; Filter suppressed remote file-notify libraries.
(when (stringp (getenv "REMOTE_FILE_NOTIFY_LIBRARY"))
  (dolist (lib '("inotifywait" "gio" "smb-notify"))
    (unless (string-equal (getenv "REMOTE_FILE_NOTIFY_LIBRARY") lib)
      (add-to-list 'tramp-connection-properties `(nil ,lib nil)))))

(defvar auto-revert--test-enabled-remote-checked nil
  "Cached result of `auto-revert--test-enabled-remote'.
If the function did run, the value is a cons cell, the `cdr'
being the result.")

(defun auto-revert--test-enabled-remote ()
  "Whether remote file access is enabled."
  (unless (consp auto-revert--test-enabled-remote-checked)
    (setq
     auto-revert--test-enabled-remote-checked
     (cons
      t (ignore-errors
	  (and
	   (file-remote-p ert-remote-temporary-file-directory)
	   (file-directory-p ert-remote-temporary-file-directory)
	   (file-writable-p ert-remote-temporary-file-directory))))))
  ;; Return result.
  (cdr auto-revert--test-enabled-remote-checked))

(defun auto-revert--wait-for-revert (buffer)
  "Wait until a message reports reversion of BUFFER.
This expects `auto-revert--messages' to be bound by
`ert-with-message-capture' before calling."
  ;; This is a central place to check for proper library and monitor.
  (auto-revert--skip-unless-proper-library-and-monitor buffer)
  (let ((text-quoting-style 'grave))
    (auto-revert-test--wait-for
     (lambda () (string-match-p
                 (rx bol "Reverting buffer `"
                     (literal (buffer-name buffer)) "'" eol)
                 (or auto-revert--messages "")))
     (auto-revert--timeout))))

(defun auto-revert--test-library ()
  "The used library for the test, as a string.
In the remote case, it is the process name which runs on the
remote host, or nil."
  (if (null (file-remote-p auto-revert--test-rootdir))
      (symbol-name file-notify--library)
    (and (processp auto-revert-notify-watch-descriptor)
	 (replace-regexp-in-string
	  "<[[:digit:]]+>\\'" ""
	  (process-name auto-revert-notify-watch-descriptor)))))

(defun auto-revert--test-monitor ()
  "The used monitor for the test, as a symbol.
This returns only for (local) gfilenotify, (remote) gio or (remote)
smb-notify libraries; otherwise it is nil.
`auto-revert-notify-watch-descriptor' must be a valid watch descriptor."
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
  (when auto-revert-notify-watch-descriptor
    (or (alist-get
         auto-revert-notify-watch-descriptor auto-revert--test-monitors)
        (when (member
               (auto-revert--test-library) '("gfilenotify" "gio" "smb-notify"))
	  (add-to-list
	   'auto-revert--test-monitors
	   (cons auto-revert-notify-watch-descriptor
	         (if (file-remote-p auto-revert--test-rootdir)
                     ;; `auto-revert-notify-watch-descriptor' is the
                     ;; connection process.
                     (progn
                       (while (and (process-live-p
                                    auto-revert-notify-watch-descriptor)
                                   (not (tramp-connection-property-p
		                         auto-revert-notify-watch-descriptor
                                         "file-monitor")))
                         (accept-process-output
                          auto-revert-notify-watch-descriptor 0))
		       (tramp-get-connection-property
		        auto-revert-notify-watch-descriptor "file-monitor"))
		   (and (functionp 'gfile-monitor-name)
		        (gfile-monitor-name
                         auto-revert-notify-watch-descriptor)))))
          ;; If we don't know the monitor, there are good chances the
          ;; test will fail.  We skip it.
          (unless (alist-get
                   auto-revert-notify-watch-descriptor auto-revert--test-monitors)
            (ert-skip "Cannot determine test monitor")))
	(alist-get
         auto-revert-notify-watch-descriptor auto-revert--test-monitors))))

(defun auto-revert--skip-unless-proper-library-and-monitor (&optional buffer)
  "Skip unless there is no proper file notification library.
It is checked for buffer-local `auto-revert-notify-watch-descriptor'."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq (auto-revert--test-monitor) 'GKqueueFileMonitor)
      (ert-skip (format "Monitor %s does not support auto-revert"
                        (auto-revert--test-monitor))))))

(defmacro auto-revert--deftest-remote (test docstring)
  "Define ert `TEST-remote' for remote files."
  (declare (indent 1) (debug (symbolp stringp)))
  `(ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
     ,docstring
     :tags '(:expensive-test)
     (let ((auto-revert--test-rootdir
	    ert-remote-temporary-file-directory)
           (auto-revert-remote-files t)
	   (ert-test (ert-get-test ',test))
           vc-handled-backends)
       (skip-unless (auto-revert--test-enabled-remote))
       (tramp-cleanup-connection
	(tramp-dissect-file-name auto-revert--test-rootdir) t 'keep-password)
       (condition-case err
           (funcall (ert-test-body ert-test))
         (error (message "%s" err) (signal (car err) (cdr err)))))))

(defmacro with-auto-revert-test (&rest body)
  (declare (debug t))
  `(let ((auto-revert-interval-orig auto-revert-interval)
         (auto-revert--lockout-interval-orig auto-revert--lockout-interval)
         (ert-temp-file-prefix
          (expand-file-name "auto-revert-test" auto-revert--test-rootdir)))
     (unwind-protect
         (progn
           (unless (file-remote-p auto-revert--test-rootdir)
             (customize-set-variable 'auto-revert-interval 0.1)
             (setq auto-revert--lockout-interval 0.05))
           ,@body)
       (customize-set-variable 'auto-revert-interval auto-revert-interval-orig)
       (setq auto-revert--lockout-interval auto-revert--lockout-interval-orig)
       (ignore-errors (file-notify-rm-all-watches))
       (sleep-for 1))))

(defun auto-revert-test--write-region (string file &optional append)
  "Write STRING to FILE."
  (write-region string nil file append 'no-message))

(defun auto-revert-test--write-file (string file &optional append)
  "Write STRING to FILE.
Set modified file time in order to trigger auto-revert."
  (auto-revert-test--write-region string file append)
  (set-file-times file (time-subtract nil (seconds-to-time 60))))

(ert-deftest auto-revert-test00-auto-revert-mode ()
  "Check autorevert for a file."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     :prefix ert-temp-file-prefix
     (let (buf)
       (unwind-protect
           (progn
             (auto-revert-test--write-region "any text" tmpfile)
             (setq buf (find-file-noselect tmpfile))
             (with-current-buffer buf
               (ert-with-message-capture auto-revert--messages
                 (should
                  (string-equal
                   (substring-no-properties (buffer-string)) "any text"))
                 ;; `buffer-stale--default-function' checks for
                 ;; `verify-visited-file-modtime'.  We must ensure that it
                 ;; returns nil.  We also clean up.
                 (auto-revert-mode 1)
                 (should auto-revert-mode)
                 ;; There was already an initial call of
                 ;; `auto-revert-handler', which locks file
                 ;; notification.  Reset this lock.
                 (setq auto-revert--last-time 0)
                 (auto-revert-test--wait-for
                  (lambda () (null auto-revert--lockout-timer))
                  (auto-revert--timeout))

                 (auto-revert-test--write-file "another text" tmpfile)

                 ;; Check, that the buffer has been reverted.
                 (auto-revert--wait-for-revert buf))
               (should
                (string-match-p
                 "another text" (substring-no-properties (buffer-string))))

               ;; When the buffer is modified, it shall not be reverted.
               (ert-with-message-capture auto-revert--messages
                 (set-buffer-modified-p t)
                 (auto-revert-test--write-file "any text" tmpfile)

                 ;; Check, that the buffer hasn't been reverted.
                 (auto-revert--wait-for-revert buf))
               (should-not
                (string-match-p
                 "any text" (substring-no-properties (buffer-string))))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test00-auto-revert-mode
  "Check autorevert for a remote file.")

;; This is inspired by Bug#21841.
(ert-deftest auto-revert-test01-auto-revert-several-files ()
  "Check autorevert for several files at once."
  (let ((default-directory auto-revert--test-rootdir))
    (skip-unless
     (executable-find "cp" (file-remote-p auto-revert--test-rootdir))))

  (with-auto-revert-test
   (ert-with-temp-directory tmpdir1
     :prefix (concat ert-temp-file-prefix "-parent")
     (ert-with-temp-directory tmpdir2
       :prefix (concat ert-temp-file-prefix "-parent")
       (ert-with-temp-file tmpfile1
         :prefix (expand-file-name "auto-revert-test" tmpdir1)
         (ert-with-temp-file tmpfile2
           :prefix (expand-file-name "auto-revert-test" tmpdir2)
           (let (buf1 buf2)
             (unwind-protect
                 (ert-with-message-capture auto-revert--messages
                   (auto-revert-test--write-region "any text" tmpfile1)
                   (setq buf1 (find-file-noselect tmpfile1))
                   (auto-revert-test--write-region "any text" tmpfile2)
                   (setq buf2 (find-file-noselect tmpfile2))

                   (dolist (buf (list buf1 buf2))
                     (with-current-buffer buf
                       (should
                        (string-equal
                         (substring-no-properties (buffer-string)) "any text"))
                       ;; `buffer-stale--default-function' checks for
                       ;; `verify-visited-file-modtime'.  We must ensure that
                       ;; it returns nil.
                       (auto-revert-mode 1)
                       (should auto-revert-mode)))

                   ;; Modify files.  We wait for a second, in order to have
                   ;; another timestamp.
                   ;; `tmpdir2' is not under auto-revert.
                   (auto-revert-test--write-file
                    "another text"
                    (expand-file-name (file-name-nondirectory tmpfile1) tmpdir2))
                   (auto-revert-test--write-file
                    "another text"
                    (expand-file-name (file-name-nondirectory tmpfile2) tmpdir2))

                   ;; (copy-directory tmpdir2 tmpdir1 'keep 'copy-contents)
                   ;; Strange, that `copy-directory' does not work as
                   ;; expected.  The following shell command is not
                   ;; portable on all platforms, unfortunately.
                   (let ((default-directory auto-revert--test-rootdir))
                     (shell-command
                      (format "%s -pf %s/* %s"
                              (executable-find "cp" t)
                              (file-local-name tmpdir2)
                              (file-local-name tmpdir1))))

                   ;; Check, that the buffers have been reverted.
                   (dolist (buf (list buf1 buf2))
                     (with-current-buffer buf
                       (auto-revert--wait-for-revert buf)
                       (should
                        (string-match-p
                         "another text"
                         (substring-no-properties (buffer-string)))))))

               ;; Exit.
               (ignore-errors
                 (dolist (buf (list buf1 buf2))
                   (with-current-buffer buf (set-buffer-modified-p nil))
                   (kill-buffer buf)))))))))))

(auto-revert--deftest-remote auto-revert-test01-auto-revert-several-files
  "Check autorevert for several remote files at once.")

;; This is inspired by Bug#23276.
(ert-deftest auto-revert-test02-auto-revert-deleted-file ()
  "Check autorevert for a deleted file."
  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     :prefix ert-temp-file-prefix
     (let (buf)
       (unwind-protect
           (progn
             (auto-revert-test--write-region "any text" tmpfile)
             (setq buf (find-file-noselect tmpfile))
             (with-current-buffer buf
               (should-not
                (file-notify-valid-p auto-revert-notify-watch-descriptor))
               (should
                (string-equal
                 (substring-no-properties (buffer-string)) "any text"))
               ;; `buffer-stale--default-function' checks for
               ;; `verify-visited-file-modtime'.  We must ensure that
               ;; it returns nil.
               (auto-revert-mode 1)
               (should auto-revert-mode)

               ;; Remove file while reverting.  We simulate this by
               ;; modifying `before-revert-hook'.
               (add-hook
                'before-revert-hook
                (lambda ()
                  (when auto-revert-debug
                    (message "before-revert-hook %s deleted" buffer-file-name))
                  (delete-file buffer-file-name))
                nil t)

               (ert-with-message-capture auto-revert--messages
                 (auto-revert-test--write-file "another text" tmpfile)
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer hasn't been reverted.  File
               ;; notification should be disabled, falling back to
               ;; polling.
               (should
                (string-match-p
                 "any text" (substring-no-properties (buffer-string))))

               ;; Once the file has been recreated, the buffer shall be
               ;; reverted.
               (kill-local-variable 'before-revert-hook)
               (ert-with-message-capture auto-revert--messages
                 (auto-revert-test--write-file "another text" tmpfile)
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer has been reverted.
               (should
                (string-match-p
                 "another text" (substring-no-properties (buffer-string))))

               ;; An empty file shall still be reverted.
               (ert-with-message-capture auto-revert--messages
                 (auto-revert-test--write-file "" tmpfile)
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer has been reverted.
               (should
                (string-equal "" (substring-no-properties (buffer-string))))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test02-auto-revert-deleted-file
  "Check autorevert for a deleted remote file.")

(ert-deftest auto-revert-test03-auto-revert-tail-mode ()
  "Check autorevert tail mode."
  (with-auto-revert-test
   ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
   ;; file has been reverted.
   (ert-with-temp-file tmpfile
     :prefix ert-temp-file-prefix
     (let (buf)
       (unwind-protect
           (ert-with-message-capture auto-revert--messages
             (auto-revert-test--write-region "any text" tmpfile)
             (setq buf (find-file-noselect tmpfile))
             (with-current-buffer buf
               ;; `buffer-stale--default-function' checks for
               ;; `verify-visited-file-modtime'.  We must ensure that it
               ;; returns nil.
               (auto-revert-tail-mode 1)
               (should auto-revert-tail-mode)
               (erase-buffer)
               (insert "modified text\n")
               (set-buffer-modified-p nil)

               ;; Modify file.
               (auto-revert-test--write-file "another text" tmpfile 'append)

               ;; Check, that the buffer has been reverted.
               (auto-revert--wait-for-revert buf)
               (should
                (string-match-p
                 "modified text\nanother text"
                 (substring-no-properties (buffer-string))))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test03-auto-revert-tail-mode
  "Check remote autorevert tail mode.")

(ert-deftest auto-revert-test04-auto-revert-mode-dired ()
  "Check autorevert for dired."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (with-auto-revert-test
   (ert-with-temp-directory tmpdir
     :prefix (concat ert-temp-file-prefix "-parent")
     (ert-with-temp-file tmpfile
       :prefix (expand-file-name "auto-revert-test" tmpdir)
       (let ((name (file-name-nondirectory tmpfile))
             buf)
         (unwind-protect
             (progn
               (setq buf (dired-noselect tmpdir))
               (with-current-buffer buf
                 ;; `buffer-stale--default-function' checks for
                 ;; `verify-visited-file-modtime'.  We must ensure that
                 ;; it returns nil.
                 (auto-revert-mode 1)
                 (should auto-revert-mode)
                 (should
                  (string-match-p
                   (rx bow (literal name) eow)
                   (substring-no-properties (buffer-string))))
                 ;; If we don't sleep for a while, this test fails on
                 ;; MS-Windows.
                 (if (eq system-type 'windows-nt)
                     (sleep-for 0.5))

                 ;; File stamps of remote files have an accuracy of 1
                 ;; second.  Wait a little bit.
                 (when (file-remote-p tmpfile)
                   (sleep-for (auto-revert--timeout)))
                 (ert-with-message-capture auto-revert--messages
                   ;; Delete file.
                   (delete-file tmpfile)
                   (auto-revert--wait-for-revert buf))
                 (if (eq system-type 'windows-nt)
                     (sleep-for 1))
                 ;; Check, that the buffer has been reverted.
                 (should-not
                  (string-match-p
                   (rx bow (literal name) eow)
                   (substring-no-properties (buffer-string))))

                 ;; File stamps of remote files have an accuracy of 1
                 ;; second.  Wait a little bit.
                 (when (file-remote-p tmpfile)
                   (sleep-for (auto-revert--timeout)))
                 (ert-with-message-capture auto-revert--messages
                   ;; Make dired buffer modified.  Check, that the
                   ;; buffer has been still reverted.
                   (set-buffer-modified-p t)
                   (auto-revert-test--write-region "any text" tmpfile)
                   (auto-revert--wait-for-revert buf))
                 ;; Check, that the buffer has been reverted.
                 (should
                  (string-match-p
                   (rx bow (literal name) eow)
                   (substring-no-properties (buffer-string))))))

           ;; Exit.
           (ignore-errors
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))))))

(auto-revert--deftest-remote auto-revert-test04-auto-revert-mode-dired
  "Check remote autorevert for dired.")

(defun auto-revert-test--buffer-string (buffer)
  "Contents of BUFFER as a string."
  (with-current-buffer buffer
    (substring-no-properties (buffer-string))))

(defun auto-revert-test--wait-for (pred max-wait)
  "Wait until PRED is true, or MAX-WAIT seconds elapsed."
  (let ((ct (current-time)))
    (while (and (< (float-time (time-subtract nil ct)) max-wait)
                (not (funcall pred)))
      (read-event nil nil 0.1))))

(defun auto-revert-test--wait-for-buffer-text (buffer string max-wait)
  "Wait until BUFFER has the contents STRING, or MAX-WAIT seconds elapsed."
  (auto-revert-test--wait-for
   (lambda () (string-equal (auto-revert-test--buffer-string buffer) string))
   max-wait))

(defun auto-revert-test--instrument-kill-buffer-hook (buffer)
  "Instrument local `kill-buffer-hook' with messages."
  (when auto-revert-debug
    (with-current-buffer buffer
      (add-hook
       'kill-buffer-hook
       (lambda ()
         (message
          "%S killed\n%s" (current-buffer) (with-output-to-string (backtrace))))
       nil 'local))))

(ert-deftest auto-revert-test05-global-notify ()
  "Test `global-auto-revert-mode'."
  (skip-unless (or file-notify--library
                   (file-remote-p auto-revert--test-rootdir)))

  (with-auto-revert-test
   (ert-with-temp-file file-1
     :prefix ert-temp-file-prefix
     (ert-with-temp-file file-2
       :prefix ert-temp-file-prefix
       (ert-with-temp-file file-3
         :prefix ert-temp-file-prefix
         (let ((auto-revert-use-notify t)
               (was-in-global-auto-revert-mode global-auto-revert-mode)
               (file-2b (concat file-2 "-b"))
               require-final-newline buf-1 buf-2 buf-3)
           (unwind-protect
               (progn
                 (setq buf-1 (find-file-noselect file-1))
                 (auto-revert-test--instrument-kill-buffer-hook buf-1)
		 (should-not (buffer-local-value 'auto-revert-mode buf-1))
		 (should-not (buffer-local-value 'auto-revert--global-mode buf-1))
                 (setq buf-2 (find-file-noselect file-2))
                 (auto-revert-test--instrument-kill-buffer-hook buf-2)
		 (should-not (buffer-local-value 'auto-revert-mode buf-2))
		 (should-not (buffer-local-value 'auto-revert--global-mode buf-2))
                 ;; File stamps of remote files have an accuracy of 1
                 ;; second.  Wait a little bit.
                 (when (file-remote-p file-1)
                   (sleep-for (auto-revert--timeout)))
                 (auto-revert-test--write-file "1-a" file-1)
                 (should
                  (string-equal (auto-revert-test--buffer-string buf-1) ""))

                 (global-auto-revert-mode 1) ; Turn it on.
                 (auto-revert--skip-unless-proper-library-and-monitor buf-1)
		 (should-not (buffer-local-value 'auto-revert-mode buf-1))
		 (should (buffer-local-value 'auto-revert--global-mode buf-1))
		 (should-not (buffer-local-value 'auto-revert-mode buf-2))
		 (should (buffer-local-value 'auto-revert--global-mode buf-2))

                 ;; buf-1 should have been reverted immediately when the mode
                 ;; was enabled.
                 (should
                  (string-equal (auto-revert-test--buffer-string buf-1) "1-a"))

                 ;; Alter a file.
                 (auto-revert-test--write-region "2-a" file-2)
                 (auto-revert-test--wait-for-buffer-text
                  buf-2 "2-a" (auto-revert--timeout))
                 (should
                  (string-equal (auto-revert-test--buffer-string buf-2) "2-a"))

                 ;; Visit a file, and modify it on disk.
                 (setq buf-3 (find-file-noselect file-3))
                 (auto-revert-test--instrument-kill-buffer-hook buf-3)
		 (should-not (buffer-local-value 'auto-revert-mode buf-3))
		 (should (buffer-local-value 'auto-revert--global-mode buf-3))
                 (auto-revert-test--write-region "3-a" file-3)
                 (auto-revert-test--wait-for-buffer-text
                  buf-3 "3-a" (auto-revert--timeout))
                 (should
                  (string-equal (auto-revert-test--buffer-string buf-3) "3-a"))

                 ;; Delete a visited file, and re-create it with new contents.
                 (delete-file file-1)
                 (should
                  (string-equal (auto-revert-test--buffer-string buf-1) "1-a"))
                 (auto-revert-test--write-region "1-b" file-1)
                 ;; Since the file is deleted, it needs at least
                 ;; `auto-revert-interval' to recognize the new file,
                 ;; while polling.  So increase the timeout.
                 (auto-revert-test--wait-for-buffer-text
                  buf-1 "1-b" (* 2 (auto-revert--timeout)))

                 ;; Write a buffer to a new file, then modify the new
                 ;; file on disk.
                 (with-current-buffer buf-2
                   (write-file file-2b))
                 (should
                  (string-equal (auto-revert-test--buffer-string buf-2) "2-a"))
                 (auto-revert-test--write-region "2-b" file-2b)
                 (auto-revert-test--wait-for-buffer-text
                  buf-2 "2-b" (auto-revert--timeout)))

             ;; Clean up.
             (unless was-in-global-auto-revert-mode
               (global-auto-revert-mode 0)) ; Turn it off.
             (dolist (buf (list buf-1 buf-2 buf-3))
               (ignore-errors
                 (with-current-buffer buf (setq-local kill-buffer-hook nil))
                 (kill-buffer buf)))
             (ignore-errors (delete-file file-2b)))))))))

(auto-revert--deftest-remote auto-revert-test05-global-notify
  "Test `global-auto-revert-mode' without polling for remote buffers.")

(ert-deftest auto-revert-test06-write-file ()
  "Verify that notification follows `write-file' correctly."
  (skip-unless (or file-notify--library
                   (file-remote-p auto-revert--test-rootdir)))

  (with-auto-revert-test
   (ert-with-temp-file file-1
     :prefix ert-temp-file-prefix
     (let ((auto-revert-use-notify t)
           (file-2 (concat file-1 "-2"))
           require-final-newline buf)
       (unwind-protect
           (progn
             (setq buf (find-file-noselect file-1))
             (with-current-buffer buf
               (insert "A")
               (save-buffer)

               (auto-revert-mode 1)
               (auto-revert--skip-unless-proper-library-and-monitor)

               (insert "B")
               (write-file file-2)

               ;; File stamps of remote files have an accuracy of 1
               ;; second.  Wait a little bit.
               (when (file-remote-p file-1)
                 (sleep-for (auto-revert--timeout)))
               (auto-revert-test--write-region "C" file-2)
               (auto-revert-test--wait-for-buffer-text
                buf "C" (auto-revert--timeout))
               (should
                (string-equal (substring-no-properties (buffer-string)) "C"))))

         ;; Clean up.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))
         (ignore-errors (delete-file file-2)))))))

(auto-revert--deftest-remote auto-revert-test06-write-file
  "Test `write-file' in `auto-revert-mode' for remote buffers.")

;; This is inspired by Bug#44638, Bug#71424.
(ert-deftest auto-revert-test07-auto-revert-several-buffers ()
  "Check autorevert for several buffers visiting the same file."
  (skip-unless (or file-notify--library
                   (file-remote-p auto-revert--test-rootdir)))

  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     :prefix ert-temp-file-prefix
     (let ((auto-revert-use-notify t)
           (num-buffers 10)
           require-final-newline buffers)

       (unwind-protect
           ;; Check indirect buffers.
           (ert-with-message-capture auto-revert--messages
             (auto-revert-test--write-region "any text" tmpfile)
             (push (find-file-noselect tmpfile) buffers)
             (with-current-buffer (car buffers)
               (should
                (string-equal
                 (substring-no-properties (buffer-string)) "any text"))
               ;; `buffer-stale--default-function' checks for
               ;; `verify-visited-file-modtime'.  We must ensure that
               ;; it returns nil.
               (auto-revert-mode 1)
               (should auto-revert-mode))

             (dolist (clone '(clone nil))
               (dotimes (i num-buffers)
                 (push (make-indirect-buffer
                        (car (last buffers))
                        (format "%s-%d-%s"
                                (buffer-file-name (car (last buffers))) i clone)
                        clone)
                       buffers)))
             (setq buffers (nreverse buffers))
             (dolist (buf buffers)
               (with-current-buffer buf
                 (should
                  (string-equal
                   (substring-no-properties (buffer-string)) "any text"))
                 (if (string-suffix-p "-nil" (buffer-name buf))
                     (should-not auto-revert-mode)
                   (should auto-revert-mode))))

             (auto-revert-test--write-file "another text" tmpfile)
             ;; Check, that the buffer has been reverted.
             (auto-revert--wait-for-revert (car buffers))
             (dolist (buf buffers)
               (with-current-buffer buf
                 (should
                  (string-equal
                   (substring-no-properties (buffer-string)) "another text"))))

             ;; Disabling autorevert in an indirect buffer does not
             ;; disable autorevert in the corresponding base buffer.
             (dolist (buf (cdr buffers))
               (with-current-buffer buf
                 (auto-revert-mode 0)
                 (should-not auto-revert-mode))
               (with-current-buffer (car buffers)
                 (should auto-revert-mode)))

             ;; Killing an indirect buffer does not disable autorevert in
             ;; the corresponding base buffer.
             (dolist (buf (cdr buffers))
               (kill-buffer buf))
             (with-current-buffer (car buffers)
               (should auto-revert-mode)))

         ;; Exit.
         (ignore-errors
           (dolist (buf buffers)
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf)))
         (setq buffers nil)
         (ignore-errors (delete-file tmpfile)))

       ;; Check direct buffers.
       (unwind-protect
           (ert-with-message-capture auto-revert--messages
             (auto-revert-test--write-region "any text" tmpfile)

             (dotimes (i num-buffers)
               (push (generate-new-buffer
                      (format "%s-%d" (file-name-nondirectory tmpfile) i))
                     buffers))
             (setq buffers (nreverse buffers))
             (dolist (buf buffers)
               (with-current-buffer buf
                 (insert-file-contents tmpfile 'visit)
                 (should
                  (string-equal
                   (substring-no-properties (buffer-string)) "any text"))
                 (auto-revert-mode 1)
                 (should auto-revert-mode)))

             (auto-revert-test--write-file "another text" tmpfile)
             ;; Check, that the buffers have been reverted.
             (dolist (buf buffers)
               (auto-revert--wait-for-revert buf)
               (with-current-buffer buf
                 (should
                  (string-equal
                   (substring-no-properties (buffer-string)) "another text")))))

         ;; Exit.
         (ignore-errors
           (dolist (buf buffers)
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))))))

(auto-revert--deftest-remote auto-revert-test07-auto-revert-several-buffers
  "Check autorevert for several buffers visiting the same remote file.")

(ert-deftest auto-revert-test08-auto-revert-inhibit-auto-revert ()
  "Check the power of `inhibit-auto-revert'."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     :prefix ert-temp-file-prefix
     (let (buf)
       (unwind-protect
           (progn
             (auto-revert-test--write-region "any text" tmpfile)
             (setq buf (find-file-noselect tmpfile))
             (with-current-buffer buf
               (ert-with-message-capture auto-revert--messages
                 (inhibit-auto-revert
                   (auto-revert-mode 1)
                   (should auto-revert-mode)

                   (auto-revert-test--write-file "another text" tmpfile)
                   ;; Check, that the buffer hasn't been reverted.
                   (auto-revert--wait-for-revert buf)
                   (should-not
                    (string-match-p
                     "another text" (substring-no-properties (buffer-string)))))

                 ;; Check, that the buffer has been reverted.
                 (auto-revert--wait-for-revert buf)
                 (should
                  (string-match-p
                   "another text" (substring-no-properties (buffer-string)))))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test08-auto-revert-inhibit-auto-revert
  "Check the power of `inhibit-auto-revert' on a remote file.")

;; Mark all tests as unstable on Cygwin (bug#49665).
(when (eq system-type 'cygwin)
  (dolist (test (apropos-internal "^auto-revert" #'ert-test-boundp))
    (setf (ert-test-tags (ert-get-test test))
	  (cons :unstable (ert-test-tags (ert-get-test test))))))

(defun auto-revert-test-all (&optional interactive)
  "Run all tests for \\[auto-revert]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^auto-revert-")
    (ert-run-tests-batch "^auto-revert-")))

(provide 'autorevert-tests)
;;; autorevert-tests.el ends here
