;;; autorevert-tests.el --- Tests of auto-revert   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Free Software Foundation, Inc.

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
;; "inotifywait", "gio-monitor" and "gvfs-monitor-dir".

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

(setq auto-revert-debug nil
      auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-stop-on-user-input nil
      file-notify-debug nil
      tramp-verbose 0)

(defun auto-revert--timeout ()
  "Time to wait for a message."
  (+ auto-revert-interval 0.1))

(defvar auto-revert--messages nil
  "Used to collect messages issued during a section of a test.")

;; Filter suppressed remote file-notify libraries.
(when (stringp (getenv "REMOTE_FILE_NOTIFY_LIBRARY"))
  (dolist (lib '("inotifywait" "gio-monitor" "gvfs-monitor-dir"))
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
	   (not (getenv "EMACS_HYDRA_CI"))
	   (file-remote-p ert-remote-temporary-file-directory)
	   (file-directory-p ert-remote-temporary-file-directory)
	   (file-writable-p ert-remote-temporary-file-directory))))))
  ;; Return result.
  (cdr auto-revert--test-enabled-remote-checked))

(defun auto-revert--wait-for-revert (buffer)
  "Wait until a message reports reversion of BUFFER.
This expects `auto-revert--messages' to be bound by
`ert-with-message-capture' before calling."
  ;; Remote files do not cooperate well with timers.  So we count ourselves.
  (let ((ct (current-time)))
    (while (and (< (float-time (time-subtract nil ct))
                   (auto-revert--timeout))
                (null (string-match
                       (format-message
                        "Reverting buffer `%s'" (buffer-name buffer))
                       (or auto-revert--messages ""))))
      (if (and (or file-notify--library
                   (file-remote-p temporary-file-directory))
               (with-current-buffer buffer auto-revert-use-notify))
          (read-event nil nil 0.05)
        (sleep-for 0.05)))))

(defmacro auto-revert--deftest-remote (test docstring)
  "Define ert `TEST-remote' for remote files."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
     ,docstring
     :tags '(:expensive-test :unstable)
     (let ((temporary-file-directory
	    ert-remote-temporary-file-directory)
           (auto-revert-remote-files t)
	   (ert-test (ert-get-test ',test))
           vc-handled-backends)
       (skip-unless (auto-revert--test-enabled-remote))
       (tramp-cleanup-connection
	(tramp-dissect-file-name temporary-file-directory) nil 'keep-password)
       (condition-case err
           (funcall (ert-test-body ert-test))
         (error (message "%s" err) (signal (car err) (cdr err)))))))

(defmacro with-auto-revert-test (&rest body)
  `(let ((auto-revert-interval-orig auto-revert-interval))
     (unwind-protect
         (progn
           (customize-set-variable 'auto-revert-interval 0.1)
           ,@body)
       (customize-set-variable 'auto-revert-interval auto-revert-interval-orig))))

(defun auto-revert-tests--write-file (text file time-delta &optional append)
  (write-region text nil file append 'no-message)
  (set-file-times file (time-subtract nil time-delta)))

(ert-deftest auto-revert-test00-auto-revert-mode ()
  "Check autorevert for a file."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     (let ((times '(60 30 15))
           buf)
       (unwind-protect
           (progn
             (auto-revert-tests--write-file "any text" tmpfile (pop times))
             (setq buf (find-file-noselect tmpfile))
             (with-current-buffer buf
               (ert-with-message-capture auto-revert--messages
                 (should (string-equal (buffer-string) "any text"))
                 ;; `buffer-stale--default-function' checks for
                 ;; `verify-visited-file-modtime'.  We must ensure that it
                 ;; returns nil.
                 (auto-revert-mode 1)
                 (should auto-revert-mode)

                 (auto-revert-tests--write-file "another text" tmpfile (pop times))

                 ;; Check, that the buffer has been reverted.
                 (auto-revert--wait-for-revert buf))
               (should (string-match "another text" (buffer-string)))

               ;; When the buffer is modified, it shall not be reverted.
               (ert-with-message-capture auto-revert--messages
                 (set-buffer-modified-p t)
                 (auto-revert-tests--write-file "any text" tmpfile (pop times))

                 ;; Check, that the buffer hasn't been reverted.
                 (auto-revert--wait-for-revert buf))
               (should-not (string-match "any text" (buffer-string)))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test00-auto-revert-mode
  "Check autorevert for a remote file.")

;; This is inspired by Bug#21841.
(ert-deftest auto-revert-test01-auto-revert-several-files ()
  "Check autorevert for several files at once."
  (skip-unless (executable-find "cp" (file-remote-p temporary-file-directory)))

  (ert-with-temp-directory tmpdir1
    (ert-with-temp-directory tmpdir2
      (ert-with-temp-file tmpfile1
        :prefix (expand-file-name "auto-revert-test" tmpdir1)
        (ert-with-temp-file tmpfile2
          :prefix (expand-file-name "auto-revert-test" tmpdir1)
          (with-auto-revert-test
           (let* ((cp (executable-find "cp" (file-remote-p temporary-file-directory)))
                  (times '(120 60 30 15))
                  buf1 buf2)
             (unwind-protect
                 (ert-with-message-capture auto-revert--messages
                   (auto-revert-tests--write-file "any text" tmpfile1 (pop times))
                   (setq buf1 (find-file-noselect tmpfile1))
                   (auto-revert-tests--write-file "any text" tmpfile2 (pop times))
                   (setq buf2 (find-file-noselect tmpfile2))

                   (dolist (buf (list buf1 buf2))
                     (with-current-buffer buf
                       (should (string-equal (buffer-string) "any text"))
                       ;; `buffer-stale--default-function' checks for
                       ;; `verify-visited-file-modtime'.  We must ensure that
                       ;; it returns nil.
                       (auto-revert-mode 1)
                       (should auto-revert-mode)))

                   ;; Modify files.  We wait for a second, in order to have
                   ;; another timestamp.
                   (auto-revert-tests--write-file
                    "another text"
                    (expand-file-name (file-name-nondirectory tmpfile1) tmpdir2)
                    (pop times))
                   (auto-revert-tests--write-file
                    "another text"
                    (expand-file-name (file-name-nondirectory tmpfile2) tmpdir2)
                    (pop times))
                   ;;(copy-directory tmpdir2 tmpdir1 nil 'copy-contents)
                   ;; Strange, that `copy-directory' does not work as expected.
                   ;; The following shell command is not portable on all
                   ;; platforms, unfortunately.
                   (shell-command
                    (format "%s -f %s/* %s"
                            cp (file-local-name tmpdir2) (file-local-name tmpdir1)))

                   ;; Check, that the buffers have been reverted.
                   (dolist (buf (list buf1 buf2))
                     (with-current-buffer buf
                       (auto-revert--wait-for-revert buf)
                       (should (string-match "another text" (buffer-string))))))

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
  ;; Repeated unpredictable failures, bug#32645.
  :tags '(:unstable)
  ;; Unlikely to be hydra-specific?
  ;; (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     (let (;; Try to catch bug#32645.
           (auto-revert-debug (getenv "EMACS_HYDRA_CI"))
           (file-notify-debug (getenv "EMACS_HYDRA_CI"))
           (times '(120 60 30 15))
           buf desc)
       (unwind-protect
           (progn
             (auto-revert-tests--write-file "any text" tmpfile (pop times))
             (setq buf (find-file-noselect tmpfile))
             (with-current-buffer buf
               (should-not
                (file-notify-valid-p auto-revert-notify-watch-descriptor))
               (should (string-equal (buffer-string) "any text"))
               ;; `buffer-stale--default-function' checks for
               ;; `verify-visited-file-modtime'.  We must ensure that
               ;; it returns nil.
               (auto-revert-mode 1)
               (should auto-revert-mode)
               (setq desc auto-revert-notify-watch-descriptor)

               ;; Remove file while reverting.  We simulate this by
               ;; modifying `before-revert-hook'.
               (add-hook
                'before-revert-hook
                (lambda ()
                  (when auto-revert-debug
                    (message "%s deleted" buffer-file-name))
                  (delete-file buffer-file-name))
                nil t)

               (ert-with-message-capture auto-revert--messages
                 (auto-revert-tests--write-file "another text" tmpfile (pop times))
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer hasn't been reverted.  File
               ;; notification should be disabled, falling back to
               ;; polling.
               (should (string-match "any text" (buffer-string)))
               ;; With w32notify, and on emba, the `stopped' events are not sent.
               (or (eq file-notify--library 'w32notify)
                   (getenv "EMACS_EMBA_CI")
                   (should-not
                    (file-notify-valid-p auto-revert-notify-watch-descriptor)))

               ;; Once the file has been recreated, the buffer shall be
               ;; reverted.
               (kill-local-variable 'before-revert-hook)
               (ert-with-message-capture auto-revert--messages
                 (auto-revert-tests--write-file "another text" tmpfile (pop times))
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer has been reverted.
               (should (string-match "another text" (buffer-string)))
               ;; When file notification is used, it must be reenabled
               ;; after recreation of the file.  We cannot expect that
               ;; the descriptor is the same, so we just check the
               ;; existence.
               (should (eq (null desc) (null auto-revert-notify-watch-descriptor)))

               ;; An empty file shall still be reverted.
               (ert-with-message-capture auto-revert--messages
                 (auto-revert-tests--write-file "" tmpfile (pop times))
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer has been reverted.
               (should (string-equal "" (buffer-string)))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test02-auto-revert-deleted-file
  "Check autorevert for a deleted remote file.")

(ert-deftest auto-revert-test03-auto-revert-tail-mode ()
  "Check autorevert tail mode."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (ert-with-temp-file tmpfile
    (let ((times '(30 15))
          buf)
      (unwind-protect
          (ert-with-message-capture auto-revert--messages
            (auto-revert-tests--write-file "any text" tmpfile (pop times))
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
              (auto-revert-tests--write-file "another text" tmpfile (pop times) 'append)

              ;; Check, that the buffer has been reverted.
              (auto-revert--wait-for-revert buf)
              (should
               (string-match "modified text\nanother text" (buffer-string)))))

        ;; Exit.
        (ignore-errors (kill-buffer buf))))))

(auto-revert--deftest-remote auto-revert-test03-auto-revert-tail-mode
  "Check remote autorevert tail mode.")

(ert-deftest auto-revert-test04-auto-revert-mode-dired ()
  "Check autorevert for dired."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (with-auto-revert-test
   (ert-with-temp-file tmpfile
     (let* ((name (file-name-nondirectory tmpfile))
            (times '(30))
            buf)
       (unwind-protect
           (progn
             (setq buf (dired-noselect temporary-file-directory))
             (with-current-buffer buf
               ;; `buffer-stale--default-function' checks for
               ;; `verify-visited-file-modtime'.  We must ensure that it
               ;; returns nil.
               (auto-revert-mode 1)
               (should auto-revert-mode)
               (should
                (string-match name (substring-no-properties (buffer-string))))

               (ert-with-message-capture auto-revert--messages
                 ;; Delete file.
                 (delete-file tmpfile)
                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer has been reverted.
               (should-not
                (string-match name (substring-no-properties (buffer-string))))

               (ert-with-message-capture auto-revert--messages
                 ;; Make dired buffer modified.  Check, that the buffer has
                 ;; been still reverted.
                 (set-buffer-modified-p t)
                 (auto-revert-tests--write-file "any text" tmpfile (pop times))

                 (auto-revert--wait-for-revert buf))
               ;; Check, that the buffer has been reverted.
               (should
                (string-match name (substring-no-properties (buffer-string))))))

         ;; Exit.
         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))))))

(auto-revert--deftest-remote auto-revert-test04-auto-revert-mode-dired
  "Check remote autorevert for dired.")

(defun auto-revert-test--write-file (string file)
  "Write STRING to FILE."
  (write-region string nil file nil 'no-message))

(defun auto-revert-test--buffer-string (buffer)
  "Contents of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-string)))

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
          "%s killed\n%s" (current-buffer) (with-output-to-string (backtrace))))
       nil 'local))))

(ert-deftest auto-revert-test05-global-notify ()
  "Test `global-auto-revert-mode' without polling."
  (skip-unless (or file-notify--library
                   (file-remote-p temporary-file-directory)))
  (with-auto-revert-test
   (ert-with-temp-file file-1
     (ert-with-temp-file file-2
       (ert-with-temp-file file-3
         (let* ((auto-revert-use-notify t)
                (auto-revert-avoid-polling t)
                (was-in-global-auto-revert-mode global-auto-revert-mode)
                (file-2b (concat file-2 "-b"))
                require-final-newline buf-1 buf-2 buf-3)
           (unwind-protect
               (progn
                 (setq buf-1 (find-file-noselect file-1))
                 (auto-revert-test--instrument-kill-buffer-hook buf-1)
                 (setq buf-2 (find-file-noselect file-2))
                 (auto-revert-test--instrument-kill-buffer-hook buf-2)
                 (auto-revert-test--write-file "1-a" file-1)
                 (should (equal (auto-revert-test--buffer-string buf-1) ""))

                 (global-auto-revert-mode 1) ; Turn it on.

                 (should (buffer-local-value
                          'auto-revert-notify-watch-descriptor buf-1))
                 (should (buffer-local-value
                          'auto-revert-notify-watch-descriptor buf-2))

                 ;; buf-1 should have been reverted immediately when the mode
                 ;; was enabled.
                 (should (equal (auto-revert-test--buffer-string buf-1) "1-a"))

                 ;; Alter a file.
                 (auto-revert-test--write-file "2-a" file-2)
                 ;; Allow for some time to handle notification events.
                 (auto-revert-test--wait-for-buffer-text buf-2 "2-a" 1)
                 (should (equal (auto-revert-test--buffer-string buf-2) "2-a"))

                 ;; Visit a file, and modify it on disk.
                 (setq buf-3 (find-file-noselect file-3))
                 (auto-revert-test--instrument-kill-buffer-hook buf-3)
                 ;; Newly opened buffers won't be use notification until the
                 ;; first poll cycle; wait for it.
                 (auto-revert-test--wait-for
                  (lambda () (buffer-local-value
                              'auto-revert-notify-watch-descriptor buf-3))
                  (auto-revert--timeout))
                 (should (buffer-local-value
                          'auto-revert-notify-watch-descriptor buf-3))
                 (auto-revert-test--write-file "3-a" file-3)
                 (auto-revert-test--wait-for-buffer-text buf-3 "3-a" 1)
                 (should (equal (auto-revert-test--buffer-string buf-3) "3-a"))

                 ;; Delete a visited file, and re-create it with new contents.
                 (delete-file file-1)
                 (should (equal (auto-revert-test--buffer-string buf-1) "1-a"))
                 (auto-revert-test--write-file "1-b" file-1)
                 ;; Since the file is deleted, it needs at least
                 ;; `auto-revert-interval' to recognize the new file,
                 ;; while polling.  So increase the timeout.
                 (auto-revert-test--wait-for-buffer-text
                  buf-1 "1-b" (* 2 (auto-revert--timeout)))
                 (should (buffer-local-value
                          'auto-revert-notify-watch-descriptor buf-1))

                 ;; Write a buffer to a new file, then modify the new file on disk.
                 (with-current-buffer buf-2
                   (write-file file-2b))
                 (should (equal (auto-revert-test--buffer-string buf-2) "2-a"))
                 (auto-revert-test--write-file "2-b" file-2b)
                 (auto-revert-test--wait-for-buffer-text
                  buf-2 "2-b" (auto-revert--timeout))
                 (should (buffer-local-value
                          'auto-revert-notify-watch-descriptor buf-2)))

             ;; Clean up.
             (unless was-in-global-auto-revert-mode
               (global-auto-revert-mode 0)) ; Turn it off.
             (dolist (buf (list buf-1 buf-2 buf-3))
               (with-current-buffer buf (setq-local kill-buffer-hook nil))
               (ignore-errors (kill-buffer buf)))
             (ignore-errors (delete-file file-2b)))))))))

(auto-revert--deftest-remote auto-revert-test05-global-notify
  "Test `global-auto-revert-mode' without polling for remote buffers.")

(ert-deftest auto-revert-test06-write-file ()
  "Verify that notification follows `write-file' correctly."
  (skip-unless (or file-notify--library
                   (file-remote-p temporary-file-directory)))
  (with-auto-revert-test
   (ert-with-temp-file file-1
     (let* ((auto-revert-use-notify t)
            (file-2 (concat file-1 "-2"))
            require-final-newline buf)
       (unwind-protect
           (progn
             (setq buf (find-file-noselect file-1))
             (with-current-buffer buf
               (insert "A")
               (save-buffer)

               (auto-revert-mode 1)

               (insert "B")
               (write-file file-2)

               (auto-revert-test--write-file "C" file-2)
               (auto-revert-test--wait-for-buffer-text
                buf "C" (auto-revert--timeout))
               (should (equal (buffer-string) "C"))))

         ;; Clean up.
         (ignore-errors (kill-buffer buf))
         (ignore-errors (delete-file file-2)))))))

(auto-revert--deftest-remote auto-revert-test06-write-file
  "Test `write-file' in `auto-revert-mode' for remote buffers.")

;; This is inspired by Bug#44638.
(ert-deftest auto-revert-test07-auto-revert-several-buffers ()
  "Check autorevert for several buffers visiting the same file."
  ;; (with-auto-revert-test
  (ert-with-temp-file tmpfile
    (let ((auto-revert-use-notify t)
          (times '(120 60 30 15))
          (num-buffers 10)
          require-final-newline buffers)

      (unwind-protect
          ;; Check indirect buffers.
          (ert-with-message-capture auto-revert--messages
            (auto-revert-tests--write-file "any text" tmpfile (pop times))
            (push (find-file-noselect tmpfile) buffers)
            (with-current-buffer (car buffers)
              (should (string-equal (buffer-string) "any text"))
              ;; `buffer-stale--default-function' checks for
              ;; `verify-visited-file-modtime'.  We must ensure that
              ;; it returns nil.
              (auto-revert-mode 1)
              (should auto-revert-mode))

            (dotimes (i num-buffers)
              (push (make-indirect-buffer
                     (car buffers)
                     (format "%s-%d" (buffer-file-name (car buffers)) i)
                     'clone)
                    buffers))
            (setq buffers (nreverse buffers))
            (dolist (buf buffers)
              (with-current-buffer buf
                (should (string-equal (buffer-string) "any text"))
                (should auto-revert-mode)))

            (auto-revert-tests--write-file "another text" tmpfile (pop times))
            ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert (car buffers))
            (dolist (buf buffers)
              (with-current-buffer buf
                (should (string-equal (buffer-string) "another text")))))

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
            (auto-revert-tests--write-file "any text" tmpfile (pop times))

            (dotimes (i num-buffers)
              (push (generate-new-buffer
                     (format "%s-%d" (file-name-nondirectory tmpfile) i))
                    buffers))
            (setq buffers (nreverse buffers))
            (dolist (buf buffers)
              (with-current-buffer buf
                (insert-file-contents tmpfile 'visit)
                (should (string-equal (buffer-string) "any text"))
                (auto-revert-mode 1)
                (should auto-revert-mode)))

            (auto-revert-tests--write-file "another text" tmpfile (pop times))
            ;; Check, that the buffers have been reverted.
            (dolist (buf buffers)
              (auto-revert--wait-for-revert buf)
              (with-current-buffer buf
                (should (string-equal (buffer-string) "another text")))))

        ;; Exit.
        (ignore-errors
          (dolist (buf buffers)
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))))));)

(auto-revert--deftest-remote auto-revert-test07-auto-revert-several-buffers
  "Check autorevert for several buffers visiting the same remote file.")

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

(provide 'auto-revert-tests)
;;; autorevert-tests.el ends here
