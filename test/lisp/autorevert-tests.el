;;; auto-revert-tests.el --- Tests of auto-revert

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

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

;; A whole test run can be performed calling the command `auto-revert-test-all'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'autorevert)
(setq auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-stop-on-user-input nil)

(defconst auto-revert--timeout 10
  "Time to wait for a message.")

(defvar auto-revert--messages nil
  "Used to collect messages issued during a section of a test.")

(defun auto-revert--wait-for-revert (buffer)
  "Wait until a message reports reversion of BUFFER.
This expects `auto-revert--messages' to be bound by
`ert-with-message-capture' before calling."
  (with-timeout (auto-revert--timeout nil)
    (while
        (null (string-match
               (format-message "Reverting buffer `%s'." (buffer-name buffer))
               auto-revert--messages))
      (if (with-current-buffer buffer auto-revert-use-notify)
          (read-event nil nil 0.1)
        (sleep-for 0.1)))))

(ert-deftest auto-revert-test00-auto-revert-mode ()
  "Check autorevert for a file."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
	(progn
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
          (with-current-buffer buf
            (ert-with-message-capture auto-revert--messages
              (should (string-equal (buffer-string) "any text"))
              ;; `buffer-stale--default-function' checks for
              ;; `verify-visited-file-modtime'.  We must ensure that it
              ;; returns nil.
              (sleep-for 1)
              (auto-revert-mode 1)
              (should auto-revert-mode)

              ;; Modify file.  We wait for a second, in order to have
              ;; another timestamp.
              (sleep-for 1)
              (write-region "another text" nil tmpfile nil 'no-message)

              ;; Check, that the buffer has been reverted.
              (auto-revert--wait-for-revert buf))
            (should (string-match "another text" (buffer-string)))

            ;; When the buffer is modified, it shall not be reverted.
            (ert-with-message-capture auto-revert--messages
              (set-buffer-modified-p t)
              (sleep-for 1)
              (write-region "any text" nil tmpfile nil 'no-message)

              ;; Check, that the buffer hasn't been reverted.
              (auto-revert--wait-for-revert buf))
            (should-not (string-match "any text" (buffer-string)))))

      ;; Exit.
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

;; This is inspired by Bug#21841.
(ert-deftest auto-revert-test01-auto-revert-several-files ()
  "Check autorevert for several files at once."
  :tags '(:expensive-test)
  (skip-unless (executable-find "cp"))

  (let* ((cp (executable-find "cp"))
         (tmpdir1 (make-temp-file "auto-revert-test" 'dir))
         (tmpdir2 (make-temp-file "auto-revert-test" 'dir))
         (tmpfile1
          (make-temp-file (expand-file-name "auto-revert-test" tmpdir1)))
         (tmpfile2
          (make-temp-file (expand-file-name "auto-revert-test" tmpdir1)))
         buf1 buf2)
    (unwind-protect
        (ert-with-message-capture auto-revert--messages
          (write-region "any text" nil tmpfile1 nil 'no-message)
          (setq buf1 (find-file-noselect tmpfile1))
          (write-region "any text" nil tmpfile2 nil 'no-message)
          (setq buf2 (find-file-noselect tmpfile2))

          (dolist (buf (list buf1 buf2))
            (with-current-buffer buf
              (should (string-equal (buffer-string) "any text"))
              ;; `buffer-stale--default-function' checks for
              ;; `verify-visited-file-modtime'.  We must ensure that
              ;; it returns nil.
              (sleep-for 1)
              (auto-revert-mode 1)
              (should auto-revert-mode)))

          ;; Modify files.  We wait for a second, in order to have
          ;; another timestamp.
          (sleep-for 1)
          (write-region
           "another text" nil
           (expand-file-name (file-name-nondirectory tmpfile1) tmpdir2)
           nil 'no-message)
          (write-region
           "another text" nil
           (expand-file-name (file-name-nondirectory tmpfile2) tmpdir2)
           nil 'no-message)
          ;;(copy-directory tmpdir2 tmpdir1 nil 'copy-contents)
          ;; Strange, that `copy-directory' does not work as expected.
          ;; The following shell command is not portable on all
          ;; platforms, unfortunately.
          (shell-command (format "%s -f %s/* %s" cp tmpdir2 tmpdir1))

          ;; Check, that the buffers have been reverted.
          (dolist (buf (list buf1 buf2))
            (with-current-buffer buf
              (auto-revert--wait-for-revert buf)
              (should (string-match "another text" (buffer-string))))))

      ;; Exit.
      (ignore-errors
        (dolist (buf (list buf1 buf2))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))
      (ignore-errors (delete-directory tmpdir1 'recursive))
      (ignore-errors (delete-directory tmpdir2 'recursive)))))

;; This is inspired by Bug#23276.
(ert-deftest auto-revert-test02-auto-revert-deleted-file ()
  "Check autorevert for a deleted file."
  :tags '(:expensive-test)

  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
	(progn
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
	  (with-current-buffer buf
            (should (string-equal (buffer-string) "any text"))
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that
            ;; it returns nil.
            (sleep-for 1)
            (auto-revert-mode 1)
            (should auto-revert-mode)

            ;; Remove file while reverting.  We simulate this by
            ;; modifying `before-revert-hook'.
            (add-hook
             'before-revert-hook
             (lambda () (delete-file buffer-file-name))
             nil t)

            (ert-with-message-capture auto-revert--messages
              (sleep-for 1)
              (write-region "another text" nil tmpfile nil 'no-message)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer hasn't been reverted.  File
            ;; notification should be disabled, falling back to
            ;; polling.
            (should (string-match "any text" (buffer-string)))
            ;; With w32notify, the 'stopped' events are not sent.
            (or (eq file-notify--library 'w32notify)
                (should-not auto-revert-use-notify))

            ;; Once the file has been recreated, the buffer shall be
            ;; reverted.
            (kill-local-variable 'before-revert-hook)
            (ert-with-message-capture auto-revert--messages
              (sleep-for 1)
              (write-region "another text" nil tmpfile nil 'no-message)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should (string-match "another text" (buffer-string)))

            ;; An empty file shall still be reverted.
            (ert-with-message-capture auto-revert--messages
              (sleep-for 1)
              (write-region "" nil tmpfile nil 'no-message)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should (string-equal "" (buffer-string)))))

      ;; Exit.
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(ert-deftest auto-revert-test03-auto-revert-tail-mode ()
  "Check autorevert tail mode."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
        (ert-with-message-capture auto-revert--messages
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
	  (with-current-buffer buf
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
	    (auto-revert-tail-mode 1)
	    (should auto-revert-tail-mode)
            (erase-buffer)
            (insert "modified text\n")
            (set-buffer-modified-p nil)

	    ;; Modify file.  We wait for a second, in order to have
	    ;; another timestamp.
	    (sleep-for 1)
            (write-region "another text" nil tmpfile 'append 'no-message)

	    ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert buf)
            (should
             (string-match "modified text\nanother text" (buffer-string)))))

      ;; Exit.
      (ignore-errors (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(ert-deftest auto-revert-test04-auto-revert-mode-dired ()
  "Check autorevert for dired."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let* ((tmpfile (make-temp-file "auto-revert-test"))
         (name (file-name-nondirectory tmpfile))
         buf)
    (unwind-protect
	(progn
	  (setq buf (dired-noselect temporary-file-directory))
	  (with-current-buffer buf
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
            (auto-revert-mode 1)
            (should auto-revert-mode)
	    (should
             (string-match name (substring-no-properties (buffer-string))))

            (ert-with-message-capture auto-revert--messages
              ;; Delete file.  We wait for a second, in order to have
              ;; another timestamp.
              (sleep-for 1)
              (delete-file tmpfile)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should-not
             (string-match name (substring-no-properties (buffer-string))))

            (ert-with-message-capture auto-revert--messages
              ;; Make dired buffer modified.  Check, that the buffer has
              ;; been still reverted.
              (set-buffer-modified-p t)
              (sleep-for 1)
              (write-region "any text" nil tmpfile nil 'no-message)

              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should
             (string-match name (substring-no-properties (buffer-string))))))

      ;; Exit.
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(defun auto-revert-test-all (&optional interactive)
  "Run all tests for \\[auto-revert]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^auto-revert-")
    (ert-run-tests-batch "^auto-revert-")))

(provide 'auto-revert-tests)
;;; auto-revert-tests.el ends here
