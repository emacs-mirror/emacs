;;; filelock-tests.el --- test file locking -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file tests code in src/filelock.c and, to some extent, the
;; related code in src/fileio.c.
;;
;; See also (info "(emacs)Interlocking") and (info "(elisp)File Locks")

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'seq)

(defmacro filelock-tests--fixture (&rest body)
  "Call BODY under a test fixture.
Create a test directory and a buffer whose `buffer-file-name' and
`buffer-file-truename' are a file within it, then call BODY.
Finally, delete the buffer and the test directory."
  (declare (debug (body)))
  `(ert-with-temp-directory temp-dir
     (let ((name
            ;; Use file-truename for when 'temporary-file-directory'
            ;; is a symlink, to make sure 'buffer-file-name' is set
            ;; below to a real existing file.
            (file-truename (concat (file-name-as-directory temp-dir)
                                   "userfile")))
           (create-lockfiles t))
       (with-temp-buffer
         (setq buffer-file-name name
               buffer-file-truename name)
         (unwind-protect
             (save-current-buffer
               ,@body)
           ;; Set `buffer-file-truename' nil to prevent unlocking,
           ;; which might prompt the user and/or signal errors.
           (setq buffer-file-name nil
                 buffer-file-truename nil))))))

(defun filelock-tests--make-lock-name (file-name)
  "Return the lock file name for FILE-NAME.
Equivalent logic in Emacs proper is implemented in C and
unavailable to Lisp."
  (concat (file-name-directory (expand-file-name file-name))
          ".#"
          (file-name-nondirectory file-name)))

(defun filelock-tests--spoil-lock-file (file-name)
  "Spoil the lock file for FILE-NAME.
Cause Emacs to report errors for various file locking operations
on FILE-NAME going forward.  Create a file that is incompatible
with Emacs's file locking protocol, but uses the same name as
FILE-NAME's lock file.  A directory file is used, which is
portable in practice."
  (make-directory (filelock-tests--make-lock-name file-name)))

(defun filelock-tests--unspoil-lock-file (file-name)
  "Remove the lock file spoiler for FILE-NAME.
See `filelock-tests--spoil-lock-file'."
  (delete-directory (filelock-tests--make-lock-name file-name) t))

(defun filelock-tests--should-be-locked ()
  "Abort the current test if the current buffer is not locked.
Exception: on systems without lock file support, aborts the
current test if the current file is locked (which should never
the case)."
  (if (eq system-type 'ms-dos)
      (should-not (file-locked-p buffer-file-truename))
    (should (file-locked-p buffer-file-truename))))

(ert-deftest filelock-tests-lock-unlock-no-errors ()
  "Check that locking and unlocking works without error."
  (filelock-tests--fixture
   (should-not (file-locked-p (buffer-file-name)))

   ;; Inserting text should lock the buffer's file.
   (insert "this locks the buffer's file")
   (filelock-tests--should-be-locked)
   (unlock-buffer)
   (set-buffer-modified-p nil)
   (should-not (file-locked-p (buffer-file-name)))

   ;; `set-buffer-modified-p' should lock the buffer's file.
   (set-buffer-modified-p t)
   (filelock-tests--should-be-locked)
   (unlock-buffer)
   (should-not (file-locked-p (buffer-file-name)))

   (should-not (file-locked-p (buffer-file-name)))))

(ert-deftest filelock-tests-lock-spoiled ()
  "Check `lock-buffer'."
  (skip-when (eq system-type 'ms-dos)) ; no filelock support
  (filelock-tests--fixture
   (filelock-tests--spoil-lock-file buffer-file-truename)
   ;; FIXME: errors when locking a file are ignored; should they be?
   (set-buffer-modified-p t)
   (filelock-tests--unspoil-lock-file buffer-file-truename)
   (should-not (file-locked-p buffer-file-truename))))

(ert-deftest filelock-tests-file-locked-p-spoiled ()
  "Check that `file-locked-p' fails if the lockfile is \"spoiled\"."
  (skip-when (eq system-type 'ms-dos)) ; no filelock support
  (filelock-tests--fixture
   (filelock-tests--spoil-lock-file buffer-file-truename)
   (let ((err (should-error (file-locked-p (buffer-file-name)))))
     (should (equal (seq-subseq err 0 2) '(file-error "Testing file lock"))))))

(ert-deftest filelock-tests-unlock-spoiled ()
  "Check that `unlock-buffer' fails if the lockfile is \"spoiled\"."
  (skip-when (eq system-type 'ms-dos)) ; no filelock support
  (filelock-tests--fixture
   ;; Set the buffer modified with file locking temporarily disabled.
   (let ((create-lockfiles nil))
     (set-buffer-modified-p t))
   (should-not (file-locked-p buffer-file-truename))
   (filelock-tests--spoil-lock-file buffer-file-truename)

   ;; Errors from `unlock-buffer' should call
   ;; `userlock--handle-unlock-error' (bug#46397).
   (cl-letf (((symbol-function 'userlock--handle-unlock-error)
              (lambda (err) (signal (car err) (cdr err)))))
     (should (equal '(file-error "Unlocking file")
                    (seq-subseq (should-error (unlock-buffer)) 0 2))))))

(ert-deftest filelock-tests-kill-buffer-spoiled ()
  "Check that `kill-buffer' fails if a lockfile is \"spoiled\"."
  (skip-when (eq system-type 'ms-dos)) ; no filelock support
  (filelock-tests--fixture
   ;; Set the buffer modified with file locking temporarily disabled.
   (let ((create-lockfiles nil))
     (set-buffer-modified-p t))
   (should-not (file-locked-p buffer-file-truename))
   (filelock-tests--spoil-lock-file buffer-file-truename)

   ;; Kill the current buffer.  Because the buffer is modified Emacs
   ;; will attempt to unlock it.  Temporarily bind `yes-or-no-p' to a
   ;; function that fakes a "yes" answer for the "Buffer modified;
   ;; kill anyway?" prompt.
   ;;
   ;; File errors from unlocking files should call
   ;; `userlock--handle-unlock-error' (bug#46397).
   (cl-letf (((symbol-function 'yes-or-no-p) #'always)
             ((symbol-function 'userlock--handle-unlock-error)
              (lambda (err) (signal (car err) (cdr err)))))
     (should (equal '(file-error "Unlocking file")
                    (seq-subseq (should-error (kill-buffer)) 0 2))))))

(ert-deftest filelock-tests-detect-external-change ()
  "Check that an external file modification is reported."
  (skip-when (eq system-type 'ms-dos)) ; no filelock support
  (skip-unless (executable-find "touch"))
  (skip-unless (executable-find "echo"))
  (dolist (cl '(t nil))
    (filelock-tests--fixture
     (let ((create-lockfiles cl))
       (write-region "foo" nil (buffer-file-name))
       (revert-buffer nil 'noconfirm)
       (should-not (file-locked-p (buffer-file-name)))

       ;; Just changing the file modification on disk doesn't hurt,
       ;; because file contents in buffer and on disk look equal.
       (shell-command (format "touch %s"
                              (shell-quote-argument (buffer-file-name))))
       (insert "bar")
       (when cl (filelock-tests--should-be-locked))

       ;; Bug#53207: with `create-lockfiles' nil, saving the buffer
       ;; results in a prompt.
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (_) (ert-fail "Test failed unexpectedly"))))
         (save-buffer))
       (should-not (file-locked-p (buffer-file-name)))

       ;; Changing the file contents on disk hurts when buffer is
       ;; modified.  There shall be a query, which we answer.
       ;; *Messages* buffer is checked for prompt.
       (shell-command (format "echo bar >>%s"
                              (shell-quote-argument (buffer-file-name))))
       (cl-letf (((symbol-function 'read-char-choice)
                  (lambda (prompt &rest _) (message "%s" prompt) ?y)))
         (ert-with-message-capture captured-messages
           ;; `ask-user-about-supersession-threat' does not work in
           ;; batch mode, let's simulate interactiveness.
           (let (noninteractive)
             (insert "baz"))
           (should (string-match-p
		    (format
                     "^%s changed on disk; really edit the buffer\\?"
                     (file-name-nondirectory (buffer-file-name)))
                    captured-messages))))
       (when cl (filelock-tests--should-be-locked))))))

(provide 'filelock-tests)
;;; filelock-tests.el ends here
