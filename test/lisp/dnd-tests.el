;;; dnd-tests.el --- Tests for window system independent DND support -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for stuff in dnd.el that doesn't require a window system.

;; At present, these tests only checks the behavior of the simplified
;; drag APIs in dnd.el.  Actual drags are not performed.

;;; Code:

(require 'dnd)
(require 'cl-lib)
(require 'tramp)

;; This code was taken from tramp-tests.el: perhaps some of it isn't
;; strictly necessary.
(defconst dnd-tests-temporary-file-directory
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
      ;; Emacs's Makefile sets $HOME to a nonexistent value.  Needed
      ;; in batch mode only, therefore.
      (unless (and (null noninteractive) (file-directory-p "~/"))
        (setenv "HOME" temporary-file-directory))
      (format "/mock::%s" temporary-file-directory)))
  "Temporary directory for drag-and-drop tests involving remote files.")

;; Substitute for x-begin-drag, which isn't present on all systems.
(defalias 'x-begin-drag
  (lambda (_targets &optional action frame &rest _)
    ;; Verify that frame is either nil or a valid frame.
    (when (and frame (not (frame-live-p frame)))
      (signal 'wrong-type-argument frame))
    ;; Verify that the action is valid and pretend the drag succeeded
    ;; (by returning the action).
    (cl-ecase action
      ('XdndActionCopy action)
      ('XdndActionMove action)
      ('XdndActionLink action)
      ;; These two are not technically valid, but x-begin-drag accepts
      ;; them anyway.
      ('XdndActionPrivate action)
      ('XdndActionAsk 'XdndActionPrivate))))

;; This doesn't work during tests.
(defalias 'gui-set-selection
  (lambda (&rest _)))

(defun dnd-tests-remote-accessible-p ()
  "Return if a test involving remote files can proceed."
  (ignore-errors
    (and
     (file-remote-p dnd-tests-temporary-file-directory)
     (file-directory-p dnd-tests-temporary-file-directory)
     (file-writable-p dnd-tests-temporary-file-directory))))

(defun dnd-tests-make-temp-name ()
  "Return a temporary remote file name for test.
The temporary file is not created."
  (expand-file-name (make-temp-name "dnd-test-remote")
                    dnd-tests-temporary-file-directory))

(ert-deftest dnd-tests-begin-text-drag ()
  (should (eq (dnd-begin-text-drag "some test text that will be dragged")
              'copy))
  (should (eq (dnd-begin-text-drag "some test text that will be dragged"
                                   nil 'move)
              'move)))

(ert-deftest dnd-tests-begin-file-drag ()
  ;; These tests also involve handling remote file names.
  (skip-unless (dnd-tests-remote-accessible-p))
  (let ((normal-temp-file (expand-file-name (make-temp-name "dnd-test")
                                            temporary-file-directory))
        (remote-temp-file (dnd-tests-make-temp-name)))
    ;; Touch those files if they don't exist.
    (unless (file-exists-p normal-temp-file)
      (write-region "" 0 normal-temp-file))
    (unless (file-exists-p remote-temp-file)
      (write-region "" 0 remote-temp-file))
    (unwind-protect
        (progn
          ;; Now test dragging a normal file.
          (should (eq (dnd-begin-file-drag normal-temp-file) 'copy))
          ;; And the remote file.
          (should (eq (dnd-begin-file-drag remote-temp-file) 'copy))
          ;; Test that the remote file was added to the list of files
          ;; to remove later.
          (should dnd-last-dragged-remote-file)
          ;; Test that the remote file was removed.
          (should (progn
                    (dnd-begin-file-drag normal-temp-file)
                    (not dnd-last-dragged-remote-file)))
          ;; Test that links to remote files can't be created.
          (should-error (dnd-begin-file-drag remote-temp-file nil 'link)))
      (delete-file normal-temp-file)
      (delete-file remote-temp-file))))

(ert-deftest dnd-tests-begin-drag-files ()
  (skip-unless (dnd-tests-remote-accessible-p))
  (let ((normal-temp-file (expand-file-name (make-temp-name "dnd-test")
                                            temporary-file-directory))
        (normal-temp-file-1 (expand-file-name (make-temp-name "dnd-test")
                                              temporary-file-directory))
        (remote-temp-file (dnd-tests-make-temp-name))
        (nonexistent-local-file
         (expand-file-name (make-temp-name "dnd-test")
                           temporary-file-directory))
        (nonexistent-remote-file (dnd-tests-make-temp-name))
        (nonexistent-remote-file-1 (dnd-tests-make-temp-name)))
    ;; Touch those files if they don't exist.
    (unless (file-exists-p normal-temp-file)
      (write-region "" 0 normal-temp-file))
    (unless (file-exists-p normal-temp-file-1)
      (write-region "" 0 normal-temp-file))
    (unless (file-exists-p remote-temp-file)
      (write-region "" 0 remote-temp-file))
    (ignore-errors
      (delete-file nonexistent-local-file)
      (delete-file nonexistent-remote-file)
      (delete-file nonexistent-remote-file-1))
    (unwind-protect
        (progn
          ;; Now test dragging a normal file and a remote file.
          (should (eq (dnd-begin-drag-files (list normal-temp-file
                                                  remote-temp-file))
                      'copy))
          ;; Test that the remote file produced was added to the list
          ;; of files to remove upon the next call.
          (should dnd-last-dragged-remote-file)
          ;; Two remote files at the same time.
          (should (eq (dnd-begin-drag-files (list normal-temp-file
                                                  normal-temp-file-1))
                      'copy))
          ;; Test that the remote files were removed.
          (should-not dnd-last-dragged-remote-file)
          ;; Multiple local files with some remote files that will
          ;; fail, and some that won't.
          (should (and (eq (dnd-begin-drag-files (list normal-temp-file
                                                       remote-temp-file
                                                       remote-temp-file
                                                       nonexistent-remote-file
                                                       normal-temp-file-1
                                                       nonexistent-remote-file-1))
                           'copy)
                       ;; Make sure exactly two valid remote files
                       ;; were downloaded.
                       (eq (length dnd-last-dragged-remote-file) 2)))
          ;; Make sure links can't be created to remote files.
          (should-error (dnd-begin-drag-files (list normal-temp-file
                                                    remote-temp-file
                                                    normal-temp-file-1)
                                              nil 'link))
          ;; And that they can to normal files.
          (should (eq (dnd-begin-drag-files (list normal-temp-file
                                                  normal-temp-file-1)
                                            nil 'link)
                      'link))
          ;; Make sure you can't drag an empty list of files.
          (should-error (dnd-begin-drag-files nil))
          ;; And when all remote files are inaccessible.
          (should-error (dnd-begin-drag-files (list nonexistent-remote-file
                                                    nonexistent-remote-file-1))))
      (delete-file normal-temp-file)
      (delete-file normal-temp-file-1)
      (delete-file remote-temp-file))))

(provide 'dnd-tests)
;;; dnd-tests.el ends here
