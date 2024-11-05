;;; vc-git-tests.el --- tests for vc/vc-git.el  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Justin Schell <justinmschell@gmail.com>
;; Maintainer: emacs-devel@gnu.org

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

;;; Code:

(require 'ert-x)
(require 'vc)
(require 'vc-dir)
(require 'vc-git)

(ert-deftest vc-git-test-program-version-general ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.0"
   "2.30.1.0"))

(ert-deftest vc-git-test-program-version-windows ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.1.windows.1"
   "2.30.1.1"))

(ert-deftest vc-git-test-program-version-apple ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.2 (Apple Git-130)"
   "2.30.1.2"))

(ert-deftest vc-git-test-program-version-other ()
  (vc-git-test--run-program-version-test
   "git version 2.30.1.3.foo.bar"
   "2.30.1.3"))

(ert-deftest vc-git-test-program-version-invalid-leading-string ()
  (vc-git-test--run-program-version-test
   "git version foo.bar.2.30.1.4"
   "0"))

(ert-deftest vc-git-test-program-version-invalid-leading-dot ()
  (vc-git-test--run-program-version-test
   "git version .2.30.1.5"
   "0"))

(defun vc-git-test--run-program-version-test
    (mock-version-string expected-output)
  (cl-letf* (((symbol-function 'vc-git--run-command-string)
              (lambda (_file _args) mock-version-string))
             (vc-git--program-version nil)
             (actual-output (vc-git--program-version)))
    (should (equal actual-output expected-output))))

(ert-deftest vc-git-test-annotate-time ()
  "Test `vc-git-annotate-time'."
  (require 'vc-annotate)
  (with-temp-buffer
    (insert "\
00000000 (Foo Bar 2023-06-14  1) a
00000001 (Foo Bar 2023-06-14 00:00:00 -0130  2) b
00000002 (Foo Bar 2023-06-14 00:00:00 +0145  3) c
00000003 (Foo Bar 2023-06-14 00:00:00  4) d
00000004 (Foo Bar 0-0-0  5) \n")
    (goto-char (point-min))
    (should (floatp (vc-git-annotate-time)))
    (should (> (vc-git-annotate-time)
               (vc-git-annotate-time)))
    (should-not (vc-git-annotate-time))
    (should-not (vc-git-annotate-time))))

(defmacro vc-git-test--with-repo (name &rest body)
  "Initialize a repository in a temporary directory and evaluate BODY.

The current directory will be set to the top of that repository; NAME
will be bound to that directory's file name.  Once BODY exits, the
directory will be deleted.

Some dummy environment variables will be set for the duration of BODY to
allow `git commit' to determine identities for authors and committers."
  (declare (indent 1))
  `(ert-with-temp-directory ,name
     (let ((default-directory ,name)
           (process-environment (append '("EMAIL=john@doe.ee"
                                          "GIT_AUTHOR_NAME=A"
                                          "GIT_COMMITTER_NAME=C")
                                        process-environment)))
       (vc-create-repo 'Git)
       ,@body)))

(defun vc-git-test--run (&rest args)
  "Run git ARGS…, check for non-zero status, and return output."
  (with-temp-buffer
    (apply 'vc-git-command t 0 nil args)
    (buffer-string)))

(defun vc-git-test--start-branch ()
  "Get a branch started in a freshly initialized repository.

This returns the name of the current branch, so that tests can remain
agnostic of init.defaultbranch."
  (write-region "hello" nil "README")
  (vc-git-test--run "add" "README")
  (vc-git-test--run "commit" "-mFirst")
  (string-trim (vc-git-test--run "branch" "--show-current")))

(defun vc-git-test--dir-headers (headers)
  "Return an alist of header values for the current `vc-dir' buffer.

HEADERS should be a list of (NAME ...) strings.  This function will
return a list of (NAME . VALUE) pairs, where VALUE is nil if the header
is absent."
  ;; FIXME: to reproduce interactive sessions faithfully, we would need
  ;; to wait for the dir-status-files process to terminate; have not
  ;; found a reliable way to do this.  As a workaround, kill pending
  ;; processes and revert the `vc-dir' buffer.
  (vc-dir-kill-dir-status-process)
  (revert-buffer)
  (mapcar
   (lambda (header)
     (let* ((pattern
             (rx bol
                 (literal header) (* space) ": " (group (+ nonl))
                 eol))
            (value (and (goto-char (point-min))
                        (re-search-forward pattern nil t)
                        (match-string 1))))
       (cons header value)))
   headers))

(ert-deftest vc-git-test-dir-branch-headers ()
  "Check that `vc-dir' shows expected branch-related headers."
  (skip-unless (executable-find vc-git-program))
  ;; Create a repository that will serve as the "remote".
  (vc-git-test--with-repo origin-repo
    (let ((main-branch (vc-git-test--start-branch)))
      ;; 'git clone' this repository and test things in this clone.
      (ert-with-temp-directory clone-repo
        (vc-git-test--run "clone" origin-repo clone-repo)
        (vc-dir clone-repo)
        (should
         (equal
          (vc-git-test--dir-headers
           '("Branch" "Tracking" "Remote"))
          `(("Branch"   . ,main-branch)
            ("Tracking" . ,(concat "origin/" main-branch))
            ("Remote"   . ,origin-repo))))
        ;; Checkout a new branch: no tracking information.
        (vc-git-test--run "checkout" "-b" "feature/foo" main-branch)
        (should
         (equal
          (vc-git-test--dir-headers
           '("Branch" "Tracking" "Remote"))
          '(("Branch"   . "feature/foo")
            ("Tracking" . nil)
            ("Remote"   . nil))))
        ;; Push with '--set-upstream origin': tracking information
        ;; should be updated.
        (vc-git-test--run "push" "--set-upstream" "origin" "feature/foo")
        (should
         (equal
          (vc-git-test--dir-headers
           '("Branch" "Tracking" "Remote"))
          `(("Branch"   . "feature/foo")
            ("Tracking" . "origin/feature/foo")
            ("Remote"   . ,origin-repo))))
        ;; Checkout a new branch tracking the _local_ main branch.
        ;; Bug#68183.
        (vc-git-test--run "checkout" "-b" "feature/bar" "--track" main-branch)
        (should
         (equal
          (vc-git-test--dir-headers
           '("Branch" "Tracking" "Remote"))
          `(("Branch"   . "feature/bar")
            ("Tracking" . ,main-branch)
            ("Remote"   . "none (tracking local branch)"))))))))

;;; vc-git-tests.el ends here
