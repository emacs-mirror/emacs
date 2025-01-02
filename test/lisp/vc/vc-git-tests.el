;;; vc-git-tests.el --- tests for vc/vc-git.el  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2025 Free Software Foundation, Inc.

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

(ert-deftest vc-git-test-dir-track-local-branch ()
  "Test that `vc-dir' works when tracking local branches.  Bug#68183."
  (skip-unless (executable-find vc-git-program))
  (vc-git-test--with-repo repo
    ;; Create an initial commit to get a branch started.
    (write-region "hello" nil "README")
    (vc-git-test--run "add" "README")
    (vc-git-test--run "commit" "-mFirst")
    ;; Get current branch name lazily, to remain agnostic of
    ;; init.defaultbranch.
    (let ((upstream-branch
           (string-trim (vc-git-test--run "branch" "--show-current"))))
      (vc-git-test--run "checkout" "--track" "-b" "hack" upstream-branch)
      (vc-dir default-directory)
      (pcase-dolist (`(,header ,value)
                     `(("Branch" "hack")
                       ("Tracking" ,upstream-branch)))
        (goto-char (point-min))
        (re-search-forward (format "^%s *: %s$" header value))))))

;;; vc-git-tests.el ends here
