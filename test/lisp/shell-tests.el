;;; shell-tests.el --- Tests for shell.el  -*- lexical-binding:t -*-

;; Copyright (C) 2010-2026 Free Software Foundation, Inc.

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

;; Tests for comint and related modes.

;;; Code:

(require 'shell)
(require 'ert)

(ert-deftest shell-tests-unquote-1 ()
  "Test problem found by Filipp Gunbin in emacs-devel."
  (should (equal (car (shell--unquote&requote-argument "te'st" 2)) "test")))

(ert-deftest shell-tests-completion-before-semi ()
  (with-temp-buffer
    (shell-mode)
    (insert "cd ba;")
    (forward-char -1)
    (should (equal (shell--parse-pcomplete-arguments)
                   '(("cd" "ba") 1 4)))))

(ert-deftest shell-tests-completion-after-semi ()
  (with-temp-buffer
    (shell-mode)
    (insert "cd ba;")
    (should (equal (shell--parse-pcomplete-arguments)
                   '(("cd" "ba" "") 1 4 7)))))

(ert-deftest shell-tests-split-string ()
  (should (equal (split-string-shell-command "ls /tmp")
                 '("ls" "/tmp")))
  (should (equal (split-string-shell-command "ls '/tmp/foo bar'")
                 '("ls" "/tmp/foo bar")))
  (should (equal (split-string-shell-command "ls \"/tmp/foo bar\"")
                 '("ls" "/tmp/foo bar")))
  (should (equal (split-string-shell-command "ls /tmp/'foo bar'")
                 '("ls" "/tmp/foo bar")))
  (should (equal (split-string-shell-command "ls /tmp/'foo\"bar'")
                 '("ls" "/tmp/foo\"bar")))
  (should (equal (split-string-shell-command "ls /tmp/\"foo''bar\"")
                 '("ls" "/tmp/foo''bar")))
  (should (equal (split-string-shell-command "ls /tmp/'foo\\ bar'")
                 '("ls" "/tmp/foo\\ bar")))
  (unless (memq system-type '(windows-nt ms-dos))
    (should (equal (split-string-shell-command "ls /tmp/foo\\ bar")
                   '("ls" "/tmp/foo bar")))))

(ert-deftest shell-dirtrack-on-by-default ()
  (with-temp-buffer
    (shell-mode)
    (should shell-dirtrack-mode)))

(ert-deftest shell-dirtrack-should-not-be-on-in-unrelated-modes ()
  (with-temp-buffer
    (should (not shell-dirtrack-mode))))

(ert-deftest shell-dirtrack-sets-list-buffers-directory ()
  (let ((start-dir default-directory))
    (with-temp-buffer
      (should-not list-buffers-directory)
      (shell-mode)
      (shell-cd "..")
      (should list-buffers-directory)
      (should (not (equal start-dir list-buffers-directory)))
      (should (string-prefix-p list-buffers-directory start-dir)))))

(ert-deftest shell-directory-tracker-cd ()
  (let ((start-dir default-directory))
    (with-temp-buffer
      (should-not list-buffers-directory)
      (shell-mode)
      (cl-letf (((symbol-function 'shell-unquote-argument)
                 (lambda (x) x)))
        (shell-directory-tracker "cd .."))
      (should list-buffers-directory)
      (should (not (equal start-dir list-buffers-directory)))
      (should (string-prefix-p list-buffers-directory start-dir)))))

;;; shell-tests.el ends here
