;;; em-unix-tests.el --- em-unix test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

;; Tests for Eshell's implementation of various UNIX commands.

;;; Code:

(require 'ert)
(require 'em-unix)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

;;; Tests:

(ert-deftest em-unix-test/compile/interactive ()
  "Check that `eshell/compile' opens a compilation buffer interactively."
  (skip-unless (executable-find "echo"))
  (with-temp-eshell
   (eshell-match-command-output "compile echo hello"
                                "#<buffer \\*compilation\\*>")
   (with-current-buffer "*compilation*"
     (forward-line 3)
     (should (looking-at "echo hello")))))

(ert-deftest em-unix-test/compile/noninteractive ()
  "Check that `eshell/compile' writes to stdout noninteractively."
  (skip-unless (executable-find "echo"))
  (eshell-command-result-equal "compile echo hello"
                               "hello\n"))

(ert-deftest em-unix-test/compile/pipeline ()
  "Check that `eshell/compile' writes to stdout from a pipeline."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (with-temp-eshell
   (eshell-match-command-output "compile echo hello | *cat"
                                "\\`hello\n")))

(ert-deftest em-unix-test/compile/subcommand ()
  "Check that `eshell/compile' writes to stdout from a subcommand."
  (skip-unless (and (executable-find "echo")
                    (executable-find "cat")))
  (with-temp-eshell
   (eshell-match-command-output "echo ${compile echo hello}"
                                "\\`hello\n")))

;; em-unix-tests.el ends here
