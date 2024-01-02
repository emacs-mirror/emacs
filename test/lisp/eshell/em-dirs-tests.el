;;; em-dirs-tests.el --- em-dirs test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;; Tests for Eshell's dirs module.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'em-dirs)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))
;;; Tests:

(ert-deftest em-dirs-test/pwd-var ()
  "Test using the $PWD variable."
  (let ((default-directory "/some/path"))
    (eshell-command-result-equal "echo $PWD"
                                 (expand-file-name default-directory))))

(ert-deftest em-dirs-test/pwd-var-indices ()
  "Test using the $PWD variable with indices."
  (let ((default-directory "/some/path/here"))
    (eshell-command-result-equal "echo $PWD[/ 1]"
                                 "some")
    (eshell-command-result-equal "echo $PWD[/ 1 3]"
                                 '("some" "here"))))

(ert-deftest em-dirs-test/short-pwd-var ()
  "Test using the $+ (current directory) variable."
  (let ((default-directory "/some/path"))
    (eshell-command-result-equal "echo $+"
                                 (expand-file-name default-directory))))

(ert-deftest em-dirs-test/oldpwd-var ()
  "Test using the $OLDPWD variable."
  (let (eshell-last-dir-ring-file-name)
    (with-temp-eshell
     (eshell-match-command-output "echo $OLDPWD"
                                  "\\`\\'")
     (ring-insert eshell-last-dir-ring "/some/path")
     (eshell-match-command-output "echo $OLDPWD"
                                  "/some/path\n"))))

(ert-deftest em-dirs-test/oldpwd-var-indices ()
  "Test using the $OLDPWD variable with indices."
  (let (eshell-last-dir-ring-file-name)
    (with-temp-eshell
     (ring-insert eshell-last-dir-ring "/some/path/here")
     (eshell-match-command-output "echo $OLDPWD[/ 1]"
                                  "some\n")
     (eshell-match-command-output "echo $OLDPWD[/ 1 3]"
                                  "(\"some\" \"here\")\n"))))

(ert-deftest em-dirs-test/directory-ring-var ()
  "Test using the $- (directory ring) variable."
  (let (eshell-last-dir-ring-file-name)
    (with-temp-eshell
     (eshell-match-command-output "echo $-"
                                  "\\`\\'")
     (ring-insert eshell-last-dir-ring "/some/path")
     (ring-insert eshell-last-dir-ring "/other/path")
     (eshell-match-command-output "echo $-"
                                  "/other/path\n")
     (eshell-match-command-output "echo $-[0]"
                                  "/other/path\n")
     (eshell-match-command-output "echo $-[1]"
                                  "/some/path\n"))))

(ert-deftest em-dirs-test/directory-ring-var-indices ()
  "Test using the $- (directory ring) variable with multiple indices."
  (let (eshell-last-dir-ring-file-name)
    (with-temp-eshell
     (ring-insert eshell-last-dir-ring "/some/path/here")
     (eshell-match-command-output "echo $-[0][/ 1]"
                                  "some\n")
     (eshell-match-command-output "echo $-[1][/ 1 3]"
                                  "(\"some\" \"here\")\n"))))

;; em-dirs-tests.el ends here
