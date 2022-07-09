;;; em-dirs-tests.el --- em-dirs test suite  -*- lexical-binding:t -*-

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
    (should (equal (eshell-test-command-result "echo $PWD")
                   (expand-file-name default-directory)))))

(ert-deftest em-dirs-test/short-pwd-var ()
  "Test using the $+ (current directory) variable."
  (let ((default-directory "/some/path"))
    (should (equal (eshell-test-command-result "echo $+")
                   (expand-file-name default-directory)))))

(ert-deftest em-dirs-test/oldpwd-var ()
  "Test using the $OLDPWD variable."
  (let (eshell-last-dir-ring-file-name)
    (with-temp-eshell
     (eshell-command-result-p "echo $OLDPWD"
                              "\\`\\'")
     (ring-insert eshell-last-dir-ring "/some/path")
     (eshell-command-result-p "echo $OLDPWD"
                              "/some/path\n"))))

(ert-deftest em-dirs-test/directory-ring-var ()
  "Test using the $- (directory ring) variable."
  (let (eshell-last-dir-ring-file-name)
    (with-temp-eshell
     (eshell-command-result-p "echo $-"
                              "\\`\\'")
     (ring-insert eshell-last-dir-ring "/some/path")
     (ring-insert eshell-last-dir-ring "/other/path")
     (eshell-command-result-p "echo $-"
                              "/other/path\n")
     (eshell-command-result-p "echo $-[0]"
                              "/other/path\n")
     (eshell-command-result-p "echo $-[1]"
                              "/some/path\n"))))

;; em-dirs-tests.el ends here
