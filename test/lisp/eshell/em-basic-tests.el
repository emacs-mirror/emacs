;;; em-basic-tests.el --- em-basic test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

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

;; Tests for basic Eshell commands.

;;; Code:

(require 'ert)
(require 'em-basic)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

;;; Tests:

(ert-deftest em-basic-test/umask-print-numeric ()
  "Test printing umask numerically."
  (cl-letf (((symbol-function 'default-file-modes) (lambda () #o775)))
    (eshell-command-result-equal "umask" "002\n"))
  (cl-letf (((symbol-function 'default-file-modes) (lambda () #o654)))
    (eshell-command-result-equal "umask" "123\n"))
  ;; Make sure larger numbers don't cause problems.
  (cl-letf (((symbol-function 'default-file-modes) (lambda () #o1775)))
    (eshell-command-result-equal "umask" "002\n")))

(ert-deftest em-basic-test/umask-read-symbolic ()
  "Test printing umask symbolically."
  (cl-letf (((symbol-function 'default-file-modes) (lambda () #o775)))
    (eshell-command-result-equal "umask -S"
                                 "u=rwx,g=rwx,o=rx\n"))
  (cl-letf (((symbol-function 'default-file-modes) (lambda () #o654)))
    (eshell-command-result-equal "umask -S"
                                 "u=wx,g=rx,o=x\n"))
  ;; Make sure larger numbers don't cause problems.
  (cl-letf (((symbol-function 'default-file-modes) (lambda () #o1775)))
    (eshell-command-result-equal "umask -S"
                                 "u=rwx,g=rwx,o=rx\n")))

(ert-deftest em-basic-test/umask-set ()
  "Test setting umask."
  (let ((file-modes 0))
    (cl-letf (((symbol-function 'set-default-file-modes)
               (lambda (mode) (setq file-modes mode))))
      (eshell-test-command-result "umask 002")
      (should (= file-modes #o775))
      (eshell-test-command-result "umask 123")
      (should (= file-modes #o654))
      (eshell-test-command-result "umask $(identity #o222)")
      (should (= file-modes #o555)))))

;; em-basic-tests.el ends here
