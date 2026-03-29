;;; esh-mode-tests.el --- esh-mode test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;; Tests for Eshell's command invocation.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'em-banner)
(require 'em-prompt)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

;;; Tests:

(ert-deftest esh-mode-test/handle-control-codes/carriage-return ()
  "Test that Eshell handles carriage returns properly."
  (with-temp-eshell
    (eshell-match-command-output "(format \"hello\r\ngoodbye\")"
                                 "\\`hello\ngoodbye\n")
    (eshell-match-command-output "(format \"hello\rgoodbye\")"
                                 "\\`goodbye\n")
    (eshell-match-command-output "(format \"hello\r\")"
                                 "\\`hello")))

(ert-deftest esh-mode-test/handle-control-codes/bell ()
  "Test that Eshell handles bells properly."
  (cl-letf* ((beep-called nil)
             ((symbol-function 'beep) (lambda () (setq beep-called t))))
    (with-temp-eshell
      (eshell-match-command-output "(format \"hello\athere\")"
                                   "\\`hellothere\n")
      (should beep-called))))

(ert-deftest esh-mode-test/handle-control-codes/backspace ()
  "Test that Eshell handles backspaces properly."
  (with-temp-eshell
    (eshell-match-command-output (format "(format \"hello%c%cp\")" ?\C-h ?\C-h)
                                 "\\`help\n")))

(ert-deftest esh-mode-test/clear/eshell-command ()
  "Test that `eshell/clear' works as an Eshell command."
  (let ((eshell-banner-message "")
        (eshell-prompt-function (lambda () "$ ")))
    (with-temp-eshell
      (eshell-insert-command "echo hi")
      (eshell-insert-command "clear")
      (should (string-match "\\`\\$ echo hi\nhi\n\\$ clear\n+\\$ "
                            (buffer-string))))))

(ert-deftest esh-mode-test/clear/eshell-command/erase ()
  "Test that `eshell/clear' can erase the buffer."
  (let ((eshell-banner-message "")
        (eshell-prompt-function (lambda () "$ ")))
    (with-temp-eshell
      (eshell-insert-command "echo hi")
      (eshell-insert-command "clear t")
      (should (string-match "\\`\\$ " (buffer-string))))))

(ert-deftest esh-mode-test/clear/emacs-command ()
  "Test that `eshell-clear' works as an interactive Emacs command."
  (let ((eshell-banner-message "")
        (eshell-prompt-function (lambda () "$ ")))
    (with-temp-eshell
      (eshell-insert-command "echo hi")
      (insert "echo b")
      (eshell-clear)
      (should (string-match "\\`\\$ echo hi\nhi\n\n+\\$ echo b"
                            (buffer-string))))))

(ert-deftest esh-mode-test/clear/emacs-command/erase ()
  "Test that `eshell-clear' can erase the buffer."
  (let ((eshell-banner-message "")
        (eshell-prompt-function (lambda () "$ ")))
    (with-temp-eshell
      (eshell-insert-command "echo hi")
      (insert "echo b")
      (eshell-clear t)
      (should (string-match "\\`\\$ echo b" (buffer-string))))))

;; esh-mode-tests.el ends here
