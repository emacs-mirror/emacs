;;; esh-mode-tests.el --- esh-mode test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's command invocation.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

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

;; esh-mode-tests.el ends here
