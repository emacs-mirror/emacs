;;; esh-proc-tests.el --- esh-proc test suite  -*- lexical-binding:t -*-

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

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(ert-deftest esh-proc-test/sigpipe-exits-process ()
  "Test that a SIGPIPE is properly sent to a process if a pipe closes"
  (skip-unless (and (executable-find "sh")
                    (executable-find "echo")
                    (executable-find "sleep")))
  (with-temp-eshell
   (eshell-command-result-p
    ;; The first command is like `yes' but slower.  This is to prevent
    ;; it from taxing Emacs's process filter too much and causing a
    ;; hang.
    (concat "sh -c 'while true; do echo y; sleep 1; done' | "
            "sh -c 'read NAME; echo ${NAME}'")
    "y\n")
   (eshell-wait-for-subprocess t)
   (should (eq (process-list) nil))))
