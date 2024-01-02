;;; esh-ext-tests.el --- esh-ext test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's external command handling.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'esh-ext)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

;;; Tests:

(ert-deftest esh-ext-test/addpath/end ()
  "Test that \"addpath\" adds paths to the end of $PATH."
  (with-temp-eshell
   (let ((eshell-path-env-list '("/some/path" "/other/path"))
         (expected-path (string-join '("/some/path" "/other/path" "/new/path"
                                       "/new/path2")
                                     (path-separator))))
     (eshell-match-command-output "addpath /new/path /new/path2"
                                  (concat expected-path "\n"))
     (eshell-match-command-output "echo $PATH"
                                  (concat expected-path "\n")))))

(ert-deftest esh-ext-test/addpath/begin ()
  "Test that \"addpath -b\" adds paths to the beginning of $PATH."
  (with-temp-eshell
   (let ((eshell-path-env-list '("/some/path" "/other/path"))
         (expected-path (string-join '("/new/path" "/new/path2" "/some/path"
                                       "/other/path")
                                     (path-separator))))
     (eshell-match-command-output "addpath -b /new/path /new/path2"
                                  (concat expected-path "\n"))
     (eshell-match-command-output "echo $PATH"
                                  (concat expected-path "\n")))))

(ert-deftest esh-ext-test/addpath/set-locally ()
  "Test adding to the path temporarily in a subcommand."
  (let* ((eshell-path-env-list '("/some/path" "/other/path"))
         (original-path (string-join eshell-path-env-list (path-separator)))
         (local-path (string-join (append eshell-path-env-list '("/new/path"))
                                  (path-separator))))
    (with-temp-eshell
     (eshell-match-command-output
      "{ addpath /new/path; env }"
      (format "PATH=%s\n" (regexp-quote local-path)))
     ;; After the last command, the previous $PATH value should be restored.
     (eshell-match-command-output "echo $PATH"
                                  (concat original-path "\n")))))

;; esh-ext-tests.el ends here
