;;; em-script-tests.el --- em-script test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's script module.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'em-script)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))
;;; Tests:

(ert-deftest em-script-test/source-script ()
  "Test sourcing script with no argumentss"
  (ert-with-temp-file temp-file :text "echo hi"
    (with-temp-eshell
     (eshell-match-command-output (format "source %s" temp-file)
                                  "hi\n"))))

(ert-deftest em-script-test/source-script-arg-vars ()
  "Test sourcing script with $0, $1, ... variables"
  (ert-with-temp-file temp-file :text "printnl $0 \"$1 $2\""
    (with-temp-eshell
     (eshell-match-command-output (format "source %s one two" temp-file)
                                  (format "%s\none two\n" temp-file)))))

(ert-deftest em-script-test/source-script-all-args-var ()
  "Test sourcing script with the $* variable"
  (ert-with-temp-file temp-file :text "printnl $*"
    (with-temp-eshell
     (eshell-match-command-output (format "source %s" temp-file)
                                  "\\`\\'")
     (eshell-match-command-output (format "source %s a" temp-file)
                                  "a\n")
     (eshell-match-command-output (format "source %s a b c" temp-file)
                                  "a\nb\nc\n"))))

;; em-script-tests.el ends here
