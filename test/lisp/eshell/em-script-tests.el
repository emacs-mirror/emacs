;;; em-script-tests.el --- em-script test suite  -*- lexical-binding:t -*-

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

;; Tests for Eshell's script module.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'esh-mode)
(require 'eshell)
(require 'em-script)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar eshell-execute-file-output)

;;; Tests:

(ert-deftest em-script-test/source-script ()
  "Test sourcing a simple script."
  (ert-with-temp-file temp-file :text "echo hi"
    (with-temp-eshell
     (eshell-match-command-output (format "source %s" temp-file)
                                  "hi\n"))))

(ert-deftest em-script-test/source-script/redirect ()
  "Test sourcing a script and redirecting its output."
  (ert-with-temp-file temp-file
    :text "echo hi\necho bye"
    (eshell-with-temp-buffer bufname "old"
      (with-temp-eshell
       (eshell-match-command-output
        (format "source %s > #<%s>" temp-file bufname)
        "\\`\\'"))
      (should (equal (buffer-string) "hibye")))))

(ert-deftest em-script-test/source-script/redirect/dev-null ()
  "Test sourcing a script and redirecting its output, including to /dev/null."
  (ert-with-temp-file temp-file
    :text "echo hi\necho bad > /dev/null\necho bye"
    (eshell-with-temp-buffer bufname "old"
      (with-temp-eshell
       (eshell-match-command-output
        (format "source %s > #<%s>" temp-file bufname)
        "\\`\\'"))
      (should (equal (buffer-string) "hibye")))))

(ert-deftest em-script-test/source-script/background ()
  "Test sourcing a script in the background."
  (skip-unless (executable-find "echo"))
  (ert-with-temp-file temp-file
    :text "*echo hi\nif {[ foo = foo ]} {*echo bye}"
    (eshell-with-temp-buffer bufname "old"
      (with-temp-eshell
       (eshell-match-command-output
        (format "source %s > #<%s> &" temp-file bufname)
        "\\`\\'")
       (eshell-wait-for-subprocess t))
      (should (equal (buffer-string) "hi\nbye\n")))))

(ert-deftest em-script-test/source-script/arg-vars ()
  "Test sourcing script with $0, $1, ... variables."
  (ert-with-temp-file temp-file :text "printnl $0 \"$1 $2\""
    (with-temp-eshell
     (eshell-match-command-output (format "source %s one two" temp-file)
                                  (format "%s\none two\n" temp-file)))))

(ert-deftest em-script-test/source-script/all-args-var ()
  "Test sourcing script with the $* variable."
  (ert-with-temp-file temp-file :text "printnl $*"
    (with-temp-eshell
     (eshell-match-command-output (format "source %s" temp-file)
                                  "\\`\\'")
     (eshell-match-command-output (format "source %s a" temp-file)
                                  "a\n")
     (eshell-match-command-output (format "source %s a b c" temp-file)
                                  "a\nb\nc\n"))))

(ert-deftest em-script-test/execute-file ()
  "Test running an Eshell script file via `eshell-execute-file'."
  (ert-with-temp-file temp-file
    :text "echo hi\necho bye"
    (with-temp-buffer
      (with-temp-eshell-settings
        (eshell-execute-file temp-file nil t))
      (should (equal (buffer-string) "hibye")))))

(ert-deftest em-script-test/execute-file/args ()
  "Test running an Eshell script file with args via `eshell-execute-file'."
  (ert-with-temp-file temp-file
    :text "+ $@*"
    (with-temp-buffer
      (with-temp-eshell-settings
        (eshell-execute-file temp-file '(1 2 3) t))
      (should (equal (buffer-string) "6")))))

(ert-deftest em-script-test/execute-file/output-file ()
  "Test `eshell-execute-file' redirecting to a file."
  (ert-with-temp-file temp-file :text "echo more"
    (ert-with-temp-file output-file :text "initial"
      (with-temp-eshell-settings
        (eshell-execute-file temp-file nil output-file))
      (should (equal (eshell-test-file-string output-file) "moreinitial")))))

(ert-deftest em-script-test/execute-file/output-symbol ()
  "Test `eshell-execute-file' redirecting to a symbol."
  (ert-with-temp-file temp-file :text "echo hi\necho bye"
    (with-temp-eshell-settings
      (eshell-execute-file temp-file nil 'eshell-execute-file-output))
    (should (equal eshell-execute-file-output "hibye"))))

(ert-deftest em-script-test/batch-file ()
  "Test running an Eshell script file as a batch script."
  (ert-with-temp-file temp-file
    :text "echo hi"
    (with-temp-buffer
      (with-temp-eshell-settings
       (call-process (expand-file-name invocation-name invocation-directory)
                     nil '(t nil) nil
                     "--batch" "-f" "eshell-batch-file" temp-file))
      (should (equal (buffer-string) "hi\n")))))

(ert-deftest em-script-test/batch-file/shebang ()
  "Test running an Eshell script file as a batch script via a shebang."
  (skip-when (or (memq system-type '(windows-nt ms-dos))
                 ;; OpenBSD's env does not support -S
                 (and (eq system-type 'berkeley-unix)
                      (string-match-p "openbsd" system-configuration))))
  (ert-with-temp-file temp-file
    :text (format
           "#!/usr/bin/env -S %s --batch -f eshell-batch-file\necho hi"
           (expand-file-name invocation-name invocation-directory))
    (set-file-modes temp-file #o744)
    (with-temp-buffer
      (with-temp-eshell-settings
        (call-process temp-file nil '(t nil)))
      (should (equal (buffer-string) "hi\n")))))

;; em-script-tests.el ends here
