;;; pcomplete-tests.el --- Tests for pcomplete.el  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'pcomplete)

(ert-deftest pcomplete-test-parse-gpg-help ()
  (cl-letf ((pcomplete-from-help (make-hash-table :test #'equal))
            ((symbol-function 'call-process)
             (lambda (&rest _) (insert "\
gpg (GnuPG) 2.3.7

Commands:

 -s, --sign                         make a signature
     --clear-sign                   make a clear text signature
 -b, --detach-sign                  make a detached signature
     --tofu-policy VALUE            set the TOFU policy for a key

Options to specify keys:
 -r, --recipient USER-ID            encrypt for USER-ID
 -u, --local-user USER-ID           use USER-ID to sign or decrypt

(See the man page for a complete listing of all commands and options)

Examples:

 -se -r Bob [file]          sign and encrypt for user Bob
 --clear-sign [file]        make a clear text signature
"))))
    (should
     (equal-including-properties
      (pcomplete-from-help "gpg --help" :narrow-end "^ -se")
      '(#("-s" 0 1 (pcomplete-help "make a signature"))
        #("--sign" 0 1 (pcomplete-help "make a signature"))
        #("--clear-sign" 0 1 (pcomplete-help "make a clear text signature"))
        #("-b" 0 1 (pcomplete-help "make a detached signature"))
        #("--detach-sign" 0 1 (pcomplete-help "make a detached signature"))
        #("--tofu-policy" 0 1
          (pcomplete-help "set the TOFU policy for a key" pcomplete-annotation " VALUE"))
        #("-r" 0 1 (pcomplete-help "encrypt for USER-ID"))
        #("--recipient" 0 1
          (pcomplete-help "encrypt for USER-ID" pcomplete-annotation " USER-ID"))
        #("-u" 0 1
          (pcomplete-help "use USER-ID to sign or decrypt"))
        #("--local-user" 0 1
          (pcomplete-help "use USER-ID to sign or decrypt" pcomplete-annotation " USER-ID")))))))

(ert-deftest pcomplete-test-parse-git-help ()
  (cl-letf ((pcomplete-from-help (make-hash-table :test #'equal))
            ((symbol-function 'call-process)
             (lambda (&rest _) (insert "\
usage: git [-v | --version] [-h | --help] [-C <path>] [-c <name>=<value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [-p | --paginate | -P | --no-pager] [--no-replace-objects] [--bare]
           [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
           [--super-prefix=<path>] [--config-env=<name>=<envvar>]
           <command> [<args>]
"))))
    (should
     (equal-including-properties
      (pcomplete-from-help "git help"
                           :margin "\\(\\[\\)-"
                           :separator " | "
                           :description "\\`")
      '("-v" "--version" "-h" "--help"
        #("-C" 0 1 (pcomplete-annotation " <path>"))
        #("-c" 0 1 (pcomplete-annotation " <name>"))
        #("--exec-path" 0 1 (pcomplete-annotation "[=<path>]"))
        "--html-path" "--man-path" "--info-path"
        "-p" "--paginate" "-P" "--no-pager"
        "--no-replace-objects" "--bare"
        #("--git-dir=" 0 1 (pcomplete-annotation "<path>"))
        #("--work-tree=" 0 1 (pcomplete-annotation "<path>"))
        #("--namespace=" 0 1 (pcomplete-annotation "<name>"))
        #("--super-prefix=" 0 1 (pcomplete-annotation "<path>"))
        #("--config-env=" 0 1 (pcomplete-annotation "<name>")))))))

(defmacro pcomplete-shell-completion (input &rest body)
  "Insert INPUT into shell mode, run completion, and return the result.
BODY is a list of extra forms to call after completion (e.g. to choose a
completion from the minibuffer)."
  (declare (indent 1))
  `(with-temp-buffer
     (shell (current-buffer))
     (unwind-protect
         (let ((start (point)))
           (insert ,input)
           (completion-at-point)
           ,@body
           (buffer-substring start (point)))
       (let (kill-buffer-query-functions)
         (kill-buffer (current-buffer))))))

(defun pcomplete/pcmpl-option-test ()
  "Completion for a test command taking long options."
  (while (pcomplete-match "^--" 0)
    (pcomplete-here* (pcomplete-long-option-completion-table
                      '("--version" "--owner=")))))

(ert-deftest pcomplete-test-long-options/with-value ()
  "Test that completing long options taking values doesn't insert a space."
  (should (string= (pcomplete-shell-completion "pcmpl-option-test --own")
                   "pcmpl-option-test --owner=")))

(ert-deftest pcomplete-test-long-options/without-value ()
  "Test that completing long options without values inserts a space."
  (should (string= (pcomplete-shell-completion "pcmpl-option-test --ver")
                   "pcmpl-option-test --version ")))

(ert-deftest pcomplete-test-long-options/with-value/multiple ()
  "Test that completing long options taking values doesn't insert a space.
This tests completing when there are multiple possibilities."
  (should (string= (pcomplete-shell-completion "pcmpl-option-test --"
                     (minibuffer-next-completion 1)
                     (minibuffer-choose-completion))
                   "pcmpl-option-test --owner=")))

(ert-deftest pcomplete-test-long-options/without-value/multiple ()
  "Test that completing long options without values inserts a space.
This tests completing when there are multiple possibilities."
  (should (string= (pcomplete-shell-completion "pcmpl-option-test --"
                     (minibuffer-next-completion 2)
                     (minibuffer-choose-completion))
                   "pcmpl-option-test --version ")))

(provide 'pcomplete-tests)
;;; pcomplete-tests.el ends here
