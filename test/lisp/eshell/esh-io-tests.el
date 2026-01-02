;;; esh-io-tests.el --- esh-io test suite  -*- lexical-binding:t -*-

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'esh-mode)
(require 'eshell)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar eshell-test-value nil)

(defvar eshell-test-value-with-fun nil)
(defun eshell-test-value-with-fun ())

(defun eshell/test-output ()
  "Write some test output separately to stdout and stderr."
  (eshell-printn "stdout")
  (eshell-errorn "stderr"))

;;; Tests:


;; Newlines

(ert-deftest esh-io-test/output-newline/add-newline ()
  "Ensure we add a newline when writing a string to stdout."
  (with-temp-eshell
    (eshell-match-command-output "(concat \"hello\")" "\\`hello\n\\'")))

(ert-deftest esh-io-test/output-newline/no-newline ()
  "Ensure we don't add a newline when writing a string to a buffer."
  (eshell-with-temp-buffer bufname ""
    (with-temp-eshell
      (eshell-match-command-output
       (format "(concat \"hello\") > #<%s>" bufname)
       "\\`\\'"))
    (should (equal (buffer-string) "hello"))))

(ert-deftest esh-io-test/output-newline/no-extra-newline ()
  "Ensure we don't add an extra newline when writing to stdout."
  (with-temp-eshell
    (eshell-match-command-output "(concat \"hello\n\")" "\\`hello\n\\'")))


;; Basic redirection

(ert-deftest esh-io-test/redirect-file/overwrite ()
  "Check that redirecting to a file in overwrite mode works."
  (ert-with-temp-file temp-file
    :text "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new > %s" temp-file)))
    (should (equal (eshell-test-file-string temp-file) "new"))))

(ert-deftest esh-io-test/redirect-file/append ()
  "Check that redirecting to a file in append mode works."
  (ert-with-temp-file temp-file
    :text "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new >> %s" temp-file)))
    (should (equal (eshell-test-file-string temp-file) "oldnew"))))

(ert-deftest esh-io-test/redirect-file/insert ()
  "Check that redirecting to a file in insert works."
  (ert-with-temp-file temp-file
    :text "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new >>> %s" temp-file)))
    (should (equal (eshell-test-file-string temp-file) "newold"))))

(ert-deftest esh-io-test/redirect-buffer/overwrite ()
  "Check that redirecting to a buffer in overwrite mode works."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new > #<%s>" bufname)))
    (should (equal (buffer-string) "new"))))

(ert-deftest esh-io-test/redirect-buffer/append ()
  "Check that redirecting to a buffer in append mode works."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new >> #<%s>" bufname)))
    (should (equal (buffer-string) "oldnew"))))

(ert-deftest esh-io-test/redirect-buffer/insert ()
  "Check that redirecting to a buffer in insert mode works."
  (eshell-with-temp-buffer bufname "old"
    (goto-char (point-min))
    (with-temp-eshell
     (eshell-insert-command (format "echo new >>> #<%s>" bufname)))
    (should (equal (buffer-string) "newold"))))

(ert-deftest esh-io-test/redirect-buffer/escaped ()
  "Check that redirecting to a buffer with escaped characters works."
  (with-temp-buffer
    (rename-buffer "eshell\\temp\\buffer" t)
    (let ((bufname (buffer-name)))
      (with-temp-eshell
       (eshell-insert-command (format "echo hi > #<%s>"
                                      (string-replace "\\" "\\\\" bufname))))
      (should (equal (buffer-string) "hi")))))

(ert-deftest esh-io-test/redirect-symbol/overwrite ()
  "Check that redirecting to a symbol in overwrite mode works."
  (let ((eshell-test-value "old"))
    (with-temp-eshell
     (eshell-insert-command "echo new > #'eshell-test-value"))
    (should (equal eshell-test-value "new"))))

(ert-deftest esh-io-test/redirect-symbol/append ()
  "Check that redirecting to a symbol in append mode works."
  (let ((eshell-test-value "old"))
    (with-temp-eshell
     (eshell-insert-command "echo new >> #'eshell-test-value"))
    (should (equal eshell-test-value "oldnew"))))

(ert-deftest esh-io-test/redirect-symbol/with-function-slot ()
  "Check that redirecting to a symbol with function slot set works."
  (let ((eshell-test-value-with-fun))
    (with-temp-eshell
     (eshell-insert-command "echo hi > #'eshell-test-value-with-fun"))
    (should (equal eshell-test-value-with-fun "hi"))))

(ert-deftest esh-io-test/redirect-marker ()
  "Check that redirecting to a marker works."
  (with-temp-buffer
    (let ((eshell-test-value (point-marker)))
      (with-temp-eshell
       (eshell-insert-command "echo hi > $eshell-test-value"))
      (should (equal (buffer-string) "hi")))))

(ert-deftest esh-io-test/redirect-multiple ()
  "Check that redirecting to multiple targets works."
  (let ((eshell-test-value "old"))
    (eshell-with-temp-buffer bufname "old"
     (with-temp-eshell
      (eshell-insert-command (format "echo new > #<%s> > #'eshell-test-value"
                                     bufname)))
     (should (equal (buffer-string) "new"))
     (should (equal eshell-test-value "new")))))

(ert-deftest esh-io-test/redirect-multiple/repeat ()
  "Check that redirecting to multiple targets works when repeating a target."
  (let ((eshell-test-value "old"))
    (eshell-with-temp-buffer bufname "old"
     (with-temp-eshell
      (eshell-insert-command
       (format "echo new > #<%s> > #'eshell-test-value > #<%s>"
               bufname bufname)))
     (should (equal (buffer-string) "new"))
     (should (equal eshell-test-value "new")))))

(ert-deftest esh-io-test/redirect-subcommands ()
  "Check that redirecting subcommands applies to all subcommands."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command (format "{echo foo; echo bar} > #<%s>" bufname)))
    (should (equal (buffer-string) "foobar"))))

(ert-deftest esh-io-test/redirect-subcommands/override ()
  "Check that redirecting subcommands applies to all subcommands.
Include a redirect to another location in the subcommand to
ensure only its statement is redirected."
  (eshell-with-temp-buffer bufname "old"
    (eshell-with-temp-buffer bufname-2 "also old"
      (with-temp-eshell
       (eshell-insert-command
        (format "{echo foo; echo bar > #<%s>; echo baz} > #<%s>"
                bufname-2 bufname)))
      (should (equal (buffer-string) "bar")))
    (should (equal (buffer-string) "foobaz"))))

(ert-deftest esh-io-test/redirect-subcommands/dev-null ()
  "Check that redirecting subcommands applies to all subcommands.
Include a redirect to /dev/null to ensure it only applies to its
statement."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command
      (format "{echo foo; echo bar > /dev/null; echo baz} > #<%s>"
              bufname)))
    (should (equal (buffer-string) "foobaz"))))

(ert-deftest esh-io-test/redirect-subcommands/interpolated ()
  "Check that redirecting interpolated subcommands applies to all subcommands."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command
      (format "echo ${echo foo; echo bar} > #<%s>" bufname)))
    (should (equal (buffer-string) "foobar"))))


;; Redirecting specific handles

(ert-deftest esh-io-test/redirect-stdout ()
  "Check that redirecting to stdout doesn't redirect stderr."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output > #<%s>" bufname)
                                  "stderr\n"))
    (should (equal (buffer-string) "stdout\n")))
  ;; Also check explicitly specifying the stdout fd.
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 1> #<%s>" bufname)
                                  "stderr\n"))
    (should (equal (buffer-string) "stdout\n"))))

(ert-deftest esh-io-test/redirect-stderr/overwrite ()
  "Check that redirecting to stderr doesn't redirect stdout."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2> #<%s>" bufname)
                                  "stdout\n"))
    (should (equal (buffer-string) "stderr\n"))))

(ert-deftest esh-io-test/redirect-stderr/append ()
  "Check that redirecting to stderr doesn't redirect stdout."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2>> #<%s>" bufname)
                                  "stdout\n"))
    (should (equal (buffer-string) "oldstderr\n"))))

(ert-deftest esh-io-test/redirect-stderr/insert ()
  "Check that redirecting to stderr doesn't redirect stdout."
  (eshell-with-temp-buffer bufname "old"
    (goto-char (point-min))
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2>>> #<%s>" bufname)
                                  "stdout\n"))
    (should (equal (buffer-string) "stderr\nold"))))

(ert-deftest esh-io-test/redirect-stdout-and-stderr ()
  "Check that redirecting to both stdout and stderr works."
  (eshell-with-temp-buffer bufname-1 "old"
    (eshell-with-temp-buffer bufname-2 "old"
      (with-temp-eshell
       (eshell-match-command-output (format "test-output > #<%s> 2> #<%s>"
                                            bufname-1 bufname-2)
                                    "\\`\\'"))
      (should (equal (buffer-string) "stderr\n")))
    (should (equal (buffer-string) "stdout\n"))))

(ert-deftest esh-io-test/redirect-all/overwrite ()
  "Check that redirecting to stdout and stderr via shorthand works."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output &> #<%s>" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "stdout\nstderr\n")))
  ;; Also check the alternate (and less-preferred in Bash) `>&' syntax.
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output >& #<%s>" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "stdout\nstderr\n"))))

(ert-deftest esh-io-test/redirect-all/append ()
  "Check that redirecting to stdout and stderr via shorthand works."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output &>> #<%s>" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "oldstdout\nstderr\n")))
  ;; Also check the alternate (and less-preferred in Bash) `>>&' syntax.
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output >>& #<%s>" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "oldstdout\nstderr\n"))))

(ert-deftest esh-io-test/redirect-all/insert ()
  "Check that redirecting to stdout and stderr via shorthand works."
  (eshell-with-temp-buffer bufname "old"
    (goto-char (point-min))
    (with-temp-eshell
     (eshell-match-command-output (format "test-output &>>> #<%s>" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "stdout\nstderr\nold")))
  ;; Also check the alternate `>>>&' syntax.
  (eshell-with-temp-buffer bufname "old"
    (goto-char (point-min))
    (with-temp-eshell
     (eshell-match-command-output (format "test-output >>>& #<%s>" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "stdout\nstderr\nold"))))

(ert-deftest esh-io-test/redirect-copy ()
  "Check that redirecting stdout and then copying stdout to stderr works.
This should redirect both stdout and stderr to the same place."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output > #<%s> 2>&1" bufname)
                                  "\\`\\'"))
    (should (equal (buffer-string) "stdout\nstderr\n"))))

(ert-deftest esh-io-test/redirect-copy-first ()
  "Check that copying stdout to stderr and then redirecting stdout works.
This should redirect stdout to a buffer, and stderr to where
stdout originally pointed (the terminal)."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2>&1 > #<%s>" bufname)
                                  "stderr\n"))
    (should (equal (buffer-string) "stdout\n"))))


;; Pipelines

(ert-deftest esh-io-test/pipeline/default ()
  "Check that `|' only pipes stdout."
  (skip-unless (executable-find "rev"))
  (eshell-command-result-equal "test-output | rev"
                               "stderr\ntuodts\n"))


(ert-deftest esh-io-test/pipeline/all ()
  "Check that `|&' only pipes stdout and stderr."
  (skip-unless (executable-find "rev"))
  (eshell-command-result-equal "test-output |& rev"
                               "tuodts\nrredts\n"))

(ert-deftest esh-io-test/pipeline/subcommands ()
  "Check that all commands in a subcommand are properly piped."
  (skip-unless (executable-find "rev"))
  (with-temp-eshell
   (eshell-match-command-output "{echo foo; echo bar} | rev"
                                "\\`raboof\n?")))

(ert-deftest esh-io-test/pipeline/stdin-to-head ()
  "Check that standard input is sent to the head process in a pipeline."
  (skip-unless (and (executable-find "tr")
                    (executable-find "rev")))
  (with-temp-eshell
   (eshell-insert-command "tr a-z A-Z | rev")
   (eshell-insert-command "hello")
   (eshell-send-eof-to-process)
   (eshell-wait-for-subprocess)
   (should (eshell-match-output "OLLEH\n"))))


;; Virtual targets

(ert-deftest esh-io-test/virtual/dev-null ()
  "Check that redirecting to /dev/null works."
  (with-temp-eshell
   (eshell-match-command-output "echo hi > /dev/null" "\\`\\'")))

(ert-deftest esh-io-test/virtual/dev-null/multiple ()
  "Check that redirecting to /dev/null works alongside other redirections."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output
      (format "echo new > /dev/null > #<%s>" bufname) "\\`\\'"))
    (should (equal (buffer-string) "new")))
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output
      (format "echo new > #<%s> > /dev/null" bufname) "\\`\\'"))
    (should (equal (buffer-string) "new"))))

(ert-deftest esh-io-test/virtual/dev-eshell ()
  "Check that redirecting to /dev/eshell works."
  (with-temp-eshell
   (eshell-match-command-output "echo hi > /dev/eshell" "hi")))

(ert-deftest esh-io-test/virtual/dev-kill ()
  "Check that redirecting to /dev/kill works."
  (with-temp-eshell
   (eshell-insert-command "echo one > /dev/kill")
   (should (equal (car kill-ring) "one"))
   (eshell-insert-command "echo two > /dev/kill")
   (should (equal (car kill-ring) "two"))
   (eshell-insert-command "echo three >> /dev/kill")
   (should (equal (car kill-ring) "twothree"))))

(ert-deftest esh-io-test/virtual/device-close ()
  "Check that the close function for `eshell-function-target' works."
  (let* ((data nil)
         (status nil)
         (eshell-virtual-targets
          `(("/dev/virtual"
             ,(eshell-function-target-create
               (lambda (d) (setq data d))
               (lambda (s) (setq status s)))
             nil))))
    (with-temp-eshell
     (eshell-insert-command "echo hello > /dev/virtual")
     (should (equal data "hello"))
     (should (equal status t)))))

;;; esh-io-tests.el ends here
