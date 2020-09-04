;;; cperl-mode-tests --- Test for cperl-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords: internal
;; Homepage: https://github.com/HaraldJoerg/cperl-mode

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

;; This is a collection of tests for CPerl-mode.

;;; Code:

(defvar cperl-test-mode #'cperl-mode)

(require 'cperl-mode)

(defvar cperl-mode-tests-data-directory
  (expand-file-name "lisp/progmodes/cperl-mode-resources"
                    (or (getenv "EMACS_TEST_DIRECTORY")
                        (expand-file-name "../../../"
                                          (or load-file-name
                                              buffer-file-name))))
  "Directory containing cperl-mode test data.")

(defun cperl-test-ppss (text regexp)
  "Return the `syntax-ppss' of the first character matched by REGEXP in TEXT."
  (interactive)
  (with-temp-buffer
    (insert text)
    (funcall cperl-test-mode)
    (goto-char (point-min))
    (re-search-forward regexp)
    (syntax-ppss)))

(ert-deftest cperl-mode-test-bug-42168 ()
  "Verify that '/' is a division after ++ or --, not a regexp.
Reported in https://github.com/jrockway/cperl-mode/issues/45.
If seen as regular expression, then the slash is displayed using
font-lock-constant-face.  If seen as a division, then it doesn't
have a face property."
  :tags '(:fontification)
  ;; The next two Perl expressions have divisions.  Perl "punctuation"
  ;; operators don't get a face.
  (let ((code "{ $a++ / $b }"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) nil)))
  (let ((code "{ $a-- / $b }"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) nil)))
  ;; The next two Perl expressions have regular expressions.  The
  ;; delimiter of a RE is fontified with font-lock-constant-face.
  (let ((code "{ $a+ / $b } # /"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) 7)))
  (let ((code "{ $a- / $b } # /"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) 7))))

(ert-deftest cperl-mode-test-bug-16368 ()
  "Verify that `cperl-forward-group-in-re' doesn't hide errors."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((code "/(\\d{4})(?{2}/;")     ; the regex from the bug report
        (result))
    (with-temp-buffer
      (insert code)
      (goto-char 9)
      (setq result (cperl-forward-group-in-re))
      (should (equal (car result) 'scan-error))
      (should (equal (nth 1 result) "Unbalanced parentheses"))
      (should (= (point) 9))))        ; point remains unchanged on error
  (let ((code "/(\\d{4})(?{2})/;")    ; here all parens are balanced
        (result))
    (with-temp-buffer
      (insert code)
      (goto-char 9)
      (setq result (cperl-forward-group-in-re))
      (should (equal result nil))
      (should (= (point) 15)))))      ; point has skipped the group

(defun cperl-mode-test--run-bug-10483 ()
  "Runs a short program, intended to be under timer scrutiny.
This function is intended to be used by an Emacs subprocess in
batch mode.  The message buffer is used to report the result of
running `cperl-indent-exp' for a very simple input.  The result
is expected to be different from the input, to verify that
indentation actually takes place.."
  (let ((code "poop ('foo', \n'bar')")) ; see the bug report
    (message "Test Bug#10483 started")
    (with-temp-buffer
      (insert code)
      (funcall cperl-test-mode)
      (goto-char (point-min))
      (search-forward "poop")
      (cperl-indent-exp)
      (message "%s" (buffer-string)))))

(ert-deftest cperl-mode-test-bug-10483 ()
  "Verifies that a piece of code which ends in a paren without a
statement terminato ron tne same line does not loop forever.  The
test starts an asynchronous Emacs batch process under timeout
control."
  (interactive)
  (let* ((emacs (concat invocation-directory invocation-name))
         (test-function 'cperl-mode-test--run-bug-10483)
         (test-function-name (symbol-name test-function))
         (test-file (symbol-file test-function 'defun))
         (ran-out-of-time nil)
         (process-connection-type nil)
         runner)
    (with-temp-buffer
      (with-timeout (1
                     (delete-process runner)
                     (setq ran-out-of-time t))
        (setq runner (start-process "speedy"
                                    (current-buffer)
                                    emacs
                                    "-batch"
                                    "--quick"
                                    "--load" test-file
                                    "--funcall" test-function-name))
        (while (accept-process-output runner)))
      (should (equal ran-out-of-time nil))
      (goto-char (point-min))
      ;; just a very simple test for indentation: This should
      ;; be rather robust with regard to indentation defaults
      (should (string-match
               "poop ('foo', \n      'bar')" (buffer-string))))))

(ert-deftest cperl-mode-test-indent-exp ()
  "Run various tests for `cperl-indent-exp' edge cases.
These exercise some standard blocks and also the special
treatment for Perl expressions where a closing paren isn't the
end of the statement."
  (let ((file (expand-file-name "cperl-indent-exp.pl"
                                cperl-mode-tests-data-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^# ?-+ \\_<\\(?1:.+?\\)\\_>: input ?-+\n"
                      "\\(?2:\\(?:.*\n\\)+?\\)"
                      "# ?-+ \\1: expected output ?-+\n"
                      "\\(?3:\\(?:.*\n\\)+?\\)"
                      "# ?-+ \\1: end ?-+")
              nil t)
        (let ((name (match-string 1))
              (code (match-string 2))
              (expected (match-string 3))
              got)
          (with-temp-buffer
            (insert code)
            (goto-char (point-min))
            (cperl-indent-exp) ; here we go!
            (setq expected (concat "test case " name ":\n" expected))
            (setq got (concat "test case " name ":\n" (buffer-string)))
            (should (equal got expected))))))))

;;; cperl-mode-tests.el ends here
