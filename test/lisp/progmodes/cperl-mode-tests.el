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
(require 'ert)
(require 'ert-x)

;;; Utilities

(defun cperl-test-ppss (text regexp)
  "Return the `syntax-ppss' of the first character matched by REGEXP in TEXT."
  (interactive)
  (with-temp-buffer
    (insert text)
    (funcall cperl-test-mode)
    (goto-char (point-min))
    (re-search-forward regexp)
    (syntax-ppss)))

(defmacro cperl--run-test-cases (file &rest body)
  "Run all test cases in FILE with BODY.
This macro helps with tests which reformat Perl code, e.g. when
indenting or rearranging flow control.  It extracts source code
snippets and corresponding expected results from a resource file,
runs BODY on the snippets, and compares the resulting buffer with
the expected results.

Test cases in FILE are formatted like this:

# -------- NAME: input --------
Your input to the test case comes here.
Both input and expected output may span several lines.
# -------- NAME: expected output --------
The expected output from running BODY on the input goes here.
# -------- NAME: end --------

You can have many of these blocks in one test file.  You can
chose a NAME for each block, which is passed to the 'should'
clause for easy identification of the first test case that
failed (if any).  Text outside these the blocks is ignored by the
tests, so you can use it to document the test cases if you wish."
  `(with-temp-buffer
     (insert-file-contents ,file)
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
           (funcall cperl-test-mode)
           ,@body
           (setq expected (concat "test case " name ":\n" expected))
           (setq got (concat "test case " name ":\n" (buffer-string)))
           (should (equal got expected)))))))

;;; Indentation tests

(ert-deftest cperl-test-indent-exp ()
  "Run various tests for `cperl-indent-exp' edge cases.
These exercise some standard blocks and also the special
treatment for Perl expressions where a closing paren isn't the
end of the statement."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (cperl--run-test-cases
   (ert-resource-file "cperl-indent-exp.pl")
   (cperl-indent-exp))) ; here we go!

(ert-deftest cperl-test-indent-styles ()
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (cperl--run-test-cases
   (ert-resource-file "cperl-indent-styles.pl")
   (cperl-set-style "PBP")
   (indent-region (point-min) (point-max)) ; here we go!
   (cperl-set-style-back)))

;;; Fontification tests

(ert-deftest cperl-test-fontify-punct-vars ()
  "Test fontification of Perl's punctiation variables.
Perl has variable names containing unbalanced quotes for the list
separator $\" and pre- and postmatch $` and $'.  A reference to
these variables, for example \\$\", should not cause the dollar
to be escaped, which would then start a string beginning with the
quote character.  This used to be broken in cperl-mode at some
point in the distant past, and is still broken in perl-mode. "
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((file (ert-resource-file "fontify-punctuation-vars.pl")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (while (search-forward "##" nil t)
        ;; The third element of syntax-ppss is true if in a string,
        ;; which would indicate bad interpretation of the quote.  The
        ;; fourth element is true if in a comment, which should be the
        ;; case.
        (should (equal (nth 3 (syntax-ppss)) nil))
        (should (equal (nth 4 (syntax-ppss)) t))))))

;;; Tests for issues reported in the Bug Tracker

(defun cperl-test--run-bug-10483 ()
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

(ert-deftest cperl-test-bug-10483 ()
  "Check that indenting certain perl code does not loop forever.
This verifies that indenting a piece of code that ends in a paren
without a statement terminator on the same line does not loop
forever.  The test starts an asynchronous Emacs batch process
under timeout control."
  :tags '(:expensive-test)
  (interactive)
  (skip-unless (not (getenv "EMACS_HYDRA_CI"))) ; FIXME times out
  (skip-unless (not (< emacs-major-version 28))) ; times out in older Emacsen
  (let* ((emacs (concat invocation-directory invocation-name))
         (test-function 'cperl-test--run-bug-10483)
         (test-function-name (symbol-name test-function))
         (test-file (symbol-file test-function 'defun))
         (ran-out-of-time nil)
         (process-connection-type nil)
         runner)
    (with-temp-buffer
      (with-timeout (2
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

(ert-deftest cperl-test-bug-16368 ()
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

(ert-deftest cperl-test-bug-19709 ()
  "Verify that indentation of closing paren works as intended.
Note that Perl mode has no setting for close paren offset, per
documentation it does the right thing anyway."
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-19709.pl")
   ;; settings from the bug report
   (setq-local cperl-indent-level 4)
   (setq-local cperl-indent-parens-as-block t)
   (setq-local  cperl-close-paren-offset -4)
   ;; same, adapted for per-mode
   (setq-local perl-indent-level 4)
   (setq-local perl-indent-parens-as-block t)
   (while (null (eobp))
     (cperl-indent-command)
     (forward-line 1))))

(ert-deftest cperl-test-bug-28650 ()
  "Verify that regular expressions are recognized after 'return'.
The test uses the syntax property \"inside a string\" for the
text in regular expressions, which is non-nil for both cperl-mode
and perl-mode."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-26850.pl"))
    (goto-char (point-min))
    (re-search-forward "sub interesting {[^}]*}")
    (should-not (equal (nth 3 (cperl-test-ppss (match-string 0) "Today"))
                       nil))
    (re-search-forward "sub boring {[^}]*}")
    (should-not (equal (nth 3 (cperl-test-ppss (match-string 0) "likes\\?"))
                       nil))))

(ert-deftest cperl-test-bug-30393 ()
  "Verify that indentation is not disturbed by an open paren in col 0.
Perl is not Lisp: An open paren in column 0 does not start a function."
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-30393.pl")
   (while (null (eobp))
     (cperl-indent-command)
     (forward-line 1))))

(ert-deftest cperl-test-bug-37127 ()
  "Verify that closing a paren in a regex goes without a message.
Also check that the message is issued if the regex terminator is
missing."
  ;; The actual fix for this bug is in simple.el, which is not
  ;; backported to older versions of Emacs.  Therefore we skip this
  ;; test if we're running Emacs 27 or older.
  (skip-unless (< 27 emacs-major-version))
  ;; Part one: Regex is ok, no messages
  (ert-with-message-capture collected-messages
    (with-temp-buffer
      (insert "$_ =~ /(./;")
      (funcall cperl-test-mode)
      (goto-char (point-min))
      (search-forward ".")
      (let ((last-command-event ?\))
            ;; Don't emit "Matches ..." even if not visible (e.g. in batch).
            (blink-matching-paren 'jump-offscreen))
        (self-insert-command 1)
        ;; `self-insert-command' doesn't call `blink-matching-open' in
        ;; batch mode, so we need to call it explicitly.
        (blink-matching-open))
      (syntax-propertize (point-max)))
    (should (string-equal collected-messages "")))
  ;; part two: Regex terminator missing -> message
  (when (eq cperl-test-mode #'cperl-mode)
    ;; This test is only run in `cperl-mode' because only cperl-mode
    ;; emits a message to warn about such unclosed REs.
    (ert-with-message-capture collected-messages
      (with-temp-buffer
        (insert "$_ =~ /(..;")
        (goto-char (point-min))
        (funcall cperl-test-mode)
        (search-forward ".")
        (let ((last-command-event ?\)))
          (self-insert-command 1))
        (syntax-propertize (point-max)))
      (should (string-match "^End of .* string/RE"
                            collected-messages)))))

(ert-deftest cperl-test-bug-42168 ()
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

;;; cperl-mode-tests.el ends here
