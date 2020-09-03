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

;; This is a collection of tests for the fontification of CPerl-mode.

;; Run these tests interactively:
;; (ert-run-tests-interactively '(tag :fontification))

;;; Code:

(defvar cperl-test-mode #'cperl-mode)

(require 'cperl-mode)

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

;;; cperl-mode-tests.el ends here
