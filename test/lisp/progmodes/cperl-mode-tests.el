;;; cperl-mode-tests --- Test for cperl-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords: internal
;; Homepage: https://github.com/HaraldJoerg/cperl-mode

;;; Commentary:

;; This is a collection of tests for the fontification of CPerl-mode.

;; Run these tests interactively:
;; (ert-run-tests-interactively '(tag :fontification))

;;; Code:

(defun cperl-test-face (text regexp)
  "Returns the face of the first character matched by REGEXP in TEXT."
  (interactive)
  (with-temp-buffer
      (insert text)
      (cperl-mode)
      (font-lock-ensure (point-min) (point-max))
      (goto-char (point-min))
      (re-search-forward regexp)
      (get-text-property (match-beginning 0) 'face)))

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
    (should (equal (cperl-test-face code "/" ) nil)))
  (let ((code "{ $a-- / $b }"))
    (should (equal (cperl-test-face code "/" ) nil)))
  ;; The next two Perl expressions have regular expressions.  The
  ;; delimiter of a RE is fontified with font-lock-constant-face.
  (let ((code "{ $a+ / $b } # /"))
    (should (equal (cperl-test-face code "/" ) font-lock-constant-face)))
  (let ((code "{ $a- / $b } # /"))
    (should (equal (cperl-test-face code "/" ) font-lock-constant-face))))

;;; cperl-mode-tests.el ends here
