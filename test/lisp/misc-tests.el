;;; misc-tests.el --- Tests for misc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'misc)

(defmacro with-misc-test (original result &rest body)
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,original)
     ,@body
     (should (equal (buffer-string) ,result))))

(ert-deftest misc-test-copy-from-above-command ()
  (with-misc-test "abc\n" "abc\nabc"
    (copy-from-above-command))
  (with-misc-test "abc\n" "abc\nab"
    (copy-from-above-command 2)))

(ert-deftest misc-test-zap-up-to-char ()
  (with-misc-test "abcde" "cde"
    (goto-char (point-min))
    (zap-up-to-char 1 ?c))
  (with-misc-test "abcde abc123" "c123"
    (goto-char (point-min))
    (zap-up-to-char 2 ?c))
  (let ((case-fold-search t))
    (with-misc-test "abcdeCXYZ" "cdeCXYZ"
      (goto-char (point-min))
      (zap-up-to-char 1 ?C))
    (with-misc-test "abcdeCXYZ" "CXYZ"
      (goto-char (point-min))
      (zap-up-to-char 1 ?C 'interactive))))

(ert-deftest misc-test-upcase-char ()
  (with-misc-test "abcde" "aBCDe"
    (goto-char (1+ (point-min)))
    (upcase-char 3)))

(ert-deftest misc-test-forward-to-word ()
  (with-temp-buffer
    (insert "    -   abc")
    (goto-char (point-min))
    (forward-to-word 1)
    (should (equal (point) 9)))
  (with-temp-buffer
    (insert "a b c")
    (goto-char (point-min))
    (forward-to-word 3)
    (should (equal (point) 6))))

(ert-deftest misc-test-backward-to-word ()
  (with-temp-buffer
    (insert "abc    -   ")
    (backward-to-word 1)
    (should (equal (point) 4)))
  (with-temp-buffer
    (insert "a b c")
    (backward-to-word 3)
    (should (equal (point) 1))))

(ert-deftest misc--duplicate-line ()
  ;; Duplicate a line (twice).
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (duplicate-line 2)
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 7)))
  ;; Duplicate a line (twice) and move point to the first duplicated line.
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (let ((duplicate-line-final-position 1)) (duplicate-line 2))
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 12)))
  ;; Duplicate a line (twice) and move point to the last duplicated line.
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (let ((duplicate-line-final-position -1)) (duplicate-line 2))
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 17)))
  ;; Duplicate a non-terminated line.
  (with-temp-buffer
    (insert "abc")
    (goto-char 2)
    (duplicate-line)
    (should (equal (buffer-string) "abc\nabc\n"))
    (should (equal (point) 2))))

(require 'rect)

(ert-deftest misc--duplicate-dwim ()
  ;; Duplicate a line.
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (duplicate-dwim 2)
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 7)))

  ;; Duplicate a region.
  (with-temp-buffer
    (insert "abc\ndef\n")
    (set-mark 2)
    (goto-char 7)
    (transient-mark-mode)
    (should (use-region-p))
    (duplicate-dwim)
    (should (equal (buffer-string) "abc\ndebc\ndef\n"))
    (should (equal (point) 7))
    (should (region-active-p))
    (should (equal (mark) 2)))

  ;; Duplicate a rectangular region.
  (with-temp-buffer
    (insert "x\n>a\n>bcde\n>fg\nyz\n")
    (goto-char 4)
    (rectangle-mark-mode)
    (goto-char 15)
    (rectangle-forward-char 1)
    (duplicate-dwim)
    (should (equal (buffer-string) "x\n>a  a  \n>bcdbcde\n>fg fg \nyz\n"))
    (should (equal (point) 24))
    (should (region-active-p))
    (should rectangle-mark-mode)
    (should (equal (mark) 4))))

(provide 'misc-tests)
;;; misc-tests.el ends here
