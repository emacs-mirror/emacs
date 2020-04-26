;;; misc-tests.el --- Tests for misc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
    (zap-up-to-char 2 ?c)))

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

(provide 'misc-tests)
;;; misc-tests.el ends here
