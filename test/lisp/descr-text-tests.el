;;; descr-text-test.el --- ERT tests for descr-text.el -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2016-2017 Free Software Foundation, Inc.

;; Author:     Michal Nazarewicz <mina86@mina86.com>

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

;; This package defines regression tests for the descr-text package.

;;; Code:

(require 'ert)
(require 'descr-text)


(ert-deftest descr-text-test-truncate ()
  "Tests describe-char-eldoc--truncate function."
  (should (equal ""
                 (describe-char-eldoc--truncate " \t \n" 100)))
  (should (equal "foo"
                 (describe-char-eldoc--truncate "foo" 1)))
  (should (equal "foo..."
                 (describe-char-eldoc--truncate "foo wilma fred" 0)))
  (should (equal "foo..."
                 (describe-char-eldoc--truncate
                  "foo wilma fred" (length "foo wilma"))))
  (should (equal "foo wilma..."
                 (describe-char-eldoc--truncate
                  "foo wilma fred" (+ 3 (length "foo wilma")))))
  (should (equal "foo wilma..."
                 (describe-char-eldoc--truncate
                  "foo wilma fred" (1- (length "foo wilma fred")))))
  (should (equal "foo wilma fred"
                 (describe-char-eldoc--truncate
                  "foo wilma fred" (length "foo wilma fred"))))
  (should (equal "foo wilma fred"
                 (describe-char-eldoc--truncate
                  "  foo\t wilma \nfred\t " (length "foo wilma fred")))))

(ert-deftest descr-text-test-format-desc ()
  "Tests describe-char-eldoc--format function."
  (should (equal "U+2026: Horizontal ellipsis (Po: Punctuation, Other)"
                 (describe-char-eldoc--format ?…)))
  (should (equal "U+2026: Horizontal ellipsis (Punctuation, Other)"
                 (describe-char-eldoc--format ?… 51)))
  (should (equal "U+2026: Horizontal ellipsis (Po)"
                 (describe-char-eldoc--format ?… 40)))
  (should (equal "Horizontal ellipsis (Po)"
                 (describe-char-eldoc--format ?… 30)))
  (should (equal "Horizontal ellipsis"
                 (describe-char-eldoc--format ?… 20)))
  (should (equal "Horizontal..."
                 (describe-char-eldoc--format ?… 10))))

(ert-deftest descr-text-test-desc ()
  "Tests describe-char-eldoc function."
  (with-temp-buffer
    (insert "a…")
    (goto-char (point-min))
    (should (eq ?a (following-char))) ; make sure we are where we think we are
    ;; Function should return nil for an ASCII character.
    (should (not (describe-char-eldoc)))

    (goto-char (1+ (point)))
    (should (eq ?… (following-char)))
    (let ((eldoc-echo-area-use-multiline-p t))
      ;; Function should return description of an Unicode character.
      (should (equal "U+2026: Horizontal ellipsis (Po: Punctuation, Other)"
                     (describe-char-eldoc))))

    (goto-char (point-max))
    ;; At the end of the buffer, function should return nil and not blow up.
    (should (not (describe-char-eldoc)))))


(provide 'descr-text-test)

;;; descr-text-test.el ends here
