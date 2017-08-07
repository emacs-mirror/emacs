;;; which-key-tests.el --- Tests for which-key.el -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: Justin Burkett <justin@burkett.cc>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for which-key.el

;;; Code:

(require 'which-key)
(require 'ert)

(ert-deftest which-key-test-prefix-declaration ()
  "Test `which-key-declare-prefixes' and
`which-key-declare-prefixes-for-mode'. See Bug #109."
  (let* ((major-mode 'test-mode)
         which-key-replacement-alist)
    (which-key-add-key-based-replacements
      "SPC C-c" '("complete" . "complete title")
      "SPC C-k" "cancel")
    (which-key-add-major-mode-key-based-replacements 'test-mode
      "C-c C-c" '("complete" . "complete title")
      "C-c C-k" "cancel")
    (should (equal
             (which-key--maybe-replace '("SPC C-k" . ""))
             '("SPC C-k" . "cancel")))
    (should (equal
             (which-key--maybe-replace '("C-c C-c" . ""))
             '("C-c C-c" . "complete")))))

(ert-deftest which-key-test--maybe-replace ()
  "Test `which-key--maybe-replace'. See #154"
  (let ((which-key-replacement-alist
         '((("C-c [a-d]" . nil) . ("C-c a" . "c-c a"))
           (("C-c .+" . nil) . ("C-c *" . "c-c *"))))
        (test-mode-1 t)
        (test-mode-2 nil)
        which-key-allow-multiple-replacements)
    (which-key-add-key-based-replacements
      "C-c ." "test ."
      "SPC ." "SPC ."
      "C-c \\" "regexp quoting"
      "C-c [" "bad regexp"
      "SPC t1" (lambda (kb)
                 (cons (car kb)
                       (if test-mode-1
                           "[x] test mode"
                         "[ ] test mode")))
      "SPC t2" (lambda (kb)
                 (cons (car kb)
                       (if test-mode-2
                           "[x] test mode"
                         "[ ] test mode"))))
    (should (equal
             (which-key--maybe-replace '("C-c g" . "test"))
             '("C-c *" . "c-c *")))
    (should (equal
             (which-key--maybe-replace '("C-c b" . "test"))
             '("C-c a" . "c-c a")))
    (should (equal
             (which-key--maybe-replace '("C-c ." . "not test ."))
             '("C-c ." . "test .")))
    (should (not
             (equal
              (which-key--maybe-replace '("C-c +" . "not test ."))
              '("C-c ." . "test ."))))
    (should (equal
             (which-key--maybe-replace '("C-c [" . "orig bad regexp"))
             '("C-c [" . "bad regexp")))
    (should (equal
             (which-key--maybe-replace '("C-c \\" . "pre quoting"))
             '("C-c \\" . "regexp quoting")))
    ;; see #155
    (should (equal
             (which-key--maybe-replace '("SPC . ." . "don't replace"))
             '("SPC . ." . "don't replace")))
    (should (equal
             (which-key--maybe-replace '("SPC t 1" . "test mode"))
             '("SPC t 1" . "[x] test mode")))
    (should (equal
             (which-key--maybe-replace '("SPC t 2" . "test mode"))
             '("SPC t 2" . "[ ] test mode")))))

(ert-deftest which-key-test--maybe-replace-multiple ()
  "Test `which-key-allow-multiple-replacements'. See #156."
  (let ((which-key-replacement-alist
         '(((nil . "helm") . (nil . "HLM"))
           ((nil . "projectile") . (nil . "PRJTL"))))
        (which-key-allow-multiple-replacements t))
    (should (equal
             (which-key--maybe-replace '("C-c C-c" . "helm-x"))
             '("C-c C-c" . "HLM-x")))
    (should (equal
             (which-key--maybe-replace '("C-c C-c" . "projectile-x"))
             '("C-c C-c" . "PRJTL-x")))
    (should (equal
             (which-key--maybe-replace '("C-c C-c" . "helm-projectile-x"))
             '("C-c C-c" . "HLM-PRJTL-x")))))

(ert-deftest which-key-test--key-extraction ()
  "Test `which-key--extract-key'. See #161."
  (should (equal (which-key--extract-key "SPC a") "a"))
  (should (equal (which-key--extract-key "C-x a") "a"))
  (should (equal (which-key--extract-key "<left> b a") "a"))
  (should (equal (which-key--extract-key "<left> a .. c") "a .. c"))
  (should (equal (which-key--extract-key "M-a a .. c") "a .. c")))

(provide 'which-key-tests)
;;; which-key-tests.el ends here
