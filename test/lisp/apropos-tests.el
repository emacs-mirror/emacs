;;; apropos-tests.el --- Tests for apropos.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'apropos)
(require 'ert)

(ert-deftest apropos-tests-words-to-regexp-1 ()
  (let ((re (apropos-words-to-regexp '("foo" "bar") "baz")))
    (should (string-match-p re "foobazbar"))
    (should (string-match-p re "barbazfoo"))
    (should-not (string-match-p re "foo-bar"))
    (should-not (string-match-p re "foobazbazbar"))))

(ert-deftest apropos-tests-words-to-regexp-2 ()
  (let ((re (apropos-words-to-regexp '("foo" "bar" "baz") "-")))
    (should-not (string-match-p re "foo"))
    (should-not (string-match-p re "foobar"))
    (should (string-match-p re "foo-bar"))
    (should (string-match-p re "foo-baz"))))

(ert-deftest apropos-tests-parse-pattern-1 ()
  (apropos-parse-pattern '("foo"))
  (should (string-match-p apropos-regexp "foo"))
  (should (string-match-p apropos-regexp "foo-bar"))
  (should (string-match-p apropos-regexp "bar-foo"))
  (should (string-match-p apropos-regexp "foo-foo"))
  (should-not (string-match-p apropos-regexp "bar")))

(ert-deftest apropos-tests-parse-pattern-2 ()
  (apropos-parse-pattern '("foo" "bar"))
  (should (string-match-p apropos-regexp "foo-bar"))
  (should (string-match-p apropos-regexp "bar-foo"))
  (should-not (string-match-p apropos-regexp "foo"))
  (should-not (string-match-p apropos-regexp "bar"))
  (should-not (string-match-p apropos-regexp "baz"))
  (should-not (string-match-p apropos-regexp "foo-foo"))
  (should-not (string-match-p apropos-regexp "bar-bar")))

(ert-deftest apropos-tests-parse-pattern-3 ()
  (apropos-parse-pattern '("foo" "bar" "baz"))
  (should (string-match-p apropos-regexp "foo-bar"))
  (should (string-match-p apropos-regexp "foo-baz"))
  (should (string-match-p apropos-regexp "bar-foo"))
  (should (string-match-p apropos-regexp "bar-baz"))
  (should (string-match-p apropos-regexp "baz-foo"))
  (should (string-match-p apropos-regexp "baz-bar"))
  (should-not (string-match-p apropos-regexp "foo"))
  (should-not (string-match-p apropos-regexp "bar"))
  (should-not (string-match-p apropos-regexp "baz"))
  (should-not (string-match-p apropos-regexp "foo-foo"))
  (should-not (string-match-p apropos-regexp "bar-bar"))
  (should-not (string-match-p apropos-regexp "baz-baz")))

(ert-deftest apropos-tests-parse-pattern-single-regexp ()
  (apropos-parse-pattern "foo+bar")
  (should-not (string-match-p apropos-regexp "fobar"))
  (should (string-match-p apropos-regexp "foobar"))
  (should (string-match-p apropos-regexp "fooobar")))

(ert-deftest apropos-tests-parse-pattern-synonyms ()
  (let ((apropos-synonyms '(("find" "open" "edit"))))
    (apropos-parse-pattern '("open"))
    (should (string-match-p apropos-regexp "find-file"))
    (should (string-match-p apropos-regexp "open-file"))
    (should (string-match-p apropos-regexp "edit-file"))))

(ert-deftest apropos-tests-calc-scores ()
  (let ((str "Return apropos score for string STR."))
    (should (equal (apropos-calc-scores str '("apr")) '(7)))
    (should (equal (apropos-calc-scores str '("apr" "str")) '(25 7)))
    (should (equal (apropos-calc-scores str '("appr" "str")) '(25)))
    (should-not (apropos-calc-scores str '("appr" "strr")))))

(ert-deftest apropos-tests-score-str ()
  (apropos-parse-pattern '("foo" "bar"))
  (should (< (apropos-score-str "baz")
             (apropos-score-str "foo baz")
             (apropos-score-str "foo bar baz"))))

(ert-deftest apropos-tests-score-doc ()
  (apropos-parse-pattern '("foo" "bar"))
  (should (< (apropos-score-doc "baz")
             (apropos-score-doc "foo baz")
             (apropos-score-doc "foo bar baz"))))

(ert-deftest apropos-tests-score-symbol ()
  (apropos-parse-pattern '("foo" "bar"))
  (should (< (apropos-score-symbol 'baz)
             (apropos-score-symbol 'foo-baz)
             (apropos-score-symbol 'foo-bar-baz))))

(ert-deftest apropos-tests-true-hit ()
  (should-not (apropos-true-hit "foo" '("foo" "bar")))
  (should (apropos-true-hit "foo bar" '("foo" "bar")))
  (should (apropos-true-hit "foo bar baz" '("foo" "bar"))))

(ert-deftest apropos-tests-format-plist ()
  (let ((foo (make-symbol "foo")))
    (setplist foo '(a 1 b (2 3) c nil))
    (apropos-parse-pattern '("b"))
    (should (equal (apropos-format-plist foo ", ")
                   "a 1, b (2 3), c nil"))
    (should (equal (apropos-format-plist foo ", " t)
                   "b (2 3)"))
    (apropos-parse-pattern '("d"))
    (should-not (apropos-format-plist foo ", " t))))

(provide 'apropos-tests)
;;; apropos-tests.el ends here
