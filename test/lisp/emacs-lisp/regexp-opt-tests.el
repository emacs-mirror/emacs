;;; regexp-opt-tests.el --- Tests for regexp-opt.el  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2024 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:       internal
;; Human-Keywords: internal

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

(require 'regexp-opt)

(defun regexp-opt-test--permutations (l)
  "All permutations of L, assuming no duplicates."
  (if (cdr l)
      (mapcan (lambda (x)
                (mapcar (lambda (p) (cons x p))
                        (regexp-opt-test--permutations (remove x l))))
              l)
    (list l)))

(ert-deftest regexp-opt-longest-match ()
  "Check that the regexp always matches as much as possible."
  (let ((s "abcd"))
    (dolist (perm (regexp-opt-test--permutations '("a" "ab" "ac" "abc")))
      (should (equal (and (string-match (regexp-opt perm) s)
                          (match-string 0 s))
                     "abc")))))

(ert-deftest regexp-opt-charset ()
  (should (equal (regexp-opt-charset '(?a ?b ?a)) "[ab]"))
  (should (equal (regexp-opt-charset '(?D ?d ?B ?a ?b ?C ?7 ?a ?c ?A))
                 "[7A-Da-d]"))
  (should (equal (regexp-opt-charset '(?a)) "a"))

  (should (equal (regexp-opt-charset '(?^)) "\\^"))
  (should (equal (regexp-opt-charset '(?-)) "-"))
  (should (equal (regexp-opt-charset '(?\])) "]"))
  (should (equal (regexp-opt-charset '(?^ ?\])) "[]^]"))
  (should (equal (regexp-opt-charset '(?^ ?-)) "[-^]"))
  (should (equal (regexp-opt-charset '(?- ?\])) "[]-]"))
  (should (equal (regexp-opt-charset '(?- ?\] ?^)) "[]^-]"))

  (should (equal (regexp-opt-charset '(?^ ?a)) "[a^]"))
  (should (equal (regexp-opt-charset '(?- ?a)) "[a-]"))
  (should (equal (regexp-opt-charset '(?\] ?a)) "[]a]"))
  (should (equal (regexp-opt-charset '(?^ ?\] ?a)) "[]a^]"))
  (should (equal (regexp-opt-charset '(?^ ?- ?a)) "[a^-]"))
  (should (equal (regexp-opt-charset '(?- ?\] ?a)) "[]a-]"))
  (should (equal (regexp-opt-charset '(?- ?\] ?^ ?a)) "[]a^-]"))

  (should (equal (regexp-opt-charset '()) regexp-unmatchable)))

;;; regexp-opt-tests.el ends here
