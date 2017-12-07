;;; let-alist.el --- tests for file handling. -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

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

(require 'ert)
(require 'cl-lib)
(require 'let-alist)

(ert-deftest let-alist-surface-test ()
  "Tests basic macro expansion for `let-alist'."
  (should
   (equal '(let ((symbol data))
             (let ((.test-one (cdr (assq 'test-one symbol)))
                   (.test-two (cdr (assq 'test-two symbol))))
               (list .test-one .test-two
                     .test-two .test-two)))
          (cl-letf (((symbol-function #'make-symbol) (lambda (_x) 'symbol)))
            (macroexpand
             '(let-alist data (list .test-one .test-two
                                    .test-two .test-two))))))
  (should
   (equal
    (let ((.external "ext")
          (.external.too "et"))
      (let-alist '((test-two . 0)
                   (test-three . 1)
                   (sublist . ((foo . 2)
                               (bar . 3))))
        (list .test-one .test-two .test-three
              .sublist.foo .sublist.bar
              ..external ..external.too)))
    (list nil 0 1 2 3 "ext" "et"))))

(ert-deftest let-alist-cons ()
  (should
   (equal
    (let ((.external "ext"))
      (let-alist '((test-two . 0)
                   (test-three . 1)
                   (sublist . ((foo . 2)
                               (bar . 3))))
        (list `(, .test-one . , .test-two)
              .sublist.bar ..external)))
    (list '(nil . 0) 3 "ext"))))

(defvar let-alist--test-counter 0
  "Used to count number of times a function is called.")

(ert-deftest let-alist-evaluate-once ()
  "Check that the alist argument is only evaluated once."
  (let ((let-alist--test-counter 0))
    (should
     (equal
      (let-alist (list
                  (cons 'test-two (cl-incf let-alist--test-counter))
                  (cons 'test-three (cl-incf let-alist--test-counter)))
        (list .test-one .test-two .test-two .test-three .cl-incf))
      '(nil 1 1 2 nil)))))

(ert-deftest let-alist-remove-dot ()
  "Remove first dot from symbol."
  (should (equal (let-alist--remove-dot 'hi) 'hi))
  (should (equal (let-alist--remove-dot '.hi) 'hi))
  (should (equal (let-alist--remove-dot '..hi) '.hi)))

(ert-deftest let-alist-list-to-sexp ()
  "Check that multiple dots are handled correctly."
  (should (= 1 (eval (let-alist--list-to-sexp '(a b c d) ''((d (c (b (a . 1)))))))))
  (should (equal (let-alist--access-sexp '.foo.bar.baz 'var)
                 '(cdr (assq 'baz (cdr (assq 'bar (cdr (assq 'foo var))))))))
  (should (equal (let-alist--access-sexp '..foo.bar.baz 'var) '.foo.bar.baz)))

(ert-deftest let-alist--deep-dot-search--nested ()
  "Check that nested `let-alist' forms don't generate spurious bindings.
See Bug#24641."
  (should (equal (let-alist--deep-dot-search '(foo .bar (baz .qux)))
                 '((.bar . bar) (.qux . qux))))
  (should (equal (let-alist--deep-dot-search '(foo .bar (let-alist .qux .baz)))
                 '((.bar . bar) (.qux . qux)))))  ; no .baz

;;; let-alist.el ends here
