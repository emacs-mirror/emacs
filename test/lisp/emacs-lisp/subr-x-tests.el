;;; subr-x-tests.el --- Testing the extended lisp routines  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2026 Free Software Foundation, Inc.

;; Author: Fabián E. Gallina <fgallina@gnu.org>
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

(require 'ert)
(require 'subr-x)


;; `if-let*' tests

(ert-deftest subr-x-test-if-let*-single-binding-expansion ()
  "Test single bindings are expanded properly."
  (should (equal
           (macroexpand
            '(if-let* ((a 1))
                 (- a)
               "no"))
           '(let* ((a (and t 1)))
              (if a
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let* (a)
                 (- a)
               "no"))
           '(let* ((a (and t a)))
              (if a
                  (- a)
                "no")))))

(ert-deftest subr-x-test-if-let*-single-symbol-expansion ()
  "Test single symbol bindings are expanded properly."
  (should (equal
           (macroexpand
            '(if-let* (a)
                 (- a)
               "no"))
           '(let* ((a (and t a)))
              (if a
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let* (a b c)
                 (- a)
               "no"))
           '(let* ((a (and t a))
                   (b (and a b))
                   (c (and b c)))
              (if c
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let* (a (b 2) c)
                 (- a)
               "no"))
           '(let* ((a (and t a))
                   (b (and a 2))
                   (c (and b c)))
              (if c
                  (- a)
                "no")))))

(ert-deftest subr-x-test-if-let*-nil-related-expansion ()
  "Test nil is processed properly."
  (should (equal
           (macroexpand
            '(if-let* (nil)
                 (- a)
               "no"))
           '(let* ((nil (and t nil)))
              (if nil
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let* ((a 1) nil (b 2))
                 (- a)
               "no"))
           '(let* ((a (and t 1))
                   (nil (and a nil))
                   (b (and nil 2)))
              (if b
                  (- a)
                "no")))))

(ert-deftest subr-x-test-if-let*-malformed-binding ()
  "Test malformed bindings trigger errors."
  (should-error (macroexpand
                 '(if-let* (_ (a 1 1) (b 2) (c 3) d)
                      (- a)
                    "no"))
                :type 'error)
  (should-error (macroexpand
                 '(if-let* (_ (a 1) (b 2 2) (c 3) d)
                      (- a)
                    "no"))
                :type 'error)
  (should-error (macroexpand
                 '(if-let* (_ (a 1) (b 2) (c 3 3) d)
                      (- a)
                    "no"))
                :type 'error)
  (should-error (macroexpand
                 '(if-let* ((a 1 1))
                      (- a)
                    "no"))
                :type 'error))

(ert-deftest subr-x-test-if-let*-true ()
  "Test `if-let' with truthy bindings."
  (should (equal
           (if-let* ((a 1))
               a
             "no")
           1))
  (should (equal
           (if-let* ((a 1) (b 2) (c 3))
               (list a b c)
             "no")
           (list 1 2 3))))

(ert-deftest subr-x-test-if-let*-false ()
  "Test `if-let' with falsie bindings."
  (should (equal
           (if-let* ((a nil))
               "yes"
             "no")
           "no"))
  (should (equal
           (if-let* ((a nil) (b 2) (c 3))
               "yes"
             "no")
           "no"))
  (should (equal
           (if-let* ((a 1) (b nil) (c 3))
               "yes"
             "no")
           "no"))
  (should (equal
           (if-let* ((a 1) (b 2) (c nil))
               "yes"
             "no")
           "no"))
  (should (equal
           (let ((z nil))
             (if-let* (z (a 1) (b 2) (c 3))
                 "yes"
               "no"))
           "no"))
  (should (equal
           (let ((d nil))
             (if-let* ((a 1) (b 2) (c 3) d)
                 "yes"
               "no"))
           "no")))

(ert-deftest subr-x-test-if-let*-bound-references ()
  "Test `if-let' bindings can refer to already bound symbols."
  (should (equal
           (if-let* ((a (1+ 0)) (b (1+ a)) (c (1+ b)))
               (list a b c)
             "no")
           (list 1 2 3))))

(ert-deftest subr-x-test-if-let*-and-laziness-is-preserved ()
  "Test `if-let' respects `and' laziness."
  (let ((a-called nil) (b-called nil) c-called)
    (should (equal
             (if-let* ((a nil)
                       (b (setq b-called t))
                       (c (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list nil nil nil))))
  (let ((a-called nil) (b-called nil) c-called)
    (should (equal
             (if-let* ((a (setq a-called t))
                       (b nil)
                       (c (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list t nil nil))))
  (let ((a-called nil) (b-called nil) c-called)
    (should (equal
             (if-let* ((a (setq a-called t))
                       (b (setq b-called t))
                       (c nil)
                       (d (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list t t nil)))))


;; `when-let*' tests

(ert-deftest subr-x-test-when-let*-body-expansion ()
  "Test body allows for multiple sexps wrapping with progn."
  (should (equal
           (macroexpand
            '(when-let* ((a 1))
               (message "opposite")
               (- a)))
           '(let* ((a (and t 1)))
              (if a
                  (progn
                    (message "opposite")
                    (- a)))))))

(ert-deftest subr-x-test-when-let*-single-symbol-expansion ()
  "Test single symbol bindings are expanded properly."
  (should (equal
           (macroexpand
            '(when-let* (a)
               (- a)))
           '(let* ((a (and t a)))
              (if a
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let* (a b c)
               (- a)))
           '(let* ((a (and t a))
                   (b (and a b))
                   (c (and b c)))
              (if c
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let* (a (b 2) c)
               (- a)))
           '(let* ((a (and t a))
                   (b (and a 2))
                   (c (and b c)))
              (if c
                  (- a))))))

(ert-deftest subr-x-test-when-let*-nil-related-expansion ()
  "Test nil is processed properly."
  (should (equal
           (macroexpand
            '(when-let* (nil)
               (- a)))
           '(let* ((nil (and t nil)))
              (if nil
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let* ((a 1) nil (b 2))
               (- a)))
           '(let* ((a (and t 1))
                   (nil (and a nil))
                   (b (and nil 2)))
              (if b
                  (- a))))))

(ert-deftest subr-x-test-when-let*-malformed-binding ()
  "Test malformed bindings trigger errors."
  (should-error (macroexpand
                 '(when-let* (_ (a 1 1) (b 2) (c 3) d)
                    (- a)))
                :type 'error)
  (should-error (macroexpand
                 '(when-let* (_ (a 1) (b 2 2) (c 3) d)
                    (- a)))
                :type 'error)
  (should-error (macroexpand
                 '(when-let* (_ (a 1) (b 2) (c 3 3) d)
                    (- a)))
                :type 'error)
  (should-error (macroexpand
                 '(when-let* ((a 1 1))
                    (- a)))
                :type 'error))

(ert-deftest subr-x-test-when-let*-true ()
  "Test `when-let' with truthy bindings."
  (should (equal
           (when-let* ((a 1))
             a)
           1))
  (should (equal
           (when-let* ((a 1) (b 2) (c 3))
             (list a b c))
           (list 1 2 3))))

(ert-deftest subr-x-test-when-let*-false ()
  "Test `when-let' with falsie bindings."
  (should (equal
           (when-let* ((a nil))
             "no")
           nil))
  (should (equal
           (when-let* ((a nil) (b 2) (c 3))
             "no")
           nil))
  (should (equal
           (when-let* ((a 1) (b nil) (c 3))
             "no")
           nil))
  (should (equal
           (when-let* ((a 1) (b 2) (c nil))
             "no")
           nil))
  (should (equal
           (let ((z nil))
             (when-let* (z (a 1) (b 2) (c 3))
               "no"))
           nil))
  (should (equal
           (let ((d nil))
             (when-let* ((a 1) (b 2) (c 3) d)
               "no"))
           nil)))

(ert-deftest subr-x-test-when-let*-bound-references ()
  "Test `when-let' bindings can refer to already bound symbols."
  (should (equal
           (when-let* ((a (1+ 0)) (b (1+ a)) (c (1+ b)))
             (list a b c))
           (list 1 2 3))))

(ert-deftest subr-x-test-when-let*-and-laziness-is-preserved ()
  "Test `when-let' respects `and' laziness."
  (let ((a-called nil) (b-called nil) (c-called nil))
    (should (equal
             (progn
               (when-let* ((a nil)
                           (b (setq b-called t))
                           (c (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list nil nil nil))))
  (let ((a-called nil) (b-called nil) (c-called nil))
    (should (equal
             (progn
               (when-let* ((a (setq a-called t))
                           (b nil)
                           (c (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list t nil nil))))
  (let ((a-called nil) (b-called nil) (c-called nil))
    (should (equal
             (progn
               (when-let* ((a (setq a-called t))
                           (b (setq b-called t))
                           (c nil)
                           (d (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list t t nil)))))


;; `and-let*' tests

;; Adapted from the Guile tests
;; https://git.savannah.gnu.org/cgit/guile.git/tree/test-suite/tests/srfi-2.test

(ert-deftest subr-x-and-let*-test-empty-varlist ()
  (should (equal 1 (and-let* () 1)))
  (should (equal 2 (and-let* () 1 2)))
  (should (equal t (and-let* ()))))

(ert-deftest subr-x-and-let*-test-group-1 ()
   (should (equal nil (let ((x nil)) (and-let* (x)))))
   (should (equal 1 (let ((x 1)) (and-let* (x)))))
   (should (equal nil (and-let* ((x nil)))))
   (should (equal 1 (and-let* ((x 1)))))
   ;; The error doesn't trigger when compiled: the compiler will give
   ;; a warning and then drop the erroneous code.  Therefore, use
   ;; `eval' to avoid compilation.
   (should-error (eval '(and-let* (nil (x 1))) lexical-binding)
                 :type 'setting-constant)
   (should (equal nil (and-let* ((nil) (x 1)))))
   (should-error (eval '(and-let* (2 (x 1))) lexical-binding)
                 :type 'wrong-type-argument)
   (should (equal 1 (and-let* ((2) (x 1)))))
   (should (equal 2 (and-let* ((x 1) (2)))))
   (should (equal nil (let ((x nil)) (and-let* (x) x))))
   (should (equal "" (let ((x "")) (and-let* (x) x))))
   (should (equal "" (let ((x "")) (and-let* (x)))))
   (should (equal 2 (let ((x 1)) (and-let* (x) (+ x 1)))))
   (should (equal nil (let ((x nil)) (and-let* (x) (+ x 1)))))
   (should (equal 2 (let ((x 1)) (and-let* (((> x 0))) (+ x 1)))))
   (should (equal t (let ((x 1)) (and-let* (((> x 0)))))))
   (should (equal nil (let ((x 0)) (and-let* (((> x 0))) (+ x 1)))))
   (should (equal 3
                  (let ((x 1)) (and-let* (((> x 0)) (x (+ x 1))) (+ x 1))))))

(ert-deftest subr-x-and-let*-test-rebind ()
   (should
    (equal 4
           (let ((x 1))
             (and-let* (((> x 0)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))))

(ert-deftest subr-x-and-let*-test-group-2 ()
   (should
    (equal 2 (let ((x 1)) (and-let* (x ((> x 0))) (+ x 1)))))
   (should
    (equal 2 (let ((x 1)) (and-let* (((progn x)) ((> x 0))) (+ x 1)))))
   (should (equal nil (let ((x 0)) (and-let* (x ((> x 0))) (+ x 1)))))
   (should (equal nil (let ((x nil)) (and-let* (x ((> x 0))) (+ x 1)))))
   (should
    (equal nil (let ((x nil)) (and-let* (((progn x)) ((> x 0))) (+ x 1))))))

(ert-deftest subr-x-and-let*-test-group-3 ()
   (should
    (equal nil (let ((x 1)) (and-let* (x (y (- x 1)) ((> y 0))) (/ x y)))))
   (should
    (equal nil (let ((x 0)) (and-let* (x (y (- x 1)) ((> y 0))) (/ x y)))))
   (should
    (equal nil
           (let ((x nil)) (and-let* (x (y (- x 1)) ((> y 0))) (/ x y)))))
   (should
    (equal (/ 3.0 2)
           (let ((x 3.0)) (and-let* (x (y (- x 1)) ((> y 0))) (/ x y))))))



;; Thread first tests

(ert-deftest subr-x-test-thread-first-no-forms ()
  "Test `thread-first' with no forms expands to the first form."
  (should (equal (macroexpand '(thread-first 5)) 5))
  (should (equal (macroexpand '(thread-first (+ 1 2))) '(+ 1 2))))

(ert-deftest subr-x-test-thread-first-function-names-are-threaded ()
  "Test `thread-first' wraps single function names."
  (should (equal (macroexpand
                  '(thread-first 5
                                 -))
                 '(- 5)))
  (should (equal (macroexpand
                  '(thread-first (+ 1 2)
                                 -))
                 '(- (+ 1 2)))))

(ert-deftest subr-x-test-thread-first-expansion ()
  "Test `thread-first' expands correctly."
  (should (equal
           (macroexpand '(thread-first
                           5
                           (+ 20)
                           (/ 25)
                           -
                           (+ 40)))
           '(+ (- (/ (+ 5 20) 25)) 40))))

(ert-deftest subr-x-test-thread-first-examples ()
  "Test several `thread-first' examples."
  (should (equal (thread-first (+ 40 2)) 42))
  (should (equal (thread-first
                   5
                   (+ 20)
                   (/ 25)
                   -
                   (+ 40)) 39))
  (should (equal (thread-first
                   "this-is-a-string"
                   (split-string "-")
                   (nbutlast 2)
                   (append (list "good")))
                 (list "this" "is" "good"))))

;; Thread last tests

(ert-deftest subr-x-test-thread-last-no-forms ()
  "Test `thread-last' with no forms expands to the first form."
  (should (equal (macroexpand '(thread-last 5)) 5))
  (should (equal (macroexpand '(thread-last (+ 1 2))) '(+ 1 2))))

(ert-deftest subr-x-test-thread-last-function-names-are-threaded ()
  "Test `thread-last' wraps single function names."
  (should (equal (macroexpand
                  '(thread-last 5
                                -))
                 '(- 5)))
  (should (equal (macroexpand
                  '(thread-last (+ 1 2)
                                -))
                 '(- (+ 1 2)))))

(ert-deftest subr-x-test-thread-last-expansion ()
  "Test `thread-last' expands correctly."
  (should (equal
           (macroexpand '(thread-last
                           5
                           (+ 20)
                           (/ 25)
                           -
                           (+ 40)))
           '(+ 40 (- (/ 25 (+ 20 5)))))))

(ert-deftest subr-x-test-thread-last-examples ()
  "Test several `thread-last' examples."
  (should (equal (thread-last (+ 40 2)) 42))
  (should (equal (thread-last
                   5
                   (+ 20)
                   (/ 25)
                   -
                   (+ 40)) 39))
  (should (equal (thread-last
                   (list 1 -2 3 -4 5)
                   (mapcar #'abs)
                   (cl-reduce #'+)
                   (format "abs sum is: %s"))
                 "abs sum is: 15")))


;; Substring tests

(ert-deftest subr-x-test-string-remove-prefix ()
  "Test `string-remove-prefix' behavior."
  (should (equal (string-remove-prefix "" "") ""))
  (should (equal (string-remove-prefix "" "a") "a"))
  (should (equal (string-remove-prefix "a" "") ""))
  (should (equal (string-remove-prefix "a" "b") "b"))
  (should (equal (string-remove-prefix "a" "a") ""))
  (should (equal (string-remove-prefix "a" "aa") "a"))
  (should (equal (string-remove-prefix "a" "ab") "b")))

(ert-deftest subr-x-test-string-remove-suffix ()
  "Test `string-remove-suffix' behavior."
  (should (equal (string-remove-suffix "" "") ""))
  (should (equal (string-remove-suffix "" "a") "a"))
  (should (equal (string-remove-suffix "a" "") ""))
  (should (equal (string-remove-suffix "a" "b") "b"))
  (should (equal (string-remove-suffix "a" "a") ""))
  (should (equal (string-remove-suffix "a" "aa") "a"))
  (should (equal (string-remove-suffix "a" "ba") "b")))

(ert-deftest subr-clean-whitespace ()
  (should (equal (string-clean-whitespace " foo ") "foo"))
  (should (equal (string-clean-whitespace " foo   \r\n\t  Bar") "foo Bar")))

(ert-deftest subr-string-fill ()
  (should (equal (string-fill "foo" 10) "foo"))
  (should (equal (string-fill "foobar" 5) "foobar"))
  (should (equal (string-fill "foo bar zot" 5) "foo\nbar\nzot"))
  (should (equal (string-fill "foo bar zot" 7) "foo bar\nzot")))

(ert-deftest subr-string-limit ()
  (should (equal (string-limit "foo" 10) "foo"))
  (should (equal (string-limit "foo" 2) "fo"))
  (should (equal (string-limit "foo" 2 t) "oo"))
  (should (equal (string-limit "abc" 10 t) "abc"))
  (should (equal (string-limit "foo" 0) ""))
  (should-error (string-limit "foo" -1)))

(ert-deftest subr-string-limit-coding ()
  (should (not (multibyte-string-p (string-limit "foó" 10 nil 'utf-8))))
  (should (equal (string-limit "foó" 10 nil 'utf-8) "fo\303\263"))
  (should (equal (string-limit "foó" 3 nil 'utf-8) "fo"))
  (should (equal (string-limit "foó" 4 nil 'utf-8) "fo\303\263"))
  (should (equal (string-limit "foóa" 4 nil 'utf-8) "fo\303\263"))
  (should (equal (string-limit "foóá" 4 nil 'utf-8) "fo\303\263"))
  (should (equal (string-limit "foóá" 2 nil 'utf-8-with-signature)
                 ""))
  (should (equal (string-limit "foóá" 4 nil 'utf-8-with-signature)
                 "\357\273\277f"))
  (should (equal (string-limit "foóa" 4 nil 'iso-8859-1) "fo\363a"))
  (should (equal (string-limit "foóá" 4 nil 'iso-8859-1) "fo\363\341"))
  (should (equal (string-limit "foóá" 3 nil 'utf-16) ""))
  (should (equal (string-limit "foóá" 6 nil 'utf-16) "\376\377\000f\000o"))

  (should (equal (string-limit "foó" 10 t 'utf-8) "fo\303\263"))
  (should (equal (string-limit "foó" 3 t 'utf-8) "o\303\263"))
  (should (equal (string-limit "foó" 4 t 'utf-8) "fo\303\263"))
  (should (equal (string-limit "foóa" 4 t 'utf-8) "o\303\263a"))
  (should (equal (string-limit "foóá" 4 t 'utf-8) "\303\263\303\241"))
  (should (equal (string-limit "foóá" 2 t 'utf-8-with-signature)
                 ""))
  (should (equal (string-limit "foóa" 4 t 'iso-8859-1) "fo\363a"))
  (should (equal (string-limit "foóá" 4 t 'iso-8859-1) "fo\363\341"))
  (should (equal (string-limit "foóá" 6 t 'utf-16) "\376\377\000\363\000\341")))

(ert-deftest subr-string-limit-glyphs ()
  (should (equal (encode-coding-string "Hello, 👼🏻🧑🏼‍🤝‍🧑🏻" 'utf-8)
                 "Hello, \360\237\221\274\360\237\217\273\360\237\247\221\360\237\217\274\342\200\215\360\237\244\235\342\200\215\360\237\247\221\360\237\217\273"))
  (should (= (length (encode-coding-string "Hello, 👼🏻🧑🏼‍🤝‍🧑🏻" 'utf-8)) 41))
  (should (equal (string-limit "Hello, 👼🏻🧑🏼‍🤝‍🧑🏻" 100 nil 'utf-8)
                 "Hello, \360\237\221\274\360\237\217\273\360\237\247\221\360\237\217\274\342\200\215\360\237\244\235\342\200\215\360\237\247\221\360\237\217\273"))
  (should (equal (string-limit "Hello, 👼🏻🧑🏼‍🤝‍🧑🏻" 15 nil 'utf-8)
                 "Hello, \360\237\221\274\360\237\217\273"))
  (should (equal (string-limit "Hello, 👼🏻🧑🏼‍🤝‍🧑🏻" 10 nil 'utf-8)
                 "Hello, ")))

(ert-deftest subr-string-lines ()
  (should (equal (string-lines "foo") '("foo")))
  (should (equal (string-lines "foo \nbar") '("foo " "bar"))))

(ert-deftest subr-string-pad ()
  (should (equal (string-pad "foo" 5) "foo  "))
  (should (equal (string-pad "foo" 5 ?-) "foo--"))
  (should (equal (string-pad "foo" 5 ?- t) "--foo"))
  (should (equal (string-pad "foo" 2 ?-) "foo")))

(ert-deftest subr-string-chop-newline ()
  (should (equal (string-chop-newline "foo\n") "foo"))
  (should (equal (string-chop-newline "foo\nbar\n") "foo\nbar"))
  (should (equal (string-chop-newline "foo\nbar") "foo\nbar")))

(ert-deftest subr-ensure-empty-lines ()
  (should
   (equal
    (with-temp-buffer
      (insert "foo")
      (goto-char (point-min))
      (ensure-empty-lines 2)
      (buffer-string))
    "\n\nfoo"))
  (should
   (equal
    (with-temp-buffer
      (insert "foo")
      (ensure-empty-lines 2)
      (buffer-string))
    "foo\n\n\n"))
  (should
   (equal
    (with-temp-buffer
      (insert "foo\n")
      (ensure-empty-lines 2)
      (buffer-string))
    "foo\n\n\n"))
  (should
   (equal
    (with-temp-buffer
      (insert "foo\n\n\n\n\n")
      (ensure-empty-lines 2)
      (buffer-string))
    "foo\n\n\n"))
  (should
   (equal
    (with-temp-buffer
      (insert "foo\n\n\n")
      (ensure-empty-lines 0)
      (buffer-string))
    "foo\n")))

(ert-deftest subr-x-test-add-display-text-property ()
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (add-display-text-property 4 8 'height 2.0)
    (add-display-text-property 2 12 'raise 0.5)
    (add-display-text-property 6 10 'height 1.0)
    (should (equal-including-properties
             (buffer-string)
             #("Foo bar zot gazonk"
               1 3 (display (raise 0.5))
               3 5 (display ((raise 0.5) (height 2.0)))
               5 9 (display ((height 1.0) (raise 0.5)))
               9 11 (display (raise 0.5))))))
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (put-text-property 4 8 'display [(height 2.0)])
    (add-display-text-property 2 12 'raise 0.5)
    (add-display-text-property 6 10 'height 1.0)
    (should (equal-including-properties
             (buffer-string)
             #("Foo bar zot gazonk"
               1 3 (display (raise 0.5))
               3 5 (display [(raise 0.5) (height 2.0)])
               5 7 (display [(height 1.0) (raise 0.5)])
               7 9 (display ((height 1.0) (raise 0.5)))
               9 11 (display (raise 0.5))))))
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (add-display-text-property 4 8 '(margin nil) "Hi")
    (add-display-text-property 2 12 'raise 0.5)
    (add-display-text-property 6 10 '(margin nil) "Bye")
    (should (equal-including-properties
             (buffer-string)
             #("Foo bar zot gazonk"
               1 3 (display (raise 0.5))
               3 5 (display ((raise 0.5) ((margin nil) "Hi")))
               5 9 (display (((margin nil) "Bye") (raise 0.5)))
               9 11 (display (raise 0.5))))))
  (with-temp-buffer
    (should (equal-including-properties
             (let ((str (copy-sequence "some useless string")))
               (add-display-text-property 4 8 'height 2.0 str)
               (add-display-text-property 2 12 'raise 0.5 str)
               str)
             #("some useless string"
               2 4 (display (raise 0.5))
               4 8 (display ((raise 0.5) (height 2.0)))
               8 12 (display (raise 0.5)))))))

(ert-deftest subr-x-test-remove-display-text-property ()
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (add-display-text-property 4 12 'height 2.0)
    (add-display-text-property 2 8 'raise 0.5)
    (remove-display-text-property 6 10 'height)
    (should (equal-including-properties
             (buffer-string)
             #("Foo bar zot gazonk"
               1 3 (display (raise 0.5))
               3 5 (display ((raise 0.5) (height 2.0)))
               5 7 (display ((raise 0.5)))
               9 11 (display (height 2.0))))))
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (put-text-property 4 12 'display [(height 2.0)])
    (add-display-text-property 2 8 'raise 0.5)
    (remove-display-text-property 6 10 'height)
    (should (equal-including-properties
             (buffer-string)
             #("Foo bar zot gazonk"
               1 3 (display (raise 0.5))
               3 5 (display [(raise 0.5) (height 2.0)])
               5 7 (display [(raise 0.5)])
               9 11 (display [(height 2.0)])))))
  (with-temp-buffer
    (should (equal-including-properties
             (let ((str (copy-sequence "Foo bar zot gazonk")))
               (add-display-text-property 3 11 'height 2.0 str)
               (add-display-text-property 1 7 'raise 0.5 str)
               (remove-display-text-property 5 9 'height str)
               str)
             #("Foo bar zot gazonk"
               1 3 (display (raise 0.5))
               3 5 (display ((raise 0.5) (height 2.0)))
               5 7 (display ((raise 0.5)))
               9 11 (display (height 2.0)))))))

(ert-deftest subr-x-named-let ()
  (let ((funs ()))
    (named-let loop
        ((rest '(1 42 3))
         (sum 0))
      (when rest
        ;; Here, we make sure that the variables are distinct in every
        ;; iteration, since a naive tail-call optimization would tend to end up
        ;; with a single `sum' variable being shared by all the closures.
        (push (lambda () sum) funs)
        ;; Here we add a dummy `sum' variable which shadows the `sum' iteration
        ;; variable since a naive tail-call optimization could also trip here
        ;; thinking it can `(setq sum ...)' to set the iteration
        ;; variable's value.
        (let ((sum sum))
          (loop (cdr rest) (+ sum (car rest))))))
    (should (equal (mapcar #'funcall funs) '(43 1 0)))))

(ert-deftest test-with-buffer-unmodified-if-unchanged ()
  (with-temp-buffer
    (with-buffer-unmodified-if-unchanged
      (insert "t"))
    (should (buffer-modified-p)))

  (with-temp-buffer
    (with-buffer-unmodified-if-unchanged
      (insert "t")
      (delete-char -1))
    (should-not (buffer-modified-p)))

  ;; Shouldn't error.
  (should
   (with-temp-buffer
     (with-buffer-unmodified-if-unchanged
       (insert "t")
       (delete-char -1)
       (kill-buffer))))

  (with-temp-buffer
    (let ((outer (current-buffer)))
      (with-temp-buffer
        (let ((inner (current-buffer)))
          (with-buffer-unmodified-if-unchanged
            (insert "t")
            (delete-char -1)
            (set-buffer outer))
          (with-current-buffer inner
            (should-not (buffer-modified-p))))))))

(ert-deftest subr-x--hash-table-keys-and-values ()
  (let ((h (make-hash-table)))
    (puthash 'a 1 h)
    (puthash 'c 3 h)
    (puthash 'b 2 h)
    (should (equal (sort (hash-table-keys h) #'string<) '(a b c)))
    (should (equal (sort (hash-table-values h) #'<) '(1 2 3)))))

(ert-deftest test-string-truncate-left ()
  (should (equal (string-truncate-left "band" 3) "...d"))
  (should (equal (string-truncate-left "band" 2) "...d"))
  (should (equal (string-truncate-left "longstring" 8) "...tring")))

(provide 'subr-x-tests)
;;; subr-x-tests.el ends here
