;;; subr-x-tests.el --- Testing the extended lisp routines

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Fabián E. Gallina <fgallina@gnu.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
           (let (z)
             (if-let* (z (a 1) (b 2) (c 3))
                 "yes"
               "no"))
           "no"))
  (should (equal
           (let (d)
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
  (let (a-called b-called c-called)
    (should (equal
             (if-let* ((a nil)
                       (b (setq b-called t))
                       (c (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list nil nil nil))))
  (let (a-called b-called c-called)
    (should (equal
             (if-let* ((a (setq a-called t))
                       (b nil)
                       (c (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list t nil nil))))
  (let (a-called b-called c-called)
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
           (let (z)
             (when-let* (z (a 1) (b 2) (c 3))
               "no"))
           nil))
  (should (equal
           (let (d)
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
  (let (a-called b-called c-called)
    (should (equal
             (progn
               (when-let* ((a nil)
                           (b (setq b-called t))
                           (c (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list nil nil nil))))
  (let (a-called b-called c-called)
    (should (equal
             (progn
               (when-let* ((a (setq a-called t))
                           (b nil)
                           (c (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list t nil nil))))
  (let (a-called b-called c-called)
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

(ert-deftest subr-x-test-string-trim-left ()
  "Test `string-trim-left' behavior."
  (should (equal (string-trim-left "") ""))
  (should (equal (string-trim-left " \t\n\r") ""))
  (should (equal (string-trim-left " \t\n\ra") "a"))
  (should (equal (string-trim-left "a \t\n\r") "a \t\n\r"))
  (should (equal (string-trim-left "" "") ""))
  (should (equal (string-trim-left "a" "") "a"))
  (should (equal (string-trim-left "aa" "a*") ""))
  (should (equal (string-trim-left "ba" "a*") "ba"))
  (should (equal (string-trim-left "aa" "a*?") "aa"))
  (should (equal (string-trim-left "aa" "a+?") "a")))

(ert-deftest subr-x-test-string-trim-right ()
  "Test `string-trim-right' behavior."
  (should (equal (string-trim-right "") ""))
  (should (equal (string-trim-right " \t\n\r") ""))
  (should (equal (string-trim-right " \t\n\ra") " \t\n\ra"))
  (should (equal (string-trim-right "a \t\n\r") "a"))
  (should (equal (string-trim-right "" "") ""))
  (should (equal (string-trim-right "a" "") "a"))
  (should (equal (string-trim-right "aa" "a*") ""))
  (should (equal (string-trim-right "ab" "a*") "ab"))
  (should (equal (string-trim-right "aa" "a*?") "")))

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

(provide 'subr-x-tests)
;;; subr-x-tests.el ends here
