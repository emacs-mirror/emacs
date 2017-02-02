;;; subr-x-tests.el --- Testing the extended lisp routines

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'subr-x)


;; if-let tests

(ert-deftest subr-x-test-if-let-single-binding-expansion ()
  "Test single bindings are expanded properly."
  (should (equal
           (macroexpand
            '(if-let (a 1)
                 (- a)
               "no"))
           '(let* ((a (and t 1)))
              (if a
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let (a)
                 (- a)
               "no"))
           '(let* ((a (and t nil)))
              (if a
                  (- a)
                "no")))))

(ert-deftest subr-x-test-if-let-single-symbol-expansion ()
  "Test single symbol bindings are expanded properly."
  (should (equal
           (macroexpand
            '(if-let (a)
                 (- a)
               "no"))
           '(let* ((a (and t nil)))
              (if a
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let (a b c)
                 (- a)
               "no"))
           '(let* ((a (and t nil))
                   (b (and a nil))
                   (c (and b nil)))
              (if c
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let (a (b 2) c)
                 (- a)
               "no"))
           '(let* ((a (and t nil))
                   (b (and a 2))
                   (c (and b nil)))
              (if c
                  (- a)
                "no")))))

(ert-deftest subr-x-test-if-let-nil-related-expansion ()
  "Test nil is processed properly."
  (should (equal
           (macroexpand
            '(if-let (nil)
                 (- a)
               "no"))
           '(let* ((nil (and t nil)))
              (if nil
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let ((nil))
                 (- a)
               "no"))
           '(let* ((nil (and t nil)))
              (if nil
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let ((a 1) (nil) (b 2))
                 (- a)
               "no"))
           '(let* ((a (and t 1))
                   (nil (and a nil))
                   (b (and nil 2)))
              (if b
                  (- a)
                "no"))))
  (should (equal
           (macroexpand
            '(if-let ((a 1) nil (b 2))
                 (- a)
               "no"))
           '(let* ((a (and t 1))
                   (nil (and a nil))
                   (b (and nil 2)))
              (if b
                  (- a)
                "no")))))

(ert-deftest subr-x-test-if-let-malformed-binding ()
  "Test malformed bindings trigger errors."
  (should-error (macroexpand
                 '(if-let (_ (a 1 1) (b 2) (c 3) d)
                      (- a)
                    "no"))
                :type 'error)
  (should-error (macroexpand
                 '(if-let (_ (a 1) (b 2 2) (c 3) d)
                      (- a)
                    "no"))
                :type 'error)
  (should-error (macroexpand
                 '(if-let (_ (a 1) (b 2) (c 3 3) d)
                      (- a)
                    "no"))
                :type 'error)
  (should-error (macroexpand
                 '(if-let ((a 1 1))
                      (- a)
                    "no"))
                :type 'error))

(ert-deftest subr-x-test-if-let-true ()
  "Test `if-let' with truthy bindings."
  (should (equal
           (if-let (a 1)
               a
             "no")
           1))
  (should (equal
           (if-let ((a 1) (b 2) (c 3))
               (list a b c)
             "no")
           (list 1 2 3))))

(ert-deftest subr-x-test-if-let-false ()
  "Test `if-let' with falsie bindings."
  (should (equal
           (if-let (a nil)
               (list a b c)
             "no")
           "no"))
  (should (equal
           (if-let ((a nil) (b 2) (c 3))
               (list a b c)
             "no")
           "no"))
  (should (equal
           (if-let ((a 1) (b nil) (c 3))
               (list a b c)
             "no")
           "no"))
  (should (equal
           (if-let ((a 1) (b 2) (c nil))
               (list a b c)
             "no")
           "no"))
  (should (equal
           (if-let (z (a 1) (b 2) (c 3))
               (list a b c)
             "no")
           "no"))
  (should (equal
           (if-let ((a 1) (b 2) (c 3) d)
               (list a b c)
             "no")
           "no")))

(ert-deftest subr-x-test-if-let-bound-references ()
  "Test `if-let' bindings can refer to already bound symbols."
  (should (equal
           (if-let ((a (1+ 0)) (b (1+ a)) (c (1+ b)))
               (list a b c)
             "no")
           (list 1 2 3))))

(ert-deftest subr-x-test-if-let-and-laziness-is-preserved ()
  "Test `if-let' respects `and' laziness."
  (let (a-called b-called c-called)
    (should (equal
             (if-let ((a nil)
                      (b (setq b-called t))
                      (c (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list nil nil nil))))
  (let (a-called b-called c-called)
    (should (equal
             (if-let ((a (setq a-called t))
                      (b nil)
                      (c (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list t nil nil))))
  (let (a-called b-called c-called)
    (should (equal
             (if-let ((a (setq a-called t))
                      (b (setq b-called t))
                      (c nil)
                      (d (setq c-called t)))
                 "yes"
               (list a-called b-called c-called))
             (list t t nil)))))


;; when-let tests

(ert-deftest subr-x-test-when-let-body-expansion ()
  "Test body allows for multiple sexps wrapping with progn."
  (should (equal
           (macroexpand
            '(when-let (a 1)
               (message "opposite")
               (- a)))
           '(let* ((a (and t 1)))
              (if a
                  (progn
                    (message "opposite")
                    (- a)))))))

(ert-deftest subr-x-test-when-let-single-binding-expansion ()
  "Test single bindings are expanded properly."
  (should (equal
           (macroexpand
            '(when-let (a 1)
               (- a)))
           '(let* ((a (and t 1)))
              (if a
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let (a)
               (- a)))
           '(let* ((a (and t nil)))
              (if a
                  (- a))))))

(ert-deftest subr-x-test-when-let-single-symbol-expansion ()
  "Test single symbol bindings are expanded properly."
  (should (equal
           (macroexpand
            '(when-let (a)
               (- a)))
           '(let* ((a (and t nil)))
              (if a
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let (a b c)
               (- a)))
           '(let* ((a (and t nil))
                   (b (and a nil))
                   (c (and b nil)))
              (if c
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let (a (b 2) c)
               (- a)))
           '(let* ((a (and t nil))
                   (b (and a 2))
                   (c (and b nil)))
              (if c
                  (- a))))))

(ert-deftest subr-x-test-when-let-nil-related-expansion ()
  "Test nil is processed properly."
  (should (equal
           (macroexpand
            '(when-let (nil)
               (- a)))
           '(let* ((nil (and t nil)))
              (if nil
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let ((nil))
               (- a)))
           '(let* ((nil (and t nil)))
              (if nil
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let ((a 1) (nil) (b 2))
               (- a)))
           '(let* ((a (and t 1))
                   (nil (and a nil))
                   (b (and nil 2)))
              (if b
                  (- a)))))
  (should (equal
           (macroexpand
            '(when-let ((a 1) nil (b 2))
               (- a)))
           '(let* ((a (and t 1))
                   (nil (and a nil))
                   (b (and nil 2)))
              (if b
                  (- a))))))

(ert-deftest subr-x-test-when-let-malformed-binding ()
  "Test malformed bindings trigger errors."
  (should-error (macroexpand
                 '(when-let (_ (a 1 1) (b 2) (c 3) d)
                    (- a)))
                :type 'error)
  (should-error (macroexpand
                 '(when-let (_ (a 1) (b 2 2) (c 3) d)
                    (- a)))
                :type 'error)
  (should-error (macroexpand
                 '(when-let (_ (a 1) (b 2) (c 3 3) d)
                    (- a)))
                :type 'error)
  (should-error (macroexpand
                 '(when-let ((a 1 1))
                    (- a)))
                :type 'error))

(ert-deftest subr-x-test-when-let-true ()
  "Test `when-let' with truthy bindings."
  (should (equal
           (when-let (a 1)
             a)
           1))
  (should (equal
           (when-let ((a 1) (b 2) (c 3))
             (list a b c))
           (list 1 2 3))))

(ert-deftest subr-x-test-when-let-false ()
  "Test `when-let' with falsie bindings."
  (should (equal
           (when-let (a nil)
             (list a b c)
             "no")
           nil))
  (should (equal
           (when-let ((a nil) (b 2) (c 3))
             (list a b c)
             "no")
           nil))
  (should (equal
           (when-let ((a 1) (b nil) (c 3))
             (list a b c)
             "no")
           nil))
  (should (equal
           (when-let ((a 1) (b 2) (c nil))
             (list a b c)
             "no")
           nil))
  (should (equal
           (when-let (z (a 1) (b 2) (c 3))
             (list a b c)
             "no")
           nil))
  (should (equal
           (when-let ((a 1) (b 2) (c 3) d)
             (list a b c)
             "no")
           nil)))

(ert-deftest subr-x-test-when-let-bound-references ()
  "Test `when-let' bindings can refer to already bound symbols."
  (should (equal
           (when-let ((a (1+ 0)) (b (1+ a)) (c (1+ b)))
             (list a b c))
           (list 1 2 3))))

(ert-deftest subr-x-test-when-let-and-laziness-is-preserved ()
  "Test `when-let' respects `and' laziness."
  (let (a-called b-called c-called)
    (should (equal
             (progn
               (when-let ((a nil)
                          (b (setq b-called t))
                          (c (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list nil nil nil))))
  (let (a-called b-called c-called)
    (should (equal
             (progn
               (when-let ((a (setq a-called t))
                          (b nil)
                          (c (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list t nil nil))))
  (let (a-called b-called c-called)
    (should (equal
             (progn
               (when-let ((a (setq a-called t))
                          (b (setq b-called t))
                          (c nil)
                          (d (setq c-called t)))
                 "yes")
               (list a-called b-called c-called))
             (list t t nil)))))


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


(provide 'subr-x-tests)
;;; subr-x-tests.el ends here
