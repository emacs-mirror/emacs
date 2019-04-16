;;; cl-print-tests.el --- Test suite for the cl-print facility.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2019 Free Software Foundation, Inc.

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

(cl-defstruct cl-print--test a b)

(ert-deftest cl-print-tests-1 ()
  "Test cl-print code."
  (let ((x (make-cl-print--test :a 1 :b 2)))
    (let ((print-circle nil))
      (should (equal (cl-prin1-to-string `((x . ,x) (y . ,x)))
                     "((x . #s(cl-print--test :a 1 :b 2)) (y . #s(cl-print--test :a 1 :b 2)))")))
    (let ((print-circle t))
      (should (equal (cl-prin1-to-string `((x . ,x) (y . ,x)))
                     "((x . #1=#s(cl-print--test :a 1 :b 2)) (y . #1#))")))
    (should (string-match "\\`#f(compiled-function (x) \"[^\"]+\" [^)]*)\\'"
                          (cl-prin1-to-string (symbol-function #'caar))))))

(ert-deftest cl-print-tests-2 ()
  (let ((x (record 'foo 1 2 3)))
    (should (equal
             x
             (car (read-from-string (with-output-to-string (prin1 x))))))
    (let ((print-circle t))
      (should (string-match
               "\\`(#1=#s(foo 1 2 3) #1#)\\'"
               (cl-prin1-to-string (list x x)))))))

(cl-defstruct (cl-print-tests-struct
               (:constructor cl-print-tests-con))
  a b c d e)

(ert-deftest cl-print-tests-3 ()
  "CL printing observes `print-length'."
  (let ((long-list (make-list 5 'a))
        (long-vec (make-vector 5 'b))
        (long-struct (cl-print-tests-con))
        (long-string (make-string 5 ?a))
        (print-length 4))
    (should (equal "(a a a a ...)" (cl-prin1-to-string long-list)))
    (should (equal "[b b b b ...]" (cl-prin1-to-string long-vec)))
    (should (equal "#s(cl-print-tests-struct :a nil :b nil :c nil :d nil ...)"
                   (cl-prin1-to-string long-struct)))
    (should (equal "\"aaaa...\"" (cl-prin1-to-string long-string)))))

(ert-deftest cl-print-tests-4 ()
  "CL printing observes `print-level'."
  (let* ((deep-list '(a (b (c (d (e))))))
         (buried-vector '(a (b (c (d [e])))))
         (deep-struct (cl-print-tests-con))
         (buried-struct `(a (b (c (d ,deep-struct)))))
         (buried-string '(a (b (c (d #("hello" 0 5 (cl-print-test t)))))))
         (buried-simple-string '(a (b (c (d "hello")))))
         (print-level 4))
    (setf (cl-print-tests-struct-a deep-struct) deep-list)
    (should (equal "(a (b (c (d ...))))" (cl-prin1-to-string deep-list)))
    (should (equal "(a (b (c (d ...))))" (cl-prin1-to-string buried-vector)))
    (should (equal "(a (b (c (d ...))))" (cl-prin1-to-string buried-struct)))
    (should (equal "(a (b (c (d ...))))" (cl-prin1-to-string buried-string)))
    (should (equal "(a (b (c (d \"hello\"))))"
                   (cl-prin1-to-string buried-simple-string)))
    (should (equal "#s(cl-print-tests-struct :a (a (b (c ...))) :b nil :c nil :d nil :e nil)"
                   (cl-prin1-to-string deep-struct)))))

(ert-deftest cl-print-tests-5 ()
  "CL printing observes `print-quoted'."
  (let ((quoted-stuff '('a #'b `(,c ,@d))))
    (let ((print-quoted t))
      (should (equal "('a #'b `(,c ,@d))"
                     (cl-prin1-to-string quoted-stuff))))
    (let ((print-quoted nil))
      (should (equal "((quote a) (function b) (\\` ((\\, c) (\\,@ d))))"
                     (cl-prin1-to-string quoted-stuff))))))

(ert-deftest cl-print-tests-strings ()
  "CL printing prints strings and propertized strings."
  (let* ((str1 "abcdefghij")
         (str2 #("abcdefghij" 3 6 (bold t) 7 9 (italic t)))
         (str3 #("abcdefghij" 0 10 (test t)))
         (obj '(a b))
         ;; Since the byte compiler reuses string literals,
         ;; and the put-text-property call is destructive, use
         ;; copy-sequence to make a new string.
         (str4 (copy-sequence "abcdefghij")))
    (put-text-property 0 5 'test obj str4)
    (put-text-property 7 10 'test obj str4)

    (should (equal "\"abcdefghij\"" (cl-prin1-to-string str1)))
    (should (equal "#(\"abcdefghij\" 3 6 (bold t) 7 9 (italic t))"
                   (cl-prin1-to-string str2)))
    (should (equal "#(\"abcdefghij\" 0 10 (test t))"
                   (cl-prin1-to-string str3)))
    (let ((print-circle nil))
      (should
       (equal
        "#(\"abcdefghij\" 0 5 (test (a b)) 7 10 (test (a b)))"
        (cl-prin1-to-string str4))))
    (let ((print-circle t))
      (should
       (equal
        "#(\"abcdefghij\" 0 5 (test #1=(a b)) 7 10 (test #1#))"
        (cl-prin1-to-string str4))))))

(ert-deftest cl-print-tests-ellipsis-cons ()
  "Ellipsis expansion works in conses."
  (let ((print-length 4)
        (print-level 3))
    (cl-print-tests-check-ellipsis-expansion
     '(0 1 2 3 4 5) "(0 1 2 3 ...)" "4 5")
    (cl-print-tests-check-ellipsis-expansion
     '(0 1 2 3 4 5 6 7 8 9) "(0 1 2 3 ...)" "4 5 6 7 ...")
    (cl-print-tests-check-ellipsis-expansion
     '(a (b (c (d (e))))) "(a (b (c ...)))" "(d (e))")
    (cl-print-tests-check-ellipsis-expansion
     (let ((x (make-list 6 'b)))
       (setf (nthcdr 6 x) 'c)
       x)
     "(b b b b ...)" "b b . c")))

(ert-deftest cl-print-tests-ellipsis-vector ()
  "Ellipsis expansion works in vectors."
  (let ((print-length 4)
        (print-level 3))
    (cl-print-tests-check-ellipsis-expansion
     [0 1 2 3 4 5] "[0 1 2 3 ...]" "4 5")
    (cl-print-tests-check-ellipsis-expansion
     [0 1 2 3 4 5 6 7 8 9] "[0 1 2 3 ...]" "4 5 6 7 ...")
    (cl-print-tests-check-ellipsis-expansion
     [a [b [c [d [e]]]]] "[a [b [c ...]]]" "[d [e]]")))

(ert-deftest cl-print-tests-ellipsis-string ()
  "Ellipsis expansion works in strings."
  (let ((print-length 4)
        (print-level 3))
    (cl-print-tests-check-ellipsis-expansion
     "abcdefg" "\"abcd...\"" "efg")
    (cl-print-tests-check-ellipsis-expansion
     "abcdefghijk" "\"abcd...\"" "efgh...")
    (cl-print-tests-check-ellipsis-expansion
     '(1 (2 (3 #("abcde" 0 5 (test t)))))
     "(1 (2 (3 ...)))" "#(\"abcd...\" 0 5 (test t))")
    (cl-print-tests-check-ellipsis-expansion
     #("abcd" 0 1 (bold t) 1 2 (invisible t) 3 4 (italic t))
     "#(\"abcd\" 0 1 (bold t) ...)" "1 2 (invisible t) ...")))

(ert-deftest cl-print-tests-ellipsis-struct ()
  "Ellipsis expansion works in structures."
  (let ((print-length 4)
        (print-level 3)
        (struct (cl-print-tests-con)))
    (cl-print-tests-check-ellipsis-expansion
     struct "#s(cl-print-tests-struct :a nil :b nil :c nil :d nil ...)" ":e nil")
    (let ((print-length 2))
      (cl-print-tests-check-ellipsis-expansion
       struct "#s(cl-print-tests-struct :a nil :b nil ...)" ":c nil :d nil ..."))
    (cl-print-tests-check-ellipsis-expansion
     `(a (b (c ,struct)))
     "(a (b (c ...)))"
     "#s(cl-print-tests-struct :a nil :b nil :c nil :d nil ...)")))

(ert-deftest cl-print-tests-ellipsis-circular ()
  "Ellipsis expansion works with circular objects."
  (let ((wide-obj (list 0 1 2 3 4))
        (deep-obj `(0 (1 (2 (3 (4))))))
        (print-length 4)
        (print-level 3))
    (setf (nth 4 wide-obj) wide-obj)
    (setf (car (cadadr (cadadr deep-obj))) deep-obj)
    (let ((print-circle nil))
      (cl-print-tests-check-ellipsis-expansion-rx
       wide-obj (regexp-quote "(0 1 2 3 ...)") "\\`#[0-9]\\'")
      (cl-print-tests-check-ellipsis-expansion-rx
       deep-obj (regexp-quote "(0 (1 (2 ...)))") "\\`(3 (#[0-9]))\\'"))
    (let ((print-circle t))
      (cl-print-tests-check-ellipsis-expansion
       wide-obj "#1=(0 1 2 3 ...)" "#1#")
      (cl-print-tests-check-ellipsis-expansion
       deep-obj "#1=(0 (1 (2 ...)))" "(3 (#1#))"))))

(defun cl-print-tests-check-ellipsis-expansion (obj expected expanded)
  (let* ((result (cl-prin1-to-string obj))
         (pos (next-single-property-change 0 'cl-print-ellipsis result))
         value)
    (should pos)
    (setq value (get-text-property pos 'cl-print-ellipsis result))
    (should (equal expected result))
    (should (equal expanded (with-output-to-string (cl-print-expand-ellipsis
                                                    value nil))))))

(defun cl-print-tests-check-ellipsis-expansion-rx (obj expected expanded)
  (let* ((result (cl-prin1-to-string obj))
         (pos (next-single-property-change 0 'cl-print-ellipsis result))
         (value (get-text-property pos 'cl-print-ellipsis result)))
    (should (string-match expected result))
    (should (string-match expanded (with-output-to-string
                                     (cl-print-expand-ellipsis value nil))))))

(ert-deftest cl-print-circle ()
  (let ((x '(#1=(a . #1#) #1#)))
    (let ((print-circle nil))
      (should (string-match "\\`((a . #[0-9]) (a . #[0-9]))\\'"
                            (cl-prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(#1=(a . #1#) #1#)" (cl-prin1-to-string x))))))

(ert-deftest cl-print-circle-2 ()
  ;; Bug#31146.
  (let ((x '(0 . #1=(0 . #1#))))
    (let ((print-circle nil))
      (should (string-match "\\`(0 0 . #[0-9])\\'"
                            (cl-prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(0 . #1=(0 . #1#))" (cl-prin1-to-string x))))))

(ert-deftest cl-print-tests-print-to-string-with-limit ()
  (let* ((thing10 (make-list 10 'a))
         (thing100 (make-list 100 'a))
         (thing10x10 (make-list 10 thing10))
         (nested-thing (let ((val 'a))
                         (dotimes (_i 20)
                           (setq val (list val)))
                         val))
         ;; Make a consistent environment for this test.
         (print-circle nil)
         (print-level nil)
         (print-length nil))

    ;; Print something that fits in the space given.
    (should (string= (cl-prin1-to-string thing10)
                     (cl-print-to-string-with-limit #'cl-prin1 thing10 100)))

    ;; Print something which needs to be abbreviated and which can be.
    (should (< (length (cl-print-to-string-with-limit #'cl-prin1 thing100 100))
               100
               (length (cl-prin1-to-string thing100))))

    ;; Print something resistant to easy abbreviation.
    (should (string= (cl-prin1-to-string thing10x10)
                     (cl-print-to-string-with-limit #'cl-prin1 thing10x10 100)))

    ;; Print something which should be abbreviated even if the limit is large.
    (should (< (length (cl-print-to-string-with-limit #'cl-prin1 nested-thing 1000))
               (length (cl-prin1-to-string nested-thing))))

    ;; Print with no limits.
    (dolist (thing (list thing10 thing100 thing10x10 nested-thing))
      (let ((rep (cl-prin1-to-string thing)))
        (should (string= rep (cl-print-to-string-with-limit #'cl-prin1 thing 0)))
        (should (string= rep (cl-print-to-string-with-limit #'cl-prin1 thing nil)))))))


;;; cl-print-tests.el ends here.
