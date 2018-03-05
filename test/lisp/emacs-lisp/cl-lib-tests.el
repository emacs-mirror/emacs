;;; cl-lib-tests.el --- tests for emacs-lisp/cl-lib.el  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Commentary:

;; Extracted from ert-tests.el, back when ert used to reimplement some
;; cl functions.

;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest cl-lib-test-remprop ()
  (let ((x (cl-gensym)))
    (should (equal (symbol-plist x) '()))
    ;; Remove nonexistent property on empty plist.
    (cl-remprop x 'b)
    (should (equal (symbol-plist x) '()))
    (put x 'a 1)
    (should (equal (symbol-plist x) '(a 1)))
    ;; Remove nonexistent property on nonempty plist.
    (cl-remprop x 'b)
    (should (equal (symbol-plist x) '(a 1)))
    (put x 'b 2)
    (put x 'c 3)
    (put x 'd 4)
    (should (equal (symbol-plist x) '(a 1 b 2 c 3 d 4)))
    ;; Remove property that is neither first nor last.
    (cl-remprop x 'c)
    (should (equal (symbol-plist x) '(a 1 b 2 d 4)))
    ;; Remove last property from a plist of length >1.
    (cl-remprop x 'd)
    (should (equal (symbol-plist x) '(a 1 b 2)))
    ;; Remove first property from a plist of length >1.
    (cl-remprop x 'a)
    (should (equal (symbol-plist x) '(b 2)))
    ;; Remove property when there is only one.
    (cl-remprop x 'b)
    (should (equal (symbol-plist x) '()))))

(ert-deftest cl-lib-test-remove-if-not ()
  (let ((list (list 'a 'b 'c 'd))
        (i 0))
    (let ((result (cl-remove-if-not (lambda (x)
                                        (should (eql x (nth i list)))
                                        (cl-incf i)
                                        (member i '(2 3)))
                                      list)))
      (should (equal i 4))
      (should (equal result '(b c)))
      (should (equal list '(a b c d)))))
  (should (equal '()
                 (cl-remove-if-not (lambda (_x) (should nil)) '()))))

(ert-deftest cl-lib-test-remove ()
  (let ((list (list 'a 'b 'c 'd))
        (key-index 0)
        (test-index 0))
    (let ((result
           (cl-remove 'foo list
                         :key (lambda (x)
                                (should (eql x (nth key-index list)))
                                (prog1
                                    (list key-index x)
                                  (cl-incf key-index)))
                         :test
                         (lambda (a b)
                           (should (eql a 'foo))
                           (should (equal b (list test-index
                                                  (nth test-index list))))
                           (cl-incf test-index)
                           (member test-index '(2 3))))))
      (should (equal key-index 4))
      (should (equal test-index 4))
      (should (equal result '(a d)))
      (should (equal list '(a b c d)))))
  (let ((x (cons nil nil))
        (y (cons nil nil)))
    (should (equal (cl-remove x (list x y))
                   ;; or (list x), since we use `equal' -- the
                   ;; important thing is that only one element got
                   ;; removed, this proves that the default test is
                   ;; `eql', not `equal'
                   (list y)))))


(ert-deftest cl-lib-test-set-functions ()
  (let ((c1 (cons nil nil))
        (c2 (cons nil nil))
        (sym (make-symbol "a")))
    (let ((e '())
          (a (list 'a 'b sym nil "" "x" c1 c2))
          (b (list c1 'y 'b sym 'x)))
      (should (equal (cl-set-difference e e) e))
      (should (equal (cl-set-difference a e) a))
      (should (equal (cl-set-difference e a) e))
      (should (equal (cl-set-difference a a) e))
      (should (equal (cl-set-difference b e) b))
      (should (equal (cl-set-difference e b) e))
      (should (equal (cl-set-difference b b) e))
      ;; Note: this test (and others) is sensitive to the order of the
      ;; result, which is not documented.
      (should (equal (cl-set-difference a b) (list 'a  nil "" "x" c2)))
      (should (equal (cl-set-difference b a) (list 'y 'x)))

      ;; We aren't testing whether this is really using `eq' rather than `eql'.
      (should (equal (cl-set-difference e e :test 'eq) e))
      (should (equal (cl-set-difference a e :test 'eq) a))
      (should (equal (cl-set-difference e a :test 'eq) e))
      (should (equal (cl-set-difference a a :test 'eq) e))
      (should (equal (cl-set-difference b e :test 'eq) b))
      (should (equal (cl-set-difference e b :test 'eq) e))
      (should (equal (cl-set-difference b b :test 'eq) e))
      (should (equal (cl-set-difference a b :test 'eq) (list 'a  nil "" "x" c2)))
      (should (equal (cl-set-difference b a :test 'eq) (list 'y 'x)))

      (should (equal (cl-union e e) e))
      (should (equal (cl-union a e) a))
      (should (equal (cl-union e a) a))
      (should (equal (cl-union a a) a))
      (should (equal (cl-union b e) b))
      (should (equal (cl-union e b) b))
      (should (equal (cl-union b b) b))
      (should (equal (cl-union a b) (list 'x 'y 'a 'b sym nil "" "x" c1 c2)))

      (should (equal (cl-union b a) (list 'x 'y 'a 'b sym nil "" "x" c1 c2)))

      (should (equal (cl-intersection e e) e))
      (should (equal (cl-intersection a e) e))
      (should (equal (cl-intersection e a) e))
      (should (equal (cl-intersection a a) a))
      (should (equal (cl-intersection b e) e))
      (should (equal (cl-intersection e b) e))
      (should (equal (cl-intersection b b) b))
      (should (equal (cl-intersection a b) (list sym 'b c1)))
      (should (equal (cl-intersection b a) (list sym 'b c1))))))

(ert-deftest cl-lib-test-gensym ()
  ;; Since the expansion of `should' calls `cl-gensym' and thus has a
  ;; side-effect on `cl--gensym-counter', we have to make sure all
  ;; macros in our test body are expanded before we rebind
  ;; `cl--gensym-counter' and run the body.  Otherwise, the test would
  ;; fail if run interpreted.
  (let ((body (byte-compile
               '(lambda ()
                  (should (equal (symbol-name (cl-gensym)) "G0"))
                  (should (equal (symbol-name (cl-gensym)) "G1"))
                  (should (equal (symbol-name (cl-gensym)) "G2"))
                  (should (equal (symbol-name (cl-gensym "foo")) "foo3"))
                  (should (equal (symbol-name (cl-gensym "bar")) "bar4"))
                  (should (equal cl--gensym-counter 5))))))
    (let ((cl--gensym-counter 0))
      (funcall body))))

(ert-deftest cl-lib-test-coerce-to-vector ()
  (let* ((a (vector))
         (b (vector 1 a 3))
         (c (list))
         (d (list b a)))
    (should (eql (cl-coerce a 'vector) a))
    (should (eql (cl-coerce b 'vector) b))
    (should (equal (cl-coerce c 'vector) (vector)))
    (should (equal (cl-coerce d 'vector) (vector b a)))))

(ert-deftest cl-lib-test-string-position ()
  (should (eql (cl-position ?x "") nil))
  (should (eql (cl-position ?a "abc") 0))
  (should (eql (cl-position ?b "abc") 1))
  (should (eql (cl-position ?c "abc") 2))
  (should (eql (cl-position ?d "abc") nil))
  (should (eql (cl-position ?A "abc") nil)))

(ert-deftest cl-lib-test-mismatch ()
  (should (eql (cl-mismatch "" "") nil))
  (should (eql (cl-mismatch "" "a") 0))
  (should (eql (cl-mismatch "a" "a") nil))
  (should (eql (cl-mismatch "ab" "a") 1))
  (should (eql (cl-mismatch "Aa" "aA") 0))
  (should (eql (cl-mismatch '(a b c) '(a b d)) 2)))

(ert-deftest cl-lib-keyword-names-versus-values ()
  (should (equal
           (funcall (cl-function (lambda (&key a b) (list a b)))
                    :b :a :a 42)
           '(42 :a))))

(ert-deftest cl-lib-empty-keyargs ()
  (should-error (funcall (cl-function (lambda (&key) 1))
                         :b 1)))

(cl-defstruct (mystruct
               (:constructor cl-lib--con-1 (&aux (abc 1)))
               (:constructor cl-lib--con-2 (&optional def) "Constructor docstring."))
  "General docstring."
  (abc 5 :readonly t) (def nil))
(ert-deftest cl-lib-struct-accessors ()
  (let ((x (make-mystruct :abc 1 :def 2)))
    (should (eql (cl-struct-slot-value 'mystruct 'abc x) 1))
    (should (eql (cl-struct-slot-value 'mystruct 'def x) 2))
    (setf (cl-struct-slot-value 'mystruct 'def x) -1)
    (should (eql (cl-struct-slot-value 'mystruct 'def x) -1))
    (should (eql (cl-struct-slot-offset 'mystruct 'abc) 1))
    (should-error (cl-struct-slot-offset 'mystruct 'marypoppins))
    (should (pcase (cl-struct-slot-info 'mystruct)
              (`((cl-tag-slot) (abc 5 :readonly t)
                 (def . ,(or `nil `(nil))))
               t)))))
(ert-deftest cl-lib-struct-constructors ()
  (should (string-match "\\`Constructor docstring."
                        (documentation 'cl-lib--con-2 t)))
  (should (mystruct-p (cl-lib--con-1)))
  (should (mystruct-p (cl-lib--con-2))))

(ert-deftest cl-lib-arglist-performance ()
  ;; An `&aux' should not cause lambda's arglist to be turned into an &rest
  ;; that's parsed by hand.
  (should (equal () (help-function-arglist 'cl-lib--con-1)))
  (should (pcase (help-function-arglist 'cl-lib--con-2)
            (`(&optional ,_) t))))

(ert-deftest cl-the ()
  (should (eql (cl-the integer 42) 42))
  (should-error (cl-the integer "abc"))
  (let ((side-effect 0))
    (should (= (cl-the integer (cl-incf side-effect)) 1))
    (should (= side-effect 1))))

(ert-deftest cl-lib-test-plusp ()
  (should-not (cl-plusp -1.0e+INF))
  (should-not (cl-plusp -1.5e2))
  (should-not (cl-plusp -3.14))
  (should-not (cl-plusp -1))
  (should-not (cl-plusp -0.0))
  (should-not (cl-plusp 0))
  (should-not (cl-plusp 0.0))
  (should-not (cl-plusp -0.0e+NaN))
  (should-not (cl-plusp 0.0e+NaN))
  (should (cl-plusp 1))
  (should (cl-plusp 3.14))
  (should (cl-plusp 1.5e2))
  (should (cl-plusp 1.0e+INF))
  (should-error (cl-plusp "42") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-minusp ()
  (should (cl-minusp -1.0e+INF))
  (should (cl-minusp -1.5e2))
  (should (cl-minusp -3.14))
  (should (cl-minusp -1))
  (should-not (cl-minusp -0.0))
  (should-not (cl-minusp 0))
  (should-not (cl-minusp 0.0))
  (should-not (cl-minusp -0.0e+NaN))
  (should-not (cl-minusp 0.0e+NaN))
  (should-not (cl-minusp 1))
  (should-not (cl-minusp 3.14))
  (should-not (cl-minusp 1.5e2))
  (should-not (cl-minusp 1.0e+INF))
  (should-error (cl-minusp "-42") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-oddp ()
  (should (cl-oddp -3))
  (should (cl-oddp 3))
  (should-not (cl-oddp -2))
  (should-not (cl-oddp 0))
  (should-not (cl-oddp 2))
  (should-error (cl-oddp 3.0e+NaN) :type 'wrong-type-argument)
  (should-error (cl-oddp 3.0) :type 'wrong-type-argument)
  (should-error (cl-oddp "3") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-evenp ()
  (should (cl-evenp -2))
  (should (cl-evenp 0))
  (should (cl-evenp 2))
  (should-not (cl-evenp -3))
  (should-not (cl-evenp 3))
  (should-error (cl-evenp 2.0e+NaN) :type 'wrong-type-argument)
  (should-error (cl-evenp 2.0) :type 'wrong-type-argument)
  (should-error (cl-evenp "2") :type 'wrong-type-argument))

(ert-deftest cl-digit-char-p ()
  (should (eql 3 (cl-digit-char-p ?3)))
  (should (eql 10 (cl-digit-char-p ?a 11)))
  (should (eql 10 (cl-digit-char-p ?A 11)))
  (should-not (cl-digit-char-p ?a))
  (should (eql 32 (cl-digit-char-p ?w 36)))
  (should-error (cl-digit-char-p ?a 37) :type 'args-out-of-range)
  (should-error (cl-digit-char-p ?a 1) :type 'args-out-of-range))

(ert-deftest cl-lib-test-first ()
  (should (null (cl-first '())))
  (should (= 4 (cl-first '(4))))
  (should (= 4 (cl-first '(4 2))))
  (should-error (cl-first "42") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-second ()
  (should (null (cl-second '())))
  (should (null (cl-second '(4))))
  (should (= 2 (cl-second '(1 2))))
  (should (= 2 (cl-second '(1 2 3))))
  (should-error (cl-second "1 2 3") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-third ()
  (should (null (cl-third '())))
  (should (null (cl-third '(1 2))))
  (should (= 3 (cl-third '(1 2 3))))
  (should (= 3 (cl-third '(1 2 3 4))))
  (should-error (cl-third "123") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-fourth ()
  (should (null (cl-fourth '())))
  (should (null (cl-fourth '(1 2 3))))
  (should (= 4 (cl-fourth '(1 2 3 4))))
  (should (= 4 (cl-fourth '(1 2 3 4 5))))
  (should-error (cl-fourth "1234") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-fifth ()
  (should (null (cl-fifth '())))
  (should (null (cl-fifth '(1 2 3 4))))
  (should (= 5 (cl-fifth '(1 2 3 4 5))))
  (should (= 5 (cl-fifth '(1 2 3 4 5 6))))
  (should-error (cl-fifth "12345") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-fifth ()
  (should (null (cl-fifth '())))
  (should (null (cl-fifth '(1 2 3 4))))
  (should (= 5 (cl-fifth '(1 2 3 4 5))))
  (should (= 5 (cl-fifth '(1 2 3 4 5 6))))
  (should-error (cl-fifth "12345") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-sixth ()
  (should (null (cl-sixth '())))
  (should (null (cl-sixth '(1 2 3 4 5))))
  (should (= 6 (cl-sixth '(1 2 3 4 5 6))))
  (should (= 6 (cl-sixth '(1 2 3 4 5 6 7))))
  (should-error (cl-sixth "123456") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-seventh ()
  (should (null (cl-seventh '())))
  (should (null (cl-seventh '(1 2 3 4 5 6))))
  (should (= 7 (cl-seventh '(1 2 3 4 5 6 7))))
  (should (= 7 (cl-seventh '(1 2 3 4 5 6 7 8))))
  (should-error (cl-seventh "1234567") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-eighth ()
  (should (null (cl-eighth '())))
  (should (null (cl-eighth '(1 2 3 4 5 6 7))))
  (should (= 8 (cl-eighth '(1 2 3 4 5 6 7 8))))
  (should (= 8 (cl-eighth '(1 2 3 4 5 6 7 8 9))))
  (should-error (cl-eighth "12345678") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-ninth ()
  (should (null (cl-ninth '())))
  (should (null (cl-ninth '(1 2 3 4 5 6 7 8))))
  (should (= 9 (cl-ninth '(1 2 3 4 5 6 7 8 9))))
  (should (= 9 (cl-ninth '(1 2 3 4 5 6 7 8 9 10))))
  (should-error (cl-ninth "123456789") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-tenth ()
  (should (null (cl-tenth '())))
  (should (null (cl-tenth '(1 2 3 4 5 6 7 8 9))))
  (should (= 10 (cl-tenth '(1 2 3 4 5 6 7 8 9 10))))
  (should (= 10 (cl-tenth '(1 2 3 4 5 6 7 8 9 10 11))))
  (should-error (cl-tenth "1234567890") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-endp ()
  (should (cl-endp '()))
  (should-not (cl-endp '(1)))
  (should-error (cl-endp 1) :type 'wrong-type-argument)
  (should-error (cl-endp [1]) :type 'wrong-type-argument))

(ert-deftest cl-lib-test-nth-value ()
  (let ((vals (cl-values 2 3)))
    (should (= (cl-nth-value 0 vals) 2))
    (should (= (cl-nth-value 1 vals) 3))
    (should (null (cl-nth-value 2 vals)))
    (should-error (cl-nth-value 0.0 vals) :type 'wrong-type-argument)))

(ert-deftest cl-lib-nth-value-test-multiple-values ()
  "While CL multiple values are an alias to list, these won't work."
  :expected-result :failed
  (should (eq (cl-nth-value 0 '(2 3)) '(2 3)))
  (should (= (cl-nth-value 0 1) 1))
  (should (null (cl-nth-value 1 1)))
  (should-error (cl-nth-value -1 (cl-values 2 3)) :type 'args-out-of-range)
  (should (string= (cl-nth-value 0 "only lists") "only lists")))

(ert-deftest cl-test-caaar ()
  (should (null (cl-caaar '())))
  (should (null (cl-caaar '(() (2)))))
  (should (null (cl-caaar '((() (2)) (a b)))))
  (should-error (cl-caaar '(1 2)) :type 'wrong-type-argument)
  (should-error (cl-caaar '((1 2))) :type 'wrong-type-argument)
  (should (=  1 (cl-caaar '(((1 2) (3 4))))))
  (should (null (cl-caaar '((() (3 4)))))))

(ert-deftest cl-test-caadr ()
  (should (null (cl-caadr '())))
  (should (null (cl-caadr '(1))))
  (should-error (cl-caadr '(1 2)) :type 'wrong-type-argument)
  (should (= 2 (cl-caadr '(1 (2 3)))))
  (should (equal '((2) (3)) (cl-caadr '((1) (((2) (3))) (4))))))

(ert-deftest cl-test-ldiff ()
  (let ((l '(1 2 3)))
    (should (null (cl-ldiff '() '())))
    (should (null (cl-ldiff '() l)))
    (should (null (cl-ldiff l l)))
    (should (equal l (cl-ldiff l '())))
    ;; must be part of the list
    (should (equal l (cl-ldiff l '(2 3))))
    (should (equal '(1) (cl-ldiff l (nthcdr 1 l))))
    ;; should return a copy
    (should-not (eq (cl-ldiff l '()) l))))

(ert-deftest cl-lib-adjoin-test ()
  (let ((nums '(1 2))
        (myfn-p '=))
    ;; add non-existing item to the front
    (should (equal '(3 1 2) (cl-adjoin 3 nums)))
    ;; just add - don't copy rest
    (should (eq nums (cdr (cl-adjoin 3 nums))))
    ;; add only when not already there
    (should (eq nums (cl-adjoin 2 nums)))
    (should (equal '(2 1 (2)) (cl-adjoin 2 '(1 (2)))))
    ;; default test function is eql
    (should (equal '(1.0 1 2) (cl-adjoin 1.0 nums)))
    ;; own :test function - returns true if match
    (should (equal '(1.0 1 2) (cl-adjoin 1.0 nums :test nil))) ;defaults to eql
    (should (eq nums (cl-adjoin 2 nums :test myfn-p))) ;match
    (should (equal '(3 1 2) (cl-adjoin 3 nums :test myfn-p))) ;no match
    ;; own :test-not function - returns false if match
    (should (equal '(1.0 1 2) (cl-adjoin 1.0 nums :test-not nil))) ;defaults to eql
    (should (equal '(2 2) (cl-adjoin 2 '(2) :test-not myfn-p))) ; no match
    (should (eq nums (cl-adjoin 2 nums :test-not myfn-p))) ; 1 matches
    (should (eq nums (cl-adjoin 3 nums :test-not myfn-p))) ; 1 and 2 matches

    ;; according to CLtL2 passing both :test and :test-not should signal error
    ;;(should-error (cl-adjoin 3 nums :test 'myfn-p :test-not myfn-p))

    ;; own :key fn
    (should (eq nums (cl-adjoin 3 nums :key (lambda (x) (if (cl-evenp x) (1+ x) x)))))
    (should (equal '(3 1 2) (cl-adjoin 3 nums :key (lambda (x) (if (cl-evenp x) (+ 2 x) x)))))

    ;; convert using :key, then compare with :test
    (should (eq nums (cl-adjoin 1 nums :key 'int-to-string :test 'string=)))
    (should (equal '(3 1 2) (cl-adjoin 3 nums :key 'int-to-string :test 'string=)))
    (should-error (cl-adjoin 3 nums :key 'int-to-string :test myfn-p)
                  :type 'wrong-type-argument)

    ;; convert using :key, then compare with :test-not
    (should (eq nums (cl-adjoin 3 nums :key 'int-to-string :test-not 'string=)))
    (should (equal '(1 1) (cl-adjoin 1 '(1) :key 'int-to-string :test-not 'string=)))
    (should-error (cl-adjoin 1 nums :key 'int-to-string :test-not myfn-p)
                  :type 'wrong-type-argument)))

(ert-deftest cl-parse-integer ()
  (should-error (cl-parse-integer "abc"))
  (should (null (cl-parse-integer "abc" :junk-allowed t)))
  (should (null (cl-parse-integer "" :junk-allowed t)))
  (should (= 342391 (cl-parse-integer "0123456789" :radix 8 :junk-allowed t)))
  (should-error (cl-parse-integer "0123456789" :radix 8))
  (should (= -239 (cl-parse-integer "-efz" :radix 16 :junk-allowed t)))
  (should-error (cl-parse-integer "efz" :radix 16))
  (should (= 239 (cl-parse-integer "zzef" :radix 16 :start 2)))
  (should (= -123 (cl-parse-integer "	-123  "))))

(ert-deftest cl-flet-test ()
  (should (equal (cl-flet ((f1 (x) x)) (let ((x #'f1)) (funcall x 5))) 5)))

(ert-deftest cl-lib-test-typep ()
  (cl-deftype cl-lib-test-type (&optional x) `(member ,x))
  ;; Make sure we correctly implement the rule that deftype's optional args
  ;; default to `*' rather than to nil.
  (should (cl-typep '* 'cl-lib-test-type))
  (should-not (cl-typep 1 'cl-lib-test-type)))

(ert-deftest cl-lib-symbol-macrolet ()
  ;; bug#26325
  (should (equal (cl-flet ((f (x) (+ x 5)))
                   (let ((x 5))
                     (f (+ x 6))))
                 ;; Go through `eval', otherwise the macro-expansion
                 ;; error prevents running the whole test suite :-(
                 (eval '(cl-symbol-macrolet ((f (+ x 6)))
                          (cl-flet ((f (x) (+ x 5)))
                            (let ((x 5))
                              (f f))))
                       t))))

(defmacro cl-lib-symbol-macrolet-4+5 ()
  ;; bug#26068
  (let* ((sname "x")
         (s1 (make-symbol sname))
         (s2 (make-symbol sname)))
    `(cl-symbol-macrolet ((,s1 4)
                          (,s2 5))
       (+ ,s1 ,s2))))

(ert-deftest cl-lib-symbol-macrolet-2 ()
  (should (equal (cl-lib-symbol-macrolet-4+5) (+ 4 5))))


(ert-deftest cl-lib-symbol-macrolet-hide ()
  ;; bug#26325, bug#26073
  (should (equal (let ((y 5))
                   (cl-symbol-macrolet ((x y))
                     (list x
                           (let ((x 6)) (list x y))
                           (cl-letf ((x 6)) (list x y))
                           (apply (lambda (x) (+ x 1)) (list 8)))))
                 '(5 (6 5) (6 6) 9))))

(defun cl-lib-tests--dummy-function ()
  ;; Dummy function to see if the file is compiled.
  t)

(ert-deftest cl-lib-defstruct-record ()
  ;; This test fails when compiled, see Bug#24402/27718.
  :expected-result (if (byte-code-function-p
                        (symbol-function 'cl-lib-tests--dummy-function))
                       :failed :passed)
  (cl-defstruct foo x)
  (let ((x (make-foo :x 42)))
    (should (recordp x))
    (should (eq (type-of x) 'foo))
    (should (eql (foo-x x) 42))))

(ert-deftest old-struct ()
  (cl-defstruct foo x)
  (let ((x [cl-struct-foo])
        (saved cl-old-struct-compat-mode))
    (cl-old-struct-compat-mode -1)
    (should (eq (type-of x) 'vector))

    (cl-old-struct-compat-mode 1)
    (let ((cl-struct-foo (cl--struct-get-class 'foo)))
      (setf (symbol-function 'cl-struct-foo) :quick-object-witness-check)
      (should (eq (type-of x) 'foo))
      (should (eq (type-of [foo]) 'vector)))

    (cl-old-struct-compat-mode (if saved 1 -1))))

(ert-deftest cl-lib-old-struct ()
  (let ((saved cl-old-struct-compat-mode))
    (cl-old-struct-compat-mode -1)
    (cl-struct-define 'foo "" 'cl-structure-object nil nil nil
                      'cl-struct-foo-tags 'cl-struct-foo t)
    (should cl-old-struct-compat-mode)
    (cl-old-struct-compat-mode (if saved 1 -1))))

;;; cl-lib-tests.el ends here
