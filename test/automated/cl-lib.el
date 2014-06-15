;;; cl-lib.el --- tests for emacs-lisp/cl-lib.el

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

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
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

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
      (should (equal (cl-set-difference a b) (list c2 "x" "" nil 'a)))
      (should (equal (cl-set-difference b a) (list 'x 'y)))

      ;; We aren't testing whether this is really using `eq' rather than `eql'.
      (should (equal (cl-set-difference e e :test 'eq) e))
      (should (equal (cl-set-difference a e :test 'eq) a))
      (should (equal (cl-set-difference e a :test 'eq) e))
      (should (equal (cl-set-difference a a :test 'eq) e))
      (should (equal (cl-set-difference b e :test 'eq) b))
      (should (equal (cl-set-difference e b :test 'eq) e))
      (should (equal (cl-set-difference b b :test 'eq) e))
      (should (equal (cl-set-difference a b :test 'eq) (list c2 "x" "" nil 'a)))
      (should (equal (cl-set-difference b a :test 'eq) (list 'x 'y)))

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

(ert-deftest cl-lib-test-loop ()
  (should (eql (cl-loop with (a b c) = '(1 2 3) return (+ a b c)) 6)))

(ert-deftest cl-lib-keyword-names-versus-values ()
  (should (equal
           (funcall (cl-function (lambda (&key a b) (list a b)))
                    :b :a :a 42)
           '(42 :a))))

(cl-defstruct mystruct (abc :readonly t) def)
(ert-deftest cl-lib-struct-accessors ()
  (let ((x (make-mystruct :abc 1 :def 2)))
    (should (eql (cl-struct-slot-value 'mystruct 'abc x) 1))
    (should (eql (cl-struct-slot-value 'mystruct 'def x) 2))
    (setf (cl-struct-slot-value 'mystruct 'def x) -1)
    (should (eql (cl-struct-slot-value 'mystruct 'def x) -1))
    (should (eql (cl-struct-slot-offset 'mystruct 'abc) 1))
    (should-error (cl-struct-slot-offset 'mystruct 'marypoppins))
    (should (equal (cl-struct-slot-info 'mystruct)
                   '((cl-tag-slot) (abc :readonly t) (def))))))

(ert-deftest cl-the ()
  (should (eql (cl-the integer 42) 42))
  (should-error (cl-the integer "abc"))
  (let ((side-effect 0))
    (should (= (cl-the integer (cl-incf side-effect)) 1))
    (should (= side-effect 1))))

(ert-deftest cl-loop-destructuring-with ()
  (should (equal (cl-loop with (a b c) = '(1 2 3) return (+ a b c)) 6)))

;;; cl-lib.el ends here
