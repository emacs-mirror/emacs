;;; cl-extra-tests.el --- tests for emacs-lisp/cl-extra.el  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

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

(require 'cl-lib)
(require 'ert)

(ert-deftest cl-lib-test-remprop ()
  (cl-with-gensyms (x)
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

(ert-deftest cl-get ()
  (put 'cl-get-test 'x 1)
  (put 'cl-get-test 'y nil)
  (should (eq (cl-get 'cl-get-test 'x) 1))
  (should (eq (cl-get 'cl-get-test 'y :none) nil))
  (should (eq (cl-get 'cl-get-test 'z :none) :none))
  (let ((sym (make-symbol "test")))
    (put sym 'foo 'bar)
    (should (equal (cl-get sym 'foo) 'bar))
    (cl-remprop sym 'foo)
    (should (equal (cl-get sym 'foo 'default) 'default))))

(ert-deftest cl-lib-test-coerce-to-vector ()
  (let* ((a (vector))
         (b (vector 1 a 3))
         (c (list))
         (d (list b a)))
    (should (eql (cl-coerce a 'vector) a))
    (should (eql (cl-coerce b 'vector) b))
    (should (equal (cl-coerce c 'vector) (vector)))
    (should (equal (cl-coerce d 'vector) (vector b a)))))

(ert-deftest cl-extra-test-coerce ()
  (should (equal (cl-coerce "abc" 'list) '(?a ?b ?c)))
  (should (equal (cl-coerce ["a" "b" "c"] 'list) '("a" "b" "c")))
  (should (equal (cl-coerce "abc" 'vector) [97 98 99]))
  (should (equal (cl-coerce '("a" "b" "c") 'vector) ["a" "b" "c"]))
  (should (equal (cl-coerce '(3 4) 'bool-vector) #&2""))
  (should (equal (cl-coerce "abc" 'bool-vector) #&3""))
  (should (equal (cl-coerce [1] 'string) (char-to-string 1)))
  (should (equal (cl-coerce '(1) 'string) (char-to-string 1)))
  (should (equal (cl-coerce '(1 2 3) 'array) [1 2 3]))
  (should (equal (cl-coerce "abc" 'array) "abc"))
  (should-error (cl-coerce (list 1 2 3) 'character))
  (should-error (cl-coerce [1 2 3] 'character))
  (should-error (cl-coerce "abc" 'character))
  (should (equal (cl-coerce "a" 'character) 97))
  (should (equal (cl-coerce 'a 'character) 97)))

(ert-deftest cl-extra-test-equalp ()
  (should (cl-equalp "Test" "test"))
  (should (cl-equalp 1 1.0))
  (should (cl-equalp '(1 2 3) '(1 2 3)))
  (should (cl-equalp [1 2 3] [1 2 3]))
  (should-not (cl-equalp "Test1" "Test2"))
  (should-not (cl-equalp 1 2))
  (should-not (cl-equalp '(1 2 3) '(4 5 6)))
  (should-not (cl-equalp [1 2 3] [4 5 6])))

(ert-deftest cl-getf ()
  (let ((plist '(x 1 y nil)))
    (should (eq (cl-getf plist 'x) 1))
    (should-not (cl-getf plist 'y :none))
    (should (eq (cl-getf plist 'z :none) :none))
    (should (eq (incf (cl-getf plist 'x 10) 2) 3))
    (should (equal plist '(x 3 y nil)))
    (should-error (incf (cl-getf plist 'y 10) 4) :type 'wrong-type-argument)
    (should (equal plist '(x 3 y nil)))
    (should (eq (incf (cl-getf plist 'z 10) 5) 15))
    (should (equal plist '(z 15 x 3 y nil))))
  (let ((plist '(x 1 y)))
    (should (eq (cl-getf plist 'x) 1))
    (should (eq (cl-getf plist 'y :none) :none))
    (should (eq (cl-getf plist 'z :none) :none))
    (should (eq (incf (cl-getf plist 'x 10) 2) 3))
    (should (equal plist '(x 3 y)))
    (should (eq (incf (cl-getf plist 'y 10) 4) 14))
    (should (equal plist '(y 14 x 3 y))))
  (let ((plist '(x 1 y . 2)))
    (should (eq (cl-getf plist 'x) 1))
    (should (eq (incf (cl-getf plist 'x 10) 2) 3))
    (should (equal plist '(x 3 y . 2)))
    (should-error (cl-getf plist 'y :none) :type 'wrong-type-argument)
    (should-error (cl-getf plist 'z :none) :type 'wrong-type-argument)))

(ert-deftest cl-extra-test-mapc ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (_x) nil))
        (fn2 (lambda (_x _y) nil))
        (fn3 (lambda (_x _y _z) nil)))
    (should (equal lst (cl-mapc fn1 lst)))
    (should (equal lst (cl-mapc fn2 lst lst2)))
    (should (equal lst (cl-mapc fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-mapl ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) (should (consp x))))
        (fn2 (lambda (x y) (should (and (consp x) (consp y)))))
        (fn3 (lambda (x y z) (should (and (consp x) (consp y) (consp z))))))
    (should (equal lst (cl-mapl fn1 lst)))
    (should (equal lst (cl-mapl fn2 lst lst2)))
    (should (equal lst (cl-mapl fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-mapcar ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) x))
        (fn2 (lambda (_x y) y))
        (fn3 (lambda (_x _y z) z)))
    (should (equal lst (cl-mapcar fn1 lst)))
    (should (equal lst2 (cl-mapcar fn2 lst lst2)))
    (should (equal lst3 (cl-mapcar fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-map ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) x))
        (fn2 (lambda (_x y) y))
        (fn3 (lambda (x _y _z) (string-to-char (format "%S" x)))))
    (should (equal lst (cl-map 'list fn1 lst)))
    (should (equal (vconcat lst2) (cl-map 'vector fn2 lst lst2)))
    (should (equal (mapconcat (lambda (x) (format "%S" x)) lst)
                   (cl-map 'string fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-maplist ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) (should (consp x)) x))
        (fn2 (lambda (x y) (should (and (consp x) (consp y))) y))
        (fn3 (lambda (x y z) (should (and (consp x) (consp y) (consp z))) z)))
    (should (equal (list lst (cdr lst) (cddr lst))
                   (cl-maplist fn1 lst)))
    (should (equal (list lst2 (cdr lst2) (cddr lst2))
                   (cl-maplist fn2 lst lst2)))
    (should (equal (list lst3 (cdr lst3) (cddr lst3))
                   (cl-maplist fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-cl-make-random-state ()
  (let ((s (cl-make-random-state)))
    ;; Test for Bug#33731.
    (should-not (eq s (cl-make-random-state s)))))

(ert-deftest cl-concatenate ()
  (should (equal (cl-concatenate 'list '(1 2 3) '(4 5 6))
                 '(1 2 3 4 5 6)))
  (should (equal (cl-concatenate 'vector [1 2 3] [4 5 6])
                 [1 2 3 4 5 6]))
  (should (equal (cl-concatenate 'string "123" "456")
                 "123456"))
  (should (equal (cl-concatenate 'list '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6))))

(ert-deftest cl-extra-test-mapcan ()
  (should (equal (cl-mapcan #'list '(1 2 3)) '(1 2 3)))
  (should (equal (cl-mapcan #'list '(1 2 3) '(4 5 6)) '(1 4 2 5 3 6)))
  (should (equal (cl-mapcan #'list '(1 2) '(3 4 5)) '(1 3 2 4)))
  (should (equal (cl-mapcan #'list '(1 2 3) "#$%") '(1 ?# 2 ?$ 3 ?%)))
  (should (equal (cl-mapcan #'list '()) '()))
  (should (equal (cl-mapcan #'list '() '()) '())))

(ert-deftest cl-extra-test-mapcon ()
  (should (equal (cl-mapcon #'list '(1 2 3)) '((1 2 3) (2 3) (3))))
  (should (equal (cl-mapcon #'list '()) nil))
  (should (equal (cl-mapcon #'list '() '()) nil)))

(ert-deftest cl-extra-test-some ()
  (should (equal (cl-some #'identity (list nil nil "foo")) "foo"))
  (should (equal (cl-some #'identity [nil nil nil]) nil))
  (should (equal (cl-some (lambda (a b) (> (+ a b) 198)) (list ?a ?b ?c) "abcz") nil))
  (should (equal (cl-some (lambda (a b) (> (+ a b) 198)) (list ?a ?b ?c) "abz") t)))

(ert-deftest cl-extra-test-every ()
  (should (equal (cl-every #'identity (list t 42 "foo")) t))
  (should (equal (cl-every #'identity [t nil "foo"]) nil))
  (should (equal (cl-every (lambda (a b) (<= (+ a b) 198))
                           (list ?a ?b ?c) "abcz")
                 t))
  (should (equal (cl-every (lambda (a b) (<= (+ a b) 198))
                           (list ?a ?b ?c) "abz")
                 nil)))

(ert-deftest cl-extra-test-notany ()
  (should (equal (cl-notany #'oddp '(1 3 5)) nil))
  (should (equal (cl-notany #'oddp '(2 4 6)) t))
  (should (equal (cl-notany #'oddp '(1 2 3 4 5)) nil)))

(ert-deftest cl-extra-test-notevery ()
  (should (equal (cl-notevery #'oddp '(1 3 5)) nil))
  (should (equal (cl-notevery #'oddp '(2 4 6)) t))
  (should (equal (cl-notevery #'oddp '(1 2 3 4 5)) t)))

(ert-deftest cl-extra-test-gcd ()
  (should (equal (cl-gcd 4) 4))
  (should (equal (cl-gcd 3 5) 1))
  (should (equal (cl-gcd 4 8) 4))
  (should (equal (cl-gcd 3 5 7) 1))
  (should (equal (cl-gcd 4 8 12) 4))
  (should (equal (cl-gcd 0) 0))
  (should (equal (cl-gcd 4 0) 4))
  (should (equal (cl-gcd 0 0) 0)))

(ert-deftest cl-extra-test-lcm ()
  (should (equal (cl-lcm 4) 4))
  (should (equal (cl-lcm 3 5) 15))
  (should (equal (cl-lcm 4 8) 8))
  (should (equal (cl-lcm 3 5 7) 105))
  (should (equal (cl-lcm 4 8 12) 24))
  (should (equal (cl-lcm 0 4) 0))
  (should (equal (cl-lcm 0 0) 0))
  (should (equal (cl-lcm) 1)))

(ert-deftest cl-extra-test-isqrt ()
  (should (equal (cl-isqrt 4) 2))
  (should (equal (cl-isqrt 100) 10))
  (should (equal (cl-isqrt 1) 1))
  (should (equal (cl-isqrt 0) 0))
  (should (equal (cl-isqrt 3) 1))
  (should (equal (cl-isqrt 10) 3))
  (should-error (cl-isqrt -4) :type 'arith-error)
  (should-error (cl-isqrt 2.5) :type 'arith-error))

(ert-deftest cl-extra-test-floor ()
  (should (equal (cl-floor 4.5) '(4 0.5)))
  (should (equal (cl-floor 10 3) '(3 1))))

(ert-deftest cl-extra-test-ceiling ()
  (should (equal (cl-ceiling 4.5) '(5 -0.5)))
  (should (equal (cl-ceiling 10 3) '(4 -2))))

(ert-deftest cl-extra-test-truncate ()
  (should (equal (cl-truncate 4.5) '(4 0.5)))
  (should (equal (cl-truncate 10 3) '(3 1))))

(ert-deftest cl-extra-test-round ()
  (should (equal (cl-round 4.5) '(4 0.5)))
  (should (equal (cl-round 10 3) '(3 1)))
  (should (equal (cl-round 1.5) '(2 -0.5)))
  (should (equal (cl-round 2.5) '(2 0.5))))

(ert-deftest cl-extra-test-mod ()
  (should (equal (cl-mod 10 3) 1))
  (should (equal (cl-mod -10 -3) -1))
  (should (equal (cl-mod -10 3) 2))
  (should (equal (cl-mod 10 -3) -2)))

(ert-deftest cl-extra-test-rem ()
  (should (equal (cl-rem 10 3) 1))
  (should (equal (cl-rem -10 -3) -1))
  (should (equal (cl-rem -10 3) -1))
  (should (equal (cl-rem 10 -3) 1)))

(ert-deftest cl-extra-test-signum ()
  (should (equal (cl-signum 10) 1))
  (should (equal (cl-signum -10) -1))
  (should (equal (cl-signum 0) 0)))

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

(ert-deftest cl-extra-test-parse-integer ()
  (should (equal (cl-parse-integer "10") 10))
  (should (equal (cl-parse-integer "-10") -10))
  (should (equal (cl-parse-integer "+10") 10))
  (should (equal (cl-parse-integer "ff" :radix 16) 255))
  (should (equal (cl-parse-integer "11" :start 1) 1))
  (should (equal (cl-parse-integer "abc def" :end 3 :junk-allowed t) nil)))

(ert-deftest cl-extra-test-subseq ()
  (should (equal (cl-subseq "hello" 1) "ello"))
  (should (equal (cl-subseq "hello" 1 4) "ell"))
  (should (equal (cl-subseq "hello" -1) "o"))
  (should (equal (cl-subseq "hello world" -5 -1) "worl"))
  (should (equal (cl-subseq '(1 2 3 4 5) 2) '(3 4 5)))
  (should (equal (cl-subseq '(1 2 3 4 5) 1 3) '(2 3))))

(ert-deftest cl-extra-test-revappend ()
  (should (equal (cl-revappend '(1 2 3) '(4 5 6)) '(3 2 1 4 5 6))))

(ert-deftest cl-extra-test-nreconc ()
  (should (equal (cl-nreconc (list 1 2 3) '(4 5 6)) '(3 2 1 4 5 6))))

(ert-deftest cl-extra-test-list-length ()
  (should (equal (cl-list-length '(1 2 3)) 3))
  (should (equal (cl-list-length '()) 0))
  (let ((xl (number-sequence 1 100)))
    (nconc xl xl)
    (should (equal (cl-list-length xl) nil))))

(ert-deftest cl-extra-test-tailp ()
  (let ((l '(1 2 3 4 5)))
    (should (cl-tailp (nthcdr 2 l) l))
    (should (cl-tailp l l))
    (should (not (cl-tailp '(4 5) l)))))

;;;; Method dispatch for derived types.

(cl-deftype multiples-of (&optional m)
  (let ((multiplep (if (memq m '(nil *))
                       #'ignore
		     (lambda (n) (= 0 (% n m))))))
    `(and integer (satisfies ,multiplep))))

(cl-deftype multiples-of-2 ()
  '(multiples-of 2))

(cl-deftype multiples-of-3 ()
  '(multiples-of 3))

(cl-deftype multiples-of-4 ()
  (declare (parents multiples-of-2))
  '(and multiples-of-2 (multiples-of 4)))

(cl-deftype unsigned-byte (&optional bits)
  "Unsigned integer."
  `(integer 0 ,(if (memq bits '(nil *)) bits (1- (ash 1 bits)))))

(cl-deftype unsigned-16bits ()
  "Unsigned 16-bits integer."
  (declare (parents unsigned-byte))
  '(unsigned-byte 16))

(cl-deftype unsigned-8bits ()
  "Unsigned 8-bits integer."
  (declare (parents unsigned-16bits))
  '(unsigned-byte 8))

(cl-defmethod my-foo ((_n unsigned-byte))
  (format "unsigned"))

(cl-defmethod my-foo ((_n unsigned-16bits))
  (format "unsigned 16bits - also %s"
          (cl-call-next-method)))

(cl-defmethod my-foo ((_n unsigned-8bits))
  (format "unsigned 8bits - also %s"
          (cl-call-next-method)))

(ert-deftest cl-types-test ()
  "Test types definition, specializers, and method dispatching."

  ;; Invalid DAG error
  ;; FIXME: We don't test that any more.
  ;; (should-error
  ;;  (eval
  ;;   '(cl-deftype unsigned-16bits ()
  ;;      "Unsigned 16-bits integer."
  ;;      (declare (parents unsigned-8bits))
  ;;      '(unsigned-byte 16))
  ;;   lexical-binding
  ;;   ))

  ;; With possible 'types' in multiples-of-{2,3,4} verify that:
  (let ((types '(multiples-of-2 multiples-of-3 multiples-of-4)))

    ;; (cl--derived-type-specializers  2 types) is multiples-of-2
    (should (equal '(multiples-of-2)
		   (seq-intersection
                    (cl--derived-type-specializers 2 types) types)))

    ;; (cl--derived-type-specializers  4 types) is multiples-of-{4,2}
    (should (equal '(multiples-of-4 multiples-of-2)
		   (seq-intersection
                    (cl--derived-type-specializers 4 types) types)))

    ;; (cl--derived-type-specializers  6 types) is multiples-of-{3,2}
    (should (equal '(multiples-of-3 multiples-of-2)
		   (seq-intersection
                    (cl--derived-type-specializers 6 types) types)))

    ;; (cl--derived-type-specializers 12 types) is multiples-of-{4,3,2}
    (should (member (seq-intersection
                     (cl--derived-type-specializers 12 types) types)
		    ;; Order between 3 and 4/2 is undefined.
		    '((multiples-of-3 multiples-of-4 multiples-of-2)
		      (multiples-of-4 multiples-of-2 multiples-of-3))))

    ;; (cl--derived-type-specializers  5 types) is not of any 'types'
    (should (equal '()
		   (seq-intersection
                    (cl--derived-type-specializers 5 types) types)))
    )

  ;;; Method dispatching.
  (should (equal "unsigned 8bits - also unsigned 16bits - also unsigned"
		 (my-foo 100)))

  (should (equal "unsigned 16bits - also unsigned"
		 (my-foo 256)))

  (should (equal "unsigned"
		 (my-foo most-positive-fixnum)))
  )

(ert-deftest cl-extra-test-random ()
  (should-error (cl-random -1))
  (should-error (cl-random -0.5))
  (should-error (cl-random -1.0e+INF))
  (should-error (cl-random 0))
  (should-error (cl-random 0.0))
  (should-error (cl-random -0.0))
  (should-error (cl-random 1.0e+INF))
  (should (eql (cl-random 1) 0)))


;;; cl-extra-tests.el ends here
