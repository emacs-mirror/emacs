;;; map-tests.el --- Tests for map.el  -*- lexical-binding:t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Maintainer: emacs-devel@gnu.org

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

;; Tests for map.el.

;;; Code:

(require 'ert)
(require 'map)

(eval-when-compile
  (require 'cl-lib))

(defmacro with-maps-do (var &rest body)
  "Successively bind VAR to an alist, plist, vector, and hash-table.
Each map is built from the following alist data:
  ((0 . 3) (1 . 4) (2 . 5))
Evaluate BODY for each created map."
  (declare (indent 1) (debug (symbolp body)))
  (let ((alist (make-symbol "alist"))
        (plist (make-symbol "plist"))
        (vec (make-symbol "vec"))
        (ht (make-symbol "ht")))
   `(let ((,alist (list (cons 0 3)
                        (cons 1 4)
                        (cons 2 5)))
          (,plist (list 0 3 1 4 2 5))
          (,vec (vector 3 4 5))
          (,ht (make-hash-table)))
      (puthash 0 3 ,ht)
      (puthash 1 4 ,ht)
      (puthash 2 5 ,ht)
      (dolist (,var (list ,alist ,plist ,vec ,ht))
        ,@body))))

(defmacro with-empty-maps-do (var &rest body)
  "Like `with-maps-do', but with empty maps."
  (declare (indent 1) (debug (symbolp body)))
  `(dolist (,var (list (list) (vector) (make-hash-table)))
     ,@body))

(ert-deftest test-map-plist-p ()
  "Test `map--plist-p'."
  (with-empty-maps-do map
    (should-not (map--plist-p map)))
  (should-not (map--plist-p ""))
  (should-not (map--plist-p '((()))))
  (should (map--plist-p '(:a)))
  (should (map--plist-p '(a)))
  (should (map--plist-p '(nil)))
  (should (map--plist-p '(""))))

(ert-deftest test-map-elt ()
  (with-maps-do map
    (should (= 3 (map-elt map 0)))
    (should (= 4 (map-elt map 1)))
    (should (= 5 (map-elt map 2)))
    (should-not (map-elt map -1))
    (should-not (map-elt map 4))
    (should-not (map-elt map 0.1))))

(ert-deftest test-map-elt-default ()
  (with-maps-do map
    (should (= 5 (map-elt map 7 5)))
    (should (= 5 (map-elt map 0.1 5))))
  (with-empty-maps-do map
    (should (= 5 (map-elt map 0 5)))))

(ert-deftest test-map-elt-testfn-alist ()
  "Test the default alist predicate of `map-elt'."
  (let* ((a (string ?a))
         (map `((,a . 0) (,(string ?b) . 1))))
    (should (= 0 (map-elt map a)))
    (should (= 0 (map-elt map "a")))
    (should (= 0 (map-elt map (string ?a))))
    (should (= 1 (map-elt map "b")))
    (should (= 1 (map-elt map (string ?b))))
    (with-suppressed-warnings ((callargs map-elt))
      (should (= 0 (map-elt map 'a nil #'string=)))
      (should (= 1 (map-elt map 'b nil #'string=))))))

(ert-deftest test-map-elt-testfn-plist ()
  "Test the default plist predicate of `map-elt'."
  (let* ((a (string ?a))
         (map `(,a 0 "b" 1)))
    (should-not (map-elt map "a"))
    (should-not (map-elt map "b"))
    (should-not (map-elt map (string ?a)))
    (should-not (map-elt map (string ?b)))
    (should (= 0 (map-elt map a)))
    (with-suppressed-warnings ((callargs map-elt))
      (should (= 0 (map-elt map a nil #'equal)))
      (should (= 0 (map-elt map "a" nil #'equal)))
      (should (= 0 (map-elt map (string ?a) nil #'equal)))
      (should (= 1 (map-elt map "b" nil #'equal)))
      (should (= 1 (map-elt map (string ?b) nil #'equal))))))

(ert-deftest test-map-elt-gv ()
  "Test the generalized variable `map-elt'."
  (let ((sort (lambda (map) (sort (map-pairs map) #'car-less-than-car))))
    (with-empty-maps-do map
      ;; Empty map, without default.
      (should-error (incf (map-elt map 1)) :type 'wrong-type-argument)
      (with-suppressed-warnings ((callargs map-elt))
        (should-error (incf (map-elt map 1.0 nil #'=))
                      :type 'wrong-type-argument))
      (should (map-empty-p map))
      ;; Empty map, with default.
      (if (vectorp map)
          (progn
            (should-error (incf (map-elt map 1 3)) :type 'args-out-of-range)
            (with-suppressed-warnings ((callargs map-elt))
              (should-error (incf (map-elt map 1 3 #'=))
                            :type 'args-out-of-range))
            (should (map-empty-p map)))
        (should (= (incf (map-elt map 1 3) 10) 13))
        (with-suppressed-warnings ((callargs map-elt))
          (should (= (incf (map-elt map 2.0 5 #'=) 12) 17)))
        (should (equal (funcall sort map) '((1 . 13) (2.0 . 17))))))
    (with-maps-do map
      ;; Nonempty map, without predicate.
      (should (= (incf (map-elt map 1 3) 10) 14))
      (should (equal (funcall sort map) '((0 . 3) (1 . 14) (2 . 5))))
      ;; Nonempty map, with predicate.
      (with-suppressed-warnings ((callargs map-elt))
        (pcase-exhaustive map
          ((pred consp)
           (should (= (incf (map-elt map 2.0 6 #'=) 12) 17))
           (should (equal (funcall sort map) '((0 . 3) (1 . 14) (2 . 17))))
           (should (= (incf (map-elt map 0 7 #'=) 13) 16))
           (should (equal (funcall sort map) '((0 . 16) (1 . 14) (2 . 17)))))
          ((pred vectorp)
           (should-error (incf (map-elt map 2.0 6 #'=))
                         :type 'wrong-type-argument)
           (should (equal (funcall sort map) '((0 . 3) (1 . 14) (2 . 5))))
           (should (= (incf (map-elt map 2 6 #'=) 12) 17))
           (should (equal (funcall sort map) '((0 . 3) (1 . 14) (2 . 17))))
           (should (= (incf (map-elt map 0 7 #'=) 13) 16))
           (should (equal (funcall sort map) '((0 . 16) (1 . 14) (2 . 17)))))
          ((pred hash-table-p)
           (should (= (incf (map-elt map 2.0 6 #'=) 12) 18))
           (should (member (funcall sort map)
                           '(((0 . 3) (1 . 14) (2 . 5) (2.0 . 18))
                             ((0 . 3) (1 . 14) (2.0 . 18) (2 . 5)))))
           (should (= (incf (map-elt map 0 7 #'=) 13) 16))
           (should (member (funcall sort map)
                           '(((0 . 16) (1 . 14) (2 . 5) (2.0 . 18))
                             ((0 . 16) (1 . 14) (2.0 . 18) (2 . 5)))))))))))

(ert-deftest test-map-elt-with-nil-value ()
  (should-not (map-elt '((a . 1) (b)) 'b 2)))

(ert-deftest test-map-elt-signature ()
  "Test that `map-elt' has the right advertised signature.
See bug#58531#25 and bug#58563."
  (should (equal (get-advertised-calling-convention (symbol-function 'map-elt))
                 '(map key &optional default))))

(ert-deftest test-map-put! ()
  (with-maps-do map
    (setf (map-elt map 2) 'hello)
    (should (eq (map-elt map 2) 'hello)))
  (with-maps-do map
    (with-suppressed-warnings ((obsolete map-put))
      (map-put map 2 'hello))
    (should (eq (map-elt map 2) 'hello)))
  (with-maps-do map
    (map-put! map 2 'hello)
    (should (eq (map-elt map 2) 'hello))
    (if (not (or (hash-table-p map)
                 (map--plist-p map)))
        (should-error (map-put! map 5 'value)
                      ;; For vectors, it could arguably signal
                      ;; map-not-inplace as well, but it currently doesn't.
                      :type (if (listp map)
                                'map-not-inplace
                              'error))
      (map-put! map 5 'value)
      (should (eq (map-elt map 5) 'value)))))

(ert-deftest test-map-put!-new-keys ()
  "Test `map-put!' with new keys."
  (with-maps-do map
    (let ((size (map-length map)))
      (if (arrayp map)
          (progn
            (should-error (setf (map-elt map 'k) 'v))
            (should-error (setf (map-elt map size) 'v)))
        (setf (map-elt map 'k) 'v)
        (should (eq (map-elt map 'k) 'v))
        (setf (map-elt map size) 'v)
        (should (eq (map-elt map size) 'v))))))

(ert-deftest test-map-put!-alist ()
  "Test `map-put!' test function on alists."
  (let ((key (string ?a))
        (val 0)
        map)
    (should-error (map-put! map key val) :type 'map-not-inplace)
    (setq map (list (cons key val)))
    (map-put! map key (1- val))
    (should (equal map '(("a" . -1))))
    (map-put! map (string ?a) (1+ val))
    (should (equal map '(("a" . 1))))
    (should-error (map-put! map (string ?a) val #'eq) :type 'map-not-inplace)))

(ert-deftest test-map-put!-plist ()
  "Test `map-put!' predicate on plists."
  (let* ((a (string ?a))
         (map (list a 0)))
    (map-put! map a -1)
    (should (equal map '("a" -1)))
    (map-put! map 'a 2)
    (should (equal map '("a" -1 a 2)))
    (with-suppressed-warnings ((callargs map-put!))
      (map-put! map 'a -3 #'string=))
    (should (equal map '("a" -3 a 2)))))

(ert-deftest test-map-put!-signature ()
  "Test that `map-put!' has the right advertised signature.
See bug#58531#25 and bug#58563."
  (should (equal (get-advertised-calling-convention (symbol-function 'map-put!))
                 '(map key value))))

(ert-deftest test-map-put-alist-new-key ()
  "Regression test for Bug#23105."
  (let ((alist (list (cons 0 'a))))
    (with-suppressed-warnings ((obsolete map-put))
      (map-put alist 2 'b))
    (should (eq (map-elt alist 2) 'b))))

(ert-deftest test-map-put-testfn-alist ()
  (let ((alist (list (cons "a" 1) (cons "b" 2)))
        ;; Make sure to use a non-eq "a", even when compiled.
        (noneq-key (string ?a)))
    (with-suppressed-warnings ((obsolete map-put))
      (map-put alist noneq-key 3 #'equal)
      (should-not (cddr alist))
      (map-put alist noneq-key 9 #'eql)
      (should (cddr alist)))))

(ert-deftest test-map-put-return-value ()
  (let ((ht (make-hash-table)))
    (with-suppressed-warnings ((obsolete map-put))
      (should (eq (map-put ht 'a 'hello) 'hello)))))

(ert-deftest test-map-insert-empty ()
  "Test `map-insert' on empty maps."
  (with-empty-maps-do map
    (if (arrayp map)
        (should-error (map-insert map 0 6))
      (let ((new (map-insert map 0 6)))
        (should-not (eq map new))
        (should-not (map-pairs map))
        (should (= (map-elt new 0) 6))))))

(ert-deftest test-map-insert ()
  "Test `map-insert'."
  (with-maps-do map
    (let ((pairs (map-pairs map))
          (size (map-length map))
          (new (map-insert map 0 6)))
      (should-not (eq map new))
      (should (equal (map-pairs map) pairs))
      (should (= (map-elt new 0) 6))
      (if (arrayp map)
          (should-error (map-insert map size 7))
        (setq new (map-insert map size 7))
        (should-not (eq map new))
        (should (equal (map-pairs map) pairs))
        (should (= (map-elt new size) 7))))))

(ert-deftest test-map-delete ()
  (with-maps-do map
    (should (map-elt map 1))
    (should (eq map (map-delete map 1)))
    (should-not (map-elt map 1)))
  (with-maps-do map
    (should-not (map-elt map -2))
    (should (eq map (map-delete map -2)))
    (should-not (map-elt map -2)))
  (with-maps-do map
    ;; Check for OBOE.
    (let ((key (map-length map)))
      (should-not (map-elt map key))
      (should (eq map (map-delete map key)))
      (should-not (map-elt map key)))))

(ert-deftest test-map-delete-empty ()
  (with-empty-maps-do map
    (should (eq map (map-delete map t)))))

(ert-deftest test-map-delete-alist ()
  "Test `map-delete' test function on alists."
  (let* ((a (string ?a))
         (map `((,a) (,(string ?b)))))
    (setq map (map-delete map a))
    (should (equal map '(("b"))))
    (setq map (map-delete map (string ?b)))
    (should-not map)))

(ert-deftest test-map-nested-elt ()
  (let ((vec [a b [c d [e f]]]))
    (should (eq (map-nested-elt vec '(2 2 0)) 'e)))
  (let ((alist '((a . 1)
                 (b . ((c . 2)
                       (d . 3)
                       (e . ((f . 4)
                             (g . 5))))))))
    (should (eq (map-nested-elt alist '(b e f)) 4)))
  (let ((plist '(a 1 b (c 2 d 3 e (f 4 g 5)))))
    (should (eq (map-nested-elt plist '(b e f)) 4)))
  (let ((ht (make-hash-table)))
    (setf (map-elt ht 'a) 1)
    (setf (map-elt ht 'b) (make-hash-table))
    (setf (map-elt (map-elt ht 'b) 'c) 2)
    (should (eq (map-nested-elt ht '(b c))
                2))))

(ert-deftest test-map-nested-elt-default ()
  (let ((vec [a b [c d]]))
    (should-not (map-nested-elt vec '(2 3)))
    (should-not (map-nested-elt vec '(2 1 1)))
    (should (= 4 (map-nested-elt vec '(2 1 1) 4)))))

(ert-deftest test-mapp ()
  (with-empty-maps-do map
    (should (mapp map)))
  (with-maps-do map
    (should (mapp map)))
  (should (mapp ""))
  (should (mapp "hello"))
  (should-not (mapp 1))
  (should-not (mapp 'hello)))

(ert-deftest test-map-keys ()
  (with-maps-do map
    (should (equal (map-keys map) '(0 1 2))))
  (with-empty-maps-do map
    (should-not (map-keys map))))

(ert-deftest test-map-values ()
  (with-maps-do map
    (should (equal (map-values map) '(3 4 5))))
  (with-empty-maps-do map
    (should-not (map-values map))))

(ert-deftest test-map-pairs ()
  (with-maps-do map
    (should (equal (map-pairs map)
                   '((0 . 3)
                     (1 . 4)
                     (2 . 5)))))
  (with-empty-maps-do map
    (should-not (map-pairs map))))

(ert-deftest test-map-length ()
  (with-empty-maps-do map
    (should (zerop (map-length map))))
  (with-maps-do map
    (should (= 3 (map-length map))))
  (should (= 1 (map-length '(nil 1))))
  (should (= 2 (map-length '(nil 1 t 2))))
  (should (= 2 (map-length '((a . 1) (b . 2)))))
  (should (= 5 (map-length [0 1 2 3 4])))
  (should (= 4 (map-length #s(hash-table data (a 1 b 2 c 3 d 4))))))

(ert-deftest test-map-copy ()
  (with-maps-do map
    (let ((copy (map-copy map)))
      (should (equal (map-pairs map) (map-pairs copy)))
      (should-not (eq map copy))
      (map-put! map 0 0)
      (should-not (equal (map-pairs map) (map-pairs copy)))))
  (with-empty-maps-do map
    (should-not (map-pairs (map-copy map)))))

(ert-deftest test-map-copy-alist ()
  "Test use of `copy-alist' for alists."
  (let* ((cons (list 'a 1 2))
         (alist (list cons))
         (copy (map-copy alist)))
    (setcar cons 'b)
    (should (equal alist '((b 1 2))))
    (should (equal copy '((a 1 2))))
    (setcar (cdr cons) 0)
    (should (equal alist '((b 0 2))))
    (should (equal copy '((a 0 2))))
    (setcdr cons 3)
    (should (equal alist '((b . 3))))
    (should (equal copy '((a 0 2))))))

(ert-deftest test-map-apply ()
  (let ((fn (lambda (k v) (cons (number-to-string k) v))))
    (with-maps-do map
      (should (equal (map-apply fn map)
                     '(("0" . 3) ("1" . 4) ("2" . 5)))))
    (with-empty-maps-do map
      (should-not (map-apply fn map)))))

(ert-deftest test-map-do ()
  (let* (res
         (fn (lambda (k v)
               (push (list (number-to-string k) v) res))))
    (with-empty-maps-do map
      (should-not (map-do fn map))
      (should-not res))
    (with-maps-do map
      (setq res nil)
      (should-not (map-do fn map))
      (should (equal res '(("2" 5) ("1" 4) ("0" 3)))))))

(ert-deftest test-map-keys-apply ()
  (with-maps-do map
    (should (equal (map-keys-apply #'1+ map) '(1 2 3))))
  (with-empty-maps-do map
    (let (ks)
      (should-not (map-keys-apply (lambda (k) (push k ks)) map))
      (should-not ks))))

(ert-deftest test-map-values-apply ()
  (with-maps-do map
    (should (equal (map-values-apply #'1+ map) '(4 5 6))))
  (with-empty-maps-do map
    (let (vs)
      (should-not (map-values-apply (lambda (v) (push v vs)) map))
      (should-not vs))))

(ert-deftest test-map-filter ()
  (with-maps-do map
    (should (equal (map-filter (lambda (_k v) (> v 3)) map)
                   '((1 . 4) (2 . 5))))
    (should (equal (map-filter #'always map) (map-pairs map)))
    (should-not (map-filter #'ignore map)))
  (with-empty-maps-do map
    (should-not (map-filter #'always map))
    (should-not (map-filter #'ignore map))))

(ert-deftest test-map-remove ()
  (with-maps-do map
    (should (equal (map-remove (lambda (_k v) (> v 3)) map)
                   '((0 . 3))))
    (should (equal (map-remove #'ignore map) (map-pairs map)))
    (should-not (map-remove #'always map)))
  (with-empty-maps-do map
    (should-not (map-remove #'always map))
    (should-not (map-remove #'ignore map))))

(ert-deftest test-map-empty-p ()
  (with-empty-maps-do map
    (should (map-empty-p map)))
  (should (map-empty-p ""))
  (should-not (map-empty-p '((a . b) (c . d))))
  (should-not (map-empty-p [1 2 3]))
  (should-not (map-empty-p "hello")))

(ert-deftest test-map-contains-key ()
  (with-empty-maps-do map
    (should-not (map-contains-key map -1))
    (should-not (map-contains-key map 0))
    (should-not (map-contains-key map 1))
    (should-not (map-contains-key map (map-length map))))
  (with-maps-do map
    (should-not (map-contains-key map -1))
    (should (map-contains-key map 0))
    (should (map-contains-key map 1))
    (should-not (map-contains-key map (map-length map)))))

(ert-deftest test-map-contains-key-testfn ()
  "Test `map-contains-key' under different equalities."
  (let ((key (string ?a))
        (plist '("a" 1 a 2))
        (alist '(("a" . 1) (a . 2))))
    (should (map-contains-key alist 'a))
    (should (map-contains-key plist 'a))
    ;; FIXME: Why is no warning emitted for these (bug#58563#13)?
    (should (map-contains-key alist 'a #'eq))
    (should (map-contains-key plist 'a #'eq))
    (should (map-contains-key alist key))
    (should (map-contains-key alist "a"))
    (should (map-contains-key plist (string ?a) #'equal))
    (should-not (map-contains-key plist key))
    (should-not (map-contains-key alist key #'eq))
    (should-not (map-contains-key plist key #'eq))))

(ert-deftest test-map-contains-key-signature ()
  "Test that `map-contains-key' has the right advertised signature.
See bug#58531#25 and bug#58563."
  (should (equal (get-advertised-calling-convention
                  (symbol-function 'map-contains-key))
                 '(map key))))

(ert-deftest test-map-some ()
  (with-maps-do map
    (should (eq (map-some (lambda (k _v) (and (= k 1) 'found)) map)
                'found))
    (should-not (map-some #'ignore map)))
  (with-empty-maps-do map
    (should-not (map-some #'always map))
    (should-not (map-some #'ignore map))))

(ert-deftest test-map-every-p ()
  (with-maps-do map
    (should (map-every-p #'always map))
    (should-not (map-every-p #'ignore map))
    (should-not (map-every-p (lambda (k _v) (zerop k)) map)))
  (with-empty-maps-do map
    (should (map-every-p #'always map))
    (should (map-every-p #'ignore map))
    (should (map-every-p (lambda (k _v) (zerop k)) map))))

(ert-deftest test-map-into ()
  (let* ((plist '(a 1 b 2))
         (alist '((a . 1) (b . 2)))
         (ht (map-into alist 'hash-table))
         (ht2 (map-into alist '(hash-table :test equal))))
    (should (hash-table-p ht))
    (should (equal (map-into ht 'list) alist))
    (should (equal (map-pairs (map-into (map-into ht 'list) 'hash-table))
                   (map-pairs ht)))
    (should (equal (map-into ht 'alist) (map-into ht2 'alist)))
    (should (equal (map-into alist 'list) alist))
    (should (equal (map-into alist 'alist) alist))
    (should (equal (map-into alist 'plist) plist))
    (should (equal (map-into plist 'alist) alist))
    (should (equal (map-into plist 'plist) plist)))
  (should-error (map-into [1 2 3] 'string) :type 'cl-no-applicable-method))

(ert-deftest test-map-into-hash-test ()
  "Test `map-into' with different hash-table test functions."
  (should (eq (hash-table-test (map-into () 'hash-table)) #'equal))
  (should (eq (hash-table-test (map-into () '(hash-table))) #'eql))
  (should (eq (hash-table-test (map-into () '(hash-table :test eq))) #'eq))
  (should (eq (hash-table-test (map-into () '(hash-table :test eql))) #'eql))
  (should (eq (hash-table-test (map-into () '(hash-table :test equal)))
              #'equal)))

(ert-deftest test-map-into-empty ()
  "Test `map-into' with empty maps."
  (with-empty-maps-do map
    (should-not (map-into map 'list))
    (should-not (map-into map 'alist))
    (should-not (map-into map 'plist))
    (should (map-empty-p (map-into map 'hash-table)))))

(ert-deftest test-map-let ()
  (map-let (foo bar baz) '((foo . 1) (bar . 2))
    (should (= foo 1))
    (should (= bar 2))
    (should-not baz))
  (map-let (('foo a)
            ('bar b)
            ('baz c))
      '((foo . 1) (bar . 2))
    (should (= a 1))
    (should (= b 2))
    (should-not c)))

(ert-deftest test-map-let-default ()
  (map-let (('foo a 3)
            ('baz b 4))
      '((foo . 1))
    (should (equal a 1))
    (should (equal b 4))))

(ert-deftest test-map-merge ()
  "Test `map-merge'."
  (should (equal (sort (map-merge 'list '(a 1) '((b . 2) (c . 3))
                                  #s(hash-table data (c 4)))
                       (lambda (x y) (string< (car x) (car y))))
                 '((a . 1) (b . 2) (c . 4))))
  (should (equal (map-merge 'list () '(:a 1)) '((:a . 1))))
  (should (equal (map-merge 'alist () '(:a 1)) '((:a . 1))))
  (should (equal (map-merge 'plist () '(:a 1)) '(:a 1))))

(ert-deftest test-map-merge-with ()
  (should (equal (sort (map-merge-with 'list #'+
                                       '((1 . 2))
                                       '((1 . 3) (2 . 4))
                                       '((1 . 1) (2 . 5) (3 . 0)))
                       #'car-less-than-car)
                 '((1 . 6) (2 . 9) (3 . 0))))
  (should (equal (map-merge-with 'list #'+ () '(:a 1)) '((:a . 1))))
  (should (equal (map-merge-with 'alist #'+ () '(:a 1)) '((:a . 1))))
  (should (equal (map-merge-with 'plist #'+ () '(:a 1)) '(:a 1))))

(ert-deftest test-map-merge-empty ()
  "Test merging of empty maps."
  (should-not (map-merge 'list))
  (should-not (map-merge 'alist))
  (should-not (map-merge 'plist))
  (should-not (map-merge-with 'list #'+))
  (should-not (map-merge-with 'alist #'+))
  (should-not (map-merge-with 'plist #'+))
  (should (map-empty-p (map-merge 'hash-table)))
  (should (map-empty-p (map-merge-with 'hash-table #'+)))
  (should-error (map-merge 'array) :type 'cl-no-applicable-method)
  (should-error (map-merge-with 'array #'+) :type 'cl-no-applicable-method))

(ert-deftest test-map-plist-pcase ()
  (let ((plist '(:one 1 :two 2)))
    (should (equal (pcase-let (((map :one (:two two)) plist))
                     (list one two))
                   '(1 2)))))

(ert-deftest test-map-plist-pcase-default ()
  (let ((plist '(:two 2)))
    (should (equal (pcase-let (((map (:two two 33)
                                     (:three three 44))
                                plist))
                     (list two three))
                   '(2 44)))))

(ert-deftest test-map-pcase-matches ()
  (let ((plist '(:two 2)))
    (should (equal (pcase plist
                     ((map (:two two 33)
                           (:three three))
                      (list two three))
                     (_ 'fail))
                   '(2 nil)))

    (should (equal (pcase plist
                     ((map (:two two 33)
                           (:three three 44))
                      (list two three))
                     (_ 'fail))
                   '(2 44)))

    (should (equal (pcase plist
                     ((map (:two two 33)
                           (:three `(,a . ,b) '(11 . 22)))
                      (list two a b))
                     (_ 'fail))
                   '(2 11 22)))

    (should (equal 'fail
                   (pcase plist
                     ((map (:two two 33)
                           (:three `(,a . ,b) 44))
                      (list two a b))
                     (_ 'fail))))

    (should (equal 'fail
                   (pcase plist
                     ((map (:two two 33)
                           (:three `(,a . ,b) nil))
                      (list two a b))
                     (_ 'fail))))

    (should (equal 'fail
                   (pcase plist
                     ((map (:two two 33)
                           (:three `(,a . ,b)))
                      (list two a b))
                     (_ 'fail))))))

(ert-deftest test-map-setf-alist-insert-key ()
  (let ((alist))
    (should (equal (setf (map-elt alist 'key) 'value)
                   'value))
    (should (equal alist '((key . value))))))

(ert-deftest test-map-setf-alist-overwrite-key ()
  (let ((alist (list (cons 'key 'value1))))
    (should (equal (setf (map-elt alist 'key) 'value2)
                   'value2))
    (should (equal alist '((key . value2))))))

(ert-deftest test-map-setf-plist-insert-key ()
  (let ((plist (list 'key 'value)))
    (should (equal (setf (map-elt plist 'key2) 'value2)
                   'value2))
    (should (equal plist '(key value key2 value2)))))

(ert-deftest test-map-setf-plist-overwrite-key ()
  (let ((plist (list 'key 'value)))
    (should (equal (setf (map-elt plist 'key) 'value2)
                   'value2))
    (should (equal plist '(key value2)))))

(ert-deftest test-hash-table-setf-insert-key ()
  (let ((ht (make-hash-table)))
    (should (equal (setf (map-elt ht 'key) 'value)
                   'value))
    (should (equal (map-elt ht 'key) 'value))))

(ert-deftest test-hash-table-setf-overwrite-key ()
  (let ((ht (make-hash-table)))
    (puthash 'key 'value1 ht)
    (should (equal (setf (map-elt ht 'key) 'value2)
                   'value2))
    (should (equal (map-elt ht 'key) 'value2))))

(ert-deftest test-setf-map-with-function ()
  (let ((num 0)
        (map nil))
    (setf (map-elt map 'foo)
          (funcall (lambda ()
                     (incf num))))
    (should (equal map '((foo . 1))))
    ;; Check that the function is only called once.
    (should (= num 1))))

(ert-deftest test-map-plist-member ()
  "Test `map--plist-member' and `map--plist-member-1'."
  (dolist (mem '(map--plist-member map--plist-member-1))
    ;; Lambda exercises Lisp implementation.
    (dolist (= `(nil ,(lambda (a b) (eq a b))))
      (should-not (funcall mem () 'a =))
      (should-not (funcall mem '(a) 'b =))
      (should-not (funcall mem '(a 1) 'b =))
      (should (equal (funcall mem '(a) 'a =) '(a)))
      (should (equal (funcall mem '(a . 1) 'a =) '(a . 1)))
      (should (equal (funcall mem '(a 1 . b) 'a =) '(a 1 . b)))
      (should (equal (funcall mem '(a 1 b) 'a =) '(a 1 b)))
      (should (equal (funcall mem '(a 1 b) 'b =) '(b)))
      (should (equal (funcall mem '(a 1 b . 2) 'a =) '(a 1 b . 2)))
      (should (equal (funcall mem '(a 1 b . 2) 'b =) '(b . 2)))
      (should (equal (funcall mem '(a 1 b 2) 'a =) '(a 1 b 2)))
      (should (equal (funcall mem '(a 1 b 2) 'b =) '(b 2)))
      (should (equal (should-error (funcall mem '(a . 1) 'b =))
                     '(wrong-type-argument plistp (a . 1))))
      (should (equal (should-error (funcall mem '(a 1 . b) 'b =))
                     '(wrong-type-argument plistp (a 1 . b)))))
    (should (equal (funcall mem '(a 1 b 2) "a" #'string=) '(a 1 b 2)))
    (should (equal (funcall mem '(a 1 b 2) "b" #'string=) '(b 2)))))

(ert-deftest test-map-plist-put ()
  "Test `map--plist-put' and `map--plist-put-1'."
  (dolist (put '(map--plist-put map--plist-put-1))
    ;; Lambda exercises Lisp implementation.
    (dolist (= `(nil ,(lambda (a b) (eq a b))))
      (let ((l ()))
        (should (equal (funcall put l 'a 1 =) '(a 1)))
        (should-not l))
      (let ((l (list 'a)))
        (dolist (key '(a b))
          (should (equal (should-error (funcall put l key 1 =))
                         '(wrong-type-argument plistp (a)))))
        (should (equal l '(a))))
      (let ((l (cons 'a 1)))
        (dolist (key '(a b))
          (should (equal (should-error (funcall put l key 1 =))
                         '(wrong-type-argument plistp (a . 1)))))
        (should (equal l '(a . 1))))
      (let ((l (cons 'a (cons 1 'b))))
        (should (equal (funcall put l 'a 2 =) '(a 2 . b)))
        (dolist (key '(b c))
          (should (equal (should-error (funcall put l key 3 =))
                         '(wrong-type-argument plistp (a 2 . b)))))
        (should (equal l '(a 2 . b))))
      (let ((l (list 'a 1 'b)))
        (should (equal (funcall put l 'a 2 =) '(a 2 b)))
        (dolist (key '(b c))
          (should (equal (should-error (funcall put l key 3 =))
                         '(wrong-type-argument plistp (a 2 b)))))
        (should (equal l '(a 2 b))))
      (let ((l (cons 'a (cons 1 (cons 'b 2)))))
        (should (equal (funcall put l 'a 3 =) '(a 3 b . 2)))
        (dolist (key '(b c))
          (should (equal (should-error (funcall put l key 4 =))
                         '(wrong-type-argument plistp (a 3 b . 2)))))
        (should (equal l '(a 3 b . 2))))
      (let ((l (list 'a 1 'b 2)))
        (should (equal (funcall put l 'a 3 =) '(a 3 b 2)))
        (should (equal (funcall put l 'b 4 =) '(a 3 b 4)))
        (should (equal (funcall put l 'c 5 =) '(a 3 b 4 c 5)))
        (should (equal l '(a 3 b 4 c 5)))))
    (let ((l (list 'a 1 'b 2)))
      (should (equal (funcall put l "a" 3 #'string=) '(a 3 b 2)))
      (should (equal (funcall put l "b" 4 #'string=) '(a 3 b 4)))
      (should (equal (funcall put l "c" 5 #'string=) '(a 3 b 4 "c" 5))))))

(provide 'map-tests)
;;; map-tests.el ends here
