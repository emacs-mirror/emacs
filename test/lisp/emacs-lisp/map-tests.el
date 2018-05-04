;;; map-tests.el --- Tests for map.el  -*- lexical-binding:t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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

;; Tests for map.el

;;; Code:

(require 'ert)
(require 'map)

(defmacro with-maps-do (var &rest body)
  "Successively bind VAR to an alist, vector and hash-table.
Each map is built from the following alist data:
'((0 . 3) (1 . 4) (2 . 5)).
Evaluate BODY for each created map.

\(fn (var map) body)"
  (declare (indent 1) (debug (symbolp body)))
  (let ((alist (make-symbol "alist"))
        (vec (make-symbol "vec"))
        (ht (make-symbol "ht")))
   `(let ((,alist (list (cons 0 3)
                        (cons 1 4)
                        (cons 2 5)))
          (,vec (vector 3 4 5))
          (,ht (make-hash-table)))
      (puthash 0 3 ,ht)
      (puthash 1 4 ,ht)
      (puthash 2 5 ,ht)
      (dolist (,var (list ,alist ,vec ,ht))
        ,@body))))

(ert-deftest test-map-elt ()
  (with-maps-do map
    (should (= 3 (map-elt map 0)))
    (should (= 4 (map-elt map 1)))
    (should (= 5 (map-elt map 2)))
    (should (null (map-elt map -1)))
    (should (null (map-elt map 4)))))

(ert-deftest test-map-elt-default ()
  (with-maps-do map
    (should (= 5 (map-elt map 7 5)))))

(ert-deftest test-map-elt-testfn ()
  (let ((map (list (cons "a" 1) (cons "b" 2)))
        ;; Make sure to use a non-eq "a", even when compiled.
        (noneq-key (string ?a)))
    (should-not (map-elt map noneq-key))
    (should (map-elt map noneq-key nil 'equal))))

(ert-deftest test-map-elt-with-nil-value ()
  (should (null (map-elt '((a . 1)
                           (b))
                         'b
                         '2))))

(ert-deftest test-map-put ()
  (with-maps-do map
    (setf (map-elt map 2) 'hello)
    (should (eq (map-elt map 2) 'hello)))
  (with-maps-do map
    (map-put map 2 'hello)
    (should (eq (map-elt map 2) 'hello)))
  (let ((ht (make-hash-table)))
    (setf (map-elt ht 2) 'a)
    (should (eq (map-elt ht 2)
                'a)))
  (let ((alist '((0 . a) (1 . b) (2 . c))))
    (setf (map-elt alist 2) 'a)
    (should (eq (map-elt alist 2)
                'a)))
  (let ((vec [3 4 5]))
   (should-error (setf (map-elt vec 3) 6))))

(ert-deftest test-map-put-alist-new-key ()
  "Regression test for Bug#23105."
  (let ((alist '((0 . a))))
    (map-put alist 2 'b)
    (should (eq (map-elt alist 2)
                'b))))

(ert-deftest test-map-put-testfn-alist ()
  (let ((alist (list (cons "a" 1) (cons "b" 2)))
        ;; Make sure to use a non-eq "a", even when compiled.
        (noneq-key (string ?a)))
    (map-put alist noneq-key 3 'equal)
    (should-not (cddr alist))
    (map-put alist noneq-key 9)
    (should (cddr alist))))

(ert-deftest test-map-put-return-value ()
  (let ((ht (make-hash-table)))
    (should (eq (map-put ht 'a 'hello) 'hello))))

(ert-deftest test-map-delete ()
  (with-maps-do map
    (map-delete map 1)
    (should (null (map-elt map 1))))
  (with-maps-do map
    (map-delete map -2)
    (should (null (map-elt map -2)))))

(ert-deftest test-map-delete-return-value ()
  (let ((ht (make-hash-table)))
    (should (eq (map-delete ht 'a) ht))))

(ert-deftest test-map-nested-elt ()
  (let ((vec [a b [c d [e f]]]))
    (should (eq (map-nested-elt vec '(2 2 0)) 'e)))
  (let ((alist '((a . 1)
                 (b . ((c . 2)
                       (d . 3)
                       (e . ((f . 4)
                             (g . 5))))))))
    (should (eq (map-nested-elt alist '(b e f))
                4)))
  (let ((ht (make-hash-table)))
    (setf (map-elt ht 'a) 1)
    (setf (map-elt ht 'b) (make-hash-table))
    (setf (map-elt (map-elt ht 'b) 'c) 2)
    (should (eq (map-nested-elt ht '(b c))
                2))))

(ert-deftest test-map-nested-elt-default ()
  (let ((vec [a b [c d]]))
    (should (null (map-nested-elt vec '(2 3))))
    (should (null (map-nested-elt vec '(2 1 1))))
    (should (= 4 (map-nested-elt vec '(2 1 1) 4)))))

(ert-deftest test-mapp ()
  (should (mapp nil))
  (should (mapp '((a . b) (c . d))))
  (should (mapp '(a b c d)))
  (should (mapp []))
  (should (mapp [1 2 3]))
  (should (mapp (make-hash-table)))
  (should (mapp "hello"))
  (should (not (mapp 1)))
  (should (not (mapp 'hello))))

(ert-deftest test-map-keys ()
  (with-maps-do map
    (should (equal (map-keys map) '(0 1 2))))
  (should (null (map-keys nil)))
  (should (null (map-keys []))))

(ert-deftest test-map-values ()
  (with-maps-do map
    (should (equal (map-values map) '(3 4 5)))))

(ert-deftest test-map-pairs ()
  (with-maps-do map
    (should (equal (map-pairs map) '((0 . 3)
                                     (1 . 4)
                                     (2 . 5))))))

(ert-deftest test-map-length ()
  (let ((ht (make-hash-table)))
    (puthash 'a 1 ht)
    (puthash 'b 2 ht)
    (puthash 'c 3 ht)
    (puthash 'd 4 ht)
    (should (= 0 (map-length nil)))
    (should (= 0 (map-length [])))
    (should (= 0 (map-length (make-hash-table))))
    (should (= 5 (map-length [0 1 2 3 4])))
    (should (= 2 (map-length '((a . 1) (b . 2)))))
    (should (= 4 (map-length ht)))))

(ert-deftest test-map-copy ()
  (with-maps-do map
    (let ((copy (map-copy map)))
      (should (equal (map-keys map) (map-keys copy)))
      (should (equal (map-values map) (map-values copy)))
      (should (not (eq map copy))))))

(ert-deftest test-map-apply ()
  (with-maps-do map
    (should (equal (map-apply (lambda (k v) (cons (int-to-string k) v))
                              map)
                   '(("0" . 3) ("1" . 4) ("2" . 5)))))
  (let ((vec [a b c]))
    (should (equal (map-apply (lambda (k v) (cons (1+ k) v))
                              vec)
                   '((1 . a)
                     (2 . b)
                     (3 . c))))))

(ert-deftest test-map-do ()
  (with-maps-do map
    (let ((result nil))
      (map-do (lambda (k v)
                (add-to-list 'result (list (int-to-string k) v)))
              map)
      (should (equal result '(("2" 5) ("1" 4) ("0" 3)))))))

(ert-deftest test-map-keys-apply ()
  (with-maps-do map
    (should (equal (map-keys-apply (lambda (k) (int-to-string k))
                                   map)
                   '("0" "1" "2"))))
  (let ((vec [a b c]))
    (should (equal (map-keys-apply (lambda (k) (1+ k))
                                   vec)
                   '(1 2 3)))))

(ert-deftest test-map-values-apply ()
  (with-maps-do map
    (should (equal (map-values-apply (lambda (v) (1+ v))
                                     map)
                   '(4 5 6))))
  (let ((vec [a b c]))
    (should (equal (map-values-apply (lambda (v) (symbol-name v))
                                     vec)
                   '("a" "b" "c")))))

(ert-deftest test-map-filter ()
  (with-maps-do map
    (should (equal (map-keys (map-filter (lambda (_k v)
                                           (<= 4 v))
                                         map))
                   '(1 2)))
    (should (null (map-filter (lambda (k _v)
                                (eq 'd k))
                              map))))
  (should (null (map-filter (lambda (_k v)
                              (eq 3 v))
                            [1 2 4 5])))
  (should (equal (map-filter (lambda (k _v)
                               (eq 3 k))
                             [1 2 4 5])
                 '((3 . 5)))))

(ert-deftest test-map-remove ()
  (with-maps-do map
    (should (equal (map-keys (map-remove (lambda (_k v)
                                           (>= v 4))
                                         map))
                   '(0)))
    (should (equal (map-keys (map-remove (lambda (k _v)
                                           (eq 'd k))
                                         map))
                   (map-keys map))))
  (should (equal (map-remove (lambda (_k v)
                               (eq 3 v))
                             [1 2 4 5])
                 '((0 . 1)
                   (1 . 2)
                   (2 . 4)
                   (3 . 5))))
  (should (null (map-remove (lambda (k _v)
                              (>= k 0))
                            [1 2 4 5]))))

(ert-deftest test-map-empty-p ()
  (should (map-empty-p nil))
  (should (not (map-empty-p '((a . b) (c . d)))))
  (should (map-empty-p []))
  (should (not (map-empty-p [1 2 3])))
  (should (map-empty-p (make-hash-table)))
  (should (not (map-empty-p "hello")))
  (should (map-empty-p "")))

(ert-deftest test-map-contains-key ()
  (should (map-contains-key '((a . 1) (b . 2)) 'a))
  (should (not (map-contains-key '((a . 1) (b . 2)) 'c)))
  (should (map-contains-key '(("a" . 1)) "a"))
  (should (not (map-contains-key '(("a" . 1)) "a" #'eq)))
  (should (map-contains-key [a b c] 2))
  (should (not (map-contains-key [a b c] 3))))

(ert-deftest test-map-some ()
  (with-maps-do map
    (should (map-some (lambda (k _v)
                        (eq 1 k))
                      map))
    (should-not (map-some (lambda (k _v)
                            (eq 'd k))
                          map)))
  (let ((vec [a b c]))
    (should (map-some (lambda (k _v)
                        (> k 1))
                      vec))
    (should-not (map-some (lambda (k _v)
                            (> k 3))
                          vec))))

(ert-deftest test-map-every-p ()
  (with-maps-do map
    (should (map-every-p (lambda (k _v)
                           k)
                         map))
    (should (not (map-every-p (lambda (_k _v)
                                nil)
                              map))))
  (let ((vec [a b c]))
    (should (map-every-p (lambda (k _v)
                           (>= k 0))
                         vec))
    (should (not (map-every-p (lambda (k _v)
                                (> k 3))
                              vec)))))

(ert-deftest test-map-into ()
  (let* ((alist '((a . 1) (b . 2)))
         (ht (map-into alist 'hash-table)))
    (should (hash-table-p ht))
    (should (equal (map-into (map-into alist 'hash-table) 'list)
                   alist))
    (should (listp (map-into ht 'list)))
    (should (equal (map-keys (map-into (map-into ht 'list) 'hash-table))
                   (map-keys ht)))
    (should (equal (map-values (map-into (map-into ht 'list) 'hash-table))
                   (map-values ht)))
    (should (null (map-into nil 'list)))
    (should (map-empty-p (map-into nil 'hash-table)))
    (should-error (map-into [1 2 3] 'string))))

(ert-deftest test-map-let ()
  (map-let (foo bar baz) '((foo . 1) (bar . 2))
    (should (= foo 1))
    (should (= bar 2))
    (should (null baz)))
  (map-let (('foo a)
            ('bar b)
            ('baz c))
      '((foo . 1) (bar . 2))
    (should (= a 1))
    (should (= b 2))
    (should (null c))))

(ert-deftest test-map-merge-with ()
  (should (equal (map-merge-with 'list #'+
                                 '((1 . 2))
                                 '((1 . 3) (2 . 4))
                                 '((1 . 1) (2 . 5) (3 . 0)))
                 '((3 . 0) (2 . 9) (1 . 6)))))

(provide 'map-tests)
;;; map-tests.el ends here
