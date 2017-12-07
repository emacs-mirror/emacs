;;; ring-tests.el --- Tests for ring.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
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
(require 'ring)

(ert-deftest ring-tests-make-ring-ring-p ()
  (should (ring-p (make-ring 5))))

(ert-deftest ring-tests-insert-at-beginning ()
  (let ((ring (make-ring 5)))
    (ring-insert-at-beginning ring 'foo)
    (ring-insert-at-beginning ring 'bar)
    (should (equal (ring-elements ring) '(foo bar)))))

(ert-deftest ring-tests-plus1 ()
  (should (= (ring-plus1 0 5) 1))
  (should (= (ring-plus1 4 5) 0)))

(ert-deftest ring-tests-minus1 ()
  (should (= (ring-minus1 0 5) 4))
  (should (= (ring-minus1 4 5) 3)))

(ert-deftest ring-tests-length ()
  (let ((ring (make-ring 2)))
    (should (= (ring-length ring) 0))
    (ring-insert ring 'a)
    (should (= (ring-length ring) 1))
    (ring-insert ring 'b)
    (should (= (ring-length ring) 2))
    (ring-insert ring 'c)
    (should (= (ring-length ring) 2))))

(ert-deftest ring-tests-index ()
  (should (= (ring-index 0 0 3 3) 2))
  (should (= (ring-index 0 0 3 5) 2))
  (should (= (ring-index 0 2 3 3) 1))
  (should (= (ring-index 1 2 3 3) 0))
  (should (= (ring-index 2 2 3 3) 2)))

(ert-deftest ring-tests-empty-p ()
  (let ((ring (make-ring 5)))
    (should (ring-empty-p ring))
    (ring-insert ring 1)
    (should-not (ring-empty-p ring))
    (ring-remove ring)
    (should (ring-empty-p ring))))

(ert-deftest ring-tests-size ()
  (let ((ring (make-ring 2)))
    (should (= (ring-size ring) 2))
    (ring-insert ring "a")
    (ring-insert ring "b")
    (ring-insert ring "c")
    (should (= (ring-size ring) 2))
    (ring-extend ring 3)
    (should (= (ring-size ring) 5))))

(ert-deftest ring-tests-copy ()
  (let ((ring1 (make-ring 3)))
    (ring-insert ring1 1)
    (ring-insert ring1 2)
    (let ((ring2 (ring-copy ring1)))
      (should-not (eq ring1 ring2))
      (should (= (ring-size ring1) (ring-size ring2)))
      (should (equal (ring-elements ring1) (ring-elements ring2))))))

(ert-deftest ring-tests-insert ()
  (let ((ring (make-ring 2)))
    (ring-insert ring :a)
    (should (equal (ring-elements ring) '(:a)))
    (ring-insert ring :b)
    (should (equal (ring-elements ring) '(:b :a)))
    (ring-insert ring :c)
    (should (equal (ring-elements ring) '(:c :b)))))

(ert-deftest ring-tests-remove ()
  (let ((ring (make-ring 2)))
    (should-error (ring-remove ring))
    (ring-insert ring 'foo)
    (ring-insert ring 'bar)
    (should (eq (ring-remove ring) 'foo))
    (should (equal (ring-elements ring) '(bar)))
    (ring-insert ring 'baz)
    (should (eq (ring-remove ring 0) 'baz))
    (should (equal (ring-elements ring) '(bar)))))

(ert-deftest ring-tests-ref ()
  (let ((ring (make-ring 2)))
    (should-error (ring-ref ring 0))
    (ring-insert ring :a)
    (should (eq (ring-ref ring 0) :a))
    (ring-insert ring :b)
    (should (eq (ring-ref ring 0) :b))
    (should (eq (ring-ref ring 1) :a))
    (should (eq (ring-ref ring 2) :b))))

(ert-deftest ring-tests-elements ()
  (let ((ring (make-ring 5)))
    (ring-insert ring 3)
    (ring-insert ring 2)
    (ring-insert ring 1)
    (should (equal (ring-elements ring) '(1 2 3)))))

(ert-deftest ring-tests-member ()
  (let ((ring (make-ring 3)))
    (ring-insert ring "foo")
    (ring-insert ring "bar")
    (should (= (ring-member ring "foo") 1))
    (should (= (ring-member ring "bar") 0))
    (should-not (ring-member ring "baz"))))

(ert-deftest ring-tests-next ()
  (let ((ring (make-ring 3)))
    (ring-insert ring 'a)
    (ring-insert ring 'b)
    (should (eq (ring-next ring 'b) 'a))
    (should (eq (ring-next ring 'a) 'b))
    (should-error (ring-next ring 'c))))

(ert-deftest ring-tests-previous ()
  (let ((ring (make-ring 3)))
    (ring-insert ring 'a)
    (ring-insert ring 'b)
    (should (eq (ring-previous ring 'b) 'a))
    (should (eq (ring-previous ring 'a) 'b))
    (should-error (ring-previous ring 'c))))

(ert-deftest ring-tests-extend ()
  (let ((ring (make-ring 2)))
    (ring-insert ring 1)
    (ring-insert ring 2)
    (should (= (ring-size ring) 2))
    (should (= (ring-length ring) 2))
    (ring-extend ring 3)
    (ring-insert ring 3)
    (should (= (ring-size ring) 5))
    (should (equal (ring-elements ring) '(3 2 1)))))

(ert-deftest ring-tests-insert ()
  (let ((ring (make-ring 2)))
    (ring-insert+extend ring :a)
    (ring-insert+extend ring :b)
    (should (equal (ring-elements ring) '(:b :a)))
    (ring-insert+extend ring :c)
    (should (equal (ring-elements ring) '(:c :b)))
    (ring-insert+extend ring :d t)
    (should (equal (ring-elements ring) '(:d :c :b)))))

(ert-deftest ring-tests-remove+insert+extend ()
  (let ((ring (make-ring 3)))
    (ring-insert ring 1)
    (ring-insert ring 1)
    (ring-insert ring 2)
    (ring-remove+insert+extend ring 1)
    (should (equal (ring-elements ring) '(1 2)))
    (ring-remove+insert+extend ring 0)
    (should (equal (ring-elements ring) '(0 1 2)))
    (ring-remove+insert+extend ring 3)
    (should (equal (ring-elements ring) '(3 0 1)))
    (ring-remove+insert+extend ring 4 t)
    (should (equal (ring-elements ring) '(4 3 0 1)))
    (ring-remove+insert+extend ring 1 t)
    (should (equal (ring-elements ring) '(1 4 3 0)))))

(ert-deftest ring-tests-convert-sequence-to-ring ()
  (let ((ring (ring-convert-sequence-to-ring '(a b c))))
    (should (equal (ring-elements ring) '(a b c))))
  (let ((ring (ring-convert-sequence-to-ring [1 2 3])))
    (should (equal (ring-elements ring) '(1 2 3))))
  (let ((ring (ring-convert-sequence-to-ring "abc")))
    (should (equal (ring-elements ring) '(?a ?b ?c))))
  (let ((ring (make-ring 2)))
    (ring-insert ring :a)
    (ring-insert ring :b)
    (should
     (equal (ring-elements (ring-convert-sequence-to-ring ring))
            (ring-elements ring)))))

(provide 'ring-tests)
;;; ring-tests.el ends here
