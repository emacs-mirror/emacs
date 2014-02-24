;;; eieio-testsinvoke.el -- eieio tests for method invocation

;; Copyright (C) 2005, 2008, 2010, 2013-2014 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Test method invocation order.  From the common lisp reference
;; manual:
;;
;; QUOTE:
;; - All the :before methods are called, in most-specific-first
;;   order.  Their values are ignored.  An error is signaled if
;;   call-next-method is used in a :before method.
;;
;; - The most specific primary method is called. Inside the body of a
;;   primary method, call-next-method may be used to call the next
;;   most specific primary method. When that method returns, the
;;   previous primary method can execute more code, perhaps based on
;;   the returned value or values. The generic function no-next-method
;;   is invoked if call-next-method is used and there are no more
;;   applicable primary methods. The function next-method-p may be
;;   used to determine whether a next method exists. If
;;   call-next-method is not used, only the most specific primary
;;   method is called.
;;
;; - All the :after methods are called, in most-specific-last order.
;;   Their values are ignored.  An error is signaled if
;;   call-next-method is used in a :after method.
;;
;;
;; Also test behavior of `call-next-method'. From clos.org:
;;
;; QUOTE:
;; When call-next-method is called with no arguments, it passes the
;; current method's original arguments to the next method.

(require 'eieio)
(require 'ert)

(defvar eieio-test-method-order-list nil
  "List of symbols stored during method invocation.")

(defun eieio-test-method-store ()
  "Store current invocation class symbol in the invocation order list."
  (let* ((keysym (aref [ :STATIC :BEFORE :PRIMARY :AFTER ]
		       (or eieio-generic-call-key 0)))
	 (c (list eieio-generic-call-methodname keysym (eieio--scoped-class))))
    (setq eieio-test-method-order-list
	  (cons c eieio-test-method-order-list))))

(defun eieio-test-match (rightanswer)
  "Do a test match."
  (if (equal rightanswer eieio-test-method-order-list)
      t
    (error "eieio-test-methodinvoke.el: Test Failed!")))

(defvar eieio-test-call-next-method-arguments nil
  "List of passed to methods during execution of `call-next-method'.")

(defun eieio-test-arguments-for (class)
  "Returns arguments passed to method of CLASS during `call-next-method'."
  (cdr (assoc class eieio-test-call-next-method-arguments)))

(defclass eitest-A () ())
(defclass eitest-AA (eitest-A) ())
(defclass eitest-AAA (eitest-AA) ())
(defclass eitest-B-base1 () ())
(defclass eitest-B-base2 () ())
(defclass eitest-B (eitest-B-base1 eitest-B-base2) ())

(defmethod eitest-F :BEFORE ((p eitest-B-base1))
  (eieio-test-method-store))

(defmethod eitest-F :BEFORE ((p eitest-B-base2))
  (eieio-test-method-store))

(defmethod eitest-F :BEFORE ((p eitest-B))
  (eieio-test-method-store))

(defmethod eitest-F ((p eitest-B))
  (eieio-test-method-store)
  (call-next-method))

(defmethod eitest-F ((p eitest-B-base1))
  (eieio-test-method-store)
  (call-next-method))

(defmethod eitest-F ((p eitest-B-base2))
  (eieio-test-method-store)
  (when (next-method-p)
    (call-next-method))
  )

(defmethod eitest-F :AFTER ((p eitest-B-base1))
  (eieio-test-method-store))

(defmethod eitest-F :AFTER ((p eitest-B-base2))
  (eieio-test-method-store))

(defmethod eitest-F :AFTER ((p eitest-B))
  (eieio-test-method-store))

(ert-deftest eieio-test-method-order-list-3 ()
  (let ((eieio-test-method-order-list nil)
	(ans '(
	       (eitest-F :BEFORE eitest-B)
	       (eitest-F :BEFORE eitest-B-base1)
	       (eitest-F :BEFORE eitest-B-base2)

	       (eitest-F :PRIMARY eitest-B)
	       (eitest-F :PRIMARY eitest-B-base1)
	       (eitest-F :PRIMARY eitest-B-base2)

	       (eitest-F :AFTER eitest-B-base2)
	       (eitest-F :AFTER eitest-B-base1)
	       (eitest-F :AFTER eitest-B)
	       )))
    (eitest-F (eitest-B nil))
    (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
    (eieio-test-match ans)))

;;; Test static invocation
;;
(defmethod eitest-H :STATIC ((class eitest-A))
  "No need to do work in here."
  'moose)

(ert-deftest eieio-test-method-order-list-4 ()
  ;; Both of these situations should succeed.
  (should (eitest-H eitest-A))
  (should (eitest-H (eitest-A nil))))

;;; Return value from :PRIMARY
;;
(defmethod eitest-I :BEFORE ((a eitest-A))
  (eieio-test-method-store)
  ":before")

(defmethod eitest-I :PRIMARY ((a eitest-A))
  (eieio-test-method-store)
  ":primary")

(defmethod eitest-I :AFTER ((a eitest-A))
  (eieio-test-method-store)
  ":after")

(ert-deftest eieio-test-method-order-list-5 ()
  (let ((eieio-test-method-order-list nil)
	(ans  (eitest-I (eitest-A nil))))
    (should (string= ans ":primary"))))

;;; Multiple inheritance and the 'constructor' method.
;;
;; Constructor is a static method, so this is really testing
;; static method invocation and multiple inheritance.
;;
(defclass C-base1 () ())
(defclass C-base2 () ())
(defclass C (C-base1 C-base2) ())

(defmethod constructor :STATIC ((p C-base1) &rest args)
  (eieio-test-method-store)
  (if (next-method-p) (call-next-method))
  )

(defmethod constructor :STATIC ((p C-base2) &rest args)
  (eieio-test-method-store)
  (if (next-method-p) (call-next-method))
  )

(defmethod constructor :STATIC ((p C) &rest args)
  (eieio-test-method-store)
  (call-next-method)
  )

(ert-deftest eieio-test-method-order-list-6 ()
  (let ((eieio-test-method-order-list nil)
	(ans '(
	       (constructor :STATIC C)
	       (constructor :STATIC C-base1)
	       (constructor :STATIC C-base2)
	       )))
    (C nil)
    (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
    (eieio-test-match ans)))

;;; Diamond Test
;;
;; For a diamond shaped inheritance structure, (call-next-method) can break.
;; As such, there are two possible orders.

(defclass D-base0 () () :method-invocation-order :depth-first)
(defclass D-base1 (D-base0) () :method-invocation-order :depth-first)
(defclass D-base2 (D-base0) () :method-invocation-order :depth-first)
(defclass D (D-base1 D-base2) () :method-invocation-order :depth-first)

(defmethod eitest-F ((p D))
  "D"
  (eieio-test-method-store)
  (call-next-method))

(defmethod eitest-F ((p D-base0))
  "D-base0"
  (eieio-test-method-store)
  ;; This should have no next
  ;; (when (next-method-p) (call-next-method))
  )

(defmethod eitest-F ((p D-base1))
  "D-base1"
  (eieio-test-method-store)
  (call-next-method))

(defmethod eitest-F ((p D-base2))
  "D-base2"
  (eieio-test-method-store)
  (when (next-method-p)
    (call-next-method))
  )

(ert-deftest eieio-test-method-order-list-7 ()
  (let ((eieio-test-method-order-list nil)
	(ans '(
	       (eitest-F :PRIMARY D)
	       (eitest-F :PRIMARY D-base1)
	       ;; (eitest-F :PRIMARY D-base2)
	       (eitest-F :PRIMARY D-base0)
	       )))
    (eitest-F (D nil))
    (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
    (eieio-test-match ans)))

;;; Other invocation order

(defclass E-base0 () () :method-invocation-order :breadth-first)
(defclass E-base1 (E-base0) () :method-invocation-order :breadth-first)
(defclass E-base2 (E-base0) () :method-invocation-order :breadth-first)
(defclass E (E-base1 E-base2) () :method-invocation-order :breadth-first)

(defmethod eitest-F ((p E))
  (eieio-test-method-store)
  (call-next-method))

(defmethod eitest-F ((p E-base0))
  (eieio-test-method-store)
  ;; This should have no next
  ;; (when (next-method-p) (call-next-method))
  )

(defmethod eitest-F ((p E-base1))
  (eieio-test-method-store)
  (call-next-method))

(defmethod eitest-F ((p E-base2))
  (eieio-test-method-store)
  (when (next-method-p)
    (call-next-method))
  )

(ert-deftest eieio-test-method-order-list-8 ()
  (let ((eieio-test-method-order-list nil)
	(ans '(
	       (eitest-F :PRIMARY E)
	       (eitest-F :PRIMARY E-base1)
	       (eitest-F :PRIMARY E-base2)
	       (eitest-F :PRIMARY E-base0)
	       )))
    (eitest-F (E nil))
    (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
    (eieio-test-match ans)))

;;; Jan's methodinvoke order w/ multiple inheritance and :after methods.
;;
(defclass eitest-Ja ()
  ())

(defmethod initialize-instance :after ((this eitest-Ja) &rest slots)
  ;(message "+Ja")
  (when (next-method-p)
    (call-next-method))
  ;(message "-Ja")
  )

(defclass eitest-Jb ()
  ())

(defmethod initialize-instance :after ((this eitest-Jb) &rest slots)
  ;(message "+Jb")
  (when (next-method-p)
    (call-next-method))
  ;(message "-Jb")
  )

(defclass eitest-Jc (eitest-Jb)
  ())

(defclass eitest-Jd (eitest-Jc eitest-Ja)
  ())

(defmethod initialize-instance ((this eitest-Jd) &rest slots)
  ;(message "+Jd")
  (when (next-method-p)
    (call-next-method))
  ;(message "-Jd")
  )

(ert-deftest eieio-test-method-order-list-9 ()
  (should (eitest-Jd "test")))

;;; call-next-method with replacement arguments across a simple class hierarchy.
;;

(defclass CNM-0 ()
  ())

(defclass CNM-1-1 (CNM-0)
  ())

(defclass CNM-1-2 (CNM-0)
  ())

(defclass CNM-2 (CNM-1-1 CNM-1-2)
  ())

(defmethod CNM-M ((this CNM-0) args)
  (push (cons 'CNM-0 (copy-sequence args))
	eieio-test-call-next-method-arguments)
  (when (next-method-p)
    (call-next-method
     this (cons 'CNM-0 args))))

(defmethod CNM-M ((this CNM-1-1) args)
  (push (cons 'CNM-1-1 (copy-sequence args))
	eieio-test-call-next-method-arguments)
  (when (next-method-p)
    (call-next-method
     this (cons 'CNM-1-1 args))))

(defmethod CNM-M ((this CNM-1-2) args)
  (push (cons 'CNM-1-2 (copy-sequence args))
	eieio-test-call-next-method-arguments)
  (when (next-method-p)
    (call-next-method)))

(defmethod CNM-M ((this CNM-2) args)
  (push (cons 'CNM-2 (copy-sequence args))
	eieio-test-call-next-method-arguments)
  (when (next-method-p)
    (call-next-method
     this (cons 'CNM-2 args))))

(ert-deftest eieio-test-method-order-list-10 ()
  (let ((eieio-test-call-next-method-arguments nil))
    (CNM-M (CNM-2 "") '(INIT))
    (should (equal (eieio-test-arguments-for 'CNM-0)
		   '(CNM-1-1 CNM-2 INIT)))
    (should (equal (eieio-test-arguments-for 'CNM-1-1)
		   '(CNM-2 INIT)))
    (should (equal (eieio-test-arguments-for 'CNM-1-2)
		   '(CNM-1-1 CNM-2 INIT)))
    (should (equal (eieio-test-arguments-for 'CNM-2)
		   '(INIT)))))
