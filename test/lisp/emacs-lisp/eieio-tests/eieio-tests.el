;;; eieio-tests.el -- eieio tests routines

;; Copyright (C) 1999-2003, 2005-2010, 2012-2018 Free Software
;; Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Test the various features of EIEIO.

(require 'ert)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-opt)

(eval-when-compile (require 'cl-lib))

;;; Code:
;; Set up some test classes
(defclass class-a ()
  ((water :initarg :water
	  :initform h20
	  :type symbol
	  :documentation "Detail about water.")
   (classslot :initform penguin
	      :type symbol
	      :documentation "A class allocated slot."
	      :allocation :class)
   (test-tag :initform nil
	     :documentation "Used to make sure methods are called.")
   (self :initform nil
	 :type (or null class-a)
	 :documentation "Test self referencing types.")
   )
  "Class A")

(defclass class-b ()
  ((land :initform "Sc"
	 :type string
	 :documentation "Detail about land."))
  "Class B")

(defclass class-ab (class-a class-b)
  ((amphibian :initform "frog"
	      :documentation "Detail about amphibian on land and water."))
  "Class A and B combined.")

(defclass class-c ()
  ((slot-1 :initarg :moose
	   :initform moose
	   :type symbol
	   :allocation :instance
	   :documentation "First slot testing slot arguments."
	   :custom symbol
	   :label "Wild Animal"
	   :group borg
	   :protection :public)
   (slot-2 :initarg :penguin
	   :initform "penguin"
	   :type string
	   :allocation :instance
	   :documentation "Second slot testing slot arguments."
	   :custom string
	   :label "Wild bird"
	   :group vorlon
	   :accessor get-slot-2
	   :protection :private)
   (slot-3 :initarg :emu
	   :initform emu
	   :type symbol
	   :allocation :class
	   :documentation "Third slot test class allocated accessor"
	   :custom symbol
	   :label "Fuzz"
	   :group tokra
	   :accessor get-slot-3
	   :protection :private)
   )
  (:custom-groups (foo))
  "A class for testing slot arguments."
  )

(defclass class-subc (class-c)
  ((slot-1 ;; :initform moose  - don't override this
    )
   (slot-2 :initform "linux" ;; Do override this one
	   :protection :private
	   ))
  "A class for testing slot arguments.")

;;; Defining a class with a slot tag error
;;
;; Temporarily disable this test because of macro expansion changes in
;; current Emacs trunk. It can be re-enabled when we have moved
;; `eieio-defclass' into the `defclass' macro and the
;; `eval-and-compile' there is removed.

;; (let ((eieio-error-unsupported-class-tags t))
;;   (condition-case nil
;;       (progn
;; 	(defclass class-error ()
;; 	  ((error-slot :initarg :error-slot
;; 		       :badslottag 1))
;; 	  "A class with a bad slot tag.")
;; 	(error "No error was thrown for badslottag"))
;;     (invalid-slot-type nil)))

;; (let ((eieio-error-unsupported-class-tags nil))
;;   (condition-case nil
;;       (progn
;; 	(defclass class-error ()
;; 	  ((error-slot :initarg :error-slot
;; 		       :badslottag 1))
;; 	  "A class with a bad slot tag."))
;;     (invalid-slot-type
;;      (error "invalid-slot-type thrown when eieio-error-unsupported-class-tags is nil")
;;      )))

(ert-deftest eieio-test-01-mix-alloc-initarg ()
  ;; Only run this test if the message framework thingy works.
  (when (and (message "foo") (string= "foo" (current-message)))

    ;; Defining this class should generate a warning(!) message that
    ;; you should not mix :initarg with class allocated slots.
    (defclass class-alloc-initarg ()
      ((throwwarning :initarg :throwwarning
		     :allocation :class))
      "Throw a warning mixing allocation class and an initarg.")

    ;; Check that message is there
    (should (current-message))
    (should (string-match "Class allocated slots do not need :initarg"
			  (current-message)))))

(defclass abstract-class ()
  ((some-slot :initarg :some-slot
	      :initform nil
	      :documentation "A slot."))
  :documentation "An abstract class."
  :abstract t)

(ert-deftest eieio-test-02-abstract-class ()
  ;; Abstract classes cannot be instantiated, so this should throw an
  ;; error
  (should-error (abstract-class)))

(defgeneric generic1 () "First generic function")

(ert-deftest eieio-test-03-generics ()
  (defun anormalfunction () "A plain function for error testing." nil)
  (should-error
   (progn
     (defgeneric anormalfunction ()
       "Attempt to turn it into a generic.")))

  ;; Check that generic-p works
  (should (generic-p 'generic1))

  (defmethod generic1 ((c class-a))
    "Method on generic1."
    'monkey)

  (defmethod generic1 (not-an-object)
    "Method generic1 that can take a non-object."
    not-an-object)

  (let ((ans-obj (generic1 (class-a)))
	(ans-num (generic1 666)))
    (should (eq ans-obj 'monkey))
    (should (eq ans-num 666))))

(defclass static-method-class ()
  ((some-slot :initform nil
	      :allocation :class
	      :documentation "A slot."))
  :documentation "A class used for testing static methods.")

(defmethod static-method-class-method :STATIC ((c static-method-class) value)
  "Test static methods.
Argument C is the class bound to this static method."
  (if (eieio-object-p c) (setq c (eieio-object-class c)))
  (oset-default c some-slot value))

(ert-deftest eieio-test-04-static-method ()
  ;; Call static method on a class and see if it worked
  (static-method-class-method 'static-method-class 'class)
  (should (eq (oref-default 'static-method-class some-slot) 'class))
  (static-method-class-method (static-method-class) 'object)
  (should (eq (oref-default 'static-method-class some-slot) 'object)))

(ert-deftest eieio-test-05-static-method-2 ()
  (defclass static-method-class-2 (static-method-class)
    ()
    "A second class after the previous for static methods.")

  (defmethod static-method-class-method :STATIC ((c static-method-class-2) value)
    "Test static methods.
Argument C is the class bound to this static method."
    (if (eieio-object-p c) (setq c (eieio-object-class c)))
    (oset-default c some-slot (intern (concat "moose-" (symbol-name value)))))

  (static-method-class-method 'static-method-class-2 'class)
  (should (eq (oref-default 'static-method-class-2 some-slot) 'moose-class))
  (static-method-class-method (static-method-class-2) 'object)
  (should (eq (oref-default 'static-method-class-2 some-slot) 'moose-object)))


;;; Perform method testing
;;

;;; Multiple Inheritance, and method signal testing
;;
(defvar eitest-ab nil)
(defvar eitest-a nil)
(defvar eitest-b nil)
(ert-deftest eieio-test-06-allocate-objects ()
   ;; allocate an object to use
   (should (setq eitest-ab (class-ab)))
   (should (setq eitest-a (class-a)))
   (should (setq eitest-b (class-b))))

(ert-deftest eieio-test-07-make-instance ()
  (should (make-instance 'class-ab))
  (should (make-instance 'class-a :water 'cho))
  (should (make-instance 'class-b)))

(defmethod class-cn ((a class-a))
  "Try calling `call-next-method' when there isn't one.
Argument A is object of type symbol `class-a'."
  (call-next-method))

(defmethod no-next-method ((a class-a) &rest args)
  "Override signal throwing for variable `class-a'.
Argument A is the object of class variable `class-a'."
  'moose)

(ert-deftest eieio-test-08-call-next-method ()
  ;; Play with call-next-method
  (should (eq (class-cn eitest-ab) 'moose)))

(defmethod no-applicable-method ((b class-b) method &rest args)
  "No need.
Argument B is for booger.
METHOD is the method that was attempting to be called."
  'moose)

(ert-deftest eieio-test-09-no-applicable-method ()
  ;; Non-existing methods.
  (should (eq (class-cn eitest-b) 'moose)))

(defmethod class-fun ((a class-a))
  "Fun with class A."
  'moose)

(defmethod class-fun ((b class-b))
  "Fun with class B."
  (error "Class B fun should not be called")
  )

(defmethod class-fun-foo ((b class-b))
  "Foo Fun with class B."
  'moose)

(defmethod class-fun2 ((a class-a))
  "More fun with class A."
  'moose)

(defmethod class-fun2 ((b class-b))
  "More fun with class B."
  (error "Class B fun2 should not be called")
  )

(defmethod class-fun2 ((ab class-ab))
  "More fun with class AB."
  (call-next-method))

;; How about if B is the only slot?
(defmethod class-fun3 ((b class-b))
  "Even More fun with class B."
  'moose)

(defmethod class-fun3 ((ab class-ab))
  "Even More fun with class AB."
  (call-next-method))

(ert-deftest eieio-test-10-multiple-inheritance ()
  ;; play with methods and mi
  (should (eq (class-fun eitest-ab) 'moose))
  (should (eq (class-fun-foo eitest-ab) 'moose))
  ;; Play with next-method and mi
  (should (eq (class-fun2 eitest-ab) 'moose))
  (should (eq (class-fun3 eitest-ab) 'moose)))

(ert-deftest eieio-test-11-self ()
  ;; Try the self referencing test
  (should (oset eitest-a self eitest-a))
  (should (oset eitest-ab self eitest-ab)))


(defvar class-fun-value-seq '())
(defmethod class-fun-value :BEFORE ((a class-a))
  "Return `before', and push `before' in `class-fun-value-seq'."
  (push 'before class-fun-value-seq)
  'before)

(defmethod class-fun-value :PRIMARY ((a class-a))
  "Return `primary', and push `primary' in `class-fun-value-seq'."
  (push 'primary class-fun-value-seq)
  'primary)

(defmethod class-fun-value :AFTER ((a class-a))
  "Return `after', and push `after' in `class-fun-value-seq'."
  (push 'after class-fun-value-seq)
  'after)

(ert-deftest eieio-test-12-generic-function-call ()
  ;; Test value of a generic function call
  ;;
  (let* ((class-fun-value-seq nil)
	 (value (class-fun-value eitest-a)))
    ;; Test if generic function call returns the primary method's value
    (should (eq value 'primary))
    ;; Make sure :before and :after methods were run
    (should (equal class-fun-value-seq '(after primary before)))))

;;; Test initialization methods
;;

(ert-deftest eieio-test-13-init-methods ()
  (defmethod initialize-instance ((a class-a) &rest slots)
    "Initialize the slots of class-a."
    (call-next-method)
    (if (/= (oref a test-tag) 1)
	(error "shared-initialize test failed."))
    (oset a test-tag 2))

  (defmethod shared-initialize ((a class-a) &rest slots)
    "Shared initialize method for class-a."
    (call-next-method)
    (oset a test-tag 1))

  (let ((ca (class-a)))
    (should-not (/=  (oref ca test-tag) 2))))


;;; Perform slot testing
;;
(ert-deftest eieio-test-14-slots ()
  ;; Check slot existence
  (should (oref eitest-ab water))
  (should (oref eitest-ab land))
  (should (oref eitest-ab amphibian)))

(ert-deftest eieio-test-15-slot-missing ()

  (defmethod slot-missing ((ab class-ab) &rest foo)
    "If a slot in AB is unbound, return something cool.  FOO."
    'moose)

  (should (eq (oref eitest-ab ooga-booga) 'moose))
  (should-error (oref eitest-a ooga-booga) :type 'invalid-slot-name))

(ert-deftest eieio-test-16-slot-makeunbound ()
  (slot-makeunbound eitest-a 'water)
  ;; Should now be unbound
  (should-not (slot-boundp eitest-a 'water))
  ;; But should still exist
  (should (slot-exists-p eitest-a 'water))
  (should-not (slot-exists-p eitest-a 'moose))
  ;; oref of unbound slot must fail
  (should-error (oref eitest-a water) :type 'unbound-slot))

(defvar eitest-vsca nil)
(defvar eitest-vscb nil)
(defclass virtual-slot-class ()
  ((base-value :initarg :base-value))
  "Class has real slot :base-value and simulated slot :derived-value.")
(defmethod slot-missing ((vsc virtual-slot-class)
			 slot-name operation &optional new-value)
  "Simulate virtual slot derived-value."
  (cond
   ((or (eq slot-name :derived-value)
	(eq slot-name 'derived-value))
    (with-slots (base-value) vsc
      (if (eq operation 'oref)
	  (+ base-value 1)
	(setq base-value (- new-value 1)))))
   (t (call-next-method))))

(ert-deftest eieio-test-17-virtual-slot ()
  (setq eitest-vsca (virtual-slot-class :base-value 1))
  ;; Check slot values
  (should (= (oref eitest-vsca base-value) 1))
  (should (= (oref eitest-vsca :derived-value) 2))

  (oset eitest-vsca derived-value 3)
  (should (= (oref eitest-vsca base-value) 2))
  (should (= (oref eitest-vsca :derived-value) 3))

  (oset eitest-vsca base-value 3)
  (should (= (oref eitest-vsca base-value) 3))
  (should (= (oref eitest-vsca :derived-value) 4))

  ;; should also be possible to initialize instance using virtual slot

  (setq eitest-vscb (virtual-slot-class :derived-value 5))
  (should (= (oref eitest-vscb base-value) 4))
  (should (= (oref eitest-vscb :derived-value) 5)))

(ert-deftest eieio-test-18-slot-unbound ()

  (defmethod slot-unbound ((a class-a) &rest foo)
    "If a slot in A is unbound, ignore FOO."
    'moose)

  (should (eq (oref eitest-a water) 'moose))

  ;; Check if oset of unbound works
  (oset eitest-a water 'moose)
  (should (eq (oref eitest-a water) 'moose))

  ;; oref/oref-default comparison
  (should-not (eq (oref eitest-a water) (oref-default eitest-a water)))

  ;; oset-default -> oref/oref-default comparison
  (oset-default (eieio-object-class eitest-a) water 'moose)
  (should (eq (oref eitest-a water) (oref-default eitest-a water)))

  ;; After setting 'water to 'moose, make sure a new object has
  ;; the right stuff.
  (oset-default (eieio-object-class eitest-a) water 'penguin)
  (should (eq (oref (class-a) water) 'penguin))

  ;; Revert the above
  (defmethod slot-unbound ((a class-a) &rest foo)
    "If a slot in A is unbound, ignore FOO."
    ;; Disable the old slot-unbound so we can run this test
    ;; more than once
    (call-next-method)))

(ert-deftest eieio-test-19-slot-type-checking ()
  ;; Slot type checking
  ;; We should not be able to set a string here
  (should-error (oset eitest-ab water "a string, not a symbol") :type 'invalid-slot-type)
  (should-error (oset eitest-ab classslot "a string, not a symbol") :type 'invalid-slot-type)
  (should-error (class-a :water "a string not a symbol") :type 'invalid-slot-type))

(ert-deftest eieio-test-20-class-allocated-slots ()
  ;; Test out class allocated slots
  (defvar eitest-aa nil)
  (setq eitest-aa (class-a))

  ;; Make sure class slots do not track between objects
  (let ((newval 'moose))
    (oset eitest-aa classslot newval)
    (should (eq (oref eitest-a classslot) newval))
    (should (eq (oref eitest-aa classslot) newval)))

  ;; Slot should be bound
  (should (slot-boundp eitest-a 'classslot))
  (should (slot-boundp 'class-a 'classslot))

  (slot-makeunbound eitest-a 'classslot)

  (should-not (slot-boundp eitest-a 'classslot))
  (should-not (slot-boundp 'class-a 'classslot)))


(defvar eieio-test-permuting-value nil)
(defvar eitest-pvinit nil)
(eval-and-compile
  (setq eieio-test-permuting-value 1))

(defclass inittest nil
  ((staticval :initform 1)
   (symval :initform eieio-test-permuting-value)
   (evalval :initform (symbol-value 'eieio-test-permuting-value))
   (evalnow :initform (symbol-value 'eieio-test-permuting-value)
	    :allocation :class)
   )
  "Test initforms that eval.")

(ert-deftest eieio-test-21-eval-at-construction-time ()
  ;; initforms that need to be evalled at construction time.
  (setq eieio-test-permuting-value 2)
  (setq eitest-pvinit (inittest))

  (should (eq (oref eitest-pvinit staticval) 1))
  (should (eq (oref eitest-pvinit symval) 'eieio-test-permuting-value))
  (should (eq (oref eitest-pvinit evalval) 2))
  (should (eq (oref eitest-pvinit evalnow) 1)))

(defvar eitest-tests nil)

(ert-deftest eieio-test-22-init-forms-dont-match-runnable ()
  ;; Init forms with types that don't match the runnable.
  (defclass eitest-subordinate nil
    ((text :initform "" :type string))
    "Test class that will be a calculated value.")

  (defclass eitest-superior nil
    ((sub :initform (eitest-subordinate)
	  :type eitest-subordinate))
    "A class with an initform that creates a class.")

  (should (setq eitest-tests (eitest-superior)))

  (should-error
   (eval
    '(defclass broken-init nil
       ((broken :initform 1
		:type string))
       "This class should break."))
   :type 'invalid-slot-type))

(ert-deftest eieio-test-23-inheritance-check ()
  (should (child-of-class-p 'class-ab 'class-a))
  (should (child-of-class-p 'class-ab 'class-b))
  (should (object-of-class-p eitest-a 'class-a))
  (should (object-of-class-p eitest-ab 'class-a))
  (should (object-of-class-p eitest-ab 'class-b))
  (should (object-of-class-p eitest-ab 'class-ab))
  (should (eq (eieio-class-parents 'class-a) nil))
  (should (equal (eieio-class-parents 'class-ab)
                 (mapcar #'find-class '(class-a class-b))))
  (should (same-class-p eitest-a 'class-a))
  (should (class-a-p eitest-a))
  (should (not (class-a-p eitest-ab)))
  (should (cl-typep eitest-a 'class-a))
  (should (cl-typep eitest-ab 'class-a))
  (should (not (class-a-p "foo")))
  (should (not (cl-typep "foo" 'class-a))))

(ert-deftest eieio-test-24-object-predicates ()
  (let ((listooa (list (class-ab) (class-a)))
	(listoob (list (class-ab) (class-b))))
    (should (cl-typep listooa '(list-of class-a)))
    (should (cl-typep listoob '(list-of class-b)))
    (should-not (cl-typep listooa '(list-of class-b)))
    (should-not (cl-typep listoob '(list-of class-a)))))

(defvar eitest-t1 nil)
(ert-deftest eieio-test-25-slot-tests ()
  (setq eitest-t1 (class-c))
  ;; Slot initialization
  (should (eq (oref eitest-t1 slot-1) 'moose))
  ;; Accessing via the initarg name is deprecated!
  ;; (should (eq (oref eitest-t1 :moose) 'moose))
  ;; Don't pass reference of private slot
  ;;PRIVATE (should-error (oref eitest-t1 slot-2) :type 'invalid-slot-name)
  ;; Check private slot accessor
  (should (string= (get-slot-2 eitest-t1) "penguin"))
  ;; Pass string instead of symbol
  (should-error (class-c :moose "not a symbol") :type 'invalid-slot-type)
  (should (eq (get-slot-3 eitest-t1) 'emu))
  (should (eq (get-slot-3 'class-c) 'emu))
  ;; Check setf
  (setf (get-slot-3 eitest-t1) 'setf-emu)
  (should (eq (get-slot-3 eitest-t1) 'setf-emu))
  ;; Roll back
  (setf (get-slot-3 eitest-t1) 'emu))

(defvar eitest-t2 nil)
(ert-deftest eieio-test-26-default-inheritance ()
  ;; See previous test, nor for subclass
  (setq eitest-t2 (class-subc))
  (should (eq (oref eitest-t2 slot-1) 'moose))
  ;; Accessing via the initarg name is deprecated!
  ;;(should (eq (oref eitest-t2 :moose) 'moose))
  (should (string= (get-slot-2 eitest-t2) "linux"))
  ;;PRIVATE (should-error (oref eitest-t2 slot-2) :type 'invalid-slot-name)
  (should (string= (get-slot-2 eitest-t2) "linux"))
  (should-error (class-subc :moose "not a symbol") :type 'invalid-slot-type))

;;(ert-deftest eieio-test-27-inherited-new-value ()
  ;;; HACK ALERT: The new value of a class slot is inherited by the
  ;; subclass!  This is probably a bug.  We should either share the slot
  ;; so sets on the baseclass change the subclass, or we should inherit
  ;; the original value.
;;  (should (eq (get-slot-3 eitest-t2) 'emu))
;;  (should (eq (get-slot-3 class-subc) 'emu))
;;  (setf (get-slot-3 eitest-t2) 'setf-emu)
;;  (should (eq (get-slot-3 eitest-t2) 'setf-emu)))

;; Slot protection
(defclass prot-0 ()
  ()
  "Protection testing baseclass.")

(defmethod prot0-slot-2 ((s2 prot-0))
  "Try to access slot-2 from this class which doesn't have it.
The object S2 passed in will be of class prot-1, which does have
the slot.  This could be allowed, and currently is in EIEIO.
Needed by the eieio persistent base class."
  (oref s2 slot-2))

(defclass prot-1 (prot-0)
  ((slot-1 :initarg :slot-1
	   :initform nil
	   :protection :public)
   (slot-2 :initarg :slot-2
	   :initform nil
	   :protection :protected)
   (slot-3 :initarg :slot-3
	   :initform nil
	   :protection :private))
  "A class for testing the :protection option.")

(defclass prot-2 (prot-1)
  nil
  "A class for testing the :protection option.")

(defmethod prot1-slot-2 ((s2 prot-1))
  "Try to access slot-2 in S2."
  (oref s2 slot-2))

(defmethod prot1-slot-2 ((s2 prot-2))
  "Try to access slot-2 in S2."
  (oref s2 slot-2))

(defmethod prot1-slot-3-only ((s2 prot-1))
  "Try to access slot-3 in S2.
Do not override for `prot-2'."
  (oref s2 slot-3))

(defmethod prot1-slot-3 ((s2 prot-1))
  "Try to access slot-3 in S2."
  (oref s2 slot-3))

(defmethod prot1-slot-3 ((s2 prot-2))
  "Try to access slot-3 in S2."
  (oref s2 slot-3))

(defvar eitest-p1 nil)
(defvar eitest-p2 nil)
(ert-deftest eieio-test-28-slot-protection ()
  (setq eitest-p1 (prot-1))
  (setq eitest-p2 (prot-2))
  ;; Access public slots
  (oref eitest-p1 slot-1)
  (oref eitest-p2 slot-1)
  ;; Accessing protected slot out of context used to fail, but we dropped this
  ;; feature, since it was underused and no one noticed that the check was
  ;; incorrect (much too loose).
  ;;PROTECTED (should-error (oref eitest-p1 slot-2) :type 'invalid-slot-name)
  ;; Access protected slot in method
  (prot1-slot-2 eitest-p1)
  ;; Protected slot in subclass method
  (prot1-slot-2 eitest-p2)
  ;; Protected slot from parent class method
  (prot0-slot-2 eitest-p1)
  ;; Accessing private slot out of context used to fail, but we dropped this
  ;; feature, since it was not used.
  ;;PRIVATE (should-error (oref eitest-p1 slot-3) :type 'invalid-slot-name)
  ;; Access private slot in method
  (prot1-slot-3 eitest-p1)
  ;; Access private slot in subclass method must fail
  ;;PRIVATE (should-error (prot1-slot-3 eitest-p2) :type 'invalid-slot-name)
  ;; Access private slot by same class
  (prot1-slot-3-only eitest-p1)
  ;; Access private slot by subclass in sameclass method
  (prot1-slot-3-only eitest-p2))

;;; eieio-instance-inheritor
;; Test to make sure this works.
(defclass II (eieio-instance-inheritor)
  ((slot1 :initform 1)
   (slot2)
   (slot3))
  "Instance Inheritor test class.")

(defvar eitest-II1 nil)
(defvar eitest-II2 nil)
(defvar eitest-II3 nil)
(ert-deftest eieio-test-29-instance-inheritor ()
  (setq eitest-II1 (II "II Test."))
  (oset eitest-II1 slot2 'cat)
  (setq eitest-II2 (clone eitest-II1 "eitest-II2 Test."))
  (oset eitest-II2 slot1 'moose)
  (setq eitest-II3 (clone eitest-II2 "eitest-II3 Test."))
  (oset eitest-II3 slot3 'penguin)

  ;; Test level 1 inheritance
  (should (eq (oref eitest-II3 slot1) 'moose))
  ;; Test level 2 inheritance
  (should (eq (oref eitest-II3 slot2) 'cat))
  ;; Test level 0 inheritance
  (should (eq (oref eitest-II3 slot3) 'penguin)))

(defclass slotattr-base ()
  ((initform :initform init)
   (type :type list)
   (initarg :initarg :initarg)
   (protection :protection :private)
   (custom :custom (repeat string)
	   :label "Custom Strings"
	   :group moose)
   (docstring :documentation
	      "Replace the doc-string for this property.")
   (printer :printer printer1)
   )
  "Baseclass we will attempt to subclass.
Subclasses to override slot attributes.")

(defclass slotattr-ok (slotattr-base)
  ((initform :initform no-init)
   (initarg :initarg :initblarg)
   (custom :custom string
	   :label "One String"
	   :group cow)
   (docstring :documentation
	      "A better doc string for this class.")
   (printer :printer printer2)
   )
  "This class should allow overriding of various slot attributes.")


(ert-deftest eieio-test-30-slot-attribute-override ()
  ;; Subclass should not override :protection slot attribute
  ;;PROTECTION is gone.
  ;;(should-error
  ;;       (eval
  ;;        '(defclass slotattr-fail (slotattr-base)
  ;;           ((protection :protection :public)
  ;;            )
  ;;           "This class should throw an error.")))

  ;; Subclass should not override :type slot attribute
  (should-error
	(eval
	 '(defclass slotattr-fail (slotattr-base)
	  ((type :type string)
	   )
	  "This class should throw an error.")))

  ;; Initform should override instance allocation
  (let ((obj (slotattr-ok)))
    (should (eq (oref obj initform) 'no-init))))

(defclass slotattr-class-base ()
  ((initform :allocation :class
	     :initform init)
   (type :allocation :class
	 :type list)
   (initarg :allocation :class
	    :initarg :initarg)
   (protection :allocation :class
	       :protection :private)
   (custom :allocation :class
	   :custom (repeat string)
	   :label "Custom Strings"
	   :group moose)
   (docstring :allocation :class
	      :documentation
	      "Replace the doc-string for this property.")
   )
  "Baseclass we will attempt to subclass.
Subclasses to override slot attributes.")

(defclass slotattr-class-ok (slotattr-class-base)
  ((initform :initform no-init)
   (initarg :initarg :initblarg)
   (custom :custom string
	   :label "One String"
	   :group cow)
   (docstring :documentation
	      "A better doc string for this class.")
   )
  "This class should allow overriding of various slot attributes.")


(ert-deftest eieio-test-31-slot-attribute-override-class-allocation ()
  ;; Same as test-30, but with class allocation
  ;;PROTECTION is gone.
  ;;(should-error
  ;;     (eval
  ;;      '(defclass slotattr-fail (slotattr-class-base)
  ;;         ((protection :protection :public)
  ;;          )
  ;;         "This class should throw an error.")))
  (should-error
      (eval
       '(defclass slotattr-fail (slotattr-class-base)
	  ((type :type string)
	   )
	  "This class should throw an error.")))
  (should (eq (oref-default 'slotattr-class-ok initform) 'no-init)))

(ert-deftest eieio-test-32-slot-attribute-override-2 ()
  (let* ((cv (cl--find-class 'slotattr-ok))
         (slots  (eieio--class-slots cv))
	 (args   (eieio--class-initarg-tuples cv)))
    ;; :initarg should override for subclass
    (should (assoc :initblarg args))

    (dotimes (i (length slots))
      (let* ((slot (aref slots i))
             (props (cl--slot-descriptor-props slot)))
        (cond
         ((eq (cl--slot-descriptor-name slot) 'custom)
          ;; Custom slot attributes must override
          (should (eq (alist-get :custom props) 'string))
          ;; Custom label slot attribute must override
          (should (string= (alist-get :label props) "One String"))
          (let ((grp (alist-get :group props)))
            ;; Custom group slot attribute must combine
            (should (and (memq 'moose grp) (memq 'cow grp)))))
         (t nil))))))

(defvar eitest-CLONETEST1 nil)
(defvar eitest-CLONETEST2 nil)

(ert-deftest eieio-test-32-test-clone-boring-objects ()
  ;; A simple make instance with EIEIO extension
  (should (setq eitest-CLONETEST1 (make-instance 'class-a)))
  (should (setq eitest-CLONETEST2 (clone eitest-CLONETEST1)))

  ;; CLOS form of make-instance
  (should (setq eitest-CLONETEST1 (make-instance 'class-a)))
  (should (setq eitest-CLONETEST2 (clone eitest-CLONETEST1))))

(defclass IT (eieio-instance-tracker)
  ((tracking-symbol :initform IT-list)
   (slot1 :initform 'die))
  "Instance Tracker test object.")

(ert-deftest eieio-test-33-instance-tracker ()
  (let (IT-list IT1)
    (should (setq IT1 (IT)))
    ;; The instance tracker must find this
    (should (eieio-instance-tracker-find 'die 'slot1 'IT-list))
    ;; Test deletion
    (delete-instance IT1)
    (should-not (eieio-instance-tracker-find 'die 'slot1 'IT-list))))

(defclass SINGLE (eieio-singleton)
  ((a-slot :initarg :a-slot :initform t))
  "A Singleton test object.")

(ert-deftest eieio-test-34-singletons ()
  (let ((obj1 (SINGLE))
	(obj2 (SINGLE)))
    (should (eieio-object-p obj1))
    (should (eieio-object-p obj2))
    (should (eq obj1 obj2))
    (should (oref obj1 a-slot))))

(defclass NAMED (eieio-named)
  ((some-slot :initform nil)
   )
  "A class inheriting from eieio-named.")

(ert-deftest eieio-test-35-named-object ()
  (let (N)
    (should (setq N (NAMED :object-name "Foo")))
    (should (string= "Foo" (oref N object-name)))
    (should-error (oref N missing-slot) :type 'invalid-slot-name)
    (oset N object-name "NewName")
    (should (string= "NewName" (oref N object-name)))))

(defclass opt-test1 ()
  ()
  "Abstract base class"
  :abstract t)

(defclass opt-test2 (opt-test1)
  ()
  "Instantiable child")

(ert-deftest eieio-test-36-build-class-alist ()
  (should (= (length (eieio-build-class-alist 'opt-test1 nil)) 2))
  (should (= (length (eieio-build-class-alist 'opt-test1 t)) 1)))

(mapatoms (lambda (a)
            (when (and (fboundp a)
                       (string-match "\\`cl--?generic"
                                     (symbol-name a)))
              (trace-function-background a))))

(defclass eieio--testing () ())

(defmethod constructor :static ((_x eieio--testing) newname &rest _args)
  (list newname 2))

(defun eieio-test-dump-trace ()
  (message "%s" (with-current-buffer "*trace-output*"
                  (goto-char (point-min))
                  (while (re-search-forward "[\0-\010\013-\037]" nil t)
                    (insert (prog1 (format "\\%03o" (char-before))
                              (delete-char -1))))
                  (buffer-string))))
(eieio-test-dump-trace)

(ert-deftest eieio-test-37-obsolete-name-in-constructor ()
  ;; FIXME repeated intermittent failures on hydra and elsewhere (bug#24503).
  :tags '(:unstable)
  (with-current-buffer "*trace-output*"
    (erase-buffer))
  (unwind-protect
      (should (equal (eieio--testing "toto") '("toto" 2)))
    (eieio-test-dump-trace)))

(ert-deftest eieio-autoload ()
  "Tests to see whether reftex-auc has been autoloaded"
  (should
   (fboundp 'eieio--defalias)))


(provide 'eieio-tests)

;;; eieio-tests.el ends here
