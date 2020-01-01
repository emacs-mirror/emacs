;;; eieio-test-persist.el --- Tests for eieio-persistent class

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

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
;; The eieio-persistent base-class provides a vital service, that
;; could be used to accidentally load in malicious code.  As such,
;; something as simple as calling eval on the generated code can't be
;; used.  These tests exercises various flavors of data that might be
;; in a persistent object, and tries to save/load them.

;;; Code:
(require 'eieio)
(require 'eieio-base)
(require 'ert)

(defun eieio--attribute-to-initarg (class attribute)
  "In CLASS, convert the ATTRIBUTE into the corresponding init argument tag.
This is usually a symbol that starts with `:'."
  (let ((tuple (rassoc attribute (eieio--class-initarg-tuples class))))
    (if tuple
	(car tuple)
      nil)))

(defun hash-equal (hash1 hash2)
  "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag
         (maphash (lambda (x y)
                    (or (equal (gethash x hash2) y)
                        (throw 'flag nil)))
                  hash1)
         (throw 'flag t))))

(defun persist-test-save-and-compare (original)
  "Compare the object ORIGINAL against the one read fromdisk."

  (eieio-persistent-save original)

  (let* ((file (oref original file))
	 (class (eieio-object-class original))
	 (fromdisk (eieio-persistent-read file class))
	 (cv (cl--find-class class))
	 (slots  (eieio--class-slots cv)))

    (unless (object-of-class-p fromdisk class)
      (error "Persistent class %S != original class %S"
	     (eieio-object-class fromdisk)
	     class))

    (dotimes (i (length slots))
      (let* ((slot (aref slots i))
             (oneslot (cl--slot-descriptor-name slot))
	     (origvalue (eieio-oref original oneslot))
	     (fromdiskvalue (eieio-oref fromdisk oneslot))
	     (initarg-p (eieio--attribute-to-initarg
                         (cl--find-class class) oneslot)))

	(if initarg-p
	    (unless
		(cond ((and (hash-table-p origvalue) (hash-table-p fromdiskvalue))
		       (hash-equal origvalue fromdiskvalue))
		      (t (equal origvalue fromdiskvalue)))
	      (error "Slot %S Original Val %S != Persistent Val %S"
		     oneslot origvalue fromdiskvalue))
	  ;; Else !initarg-p
	  (let ((origval (cl--slot-descriptor-initform slot))
		(diskval fromdiskvalue))
	    (unless
		(cond ((and (hash-table-p origval) (hash-table-p diskval))
		       (hash-equal origval diskval))
		      (t (equal origval diskval)))
	    (error "Slot %S Persistent Val %S != Default Value %S"
		   oneslot diskval origvalue))))))))

;;; Simple Case
;;
;; Simplest case is a mix of slots with and without initargs.

(defclass persist-simple (eieio-persistent)
  ((slot1 :initarg :slot1
	  :type symbol
	  :initform moose)
   (slot2 :initarg :slot2
	  :initform "foo")
   (slot3 :initform 2))
  "A Persistent object with two initializable slots, and one not.")

(ert-deftest eieio-test-persist-simple-1 ()
  (let ((persist-simple-1
	 (persist-simple :slot1 'goose :slot2 "testing"
			 :file (concat default-directory "test-ps1.pt"))))
    (should persist-simple-1)

    ;; When the slot w/out an initarg has not been changed
    (persist-test-save-and-compare persist-simple-1)

    ;; When the slot w/out an initarg HAS been changed
    (oset persist-simple-1 slot3 3)
    (persist-test-save-and-compare persist-simple-1)
    (delete-file (oref persist-simple-1 file))))

;;; Slot Writers
;;
;; Replica of the test in eieio-tests.el -

(defclass persist-:printer (eieio-persistent)
  ((slot1 :initarg :slot1
	  :initform 'moose
	  :printer PO-slot1-printer)
   (slot2 :initarg :slot2
	  :initform "foo"))
  "A Persistent object with two initializable slots.")

(defun PO-slot1-printer (slotvalue)
  "Print the slot value SLOTVALUE to stdout.
Assume SLOTVALUE is a symbol of some sort."
  (princ "'")
  (princ (symbol-name slotvalue))
  (princ " ;; RAN PRINTER")
  nil)

(ert-deftest eieio-test-persist-printer ()
  (let ((persist-:printer-1
	 (persist-:printer :slot1 'goose :slot2 "testing"
			   :file (concat default-directory "test-ps2.pt"))))
    (should persist-:printer-1)
    (persist-test-save-and-compare persist-:printer-1)

    (let* ((find-file-hook nil)
	   (tbuff (find-file-noselect "test-ps2.pt"))
	   )
      (condition-case nil
	  (unwind-protect
	      (with-current-buffer tbuff
		(goto-char (point-min))
		(re-search-forward "RAN PRINTER"))
	    (kill-buffer tbuff))
	(error "persist-:printer-1's Slot1 printer function didn't work.")))
    (delete-file (oref persist-:printer-1 file))))

;;; Slot with Object
;;
;; A slot that contains another object that isn't persistent
(defclass persist-not-persistent ()
  ((slot1 :initarg :slot1
	  :initform 1)
   (slot2 :initform 2))
  "Class for testing persistent saving of an object that isn't
persistent.  This class is instead used as a slot value in a
persistent class.")

(defclass persistent-with-objs-slot (eieio-persistent)
  ((pnp :initarg :pnp
	:type (or null persist-not-persistent)
	:initform nil))
  "Class for testing the saving of slots with objects in them.")

(ert-deftest eieio-test-non-persistent-as-slot ()
  (let ((persist-wos
	 (persistent-with-objs-slot
	  :pnp (persist-not-persistent :slot1 3)
	  :file (concat default-directory "test-ps3.pt"))))

    (persist-test-save-and-compare persist-wos)
    (delete-file (oref persist-wos file))))

;;; Slot with Object child of :type
;;
;; A slot that contains another object that isn't persistent
(defclass persist-not-persistent-subclass (persist-not-persistent)
  ((slot3 :initarg :slot1
	  :initform 1)
   (slot4 :initform 2))
  "Class for testing persistent saving of an object subclass that isn't
persistent.  This class is instead used as a slot value in a
persistent class.")

(defclass persistent-with-objs-slot-subs (eieio-persistent)
  ((pnp :initarg :pnp
	:type (or null persist-not-persistent)
	:initform nil))
  "Class for testing the saving of slots with objects in them.")

(ert-deftest eieio-test-non-persistent-as-slot-child ()
  (let ((persist-woss
	 (persistent-with-objs-slot-subs
	  :pnp (persist-not-persistent-subclass :slot1 3)
	  :file (concat default-directory "test-ps4.pt"))))

    (persist-test-save-and-compare persist-woss)
    (delete-file (oref persist-woss file))))

;; A slot that can contain one of two different classes, to exercise
;; the `or' slot type.

(defclass persistent-random-class ()
  ())

(defclass persistent-multiclass-slot (eieio-persistent)
  ((slot1 :initarg :slot1
          :type (or persistent-random-class null persist-not-persistent))
   (slot2 :initarg :slot2
          :type (or persist-not-persistent persistent-random-class null))
   (slot3 :initarg :slot3
          :type persistent-random-class)))

(ert-deftest eieio-test-multiple-class-slot ()
  (let ((persist
         (persistent-multiclass-slot
          :slot1 (persistent-random-class)
          :slot2 (persist-not-persistent)
          :slot3 (persistent-random-class)
          :file (concat default-directory "test-ps5.pt"))))
    (unwind-protect
        (persist-test-save-and-compare persist)
     (ignore-errors (delete-file (oref persist file))))))

;;; Slot with a list of Objects
;;
;; A slot that contains another object that isn't persistent
(defclass persistent-with-objs-list-slot (eieio-persistent)
  ((pnp :initarg :pnp
	:type (list-of persist-not-persistent)
	:initform nil))
  "Class for testing the saving of slots with objects in them.")

(ert-deftest eieio-test-slot-with-list-of-objects ()
  (let ((persist-wols
	 (persistent-with-objs-list-slot
	  :pnp (list (persist-not-persistent :slot1 3)
		     (persist-not-persistent :slot1 4)
		     (persist-not-persistent :slot1 5))
	  :file (concat default-directory "test-ps5.pt"))))

    (persist-test-save-and-compare persist-wols)
    (delete-file (oref persist-wols file))))

;;; Tests targeted at popular libraries in the wild.

;; Objects inside hash tables and vectors (pcache), see bug#29220.
(defclass person ()
  ((name :type string :initarg :name)))

(defclass classy (eieio-persistent)
  ((teacher
    :type person
    :initarg :teacher)
   (students
    :initarg :students :initform (make-hash-table :test 'equal))
   (janitors
    :type list
    :initarg :janitors)
   (random-vector
    :type vector
    :initarg :random-vector)))

(defun eieio-test-persist-hash-and-vector ()
  (let* ((jane (make-instance 'person :name "Jane"))
         (bob  (make-instance 'person :name "Bob"))
         (hans (make-instance 'person :name "Hans"))
         (dierdre (make-instance 'person :name "Dierdre"))
         (class (make-instance 'classy
			       :teacher jane
                               :janitors (list [tuesday nil]
                                              [friday nil])
                               :random-vector [nil]
			       :file (concat default-directory "classy-" emacs-version ".eieio"))))
    (puthash "Bob" bob (slot-value class 'students))
    (aset (slot-value class 'random-vector) 0
          (make-instance 'persistent-random-class))
    (unwind-protect
        (persist-test-save-and-compare class)
      (delete-file (oref class file)))
    (aset (car (slot-value class 'janitors)) 1 hans)
    (aset (nth 1 (slot-value class 'janitors)) 1 dierdre)
    (unwind-protect
        (persist-test-save-and-compare class)
      (delete-file (oref class file)))))

(ert-deftest eieio-persist-hash-and-vector-backward-compatibility ()
  (let ((eieio-backward-compatibility t)) ; The default.
    (eieio-test-persist-hash-and-vector)))

(ert-deftest eieio-persist-hash-and-vector-no-backward-compatibility ()
  :expected-result :failed ;; Bug#29220.
  (let ((eieio-backward-compatibility nil))
    (eieio-test-persist-hash-and-vector)))

;; Extra quotation of lists inside other objects (Gnus registry), also
;; bug#29220.

(defclass eieio-container (eieio-persistent)
  ((alist
    :initarg :alist
    :type list)
   (vec
    :initarg :vec
    :type vector)
   (htab
    :initarg :htab
    :type hash-table)))

(defun eieio-test-persist-interior-lists ()
  (let* ((thing (make-instance
                 'eieio-container
                 :vec [nil]
                 :htab (make-hash-table :test #'equal)
                 :file (concat default-directory
                               "container-" emacs-version ".eieio")))
         (john (make-instance 'person :name "John"))
         (alexie (make-instance 'person :name "Alexie"))
         (alst '(("first" (one two three))
                 ("second" (four five six)))))
    (setf (slot-value thing 'alist) alst)
    (puthash "alst" alst (slot-value thing 'htab))
    (aset (slot-value thing 'vec) 0 alst)
    (unwind-protect
        (persist-test-save-and-compare thing)
      (delete-file (slot-value thing 'file)))
    (setf (nth 2 (cadar alst)) john
          (nth 2 (cadadr alst)) alexie)
    (unwind-protect
        (persist-test-save-and-compare thing)
      (delete-file (slot-value thing 'file)))))

(ert-deftest eieio-test-persist-interior-lists-backward-compatibility ()
  (let ((eieio-backward-compatibility t)) ; The default.
    (eieio-test-persist-interior-lists)))

(ert-deftest eieio-test-persist-interior-lists-no-backward-compatibility ()
  :expected-result :failed ;; Bug#29220.
  (let ((eieio-backward-compatibility nil))
    (eieio-test-persist-interior-lists)))

;;; eieio-test-persist.el ends here
