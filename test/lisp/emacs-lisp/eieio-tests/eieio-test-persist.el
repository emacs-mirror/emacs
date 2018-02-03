;;; eieio-persist.el --- Tests for eieio-persistent class

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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

(defun persist-test-save-and-compare (original)
  "Compare the object ORIGINAL against the one read fromdisk."

  (eieio-persistent-save original)

  (let* ((file (oref original file))
	 (class (eieio-object-class original))
	 (fromdisk (eieio-persistent-read file class))
	 (cv (cl--find-class class))
	 (slots  (eieio--class-slots cv))
	 )
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
                         (cl--find-class class) oneslot))
	     )

	(if initarg-p
	    (unless (equal origvalue fromdiskvalue)
	      (error "Slot %S Original Val %S != Persistent Val %S"
		     oneslot origvalue fromdiskvalue))
	  ;; Else !initarg-p
	  (unless (equal (cl--slot-descriptor-initform slot) fromdiskvalue)
	    (error "Slot %S Persistent Val %S != Default Value %S"
		   oneslot fromdiskvalue (cl--slot-descriptor-initform slot))))
	))))

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
	 (persist-simple "simple 1" :slot1 'goose :slot2 "testing"
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
	 (persist-:printer "persist" :slot1 'goose :slot2 "testing"
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
	  "persist wos 1"
	  :pnp (persist-not-persistent "pnp 1" :slot1 3)
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
	  "persist woss 1"
	  :pnp (persist-not-persistent-subclass "pnps 1" :slot1 3)
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
          :type (or persist-not-persistent persist-random-class null))))

(ert-deftest eieio-test-multiple-class-slot ()
  (let ((persist
         (persistent-multiclass-slot "random string"
          :slot1 (persistent-random-class)
          :slot2 (persist-not-persistent)
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
	  "persist wols 1"
	  :pnp (list (persist-not-persistent "pnp 1" :slot1 3)
		     (persist-not-persistent "pnp 2" :slot1 4)
		     (persist-not-persistent "pnp 3" :slot1 5))
	  :file (concat default-directory "test-ps5.pt"))))

    (persist-test-save-and-compare persist-wols)
    (delete-file (oref persist-wols file))))

;;; eieio-test-persist.el ends here
