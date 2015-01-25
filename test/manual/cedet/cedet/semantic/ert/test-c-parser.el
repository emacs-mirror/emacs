;;; test-c-parser.el --- Testing the C Bovinator
;;
;; Copyright (C) 2015 Free Software Foundation, Inc.
;;
;; Author: David Engster <deng@randomsample.de>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'ert)

(defun semantic-c-parser-test-ert ()
  (ert-run-tests-batch "test-c-parser-.*"))

;; Generate Tags

(defvar test-c-parser-temp-file
  (make-temp-file "test-c-parser" nil ".cpp"))

(defun test-c-parser-bovinate (str)
  (with-current-buffer (find-file-noselect test-c-parser-temp-file)
    (unless (semantic-active-p)
      (semantic-mode 1))
    (erase-buffer)
    (insert str "\n")
    (semantic-fetch-tags)))

;; Test helpers

(defun test-c-parser-remove-from-plist (element plist)
  (let (new)
    (while (and plist
		(not (eq (car plist) element)))
      (setq new (append new (list (car plist)
				  (cadr plist))))
      (setq plist (cddr plist)))
    (when plist
      (setq plist (cddr plist)))
    (when plist
      (setq new (append new plist)))
    new))

(defsubst test-c-parser-list-of-tags-p (tags)
  (and (listp tags) (semantic-tag-p (car tags))))

(defun test-c-parser-compare-attributes (actual expected &optional ignore)
  (let ((actualattr (semantic-tag-attributes actual))
	(expectattr (semantic-tag-attributes expected)))
    ;; :type is always ignored by default, as it is checked separately.
    (unless (member :type ignore)
      (setq ignore (append ignore '(:type))))
    ;; Remove all ignored attributes.
    (dolist (cur ignore)
      (setq actualattr (test-c-parser-remove-from-plist cur actualattr))
      (setq expectattr (test-c-parser-remove-from-plist cur expectattr)))
    ;; Check attributes.
    (while expectattr
      (let ((attrname (car expectattr))
	    (attrval (nth 1 expectattr)))
	(should (test-c-attribute-exists attrname actualattr))
	(should (test-c-attribute-value attrval
					(plist-get actualattr attrname)
					attrname))
	(setq actualattr
	      (test-c-parser-remove-from-plist attrname actualattr)))
      (setq expectattr (cddr expectattr)))
    (should (test-c-parser-no-attributes-left actualattr))))

(defun test-c-parser-no-attributes-left (remaining)
  (null remaining))

(defun test-c-attribute-exists (name actualattr)
  (member name actualattr))

(defun test-c-attribute-value (expect actual name)
  ;; List of tags have to be checked recursively.
  (if (and (test-c-parser-list-of-tags-p expect)
	   (test-c-parser-list-of-tags-p actual))
      (progn
	(while expect
	  (test-c-parser-compare-tag (car expect)
				     (car actual))
	  (setq expect (cdr expect)
		actual (cdr actual)))
	t)
    (equal expect actual)))

(defun test-c-check-tags-length (tags expect)
  (= (length tags) expect))

(defun test-c-parser-compare-tag (actual expect)
  ;; Check that it is a tag.
  (should (semantic-tag-p actual))
  ;; Check name.
  (should (string= (semantic-tag-name actual)
		   (semantic-tag-name expect)))
  ;; Check class.
  (should (semantic-tag-of-class-p
	   actual
	   (semantic-tag-class expect)))
  ;; Check type.
  (should (semantic-tag-of-type-p
	   actual
	   (semantic-tag-type expect)))
  ;; Compare attributes.
  (test-c-parser-compare-attributes actual expect))

;;; ERT explainers

(put 'test-c-check-tags-length 'ert-explainer
     (lambda (tags expect)
       (format "Wrong number of tags. Expected: %d, actual: %d."
	       expect (length tags))))

(put 'test-c-parser-no-attributes-left 'ert-explainer
     (lambda (remaining)
       (format "Tag has unexpected attribute(s): %s."
	       (prin1-to-string remaining))))

(put 'test-c-attribute-exists 'ert-explainer
     (lambda (name attributes)
       (format "Tag is missing attribute: %s."
	       (symbol-name name))))

(put 'test-c-attribute-value 'ert-explainer
     (lambda (expect actual name)
       (format "Wrong value for attribute %s. Expected: %s, actual: %s."
	       (symbol-name name)
	       (prin1-to-string expect)
	       (prin1-to-string actual))))

(put 'semantic-tag-p 'ert-explainer
     (lambda (tag)
       (if (null tag)
	   "Parser returned nil for expression."
	 "Parser did not return valid tag.")))

(put 'semantic-tag-of-class-p 'ert-explainer
     (lambda (tag class)
       (format "Wrong class. Expected: %s, actual: %s."
	       (symbol-name class)
	       (semantic-tag-class tag))))

(put 'semantic-tag-of-type-p 'ert-explainer
     (lambda (tag type)
       (format "Wrong type. Expected: %s, actual: %s"
	       (semantic-tag-type tag) type)))

;;; Tests

;;;; Enum

(ert-deftest test-c-parser-enum-01 ()
  (let ((actual
	 (test-c-parser-bovinate "enum foo { RED, GREEN, BLUE };"))
	(expect '("foo" type
		  (:members
		   (("RED" variable
		     (:constant-flag t :type "int"))
		    ("GREEN" variable
		     (:constant-flag t :type "int"))
		    ("BLUE" variable
		     (:constant-flag t :type "int")))
		   :type "enum"))))
    (should (test-c-check-tags-length actual 1))
    (setq actual (car actual))
    (test-c-parser-compare-tag actual expect)))

(ert-deftest test-c-parser-enum-02-typed ()
  (let ((actual
	 (test-c-parser-bovinate "enum foo : unsigned { RED, GREEN, BLUE };"))
	(expect '("foo" type
		  (:enum-type
		   "unsigned int"
		   :members
		   (("RED" variable
		     (:constant-flag t :type "unsigned int"))
		    ("GREEN" variable
		     (:constant-flag t :type "unsigned int"))
		    ("BLUE" variable
		     (:constant-flag t :type "unsigned int")))
		   :type "enum"))))
	(should (test-c-check-tags-length actual 1))
	(setq actual (car actual))
	(test-c-parser-compare-tag actual expect)))

;;;; Typedef

(ert-deftest test-c-parser-typedef-01 ()
  (let ((actual
	 (test-c-parser-bovinate "typedef int foo;"))
	(expect '("foo" type (:typedef ("int") :type "typedef"))))
    (should (test-c-check-tags-length actual 1))
    (setq actual (car actual))
    (test-c-parser-compare-tag actual expect)))

(ert-deftest test-c-parser-typedef-02-pointer ()
  (let ((actual
	 (test-c-parser-bovinate "typedef int* foo;"))
	(expect '("foo" type (:typedef ("int") :pointer 1 :type "typedef"))))
    (should (test-c-check-tags-length actual 1))
    (setq actual (car actual))
    (test-c-parser-compare-tag actual expect)))

(ert-deftest test-c-parser-typedef-03-reference ()
  (let ((actual
	 (test-c-parser-bovinate "typedef int& foo;"))
	(expect '("foo" type (:typedef ("int") :reference 1 :type "typedef"))))
    (should (test-c-check-tags-length actual 1))
    (setq actual (car actual))
    (test-c-parser-compare-tag actual expect)))

(provide 'cedet/semantic/ert/test-c-parser)
