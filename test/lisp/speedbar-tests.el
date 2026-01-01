;;; speedbar-tests.el --- Tests for speedbar.el  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'eieio-base)
(require 'eieio-speedbar)

(defclass speedbar-tests-container (eieio-named eieio-speedbar-file-button)
  ((child-items :initarg :child-items
                :type list))
  "An expandable Speedbar item which can contain other items.")

(cl-defmethod eieio-speedbar-object-children ((item speedbar-tests-container))
  "Return the list of child items for ITEM."
  (slot-value item 'child-items))

(defclass speedbar-tests-item (eieio-named eieio-speedbar)
  nil
  "A Speedbar item which cannot contain other items.")

(defun speedbar-tests--make-object (item-spec)
  "Return an object representing a Speedbar item.

The object is constructed based on the specification ITEM-SPEC which
should be a cons pair of the form (NAME . CHILD-ITEMS).  NAME is a
string which will be used for display purposes.  CHILD-ITEMS is a list
of additional ITEM-SPEC values which will be referenced as children."
  (let ((name (car item-spec))
        (child-items (cdr item-spec)))
    (unless (stringp name)
      (error "Item name must be a string"))
    (unless (listp child-items)
      (error "Child-items must be a list"))
    (if child-items
        (speedbar-tests-container
         :object-name name
         :child-items (mapcar #'speedbar-tests--make-object
                              child-items))
      (speedbar-tests-item
       :object-name name))))

(defvar speedbar-tests--setup-strings nil
  "An alist of strings which represents a hierarchy of Speedbar items.")

(defvar speedbar-tests--object-hierarchy nil
  "The current object hierarchy for the Speedbar being tested.")

(defun speedbar-tests--base-items (_directory)
  "Return the list of top-level objects for the Speedbar."
  (setq speedbar-tests--object-hierarchy
        (mapcar #'speedbar-tests--make-object
                speedbar-tests--setup-strings)))

(eieio-speedbar-create #'eieio-speedbar-make-map
		       'eieio-speedbar-key-map
		       'eieio-speedbar-menu
		       "Test"
		       #'speedbar-tests--base-items)

(defun speedbar-tests--clean-up ()
  "Clean-up after Speedbar test."
  (when (framep speedbar-frame)
    (delete-frame speedbar-frame)))

(defun speedbar-tests--initialize ()
  "Initialize a Speedbar for testing."
  (speedbar-get-focus)
  (speedbar-change-initial-expansion-list "Test"))

(defun speedbar-tests--object-name-expanded (object)
  "Return the string name of OBJECT when it is expanded."
  (let ((name (eieio-speedbar-object-buttonname object)))
    (if (slot-value object 'expanded)
        (concat name "+")
      name)))

(defvar speedbar-tests--object-name-function
  #'eieio-speedbar-object-buttonname
  "The function which returns the string representation of an object.")

(defun speedbar-tests--objects-as-strings (object-list)
  "Return the object hierarchy OBJECT-LIST as an alist of strings.

The string used to represent the object is determined by the function
bound to `speedbar-tests--object-name-function' is a function, which
should accept the object as the only argument and return a string to use
as the name."
  (mapcar (lambda (object)
            (let ((name (funcall speedbar-tests--object-name-function
                                 object))
                  (child-items (eieio-speedbar-object-children
                                object)))
              (cons name (speedbar-tests--objects-as-strings
                          child-items))))
          object-list))

(cl-defmacro speedbar-tests--state-test
    ((&optional &key setup final name-function) &rest body)
  "Evaluate BODY and verify the Speedbar is in an expected state.

`:setup' specifies an alist of strings which will be used to create an
object hierarchy used for the Speedbar display.

`:final' specifies an alist of strings which should represent the final
Speedbar state once BODY has been evaluated and the object hierarchy has
been converted back to an alist of strings.  `:name-function' specifies
the function to use to generate a string from an object, which should
accept the object as an argument and return a string which represents
the object as well as its state."
  (declare (indent 1))
  (let ((let-vars `((speedbar-tests--setup-strings ',setup))))
    (when name-function
      (push `(speedbar-tests--object-name-function #',name-function)
            let-vars))
    `(unwind-protect
         (let ,let-vars
           (speedbar-tests--initialize)
           (should (equal (speedbar-tests--objects-as-strings
                           speedbar-tests--object-hierarchy)
                          ',setup))
           ,@body
           (should (equal (speedbar-tests--objects-as-strings
                           speedbar-tests--object-hierarchy)
                          ',final)))
       (speedbar-tests--clean-up))))

(ert-deftest speedbar-tests--expand-descendants-single ()
  "Expand the first item."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"))))
        :final (("A+" . (("A1"))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (should (string-equal "A" (speedbar-line-text)))
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-nested ()
  "Expand the first item and its only child."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A"))))))
        :final (("A+" . (("A1+" . (("A1A"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (should (string-equal "A" (speedbar-line-text)))
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-nested-wide ()
  "Expand all descendants of first item which has multiple children."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A"))))))
        :final (("A+" . (("A1+" . (("A1A")))
                         ("A2+" . (("A2A"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (should (string-equal "A" (speedbar-line-text)))
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-of-first ()
  "Expand the first item and all descendants."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B"))))))
        :final (("A+" . (("A1+" . (("A1A")))
                         ("A2+" . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (should (string-equal "A" (speedbar-line-text)))
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-of-first-expanded ()
  "Expand the already expanded first item and all descendants."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B"))))))
        :final (("A+" . (("A1+" . (("A1A")))
                         ("A2+" . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (should (string-equal "A" (speedbar-line-text)))
      (speedbar-expand-line 'nocache)
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-of-last ()
  "Expand the last item and all descendants."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B"))))))
        :final (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B+" . (("B1+" . (("B1B")))
                         ("B2+" . (("B2B"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (forward-line)
      (should (string-equal "B" (speedbar-line-text)))
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-of-last-expanded ()
  "Expand the already expanded last item and all descendants."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B"))))))
        :final (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B+" . (("B1+" . (("B1B")))
                         ("B2+" . (("B2B"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (save-excursion
        (forward-line)
        (should (string-equal "B" (speedbar-line-text)))
        (speedbar-expand-line 'nocache))
      (save-excursion
        (forward-line)
        (should (string-equal "B" (speedbar-line-text)))
        (speedbar-expand-line-descendants 'nocache)))))

(ert-deftest speedbar-tests--expand-descendants-of-middle ()
  "Expand the middle item and all descendants."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B")))))
                ("C"  . (("C1"  . (("C1C")))
                         ("C2"  . (("C2C"))))))
        :final (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B+" . (("B1+" . (("B1B")))
                         ("B2+" . (("B2B")))))
                ("C"  . (("C1"  . (("C1C")))
                         ("C2"  . (("C2C"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (goto-char (point-min))
      (forward-line)
      (should (string-equal "B" (speedbar-line-text)))
      (speedbar-expand-line-descendants 'nocache))))

(ert-deftest speedbar-tests--expand-descendants-of-middle-expanded ()
  "Expand the already expanded middle item and all descendants."
  (skip-when noninteractive)
  (speedbar-tests--state-test
      ( :setup (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B"  . (("B1"  . (("B1B")))
                         ("B2"  . (("B2B")))))
                ("C"  . (("C1"  . (("C1C")))
                         ("C2"  . (("C2C"))))))
        :final (("A"  . (("A1"  . (("A1A")))
                         ("A2"  . (("A2A")))))
                ("B+" . (("B1+" . (("B1B")))
                         ("B2+" . (("B2B")))))
                ("C"  . (("C1"  . (("C1C")))
                         ("C2"  . (("C2C"))))))
        :name-function speedbar-tests--object-name-expanded)
    (with-current-buffer speedbar-buffer
      (goto-char (point-min))
      (save-excursion
        (forward-line)
        (should (string-equal "B" (speedbar-line-text)))
        (speedbar-expand-line 'nocache))
      (save-excursion
        (forward-line)
        (should (string-equal "B" (speedbar-line-text)))
        (speedbar-expand-line-descendants 'nocache)))))

(provide 'speedbar-tests)
;;; speedbar-tests.el ends here
