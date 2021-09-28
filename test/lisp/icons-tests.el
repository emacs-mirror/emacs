;;; icons-tests.el --- tests for icons.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

(require 'ert)
(require 'ert-x)
(require 'icons)

(defmacro with-icons-test (&rest body)
  (declare (indent defun))
  `(let (icons-alist
         (image-load-path (cons
                           (ert-resource-directory)
                           image-load-path))
         (icons-format-priority '(svg xpm pbm)))
     (icons-define-set 'set1 `(("apple" "apple2.svg" 24)
                         ("orange" "orange.svg" 24)
                         ("lemon" "lemon.svg")))
     (icons-define-set 'set2 `(("apple" "apple.svg")))
     (icons-define-set 'xpmset `(("apple" "apple.xpm")))
     (icons-define-set 'pbmset `(("apple" "apple.pbm")))
     ,@body))

;;;; Data.

(ert-deftest icons-define-set ()
  (with-icons-test
    (should (= (length (cdr (assoc "apple" icons-alist))) 4))))

;;;; Inserting and getting icons.

(ert-deftest icons--get-icon ()
  (with-icons-test
    (let ((icon (cdr (icons--get-icon "apple"))))
      (plist-get icon :file)
      (should (string-match "apple\.svg\\'"
                            (plist-get icon :file))))))

(ert-deftest icons--get-icon/missing ()
  (with-icons-test
    (should-error (icons--get-icon "foo-missing-icon"))))

(ert-deftest icons--get-sorted-icons/format-priority ()
  (with-icons-test
    (should (eq (icons-icon-type (car (last (icons--get-sorted-icons "apple"))))
                (car (last icons-format-priority))))))

(ert-deftest icons--get-sorted-icons/set-priority ()
  (let ((icons-set-priority '(set1 set2)))
    (with-icons-test
      (should (equal (icons-icon-filename (car (icons--get-sorted-icons "apple")))
                     "apple2.svg"))))
  (let ((icons-set-priority '(set2 set1)))
    (with-icons-test
      (should (equal (icons-icon-filename (car (icons--get-sorted-icons "apple")))
                     "apple.svg")))))

(ert-deftest icons-get/returns-space ()
  (with-icons-test
    (should (equal (with-icons-test (icons-get "apple")) " "))))

(ert-deftest icons-get/has-display-property ()
  (should (get-text-property 0 'display (with-icons-test (icons-get "apple")))))

(ert-deftest icons-get-icon ()
  (should (eq (car (with-icons-test (icons-get-for-modeline "apple")))
              :propertize)))

(ert-deftest icons-tests--remove-set ()
  (with-icons-test
   (icons--remove-set 'set1)
   (icons--remove-set 'set2)
   (icons--remove-set 'xpmset)
   (icons--remove-set 'pbmset)
   (should (not icons-alist))))

;; (ert-deftest icons-add-icon ()
;;   (let (icons-alist
;;         (icon (icons-icon-create :filename "bar" :set 'set)))
;;     (icons-add-icon "foo" icon)
;;     (should (assoc "foo" icons-alist))
;;     ;; Invalid names.
;;     (should-error (icons-add-icon nil icon))
;;     (should-error (icons-add-icon 'foo icon))
;;     ;; Invalid icons.
;;     (should-error (icons-add-icon "foo" "not an icon"))))

;; (ert-deftest test-list-make-entries ()
;;   (with-icons-test
;;    (let ((entries (icons-list-make-entries)))
;;      (should (listp entries))
;;      (should (= (length entries) 2)))))

;; (ert-deftest test-icons--filename-for-size/string ()
;;   (should (equal (icons--filename-for-size 20 "foobar")
;;                  "foobar")))

;; (ert-deftest test-icons--filename-for-size/alist ()
;;   (should (equal (icons--filename-for-size 20 '((5 . "foo")
;;                                            (10 . "bar")))
;;                  "bar")))

;;;; Util.

(ert-deftest test-icons--find-closest ()
  (should (= (icons--closest-to 14 '(10 20)) 10))
  (should (= (icons--closest-to 15 '(10 20)) 20))
  (should (= (icons--closest-to 8 '(6 12)) 6))
  (should (= (icons--closest-to 9 '(6 12)) 12))
  (should (= (icons--closest-to 14 '(10 18 20)) 18)))

;;; icons-tests.el ends here
