;;; loadhist-tests.el --- Tests for loadhist.el  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'loadhist)

(ert-deftest loadhist-tests-feature-symbols ()
  (should (equal (file-name-base (car (feature-symbols 'loadhist))) "loadhist"))
  (should-not (feature-symbols 'non-existent-feature)))

(ert-deftest loadhist-tests-feature-file ()
  (should (equal (file-name-base (feature-file 'loadhist)) "loadhist"))
  (should-error (feature-file 'non-existent-feature)))

(ert-deftest loadhist-tests-file-loadhist-lookup ()
  ;; This should probably be extended...
  (should (listp (file-loadhist-lookup "loadhist"))))

(ert-deftest loadhist-tests-file-provides ()
  (should (eq (car (file-provides "loadhist")) 'loadhist)))

(ert-deftest loadhist-tests-file-requires ()
  (should-not (file-requires "loadhist")))

(ert-deftest loadhist-tests-file-dependents ()
  (require 'dired-x)
  (let ((deps (file-dependents "dired")))
    (should (member "dired-x" (mapcar #'file-name-base deps)))))

(ert-deftest loadhist-tests-unload-feature ()
  (require 'dired-x)
  (should-error (unload-feature 'dired))
  (unload-feature 'dired-x))

(defvar loadhist--tests-dir (file-name-directory (macroexp-file-name)))

(ert-deftest loadhist-tests-unload-feature-nested ()
  (add-to-list 'load-path (expand-file-name
                           "loadhist-resources/"
                           loadhist--tests-dir))
  (declare-function loadhist--foo-inc "loadhist--foo")
  (declare-function loadhist--bar-dec "loadhist--dec")
  (load "loadhist--foo" nil t)
  (should (and (functionp 'loadhist--bar-dec) (functionp 'loadhist--foo-inc)))
  (should (autoloadp (symbol-function 'loadhist--bar-dec)))
  (load "loadhist--bar" nil t)
  (should (and (functionp 'loadhist--bar-dec) (functionp 'loadhist--foo-inc)))
  (should (not (autoloadp (symbol-function 'loadhist--bar-dec))))
  (should (not (autoloadp (symbol-function 'loadhist--foo-inc))))
  (should (equal (list 40 42)
                 (list (loadhist--bar-dec 41) (loadhist--foo-inc 41))))
  (unload-feature 'loadhist--bar)
  (should (and (functionp 'loadhist--bar-dec) (functionp 'loadhist--foo-inc)))
  (should (autoloadp (symbol-function 'loadhist--bar-dec)))
  (should (not (autoloadp (symbol-function 'loadhist--foo-inc))))
  (unload-feature 'loadhist--foo)
  (should (null (symbol-function 'loadhist--bar-dec)))
  (should (null (symbol-function 'loadhist--foo-inc)))
  (should (null (get 'loadhist--bar-dec 'function-history)))
  (should (null (get 'loadhist--foo-inc 'function-history))))

(ert-deftest loadhist-tests-unload-feature-notnested ()
  (add-to-list 'load-path (expand-file-name
                           "loadhist-resources/"
                           loadhist--tests-dir))
  (load "loadhist--foo" nil t)
  (load "loadhist--bar" nil t)
  (should (equal (list 40 42)
                 (list (loadhist--bar-dec 41) (loadhist--foo-inc 41))))
  (unload-feature 'loadhist--foo)
  (should (functionp 'loadhist--bar-dec))
  (should (not (autoloadp (symbol-function 'loadhist--bar-dec))))
  (should  (let ((f (symbol-function 'loadhist--foo-inc)))
             ;; Both choices seem acceptable.
             (or (null f) (autoloadp f))))
  (unload-feature 'loadhist--bar)
  (should (null (symbol-function 'loadhist--bar-dec)))
  (should (null (symbol-function 'loadhist--foo-inc)))
  (should (null (get 'loadhist--bar-dec 'function-history)))
  (should (null (get 'loadhist--foo-inc 'function-history))))

(ert-deftest loadhist-test-unload-feature-alias ()
  "Check that bug#76748 has been fixed."
  (add-to-list 'load-path (expand-file-name
                           "loadhist-resources/"
                           loadhist--tests-dir))
  (load "loadhist--alias" nil t)
  (unload-feature 'loadhist--alias))

;;; loadhist-tests.el ends here
