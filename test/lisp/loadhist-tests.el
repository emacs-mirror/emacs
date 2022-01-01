;;; loadhist-tests.el --- Tests for loadhist.el  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;;; loadhist-tests.el ends here
