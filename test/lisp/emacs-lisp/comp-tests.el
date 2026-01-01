;;; comp-tests.el --- Tests for comp.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'comp)

(defvar comp-native-version-dir)
(defvar native-comp-eln-load-path)

(defmacro with-test-native-compile-prune-cache (&rest body)
  (declare (indent 0) (debug t))
  `(ert-with-temp-directory testdir
     (let ((usr-cache (expand-file-name "eln-usr-cache" testdir))
	   (sys-cache (expand-file-name "eln-sys-cache" testdir)))
       (make-directory usr-cache)
       (make-directory sys-cache)
       (let* ((c1 (expand-file-name "29.0.50-cur" usr-cache))
              (c2 (expand-file-name "29.0.50-old" usr-cache))
	      (s1 (expand-file-name "29.0.50-cur" sys-cache))
	      (s2 (expand-file-name "preloaded" s1))
              (native-comp-eln-load-path (list usr-cache sys-cache))
              (comp-native-version-dir "29.0.50-cur"))
	 (dolist (d (list c1 c2 s1 s2))
           (make-directory d)
           (with-temp-file (expand-file-name "some.eln" d) (insert "foo"))
           (with-temp-file (expand-file-name "some.eln.tmp" d) (insert "foo")))
	 ,@body))))

(ert-deftest test-native-compile-prune-cache ()
  (skip-unless (featurep 'native-compile))
  (with-test-native-compile-prune-cache
    (native-compile-prune-cache)
    (dolist (d (list c1 s1 s2))
      (should (file-directory-p d))
      (should (file-regular-p (expand-file-name "some.eln" d)))
      (should (file-regular-p (expand-file-name "some.eln.tmp" d))))
    (should-not (file-directory-p c2))
    (should-not (file-regular-p (expand-file-name "some.eln" c2)))
    (should-not (file-regular-p (expand-file-name "some.eln.tmp" c2)))))

(ert-deftest test-native-compile-prune-cache/delete-only-eln ()
  (skip-unless (featurep 'native-compile))
  (with-test-native-compile-prune-cache
    (dolist (d (list c1 c2 s1 s2))
      (with-temp-file (expand-file-name "keep.txt" d) (insert "foo")))
    (native-compile-prune-cache)
    (dolist (d (list c1 c2 s1 s2))
      (should (file-regular-p (expand-file-name "keep.txt" d))))))

(ert-deftest test-native-compile-prune-cache/dont-delete-in-parent-of-cache ()
  (skip-unless (featurep 'native-compile))
  (with-test-native-compile-prune-cache
    (let ((f1 (expand-file-name "../some.eln" usr-cache))
          (f2 (expand-file-name "some.eln" usr-cache))
	  (f3 (expand-file-name "../some.eln" sys-cache))
	  (f4 (expand-file-name "some.eln" sys-cache)))
      (dolist (f (list f1 f2 f3 f4))
	(with-temp-file f (insert "foo")))
      (native-compile-prune-cache)
      (dolist (f (list f1 f2 f3 f4))
	(should (file-regular-p f))))))

;;; comp-tests.el ends here
