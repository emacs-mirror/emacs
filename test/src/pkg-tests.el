;;; pkg-tests.el --- tests for src/pkg.c  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
(require 'cl-lib)

(defmacro with-packages (packages &rest body)
  (declare (indent 1))
  (let (vars shoulds makes deletions)
    (dolist (p packages)
      (let ((name (if (consp p) (cl-first p) p))
            (options (if (consp p) (cl-rest p))))
        (push `(,name nil) vars)
        (push `(should (not (find-package ',name))) shoulds)
        (push `(setq ,name (make-package ',name ,@options)) makes)
        (push `(when (packagep ,name) (delete-package ,name)) deletions)))
    `(let (,@vars)
       ,@(nreverse shoulds)
       (unwind-protect
	   (progn ,@(nreverse makes) ,@body)
	 ,@(nreverse deletions)))))

(ert-deftest pkg-tests-packagep ()
  (should (packagep (make-package "x")))
  (should (not (packagep "emacs")))
  (should (not (packagep nil))))

(ert-deftest pkg-tests-*package* ()
  (should (eq (let ((*package* (find-package "emacs"))) 'good) 'good))
  (should-error (let ((*package* :emacs)) nil))
  (should-error (let ((*package* 1)) nil))
  (should-error (setq *package* :keyword))
  (should-error (makunbound *package*))
  (with-temp-buffer
    (in-package* :emacs-user)
    (kill-all-local-variables)
    (should (eq *package* (find-package :emacs)))))

(ert-deftest pkg-tests-standard-packages ()
  (should (packagep (find-package "emacs")))
  (should (packagep (find-package 'emacs)))
  (should (packagep (find-package :emacs)))
  (should (packagep (find-package "keyword")))
  (should (packagep (find-package "")))
  (should (eq (find-package "keyword") (find-package ""))))

(ert-deftest pkg-tests-make-package ()
  ;; Valid package names
  (dolist (name '(?a "a" :a a))
    (let ((p (make-package name)))
      (should (packagep p))
      (should (equal (package-name p) "a"))))
  (should (packagep (make-package nil)))
  ;; Invalid package names
  (dolist (name '(1.0 (a)))
    (should-error (make-package name)))
  ;; Otherwise invalid forms.
  (should-error (make-package))
  (should-error (make-package 1.0))
  (should-error (make-package :hansi 1))
  (should-error (make-package "x" :hansi 1))
  (should-error (make-package "x" :nicknames))
  (should-error (make-package "x" :use))
  (should-error (make-package "x" :nicknames 1))
  (should-error (make-package "x" :use 1))
  ;; Registering package
  (let ((p (make-package "x" :nicknames '(y) :register t)))
    (unwind-protect
        (progn
          (should (packagep p))
          (should (eq (find-package "x") p))
          (should (eq (find-package "y") p)))
      (delete-package p))))

(ert-deftest pkg-tests-read ()
  (with-packages ((x :register t))
    (let* ((symbol-packages nil)
           (sym (read "x::y")))
      (should (symbolp sym))
      (should (equal (symbol-name sym) "x::y"))
      (should (eq (symbol-package sym) *emacs-package*))

      (setq sym (read ":b"))
      (should (keywordp sym))
      (should (equal (cl-symbol-name sym) "b"))
      (should (equal (symbol-name sym) ":b"))
      (should (eq (symbol-package sym) *keyword-package*))))

  (with-packages ((x :register t))
    (let* ((symbol-packages t)
           (sym (read "x::y")))
      (should (symbolp sym))
      (should (equal (symbol-name sym) "y"))
      (should (eq (symbol-package sym) x))

      (setq sym (read ":a"))
      (should (keywordp sym))
      (should (equal (cl-symbol-name sym) "a"))
      (should (equal (symbol-name sym) ":a"))
      (should (eq (symbol-package sym) *keyword-package*)))))

(ert-deftest pkg-tests-make-package-nicknames ()
  ;; Valid nicknames
  (dolist (nickname '("a" b ?c))
    (should (packagep (make-package "x" :nicknames (list nickname)))))
  ;; Invalid nicknames
  (dolist (nickname '(1.0))
    (should-error (packagep (make-package "x" :nicknames (list nickname)))))
  (with-packages ((x :nicknames '(x z)))
    ;; Package name allowed in nicknames.
    (should (equal (package-nicknames x) '("x" "z"))))
  (with-packages ((x :nicknames '(y y z)))
    ;; Duplicates removed, order-preserving.
    (should (equal (package-nicknames x) '("y" "z")))))

(ert-deftest pkg-tests-package-name ()
  (should (equal (package-name (make-package "x")) "x"))
  (should (equal (package-name (make-package :x)) "x"))
  (should (equal (package-name "emacs") "emacs"))
  (let ((p (make-package "x")))
    (delete-package p)
    (should (null (package-name p))))
  (should-error (package-name 1)))

(ert-deftest pkg-tests-package-nicknames ()
  (let ((nicknames '(("a" "b") (?a :b))))
    (dolist (n nicknames)
      (let ((p (make-package "x" :nicknames n)))
        (should (equal (package-nicknames p) '("a" "b")))))))

(ert-deftest pkg-tests-list-all-packages ()
  (let ((all (list-all-packages)))
    (should (cl-every #'packagep all))
    (should (memq (find-package "emacs") all))
    (should (memq (find-package "keyword") all))
    (should (memq (find-package "") all))))

(ert-deftest pkg-tests-package-find-package ()
  (with-packages (x)
    ;; If called with a package, returns that package.
    (should (eq (find-package x) x))
    (package-%register x)
    (should-error (find-package 1.0))
    (should (eq (find-package 'x) x))
    (should (eq (find-package "x") x))
    (should (eq (find-package ?x) x))
    (should (not (find-package "X"))))
  (with-packages ((x :nicknames '("y" "z")))
    (package-%register x)
    (should (eq (find-package 'y) (find-package 'x)))
    (should (eq (find-package 'z) (find-package 'x)))))

(ert-deftest pkg-tests-delete-package ()
  (with-packages (x)
    (package-%register x)
    (should (find-package "x"))
    (should (delete-package x))
    (should (null (delete-package x)))
    (should (null (package-name x)))
    (should (not (find-package 'x))))
  ;; Symbols whose home package is a package that is deleted, become
  ;; uninterned.
  (with-packages (x)
    (let ((sym (intern "a" x)))
      (delete-package x)
      (should (null (symbol-package sym))))))

(ert-deftest pkg-tests-rename-package ()
  (with-packages (x y)
    (package-%register x)
    (should (find-package 'x))
    (should (eq x (rename-package x 'a '(b))))
    (should (not (find-package 'x)))
    (should (eq (find-package 'a) x))
    (should (eq (find-package 'b) x))
    ;; Can't rename to an existing name or nickname.
    (should-error (rename-package y 'a))
    (should-error (rename-package y 'c :nicknames '("b")))
    ;; Can't rename deleted package.
    (should (delete-package x))
    (should-error (rename-package x 'd))))

(ert-deftest pkg-tests-use-package ()
  (with-packages (x y)
    (let ((sym-a (intern "a" x)))
      (should (eq (symbol-package sym-a) x))
      (use-package x y)
      (cl-multiple-value-bind (sym _status)
          (find-symbol "a" y)
        (should (null sym))
        (when nil
          (export sym-a x)
          (cl-multiple-value-bind (sym status)
              (find-symbol "a" y)
            (should (eq sym sym-a))
            (should (eq status :inherited))))))))

;; (ert-deftest pkg-tests-find-symbol ()
;;   (should nil))

;; (ert-deftest pkg-tests-cl-intern ()
;;   (cl-assert (not (find-symbol "foo")))
;;   (unwind-protect
;;       (progn
;;         (cl-intern "foo")
;;         (should (find-symbol "foo")))
;;     (cl-unintern 'foo)))

;; (ert-deftest pkg-tests-cl-unintern ()
;;   (cl-assert (not (find-symbol "foo")))
;;   (unwind-protect
;;       (progn
;;         (cl-intern "foo")
;;         (cl-unintern 'foo)
;;         (should-not (find-symbol "foo")))
;;     (cl-unintern 'foo)))

;; (ert-deftest pkg-tests-package-name ()
;;   (should (equal (package-name "emacs") "emacs")))

;; (ert-deftest pkg-tests-export ()
;;   (should nil))

;; (ert-deftest pkg-tests-unexport ()
;;   (should nil))

;; (ert-deftest pkg-tests-import ()
;;   (should nil))

;; (ert-deftest pkg-tests-shadow ()
;;   (should nil))

;; (ert-deftest pkg-tests-shadowing-import ()
;;   (should nil))

;; (ert-deftest pkg-tests-shadowing-use-package ()
;;   (should nil))

;; (ert-deftest pkg-tests-shadowing-unuse-package ()
;;   (should nil))
