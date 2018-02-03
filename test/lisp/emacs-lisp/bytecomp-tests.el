;;; bytecomp-tests.el

;; Copyright (C) 2008-2018 Free Software Foundation, Inc.

;; Author: Shigeru Fukaya <shigeru.fukaya@gmail.com>
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Created:        November 2008
;; Keywords:       internal
;; Human-Keywords: internal

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

(require 'ert)
(require 'cl-lib)

;;; Code:
(defconst byte-opt-testsuite-arith-data
  '(
    ;; some functional tests
    (let ((a most-positive-fixnum) (b 1) (c 1.0))  (+ a b c))
    (let ((a most-positive-fixnum) (b -2) (c 1.0)) (- a b c))
    (let ((a most-positive-fixnum) (b 2) (c 1.0))  (* a b c))
    (let ((a 3) (b 2) (c 1.0))                     (/ a b c))
    (let ((a (+ 1 (expt 2 -64))) (b (expt 2 -65))) (+ a -1 b))
    (let ((a (+ 1 (expt 2 -64))) (b (expt 2 -65))) (- a 1 (- b)))
    ;; This fails.  Should it be a bug?
    ;; (let ((a (expt 2 -1074)) (b 0.125))		   (* a 8 b))
    (let ((a 1.0))				   (* a 0))
    (let ((a 1.0))				   (* a 2.0 0))
    (let ((a 1.0))				   (/ 0 a))
    (let ((a 1.0))				   (/ 3 a 2))
    (let ((a most-positive-fixnum) (b 2.0))	   (* a 2 b))
    (let ((a 3) (b 2))				   (/ a b 1.0))
    (/ 3 -1)
    (+ 4 3 2 1)
    (+ 4 3 2.0 1)
    (- 4 3 2 1)				; not new, for reference
    (- 4 3 2.0 1)			; not new, for reference
    (* 4 3 2 1)
    (* 4 3 2.0 1)
    (/ 4 3 2 1)
    (/ 4 3 2.0 1)
    (let ((a 3) (b 2))				   (+ a b 1))
    (let ((a 3) (b 2))				   (+ a b -1))
    (let ((a 3) (b 2))				   (- a b 1))
    (let ((a 3) (b 2))				   (- a b -1))
    (let ((a 3) (b 2))				   (+ a b a 1))
    (let ((a 3) (b 2))				   (+ a b a -1))
    (let ((a 3) (b 2))				   (- a b a 1))
    (let ((a 3) (b 2))				   (- a b a -1))
    (let ((a 3) (b 2))				   (* a b -1))
    (let ((a 3) (b 2))				   (* a -1))
    (let ((a 3) (b 2))				   (/ a b 1))
    (let ((a 3) (b 2))				   (/ (+ a b) 1))

    ;; coverage test
    (let ((a 3) (b 2) (c 1.0)) (+))
    (let ((a 3) (b 2) (c 1.0)) (+ 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 2 0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (+ a))
    (let ((a 3) (b 2) (c 1.0)) (+ a 0))
    (let ((a 3) (b 2) (c 1.0)) (+ a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (+ c 0))
    (let ((a 3) (b 2) (c 1.0)) (+ c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 c))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (+ 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (+ 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (+ a 1))
    (let ((a 3) (b 2) (c 1.0)) (+ a -1))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 a))
    (let ((a 3) (b 2) (c 1.0)) (+ -1 a))
    (let ((a 3) (b 2) (c 1.0)) (+ c 1))
    (let ((a 3) (b 2) (c 1.0)) (+ c -1))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 c))
    (let ((a 3) (b 2) (c 1.0)) (+ -1 c))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 0))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 1))
    (let ((a 3) (b 2) (c 1.0)) (+ a b -1))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (+ a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (+ a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (+ a b c -1))

    (let ((a 3) (b 2) (c 1.0)) (-))
    (let ((a 3) (b 2) (c 1.0)) (- 2))
    (let ((a 3) (b 2) (c 1.0)) (- 2 0))
    (let ((a 3) (b 2) (c 1.0)) (- 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 2.0))
    (let ((a 3) (b 2) (c 1.0)) (- 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (- 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 2))
    (let ((a 3) (b 2) (c 1.0)) (- 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (- a))
    (let ((a 3) (b 2) (c 1.0)) (- a 0))
    (let ((a 3) (b 2) (c 1.0)) (- a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (- c 0))
    (let ((a 3) (b 2) (c 1.0)) (- c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 c))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (- a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (- 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (- 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (- 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (- a 1))
    (let ((a 3) (b 2) (c 1.0)) (- a -1))
    (let ((a 3) (b 2) (c 1.0)) (- 1 a))
    (let ((a 3) (b 2) (c 1.0)) (- -1 a))
    (let ((a 3) (b 2) (c 1.0)) (- c 1))
    (let ((a 3) (b 2) (c 1.0)) (- c -1))
    (let ((a 3) (b 2) (c 1.0)) (- 1 c))
    (let ((a 3) (b 2) (c 1.0)) (- -1 c))
    (let ((a 3) (b 2) (c 1.0)) (- a b 0))
    (let ((a 3) (b 2) (c 1.0)) (- a b 1))
    (let ((a 3) (b 2) (c 1.0)) (- a b -1))
    (let ((a 3) (b 2) (c 1.0)) (- a b 2))
    (let ((a 3) (b 2) (c 1.0)) (- 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (- a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (- a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (- a b c -1))

    (let ((a 3) (b 2) (c 1.0)) (*))
    (let ((a 3) (b 2) (c 1.0)) (* 2))
    (let ((a 3) (b 2) (c 1.0)) (* 2 0))
    (let ((a 3) (b 2) (c 1.0)) (* 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 2.0))
    (let ((a 3) (b 2) (c 1.0)) (* 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (* 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 2))
    (let ((a 3) (b 2) (c 1.0)) (* 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (* a))
    (let ((a 3) (b 2) (c 1.0)) (* a 0))
    (let ((a 3) (b 2) (c 1.0)) (* a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (* c 0))
    (let ((a 3) (b 2) (c 1.0)) (* c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 c))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (* a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (* 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (* 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (* 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (* a 1))
    (let ((a 3) (b 2) (c 1.0)) (* a -1))
    (let ((a 3) (b 2) (c 1.0)) (* 1 a))
    (let ((a 3) (b 2) (c 1.0)) (* -1 a))
    (let ((a 3) (b 2) (c 1.0)) (* c 1))
    (let ((a 3) (b 2) (c 1.0)) (* c -1))
    (let ((a 3) (b 2) (c 1.0)) (* 1 c))
    (let ((a 3) (b 2) (c 1.0)) (* -1 c))
    (let ((a 3) (b 2) (c 1.0)) (* a b 0))
    (let ((a 3) (b 2) (c 1.0)) (* a b 1))
    (let ((a 3) (b 2) (c 1.0)) (* a b -1))
    (let ((a 3) (b 2) (c 1.0)) (* a b 2))
    (let ((a 3) (b 2) (c 1.0)) (* 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (* a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (* a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (* a b c -1))

    (let ((a 3) (b 2) (c 1.0)) (/))
    (let ((a 3) (b 2) (c 1.0)) (/ 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 2 0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (/ a))
    (let ((a 3) (b 2) (c 1.0)) (/ a 0))
    (let ((a 3) (b 2) (c 1.0)) (/ a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (/ c 0))
    (let ((a 3) (b 2) (c 1.0)) (/ c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 c))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (/ 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (/ 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (/ a 1))
    (let ((a 3) (b 2) (c 1.0)) (/ a -1))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 a))
    (let ((a 3) (b 2) (c 1.0)) (/ -1 a))
    (let ((a 3) (b 2) (c 1.0)) (/ c 1))
    (let ((a 3) (b 2) (c 1.0)) (/ c -1))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 c))
    (let ((a 3) (b 2) (c 1.0)) (/ -1 c))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 0))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 1))
    (let ((a 3) (b 2) (c 1.0)) (/ a b -1))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (/ a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (/ a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (/ a b c -1))
    ;; Test switch bytecode
    (let ((a 3)) (cond ((eq a 1) 'one) ((eq a 2) 'two) ((eq a 3) 'three) (t t)))
    (let ((a 'three)) (cond ((eq a 'one) 1) ((eq a 2) 'two) ((eq a 'three) 3)
                            (t t)))
    (let ((a 2)) (cond ((eq a 'one) 1) ((eq a 1) 'one) ((eq a 2) 'two)
                       (t nil)))
    (let ((a 2.0)) (cond ((eql a 2) 'incorrect) ((eql a 2.00) 'correct)))
    (let ((a "foobar")) (cond ((equal "notfoobar" a) 'incorrect)
                              ((equal 1 a) 'incorrect)
                              ((equal a "foobar") 'correct)
                              (t 'incorrect)))
    (let ((a "foobar") (l t)) (pcase a
                                ("bar" 'incorrect)
                                ("foobar" (while l
                                            a (setq l nil))
                                 'correct)))
    (let ((a 'foobar) (l t)) (cl-case a
                         ('foo 'incorrect)
                         ('bar 'incorrect)
                         ('foobar (while l
                                    a (setq l nil))
                                  'correct)))
    (let ((a 'foobar) (l t)) (cond
                        ((eq a 'bar) 'incorrect)
                        ((eq a 'foo) 'incorrect)
                        ((eq a 'bar) 'incorrect)
                        (t (while l
                             a (setq l nil))
                           'correct)))
    (let ((a 'foobar) (l t)) (cond
                        ((eq a 'bar) 'incorrect)
                        ((eq a 'foo) 'incorrect)
                        ((eq a 'foobar)
                         (while l
                           a (setq l nil))
                         'correct)
                        (t 'incorrect)))
    (let ((a))
      (cond ((eq a 'foo) 'incorrect)
            (t)))
    (let ((a))
      (cond ((eq a 'foo) 'incorrect)
            ('correct))))
  "List of expression for test.
Each element will be executed by interpreter and with
bytecompiled code, and their results compared.")

(defun bytecomp-check-1 (pat)
  "Return non-nil if PAT is the same whether directly evalled or compiled."
  (let ((warning-minimum-log-level :emergency)
	(byte-compile-warnings nil)
	(v0 (condition-case nil
		(eval pat)
	      (error nil)))
	(v1 (condition-case nil
		(funcall (byte-compile (list 'lambda nil pat)))
	      (error nil))))
    (equal v0 v1)))

(put 'bytecomp-check-1 'ert-explainer 'bytecomp-explain-1)

(defun bytecomp-explain-1 (pat)
  (let ((v0 (condition-case nil
		(eval pat)
	      (error nil)))
	(v1 (condition-case nil
		(funcall (byte-compile (list 'lambda nil pat)))
	      (error nil))))
    (format "Expression `%s' gives `%s' if directly evalled, `%s' if compiled."
	    pat v0 v1)))

(ert-deftest bytecomp-tests ()
  "Test the Emacs byte compiler."
  (dolist (pat byte-opt-testsuite-arith-data)
    (should (bytecomp-check-1 pat))))

(defun test-byte-opt-arithmetic (&optional arg)
  "Unit test for byte-opt arithmetic operations.
Subtests signal errors if something goes wrong."
  (interactive "P")
  (switch-to-buffer (generate-new-buffer "*Font Pase Test*"))
  (let ((warning-minimum-log-level :emergency)
	(byte-compile-warnings nil)
	(pass-face '((t :foreground "green")))
	(fail-face '((t :foreground "red")))
	(print-escape-nonascii t)
	(print-escape-newlines t)
	(print-quoted t)
	v0 v1)
    (dolist (pat byte-opt-testsuite-arith-data)
      (condition-case nil
	  (setq v0 (eval pat))
	(error (setq v0 nil)))
      (condition-case nil
	  (setq v1 (funcall (byte-compile (list 'lambda nil pat))))
	(error (setq v1 nil)))
      (insert (format "%s" pat))
      (indent-to-column 65)
      (if (equal v0 v1)
	  (insert (propertize "OK" 'face pass-face))
	(insert (propertize "FAIL\n" 'face fail-face))
	(indent-to-column 55)
	(insert (propertize (format "[%s] vs [%s]" v0 v1)
			    'face fail-face)))
      (insert "\n"))))

(defun test-byte-comp-compile-and-load (compile &rest forms)
  (let ((elfile nil)
        (elcfile nil))
    (unwind-protect
         (progn
           (setf elfile (make-temp-file "test-bytecomp" nil ".el"))
           (when compile
             (setf elcfile (make-temp-file "test-bytecomp" nil ".elc")))
           (with-temp-buffer
             (dolist (form forms)
               (print form (current-buffer)))
             (write-region (point-min) (point-max) elfile nil 'silent))
           (if compile
               (let ((byte-compile-dest-file-function
                      (lambda (e) elcfile)))
                 (byte-compile-file elfile t))
             (load elfile nil 'nomessage)))
      (when elfile (delete-file elfile))
      (when elcfile (delete-file elcfile)))))
(put 'test-byte-comp-compile-and-load 'lisp-indent-function 1)

(ert-deftest test-byte-comp-macro-expansion ()
  (test-byte-comp-compile-and-load t
    '(progn (defmacro abc (arg) 1) (defun def () (abc 2))))
  (should (equal (funcall 'def) 1)))

(ert-deftest test-byte-comp-macro-expansion-eval-and-compile ()
  (test-byte-comp-compile-and-load t
    '(eval-and-compile (defmacro abc (arg) -1) (defun def () (abc 2))))
  (should (equal (funcall 'def) -1)))

(ert-deftest test-byte-comp-macro-expansion-eval-when-compile ()
  ;; Make sure we interpret eval-when-compile forms properly.  CLISP
  ;; and SBCL interpreter eval-when-compile (well, the CL equivalent)
  ;; in the same way.
  (test-byte-comp-compile-and-load t
    '(eval-when-compile
      (defmacro abc (arg) -10)
      (defun abc-1 () (abc 2)))
    '(defmacro abc-2 () (abc-1))
    '(defun def () (abc-2)))
  (should (equal (funcall 'def) -10)))

(ert-deftest test-byte-comp-macro-expand-lexical-override ()
  ;; Intuitively, one might expect the defmacro to override the
  ;; macrolet since macrolet's is explicitly called out as being
  ;; equivalent to toplevel, but CLISP and SBCL both evaluate the form
  ;; this way, so we should too.
  (test-byte-comp-compile-and-load t
    '(require 'cl-lib)
    '(cl-macrolet ((m () 4))
      (defmacro m () 5)
      (defun def () (m))))
  (should (equal (funcall 'def) 4)))

(ert-deftest bytecomp-tests--warnings ()
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (let ((inhibit-read-only t)) (erase-buffer)))
  (test-byte-comp-compile-and-load t
    '(progn
       (defun my-test0 ()
         (my--test11 3)
         (my--test12 3)
         (my--test2 5))
       (defmacro my--test11 (arg) (+ arg 1))
       (eval-and-compile
         (defmacro my--test12 (arg) (+ arg 1))
         (defun my--test2 (arg) (+ arg 1)))))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (goto-char (point-min))
    ;; Should warn that mt--test1[12] are first used as functions.
    ;; The second alternative is for when the file name is so long
    ;; that pretty-printing starts the message on the next line.
    (should (or (re-search-forward "my--test11:\n.*macro" nil t)
                (re-search-forward "my--test11:\n.*:\n.*macro" nil t)))
    (should (or (re-search-forward "my--test12:\n.*macro" nil t)
                (re-search-forward "my--test12:\n.*:\n.*macro" nil t)))
    (goto-char (point-min))
    ;; Should not warn that mt--test2 is not known to be defined.
    (should-not (re-search-forward "my--test2" nil t))))

(ert-deftest test-eager-load-macro-expansion ()
  (test-byte-comp-compile-and-load nil
    '(progn (defmacro abc (arg) 1) (defun def () (abc 2))))
  (should (equal (funcall 'def) 1)))

(ert-deftest test-eager-load-macro-expansion-eval-and-compile ()
  (test-byte-comp-compile-and-load nil
    '(eval-and-compile (defmacro abc (arg) -1) (defun def () (abc 2))))
  (should (equal (funcall 'def) -1)))

(ert-deftest test-eager-load-macro-expansion-eval-when-compile ()
  ;; Make sure we interpret eval-when-compile forms properly.  CLISP
  ;; and SBCL interpreter eval-when-compile (well, the CL equivalent)
  ;; in the same way.
  (test-byte-comp-compile-and-load nil
    '(eval-when-compile
      (defmacro abc (arg) -10)
      (defun abc-1 () (abc 2)))
    '(defmacro abc-2 () (abc-1))
    '(defun def () (abc-2)))
  (should (equal (funcall 'def) -10)))

(ert-deftest test-eager-load-macro-expand-lexical-override ()
  ;; Intuitively, one might expect the defmacro to override the
  ;; macrolet since macrolet's is explicitly called out as being
  ;; equivalent to toplevel, but CLISP and SBCL both evaluate the form
  ;; this way, so we should too.
  (test-byte-comp-compile-and-load nil
    '(require 'cl-lib)
    '(cl-macrolet ((m () 4))
      (defmacro m () 5)
      (defun def () (m))))
  (should (equal (funcall 'def) 4)))

(defconst bytecomp-lexbind-tests
  `(
    (let ((f #'car))
      (let ((f (lambda (x) (cons (funcall f x) (cdr x)))))
        (funcall f '(1 . 2))))
    )
  "List of expression for test.
Each element will be executed by interpreter and with
bytecompiled code, and their results compared.")

(defun bytecomp-lexbind-check-1 (pat)
  "Return non-nil if PAT is the same whether directly evalled or compiled."
  (let ((warning-minimum-log-level :emergency)
	(byte-compile-warnings nil)
	(v0 (condition-case nil
		(eval pat t)
	      (error nil)))
	(v1 (condition-case nil
		(funcall (let ((lexical-binding t))
                           (byte-compile `(lambda nil ,pat))))
	      (error nil))))
    (equal v0 v1)))

(put 'bytecomp-lexbind-check-1 'ert-explainer 'bytecomp-lexbind-explain-1)

(defun bytecomp-lexbind-explain-1 (pat)
  (let ((v0 (condition-case nil
		(eval pat t)
	      (error nil)))
	(v1 (condition-case nil
		(funcall (let ((lexical-binding t))
                           (byte-compile (list 'lambda nil pat))))
	      (error nil))))
    (format "Expression `%s' gives `%s' if directly evalled, `%s' if compiled."
	    pat v0 v1)))

(ert-deftest bytecomp-lexbind-tests ()
  "Test the Emacs byte compiler lexbind handling."
  (dolist (pat bytecomp-lexbind-tests)
    (should (bytecomp-lexbind-check-1 pat))))

(defmacro bytecomp-tests--with-temp-file (file-name-var &rest body)
  (declare (indent 1))
  (cl-check-type file-name-var symbol)
  `(let ((,file-name-var (make-temp-file "emacs")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,file-name-var)
       (let ((elc (concat ,file-name-var ".elc")))
         (if (file-exists-p elc) (delete-file elc))))))

(ert-deftest bytecomp-tests--unescaped-char-literals ()
  "Check that byte compiling warns about unescaped character
literals (Bug#20852)."
  (should (boundp 'lread--unescaped-character-literals))
  (bytecomp-tests--with-temp-file source
    (write-region "(list ?) ?( ?; ?\" ?[ ?])" nil source)
    (bytecomp-tests--with-temp-file destination
      (let* ((byte-compile-dest-file-function (lambda (_) destination))
            (byte-compile-error-on-warn t)
            (byte-compile-debug t)
            (err (should-error (byte-compile-file source))))
        (should (equal (cdr err)
                       (list (concat "unescaped character literals "
                                     "`?\"', `?(', `?)', `?;', `?[', `?]' "
                                     "detected!"))))))))

(ert-deftest bytecomp-tests--old-style-backquotes ()
  "Check that byte compiling warns about old-style backquotes."
  (bytecomp-tests--with-temp-file source
    (write-region "(` (a b))" nil source)
    (bytecomp-tests--with-temp-file destination
      (let* ((byte-compile-dest-file-function (lambda (_) destination))
             (byte-compile-debug t)
             (err (should-error (byte-compile-file source))))
        (should (equal (cdr err) '("Old-style backquotes detected!")))))))


(ert-deftest bytecomp-tests-function-put ()
  "Check `function-put' operates during compilation."
  (bytecomp-tests--with-temp-file source
    (dolist (form '((function-put 'bytecomp-tests--foo 'foo 1)
                    (function-put 'bytecomp-tests--foo 'bar 2)
                    (defmacro bytecomp-tests--foobar ()
                      `(cons ,(function-get 'bytecomp-tests--foo 'foo)
                             ,(function-get 'bytecomp-tests--foo 'bar)))
                    (defvar bytecomp-tests--foobar 1)
                    (setq bytecomp-tests--foobar (bytecomp-tests--foobar))))
      (print form (current-buffer)))
    (write-region (point-min) (point-max) source nil 'silent)
    (byte-compile-file source t)
    (should (equal bytecomp-tests--foobar (cons 1 2)))))

(ert-deftest bytecomp-tests--test-no-warnings-with-advice ()
  (defun f ())
  (define-advice f (:around (oldfun &rest args) test)
    (apply oldfun args))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (let ((inhibit-read-only t)) (erase-buffer)))
  (test-byte-comp-compile-and-load t '(defun f ()))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (goto-char (point-min))
    (should-not (search-forward "Warning" nil t))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'bytecomp-tests)
;; bytecomp-tests.el ends here.
