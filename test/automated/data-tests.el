;;; data-tests.el --- tests for src/data.c

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'cl))

(ert-deftest data-tests-= ()
  (should-error (=))
  (should (= 1))
  (should (= 2 2))
  (should (= 9 9 9 9 9 9 9 9 9))
  (should-not (apply #'= '(3 8 3)))
  (should-error (= 9 9 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (= 9 8 'foo)))

(ert-deftest data-tests-< ()
  (should-error (<))
  (should (< 1))
  (should (< 2 3))
  (should (< -6 -1 0 2 3 4 8 9 999))
  (should-not (apply #'< '(3 8 3)))
  (should-error (< 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (< 9 8 'foo)))

(ert-deftest data-tests-> ()
  (should-error (>))
  (should (> 1))
  (should (> 3 2))
  (should (> 6 1 0 -2 -3 -4 -8 -9 -999))
  (should-not (apply #'> '(3 8 3)))
  (should-error (> 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (> 8 9 'foo)))

(ert-deftest data-tests-<= ()
  (should-error (<=))
  (should (<= 1))
  (should (<= 2 3))
  (should (<= -6 -1 -1 0 0 0 2 3 4 8 999))
  (should-not (apply #'<= '(3 8 3 3)))
  (should-error (<= 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (<= 9 8 'foo)))

(ert-deftest data-tests->= ()
  (should-error (>=))
  (should (>= 1))
  (should (>= 3 2))
  (should (>= 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999))
  (should-not (apply #'>= '(3 8 3)))
  (should-error (>= 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (>= 8 9 'foo)))

;; Bool vector tests.  Compactly represent bool vectors as hex
;; strings.

(ert-deftest bool-vector-count-population-all-0-nil ()
  (cl-loop for sz in '(0 45 1 64 9 344)
           do (let* ((bv (make-bool-vector sz nil)))
                (should
                 (zerop
                  (bool-vector-count-population bv))))))

(ert-deftest bool-vector-count-population-all-1-t ()
  (cl-loop for sz in '(0 45 1 64 9 344)
           do (let* ((bv (make-bool-vector sz t)))
                (should
                 (eql
                  (bool-vector-count-population bv)
                  sz)))))

(ert-deftest bool-vector-count-population-1-nil ()
  (let* ((bv (make-bool-vector 45 nil)))
    (aset bv 40 t)
    (aset bv 0 t)
    (should
     (eql
      (bool-vector-count-population bv)
      2))))

(ert-deftest bool-vector-count-population-1-t ()
  (let* ((bv (make-bool-vector 45 t)))
    (aset bv 40 nil)
    (aset bv 0 nil)
    (should
     (eql
      (bool-vector-count-population bv)
      43))))

(defun mock-bool-vector-count-consecutive (a b i)
  (loop for i from i below (length a)
        while (eq (aref a i) b)
        sum 1))

(defun test-bool-vector-bv-from-hex-string (desc)
  (let (bv nchars nibbles)
    (dolist (c (string-to-list desc))
      (push (string-to-number
             (char-to-string c)
             16)
            nibbles))
    (setf bv (make-bool-vector (* 4 (length nibbles)) nil))
    (let ((i 0))
      (dolist (n (nreverse nibbles))
        (dotimes (_ 4)
          (aset bv i (> (logand 1 n) 0))
          (incf i)
          (setf n (lsh n -1)))))
    bv))

(defun test-bool-vector-to-hex-string (bv)
  (let (nibbles (v (cl-coerce bv 'list)))
    (while v
      (push (logior
             (lsh (if (nth 0 v) 1 0) 0)
             (lsh (if (nth 1 v) 1 0) 1)
             (lsh (if (nth 2 v) 1 0) 2)
             (lsh (if (nth 3 v) 1 0) 3))
            nibbles)
      (setf v (nthcdr 4 v)))
    (mapconcat (lambda (n) (format "%X" n))
               (nreverse nibbles)
               "")))

(defun test-bool-vector-count-consecutive-tc (desc)
  "Run a test case for bool-vector-count-consecutive.
DESC is a string describing the test.  It is a sequence of
hexadecimal digits describing the bool vector.  We exhaustively
test all counts at all possible positions in the vector by
comparing the subr with a much slower lisp implementation."
  (let ((bv (test-bool-vector-bv-from-hex-string desc)))
    (loop
     for lf in '(nil t)
     do (loop
         for pos from 0 upto (length bv)
         for cnt = (mock-bool-vector-count-consecutive bv lf pos)
         for rcnt = (bool-vector-count-consecutive bv lf pos)
         unless (eql cnt rcnt)
         do (error "FAILED testcase %S %3S %3S %3S"
                   pos lf cnt rcnt)))))

(defconst bool-vector-test-vectors
'(""
  "0"
  "F"
  "0F"
  "F0"
  "00000000000000000000000000000FFFFF0000000"
  "44a50234053fba3340000023444a50234053fba33400000234"
  "12341234123456123412346001234123412345612341234600"
  "44a50234053fba33400000234"
  "1234123412345612341234600"
  "44a50234053fba33400000234"
  "1234123412345612341234600"
  "44a502340"
  "123412341"
  "0000000000000000000000000"
  "FFFFFFFFFFFFFFFF1"))

(ert-deftest bool-vector-count-consecutive ()
  (mapc #'test-bool-vector-count-consecutive-tc
        bool-vector-test-vectors))

(defun test-bool-vector-apply-mock-op (mock a b c)
  "Compute (slowly) the correct result of a bool-vector set operation."
  (let (changed nv)
    (assert (eql (length b) (length c)))
    (if a (setf nv a)
      (setf a (make-bool-vector (length b) nil))
      (setf changed t))

    (loop for i below (length b)
          for mockr = (funcall mock
                               (if (aref b i) 1 0)
                               (if (aref c i) 1 0))
          for r = (not (= 0 mockr))
          do (progn
               (unless (eq (aref a i) r)
                 (setf changed t))
               (setf (aref a i) r)))
    (if changed a)))

(defun test-bool-vector-binop (mock real)
  "Test a binary set operation."
  (loop for s1 in bool-vector-test-vectors
        for bv1 = (test-bool-vector-bv-from-hex-string s1)
        for vecs2 = (cl-remove-if-not
                     (lambda (x) (eql (length x) (length s1)))
                     bool-vector-test-vectors)
        do (loop for s2 in vecs2
                 for bv2 = (test-bool-vector-bv-from-hex-string s2)
                 for mock-result = (test-bool-vector-apply-mock-op
                                    mock nil bv1 bv2)
                 for real-result = (funcall real bv1 bv2)
                 do (progn
                      (should (equal mock-result real-result))))))

(ert-deftest bool-vector-intersection-op ()
  (test-bool-vector-binop
   #'logand
   #'bool-vector-intersection))

(ert-deftest bool-vector-union-op ()
  (test-bool-vector-binop
   #'logior
   #'bool-vector-union))

(ert-deftest bool-vector-xor-op ()
  (test-bool-vector-binop
   #'logxor
   #'bool-vector-exclusive-or))

(ert-deftest bool-vector-set-difference-op ()
  (test-bool-vector-binop
   (lambda (a b) (logand a (lognot b)))
   #'bool-vector-set-difference))

(ert-deftest bool-vector-change-detection ()
  (let* ((vc1 (test-bool-vector-bv-from-hex-string "abcdef"))
         (vc2 (test-bool-vector-bv-from-hex-string "012345"))
         (vc3 (make-bool-vector (length vc1) nil))
         (c1 (bool-vector-union vc1 vc2 vc3))
         (c2 (bool-vector-union vc1 vc2 vc3)))
    (should (equal c1 (test-bool-vector-apply-mock-op
                       #'logior
                       nil
                       vc1 vc2)))
    (should (not c2))))

(ert-deftest bool-vector-not ()
  (let* ((v1 (test-bool-vector-bv-from-hex-string "FFFF3"))
         (v2 (test-bool-vector-bv-from-hex-string "0000C"))
         (v3 (bool-vector-not v1)))
    (should (equal v2 v3))))
