;;; data-tests.el --- tests for src/data.c

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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

(require 'cl-lib)

(ert-deftest data-tests-= ()
  (should-error (=))
  (should (= 1))
  (should (= 2 2))
  (should (= 9 9 9 9 9 9 9 9 9))
  (should (= most-negative-fixnum (float most-negative-fixnum)))
  (should-not (= most-positive-fixnum (+ 1.0 most-positive-fixnum)))
  (should-not (apply #'= '(3 8 3)))
  (should-error (= 9 9 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (= 9 8 'foo)))

(ert-deftest data-tests-< ()
  (should-error (<))
  (should (< 1))
  (should (< 2 3))
  (should (< -6 -1 0 2 3 4 8 9 999))
  (should (< 0.5 most-positive-fixnum (+ 1.0 most-positive-fixnum)))
  (should-not (apply #'< '(3 8 3)))
  (should-error (< 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (< 9 8 'foo)))

(ert-deftest data-tests-> ()
  (should-error (>))
  (should (> 1))
  (should (> 3 2))
  (should (> 6 1 0 -2 -3 -4 -8 -9 -999))
  (should (> (+ 1.0 most-positive-fixnum) most-positive-fixnum 0.5))
  (should-not (apply #'> '(3 8 3)))
  (should-error (> 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (> 8 9 'foo)))

(ert-deftest data-tests-<= ()
  (should-error (<=))
  (should (<= 1))
  (should (<= 2 3))
  (should (<= -6 -1 -1 0 0 0 2 3 4 8 999))
  (should (<= 0.5 most-positive-fixnum (+ 1.0 most-positive-fixnum)))
  (should-not (apply #'<= '(3 8 3 3)))
  (should-error (<= 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (<= 9 8 'foo)))

(ert-deftest data-tests->= ()
  (should-error (>=))
  (should (>= 1))
  (should (>= 3 2))
  (should (>= 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999))
  (should (>= (+ 1.0 most-positive-fixnum) most-positive-fixnum))
  (should-not (apply #'>= '(3 8 3)))
  (should-error (>= 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (>= 8 9 'foo)))

(ert-deftest data-tests-max ()
  (should-error (max))
  (should (= 1 (max 1)))
  (should (= 3 (max 3 2)))
  (should (= 666 (max 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999)))
  (should (= (1+ most-negative-fixnum)
             (max (float most-negative-fixnum) (1+ most-negative-fixnum))))
  (should (= 8 (apply #'max '(3 8 3))))
  (should-error (max 9 8 'foo))
  (should-error (max (make-marker)))
  (should (eql 1 (max (point-min-marker) 1))))

(ert-deftest data-tests-min ()
  (should-error (min))
  (should (= 1 (min 1)))
  (should (= 2 (min 3 2)))
  (should (= -999 (min 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999)))
  (should (= most-positive-fixnum
             (min (+ 1.0 most-positive-fixnum) most-positive-fixnum)))
  (should (= 3 (apply #'min '(3 8 3))))
  (should-error (min 9 8 'foo))
  (should-error (min (make-marker)))
  (should (eql 1 (min (point-min-marker) 1)))
  (should (isnan (min 0.0e+NaN)))
  (should (isnan (min 0.0e+NaN 1 2)))
  (should (isnan (min 1.0 0.0e+NaN)))
  (should (isnan (min 1.0 0.0e+NaN 1.1))))

(defun data-tests-popcnt (byte)
  "Calculate the Hamming weight of BYTE."
  (if (< byte 0)
      (setq byte (lognot byte)))
  (setq byte (- byte (logand (lsh byte -1) #x55555555)))
  (setq byte (+ (logand byte #x33333333) (logand (lsh byte -2) #x33333333)))
  (lsh (* (logand (+ byte (lsh byte -4)) #x0f0f0f0f) #x01010101) -24))

(ert-deftest data-tests-logcount ()
  (should (cl-loop for n in (number-sequence -255 255)
                   always (= (logcount n) (data-tests-popcnt n))))
  ;; https://oeis.org/A000120
  (should (= 11 (logcount 9727)))
  (should (= 8 (logcount 9999))))

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
  (cl-loop for i from i below (length a)
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
          (cl-incf i)
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
    (cl-loop
     for lf in '(nil t)
     do (cl-loop
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
    (cl-assert (eql (length b) (length c)))
    (if a (setf nv a)
      (setf a (make-bool-vector (length b) nil))
      (setf changed t))

    (cl-loop for i below (length b)
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
  (cl-loop for s1 in bool-vector-test-vectors
           for bv1 = (test-bool-vector-bv-from-hex-string s1)
           for vecs2 = (cl-remove-if-not
                        (lambda (x) (eql (length x) (length s1)))
                        bool-vector-test-vectors)
           do (cl-loop for s2 in vecs2
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

;; Tests for variable bindings

(defvar binding-test-buffer-A (get-buffer-create "A"))
(defvar binding-test-buffer-B (get-buffer-create "B"))

(defvar binding-test-always-local 'always)
(make-variable-buffer-local 'binding-test-always-local)

(defvar binding-test-some-local 'some)
(with-current-buffer binding-test-buffer-A
  (set (make-local-variable 'binding-test-some-local) 'local))

(ert-deftest binding-test-manual ()
  "A test case from the elisp manual."
  (with-current-buffer binding-test-buffer-A
    (let ((binding-test-some-local 'something-else))
      (should (eq binding-test-some-local 'something-else))
      (set-buffer binding-test-buffer-B)
      (should (eq binding-test-some-local 'some)))
    (should (eq binding-test-some-local 'some))
    (set-buffer binding-test-buffer-A)
    (should (eq binding-test-some-local 'local))))

(ert-deftest binding-test-setq-default ()
  "Test that a setq-default has no effect when there is a local binding."
  (with-current-buffer binding-test-buffer-B
    ;; This variable is not local in this buffer.
    (let ((binding-test-some-local 'something-else))
      (setq-default binding-test-some-local 'new-default))
    (should (eq binding-test-some-local 'some))))

(ert-deftest binding-test-makunbound ()
  "Tests of makunbound, from the manual."
  (with-current-buffer binding-test-buffer-B
    (should (boundp 'binding-test-some-local))
    (let ((binding-test-some-local 'outer))
      (let ((binding-test-some-local 'inner))
	(makunbound 'binding-test-some-local)
	(should (not (boundp 'binding-test-some-local))))
      (should (and (boundp 'binding-test-some-local)
		   (eq binding-test-some-local 'outer))))))

(ert-deftest binding-test-defvar-bool ()
  "Test DEFVAR_BOOL"
  (let ((display-hourglass 5))
    (should (eq display-hourglass t))))

(ert-deftest binding-test-defvar-int ()
  "Test DEFVAR_INT"
  (should-error (setq gc-cons-threshold 5.0) :type 'wrong-type-argument))

(ert-deftest binding-test-set-constant-t ()
  "Test setting the constant t"
  (with-no-warnings (should-error (setq t 'bob) :type 'setting-constant)))

(ert-deftest binding-test-set-constant-nil ()
  "Test setting the constant nil"
  (with-no-warnings (should-error (setq nil 'bob) :type 'setting-constant)))

(ert-deftest binding-test-set-constant-keyword ()
  "Test setting a keyword constant"
  (with-no-warnings (should-error (setq :keyword 'bob) :type 'setting-constant)))

(ert-deftest binding-test-set-constant-nil ()
  "Test setting a keyword to itself"
  (with-no-warnings (should (setq :keyword :keyword))))

;; More tests to write -
;; kill-local-variable
;; defconst; can modify
;; defvar and defconst modify the local binding [ doesn't matter for us ]
;; various kinds of special internal forwarding objects
;;   a couple examples in manual, not enough
;; variable aliases

;; Tests for watchpoints

(ert-deftest data-tests-variable-watchers ()
  (defvar data-tests-var 0)
  (let* ((watch-data nil)
         (collect-watch-data
          (lambda (&rest args) (push args watch-data))))
    (cl-flet ((should-have-watch-data (data)
                                      (should (equal (pop watch-data) data))
                                      (should (null watch-data))))
      (add-variable-watcher 'data-tests-var collect-watch-data)
      (setq data-tests-var 1)
      (should-have-watch-data '(data-tests-var 1 set nil))
      (let ((data-tests-var 2))
        (should-have-watch-data '(data-tests-var 2 let nil))
        (setq data-tests-var 3)
        (should-have-watch-data '(data-tests-var 3 set nil)))
      (should-have-watch-data '(data-tests-var 1 unlet nil))
      ;; `setq-default' on non-local variable is same as `setq'.
      (setq-default data-tests-var 4)
      (should-have-watch-data '(data-tests-var 4 set nil))
      (makunbound 'data-tests-var)
      (should-have-watch-data '(data-tests-var nil makunbound nil))
      (setq data-tests-var 5)
      (should-have-watch-data '(data-tests-var 5 set nil))
      (remove-variable-watcher 'data-tests-var collect-watch-data)
      (setq data-tests-var 6)
      (should (null watch-data)))))

(ert-deftest data-tests-varalias-watchers ()
  (defvar data-tests-var0 0)
  (defvar data-tests-var1 0)
  (defvar data-tests-var2 0)
  (defvar data-tests-var3 0)
  (let* ((watch-data nil)
         (collect-watch-data
          (lambda (&rest args) (push args watch-data))))
    (cl-flet ((should-have-watch-data (data)
                                      (should (equal (pop watch-data) data))
                                      (should (null watch-data))))
      ;; Watch var0, then alias it.
      (add-variable-watcher 'data-tests-var0 collect-watch-data)
      (defvar data-tests-var0-alias)
      (defvaralias 'data-tests-var0-alias 'data-tests-var0)
      (setq data-tests-var0 1)
      (should-have-watch-data '(data-tests-var0 1 set nil))
      (setq data-tests-var0-alias 2)
      (should-have-watch-data '(data-tests-var0 2 set nil))
      ;; Alias var1, then watch var1-alias.
      (defvar data-tests-var1-alias)
      (defvaralias 'data-tests-var1-alias 'data-tests-var1)
      (add-variable-watcher 'data-tests-var1-alias collect-watch-data)
      (setq data-tests-var1 1)
      (should-have-watch-data '(data-tests-var1 1 set nil))
      (setq data-tests-var1-alias 2)
      (should-have-watch-data '(data-tests-var1 2 set nil))
      ;; Alias var2, then watch it.
      (defvar data-tests-var2-alias)
      (defvaralias 'data-tests-var2-alias 'data-tests-var2)
      (add-variable-watcher 'data-tests-var2 collect-watch-data)
      (setq data-tests-var2 1)
      (should-have-watch-data '(data-tests-var2 1 set nil))
      (setq data-tests-var2-alias 2)
      (should-have-watch-data '(data-tests-var2 2 set nil))
      ;; Watch var3-alias, then make it alias var3 (this removes the
      ;; watcher flag).
      (defvar data-tests-var3-alias 0)
      (add-variable-watcher 'data-tests-var3-alias collect-watch-data)
      (defvaralias 'data-tests-var3-alias 'data-tests-var3)
      (should-have-watch-data '(data-tests-var3-alias
                                data-tests-var3 defvaralias nil))
      (setq data-tests-var3 1)
      (setq data-tests-var3-alias 2)
      (should (null watch-data)))))

(ert-deftest data-tests-local-variable-watchers ()
  (with-no-warnings
    (defvar-local data-tests-lvar 0))
  (let* ((buf1 (current-buffer))
         (buf2 nil)
         (watch-data nil)
         (collect-watch-data
          (lambda (&rest args) (push args watch-data))))
    (cl-flet ((should-have-watch-data (data)
                                      (should (equal (pop watch-data) data))
                                      (should (null watch-data))))
      (add-variable-watcher 'data-tests-lvar collect-watch-data)
      (setq data-tests-lvar 1)
      (should-have-watch-data `(data-tests-lvar 1 set ,buf1))
      (let ((data-tests-lvar 2))
        (should-have-watch-data `(data-tests-lvar 2 let ,buf1))
        (setq data-tests-lvar 3)
        (should-have-watch-data `(data-tests-lvar 3 set ,buf1)))
      (should-have-watch-data `(data-tests-lvar 1 unlet ,buf1))
      (setq-default data-tests-lvar 4)
      (should-have-watch-data `(data-tests-lvar 4 set nil))
      (with-temp-buffer
        (setq buf2 (current-buffer))
        (setq data-tests-lvar 1)
        (should-have-watch-data `(data-tests-lvar 1 set ,buf2))
        (let ((data-tests-lvar 2))
          (should-have-watch-data `(data-tests-lvar 2 let ,buf2))
          (setq data-tests-lvar 3)
          (should-have-watch-data `(data-tests-lvar 3 set ,buf2)))
        (should-have-watch-data `(data-tests-lvar 1 unlet ,buf2))
        (kill-local-variable 'data-tests-lvar)
        (should-have-watch-data `(data-tests-lvar nil makunbound ,buf2))
        (setq data-tests-lvar 3.5)
        (should-have-watch-data `(data-tests-lvar 3.5 set ,buf2))
        (kill-all-local-variables)
        (should-have-watch-data `(data-tests-lvar nil makunbound ,buf2)))
      (setq-default data-tests-lvar 4)
      (should-have-watch-data `(data-tests-lvar 4 set nil))
      (makunbound 'data-tests-lvar)
      (should-have-watch-data '(data-tests-lvar nil makunbound nil))
      (setq data-tests-lvar 5)
      (should-have-watch-data `(data-tests-lvar 5 set ,buf1))
      (remove-variable-watcher 'data-tests-lvar collect-watch-data)
      (setq data-tests-lvar 6)
      (should (null watch-data)))))
