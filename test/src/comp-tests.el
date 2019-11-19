;;; comp-tests.el --- unit tests for src/comp.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.org>

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

;; Unit tests for src/comp.c.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'comp)

;; (setq comp-debug 1)
(setq comp-speed 3)

(defconst comp-test-directory (file-name-directory (or load-file-name
                                                       buffer-file-name)))
(defconst comp-test-src
  (concat comp-test-directory "comp-test-funcs.el"))

(message "Compiling %s" comp-test-src)
(load (native-compile comp-test-src))

(ert-deftest comp-tests-bootstrap ()
  "Compile the compiler and load it to compile it-self.
Check that the resulting binaries do not differ."
  (let* ((comp-src (concat comp-test-directory
                           "../../lisp/emacs-lisp/comp.el"))
         (comp1-src (concat temporary-file-directory
                            (make-temp-name "stage1-")
                            ".el"))
         (comp2-src (concat temporary-file-directory
                            (make-temp-name "stage2-")
                            ".el"))
         (comp1 (concat comp1-src "n"))
         (comp2 (concat comp2-src "n")))
    (copy-file comp-src comp1-src)
    (copy-file comp-src comp2-src)
    (load (concat comp-src "c") nil nil t t)
    (should (null (subr-native-elisp-p (symbol-function #'native-compile))))
    (message "Compiling stage1...")
    (load (native-compile comp1-src) nil nil t t)
    (should (subr-native-elisp-p (symbol-function 'native-compile)))
    (message "Compiling stage2...")
    (native-compile comp2-src)
    (message "Comparing %s %s" comp1 comp2)
    (should (= (call-process "cmp" nil nil nil comp1 comp2) 0))))

(ert-deftest comp-tests-provide ()
  "Testing top level provide."
  (should (featurep 'comp-test-funcs)))

(ert-deftest comp-tests-varref ()
  "Testing varref."
  (should (= (comp-tests-varref-f) 3)))

(ert-deftest comp-tests-list ()
  "Testing cons car cdr."
  (should (equal (comp-tests-list-f) '(1 2 3)))
  (should (equal (comp-tests-list2-f 1 2 3) '(1 2 3)))
  (should (= (comp-tests-car-f '(1 . 2)) 1))
  (should (null (comp-tests-car-f nil)))
  (should (= (condition-case err
                 (comp-tests-car-f 3)
               (error 10))
             10))
  (should (= (comp-tests-cdr-f '(1 . 2)) 2))
  (should (null (comp-tests-cdr-f nil)))
  (should (= (condition-case err
                 (comp-tests-cdr-f 3)
               (error 10))
             10))
  (should (= (comp-tests-car-safe-f '(1 . 2)) 1))
  (should (null (comp-tests-car-safe-f 'a)))
  (should (= (comp-tests-cdr-safe-f '(1 . 2)) 2))
  (should (null (comp-tests-cdr-safe-f 'a))))

(ert-deftest  comp-tests-cons-car-cdr ()
  "Testing cons car cdr."
  (should (= (comp-tests-cons-car-f) 1))
  (should (= (comp-tests-cons-cdr-f 3) 3)))

(ert-deftest comp-tests-varset ()
  "Testing varset."
  (comp-tests-varset0-f)
  (should (= comp-tests-var1 55))

  (should (= (comp-tests-varset1-f) 4))
  (should (= comp-tests-var1 66)))

(ert-deftest comp-tests-length ()
  "Testing length."
  (should (= (comp-tests-length-f) 3)))

(ert-deftest comp-tests-aref-aset ()
  "Testing aref and aset."
  (should (= (comp-tests-aref-aset-f) 100)))

(ert-deftest comp-tests-symbol-value ()
  "Testing aref and aset."
  (should (= (comp-tests-symbol-value-f) 3)))

(ert-deftest comp-tests-concat ()
  "Testing concatX opcodes."
  (should (string= (comp-tests-concat-f "bar") "abcdabcabfoobar")))

(ert-deftest comp-tests-ffuncall ()
  "Test calling conventions."

  ;; (defun comp-tests-ffuncall-caller-f ()
  ;;   (comp-tests-ffuncall-callee-f 1 2 3))

  ;; (should (equal (comp-tests-ffuncall-caller-f) '(1 2 3)))

  ;; ;; After it gets compiled
  ;; (native-compile #'comp-tests-ffuncall-callee-f)
  ;; (should (equal (comp-tests-ffuncall-caller-f) '(1 2 3)))

  ;; ;; Recompiling the caller once with callee already compiled
  ;; (defun comp-tests-ffuncall-caller-f ()
  ;;   (comp-tests-ffuncall-callee-f 1 2 3))
  ;; (should (equal (comp-tests-ffuncall-caller-f) '(1 2 3)))

  (should (equal (comp-tests-ffuncall-callee-optional-f 1 2 3 4)
                 '(1 2 3 4)))
  (should (equal (comp-tests-ffuncall-callee-optional-f 1 2 3)
                 '(1 2 3 nil)))
  (should (equal (comp-tests-ffuncall-callee-optional-f 1 2)
                 '(1 2 nil nil)))

  (should (equal (comp-tests-ffuncall-callee-rest-f 1 2)
                 '(1 2 nil)))
  (should (equal (comp-tests-ffuncall-callee-rest-f 1 2 3)
                 '(1 2 (3))))
  (should (equal (comp-tests-ffuncall-callee-rest-f 1 2 3 4)
                 '(1 2 (3 4))))

  (should (equal (comp-tests-ffuncall-callee-more8-f 1 2 3 4 5 6 7 8 9 10)
                 '(1 2 3 4 5 6 7 8 9 10)))

  (should (equal (comp-tests-ffuncall-callee-more8-rest-f 1 2 3 4 5 6 7 8 9 10 11)
                 '(1 2 3 4 5 6 7 8 9 (10 11))))

  (should (equal (comp-tests-ffuncall-native-f) [nil]))

  (should (equal (comp-tests-ffuncall-native-rest-f) [1 2 3]))

  (should (equal (comp-tests-ffuncall-apply-many-f '(1 2 3))
                 '(1 2 3)))

  (should (= (comp-tests-ffuncall-lambda-f 1) 2)))

(ert-deftest comp-tests-jump-table ()
  "Testing jump tables"
  (should (eq (comp-tests-jump-table-1-f 'x) 'a))
  (should (eq (comp-tests-jump-table-1-f 'y) 'b))
  (should (eq (comp-tests-jump-table-1-f 'xxx) 'c))

  ;; Jump table not with eq as test
  (should (eq (comp-tests-jump-table-2-f "aaa") 'a))
  (should (eq (comp-tests-jump-table-2-f "bbb") 'b)))

(ert-deftest comp-tests-conditionals ()
  "Testing conditionals."
  (should (= (comp-tests-conditionals-1-f t) 1))
  (should (= (comp-tests-conditionals-1-f nil) 2))
  (should (= (comp-tests-conditionals-2-f t) 1340))
  (should (eq (comp-tests-conditionals-2-f nil) nil)))

(ert-deftest comp-tests-fixnum ()
  "Testing some fixnum inline operation."
  (should (= (comp-tests-fixnum-1-minus-f 10) 9))
  (should (= (comp-tests-fixnum-1-minus-f most-negative-fixnum)
             (1- most-negative-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-1-minus-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a)))
  (should (= (comp-tests-fixnum-1-plus-f 10) 11))
  (should (= (comp-tests-fixnum-1-plus-f most-positive-fixnum)
             (1+ most-positive-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-1-plus-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a)))
  (should (= (comp-tests-fixnum-minus-f 10) -10))
  (should (= (comp-tests-fixnum-minus-f most-negative-fixnum)
             (- most-negative-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-minus-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a))))

(ert-deftest comp-tests-arith-comp ()
  "Testing arithmetic comparisons."
  (should (eq (comp-tests-eqlsign-f 4 3) nil))
  (should (eq (comp-tests-eqlsign-f 3 3) t))
  (should (eq (comp-tests-eqlsign-f 2 3) nil))
  (should (eq (comp-tests-gtr-f 4 3) t))
  (should (eq (comp-tests-gtr-f 3 3) nil))
  (should (eq (comp-tests-gtr-f 2 3) nil))
  (should (eq (comp-tests-lss-f 4 3) nil))
  (should (eq (comp-tests-lss-f 3 3) nil))
  (should (eq (comp-tests-lss-f 2 3) t))
  (should (eq (comp-tests-les-f 4 3) nil))
  (should (eq (comp-tests-les-f 3 3) t))
  (should (eq (comp-tests-les-f 2 3) t))
  (should (eq (comp-tests-geq-f 4 3) t))
  (should (eq (comp-tests-geq-f 3 3) t))
  (should (eq (comp-tests-geq-f 2 3) nil)))

(ert-deftest comp-tests-setcarcdr ()
  "Testing setcar setcdr."
  (should (equal (comp-tests-setcar-f '(10 . 10) 3) '(3 . 10)))
  (should (equal (comp-tests-setcdr-f '(10 . 10) 3) '(10 . 3)))
  (should (equal (condition-case
                     err
                     (comp-tests-setcar-f 3 10)
                   (error err))
                 '(wrong-type-argument consp 3)))
  (should (equal (condition-case
                     err
                     (comp-tests-setcdr-f 3 10)
                   (error err))
                 '(wrong-type-argument consp 3))))

(ert-deftest comp-tests-bubble-sort ()
  "Run bubble sort."
  (let* ((list1 (mapcar #'random (make-list 1000 most-positive-fixnum)))
         (list2 (copy-sequence list1)))
    (should (equal (comp-bubble-sort-f list1)
                   (sort list2 #'<)))))

(ert-deftest comp-test-apply ()
  "Test some inlined list functions."
  (should (eq (comp-tests-consp-f '(1)) t))
  (should (eq (comp-tests-consp-f 1) nil))
  (let ((x (cons 1 2)))
    (should (= (comp-tests-setcar2-f x) 3))
    (should (equal x '(3 . 2)))))

(ert-deftest comp-tests-num-inline ()
  "Test some inlined number functions."
  (should (eq (comp-tests-integerp-f 1) t))
  (should (eq (comp-tests-integerp-f '(1)) nil))
  (should (eq (comp-tests-integerp-f 3.5) nil))
  (should (eq (comp-tests-integerp-f (1+ most-negative-fixnum)) t))

  (should (eq (comp-tests-numberp-f 1) t))
  (should (eq (comp-tests-numberp-f 'a) nil))
  (should (eq (comp-tests-numberp-f 3.5) t)))

(ert-deftest comp-tests-stack ()
  "Test some stack operation."
  (should (= (comp-tests-discardn-f 10) 2))
  (should (string= (with-temp-buffer
                      (comp-tests-insertn-f "a" "b" "c" "d")
                      (buffer-string))
                   "abcd")))

(ert-deftest comp-tests-non-locals ()
  "Test non locals."
  (let ((gc-cons-threshold most-positive-fixnum)) ;; FIXME!!
    (should (string= (comp-tests-condition-case-0-f)
                     "arith-error Arithmetic error catched"))
    (should (string= (comp-tests-condition-case-1-f)
                     "error foo catched"))
    (should (= (comp-tests-catch-f
                                (lambda () (throw 'foo 3)))
               3))
    (should (= (catch 'foo
                 (comp-tests-throw-f 3))))))

(ert-deftest comp-tests-gc ()
  "Try to do some longer computation to let the gc kick in."
  (dotimes (_ 100000)
    (comp-tests-cons-cdr-f 3))
  (should (= (comp-tests-cons-cdr-f 3) 3)))

(ert-deftest comp-tests-buffer ()
  (should (string= (comp-tests-buff0-f) "foo")))

(ert-deftest comp-tests-lambda-return ()
  (should (= (funcall (comp-tests-lambda-return-f) 3) 4)))

(ert-deftest comp-tests-recursive ()
  (should (= (comp-tests-fib-f 10) 55)))

(ert-deftest comp-tests-macro ()
  "Just check we can define macros"
  (should (macrop (symbol-function 'comp-tests-macro-m))))

(ert-deftest comp-tests-string-trim ()
  (should (string= (comp-tests-string-trim-f "dsaf ") "dsaf")))

(ert-deftest comp-tests-trampoline-removal ()
  ;; This tests that we can can call primitives with no dedicated bytecode.
  ;; At speed >= 2 the trampoline will not be used.
  (should (hash-table-p (comp-tests-trampoline-removal-f))))

(ert-deftest comp-tests-signal ()
  (should (equal (condition-case err
                     (comp-tests-signal-f)
                   (t err))
                 '(foo . t))))

(ert-deftest comp-tests-func-call-removal ()
  ;; See `comp-propagate-insn' `comp-function-call-remove'.
  (should (= (comp-tests-func-call-removal-f) 1)))


;;;;;;;;;;;;;;;;;;;;
;; Tromey's tests ;;
;;;;;;;;;;;;;;;;;;;;

(ert-deftest comp-consp ()
  (should-not (comp-test-consp 23))
  (should-not (comp-test-consp nil))
  (should (comp-test-consp '(1 . 2))))

(ert-deftest comp-listp ()
  (should-not (comp-test-listp 23))
  (should (comp-test-listp nil))
  (should (comp-test-listp '(1 . 2))))

(ert-deftest comp-stringp ()
  (should-not (comp-test-stringp 23))
  (should-not (comp-test-stringp nil))
  (should (comp-test-stringp "hi")))

(ert-deftest comp-symbolp ()
  (should-not (comp-test-symbolp 23))
  (should-not (comp-test-symbolp "hi"))
  (should (comp-test-symbolp 'whatever)))

(ert-deftest comp-integerp ()
  (should (comp-test-integerp 23))
  (should-not (comp-test-integerp 57.5))
  (should-not (comp-test-integerp "hi"))
  (should-not (comp-test-integerp 'whatever)))

(ert-deftest comp-numberp ()
  (should (comp-test-numberp 23))
  (should (comp-test-numberp 57.5))
  (should-not (comp-test-numberp "hi"))
  (should-not (comp-test-numberp 'whatever)))

(ert-deftest comp-add1 ()
  (should (eq (comp-test-add1 23) 24))
  (should (eq (comp-test-add1 -17) -16))
  (should (eql (comp-test-add1 1.0) 2.0))
  (should-error (comp-test-add1 nil)
                :type 'wrong-type-argument))

(ert-deftest comp-sub1 ()
  (should (eq (comp-test-sub1 23) 22))
  (should (eq (comp-test-sub1 -17) -18))
  (should (eql (comp-test-sub1 1.0) 0.0))
  (should-error (comp-test-sub1 nil)
                :type 'wrong-type-argument))

(ert-deftest comp-negate ()
  (should (eq (comp-test-negate 23) -23))
  (should (eq (comp-test-negate -17) 17))
  (should (eql (comp-test-negate 1.0) -1.0))
  (should-error (comp-test-negate nil)
                :type 'wrong-type-argument))

(ert-deftest comp-not ()
  (should (eq (comp-test-not 23) nil))
  (should (eq (comp-test-not nil) t))
  (should (eq (comp-test-not t) nil)))

(ert-deftest comp-bobp-and-eobp ()
  (with-temp-buffer
    (should (comp-test-bobp))
    (should (comp-test-eobp))
    (insert "hi")
    (goto-char (point-min))
    (should (eq (comp-test-point-min) (point-min)))
    (should (eq (comp-test-point) (point-min)))
    (should (comp-test-bobp))
    (should-not (comp-test-eobp))
    (goto-char (point-max))
    (should (eq (comp-test-point-max) (point-max)))
    (should (eq (comp-test-point) (point-max)))
    (should-not (comp-test-bobp))
    (should (comp-test-eobp))))

(ert-deftest comp-car-cdr ()
  (let ((pair '(1 . b)))
    (should (eq (comp-test-car pair) 1))
    (should (eq (comp-test-car nil) nil))
    (should-error (comp-test-car 23)
                  :type 'wrong-type-argument)
    (should (eq (comp-test-cdr pair) 'b))
    (should (eq (comp-test-cdr nil) nil))
    (should-error (comp-test-cdr 23)
                  :type 'wrong-type-argument)))

(ert-deftest comp-car-cdr-safe ()
  (let ((pair '(1 . b)))
    (should (eq (comp-test-car-safe pair) 1))
    (should (eq (comp-test-car-safe nil) nil))
    (should (eq (comp-test-car-safe 23) nil))
    (should (eq (comp-test-cdr-safe pair) 'b))
    (should (eq (comp-test-cdr-safe nil) nil))
    (should (eq (comp-test-cdr-safe 23) nil))))

(ert-deftest comp-eq ()
  (should (comp-test-eq 'a 'a))
  (should (comp-test-eq 5 5))
  (should-not (comp-test-eq 'a 'b))
  (should-not (comp-test-eq "x" "x")))

(ert-deftest comp-if ()
  (should (eq (comp-test-if 'a 'b) 'a))
  (should (eq (comp-test-if 0 23) 0))
  (should (eq (comp-test-if nil 'b) 'b)))

(ert-deftest comp-and ()
  (should (eq (comp-test-and 'a 'b) 'b))
  (should (eq (comp-test-and 0 23) 23))
  (should (eq (comp-test-and nil 'b) nil)))

(ert-deftest comp-or ()
  (should (eq (comp-test-or 'a 'b) 'a))
  (should (eq (comp-test-or 0 23) 0))
  (should (eq (comp-test-or nil 'b) 'b)))

(ert-deftest comp-save-excursion ()
  (with-temp-buffer
    (comp-test-save-excursion)
    (should (eq (point) (point-min)))
    (should (eq (comp-test-current-buffer) (current-buffer)))))

(ert-deftest comp-> ()
  (should (eq (comp-test-> 0 23) nil))
  (should (eq (comp-test-> 23 0) t)))

(ert-deftest comp-catch ()
  (should (eq (comp-test-catch 0 1 2 3 4) nil))
  (should (eq (comp-test-catch 20 21 22 23 24 25 26 27 28) 24)))

(ert-deftest comp-memq ()
  (should (equal (comp-test-memq 0 '(5 4 3 2 1 0)) '(0)))
  (should (eq (comp-test-memq 72 '(5 4 3 2 1 0)) nil)))

(ert-deftest comp-listN ()
  (should (equal (comp-test-listN 57)
                 '(57 57 57 57 57 57 57 57 57 57 57 57 57 57 57 57))))

(ert-deftest comp-concatN ()
  (should (equal (comp-test-concatN "x") "xxxxxx")))

(ert-deftest comp-opt-rest ()
  (should (equal (comp-test-opt-rest 1) '(1 nil nil)))
  (should (equal (comp-test-opt-rest 1 2) '(1 2 nil)))
  (should (equal (comp-test-opt-rest 1 2 3) '(1 2 (3))))
  (should (equal (comp-test-opt-rest 1 2 56 57 58)
                 '(1 2 (56 57 58)))))

(ert-deftest comp-opt ()
  (should (equal (comp-test-opt 23) '(23)))
  (should (equal (comp-test-opt 23 24) '(23 . 24)))
  (should-error (comp-test-opt)
                :type 'wrong-number-of-arguments)
  (should-error (comp-test-opt nil 24 97)
                :type 'wrong-number-of-arguments))

(ert-deftest comp-unwind-protect ()
  (comp-test-unwind-protect 'ignore)
  (should (eq comp-test-up-val 999))
  (condition-case nil
      (comp-test-unwind-protect (lambda () (error "HI")))
    (error
     nil))
  (should (eq comp-test-up-val 999)))

;;; comp-tests.el ends here
