;;; comp-tests.el --- unit tests for src/comp.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(defconst comp-test-directory (file-name-directory (or load-file-name
                                                       buffer-file-name)))
(defconst comp-test-src
  (concat comp-test-directory "comp-test-funcs.el"))

(defconst comp-test-dyn-src
  (concat comp-test-directory "comp-test-funcs-dyn.el"))

(message "Compiling tests...")
(load (native-compile comp-test-src))
(load (native-compile comp-test-dyn-src))



(ert-deftest comp-tests-bootstrap ()
  "Compile the compiler and load it to compile it-self.
Check that the resulting binaries do not differ."
  :tags '(:expensive-test)
  (let* ((comp-src (concat comp-test-directory
                           "../../lisp/emacs-lisp/comp.el"))
         (comp1-src (make-temp-file "stage1-" nil ".el"))
         (comp2-src (make-temp-file "stage2-" nil ".el"))
         ;; Can't use debug symbols.
         (comp-debug 0))
    (copy-file comp-src comp1-src t)
    (copy-file comp-src comp2-src t)
    (let ((load-no-native t))
      (load (concat comp-src "c") nil nil t t))
    (should-not (subr-native-elisp-p (symbol-function #'native-compile)))
    (message "Compiling stage1...")
    (let* ((t0 (current-time))
           (comp1-eln (native-compile comp1-src)))
      (message "Done in %d secs" (float-time (time-since t0)))
      (load comp1-eln nil nil t t)
      (should (subr-native-elisp-p (symbol-function 'native-compile)))
      (message "Compiling stage2...")
      (let ((t0 (current-time))
            (comp2-eln (native-compile comp2-src)))
        (message "Done in %d secs" (float-time (time-since t0)))
        (message "Comparing %s %s" comp1-eln comp2-eln)
        (should (= (call-process "cmp" nil nil nil comp1-eln comp2-eln) 0))))))

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
  (should-error (comp-tests-car-f 3)
                :type 'wrong-type-argument)
  (should (= (comp-tests-cdr-f '(1 . 2)) 2))
  (should (null (comp-tests-cdr-f nil)))
  (should-error (comp-tests-cdr-f 3)
                :type 'wrong-type-argument)
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
  (should-error (comp-tests-fixnum-1-minus-f 'a)
                :type 'wrong-type-argument)
  (should (= (comp-tests-fixnum-1-plus-f 10) 11))
  (should (= (comp-tests-fixnum-1-plus-f most-positive-fixnum)
             (1+ most-positive-fixnum)))
  (should-error (comp-tests-fixnum-1-plus-f 'a)
                :type 'wrong-type-argument)
  (should (= (comp-tests-fixnum-minus-f 10) -10))
  (should (= (comp-tests-fixnum-minus-f most-negative-fixnum)
             (- most-negative-fixnum)))
  (should-error (comp-tests-fixnum-minus-f 'a)
                :type 'wrong-type-argument))

(ert-deftest comp-tests-type-hints ()
  "Just test compiler hints are transparent in this case."
  ;; FIXME we should really check they are also effective.
  (should (= (comp-tests-hint-fixnum-f 3) 4))
  (should (= (comp-tests-hint-cons-f (cons 1 2)) 1)))

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
  (should-error (comp-tests-setcar-f 3 10)
                :type 'wrong-type-argument)
  (should-error (comp-tests-setcdr-f 3 10)
                :type 'wrong-type-argument))

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
  (should (string= (comp-tests-condition-case-0-f)
                   "arith-error Arithmetic error catched"))
  (should (string= (comp-tests-condition-case-1-f)
                   "error foo catched"))
  (should (= (comp-tests-catch-f
              (lambda () (throw 'foo 3)))
             3))
  (should (= (catch 'foo
               (comp-tests-throw-f 3)))))

(ert-deftest comp-tests-gc ()
  "Try to do some longer computation to let the gc kick in."
  (dotimes (_ 100000)
    (comp-tests-cons-cdr-f 3))
  (should (= (comp-tests-cons-cdr-f 3) 3)))

(ert-deftest comp-tests-buffer ()
  (should (string= (comp-tests-buff0-f) "foo")))

(ert-deftest comp-tests-lambda-return ()
  (let ((f (comp-tests-lambda-return-f)))
    (should (subr-native-elisp-p f))
    (should (= (funcall f 3) 4))))

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

(ert-deftest comp-tests-doc ()
  (should (string= (documentation #'comp-tests-doc-f)
                   "A nice docstring"))
  ;; Check a preloaded function, we can't use `comp-tests-doc-f' now
  ;; as this is loaded manually with no .elc.
  (should (string-match "\\.*.elc\\'" (symbol-file #'error))))

(ert-deftest comp-test-interactive-form ()
  (should (equal (interactive-form #'comp-test-interactive-form0-f)
                 '(interactive "D")))
  (should (equal (interactive-form #'comp-test-interactive-form1-f)
                 '(interactive '(1 2))))
  (should (equal (interactive-form #'comp-test-interactive-form2-f)
                 '(interactive nil)))
  (should (cl-every #'commandp '(comp-test-interactive-form0-f
                                 comp-test-interactive-form1-f
                                 comp-test-interactive-form2-f)))
  (should-not (commandp #'comp-tests-doc-f)))

(ert-deftest comp-tests-free-fun ()
  "Check we are able to compile a single function."
  (eval '(defun comp-tests-free-fun-f ()
           "Some doc."
           (interactive)
           3)
        t)
  (load (native-compile #'comp-tests-free-fun-f))

  (should (subr-native-elisp-p (symbol-function #'comp-tests-free-fun-f)))
  (should (= (comp-tests-free-fun-f) 3))
  (should (string= (documentation #'comp-tests-free-fun-f)
                   "Some doc."))
  (should (commandp #'comp-tests-free-fun-f))
  (should (equal (interactive-form #'comp-tests-free-fun-f)
                 '(interactive))))

(ert-deftest comp-test-40187 ()
  "Check function name shadowing.
https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-03/msg00914.html."
  (should (eq (comp-test-40187-1-f) 'foo))
  (should (eq (comp-test-40187-2-f) 'bar)))

(ert-deftest comp-test-speed--1 ()
  "Check that at speed -1 we do not native compile."
  (should (= (comp-test-speed--1-f) 3))
  (should-not (subr-native-elisp-p (symbol-function #'comp-test-speed--1-f))))

(ert-deftest comp-test-42360 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-07/msg00418.html>."
  (should (string= (comp-test-42360-f "Nel mezzo del " 18 0 32 "yyy" nil)
                   "Nel mezzo del     yyy")))

(defvar comp-test-primitive-advice)
(ert-deftest comp-test-primitive-advice ()
  "Test effectiveness of primitve advicing."
  (let (comp-test-primitive-advice
        (f (lambda (&rest args)
             (setq comp-test-primitive-advice args))))
    (advice-add #'+ :before f)
    (unwind-protect
        (progn
          (should (= (comp-test-primitive-advice-f 3 4) 7))
          (should (equal comp-test-primitive-advice '(3 4))))
      (advice-remove #'+ f))))


;;;;;;;;;;;;;;;;;;;;;
;; Tromey's tests. ;;
;;;;;;;;;;;;;;;;;;;;;

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
  (should-not (comp-test-eq 'a 'b)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for dynamic scope. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest comp-tests-dynamic-ffuncall ()
  "Test calling convention for dynamic binding."

  (should (equal (comp-tests-ffuncall-callee-dyn-f 1 2)
                 '(1 2)))

  (should (equal (comp-tests-ffuncall-callee-opt-dyn-f 1 2 3 4)
                 '(1 2 3 4)))
  (should (equal (comp-tests-ffuncall-callee-opt-dyn-f 1 2 3)
                 '(1 2 3 nil)))
  (should (equal (comp-tests-ffuncall-callee-opt-dyn-f 1 2)
                 '(1 2 nil nil)))

  (should (equal (comp-tests-ffuncall-callee-rest-dyn-f 1 2)
                 '(1 2 nil)))
  (should (equal (comp-tests-ffuncall-callee-rest-dyn-f 1 2 3)
                 '(1 2 (3))))
  (should (equal (comp-tests-ffuncall-callee-rest-dyn-f 1 2 3 4)
                 '(1 2 (3 4))))

  (should (equal (comp-tests-ffuncall-callee-opt-rest-dyn-f 1 2)
                 '(1 2 nil nil)))
  (should (equal (comp-tests-ffuncall-callee-opt-rest-dyn-f 1 2 3)
                 '(1 2 3 nil)))
  (should (equal (comp-tests-ffuncall-callee-opt-rest-dyn-f 1 2 3 4)
                 '(1 2 3 (4)))))

(ert-deftest comp-tests-dynamic-arity ()
  "Test func-arity on dynamic scope functions."
  (should (equal '(2 . 2)
                 (func-arity #'comp-tests-ffuncall-callee-dyn-f)))
  (should (equal '(2 . 4)
                 (func-arity #'comp-tests-ffuncall-callee-opt-dyn-f)))
  (should (equal '(2 . many)
                 (func-arity #'comp-tests-ffuncall-callee-rest-dyn-f)))
  (should (equal '(2 . many)
                 (func-arity #'comp-tests-ffuncall-callee-opt-rest-dyn-f))))

(ert-deftest comp-tests-dynamic-help-arglist ()
  "Test `help-function-arglist' works on lisp/d (bug#42572)."
  (should (equal (help-function-arglist
                  (symbol-function #'comp-tests-ffuncall-callee-opt-rest-dyn-f)
                  t)
                 '(a b &optional c &rest d))))

(ert-deftest comp-tests-cl-macro-exp ()
  "Verify CL macro expansion (bug#42088)."
  (should (equal (comp-tests-cl-macro-exp-f) '(a b))))

(ert-deftest comp-tests-cl-uninterned-arg-parse-f ()
  "Verify the parsing of a lambda list with uninterned symbols (bug#42120)."
  (should (equal (comp-tests-cl-uninterned-arg-parse-f 1 2)
                 '(1 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Middle-end specific tests. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comp-tests-mentioned-p-1 (x insn)
  (cl-loop for y in insn
           when (cond
                 ((consp y) (comp-tests-mentioned-p x y))
                 ((and (comp-mvar-p y) (comp-mvar-const-vld y))
                  (equal (comp-mvar-constant y) x))
                 (t (equal x y)))
             return t))

(defun comp-tests-mentioned-p (x insn)
  "Check if X is actively mentioned in INSN."
  (unless (eq (car-safe insn)
              'comment)
    (comp-tests-mentioned-p-1 x insn)))

(defun comp-tests-make-insn-checker (func-name checker)
  "Apply CHECKER to each insn in FUNC-NAME.
CHECKER should always return nil to have a pass."
  (should-not
   (cl-loop
    named checker-loop
    with func-c-name = (comp-c-func-name func-name "F" t)
    with f = (gethash func-c-name (comp-ctxt-funcs-h comp-ctxt))
    for bb being each hash-value of (comp-func-blocks f)
    do (cl-loop
        for insn in (comp-block-insns bb)
        when (funcall checker insn)
          do (cl-return-from checker-loop 'mentioned)))))

(defun comp-tests-tco-checker (_)
  "Check that inside `comp-tests-tco-f' we have no recursion."
  (comp-tests-make-insn-checker
   'comp-tests-tco-f
   (lambda (insn)
     (or (comp-tests-mentioned-p 'comp-tests-tco-f insn)
         (comp-tests-mentioned-p (comp-c-func-name 'comp-tests-tco-f "F" t)
                                 insn)))))

(ert-deftest comp-tests-tco ()
  "Check for tail recursion elimination."
  (let ((comp-speed 3)
        ;; Disable ipa-pure otherwise `comp-tests-tco-f' gets
        ;; optimized-out.
        (comp-disabled-passes '(comp-ipa-pure))
        (comp-post-pass-hooks '((comp-tco comp-tests-tco-checker)
                                (comp-final comp-tests-tco-checker))))
    (eval '(defun comp-tests-tco-f (a b count)
             (if (= count 0)
                 b
               (comp-tests-tco-f (+ a b) a (- count 1))))
          t)
    (load (native-compile #'comp-tests-tco-f))
    (should (subr-native-elisp-p (symbol-function #'comp-tests-tco-f)))
    (should (= (comp-tests-tco-f 1 0 10) 55))))

(defun comp-tests-fw-prop-checker-1 (_)
  "Check that inside `comp-tests-fw-prop-f' `concat' and `length' are folded."
  (comp-tests-make-insn-checker
   'comp-tests-fw-prop-1-f
   (lambda (insn)
     (or (comp-tests-mentioned-p 'concat insn)
         (comp-tests-mentioned-p 'length insn)))))

(ert-deftest comp-tests-fw-prop ()
  "Some tests for forward propagation."
  (let ((comp-speed 2)
        (comp-post-pass-hooks '((comp-final comp-tests-fw-prop-checker-1))))
    (eval '(defun comp-tests-fw-prop-1-f ()
             (let* ((a "xxx")
	            (b "yyy")
	            (c (concat a b))) ; <= has to optimize
               (length c))) ; <= has to optimize
          t)
    (load (native-compile #'comp-tests-fw-prop-1-f))
    (should (subr-native-elisp-p (symbol-function #'comp-tests-fw-prop-1-f)))
    (should (= (comp-tests-fw-prop-1-f) 6))))

(defun comp-tests-pure-checker-1 (_)
  "Check that inside `comp-tests-pure-caller-f' `comp-tests-pure-callee-f' is
 folded."
  (comp-tests-make-insn-checker
   'comp-tests-pure-caller-f
   (lambda (insn)
     (or (comp-tests-mentioned-p 'comp-tests-pure-callee-f insn)
         (comp-tests-mentioned-p (comp-c-func-name 'comp-tests-pure-callee-f "F" t)
                                 insn)))))

(defun comp-tests-pure-checker-2 (_)
  "Check that `comp-tests-pure-fibn-f' is folded."
  (comp-tests-make-insn-checker
   'comp-tests-pure-fibn-entry-f
   (lambda (insn)
     (or (comp-tests-mentioned-p 'comp-tests-pure-fibn-f insn)
         (comp-tests-mentioned-p (comp-c-func-name 'comp-tests-pure-fibn-f "F" t)
                                 insn)))))

(ert-deftest comp-tests-pure ()
  "Some tests for pure functions optimization."
  (let ((comp-speed 3)
        (comp-post-pass-hooks '((comp-final comp-tests-pure-checker-1
                                            comp-tests-pure-checker-2))))
    (load (native-compile (concat comp-test-directory "comp-test-pure.el")))

    (should (subr-native-elisp-p (symbol-function #'comp-tests-pure-caller-f)))
    (should (= (comp-tests-pure-caller-f) 4))

    (should (subr-native-elisp-p (symbol-function #'comp-tests-pure-fibn-entry-f)))
    (should (= (comp-tests-pure-fibn-entry-f) 6765))))

;;; comp-tests.el ends here
