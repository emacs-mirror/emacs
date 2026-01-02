;;; comp-tests.el --- unit tests for src/comp.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>

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
(require 'ert-x)
(require 'cl-lib)
(require 'cl-seq)
(require 'comp)
(require 'comp-cstr)

(eval-and-compile
  (defconst comp-test-src (ert-resource-file "comp-test-funcs.el"))
  (defconst comp-test-dyn-src (ert-resource-file "comp-test-funcs-dyn.el"))
  (defconst comp-test-dyn-src2 (ert-resource-file "comp-test-funcs-dyn2.el")))

(when (native-comp-available-p)
  (message "Compiling tests...")
  (load (native-compile comp-test-src))
  (load (native-compile comp-test-dyn-src)))

;; Load the test code here so the compiler can check the function
;; names used in this file.
(require 'comp-test-funcs comp-test-src)
(require 'comp-test-dyn-funcs comp-test-dyn-src) ;Non-standard feature name!
(require 'comp-test-funcs-dyn2 comp-test-dyn-src2)

(defmacro comp-deftest (name args &rest docstring-and-body)
  "Define a test for the native compiler tagging it as :nativecomp."
  (declare (indent defun)
           (doc-string 3))
  `(ert-deftest ,(intern (concat "comp-tests-" (symbol-name name))) ,args
     :tags '(:nativecomp)
     ,@(and (stringp (car docstring-and-body))
            (list (pop docstring-and-body)))
     ;; Some of the tests leave spill files behind -- so create a
     ;; sub-dir where native-comp can do its work, and then delete it
     ;; at the end.
     (ert-with-temp-directory dir
       (let ((temporary-file-directory dir))
         ,@docstring-and-body))))



(defvar native-comp-eln-load-path)
(ert-deftest comp-tests-bootstrap ()
  "Compile the compiler and load it to compile it-self.
Check that the resulting binaries do not differ."
  :tags '(:expensive-test :nativecomp)
  (ert-with-temp-file comp1-src
    :suffix "-comp-stage1.el"
    (ert-with-temp-file comp2-src
      :suffix "-comp-stage2.el"
      (let* ((byte+native-compile t)
             (native-compile-target-directory
              (car (last native-comp-eln-load-path)))
             (comp-src (expand-file-name "../../../lisp/emacs-lisp/comp.el"
                                         (ert-resource-directory)))
             ;; Can't use debug symbols.
             (native-comp-debug 0))
        (copy-file comp-src comp1-src t)
        (copy-file comp-src comp2-src t)
        (let ((load-no-native t))
          (load (concat comp-src "c") nil nil t t))
        (should-not (native-comp-function-p (symbol-function 'native-compile)))
        (message "Compiling stage1...")
        (let* ((t0 (current-time))
               (comp1-eln (native-compile comp1-src)))
          (message "Done in %d secs" (float-time (time-since t0)))
          (load comp1-eln nil nil t t)
          (should (native-comp-function-p (symbol-function 'native-compile)))
          (message "Compiling stage2...")
          (let ((t0 (current-time))
                (comp2-eln (native-compile comp2-src)))
            (message "Done in %d secs" (float-time (time-since t0)))
            (message "Comparing %s %s" comp1-eln comp2-eln)
            (should (= (call-process "cmp" nil nil nil comp1-eln comp2-eln) 0))))))))

(comp-deftest provide ()
  "Testing top level provide."
  (should (featurep 'comp-test-funcs)))

(comp-deftest varref ()
  "Testing varref."
  (should (= (comp-tests-varref-f) 3)))

(comp-deftest list ()
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

(comp-deftest comp-tests-cons-car-cdr ()
  "Testing cons car cdr."
  (should (= (comp-tests-cons-car-f) 1))
  (should (= (comp-tests-cons-cdr-f 3) 3)))

(comp-deftest varset ()
  "Testing varset."
  (comp-tests-varset0-f)
  (should (= comp-tests-var1 55))

  (should (= (comp-tests-varset1-f) 4))
  (should (= comp-tests-var1 66)))

(comp-deftest length ()
  "Testing length."
  (should (= (comp-tests-length-f) 3)))

(comp-deftest aref-aset ()
  "Testing aref and aset."
  (should (= (comp-tests-aref-aset-f) 100)))

(comp-deftest symbol-value ()
  "Testing `symbol-value'."
  (should (= (comp-tests-symbol-value-f) 3)))

(comp-deftest concat ()
  "Testing concatX opcodes."
  (should (string= (comp-tests-concat-f "bar") "abcdabcabfoobar")))

(comp-deftest ffuncall ()
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

(comp-deftest jump-table ()
  "Testing jump tables"
  (should (eq (comp-tests-jump-table-1-f 'x) 'a))
  (should (eq (comp-tests-jump-table-1-f 'y) 'b))
  (should (eq (comp-tests-jump-table-1-f 'xxx) 'c))

  ;; Jump table not with eq as test
  (should (eq (comp-tests-jump-table-2-f "aaa") 'a))
  (should (eq (comp-tests-jump-table-2-f "bbb") 'b)))

(comp-deftest conditionals ()
  "Testing conditionals."
  (should (= (comp-tests-conditionals-1-f t) 1))
  (should (= (comp-tests-conditionals-1-f nil) 2))
  (should (= (comp-tests-conditionals-2-f t) 1340))
  (should (eq (comp-tests-conditionals-2-f nil) nil)))

(comp-deftest fixnum ()
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

(comp-deftest type-hints ()
  "Just test compiler hints are transparent in this case."
  ;; FIXME we should really check they are also effective.
  (should (= (comp-tests-hint-fixnum-f 3) 4))
  (should (= (comp-tests-hint-cons-f (cons 1 2)) 1)))

(comp-deftest arith-comp ()
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

(comp-deftest setcarcdr ()
  "Testing setcar setcdr."
  (should (equal (comp-tests-setcar-f (cons 10 10) 3) '(3 . 10)))
  (should (equal (comp-tests-setcdr-f (cons 10 10) 3) '(10 . 3)))
  (should-error (comp-tests-setcar-f 3 10)
                :type 'wrong-type-argument)
  (should-error (comp-tests-setcdr-f 3 10)
                :type 'wrong-type-argument))

(comp-deftest bubble-sort ()
  "Run bubble sort."
  (let* ((list1 (mapcar #'random (make-list 1000 most-positive-fixnum)))
         (list2 (copy-sequence list1)))
    (should (equal (comp-bubble-sort-f list1)
                   (sort list2 #'<)))))

(comp-deftest apply ()
  "Test some inlined list functions."
  (should (eq (comp-tests-consp-f '(1)) t))
  (should (eq (comp-tests-consp-f 1) nil))
  (let ((x (cons 1 2)))
    (should (= (comp-tests-setcar2-f x) 3))
    (should (equal x '(3 . 2)))))

(comp-deftest num-inline ()
  "Test some inlined number functions."
  (should (eq (comp-tests-integerp-f 1) t))
  (should (eq (comp-tests-integerp-f '(1)) nil))
  (should (eq (comp-tests-integerp-f 3.5) nil))
  (should (eq (comp-tests-integerp-f (1+ most-negative-fixnum)) t))

  (should (eq (comp-tests-numberp-f 1) t))
  (should (eq (comp-tests-numberp-f 'a) nil))
  (should (eq (comp-tests-numberp-f 3.5) t)))

(comp-deftest stack ()
  "Test some stack operation."
  (should (= (comp-tests-discardn-f 10) 2))
  (should (string= (with-temp-buffer
                      (comp-tests-insertn-f "a" "b" "c" "d")
                      (buffer-string))
                   "abcd")))

(comp-deftest non-locals ()
  "Test non locals."
  (should (string= (comp-tests-condition-case-0-f)
                   "arith-error Arithmetic error caught"))
  (should (string= (comp-tests-condition-case-1-f)
                   "error Foo caught"))
  (should (= (comp-tests-catch-f
              (lambda () (throw 'foo 3)))
             3))
  (should (= (catch 'foo
               (comp-tests-throw-f 3))
             3)))

(comp-deftest gc ()
  "Try to do some longer computation to let the GC kick in."
  (dotimes (_ 100000)
    (comp-tests-cons-cdr-f 3))
  (should (= (comp-tests-cons-cdr-f 3) 3)))

(comp-deftest buffer ()
  (should (string= (comp-tests-buff0-f) "foo")))

(comp-deftest lambda-return ()
  (let ((f (comp-tests-lambda-return-f)))
    (should (native-comp-function-p f))
    (should (= (funcall f 3) 4))))

(comp-deftest lambda-return2 ()
  "Check a nested lambda function gets natively compiled."
  (let ((f (comp-tests-lambda-return-f2)))
    (should (native-comp-function-p f))
    (let ((f2 (funcall f)))
      (should (native-comp-function-p f2))
      (should (= (funcall f2 3) 4)))))

(comp-deftest recursive ()
  (should (= (comp-tests-fib-f 10) 55)))

(comp-deftest macro ()
  "Just check we can define macros"
  (should (macrop (symbol-function 'comp-tests-macro-m))))

(comp-deftest string-trim ()
  (should (string= (comp-tests-string-trim-f "dsaf ") "dsaf")))

(comp-deftest trampoline-removal ()
  ;; This tests that we can call primitives with no dedicated bytecode.
  ;; At speed >= 2 the trampoline will not be used.
  (should (hash-table-p (comp-tests-trampoline-removal-f))))

(comp-deftest signal ()
  (should (equal (condition-case err
                     (comp-tests-signal-f)
                   (t err))
                 '(foo . t))))

(comp-deftest func-call-removal ()
  ;; See `comp-propagate-insn' `comp-function-call-remove'.
  (should (= (comp-tests-func-call-removal-f) 1)))

(comp-deftest doc ()
  (should (string= (documentation #'comp-tests-doc-f)
                   "A nice docstring."))
  ;; Check a preloaded function, we can't use `comp-tests-doc-f' now
  ;; as this is loaded manually with no .elc.
  (should (string-match "\\.*.elc\\'" (symbol-file #'error))))

(comp-deftest interactive-form ()
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

(declare-function comp-tests-free-fun-f nil)

(comp-deftest free-fun ()
  "Check we are able to compile a single function."
  (eval '(defun comp-tests-free-fun-f ()
           "Some doc."
           (interactive)
           3)
        t)
  (native-compile #'comp-tests-free-fun-f)

  (should (native-comp-function-p (symbol-function 'comp-tests-free-fun-f)))
  (should (= (comp-tests-free-fun-f) 3))
  (should (string= (documentation #'comp-tests-free-fun-f)
                   "Some doc."))
  (should (commandp #'comp-tests-free-fun-f))
  (should (equal (interactive-form #'comp-tests-free-fun-f)
                 '(interactive nil))))

(declare-function comp-tests-free-fun-f2 nil)

(comp-deftest free-fun2 ()
  "Check compiling a symbol's function compiles contained lambdas."
  (eval '(defun comp-tests-free-fun-f2 ()
           (lambda (x)
             "Some doc."
             (interactive)
             x)))
  (native-compile #'comp-tests-free-fun-f2)

  (let* ((f (symbol-function 'comp-tests-free-fun-f2))
         (f2 (funcall f)))
    (should (native-comp-function-p f))
    (should (native-comp-function-p f2))
    (should (string= (documentation f2) "Some doc."))
    (should (commandp f2))
    (should (equal (interactive-form f2) '(interactive nil)))
    (should (= (funcall f2 3) 3))))

(declare-function comp-tests/free\fun-f nil)

(comp-deftest free-fun-silly-name ()
  "Check we are able to compile a single function."
  (eval '(defun comp-tests/free\fun-f ()) t)
  (native-compile #'comp-tests/free\fun-f)
  (should (native-comp-function-p (symbol-function 'comp-tests/free\fun-f))))

(comp-deftest bug-40187 ()
  "Check function name shadowing.
https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-03/msg00914.html."
  (should (eq (comp-test-40187-1-f) 'foo))
  (should (eq (comp-test-40187-2-f) 'bar)))

(comp-deftest speed--1 ()
  "Check that at speed -1 we do not native compile."
  (should (= (comp-test-speed--1-f) 3))
  (should-not (native-comp-function-p (symbol-function 'comp-test-speed--1-f))))

(comp-deftest bug-42360 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-07/msg00418.html>."
  (should (string= (comp-test-42360-f "Nel mezzo del " 18 0 32 "yyy" nil)
                   "Nel mezzo del     yyy")))

(comp-deftest bug-44968 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-11/msg02357.html>"
  (comp-test-44968-f "/tmp/test/foo" "/tmp"))

(comp-deftest bug-45342 ()
  "Preserve multibyte immediate strings.
<https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-12/msg01771.html>"
  (should (string= " ➊" (comp-test-45342-f 1))))

(comp-deftest assume-double-neg ()
  "In fwprop assumptions (not (not (member x))) /= (member x)."
  (should-not (comp-test-assume-double-neg-f "bar" "foo")))

(comp-deftest assume-in-loop-1 ()
  "Broken call args assumptions lead to infinite loop."
  (should (equal (comp-test-assume-in-loop-1-f "cd") '("cd"))))

(comp-deftest bug-45376-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-12/msg01883.html>"
  (should (equal (comp-test-45376-1-f) '(1 0))))

(comp-deftest bug-45376-2 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-12/msg01883.html>"
  (should (equal (comp-test-45376-2-f) '(0 2 1 0 1 0 1 0 0 0 0 0))))

(defvar comp-test-primitive-advice)
(comp-deftest primitive-advice ()
  "Test effectiveness of primitive advising."
  (let (comp-test-primitive-advice
        (f (lambda (&rest args)
             (setq comp-test-primitive-advice args))))
    (advice-add #'+ :before f)
    (unwind-protect
        (progn
          (should (= (comp-test-primitive-advice-f 3 4) 7))
          (should (equal comp-test-primitive-advice '(3 4))))
      (advice-remove #'+ f))))

(defvar comp-test-primitive-redefine-args nil)
(comp-deftest primitive-redefine ()
  "Test effectiveness of primitive redefinition."
  (cl-letf ((comp-test-primitive-redefine-args nil)
            ((symbol-function '-)
             (lambda (&rest args)
	       (setq comp-test-primitive-redefine-args args)
               'xxx)))
    (should (eq (comp-test-primitive-redefine-f 10 2) 'xxx))
    (should (equal comp-test-primitive-redefine-args '(10 2)))))

(comp-deftest compile-forms ()
  "Verify lambda form native compilation."
  (should-error (native-compile '(+ 1 foo)))
  (let ((f (native-compile '(lambda (x) (1+ x)))))
    (should (native-comp-function-p f))
    (should (= (funcall f 2) 3)))
  (let* ((lexical-binding nil)
         (f (native-compile '(lambda (x) (1+ x)))))
    (should (native-comp-function-p f))
    (should (= (funcall f 2) 3))))

(comp-deftest compile-interpreted-functions ()
  "Verify native compilation of interpreted functions."
  (let ((f (native-compile (eval '(lambda (x) (1+ x))))))
    (should (native-comp-function-p f))
    (should (= (funcall f 2) 3))))

(comp-deftest comp-test-defsubst ()
  ;; Bug#42664, Bug#43280, Bug#44209.
  (should-not (native-comp-function-p (symbol-function 'comp-test-defsubst-f))))

(comp-deftest primitive-redefine-compile-44221 ()
  "Test the compiler still works while primitives are redefined (bug#44221)."
  (cl-letf (((symbol-function 'delete-region)
             (lambda (_ _))))
    (should (native-comp-function-p
             (native-compile
              '(lambda ()
                 (delete-region (point-min) (point-max))))))))

(comp-deftest and-3 ()
  (should (= (comp-test-and-3-f t) 2))
  (should (null (comp-test-and-3-f '(1 2)))))

(comp-deftest copy-insn ()
  (should (equal (comp-test-copy-insn-f '(1 2 3 (4 5 6)))
                 '(1 2 3 (4 5 6))))
  (should (null (comp-test-copy-insn-f nil))))

(comp-deftest cond-rw-1 ()
  "Check cond-rw does not break target blocks with multiple predecessor."
  (should (null (comp-test-cond-rw-1-2-f))))

(comp-deftest not-cons-1 ()
  (should-not (comp-test-not-cons-f nil)))

(comp-deftest 45576-1 ()
  "Functionp satisfies also symbols.
<https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-01/msg00029.html>."
  (should (eq (comp-test-45576-f) 'eval)))

(comp-deftest 45635-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-01/msg00158.html>."
  (should (string= (comp-test-45635-f :height 180 :family "PragmataPro Liga")
                   "PragmataPro Liga")))

(comp-deftest 46670-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-02/msg01413.html>"
  (should (string= (comp-test-46670-2-f "foo") "foo"))
  (should (equal (subr-type (symbol-function 'comp-test-46670-2-f))
                 '(function (t) t))))

(comp-deftest 46824-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-02/msg01949.html>"
  (should (equal (comp-test-46824-1-f) nil)))

(comp-deftest comp-test-47868-1 ()
  "Verify string hash consing strategy.

<https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-04/msg00921.html>"
  (should-not (equal-including-properties (comp-test-47868-1-f)
                                          (comp-test-47868-2-f)))
  (should (eq (comp-test-47868-1-f) (comp-test-47868-3-f)))
  (should (eq (comp-test-47868-2-f) (comp-test-47868-4-f))))

(comp-deftest 48029-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2022-07/msg00666.html>"
  (should (native-comp-function-p
           (symbol-function 'comp-test-48029-nonascii-žžž-f))))

(comp-deftest 61917-1 ()
  "Verify we can compile calls to redefined primitives with
dedicated byte-op code."
  (let (x
        (f (lambda (_fn &rest args)
             (setq comp-test-primitive-redefine-args args))))
    (advice-add #'delete-region :around f)
    (unwind-protect
        (setf x (native-compile
                 '(lambda ()
                    (delete-region 1 2))))
      (should (native-comp-function-p x))
      (funcall x)
      (advice-remove #'delete-region f)
      (should (equal comp-test-primitive-redefine-args '(1 2))))))

(comp-deftest 67239-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-11/msg00925.html>"
  (should-not (comp-test-67239-1-f)))

(comp-deftest comp-test-73270-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-09/msg00794.html>"
  (should (eq (comp-test-73270-1-f (make-comp-test-73270-child4)) 'child4)))

(comp-deftest comp-test-78606-1 ()
  "<https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-05/msg01270.html>"
  (should (let ((x 1.0))
            (eq (comp-test-78606-1-f x) x))))


;;;;;;;;;;;;;;;;;;;;;
;; Tromey's tests. ;;
;;;;;;;;;;;;;;;;;;;;;

(comp-deftest consp ()
  (should-not (comp-test-consp 23))
  (should-not (comp-test-consp nil))
  (should (comp-test-consp '(1 . 2))))

(comp-deftest listp ()
  (should-not (comp-test-listp 23))
  (should (comp-test-listp nil))
  (should (comp-test-listp '(1 . 2))))

(comp-deftest stringp ()
  (should-not (comp-test-stringp 23))
  (should-not (comp-test-stringp nil))
  (should (comp-test-stringp "hi")))

(comp-deftest symbolp ()
  (should-not (comp-test-symbolp 23))
  (should-not (comp-test-symbolp "hi"))
  (should (comp-test-symbolp 'whatever)))

(comp-deftest integerp ()
  (should (comp-test-integerp 23))
  (should-not (comp-test-integerp 57.5))
  (should-not (comp-test-integerp "hi"))
  (should-not (comp-test-integerp 'whatever)))

(comp-deftest numberp ()
  (should (comp-test-numberp 23))
  (should (comp-test-numberp 57.5))
  (should-not (comp-test-numberp "hi"))
  (should-not (comp-test-numberp 'whatever)))

(comp-deftest add1 ()
  (should (eq (comp-test-add1 23) 24))
  (should (eq (comp-test-add1 -17) -16))
  (should (eql (comp-test-add1 1.0) 2.0))
  (should-error (comp-test-add1 nil)
                :type 'wrong-type-argument))

(comp-deftest sub1 ()
  (should (eq (comp-test-sub1 23) 22))
  (should (eq (comp-test-sub1 -17) -18))
  (should (eql (comp-test-sub1 1.0) 0.0))
  (should-error (comp-test-sub1 nil)
                :type 'wrong-type-argument))

(comp-deftest negate ()
  (should (eq (comp-test-negate 23) -23))
  (should (eq (comp-test-negate -17) 17))
  (should (eql (comp-test-negate 1.0) -1.0))
  (should-error (comp-test-negate nil)
                :type 'wrong-type-argument))

(comp-deftest not ()
  (should (eq (comp-test-not 23) nil))
  (should (eq (comp-test-not nil) t))
  (should (eq (comp-test-not t) nil)))

(comp-deftest bobp-and-eobp ()
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

(comp-deftest car-cdr ()
  (let ((pair '(1 . b)))
    (should (eq (comp-test-car pair) 1))
    (should (eq (comp-test-car nil) nil))
    (should-error (comp-test-car 23)
                  :type 'wrong-type-argument)
    (should (eq (comp-test-cdr pair) 'b))
    (should (eq (comp-test-cdr nil) nil))
    (should-error (comp-test-cdr 23)
                  :type 'wrong-type-argument)))

(comp-deftest car-cdr-safe ()
  (let ((pair '(1 . b)))
    (should (eq (comp-test-car-safe pair) 1))
    (should (eq (comp-test-car-safe nil) nil))
    (should (eq (comp-test-car-safe 23) nil))
    (should (eq (comp-test-cdr-safe pair) 'b))
    (should (eq (comp-test-cdr-safe nil) nil))
    (should (eq (comp-test-cdr-safe 23) nil))))

(comp-deftest eq ()
  (should (comp-test-eq 'a 'a))
  (should (comp-test-eq 5 5))
  (should-not (comp-test-eq 'a 'b)))

(comp-deftest if ()
  (should (eq (comp-test-if 'a 'b) 'a))
  (should (eq (comp-test-if 0 23) 0))
  (should (eq (comp-test-if nil 'b) 'b)))

(comp-deftest and ()
  (should (eq (comp-test-and 'a 'b) 'b))
  (should (eq (comp-test-and 0 23) 23))
  (should (eq (comp-test-and nil 'b) nil)))

(comp-deftest or ()
  (should (eq (comp-test-or 'a 'b) 'a))
  (should (eq (comp-test-or 0 23) 0))
  (should (eq (comp-test-or nil 'b) 'b)))

(comp-deftest save-excursion ()
  (with-temp-buffer
    (comp-test-save-excursion)
    (should (eq (point) (point-min)))
    (should (eq (comp-test-current-buffer) (current-buffer)))))

(comp-deftest > ()
  (should (eq (comp-test-> 0 23) nil))
  (should (eq (comp-test-> 23 0) t)))

(comp-deftest catch ()
  (should (eq (comp-test-catch 0 1 2 3 4) nil))
  (should (eq (comp-test-catch 20 21 22 23 24 25 26 27 28) 24)))

(comp-deftest memq ()
  (should (equal (comp-test-memq 0 '(5 4 3 2 1 0)) '(0)))
  (should (eq (comp-test-memq 72 '(5 4 3 2 1 0)) nil)))

(comp-deftest listN ()
  (should (equal (comp-test-listN 57)
                 '(57 57 57 57 57 57 57 57 57 57 57 57 57 57 57 57))))

(comp-deftest concatN ()
  (should (equal (comp-test-concatN "x") "xxxxxx")))

(comp-deftest opt-rest ()
  (should (equal (comp-test-opt-rest 1) '(1 nil nil)))
  (should (equal (comp-test-opt-rest 1 2) '(1 2 nil)))
  (should (equal (comp-test-opt-rest 1 2 3) '(1 2 (3))))
  (should (equal (comp-test-opt-rest 1 2 56 57 58)
                 '(1 2 (56 57 58)))))

(comp-deftest opt ()
  (should (equal (comp-test-opt 23) '(23)))
  (should (equal (comp-test-opt 23 24) '(23 . 24)))
  (should-error (comp-test-opt)
                :type 'wrong-number-of-arguments)
  (should-error (comp-test-opt nil 24 97)
                :type 'wrong-number-of-arguments))

(comp-deftest unwind-protect ()
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

(comp-deftest dynamic-ffuncall ()
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

(comp-deftest dynamic-arity ()
  "Test func-arity on dynamic scope functions."
  (should (equal '(2 . 2)
                 (func-arity #'comp-tests-ffuncall-callee-dyn-f)))
  (should (equal '(2 . 4)
                 (func-arity #'comp-tests-ffuncall-callee-opt-dyn-f)))
  (should (equal '(2 . many)
                 (func-arity #'comp-tests-ffuncall-callee-rest-dyn-f)))
  (should (equal '(2 . many)
                 (func-arity #'comp-tests-ffuncall-callee-opt-rest-dyn-f))))

(comp-deftest dynamic-help-arglist ()
  "Test `help-function-arglist' works on lisp/d (bug#42572)."
  (should (equal (help-function-arglist
                  (symbol-function 'comp-tests-ffuncall-callee-opt-rest-dyn-f)
                  t)
                 '(a b &optional c &rest d))))

(comp-deftest cl-macro-exp ()
  "Verify CL macro expansion (bug#42088)."
  (should (equal (comp-tests-cl-macro-exp-f) '(a b))))

(comp-deftest cl-uninterned-arg-parse-f ()
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
                 ((and (comp-mvar-p y) (comp-cstr-imm-vld-p y))
                  (equal (comp-cstr-imm y) x))
                 (t (equal x y)))
             return t))

(defun comp-tests-mentioned-p (x insn)
  "Check if X is actively mentioned in INSN."
  (unless (eq (car-safe insn)
              'comment)
    (comp-tests-mentioned-p-1 x insn)))

(defun comp-tests-map-checker (func-name checker)
  "Apply CHECKER to each insn of FUNC-NAME.
Return a list of results."
  (cl-loop
    with func-c-name = (comp-c-func-name (or func-name 'anonymous-lambda) "F" t)
    with f = (gethash func-c-name (comp-ctxt-funcs-h comp-ctxt))
    for bb being each hash-value of (comp-func-blocks f)
    nconc
    (cl-loop
     for insn in (comp-block-insns bb)
     collect (funcall checker insn))))

(defun comp-tests-tco-checker (_)
  "Check that inside `comp-tests-tco-f' we have no recursion."
  (should
   (cl-notany
    #'identity
    (comp-tests-map-checker
     'comp-tests-tco-f
     (lambda (insn)
       (or (comp-tests-mentioned-p 'comp-tests-tco-f insn)
           (comp-tests-mentioned-p (comp-c-func-name 'comp-tests-tco-f "F" t)
                                   insn)))))))

(declare-function comp-tests-tco-f nil)

(comp-deftest tco ()
  "Check for tail recursion elimination."
  (let ((native-comp-speed 3)
        ;; Disable ipa-pure otherwise `comp-tests-tco-f' gets
        ;; optimized-out.
        (comp-disabled-passes '(comp--ipa-pure))
        (comp-post-pass-hooks '((comp--tco comp-tests-tco-checker)
                                (comp-final comp-tests-tco-checker))))
    (eval '(defun comp-tests-tco-f (a b count)
             (if (= count 0)
                 b
               (comp-tests-tco-f (+ a b) a (- count 1))))
          t)
    (native-compile #'comp-tests-tco-f)
    (should (native-comp-function-p (symbol-function 'comp-tests-tco-f)))
    (should (= (comp-tests-tco-f 1 0 10) 55))))

(defun comp-tests-fw-prop-checker-1 (_)
  "Check that inside `comp-tests-fw-prop-1-f' `concat' and `length' are folded."
  (should
   (cl-notany
    #'identity
    (comp-tests-map-checker
     'comp-tests-fw-prop-1-f
     (lambda (insn)
       (or (comp-tests-mentioned-p 'concat insn)
           (comp-tests-mentioned-p 'length insn)))))))

(declare-function comp-tests-fw-prop-1-f nil)

(comp-deftest fw-prop-1 ()
  "Some tests for forward propagation."
  (let ((native-comp-speed 2)
        (comp-post-pass-hooks '((comp--final comp-tests-fw-prop-checker-1))))
    (eval '(defun comp-tests-fw-prop-1-f ()
             (let* ((a "xxx")
	            (b "yyy")
	            (c (concat a b))) ; <= has to optimize
               (length c))) ; <= has to optimize
          t)
    (native-compile #'comp-tests-fw-prop-1-f)
    (should (native-comp-function-p (symbol-function 'comp-tests-fw-prop-1-f)))
    (should (= (comp-tests-fw-prop-1-f) 6))))

(defun comp-tests--type-lists-equal (l1 l2)
  (and (= (length l1) (length l2))
       (cl-every #'comp-tests--types-equal l1 l2)))

(defun comp-tests--types-equal (t1 t2)
 "Whether the types T1 and T2 are equal."
 (or (equal t1 t2)   ; for atoms, and optimization for the common case
     (and (consp t1) (consp t2)
          (eq (car t1) (car t2))
          (cond ((memq (car t1) '(and or member))
                 ;; Order or duplicates don't matter.
                 (null (cl-set-exclusive-or (cdr t1) (cdr t2)
                                            :test #'comp-tests--types-equal)))
                ((eq (car t1) 'function)
                 (and (comp-tests--type-lists-equal (nth 1 t1) (nth 1 t2))
                      (comp-tests--types-equal (nth 2 t1) (nth 2 t2))))
                (t (comp-tests--type-lists-equal (cdr t1) (cdr t2)))))))

(defun comp-tests-check-ret-type-spec (func-form ret-type)
  (let ((lexical-binding t)
        (native-comp-speed 2)
        (f-name (cl-second func-form)))
    (eval func-form t)
    (native-compile f-name)
    (should (comp-tests--types-equal
             (cl-third (subr-type (symbol-function f-name)))
             ret-type))))

(cl-eval-when (compile eval load)
  (cl-defstruct comp-foo a b)
  (cl-defstruct (comp-bar (:include comp-foo)) c)
  (defconst comp-tests-type-spec-tests
    ;; Why we quote everything here, you ask?  So that values of
    ;; `most-positive-fixnum' and `most-negative-fixnum', which can be
    ;; architecture-dependent, do not end up hardcoded in the
    ;; resulting byte-compiled file, and thus we could run the same
    ;; .elc file on several architectures without fear.
    '(
      ;; 1
      ((defun comp-tests-ret-type-spec-f (x)
         x)
       't)

      ;; 2
      ((defun comp-tests-ret-type-spec-f ()
         1)
       '(integer 1 1))

      ;; 3
      ((defun comp-tests-ret-type-spec-f (x)
         (if x 1 3))
       '(or (integer 1 1) (integer 3 3)))

      ;; 4
      ((defun comp-tests-ret-type-spec-f (x)
         (let (y)
           (if x
               (setf y 1)
             (setf y 2))
           y))
       '(integer 1 2))

      ;; 5
      ((defun comp-tests-ret-type-spec-f (x)
         (let (y)
           (if x
               (setf y 1)
             (setf y 3))
           y))
       '(or (integer 1 1) (integer 3 3)))

      ;; 6
      ((defun comp-tests-ret-type-spec-f (x)
         (if x
             (list x)
           3))
       '(or cons (integer 3 3)))

      ;; 7
      ((defun comp-tests-ret-type-spec-f (x)
         (if x
             'foo
           3))
       '(or (member foo) (integer 3 3)))

      ;; 8
      ((defun comp-tests-ret-type-spec-f (x)
         (if (eq x 3)
             x
           'foo))
       '(or (member foo) (integer 3 3)))

      ;; 9
      ((defun comp-tests-ret-type-spec-f (x)
         (if (eq 3 x)
             x
           'foo))
       '(or (member foo) (integer 3 3)))

      ;; 10
      ((defun comp-tests-ret-type-spec-f (x)
         (if (eql x 3)
             x
           'foo))
       '(or (member foo) (integer 3 3)))

      ;; 11
      ((defun comp-tests-ret-type-spec-f (x)
         (if (eql 3 x)
             x
           'foo))
       '(or (member foo) (integer 3 3)))

      ;; 12
      ((defun comp-tests-ret-type-spec-f (x)
         (if (eql x 3)
             'foo
           x))
       '(not (integer 3 3)))

      ;; 13
      ((defun comp-tests-ret-type-spec-f (x y)
         (if (= x y)
             x
           'foo))
       '(or (member foo) number-or-marker))

      ;; 14
      ((defun comp-tests-ret-type-spec-f (x)
         (comp-hint-fixnum x))
       `(integer ,most-negative-fixnum ,most-positive-fixnum))

      ;; 15
      ((defun comp-tests-ret-type-spec-f (x)
         (comp-hint-cons x))
       'cons)

      ;; 16
      ((defun comp-tests-ret-type-spec-f (x)
         (let (y)
           (when x
             (setf y 4))
           y))
       '(or null (integer 4 4)))

      ;; 17
      ((defun comp-tests-ret-type-spec-f ()
         (let (x
               (y 3))
           (setf x y)
           y))
       '(integer 3 3))

      ;; 18
      ((defun comp-tests-ret-type-spec-f (x)
         (let ((y 3))
           (when x
             (setf y x))
           y))
       't)

      ;; 19
      ((defun comp-tests-ret-type-spec-f (x y)
         (eq x y))
       'boolean)

      ;; 20
      ((defun comp-tests-ret-type-spec-f (x)
         (when x
           'foo))
       '(or (member foo) null))

      ;; 21
      ((defun comp-tests-ret-type-spec-f (x)
         (unless x
           'foo))
       '(or (member foo) null))

      ;; 22
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (> x 3)
	   x))
       '(or null float (integer 4 *)))

      ;; 23
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (>= x 3)
	   x))
       '(or null float (integer 3 *)))

      ;; 24
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (< x 3)
	   x))
       '(or null float (integer * 2)))

      ;; 25
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (<= x 3)
	   x))
       '(or null float (integer * 3)))

      ;; 26
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (> 3 x)
	   x))
       '(or null float (integer * 2)))

      ;; 27
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (>= 3 x)
	   x))
       '(or null float (integer * 3)))

      ;; 28
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (< 3 x)
	   x))
       '(or null float (integer 4 *)))

      ;; 29
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (<= 3 x)
	   x))
       '(or null float (integer 3 *)))

      ;; 30
      ((defun comp-tests-ret-type-spec-f (x)
         (let ((y 3))
	   (when (> x y)
	     x)))
       '(or null float (integer 4 *)))

      ;; 31
      ((defun comp-tests-ret-type-spec-f (x)
         (let ((y 3))
	   (when (> y x)
	     x)))
       '(or null float (integer * 2)))

      ;; 32
      ((defun comp-tests-ret-type-spec-f (x)
         (when (and (> x 3)
		    (< x 10))
	   x))
       '(or null float (integer 4 9)))

      ;; 33
      ((defun comp-tests-ret-type-spec-f (x)
         (when (or (> x 3)
                   (< x 10))
	   x))
       '(or null float integer))

      ;; 34
      ((defun comp-tests-ret-type-spec-f (x)
         (when (or (< x 3)
                   (> x 10))
	   x))
       '(or null float (integer * 2) (integer 11 *)))

      ;; 35 No float range support.
      ((defun comp-tests-ret-type-spec-f (x)
	 (when (> x 1.0)
	   x))
       '(or null number-or-marker))

      ;; 36
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (> x 3)
                    (> y 2))
           (+ x y)))
       '(or null float (integer 7 *)))

      ;; 37
      ;; SBCL: (OR REAL NULL)
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (<= x 3)
                    (<= y 2))
           (+ x y)))
       '(or null float (integer * 5)))

      ;; 38
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (< 1 x 5)
	            (< 1 y 5))
           (+ x y)))
       '(or null float (integer 4 8)))

      ;; 39
      ;; SBCL gives: (OR REAL NULL)
      ((defun comp-tests-ret-type-spec-f (x y)
	 (when (and (<= 1 x 10)
		    (<= 2 y 3))
	   (+ x y)))
       '(or null float (integer 3 13)))

      ;; 40
      ;; SBCL: (OR REAL NULL)
      ((defun comp-tests-ret-type-spec-f (x y)
	 (when (and (<= 1 x 10)
		    (<= 2 y 3))
	   (- x y)))
       '(or null float (integer -2 8)))

      ;; 41
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (<= 1 x)
                    (<= 2 y 3))
           (- x y)))
       '(or null float (integer -2 *)))

      ;; 42
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (<= 1 x 10)
                    (<= 2 y))
           (- x y)))
       '(or null float (integer * 8)))

      ;; 43
      ((defun comp-tests-ret-type-spec-f (x y)
	 (when (and (<= x 10)
		    (<= 2 y))
	   (- x y)))
       '(or null float (integer * 8)))

      ;; 44
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (<= x 10)
                    (<= y 3))
           (- x y)))
       '(or null float integer))

      ;; 45
      ((defun comp-tests-ret-type-spec-f (x y)
         (when (and (<= 2 x)
                    (<= 3 y))
           (- x y)))
       '(or null float integer))

      ;; 46
      ;; SBCL: (OR (RATIONAL (6) (30)) (SINGLE-FLOAT 6.0 30.0)
      ;;           (DOUBLE-FLOAT 6.0d0 30.0d0) NULL)
      ((defun comp-tests-ret-type-spec-f (x y z i j k)
         (when (and (< 1 x 5)
	            (< 1 y 5)
	            (< 1 z 5)
	            (< 1 i 5)
	            (< 1 j 5)
	            (< 1 k 5))
           (+ x y z i j k)))
       '(or null float (integer 12 24)))

      ;; 47
      ((defun comp-tests-ret-type-spec-f (x)
         (when (<= 1 x 5)
           (1+ x)))
       '(or null float (integer 2 6)))

      ;;48
      ((defun comp-tests-ret-type-spec-f (x)
         (when (<= 1 x 5)
           (1- x)))
       '(or null float (integer 0 4)))

      ;; 49
      ((defun comp-tests-ret-type-spec-f ()
         (error "Foo"))
       'nil)

      ;; 50
      ((defun comp-tests-ret-type-spec-f (x)
         (if (stringp x)
	     x
           'bar))
       '(or (member bar) string))

      ;; 51
      ((defun comp-tests-ret-type-spec-f (x)
         (if (stringp x)
             'bar
           x))
       '(not string))

      ;; 52
      ((defun comp-tests-ret-type-spec-f (x)
         (if (integerp x)
             x
           'bar))
       '(or (member bar) integer))

      ;; 53
      ((defun comp-tests-ret-type-spec-f (x)
         (when (integerp x)
           x))
       '(or null integer))

      ;; 54
      ((defun comp-tests-ret-type-spec-f (x)
         (unless (symbolp x)
           x))
       't)

      ;; 55
      ((defun comp-tests-ret-type-spec-f (x)
         (unless (integerp x)
           x))
       '(not integer))

      ;; 56
      ((defun comp-tests-ret-type-spec-f (x)
         (cl-ecase x
           (1 (message "one"))
           (5 (message "five")))
         x)
       't
       ;; FIXME improve `comp--cond-cstrs-target-mvar' to cross block
       ;; boundary if necessary as this should return:
       ;; (or (integer 1 1) (integer 5 5))
       )

      ;; 57
      ((defun comp-tests-ret-type-spec-f (x)
         (unless (or (eq x 'foo)
	             (eql x 3))
           (error "Not foo or 3"))
         x)
       '(or (member foo) (integer 3 3)))

      ;;58
      ((defun comp-tests-ret-type-spec-f (x y)
         (if (and (natnump x)
                  (natnump y)
                  (<= x y))
             x
           (error "")))
       '(integer 0 *))

      ;; 59
      ((defun comp-tests-ret-type-spec-f (x y)
         (if (and (>= x 3)
                  (<= y 10)
                  (<= x y))
             x
           (error "")))
       '(or float (integer 3 10)))

      ;; 60
      ((defun comp-tests-ret-type-spec-f (x y)
		    (if (and (<= x 10)
			     (>= y 3)
			     (>= x y))
             x
           (error "")))
       '(or float (integer 3 10)))

      ;; 61
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (= x 1.0)
             x
           (error "")))
       '(or (member 1.0) (integer 1 1)))

      ;; 62
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (= x 1.0)
             x
           (error "")))
       '(or (member 1.0) (integer 1 1)))

      ;; 63
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (= x 1.1)
             x
           (error "")))
       '(member 1.1))

      ;; 64
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (= x 1)
             x
           (error "")))
       '(or (member 1.0) (integer 1 1)))

      ;; 65
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (= x 1)
             x
           (error "")))
       '(or (member 1.0) (integer 1 1)))

      ;; 66
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (eql x 0.0)
	     x
	   (error "")))
       'float)

      ;; 67
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (equal x '(1 2 3))
	     x
	   (error "")))
       'cons)

      ;; 68
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (and (floatp x)
	          (= x 1))
             x
           (error "")))
       ;; Conservative (see cstr relax in `comp-cstr-=').
       '(or (member 1.0) (integer 1 1)))

      ;; 69
      ((defun comp-tests-ret-type-spec-f (x)
	 (if (and (integer x)
	          (= x 1))
             x
           (error "")))
       ;; Conservative (see cstr relax in `comp-cstr-=').
       '(or (member 1.0) (integer 1 1)))

      ;; 70
      ((defun comp-tests-ret-type-spec-f (x y)
	 (if (and (floatp x)
	          (integerp y)
	          (= x y))
             x
           (error "")))
       '(or float integer))

      ;; 71
      ((defun comp-tests-ret-type-spec-f (x)
         (if (= x 0.0)
             x
           (error "")))
       '(or (member -0.0 0.0) (integer 0 0)))

      ;; 72
      ((defun comp-tests-ret-type-spec-f (x)
         (unless (= x 0.0)
           (error ""))
         (unless (eql x -0.0)
           (error ""))
         x)
       'float)

      ;; 73
      ((defun comp-tests-ret-type-spec-f (x)
         (when (eql x 1.0)
	   (error ""))
         x)
       't)

      ;; 74
      ((defun comp-tests-ret-type-spec-f (x)
         (if (eq x 0)
	     (error "")
	   (1+ x)))
       'number)

      ;; 75
      ((defun comp-tests-ret-type-spec-f ()
         (make-comp-foo))
       'comp-foo)

      ;; 76
      ((defun comp-tests-ret-type-spec-f ()
         (make-comp-bar))
       'comp-bar)

      ;; 77
      ((defun comp-tests-ret-type-spec-f (x)
          (setf (comp-foo-a x) 2)
          x)
       'comp-foo)

      ;; 78
      ((defun comp-tests-ret-type-spec-f (x)
          (if x
              (if (> x 11)
	          x
	        (make-comp-foo))
            (make-comp-bar)))
       '(or comp-foo float (integer 12 *)))

      ;; 79
      ((defun comp-tests-ret-type-spec-f (x)
         (if (comp-foo-p x)
             x
           (error "")))
       'comp-foo)

      ;; 80
      ((defun comp-tests-ret-type-spec-f (x)
         (if (functionp x)
             (error "")
           x))
       '(not function))
      ;; 81
      ((defun comp-tests-ret-type-spec-f (x)
         (print (comp-foo-p x))
         (comp-foo-p x))
       'boolean)))

  (defun comp-tests-define-type-spec-test (number x)
    `(comp-deftest ,(intern (format "ret-type-spec-%d" number)) ()
                   ,(format "Type specifier test number %d." number)
                   (let ((comp-ctxt (make-comp-cstr-ctxt)))
                     (comp-tests-check-ret-type-spec ',(car x) ,(cadr x))))))

(defmacro comp-tests-define-type-spec-tests ()
  "Define all type specifier tests."
  `(progn
     ,@(cl-loop
        for test in comp-tests-type-spec-tests
        for n from 1
        collect (comp-tests-define-type-spec-test n test))))

(comp-tests-define-type-spec-tests)

(defun comp-tests-pure-checker-1 (_)
  "Check that inside `comp-tests-pure-caller-f' `comp-tests-pure-callee-f' is
folded."
  (should
   (cl-notany
    #'identity
    (comp-tests-map-checker
     'comp-tests-pure-caller-f
     (lambda (insn)
       (or (comp-tests-mentioned-p 'comp-tests-pure-callee-f insn)
           (comp-tests-mentioned-p (comp-c-func-name
                                    'comp-tests-pure-callee-f "F" t)
                                   insn)))))))

(defun comp-tests-pure-checker-2 (_)
  "Check that `comp-tests-pure-fibn-f' is folded."
  (should
   (cl-notany
    #'identity
    (comp-tests-map-checker
     'comp-tests-pure-fibn-entry-f
     (lambda (insn)
       (or (comp-tests-mentioned-p 'comp-tests-pure-fibn-f insn)
           (comp-tests-mentioned-p (comp-c-func-name 'comp-tests-pure-fibn-f "F" t)
                                   insn)))))))

(comp-deftest pure ()
  "Some tests for pure functions optimization."
  (let ((native-comp-speed 3)
        (comp-post-pass-hooks '((comp--final comp-tests-pure-checker-1
                                             comp-tests-pure-checker-2))))
    (load (native-compile (ert-resource-file "comp-test-pure.el")))
    (declare-function comp-tests-pure-caller-f nil)
    (declare-function comp-tests-pure-fibn-entry-f nil)

    (should (native-comp-function-p (symbol-function 'comp-tests-pure-caller-f)))
    (should (= (comp-tests-pure-caller-f) 4))

    (should (native-comp-function-p (symbol-function 'comp-tests-pure-fibn-entry-f)))
    (should (= (comp-tests-pure-fibn-entry-f) 6765))))

(comp-deftest comp-tests-result-lambda ()
  (native-compile 'comp-tests-result-lambda)
  (should (eq (funcall (comp-tests-result-lambda) '(a . b)) 'a)))

(defun comp-tests-type-branch-optim-checker (_)
  "Check there's only a single call to `type-of'."
  (should (= (cl-count t (comp-tests-map-checker
                          #'comp-tests-type-branch-optim-1-f
                          (lambda (insn)
                            (pcase insn
                              (`(set ,_mvar-1 (call type-of ,_mvar-2))
                               t)))))
             1)))

(declare-function comp-tests-type-branch-optim-1-f nil)

(comp-deftest comp-tests-type-branch-optim ()
  (let ((native-comp-speed 2)
        (comp-post-pass-hooks '((comp--final comp-tests-type-branch-optim-checker))))
    (eval '(progn
             (cl-defstruct type-branch-optim-struct a b c)
             (defun comp-tests-type-branch-optim-1-f (x)
               (setf (type-branch-optim-struct-a x) 3)
               (+ (type-branch-optim-struct-b x) (type-branch-optim-struct-c x))))
          t)
    (native-compile #'comp-tests-type-branch-optim-1-f)))

;;; comp-tests.el ends here
