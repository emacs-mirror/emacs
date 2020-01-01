;;;; testcases.el -- Test cases for testcover-tests.el

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Gemini Lasswell

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
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Commentary:

;; * This file should not be loaded directly.  It is meant to be read
;;   by `testcover-tests-build-test-cases'.
;;
;; * Test cases begin with ;; ==== name ====.  The symbol name between
;;   the ===='s is used to create the name of the test.
;;
;; * Following the beginning comment place the test docstring and
;;   any tags or keywords for ERT.  These will be spliced into the
;;   ert-deftest for the test.
;;
;; * To separate the above from the test case code, use another
;;   comment: ;; ====
;;
;; * These special comments should start at the beginning of a line.
;;
;; * `testcover-tests-skeleton' will prompt you for a test name and
;;   insert the special comments.
;;
;; * The test case code should be annotated with %%% at the end of
;;   each form where a tan splotch is expected, and !!! at the end
;;   of each form where a red mark is expected.
;;
;; * If Testcover is working correctly on your code sample, using
;;   `testcover-tests-markup-region' and
;;   `testcover-tests-unmarkup-region' can make creating test cases
;;   easier.

;;; Code:
;;; Test Cases:

;; ==== constants-bug-25316 ====
"Testcover doesn't splotch constants."
;; ====
(defconst testcover-testcase-const "apples")
(defun testcover-testcase-zero () 0)
(defun testcover-testcase-list-consts ()
  (list
   emacs-version 10
   "hello"
   `(a b c ,testcover-testcase-const)
   '(1 2 3)
   testcover-testcase-const
   (testcover-testcase-zero)
   nil))

(defun testcover-testcase-add-to-const-list (arg)
  (cons arg%%% (testcover-testcase-list-consts))%%%)

(should (equal (testcover-testcase-add-to-const-list 'a)
               `(a ,emacs-version 10 "hello" (a b c "apples") (1 2 3)
                   "apples" 0 nil)))

;; ==== customize-defcustom-bug-25326 ====
"Testcover doesn't prevent testing of defcustom values."
;; ====
(defgroup testcover-testcase nil
  "Test case for testcover"
  :group 'lisp
  :prefix "testcover-testcase-"
  :version "26.0")
(defcustom testcover-testcase-flag t
  "Test value used by testcover-tests.el"
  :type 'boolean
  :group 'testcover-testcase)
(defun testcover-testcase-get-flag ()
  testcover-testcase-flag)

(testcover-testcase-get-flag)
(setq testcover-testcase-flag (not testcover-testcase-flag))
(testcover-testcase-get-flag)

;; ==== no-returns ====
"Testcover doesn't splotch functions which don't return."
;; ====
(defun testcover-testcase-play-ball (retval)
  (catch 'ball
    (throw 'ball retval%%%))%%%)  ; catch gets marked but not throw

(defun testcover-testcase-not-my-favorite-error-message ()
  (signal 'wrong-type-argument (list 'consp nil)))

(should (testcover-testcase-play-ball t))
(condition-case nil
    (testcover-testcase-not-my-favorite-error-message)
  (error nil))

;; ==== noreturn-symbol ====
"Wrapping a form with noreturn prevents splotching."
;; ====
(defun testcover-testcase-cancel (spacecraft)
  (error "no destination for %s" spacecraft))
(defun testcover-testcase-launch (spacecraft planet)
  (if (null planet)
      (noreturn (testcover-testcase-cancel spacecraft%%%))
    (list spacecraft%%% planet%%%)%%%)%%%)
(defun testcover-testcase-launch-2 (spacecraft planet)
  (if (null planet%%%)%%%
    (testcover-testcase-cancel spacecraft%%%)!!!
    (list spacecraft!!! planet!!!)!!!)!!!)
(should (equal (testcover-testcase-launch "Curiosity" "Mars") '("Curiosity" "Mars")))
(condition-case err
    (testcover-testcase-launch "Voyager" nil)
  (error err))
(condition-case err
    (testcover-testcase-launch-2 "Voyager II" nil)
  (error err))

(should-error (testcover-testcase-launch "Voyager" nil))
(should-error (testcover-testcase-launch-2 "Voyager II" nil))

;; ==== 1-value-symbol-bug-25316 ====
"Wrapping a form with 1value prevents splotching."
;; ====
(defun testcover-testcase-always-zero (num)
  (- num%%% num%%%)%%%)
(defun testcover-testcase-still-always-zero (num)
  (1value (- num%%% num%%% (- num%%% num%%%)%%%)))
(defun testcover-testcase-never-called (num)
  (1value (/ num!!! num!!!)!!!)!!!)
(should (eql 0 (testcover-testcase-always-zero 3)))
(should (eql 0 (testcover-testcase-still-always-zero 5)))

;; ==== dotimes-dolist ====
"Dolist and dotimes with a 1valued return value are 1valued."
;; ====
(defun testcover-testcase-do-over (things)
  (dolist (thing things%%%)
    (list thing))
  (dolist (thing things%%% 42)
    (list thing))
  (dolist (thing things%%% things%%%)
    (list thing))%%%)
(defun testcover-testcase-do-more (count)
  (dotimes (num count%%%)
    (+ num num))
  (dotimes (num count%%% count%%%)
    (+ num num))%%%
    (dotimes (num count%%% 0)
      (+ num num)))
(should (equal '(a b c) (testcover-testcase-do-over '(a b c))))
(should (eql 0 (testcover-testcase-do-more 2)))

;; ==== let-last-form ====
"A let form is 1valued if its last form is 1valued."
;; ====
(defun testcover-testcase-double (num)
  (let ((double (* num%%% 2)%%%))
    double%%%)%%%)
(defun testcover-testcase-nullbody-let (num)
  (let* ((square (* num%%% num%%%)%%%)
         (double (* 2 num%%%)%%%))))
(defun testcover-testcase-answer ()
  (let ((num 100))
    42))
(should-not (testcover-testcase-nullbody-let 3))
(should (eql (testcover-testcase-answer) 42))
(should (eql (testcover-testcase-double 10) 20))

;; ==== if-with-1value-clauses ====
"An if is 1valued if both then and else are 1valued."
;; ====
(defun testcover-testcase-describe (val)
  (if (zerop val%%%)%%%
    "a number"
    "a different number"))
(defun testcover-testcase-describe-2 (val)
  (if (zerop val)
      "zero"
    "not zero"))
(defun testcover-testcase-describe-3 (val)
  (if (zerop val%%%)%%%
    "zero"
    (format "%d" val%%%)%%%)%%%)
(should (equal (testcover-testcase-describe 0) "a number"))
(should (equal (testcover-testcase-describe-2 0) "zero"))
(should (equal (testcover-testcase-describe-2 1) "not zero"))
(should (equal (testcover-testcase-describe-3 1) "1"))

;; ==== cond-with-1value-clauses ====
"A cond form is marked 1valued if all clauses are 1valued."
;; ====
(defun testcover-testcase-cond (num)
  (cond
   ((eql num%%% 0)%%% 'a)
   ((eql num%%% 1)%%% 'b)
   ((eql num!!! 2)!!! 'c)))
(defun testcover-testcase-cond-2 (num)
  (cond
   ((eql num%%% 0)%%% (cons 'a 0)!!!)
   ((eql num%%% 1)%%% 'b))%%%)
(should (eql (testcover-testcase-cond 1) 'b))
(should (eql (testcover-testcase-cond-2 1) 'b))

;; ==== condition-case-with-1value-components ====
"A condition-case is marked 1valued if its body and handlers are."
;; ====
(defun testcover-testcase-cc (arg)
  (condition-case nil
      (if (null arg%%%)%%%
        (error "foo")
        "0")!!!
        (error nil)))
(should-not (testcover-testcase-cc nil))

;; ==== quotes-within-backquotes-bug-25316 ====
"Forms to analyze are found within quotes within backquotes."
;; ====
(defun testcover-testcase-make-list ()
  (list 'defun 'defvar))
(defmacro testcover-testcase-bq-macro (arg)
  (declare (debug t))
  `(memq ,arg%%% '(defconst ,@(testcover-testcase-make-list)))%%%)
(defun testcover-testcase-use-bq-macro (arg)
  (testcover-testcase-bq-macro arg%%%)%%%)
(should (equal '(defun defvar) (testcover-testcase-use-bq-macro 'defun)))

;; ==== progn-functions ====
"Some forms are 1value if their last argument is 1value."
;; ====
(defun testcover-testcase-one (arg)
  (progn
    (setq arg (1- arg%%%)%%%)%%%)%%%
    (progn
      (setq arg (1+ arg%%%)%%%)%%%
      1))

(should (eql 1 (testcover-testcase-one 0)))
;; ==== prog1-functions ====
"Some forms are 1value if their first argument is 1value."
;; ====
(defun testcover-testcase-unwinder (arg)
  (unwind-protect
      (if ( > arg%%% 0)%%%
        1
        0)
    (format "unwinding %s!" arg%%%)%%%))
(defun testcover-testcase-divider (arg)
  (unwind-protect
      (/ 100 arg%%%)%%%
      (format "unwinding! %s" arg%%%)%%%)%%%)

(should (eq 0 (testcover-testcase-unwinder 0)))
(should (eq 1 (testcover-testcase-divider 100)))

;; ==== compose-functions ====
"Some functions are 1value if all their arguments are 1value."
;; ====
(defconst testcover-testcase-count 3)
(defun testcover-testcase-number ()
  (+ 1 testcover-testcase-count))
(defun testcover-testcase-more ()
  (+ 1 (testcover-testcase-number) testcover-testcase-count))

(should (equal (testcover-testcase-more) 8))

;; ==== apply-quoted-symbol ====
"Apply with a quoted function symbol treated as 1value if function is."
;; ====
(defun testcover-testcase-numlist (flag)
  (if flag%%%
      '(1 2 3)
    '(4 5 6)))
(defun testcover-testcase-sum (flag)
  (apply '+ (testcover-testcase-numlist flag%%%)))
(defun testcover-testcase-label ()
  (apply 'message "edebug uses: %s %s" (list 1 2)!!!)!!!)

(should (equal 6 (testcover-testcase-sum t)))

;; ==== backquote-1value-bug-24509 ====
"Commas within backquotes are recognized as non-1value."
;; ====
(defmacro testcover-testcase-lambda (&rest body)
  `(lambda () ,@body))

(defun testcover-testcase-example ()
  (let ((lambda-1 (testcover-testcase-lambda (format "lambda-%d" 1))%%%)
        (lambda-2 (testcover-testcase-lambda (format "lambda-%d" 2))%%%))
    (concat (funcall lambda-1%%%)%%% " "
            (funcall lambda-2%%%)%%%)%%%)%%%)

(defmacro testcover-testcase-message-symbol (name)
  `(message "%s" ',name))

(defun testcover-testcase-example-2 ()
  (concat
   (testcover-testcase-message-symbol foo)%%%
   (testcover-testcase-message-symbol bar)%%%)%%%)

(should (equal "lambda-1 lambda-2" (testcover-testcase-example)))
(should (equal "foobar" (testcover-testcase-example-2)))

;; ==== pcase-bug-24688 ====
"Testcover copes with condition-case within backquoted list."
;; ====
(defun testcover-testcase-pcase (form)
  (pcase form%%%
    (`(condition-case ,var ,protected-form . ,handlers)
     (list var%%% protected-form%%% handlers%%%)%%%)
    (_ nil))%%%)

(should (equal (testcover-testcase-pcase '(condition-case a
                                              (/ 5 a)
                                            (error 0)))
               '(a (/ 5 a) ((error 0)))))

;; ==== defun-in-backquote-bug-11307-and-24743 ====
"Testcover handles defun forms within backquoted list."
;; ====
(defmacro testcover-testcase-defun (name &rest body)
  (declare (debug (symbolp def-body)))
  `(defun ,name () ,@body))

(testcover-testcase-defun foo (+ 1 2))
(testcover-testcase-defun bar (+ 3 4))
(should (eql (foo) 3))
(should (eql (bar) 7))

;; ==== closure-1value-bug ====
"Testcover does not mark closures as 1value."
;; ====
;; -*- lexical-binding:t -*-
(setq testcover-testcase-foo nil)
(setq testcover-testcase-bar 0)

(defun testcover-testcase-baz (arg)
  (setq testcover-testcase-foo
        (lambda () (+ arg testcover-testcase-bar%%%))))

(testcover-testcase-baz 2)
(should (equal 2 (funcall testcover-testcase-foo)))
(testcover-testcase-baz 3)
(should (equal 3 (funcall testcover-testcase-foo)))

;; ==== by-value-vs-by-reference-bug-25351 ====
"An object created by a 1value expression may be modified by other code."
;; ====
(defun testcover-testcase-ab ()
  (list 'a 'b))
(defun testcover-testcase-change-it (arg)
  (setf (cadr arg%%%)%%% 'c)%%%
  arg%%%)

(should (equal (testcover-testcase-change-it (testcover-testcase-ab)) '(a c)))
(should (equal (testcover-testcase-ab) '(a b)))

;; ==== 1value-error-test ====
"Forms wrapped by `1value' should always return the same value."
;; ====
(defun testcover-testcase-thing (arg)
  (1value (list 1 arg 3)))

(should (equal '(1 2 3) (testcover-testcase-thing 2)))
(should-error (testcover-testcase-thing 3))

;; ==== dotted-backquote ====
"Testcover can analyze code inside dotted backquoted lists."
;; ====
(defun testcover-testcase-dotted-bq (flag extras)
  (let* ((bq
          `(a b c . ,(and flag extras%%%))))
    bq))

(should (equal '(a b c) (testcover-testcase-dotted-bq nil '(d e))))
(should (equal '(a b c d e) (testcover-testcase-dotted-bq t '(d e))))

;; ==== quoted-backquote ====
"Testcover correctly handles the quoted backquote symbol."
;; ====
(defun testcover-testcase-special-symbols ()
  (list '\` '\, '\,@))

(should (equal '(\` \, \,@) (testcover-testcase-special-symbols)))

;; ==== backquoted-vector-bug-25316 ====
"Testcover can analyze code within backquoted vectors."
;; ====
(defun testcover-testcase-vec (a b c)
  `[,a%%% ,(list b%%% c%%%)%%%]%%%)

(defun testcover-testcase-vec-in-list (d e f)
  `([[,d%%% ,e%%%] ,f%%%])%%%)

(defun testcover-testcase-vec-arg (num)
  (list `[,num%%%]%%%)%%%)

(should (equal [1 (2 3)] (testcover-testcase-vec 1 2 3)))
(should (equal '([[4 5] 6]) (testcover-testcase-vec-in-list 4 5 6)))
(should (equal '([100]) (testcover-testcase-vec-arg 100)))

;; ==== dotted-list-in-vector-bug-30909 ====
"Testcover can analyze dotted pairs within vectors."
;; ====
(defun testcover-testcase-vectors-with-dotted-pairs ()
  (equal [(1 . "x")] [(1 2 . "y")])%%%)
(should-not (testcover-testcase-vectors-with-dotted-pairs))

;; ==== vector-in-macro-spec-bug-25316 ====
"Testcover can analyze code inside vectors."
;; ====
(defmacro testcover-testcase-nth-case (arg vec)
  (declare (indent 1)
           (debug (form (vector &rest form))))
  `(eval (aref ,vec%%% ,arg%%%))%%%)

(defun testcover-testcase-use-nth-case (choice val)
  (testcover-testcase-nth-case choice
                               [(+ 1 val!!!)!!!
                                (- 1 val%%%)%%%
                                (* 7 val)
                                (/ 4 val!!!)!!!]))

(should (eql 42 (testcover-testcase-use-nth-case 2 6)))
(should (eql 49 (testcover-testcase-use-nth-case 2 7)))
(should (eql 0 (testcover-testcase-use-nth-case 1 1 )))

;; ==== mapcar-is-not-compose ====
"Mapcar with 1value arguments is not 1value."
;; ====
(defvar testcover-testcase-num 0)
(defun testcover-testcase-add-num (n)
  (+ testcover-testcase-num n))
(defun testcover-testcase-mapcar-sides ()
  (mapcar 'testcover-testcase-add-num '(1 2 3)))

(setq testcover-testcase-num 1)
(should (equal (testcover-testcase-mapcar-sides) '(2 3 4)))
(setq testcover-testcase-num 2)
(should (equal (testcover-testcase-mapcar-sides) '(3 4 5)))

;; ==== function-with-edebug-spec-bug-25316 ====
"Functions can have edebug specs too.
See `c-make-font-lock-search-function' for an example in the
Emacs sources. `c-make-font-lock-search-function''s Edebug spec
also contains a quote.  See comment in `testcover-analyze-coverage'
regarding the odd-looking coverage result for the quoted form."
;; ====
(defun testcover-testcase-make-function (forms)
  `(lambda (flag) (if flag 0 ,@forms%%%))%%%)

(def-edebug-spec testcover-testcase-make-function
  (("quote" (&rest def-form))))

(defun testcover-testcase-thing ()
  (testcover-testcase-make-function '(!!!(+ 1 !!!(+ 2 !!!(+ 3 !!!(+ 4 5)%%%)%%%)%%%)%%%))%%%)

(defun testcover-testcase-use-thing ()
  (funcall (testcover-testcase-thing)%%% nil)%%%)

(should (equal (testcover-testcase-use-thing) 15))

;; ==== backquoted-dotted-alist ====
"Testcover can analyze a dotted alist constructed with backquote."
;; ====
(defun testcover-testcase-make-alist (expr entries)
  `((0 . ,expr%%%) . ,entries%%%)%%%)

(should (equal (testcover-testcase-make-alist "foo" '((1 . "bar") (2 . "baz")))
               '((0 . "foo") (1 . "bar") (2 . "baz"))))

;; ==== coverage-of-the-unknown-symbol-bug-25471 ====
"Testcover correctly records coverage of code which uses `unknown'"
;; ====
(defun testcover-testcase-how-do-i-know-you (name)
  (let ((val 'unknown))
    (when (equal name%%% "Bob")%%%
          (setq val 'known)!!!)
    val%%%)%%%)

(should (eq (testcover-testcase-how-do-i-know-you "Liz") 'unknown))

;; ==== circular-lists-bug-24402 ====
"Testcover captures and ignores circular list errors."
;; ====
(defun testcover-testcase-cyc1 (a)
  (let ((ls (make-list 10 a%%%)%%%))
    (nconc ls%%% ls%%%)
    ls)) ; The lack of a mark here is due to an ignored circular list error.
(testcover-testcase-cyc1 1)
(testcover-testcase-cyc1 1)
(defun testcover-testcase-cyc2 (a b)
  (let ((ls1 (make-list 10 a%%%)%%%)
        (ls2 (make-list 10 b)))
    (nconc ls2 ls2)
    (nconc ls1%%% ls2)
    ls1))
(testcover-testcase-cyc2 1 2)
(testcover-testcase-cyc2 1 4)

;; testcases.el ends here.
