;;; ert-tests.el --- ERT's self-tests  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2008, 2010-2017 Free Software Foundation, Inc.

;; Author: Christian Ohler <ohler@gnu.org>

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

;; This file is part of ERT, the Emacs Lisp Regression Testing tool.
;; See ert.el or the texinfo manual for more details.

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; Self-test that doesn't rely on ERT, for bootstrapping.

;; This is used to test that bodies actually run.
(defvar ert--test-body-was-run)
(ert-deftest ert-test-body-runs ()
  (setq ert--test-body-was-run t))

(defun ert-self-test ()
  "Run ERT's self-tests and make sure they actually ran."
  (let ((window-configuration (current-window-configuration)))
    (let ((ert--test-body-was-run nil))
      ;; The buffer name chosen here should not compete with the default
      ;; results buffer name for completion in `switch-to-buffer'.
      (let ((stats (ert-run-tests-interactively "^ert-" " *ert self-tests*")))
        (cl-assert ert--test-body-was-run)
        (if (zerop (ert-stats-completed-unexpected stats))
            ;; Hide results window only when everything went well.
            (set-window-configuration window-configuration)
          (error "ERT self-test failed"))))))

(defun ert-self-test-and-exit ()
  "Run ERT's self-tests and exit Emacs.

The exit code will be zero if the tests passed, nonzero if they
failed or if there was a problem."
  (unwind-protect
      (progn
        (ert-self-test)
        (kill-emacs 0))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 1))))


;;; Further tests are defined using ERT.

(ert-deftest ert-test-nested-test-body-runs ()
  "Test that nested test bodies run."
  (let ((was-run nil))
    (let ((test (make-ert-test :body (lambda ()
                                       (setq was-run t)))))
      (cl-assert (not was-run))
      (ert-run-test test)
      (cl-assert was-run))))


;;; Test that pass/fail works.
(ert-deftest ert-test-pass ()
  (let ((test (make-ert-test :body (lambda ()))))
    (let ((result (ert-run-test test)))
      (cl-assert (ert-test-passed-p result)))))

(ert-deftest ert-test-fail ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (cl-assert (ert-test-failed-p result) t)
      (cl-assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed "failure message"))
              t))))

(ert-deftest ert-test-fail-debug-with-condition-case ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (cl-assert nil))
      ((error)
       (cl-assert (equal condition '(ert-test-failed "failure message")) t)))))

(ert-deftest ert-test-fail-debug-with-debugger-1 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((debugger (lambda (&rest _args)
                      (cl-assert nil))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test)))))

(ert-deftest ert-test-fail-debug-with-debugger-2 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (cl-block nil
      (let ((debugger (lambda (&rest _args)
                        (cl-return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (cl-assert nil)))))

(ert-deftest ert-test-fail-debug-nested-with-debugger ()
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error t))
                                       (ert-fail "failure message"))))))
    (let ((debugger (lambda (&rest _args)
                      (cl-assert nil nil "Assertion a"))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error nil))
                                       (ert-fail "failure message"))))))
    (cl-block nil
      (let ((debugger (lambda (&rest _args)
                        (cl-return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (cl-assert nil nil "Assertion b")))))

(ert-deftest ert-test-error ()
  (let ((test (make-ert-test :body (lambda () (error "Error message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (cl-assert (ert-test-failed-p result) t)
      (cl-assert (equal (ert-test-result-with-condition-condition result)
                     '(error "Error message"))
              t))))

(ert-deftest ert-test-error-debug ()
  (let ((test (make-ert-test :body (lambda () (error "Error message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (cl-assert nil))
      ((error)
       (cl-assert (equal condition '(error "Error message")) t)))))


;;; Test that `should' works.
(ert-deftest ert-test-should ()
  (let ((test (make-ert-test :body (lambda () (should nil)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (cl-assert (ert-test-failed-p result) t)
      (cl-assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((should nil) :form nil :value nil)))
              t)))
  (let ((test (make-ert-test :body (lambda () (should t)))))
    (let ((result (ert-run-test test)))
      (cl-assert (ert-test-passed-p result) t))))

(ert-deftest ert-test-should-value ()
  (should (eql (should 'foo) 'foo))
  (should (eql (should 'bar) 'bar)))

(ert-deftest ert-test-should-not ()
  (let ((test (make-ert-test :body (lambda () (should-not t)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (cl-assert (ert-test-failed-p result) t)
      (cl-assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((should-not t) :form t :value t)))
              t)))
  (let ((test (make-ert-test :body (lambda () (should-not nil)))))
    (let ((result (ert-run-test test)))
      (cl-assert (ert-test-passed-p result)))))


(ert-deftest ert-test-should-with-macrolet ()
  (let ((test (make-ert-test :body (lambda ()
                                     (cl-macrolet ((foo () `(progn t nil)))
                                       (should (foo)))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (ert-test-failed-p result))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed ((should (foo))
                                  :form (progn t nil)
                                  :value nil)))))))

(ert-deftest ert-test-should-error ()
  ;; No error.
  (let ((test (make-ert-test :body (lambda () (should-error (progn))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (ert-test-failed-p result))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (progn))
                        :form (progn)
                        :value nil
                        :fail-reason "did not signal an error"))))))
  ;; A simple error.
  (should (equal (should-error (error "Foo"))
                 '(error "Foo")))
  ;; Error of unexpected type.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (error "Foo")
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (error "Foo") :type 'singularity-error)
                  :form (error "Foo")
                  :condition (error "Foo")
                  :fail-reason
                  "the error signaled did not have the expected type"))))))
  ;; Error of the expected type.
  (let* ((error nil)
         (test (make-ert-test
                :body (lambda ()
                        (setq error
                              (should-error (signal 'singularity-error nil)
                                            :type 'singularity-error))))))
    (let ((result (ert-run-test test)))
      (should (ert-test-passed-p result))
      (should (equal error '(singularity-error))))))

(ert-deftest ert-test-should-error-subtypes ()
  (should-error (signal 'singularity-error nil)
                :type 'singularity-error
                :exclude-subtypes t)
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'arith-error nil)
                                     :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'arith-error nil)
                                :type 'singularity-error)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signaled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'arith-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'arith-error nil)
                                :type 'singularity-error
                                :exclude-subtypes t)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signaled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'singularity-error nil)
                                     :type 'arith-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'singularity-error nil)
                                :type 'arith-error
                                :exclude-subtypes t)
                  :form (signal singularity-error nil)
                  :condition (singularity-error)
                  :fail-reason
                  "the error signaled was a subtype of the expected type")))))
    ))

(ert-deftest ert-test-should-error-argument ()
  "Errors due to evaluating arguments should not break tests."
  (should-error (identity (/ 1 0))))

(ert-deftest ert-test-should-error-macroexpansion ()
  "Errors due to expanding macros should not break tests."
  (cl-macrolet ((test () (error "Foo")))
    (should-error (test))))

(ert-deftest ert-test-skip-unless ()
  ;; Don't skip.
  (let ((test (make-ert-test :body (lambda () (skip-unless t)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-passed-p result))))
  ;; Skip.
  (let ((test (make-ert-test :body (lambda () (skip-unless nil)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-skipped-p result))))
  ;; Skip in case of error.
  (let ((test (make-ert-test :body (lambda () (skip-unless (error "Foo"))))))
    (let ((result (ert-run-test test)))
      (should (ert-test-skipped-p result)))))

(defmacro ert--test-my-list (&rest args)
  "Don't use this.  Instead, call `list' with ARGS, it does the same thing.

This macro is used to test if macroexpansion in `should' works."
  `(list ,@args))

(ert-deftest ert-test-should-failure-debugging ()
  "Test that `should' errors contain the information we expect them to."
  (cl-loop
   for (body expected-condition) in
   `((,(lambda () (let ((x nil)) (should x)))
      (ert-test-failed ((should x) :form x :value nil)))
     (,(lambda () (let ((x t)) (should-not x)))
      (ert-test-failed ((should-not x) :form x :value t)))
     (,(lambda () (let ((x t)) (should (not x))))
      (ert-test-failed ((should (not x)) :form (not t) :value nil)))
     (,(lambda () (let ((x nil)) (should-not (not x))))
      (ert-test-failed ((should-not (not x)) :form (not nil) :value t)))
     (,(lambda () (let ((x t) (y nil)) (should-not
                                   (ert--test-my-list x y))))
      (ert-test-failed
       ((should-not (ert--test-my-list x y))
        :form (list t nil)
        :value (t nil))))
     (,(lambda () (let ((_x t)) (should (error "Foo"))))
      (error "Foo")))
   do
   (let ((test (make-ert-test :body body)))
     (condition-case actual-condition
         (progn
           (let ((ert-debug-on-error t))
             (ert-run-test test))
           (cl-assert nil))
       ((error)
        (should (equal actual-condition expected-condition)))))))

(defun ert-test--which-file ()
  "Dummy function to help test `symbol-file' for tests.")

(ert-deftest ert-test-deftest ()
  (ert-deftest ert-test-abc () "foo" :tags '(bar))
  (let ((abc (ert-get-test 'ert-test-abc)))
    (should (equal (ert-test-tags abc) '(bar)))
    (should (equal (ert-test-documentation abc) "foo")))
  (should (equal (symbol-file 'ert-test-deftest 'ert--test)
                 (symbol-file 'ert-test--which-file 'defun)))

  (ert-deftest ert-test-def () :expected-result ':passed)
  (let ((def (ert-get-test 'ert-test-def)))
    (should (equal (ert-test-expected-result-type def) :passed)))
  ;; :documentation keyword is forbidden
  (should-error (macroexpand '(ert-deftest ghi ()
                                :documentation "foo"))))

(ert-deftest ert-test-record-backtrace ()
  (let* ((test-body (lambda () (ert-fail "foo")))
         (test (make-ert-test :body test-body))
         (result (ert-run-test test)))
    (should (ert-test-failed-p result))
    (should (eq (nth 1 (car (ert-test-failed-backtrace result)))
                'signal))))

(ert-deftest ert-test-messages ()
  :tags '(:causes-redisplay)
  (let* ((message-string "Test message")
         (messages-buffer (get-buffer-create "*Messages*"))
         (test (make-ert-test :body (lambda () (message "%s" message-string)))))
    (with-current-buffer messages-buffer
      (let ((result (ert-run-test test)))
        (should (equal (concat message-string "\n")
                       (ert-test-result-messages result)))))))

(ert-deftest ert-test-running-tests ()
  (let ((outer-test (ert-get-test 'ert-test-running-tests)))
    (should (equal (ert-running-test) outer-test))
    (let (test1 test2 test3)
      (setq test1 (make-ert-test
                   :name "1"
                   :body (lambda ()
                           (should (equal (ert-running-test) outer-test))
                           (should (equal ert--running-tests
                                          (list test1 test2 test3
                                                outer-test)))))
            test2 (make-ert-test
                   :name "2"
                   :body (lambda ()
                           (should (equal (ert-running-test) outer-test))
                           (should (equal ert--running-tests
                                          (list test3 test2 outer-test)))
                           (ert-run-test test1)))
            test3 (make-ert-test
                   :name "3"
                   :body (lambda ()
                           (should (equal (ert-running-test) outer-test))
                           (should (equal ert--running-tests
                                          (list test3 outer-test)))
                           (ert-run-test test2))))
      (should (ert-test-passed-p (ert-run-test test3))))))

(ert-deftest ert-test-test-result-expected-p ()
  "Test `ert-test-result-expected-p' and (implicitly) `ert-test-result-type-p'."
  ;; passing test
  (let ((test (make-ert-test :body (lambda ()))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  ;; unexpected failure
  (let ((test (make-ert-test :body (lambda () (ert-fail "failed")))))
    (should-not (ert-test-result-expected-p test (ert-run-test test))))
  ;; expected failure
  (let ((test (make-ert-test :body (lambda () (ert-fail "failed"))
                             :expected-result-type ':failed)))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  ;; `not' expected type
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(not :failed))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(not :passed))))
    (should-not (ert-test-result-expected-p test (ert-run-test test))))
  ;; `and' expected type
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(and :passed :failed))))
    (should-not (ert-test-result-expected-p test (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(and :passed
                                                         (not :failed)))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  ;; `or' expected type
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(or (and :passed :failed)
                                                        :passed))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(or (and :passed :failed)
                                                        nil (not t)))))
    (should-not (ert-test-result-expected-p test (ert-run-test test)))))

;;; Test `ert-select-tests'.
(ert-deftest ert-test-select-regexp ()
  (should (equal (ert-select-tests "^ert-test-select-regexp$" t)
                 (list (ert-get-test 'ert-test-select-regexp)))))

(ert-deftest ert-test-test-boundp ()
  (should (ert-test-boundp 'ert-test-test-boundp))
  (should-not (ert-test-boundp (make-symbol "ert-not-a-test"))))

(ert-deftest ert-test-select-member ()
  (should (equal (ert-select-tests '(member ert-test-select-member) t)
                 (list (ert-get-test 'ert-test-select-member)))))

(ert-deftest ert-test-select-test ()
  (should (equal (ert-select-tests (ert-get-test 'ert-test-select-test) t)
                 (list (ert-get-test 'ert-test-select-test)))))

(ert-deftest ert-test-select-symbol ()
  (should (equal (ert-select-tests 'ert-test-select-symbol t)
                 (list (ert-get-test 'ert-test-select-symbol)))))

(ert-deftest ert-test-select-and ()
  (let ((test (make-ert-test
               :name nil
               :body nil
               :most-recent-result (make-ert-test-failed
                                    :condition nil
                                    :backtrace nil
                                    :infos nil))))
    (should (equal (ert-select-tests `(and (member ,test) :failed) t)
                   (list test)))))

(ert-deftest ert-test-select-tag ()
  (let ((test (make-ert-test
               :name nil
               :body nil
               :tags '(a b))))
    (should (equal (ert-select-tests `(tag a) (list test)) (list test)))
    (should (equal (ert-select-tests `(tag b) (list test)) (list test)))
    (should (equal (ert-select-tests `(tag c) (list test)) '()))))


;;; Tests for utility functions.
(ert-deftest ert-test-proper-list-p ()
  (should (ert--proper-list-p '()))
  (should (ert--proper-list-p '(1)))
  (should (ert--proper-list-p '(1 2)))
  (should (ert--proper-list-p '(1 2 3)))
  (should (ert--proper-list-p '(1 2 3 4)))
  (should (not (ert--proper-list-p 'a)))
  (should (not (ert--proper-list-p '(1 . a))))
  (should (not (ert--proper-list-p '(1 2 . a))))
  (should (not (ert--proper-list-p '(1 2 3 . a))))
  (should (not (ert--proper-list-p '(1 2 3 4 . a))))
  (let ((a (list 1)))
    (setf (cdr (last a)) a)
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) a)
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) a)
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) a)
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cddr a))
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cddr a))
    (should (not (ert--proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cl-cdddr a))
    (should (not (ert--proper-list-p a)))))

(ert-deftest ert-test-parse-keys-and-body ()
  (should (equal (ert--parse-keys-and-body '(foo)) '(nil (foo))))
  (should (equal (ert--parse-keys-and-body '(:bar foo)) '((:bar foo) nil)))
  (should (equal (ert--parse-keys-and-body '(:bar foo a (b)))
                 '((:bar foo) (a (b)))))
  (should (equal (ert--parse-keys-and-body '(:bar foo :a (b)))
                 '((:bar foo :a (b)) nil)))
  (should (equal (ert--parse-keys-and-body '(bar foo :a (b)))
                 '(nil (bar foo :a (b)))))
  (should-error (ert--parse-keys-and-body '(:bar foo :a))))


(ert-deftest ert-test-run-tests-interactively ()
  :tags '(:causes-redisplay)
  (let ((passing-test (make-ert-test :name 'passing-test
                                     :body (lambda () (ert-pass))))
        (failing-test (make-ert-test :name 'failing-test
                                     :body (lambda () (ert-fail
                                                       "failure message"))))
        (skipped-test (make-ert-test :name 'skipped-test
                                     :body (lambda () (ert-skip
                                                       "skip message")))))
    (let ((ert-debug-on-error nil))
      (let* ((buffer-name (generate-new-buffer-name " *ert-test-run-tests*"))
             (messages nil)
             (mock-message-fn
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
        (save-window-excursion
          (unwind-protect
              (let ((case-fold-search nil))
                (ert-run-tests-interactively
                 `(member ,passing-test ,failing-test, skipped-test) buffer-name
                 mock-message-fn)
                (should (equal messages `(,(concat
                                            "Ran 3 tests, 1 results were "
                                            "as expected, 1 unexpected, "
					    "1 skipped"))))
                (with-current-buffer buffer-name
                  (goto-char (point-min))
                  (should (equal
                           (buffer-substring (point-min)
                                             (save-excursion
                                               (forward-line 5)
                                               (point)))
                           (concat
                            "Selector: (member <passing-test> <failing-test> "
			    "<skipped-test>)\n"
                            "Passed:  1\n"
                            "Failed:  1 (1 unexpected)\n"
			    "Skipped: 1\n"
                            "Total:   3/3\n")))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))))))))

(ert-deftest ert-test-special-operator-p ()
  (should (ert--special-operator-p 'if))
  (should-not (ert--special-operator-p 'car))
  (should-not (ert--special-operator-p 'ert--special-operator-p))
  (let ((b (cl-gensym)))
    (should-not (ert--special-operator-p b))
    (fset b 'if)
    (should (ert--special-operator-p b))))

(ert-deftest ert-test-list-of-should-forms ()
  (let ((test (make-ert-test :body (lambda ()
                                     (should t)
                                     (should (null '()))
                                     (should nil)
                                     (should t)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (equal (ert-test-result-should-forms result)
                     '(((should t) :form t :value t)
                       ((should (null '())) :form (null nil) :value t)
                       ((should nil) :form nil :value nil)))))))

(ert-deftest ert-test-list-of-should-forms-observers-should-not-stack ()
  (let ((test (make-ert-test
               :body (lambda ()
                       (let ((test2 (make-ert-test
                                     :body (lambda ()
                                             (should t)))))
                         (let ((result (ert-run-test test2)))
                           (should (ert-test-passed-p result))))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (ert-test-passed-p result))
      (should (eql (length (ert-test-result-should-forms result))
                   1)))))

(ert-deftest ert-test-list-of-should-forms-no-deep-copy ()
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((obj (list 'a)))
                                       (should (equal obj '(a)))
                                       (setf (car obj) 'b)
                                       (should (equal obj '(b))))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-should-forms result)
                     '(((should (equal obj '(a))) :form (equal (b) (a)) :value t
                        :explanation nil)
                       ((should (equal obj '(b))) :form (equal (b) (b)) :value t
                        :explanation nil)
                       ))))))

(ert-deftest ert-test-string-first-line ()
  (should (equal (ert--string-first-line "") ""))
  (should (equal (ert--string-first-line "abc") "abc"))
  (should (equal (ert--string-first-line "abc\n") "abc"))
  (should (equal (ert--string-first-line "foo\nbar") "foo"))
  (should (equal (ert--string-first-line " foo\nbar\nbaz\n") " foo")))

(ert-deftest ert-test-explain-equal ()
  (should (equal (ert--explain-equal nil 'foo)
                 '(different-atoms nil foo)))
  (should (equal (ert--explain-equal '(a a) '(a b))
                 '(list-elt 1 (different-atoms a b))))
  (should (equal (ert--explain-equal '(1 48) '(1 49))
                 '(list-elt 1 (different-atoms (48 "#x30" "?0")
                                               (49 "#x31" "?1")))))
  (should (equal (ert--explain-equal 'nil '(a))
                 '(different-types nil (a))))
  (should (equal (ert--explain-equal '(a b c) '(a b c d))
                 '(proper-lists-of-different-length 3 4 (a b c) (a b c d)
                                                    first-mismatch-at 3)))
  (let ((sym (make-symbol "a")))
    (should (equal (ert--explain-equal 'a sym)
                   `(different-symbols-with-the-same-name a ,sym)))))

(ert-deftest ert-test-explain-equal-improper-list ()
  (should (equal (ert--explain-equal '(a . b) '(a . c))
                 '(cdr (different-atoms b c)))))

(ert-deftest ert-test-explain-equal-keymaps ()
  ;; This used to be very slow.
  (should (equal (make-keymap) (make-keymap)))
  (should (equal (make-sparse-keymap) (make-sparse-keymap))))

(ert-deftest ert-test-significant-plist-keys ()
  (should (equal (ert--significant-plist-keys '()) '()))
  (should (equal (ert--significant-plist-keys '(a b c d e f c g p q r nil s t))
                 '(a c e p s))))

(ert-deftest ert-test-plist-difference-explanation ()
  (should (equal (ert--plist-difference-explanation
                  '(a b c nil) '(a b))
                 nil))
  (should (equal (ert--plist-difference-explanation
                  '(a b c t) '(a b))
                 '(different-properties-for-key c (different-atoms t nil))))
  (should (equal (ert--plist-difference-explanation
                  '(a b c t) '(c nil a b))
                 '(different-properties-for-key c (different-atoms t nil))))
  (should (equal (ert--plist-difference-explanation
                  '(a b c (foo . bar)) '(c (foo . baz) a b))
                 '(different-properties-for-key c
                                                (cdr
                                                 (different-atoms bar baz))))))

(ert-deftest ert-test-abbreviate-string ()
  (should (equal (ert--abbreviate-string "foo" 4 nil) "foo"))
  (should (equal (ert--abbreviate-string "foo" 3 nil) "foo"))
  (should (equal (ert--abbreviate-string "foo" 3 nil) "foo"))
  (should (equal (ert--abbreviate-string "foo" 2 nil) "fo"))
  (should (equal (ert--abbreviate-string "foo" 1 nil) "f"))
  (should (equal (ert--abbreviate-string "foo" 0 nil) ""))
  (should (equal (ert--abbreviate-string "bar" 4 t) "bar"))
  (should (equal (ert--abbreviate-string "bar" 3 t) "bar"))
  (should (equal (ert--abbreviate-string "bar" 3 t) "bar"))
  (should (equal (ert--abbreviate-string "bar" 2 t) "ar"))
  (should (equal (ert--abbreviate-string "bar" 1 t) "r"))
  (should (equal (ert--abbreviate-string "bar" 0 t) "")))

(ert-deftest ert-test-explain-equal-string-properties ()
  (should
   (equal (ert--explain-equal-including-properties #("foo" 0 1 (a b))
                                                   "foo")
          '(char 0 "f"
                 (different-properties-for-key a (different-atoms b nil))
                 context-before ""
                 context-after "oo")))
  (should (equal (ert--explain-equal-including-properties
                  #("foo" 1 3 (a b))
                  #("goo" 0 1 (c d)))
                 '(array-elt 0 (different-atoms (?f "#x66" "?f")
                                                (?g "#x67" "?g")))))
  (should
   (equal (ert--explain-equal-including-properties
           #("foo" 0 1 (a b c d) 1 3 (a b))
           #("foo" 0 1 (c d a b) 1 2 (a foo)))
          '(char 1 "o" (different-properties-for-key a (different-atoms b foo))
                 context-before "f" context-after "o"))))

(ert-deftest ert-test-equal-including-properties ()
  (should (equal-including-properties "foo" "foo"))
  (should (ert-equal-including-properties "foo" "foo"))

  (should (equal-including-properties #("foo" 0 3 (a b))
                                      (propertize "foo" 'a 'b)))
  (should (ert-equal-including-properties #("foo" 0 3 (a b))
                                          (propertize "foo" 'a 'b)))

  (should (equal-including-properties #("foo" 0 3 (a b c d))
                                      (propertize "foo" 'a 'b 'c 'd)))
  (should (ert-equal-including-properties #("foo" 0 3 (a b c d))
                                          (propertize "foo" 'a 'b 'c 'd)))

  (should-not (equal-including-properties #("foo" 0 3 (a b c e))
                                          (propertize "foo" 'a 'b 'c 'd)))
  (should-not (ert-equal-including-properties #("foo" 0 3 (a b c e))
                                              (propertize "foo" 'a 'b 'c 'd)))

  ;; This is bug 6581.
  (should-not (equal-including-properties #("foo" 0 3 (a (t)))
                                          (propertize "foo" 'a (list t))))
  (should (ert-equal-including-properties #("foo" 0 3 (a (t)))
                                          (propertize "foo" 'a (list t)))))

(ert-deftest ert-test-stats-set-test-and-result ()
  (let* ((test-1 (make-ert-test :name 'test-1
                                :body (lambda () nil)))
         (test-2 (make-ert-test :name 'test-2
                                :body (lambda () nil)))
         (test-3 (make-ert-test :name 'test-2
                                :body (lambda () nil)))
         (stats (ert--make-stats (list test-1 test-2) 't))
         (failed (make-ert-test-failed :condition nil
                                       :backtrace nil
                                       :infos nil))
         (skipped (make-ert-test-skipped :condition nil
					 :backtrace nil
					 :infos nil)))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 0 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 0 test-1 (make-ert-test-passed))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 1 (ert-stats-completed stats)))
    (should (eql 1 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 0 test-1 failed)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 1 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 1 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 0 test-1 nil)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 0 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 0 test-3 failed)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 1 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 1 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 1 test-2 (make-ert-test-passed))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 2 (ert-stats-completed stats)))
    (should (eql 1 (ert-stats-completed-expected stats)))
    (should (eql 1 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 0 test-1 (make-ert-test-passed))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 2 (ert-stats-completed stats)))
    (should (eql 2 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (should (eql 0 (ert-stats-skipped stats)))
    (ert--stats-set-test-and-result stats 0 test-1 skipped)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 2 (ert-stats-completed stats)))
    (should (eql 1 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (should (eql 1 (ert-stats-skipped stats)))))


(provide 'ert-tests)

;;; ert-tests.el ends here
