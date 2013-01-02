;;; ert-tests.el --- ERT's self-tests  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2008, 2010-2013 Free Software Foundation, Inc.

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
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of ERT, the Emacs Lisp Regression Testing tool.
;; See ert.el or the texinfo manual for more details.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
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

(ert-deftest ert-test-deftest ()
  (should (equal (macroexpand '(ert-deftest abc () "foo" :tags '(bar)))
                 '(progn
                    (ert-set-test 'abc
                                  (make-ert-test :name 'abc
                                                 :documentation "foo"
                                                 :tags '(bar)
                                                 :body (lambda ())))
                    (push '(ert-deftest . abc) current-load-list)
                    'abc)))
  (should (equal (macroexpand '(ert-deftest def ()
                                 :expected-result ':passed))
                 '(progn
                    (ert-set-test 'def
                                  (make-ert-test :name 'def
                                                 :expected-result-type ':passed
                                                 :body (lambda ())))
                    (push '(ert-deftest . def) current-load-list)
                    'def)))
  ;; :documentation keyword is forbidden
  (should-error (macroexpand '(ert-deftest ghi ()
                                :documentation "foo"))))

(ert-deftest ert-test-record-backtrace ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "foo")))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result))
      (with-temp-buffer
        (ert--print-backtrace (ert-test-failed-backtrace result))
        (goto-char (point-min))
        (end-of-line)
        (let ((first-line (buffer-substring-no-properties (point-min) (point))))
          (should (equal first-line "  signal(ert-test-failed (\"foo\"))")))))))

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
                                                       "failure message")))))
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
                 `(member ,passing-test ,failing-test) buffer-name
                 mock-message-fn)
                (should (equal messages `(,(concat
                                            "Ran 2 tests, 1 results were "
                                            "as expected, 1 unexpected"))))
                (with-current-buffer buffer-name
                  (goto-char (point-min))
                  (should (equal
                           (buffer-substring (point-min)
                                             (save-excursion
                                               (forward-line 4)
                                               (point)))
                           (concat
                            "Selector: (member <passing-test> <failing-test>)\n"
                            "Passed: 1\n"
                            "Failed: 1 (1 unexpected)\n"
                            "Total:  2/2\n")))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))))))))

(ert-deftest ert-test-special-operator-p ()
  (should (ert--special-operator-p 'if))
  (should-not (ert--special-operator-p 'car))
  (should-not (ert--special-operator-p 'ert--special-operator-p))
  (let ((b (ert--gensym)))
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

(ert-deftest ert-test-remprop ()
  (let ((x (ert--gensym)))
    (should (equal (symbol-plist x) '()))
    ;; Remove nonexistent property on empty plist.
    (ert--remprop x 'b)
    (should (equal (symbol-plist x) '()))
    (put x 'a 1)
    (should (equal (symbol-plist x) '(a 1)))
    ;; Remove nonexistent property on nonempty plist.
    (ert--remprop x 'b)
    (should (equal (symbol-plist x) '(a 1)))
    (put x 'b 2)
    (put x 'c 3)
    (put x 'd 4)
    (should (equal (symbol-plist x) '(a 1 b 2 c 3 d 4)))
    ;; Remove property that is neither first nor last.
    (ert--remprop x 'c)
    (should (equal (symbol-plist x) '(a 1 b 2 d 4)))
    ;; Remove last property from a plist of length >1.
    (ert--remprop x 'd)
    (should (equal (symbol-plist x) '(a 1 b 2)))
    ;; Remove first property from a plist of length >1.
    (ert--remprop x 'a)
    (should (equal (symbol-plist x) '(b 2)))
    ;; Remove property when there is only one.
    (ert--remprop x 'b)
    (should (equal (symbol-plist x) '()))))

(ert-deftest ert-test-remove-if-not ()
  (let ((list (list 'a 'b 'c 'd))
        (i 0))
    (let ((result (ert--remove-if-not (lambda (x)
                                        (should (eql x (nth i list)))
                                        (cl-incf i)
                                        (member i '(2 3)))
                                      list)))
      (should (equal i 4))
      (should (equal result '(b c)))
      (should (equal list '(a b c d)))))
  (should (equal '()
                 (ert--remove-if-not (lambda (_x) (should nil)) '()))))

(ert-deftest ert-test-remove* ()
  (let ((list (list 'a 'b 'c 'd))
        (key-index 0)
        (test-index 0))
    (let ((result
           (ert--remove* 'foo list
                         :key (lambda (x)
                                (should (eql x (nth key-index list)))
                                (prog1
                                    (list key-index x)
                                  (cl-incf key-index)))
                         :test
                         (lambda (a b)
                           (should (eql a 'foo))
                           (should (equal b (list test-index
                                                  (nth test-index list))))
                           (cl-incf test-index)
                           (member test-index '(2 3))))))
      (should (equal key-index 4))
      (should (equal test-index 4))
      (should (equal result '(a d)))
      (should (equal list '(a b c d)))))
  (let ((x (cons nil nil))
        (y (cons nil nil)))
    (should (equal (ert--remove* x (list x y))
                   ;; or (list x), since we use `equal' -- the
                   ;; important thing is that only one element got
                   ;; removed, this proves that the default test is
                   ;; `eql', not `equal'
                   (list y)))))


(ert-deftest ert-test-set-functions ()
  (let ((c1 (cons nil nil))
        (c2 (cons nil nil))
        (sym (make-symbol "a")))
    (let ((e '())
          (a (list 'a 'b sym nil "" "x" c1 c2))
          (b (list c1 'y 'b sym 'x)))
      (should (equal (ert--set-difference e e) e))
      (should (equal (ert--set-difference a e) a))
      (should (equal (ert--set-difference e a) e))
      (should (equal (ert--set-difference a a) e))
      (should (equal (ert--set-difference b e) b))
      (should (equal (ert--set-difference e b) e))
      (should (equal (ert--set-difference b b) e))
      (should (equal (ert--set-difference a b) (list 'a nil "" "x" c2)))
      (should (equal (ert--set-difference b a) (list 'y 'x)))

      ;; We aren't testing whether this is really using `eq' rather than `eql'.
      (should (equal (ert--set-difference-eq e e) e))
      (should (equal (ert--set-difference-eq a e) a))
      (should (equal (ert--set-difference-eq e a) e))
      (should (equal (ert--set-difference-eq a a) e))
      (should (equal (ert--set-difference-eq b e) b))
      (should (equal (ert--set-difference-eq e b) e))
      (should (equal (ert--set-difference-eq b b) e))
      (should (equal (ert--set-difference-eq a b) (list 'a nil "" "x" c2)))
      (should (equal (ert--set-difference-eq b a) (list 'y 'x)))

      (should (equal (ert--union e e) e))
      (should (equal (ert--union a e) a))
      (should (equal (ert--union e a) a))
      (should (equal (ert--union a a) a))
      (should (equal (ert--union b e) b))
      (should (equal (ert--union e b) b))
      (should (equal (ert--union b b) b))
      (should (equal (ert--union a b) (list 'a 'b sym nil "" "x" c1 c2 'y 'x)))
      (should (equal (ert--union b a) (list c1 'y 'b sym 'x 'a nil "" "x" c2)))

      (should (equal (ert--intersection e e) e))
      (should (equal (ert--intersection a e) e))
      (should (equal (ert--intersection e a) e))
      (should (equal (ert--intersection a a) a))
      (should (equal (ert--intersection b e) e))
      (should (equal (ert--intersection e b) e))
      (should (equal (ert--intersection b b) b))
      (should (equal (ert--intersection a b) (list 'b sym c1)))
      (should (equal (ert--intersection b a) (list c1 'b sym))))))

(ert-deftest ert-test-gensym ()
  ;; Since the expansion of `should' calls `ert--gensym' and thus has a
  ;; side-effect on `ert--gensym-counter', we have to make sure all
  ;; macros in our test body are expanded before we rebind
  ;; `ert--gensym-counter' and run the body.  Otherwise, the test would
  ;; fail if run interpreted.
  (let ((body (byte-compile
               '(lambda ()
                  (should (equal (symbol-name (ert--gensym)) "G0"))
                  (should (equal (symbol-name (ert--gensym)) "G1"))
                  (should (equal (symbol-name (ert--gensym)) "G2"))
                  (should (equal (symbol-name (ert--gensym "foo")) "foo3"))
                  (should (equal (symbol-name (ert--gensym "bar")) "bar4"))
                  (should (equal ert--gensym-counter 5))))))
    (let ((ert--gensym-counter 0))
      (funcall body))))

(ert-deftest ert-test-coerce-to-vector ()
  (let* ((a (vector))
         (b (vector 1 a 3))
         (c (list))
         (d (list b a)))
    (should (eql (ert--coerce-to-vector a) a))
    (should (eql (ert--coerce-to-vector b) b))
    (should (equal (ert--coerce-to-vector c) (vector)))
    (should (equal (ert--coerce-to-vector d) (vector b a)))))

(ert-deftest ert-test-string-position ()
  (should (eql (ert--string-position ?x "") nil))
  (should (eql (ert--string-position ?a "abc") 0))
  (should (eql (ert--string-position ?b "abc") 1))
  (should (eql (ert--string-position ?c "abc") 2))
  (should (eql (ert--string-position ?d "abc") nil))
  (should (eql (ert--string-position ?A "abc") nil)))

(ert-deftest ert-test-mismatch ()
  (should (eql (ert--mismatch "" "") nil))
  (should (eql (ert--mismatch "" "a") 0))
  (should (eql (ert--mismatch "a" "a") nil))
  (should (eql (ert--mismatch "ab" "a") 1))
  (should (eql (ert--mismatch "Aa" "aA") 0))
  (should (eql (ert--mismatch '(a b c) '(a b d)) 2)))

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
                                       :infos nil)))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 0 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (ert--stats-set-test-and-result stats 0 test-1 (make-ert-test-passed))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 1 (ert-stats-completed stats)))
    (should (eql 1 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (ert--stats-set-test-and-result stats 0 test-1 failed)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 1 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 1 (ert-stats-completed-unexpected stats)))
    (ert--stats-set-test-and-result stats 0 test-1 nil)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 0 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))
    (ert--stats-set-test-and-result stats 0 test-3 failed)
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 1 (ert-stats-completed stats)))
    (should (eql 0 (ert-stats-completed-expected stats)))
    (should (eql 1 (ert-stats-completed-unexpected stats)))
    (ert--stats-set-test-and-result stats 1 test-2 (make-ert-test-passed))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 2 (ert-stats-completed stats)))
    (should (eql 1 (ert-stats-completed-expected stats)))
    (should (eql 1 (ert-stats-completed-unexpected stats)))
    (ert--stats-set-test-and-result stats 0 test-1 (make-ert-test-passed))
    (should (eql 2 (ert-stats-total stats)))
    (should (eql 2 (ert-stats-completed stats)))
    (should (eql 2 (ert-stats-completed-expected stats)))
    (should (eql 0 (ert-stats-completed-unexpected stats)))))


(provide 'ert-tests)

;;; ert-tests.el ends here
