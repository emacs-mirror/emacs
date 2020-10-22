;;; cconv-tests.el -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

(ert-deftest cconv-tests-lambda-:documentation ()
  "Docstring for lambda can be specified with :documentation."
  (let ((fun (lambda ()
               (:documentation (concat "lambda" " documentation"))
               'lambda-result)))
    (should (string= (documentation fun) "lambda documentation"))
    (should (eq (funcall fun) 'lambda-result))))

(ert-deftest cconv-tests-pcase-lambda-:documentation ()
  "Docstring for pcase-lambda can be specified with :documentation."
  (let ((fun (pcase-lambda (`(,a ,b))
               (:documentation (concat "pcase-lambda" " documentation"))
               (list b a))))
    (should (string= (documentation fun) "pcase-lambda documentation"))
    (should (equal '(2 1) (funcall fun '(1 2))))))

(defun cconv-tests-defun ()
  (:documentation (concat "defun" " documentation"))
  'defun-result)
(ert-deftest cconv-tests-defun-:documentation ()
  "Docstring for defun can be specified with :documentation."
  (should (string= (documentation 'cconv-tests-defun)
                   "defun documentation"))
  (should (eq (cconv-tests-defun) 'defun-result)))

(cl-defun cconv-tests-cl-defun ()
  (:documentation (concat "cl-defun" " documentation"))
  'cl-defun-result)
(ert-deftest cconv-tests-cl-defun-:documentation ()
  "Docstring for cl-defun can be specified with :documentation."
  (should (string= (documentation 'cconv-tests-cl-defun)
                   "cl-defun documentation"))
  (should (eq (cconv-tests-cl-defun) 'cl-defun-result)))

;; FIXME: The byte-complier croaks on this.  See Bug#28557.
;; (defmacro cconv-tests-defmacro ()
;;   (:documentation (concat "defmacro" " documentation"))
;;   '(quote defmacro-result))
;; (ert-deftest cconv-tests-defmacro-:documentation ()
;;   "Docstring for defmacro can be specified with :documentation."
;;   (should (string= (documentation 'cconv-tests-defmacro)
;;                    "defmacro documentation"))
;;   (should (eq (cconv-tests-defmacro) 'defmacro-result)))

;; FIXME: The byte-complier croaks on this.  See Bug#28557.
;; (cl-defmacro cconv-tests-cl-defmacro ()
;;   (:documentation (concat "cl-defmacro" " documentation"))
;;   '(quote cl-defmacro-result))
;; (ert-deftest cconv-tests-cl-defmacro-:documentation ()
;;   "Docstring for cl-defmacro can be specified with :documentation."
;;   (should (string= (documentation 'cconv-tests-cl-defmacro)
;;                    "cl-defmacro documentation"))
;;   (should (eq (cconv-tests-cl-defmacro) 'cl-defmacro-result)))

(cl-iter-defun cconv-tests-cl-iter-defun ()
  (:documentation (concat "cl-iter-defun" " documentation"))
  (iter-yield 'cl-iter-defun-result))
(ert-deftest cconv-tests-cl-iter-defun-:documentation ()
  "Docstring for cl-iter-defun can be specified with :documentation."
  ;; FIXME: See Bug#28557.
  :tags '(:unstable)
  :expected-result :failed
  (should (string= (documentation 'cconv-tests-cl-iter-defun)
                   "cl-iter-defun documentation"))
  (should (eq (iter-next (cconv-tests-cl-iter-defun))
              'cl-iter-defun-result)))

(iter-defun cconv-tests-iter-defun ()
  (:documentation (concat "iter-defun" " documentation"))
  (iter-yield 'iter-defun-result))
(ert-deftest cconv-tests-iter-defun-:documentation ()
  "Docstring for iter-defun can be specified with :documentation."
  ;; FIXME: See Bug#28557.
  :tags '(:unstable)
  :expected-result :failed
  (should (string= (documentation 'cconv-tests-iter-defun)
                   "iter-defun documentation"))
  (should (eq (iter-next (cconv-tests-iter-defun)) 'iter-defun-result)))

(ert-deftest cconv-tests-iter-lambda-:documentation ()
  "Docstring for iter-lambda can be specified with :documentation."
  ;; FIXME: See Bug#28557.
  :expected-result :failed
  (let ((iter-fun
         (iter-lambda ()
           (:documentation (concat "iter-lambda" " documentation"))
           (iter-yield 'iter-lambda-result))))
    (should (string= (documentation iter-fun) "iter-lambda documentation"))
    (should (eq (iter-next (funcall iter-fun)) 'iter-lambda-result))))

(ert-deftest cconv-tests-cl-function-:documentation ()
  "Docstring for cl-function can be specified with :documentation."
  ;; FIXME: See Bug#28557.
  :expected-result :failed
  (let ((fun (cl-function (lambda (&key arg)
                            (:documentation (concat "cl-function"
                                                    " documentation"))
                            (list arg 'cl-function-result)))))
    (should (string= (documentation fun) "cl-function documentation"))
    (should (equal (funcall fun :arg t) '(t cl-function-result)))))

(ert-deftest cconv-tests-function-:documentation ()
  "Docstring for lambda inside function can be specified with :documentation."
  (let ((fun #'(lambda (arg)
                 (:documentation (concat "function" " documentation"))
                 (list arg 'function-result))))
    (should (string= (documentation fun) "function documentation"))
    (should (equal (funcall fun t) '(t function-result)))))

(fmakunbound 'cconv-tests-cl-defgeneric)
(setplist 'cconv-tests-cl-defgeneric nil)
(cl-defgeneric cconv-tests-cl-defgeneric (n)
  (:documentation (concat "cl-defgeneric" " documentation")))
(cl-defmethod cconv-tests-cl-defgeneric ((n integer))
  (:documentation (concat "cl-defmethod" " documentation"))
  (+ 1 n))
(ert-deftest cconv-tests-cl-defgeneric-:documentation ()
  "Docstring for cl-defgeneric can be specified with :documentation."
  ;; FIXME: See Bug#28557.
  :expected-result :failed
  (let ((descr (describe-function 'cconv-tests-cl-defgeneric)))
    (set-text-properties 0 (length descr) nil descr)
    (should (string-match-p "cl-defgeneric documentation" descr))
    (should (string-match-p "cl-defmethod documentation" descr)))
  (should (= 11 (cconv-tests-cl-defgeneric 10))))

(fmakunbound 'cconv-tests-cl-defgeneric-literal)
(setplist 'cconv-tests-cl-defgeneric-literal nil)
(cl-defgeneric cconv-tests-cl-defgeneric-literal (n)
  (:documentation "cl-defgeneric-literal documentation"))
(cl-defmethod cconv-tests-cl-defgeneric-literal ((n integer))
  (:documentation "cl-defmethod-literal documentation")
  (+ 1 n))
(ert-deftest cconv-tests-cl-defgeneric-literal-:documentation ()
  "Docstring for cl-defgeneric can be specified with :documentation."
  (let ((descr (describe-function 'cconv-tests-cl-defgeneric-literal)))
    (set-text-properties 0 (length descr) nil descr)
    (should (string-match-p "cl-defgeneric-literal documentation" descr))
    (should (string-match-p "cl-defmethod-literal documentation" descr)))
  (should (= 11 (cconv-tests-cl-defgeneric-literal 10))))

(defsubst cconv-tests-defsubst ()
  (:documentation (concat "defsubst" " documentation"))
  'defsubst-result)
(ert-deftest cconv-tests-defsubst-:documentation ()
  "Docstring for defsubst can be specified with :documentation."
  (should (string= (documentation 'cconv-tests-defsubst)
                   "defsubst documentation"))
  (should (eq (cconv-tests-defsubst) 'defsubst-result)))

(cl-defsubst cconv-tests-cl-defsubst ()
  (:documentation (concat "cl-defsubst" " documentation"))
  'cl-defsubst-result)
(ert-deftest cconv-tests-cl-defsubst-:documentation ()
  "Docstring for cl-defsubst can be specified with :documentation."
  (should (string= (documentation 'cconv-tests-cl-defsubst)
                   "cl-defsubst documentation"))
  (should (eq (cconv-tests-cl-defsubst) 'cl-defsubst-result)))

(ert-deftest cconv-convert-lambda-lifted ()
  "Bug#30872."
  (should
   (equal (funcall
           (byte-compile
            '#'(lambda (handle-fun arg)
                 (let* ((subfun
                         #'(lambda (params)
                             (ignore handle-fun)
                             (funcall #'(lambda () (setq params 42)))
                             params)))
                   (funcall subfun arg))))
           nil 99)
          42)))

(provide 'cconv-tests)
;; cconv-tests.el ends here.
