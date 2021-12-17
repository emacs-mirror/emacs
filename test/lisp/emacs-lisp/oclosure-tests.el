;;; oclosure-tests.e; --- Tests for Open Closures  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'oclosure)
(require 'cl-lib)

(oclosure-define (oclosure-test
                ;; FIXME: Test `:parent'!
                (:copier oclosure-test-copy))
  "Simple OClosure."
  fst snd name)

(cl-defmethod oclosure-test-gen ((_x compiled-function)) "#<bytecode>")

(cl-defmethod oclosure-test-gen ((_x cons)) "#<cons>")

(cl-defmethod oclosure-test-gen ((_x oclosure-object))
  (format "#<oclosure:%s>" (cl-call-next-method)))

(cl-defmethod oclosure-test-gen ((_x oclosure-test))
  (format "#<oclosure-test:%s>" (cl-call-next-method)))

(ert-deftest oclosure-tests ()
  (let* ((i 42)
         (ocl1 (oclosure-lambda oclosure-test ((fst 1) (snd 2) (name "hi"))
                           ()
                 (list fst snd i)))
         (ocl2 (oclosure-lambda oclosure-test ((name (cl-incf i)) (fst (cl-incf i)))
                           ()
                 (list fst snd 152 i))))
    (message "hello-1")
    (should (equal (list (oclosure-test--fst ocl1)
                         (oclosure-test--snd ocl1)
                         (oclosure-test--name ocl1))
                   '(1 2 "hi")))
    (message "hello-2")
    (should (equal (list (oclosure-test--fst ocl2)
                         (oclosure-test--snd ocl2)
                         (oclosure-test--name ocl2))
                   '(44 nil 43)))
    (message "hello-3")
    (should (equal (funcall ocl1) '(1 2 44)))
    (message "hello-4")
    (should (equal (funcall ocl2) '(44 nil 152 44)))
    (message "hello-5")
    (should (equal (funcall (oclosure-test-copy ocl1 :fst 7)) '(7 2 44)))
    (message "hello-6")
    (should (cl-typep ocl1 'oclosure-test))
    (message "hello-7")
    (should (cl-typep ocl1 'oclosure-object))
    (should (member (oclosure-test-gen ocl1)
                    '("#<oclosure-test:#<oclosure:#<cons>>>"
                      "#<oclosure-test:#<oclosure:#<bytecode>>>")))
    ))

;;; oclosure-tests.el ends here.
