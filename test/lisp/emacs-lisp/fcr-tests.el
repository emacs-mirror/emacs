;;; fcr-tests.e; --- Tests for FunCallableRecords  -*- lexical-binding: t; -*-

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
(require 'fcr)
(require 'cl-lib)

(fcr-defstruct (fcr-test
                ;; FIXME: Test `:parent'!
                (:copier fcr-test-copy))
  "Simple FCR."
  fst snd name)

(cl-defmethod fcr-test-gen ((_x compiled-function)) "#<bytecode>")

(cl-defmethod fcr-test-gen ((_x cons)) "#<cons>")

(cl-defmethod fcr-test-gen ((_x fcr-object))
  (format "#<fcr:%s>" (cl-call-next-method)))

(cl-defmethod fcr-test-gen ((_x fcr-test))
  (format "#<fcr-test:%s>" (cl-call-next-method)))

(ert-deftest fcr-tests ()
  (let* ((i 42)
         (fcr1 (fcr-lambda fcr-test ((fst 1) (snd 2) (name "hi"))
                           ()
                 (list fst snd i)))
         (fcr2 (fcr-lambda fcr-test ((name (cl-incf i)) (fst (cl-incf i)))
                           ()
                 (list fst snd 152 i))))
    (message "hello-1")
    (should (equal (list (fcr-test--fst fcr1)
                         (fcr-test--snd fcr1)
                         (fcr-test--name fcr1))
                   '(1 2 "hi")))
    (message "hello-2")
    (should (equal (list (fcr-test--fst fcr2)
                         (fcr-test--snd fcr2)
                         (fcr-test--name fcr2))
                   '(44 nil 43)))
    (message "hello-3")
    (should (equal (funcall fcr1) '(1 2 44)))
    (message "hello-4")
    (should (equal (funcall fcr2) '(44 nil 152 44)))
    (message "hello-5")
    (should (equal (funcall (fcr-test-copy fcr1 :fst 7)) '(7 2 44)))
    (message "hello-6")
    (should (cl-typep fcr1 'fcr-test))
    (message "hello-7")
    (should (cl-typep fcr1 'fcr-object))
    (should (member (fcr-test-gen fcr1)
                    '("#<fcr-test:#<fcr:#<cons>>>"
                      "#<fcr-test:#<fcr:#<bytecode>>>")))
    ))

;;; fcr-tests.el ends here.
