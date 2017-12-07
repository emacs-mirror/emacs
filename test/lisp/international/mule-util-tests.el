;;; mule-util --- tests for international/mule-util.el

;; Copyright (C) 2002-2017 Free Software Foundation, Inc.

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

(require 'ert)
(require 'mule-util)

(defconst mule-util-test-truncate-data
  '((("" 0) . "")
    (("x" 1) . "x")
    (("xy" 1) . "x")
    (("xy" 2 1) . "y")
    (("xy" 0) . "")
    (("xy" 3) . "xy")
    (("中" 0) . "")
    (("中" 1) . "")
    (("中" 2) . "中")
    (("中" 1 nil ? ) . " ")
    (("中文" 3 1 ? ) . "  ")
    (("x中x" 2) . "x")
    (("x中x" 3) . "x中")
    (("x中x" 3) . "x中")
    (("x中x" 4 1) . "中x")
    (("kor한e글an" 8 1 ? ) . "or한e글")
    (("kor한e글an" 7 2 ? ) . "r한e ")
    (("" 0 nil nil "...") . "")
    (("x" 3 nil nil "...") . "x")
    (("中" 3 nil nil "...") . "中")
    (("foo" 3 nil nil "...") . "foo")
    (("foo" 2 nil nil "...") . "fo") ;; XEmacs failure?
    (("foobar" 6 0 nil "...") . "foobar")
    (("foobarbaz" 6 nil nil "...") . "foo...")
    (("foobarbaz" 7 2 nil "...") . "ob...")
    (("foobarbaz" 9 3 nil "...") . "barbaz")
    (("こhんeにlちlはo" 15 1 ?  t) . " hんeにlちlはo")
    (("こhんeにlちlはo" 14 1 ?  t) . " hんeにlち...")
    (("x" 3 nil nil "粵語") . "x")
    (("中" 2 nil nil "粵語") . "中")
    (("中" 1 nil ?x "粵語") . "x") ;; XEmacs error
    (("中文" 3 nil ?  "粵語") . "中 ") ;; XEmacs error
    (("foobarbaz" 4 nil nil  "粵語") . "粵語")
    (("foobarbaz" 5 nil nil  "粵語") . "f粵語")
    (("foobarbaz" 6 nil nil  "粵語") . "fo粵語")
    (("foobarbaz" 8 3 nil "粵語") . "b粵語")
    (("こhんeにlちlはo" 14 4 ?x "日本語") . "xeに日本語")
    (("こhんeにlちlはo" 13 4 ?x "日本語") . "xex日本語")
    )
  "Test data for `truncate-string-to-width'.")

(defun mule-util-test-truncate-create (n)
  "Create a test for element N of the `mule-util-test-truncate-data' constant."
  (let ((testname (intern (format "mule-util-test-truncate-%.2d" n)))
        (testdoc (format "Test element %d of `mule-util-test-truncate-data'."
                         n))
        (testdata (nth n mule-util-test-truncate-data)))
    (eval
     `(ert-deftest ,testname ()
        ,testdoc
        (should (equal (apply 'truncate-string-to-width ',(car testdata))
                              ,(cdr testdata)))))))

(dotimes (i (length mule-util-test-truncate-data))
  (mule-util-test-truncate-create i))

;;; mule-util.el ends here
