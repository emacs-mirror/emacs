;;; mule-util-tests.el --- tests for international/mule-util.el  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
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
        (let ((truncate-string-ellipsis "..."))
          (should (equal (apply 'truncate-string-to-width ',(car testdata))
                         ,(cdr testdata))))))))

(dotimes (i (length mule-util-test-truncate-data))
  (mule-util-test-truncate-create i))

(ert-deftest filepos/bufferpos-tests-utf-8 ()
  (let ((coding-system-for-read 'utf-8-unix))
    (with-temp-buffer
      (insert-file-contents (ert-resource-file "utf-8.txt"))
      (should (eq buffer-file-coding-system 'utf-8-unix))
      ;; First line is "Thís is a test line 1.".
      ;; Bytes start counting at 0; chars at 1.
      (should (= (filepos-to-bufferpos 1 'exact) 2))
      (should (= (bufferpos-to-filepos 2 'exact) 1))
      ;; After non-ASCII.
      (should (= (filepos-to-bufferpos 4 'exact) 4))
      (should (= (bufferpos-to-filepos 4 'exact) 4)))))

(ert-deftest filepos/bufferpos-tests-binary ()
  (let ((coding-system-for-read 'binary))
    (with-temp-buffer
      (insert-file-contents (ert-resource-file "utf-8.txt"))
      (should (eq buffer-file-coding-system 'no-conversion))
      ;; First line is "Thís is a test line 1.".
      ;; Bytes start counting at 0; chars at 1.
      (should (= (filepos-to-bufferpos 1 'exact) 2))
      (should (= (bufferpos-to-filepos 2 'exact) 1))
      ;; After non-ASCII.
      (should (= (filepos-to-bufferpos 4 'exact) 5))
      (should (= (bufferpos-to-filepos 5 'exact) 4)))))

(ert-deftest filepos/bufferpos-tests-undecided ()
  (let ((coding-system-for-read 'binary))
    (with-temp-buffer
      (insert-file-contents (ert-resource-file "utf-8.txt"))
      (setq buffer-file-coding-system 'undecided)
      (should-error (filepos-to-bufferpos 1 'exact))
      (should-error (bufferpos-to-filepos 2 'exact))
      (should (= (filepos-to-bufferpos 1 'approximate) 2))
      (should (= (bufferpos-to-filepos 2 'approximate) 1))
      ;; After non-ASCII.
      (should (= (filepos-to-bufferpos 4 'approximate) 5))
      (should (= (bufferpos-to-filepos 5 'approximate) 4)))))

;;; mule-util-tests.el ends here
