;;; cc-mode-tests.el --- Test suite for cc-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Michal Nazarewicz <mina86@mina86.com>
;; Keywords:       internal
;; Human-Keywords: internal

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

;; Unit tests for cc-mode.el.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cc-mode)

(ert-deftest c-or-c++-mode ()
  "Test c-or-c++-mode language detection."
  (cl-letf* ((mode nil)
             (do-test (lambda (content expected)
                        (delete-region (point-min) (point-max))
                        (insert content)
                        (setq mode nil)
                        (c-or-c++-mode)
                        (unless (eq expected mode)
                          (ert-fail
                           (format "expected %s but got %s when testing '%s'"
                                   expected mode content)))))
             ((symbol-function 'c-mode) (lambda () (setq mode 'c-mode)))
             ((symbol-function 'c++-mode) (lambda () (setq mode 'c++-mode))))
    (with-temp-buffer
      (mapc (lambda (content)
              (funcall do-test content 'c++-mode)
              (funcall do-test (concat "// " content) 'c-mode)
              (funcall do-test (concat " * " content) 'c-mode))
            '("using \t namespace \t std;"
              "using \t std::string;"
              "using Foo = Bar;"
              "namespace \t {"
              "namespace \t foo \t {"
              "namespace \t foo::bar \t {"
              "inline namespace \t foo \t {"
              "inline namespace \t foo::bar \t {"
              "class \t Blah_42 \t \n"
              "class \t Blah_42;"
              "class \t Blah_42 \t final {"
              "struct \t Blah_42 \t final {"
              "class \t _42_Blah:public Foo {"
              "struct \t _42_Blah:public Foo {"
              "template \t < class T >"
              "template< class T >"
              "#include <string>"
              "#include<iostream>"
              "#include \t <map>"))

      (mapc (lambda (content) (funcall do-test content 'c-mode))
            '("struct \t Blah_42 \t {"
              "struct template {"
              "struct Blah;"
              "#include <string.h>")))))

(ert-deftest c-mode-macro-comment ()
  "Test for bug#36484."
  (dolist (macro-string '("#define /***/f"
                          "#define x /***/5"
                          "#define a(x)get/***/x/***/id())"))
    (with-temp-buffer
      (insert macro-string)
      (c-mode))))

(ert-deftest c-lineup-ternary-bodies ()
  "Test for c-lineup-ternary-bodies function"
  (with-temp-buffer
    (c-mode)
    (let* ((common-prefix "int value = condition ")
           (expected-column (length common-prefix)))
      (dolist (test '(("? a : \n b" . nil)
                      ("? a \n ::b" . nil)
                      ("a \n : b" . nil)
                      ("? a \n : b" . t)
                      ("? ::a \n : b" . t)
                      ("? (p ? q : r) \n : b" . t)
                      ("? p ?: q \n : b" . t)
                      ("? p ? : q \n : b" . t)
                      ("? p ? q : r \n : b" . t)))
        (delete-region (point-min) (point-max))
        (insert common-prefix (car test))
        (should (equal
                 (and (cdr test) (vector expected-column))
                 (c-lineup-ternary-bodies '(statement-cont . 1))))))))

;;; cc-mode-tests.el ends here
