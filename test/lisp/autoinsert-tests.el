;;; autoinsert-tests.el --- Tests for autoinsert.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'autoinsert)
(require 'ert)

(ert-deftest autoinsert-tests-auto-insert-skeleton ()
  (let ((auto-insert-alist '((text-mode nil "f" _ "oo")))
        (auto-insert-query nil))
    (with-temp-buffer
      (text-mode)
      (auto-insert)
      (should (equal (buffer-string) "foo"))
      (should (equal (point) (+ (point-min) 1))))))

(ert-deftest autoinsert-tests-auto-insert-file ()
  (let ((temp-file (make-temp-file "autoinsert-tests" nil nil "foo")))
    (unwind-protect
        (let ((auto-insert-alist `((text-mode . ,temp-file)))
              (auto-insert-query nil))
          (with-temp-buffer
            (text-mode)
            (auto-insert)
            (should (equal (buffer-string) "foo"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest autoinsert-tests-auto-insert-function ()
  (let ((auto-insert-alist '((text-mode . (lambda () (insert "foo")))))
        (auto-insert-query nil))
    (with-temp-buffer
      (text-mode)
      (auto-insert)
      (should (equal (buffer-string) "foo")))))

(ert-deftest autoinsert-tests-auto-insert-vector ()
  (let ((auto-insert-alist '((text-mode . [(nil "f" _ "bar")
                                           (lambda () (insert "oo"))])))
        (auto-insert-query nil))
    (with-temp-buffer
      (text-mode)
      (auto-insert)
      (should (equal (buffer-string) "foobar")))))

(ert-deftest autoinsert-tests-auto-insert-regexp-match ()
  (let ((auto-insert-alist '(("foobar" nil "1st")
                             ("fo+bar" nil "2nd")
                             ("fo*bar" nil "3rd")))
        (auto-insert-query nil))
    (with-temp-buffer
      (setq-local buffer-file-name "fooobar")
      (auto-insert)
      (should (equal (buffer-string) "2nd")))))

(ert-deftest autoinsert-tests-define-auto-insert-before ()
  (let ((auto-insert-alist
         (list (cons 'text-mode '(lambda () (insert "foo")))))
        (auto-insert-query nil))
    (define-auto-insert 'text-mode
      '(lambda () (insert "bar")))
    (with-temp-buffer
      (text-mode)
      (auto-insert)
      (should (equal (buffer-string) "barfoo")))))

(ert-deftest autoinsert-tests-define-auto-insert-after ()
  (let ((auto-insert-alist
         (list (cons 'text-mode '(lambda () (insert "foo")))))
        (auto-insert-query nil))
    (define-auto-insert 'text-mode
      '(lambda () (insert "bar"))
      t)
    (with-temp-buffer
      (text-mode)
      (auto-insert)
      (should (equal (buffer-string) "foobar")))))

(provide 'autoinsert-tests)
;;; autoinsert-tests.el ends here
