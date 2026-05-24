;;; cmds-tests.el --- Testing some Emacs commands -*- lexical-binding: t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

;; Author: Nicolas Richard <youngfrog@members.fsf.org>
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


(ert-deftest self-insert-command-with-negative-argument ()
  "Test `self-insert-command' with a negative argument."
  (let ((last-command-event ?a))
    (should-error (self-insert-command -1))))

(ert-deftest forward-line-with-bignum ()
  (with-temp-buffer
    (insert "x\n")
    (let ((shortage (forward-line (1- most-negative-fixnum))))
      (should (= shortage most-negative-fixnum)))
    (let ((shortage (forward-line (+ 2 most-positive-fixnum))))
      (should (= shortage (1+ most-positive-fixnum))))))

(ert-deftest self-insert-zero-newlines ()
  "Test `self-insert-command' with arguments which used to cause a crash."
  (with-temp-buffer
    (let* ((pt nil)
           (auto-fill-function (lambda () (setq pt (point)))))
      (self-insert-command 0 10)
      (should-not (equal pt 0)))))

(ert-deftest self-insert-nonascii-autofill ()
  "Test `self-insert-command' with a non-ASCII autofill function."
  (with-temp-buffer
    (let ((auto-fill-function
           (lambda ()
             (delete-char 1)
             (insert #x2000)
             (forward-char -1))))
      (dotimes (_ 10)
        (self-insert-command 1 10)
        (goto-char 2)
        (should (equal (point) 2))
        (should (equal (length (buffer-string)) 1))
        (should (equal (format "%S" (buffer-string))
                       "\"\x2000\""))
        (delete-char -1)))))

(provide 'cmds-tests)
;;; cmds-tests.el ends here
