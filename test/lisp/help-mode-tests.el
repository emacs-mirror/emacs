;;; help-mode-tests.el --- Tests for help-mode.el    -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

(require 'ert)
(require 'help-mode)
(require 'pp)

(ert-deftest help-mode-tests-help-buffer ()
  (let ((help-xref-following nil))
    (should (equal "*Help*" (help-buffer)))))

(ert-deftest help-mode-tests-help-buffer-current-buffer ()
  (with-temp-buffer
    (help-mode)
    (let ((help-xref-following t))
      (should (equal (buffer-name (current-buffer))
                     (help-buffer))))))

(ert-deftest help-mode-tests-make-xrefs ()
  (with-temp-buffer
    (insert "car is a built-in function in ‘C source code’.

(car LIST)

  Probably introduced at or before Emacs version 1.2.
  This function does not change global state, including the match data.

Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also ‘car-safe’.

See Info node ‘(elisp)Cons Cells’ for a discussion of related basic
Lisp concepts such as car, cdr, cons cell and list.")
    (help-mode)
    (help-make-xrefs)
    (let ((car-safe-button (button-at 298)))
      (should (eq (button-type car-safe-button) 'help-symbol))
      (should (eq (button-get car-safe-button 'help-function)
                  #'describe-symbol)))
    (let ((cons-cells-info-button (button-at 333)))
      (should (eq (button-type cons-cells-info-button) 'help-info))
      (should (eq (button-get cons-cells-info-button 'help-function)
                  #'info)))))

(ert-deftest help-mode-tests-xref-button ()
  (let* ((fmt "See also the function ‘%s’.")
         ;; 1+ translates string index to buffer position.
         (beg (1+ (string-search "%" fmt))))
    (with-temp-buffer
      (dolist (fn '(interactive \` = + - * / %))
        (erase-buffer)
        (insert (format fmt fn))
        (goto-char (point-min))
        (re-search-forward help-xref-symbol-regexp)
        (help-xref-button 9 'help-function)
        (should-not (button-at (1- beg)))
        (should-not (button-at (+ beg (length (symbol-name fn)))))
        (should (eq (button-type (button-at beg)) 'help-function))))))

(ert-deftest help-mode-tests-insert-xref-button ()
  (with-temp-buffer
    (help-insert-xref-button "[back]" 'help-back)
    (goto-char (point-min))
    (should (eq (button-type (button-at (point))) 'help-back))
    (help-insert-xref-button "[forward]" 'help-forward)
    ;; The back button should stay unchanged.
    (should (eq (button-type (button-at (point))) 'help-back))))

(ert-deftest help-mode-tests-xref-on-pp ()
  (with-temp-buffer
    (insert (pp '(cons fill-column)))
    (help-xref-on-pp (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "co")
    (should (eq (button-type (button-at (point))) 'help-function))
    (search-forward "-")
    (should (eq (button-type (button-at (point))) 'help-variable))))

(ert-deftest help-mode-tests-xref-go-back ()
  (let ((help-xref-stack
         `((2 ,(lambda () (erase-buffer) (insert "bar"))))))
    (with-temp-buffer
      (insert "foo")
      (help-xref-go-back (current-buffer))
      (should (= (point) 2))
      (should (equal (buffer-string) "bar")))))

(ert-deftest help-mode-tests-xref-go-forward ()
  (let ((help-xref-forward-stack
         `((2 ,(lambda () (erase-buffer) (insert "bar"))))))
    (with-temp-buffer
      (insert "foo")
      (help-xref-go-forward (current-buffer))
      (should (= (point) 2))
      (should (equal (buffer-string) "bar")))))

(ert-deftest help-mode-tests-go-back ()
  (let ((help-xref-stack
         `((2 ,(lambda () (erase-buffer) (insert "bar"))))))
    (with-temp-buffer
      (insert "foo")
      (help-go-back)
      (should (= (point) 2))
      (should (equal (buffer-string) "bar")))))

(ert-deftest help-mode-tests-go-back-no-stack ()
  (let ((help-xref-stack '()))
    (should-error (help-go-back))))

(ert-deftest help-mode-tests-go-forward ()
  (let ((help-xref-forward-stack
         `((2 ,(lambda () (erase-buffer) (insert "bar"))))))
    (with-temp-buffer
      (insert "foo")
      (help-go-forward)
      (should (= (point) 2))
      (should (equal (buffer-string) "bar")))))

(ert-deftest help-mode-tests-go-forward-no-stack ()
  (let ((help-xref-forward-stack '()))
    (should-error (help-go-forward))))

(ert-deftest help-mode-tests-do-xref ()
  (with-temp-buffer
    (help-mode)
    (help-do-xref 0 #'describe-symbol '(car))
    (should (looking-at-p "car is a"))
    (should (string-match-p "[back]" (buffer-string)))))

(ert-deftest help-mode-tests-follow-symbol ()
  (with-temp-buffer
    (insert "car")
    (help-mode)
    (help-follow-symbol 0)
    (should (looking-at-p "car is a"))
    (should (string-match-p "[back]" (buffer-string)))))

(ert-deftest help-mode-tests-follow-symbol-no-symbol ()
  (with-temp-buffer
    (insert "fXYEWnRHI0B9w6VJqQIw")
    (help-mode)
    (should-error (help-follow-symbol 0))))

(provide 'help-mode-tests)
;;; help-mode-tests.el ends here
