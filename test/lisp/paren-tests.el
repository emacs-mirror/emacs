;;; paren-tests.el --- Tests for paren.el  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'paren)

(ert-deftest paren-tests-unescaped-p ()
  (with-temp-buffer
    (insert "(insert)")
    (backward-char)
    (should (show-paren--unescaped-p (point)))
    (insert "\\")
    (should-not (show-paren--unescaped-p (point)))))

(ert-deftest paren-tests-categorize-paren ()
  (with-temp-buffer
    (insert "(insert)")
    (backward-char)
    (should (equal (show-paren--categorize-paren (point))
                   (cons -1 (+ (point) 1))))
    (goto-char (point-min))
    (should (equal (show-paren--categorize-paren (point))
                   (cons 1 (point))))))

(ert-deftest paren-tests-locate-near-paren ()
  (with-temp-buffer
    (let ((show-paren-when-point-inside-paren nil)
          (show-paren-when-point-in-periphery nil))
      (insert "(insert)")
      (should (equal (show-paren--locate-near-paren)
                     (cons -1 (point))))
      (backward-char)
      (should-not (show-paren--locate-near-paren))
      (goto-char (point-min))
      (should (equal (show-paren--locate-near-paren)
                     (cons 1 (point-min))))
      (forward-char)
      (should-not (show-paren--locate-near-paren)))))

(ert-deftest paren-tests-locate-near-paren-inside ()
  (with-temp-buffer
    (let ((show-paren-when-point-inside-paren t)
          (show-paren-when-point-in-periphery nil))
      (insert "(insert)")
      (should (equal (show-paren--locate-near-paren)
                     (cons -1 (point))))
      (backward-char)
      (should (equal (show-paren--locate-near-paren)
                     (cons -1 (+ (point) 1))))
      (goto-char (point-min))
      (should (equal (show-paren--locate-near-paren)
                     (cons 1 (point-min))))
      (forward-char)
      (should (equal (show-paren--locate-near-paren)
                     (cons 1 (point-min)))))))

(ert-deftest paren-tests-locate-near-paren-in-periphery ()
  (with-temp-buffer
    (let ((show-paren-when-point-inside-paren nil)
          (show-paren-when-point-in-periphery t))
      (insert " (insert) ")
      (should (equal (show-paren--locate-near-paren)
                     (cons -1 (- (point) 1))))
      (backward-char 2)
      (should-not (show-paren--locate-near-paren))
      (goto-char (point-min))
      (should (equal (show-paren--locate-near-paren)
                     (cons 1 (+ (point) 1))))
      (forward-char)
      (should (equal (show-paren--locate-near-paren)
                     (cons 1 (point))))
      (forward-char)
      (should-not (show-paren--locate-near-paren)))))

(ert-deftest paren-tests-default ()
  (with-temp-buffer
    (insert "(insert")
    (goto-char (point-min))
    (should (equal (show-paren--default)
                   (list (point) (+ (point) 1)
                         nil nil
                         t)))
    (goto-char (point-max))
    (insert ")")
    (should (equal (show-paren--default)
                   (list (- (point) 1) (point)
                         (point-min) (+ (point-min) 1)
                         nil)))
    (goto-char (point-min))
    (should (equal (show-paren--default)
                   (list (point) (+ (point) 1)
                         (- (point-max) 1) (point-max)
                         nil)))))

(provide 'paren-tests)
;;; paren-tests.el ends here
