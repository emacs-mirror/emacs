;;; glasses-tests.el --- Tests for glasses.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
(require 'glasses)
(require 'seq)

(ert-deftest glasses-tests-parenthesis-exception-p ()
  (with-temp-buffer
    (insert "public OnClickListener menuListener() {}")
    (let ((glasses-separate-parentheses-exceptions '("^Listen")))
      (should-not (glasses-parenthesis-exception-p 1 (point-max)))
      (should (glasses-parenthesis-exception-p 15 (point-max)))
      (should-not (glasses-parenthesis-exception-p 24 (point-max)))
      (should (glasses-parenthesis-exception-p 28 (point-max))))))

(ert-deftest glasses-tests-overlay-p ()
  (should
   (glasses-overlay-p (glasses-make-overlay (point-min) (point-max))))
  (should-not
   (glasses-overlay-p (make-overlay (point-min) (point-max)))))

(ert-deftest glasses-tests-make-overlay-p ()
  (let ((o (glasses-make-overlay (point-min) (point-max))))
    (should (eq (overlay-get o 'category) 'glasses)))
  (let ((o (glasses-make-overlay (point-min) (point-max) 'foo)))
    (should (eq (overlay-get o 'category) 'foo))))

(ert-deftest glasses-tests-make-readable ()
  (with-temp-buffer
    (insert "pp.setBackgroundResource(R.drawable.button_right);")
    (glasses-make-readable (point-min) (point-max))
    (pcase-let ((`(,o1 ,o2 ,o3)
                 (sort (overlays-in (point-min) (point-max))
                       (lambda (o1 o2)
                         (< (overlay-start o1) (overlay-start o2))))))
      (should (= (overlay-start o1) 7))
      (should (equal (overlay-get o1 'before-string)
                     glasses-separator))
      (should (= (overlay-start o2) 17))
      (should (equal (overlay-get o2 'before-string)
                     glasses-separator))
      (should (= (overlay-start o3) 25))
      (should (equal (overlay-get o3 'before-string) " ")))))

(ert-deftest glasses-tests-make-readable-dont-separate-parentheses ()
  (with-temp-buffer
    (insert "pp.setBackgroundResource(R.drawable.button_right);")
    (let ((glasses-separate-parentheses-p nil))
      (glasses-make-readable (point-min) (point-max))
      (should-not (overlays-at 25)))))

(ert-deftest glasses-tests-make-unreadable ()
  (with-temp-buffer
    (insert "pp.setBackgroundResource(R.drawable.button_right);")
    (glasses-make-readable (point-min) (point-max))
    (should (seq-some #'glasses-overlay-p
                      (overlays-in (point-min) (point-max))))
    (glasses-make-unreadable (point-min) (point-max))
    (should-not (seq-some #'glasses-overlay-p
                          (overlays-in (point-min) (point-max))))))

(ert-deftest glasses-tests-convert-to-unreadable ()
  (with-temp-buffer
    (insert "set_Background_Resource(R.button_right);")
    (let ((glasses-convert-on-write-p nil))
      (should-not (glasses-convert-to-unreadable))
      (should (equal (buffer-string)
                     "set_Background_Resource(R.button_right);")))
    (let ((glasses-convert-on-write-p t))
      (should-not (glasses-convert-to-unreadable))
      (should (equal (buffer-string)
                     "setBackgroundResource(R.button_right);")))))

(provide 'glasses-tests)
;;; glasses-tests.el ends here
