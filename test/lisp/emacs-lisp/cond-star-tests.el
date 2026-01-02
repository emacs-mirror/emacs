;;; cond-star-tests.el --- tests for emacs-lisp/cond-star.el  -*- lexical-binding:t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

(require 'cond-star)
(require 'ert)

(ert-deftest cond-star-test-1 ()
  (should (equal (cond*
                   ((pcase* `(,x . ,y) (cons 5 4)) (list x y))
                   (t 6))
                 '(5 4)))
  (should (equal (cond*
                   ((pcase* `(,x . ,y) nil) (list x y))
                   (t 6))
                 6))
  ;; FIXME: Not supported.
  ;; (let* ((z nil)
  ;;        (res (cond*
  ;;              ((pcase* `(,x . ,y) (cons 5 4)) (setq z 6) :non-exit)
  ;;              (t `(,x ,y ,z)))))
  ;;   (should (equal res '(5 4 6))))
  (should (equal (cond*
                   ((pcase* `(,x . ,y) (cons 5 4)))
                   (t (list x y)))
                 '(5 4)))
  (should (equal (cond*
                   ((pcase* `(,x . ,y) nil))
                   (t (list x y)))
                 '(nil nil)))
  )


;;; cond-star-tests.el ends here
