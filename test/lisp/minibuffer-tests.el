;;; completion-tests.el --- Tests for completion functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl-lib))

(ert-deftest completion-test1 ()
  (with-temp-buffer
    (cl-flet* ((test/completion-table (_string _pred action)
                                      (if (eq action 'lambda)
                                          nil
                                        "test: "))
               (test/completion-at-point ()
                                         (list (copy-marker (point-min))
                                               (copy-marker (point))
                                               #'test/completion-table)))
      (let ((completion-at-point-functions (list #'test/completion-at-point)))
        (insert "TEST")
        (completion-at-point)
        (should (equal (buffer-string)
                       "test: "))))))

(ert-deftest completion-table-with-predicate-test ()
  (let ((full-collection
         '("apple"                      ; Has A.
           "beet"                       ; Has B.
           "banana"                     ; Has A & B.
           "cherry"                     ; Has neither.
           ))
        (no-A (lambda (x) (not (string-match-p "a" x))))
        (no-B (lambda (x) (not (string-match-p "b" x)))))
    (should
     (member "cherry"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    (should-not
     (member "banana"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    ;; "apple" should still match when strict is nil.
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil)
                   no-B)))
    ;; "apple" should still match when strict is nil and pred2 is nil
    ;; (Bug#27841).
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil))))))

(provide 'completion-tests)
;;; completion-tests.el ends here
