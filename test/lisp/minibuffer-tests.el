;;; completion-tests.el --- Tests for completion functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
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
(require 'ert-x)

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
      (let ((completion-styles '(basic))
            (completion-at-point-functions (list #'test/completion-at-point)))
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

(ert-deftest completion-table-subvert-test ()
  (let* ((origtable '("A-hello" "A-there"))
         (subvtable (completion-table-subvert origtable "B" "A")))
    (should (equal (try-completion "B-hel" subvtable)
                   "B-hello"))))

(ert-deftest completion-table-test-quoting ()
  (let ((process-environment
         `("CTTQ1=ed" "CTTQ2=et/" ,@process-environment))
        (default-directory (ert-resource-directory)))
    (pcase-dolist (`(,input ,output)
                   '(
                     ;; Test that $ in files is properly $$ quoted.
                     ("data/m-cttq" "data/minibuffer-test-cttq$$tion")
                     ;; Test that $$ in input is properly unquoted.
                     ("data/m-cttq$$t" "data/minibuffer-test-cttq$$tion")
                     ;; Test that env-vars are preserved.
                     ("lisp/c${CTTQ1}et/se-u" "lisp/c${CTTQ1}et/semantic-utest")
                     ("lisp/ced${CTTQ2}se-u" "lisp/ced${CTTQ2}semantic-utest")
                     ;; Test that env-vars don't prevent partial-completion.
                     ;; FIXME: Ideally we'd like to keep the ${CTTQ}!
                     ("lis/c${CTTQ1}/se-u" "lisp/cedet/semantic-utest")
                     ))
      (should (equal (completion-try-completion input
                                                #'completion--file-name-table
                                                nil (length input))
                     (cons output (length output)))))))

(defun completion--pcm-score (comp)
  (get-text-property 0 'completion-score comp))

(defun completion--pcm-first-difference-pos (comp)
  (cl-loop for pos = (next-single-property-change 0 'face comp)
           then (next-single-property-change pos 'face comp)
           while pos
           when (eq (get-text-property pos 'face comp)
                    'completions-first-difference)
           return pos))

(ert-deftest completion-pcm-test-1 ()
  ;; Point is at end, this does not match anything
  (should (equal
           (completion-pcm-all-completions
            "foo" '("hello" "world" "barfoobar") nil 3)
           nil)))

(ert-deftest completion-pcm-test-2 ()
  ;; Point is at beginning, this matches "barfoobar"
  (should (equal
           (car (completion-pcm-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 0))
           "barfoobar")))

(ert-deftest completion-pcm-test-3 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-pcm-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-pcm-test-4 ()
  ;; One fourth of a match and no match due to point being at the end
  (should (eql
           (completion--pcm-score
            (car (completion-pcm-all-completions
                  "RO" '("RaOb") nil 1)))
           (/ 1.0 4.0)))
  (should (equal
           (completion-pcm-all-completions
            "RO" '("RaOb") nil 2)
           nil)))

(ert-deftest completion-pcm-test-5 ()
  ;; Point is at beginning, but `completions-first-difference' is
  ;; moved after it
  (should (eql
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "f" '("few" "many") nil 0)))
           1)))

(ert-deftest completion-pcm-test-6 ()
  ;; Wildcards and delimiters work
  (should (equal
           (car (completion-pcm-all-completions
                 "li-pac*" '("list-packages") nil 7))
           "list-packages"))
  (should (equal
             (car (completion-pcm-all-completions
                   "li-pac*" '("do-not-list-packages") nil 7))
             nil)))

(ert-deftest completion-substring-test-1 ()
  ;; One third of a match!
  (should (equal
           (car (completion-substring-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 3))
           "barfoobar"))
  (should (eql
           (completion--pcm-score
            (car (completion-substring-all-completions
                  "foo" '("hello" "world" "barfoobar") nil 3)))
           (/ 1.0 3.0))))

(ert-deftest completion-substring-test-2 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-substring-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-substring-test-3 ()
  ;; Substring match
  (should (equal
           (car (completion-substring-all-completions
                  "custgroup" '("customize-group") nil 4))
           "customize-group"))
  (should (equal
           (car (completion-substring-all-completions
                  "custgroup" '("customize-group") nil 5))
           nil)))

(ert-deftest completion-substring-test-4 ()
  ;; `completions-first-difference' should be at the right place
  (should (eql
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjobstabby" "many") nil 1)))
           4))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 1)))
           6))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 3)))
           6)))

(ert-deftest completion-flex-test-1 ()
  ;; Fuzzy match
  (should (equal
           (car (completion-flex-all-completions
                 "foo" '("hello" "world" "fabrobazo") nil 3))
           "fabrobazo")))

(ert-deftest completion-flex-test-2 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-flex-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-flex-test-3 ()
  ;; Another fuzzy match, but more of a "substring" one
  (should (equal
           (car (completion-flex-all-completions
                 "custgroup" '("customize-group-other-window") nil 4))
           "customize-group-other-window"))
  ;; `completions-first-difference' should be at the right place
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 4)))
           4))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 9)))
           15)))

(provide 'completion-tests)
;;; completion-tests.el ends here
