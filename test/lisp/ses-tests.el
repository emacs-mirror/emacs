;;; SES-tests.el --- Tests for ses.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2025 Free Software Foundation, Inc.

;; Author: Vincent Belaïche <vincentb1@users.sourceforge.net>

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

;;; Code:

(require 'ert)
(eval-when-compile (require 'ert-x))
(require 'ses)

;; Silence byte-compiler.
(with-suppressed-warnings ((lexical ses--cells)
                           (lexical A1)
                           (lexical A2)
                           (lexical A3)
                           (lexical ses--foo)
                           (lexical ses--bar)
                           (lexical B1)
                           (lexical B2)
                           (lexical ses--toto))
  (defvar ses--cells)
  (defvar A1)
  (defvar A2)
  (defvar A3)
  (defvar ses--foo)
  (defvar ses--bar)
  (defvar B1)
  (defvar B2)
  (defvar ses--toto))

;; PLAIN FORMULA TESTS
;; ======================================================================

(ert-deftest ses-tests-lowlevel-plain-formula ()
  "Check that setting A1 to 1 and A2 to (1+ A1), makes A2 value
equal to 2. This is done with low level functions calls, not like
interactively."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'ses-cell-set-formula c)
        (apply 'ses-calculate-cell (list (car c) (cadr c) nil)))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-plain-formula ()
  "Check that setting A1 to 1 and A2 to (1+ A1), makes A2 value
equal to 2. This is done  using interactive calls."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook)
      (should (eq (bound-and-true-p A2) 2)))))

;; PLAIN CELL RENAMING TESTS
;; ======================================================================

(ert-deftest ses-tests-lowlevel-renamed-cell ()
  "Check that renaming A1 to `ses--foo' and setting `ses--foo' to 1 and A2 to (1+ ses--foo), makes A2 value equal to 2.
This is done using low level functions, `ses-rename-cell' is not
called but instead we use text replacement in the buffer
previously passed in text mode."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'ses-cell-set-formula c)
        (apply 'ses-calculate-cell (list (car c) (cadr c) nil)))
      (ses-write-cells)
      (text-mode)
      (goto-char (point-min))
      (while (re-search-forward "\\<A1\\>" nil t)
        (replace-match "ses--foo" t t))
      (ses-mode)
      (should-not  (local-variable-p 'A1))
      (should (eq ses--foo 1))
      (should (equal (ses-cell-formula 1 0) '(ses-safe-formula (1+ ses--foo))))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-renamed-cell ()
  "Check that renaming A1 to `ses--foo' and setting `ses--foo' to 1 and A2
to (1+ ses--foo), makes A2 value equal to 2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (ses-rename-cell 'ses--foo (ses-get-cell 0 0))
      (dolist (c '((0 0 1) (1 0 (1+ ses--foo))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook)
      (should-not  (local-variable-p 'A1))
      (should (eq ses--foo 1))
      (should (equal (ses-cell-formula 1 0) '(1+ ses--foo)))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-renamed-cell-after-setting ()
  "Check that setting A1 to 1 and A2 to (1+ A1), and then
renaming A2 to `ses--foo' makes `ses--foo' value equal to 2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook); deferred recalc
      (ses-rename-cell 'ses--foo (ses-get-cell 0 0))
      (should-not  (local-variable-p 'A1))
      (should (eq ses--foo 1))
      (should (equal (ses-cell-formula 1 0) '(1+ ses--foo)))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-renaming-cell-with-one-symbol-formula ()
  "Check that setting A1 to 1 and A2 to A1, and then renaming A1
to `ses--foo' makes `ses--foo' value equal to 1. Then set A1 to 2 and check
that `ses--foo' becomes 2."
  (let ((ses-initial-size '(3 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 A1)))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook); deferred recalc
      (ses-rename-cell 'ses--foo (ses-get-cell 0 0))
      (ses-command-hook); deferred recalc
      (should-not  (local-variable-p 'A1))
      (should (eq ses--foo 1))
      (should (equal (ses-cell-formula 1 0) 'ses--foo))
      (should (eq (bound-and-true-p A2) 1))
      (funcall-interactively 'ses-edit-cell 0 0 2)
      (ses-command-hook); deferred recalc
      (should (eq (bound-and-true-p A2) 2))
      (should (eq ses--foo 2)))))


;; ROW INSERTION TESTS
;; ======================================================================

(ert-deftest ses-tests-plain-row-insertion ()
  "Check that setting A1 to 1 and A2 to (1+ A1), and then jumping
to A2 and inserting a row, makes A2 value empty, and A3 equal to
2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook)
      (ses-jump 'A2)
      (ses-insert-row 1)
      (ses-command-hook)
      (should-not (bound-and-true-p A2))
      (should (eq (bound-and-true-p A3) 2)))))


(ert-deftest ses-tests-renamed-cells-row-insertion ()
  "Check that setting A1 to 1 and A2 to (1+ A1), and then renaming A1 to `ses--foo' and A2 to `ses--bar' jumping
to `ses--bar' and inserting a row, makes A2 value empty, and `ses--bar' equal to
2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook)
      (ses-rename-cell 'ses--foo (ses-get-cell 0 0))
      (ses-command-hook)
      (ses-rename-cell 'ses--bar (ses-get-cell 1 0))
      (ses-command-hook)
      (should (eq ses--bar 2))
      (ses-jump 'ses--bar)
      (ses-insert-row 1)
      (ses-command-hook)
      (should-not (bound-and-true-p A2))
      (should (eq ses--bar 2)))))


;; JUMP tests
;; ======================================================================
(ert-deftest ses-jump-B2-prefix-arg ()
  "Test jumping to cell B2 by use of prefix argument"
  (let ((ses-initial-size '(3 . 3))
        ses-after-entry-functions)
    (with-temp-buffer
      (ses-mode)
      ;; C-u 4 M-x ses-jump
      (let ((current-prefix-arg 4))
        (call-interactively 'ses-jump))
      (should (eq (ses--cell-at-pos (point)) 'B2)))))


(ert-deftest ses-jump-B2-lowcase ()
  "Test jumping to cell B2 by use of lowercase cell name string"
  (let ((ses-initial-size '(3 . 3))
        ses-after-entry-functions)
    (with-temp-buffer
      (ses-mode)
      (funcall-interactively 'ses-jump "b2")
      (ses-command-hook)
      (should (eq (ses--cell-at-pos (point)) 'B2)))))

(ert-deftest ses-jump-B2-lowcase-keys ()
  "Test jumping to cell B2 by use of lowercase cell name string with simulating keys"
  (let ((ses-initial-size '(3 . 3))
        ses-after-entry-functions)
    (with-temp-buffer
      (ses-mode)
      (ert-simulate-keys [ ?b ?2 return] (ses-jump))
      (ses-command-hook)
      (should (eq (ses--cell-at-pos (point)) 'B2)))))

(ert-deftest ses-jump-B2-symbol ()
  "Test jumping to cell B2 by use of cell name symbol"
  (let ((ses-initial-size '(3 . 3))
        ses-after-entry-functions)
    (with-temp-buffer
      (ses-mode)
      (funcall-interactively 'ses-jump 'B2)
      (ses-command-hook)
      (should (eq (ses--cell-at-pos (point)) 'B2)))))

(ert-deftest ses-jump-B2-renamed ()
  "Test jumping to cell B2 after renaming it `ses--toto'."
  (let ((ses-initial-size '(3 . 3))
        ses-after-entry-functions)
    (with-temp-buffer
      (ses-mode)
      (ses-rename-cell 'ses--toto (ses-get-cell 1 1))
      (ses-jump 'ses--toto)
      (ses-command-hook)
      (should (eq (ses--cell-at-pos (point)) 'ses--toto)))))

(ert-deftest ses-expand-range ()
  "Test `ses-range' expansion works well even if the current buffer is not a SES buffer during expansion."
  (let ((ses-initial-size '(4 . 3))
        ses-after-entry-functions
        (ses-buffer (generate-new-buffer "*SES tests*")))
    (unwind-protect
        (progn
          (with-current-buffer ses-buffer
            (ses-mode)
            (dolist (c '([0 0 1] [1 0 2] [2 0 3]))
              (ses-set-cell (aref c 0) (aref c 1) 'value (aref c 2)))
            (dolist (c '((0 1 (* 2 A1)) (1 1 (* 2 A2)) (2 1 (* 2 A3))))
              (apply 'ses-cell-set-formula c)
              (apply 'ses-calculate-cell (list (car c) (cadr c) nil))))

          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 v))
                          '(1 2 3)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 ^))
                          '(3 2 1)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 B3 >v))
                          '(1 2 2 4 3 6)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 B3 >^))
                          '(3 6 2 4 1 2)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 B3 <v))
                          '(2 1 4 2 6 3)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 B3 <^))
                          '(6 3 4 2 2 1)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 v !))
                          '(1 2 3)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 B4 >v !))
                          '(1 2 2 4 3 6)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A4 v _ 10))
                          '(1 2 3 10)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 B4 >v _ 10))
                          '(1 2 2 4 3 6 10 10)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 v *))
                          '(vec 1 2 3)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 v *1))
                          '(vec 1 2 3)))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 v *2))
                          '(vec (vec 1 2 3))))
          (should (equal  (with-current-buffer ses-buffer (ses-range A1 A3 > *2))
                          '(vec (vec 1) (vec 2) (vec 3)))))
      (kill-buffer ses-buffer))))

(ert-deftest ses-set-formula-write-cells-with-changed-references ()
  "Test fix of bug#5852.
When setting a formula has some cell with changed references, this
cell has to be rewritten to data area."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 1 1); B1
                   (1 0 2) (1 1 (+ B1 A2)); A2 B2
                   (2 0 4); A3
                   (3 0 3) (3 1 (+ B2 A4))));A4 B4
        (apply 'ses-cell-set-formula c)
        (apply 'ses-calculate-cell (list (car c) (cadr c) nil)))
      (ses-cell-set-formula 2 1 '(+ B2 A3)); B3
      (ses-command-hook)
      (ses-cell-set-formula 3 1 '(+ B3 A4)); B4
      (ses-command-hook)
      (should (equal (ses-cell-references 1 1) '(B3)))
      (ses-mode)
      (should (equal (ses-cell-references 1 1) '(B3))))))

;; Tests for ses-select

(ert-deftest ses-select-different-list-sizes ()
  "Test `ses-select' thows an error when list sizes have different size."
  (should (null (eq (condition-case nil
                        ;; protected form
                        (progn
                          (ses-select '(1 2) 2 '(1 2 3))
                          :is-not-error)
                      ;; handler
                      (error
                       :is-error))
                    :is-not-error))))

(ert-deftest ses-select-plain ()
  "Test `ses-select' on a plain case."
  (should (equal (ses-select '(nil t t) t '(1 2 3)) '(2 3))))

(ert-deftest ses-select-specific-test-function ()
  "Test `ses-select' with an specific test function."
  (should (equal (ses-select '(1 2 3 4 5)
                             nil
                             '(6 7 8 9 10)
                             (lambda (x y)
                               (unless (null y) (error "y"))
                               (unless (integerp x) (error "x"))
                               (>= x 4)))
                 '(9 10))))

;; Tests for ses-set

(ert-deftest ses-set-sv ()
  "Set values, cells denoted by symbol."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil))
    (with-temp-buffer
      (ses-mode)
      (ses-set A1 1 B1 2
                A2 (+ A1 B1) B2 (+ B1 A2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      ;; values are'nt changed because (+ A1 B1) and (+ B1 A2) are
      ;; evaluated before being set to A2 and B2. So A2's formula is 3,
      ;; and B2's formula is 5
      (should (eq A2 3))
      (should (eq B2 5))
      )))

(ert-deftest ses-set-sqv-sqfq ()
  "Set values and formulas, cells denoted by symbol."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil))
    (with-temp-buffer
      (ses-mode)
      (ses-set A1 1 B1 2
                :: sqfq A2 (+ A1 B1) B2 (+ B1 A2))
      (should (eq A1 1))
      (should (eq B1 2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A2 2))
      (should (eq B2 4))
      )))

(ert-deftest ses-set-rcv ()
  "Set values, cells denoted by coordinates."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil))
    (with-temp-buffer
      (ses-mode)
      (ses-set :: rcv
                0 0 1; A1 := 1
                0 1 2; B1 := 2
                1 0 (+ A1 B1); A2 := A1 + B1
                1 1 (+ B1 A2); B2 := B1 + A2
                )
      (should (eq A1 1))
      (should (eq B1 2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      ;; values are'nt changed because (+ A1 B1) and (+ B1 A2) are
      ;; evaluated before being set to A2 and B2. So A2's formula is 3,
      ;; and B2's formula is 5
      (should (eq A2 3))
      (should (eq B2 5))
      )))

(ert-deftest ses-set-rcv-rcfq ()
  "Set values and formulas, cells denoted by coordinates."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil))
    (with-temp-buffer
      (ses-mode)
      (ses-set :: rcv
                0 0 1; A1 := 1
                0 1 2; B1 := 2
                :: rcfq
                1 0 (+ A1 B1); A2 := A1 + B1
                1 1 (+ B1 A2); B2 := B1 + A2
                )
      (should (eq A1 1))
      (should (eq B1 2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      (should (eq A2 2))
      (should (eq B2 4))
      )))

(ert-deftest ses-set-rcxv ()
  "Set values, cells denoted by coordinates expressions."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil)
        (zero 0)
        (one 1))
    (with-temp-buffer
      (ses-mode)
      (ses-set :: rcv
               zero zero 1; A1 := 1
               zero one  2; B1 := 2
               one zero  (+ A1 B1); A2 := A1 + B1
               one one   (+ B1 A2); B2 := B1 + A2
               )
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      ;; values are'nt changed because (+ A1 B1) and (+ B1 A2) are
      ;; evaluated before being set to A2 and B2. So A2's formula is 3,
      ;; and B2's formula is 5
      (should (eq A2 3))
      (should (eq B2 5)))))

(ert-deftest ses-set-rcxv-rcxfq ()
  "Set values and formulas, cells denoted by coordinates expressions."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil)
        (zero 0)
        (one 1))
    (with-temp-buffer
      (ses-mode)
      (ses-set :: rcv
               zero zero 1; A1 := 1
               zero one  2; B1 := 2
               :: rcfq
               one zero  (+ A1 B1); A2 := A1 + B1
               one one   (+ B1 A2); B2 := B1 + A2
               )
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      (should (eq A2 2))
      (should (eq B2 4)))))

(ert-deftest ses-set-sqv-sqf ()
  "Set values and formulas, formulas are expressions."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil)
        (A2-form '(+ A1 B1))
        (B2-form '(+ B1 A2)))
    (with-temp-buffer
      (ses-mode)
      (ses-set
       A1 1; A1 := 1
       B1 2; B1 := 2
       :: sqf
       A2 A2-form; A2 := A1 + B1
       B2 B2-form; B2 := B1 + A2
       )
      (should (eq A1 1))
      (should (eq B1 2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      (should (eq A2 2))
      (should (eq B2 4)))))

(ert-deftest ses-set-rcv-rcf ()
  "Set values and formulas, cells denoted by coordinates, formulas are expressions."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil)
        (A2-form '(+ A1 B1))
        (B2-form '(+ B1 A2)))
    (with-temp-buffer
      (ses-mode)
      (ses-set :: rcv
               0 0 1; A1 := 1
               0 1 2; B1 := 2
               :: rcf
               1 0 A2-form; A2 := A1 + B1
               1 1 B2-form; B2 := B1 + A2
               )
      (should (eq A1 1))
      (should (eq B1 2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      (should (eq A2 2))
      (should (eq B2 4)))))


(ert-deftest ses-set-sv-sfq ()
  "Set values and formulas, symbols expressions."
  (let ((ses-initial-size '(4 . 3))
        (ses-after-entry-functions nil)
        (A1-sym 'A1)
        (A2-sym 'A2))
    (with-temp-buffer
      (ses-mode)
      (ses-set
       :: sv
       A1-sym 1; A1 := 1
       'B1 2; B1 := 2
       :: sf
       A2-sym '(+ A1 B1); A2 := A1 + B1
       :: sfq
       'B2    (+ B1 A2); B2 := B1 + A2
       )
      (should (eq A1 1))
      (should (eq B1 2))
      (should (eq A2 3))
      (should (eq B2 5))
      (ses-set A1 0)
      (should (eq A1 0))
      (should (eq A2 2))
      (should (eq B2 4)))))

(ert-deftest ses-range-reading-directions ()
  "Test ses-range with reading directions"
  (let ((ses-initial-size '(5 . 3))
        (ses-initial-default-printer "%S")
        (ses-after-entry-functions nil))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 "A1"); A1
                   (1 0 "A2"); A2
                   (0 1 "B1"); B1
                   (1 1 "B2"); B2
                   (2 0 (apply 'concat (ses-range A1 B1 <))); A3
                   (2 1 (apply 'concat (ses-range A1 B1 >))); B3
                   (2 2 (apply 'concat (ses-range A1 B2 >v))); C3
                   (3 0 (apply 'concat (ses-range A1 B2 >^))); A4
                   (3 1 (apply 'concat (ses-range A1 B2 <v))); B4
                   (3 2 (apply 'concat (ses-range A1 B2 <^))); C4

                   (0 2 (apply 'concat (ses-range A1 B2 v>))); C1
                   (1 2 (apply 'concat (ses-range A1 B2 ^>))); C2
                   (4 0 (apply 'concat (ses-range A1 B2 v<))); A5
                   (4 1 (apply 'concat (ses-range A1 B2 ^<))); B5
                   ))
        (apply 'ses-cell-set-formula c)
        (apply 'ses-calculate-cell (list (car c) (cadr c) nil)))
      (ses-recalculate-all)
      (should (string= A3 "B1A1"))
      (should (string= B3 "A1B1"))

      (should (string= C3 "A1B1A2B2"))
      (should (string= A4 "A2B2A1B1"))
      (should (string= B4 "B1A1B2A2"))
      (should (string= C4 "B2A2B1A1"))
      
      (should (string= C1 "A1A2B1B2"))
      (should (string= C2 "A2A1B2B1"))
      (should (string= A5 "B1B2A1A2"))
      (should (string= B5 "B2B1A2A1"))
      )))

(provide 'ses-tests)

;;; ses-tests.el ends here
