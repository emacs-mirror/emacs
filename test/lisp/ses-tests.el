;;; ses-tests.el --- Tests for ses.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

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
                           (lexical A2)
                           (lexical A3)
                           (lexical foo)
                           (lexical bar)
                           (lexical B2)
                           (lexical toto))
  (defvar ses--cells)
  (defvar A2)
  (defvar A3)
  (defvar foo)
  (defvar bar)
  (defvar B2)
  (defvar toto))

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
  "Check that renaming A1 to `foo' and setting `foo' to 1 and A2 to (1+ foo), makes A2 value equal to 2.
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
        (replace-match "foo" t t))
      (ses-mode)
      (should-not  (local-variable-p 'A1))
      (should (eq foo 1))
      (should (equal (ses-cell-formula 1 0) '(ses-safe-formula (1+ foo))))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-renamed-cell ()
  "Check that renaming A1 to `foo' and setting `foo' to 1 and A2
to (1+ foo), makes A2 value equal to 2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (ses-rename-cell 'foo (ses-get-cell 0 0))
      (dolist (c '((0 0 1) (1 0 (1+ foo))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook)
      (should-not  (local-variable-p 'A1))
      (should (eq foo 1))
      (should (equal (ses-cell-formula 1 0) '(1+ foo)))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-renamed-cell-after-setting ()
  "Check that setting A1 to 1 and A2 to (1+ A1), and then
renaming A1 to `foo' makes `foo' value equal to 2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook); deferred recalc
      (ses-rename-cell 'foo (ses-get-cell 0 0))
      (should-not  (local-variable-p 'A1))
      (should (eq foo 1))
      (should (equal (ses-cell-formula 1 0) '(1+ foo)))
      (should (eq (bound-and-true-p A2) 2)))))

(ert-deftest ses-tests-renaming-cell-with-one-symbol-formula ()
  "Check that setting A1 to 1 and A2 to A1, and then renaming A1
to `foo' makes `foo' value equal to 1. Then set A1 to 2 and check
that `foo' becomes 2."
  (let ((ses-initial-size '(3 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 A1)))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook); deferred recalc
      (ses-rename-cell 'foo (ses-get-cell 0 0))
      (ses-command-hook); deferred recalc
      (should-not  (local-variable-p 'A1))
      (should (eq foo 1))
      (should (equal (ses-cell-formula 1 0) 'foo))
      (should (eq (bound-and-true-p A2) 1))
      (funcall-interactively 'ses-edit-cell 0 0 2)
      (ses-command-hook); deferred recalc
      (should (eq (bound-and-true-p A2) 2))
      (should (eq foo 2)))))


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
  "Check that setting A1 to 1 and A2 to (1+ A1), and then renaming A1 to `foo' and A2 to `bar' jumping
to `bar' and inserting a row, makes A2 value empty, and `bar' equal to
2."
  (let ((ses-initial-size '(2 . 1)))
    (with-temp-buffer
      (ses-mode)
      (dolist (c '((0 0 1) (1 0 (1+ A1))))
        (apply 'funcall-interactively 'ses-edit-cell c))
      (ses-command-hook)
      (ses-rename-cell 'foo (ses-get-cell 0 0))
      (ses-command-hook)
      (ses-rename-cell 'bar (ses-get-cell 1 0))
      (ses-command-hook)
      (should (eq bar 2))
      (ses-jump 'bar)
      (ses-insert-row 1)
      (ses-command-hook)
      (should-not (bound-and-true-p A2))
      (should (eq bar 2)))))


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
  "Test jumping to cell B2 by use of lowcase cell name string"
    (let ((ses-initial-size '(3 . 3))
          ses-after-entry-functions)
      (with-temp-buffer
        (ses-mode)
        (funcall-interactively 'ses-jump "b2")
        (ses-command-hook)
        (should (eq (ses--cell-at-pos (point)) 'B2)))))

(ert-deftest ses-jump-B2-lowcase-keys ()
  "Test jumping to cell B2 by use of lowcase cell name string with simulating keys"
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
  "Test jumping to cell B2 after renaming it `toto'."
  (let ((ses-initial-size '(3 . 3))
        ses-after-entry-functions)
    (with-temp-buffer
      (ses-mode)
      (ses-rename-cell 'toto (ses-get-cell 1 1))
      (ses-jump 'toto)
      (ses-command-hook)
      (should (eq (ses--cell-at-pos (point)) 'toto)))))

(provide 'ses-tests)

;;; ses-tests.el ends here
