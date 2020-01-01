;;; ses-tests.el --- Tests for ses.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
(require 'ses)


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

(defvar ses--foo)
(defvar ses--cells)

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
renaming A1 to `ses--foo' makes `ses--foo' value equal to 2."
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

(defvar ses--bar)

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


(provide 'ses-tests)
