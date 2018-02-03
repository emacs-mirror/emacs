;;; abbrev-tests.el --- Test suite for abbrevs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>
;; Keywords: abbrevs

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

;; `kill-all-abbrevs-test' will remove all user *and* system abbrevs
;; if called noninteractively with the init file loaded.

;;; Code:

(require 'ert)
(require 'abbrev)
(require 'seq)

;; set up test abbrev table and abbrev entry
(defun setup-test-abbrev-table ()
  (defvar ert-test-abbrevs nil)
  (define-abbrev-table 'ert-test-abbrevs '(("a-e-t" "abbrev-ert-test")))
  (abbrev-table-put ert-test-abbrevs :ert-test "ert-test-value")
  ert-test-abbrevs)

(ert-deftest abbrev-table-p-test ()
  (should-not (abbrev-table-p 42))
  (should-not (abbrev-table-p "aoeu"))
  (should-not (abbrev-table-p '()))
  (should-not (abbrev-table-p []))
  ;; Missing :abbrev-table-modiff counter:
  (should-not (abbrev-table-p (obarray-make)))
  (should (abbrev-table-empty-p (make-abbrev-table))))

(ert-deftest abbrev-make-abbrev-table-test ()
  ;; Table without properties:
  (let ((table (make-abbrev-table)))
    (should (abbrev-table-p table))
    (should (= (length table) obarray-default-size)))
  ;; Table with one property 'foo with value 'bar:
  (let ((table (make-abbrev-table '(foo bar))))
    (should (abbrev-table-p table))
    (should (= (length table) obarray-default-size))
    (should (eq (abbrev-table-get table 'foo) 'bar))))

(ert-deftest abbrev-table-get-put-test ()
  (let ((table (make-abbrev-table)))
    (should-not (abbrev-table-get table 'foo))
    (should (= (abbrev-table-put table 'foo 42) 42))
    (should (= (abbrev-table-get table 'foo) 42))
    (should (eq (abbrev-table-put table 'foo 'bar) 'bar))
    (should (eq (abbrev-table-get table 'foo) 'bar))))

(ert-deftest copy-abbrev-table-test ()
  (defvar foo-abbrev-table nil)         ; Avoid compiler warning
  (define-abbrev-table 'foo-abbrev-table
    '())
  (should (abbrev-table-p foo-abbrev-table))
  ;; Bug 21828
  (let ((new-foo-abbrev-table
         (condition-case nil
             (copy-abbrev-table foo-abbrev-table)
           (error nil))))
    (should (abbrev-table-p new-foo-abbrev-table)))
  (should-not (string-equal (buffer-name) "*Backtrace*")))

(ert-deftest abbrev-table-empty-p-test ()
  (should-error (abbrev-table-empty-p 42))
  (should-error (abbrev-table-empty-p "aoeu"))
  (should-error (abbrev-table-empty-p '()))
  (should-error (abbrev-table-empty-p []))
  ;; Missing :abbrev-table-modiff counter:
  (should-error (abbrev-table-empty-p (obarray-make)))
  (let* ((table (obarray-make)))
    (abbrev-table-put table :abbrev-table-modiff 42)
    (should (abbrev-table-empty-p table))))

(ert-deftest kill-all-abbrevs-test ()
  "Test undefining all defined abbrevs"
  (unless noninteractive
    (ert-skip "Cannot test kill-all-abbrevs in interactive mode"))

  (let ((num-tables 0))
    ;; ensure at least one abbrev exists
    (should (abbrev-table-p (setup-test-abbrev-table)))
    (setf num-tables (length abbrev-table-name-list))
    (kill-all-abbrevs)

    ;; no tables should have been removed/added
    (should (= num-tables (length abbrev-table-name-list)))
    ;; number of empty tables should be the same as number of tables
    (should (= num-tables (length (seq-filter
                                   (lambda (table)
                                       (abbrev-table-empty-p (symbol-value table)))
                                   abbrev-table-name-list))))))

(ert-deftest abbrev-table-name-test ()
  "Test returning name of abbrev-table"
  (let ((ert-test-abbrevs (setup-test-abbrev-table))
        (no-such-table nil))
    (should (equal 'ert-test-abbrevs (abbrev-table-name ert-test-abbrevs)))
    (should (equal nil (abbrev-table-name no-such-table)))))

(ert-deftest clear-abbrev-table-test ()
  "Test clearing single abbrev table"
  (let ((ert-test-abbrevs (setup-test-abbrev-table)))
    (should (equal "abbrev-ert-test" (abbrev-expansion "a-e-t" ert-test-abbrevs)))
    (clear-abbrev-table ert-test-abbrevs)
    (should (equal nil (abbrev-expansion "a-e-t" ert-test-abbrevs)))
    (should (equal t (abbrev-table-empty-p ert-test-abbrevs)))))

(ert-deftest list-abbrevs-test ()
  "Test generation of abbrev list buffer"
  ;; Somewhat redundant as prepare-abbrev-list-buffer is also tested.
  ;; all abbrevs
  (let ((abbrev-buffer (prepare-abbrev-list-buffer)))
    (should (equal "*Abbrevs*" (buffer-name abbrev-buffer)))
    (kill-buffer abbrev-buffer))
  ;; mode-specific abbrevs
  (let ((abbrev-buffer (prepare-abbrev-list-buffer t)))
    (should (equal "*Abbrevs*" (buffer-name abbrev-buffer)))
    (kill-buffer abbrev-buffer)))

(ert-deftest prepare-abbrev-list-buffer-test ()
  "Test generation of abbrev list buffer"
  ;; all abbrevs
  (let ((ert-test-abbrevs (setup-test-abbrev-table)))
    (with-current-buffer (prepare-abbrev-list-buffer)
      ;; Check for a couple of abbrev-table names in buffer.
      (should (and (progn
                     (goto-char (point-min))
                     (search-forward (symbol-name (abbrev-table-name ert-test-abbrevs))))
                   (progn
                     (goto-char (point-min))
                     (search-forward "global-abbrev-table"))))
      (should (equal 'edit-abbrevs-mode major-mode))
      (kill-buffer "*Abbrevs*")))

  ;; mode-specific abbrevs (temp buffer uses fundamental-mode)
  (with-temp-buffer
    (prepare-abbrev-list-buffer t)
    (with-current-buffer "*Abbrevs*"
      (should (progn
                (goto-char (point-min))
                (search-forward "fundamental-mode-abbrev-table")))
      (should-error (progn
                      (goto-char (point-min))
                      (search-forward "global-abbrev-table")))
      (should-not (equal 'edit-abbrevs-mode major-mode))
      (kill-buffer "*Abbrevs*"))))

(ert-deftest insert-abbrevs-test ()
  "Test inserting abbrev definitions into buffer"
  (with-temp-buffer
    (insert-abbrevs)
      (should (progn
                (goto-char (point-min))
                (search-forward "global-abbrev-table")))))

(ert-deftest edit-abbrevs-test ()
  "Test editing abbrevs from buffer"
  (defvar ert-edit-abbrevs-test-table nil)
  (let ((ert-test-abbrevs (setup-test-abbrev-table)))
    (with-temp-buffer
      ;; insert test table and new abbrev, redefine, check definition
      (goto-char (point-min))
      (insert "(ert-edit-abbrevs-test-table)\n")
      (insert "\n" "\"e-a-t\"\t" "0\t" "\"edit-abbrevs-test\"\n")
      ;; check test table before redefine
      (should (equal "abbrev-ert-test"
                     (abbrev-expansion "a-e-t" ert-test-abbrevs)))
      (edit-abbrevs-redefine)
      (should-not (abbrev-expansion "a-e-t" ert-test-abbrevs))
      (should (equal "edit-abbrevs-test"
                     (abbrev-expansion "e-a-t" ert-edit-abbrevs-test-table))))))

(ert-deftest define-abbrevs-test ()
  "Test defining abbrevs from buffer"
  (defvar ert-bad-abbrev-table nil)
  (defvar ert-good-abbrev-table nil)
  (defvar ert-redefine-abbrev-table nil)
  (with-temp-buffer
    ;; insert bad abbrev data and attempt define
    (goto-char (point-min))
    (insert "ert-bad-abbrev-table\n")
    (insert "\n" "\"b-a-t\"\t" "0\t" "\n")
    (should-not (define-abbrevs))
    (should (equal nil (abbrev-expansion "b-a-t" ert-bad-abbrev-table)))
    (delete-region (point-min) (point-max))
    ;; try with valid abbrev data
    (goto-char (point-min))
    (insert "(ert-good-abbrev-table)\n")
    (insert "\n" "\"g-a-t\"\t" "0\t" "\"good-abbrev-table\"\n")
    (define-abbrevs)
    (should (equal "good-abbrev-table"
                   (abbrev-expansion "g-a-t" ert-good-abbrev-table)))
    ;; redefine from buffer
    (delete-region (point-min) (point-max))
    (insert "(ert-redefine-abbrev-table)\n")
    (insert "\n" "\"r-a-t\"\t" "0\t" "\"redefine-abbrev-table\"\n")
    ;; arg = kill-all-abbrevs
    (define-abbrevs t)
    (should (equal "redefine-abbrev-table"
                   (abbrev-expansion "r-a-t" ert-redefine-abbrev-table)))
    (should (equal nil (abbrev-expansion "g-a-t" ert-good-abbrev-table)))))

(ert-deftest read-write-abbrev-file-test ()
  "Test reading and writing abbrevs from file"
  (let ((temp-test-file (make-temp-file "ert-abbrev-test"))
        (ert-test-abbrevs (setup-test-abbrev-table)))
    (write-abbrev-file temp-test-file)
    (clear-abbrev-table ert-test-abbrevs)
    (should (abbrev-table-empty-p ert-test-abbrevs))
    (read-abbrev-file temp-test-file)
    (should (equal "abbrev-ert-test" (abbrev-expansion "a-e-t" ert-test-abbrevs)))
    (delete-file temp-test-file)))

(ert-deftest abbrev-edit-save-to-file-test ()
  "Test saving abbrev definitions in buffer to file"
  (defvar ert-save-test-table nil)
  (let ((temp-test-file (make-temp-file "ert-abbrev-test"))
        (ert-test-abbrevs (setup-test-abbrev-table)))
    (with-temp-buffer
      (goto-char (point-min))
      (insert "(ert-save-test-table)\n")
      (insert "\n" "\"s-a-t\"\t" "0\t" "\"save-abbrevs-test\"\n")
      (should (equal "abbrev-ert-test"
                     (abbrev-expansion "a-e-t" ert-test-abbrevs)))
      ;; clears abbrev tables
      (abbrev-edit-save-to-file temp-test-file)
      (should-not (abbrev-expansion "a-e-t" ert-test-abbrevs))
      (read-abbrev-file temp-test-file)
      (should (equal "save-abbrevs-test"
                     (abbrev-expansion "s-a-t" ert-save-test-table)))
      (delete-file temp-test-file))))

(provide 'abbrev-tests)

;;; abbrev-tests.el ends here
