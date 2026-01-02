;;; sqlite-tests.el --- Tests for sqlite.el  -*- lexical-binding: t; -*-

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
(require 'sqlite)

(ert-deftest with-sqlite-transaction ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open)))
    (sqlite-execute db "create table test (a)")
    (should
     (eql 42 (with-sqlite-transaction db
               (sqlite-execute db "insert into test values (1)")
               (should (equal '((1)) (sqlite-select db "select * from test")))
               42)))
    ;; Body runs exactly once.
    (should (equal '((1)) (sqlite-select db "select * from test")))))

(ert-deftest with-sqlite-transaction/rollback ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open)))
    (sqlite-execute db "create table test (a)")
    (should (equal '(sqlite-error
                     ("SQL logic error" "no such function: fake" 1 1))
                   (should-error
                    (with-sqlite-transaction db
                      (sqlite-execute db "insert into test values (1)")
                      (sqlite-execute db "insert into test values (fake(2))")
                      42))))
    ;; First insertion (a=1) rolled back.
    (should-not (sqlite-select db "select * from test"))))

;;; sqlite-tests.el ends here
