;;; edmacro-tests.el --- Tests for edmacro.el  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

(require 'ert)
(require 'edmacro)

(ert-deftest edmacro-test-edmacro-parse-keys ()
  (should (equal (edmacro-parse-keys "") []))
  (should (equal (edmacro-parse-keys "x") [?x]))
  (should (equal (edmacro-parse-keys "C-a") [?\C-a]))

  ;; comments
  (should (equal (edmacro-parse-keys ";; foobar") []))
  (should (equal (edmacro-parse-keys ";;;") []))
  (should (equal (edmacro-parse-keys "; ; ;") [?\; ?\; ?\;]))
  (should (equal (edmacro-parse-keys "REM foobar") []))
  (should (equal (edmacro-parse-keys "x ;; foobar") [?x]))
  (should (equal (edmacro-parse-keys "x REM foobar") [?x]))
  (should (equal (edmacro-parse-keys "<<goto-line>>")
                 [?\M-x ?g ?o ?t ?o ?- ?l ?i ?n ?e ?\r]))

  ;; repetitions
  (should (equal (edmacro-parse-keys "3*x") [?x ?x ?x]))
  (should (equal (edmacro-parse-keys "3*C-m") [?\C-m ?\C-m ?\C-m]))
  (should (equal (edmacro-parse-keys "10*foo")
                 (apply #'vconcat (make-list 10 [?f ?o ?o])))))

;;; edmacro-tests.el ends here
