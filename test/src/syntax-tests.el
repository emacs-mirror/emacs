;;; syntax-tests.el --- tests for syntax.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

(ert-deftest parse-partial-sexp-continue-over-comment-marker ()
  "Continue a parse that stopped in the middle of a comment marker."
  (with-temp-buffer
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?/ ". 124")
      (modify-syntax-entry ?* ". 23b")
      (set-syntax-table table))
    (insert "/*C*/\nX")
    (goto-char (point-min))
    (let* ((pointC (progn (search-forward "C") (1- (point))))
           (preC (1- pointC))
           (pointX (progn (search-forward "X") (1- (point))))
           (aftC (+ 2 pointC))
           (ppsC (parse-partial-sexp (point-min) pointC))
           (pps-preC (parse-partial-sexp (point-min) preC))
           (pps-aftC (parse-partial-sexp (point-min) aftC))
           (ppsX (parse-partial-sexp (point-min) pointX)))
      ;; C should be inside comment.
      (should (= (nth 0 ppsC) 0))
      (should (eq (nth 4 ppsC) t))
      (should (= (nth 8 ppsC) (- pointC 2)))
      ;; X should not be in comment or list.
      (should (= (nth 0 ppsX) 0))
      (should-not (nth 4 ppsX))
      ;; Try using OLDSTATE.
      (should (equal (parse-partial-sexp preC pointC nil nil pps-preC)
                     ppsC))
      (should (equal (parse-partial-sexp pointC aftC nil nil ppsC)
                     pps-aftC))
      (should (equal (parse-partial-sexp preC aftC nil nil pps-preC)
                     pps-aftC))
      (should (equal (parse-partial-sexp aftC pointX nil nil pps-aftC)
                     ppsX)))))

(ert-deftest parse-partial-sexp-paren-comments ()
  "Test syntax parsing with paren comment markers.
Specifically, where the first character of the comment marker is
also has open paren syntax (see Bug#24870)."
  (with-temp-buffer
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?\{  "(}1nb" table)
      (modify-syntax-entry ?\}  "){4nb" table)
      (modify-syntax-entry ?-  ". 123" table)
      (set-syntax-table table))
    (insert "{-C-}\nX")
    (goto-char (point-min))
    (let* ((pointC (progn (search-forward "C") (1- (point))))
           (pointX (progn (search-forward "X") (1- (point))))
           (ppsC (parse-partial-sexp (point-min) pointC))
           (ppsX (parse-partial-sexp (point-min) pointX)))
      ;; C should be inside nestable comment, not list.
      (should (= (nth 0 ppsC) 0))
      (should (= (nth 4 ppsC) 1))
      (should (= (nth 8 ppsC) (- pointC 2)))
      ;; X should not be in comment or list.
      (should (= (nth 0 ppsX) 0))
      (should-not (nth 4 ppsX))
      ;; Try using OLDSTATE.
      (should (equal (parse-partial-sexp pointC pointX nil nil ppsC)
                     ppsX)))))

;;; syntax-tests.el ends here
