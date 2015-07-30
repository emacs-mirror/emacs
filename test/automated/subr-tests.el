;;; subr-tests.el --- Tests for subr.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>,
;;         Nicolas Petton <nicolas@petton.fr>
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)

(ert-deftest let-when-compile ()
  ;; good case
  (should (equal (macroexpand '(let-when-compile ((foo (+ 2 3)))
                                (setq bar (eval-when-compile (+ foo foo)))
                                (setq boo (eval-when-compile (* foo foo)))))
                 '(progn
                   (setq bar (quote 10))
                   (setq boo (quote 25)))))
  ;; bad case: `eval-when-compile' omitted, byte compiler should catch this
  (should (equal (macroexpand
                  '(let-when-compile ((foo (+ 2 3)))
                    (setq bar (+ foo foo))
                    (setq boo (eval-when-compile (* foo foo)))))
                 '(progn
                   (setq bar (+ foo foo))
                   (setq boo (quote 25)))))
  ;; something practical
  (should (equal (macroexpand
                  '(let-when-compile ((keywords '("true" "false")))
                    (font-lock-add-keywords
                     'c++-mode
                     `((,(eval-when-compile
                           (format "\\<%s\\>" (regexp-opt keywords)))
                         0 font-lock-keyword-face)))))
                 '(font-lock-add-keywords
                   (quote c++-mode)
                   (list
                    (cons (quote
                           "\\<\\(?:\\(?:fals\\|tru\\)e\\)\\>")
                     (quote
                      (0 font-lock-keyword-face))))))))

(ert-deftest string-comparison-test ()
  (should (string-lessp "abc" "acb"))
  (should (string-lessp "aBc" "abc"))
  (should (string-lessp "abc" "abcd"))
  (should (string-lessp "abc" "abcd"))
  (should-not (string-lessp "abc" "abc"))
  (should-not (string-lessp "" ""))

  (should (string-greaterp "acb" "abc"))
  (should (string-greaterp "abc" "aBc"))
  (should (string-greaterp "abcd" "abc"))
  (should (string-greaterp "abcd" "abc"))
  (should-not (string-greaterp "abc" "abc"))
  (should-not (string-greaterp "" ""))

  ;; Symbols are also accepted
  (should (string-lessp 'abc 'acb))
  (should (string-lessp "abc" 'acb))
  (should (string-greaterp 'acb 'abc))
  (should (string-greaterp "acb" 'abc)))

(provide 'subr-tests)
;;; subr-tests.el ends here
