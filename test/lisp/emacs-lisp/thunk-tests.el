;;; thunk-tests.el --- Tests for thunk.el -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Maintainer: emacs-devel@gnu.org

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

;; Tests for thunk.el

;;; Code:

(require 'ert)
(require 'thunk)

(ert-deftest thunk-should-be-lazy ()
  (let (x)
    (thunk-delay (setq x t))
    (should (null x))))

(ert-deftest thunk-can-be-evaluated ()
  (let* (x
         (thunk (thunk-delay (setq x t))))
    (should-not (thunk-evaluated-p thunk))
    (should (null x))
    (thunk-force thunk)
    (should (thunk-evaluated-p thunk))
    (should x)))

(ert-deftest thunk-evaluation-is-cached ()
  (let* ((x 0)
        (thunk (thunk-delay (setq x (1+ x)))))
    (thunk-force thunk)
    (should (= x 1))
    (thunk-force thunk)
    (should (= x 1))))

(provide 'thunk-tests)
;;; thunk-tests.el ends here
