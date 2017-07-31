;;; rx-tests.el --- test for rx.el functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

(require 'ert)
(require 'rx)

;;; Code:

(ert-deftest rx-char-any ()
  "Test character alternatives with `\]' and `-' (Bug#25123)."
  (should (string-match
           (rx string-start (1+ (char (?\] . ?\{) (?< . ?\]) (?- . ?:)))
               string-end)
           (apply #'string (nconc (number-sequence ?\] ?\{)
                                  (number-sequence ?< ?\])
                                  (number-sequence ?- ?:))))))

(ert-deftest rx-pcase ()
  (should (equal (pcase "a 1 2 3 1 1 b"
                   ((rx (let u (+ digit)) space
                        (let v (+ digit)) space
                        (let v (+ digit)) space
                        (backref u) space
                        (backref 1))
                    (list u v)))
                 '("1" "3"))))

(provide 'rx-tests)
;; rx-tests.el ends here.
