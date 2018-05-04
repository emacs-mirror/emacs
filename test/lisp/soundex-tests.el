;;; soundex-tests.el --- tests for soundex.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test `soundex-test-names' originally adapted from code in
;; soundex.el by Christian Plaunt <chris@bliss.berkeley.edu>

;;; Code:

(require 'ert)
(require 'soundex)

(defconst soundex-test-name-list
 '("Euler" "Gauss" "Hilbert" "Knuth" "Lloyd" "Lukasiewicz"
   "Ellery" "Ghosh" "Heilbronn" "Kant" "Ladd" "Lissajous")
 "Knuth's names to demonstrate the Soundex algorithm.")

(ert-deftest soundex-test-names ()
  (should
   (equal (mapcar #'soundex soundex-test-name-list)
          '("E460" "G200" "H416" "K530" "L300" "L222"
            "E460" "G200" "H416" "K530" "L300" "L222"))))

;;; soundex-tests.el ends here
