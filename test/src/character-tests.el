;;; character-tests.el --- tests for character.c  -*- lexical-binding:t -*-

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

;;; Code:

(require 'ert)

(ert-deftest character-test-string-width ()
  "Test `string-width' with and without compositions."
  (should (= (string-width "1234") 4))
  (should (= (string-width "12\t34") (+ 4 tab-width)))
  (should (= (string-width "áëòç") 4))
  (should (= (string-width "áëòç") 4))
  (should (= (string-width "הַרְבֵּה אַהֲבָה") 9))
  (should (= (string-width "1234" 1 3) 2))
  (should (= (string-width "1234" nil -1) 3))
  (should (= (string-width "1234" 2) 2))
  (should-error (string-width "1234" nil 5))
  (should-error (string-width "1234" -5))
  (should (= (string-width "12\t34") (+ 4 tab-width)))
  (should (= (string-width "1234\t56") (+ 6 tab-width)))
  (should (= (string-width "áëòç") 4))
  (should (= (string-width "áëòç" nil 3) 3))
  (should (= (string-width "áëòç" 1 3) 2))
  (should (= (string-width "áëòç" nil 2) 1))
  (should (= (string-width "áëòç" nil 3) 2))
  (should (= (string-width "áëòç" nil 4) 2))
  (should (= (string-width "הַרְבֵּה אַהֲבָה") 9))
  (should (= (string-width "הַרְבֵּה אַהֲבָה" nil 8) 4)))

;;; character-tests.el ends here
