;;; puny-tests.el --- tests for net/puny.el  -*- coding: utf-8; -*-

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
(require 'puny)

(ert-deftest puny-test-encode ()
  "Test puny encoding."
  (should (string= (puny-encode-string "bücher") "xn--bcher-kva")))

(ert-deftest puny-test-decode ()
  "Test puny decoding."
  (should (string= (puny-decode-string "xn--bcher-kva") "bücher")))

(ert-deftest puny-test-encode2 ()
  "Test puny encoding."
  (should (string= (puny-encode-string "חנוך") "xn--9dbdkw")))

(ert-deftest puny-test-decode2 ()
  "Test puny decoding."
  (should (string= (puny-decode-string "xn--9dbdkw") "חנוך")))

;;; puny-tests.el ends here
