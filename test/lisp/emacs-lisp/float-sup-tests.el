;;; float-sup-tests.el --- Tests for float-sup.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

(ert-deftest float-sup-degrees-and-radians ()
  (should (equal (degrees-to-radians 180.0) float-pi))
  (should (equal (radians-to-degrees float-pi) 180.0))
  (should (equal (radians-to-degrees (degrees-to-radians 360.0)) 360.0))
  (should (equal (degrees-to-radians (radians-to-degrees float-pi)) float-pi)))

(provide 'float-sup-tests)
;;; float-sup-tests.el ends here
