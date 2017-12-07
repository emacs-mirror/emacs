;;; rot13-tests.el --- Tests for rot13.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'rot13)

(ert-deftest rot13-tests-rot13 ()
  (should (equal (rot13 "Super-secret text") "Fhcre-frperg grkg"))
  (with-temp-buffer
    (insert "Super-secret text")
    (rot13 (current-buffer) (point-min) (point-max))
    (should (equal (buffer-string) "Fhcre-frperg grkg"))
    (rot13 (current-buffer) (point-min) (+ (point-min) 5))
    (should (equal (buffer-string) "Super-frperg grkg"))))

(ert-deftest rot13-tests-rot13-string ()
  (should (equal (rot13-string "") ""))
  (should (equal (rot13-string (rot13-string "foo")) "foo"))
  (should (equal (rot13-string "Super-secret text")
                 "Fhcre-frperg grkg")))

(ert-deftest rot13-tests-rot13-region ()
  (with-temp-buffer
    (insert "Super-secret text")
    (rot13-region (+ (point-min) 6) (+ (point-min) 12))
    (should (equal (buffer-string) "Super-frperg text"))))

(provide 'rot13-tests)
;;; rot13-tests.el ends here
