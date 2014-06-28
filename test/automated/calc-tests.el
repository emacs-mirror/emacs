;;; calc-tests.el --- tests for calc                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: maint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'calc)

(ert-deftest test-math-bignum ()
  ;; bug#17556
  (let ((n (math-bignum most-negative-fixnum)))
    (should (math-negp n))
    (should (cl-notany #'cl-minusp (cdr n)))))

(provide 'calc-tests)
;;; calc-tests.el ends here

;; Local Variables:
;; bug-reference-url-format: "http://debbugs.gnu.org/%s"
;; End:
