;;; rmc-tests.el --- Test suite for rmc.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'rmc)
(eval-when-compile (require 'cl-lib))


(ert-deftest test-read-multiple-choice ()
  (dolist (char '(?y ?n))
    (cl-letf* (((symbol-function #'read-char) (lambda () char))
               (str (if (eq char ?y) "yes" "no")))
      (should (equal (list char str)
                     (read-multiple-choice "Do it? " '((?y "yes") (?n "no"))))))))


(provide 'rmc-tests)
;;; rmc-tests.el ends here
