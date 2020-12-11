;;; memory-report-tests.el --- tests for memory-report.el              -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

(require 'ert)
(require 'memory-report)

(defun setup-memory-report-tests ()
  (memory-report--set-size
   '((conses 16 499173 99889)
     (symbols 48 22244 3)
     (strings 32 92719 4559)
     (string-bytes 1 40402011)
     (vectors 16 31919)
     (vector-slots 8 385148 149240)
     (floats 8 434 4519)
     (intervals 56 24499 997)
     (buffers 984 33))))

(ert-deftest memory-report-sizes ()
  (setup-memory-report-tests)
  (should (equal (memory-report-object-size (cons nil nil)) 16))
  (should (equal (memory-report-object-size (cons 1 2)) 16))

  (should (equal (memory-report-object-size (list 1 2)) 32))
  (should (equal (memory-report-object-size (list 1)) 16))

  (should (equal (memory-report-object-size (list 'foo)) 16))

  (should (equal (memory-report-object-size (vector 1 2 3 4)) 80))

  (should (equal (memory-report-object-size "") 32))
  (should (equal (memory-report-object-size "a") 33))
  (should (equal (memory-report-object-size (propertize "a" 'face 'foo))
                 81)))

(provide 'memory-report-tests)

;;; memory-report-tests.el ends here
