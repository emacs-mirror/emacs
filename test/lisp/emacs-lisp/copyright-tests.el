;;; copyright-tests.el --- tests for copyright.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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
(require 'cl-lib)
(require 'copyright)

(defmacro with-copyright-test (orig result)
  `(cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2019")))
     (let ((copyright-query nil)
           (copyright-current-year 2019))
       (with-temp-buffer
         (insert ,orig)
         (copyright-update)
         (should (equal (buffer-string) ,result))))))

(defvar copyright-tests--data
  '((";; Copyright (C) 2017 Free Software Foundation, Inc."
     . ";; Copyright (C) 2017, 2019 Free Software Foundation, Inc.")
    (";; Copyright (C) 2017-2018 Free Software Foundation, Inc."
     . ";; Copyright (C) 2017-2019 Free Software Foundation, Inc.")
    (";; Copyright (C) 2005-2006, 2015, 2017-2018 Free Software Foundation, Inc."
     . ";; Copyright (C) 2005-2006, 2015, 2017-2019 Free Software Foundation, Inc.")
    (";; copyright '18 FSF"
     . ";; copyright '18, '19 FSF")))

(ert-deftest test-copyright-update ()
  (dolist (test copyright-tests--data)
    (with-copyright-test (car test) (cdr test))))

(provide 'copyright-tests)
;;; copyright-tests.el ends here
