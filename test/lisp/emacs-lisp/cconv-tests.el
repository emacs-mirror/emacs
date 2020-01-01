;;; cconv-tests.el -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

(require 'ert)

(ert-deftest cconv-convert-lambda-lifted ()
  "Bug#30872."
  (should
   (equal (funcall
           (byte-compile
            '#'(lambda (handle-fun arg)
                 (let* ((subfun
                         #'(lambda (params)
                             (ignore handle-fun)
                             (funcall #'(lambda () (setq params 42)))
                             params)))
                   (funcall subfun arg))))
           nil 99)
          42)))

(provide 'cconv-tests)
;; cconv-tests.el ends here.
