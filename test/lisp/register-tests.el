;;; register-tests.el --- tests for register.el  -*- lexical-binding: t-*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
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


;;; Code:
(require 'ert)
(require 'cl-lib)

(ert-deftest register-test-bug27634 ()
  "Test for https://debbugs.gnu.org/27634 ."
  (dolist (event (list ?\C-g 'escape ?\C-\[))
    (cl-letf (((symbol-function 'read-key) #'ignore)
              (last-input-event event)
              (register-alist nil))
      (should (equal 'quit
                     (condition-case err
                         (call-interactively 'point-to-register)
                       (quit (car err)))))
      (should-not register-alist))))

(provide 'register-tests)
;;; register-tests.el ends here
