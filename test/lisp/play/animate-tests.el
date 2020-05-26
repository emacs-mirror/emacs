;;; animate-tests.el --- Tests for animate.el  -*- lexical-binding:t -*-

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
(require 'animate)

(ert-deftest animate-test-birthday-present ()
  (unwind-protect
      (save-window-excursion
        (cl-letf (((symbol-function 'sit-for) (lambda (_) nil)))
          (animate-birthday-present "foo")
          (should (equal (buffer-string)
                         "





Happy Birthday,
   Foo


                              You are my sunshine,
                              My only sunshine.
                              I'm awful sad that
                              You've moved away.

                              Let's talk together
                              And love more deeply.
                              Please bring back
                                  my sunshine
                                  to stay!"))))
    (kill-buffer "*A-Present-for-Foo*")))

(provide 'animate-tests)
;;; animate-tests.el ends here
