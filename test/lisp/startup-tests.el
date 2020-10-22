;;; startup-tests.el --- unit tests for startup.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for startup.el.

;;; Code:

(ert-deftest startup-tests/command-switch-alist ()
  (let* ((foo-args ()) (bar-args ())
         (command-switch-alist
          (list (cons "--foo"
                      (lambda (arg)
                        (ert-info ("Processing argument --foo")
                          (push arg foo-args)
                          (should (equal command-line-args-left
                                         '("value" "--bar=value")))
                          (pop command-line-args-left))))
                (cons "--bar=value"
                      (lambda (arg)
                        (ert-info ("Processing argument --bar")
                          (push arg bar-args)
                          (should-not command-line-args-left)))))))
    (command-line-1 '("--foo" "value" "--bar=value"))
    (should (equal foo-args '("--foo")))
    (should (equal bar-args '("--bar=value")))))

;;; startup-tests.el ends here
