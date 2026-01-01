;;; cl-tests.el --- tests for emacs-lisp/cl.el  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

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

(with-no-warnings
  (require 'cl))
(require 'ert)

(ert-deftest labels-function-quoting ()
  "Test that #'foo does the right thing in `labels'." ; Bug#31792.
  (with-suppressed-warnings ((obsolete labels))
    (should (eq (funcall (labels ((foo () t))
                                 #'foo))
                t))))

;;; cl-tests.el ends here
