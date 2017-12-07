;;; ediff-diff-tests.el --- Unit tests for ediff-diff.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

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

;; Unit tests for lisp/vc/ediff-diff.el.

;;; Code:

(require 'ediff-diff)

(require 'cl-lib)
(require 'ert)

(ert-deftest ediff-diff-tests--ediff-exec-process--quoted-file ()
  "Check that Bug#25950 is fixed."
  (cl-letf* ((call-process-args ())
             ((symbol-function #'call-process)
              (lambda (&rest args) (push args call-process-args) 0)))
    (with-temp-buffer
      (ediff-exec-process "diff" (current-buffer) :synchronous ""
                          "/:/a" "/:/b")
      (should (equal call-process-args
                     `(("diff" nil ,(current-buffer) nil "/a" "/b")))))))

(ert-deftest ediff-diff-tests--ediff-exec-process--nil ()
  "Check that Bug#26378 is fixed."
  (cl-letf* ((call-process-args ())
             ((symbol-function #'call-process)
              (lambda (&rest args) (push args call-process-args) 0)))
    (with-temp-buffer
      (ediff-exec-process "diff" (current-buffer) :synchronous ""
                          "foo" nil "")
      (should (equal call-process-args
                     `(("diff" nil ,(current-buffer) nil "foo")))))))

;;; ediff-diff-tests.el ends here
