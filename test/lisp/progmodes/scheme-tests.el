;;; scheme-tests.el --- Test suite for scheme.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'scheme)

(ert-deftest scheme-test-indent ()
  ;; FIXME: Look into what is the expected indent here and fix it.
  :expected-result :failed
  ;; Converted from manual test.
  (with-temp-buffer
    (scheme-mode)
    ;; TODO: Should some of these be fontification tests as well?
    (let ((orig "#!/usr/bin/scheme is this a comment?

;; This one is a comment
(a)
#| and this one as #|well|# as this! |#
(b)
(cons #;(this is a
         comment)
 head tail)
"))
      (insert orig)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) orig)))))

(provide 'scheme-tests)

;;; scheme-tests.el ends here
