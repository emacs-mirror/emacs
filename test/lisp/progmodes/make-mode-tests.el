;;; make-mode-tests.el --- tests for make-mode.el   -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

(ert-deftest make-mode-tests--bug17400 ()
  (let ((code "
foo:
	bar; : baz"))
    (with-temp-buffer
      (insert code "\n")
      (makefile-mode)
      (font-lock-ensure)
      (re-search-backward "bar")
      (let ((bar-pos (point)))
        (re-search-backward "foo")
        (let ((foo-pos (point)))
          ;; Make sure we don't confuse "bar;:" for a target!
          (should-not (equal (get-text-property bar-pos 'face)
                             (get-text-property foo-pos 'face))))))))

(provide 'make-mode-tests)

;;; make-mode-tests.el ends here
