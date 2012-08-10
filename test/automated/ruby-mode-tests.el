;;; ruby-mode-tests.el --- Test suite for ruby-mode

;; Copyright (C) 2012  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ruby-mode)

(defun ruby-should-indent (content column)
  (with-temp-buffer
    (insert content)
    (ruby-mode)
    (ruby-indent-line)
    (should (= (current-column) column))))

(defun ruby-assert-state (content &rest values-plist)
  "Assert syntax state values at the end of CONTENT.

VALUES-PLIST is a list with alternating index and value elements."
  (with-temp-buffer
    (insert content)
    (ruby-mode)
    (syntax-propertize (point))
    (while values-plist
      (should (eq (nth (car values-plist)
                       (parse-partial-sexp (point-min) (point)))
                  (cadr values-plist)))
      (setq values-plist (cddr values-plist)))))

(ert-deftest ruby-indent-after-symbol-made-from-string-interpolation ()
  "It can indent the line after symbol made using string interpolation."
  (ruby-should-indent "def foo(suffix)\n  :\"bar#{suffix}\"\n"
                      ruby-indent-level))

(ert-deftest ruby-indent-after-js-style-symbol-with-block-beg-name ()
  "JS-style hash symbol can have keyword name."
  (ruby-should-indent "link_to \"home\", home_path, class: \"foo\"\n" 0))

(ert-deftest ruby-discern-singleton-class-from-heredoc ()
  (ruby-assert-state "foo <<asd\n" 3 ?\n)
  (ruby-assert-state "class <<asd\n" 3 nil))

(provide 'ruby-mode-tests)

;;; ruby-mode-tests.el ends here
