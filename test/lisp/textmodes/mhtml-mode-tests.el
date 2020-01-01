;;; mhtml-mode-tests.el --- Tests for mhtml-mode

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Keywords: tests

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

(require 'mhtml-mode)
(require 'ert)

(defun mhtml-test-syntax (before after what)
  (with-temp-buffer
    (mhtml-mode)
    (insert before)
    (save-excursion
      (insert after))
    (font-lock-ensure)
    (should (eq (syntax-ppss-context (syntax-ppss)) what))))

(ert-deftest mhtml-comment-js ()
  (mhtml-test-syntax "<html><script>\n/* "
                     " some text */<script></html>"
                     'comment))

(ert-deftest mhtml-string-js ()
  (mhtml-test-syntax "<html><script>\n\" "
                     " some text \"<script></html>"
                     'string))

(ert-deftest mhtml-comment-css ()
  (mhtml-test-syntax "<html><style>\n/* "
                      " some text */<style></html>"
                      'comment))

(ert-deftest mhtml-string-css ()
  (mhtml-test-syntax "<html><style>\n\" "
                      " some text \"<style></html>"
                      'string))

;;; mhtml-mode-tests.el ends here
