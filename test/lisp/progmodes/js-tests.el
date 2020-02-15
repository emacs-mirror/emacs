;;; js-tests.el --- Test suite for js-mode

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
(require 'js)
(require 'syntax)

(ert-deftest js-mode-fill-bug-19399 ()
  (with-temp-buffer
    (insert "/")
    (save-excursion (insert "/ comment"))
    (js-mode)
    (fill-paragraph)
    (should (equal (buffer-substring (point-min) (point-max))
                   "// comment"))))

(ert-deftest js-mode-fill-bug-22431 ()
  (with-temp-buffer
    (insert "/**\n")
    (insert " * Load the inspector's shared head.js for use by tests that ")
    (insert "need to open the something or other")
    (js-mode)
    ;; This fails with auto-fill but not fill-paragraph.
    (do-auto-fill)
    (should (equal (buffer-substring (point-min) (point-max))
                   "/**
 * Load the inspector's shared head.js for use by tests that need to
 * open the something or other"))))

(ert-deftest js-mode-fill-bug-22431-fill-paragraph-at-start ()
  (with-temp-buffer
    (insert "/**\n")
    (insert " * Load the inspector's shared head.js for use by tests that ")
    (insert "need to open the something or other")
    (js-mode)
    (goto-char (point-min))
    (fill-paragraph)
    (should (equal (buffer-substring (point-min) (point-max))
                   "/**
 * Load the inspector's shared head.js for use by tests that need to
 * open the something or other"))))

(ert-deftest js-mode-fill-comment-bug ()
  (with-temp-buffer
    (insert "/**
 * javadoc stuff here
 *
 * what
 */
function f( ) {
    // comment-auto-fill-only-comments is a variable defined in ‘newcomment.el’. comment comment")
    (js-mode)
    (setq-local comment-auto-fill-only-comments t)
    (setq-local fill-column 75)
    (auto-fill-mode 1)
    (funcall auto-fill-function)
    (beginning-of-line)
    ;; Filling should have inserted the correct comment start.
    (should (equal (buffer-substring (point) (+ 7 (point)))
                   "    // "))))

(ert-deftest js-mode-regexp-syntax ()
  (with-temp-buffer
    ;; Normally indentation tests are done in manual/indent, but in
    ;; this case we are specifically testing a case where the bug
    ;; caused the indenter not to do anything, and manual/indent can
    ;; only be used for already-correct files.
    (insert "function f(start, value) {
if (start - 1 === 0 || /[ (:,='\"]/.test(value)) {
--start;
}
if (start - 1 === 0 && /[ (:,='\"]/.test(value)) {
--start;
}
if (!/[ (:,='\"]/.test(value)) {
--start;
}
}
")
    (js-mode)
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (dolist (x '(0 4 8 4 4 8 4 4 8 4 0))
      (back-to-indentation)
      (should (= (current-column) x))
      (forward-line))))

(ert-deftest js-mode-auto-fill ()
  (with-temp-buffer
    (js-mode)
    (let ((fill-column 10)
          (comment-multi-line t))
      (insert "/* test test")
      (do-auto-fill)
      ;; Filling should continue the multi line comment.
      (should (equal (buffer-string) "/* test\n * test"))
      (erase-buffer)
      (insert "/* test test")
      (setq comment-multi-line nil)
      (do-auto-fill)
      ;; Filling should start a new comment on the next line.
      (should (equal (buffer-string) "/* test */\n/* test")))))

(ert-deftest js-mode-regexp-syntax-bug-25529 ()
  (dolist (regexp-contents '("[^[]"
                             "[/]"
                             ;; A comment with the regexp on the next
                             ;; line.
                             "*comment*/\n/regexp"))
    (with-temp-buffer
      (js-mode)
      (insert "let x = /" regexp-contents "/;\n")
      (save-excursion (insert "something();\n"))
      ;; The failure mode was that the regexp literal was not
      ;; recognized, causing the next line to be given string syntax;
      ;; but check for comment syntax as well to prevent an
      ;; implementation not recognizing the comment example.
      (should-not (syntax-ppss-context (syntax-ppss))))))

(ert-deftest js-mode-indentation-error ()
  (with-temp-buffer
    (js-mode)
    ;; The bug previously was that requesting re-indentation on the
    ;; "{" line here threw an exception.
    (insert "const TESTS = [\n{")
    (js-indent-line)
    ;; Any success is ok here.
    (should t)))

(ert-deftest js-mode-doc-comment-face ()
  (dolist (test '(("/*" "*/" font-lock-comment-face)
                  ("//" "\n" font-lock-comment-face)
                  ("/**" "*/" font-lock-doc-face)
                  ("\"" "\"" font-lock-string-face)))
    (with-temp-buffer
      (js-mode)
      (insert (car test) " he")
      (save-excursion (insert "llo " (cadr test)))
      (font-lock-ensure)
      (should (eq (get-text-property (point) 'face) (caddr test))))))

(ert-deftest js-mode-propertize-bug-1 ()
  (with-temp-buffer
    (js-mode)
    (save-excursion (insert "x"))
    (insert "/")
    ;; The bug was a hang.
    (should t)))

(ert-deftest js-mode-propertize-bug-2 ()
  (with-temp-buffer
    (js-mode)
    (insert "function f() {
    function g()
    {
        1 / 2;
    }

    function h() {
")
    (save-excursion
      (insert "
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00000000000000000000000000000000000000000000000000;
        00;
    }
}
"))
    (insert "/")
    ;; The bug was a hang.
    (should t)))

(provide 'js-tests)

;;; js-tests.el ends here
