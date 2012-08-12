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
  "Assert indentation COLUMN on the last line of CONTENT."
  (with-temp-buffer
    (insert content)
    (ruby-mode)
    (ruby-indent-line)
    (should (= (current-indentation) column))))

(defun ruby-should-indent-buffer (expected content)
  "Assert that CONTENT turns into EXPECTED after the buffer is re-indented.

The whitespace before and including \"|\" on each line is removed."
  (with-temp-buffer
    (cl-flet ((fix-indent (s) (replace-regexp-in-string "^[ \t]*|" "" s)))
      (insert (fix-indent content))
      (ruby-mode)
      (indent-region (point-min) (point-max))
      (should (string= (fix-indent expected) (buffer-substring-no-properties
                                              (point-min) (point-max)))))))

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

(ert-deftest ruby-deep-indent ()
  (let ((ruby-deep-arglist nil)
        (ruby-deep-indent-paren '(?\( ?\{ ?\[ ?\] t)))
    (ruby-should-indent "foo = [1,\n2" 7)
    (ruby-should-indent "foo = {a: b,\nc: d" 7)
    (ruby-should-indent "foo(a,\nb" 4)))

(ert-deftest ruby-deep-indent-disabled ()
  (let ((ruby-deep-arglist nil)
        (ruby-deep-indent-paren nil))
    (ruby-should-indent "foo = [\n1" ruby-indent-level)
    (ruby-should-indent "foo = {\na: b" ruby-indent-level)
    (ruby-should-indent "foo(\na" ruby-indent-level)))

(ert-deftest ruby-indent-simple ()
  (ruby-should-indent-buffer
   "if foo
   |  bar
   |end
   |zot
   |"
   "if foo
   |bar
   |  end
   |    zot
   |"))

(ert-deftest ruby-indent-keyword-label ()
  (ruby-should-indent-buffer
   "bar(class: XXX) do
   |  foo
   |end
   |bar
   |"
   "bar(class: XXX) do
   |     foo
   |  end
   |    bar
   |"))

(ert-deftest ruby-indent-method-with-question-mark ()
  (ruby-should-indent-buffer
   "if x.is_a?(XXX)
   |  foo
   |end
   |"
   "if x.is_a?(XXX)
   | foo
   |   end
   |"))

(ert-deftest ruby-indent-expr-in-regexp ()
  (ruby-should-indent-buffer
   "if /#{foo}/ =~ s
   |  x = 1
   |end
   |"
   "if /#{foo}/ =~ s
   | x = 1
   |  end
   |"))

(ert-deftest ruby-indent-singleton-class ()
  :expected-result :failed   ; Doesn't work yet, when no space before "<<".
  (ruby-should-indent-buffer
   "class<<bar
   |  foo
   |end
   |"
   "class<<bar
   |foo
   |   end
   |"))

(ert-deftest ruby-indent-array-literal ()
  (let ((ruby-deep-indent-paren nil))
    (ruby-should-indent-buffer
     "foo = [
     |  bar
     |]
     |"
     "foo = [
     | bar
     |  ]
     |"))
  (ruby-should-indent-buffer
   "foo do
   |  [bar]
   |end
   |"
   "foo do
   |[bar]
   |  end
   |"))

(ert-deftest ruby-indent-begin-end ()
  (ruby-should-indent-buffer
   "begin
   |  a[b]
   |end
   |"
   "begin
   | a[b]
   |  end
   |"))

(ert-deftest ruby-indent-array-after-paren-and-space ()
  (ruby-should-indent-buffer
   "class A
   |  def foo
   |    foo( [])
   |  end
   |end
   |"
   "class A
   | def foo
   |foo( [])
   |end
   |  end
   |"))

(ert-deftest ruby-move-to-block-stops-at-opening ()
  (with-temp-buffer
    (insert "def f\nend")
    (beginning-of-line)
    (ruby-mode)
    (ruby-move-to-block -1)
    (should (looking-at "f$"))))

(ert-deftest ruby-toggle-block-to-do-end ()
  (with-temp-buffer
    (insert "foo {|b|\n}\n")
    (ruby-mode)
    (search-backward "{")
    (ruby-toggle-block)
    (should (string= "foo do |b|\nend\n" (buffer-substring-no-properties
                                          (point-min) (point-max))))))

(ert-deftest ruby-toggle-block-to-brace ()
  (with-temp-buffer
    (insert "foo do |b|\nend\n")
    (ruby-mode)
    (search-backward "do")
    (ruby-toggle-block)
    (should (string= "foo {|b|\n}\n" (buffer-substring-no-properties
                                      (point-min) (point-max))))))

(provide 'ruby-mode-tests)

;;; ruby-mode-tests.el ends here
