;;; ruby-mode-tests.el --- Test suite for ruby-mode  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'ruby-ts-mode)

(defmacro ruby-ts-with-temp-buffer (contents &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (ruby-ts-mode)
     ,@body))

(defun ruby-ts-should-indent-buffer (expected content)
  "Assert that CONTENT turns into EXPECTED after the buffer is re-indented.

The whitespace before and including \"|\" on each line is removed."
  (ruby-ts-with-temp-buffer (ruby-ts-test-string content)
    (indent-region (point-min) (point-max))
    (should (string= (ruby-ts-test-string expected) (buffer-string)))))

(defun ruby-ts-test-string (s &rest args)
  (apply 'format (replace-regexp-in-string "^[ \t]*|" "" s) args))

(ert-deftest ruby-ts-indent-simple ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-should-indent-buffer
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

(ert-deftest ruby-ts-align-to-stmt-keywords-t ()
  (skip-unless (treesit-ready-p 'ruby t))
  (let ((ruby-align-to-stmt-keywords t))
    (ruby-ts-should-indent-buffer
     "foo = if bar?
     |  1
     |else
     |  2
     |end
     |
     |foo || begin
     |  bar
     |end
     |
     |foo ||
     |  begin
     |    bar
     |  end
     |"
     "foo = if bar?
     |       1
     |else
     |  2
     | end
     |
     | foo || begin
     |    bar
     |end
     |
     |  foo ||
     | begin
     |bar
     |  end
     |")
    ))

(ert-deftest ruby-ts-align-to-stmt-keywords-case ()
  (skip-unless (treesit-ready-p 'ruby t))
  (let ((ruby-align-to-stmt-keywords '(case)))
    (ruby-ts-should-indent-buffer
     "b = case a
     |when 13
     |  6
     |else
     |  42
     |end"
     "b = case a
     |    when 13
     |  6
     |    else
     |      42
     |    end")))


(ert-deftest ruby-ts-indent-call-no-args ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer
      "variable = foo(

)"
    (goto-char (point-min))
    (forward-line 1)
    (funcall indent-line-function)
    (should (= (current-indentation) ruby-indent-level))))

(ert-deftest ruby-ts-indent-empty-if-else ()
  (skip-unless (treesit-ready-p 'ruby t))
  (let* ((str "c = if foo
      zz
    else
      zz
    end
"))
    (ruby-ts-with-temp-buffer str
      (goto-char (point-min))
      (dotimes (_ 2)
        (re-search-forward "^ *zz")
        (replace-match "")
        (funcall indent-line-function)
        (should (= (current-indentation) 6))))))

(ert-deftest ruby-ts-add-log-current-method-examples ()
  (skip-unless (treesit-ready-p 'ruby t))
  (let ((pairs '(("foo" . "#foo")
                 ("C.foo" . ".foo")
                 ("self.foo" . ".foo")
                 ("<<" . "#<<"))))
    (dolist (pair pairs)
      (let ((name  (car pair))
            (value (cdr pair)))
        (ruby-ts-with-temp-buffer (ruby-ts-test-string
                                   "module M
                                   |  class C
                                   |    def %s
                                   |      _
                                   |    end
                                   |  end
                                   |end"
                                name)
          (search-backward "_")
          (forward-line)
          (should (string= (ruby-ts-add-log-current-function)
                           (format "M::C%s" value))))))))

(ert-deftest ruby-ts-add-log-current-method-outside-of-method ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "module M
                             |  class C
                             |    def foo
                             |    end
                             |    _
                             |    def bar
                             |    end
                             |  end
                             |end")
    (search-backward "_")
    (delete-char 1)
    (should (string= (ruby-ts-add-log-current-function) "M::C"))))

(ert-deftest ruby-ts-add-log-current-method-in-singleton-class ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "class C
                             |  class << self
                             |    def foo
                             |      _
                             |    end
                             |  end
                             |end")
    (search-backward "_")
    (should (string= (ruby-ts-add-log-current-function) "C.foo"))))

(ert-deftest ruby-ts-add-log-current-method-namespace-shorthand ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "class C::D
                             |  def foo
                             |    _
                             |  end
                             |end")
    (search-backward "_")
    (should (string= (ruby-ts-add-log-current-function) "C::D#foo"))))

(ert-deftest ruby-ts-add-log-current-method-after-inner-class ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "module M
                             |  class C
                             |    class D
                             |    end
                             |    def foo
                             |      _
                             |    end
                             |  end
                             |end")
    (search-backward "_")
    (should (string= (ruby-ts-add-log-current-function) "M::C#foo"))))

(ert-deftest ruby-ts-add-log-current-method-after-inner-class-outside-methods ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "module M
                             |  class C
                             |    class D
                             |    end
                             |
                             |_
                             |  end
                             |end")
    (search-backward "_")
    (delete-char 1)
    (should (string= (ruby-ts-add-log-current-function) "M::C"))))

(ert-deftest ruby-ts-add-log-current-method-after-inner-class-outside-methods-with-text ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "module M
                             |  class C
                             |    class D
                             |    end
                             |
                             |    FOO = 5
                             |  end
                             |end")
    (search-backward "FOO")
    (should (string= (ruby-ts-add-log-current-function) "M::C"))))

(ert-deftest ruby-ts-add-log-current-method-after-endless-method ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer (ruby-ts-test-string
                             "module M
                             |  class C
                             |    def foo =
                             |      4_
                             |  end
                             |end")
    (search-backward "_")
    (delete-char 1)
    (should (string= (ruby-ts-add-log-current-function) "M::C#foo"))))

(ert-deftest ruby-ts-syntax-propertize-symbol ()
  (skip-unless (treesit-ready-p 'ruby t))
  (pcase-dolist (`(,str . ,expected)
                 '((":abc" . ":abc")
                   (":foo?" . #(":foo?" 4 5 (syntax-table (3))))
                   (":<=>" . #(":<=>" 1 4 (syntax-table (3))))))
    (ruby-ts-with-temp-buffer str
      (syntax-propertize (point-max))
      (let ((text (buffer-string)))
        (remove-text-properties 0 (1- (point-max))
                                '(fontified)
                                text)
        (should (equal-including-properties
                 text
                 expected))))))

(defmacro ruby-ts-resource-file (file)
  `(when-let* ((testfile ,(or (macroexp-file-name)
                              buffer-file-name)))
     (let ((default-directory (file-name-directory testfile)))
       (file-truename
        (expand-file-name (format "ruby-mode-resources/%s" ,file))))))

(ert-deftest ruby-ts-imenu-index ()
  (skip-unless (treesit-ready-p 'ruby t))
  (ruby-ts-with-temp-buffer
      (ruby-ts-test-string
       "module Foo
       |  class Blub
       |    def hi
       |      'Hi!'
       |    end
       |
       |    def bye
       |      'Bye!'
       |    end
       |
       |    private def self.hiding
       |      'You can't see me'
       |    end
       |  end
       |end")
    (should (equal (mapcar #'car (ruby-ts--imenu))
                   '("Foo"
                     "Foo::Blub"
                     "Foo::Blub#hi"
                     "Foo::Blub#bye"
                     "Foo::Blub.hiding")))))

(defmacro ruby-ts-deftest-indent (file)
  `(ert-deftest ,(intern (format "ruby-ts-indent-test/%s" file)) ()
     ;; :tags '(:expensive-test)
     (skip-unless (treesit-ready-p 'ruby t))
     (let ((buf (find-file-noselect (ruby-ts-resource-file ,file))))
       (unwind-protect
           (with-current-buffer buf
             (let ((orig (buffer-string)))
               ;; Indent and check that we get the original text.
               (indent-region (point-min) (point-max))
               (should (equal (buffer-string) orig))))
         (kill-buffer buf)))))

(ruby-ts-deftest-indent "ruby-ts.rb")
(ruby-ts-deftest-indent "ruby-after-operator-indent.rb")
(ruby-ts-deftest-indent "ruby-block-indent.rb")
(ruby-ts-deftest-indent "ruby-method-call-indent.rb")
(ruby-ts-deftest-indent "ruby-method-params-indent.rb")
(ruby-ts-deftest-indent "ruby-parenless-call-arguments-indent.rb")
(ruby-ts-deftest-indent "ruby-bracketed-args-indent.rb")

(provide 'ruby-ts-mode-tests)

;;; ruby-ts-mode-tests.el ends here
