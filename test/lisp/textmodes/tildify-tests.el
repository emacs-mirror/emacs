;;; tildify-test.el --- ERT tests for tildify.el -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author:     Michal Nazarewicz <mina86@mina86.com>
;; Version:    4.5
;; Keywords:   text, TeX, SGML, wp

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

;; This package defines regression tests for the tildify package.

;;; Code:

(require 'ert)
(require 'tildify)

(defun tildify-test--example-sentence (space)
  "Return an example sentence with SPACE where hard space is required."
  (concat "Lorem ipsum v" space "dolor sit amet, a" space
          "consectetur adipiscing elit."))


(defun tildify-test--example-html (sentence &optional with-nbsp is-xml)
  "Return an example HTML code.
SENTENCE is placed where spaces should not be replaced with hard spaces, and
WITH-NBSP is placed where spaces should be replaced with hard spaces.  If the
latter is missing, SENTENCE will be used in all placeholder positions.
If IS-XML is non-nil, <pre> tag is not treated specially."
  (let ((with-nbsp (or with-nbsp sentence)))
    (concat "<p>" with-nbsp "</p>\n"
            "<pre>" (if is-xml with-nbsp sentence) "</pre>\n"
            "<! -- " sentence " -- >\n"
            "<p>" with-nbsp "</p>\n"
            "<" sentence ">\n")))


(defun tildify-test--test (modes input expected)
  "Test tildify running in MODES.
INPUT is the initial content of the buffer and EXPECTED is expected result
after `tildify-buffer' is run."
  (with-temp-buffer
    (setq-local buffer-file-coding-system 'utf-8)
    (dolist (mode modes)
      (erase-buffer)
      (funcall mode)
      (let ((header (concat "Testing `tildify-buffer' in "
                            (symbol-name mode) "\n")))
        (insert header input)
        (tildify-buffer t)
        (should (string-equal (concat header expected) (buffer-string))))
      (erase-buffer)
      (let ((header (concat "Testing `tildify-region' in "
                            (symbol-name mode) "\n")))
        (insert header input)
        (tildify-region (point-min) (point-max) t)
        (should (string-equal (concat header expected) (buffer-string)))))))

(ert-deftest tildify-test-html ()
  "Tests tildification in an HTML document"
  (let* ((sentence (tildify-test--example-sentence " "))
         (with-nbsp (tildify-test--example-sentence " ")))
    (tildify-test--test '(html-mode sgml-mode)
                        (tildify-test--example-html sentence sentence)
                        (tildify-test--example-html sentence with-nbsp))))

(ert-deftest tildify-test-xml ()
  "Tests tildification in an XML document"
  (let* ((sentence (tildify-test--example-sentence " "))
         (with-nbsp (tildify-test--example-sentence " ")))
    (tildify-test--test '(nxml-mode)
                        (tildify-test--example-html sentence sentence t)
                        (tildify-test--example-html sentence with-nbsp t))))


(defun tildify-test--example-tex (sentence &optional with-nbsp)
  "Return an example (La)Tex code.
SENTENCE is placed where spaces should not be replaced with hard spaces, and
WITH-NBSP is placed where spaces should be replaced with hard spaces.  If the
latter is missing, SENTENCE will be used in all placeholder positions."
  (let ((with-nbsp (or with-nbsp sentence)))
    (concat with-nbsp "\n"
            "\\begin{verbatim}\n" sentence "\n\\end{verbatim}\n"
            "\\verb#" sentence "#\n"
            "$$" sentence "$$\n"
            "$" sentence "$\n"
            "\\[" sentence "\\]\n"
            "\\v A % " sentence "\n"
            with-nbsp "\n")))

(ert-deftest tildify-test-tex ()
  "Tests tildification in a (La)TeX document"
  (let* ((sentence (tildify-test--example-sentence " "))
         (with-nbsp (tildify-test--example-sentence "~")))
    (tildify-test--test '(tex-mode latex-mode plain-tex-mode)
                        (tildify-test--example-tex sentence sentence)
                        (tildify-test--example-tex sentence with-nbsp))))


(ert-deftest tildify-test-find-env-end-re-bug ()
    "Tests generation of end-regex using mix of indexes and strings"
  (with-temp-buffer
    (insert "foo whatever end-foo")
    (goto-char (point-min))
    (should (string-equal "end-foo"
                          (tildify--find-env "foo\\|bar"
                                             '(("foo\\|bar" . ("end-" 0))))))))


(ert-deftest tildify-test-find-env-group-index-bug ()
    "Tests generation of match-string indexes"
  (with-temp-buffer
    (let ((pairs '(("start-\\(foo\\|bar\\)" . ("end-" 1))
                   ("open-\\(foo\\|bar\\)" . ("close-" 1))))
          (beg-re "start-\\(foo\\|bar\\)\\|open-\\(foo\\|bar\\)"))
      (insert "open-foo whatever close-foo")
      (goto-char (point-min))
      (should (string-equal "close-foo" (tildify--find-env beg-re pairs))))))


(defmacro with-test-foreach (expected &rest body)
  "Helper macro for testing foreach functions.
BODY has access to pairs variable and called lambda."
  (declare (indent 1))
  (let ((got (make-symbol "got")))
    `(with-temp-buffer
       (insert "1 /- 2 -/ 3 V~ 4 ~ 5 /- 6 -/ 7")
       (let* ((pairs '(("/-" . "-/") ("V\\(.\\)" . (1))))
              (,got "")
              (called (lambda (s e)
                        (setq ,got (concat ,got (buffer-substring s e))))))
         (setq-local tildify-foreach-region-function
                     (apply-partially 'tildify-foreach-ignore-environments
                                      pairs))
         ,@body
         (should (string-equal ,expected ,got))))))

(ert-deftest tildify-test-foreach-ignore-environments ()
    "Basic test of `tildify-foreach-ignore-environments'"
  (with-test-foreach "1  3  5  7"
    (tildify-foreach-ignore-environments pairs called (point-min) (point-max))))


(ert-deftest tildify-test-foreach-ignore-environments-early-return ()
    "Test whether `tildify-foreach-ignore-environments' returns early
The function must terminate as soon as callback returns nil."
  (with-test-foreach "1 "
    (tildify-foreach-ignore-environments
     pairs (lambda (start end) (funcall called start end) nil)
     (point-min) (point-max))))

(ert-deftest tildify-test-foreach-region ()
    "Basic test of `tildify--foreach-region'"
  (with-test-foreach "1  3  5  7"
    (tildify--foreach-region called (point-min) (point-max))))

(ert-deftest tildify-test-foreach-region-early-return ()
    "Test whether `tildify--foreach-ignore' returns early
The function must terminate as soon as callback returns nil."
  (with-test-foreach "1 "
    (tildify--foreach-region (lambda (start end) (funcall called start end) nil)
      (point-min) (point-max))))

(ert-deftest tildify-test-foreach-region-limit-region ()
    "Test whether `tildify--foreach-ignore' limits callback to given region"
  (with-test-foreach "3 "
    (tildify--foreach-region called
      (+ (point-min) 10) (+ (point-min) 16))) ; start at "3" end past "4"
  (with-test-foreach "3  5"
    (tildify--foreach-region called
      (+ (point-min) 10) (+ (point-min) 20)))) ; start at "3" end past "5"


(defun tildify-space-test--test (modes nbsp env-open &optional set-space-string)
  (with-temp-buffer
    (setq-local buffer-file-coding-system 'utf-8)
    (dolist (mode modes)
      (funcall mode)
      (when set-space-string
        (setq-local tildify-space-string nbsp))
      (let ((header (concat "Testing `tildify-space' in "
                            (symbol-name mode) "\n")))
        ;; Replace space with hard space.
        (erase-buffer)
        (insert header "Lorem v ")
        (should (tildify-space))
        (should (string-equal (concat header "Lorem v" nbsp) (buffer-string)))
        ;; Inside and ignore environment, replacing does not happen.
        (erase-buffer)
        (insert header env-open "Lorem v ")
        (should (not (tildify-space)))
        (should (string-equal (concat header env-open "Lorem v ")
                              (buffer-string)))))))

(ert-deftest tildify-space-test-html ()
  "Tests auto-tildification in an HTML document"
  (tildify-space-test--test '(html-mode sgml-mode) " " "<pre>"))

(ert-deftest tildify-space-test-html-nbsp ()
  "Tests auto-tildification in an HTML document"
  (tildify-space-test--test '(html-mode sgml-mode) "&nbsp;" "<pre>" t))

(ert-deftest tildify-space-test-xml ()
  "Tests auto-tildification in an XML document"
  (tildify-space-test--test '(nxml-mode) " " "<! -- "))

(ert-deftest tildify-space-test-tex ()
  "Tests tildification in a TeX document"
  (tildify-space-test--test '(tex-mode latex-mode plain-tex-mode)
                            "~" "\\verb# "))


(defun tildify-space-undo-test--test
    (modes nbsp _env-open &optional set-space-string)
  (with-temp-buffer
    (setq-local buffer-file-coding-system 'utf-8)
    (dolist (mode modes)
      (funcall mode)
      (when set-space-string
        (setq-local tildify-space-string nbsp))
      (let ((header (concat "Testing double-space-undos in "
                            (symbol-name mode) "\n")))
        (erase-buffer)
        (insert header "Lorem v" nbsp " ")
        (should (not (tildify-space)))
        (should (string-equal (concat header "Lorem v ") (buffer-string)))))))

(ert-deftest tildify-space-undo-test-html ()
  "Tests auto-tildification in an HTML document"
  (tildify-space-undo-test--test '(html-mode sgml-mode) " " "<pre>"))

(ert-deftest tildify-space-undo-test-html-nbsp ()
  "Tests auto-tildification in an HTML document"
  (tildify-space-undo-test--test '(html-mode sgml-mode) "&nbsp;" "<pre>" t))

(ert-deftest tildify-space-undo-test-xml ()
  "Tests auto-tildification in an XML document"
  (tildify-space-undo-test--test '(nxml-mode) " " "<! -- "))

(ert-deftest tildify-space-undo-test-tex ()
  "Tests tildification in a TeX document"
  (tildify-space-undo-test--test '(tex-mode latex-mode plain-tex-mode)
                                 "~" "\\verb# "))



(provide 'tildify-tests)

;;; tildify-tests.el ends here
