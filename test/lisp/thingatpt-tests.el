;;; thingatpt.el --- tests for thing-at-point.

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

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

(defvar thing-at-point-test-data
  '(("https://1.gnu.org" 1  url "https://1.gnu.org")
    ("https://2.gnu.org" 6 url "https://2.gnu.org")
    ("https://3.gnu.org" 19 url "https://3.gnu.org")
    ("https://4.gnu.org" 1  url "https://4.gnu.org")
    ("A geo URI (geo:3.14159,-2.71828)." 12 url "geo:3.14159,-2.71828")
    ("Visit https://5.gnu.org now." 5 url nil)
    ("Visit https://6.gnu.org now." 7 url "https://6.gnu.org")
    ("Visit https://7.gnu.org now." 22 url "https://7.gnu.org")
    ("Visit https://8.gnu.org now." 22 url "https://8.gnu.org")
    ("Visit https://9.gnu.org now." 25 url nil)
    ;; Invalid URIs
    ("<<<<" 2 url nil)
    ("<>" 1 url nil)
    ("<url:>" 1 url nil)
    ("http://" 1 url nil)
    ;; Invalid schema
    ("foo://www.gnu.org" 1 url nil)
    ("foohttp://www.gnu.org" 1 url nil)
    ;; Non alphanumeric characters can be found in URIs
    ("ftp://example.net/~foo!;#bar=baz&goo=bob" 3 url "ftp://example.net/~foo!;#bar=baz&goo=bob")
    ("bzr+ssh://user@example.net:5/a%20d,5" 34 url "bzr+ssh://user@example.net:5/a%20d,5")
    ;; <url:...> markup
    ("Url: <url:foo://1.example.com>..." 8 url "foo://1.example.com")
    ("Url: <url:foo://2.example.com>..." 30 url "foo://2.example.com")
    ("Url: <url:foo://www.gnu.org/a bc>..." 20 url "foo://www.gnu.org/a bc")
    ;; Hack used by thing-at-point: drop punctuation at end of URI.
    ("Go to https://www.gnu.org, for details" 7 url "https://www.gnu.org")
    ("Go to https://www.gnu.org." 24 url "https://www.gnu.org")
    ;; Standard URI delimiters
    ("Go to \"https://10.gnu.org\"." 8 url "https://10.gnu.org")
    ("Go to \"https://11.gnu.org/\"." 26 url "https://11.gnu.org/")
    ("Go to <https://12.gnu.org> now." 8 url "https://12.gnu.org")
    ("Go to <https://13.gnu.org> now." 24 url "https://13.gnu.org")
    ;; Parenthesis handling (non-standard)
    ("http://example.com/a(b)c" 21 url "http://example.com/a(b)c")
    ("http://example.com/a(b)" 21 url "http://example.com/a(b)")
    ("(http://example.com/abc)" 2 url "http://example.com/abc")
    ("This (http://example.com/a(b))" 7 url "http://example.com/a(b)")
    ("This (http://example.com/a(b))" 30 url "http://example.com/a(b)")
    ("This (http://example.com/a(b))" 5 url nil)
    ("http://example.com/ab)c" 4 url "http://example.com/ab)c")
    ;; URL markup, lacking schema
    ("<url:foo@example.com>" 1 url "mailto:foo@example.com")
    ("<url:ftp.example.net/abc/>" 1 url "ftp://ftp.example.net/abc/"))
  "List of thing-at-point tests.
Each list element should have the form

  (STRING POS THING RESULT)

where STRING is a string of buffer contents, POS is the value of
point, THING is a symbol argument for `thing-at-point', and
RESULT should be the result of calling `thing-at-point' from that
position to retrieve THING.")

(ert-deftest thing-at-point-tests ()
  "Test the file-local variables implementation."
  (dolist (test thing-at-point-test-data)
    (with-temp-buffer
      (insert (nth 0 test))
      (goto-char (nth 1 test))
      (should (equal (thing-at-point (nth 2 test)) (nth 3 test))))))

;; These tests reflect the actual behavior of
;; `thing-at-point-bounds-of-list-at-point'.
(ert-deftest thing-at-point-bug24627 ()
  "Test for https://debbugs.gnu.org/24627 ."
  (let ((string-result '(("(a \"b\" c)" . (a "b" c))
                         (";(a \"b\" c)")
                         ("(a \"b\" c\n)" . (a "b" c))
                         ("\"(a b c)\"")
                         ("(a ;(b c d)\ne)" . (a e))
                         ("(foo\n(a ;(b c d)\ne) bar)" . (a e))
                         ("(foo\na ;(b c d)\ne bar)" . (foo a e bar))
                         ("(foo\n(a \"(b c d)\"\ne) bar)" . (a "(b c d)" e))
                         ("(b\n(a ;(foo c d)\ne) bar)" . (a e))
                         ("(princ \"(a b c)\")" . (princ "(a b c)"))
                         ("(defun foo ()\n  \"Test function.\"\n  ;;(a b)\n  nil)" . (defun foo nil "Test function." nil))))
        (file
         (expand-file-name "lisp/thingatpt.el" source-directory))
        buf)
    ;; Test for `thing-at-point'.
    (when (file-exists-p file)
      (unwind-protect
          (progn
            (setq buf (find-file file))
            (goto-char (point-max))
            (forward-line -1)
            (should-not (thing-at-point 'list)))
        (kill-buffer buf)))
    ;; Tests for `list-at-point'.
    (dolist (str-res string-result)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert (car str-res))
        (re-search-backward "\\((a\\|^a\\)")
        (should (equal (list-at-point)
                       (cdr str-res)))))))

(ert-deftest thing-at-point-url-in-comment ()
  (with-temp-buffer
    (c-mode)
    (insert "/* (http://foo/bar)\n(http://foo/bar(baz)) */\n")
    (goto-char 6)
    (should (equal (thing-at-point 'url) "http://foo/bar"))
    (goto-char 23)
    (should (equal (thing-at-point 'url) "http://foo/bar(baz)"))))

;;; thingatpt.el ends here
