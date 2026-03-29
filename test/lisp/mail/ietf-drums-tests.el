;;; ietf-drums-tests.el --- Test suite for ietf-drums.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Bob Rogers <rogers@rgrjr.com>

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
(require 'ietf-drums)

(ert-deftest ietf-drums-tests ()
  "Test ietf-drums functionality."

  ;; ietf-drums-remove-comments
  (should (equal (ietf-drums-remove-comments "random string") "random string"))
  (should (equal (ietf-drums-remove-comments "random \"non comment\" string")
                 "random \"non comment\" string"))
  (should (equal (ietf-drums-remove-comments "random (comment) string")
                 "random  string"))
  (should (equal (ietf-drums-remove-comments "random (comment) (string)")
                 "random  "))
  (should (equal (ietf-drums-remove-comments
                  "random (first) (second (and)) (third) not fourth")
                 "random    not fourth"))
  ;; Test some unterminated comments.
  (should (equal (ietf-drums-remove-comments "test an (unterminated comment")
                 "test an "))
  (should (equal (ietf-drums-remove-comments "test an \"unterminated quote")
                 ;; returns the string unchanged (and doesn't barf).
                 "test an \"unterminated quote"))
  (should (equal (ietf-drums-remove-comments
                  ;; note that double-quote is not special.
                  "test (unterminated comments with \"quoted (\" )stuff")
                 "test "))

  ;; ietf-drums-remove-whitespace
  (should (equal (ietf-drums-remove-whitespace "random string")
                 "randomstring"))
  (should (equal (ietf-drums-remove-whitespace "random (comment) string")
                 "random(comment)string"))
  (should (equal (ietf-drums-remove-whitespace "random \"non comment\" string")
                 "random\"non comment\"string"))
  (should (equal (ietf-drums-remove-whitespace "random (comment)\r\n(string)")
                 "random(comment)(string)"))
  (should (equal (ietf-drums-remove-whitespace
                  "random (first) (second (and)) (third) not fourth")
                 "random(first)(second (and))(third)notfourth"))
  ;; Test some unterminated comments and quotes.
  (should (equal (ietf-drums-remove-whitespace
                  "random (first) (second (and)) (third unterminated")
                 "random(first)(second (and))(third unterminated"))
  (should (equal (ietf-drums-remove-whitespace "random \"non terminated string")
                 "random\"non terminated string"))

  ;; ietf-drums-strip
  (should (equal (ietf-drums-strip "random string") "randomstring"))
  (should (equal (ietf-drums-strip "random \"non comment\" string")
                 "random\"non comment\"string"))
  (should (equal (ietf-drums-strip "random (comment) string")
                 "randomstring"))
  (should (equal (ietf-drums-strip "random (comment) (string)")
                 "random"))
  (should (equal (ietf-drums-strip
                  "random (first) (second (and)) (third) not fourth")
                 "randomnotfourth"))

  ;; ietf-drums-strip-cte
  (should (equal (ietf-drums-strip-cte "random \"non comment\" string")
                 ;; [the " " is still in there because it was quoted
                 ;; through the "strip".  -- rgr, 5-Feb-22.]
                 "randomnon commentstring"))
  (should (equal (ietf-drums-strip-cte "ran(d)do<m@>[s;t:r],,in=g")
                 "randomstring"))

  ;; ietf-drums-quote-string
  (should (equal (ietf-drums-quote-string "Bob") "Bob"))
  (should (equal (ietf-drums-quote-string "Foo Bar") "\"Foo Bar\""))

  ;; ietf-drums-get-comment
  (should (equal (ietf-drums-get-comment "random string") nil))
  (should (equal (ietf-drums-get-comment "random (comment) string") "comment"))
  (should (equal (ietf-drums-get-comment "random \"non comment\" string") nil))
  (should (equal (ietf-drums-get-comment "\"still (non) comment\" string")
                 nil))
  (should (equal (ietf-drums-get-comment "random (comment)\r\nstring")
                 "comment"))
  (should (equal (ietf-drums-get-comment "random (comment) (string)") "string"))
  (should (equal (ietf-drums-get-comment
                  "random (first) (second (and)) (third) not fourth")
                 "third"))

  ;; ietf-drums-make-address
  (should (equal (ietf-drums-make-address "Bob Rogers" "rogers@rgrjr.com")
                 "\"Bob Rogers\" <rogers@rgrjr.com>"))
  (should (equal (ietf-drums-make-address nil "rogers@rgrjr.com")
                 "rogers@rgrjr.com"))

  ;; ietf-drums-parse-address
  (should (equal (ietf-drums-parse-address "foo@example.com")
                 '("foo@example.com")))
  (should (equal (ietf-drums-parse-address "<foo@example.com>")
                 '("foo@example.com")))
  (should (equal (ietf-drums-parse-address "'foo' <foo@example.com>")
                 '("foo@example.com" . "'foo'")))
  (should (equal (ietf-drums-parse-address "foo <foo@example.com>")
                 '("foo@example.com" . "foo")))
  (should (equal (ietf-drums-parse-address "foo <foo@example.com> bar")
                 ;; [contrary to RFC2822, which wants the display-name
                 ;; before the address.  -- rgr, 5-Feb-22.]
                 '("foo@example.com" . "foo bar")))
  (should (equal (ietf-drums-parse-address " <foo@example.com> foo ")
                 ;; [ditto.  -- rgr, 5-Feb-22.]
                 '("foo@example.com" . "foo")))
  (should (equal (ietf-drums-parse-address "foo@example.com (foo)")
                 '("foo@example.com" . "foo")))
  (should (equal (ietf-drums-parse-address "Bar Baz <barbaz@example.com>")
                 '("barbaz@example.com" . "Bar Baz")))
  (should (equal (ietf-drums-parse-address "barbaz@example.com (Bar Baz)")
                 '("barbaz@example.com" . "Bar Baz")))
  (should (equal (ietf-drums-parse-address
                  "Bar Baz (ignored) <barbaz@example.com>")
                 '("barbaz@example.com" . "Bar Baz")))
  (should (equal (ietf-drums-parse-address "<barbaz@example.com> Bar Baz")
                 '("barbaz@example.com" . "Bar Baz")))
  (should (equal (ietf-drums-parse-address
                  "(Bar Baz not ignored) barbaz@example.com")
                 ;; [not strictly RFC2822, which expects the name
                 ;; comment after the address.  -- rgr, 5-Feb-22.]
                 '("barbaz@example.com" . "Bar Baz not ignored")))
  (should (equal (ietf-drums-parse-address
                  "(ignored) <barbaz@example.com> (Bar Baz not ignored)")
                 '("barbaz@example.com" . "Bar Baz not ignored")))
  (should (equal (ietf-drums-parse-address
                  "(ignored) barbaz@example.com (Bar Baz not ignored)")
                 '("barbaz@example.com" . "Bar Baz not ignored")))
  ;; Test for RFC2047 token decoding.
  (should (equal (ietf-drums-parse-address
                  "=?utf-8?B?0JfQtNGA0LDMgdCy0YHRgtCy0YPQudGC0LUh?= <foo@goo.ru>"
                  t)
                 '("foo@goo.ru" . "Здра́вствуйте!")))

  ;; ietf-drums-parse-addresses
  ;; Note that it's not worth getting too elaborate here, as the heavy
  ;; lifting is all done by ietf-drums-parse-address.
  (should (equal (ietf-drums-parse-addresses "foo@example.com")
                 '(("foo@example.com"))))
  (should (equal (ietf-drums-parse-addresses
                  "foo@example.com, bar@example.com")
                 '(("foo@example.com") ("bar@example.com"))))
  (should (equal (ietf-drums-parse-addresses
                  "foo@example.com, quux, bar@example.com")
                 '(("foo@example.com") ("bar@example.com"))))
  (should (equal (ietf-drums-parse-addresses
                  "foo@example.com, Quux Dude <quux@noop.org>, bar@example.com")
                 '(("foo@example.com") ("quux@noop.org" . "Quux Dude")
                   ("bar@example.com")))))

(provide 'ietf-drums-tests)

;;; ietf-drums-tests.el ends here
