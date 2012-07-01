;;; xml-parse-tests.el --- Test suite for XML parsing.

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken.com>
;; Keywords:       internal
;; Human-Keywords: internal

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

;; Type M-x test-xml-parse RET to generate the test buffer.

;;; Code:

(require 'xml)

(defvar xml-parse-tests--data
  '(;; General entity substitution
    ("<?xml version=\"1.0\"?><!DOCTYPE foo SYSTEM \"bar.dtd\" [<!ENTITY ent \"AbC\">]><foo a=\"b\"><bar>&ent;;</bar></foo>" .
     ((foo ((a . "b")) (bar nil "AbC;"))))
    ;; Parameter entity substitution
    ("<?xml version=\"1.0\"?><!DOCTYPE foo SYSTEM \"bar.dtd\" [<!ENTITY % pent \"AbC\"><!ENTITY ent \"%pent;\">]><foo a=\"b\"><bar>&ent;;</bar></foo>" .
     ((foo ((a . "b")) (bar nil "AbC;"))))
    ;; Tricky parameter entity substitution (like XML spec Appendix D)
    ("<?xml version='1.0'?><!DOCTYPE foo [ <!ENTITY % xx '&#37;zz;'><!ENTITY % zz '&#60;!ENTITY ent \"b\" >' > %xx; ]><foo>A&ent;C</foo>" .
     ((foo nil "AbC")))
    ;; Bug#7172
    ("<?xml version=\"1.0\"?><!DOCTYPE foo [ <!ELEMENT EXAM_PLE EMPTY> ]><foo></foo>" .
     ((foo nil))))
  "Alist of XML strings and their expected parse trees.")

(ert-deftest xml-parse-tests ()
  "Test XML parsing."
  (with-temp-buffer
    (dolist (test xml-parse-tests--data)
      (erase-buffer)
      (insert (car test))
      (should (equal (cdr test)
		     (xml-parse-region (point-min) (point-max)))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; xml-parse-tests.el ends here.
