;;; xml-parse-tests.el --- Test suite for XML parsing.

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Type M-x test-xml-parse RET to generate the test buffer.

;;; Code:

(require 'ert)
(require 'xml)

(defvar xml-parse-tests--data
  `(;; General entity substitution
    ("<?xml version=\"1.0\"?><!DOCTYPE foo SYSTEM \"bar.dtd\" [<!ENTITY ent \"AbC\">]><foo a=\"b\"><bar>&ent;;</bar></foo>" .
     ((foo ((a . "b")) (bar nil "AbC;"))))
    ("<?xml version=\"1.0\"?><foo>&amp;amp;&#x26;apos;&apos;&lt;&gt;&quot;</foo>" .
     ((foo () "&amp;&apos;'<>\"")))
    ;; Parameter entity substitution
    ("<?xml version=\"1.0\"?><!DOCTYPE foo SYSTEM \"bar.dtd\" [<!ENTITY % pent \"AbC\"><!ENTITY ent \"%pent;\">]><foo a=\"b\"><bar>&ent;;</bar></foo>" .
     ((foo ((a . "b")) (bar nil "AbC;"))))
    ;; Tricky parameter entity substitution (like XML spec Appendix D)
    ("<?xml version='1.0'?><!DOCTYPE foo [ <!ENTITY % xx '&#37;zz;'><!ENTITY % zz '&#60;!ENTITY ent \"b\" >' > %xx; ]><foo>A&ent;C</foo>" .
     ((foo () "AbC")))
    ;; Bug#7172
    ("<?xml version=\"1.0\"?><!DOCTYPE foo [ <!ELEMENT EXAM_PLE EMPTY> ]><foo></foo>" .
     ((foo ())))
    ;; Entities referencing entities, in character data
    ("<!DOCTYPE foo [ <!ENTITY b \"B\"><!ENTITY abc \"a&b;c\">]><foo>&abc;</foo>" .
     ((foo () "aBc")))
    ;; Entities referencing entities, in attribute values
    ("<!DOCTYPE foo [ <!ENTITY b \"B\"><!ENTITY abc \"a&b;c\">]><foo a=\"-&abc;-\">1</foo>" .
     ((foo ((a . "-aBc-")) "1")))
    ;; Character references must be treated as character data
    ("<foo>AT&amp;T;</foo>" . ((foo () "AT&T;")))
    ("<foo>&#38;amp;</foo>" . ((foo () "&amp;")))
    ("<foo>&#x26;amp;</foo>" . ((foo () "&amp;")))
    ;; Unusual but valid XML names [5]
    ("<ÀÖØö.3·-‿⁀󯿿>abc</ÀÖØö.3·-‿⁀󯿿>" . ((,(intern "ÀÖØö.3·-‿⁀󯿿") () "abc")))
    ("<:>abc</:>" . ((,(intern ":") () "abc"))))
  "Alist of XML strings and their expected parse trees.")

(defvar xml-parse-tests--bad-data
  '(;; XML bomb in content
    "<!DOCTYPE foo [<!ENTITY lol \"lol\"><!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"><!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\">]><foo>&lol2;</foo>"
    ;; XML bomb in attribute value
    "<!DOCTYPE foo [<!ENTITY lol \"lol\"><!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"><!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\">]><foo a=\"&lol2;\">!</foo>"
    ;; Non-terminating DTD
    "<!DOCTYPE foo [ <!ENTITY b \"B\"><!ENTITY abc \"a&b;c\">"
    "<!DOCTYPE foo [ <!ENTITY b \"B\"><!ENTITY abc \"a&b;c\">asdf"
    "<!DOCTYPE foo [ <!ENTITY b \"B\"><!ENTITY abc \"a&b;c\">asdf&abc;"
    ;; Invalid XML names
    "<0foo>abc</0foo>"
    "<‿foo>abc</‿foo>"
    "<f¿>abc</f¿>"
    ;; Two root tags
    "<a/><b></b>"
    ;; Bug#16344
    "<!----><x>< /x>"
    "<a>< b/></a>")
  "List of XML strings that should signal an error in the parser")

(defvar xml-parse-tests--qnames
  '( ;; Test data for name expansion
    ("<?xml version=\"1.0\" encoding=\"UTF-8\"?><D:multistatus xmlns:D=\"DAV:\"><D:response><D:href>/calendar/events/</D:href><D:propstat><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>"
    ;; Result with qnames as cons
    ((("DAV:" . "multistatus")
      ((("http://www.w3.org/2000/xmlns/" . "D") . "DAV:"))
      (("DAV:" . "response") nil (("DAV:" . "href") nil "/calendar/events/")
       (("DAV:" . "propstat") nil (("DAV:" . "status") nil "HTTP/1.1 200 OK")))))
    ;; Result with qnames as symbols
    ((DAV:multistatus
      ((("http://www.w3.org/2000/xmlns/" . "D") . "DAV:"))
      (DAV:response nil (DAV:href nil "/calendar/events/")
		    (DAV:propstat nil (DAV:status nil "HTTP/1.1 200 OK"))))))
    ("<?xml version=\"1.0\" encoding=\"UTF-8\"?><F:something>hi there</F:something>"
     ((("FOOBAR:" . "something") nil "hi there"))
     ((FOOBAR:something nil "hi there"))))
  "List of strings which are parsed using namespace expansion.
Parser is called with and without 'symbol-qnames argument.")

(ert-deftest xml-parse-tests ()
  "Test XML parsing."
  (with-temp-buffer
    (dolist (test xml-parse-tests--data)
      (erase-buffer)
      (insert (car test))
      (should (equal (cdr test) (xml-parse-region))))
    (let ((xml-entity-expansion-limit 50))
      (dolist (test xml-parse-tests--bad-data)
	(erase-buffer)
	(insert test)
	(should-error (xml-parse-region))))
    (let ((testdata (car xml-parse-tests--qnames)))
      (erase-buffer)
      (insert (car testdata))
      (should (equal (nth 1 testdata)
		     (xml-parse-region nil nil nil nil t)))
      (should (equal (nth 2 testdata)
		     (xml-parse-region nil nil nil nil 'symbol-qnames))))
    (let ((testdata (nth 1 xml-parse-tests--qnames)))
      (erase-buffer)
      (insert (car testdata))
      ;; Provide additional namespace-URI mapping
      (should (equal (nth 1 testdata)
		     (xml-parse-region
		      nil nil nil nil
		      (append xml-default-ns
			      '(("F" . "FOOBAR:"))))))
      (should (equal (nth 2 testdata)
		     (xml-parse-region
		      nil nil nil nil
		      (cons 'symbol-qnames
			    (append xml-default-ns
				    '(("F" . "FOOBAR:"))))))))))

;; Test bug #23440 (proper expansion of default namespace)
; Test data for default namespace
(defvar xml-parse-test--default-namespace-qnames
  (cons "<something xmlns=\"myns:\"><whatever></whatever></something>"
        '((myns:something
           ((("http://www.w3.org/2000/xmlns/" . "")
             . "myns:"))
           (myns:whatever nil)))))

(ert-deftest xml-parse-test-default-namespace-qnames ()
  (with-temp-buffer
    (insert (car xml-parse-test--default-namespace-qnames))
    (should (equal (cdr xml-parse-test--default-namespace-qnames)
                   (xml-parse-region nil nil nil nil 'symbol-qnames)))))

;; Test bug #26533 (proper expansion in prefixed attributes with 'symbol-qnames)
(defvar xml-parse-test--namespace-attribute-qnames
  (cons "<something xmlns:a=\"myns:\"><whatever a:b='c'></whatever></something>"
        '((something
           ((("http://www.w3.org/2000/xmlns/" . "a")
             . "myns:"))
           (whatever
            ((myns:b . "c")))))))

(ert-deftest xml-parse-namespace-attribute-qnames ()
  (with-temp-buffer
    (insert (car xml-parse-test--namespace-attribute-qnames))
    (should (equal (cdr xml-parse-test--namespace-attribute-qnames)
                   (xml-parse-region nil nil nil nil 'symbol-qnames)))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; xml-parse-tests.el ends here.
