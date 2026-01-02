;;; url-parse-tests.el --- Test suite for URI/URL parsing.  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2026 Free Software Foundation, Inc.

;; Author: Alain Schneble <a.s@realize.ch>

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

;; Test cases covering generic URI syntax as described in RFC3986,
;; section 3. Syntax Components and 4. Usage.  See also appendix
;; A. Collected ABNF for URI, as the example given here are all
;; productions of this grammar.

;; Each tests parses a given URI string - whether relative or absolute -
;; using `url-generic-parse-url' and compares the constructed
;; URL-struct (Actual) against a manually `url-parse-make-urlobj'-
;; constructed URL-struct (Expected).

;;; Code:

(require 'url-parse)
(require 'ert)

(ert-deftest url-generic-parse-url/generic-uri-examples ()
  "RFC 3986, section 1.1.2. Examples / Example illustrating several URI schemes and variations in their common syntax components"
  (should (equal (url-generic-parse-url "ftp://ftp.is.co.za/rfc/rfc1808.txt") (url-parse-make-urlobj "ftp" nil nil "ftp.is.co.za" nil "/rfc/rfc1808.txt" nil nil t)))
  (should (equal (url-generic-parse-url "http://www.ietf.org/rfc/rfc2396.txt") (url-parse-make-urlobj "http" nil nil "www.ietf.org" nil "/rfc/rfc2396.txt" nil nil t)))
  (should (equal (url-generic-parse-url "ldap://[2001:db8::7]/c=GB?objectClass?one") (url-parse-make-urlobj "ldap" nil nil "[2001:db8::7]" nil "/c=GB?objectClass?one" nil nil t)))
  (should (equal (url-generic-parse-url "mailto:John.Doe@example.com") (url-parse-make-urlobj "mailto" nil nil nil nil "John.Doe@example.com" nil nil nil)))
  (should (equal (url-generic-parse-url "news:comp.infosystems.www.servers.unix") (url-parse-make-urlobj "news" nil nil nil nil "comp.infosystems.www.servers.unix" nil nil nil)))
  (should (equal (url-generic-parse-url "tel:+1-816-555-1212") (url-parse-make-urlobj "tel" nil nil nil nil "+1-816-555-1212" nil nil nil)))
  (should (equal (url-generic-parse-url "telnet://192.0.2.16:80/") (url-parse-make-urlobj "telnet" nil nil "192.0.2.16" 80 "/" nil nil t)))
  (should (equal (url-generic-parse-url "urn:oasis:names:specification:docbook:dtd:xml:4.1.2") (url-parse-make-urlobj "urn" nil nil nil nil "oasis:names:specification:docbook:dtd:xml:4.1.2" nil nil nil))))

(ert-deftest url-generic-parse-url/generic-uri ()
  "RFC 3986, section 3. Syntax Components / generic URI syntax"
  ;; empty path
  (should (equal (url-generic-parse-url "http://host#") (url-parse-make-urlobj "http" nil nil "host" nil "" "" nil t)))
  (should (equal (url-generic-parse-url "http://host#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host?#") (url-parse-make-urlobj "http" nil nil "host" nil "?" "" nil t)))
  (should (equal (url-generic-parse-url "http://host?query#") (url-parse-make-urlobj "http" nil nil "host" nil "?query" "" nil t)))
  (should (equal (url-generic-parse-url "http://host?#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "?" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host?query#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "?query" "fragment" nil t)))
  ;; absolute path /
  (should (equal (url-generic-parse-url "http://host/#") (url-parse-make-urlobj "http" nil nil "host" nil "/" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/?#") (url-parse-make-urlobj "http" nil nil "host" nil "/?" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/?query#") (url-parse-make-urlobj "http" nil nil "host" nil "/?query" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/?#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/?" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/?query#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/?query" "fragment" nil t)))
  ;; absolute path /foo
  (should (equal (url-generic-parse-url "http://host/foo#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo?#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo?" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo?query#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo?query" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo?#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo?" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo?query#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo?query" "fragment" nil t)))
  ;; absolute path /foo/
  (should (equal (url-generic-parse-url "http://host/foo/#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/?#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/?" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/?query#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/?query" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/?#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/?" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/?query#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/?query" "fragment" nil t)))
  ;; absolute path /foo/bar
  (should (equal (url-generic-parse-url "http://host/foo/bar#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar?#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar?" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar?query#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar?query" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar?#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar?" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar?query#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar?query" "fragment" nil t)))
  ;; absolute path /foo/bar/
  (should (equal (url-generic-parse-url "http://host/foo/bar/#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/?#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/?" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/?query#") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/?query" "" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/?#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/?" "fragment" nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/?query#fragment") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/?query" "fragment" nil t)))
  ;; for more examples of URIs without fragments, see tests covering section 4.3. Absolute URI
  )

(ert-deftest url-generic-parse-url/network-path-reference ()
  "RFC 3986, section 4.2. Relative Reference / network-path reference: a relative reference that begins with two slash characters"
  (should (equal (url-generic-parse-url "//host") (url-parse-make-urlobj nil nil nil "host" nil "" nil nil t)))
  (should (equal (url-generic-parse-url "//host/") (url-parse-make-urlobj nil nil nil "host" nil "/" nil nil t)))
  (should (equal (url-generic-parse-url "//host/foo") (url-parse-make-urlobj nil nil nil "host" nil "/foo" nil nil t)))
  (should (equal (url-generic-parse-url "//host/foo/bar") (url-parse-make-urlobj nil nil nil "host" nil "/foo/bar" nil nil t)))
  (should (equal (url-generic-parse-url "//host/foo/bar/") (url-parse-make-urlobj nil nil nil "host" nil "/foo/bar/" nil nil t))))

(ert-deftest url-generic-parse-url/absolute-path-reference ()
  "RFC 3986, section 4.2. Relative Reference / absolute-path reference: a relative reference that begins with a single slash character"
  (should (equal (url-generic-parse-url "/") (url-parse-make-urlobj nil nil nil nil nil "/" nil nil nil)))
  (should (equal (url-generic-parse-url "/foo") (url-parse-make-urlobj nil nil nil nil nil "/foo" nil nil nil)))
  (should (equal (url-generic-parse-url "/foo/bar") (url-parse-make-urlobj nil nil nil nil nil "/foo/bar" nil nil nil)))
  (should (equal (url-generic-parse-url "/foo/bar/") (url-parse-make-urlobj nil nil nil nil nil "/foo/bar/" nil nil nil)))
  (should (equal (url-generic-parse-url "/foo/bar#") (url-parse-make-urlobj nil nil nil nil nil "/foo/bar" "" nil nil)))
  (should (equal (url-generic-parse-url "/foo/bar/#") (url-parse-make-urlobj nil nil nil nil nil "/foo/bar/" "" nil nil))))

(ert-deftest url-generic-parse-url/relative-path-reference ()
  "RFC 3986, section 4.2. Relative Reference / relative-path reference: a relative reference that does not begin with a slash character"
  (should (equal (url-generic-parse-url "foo") (url-parse-make-urlobj nil nil nil nil nil "foo" nil nil nil)))
  (should (equal (url-generic-parse-url "foo/bar") (url-parse-make-urlobj nil nil nil nil nil "foo/bar" nil nil nil)))
  (should (equal (url-generic-parse-url "foo/bar/") (url-parse-make-urlobj nil nil nil nil nil "foo/bar/" nil nil nil)))
  (should (equal (url-generic-parse-url "./foo") (url-parse-make-urlobj nil nil nil nil nil "./foo" nil nil nil)))
  (should (equal (url-generic-parse-url "./foo/bar") (url-parse-make-urlobj nil nil nil nil nil "./foo/bar" nil nil nil)))
  (should (equal (url-generic-parse-url "./foo/bar/") (url-parse-make-urlobj nil nil nil nil nil "./foo/bar/" nil nil nil)))
  (should (equal (url-generic-parse-url "../foo") (url-parse-make-urlobj nil nil nil nil nil "../foo" nil nil nil)))
  (should (equal (url-generic-parse-url "../foo/bar") (url-parse-make-urlobj nil nil nil nil nil "../foo/bar" nil nil nil)))
  (should (equal (url-generic-parse-url "../foo/bar/") (url-parse-make-urlobj nil nil nil nil nil "../foo/bar/" nil nil nil)))
  (should (equal (url-generic-parse-url "./this:that") (url-parse-make-urlobj nil nil nil nil nil "./this:that" nil nil nil)))
  ;; for more examples of relative-path references, see tests covering section 4.4. Same-Document Reference
  )

(ert-deftest url-generic-parse-url/absolute-uri ()
  "RFC 3986, section 4.3. Absolute URI / absolute URI: absolute form of a URI without a fragment identifier"
  ;; empty path
  (should (equal (url-generic-parse-url "http://host") (url-parse-make-urlobj "http" nil nil "host" nil "" nil nil t)))
  (should (equal (url-generic-parse-url "http://host?") (url-parse-make-urlobj "http" nil nil "host" nil "?" nil nil t)))
  (should (equal (url-generic-parse-url "http://host?query") (url-parse-make-urlobj "http" nil nil "host" nil "?query" nil nil t)))
  ;; absolute path /
  (should (equal (url-generic-parse-url "http://host/") (url-parse-make-urlobj "http" nil nil "host" nil "/" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/?") (url-parse-make-urlobj "http" nil nil "host" nil "/?" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/?query") (url-parse-make-urlobj "http" nil nil "host" nil "/?query" nil nil t)))
  ;; absolute path /foo
  (should (equal (url-generic-parse-url "http://host/foo") (url-parse-make-urlobj "http" nil nil "host" nil "/foo" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo?") (url-parse-make-urlobj "http" nil nil "host" nil "/foo?" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo?query") (url-parse-make-urlobj "http" nil nil "host" nil "/foo?query" nil nil t)))
  ;; absolute path /foo/
  (should (equal (url-generic-parse-url "http://host/foo/") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/?") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/?" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/?query") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/?query" nil nil t)))
  ;; absolute path /foo/bar
  (should (equal (url-generic-parse-url "http://host/foo/bar") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar?") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar?" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar?query") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar?query" nil nil t)))
  ;; absolute path /foo/bar/
  (should (equal (url-generic-parse-url "http://host/foo/bar/") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/?") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/?" nil nil t)))
  (should (equal (url-generic-parse-url "http://host/foo/bar/?query") (url-parse-make-urlobj "http" nil nil "host" nil "/foo/bar/?query" nil nil t)))
  ;; example mentioned in RFC3986, section 5.4. Reference Resolution Examples
  (should (equal (url-generic-parse-url "http://a/b/c/d;p?q") (url-parse-make-urlobj "http" nil nil "a" nil "/b/c/d;p?q" nil nil t))))

(ert-deftest url-generic-parse-url/same-document-reference ()
  "RFC 3986, section 4.4. Same-Document Reference / same-document reference: empty or number sign (\"#\") followed by a fragment identifier"
  (should (equal (url-generic-parse-url "") (url-parse-make-urlobj nil nil nil nil nil "" nil nil nil)))
  (should (equal (url-generic-parse-url "#") (url-parse-make-urlobj nil nil nil nil nil "" "" nil nil)))
  (should (equal (url-generic-parse-url "#foo") (url-parse-make-urlobj nil nil nil nil nil "" "foo" nil nil))))

(ert-deftest url-generic-parse-url/multibyte-host-and-path ()
  (should (equal (url-generic-parse-url "http://банки.рф/фыва/")
                 (url-parse-make-urlobj "http" nil nil "банки.рф" nil
                                        "/фыва/" nil nil t))))

(ert-deftest url-generic-parse-url/ms-windows-file-uri-handling ()
  "Make an exception if a file:// URI  \"looks like\" a Windows file."
  (should (equal (url-generic-parse-url "file:///c:/windows-path")
                 (url-parse-make-urlobj "file" nil nil "" nil
                                        "c:/windows-path" nil nil t)))
  (should (equal (url-filename (url-generic-parse-url
                                "file:///c:/directory/file.txt"))
                 "c:/directory/file.txt"))
  (should (equal (url-recreate-url
                  (url-parse-make-urlobj "file" nil nil "" nil
                                         "c:/directory/file.txt" nil nil t))
                 "file:///c:/directory/file.txt"))
  ;; https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.2
  (should (equal (url-generic-parse-url "file:c:/path/to/file")
                 (url-parse-make-urlobj "file" nil nil nil nil
                                        "c:/path/to/file" nil nil nil)))
  (should (equal (url-recreate-url
                  (url-parse-make-urlobj "file" nil nil nil nil
                                         "c:/path/to/file" nil nil nil))
                 "file:/c:/path/to/file"))
  ;; accept backslashes too
  (should (equal (url-filename
                  (url-generic-parse-url "file:///c:\\directory\\file.txt"))
                 "c:\\directory\\file.txt"))
  ;; paths with hostname = "localhost" should work too
  (should (equal (url-filename
                  (url-generic-parse-url "file://localhost/c:/path/to/file"))
                 "c:/path/to/file"))
  ;; empty "file" url structs have to behave proper
  (should (equal (url-recreate-url
                  (url-parse-make-urlobj "file" nil nil "myhost" nil
                                         nil nil nil t))
                 "file://myhost/")))


(provide 'url-parse-tests)

;;; url-parse-tests.el ends here
