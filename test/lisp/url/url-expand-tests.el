;;; url-expand-tests.el --- Test suite for relative URI/URL resolution.

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Alain Schneble <a.s@realize.ch>
;; Version: 1.0

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

;; Test cases covering URI reference resolution as described in RFC3986,
;; section 5. Reference Resolution and especially the relative resolution
;; rules specified in section 5.2. Relative Resolution.

;; Each test calls `url-expand-file-name', typically with a relative
;; reference URI and a base URI as string and compares the result (Actual)
;; against a manually specified URI (Expected)

;;; Code:

(require 'url-expand)
(require 'ert)

(ert-deftest url-expand-file-name/relative-resolution-normal-examples ()
  "RFC 3986, Section 5.4 Reference Resolution Examples / Section 5.4.1. Normal Examples"
  (should (equal (url-expand-file-name "g:h"     "http://a/b/c/d;p?q") "g:h"))
  (should (equal (url-expand-file-name "g"       "http://a/b/c/d;p?q") "http://a/b/c/g"))
  (should (equal (url-expand-file-name "./g"     "http://a/b/c/d;p?q") "http://a/b/c/g"))
  (should (equal (url-expand-file-name "g/"      "http://a/b/c/d;p?q") "http://a/b/c/g/"))
  (should (equal (url-expand-file-name "/g"      "http://a/b/c/d;p?q") "http://a/g"))
  (should (equal (url-expand-file-name "//g"     "http://a/b/c/d;p?q") "http://g"))
  (should (equal (url-expand-file-name "?y"      "http://a/b/c/d;p?q") "http://a/b/c/d;p?y"))
  (should (equal (url-expand-file-name "g?y"     "http://a/b/c/d;p?q") "http://a/b/c/g?y"))
  (should (equal (url-expand-file-name "#s"      "http://a/b/c/d;p?q") "http://a/b/c/d;p?q#s"))
  (should (equal (url-expand-file-name "g#s"     "http://a/b/c/d;p?q") "http://a/b/c/g#s"))
  (should (equal (url-expand-file-name "g?y#s"   "http://a/b/c/d;p?q") "http://a/b/c/g?y#s"))
  (should (equal (url-expand-file-name ";x"      "http://a/b/c/d;p?q") "http://a/b/c/;x"))
  (should (equal (url-expand-file-name "g;x"     "http://a/b/c/d;p?q") "http://a/b/c/g;x"))
  (should (equal (url-expand-file-name "g;x?y#s" "http://a/b/c/d;p?q") "http://a/b/c/g;x?y#s"))
  (should (equal (url-expand-file-name ""        "http://a/b/c/d;p?q") "http://a/b/c/d;p?q"))
  (should (equal (url-expand-file-name "."       "http://a/b/c/d;p?q") "http://a/b/c/"))
  (should (equal (url-expand-file-name "./"      "http://a/b/c/d;p?q") "http://a/b/c/"))
  (should (equal (url-expand-file-name ".."      "http://a/b/c/d;p?q") "http://a/b/"))
  (should (equal (url-expand-file-name "../"     "http://a/b/c/d;p?q") "http://a/b/"))
  (should (equal (url-expand-file-name "../g"    "http://a/b/c/d;p?q") "http://a/b/g"))
  (should (equal (url-expand-file-name "../.."   "http://a/b/c/d;p?q") "http://a/"))
  (should (equal (url-expand-file-name "../../"  "http://a/b/c/d;p?q") "http://a/"))
  (should (equal (url-expand-file-name "../../g" "http://a/b/c/d;p?q") "http://a/g")))

(ert-deftest url-expand-file-name/relative-resolution-absolute-examples ()
  "RFC 3986, Section 5.4 Reference Resolution Examples / Section 5.4.2. Abnormal Examples"
  (should (equal (url-expand-file-name "../../../g"    "http://a/b/c/d;p?q") "http://a/g"))
  (should (equal (url-expand-file-name "../../../../g" "http://a/b/c/d;p?q") "http://a/g"))

  (should (equal (url-expand-file-name "/./g"          "http://a/b/c/d;p?q") "http://a/g"))
  (should (equal (url-expand-file-name "/../g"         "http://a/b/c/d;p?q") "http://a/g"))
  (should (equal (url-expand-file-name "g."            "http://a/b/c/d;p?q") "http://a/b/c/g."))
  (should (equal (url-expand-file-name ".g"            "http://a/b/c/d;p?q") "http://a/b/c/.g"))
  (should (equal (url-expand-file-name "g.."           "http://a/b/c/d;p?q") "http://a/b/c/g.."))
  (should (equal (url-expand-file-name "..g"           "http://a/b/c/d;p?q") "http://a/b/c/..g"))

  (should (equal (url-expand-file-name "./../g"        "http://a/b/c/d;p?q") "http://a/b/g"))
  (should (equal (url-expand-file-name "./g/."         "http://a/b/c/d;p?q") "http://a/b/c/g/"))
  (should (equal (url-expand-file-name "g/./h"         "http://a/b/c/d;p?q") "http://a/b/c/g/h"))
  (should (equal (url-expand-file-name "g/../h"        "http://a/b/c/d;p?q") "http://a/b/c/h"))
  (should (equal (url-expand-file-name "g;x=1/./y"     "http://a/b/c/d;p?q") "http://a/b/c/g;x=1/y"))
  (should (equal (url-expand-file-name "g;x=1/../y"    "http://a/b/c/d;p?q") "http://a/b/c/y"))

  (should (equal (url-expand-file-name "g?y/./x"       "http://a/b/c/d;p?q") "http://a/b/c/g?y/./x"))
  (should (equal (url-expand-file-name "g?y/../x"      "http://a/b/c/d;p?q") "http://a/b/c/g?y/../x"))
  (should (equal (url-expand-file-name "g#s/./x"       "http://a/b/c/d;p?q") "http://a/b/c/g#s/./x"))
  (should (equal (url-expand-file-name "g#s/../x"      "http://a/b/c/d;p?q") "http://a/b/c/g#s/../x"))

  (should (equal (url-expand-file-name "http:g"        "http://a/b/c/d;p?q") "http:g")) ; for strict parsers
  )

(ert-deftest url-expand-file-name/relative-resolution-additional-examples ()
  "Reference Resolution Examples / Arbitrary Examples"
  (should (equal (url-expand-file-name "" "http://host/foobar") "http://host/foobar"))
  (should (equal (url-expand-file-name "?y"      "http://a/b/c/d") "http://a/b/c/d?y"))
  (should (equal (url-expand-file-name "?y"      "http://a/b/c/d/") "http://a/b/c/d/?y"))
  (should (equal (url-expand-file-name "?y#fragment"      "http://a/b/c/d;p?q") "http://a/b/c/d;p?y#fragment"))
  (should (equal (url-expand-file-name "#bar" "http://host") "http://host#bar"))
  (should (equal (url-expand-file-name "#bar" "http://host/") "http://host/#bar"))
  (should (equal (url-expand-file-name "#bar" "http://host/foo") "http://host/foo#bar"))
  (should (equal (url-expand-file-name "foo#bar" "http://host/foobar") "http://host/foo#bar"))
  (should (equal (url-expand-file-name "foo#bar" "http://host/foobar/") "http://host/foobar/foo#bar")))

(provide 'url-expand-tests)

;;; url-expand-tests.el ends here
