;;; json-tests.el --- Test suite for json.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'json)

(ert-deftest test-json-plist-reverse ()
  (should (equal (json--plist-reverse '()) '()))
  (should (equal (json--plist-reverse '(:a 1)) '(:a 1)))
  (should (equal (json--plist-reverse '(:a 1 :b 2 :c 3))
                 '(:c 3 :b 2 :a 1))))

(ert-deftest test-json-plist-to-alist ()
  (should (equal (json--plist-to-alist '()) '()))
  (should (equal (json--plist-to-alist '(:a 1)) '((:a . 1))))
  (should (equal (json--plist-to-alist '(:a 1 :b 2 :c 3))
                 '((:a . 1) (:b . 2) (:c . 3)))))

(ert-deftest test-json-encode-plist ()
  (let ((plist '(:a 1 :b 2)))
    (should (equal (json-encode plist) "{\"a\":1,\"b\":2}"))))

(ert-deftest json-encode-simple-alist ()
  (should (equal (json-encode '((a . 1)
                                (b . 2)))
                 "{\"a\":1,\"b\":2}")))

(ert-deftest test-json-encode-hash-table ()
  (let ((hash-table (make-hash-table))
        (json-encoding-object-sort-predicate 'string<))
    (puthash :a 1 hash-table)
    (puthash :b 2 hash-table)
    (puthash :c 3 hash-table)
    (should (equal (json-encode hash-table)
                   "{\"a\":1,\"b\":2,\"c\":3}"))))

(ert-deftest test-json-encode-alist-with-sort-predicate ()
  (let ((alist '((:c . 3) (:a . 1) (:b . 2)))
        (json-encoding-object-sort-predicate 'string<))
    (should (equal (json-encode alist) "{\"a\":1,\"b\":2,\"c\":3}"))))

(ert-deftest test-json-encode-plist-with-sort-predicate ()
  (let ((plist '(:c 3 :a 1 :b 2))
        (json-encoding-object-sort-predicate 'string<))
    (should (equal (json-encode plist) "{\"a\":1,\"b\":2,\"c\":3}"))))

(ert-deftest json-read-simple-alist ()
  (let ((json-object-type 'alist))
    (should (equal (json-read-from-string "{\"a\": 1, \"b\": 2}")
                   '((a . 1)
                     (b . 2))))))

(ert-deftest json-encode-string-with-special-chars ()
  (should (equal (json-encode-string "a\n\fb")
                 "\"a\\n\\fb\""))
  (should (equal (json-encode-string "\nasdфыв\u001f\u007ffgh\t")
                 "\"\\nasdфыв\\u001f\u007ffgh\\t\"")))

(ert-deftest json-read-string-with-special-chars ()
  (should (equal (json-read-from-string "\"\\nasd\\u0444\\u044b\\u0432fgh\\t\"")
                 "\nasdфывfgh\t")))

(ert-deftest test-json-path-to-position-with-objects ()
  (let* ((json-string "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}")
         (matched-path (json-path-to-position 32 json-string)))
    (should (equal (plist-get matched-path :path) '("foo" "bar" "baz")))
    (should (equal (plist-get matched-path :match-start) 25))
    (should (equal (plist-get matched-path :match-end) 32))))

(ert-deftest test-json-path-to-position-with-arrays ()
  (let* ((json-string "{\"foo\": [\"bar\", [\"baz\"]]}")
         (matched-path (json-path-to-position 20 json-string)))
    (should (equal (plist-get matched-path :path) '("foo" 1 0)))
    (should (equal (plist-get matched-path :match-start) 18))
    (should (equal (plist-get matched-path :match-end) 23))))

(ert-deftest test-json-path-to-position-no-match ()
  (let* ((json-string "{\"foo\": {\"bar\": \"baz\"}}")
         (matched-path (json-path-to-position 5 json-string)))
    (should (null matched-path))))

(provide 'json-tests)
;;; json-tests.el ends here
