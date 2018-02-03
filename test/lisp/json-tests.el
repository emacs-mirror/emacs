;;; json-tests.el --- Test suite for json.el

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'json)

(defmacro json-tests--with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and evaluate BODY there.
Point is moved to beginning of the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Utilities

(ert-deftest test-json-join ()
  (should (equal (json-join '() ", ")  ""))
  (should (equal (json-join '("a" "b" "c") ", ")  "a, b, c")))

(ert-deftest test-json-alist-p ()
  (should (json-alist-p '()))
  (should (json-alist-p '((a 1) (b 2) (c 3))))
  (should (json-alist-p '((:a 1) (:b 2) (:c 3))))
  (should (json-alist-p '(("a" 1) ("b" 2) ("c" 3))))
  (should-not (json-alist-p '(:a :b :c)))
  (should-not (json-alist-p '(:a 1 :b 2 :c 3)))
  (should-not (json-alist-p '((:a 1) (:b 2) 3))))

(ert-deftest test-json-plist-p ()
  (should (json-plist-p '()))
  (should (json-plist-p '(:a 1 :b 2 :c 3)))
  (should-not (json-plist-p '(a 1 b 2 c 3)))
  (should-not (json-plist-p '("a" 1 "b" 2 "c" 3)))
  (should-not (json-plist-p '(:a :b :c)))
  (should-not (json-plist-p '((:a 1) (:b 2) (:c 3)))))

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

(ert-deftest test-json-advance ()
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    (json-advance 0)
    (should (= (point) (point-min)))
    (json-advance 3)
    (should (= (point) (+ (point-min) 3)))))

(ert-deftest test-json-peek ()
  (json-tests--with-temp-buffer ""
    (should (zerop (json-peek))))
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    (should (equal (json-peek) ?{))))

(ert-deftest test-json-pop ()
  (json-tests--with-temp-buffer ""
    (should-error (json-pop) :type 'json-end-of-file))
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    (should (equal (json-pop) ?{))
    (should (= (point) (+ (point-min) 1)))))

(ert-deftest test-json-skip-whitespace ()
  (json-tests--with-temp-buffer "\t\r\n\f\b { \"a\": 1 }"
    (json-skip-whitespace)
    (should (equal (char-after) ?\f)))
  (json-tests--with-temp-buffer "\t\r\n\t { \"a\": 1 }"
    (json-skip-whitespace)
    (should (equal (char-after) ?{))))

;;; Paths

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

;;; Keywords

(ert-deftest test-json-read-keyword ()
  (json-tests--with-temp-buffer "true"
    (should (json-read-keyword "true")))
  (json-tests--with-temp-buffer "true"
    (should-error
     (json-read-keyword "false") :type 'json-unknown-keyword))
  (json-tests--with-temp-buffer "foo"
    (should-error
     (json-read-keyword "foo") :type 'json-unknown-keyword)))

(ert-deftest test-json-encode-keyword ()
  (should (equal (json-encode-keyword t) "true"))
  (should (equal (json-encode-keyword json-false) "false"))
  (should (equal (json-encode-keyword json-null) "null")))

;;; Numbers

(ert-deftest test-json-read-number ()
  (json-tests--with-temp-buffer "3"
    (should (= (json-read-number) 3)))
  (json-tests--with-temp-buffer "-5"
    (should (= (json-read-number) -5)))
  (json-tests--with-temp-buffer "123.456"
    (should (= (json-read-number) 123.456)))
  (json-tests--with-temp-buffer "1e3"
    (should (= (json-read-number) 1e3)))
  (json-tests--with-temp-buffer "2e+3"
    (should (= (json-read-number) 2e3)))
  (json-tests--with-temp-buffer "3E3"
    (should (= (json-read-number) 3e3)))
  (json-tests--with-temp-buffer "1e-7"
    (should (= (json-read-number) 1e-7)))
  (json-tests--with-temp-buffer "abc"
    (should-error (json-read-number) :type 'json-number-format)))

(ert-deftest test-json-encode-number ()
  (should (equal (json-encode-number 3) "3"))
  (should (equal (json-encode-number -5) "-5"))
  (should (equal (json-encode-number 123.456) "123.456")))

;; Strings

(ert-deftest test-json-read-escaped-char ()
  (json-tests--with-temp-buffer "\\\""
    (should (equal (json-read-escaped-char) ?\"))))

(ert-deftest test-json-read-string ()
  (json-tests--with-temp-buffer "\"formfeed\f\""
    (should-error (json-read-string) :type 'json-string-format))
  (json-tests--with-temp-buffer "\"foo \\\"bar\\\"\""
    (should (equal (json-read-string) "foo \"bar\"")))
  (json-tests--with-temp-buffer "\"abcαβγ\""
    (should (equal (json-read-string) "abcαβγ")))
  (json-tests--with-temp-buffer "\"\\nasd\\u0444\\u044b\\u0432fgh\\t\""
    (should (equal (json-read-string) "\nasdфывfgh\t")))
  ;; Bug#24784
  (json-tests--with-temp-buffer "\"\\uD834\\uDD1E\""
    (should (equal (json-read-string) "\U0001D11E")))
  (json-tests--with-temp-buffer "foo"
    (should-error (json-read-string) :type 'json-string-format)))

(ert-deftest test-json-encode-string ()
  (should (equal (json-encode-string "foo") "\"foo\""))
  (should (equal (json-encode-string "a\n\fb") "\"a\\n\\fb\""))
  (should (equal (json-encode-string "\nasdфыв\u001f\u007ffgh\t")
                 "\"\\nasdфыв\\u001f\u007ffgh\\t\"")))

(ert-deftest test-json-encode-key ()
  (should (equal (json-encode-key "foo") "\"foo\""))
  (should (equal (json-encode-key 'foo) "\"foo\""))
  (should (equal (json-encode-key :foo) "\"foo\""))
  (should-error (json-encode-key 5) :type 'json-key-format)
  (should-error (json-encode-key ["foo"]) :type 'json-key-format)
  (should-error (json-encode-key '("foo")) :type 'json-key-format))

;;; Objects

(ert-deftest test-json-new-object ()
  (let ((json-object-type 'alist))
    (should (equal (json-new-object) '())))
  (let ((json-object-type 'plist))
    (should (equal (json-new-object) '())))
  (let* ((json-object-type 'hash-table)
         (json-object (json-new-object)))
    (should (hash-table-p json-object))
    (should (= (hash-table-count json-object) 0))))

(ert-deftest test-json-add-to-object ()
  (let* ((json-object-type 'alist)
         (json-key-type nil)
         (obj (json-new-object)))
    (setq obj (json-add-to-object obj "a" 1))
    (setq obj (json-add-to-object obj "b" 2))
    (should (equal (assq 'a obj) '(a . 1)))
    (should (equal (assq 'b obj) '(b . 2))))
  (let* ((json-object-type 'plist)
         (json-key-type nil)
         (obj (json-new-object)))
    (setq obj (json-add-to-object obj "a" 1))
    (setq obj (json-add-to-object obj "b" 2))
    (should (= (plist-get obj :a) 1))
    (should (= (plist-get obj :b) 2)))
  (let* ((json-object-type 'hash-table)
         (json-key-type nil)
         (obj (json-new-object)))
    (setq obj (json-add-to-object obj "a" 1))
    (setq obj (json-add-to-object obj "b" 2))
    (should (= (gethash "a" obj) 1))
    (should (= (gethash "b" obj) 2))))

(ert-deftest test-json-read-object ()
  (json-tests--with-temp-buffer "{ \"a\": 1, \"b\": 2 }"
    (let ((json-object-type 'alist))
      (should (equal (json-read-object) '((a . 1) (b . 2))))))
  (json-tests--with-temp-buffer "{ \"a\": 1, \"b\": 2 }"
    (let ((json-object-type 'plist))
      (should (equal (json-read-object) '(:a 1 :b 2)))))
  (json-tests--with-temp-buffer "{ \"a\": 1, \"b\": 2 }"
    (let* ((json-object-type 'hash-table)
           (hash-table (json-read-object)))
      (should (= (gethash "a" hash-table) 1))
      (should (= (gethash "b" hash-table) 2))))
  (json-tests--with-temp-buffer "{ \"a\": 1 \"b\": 2 }"
    (should-error (json-read-object) :type 'json-object-format)))

(ert-deftest test-json-encode-hash-table ()
  (let ((hash-table (make-hash-table))
        (json-encoding-object-sort-predicate 'string<)
        (json-encoding-pretty-print nil))
    (puthash :a 1 hash-table)
    (puthash :b 2 hash-table)
    (puthash :c 3 hash-table)
    (should (equal (json-encode hash-table)
                   "{\"a\":1,\"b\":2,\"c\":3}"))))

(ert-deftest json-encode-simple-alist ()
  (let ((json-encoding-pretty-print nil))
    (should (equal (json-encode '((a . 1) (b . 2)))
                   "{\"a\":1,\"b\":2}"))))

(ert-deftest test-json-encode-plist ()
  (let ((plist '(:a 1 :b 2))
        (json-encoding-pretty-print nil))
    (should (equal (json-encode plist) "{\"a\":1,\"b\":2}"))))

(ert-deftest test-json-encode-plist-with-sort-predicate ()
  (let ((plist '(:c 3 :a 1 :b 2))
        (json-encoding-object-sort-predicate 'string<)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode plist) "{\"a\":1,\"b\":2,\"c\":3}"))))

(ert-deftest test-json-encode-alist-with-sort-predicate ()
  (let ((alist '((:c . 3) (:a . 1) (:b . 2)))
        (json-encoding-object-sort-predicate 'string<)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode alist) "{\"a\":1,\"b\":2,\"c\":3}"))))

(ert-deftest test-json-encode-list ()
  (let ((json-encoding-pretty-print nil))
    (should (equal (json-encode-list '(:a 1 :b 2))
                   "{\"a\":1,\"b\":2}"))
    (should (equal (json-encode-list '((:a . 1) (:b . 2)))
                   "{\"a\":1,\"b\":2}"))
    (should (equal (json-encode-list '(1 2 3 4)) "[1,2,3,4]"))))

;;; Arrays

(ert-deftest test-json-read-array ()
  (let ((json-array-type 'vector))
    (json-tests--with-temp-buffer "[1, 2, \"a\", \"b\"]"
      (should (equal (json-read-array) [1 2 "a" "b"]))))
  (let ((json-array-type 'list))
    (json-tests--with-temp-buffer "[1, 2, \"a\", \"b\"]"
      (should (equal (json-read-array) '(1 2 "a" "b")))))
  (json-tests--with-temp-buffer "[1 2]"
    (should-error (json-read-array) :type 'json-error)))

(ert-deftest test-json-encode-array ()
  (let ((json-encoding-pretty-print nil))
    (should (equal (json-encode-array [1 2 "a" "b"])
                   "[1,2,\"a\",\"b\"]"))))

;;; Reader

(ert-deftest test-json-read ()
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    ;; We don't care exactly what the return value is (that is tested
    ;; in `test-json-read-object'), but it should parse without error.
    (should (json-read)))
  (json-tests--with-temp-buffer ""
    (should-error (json-read) :type 'json-end-of-file))
  (json-tests--with-temp-buffer "xxx"
    (should-error (json-read) :type 'json-readtable-error)))

(ert-deftest test-json-read-from-string ()
  (let ((json-string "{ \"a\": 1 }"))
    (json-tests--with-temp-buffer json-string
      (should (equal (json-read-from-string json-string)
                     (json-read))))))

;;; JSON encoder

(ert-deftest test-json-encode ()
  (should (equal (json-encode "foo") "\"foo\""))
  (with-temp-buffer
    (should-error (json-encode (current-buffer)) :type 'json-error)))

(provide 'json-tests)
;;; json-tests.el ends here
