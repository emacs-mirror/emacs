;;; json-tests.el --- Test suite for json.el  -*- lexical-binding:t -*-

;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>

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
(require 'json)
(require 'map)
(require 'seq)

(eval-when-compile
  (require 'cl-lib))

(defmacro json-tests--with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and evaluate BODY there.
Point is moved to beginning of the buffer."
  (declare (debug t) (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Utilities

(ert-deftest test-json-alist-p ()
  (should (json-alist-p '()))
  (should (json-alist-p '((()))))
  (should (json-alist-p '((a))))
  (should (json-alist-p '((a . 1))))
  (should (json-alist-p '((a . 1) (b 2) (c))))
  (should (json-alist-p '((:a) (:b 2) (:c . 3))))
  (should (json-alist-p '(("a" . 1) ("b" 2) ("c"))))
  (should-not (json-alist-p '(())))
  (should-not (json-alist-p '(a)))
  (should-not (json-alist-p '(a . 1)))
  (should-not (json-alist-p '((a . 1) . [])))
  (should-not (json-alist-p '((a . 1) [])))
  (should-not (json-alist-p '(:a :b :c)))
  (should-not (json-alist-p '(:a 1 :b 2 :c 3)))
  (should-not (json-alist-p '((:a 1) (:b 2) 3)))
  (should-not (json-alist-p '((:a 1) (:b 2) ())))
  (should-not (json-alist-p '(((a) 1) (b 2) (c 3))))
  (should-not (json-alist-p []))
  (should-not (json-alist-p [(a . 1)]))
  (should-not (json-alist-p #s(hash-table))))

(ert-deftest test-json-plist-p ()
  (should (json-plist-p '()))
  (should (json-plist-p '(:a 1)))
  (should (json-plist-p '(:a 1 :b 2 :c 3)))
  (should (json-plist-p '(:a :b)))
  (should (json-plist-p '(:a :b :c :d)))
  (should-not (json-plist-p '(a)))
  (should-not (json-plist-p '(a 1)))
  (should-not (json-plist-p '(a 1 b 2 c 3)))
  (should-not (json-plist-p '("a" 1 "b" 2 "c" 3)))
  (should-not (json-plist-p '(:a)))
  (should-not (json-plist-p '(:a :b :c)))
  (should-not (json-plist-p '(:a 1 :b 2 :c)))
  (should-not (json-plist-p '((:a 1))))
  (should-not (json-plist-p '((:a 1) (:b 2) (:c 3))))
  (should-not (json-plist-p []))
  (should-not (json-plist-p [:a 1]))
  (should-not (json-plist-p #s(hash-table))))

(ert-deftest test-json-plist-nreverse ()
  (should (equal (json--plist-nreverse '()) '()))
  (should (equal (json--plist-nreverse (list :a 1)) '(:a 1)))
  (should (equal (json--plist-nreverse (list :a 1 :b 2)) '(:b 2 :a 1)))
  (should (equal (json--plist-nreverse (list :a 1 :b 2 :c 3))
                 '(:c 3 :b 2 :a 1))))

(ert-deftest test-json-advance ()
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    (json-advance 0)
    (should (bobp))
    (json-advance)
    (should (= (point) (1+ (point-min))))
    (json-advance 0)
    (should (= (point) (1+ (point-min))))
    (json-advance 1)
    (should (= (point) (+ (point-min) 2)))
    (json-advance 3)
    (should (= (point) (+ (point-min) 5)))))

(ert-deftest test-json-peek ()
  (json-tests--with-temp-buffer ""
    (should (zerop (json-peek))))
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    (should (= (json-peek) ?\{))
    (goto-char (1- (point-max)))
    (should (= (json-peek) ?\}))
    (json-advance)
    (should (zerop (json-peek)))))

(ert-deftest test-json-pop ()
  (json-tests--with-temp-buffer ""
    (should-error (json-pop) :type 'json-end-of-file))
  (json-tests--with-temp-buffer "{ \"a\": 1 }"
    (should (= (json-pop) ?\{))
    (should (= (point) (1+ (point-min))))
    (goto-char (1- (point-max)))
    (should (= (json-pop) ?\}))
    (should-error (json-pop) :type 'json-end-of-file)))

(ert-deftest test-json-skip-whitespace ()
  (json-tests--with-temp-buffer ""
    (json-skip-whitespace)
    (should (bobp))
    (should (eobp)))
  (json-tests--with-temp-buffer "{}"
    (json-skip-whitespace)
    (should (bobp))
    (json-advance)
    (json-skip-whitespace)
    (should (= (point) (1+ (point-min))))
    (json-advance)
    (json-skip-whitespace)
    (should (eobp)))
  (json-tests--with-temp-buffer "\t\r\n\f\b { \"a\": 1 }"
    (json-skip-whitespace)
    (should (= (json-peek) ?\f)))
  (json-tests--with-temp-buffer "\t\r\n\t { \"a\": 1 }"
    (json-skip-whitespace)
    (should (= (json-peek) ?\{))))

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
    (should-not matched-path)))

;;; Keywords

(ert-deftest test-json-read-keyword ()
  (json-tests--with-temp-buffer "true"
    (should (eq (json-read-keyword "true") t))
    (should (eobp)))
  (json-tests--with-temp-buffer "true "
    (should (eq (json-read-keyword "true") t))
    (should (eobp)))
  (json-tests--with-temp-buffer "true}"
    (should (eq (json-read-keyword "true") t))
    (should (= (point) (+ (point-min) 4))))
  (json-tests--with-temp-buffer "true false"
    (should (eq (json-read-keyword "true") t))
    (should (= (point) (+ (point-min) 5))))
  (json-tests--with-temp-buffer "true }"
    (should (eq (json-read-keyword "true") t))
    (should (= (point) (+ (point-min) 5))))
  (json-tests--with-temp-buffer "true |"
    (should (eq (json-read-keyword "true") t))
    (should (= (point) (+ (point-min) 5))))
  (json-tests--with-temp-buffer "false"
    (let ((json-false 'false))
      (should (eq (json-read-keyword "false") 'false)))
    (should (eobp)))
  (json-tests--with-temp-buffer "null"
    (let ((json-null 'null))
      (should (eq (json-read-keyword "null") 'null)))
    (should (eobp))))

(ert-deftest test-json-read-keyword-invalid ()
  (json-tests--with-temp-buffer ""
    (should (equal (should-error (json-read-keyword ""))
                   '(json-unknown-keyword "")))
    (should (equal (should-error (json-read-keyword "true"))
                   '(json-unknown-keyword ()))))
  (json-tests--with-temp-buffer "true"
    (should (equal (should-error (json-read-keyword "false"))
                   '(json-unknown-keyword "true"))))
  (json-tests--with-temp-buffer "foo"
    (should (equal (should-error (json-read-keyword "foo"))
                   '(json-unknown-keyword "foo")))
    (should (equal (should-error (json-read-keyword "bar"))
                   '(json-unknown-keyword "bar"))))
  (json-tests--with-temp-buffer " true"
    (should (equal (should-error (json-read-keyword "true"))
                   '(json-unknown-keyword ()))))
  (json-tests--with-temp-buffer "truefalse"
    (should (equal (should-error (json-read-keyword "true"))
                   '(json-unknown-keyword "truefalse"))))
  (json-tests--with-temp-buffer "true|"
    (should (equal (should-error (json-read-keyword "true"))
                   '(json-unknown-keyword "true")))))

(ert-deftest test-json-encode-keyword ()
  (should (equal (json-encode-keyword t) "true"))
  (let ((json-false 'false))
    (should (equal (json-encode-keyword 'false) "false"))
    (should (equal (json-encode-keyword json-false) "false")))
  (let ((json-null 'null))
    (should (equal (json-encode-keyword 'null) "null"))
    (should (equal (json-encode-keyword json-null) "null"))))

;;; Numbers

(ert-deftest test-json-read-integer ()
  (json-tests--with-temp-buffer "0 "
    (should (= (json-read-number) 0))
    (should (eobp)))
  (json-tests--with-temp-buffer "-0 "
    (should (= (json-read-number) 0))
    (should (eobp)))
  (json-tests--with-temp-buffer "3 "
    (should (= (json-read-number) 3))
    (should (eobp)))
  (json-tests--with-temp-buffer "-10 "
    (should (= (json-read-number) -10))
    (should (eobp)))
  (json-tests--with-temp-buffer (format "%d " (1+ most-positive-fixnum))
    (should (= (json-read-number) (1+ most-positive-fixnum)))
    (should (eobp)))
  (json-tests--with-temp-buffer (format "%d " (1- most-negative-fixnum))
    (should (= (json-read-number) (1- most-negative-fixnum)))
    (should (eobp))))

(ert-deftest test-json-read-fraction ()
  (json-tests--with-temp-buffer "0.0 "
    (should (= (json-read-number) 0.0))
    (should (eobp)))
  (json-tests--with-temp-buffer "-0.0 "
    (should (= (json-read-number) 0.0))
    (should (eobp)))
  (json-tests--with-temp-buffer "0.01 "
    (should (= (json-read-number) 0.01))
    (should (eobp)))
  (json-tests--with-temp-buffer "-0.01 "
    (should (= (json-read-number) -0.01))
    (should (eobp)))
  (json-tests--with-temp-buffer "123.456 "
    (should (= (json-read-number) 123.456))
    (should (eobp)))
  (json-tests--with-temp-buffer "-123.456 "
    (should (= (json-read-number) -123.456))
    (should (eobp))))

(ert-deftest test-json-read-exponent ()
  (json-tests--with-temp-buffer "0e0 "
    (should (= (json-read-number) 0e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "-0E0 "
    (should (= (json-read-number) 0e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "-0E+0 "
    (should (= (json-read-number) 0e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "0e-0 "
    (should (= (json-read-number) 0e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "12e34 "
    (should (= (json-read-number) 12e34))
    (should (eobp)))
  (json-tests--with-temp-buffer "-12E34 "
    (should (= (json-read-number) -12e34))
    (should (eobp)))
  (json-tests--with-temp-buffer "-12E+34 "
    (should (= (json-read-number) -12e34))
    (should (eobp)))
  (json-tests--with-temp-buffer "12e-34 "
    (should (= (json-read-number) 12e-34))
    (should (eobp))))

(ert-deftest test-json-read-fraction-exponent ()
  (json-tests--with-temp-buffer "0.0e0 "
    (should (= (json-read-number) 0.0e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "-0.0E0 "
    (should (= (json-read-number) 0.0e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "0.12E-0 "
    (should (= (json-read-number) 0.12e0))
    (should (eobp)))
  (json-tests--with-temp-buffer "-12.34e+56 "
    (should (= (json-read-number) -12.34e+56))
    (should (eobp))))

(ert-deftest test-json-read-number-invalid ()
  (cl-flet ((read (str)
                  ;; Return error and point resulting from reading STR.
                  (json-tests--with-temp-buffer str
                    (cons (should-error (json-read-number)) (point)))))
    ;; POS is where each of its STRINGS becomes invalid.
    (pcase-dolist (`(,pos . ,strings)
                   '((1 "" "+" "-" "." "e" "e1" "abc" "++0" "++1"
                        "+0"  "+0.0" "+12" "+12.34" "+12.34e56"
                        ".0" "+.0" "-.0" ".12" "+.12" "-.12"
                        ".e0" "+.e0" "-.e0" ".0e0" "+.0e0" "-.0e0")
                     (2 "01" "1ee1" "1e++1")
                     (3 "-01")
                     (4 "0.0.0" "1.1.1" "1e1e1")
                     (5 "-0.0.0" "-1.1.1")))
      ;; Expected error and point.
      (let ((res `((json-number-format ,pos) . ,pos)))
        (dolist (str strings)
          (should (equal (read str) res)))))))

(ert-deftest test-json-encode-number ()
  (should (equal (json-encode 0) "0"))
  (should (equal (json-encode -0) "0"))
  (should (equal (json-encode 3) "3"))
  (should (equal (json-encode -5) "-5"))
  (should (equal (json-encode 123.456) "123.456"))
  (let ((bignum (1+ most-positive-fixnum)))
    (should (equal (json-encode bignum)
                   (number-to-string bignum)))))

;;; Strings

(ert-deftest test-json-read-escaped-char ()
  (json-tests--with-temp-buffer "\\\""
    (should (= (json-read-escaped-char) ?\"))
    (should (eobp)))
  (json-tests--with-temp-buffer "\\\\ "
    (should (= (json-read-escaped-char) ?\\))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\b "
    (should (= (json-read-escaped-char) ?\b))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\f "
    (should (= (json-read-escaped-char) ?\f))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\n "
    (should (= (json-read-escaped-char) ?\n))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\r "
    (should (= (json-read-escaped-char) ?\r))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\t "
    (should (= (json-read-escaped-char) ?\t))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\x "
    (should (= (json-read-escaped-char) ?x))
    (should (= (point) (+ (point-min) 2))))
  (json-tests--with-temp-buffer "\\ud800\\uDC00 "
    (should (= (json-read-escaped-char) #x10000))
    (should (= (point) (+ (point-min) 12))))
  (json-tests--with-temp-buffer "\\ud7ff\\udc00 "
    (should (= (json-read-escaped-char) #xd7ff))
    (should (= (point) (+ (point-min) 6))))
  (json-tests--with-temp-buffer "\\uffff "
    (should (= (json-read-escaped-char) #xffff))
    (should (= (point) (+ (point-min) 6))))
  (json-tests--with-temp-buffer "\\ufffff "
    (should (= (json-read-escaped-char) #xffff))
    (should (= (point) (+ (point-min) 6)))))

(ert-deftest test-json-read-escaped-char-invalid ()
  (json-tests--with-temp-buffer ""
    (should-error (json-read-escaped-char)))
  (json-tests--with-temp-buffer "\\"
    (should-error (json-read-escaped-char) :type 'json-end-of-file))
  (json-tests--with-temp-buffer "\\ufff "
    (should (equal (should-error (json-read-escaped-char))
                   (list 'json-string-escape (+ (point-min) 2)))))
  (json-tests--with-temp-buffer "\\ufffg "
    (should (equal (should-error (json-read-escaped-char))
                   (list 'json-string-escape (+ (point-min) 2))))))

(ert-deftest test-json-read-string ()
  (json-tests--with-temp-buffer ""
    (should-error (json-read-string)))
  (json-tests--with-temp-buffer "\"formfeed\f\""
    (should (equal (should-error (json-read-string))
                   '(json-string-format ?\f))))
  (json-tests--with-temp-buffer "\"\""
    (should (equal (json-read-string) "")))
  (json-tests--with-temp-buffer "\"foo \\\"bar\\\"\""
    (should (equal (json-read-string) "foo \"bar\"")))
  (json-tests--with-temp-buffer "\"abcαβγ\""
    (should (equal (json-read-string) "abcαβγ")))
  (json-tests--with-temp-buffer "\"\\nasd\\u0444\\u044b\\u0432fgh\\t\""
    (should (equal (json-read-string) "\nasdфывfgh\t")))
  (json-tests--with-temp-buffer "\"abc\uFFFFαβγ𝔸𝐁𝖢\\\"\\\\\""
    (should (equal (json-read-string) "abc\uFFFFαβγ𝔸𝐁𝖢\"\\")))
  ;; Bug#24784
  (json-tests--with-temp-buffer "\"\\uD834\\uDD1E\""
    (should (equal (json-read-string) "\U0001D11E")))
  (json-tests--with-temp-buffer "f"
    (should-error (json-read-string) :type 'json-end-of-file))
  (json-tests--with-temp-buffer "foo"
    (should-error (json-read-string) :type 'json-end-of-file)))

(ert-deftest test-json-encode-string ()
  (should (equal (json-encode-string "") "\"\""))
  (should (equal (json-encode-string "a") "\"a\""))
  (should (equal (json-encode-string "foo") "\"foo\""))
  (should (equal (json-encode-string "a\n\fb") "\"a\\n\\fb\""))
  (should (equal (json-encode-string "\nasdфыв\u001f\u007ffgh\t")
                 "\"\\nasdфыв\\u001f\u007ffgh\\t\""))
  ;; Bug#43549.
  (should (equal (json-encode-string (propertize "foo" 'read-only t))
                 "\"foo\""))
  (should (equal (json-encode-string "a\0b") "\"a\\u0000b\""))
  (should (equal (json-encode-string "abc\uFFFFαβγ𝔸𝐁𝖢\"\\")
                 "\"abc\uFFFFαβγ𝔸𝐁𝖢\\\"\\\\\"")))

(ert-deftest test-json-encode-key ()
  (with-suppressed-warnings ((obsolete json-encode-key))
    (should (equal (json-encode-key '##) "\"\""))
    (should (equal (json-encode-key :) "\"\""))
    (should (equal (json-encode-key "") "\"\""))
    (should (equal (json-encode-key 'a) "\"a\""))
    (should (equal (json-encode-key :a) "\"a\""))
    (should (equal (json-encode-key "a") "\"a\""))
    (should (equal (json-encode-key t) "\"t\""))
    (should (equal (json-encode-key :t) "\"t\""))
    (should (equal (json-encode-key "t") "\"t\""))
    (should (equal (json-encode-key nil) "\"nil\""))
    (should (equal (json-encode-key :nil) "\"nil\""))
    (should (equal (json-encode-key "nil") "\"nil\""))
    (should (equal (json-encode-key ":a") "\":a\""))
    (should (equal (json-encode-key ":t") "\":t\""))
    (should (equal (json-encode-key ":nil") "\":nil\""))
    (should (equal (should-error (json-encode-key 5))
                   '(json-key-format 5)))
    (should (equal (should-error (json-encode-key ["foo"]))
                   '(json-key-format ["foo"])))
    (should (equal (should-error (json-encode-key '("foo")))
                   '(json-key-format ("foo"))))))

;;; Objects

(ert-deftest test-json-new-object ()
  (let ((json-object-type 'alist))
    (should-not (json-new-object)))
  (let ((json-object-type 'plist))
    (should-not (json-new-object)))
  (let* ((json-object-type 'hash-table)
         (json-object (json-new-object)))
    (should (hash-table-p json-object))
    (should (map-empty-p json-object))
    (should (eq (hash-table-test json-object) #'equal))))

(ert-deftest test-json-add-to-alist ()
  (let* ((json-object-type 'alist)
         (obj (json-new-object)))
    (let ((json-key-type nil))
      (setq obj (json-add-to-object obj "a" 1))
      (setq obj (json-add-to-object obj "b" 2))
      (should (equal (assq 'a obj) '(a . 1)))
      (should (equal (assq 'b obj) '(b . 2))))
    (let ((json-key-type 'symbol))
      (setq obj (json-add-to-object obj "c" 3))
      (setq obj (json-add-to-object obj "d" 4))
      (should (equal (assq 'c obj) '(c . 3)))
      (should (equal (assq 'd obj) '(d . 4))))
    (let ((json-key-type 'keyword))
      (setq obj (json-add-to-object obj "e" 5))
      (setq obj (json-add-to-object obj "f" 6))
      (should (equal (assq :e obj) '(:e . 5)))
      (should (equal (assq :f obj) '(:f . 6))))
    (let ((json-key-type 'string))
      (setq obj (json-add-to-object obj "g" 7))
      (setq obj (json-add-to-object obj "h" 8))
      (should (equal (assoc "g" obj) '("g" . 7)))
      (should (equal (assoc "h" obj) '("h" . 8))))))

(ert-deftest test-json-add-to-plist ()
  (let* ((json-object-type 'plist)
         (obj (json-new-object)))
    (let ((json-key-type nil))
      (setq obj (json-add-to-object obj "a" 1))
      (setq obj (json-add-to-object obj "b" 2))
      (should (= (plist-get obj :a) 1))
      (should (= (plist-get obj :b) 2)))
    (let ((json-key-type 'keyword))
      (setq obj (json-add-to-object obj "c" 3))
      (setq obj (json-add-to-object obj "d" 4))
      (should (= (plist-get obj :c) 3))
      (should (= (plist-get obj :d) 4)))
    (let ((json-key-type 'symbol))
      (setq obj (json-add-to-object obj "e" 5))
      (setq obj (json-add-to-object obj "f" 6))
      (should (= (plist-get obj 'e) 5))
      (should (= (plist-get obj 'f) 6)))
    (let ((json-key-type 'string))
      (setq obj (json-add-to-object obj "g" 7))
      (setq obj (json-add-to-object obj "h" 8))
      (should (= (plist-get obj "g" #'equal) 7))
      (should (= (plist-get obj "h" #'equal) 8)))))

(ert-deftest test-json-add-to-hash-table ()
  (let* ((json-object-type 'hash-table)
         (obj (json-new-object)))
    (let ((json-key-type nil))
      (setq obj (json-add-to-object obj "a" 1))
      (setq obj (json-add-to-object obj "b" 2))
      (should (= (gethash "a" obj) 1))
      (should (= (gethash "b" obj) 2)))
    (let ((json-key-type 'string))
      (setq obj (json-add-to-object obj "c" 3))
      (setq obj (json-add-to-object obj "d" 4))
      (should (= (gethash "c" obj) 3))
      (should (= (gethash "d" obj) 4)))
    (let ((json-key-type 'symbol))
      (setq obj (json-add-to-object obj "e" 5))
      (setq obj (json-add-to-object obj "f" 6))
      (should (= (gethash 'e obj) 5))
      (should (= (gethash 'f obj) 6)))
    (let ((json-key-type 'keyword))
      (setq obj (json-add-to-object obj "g" 7))
      (setq obj (json-add-to-object obj "h" 8))
      (should (= (gethash :g obj) 7))
      (should (= (gethash :h obj) 8)))))

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
      (should (= (gethash "b" hash-table) 2)))))

(ert-deftest test-json-read-object-empty ()
  (json-tests--with-temp-buffer "{}"
    (let ((json-object-type 'alist))
      (should-not (save-excursion (json-read-object))))
    (let ((json-object-type 'plist))
      (should-not (save-excursion (json-read-object))))
    (let* ((json-object-type 'hash-table)
           (hash-table (json-read-object)))
      (should (hash-table-p hash-table))
      (should (map-empty-p hash-table)))))

(ert-deftest test-json-read-object-invalid ()
  (json-tests--with-temp-buffer "{ \"a\" 1, \"b\": 2 }"
    (should (equal (should-error (json-read-object))
                   '(json-object-format ":" ?1))))
  (json-tests--with-temp-buffer "{ \"a\": 1 \"b\": 2 }"
    (should (equal (should-error (json-read-object))
                   '(json-object-format "," ?\")))))

(ert-deftest test-json-read-object-function ()
  (let* ((pre nil)
         (post nil)
         (keys '("b" "a"))
         (json-pre-element-read-function
          (lambda (key)
            (setq pre 'pre)
            (should (equal key (pop keys)))))
         (json-post-element-read-function
          (lambda () (setq post 'post))))
    (json-tests--with-temp-buffer "{ \"b\": 2, \"a\": 1 }"
      (json-read-object)
      (should (eq pre 'pre))
      (should (eq post 'post)))))

(ert-deftest test-json-encode-hash-table ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode #s(hash-table)) "{}"))
    (should (equal (json-encode #s(hash-table data (a 1))) "{\"a\":1}"))
    (should (equal (json-encode #s(hash-table data (t 1))) "{\"t\":1}"))
    (should (equal (json-encode #s(hash-table data (nil 1))) "{\"nil\":1}"))
    (should (equal (json-encode #s(hash-table data (:a 1))) "{\"a\":1}"))
    (should (equal (json-encode #s(hash-table data (:t 1))) "{\"t\":1}"))
    (should (equal (json-encode #s(hash-table data (:nil 1))) "{\"nil\":1}"))
    (should (equal (json-encode #s(hash-table test equal data ("a" 1)))
                   "{\"a\":1}"))
    (should (equal (json-encode #s(hash-table test equal data ("t" 1)))
                   "{\"t\":1}"))
    (should (equal (json-encode #s(hash-table test equal data ("nil" 1)))
                   "{\"nil\":1}"))
    (should (equal (json-encode #s(hash-table test equal data (":a" 1)))
                   "{\":a\":1}"))
    (should (equal (json-encode #s(hash-table test equal data (":t" 1)))
                   "{\":t\":1}"))
    (should (equal (json-encode #s(hash-table test equal data (":nil" 1)))
                   "{\":nil\":1}"))
    (should (member (json-encode #s(hash-table data (t 2 :nil 1)))
                    '("{\"nil\":1,\"t\":2}" "{\"t\":2,\"nil\":1}")))
    (should (member (json-encode #s(hash-table test equal data (:t 2 ":t" 1)))
                    '("{\":t\":1,\"t\":2}" "{\"t\":2,\":t\":1}")))
    (should (member (json-encode #s(hash-table data (b 2 a 1)))
                    '("{\"a\":1,\"b\":2}" "{\"b\":2,\"a\":1}")))
    (should (member (json-encode #s(hash-table data (c 3 b 2 a 1)))
                    '("{\"a\":1,\"b\":2,\"c\":3}"
                      "{\"a\":1,\"c\":3,\"b\":2}"
                      "{\"b\":2,\"a\":1,\"c\":3}"
                      "{\"b\":2,\"c\":3,\"a\":1}"
                      "{\"c\":3,\"a\":1,\"b\":2}"
                      "{\"c\":3,\"b\":2,\"a\":1}")))))

(ert-deftest test-json-encode-hash-table-pretty ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings nil))
    (should (equal (json-encode #s(hash-table)) "{}"))
    (should (equal (json-encode #s(hash-table data (a 1))) "{\n \"a\": 1\n}"))
    (should (member (json-encode #s(hash-table data (b 2 a 1)))
                    '("{\n \"a\": 1,\n \"b\": 2\n}"
                      "{\n \"b\": 2,\n \"a\": 1\n}")))
    (should (member (json-encode #s(hash-table data (c 3 b 2 a 1)))
                    '("{\n \"a\": 1,\n \"b\": 2,\n \"c\": 3\n}"
                      "{\n \"a\": 1,\n \"c\": 3,\n \"b\": 2\n}"
                      "{\n \"b\": 2,\n \"a\": 1,\n \"c\": 3\n}"
                      "{\n \"b\": 2,\n \"c\": 3,\n \"a\": 1\n}"
                      "{\n \"c\": 3,\n \"a\": 1,\n \"b\": 2\n}"
                      "{\n \"c\": 3,\n \"b\": 2,\n \"a\": 1\n}")))))

(ert-deftest test-json-encode-hash-table-lisp-style ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings t))
    (should (equal (json-encode #s(hash-table)) "{}"))
    (should (equal (json-encode #s(hash-table data (a 1))) "{\n \"a\": 1}"))
    (should (member (json-encode #s(hash-table data (b 2 a 1)))
                    '("{\n \"a\": 1,\n \"b\": 2}"
                      "{\n \"b\": 2,\n \"a\": 1}")))
    (should (member (json-encode #s(hash-table data (c 3 b 2 a 1)))
                    '("{\n \"a\": 1,\n \"b\": 2,\n \"c\": 3}"
                      "{\n \"a\": 1,\n \"c\": 3,\n \"b\": 2}"
                      "{\n \"b\": 2,\n \"a\": 1,\n \"c\": 3}"
                      "{\n \"b\": 2,\n \"c\": 3,\n \"a\": 1}"
                      "{\n \"c\": 3,\n \"a\": 1,\n \"b\": 2}"
                      "{\n \"c\": 3,\n \"b\": 2,\n \"a\": 1}")))))

(ert-deftest test-json-encode-hash-table-sort ()
  (let ((json-encoding-object-sort-predicate #'string<)
        (json-encoding-pretty-print nil))
    (pcase-dolist (`(,in . ,out)
                   '((#s(hash-table) . "{}")
                     (#s(hash-table data (a 1)) . "{\"a\":1}")
                     (#s(hash-table data (b 2 a 1)) . "{\"a\":1,\"b\":2}")
                     (#s(hash-table data (c 3 b 2 a 1))
                        . "{\"a\":1,\"b\":2,\"c\":3}")))
      (let ((copy (map-pairs in)))
        (should (equal (json-encode in) out))
        ;; Ensure sorting isn't destructive.
        (should (seq-set-equal-p (map-pairs in) copy))))))

(ert-deftest test-json-encode-alist ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode-alist ()) "{}"))
    (should (equal (json-encode-alist '((a . 1) (t . 2) (nil . 3)))
                   "{\"a\":1,\"t\":2,\"nil\":3}"))
    (should (equal (json-encode-alist '((:a . 1) (:t . 2) (:nil . 3)))
                   "{\"a\":1,\"t\":2,\"nil\":3}"))
    (should (equal (json-encode-alist '(("a" . 1) ("t" . 2) ("nil" . 3)))
                   "{\"a\":1,\"t\":2,\"nil\":3}"))
    (should (equal (json-encode-alist '((":a" . 1) (":t" . 2) (":nil" . 3)))
                   "{\":a\":1,\":t\":2,\":nil\":3}"))
    (should (equal (json-encode-alist '((t . 1) (:nil . 2) (":nil" . 3)))
                   "{\"t\":1,\"nil\":2,\":nil\":3}"))
    (should (equal (json-encode-alist '((b . 2) (a . 1))) "{\"b\":2,\"a\":1}"))
    (should (equal (json-encode-alist '((c . 3) (b . 2) (a . 1)))
                   "{\"c\":3,\"b\":2,\"a\":1}"))))

(ert-deftest test-json-encode-alist-pretty ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings nil))
    (should (equal (json-encode-alist ()) "{}"))
    (should (equal (json-encode-alist '((a . 1))) "{\n \"a\": 1\n}"))
    (should (equal (json-encode-alist '((b . 2) (a . 1)))
                   "{\n \"b\": 2,\n \"a\": 1\n}"))
    (should (equal (json-encode-alist '((c . 3) (b . 2) (a . 1)))
                   "{\n \"c\": 3,\n \"b\": 2,\n \"a\": 1\n}"))))

(ert-deftest test-json-encode-alist-lisp-style ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings t))
    (should (equal (json-encode-alist ()) "{}"))
    (should (equal (json-encode-alist '((a . 1))) "{\n \"a\": 1}"))
    (should (equal (json-encode-alist '((b . 2) (a . 1)))
                   "{\n \"b\": 2,\n \"a\": 1}"))
    (should (equal (json-encode-alist '((c . 3) (b . 2) (a . 1)))
                   "{\n \"c\": 3,\n \"b\": 2,\n \"a\": 1}"))))

(ert-deftest test-json-encode-alist-sort ()
  (let ((json-encoding-object-sort-predicate #'string<)
        (json-encoding-pretty-print nil))
    (pcase-dolist (`(,in . ,out)
                   '((() . "{}")
                     (((a . 1)) . "{\"a\":1}")
                     (((b . 2) (a . 1)) . "{\"a\":1,\"b\":2}")
                     (((c . 3) (b . 2) (a . 1))
                      . "{\"a\":1,\"b\":2,\"c\":3}")))
      (let ((copy (copy-alist in)))
        (should (equal (json-encode-alist in) out))
        ;; Ensure sorting isn't destructive (bug#40693).
        (should (equal in copy))))))

(ert-deftest test-json-encode-plist ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode-plist ()) "{}"))
    (should (equal (json-encode-plist '(:a 1)) "{\"a\":1}"))
    (should (equal (json-encode-plist '(:b 2 :a 1)) "{\"b\":2,\"a\":1}"))
    (should (equal (json-encode-plist '(":d" 4 "c" 3 b 2 :a 1))
                   "{\":d\":4,\"c\":3,\"b\":2,\"a\":1}"))
    (should (equal (json-encode-plist '(nil 2 t 1))
                   "{\"nil\":2,\"t\":1}"))
    (should (equal (json-encode-plist '(:nil 2 :t 1))
                   "{\"nil\":2,\"t\":1}"))
    (should (equal (json-encode-plist '(":nil" 4 "nil" 3 ":t" 2 "t" 1))
                   "{\":nil\":4,\"nil\":3,\":t\":2,\"t\":1}"))))

(ert-deftest test-json-encode-plist-pretty ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings nil))
    (should (equal (json-encode-plist ()) "{}"))
    (should (equal (json-encode-plist '(:a 1)) "{\n \"a\": 1\n}"))
    (should (equal (json-encode-plist '(:b 2 :a 1))
                   "{\n \"b\": 2,\n \"a\": 1\n}"))
    (should (equal (json-encode-plist '(:c 3 :b 2 :a 1))
                   "{\n \"c\": 3,\n \"b\": 2,\n \"a\": 1\n}"))))

(ert-deftest test-json-encode-plist-lisp-style ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings t))
    (should (equal (json-encode-plist ()) "{}"))
    (should (equal (json-encode-plist '(:a 1)) "{\n \"a\": 1}"))
    (should (equal (json-encode-plist '(:b 2 :a 1))
                   "{\n \"b\": 2,\n \"a\": 1}"))
    (should (equal (json-encode-plist '(:c 3 :b 2 :a 1))
                   "{\n \"c\": 3,\n \"b\": 2,\n \"a\": 1}"))))

(ert-deftest test-json-encode-plist-sort ()
  (let ((json-encoding-object-sort-predicate #'string<)
        (json-encoding-pretty-print nil))
    (pcase-dolist (`(,in . ,out)
                   '((() . "{}")
                     ((:a 1) . "{\"a\":1}")
                     ((:b 2 :a 1) . "{\"a\":1,\"b\":2}")
                     ((:c 3 :b 2 :a 1) . "{\"a\":1,\"b\":2,\"c\":3}")))
      (let ((copy (copy-sequence in)))
        (should (equal (json-encode-plist in) out))
        ;; Ensure sorting isn't destructive.
        (should (equal in copy))))))

(ert-deftest test-json-encode-list ()
  "Test `json-encode-list' or its more moral equivalents."
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print nil))
    ;; Trick `json-encode' into using `json--print-list'.
    (let ((json-null (list nil)))
      (should (equal (json-encode ()) "{}")))
    (should (equal (json-encode '(a)) "[\"a\"]"))
    (should (equal (json-encode '(:a)) "[\"a\"]"))
    (should (equal (json-encode '("a")) "[\"a\"]"))
    (should (equal (json-encode '(a 1)) "[\"a\",1]"))
    (should (equal (json-encode '("a" 1)) "[\"a\",1]"))
    (should (equal (json-encode '(:a 1)) "{\"a\":1}"))
    (should (equal (json-encode '((a . 1))) "{\"a\":1}"))
    (should (equal (json-encode '((:a . 1))) "{\"a\":1}"))
    (should (equal (json-encode '(:b 2 :a)) "[\"b\",2,\"a\"]"))
    (should (equal (json-encode '(4 3 2 1)) "[4,3,2,1]"))
    (should (equal (json-encode '(b 2 a 1)) "[\"b\",2,\"a\",1]"))
    (should (equal (json-encode '(:b 2 :a 1)) "{\"b\":2,\"a\":1}"))
    (should (equal (json-encode '((b . 2) (a . 1))) "{\"b\":2,\"a\":1}"))
    (should (equal (json-encode '((:b . 2) (:a . 1)))
                   "{\"b\":2,\"a\":1}"))
    (should (equal (json-encode '((a) 1)) "[[\"a\"],1]"))
    (should (equal (json-encode '((:a) 1)) "[[\"a\"],1]"))
    (should (equal (json-encode '(("a") 1)) "[[\"a\"],1]"))
    (should (equal (json-encode '((a 1) 2)) "[[\"a\",1],2]"))
    (should (equal (json-encode '((:a 1) 2)) "[{\"a\":1},2]"))
    (should (equal (json-encode '(((a . 1)) 2)) "[{\"a\":1},2]"))
    (should (equal (json-encode '(:a 1 :b (2))) "{\"a\":1,\"b\":[2]}"))
    (should (equal (json-encode '((a . 1) (b 2))) "{\"a\":1,\"b\":[2]}"))
    (should-error (json-encode '(a . 1)) :type 'wrong-type-argument)
    (should-error (json-encode '((a . 1) 2)) :type 'wrong-type-argument)
    (with-suppressed-warnings ((obsolete json-encode-list))
      (should (equal (should-error (json-encode-list []))
                     '(json-error [])))
      (should (equal (should-error (json-encode-list [a]))
                     '(json-error [a]))))))

;;; Arrays

(ert-deftest test-json-read-array ()
  (let ((json-array-type 'vector))
    (json-tests--with-temp-buffer "[]"
      (should (equal (json-read-array) [])))
    (json-tests--with-temp-buffer "[ ]"
      (should (equal (json-read-array) [])))
    (json-tests--with-temp-buffer "[1]"
      (should (equal (json-read-array) [1])))
    (json-tests--with-temp-buffer "[1, 2, \"a\", \"b\"]"
      (should (equal (json-read-array) [1 2 "a" "b"]))))
  (let ((json-array-type 'list))
    (json-tests--with-temp-buffer "[]"
      (should-not (json-read-array)))
    (json-tests--with-temp-buffer "[ ]"
      (should-not (json-read-array)))
    (json-tests--with-temp-buffer "[1]"
      (should (equal (json-read-array) '(1))))
    (json-tests--with-temp-buffer "[1, 2, \"a\", \"b\"]"
      (should (equal (json-read-array) '(1 2 "a" "b")))))
  (json-tests--with-temp-buffer "[1 2]"
    (should (equal (should-error (json-read-array))
                   '(json-array-format "," ?2)))))

(ert-deftest test-json-read-array-function ()
  (let* ((pre nil)
         (post nil)
         (keys '(0 1))
         (json-pre-element-read-function
          (lambda (key)
            (setq pre 'pre)
            (should (equal key (pop keys)))))
         (json-post-element-read-function
          (lambda () (setq post 'post))))
    (json-tests--with-temp-buffer "[1, 0]"
      (json-read-array)
      (should (eq pre 'pre))
      (should (eq post 'post)))))

(ert-deftest test-json-encode-array ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode-array ()) "[]"))
    (should (equal (json-encode-array []) "[]"))
    (should (equal (json-encode-array '(1)) "[1]"))
    (should (equal (json-encode-array '[1]) "[1]"))
    (should (equal (json-encode-array '(2 1)) "[2,1]"))
    (should (equal (json-encode-array '[2 1]) "[2,1]"))
    (should (equal (json-encode-array '[:b a 2 1]) "[\"b\",\"a\",2,1]"))))

(ert-deftest test-json-encode-array-pretty ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings nil))
    (should (equal (json-encode-array ()) "[]"))
    (should (equal (json-encode-array []) "[]"))
    (should (equal (json-encode-array '(1)) "[\n 1\n]"))
    (should (equal (json-encode-array '[1]) "[\n 1\n]"))
    (should (equal (json-encode-array '(2 1)) "[\n 2,\n 1\n]"))
    (should (equal (json-encode-array '[2 1]) "[\n 2,\n 1\n]"))
    (should (equal (json-encode-array '[:b a 2 1])
                   "[\n \"b\",\n \"a\",\n 2,\n 1\n]"))))

(ert-deftest test-json-encode-array-lisp-style ()
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print t)
        (json-encoding-default-indentation " ")
        (json-encoding-lisp-style-closings t))
    (should (equal (json-encode-array ()) "[]"))
    (should (equal (json-encode-array []) "[]"))
    (should (equal (json-encode-array '(1)) "[\n 1]"))
    (should (equal (json-encode-array '[1]) "[\n 1]"))
    (should (equal (json-encode-array '(2 1)) "[\n 2,\n 1]"))
    (should (equal (json-encode-array '[2 1]) "[\n 2,\n 1]"))
    (should (equal (json-encode-array '[:b a 2 1])
                   "[\n \"b\",\n \"a\",\n 2,\n 1]"))))

;;; Reader

(ert-deftest test-json-read ()
  (pcase-dolist (`(,fn . ,contents)
                 '((json-read-string  "\"\"" "\"a\"")
                   (json-read-array   "[]" "[1]")
                   (json-read-object  "{}" "{\"a\":1}")
                   (json-read-keyword "null" "false" "true")
                   (json-read-number
                    "-0" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (dolist (content contents)
      ;; Check that leading whitespace is skipped.
      (dolist (str (list content (concat " " content)))
        (cl-letf* ((called nil)
                   ((symbol-function fn)
                    (lambda (&rest _) (setq called t))))
          (json-tests--with-temp-buffer str
            ;; We don't care exactly what the return value is (that is
            ;; tested elsewhere), but it should parse without error.
            (should (json-read))
            (should called)))))))

(ert-deftest test-json-read-invalid ()
  (json-tests--with-temp-buffer ""
    (should-error (json-read) :type 'json-end-of-file))
  (json-tests--with-temp-buffer " "
    (should-error (json-read) :type 'json-end-of-file))
  (json-tests--with-temp-buffer "x"
    (should (equal (should-error (json-read))
                  '(json-readtable-error ?x))))
  (json-tests--with-temp-buffer " x"
    (should (equal (should-error (json-read))
                  '(json-readtable-error ?x)))))

(ert-deftest test-json-read-from-string ()
  (dolist (str '("\"\"" "\"a\"" "[]" "[1]" "{}" "{\"a\":1}"
                 "null" "false" "true" "0" "123"))
    (json-tests--with-temp-buffer str
      (should (equal (json-read-from-string str)
                     (json-read))))))

;;; Encoder

(ert-deftest test-json-encode ()
  (should (equal (json-encode t) "true"))
  (let ((json-null 'null))
    (should (equal (json-encode json-null) "null")))
  (let ((json-false 'false))
    (should (equal (json-encode json-false) "false")))
  (should (equal (json-encode "") "\"\""))
  (should (equal (json-encode "foo") "\"foo\""))
  (should (equal (json-encode :) "\"\""))
  (should (equal (json-encode :foo) "\"foo\""))
  (should (equal (json-encode '(1)) "[1]"))
  (should (equal (json-encode 'foo) "\"foo\""))
  (should (equal (json-encode 0) "0"))
  (should (equal (json-encode 123) "123"))
  (let ((json-encoding-object-sort-predicate nil)
        (json-encoding-pretty-print nil))
    (should (equal (json-encode []) "[]"))
    (should (equal (json-encode [1]) "[1]"))
    (should (equal (json-encode #s(hash-table)) "{}"))
    (should (equal (json-encode #s(hash-table data (a 1))) "{\"a\":1}")))
  (with-temp-buffer
    (should (equal (should-error (json-encode (current-buffer)))
                   (list 'json-error (current-buffer))))))

;;; Pretty printing & minimizing

(defun json-tests-equal-pretty-print (original &optional expected)
  "Abort current test if pretty-printing ORIGINAL does not yield EXPECTED.

Both ORIGINAL and EXPECTED should be strings.  If EXPECTED is
nil, ORIGINAL should stay unchanged by pretty-printing."
  (with-temp-buffer
    (insert original)
    (json-pretty-print-buffer)
    (should (equal (buffer-string) (or expected original)))))

(ert-deftest test-json-pretty-print-string ()
  (json-tests-equal-pretty-print "\"\"")
  (json-tests-equal-pretty-print "\"foo\""))

(ert-deftest test-json-pretty-print-atom ()
  (json-tests-equal-pretty-print "true")
  (json-tests-equal-pretty-print "false")
  (json-tests-equal-pretty-print "null"))

(ert-deftest test-json-pretty-print-number ()
  (json-tests-equal-pretty-print "123")
  (json-tests-equal-pretty-print "0.123"))

(ert-deftest test-json-pretty-print-object ()
  ;; Empty (regression test for bug#24252).
  (json-tests-equal-pretty-print "{}")
  ;; One pair.
  (json-tests-equal-pretty-print
   "{\"key\":1}"
   "{\n  \"key\": 1\n}")
  ;; Two pairs.
  (json-tests-equal-pretty-print
   "{\"key1\":1,\"key2\":2}"
   "{\n  \"key1\": 1,\n  \"key2\": 2\n}")
  ;; Nested object.
  (json-tests-equal-pretty-print
   "{\"foo\":{\"key\":1}}"
   "{\n  \"foo\": {\n    \"key\": 1\n  }\n}")
  ;; Nested array.
  (json-tests-equal-pretty-print
   "{\"key\":[1,2]}"
   "{\n  \"key\": [\n    1,\n    2\n  ]\n}")
  ;; Confusable keys (bug#24252, bug#42545).
  (json-tests-equal-pretty-print
   (concat "{\"t\":1,\"nil\":2,\":t\":3,\":nil\":4,"
           "\"null\":5,\":json-null\":6,\":json-false\":7}")
   (concat "{\n  \"t\": 1,\n  \"nil\": 2,\n  \":t\": 3,\n  \":nil\": 4,"
           "\n  \"null\": 5,\n  \":json-null\": 6,\n  \":json-false\": 7\n}")))

(ert-deftest test-json-pretty-print-array ()
  ;; Empty.
  (json-tests-equal-pretty-print "[]")
  ;; One item.
  (json-tests-equal-pretty-print
   "[1]"
   "[\n  1\n]")
  ;; Two items.
  (json-tests-equal-pretty-print
   "[1,2]"
   "[\n  1,\n  2\n]")
  ;; Nested object.
  (json-tests-equal-pretty-print
   "[{\"key\":1}]"
   "[\n  {\n    \"key\": 1\n  }\n]")
  ;; Nested array.
  (json-tests-equal-pretty-print
   "[[1,2]]"
   "[\n  [\n    1,\n    2\n  ]\n]"))

(provide 'json-tests)

;;; json-tests.el ends here
