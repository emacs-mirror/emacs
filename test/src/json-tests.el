;;; json-tests.el --- unit tests for json.c          -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

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

;; Unit tests for src/json.c.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function json-serialize "json.c" (object))
(declare-function json-insert "json.c" (object))
(declare-function json-parse-string "json.c" (string &rest args))
(declare-function json-parse-buffer "json.c" (&rest args))

(define-error 'json-tests--error "JSON test error")

(ert-deftest json-serialize/roundtrip ()
  (skip-unless (fboundp 'json-serialize))
  ;; The noncharacter U+FFFF should be passed through,
  ;; cf. https://www.unicode.org/faq/private_use.html#noncharacters.
  (let ((lisp [:null :false t 0 123 -456 3.75 "abc\uFFFFαβγ𝔸𝐁𝖢\"\\"])
        (json "[null,false,true,0,123,-456,3.75,\"abc\uFFFFαβγ𝔸𝐁𝖢\\\"\\\\\"]"))
    (should (equal (json-serialize lisp) json))
    (with-temp-buffer
      (json-insert lisp)
      (should (equal (buffer-string) json))
      (should (eobp)))
    (should (equal (json-parse-string json) lisp))
    (with-temp-buffer
      (insert json)
      (goto-char 1)
      (should (equal (json-parse-buffer) lisp))
      (should (eobp)))))

(ert-deftest json-serialize/object ()
  (skip-unless (fboundp 'json-serialize))
  (let ((table (make-hash-table :test #'equal)))
    (puthash "abc" [1 2 t] table)
    (puthash "def" :null table)
    (should (equal (json-serialize table)
                   "{\"abc\":[1,2,true],\"def\":null}")))
  (should (equal (json-serialize '((abc . [1 2 t]) (def . :null)))
                 "{\"abc\":[1,2,true],\"def\":null}"))
  (should (equal (json-serialize nil) "{}"))
  (should (equal (json-serialize '((abc))) "{\"abc\":{}}"))
  (should (equal (json-serialize '((a . 1) (b . 2) (a . 3)))
                 "{\"a\":1,\"b\":2}"))
  (should-error (json-serialize '(abc)) :type 'wrong-type-argument)
  (should-error (json-serialize '((a 1))) :type 'wrong-type-argument)
  (should-error (json-serialize '((1 . 2))) :type 'wrong-type-argument)
  (should-error (json-serialize '((a . 1) . b)) :type 'wrong-type-argument)
  (should-error (json-serialize '#1=((a . 1) . #1#)) :type 'circular-list)
  (should-error (json-serialize '(#1=(a #1#)))))

(ert-deftest json-serialize/object-with-duplicate-keys ()
  (skip-unless (fboundp 'json-serialize))
  (let ((table (make-hash-table :test #'eq)))
    (puthash (copy-sequence "abc") [1 2 t] table)
    (puthash (copy-sequence "abc") :null table)
    (should (equal (hash-table-count table) 2))
    (should-error (json-serialize table) :type 'wrong-type-argument)))

(ert-deftest json-parse-string/object ()
  (skip-unless (fboundp 'json-parse-string))
  (let ((input
         "{ \"abc\" : [1, 2, true], \"def\" : null, \"abc\" : [9, false] }\n"))
    (let ((actual (json-parse-string input)))
      (should (hash-table-p actual))
      (should (equal (hash-table-count actual) 2))
      (should (equal (cl-sort (map-pairs actual) #'string< :key #'car)
                     '(("abc" . [9 :false]) ("def" . :null)))))
    (should (equal (json-parse-string input :object-type 'alist)
                   '((abc . [9 :false]) (def . :null))))))

(ert-deftest json-parse-string/string ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "[\"formfeed\f\"]") :type 'json-parse-error)
  (should (equal (json-parse-string "[\"foo \\\"bar\\\"\"]") ["foo \"bar\""]))
  (should (equal (json-parse-string "[\"abcαβγ\"]") ["abcαβγ"]))
  (should (equal (json-parse-string "[\"\\nasd\\u0444\\u044b\\u0432fgh\\t\"]")
                 ["\nasdфывfgh\t"]))
  (should (equal (json-parse-string "[\"\\uD834\\uDD1E\"]") ["\U0001D11E"]))
  (should-error (json-parse-string "foo") :type 'json-parse-error)
  ;; FIXME: Is this the right behavior?
  (should (equal (json-parse-string "[\"\u00C4\xC3\x84\"]") ["\u00C4\u00C4"])))

(ert-deftest json-serialize/string ()
  (skip-unless (fboundp 'json-serialize))
  (should (equal (json-serialize ["foo"]) "[\"foo\"]"))
  (should (equal (json-serialize ["a\n\fb"]) "[\"a\\n\\fb\"]"))
  (should (equal (json-serialize ["\nasdфыв\u001f\u007ffgh\t"])
                 "[\"\\nasdфыв\\u001F\u007ffgh\\t\"]"))
  (should (equal (json-serialize ["a\0b"]) "[\"a\\u0000b\"]"))
  ;; FIXME: Is this the right behavior?
  (should (equal (json-serialize ["\u00C4\xC3\x84"]) "[\"\u00C4\u00C4\"]")))

(ert-deftest json-serialize/invalid-unicode ()
  (skip-unless (fboundp 'json-serialize))
  (should-error (json-serialize ["a\uDBBBb"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\x110000v"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\x3FFFFFv"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\xCCv"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\u00C4\xCCv"]) :type 'wrong-type-argument))

(ert-deftest json-parse-string/null ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "\x00") :type 'wrong-type-argument)
  ;; FIXME: Reconsider whether this is the right behavior.
  (should-error (json-parse-string "[a\\u0000b]") :type 'json-parse-error))

(ert-deftest json-parse-string/invalid-unicode ()
  "Some examples from
https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt.
Test with both unibyte and multibyte strings."
  (skip-unless (fboundp 'json-parse-string))
  ;; Invalid UTF-8 code unit sequences.
  (should-error (json-parse-string "[\"\x80\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\x80\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\xBF\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xBF\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\xFE\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xFE\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\xC0\xAF\"]") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xC0\xAF\"]")
                :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xC0\x80\"]")
                :type 'json-parse-error)
  ;; Surrogates.
  (should-error (json-parse-string "[\"\uDB7F\"]")
                :type 'json-parse-error)
  (should-error (json-parse-string "[\"\xED\xAD\xBF\"]")
                :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xED\xAD\xBF\"]")
                :type 'json-parse-error)
  (should-error (json-parse-string "[\"\uDB7F\uDFFF\"]")
                :type 'json-parse-error)
  (should-error (json-parse-string "[\"\xED\xAD\xBF\xED\xBF\xBF\"]")
                :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xED\xAD\xBF\xED\xBF\xBF\"]")
                :type 'json-parse-error))

(ert-deftest json-parse-string/incomplete ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "[123") :type 'json-end-of-file))

(ert-deftest json-parse-string/trailing ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "[123] [456]") :type 'json-trailing-content))

(ert-deftest json-parse-buffer/incomplete ()
  (skip-unless (fboundp 'json-parse-buffer))
  (with-temp-buffer
    (insert "[123")
    (goto-char 1)
    (should-error (json-parse-buffer) :type 'json-end-of-file)
    (should (bobp))))

(ert-deftest json-parse-buffer/trailing ()
  (skip-unless (fboundp 'json-parse-buffer))
  (with-temp-buffer
    (insert "[123] [456]")
    (goto-char 1)
    (should (equal (json-parse-buffer) [123]))
    (should-not (bobp))
    (should (looking-at-p (rx " [456]" eos)))))

(ert-deftest json-insert/signal ()
  (skip-unless (fboundp 'json-insert))
  (with-temp-buffer
    (let ((calls 0))
      (add-hook 'after-change-functions
                (lambda (_begin _end _length)
                  (cl-incf calls)
                  (signal 'json-tests--error
                          '("Error in `after-change-functions'")))
                :local)
      (should-error
       (json-insert '((a . "b") (c . 123) (d . [1 2 t :false])))
       :type 'json-tests--error)
      (should (equal calls 1)))))

(ert-deftest json-insert/throw ()
  (skip-unless (fboundp 'json-insert))
  (with-temp-buffer
    (let ((calls 0))
      (add-hook 'after-change-functions
                (lambda (_begin _end _length)
                  (cl-incf calls)
                  (throw 'test-tag 'throw-value))
                :local)
      (should-error
       (catch 'test-tag
         (json-insert '((a . "b") (c . 123) (d . [1 2 t :false]))))
       :type 'no-catch)
      (should (equal calls 1)))))

(provide 'json-tests)
;;; json-tests.el ends here
