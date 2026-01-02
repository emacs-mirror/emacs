;;; json-tests.el --- unit tests for json.c          -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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
(require 'subr-x)

(define-error 'json-tests--error "JSON test error")

(ert-deftest json-serialize/roundtrip ()
  ;; The noncharacter U+FFFF should be passed through,
  ;; cf. https://www.unicode.org/faq/private_use.html#noncharacters.
  (let* ((lisp [:null :false t 0 123 -456 3.75 "abc\uFFFFαβγ𝔸𝐁𝖢\"\\"])
         (json
          "[null,false,true,0,123,-456,3.75,\"abc\uFFFFαβγ𝔸𝐁𝖢\\\"\\\\\"]")
         (json-bytes (encode-coding-string json 'utf-8)))
    (should (equal (json-serialize lisp) json-bytes))
    (with-temp-buffer
      ;; multibyte buffer
      (json-insert lisp)
      (should (equal (buffer-string) json))
      (should (equal (point) (1+ (length json))))
      (should (eobp)))
    (with-temp-buffer
      ;; unibyte buffer
      (set-buffer-multibyte nil)
      (json-insert lisp)
      (should (equal (buffer-string) json-bytes))
      (should (equal (point) (1+ (length json-bytes))))
      (should (eobp)))
    (should (equal (json-parse-string json) lisp))
    (with-temp-buffer
      ;; multibyte buffer
      (insert json)
      (goto-char 1)
      (should (equal (json-parse-buffer) lisp))
      (should (equal (point) (1+ (length json))))
      (should (eobp)))
    (with-temp-buffer
      ;; unibyte buffer
      (set-buffer-multibyte nil)
      (insert json-bytes)
      (goto-char 1)
      (should (equal (json-parse-buffer) lisp))
      (should (equal (point) (1+ (length json-bytes))))
      (should (eobp)))))

(ert-deftest json-serialize/roundtrip-scalars ()
  "Check that Bug#42994 is fixed."
  (dolist (case '((:null "null")
                  (:false "false")
                  (t "true")
                  (0 "0")
                  (123 "123")
                  (-456 "-456")
                  (3.75 "3.75")
                  ;; The noncharacter U+FFFF should be passed through,
                  ;; cf. https://www.unicode.org/faq/private_use.html#noncharacters.
                  ("abc\uFFFFαβγ𝔸𝐁𝖢\"\\"
                   "\"abc\uFFFFαβγ𝔸𝐁𝖢\\\"\\\\\"")))
    (cl-destructuring-bind (lisp json) case
      (ert-info ((format "%S ↔ %S" lisp json))
        (let ((json-bytes (encode-coding-string json 'utf-8)))
          (should (equal (json-serialize lisp) json-bytes))
          (with-temp-buffer
            (json-insert lisp)
            (should (equal (buffer-string) json))
            (should (eobp)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (json-insert lisp)
            (should (equal (buffer-string) (encode-coding-string json 'utf-8)))
            (should (eobp)))
          (should (equal (json-parse-string json) lisp))
          (with-temp-buffer
            (insert json)
            (goto-char 1)
            (should (equal (json-parse-buffer) lisp))
            (should (eobp)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (encode-coding-string json 'utf-8))
            (goto-char 1)
            (should (equal (json-parse-buffer) lisp))
            (should (eobp))))))))

(ert-deftest json-serialize/object ()
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
  (should-error (json-serialize '(#1=(a #1#))))

  (should (equal (json-serialize '(:abc [1 2 t] :def :null))
                 "{\"abc\":[1,2,true],\"def\":null}"))
  (should (equal (json-serialize '(abc [1 2 t] :def :null))
                 "{\"abc\":[1,2,true],\"def\":null}"))
  (should-error (json-serialize '#1=(:a 1 . #1#)) :type 'circular-list)
  (should-error (json-serialize '#1=(:a 1 :b . #1#))
                :type '(circular-list wrong-type-argument))
  (should-error (json-serialize '(:foo "bar" (unexpected-alist-key . 1)))
                :type 'wrong-type-argument)
  (should-error (json-serialize '((abc . "abc") :unexpected-plist-key "key"))
                :type 'wrong-type-argument)
  (should-error (json-serialize '(:foo bar :odd-numbered))
                :type 'wrong-type-argument)
  (should (equal
           (json-serialize
            (list :detect-hash-table #s(hash-table test equal data ("bla" "ble"))
                  :detect-alist '((bla . "ble"))
                  :detect-plist '(:bla "ble")))
           "\
{\
\"detect-hash-table\":{\"bla\":\"ble\"},\
\"detect-alist\":{\"bla\":\"ble\"},\
\"detect-plist\":{\"bla\":\"ble\"}\
}")))

(ert-deftest json-serialize/object-with-duplicate-keys ()
  (dolist (n '(1 5 20 100))
    (let ((symbols (mapcar (lambda (i) (make-symbol (format "s%d" i)))
                           (number-sequence 1 n)))
          (expected (concat "{"
                            (mapconcat (lambda (i) (format "\"s%d\":%d" i i))
                                       (number-sequence 1 n) ",")
                            "}")))
      ;; alist
      (should (equal (json-serialize
                      (append
                       (cl-mapcar #'cons
                                  symbols (number-sequence 1 n))
                       (cl-mapcar #'cons
                                  symbols (number-sequence 1001 (+ 1000 n)))))
                     expected))
      ;; plist
      (should (equal (json-serialize
                      (append
                       (cl-mapcan #'list
                                  symbols (number-sequence 1 n))
                       (cl-mapcan #'list
                                  symbols (number-sequence 1001 (+ 1000 n)))))
                     expected))))

  ;; We don't check for duplicated keys in hash tables.
  ;; (let ((table (make-hash-table :test #'eq)))
  ;;   (puthash (copy-sequence "abc") [1 2 t] table)
  ;;   (puthash (copy-sequence "abc") :null table)
  ;;   (should (equal (hash-table-count table) 2))
  ;;   (should-error (json-serialize table) :type 'wrong-type-argument))
  )

(ert-deftest json-parse-string/object ()
  (let ((input
         "{ \"abc\" : [1, 2, true], \"def\" : null, \"abc\" : [9, false] }\n"))
    (let ((actual (json-parse-string input)))
      (should (hash-table-p actual))
      (should (equal (hash-table-count actual) 2))
      (should (equal (cl-sort (map-pairs actual) #'string< :key #'car)
                     '(("abc" . [9 :false]) ("def" . :null)))))
    (should (equal (json-parse-string input :object-type 'alist)
                   '((abc . [1 2 t]) (def . :null) (abc . [9 :false]))))
    (should (equal (json-parse-string input :object-type 'plist)
                   '(:abc [1 2 t]  :def :null :abc [9 :false])))))

(ert-deftest json-parse-string/object-unicode-keys ()
  (let ((input "{\"é\":1,\"☃\":2,\"𐌐\":3}"))
    (let ((actual (json-parse-string input)))
      (should (equal (sort (hash-table-keys actual)) '("é" "☃" "𐌐"))))
    (should (equal (json-parse-string input :object-type 'alist)
                   '((é . 1) (☃ . 2) (𐌐 . 3))))
    (should (equal (json-parse-string input :object-type 'plist)
                   '(:é 1 :☃ 2 :𐌐 3)))))

(ert-deftest json-parse-string/array ()
  (let ((input "[\"a\", 1, [\"b\", 2]]"))
    (should (equal (json-parse-string input)
                   ["a" 1 ["b" 2]]))
    (should (equal (json-parse-string input :array-type 'list)
                   '("a" 1 ("b" 2))))))

(ert-deftest json-parse-string/string ()
  (should-error (json-parse-string "[\"formfeed\f\"]") :type 'json-parse-error)
  (should (equal (json-parse-string "[\"foo \\\"bar\\\"\"]") ["foo \"bar\""]))
  (should (equal (json-parse-string "[\"abcαβγ\"]") ["abcαβγ"]))
  (should (equal (json-parse-string "[\"\\nasd\\u0444\\u044b\\u0432fgh\\t\"]")
                 ["\nasdфывfgh\t"]))
  (should (equal (json-parse-string "[\"\\uD834\\uDD1E\"]") ["\U0001D11E"]))
  (should-error (json-parse-string "foo") :type 'json-parse-error)
  (should-error (json-parse-string "[\"\u00C4\xC3\x84\"]")
                :type 'json-utf8-decode-error))

(ert-deftest json-serialize/string ()
  (should (equal (json-serialize ["foo"]) "[\"foo\"]"))
  (should (equal (json-serialize ["a\n\fb"]) "[\"a\\n\\fb\"]"))
  (should (equal (json-serialize ["\nasdфыв\u001f\u007ffgh\t"])
                 (encode-coding-string "[\"\\nasdфыв\\u001F\u007ffgh\\t\"]"
                                       'utf-8)))
  (should (equal (json-serialize ["a\0b"]) "[\"a\\u0000b\"]"))
  (should-error (json-serialize ["\xC3\x84"]))
  (should-error (json-serialize ["\u00C4\xC3\x84"])))

(ert-deftest json-serialize/invalid-unicode ()
  (should-error (json-serialize ["a\uDBBBb"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\x110000v"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\x3FFFFFv"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\xCCv"]) :type 'wrong-type-argument)
  (should-error (json-serialize ["u\u00C4\xCCv"]) :type 'wrong-type-argument))

(ert-deftest json-parse-string/short ()
  (should-error (json-parse-string "") :type 'json-end-of-file)
  (should-error (json-parse-string " ") :type 'json-end-of-file)
  (dolist (s '("a" "ab" "abc" "abcd" "\0" "\1"
               "t" "tr" "tru" "truE" "truee"
               "n" "nu" "nul" "nulL" "nulll"
               "f" "fa" "fal" "fals" "falsE" "falsee"))
    (condition-case err
        (json-parse-string s)
      (error
       (should (eq (car err) 'json-parse-error)))
      (:success (error "parsing %S should fail" s)))))

(ert-deftest json-parse-string/null ()
  (should (equal (json-parse-string "[\"a\\u0000b\"]") ["a\0b"]))
  (let* ((string "{\"foo\":\"this is a string including a literal \\u0000\"}")
         (data (json-parse-string string)))
    (should (hash-table-p data))
    (should (equal string (json-serialize data)))))

(ert-deftest json-parse-string/invalid-unicode ()
  "Some examples from
https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt.
Test with both unibyte and multibyte strings."
  ;; Invalid UTF-8 code unit sequences.
  (should-error (json-parse-string "[\"\x80\"]") :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\x80\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\xBF\"]") :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\xBF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\xFE\"]") :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\xFE\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\xC0\xAF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\xC0\xAF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\xC0\x80\"]")
                :type 'json-utf8-decode-error)
  ;; Surrogates.
  (should-error (json-parse-string "[\"\uDB7F\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\xED\xAD\xBF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\xED\xAD\xBF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\uDB7F\uDFFF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\xED\xAD\xBF\xED\xBF\xBF\"]")
                :type 'json-utf8-decode-error)
  (should-error (json-parse-string "[\"\u00C4\xED\xAD\xBF\xED\xBF\xBF\"]")
                :type 'json-utf8-decode-error))

(ert-deftest json-parse-string/incomplete ()
  (should-error (json-parse-string "[123") :type 'json-end-of-file))

(ert-deftest json-parse-string/trailing ()
  (should-error (json-parse-string "[123] [456]") :type 'json-trailing-content))

(ert-deftest json-parse-buffer/incomplete ()
  (with-temp-buffer
    (insert "[123")
    (goto-char 1)
    (should-error (json-parse-buffer) :type 'json-end-of-file)
    (should (bobp))))

(ert-deftest json-parse-buffer/trailing ()
  (with-temp-buffer
    (insert "[123] [456]")
    (goto-char 1)
    (should (equal (json-parse-buffer) [123]))
    (should-not (bobp))
    (should (looking-at-p (rx " [456]" eos)))))

(defmacro with-all-gap-positions-in-temp-buffer (string &rest body)
  "Create a temporary buffer containing STRING, and evaluate BODY
with each possible gap position.
See also `with-temp-buffer'."
  `(with-temp-buffer
     (insert ,string)
     (dotimes (i (- (point-max) (point-min)))
       (goto-char (- (point-max) i))
       (insert "X")
       (delete-region (1- (point)) (point))
       ,@body)))

(ert-deftest json-parse-buffer/restricted ()
  (with-all-gap-positions-in-temp-buffer
   "[123] [456] [789]"
   (pcase-dolist (`((,beg . ,end) ,result)
                  '(((7 . 12) [456])
                    ((1 . 6) [123])
                    ((13 . 18) [789])))
     (goto-char beg)
     (narrow-to-region beg end)
     (should (equal (json-parse-buffer) result))
     (should (= (point) end))
     (should-error (json-parse-buffer) :type 'json-end-of-file)
     (widen))))

(ert-deftest json-parse-with-custom-null-and-false-objects ()
  (let* ((input
          "{ \"abc\" : [9, false] , \"def\" : null }")
         (output
          (string-replace " " "" input)))
    (should (equal (json-parse-string input
                                      :object-type 'plist
                                      :null-object :json-null
                                      :false-object :json-false)
                   '(:abc [9 :json-false] :def :json-null)))
    (should (equal (json-parse-string input
                                      :object-type 'plist
                                      :false-object :json-false)
                   '(:abc [9 :json-false] :def :null)))
    (should (equal (json-parse-string input
                                      :object-type 'alist
                                      :null-object :zilch)
                   '((abc . [9 :false]) (def . :zilch))))
    (should (equal (json-parse-string input
                                      :object-type 'alist
                                      :false-object nil
                                      :null-object nil)
                   '((abc . [9 nil]) (def))))
    (let* ((thingy '(1 2 3))
           (retval (json-parse-string input
                                      :object-type 'alist
                                      :false-object thingy
                                      :null-object nil)))
      (should (equal retval `((abc . [9 ,thingy]) (def))))
      (should (eq (elt (cdr (car retval)) 1) thingy)))
    (should (equal output
                   (json-serialize '((abc . [9 :myfalse]) (def . :mynull))
                                   :false-object :myfalse
                                   :null-object :mynull)))
    ;; :object-type is not allowed in json-serialize
    (should-error (json-serialize '() :object-type 'alist))))

(ert-deftest json-insert/signal ()
  (with-temp-buffer
    (let ((calls 0))
      (add-hook 'after-change-functions
                (lambda (_begin _end _length)
                  (incf calls)
                  (signal 'json-tests--error
                          '("Error in `after-change-functions'")))
                :local)
      (should-error
       (json-insert '((a . "b") (c . 123) (d . [1 2 t :false])))
       :type 'json-tests--error)
      (should (equal calls 1)))))

(ert-deftest json-insert/throw ()
  (with-temp-buffer
    (let ((calls 0))
      (add-hook 'after-change-functions
                (lambda (_begin _end _length)
                  (incf calls)
                  (throw 'test-tag 'throw-value))
                :local)
      (should
       (equal
        (catch 'test-tag
          (json-insert '((a . "b") (c . 123) (d . [1 2 t :false]))))
        'throw-value))
      (should (equal calls 1)))))

(ert-deftest json-serialize/bignum ()
  (should (equal (json-serialize (vector (1+ most-positive-fixnum)
                                         (1- most-negative-fixnum)))
                 (format "[%d,%d]"
                         (1+ most-positive-fixnum)
                         (1- most-negative-fixnum)))))

(ert-deftest json-parse-string/wrong-type ()
  "Check that Bug#42113 is fixed."
  (should-error (json-parse-string 1) :type 'wrong-type-argument))

(ert-deftest json-serialize/wrong-hash-key-type ()
  "Check that Bug#42113 is fixed."
  (let ((table (make-hash-table :test #'eq)))
    (puthash 1 2 table)
    (should-error (json-serialize table) :type 'wrong-type-argument)))

(defun json-tests--parse-string-error-pos (s)
  (condition-case e
      (json-parse-string s)
    (json-error (nth 3 e))
    (:success 'no-error)))

(defun json-tests--parse-buffer-error-pos ()
  (condition-case e
      (json-parse-buffer)
    (json-error (nth 3 e))
    (:success 'no-error)))

(ert-deftest json-parse-error-position ()
  (let* ((s "[\"*Ωßœ☃*\",,8]")
         (su (encode-coding-string s 'utf-8-emacs)))
    (should (equal (json-tests--parse-string-error-pos s) 11))
    (should (equal (json-tests--parse-string-error-pos su) 16))

    (with-temp-buffer
      (let ((junk "some leading junk"))
        (insert junk)
        (insert s)
        (goto-char (1+ (length junk)))
        (should (equal (json-tests--parse-buffer-error-pos) 11))

        (set-buffer-multibyte nil)
        (goto-char (1+ (length junk)))
        (should (equal (json-tests--parse-buffer-error-pos) 16))))))

(provide 'json-tests)
;;; json-tests.el ends here
