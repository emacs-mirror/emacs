;;; lread-tests.el --- tests for lread.c -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for code in src/lread.c.

;;; Code:

(require 'ert)
(require 'ert-x)

(ert-deftest lread-char-number ()
  (should (equal (read "?\\N{U+A817}") #xA817)))

(ert-deftest lread-char-name-1 ()
  (should (equal (read "?\\N{SYLOTI  NAGRI LETTER \n DHO}")
                 #xA817)))
(ert-deftest lread-char-name-2 ()
  (should (equal (read "?\\N{BED}") #x1F6CF)))
(ert-deftest lread-char-name-3 ()
  (should (equal (read "?\\N{U+BED}") #xBED)))
(ert-deftest lread-char-name-4 ()
  (should (equal (read "?\\N{VARIATION SELECTOR-1}") #xFE00)))
(ert-deftest lread-char-name-5 ()
  (should (equal (read "?\\N{VARIATION SELECTOR-16}") #xFE0F)))
(ert-deftest lread-char-name-6 ()
  (should (equal (read "?\\N{VARIATION SELECTOR-17}") #xE0100)))
(ert-deftest lread-char-name-7 ()
  (should (equal (read "?\\N{VARIATION SELECTOR-256}") #xE01EF)))
(ert-deftest lread-char-name-8 ()
  (should (equal (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-F900}") #xF900)))
(ert-deftest lread-char-name-9 ()
  (should (equal (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-FAD9}") #xFAD9)))
(ert-deftest lread-char-name-10 ()
  (should (equal (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-2F800}") #x2F800)))
(ert-deftest lread-char-name-11 ()
  (should (equal (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-2FA1D}") #x2FA1D)))

(ert-deftest lread-char-invalid-number ()
  (should-error (read "?\\N{U+110000}") :type 'invalid-read-syntax))

(ert-deftest lread-char-invalid-name-1 ()
  (should-error (read "?\\N{DOES NOT EXIST}")) :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-2 ()
  (should-error (read "?\\N{VARIATION SELECTOR-0}")) :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-3 ()
  (should-error (read "?\\N{VARIATION SELECTOR-257}"))
  :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-4 ()
  (should-error (read "?\\N{VARIATION SELECTOR--0}"))
  :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-5 ()
  (should-error (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-F8FF}"))
  :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-6 ()
  (should-error (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-FADA}"))
  :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-7 ()
  (should-error (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-2F7FF}"))
  :type 'invalid-read-syntax)
(ert-deftest lread-char-invalid-name-8 ()
  (should-error (read "?\\N{CJK COMPATIBILITY IDEOGRAPH-2FA1E}"))
  :type 'invalid-read-syntax)

(ert-deftest lread-char-non-ascii-name ()
  (should-error (read "?\\N{LATIN CAPITAL LETTER Ø}")
                :type 'invalid-read-syntax))

(ert-deftest lread-char-empty-name ()
  (should-error (read "?\\N{}") :type 'invalid-read-syntax))

(ert-deftest lread-char-surrogate-1 ()
  (should-error (read "?\\N{U+D800}") :type 'invalid-read-syntax))
(ert-deftest lread-char-surrogate-2 ()
  (should-error (read "?\\N{U+D801}") :type 'invalid-read-syntax))
(ert-deftest lread-char-surrogate-3 ()
  (should-error (read "?\\N{U+Dffe}") :type 'invalid-read-syntax))
(ert-deftest lread-char-surrogate-4 ()
  (should-error (read "?\\N{U+DFFF}") :type 'invalid-read-syntax))

(ert-deftest lread-string-char-number-1 ()
  (should (equal (read "\"a\\N{U+A817}b\"") "a\uA817b")))
(ert-deftest lread-string-char-number-2 ()
  (should-error (read "?\\N{0.5}") :type 'invalid-read-syntax))
(ert-deftest lread-string-char-number-3 ()
  (should-error (read "?\\N{U+-0}") :type 'invalid-read-syntax))

(ert-deftest lread-string-char-name ()
  (should (equal (read "\"a\\N{SYLOTI NAGRI  LETTER DHO}b\"") "a\uA817b")))

(ert-deftest lread-empty-int-literal ()
  "Check that Bug#25120 is fixed."
  (should-error (read "#b") :type 'invalid-read-syntax)
  (should-error (read "#o") :type 'invalid-read-syntax)
  (should-error (read "#x") :type 'invalid-read-syntax)
  (should-error (read "#24r") :type 'invalid-read-syntax)
  (should-error (read "#") :type 'invalid-read-syntax))

(ert-deftest lread-char-modifiers ()
  (should (eq ?\C-\M-é (+ (- ?\M-a ?a) ?\C-é)))
  (should (eq (- ?\C-ŗ ?ŗ) (- ?\C-é ?é))))

(ert-deftest lread-record-1 ()
  (should (equal '(#s(foo) #s(foo))
                 (read "(#1=#s(foo) #1#)"))))

(defun lread-tests--last-message ()
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (buffer-substring (pos-bol) (point)))))

(ert-deftest lread-tests--unescaped-char-literals ()
  "Check that loading warns about unescaped character
literals (Bug#20852)."
  (ert-with-temp-file file-name
    (write-region "?) ?( ?; ?\" ?[ ?]" nil file-name)
    (should (equal (load file-name nil :nomessage :nosuffix) t))
    (should (equal (lread-tests--last-message)
                   (concat (format-message "Loading `%s': " file-name)
                           "unescaped character literals "
                           "`?\"', `?(', `?)', `?;', `?[', `?]' detected, "
                           "`?\\\"', `?\\(', `?\\)', `?\\;', `?\\[', `?\\]' "
                           "expected!")))))

(ert-deftest lread-test-bug26837 ()
  "Test for https://debbugs.gnu.org/26837 ."
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (load "somelib" nil t)
    (should (string-suffix-p "/somelib.el" (caar load-history)))
    (load "somelib2" nil t)
    (should (string-suffix-p "/somelib2.el" (caar load-history)))
    (load "somelib" nil t)
    (should (string-suffix-p "/somelib.el" (caar load-history)))))

(ert-deftest lread-lread--substitute-object-in-subtree ()
  (let ((x (cons 0 1)))
    (setcar x x)
    (lread--substitute-object-in-subtree x 1 t)
    (should (eq x (cdr x)))))

(ert-deftest lread-long-hex-integer ()
  (should (bignump (read "#xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))))

(ert-deftest lread-test-bug-31186 ()
  (with-temp-buffer
    (insert ";; -*- -:*-")
    (should-not
     ;; This used to crash in lisp_file_lexically_bound_p before the
     ;; bug was fixed.
     (eval-buffer))))

(ert-deftest lread-invalid-bytecodes ()
  (should-error
   (let ((load-force-doc-strings t)) (read "#[0 \"\"]"))))

(ert-deftest lread-string-to-number-trailing-dot ()
  (dolist (n (list (* most-negative-fixnum most-negative-fixnum)
                   (1- most-negative-fixnum) most-negative-fixnum
                   (1+ most-negative-fixnum) -1 0 1
                   (1- most-positive-fixnum) most-positive-fixnum
                   (1+ most-positive-fixnum)
                   (* most-positive-fixnum most-positive-fixnum)))
    (should (= n (string-to-number (format "%d." n))))))

(ert-deftest lread-circular-hash ()
  (should-error (read "#s(hash-table data #0=(#0# . #0#))")))

(ert-deftest test-inhibit-interaction ()
  (let ((inhibit-interaction t))
    (should-error (read-char "foo: "))
    (should-error (read-event "foo: "))
    (should-error (read-char-exclusive "foo: "))))

(ert-deftest lread-float ()
  (should (equal (read "13") 13))
  (should (equal (read "+13") 13))
  (should (equal (read "-13") -13))
  (should (equal (read "13.") 13))
  (should (equal (read "+13.") 13))
  (should (equal (read "-13.") -13))
  (should (equal (read "13.25") 13.25))
  (should (equal (read "+13.25") 13.25))
  (should (equal (read "-13.25") -13.25))
  (should (equal (read ".25") 0.25))
  (should (equal (read "+.25") 0.25))
  (should (equal (read "-.25") -0.25))
  (should (equal (read "13e4") 130000.0))
  (should (equal (read "+13e4") 130000.0))
  (should (equal (read "-13e4") -130000.0))
  (should (equal (read "13e+4") 130000.0))
  (should (equal (read "+13e+4") 130000.0))
  (should (equal (read "-13e+4") -130000.0))
  (should (equal (read "625e-4") 0.0625))
  (should (equal (read "+625e-4") 0.0625))
  (should (equal (read "-625e-4") -0.0625))
  (should (equal (read "1.25e2") 125.0))
  (should (equal (read "+1.25e2") 125.0))
  (should (equal (read "-1.25e2") -125.0))
  (should (equal (read "1.25e+2") 125.0))
  (should (equal (read "+1.25e+2") 125.0))
  (should (equal (read "-1.25e+2") -125.0))
  (should (equal (read "1.25e-1") 0.125))
  (should (equal (read "+1.25e-1") 0.125))
  (should (equal (read "-1.25e-1") -0.125))
  (should (equal (read "4.e3") 4000.0))
  (should (equal (read "+4.e3") 4000.0))
  (should (equal (read "-4.e3") -4000.0))
  (should (equal (read "4.e+3") 4000.0))
  (should (equal (read "+4.e+3") 4000.0))
  (should (equal (read "-4.e+3") -4000.0))
  (should (equal (read "5.e-1") 0.5))
  (should (equal (read "+5.e-1") 0.5))
  (should (equal (read "-5.e-1") -0.5))
  (should (equal (read "0") 0))
  (should (equal (read "+0") 0))
  (should (equal (read "-0") 0))
  (should (equal (read "0.") 0))
  (should (equal (read "+0.") 0))
  (should (equal (read "-0.") 0))
  (should (equal (read "0.0") 0.0))
  (should (equal (read "+0.0") 0.0))
  (should (equal (read "-0.0") -0.0))
  (should (equal (read "0e5") 0.0))
  (should (equal (read "+0e5") 0.0))
  (should (equal (read "-0e5") -0.0))
  (should (equal (read "0e-5") 0.0))
  (should (equal (read "+0e-5") 0.0))
  (should (equal (read "-0e-5") -0.0))
  (should (equal (read ".0e-5") 0.0))
  (should (equal (read "+.0e-5") 0.0))
  (should (equal (read "-.0e-5") -0.0))
  (should (equal (read "0.0e-5") 0.0))
  (should (equal (read "+0.0e-5") 0.0))
  (should (equal (read "-0.0e-5") -0.0))
  (should (equal (read "0.e-5") 0.0))
  (should (equal (read "+0.e-5") 0.0))
  (should (equal (read "-0.e-5") -0.0))
  )

(defun lread-test-read-and-print (str)
  (let* ((read-circle t)
         (print-circle t)
         (val (read-from-string str)))
    (if (consp val)
        (prin1-to-string (car val))
      (error "reading %S failed: %S" str val))))

(defconst lread-test-circle-cases
  '("#1=(#1# . #1#)"
    "#1=[#1# a #1#]"
    "#1=(#2=[#1# #2#] . #1#)"
    "#1=(#2=[#1# #2#] . #2#)"
    "#1=[#2=(#1# . #2#)]"
    "#1=(#2=[#3=(#1# . #2#) #4=(#3# . #4#)])"
    ))

(ert-deftest lread-circle ()
  (dolist (str lread-test-circle-cases)
    (ert-info (str :prefix "input: ")
      (should (equal (lread-test-read-and-print str) str))))
  (should-error (read-from-string "#1=#1#") :type 'invalid-read-syntax))

(ert-deftest lread-deeply-nested ()
  ;; Check that we can read a deeply nested data structure correctly.
  (let ((levels 10000)
        (prefix nil)
        (suffix nil))
    (dotimes (_ levels)
      (push "([#s(r " prefix)
      (push ")])" suffix))
    (let ((str (concat (apply #'concat prefix)
                       "a"
                       (apply #'concat suffix))))
      (let* ((read-circle t)
             (result (read-from-string str)))
        (should (equal (cdr result) (length str)))
        ;; Check the result.  (We can't build a reference value and compare
        ;; using `equal' because that function is currently depth-limited.)
        (named-let check ((x (car result)) (level 0))
          (if (equal level levels)
              (should (equal x 'a))
            (should (and (consp x) (null (cdr x))))
            (let ((x2 (car x)))
              (should (and (vectorp x2) (equal (length x2) 1)))
              (let ((x3 (aref x2 0)))
                (should (and (recordp x3) (equal (length x3) 2)
                             (equal (aref x3 0) 'r)))
                (check (aref x3 1) (1+ level))))))))))

(ert-deftest lread-misc ()
  ;; Regression tests for issues found and fixed in bug#55676:
  ;; Non-breaking space after a dot makes it a dot token.
  (should (equal (read-from-string "(a .\u00A0b)")
                 '((a . b) . 7)))
  ;; #_ without symbol following is the interned empty symbol.
  (should (equal (read-from-string "#_")
                 '(## . 2))))

(ert-deftest lread-escaped-lf ()
  ;; ?\LF should signal an error; \LF is ignored inside string literals.
  (should-error (read-from-string "?\\\n x"))
  (should (equal (read-from-string "\"a\\\nb\"") '("ab" . 6))))

(ert-deftest lread-force-load-doc-strings ()
  ;; Verify that lazy doc strings are loaded lazily by default,
  ;; but eagerly with `force-load-doc-strings' set.
  (let ((file (expand-file-name "lazydoc.el" (ert-resource-directory))))
    (fmakunbound 'lazydoc-fun)
    (load file)
    (let ((f (symbol-function 'lazydoc-fun)))
      (should (byte-code-function-p f))
      (should (equal (aref f 4) (cons file 87))))

    (fmakunbound 'lazydoc-fun)
    (let ((load-force-doc-strings t))
      (load file)
      (let ((f (symbol-function 'lazydoc-fun)))
        (should (byte-code-function-p f))
        (should (equal (aref f 4) "My little\ndoc string\nhere"))))))

(ert-deftest lread-skip-to-eof ()
  ;; Check the special #@00 syntax that, for compatibility, reads as
  ;; nil while absorbing the remainder of the input.
  (with-temp-buffer
    (insert "#@00 and the rest\n"
            "should be ignored) entirely\n")
    (goto-char (point-min))
    (should (equal (read (current-buffer)) nil))
    (should (eobp))
    ;; Add an unbalanced bracket to the beginning and try again;
    ;; we should get an error.
    (goto-char (point-min))
    (insert "( ")
    (goto-char (point-min))
    (should-error (read (current-buffer)) :type 'end-of-file)))

;;; lread-tests.el ends here
