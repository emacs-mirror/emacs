;;; lread-tests.el --- tests for lread.c -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

;; This file is part of GNU Emacs.

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

;;; Commentary:

;; Unit tests for code in src/lread.c.

;;; Code:

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

(ert-deftest lread-record-1 ()
  (should (equal '(#s(foo) #s(foo))
                 (read "(#1=#s(foo) #1#)"))))

(defmacro lread-tests--with-temp-file (file-name-var &rest body)
  (declare (indent 1))
  (cl-check-type file-name-var symbol)
  `(let ((,file-name-var (make-temp-file "emacs")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,file-name-var))))

(defun lread-tests--last-message ()
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (buffer-substring (line-beginning-position) (point)))))

(ert-deftest lread-tests--unescaped-char-literals ()
  "Check that loading warns about unescaped character
literals (Bug#20852)."
  (lread-tests--with-temp-file file-name
    (write-region "?) ?( ?; ?\" ?[ ?]" nil file-name)
    (should (equal (load file-name nil :nomessage :nosuffix) t))
    (should (equal (lread-tests--last-message)
                   (concat (format-message "Loading `%s': " file-name)
                           "unescaped character literals "
                           "`?\"', `?(', `?)', `?;', `?[', `?]' detected!")))))

(ert-deftest lread-tests--funny-quote-symbols ()
  "Check that 'smart quotes' or similar trigger errors in symbol names."
  (dolist (quote-char
           '(#x2018 ;; LEFT SINGLE QUOTATION MARK
             #x2019 ;; RIGHT SINGLE QUOTATION MARK
             #x201B ;; SINGLE HIGH-REVERSED-9 QUOTATION MARK
             #x201C ;; LEFT DOUBLE QUOTATION MARK
             #x201D ;; RIGHT DOUBLE QUOTATION MARK
             #x201F ;; DOUBLE HIGH-REVERSED-9 QUOTATION MARK
             #x301E ;; DOUBLE PRIME QUOTATION MARK
             #xFF02 ;; FULLWIDTH QUOTATION MARK
             #xFF07 ;; FULLWIDTH APOSTROPHE
             ))
    (let ((str (format "%cfoo" quote-char)))
     (should-error (read str) :type 'invalid-read-syntax)
     (should (eq (read (concat "\\" str)) (intern str))))))

(ert-deftest lread-test-bug26837 ()
  "Test for https://debbugs.gnu.org/26837 ."
  (let ((load-path (cons
                    (file-name-as-directory
                     (expand-file-name "data" (getenv "EMACS_TEST_DIRECTORY")))
                    load-path)))
    (load "somelib" nil t)
    (should (string-suffix-p "/somelib.el" (caar load-history)))
    (load "somelib2" nil t)
    (should (string-suffix-p "/somelib2.el" (caar load-history)))
    (load "somelib" nil t)
    (should (string-suffix-p "/somelib.el" (caar load-history)))))

(ert-deftest lread-tests--old-style-backquotes ()
  "Check that loading warns about old-style backquotes."
  (lread-tests--with-temp-file file-name
    (write-region "(` (a b))" nil file-name)
    (should (equal (load file-name nil :nomessage :nosuffix) t))
    (should (equal (lread-tests--last-message)
                   (concat (format-message "Loading `%s': " file-name)
                           "old-style backquotes detected!")))))

(ert-deftest lread-lread--substitute-object-in-subtree ()
  (let ((x (cons 0 1)))
    (setcar x x)
    (lread--substitute-object-in-subtree x 1 t)
    (should (eq x (cdr x)))))

;;; lread-tests.el ends here
