;;; editfns-tests.el -- tests for editfns.c

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

(ert-deftest format-properties ()
  ;; Bug #23730
  (should (ert-equal-including-properties
           (format (propertize "%d" 'face '(:background "red")) 1)
           #("1" 0 1 (face (:background "red")))))
  (should (ert-equal-including-properties
           (format (propertize "%2d" 'face '(:background "red")) 1)
           #(" 1" 0 2 (face (:background "red")))))
  (should (ert-equal-including-properties
           (format (propertize "%02d" 'face '(:background "red")) 1)
           #("01" 0 2 (face (:background "red")))))
  (should (ert-equal-including-properties
           (format (concat (propertize "%2d" 'x 'X)
                           (propertize "a" 'a 'A)
                           (propertize "b" 'b 'B))
                   1)
           #(" 1ab" 0 2 (x X) 2 3 (a A) 3 4 (b B))))

  ;; Bug #5306
  (should (ert-equal-including-properties
           (format "%.10s"
                   (concat "1234567890aaaa"
                           (propertize "12345678901234567890" 'xxx 25)))
           "1234567890"))
  (should (ert-equal-including-properties
           (format "%.10s"
                   (concat "123456789"
                           (propertize "12345678901234567890" 'xxx 25)))
           #("1234567891" 9 10 (xxx 25))))

  ;; Bug #23859
  (should (ert-equal-including-properties
           (format "%4s" (propertize "hi" 'face 'bold))
           #("  hi" 2 4 (face bold))))

  ;; Bug #23897
  (should (ert-equal-including-properties
           (format "%s" (concat (propertize "01234" 'face 'bold) "56789"))
           #("0123456789" 0 5 (face bold))))
  (should (ert-equal-including-properties
           (format "%s" (concat (propertize "01" 'face 'bold)
                                (propertize "23" 'face 'underline)
                                "45"))
           #("012345" 0 2 (face bold) 2 4 (face underline))))
  ;; The last property range is extended to include padding on the
  ;; right, but the first range is not extended to the left to include
  ;; padding on the left!
  (should (ert-equal-including-properties
           (format "%12s" (concat (propertize "01234" 'face 'bold) "56789"))
           #("  0123456789" 2 7 (face bold))))
  (should (ert-equal-including-properties
           (format "%-12s" (concat (propertize "01234" 'face 'bold) "56789"))
           #("0123456789  " 0 5 (face bold))))
  (should (ert-equal-including-properties
           (format "%10s" (concat (propertize "01" 'face 'bold)
                                  (propertize "23" 'face 'underline)
                                  "45"))
           #("    012345" 4 6 (face bold) 6 8 (face underline))))
  (should (ert-equal-including-properties
           (format "%-10s" (concat (propertize "01" 'face 'bold)
                                   (propertize "23" 'face 'underline)
                                   "45"))
           #("012345    " 0 2 (face bold) 2 4 (face underline))))
  (should (ert-equal-including-properties
           (format "%-10s" (concat (propertize "01" 'face 'bold)
                                   (propertize "23" 'face 'underline)
                                   (propertize "45" 'face 'italic)))
           #("012345    " 0 2 (face bold) 2 4 (face underline) 4 10 (face italic)))))

;; Tests for bug#5131.
(defun transpose-test-reverse-word (start end)
  "Reverse characters in a word by transposing pairs of characters."
  (let ((begm (make-marker))
        (endm (make-marker)))
    (set-marker begm start)
    (set-marker endm end)
    (while (> endm begm)
      (progn (transpose-regions begm (1+ begm) endm (1+ endm) t)
             (set-marker begm (1+ begm))
             (set-marker endm (1- endm))))))

(defun transpose-test-get-byte-positions (len)
  "Validate character position to byte position translation."
  (let ((bytes '()))
    (dotimes (pos len)
      (setq bytes (add-to-list 'bytes (position-bytes (1+ pos)) t)))
    bytes))

(ert-deftest transpose-ascii-regions-test ()
  (with-temp-buffer
    (erase-buffer)
    (insert "abcd")
    (transpose-test-reverse-word 1 4)
    (should (string= (buffer-string) "dcba"))
    (should (equal (transpose-test-get-byte-positions 5) '(1 2 3 4 5)))))

(ert-deftest transpose-nonascii-regions-test-1 ()
  (with-temp-buffer
    (erase-buffer)
    (insert "÷bcd")
    (transpose-test-reverse-word 1 4)
    (should (string= (buffer-string) "dcb÷"))
    (should (equal (transpose-test-get-byte-positions 5) '(1 2 3 4 6)))))

(ert-deftest transpose-nonascii-regions-test-2 ()
  (with-temp-buffer
    (erase-buffer)
    (insert "÷ab\"äé")
    (transpose-test-reverse-word 1 6)
    (should (string= (buffer-string) "éä\"ba÷"))
    (should (equal (transpose-test-get-byte-positions 7) '(1 3 5 6 7 8 10)))))

(ert-deftest format-c-float ()
  (should-error (format "%c" 0.5)))

;;; Test for Bug#29609.
(ert-deftest format-sharp-0-x ()
  (should (string-equal (format "%#08x" #x10) "0x000010"))
  (should (string-equal (format "%#05X" #x10) "0X010"))
  (should (string-equal (format "%#04x" 0) "0000")))

;;; Check format-time-string with various TZ settings.
;;; Use only POSIX-compatible TZ values, since the tests should work
;;; even if tzdb is not in use.
(ert-deftest format-time-string-with-zone ()
  ;; Don’t use (0 0 0 0) as the test case, as there are too many bugs
  ;; in MS-Windows (and presumably other) C libraries when formatting
  ;; time stamps near the Epoch of 1970-01-01 00:00:00 UTC, and this
  ;; test is for GNU Emacs, not for C runtimes.  Instead, look before
  ;; you leap: "look" is the timestamp just before the first leap
  ;; second on 1972-06-30 23:59:60 UTC, so it should format to the
  ;; same string regardless of whether the underlying C library
  ;; ignores leap seconds, while avoiding circa-1970 glitches.
  ;;
  ;; Similarly, stick to the limited set of time zones that are
  ;; supported by both POSIX and MS-Windows: exactly 3 ASCII letters
  ;; in the abbreviation, and no DST.
  (let ((look '(1202 22527 999999 999999))
        (format "%Y-%m-%d %H:%M:%S.%3N %z (%Z)"))
    ;; UTC.
    (should (string-equal
             (format-time-string "%Y-%m-%d %H:%M:%S.%3N %z" look t)
             "1972-06-30 23:59:59.999 +0000"))
    ;; "UTC0".
    (should (string-equal
             (format-time-string format look "UTC0")
             "1972-06-30 23:59:59.999 +0000 (UTC)"))
    ;; Negative UTC offset, as a Lisp list.
    (should (string-equal
             (format-time-string format look '(-28800 "PST"))
             "1972-06-30 15:59:59.999 -0800 (PST)"))
    ;; Negative UTC offset, as a Lisp integer.
    (should (string-equal
             (format-time-string format look -28800)
             ;; MS-Windows build replaces unrecognizable TZ values,
             ;; such as "-08", with "ZZZ".
             (if (eq system-type 'windows-nt)
                 "1972-06-30 15:59:59.999 -0800 (ZZZ)"
               "1972-06-30 15:59:59.999 -0800 (-08)")))
    ;; Positive UTC offset that is not an hour multiple, as a string.
    (should (string-equal
             (format-time-string format look "IST-5:30")
             "1972-07-01 05:29:59.999 +0530 (IST)"))))

;;; This should not dump core.
(ert-deftest format-time-string-with-outlandish-zone ()
  (should (stringp
           (format-time-string "%Y-%m-%d %H:%M:%S.%3N %z" nil
                               (concat (make-string 2048 ?X) "0")))))

(ert-deftest format-with-field ()
  (should (equal (format "First argument %2$s, then %3$s, then %1$s" 1 2 3)
                 "First argument 2, then 3, then 1"))
  (should (equal (format "a %2$s %3$d %1$d %2$S %3$d %4$d b" 11 "22" 33 44)
                 "a 22 33 11 \"22\" 33 44 b"))
  (should (equal (format "a %08$s %0000000000000000009$s b" 1 2 3 4 5 6 7 8 9)
                 "a 8 9 b"))
  (should (equal (should-error (format "a %999999$s b" 11))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error (format "a %2147483647$s b"))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error (format "a %9223372036854775807$s b"))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error (format "a %9223372036854775808$s b"))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error (format "a %18446744073709551615$s b"))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error (format "a %18446744073709551616$s b"))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error
                  (format (format "a %%%d$d b" most-positive-fixnum)))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error
                  (format (format "a %%%d$d b" (+ 1.0 most-positive-fixnum))))
                 '(error "Not enough arguments for format string")))
  (should (equal (should-error (format "a %$s b" 11))
                 '(error "Invalid format operation %$")))
  (should (equal (should-error (format "a %-1$s b" 11))
                 '(error "Invalid format operation %$")))
  (should (equal (format "%1$c %1$s" ?±) "± 177")))

(ert-deftest replace-buffer-contents-1 ()
  (with-temp-buffer
    (insert #("source" 2 4 (prop 7)))
    (let ((source (current-buffer)))
      (with-temp-buffer
        (insert "before dest after")
        (let ((marker (set-marker (make-marker) 14)))
          (save-restriction
            (narrow-to-region 8 12)
            (replace-buffer-contents source))
          (should (equal (marker-buffer marker) (current-buffer)))
          (should (equal (marker-position marker) 16)))
        (should (equal-including-properties
                 (buffer-string)
                 #("before source after" 9 11 (prop 7))))
        (should (equal (point) 9))))
    (should (equal-including-properties
             (buffer-string)
             #("source" 2 4 (prop 7))))))

(ert-deftest replace-buffer-contents-2 ()
  (with-temp-buffer
    (insert "foo bar baz qux")
    (let ((source (current-buffer)))
      (with-temp-buffer
        (insert "foo BAR baz qux")
        (replace-buffer-contents source)
        (should (equal-including-properties
                 (buffer-string)
                 "foo bar baz qux"))))))

;;; editfns-tests.el ends here
