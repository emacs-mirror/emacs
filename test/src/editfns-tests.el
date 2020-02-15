;;; editfns-tests.el -- tests for editfns.c

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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
           #("012345    "
             0 2 (face bold) 2 4 (face underline) 4 10 (face italic))))
  ;; Bug #38191
  (should (ert-equal-including-properties
           (format (propertize "‘foo’ %s bar" 'face 'bold) "xxx")
           #("‘foo’ xxx bar" 0 13 (face bold))))
  ;; Bug #32404
  (should (ert-equal-including-properties
           (format (concat (propertize "%s" 'face 'bold)
                           ""
                           (propertize "%s" 'face 'error))
                   "foo" "bar")
           #("foobar" 0 3 (face bold) 3 6 (face error))))
  (should (ert-equal-including-properties
           (format (concat "%s" (propertize "%s" 'face 'error)) "foo" "bar")
           #("foobar" 3 6 (face error))))
  (should (ert-equal-including-properties
           (format (concat "%s " (propertize "%s" 'face 'error)) "foo" "bar")
           #("foo bar" 4 7 (face error)))))

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


;;; Tests for Bug#30408.

(ert-deftest format-%d-large-float ()
  (should (string-equal (format "%d" 18446744073709551616.0)
                        "18446744073709551616"))
  (should (string-equal (format "%d" -18446744073709551616.0)
                        "-18446744073709551616")))

(ert-deftest format-%x-large-float ()
  (should (string-equal (format "%x" 18446744073709551616.0)
                        "10000000000000000")))
(ert-deftest read-large-integer ()
  (should (eq (type-of (read (format "%d0" most-negative-fixnum))) 'integer))
  (should (eq (type-of (read (format "%+d" (* -8.0 most-negative-fixnum))))
              'integer))
  (should (eq (type-of (read (substring (format "%d" most-negative-fixnum) 1)))
              'integer))
  (should (eq (type-of (read (format "#x%x" most-negative-fixnum)))
              'integer))
  (should (eq (type-of (read (format "#o%o" most-negative-fixnum)))
              'integer))
  (should (eq (type-of (read (format "#32rG%x" most-positive-fixnum)))
              'integer))
  (dolist (fmt '("%d" "%s" "#o%o" "#x%x"))
    (dolist (val (list most-negative-fixnum (1+ most-negative-fixnum)
		       -1 0 1
		       (1- most-positive-fixnum) most-positive-fixnum))
      (should (eq val (read (format fmt val)))))
    (dolist (val (list (1+ most-positive-fixnum)
		       (* 2 (1+ most-positive-fixnum))
		       (* 4 (1+ most-positive-fixnum))
		       (* 8 (1+ most-positive-fixnum))
		       18446744073709551616.0))
      (should (= val (read (format fmt val)))))))

(ert-deftest format-%o-negative-float ()
  (should (string-equal (format "%o" -1e-37) "0")))

;; Bug#31938
(ert-deftest format-%d-float ()
  (should (string-equal (format "%d" -1.1) "-1"))
  (should (string-equal (format "%d" -0.9) "0"))
  (should (string-equal (format "%d" -0.0) "0"))
  (should (string-equal (format "%d" 0.0) "0"))
  (should (string-equal (format "%d" 0.9) "0"))
  (should (string-equal (format "%d" 1.1) "1")))

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

(ert-deftest replace-buffer-contents-bug31837 ()
  (switch-to-buffer "a")
  (insert-char (char-from-name "SMILE"))
  (insert "1234")
  (switch-to-buffer "b")
  (insert-char (char-from-name "SMILE"))
  (insert "5678")
  (replace-buffer-contents "a")
  (should (equal (buffer-substring-no-properties (point-min) (point-max))
                 (concat (string (char-from-name "SMILE")) "1234"))))

(ert-deftest delete-region-undo-markers-1 ()
  "Make sure we don't end up with freed markers reachable from Lisp."
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30931#40
  (with-temp-buffer
    (insert "1234567890")
    (setq buffer-undo-list nil)
    (narrow-to-region 2 5)
    ;; `save-restriction' in a narrowed buffer creates two markers
    ;; representing the current restriction.
    (save-restriction
      (widen)
      ;; Any markers *within* the deleted region are put onto the undo
      ;; list.
      (delete-region 1 6))
    ;; (princ (format "%S" buffer-undo-list) #'external-debugging-output)
    ;; `buffer-undo-list' is now
    ;; (("12345" . 1) (#<temp-marker1> . -1) (#<temp-marker2> . 1))
    ;;
    ;; If temp-marker1 or temp-marker2 are freed prematurely, calling
    ;; `type-of' on them will cause Emacs to abort.  Calling
    ;; `garbage-collect' will also abort if it finds any reachable
    ;; freed objects.
    (should (eq (type-of (car (nth 1 buffer-undo-list))) 'marker))
    (should (eq (type-of (car (nth 2 buffer-undo-list))) 'marker))
    (garbage-collect)))

(ert-deftest delete-region-undo-markers-2 ()
  "Make sure we don't end up with freed markers reachable from Lisp."
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30931#55
  (with-temp-buffer
    (insert "1234567890")
    (setq buffer-undo-list nil)
    ;; signal_before_change creates markers delimiting a change
    ;; region.
    (let ((before-change-functions
           (list (lambda (beg end)
                   (delete-region (1- beg) (1+ end))))))
      (delete-region 2 5))
    ;; (princ (format "%S" buffer-undo-list) #'external-debugging-output)
    ;; `buffer-undo-list' is now
    ;; (("678" . 1) ("12345" . 1) (#<marker in no buffer> . -1)
    ;;  (#<temp-marker1> . -1) (#<temp-marker2> . -4))
    ;;
    ;; If temp-marker1 or temp-marker2 are freed prematurely, calling
    ;; `type-of' on them will cause Emacs to abort.  Calling
    ;; `garbage-collect' will also abort if it finds any reachable
    ;; freed objects.
    (should (eq (type-of (car (nth 3 buffer-undo-list))) 'marker))
    (should (eq (type-of (car (nth 4 buffer-undo-list))) 'marker))
    (garbage-collect)))

(ert-deftest format-bignum ()
  (let* ((s1 "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
         (v1 (read (concat "#x" s1)))
         (s2 "99999999999999999999999999999999")
         (v2 (read s2))
         (v3 #x-3ffffffffffffffe000000000000000))
    (should (> v1 most-positive-fixnum))
    (should (equal (format "%X" v1) s1))
    (should (> v2 most-positive-fixnum))
    (should (equal (format "%d" v2) s2))
    (should (equal (format "%d" v3) "-5316911983139663489309385231907684352"))
    (should (equal (format "%+d" v3) "-5316911983139663489309385231907684352"))
    (should (equal (format "%+d" (- v3))
                   "+5316911983139663489309385231907684352"))
    (should (equal (format "% d" (- v3))
                   " 5316911983139663489309385231907684352"))
    (should (equal (format "%o" v3)
                   "-37777777777777777777600000000000000000000"))
    (should (equal (format "%#50.40x" v3)
                   "        -0x000000003ffffffffffffffe000000000000000"))
    (should (equal (format "%-#50.40x" v3)
                   "-0x000000003ffffffffffffffe000000000000000        "))))

(ert-deftest test-group-name ()
  (let ((group-name (group-name (group-gid))))
    ;; If the GID has no associated entry in /etc/group there's no
    ;; name for it and `group-name' should return nil!
    (should (or (null group-name) (stringp group-name))))
  (should-error (group-name 'foo))
  (cond
   ((memq system-type '(windows-nt ms-dos))
    (should-not (group-name 123456789)))
   ((executable-find "getent")
    (with-temp-buffer
      (let (stat name)
      (dolist (gid (list 0 1212345 (group-gid)))
        (erase-buffer)
        (setq stat (ignore-errors
                     (call-process "getent" nil '(t nil) nil "group"
                                   (number-to-string gid))))
        (setq name (group-name gid))
        (goto-char (point-min))
        (cond ((eq stat 0)
               (if (looking-at "\\([[:alnum:]_-]+\\):")
                   (should (string= (match-string 1) name))))
              ((eq stat 2)
               (should-not name)))))))))

(ert-deftest test-translate-region-internal ()
  (with-temp-buffer
    (let ((max-char #16r3FFFFF)
          (tt (make-char-table 'translation-table)))
      (aset tt max-char ?*)
      (insert max-char)
      (translate-region-internal (point-min) (point-max) tt)
      (should (string-equal (buffer-string) "*")))))

;;; editfns-tests.el ends here
