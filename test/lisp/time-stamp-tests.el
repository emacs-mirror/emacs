;;; time-stamp-tests.el --- tests for time-stamp.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Free Software Foundation, Inc.

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
(require 'generator)
(eval-when-compile (require 'cl-lib))
(require 'time-stamp)

(defmacro with-time-stamp-test-env (&rest body)
  "Evaluate BODY with some standard `time-stamp' test variables bound."
  (declare (indent 0) (debug t))
  `(let ((user-login-name "test-logname")
         (user-full-name "100%d Tester") ;verify "%" passed unchanged
         (buffer-file-name "/emacs/test/0-9AZaz (time)_stamped.file$+^")
         (mail-host-address "test-mail-host-name")
         (ref-time1 '(17337 16613))    ;Monday, Jan 2, 2006, 3:04:05 PM
         (ref-time2 '(22574 61591))    ;Friday, Nov 18, 2016, 12:14:15 PM
         (ref-time3 '(21377 34956))    ;Sunday, May 25, 2014, 06:07:08 AM
         (time-stamp-time-zone t))     ;use UTC
     (cl-letf (((symbol-function 'time-stamp-conv-warn)
                (lambda (old-format _new &optional _newer)
                  (ert-fail
                   (format "Unexpected format warning for '%s'" old-format)))))
       ;; Not all reference times are used in all tests;
       ;; suppress the byte compiler's "unused" warning.
       (list ref-time1 ref-time2 ref-time3)
       ,@body)))

(defmacro with-time-stamp-test-time (reference-time &rest body)
  "Force `time-stamp' to use time REFERENCE-TIME while evaluating BODY."
  (declare (indent 1) (debug t))
  (cl-with-gensyms (g-orig-time-stamp-string-fn)
    `(cl-letf*
         ((,g-orig-time-stamp-string-fn (symbol-function 'time-stamp-string))
          ((symbol-function 'time-stamp-string)
           (lambda (ts-format)
             (funcall ,g-orig-time-stamp-string-fn ts-format ,reference-time))))
       ,@body)))

(defmacro with-time-stamp-system-name (name &rest body)
  "Force function `system-name' to return NAME while evaluating BODY."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'system-name)
              (lambda () ,name)))
     ,@body))

(defmacro time-stamp-should-warn (form)
  "Similar to `should' and also verify that FORM generates a format warning."
  (declare (debug t))
  (cl-with-gensyms (g-warning-count)
    `(let ((,g-warning-count 0))
       (cl-letf (((symbol-function 'time-stamp-conv-warn)
                  (lambda (_old _new &optional _newer)
                    (incf ,g-warning-count))))
         (should ,form)
         (unless (= ,g-warning-count 1)
           (ert-fail (format "Should have warned about format: %S" ',form)))))))

;;; Tests:

;;; Tests of customization variables

(ert-deftest time-stamp-custom-time-zone ()
  "Test that setting `time-stamp-time-zone' affects the format."
  (with-time-stamp-test-env
    (let ((time-stamp-time-zone "PST8"))
      (should (equal (time-stamp-string "%H %Z" ref-time1) "07 PST")))
    (let ((time-stamp-time-zone "UTC0"))
      (should (equal (time-stamp-string "%H %Z" ref-time1) "15 UTC")))
    (let ((time-stamp-time-zone "GMT0"))
      (should (equal (time-stamp-string "%H %Z" ref-time1) "15 GMT")))))

(iter-defun time-stamp-test-pattern-sequential ()
  "Iterate through each possibility for a part of `time-stamp-pattern'."
  (let ((pattern-value-parts
         '(("4/" "10/" "-9/" "0/" "")                      ;0: line limit
           ("stamp:" "")                                   ;1: start
           ("%-d" "%_H" "%^a" "%#Z" "%:A" "%019z" "%%" "") ;2: format part 1
           (" " "x" ":" "\n" "")                           ;3: format part 2
           ("%-d" "%_H" "%^a" "%#Z" "%:A" "%019z" "%%")    ;4: format part 3
           ("end" ""))))                                   ;5: end
    (dotimes (cur (length pattern-value-parts))
      (dotimes (cur-index (length (nth cur pattern-value-parts)))
        (cl-flet ((extract-part
                   (lambda (desired-part)
                     (let ((part-list (nth desired-part pattern-value-parts)))
                       (if (= desired-part cur)
                           (nth cur-index part-list)
                         (nth 0 part-list))))))
          ;; Don't repeat the default pattern.
          (when (or (= cur 0) (> cur-index 0))
            ;; The whole format must start with %, so not all
            ;; generated combinations are valid
            (when (or (not (equal (extract-part 2) ""))
                      (equal (extract-part 3) ""))
              (iter-yield (list (extract-part 0)
                                (extract-part 1)
                                (apply #'concat
                                       (mapcar #'extract-part '(2 3 4)))
                                (extract-part 5))))))))))

(iter-defun time-stamp-test-pattern-multiply ()
  "Iterate through every combination of parts of `time-stamp-pattern'."
  (let ((line-limit-values '("" "4/"))
        (start-values '("" "/stamp1/"))
        (format-values '("" "%%" "%m"))
        (end-values '("" ">end")))
    ;; yield all combinations of the above
    (dolist (line-limit line-limit-values)
      (dolist (start start-values)
        (dolist (format format-values)
          (dolist (end end-values)
            ;; If the format is not supplied, the end cannot be either,
            ;; so not all generated combinations are valid.
            ;; (This is why the format can be supplied as "%%" to
            ;; preserve the default format.)
            (when (or (not (equal format ""))
                      (equal end ""))
              (iter-yield (list line-limit start format end)))))))))

(iter-defun time-stamp-test-pattern-all ()
  (iter-yield-from (time-stamp-test-pattern-sequential))
  (iter-yield-from (time-stamp-test-pattern-multiply)))

(ert-deftest time-stamp-custom-start ()
  "Test that `time-stamp' isn't stuck by a start matching 0 characters."
  (with-time-stamp-test-env
    (with-time-stamp-test-time ref-time1
      (let ((time-stamp-pattern "^%Y-%m-%d<-TS")) ;start matches 0 chars
        (with-temp-buffer
          (insert "\n<-TS\n")
          ;; we should advance to line 2 and find the template
          (time-stamp)
          (should (equal (buffer-string) "\n2006-01-02<-TS\n"))))
      (let ((time-stamp-pattern "\\b%Y-%m-%d\\b") ;start and end match 0 chars
            (time-stamp-count 2))
        (with-temp-buffer
          (insert "..")
          ;; the two time stamps should be in different places
          (time-stamp)
          (should (equal (buffer-string) "2006-01-02..2006-01-02"))))
      (let ((time-stamp-pattern "::%S\\_>") ;end matches 0 chars
            (time-stamp-count 2))
        (with-temp-buffer
          (insert "::0::0")
          ;; the second template should be found immediately after the first
          (time-stamp)
          (should (equal (buffer-string) "::05::05")))))))

(ert-deftest time-stamp-custom-pattern ()
  "Test that `time-stamp-pattern' is parsed correctly."
  (iter-do (pattern-parts (time-stamp-test-pattern-all))
    (cl-destructuring-bind (line-limit1 start1 whole-format end1) pattern-parts
      (cl-letf
          (((symbol-function 'time-stamp-once)
            (lambda (start search-limit ts-start ts-end
                           ts-format _format-lines _end-lines)
              ;; Verify that time-stamp parsed time-stamp-pattern and
              ;; called us with the correct pieces.
              (let ((limit-number (if (equal line-limit1 "")
                                      time-stamp-line-limit
                                    (string-to-number line-limit1))))
                (goto-char (point-min))
                (if (> limit-number 0)
                    (should (= search-limit (pos-bol (1+ limit-number))))
                  (should (= search-limit (point-max))))
                (goto-char (point-max))
                (if (< limit-number 0)
                    (should (= start (pos-bol (1+ limit-number))))
                  (should (= start (point-min)))))
              (if (equal start1 "")
                  (should (equal ts-start time-stamp-start))
                (should (equal ts-start start1)))
              (if (or (equal whole-format "")
                      (equal whole-format "%%"))
                  (should (equal ts-format time-stamp-format))
                (should (equal ts-format whole-format)))
              (if (equal end1 "")
                  (should (equal ts-end time-stamp-end))
                (should (equal ts-end end1)))
              ;; return nil to stop time-stamp from calling us again
              nil)))
        (let ((time-stamp-pattern (concat
                                   line-limit1 start1 whole-format end1))
              (case-fold-search nil))
          (with-temp-buffer
            ;; prep the buffer with more than the
            ;; largest line-limit1 number of lines
            (insert "\n\n\n\n\n\n\n\n\n\n\n\n")
            ;; Call time-stamp, which will call time-stamp-once,
            ;; triggering the tests above.
            (time-stamp)))))))

(ert-deftest time-stamp-custom-format-tabs-expand ()
  "Test that Tab characters expand in the format but not elsewhere."
  (with-time-stamp-test-env
    (let ((time-stamp-start "Updated in: <\t")
          ;; Tabs in the format should expand
          (time-stamp-format "\t%Y\t")
          (time-stamp-end "\t>"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
          (insert "Updated in: <\t\t>")
          (time-stamp)
          (should (equal (buffer-string)
                         "Updated in: <\t        2006    \t>")))))))

(ert-deftest time-stamp-custom-inserts-lines ()
  "Test that `time-stamp' inserts lines or not, as directed."
  (with-time-stamp-test-env
    (let ((time-stamp-start "Updated on:")
          ;; the newline in the format will insert a line if we let it
          (time-stamp-format "\n  %Y-%m-%d")
          (time-stamp-end "$")
          (time-stamp-inserts-lines nil) ;changed later in the test
          (buffer-expected-1line "Updated on:\n  2006-01-02\n")
          (buffer-expected-2line "Updated on:\n  2006-01-02\n  2006-01-02\n"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
          (insert "Updated on:\n\n")
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-1line))
          ;; second call should not add a line
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-1line))

          (setq time-stamp-inserts-lines t)
          ;; with time-stamp-inserts-lines set, should add a line
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-2line)))))))

(ert-deftest time-stamp-custom-end ()
  "Test that `time-stamp' finds the end pattern on the correct line."
  (with-time-stamp-test-env
    (let ((time-stamp-start "Updated on: <")
          (time-stamp-format "%Y-%m-%d")
          (time-stamp-end ">")          ;changed later in the test
          (buffer-original-contents "Updated on: <\n>\n")
          (buffer-expected-time-stamped "Updated on: <2006-01-02\n>\n"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
          (insert buffer-original-contents)
          ;; time-stamp-end is not on same line, should not be seen
          (time-stamp)
          (should (equal (buffer-string) buffer-original-contents))

          ;; add a newline to time-stamp-end, so it starts on same line
          (setq time-stamp-end "\n>")
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-time-stamped)))))))

(ert-deftest time-stamp-custom-count ()
  "Test that `time-stamp' updates no more than `time-stamp-count' templates."
  (with-time-stamp-test-env
    (let ((time-stamp-start "TS: <")
          (time-stamp-format "%Y-%m-%d")
          (time-stamp-count 0)          ;changed later in the test
          (buffer-expected-once "TS: <2006-01-02>TS: <>")
          (buffer-expected-twice "TS: <2006-01-02>TS: <2006-01-02>"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
          (insert "TS: <>TS: <>")
          (time-stamp)
          ;; even with count = 0, expect one time stamp
          (should (equal (buffer-string) buffer-expected-once)))
        (with-temp-buffer
          (setq time-stamp-count 1)
          (insert "TS: <>TS: <>")
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-once))

          (setq time-stamp-count 2)
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-twice)))))))

(ert-deftest time-stamp-custom-limit ()
  "Test that `time-stamp' can expand two templates near the line limit."
  (with-time-stamp-test-env
    (let ((time-stamp-start "TS: ")
          (time-stamp-format "%Y-%m-%d")
          (time-stamp-end "$")
          (time-stamp-count 2)
          (time-stamp-line-limit 1)     ;changed later in the test
          (buffer-starts-as "TS: \nTS: ")
          (buffer-expected-1 "TS: 2006-01-02\nTS: ")
          (buffer-expected-2 "TS: 2006-01-02\nTS: 2006-01-02"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
          (insert buffer-starts-as)
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-1)))
        (with-temp-buffer
          (insert buffer-starts-as)
          (setq time-stamp-line-limit 2)
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-2)))))))

(ert-deftest time-stamp-custom-file-name ()
  "Test that `time-stamp' isn't confused by a newline in the file name."
  (with-time-stamp-test-env
    (let ((time-stamp-format "1 %f")     ;changed later in the test
          (buffer-original-contents "Time-stamp: <>")
          (expected-1 "Time-stamp: <1 Embedded?Newline>")
          (expected-2 "Time-stamp: <2 Embedded?Newline>"))
      (with-temp-buffer
        (let ((buffer-file-name "Embedded\nNewline"))
          (insert buffer-original-contents)
          (time-stamp)
          (should (equal (buffer-string) expected-1))
          ;; If the first time-stamp inserted an unexpected newline, the
          ;; next time-stamp would be unable to find the end pattern.
          (setq time-stamp-format "2 %f")
          (time-stamp)
          (should (equal (buffer-string) expected-2)))))))

;;; Tests of time-stamp-string formatting

(ert-deftest time-stamp-format-day-of-week ()
  "Test `time-stamp' formats for named day of week."
  (with-time-stamp-test-env
    (let* ((Mon (format-time-string "%a" ref-time1 t))
           (MON (format-time-string "%^a" ref-time1 t))
           (mon (downcase (format-time-string "%a" ref-time1 t)))
           (Mon-tc (capitalize (format-time-string "%a" ref-time1 t)))
           (Monday (format-time-string "%A" ref-time1 t))
           (MONDAY (format-time-string "%^A" ref-time1 t))
           (monday (downcase (format-time-string "%A" ref-time1 t)))
           (Monday-tc (capitalize (format-time-string "%A" ref-time1 t)))
           (p4-Mon (string-pad Mon 4 ?\s t))
           (p4-MON (string-pad MON 4 ?\s t))
           (p10-Monday (string-pad Monday 10 ?\s t))
           (p10-MONDAY (string-pad MONDAY 10 ?\s t)))
      ;; implemented and recommended since 1997
      (should (equal (time-stamp-string "%#A" ref-time1) MONDAY))
      (should (equal (time-stamp-string "%#10A" ref-time1) p10-MONDAY))
      ;; implemented since 1997, recommended 1997-2024
      (should (equal (time-stamp-string "%3a" ref-time1) Mon))
      ;; recommended 1997-2019
      (should (equal (time-stamp-string "%:a" ref-time1) Monday))
      ;; recommended 1997-2019, warned since 2024, will change
      (time-stamp-should-warn
       (equal (time-stamp-string "%3A" ref-time1) MON))
      (time-stamp-should-warn
       (equal (time-stamp-string "%10A" ref-time1) p10-MONDAY))
      ;; implemented since 2001, recommended since 2019
      (should (equal (time-stamp-string "%#a" ref-time1) MON))
      (should (equal (time-stamp-string "%#3a" ref-time1) MON))
      (should (equal (time-stamp-string "%#4a" ref-time1) p4-MON))
      ;; implemented since 2001, recommended 2019-2024
      (should (equal (time-stamp-string "%:A" ref-time1) Monday))
      ;; broken 2019-2024
      (should (equal (time-stamp-string "%:10A" ref-time1) p10-Monday))
      ;; broken in 2019, changed in 2024
      (should (equal (time-stamp-string "%-A" ref-time1) Monday))
      (should (equal (time-stamp-string "%_A" ref-time1) Monday))
      ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
      (should (equal (time-stamp-string "%a" ref-time1) Mon))
      (should (equal (time-stamp-string "%4a" ref-time1) p4-Mon))
      (should (equal (time-stamp-string "%04a" ref-time1) p4-Mon))
      (should (equal (time-stamp-string "%A" ref-time1) Monday))
      (should (equal (time-stamp-string "%^A" ref-time1) MONDAY))
      ;; warned 1997-2019, changed in 2019
      (should (equal (time-stamp-string "%^a" ref-time1) MON))
      (should (equal (time-stamp-string "%^4a" ref-time1) p4-MON))
      ;; implemented since 2025
      (should (equal (time-stamp-string "%^#A" ref-time1) monday))
      (should (equal (time-stamp-string "%^#a" ref-time1) mon))
      (should (equal (time-stamp-string "%*A" ref-time1) Monday-tc))
      (should (equal (time-stamp-string "%*a" ref-time1) Mon-tc))
      ;; discouraged
      (should (equal (time-stamp-string "%:3a" ref-time1) "   "))
      )))

(ert-deftest time-stamp-format-month-name ()
  "Test `time-stamp' formats for month name."
  (with-time-stamp-test-env
    (let* ((Jan (format-time-string "%b" ref-time1 t))
           (JAN (format-time-string "%^b" ref-time1 t))
           (jan (downcase (format-time-string "%b" ref-time1 t)))
           (Jan-tc (capitalize (format-time-string "%^b" ref-time1 t)))
           (January (format-time-string "%B" ref-time1 t))
           (JANUARY (format-time-string "%^B" ref-time1 t))
           (january (downcase (format-time-string "%B" ref-time1 t)))
           (January-tc (capitalize (format-time-string "%B" ref-time1 t)))
           (p4-Jan (string-pad Jan 4 ?\s t))
           (p4-JAN (string-pad JAN 4 ?\s t))
           (p10-January (string-pad January 10 ?\s t))
           (p10-JANUARY (string-pad JANUARY 10 ?\s t)))
      ;; implemented and recommended since 1997
      (should (equal (time-stamp-string "%#B" ref-time1) JANUARY))
      (should (equal (time-stamp-string "%#10B" ref-time1) p10-JANUARY))
      ;; implemented since 1997, recommended 1997-2024
      (should (equal (time-stamp-string "%3b" ref-time1) Jan))
      ;; recommended 1997-2019
      (should (equal (time-stamp-string "%:b" ref-time1) January))
      ;; recommended 1997-2019, warned since 2024, will change
      (time-stamp-should-warn
       (equal (time-stamp-string "%3B" ref-time1) JAN))
      (time-stamp-should-warn
       (equal (time-stamp-string "%10B" ref-time1) p10-JANUARY))
      ;; implemented since 2001, recommended since 2019
      (should (equal (time-stamp-string "%#b" ref-time1) JAN))
      (should (equal (time-stamp-string "%#3b" ref-time1) JAN))
      (should (equal (time-stamp-string "%#4b" ref-time1) p4-JAN))
      ;; implemented since 2001, recommended 2019-2024
      (should (equal (time-stamp-string "%:B" ref-time1) January))
      ;; broken 2019-2024
      (should (equal (time-stamp-string "%:10B" ref-time1) p10-January))
      ;; broken in 2019, changed in 2024
      (should (equal (time-stamp-string "%-B" ref-time1) January))
      (should (equal (time-stamp-string "%_B" ref-time1) January))
      ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
      (should (equal (time-stamp-string "%b" ref-time1) Jan))
      (should (equal (time-stamp-string "%4b" ref-time1) p4-Jan))
      (should (equal (time-stamp-string "%04b" ref-time1) p4-Jan))
      (should (equal (time-stamp-string "%B" ref-time1) January))
      (should (equal (time-stamp-string "%^B" ref-time1) JANUARY))
      ;; warned 1997-2019, changed in 2019
      (should (equal (time-stamp-string "%^b" ref-time1) JAN))
      (should (equal (time-stamp-string "%^4b" ref-time1) p4-JAN))
      ;; implemented since 2025
      (should (equal (time-stamp-string "%^#B" ref-time1) january))
      (should (equal (time-stamp-string "%^#b" ref-time1) jan))
      (should (equal (time-stamp-string "%*B" ref-time1) January-tc))
      (should (equal (time-stamp-string "%*b" ref-time1) Jan-tc))
      ;; discouraged
      (should (equal (time-stamp-string "%:3b" ref-time1) "   "))
      )))

(ert-deftest time-stamp-format-day-of-month ()
  "Test `time-stamp' formats for day of month."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended until 2024
    (should (equal (time-stamp-string "%2d" ref-time1) " 2"))
    (should (equal (time-stamp-string "%2d" ref-time2) "18"))
    (should (equal (time-stamp-string "%02d" ref-time1) "02"))
    (should (equal (time-stamp-string "%02d" ref-time2) "18"))
    ;; recommended 1997-2019
    (should (equal (time-stamp-string "%:d" ref-time1) "2"))
    (should (equal (time-stamp-string "%:d" ref-time2) "18"))
    ;; implemented since 1997, recommended 2019-2024
    (should (equal (time-stamp-string "%1d" ref-time1) "2"))
    (should (equal (time-stamp-string "%1d" ref-time2) "18"))
    ;; warned 1997-2019, allowed 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%-d" ref-time1) "2"))
    (should (equal (time-stamp-string "%-d" ref-time2) "18"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%_d" ref-time1) " 2"))
    (should (equal (time-stamp-string "%_d" ref-time2) "18"))
    (should (equal (time-stamp-string "%d" ref-time1) "02"))
    (should (equal (time-stamp-string "%d" ref-time2) "18"))
    ;; discouraged
    (should (equal (time-stamp-string "%:2d" ref-time1) "  "))
    ))

(ert-deftest time-stamp-format-hours-24 ()
  "Test `time-stamp' formats for hour on a 24-hour clock."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended until 2024
    (should (equal (time-stamp-string "%2H" ref-time1) "15"))
    (should (equal (time-stamp-string "%2H" ref-time2) "12"))
    (should (equal (time-stamp-string "%2H" ref-time3) " 6"))
    (should (equal (time-stamp-string "%02H" ref-time1) "15"))
    (should (equal (time-stamp-string "%02H" ref-time2) "12"))
    (should (equal (time-stamp-string "%02H" ref-time3) "06"))
    ;; recommended 1997-2019
    (should (equal (time-stamp-string "%:H" ref-time1) "15"))
    (should (equal (time-stamp-string "%:H" ref-time2) "12"))
    (should (equal (time-stamp-string "%:H" ref-time3) "6"))
    ;; implemented since 1997, recommended 2019-2024
    (should (equal (time-stamp-string "%1H" ref-time1) "15"))
    (should (equal (time-stamp-string "%1H" ref-time2) "12"))
    (should (equal (time-stamp-string "%1H" ref-time3) "6"))
    ;; warned 1997-2019, allowed 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%-H" ref-time1) "15"))
    (should (equal (time-stamp-string "%-H" ref-time2) "12"))
    (should (equal (time-stamp-string "%-H" ref-time3) "6"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%_H" ref-time1) "15"))
    (should (equal (time-stamp-string "%_H" ref-time2) "12"))
    (should (equal (time-stamp-string "%_H" ref-time3) " 6"))
    (should (equal (time-stamp-string "%H" ref-time1) "15"))
    (should (equal (time-stamp-string "%H" ref-time2) "12"))
    (should (equal (time-stamp-string "%H" ref-time3) "06"))))

(ert-deftest time-stamp-format-hours-12 ()
  "Test `time-stamp' formats for hour on a 12-hour clock."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended until 2024
    (should (equal (time-stamp-string "%2I" ref-time1) " 3"))
    (should (equal (time-stamp-string "%2I" ref-time2) "12"))
    (should (equal (time-stamp-string "%2I" ref-time3) " 6"))
    (should (equal (time-stamp-string "%02I" ref-time1) "03"))
    (should (equal (time-stamp-string "%02I" ref-time2) "12"))
    (should (equal (time-stamp-string "%02I" ref-time3) "06"))
    ;; recommended 1997-2019
    (should (equal (time-stamp-string "%:I" ref-time1) "3")) ;PM
    (should (equal (time-stamp-string "%:I" ref-time2) "12")) ;PM
    (should (equal (time-stamp-string "%:I" ref-time3) "6")) ;AM
    ;; implemented since 1997, recommended 2019-2024
    (should (equal (time-stamp-string "%1I" ref-time1) "3"))
    (should (equal (time-stamp-string "%1I" ref-time2) "12"))
    (should (equal (time-stamp-string "%1I" ref-time3) "6"))
    ;; warned 1997-2019, allowed 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%-I" ref-time1) "3"))
    (should (equal (time-stamp-string "%-I" ref-time2) "12"))
    (should (equal (time-stamp-string "%-I" ref-time3) "6"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%_I" ref-time1) " 3"))
    (should (equal (time-stamp-string "%_I" ref-time2) "12"))
    (should (equal (time-stamp-string "%_I" ref-time3) " 6"))
    (should (equal (time-stamp-string "%I" ref-time1) "03"))
    (should (equal (time-stamp-string "%I" ref-time2) "12"))
    (should (equal (time-stamp-string "%I" ref-time3) "06"))))

(ert-deftest time-stamp-format-month-number ()
  "Test `time-stamp' formats for month number."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended until 2024
    (should (equal (time-stamp-string "%2m" ref-time1) " 1"))
    (should (equal (time-stamp-string "%2m" ref-time2) "11"))
    (should (equal (time-stamp-string "%02m" ref-time1) "01"))
    (should (equal (time-stamp-string "%02m" ref-time2) "11"))
    ;; recommended 1997-2019
    (should (equal (time-stamp-string "%:m" ref-time1) "1"))
    (should (equal (time-stamp-string "%:m" ref-time2) "11"))
    ;; implemented since 1997, recommended 2019-2024
    (should (equal (time-stamp-string "%1m" ref-time1) "1"))
    (should (equal (time-stamp-string "%1m" ref-time2) "11"))
    ;; warned 1997-2019, allowed 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%-m" ref-time1) "1"))
    (should (equal (time-stamp-string "%-m" ref-time2) "11"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%_m" ref-time1) " 1"))
    (should (equal (time-stamp-string "%_m" ref-time2) "11"))
    (should (equal (time-stamp-string "%m" ref-time1) "01"))
    (should (equal (time-stamp-string "%m" ref-time2) "11"))))

(ert-deftest time-stamp-format-minute ()
  "Test `time-stamp' formats for minute."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended until 2024
    (should (equal (time-stamp-string "%2M" ref-time1) " 4"))
    (should (equal (time-stamp-string "%2M" ref-time2) "14"))
    (should (equal (time-stamp-string "%02M" ref-time1) "04"))
    (should (equal (time-stamp-string "%02M" ref-time2) "14"))
    ;; recommended 1997-2019
    (should (equal (time-stamp-string "%:M" ref-time1) "4"))
    (should (equal (time-stamp-string "%:M" ref-time2) "14"))
    ;; implemented since 1997, recommended 2019-2024
    (should (equal (time-stamp-string "%1M" ref-time1) "4"))
    (should (equal (time-stamp-string "%1M" ref-time2) "14"))
    ;; warned 1997-2019, allowed 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%-M" ref-time1) "4"))
    (should (equal (time-stamp-string "%-M" ref-time2) "14"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%_M" ref-time1) " 4"))
    (should (equal (time-stamp-string "%_M" ref-time2) "14"))
    (should (equal (time-stamp-string "%M" ref-time1) "04"))
    (should (equal (time-stamp-string "%M" ref-time2) "14"))))

(ert-deftest time-stamp-format-second ()
  "Test `time-stamp' formats for second."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended until 2024
    (should (equal (time-stamp-string "%2S" ref-time1) " 5"))
    (should (equal (time-stamp-string "%2S" ref-time2) "15"))
    (should (equal (time-stamp-string "%02S" ref-time1) "05"))
    (should (equal (time-stamp-string "%02S" ref-time2) "15"))
    ;; recommended 1997-2019
    (should (equal (time-stamp-string "%:S" ref-time1) "5"))
    (should (equal (time-stamp-string "%:S" ref-time2) "15"))
    ;; implemented since 1997, recommended 2019-2024
    (should (equal (time-stamp-string "%1S" ref-time1) "5"))
    (should (equal (time-stamp-string "%1S" ref-time2) "15"))
    ;; warned 1997-2019, allowed 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%-S" ref-time1) "5"))
    (should (equal (time-stamp-string "%-S" ref-time2) "15"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%_S" ref-time1) " 5"))
    (should (equal (time-stamp-string "%_S" ref-time2) "15"))
    (should (equal (time-stamp-string "%S" ref-time1) "05"))
    (should (equal (time-stamp-string "%S" ref-time2) "15"))))

(ert-deftest time-stamp-format-year-2digit ()
  "Test `time-stamp' formats for %y."
  (with-time-stamp-test-env
    ;; implemented since 1995, recommended 1995-2024
    (should (equal (time-stamp-string "%02y" ref-time1) "06"))
    (should (equal (time-stamp-string "%02y" ref-time2) "16"))
    ;; recommended 1997-2019, warned since 2024
    (time-stamp-should-warn
     (equal (time-stamp-string "%:y" ref-time1) "2006"))
    (time-stamp-should-warn
     (equal (time-stamp-string "%:y" ref-time2) "2016"))
    ;; %-y and %_y warned 1997-2019, changed in 2019
    ;; (We don't expect these forms to be useful,
    ;; but we test here so that we can confidently state that
    ;; all 2-digit conversions behave identically.)
    (should (equal (time-stamp-string "%1y" ref-time1) "6"))
    (should (equal (time-stamp-string "%1y" ref-time2) "16"))
    (should (equal (time-stamp-string "%-y" ref-time1) "6"))
    (should (equal (time-stamp-string "%-y" ref-time2) "16"))
    (should (equal (time-stamp-string "%_y" ref-time1) " 6"))
    (should (equal (time-stamp-string "%_y" ref-time2) "16"))
    ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
    (should (equal (time-stamp-string "%y" ref-time1) "06"))
    (should (equal (time-stamp-string "%y" ref-time2) "16"))
    ;; implemented since 1995, warned since 2019, will change
    (time-stamp-should-warn
     (equal (time-stamp-string "%04y" ref-time1) "2006"))
    (time-stamp-should-warn
     (equal (time-stamp-string "%4y" ref-time1) "2006"))))

(ert-deftest time-stamp-format-year-4digit ()
  "Test `time-stamp' format %Y."
  (with-time-stamp-test-env
    ;; implemented since 1997, recommended since 2019
    (should (equal (time-stamp-string "%Y" ref-time1) "2006"))
    ;; numbers do not truncate
    (should (equal (time-stamp-string "%2Y" ref-time1) "2006"))
    (should (equal (time-stamp-string "%02Y" ref-time1) "2006"))))

(ert-deftest time-stamp-format-am-pm ()
  "Test `time-stamp' formats for AM and PM strings."
  (with-time-stamp-test-env
    (let ((pm (format-time-string "%P" ref-time1 t))
          (am (format-time-string "%P" ref-time3 t))
          (Pm (format-time-string "%p" ref-time1 t))
          (Am (format-time-string "%p" ref-time3 t))
          (Pm-tc (capitalize (format-time-string "%p" ref-time1 t)))
          (Am-tc (capitalize (format-time-string "%p" ref-time3 t)))
          (PM (format-time-string "%^p" ref-time1 t))
          (AM (format-time-string "%^p" ref-time3 t)))
      ;; implemented and recommended since 1997
      (should (equal (time-stamp-string "%#p" ref-time1) pm))
      (should (equal (time-stamp-string "%#p" ref-time3) am))
      ;; implemented since 1997, recommended 1997-2024
      (should (equal (time-stamp-string "%P" ref-time1) Pm))
      (should (equal (time-stamp-string "%P" ref-time3) Am))
      ;; implemented since 1997
      (should (equal (time-stamp-string "%^#p" ref-time1) pm))
      (should (equal (time-stamp-string "%^#p" ref-time3) am))
      ;; warned 1997-2019, changed in 2019, recommended (with caveat) since 2024
      (should (equal (time-stamp-string "%p" ref-time1) Pm))
      (should (equal (time-stamp-string "%p" ref-time3) Am))
      ;; changed in 2024
      (should (equal (time-stamp-string "%^p" ref-time1) PM))
      (should (equal (time-stamp-string "%^p" ref-time3) AM))
      (should (equal (time-stamp-string "%#^p" ref-time1) PM))
      (should (equal (time-stamp-string "%#^p" ref-time3) AM))
      (should (equal (time-stamp-string "%#P" ref-time1) pm))
      (should (equal (time-stamp-string "%#P" ref-time3) am))
      (should (equal (time-stamp-string "%^#P" ref-time1) pm))
      (should (equal (time-stamp-string "%^#P" ref-time3) am))
      (should (equal (time-stamp-string "%^P" ref-time1) ""))
      (should (equal (time-stamp-string "%^P" ref-time3) ""))
      ;; implemented since 2025
      (should (equal (time-stamp-string "%*p" ref-time1) Pm-tc))
      (should (equal (time-stamp-string "%*p" ref-time3) Am-tc))
      (should (equal (time-stamp-string "%*P" ref-time1) Pm-tc))
      (should (equal (time-stamp-string "%*P" ref-time3) Am-tc))
      ;; reserved for possible adding or removing periods (dots)
      (should (equal (time-stamp-string "%:p" ref-time1) Pm))
      (should (equal (time-stamp-string "%#:p" ref-time1) pm))
      (should (equal (time-stamp-string "%^:p" ref-time1) PM))
      (should (equal (time-stamp-string "%.p" ref-time1) Pm))
      (should (equal (time-stamp-string "%#.p" ref-time1) pm))
      (should (equal (time-stamp-string "%^.p" ref-time1) PM))
      (should (equal (time-stamp-string "%@p" ref-time1) Pm))
      (should (equal (time-stamp-string "%#@p" ref-time1) pm))
      (should (equal (time-stamp-string "%^@p" ref-time1) PM))
      )))

(ert-deftest time-stamp-format-day-number-in-week ()
  "Test `time-stamp' formats for day number in week."
  (with-time-stamp-test-env
    (should (equal (time-stamp-string "%w" ref-time1) "1"))
    (should (equal (time-stamp-string "%w" ref-time2) "5"))
    (should (equal (time-stamp-string "%w" ref-time3) "0"))))

(ert-deftest time-stamp-format-time-zone-name ()
  "Test `time-stamp' format %Z."
  (with-time-stamp-test-env
    (let ((UTC-abbr (format-time-string "%Z" ref-time1 t))
          (Utc-abbr (capitalize (format-time-string "%Z" ref-time1 t)))
          (utc-abbr (format-time-string "%#Z" ref-time1 t)))
      ;; implemented and recommended since 1995
      (should (equal (time-stamp-string "%Z" ref-time1) UTC-abbr))
      ;; implemented since 1997, recommended since 2019
      (should (equal (time-stamp-string "%#Z" ref-time1) utc-abbr))
      ;; ^ accepted and ignored since 1995/1997, test for consistency with %p
      (should (equal (time-stamp-string "%^Z" ref-time1) UTC-abbr))
      (should (equal (time-stamp-string "%^#Z" ref-time1) utc-abbr))
      ;; implemented since 2025
      (should (equal (time-stamp-string "%*Z" ref-time1) Utc-abbr))
      )))

(ert-deftest time-stamp-format-time-zone-offset ()
  "Test `time-stamp' legacy format %z and spot-test new offset format %5z."
  (with-time-stamp-test-env
    (let ((utc-abbr (format-time-string "%#Z" ref-time1 t)))
    ;; recommended 1995-2019, warned since 2019, will change
      (time-stamp-should-warn
       (equal (time-stamp-string "%z" ref-time1) utc-abbr)))
    ;; implemented and recommended (with compat caveat) since 2019
    (should (equal (time-stamp-string "%5z" ref-time1) "+0000"))
    (let ((time-stamp-time-zone "PST8"))
      (should (equal (time-stamp-string "%5z" ref-time1) "-0800")))
    (let ((time-stamp-time-zone '(-36000 "HST")))
      (should (equal (time-stamp-string "%5z" ref-time1) "-1000")))
    (let ((time-stamp-time-zone "CET-1"))
      (should (equal (time-stamp-string "%5z" ref-time1) "+0100")))
    ;; implemented since 2019, recommended (with compat caveat) since 2024
    ;; See also the "formatz" tests below, which since 2021 test more
    ;; variants with more offsets.
    (should (equal (time-stamp-string "%-z" ref-time1) "+00"))
    (should (equal (time-stamp-string "%:::z" ref-time1) "+00"))
    (should (equal (time-stamp-string "%:z" ref-time1) "+00:00"))
    ;; implemented since 2019
    (should (equal (time-stamp-string "%::z" ref-time1) "+00:00:00"))
    (should (equal (time-stamp-string "%9::z" ref-time1) "+00:00:00"))))

(ert-deftest time-stamp-format-non-date-conversions ()
  "Test `time-stamp' formats for non-date items."
  (with-time-stamp-test-env
    (with-time-stamp-system-name "test-system-name.example.org"
      ;; implemented and recommended since 1995
      (should (equal (time-stamp-string "%%" ref-time1) "%")) ;% last char
      (should (equal (time-stamp-string "%%P" ref-time1) "%P")) ;% not last char
      (should (equal (time-stamp-string "%f" ref-time1)
                     "0-9AZaz (time)_stamped.file$+^"))
      (should (equal (time-stamp-string "%F" ref-time1)
                     "/emacs/test/0-9AZaz (time)_stamped.file$+^"))
      (with-temp-buffer
        (should (equal (time-stamp-string "%f" ref-time1) "(no file)"))
        (should (equal (time-stamp-string "%F" ref-time1) "(no file)")))
      (should (equal (time-stamp-string "%h" ref-time1) "test-mail-host-name"))
      (let ((mail-host-address nil))
        (should (equal (time-stamp-string "%h" ref-time1)
                       "test-system-name.example.org")))
      ;; recommended 1997-2019, warned since 2024
      (time-stamp-should-warn
       (equal (time-stamp-string "%s" ref-time1)
              "test-system-name.example.org"))
      (time-stamp-should-warn
       (equal (time-stamp-string "%U" ref-time1) "100%d Tester"))
      (time-stamp-should-warn
       (equal (time-stamp-string "%u" ref-time1) "test-logname"))
      ;; implemented since 2001, recommended since 2019
      (should (equal (time-stamp-string "%L" ref-time1) "100%d Tester"))
      (should (equal (time-stamp-string "%l" ref-time1) "test-logname"))
      ;; implemented since 2007, recommended since 2019
      (should (equal (time-stamp-string "%Q" ref-time1)
                     "test-system-name.example.org"))
      (should (equal (time-stamp-string "%q" ref-time1) "test-system-name"))
      ;; implemented since 2025
      (should (equal (time-stamp-string "%X" ref-time1)
                     "test-system-name.example.org"))
      (should (equal (time-stamp-string "%x" ref-time1) "test-system-name")))
    (with-time-stamp-system-name "sysname-no-dots"
      ;; implemented since 2007, recommended since 2019
      (should (equal (time-stamp-string "%Q" ref-time1) "sysname-no-dots"))
      (should (equal (time-stamp-string "%q" ref-time1) "sysname-no-dots"))
      ;; implemented since 2025
      (should (equal (time-stamp-string "%X" ref-time1) "sysname-no-dots"))
      (should (equal (time-stamp-string "%x" ref-time1) "sysname-no-dots"))
      )))

(ert-deftest time-stamp-format-ignored-modifiers ()
  "Test additional args allowed (but ignored) to allow for future expansion."
  (with-time-stamp-test-env
    (let ((May (format-time-string "%B" ref-time3 t)))
      ;; allowed modifiers
      (should (equal (time-stamp-string "%.,@+~EO (stuff)B" ref-time3) May))
      ;; parens nest
      (should (equal (time-stamp-string "%(st(u)ff)B" ref-time3) May))
      ;; escaped parens do not change the nesting level
      (should (equal (time-stamp-string "%(st\\)u\\(ff)B" ref-time3) May))
      ;; incorrectly nested parens do not crash us
      (should-not (equal (time-stamp-string "%(stuffB" ref-time3) May))
      (should-not (equal (time-stamp-string "%)B" ref-time3) May))
      ;; unterminated format does not crash us
      (should-not (equal (time-stamp-string "%" ref-time3) May))
      ;; not all punctuation is allowed
      (should-not (equal (time-stamp-string "%&B" ref-time3) May))
      (should-not (equal (time-stamp-string "%/B" ref-time3) May))
      (should-not (equal (time-stamp-string "%;B" ref-time3) May)))))

(ert-deftest time-stamp-format-non-conversions ()
  "Test that without a %, the text is copied literally."
  (with-time-stamp-test-env
    (should (equal (time-stamp-string "No percent" ref-time1) "No percent"))))

(ert-deftest time-stamp-format-multiple-conversions ()
  "Test that multiple %-conversions are independent."
  (with-time-stamp-test-env
    (let ((Mon (format-time-string "%a" ref-time1 t))
          (MON (format-time-string "%^a" ref-time1 t))
          (Monday (format-time-string "%A" ref-time1 t)))
      ;; change-case flag is independent
      (should (equal (time-stamp-string "%a.%#a.%a" ref-time1)
                     (concat Mon "." MON "." Mon)))
      ;; up-case flag is independent
      (should (equal (time-stamp-string "%a.%^a.%a" ref-time1)
                     (concat Mon "." MON "." Mon)))
      ;; underscore flag is independent
      (should (equal (time-stamp-string "%_d.%d.%_d" ref-time1) " 2.02. 2"))
      (should (equal (time-stamp-string "%_7z.%7z.%_7z" ref-time1)
                     "+000000.+0000  .+000000"))
      ;; minus flag is independent
      (should (equal (time-stamp-string "%d.%-d.%d" ref-time1) "02.2.02"))
      (should (equal (time-stamp-string "%3z.%-3z.%3z" ref-time1)
                     "+0000.+00.+0000"))
      ;; 0 flag is independent
      (should (equal (time-stamp-string "%2d.%02d.%2d" ref-time1) " 2.02. 2"))
      (should (equal (time-stamp-string "%6:::z.%06:::z.%6:::z" ref-time1)
                     "+00   .+00:00.+00   "))
      ;; field width is independent
      (should (equal
               (time-stamp-string "%6Y.%Y.%6Y" ref-time1) "  2006.2006.  2006"))
      ;; colon modifier is independent
      (should (equal (time-stamp-string "%a.%:a.%a" ref-time1)
                     (concat Mon "." Monday "." Mon)))
      (should (equal (time-stamp-string "%5z.%5::z.%5z" ref-time1)
                     "+0000.+00:00:00.+0000"))
      ;; format character is independent
      (should (equal (time-stamp-string "%H:%M%%%S" ref-time1) "15:04%05")))))

(ert-deftest time-stamp-format-string-width ()
  "Test `time-stamp' string width modifiers."
  (with-time-stamp-test-env
    (let ((UTC-abbr (format-time-string "%Z" ref-time1 t)))
      (should (equal (time-stamp-string "%1%" ref-time3) "%"))
      (should (equal (time-stamp-string "%2%" ref-time3) " %"))
      (should (equal (time-stamp-string "%9%" ref-time3) "        %"))
      (should (equal (time-stamp-string "%10%" ref-time3) "         %"))
      (should (equal (time-stamp-string "%03d" ref-time3) "025"))
      (should (equal (time-stamp-string "%3d" ref-time3) " 25"))
      (should (equal (time-stamp-string "%_3d" ref-time3) " 25"))
      (should (equal (time-stamp-string "%99z" ref-time1)
                     (time-stamp-string "%100z" ref-time1)))
      (should (equal (time-stamp-string "%099Y" ref-time1)
                     (time-stamp-string "%0100Y" ref-time1)))
      ;; since 2024
      (should (equal (time-stamp-string "%0d" ref-time1) "02"))
      (should (equal (time-stamp-string "%0d" ref-time2) "18"))
      ;; broken 2019-2024
      (should (equal (time-stamp-string "%-Z" ref-time1) UTC-abbr))
      (should (equal (time-stamp-string "%_Z" ref-time1) UTC-abbr)))))

(ert-deftest time-stamp-format-letter-case ()
  "Test `time-stamp' upcase and downcase modifiers not tested elsewhere."
  (with-time-stamp-test-env
    (let ((MONDAY (format-time-string "%^A" ref-time1 t)))
      (should (equal (time-stamp-string "%*^A" ref-time1) MONDAY))
      (should (equal (time-stamp-string "%*#A" ref-time1) MONDAY))
      )))

;;; Tests of helper functions

(ert-deftest time-stamp-helper-string-defaults ()
  "Test that `time-stamp-string' defaults its format to `time-stamp-format'."
  (with-time-stamp-test-env
    (should (equal (time-stamp-string nil ref-time1)
                   (time-stamp-string time-stamp-format ref-time1)))
    (should (equal (time-stamp-string 'not-a-string ref-time1) nil))))

(ert-deftest time-stamp-helper-zone-type-p ()
  "Test `time-stamp-zone-type-p'."
  (should (time-stamp-zone-type-p t))
  (should (time-stamp-zone-type-p nil))
  (should (time-stamp-zone-type-p 'wall))
  (should-not (time-stamp-zone-type-p 'floor))
  (should (time-stamp-zone-type-p "arbitrary string"))
  (should (time-stamp-zone-type-p 0))
  (should-not (time-stamp-zone-type-p 3.14))
  (should-not (time-stamp-zone-type-p '(0)))
  (should-not (time-stamp-zone-type-p '(0 . "A")))
  (should (time-stamp-zone-type-p '(0 "A")))
  (should-not (time-stamp-zone-type-p '(0 0)))
  (should-not (time-stamp-zone-type-p '("A" "A"))))

(ert-deftest time-stamp-helper-safe-locals ()
  "Test that our variables are known to be safe local variables."
  (should (safe-local-variable-p 'time-stamp-format "a string"))
  (should-not (safe-local-variable-p 'time-stamp-format '(a list)))
  (should (safe-local-variable-p 'time-stamp-time-zone "a string"))
  (should-not (safe-local-variable-p 'time-stamp-time-zone 0.5))
  (should (safe-local-variable-p 'time-stamp-line-limit -10))
  (should-not (safe-local-variable-p 'time-stamp-line-limit "a string"))
  (should (safe-local-variable-p 'time-stamp-start "a string"))
  (should-not (safe-local-variable-p 'time-stamp-start 17))
  (should (safe-local-variable-p 'time-stamp-end "a string"))
  (should-not (safe-local-variable-p 'time-stamp-end 17))
  (should (safe-local-variable-p 'time-stamp-inserts-lines t))
  (should-not (safe-local-variable-p 'time-stamp-inserts-lines 17))
  (should (safe-local-variable-p 'time-stamp-count 2))
  (should-not (safe-local-variable-p 'time-stamp-count 100))
  (should-not (safe-local-variable-p 'time-stamp-count t))
  (should (safe-local-variable-p 'time-stamp-pattern "a string"))
  (should-not (safe-local-variable-p 'time-stamp-pattern 17)))

;;;; Setup for tests of time offset formatting with %z

(defun formatz (format zone)
  "Use FORMAT to format the offset of ZONE, returning the result.
FORMAT must be time format \"%z\" or some variation thereof.
ZONE is as the ZONE argument of the `format-time-string' function.
This function is called by 99% of the `time-stamp' \"%z\" unit tests."
  (with-time-stamp-test-env
    (let ((time-stamp-time-zone zone))
      ;; Call your favorite time formatter here.
      ;; For narrower-scope unit testing,
      ;; instead of calling time-stamp-string here,
      ;; we could directly call (format-time-offset format zone)
      (time-stamp-string format)
      )))

(defun format-time-offset (format offset-secs)
  "Use FORMAT to format the time zone represented by OFFSET-SECS.
FORMAT must be time format \"%z\" or some variation thereof.
This function is a wrapper around `time-stamp-formatz-from-parsed-options'
and is called by some low-level `time-stamp' \"%z\" unit tests."
  ;; This wrapper adds a simple regexp-based parser that handles only
  ;; %z and variants.  In normal use, time-stamp-formatz-from-parsed-options
  ;; is called from a parser that handles all time string formats.
  (string-match
   "\\`\\([^%]*\\)%\\([-_]?\\)\\(0?\\)\\([1-9][0-9]*\\)?\\([EO]?\\)\\(:*\\)\\([^a-zA-Z]+\\)?z\\(.*\\)"
   format)
  (let ((leading-string (match-string 1 format))
        (flag-minimize (seq-find (lambda (x) (eq x ?-))
                                 (match-string 2 format)))
        (flag-pad-with-spaces (seq-find (lambda (x) (eq x ?_))
                                        (match-string 2 format)))
        (flag-pad-with-zeros (equal (match-string 3 format) "0"))
        (field-width (string-to-number (or (match-string 4 format) "")))
        (colon-count (length (match-string 6 format)))
        (garbage (match-string 7 format))
        (trailing-string (match-string 8 format)))
    (concat leading-string
            (if garbage
                ""
              (time-stamp-formatz-from-parsed-options flag-minimize
                                                      flag-pad-with-spaces
                                                      flag-pad-with-zeros
                                                      colon-count
                                                      field-width
                                                      offset-secs))
            trailing-string)))

(defun fz-make+zone (h &optional m s)
  "Create a non-negative offset from interval H M S."
  (declare (pure t))
  (let ((m (or m 0))
        (s (or s 0)))
    (+ (* 3600 h) (* 60 m) s)))

(defun fz-make-zone (h &optional m s)
  "Create a negative offset.
The interval arguments H M and S are all non-negative."
  (declare (pure t))
  (- (fz-make+zone h m s)))

(defmacro formatz-should-equal (zone expect)
  "Format ZONE and compare it to EXPECT.
Use the free variables `form-string' and `pattern-mod'.
The functions in `pattern-mod' are composed left to right."
  (declare (debug t))
  (cl-with-gensyms (g-result g-fn)
    `(let ((,g-result ,expect))
       (dolist (,g-fn pattern-mod)
         (setq ,g-result (funcall ,g-fn ,g-result)))
       (should (equal (formatz form-string ,zone) ,g-result)))))

;; These test cases have zeros in all places (first, last, none, both)
;; for hours, minutes, and seconds.

(defun formatz-hours-exact-helper (form-string pattern-mod)
  "Test format %z with whole hours."
  (formatz-should-equal (fz-make+zone 0) "+00") ;0 sign always +, both digits
  (formatz-should-equal (fz-make+zone 10) "+10")
  (formatz-should-equal (fz-make-zone 10) "-10")
  (formatz-should-equal (fz-make+zone 2) "+02")
  (formatz-should-equal (fz-make-zone 2) "-02")
  (formatz-should-equal (fz-make+zone 13) "+13")
  (formatz-should-equal (fz-make-zone 13) "-13")
  )

(defun formatz-nonzero-minutes-helper (form-string pattern-mod)
  "Test format %z with whole minutes."
  (formatz-should-equal (fz-make+zone 0 30) "+00:30") ;has hours even though 0
  (formatz-should-equal (fz-make-zone 0 30) "-00:30")
  (formatz-should-equal (fz-make+zone 0 4) "+00:04")
  (formatz-should-equal (fz-make-zone 0 4) "-00:04")
  (formatz-should-equal (fz-make+zone 8 40) "+08:40")
  (formatz-should-equal (fz-make-zone 8 40) "-08:40")
  (formatz-should-equal (fz-make+zone 0 15) "+00:15")
  (formatz-should-equal (fz-make-zone 0 15) "-00:15")
  (formatz-should-equal (fz-make+zone 11 30) "+11:30")
  (formatz-should-equal (fz-make-zone 11 30) "-11:30")
  (formatz-should-equal (fz-make+zone 3 17) "+03:17")
  (formatz-should-equal (fz-make-zone 3 17) "-03:17")
  (formatz-should-equal (fz-make+zone 12 45) "+12:45")
  (formatz-should-equal (fz-make-zone 12 45) "-12:45")
  )

(defun formatz-nonzero-seconds-helper (form-string pattern-mod)
  "Test format %z with non-0 seconds."
  ;; non-0 seconds are always included
  (formatz-should-equal (fz-make+zone 0 0 50) "+00:00:50")
  (formatz-should-equal (fz-make-zone 0 0 50) "-00:00:50")
  (formatz-should-equal (fz-make+zone 0 0 06) "+00:00:06")
  (formatz-should-equal (fz-make-zone 0 0 06) "-00:00:06")
  (formatz-should-equal (fz-make+zone 0 7 50) "+00:07:50")
  (formatz-should-equal (fz-make-zone 0 7 50) "-00:07:50")
  (formatz-should-equal (fz-make+zone 0 0 16) "+00:00:16")
  (formatz-should-equal (fz-make-zone 0 0 16) "-00:00:16")
  (formatz-should-equal (fz-make+zone 0 12 36) "+00:12:36")
  (formatz-should-equal (fz-make-zone 0 12 36) "-00:12:36")
  (formatz-should-equal (fz-make+zone 0 3 45) "+00:03:45")
  (formatz-should-equal (fz-make-zone 0 3 45) "-00:03:45")
  (formatz-should-equal (fz-make+zone 8 45 30) "+08:45:30")
  (formatz-should-equal (fz-make-zone 8 45 30) "-08:45:30")
  (formatz-should-equal (fz-make+zone 0 11 45) "+00:11:45")
  (formatz-should-equal (fz-make-zone 0 11 45) "-00:11:45")
  (formatz-should-equal (fz-make+zone 3 20 15) "+03:20:15")
  (formatz-should-equal (fz-make-zone 3 20 15) "-03:20:15")
  (formatz-should-equal (fz-make+zone 11 14 30) "+11:14:30")
  (formatz-should-equal (fz-make-zone 11 14 30) "-11:14:30")
  (formatz-should-equal (fz-make+zone 12 30 49) "+12:30:49")
  (formatz-should-equal (fz-make-zone 12 30 49) "-12:30:49")
  (formatz-should-equal (fz-make+zone 12 0 34) "+12:00:34")
  (formatz-should-equal (fz-make-zone 12 0 34) "-12:00:34")
  )

(defun formatz-hours-big-helper (form-string pattern-mod)
  "Test format %z with hours that don't fit in two digits."
  (formatz-should-equal (fz-make+zone 101) "+101:00")
  (formatz-should-equal (fz-make+zone 123 10) "+123:10")
  (formatz-should-equal (fz-make-zone 123 10) "-123:10")
  (formatz-should-equal (fz-make+zone 123 2) "+123:02")
  (formatz-should-equal (fz-make-zone 123 2) "-123:02")
  )

(defun formatz-seconds-big-helper (form-string pattern-mod)
  "Test format %z with hours greater than 99 and non-zero seconds."
  (formatz-should-equal (fz-make+zone 123 0 30) "+123:00:30")
  (formatz-should-equal (fz-make-zone 123 0 30) "-123:00:30")
  (formatz-should-equal (fz-make+zone 120 0 4) "+120:00:04")
  (formatz-should-equal (fz-make-zone 120 0 4) "-120:00:04")
  )

;; Functions that modify the expected output string, so that we can
;; use the above test cases for multiple formats.

(defun formatz-mod-del-colons (string)
  "Return STRING with any colons removed."
  (string-replace ":" "" string))

(defun formatz-mod-add-00 (string)
  "Return STRING with \"00\" appended."
  (concat string "00"))

(defun formatz-mod-add-colon00 (string)
  "Return STRING with \":00\" appended."
  (concat string ":00"))

(defun formatz-mod-pad-r10 (string)
  "Return STRING padded on the right to 10 characters."
  (string-pad string 10))

(defun formatz-mod-pad-r12 (string)
  "Return STRING padded on the right to 12 characters."
  (string-pad string 12))

;; Convenience macro for generating groups of test cases.

(defmacro define-formatz-tests
    (form-strings hour-mod mins-mod secs-mod big-mod secbig-mod)
  "Define tests for time formats FORM-STRINGS.
FORM-STRINGS is a list of formats, each \"%z\" or some variation thereof.

Each of the remaining arguments is an unquoted list of the form
(SAMPLE-OUTPUT . MODIFIERS).  SAMPLE-OUTPUT is the result of the
FORM-STRINGS for a particular offset, detailed below for each argument.
The remaining elements of the list, the MODIFIERS, are the names of
functions to modify the expected results for sets of tests.
The MODIFIERS do not modify the SAMPLE-OUTPUT.

The one, literal sample output is given in the call to this macro
to provide a visual check at the call site that the format
behaves as expected.

HOUR-MOD is the result for offset 0 and modifiers for the other
expected results for whole hours.
MINS-MOD is the result for offset +30 minutes and modifiers for the
other expected results for whole minutes.
SECS-MOD is the result for offset +30 seconds and modifiers for the
other expected results for offsets with non-zero seconds.
BIG-MOD is the result for offset +100 hours and modifiers for the other
expected results for hours greater than 99 with a whole number of minutes.
SECBIG-MOD is the result for offset +100 hours 30 seconds and modifiers for
the other expected results for hours greater than 99 with non-zero seconds."
  (declare (indent 1) (debug (&rest sexp)))
  ;; Generate a form to create a list of tests to define.  When this
  ;; macro is called, the form is evaluated, thus defining the tests.
  ;; We will modify this list, so start with a list consed at runtime.
  (let ((ert-test-list (list 'progn))
        (common-description
         (concat "\nThis test is defined by a call to"
                 " the macro `define-formatz-tests'.")))
    (dolist (form-string form-strings ert-test-list)
      (let ((test-name-hhmm
             (intern (concat "formatz-" form-string "-hhmm")))
            (test-name-seconds
             (intern (concat "formatz-" form-string "-seconds")))
            (test-name-threedigit
             (intern (concat "formatz-" form-string "-threedigit"))))
        (nconc
         ert-test-list
         (list
          `(find-function-update-type-alist
            ',test-name-hhmm 'ert--test 'formatz-find-test-def-function)
          `(ert-deftest ,test-name-hhmm ()
             ,(concat "Test `time-stamp' format " form-string
                      " with whole hours and whole minutes.\n"
                      common-description)
             (should (equal (formatz ,form-string (fz-make+zone 0))
                            ,(car hour-mod)))
             (formatz-hours-exact-helper ,form-string ',(cdr hour-mod))
             (should (equal (formatz ,form-string (fz-make+zone 0 30))
                            ,(car mins-mod)))
             (formatz-nonzero-minutes-helper ,form-string ',(cdr mins-mod)))
          `(find-function-update-type-alist
            ',test-name-seconds 'ert--test 'formatz-find-test-def-function)
          `(ert-deftest ,test-name-seconds ()
             ,(concat "Test `time-stamp' format " form-string
                      " with offsets that have non-zero seconds.\n"
                      common-description)
             (should (equal (formatz ,form-string (fz-make+zone 0 0 30))
                            ,(car secs-mod)))
             (formatz-nonzero-seconds-helper ,form-string ',(cdr secs-mod)))
          `(find-function-update-type-alist
            ',test-name-threedigit 'ert--test 'formatz-find-test-def-function)
          `(ert-deftest ,test-name-threedigit ()
             ,(concat "Test `time-stamp' format " form-string
                      " with offsets of 100 hours or greater.\n"
                      common-description)
             (should (equal (formatz ,form-string (fz-make+zone 100))
                            ,(car big-mod)))
             (formatz-hours-big-helper ,form-string ',(cdr big-mod))
             (should (equal (formatz ,form-string (fz-make+zone 100 0 30))
                            ,(car secbig-mod)))
             (formatz-seconds-big-helper ,form-string ',(cdr secbig-mod)))
          ))))))

(defun formatz-find-test-def-function (test-name)
  "Search for the `define-formatz-tests' call defining test TEST-NAME.
Return non-nil if the definition is found."
  (let* ((z-format (replace-regexp-in-string "\\`formatz-\\([^z]+z\\)-.*\\'"
                                             "\\1"
                                             (symbol-name test-name)))
         (regexp (concat "^(define-formatz-tests ("
                         "\\(?:[^)]\\|;.*\n\\)*"
                         "\"" (regexp-quote z-format) "\"")))
    (re-search-forward regexp nil t)))

;;;; The actual test cases for %z

;;; Test %z formats without colons.

;; Option character "-" (minus) minimizes; it removes "00" minutes.
(define-formatz-tests ("%-z" "%-3z")
  ("+00")
  ("+0030" formatz-mod-del-colons)
  ("+000030" formatz-mod-del-colons)
  ("+100:00")
  ("+100:00:30"))

;; Minus with padding pads with spaces.
(define-formatz-tests ("%-12z")
  ("+00         " formatz-mod-pad-r12)
  ("+0030       " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+000030     " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;; 0 after other digits becomes padding of ten, not zero flag.
(define-formatz-tests ("%-10z")
  ("+00       " formatz-mod-pad-r10)
  ("+0030     " formatz-mod-del-colons formatz-mod-pad-r10)
  ("+000030   " formatz-mod-del-colons formatz-mod-pad-r10)
  ("+100:00   " formatz-mod-pad-r10)
  ("+100:00:30"))

;; Although time-stamp doesn't call us for %z, we do want to spot-check
;; it here, to verify the implementation we will eventually use.
;; The legacy exception for %z in time-stamp will need to remain
;; through at least 2024 and Emacs 28.
(ert-deftest formatz-%z-spotcheck ()
  "Spot-check internal implementation of `time-stamp' format %z."
  (should (equal (format-time-offset "%z" (fz-make+zone 0)) "+0000"))
  (should (equal (format-time-offset "%z" (fz-make+zone 0 30)) "+0030"))
  (should (equal (format-time-offset "%z" (fz-make+zone 0 0 30)) "+000030"))
  (should (equal (format-time-offset "%z" (fz-make+zone 100)) "+100:00"))
  (should (equal (format-time-offset "%z" (fz-make+zone 100 0 30)) "+100:00:30"))
  )

;; Basic %z outputs 4 digits.
;; Small padding values do not extend the result.
(define-formatz-tests (;; We don't check %z here because time-stamp
                         ;; has a legacy behavior for it.
                         ;;"%z"
                         "%5z" "%0z" "%05z")
  ("+0000" formatz-mod-add-00)
  ("+0030" formatz-mod-del-colons)
  ("+000030" formatz-mod-del-colons)
  ("+100:00")
  ("+100:00:30"))

;; Padding adds spaces.
(define-formatz-tests ("%12z")
  ("+0000       " formatz-mod-add-00 formatz-mod-pad-r12)
  ("+0030       " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+000030     " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;; Requiring 0-padding to 6 adds seconds (only) as needed.
(define-formatz-tests ("%06z")
  ("+000000" formatz-mod-add-00 formatz-mod-add-00)
  ("+003000" formatz-mod-del-colons formatz-mod-add-00)
  ("+000030" formatz-mod-del-colons)
  ("+100:00")
  ("+100:00:30"))

;; Option character "_" always adds seconds.
(define-formatz-tests ("%_z" "%_7z")
  ("+000000" formatz-mod-add-00 formatz-mod-add-00)
  ("+003000" formatz-mod-del-colons formatz-mod-add-00)
  ("+000030" formatz-mod-del-colons)
  ("+100:00:00" formatz-mod-add-colon00)
  ("+100:00:30"))

;; Enough 0-padding adds seconds, then adds spaces.
(define-formatz-tests ("%012z" "%_12z")
  ("+000000     " formatz-mod-add-00 formatz-mod-add-00 formatz-mod-pad-r12)
  ("+003000     " formatz-mod-del-colons formatz-mod-add-00 formatz-mod-pad-r12)
  ("+000030     " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+100:00:00  " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;;; Test %z formats with colons.

;; Three colons can output hours only,
;; like %-z, but uses colons with non-zero minutes and seconds.
(define-formatz-tests ("%:::z" "%0:::z"
                         "%3:::z" "%03:::z")
  ("+00")
  ("+00:30")
  ("+00:00:30")
  ("+100:00")
  ("+100:00:30"))

;; Padding with three colons adds spaces.
(define-formatz-tests ("%12:::z")
  ("+00         " formatz-mod-pad-r12)
  ("+00:30      " formatz-mod-pad-r12)
  ("+00:00:30   " formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;; 0 after other digits becomes padding of ten, not zero flag.
(define-formatz-tests ("%10:::z")
  ("+00       " formatz-mod-pad-r10)
  ("+00:30    " formatz-mod-pad-r10)
  ("+00:00:30 " formatz-mod-pad-r10)
  ("+100:00   " formatz-mod-pad-r10)
  ("+100:00:30"))

;; One colon outputs minutes, like %z but with colon.
(define-formatz-tests ("%:z" "%6:z" "%0:z" "%06:z" "%06:::z")
  ("+00:00" formatz-mod-add-colon00)
  ("+00:30")
  ("+00:00:30")
  ("+100:00")
  ("+100:00:30"))

;; Padding with one colon adds spaces.
(define-formatz-tests ("%12:z")
  ("+00:00      " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+00:30      " formatz-mod-pad-r12)
  ("+00:00:30   " formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;; Requiring 0-padding to 7 adds seconds (only) as needed.
(define-formatz-tests ("%07:z" "%07:::z")
  ("+00:00:00" formatz-mod-add-colon00 formatz-mod-add-colon00)
  ("+00:30:00" formatz-mod-add-colon00)
  ("+00:00:30")
  ("+100:00")
  ("+100:00:30"))

;; Two colons outputs HH:MM:SS, like %_z but with colons.
(define-formatz-tests ("%::z" "%9::z" "%0::z" "%09::z")
  ("+00:00:00" formatz-mod-add-colon00 formatz-mod-add-colon00)
  ("+00:30:00" formatz-mod-add-colon00)
  ("+00:00:30")
  ("+100:00:00" formatz-mod-add-colon00)
  ("+100:00:30"))

;; Enough padding adds minutes and seconds, then adds spaces.
(define-formatz-tests ("%012:z" "%012::z" "%12::z" "%012:::z")
  ("+00:00:00   " formatz-mod-add-colon00 formatz-mod-add-colon00
                  formatz-mod-pad-r12)
  ("+00:30:00   " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+00:00:30   " formatz-mod-pad-r12)
  ("+100:00:00  " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;;; Test illegal %z formats.

(ert-deftest formatz-illegal-options ()
  "Test that illegal/nonsensical/ambiguous %z formats don't produce output."
  ;; multiple options
  (should (equal "" (formatz "%_-z" 0)))
  (should (equal "" (formatz "%-_z" 0)))
  (should (equal "" (formatz "%_0z" 0)))
  (should (equal "" (formatz "%0_z" 0)))
  (should (equal "" (formatz "%0-z" 0)))
  (should (equal "" (formatz "%-0z" 0)))
  ;; inconsistent to both minimize and require mins or secs
  (should (equal "" (formatz "%-:z" 0)))
  (should (equal "" (formatz "%-::z" 0)))
  ;; consistent, but redundant
  (should (equal "" (formatz "%-:::z" 0)))
  (should (equal "" (formatz "%_::z" 0)))
  ;; inconsistent to both pre-expand and default to hours or mins
  (should (equal "" (formatz "%_:::z" 0)))
  (should (equal "" (formatz "%_:z" 0)))
  ;; options that don't make sense with %z
  (should (equal "" (formatz "%#z" 0)))
  )


;;; Repeat the indent properties declared by the macros above,
;;; so that we can indent code before we eval this buffer.
;; Local variables:
;; eval: (put 'with-time-stamp-test-env 'lisp-indent-function 0)
;; eval: (put 'with-time-stamp-test-time 'lisp-indent-function 1)
;; eval: (put 'with-time-stamp-system-name 'lisp-indent-function 1)
;; eval: (put 'define-formatz-tests 'lisp-indent-function 1)
;; End:

;;; time-stamp-tests.el ends here
