;;; time-stamp-tests.el --- tests for time-stamp.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

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
  "Evaluate BODY with some standard time-stamp test variables bound."
  (declare (indent 0) (debug t))
  `(let ((user-login-name "test-logname")
         (user-full-name "100%d Tester") ;verify "%" passed unchanged
         (buffer-file-name "/emacs/test/time-stamped-file")
         (mail-host-address "test-mail-host-name")
         (ref-time1 '(17337 16613))    ;Monday, Jan 2, 2006, 3:04:05 PM
         (ref-time2 '(22574 61591))    ;Friday, Nov 18, 2016, 12:14:15 PM
         (ref-time3 '(21377 34956))    ;Sunday, May 25, 2014, 06:07:08 AM
         (time-stamp-time-zone t))     ;use UTC
     (cl-letf (((symbol-function 'time-stamp-conv-warn)
                (lambda (old-format _new)
                  (ert-fail
                   (format "Unexpected format warning for '%s'" old-format)))))
       ;; Not all reference times are used in all tests;
       ;; suppress the byte compiler's "unused" warning.
       (list ref-time1 ref-time2 ref-time3)
       ,@body)))

(defmacro with-time-stamp-test-time (reference-time &rest body)
  "Force any contained time-stamp call to use time REFERENCE-TIME."
  (declare (indent 1) (debug t))
  `(cl-letf*
         ((orig-time-stamp-string-fn (symbol-function 'time-stamp-string))
         ((symbol-function 'time-stamp-string)
          (lambda (ts-format)
            (apply orig-time-stamp-string-fn ts-format ,reference-time nil))))
     ,@body))

(defmacro with-time-stamp-system-name (name &rest body)
  "Force (system-name) to return NAME while evaluating BODY."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'system-name)
              (lambda () ,name)))
     ,@body))

(defmacro time-stamp-should-warn (form)
  "Similar to `should' but verifies that a format warning is generated."
  (declare (debug t))
  `(let ((warning-count 0))
     (cl-letf (((symbol-function 'time-stamp-conv-warn)
                (lambda (_old _new)
                  (setq warning-count (1+ warning-count)))))
       (should ,form)
       (if (not (= warning-count 1))
           (ert-fail (format "Should have warned about format: %S" ',form))))))

;;; Tests:

;;; Tests of customization variables

(ert-deftest time-stamp-custom-time-zone ()
  "Test that setting time-stamp-time-zone affects the format."
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
         '(("4/" "10/" "-9/" "0/" "")                     ;0: line limit
           ("stamp<" "")                                  ;1: start
           ("%-d" "%_H" "%^a" "%#Z" "%:A" "%09z" "%%" "") ;2: format part 1
           (" " "x" ":" "\n" "")                          ;3: format part 2
           ("%-d" "%_H" "%^a" "%#Z" "%:A" "%09z" "%%")    ;4: format part 3
           (">end" ""))))                                 ;5: end
    (dotimes (cur (length pattern-value-parts))
      (dotimes (cur-index (length (nth cur pattern-value-parts)))
        (cl-flet ((extract-part
                   (lambda (desired-part)
                     (let ((part-list (nth desired-part pattern-value-parts)))
                       (if (= desired-part cur)
                           (nth cur-index part-list)
                         (nth 0 part-list))))))
          ;; Don't repeat the default pattern.
          (if (or (= cur 0) (> cur-index 0))
              ;; The whole format must start with %, so not all
              ;; generated combinations are valid
              (if (or (not (equal (extract-part 2) ""))
                      (equal (extract-part 3) ""))
                  (iter-yield (list (extract-part 0)
                                    (extract-part 1)
                                    (apply #'concat
                                           (mapcar #'extract-part '(2 3 4)))
                                    (extract-part 5))))))))))

(iter-defun time-stamp-test-pattern-multiply ()
  "Iterate through every combination of parts of `time-stamp-pattern'."
  (let ((line-limit-values '("" "4/"))
        (start-values '("" "/stamp/"))
        (format-values '("%%" "%m"))
        (end-values '("" ">end")))
    ;; yield all combinations of the above
    (dolist (line-limit line-limit-values)
      (dolist (start start-values)
        (dolist (format format-values)
          (dolist (end end-values)
            (iter-yield (list line-limit start format end))))))))

(iter-defun time-stamp-test-pattern-all ()
  (iter-yield-from (time-stamp-test-pattern-sequential))
  (iter-yield-from (time-stamp-test-pattern-multiply)))

(ert-deftest time-stamp-custom-pattern ()
  "Test that time-stamp-pattern is parsed correctly."
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
              (if (equal whole-format "%%")
                  (should (equal ts-format time-stamp-format))
                (should (equal ts-format whole-format)))
              (if (equal end1 "")
                  (should (equal ts-end time-stamp-end))
                (should (equal ts-end end1)))
              ;; return nil to stop time-stamp from calling us again
              nil)))
        (let ((time-stamp-pattern (concat
                                   line-limit1 start1 whole-format end1)))
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
  "Test that time-stamp inserts lines or not, as directed."
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
  "Test that time-stamp finds the end pattern on the correct line."
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
  "Test that time-stamp updates no more than time-stamp-count templates."
  (with-time-stamp-test-env
    (let ((time-stamp-start "TS: <")
          (time-stamp-format "%Y-%m-%d")
          (time-stamp-count 0)          ;changed later in the test
          (buffer-expected-once "TS: <2006-01-02>\nTS: <>")
          (buffer-expected-twice "TS: <2006-01-02>\nTS: <2006-01-02>"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
          (insert "TS: <>\nTS: <>")
          (time-stamp)
          ;; even with count = 0, expect one time stamp
          (should (equal (buffer-string) buffer-expected-once)))
        (with-temp-buffer
          (setq time-stamp-count 1)
          (insert "TS: <>\nTS: <>")
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-once))

          (setq time-stamp-count 2)
          (time-stamp)
          (should (equal (buffer-string) buffer-expected-twice)))))))

;;; Tests of time-stamp-string formatting

(ert-deftest time-stamp-format-day-of-week ()
  "Test time-stamp formats for named day of week."
  (with-time-stamp-test-env
   (let ((Mon (format-time-string "%a" ref-time1 t))
         (MON (format-time-string "%^a" ref-time1 t))
         (Monday (format-time-string "%A" ref-time1 t))
         (MONDAY (format-time-string "%^A" ref-time1 t)))
     ;; implemented and documented since 1997
     (should (equal (time-stamp-string "%3a" ref-time1) Mon))
     (should (equal (time-stamp-string "%#A" ref-time1) MONDAY))
     ;; documented 1997-2019
     (should (equal (time-stamp-string "%3A" ref-time1)
                    (substring MONDAY 0 3)))
     (should (equal (time-stamp-string "%:a" ref-time1) Monday))
     ;; implemented since 2001, documented since 2019
     (should (equal (time-stamp-string "%#a" ref-time1) MON))
     (should (equal (time-stamp-string "%:A" ref-time1) Monday))
     ;; allowed but undocumented since 2019 (warned 1997-2019)
     (should (equal (time-stamp-string "%^A" ref-time1) MONDAY))
     ;; warned 1997-2019, changed in 2019
     (should (equal (time-stamp-string "%a" ref-time1) Mon))
     (should (equal (time-stamp-string "%^a" ref-time1) MON))
     (should (equal (time-stamp-string "%A" ref-time1) Monday)))))

(ert-deftest time-stamp-format-month-name ()
  "Test time-stamp formats for month name."
  (with-time-stamp-test-env
   (let ((Jan (format-time-string "%b" ref-time1 t))
         (JAN (format-time-string "%^b" ref-time1 t))
         (January (format-time-string "%B" ref-time1 t))
         (JANUARY (format-time-string "%^B" ref-time1 t)))
     ;; implemented and documented since 1997
     (should (equal (time-stamp-string "%3b" ref-time1)
                    (substring January 0 3)))
     (should (equal (time-stamp-string "%#B" ref-time1) JANUARY))
     ;; documented 1997-2019
     (should (equal (time-stamp-string "%3B" ref-time1)
                    (substring JANUARY 0 3)))
     (should (equal (time-stamp-string "%:b" ref-time1) January))
     ;; implemented since 2001, documented since 2019
     (should (equal (time-stamp-string "%#b" ref-time1) JAN))
     (should (equal (time-stamp-string "%:B" ref-time1) January))
     ;; allowed but undocumented since 2019 (warned 1997-2019)
     (should (equal (time-stamp-string "%^B" ref-time1) JANUARY))
     ;; warned 1997-2019, changed in 2019
     (should (equal (time-stamp-string "%b" ref-time1) Jan))
     (should (equal (time-stamp-string "%^b" ref-time1) JAN))
     (should (equal (time-stamp-string "%B" ref-time1) January)))))

(ert-deftest time-stamp-format-day-of-month ()
  "Test time-stamp formats for day of month."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%2d" ref-time1) " 2"))
    (should (equal (time-stamp-string "%2d" ref-time2) "18"))
    (should (equal (time-stamp-string "%02d" ref-time1) "02"))
    (should (equal (time-stamp-string "%02d" ref-time2) "18"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:d" ref-time1) "2"))
    (should (equal (time-stamp-string "%:d" ref-time2) "18"))
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%1d" ref-time1) "2"))
    (should (equal (time-stamp-string "%1d" ref-time2) "18"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%-d" ref-time1) "2"))
    (should (equal (time-stamp-string "%-d" ref-time2) "18"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%_d" ref-time1) " 2"))
    (should (equal (time-stamp-string "%_d" ref-time2) "18"))
    (should (equal (time-stamp-string "%d" ref-time1) "02"))
    (should (equal (time-stamp-string "%d" ref-time2) "18"))))

(ert-deftest time-stamp-format-hours-24 ()
  "Test time-stamp formats for hour on a 24-hour clock."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%2H" ref-time1) "15"))
    (should (equal (time-stamp-string "%2H" ref-time2) "12"))
    (should (equal (time-stamp-string "%2H" ref-time3) " 6"))
    (should (equal (time-stamp-string "%02H" ref-time1) "15"))
    (should (equal (time-stamp-string "%02H" ref-time2) "12"))
    (should (equal (time-stamp-string "%02H" ref-time3) "06"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:H" ref-time1) "15"))
    (should (equal (time-stamp-string "%:H" ref-time2) "12"))
    (should (equal (time-stamp-string "%:H" ref-time3) "6"))
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%1H" ref-time1) "15"))
    (should (equal (time-stamp-string "%1H" ref-time2) "12"))
    (should (equal (time-stamp-string "%1H" ref-time3) "6"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%-H" ref-time1) "15"))
    (should (equal (time-stamp-string "%-H" ref-time2) "12"))
    (should (equal (time-stamp-string "%-H" ref-time3) "6"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%_H" ref-time1) "15"))
    (should (equal (time-stamp-string "%_H" ref-time2) "12"))
    (should (equal (time-stamp-string "%_H" ref-time3) " 6"))
    (should (equal (time-stamp-string "%H" ref-time1) "15"))
    (should (equal (time-stamp-string "%H" ref-time2) "12"))
    (should (equal (time-stamp-string "%H" ref-time3) "06"))))

(ert-deftest time-stamp-format-hours-12 ()
  "Test time-stamp formats for hour on a 12-hour clock."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%2I" ref-time1) " 3"))
    (should (equal (time-stamp-string "%2I" ref-time2) "12"))
    (should (equal (time-stamp-string "%2I" ref-time3) " 6"))
    (should (equal (time-stamp-string "%02I" ref-time1) "03"))
    (should (equal (time-stamp-string "%02I" ref-time2) "12"))
    (should (equal (time-stamp-string "%02I" ref-time3) "06"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:I" ref-time1) "3")) ;PM
    (should (equal (time-stamp-string "%:I" ref-time2) "12")) ;PM
    (should (equal (time-stamp-string "%:I" ref-time3) "6")) ;AM
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%1I" ref-time1) "3"))
    (should (equal (time-stamp-string "%1I" ref-time2) "12"))
    (should (equal (time-stamp-string "%1I" ref-time3) "6"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%-I" ref-time1) "3"))
    (should (equal (time-stamp-string "%-I" ref-time2) "12"))
    (should (equal (time-stamp-string "%-I" ref-time3) "6"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%_I" ref-time1) " 3"))
    (should (equal (time-stamp-string "%_I" ref-time2) "12"))
    (should (equal (time-stamp-string "%_I" ref-time3) " 6"))
    (should (equal (time-stamp-string "%I" ref-time1) "03"))
    (should (equal (time-stamp-string "%I" ref-time2) "12"))
    (should (equal (time-stamp-string "%I" ref-time3) "06"))))

(ert-deftest time-stamp-format-month-number ()
  "Test time-stamp formats for month number."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%2m" ref-time1) " 1"))
    (should (equal (time-stamp-string "%2m" ref-time2) "11"))
    (should (equal (time-stamp-string "%02m" ref-time1) "01"))
    (should (equal (time-stamp-string "%02m" ref-time2) "11"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:m" ref-time1) "1"))
    (should (equal (time-stamp-string "%:m" ref-time2) "11"))
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%1m" ref-time1) "1"))
    (should (equal (time-stamp-string "%1m" ref-time2) "11"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%-m" ref-time1) "1"))
    (should (equal (time-stamp-string "%-m" ref-time2) "11"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%_m" ref-time1) " 1"))
    (should (equal (time-stamp-string "%_m" ref-time2) "11"))
    (should (equal (time-stamp-string "%m" ref-time1) "01"))
    (should (equal (time-stamp-string "%m" ref-time2) "11"))))

(ert-deftest time-stamp-format-minute ()
  "Test time-stamp formats for minute."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%2M" ref-time1) " 4"))
    (should (equal (time-stamp-string "%2M" ref-time2) "14"))
    (should (equal (time-stamp-string "%02M" ref-time1) "04"))
    (should (equal (time-stamp-string "%02M" ref-time2) "14"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:M" ref-time1) "4"))
    (should (equal (time-stamp-string "%:M" ref-time2) "14"))
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%1M" ref-time1) "4"))
    (should (equal (time-stamp-string "%1M" ref-time2) "14"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%-M" ref-time1) "4"))
    (should (equal (time-stamp-string "%-M" ref-time2) "14"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%_M" ref-time1) " 4"))
    (should (equal (time-stamp-string "%_M" ref-time2) "14"))
    (should (equal (time-stamp-string "%M" ref-time1) "04"))
    (should (equal (time-stamp-string "%M" ref-time2) "14"))))

(ert-deftest time-stamp-format-second ()
  "Test time-stamp formats for second."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%2S" ref-time1) " 5"))
    (should (equal (time-stamp-string "%2S" ref-time2) "15"))
    (should (equal (time-stamp-string "%02S" ref-time1) "05"))
    (should (equal (time-stamp-string "%02S" ref-time2) "15"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:S" ref-time1) "5"))
    (should (equal (time-stamp-string "%:S" ref-time2) "15"))
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%1S" ref-time1) "5"))
    (should (equal (time-stamp-string "%1S" ref-time2) "15"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%-S" ref-time1) "5"))
    (should (equal (time-stamp-string "%-S" ref-time2) "15"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%_S" ref-time1) " 5"))
    (should (equal (time-stamp-string "%_S" ref-time2) "15"))
    (should (equal (time-stamp-string "%S" ref-time1) "05"))
    (should (equal (time-stamp-string "%S" ref-time2) "15"))))

(ert-deftest time-stamp-format-year-2digit ()
  "Test time-stamp formats for %y."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%02y" ref-time1) "06"))
    (should (equal (time-stamp-string "%02y" ref-time2) "16"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%:y" ref-time1) "2006"))
    (should (equal (time-stamp-string "%:y" ref-time2) "2016"))
    ;; warned 1997-2019, changed in 2019
    ;; (We don't expect the %-y or %_y form to be useful,
    ;; but we test both so that we can confidently state that
    ;; `-' and `_' affect all 2-digit conversions identically.)
    (should (equal (time-stamp-string "%-y" ref-time1) "6"))
    (should (equal (time-stamp-string "%-y" ref-time2) "16"))
    (should (equal (time-stamp-string "%_y" ref-time1) " 6"))
    (should (equal (time-stamp-string "%_y" ref-time2) "16"))
    (should (equal (time-stamp-string "%y" ref-time1) "06"))
    (should (equal (time-stamp-string "%y" ref-time2) "16"))
    ;; implemented since 1995, warned since 2019, will change
    (time-stamp-should-warn
     (equal (time-stamp-string "%04y" ref-time1) "2006"))
    (time-stamp-should-warn
     (equal (time-stamp-string "%4y" ref-time1) "2006"))))

(ert-deftest time-stamp-format-year-4digit ()
  "Test time-stamp format %Y."
  (with-time-stamp-test-env
    ;; implemented since 1997, documented since 2019
    (should (equal (time-stamp-string "%Y" ref-time1) "2006"))
    ;; numbers do not truncate
    (should (equal (time-stamp-string "%2Y" ref-time1) "2006"))
    (should (equal (time-stamp-string "%02Y" ref-time1) "2006"))))

(ert-deftest time-stamp-format-am-pm ()
  "Test time-stamp formats for AM and PM strings."
  (with-time-stamp-test-env
    (let ((pm (format-time-string "%#p" ref-time1 t))
          (am (format-time-string "%#p" ref-time3 t))
          (PM (format-time-string "%p" ref-time1 t))
          (AM (format-time-string "%p" ref-time3 t)))
      ;; implemented and documented since 1997
      (should (equal (time-stamp-string "%#p" ref-time1) pm))
      (should (equal (time-stamp-string "%#p" ref-time3) am))
      (should (equal (time-stamp-string "%P" ref-time1) PM))
      (should (equal (time-stamp-string "%P" ref-time3) AM))
      ;; warned 1997-2019, changed in 2019
      (should (equal (time-stamp-string "%p" ref-time1) PM))
      (should (equal (time-stamp-string "%p" ref-time3) AM)))))

(ert-deftest time-stamp-format-day-number-in-week ()
  "Test time-stamp formats for day number in week."
  (with-time-stamp-test-env
    (should (equal (time-stamp-string "%w" ref-time1) "1"))
    (should (equal (time-stamp-string "%w" ref-time2) "5"))
    (should (equal (time-stamp-string "%w" ref-time3) "0"))))

(ert-deftest time-stamp-format-time-zone-name ()
  "Test time-stamp format %Z."
  (with-time-stamp-test-env
    (let ((UTC-abbr (format-time-string "%Z" ref-time1 t))
	  (utc-abbr (format-time-string "%#Z" ref-time1 t)))
      ;; implemented and documented since 1995
      (should (equal (time-stamp-string "%Z" ref-time1) UTC-abbr))
      ;; implemented since 1997, documented since 2019
      (should (equal (time-stamp-string "%#Z" ref-time1) utc-abbr)))))

(ert-deftest time-stamp-format-time-zone-offset ()
  "Tests time-stamp legacy format %z and spot tests of new offset format %5z."
  (with-time-stamp-test-env
    (let ((utc-abbr (format-time-string "%#Z" ref-time1 t)))
    ;; documented 1995-2019, warned since 2019, will change
      (time-stamp-should-warn
       (equal (time-stamp-string "%z" ref-time1) utc-abbr)))
    ;; implemented and documented (with compat caveat) since 2019
    (should (equal (time-stamp-string "%5z" ref-time1) "+0000"))
    (let ((time-stamp-time-zone "PST8"))
      (should (equal (time-stamp-string "%5z" ref-time1) "-0800")))
    (let ((time-stamp-time-zone "HST10"))
      (should (equal (time-stamp-string "%5z" ref-time1) "-1000")))
    (let ((time-stamp-time-zone "CET-1"))
      (should (equal (time-stamp-string "%5z" ref-time1) "+0100")))
    ;; implemented since 2019, verify that these don't warn
    ;; See also the "formatz" tests below, which since 2021 test more
    ;; variants with more offsets.
    (should (equal (time-stamp-string "%-z" ref-time1) "+00"))
    (should (equal (time-stamp-string "%:z" ref-time1) "+00:00"))
    (should (equal (time-stamp-string "%::z" ref-time1) "+00:00:00"))
    (should (equal (time-stamp-string "%9::z" ref-time1) "+00:00:00"))
    (should (equal (time-stamp-string "%:::z" ref-time1) "+00"))))

(ert-deftest time-stamp-format-non-date-conversions ()
  "Test time-stamp formats for non-date items."
  (with-time-stamp-test-env
    (with-time-stamp-system-name "test-system-name.example.org"
      ;; implemented and documented since 1995
      (should (equal (time-stamp-string "%%" ref-time1) "%")) ;% last char
      (should (equal (time-stamp-string "%%P" ref-time1) "%P")) ;% not last char
      (should (equal (time-stamp-string "%f" ref-time1) "time-stamped-file"))
      (should (equal (time-stamp-string "%F" ref-time1)
                     "/emacs/test/time-stamped-file"))
      (with-temp-buffer
        (should (equal (time-stamp-string "%f" ref-time1) "(no file)"))
        (should (equal (time-stamp-string "%F" ref-time1) "(no file)")))
      (should (equal (time-stamp-string "%h" ref-time1) "test-mail-host-name"))
      (let ((mail-host-address nil))
        (should (equal (time-stamp-string "%h" ref-time1)
                       "test-system-name.example.org")))
      ;; documented 1995-2019
      (should (equal (time-stamp-string "%s" ref-time1)
                     "test-system-name.example.org"))
      (should (equal (time-stamp-string "%U" ref-time1) "100%d Tester"))
      (should (equal (time-stamp-string "%u" ref-time1) "test-logname"))
      ;; implemented since 2001, documented since 2019
      (should (equal (time-stamp-string "%L" ref-time1) "100%d Tester"))
      (should (equal (time-stamp-string "%l" ref-time1) "test-logname"))
      ;; implemented since 2007, documented since 2019
      (should (equal (time-stamp-string "%Q" ref-time1)
                     "test-system-name.example.org"))
      (should (equal (time-stamp-string "%q" ref-time1) "test-system-name")))
    (with-time-stamp-system-name "sysname-no-dots"
      (should (equal (time-stamp-string "%Q" ref-time1) "sysname-no-dots"))
      (should (equal (time-stamp-string "%q" ref-time1) "sysname-no-dots")))))

(ert-deftest time-stamp-format-ignored-modifiers ()
  "Test additional args allowed (but ignored) to allow for future expansion."
  (with-time-stamp-test-env
   (let ((May (format-time-string "%B" ref-time3 t)))
     ;; allowed modifiers
     (should (equal (time-stamp-string "%.,@+ (stuff)B" ref-time3) May))
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
  "Tests that multiple %-conversions are independent."
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
  "Test time-stamp string width modifiers."
  (with-time-stamp-test-env
   (let ((May (format-time-string "%b" ref-time3 t))
         (SUN (format-time-string "%^a" ref-time3 t))
         (NOV (format-time-string "%^b" ref-time2 t)))
     ;; strings truncate on the right or are blank-padded on the left
     (should (equal (time-stamp-string "%0b" ref-time3) ""))
     (should (equal (time-stamp-string "%1b" ref-time3) (substring May 0 1)))
     (should (equal (time-stamp-string "%2b" ref-time3) (substring May 0 2)))
     (should (equal (time-stamp-string "%3b" ref-time3) (substring May 0 3)))
     (should (equal (time-stamp-string "%4b" ref-time3) (concat " " May)))
     (should (equal (time-stamp-string "%0%" ref-time3) ""))
     (should (equal (time-stamp-string "%1%" ref-time3) "%"))
     (should (equal (time-stamp-string "%2%" ref-time3) " %"))
     (should (equal (time-stamp-string "%9%" ref-time3) "        %"))
     (should (equal (time-stamp-string "%10%" ref-time3) "         %"))
     (should (equal (time-stamp-string "%#3a" ref-time3)
                    (substring SUN 0 3)))
     (should (equal (time-stamp-string "%#3b" ref-time2)
                    (substring NOV 0 3))))))

;;; Tests of helper functions

(ert-deftest time-stamp-helper-string-defaults ()
  "Test that time-stamp-string defaults its format to time-stamp-format."
  (with-time-stamp-test-env
    (should (equal (time-stamp-string nil ref-time1)
                   (time-stamp-string time-stamp-format ref-time1)))
    (should (equal (time-stamp-string 'not-a-string ref-time1) nil))))

(ert-deftest time-stamp-helper-zone-type-p ()
  "Test time-stamp-zone-type-p."
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
  (should (safe-local-variable-p 'time-stamp-line-limit 8))
  (should-not (safe-local-variable-p 'time-stamp-line-limit "a string"))
  (should (safe-local-variable-p 'time-stamp-start "a string"))
  (should-not (safe-local-variable-p 'time-stamp-start 17))
  (should (safe-local-variable-p 'time-stamp-end "a string"))
  (should-not (safe-local-variable-p 'time-stamp-end 17))
  (should (safe-local-variable-p 'time-stamp-inserts-lines t))
  (should-not (safe-local-variable-p 'time-stamp-inserts-lines 17))
  (should (safe-local-variable-p 'time-stamp-count 2))
  (should-not (safe-local-variable-p 'time-stamp-count t))
  (should (safe-local-variable-p 'time-stamp-pattern "a string"))
  (should-not (safe-local-variable-p 'time-stamp-pattern 17)))

;;;; Setup for tests of time offset formatting with %z

(defun formatz (format zone)
  "Uses FORMAT to format the offset of ZONE, returning the result.
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
  "Uses FORMAT to format the time zone represented by OFFSET-SECS.
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
  "Creates a non-negative offset."
  (let ((m (or m 0))
        (s (or s 0)))
    (+ (* 3600 h) (* 60 m) s)))

(defun fz-make-zone (h &optional m s)
  "Creates a negative offset.  The arguments are all non-negative."
  (- (fz-make+zone h m s)))

(defmacro formatz-should-equal (zone expect)
  "Formats ZONE and compares it to EXPECT.
Uses the free variables `form-string' and `pattern-mod'.
The functions in `pattern-mod' are composed left to right."
  (declare (debug t))
  `(let ((result ,expect))
     (dolist (fn pattern-mod)
       (setq result (funcall fn result)))
     (should (equal (formatz form-string ,zone) result))))

;; These test cases have zeros in all places (first, last, none, both)
;; for hours, minutes, and seconds.

(defun formatz-hours-exact-helper (form-string pattern-mod)
  "Tests format %z with whole hours."
  (formatz-should-equal (fz-make+zone 0) "+00") ;0 sign always +, both digits
  (formatz-should-equal (fz-make+zone 10) "+10")
  (formatz-should-equal (fz-make-zone 10) "-10")
  (formatz-should-equal (fz-make+zone 2) "+02")
  (formatz-should-equal (fz-make-zone 2) "-02")
  (formatz-should-equal (fz-make+zone 13) "+13")
  (formatz-should-equal (fz-make-zone 13) "-13")
  )

(defun formatz-nonzero-minutes-helper (form-string pattern-mod)
  "Tests format %z with whole minutes."
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
  "Tests format %z with non-0 seconds."
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
  "Tests format %z with hours that don't fit in two digits."
  (formatz-should-equal (fz-make+zone 101) "+101:00")
  (formatz-should-equal (fz-make+zone 123 10) "+123:10")
  (formatz-should-equal (fz-make-zone 123 10) "-123:10")
  (formatz-should-equal (fz-make+zone 123 2) "+123:02")
  (formatz-should-equal (fz-make-zone 123 2) "-123:02")
  )

(defun formatz-seconds-big-helper (form-string pattern-mod)
  "Tests format %z with hours greater than 99 and non-zero seconds."
  (formatz-should-equal (fz-make+zone 123 0 30) "+123:00:30")
  (formatz-should-equal (fz-make-zone 123 0 30) "-123:00:30")
  (formatz-should-equal (fz-make+zone 120 0 4) "+120:00:04")
  (formatz-should-equal (fz-make-zone 120 0 4) "-120:00:04")
  )

;; Functions that modify the expected output string, so that we can
;; use the above test cases for multiple formats.

(defun formatz-mod-del-colons (string)
  "Returns STRING with any colons removed."
  (string-replace ":" "" string))

(defun formatz-mod-add-00 (string)
  "Returns STRING with \"00\" appended."
  (concat string "00"))

(defun formatz-mod-add-colon00 (string)
  "Returns STRING with \":00\" appended."
  (concat string ":00"))

(defun formatz-mod-pad-r10 (string)
  "Returns STRING padded on the right to 10 characters."
  (concat string (make-string (- 10 (length string)) ?\s)))

(defun formatz-mod-pad-r12 (string)
  "Returns STRING padded on the right to 12 characters."
  (concat string (make-string (- 12 (length string)) ?\s)))

;; Convenience macro for generating groups of test cases.

(defmacro formatz-generate-tests
    (form-strings hour-mod mins-mod secs-mod big-mod secbig-mod)
  "Defines tests for time formats FORM-STRINGS.
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
  (let ((ert-test-list (list 'list)))
    (dolist (form-string form-strings ert-test-list)
      (nconc
       ert-test-list
       (list
        `(ert-deftest ,(intern (concat "formatz-" form-string "-hhmm")) ()
           ,(concat "Tests time-stamp format " form-string
                   " with whole hours or minutes.")
           (should (equal (formatz ,form-string (fz-make+zone 0))
                          ,(car hour-mod)))
           (formatz-hours-exact-helper ,form-string ',(cdr hour-mod))
           (should (equal (formatz ,form-string (fz-make+zone 0 30))
                          ,(car mins-mod)))
           (formatz-nonzero-minutes-helper ,form-string ',(cdr mins-mod)))
        `(ert-deftest ,(intern (concat "formatz-" form-string "-seconds")) ()
           ,(concat "Tests time-stamp format " form-string
                   " with offsets that have non-zero seconds.")
           (should (equal (formatz ,form-string (fz-make+zone 0 0 30))
                          ,(car secs-mod)))
           (formatz-nonzero-seconds-helper ,form-string ',(cdr secs-mod)))
        `(ert-deftest ,(intern (concat "formatz-" form-string "-threedigit")) ()
           ,(concat "Tests time-stamp format " form-string
                   " with offsets that are 100 hours or greater.")
           (should (equal (formatz ,form-string (fz-make+zone 100))
                          ,(car big-mod)))
           (formatz-hours-big-helper ,form-string ',(cdr big-mod))
           (should (equal (formatz ,form-string (fz-make+zone 100 0 30))
                          ,(car secbig-mod)))
           (formatz-seconds-big-helper ,form-string ',(cdr secbig-mod)))
        )))))

;;;; The actual test cases for %z

;;; %z formats without colons.

;; Option character "-" (minus) minimizes; it removes "00" minutes.
(formatz-generate-tests ("%-z" "%-3z")
  ("+00")
  ("+0030" formatz-mod-del-colons)
  ("+000030" formatz-mod-del-colons)
  ("+100:00")
  ("+100:00:30"))
;; Tests that minus with padding pads with spaces.
(formatz-generate-tests ("%-12z")
  ("+00         " formatz-mod-pad-r12)
  ("+0030       " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+000030     " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))
;; Tests that 0 after other digits becomes padding of ten, not zero flag.
(formatz-generate-tests ("%-10z")
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
  "Spot-checks internal implementation of time-stamp format %z."
  (should (equal (format-time-offset "%z" (fz-make+zone 0)) "+0000"))
  (should (equal (format-time-offset "%z" (fz-make+zone 0 30)) "+0030"))
  (should (equal (format-time-offset "%z" (fz-make+zone 0 0 30)) "+000030"))
  (should (equal (format-time-offset "%z" (fz-make+zone 100)) "+100:00"))
  (should (equal (format-time-offset "%z" (fz-make+zone 100 0 30)) "+100:00:30"))
  )

;; Basic %z outputs 4 digits.
;; Small padding values do not extend the result.
(formatz-generate-tests (;; We don't check %z here because time-stamp
                         ;; has a legacy behavior for it.
                         ;;"%z"
                         "%5z" "%0z" "%05z")
  ("+0000" formatz-mod-add-00)
  ("+0030" formatz-mod-del-colons)
  ("+000030" formatz-mod-del-colons)
  ("+100:00")
  ("+100:00:30"))

;; Tests that padding adds spaces.
(formatz-generate-tests ("%12z")
  ("+0000       " formatz-mod-add-00 formatz-mod-pad-r12)
  ("+0030       " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+000030     " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;; Requiring 0-padding to 6 adds seconds (only) as needed.
(formatz-generate-tests ("%06z")
  ("+000000" formatz-mod-add-00 formatz-mod-add-00)
  ("+003000" formatz-mod-del-colons formatz-mod-add-00)
  ("+000030" formatz-mod-del-colons)
  ("+100:00")
  ("+100:00:30"))

;; Option character "_" always adds seconds.
(formatz-generate-tests ("%_z" "%_7z")
  ("+000000" formatz-mod-add-00 formatz-mod-add-00)
  ("+003000" formatz-mod-del-colons formatz-mod-add-00)
  ("+000030" formatz-mod-del-colons)
  ("+100:00:00" formatz-mod-add-colon00)
  ("+100:00:30"))

;; Enough 0-padding adds seconds, then adds spaces.
(formatz-generate-tests ("%012z" "%_12z")
  ("+000000     " formatz-mod-add-00 formatz-mod-add-00 formatz-mod-pad-r12)
  ("+003000     " formatz-mod-del-colons formatz-mod-add-00 formatz-mod-pad-r12)
  ("+000030     " formatz-mod-del-colons formatz-mod-pad-r12)
  ("+100:00:00  " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;;; %z formats with colons

;; Three colons can output hours only,
;; like %-z, but uses colons with non-zero minutes and seconds.
(formatz-generate-tests ("%:::z" "%0:::z"
                         "%3:::z" "%03:::z")
  ("+00")
  ("+00:30")
  ("+00:00:30")
  ("+100:00")
  ("+100:00:30"))

;; Padding with three colons adds spaces
(formatz-generate-tests ("%12:::z")
  ("+00         " formatz-mod-pad-r12)
  ("+00:30      " formatz-mod-pad-r12)
  ("+00:00:30   " formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))
;; Tests that 0 after other digits becomes padding of ten, not zero flag.
(formatz-generate-tests ("%10:::z")
  ("+00       " formatz-mod-pad-r10)
  ("+00:30    " formatz-mod-pad-r10)
  ("+00:00:30 " formatz-mod-pad-r10)
  ("+100:00   " formatz-mod-pad-r10)
  ("+100:00:30"))

;; One colon outputs minutes, like %z but with colon.
(formatz-generate-tests ("%:z" "%6:z" "%0:z" "%06:z" "%06:::z")
  ("+00:00" formatz-mod-add-colon00)
  ("+00:30")
  ("+00:00:30")
  ("+100:00")
  ("+100:00:30"))

;; Padding with one colon adds spaces
(formatz-generate-tests ("%12:z")
  ("+00:00      " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+00:30      " formatz-mod-pad-r12)
  ("+00:00:30   " formatz-mod-pad-r12)
  ("+100:00     " formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;; Requiring 0-padding to 7 adds seconds (only) as needed.
(formatz-generate-tests ("%07:z" "%07:::z")
  ("+00:00:00" formatz-mod-add-colon00 formatz-mod-add-colon00)
  ("+00:30:00" formatz-mod-add-colon00)
  ("+00:00:30")
  ("+100:00")
  ("+100:00:30"))

;; Two colons outputs HH:MM:SS, like %_z but with colons.
(formatz-generate-tests ("%::z" "%9::z" "%0::z" "%09::z")
  ("+00:00:00" formatz-mod-add-colon00 formatz-mod-add-colon00)
  ("+00:30:00" formatz-mod-add-colon00)
  ("+00:00:30")
  ("+100:00:00" formatz-mod-add-colon00)
  ("+100:00:30"))

;; Enough padding adds minutes and seconds, then adds spaces.
(formatz-generate-tests ("%012:z" "%012::z" "%12::z" "%012:::z")
  ("+00:00:00   " formatz-mod-add-colon00 formatz-mod-add-colon00
                  formatz-mod-pad-r12)
  ("+00:30:00   " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+00:00:30   " formatz-mod-pad-r12)
  ("+100:00:00  " formatz-mod-add-colon00 formatz-mod-pad-r12)
  ("+100:00:30  " formatz-mod-pad-r12))

;;; Illegal %z formats

(ert-deftest formatz-illegal-options ()
  "Tests that illegal/nonsensical/ambiguous %z formats don't produce output."
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

;;; time-stamp-tests.el ends here
