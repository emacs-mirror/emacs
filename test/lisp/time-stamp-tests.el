;;; time-stamp-tests.el --- tests for time-stamp.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

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
  (declare (indent defun))
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
                   (format "Unexpected format warning for '%s'" old-format))))
               ((symbol-function 'system-name)
                (lambda () "test-system-name.example.org")))
       ;; Not all reference times are used in all tests;
       ;; suppress the byte compiler's "unused" warning.
       (list ref-time1 ref-time2 ref-time3)
       ,@body)))

(defmacro with-time-stamp-test-time (reference-time &rest body)
  "Force any contained time-stamp call to use time REFERENCE-TIME."
  (declare (indent defun))
  `(cl-letf*
         ((orig-time-stamp-string-fn (symbol-function 'time-stamp-string))
         ((symbol-function 'time-stamp-string)
          (lambda (ts-format)
            (apply orig-time-stamp-string-fn ts-format ,reference-time nil))))
     ,@body))

(defmacro time-stamp-should-warn (form)
  "Similar to `should' but verifies that a format warning is generated."
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
  "Iterate through each possibility for a part of time-stamp-pattern."
  (let ((pattern-value-parts
         '(("4/" "10/" "-4/" "0/" "")                     ;0: line limit
           ("stamp<" "")                                  ;1: start
           ("%-d" "%_H" "%^a" "%#Z" "%:A" "%02H" "%%" "") ;2: format part 1
           (" " "x" ":" "\n" "")                          ;3: format part 2
           ("%-d" "%_H" "%^a" "%#Z" "%:A" "%02H" "%%")    ;4: format part 3
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
  "Iterate through every combination of parts of time-stamp-pattern."
  (let ((line-limit-values '("" "4/"))
        (start-values '("" "stamp<"))
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
              (let ((limit-number (string-to-number line-limit1)))
                (if (equal line-limit1 "")
                    (setq limit-number time-stamp-line-limit))
                (goto-char (point-min))
                (if (> limit-number 0)
                    (should (= search-limit (line-beginning-position
                                             (1+ limit-number))))
                  (should (= search-limit (point-max))))
                (goto-char (point-max))
                (if (< limit-number 0)
                    (should (= start (line-beginning-position
                                      (1+ limit-number))))
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

(ert-deftest time-stamp-custom-count ()
  "Test that time-stamp updates no more than time-stamp-count templates."
  (with-time-stamp-test-env
    (let ((time-stamp-start "TS: <")
          (time-stamp-format "%Y-%m-%d")
          (time-stamp-count 1)          ;changed later in the test
          (buffer-expected-once "TS: <2006-01-02>\nTS: <>")
          (buffer-expected-twice "TS: <2006-01-02>\nTS: <2006-01-02>"))
      (with-time-stamp-test-time ref-time1
        (with-temp-buffer
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
    ;; implemented and documented since 1997
    (should (equal (time-stamp-string "%3a" ref-time1) "Mon"))
    (should (equal (time-stamp-string "%#A" ref-time1) "MONDAY"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%3A" ref-time1) "MON"))
    (should (equal (time-stamp-string "%:a" ref-time1) "Monday"))
    ;; implemented since 2001, documented since 2019
    (should (equal (time-stamp-string "%#a" ref-time1) "MON"))
    (should (equal (time-stamp-string "%:A" ref-time1) "Monday"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%^A" ref-time1) "MONDAY"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%a" ref-time1) "Mon"))
    (should (equal (time-stamp-string "%^a" ref-time1) "MON"))
    (should (equal (time-stamp-string "%A" ref-time1) "Monday"))))

(ert-deftest time-stamp-format-month-name ()
  "Test time-stamp formats for month name."
  (with-time-stamp-test-env
    ;; implemented and documented since 1997
    (should (equal (time-stamp-string "%3b" ref-time1) "Jan"))
    (should (equal (time-stamp-string "%#B" ref-time1) "JANUARY"))
    ;; documented 1997-2019
    (should (equal (time-stamp-string "%3B" ref-time1) "JAN"))
    (should (equal (time-stamp-string "%:b" ref-time1) "January"))
    ;; implemented since 2001, documented since 2019
    (should (equal (time-stamp-string "%#b" ref-time1) "JAN"))
    (should (equal (time-stamp-string "%:B" ref-time1) "January"))
    ;; allowed but undocumented since 2019 (warned 1997-2019)
    (should (equal (time-stamp-string "%^B" ref-time1) "JANUARY"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%b" ref-time1) "Jan"))
    (should (equal (time-stamp-string "%^b" ref-time1) "JAN"))
    (should (equal (time-stamp-string "%B" ref-time1) "January"))))

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
    (should (equal (time-stamp-string "%Y" ref-time1) "2006"))))

(ert-deftest time-stamp-format-am-pm ()
  "Test time-stamp formats for AM and PM strings."
  (with-time-stamp-test-env
    ;; implemented and documented since 1997
    (should (equal (time-stamp-string "%#p" ref-time1) "pm"))
    (should (equal (time-stamp-string "%#p" ref-time3) "am"))
    (should (equal (time-stamp-string "%P" ref-time1) "PM"))
    (should (equal (time-stamp-string "%P" ref-time3) "AM"))
    ;; warned 1997-2019, changed in 2019
    (should (equal (time-stamp-string "%p" ref-time1) "PM"))
    (should (equal (time-stamp-string "%p" ref-time3) "AM"))))

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
  "Test time-stamp format %z."
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
    (should (equal (time-stamp-string "%-z" ref-time1) "+00"))
    (should (equal (time-stamp-string "%_z" ref-time1) "+0000"))
    (should (equal (time-stamp-string "%:z" ref-time1) "+00:00"))
    (should (equal (time-stamp-string "%::z" ref-time1) "+00:00:00"))
    (should (equal (time-stamp-string "%:::z" ref-time1) "+00"))))

(ert-deftest time-stamp-format-non-date-conversions ()
  "Test time-stamp formats for non-date items."
  (with-time-stamp-test-env
    ;; implemented and documented since 1995
    (should (equal (time-stamp-string "%%" ref-time1) "%")) ;% last char
    (should (equal (time-stamp-string "%%P" ref-time1) "%P")) ;% not last char
    (should (equal (time-stamp-string "%f" ref-time1) "time-stamped-file"))
    (should
     (equal (time-stamp-string "%F" ref-time1) "/emacs/test/time-stamped-file"))
    (should (equal (time-stamp-string "%h" ref-time1) "test-mail-host-name"))
    ;; documented 1995-2019
    (should (equal
             (time-stamp-string "%s" ref-time1) "test-system-name.example.org"))
    (should (equal (time-stamp-string "%U" ref-time1) "100%d Tester"))
    (should (equal (time-stamp-string "%u" ref-time1) "test-logname"))
    ;; implemented since 2001, documented since 2019
    (should (equal (time-stamp-string "%L" ref-time1) "100%d Tester"))
    (should (equal (time-stamp-string "%l" ref-time1) "test-logname"))
    ;; implemented since 2007, documented since 2019
    (should (equal
             (time-stamp-string "%Q" ref-time1) "test-system-name.example.org"))
    (should (equal
             (time-stamp-string "%q" ref-time1) "test-system-name"))))

(ert-deftest time-stamp-format-ignored-modifiers ()
  "Test additional args allowed (but ignored) to allow for future expansion."
  (with-time-stamp-test-env
    ;; allowed modifiers
    (should (equal (time-stamp-string "%.,@-+_ ^(stuff)P" ref-time3) "AM"))
    ;; not all punctuation is allowed
    (should-not (equal (time-stamp-string "%&P" ref-time3) "AM"))))

(ert-deftest time-stamp-format-non-conversions ()
  "Test that without a %, the text is copied literally."
  (with-time-stamp-test-env
    (should (equal (time-stamp-string "No percent" ref-time1) "No percent"))))

(ert-deftest time-stamp-format-string-width ()
  "Test time-stamp string width modifiers."
  (with-time-stamp-test-env
    ;; strings truncate on the right or are blank-padded on the left
    (should (equal (time-stamp-string "%0P" ref-time3) ""))
    (should (equal (time-stamp-string "%1P" ref-time3) "A"))
    (should (equal (time-stamp-string "%2P" ref-time3) "AM"))
    (should (equal (time-stamp-string "%3P" ref-time3) " AM"))
    (should (equal (time-stamp-string "%0%" ref-time3) ""))
    (should (equal (time-stamp-string "%1%" ref-time3) "%"))
    (should (equal (time-stamp-string "%2%" ref-time3) " %"))
    (should (equal (time-stamp-string "%#3a" ref-time3) "SUN"))
    (should (equal (time-stamp-string "%#3b" ref-time2) "NOV"))))

;;; Tests of helper functions

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

;;; time-stamp-tests.el ends here
