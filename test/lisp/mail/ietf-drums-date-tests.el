;;; ietf-drums-date-tests.el --- Test suite for ietf-drums-date.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Bob Rogers <rogers@rgrjr.com>

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

;;; Code:

(require 'ert)
(require 'ietf-drums)
(require 'ietf-drums-date)

(ert-deftest ietf-drums-date-tests ()
  "Test basic ietf-drums-parse-date-string functionality."

  ;; Test tokenization.
  (should (equal (ietf-drums-date--tokenize-string " ") '()))
  (should (equal (ietf-drums-date--tokenize-string " a b") '("a" "b")))
  (should (equal (ietf-drums-date--tokenize-string "a bbc dde")
                 '("a" "bbc" "dde")))
  (should (equal (ietf-drums-date--tokenize-string " , a 27 b,, c 14:32 ")
                 '("a" 27 "b" "c" "14:32")))
  ;; Some folding whitespace tests.
  (should (equal (ietf-drums-date--tokenize-string " a b (end) c" t)
                 '("a" "b")))
  (should (equal (ietf-drums-date--tokenize-string "(quux)a (foo (bar)) b(baz)")
                 '("a" "b")))
  (should (equal (ietf-drums-date--tokenize-string "a b\\cde")
                 ;; Strictly incorrect, but strictly unnecessary syntax.
                 '("a" "b\\cde")))
  (should (equal (ietf-drums-date--tokenize-string "a b\\ de")
                 '("a" "b\\ de")))
  (should (equal (ietf-drums-date--tokenize-string "a \\de \\(f")
                 '("a" "\\de" "\\(f")))

  ;; Start with some compatible RFC822 dates.
  (dolist (case '(("Mon, 22 Feb 2016 19:35:42 +0100"
		   (42 35 19 22 2 2016 1 -1 3600))
                  ("22 Feb 2016 19:35:42 +0100"
		   (42 35 19 22 2 2016 nil -1 3600))
                  ("Mon, 22 February 2016 19:35:42 +0100"
		   (42 35 19 22 2 2016 1 -1 3600))
                  ("Mon, 22 feb 2016 19:35:42 +0100"
		   (42 35 19 22 2 2016 1 -1 3600))
                  ("Monday, 22 february 2016 19:35:42 +0100"
		   (42 35 19 22 2 2016 1 -1 3600))
                  ("Monday, 22 february 2016 19:35:42 PST"
		   (42 35 19 22 2 2016 1 nil -28800))
                  ("Friday, 21 Sep 2018 13:47:58 PDT"
		   (58 47 13 21 9 2018 5 t -25200))
                  ("Friday, 21 Sep 2018 13:47:58 EDT"
		   (58 47 13 21 9 2018 5 t -14400))
		  ("Mon, 22 Feb 2016 19:35:42"
		   (42 35 19 22 2 2016 1 -1 nil))
		  ("Friday, 21 Sep 2018 13:47:58"
		   (58 47 13 21 9 2018 5 -1 nil))))
    (let* ((input (car case))
	   (parsed (cadr case)))
      ;; The input should parse the same without RFC822.
      (should (equal (ietf-drums-parse-date-string input) parsed))
      (should (equal (ietf-drums-parse-date-string input nil t) parsed))
      ;; Check the encoded date (the official output, though the
      ;; decoded-time is easier to debug).
      (should (time-equal-p (ietf-drums-parse-date input)
			    (encode-time parsed)))))

  ;; Two-digit years are not allowed by the "modern" format.
  (should (equal (ietf-drums-parse-date-string "22 Feb 16 19:35:42 +0100")
                 '(42 35 19 22 2 2016 nil -1 3600)))
  (should (equal (ietf-drums-parse-date-string "22 Feb 16 19:35:42 +0100" nil t)
                 '(nil nil nil 22 2 nil nil -1 nil)))
  (should (equal (should-error (ietf-drums-parse-date-string
                                "22 Feb 16 19:35:42 +0100" t t))
                 '(date-parse-error "Four-digit years are required" 16)))
  (should (equal (ietf-drums-parse-date-string "22 Feb 96 19:35:42 +0100")
                 '(42 35 19 22 2 1996 nil -1 3600)))
  (should (equal (ietf-drums-parse-date-string "22 Feb 96 19:35:42 +0100" nil t)
                 '(nil nil nil 22 2 nil nil -1 nil)))
  (should (equal (should-error (ietf-drums-parse-date-string
                                "22 Feb 96 19:35:42 +0100" t t))
                 '(date-parse-error "Four-digit years are required" 96)))

  ;; Try some dates with comments.
  (should (equal (ietf-drums-parse-date-string
                  "22 Feb (today) 16 19:35:42 +0100")
                 '(42 35 19 22 2 2016 nil -1 3600)))
  (should (equal (ietf-drums-parse-date-string
                  "22 Feb (today) 16 19:35:42 +0100" nil t)
                 '(nil nil nil 22 2 nil nil -1 nil)))
  (should (equal (should-error (ietf-drums-parse-date-string
                                "22 Feb (today) 16 19:35:42 +0100" t t))
                 '(date-parse-error "Expected a year" nil)))
  (should (equal (ietf-drums-parse-date-string
                  "22 Feb 96 (long ago) 19:35:42 +0100")
                 '(42 35 19 22 2 1996 nil -1 3600)))
  (should (equal (ietf-drums-parse-date-string
                  "Friday, 21 Sep(comment \\) with \\( parens)18 19:35:42")
                 '(42 35 19 21 9 2018 5 -1 nil)))
  (should (equal (ietf-drums-parse-date-string
                  "Friday, 21 Sep 18 19:35:42 (unterminated comment")
                 '(42 35 19 21 9 2018 5 -1 nil)))

  ;; Test some RFC822 error cases
  (dolist (test '(("33 1 2022" ("Slot out of range" day 33 1 31))
                  ("0 1 2022" ("Slot out of range" day 0 1 31))
                  ("1 1 2020 2021" ("Expected an alphabetic month" 1))
                  ("1 Jan 2020 2021" ("Expected a time" 2021))
                  ("1 Jan 2020 20:21 2000" ("Expected a timezone" 2000))
                  ("1 Jan 2020 20:21 +0200 33" ("Extra token(s)" 33))))
    (should (equal (should-error (ietf-drums-parse-date-string (car test) t))
                   (cons 'date-parse-error (cadr test)))))

  (dolist (test '(("22 Feb 196" nil		;; bad year
                   ("Four-digit years are required" 196))
                  ("22 Feb 16 19:35:24" t	;; two-digit year
                   ("Four-digit years are required" 16))
                  ("22 Feb 96 19:35:42" t	;; two-digit year
                   ("Four-digit years are required" 96))
                  ("2 Feb 2021 1996" nil
                   ("Expected a time" 1996))
                  ("22 Fub 1996" nil
                   ("Expected an alphabetic month" "fub"))
                  ("1 Jan 2020 30" nil
                   ("Expected a time" 30))
                  ("1 Jan 2020 16:47 15:15" nil
                   ("Expected a timezone" "15:15"))
                  ("1 Jan 2020 16:47 +0800 -0800" t
                   ("Extra token(s)" "-0800"))
                  ;; Range tests
                  ("32 Dec 2021" nil
                   ("Slot out of range" day 32 1 31))
                  ("0 Dec 2021" nil
                   ("Slot out of range" day 0 1 31))
                  ("3 13 2021" nil
                   ("Expected an alphabetic month" 13))
                  ("3 Dec 0000" t
                   ("Four-digit years are required" 0))
                  ("3 Dec 20021" nil
                   ("Slot out of range" year 20021 1 9999))
                  ("1 Jan 2020 24:21:14" nil
                   ("Slot out of range" hour "24:21:14" 0 23))
                  ("1 Jan 2020 14:60:21" nil
                   ("Slot out of range" minute "14:60:21" 0 59))
                  ("1 Jan 2020 14:21:61" nil
                   ("Slot out of range" second "14:21:61" 0 60))))
    (should (equal (should-error
                    (ietf-drums-parse-date-string (car test) t (cadr test)))
                   (cons 'date-parse-error (caddr test)))))
  (should (equal (ietf-drums-parse-date-string
                  "1 Jan 2020 14:21:60")	;; a leap second!
                 '(60 21 14 1 1 2020 nil -1 nil))))

(provide 'ietf-drums-date-tests)

;;; ietf-drums-date-tests.el ends here
