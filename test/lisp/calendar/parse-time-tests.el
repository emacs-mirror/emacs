;; parse-time-tests.el --- Test suite for parse-time.el

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
(require 'parse-time)

(ert-deftest parse-time-tests ()
  (should (equal (parse-time-string "Mon, 22 Feb 2016 19:35:42 +0100")
                 '(42 35 19 22 2 2016 1 -1 3600)))
  (should (equal (parse-time-string "22 Feb 2016 19:35:42 +0100")
                 '(42 35 19 22 2 2016 nil -1 3600)))
  (should (equal (parse-time-string "22 Feb 2016 +0100")
                 '(nil nil nil 22 2 2016 nil -1 3600)))
  (should (equal (parse-time-string "Mon, 22 Feb 16 19:35:42 +0100")
                 '(42 35 19 22 2 2016 1 -1 3600)))
  (should (equal (parse-time-string "Mon, 22 February 2016 19:35:42 +0100")
                 '(42 35 19 22 2 2016 1 -1 3600)))
  (should (equal (parse-time-string "Mon, 22 feb 2016 19:35:42 +0100")
                 '(42 35 19 22 2 2016 1 -1 3600)))
  (should (equal (parse-time-string "Monday, 22 february 2016 19:35:42 +0100")
                 '(42 35 19 22 2 2016 1 -1 3600)))
  (should (equal (parse-time-string "Monday, 22 february 2016 19:35:42 PST")
                 '(42 35 19 22 2 2016 1 nil -28800)))
  (should (equal (parse-time-string "Friday, 21 Sep 2018 13:47:58 PDT")
                 '(58 47 13 21 9 2018 5 t -25200)))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54-0200") t)
		 "1998-09-12 14:21:54"))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54-0230") t)
		 "1998-09-12 14:51:54"))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54-02:00") t)
		 "1998-09-12 14:21:54"))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54-02") t)
		 "1998-09-12 14:21:54"))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54+0230") t)
		 "1998-09-12 09:51:54"))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54+02") t)
		 "1998-09-12 10:21:54"))
  (should (equal (format-time-string
		  "%Y-%m-%d %H:%M:%S"
		  (parse-iso8601-time-string "1998-09-12T12:21:54Z") t)
		 "1998-09-12 12:21:54"))
  (should (equal (parse-iso8601-time-string "1998-09-12T12:21:54")
                 (encode-time 54 21 12 12 9 1998))))

(provide 'parse-time-tests)

;;; parse-time-tests.el ends here
