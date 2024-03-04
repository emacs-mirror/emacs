;;; gnus-search-tests.el --- Tests for Gnus' search routines  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2021-2024 Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords:

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

;; Tests for the search parsing, search engines, and their
;; transformations.

;;; Code:

(require 'ert)
(require 'gnus-search)

(ert-deftest gnus-s-parse ()
  "Test basic structural parsing."
  (let ((pairs
         '(("string" . ("string"))
           ("from:john" . ((from . "john")))
           ("here and there" . ("here" and "there"))
           ("here or there" . ((or "here" "there")))
           ("here (there or elsewhere)" . ("here" ((or "there" "elsewhere"))))
           ("here not there" . ("here" (not "there")))
           ("from:boss or not vacation" . ((or (from . "boss") (not "vacation")))))))
    (dolist (p pairs)
      (should (equal (gnus-search-parse-query (car p)) (cdr p))))))

(ert-deftest gnus-s-expand-keyword ()
  "Test expansion of keywords"
  (let ((gnus-search-expandable-keys
         (default-value 'gnus-search-expandable-keys))
        (pairs
         '(("su" . "subject")
           ("sin" . "since")
           ("body" . "body")
           ("list-id" . "list-id"))))
    (dolist (p pairs)
      (should (equal (gnus-search-query-expand-key (car p))
                     (cdr p))))
    (should-error (gnus-search-query-expand-key "s")
                  :type 'gnus-search-parse-error)))

(ert-deftest gnus-s-parse-date ()
  "Test parsing of date expressions."
  (let ((rel-date (encode-time 0 0 0 15 4 2017))
        (pairs
         '(("January" . (nil 1 nil))
           ("2017" . (nil nil 2017))
           ("15" . (15 nil nil))
           ("January 15" . (15 1 nil))
           ("tuesday" . (11 4 2017))
           ("1d" . (14 4 2017))
           ("1w" . (8 4 2017)))))
    (dolist (p pairs)
      (should (equal (gnus-search-query-parse-date (car p) rel-date)
                     (cdr p))))))

(ert-deftest gnus-s-delimited-string ()
  "Test proper functioning of `gnus-search-query-return-string'."
  (with-temp-buffer
    (insert "one\ntwo words\nthree \"words with quotes\"\n\"quotes at start\"\n/alternate \"quotes\"/\n(more bits)")
    (goto-char (point-min))
    (should (string= (gnus-search-query-return-string)
                     "one"))
    (forward-line)
    (should (string= (gnus-search-query-return-string)
                     "two"))
    (forward-line)
    (should (string= (gnus-search-query-return-string)
                     "three"))
    (forward-line)
    (should (string= (gnus-search-query-return-string "\"")
                     "\"quotes at start\""))
    (forward-line)
    (should (string= (gnus-search-query-return-string "/")
                     "/alternate \"quotes\"/"))
    (forward-line)
    (should (string= (gnus-search-query-return-string ")" t)
                     "more bits"))))

(provide 'gnus-search-tests)
;;; gnus-search-tests.el ends here
