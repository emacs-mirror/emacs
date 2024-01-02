;;; gnus-util-tests.el --- Selectived tests only.  -*- lexical-binding:t -*-
;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: Jens Lechtenbörger <jens.lechtenboerger@fsfe.org>

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
(require 'gnus-util)

(ert-deftest gnus-string> ()
  ;; Failure paths
  (should-error (gnus-string> "" 1)
                :type 'wrong-type-argument)
  (should-error (gnus-string> "")
                :type 'wrong-number-of-arguments)

  ;; String tests
  (should (gnus-string> "def" "abc"))
  (should (gnus-string> 'def 'abc))
  (should (gnus-string> "abc" "DEF"))
  (should (gnus-string> "abc" 'DEF))
  (should (gnus-string> "αβγ" "abc"))
  (should (gnus-string> "אבג" "αβγ"))
  (should (gnus-string> nil ""))
  (should (gnus-string> "abc" ""))
  (should (gnus-string> "abc" "ab"))
  (should-not (gnus-string> "abc" "abc"))
  (should-not (gnus-string> "abc" "def"))
  (should-not (gnus-string> "DEF" "abc"))
  (should-not (gnus-string> 'DEF "abc"))
  (should-not (gnus-string> "123" "abc"))
  (should-not (gnus-string> "" "")))

(ert-deftest gnus-string< ()
  ;; Failure paths
  (should-error (gnus-string< "" 1)
                :type 'wrong-type-argument)
  (should-error (gnus-string< "")
                :type 'wrong-number-of-arguments)

  ;; String tests
  (setq case-fold-search nil)
  (should (gnus-string< "abc" "def"))
  (should (gnus-string< 'abc 'def))
  (should (gnus-string< "DEF" "abc"))
  (should (gnus-string< "DEF" 'abc))
  (should (gnus-string< "abc" "αβγ"))
  (should (gnus-string< "αβγ" "אבג"))
  (should (gnus-string< "" nil))
  (should (gnus-string< "" "abc"))
  (should (gnus-string< "ab" "abc"))
  (should-not (gnus-string< "abc" "abc"))
  (should-not (gnus-string< "def" "abc"))
  (should-not (gnus-string< "abc" "DEF"))
  (should-not (gnus-string< "abc" 'DEF))
  (should-not (gnus-string< "abc" "123"))
  (should-not (gnus-string< "" ""))

  ;; gnus-string< checks case-fold-search
  (setq case-fold-search t)
  (should (gnus-string< "abc" "DEF"))
  (should (gnus-string< "abc" 'GHI))
  (should (gnus-string< 'abc "DEF"))
  (should (gnus-string< 'GHI 'JKL))
  (should (gnus-string< "abc" "ΑΒΓ"))
  (should-not (gnus-string< "ABC" "abc"))
  (should-not (gnus-string< "def" "ABC")))

(ert-deftest gnus-subsetp ()
  ;; False for non-lists.
  (should-not (gnus-subsetp "1" "1"))
  (should-not (gnus-subsetp "1" '("1")))
  (should-not (gnus-subsetp '("1") "1"))

  ;; Real tests.
  (should (gnus-subsetp '() '()))
  (should (gnus-subsetp '() '("1")))
  (should (gnus-subsetp '("1") '("1")))
  (should (gnus-subsetp '(42) '("1" 42)))
  (should (gnus-subsetp '(42) '(42 "1")))
  (should (gnus-subsetp '(42) '("1" 42 2)))
  (should-not (gnus-subsetp '("1") '()))
  (should-not (gnus-subsetp '("1") '(2)))
  (should-not (gnus-subsetp '("1" 2) '(2)))
  (should-not (gnus-subsetp '(2 "1") '(2)))
  (should-not (gnus-subsetp '("1" 2) '(2 3)))

  ;; Duplicates don't matter for sets.
  (should (gnus-subsetp '("1" "1") '("1")))
  (should (gnus-subsetp '("1" 2 "1") '(2 "1")))
  (should (gnus-subsetp '("1" 2 "1") '(2 "1" "1" 2)))
  (should-not (gnus-subsetp '("1" 2 "1" 3) '(2 "1" "1" 2))))

(ert-deftest gnus-setdiff ()
  ;; False for non-lists.
  (should-not (gnus-setdiff "1" "1"))
  (should-not (gnus-setdiff "1" '()))
  (should-not (gnus-setdiff '() "1"))

  ;; Real tests.
  (should-not (gnus-setdiff '() '()))
  (should-not (gnus-setdiff '() '("1")))
  (should-not (gnus-setdiff '("1") '("1")))
  (should (equal '("1") (gnus-setdiff '("1") '())))
  (should (equal '("1") (gnus-setdiff '("1") '(2))))
  (should (equal '("1") (gnus-setdiff '("1" 2) '(2))))
  (should (equal '("1") (gnus-setdiff '("1" 2 3) '(3 2))))
  (should (equal '("1") (gnus-setdiff '(2 "1" 3) '(3 2))))
  (should (equal '("1") (gnus-setdiff '(2 3 "1") '(3 2))))
  (should (equal '(2 "1") (gnus-setdiff '(2 3 "1") '(3))))

  ;; Duplicates aren't touched for sets if they are not removed.
  (should-not (gnus-setdiff '("1" "1") '("1")))
  (should (equal '("1") (gnus-setdiff '(2 "1" 2) '(2))))
  (should (equal '("1" "1") (gnus-setdiff '(2 "1" 2 "1") '(2)))))

;;; gnus-util-tests.el ends here
