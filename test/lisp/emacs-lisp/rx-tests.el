;;; rx-tests.el --- test for rx.el functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019 Free Software Foundation, Inc.

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

(require 'ert)
(require 'rx)

;;; Code:

(ert-deftest rx-char-any ()
  "Test character alternatives with `]' and `-' (Bug#25123)."
  (should (string-match
           (rx string-start (1+ (char (?\] . ?\{) (?< . ?\]) (?- . ?:)))
               string-end)
           (apply #'string (nconc (number-sequence ?\] ?\{)
                                  (number-sequence ?< ?\])
                                  (number-sequence ?- ?:))))))

(ert-deftest rx-char-any-range-nl ()
  "Test character alternatives with LF as a range endpoint."
  (should (equal (rx (any "\n-\r"))
                 "[\n-\r]"))
  (should (equal (rx (any "\a-\n"))
                 "[\a-\n]")))

(ert-deftest rx-char-any-range-bad ()
  (should-error (rx (any "0-9a-Z")))
  (should-error (rx (any (?0 . ?9) (?a . ?Z)))))

(ert-deftest rx-char-any-raw-byte ()
  "Test raw bytes in character alternatives."
  ;; Separate raw characters.
  (should (equal (string-match-p (rx (any "\326A\333B"))
                                 "X\326\333")
                 1))
  ;; Range of raw characters, unibyte.
  (should (equal (string-match-p (rx (any "\200-\377"))
                                 "ÿA\310B")
                 2))
  ;; Range of raw characters, multibyte.
  (should (equal (string-match-p (rx (any "Å\211\326-\377\177"))
                                 "XY\355\177\327")
                 2))
  ;; Split range; \177-\377ÿ should not be optimised to \177-\377.
  (should (equal (string-match-p (rx (any "\177-\377" ?ÿ))
                                 "ÿA\310B")
                 0)))

(ert-deftest rx-pcase ()
  (should (equal (pcase "a 1 2 3 1 1 b"
                   ((rx (let u (+ digit)) space
                        (let v (+ digit)) space
                        (let v (+ digit)) space
                        (backref u) space
                        (backref 1))
                    (list u v)))
                 '("1" "3"))))

(ert-deftest rx-kleene ()
  "Test greedy and non-greedy repetition operators."
  (should (equal (rx (* "a") (+ "b") (\? "c") (?\s "d")
                     (*? "e") (+? "f") (\?? "g") (?? "h"))
                 "a*b+c?d?e*?f+?g??h??"))
  (should (equal (rx (zero-or-more "a") (0+ "b")
                     (one-or-more "c") (1+ "d")
                     (zero-or-one "e") (optional "f") (opt "g"))
                 "a*b*c+d+e?f?g?"))
  (should (equal (rx (minimal-match
                      (seq (* "a") (+ "b") (\? "c") (?\s "d")
                           (*? "e") (+? "f") (\?? "g") (?? "h"))))
                 "a*b+c?d?e*?f+?g??h??"))
  (should (equal (rx (minimal-match
                      (seq (zero-or-more "a") (0+ "b")
                           (one-or-more "c") (1+ "d")
                           (zero-or-one "e") (optional "f") (opt "g"))))
                 "a*?b*?c+?d+?e??f??g??"))
  (should (equal (rx (maximal-match
                      (seq (* "a") (+ "b") (\? "c") (?\s "d")
                         (*? "e") (+? "f") (\?? "g") (?? "h"))))
                 "a*b+c?d?e*?f+?g??h??")))

(ert-deftest rx-or ()
  ;; Test or-pattern reordering (Bug#34641).
  (let ((s "abc"))
    (should (equal (and (string-match (rx (or "abc" "ab" "a")) s)
                        (match-string 0 s))
                   "abc"))
    (should (equal (and (string-match (rx (or "ab" "abc" "a")) s)
                        (match-string 0 s))
                   "ab"))
    (should (equal (and (string-match (rx (or "a" "ab" "abc")) s)
                        (match-string 0 s))
                   "a"))))

(provide 'rx-tests)
;; rx-tests.el ends here.
