;;; xsd-regexp-tests.el --- Test NXML Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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
(require 'xsd-regexp)

(ert-deftest xsdre-matches ()
  (should (equal (string-match (xsdre-translate "\\p{Pd}") "a-b") 1))
  (should (equal (string-match (xsdre-translate "\\p{P}") "a-b") 1))
  ;; Make sure successive xsdre-translate calls return identical values
  ;; (see bug#82281).
  (should (= (+ (string-match-p (concat "\\`" (xsdre-translate "\\p{P}") "\\'")
                                "#")
                (string-match-p (concat "\\`" (xsdre-translate "\\p{P}") "\\'")
                                "#")
                (string-match-p (concat "\\`" (xsdre-translate "\\p{P}") "\\'")
                                "#"))
             0))
  (let (case-fold-search)
    (should-not (string-match-p (concat "\\`" (xsdre-translate "\\p{Lu}") "\\'")
                                "a"))))

(provide 'xsd-regexp-tests)

;;; xsd-regexp-tests.el ends here
