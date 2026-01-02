;;; document-tests.el --- Tests for srecode/document.el  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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

;; Extracted from srecode-document.el in the CEDET distribution.

;; Converted to ert from test/manual/cedet/srecode-tests.el

;;; Code:

(require 'ert)
(require 'srecode/document)

;; FIXME: This test fails even before conversion to ert.
(ert-deftest srecode-document-function-comment-extract-test ()
  "Test old comment extraction.
Dump out the extracted dictionary."
  :tags '(:unstable)
  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (should (srecode-table))
  ;; (error "No template table found for mode %s" major-mode)

  (let* ((temp (srecode-template-get-table (srecode-table)
                                           "function-comment"
                                           "declaration"
                                           'document))
         (fcn-in (semantic-current-tag)))

    (should temp)
    ;; (error "No templates for function comments")

    ;; Try to figure out the tag we want to use.
    (should fcn-in)
    (should (semantic-tag-of-class-p fcn-in 'function))
    ;; (error "No tag of class 'function to insert comment for")

    (let ((lextok (semantic-documentation-comment-preceding-tag fcn-in 'lex)))

      (should lextok)
      ;; (error "No comment to attempt an extraction")

      (let ((s (semantic-lex-token-start lextok))
            (e (semantic-lex-token-end lextok))
            (extract nil))

        (pulse-momentary-highlight-region s e)

        ;; Extract text from the existing comment.
        (setq extract (srecode-extract temp s e))

        (with-output-to-temp-buffer "*SRECODE DUMP*"
          (princ "EXTRACTED DICTIONARY FOR ")
          (princ (semantic-tag-name fcn-in))
          (princ "\n--------------------------------------------\n")
          (srecode-dump extract))))))

;;; document-tests.el ends here
