;;; oc-bibtex.el --- Vanilla citation processor for LaTeX -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;; This library registers the `bibtex' citation processor, which
;; provides the "export" capability for citations.  It doesn't require
;; any LaTeX package.
;;
;; It supports the following citation styles:
;;
;; - nocite (n),
;; - default.
;;
;; Only suffixes are supported.  Prefixes are ignored.
;;
;; Bibliography should consist of ".bib" files only.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'oc)

(declare-function org-element-property "org-element" (property element))

(declare-function org-export-data "org-export" (data info))


;;; Export capability
(defun org-cite-bibtex-export-bibliography (_keys files style &rest _)
  "Print references from bibliography FILES.
FILES is a list of absolute file names.  STYLE is the bibliography style, as
a string or nil."
  (concat (and style (format "\\bibliographystyle{%s}\n" style))
          (format "\\bibliography{%s}"
                  (mapconcat #'file-name-sans-extension
                             files
                             ","))))

(defun org-cite-bibtex-export-citation (citation style _ info)
  "Export CITATION object.
STYLE is the citation style, as a pair of strings or nil.  INFO is the export
state, as a property list."
  (let ((references (org-cite-get-references citation)))
    (format "\\%s%s{%s}"
            (pcase style
              (`(,(or "nocite" "n") . ,_) "nocite")
              (_ "cite"))
            (let ((suffix (cdr (org-cite-main-affixes citation))))
              (if suffix
                  (format "[%s]" (org-trim (org-export-data suffix info)))
                ""))
            (mapconcat (lambda (r) (org-element-property :key r))
                       references
                       ","))))


;;; Register `bibtex' processor
(org-cite-register-processor 'bibtex
  :export-bibliography #'org-cite-bibtex-export-bibliography
  :export-citation #'org-cite-bibtex-export-citation
  :cite-styles
  '((("nocite" "n"))
    (("nil"))))

(provide 'oc-bibtex)
;;; oc-bibtex.el ends here
