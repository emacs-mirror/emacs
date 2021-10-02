;;; oc-natbib.el --- Citation processor using natbib LaTeX package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;; This library registers the `natbib' citation processor, which provides the
;; "export" capability for citations.

;; The processor relies on "natbib" LaTeX package.  As such it ensures that the
;; package is properly required in the document's preamble.  More accurately, it
;; will use any "\\usepackage{natbib}" command already present in the document
;; (e.g., through `org-latex-packages-alist'), or insert one using options
;; defined in `org-cite-natbib-options'.

;; It supports the following citation styles:
;;
;; - author (a), including caps (c), and full (f) variants,
;; - noauthor (na), including bare (b) variant,
;; - text (t), including bare (b), caps (c), full (f), bare-caps (bc),
;;   bare-full (bf), caps-full (cf), and bare-caps-full (bcf) variants,
;; - default, including bare (b), caps (c), full (f), bare-caps (bc),
;;   bare-full (bf), caps-full (cf), and bare-caps-full (bcf) variants.

;; Bibliography accepts any style supported by "natbib" package.

;;; Code:
(require 'oc)

(declare-function org-element-property "org-element" (property element))

(declare-function org-export-data "org-export" (data info))


;;; Customization
(defcustom org-cite-natbib-options nil
  "List of options added to \"natbib\" package.
If \"natbib\" package is already required in the document, e.g., through
`org-latex-packages-alist' variable, these options are ignored."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type
  '(set
    (const :tag "use round parentheses (default)" round)
    (const :tag "use square brackets" square)
    (const :tag "use curly braces" curly)
    (const :tag "use angle brackets" angle)
    (const :tag "separate multiple citations with colons (default)" colon)
    (const :tag "separate multiple citations with comas" comma)
    (const :tag "generate author-year citations" authoryear)
    (const :tag "generate numerical citations" numbers)
    (const :tag "generate superscripted numerical citations" super)
    (const :tag "order multiple citations according to the list of references" sort)
    (const :tag "order as above, but numerical citations are compressed if possible" sort&compress)
    (const :tag "display full author list on first citation, abbreviate the others" longnamesfirst)
    (const :tag "redefine \\thebibliography to issue \\section* instead of \\chapter*" sectionbib)
    (const :tag "keep all the authors' names in a citation on one line" nonamebreak)))


;;; Internal functions
(defun org-cite-natbib--style-to-command (style)
  "Return command name to use according to STYLE pair."
  (pcase style
    ;; "author" style.
    (`(,(or "author" "a") . ,variant)
     (pcase variant
       ((or "caps" "c")             "\\Citeauthor")
       ((or "full" "f")             "\\citeauthor*")
       (_                           "\\citeauthor")))
    ;; "noauthor" style.
    (`(,(or "noauthor" "na") . ,variant)
     (pcase variant
       ((or "bare" "b")             "\\citeyear")
       (_                           "\\citeyearpar")))
    ;; "nocite" style.
    (`(,(or "nocite" "n") . ,_)     "\\nocite")
    ;; "text" style.
    (`(,(or "text" "t") . ,variant)
     (pcase variant
       ((or "bare" "b")             "\\citealt")
       ((or "caps" "c")             "\\Citet")
       ((or "full" "f")             "\\citet*")
       ((or "bare-caps" "bc")       "\\Citealt")
       ((or "bare-full" "bf")       "\\citealt*")
       ((or "caps-full" "cf")       "\\Citet*")
       ((or "bare-caps-full" "bcf") "\\Citealt*")
       (_ "\\citet")))
    ;; Default ("nil") style.
    (`(,_ . ,variant)
     (pcase variant
       ((or "bare" "b")             "\\citealp")
       ((or "caps" "c")             "\\Citep")
       ((or "full" "f")             "\\citep*")
       ((or "bare-caps" "bc")       "\\Citealp")
       ((or "bare-full" "bf")       "\\citealp*")
       ((or "caps-full" "cf")       "\\Citep*")
       ((or "bare-caps-full" "bcf") "\\Citealp*")
       (_                           "\\citep")))
    ;; This should not happen.
    (_ (error "Invalid style: %S" style))))

(defun org-cite-natbib--build-optional-arguments (citation info)
  "Build optional arguments for citation command.
CITATION is the citation object.  INFO is the export state, as a property list."
  (let* ((origin (pcase (org-cite-get-references citation)
                   (`(,reference) reference)
                   (_ citation)))
         (suffix (org-element-property :suffix origin))
         (prefix (org-element-property :prefix origin)))
    (concat (and prefix (format "[%s]" (org-trim (org-export-data prefix info))))
            (cond
             (suffix (format "[%s]" (org-trim (org-export-data suffix info))))
             (prefix "[]")
             (t nil)))))

(defun org-cite-natbib--build-arguments (citation)
  "Build arguments for citation command for CITATION object."
  (format "{%s}"
          (mapconcat #'identity
                     (org-cite-get-references citation t)
                     ",")))


;;; Export capability
(defun org-cite-natbib-export-bibliography (_keys files style &rest _)
  "Print references from bibliography FILES.
FILES is a list of absolute file names.  STYLE is the bibliography style, as
a string or nil."
  (concat (and style (format "\\bibliographystyle{%s}\n" style))
          (format "\\bibliography{%s}"
                  (mapconcat #'file-name-sans-extension
                             files
                             ","))))

(defun org-cite-natbib-export-citation (citation style _ info)
  "Export CITATION object.
STYLE is the citation style, as a pair of strings or nil.  INFO is the export
state, as a property list."
  (concat (org-cite-natbib--style-to-command style)
          (org-cite-natbib--build-optional-arguments citation info)
          (org-cite-natbib--build-arguments citation)))

(defun org-cite-natbib-use-package (output &rest _)
  "Ensure output requires \"natbib\" package.
OUTPUT is the final output of the export process."
  (with-temp-buffer
    (save-excursion (insert output))
    (when (search-forward "\\begin{document}" nil t)
      ;; Ensure there is a \usepackage{natbib} somewhere or add one.
      (goto-char (match-beginning 0))
      (let ((re (rx "\\usepackage" (opt "[" (*? nonl) "]") "{natbib}")))
        (unless (re-search-backward re nil t)
          (insert
           (format "\\usepackage%s{natbib}\n"
                   (if (null org-cite-natbib-options)
                       ""
                     (format "[%s]"
                             (mapconcat #'symbol-name
                                        org-cite-natbib-options
                                        ","))))))))
    (buffer-string)))


;;; Register `natbib' processor
(org-cite-register-processor 'natbib
  :export-bibliography #'org-cite-natbib-export-bibliography
  :export-citation #'org-cite-natbib-export-citation
  :export-finalizer #'org-cite-natbib-use-package
  :cite-styles
  '((("author" "a") ("caps" "a") ("full" "f"))
    (("noauthor" "na") ("bare" "b"))
    (("text" "t")
     ("bare" "b") ("caps" "c") ("full" "f") ("bare-caps" "bc")
     ("bare-full" "bf") ("caps-full" "cf") ("bare-caps-full" "bcf"))
    (("nil")
     ("bare" "b") ("caps" "c") ("full" "f") ("bare-caps" "bc")
     ("bare-full" "bf") ("caps-full" "cf") ("bare-caps-full" "bcf"))))

(provide 'oc-natbib)
;;; oc-natbib.el ends here
