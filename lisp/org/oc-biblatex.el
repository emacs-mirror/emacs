;;; oc-biblatex.el --- biblatex citation processor for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; This library registers the `biblatex' citation processor, which provides
;; the "export" capability for citations.

;; The processor relies on "biblatex" LaTeX package.  As such it ensures that
;; the package is properly required in the document's preamble.  More
;; accurately, it will re-use any "\usepackage{biblatex}" already present in
;; the document (e.g., through `org-latex-packages-alist'), or insert one using
;; options defined in `org-cite-biblatex-options'.

;; In any case, the library will override style-related options with those
;; specified with the citation processor, in `org-cite-export-processors' or
;; "cite_export" keyword.  If you need to use different styles for bibliography
;; and citations, you can separate them with "bibstyle/citestyle" syntax.  E.g.,
;;
;;   #+cite_export: biblatex authortitle/authortitle-ibid

;; The library supports the following citation styles:
;;
;; - author (a), including caps (c), full (f) and caps-full (cf) variants,
;; - locators (l), including bare (b), caps (c) and bare-caps (bc) variants,
;; - noauthor (na),
;; - nocite (n),
;; - text (t), including caps (c) variant,
;; - default style, including bare (b), caps (c) and bare-caps (bc) variants.

;; When citation and style permit, the library automatically generates
;; "multicite" versions of the commands above.

;; Bibliography is printed using "\printbibliography" command.  Additional
;; options may be passed to it through a property list attached to the
;; "print_bibliography" keyword.  E.g.,
;;
;;    #+print_bibliography: :section 2 :heading subbibliography
;;
;; Values including spaces must be surrounded with double quotes.  If you need
;; to use a key multiple times, you can separate its values with commas, but
;; without any space in-between:
;;
;;    #+print_bibliography: :keyword abc,xyz :title "Primary Sources"

;;; Code:
(require 'org-macs)
(require 'oc)

(declare-function org-element-property "org-element" (property element))
(declare-function org-export-data "org-export" (data info))
(declare-function org-export-get-next-element "org-export" (blob info &optional n))


;;; Customization
(defcustom org-cite-biblatex-options nil
  "Options added to \"biblatex\" package.
If \"biblatex\" package is already required in the document, e.g., through
`org-latex-packages-alist' variable, these options are ignored."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice
          (string :tag "Options (key=value,key2=value2...)")
          (const :tag "No option" nil))
  :safe #'string-or-null-p)


;;; Internal functions
(defun org-cite-biblatex--package-options (initial style)
  "Return options string for \"biblatex\" package.

INITIAL is an initial style of comma-separated options, as a string or nil.
STYLE is the style definition as a string or nil.

Return a string."
  (let ((options-no-style
         (and initial
              (let ((re (rx string-start (or "bibstyle" "citestyle" "style"))))
                (seq-filter
                 (lambda (option) (not (string-match re option)))
                 (split-string (org-unbracket-string "[" "]" initial)
                               "," t " \t")))))
        (style-options
         (cond
          ((null style) nil)
          ((not (string-match "/" style)) (list (concat "style=" style)))
          (t
           (list (concat "bibstyle=" (substring style nil (match-beginning 0)))
                 (concat "citestyle=" (substring style (match-end 0))))))))
    (if (or options-no-style style-options)
        (format "[%s]"
                (mapconcat #'identity
                           (append options-no-style style-options)
                           ","))
      "")))

(defun org-cite-biblatex--multicite-p  (citation)
  "Non-nil when citation could make use of a \"multicite\" command."
  (let ((references (org-cite-get-references citation)))
    (and (< 1 (length references))
         (seq-some (lambda (r)
                     (or (org-element-property :prefix r)
                         (org-element-property :suffix r)))
                   references))))

(defun org-cite-biblatex--atomic-arguments (references info &optional no-opt)
  "Build argument for the list of citation REFERENCES.
When NO-OPT argument is non-nil, only provide mandatory arguments."
  (let ((mandatory
         (format "{%s}"
                 (mapconcat (lambda (r) (org-element-property :key r))
                            references
                            ","))))
    (if no-opt mandatory
      (let* ((origin (pcase references
                       (`(,reference) reference)
                       (`(,reference . ,_)
                        (org-element-property :parent reference))))
             (suffix (org-element-property :suffix origin))
             (prefix (org-element-property :prefix origin)))
        (concat (and prefix
                     (format "[%s]" (org-trim (org-export-data prefix info))))
                (cond
                 (suffix (format "[%s]"
                                 (org-trim (org-export-data suffix info))))
                 (prefix "[]")
                 (t nil))
                mandatory)))))

(defun org-cite-biblatex--multi-arguments (citation info)
  "Build \"multicite\" command arguments for CITATION object.
INFO is the export state, as a property list."
  (let ((global-prefix (org-element-property :prefix citation))
        (global-suffix (org-element-property :suffix citation)))
    (concat (and global-prefix
                 (format "(%s)"
                         (org-trim (org-export-data global-prefix info))))
            (cond
             ;; Global pre/post-notes.
             (global-suffix
              (format "(%s)"
                      (org-trim (org-export-data global-suffix info))))
             (global-prefix "()")
             (t nil))
            ;; All arguments.
            (mapconcat (lambda (r)
                         (org-cite-biblatex--atomic-arguments (list r) info))
                       (org-cite-get-references citation)
                       "")
            ;; According to BibLaTeX manual, left braces or brackets
            ;; following a multicite command could be parsed as other
            ;; arguments. So we stop any further parsing by inserting
            ;; a \relax unconditionally.
            "\\relax")))

(defun org-cite-biblatex--command (citation info base &optional multi no-opt)
  "Return biblatex command using BASE name for CITATION object.

INFO is the export state, as a property list.

When optional argument MULTI is non-nil, generate a \"multicite\" command when
appropriate.  When optional argument NO-OPT is non-nil, do not add optional
arguments to the command."
  (format "\\%s%s"
          base
          (if (and multi (org-cite-biblatex--multicite-p citation))
              (concat "s" (org-cite-biblatex--multi-arguments citation info))
            (org-cite-biblatex--atomic-arguments
             (org-cite-get-references citation) info no-opt))))


;;; Export capability
(defun org-cite-biblatex-export-bibliography (_keys _files _style props &rest _)
  "Print references from bibliography.
PROPS is the local properties of the bibliography, as a property list."
  (concat "\\printbibliography"
          (and props
               (let ((key nil)
                     (results nil))
                 (dolist (datum props)
                   (cond
                    ((keywordp datum)
                     (when key (push key results))
                     (setq key (substring (symbol-name datum) 1)))
                    (t
                     ;; Comma-separated values are associated to the
                     ;; same keyword.
                     (push (mapconcat (lambda (v) (concat key "=" v))
                                      (split-string datum "," t)
                                      ",")
                           results)
                     (setq key nil))))
                 (format "[%s]"
                         (mapconcat #'identity (nreverse results) ","))))))

(defun org-cite-biblatex-export-citation (citation style _ info)
  "Export CITATION object.
STYLE is the citation style, as a pair of either strings or nil.
INFO is the export state, as a property list."
  (apply
   #'org-cite-biblatex--command citation info
   (pcase style
     ;; "author" style.
     (`(,(or "author" "a") . ,variant)
      (pcase variant
        ((or "caps" "c")            '("Citeauthor*"))
        ((or "full" "f")            '("citeauthor"))
        ((or "caps-full" "cf")      '("Citeauthor"))
        (_                          '("citeauthor*"))))
     ;; "locators" style.
     (`(,(or "locators" "l") . ,variant)
      (pcase variant
        ((or "bare" "b")            '("notecite"))
        ((or "caps" "c")            '("Pnotecite"))
        ((or "bare-caps" "bc")      '("Notecite"))
        (_                          '("pnotecite"))))
     ;; "noauthor" style.
     (`(,(or "noauthor" "na") . ,_) '("autocite*"))
     ;; "nocite" style.
     (`(,(or "nocite" "n") . ,_)    '("nocite" nil t))
     ;; "text" style.
     (`(,(or "text" "t") . ,variant)
      (pcase variant
        ((or "caps" "c")            '("Textcite" t))
        (_                          '("textcite" t))))
     ;; Default "nil" style.
     (`(,_ . ,variant)
      (pcase variant
        ((or "bare" "b")            '("cite" t))
        ((or "caps" "c")            '("Autocite" t))
        ((or "bare-caps" "bc")      '("Cite" t))
        (_                          '("autocite" t))))
     ;; This should not happen.
     (_ (error "Invalid style: %S" style)))))

(defun org-cite-biblatex-prepare-preamble (output _keys files style &rest _)
  "Prepare document preamble for \"biblatex\" usage.

OUTPUT is the final output of the export process.  FILES is the list of file
names used as the bibliography.

This function ensures \"biblatex\" package is required.  It also adds resources
to the document, and set styles."
  (with-temp-buffer
    (save-excursion (insert output))
    (when (search-forward "\\begin{document}" nil t)
      ;; Ensure there is a \usepackage{biblatex} somewhere or add one.
      ;; Then set options.
      (goto-char (match-beginning 0))
      (let ((re (rx "\\usepackage"
                    (opt (group "[" (*? anything) "]"))
                    "{biblatex}")))
        (cond
         ;; No "biblatex" package loaded.  Insert "usepackage" command
         ;; with appropriate options, including style.
         ((not (re-search-backward re nil t))
          (save-excursion
            (insert
             (format "\\usepackage%s{biblatex}\n"
                     (org-cite-biblatex--package-options
                      org-cite-biblatex-options style)))))
         ;; "biblatex" package loaded, but without any option.
         ;; Include style only.
         ((not (match-beginning 1))
          (search-forward "{" nil t)
          (insert (org-cite-biblatex--package-options nil style)))
         ;; "biblatex" package loaded with some options set.  Override
         ;; style-related options with ours.
         (t
          (replace-match
           (save-match-data
             (org-cite-biblatex--package-options (match-string 1) style))
           nil nil nil 1))))
      ;; Insert resources below.
      (forward-line)
      (insert (mapconcat (lambda (f)
                           (format "\\addbibresource%s{%s}"
                                   (if (org-url-p f) "[location=remote]" "")
                                   f))
                         files
                         "\n")
              "\n"))
    (buffer-string)))


;;; Register `biblatex' processor
(org-cite-register-processor 'biblatex
  :export-bibliography #'org-cite-biblatex-export-bibliography
  :export-citation #'org-cite-biblatex-export-citation
  :export-finalizer #'org-cite-biblatex-prepare-preamble
  :cite-styles
  '((("author" "a") ("caps" "c") ("full" "f") ("caps-full" "cf"))
    (("locators" "l") ("bare" "b") ("caps" "c") ("bare-caps" "bc"))
    (("noauthor" "na"))
    (("nocite" "n"))
    (("text" "t") ("caps" "c"))
    (("nil") ("bare" "b") ("caps" "c") ("bare-caps" "bc"))))

(provide 'oc-biblatex)
;;; oc-biblatex.el ends here
