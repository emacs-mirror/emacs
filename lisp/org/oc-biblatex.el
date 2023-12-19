;;; oc-biblatex.el --- biblatex citation processor for Org  -*- lexical-binding: t; -*-

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

;; This library registers the `biblatex' citation processor, which provides
;; the "export" capability for citations.

;; The processor relies on "biblatex" LaTeX package.  As such it ensures that
;; the package is properly required in the document's preamble.  More
;; accurately, it will reuse any "\usepackage{biblatex}" already present in
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
;; - noauthor (na), including bare (b) variant,
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
(org-assert-version)

(require 'map)
(require 'org-macs)
(require 'oc)

(declare-function org-element-property "org-element" (property element))
(declare-function org-export-data "org-export" (data info))


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

(defcustom org-cite-biblatex-styles
  '(("author"   "caps"      "Citeauthor*" nil         nil)
    ("author"   "full"      "citeauthor"  nil         nil)
    ("author"   "caps-full" "Citeauthor"  nil         nil)
    ("author"   nil         "citeauthor*" nil         nil)
    ("locators" "bare"      "notecite"    nil         nil)
    ("locators" "caps"      "Pnotecite"   nil         nil)
    ("locators" "bare-caps" "Notecite"    nil         nil)
    ("locators" nil         "pnotecite"   nil         nil)
    ("noauthor" "bare"      "cite*"       nil         nil)
    ("noauthor" nil         "autocite*"   nil         nil)
    ("nocite"   nil         "nocite"      nil         t)
    ("text"     "caps"      "Textcite"    "Textcites" nil)
    ("text"     nil         "textcite"    "textcites" nil)
    (nil        "bare"      "cite"        "cites"     nil)
    (nil        "caps"      "Autocite"    "Autocites" nil)
    (nil        "bare-caps" "Cite"        "Cites"     nil)
    (nil        nil         "autocite"    "autocites" nil))
  "List of styles and variants, with associated BibLaTeX commands.

Each style follows the pattern

  (NAME VARIANT COMMAND MULTI-COMMAND NO-OPTION)

where:

  NAME is the name of the style, as a string, or nil.  The nil
  style is the default style.  As such, it must have an entry in
  the list.

  VARIANT is the name of the style variant, as a string or nil.
  The nil variant is the default variant for the current style.
  As such, each style name must be associated to a nil variant.

  COMMAND is the LaTeX command to use, as a string.  It should
  not contain the leading backslash character.

  MULTI-COMMAND is the LaTeX command to use when a multi-cite
  command is appropriate.  When nil, the style is deemed
  inappropriate for multi-cites.  The command should not contain
  the leading backslash character.

  NO-OPTION is a boolean.  When non-nil, no optional argument
  should be added to the LaTeX command.

Each NAME-VARIANT pair should be unique in the list.

It is also possible to provide shortcuts for style and variant
names.  See `org-cite-biblatex-style-shortcuts'."
  :group 'org-cite
  :package-version '(Org . "9.6")
  :type '(repeat
          (list :tag "Style/variant combination"
                ;; Name part.
                (choice :tag "Style"
                        (string :tag "Name")
                        (const :tag "Default style" nil))
                ;; Variant part.
                (choice :tag "Variant"
                        (string :tag "Name")
                        (const :tag "Default variant" nil))
                ;; Command part.
                (string :tag "Command name")
                (choice :tag "Multicite command"
                        (string :tag "Command name")
                        (const :tag "No multicite support" nil))
                (choice :tag "Skip optional arguments"
                        (const :tag "Yes" t)
                        (const :tag "No" nil)))))

(defcustom org-cite-biblatex-style-shortcuts
  '(("a"  . "author")
    ("b"  . "bare")
    ("bc" . "bare-caps")
    ("c"  . "caps")
    ("cf" . "caps-full")
    ("f"  . "full")
    ("l"  . "locators")
    ("n"  . "nocite")
    ("na" . "noauthor")
    ("t"  . "text"))
  "List of shortcuts associated to style or variant names.

Each entry is a pair (NAME . STYLE-NAME) where NAME is the name
of the shortcut, as a string, and STYLE-NAME is the name of
a style in `org-cite-biblatex-styles'."
  :group 'org-cite
  :package-version '(Org . "9.6")
  :type '(repeat
          (cons :tag "Shortcut"
                (string :tag "Name")
                (string :tag "Full name")))
  :safe t)


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
                       ""))))

(defun org-cite-biblatex--command (citation info name &optional multi no-opt)
  "Return BibLaTeX command NAME for CITATION object.

INFO is the export state, as a property list.

When optional argument MULTI is non-nil, use it as a multicite
command name when appropriate.  When optional argument NO-OPT is
non-nil, do not add optional arguments to the command."
  (if (and multi (org-cite-biblatex--multicite-p citation))
      (format "\\%s%s" multi (org-cite-biblatex--multi-arguments citation info))
    (format "\\%s%s"
            name
            (org-cite-biblatex--atomic-arguments
             (org-cite-get-references citation) info no-opt))))

(defun org-cite-biblatex--expand-shortcuts (style)
  "Return STYLE pair with shortcuts expanded."
  (pcase style
    (`(,style . ,variant)
     (cons (or (alist-get style org-cite-biblatex-style-shortcuts
                          nil nil #'equal)
               style)
           (or (alist-get variant org-cite-biblatex-style-shortcuts
                          nil nil #'equal)
               variant)))
    (_ (error "This should not happen"))))

(defun org-cite-biblatex-list-styles ()
  "List styles and variants supported in `biblatex' citation processor.
The output format is appropriate as a value for `:cite-styles' keyword
in `org-cite-register-processor', which see."
  (let ((shortcuts (make-hash-table :test #'equal))
        (variants (make-hash-table :test #'equal)))
    (pcase-dolist (`(,name . ,full-name) org-cite-biblatex-style-shortcuts)
      (push name (gethash full-name shortcuts)))
    (pcase-dolist (`(,name ,variant . ,_) org-cite-biblatex-styles)
      (unless (null variant) (push variant (gethash name variants))))
    (map-apply (lambda (style-name variants)
                 (cons (cons (or style-name "nil")
                             (gethash style-name shortcuts))
                       (mapcar (lambda (v)
                                 (cons v (gethash v shortcuts)))
                               variants)))
               variants)))


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
  (pcase-let* ((`(,name . ,variant) (org-cite-biblatex--expand-shortcuts style))
               (candidates nil)
               (style-match-flag nil))
    (catch :match
      ;; Walk `org-cite-biblatex-styles' and prioritize matching
      ;; candidates.  At the end of the process, the optimal candidate
      ;; should appear in front of CANDIDATES.
      (dolist (style org-cite-biblatex-styles)
        (pcase style
          ;; A matching style-variant pair trumps anything else.
          ;; Return it.
          (`(,(pred (equal name)) ,(pred (equal variant)) . ,_)
           (throw :match (setq candidates (list style))))
          ;; nil-nil style-variant is the fallback value.  Consider it
          ;; only if nothing else matches.
          (`(nil nil . ,_)
           (unless candidates (push style candidates)))
          ;; A matching style with default variant trumps a matching
          ;; variant without the adequate style.  Ensure the former
          ;; appears first in the list.
          (`(,(pred (equal name)) nil . ,_)
           (push style candidates)
           (setq style-match-flag t))
          (`(nil ,(pred (equal variant)) . ,_)
           (unless style-match-flag (push style candidates)))
          ;; Discard anything else.
          (_ nil))))
    (apply
     #'org-cite-biblatex--command citation info
     (pcase (seq-elt candidates 0) ;; `seq-first' is not available in Emacs 26.
       (`(,_ ,_ . ,command-parameters) command-parameters)
       ('nil
        (user-error
         "Missing default style or variant in `org-cite-biblatex-styles'"))
       (other
        (user-error "Invalid entry %S in `org-cite-biblatex-styles'" other))))))

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
  :cite-styles #'org-cite-biblatex-list-styles)

(provide 'oc-biblatex)
;;; oc-biblatex.el ends here
