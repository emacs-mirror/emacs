;;; oc-csl.el --- csl citation processor for Org -*- lexical-binding: t; -*-

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

;; This library registers the `csl' citation processor, which provides
;; the "export" capability for citations.

;; The processor relies on the external Citeproc Emacs library, which must be
;; available prior to loading this library.

;; By default, citations are rendered in Chicago author-date CSL style.  You can
;; use another style file by specifying it in `org-cite-export-processors' or
;; from within the document by adding the file name to "cite_export" keyword
;;
;;    #+cite_export: csl /path/to/style-file.csl
;;    #+cite_export: csl "/path/to/style-file.csl"
;;
;; With the variable `org-cite-csl-styles-dir' set appropriately, the
;; above can even be shortened to
;;
;;     #+cite_export: csl style-file.csl
;;
;; Styles can be downloaded, for instance, from the Zotero Style Repository
;; (<https://www.zotero.org/styles>).  Dependent styles (which are not "unique"
;; in the Zotero Style Repository terminology) are not supported.

;; The processor uses the "en-US" CSL locale file shipped with Org for rendering
;; localized dates and terms in the references, independently of the language
;; settings of the Org document.  Additional CSL locales can be made available
;; by setting `org-cite-csl-locales-dir' to a directory containing the locale
;; files in question (see <https://github.com/citation-style-language/locales>
;; for such files).

;; Bibliography is defined with the "bibliography" keyword.  It supports files
;; with ".bib", ".bibtex", and ".json" extensions.  References are exported using
;; the "print_bibliography" keyword.

;; The library supports the following citation styles:
;;
;; - author (a), including caps (c), full (f), and caps-full (cf) variants,
;; - noauthor (na), including bare (b), caps (c) and bare-caps (bc) variants,
;; - year (y), including a bare (b) variant,
;; - text (t). including caps (c), full (f), and caps-full (cf) variants,
;; - default style, including bare (b), caps (c) and bare-caps (bc) variants.

;; CSL styles recognize "locator" in citation references' suffix.  For example,
;; in the citation
;;
;;     [cite:see @Tarski-1965 chapter 1, for an example]
;;
;; "chapter 1" is the locator.  The whole citation is rendered as
;;
;;     (see Tarski 1965, chap. 1 for an example)
;;
;; in the default CSL style.
;;
;; The locator starts with a locator term, among "bk.", "bks.", "book", "chap.",
;; "chaps.", "chapter", "col.", "cols.", "column", "figure", "fig.", "figs.",
;; "folio", "fol.", "fols.", "number", "no.", "nos.", "line", "l.", "ll.",
;; "note", "n.", "nn.", "opus", "op.", "opp.", "page", "p.", "pp.", "paragraph",
;; "para.", "paras.", "¶", "¶¶", "§", "§§", "part", "pt.", "pts.", "section",
;; "sec.", "secs.", "sub verbo", "s.v.", "s.vv.", "verse", "v.", "vv.",
;; "volume", "vol.", and "vols.".  It ends with the last comma or digit in the
;; suffix, whichever comes last, or runs till the end of the suffix.
;;
;; The part of the suffix before the locator is appended to reference's prefix.
;; If no locator term is used, but a number is present, then "page" is assumed.

;; This library was heavily inspired by and borrows from András Simonyi's
;; Citeproc Org (<https://github.com/andras-simonyi/citeproc-org>) library.
;; Many thanks to him!

;;; Code:
(require 'bibtex)
(require 'json)
(require 'oc)

(require 'citeproc nil t)
(declare-function citeproc-style-cite-note "ext:citeproc")
(declare-function citeproc-proc-style "ext:citeproc")
(declare-function citeproc-bt-entry-to-csl "ext:citeproc")
(declare-function citeproc-locale-getter-from-dir "ext:citeproc")
(declare-function citeproc-create "ext:citeproc")
(declare-function citeproc-citation-create "ext:citeproc")
(declare-function citeproc-append-citations "ext:citeproc")
(declare-function citeproc-render-citations "ext:citeproc")
(declare-function citeproc-render-bib "ext:citeproc")
(declare-function citeproc-hash-itemgetter-from-any "ext:citeproc")

(declare-function org-element-interpret-data "org-element" (data))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-put-property "org-element" (element property value))

(declare-function org-export-data "org-export" (data info))
(declare-function org-export-derived-backend-p "org-export" (backend &rest backends))
(declare-function org-export-get-footnote-number "org-export" (footnote info &optional data body-first))


;;; Customization

;;;; Location of CSL directories
(defcustom org-cite-csl-locales-dir nil
  "Directory of CSL locale files.
If nil then only the fallback en-US locale will be available."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice
          (directory :tag "Locales directory")
          (const :tag "Use en-US locale only" nil))
  ;; It's not obvious to me that arbitrary locations are safe.
;;;  :safe #'string-or-null-p
  )

(defcustom org-cite-csl-styles-dir nil
  "Directory of CSL style files.
When non-nil, relative style file names are expanded relatively to this
directory.  This variable is ignored when style file is absolute."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice
          (directory :tag "Styles directory")
          (const :tag "Use absolute file names" nil))
  ;; It's not obvious to me that arbitrary locations are safe.
;;;  :safe #'string-or-null-p
  )

;;;; Citelinks
(defcustom org-cite-csl-link-cites t
  "When non-nil, link cites to references."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-cite-csl-no-citelinks-backends '(ascii)
  "List of export back-ends for which cite linking is disabled.
Cite linking for export back-ends derived from any of the back-ends listed here,
is also disabled."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(repeat symbol))

;;;; Output-specific variables
(defcustom org-cite-csl-html-hanging-indent "1.5em"
  "Size of hanging-indent for HTML output in valid CSS units."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'string
  :safe #'stringp)

(defcustom org-cite-csl-html-label-width-per-char "0.6em"
  "Character width in CSS units for calculating entry label widths.
Used only when `second-field-align' is activated by the used CSL style."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'string
  :safe #'stringp)

(defcustom org-cite-csl-latex-hanging-indent "1.5em"
  "Size of hanging-indent for LaTeX output in valid LaTeX units."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'string
  :safe #'stringp)


;;; Internal variables
(defconst org-cite-csl--etc-dir
  (let ((oc-root (file-name-directory (locate-library "oc"))))
    (cond
     ;; First check whether it looks like we're running from the main
     ;; Org repository.
     ((let ((csl-org (expand-file-name "../etc/csl/" oc-root)))
        (and (file-directory-p csl-org) csl-org)))
     ;; Next look for the directory alongside oc.el because package.el
     ;; and straight will put all of org-mode/lisp/ in org-mode/.
     ((let ((csl-pkg (expand-file-name "etc/csl/" oc-root)))
        (and (file-directory-p csl-pkg) csl-pkg)))
     ;; Finally fall back the location used by shared system installs
     ;; and when running directly from Emacs repository.
     (t
      (expand-file-name "org/csl/" data-directory))))
  "Directory containing CSL-related data files.")

(defconst org-cite-csl--fallback-locales-dir org-cite-csl--etc-dir
  "Fallback CSL locale files directory.")

(defconst org-cite-csl--fallback-style-file
  (expand-file-name "chicago-author-date.csl"
                    org-cite-csl--etc-dir)
  "Default CSL style file, or nil.
If nil then the Chicago author-date style is used as a fallback.")

(defconst org-cite-csl--label-alist
  '(("bk."       . "book")
    ("bks."      . "book")
    ("book"      . "book")
    ("chap."     . "chapter")
    ("chaps."    . "chapter")
    ("chapter"   . "chapter")
    ("col."      . "column")
    ("cols."     . "column")
    ("column"    . "column")
    ("figure"    . "figure")
    ("fig."      . "figure")
    ("figs."     . "figure")
    ("folio"     . "folio")
    ("fol."      . "folio")
    ("fols."     . "folio")
    ("number"    . "number")
    ("no."       . "number")
    ("nos."      . "number")
    ("line"      . "line")
    ("l."        . "line")
    ("ll."       . "line")
    ("note"      . "note")
    ("n."        . "note")
    ("nn."       . "note")
    ("opus"      . "opus")
    ("op."       . "opus")
    ("opp."      . "opus")
    ("page"      . "page")
    ("p"         . "page")
    ("p."        . "page")
    ("pp."       . "page")
    ("paragraph" . "paragraph")
    ("para."     . "paragraph")
    ("paras."    . "paragraph")
    ("¶"         . "paragraph")
    ("¶¶"        . "paragraph")
    ("part"      . "part")
    ("pt."       . "part")
    ("pts."      . "part")
    ("§"         . "section")
    ("§§"        . "section")
    ("section"   . "section")
    ("sec."      . "section")
    ("secs."     . "section")
    ("sub verbo" . "sub verbo")
    ("s.v."      . "sub verbo")
    ("s.vv."     . "sub verbo")
    ("verse"     . "verse")
    ("v."        . "verse")
    ("vv."       . "verse")
    ("volume"    . "volume")
    ("vol."      . "volume")
    ("vols."     . "volume"))
  "Alist mapping locator names to locators.")

(defconst org-cite-csl--label-regexp
  ;; Prior to Emacs-27.1 argument of `regexp' form must be a string literal.
  ;; It is the reason why `rx' is avoided here.
  (rx-to-string
   `(seq (or line-start space)
         (regexp ,(regexp-opt (mapcar #'car org-cite-csl--label-alist) t))
         (0+ digit)
         (or word-end line-end space " "))
   t)
  "Regexp matching a label in a citation reference suffix.
Label is in match group 1.")


;;; Internal functions
(defun org-cite-csl--barf-without-citeproc ()
  "Raise an error if Citeproc library is not loaded."
  (unless (featurep 'citeproc)
    (error "Citeproc library is not loaded")))

(defun org-cite-csl--note-style-p (info)
  "Non-nil when bibliography style implies wrapping citations in footnotes.
INFO is the export state, as a property list."
  (citeproc-style-cite-note
   (citeproc-proc-style
    (org-cite-csl--processor info))))

(defun org-cite-csl--create-structure-params (citation info)
  "Return citeproc structure creation params for CITATION object.
STYLE is the citation style, as a string or nil. INFO is the export state, as
a property list."
  (let ((style (org-cite-citation-style citation info)))
    (pcase style
      ;; "author" style.
      (`(,(or "author" "a") . ,variant)
       (pcase variant
	 ((or "caps" "c") '(:mode author-only :capitalize-first t))
	 ((or "full" "f") '(:mode author-only :ignore-et-al t))
	 ((or "caps-full" "cf") '(:mode author-only :capitalize-first t :ignore-et-al t))
	 (_ '(:mode author-only))))
      ;; "noauthor" style.
      (`(,(or "noauthor" "na") . ,variant)
       (pcase variant
	 ((or "bare" "b") '(:mode suppress-author :suppress-affixes t))
	 ((or "caps" "c") '(:mode suppress-author :capitalize-first t))
	 ((or "bare-caps" "bc")
          '(:mode suppress-author :suppress-affixes t :capitalize-first t))
	 (_ '(:mode suppress-author))))
      ;; "year" style.
      (`(,(or "year" "y") . ,variant)
       (pcase variant
	 ((or "bare" "b") '(:mode year-only :suppress-affixes t))
	 (_ '(:mode year-only))))
      ;; "text" style.
      (`(,(or "text" "t") . ,variant)
       (pcase variant
         ((or "caps" "c") '(:mode textual :capitalize-first t))
         ((or "full" "f") '(:mode textual :ignore-et-al t))
         ((or "caps-full" "cf") '(:mode textual :ignore-et-al t :capitalize-first t))
         (_ '(:mode textual))))
      ;; Default "nil" style.
      (`(,_ . ,variant)
       (pcase variant
         ((or "caps" "c") '(:capitalize-first t))
         ((or "bare" "b") '(:suppress-affixes t))
         ((or "bare-caps" "bc") '(:suppress-affixes t :capitalize-first t))
         (_  nil)))
      ;; This should not happen.
      (_ (error "Invalid style: %S" style)))))

(defun org-cite-csl--no-citelinks-p (info)
  "Non-nil when export BACKEND should not create cite-reference links."
  (or (not org-cite-csl-link-cites)
      (and org-cite-csl-no-citelinks-backends
           (apply #'org-export-derived-backend-p
                  (plist-get info :back-end)
                  org-cite-csl-no-citelinks-backends))
      ;; No references are being exported anyway.
      (not (org-element-map (plist-get info :parse-tree) 'keyword
             (lambda (k)
               (equal "PRINT_BIBLIOGRAPHY" (org-element-property :key k)))
             info t))))

(defun org-cite-csl--output-format (info)
  "Return expected Citeproc's output format.
INFO is the export state, as a property list.  The return value is a symbol
corresponding to one of the output formats supported by Citeproc: `html',
`latex', or `org'."
  (let ((backend (plist-get info :back-end)))
    (cond
     ((org-export-derived-backend-p backend 'html) 'html)
     ((org-export-derived-backend-p backend 'latex) 'latex)
     (t 'org))))

(defun org-cite-csl--style-file (info)
  "Return style file associated to current export process.

INFO is the export state, as a property list.

When file name is relative, expand it according to `org-cite-csl-styles-dir',
or raise an error if the variable is unset."
  (pcase (org-cite-bibliography-style info)
    ('nil org-cite-csl--fallback-style-file)
    ((and (pred file-name-absolute-p) file) file)
    ((and (guard org-cite-csl-styles-dir) file)
     (expand-file-name file org-cite-csl-styles-dir))
    (other
     (user-error "Cannot handle relative style file name: %S" other))))

(defun org-cite-csl--locale-getter ()
  "Return a locale getter.
The getter looks for locales in `org-cite-csl-locales-dir' directory.  If it
cannot find them, it retrieves the default \"en_US\" from
`org-cite-csl--fallback-locales-dir'."
  (lambda (loc)
    (or (and org-cite-csl-locales-dir
             (ignore-errors
               (funcall (citeproc-locale-getter-from-dir org-cite-csl-locales-dir)
                        loc)))
        (funcall (citeproc-locale-getter-from-dir
                  org-cite-csl--fallback-locales-dir)
                 loc))))

(defun org-cite-csl--processor (info)
  "Return Citeproc processor reading items from current bibliography.

INFO is the export state, as a property list.

Newly created processor is stored as the value of the `:cite-citeproc-processor'
property in INFO."
  (or (plist-get info :cite-citeproc-processor)
      (let* ((bibliography (plist-get info :bibliography))
             (locale (or (plist-get info :language) "en_US"))
             (processor
              (citeproc-create
               (org-cite-csl--style-file info)
               (citeproc-hash-itemgetter-from-any bibliography)
               (org-cite-csl--locale-getter)
               locale)))
        (plist-put info :cite-citeproc-processor processor)
        processor)))

(defun org-cite-csl--parse-reference (reference info)
  "Return Citeproc's structure associated to citation REFERENCE.

INFO is the export state, as a property list.

The result is a association list.  Keys are: `id', `prefix',`suffix',
`location', `locator' and `label'."
  (let (label location-start locator-start location locator prefix suffix)
    ;; Parse suffix.  Insert it in a temporary buffer to find
    ;; different parts: pre-label, label, locator, location (label +
    ;; locator), and suffix.
    (with-temp-buffer
      (save-excursion
        (insert (org-element-interpret-data
                 (org-element-property :suffix reference))))
      (cond
       ((re-search-forward org-cite-csl--label-regexp nil t)
        (setq location-start (match-beginning 0))
        (setq label (cdr (assoc (match-string 1) org-cite-csl--label-alist)))
        (goto-char (match-end 1))
        (skip-chars-forward "[:space:] ")
        (setq locator-start (point)))
       ((re-search-forward (rx digit) nil t)
        (setq location-start (match-beginning 0))
        (setq label "page")
        (setq locator-start location-start))
       (t
        (setq suffix (org-element-property :suffix reference))))
      ;; Find locator's end, and suffix, if any. To that effect, look
      ;; for the last comma or digit after label, whichever comes
      ;; last.
      (unless suffix
        (goto-char (point-max))
        (let ((re (rx (or "," (group digit)))))
          (when (re-search-backward re location-start t)
            (goto-char (or (match-end 1) (match-beginning 0)))
            (setq location (buffer-substring location-start (point)))
            (setq locator (org-trim (buffer-substring locator-start (point))))
            ;; Skip comma in suffix.
            (setq suffix
                  (org-cite-parse-objects
                   (buffer-substring (match-end 0) (point-max))
                   t)))))
      (setq prefix
            (org-cite-concat
             (org-element-property :prefix reference)
             (and location-start
                  (org-cite-parse-objects
                   (buffer-substring 1 location-start)
                   t)))))
    ;; Return value.
    (let ((export
           (lambda (data)
             (org-string-nw-p
              (org-trim
               ;; When Citeproc exports to Org syntax, avoid mix and
               ;; matching output formats by also generating Org
               ;; syntax for prefix and suffix.
               (if (eq 'org (org-cite-csl--output-format info))
                   (org-element-interpret-data data)
                 (org-export-data data info)))))))
      `((id . ,(org-element-property :key reference))
        (prefix . ,(funcall export prefix))
        (suffix . ,(funcall export suffix))
        (locator . ,locator)
        (label . ,label)
        (location . ,location)))))

(defun org-cite-csl--create-structure (citation info)
  "Create Citeproc structure for CITATION object.
INFO is the export state, as a property list."
  (let* ((cites (mapcar (lambda (r)
                          (org-cite-csl--parse-reference r info))
                        (org-cite-get-references citation)))
         (footnote (org-cite-inside-footnote-p citation)))
    ;; Global prefix is inserted in front of the prefix of the first
    ;; reference.
    (let ((global-prefix (org-element-property :prefix citation)))
      (when global-prefix
        (let* ((first (car cites))
               (prefix-item (assq 'prefix first)))
          (setcdr prefix-item
                  (concat (org-element-interpret-data global-prefix)
                          " "
                          (cdr prefix-item))))))
    ;; Global suffix is appended to the suffix of the last reference.
    (let ((global-suffix (org-element-property :suffix citation)))
      (when global-suffix
        (let* ((last (org-last cites))
               (suffix-item (assq 'suffix last)))
          (setcdr suffix-item
                  (concat (cdr suffix-item)
                          " "
                          (org-element-interpret-data global-suffix))))))
    ;; Check if CITATION needs wrapping, i.e., it should be wrapped in
    ;; a footnote, but isn't yet.
    (when (and (not footnote) (org-cite-csl--note-style-p info))
      (org-cite-adjust-note citation info)
      (setq footnote (org-cite-wrap-citation citation info)))
    ;; Return structure.
    (apply #'citeproc-citation-create
           `(:note-index
             ,(and footnote (org-export-get-footnote-number footnote info))
             :cites ,cites
             ,@(org-cite-csl--create-structure-params citation info)))))

(defun org-cite-csl--rendered-citations (info)
  "Return the rendered citations as an association list.

INFO is the export state, as a property list.

Return an alist (CITATION . OUTPUT) where CITATION object has been rendered as
OUTPUT using Citeproc."
  (or (plist-get info :cite-citeproc-rendered-citations)
      (let* ((citations (org-cite-list-citations info))
             (processor (org-cite-csl--processor info))
             (structures
              (mapcar (lambda (c) (org-cite-csl--create-structure c info))
                      citations)))
        (citeproc-append-citations structures processor)
        (let* ((rendered
                (citeproc-render-citations
                 processor
                 (org-cite-csl--output-format info)
                 (org-cite-csl--no-citelinks-p info)))
               (result (seq-mapn #'cons citations rendered)))
          (plist-put info :cite-citeproc-rendered-citations result)
          result))))


;;; Export capability
(defun org-cite-csl-render-citation (citation _style _backend info)
  "Export CITATION object.
INFO is the export state, as a property list."
  (org-cite-csl--barf-without-citeproc)
  (let ((output (cdr (assq citation (org-cite-csl--rendered-citations info)))))
    (if (not (eq 'org (org-cite-csl--output-format info)))
        output
      ;; Parse Org output to re-export it during the regular export
      ;; process.
      (org-cite-parse-objects output))))

(defun org-cite-csl-render-bibliography (_keys _files _style _props _backend info)
  "Export bibliography.
INFO is the export state, as a property list."
  (org-cite-csl--barf-without-citeproc)
  (pcase-let* ((format (org-cite-csl--output-format info))
               (`(,output . ,parameters)
                (citeproc-render-bib
                 (org-cite-csl--processor info)
                 format
                 (org-cite-csl--no-citelinks-p info))))
    (pcase format
      ('html
       (concat
        (and (cdr (assq 'second-field-align parameters))
             (let* ((max-offset (cdr (assq 'max-offset parameters)))
                    (char-width
                     (string-to-number org-cite-csl-html-label-width-per-char))
                    (char-width-unit
                     (progn
                       (string-match (number-to-string char-width)
                                     org-cite-csl-html-label-width-per-char)
                       (substring org-cite-csl-html-label-width-per-char
                                  (match-end 0)))))
               (format
                "<style>.csl-left-margin{float: left; padding-right: 0em;}
 .csl-right-inline{margin: 0 0 0 %d%s;}</style>"
                (* max-offset char-width)
                char-width-unit)))
        (and (cdr (assq 'hanging-indent parameters))
             (format
              "<style>.csl-entry{text-indent: -%s; margin-left: %s;}</style>"
              org-cite-csl-html-hanging-indent
              org-cite-csl-html-hanging-indent))
        output))
      ('latex
       (if (cdr (assq 'hanging-indent parameters))
           (format "\\begin{hangparas}{%s}{1}\n%s\n\\end{hangparas}"
                   org-cite-csl-latex-hanging-indent
                   output)
         output))
      (_
       ;; Parse Org output to re-export it during the regular export
       ;; process.
       (org-cite-parse-elements output)))))

(defun org-cite-csl-finalizer (output _keys _files _style _backend info)
  "Add \"hanging\" package if missing from LaTeX output.
OUTPUT is the export document, as a string.  INFO is the export state, as a
property list."
  (org-cite-csl--barf-without-citeproc)
  (if (not (eq 'latex (org-cite-csl--output-format info)))
      output
    (with-temp-buffer
      (save-excursion (insert output))
      (when (search-forward "\\begin{document}" nil t)
        ;; Ensure that \citeprocitem is defined for citeproc-el
        (insert "\\makeatletter\n\\newcommand{\\citeprocitem}[2]{\\hyper@linkstart{cite}{citeproc_bib_item_#1}#2\\hyper@linkend}\n\\makeatother\n\n")
        ;; Ensure there is a \usepackage{hanging} somewhere or add one.
        (goto-char (match-beginning 0))
        (let ((re (rx "\\usepackage" (opt "[" (*? nonl) "]") "{hanging}")))
          (unless (re-search-backward re nil t)
            (insert "\\usepackage[notquote]{hanging}\n"))))
      (buffer-string))))


;;; Register `csl' processor
(org-cite-register-processor 'csl
  :export-citation #'org-cite-csl-render-citation
  :export-bibliography #'org-cite-csl-render-bibliography
  :export-finalizer #'org-cite-csl-finalizer
  :cite-styles
  '((("author" "a") ("full" "f") ("caps" "c") ("caps-full" "cf"))
    (("noauthor" "na") ("bare" "b") ("caps" "c") ("bare-caps" "bc"))
    (("year" "y") ("bare" "b"))
    (("text" "t") ("caps" "c") ("full" "f") ("caps-full" "cf"))
    (("nil") ("bare" "b") ("caps" "c") ("bare-caps" "bc"))))

(provide 'oc-csl)
;;; oc-csl.el ends here
