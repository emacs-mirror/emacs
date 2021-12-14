;;; oc.el --- Org Cite library                  -*- lexical-binding: t; -*-

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

;; This library provides tooling to handle citations in Org, e.g,
;; activate, follow, insert, and export them, respectively called
;; "activate", "follow", "insert" and "export" capabilities.
;; Libraries responsible for providing some, or all, of these
;; capabilities are called "citation processors".

;; Such processors are defined using `org-cite-register-processor'.
;; Using this function, it is possible, in addition to giving it a
;; name, to attach functions associated to capabilities.  As such, a
;; processor handling citation export must set the `:export-citation'
;; property to an appropriate function.  Likewise, "activate"
;; capability requires an appropriate `:activate' property, "insert"
;; requires `:insert' property and, unsurprisingly, "follow"
;; capability implies `:follow' property.

;; As a user, the first thing to do is setting a bibliography, either
;; globally with `org-cite-global-bibliography', or locally using one
;; or more "bibliography" keywords.  Then one can select any
;; registered processor for each capability by providing a processor
;; name to the variables `org-cite-activate-processor' and
;; `org-cite-follow-processor'.

;; The "export" capability is slightly more involved as one need to
;; select the processor providing it, but may also provide a default
;; style for citations and bibliography.  Also, the choice of an
;; export processor may depend of the current export back-end.  The
;; association between export back-ends and triplets of parameters can
;; be set in `org-cite-export-processors' variable, or in a document,
;; through the "cite_export" keyword.

;; Eventually, this library provides some tools, mainly targeted at
;; processor implementors.  Most are export-specific and are located
;; in the "Tools only available during export" and "Tools generating
;; or operating on parsed data" sections.

;; The few others can be used directly from an Org buffer, or operate
;; on processors.  See "Generic tools" section.

;;; Code:

(require 'org-compat)
(require 'org-macs)
(require 'seq)

(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-collect-keywords "org" (keywords &optional unique directory))

(declare-function org-element-adopt-elements "org-element" (parent &rest children))
(declare-function org-element-citation-parser "org-element" ())
(declare-function org-element-citation-reference-parser "org-element" ())
(declare-function org-element-class "org-element" (datum &optional parent))
(declare-function org-element-contents "org-element" (element))
(declare-function org-element-create "org-element" (type &optional props &rest children))
(declare-function org-element-extract-element "org-element" (element))
(declare-function org-element-insert-before "org-element" (element location))
(declare-function org-element-lineage "org-element" (datum &optional types with-self))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only))
(declare-function org-element-parse-secondary-string "org-element" (string restriction &optional parent))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-put-property "org-element" (element property value))
(declare-function org-element-restriction "org-element" (element))
(declare-function org-element-set-element "org-element" (old new))
(declare-function org-element-type "org-element" (element))

(declare-function org-export-derived-backend-p "org-export" (backend &rest backends))
(declare-function org-export-get-next-element "org-export" (blob info &optional n))
(declare-function org-export-get-previous-element "org-export" (blob info &optional n))
(declare-function org-export-raw-string "org-export" (s))

(defvar org-complex-heading-regexp)
(defvar org-element-all-objects)
(defvar org-element-citation-key-re)
(defvar org-element-citation-prefix-re)
(defvar org-element-parsed-keywords)


;;; Constants
;; Borrowed from "citeproc.el" library.
(defconst org-cite--default-region-alist
  '(("af" . "za") ("ca" . "ad") ("cs" . "cz") ("cy" . "gb")
    ("da" . "dk") ("el" . "gr") ("et" . "ee") ("fa" . "ir")
    ("he" . "ir") ("ja" . "jp") ("km" . "kh") ("ko" . "kr")
    ("nb" . "no") ("nn" . "no") ("sl" . "si") ("sr" . "rs")
    ("sv" . "se") ("uk" . "ua") ("vi" . "vn") ("zh" . "cn"))
  "Alist mapping those languages to their default region.
Only those languages are given for which the default region is not simply the
result of duplicating the language part.")


;;; Configuration variables
(defgroup org-cite nil
  "Options concerning citations in Org mode."
  :group 'org
  :tag "Org Cite")

(defcustom org-cite-global-bibliography nil
  "List of bibliography files available in all documents.
File names must be absolute."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No global bibliography" nil)
		 (repeat :tag "List of bibliography files"
                         (file :tag "Bibliography"))))

(defcustom org-cite-activate-processor 'basic
  "Processor used for activating citations, as a symbol."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "Default fontification" nil)
                 (symbol :tag "Citation processor")))

(defcustom org-cite-export-processors '((t basic))
  "Processor used for exporting citations, as a triplet, or nil.

When nil, citations and bibliography are not exported.

When non-nil, the value is an association list between export back-ends and
citation export processors:

  (BACK-END . PROCESSOR)

where BACK-END is the name of an export back-end or t, and PROCESSOR is a
triplet following the pattern

  (NAME BIBLIOGRAPHY-STYLE CITATION-STYLE)

There, NAME is the name of a registered citation processor providing export
functionality, as a symbol.  BIBLIOGRAPHY-STYLE (respectively CITATION-STYLE)
is the desired default style to use when printing a bibliography (respectively
exporting a citation), as a string or nil.  Both BIBLIOGRAPHY-STYLE and
CITATION-STYLE are optional.  NAME is mandatory.

The export process selects the citation processor associated to the current
export back-end, or the most specific back-end the current one is derived from,
or, if all are inadequate, to the processor associated to t.  For example, with
the following value

  ((beamer natbib)
   (latex biblatex)
   (t csl))

exporting with `beamer' or any back-end derived from it will use `natbib',
whereas exporting with `latex' or any back-end derived from it but different
from `beamer' will use `biblatex' processor.  Any other back-end, such as
`html', will use `csl' processor.

CITATION-STYLE is overridden by adding a style to any citation object.  A nil
style lets the export processor choose the default output.  Any style not
recognized by the export processor is equivalent to nil.

The citation triplet can also be set with the CITE_EXPORT keyword.
E.g.,

  #+CITE_EXPORT: basic note numeric

or

  #+CITE_EXPORT: basic

In that case, `basic' processor is used on every export, independently on the
back-end."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No export" nil)
                 (alist :key-type symbol
                        :value-type
                        (list :tag "Citation processor"
                              (symbol :tag "Processor name")
                              (choice
                               (const :tag "Default bibliography style" nil)
                               (string :tag "Use specific bibliography style"))
                              (choice
                               (const :tag "Default citation style" nil)
                               (string :tag "Use specific citation style"))))))

(defcustom org-cite-follow-processor 'basic
  "Processor used for following citations, as a symbol."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No following" nil)
                 (symbol :tag "Citation processor")))

(defcustom org-cite-insert-processor 'basic
  "Processor used for inserting citations, as a symbol."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No insertion" nil)
                 (symbol :tag "Citation processor")))

(defcustom org-cite-adjust-note-numbers t
  "When non-nil, allow process to modify location of note numbers.

When this variable is non-nil, it is possible to swap between author-date and
note style without modifying the document.  To that effect, citations should
always be located as in an author-date style.  Prior to turning the citation
into a footnote, the citation processor moves the citation (i.e., the future
note number), and the surrounding punctuation, according to rules defined in
`org-cite-note-rules'.

When nil, the note number is not moved."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "Automatic note number location" t)
                 (const :tag "Place note numbers manually" nil))
  :safe #'booleanp)

(defcustom org-cite-note-rules
  '(("en-us" inside outside after)
    ("fr" adaptive same before))
  "Alist between languages and typographic rules for citations in note style.

When `org-cite-adjust-note-numbers' is non-nil, and note style is requested,
citation processor is allowed to move the note marker according to some specific
rules, detailed here.  More accurately, a rule is a list following the pattern

    (LANGUAGE-TAG . RULE)

  LANGUAGE-TAG is a down-cased string representing a language tag as defined in
  RFC 4646.  It may constituted of a language and a region separated with an
  hyphen (e.g., \"en-us\"), or the language alone (e.g., \"fr\").  A language
  without a region applies to all regions.

  RULE is a triplet

    (PUNCTUATION NUMBER ORDER)

  PUNCTUATION is the desired location of the punctuation with regards to the
  quotation, if any.  It may be `inside', `outside', or `adaptive'.  The latter
  permits subtler control over the punctuation: when there is no space between
  the quotation mark and the punctuation, it is equivalent to `inside'.
  Otherwise, it means `outside', as illustrated in the following examples:

      \"A quotation ending without punctuation\" [cite:@org21].
      \"A quotation ending with a period\"[cite:@org21].

  Notwithstanding the above, a space always appear before the citation when it
  is to become anything else than a note.

  NUMBER is the desired location of the note number with regards to the
  quotation mark, if any.  It may be `inside', `outside', or `same'.  When set
  to `same', the number appears on the same side as the punctuation, unless
  there is punctuation on both sides or on none.

  ORDER is the relative position of the citation with regards to the closest
  punctuation.  It may be `after' or `before'.

For example (adaptive same before) corresponds to French typography.

When the locale is unknown to this variable, the default rule is:

  (adaptive outside after)

This roughly follows the Oxford Guide to Style recommendations."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type
  '(repeat
    (list :tag "Typographic rule"
          (string :tag "Language code")
          (choice :tag "Location of punctuation"
                  (const :tag "Punctuation inside quotation" inside)
                  (const :tag "Punctuation outside quotation" outside)
                  (const :tag "Location depends on spacing" adaptive))
          (choice :tag "Location of citation"
                  (const :tag "Citation inside quotation" inside)
                  (const :tag "Citation outside quotation" outside)
                  (const :tag "Citation next to punctuation" same))
          (choice :tag "Order of citation and punctuation"
                  (const :tag "Citation first" before)
                  (const :tag "Citation last" after)))))

(defcustom org-cite-punctuation-marks '("." "," ";" ":" "!" "?")
  "List of strings that can be moved around when placing note numbers.

When `org-cite-adjust-note-numbers' is non-nil, the citation processor is
allowed to shuffle punctuation marks specified in this list in order to
place note numbers according to rules defined in `org-cite-note-rules'."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(repeat string))


;;; Citation processors
(cl-defstruct (org-cite-processor (:constructor org-cite--make-processor)
				  (:copier nil))
  (name nil :read-only t)
  (activate nil :read-only t)
  (cite-styles nil :read-only t)
  (export-bibliography nil :read-only t)
  (export-citation nil :read-only t)
  (export-finalizer nil :read-only t)
  (follow nil :read-only t)
  (insert nil :read-only t))

(defvar org-cite--processors nil
  "List of registered citation processors.
See `org-cite-register-processor' for more information about
processors.")

(defun org-cite--get-processor (name)
  "Return citation processor named after symbol NAME.
Return nil if no such processor is found."
  (seq-find (lambda (p) (eq name (org-cite-processor-name p)))
	    org-cite--processors))

(defun org-cite-register-processor (name &rest body)
  "Mark citation processor NAME as available.

NAME is a symbol.  BODY is a property list, where the following
optional keys can be set:

  `:activate'

    Function activating a citation.  It is called with a single
    argument: a citation object extracted from the current
    buffer.  It may add text properties to the buffer.  If it is
    not provided, `org-cite-fontify-default' is used.

  `:export-bibliography'

    Function rendering a bibliography.  It is called with six
    arguments: the list of citation keys used in the document, as
    strings, a list of bibliography files, the style, as a string
    or nil, the local properties, as a property list, the export
    back-end, as a symbol, and the communication channel, as a
    property list.

    It is called at each \"print_bibliography\" keyword in the
    parse tree.  It may return a string, a parsed element, a list
    of parsed elements, or nil.  When it returns nil, the keyword
    is ignored.  Otherwise, the value it returns replaces the
    keyword in the export output.

  `:export-citation'    (mandatory for \"export\" capability)

    Function rendering citations.  It is called with four
    arguments: a citation object, the style, as a pair, the
    export back-end, as a symbol, and the communication channel,
    as a property list.

    It is called on each citation object in the parse tree.  It
    may return a string, a parsed object, a secondary string, or
    nil.  When it returns nil, the citation is ignored.
    Otherwise, the value it returns replaces the citation object
    in the export output.

  `:export-finalizer'

    Function called at the end of export process.  It must accept
    six arguments: the output, as a string, a list of citation
    keys used in the document, a list of bibliography files, the
    expected bibliography style, as a string or nil, the export
    back-end, as a symbol, and the communication channel, as a
    property list.

    It must return a string, which will become the final output
    from the export process, barring subsequent modifications
    from export filters.

  `:follow'

    Function called to follow a citation.  It accepts two
    arguments, the citation or citation reference object at
    point, and any prefix argument received during interactive
    call of `org-open-at-point'.

  `:insert'

    Function called to insert a citation.  It accepts two
    arguments, the citation or citation reference object at point
    or nil, and any prefix argument received.

  `:cite-styles'

    When the processor has export capability, the value can
    specify what cite styles, variants, and their associated
    shortcuts are supported.  It can be useful information for
    completion or linting.

    The expected format is

      ((STYLE . SHORTCUTS) . VARIANTS))

    where STYLE is a string, SHORTCUTS a list of strings or nil,
    and VARIANTS is a list of pairs (VARIANT . SHORTCUTS),
    VARIANT being a string and SHORTCUTS a list of strings or
    nil.

    The \"nil\" style denotes the processor fall-back style.  It
    should have a corresponding entry in the value.

Return a non-nil value on a successful operation."
  (declare (indent 1))
  (unless (and name (symbolp name))
    (error "Invalid processor name: %S" name))
  (when (org-cite--get-processor name)
    (org-cite-unregister-processor name))
  (push (apply #'org-cite--make-processor :name name body)
	org-cite--processors))

(defun org-cite-unregister-processor (name)
  "Unregister citation processor NAME.
NAME is a symbol.  Raise an error if processor is not registered.
Return a non-nil value on a successful operation."
  (unless (and name (symbolp name))
    (error "Invalid processor name: %S" name))
  (pcase (org-cite--get-processor name)
    ('nil (error "Processor %S not registered" name))
    (processor
     (setq org-cite--processors (delete processor org-cite--processors))))
  t)

(defun org-cite-processor-has-capability-p (processor capability)
  "Return non-nil if PROCESSOR is able to handle CAPABILITY.
PROCESSOR is the name of a cite processor, as a symbol.  CAPABILITY is
`activate', `export', `follow', or `insert'."
  (let ((p (org-cite--get-processor processor)))
    (pcase capability
      ((guard (not p)) nil)             ;undefined processor
      ('activate (functionp (org-cite-processor-activate p)))
      ('export (functionp (org-cite-processor-export-citation p)))
      ('follow (functionp (org-cite-processor-follow p)))
      ('insert (functionp (org-cite-processor-insert p)))
      (other (error "Invalid capability: %S" other)))))


;;; Internal functions
(defun org-cite--set-post-blank (datum blanks)
  "Set `:post-blank' property from element or object before DATUM to BLANKS.
DATUM is an element or object.  BLANKS is an integer.  DATUM is modified
by side-effect."
  (if (not (eq 'plain-text (org-element-type datum)))
      (org-element-put-property datum :post-blank blanks)
    ;; Remove any blank from string before DATUM so it is exported
    ;; with exactly BLANKS white spaces.
    (org-element-set-element
     datum
     (replace-regexp-in-string
      "[ \t\n]*\\'" (make-string blanks ?\s) datum))))

(defun org-cite--set-previous-post-blank (datum blanks info)
  "Set `:post-blank' property from element or object before DATUM to BLANKS.
DATUM is an element or object.  BLANKS is an integer.  INFO is the export
state, as a property list.  Previous element or object, if any, is modified by
side-effect."
  (let ((previous (org-export-get-previous-element datum info)))
    (when previous
      (org-cite--set-post-blank previous blanks))))

(defun org-cite--insert-at-split (s citation n regexp)
  "Split string S and insert CITATION object between the two parts.
S is split at beginning of match group N upon matching REGEXP against it.
This function assumes S precedes CITATION."
  ;; When extracting the citation, remove white spaces before it, but
  ;; preserve those after it.
  (let ((post-blank (org-element-property :post-blank citation)))
    (when (and post-blank (> post-blank 0))
      (org-element-insert-before (make-string post-blank ?\s) citation)))
  (org-element-insert-before
   (org-element-put-property (org-element-extract-element citation)
                             :post-blank 0)
   s)
  (string-match regexp s)
  (let* ((split (match-beginning n))
         (first-part (substring s nil split))
         ;; Remove trailing white spaces as they are before the
         ;; citation.
         (last-part
          (replace-regexp-in-string (rx (1+ (any blank ?\n)) string-end)
                                    ""
                                    (substring s split))))
    (when (org-string-nw-p first-part)
      (org-element-insert-before first-part citation))
    (org-element-set-element s last-part)))

(defun org-cite--move-punct-before (punct citation s info)
  "Move punctuation PUNCT before CITATION object.
String S contains PUNCT.  INFO is the export state, as a property list.
The function assumes S follows CITATION.  Parse tree is modified by side-effect."
  (if (equal s punct)
      (org-element-extract-element s)   ;it would be empty anyway
    (org-element-set-element s (substring s (length punct))))
  ;; Remove blanks before citation.
  (org-cite--set-previous-post-blank citation 0 info)
  (org-element-insert-before
   ;; Blanks between citation and punct are now before punct and
   ;; citation.
   (concat (make-string (or (org-element-property :post-blank citation) 0) ?\s)
           punct)
   citation))

(defun org-cite--parse-as-plist (s)
  "Parse string S as a property list.
Values are always strings.  Return nil if S is nil."
  (cond
   ((null s) nil)
   ((stringp s)
    (with-temp-buffer
      (save-excursion (insert s))
      (skip-chars-forward " \t")
      (let ((results nil)
            (value-flag nil))
        (while (not (eobp))
          (pcase (char-after)
            (?:
             (push (read (current-buffer)) results)
             (setq value-flag t))
            ((guard (not value-flag))
             (skip-chars-forward "^ \t"))
            (?\"
             (let ((origin (point)))
               (condition-case _
                   (progn
                     (read (current-buffer))
                     (push (buffer-substring (1+ origin) (1- (point))) results))
                 (end-of-file
                  (goto-char origin)
                  (skip-chars-forward "^ \t")
                  (push (buffer-substring origin (point)) results)))
               (setq value-flag nil)))
            (_
             (let ((origin (point)))
               (skip-chars-forward "^ \t")
               (push (buffer-substring origin (point)) results)
               (setq value-flag nil))))
          (skip-chars-forward " \t"))
        (nreverse results))))
   (t (error "Invalid argument type: %S" s))))

(defun org-cite--get-note-rule (info)
  "Return punctuation rule according to language used for export.

INFO is the export state, as a property list.

Rule is found according to the language used for export and
`org-cite-note-rules', which see.

If there is no rule matching current language, the rule defaults
to (adaptive outside after)."
  (let* ((language-tags
          ;; Normalize language as a language-region tag, as described
          ;; in RFC 4646.
          (pcase (split-string (plist-get info :language) "[-_]")
            (`(,language)
             (list language
                   (or (cdr (assoc language org-cite--default-region-alist))
                       language)))
            (`(,language ,region)
             (list language region))
            (other
             (error "Invalid language identifier: %S" other))))
         (language-region (mapconcat #'downcase language-tags "-"))
         (language (car language-tags)))
    (or (cdr (assoc language-region org-cite-note-rules))
        (cdr (assoc language org-cite-note-rules))
        '(adaptive outside after))))


;;; Generic tools
(defun org-cite-list-bibliography-files ()
  "List all bibliography files defined in the buffer."
  (delete-dups
   (append (mapcar (lambda (value)
		     (pcase value
		       (`(,f . ,d)
                        (expand-file-name (org-strip-quotes f) d))))
		   (pcase (org-collect-keywords
                           '("BIBLIOGRAPHY") nil '("BIBLIOGRAPHY"))
		     (`(("BIBLIOGRAPHY" . ,pairs)) pairs)))
	   org-cite-global-bibliography)))

(defun org-cite-get-references (citation &optional keys-only)
  "Return citations references contained in CITATION object.

When optional argument KEYS-ONLY is non-nil, return the references' keys, as a
list of strings.

Assume CITATION object comes from either a full parse tree, e.g., during export,
or from the current buffer."
  (let ((contents (org-element-contents citation)))
    (cond
     ((null contents)
      (org-with-point-at (org-element-property :contents-begin citation)
        (narrow-to-region (point) (org-element-property :contents-end citation))
        (let ((references nil))
          (while (not (eobp))
            (let ((reference (org-element-citation-reference-parser)))
              (goto-char (org-element-property :end reference))
              (push (if keys-only
                        (org-element-property :key reference)
                      reference)
                    references)))
          (nreverse references))))
     (keys-only (mapcar (lambda (r) (org-element-property :key r)) contents))
     (t contents))))

(defun org-cite-boundaries (citation)
  "Return the beginning and end strict position of CITATION.
Returns a (BEG . END) pair."
  (let ((beg (org-element-property :begin citation))
	(end (org-with-point-at (org-element-property :end citation)
	       (skip-chars-backward " \t")
	       (point))))
    (cons beg end)))

(defun org-cite-key-boundaries (reference)
  "Return citation REFERENCE's key boundaries as buffer positions.
The function returns a pair (START . END) where START and END denote positions
in the current buffer.  Positions include leading \"@\" character."
  (org-with-point-at (org-element-property :begin reference)
    (let ((end (org-element-property :end reference)))
      (re-search-forward org-element-citation-key-re end t)
      (cons (match-beginning 0) (match-end 0)))))

(defun org-cite-main-affixes (citation)
  "Return main affixes for CITATION object.

Some export back-ends only support a single pair of affixes per
citation, even if it contains multiple keys.  This function
decides what affixes are the most appropriate.

Return a pair (PREFIX . SUFFIX) where PREFIX and SUFFIX are
parsed data."
  (let ((source
         ;; When there are multiple references, use global affixes.
         ;; Otherwise, local affixes have priority.
         (pcase (org-cite-get-references citation)
           (`(,reference) reference)
           (_ citation))))
    (cons (org-element-property :prefix source)
          (org-element-property :suffix source))))

(defun org-cite-supported-styles (&optional processors)
  "List of supported citation styles and variants.

Supported styles are those handled by export processors from
`org-cite-export-processors', or in PROCESSORS, as a list of symbols,
when non-nil.

Return value is a list with the following items:

  ((STYLE . SHORTCUTS) . VARIANTS))

where STYLE is a string, SHORTCUTS a list of strings, and VARIANTS is a list of
pairs (VARIANT . SHORTCUTS), VARIANT being a string and SHORTCUTS a list of
strings."
  (let ((collection
         (seq-mapcat
          (lambda (name)
            (org-cite-processor-cite-styles (org-cite--get-processor name)))
          (or processors
              (mapcar (pcase-lambda (`(,_ . (,name . ,_))) name)
                      org-cite-export-processors))))
        (result nil))
    ;; Merge duplicate styles.  Each style full name is guaranteed to
    ;; be unique, and associated to all shortcuts and all variants in
    ;; the initial collection.
    (pcase-dolist (`((,style . ,shortcuts) . ,variants) collection)
      (let ((entry (assoc style result)))
        (if (not entry)
            (push (list style shortcuts variants) result)
          (setf (nth 1 entry)
                (seq-uniq (append shortcuts (nth 1 entry))))
          (setf (nth 2 entry)
                (append variants (nth 2 entry))))))
    ;; Return value with the desired format.
    (nreverse
     (mapcar (pcase-lambda (`(,style ,shortcuts ,variants))
               (cons (cons style (nreverse shortcuts))
                     ;; Merge variant shortcuts.
                     (let ((result nil))
                       (pcase-dolist (`(,variant . ,shortcuts) variants)
                         (let ((entry (assoc variant result)))
                           (if (not entry)
                               (push (cons variant shortcuts) result)
                             (setf (cdr entry)
                                   (seq-uniq (append shortcuts (cdr entry)))))))
                       result)))
             result))))

(defun org-cite-delete-citation (datum)
  "Delete citation or citation reference DATUM.
When removing the last reference, also remove the whole citation."
  (pcase (org-element-type datum)
    ('citation
     (pcase-let* ((`(,begin . ,end) (org-cite-boundaries datum))
                  (pos-before-blank
                   (org-with-point-at begin
                     (skip-chars-backward " \t")
                     (point)))
                  (pos-after-blank (org-element-property :end datum))
                  (first-on-line?
                   (= pos-before-blank (line-beginning-position)))
                  (last-on-line?
                   (= pos-after-blank (line-end-position))))
       (cond
        ;; The citation is alone on its line.  Remove the whole line.
        ;; Do not leave it blank as it might break a surrounding
        ;; paragraph.
        ((and first-on-line? last-on-line?)
         (delete-region (line-beginning-position) (line-beginning-position 2)))
        ;; When the citation starts the line, preserve indentation.
        (first-on-line? (delete-region begin pos-after-blank))
        ;; When the citation ends the line, remove any trailing space.
        (last-on-line? (delete-region pos-before-blank (line-end-position)))
        ;; Otherwise, delete blanks before the citation.
        ;; Nevertheless, make sure there is at least one blank left,
        ;; so as to not splice unrelated surroundings.
        (t
         (delete-region pos-before-blank end)
         (when (= pos-after-blank end)
           (org-with-point-at pos-before-blank (insert " ")))))))
    ('citation-reference
     (let* ((citation (org-element-property :parent datum))
            (references (org-cite-get-references citation))
            (begin (org-element-property :begin datum))
            (end (org-element-property :end datum)))
       (cond
        ;; Single reference.
        ((= 1 (length references))
         (org-cite-delete-citation citation))
        ;; First reference, no prefix.
        ((and (= begin (org-element-property :contents-begin citation))
              (not (org-element-property :prefix citation)))
         (org-with-point-at (org-element-property :begin datum)
           (skip-chars-backward " \t")
           (delete-region (point) end)))
        ;; Last reference, no suffix.
        ((and (= end (org-element-property :contents-end citation))
              (not (org-element-property :suffix citation)))
         (delete-region (1- begin) (1- (cdr (org-cite-boundaries citation)))))
        ;; Somewhere in-between.
        (t
         (delete-region begin end)))))
    (other
     (error "Invalid object type: %S" other))))


;;; Tools only available during export
(defun org-cite-citation-style (citation info)
  "Return citation style used for CITATION object.

Style is a pair (NAME . VARIANT) where NAME and VARIANT are strings or nil.
A nil NAME means the default style for the current processor should be used.

INFO is a plist used as a communication channel."
  (let* ((separate
          (lambda (s)
            (cond
             ((null s) (cons nil nil))
             ((not (string-match "/" s)) (cons s nil))
             (t (cons (substring s nil (match-beginning 0))
                      (org-string-nw-p (substring s (match-end 0))))))))
         (local (funcall separate (org-element-property :style citation)))
         (global
          (funcall separate (pcase (plist-get info :cite-export)
                              (`(,_ ,_ ,style) style)
                              (_ nil)))))
    (cond
     ((org-string-nw-p (car local))
      (cons (org-not-nil (car local)) (cdr local)))
     (t
      (cons (org-not-nil (car global))
            (or (cdr local) (cdr global)))))))

(defun org-cite-bibliography-style (info)
  "Return expected bibliography style.
INFO is a plist used as a communication channel."
  (pcase (plist-get info :cite-export)
    (`(,_ ,style ,_) style)
    (_ nil)))

(defun org-cite-bibliography-properties (keyword)
  "Return properties associated to \"print_bibliography\" KEYWORD object.
Return value is a property list."
  (org-cite--parse-as-plist (org-element-property :value keyword)))

(defun org-cite-list-citations (info)
  "List citations in the exported document.
Citations are ordered by appearance in the document, when following footnotes.
INFO is the export communication channel, as a property list."
  (or (plist-get info :citations)
      (letrec ((cites nil)
               (tree (plist-get info :parse-tree))
               (find-definition
                ;; Find definition for standard reference LABEL.  At
                ;; this point, it is impossible to rely on
                ;; `org-export-get-footnote-definition' because the
                ;; function caches results that could contain
                ;; un-processed citation objects.  So we use
                ;; a simplified version of the function above.
                (lambda (label)
                  (org-element-map tree 'footnote-definition
                    (lambda (d)
                      (and (equal label (org-element-property :label d))
                           (or (org-element-contents d) "")))
                    info t)))
               (search-cites
                (lambda (data)
                  (org-element-map data '(citation footnote-reference)
                    (lambda (datum)
                      (pcase (org-element-type datum)
                        ('citation (push datum cites))
		        ;; Do not force entering inline definitions, since
		        ;; `org-element-map' is going to enter it anyway.
                        ((guard (eq 'inline (org-element-property :type datum))))
                        ;; Walk footnote definition.
                        (_
                         (let ((label (org-element-property :label datum)))
                           (funcall search-cites
                                    (funcall find-definition label))))))
                    info nil 'footnote-definition t))))
        (funcall search-cites tree)
        (let ((result (nreverse cites)))
          (plist-put info :citations result)
          result))))

(defun org-cite-list-keys (info)
  "List citation keys in the exported document.
Keys are ordered by first appearance in the document, when following footnotes.
Duplicate keys are removed.  INFO is the export communication channel, as a
property list."
  (delete-dups
   (org-element-map (org-cite-list-citations info) 'citation-reference
     (lambda (r) (org-element-property :key r))
     info)))

(defun org-cite-key-number (key info &optional predicate)
  "Return number associated to string KEY.

INFO is the export communication channel, as a property list.

Optional argument PREDICATE is called with two keys, and returns non-nil
if the first reference should sort before the second.  When nil, references
are sorted in order cited."
  (let* ((keys (org-cite-list-keys info))
         (sorted-keys (if (functionp predicate)
                          (sort keys predicate)
                        keys))
         (position (seq-position sorted-keys key #'string-equal)))
    (and (integerp position)
         (1+ position))))

(defun org-cite-inside-footnote-p (citation &optional strict)
  "Non-nil when CITATION object is contained within a footnote.

When optional argument STRICT is non-nil, return t only if CITATION represents
the sole contents of the footnote, e.g., after calling `org-cite-wrap-citation'.

When non-nil, the return value if the footnote container."
  (let ((footnote
         (org-element-lineage citation
                              '(footnote-definition footnote-reference))))
    (and footnote
         (or (not strict)
             (equal (org-element-contents (org-element-property :parent citation))
                    (list citation)))
         ;; Return value.
         footnote)))

(defun org-cite-wrap-citation (citation info)
  "Wrap an anonymous inline footnote around CITATION object in the parse tree.

INFO is the export state, as a property list.

White space before the citation, if any, are removed.  The parse tree is
modified by side-effect.

Return newly created footnote object."
  (let ((footnote
         (list 'footnote-reference
               (list :label nil
                     :type 'inline
                     :contents-begin (org-element-property :begin citation)
                     :contents-end (org-element-property :end citation)
                     :post-blank (org-element-property :post-blank citation)))))
    ;; Remove any white space before citation.
    (org-cite--set-previous-post-blank citation 0 info)
    ;; Footnote swallows citation.
    (org-element-insert-before footnote citation)
    (org-element-adopt-elements footnote
      (org-element-extract-element citation))))

(defun org-cite-adjust-note (citation info &optional rule punct)
  "Adjust note number location for CITATION object, and punctuation around it.

INFO is the export state, as a property list.

Optional argument RULE is the punctuation rule used, as a triplet.  When nil,
rule is determined according to `org-cite-note-rules', which see.

Optional argument PUNCT is a list of punctuation marks to be considered.
When nil, it defaults to `org-cite-punctuation-marks'.

Parse tree is modified by side-effect.

Note: when calling both `org-cite-adjust-note' and `org-cite-wrap-citation' on
the same object, call `org-cite-adjust-note' first."
  (when org-cite-adjust-note-numbers
    (pcase-let* ((rule (or rule (org-cite--get-note-rule info)))
                 (punct-re (regexp-opt (or punct org-cite-punctuation-marks)))
                 ;; with Emacs <27.1. Argument of `regexp' form (PUNCT-RE this case)
                 ;; must be a string literal.
                 (previous-punct-re
                  (rx-to-string `(seq (opt (group (regexp ,(rx (0+ (any blank ?\n))))
                                                  (regexp ,punct-re)))
                                      (regexp ,(rx (opt (0+ (any blank ?\n)) (group ?\"))
                                                   (opt (group (1+ (any blank ?\n))))
                                                   string-end)))
                                t))
                 (next-punct-re
                  (rx-to-string `(seq string-start
                                      (group (0+ (any blank ?\n)) (regexp ,punct-re)))
                                t))
                 (next (org-export-get-next-element citation info))
                 (final-punct
                  (and (stringp next)
                       (string-match next-punct-re next)
                       (match-string 1 next)))
                 (previous
                  ;; Find the closest terminal object.  Consider
                  ;; citation, subscript and superscript objects as
                  ;; terminal.
                  (org-last
                   (org-element-map (org-export-get-previous-element citation info)
                       '(citation code entity export-snippet footnote-reference
                                  line-break latex-fragment link plain-text
                                  radio-target statistics-cookie timestamp
                                  verbatim)
                     #'identity info nil '(citation subscript superscript))))
                 (`(,punct ,quote ,spacing)
                  (and (stringp previous)
                       (string-match previous-punct-re previous)
                       (list (match-string 1 previous)
                             (match-string 2 previous)
                             (match-string 3 previous)))))
      ;; Bail you when there is no quote and either no punctuation, or
      ;; punctuation on both sides.
      (when (or quote (org-xor punct final-punct))
        ;; Phase 1: handle punctuation rule.
        (pcase rule
          ((guard (not quote)) nil)
          ;; Move punctuation inside.
          (`(,(or `inside (and `adaptive (guard (not spacing)))) . ,_)
           ;; This only makes sense if there is a quotation before the
           ;; citation that does not end with some punctuation.
           (when (and (not punct) final-punct)
             ;; Quote guarantees there is a string object before
             ;; citation.  Likewise, any final punctuation guarantees
             ;; there is a string object following citation.
             (let ((new-prev
                    (replace-regexp-in-string
                     previous-punct-re
                     (concat final-punct "\"") previous nil nil 2))
                   (new-next
                    (replace-regexp-in-string
                     ;; Before Emacs-27.1 `literal' `rx' form with a variable
                     ;; as an argument is not available.
                     (rx-to-string `(seq string-start ,final-punct) t)
                     "" next)))
               (org-element-set-element previous new-prev)
               (org-element-set-element next new-next)
               (setq previous new-prev)
               (setq next new-next)
               (setq punct final-punct)
               (setq final-punct nil))))
          ;; Move punctuation outside.
          (`(,(or `outside (and `adaptive (guard spacing))) . ,_)
           ;; This is only meaningful if there is some inner
           ;; punctuation and no final punctuation already.
           (when (and punct (not final-punct))
             ;; Inner punctuation guarantees there is text object
             ;; before the citation.  However, there is no information
             ;; about the object following citation, if any.
             ;; Therefore, we handle all the possible cases (string,
             ;; other type, or none).
             (let ((new-prev
                    (replace-regexp-in-string
                     previous-punct-re "" previous nil nil 1))
                   (new-next (if (stringp next) (concat punct next) punct)))
               (org-element-set-element previous new-prev)
               (cond
                ((stringp next)
                 (org-element-set-element next new-next))
                (next
                 (org-element-insert-before new-next next))
                (t
                 (org-element-adopt-elements
                     (org-element-property :parent citation)
                   new-next)))
               (setq previous new-prev)
               (setq next new-next)
               (setq final-punct punct)
               (setq punct nil))))
          (_
           (error "Invalid punctuation rule: %S" rule))))
      ;; Phase 2: move citation to its appropriate location.
      ;;
      ;; First transform relative citation location into a definitive
      ;; location, according to the surrounding punctuation.
      (pcase rule
        (`(,punctuation same ,order)
         (setf rule
               (list punctuation
                     (cond
                      ;; When there is punctuation on both sides, the
                      ;; citation is necessarily on the outside.
                      ((and punct final-punct) 'outside)
                      (punct 'inside)
                      (final-punct 'outside)
                      ;; No punctuation: bail out on next step.
                      (t nil))
                     order))))
      (pcase rule
        (`(,_ nil ,_) nil)
        (`(,_ inside after)
         ;; Citation has to be moved after punct, if there is
         ;; a quotation mark, or after final punctuation.
         (cond
          (quote
           (org-cite--insert-at-split previous citation 2 previous-punct-re))
          (final-punct
           (org-cite--move-punct-before final-punct citation next info))
          ;; There is only punct, and we're already after it.
          (t nil)))
        (`(,_ inside before)
         ;; Citation is already behind final-punct, so only consider
         ;; other locations.
         (when (or punct quote)
           (org-cite--insert-at-split previous citation 0 previous-punct-re)))
        (`(,_ outside after)
         ;; Citation is already after any punct or quote.  It can only
         ;; move past final punctuation, if there is one.
         (when final-punct
           (org-cite--move-punct-before final-punct citation next info)))
        (`(,_ outside before)
         ;; The only non-trivial case is when citation follows punct
         ;; without a quote.
         (when (and punct (not quote))
           (org-cite--insert-at-split previous citation 0 previous-punct-re)))
        (_
         (error "Invalid punctuation rule: %S" rule))))))


;;; Tools generating or operating on parsed data
(defun org-cite-parse-elements (s)
  "Parse string S as a list of Org elements.

The return value is suitable as a replacement for a
\"print_bibliography\" keyword.  As a consequence, the function
raises an error if S contains a headline."
  (with-temp-buffer
    (insert s)
    (pcase (org-element-contents (org-element-parse-buffer))
      ('nil nil)
      (`(,(and section (guard (eq 'section (org-element-type section)))))
       (org-element-contents section))
      (_
       (error "Headlines cannot replace a keyword")))))

(defun org-cite-parse-objects (s &optional affix)
  "Parse string S as a secondary string.

The return value is suitable as a replacement for a citation object.

When optional argument AFFIX is non-nil, restrict the set of allowed object
types to match the contents of a citation affix."
  (org-element-parse-secondary-string
   s (org-element-restriction (if affix 'citation-reference 'paragraph))))

(defun org-cite-make-paragraph (&rest data)
  "Return a paragraph element containing DATA.
DATA are strings, objects or secondary strings."
  (apply #'org-element-create 'paragraph nil (apply #'org-cite-concat data)))

(defun org-cite-emphasize (type &rest data)
  "Apply emphasis TYPE on DATA.
TYPE is a symbol among `bold', `italic', `strike-through' and `underline'.
DATA are strings, objects or secondary strings.  Return an object of type TYPE."
  (declare (indent 1))
  (unless (memq type '(bold italic strike-through underline))
    (error "Wrong emphasis type: %S" type))
  (apply #'org-element-create type nil (apply #'org-cite-concat data)))

(defun org-cite-concat (&rest data)
  "Concatenate all the DATA arguments and make the result a secondary string.
Each argument may be a string, an object, or a secondary string."
  (let ((results nil))
    (dolist (datum (reverse data))
      (pcase datum
        ('nil nil)
        ;; Element or object.
        ((pred org-element-type) (push datum results))
        ;; Secondary string.
        ((pred consp) (setq results (append datum results)))
        (_
         (signal
          'wrong-type-argument
          (list (format "Argument is not a string or a secondary string: %S"
                        datum))))))
    results))

(defun org-cite-mapconcat (function data separator)
  "Apply FUNCTION to each element of DATA, and return a secondary string.

In between each pair of results, stick SEPARATOR, which may be a string,
an object, or a secondary string.  FUNCTION must be a function of one argument,
and must return either a string, an object, or a secondary string."
  (and data
       (let ((result (list (funcall function (car data)))))
         (dolist (datum (cdr data))
           (setq result
                 (org-cite-concat result separator (funcall function datum))))
         result)))


;;; Internal interface with fontification (activate capability)
(defun org-cite-fontify-default (cite)
  "Fontify CITE with `org-cite' and `org-cite-key' faces.
CITE is a citation object.  The function applies `org-cite' face
on the whole citation, and `org-cite-key' face on each key."
  (let ((beg (org-element-property :begin cite))
        (end (org-with-point-at (org-element-property :end cite)
               (skip-chars-backward " \t")
               (point))))
    (add-text-properties beg end '(font-lock-multiline t))
    (add-face-text-property beg end 'org-cite)
    (dolist (reference (org-cite-get-references cite))
      (let ((boundaries (org-cite-key-boundaries reference)))
        (add-face-text-property (car boundaries) (cdr boundaries)
                                'org-cite-key)))))

(defun org-cite-activate (limit)
  "Activate citations from up to LIMIT buffer position.
Each citation encountered is activated using the appropriate function
from the processor set in `org-cite-activate-processor'."
  (let* ((name org-cite-activate-processor)
         (activate
          (or (and name
                   (org-cite-processor-has-capability-p name 'activate)
                   (org-cite-processor-activate (org-cite--get-processor name)))
              #'org-cite-fontify-default)))
    (when (re-search-forward org-element-citation-prefix-re limit t)
      (let ((cite (org-with-point-at (match-beginning 0)
                    (org-element-citation-parser))))
        (when cite
          (funcall activate cite)
          ;; Move after cite object and make sure to return
          ;; a non-nil value.
          (goto-char (org-element-property :end cite)))))))


;;; Internal interface with Org Export library (export capability)
(defun org-cite-store-bibliography (info)
  "Store bibliography in the communication channel.

Bibliography is stored as a list of absolute file names in the `:bibliography'
property.

INFO is the communication channel, as a plist.  It is modified by side-effect."
  (plist-put info :bibliography (org-cite-list-bibliography-files)))

(defun org-cite-store-export-processor (info)
  "Store export processor in the `:cite-export' property during export.

Export processor is stored as a triplet, or nil.

When non-nil, it is defined as (NAME BIBLIOGRAPHY-STYLE CITATION-STYLE) where
NAME is a symbol, whereas BIBLIOGRAPHY-STYLE and CITATION-STYLE are strings,
or nil.

INFO is the communication channel, as a plist.  It is modified by side-effect."
  (let* ((err
          (lambda (s)
            (user-error "Invalid cite export processor definition: %S" s)))
         (processor
          (pcase (plist-get info :cite-export)
            ((or "" `nil) nil)
            ;; Value is a string.  It comes from a "cite_export"
            ;; keyword.  It may contain between 1 and 3 tokens, the
            ;; first one being a symbol and the other (optional) two,
            ;; strings.
            ((and (pred stringp) s)
             (with-temp-buffer
               (save-excursion (insert s))
               (let ((result (list (read (current-buffer)))))
                 (dotimes (_ 2)
                   (skip-chars-forward " \t")
                   (cond
                    ((eobp) (push nil result))
                    ((char-equal ?\" (char-after))
                     (condition-case _
                         (push (org-not-nil (read (current-buffer))) result)
                       (error (funcall err s))))
                    (t
                     (let ((origin (point)))
                       (skip-chars-forward "^ \t")
                       (push (org-not-nil (buffer-substring origin (point)))
                             result)))))
                 (unless (eobp) (funcall err s))
                 (nreverse result))))
            ;; Value is an alist.  It must come from
            ;; `org-cite-export-processors' variable.  Find the most
            ;; appropriate processor according to current export
            ;; back-end.
            ((and (pred consp) alist)
             (let* ((backend (plist-get info :back-end))
                    (candidates
                     ;; Limit candidates to processors associated to
                     ;; back-ends derived from or equal to the current
                     ;; one.
                     (sort (seq-filter
                            (pcase-lambda (`(,key . ,_))
                              (org-export-derived-backend-p backend key))
                            alist)
                           (lambda (a b)
                             (org-export-derived-backend-p (car a) (car b))))))
               ;; Select the closest candidate, or fallback to t.
               (pcase (or (car candidates) (assq t alist))
                 ('nil nil)
                 (`(,_ . ,p)
                  ;; Normalize value by turning it into a triplet.
                  (pcase p
                    (`(,(pred symbolp))
                     (append p (list nil nil)))
                    (`(,(pred symbolp) ,(pred string-or-null-p))
                     (append p (list nil)))
                    (`(,(pred symbolp)
                       ,(pred string-or-null-p)
                       ,(pred string-or-null-p))
                     p)
                    (_ (funcall err p))))
                 (other (funcall err (cdr other))))))
            (other (funcall err other)))))
    (pcase processor
      ('nil nil)
      (`(,name . ,_)
       (cond
        ((not (org-cite--get-processor name))
         (user-error "Unknown processor %S" name))
        ((not (org-cite-processor-has-capability-p name 'export))
         (user-error "Processor %S is unable to handle citation export" name)))))
    (plist-put info :cite-export processor)))

(defun org-cite-export-citation (citation _ info)
  "Export CITATION object according to INFO property list.
This function delegates the export of the current citation to the
selected citation processor."
  (pcase (plist-get info :cite-export)
    ('nil nil)
    (`(,p ,_ ,_)
     (funcall (org-cite-processor-export-citation (org-cite--get-processor p))
	      citation
              (org-cite-citation-style citation info)
              (plist-get info :back-end)
              info))
    (other (error "Invalid `:cite-export' value: %S" other))))

(defun org-cite-export-bibliography (keyword _ info)
  "Return bibliography associated to \"print_bibliography\" KEYWORD.
BACKEND is the export back-end, as a symbol.  INFO is a plist
used as a communication channel."
  (pcase (plist-get info :cite-export)
    ('nil nil)
    (`(,p ,_ ,_)
     (let ((export-bibilography
            (org-cite-processor-export-bibliography
             (org-cite--get-processor p))))
       (when export-bibilography
         (funcall export-bibilography
	          (org-cite-list-keys info)
                  (plist-get info :bibliography)
                  (org-cite-bibliography-style info)
                  (org-cite-bibliography-properties keyword)
                  (plist-get info :back-end)
                  info))))
    (other (error "Invalid `:cite-export' value: %S" other))))

(defun org-cite-process-citations (info)
  "Replace all citations in the parse tree.
INFO is the communication channel, as a plist.  Parse tree is modified
by side-effect."
  (dolist (cite (org-cite-list-citations info))
    (let ((replacement (org-cite-export-citation cite nil info))
          (blanks (or (org-element-property :post-blank cite) 0)))
      (if (null replacement)
          ;; Before removing the citation, transfer its `:post-blank'
          ;; property to the object before, if any.
          (org-cite--set-previous-post-blank cite blanks info)
        ;; Make sure there is a space between a quotation mark and
        ;; a citation.  This is particularly important when using
        ;; `adaptive' note rule.  See `org-cite-note-rules'.
        (let ((previous (org-export-get-previous-element cite info)))
          (when (and (org-string-nw-p previous)
                     (string-suffix-p "\"" previous))
            (org-cite--set-previous-post-blank cite 1 info)))
        (pcase replacement
          ;; String.
          ((pred stringp)
           ;; Handle `:post-blank' before replacing value.
           (let ((output (concat (org-trim replacement)
                                 (make-string blanks ?\s))))
             (org-element-insert-before (org-export-raw-string output) cite)))
          ;; Single element.
          (`(,(pred symbolp) . ,_)
           (org-cite--set-post-blank replacement blanks)
           (org-element-insert-before replacement cite))
          ;; Secondary string: splice objects at cite's place.
          ;; Transfer `:post-blank' to the last object.
          ((pred consp)
           (let ((last nil))
             (dolist (datum replacement)
               (setq last datum)
               (org-element-insert-before datum cite))
             (org-cite--set-post-blank last blanks)))
          (_
           (error "Invalid return value from citation export processor: %S"
                  replacement))))
      (org-element-extract-element cite))))

(defun org-cite-process-bibliography (info)
  "Replace all \"print_bibliography\" keywords in the parse tree.

INFO is the communication channel, as a plist.  Parse tree is modified
by side effect."
  (org-element-map (plist-get info :parse-tree) 'keyword
    (lambda (keyword)
      (when (equal "PRINT_BIBLIOGRAPHY" (org-element-property :key keyword))
        (let ((replacement (org-cite-export-bibliography keyword nil info))
              (blanks (or (org-element-property :post-blank keyword) 0)))
          (pcase replacement
            ;; Before removing the citation, transfer its
            ;; `:post-blank' property to the element before, if any.
            ('nil
             (org-cite--set-previous-post-blank keyword blanks info)
             (org-element-extract-element keyword))
            ;; Handle `:post-blank' before replacing keyword with string.
            ((pred stringp)
             (let ((output (concat (org-element-normalize-string replacement)
                                   (make-string blanks ?\n))))
               (org-element-set-element keyword (org-export-raw-string output))))
            ;; List of elements: splice contents before keyword and
            ;; remove the latter.  Transfer `:post-blank' to last
            ;; element.
            ((and `(,(pred listp) . ,_) contents)
             (let ((last nil))
               (dolist (datum contents)
                 (setq last datum)
                 (org-element-insert-before datum keyword))
               (org-cite--set-post-blank last blanks)
               (org-element-extract-element keyword)))
            ;; Single element: replace the keyword.
            (`(,(pred symbolp) . ,_)
             (org-cite--set-post-blank replacement blanks)
             (org-element-set-element keyword replacement))
            (_
             (error "Invalid return value from citation export processor: %S"
                    replacement))))))
    info))

(defun org-cite-finalize-export (output info)
  "Finalizer for export process.
OUTPUT is the full output of the export process.  INFO is the communication
channel, as a property list."
  (pcase (plist-get info :cite-export)
    ('nil output)
    (`(,p ,_ ,_)
     (let ((finalizer
            (org-cite-processor-export-finalizer (org-cite--get-processor p))))
       (if (not finalizer)
           output
         (funcall finalizer
                  output
                  (org-cite-list-keys info)
                  (plist-get info :bibliography)
                  (org-cite-bibliography-style info)
                  (plist-get info :back-end)
                  info))))
    (other (error "Invalid `:cite-export' value: %S" other))))


;;; Internal interface with `org-open-at-point' (follow capability)
(defun org-cite-follow (datum arg)
  "Follow citation or citation-reference DATUM.
Following is done according to the processor set in `org-cite-follow-processor'.
ARG is the prefix argument received when calling `org-open-at-point', or nil."
  (let ((name org-cite-follow-processor))
    (cond
     ((null name)
      (user-error "No processor set to follow citations"))
     ((not (org-cite--get-processor name))
      (user-error "Unknown processor %S" name))
     ((not (org-cite-processor-has-capability-p name 'follow))
      (user-error "Processor %S cannot follow citations" name))
     (t
      (let ((follow (org-cite-processor-follow (org-cite--get-processor name))))
        (funcall follow datum arg))))))


;;; Meta-command for citation insertion (insert capability)
(defun org-cite--allowed-p (context)
  "Non-nil when a citation can be inserted at point.
CONTEXT is the element or object at point, as returned by `org-element-context'."
  (let ((type (org-element-type context)))
    (cond
     ;; No citation in attributes, except in parsed ones.
     ;;
     ;; XXX: Inserting citation in a secondary value is not allowed
     ;; yet.  Is it useful?
     ((let ((post (org-element-property :post-affiliated context)))
	(and post (< (point) post)))
      (let ((case-fold-search t))
        (looking-back
         (rx-to-string
          `(seq line-start (0+ (any " \t"))
                "#+"
                (or ,@org-element-parsed-keywords)
                ":"
                (0+ nonl))
          t)
         (line-beginning-position))))
     ;; Paragraphs and blank lines at top of document are fine.
     ((memq type '(nil paragraph)))
     ;; So are contents of verse blocks.
     ((eq type 'verse-block)
      (and (>= (point) (org-element-property :contents-begin context))
	   (< (point) (org-element-property :contents-end context))))
     ;; In an headline or inlinetask, point must be either on the
     ;; heading itself or on the blank lines below.
     ((memq type '(headline inlinetask))
      (or (not (org-at-heading-p))
	  (and (save-excursion
		 (beginning-of-line)
		 (and (let ((case-fold-search t))
			(not (looking-at-p "\\*+ END[ \t]*$")))
		      (let ((case-fold-search nil))
			(looking-at org-complex-heading-regexp))))
	       (match-beginning 4)
	       (>= (point) (match-beginning 4))
	       (or (not (match-beginning 5))
		   (< (point) (match-beginning 5))))))
     ;; White spaces after an object or blank lines after an element
     ;; are OK.
     ((>= (point)
	  (save-excursion (goto-char (org-element-property :end context))
			  (skip-chars-backward " \r\t\n")
			  (if (eq (org-element-class context) 'object) (point)
			    (line-beginning-position 2)))))
     ;; At the beginning of a footnote definition, right after the
     ;; label, is OK.
     ((eq type 'footnote-definition) (looking-at (rx space)))
     ;; At the start of a list item is fine, as long as the bullet is
     ;; unaffected.
     ((eq type 'item)
      (> (point) (+ (org-element-property :begin context)
                    (current-indentation)
                    (if (org-element-property :checkbox context)
                        5 1))))
     ;; Other elements are invalid.
     ((eq (org-element-class context) 'element) nil)
     ;; Just before object is fine.
     ((= (point) (org-element-property :begin context)))
     ;; Within recursive object too, but not in a link.
     ((eq type 'link) nil)
     ((eq type 'table-cell)
      ;; :contents-begin is not reliable on empty cells, so special
      ;; case it.
      (<= (save-excursion (skip-chars-backward " \t") (point))
          (org-element-property :contents-end context)))
     ((let ((cbeg (org-element-property :contents-begin context))
	    (cend (org-element-property :contents-end context)))
	(and cbeg (>= (point) cbeg) (<= (point) cend)))))))

(defun org-cite--insert-string-before (string reference)
  "Insert STRING before citation REFERENCE object."
  (org-with-point-at (org-element-property :begin reference)
    (insert string ";")))

(defun org-cite--insert-string-after (string reference)
  "Insert STRING after citation REFERENCE object."
  (org-with-point-at (org-element-property :end reference)
    ;; Make sure to move forward when we're inserting at point, so the
    ;; insertion can happen multiple times.
    (if (char-equal ?\; (char-before))
        (insert-before-markers  string ";")
      (insert-before-markers ";" string))))

(defun org-cite--keys-to-citation (keys)
  "Build a citation object from a list of citation KEYS.
Citation keys are strings without the leading \"@\"."
  (apply #'org-element-create
         'citation
         nil
         (mapcar (lambda (k)
                   (org-element-create 'citation-reference (list :key k)))
                 keys)))

(defun org-cite-make-insert-processor (select-key select-style)
  "Build a function appropriate as an insert processor.

SELECT-KEY is a function called with one argument.  When it is nil, the function
should return a citation key as a string, or nil.  Otherwise, the function
should return a list of such keys, or nil.  The keys should not have any leading
\"@\" character.

SELECT-STYLE is a function called with one argument, the citation object being
edited or constructed so far.  It should return a style string, or nil.

The return value is a function of two arguments: CONTEXT and ARG.  CONTEXT is
either a citation reference, a citation object, or nil.  ARG is a prefix
argument.

The generated function inserts or edit a citation at point.  More specifically,

  On a citation reference:

    - on the prefix or right before the \"@\" character, insert a new reference
      before the current one,
    - on the suffix, insert it after the reference,
    - otherwise, update the cite key, preserving both affixes.

    When ARG is non-nil, remove the reference, possibly removing the whole
    citation if it contains a single reference.

  On a citation object:

    - on the style part, offer to update it,
    - on the global prefix, add a new reference before the first one,
    - on the global suffix, add a new reference after the last one,

  Elsewhere, insert a citation at point.  When ARG is non-nil, offer to complete
  style in addition to references."
  (unless (and (functionp select-key) (functionp select-style))
    (error "Wrong argument type(s)"))
  (lambda (context arg)
    (pcase (org-element-type context)
      ;; When on a citation, check point is not on the blanks after it.
      ;; Otherwise, consider we're after it.
      ((and 'citation
            (guard
             (let ((boundaries (org-cite-boundaries context)))
               (and (< (point) (cdr boundaries))
                    (> (point) (car boundaries))))))
       ;; When ARG is non-nil, delete the whole citation.  Otherwise,
       ;; action depends on the point.
       (if arg
           (org-cite-delete-citation context)
         (let* ((begin (org-element-property :begin context))
                (style-end (1- (org-with-point-at begin (search-forward ":")))))
           (if (>= style-end (point))
               ;; On style part, edit the style.
               (let ((style-start (+ 5 begin))
                     (style (funcall select-style)))
                 (unless style (user-error "Aborted"))
                 (org-with-point-at style-start
                   (delete-region style-start style-end)
                   (when (org-string-nw-p style) (insert "/" style))))
             ;; On an affix, insert a new reference before or after
             ;; point.
             (let* ((references (org-cite-get-references context))
                    (key (concat "@" (funcall select-key nil))))
               (if (< (point) (org-element-property :contents-begin context))
                   (org-cite--insert-string-before key (car references))
                 (org-cite--insert-string-after key (org-last references))))))))
      ;; On a citation reference.  If ARG is not nil, remove the
      ;; reference.  Otherwise, action depends on the point.
      ((and 'citation-reference (guard arg)) (org-cite-delete-citation context))
      ('citation-reference
       (pcase-let* ((`(,start . ,end) (org-cite-key-boundaries context))
                    (key (concat "@"
                                 (or (funcall select-key nil)
                                     (user-error "Aborted")))))
         ;; Right before the "@" character, do not replace the reference
         ;; at point, but insert a new one before it.  It makes adding
         ;; a new reference at the beginning easier in the following
         ;; case: [cite:@key].
         (cond
          ((>= start (point)) (org-cite--insert-string-before key context))
          ((<= end (point)) (org-cite--insert-string-after key context))
          (t
           (org-with-point-at start
             (delete-region start end)
             (insert key))))))
      (_
       (let ((keys (funcall select-key t)))
         (unless keys (user-error "Aborted"))
         (insert
          (format "[cite%s:%s]"
                  (if arg
                      (let ((style (funcall select-style
                                            (org-cite--keys-to-citation keys))))
                        (if (org-string-nw-p style)
                            (concat "/" style)
                          ""))
                    "")
                  (mapconcat (lambda (k) (concat "@" k)) keys "; "))))))))

;;;###autoload
(defun org-cite-insert (arg)
  "Insert a citation at point.
Insertion is done according to the processor set in `org-cite-insert-processor'.
ARG is the prefix argument received when calling interactively the function."
  (interactive "P")
  (let ((name org-cite-insert-processor))
    (cond
     ((null name)
      (user-error "No processor set to insert citations"))
     ((not (org-cite--get-processor name))
      (user-error "Unknown processor %S" name))
     ((not (org-cite-processor-has-capability-p name 'insert))
      (user-error "Processor %S cannot insert citations" name))
     (t
      (let ((context (org-element-context))
            (insert (org-cite-processor-insert (org-cite--get-processor name))))
        (cond
         ((memq (org-element-type context) '(citation citation-reference))
          (funcall insert context arg))
         ((org-cite--allowed-p context)
          (funcall insert nil arg))
         (t
          (user-error "Cannot insert a citation here"))))))))

(provide 'oc)
;;; oc.el ends here
