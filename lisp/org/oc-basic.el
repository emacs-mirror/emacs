;;; oc-basic.el --- basic backend for citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;; The `basic' citation processor provides "activate", "follow", "export" and
;; "insert" capabilities.

;; "activate" capability reuses default fontification, but provides additional
;; features on both correct and wrong keys according to the bibliography
;; defined in the document.

;; When the mouse is over a known key, it displays the corresponding
;; bibliography entry.  Any wrong key, however, is highlighted with `error'
;; face.  Moreover, moving the mouse onto it displays a list of suggested correct
;; keys, and pressing <mouse-1> on the faulty key will try to fix it according to
;; those suggestions.

;; On a citation key, "follow" capability moves point to the corresponding entry
;; in the current bibliography.  Elsewhere on the citation, it asks the user to
;; follow any of the keys cited there, with completion.

;; "export" capability supports the following citation styles:
;;
;;   - author (a), including caps (c) variant,
;;   - noauthor (na) including bare (b) variant,
;;   - text (t), including bare (b), caps (c), and bare-caps (bc) variants,
;;   - note (ft, including bare (b), caps (c), and bare-caps (bc) variants,
;;   - nocite (n)
;;   - numeric (nb),
;;   - default, including bare (b), caps (c), and bare-caps (bc) variants.
;;
;; It also supports the following styles for bibliography:
;;   - plain
;;   - numeric
;;   - author-year (default)

;; "insert" capability inserts or edits (with completion) citation style or
;; citation reference keys.  In an appropriate place, it offers to insert a new
;; citation.  With a prefix argument, it removes the one at point.

;; It supports bibliography files in BibTeX (".bibtex"), biblatex (".bib") and
;; JSON (".json") format.

;; Disclaimer: this citation processor is meant to be a proof of concept, and
;; possibly a fall-back mechanism when nothing else is available.  It is too
;; limited for any serious use case.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'bibtex)
(require 'json)
(require 'map)
(require 'oc)
(require 'seq)

(declare-function org-open-at-point "org" (&optional arg))
(declare-function org-open-file "org" (path &optional in-emacs line search))

(declare-function org-element-create "org-element-ast" (type &optional props &rest children))
(declare-function org-element-set "org-element-ast" (old new &optional keep-props))

(declare-function org-element-interpret-data "org-element" (data))
(declare-function org-element-parse-secondary-string "org-element" (string restriction &optional parent))
(declare-function org-element-map "org-element"
                  ( data types fun
                    &optional
                    info first-match no-recursion
                    with-affiliated no-undefer))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-contents "org-element-ast" (node))

(declare-function org-export-data "org-export" (data info))
(declare-function org-export-derived-backend-p "org-export" (backend &rest backends))
(declare-function org-export-raw-string "org-export" (contents))


;;; Customization
(defcustom org-cite-basic-sorting-field 'author
  "Field used to sort bibliography items as a symbol, or nil."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'symbol
  :safe #'symbolp)

(defcustom org-cite-basic-author-year-separator ", "
  "String used to separate cites in an author-year configuration."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'string
  :safe #'stringp)

(defcustom org-cite-basic-max-key-distance 2
  "Maximum (Levenshtein) distance between a wrong key and its suggestions."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'integer
  :safe #'integerp)

(defcustom org-cite-basic-author-column-end 25
  "Column where author field ends in completion table, as an integer."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'integer
  :safe #'integerp)

(defcustom org-cite-basic-column-separator "  "
  "Column separator in completion table, as a string."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'string
  :safe #'stringp)

(defcustom org-cite-basic-mouse-over-key-face 'highlight
  "Face used when mouse is over a citation key."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type 'face
  :safe #'facep)


;;; Internal variables
(defvar org-cite-basic--bibliography-cache nil
  "Cache for parsed bibliography files.

This is an association list following the pattern:

  (FILE-ID . ENTRIES)

FILE-ID is a cons cell (FILE . HASH), with FILE being the absolute file name of
the bibliography file, and HASH a hash of its contents.

ENTRIES is a hash table with citation references as keys and fields alist as
values.")

(defvar org-cite-basic--completion-cache (make-hash-table :test #'equal)
  "Cache for key completion table.

This is an a hash-table.")


;;; Internal functions
(defun org-cite-basic--parse-json ()
  "Parse JSON entries in the current buffer.
Return a hash table with citation references as keys and fields alist as values."
  (let ((entries (make-hash-table :test #'equal)))
    (let ((json-array-type 'list)
          (json-key-type 'symbol))
      (dolist (item (json-read))
        (puthash (cdr (assq 'id item))
                 (mapcar (pcase-lambda (`(,field . ,value))
                           (pcase field
                             ((or 'author 'editor)
                              ;; Author and editors are arrays of
                              ;; objects, each of them designing a
                              ;; person.  These objects may contain
                              ;; multiple properties, but for this
                              ;; basic processor, we'll focus on
                              ;; `given' and `family'.
                              ;;
                              ;; For compatibility with BibTeX, add
                              ;; "and" between authors and editors.
                              (cons field
                                    (mapconcat
                                     (lambda (alist)
                                       (concat (alist-get 'family alist)
                                               " "
                                               (alist-get 'given alist)))
                                     value
                                     " and ")))
                             ('issued
                              ;; Date are expressed as an array
                              ;; (`date-parts') or a "string (`raw'
                              ;; or `literal'). In both cases,
                              ;; extract the year and associate it
                              ;; to `year' field, for compatibility
                              ;; with BibTeX format.
                              (let ((date (or (alist-get 'date-parts value)
                                              (alist-get 'literal value)
                                              (alist-get 'raw value))))
                                (cons 'year
                                      (cond
                                       ((consp date)
                                         (let ((year (caar date)))
                                           (cond
                                             ((numberp year) (number-to-string year))
                                             ((stringp year) year)
                                             (t
                                               (error
                                                 "First element of CSL-JSON date-parts should be a number or string, got %s: %S"
                                                 (type-of year) year)))))
                                       ((stringp date)
                                        (replace-regexp-in-string
                                          (rx
                                            (minimal-match (zero-or-more anything))
                                            (group-n 1 (repeat 4 digit))
                                            (zero-or-more anything))
                                          (rx (backref 1))
                                          date))
                                       (t
                                        (error "Unknown CSL-JSON date format: %S"
                                               value))))))
                             (_
                              (cons field value))))
                         item)
                 entries))
      entries)))

(defun org-cite-basic--parse-bibtex (dialect)
  "Parse BibTeX entries in the current buffer.
DIALECT is the BibTeX dialect used.  See `bibtex-dialect'.
Return a hash table with citation references as keys and fields alist as values."
  (let ((entries (make-hash-table :test #'equal))
        (bibtex-sort-ignore-string-entries t))
    (bibtex-set-dialect dialect t)
    ;; Throw an error if bibliography is malformed.
    (unless (bibtex-validate)
      (user-error "Malformed bibliography at %S"
                  (or (buffer-file-name) (current-buffer))))
    (bibtex-map-entries
     (lambda (key &rest _)
       ;; Normalize entries: field names are turned into symbols
       ;; including special "=key=" and "=type=", and consecutive
       ;; white spaces are removed from values.
       (puthash key
                (mapcar
                 (pcase-lambda (`(,field . ,value))
                   (pcase field
                     ("=key=" (cons 'id key))
                     ("=type=" (cons 'type value))
                     (_
                      (cons
                       (intern (downcase field))
                       (replace-regexp-in-string "[ \t\n]+" " " value)))))
                 ;; Parse, substituting the @string replacements.
                 ;; See Emacs bug#56475 discussion.
                 (let ((bibtex-string-files `(,(buffer-file-name)))
                       (bibtex-expand-strings t))
                   (bibtex-parse-entry t)))
                entries)))
    entries))

(defvar org-cite-basic--file-id-cache nil
  "Hash table linking files to their hash.")
(defun org-cite-basic--parse-bibliography (&optional info)
  "List all entries available in the buffer.

Each association follows the pattern

  (FILE . ENTRIES)

where FILE is the absolute file name of the BibTeX file, and ENTRIES is a hash
table where keys are references and values are association lists between fields,
as symbols, and values as strings or nil.

Optional argument INFO is the export state, as a property list."
  (unless (hash-table-p org-cite-basic--file-id-cache)
    (setq org-cite-basic--file-id-cache (make-hash-table :test #'equal)))
  (if (plist-member info :cite-basic/bibliography)
      (plist-get info :cite-basic/bibliography)
    (let ((results nil))
      (dolist (file (org-cite-list-bibliography-files))
        ;; Follow symlinks, to look into modification time of the
        ;; actual file, not its symlink.
        (setq file (file-truename file))
        (when (file-readable-p file)
          (with-temp-buffer
            (when (or (org-file-has-changed-p file)
                      (not (gethash file org-cite-basic--file-id-cache)))
              (insert-file-contents file)
              (set-visited-file-name file t)
              (puthash file (org-buffer-hash) org-cite-basic--file-id-cache))
            (condition-case nil
                (unwind-protect
	            (let* ((file-id (cons file (gethash file org-cite-basic--file-id-cache)))
                           (entries
                            (or (cdr (assoc file-id org-cite-basic--bibliography-cache))
                                (let ((table
                                       (pcase (file-name-extension file)
                                         ("json" (org-cite-basic--parse-json))
                                         ("bib" (org-cite-basic--parse-bibtex 'biblatex))
                                         ("bibtex" (org-cite-basic--parse-bibtex 'BibTeX))
                                         (ext
                                          (user-error "Unknown bibliography extension: %S"
                                                      ext)))))
                                  (push (cons file-id table) org-cite-basic--bibliography-cache)
                                  table))))
                      (push (cons file entries) results))
                  (set-visited-file-name nil t))
              (error (setq org-cite-basic--file-id-cache nil))))))
      (when info (plist-put info :cite-basic/bibliography results))
      results)))

(defun org-cite-basic--key-number (key info)
  "Return number associated to cited KEY.
INFO is the export state, as a property list."
  (let ((predicate
         (org-cite-basic--field-less-p org-cite-basic-sorting-field info)))
    (org-cite-key-number key info predicate)))

(defun org-cite-basic--all-keys ()
  "List all keys available in current bibliography."
  (seq-mapcat (pcase-lambda (`(,_ . ,entries))
                (map-keys entries))
              (org-cite-basic--parse-bibliography)))

(defun org-cite-basic--get-entry (key &optional info)
  "Return BibTeX entry for KEY, as an association list.
When non-nil, INFO is the export state, as a property list."
  (catch :found
    (pcase-dolist (`(,_ . ,entries) (org-cite-basic--parse-bibliography info))
      (let ((entry (gethash key entries)))
        (when entry (throw :found entry))))
    nil))

(defun org-cite-basic--get-field (field entry-or-key &optional info raw)
  "Return FIELD value for ENTRY-OR-KEY, or nil.

FIELD is a symbol.  ENTRY-OR-KEY is either an association list, as returned by
`org-cite-basic--get-entry', or a string representing a citation key.

Optional argument INFO is the export state, as a property list.

Return value may be nil or a string.  If current export backend is derived
from `latex', return a raw string object instead, unless optional
argument RAW is non-nil.

Throw an error if the field value is non-string and non-nil."
  (let ((value
         (cdr
          (assq field
                (pcase entry-or-key
                  ((pred stringp)
                   (org-cite-basic--get-entry entry-or-key info))
                  ((pred consp)
                   entry-or-key)
                  (_
                   (error "Wrong value for ENTRY-OR-KEY: %S" entry-or-key)))))))
    (when (and value (not (stringp value)))
      (error "Non-string bibliography field value: %S" value))
    (if (and value
             (not raw)
             (org-export-derived-backend-p (plist-get info :back-end) 'latex))
        (org-export-raw-string value)
      value)))

(defun org-cite-basic--shorten-names (names)
  "Return a list of family names from a list of full NAMES.
NAMES can be a string or raw string object.

To better accomomodate corporate names, this will only shorten
personal names of the form \"family, given\"."
  (let (names-string raw-p)
    (cond
     ((stringp names) (setq names-string names))
     ((org-element-type-p names 'raw)
      (setq names-string (mapconcat #'identity (org-element-contents names) "")
            raw-p t)))
    (when names-string
      (setq names-string
            (mapconcat
             (lambda (name)
               (if (eq 1 (length name))
                   (cdr (split-string name))
                 (car (split-string name ", "))))
             (split-string names-string " and ")
             ", "))
      (if raw-p (org-export-raw-string names-string)
        names-string))))

(defun org-cite-basic--number-to-suffix (n)
  "Compute suffix associated to number N.
This is used for disambiguation."
  (let ((result nil))
    (apply #'string
           (mapcar (lambda (n) (+ 97 n))
                   (catch :complete
                     (while t
                       (push (% n 26) result)
                       (setq n (/ n 26))
                       (cond
                        ((= n 0) (throw :complete result))
                        ((< n 27) (throw :complete (cons (1- n) result)))
                        ((= n 27) (throw :complete (cons 0 (cons 0 result))))
                        (t nil))))))))

(defun org-cite-basic--get-author (entry-or-key &optional info raw)
  "Return author associated to ENTRY-OR-KEY.

ENTRY-OR-KEY, INFO and RAW arguments are the same arguments as
used in `org-cite-basic--get-field', which see.

Author is obtained from the \"author\" field, if available, or
from the \"editor\" field otherwise."
  (or (org-cite-basic--get-field 'author entry-or-key info raw)
      (org-cite-basic--get-field 'editor entry-or-key info raw)))

(defun org-cite-basic--get-year (entry-or-key info &optional no-suffix)
  "Return year associated to ENTRY-OR-KEY.

ENTRY-OR-KEY is either an association list, as returned by
`org-cite-basic--get-entry', or a string representing a citation
key.  INFO is the export state, as a property list.

Year is obtained from the \"year\" field, if available, or from
the \"date\" field if it starts with a year pattern.

Unlike `org-cite-basic--get-field', this function disambiguates
author-year patterns by adding a letter suffix to the year when
necessary, unless optional argument NO-SUFFIX is non-nil."
  ;; The cache is an association list with the following structure:
  ;;
  ;;    (AUTHOR-YEAR . KEY-SUFFIX-ALIST).
  ;;
  ;; AUTHOR-YEAR is the author year pair associated to current entry
  ;; or key.
  ;;
  ;; KEY-SUFFIX-ALIST is an association (KEY . SUFFIX), where KEY is
  ;; the cite key, as a string, and SUFFIX is the generated suffix
  ;; string, or the empty string.
  (let* ((author (org-cite-basic--get-author entry-or-key info 'raw))
         (year
          (or (org-cite-basic--get-field 'year entry-or-key info 'raw)
              (let ((date
                     (org-cite-basic--get-field 'date entry-or-key info 'raw)))
                (and (stringp date)
                     (string-match (rx string-start
                                       (group (= 4 digit))
                                       (or string-end (not digit)))
                                   date)
                     (match-string 1 date)))))
         (cache-key (cons author year))
         (key
          (pcase entry-or-key
            ((pred stringp) entry-or-key)
            ((pred consp) (cdr (assq 'id entry-or-key)))
            (_ (error "Wrong value for ENTRY-OR-KEY: %S" entry-or-key))))
         (cache (plist-get info :cite-basic/author-date-cache)))
    (pcase (assoc cache-key cache)
      ('nil
       (let ((value (cons cache-key (list (cons key "")))))
         (plist-put info :cite-basic/author-date-cache (cons value cache))
         year))
      (`(,_ . ,alist)
       (let ((suffix
              (or (cdr (assoc key alist))
                  (let ((new (org-cite-basic--number-to-suffix
                              (1- (length alist)))))
                    (push (cons key new) alist)
                    new))))
         (if no-suffix year (concat year suffix)))))))

(defun org-cite-basic--print-bibtex-string (element &optional info)
  "Print Bibtex formatted string ELEMENT, according to Bibtex syntax.
Remove all the {...} that are not a part of LaTeX macros and parse the
LaTeX fragments.  Do nothing when current backend is derived from
LaTeX, according to INFO.

Return updated ELEMENT."
  (if (org-export-derived-backend-p (plist-get info :back-end) 'latex)
      ;; Derived from LaTeX, no need to use manual ad-hoc LaTeX
      ;; parser.
      element
    ;; Convert ELEMENT to anonymous when ELEMENT is string.
    ;; Otherwise, we cannot modify ELEMENT by side effect.
    (when (org-element-type-p element 'plain-text)
      (setq element (org-element-create 'anonymous nil element)))
    ;; Approximately parse LaTeX fragments, assuming Org mode syntax
    ;; (it is close to original LaTeX, and we do not want to
    ;; re-implement complete LaTeX parser here))
    (org-element-map element t
      (lambda (str)
        (when (stringp str)
          (org-element-set
           str
           (org-element-parse-secondary-string
            str '(latex-fragment entity))))))
    ;; Strip the remaining { and }.
    (org-element-map element t
      (lambda (str)
        (when (stringp str)
          (org-element-set str (replace-regexp-in-string "[{}]" "" str)))))
    element))

(defun org-cite-basic--print-entry (entry style &optional info)
  "Format ENTRY according to STYLE string.
ENTRY is an alist, as returned by `org-cite-basic--get-entry'.
Optional argument INFO is the export state, as a property list."
  (let ((author (org-cite-basic--get-author entry info))
        (title (org-cite-basic--get-field 'title entry info))
        (from
         (or (org-cite-basic--get-field 'publisher entry info)
             (org-cite-basic--get-field 'journal entry info)
             (org-cite-basic--get-field 'institution entry info)
             (org-cite-basic--get-field 'school entry info))))
    (org-cite-basic--print-bibtex-string
     (pcase style
       ("plain"
        (let ((year (org-cite-basic--get-year entry info 'no-suffix)))
          (org-cite-concat
           (org-cite-basic--shorten-names author) ". "
           title (and from (list ", " from)) ", " year ".")))
       ("numeric"
        (let ((n (org-cite-basic--key-number (cdr (assq 'id entry)) info))
              (year (org-cite-basic--get-year entry info 'no-suffix)))
          (org-cite-concat
           (format "[%d] " n) author ", "
           (org-cite-emphasize 'italic title)
           (and from (list ", " from)) ", "
           year ".")))
       ;; Default to author-year.  Use year disambiguation there.
       (_
        (let ((year (org-cite-basic--get-year entry info)))
          (org-cite-concat
           author " (" year "). "
           (org-cite-emphasize 'italic title)
           (and from (list ", " from)) "."))))
     info)))


;;; "Activate" capability
(defun org-cite-basic--close-keys (key keys)
  "List cite keys close to KEY in terms of string distance."
  (seq-filter (lambda (k)
                (>= org-cite-basic-max-key-distance
                    (org-string-distance k key)))
              keys))

(defun org-cite-basic--set-keymap (beg end suggestions)
  "Set keymap on citation key between BEG and END positions.

When the key is know, SUGGESTIONS is nil.  Otherwise, it may be
a list of replacement keys, as strings, which will be offered as
substitutes for the unknown key.  Finally, it may be the symbol
`all'."
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<mouse-1>")
      (pcase suggestions
        ('nil #'org-open-at-point)
        ('all #'org-cite-insert)
        (_
         (lambda ()
           (interactive)
           (save-excursion
             (goto-char beg)
             (delete-region beg end)
             (insert
              "@"
              (if (= 1 (length suggestions))
                  (car suggestions)
                (completing-read "Did you mean: "
                                 suggestions nil t))))))))
    (put-text-property beg end 'keymap km)))

(defun org-cite-basic-activate (citation)
  "Set various text properties on CITATION object.

Fontify whole citation with `org-cite' face.  Fontify key with `error' face
when it does not belong to known keys.  Otherwise, use `org-cite-key' face.

Moreover, when mouse is on a known key, display the corresponding bibliography.
On a wrong key, suggest a list of possible keys, and offer to substitute one of
them with a mouse click."
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation))
              (keys (org-cite-basic--all-keys)))
    (put-text-property beg end 'font-lock-multiline t)
    (add-face-text-property beg end 'org-cite)
    (dolist (reference (org-cite-get-references citation))
      (pcase-let* ((`(,beg . ,end) (org-cite-key-boundaries reference))
                   (key (org-element-property :key reference)))
        ;; Highlight key on mouse over.
        (put-text-property beg end
                           'mouse-face
                           org-cite-basic-mouse-over-key-face)
        (if (member key keys)
            ;; Activate a correct key.  Face is `org-cite-key' and
            ;; `help-echo' displays bibliography entry, for reference.
            ;; <mouse-1> calls `org-open-at-point'.
            (let* ((entry (org-cite-basic--get-entry key))
                   (bibliography-entry
                    (org-element-interpret-data
                     (org-cite-basic--print-entry entry "plain"))))
              (add-face-text-property beg end 'org-cite-key)
              (put-text-property beg end 'help-echo bibliography-entry)
              (org-cite-basic--set-keymap beg end nil))
          ;; Activate a wrong key.  Face is `error', `help-echo'
          ;; displays possible suggestions.
          (add-face-text-property beg end 'error)
          (let ((close-keys (org-cite-basic--close-keys key keys)))
            (when close-keys
              (put-text-property beg end 'help-echo
                                 (concat "Suggestions (mouse-1 to substitute): "
                                         (mapconcat #'identity close-keys " "))))
            ;; When the are close know keys, <mouse-1> provides
            ;; completion to fix the current one.  Otherwise, call
            ;; `org-cite-insert'.
            (org-cite-basic--set-keymap beg end (or close-keys 'all))))))))


;;; "Export" capability
(defun org-cite-basic--format-author-year (citation format-cite format-ref info)
  "Format CITATION object according to author-year format.

FORMAT-CITE is a function of three arguments: the global prefix, the contents,
and the global suffix.  All arguments can be strings or secondary strings.

FORMAT-REF is a function of four arguments: the reference prefix, as a string or
secondary string, the author, the year, and the reference suffix, as a string or
secondary string.

INFO is the export state, as a property list."
  (org-export-data
   (funcall format-cite
            (org-element-property :prefix citation)
            (org-cite-mapconcat
             (lambda (ref)
               (let ((k (org-element-property :key ref))
                     (prefix (org-element-property :prefix ref))
                     (suffix (org-element-property :suffix ref)))
                 (funcall format-ref
                          prefix
                          (or (org-cite-basic--get-author k info) "??")
                          (or (org-cite-basic--get-year k info) "????")
                          suffix)))
             (org-cite-get-references citation)
             org-cite-basic-author-year-separator)
            (org-element-property :suffix citation))
   info))

(defun org-cite-basic--citation-numbers (citation info)
  "Return numbers associated to references in CITATION object.
INFO is the export state as a property list."
  (let* ((numbers
          (sort (mapcar (lambda (k) (org-cite-basic--key-number k info))
                        (org-cite-get-references citation t))
                #'<))
         (last (car numbers))
         (result (list (number-to-string (pop numbers)))))
    ;; Use compact number references, i.e., "1, 2, 3" becomes "1-3".
    (while numbers
      (let ((current (pop numbers))
            (next (car numbers)))
        (cond
         ((and next
               (= current (1+ last))
               (= current (1- next)))
          (unless (equal "-" (car result))
            (push "-" result)))
         ((equal "-" (car result))
          (push (number-to-string current) result))
         (t
          (push (format ", %d" current) result)))
        (setq last current)))
    (apply #'concat (nreverse result))))

(defun org-cite-basic--field-less-p (field info)
  "Return a sort predicate comparing FIELD values for two citation keys.
INFO is the export state, as a property list."
  (and field
       (lambda (a b)
         (org-string<
          (org-cite-basic--get-field field a info 'raw)
          (org-cite-basic--get-field field b info 'raw)
          nil t))))

(defun org-cite-basic--sort-keys (keys info)
  "Sort KEYS by author name.
INFO is the export communication channel, as a property list."
  (let ((predicate (org-cite-basic--field-less-p org-cite-basic-sorting-field info)))
    (if predicate
        (sort keys predicate)
      keys)))

(defun org-cite-basic-export-citation (citation style _ info)
  "Export CITATION object.
STYLE is the expected citation style, as a pair of strings or nil.  INFO is the
export communication channel, as a property list."
  (let ((has-variant-p
         (lambda (variant type)
           ;; Non-nil when style VARIANT has TYPE.  TYPE is either
           ;; `bare' or `caps'.
           (member variant
                   (pcase type
                     ('bare '("bare" "bare-caps" "b" "bc"))
                     ('caps '("caps" "bare-caps" "c" "bc"))
                     (_ (error "Invalid variant type: %S" type)))))))
    (pcase style
      ;; "author" style.
      (`(,(or "author" "a") . ,variant)
       (let ((caps (member variant '("caps" "c"))))
         (org-cite-basic--format-author-year
          citation
          (lambda (p c s) (org-cite-concat p c s))
          (lambda (prefix author _ suffix)
            (org-cite-concat
             prefix
             (if caps (org-cite-capitalize author) author)
             suffix))
          info)))
      ;; "noauthor" style.
      (`(,(or "noauthor" "na") . ,variant)
       (let ((bare? (funcall has-variant-p variant 'bare)))
         (org-cite-basic--format-author-year
          citation
          (lambda (prefix contents suffix)
            (org-cite-concat
             (unless bare? "(")
             prefix
             contents
             suffix
             (unless bare? ")")))
          (lambda (prefix _ year suffix)
            (org-cite-concat prefix year suffix))
          info)))
      ;; "nocite" style.
      (`(,(or "nocite" "n") . ,_) nil)
      ;; "text" and "note" styles.
      (`(,(and (or "text" "note" "t" "ft") style) . ,variant)
       (when (and (member style '("note" "ft"))
                  (not (org-cite-inside-footnote-p citation)))
         (org-cite-adjust-note citation info)
         (org-cite-wrap-citation citation info))
       (let ((bare (funcall has-variant-p variant 'bare))
             (caps (funcall has-variant-p variant 'caps)))
         (org-cite-basic--format-author-year
          citation
          (lambda (p c s) (org-cite-concat p c s))
          (lambda (p a y s)
            (org-cite-concat p
                             (if caps (org-cite-capitalize a) a)
                             (if bare " " " (")
                             y
                             (and (not bare) ")")
                             s))
          info)))
      ;; "numeric" style.
      ;;
      ;; When using this style on citations with multiple references,
      ;; use global affixes and ignore local ones.
      (`(,(or "numeric" "nb") . ,_)
       (pcase-let ((`(,prefix . ,suffix) (org-cite-main-affixes citation)))
         (org-export-data
          (org-cite-concat
           "(" prefix (org-cite-basic--citation-numbers citation info) suffix ")")
          info)))
      ;; Default ("nil") style.
      (`(,_ . ,variant)
       (let ((bare (funcall has-variant-p variant 'bare))
             (caps (funcall has-variant-p variant 'caps)))
         (org-cite-basic--format-author-year
          citation
          (lambda (p c s)
            (org-cite-concat (and (not bare) "(") p c s (and (not bare) ")")))
          (lambda (p a y s)
            (org-cite-concat p (if caps (org-cite-capitalize a) a) ", " y s))
          info)))
      ;; This should not happen.
      (_ (error "Invalid style: %S" style)))))

(defun org-cite-basic-export-bibliography (keys _files style _props backend info)
  "Generate bibliography.
KEYS is the list of cited keys, as strings.  STYLE is the expected bibliography
style, as a string.  BACKEND is the export backend, as a symbol.  INFO is the
export state, as a property list."
  (mapconcat
   (lambda (entry)
     (org-export-data
      (org-cite-make-paragraph
       (and (org-export-derived-backend-p backend 'latex)
            (org-export-raw-string "\\noindent\n"))
       (org-cite-basic--print-entry entry style info))
      info))
   (delq nil
         (mapcar
          (lambda (k) (org-cite-basic--get-entry k info))
          (org-cite-basic--sort-keys keys info)))
   "\n"))


;;; "Follow" capability
(defun org-cite-basic-goto (datum _)
  "Follow citation or citation reference DATUM.
When DATUM is a citation reference, open bibliography entry referencing
the citation key.  Otherwise, select which key to follow among all keys
present in the citation."
  (let* ((key
          (if (org-element-type-p datum 'citation-reference)
              (org-element-property :key datum)
            (pcase (org-cite-get-references datum t)
              (`(,key) key)
              (keys
               (or (completing-read "Select citation key: " keys nil t)
                   (user-error "Aborted"))))))
         (file
          (pcase (seq-find (pcase-lambda (`(,_ . ,entries))
                             (gethash key entries))
                           (org-cite-basic--parse-bibliography))
            (`(,f . ,_) f)
            (_  (user-error "Cannot find citation key: %S" key)))))
    (org-open-file file '(4))
    (pcase (file-name-extension file)
      ("json"
       ;; `rx' can not be used with Emacs <27.1 since `literal' form
       ;; is not supported.
       (let ((regexp (rx-to-string `(seq "\"id\":" (0+ (any "[ \t]")) "\"" ,key "\"") t)))
         (goto-char (point-min))
         (re-search-forward regexp)
         (search-backward "{")))
      (_
       (bibtex-set-dialect)
       (bibtex-search-entry key)))))


;;; "Insert" capability
(defun org-cite-basic--complete-style (_)
  "Offer completion for style.
Return chosen style as a string."
  (let* ((styles
          (mapcar (pcase-lambda (`((,style . ,_) . ,_))
                    style)
                  (org-cite-supported-styles))))
    (pcase styles
      (`(,style) style)
      (_ (completing-read "Style (\"\" for default): " styles nil t)))))

(defun org-cite-basic--key-completion-table ()
  "Return completion table for cite keys, as a hash table.

In this hash table, keys are a strings with author, date, and
title of the reference.  Values are the cite keys.

Return nil if there are no bibliography files or no entries."
  ;; Populate bibliography cache.
  (let ((entries (org-cite-basic--parse-bibliography)))
    (cond
     ((null entries) nil)               ;no bibliography files
     ((gethash entries org-cite-basic--completion-cache)
      org-cite-basic--completion-cache)
     (t
      (clrhash org-cite-basic--completion-cache)
      (dolist (key (org-cite-basic--all-keys))
        (let* ((entry (org-cite-basic--get-entry
                       key
                       ;; Supply pre-calculated bibliography to avoid
                       ;; performance degradation.
                       (list :cite-basic/bibliography entries)))
               (completion
                (concat
                 (let ((author (org-cite-basic--get-author entry nil 'raw)))
                   (if author
                       (truncate-string-to-width
                        (replace-regexp-in-string " and " "; " author)
                        org-cite-basic-author-column-end nil ?\s)
                     (make-string org-cite-basic-author-column-end ?\s)))
                 org-cite-basic-column-separator
                 (let ((date (org-cite-basic--get-year entry nil 'no-suffix)))
                   (format "%4s" (or date "")))
                 org-cite-basic-column-separator
                 (org-cite-basic--get-field 'title entry nil 'raw))))
          (puthash completion key org-cite-basic--completion-cache)))
      (unless (map-empty-p org-cite-basic--completion-cache) ;no key
        (puthash entries t org-cite-basic--completion-cache)
        org-cite-basic--completion-cache)))))

(defun org-cite-basic--complete-key (&optional multiple)
  "Prompt for a reference key and return a citation reference string.

When optional argument MULTIPLE is non-nil, prompt for multiple
keys, until one of them is nil.  Then return the list of
reference strings selected.

Raise an error when no bibliography is set in the buffer."
  (let* ((table
          (or (org-cite-basic--key-completion-table)
              (user-error "No bibliography set")))
         (prompt
          (lambda (text)
            (completing-read text table nil t))))
    (if (null multiple)
        (let ((key (gethash (funcall prompt "Key: ") table)))
          (org-string-nw-p key))
      (let* ((keys nil)
             (build-prompt
              (lambda ()
                (if keys
                    (format "Key (empty input exits) %s: "
                            (mapconcat #'identity (reverse keys) ";"))
                  "Key (empty input exits): "))))
        (let ((key (funcall prompt (funcall build-prompt))))
          (while (org-string-nw-p key)
            (push (gethash key table) keys)
            (setq key (funcall prompt (funcall build-prompt)))))
        keys))))


;;; Register processor
(org-cite-register-processor 'basic
  :activate #'org-cite-basic-activate
  :export-citation #'org-cite-basic-export-citation
  :export-bibliography #'org-cite-basic-export-bibliography
  :follow #'org-cite-basic-goto
  :insert (org-cite-make-insert-processor #'org-cite-basic--complete-key
                                          #'org-cite-basic--complete-style)
  :cite-styles
  '((("author" "a") ("caps" "c"))
    (("noauthor" "na") ("bare" "b"))
    (("nocite" "n"))
    (("note" "ft") ("bare-caps" "bc") ("caps" "c"))
    (("numeric" "nb"))
    (("text" "t") ("bare-caps" "bc") ("caps" "c"))
    (("nil") ("bare" "b") ("bare-caps" "bc") ("caps" "c"))))

(provide 'oc-basic)
;;; oc-basic.el ends here
