;;; bibtex.el --- BibTeX mode for GNU Emacs

;; Copyright (C) 1992, 1994, 1995, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

;; Author: Stefan Schoef <schoef@offis.uni-oldenburg.de>
;;	Bengt Martensson <bengt@mathematik.uni-Bremen.de>
;;	Mark Shapiro <shapiro@corto.inria.fr>
;;	Mike Newton <newton@gumby.cs.caltech.edu>
;;	Aaron Larson <alarson@src.honeywell.com>
;; Maintainer: none
;; Keywords: BibTeX, LaTeX, TeX

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  Major mode for editing and validating BibTeX files.

;;  Usage:
;;  See documentation for function bibtex-mode (or type "\M-x describe-mode"
;;  when you are in BibTeX mode).

;;  Todo:
;;  Distribute texinfo file.

;;; Code:

(eval-when-compile
  (require 'compile))


;; Bug Reporting

(defconst
  bibtex-maintainer-address "Dirk Herrmann <D.Herrmann@tu-bs.de>")
;; current maintainer

(defconst
  bibtex-maintainer-salutation "Hallo Dirk,")
;; current maintainer

(defconst
  bibtex-version "(emacs 20.4)")
;; current version of the bibtex.el file


;; User Options:

(defgroup bibtex nil
  "BibTeX mode."
  :group 'tex
  :prefix "bibtex-")

(defgroup bibtex-autokey nil
  "Generates automatically a key from the author/editor and the title field"
  :group 'bibtex
  :prefix "bibtex-autokey-")

(defcustom bibtex-mode-hook nil
  "List of functions to call on entry to BibTeX mode."
  :group 'bibtex
  :type 'hook)

(defcustom bibtex-field-delimiters 'braces
  "*Controls type of field delimiters used.
Set this to `braces' or `double-quotes' according to your personal
preferences.  This variable is buffer-local."
  :group 'bibtex
  :type '(choice (const braces)
		 (const double-quotes)))
(make-variable-buffer-local 'bibtex-field-delimiters)

(defcustom bibtex-entry-delimiters 'braces
  "*Controls type of entry delimiters used.
Set this to `braces' or `parentheses' according to your personal
preferences.  This variable is buffer-local."
  :group 'bibtex
  :type '(choice (const braces)
		 (const parentheses)))
(make-variable-buffer-local 'bibtex-entry-delimiters)

(defcustom bibtex-include-OPTcrossref '("InProceedings" "InCollection")
  "*All entries listed here will have an OPTcrossref field."
  :group 'bibtex
  :type '(repeat string))

(defcustom bibtex-include-OPTkey t
  "*If non-nil, all entries will have an OPTkey field.
If this is a string, it will be used as the initial field text.
If this is a function, it will be called to generate the initial field text."
  :group 'bibtex
  :type '(choice (const :tag "None" nil)
		 (string :tag "Initial text")
		 (function :tag "Initialize Function" :value fun)
		 (other :tag "Default" t)))

(defcustom bibtex-user-optional-fields
  '(("annote" "Personal annotation (ignored)"))
  "*List of optional fields the user wants to have always present.
Entries should be of the same form as the OPTIONAL and
CROSSREF-OPTIONAL lists in `bibtex-entry-field-alist' (see documentation
of this variable for details)."
  :group 'bibtex
  :type '(repeat
	  (group (string :tag "Field")
		 (string :tag "Comment")
		 (option (group :inline t
				:extra-offset -4
				(choice :tag "Init" :value ""
					string
					function))))))

(defcustom bibtex-entry-format '(opts-or-alts numerical-fields)
  "*Controls type of formatting performed by `bibtex-clean-entry'.
It may be t, nil, or a list of symbols out of the following:
opts-or-alts        Delete empty optional and alternative fields and
                      remove OPT and ALT prefixes from used fields.
numerical-fields    Delete delimiters around numeral fields.
page-dashes         Change double dashes in page field to single dash
                      (for scribe compatibility).
inherit-booktitle   If entry contains a crossref field and booktitle
                      field is empty, it is set to the contents of the
                      title field of the crossreferenced entry.
                      Caution: this will work only if buffer is
                       correctly sorted.
realign             Realign entries, so that field texts and perhaps equal
                      signs (depending on the value of
                      `bibtex-align-at-equal-sign') begin in the same column.
last-comma          Add or delete comma on end of last field in entry,
                      according to value of `bibtex-comma-after-last-field'.
delimiters          Change delimiters according to variables
                      `bibtex-field-delimiters' and `bibtex-entry-delimiters'.
unify-case          Change case of entry and field names.

The value t means do all of the above formatting actions.
The value nil means do no formatting at all."
  :group 'bibtex
  :type '(choice (const :tag "None" nil)
		 (const :tag "All" t)
		 (set :menu-tag "Some"
		      (const opts-or-alts)
		      (const numerical-fields)
		      (const page-dashes)
		      (const inherit-booktitle)
		      (const realign)
		      (const last-comma)
		      (const delimiters)
		      (const unify-case))))

(defcustom bibtex-clean-entry-hook nil
  "*List of functions to call when entry has been cleaned.
Functions are called with point inside the cleaned entry, and the buffer
narrowed to just the entry."
  :group 'bibtex
  :type 'hook)

(defcustom bibtex-sort-ignore-string-entries t
  "*If non-nil, BibTeX @String entries are not sort-significant.
That means they are ignored when determining ordering of the buffer
\(e.g., sorting, locating alphabetical position for new entries, etc.).
This variable is buffer-local."
  :group 'bibtex
  :type 'boolean)
(make-variable-buffer-local 'bibtex-sort-ignore-string-entries)

(defcustom bibtex-maintain-sorted-entries nil
  "*If non-nil, BibTeX mode maintains all BibTeX entries in sorted order.
Setting this variable to nil will strip off some comfort (e.g., TAB
completion for reference keys in minibuffer, automatic detection of
duplicates) from BibTeX mode.  See also `bibtex-sort-ignore-string-entries'.
This variable is buffer-local."
  :group 'bibtex
  :type 'boolean)
(make-variable-buffer-local 'bibtex-maintain-sorted-entries)

(defcustom bibtex-field-kill-ring-max 20
  "*Max length of `bibtex-field-kill-ring' before discarding oldest elements."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-entry-kill-ring-max 20
  "*Max length of `bibtex-entry-kill-ring' before discarding oldest elements."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-parse-keys-timeout 60
  "*Specifies interval for parsing buffers.
All BibTeX buffers in Emacs are parsed if Emacs has been idle
`bibtex-parse-keys-timeout' seconds.  Only buffers which were modified
after last parsing and which are maintained in sorted order are parsed."
  :group 'bibtex
  :type 'integer)

(defvar bibtex-entry-field-alist
  '(
    ("Article" . (((("author" "Author1 [and Author2 ...] [and others]")
                    ("title" "Title of the article (BibTeX converts it to lowercase)")
                    ("journal" "Name of the journal (use string, remove braces)")
                    ("year" "Year of publication"))
		   (("volume" "Volume of the journal")
                    ("number" "Number of the journal (only allowed if entry contains volume)")
                    ("pages" "Pages in the journal")
                    ("month" "Month of the publication as a string (remove braces)")
                    ("note" "Remarks to be put at the end of the \\bibitem")))
		  ((("author" "Author1 [and Author2 ...] [and others]")
                    ("title" "Title of the article (BibTeX converts it to lowercase)"))
		   (("pages" "Pages in the journal")
                    ("journal" "Name of the journal (use string, remove braces)")
                    ("year" "Year of publication")
                    ("volume" "Volume of the journal")
                    ("number" "Number of the journal")
		    ("month" "Month of the publication as a string (remove braces)")
                    ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Book" . (((("author" "Author1 [and Author2 ...] [and others]" "" t)
                 ("editor" "Editor1 [and Editor2 ...] [and others]" "" t)
                 ("title" "Title of the book")
                 ("publisher" "Publishing company")
                 ("year" "Year of publication"))
		(("volume" "Volume of the book in the series")
                 ("number" "Number of the book in a small series (overwritten by volume)")
                 ("series" "Series in which the book appeared")
                 ("address" "Address of the publisher")
		 ("edition" "Edition of the book as a capitalized English word")
                 ("month" "Month of the publication as a string (remove braces)")
                 ("note" "Remarks to be put at the end of the \\bibitem")))
               ((("author" "Author1 [and Author2 ...] [and others]" "" t)
                 ("editor" "Editor1 [and Editor2 ...] [and others]" "" t)
                 ("title" "Title of the book"))
                (("publisher" "Publishing company")
                 ("year" "Year of publication")
                 ("volume" "Volume of the book in the series")
                 ("number" "Number of the book in a small series (overwritten by volume)")
                 ("series" "Series in which the book appeared")
                 ("address" "Address of the publisher")
		 ("edition" "Edition of the book as a capitalized English word")
                 ("month" "Month of the publication as a string (remove braces)")
                 ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Booklet" . (((("title" "Title of the booklet (BibTeX converts it to lowercase)"))
		   (("author" "Author1 [and Author2 ...] [and others]")
                    ("howpublished" "The way in which the booklet was published")
                    ("address" "Address of the publisher")
                    ("month" "Month of the publication as a string (remove braces)")
                    ("year" "Year of publication")
                    ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("InBook" . (((("author" "Author1 [and Author2 ...] [and others]" "" t)
                   ("editor" "Editor1 [and Editor2 ...] [and others]" "" t)
                   ("title" "Title of the book")
                   ("chapter" "Chapter in the book")
                   ("publisher" "Publishing company")
                   ("year" "Year of publication"))
		  (("volume" "Volume of the book in the series")
                   ("number" "Number of the book in a small series (overwritten by volume)")
                   ("series" "Series in which the book appeared")
                   ("type" "Word to use instead of \"chapter\"")
                   ("address" "Address of the publisher")
		   ("edition" "Edition of the book as a capitalized English word")
                   ("month" "Month of the publication as a string (remove braces)")
                   ("pages" "Pages in the book")
                   ("note" "Remarks to be put at the end of the \\bibitem")))
		 ((("author" "Author1 [and Author2 ...] [and others]" "" t)
                   ("editor" "Editor1 [and Editor2 ...] [and others]" "" t)
                   ("title" "Title of the book")
                   ("chapter" "Chapter in the book"))
		  (("pages" "Pages in the book")
                   ("publisher" "Publishing company")
                   ("year" "Year of publication")
                   ("volume" "Volume of the book in the series")
                   ("number" "Number of the book in a small series (overwritten by volume)")
		   ("series" "Series in which the book appeared")
                   ("type" "Word to use instead of \"chapter\"")
                   ("address" "Address of the publisher")
                   ("edition" "Edition of the book as a capitalized English word")
                   ("month" "Month of the publication as a string (remove braces)")
                   ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("InCollection" . (((("author" "Author1 [and Author2 ...] [and others]")
                         ("title" "Title of the article in book (BibTeX converts it to lowercase)")
			 ("booktitle" "Name of the book")
                         ("publisher" "Publishing company")
                         ("year" "Year of publication"))
			(("editor" "Editor1 [and Editor2 ...] [and others]")
                         ("volume" "Volume of the book in the series")
                         ("number" "Number of the book in a small series (overwritten by volume)")
                         ("series" "Series in which the book appeared")
                         ("type" "Word to use instead of \"chapter\"")
                         ("chapter" "Chapter in the book")
			 ("pages" "Pages in the book")
                         ("address" "Address of the publisher")
                         ("edition" "Edition of the book as a capitalized English word")
                         ("month" "Month of the publication as a string (remove braces)")
                         ("note" "Remarks to be put at the end of the \\bibitem")))
		       ((("author" "Author1 [and Author2 ...] [and others]")
                         ("title" "Title of the article in book (BibTeX converts it to lowercase)")
                         ("booktitle" "Name of the book"))
			(("pages" "Pages in the book")
                         ("publisher" "Publishing company")
                         ("year" "Year of publication")
			 ("editor" "Editor1 [and Editor2 ...] [and others]")
                         ("volume" "Volume of the book in the series")
                         ("number" "Number of the book in a small series (overwritten by volume)")
                         ("series" "Series in which the book appeared")
                         ("type" "Word to use instead of \"chapter\"")
                         ("chapter" "Chapter in the book")
                         ("address" "Address of the publisher")
                         ("edition" "Edition of the book as a capitalized English word")
                         ("month" "Month of the publication as a string (remove braces)")
                         ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("InProceedings" . (((("author" "Author1 [and Author2 ...] [and others]")
                          ("title" "Title of the article in proceedings (BibTeX converts it to lowercase)")
                          ("booktitle" "Name of the conference proceedings")
                          ("year" "Year of publication"))
			 (("editor" "Editor1 [and Editor2 ...] [and others]")
                          ("volume" "Volume of the conference proceedings in the series")
                          ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                          ("series" "Series in which the conference proceedings appeared")
                          ("pages" "Pages in the conference proceedings")
                          ("address" "Location of the Proceedings")
                          ("month" "Month of the publication as a string (remove braces)")
			  ("organization" "Sponsoring organization of the conference")
                          ("publisher" "Publishing company, its location")
                          ("note" "Remarks to be put at the end of the \\bibitem")))
			((("author" "Author1 [and Author2 ...] [and others]")
                          ("title" "Title of the article in proceedings (BibTeX converts it to lowercase)"))
			 (("booktitle" "Name of the conference proceedings")
			  ("pages" "Pages in the conference proceedings")
                          ("year" "Year of publication")
                          ("editor" "Editor1 [and Editor2 ...] [and others]")
                          ("volume" "Volume of the conference proceedings in the series")
                          ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                          ("series" "Series in which the conference proceedings appeared")
                          ("address" "Location of the Proceedings")
                          ("month" "Month of the publication as a string (remove braces)")
			  ("organization" "Sponsoring organization of the conference")
                          ("publisher" "Publishing company, its location")
                          ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Manual" . (((("title" "Title of the manual"))
		  (("author" "Author1 [and Author2 ...] [and others]")
                   ("organization" "Publishing organization of the manual")
                   ("address" "Address of the organization")
                   ("edition" "Edition of the manual as a capitalized English word")
		   ("month" "Month of the publication as a string (remove braces)")
                   ("year" "Year of publication")
                   ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("MastersThesis" . (((("author" "Author1 [and Author2 ...] [and others]")
                          ("title" "Title of the master\'s thesis (BibTeX converts it to lowercase)")
                          ("school" "School where the master\'s thesis was written")
                          ("year" "Year of publication"))
                         (("type" "Type of the master\'s thesis (if other than \"Master\'s thesis\")")
			  ("address" "Address of the school (if not part of field \"school\") or country")
                          ("month" "Month of the publication as a string (remove braces)")
                          ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Misc" . ((()
		(("author" "Author1 [and Author2 ...] [and others]")
                 ("title" "Title of the work (BibTeX converts it to lowercase)")
                 ("howpublished" "The way in which the work was published")
                 ("month" "Month of the publication as a string (remove braces)")
                 ("year" "Year of publication")
                 ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("PhdThesis" . (((("author" "Author1 [and Author2 ...] [and others]")
                      ("title" "Title of the PhD. thesis")
                      ("school" "School where the PhD. thesis was written")
                      ("year" "Year of publication"))
                     (("type" "Type of the PhD. thesis")
                      ("address" "Address of the school (if not part of field \"school\") or country")
                      ("month" "Month of the publication as a string (remove braces)")
                      ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Proceedings" . (((("title" "Title of the conference proceedings")
                        ("year" "Year of publication"))
		       (("booktitle" "Title of the proceedings for cross references")
			("editor" "Editor1 [and Editor2 ...] [and others]")
                        ("volume" "Volume of the conference proceedings in the series")
                        ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                        ("series" "Series in which the conference proceedings appeared")
                        ("address" "Location of the Proceedings")
                        ("month" "Month of the publication as a string (remove braces)")
			("organization" "Sponsoring organization of the conference")
                        ("publisher" "Publishing company, its location")
                        ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("TechReport" . (((("author" "Author1 [and Author2 ...] [and others]")
                       ("title" "Title of the technical report (BibTeX converts it to lowercase)")
                       ("institution" "Sponsoring institution of the report")
                       ("year" "Year of publication"))
		      (("type" "Type of the report (if other than \"technical report\")")
                       ("number" "Number of the technical report")
                       ("address" "Address of the institution (if not part of field \"institution\") or country")
                       ("month" "Month of the publication as a string (remove braces)")
                       ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Unpublished" . (((("author" "Author1 [and Author2 ...] [and others]")
                        ("title" "Title of the unpublished work (BibTeX converts it to lowercase)")
                        ("note" "Remarks to be put at the end of the \\bibitem"))
		       (("month" "Month of the publication as a string (remove braces)")
                        ("year" "Year of publication")))))
    )

  "Defines entry types and their associated fields.
List of
\(ENTRY-NAME (REQUIRED OPTIONAL) (CROSSREF-REQUIRED CROSSREF-OPTIONAL))
triples.
If the third element is nil, the first pair is always used.
If not, the second pair is used in the case of presence of a crossref
field and the third in the case of absence.
REQUIRED, OPTIONAL, CROSSREF-REQUIRED and CROSSREF-OPTIONAL are lists.
Each element of these lists is a list of the form
\(FIELD-NAME COMMENT-STRING INIT ALTERNATIVE-FLAG).
COMMENT-STRING, INIT, and ALTERNATIVE-FLAG are optional.
FIELD-NAME is the name of the field, COMMENT-STRING the comment to
appear in the echo area, INIT is either the initial content of the
field or a function, which is called to determine the initial content
of the field, and ALTERNATIVE-FLAG (either nil or t) marks if the
field is an alternative.  ALTERNATIVE-FLAG may be t only in the
REQUIRED or CROSSREF-REQUIRED lists.")

(defvar bibtex-comment-start "@Comment ")

(defcustom bibtex-add-entry-hook nil
  "List of functions to call when entry has been inserted."
  :group 'bibtex
  :type 'hook)

(defcustom bibtex-predefined-month-strings
  '(
    ("jan") ("feb") ("mar") ("apr") ("may") ("jun")
    ("jul") ("aug") ("sep") ("oct") ("nov") ("dec")
    )
  "Alist of month string definitions.
Should contain all strings used for months in the BibTeX style files.
Each element is a list with just one element: the string."
  :group 'bibtex
  :type '(repeat
	  (list string)))

(defcustom bibtex-predefined-strings
  (append
   bibtex-predefined-month-strings
   '(
     ("acmcs") ("acta") ("cacm") ("ibmjrd") ("ibmsj") ("ieeese")
     ("ieeetc") ("ieeetcad") ("ipl") ("jacm") ("jcss") ("scp")
     ("sicomp") ("tcs") ("tocs") ("tods") ("tog") ("toms") ("toois")
     ("toplas")
     ))
  "Alist of string definitions.
Should contain the strings defined in the BibTeX style files.  Each
element is a list with just one element: the string."
  :group 'bibtex
  :type '(repeat
	  (list string)))

(defcustom bibtex-string-files nil
  "*List of BibTeX files containing string definitions.
Those files must be specified using pathnames relative to the
directories specified in `bibtex-string-file-path'.  This variable is only
evaluated when BibTeX mode is entered (i.e., when loading the BibTeX
file)."
  :group 'bibtex
  :type '(repeat file))

(defvar bibtex-string-file-path (getenv "BIBINPUTS")
  "*Colon separated list of paths to search for `bibtex-string-files'.")

(defcustom bibtex-help-message t
  "*If not nil print help messages in the echo area on entering a new field."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-autokey-prefix-string ""
  "*String to use as a prefix for all generated keys.
See the documentation of function `bibtex-generate-autokey' for more detail."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-names 1
  "*Number of names to use for the automatically generated reference key.
If this is variable is nil, all names are used.
Possibly more names are used according to `bibtex-autokey-names-stretch'.
See the documentation of function `bibtex-generate-autokey' for more detail."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
		 integer))

(defcustom bibtex-autokey-names-stretch 0
  "*Number of names that can additionally be used.
These names are used only, if all names are used then.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'integer)

(defcustom bibtex-autokey-additional-names ""
  "*String to prepend to the generated key if not all names could be used.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defvar bibtex-autokey-transcriptions
  '(
    ;; language specific characters
    ("\\\\aa" "a")
    ("\\\\AA" "A")
    ("\\\"a\\|\\\\\\\"a\\|\\\\ae" "ae")
    ("\\\"A\\|\\\\\\\"A\\|\\\\AE" "Ae")
    ("\\\\i" "i")
    ("\\\\j" "j")
    ("\\\\l" "l")
    ("\\\\L" "L")
    ("\\\"o\\|\\\\\\\"o\\|\\\\o\\|\\\\oe" "oe")
    ("\\\"O\\|\\\\\\\"O\\|\\\\O\\|\\\\OE" "Oe")
    ("\\\"s\\|\\\\\\\"s" "ss")
    ("\\\"u\\|\\\\\\\"u" "ue")
    ("\\\"U\\|\\\\\\\"U" "Ue")
    ;; accents
    ("\\\\`\\|\\\\'\\|\\\\\\^\\|\\\\~\\|\\\\=\\|\\\\\\.\\|\\\\u\\|\\\\v\\|\\\\H\\|\\\\t\\|\\\\c\\|\\\\d\\|\\\\b" "")
    ;; braces
    ("{" "") ("}" ""))
  "Alist of (old-regexp new-string) pairs.
Used by the default values of `bibtex-autokey-name-change-strings' and
`bibtex-autokey-titleword-change-strings'.  Defaults to translating some
language specific characters to their ASCII transcriptions, and
removing any character accents.")

(defcustom bibtex-autokey-name-change-strings
  bibtex-autokey-transcriptions
  "Alist of (OLD-REGEXP NEW-STRING) pairs.
Any part of name matching a OLD-REGEXP is replaced by NEW-STRING.
Case is significant in OLD-REGEXP.  All regexps are tried in the
order in which they appear in the list, so be sure to avoid inifinite
loops here.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat
	  (list (regexp :tag "Old")
		(string :tag "New"))))

(defcustom bibtex-autokey-name-case-convert 'downcase
  "*Function called for each name to perform case conversion.
See the documentation of function `bibtex-generate-autokey' for more detail."
  :group 'bibtex-autokey
  :type '(choice (const :tag "Preserve case" identity)
		 (const :tag "Downcase" downcase)
		 (const :tag "Capitalize" capitalize)
		 (const :tag "Upcase" upcase)
		 (function :tag "Conversion function")))

(defcustom bibtex-autokey-name-length 'infty
  "*Number of characters from name to incorporate into key.
If this is set to anything but a number, all characters are used.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
		 integer))

(defcustom bibtex-autokey-name-separator ""
  "*String that comes between any two names in the key.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-year-length 2
  "*Number of rightmost digits from the year field to incorporate into key.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'integer)

(defcustom bibtex-autokey-year-use-crossref-entry t
  "*If non-nil use year field from crossreferenced entry if necessary.
If this variable is non-nil and the current entry has no year, but a
valid crossref entry, the year field from the crossreferenced entry is
used.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'boolean)

(defcustom bibtex-autokey-titlewords 5
  "*Number of title words to use for the automatically generated reference key.
If this is set to anything but a number, all title words are used.
Possibly more words from the title are used according to
`bibtex-autokey-titlewords-stretch'.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
		 integer))

(defcustom bibtex-autokey-title-terminators
  '("\\." "!"  "\\?" ":" ";" "--")
  "*Regexp list defining the termination of the main part of the title.
Case of the regexps is ignored.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat regexp))

(defcustom bibtex-autokey-titlewords-stretch 2
  "*Number of words that can additionally be used from the title.
These words are used only, if a sentence from the title can be ended then.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'integer)

(defcustom bibtex-autokey-titleword-ignore
  '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
    "[^A-Z].*" ".*[^a-zA-Z0-9].*")
  "*Determines words from the title that are not to be used in the key.
Each item of the list is a regexp.  If a word of the title matchs a
regexp from that list, it is not included in the title part of the key.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat regexp))

(defcustom bibtex-autokey-titleword-case-convert 'downcase
  "*Function called for each titleword to perform case conversion.
See the documentation of function `bibtex-generate-autokey' for more detail."
  :group 'bibtex-autokey
  :type '(choice (const :tag "Preserve case" identity)
		 (const	:tag "Downcase" downcase)
		 (const	:tag "Capitalize" capitalize)
		 (const	:tag "Upcase" upcase)
		 (function :tag "Conversion function")))

(defcustom bibtex-autokey-titleword-abbrevs nil
  "*Determines exceptions to the usual abbreviation mechanism.
An alist of (OLD-REGEXP NEW-STRING) pairs.  Case is ignored
in matching against OLD-REGEXP, and the first matching pair is used.
See the documentation of function `bibtex-generate-autokey' for details.")

(defcustom bibtex-autokey-titleword-change-strings
  bibtex-autokey-transcriptions
  "Alist of (OLD-REGEXP NEW-STRING) pairs.
Any part of title word matching a OLD-REGEXP is replaced by NEW-STRING.
Case is significant in OLD-REGEXP.  All regexps are tried in the
order in which they appear in the list, so be sure to avoid inifinite
loops here.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat
	  (list (regexp :tag "Old")
		(string :tag "New"))))

(defcustom bibtex-autokey-titleword-length 5
  "*Number of characters from title words to incorporate into key.
If this is set to anything but a number, all characters are used.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
		 integer))

(defcustom bibtex-autokey-titleword-separator "_"
  "*String to be put between the title words.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-name-year-separator ""
  "*String to be put between name part and year part of key.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-year-title-separator ":_"
  "*String to be put between name part and year part of key.
See the documentation of function `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-edit-before-use t
  "*If non-nil, user is allowed to edit the generated key before it is used."
  :group 'bibtex-autokey
  :type 'boolean)

(defcustom bibtex-autokey-before-presentation-function nil
  "Function to call before the generated key is presented.
If non-nil this should be a single function, which is called before
the generated key is presented (in entry or, if
`bibtex-autokey-edit-before-use' is t, in minibuffer).  This function
must take one argument (the automatically generated key), and must
return with a string (the key to use)."
  :group 'bibtex-autokey
  :type '(choice (const nil) function))

(defcustom bibtex-entry-offset 0
  "*Offset for BibTeX entries.
Added to the value of all other variables which determine colums."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-field-indentation 2
  "*Starting column for the name part in BibTeX fields."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-text-indentation
  (+
   bibtex-field-indentation
   (length "organization = "))
  "*Starting column for the text part in BibTeX fields.
Should be equal to the space needed for the longest name part."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-contline-indentation
  (+ bibtex-text-indentation 1)
  "*Starting column for continuation lines of BibTeX fields."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-align-at-equal-sign nil
  "*If non-nil, align fields at equal sign instead of field text.
If non-nil, the column for the equal sign is
the value of `bibtex-text-indentation', minus 2."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-comma-after-last-field nil
  "*If non-nil, a comma is put at end of last field in the entry template."
  :group 'bibtex
  :type 'boolean)

;; bibtex-font-lock-keywords is a user option as well, but since the
;; patterns used to define this variable are defined in a later
;; section of this file, it is defined later.

;; Special support taking care of variants
(if (boundp 'mark-active)
    (defun bibtex-mark-active ()
      ;; In Emacs mark-active indicates if mark is active.
      mark-active)
  (defun bibtex-mark-active ()
    ;; In XEmacs (mark) returns nil when not active.
    (if zmacs-regions (mark) (mark t))))

(if (fboundp 'run-with-idle-timer)
    ;; timer.el is distributed with Emacs
    (fset 'bibtex-run-with-idle-timer 'run-with-idle-timer)
  ;; timer.el is not distributed with XEmacs
  ;; Notice that this does not (yet) pass the arguments, but they
  ;; are not used (yet) in bibtex.el. Fix if needed.
  (defun bibtex-run-with-idle-timer (secs repeat function &rest args)
    (start-itimer "bibtex" function secs (if repeat secs nil) t)))


;; Support for hideshow minor mode
(defun bibtex-hs-forward-sexp (arg)
  "Replacement for `forward-sexp' to be used by `hs-minor-mode'."
  (if (< arg 0)
      (backward-sexp 1)
    (if (looking-at "@\\S(*\\s(")
	(progn
	  (goto-char (match-end 0))
	  (forward-char -1)
	  (forward-sexp 1))
      (forward-sexp 1))))

(add-to-list
 'hs-special-modes-alist
 '(bibtex-mode "@\\S(*\\s(" "\\s)" nil bibtex-hs-forward-sexp nil))


;; Syntax Table, Keybindings and BibTeX Entry List
(defvar bibtex-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?$ "$$  " st)
    (modify-syntax-entry ?% "<   " st)
    (modify-syntax-entry ?' "w   " st)
    (modify-syntax-entry ?@ "w   " st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?~ " " st)
    st))

(defvar bibtex-mode-map
  (let ((km (make-sparse-keymap)))
    ;; The Key `C-c&' is reserved for reftex.el
    (define-key km "\t" 'bibtex-find-text)
    (define-key km "\n" 'bibtex-next-field)
    (define-key km "\M-\t" 'bibtex-complete-string)
    (define-key km [(meta tab)] 'bibtex-complete-key)
    (define-key km "\C-c\"" 'bibtex-remove-delimiters)
    (define-key km "\C-c{" 'bibtex-remove-delimiters)
    (define-key km "\C-c}" 'bibtex-remove-delimiters)
    (define-key km "\C-c\C-c" 'bibtex-clean-entry)
    (define-key km "\C-c\C-q" 'bibtex-fill-entry)
    (define-key km "\C-c?" 'bibtex-print-help-message)
    (define-key km "\C-c\C-p" 'bibtex-pop-previous)
    (define-key km "\C-c\C-n" 'bibtex-pop-next)
    (define-key km "\C-c\C-k" 'bibtex-kill-field)
    (define-key km "\C-c\M-k" 'bibtex-copy-field-as-kill)
    (define-key km "\C-c\C-w" 'bibtex-kill-entry)
    (define-key km "\C-c\M-w" 'bibtex-copy-entry-as-kill)
    (define-key km "\C-c\C-y" 'bibtex-yank)
    (define-key km "\C-c\M-y" 'bibtex-yank-pop)
    (define-key km "\C-c\C-d" 'bibtex-empty-field)
    (define-key km "\C-c\C-f" 'bibtex-make-field)
    (define-key km "\C-c$" 'bibtex-ispell-abstract)
    (define-key km "\M-\C-a" 'bibtex-beginning-of-entry)
    (define-key km "\M-\C-e" 'bibtex-end-of-entry)
    (define-key km "\C-\M-l" 'bibtex-reposition-window)
    (define-key km "\C-\M-h" 'bibtex-mark-entry)
    (define-key km "\C-c\C-b" 'bibtex-entry)
    (define-key km "\C-c\C-rn" 'bibtex-narrow-to-entry)
    (define-key km "\C-c\C-rw" 'widen)
    (define-key km "\C-c\C-o" 'bibtex-remove-OPT-or-ALT)
    (define-key km "\C-c\C-e\C-i" 'bibtex-InProceedings)
    (define-key km "\C-c\C-ei" 'bibtex-InCollection)
    (define-key km "\C-c\C-eI" 'bibtex-InBook)
    (define-key km "\C-c\C-e\C-a" 'bibtex-Article)
    (define-key km "\C-c\C-e\C-b" 'bibtex-InBook)
    (define-key km "\C-c\C-eb" 'bibtex-Book)
    (define-key km "\C-c\C-eB" 'bibtex-Booklet)
    (define-key km "\C-c\C-e\C-c" 'bibtex-InCollection)
    (define-key km "\C-c\C-e\C-m" 'bibtex-Manual)
    (define-key km "\C-c\C-em" 'bibtex-MastersThesis)
    (define-key km "\C-c\C-eM" 'bibtex-Misc)
    (define-key km "\C-c\C-e\C-p" 'bibtex-InProceedings)
    (define-key km "\C-c\C-ep" 'bibtex-Proceedings)
    (define-key km "\C-c\C-eP" 'bibtex-PhdThesis)
    (define-key km "\C-c\C-e\M-p" 'bibtex-Preamble)
    (define-key km "\C-c\C-e\C-s" 'bibtex-String)
    (define-key km "\C-c\C-e\C-t" 'bibtex-TechReport)
    (define-key km "\C-c\C-e\C-u" 'bibtex-Unpublished)
    km))

(easy-menu-define
 bibtex-edit-menu bibtex-mode-map "BibTeX-Edit Menu in BibTeX mode"
 '("BibTeX-Edit"
   ("Moving inside an Entry"
    ["End of Field" bibtex-find-text t]
    ["Next Field" bibtex-next-field t]
    ["Beginning of Entry" bibtex-beginning-of-entry t]
    ["End of Entry" bibtex-end-of-entry t])
   ("Operating on Current Entry"
    ["Fill Entry" bibtex-fill-entry t]
    ["Clean Entry" bibtex-clean-entry t]
    "--"
    ["Kill Entry" bibtex-kill-entry t]
    ["Copy Entry to Kill Ring" bibtex-copy-entry-as-kill t]
    ["Paste Most Recently Killed Entry" bibtex-yank t]
    ["Paste Previously Killed Entry" bibtex-yank-pop t]
    "--"
    ["Ispell Entry" bibtex-ispell-entry t]
    ["Ispell Entry Abstract" bibtex-ispell-abstract t]
    ["Narrow to Entry" bibtex-narrow-to-entry t]
    "--"
    ["View Cite Locations (RefTeX)" reftex-view-crossref-from-bibtex
     (fboundp 'reftex-view-crossref-from-bibtex)])
   ("Operating on Current Field"
    ["Remove Delimiters" bibtex-remove-delimiters t]
    ["Remove OPT or ALT Prefix" bibtex-remove-OPT-or-ALT t]
    ["Clear Field" bibtex-empty-field t]
    "--"
    ["Kill Field" bibtex-kill-field t]
    ["Copy Field to Kill Ring" bibtex-copy-field-as-kill t]
    ["Paste Most Recently Killed Field" bibtex-yank t]
    ["Paste Previously Killed Field" bibtex-yank-pop t]
    "--"
    ["Make New Field" bibtex-make-field t]
    "--"
    ["Snatch from Similar Following Field" bibtex-pop-next t]
    ["Snatch from Similar Preceding Field" bibtex-pop-previous t]
    "--"
    ["String Complete" bibtex-complete-string t]
    ["Key Complete" bibtex-complete-key t]
    "--"
    ["Help about Current Field" bibtex-print-help-message t])
   ("Operating on Buffer or Region"
    ["Validate Entries" bibtex-validate t]
    ["Sort Entries" bibtex-sort-buffer t]
    ["Reformat Entries" bibtex-reformat t]
    ["Count Entries" bibtex-count-entries t])
   ("Miscellaneous"
    ["Convert Alien Buffer" bibtex-convert-alien t]
    ["Submit Bug Report" bibtex-submit-bug-report t])))

(easy-menu-define
 bibtex-entry-menu bibtex-mode-map "Entry-Types Menu in BibTeX mode"
 (list "Entry-Types"
       ["Article in Journal" bibtex-Article t]
       ["Article in Conference Proceedings" bibtex-InProceedings t]
       ["Article in a Collection" bibtex-InCollection t]
       ["Chapter or Pages in a Book" bibtex-InBook t]
       ["Conference Proceedings" bibtex-Proceedings t]
       ["Book" bibtex-Book t]
       ["Booklet (Bound, but no Publisher/Institution)" bibtex-Booklet t]
       ["PhD. Thesis" bibtex-PhdThesis t]
       ["Master's Thesis" bibtex-MastersThesis t]
       ["Technical Report" bibtex-TechReport t]
       ["Technical Manual" bibtex-Manual t]
       ["Unpublished" bibtex-Unpublished t]
       ["Miscellaneous" bibtex-Misc t]
       ["String" bibtex-String t]
       ["Preamble" bibtex-Preamble t]))


;; Internal Variables

(defvar bibtex-pop-previous-search-point nil)
;; Next point where bibtex-pop-previous starts looking for a similar
;; entry.

(defvar bibtex-pop-next-search-point nil)
;; Next point where bibtex-pop-next starts looking for a similar entry.

(defvar bibtex-field-kill-ring nil)
;; Ring of least recently killed fields. At most
;; bibtex-field-kill-ring-max items are kept here.

(defvar bibtex-field-kill-ring-yank-pointer nil)
;; The tail of bibtex-field-kill-ring whose car is the last item yanked.

(defvar bibtex-entry-kill-ring nil)
;; Ring of least recently killed entries. At most
;; bibtex-entry-kill-ring-max items are kept here.

(defvar bibtex-entry-kill-ring-yank-pointer nil)
;; The tail of bibtex-entry-kill-ring whose car is the last item yanked.

(defvar bibtex-last-kill-command nil)
;; Holds the type of the last kill command (either 'field or 'entry)

(defvar bibtex-strings nil)
;; Candidates for bibtex-complete-string. Initialized from
;; bibtex-predefined-strings and bibtex-string-files.
(make-variable-buffer-local 'bibtex-strings)

(defvar bibtex-reference-keys nil)
;; Candidates for TAB completion when entering a reference key using
;; the minibuffer. Also used for bibtex-complete-key. Initialized in
;; bibtex-mode and updated for each new entry.
(make-variable-buffer-local 'bibtex-reference-keys)

(defvar bibtex-buffer-last-parsed-tick nil)
;; Remembers the value returned by buffer-modified-tick when buffer
;; was parsed for keys the last time.
(make-variable-buffer-local 'bibtex-buffer-last-parsed-tick)

(defvar bibtex-parse-idle-timer nil)
;; Stores if timer is already installed

(defvar bibtex-progress-lastperc nil)
;; Holds the last reported percentage for the progress message

(defvar bibtex-progress-lastmes nil)
;; Holds the last reported progress message

(defvar bibtex-progress-interval nil)
;; Holds the chosen interval

(defvar bibtex-key-history nil)
;; Used to store the history list for reading keys

(defvar bibtex-entry-type-history nil)
;; Used to store the history list for reading entry types

(defvar bibtex-field-history nil)
;; Used to store the history list for reading field names

(defvar bibtex-reformat-previous-options nil)
;; Used to store the last reformat options given

(defvar bibtex-reformat-previous-reference-keys nil)
;; Used to store the last reformat reference keys option given


;; Functions to Parse the BibTeX Entries

(defconst bibtex-field-name "[^\"#%'(),={} \t\n0-9][^\"#%'(),={} \t\n]*")
;; Regexp defining the name part of a BibTeX field.

(defconst bibtex-entry-type (concat "@" bibtex-field-name))
;; Regexp defining the type part of a BibTeX entry.

(defconst bibtex-reference-key "[][a-zA-Z0-9.:;?!`'/*@+=|()<>&_^$-]+")
;; Regexp defining the reference key part of a BibTeX entry

(defun bibtex-parse-nested-braces (nesting-level)
  "*Starting on an opening brace, find the corresponding closing brace.
When the function is called, NESTING-LEVEL has to be set to `0'."
  (cond ((looking-at "{")
	 (search-forward-regexp "{[^{}]*")
	 (bibtex-parse-nested-braces (+ nesting-level 1)))
	((looking-at "}")
	 (forward-char 1)
	 (if (= nesting-level 1)
	     (point)
	   (search-forward-regexp "[^{}]*")
	   (bibtex-parse-nested-braces (- nesting-level 1))))
	(t nil)))

(defun bibtex-parse-field-string-braced ()
  "*Parse a field string enclosed by braces.
The field string has to be syntactically correct, which means that the number
of opening and closing braces has to match.  If this is the case, a pair
containing the start and end position of the field string is returned, nil
otherwise."
  (save-match-data
    (let ((starting-point (point))
	  (end-point nil))
      (if (looking-at "{")
	  (setq end-point (bibtex-parse-nested-braces 0)))
      (goto-char starting-point)
      (if end-point
	  (cons starting-point end-point)
	nil))))

(defun bibtex-parse-quoted-string ()
  "*Starting on an opening quote, find the corresponding closing quote."
  (let ((rx (concat "\""
		        "\\("
			    "[^\"\\]"               ;; anything but quote or backslash
			    "\\|"
			    "\\(" 
			        "\\\\\\(.\\|\n\\)"  ;; any backslash quoted character
			    "\\)"
			"\\)*"
		    "\"")))
    (if (looking-at rx)
	(search-forward-regexp rx nil t)
      nil)))

(defun bibtex-parse-field-string-quoted ()
  "*Parse a field string enclosed by quotes.
If a syntactically correct string is found, a pair containing the start and
end position of the field string is returned, nil otherwise."
  (save-match-data
    (let ((starting-point (point))
	  (end-point nil))
      (if (looking-at "\"")
	  (setq end-point (bibtex-parse-quoted-string)))
      (goto-char starting-point)
      (if end-point 
	  (cons starting-point end-point)
	nil))))

(defun bibtex-parse-field-string ()
  "*Parse a field string enclosed by braces or quotes.
If a syntactically correct string is found, a pair containing the start and
end position of the field string is returned, nil otherwise."
  (save-match-data
    (let ((starting-point (point))
	  (boundaries (or (bibtex-parse-field-string-braced)
			  (bibtex-parse-field-string-quoted))))
      (goto-char starting-point)
      boundaries)))

(defun bibtex-search-forward-field-string (bound)
  "*Search forward to find a field string enclosed by braces or quotes.
If a syntactically correct string is found, a pair containing the start and
end position of the field string is returned, nil otherwise.  The search is
delimited by BOUND."
  (save-match-data
    (let ((starting-point (point))
	  (boundaries nil))
      (while (and (not boundaries) (< (point) bound))
	(if (search-forward-regexp "[{\"]" bound 'move)
	    (progn
	      (goto-char (match-beginning 0))
	      (let ((temp-boundaries (or (bibtex-parse-field-string-braced)
					 (bibtex-parse-field-string-quoted))))
		(if (and temp-boundaries (<= (cdr temp-boundaries) bound))
		    (setq boundaries temp-boundaries)
		  (forward-char 1))))))
      (goto-char starting-point)
      boundaries)))

(defun bibtex-parse-association (parse-lhs parse-rhs)
  "*Parse a string of the format <left hand side = right-hand-side>.
The functions PARSE-LHS and PARSE-RHS are used to parse the corresponding
substrings.  These functions are expected to return nil if parsing is not
successfull.  If both functions return non-nil, a pair containing the returned
values of the functions PARSE-LHS and PARSE-RHSis returned."
  (save-match-data
    (let ((starting-point (point))
	  (left (funcall parse-lhs))
	  (right nil))
      (if (and left (looking-at "[ \t\n]*=[ \t\n]*"))
	  (progn
	    (goto-char (match-end 0))
	    (setq right (funcall parse-rhs))))
      (goto-char starting-point)
      (if (and left right)
	  (cons left right)
	nil))))

(defvar bibtex-field-name-for-parsing nil)
;; Temporary variable storing the name string to be parsed by the callback
;; function bibtex-parse-field-name.
(make-variable-buffer-local 'bibtex-field-name-for-parsing)

(defun bibtex-parse-field-name ()
  "*Parse the field name stored in bibtex-field-name-for-parsing.
If the field name is found, return a triple consisting of the position of the
very first character of the match, the actual starting position of the name
part and end position of the match."
  (if (looking-at ",[ \t\n]*")
      (let ((start (point)))
	(goto-char (match-end 0))
	(if (looking-at bibtex-field-name-for-parsing)
	    (let ((boundaries (list start (match-beginning 0) (match-end 0))))
	      (goto-char (match-end 0))
	      boundaries)))))

(defconst bibtex-field-const "[][a-zA-Z0-9.:;?!`'/*@+=|<>&_^$-]+")
;; Regexp defining a bibtex field constant

(defun bibtex-parse-field-text ()
  "*Parse the text part of a BibTeX field.
The text part is either a string, or an empty string, or a constant followed
by one or more <# (string|constant)> pairs.  If a syntactically correct text
is found, a pair containing the start and end position of the text is
returned, nil otherwise."
  (let ((starting-point (point))
	(end-point nil)
	(failure nil))
    (while (and (not end-point) (not failure))
      (if (looking-at bibtex-field-const)
	  (goto-char (match-end 0))
	(let ((boundaries (bibtex-parse-field-string)))
	  (if boundaries
	      (goto-char (cdr boundaries))
	    (setq failure t))))
      (if (not (looking-at "[ \t\n]*#[ \t\n]*"))
	  (setq end-point (point))
	(goto-char (match-end 0))))
    (if (and (not failure) end-point)
	(cons starting-point end-point)
      nil)))

(defun bibtex-parse-field (name)
  "*Parse a BibTeX field of regexp NAME.
If a syntactically correct field is found, a pair containing the boundaries of
the name and text parts of the field is returned."
  (setq bibtex-field-name-for-parsing name)
  (bibtex-parse-association 'bibtex-parse-field-name
			    'bibtex-parse-field-text))

(defun bibtex-search-forward-field (name bound)
  "*Search forward to find a field of name NAME.
If a syntactically correct field is found, a pair containing the boundaries of
the name and text parts of the field is returned.  The search is limited by
BOUND."
  (save-match-data
    (setq bibtex-field-name-for-parsing name)
    (let ((starting-point (point))
	  (boundaries nil))
      (while (and (not boundaries) 
		  (< (point) bound)
		  (search-forward "," bound t))
	(goto-char (match-beginning 0))
	(let ((temp-boundaries 
	       (bibtex-parse-association 'bibtex-parse-field-name
					 'bibtex-parse-field-text)))
	  (if (and temp-boundaries (<= (cdr (cdr temp-boundaries)) bound))
	      (setq boundaries temp-boundaries)
	    (forward-char 1))))
      (goto-char starting-point)
      boundaries)))

(defun bibtex-search-backward-field (name bound)
  "*Search backward to find a field of name NAME.
If a syntactically correct field is found, a pair containing the boundaries of
the name and text parts of the field is returned.  The search is limited by
BOUND."
  (save-match-data
    (setq bibtex-field-name-for-parsing name)
    (let ((starting-point (point))
	  (boundaries nil))
      (while (and (not boundaries)
		  (>= (point) bound)
		  (search-backward "," bound t))
	(let ((temp-boundaries 
	       (bibtex-parse-association 'bibtex-parse-field-name
					 'bibtex-parse-field-text)))
	  (if temp-boundaries
	      (setq boundaries temp-boundaries))))
      (goto-char starting-point)
      boundaries)))

(defun bibtex-start-of-field (bounds)
  (car (car bounds)))
(defun bibtex-end-of-field (bounds)
  (cdr (cdr bounds)))
(defun bibtex-start-of-name-in-field (bounds)
  (car (cdr (car bounds))))
(defun bibtex-end-of-name-in-field (bounds)
  (car (cdr (cdr (car bounds)))))
(defun bibtex-start-of-text-in-field (bounds)
  (car (cdr bounds)))
(defun bibtex-end-of-text-in-field (bounds)
  (cdr (cdr bounds)))

(defun bibtex-parse-string-prefix ()
  "*Parse the prefix part of a bibtex string, including the reference key.
If the string prefix is found, return a triple consisting of the position of
the very first character of the match, the actual starting position of the
reference key and the end position of the match."
  (let* ((case-fold-search t))
    (if (looking-at "^[ \t]*@string[ \t\n]*[({][ \t\n]*")
	(let ((start (point)))
	  (goto-char (match-end 0))
	  (if (looking-at bibtex-reference-key)
	      (let ((boundaries (list start (match-beginning 0) (match-end 0))))
		(goto-char (match-end 0))
		boundaries))))))

(defun bibtex-parse-string-postfix ()
  "*Parse the postfix part of a bibtex string, including the text.
If the string postfix is found, return a triple consisting of the position of
the actual starting and ending position of the text and the very last
character of the string entry."
  (let* ((case-fold-search t)
	 (text-boundaries (bibtex-parse-field-text)))
    (if text-boundaries
	(progn
	  (goto-char (cdr text-boundaries))
	  (if (looking-at "[ \t\n]*[})]")
	      (let ((boundaries (list (car text-boundaries) 
				      (cdr text-boundaries)
				      (match-end 0))))
		(goto-char (match-end 0))
		boundaries))))))

(defun bibtex-parse-string ()
  "*Parse a BibTeX string entry.
If a syntactically correct entry is found, a pair containing the boundaries of
the reference key and text parts of the entry is returned."
  (bibtex-parse-association 'bibtex-parse-string-prefix
			    'bibtex-parse-string-postfix))

(defun bibtex-search-forward-string ()
  "*Search forward to find a bibtex string entry.
If a syntactically correct entry is found, a pair containing the boundaries of
the reference key and text parts of the string is returned."
  (save-match-data
    (let* ((case-fold-search t)
	   (starting-point (point))
	   (boundaries nil))
      (while (and (not boundaries) 
		  (search-forward-regexp 
		   "^[ \t]*@string[ \t\n]*[({][ \t\n]*" nil t))
	(goto-char (match-beginning 0))
	(let ((temp-boundaries (bibtex-parse-string)))
	  (if temp-boundaries
	      (setq boundaries temp-boundaries)
	    (forward-char 1))))
      (goto-char starting-point)
      boundaries)))

(defun bibtex-search-backward-string ()
  "*Search backward to find a bibtex string entry.
If a syntactically correct entry is found, a pair containing the boundaries of
the reference key and text parts of the field is returned."
  (save-match-data
    (let* ((case-fold-search t)
	   (starting-point (point))
	   (boundaries nil))
      (while (and (not boundaries)
		  (search-backward-regexp 
		   "^[ \t]*@string[ \t\n]*[({][ \t\n]*" nil t))
	(goto-char (match-beginning 0))
	(let ((temp-boundaries (bibtex-parse-string)))
	  (if temp-boundaries
	      (setq boundaries temp-boundaries))))
      (goto-char starting-point)
      boundaries)))

(defun bibtex-end-of-string (bounds)
  (car (cdr (cdr (cdr bounds)))))
(defun bibtex-start-of-reference-key-in-string (bounds)
  (car (cdr (car bounds))))
(defun bibtex-end-of-reference-key-in-string (bounds)
  (car (cdr (cdr (car bounds)))))
(defun bibtex-start-of-text-in-string (bounds)
  (car (cdr bounds)))
(defun bibtex-end-of-text-in-string (bounds)
  (car (cdr (cdr bounds))))

(defconst bibtex-entry-head
  (concat "^[ \t]*\\("
	  bibtex-entry-type
	  "\\)[ \t]*[({][ \t\n]*\\("
	  bibtex-reference-key
	  "\\)"))
;; Regexp defining format of the header line of a BibTeX entry.

(defconst bibtex-entry-maybe-empty-head
  (concat bibtex-entry-head "?"))
;; Regexp defining format of the header line of a maybe empty
;; BibTeX entry (possibly without reference key).

(defconst bibtex-type-in-head 1)
;; The regexp subexpression number of the type part in
;; bibtex-entry-head.

(defconst bibtex-key-in-head 2)
;; The regexp subexpression number of the key part in
;; bibtex-entry-head.

(defconst bibtex-entry-postfix "[ \t\n]*,?[ \t\n]*[})]")
;; Regexp defining the postfix of a bibtex entry

(defconst bibtex-key-in-entry bibtex-key-in-head)
;; The regexp subexpression number of the key part in a bibtex entry.

(defvar bibtex-font-lock-keywords
  (list
   ;; entry type and reference key
   (list bibtex-entry-maybe-empty-head
         (list bibtex-type-in-head 'font-lock-function-name-face)
         (list bibtex-key-in-head 'font-lock-constant-face nil t))
   ;; comments
   (list
    (concat "^\\([ \t]*" bibtex-comment-start ".*\\)$")
    1 'font-lock-comment-face)
   ;; optional field names (treated as comments)
   (list
    (concat "^[ \t]*\\(OPT" bibtex-field-name "\\)[ \t]*=")
    1 'font-lock-comment-face)
   ;; field names
   (list (concat "^[ \t]*\\(" bibtex-field-name "\\)[ \t]*=")
         1 'font-lock-variable-name-face)
   "*Default expressions to highlight in BibTeX mode."))
;; now all needed patterns are defined



;; Helper Functions

(defun bibtex-delete-whitespace ()
  ;; Delete all whitespace starting at point
  (if (looking-at "[ \t\n]+")
      (delete-region (point) (match-end 0))))

(defun bibtex-current-line ()
  ;; this computes line number of point regardless whether the buffer
  ;; is narrowed
  (+ (count-lines 1 (point))
     (if (equal (current-column) 0) 1 0)))

(defun bibtex-member-of-regexp (string list)
  ;; Return non-nil if STRING is exactly matched by an element of
  ;; LIST. The value is actually the tail of LIST whose
  ;; car matches STRING.
  (let* (case-fold-search)
    (while
        (and list (not (string-match (concat "^" (car list) "$") string)))
      (setq list (cdr list)))
    list))

(defun bibtex-assoc-of-regexp (string alist)
  ;; Return non-nil if STRING is exactly matched by the car of an
  ;; element of LIST (case ignored). The value is actually the element
  ;; of LIST whose car matches STRING.
  (let* ((case-fold-search t))
    (while
        (and alist
             (not (string-match (concat "^" (car (car alist)) "$") string)))
      (setq alist (cdr alist)))
    (car alist)))

(defun bibtex-skip-to-valid-entry (&optional backward)
  ;; If not at beginning of valid BibTeX entry, move to beginning of
  ;; the next valid one. With argument backward non-nil, move backward
  ;; to beginning of previous valid one. A valid entry is a
  ;; syntactical correct one with type contained in
  ;; bibtex-entry-field-alist or, if bibtex-sort-ignore-string-entries
  ;; is nil, a syntactical correct string entry.
  (let* ((case-fold-search t)
	 (valid-bibtex-entry
	  (concat
	   "@[ \t]*\\("
	   (mapconcat
	    (lambda (type)
	      (concat "\\(" (car type) "\\)"))
	    bibtex-entry-field-alist
	    "\\|")
	   "\\)"))
	 found)
    (while (and (not found)
                (not (if backward
                         (bobp)
                       (eobp))))
      (let ((pnt (point)))
        (cond
         ((looking-at valid-bibtex-entry)
          (if (and
               (bibtex-search-entry nil nil t)
               (equal (match-beginning 0) pnt))
              (setq found t)))
         ((and (not bibtex-sort-ignore-string-entries)
               (bibtex-parse-string))
          (setq found t)))
        (if found
            (goto-char pnt)
          (if backward
              (progn
                (goto-char (1- pnt))
                (if (re-search-backward "^[ \t]*\\(@\\)" nil 'move)
                    (goto-char (match-beginning 1))))
            (goto-char (1+ pnt))
            (if (re-search-forward "^[ \t]*@" nil 'move)
                (forward-char -1))))))))

(defun bibtex-map-entries (fun)
  ;; Call FUN for each BibTeX entry starting with the current. Do this
  ;; to the end of the file. FUN is called with one argument, the key
  ;; of the entry, and with point inside the entry. If
  ;; bibtex-sort-ignore-string-entries is non-nil, FUN will not be
  ;; called for @String entries.
  (let* ((case-fold-search t))
    (bibtex-beginning-of-entry)
    (while (re-search-forward bibtex-entry-maybe-empty-head nil t)
      (let ((pnt (point))
            (entry-type
             (downcase (buffer-substring-no-properties
                        (1+ (match-beginning bibtex-type-in-head))
                        (match-end bibtex-type-in-head))))
            (reference-key
             (if (match-beginning bibtex-key-in-head)
                 (buffer-substring-no-properties
                  (match-beginning bibtex-key-in-head)
                  (match-end bibtex-key-in-head))
               "")))
        (if (or
             (and
              (not bibtex-sort-ignore-string-entries)
              (string-equal "string" (downcase entry-type)))
             (assoc-ignore-case entry-type bibtex-entry-field-alist))
            (funcall fun reference-key))
        (goto-char pnt)
        (bibtex-end-of-entry)))))

(defun bibtex-progress-message (&optional flag interval)
  ;; echos a message about progress of current buffer
  ;; if flag is a string, the message is initialized (in this case a
  ;; value for INTERVAL may be given as well (if not this is set to 5))
  ;; if flag is done, the message is deinitialized
  ;; if flag is absent, a message is echoed if point was incremented
  ;; at least INTERVAL percent since last message was echoed
  (let* ((size (- (point-max) (point-min)))
         (perc (if (= size 0)
                   100
                 (/ (* 100 (- (point) (point-min))) size))))
    (if (or (and (not flag)
                 (>= perc
                     (+ bibtex-progress-interval bibtex-progress-lastperc)))
            (stringp flag))
        (progn
          (if (stringp flag)
              (progn
                (setq bibtex-progress-lastmes flag)
                (if interval
                    (setq bibtex-progress-interval interval)
                  (setq bibtex-progress-interval 5))))
          (setq bibtex-progress-lastperc perc)
          (message (concat bibtex-progress-lastmes " (%d%%)") perc))
      (if (equal flag 'done)
          (progn
            (message (concat bibtex-progress-lastmes " (done)"))
            (setq bibtex-progress-lastmes nil))))))


(defun bibtex-field-left-delimiter ()
  ;; returns a string dependent on bibtex-field-delimiters
  (if (equal bibtex-field-delimiters 'braces)
      "{"
    "\""))

(defun bibtex-field-right-delimiter ()
  ;; returns a string dependent on bibtex-field-delimiters
  (if (equal bibtex-field-delimiters 'braces)
      "}"
    "\""))

(defun bibtex-entry-left-delimiter ()
  ;; returns a string dependent on bibtex-field-delimiters
  (if (equal bibtex-entry-delimiters 'braces)
      "{"
    "("))

(defun bibtex-entry-right-delimiter ()
  ;; returns a string dependent on bibtex-field-delimiters
  (if (equal bibtex-entry-delimiters 'braces)
      "}"
    ")"))

(defun bibtex-search-entry
  (empty-head &optional bound noerror backward)
  ;; A helper function necessary since the failure stack size limit for
  ;; regexps was reduced in emacs 19.32.
  ;; It searches for a BibTeX entry (maybe without a reference key if
  ;; EMPTY-HEAD is t).
  ;; BOUND and NOERROR are exactly as in re-search-forward. If
  ;; BACKWARD is non-nil, search is done in reverse direction. After
  ;; call to this function MATCH-BEGINNING and MATCH-END functions are
  ;; defined, but only for the head part of the entry (especially
  ;; (match-end 0) just gives the end of the head part).
  (let ((pnt (point))
        (prefix (if empty-head
                    bibtex-entry-maybe-empty-head
                  bibtex-entry-head)))
    (if backward
        (let (found)
          (while (and (not found)
                      (re-search-backward prefix bound noerror))
            (setq found (bibtex-search-entry empty-head pnt t)))
          (if found
              (goto-char (match-beginning 0))
            (if (equal noerror nil)
                ;; yell
                (error "Search of BibTeX entry failed"))
            (if (equal noerror t)
                ;; don't move
                (goto-char pnt))
            nil))
      (let ((limit (if bound bound (point-max)))
            md
            found)
        (while (and (not found)
                    (re-search-forward prefix bound noerror))
          (setq md (match-data))
          ;; save match-data of prefix regexp
          (let ((entry-closer
                 (if (save-excursion
                       (goto-char (match-end bibtex-type-in-head))
                       (looking-at "[ \t]*("))
                     ;; entry opened with parenthesis
                     ")"
                   "}")))
	    (let ((infix-start (point))
		  (finished nil))
	      (while (not finished)
		(if (and (looking-at "[ \t\n]*")
			 (<= (match-end 0) limit))
		    (goto-char (match-end 0)))
		(let ((bounds (bibtex-parse-field bibtex-field-name)))
		  (if (and bounds (<= (bibtex-end-of-field bounds) limit))
		      (progn
			(goto-char (bibtex-end-of-field bounds))
			(setq infix-start (point)))
		    (goto-char infix-start)
		    (setq finished t)))))
            ;; This matches the infix* part. The AND construction assures
            ;; that BOUND is respected.
            (if (and (looking-at bibtex-entry-postfix)
                     (string-equal
                      (buffer-substring-no-properties
                       (1- (match-end 0)) (match-end 0))
                      entry-closer)
                     (<= (match-end 0) limit))
                (progn
                  (re-search-forward bibtex-entry-postfix)
                  (setq found t)))))
        (if found
            (progn
              (set-match-data md)
              ;; to set match-beginning/end again
              (point))
          (if (equal noerror nil)
              ;; yell
              (error "Search of BibTeX entry failed"))
          (if (equal noerror t)
              ;; don't move
              (goto-char pnt))
          nil)))))

(defun bibtex-flash-head ()
  ;; Flash at BibTeX entry head before point, if exists.
  (let* ((case-fold-search t)
	 flash)
    (cond ((re-search-backward bibtex-entry-head nil t)
	   (goto-char (match-beginning bibtex-type-in-head))
	   (setq flash (match-end bibtex-key-in-entry)))
	  (t
	   (end-of-line)
	   (skip-chars-backward " \t")
	   (setq flash (point))
	   (beginning-of-line)
	   (skip-chars-forward " \t")))
    (if (pos-visible-in-window-p (point))
	(sit-for 1)
      (message "From: %s"
	       (buffer-substring (point) flash)))))

(defun bibtex-make-optional-field (e-t)
  "Makes an optional field named E-T in current BibTeX entry."
  (if (consp e-t)
      (bibtex-make-field (cons (concat "OPT" (car e-t)) (cdr e-t)))
    (bibtex-make-field (concat "OPT" e-t))))

(defun bibtex-move-outside-of-entry ()
  ;; Make sure we are outside of a BibTeX entry.
  (let ((orig-point (point)))
    (bibtex-end-of-entry)
    (if (< (point) orig-point)
        ;; We moved backward, so we weren't inside an entry to begin with.
        ;; Leave point at the beginning of a line, and preferably
        ;; at the beginning of a paragraph.
        (progn
          (goto-char orig-point)
          (beginning-of-line 1)
          (if (not (= ?\n (char-before (1- (point)))))
              (progn
                (re-search-forward "^[ \t]*[@\n]" nil 'move)
                (backward-char 1)))))
    (skip-chars-forward " \t\n")))

(defun bibtex-beginning-of-first-entry ()
  ;; Go to the beginning of the first BibTeX entry in buffer. Return
  ;; point.
  (goto-char (point-min))
  (if (re-search-forward "^[ \t]*@" nil 'move)
      (beginning-of-line))
  (point))

(defun bibtex-beginning-of-last-entry ()
  ;; Go to the beginning of the last BibTeX entry in buffer.
  (goto-char (point-max))
  (if (re-search-backward "^[ \t]*@" nil 'move)
      (beginning-of-line))
  (point))

(defun bibtex-inside-field ()
  ;; Try to avoid point being at end of a BibTeX field.
  (end-of-line)
  (skip-chars-backward " \t")
  (cond ((= (preceding-char) ?,)
	 (forward-char -2)))
  (cond ((or
          (= (preceding-char) ?})
          (= (preceding-char) ?\"))
         (forward-char -1))))

(defun bibtex-enclosing-field (&optional noerr)
  ;; Search for BibTeX field enclosing point. Point moves to end of
  ;; field. Use match-beginning and match-end to parse the field. If
  ;; NOERR is non-nil, no error is signalled. In this case, t is
  ;; returned on success, nil otherwise.
  (let* ((case-fold-search t)
	 (old-point (point))
	 (boe (bibtex-beginning-of-entry)))
    (goto-char old-point)
    (let ((bounds (bibtex-search-backward-field bibtex-field-name boe)))
      (if (and bounds
	       (<= (bibtex-start-of-field bounds) old-point)
	       (>= (bibtex-end-of-field bounds) old-point))
	  bounds
	(if noerr
	    nil
	  (error "Can't find enclosing BibTeX field"))))))

(defun bibtex-enclosing-entry-maybe-empty-head ()
  ;; Search for BibTeX entry enclosing point. Point moves to
  ;; end of entry. Beginning (but not end) of entry is given
  ;; by (match-beginning 0).
  (let* ((case-fold-search t)
	 (old-point (point)))
    (if (not
         (re-search-backward
          bibtex-entry-maybe-empty-head nil t))
        (progn
          (error "Can't find enclosing BibTeX entry")
          (goto-char old-point)))
    (goto-char (match-beginning bibtex-type-in-head))
    (if (not
         (bibtex-search-entry t nil t))
        (progn
          (error "Can't find enclosing BibTeX entry")
          (goto-char old-point)))))

(defun bibtex-insert-current-kill (n)
  (if (not bibtex-last-kill-command)
      (error "BibTeX kill ring is empty")
    (let* ((kr (if (equal bibtex-last-kill-command 'field)
                   'bibtex-field-kill-ring
                 'bibtex-entry-kill-ring))
           (kryp (if (equal bibtex-last-kill-command 'field)
                     'bibtex-field-kill-ring-yank-pointer
                   'bibtex-entry-kill-ring-yank-pointer))
           (ARGth-kill-element
            (nthcdr
             (mod (- n (length (eval kryp))) (length (eval kr)))
             (eval kr)))
           (current (car (set kryp ARGth-kill-element))))
      (cond
       ((equal bibtex-last-kill-command 'field)
        (let (bibtex-help-message)
          (bibtex-find-text nil t)
          (if (looking-at "[}\"]")
              (forward-char)))
        (set-mark (point))
        (message "Mark set")
        (bibtex-make-field (list (elt current 1) nil (elt current 2)) t))
       ((equal bibtex-last-kill-command 'entry)
        (if (not (eobp))
            (bibtex-beginning-of-entry))
        (set-mark (point))
        (message "Mark set")
        (insert (elt current 1)))
       (t
        (error
         "Unknown tag field: %s.  Please submit a bug report"
         bibtex-last-kill-command))))))

(defun bibtex-format-field-delimiters (start stop)
  "*Replaces delimiters for field strings between START and STOP.
If the current delimiters equal the new delimiters, the buffer is not
changed."
  (goto-char start)
  (let ((boundaries (bibtex-search-forward-field-string stop)))
    (while boundaries
      (goto-char (car boundaries))
      (if (not (looking-at (bibtex-field-left-delimiter)))
	  (progn
	    (delete-char 1)
	    (insert (bibtex-field-left-delimiter))))
      (goto-char (- (cdr boundaries) 1))
      (if (not (looking-at (bibtex-field-right-delimiter)))
	  (progn
	    (delete-char 1)
	    (insert (bibtex-field-right-delimiter))))
      (setq boundaries (bibtex-search-forward-field-string stop)))))

(defun bibtex-format-entry ()
  ;; Helper function for bibtex-clean-entry. Formats current entry
  ;; according to variable bibtex-entry-format.
  (let* ((case-fold-search t)
	 (beg (point))
	 (start (bibtex-beginning-of-entry))
	 crossref-there
	 alternatives-there
	 non-empty-alternative)
    (let ((end (copy-marker (bibtex-end-of-entry))))
      (if (equal start (marker-position end))
          (error "Not on a known BibTeX entry")
        (goto-char start)
	(let ((bounds (bibtex-search-forward-field bibtex-field-name end)))
	  (while bounds
	    ;; determine if entry has crossref field and if at least
	    ;; one alternative is non-empty
	    (let ((begin-name (bibtex-start-of-name-in-field bounds))
		  (end-name (bibtex-end-of-name-in-field bounds))
		  (begin-text (bibtex-start-of-text-in-field bounds))
		  (end-text (bibtex-end-of-text-in-field bounds)))
	      (goto-char begin-name)
	      (if (looking-at "ALT")
		  (progn
		    (setq alternatives-there t)
		    (goto-char begin-text)
		    (if (not (looking-at "\\(\"\"\\)\\|\\({}\\)"))
			(setq non-empty-alternative t))))
	      (if (string-match
		   "\\(OPT\\)?crossref"
		   (buffer-substring-no-properties begin-name end-name))
		  (progn
		    (setq
		     crossref-there
		     (buffer-substring-no-properties
		      (1+ begin-text) (1- end-text)))
		    (if (equal crossref-there "")
			(setq crossref-there nil)))))
	    (goto-char (bibtex-end-of-field bounds))
	    (setq bounds (bibtex-search-forward-field bibtex-field-name end))))
        (if (and alternatives-there
                 (not non-empty-alternative))
            (progn
              (goto-char beg)
              (error "All alternatives are empty")))
        (goto-char start)
        (re-search-forward bibtex-entry-type end)
        (let* ((begin-type (1+ (match-beginning 0)))
               (end-type (match-end 0))
               (entry-type
                (downcase
                 (buffer-substring-no-properties begin-type end-type)))
               (entry-list
                (assoc-ignore-case entry-type bibtex-entry-field-alist))
               (req (elt (elt entry-list 1) 0))
               (creq (elt (elt entry-list 2) 0))
               (format (if (equal bibtex-entry-format t)
                           '(realign opts-or-alts numerical-fields
                                     last-comma page-dashes delimiters
                                     unify-case inherit-booktitle)
                         bibtex-entry-format))
               field-done)
          (if (memq 'unify-case format)
              (progn
                (delete-region begin-type end-type)
                (insert (car entry-list))))
          (if (memq 'delimiters format)
              (progn
                (goto-char end-type)
                (skip-chars-forward " \t\n")
                (delete-char 1)
                (insert (bibtex-entry-left-delimiter))))
          (goto-char start)
          (let ((bounds (bibtex-search-forward-field bibtex-field-name end)))
	    (while bounds
	      (let* ((begin-field (copy-marker (bibtex-start-of-field bounds)))
		     (end-field (copy-marker (bibtex-end-of-field bounds)))
		     (begin-name (copy-marker (bibtex-start-of-name-in-field bounds)))
		     (end-name (copy-marker (bibtex-end-of-name-in-field bounds)))
		     (begin-text (copy-marker (bibtex-start-of-text-in-field bounds)))
		     (end-text (copy-marker (bibtex-end-of-text-in-field bounds)))
		     (field-name
		      (buffer-substring-no-properties
		       (if (string-match
			    "^\\(OPT\\)\\|\\(ALT\\)$"
			    (buffer-substring-no-properties
			     begin-name (+ begin-name 3)))
			   (+ begin-name 3)
			 begin-name)
		       end-name)))
		(cond
		 ((and
		   (memq 'opts-or-alts format)
		   (progn (goto-char begin-name)
			  (looking-at "OPT\\|ALT")))
		  (goto-char begin-text)
		  (if (looking-at "\\(\"\"\\)\\|\\({}\\)")
		      ;; empty: delete whole field if really optional
		      ;; (missing crossref handled) or complain
		      (if (and
			   (progn
			     (goto-char begin-name)
			     (looking-at "OPT"))
			   (not crossref-there)
			   (assoc-ignore-case field-name req))
			  ;; field is not really optional
			  (progn
			    (goto-char begin-name)
			    (bibtex-remove-OPT-or-ALT)
			    (error
			     "Mandatory field ``%s'' is empty" field-name))
			;; field is optional
			(delete-region begin-field end-field))
		    ;; otherwise: not empty, delete "OPT"
		    (goto-char begin-name)
		    (bibtex-remove-OPT-or-ALT)))
		 ((and
		   (memq 'numerical-fields format)
		   (progn
		     (goto-char begin-text)
		     (looking-at "\\(\"[0-9]+\"\\)\\|\\({[0-9]+}\\)")))
		  (goto-char end-text)
		  (delete-char -1)
		  (goto-char begin-text)
		  (delete-char 1))
		 (t
		  (if (memq 'delimiters format)
		      (bibtex-format-field-delimiters begin-text end-text))
		  (if (and
		       (memq 'page-dashes format)
		       (string-match "^\\(OPT\\)?pages$" (downcase field-name))
		       (progn
			 (goto-char begin-text)
			 (looking-at
			  "\\([\"{][0-9]+\\)[ \t\n]*--?[ \t\n]*\\([0-9]+[\"}]\\)")))
		      (replace-match "\\1-\\2"))
		  (if (and
		       (memq 'inherit-booktitle format)
		       (equal (downcase field-name) "booktitle")
		       (progn
			 (goto-char begin-text)
			 (looking-at "\\(\"\"\\)\\|\\({}\\)"))
		       crossref-there
		       (not (bibtex-find-entry-location crossref-there t)))
		      ;; booktitle field empty and crossref entry found
		      ;; --> insert title field of crossreferenced entry if there
		      (let ((stop (bibtex-end-of-entry)))
			(bibtex-beginning-of-entry)
			(let ((bounds (bibtex-search-forward-field "title" stop)))
			  (if bounds
			      (progn
				(goto-char begin-text)
				(forward-char)
				(insert
				 (buffer-substring-no-properties
				  (1+ (bibtex-start-of-text-in-field bounds))
				  (1- (bibtex-end-of-text-in-field bounds)))))))))
		  (if (progn
			(goto-char begin-text)
			(looking-at "\\(\"\"\\)\\|\\({}\\)"))
		      ;; if empty field, complain
		      (progn
			(forward-char)
			(if (or (and
				 crossref-there
				 (assoc-ignore-case
				  field-name creq))
				(and
				 (not crossref-there)
				 (assoc-ignore-case
				  field-name req)))
			    (error
			     "Mandatory field ``%s'' is empty" field-name))))
		  (if (memq 'unify-case format)
		      (let* ((fl
			      (car (cdr (assoc-ignore-case
					 entry-type
					 bibtex-entry-field-alist))))
			     (field-list
			      (append
			       (elt fl 0)
			       (elt fl 1)
			       bibtex-user-optional-fields))
			     (new-field-name
			      (car
			       (assoc-ignore-case field-name field-list))))
			(goto-char begin-name)
			(if new-field-name
			    (progn
			      (delete-region begin-name end-name)
			      (insert new-field-name))
			  (downcase-region begin-name end-name))))
		  (setq field-done t)))
		(if (not field-done)
		    (goto-char begin-field)
		  (setq field-done nil)
		  (goto-char end-field)))
	      (setq bounds (bibtex-search-forward-field bibtex-field-name end))))
          (if (looking-at (bibtex-field-right-delimiter))
              (forward-char))
          (if (memq 'last-comma format)
              (cond ((and
                      bibtex-comma-after-last-field
                      (not (looking-at ",")))
                     (insert ","))
                    ((and
                      (not bibtex-comma-after-last-field)
                      (looking-at ","))
                     (delete-char 1))))
          (if (looking-at ",")
              (forward-char))
          (if (memq 'delimiters format)
              (progn
                (skip-chars-forward " \t\n")
                (delete-char 1)
                (insert (bibtex-entry-right-delimiter))))
          (if (memq 'realign format)
              (bibtex-fill-entry)))))))

(defun bibtex-autokey-change (string change-list)
  ;; Returns a string where some regexps are changed according to
  ;; change-list. Every item of change-list is an (old-regexp
  ;; new-string) pair.
  (let* (case-fold-search
	 (return-string string)
	 (index 0)
	 (len (length change-list))
	 change-item)
    (while (< index len)
      (setq change-item (elt change-list index))
      (while (string-match (car change-item) return-string)
        (setq
         return-string
         (concat (substring return-string 0 (match-beginning 0))
                 (elt change-item 1)
                 (substring return-string (match-end 0)))))
      (setq index (1+ index)))
    return-string))

(defun bibtex-autokey-abbrev (string len)
  ;; Returns an abbreviation of string with at least len
  ;; characters.  String is aborted only after a consonant or at the
  ;; word end.  If len is not a number, string is returned unchanged.
  (cond ((or
          (not (numberp len))
          (<= (length string) len))
         string)
        ((equal len 0)
         "")
        (t
         (let* ((case-fold-search t)
                (abort-char
                 (string-match "[^aeiou]" string (1- len))))
           (if abort-char
               (substring string 0 (1+ abort-char))
             string)))))

(defun bibtex-autokey-get-namefield (min max)
  ;; returns the contents of the name field of the current entry
  ;; does some modifications based on `bibtex-autokey-name-change-strings'
  ;; and removes newlines unconditionally
  (goto-char min)
  (let* ((case-fold-search t)
	 (author-or-editor "\\(author\\)\\|\\(editor\\)")
	 (bounds (bibtex-search-forward-field author-or-editor max)))
    (if bounds
        (bibtex-autokey-change
         (buffer-substring-no-properties
          (1+ (bibtex-start-of-text-in-field bounds))
          (1- (bibtex-end-of-text-in-field bounds)))
         (append bibtex-autokey-name-change-strings '(("\n" " "))))
      "")))

(defun bibtex-autokey-get-names (namefield)
  ;; gathers all names in namefield into a list
  (let* ((case-fold-search t)
	 names)
    (while (not (equal namefield ""))
      (let (name)
        (if (string-match "[ \t\n]and[ \t\n]" namefield)
            (setq name (substring namefield 0 (match-beginning 0))
                  namefield (substring namefield (match-end 0)))
          (setq name namefield
                namefield ""))
        (setq names (append names (list name)))))
    names))

(defun bibtex-autokey-demangle-name (fullname)
  ;; gets the `last' part from a well-formed name
  (let* (case-fold-search
         (lastname
          (if (string-match "," fullname)
              ;; name is of the form "von Last, First" or
              ;; "von Last, Jr, First"
              ;; --> take only the part before the comma
              (let ((von-last
                     (substring fullname 0 (match-beginning 0))))
                (if (string-match "^[a-z]" von-last)
                    ;; von-last has a "von" part --> take the "last" part
                    (if (string-match "[ \t][A-Z]" von-last)
                        (substring von-last (1+ (match-beginning 0)))
                      (error
                       "Name %s is incorrectly formed" fullname))
                  ;; von-last has no "von" part --> take all
                  von-last))
            ;; name is of the form "First von Last"
            (if (string-match "[ \t]" fullname)
                ;; more than one token
                (if (string-match "[ \t][a-z]" fullname)
                    ;; there is a "von" part
                    ;; --> take everything after that
                    (if (string-match
                         "[ \t][A-Z]" fullname (match-end 0))
                        (substring fullname (1+ (match-beginning 0)))
                      (error
                       "Name %s is incorrectly formed" fullname))
                  ;; there is no "von" part --> take only the last token
                  (if (string-match " [^ ]*$" fullname)
                      (substring fullname (1+ (match-beginning 0)))
                    (error "Name %s is incorrectly formed" fullname)))
              ;; only one token --> take it
              fullname)))
         (usename
          (if (string-match "[ \t]+" lastname)
              ;; lastname consists of two or more tokens
              ;; --> take only the first one
              (substring lastname 0 (match-beginning 0))
            lastname)))
    (funcall bibtex-autokey-name-case-convert usename)))

(defun bibtex-autokey-get-namelist (namefield)
  ;; gets namefield, performs abbreviations on the last parts, and
  ;; return results as a list
  (mapcar
   (lambda (fullname)
     (setq
      fullname (substring fullname (string-match "[^ \t]" fullname)))
     (bibtex-autokey-abbrev
      (bibtex-autokey-demangle-name fullname)
      bibtex-autokey-name-length))
   (bibtex-autokey-get-names namefield)))

(defun bibtex-autokey-get-yearfield-digits (max)
  ;; get digit substring from year field.
  (let ((bounds (bibtex-search-forward-field "year" max)))
    (if bounds
	(let ((year (buffer-substring-no-properties
		     (bibtex-start-of-text-in-field bounds)
		     (bibtex-end-of-text-in-field bounds))))
	  (string-match "[^0-9]*\\([0-9]+\\)" year)
	  (substring year (match-beginning 1) (match-end 1)))
      "")))

(defun bibtex-autokey-get-yearfield (min max)
  ;; get year field from current or maybe crossreferenced entry
  (goto-char min)
  (let* ((case-fold-search t)
	 (year (bibtex-autokey-get-yearfield-digits max)))
    (if (and (string= year "") bibtex-autokey-year-use-crossref-entry)
	(let* ((bounds 
		(bibtex-search-forward-field "\\(OPT\\)?crossref" max))
	       (crossref-field
		(if bounds
		    (buffer-substring-no-properties
		     (1+ (bibtex-start-of-text-in-field bounds))
		     (1- (bibtex-end-of-text-in-field bounds))))))
	  (if (not (bibtex-find-entry-location crossref-field t))
	      (let ((stop (bibtex-end-of-entry)))
		(bibtex-beginning-of-entry)
		(bibtex-autokey-get-yearfield-digits stop))
	    ""))
      year)))

(defun bibtex-autokey-get-titlestring (min max)
  ;; get title field contents up to a terminator
  (goto-char min)
  (let* ((case-fold-search t)
	 (bounds (bibtex-search-forward-field "title" max))
	 (titlefield
	  (if bounds
	      (bibtex-autokey-change
	       (buffer-substring-no-properties
		(1+ (bibtex-start-of-text-in-field bounds))
		(1- (bibtex-end-of-text-in-field bounds)))
	       bibtex-autokey-titleword-change-strings)
	    ""))
	 (title-terminators bibtex-autokey-title-terminators))
    (while (not (null title-terminators))
      (if (string-match (car title-terminators) titlefield)
	  (setq titlefield (substring titlefield 0 (match-beginning 0))))
      (setq title-terminators (cdr title-terminators)))
    titlefield))

(defun bibtex-autokey-get-titles (titlestring)
  ;; gathers words from titlestring into a list. Ignores
  ;; specific words and uses only a specific amount of words.
  (let* (case-fold-search
	 titlewords
	 titlewords-extra
	 (counter 0))
    (while (and
            (not (equal titlestring ""))
            (or
             (not (numberp bibtex-autokey-titlewords))
             (< counter
                (+ bibtex-autokey-titlewords
                   bibtex-autokey-titlewords-stretch))))
      (if (string-match "\\b\\w+" titlestring)
          (let* ((end-match (match-end 0))
                 (titleword
		  (substring titlestring (match-beginning 0) end-match)))
	    (if (bibtex-member-of-regexp
		 titleword
		 bibtex-autokey-titleword-ignore)
		(setq counter (1- counter))
              (setq
	       titleword
	       (funcall bibtex-autokey-titleword-case-convert titleword))
	      (if (or (not (numberp bibtex-autokey-titlewords))
		      (< counter bibtex-autokey-titlewords))
		  (setq titlewords (append titlewords (list titleword)))
		(setq titlewords-extra
		      (append titlewords-extra (list titleword)))))
            (setq
             titlestring (substring titlestring end-match)))
        (setq titlestring ""))
      (setq counter (1+ counter)))
    (if (string-match "\\b\\w+" titlestring)
        titlewords
      (append titlewords titlewords-extra))))

(defun bibtex-autokey-get-titlelist (titlestring)
  ;; returns all words in titlestring as a list
  ;; does some abbreviation on the found words
  (mapcar
   (lambda (titleword)
     (let ((abbrev
            (bibtex-assoc-of-regexp
             titleword bibtex-autokey-titleword-abbrevs)))
       (if abbrev
           (elt abbrev 1)
         (bibtex-autokey-abbrev
          titleword
          bibtex-autokey-titleword-length))))
   (bibtex-autokey-get-titles titlestring)))

(defun bibtex-generate-autokey ()
  "Generates automatically a key from the author/editor and the title field.
This will only work for entries where each field begins on a separate line.
The generation algorithm works as follows:
 1. Use the value of `bibtex-autokey-prefix-string' as a prefix.
 2. If there is a non-empty author (preferred) or editor field,
    use it as the name part of the key.
 3. Change any substring found in
    `bibtex-autokey-name-change-strings' to the corresponding new
    one (see documentation of this variable for further detail).
 4. For every of at least first `bibtex-autokey-names' names in
    the name field, determine the last name. If there are maximal
    `bibtex-autokey-names' + `bibtex-autokey-names-stretch'
    names, all names are used.
 5. From every last name, take at least
    `bibtex-autokey-name-length' characters (abort only after a
    consonant or at a word end).
 6. Convert all last names according to the conversion function
    `bibtex-autokey-name-case-convert'.
 7. Build the name part of the key by concatenating all
    abbreviated last names with the string
    `bibtex-autokey-name-separator' between any two. If there are
    more names than are used in the name part, prepend the string
    contained in `bibtex-autokey-additional-names'.
 8. Build the year part of the key by truncating the contents of
    the year field to the rightmost `bibtex-autokey-year-length'
    digits (useful values are 2 and 4). If the year field is
    absent, but the entry has a valid crossref field and the
    variable `bibtex-autokey-year-use-crossref-entry' is non-nil,
    use the year field of the crossreferenced entry instead.
 9. For the title part of the key change the contents of the
    title field of the entry according to
    `bibtex-autokey-titleword-change-strings' to the
    corresponding new one (see documentation of this variable for
    further detail).
10. Abbreviate the result to the string up to (but not including)
    the first occurrence of a regexp matched by the items of
    `bibtex-autokey-title-terminators' and delete those words which
    appear in `bibtex-autokey-titleword-ignore'.
    Build the title part of the key by using at least the first
    `bibtex-autokey-titlewords' words from this
    abbreviated title. If the abbreviated title ends after
    maximal `bibtex-autokey-titlewords' +
    `bibtex-autokey-titlewords-stretch' words, all
    words from the abbreviated title are used.
11. Convert all used titlewords according to the conversion function
    `bibtex-autokey-titleword-case-convert'.
12. For every used title word that appears in
    `bibtex-autokey-titleword-abbrevs' use the corresponding
    abbreviation (see documentation of this variable for further
    detail).
13. From every title word not generated by an abbreviation, take
    at least `bibtex-autokey-titleword-length' characters (abort
    only after a consonant or at a word end).
14. Build the title part of the key by concatenating all
    abbreviated title words with the string
    `bibtex-autokey-titleword-separator' between any two.
15. At least, to get the key, concatenate
    `bibtex-autokey-prefix-string', the name part, the year part
    and the title part with `bibtex-autokey-name-year-separator'
    between the name part and the year part if both are non-empty
    and `bibtex-autokey-year-title-separator' between the year
    part and the title part if both are non-empty. If the year
    part is empty, but not the other two parts,
    `bibtex-autokey-year-title-separator' is used as well.
16. If the value of `bibtex-autokey-before-presentation-function'
    is non-nil, it must be a function taking one argument. This
    function is then called with the generated key as the
    argument. The return value of this function (a string) is
    used as the key.
17. If the value of `bibtex-autokey-edit-before-use' is non-nil,
    the key is then presented in the minibuffer to the user,
    where it can be edited.  The key given by the user is then
    used."
  (let* ((pnt (point))
         (min (bibtex-beginning-of-entry))
         (max (bibtex-end-of-entry))
         (namefield (bibtex-autokey-get-namefield min max))
         (name-etal "")
         (namelist
          (let ((nl (bibtex-autokey-get-namelist namefield)))
            (if (or (not (numberp bibtex-autokey-names))
                    (<= (length nl)
                        (+ bibtex-autokey-names
                           bibtex-autokey-names-stretch)))
                nl
              (setq name-etal bibtex-autokey-additional-names)
              (let (nnl)
                (while (< (length nnl) bibtex-autokey-names)
                  (setq nnl (append nnl (list (car nl)))
                        nl (cdr nl)))
                nnl))))
         (namepart
          (concat
           (mapconcat (lambda (name) name)
                      namelist
                      bibtex-autokey-name-separator)
           name-etal))
         (yearfield (bibtex-autokey-get-yearfield min max))
         (yearpart
          (if (equal yearfield "")
              ""
            (substring
             yearfield
             (- (length yearfield) bibtex-autokey-year-length))))
         (titlestring (bibtex-autokey-get-titlestring min max))
         (titlelist (bibtex-autokey-get-titlelist titlestring))
         (titlepart
          (mapconcat
           (lambda (name) name)
           titlelist
           bibtex-autokey-titleword-separator))
         (autokey
          (concat
           bibtex-autokey-prefix-string
           namepart
           (if (not
                (or
                 (equal namepart "")
                 (equal yearpart "")))
               bibtex-autokey-name-year-separator)
           yearpart
           (if (not
                (or
                 (and
                  (equal namepart "")
                  (equal yearpart ""))
                 (equal titlepart "")))
               bibtex-autokey-year-title-separator)
           titlepart)))
    (if bibtex-autokey-before-presentation-function
        (setq
         autokey
         (funcall bibtex-autokey-before-presentation-function autokey)))
    (goto-char pnt)
    autokey))

(defun bibtex-parse-keys (add verbose &optional abortable)
  ;; Sets bibtex-reference-keys to the keys used in the whole (possibly
  ;; restricted) buffer (either as entry keys or as crossref entries).
  ;; If ADD is non-nil adds the new keys to bibtex-reference-keys instead of
  ;; simply resetting it. If VERBOSE is non-nil gives messages about
  ;; progress. If ABORTABLE is non-nil abort on user input.
  (if bibtex-maintain-sorted-entries
      (let* ((case-fold-search t)
	     (reference-keys (if add bibtex-reference-keys)))
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (if verbose
                (bibtex-progress-message
                 (concat (buffer-name) ": parsing reference keys")))
            (if (catch 'userkey
                  (bibtex-skip-to-valid-entry)
                  (while (not (eobp))
                    (if (and
                         abortable
                         (input-pending-p))
                        (throw 'userkey t))
                    (if verbose
                        (bibtex-progress-message))
                    (let (reference-key
                          xrefd-reference-key)
                      (cond
                       ((looking-at bibtex-entry-head)
                        (setq
                         reference-key
                         (buffer-substring-no-properties
                          (match-beginning bibtex-key-in-head)
                          (match-end bibtex-key-in-head)))
                        (let ((p (point))
                              (m (bibtex-end-of-entry)))
                          (goto-char p)
                          (let ((bounds (bibtex-search-forward-field "crossref" m)))
                            (if bounds
                                (setq
                                 xrefd-reference-key
                                 (buffer-substring-no-properties
                                  (1+ (bibtex-start-of-text-in-field bounds))
                                  (1- (bibtex-end-of-text-in-field bounds))))))))
                       ((bibtex-parse-string)
                        (let ((bounds (bibtex-parse-string)))
                          (setq
                           reference-key
                           (buffer-substring-no-properties
                            (bibtex-start-of-reference-key-in-string bounds)
                            (bibtex-end-of-reference-key-in-string bounds))))))
                      (forward-char)
                      (bibtex-skip-to-valid-entry)
                      (if (not (assoc reference-key reference-keys))
                          (setq reference-keys
                                (cons (list reference-key) reference-keys)))
                      (if (and xrefd-reference-key
                               (not (assoc xrefd-reference-key reference-keys)))
                          (setq reference-keys
                                (cons (list xrefd-reference-key) reference-keys))))))
                ;; user has aborted by typing a key --> return nil
                nil
              ;; successful operation --> return t
              (setq
               bibtex-buffer-last-parsed-tick (buffer-modified-tick)
               bibtex-reference-keys reference-keys)
              (if verbose
                  (bibtex-progress-message 'done))
              t))))))

(defun bibtex-parse-buffers-stealthily ()
  ;; Called by bibtex-run-with-idle-timer. Whenever emacs has been idle
  ;; for bibtex-parse-keys-timeout seconds, all BibTeX buffers (starting
  ;; with the current) are parsed.
  (let ((buffers (buffer-list)))
    (save-excursion
      (while (and buffers (not (input-pending-p)))
        (set-buffer (car buffers))
        (save-restriction
          (widen)
          (if (and
               (eq major-mode 'bibtex-mode)
               bibtex-maintain-sorted-entries
               (not
                (eq (buffer-modified-tick)
                    bibtex-buffer-last-parsed-tick)))
	      ;; Output no progress messages in bibtex-parse-keys
	      ;; because when in y-or-n-p that can hide the question.
              (if (bibtex-parse-keys nil nil t)
                  ;; successful operation --> remove buffer from list
                  (setq buffers (cdr buffers)))
            ;; buffer is no BibTeX buffer or needs no parsing
            (setq buffers (cdr buffers))))))))

(defun bibtex-complete (string-list &optional complete-strings)
  ;; Complete word fragment before point to longest prefix of one
  ;; string defined in STRING-LIST. If point is not after the part of
  ;; a word, all strings are listed. If COMPLETE-STRINGS is non-nil,
  ;; add the strings defined in this buffer before cursor to
  ;; STRING-LIST and remove surrounding delimiters if complete string
  ;; could be expanded.
  (let* ((case-fold-search t)
         (end (point))
         (beg (save-excursion
                (re-search-backward "[ \t{\"]")
                (forward-char)
                (point)))
         (part-of-word (buffer-substring-no-properties beg end))
         (completions (copy-sequence string-list))
         (completion (save-excursion
                       (if complete-strings
                           (while (bibtex-search-backward-string)
			     (let ((bounds (bibtex-search-backward-string)))
			       (setq completions
				     (cons
				      (list
				       (buffer-substring-no-properties
					(bibtex-start-of-reference-key-in-string bounds)
					(bibtex-end-of-reference-key-in-string bounds)))
				      completions)))))
                       (setq completions
                             (sort completions
                                   (lambda(x y)
                                     (string-lessp
                                      (car x)
                                      (car y)))))
                       (try-completion part-of-word completions))))
    (cond ((eq completion t)
           (if complete-strings
               ;; remove double-quotes or braces if field is no concatenation
               (save-excursion
                 (bibtex-inside-field)
                 (let* ((bounds (bibtex-enclosing-field)))
		   (goto-char (bibtex-start-of-text-in-field bounds))
		   (let ((boundaries (bibtex-parse-field-string)))
		     (if (and boundaries (equal (cdr boundaries) (bibtex-end-of-text-in-field bounds)))
			 (bibtex-remove-delimiters)))))))
          ((not completion)
           (error "Can't find completion for \"%s\"" part-of-word))
          ((not (string= part-of-word completion))
           (delete-region beg end)
           (insert completion)
           (if (and (assoc completion completions)
                    complete-strings)
               ;; remove double-quotes or braces if field is no concatenation
               (save-excursion
                 (bibtex-inside-field)
		 (let* ((bounds (bibtex-enclosing-field)))
		   (goto-char (bibtex-start-of-text-in-field bounds))
		   (let ((boundaries (bibtex-parse-field-string)))
		     (if (and boundaries (equal (cdr boundaries) (bibtex-end-of-text-in-field bounds)))
			 (bibtex-remove-delimiters)))))))
          (t
           (message "Making completion list...")
           (let ((list (all-completions part-of-word completions)))
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list list)))
           (message "Making completion list...done")))))

(defun bibtex-do-auto-fill ()
  (let ((fill-prefix
         (make-string
          (+ bibtex-entry-offset bibtex-contline-indentation) ? )))
    (do-auto-fill)))

(defun bibtex-pop (arg direction)
  ;; generic function to be used by bibtex-pop-previous and bibtex-pop-next
  (let (bibtex-help-message)
    (bibtex-find-text nil))
  (save-excursion
    ;; parse current field
    (bibtex-inside-field)
    (let* ((case-fold-search t)
	   (bounds (bibtex-enclosing-field))
	   (start-old-text (bibtex-start-of-text-in-field bounds))
	   (stop-old-text (bibtex-end-of-text-in-field bounds))
	   (start-name (bibtex-start-of-name-in-field bounds))
	   (stop-name (bibtex-end-of-name-in-field bounds))
	   (new-text))
      (goto-char start-name)
      ;; construct regexp for field with same name as this one,
      ;; ignoring possible OPT's or ALT's
      (let* ((field-name
	      (buffer-substring-no-properties
	       (if (looking-at "\\(OPT\\)\\|\\(ALT\\)")
		   (match-end 0)
		 (point))
	       stop-name)))
	;; if executed several times in a row, start each search where
        ;; the last one was finished
	(cond ((eq last-command 'bibtex-pop) t)
	      (t
	       (bibtex-enclosing-entry-maybe-empty-head)
	       (setq
                bibtex-pop-previous-search-point (match-beginning 0)
                bibtex-pop-next-search-point (point))))
	(if (eq direction 'previous)
            (goto-char bibtex-pop-previous-search-point)
          (goto-char bibtex-pop-next-search-point))
        ;; Now search for arg'th previous/next similar field
	(let ((bounds nil)
	      (failure nil))
	  (while (and (not failure) (> arg 0))
	    (cond ((eq direction 'previous)
		   (setq bounds (bibtex-search-backward-field field-name (point-min)))
		   (if bounds 
		       (goto-char (bibtex-start-of-field bounds))
		     (setq failure t)))
		  ((eq direction 'next)
		   (setq bounds (bibtex-search-forward-field field-name (point-max)))
		   (if bounds 
		       (goto-char (bibtex-end-of-field bounds))
		     (setq failure t))))
	    (setq arg (- arg 1)))
	  (if failure
	      (error (if (eq direction 'previous)
			 "No previous matching BibTeX field."
		       "No next matching BibTeX field."))
	    ;; Found a matching field. Remember boundaries.
	    (setq bibtex-pop-previous-search-point (bibtex-start-of-field bounds))
	    (setq bibtex-pop-next-search-point (bibtex-end-of-field bounds))
	    (setq new-text
		  (buffer-substring-no-properties
		   (bibtex-start-of-text-in-field bounds)
		   (bibtex-end-of-text-in-field bounds)))
	    (bibtex-flash-head)
	    ;; Go back to where we started, delete old text, and pop new.
	    (goto-char stop-old-text)
	    (delete-region start-old-text stop-old-text)
	    (insert new-text))))))
  (let (bibtex-help-message)
    (bibtex-find-text nil))
  (setq this-command 'bibtex-pop))


;; Interactive Functions:

;;;###autoload
(defun bibtex-mode ()
  "Major mode for editing BibTeX files.

To submit a problem report, enter \\[bibtex-submit-bug-report] from a
BibTeX mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducable test case and send the
message.


General information on working with BibTeX mode:

You should use commands as \\[bibtex-Book] to get a template for a
specific entry. You should then fill in all desired fields using
\\[bibtex-next-field] to jump from field to field. After having filled
in all desired fields in the entry, you should clean the new entry
with command \\[bibtex-clean-entry].

Some features of BibTeX mode are available only by setting variable
bibtex-maintain-sorted-entries to t. However, then BibTeX mode will
work with buffer containing only valid (syntactical correct) entries
and with entries being sorted. This is usually the case, if you have
created a buffer completely with BibTeX mode and finished every new
entry with \\[bibtex-clean-entry].

For third party BibTeX buffers, please call the function
`bibtex-convert-alien' to fully take advantage of all features of
BibTeX mode.


Special information:

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and are thus ignored by BibTeX.
Alternatives from which only one is required start with the string ALT.
The OPT or ALT string may be removed from a field with \\[bibtex-remove-OPT-or-ALT].
\\[bibtex-make-field] inserts a new field after the current one.
\\[bibtex-kill-field] kills the current field entirely.
\\[bibtex-yank] will yank the last recently killed field after the
current field.
\\[bibtex-remove-delimiters] removes the double-quotes or braces around the text of the current field.
 \\[bibtex-empty-field] replaces the text of the current field with the default \"\" or {}.

The command \\[bibtex-clean-entry] cleans the current entry, i.e. it removes OPT/ALT
from all non-empty optional or alternative fields, checks that no required
fields are empty, and does some formatting dependent on the value of
bibtex-entry-format.
Note: some functions in BibTeX mode depend on entries being in a special
format (all fields beginning on separate lines), so it is usually a bad
idea to remove `realign' from bibtex-entry-format.

Use \\[bibtex-find-text] to position the cursor at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

The following may be of interest as well:

  Functions:
    bibtex-entry
    bibtex-kill-entry
    bibtex-yank-pop
    bibtex-pop-previous
    bibtex-pop-next
    bibtex-complete-string
    bibtex-complete-key
    bibtex-print-help-message
    bibtex-generate-autokey
    bibtex-beginning-of-entry
    bibtex-end-of-entry
    bibtex-reposition-window
    bibtex-mark-entry
    bibtex-ispell-abstract
    bibtex-ispell-entry
    bibtex-narrow-to-entry
    bibtex-sort-buffer
    bibtex-validate
    bibtex-count
    bibtex-fill-entry
    bibtex-reformat
    bibtex-convert-alien

  Variables:
    bibtex-field-delimiters
    bibtex-include-OPTcrossref
    bibtex-include-OPTkey
    bibtex-user-optional-fields
    bibtex-entry-format
    bibtex-sort-ignore-string-entries
    bibtex-maintain-sorted-entries
    bibtex-entry-field-alist
    bibtex-predefined-strings
    bibtex-string-files

---------------------------------------------------------
Entry to BibTeX mode calls the value of `bibtex-mode-hook' if that value is
non-nil.

\\{bibtex-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)
  (setq mode-name "BibTeX")
  (set-syntax-table bibtex-mode-syntax-table)
  (setq bibtex-strings bibtex-predefined-strings)
  (mapcar
   (lambda (filename)
     ;; collect pathnames
     (let* ((path (if bibtex-string-file-path
                      bibtex-string-file-path
                    "."))
            (dirs (split-string path ":+"))
            (filename (if (string-match "\.bib$" filename)
                          filename
                        (concat filename ".bib")))
            fullfilename
            (item 0)
            (size (length dirs)))
       ;; test filenames
       (while (and
               (< item size)
               (not (file-readable-p
                     (setq fullfilename
                           (concat (elt dirs item) "/" filename)))))
         (setq item (1+ item)))
       (if (< item size)
           ;; file was found
           (let* ((case-fold-search t)
		  (compl nil))
	     (with-temp-buffer
	       (insert-file-contents fullfilename)
	       (goto-char (point-min))
	       (let ((bounds (bibtex-search-forward-string)))
		 (while bounds
		   (setq compl
			 (cons (list (buffer-substring-no-properties
				      (bibtex-start-of-reference-key-in-string bounds)
				      (bibtex-end-of-reference-key-in-string bounds)))
			       compl))
		   (goto-char (bibtex-end-of-string bounds))
		   (setq bounds (bibtex-search-forward-string)))))
	     (setq bibtex-strings (append bibtex-strings (nreverse compl))))
         (error
          "File %s not in paths defined by bibtex-string-file-path variable"
          filename))))
   bibtex-string-files)
  (if bibtex-maintain-sorted-entries
      (bibtex-run-with-idle-timer
       1 nil
       (lambda ()
         (bibtex-parse-keys nil nil t))))
  ;; to get buffer parsed once if everything else (including things
  ;; installed in bibtex-mode-hook) has done its work
  (if (not bibtex-parse-idle-timer)
      (setq bibtex-parse-idle-timer
            (bibtex-run-with-idle-timer
             bibtex-parse-keys-timeout t
             'bibtex-parse-buffers-stealthily)))
  ;; Install stealthy parse function if not already installed
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "[ \f\n\t]*$")
  (make-local-variable 'comment-start)
  (setq comment-start bibtex-comment-start)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip bibtex-comment-start)
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  (make-local-variable 'defun-prompt-regexp)
  (setq defun-prompt-regexp "^@[a-zA-Z0-9]+")
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "@")
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'bibtex-do-auto-fill)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(bibtex-font-lock-keywords
	  nil t ((?$ . "\"")
		 ;; Mathematical expressions should be fontified as strings
		 (?\" . ".")
		 ;; Quotes are field delimiters and quote-delimited
		 ;; entries should be fontified in the same way as
		 ;; brace-delimited ones
		 )))
  (make-local-variable 'font-lock-mark-block-function)
  (setq font-lock-mark-block-function
	(lambda ()
	  (set-mark (bibtex-end-of-entry))
	  (bibtex-beginning-of-entry)))
  (setq imenu-generic-expression
	(list (list nil bibtex-entry-head bibtex-key-in-head)))
  (setq imenu-case-fold-search t)
  ;; XEmacs needs easy-menu-add, Emacs does not care
  (easy-menu-add bibtex-edit-menu)
  (easy-menu-add bibtex-entry-menu)
  (run-hooks 'bibtex-mode-hook))

(defun bibtex-submit-bug-report ()
  "Submit via mail a bug report on bibtex.el."
  (interactive)
  (if (y-or-n-p "Do you want to submit a bug report on BibTeX mode? ")
      (progn
        (require 'reporter)
        (let ((reporter-prompt-for-summary-p t))
          (reporter-submit-bug-report
           bibtex-maintainer-address
           "bibtex.el"
           (list
            'system-configuration
            'system-configuration-options
            'bibtex-mode-hook
            'bibtex-parse-keys-timeout
            ;; possible general errors
            'bibtex-sort-ignore-string-entries
            'bibtex-maintain-sorted-entries
            'bibtex-entry-delimiters
            'bibtex-field-delimiters
            'bibtex-comma-after-last-field
            'bibtex-entry-offset
            'bibtex-field-indentation
            'bibtex-text-indentation
            'bibtex-contline-indentation
            'bibtex-align-at-equal-sign
            ;; possible sorting and parsing bugs
            'bibtex-entry-format
            'bibtex-add-entry-hook
            'bibtex-clean-entry-hook
            ;; possible cleaning error
            'bibtex-user-optional-fields
            ;; possible format error
            'bibtex-predefined-month-strings
            'bibtex-predefined-strings
            'bibtex-string-files
            'bibtex-string-file-path
            ;; possible format error
            'bibtex-font-lock-keywords
            ;; possible bugs regarding fontlocking
            'bibtex-autokey-prefix-string
            'bibtex-autokey-names
            'bibtex-autokey-names-stretch
            'bibtex-autokey-additional-names
            'bibtex-autokey-transcriptions
            'bibtex-autokey-name-change-strings
            'bibtex-autokey-name-case-convert
            'bibtex-autokey-name-length
            'bibtex-autokey-name-separator
            'bibtex-autokey-year-length
            'bibtex-autokey-year-use-crossref-entry
            'bibtex-autokey-titlewords
            'bibtex-autokey-title-terminators
            'bibtex-autokey-titlewords-stretch
            'bibtex-autokey-titleword-ignore
            'bibtex-autokey-titleword-case-convert
            'bibtex-autokey-titleword-abbrevs
            'bibtex-autokey-titleword-change-strings
            'bibtex-autokey-titleword-length
            'bibtex-autokey-titleword-separator
            'bibtex-autokey-name-year-separator
            'bibtex-autokey-year-title-separator
            'bibtex-autokey-edit-before-use
            'bibtex-autokey-before-presentation-function
            ;; possible bugs regarding automatic reference keys
            'bibtex-entry-field-alist
            ;; possible format error
            'bibtex-help-message
            'bibtex-include-OPTcrossref
            'bibtex-include-OPTkey
            'bibtex-field-kill-ring-max
            'bibtex-entry-kill-ring-max
            ;; user variables which shouldn't cause any errors
            )
           nil nil
           (concat bibtex-maintainer-salutation "

I want to report a bug on Emacs BibTeX mode.

I've read the `Bugs' section in the `Emacs' info page, so I know how
to make a clear and unambiguous report. I have started a fresh Emacs
via `"invocation-name " --no-init-file --no-site-file', thereafter (in
case I'm reporting on a version of `bibtex.el' which is not part of
the standard emacs distribution) I loaded the questionable version
of `bibtex.el' with `M-x load-file', and then, to produce the buggy
behaviour, I did the following:")))
        (message nil))))

(defun bibtex-entry (entry-type)
  "Insert a new BibTeX entry.
After insertion it calls the functions in `bibtex-add-entry-hook'."
  (interactive (let* ((completion-ignore-case t)
		      (e-t (completing-read
                            "Entry Type: "
                            bibtex-entry-field-alist
                            nil t nil 'bibtex-entry-type-history)))
		 (list e-t)))
  (if (not bibtex-reference-keys)
      (bibtex-parse-keys nil t))
  (let* (required
         optional
         (key
          (if bibtex-maintain-sorted-entries
              (completing-read
               (format "%s key: " entry-type)
               bibtex-reference-keys nil nil nil 'bibtex-key-history)))
         (e (assoc-ignore-case entry-type bibtex-entry-field-alist))
         (r-n-o (elt e 1))
         (c-ref (elt e 2)))
    (if (not e)
        (error "Bibtex entry type %s not defined" entry-type))
    (if (and
         (member entry-type bibtex-include-OPTcrossref)
         c-ref)
        (setq required (elt c-ref 0)
              optional (elt c-ref 1))
      (setq required (elt r-n-o 0)
            optional (elt r-n-o 1)))
    (if bibtex-maintain-sorted-entries
	(bibtex-find-entry-location key)
      (bibtex-move-outside-of-entry))
    (indent-to-column bibtex-entry-offset)
    (insert "@" entry-type (bibtex-entry-left-delimiter))
    (if key
	(insert key))
    (save-excursion
      (mapcar 'bibtex-make-field required)
      (if (member entry-type bibtex-include-OPTcrossref)
	  (bibtex-make-optional-field '("crossref")))
      (if bibtex-include-OPTkey
          (if (or
               (stringp bibtex-include-OPTkey)
               (fboundp bibtex-include-OPTkey))
              (bibtex-make-optional-field
               (list "key" nil bibtex-include-OPTkey))
            (bibtex-make-optional-field '("key"))))
      (mapcar 'bibtex-make-optional-field optional)
      (mapcar 'bibtex-make-optional-field bibtex-user-optional-fields)
      (if bibtex-comma-after-last-field
          (insert ","))
      (insert "\n")
      (indent-to-column bibtex-entry-offset)
      (insert (bibtex-entry-right-delimiter) "\n\n"))
    (bibtex-next-field t)
    (run-hooks 'bibtex-add-entry-hook)))

(defun bibtex-print-help-message ()
  "Prints helpful information about current field in current BibTeX entry."
  (interactive)
  (let* ((case-fold-search t)
         (pnt (point))
         (field-name
	  (let* ((bounds (bibtex-enclosing-field))
		 (mb (bibtex-start-of-name-in-field bounds))
		 (me (bibtex-end-of-name-in-field bounds)))
	    (goto-char mb)
	    (buffer-substring-no-properties
	     (if (looking-at "OPT\\|ALT")
		 (+ 3 mb)
	       mb)
	     me)))
         (entry-type
          (progn
            (re-search-backward
             bibtex-entry-maybe-empty-head nil t)
            (buffer-substring-no-properties
             (1+ (match-beginning bibtex-type-in-head))
             (match-end bibtex-type-in-head))))
         (entry-list
          (assoc-ignore-case entry-type
                               bibtex-entry-field-alist))
         (c-r-list (elt entry-list 2))
         (req-opt-list
          (if (and
               (member entry-type bibtex-include-OPTcrossref)
               c-r-list)
              c-r-list
            (elt entry-list 1)))
         (list-of-entries (append
                           (elt req-opt-list 0)
                           (elt req-opt-list 1)
                           bibtex-user-optional-fields
                           (if (member
                                entry-type
                                bibtex-include-OPTcrossref)
                               '(("crossref"
                                  "Reference key of the crossreferenced entry")))
                           (if bibtex-include-OPTkey
                               '(("key"
                                  "Used for reference key creation if author and editor fields are missing"))))))
    (goto-char pnt)
    (let ((comment (assoc-ignore-case field-name list-of-entries)))
      (if comment
          (message (elt comment 1))
        (message "NO COMMENT AVAILABLE")))))

(defun bibtex-make-field (e-t &optional called-by-yank)
  "Makes a field named E-T in current BibTeX entry.
This function is for interactive and non-interactive purposes.  To call
it interactively, just give it no arguments and enter the field name
using the minibuffer."
  (interactive "*P")
  (if (not e-t)
      (setq
       e-t
       (let* ((entry-type
               (save-excursion
                 (bibtex-enclosing-entry-maybe-empty-head)
                 (buffer-substring-no-properties
                  (1+ (match-beginning bibtex-type-in-head))
                  (match-end bibtex-type-in-head))))
              (fl
               (car (cdr (assoc-ignore-case
                          entry-type bibtex-entry-field-alist))))
              (field-list
               (append
                (elt fl 0) (elt fl 1) bibtex-user-optional-fields
                (if bibtex-include-OPTcrossref '(("crossref" nil)))
                (if bibtex-include-OPTkey '(("key" nil)))))
              (completion-ignore-case t))
         (completing-read
          "BibTeX field name: " field-list
          nil nil nil bibtex-field-history))))
  (if (not (consp e-t))
      (setq e-t (list e-t)))
  (if (equal (length e-t) 1)
      (setq e-t (append e-t (list ""))))
  (if (equal (length e-t) 2)
      (setq e-t (append e-t (list ""))))
  (let ((name (if (elt e-t 3)
                  (concat "ALT" (car e-t))
                (car e-t))))
    (if (or (interactive-p) called-by-yank)
        (let (bibtex-help-message)
          (bibtex-find-text nil t t)
          (if (looking-at "[}\"]")
              (forward-char))))
    (insert ",\n")
    (indent-to-column
     (+ bibtex-entry-offset bibtex-field-indentation))
    (insert name " ")
    (if bibtex-align-at-equal-sign
        (indent-to-column
         (+ bibtex-entry-offset (- bibtex-text-indentation 2))))
    (insert "= ")
    (if (not bibtex-align-at-equal-sign)
        (indent-to-column
         (+ bibtex-entry-offset bibtex-text-indentation)))
    (insert (if called-by-yank
                ""
              (bibtex-field-left-delimiter))
            (let ((init (elt e-t 2)))
              (cond
               ((stringp init)
                init)
               ((fboundp init)
                (funcall init))
               (t
                (error "%s is neither a string nor a function" init))))
            (if called-by-yank
                ""
              (bibtex-field-right-delimiter)))
    (if (interactive-p)
        (forward-char -1))))

(defun bibtex-beginning-of-entry ()
  "Move to beginning of BibTeX entry.
If inside an entry, move to the beginning of it, otherwise move to the
beginning of the previous entry.
If called from a program, this function returns the new location of point."
  (interactive)
  (skip-chars-forward " \t")
  (if (looking-at "@")
      (forward-char))
  (re-search-backward "^[ \t]*@" nil 'move))

(defun bibtex-end-of-entry ()
  "Move to end of BibTeX entry.
If inside an entry, move to the end of it, otherwise move to the end
of the previous entry.
If called from a program, this function returns the new location of point."
  (interactive)
  (let* ((case-fold-search t)
	 (valid-entry-head
	  (concat "[ \t]*@[ \t]*\\("
		  (mapconcat
		   (lambda (type)
		     (concat "\\(" (car type) "\\)"))
		   bibtex-entry-field-alist
		   "\\|")
		  "\\)"))
	 (org (point))
	 (pnt (bibtex-beginning-of-entry))
	 err)
    (cond
     ((bibtex-parse-string)
      (let ((bounds (bibtex-parse-string)))
	(goto-char (bibtex-end-of-string bounds))))
     ((looking-at "[ \t]*@[ \t]*preamble[ \t\n]*")
      (goto-char (match-end 0))
      (cond
       ((looking-at "(")
        (if (not (re-search-forward ")[ \t]*\n\n" nil 'move))
            (setq err t)))
       ((looking-at "{")
        (if (not (re-search-forward "}[ \t]*\n\n" nil 'move))
            (setq err t)))
       (t
        (setq err t)))
      (if (not err)
          (progn
            (goto-char (match-beginning 0))
            (forward-char))))
     ((looking-at valid-entry-head)
      (bibtex-search-entry t nil t)
      (if (not (equal (match-beginning 0) pnt))
          (setq err t)))
     (t
      (if (interactive-p)
          (message "Not on a known BibTeX entry."))
      (goto-char org)))
    (if err
        (progn
          (goto-char pnt)
          (error "Syntactical incorrect entry starts here"))))
  (point))

(defun bibtex-reposition-window (arg)
  "Make the current BibTeX entry visible."
  (interactive "P")
  (save-excursion
    (goto-char
     (/ (+ (bibtex-beginning-of-entry) (bibtex-end-of-entry)) 2))
    (recenter arg)))

(defun bibtex-mark-entry ()
  "Put mark at beginning, point at end of current BibTeX entry."
  (interactive)
  (set-mark (bibtex-beginning-of-entry))
  (bibtex-end-of-entry))

(defun bibtex-count-entries (&optional count-string-entries)
  "Count number of entries in current buffer or region.
With prefix argument COUNT-STRING-ENTRIES it counts all entries,
otherwise it counts all except Strings.
If mark is active it counts entries in region, if not in whole buffer."
  (interactive "P")
  (let ((pnt (point))
        (start-point
         (if (bibtex-mark-active)
             (region-beginning)
           (bibtex-beginning-of-first-entry)))
        (end-point
         (if (bibtex-mark-active)
             (region-end)
           (point-max)))
        (number 0)
        (bibtex-sort-ignore-string-entries
         (not count-string-entries)))
    (save-restriction
      (narrow-to-region start-point end-point)
      (goto-char start-point)
      (bibtex-map-entries
       (lambda (current)
         (setq number (1+ number)))))
    (message (concat (if (bibtex-mark-active) "Region" "Buffer")
                     " contains %d entries.") number)
    (goto-char pnt)))

(defun bibtex-ispell-entry ()
  "Spell whole BibTeX entry."
  (interactive)
  (ispell-region (bibtex-beginning-of-entry) (bibtex-end-of-entry)))

(defun bibtex-ispell-abstract ()
  "Spell abstract of BibTeX entry."
  (interactive)
  (let* ((case-fold-search t)
	 (pnt (bibtex-end-of-entry)))
    (bibtex-beginning-of-entry)
    (let ((bounds (bibtex-search-forward-field "abstract" pnt)))
      (if bounds
	  (ispell-region (bibtex-start-of-text-in-field bounds) (bibtex-end-of-text-in-field bounds))
	(error "No abstract in entry")))))

(defun bibtex-narrow-to-entry ()
  "Narrow buffer to current BibTeX entry."
  (interactive)
  (save-excursion
    (narrow-to-region
     (bibtex-beginning-of-entry) (bibtex-end-of-entry))))

(defun bibtex-sort-buffer ()
  "Sort BibTeX buffer alphabetically by key.
Text outside of BibTeX entries is not affected.  If
`bibtex-sort-ignore-string-entries' is non-nil, @String entries will be
ignored."
  (interactive)
  (save-restriction
    (narrow-to-region
     (bibtex-beginning-of-first-entry)
     (save-excursion
       (goto-char (point-max))
       (bibtex-end-of-entry)))
    (bibtex-skip-to-valid-entry)
    (sort-subr
     nil
     ;; NEXTREC function
     'bibtex-skip-to-valid-entry
     ;; ENDREC function
     'bibtex-end-of-entry
     ;; STARTKEY function
     (lambda ()
       (let* ((case-fold-search t))
         (re-search-forward bibtex-entry-head)
         (buffer-substring-no-properties
          (match-beginning bibtex-key-in-head)
          (match-end bibtex-key-in-head)))))))

(defun bibtex-find-entry-location (entry-name &optional ignore-dups)
  "Looking for place to put the BibTeX entry named ENTRY-NAME.
Performs a binary search (therefore, buffer is assumed to be in sorted
order, without duplicates (see \\[bibtex-validate]), if it is
not, `bibtex-find-entry-location' will fail).  If entry-name is already
used as a reference key, an error is signaled.  However, if optional
variable IGNORE-DUPS is non-nil, no error messages about duplicate
entries are signaled, but the error handling is assumed to be made in
the calling function.
The value is nil if an duplicate entry error occurred,
and t in all other cases."
  (let* ((case-fold-search t)
         (left
          (progn
            (bibtex-beginning-of-first-entry)
            (bibtex-skip-to-valid-entry)
            (bibtex-end-of-entry)))
         (right
          (progn
            (bibtex-beginning-of-last-entry)
            (bibtex-skip-to-valid-entry t)
            (point)))
         actual-point
         actual-key
         (done (>= left right))
         new
         dup)
    (while (not done)
      (setq actual-point (/ (+ left right) 2))
      (goto-char actual-point)
      (bibtex-skip-to-valid-entry t)
      (setq actual-key
            (progn
              (re-search-forward bibtex-entry-head)
              (buffer-substring-no-properties
               (match-beginning bibtex-key-in-head)
               (match-end bibtex-key-in-head))))
      (cond
       ((string-lessp entry-name actual-key)
        (setq new (bibtex-beginning-of-entry))
        (if (equal right new)
            (setq done t)
          (setq right new)))
       ((string-lessp actual-key entry-name)
        (setq new (bibtex-end-of-entry))
        (if (equal left new)
            (setq done t)
          (setq left new)))
       ((string-equal actual-key entry-name)
        (setq dup t
              done t)
        (if (not ignore-dups)
            (progn
              (bibtex-beginning-of-entry)
              (error "Entry with key `%s' already exists" entry-name))))))
    (if dup
        (progn
          (bibtex-beginning-of-entry)
          nil)
      (goto-char right)
      (setq actual-key
            (if (looking-at bibtex-entry-head)
                (buffer-substring-no-properties
                 (match-beginning bibtex-key-in-entry)
                 (match-end bibtex-key-in-entry))))
      (if (or
           (not actual-key)
           (string-lessp actual-key entry-name))
          ;; buffer contains no valid entries or
          ;; greater than last entry --> append
          (progn
            (bibtex-end-of-entry)
            (if (not (bobp))
                (newline (forward-line 2)))
            (beginning-of-line))
        (goto-char right))
      t)))

(defun bibtex-validate (&optional test-thoroughly)
  "Validate if buffer or region is syntactically correct.
Only known entry types are checked, so you can put comments
outside of entries.
With optional argument TEST-THOROUGHLY non-nil it checks for absence of
required fields and questionable month fields as well.
If mark is active, it validates current region, if not whole buffer.
Returns t if test was successful, nil otherwise."
  (interactive "P")
  (let* (error-list
	 syntax-error
	 (case-fold-search t)
	 (valid-bibtex-entry
	  (concat
	   "@[ \t]*\\(\\(string\\)\\|"
	   (mapconcat
	    (lambda (type)
	      (concat "\\(" (car type) "\\)"))
	    bibtex-entry-field-alist
	    "\\|")
	   "\\)"))
	 (pnt (point))
	 (start-point
	  (if (bibtex-mark-active)
	      (region-beginning)
	    (bibtex-beginning-of-first-entry)))
	 (end-point
	  (if (bibtex-mark-active)
	      (region-end)
	    (point-max))))
    (save-restriction
      (narrow-to-region start-point end-point)
      ;; looking if entries fit syntactical structure
      (goto-char start-point)
      (bibtex-progress-message "Checking syntactical structure")
      (while (re-search-forward "^[ \t]*@" nil t)
        (bibtex-progress-message)
        (forward-char -1)
        (let ((p (point))
              (must-match
               (looking-at valid-bibtex-entry)))
          (if (not must-match)
              (forward-char)
            (let (bibtex-sort-ignore-string-entries)
              (bibtex-skip-to-valid-entry))
            (if (equal (point) p)
                (forward-char)
              (goto-char p)
              (setq
               error-list
               (cons (list
                      (bibtex-current-line)
                      "Syntax error (check esp. commas, braces, and quotes)")
                     error-list))
              (forward-char)))))
      (bibtex-progress-message 'done)
      (if error-list
          (setq syntax-error t)
        ;; looking for correct sort order and duplicates (only if
        ;; there were no syntax errors)
        (if bibtex-maintain-sorted-entries
            (let (previous)
              (goto-char start-point)
              (bibtex-progress-message "Checking correct sort order")
              (bibtex-map-entries
               (lambda (current)
                 (bibtex-progress-message)
                 (cond ((or (not previous)
                            (string< previous current))
                        (setq previous current))
                       ((string-equal previous current)
                        (setq
                         error-list
                         (cons (list (bibtex-current-line)
                                     "Duplicate key with previous")
                               error-list)))
                       (t
                        (setq previous current
                              error-list
                              (cons (list (bibtex-current-line)
                                          "Entries out of order")
                                    error-list))))))
              (bibtex-progress-message 'done)))
        (if test-thoroughly
            (progn
              (goto-char start-point)
              (bibtex-progress-message
               "Checking required fields and month fields")
              (let ((bibtex-sort-ignore-string-entries t)
                    (questionable-month
                     (concat
                      "[{\"]\\("
                      (mapconcat
                       (lambda (mon)
                         (concat "\\(" (car mon) "\\)"))
                       bibtex-predefined-month-strings
                       "\\|")
                      "\\)[}\"]")))
                (bibtex-map-entries
                 (lambda (current)
                   (bibtex-progress-message)
                   (let* ((beg (bibtex-beginning-of-entry))
                          (end (bibtex-end-of-entry))
                          (entry-list
                           (progn
                             (goto-char beg)
                             (bibtex-search-entry nil end)
                             (assoc-ignore-case
                              (buffer-substring-no-properties
                               (1+ (match-beginning bibtex-type-in-head))
                               (match-end bibtex-type-in-head))
                              bibtex-entry-field-alist)))
                          (req (copy-sequence (elt (elt entry-list 1) 0)))
                          (creq (copy-sequence (elt (elt entry-list 2) 0)))
                          crossref-there)
                     (goto-char beg)
		     (let ((bounds (bibtex-search-forward-field bibtex-field-name end)))
		       (while bounds
			 (let ((field-name
				(buffer-substring-no-properties
				 (bibtex-start-of-name-in-field bounds)
				 (bibtex-end-of-name-in-field bounds))))
			   (if (and (equal (downcase field-name) "month")
				    (string-match
				     questionable-month
				     (buffer-substring-no-properties
				      (bibtex-start-of-text-in-field bounds)
				      (bibtex-end-of-text-in-field bounds))))
			       (setq
				error-list
				(cons
				 (list
				  (bibtex-current-line)
				  "Questionable month field (delimited string)")
				 error-list)))
			   (setq
			    req
			    (delete (assoc-ignore-case field-name req) req)
			    creq
			    (delete (assoc-ignore-case field-name creq) creq))
			   (if (equal (downcase field-name) "crossref")
			       (setq crossref-there t)))
			 (goto-char (bibtex-end-of-field bounds))
			 (setq bounds (bibtex-search-forward-field bibtex-field-name end))))
                     (if crossref-there
                         (setq req creq))
                     (if (or (> (length req) 1)
                             (and (= (length req) 1)
                                  (not (elt (car req) 3))))
                         ;; two (or more) fields missed or one field
                         ;; missed and this isn't flagged alternative
                         ;; (notice that this fails if there are more
                         ;; than two alternatives in a BibTeX entry,
                         ;; which isn't the case momentarily)
                         (setq
                          error-list
                          (cons
                           (list (save-excursion
                                   (bibtex-beginning-of-entry)
                                   (bibtex-current-line))
                                 (concat
                                  "Required field \""
                                  (car (car req))
                                  "\" missing"))
                           error-list)))))))
              (bibtex-progress-message 'done)))))
    (goto-char pnt)
    (if error-list
        (let ((bufnam (buffer-name))
              (dir default-directory))
          (setq error-list
                (sort error-list
                      (lambda (a b)
                        (< (car a) (car b)))))
          (let ((pop-up-windows t))
            (pop-to-buffer nil t))
          (switch-to-buffer
           (get-buffer-create "*BibTeX validation errors*") t)
          ;; don't use switch-to-buffer-other-window, since this
          ;; doesn't allow the second parameter NORECORD
          (setq default-directory dir)
          (toggle-read-only -1)
          (compilation-mode)
          (delete-region (point-min) (point-max))
          (goto-char (point-min))
          (insert
           "BibTeX mode command `bibtex-validate'\n"
           (if syntax-error
               "Maybe undetected errors due to syntax errors. Correct and validate again."
             "")
           "\n")
          (while error-list
            (insert
             bufnam ":" (number-to-string (elt (car error-list) 0))
             ": " (elt (car error-list) 1) "\n")
            (setq error-list (cdr error-list)))
          (compilation-parse-errors nil nil)
          (setq compilation-old-error-list compilation-error-list)
          ;; this is necessary to avoid reparsing of buffer if you
          ;; switch to compilation buffer and enter
          ;; `compile-goto-error'
          (set-buffer-modified-p nil)
          (toggle-read-only 1)
          (goto-char (point-min))
          (other-window -1)
          ;; return nil
          nil)
      (if (bibtex-mark-active)
          (message "Region is syntactically correct")
        (message "Buffer is syntactically correct"))
      t)))

(defun bibtex-next-field (arg)
  "Finds end of text of next BibTeX field; with ARG, to its beginning."
  (interactive "P")
  (bibtex-inside-field)
  (let ((start (point)))
    (condition-case ()
	(let ((bounds (bibtex-enclosing-field)))
	  (goto-char (bibtex-end-of-field bounds))
	  (forward-char 2))
      (error
       (goto-char start)
       (end-of-line)
       (forward-char))))
  (bibtex-find-text arg t))

(defun bibtex-find-text (arg &optional as-if-interactive silent)
  "Go to end of text of current field; with ARG, go to beginning."
  (interactive "P")
  (bibtex-inside-field)
  (let ((bounds (bibtex-enclosing-field (or (interactive-p) as-if-interactive))))
    (if bounds
	(progn
	  (if arg
	      (progn
		(goto-char (bibtex-start-of-text-in-field bounds))
		(if (looking-at "[{\"]")
		    (forward-char)))
	    (goto-char (bibtex-end-of-text-in-field bounds))
	    (if (or
		 (= (preceding-char) ?})
		 (= (preceding-char) ?\"))
		(forward-char -1)))
	  (if bibtex-help-message
	      (bibtex-print-help-message)))
      (beginning-of-line)
      (cond
       ((bibtex-parse-string)
	(let ((bounds (bibtex-parse-string)))
	  (goto-char
	   (if arg
	       (bibtex-start-of-text-in-string bounds)
	     (bibtex-end-of-text-in-string bounds)))))
       ((looking-at bibtex-entry-maybe-empty-head)
	(goto-char
	 (if arg
	     (match-beginning bibtex-key-in-head)
	   (match-end 0))))
       (t
	(if (not silent)
	    (error "Not on BibTeX field")))))))

(defun bibtex-remove-OPT-or-ALT ()
  "Removes the string starting optional/alternative fields.
Aligns text and goes thereafter to end of text."
  (interactive)
  (bibtex-inside-field)
  (let* ((case-fold-search t)
	 (bounds (bibtex-enclosing-field)))
    (save-excursion
      (goto-char (bibtex-start-of-name-in-field bounds))
      (if (looking-at "OPT\\|ALT")
          (progn
            (delete-char (length "OPT"))
            ;; make field non-OPT
            (search-forward "=")
            (forward-char -1)
            (delete-horizontal-space)
            (if bibtex-align-at-equal-sign
                (indent-to-column (- bibtex-text-indentation 2))
              (insert " "))
            (search-forward "=")
            (delete-horizontal-space)
            (if bibtex-align-at-equal-sign
                (insert " ")
              (indent-to-column bibtex-text-indentation)))))
    (bibtex-inside-field)))

(defun bibtex-remove-delimiters ()
  "Removes \"\" or {} around string."
  (interactive)
  (save-excursion
    (bibtex-inside-field)
    (let* ((case-fold-search t)
	   (bounds (bibtex-enclosing-field))
	   (stop (copy-marker (bibtex-end-of-text-in-field bounds))))
      (goto-char (bibtex-start-of-text-in-field bounds))
      (let* ((boundaries (bibtex-search-forward-field-string stop)))
	(while boundaries
	  (let ((text-end (copy-marker (cdr boundaries))))
	    (goto-char (car boundaries))
	    (delete-char 1)
	    (goto-char text-end)
	    (delete-backward-char 1)
	    (setq boundaries (bibtex-search-forward-field-string stop))))))))

(defun bibtex-kill-field (&optional copy-only)
  "Kills the entire enclosing BibTeX field.
With prefix arg COPY-ONLY, copy the current field to `bibtex-field-kill-ring,'
but do not actually kill it."
  (interactive "P")
  (let* ((pnt (point))
	 (case-fold-search t))
    (bibtex-inside-field)
    (let* ((bounds (bibtex-enclosing-field))
	   (the-end (bibtex-end-of-field bounds))
	   (the-beginning (bibtex-start-of-field bounds)))
      (goto-char the-end)
      (skip-chars-forward " \t\n,")
      (setq
       bibtex-field-kill-ring
       (cons
        (list
         'field
         (buffer-substring-no-properties
          (bibtex-start-of-name-in-field bounds)
          (bibtex-end-of-name-in-field bounds))
         (buffer-substring-no-properties
          (bibtex-start-of-text-in-field bounds)
          (bibtex-end-of-text-in-field bounds)))
        bibtex-field-kill-ring))
      (if (> (length bibtex-field-kill-ring) bibtex-field-kill-ring-max)
          (setcdr
           (nthcdr (1- bibtex-field-kill-ring-max) bibtex-field-kill-ring)
           nil))
      (setq bibtex-field-kill-ring-yank-pointer bibtex-field-kill-ring)
      (if copy-only
          (goto-char pnt)
        (delete-region the-beginning the-end)
        (let (bibtex-help-message)
          (bibtex-find-text nil t t)))))
  (setq bibtex-last-kill-command 'field))

(defun bibtex-copy-field-as-kill ()
  (interactive)
  (bibtex-kill-field t))

(defun bibtex-kill-entry (&optional copy-only)
  "Kill the entire enclosing BibTeX entry.
With prefix arg COPY-ONLY the current entry to
`bibtex-entry-kill-ring', but do not actually kill it."
  (interactive "P")
  (let* ((pnt (point))
	 (case-fold-search t)
	 (beg (bibtex-beginning-of-entry))
	 (end
	  (progn
	    (bibtex-end-of-entry)
	    (if (re-search-forward
		 bibtex-entry-maybe-empty-head nil 'move)
		(goto-char (match-beginning 0)))
	    (point))))
    (setq
     bibtex-entry-kill-ring
     (cons
      (list 'entry (buffer-substring-no-properties beg end))
      bibtex-entry-kill-ring))
    (if (> (length bibtex-entry-kill-ring) bibtex-entry-kill-ring-max)
        (setcdr
         (nthcdr (1- bibtex-entry-kill-ring-max) bibtex-entry-kill-ring)
         nil))
    (setq bibtex-entry-kill-ring-yank-pointer bibtex-entry-kill-ring)
    (if copy-only
        (goto-char pnt)
      (delete-region beg end)))
  (setq bibtex-last-kill-command 'entry))

(defun bibtex-copy-entry-as-kill ()
  (interactive)
  (bibtex-kill-entry t))

(defun bibtex-yank (&optional n)
  "Reinsert the last BibTeX item.
More precisely, reinsert the field or entry killed or yanked most recently.
With argument N, reinsert the Nth most recently killed BibTeX item.
See also the command \\[bibtex-yank-pop]]."
  (interactive "*p")
  (bibtex-insert-current-kill (1- n))
  (setq this-command 'bibtex-yank))

(defun bibtex-yank-pop (n)
  "Replace just-yanked killed BibTeX item with a different.
This command is allowed only immediately after a `bibtex-yank' or a
`bibtex-yank-pop'.
At such a time, the region contains a reinserted previously killed
BibTeX item.  `bibtex-yank-pop' deletes that item and inserts in its
place a different killed BibTeX item.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'bibtex-yank))
      (error "Previous command was not a BibTeX yank"))
  (setq this-command 'bibtex-yank)
  (let ((inhibit-read-only t))
    (delete-region (point) (mark t))
    (bibtex-insert-current-kill n)))

(defun bibtex-empty-field ()
  "Delete the text part of the current field, replace with empty text."
  (interactive)
  (bibtex-inside-field)
  (let ((bounds (bibtex-enclosing-field)))
    (goto-char (bibtex-start-of-text-in-field bounds))
    (delete-region (point) (bibtex-end-of-text-in-field bounds))
    (insert (concat (bibtex-field-left-delimiter)
		    (bibtex-field-right-delimiter)) )
    (bibtex-find-text t)))

(defun bibtex-pop-previous (arg)
  "Replace text of current field with the similar field in previous entry.
With arg, goes up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-next] (bibtex-pop-next)."
  (interactive "p")
  (bibtex-pop arg 'previous))

(defun bibtex-pop-next (arg)
  "Replace text of current field with the text of similar field in next entry.
With arg, goes down ARG entries.  Repeated, goes down so many times.  May be
intermixed with \\[bibtex-pop-previous] (bibtex-pop-previous)."
  (interactive "p")
  (bibtex-pop arg 'next))

(defun bibtex-clean-entry (&optional new-reference-key called-by-reformat)
  "Finish editing the current BibTeX entry and clean it up.
Checks that no required fields are empty and formats entry dependent
on the value of bibtex-entry-format.
If the reference key of the entry is empty or a prefix argument is given,
calculate a new reference key (note: this only will work if fields in entry
begin on separate lines prior to calling bibtex-clean-entry or if 'realign is
contained in bibtex-entry-format).
Don't call this on `string' or `preamble' entries.
At end of the cleaning process, the functions in
bibtex-clean-entry-hook are called with region narrowed to entry."
  (interactive "P")
  (bibtex-format-entry)
  (let* ((case-fold-search t)
         (eob (bibtex-end-of-entry))
         (key (progn
                (bibtex-beginning-of-entry)
                (if (re-search-forward
                     bibtex-entry-head eob t)
                    (buffer-substring-no-properties
                     (match-beginning bibtex-key-in-head)
                     (match-end bibtex-key-in-head))))))
    (if (or
         new-reference-key
         (not key))
        (progn
          (let ((autokey
                 (if bibtex-autokey-edit-before-use
                     (read-from-minibuffer
                      "Key to use: " (bibtex-generate-autokey) nil nil
                      'bibtex-key-history)
                   (bibtex-generate-autokey))))
            (bibtex-beginning-of-entry)
            (re-search-forward bibtex-entry-maybe-empty-head)
            (if (match-beginning bibtex-key-in-head)
                (delete-region (match-beginning bibtex-key-in-head)
                               (match-end bibtex-key-in-head)))
            (insert autokey)
            (let* ((start (bibtex-beginning-of-entry))
                   (end (progn
                          (bibtex-end-of-entry)
                          (if (re-search-forward
                               bibtex-entry-maybe-empty-head nil 'move)
                              (goto-char (match-beginning 0)))
                          (point)))
                   (entry (buffer-substring start end)))
              (delete-region start end)
              (let ((success
                     (or
                      called-by-reformat
                      (not bibtex-maintain-sorted-entries)
                      (bibtex-find-entry-location autokey t))))
                (insert entry)
                (forward-char -1)
                (bibtex-beginning-of-entry)
                (re-search-forward bibtex-entry-head)
                (if (not success)
                    (error
                     "New inserted entry yields duplicate key"))))))))
  (if (not called-by-reformat)
      (save-excursion
        (save-restriction
          (narrow-to-region
           (bibtex-beginning-of-entry) (bibtex-end-of-entry))
          (bibtex-parse-keys t nil)
          (run-hooks 'bibtex-clean-entry-hook)))))

(defun bibtex-fill-entry ()
  "Fill current entry.
Realigns entry, so that every field starts on a separate line.  Field
names appear in column `bibtex-field-indentation', field text starts in
column `bibtex-text-indentation' and continuation lines start here, too.
If `bibtex-align-at-equal-sign' is non-nil, align equal signs also."
  (interactive "*")
  (let ((pnt (copy-marker (point)))
        (end (copy-marker (bibtex-end-of-entry))))
    (bibtex-beginning-of-entry)
    (bibtex-delete-whitespace)
    (indent-to-column bibtex-entry-offset)
    (let ((bounds (bibtex-search-forward-field bibtex-field-name end)))
      (while bounds
	(let* ((begin-field (copy-marker (bibtex-start-of-field bounds)))
	       (end-field (copy-marker (bibtex-end-of-field bounds)))
	       (begin-name (copy-marker (bibtex-start-of-name-in-field bounds)))
	       (end-name (copy-marker (bibtex-end-of-name-in-field bounds))))
	  (goto-char begin-field)
	  (forward-char)
	  (bibtex-delete-whitespace)
	  (open-line 1)
	  (forward-char)
	  (indent-to-column
	   (+ bibtex-entry-offset bibtex-field-indentation))
	  (re-search-forward "[ \t\n]*=" end)
	  (replace-match "=")
	  (forward-char -1)
	  (if bibtex-align-at-equal-sign
	      (indent-to-column
	       (+ bibtex-entry-offset (- bibtex-text-indentation 2)))
	    (insert " "))
	  (forward-char)
	  (bibtex-delete-whitespace)
	  (if bibtex-align-at-equal-sign
	      (insert " ")
	    (indent-to-column bibtex-text-indentation))
	  (while (re-search-forward "[ \t\n]+" end-field 'move)
	    (replace-match " "))
	  (bibtex-do-auto-fill)
	  (goto-char end-field))
	(setq bounds (bibtex-search-forward-field bibtex-field-name end))))
    (if (looking-at ",")
        (forward-char))
    (bibtex-delete-whitespace)
    (open-line 1)
    (forward-char)
    (indent-to-column bibtex-entry-offset)
    (goto-char pnt)))

(defun bibtex-reformat (&optional additional-options called-by-convert-alien)
  "Reformat all BibTeX entries in buffer or region.
With prefix argument, read options for reformatting from minibuffer.
With \\[universal-argument] \\[universal-argument] prefix argument, reuse previous answers (if any) again.
If mark is active it reformats entries in region, if not in whole buffer."
  (interactive "*P")
  (let* ((pnt (point))
         (use-previous-options
          (and (equal (prefix-numeric-value additional-options) 16)
               (or bibtex-reformat-previous-options
                   bibtex-reformat-previous-reference-keys)))
         (bibtex-entry-format
          (if additional-options
              (if use-previous-options
                  bibtex-reformat-previous-options
                (setq
                 bibtex-reformat-previous-options
                 (delq
                  nil
                  (list
                   (if (or
                        called-by-convert-alien
                        (y-or-n-p
                         "Realign entries (recommended for files not created by BibTeX mode)? "))
                       'realign)
                   (if (y-or-n-p
                        "Remove empty optional and alternative fields? ")
                       'opts-or-alts)
                   (if (y-or-n-p
                        "Remove delimiters around pure numerical fields? ")
                       'numerical-fields)
                   (if (y-or-n-p (concat
                                  (if bibtex-comma-after-last-field
                                      "Insert"
                                    "Remove")
                                  " comma at end of entry? "))
                       'last-comma)
                   (if (y-or-n-p
                        "Replace double page dashes by single ones? ")
                       'page-dashes)
                   (if (y-or-n-p
                        "Force delimiters? ")
                       'delimiters)
                   (if (y-or-n-p
                        "Unify case of entry types and field names? ")
                       'unify-case)))))
            '(realign)))
         (reformat-reference-keys
             (if additional-options
                 (if use-previous-options
                     bibtex-reformat-previous-reference-keys
                   (setq
                    bibtex-reformat-previous-reference-keys
                    (y-or-n-p "Generate new reference keys automatically? ")))))
         bibtex-autokey-edit-before-use
         (bibtex-sort-ignore-string-entries t)
         (start-point
          (if (bibtex-mark-active)
              (region-beginning)
            (progn
              (bibtex-beginning-of-first-entry)
              (bibtex-skip-to-valid-entry)
              (point))))
         (end-point
          (if (bibtex-mark-active)
              (region-end)
            (point-max)))
         (valid-bibtex-entry
          (concat
           "[ \t\n]+\\(@[ \t]*\\("
           (mapconcat
            (lambda (type)
              (concat "\\(" (car type) "\\)"))
            bibtex-entry-field-alist
            "\\|")
           "\\)\\)")))
    (save-restriction
      (narrow-to-region start-point end-point)
      (if (memq 'realign bibtex-entry-format)
          (progn
            (goto-char (point-min))
            (while (re-search-forward valid-bibtex-entry nil t)
              (replace-match "\n\\1"))))
      (goto-char start-point)
      (bibtex-progress-message "Formatting" 1)
      (bibtex-map-entries
       (lambda (current)
         (bibtex-progress-message)
         (bibtex-clean-entry reformat-reference-keys reformat-reference-keys)
         (if (memq 'realign bibtex-entry-format)
             (progn
               (bibtex-end-of-entry)
               (bibtex-delete-whitespace)
               (open-line 2)))))
      (bibtex-progress-message 'done))
    (if (and
         reformat-reference-keys
         bibtex-maintain-sorted-entries
         (not called-by-convert-alien))
        (progn
          (bibtex-sort-buffer)
          (setq bibtex-reference-keys nil)
          (bibtex-parse-keys nil t t)))
    (goto-char pnt)))

(defun bibtex-convert-alien (&optional do-additional-reformatting)
  "Converts an alien BibTeX buffer to be fully usable by BibTeX mode.
If a file doesn't confirm with some standards used by BibTeX mode,
some of the high-level features of BibTeX mode won't be available.
This function tries to convert current buffer to confirm with these standards.
With prefix argument DO-ADDITIONAL-REFORMATTING
non-nil, read options for reformatting entries from minibuffer."
  (interactive "*P")
  (message "Starting to validate buffer...")
  (sit-for 1 nil t)
  (goto-char (point-min))
  (while (re-search-forward "[ \t\n]+@" nil t)
    (replace-match "\n@"))
  (message
   "If errors occur, correct them and call `bibtex-convert-alien' again")
  (sit-for 5 nil t)
  (if (let ((bibtex-mark-active)
            bibtex-maintain-sorted-entries)
        (bibtex-validate))
      (progn
        (message "Starting to reformat entries...")
        (sit-for 2 nil t)
        (bibtex-reformat do-additional-reformatting t)
        (if bibtex-maintain-sorted-entries
            (progn
              (message "Starting to sort buffer...")
              (bibtex-sort-buffer)))
        (goto-char (point-max))
        (message "Buffer is now parsable. Please save it."))))

(defun bibtex-complete-string ()
  "Complete word fragment before point to longest prefix of a defined string.
If point is not after the part of a word, all strings are listed.
Remove surrounding delimiters if complete string could be expanded."
  (interactive "*")
  (bibtex-complete bibtex-strings t))

(defun bibtex-complete-key ()
  "Complete word fragment before point to longest prefix of a defined key.
If point is not after the part of a word, all keys are listed.  This
function is most useful in completing crossref entries."
  (interactive "*")
  (if (not bibtex-reference-keys)
      (bibtex-parse-keys nil t))
  (bibtex-complete bibtex-reference-keys))

(defun bibtex-Article ()
  "Insert a new BibTeX @Article entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Article"))

(defun bibtex-Book ()
  "Insert a new BibTeX @Book entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Book"))

(defun bibtex-Booklet ()
  "Insert a new BibTeX @Booklet entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Booklet"))

(defun bibtex-InBook ()
  "Insert a new BibTeX @InBook entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "InBook"))

(defun bibtex-InCollection ()
  "Insert a new BibTeX @InCollection entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "InCollection"))

(defun bibtex-InProceedings ()
  "Insert a new BibTeX @InProceedings entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "InProceedings"))

(defun bibtex-Manual ()
  "Insert a new BibTeX @Manual entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Manual"))

(defun bibtex-MastersThesis ()
  "Insert a new BibTeX @MastersThesis entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "MastersThesis"))

(defun bibtex-Misc ()
  "Insert a new BibTeX @Misc entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Misc"))

(defun bibtex-PhdThesis ()
  "Insert a new BibTeX @PhdThesis entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "PhdThesis"))

(defun bibtex-Proceedings ()
  "Insert a new BibTeX @Proceedings entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Proceedings"))

(defun bibtex-TechReport ()
  "Insert a new BibTeX @TechReport entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "TechReport"))

(defun bibtex-Unpublished ()
  "Insert a new BibTeX @Unpublished entry; see also `bibtex-entry'."
  (interactive)
  (bibtex-entry "Unpublished"))

(defun bibtex-String ()
  "Insert a new BibTeX @String entry."
  (interactive)
  (if (not bibtex-reference-keys)
      (bibtex-parse-keys nil t))
  (let ((key
         (if (and
              bibtex-maintain-sorted-entries
              (not bibtex-sort-ignore-string-entries))
             (completing-read
              "String key: " bibtex-reference-keys nil nil nil 'bibtex-key-history))))
    (if (and
         bibtex-maintain-sorted-entries
         (not bibtex-sort-ignore-string-entries))
	(bibtex-find-entry-location key)
      (bibtex-move-outside-of-entry))
    (indent-to-column bibtex-entry-offset)
    (insert
     (concat
      "@String"
      (bibtex-entry-left-delimiter)
      (if (and
           bibtex-maintain-sorted-entries
           (not bibtex-sort-ignore-string-entries))
          key)
      " = "
      (bibtex-field-left-delimiter)
      (bibtex-field-right-delimiter)
      (bibtex-entry-right-delimiter)
      "\n"))
  (forward-line -1)
  (forward-char
   (if (and
        bibtex-maintain-sorted-entries
        (not bibtex-sort-ignore-string-entries))
       (+ (length "@String{") (length key) (length " = {"))
     (length "@String{")))))

(defun bibtex-Preamble ()
  "Insert a new BibTeX @Preamble entry."
  (interactive)
  (bibtex-move-outside-of-entry)
  (indent-to-column bibtex-entry-offset)
  (insert
   "@Preamble"
   (bibtex-entry-left-delimiter)
   (bibtex-entry-right-delimiter)
   "\n")
  (forward-line -1)
  (forward-char 10))


;; Make BibTeX a Feature

(provide 'bibtex)

;;; bibtex.el ends here
