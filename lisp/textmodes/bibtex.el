;;; bibtex.el --- BibTeX mode for GNU Emacs

;; Copyright (C) 1992, 1994, 1995 Free Software Foundation, Inc.

;; Author: Stefan Schoef <schoef@informatik.uni-oldenburg.de>
;;      Bengt Martensson <ubrinf!mond!bengt>
;;	Mark Shapiro <shapiro@corto.inria.fr>
;;	Mike Newton <newton@gumby.cs.caltech.edu>
;;	Aaron Larson <alarson@src.honeywell.com>
;; Maintainer: Stefan Schoef <schoef@informatik.uni-oldenburg.de>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; TODO:
;;  Distribute texinfo file.
;;  A better concept for intermixing quote and brace delimiters is
;;  needed.

;;; PURPOSE:
;;  Major mode for editing and validating BibTeX files.


;;; USAGE:
;;  See documentation for function bibtex-mode (or type "\M-x describe-mode"
;;  when you are in bibtex-mode).


;;; KNOWN BUGS:
;;   1. using regular expressions to match the entire BibTeX entry dies
;;      on long entries (e.g. those containing abstracts) since
;;      the length of regular expression matches is fairly limited.
;;   2. Calling bibtex-find-text in a string entry results in the
;;      error message "Can't find enclosing Bibtex field" instead of
;;      moving to the empty string. [reported by gernot@cs.unsw.oz.au]

;;; (current keeper: schoef@informatik.uni-oldenburg.de
;;;  previous: alarson@src.honeywell.com)

;;; USER OPTIONS:

(defvar bibtex-field-left-delimiter "{"
  "*Set this to { or \" according to your personal preferences.
This variable is buffer local.")
(make-variable-buffer-local 'bibtex-field-left-delimiter)

(defvar bibtex-field-right-delimiter "}"
  "*Set this to } or \" according to your personal preferences.
This variable is buffer local.")
(make-variable-buffer-local 'bibtex-field-right-delimiter)

(defvar bibtex-include-OPTcrossref '("InProceedings" "InCollection")
  "*All entries listed here will have an OPTcrossref field.")

(defvar bibtex-include-OPTkey t
  "*If non-nil, all entries will have an OPTkey field.")

(defvar bibtex-include-OPTannote t
  "*If non-nil, all entries will have an OPTannote field.")

(defvar bibtex-mode-user-optional-fields nil
  "*List of optional fields the user wants to have always present.
Entries should be lists of strings with two elements (first element =
name of the field, second element = comment to appear in the echo area).")

(defvar bibtex-clean-entry-zap-empty-opts t
  "*If non-nil, bibtex-clean-entry will delete all empty optional fields.")

(defvar bibtex-sort-ignore-string-entries t
  "*If true, BibTeX @STRING entries are not sort-significant.
That means they are ignored when determining ordering of the buffer
(e.g. sorting, locating alphabetical position for new entries, etc.).")

(defvar bibtex-maintain-sorted-entries t
  "*If true, bibtex-mode maintains all BibTeX entries in sorted order.
Setting this variable to nil will strip off some comfort (e.g. TAB
completion for reference keys) from bibtex-mode.")

(defvar bibtex-entry-field-alist
  '(
    ("Article" . (((("author" "Author1 [and Author2 ...] [and others]")
                    ("title" "Title of the article (will be converted to lowercase)")
                    ("journal" "Name of the journal (use string, remove braces)")
                    ("year" "Year of publication"))
		   (("volume" "Volume of the journal")
                    ("number" "Number of the journal")
                    ("month" "Month of the publication as a string (remove braces)")
                    ("pages" "Pages in the journal")
                    ("note" "Remarks to be put at the end of the \\bibitem")))
		  ((("author" "Author1 [and Author2 ...] [and others]")
                    ("title" "Title of the article (will be converted to lowercase)"))
		   (("journal" "Name of the journal (use string, remove braces)") 
                    ("year" "Year of publication")
                    ("volume" "Volume of the journal")
                    ("number" "Number of the journal")
		    ("month" "Month of the publication as a string (remove braces)")
                    ("pages" "Pages in the journal")
                    ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Book" . (((("author" "Author1 [and Author2 ...] [and others]")
                 ("title" "Title of the book")
                 ("publisher" "Publishing company")
                 ("year" "Year of publication"))
		(("editor" "Editor1 [and Editor2 ...] [and others]")
                 ("volume" "Volume of the book in the series")
                 ("number" "Number of the book in a small series (overwritten by volume)")
                 ("series" "Series in which the book appeared")
                 ("address" "Address of the publisher")
		 ("edition" "Edition of the book as a capitalized English word")
                 ("month" "Month of the publication as a string (remove braces)")
                 ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Booklet" . (((("title" "Title of the booklet (will be converted to lowercase)"))
		   (("author" "Author1 [and Author2 ...] [and others]")
                    ("howpublished" "The way in which the booklet was published")
                    ("address" "Address of the publisher")
                    ("year" "Year of publication")
                    ("month" "Month of the publication as a string (remove braces)")
                    ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("InBook" . (((("author" "Author1 [and Author2 ...] [and others]")
                   ("title" "Title of the book")
                   ("chapter" "Chapter in the book")
                   ("publisher" "Publishing company")
                   ("year" "Year of publication"))
		  (("editor" "Editor1 [and Editor2 ...] [and others]")
                   ("volume" "Volume of the book in the series")
                   ("number" "Number of the book in a small series (overwritten by volume)")
                   ("series" "Series in which the book appeared")
                   ("address" "Address of the publisher")
		   ("edition" "Edition of the book as a capitalized English word")
                   ("month" "Month of the publication as a string (remove braces)")
                   ("pages" "Pages in the book")
                   ("type" "Word to use instead of \"chapter\"")
                   ("note" "Remarks to be put at the end of the \\bibitem")))
		 ((("author" "Author1 [and Author2 ...] [and others]")
                   ("title" "Title of the book")
                   ("chapter" "Chapter in the book"))
		  (("publisher" "Publishing company")
                   ("year" "Year of publication")
                   ("editor" "Editor1 [and Editor2 ...] [and others]")
                   ("volume" "Volume of the book in the series")
                   ("number" "Number of the book in a small series (overwritten by volume)")
		   ("series" "Series in which the book appeared")
                   ("address" "Address of the publisher")
                   ("edition" "Edition of the book as a capitalized English word")
                   ("month" "Month of the publication as a string (remove braces)")
                   ("pages" "Pages in the book")
                   ("type" "Word to use instead of \"chapter\"")
                   ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("InCollection" . (((("author" "Author1 [and Author2 ...] [and others]")
                         ("title" "Title of the article in book (will be converted to lowercase)")
			 ("booktitle" "Name of the book")
                         ("publisher" "Publishing company")
                         ("year" "Year of publication"))
			(("editor" "Editor1 [and Editor2 ...] [and others]")
                         ("volume" "Volume of the book in the series")
                         ("number" "Number of the book in a small series (overwritten by volume)")
                         ("series" "Series in which the book appeared")
                         ("chapter" "Chapter in the book")
                         ("type" "Word to use instead of \"chapter\"")
                         ("address" "Address of the publisher")
                         ("edition" "Edition of the book as a capitalized English word")
                         ("month" "Month of the publication as a string (remove braces)")
			 ("pages" "Pages in the book")
                         ("note" "Remarks to be put at the end of the \\bibitem")))
		       ((("author" "Author1 [and Author2 ...] [and others]")
                         ("title" "Title of the article in book (will be converted to lowercase)")
                         ("booktitle" "Name of the book"))
			(("publisher" "Publishing company")
                         ("year" "Year of publication")
			 ("editor" "Editor1 [and Editor2 ...] [and others]")
                         ("volume" "Volume of the book in the series")
                         ("number" "Number of the book in a small series (overwritten by volume)")
                         ("series" "Series in which the book appeared")
                         ("chapter" "Chapter in the book")
                         ("type" "Word to use instead of \"chapter\"")
                         ("address" "Address of the publisher")
                         ("edition" "Edition of the book as a capitalized English word")
                         ("month" "Month of the publication as a string (remove braces)")
			 ("pages" "Pages in the book")
                         ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("InProceedings" . (((("author" "Author1 [and Author2 ...] [and others]")
                          ("title" "Title of the article in proceedings (will be converted to lowercase)")
                          ("booktitle" "Name of the conference proceedings")
                          ("year" "Year of publication"))
			 (("editor" "Editor1 [and Editor2 ...] [and others]")
                          ("volume" "Volume of the conference proceedings in the series")
                          ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                          ("series" "Series in which the conference proceedings appeared")
			  ("organization" "Sponsoring organization of the conference")
                          ("publisher" "Publishing company, its location")
                          ("address" "Location of the Proceedings")
                          ("month" "Month of the publication as a string (remove braces)")
                          ("pages" "Pages in the conference proceedings")
                          ("note" "Remarks to be put at the end of the \\bibitem")))
			((("author" "Author1 [and Author2 ...] [and others]")
                          ("title" "Title of the article in proceedings (will be converted to lowercase)")
			  ("booktitle" "Name of the conference proceedings"))
			 (("editor" "Editor1 [and Editor2 ...] [and others]")
                          ("volume" "Volume of the conference proceedings in the series")
                          ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                          ("series" "Series in which the conference proceedings appeared")
                          ("year" "Year of publication")
			  ("organization" "Sponsoring organization of the conference")
                          ("publisher" "Publishing company, its location")
                          ("address" "Location of the Proceedings")
                          ("month" "Month of the publication as a string (remove braces)")
                          ("pages" "Pages in the conference proceedings")
                          ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Manual" . (((("title" "Title of the manual"))
		  (("author" "Author1 [and Author2 ...] [and others]")
                   ("organization" "Publishing organization of the manual")
                   ("address" "Address of the organization")
                   ("edition" "Edition of the manual as a capitalized English word")
                   ("year" "Year of publication")
		   ("month" "Month of the publication as a string (remove braces)")
                   ("note" "Remarks to be put at the end of the \\bibitem")))))

    ("MastersThesis" . (((("author" "Author1 [and Author2 ...] [and others]")
                          ("title" "Title of the master\'s thesis (will be converted to lowercase)")
                          ("school" "School where the master\'s thesis was written")
                          ("year" "Year of publication"))
			 (("address" "Address of the school (if not part of field \"school\") or country")
                          ("type" "Type of the master\'s thesis")
                          ("month" "Month of the publication as a string (remove braces)")
                          ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Misc" . ((()
		(("author" "Author1 [and Author2 ...] [and others]")
                 ("title" "Title of the reference (will be converted to lowercase)")
                 ("howpublished" "The way in which the reference was published")
                 ("year" "Year of publication")
                 ("month" "Month of the publication as a string (remove braces)")
                 ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("PhdThesis" . (((("author" "Author1 [and Author2 ...] [and others]")
                      ("title" "Title of the PhD. thesis")
                      ("school" "School where the PhD. thesis was written")
                      ("year" "Year of publication"))
		     (("address" "Address of the school (if not part of field \"school\") or country")
                      ("type" "Type of the PhD. thesis")
                      ("month" "Month of the publication as a string (remove braces)")
                      ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Proceedings" . (((("title" "Title of the conference proceedings")
                        ("year" "Year of publication"))
		       (("editor" "Editor1 [and Editor2 ...] [and others]")
                        ("volume" "Volume of the conference proceedings in the series")
                        ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                        ("series" "Series in which the conference proceedings appeared")
                        ("publisher" "Publishing company, its location")
			("organization" "Sponsoring organization of the conference")
                        ("address" "Location of the Proceedings")
                        ("month" "Month of the publication as a string (remove braces)")
                        ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("TechReport" . (((("author" "Author1 [and Author2 ...] [and others]")
                       ("title" "Title of the technical report (will be converted to lowercase)")
                       ("institution" "Sponsoring institution of the report")
                       ("year" "Year of publication"))
		      (("type" "Type of the report (if other than \"technical report\")")
                       ("number" "Number of the technical report")
                       ("address" "Address of the institution (if not part of field \"institution\") or country")
                       ("month" "Month of the publication as a string (remove braces)")
                       ("note" "Remarks to be put at the end of the \\bibitem")))))
    ("Unpublished" . (((("author" "Author1 [and Author2 ...] [and others]")
                        ("title" "Title of the unpublished reference (will be converted to lowercase)")
                        ("note" "Remarks to be put at the end of the \\bibitem"))
		       (("year" "Year of publication")
                        ("month" "Month of the publication as a string (remove braces)")))))
    )

  "Defines reference types and their associated fields.
List of
(entry-name (required optional) (crossref-required crossref-optional))
triples.
If the third element is nil, the first pair is always to be used.
If not, the second pair is to be used in the case of presence of a
crossref field and the third in the case of absence.
Required , optional, crossref-required and crossref-optional are lists. 
Each element of these lists is a list of strings with two elements
(first element  = name of the field, 
 second element = comment to appear in the echo area).")

(defvar bibtex-predefined-strings
  '(
    ("jan") ("feb") ("mar") ("apr") ("may") ("jun") ("jul") ("aug")
    ("sep") ("oct") ("nov") ("dec")
    ("acmcs") ("acta") ("cacm") ("ibmjrd") ("ibmsj") ("ieeese")
    ("ieeetc") ("ieeetcad") ("ipl") ("jacm") ("jcss") ("scp")
    ("sicomp") ("tcs") ("tocs") ("tods") ("tog") ("toms") ("toois")
    ("toplas")
    )
  "Alist of string definitions.
Should contain the strings defined in the BibTeX style files. Each
element is a list with just one element: the string.")

(defvar bibtex-string-files nil
  "*List of BibTeX files containing string definitions.
Those files must be specified using pathnames relative to the
directories specified in $BIBINPUTS. This variable is only evaluated
when bibtex-mode is entered (i. e. when loading the BibTeX file).")

(defvar bibtex-help-message t
  "*If not nil print help messages in the echo area on entering a new field.")

(defvar bibtex-autokey-names 1
  "*Number of names to use for the automatically generated reference key.
If this is set to anything but a number, all names are used.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-name-change-strings
  '(("\\\\\\\"a" "ae") ("\\\\\\\"o" "oe") ("\\\\\\\"u" "ue")
    ("\\\\\\\"s" "ss")
    ("\\\\\\\"A" "Ae") ("\\\\\\\"O" "Oe") ("\\\\\\\"U" "Ue")
    ("{" "") ("}" ""))
  "Alist of (old-regexp new-string) pairs.
Any part of name matching a old-regexp is replaced by new-string.
Case of the old-regexp is significant. All regexps are tried in the
order in which they appear in the list, so be sure to avoid recursion here.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-name-length 'infty
  "*Number of characters from name to incorporate into key.
If this is set to anything but a number, all characters are used.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-name-separator ""
  "*String that comes between any two names in the key.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-year-length 2
  "*Number of rightmost digits from the year field yo incorporate into key.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-titlewords 5
  "*Number of title words to use for the automatically generated reference key.
If this is set to anything but a number, all title words are used.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-title-terminators
  '("\\." "!"  "\\?" ":" ";" "---")
  "*Regexp list defining the termination of the main part of the title.
Case of the regexps is ignored.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-titlewords-stretch 2
  "*Number of words that can additionally be used from the title.
These words are used only, if a sentence from the title can be ended then.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-titleword-first-ignore
  '("a" "an" "on" "the" "eine?" "der" "die" "das")
  "*Determines words that may begin a title but are not to be used in the key.
Each item of the list is a regexp. If the first word of the title matchs a
regexp from that list, it is not included in the title, even if it is
capitalized. Regexps in the list must be entered using lowercase letters.")

(defvar bibtex-autokey-titleword-abbrevs nil
  "*Determines exceptions to the usual abbreviation mechanism.
A list of (old-regexp new-string) pairs. 
Use all lowercase letters for old-regexp.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-titleword-change-strings
  '(("\\\\\\\"a" "ae") ("\\\\\\\"o" "oe") ("\\\\\\\"u" "ue")
    ("\\\\\\\"s" "ss")
    ("\\\\\\\"A" "Ae") ("\\\\\\\"O" "Oe") ("\\\\\\\"U" "Ue")
    ("{" "") ("}" ""))
  "Alist of (old-regexp new-string) pairs.
Any part of title word matching a old-regexp is replaced by new-string.
Case of the old-regexp is significant.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-titleword-length 5
  "*Number of characters from title words to incorporate into key.
If this is set to anything but a number, all characters are used.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-titleword-separator "_"
  "*String to be put between the title words.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-name-year-separator ""
  "*String to be put between name part and year part of key.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-year-title-separator ":_"
  "*String to be put between name part and year part of key.
See the documentation of function bibtex-generate-autokey for further detail.")

(defvar bibtex-autokey-edit-before-use t
  "*If non-nil, user is allowed to edit the generated key before it is used.")



;;; SYNTAX TABLE, KEYBINDINGS and BIBTEX-ENTRY-LIST
(defvar bibtex-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; [alarson:19920214.1004CST] make double quote a string quote
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?$ "$$  " st)
    (modify-syntax-entry ?% "<   " st)
    (modify-syntax-entry ?'  "w   " st)
    (modify-syntax-entry ?@  "w   " st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?~ " " st)
    st))

(defvar bibtex-mode-map
  (let ((km (make-sparse-keymap)))
    
    (define-key km "\t" 'bibtex-find-text)
    (define-key km "\n" 'bibtex-next-field)
    (define-key km "\M-\t" 'bibtex-complete-string)
    (define-key km "\C-c\"" 'bibtex-remove-double-quotes-or-braces)
    (define-key km "\C-c{" 'bibtex-remove-double-quotes-or-braces)
    (define-key km "\C-c}" 'bibtex-remove-double-quotes-or-braces)
    (define-key km "\C-c\C-c" 'bibtex-clean-entry)
    (define-key km "\C-c?" 'bibtex-print-help-message)
    (define-key km "\C-c\C-p" 'bibtex-pop-previous)
    (define-key km "\C-c\C-n" 'bibtex-pop-next)
    (define-key km "\C-c\C-k" 'bibtex-kill-optional-field)
    (define-key km "\C-c\C-d" 'bibtex-empty-field)
    (define-key km "\C-c$"   'bibtex-ispell-entry)
    (define-key km "\M-\C-a"   'bibtex-beginning-of-entry)
    (define-key km "\M-\C-e"   'bibtex-end-of-entry)
    (define-key km "\C-c\C-b"   'bibtex-entry)
    (define-key km "\C-c\C-q" 'bibtex-hide-entry-bodies)
    (define-key km "\C-c\C-a" 'show-all)
    (define-key km "\C-c\C-rn" 'bibtex-narrow-to-entry)
    (define-key km "\C-c\C-rw" 'widen)
    (define-key km "\C-c\C-o" 'bibtex-remove-OPT)

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
    (define-key km "\C-c\C-e\M-p" 'bibtex-preamble)
    (define-key km "\C-c\C-e\C-s" 'bibtex-string)
    (define-key km "\C-c\C-e\C-t" 'bibtex-TechReport)
    (define-key km "\C-c\C-e\C-u" 'bibtex-Unpublished)
    km))

(define-key bibtex-mode-map [menu-bar move/edit]
  (cons "BibTeX-Edit" (make-sparse-keymap "BibTeX-Edit")))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-print-help-message]
  '("Help about Current Field" . bibtex-print-help-message))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-complete-string]
  '("String Complete" . bibtex-complete-string))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-next-field]
  '("Next Field" . bibtex-next-field))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-find-text]
  '("End of Field" . bibtex-find-text))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-pop-previous]
  '("Snatch from Similar Preceding Field" . bibtex-pop-previous))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-pop-next]
  '("Snatch from Similar Following Field" . bibtex-pop-next))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-remove-OPT]
  '("Remove OPT" . bibtex-remove-OPT))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-remove-double-quotes-or-braces]
  '("Remove Quotes or Braces" . bibtex-remove-double-quotes-or-braces))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-clean-entry]
  '("Clean Up Entry" . bibtex-clean-entry))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-sort-entries]
  '("Sort Entries" . bibtex-sort-entries))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-validate-buffer]
  '("Validate Entries" . bibtex-validate-buffer))

(define-key bibtex-mode-map [menu-bar entry-types]
  (cons "Entry-Types" (make-sparse-keymap "Entry-Types")))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-preamble]
  '("Preamble" . bibtex-preamble))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-string]
  '("String" . bibtex-string))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Misc]
  '("Miscellaneous" . bibtex-Misc))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Unpublished]
  '("Unpublished" . bibtex-Unpublished))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Manual]
  '("Technical Manual" . bibtex-Manual))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-TechReport]
  '("Technical Report" . bibtex-TechReport))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-MastersThesis]
  '("Master's Thesis" . bibtex-MastersThesis))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-PhdThesis]
  '("PhD. Thesis" . bibtex-PhdThesis))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Booklet]
  '("Booklet (Bound, but no Publisher/Institution)" . bibtex-Booklet))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Book]
  '("Book" . bibtex-Book))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Proceedings]
  '("Conference Proceedings" . bibtex-Proceedings))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-InBook]
  '("Chapter or Pages in a Book" . bibtex-InBook))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-InCollection]
  '("Article in a Collection" . bibtex-InCollection))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-InProceedings]
  '("Article in Conference Proceedings" . bibtex-InProceedings))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Article]
  '("Article in Journal" . bibtex-Article))



;;; INTERNAL VARIABLES

(defvar bibtex-pop-previous-search-point nil)
;; Next point where bibtex-pop-previous starts looking for a similar
;; entry.

(defvar bibtex-pop-next-search-point nil)
;; Next point where bibtex-pop-next starts looking for a similar entry.

(defvar bibtex-completion-candidates nil)
;; Candidates for bibtex-complete-string. Initialized from
;; bibtex-predefined-strings and bibtex-string-files. This variable is
;; buffer-local.
(make-variable-buffer-local 'bibtex-completion-candidates)


;;; FUNCTIONS to parse the BibTeX entries

(defun bibtex-cfield (name text)
  ;; Create a regexp for a BibTeX field of name NAME and text TEXT.
  (concat ",[ \t\n]*\\("
	  name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  text
	  "\\)"))
(defconst bibtex-name-in-cfield 1)
;; The regexp subexpression number of the name part in bibtex-cfield.

(defconst bibtex-text-in-cfield 2)
;; The regexp subexpression number of the text part in bibtex-cfield.

(defconst bibtex-field-name "[A-Za-z][]A-Za-z0-9.:;?!`'()/*@_+=-]*")
;; Regexp defining the name part of a BibTeX field.

(defconst bibtex-field-const "[0-9A-Za-z][A-Za-z0-9:_+-]*"
  "Format of a bibtex field constant.")

(defconst bibtex-field-string
  (concat
   "\\("
   "{\\(\\({\\(\\({[^}]*}\\)\\|\\([^{}]\\)\\)*}\\)\\|\\([^{}]\\)\\)*}"
   ;; maximal twice nested {}
   "\\)\\|\\("
   "\"[^\"]*[^\\\\]\"\\|\"\"\\)"))
;; Match either a string or an empty string.

(defconst bibtex-field-string-or-const
  (concat bibtex-field-const "\\|" bibtex-field-string))
;; Match either bibtex-field-string or bibtex-field-const.

(defconst bibtex-field-text
  (concat
    "\\(" bibtex-field-string-or-const "\\)"
        "\\([ \t\n]+#[ \t\n]+\\(" bibtex-field-string-or-const "\\)\\)*\\|"
    "{[^{}]*[^\\\\]}"))
;; Regexp defining the text part of a BibTeX field: either a string,
;; or an empty string, or a constant followed by one or more # /
;; constant pairs. Also matches simple {...} patterns.

(defconst bibtex-field
  (bibtex-cfield bibtex-field-name bibtex-field-text))
;; Regexp defining the format of a BibTeX field.

(defconst bibtex-name-in-field bibtex-name-in-cfield)
;; The regexp subexpression number of the name part in BibTeX-field.

(defconst bibtex-text-in-field bibtex-text-in-cfield)
;; The regexp subexpression number of the text part in BibTeX-field.

(defconst bibtex-reference-type
  "@[A-Za-z]+")
;; Regexp defining the type part of a BibTeX reference entry.

(defconst bibtex-reference-head
  (concat "^\\( \\|\t\\)*\\("
	  bibtex-reference-type
	  "\\)[ \t]*[({]\\("
	  bibtex-field-name
	  "\\)"))
;; Regexp defining format of the header line of a BibTeX reference
;; entry.

(defconst bibtex-type-in-head 2)
;; The regexp subexpression number of the type part in
;; bibtex-reference-head.

(defconst bibtex-key-in-head 3)
;; The regexp subexpression number of the key part in
;; bibtex-reference-head.

(defconst bibtex-reference
  (concat bibtex-reference-head
	  "\\([ \t\n]*" bibtex-field "\\)*"
	  "[ \t\n]*[})]"))
;; Regexp defining the format of a BibTeX reference entry.

(defconst bibtex-type-in-reference bibtex-type-in-head)
;; The regexp subexpression number of the type part in
;; bibtex-reference.

(defconst bibtex-key-in-reference bibtex-key-in-head)
;; The regexp subexpression number of the key part in
;; bibtex-reference.

(defconst bibtex-string
  (concat "^[ \t]*@[sS][tT][rR][iI][nN][gG][ \t\n]*[({][ \t\n]*\\("
	  bibtex-field-name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  bibtex-field-text
	  "\\)[ \t\n]*[})]"))
;; Regexp defining the format of a BibTeX string entry.

(defconst bibtex-name-in-string 1)
;; The regexp subexpression of the name part in bibtex-string.

(defconst bibtex-text-in-string 2)
;; The regexp subexpression of the text part in bibtex-string.

(defconst bibtex-name-alignment 2)
;; Alignment for the name part in BibTeX fields. Chosen on aesthetic
;; grounds only.

(defconst bibtex-text-alignment (length "  organization = "))
;; Alignment for the text part in BibTeX fields. Equal to the space
;; needed for the longest name part.



;;; HELPER FUNCTIONS

(defun assoc-ignore-case (string alist)
  ;; Return non-nil if STRING is `equal' to the car of an element of
  ;; LIST. Comparison is done with case ignored. The value is actually
  ;; the element of LIST whose car is `equal' to STRING.
  (or (assoc string alist)
      (while (and alist
		  (not (string-equal
                        (downcase string)
                        (downcase (car (car alist))))))
	(setq alist (cdr alist)))
      (car alist)))

(defun member-of-regexp (string list)
  ;; Return non-nil if STRING is exactly matched by an element of
  ;; LIST. This function is influenced by the actual value of
  ;; `case-fold-search'. The value is actually the tail of LIST whose
  ;; car matches STRING.
  (while
      (and
       list
       (not
        (string-match
         (concat "^" (car list) "$")
         string)))
    (setq list (cdr list)))
  list)

(defun assoc-of-regexp (string alist)
  ;; Return non-nil if STRING is exactly matched by the car of an
  ;; element of LIST. This function is influenced by the actual value
  ;; of `case-fold-search'. The value is actually the element of LIST
  ;; whose car matches STRING.
  (while
      (and
       alist
       (not
        (string-match
         (concat "^" (car (car alist)) "$")
         string)))
    (setq alist (cdr alist)))
  (car alist))

(defun skip-whitespace-and-comments ()
  (let ((md (match-data)))
    (unwind-protect
	(while (cond ((looking-at "\\s>+\\|\\s +")
		      ;; was whitespace
		      ;; NOTE: also checked end-comment.  In latex and
		      ;; lisp modes, newline is an end comment, but it
		      ;; should also be a whitespace char.
		      (goto-char (match-end 0)))
		     ;; If looking at beginning of comment, skip to end.
		     ((looking-at "\\s<")
		      (re-search-forward "\\s>"))))		      
      (store-match-data md))))

(defun map-bibtex-entries (fun)
  ;; Call FUN for each BibTeX entry starting with the current. Do this
  ;; to the end of the file. FUN is called with one argument, the key
  ;; of the entry, and with point inside the entry. If
  ;; bibtex-sort-ignore-string-entries is true, FUN will not be called
  ;; for @string entries.
  (bibtex-beginning-of-entry)
  (while (re-search-forward "^@[^{]*{[ \t]*\\([^, ]*\\)" nil t)
    (if (and bibtex-sort-ignore-string-entries
	     (string-equal "@string{"
                           (downcase (buffer-substring
                                      (match-beginning 0)
                                      (match-beginning 1)))))
	nil
      (funcall fun (buffer-substring (match-beginning 1) (match-end 1))))))

(defun bibtex-flash-head ()
  ;; Flash at BibTeX reference head before point, if exists.
  (let ((flash))
    (cond ((re-search-backward bibtex-reference-head (point-min) t)
	   (goto-char (match-beginning bibtex-type-in-head))
	   (setq flash (match-end bibtex-key-in-reference)))
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

(defun bibtex-move-outside-of-entry ()
  ;; Make sure we are outside of a BibTeX entry.
  (cond ((or
	  (= (point) (point-max))
	  (= (point) (point-min))
	  (looking-at "[ \n]*@")
	  )
	 t)
	(t
	 (backward-paragraph)
	 (forward-paragraph)))
  (re-search-forward "[ \t\n]*" (point-max) t))

(defun beginning-of-first-bibtex-entry ()
  ;; Go to the beginning of the first BibTeX entry in buffer.
  (goto-char (point-min))
  (cond
   ((re-search-forward "^@" nil 'move)
    (beginning-of-line))
   ((and (bobp) (eobp))
    nil)
   (t
    (message "Warning: No BibTeX entries found!"))))

(defun bibtex-inside-field ()
  ;; Try to avoid point being at end of a BibTeX field.
  (end-of-line)
  (skip-chars-backward " \t")		;MON - maybe delete these chars?
  (cond ((= (preceding-char) ?,)
	 (forward-char -2))) ; -1 --> -2 sct@dcs.edinburgh.ac.uk
  (cond ((= (preceding-char) (aref bibtex-field-right-delimiter 0))
	 (forward-char -1))))		;MON - only go back if quote

(defun bibtex-enclosing-field ()
  ;; Search for BibTeX field enclosing point. Point moves to end of
  ;; field; also, use match-beginning and match-end to parse the field.
  ;; sct@dcs.edinburgh.ac.uk
  (let ((old-point (point)))
    (condition-case errname
 	(bibtex-enclosing-regexp bibtex-field)
      (search-failed
       (goto-char old-point)
       (error "Can't find enclosing BibTeX field.")))))

(defun bibtex-enclosing-reference ()
  ;; Search for BibTeX reference enclosing point. Point moves to begin
  ;; of reference. (match-end 0) denotes end of reference.
  ;; Hacked up for speed. Parsing isn't guaranteed any more.
  ;; schoef@informatik.uni-oldenburg.de
  ;; sct@dcs.edinburgh.ac.uk
  (let ((old-point (point)))
    (if (not
         (re-search-backward
          "^@[A-Za-z]+[ \t\n]*[{(][^, \t\n]*[ \t\n]*,"
          (point-min) t))
        (progn
          (error "Can't find enclosing BibTeX reference.")
          (goto-char old-point)))
    (let ((pnt (point)))
      (if (not
           (re-search-forward "^[)}]$" (point-max) t))
          (progn
            (error "Can't find enclosing BibTeX reference.")
            (goto-char old-point))
        (goto-char pnt)))))

(defun bibtex-enclosing-regexp (regexp)
  ;; Search for REGEXP enclosing point. Point moves to end of
  ;; REGEXP. See also match-beginning and match-end. If an enclosing
  ;; REGEXP is not found, signals search-failed; point is left in an
  ;; undefined location.
  ;; Doesn't something like this exist already?
  ; compute reasonable limits for the loop
  (let* ((initial (point))
	 (right (if (re-search-forward regexp (point-max) t)
		    (match-end 0)
		  (point-max)))
	 (left
	  (progn
	    (goto-char initial)
	    (if (re-search-backward regexp (point-min) t)
		(match-beginning 0)
	      (point-min)))))
    ; within the prescribed limits, loop until a match is found
    (goto-char left)
    (re-search-forward regexp right nil 1)
    (if (> (match-beginning 0) initial)
	(signal 'search-failed (list regexp)))	  
    (while (<= (match-end 0) initial)
      (re-search-forward regexp right nil 1)
      (if (> (match-beginning 0) initial)
	  (signal 'search-failed (list regexp))))
    ))

(defun bibtex-autokey-change (string change-list)
  ;; Returns a string where some regexps are changed according to
  ;; change-list. Every item of change-list is an (old-regexp
  ;; new-string) pair.
  (let ((return-string string)
        case-fold-search
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
  ;; characters. String is aborted only after a consonant or at the
  ;; word end. If len is not a number, string is returned unchanged.
  (let* ((string-length (length string))
         (len (if (numberp len)
                  (min len string-length)
                len))
         (return-string (if (numberp len)
                            (substring string 0 len)))
         (index len)
         (vowels '(?a ?e ?i ?o ?u ?A ?E ?I ?O ?U)))
    (if (numberp len)
        (progn
          (while (and
                  (< index string-length) 
                  (member (elt return-string
                               (1- (length return-string)))
                          vowels))
            (setq return-string (concat return-string
                                        (substring
                                         string index (1+ index)))
                  index (1+ index)))
          return-string)
      string)))        

(defun bibtex-generate-autokey ()
  "Generates automatically a key from the author/editor and the title field.
The generation algorithm works as follows:
  1. If there is a non-empty author (preferred) or editor field,
     use it for the name part of the key.
  2. Change any substring found in `bibtex-autokey-name-change-strings' 
     to the corresponding new one (see documentation of this variable
     for further detail).
  3. For every of the first `bibtex-autokey-names' names in the
     \"name\" field, determine the last name.
  4. From every last name, take at least `bibtex-autokey-name-length'
     characters (abort only after a consonant or at a word end).
  5. Build the name part of the key by concatenating all abbreviated last
     names with the string `bibtex-autokey-name-separator' between
     any two.
  6. Build the year part of the key by truncating the contents of the
     \"year\" field to the rightmost `bibtex-autokey-year-length'
     digits (useful values are 2 and 4).
  7. For the title part of the key change the contents of the \"title\"
     field of the reference according to
     `bibtex-autokey-titleword-change-strings' to the corresponding
     new one (see documentation of this variable for further detail).
  8. Abbreviate the result to the string up to (but not including) the
     first occurence of a regexp matched by the items of
     `bibtex-autokey-title-terminators' and delete the first
     word if it appears in `bibtex-autokey-titleword-first-ignore'. 
     Build the title part of the key by using at least the first
     `bibtex-autokey-titlewords' capitalized words from this
     abbreviated title. If the abbreviated title ends after maximal
     `bibtex-autokey-titlewords' + `bibtex-autokey-titlewords-stretch'
     capitalized words, all capitalized words from the abbreviated title
     are used. 
  9. For every used title word that appears in
     `bibtex-autokey-titleword-abbrevs' use the corresponding abbreviation
     (see documentation of this variable for further detail).
 10. From every title word not generated by an abbreviation, take at
     least `bibtex-autokey-titleword-length' characters (abort only after
     a consonant or at a word end).
 11. Build the title part of the key by concatenating all abbreviated
     title words with the string `bibtex-autokey-titleword-separator'
     between any two.
 12. At least, to get the key, concatenate the name part, the year part
     and the title part with `bibtex-autokey-name-year-separator'
     between the name and the year if both are non-empty and
     `bibtex-autokey-year-title-separator' between the year and
     the title if both are non-empty."

  (let* ((pnt (point))
         (min
          (progn
            (bibtex-beginning-of-entry)
            (point)))
         (max
          (progn
            (bibtex-end-of-entry)
            (point)))
         (namefield
          (progn
            (goto-char min)
            (if (or
                 (search-forward-regexp "^[ \t]*author[ \t]*=" max t)
                 (search-forward-regexp "^[ \t]*editor[ \t]*=" max t))
                (let* (bibtex-help-message
                       (start (progn
                                (bibtex-find-text t)
                                (point)))
                       (end (progn
                              (bibtex-find-text nil)
                              (point))))
                  (bibtex-autokey-change
                   (buffer-substring start end)
                   bibtex-autokey-name-change-strings))
              "")))
         (namelist
          (mapcar
           (function
            (lambda (fullname)
              (bibtex-autokey-abbrev
               (if (string-match "," fullname)
                   (substring fullname 0 (match-beginning 0))
                 (progn
                   (if (string-match " [^ ]*$" fullname)
                       (substring
                        fullname (1+ (match-beginning 0)))
                     fullname)))
               bibtex-autokey-name-length)))
           ;; Gather all names into a list
           (let (names
                 (counter 0))
             (while (and
                     (not (equal namefield ""))
                     (or
                      (not (numberp bibtex-autokey-names))
                      (< counter bibtex-autokey-names)))
               (if (string-match " and " namefield)
                   (progn
                     (setq
                      names
                      (append names
                              (list
                               (downcase
                                (substring
                                 namefield 0 (match-beginning 0)))))
                      namefield
                      (substring namefield (match-end 0))))
                 (setq names
                       (append names (list (downcase namefield)))
                       namefield ""))
               (setq counter (1+ counter)))
             names)))
         (namepart (mapconcat (function (lambda (name) name))
                              namelist
                              bibtex-autokey-name-separator))
         (yearfield
          (progn
            (goto-char min)
            (if (search-forward-regexp
                 "^[ \t]*year[ \t]*=[ \t]*\\([0-9]*\\)" max t)
                (buffer-substring (match-beginning 1) (match-end 1))
              "")))
         (yearpart
          (if (equal yearfield "")
              ""
            (substring yearfield
                       (- (length yearfield)
                          bibtex-autokey-year-length))))
         (titlestring
          (let ((case-fold-search t)
                (titlefield
                 (progn
                   (goto-char min)
                   (if (search-forward-regexp
                        "^[ \t]*title[ \t]*=" max t)
                       (let* (bibtex-help-message
                              (start (progn
                                       (bibtex-find-text t)
                                       (point)))
                              (end (progn
                                     (bibtex-find-text nil)
                                     (point))))
                         (bibtex-autokey-change
                          (buffer-substring start end)
                          bibtex-autokey-titleword-change-strings))
                     "")))
                case-fold-search
                (index 0)
                (numberofitems
                 (length bibtex-autokey-title-terminators)))
            (while (< index numberofitems)
              (if (string-match
                   (elt bibtex-autokey-title-terminators index)
                   titlefield)
                  (setq titlefield
                        (substring titlefield 0 (match-beginning 0))))
              (setq index (1+ index)))
            titlefield))
         (titlelist
          (mapcar
           (function
            (lambda (titleword)
              (let ((abbrev
                     (assoc-of-regexp
                      titleword bibtex-autokey-titleword-abbrevs)))
                (if abbrev
                    (elt abbrev 1)
                  (bibtex-autokey-abbrev
                   titleword
                   bibtex-autokey-titleword-length)))))
           ;; Gather all titlewords into a list
           (let (titlewords
                 titlewords-extra
                 case-fold-search
                 (counter 0)
                 (first t))
             (while (and
                     (not (equal titlestring ""))
                     (or
                      (not (numberp bibtex-autokey-titlewords))
                      (< counter (+
                                  bibtex-autokey-titlewords
                                  bibtex-autokey-titlewords-stretch))))
               (if (string-match "\\b[A-Z][A-Za-z0-9]*" titlestring)
                   (let* ((end-match (match-end 0))
                          (titleword
                           (downcase (substring titlestring
                                                (match-beginning 0)
                                                end-match))))
                     (if (or
                          (not (numberp bibtex-autokey-titlewords))
                          (< counter bibtex-autokey-titlewords))
                         (if (and
                              first
                              (member-of-regexp
                               titleword
                               bibtex-autokey-titleword-first-ignore))
                             (setq counter -1)
                           (setq titlewords
                                 (append titlewords (list titleword))))
                       (setq
                        titlewords-extra
                        (append titlewords-extra (list titleword))))
                     (setq titlestring
                           (substring titlestring end-match)))
                 (setq titlestring ""))
               (setq first nil
                     counter (1+ counter)))
             (if (string-match "\\b[A-Z][^ ]*\\b" titlestring)
                 titlewords
               (append titlewords titlewords-extra)))))
         (titlepart (mapconcat (function (lambda (name) name))
                               titlelist
                               bibtex-autokey-titleword-separator))
         (autokey
          (concat
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
    (goto-char pnt)
    autokey))



;;; INTERACTIVE FUNCTIONS:

;;;###autoload
(defun bibtex-mode () 
  "Major mode for editing BibTeX files.

\\{bibtex-mode-map}

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and thus ignored by BibTeX.
The OPT string may be removed from a field with \\[bibtex-remove-OPT].
\\[bibtex-kill-optional-field] kills the current optional field entirely.
\\[bibtex-remove-double-quotes-or-braces] removes the double-quotes or
braces around the text of the current field.  \\[bibtex-empty-field]
replaces the text of the current field with the default \"\" or {}.

The command \\[bibtex-clean-entry] cleans the current entry, i.e. (i) removes
double-quotes or braces from entirely numerical fields, (ii) removes
OPT from all non-empty optional fields, (iii) removes all empty
optional fields, and (iv) checks that no non-optional fields are empty.

Use \\[bibtex-find-text] to position the cursor at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

The following may be of interest as well:

  Functions:
    bibtex-entry
    bibtex-print-help-message
    bibtex-beginning-of-entry
    bibtex-end-of-entry
    bibtex-ispell-abstract
    bibtex-narrow-to-entry
    bibtex-hide-entry-bodies
    bibtex-sort-entries
    bibtex-validate-buffer
    bibtex-pop-previous
    bibtex-pop-next
    bibtex-complete-string

  Variables:
    bibtex-field-left-delimiter
    bibtex-field-right-delimiter
    bibtex-include-OPTcrossref
    bibtex-include-OPTkey
    bibtex-include-OPTannote
    bibtex-mode-user-optional-fields
    bibtex-clean-entry-zap-empty-opts
    bibtex-sort-ignore-string-entries
    bibtex-maintain-sorted-entries
    bibtex-entry-field-alist
    bibtex-predefined-strings
    bibtex-string-files

---------------------------------------------------------
Entry to this mode calls the value of bibtex-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)
  (setq mode-name "BibTeX")
  (set-syntax-table bibtex-mode-syntax-table)
  (setq bibtex-completion-candidates bibtex-predefined-strings)
  (mapcar
   (function
    (lambda (filename)
      ;; collect pathnames
      (let* ((bib (getenv "BIBINPUTS"))
             (path (if bib
                       bib
                     "."))
             (dirs
              (mapcar
               (function
                (lambda (dirname)  ;; strips off trailing slashes
                  (let ((len (length dirname)))
                    (if (equal (elt dirname (1- len)) "/")
                        (substring dirname 0 (1- (1- len)))
                      dirname))))
               (let (actdirs)
                 (while (string-match ":" path)
                   (setq actdirs
                         (append actdirs
                                 (list (substring
                                        path 0
                                        (1- (match-end 0)))))
                         path (substring path (match-end 0))))
                 (append actdirs (list path)))))
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
            (let ((curbuf (current-buffer))
                  (bufname (make-temp-name ""))
                  (compl bibtex-completion-candidates))
              (create-file-buffer bufname)
              (set-buffer bufname)
              (insert-file-contents fullfilename)
              (goto-char (point-min))
              (while (search-forward-regexp bibtex-string nil t)
                (setq
                 compl
                 (append
                  compl
                  (list
                   (list (buffer-substring
                          (match-beginning bibtex-name-in-string)
                          (match-end bibtex-name-in-string)))))))
              (kill-buffer bufname)
              (set-buffer curbuf)
              (setq bibtex-completion-candidates compl))
          (error "File %s not in $BIBINPUTS paths" filename)))))
   bibtex-string-files)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "[ \f\n\t]*$")
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (auto-fill-mode 1)			; nice alignments
  (setq left-margin (+ bibtex-text-alignment 1))
  (run-hooks 'bibtex-mode-hook))

(defun bibtex-entry (entry-type &optional required optional)
  (interactive (let* ((completion-ignore-case t)
		      (e-t (completing-read
                            "Entry Type: "
                            bibtex-entry-field-alist
                            nil t)))
		 (list e-t)))
  (if (and (null required) (null optional))
      (let* ((e (assoc-ignore-case entry-type bibtex-entry-field-alist))
	     (r-n-o (elt e 1))
	     (c-ref (elt e 2)))
	(if (null e)
            (error "Bibtex entry type %s not defined!" entry-type))
	(if (and
             (member entry-type bibtex-include-OPTcrossref)
             c-ref)
	    (setq required (elt c-ref 0)
		  optional (elt c-ref 1))
	  (setq required (elt r-n-o 0)
		optional (elt r-n-o 1)))))
  (let*
      (labels
       label
       (case-fold-search t)
       (key
        (if bibtex-maintain-sorted-entries
            (progn
              (save-excursion
                (goto-char (point-min))
                (while
                    (re-search-forward
                     "\\(^@[a-z]+[ \t\n]*[{(][ \t\n]*\\([^ ,\t\n]+\\)[ \t\n]*,\\)\\|\\(^[ \t\n]*crossref[ \t\n]*=[ \t\n]*[{\"]\\([^ ,\t\n]*\\)[}\"],$\\)"
                     nil t)
                  (if (match-beginning 2)
                      (setq label (buffer-substring 
                                   (match-beginning 2) (match-end 2)))
                    (setq label (buffer-substring
                                 (match-beginning 4) (match-end 4))))
                  (if (not (assoc label labels))
                      (setq labels
                            (cons (list label) labels)))))
              (completing-read
               (format "%s key: " entry-type)
               labels)))))
    (if key
	(bibtex-find-entry-location key))	
    (bibtex-move-outside-of-entry)
    (insert "@" entry-type "{")
    (if key
	(insert key))
    (save-excursion
      (mapcar 'bibtex-make-field required)
      (if (member entry-type bibtex-include-OPTcrossref)
	  (bibtex-make-optional-field '("crossref")))
      (if bibtex-include-OPTkey
	  (bibtex-make-optional-field '("key")))
      (mapcar 'bibtex-make-optional-field optional)
      (mapcar 'bibtex-make-optional-field 
	      bibtex-mode-user-optional-fields)
      (if bibtex-include-OPTannote
	  (bibtex-make-optional-field '("annote")))
      (insert "\n}\n\n"))
    (if key
	(bibtex-next-field t))
    (run-hooks 'bibtex-add-entry-hook)))

(defun bibtex-print-help-message ()
  "Prints helpful information about current field in current BibTeX entry."
  (interactive)
    (let* ((pnt (point))
         (field-name
          (progn
            (beginning-of-line)
            (condition-case errname
                (bibtex-enclosing-regexp bibtex-field)
              (search-failed
               (goto-char pnt)
               (error "Not on BibTeX field")))
            (re-search-backward
             "^[ \t]*\\([A-Za-z]+\\)[ \t\n]*=" nil t)
            (let ((mb (match-beginning 1))
                  (me (match-end 1)))
              (buffer-substring
               (if (looking-at "^[ \t]*OPT")
                   (+ 3 mb)
                 mb)
               me))))
         (reference-type
          (progn
            (re-search-backward
             "^@\\([A-Za-z]+\\)[ \t\n]*[{(][^, \t\n]*[ \t\n]*," nil t)
            (buffer-substring (match-beginning 1) (match-end 1))))
         (entry-list
          (assoc-ignore-case reference-type
                               bibtex-entry-field-alist))
         (c-r-list (elt entry-list 2))
         (req-opt-list
          (if (and
               (member reference-type bibtex-include-OPTcrossref)
               c-r-list)
              c-r-list
            (elt entry-list 1)))
         (list-of-entries (append
                           (elt req-opt-list 0)
                           (elt req-opt-list 1)
                           bibtex-mode-user-optional-fields
                           (if (member
                                reference-type
                                bibtex-include-OPTcrossref)
                               '(("crossref"
                                  "Label of the crossreferenced entry")))
                           (if bibtex-include-OPTannote
                               '(("annote"
                                  "Personal annotation (ignored)")))
                           (if bibtex-include-OPTkey
                               '(("key"
                                  "Key used for label creation if author and editor fields are missing"))))))
    (goto-char pnt)
    (if (assoc field-name list-of-entries)
        (message (elt (assoc field-name list-of-entries) 1))
      (message "NO COMMENT AVAILABLE"))))

(defun bibtex-make-field (e-t)
  "Makes a field named E-T in current BibTeX entry."
  (interactive "sBibTeX entry type: ")
  (let ((name  (elt e-t 0)))
    (insert ",\n")
    (indent-to-column bibtex-name-alignment)
    (insert name " = ")
    (indent-to-column bibtex-text-alignment)
    (insert bibtex-field-left-delimiter bibtex-field-right-delimiter)))

(defun bibtex-make-optional-field (e-t)
  "Makes an optional field named E-T in current BibTeX entry."
  (interactive "sOptional BibTeX entry type: ")
  (if (consp e-t)
      (setq e-t (cons (concat "OPT" (car e-t)) (cdr e-t)))
    (setq e-t (concat "OPT" e-t)))
  (bibtex-make-field e-t))

(defun bibtex-beginning-of-entry ()
  "Move to beginning of BibTeX entry.
If inside an entry, move to the beginning of it, otherwise move to the
beginning of the previous entry."
  (interactive)
  (if (looking-at "^@")
      (forward-char))
  (re-search-backward "^@" nil 'move))

(defun bibtex-end-of-entry ()
  "Move to end of BibTeX entry.
If inside an entry, move to the end of it, otherwise move to the end
of the previous entry."
  (interactive)
  (bibtex-beginning-of-entry)
  (let ((parse-sexp-ignore-comments t))
    (forward-sexp 2) ;; skip entry type and body
    ))
  
(defun bibtex-ispell-entry ()
  "Spell whole BibTeX entry."
  (interactive)
  (ispell-region (progn (bibtex-beginning-of-entry) (point))
		 (progn (bibtex-end-of-entry) (point))))

(defun bibtex-ispell-abstract ()
  "Spell abstract of BibTeX entry."
  (interactive)
  (let ((pnt (bibtex-end-of-entry)))
    (bibtex-beginning-of-entry)
    (if (null
         (re-search-forward "^[ \t]*[OPT]*abstract[ \t]*=" pnt))
        (error "No abstract in entry.")))
  (ispell-region (point)
		 (save-excursion (forward-sexp) (point))))

(defun bibtex-narrow-to-entry ()
  "Narrow buffer to current BibTeX entry."
  (interactive)
  (save-excursion
    (narrow-to-region (progn (bibtex-beginning-of-entry) (point))
		      (progn (bibtex-end-of-entry) (point)))))


(defun bibtex-hide-entry-bodies (&optional arg)
  "Hide all lines between first and last BibTeX entries not beginning with @.
With argument, show all text."
  (interactive "P")
  (save-excursion
    (beginning-of-first-bibtex-entry)
    ;; subst-char-in-region modifies the buffer, despite what the
    ;; documentation says...
    (let ((modifiedp (buffer-modified-p))
	  (buffer-read-only nil))
      (if arg
	  (subst-char-in-region (point) (point-max) ?\r ?\n t)
	(while (save-excursion (re-search-forward "\n[^@]" (point-max) t))
	  ;; (save-excursion (replace-regexp "\n\\([^@]\\)" "\r\\1"))
	  (save-excursion
	    (while (re-search-forward "\n\\([^@]\\)" nil t)
	      (replace-match "\r\\1" nil nil)))))
      (setq selective-display (not arg))
      (set-buffer-modified-p modifiedp))))

(defun bibtex-sort-entries ()
  "Sort BibTeX entries alphabetically by key.
Text before the first BibTeX entry, and following the last is not affected.
If bibtex-sort-ignore-string-entries is true, @string entries will be ignored.

Bugs:
  1. Text between the closing brace ending one BibTeX entry, and the @ starting
     the next, is considered part of the PRECEDING entry.  Perhaps it should be
     part of the following entry."
  (interactive)
  (save-restriction
    (beginning-of-first-bibtex-entry)
    (narrow-to-region
     (point)
     (save-excursion
       (goto-char (point-max))
       (bibtex-end-of-entry)
       (point)))
    (sort-subr
     nil
     ;; NEXTREC function
     'forward-line
     ;; ENDREC function
     (function
      (lambda ()
        (and
         (re-search-forward "}\\s-*\n[\n \t]*@" nil 'move)
         (forward-char -2))))
     ;; STARTKEY function
     (if bibtex-sort-ignore-string-entries
         (function
          (lambda ()
            (while
                (and
                 (re-search-forward "^\\s-*\\([@a-zA-Z]*\\)\\s-*{\\s-*")
                 (string-equal
                  "@string"
                  (downcase
                   (buffer-substring
                    (match-beginning 1)
                    (match-end 1))))))
            nil))
       (function
        (lambda ()
          (re-search-forward "{\\s-*"))))
     ;; ENDKEY function
     (function
      (lambda ()
        (search-forward ","))))))
  
(defun bibtex-find-entry-location (entry-name &optional maybedup)
  "Looking for place to put the BibTeX entry named ENTRY-NAME.
Searches from beginning of buffer. Buffer is assumed to be in sorted
order, without duplicates (see \\[bibtex-sort-entries]), if it is not,
an error will be signalled. If optional argument MAYBEDUP is non-nil
no error/warning messages about ENTRY-NAME being a (potential)
duplicate of an existing entry will be emitted. This function returns
`nil' if ENTRY-NAME is a duplicate of an existing entry and t in all
other cases."
  (interactive "sBibtex entry key: ")
  (let ((nodup t)
        (previous nil)
	point)
    (beginning-of-first-bibtex-entry)
    (or
     (catch 'done
       (map-bibtex-entries
        (function
         (lambda (current)
           (cond ((string-equal entry-name current)
                  (setq nodup nil)
                  (bibtex-beginning-of-entry)
                  (if maybedup
                      (throw 'done t)
                    (error "Entry duplicates existing!")))
                 ((or (null previous)
                      (string< previous current))
                  (setq previous current
                        point (point))
                  (if (string< entry-name current)
                      (progn
                        (bibtex-beginning-of-entry)
                        ;; Many schemes append strings to
                        ;; existing entries to resolve them,
                        ;; so initial substring matches may
                        ;; indicate a duplicate entry.  
                        (let ((idx
                               (string-match
                                (regexp-quote entry-name) current)))
                          (if (and
                               (integerp idx)
                               (zerop idx)
                               (not maybedup)
                               (not (equal entry-name "")))
                              (progn
                                (message
                                 "Warning: Entry %s may be a duplicate of %s!"
                                 entry-name current)
                                (ding t))))
                        (throw 'done t))))
                 ((string-equal previous current)
                  (error "Duplicate here with previous!"))
                 (t (error "Entries out of order here!")))))))
     (goto-char (point-max)))
    nodup))

(defun bibtex-validate-buffer ()
  "Validate if the current BibTeX buffer is syntactically correct.
Any garbage (e.g. comments) before the first \"@\" is not tested (so
you can put comments here)."
  (interactive)
  (let ((pnt (point))
        (max (point-max)))
    (goto-char (point-min))
    (while (< (re-search-forward "@\\|\\'") max)
      (forward-char -1)
      (let ((p (point)))
        (if (looking-at "@string")
            (forward-char)
          (if (not (and
                    (re-search-forward bibtex-reference nil t)
                    (equal p (match-beginning 0))))
              (progn
                (goto-char p)
                (error "Bad entry begins here"))))))
    (bibtex-find-entry-location (make-string 10 255))
    ;; find duplicates
    (goto-char pnt)
    (message "BibTeX buffer is syntactically correct")))

(defun bibtex-next-field (arg)
  "Finds end of text of next BibTeX field; with arg, to its beginning."
  (interactive "P")
  (bibtex-inside-field)
  (let ((start (point)))
    (condition-case ()
	(progn
	  (bibtex-enclosing-field)
	  (goto-char (match-end 0))
	  (forward-char 2))
      (error
       (goto-char start)
       (end-of-line)
       (forward-char 1))))
  (bibtex-find-text arg))

(defun bibtex-find-text (arg)
  "Go to end of text of current field; with arg, go to beginning."
  (interactive "P")
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (if arg
      (progn
	(goto-char (match-beginning bibtex-text-in-field))
	(if (looking-at "[{\"]")
	    (forward-char 1)))
    (goto-char (match-end bibtex-text-in-field))
    (if (or
         (= (preceding-char) ?})
         (= (preceding-char) ?\"))
	(forward-char -1)))
  (if bibtex-help-message
      (bibtex-print-help-message)))

(defun bibtex-remove-OPT ()
  "Removes the 'OPT' starting optional arguments and goes to end of text."
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (save-excursion
    (goto-char (match-beginning bibtex-name-in-field))
    (if (looking-at "OPT")
	;; sct@dcs.edinburgh.ac.uk
 	(progn
 	  (delete-char (length "OPT"))
 	  (search-forward "=")
 	  (delete-horizontal-space)
 	  (indent-to-column bibtex-text-alignment))))
  (bibtex-inside-field))

(defun bibtex-remove-double-quotes-or-braces ()
  "Removes \"\" or {} around string."
  (interactive)
  (save-excursion
    (bibtex-inside-field)
    (bibtex-enclosing-field)
    (let ((start (match-beginning bibtex-text-in-field))
	  (stop (match-end  bibtex-text-in-field)))
      (goto-char stop)
      (forward-char -1)
      (if (looking-at "[}\"]")
	  (delete-char 1))
      (goto-char start)
      (if (looking-at "[{\"]")
	  (delete-char 1)))))

(defun bibtex-kill-optional-field ()
  "Kill the entire enclosing optional BibTeX field."
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (goto-char (match-beginning bibtex-name-in-field))
  (let ((the-end (match-end 0))
	(the-beginning (match-beginning 0)))
    (if (looking-at "OPT")
	(progn
	  (goto-char the-end)
	  (skip-chars-forward " \t\n,")
	  (kill-region the-beginning the-end))
      (error "Mandatory fields can't be killed"))))

(defun bibtex-empty-field ()
  "Delete the text part of the current field, replace with empty text."
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (goto-char (match-beginning bibtex-text-in-field))
  (kill-region (point) (match-end bibtex-text-in-field))
  (insert (concat bibtex-field-left-delimiter
                  bibtex-field-right-delimiter)) 
  (bibtex-find-text t))

(defun bibtex-pop-previous (arg)
  "Replace text of current field with the text of similar field in previous entry.
With arg, go up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-next] (bibtex-pop-next)."
  (interactive "p")
  (bibtex-inside-field)
  (save-excursion
    ; parse current field
    (bibtex-enclosing-field)
    (let ((start-old-text (match-beginning bibtex-text-in-field))
	  (stop-old-text  (match-end bibtex-text-in-field))
	  (start-name (match-beginning bibtex-name-in-field))
	  (stop-name (match-end bibtex-name-in-field))
	  (new-text))
      (goto-char start-name)
      ; construct regexp for previous field with same name as this one
      (let ((matching-entry
	     (bibtex-cfield
	      (buffer-substring (if (looking-at "OPT")
				    (+ (point) (length "OPT"))
				  (point))
				stop-name)
	      bibtex-field-text)))
	; if executed several times in a row, start each search where the
	; last one finished
	(cond ((or (eq last-command 'bibtex-pop-previous)
		   (eq last-command 'bibtex-pop-next))
	       t
	       )
	      (t
	       (bibtex-enclosing-reference)
	       (setq bibtex-pop-previous-search-point (point))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-previous-search-point)
	; Now search for arg'th previous similar field
	(cond
	 ((re-search-backward matching-entry (point-min) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
          ;; change delimiters, if any changes needed
          (cond
           ((and
             (equal bibtex-field-left-delimiter "{")
             (eq (aref new-text 0) ?\")
             (eq (aref new-text (1- (length new-text))) ?\"))
            (aset new-text 0 ?\{)
            (aset new-text (1- (length new-text)) ?\}))
           ((and
             (equal bibtex-field-left-delimiter "\"")
             (eq (aref new-text 0) ?\{)
             (eq (aref new-text (1- (length new-text))) ?\}))
            (aset new-text 0 ?\")
            (aset new-text (1- (length new-text)) ?\"))
           ((or
             (not (eq (aref new-text 0)
                      (aref bibtex-field-left-delimiter 0)))
             (not (eq (aref new-text (1- (length new-text)))
                      (aref bibtex-field-right-delimiter 0))))
            (setq new-text (concat bibtex-field-left-delimiter
                                   new-text 
                                   bibtex-field-right-delimiter))))
	  ; Found a matching field. Remember boundaries.
	  (setq bibtex-pop-next-search-point (match-end 0))
	  (setq bibtex-pop-previous-search-point (match-beginning 0))
	  (bibtex-flash-head)
	  ; Go back to where we started, delete old text, and pop new.
	  (goto-char stop-old-text)
	  (delete-region start-old-text stop-old-text)
	  (insert new-text))
	 (t				; search failed
	  (error "No previous matching BibTeX field."))))))
  (setq this-command 'bibtex-pop-previous))

(defun bibtex-pop-next (arg)
  "Replace text of current field with the text of similar field in next entry.
With arg, go up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-previous] (bibtex-pop-previous)."
  (interactive "p")
  (bibtex-inside-field)
  (save-excursion
    ; parse current field
    (bibtex-enclosing-field)
    (let ((start-old-text (match-beginning bibtex-text-in-field))
	  (stop-old-text  (match-end bibtex-text-in-field))
	  (start-name (match-beginning bibtex-name-in-field))
	  (stop-name (match-end bibtex-name-in-field))
	  (new-text))
      (goto-char start-name)
      ; construct regexp for next field with same name as this one,
      ; ignoring possible OPT's
      (let ((matching-entry
	     (bibtex-cfield
	      (buffer-substring (if (looking-at "OPT")
				    (+ (point) (length "OPT"))
				  (point))
				stop-name)
	      bibtex-field-text)))
	
	; if executed several times in a row, start each search where the
	; last one finished
	(cond ((or (eq last-command 'bibtex-pop-next)
		   (eq last-command 'bibtex-pop-previous))
	       t
	       )
	      (t
	       (bibtex-enclosing-reference)
	       (setq bibtex-pop-previous-search-point (point))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-next-search-point)
	
	; Now search for arg'th next similar field
	(cond
	 ((re-search-forward matching-entry (point-max) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
          ;; change delimiters, if any changes needed
          (cond
           ((and
             (equal bibtex-field-left-delimiter "{")
             (eq (aref new-text 0) ?\")
             (eq (aref new-text (1- (length new-text))) ?\"))
            (aset new-text 0 ?\{)
            (aset new-text (1- (length new-text)) ?\}))
           ((and
             (equal bibtex-field-left-delimiter "\"")
             (eq (aref new-text 0) ?\{)
             (eq (aref new-text (1- (length new-text))) ?\}))
            (aset new-text 0 ?\")
            (aset new-text (1- (length new-text)) ?\"))
           ((or
             (not (eq (aref new-text 0)
                      (aref bibtex-field-left-delimiter 0)))
             (not (eq (aref new-text (1- (length new-text)))
                      (aref bibtex-field-right-delimiter 0))))
            (setq new-text (concat bibtex-field-left-delimiter
                                   new-text 
                                   bibtex-field-right-delimiter))))
	  ; Found a matching field. Remember boundaries.
	  (setq bibtex-pop-next-search-point (match-end 0))
	  (setq bibtex-pop-previous-search-point (match-beginning 0))
	  (bibtex-flash-head)
	  ; Go back to where we started, delete old text, and pop new.
	  (goto-char stop-old-text)
	  (delete-region start-old-text stop-old-text)
	  (insert new-text))
	 (t				; search failed
	  (error "No next matching BibTeX field."))))))
  (setq this-command 'bibtex-pop-next))

(defun bibtex-clean-entry (&optional arg)
  "Finish editing the current BibTeX entry and clean it up.
For all optional fields of current BibTeX entry: if empty, kill the
whole field; otherwise, remove the \"OPT\" string in the name; if text
numerical, remove double-quotes. For all mandatory fields: if empty,
signal error. If label of entry is empty or a prefix argument was
given, calculate a new entry label."
  (interactive "P")
  (bibtex-beginning-of-entry)
  (let ((start (point))
        crossref-there)
    (save-restriction
      (narrow-to-region start (save-excursion (bibtex-end-of-entry) (point)))
      (while (and
              (re-search-forward bibtex-field (point-max) t 1)
              (not crossref-there))
        ;; determine if reference has crossref entry
	(let ((begin-name (match-beginning bibtex-name-in-field))
	      (begin-text (match-beginning bibtex-text-in-field)))
	  (goto-char begin-name)
          (if (looking-at "\\(OPTcrossref\\)\\|\\(crossref\\)")
              (progn
                (goto-char begin-text)
                (if (not (looking-at
                          (concat
                           bibtex-field-left-delimiter
                           bibtex-field-right-delimiter)))
                    (setq crossref-there t))))))
      (bibtex-enclosing-reference)
      (re-search-forward bibtex-reference-type)
      (let ((begin-type (1+ (match-beginning 0)))
            (end-type (match-end 0)))
        (goto-char start)
        (while (re-search-forward bibtex-field (point-max) t 1)
          (let ((begin-field (match-beginning 0))
                (end-field (match-end 0))
                (begin-name (match-beginning bibtex-name-in-field))
                (end-name (match-end  bibtex-name-in-field))
                (begin-text (match-beginning bibtex-text-in-field))
                (end-text (match-end bibtex-text-in-field))
                )
            (goto-char begin-name)
            (cond ((and
                    (looking-at "OPT")
                    bibtex-clean-entry-zap-empty-opts)
                   (goto-char begin-text)
                   (if (looking-at
                        (concat
                         bibtex-field-left-delimiter
                         bibtex-field-right-delimiter))
                       ;; empty: delete whole field if really optional
                       ;; (missing crossref handled) or complain
                       (if (and
                            (not crossref-there)
                            (assoc
                             (downcase
                              (buffer-substring
                               (+ (length "OPT") begin-name) end-name))
                             (car (car (cdr
                                        (assoc-ignore-case
                                         (buffer-substring begin-type end-type)
                                         bibtex-entry-field-alist))))))
                           ;; field is not really optional
                           (progn
                             (goto-char begin-name)
                             (delete-char (length "OPT"))
                             ;; make field non-OPT
                             (search-forward "=")
                             (delete-horizontal-space)
                             (indent-to-column bibtex-text-alignment)
                             (forward-char)
                             ;; and loop to go through next test
                             (error "Mandatory field ``%s'' is empty"
                                    (buffer-substring begin-name
                                                      end-name)))
                         ;; field is optional
                         (delete-region begin-field end-field))
                     ;; otherwise: not empty, delete "OPT"
                     (goto-char begin-name)
                     (delete-char (length "OPT"))
                     (progn
                       ;; fixup alignment. [alarson:19920309.2047CST]
                       (search-forward "=")
                       (delete-horizontal-space)
                       (indent-to-column bibtex-text-alignment))
                     (goto-char begin-field) ; and loop to go through next test
                     ))
                  (t
                   (goto-char begin-text)
                   (cond ((looking-at (concat
                                       bibtex-field-left-delimiter
                                       "[0-9]+"
                                       bibtex-field-right-delimiter))
                          ;; if numerical,
                          (goto-char end-text)
                          (delete-char -1) ; delete enclosing double-quotes
                          (goto-char begin-text)
                          (delete-char 1)
                          (goto-char end-field) ; go to end for next search
                          (forward-char -2) ; to compensate for the 2 quotes deleted
                          )
                         ((looking-at (concat
                                       bibtex-field-left-delimiter
                                       bibtex-field-right-delimiter))
                          ;; if empty quotes, complain
                          (forward-char 1)
                          (if (not (or (equal (buffer-substring
                                               begin-name
                                               (+ begin-name 3))
                                              "OPT")
                                       (equal (buffer-substring
                                               begin-name
                                               (+ begin-name 3))
                                              "opt")))
                              (error "Mandatory field ``%s'' is empty"
                                     (buffer-substring begin-name end-name))))
                         (t
                          (goto-char end-field)))))))))
    (goto-char start)
    (bibtex-end-of-entry)
    ;; sct@dcs.edinburgh.ac.uk
    (save-excursion
      (forward-line -1)
      (end-of-line)
      (if (eq (preceding-char) ?,)
 	  (backward-delete-char 1))))
  (let* ((eob (progn
                (bibtex-end-of-entry)
                (point)))
         (key (progn
                (bibtex-beginning-of-entry)
                (if (search-forward-regexp
                     bibtex-reference-head eob t)
                    (buffer-substring
                     (match-beginning bibtex-key-in-head)
                     (match-end bibtex-key-in-head))))))
    (if (or
         arg
         (not key))
        (progn
          (let ((autokey
                 (if bibtex-autokey-edit-before-use
                     (read-from-minibuffer "Key to use: "
                                           (bibtex-generate-autokey))
                   (bibtex-generate-autokey))))
            (bibtex-beginning-of-entry)
            (search-forward-regexp "^@[A-Za-z]+[ \t]*[({]\\([^,]*\\)")
            (delete-region (match-beginning 1)
                           (match-end 1))
            (insert autokey)
            (let ((start (progn
                           (bibtex-beginning-of-entry)
                           (point)))
                  (end (progn
                         (bibtex-end-of-entry)
                         (search-forward-regexp "^@" nil 'move)
                         (beginning-of-line)
                         (point)))
                  last-command)
              (kill-region start end)
              (let ((success (bibtex-find-entry-location autokey t)))
                (yank)
                (bibtex-beginning-of-entry)
                (search-forward-regexp bibtex-reference-head)
                (setq kill-ring (cdr kill-ring))
                (if (not success)
                    (error
                     "Duplicated key (change manually and enter `bibtex-sort-entries')")))))))))

(defun bibtex-complete-string ()
  "Complete word fragment before point to longest prefix of a defined string.
If point is not after the part of a word, all strings are listed."
  (interactive "*")
  (let* ((end (point))
         (beg (save-excursion
                (re-search-backward "[ \t{\"]")
                (forward-char 1)
                (point)))
         (part-of-word (buffer-substring beg end))
         (string-list (copy-sequence bibtex-completion-candidates))
         (case-fold-search t)
         (completion (save-excursion
                       (progn
                         (while (re-search-backward
                                 "@string[ \t\n]*{" (point-min) t)
                           (goto-char (match-end 0))
                           (let ((pnt (point))
                                 (strt (match-beginning 0)))
                             (re-search-forward "[ \t\n]*="
                                                (point-max) t)
                             (goto-char (match-beginning 0))
                             (setq string-list
                                   (cons
                                    (list (buffer-substring pnt (point)))
                                    string-list))
                             (goto-char strt)))
                         (setq string-list
                               (sort string-list
                                     (lambda(x y)
                                       (string-lessp
                                        (car x)
                                        (car y)))))
                         (try-completion part-of-word string-list)))))
    (cond ((eq completion t)
           (bibtex-remove-double-quotes-or-braces))
          ((null completion)
           (error "Can't find completion for \"%s\"" part-of-word))
          ((not (string= part-of-word completion))
           (delete-region beg end)
           (insert completion)
           (if (assoc completion string-list)
               (bibtex-remove-double-quotes-or-braces)))
          (t
           (message "Making completion list...")
           (let ((list (all-completions part-of-word string-list)))
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list list)))
           (message "Making completion list...done")))))

(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article"))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book"))

(defun bibtex-Booklet ()
  (interactive)
  (bibtex-entry "Booklet"))

(defun bibtex-InBook ()
  (interactive)
  (bibtex-entry "InBook"))

(defun bibtex-InCollection ()
  (interactive)
  (bibtex-entry "InCollection"))

(defun bibtex-InProceedings ()
  (interactive)
  (bibtex-entry "InProceedings"))

(defun bibtex-Manual ()
  (interactive)
  (bibtex-entry "Manual"))

(defun bibtex-MastersThesis ()
  (interactive)
  (bibtex-entry "MastersThesis"))

(defun bibtex-Misc ()
  (interactive)
  (bibtex-entry "Misc"))

(defun bibtex-PhdThesis ()
  (interactive)
  (bibtex-entry "PhdThesis"))

(defun bibtex-Proceedings ()
  (interactive)
  (bibtex-entry "Proceedings"))

(defun bibtex-TechReport ()
  (interactive)
  (bibtex-entry "TechReport"))

(defun bibtex-Unpublished ()
  (interactive)
  (bibtex-entry "Unpublished"))

(defun bibtex-string ()
  (interactive)
  (bibtex-move-outside-of-entry)
  (insert
   (concat
    "@string{ = "
    bibtex-field-left-delimiter
    bibtex-field-right-delimiter
    "}\n"))
  (forward-line -1)
  (forward-char 8))

(defun bibtex-preamble ()
  (interactive)
  (bibtex-move-outside-of-entry)
  (insert "@Preamble{}\n")
  (forward-line -1)
  (forward-char 10))



;;; MAKE BIBTEX a FEATURE

(provide 'bibtex)


;;; bibtex.el ends here

