;;; bibtex.el --- BibTeX mode for GNU Emacs

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Bengt Martensson <ubrinf!mond!bengt>
;;	Mark Shapiro <shapiro@corto.inria.fr>
;;	Mike Newton <newton@gumby.cs.caltech.edu>
;;	Aaron Larson <alarson@src.honeywell.com>
;; Version: 1.3.1
;; Maintainer:Aaron Larson <alarson@src.honeywell.com>
;; Adapted-By: ESR
;; Keywords: tex, bib

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

;;; TODO distribute texinfo file.

;;; LCD Archive Entry:
;;; bibtex-mode|Bengt Martensson, Marc Shapiro, Aaron Larson|
;;; alarson@src.honeywell.com|
;;; Support for maintaining BibTeX format bibliography databases|
;;; 93-03-29|version 1.3|~/modes/bibtex-mode.el.Z|

;;; Commentary:

;;; BUGS:
;;;   1. using regular expressions to match the entire bibtex entry dies
;;;      on long bibtex entires (e.g. those containing abstracts) since
;;;      the length of regular expression matches is fairly limited.
;;;   2. When inserting a string (with \C-C\C-E\s) hitting a TAB results
;;;      in the error message "Can't find enclosing Bibtex field" instead
;;;      of moving to the empty string. [reported by gernot@cs.unsw.oz.au]
;;;   3. Function string-equalp should be in a library file, not in this
;;;      file. 

;;; (current keeper: alarson@src.honeywell.com
;;;  previous: shapiro@corto.inria.fr)

;;; Change Log:

;; Mon Mar 29 14:06:06 1993  Aaron Larson  (alarson at gendibal)
;;
;;	* bibtex.el: V1.3 released Mar 30, 1993
;;	(bibtex-field-name):  Fix to match definition if latex manual,
;;	specifically letters, digits, and punctuation other than comma.
;;	Underscore is retained for historical reasons. 
;;	(bibtex-make-field):  Fix to work around bug in Lucid prin1-to-string
;;	function as reported by Martin Sjolin <marsj@ida.liu.se>. 
;;	(bibtex-entry):  minor code cleanup.
;;	(bibtex-mode-map): Remove key binding (C-c n) for
;;	narrow-to-bibtex-entry, previous binding violated emacs policy of
;;	reserving C-c followed by a letter for user customization.
;;	revise modification history to better conform to FSF changelog
;;	standards.
;;	(bibtex-refile-entry): Removed.  Would need disclaimer papers to
;;	incorporate it into official sources, and unable to contact author.
;;	Fix minor "syntax" errors in documentation strings and such found
;;	by new byte compiler.  Funs bibtex-mode, bibtex-remove-double-quotes
;;
;;
;; Fri Jan 15 14:06:06 1993  Aaron Larson  (alarson at gendibal)
;;
;;	* bibtex.el: V1.2 released Feb 15 1993
;;	(find-bibtex-entry-location bibtex-make-field): Fixed placement of
;;	"interactive specification".  [Bug report from
;;	mernst@theory.lcs.mit.edu] 
;;	Fixed problem where bibtex-entry would fail if user typed entry
;;	name in wrong case.
;;	(bibtex-inside-field) Position the cursor _before_ the last comma
;;	on a line (the comma is not necessarily "inside" the field); this
;;	does not seem to break any existing code. ref sct@dcs.edinburgh.ac.uk
;;	(bibtex-enclosing-field, bibtex-enclosing-reference): leave
;;	point unmoved if no enclosing field/reference is found.  As a
;;	result of changes (3) and (4) bibtex-next-field works properly,
;;	even when called from the entry key position. 
;;	(bibtex-remove-OPT): realign the '=' after removing the 'opt'.
;;	(bibtex-clean-entry): always remove any trailing comma from the
;;	end of a bibtex entry (these commas get stripped automatically when
;;	optional fields are killed by bibtex-kill-optional-field, but can be
;;	left if optional fields are removed by other means).
;;	(bibtex-x-help) Replace tab with spaces in X menu as noted by
;;	khera@cs.duke.edu
;;	(bibtex-refile-entry): Added (from brannon@jove.cs.caltech.edu)
;;	(bibtex-sort-ignore-string-entries sort-bibtex-entries,
;;	map-bibtex-entries): Added variable as requested by  
;;	gernot@cs.unsw.oz.au, required changes to funs.
;;	(bibtex-current-entry-label): Added at request of
;;	yasuro@maekawa.is.uec.ac.jp
;;	(bibtex-DEAthesis:) Deleted along with corresponding entry from
;;	bibtex-x-help  per shapiro@corto.inria.fr 
;;	Moved narrow-to-bibtex-entry from C-c C-n to C-c n (the previous
;;	binding was in conflict with the binding for bibtex-pop-next.
;;	bug report from [shapiro@corto.inria.fr]
;;

;;; 
;;; alarson@src.honeywell.com 92-Feb-13
;;;   1. Made bibtex-entry user callable, now prompts for entry type (e.g.
;;;      Article), with completion, and bound it to a key.  This is now my
;;;      preferred way to add most entries. 
;;;   2. Made fields of a bibtex entry derived from the alist bibtex-entry-
;;;      field-alist.
;;;   3. Fixed handling of escaped double quotes, e.g. "Schr{\"o}dinger".
;;;   4. Fixed bug where unhiding bibtex entries moved point.
;;;   5. Made "field name" specs permit (name . value) for defaulting.  E.g. 
;;;       (setq bibtex-mode-user-optional-fields '(("library" . "alarson")))
;;;      will generate the field:
;;;       library	= "alarson",
;;;   6. Added binding for narrow-to-bibtex-entry
;;;   7. Adding a bibtex entry now runs hook: bibtex-add-entry-hook
;;;   8. Made bibtex-clean-entry fixup text alignment, and eliminated the
;;;      dependency on bibtex-enclosing-reference which has a problem with
;;;      long entries (e.g. those containing abstracts).
;;; 
;;; alarson@src.honeywell.com 92-Jan-31
;;;   Added support for: ispell, beginning/end of entry movement, a simple
;;;   outline like mode (hide the bodies of bibtex entries), support for
;;;   sorting bibtex entries, and maintaining them in sorted order, and
;;;   simple buffer validation.
;;;   User visible functions added:
;;;      ispell-{abstract,bibtex-entry}, {beginning,end}-of-bibtex-entry
;;;      hide-bibtex-entry-bodies, sort-bibtex-entries, validate-bibtex-
;;;      buffer, find-bibtex-duplicates
;;;   user visible variables added:
;;; 	 bibtex-maintain-sorted-entries
;;;   new local keybindings:
;;; 	C-c$   ispell-bibtex-entry
;;; 	M-C-a  beginning-of-bibtex-entry
;;; 	M-C-e  end-of-bibtex-entry
;;; Mike Newton (newton@gumby.cs.caltech.edu) 90.11.17
;;;  * Handle items like
;;;          title = poft # "Fifth Tri-quarterly" # random-conf,
;;;    and   title = {This title is inside curlies}
;;;  * added user settable, always present, optional fields
;;;  * fixed 'bibtex-find-it's doc string's location
;;;  * bibtex-field-text made more general (it wouldn't handle the # construct)
;;;		and it now handles a small subset of the {} cases

;;; Bengt Martensson, March 6
;;;   Adapted to Bibtex 0.99 by updating the optional fields according
;;;   to the document BibTeXing, Oren Patashnik, dated January 31, 1988.
;;;   Updated documentation strings accordingly.  Added (provide 'bibtex).
;;;   If bibtex-include-OPT-crossref is non-nil, every entry will have
;;;   an OPTcrossref field, analogously for bibtex-include-OPTkey and
;;;   bibtex-include-OPTannote.  Added bibtex-preamble, bound to ^C^EP,
;;;   and also found in X- and sun-menus.  Cleaned up the sun-menu
;;;   stuff, and made it more uniform with the X-menu stuff.  Marc: I
;;;   strongly suspect that I broke your parsing...  (Or, more
;;;   correctly, BibTeX 0.99 broke it.)
;;;   Added bibtex-clean-entry-zap-empty-opts, defvar'd to t.  If it
;;;   is nil, bibtex-clean-entry will leave empty optional fields alone.

;;; Marc Shapiro 1-feb-89: integrated changes by Bengt Martensson 88-05-06:
;;;   Added Sun menu support.  Locally bound to right mouse button in 
;;;   bibtex-mode.  Emacs 18.49 allows local mouse bindings!!
;;;   Commented out DEAthesis.

;;; Marc Shapiro 6-oct-88
;;;  * skip-whitespace replaced by skip-chars-forward
;;;  * use indent-to-column instead of inserting tabs (changes to 
;;;    bibtex-entry, bibtex-make-entry, bibtex-make-OPT-entry, renamed to
;;;    bibtex-make-optional-entry)
;;;  * C-c C-k deletes the current OPT entry entirely
;;;  * C-c C-d replaces text of field with ""
;;;  * renamed bibtex-find-it to bibtex-find-text.  With arg, now goes to
;;;    start of text.  Fixed bugs in it.

;;; Marc Shapiro 23-sep-88
;;;  * bibtex-clean-entry moves past end of entry.
;;;  * bibtex-clean-entry signals mandatory fields left empty.

;;; Marc Shapiro 18-jul-88
;;;  * Fixed bug in bibtex-flash-entry
;;;  * Moved all the entry type keystrokes to "C-c C-e something" (instead of
;;;    "C-c something" previously) to make room for more.  C-c C-e is
;;;    supposed to stand for "entry" [idea taken from mail-mode].  Moved
;;;    bibtex-pop-previous to C-c C-p and bibtex-pop-next to C-c C-n.
;;;  * removed binding for "\e[25~"
;;;  * replaced bibtex-clean-optionals by bibtex-clean-entry, bound to
;;;    C-c C-c

;;; Marc Shapiro 13-jul-88 [based on ideas by Sacha Krakowiak of IMAG]
;;;  * bibtex-pop-previous replaces current field with value of
;;;    similar field in previous entry.  May be called n times in a row
;;;    (or with arg n) to pop similar field of n'th previous entry.
;;;    There is also a bibtex-pop-next to get similar field of next
;;;    entry.
;;;  * C-c C-k now kills all empty optional fields of current entry, and
;;;    removes "OPT" for those optional fields which have text. 

;;; Marc Shapiro 14-dec-87
;;;   Cosmetic fixes.  Fixed small bug in bibtex-move-outside-of-entry.
;;; Skip Montanaro <steinmetz!sprite!montanaro> 7-dec-87, Shapiro 10-dec-87
;;;   before inserting an entry, make sure we are outside of a bib entry
;;; Marc Shapiro 3-nov-87
;;;   addition for France: DEAthesis
;;; Marc Shapiro 19-oct-1987
;;;   add X window menu option; bug fixes. TAB, LFD, C-c " and C-c C-o now
;;;   behave consistently; deletion never occurs blindly.
;;; Marc Shapiro <shapiro@inria.inria.fr> 15-oct-1986
;;;    align long lines nicely; C-c C-o checks for the "OPT" string;
;;;    TAB goes to the end of the string; use lower case; use
;;;    run-hooks

;;; Bengt Martensson <ubrinf!mond!bengt> 87-06-28
;;; Bengt Martensson <bengt@mathematik.uni-Bremen.de> 87-06-28
;;;   Original version

;;; Code:

;;; NOTE by Marc Shapiro, 14-dec-87:
;;; (bibtex-x-environment) binds an X menu for bibtex mode to x-button-c-right.
;;; Trouble is, in Emacs 18.44 you can't have a mode-specific mouse binding,
;;; so it will remain active in all windows.  Yuck!

(provide 'bibtex)

;;; these guys typically don't have autoloads...[alarson:19920131.1548CST]
;;; Check for fboundp first so that if user autoloads them from non standard 
;;; places, the users bindings will take precedence.
(if (not (fboundp 'sort-subr))
    (autoload 'sort-subr "sort"))

;;; These should be in a more generally accessible location. 

(defun string-equalp (s1 s2)
  "Like string= except differences in case are ignored."
  (let ((ss1 (if (symbolp s1) (symbol-name s1) s1))
	(ss2 (if (symbolp s2) (symbol-name s2) s2)))
    (and (= (length ss1) (length ss2))
	 (string-equal (upcase ss1) (upcase ss2)))))




(defvar bibtex-clean-entry-zap-empty-opts t
  "*If non-nil, bibtex-clean-entry will delete all empty optional fields.")

(defvar bibtex-include-OPTcrossref t
  "*If non-nil, all entries will have an OPTcrossref field.")
(defvar bibtex-include-OPTkey t
  "*If non-nil, all entries will have an OPTkey field.")
(defvar bibtex-include-OPTannote t
  "*If non-nil, all entries will have an OPTannote field.")

;; note: the user should be allowed to have their own list of always
;;       available optional fields.  exs: "keywords" "categories"

(defvar bibtex-mode-user-optional-fields nil		;no default value
  "*List of optional fields that user want to have as always present 
when making a bibtex entry.  One possibility is for ``keywords''.  
Entries can be either strings or conses, in which case the car should be 
string and the cdr the value to be inserted.")

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

(defvar bibtex-mode-abbrev-table nil "")
(define-abbrev-table 'bibtex-mode-abbrev-table ())
(defvar bibtex-mode-map
  (let ((km (make-sparse-keymap)))
    
    (define-key km "\t" 'bibtex-find-text)
    (define-key km "\n" 'bibtex-next-field)
    (define-key km "\C-c\"" 'bibtex-remove-double-quotes)
    (define-key km "\C-c\C-c" 'bibtex-clean-entry)
    (define-key km "\C-c?" 'describe-mode)
    (define-key km "\C-c\C-p" 'bibtex-pop-previous)
    (define-key km "\C-c\C-n" 'bibtex-pop-next)
    (define-key km "\C-c\C-k" 'bibtex-kill-optional-field)
    (define-key km "\C-c\C-d" 'bibtex-empty-field)

    ;; [alarson:19920131.1543CST]
    (define-key km "\C-c$"   'ispell-bibtex-entry)
    (define-key km "\M-\C-a"   'beginning-of-bibtex-entry)
    (define-key km "\M-\C-e"   'end-of-bibtex-entry)
    (define-key km "\C-c\C-b"   'bibtex-entry)
;    (define-key km "\C-cn" 'narrow-to-bibtex-entry)

    (define-key km "\C-c\C-e\C-a" 'bibtex-Article)
    (define-key km "\C-c\C-e\C-b" 'bibtex-Book)
;    (define-key km "\C-c\C-e\C-d" 'bibtex-DEAthesis)
    (define-key km "\C-c\C-e\C-c" 'bibtex-InProceedings)
    (define-key km "\C-c\C-e\C-i" 'bibtex-InBook)
    (define-key km "\C-c\C-ei" 'bibtex-InCollection)
    (define-key km "\C-c\C-eI" 'bibtex-InProceedings)
    (define-key km "\C-c\C-e\C-m" 'bibtex-Manual)
    (define-key km "\C-c\C-em" 'bibtex-MastersThesis)
    (define-key km "\C-c\C-eM" 'bibtex-Misc)
    (define-key km "\C-c\C-o" 'bibtex-remove-OPT)
    (define-key km "\C-c\C-e\C-p" 'bibtex-PhdThesis)
    (define-key km "\C-c\C-ep" 'bibtex-Proceedings)
    (define-key km "\C-c\C-eP" 'bibtex-preamble)
    (define-key km "\C-c\C-e\C-t" 'bibtex-TechReport)
    (define-key km "\C-c\C-e\C-s" 'bibtex-string)
    (define-key km "\C-c\C-e\C-u" 'bibtex-Unpublished)
    km))

(defvar bibtex-pop-previous-search-point nil
  "Next point where bibtex-pop-previous should start looking for a similar
entry.")

(defvar bibtex-pop-next-search-point nil
  "Next point where bibtex-pop-next should start looking for a similar
entry.")

(defvar bibtex-entry-field-alist
  '(
    ("Article" . ((("author" "title" "journal" "year")
		   ("volume" "number" "pages" "month" "note"))
		  (("author" "title")
		   ("journal" "year" "volume" "number" "pages"
		    "month" "note"))))
    ("Book" . ((("author" "title" "publisher" "year")
		("editor" "volume" "number" "series" "address"
		 "edition" "month" "note"))))
    ("Booklet" . ((("title")
		   ("author" "howpublished" "address" "month" "year" "note"))))
    
    ;; France: Dipl\^{o}me d'Etudes Approfondies (similar to Master's)
;    ("DEAthesis" . ((("author" "title" "school" "year")
;		     ("address" "month" "note"))))
    
    ("InBook" . ((("author" "title" "chapter" "publisher" "year")
		  ("editor" "pages" "volume" "number" "series" "address"
		   "edition" "month" "type" "note"))
		 (("author" "title" "chapter")
		  ("publisher" "year" "editor" "pages" "volume" "number"
		   "series" "address" "edition" "month" "type" "note"))))


    ("InCollection" . ((("author" "title"
			 "booktitle" "publisher" "year")
			("editor" "volume" "number" "series" "type" "chapter"
			 "pages" "address" "edition" "month" "note"))
		       (("author" "title")
			("booktitle" "publisher" "year"
			 "editor" "volume" "number" "series" "type" "chapter"
			 "pages" "address" "edition" "month" "note"))))


    ("InProceedings" . ((("author" "title" "booktitle" "year")
			 ("editor" "volume" "number" "series" "pages"
			  "organization" "publisher" "address" "month" "note"))
			(("author" "title")
			 ("editor" "volume" "number" "series" "pages"
			  "booktitle" "year"
			  "organization" "publisher" "address" "month" "note"))))


    ("Manual" . ((("title")
		  ("author" "organization" "address" "edition" "year"
		   "month" "note"))))

    ("MastersThesis" . ((("author" "title" "school" "year")
			 ("address" "month" "note" "type"))))

    ("Misc" . ((()
		("author" "title" "howpublished" "year" "month" "note"))))

    ("PhdThesis" . ((("author" "title" "school" "year")
		     ("address" "month" "type" "note"))))

    ("Proceedings" . ((("title" "year")
		       ("editor" "volume" "number" "series" "publisher"
			"organization" "address" "month" "note"))))

    ("TechReport" . ((("author" "title" "institution" "year")
		      ("type" "number" "address" "month" "note"))))

    ("Unpublished" . ((("author" "title" "note")
		       ("year" "month"))))
    )

  "List of (entry-name (required optional) (crossref-required crossref-optional))
tripples.  If the third element is nil, then the first pair can be used.  Required
and optional are lists of strings.  All entry creation functions use this variable
to generate entries, and bibtex-entry ensures the entry type is valid.  This 
variable can be used for example to make bibtex manipulate a different set of entry
types, e.g. a crossreference document of organization types.")


;;; A bibtex file is a sequence of entries, either string definitions
;;; or reference entries.  A reference entry has a type part, a
;;; key part, and a comma-separated sequence of fields.  A string
;;; entry has a single field.  A field has a left and right part,
;;; separated by a '='.  The left part is the name, the right part is
;;; the text.  Here come the definitions allowing to create and/or parse
;;; entries and fields:

;;; fields
(defun bibtex-cfield (name text)
  "Create a regexp for a bibtex field of name NAME and text TEXT"
  (concat ",[ \t\n]*\\("
	  name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  text
	  "\\)"))
(defconst bibtex-name-in-cfield 1
  "The regexp subexpression number of the name part in bibtex-cfield.")
(defconst bibtex-text-in-cfield 2
  "The regexp subexpression number of the text part in bibtex-cfield.")

;;; KAWATA Yasuro <yasuro@qqqq.maekawa.is.uec.ac.jp> reported bug that "/"
;;; was not premitted in field names.  The old value of this var was:
;;;    "[A-Za-z][---A-Za-z0-9:_+]*"
;;; According to the LaTeX manual, page 71, the legal values are letters,
;;; digits, and punctuation other than comma.  Section 2.1 defines
;;; punctuation as:
;;;     .:;,?!`'()[]-/*@
;;; and says that += can be used in normal text.  Specifically #$%&~_^\{}
;;; are called out as special chars.  Some experimentation with LaTeX
;;; indicates that # and ~ definitely don't work, but that the following
;;; citation does! \cite{a0.:;?!`'()[]-/*@_&$^+=|<>}.  I chose here to
;;; permit _ since it was previously allowed, but otherwise to only handle
;;; punc and +=
;;; Amendment:  I couldn't get a regexp with both "[]"'s and hyphen to
;;; work.  It looks like you need them both to be the first entries in a
;;; regexp pattern. [alarson:19930315.0900CST]
;;;
;;; New amendment[MB]: I have corrected the following regexp according to 
;;; emacs19 rules.  I also added ']' to the list of characters allowed in 
;;; a field name.

(defconst bibtex-field-name "[A-Za-z][]A-Za-z0-9.:;?!`'()/*@_+=-]*"
  "Regexp defining the name part of a bibtex field.")

;; bibtex-field-text must be able to handle
;;   title = "Proc. Fifteenth Annual" # STOC,
;;   month = "10~" # jan,
;;   year = "{\noopsort{1973c}}1981",
;;   month = apr # "-" # may,
;;   key = {Volume-2},
;;   note = "Volume~2 is listed under Knuth \cite{book-full}"
;; i have added a few of these, but not all! -- MON

(defconst bibtex-field-const
  "[0-9A-Za-z][A-Za-z0-9:_+-]*"
  "Format of a bibtex field constant.")

(defconst bibtex-field-string
  (concat
    "\"[^\"]*[^\\\\]\"\\|\"\"")
  "Match either a string or an empty string.")

(defconst bibtex-field-string-or-const
  (concat bibtex-field-const "\\|" bibtex-field-string)
  "Match either bibtex-field-string or bibtex-field-const.")

(defconst bibtex-field-text
  (concat
    "\\(" bibtex-field-string-or-const "\\)"
        "\\([ \t\n]+#[ \t\n]+\\(" bibtex-field-string-or-const "\\)\\)*\\|"
    "{[^{}]*[^\\\\]}")
  "Regexp defining the text part of a bibtex field: either a string, or
an empty string, or a constant followed by one or more # / constant pairs.
Also matches simple {...} patterns.")

;(defconst bibtex-field-text
;  "\"[^\"]*[^\\\\]\"\\|\"\"\\|[0-9A-Za-z][A-Za-z0-9:_+-]*"
;  "Regexp defining the text part of a bibtex field: either a string, or an empty string, or a constant.")

(defconst bibtex-field
  (bibtex-cfield bibtex-field-name bibtex-field-text)
  "Regexp defining the format of a bibtex field")

(defconst bibtex-name-in-field bibtex-name-in-cfield
  "The regexp subexpression number of the name part in bibtex-field")
(defconst bibtex-text-in-field bibtex-text-in-cfield
  "The regexp subexpression number of the text part in bibtex-field")

;;; references
(defconst bibtex-reference-type
  "@[A-Za-z]+"
  "Regexp defining the type part of a bibtex reference entry")
(defconst bibtex-reference-head
  (concat "^[ \t]*\\("
	  bibtex-reference-type
	  "\\)[ \t]*[({]\\("
	  bibtex-field-name
	  "\\)")
  "Regexp defining format of the header line of a bibtex reference entry")
(defconst bibtex-type-in-head 1
  "The regexp subexpression number of the type part in bibtex-reference-head")
(defconst bibtex-key-in-head 2
  "The regexp subexpression number of the key part in
bibtex-reference-head")

(defconst bibtex-reference
  (concat
   ;; This is the unparenthesized equivalent of bibtex-reference-head:
   "^[ \t]*"
   bibtex-reference-type
   "[ \t]*[({]\\("
   bibtex-field-name
   "\\)"
   ;; End of unparenthesized equivalent of bibtex-reference-head
   "\\([ \t\n]*" bibtex-field "\\)*"
   "[ \t\n]*[})]")
  "Regexp defining the format of a bibtex reference entry")
(defconst bibtex-type-in-reference bibtex-type-in-head
  "The regexp subexpression number of the type part in bibtex-reference")
(defconst bibtex-key-in-reference bibtex-key-in-head
  "The regexp subexpression number of the key part in
bibtex-reference")

;;; strings
(defconst bibtex-string
  (concat "^[ \t]*@[sS][tT][rR][iI][nN][gG][ \t\n]*[({][ \t\n]*\\("
	  bibtex-field-name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  bibtex-field-text
	  "\\)[ \t\n]*[})]")
  "Regexp defining the format of a bibtex string entry")
(defconst bibtex-name-in-string 1
  "The regexp subexpression of the name part in bibtex-string")
(defconst bibtex-text-in-string 2
  "The regexp subexpression of the text part in bibtex-string")

(defconst bibtex-name-alignment 2
  "Alignment for the name part in BibTeX fields.
Chosen on aesthetic grounds only.")

(defconst bibtex-text-alignment (length "  organization = ")
  "Alignment for the text part in BibTeX fields.
Equal to the space needed for the longest name part.")

(defun bibtex-current-entry-label (&optional include-cite kill)
  "Return the label of the bibtex entry containing, or preceding point.
Optional argument INCLUDE-CITE, if true means put a '\\cite{}' around the
returned value.  Second optional argument KILL, if true, means place the
returned value in the kill buffer.  Interactively; providing prefix
argument makes INCLUDE-CITE true, and kill is true by default.

Rationale:
The intention is that someone will write a function that can be bound to
a mouse key so that people entering TeX can just mouse on the bibtex entry
and have the citation key inserted at the current point (which will almost
certainly be in some other buffer).  In the interim this function is 
marginally useful for keyboard binding and is not bound by default.  
Suggested binding is ^C-k."
  (interactive (list current-prefix-arg t))
  (save-excursion
    (beginning-of-bibtex-entry)
    (re-search-forward bibtex-reference-head (save-excursion (end-of-bibtex-entry) (point)))
    (let* ((key (buffer-substring (match-beginning bibtex-key-in-head)
				  (match-end bibtex-key-in-head)))
	   (val (if include-cite
		    (format "\\cite{%s}" key)
		    key)))
      (if kill
	  (kill-new val))
      val)))

;;; bibtex mode:

;;;###autoload
(defun bibtex-mode () 
  "Major mode for editing bibtex files.

\\{bibtex-mode-map}

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and thus ignored by BibTeX.
The OPT string may be removed from a field with \\[bibtex-remove-OPT].
\\[bibtex-kill-optional-field] kills the current optional field entirely.
\\[bibtex-remove-double-quotes] removes the double-quotes around the text of
the current field.  \\[bibtex-empty-field] replaces the text of the current
field with the default \"\".

The command \\[bibtex-clean-entry] cleans the current entry, i.e. (i) removes
double-quotes from entirely numerical fields, (ii) removes OPT from all
non-empty optional fields, (iii) removes all empty optional fields, and (iv)
checks that no non-optional fields are empty.

Use \\[bibtex-find-text] to position the dot at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

The following may be of interest as well:

  Functions:
    find-bibtex-duplicates
    find-bibtex-entry-location
    hide-bibtex-entry-bodies
    sort-bibtex-entries
    validate-bibtex-buffer

  Variables:
    bibtex-clean-entry-zap-empty-opts
    bibtex-entry-field-alist
    bibtex-include-OPTannote
    bibtex-include-OPTcrossref
    bibtex-include-OPTkey
    bibtex-maintain-sorted-entries
    bibtex-mode-user-optional-fields

Fields:
    address
           Publisher's address
    annote
           Long annotation used for annotated bibliographies (begins sentence)
    author
           Name(s) of author(s), in BibTeX name format
    booktitle
           Book title when the thing being referenced isn't the whole book.
           For book entries, the title field should be used instead.
    chapter
           Chapter number
    crossref
	   The database key of the entry being cross referenced.
    edition
           Edition of a book (e.g., \"second\")
    editor
           Name(s) of editor(s), in BibTeX name format.
           If there is also an author field, then the editor field should be
           for the book or collection that the work appears in
    howpublished
            How something strange has been published (begins sentence)
    institution
           Sponsoring institution
    journal
           Journal name (macros are provided for many)
    key
           Alphabetizing and labeling key (needed when no author or editor)
    month
           Month (macros are provided)
    note
           To help the reader find a reference (begins sentence)
    number
           Number of a journal or technical report
    organization
           Organization (sponsoring a conference)
    pages
           Page number or numbers (use `--' to separate a range)
    publisher
           Publisher name
    school
           School name (for theses)
    series
           The name of a series or set of books.
           An individual book will will also have it's own title
    title
           The title of the thing being referenced
    type
           Type of a technical report (e.g., \"Research Note\") to be used
           instead of the default \"Technical Report\"
    volume
           Volume of a journal or multivolume work
    year
           Year---should contain only numerals
---------------------------------------------------------
Entry to this mode calls the value of bibtex-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bibtex-mode-syntax-table)
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)
  (setq mode-name "BibTeX")
  (set-syntax-table bibtex-mode-syntax-table)
  (setq local-abbrev-table bibtex-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (setq paragraph-start "^[ \f\n\t]*$")
  (auto-fill-mode 1)			; nice alignments
  (setq left-margin (+ bibtex-text-alignment 1))

  (run-hooks 'bibtex-mode-hook))

(defun bibtex-move-outside-of-entry ()
  "Make sure we are outside of a bib entry"
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

(defun ispell-abstract ()
  (interactive)
  (beginning-of-bibtex-entry)
  (re-search-forward "^[ \t]*[OPT]*abstract[ \t]*=")
  (ispell-region (point)
		 (save-excursion (forward-sexp) (point))))

(defun beginning-of-bibtex-entry ()
  (interactive)
  (re-search-backward "^@" nil 'move))

(defun skip-whitespace-and-comments ()
  ;; It might be a good idea to have forward-sexp with argument 0 do what
  ;; this function tries to do, namely skip whitespace and comments.
  ;; Maybe a better name for this would be skip-to-next-sexp.
  ;; alternative implementation:
  ;;   (let ((parse-sexp-ignore-comments t))
  ;;     (forward-sexp 1)
  ;;     (forward-sexp -1))
  ;; but I've had problems with this not getting the parse of comments
  ;; right going backward if they contain unbalanced expressions or string
  ;; quotes. [alarson:19920217.1021CST]
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

;;; [alarson:19920214.1007CST]
(defun end-of-bibtex-entry ()
  "If inside an entry, move to the end of it, otherwise move to the end
of the next entry."
  (interactive)
  ;; if point was previously at the end of an entry, this puts us
  ;; inside the next entry, otherwise we remain in the current one.
  (progn
    (skip-whitespace-and-comments)
;;;     (skip-chars-forward " \t\n") 
    (end-of-line))
  (beginning-of-bibtex-entry)
  (let ((parse-sexp-ignore-comments t))
    (forward-sexp) ; skip entry type
    (forward-sexp) ; skip entry body
    ))
;(defun end-of-bibtex-entry ()
;  (interactive)
;  (re-search-forward "}$" nil 'move))
  
(defun ispell-bibtex-entry ()
  (interactive)
  (ispell-region (progn (beginning-of-bibtex-entry) (point))
		 (progn (end-of-bibtex-entry) (point))))

(defun narrow-to-bibtex-entry ()
  (interactive)
  (save-excursion
    (narrow-to-region (progn (beginning-of-bibtex-entry) (point))
		      (progn (end-of-bibtex-entry) (point)))))


(defun beginning-of-first-bibtex-entry ()
  (goto-char (point-min))
  (cond
   ((re-search-forward "^@" nil 'move)
    (beginning-of-line))
   ((and (bobp) (eobp))
    nil)
   (t
    (message "Warning: No bibtex entries found!"))))

(defun hide-bibtex-entry-bodies (&optional arg)
  "Hide all lines between first and last bibtex entries not beginning with @.
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

(defvar bibtex-sort-ignore-string-entries nil
  "*If true, bibtex @STRING entries are ignored when determining ordering
of the buffer (e.g. sorting, locating alphabetical position for new entries,
etc.)")

(defun sort-bibtex-entries ()
  "Sort bibtex entries alphabetically by key.
Text before the first bibtex entry, and following the last is not affected.
If bibtex-sort-ignore-string-entries is true, @string entries will be ignored.

Bugs:
  1. Text between the closing brace ending one bibtex entry, and the @ starting 
     the next, is considered part of the PRECEDING entry.  Perhaps it should be
     part of the following entry."
  (interactive)
  (save-restriction
    (beginning-of-first-bibtex-entry)
    (narrow-to-region (point)
		      (save-excursion
			(goto-char (point-max))
			(beginning-of-bibtex-entry)
			(end-of-bibtex-entry)
			(point)))
    (sort-subr nil			; reversep
	       ;; beginning of record function
	       'forward-line
	       ;; end of record function
	       (function (lambda () (and (re-search-forward "}\\s-*\n[\n \t]*@" nil 'move)
					 (forward-char -2))))
	       ;; start of key function
	       (if bibtex-sort-ignore-string-entries
		   (function (lambda ()
			       (while (and (re-search-forward "^\\s-*\\([@a-zA-Z]*\\)\\s-*{\\s-*")
					   (string-equalp "@string"
							  (buffer-substring (match-beginning 1)
									    (match-end 1)))))
			       nil))
		   (function (lambda () (re-search-forward "{\\s-*") nil)))
	       ;; end of key function
	       (function (lambda () (search-forward ",")))
	       )))
  
(defun map-bibtex-entries (fun)
  "Call FUN for each bibtex entry starting with the current, to the end of the file.
FUN is called with one argument, the key of the entry, and with point inside the entry.
If bibtex-sort-ignore-string-entries is true, FUN will not be called for @string entries."
  (beginning-of-bibtex-entry)
  (while (re-search-forward "^@[^{]*{[ \t]*\\([^, ]*\\)" nil t)
    (if (and bibtex-sort-ignore-string-entries
	     (string-equalp "@string{"
			    (buffer-substring (match-beginning 0)
					      (match-beginning 1))))
	nil ; ignore the @string entry.
      (funcall fun (buffer-substring (match-beginning 1) (match-end 1))))))
  
(defun find-bibtex-entry-location (entry-name)
  "Searches from beginning of current buffer looking for place to put the
bibtex entry named ENTRY-NAME.  Buffer is assumed to be in sorted order,
without duplicates (see \\[sort-bibtex-entries]), if it is not, an error will
be signalled."
  (interactive "sBibtex entry key: ")
  (let ((previous nil)
	point)
    (beginning-of-first-bibtex-entry)
    (or (catch 'done
	  (map-bibtex-entries (function (lambda (current)
				 (cond
				  ((string-equal entry-name current)
				   (error "Entry duplicates existing!"))
				  ((or (null previous)
				       (string< previous current))
				   (setq previous current
					 point (point))
				   (if (string< entry-name current)
				       (progn
					 (beginning-of-bibtex-entry)
					 ;; Many schemes append strings to
					 ;; existing entries to resolve them,
					 ;; so initial substring matches may
					 ;; indicate a duplicate entry.  
					 (let ((idx (string-match (regexp-quote entry-name) current)))
					   (if (and (integerp idx)
						    (zerop idx))
					       (progn
						 (message "Warning: Entry %s may be a duplicate of %s!"
							  entry-name current)
						 (ding t))))
					 (throw 'done t))))
				  ((string-equal previous current)
				   (error "Duplicate here with previous!"))
				  (t (error "Entries out of order here!")))))))
	(end-of-bibtex-entry))))

(defun validate-bibtex-buffer ()
  "Find some typical errors in bibtex files.
  1. At signs (@) not as first char of a line.
  2. Double quotes (\") inside strings.
  3. Closing braces (}) not the last character of a line."
  (interactive)
  (let ((point (point)))
    (while (re-search-forward ".@" nil t)
      (let* ((foo (parse-partial-sexp (save-excursion (beginning-of-bibtex-entry)
						      (point))
				      (point)))
	     (in-a-string (nth 3 foo)))
	(if (not in-a-string)
	    (error "At sign (@) out of place!"))))
    (goto-char point)
    (while (search-forward "\"" nil t)
      (or (looking-at "[,}][ \t]*$")
	  (char-equal (preceding-char) ?\")
	  ;; some versions put closing brace on separate line.
	  (looking-at "[ \t]*\n}")
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (point)
				(progn (beginning-of-line) (point)))
	      (looking-at "^[ \t]*[a-zA-Z]+[ \t]*=[ \t]*\"$")))
	  (error "Quote out of place, or missing \",\" or \"}\"!")))
    (goto-char point)
    ;; This is only approximate, should actually search for close braces,
    ;; then see if they are inside a string, or at the end of a line.
    ;; This just gets the typical case of whitespace after a closing brace.
    (while (search-forward "}[ \t]+$" nil t)
      (error "Brace not last char of line!"))
    (goto-char point)
    (message "Bibtex buffer appears o.k.")))

(defun find-bibtex-duplicates ()
  "Searches forward in current buffer looking for duplicate bibtex entries.
Buffer is assumed to be sorted, see \\[sort-bibtex-entries]"
  (interactive)
  (let ((point (point)))
    ;; errors if things are not right...
    (find-bibtex-entry-location (make-string 10 255))
    (goto-char point)
    (message "No duplicates found!")))


;;; assoc doesn't ignore case, so we need an assoc that does...
(defun assoc-string-equalp (thing alist)
  (or (assoc thing alist)
      (while (and alist
		  (not (string-equalp thing (car (car alist)))))
	(setq alist (cdr alist)))
      (car alist)))

(defvar bibtex-maintain-sorted-entries nil
  "*If true, bibtex-mode will attempt to maintain all bibtex entries in 
sorted order.  

Note that this is more a property of a file than a personal preference and
as such should normally be set via a file local variable entry.")

(defun bibtex-entry (entry-type &optional required optional)
  (interactive (let* ((completion-ignore-case t)
		      (e-t (completing-read "Entry Type: " bibtex-entry-field-alist
					    nil t)))
		 (list e-t)))
  (if (and (null required) (null optional))
      (let* ((e (assoc-string-equalp entry-type bibtex-entry-field-alist))
	     (r-n-o (elt e 1))
	     (c-ref (elt e 2)))
	(if (null e)
	  (error "Bibtex entry type %s not defined!"))
	(if (and bibtex-include-OPTcrossref c-ref)
	    (setq required (elt c-ref 0)
		  optional (elt c-ref 1))
	  (setq required (elt r-n-o 0)
		optional (elt r-n-o 1)))))
  (let ((key (if bibtex-maintain-sorted-entries
		 (read-string (format "%s key: " entry-type)))))
    (if key
	(find-bibtex-entry-location key))	
    (bibtex-move-outside-of-entry)
    (insert "@" entry-type "{")
    (if key
	(insert key))
    (save-excursion
      (mapcar 'bibtex-make-field required)
      (if bibtex-include-OPTcrossref
	  (bibtex-make-optional-field "crossref"))
      (if bibtex-include-OPTkey
	  (bibtex-make-optional-field "key"))
      (mapcar 'bibtex-make-optional-field optional)
      (mapcar 'bibtex-make-optional-field 
	      bibtex-mode-user-optional-fields)
      (if bibtex-include-OPTannote
	  (bibtex-make-optional-field "annote"))
      (insert "\n}\n\n"))
    (if key
	(bibtex-next-field t))
    (run-hooks 'bibtex-add-entry-hook)))

;; (defun bibtex-entry (entry-type required optional)
;;   (bibtex-move-outside-of-entry)
;;   (insert (concat "@" entry-type "{,\n\n}\n\n"))
;;   (previous-line 3)
;;   (insert (mapconcat 'bibtex-make-entry required ",\n"))
;;   (if required
;;       (if optional
;; 	  (insert ",\n")))
;;   (insert (mapconcat 'bibtex-make-OPT-entry optional ",\n"))
;;   (if bibtex-mode-user-optional-fields		;MON...
;;       (progn
;; 	(if optional
;; 	    (insert ",\n"))
;; 	(insert (mapconcat 'bibtex-make-OPT-entry
;; 			   bibtex-mode-user-optional-fields
;; 			   ",\n"))))		;MON
;;  (up-list -1)
;;  (forward-char 1))


(defun bibtex-make-field (e-t)
  (interactive "sBibTeX entry type: ")
  (let ((name  (if (consp e-t) (car e-t) e-t))
	(value (if (consp e-t) (cdr e-t) "")))
    (insert ",\n")
    (indent-to-column bibtex-name-alignment)
    (insert name " = ")
    (indent-to-column bibtex-text-alignment)
    ;; lucid emacs prin1-to-string breaks the undo chain.  When they fix
    ;; that, the  hack can be removed. [alarson:19930316.0805CST]
;    (insert (prin1-to-string value))
    ;; begin hack
    (insert (format (if (stringp value) "\"%s\"" "%s")
		    value))
    ;; end hack
    nil))

(defun bibtex-make-optional-field (e-t)
  (interactive "sOptional BibTeX entry type: ")
  (if (consp e-t)
      (setq e-t (cons (concat "OPT" (car e-t)) (cdr e-t)))
    (setq e-t (concat "OPT" e-t)))
  (bibtex-make-field e-t))

;; What to do about crossref?  if present, journal and year are 
;; both optional.  Due to this, i move all of them into optional. -- MON

(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article"))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book"))

(defun bibtex-Booklet ()
  (interactive)
  (bibtex-entry "Booklet"))

;(defun bibtex-DEAthesis ()
;  (interactive)
;  (bibtex-entry "DEAthesis"))

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
  (insert "@string{ = \"\"}\n")
  (previous-line 1)
  (forward-char 8))

(defun bibtex-preamble ()
  (interactive)
  (bibtex-move-outside-of-entry)
  (insert "@Preamble{}\n")
  (previous-line 1)
  (forward-char 10))

(defun bibtex-next-field (arg)
  "Finds end of text of next BibTeX field; with arg, to its beginning"
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

;; (defun bibtex-next-field ()
;;   "Finds end of text of next field."
;;   (interactive)
;;   (condition-case ()
;;       (progn
;; 	(bibtex-inside-field)
;; 	(re-search-forward ",[ \t\n]*" (point-max) 1)
;; 	(bibtex-enclosing-field)
;; 	(bibtex-inside-field))
;;     (error nil)))

(defun bibtex-find-text (arg)
  "Go to end of text of current field; with arg, go to beginning."
  (interactive "P")
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (if arg
      (progn
	(goto-char (match-beginning bibtex-text-in-field))
	(if (looking-at "\"")
	    (forward-char 1)))
    (goto-char (match-end bibtex-text-in-field))
    (if (= (preceding-char) ?\")
	(forward-char -1))))

;; (defun bibtex-find-text ()
;;   "Go to end of text of current field."
;;   (interactive)
;;   (condition-case ()
;;       (progn
;; 	(bibtex-inside-field)
;; 	(bibtex-enclosing-field)
;; 	(goto-char (match-end bibtex-text-in-field))
;; 	(bibtex-inside-field))
;;     (error nil)))

(defun bibtex-remove-OPT ()
  "Removes the 'OPT' starting optional arguments and goes to end of text"
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

(defun bibtex-inside-field ()
  "Try to avoid point being at end of a bibtex field."
  (interactive)
  (end-of-line)
  (skip-chars-backward " \t")		;MON - maybe delete these chars?
  (cond ((= (preceding-char) ?,)
	 (forward-char -2))) ; -1 --> -2 sct@dcs.edinburgh.ac.uk
  (cond ((= (preceding-char) ?\")
	 (forward-char -1))))		;MON - only go back if quote

(defun bibtex-remove-double-quotes ()
  "Removes \"\" around string."
  (interactive)
  (save-excursion
    (bibtex-inside-field)
    (bibtex-enclosing-field)
    (let ((start (match-beginning bibtex-text-in-field))
	  (stop (match-end  bibtex-text-in-field)))
      (goto-char stop)
      (forward-char -1)
      (if (looking-at "\"")
	  (delete-char 1))
      (goto-char start)
      (if (looking-at "\"")
	  (delete-char 1)))))

(defun bibtex-kill-optional-field ()
  "Kill the entire enclosing optional BibTeX field"
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
  "Delete the text part of the current field, replace with empty text"
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (goto-char (match-beginning bibtex-text-in-field))
  (kill-region (point) (match-end bibtex-text-in-field))
  (insert "\"\"")
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
	       (setq bibtex-pop-previous-search-point (match-beginning 0))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-previous-search-point)
	
	; Now search for arg'th previous similar field
	(cond
	 ((re-search-backward matching-entry (point-min) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
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
	       (setq bibtex-pop-previous-search-point (match-beginning 0))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-next-search-point)
	
	; Now search for arg'th next similar field
	(cond
	 ((re-search-forward matching-entry (point-max) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
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

(defun bibtex-flash-head ()
  "Flash at BibTeX reference head before point, if exists.  (Moves point)."
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



(defun bibtex-enclosing-field ()
  "Search for BibTeX field enclosing point.
Point moves to end of field; also, use match-beginning and match-end
to parse the field."
  ;; sct@dcs.edinburgh.ac.uk
  (let ((old-point (point)))
    (condition-case errname
 	(bibtex-enclosing-regexp bibtex-field)
      (search-failed
       (goto-char old-point)
       (error "Can't find enclosing BibTeX field.")))))

(defun bibtex-enclosing-reference ()
  "Search for BibTeX reference enclosing point.
Point moves to end of reference; also, use match-beginning and match-end
to parse the reference."
  ;; sct@dcs.edinburgh.ac.uk
  (let ((old-point (point)))
    (condition-case errname
 	(bibtex-enclosing-regexp bibtex-reference)
      (search-failed
       (goto-char old-point)
       (error "Can't find enclosing BibTeX reference.")))))

(defun bibtex-enclosing-regexp (regexp)
  "Search for REGEXP enclosing point.
Point moves to end of REGEXP.  See also match-beginning and match-end.
If an enclosing REGEXP is not found, signals search-failed; point is left in
an undefined location.

[Doesn't something like this exist already?]"
  
  (interactive "sRegexp: ")
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

(defun bibtex-clean-entry ()
  "For all optional fields of current BibTeX entry: if empty, kill the whole field; otherwise, remove the \"OPT\" string in the name; if text numerical, remove double-quotes.  For all mandatory fields: if empty, signal error."
  (interactive)
  (beginning-of-bibtex-entry)
  (let ((start (point)))
    (save-restriction
      (narrow-to-region start (save-excursion (end-of-bibtex-entry) (point)))
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
		 (if (looking-at "\"\"") ; empty: delete whole field
		     (delete-region begin-field end-field)
		   ; otherwise: not empty, delete "OPT"
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
		 (cond ((looking-at "\"[0-9]+\"") ; if numerical,
			(goto-char end-text)
			(delete-char -1) ; delete enclosing double-quotes
			(goto-char begin-text)
			(delete-char 1)
			(goto-char end-field) ; go to end for next search
			(forward-char -2) ; to compensate for the 2 quotes deleted
			)
		       ((looking-at "\"\"") ; if empty quotes, complain
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
			(goto-char end-field))))))))
    (goto-char start)
    (end-of-bibtex-entry)
    ;; sct@dcs.edinburgh.ac.uk
    (save-excursion
      (backward-char 1)
      (skip-syntax-backward " >")
      (if (eq (preceding-char) ?,)
 	  (backward-delete-char 1)))
    (skip-whitespace-and-comments)))


;;; Menus for bibtex mode

(define-key bibtex-mode-map [menu-bar entry-types]
  (cons "Entry Types" (make-sparse-keymap "Entry Types")))

(define-key bibtex-mode-map [menu-bar entry-types bibtex-InProceedings]
  '(" article in conference Proceedings " . bibtex-InProceedings))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Article]
  '("        Article in journal         " . bibtex-Article))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Book]
  '("               Book                " . bibtex-Book))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Booklet]
  '("             Booklet               " . bibtex-Booklet))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-InProceedings]
  '("            Conference             " . bibtex-InProceedings))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-MastersThesis]
  '("         Master's Thesis           " . bibtex-MastersThesis))
;define-key bibtex-mode-map [menu-bar entry-types bibtex-DEAthesis]
;'(("            DEA Thesis             " . bibtex-DEAthesis))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-PhdThesis]
  '("            Phd. Thesis            " . bibtex-PhdThesis))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-TechReport]
  '("         Technical Report          " . bibtex-TechReport))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Manual]
  '("         technical Manual          " . bibtex-Manual))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Proceedings]
  '("      conference Proceedings       " . bibtex-Proceedings))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-InBook]
  '("        a chapter in a Book        " . bibtex-InBook))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-InCollection]
  '("    an article in a Collection     " . bibtex-InCollection))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Misc]
  '("           miscellaneous           " . bibtex-Misc))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-Unpublished]
  '("            unpublished            " . bibtex-Unpublished))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-string]
  '("              string               " . bibtex-string))
(define-key bibtex-mode-map [menu-bar entry-types bibtex-preamble]
  '("             preamble              " . bibtex-preamble))

(define-key bibtex-mode-map [menu-bar move/edit]
  (cons "Bibtex Edit" (make-sparse-keymap "Bibtex Edit")))

(define-key bibtex-mode-map [menu-bar move/edit bibtex-next-field]
  '("            next field             " . bibtex-next-field))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-find-text]
  '("          to end of field          " . bibtex-find-text))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-pop-previous]
  '("snatch from similar preceding field" . bibtex-pop-previous))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-pop-next]
  '("snatch from similar following field" . bibtex-pop-next))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-remove-OPT]
  '("            remove OPT             " . bibtex-remove-OPT))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-remove-double-quotes]
  '("           remove quotes           " . bibtex-remove-double-quotes))
(define-key bibtex-mode-map [menu-bar move/edit bibtex-clean-entry]
  '("          clean up entry           " . bibtex-clean-entry))
(define-key bibtex-mode-map [menu-bar move/edit find-bibtex-duplicates]
  '("          find duplicates          " . find-bibtex-duplicates))
(define-key bibtex-mode-map [menu-bar move/edit sort-bibtex-entries]
  '("           sort entries            " . sort-bibtex-entries))
(define-key bibtex-mode-map [menu-bar move/edit validate-bibtex-buffer]
  '("         validate entries          " . validate-bibtex-buffer))


;; Please don't send anything to bug-gnu-emacs about these Sunwindows functions
;; since we aren't interested.  See etc/SUN-SUPPORT for the reasons why
;; we consider this nothing but a distraction from our work.

;(defmacro eval-in-menu-window (&rest l)
;  "Evaluates its argument in the window in which the mouse button was pressed."
;  (list 'eval-in-window '*menu-window* l))

;(defmenu bibtex-sun-entry-menu 
;  ("Article In Conf. Proc." eval-in-menu-window bibtex-InProceedings)
;  ("Article In Journal" eval-in-menu-window bibtex-Article)
;  ("Book" eval-in-menu-window bibtex-Book)
;  ("Booklet" eval-in-menu-window bibtex-Booklet)
;  ("Master's Thesis" eval-in-menu-window bibtex-MastersThesis)
;  ("PhD. Thesis" eval-in-menu-window bibtex-PhdThesis)
;  ("Technical Report" eval-in-menu-window bibtex-TechReport)
;  ("Technical Manual" eval-in-menu-window bibtex-Manual)
;  ("Conference Proceedings" eval-in-menu-window bibtex-Proceedings)
;  ("In A Book" eval-in-menu-window bibtex-InBook)
;  ("In A Collection" eval-in-menu-window bibtex-InCollection)
;  ("Miscellaneous" eval-in-menu-window bibtex-Misc)
;  ("Unpublished" eval-in-menu-window bibtex-Unpublished)
;  ("string" eval-in-menu-window bibtex-string)
;  ("preamble" eval-in-menu-window bibtex-preamble))
;
;(defmenu bibtex-sun-menu
;  ("BibTeX menu")
;  ("add entry" . bibtex-sun-entry-menu)
;  ("next field" eval-in-menu-window bibtex-next-field nil)
;  ("to end of field" eval-in-menu-window bibtex-find-text nil)
;  ("snatch similar preceding field" eval-in-menu-window bibtex-pop-previous 1)
;  ("snatch similar following field" eval-in-menu-window bibtex-pop-next 1)
;  ("remove OPT" eval-in-menu-window bibtex-remove-OPT)
;  ("remove quotes" eval-in-menu-window bibtex-remove-double-quotes)
;  ("clean entry" eval-in-menu-window bibtex-clean-entry)
;  ("describe BibTeX mode" eval-in-menu-window describe-mode)
;  ("Main Emacs menu" . emacs-menu))
 
;(defun bibtex-sun-menu-eval (window x y)
;  "Pop-up menu of BibTeX commands."
;  (sun-menu-evaluate window (1+ x) (1- y) 'bibtex-sun-menu))
;
;(defun bibtex-sun-environment ()
;  "Set up sun menus for BibTeX mode.  Call it as bibtex-mode-hook, or
;interactively"
;  (interactive)
;  (local-set-mouse  '(text right) 'bibtex-sun-menu-eval))
;

;;; bibtex.el ends here
