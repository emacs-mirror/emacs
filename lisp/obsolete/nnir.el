;;; nnir.el --- Search mail with various search engines  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

;; Author: Kai Großjohann <grossjohann@ls6.cs.uni-dortmund.de>
;; Swish-e and Swish++ backends by:
;;   Christoph Conrad <christoph.conrad@gmx.de>.
;; IMAP backend by: Simon Josefsson <jas@pdc.kth.se>.
;; IMAP search by: Torsten Hilbrich <torsten.hilbrich <at> gmx.net>
;; IMAP search improved by Daniel Pittman  <daniel@rimspace.net>.
;; nnmaildir support for Swish++ and Namazu backends by:
;;   Justus Piater <Justus <at> Piater.name>
;; Mostly rewritten by Andrew Cohen <cohen@bu.edu> from 2010
;; Keywords: news mail searching ir
;; Obsolete-since: 28.1

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

;; What does it do?  Well, it searches your mail using some search
;; engine (imap, namazu, swish-e, gmane and others -- see later).

;; The Lisp setup may involve setting a few variables and setting up the
;; search engine.  You can define the variables in the server definition
;; like this :
;;   (setq gnus-secondary-select-methods '(
;;       (nnimap "" (nnimap-address "localhost")
;;                  (nnir-search-engine namazu)
;;       )))
;; The main variable to set is `nnir-search-engine'.  Choose one of
;; the engines listed in `nnir-engines'.  (Actually `nnir-engines' is
;; an alist, type `C-h v nnir-engines RET' for more information; this
;; includes examples for setting `nnir-search-engine', too.)

;; The entry to searching is the single function `nnir-run-query',
;; which dispatches the search to the proper search function.  The
;; argument of `nnir-run-query' is an alist with two keys:
;; 'nnir-query-spec and 'nnir-group-spec.  The value for
;; 'nnir-query-spec is an alist.  The only required key/value pair is
;; (query . "query") specifying the search string to pass to the query
;; engine.  Individual engines may have other elements.  The value of
;; 'nnir-group-spec is a list with the specification of the
;; groups/servers to search.  The format of the 'nnir-group-spec is
;; (("server1" ("group11" "group12")) ("server2" ("group21"
;; "group22"))).  If any of the group lists is absent then all groups
;; on that server are searched.

;; The output of `nnir-run-query' is a vector, each element of which
;; should in turn be a three-element vector with the form: [fully
;; prefixed group-name of the article; the article number; the
;; Retrieval Status Value (RSV)] as returned from the search engine.
;; An RSV is the score assigned to the document by the search engine.
;; For Boolean search engines, the RSV is always 1000 (or 1 or 100, or
;; whatever you like).

;; A vector of this form is used by the nnselect backend to create
;; virtual groups.  So nnir-run-query is a suitable function to use in
;; nnselect groups.

;; The default sorting order of articles in an nnselect summary buffer
;; is based on the order of the articles in the above mentioned
;; vector, so that's where you can do the sorting you'd like.  Maybe
;; it would be nice to have a way of displaying the search result
;; sorted differently?

;; So what do you need to do when you want to add another search
;; engine?  You write a function that executes the query.  Temporary
;; data from the search engine can be put in `nnir-tmp-buffer'.  This
;; function should return the list of articles as a vector, as
;; described above.  Then, you need to register this backend in
;; `nnir-engines'.  Then, users can choose the backend by setting
;; `nnir-search-engine' as a server variable.

;; If you use one of the local indices (namazu, find-grep, swish) you
;; must also set up a search engine backend.

;; 1. Namazu
;;
;; The Namazu backend requires you to have one directory containing all
;; index files, this is controlled by the `nnir-namazu-index-directory'
;; variable.  To function the `nnir-namazu-remove-prefix' variable must
;; also be correct, see the documentation for `nnir-namazu-remove-prefix'
;; above.
;;
;; It is particularly important not to pass any switches to namazu
;; that will change the output format.  Good switches to use include
;; `--sort', `--ascending', `--early' and `--late'.  Refer to the Namazu
;; documentation for further information on valid switches.
;;
;; To index my mail with the `mknmz' program I use the following
;; configuration file:
;;
;; ,----
;; | package conf;  # Don't remove this line!
;; |
;; | # Paths which will not be indexed.  Don't use `^' or `$' anchors.
;; | $EXCLUDE_PATH = "spam|sent";
;; |
;; | # Header fields which should be searchable.  case-insensitive
;; | $REMAIN_HEADER = "from|date|message-id|subject";
;; |
;; | # Searchable fields.  case-insensitive
;; | $SEARCH_FIELD = "from|date|message-id|subject";
;; |
;; | # The max length of a word.
;; | $WORD_LENG_MAX = 128;
;; |
;; | # The max length of a field.
;; | $MAX_FIELD_LENGTH = 256;
;; `----
;;
;; My mail is stored in the directories ~/Mail/mail/, ~/Mail/lists/ and
;; ~/Mail/archive/, so to index them I go to the directory set in
;; `nnir-namazu-index-directory' and issue the following command.
;;
;;      mknmz --mailnews ~/Mail/archive/ ~/Mail/mail/ ~/Mail/lists/
;;
;; For maximum searching efficiency I have a cron job set to run this
;; command every four hours.

;; 2. find-grep
;;
;; The find-grep engine simply runs find(1) to locate eligible
;; articles and searches them with grep(1).  This, of course, is much
;; slower than using a proper search engine but OTOH doesn't require
;; maintenance of an index and is still faster than using any built-in
;; means for searching.  The method specification of the server to
;; search must include a directory for this engine to work (E.g.,
;; `nnml-directory').  The tools must be POSIX compliant.  GNU Find
;; prior to version 4.2.12 (4.2.26 on Linux due to incorrect ARG_MAX
;; handling) does not work.
;; ,----
;; |    ;; find-grep configuration for searching the Gnus Cache
;; |
;; |	(nnml "cache"
;; |          (nnml-get-new-mail nil)
;; |          (nnir-search-engine find-grep)
;; |          (nnml-directory "~/News/cache/")
;; |          (nnml-active-file "~/News/cache/active"))
;; `----


;;; Code:

;;; Setup:

(eval-when-compile (require 'cl-lib))
(require 'gnus)

;;; Internal Variables:

(defvar gnus-inhibit-demon)

(defvar nnir-search-history ()
  "Internal: the history for querying search options in nnir.")

(defconst nnir-tmp-buffer " *nnir*"
  "Internal: temporary buffer.")


;; Imap variables

(defvar nnir-imap-search-arguments
  '(("whole message" . "TEXT")
    ("subject" . "SUBJECT")
    ("to" . "TO")
    ("from" . "FROM")
    ("body" . "BODY")
    ("imap" . "")
    ("gmail" . "X-GM-RAW"))
  "Mapping from user readable keys to IMAP search items for use in nnir.")

(defvar nnir-imap-search-other "HEADER %S"
  "The IMAP search item for anything other than `nnir-imap-search-arguments'.
By default this is the name of an email header field.")

(defvar nnir-imap-search-argument-history ()
  "The history for querying search options in nnir.")

;;; Helper macros

(defmacro nnir-artitem-group (artitem)
  "Return the group from the ARTITEM."
  `(elt ,artitem 0))

(defmacro nnir-artitem-number (artitem)
  "Return the number from the ARTITEM."
  `(elt ,artitem 1))

(defmacro nnir-artitem-rsv (artitem)
  "Return the Retrieval Status Value (RSV, score) from the ARTITEM."
  `(elt ,artitem 2))


;;; User Customizable Variables:

(defgroup nnir nil
  "Search groups in Gnus with assorted search engines."
  :group 'gnus)

(make-obsolete-variable 'nnir-summary-line-format "The formatting
specs previously unique to this variable may now be set in
'gnus-summary-line-format." "28.1")

(defcustom nnir-summary-line-format nil
  "The format specification of the lines in an nnir summary buffer.

All the items from `gnus-summary-line-format' are available, along
with three items unique to nnir summary buffers:

%Z    Search retrieval score value (integer)
%G    Article original full group name (string)
%g    Article original short group name (string)

If nil this will use `gnus-summary-line-format'."
  :version "24.1"
  :type '(choice (const :tag "gnus-summary-line-format" nil) string))


(defcustom nnir-ignored-newsgroups ""
  "Newsgroups to skip when searching.
Any newsgroup in the active file matching this regexp will be
skipped when searching."
  :version "24.1"
  :type '(regexp))

(defcustom nnir-imap-default-search-key "whole message"
  "The default IMAP search key for an nnir search.
Must be one of the keys in `nnir-imap-search-arguments'.  To use
raw imap queries by default set this to \"imap\"."
  :version "24.1"
  :type `(choice ,@(mapcar (lambda (elem) (list 'const (car elem)))
			   nnir-imap-search-arguments)))

(defcustom nnir-swish++-configuration-file
  (expand-file-name "~/Mail/swish++.conf")
  "Configuration file for swish++."
  :type '(file))

(defcustom nnir-swish++-program "search"
  "Name of swish++ search executable."
  :type '(string))

(defcustom nnir-swish++-additional-switches '()
  "A list of strings, to be given as additional arguments to swish++.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-swish++-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish++-additional-switches \\='(\"-i\" \"-w\"))"
  :type '(repeat (string)))

(defcustom nnir-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from swish++ file names to get group names.
Resulting names have '/' in place of '.'.  This is a regular
expression.

This variable is very similar to `nnir-namazu-remove-prefix', except
that it is for swish++, not Namazu."
  :type '(regexp))

;; Swish-E.
;; URL: http://swish-e.org/ [dead link?]
;; Variables `nnir-swish-e-index-files', `nnir-swish-e-program' and
;; `nnir-swish-e-additional-switches'

(defcustom nnir-swish-e-index-files
  (list (expand-file-name "~/Mail/index.swish-e"))
  "List of index files for swish-e.
This could be a server parameter."
  :type '(repeat (file)))

(defcustom nnir-swish-e-program "swish-e"
  "Name of swish-e search executable.
This cannot be a server parameter."
  :type '(string))

(defcustom nnir-swish-e-additional-switches '()
  "A list of strings, to be given as additional arguments to swish-e.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-swish-e-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish-e-additional-switches \\='(\"-i\" \"-w\"))

This could be a server parameter."
  :type '(repeat (string)))

(defcustom nnir-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from swish-e file names to get group names.
Resulting names have '/' in place of '.'.  This is a regular
expression.

This variable is very similar to `nnir-namazu-remove-prefix', except
that it is for swish-e, not Namazu.

This could be a server parameter."
  :type '(regexp))

;; HyREX engine, see <URL:http://ls6-www.cs.uni-dortmund.de/> [dead link?]

(defcustom nnir-hyrex-program "nnir-search"
  "Name of the nnir-search executable."
  :type '(string))

(defcustom nnir-hyrex-additional-switches '()
  "A list of strings, to be given as additional arguments for nnir-search.
Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-hyrex-additional-switches \"-ddl ddl.xml -c nnir\") ; wrong !
Instead, use this:
    (setq nnir-hyrex-additional-switches \\='(\"-ddl\" \"ddl.xml\" \"-c\" \"nnir\"))"
  :type '(repeat (string)))

(defcustom nnir-hyrex-index-directory (getenv "HOME")
  "Index directory for HyREX."
  :type '(directory))

(defcustom nnir-hyrex-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from HyREX file names to get group names.
Resulting names have '/' in place of '.'.

For example, suppose that HyREX returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-hyrex-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory))

;; Namazu engine, see <URL:http://www.namazu.org/>

(defcustom nnir-namazu-program "namazu"
  "Name of Namazu search executable."
  :type '(string))

(defcustom nnir-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "Index directory for Namazu."
  :type '(directory))

(defcustom nnir-namazu-additional-switches '()
  "A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other
switches make any sense in this context.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-namazu-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-namazu-additional-switches \\='(\"-i\" \"-w\"))"
  :type '(repeat (string)))

(defcustom nnir-namazu-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from Namazu file names to get group names.
Resulting names have '/' in place of '.'.

For example, suppose that Namazu returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-namazu-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory))

(defcustom nnir-notmuch-program "notmuch"
  "Name of notmuch search executable."
  :version "24.1"
  :type '(string))

(defcustom nnir-notmuch-additional-switches '()
  "A list of strings, to be given as additional arguments to notmuch.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-notmuch-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-notmuch-additional-switches \\='(\"-i\" \"-w\"))"
  :version "24.1"
  :type '(repeat (string)))

(defcustom nnir-notmuch-remove-prefix
  (regexp-quote (or (getenv "MAILDIR") (expand-file-name "~/Mail")))
  "The prefix to remove from notmuch file names to get group names.
Resulting names have '/' in place of '.'.  This is a regular
expression.

This variable is very similar to `nnir-namazu-remove-prefix', except
that it is for notmuch, not Namazu."
  :version "27.1"
  :type '(regexp))

(defcustom nnir-notmuch-filter-group-names-function nil
  "Whether and how to use Gnus group names as \"path:\" search terms.
When nil, the groups being searched in are not used as notmuch
:path search terms.  It's still possible to use \"path:\" terms
manually within the search query, however.

When a function, map this function over all the group names.  To
use the group names unchanged, set to (lambda (g) g).  Multiple
transforms (for instance, converting \".\" to \"/\") can be added
like so:

\(add-function :filter-return
   nnir-notmuch-filter-group-names-function
   (lambda (g) (replace-regexp-in-string \"\\\\.\" \"/\" g)))"
  :version "27.1"
  :type '(choice function
		 (const :tag "No" nil)))

;;; Developer Extension Variable:

(defvar nnir-engines
  `((imap    nnir-run-imap
             ((criteria
	       "Imap Search in"                   ; Prompt
	       ,(mapcar #'car nnir-imap-search-arguments) ; alist for completing
	       nil                                ; allow any user input
	       nil                                ; initial value
	       nnir-imap-search-argument-history  ; the history to use
	       ,nnir-imap-default-search-key      ; default
	       )))
    (swish++ nnir-run-swish++
             ((swish++-group . "Swish++ Group spec (regexp): ")))
    (swish-e nnir-run-swish-e
             ((swish-e-group . "Swish-e Group spec (regexp): ")))
    (namazu  nnir-run-namazu
             ())
    (notmuch nnir-run-notmuch
             ())
    (hyrex   nnir-run-hyrex
	     ((hyrex-group . "Hyrex Group spec (regexp): ")))
    (find-grep nnir-run-find-grep
	       ((grep-options . "Grep options: "))))
  "Alist of supported search engines.
Each element in the alist is a three-element list (ENGINE FUNCTION ARGS).
ENGINE is a symbol designating the searching engine.  FUNCTION is also
a symbol, giving the function that does the search.  The third element
ARGS is a list of cons pairs (PARAM . PROMPT).  When issuing a query,
the FUNCTION will issue a query for each of the PARAMs, using PROMPT.

The value of `nnir-search-engine' must be one of the ENGINE symbols.
For example, for searching a server using namazu include
    (nnir-search-engine namazu)
in the server definition.  Note that you have to set additional
variables for most backends.  For example, the `namazu' backend
needs the variables `nnir-namazu-program',
`nnir-namazu-index-directory' and `nnir-namazu-remove-prefix'.

Add an entry here when adding a new search engine.")

(defcustom nnir-method-default-engines  '((nnimap . imap))
  "Alist of default search engines keyed by server method."
  :version "27.1"
  :type `(repeat (cons (choice (const nnimap) (const nntp) (const nnspool)
			       (const nneething) (const nndir) (const nnmbox)
			       (const nnml) (const nnmh) (const nndraft)
			       (const nnfolder) (const nnmaildir))
		       (choice
			,@(mapcar (lambda (elem) (list 'const (car elem)))
				  nnir-engines)))))


(defmacro nnir-add-result (dirnam artno score prefix server artlist)
  "Construct a result vector and add it to ARTLIST.
DIRNAM, ARTNO, SCORE, PREFIX and SERVER are passed to
`nnir-compose-result' to make the vector.  Only add the result if
non-nil."
  `(let ((result (nnir-compose-result ,dirnam ,artno ,score ,prefix ,server)))
     (when (not (null result))
       (push result ,artlist))))

(autoload 'nnmaildir-base-name-to-article-number "nnmaildir")

;; Helper function currently used by the Swish++ and Namazu backends;
;; perhaps useful for other backends as well
(defun nnir-compose-result (dirnam article score prefix server)
  "Construct a result vector.
The DIRNAM, ARTICLE, SCORE, PREFIX, and SERVER are used to
construct the vector entries."
  ;; remove nnir-*-remove-prefix from beginning of dirnam filename
  (when (string-match (concat "^" prefix) dirnam)
    (setq dirnam (replace-match "" t t dirnam)))

  (when (file-readable-p (concat prefix dirnam article))
    ;; remove trailing slash and, for nnmaildir, cur/new/tmp
    (setq dirnam
	  (substring dirnam 0
		     (if (string-match "\\`nnmaildir:" (gnus-group-server server))
			 -5 -1)))

    ;; Set group to dirnam without any leading dots or slashes,
    ;; and with all subsequent slashes replaced by dots
    (let ((group (replace-regexp-in-string
		  "[/\\]" "."
		  (replace-regexp-in-string "^[./\\]" "" dirnam nil t)
		  nil t)))

    (vector (gnus-group-full-name group server)
	    (if (string-match "\\`nnmaildir:" (gnus-group-server server))
		(nnmaildir-base-name-to-article-number
		 (substring article 0 (string-search ":" article))
		 group nil)
	      (string-to-number article))
	    (string-to-number score)))))

;;; Search Engine Interfaces:

(autoload 'gnus-server-get-active "gnus-int")
(autoload 'nnimap-change-group "nnimap")
(declare-function nnimap-buffer "nnimap" ())
(declare-function nnimap-command "nnimap" (&rest args))

;; imap interface
(defun nnir-run-imap (query srv &optional groups)
  "Run the QUERY search against an IMAP back-end server SRV.
Search GROUPS, or all active groups on SRV if GROUPS is nil.
This uses a custom query language parser; see
`nnir-imap-make-query' for details on the language and supported
extensions."
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
          (server (cadr (gnus-server-to-method srv)))
          (criteria (or (cdr (assq 'criteria query))
                        (cdr (assoc nnir-imap-default-search-key
                                    nnir-imap-search-arguments))))
          (gnus-inhibit-demon t)
	  (groups
	   (or groups (gnus-server-get-active srv nnir-ignored-newsgroups))))
      (message "Opening server %s" server)
      (apply
       #'vconcat
       (catch 'found
         (mapcar
          #'(lambda (group)
	      (let (artlist)
		(condition-case ()
		    (when (nnimap-change-group
			   (gnus-group-short-name group) server)
		      (with-current-buffer (nnimap-buffer)
			(message "Searching %s..." group)
			(let ((arts 0)
			      (result (nnimap-command "UID SEARCH %s"
						      (if (string= criteria "")
							  qstring
							(nnir-imap-make-query
							 criteria qstring)))))
			  (mapc
			   (lambda (artnum)
			     (let ((artn (string-to-number artnum)))
			       (when (> artn 0)
				 (push (vector group artn 100)
				       artlist)
				 (when (assq 'shortcut query)
				   (throw 'found (list artlist)))
				 (setq arts (1+ arts)))))
			   (and (car result)
				(cdr (assoc "SEARCH" (cdr result)))))
			  (message "Searching %s... %d matches" group arts)))
		      (message "Searching %s...done" group))
		  (quit nil))
		(nreverse artlist)))
          groups))))))

(defun nnir-imap-make-query (criteria qstring)
  "Make an IMAP search expression from QSTRING and CRITERIA.

This implements a little language designed to return the expected
results to an arbitrary query string to the end user.

The search is always case-insensitive, as defined by RFC2060, and
supports the following features (inspired by the Google search input
language):

Automatic \"and\" queries
    If you specify multiple words then they will be treated as an
    \"and\" expression intended to match all components.

Phrase searches
    If you wrap your query in double-quotes then it will be treated
    as a literal string.

Negative terms
    If you precede a term with \"-\" then it will negate that.

\"OR\" queries
    If you include an upper-case \"OR\" in your search it will cause
    the term before it and the term after it to be treated as
    alternatives.

In the future the following will be added to the language:
 * support for date matches
 * support for location of text matching within the query
 * from/to/etc headers
 * additional search terms
 * flag based searching
 * anything else that the RFC supports, basically."
  ;; Walk through the query and turn it into an IMAP query string.
  (nnir-imap-query-to-imap criteria (nnir-imap-parse-query qstring)))


(defun nnir-imap-query-to-imap (criteria query)
  "Turn an s-expression format QUERY with CRITERIA into IMAP."
  (mapconcat
   ;; Turn the expressions into IMAP text
   (lambda (item)
     (nnir-imap-expr-to-imap criteria item))
   ;; The query, already in s-expr format.
   query
   ;; Append a space between each expression
   " "))


(defun nnir-imap-expr-to-imap (criteria expr)
  "Convert EXPR into an IMAP search expression on CRITERIA."
  ;; What sort of expression is this, eh?
  (cond
   ;; Simple string term
   ((stringp expr)
    (format "%s %S" criteria expr))
   ;; Trivial term: and
   ((eq expr 'and) nil)
   ;; Composite term: or expression
   ((eq (car-safe expr) 'or)
    (format "OR %s %s"
	    (nnir-imap-expr-to-imap criteria (nth 1 expr))
	    (nnir-imap-expr-to-imap criteria (nth 2 expr))))
   ;; Composite term: just the fax, mam
   ((eq (car-safe expr) 'not)
    (format "NOT (%s)" (nnir-imap-query-to-imap criteria (cdr expr))))
   ;; Composite term: just expand it all.
   ((consp expr)
    (format "(%s)" (nnir-imap-query-to-imap criteria expr)))
   ;; Complex value, give up for now.
   (t (error "Unhandled input: %S" expr))))


(defun nnir-imap-parse-query (string)
  "Turn STRING into an s-expression query.
STRING is based on the IMAP query language as defined in
`nnir-imap-make-query'.

This involves turning individual tokens into higher level terms
that the search language can then understand and use."
  (with-temp-buffer
    ;; Set up the parsing environment.
    (insert string)
    (goto-char (point-min))
    ;; Now, collect the output terms and return them.
    (let (out)
      (while (not (nnir-imap-end-of-input))
	(push (nnir-imap-next-expr) out))
      (reverse out))))


(defun nnir-imap-next-expr (&optional count)
  "Return the next (COUNT) expression from the current buffer."
  (let ((term (nnir-imap-next-term count))
	(next (nnir-imap-peek-symbol)))
    ;; Are we looking at an 'or' expression?
    (cond
     ;; Handle 'expr or expr'
     ((eq next 'or)
      (list 'or term (nnir-imap-next-expr 2)))
     ;; Anything else
     (t term))))


(defun nnir-imap-next-term (&optional count)
  "Return the next (COUNT) term from the current buffer."
  (let ((term (nnir-imap-next-symbol count)))
    ;; What sort of term is this?
    (cond
     ;; and -- just ignore it
     ((eq term 'and) 'and)
     ;; negated term
     ((eq term 'not) (list 'not (nnir-imap-next-expr)))
     ;; generic term
     (t term))))


(defun nnir-imap-peek-symbol ()
  "Return the next symbol from the current buffer, but don't consume it."
  (save-excursion
    (nnir-imap-next-symbol)))

(defun nnir-imap-next-symbol (&optional count)
  "Return the next (COUNT) symbol from the current buffer.
Return nil if we are at the end of the buffer.  If supplied COUNT
skips some symbols before returning the one at the supplied
position."
  (when (and (numberp count) (> count 1))
    (nnir-imap-next-symbol (1- count)))
  (let ((case-fold-search t))
    ;; end of input stream?
    (unless (nnir-imap-end-of-input)
      ;; No, return the next symbol from the stream.
      (cond
       ;; negated expression -- return it and advance one char.
       ((looking-at "-") (forward-char 1) 'not)
       ;; quoted string
       ((looking-at "\"") (nnir-imap-delimited-string "\""))
       ;; list expression -- we parse the content and return this as a list.
       ((looking-at "(")
	(nnir-imap-parse-query (nnir-imap-delimited-string ")")))
       ;; keyword input -- return a symbol version
       ((looking-at "\\band\\b") (forward-char 3) 'and)
       ((looking-at "\\bor\\b")  (forward-char 2) 'or)
       ((looking-at "\\bnot\\b") (forward-char 3) 'not)
       ;; Simple, boring keyword
       (t (let ((start (point))
		(end (if (search-forward-regexp "[[:blank:]]" nil t)
			 (prog1
			     (match-beginning 0)
			   ;; unskip if we hit a non-blank terminal character.
			   (when (string-match "[^[:blank:]]" (match-string 0))
			     (backward-char 1)))
		       (goto-char (point-max)))))
	    (buffer-substring start end)))))))

(defun nnir-imap-delimited-string (delimiter)
  "Return a string delimited by DELIMITER from the current buffer."
  (let ((start (point)) end)
    (forward-char 1)			; skip the first delimiter.
    (while (not end)
      (unless (search-forward delimiter nil t)
	(error "Unmatched delimited input with %s in query" delimiter))
      (let ((here (point)))
	(unless (equal (buffer-substring (- here 2) (- here 1)) "\\")
	  (setq end (point)))))
    (buffer-substring (1+ start) (1- end))))

(defun nnir-imap-end-of-input ()
  "Are we at the end of input?"
  (skip-chars-forward "[:blank:]")
  (looking-at "$"))


;; Swish++ interface.
;; -cc- Todo
;; Search by
;; - group
;; Sort by
;; - rank (default)
;; - article number
;; - file size
;; - group
(defun nnir-run-swish++ (query server &optional _group)
  "Run QUERY on SERVER against swish++.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish++ 4.7 on GNU/Linux and with swish++ 5.0b2 on
Windows NT 4.0."

  ;; (when group
  ;;   (error "The swish++ backend cannot search specific groups"))

  (save-excursion
    (let ( (qstring (cdr (assq 'query query)))
	   (groupspec (cdr (assq 'swish++-group query)))
	   (prefix (nnir-read-server-parm 'nnir-swish++-remove-prefix server))
           artlist
	   ;; nnml-use-compressed-files might be any string, but probably this
	   ;; is sufficient.  Note that we can't only use the value of
	   ;; nnml-use-compressed-files because old articles might have been
	   ;; saved with a different value.
	   (article-pattern (if (string-match "\\`nnmaildir:"
					      (gnus-group-server server))
				":[0-9]+"
			      "^[0-9]+\\(\\.[a-z0-9]+\\)?$"))
           score artno dirnam filenam)

      (when (equal "" qstring)
        (error "swish++: You didn't enter anything"))

      (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (if groupspec
          (message "Doing swish++ query %s on %s..." qstring groupspec)
        (message "Doing swish++ query %s..." qstring))

      (let* ((cp-list `( ,nnir-swish++-program
                         nil            ; input from /dev/null
                         t              ; output
                         nil            ; don't redisplay
                         "--config-file" ,(nnir-read-server-parm 'nnir-swish++-configuration-file server)
                         ,@(nnir-read-server-parm 'nnir-swish++-additional-switches server)
                         ,qstring       ; the query, in swish++ format
                         ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-swish++-program
                         (mapconcat #'identity (nthcdr 4 cp-list) " ")) ;; ???
                (apply #'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run swish++: %s" exitstatus)
          ;; swish++ failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; V 4.7 Linux
      ;; rank relative-path-name file-size file-title
      ;; V 5.0b2:
      ;; rank relative-path-name file-size topic??
      ;; where rank is an integer from 1 to 100.
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[0-9]+\\) \\([^ ]+\\) [0-9]+ \\(.*\\)$" nil t)
        (setq score (match-string 1)
	      filenam (match-string 2)
              artno (file-name-nondirectory filenam)
              dirnam (file-name-directory filenam))

        ;; don't match directories
        (when (string-match article-pattern artno)
          (when (not (null dirnam))

	    ;; maybe limit results to matching groups.
	    (when (or (not groupspec)
		      (string-match groupspec dirnam))
	      (nnir-add-result dirnam artno score prefix server artlist)))))

      (message "Massaging swish++ output...done")

      ;; Sort by score
      (apply #'vector
             (sort artlist
                   (lambda (x y)
                     (> (nnir-artitem-rsv x)
                        (nnir-artitem-rsv y))))))))

;; Swish-E interface.
(defun nnir-run-swish-e (query server &optional _group)
  "Run given QUERY on SERVER against swish-e.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish-e-2.0.1 on Windows NT 4.0."

  ;; swish-e crashes with empty parameter to "-w" on commandline...
  ;; (when group
  ;;   (error "The swish-e backend cannot search specific groups"))

  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix
	   (or (nnir-read-server-parm 'nnir-swish-e-remove-prefix server)
	       (error "Missing parameter `nnir-swish-e-remove-prefix'")))
          artlist score artno dirnam group )

      (when (equal "" qstring)
        (error "swish-e: You didn't enter anything"))

      (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (message "Doing swish-e query %s..." query)
      (let* ((index-files
	      (or (nnir-read-server-parm
		   'nnir-swish-e-index-files server)
		  (error "Missing parameter `nnir-swish-e-index-files'")))
	     (additional-switches
	      (nnir-read-server-parm
	       'nnir-swish-e-additional-switches server))
	     (cp-list `(,nnir-swish-e-program
			nil		; input from /dev/null
			t		; output
			nil		; don't redisplay
			"-f" ,@index-files
			,@additional-switches
			"-w"
			,qstring	; the query, in swish-e format
			))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-swish-e-program
                         (mapconcat #'identity (nthcdr 4 cp-list) " "))
                (apply #'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run swish-e: %s" exitstatus)
          ;; swish-e failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; rank path-name file-title file-size
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[0-9]+\\) \\([^ ]+\\) \"\\([^\"]+\\)\" [0-9]+$" nil t)
        (setq score (match-string 1)
              artno (match-string 3)
              dirnam (file-name-directory (match-string 2)))

        ;; don't match directories
        (when (string-match "^[0-9]+$" artno)
          (when (not (null dirnam))

	    ;; remove nnir-swish-e-remove-prefix from beginning of dirname
            (when (string-match (concat "^" prefix) dirnam)
              (setq dirnam (replace-match "" t t dirnam)))

            (setq dirnam (substring dirnam 0 -1))
	    ;; eliminate all ".", "/", "\" from beginning. Always matches.
            (string-match "^[./\\]*\\(.*\\)$" dirnam)
            ;; "/" -> "."
            (setq group (string-replace
			 "/" "." (match-string 1 dirnam)))
            ;; Windows "\\" -> "."
            (setq group (string-replace "\\" "." group))

            (push (vector (gnus-group-full-name group server)
                          (string-to-number artno)
                          (string-to-number score))
                  artlist))))

      (message "Massaging swish-e output...done")

      ;; Sort by score
      (apply #'vector
             (sort artlist
                   (lambda (x y)
                     (> (nnir-artitem-rsv x)
                        (nnir-artitem-rsv y))))))))

;; HyREX interface
(defun nnir-run-hyrex (query server &optional group)
  "Run given QUERY with GROUP on SERVER against hyrex."
  (save-excursion
    (let ((artlist nil)
          (groupspec (cdr (assq 'hyrex-group query)))
          (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-hyrex-remove-prefix server))
	  score artno dirnam)
      (when (and (not groupspec) group)
        (setq groupspec
	      (regexp-opt
	       (mapcar (lambda (x) (gnus-group-real-name x)) group))))
      (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing hyrex-search query %s..." query)
      (let* ((cp-list
	      `( ,nnir-hyrex-program
		 nil			; input from /dev/null
		 t			; output
		 nil			; don't redisplay
		 "-i",(nnir-read-server-parm 'nnir-hyrex-index-directory server) ; index directory
		 ,@(nnir-read-server-parm 'nnir-hyrex-additional-switches server)
		 ,qstring	   ; the query, in hyrex-search format
		 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-hyrex-program
                         (mapconcat #'identity (nthcdr 4 cp-list) " "))
                (apply #'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run hyrex-search: %s" exitstatus)
          ;; nnir-search failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer)))) ;; FIXME: Don't clear buffer !
      (message "Doing hyrex-search query \"%s\"...done" qstring)
      (sit-for 0)
      ;; nnir-search returns:
      ;;   for nnml/nnfolder: "filename mailid weight"
      ;;   for nnimap:        "group mailid weight"
      (goto-char (point-min))
      (delete-non-matching-lines "^\\S + [0-9]+ [0-9]+$")
      ;; HyREX doesn't search directly in groups -- so filter out here.
      (when groupspec
	(keep-lines groupspec))
      ;; extract data from result lines
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(\\S +\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
	(setq dirnam (match-string 1)
	      artno (match-string 2)
	      score (match-string 3))
	(when (string-match prefix dirnam)
	  (setq dirnam (replace-match "" t t dirnam)))
	(push (vector (gnus-group-full-name
                       (string-replace "/" "." dirnam) server)
		      (string-to-number artno)
		      (string-to-number score))
	      artlist))
      (message "Massaging hyrex-search output...done.")
      (apply #'vector
	     (sort artlist
                   (lambda (x y)
                     (if (string-lessp (nnir-artitem-group x)
                                       (nnir-artitem-group y))
                         t
                       (< (nnir-artitem-number x)
                          (nnir-artitem-number y))))))
      )))

;; Namazu interface
(defun nnir-run-namazu (query server &optional _group)
  "Run QUERY on SERVER  against Namazu.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with Namazu 2.0.6 on a GNU/Linux system."
  ;; (when group
  ;;   (error "The Namazu backend cannot search specific groups"))
  (save-excursion
    (let ((article-pattern (if (string-match "\\`nnmaildir:"
					     (gnus-group-server server))
			       ":[0-9]+"
			     "^[0-9]+$"))
          artlist
	  (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-namazu-remove-prefix server))
          score group article
          (process-environment (copy-sequence process-environment)))
      (setenv "LC_MESSAGES" "C")
      (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (let* ((cp-list
              `( ,nnir-namazu-program
                 nil			; input from /dev/null
                 t			; output
                 nil			; don't redisplay
                 "-q"			; don't be verbose
                 "-a"			; show all matches
                 "-s"			; use short format
                 ,@(nnir-read-server-parm 'nnir-namazu-additional-switches server)
                 ,qstring		; the query, in namazu format
                 ,(nnir-read-server-parm 'nnir-namazu-index-directory server) ; index directory
                 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-namazu-program
                         (mapconcat #'identity (nthcdr 4 cp-list) " "))
                (apply #'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run namazu: %s" exitstatus)
          ;; Namazu failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; Namazu output looks something like this:
      ;; 2. Re: Gnus agent expire broken (score: 55)
      ;; /home/henrik/Mail/mail/sent/1310 (4,138 bytes)

      (goto-char (point-min))
      (while (re-search-forward
              "^\\([0-9,]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
              nil t)
        (setq score (match-string 3)
              group (file-name-directory (match-string 4))
              article (file-name-nondirectory (match-string 4)))

        ;; make sure article and group is sane
        (when (and (string-match article-pattern article)
                   (not (null group)))
	  (nnir-add-result group article score prefix server artlist)))

      ;; sort artlist by score
      (apply #'vector
             (sort artlist
                   (lambda (x y)
                     (> (nnir-artitem-rsv x)
                        (nnir-artitem-rsv y))))))))

(defun nnir-run-notmuch (query server &optional groups)
  "Run QUERY with GROUPS from SERVER against notmuch.
Returns a vector of (group name, file name) pairs (also vectors,
actually).  If GROUPS is a list of group names, use them to
construct path: search terms (see the variable
`nnir-notmuch-filter-group-names-function')."

  (save-excursion
    (let* ((qstring (cdr (assq 'query query)))
	   (prefix (nnir-read-server-parm 'nnir-notmuch-remove-prefix server))
           artlist
	   (article-pattern (if (string-match "\\`nnmaildir:"
					      (gnus-group-server server))
				":[0-9]+"
			      "^[0-9]+$"))
	   (groups (when nnir-notmuch-filter-group-names-function
		     (delq nil
			   (mapcar nnir-notmuch-filter-group-names-function
				   (mapcar #'gnus-group-short-name groups)))))
	   (pathquery (when groups
			(concat " ("
				(mapconcat (lambda (g)
					     (format "path:%s" g))
					   groups " or")
				")")))
           artno dirnam filenam)

      (when (equal "" qstring)
        (error "notmuch: You didn't enter anything"))

      (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (if groups
          (message "Doing notmuch query %s on %s..."
		   qstring (mapconcat #'identity groups " "))
        (message "Doing notmuch query %s..." qstring))

      (when groups
	(setq qstring (concat qstring pathquery)))

      (let* ((cp-list `( ,nnir-notmuch-program
                         nil            ; input from /dev/null
                         t              ; output
                         nil            ; don't redisplay
                         "search"
                         "--format=text"
                         "--output=files"
                         ,@(nnir-read-server-parm 'nnir-notmuch-additional-switches server)
                         ,qstring       ; the query, in notmuch format
                         ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-notmuch-program
                         (mapconcat #'identity (nthcdr 4 cp-list) " ")) ;; ???
                (apply #'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run notmuch: %s" exitstatus)
          ;; notmuch failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; absolute-path-name
      (goto-char (point-min))
      (while (not (eobp))
        (setq filenam (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))
              artno (file-name-nondirectory filenam)
              dirnam (file-name-directory filenam))
        (forward-line 1)

        ;; don't match directories
        (when (string-match article-pattern artno)
          (when (not (null dirnam))

	    (nnir-add-result dirnam artno "" prefix server artlist))))

      (message "Massaging notmuch output...done")

      artlist)))

(defun nnir-run-find-grep (query server &optional grouplist)
  "Run find and grep to QUERY GROUPLIST on SERVER for matching articles."
  (let* ((method (gnus-server-to-method server))
	 (sym (intern
	       (concat (symbol-name (car method)) "-directory")))
	 (directory (cadr (assoc sym (cddr method))))
	 (regexp (cdr (assoc 'query query)))
	 (grep-options (cdr (assoc 'grep-options query)))
	 (grouplist
	  (or grouplist (gnus-server-get-active server nnir-ignored-newsgroups))))
    (unless directory
      (error "No directory found in method specification of server %s"
	     server))
    (apply
     #'vconcat
     (mapcar (lambda (x)
	       (let ((group x)
		     artlist)
		 (message "Searching %s using find-grep..."
			  (or group server))
		 (save-window-excursion
		   (set-buffer (gnus-get-buffer-create nnir-tmp-buffer))
		   (if (> gnus-verbose 6)
		       (pop-to-buffer (current-buffer)))
		   (cd directory) ; Using relative paths simplifies
					; postprocessing.
		   (let ((group
			  (if (not group)
			      "."
			    ;; Try accessing the group literally as
			    ;; well as interpreting dots as directory
			    ;; separators so the engine works with
			    ;; plain nnml as well as the Gnus Cache.
			    (let ((group (gnus-group-real-name group)))
			      ;; Replace cl-func find-if.
			      (if (file-directory-p group)
				  group
				(if (file-directory-p
				     (setq group
					   (string-replace
					    "." "/"
					    group)))
				    group))))))
		     (unless group
		       (error "Cannot locate directory for group"))
		     (save-excursion
		       (apply
			#'call-process "find" nil t
			"find" group "-maxdepth" "1" "-type" "f"
			"-name" "[0-9]*" "-exec"
			"grep"
			`("-l" ,@(and grep-options
				      (split-string grep-options "\\s-" t))
			  "-e" ,regexp "{}" "+"))))

		   ;; Translate relative paths to group names.
		   (while (not (eobp))
		     (let* ((path (split-string
				   (buffer-substring
				    (point)
				    (line-end-position))
				   "/" t))
			    (art (string-to-number (car (last path)))))
		       (while (string= "." (car path))
			 (setq path (cdr path)))
		       (let ((group (mapconcat #'identity
					       ;; Replace cl-func:
					       ;; (subseq path 0 -1)
					       (let ((end (1- (length path)))
						     res)
						 (while
						     (>= (setq end (1- end)) 0)
						   (push (pop path) res))
						 (nreverse res))
					       ".")))
			 (push
			  (vector (gnus-group-full-name group server) art 0)
			  artlist))
		       (forward-line 1)))
		   (message "Searching %s using find-grep...done"
			    (or group server))
		   artlist)))
     grouplist))))

(declare-function mm-url-insert "mm-url" (url &optional follow-refresh))
(declare-function mm-url-encode-www-form-urlencoded "mm-url" (pairs))

;;; Util Code:


(defun nnir-read-parms (nnir-search-engine)
  "Read additional search parameters for NNIR-SEARCH-ENGINE.
Parameters are according to `nnir-engines'."
  (let ((parmspec (nth 2 (assoc nnir-search-engine nnir-engines))))
    (mapcar #'nnir-read-parm parmspec)))

(defun nnir-read-parm (parmspec)
  "Read a single search parameter.
PARMSPEC is a cons cell, the car is a symbol, the cdr is a prompt."
  (let ((sym (car parmspec))
        (prompt (cdr parmspec)))
    (if (listp prompt)
	(let* ((result (apply #'gnus-completing-read prompt))
	       (mapping (or (assoc result nnir-imap-search-arguments)
			    (cons nil nnir-imap-search-other))))
	  (cons sym (format (cdr mapping) result)))
      (cons sym (read-string prompt)))))

(defun nnir-run-query (specs)
  "Invoke search engine appropriate for SPECS (see `nnir-engines')."
  (apply #'vconcat
	 (mapcar
	  (lambda (x)
	    (let* ((server (car x))
		   (search-engine (nnir-server-to-search-engine server))
		   (search-func (cadr (assoc search-engine nnir-engines))))
	      (and search-func
		   (funcall search-func (cdr (assq 'nnir-query-spec specs))
			    server (cdr x)))))
	  (cdr (assq 'nnir-group-spec specs)))))

(defun nnir-server-to-search-engine (server)
  "Find search engine for SERVER."
  (or (nnir-read-server-parm 'nnir-search-engine server t)
      (cdr (assoc (car (gnus-server-to-method server))
		  nnir-method-default-engines))))

(defun nnir-read-server-parm (key server &optional not-global)
  "Return the parameter value corresponding to KEY for SERVER.
If no server-specific value is found consult the global
environment unless NOT-GLOBAL is non-nil."
  (let ((method (gnus-server-to-method server)))
    (cond ((and method (assq key (cddr method)))
           (nth 1 (assq key (cddr method))))
          ((and (not not-global) (boundp key)) (symbol-value key))
          (t nil))))

(autoload 'gnus-group-topic-name "gnus-topic" nil nil)
(defvar gnus-group-marked)
(defvar gnus-topic-alist)

(make-obsolete 'nnir-make-specs "This function should no longer
be used." "28.1")

(defun nnir-make-specs (nnir-extra-parms &optional specs)
  "Make the query-spec and group-spec for a search with NNIR-EXTRA-PARMS.
Query for the specs, or use SPECS."
  (let* ((group-spec
	  (or (cdr (assq 'nnir-group-spec specs))
	      (if (gnus-server-server-name)
		  (list (list (gnus-server-server-name)))
		(seq-group-by
		 (lambda (elt) (gnus-group-server elt))
		 (or gnus-group-marked
		     (if (gnus-group-group-name)
			 (list (gnus-group-group-name))
		       (cdr (assoc (gnus-group-topic-name) gnus-topic-alist))))))))
	 (query-spec
	  (or (cdr (assq 'nnir-query-spec specs))
	      (apply
	       #'append
	       (list (cons 'query
			   (read-string "Query: " nil 'nnir-search-history)))
	       (when nnir-extra-parms
		 (mapcar
		  (lambda (x)
		    (nnir-read-parms (nnir-server-to-search-engine (car x))))
		  group-spec))))))
    (list (cons 'nnir-query-spec query-spec)
	  (cons 'nnir-group-spec group-spec))))

(define-obsolete-function-alias 'nnir-get-active #'gnus-server-get-active "28.1")

(provide 'nnir)

;;; nnir.el ends here
