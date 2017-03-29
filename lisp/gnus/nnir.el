;;; nnir.el --- Search mail with various search engines  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2017 Free Software Foundation, Inc.

;; Author: Kai Großjohann <grossjohann@ls6.cs.uni-dortmund.de>
;; Swish-e and Swish++ backends by:
;;   Christoph Conrad <christoph.conrad@gmx.de>.
;; IMAP backend by: Simon Josefsson <jas@pdc.kth.se>.
;; IMAP search by: Torsten Hilbrich <torsten.hilbrich <at> gmx.net>
;; IMAP search improved by Daniel Pittman  <daniel@rimspace.net>.
;; nnmaildir support for Swish-e, Swish++ and Namazu backends by:
;;   Justus Piater <Justus <at> Piater.name>
;; Mostly rewritten by Andrew Cohen <cohen@bu.edu> from 2010
;; Keywords: news mail searching ir

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; What does it do?  Well, it searches your mail using some search
;; engine (imap, namazu, swish-e, gmane and others -- see later).

;; The Lisp setup may involve setting a few variables and setting up the
;; search engine. You can define the variables in the server definition
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
;; 'nnir-query-spec and 'nnir-group-spec. The value for
;; 'nnir-query-spec is an alist. The only required key/value pair is
;; (query . "query") specifying the search string to pass to the query
;; engine. Individual engines may have other elements. The value of
;; 'nnir-group-spec is a list with the specification of the
;; groups/servers to search.  The format of the 'nnir-group-spec is
;; (("server1" ("group11" "group12")) ("server2" ("group21"
;; "group22"))). If any of the group lists is absent then all groups
;; on that server are searched.

;; The output of `nnir-run-query' is a vector, each element of which
;; should in turn be a three-element vector with the form: [fully
;; prefixed group-name of the article; the article number; the
;; Retrieval Status Value (RSV)] as returned from the search engine.
;; An RSV is the score assigned to the document by the search engine.
;; For Boolean search engines, the RSV is always 1000 (or 1 or 100, or
;; whatever you like).

;; A vector of this form is used by the nnselect backend to create
;; virtual groups. So nnir-run-query is a suitable function to use in
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
;; It is particularly important not to pass any any switches to namazu
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
;; | # Paths which will not be indexed. Don't use `^' or `$' anchors.
;; | $EXCLUDE_PATH = "spam|sent";
;; |
;; | # Header fields which should be searchable. case-insensitive
;; | $REMAIN_HEADER = "from|date|message-id|subject";
;; |
;; | # Searchable fields. case-insensitive
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
  "Internal: the history for querying search options in nnir")

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
  "Mapping from user readable keys to IMAP search items for use in nnir")

(defvar nnir-imap-search-other "HEADER %S"
  "The IMAP search item to use for anything other than
  `nnir-imap-search-arguments'. By default this is the name of an
  email header field")

(defvar nnir-imap-search-argument-history ()
  "The history for querying search options in nnir")

;;; Helper macros

(defmacro nnir-artitem-group (artitem)
  "Returns the group from the ARTITEM."
  `(elt ,artitem 0))

(defmacro nnir-artitem-number (artitem)
  "Returns the number from the ARTITEM."
  `(elt ,artitem 1))

(defmacro nnir-artitem-rsv (artitem)
  "Returns the Retrieval Status Value (RSV, score) from the ARTITEM."
  `(elt ,artitem 2))


;;; User Customizable Variables:

(defgroup nnir nil
  "Search groups in Gnus with assorted search engines."
  :group 'gnus)

(defcustom nnir-use-parsed-queries t
  "When t, use Gnus' generalized search language.

The generalized search language is a sort of \"meta search\"
language that can be used across all search engines that Gnus
supports.  See the Gnus manual for details.

If this option is set to nil, search queries will be passed
directly to the search engines without being parsed or
transformed."
  :version "26.3"
  :type 'boolean
  :group 'nnir)

(defcustom nnir-ignored-newsgroups ""
  "A regexp to match newsgroups in the active file that should
  be skipped when searching."
  :version "24.1"
  :type '(regexp)
  :group 'nnir)


(defcustom nnir-swish++-configuration-file
  (expand-file-name "~/Mail/swish++.conf")
  "Location of Swish++ configuration file.

This variable can also be set per-server."
  :type 'file
  :group 'nnir)

(defcustom nnir-swish++-program "search"
  "Name of swish++ search executable.

This variable can also be set per-server."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish++-additional-switches '()
  "A list of strings, to be given as additional arguments to swish++.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-swish++-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish++-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by swish++
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type '(regexp)
  :group 'nnir)

(defcustom nnir-swish++-raw-queries-p nil
  "If t, all Swish++ engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'nnir)

(defcustom nnir-swish-e-configuration-file
  (expand-file-name "~/Mail/swish-e.conf")
  "Configuration file for swish-e.

This variable can also be set per-server."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish-e-program "search"
  "Name of swish-e search executable.

This variable can also be set per-server."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish-e-additional-switches '()
  "A list of strings, to be given as additional arguments to swish-e.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-swish-e-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish-e-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by swish-e
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type '(regexp)
  :group 'nnir)

(defcustom nnir-swish-e-index-files '()
  "A list of index files to use with this Swish-e instance.

This variable can also be set per-server."
  :type '(repeat file)
  :group 'nnir)

(defcustom nnir-swish-e-raw-queries-p nil
  "If t, all Swish-e engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'nnir)

;; HyREX engine, see <URL:http://ls6-www.cs.uni-dortmund.de/>

(defcustom nnir-hyrex-program "nnir-search"
  "Name of the nnir-search executable.

This variable can also be set per-server."
  :type '(string)
  :group 'nnir)

(defcustom nnir-hyrex-additional-switches '()
  "A list of strings, to be given as additional arguments for nnir-search.
Note that this should be a list. I.e., do NOT use the following:
    (setq nnir-hyrex-additional-switches \"-ddl ddl.xml -c nnir\") ; wrong !
Instead, use this:
    (setq nnir-hyrex-additional-switches \\='(\"-ddl\" \"ddl.xml\" \"-c\" \"nnir\"))

This variable can also be set per-server."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-hyrex-index-directory (getenv "HOME")
  "Index directory for HyREX.

This variable can also be set per-server."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-hyrex-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by HyREX
in order to get a group name (albeit with / instead of .).

For example, suppose that HyREX returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-hyrex-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\".

This variable can also be set per-server."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-hyrex-raw-queries-p nil
  "If t, all Hyrex engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'nnir)

;; Namazu engine, see <URL:http://www.namazu.org/>

(defcustom nnir-namazu-program "namazu"
  "Name of Namazu search executable.

This variable can also be set per-server."
  :type '(string)
  :group 'nnir)

(defcustom nnir-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "Index directory for Namazu.

This variable can also be set per-server."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-namazu-additional-switches '()
  "A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other switches
make any sense in this context.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-namazu-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-namazu-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-namazu-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by Namazu
in order to get a group name (albeit with / instead of .).

For example, suppose that Namazu returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-namazu-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\".

This variable can also be set per-server."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-namazu-raw-queries-p nil
  "If t, all Namazu engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'nnir)

(defcustom nnir-notmuch-program "notmuch"
  "Name of notmuch search executable.

This variable can also be set per-server."
  :version "24.1"
  :type '(string)
  :group 'nnir)

(defcustom nnir-notmuch-configuration-file
  (expand-file-name "~/.notmuch-config")
  "Configuration file for notmuch.

This variable can also be set per-server."
  :type '(file)
  :group 'nnir)

(defcustom nnir-notmuch-additional-switches '()
  "A list of strings, to be given as additional arguments to notmuch.

Note that this should be a list.  I.e., do NOT use the following:
    (setq nnir-notmuch-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-notmuch-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :version "24.1"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-notmuch-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by notmuch
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :version "24.1"
  :type '(regexp)
  :group 'nnir)

;;;  Extension Variable:

(defvar nnir-engines
  `((imap    nnir-run-imap
             ((criteria
	       "Imap Search in"                   ; Prompt
	       ,(mapcar 'car nnir-imap-search-arguments) ; alist for completing
	       nil                                ; allow any user input
	       nil                                ; initial value
	       nnir-imap-search-argument-history  ; the history to use
	       ,nnir-imap-default-search-key      ; default
	       )))
    (gmane   nnir-run-gmane
	     ((gmane-author . "Gmane Author: ")))
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

(defcustom nnir-method-default-engines  '((nnimap . imap) (nntp . gmane))
  "Alist of default search engines keyed by server method."
  :version "24.1"
  :group 'nnir
  :version "26.1"
  :type '(repeat string))



(defmacro nnir-add-result (dirnam artno score prefix server artlist)
  "Ask `nnir-compose-result' to construct a result vector,
and if it is non-nil, add it to artlist."
  `(let ((result (nnir-compose-result ,dirnam ,artno ,score ,prefix ,server)))
     (when (not (null result))
       (push result ,artlist))))

(autoload 'nnmaildir-base-name-to-article-number "nnmaildir")

;; Helper function currently used by the Swish++ and Namazu backends;
;; perhaps useful for other backends as well
(defun nnir-compose-result (dirnam article score prefix server)
  "Extract the group from dirnam, and create a result vector
ready to be added to the list of search results."

  ;; remove nnir-*-remove-prefix from beginning of dirnam filename
  (when (string-match (concat "^" prefix) dirnam)
    (setq dirnam (replace-match "" t t dirnam)))

\"contact\" will search messages to/from a contact.  Contact
management packages must push a function onto
`nnir-search-contact-sources', the docstring of which see, for
this to work.

    ;; Set group to dirnam without any leading dots or slashes,
    ;; and with all subsequent slashes replaced by dots
    (let ((group (replace-regexp-in-string
		  "[/\\]" "."
                  (replace-regexp-in-string "^[./\\]" "" dirnam nil t)
                  nil t)))

\"contact-to\" searches the same headers as \"recipient\".

Other keys can be specified, provided that the search backends
know how to interpret them.

Date values (any key in `nnir-search-date-keys') can be provided
in any format that `parse-time-string' can parse (note that this
can produce weird results).  Dates with missing bits will be
interpreted as the most recent occurance thereof (ie \"march 03\"
is the most recent March 3rd).  Lastly, relative specifications
such as 1d (one day ago) are understood.  This also accepts w, m,
and y.  m is assumed to be 30 days.

;; imap interface
(defun nnir-run-imap (query srv &optional groups)
  "Run a search against an IMAP back-end server.
This uses a custom query language parser; see `nnir-imap-make-query' for
details on the language and supported extensions."
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
          (server (cadr (gnus-server-to-method srv)))
          (criteria (or (cdr (assq 'criteria query))
                        (cdr (assoc nnir-imap-default-search-key
                                    nnir-imap-search-arguments))))
          (gnus-inhibit-demon t)
	  (groups (or groups (nnir-get-active srv))))
      (message "Opening server %s" server)
      (apply
       'vconcat
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
  "Parse the query string and criteria into an appropriate IMAP search
expression, returning the string query to make.

This implements a little language designed to return the expected results
to an arbitrary query string to the end user.

The search is always case-insensitive, as defined by RFC2060, and supports
the following features (inspired by the Google search input language):

Automatic \"and\" queries
    If you specify multiple words then they will be treated as an \"and\"
    expression intended to match all components.

Phrase searches
    If you wrap your query in double-quotes then it will be treated as a
    literal string.

Negative terms
    If you precede a term with \"-\" then it will negate that.

\"OR\" queries
    If you include an upper-case \"OR\" in your search it will cause the
    term before it and the term after it to be treated as alternatives.

In future the following will be added to the language:
 * support for date matches
 * support for location of text matching within the query
 * from/to/etc headers
 * additional search terms
 * flag based searching
 * anything else that the RFC supports, basically."
  ;; Walk through the query and turn it into an IMAP query string.
  (nnir-imap-query-to-imap criteria (nnir-imap-parse-query qstring)))


(defun nnir-imap-query-to-imap (criteria query)
  "Turn a s-expression format query into IMAP."
  (mapconcat
   ;; Turn the expressions into IMAP text
   (lambda (item)
     (nnir-imap-expr-to-imap criteria item))
   ;; The query, already in s-expr format.
   query
   ;; Append a space between each expression
   " "))


(defun nnir-imap-expr-to-imap (criteria expr)
  "Convert EXPR into an IMAP search expression on CRITERIA"
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
  "Turn STRING into an s-expression based query based on the IMAP
query language as defined in `nnir-imap-make-query'.

This involves turning individual tokens into higher level terms
that the search language can then understand and use."
  (with-temp-buffer
    ;; Set up the parsing environment.
    (insert string)
    (goto-char (point-min))
    ;; Now, collect the output terms and return them.
    (let (out)
      (while (not (nnir-query-end-of-input))
	(push (nnir-query-next-expr) out))
      (reverse out))))

(defun nnir-query-next-expr (&optional count halt)
  "Return the next expression from the current buffer."
  (let ((term (nnir-query-next-term count))
	(next (nnir-query-peek-symbol)))
    ;; Deal with top-level expressions.  And, or, not, near...  What
    ;; else?  Notmuch also provides xor and adj.  It also provides a
    ;; "nearness" parameter for near and adj.
    (cond
     ;; Handle 'expr or expr'
     ((and (eq next 'or)
	   (null halt))
      (list 'or term (nnir-query-next-expr 2)))
     ;; Handle 'near operator.
     ((and (eq next 'near))
      (let ((near-next (nnir-query-next-expr 2)))
	(if (and (stringp term)
		 (stringp near-next))
	    (list 'near term near-next)
	  (signal 'gnus-search-parse-error
		  (list "\"Near\" keyword must appear between two plain strings.")))))
     ;; Anything else
     (t term))))

(defun nnir-query-next-term (&optional count)
  "Return the next TERM from the current buffer."
  (let ((term (nnir-query-next-symbol count)))
    ;; What sort of term is this?
    (cond
     ;; negated term
     ((eq term 'not) (list 'not (nnir-query-next-expr nil 'halt)))
     ;; generic term
     (t term))))

(defun nnir-query-peek-symbol ()
  "Return the next symbol from the current buffer, but don't consume it."
  (save-excursion
    (nnir-query-next-symbol)))

(defun nnir-query-next-symbol (&optional count)
  "Return the next symbol from the current buffer, or nil if we are
at the end of the buffer.  If supplied COUNT skips some symbols before
returning the one at the supplied position."
  (when (and (numberp count) (> count 1))
    (nnir-query-next-symbol (1- count)))
  (let ((case-fold-search t))
    ;; end of input stream?
    (unless (nnir-query-end-of-input)
      ;; No, return the next symbol from the stream.
      (cond
       ;; Negated expression -- return it and advance one char.
       ((looking-at "-") (forward-char 1) 'not)
       ;; List expression -- we parse the content and return this as a list.
       ((looking-at "(")
	(nnir-search-parse-query (nnir-query-return-string ")")))
       ;; Keyword input -- return a symbol version.
       ((looking-at "\\band\\b") (forward-char 3) 'and)
       ((looking-at "\\bor\\b")  (forward-char 2) 'or)
       ((looking-at "\\bnot\\b") (forward-char 3) 'not)
       ((looking-at "\\bnear\\b") (forward-char 4) 'near)
       ;; Plain string, no keyword
       ((looking-at "\"?\\b[^:]+\\([[:blank:]]\\|\\'\\)")
	(nnir-query-return-string
	 (when (looking-at "\"") "\"")))
       ;; Assume a K:V expression.
       (t (let ((key (nnir-query-expand-key
		      (buffer-substring
		       (point)
		       (progn
			 (re-search-forward ":" (point-at-eol) t)
			 (1- (point))))))
		(value (nnir-query-return-string
			(when (looking-at "\"") "\""))))
	    (nnir-query-parse-kv key value)))))))

(defun nnir-query-parse-kv (key value)
  "Handle KEY and VALUE, parsing and expanding as necessary.

This may result in (key value) being turned into a larger query
structure.

In the simplest case, they are simply consed together.  KEY comes
in as a string, goes out as a symbol."
  (let (return)
    (cond
     ((member key nnir-search-date-keys)
      (when (string= "after" key)
	(setq key "since"))
      (setq value (nnir-query-parse-date value)))
     ((string-match-p "contact" key)
      (setq return (nnir-query-parse-contact key value)))
     ((equal key "address")
      (setq return `(or (sender . ,value) (recipient . ,value))))
     ((equal key "mark")
      (setq value (nnir-query-parse-mark value))))
    (or return
	(cons (intern key) value))))

(defun nnir-query-parse-date (value &optional rel-date)
  "Interpret VALUE as a date specification.

See the docstring of `nnir-search-parse-query' for details.

The result is a list of (dd mm yyyy); individual elements can be
nil.

If VALUE is a relative time, interpret it as relative to
REL-DATE, or \(current-time\) if REL-DATE is nil."
  ;; Time parsing doesn't seem to work with slashes.
  (let ((value (replace-regexp-in-string "/" "-" value))
	(now (append '(0 0 0)
		     (seq-subseq (decode-time (or rel-date
						  (current-time)))
				 3))))
    ;; Check for relative time parsing.
    (if (string-match "\\([[:digit:]]+\\)\\([dwmy]\\)" value)
	(seq-subseq
	 (decode-time
	  (time-subtract
	   (apply #'encode-time now)
	   (days-to-time
	    (* (string-to-number (match-string 1 value))
	       (cdr (assoc (match-string 2 value)
			   '(("d" . 1)
			     ("w" . 7)
			     ("m" . 30)
			     ("y" . 365))))))))
	 3 6)
      ;; Otherwise check the value of `parse-time-string'.

      ;; (SEC MIN HOUR DAY MON YEAR DOW DST TZ)
      (let ((d-time (parse-time-string value)))
	;; Did parsing produce anything at all?
	(if (seq-some #'integerp (seq-subseq d-time 3 7))
	    (seq-subseq
	     ;; If DOW is given, handle that specially.
	     (if (and (seq-elt d-time 6) (null (seq-elt d-time 3)))
		 (decode-time
		  (time-subtract (apply #'encode-time now)
				 (days-to-time
				  (+ (if (> (seq-elt d-time 6)
					    (seq-elt now 6))
					 7 0)
				     (- (seq-elt now 6) (seq-elt d-time 6))))))
	       d-time)
	     3 6)
	  ;; `parse-time-string' failed to produce anything, just
	  ;; return the string.
	  value)))))

(defun nnir-query-parse-mark (mark)
  "Possibly transform MARK.

If MARK is a single character, assume it is one of the
gnus-*-mark marks, and return an appropriate string."
  (if (= 1 (length mark))
      (let ((m (aref mark 0)))
	;; Neither pcase nor cl-case will work here.
       (cond
	 ((eql m gnus-ticked-mark) "flag")
	 ((eql m gnus-read-mark) "read")
	 ((eql m gnus-replied-mark) "replied")
	 ((eql m gnus-recent-mark) "recent")
	 (t mark)))
    mark))

(defun nnir-query-parse-contact (key value)
  "Handle VALUE as the name of a contact.

Runs VALUE through the elements of
`nnir-search-contact-sources' until one of them returns a list
of email addresses.  Turns those addresses into an appropriate
chunk of query syntax."
  (let ((funcs (or (copy-sequence nnir-search-contact-sources)
		   (signal 'gnus-search-parse-error
		    (list "No functions for handling contacts."))))
	func addresses)
    (while (and (setq func (pop funcs))
		(null addresses))
      (setq addresses (if (functionp func)
			  (funcall func value)
			(when (string= value (car func))
			  (cdr func)))))
    (unless addresses
      (setq addresses (list value)))
    ;; Simplest case: single From address.
    (if (and (null (cdr addresses))
	     (equal key "contact-from"))
	(cons 'sender (car addresses))
      (cons
       'or
       (mapcan
	(lambda (a)
	  (pcase key
	    ("contact-from"
	     (list (cons 'sender a)))
	    ("contact-to"
	     (list (cons 'recipient a)))
	    ("contact"
	     `(or (recipient . ,a) (sender . ,a)))))
	addresses)))))

(defun nnir-query-expand-key (key)
  "Attempt to expand KEY to a full keyword."
  (let ((bits (split-string key "-"))
	bit out-bits comp)
    (if (try-completion (car bits) nnir-search-expandable-keys)
	(progn
	  (while (setq bit (pop bits))
	    (setq comp (try-completion bit nnir-search-expandable-keys))
	    (if (stringp comp)
		(if (and (string= bit comp)
			 (null (member comp nnir-search-expandable-keys)))
		    (signal 'gnus-search-parse-error
			    (list (format "Ambiguous keyword: %s" key)))
		  (push comp out-bits))
	      (push bit out-bits)))
	  (mapconcat #'identity (reverse out-bits) "-"))
      key)))

;; (defun nnir-query-expand-key (key)
;;   "Attempt to expand (possibly abbreviated) KEY to a full keyword.

;; Can handle any non-ambiguous abbreviation, with hyphens as substring separator."
;;   (let* ((bits (split-string key "-"))
;; 	 (bit (pop bits))
;; 	 (comp (all-completions bit nnir-search-expandable-keys)))
;;     ;; Make a cl-labels recursive function, that accepts a rebuilt key and
;;     ;; results of `all-completions' back in as a COLLECTION argument.
;;     (if (= 1 (length comp))
;; 	(setq key (car comp))
;;       (when (setq comp (try-completion bit nnir-search-expandable-keys))
;; 	(if (and (string= bit comp)
;; 		 (null (member comp nnir-search-expandable-keys)))
;; 	    (error "Ambiguous keyword: %s" key)))
;;       (unless (eq t (try-completion key nnir-search-expandable-keys))))
;;     key))


(defun nnir-query-return-string (&optional delimiter)
  "Return a string from the current buffer.

If DELIMITER is given, return everything between point and the
next occurance of DELIMITER.  Otherwise, return one word."
  (let ((start (point)) end)
    (if delimiter
	(progn
	  (forward-char 1)		; skip the first delimiter.
	  (while (not end)
	    (unless (search-forward delimiter nil t)
	      (signal 'gnus-search-parse-error
		      (list (format "Unmatched delimited input with %s in query" delimiter))))
	    (let ((here (point)))
	      (unless (equal (buffer-substring (- here 2) (- here 1)) "\\")
		(setq end (1- (point))
		      start (1+ start))))))
      (setq end (progn (re-search-forward "\\([[:blank:]]+\\|$\\)" (point-max) t)
		       (match-beginning 0))))
    (buffer-substring start end)))

(defun nnir-query-end-of-input ()
  "Are we at the end of input?"
  (skip-chars-forward "[[:blank:]]")
  (looking-at "$"))

(defmacro nnir-add-result (dirnam artno score prefix server artlist)
  "Ask `nnir-compose-result' to construct a result vector,
and if it is non-nil, add it to artlist."
  `(let ((result (nnir-compose-result ,dirnam ,artno ,score ,prefix ,server) ))
     (when (not (null result))
       (push result ,artlist))))

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
  "Run QUERY against swish++.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

(defun nnir-compose-result (dirnam article score prefix server)
  "Extract the group from dirnam, and create a result vector
ready to be added to the list of search results."

  ;; remove nnir-*-remove-prefix from beginning of dirnam filename
  (when (string-match (concat "^"
			      (file-name-as-directory prefix))
		      dirnam)
    (setq dirnam (replace-match "" t t dirnam)))

  (when (file-readable-p (concat prefix dirnam article))
    ;; remove trailing slash and, for nnmaildir, cur/new/tmp
    (setq dirnam
	  (replace-regexp-in-string
	   "/?\\(cur\\|new\\|tmp\\)?/\\'" "" dirnam))

    ;; Set group to dirnam without any leading dots or slashes,
    ;; and with all subsequent slashes replaced by dots
    (let ((group (replace-regexp-in-string
		  "[/\\]" "."
		  (replace-regexp-in-string "^[./\\]" "" dirnam nil t)
		  nil t)))

      (vector (gnus-group-full-name group server)
	      (if (string-match-p "\\`[[:digit:]]+\\'" article)
		  (string-to-number article)
		(nnmaildir-base-name-to-article-number
		 (substring article 0 (string-match ":" article))
		 group nil))
	      (string-to-number score)))))

;;; Search engines

;; Search engines are implemented as classes.  This is good for two
;; things: encapsulating things like indexes and search prefixes, and
;; transforming search queries.

(defclass gnus-search-engine ()
  ((raw-queries-p
    :initarg :raw-queries-p
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "When t, searches through this engine will never be parsed or
    transformed, and must be entered \"raw\"."))
  :abstract t
  :documentation "Abstract base class for Gnus search engines.")

(defclass gnus-search-process ()
  ((proc-buffer
    :initarg :proc-buffer
    :type buffer
    :documentation "A temporary buffer this engine uses for its
    search process, and for munging its search results."))
  :abstract t
  :documentation
  "A mixin class for engines that do their searching in a single
  process launched for this purpose, which returns at the end of
  the search.  Subclass instances are safe to be run in
  threads.")

(cl-defmethod shared-initialize ((engine gnus-search-process)
				 slots)
  (setq slots (plist-put slots :proc-buffer
			 (get-buffer-create
			  (generate-new-buffer-name " *nnir-search-"))))
  (cl-call-next-method engine slots))

(defclass gnus-search-imap (gnus-search-engine)
  ((literal-plus
    :initarg :literal-plus
    :initform nil
    :type boolean
    :documentation
    "Can this search engine handle literal+ searches?  This slot
    is set automatically by the imap server, and cannot be
    set manually.  Only the LITERAL+ capability is handled.")
   (multisearch
    :initarg :multisearch
    :iniformt nil
    :type boolean
    :documentation
    "Can this search engine handle the MULTISEARCH capability?
    This slot is set automatically by the imap server, and cannot
    be set manually.  Currently unimplemented.")
   (fuzzy
    :initarg :fuzzy
    :iniformt nil
    :type boolean
    :documentation
    "Can this search engine handle the FUZZY search capability?
    This slot is set automatically by the imap server, and cannot
    be set manually.  Currently unimplemented."))
  :documentation
  "The base IMAP search engine, using an IMAP server's search capabilites.

This backend may be subclassed to handle particular IMAP servers'
quirks.")

(eieio-oset-default 'gnus-search-imap 'raw-queries-p
		    nnir-imap-raw-queries-p)

(defclass gnus-search-find-grep (gnus-search-engine gnus-search-process)
  nil)

(defclass gnus-search-gmane (gnus-search-engine gnus-search-process)
  nil)

;;; The "indexed" search engine.  These are engines that use an
;;; external program, with indexes kept on disk, to search messages
;;; usually kept in some local directory.  The three common slots are
;;; "program", holding the string name of the executable; "switches",
;;; holding additional switches to pass to the executable; and
;;; "prefix", which is sort of the path to the found messages which
;;; should be removed so that Gnus can find them.  Many of the
;;; subclasses also allow distinguishing multiple databases or
;;; indexes.  These slots can be set using a global default, or on a
;;; per-server basis.

(defclass gnus-search-indexed (gnus-search-engine gnus-search-process)
  ((program
    :initarg :program
    :type string
    :documentation
    "The executable used for indexing and searching.")
   (prefix
    :initarg :prefix
    :type string
    :documentation
    "The path to the directory where the indexed mails are
    kept. This path is removed from the search results.")
   (switches
    :initarg :switches
    :type list
    :documentation
    "Additional switches passed to the search engine command-line
    program."))
    :abstract t
  :allow-nil-initform t
  :documentation "A base search engine class that assumes a local search index
  accessed by a command line program.")

(eieio-oset-default 'gnus-search-indexed 'prefix
		    (concat (getenv "HOME") "/Mail/"))

(defclass gnus-search-swish-e (gnus-search-indexed)
  ((index-files
    :init-arg :index-files
    :type list)))

(eieio-oset-default 'gnus-search-swish-e 'program
		    nnir-swish-e-program)

(eieio-oset-default 'gnus-search-swish-e 'prefix
		    nnir-swish-e-remove-prefix)

(eieio-oset-default 'gnus-search-swish-e 'index-files
		    nnir-swish-e-index-files)

(eieio-oset-default 'gnus-search-swish-e 'switches
		    nnir-swish-e-additional-switches)

(eieio-oset-default 'gnus-search-swish-e 'raw-queries-p
		    nnir-swish-e-raw-queries-p)

(defclass gnus-search-swish++ (gnus-search-indexed)
  ((config-file
    :init-arg :config-file
    :type string)))

(eieio-oset-default 'gnus-search-swish++ 'program
		    nnir-swish++-program)

(eieio-oset-default 'gnus-search-swish++ 'prefix
		    nnir-swish++-remove-prefix)

(eieio-oset-default 'gnus-search-swish++ 'config-file
		    nnir-swish++-configuration-file)

(eieio-oset-default 'gnus-search-swish++ 'switches
		    nnir-swish++-additional-switches)

(eieio-oset-default 'gnus-search-swish++ 'raw-queries-p
		    nnir-swish++-raw-queries-p)

;; Hyrex possibly bogus, why is the default program name
;; "nnir-search"?
(defclass gnus-search-hyrex (gnus-search-indexed)
  ((index-dir
    :initarg :index
    :type string
    :custom directory)))

(eieio-oset-default 'gnus-search-hyrex 'program
		    nnir-hyrex-program)

(eieio-oset-default 'gnus-search-hyrex 'index-dir
		    nnir-hyrex-index-directory)

(eieio-oset-default 'gnus-search-hyrex 'switches
		    nnir-hyrex-additional-switches)

(eieio-oset-default 'gnus-search-hyrex 'prefix
		    nnir-hyrex-remove-prefix)

(eieio-oset-default 'gnus-search-hyrex 'raw-queries-p
		    nnir-hyrex-raw-queries-p)

(defclass gnus-search-namazu (gnus-search-indexed)
  ((index-dir
    :initarg :index-dir
    :type string
    :custom directory)))

(eieio-oset-default 'gnus-search-namazu 'program
		    nnir-namazu-program)

(eieio-oset-default 'gnus-search-namazu 'index-dir
		    nnir-namazu-index-directory)

(eieio-oset-default 'gnus-search-namazu 'switches
		    nnir-namazu-additional-switches)

(eieio-oset-default 'gnus-search-namazu 'prefix
		    nnir-namazu-remove-prefix)

;; Swish-E interface.
(defun nnir-run-swish-e (query server &optional _group)
  "Run given query against swish-e.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

(defclass gnus-search-notmuch (gnus-search-indexed)
  ((config-file
    :init-arg :config-file
    :type string)))

(eieio-oset-default 'gnus-search-notmuch 'program
		    nnir-notmuch-program)

(eieio-oset-default 'gnus-search-notmuch 'switches
		    nnir-notmuch-additional-switches)

(eieio-oset-default 'gnus-search-notmuch 'prefix
		    nnir-notmuch-remove-prefix)

(eieio-oset-default 'gnus-search-notmuch 'config-file
		    nnir-notmuch-configuration-file)

(eieio-oset-default 'gnus-search-notmuch 'raw-queries-p
		    nnir-notmuch-raw-queries-p)

(defcustom nnir-method-default-engines '((nnimap gnus-search-imap)
					 (nntp  gnus-search-gmane))
  "Alist of default search engines keyed by server method."
  :version "26.1"
  :group 'nnir
  :type `(repeat (list (choice (const nnimap) (const nntp) (const nnspool)
			       (const nneething) (const nndir) (const nnmbox)
			       (const nnml) (const nnmh) (const nndraft)
			       (const nnfolder) (const nnmaildir))
		       (choice
			,@(mapcar
			   (lambda (el) (list 'const (intern (car el))))
			   (eieio-build-class-alist 'gnus-search-engine t))))))

;;; Transforming and running search queries.

(cl-defgeneric nnir-run-search (backend server query groups)
  "Run QUERY in GROUPS against SERVER, using search BACKEND.

Should return results as a vector of vectors.")

(cl-defgeneric nnir-search-transform-top-level (backend expression)
  "Transform sexp EXPRESSION into a string search query usable by BACKEND.

Responsible for handling and, or, and parenthetical expressions.")

(cl-defgeneric nnir-search-transform-expression (backend expression)
  "Transform a basic EXPRESSION into a string usable by BACKEND.")

;; Methods that are likely to be the same for all engines.

(cl-defmethod nnir-search-transform-top-level ((engine gnus-search-engine)
					       (query list))
  (let (clauses)
   (mapc
    (lambda (item)
      (when-let ((expr (nnir-search-transform-expression engine item)))
	(push expr clauses)))
    query)
   (mapconcat #'identity (reverse clauses) " ")))

;; Most search engines want quoted string phrases.
(cl-defmethod nnir-search-transform-expression ((_ gnus-search-engine)
						(expr string))
  (if (string-match-p " " expr)
      (format "\"%s\"" expr)
    expr))

;; Most search engines use implicit ANDs.
(cl-defmethod nnir-search-transform-expression ((_ gnus-search-engine)
						(_expr (eql and)))
  nil)

;; Most search engines use explicit infixed ORs.
(cl-defmethod nnir-search-transform-expression ((engine gnus-search-engine)
						(expr (head or)))
  (let ((left (nnir-search-transform-expression engine (nth 1 expr)))
	(right (nnir-search-transform-expression engine (nth 2 expr))))
    ;; Unhandled keywords return a nil; don't create an "or" expression
    ;; unless both sub-expressions are non-nil.
    (if (and left right)
	(format "%s or %s" left right)
      (or left right))))

;; Most search engines just use the string "not"
(cl-defmethod nnir-search-transform-expression ((engine gnus-search-engine)
						(expr (head not)))
  (let ((next (nnir-search-transform-expression engine (cadr expr))))
    (when next
     (format "not %s" next))))

;;; Search Engine Interfaces:

(autoload 'nnimap-change-group "nnimap")
(declare-function nnimap-buffer "nnimap" ())
(declare-function nnimap-command "nnimap" (&rest args))

;; imap interface
(cl-defmethod nnir-run-search ((engine gnus-search-imap)
			       srv query groups)
  (save-excursion
    (let ((server (cadr (gnus-server-to-method srv)))
          (gnus-inhibit-demon t))
      (message "Opening server %s" server)
      ;; We should only be doing this once, in
      ;; `nnimap-open-connection', but it's too frustrating to try to
      ;; get to the server from the process buffer.
      (with-current-buffer (nnimap-buffer)
	(setf (slot-value engine 'literal-plus)
	      (when (nnimap-capability "LITERAL+") t))
	;; MULTISEARCH not yet implemented.
	(setf (slot-value engine 'multisearch)
	      (when (nnimap-capability "MULTISEARCH") t)))
      (when (listp query)
       (setq query
	     (nnir-search-transform-top-level
	      engine query)))
      (apply
       'vconcat
       (mapcar
	(lambda (group)
	  (let (artlist)
	    (condition-case ()
		(when (nnimap-change-group
		       (gnus-group-short-name group) server)
		  (with-current-buffer (nnimap-buffer)
		    (message "Searching %s..." group)
		    (let ((arts 0)
			  (result
			   (nnir-imap-search-command engine query)))
		      (mapc
		       (lambda (artnum)
			 (let ((artn (string-to-number artnum)))
			   (when (> artn 0)
			     (push (vector group artn 100)
				   artlist)
			     (setq arts (1+ arts)))))
		       (and (car result)
			    (cdr (assoc "SEARCH" (cdr result)))))
		      (message "Searching %s... %d matches" group arts)))
		  (message "Searching %s...done" group))
	      (quit nil))
	    (nreverse artlist)))
	groups)))))

(cl-defmethod nnir-imap-search-command ((engine gnus-search-imap)
					(query string))
  "Create the IMAP search command for QUERY.

Currenly takes into account support for the LITERAL+ capability.
Other capabilities could be tested here."
  (with-slots (literal-plus) engine
    (when literal-plus
      (setq query (split-string query "\n")))
    (cond
     ((consp query)
      ;; We're not really streaming, just need to prevent
      ;; `nnimap-send-command' from waiting for a response.
      (let* ((nnimap-streaming t)
	     (call
	      (nnimap-send-command
	       "UID SEARCH CHARSET UTF-8 %s"
	       (pop query))))
	(dolist (l query)
	  (process-send-string (get-buffer-process (current-buffer)) l)
	  (process-send-string (get-buffer-process (current-buffer))
			       (if (nnimap-newlinep nnimap-object)
				   "\n"
				 "\r\n")))
	(nnimap-get-response call)))
     (t (nnimap-command "UID SEARCH %s" query)))))

;; TODO: Don't exclude booleans and date keys, just check for them
;; before checking for general keywords.
(defvar nnir-imap-search-keys
  '(body cc from header keyword larger smaller subject text to uid)
  "Known IMAP search keys, excluding booleans and date keys.")

(cl-defmethod nnir-search-transform-top-level ((_ gnus-search-imap)
					       (_query null))
  "ALL")

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-imap)
						(expr string))
  (format "TEXT %s" (nnir-imap-handle-string engine expr)))

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-imap)
						(expr (head or)))
  (let ((left (nnir-search-transform-expression engine (nth 1 expr)))
	(right (nnir-search-transform-expression engine (nth 2 expr))))
    (if (and left right)
	(format "OR %s %s" left right)
      (or left right))))

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-imap)
						(expr (head near)))
  "Imap searches interpret \"near\" as \"or\"."
  (setcar expr 'or)
  (nnir-search-transform-expression engine expr))

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-imap)
						(expr (head not)))
  "Transform IMAP NOT.

If the term to be negated is a flag, then use the appropriate UN*
boolean instead."
  (if (eql (caadr expr) 'mark)
      (if (string= (cdadr expr) "new")
	  "OLD"
	(format "UN%s" (nnir-imap-handle-flag (cdadr expr))))
    (format "NOT %s"
	    (nnir-search-transform-expression engine (cadr expr)))))

(cl-defmethod nnir-search-transform-expression ((_ gnus-search-imap)
						(expr (head mark)))
  (nnir-imap-handle-flag (cdr expr)))

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-imap)
						(expr list))
  ;; Search keyword.  All IMAP search keywords that take a value are
  ;; supported directly.  Keywords that are boolean are supported
  ;; through other means (usually the "mark" keyword).
  (cl-case (car expr)
    (date (setcar expr 'on))
    (tag (setcar expr 'keyword)))
  (cond
   ((consp (car expr))
    (format "(%s)" (nnir-search-transform-top-level engine expr)))
   ((eq (car expr) 'sender)
    (format "FROM %s" (cdr expr)))
   ((eq (car expr) 'recipient)
    (format "OR (OR TO %s CC %s) BCC %s" (cdr expr) (cdr expr) (cdr expr)))
   ((memq (car expr) nnir-imap-search-keys)
    (format "%s %s"
	    (upcase (symbol-name (car expr)))
	    (nnir-imap-handle-string engine (cdr expr))))
   ((memq (car expr) '(before since on sentbefore senton sentsince))
    ;; Ignore dates given as strings.
    (when (listp (cdr expr))
      (format "%s %s"
	      (upcase (symbol-name (car expr)))
	      (nnir-imap-handle-date engine (cdr expr)))))
   ((eq (car expr) 'id)
    (format "HEADER Message-ID %s" (cdr expr)))
   ;; Treat what can't be handled as a HEADER search.  Probably a bad
   ;; idea.
   (t (format "HEADER %s %s"
	      (car expr)
	      (nnir-imap-handle-string engine (cdr expr))))))

(cl-defmethod nnir-imap-handle-date ((_engine gnus-search-imap)
				     (date list))
  "Turn DATE into a date string recognizable by IMAP.

While other search engines can interpret partially-qualified
dates such as a plain \"January\", IMAP requires an absolute
date.

DATE is a list of (dd mm yyyy), any element of which could be
nil.  Massage those numbers into the most recent past occurrence
of whichever date elements are present."
  (let ((now (decode-time (current-time))))
    ;; Set nil values to 1, current-month, current-year, or else 1, 1,
    ;; current-year, depending on what we think the user meant.
    (unless (seq-elt date 1)
      (setf (seq-elt date 1)
	    (if (seq-elt date 0)
		(seq-elt now 4)
	      1)))
    (unless (seq-elt date 0)
      (setf (seq-elt date 0) 1))
    (unless (seq-elt date 2)
      (setf (seq-elt date 2)
	    (seq-elt now 5)))
    ;; Fiddle with the date until it's in the past.  There
    ;; must be a way to combine all these steps.
    (unless (< (seq-elt date 2)
	       (seq-elt now 5))
      (when (< (seq-elt now 3)
	       (seq-elt date 0))
	(cl-decf (seq-elt date 1)))
      (cond ((zerop (seq-elt date 1))
	     (setf (seq-elt date 1) 1)
	     (cl-decf (seq-elt date 2)))
	    ((< (seq-elt now 4)
		(seq-elt date 1))
	     (cl-decf (seq-elt date 2))))))
  (format-time-string "%e-%b-%Y" (apply #'encode-time
					(append '(0 0 0)
						date))))

(cl-defmethod nnir-imap-handle-string ((engine gnus-search-imap)
				       (str string))
  (with-slots (literal-plus) engine
    ;; STR is not ASCII.
    (if (null (= (length str)
		 (string-bytes str)))
	(if literal-plus
	    ;; If LITERAL+ is available, use it and force UTF-8.
	    (format "{%d+}\n%s"
		    (string-bytes str)
		    (encode-coding-string str 'utf-8))
	  ;; Other servers might be able to parse it if quoted.
	  (format "\"%s\"" str))
      (if (string-match-p " " str)
	  (format "\"%s\"" str)
       str))))

(defun nnir-imap-handle-flag (flag)
  "Make sure string FLAG is something IMAP will recognize."
  ;; What else?  What about the KEYWORD search key?
  (setq flag
	(pcase flag
	  ("flag" "flagged")
	  ("read" "seen")
	  (_ flag)))
  (if (member flag '("seen" "answered" "deleted" "draft" "flagged"))
      (upcase flag)
    ""))

;;; Methods for the indexed search engines.

;; First, some common methods.

(cl-defgeneric nnir-search-indexed-massage-output (engine server &optional groups)
  "Massage the results of ENGINE's query against SERVER in GROUPS.

Most indexed search engines return results as a list of filenames
or something similar.  Turn those results into something nnir
understands.")

(cl-defmethod nnir-run-search ((engine gnus-search-indexed)
			       server query groups)
  "Run QUERY against SERVER using ENGINE.

This method is common to all indexed search engines.

Returns a vector of [group name, file name, score] vectors."

  (save-excursion
    (let* ((qstring (if (listp query)
			(nnir-search-transform-top-level engine query)
		      query))
	   (program (slot-value engine 'program))
	   (buffer (slot-value engine 'proc-buffer))
	   (cp-list (nnir-search-indexed-search-command
		     engine qstring groups))
           proc exitstatus artlist)
      (set-buffer buffer)
      (erase-buffer)

      (if groups
	  (message "Doing %s query on %s..." program groups)
	(message "Doing %s query..." program))
      (setq proc (apply #'start-process "search" buffer program cp-list))

      (accept-process-output proc)
      (setq exitstatus (process-exit-status proc))
      (if (zerop exitstatus)
	  ;; The search results have been put into the current buffer;
	  ;; `massage-output' finds them there.
	  (progn
	    (setq artlist (nnir-search-indexed-massage-output
			   engine server groups))

	    ;; Sort by score

	    (apply #'vector
		   (sort artlist
			 (function (lambda (x y)
				     (> (nnir-artitem-rsv x)
					(nnir-artitem-rsv y)))))))
	(nnheader-report 'nnir "%s error: %s" program exitstatus)
	;; Failure reason is in this buffer, show it if the user
	;; wants it.
	(when (> gnus-verbose 6)
	  (display-buffer buffer))))))

;; Swish++

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-swish++)
						(expr (head near)))
  (format "%s near %s"
	  (nnir-search-transform-expression engine (nth 1 expr))
	  (nnir-search-transform-expression engine (nth 2 expr))))

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-swish++)
						(expr list))
  (cond
   ((listp (car expr))
    (format "(%s)" (nnir-search-transform-top-level engine expr)))
   ;; Untested and likely wrong.
   ((and (stringp (cdr expr))
	 (string-prefix-p "(" (cdr expr)))
    (format "%s = %s" (car expr) (nnir-search-transform-top-level
				  engine
				  (nnir-search-parse-query (cdr expr)))))
   (t (format "%s = %s" (car expr) (cdr expr)))))

(cl-defmethod nnir-search-indexed-search-command ((engine gnus-search-swish++)
						  (qstring string))
  (with-slots (config-file switches) engine
   `("--config-file" ,config-file
     ,@switches
     ,qstring
     )))

(cl-defmethod nnir-search-indexed-massage-output ((engine gnus-search-swish++)
						  server &optional groups)
  (let ((groupspec (when groups
		     (regexp-opt
		      (mapcar
		       (lambda (x) (gnus-group-real-name x))
		       groups))))
	(prefix (slot-value engine 'prefix))
	(article-pattern (if (string-match "\\`nnmaildir:"
					   (gnus-group-server server))
			     ":[0-9]+"
			   "^[0-9]+\\(\\.[a-z0-9]+\\)?$"))
	filenam dirnam artno score artlist)
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
	    (nnir-add-result dirnam artno score prefix server artlist)))))))

;; Swish-e

;; I didn't do the query transformation for Swish-e, because the
;; program seems no longer to exist.

(cl-defmethod nnir-search-indexed-search-command ((engine gnus-search-swish-e)
						  (qstring string))
  (with-slots (index-files switches) engine
    `("-f" ,@index-files
      ,@switches
      "-w"
      ,qstring
      )))

(cl-defmethod nnir-search-indexed-massage-output ((engine gnus-search-swish-e)
						  server &optional _groups)
  (let ((prefix (slot-value engine 'prefix))
	group dirnam artno score artlist)
    (goto-char (point-min))
    (while (re-search-forward
            "\\(^[0-9]+\\) \\([^ ]+\\) \"\\([^\"]+\\)\" [0-9]+$" nil t)
      (setq score (match-string 1)
            artno (match-string 3)
            dirnam (file-name-directory (match-string 2)))
      (when (string-match "^[0-9]+$" artno)
          (when (not (null dirnam))

	    ;; remove nnir-swish-e-remove-prefix from beginning of dirname
            (when (string-match (concat "^" prefix) dirnam)
              (setq dirnam (replace-match "" t t dirnam)))

            (setq dirnam (substring dirnam 0 -1))
	    ;; eliminate all ".", "/", "\" from beginning. Always matches.
            (string-match "^[./\\]*\\(.*\\)$" dirnam)
            ;; "/" -> "."
            (setq group (replace-regexp-in-string
			 "/" "." (match-string 1 dirnam)))
            ;; Windows "\\" -> "."
            (setq group (replace-regexp-in-string "\\\\" "." group))

            (push (vector (gnus-group-full-name group server)
                          (string-to-number artno)
                          (string-to-number score))
                  artlist))))))

;; HyREX interface
(defun nnir-run-hyrex (query server &optional group)
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
      (set-buffer (get-buffer-create nnir-tmp-buffer))
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
                       (replace-regexp-in-string "/" "." dirnam) server)
		      (string-to-number artno)
		      (string-to-number score))
	      artlist))
      (message "Massaging hyrex-search output...done.")
      (apply #'vector
	     (sort artlist
                   (function (lambda (x y)
                               (if (string-lessp (nnir-artitem-group x)
                                                 (nnir-artitem-group y))
                                   t
                                 (< (nnir-artitem-number x)
                                    (nnir-artitem-number y)))))))
      )))

;; Namazu interface
(defun nnir-run-namazu (query server &optional _group)
  "Run given query against Namazu.  Returns a vector of (group name, file name)
pairs (also vectors, actually).

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
      (set-buffer (get-buffer-create nnir-tmp-buffer))
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
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

(defun nnir-run-notmuch (query server &optional _group)
  "Run QUERY against notmuch.
Returns a vector of (group name, file name) pairs (also vectors,
actually)."

  ;; (when group
  ;;   (error "The notmuch backend cannot search specific groups"))

  (save-excursion
    (let ( (qstring (cdr (assq 'query query)))
	   (groupspec (cdr (assq 'notmuch-group query)))
	   (prefix (nnir-read-server-parm 'nnir-notmuch-remove-prefix server))
           artlist
	   (article-pattern (if (string-match "\\`nnmaildir:"
					      (gnus-group-server server))
			       ":[0-9]+"
			     "^[0-9]+$"))
           artno dirnam filenam)

      (when (equal "" qstring)
        (error "notmuch: You didn't enter anything"))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (if groupspec
          (message "Doing notmuch query %s on %s..." qstring groupspec)
        (message "Doing notmuch query %s..." qstring))

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

;; Namazu interface

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-namazu)
						(expr list))
  (cond
   ((listp (car expr))
    (format "(%s)" (nnir-search-transform-top-level engine expr)))
   ;; I have no idea which fields namazu can handle.  Just do these
   ;; for now.
   ((memq (car expr) '(subject from to))
    (format "+%s:%s" (car expr) (cdr expr)))
   ((eq (car expr) 'id)
    (format "+message-id:%s" (cdr expr)))
   (t (ignore-errors (cl-call-next-method)))))

;; I can't tell if this is actually necessary.
(cl-defmethod nnir-run-search :around ((_e gnus-search-namazu)
				       _server _query _groups)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LC_MESSAGES" "C")
    (cl-call-next-method)))

(cl-defmethod search-indexed-search-command ((engine gnus-search-namazu)
					     (qstring string))
  (with-slots (switches index-dir) engine
   `("-q"				; don't be verbose
      "-a"				; show all matches
      "-s"				; use short format
      ,@switches
      ,qstring				; the query, in namazu format
      ,index-dir ; index directory
      )))

(cl-defmethod nnir-search-indexed-massage-output ((engine gnus-search-namazu)
						  server &optional groups)
  ;; Namazu output looks something like this:
  ;; 2. Re: Gnus agent expire broken (score: 55)
  ;; /home/henrik/Mail/mail/sent/1310 (4,138 bytes)

  (let ((article-pattern (if (string-match "\\'nnmaildir:"
					   (gnus-group-server server))
			     ":[0-9]+"
			   "^[0-9]+$"))
	(prefix (slot-value engine 'prefix))
	(group-regexp (when groups
			(regexp-opt
			 (mapcar
			  (lambda (x) (gnus-group-real-name x))
			  groups))))
	score group article artlist)
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\([0-9,]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
	    nil t)
      (setq score (match-string 3)
	    group (file-name-directory (match-string 4))
	    article (file-name-nondirectory (match-string 4)))

      ;; make sure article and group is sane
      (when (and (string-match article-pattern article)
		 (not (null group))
		 (or (null group-regexp)
		     (string-match-p group-regexp group)))
	(nnir-add-result group article score prefix server artlist)))
    artlist))

;;; Notmuch interface

(cl-defmethod nnir-search-transform-top-level ((_engine gnus-search-notmuch)
					       (_query null))
  "*")

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-notmuch)
						(expr (head near)))
  (format "%s near %s"
	  (nnir-search-transform-expression engine (nth 1 expr))
	  (nnir-search-transform-expression engine (nth 2 expr))))

(cl-defmethod nnir-search-transform-expression ((engine gnus-search-notmuch)
						(expr list))
  ;; Swap keywords as necessary.
  (cl-case (car expr)
    (sender (setcar expr 'from))
    (recipient (setcar expr 'to))
    (mark (setcar expr 'tag)))
  ;; Then actually format the results.
  (cl-flet ((notmuch-date (date)
			  (if (stringp date)
			      date
			    (pcase date
			      (`(nil ,m nil)
			       (nth (1- m) gnus-english-month-names))
			      (`(nil nil ,y)
			       (number-to-string y))
			      (`(,d ,m nil)
			       (format "%02d-%02d" d m))
			      (`(nil ,m ,y)
			       (format "%02d-%d" m y))
			      (`(,d ,m ,y)
			       (format "%d/%d/%d" m d y))))))
    (cond
     ((consp (car expr))
      (format "(%s)") (nnir-search-transform-top-level engine expr))
     ((memq (car expr) '(from to subject attachment mimetype tag id
			      thread folder path lastmod query property))
      (format "%s:%s" (car expr) (if (string-match-p " " (cdr expr))
				     (format "\"%s\"" (cdr expr))
				   (cdr expr))))
     ((eq (car expr) 'date)
      (format "date:%s" (notmuch-date (cdr expr))))
     ((eq (car expr) 'before)
      (format "date:..%s" (notmuch-date (cdr expr))))
     ((eq (car expr) 'since)
      (format "date:%s.." (notmuch-date (cdr expr))))
     (t (ignore-errors (cl-call-next-method))))))

(cl-defmethod nnir-search-indexed-search-command ((engine gnus-search-notmuch)
						  (qstring string)
						  &optional _groups)
  ;; Theoretically we could use the GROUPS parameter to pass a
  ;; --folder switch to notmuch, but I'm not confident of getting the
  ;; format right.
  (with-slots (switches config-file) engine
    `(,(format "--config=%s" config-file)
      "search"
      "--format=text"
      "--output=files"
      ,@switches
      ,qstring				; the query, in notmuch format
      )))

(cl-defmethod nnir-search-indexed-massage-output ((engine gnus-search-notmuch)
						  server &optional groups)
  ;; The results are output in the format of:
  ;; absolute-path-name
  (let ((article-pattern (if (string-match "\\`nnmaildir:"
					   (gnus-group-server server))
			     ":[0-9]+"
			   "^[0-9]+$"))
	(prefix (slot-value engine 'prefix))
	(group-regexp (when groups
			(regexp-opt
			 (mapcar
			  (lambda (x) (gnus-group-real-name x))
			  groups))))
	artno dirnam filenam artlist)
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

	  ;; maybe limit results to matching groups.
	  (when (or (not groups)
		    (string-match-p group-regexp dirnam))
	    (nnir-add-result dirnam artno "" prefix server artlist)))))
    artlist))

;;; Find-grep interface

(cl-defmethod nnir-run-search ((engine gnus-search-find-grep)
			       server query
			       &optional groups)
  "Run find and grep to obtain matching articles."
  (let* ((method (gnus-server-to-method server))
	 (sym (intern
	       (concat (symbol-name (car method)) "-directory")))
	 (directory (cadr (assoc sym (cddr method))))
	 (regexp (cdr (assoc 'query query)))
	 ;; `grep-options' will actually come out of the parsed query.
	 (grep-options (cdr (assoc 'grep-options query)))
	 (grouplist (or groups (nnir-get-active server)))
	 (buffer (slot-value engine 'proc-buffer)))
    (unless directory
      (error "No directory found in method specification of server %s"
	     server))
    (apply
     'vconcat
     (mapcar (lambda (x)
	       (let ((group x)
		     artlist)
		 (message "Searching %s using find-grep..."
			  (or group server))
		 (save-window-excursion
		   (set-buffer buffer)
		   (if (> gnus-verbose 6)
		       (pop-to-buffer (current-buffer)))
		   (cd directory)    ; Using relative paths simplifies
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
					   (replace-regexp-in-string
					    "\\." "/"
					    group nil t)))
				    group))))))
		     (unless group
		       (error "Cannot locate directory for group"))
		     (save-excursion
		       (apply
			'call-process "find" nil t
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
				    (line-end-position)) "/" t))
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

;; gmane interface
(cl-defmethod nnir-run-search ((engine gnus-search-gmane)
			       srv query &optional groups)
  "Run a search against a gmane back-end server."
      (let* ((case-fold-search t)
	     (qstring (cdr (assq 'query query)))
;	     (server (cadr (gnus-server-to-method srv)))
	     (groupspec (mapconcat
			 (lambda (x)
			   (if (string-match-p "gmane" x)
			       (format "group:%s" (gnus-group-short-name x))
			     (error "Can't search non-gmane groups: %s" x)))
			   groups " "))
	     (authorspec
	      (if (assq 'gmane-author query)
		  (format "author:%s" (cdr (assq 'gmane-author query))) ""))
	     (search (format "%s %s %s"
			     qstring groupspec authorspec))
	     (gnus-inhibit-demon t)
	     artlist)
	(require 'mm-url)
	(with-current-buffer (get-buffer-create nnir-tmp-buffer)
	  (erase-buffer)
	  (mm-url-insert
	   (concat
	    "http://search.gmane.org/nov.php"
	    "?"
	    (mm-url-encode-www-form-urlencoded
	     `(("query" . ,search)
	       ("HITSPERPAGE" . "999")))))
	  (set-buffer-multibyte t)
	  (decode-coding-region (point-min) (point-max) 'utf-8)
	  (goto-char (point-min))
	  (forward-line 1)
	  (while (not (eobp))
	    (unless (or (eolp) (looking-at "\x0d"))
	      (let ((header (nnheader-parse-nov)))
		(let ((xref (mail-header-xref header))
		      (xscore (string-to-number (cdr (assoc 'X-Score
			       (mail-header-extra header))))))
		  (when (string-match " \\([^:]+\\)[:/]\\([0-9]+\\)" xref)
		    (push
		     (vector
		      (gnus-group-prefixed-name (match-string 1 xref) srv)
		      (string-to-number (match-string 2 xref)) xscore)
		     artlist)))))
	    (forward-line 1)))
	(apply #'vector (nreverse (delete-dups artlist)))))

;;; Util Code:


(defun nnir-run-query (specs)
  "Invoke appropriate search engine function."
  ;; For now, run the searches synchronously.  At some point each
  ;; search can be run in its own thread, allowing concurrent searches
  ;; of multiple backends.  At present this causes problems when
  ;; multiple IMAP servers are searched at the same time, apparently
  ;; because the threads are somehow fighting for control, or the
  ;; `nntp-server-buffer' variable is getting clobbered, or something
  ;; else.
  (let* ((results [])
	 (q-spec (plist-get specs :nnir-query-spec))
	 (unparsed-query (plist-get q-spec :query))
	 (prepped-query (if (and nnir-use-parsed-queries
				 (null (plist-get q-spec :no-parse)))
			    (nnir-search-parse-query unparsed-query)
			  unparsed-query)))
    (mapc
     (lambda (x)
       (let* ((server (car x))
	      (search-engine (nnir-server-to-search-engine server))
	      (groups (cadr x))
	      (use-query (if (slot-value search-engine 'raw-queries-p)
			     unparsed-query
			   prepped-query)))
	 (setq results
	       (vconcat
		(nnir-run-search
		 search-engine server use-query groups)
		results))))
     (plist-get specs :nnir-group-spec))
    results))

;; This should be done once at Gnus startup time, when the servers are
;; first opened, and the resulting engine instance attached to the
;; server.
(defun nnir-server-to-search-engine (server)
  (let* ((server
	  (or (assoc 'nnir-search-engine
		     (cddr (gnus-server-to-method server)))
	      (assoc (car (gnus-server-to-method server))
		     nnir-method-default-engines)))
	 (inst
	  (cond
	   ((null server) nil)
	   ((eieio-object-p (cadr server))
	    (car server))
	   ((class-p (cadr server))
	    (make-instance (cadr server)))
	   (t nil))))
    (when inst
      (when (cddr server)
	(pcase-dolist (`(,key ,value) (cddr server))
	  (condition-case nil
	      (setf (slot-value inst key) value)
	    ((invalid-slot-name invalid-slot-type)
	     (nnheader-message
	      5 "Invalid search engine parameter: (%s %s)"
	      key value)))))
      inst)))

(defun nnir-read-server-parm (key server &optional not-global)
  "Returns the parameter value corresponding to `key' for
`server'. If no server-specific value is found consult the global
environment unless `not-global' is non-nil."
  (let ((method (gnus-server-to-method server)))
    (cond ((and method (assq key (cddr method)))
           (nth 1 (assq key (cddr method))))
          ((and (not not-global) (boundp key)) (symbol-value key))
          (t nil))))

(autoload 'gnus-request-list "gnus-int")

(defun nnir-get-active (srv)
  (let ((method (gnus-server-to-method srv))
	groups)
    (gnus-request-list method)
    (with-current-buffer nntp-server-buffer
      (let ((cur (current-buffer)))
	(goto-char (point-min))
	(unless (or (null nnir-ignored-newsgroups)
		    (string= nnir-ignored-newsgroups ""))
	  (delete-matching-lines nnir-ignored-newsgroups))
	(if (eq (car method) 'nntp)
	    (while (not (eobp))
	      (ignore-errors
		(push (gnus-group-decoded-name
		       (gnus-group-full-name
			(buffer-substring
			 (point)
			 (progn
			   (skip-chars-forward "^ \t")
			   (point)))
			method))
		      groups))
	      (forward-line))
	  (while (not (eobp))
	    (ignore-errors
	      (push (gnus-group-decoded-name
		     (if (eq (char-after) ?\")
			 (gnus-group-full-name (read cur) method)
		       (let ((p (point)) (name ""))
			 (skip-chars-forward "^ \t\\\\")
			 (setq name (buffer-substring p (point)))
			 (while (eq (char-after) ?\\)
			   (setq p (1+ (point)))
			   (forward-char 2)
			   (skip-chars-forward "^ \t\\\\")
			   (setq name (concat name (buffer-substring
						    p (point)))))
			 (gnus-group-full-name name method))))
		    groups))
	    (forward-line)))))
    groups))

(autoload 'nnselect-categorize "nnselect" nil nil)
(autoload 'gnus-group-topic-name "gnus-topic" nil nil)
(defvar gnus-group-marked)
(defvar gnus-topic-alist)

(defun nnir-make-specs (nnir-extra-parms &optional specs)
  (let* ((group-spec
	  (or (cdr (assq 'nnir-group-spec specs))
	      (if (gnus-server-server-name)
		  (list (list (gnus-server-server-name)))
		(nnselect-categorize
		 (or gnus-group-marked
		     (if (gnus-group-group-name)
			 (list (gnus-group-group-name))
		       (cdr (assoc (gnus-group-topic-name) gnus-topic-alist))))
		 'nnselect-group-server))))
	 (query-spec
	  (or (cdr (assq 'nnir-query-spec specs))
	      (apply
	       'append
	       (list (cons 'query
			   (read-string "Query: " nil 'nnir-search-history)))
	       (when nnir-extra-parms
		 (mapcar
		  (lambda (x)
		    (nnir-read-parms (nnir-server-to-search-engine (car x))))
		  group-spec))))))
    (list (cons 'nnir-query-spec query-spec)
	  (cons 'nnir-group-spec group-spec))))

;; The end.
(provide 'nnir)

;;; nnir.el ends here
