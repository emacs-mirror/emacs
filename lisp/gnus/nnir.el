;;; nnir.el --- search mail with various search engines -*- coding: iso-8859-1 -*-

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
;;   2007, 2008, 2009, 2010 Free Software Foundation, Inc.

;; Author: Kai Groﬂjohann <grossjohann@ls6.cs.uni-dortmund.de>
;; Swish-e and Swish++ backends by:
;;   Christoph Conrad <christoph.conrad@gmx.de>.
;; IMAP backend by: Simon Josefsson <jas@pdc.kth.se>.
;; IMAP search by: Torsten Hilbrich <torsten.hilbrich <at> gmx.net>
;; IMAP search improved by Daniel Pittman  <daniel@rimspace.net>.
;; nnmaildir support for Swish++ and Namazu backends by:
;;   Justus Piater <Justus <at> Piater.name>
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

;; TODO: Documentation in the Gnus manual

;; From: Reiner Steib
;; Subject: Re: Including nnir.el
;; Newsgroups: gmane.emacs.gnus.general
;; Message-ID: <v9d5dnp6aq.fsf@marauder.physik.uni-ulm.de>
;; Date: 2006-06-05 22:49:01 GMT
;;
;; On Sun, Jun 04 2006, Sascha Wilde wrote:
;;
;; > The one thing most hackers like to forget: Documentation.  By now the
;; > documentation is only in the comments at the head of the source, I
;; > would use it as basis to cook up some minimal texinfo docs.
;; >
;; > Where in the existing gnus manual would this fit best?

;; Maybe (info "(gnus)Combined Groups") for a general description.
;; `gnus-group-make-nnir-group' might be described in (info
;; "(gnus)Foreign Groups") as well.


;; The most recent version of this can always be fetched from the Gnus
;; repository.  See http://www.gnus.org/ for more information.

;; This code is still in the development stage but I'd like other
;; people to have a look at it.  Please do not hesitate to contact me
;; with your ideas.

;; What does it do?  Well, it allows you to index your mail using some
;; search engine (freeWAIS-sf, swish-e and others -- see later),
;; then type `G G' in the Group buffer and issue a query to the search
;; engine.  You will then get a buffer which shows all articles
;; matching the query, sorted by Retrieval Status Value (score).

;; When looking at the retrieval result (in the Summary buffer) you
;; can type `G T' (aka M-x gnus-summary-nnir-goto-thread RET) on an
;; article.  You will be teleported into the group this article came
;; from, showing the thread this article is part of.  (See below for
;; restrictions.)

;; The Lisp installation is simple: just put this file on your
;; load-path, byte-compile it, and load it from ~/.gnus or something.
;; This will install a new command `G G' in your Group buffer for
;; searching your mail.  Note that you also need to configure a number
;; of variables, as described below.

;; Restrictions:
;;
;; * This expects that you use nnml or another one-file-per-message backend,
;;   because the others doesn't support nnfolder.
;; * It can only search the mail backend's which are supported by one
;;   search engine, because of different query languages.
;; * There are restrictions to the Wais setup.
;; * There are restrictions to the imap setup.
;; * gnus-summary-nnir-goto-thread: Fetches whole group first, before
;;   limiting to the right articles.  This is much too slow, of
;;   course.  May issue a query for number of articles to fetch; you
;;   must accept the default of all articles at this point or things
;;   may break.

;; The Lisp setup involves setting a few variables and setting up the
;; search engine. You can define the variables in the server definition
;; like this :
;;   (setq gnus-secondary-select-methods '(
;;       (nnimap "" (nnimap-address "localhost")
;;                  (nnir-search-engine namazu)
;;       )))
;; Or you can define the global ones. The variables set in the mailer-
;; definition will be used first.
;; The variable to set is `nnir-search-engine'.  Choose one of the engines
;; listed in `nnir-engines'.  (Actually `nnir-engines' is an alist,
;; type `C-h v nnir-engines RET' for more information; this includes
;; examples for setting `nnir-search-engine', too.)
;;
;; The variable nnir-mail-backend isn't used anymore.
;;

;; You must also set up a search engine.  I'll tell you about the two
;; search engines currently supported:

;; 1. freeWAIS-sf
;;
;; As always with freeWAIS-sf, you need a so-called `format file'.  I
;; use the following file:
;;
;; ,-----
;; | # Kai's format file for freeWAIS-sf for indexing mails.
;; | # Each mail is in a file, much like the MH format.
;; |
;; | # Document separator should never match -- each file is a document.
;; | record-sep: /^@this regex should never match@$/
;; |
;; | # Searchable fields specification.
;; |
;; | region: /^[sS]ubject:/ /^[sS]ubject: */
;; |         subject "Subject header" stemming TEXT BOTH
;; | end: /^[^ \t]/
;; |
;; | region: /^([tT][oO]|[cC][cC]):/ /^([tT][oO]|[cC][cC]): */
;; |         to "To and Cc headers" SOUNDEX BOTH
;; | end: /^[^ \t]/
;; |
;; | region: /^[fF][rR][oO][mM]:/ /^[fF][rR][oO][mM]: */
;; |         from "From header" SOUNDEX BOTH
;; | end: /^[^ \t]/
;; |
;; | region: /^$/
;; |         stemming TEXT GLOBAL
;; | end: /^@this regex should never match@$/
;; `-----
;;
;; 1998-07-22: waisindex would dump core on me for large articles with
;; the above settings.  I used /^$/ as the end regex for the global
;; field.  That seemed to work okay.

;; There is a Perl module called `WAIS.pm' which is available from
;; CPAN as well as ls6-ftp.cs.uni-dortmund.de:/pub/wais/Perl.  This
;; module comes with a nifty tool called `makedb', which I use for
;; indexing.  Here's my `makedb.conf':
;;
;; ,-----
;; | # Config file for makedb
;; |
;; | # Global options
;; | waisindex = /usr/local/bin/waisindex
;; | wais_opt  = -stem -t fields
;; | # `-stem' option necessary when `stemming' is specified for the
;; | # global field in the *.fmt file
;; |
;; | # Own variables
;; | homedir = /home/kai
;; |
;; | # The mail database.
;; | database        = mail
;; | files           = `find $homedir/Mail -name \*[0-9] -print`
;; | dbdir           = $homedir/.wais
;; | limit           = 100
;; `-----
;;
;; The Lisp setup involves the `nnir-wais-*' variables.  The most
;; difficult to understand variable is probably
;; `nnir-wais-remove-prefix'.  Here's what it does: the output of
;; `waissearch' basically contains the file name and the (full)
;; directory name.  As Gnus works with group names rather than
;; directory names, the directory name is transformed into a group
;; name as follows: first, a prefix is removed from the (full)
;; directory name, then all `/' are replaced with `.'.  The variable
;; `nnir-wais-remove-prefix' should contain a regex matching exactly
;; this prefix.  It defaults to `$HOME/Mail/' (note the trailing
;; slash).

;; 2. Namazu
;;
;; The Namazu backend requires you to have one directory containing all
;; index files, this is controlled by the `nnir-namazu-index-directory'
;; variable.  To function the `nnir-namazu-remove-prefix' variable must
;; also be correct, see the documentation for `nnir-wais-remove-prefix'
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

;; 3. find-grep
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

;; Developer information:

;; I have tried to make the code expandable.  Basically, it is divided
;; into two layers.  The upper layer is somewhat like the `nnvirtual'
;; backend: given a specification of what articles to show from
;; another backend, it creates a group containing exactly those
;; articles.  The lower layer issues a query to a search engine and
;; produces such a specification of what articles to show from the
;; other backend.

;; The interface between the two layers consists of the single
;; function `nnir-run-query', which just selects the appropriate
;; function for the search engine one is using.  The input to
;; `nnir-run-query' is a string, representing the query as input by
;; the user.  The output of `nnir-run-query' is supposed to be a
;; vector, each element of which should in turn be a three-element
;; vector.  The first element should be full group name of the article,
;; the second element should be the article number, and the third
;; element should be the Retrieval Status Value (RSV) as returned from
;; the search engine.  An RSV is the score assigned to the document by
;; the search engine.  For Boolean search engines, the
;; RSV is always 1000 (or 1 or 100, or whatever you like).

;; The sorting order of the articles in the summary buffer created by
;; nnir is based on the order of the articles in the above mentioned
;; vector, so that's where you can do the sorting you'd like.  Maybe
;; it would be nice to have a way of displaying the search result
;; sorted differently?

;; So what do you need to do when you want to add another search
;; engine?  You write a function that executes the query.  Temporary
;; data from the search engine can be put in `nnir-tmp-buffer'.  This
;; function should return the list of articles as a vector, as
;; described above.  Then, you need to register this backend in
;; `nnir-engines'.  Then, users can choose the backend by setting
;; `nnir-search-engine'.

;; Todo, or future ideas:

;; * It should be possible to restrict search to certain groups.
;;
;; * There is currently no error checking.
;;
;; * The summary buffer display is currently really ugly, with all the
;;   added information in the subjects.  How could I make this
;;   prettier?
;;
;; * A function which can be called from an nnir summary buffer which
;;   teleports you into the group the current article came from and
;;   shows you the whole thread this article is part of.
;;   Implementation suggestions?
;;   (1998-07-24: There is now a preliminary implementation, but
;;   it is much too slow and quite fragile.)
;;
;; * Support other mail backends.  In particular, probably quite a few
;;   people use nnfolder.  How would one go about searching nnfolders
;;   and producing the right data needed?  The group name and the RSV
;;   are simple, but what about the article number?
;;   - The article number is encoded in the `X-Gnus-Article-Number'
;;     header of each mail.
;;
;; * Support compressed mail files.  Probably, just stripping off the
;;   `.gz' or `.Z' file name extension is sufficient.
;;
;; * At least for imap, the query is performed twice.
;;

;; Have you got other ideas?

;;; Setup Code:

(require 'nnoo)
(require 'gnus-group)
(require 'gnus-sum)
(require 'message)
(require 'gnus-util)
(eval-when-compile
  (require 'cl))


(eval-when-compile
  (autoload 'nnimap-buffer "nnimap")
  (autoload 'nnimap-command "nnimap")
  (autoload 'nnimap-possibly-change-group "nnimap"))

(nnoo-declare nnir)
(nnoo-define-basics nnir)

(gnus-declare-backend "nnir" 'mail)

(defvar nnir-imap-default-search-key "Whole message"
  "The default IMAP search key for an nnir search. Must be one of
  the keys in nnir-imap-search-arguments. To use raw imap queries
  by default set this to \"Imap\"")

(defvar nnir-imap-search-arguments
  '(("Whole message" . "TEXT")
    ("Subject" . "SUBJECT")
    ("To" . "TO")
    ("From" . "FROM")
    ("Imap" . ""))
  "Mapping from user readable keys to IMAP search items for use in nnir")

(defvar nnir-imap-search-other "HEADER %S"
  "The IMAP search item to use for anything other than
  nnir-imap-search-arguments. By default this is the name of an
  email header field")

(defvar nnir-imap-search-argument-history ()
  "The history for querying search options in nnir")

(defvar nnir-get-article-nov-override-function nil
  "If non-nil, a function that will be passed each search result.  This
should return a message's headers in NOV format.

If this variable is nil, or if the provided function returns nil for a search
result, `gnus-retrieve-headers' will be called instead.")

(defvar nnir-method-default-engines
  '((nnimap . imap)
    (nntp . nil))
  "Alist of default search engines by server method")

;;; Developer Extension Variable:

(defvar nnir-engines
  `((wais    nnir-run-waissearch
             ())
    (imap    nnir-run-imap
             ((criteria
	       "Search in"                        ; Prompt
	       ,(mapcar 'car nnir-imap-search-arguments) ; alist for completing
	       nil                                ; allow any user input
	       nil                                ; initial value
	       nnir-imap-search-argument-history  ; the history to use
	       ,nnir-imap-default-search-key      ; default
	       )))
    (swish++ nnir-run-swish++
             ((group . "Group spec: ")))
    (swish-e nnir-run-swish-e
             ((group . "Group spec: ")))
    (namazu  nnir-run-namazu
             ())
    (find-grep nnir-run-find-grep
	       ((grep-options . "Grep options: "))))
  "Alist of supported search engines.
Each element in the alist is a three-element list (ENGINE FUNCTION ARGS).
ENGINE is a symbol designating the searching engine.  FUNCTION is also
a symbol, giving the function that does the search.  The third element
ARGS is a list of cons pairs (PARAM . PROMPT).  When issuing a query,
the FUNCTION will issue a query for each of the PARAMs, using PROMPT.

The value of `nnir-search-engine' must be one of the ENGINE symbols.
For example, use the following line for searching using freeWAIS-sf:
    (setq nnir-search-engine 'wais)
Use the following line if you read your mail via IMAP and your IMAP
server supports searching:
    (setq nnir-search-engine 'imap)
Note that you have to set additional variables for most backends.  For
example, the `wais' backend needs the variables `nnir-wais-program',
`nnir-wais-database' and `nnir-wais-remove-prefix'.

Add an entry here when adding a new search engine.")

;;; User Customizable Variables:

(defgroup nnir nil
  "Search nnmh and nnml groups in Gnus with swish-e, freeWAIS-sf, or EWS."
  :group 'gnus)

;; Mail backend.

;; TODO:
;; If `nil', use server parameters to find out which server to search. CCC
;;
(defcustom nnir-mail-backend '(nnml "")
  "*Specifies which backend should be searched.
More precisely, this is used to determine from which backend to fetch the
messages found.

This must be equal to an existing server, so maybe it is best to use
something like the following:
    (setq nnir-mail-backend (nth 0 gnus-secondary-select-methods))
The above line works fine if the mail backend you want to search is
the first element of gnus-secondary-select-methods (`nth' starts counting
at zero)."
  :type '(sexp)
  :group 'nnir)

;; Search engine to use.

(defcustom nnir-search-engine 'wais
  "*The search engine to use.  Must be a symbol.
See `nnir-engines' for a list of supported engines, and for example
settings of `nnir-search-engine'."
  :type '(sexp)
  :group 'nnir)

;; freeWAIS-sf.

(defcustom nnir-wais-program "waissearch"
  "*Name of waissearch executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-wais-database (expand-file-name "~/.wais/mail")
  "*Name of Wais database containing the mail.

Note that this should be a file name without extension.  For example,
if you have a file /home/john/.wais/mail.fmt, use this:
    (setq nnir-wais-database \"/home/john/.wais/mail\")
The string given here is passed to `waissearch -d' as-is."
  :type '(file)
  :group 'nnir)

(defcustom nnir-wais-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each directory name returned by waissearch
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

For example, suppose that Wais returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-wais-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(regexp)
  :group 'nnir)

(defcustom nnir-swish++-configuration-file
  (expand-file-name "~/Mail/swish++.conf")
  "*Configuration file for swish++."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish++-program "search"
  "*Name of swish++ search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish++-additional-switches '()
  "*A list of strings, to be given as additional arguments to swish++.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-swish++-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish++-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by swish++
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-wais-remove-prefix', except
that it is for swish++, not Wais."
  :type '(regexp)
  :group 'nnir)

;; Swish-E.
;; URL: http://swish-e.org/
;; Variables `nnir-swish-e-index-file', `nnir-swish-e-program' and
;; `nnir-swish-e-additional-switches'

(make-obsolete-variable 'nnir-swish-e-index-file
			'nnir-swish-e-index-files "Emacs 23.1")
(defcustom nnir-swish-e-index-file
  (expand-file-name "~/Mail/index.swish-e")
  "*Index file for swish-e.
This could be a server parameter.
It is never consulted once `nnir-swish-e-index-files', which should be
used instead, has been customized."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish-e-index-files
  (list nnir-swish-e-index-file)
  "*List of index files for swish-e.
This could be a server parameter."
  :type '(repeat (file))
  :group 'nnir)

(defcustom nnir-swish-e-program "swish-e"
  "*Name of swish-e search executable.
This cannot be a server parameter."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish-e-additional-switches '()
  "*A list of strings, to be given as additional arguments to swish-e.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-swish-e-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish-e-additional-switches '(\"-i\" \"-w\"))

This could be a server parameter."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by swish-e
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-wais-remove-prefix', except
that it is for swish-e, not Wais.

This could be a server parameter."
  :type '(regexp)
  :group 'nnir)

;; Namazu engine, see <URL:http://www.namazu.org/>

(defcustom nnir-namazu-program "namazu"
  "*Name of Namazu search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "*Index directory for Namazu."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-namazu-additional-switches '()
  "*A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other switches
make any sense in this context.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-namazu-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-namazu-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-namazu-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by Namazu
in order to get a group name (albeit with / instead of .).

This variable is very similar to `nnir-wais-remove-prefix', except
that it is for Namazu, not Wais."
  :type '(directory)
  :group 'nnir)

;;; Internal Variables:

(defvar nnir-current-query nil
  "Internal: stores current query (= group name).")

(defvar nnir-current-server nil
  "Internal: stores current server (does it ever change?).")

(defvar nnir-current-group-marked nil
  "Internal: stores current list of process-marked groups.")

(defvar nnir-artlist nil
  "Internal: stores search result.")

(defvar nnir-tmp-buffer " *nnir*"
  "Internal: temporary buffer.")

;;; Code:

;; Gnus glue.

(defun gnus-group-make-nnir-group (extra-parms query)
  "Create an nnir group.  Asks for query."
  (interactive "P\nsQuery: ")
  (setq nnir-current-query nil
	nnir-current-server nil
	nnir-current-group-marked nil
	nnir-artlist nil)
  (let ((parms nil))
    (if extra-parms
        (setq parms (nnir-read-parms query))
      (setq parms (list (cons 'query query))))
    (add-to-list 'parms (cons 'unique-id (message-unique-id)) t)
    (gnus-group-read-ephemeral-group
     (concat "nnir:" (prin1-to-string parms)) '(nnir "") t
     (cons (current-buffer)
           gnus-current-window-configuration)
     nil)))

;; Why is this needed? Is this for compatibility with old/new gnusae? Using
;; gnus-group-server instead works for me.  -- Justus Piater
(defmacro nnir-group-server (group)
  "Return the server for a newsgroup GROUP.
The returned format is as `gnus-server-to-method' needs it.  See
`gnus-group-real-prefix' and `gnus-group-real-name'."
  `(let ((gname ,group))
     (if (string-match "^\\([^:]+\\):" gname)
	 (progn
	   (setq gname (match-string 1 gname))
	   (if (string-match "^\\([^+]+\\)\\+\\(.+\\)$" gname)
	       (format "%s:%s" (match-string 1 gname) (match-string 2 gname))
	     (concat gname ":")))
       (format "%s:%s" (car gnus-select-method) (cadr gnus-select-method)))))

;; Summary mode commands.

(defun gnus-summary-nnir-goto-thread ()
  "Only applies to nnir groups.  Go to group this article came from
and show thread that contains this article."
  (interactive)
  (unless (eq 'nnir (car (gnus-find-method-for-group gnus-newsgroup-name)))
    (error "Can't execute this command unless in nnir group"))
  (let* ((cur (gnus-summary-article-number))
         (group (nnir-artlist-artitem-group nnir-artlist cur))
         (backend-number (nnir-artlist-artitem-number nnir-artlist cur))
	 (id (mail-header-id (gnus-summary-article-header)))
	 (refs (split-string
		(mail-header-references (gnus-summary-article-header)))))
    (if (eq (car (gnus-group-method group)) 'nnimap)
	(progn (nnimap-possibly-change-group (gnus-group-short-name group) nil)
	       (with-current-buffer (nnimap-buffer)
		 (let* ((cmd (let ((value (format
					   "(OR HEADER REFERENCES %s HEADER Message-Id %s)"
					   id id)))
			       (dolist (refid refs value)
				 (setq value (format
					      "(OR (OR HEADER Message-Id %s HEADER REFERENCES %s) %s)"
					      refid refid value)))))
			(result (nnimap-command
				 "UID SEARCH %s" cmd)))
		   (gnus-summary-read-group-1 group t t gnus-summary-buffer nil
					      (and (car result)
						   (delete 0 (mapcar #'string-to-number
								     (cdr (assoc "SEARCH" (cdr result))))))))))
      (gnus-summary-read-group-1 group t t gnus-summary-buffer
				 nil (list backend-number))
      (gnus-summary-limit (list backend-number))
      (gnus-summary-refer-thread))))


(if (fboundp 'eval-after-load)
    (eval-after-load "gnus-sum"
      '(define-key gnus-summary-goto-map
         "T" 'gnus-summary-nnir-goto-thread))
  (add-hook 'gnus-summary-mode-hook
            (function (lambda ()
                        (define-key gnus-summary-goto-map
                          "T" 'gnus-summary-nnir-goto-thread)))))



;; Gnus backend interface functions.

(deffoo nnir-open-server (server &optional definitions)
  ;; Just set the server variables appropriately.
  (nnoo-change-server 'nnir server definitions))

(deffoo nnir-request-group (group &optional server fast info)
  "GROUP is the query string."
  (nnir-possibly-change-server server)
  ;; Check for cache and return that if appropriate.
  (if (and (equal group nnir-current-query)
           (equal gnus-group-marked nnir-current-group-marked)
           (or (null server)
               (equal server nnir-current-server)))
      nnir-artlist
    ;; Cache miss.
    (setq nnir-artlist (nnir-run-query group)))
  (with-current-buffer nntp-server-buffer
    (if (zerop (length nnir-artlist))
	(progn
	  (setq nnir-current-query nil
		nnir-current-server nil
		nnir-current-group-marked nil
		nnir-artlist nil)
	  (nnheader-report 'nnir "Search produced empty results."))
      ;; Remember data for cache.
      (setq nnir-current-query group)
      (when server (setq nnir-current-server server))
      (setq nnir-current-group-marked gnus-group-marked)
      (nnheader-insert "211 %d %d %d %s\n"
		       (nnir-artlist-length nnir-artlist) ; total #
		       1              ; first #
		       (nnir-artlist-length nnir-artlist) ; last #
		       group))))     ; group name

(deffoo nnir-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (let ((artlist (copy-sequence articles))
          art artitem artgroup artno artrsv artfullgroup
          novitem novdata foo server)
      (while (not (null artlist))
        (setq art (car artlist))
        (or (numberp art)
            (nnheader-report
             'nnir
             "nnir-retrieve-headers doesn't grok message ids: %s"
             art))
        (setq artitem (nnir-artlist-article nnir-artlist art))
        (setq artrsv (nnir-artitem-rsv artitem))
        (setq artfullgroup (nnir-artitem-group artitem))
        (setq artno (nnir-artitem-number artitem))
        (setq artgroup (gnus-group-real-name artfullgroup))
	(setq server (nnir-group-server artfullgroup))
        ;; retrieve NOV or HEAD data for this article, transform into
        ;; NOV data and prepend to `novdata'
        (set-buffer nntp-server-buffer)
	(nnir-possibly-change-server server)
        (let ((gnus-override-method
	       (gnus-server-to-method server)))
	  ;; if nnir-get-article-nov-override-function is set, use it
	  (if nnir-get-article-nov-override-function
	      (setq novitem (funcall nnir-get-article-nov-override-function
				     artitem))
	    ;; else, set novitem through nnheader-parse-nov/nnheader-parse-head
	    (case (setq foo (gnus-retrieve-headers (list artno)
						   artfullgroup nil))
	      (nov
	       (goto-char (point-min))
	       (setq novitem (nnheader-parse-nov)))
	      (headers
	       (goto-char (point-min))
	       (setq novitem (nnheader-parse-head)))
	      (t (error "Unknown header type %s while requesting article %s of group %s"
			foo artno artfullgroup)))))
	;; replace article number in original group with article number
        ;; in nnir group
	(when novitem
	  (mail-header-set-number novitem art)
	  (mail-header-set-from novitem
				(mail-header-from novitem))
	  (mail-header-set-subject
	   novitem
	   (format "[%d: %s/%d] %s"
		   artrsv artgroup artno
		   (mail-header-subject novitem)))
	  (push novitem novdata)
	  (setq artlist (cdr artlist))))
      (setq novdata (nreverse novdata))
      (set-buffer nntp-server-buffer) (erase-buffer)
      (mapc 'nnheader-insert-nov novdata)
      'nov)))

(deffoo nnir-request-article (article
                              &optional group server to-buffer)
  (if (stringp article)
      (nnheader-report
       'nnir
       "nnir-retrieve-headers doesn't grok message ids: %s"
       article)
    (save-excursion
      (let* ((artitem (nnir-artlist-article nnir-artlist
					    article))
	     (artfullgroup (nnir-artitem-group artitem))
	     (artno (nnir-artitem-number artitem))
	     ;; Bug?
	     ;; Why must we bind nntp-server-buffer here?  It won't
	     ;; work if `buf' is used, say.  (Of course, the set-buffer
	     ;; line below must then be updated, too.)
	     (nntp-server-buffer (or to-buffer nntp-server-buffer)))
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(message "Requesting article %d from group %s"
		 artno artfullgroup)
	(gnus-request-article artno artfullgroup nntp-server-buffer)
	(cons artfullgroup artno)))))


(nnoo-define-skeleton nnir)


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

  (when (file-readable-p (concat prefix dirnam article))
    ;; remove trailing slash and, for nnmaildir, cur/new/tmp
    (setq dirnam
	  (substring dirnam 0
		     (if (string= (gnus-group-server server) "nnmaildir")
			 -5 -1)))

    ;; Set group to dirnam without any leading dots or slashes,
    ;; and with all subsequent slashes replaced by dots
    (let ((group (gnus-replace-in-string
                 (gnus-replace-in-string dirnam "^[./\\]" "" t)
                 "[/\\]" "." t)))

    (vector (nnir-group-full-name group server)
	    (if (string= (gnus-group-server server) "nnmaildir")
		(nnmaildir-base-name-to-article-number
		 (substring article 0 (string-match ":" article))
		 group nil)
	      (string-to-number article))
	    (string-to-number score)))))

;;; Search Engine Interfaces:

;; freeWAIS-sf interface.
(defun nnir-run-waissearch (query server &optional group)
  "Run given query agains waissearch.  Returns vector of (group name, file name)
pairs (also vectors, actually)."
  (when group
    (error "The freeWAIS-sf backend cannot search specific groups"))
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-wais-remove-prefix server))
          artlist score artno dirnam)
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing WAIS query %s..." query)
      (call-process nnir-wais-program
                    nil                 ; input from /dev/null
                    t                   ; output to current buffer
                    nil                 ; don't redisplay
                    "-d" (nnir-read-server-parm 'nnir-wais-database server) ; database to search
                    qstring)
      (message "Massaging waissearch output...")
      ;; remove superfluous lines
      (keep-lines "Score:")
      ;; extract data from result lines
      (goto-char (point-min))
      (while (re-search-forward
              "Score: +\\([0-9]+\\).*'\\([0-9]+\\) +\\([^']+\\)/'" nil t)
        (setq score (match-string 1)
              artno (match-string 2)
              dirnam (match-string 3))
        (unless (string-match prefix dirnam)
          (nnheader-report 'nnir "Dir name %s doesn't contain prefix %s"
                           dirnam prefix))
        (setq group (gnus-replace-in-string
                     (replace-match "" t t dirnam) "/" "."))
        (push (vector (nnir-group-full-name group server)
                      (string-to-number artno)
                      (string-to-number score))
              artlist))
      (message "Massaging waissearch output...done")
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

;; IMAP interface.
;; todo:
;; send queries as literals
;; handle errors


(defun nnir-run-imap (query srv &optional group-option)
  "Run a search against an IMAP back-end server.
This uses a custom query language parser; see `nnir-imap-make-query' for
details on the language and supported extensions"
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (server (cadr (gnus-server-to-method srv)))
	  (group (or group-option (gnus-group-group-name)))
	  (defs (caddr (gnus-server-to-method srv)))
	  (criteria (or (cdr (assq 'criteria query))
			(cdr (assoc nnir-imap-default-search-key
				    nnir-imap-search-arguments))))
	  (gnus-inhibit-demon t)
	  artlist)
      (message "Opening server %s" server)
      (condition-case ()
	  (when (nnimap-possibly-change-group (gnus-group-short-name group) server)
	    (with-current-buffer (nnimap-buffer)
	      (message "Searching %s..." group)
	      (let ((arts 0)
		    (result
		     (nnimap-command "UID SEARCH %s"
				     (if (string= criteria "")
					 qstring
				       (nnir-imap-make-query criteria qstring)
				       ))))
		(mapc
		 (lambda (artnum)
		   (push (vector group artnum 1) artlist)
		   (setq arts (1+ arts)))
		 (and (car result)
		      (delete 0 (mapcar #'string-to-number
					(cdr (assoc "SEARCH" (cdr result)))))))
		(message "Searching %s... %d matches" group arts)))
	    (message "Searching %s...done" group))
	(quit nil))
      (reverse artlist))))

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
	    (nnir-imap-expr-to-imap criteria (second expr))
	    (nnir-imap-expr-to-imap criteria (third expr))))
   ;; Composite term: just the fax, mam
   ((eq (car-safe expr) 'not)
    (format "NOT (%s)" (nnir-imap-query-to-imap criteria (rest expr))))
   ;; Composite term: just expand it all.
   ((and (not (null expr)) (listp expr))
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
      (while (not (nnir-imap-end-of-input))
	(push (nnir-imap-next-expr) out))
      (reverse out))))


(defun nnir-imap-next-expr (&optional count)
  "Return the next expression from the current buffer."
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
  "Return the next TERM from the current buffer."
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
  "Return the next symbol from the current buffer, or nil if we are
at the end of the buffer.  If supplied COUNT skips some symbols before
returning the one at the supplied position."
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
  "Return a delimited string from the current buffer."
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
  (skip-chars-forward "[[:blank:]]")
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
(defun nnir-run-swish++ (query server &optional group)
  "Run QUERY against swish++.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish++ 4.7 on GNU/Linux and with swish++ 5.0b2 on
Windows NT 4.0."

  (when group
    (error "The swish++ backend cannot search specific groups"))

  (save-excursion
    (let ( (qstring (cdr (assq 'query query)))
	   (groupspec (cdr (assq 'group query)))
	   (prefix (nnir-read-server-parm 'nnir-swish++-remove-prefix server))
           artlist
	   ;; nnml-use-compressed-files might be any string, but probably this
	   ;; is sufficient.  Note that we can't only use the value of
	   ;; nnml-use-compressed-files because old articles might have been
	   ;; saved with a different value.
	   (article-pattern (if (string= (gnus-group-server server) "nnmaildir")
				":[0-9]+"
			      "^[0-9]+\\(\\.[a-z0-9]+\\)?$"))
           score artno dirnam filenam)

      (when (equal "" qstring)
        (error "swish++: You didn't enter anything"))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
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
                         (mapconcat 'identity (cddddr cp-list) " ")) ;; ???
                (apply 'call-process cp-list))))
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
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

;; Swish-E interface.
(defun nnir-run-swish-e (query server &optional group)
  "Run given query against swish-e.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish-e-2.0.1 on Windows NT 4.0."

  ;; swish-e crashes with empty parameter to "-w" on commandline...
  (when group
    (error "The swish-e backend cannot search specific groups"))

  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix
	   (or (nnir-read-server-parm 'nnir-swish-e-remove-prefix server)
	       (error "Missing parameter `nnir-swish-e-remove-prefix'")))
          artlist score artno dirnam group )

      (when (equal "" qstring)
        (error "swish-e: You didn't enter anything"))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
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
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
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
            (setq group (gnus-replace-in-string (match-string 1 dirnam) "/" "."))
            ;; Windows "\\" -> "."
            (setq group (gnus-replace-in-string group "\\\\" "."))

            (push (vector (nnir-group-full-name group server)
                          (string-to-number artno)
                          (string-to-number score))
                  artlist))))

      (message "Massaging swish-e output...done")

      ;; Sort by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

;; Namazu interface
(defun nnir-run-namazu (query server &optional group)
  "Run given query against Namazu.  Returns a vector of (group name, file name)
pairs (also vectors, actually).

Tested with Namazu 2.0.6 on a GNU/Linux system."
  (when group
    (error "The Namazu backend cannot search specific groups"))
  (save-excursion
    (let ((article-pattern (if (string= (gnus-group-server server) "nnmaildir")
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
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
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
              "^\\([0-9]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
              nil t)
        (setq score (match-string 3)
              group (file-name-directory (match-string 4))
              article (file-name-nondirectory (match-string 4)))

        ;; make sure article and group is sane
        (when (and (string-match article-pattern article)
                   (not (null group)))
	  (nnir-add-result group article score prefix server artlist)))

      ;; sort artlist by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

(defun nnir-run-find-grep (query server &optional group)
  "Run find and grep to obtain matching articles."
  (let* ((method (gnus-server-to-method server))
	 (sym (intern
	       (concat (symbol-name (car method)) "-directory")))
	 (directory (cadr (assoc sym (cddr method))))
	 (regexp (cdr (assoc 'query query)))
	 (grep-options (cdr (assoc 'grep-options query)))
	 artlist)
    (unless directory
      (error "No directory found in method specification of server %s"
	     server))
    (message "Searching %s using find-grep..." (or group server))
    (save-window-excursion
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (if (> gnus-verbose 6)
	  (pop-to-buffer (current-buffer)))
      (cd directory) ; Using relative paths simplifies postprocessing.
      (let ((group
	     (if (not group)
		 "."
	       ;; Try accessing the group literally as well as
	       ;; interpreting dots as directory separators so the
	       ;; engine works with plain nnml as well as the Gnus Cache.
               (let ((group (gnus-group-real-name group)))
                 ;; Replace cl-func find-if.
                 (if (file-directory-p group)
                     group
                   (if (file-directory-p
                        (setq group (gnus-replace-in-string group "\\." "/" t)))
                       group))))))
	(unless group
	  (error "Cannot locate directory for group"))
	(save-excursion
	  (apply
	   'call-process "find" nil t
	   "find" group "-type" "f" "-name" "[0-9]*" "-exec"
	   "grep"
	   `("-l" ,@(and grep-options
			 (split-string grep-options "\\s-" t))
	     "-e" ,regexp "{}" "+"))))

      ;; Translate relative paths to group names.
      (while (not (eobp))
	(let* ((path (split-string
		      (buffer-substring (point) (line-end-position)) "/" t))
	       (art (string-to-number (car (last path)))))
	  (while (string= "." (car path))
	    (setq path (cdr path)))
	  (let ((group (mapconcat 'identity
                                  ;; Replace cl-func: (subseq path 0 -1)
                                  (let ((end (1- (length path)))
                                        res)
                                    (while (>= (setq end (1- end)) 0)
                                      (push (pop path) res))
                                    (nreverse res))
                                  ".")))
	    (push (vector (nnir-group-full-name group server) art 0)
		  artlist))
	  (forward-line 1)))
      (message "Searching %s using find-grep...done" (or group server))
      artlist)))

;;; Util Code:

(defun nnir-read-parms (query)
  "Reads additional search parameters according to `nnir-engines'."
  (let ((parmspec (caddr (assoc nnir-search-engine nnir-engines))))
    (cons (cons 'query query)
          (mapcar 'nnir-read-parm parmspec))))

(defun nnir-read-parm (parmspec)
  "Reads a single search parameter.
`parmspec' is a cons cell, the car is a symbol, the cdr is a prompt."
  (let ((sym (car parmspec))
        (prompt (cdr parmspec)))
    (if (listp prompt)
	(let* ((result (apply 'gnus-completing-read prompt))
	       (mapping (or (assoc result nnir-imap-search-arguments)
			    (cons nil nnir-imap-search-other))))
	  (cons sym (format (cdr mapping) result)))
      (cons sym (read-string prompt)))))

(defun nnir-run-query (query)
  "Invoke appropriate search engine function (see `nnir-engines').
If some groups were process-marked, run the query for each of the groups
and concat the results."
  (let ((q (car (read-from-string query))))
    (if gnus-group-marked
	(apply 'vconcat
	       (mapcar (lambda (x)
			 (let* ((server (nnir-group-server x))
				(engine
				 (or (nnir-read-server-parm 'nnir-search-engine
							    server)
				     (cdr
				      (assoc (car (gnus-server-to-method server))
					     nnir-method-default-engines))))
				search-func)
			   (setq search-func (cadr
					      (assoc
					       engine
					       nnir-engines)))
			   (if search-func
			       (funcall search-func q server x)
			     nil)))
		       gnus-group-marked))
      (apply 'vconcat
	     (mapcar (lambda (x)
		       (if (and (equal (cadr x) 'ok) (not (equal (cadar x) "-ephemeral")))
			   (let* ((server (format "%s:%s" (caar x) (cadar x)))
				  (engine
				   (or (nnir-read-server-parm 'nnir-search-engine
							      server)
				       (cdr
					(assoc (car (gnus-server-to-method server))
					       nnir-method-default-engines))))
				  search-func)
			     (setq search-func (cadr
						(assoc
						 engine
						 nnir-engines)))
			     (if search-func
				 (funcall search-func q server nil)
			       nil))
			 nil))
		     gnus-opened-servers)
	     ))
    ))

(defun nnir-read-server-parm (key server)
  "Returns the parameter value of for the given server, where server is of
form 'backend:name'."
  (let ((method (gnus-server-to-method server)))
    (cond ((and method (assq key (cddr method)))
	   (nth 1 (assq key (cddr method))))
	  ((and nnir-mail-backend
		(gnus-server-equal method nnir-mail-backend))
	   (symbol-value key))
	  (t nil))))
;;     (if method
;;       (if (assq key (cddr method))
;; 	  (nth 1 (assq key (cddr method)))
;; 	(symbol-value key))
;;       (symbol-value key))
;;     ))

(defun nnir-group-full-name (shortname server)
  "For the given group name, return a full Gnus group name.
The Gnus backend/server information is added."
  (gnus-group-prefixed-name shortname (gnus-server-to-method server)))

(defun nnir-possibly-change-server (server)
  (unless (and server (nnir-server-opened server))
    (nnir-open-server server)))


;; Data type article list.

(defun nnir-artlist-length (artlist)
  "Returns number of articles in artlist."
  (length artlist))

(defun nnir-artlist-article (artlist n)
  "Returns from ARTLIST the Nth artitem (counting starting at 1)."
  (elt artlist (1- n)))

(defun nnir-artitem-group (artitem)
  "Returns the group from the ARTITEM."
  (elt artitem 0))

(defun nnir-artlist-artitem-group (artlist n)
  "Returns from ARTLIST the group of the Nth artitem (counting from 1)."
  (nnir-artitem-group (nnir-artlist-article artlist n)))

(defun nnir-artitem-number (artitem)
  "Returns the number from the ARTITEM."
  (elt artitem 1))

(defun nnir-artlist-artitem-number (artlist n)
  "Returns from ARTLIST the number of the Nth artitem (counting from 1)."
  (nnir-artitem-number (nnir-artlist-article artlist n)))

(defun nnir-artitem-rsv (artitem)
  "Returns the Retrieval Status Value (RSV, score) from the ARTITEM."
  (elt artitem 2))

(defun nnir-artlist-artitem-rsv (artlist n)
  "Returns from ARTLIST the Retrieval Status Value of the Nth artitem
\(counting from 1)."
  (nnir-artitem-rsv (nnir-artlist-article artlist n)))

;; unused?
(defun nnir-artlist-groups (artlist)
  "Returns a list of all groups in the given ARTLIST."
  (let ((res nil)
        (with-dups nil))
    ;; from each artitem, extract group component
    (setq with-dups (mapcar 'nnir-artitem-group artlist))
    ;; remove duplicates from above
    (mapc (function (lambda (x) (add-to-list 'res x)))
            with-dups)
    res))


;; The end.
(provide 'nnir)

;;; nnir.el ends here
