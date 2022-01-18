;;; gnus-search.el --- Search facilities for Gnus    -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>

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

;; This file defines a generalized search language, and search engines
;; that interface with various search programs.  It is responsible for
;; parsing the user's search input, sending that query to the search
;; engines, and collecting results.  Results are in the form of a
;; vector of vectors, each vector representing a found article.  The
;; nnselect backend interprets that value to create a group containing
;; the search results.

;; This file was formerly known as nnir.  Later, the backend parts of
;; nnir became nnselect, and only the search functionality was left
;; here.

;; See the Gnus manual for details of the search language.  Tests are
;; in tests/gnus-search-test.el.

;; The search parsing routines are responsible for accepting the
;; user's search query as a string and parsing it into a sexp
;; structure.  The function `gnus-search-parse-query' is the entry
;; point for that.  Once the query is in sexp form, it is passed to
;; the search engines themselves, which are responsible for
;; transforming the query into a form that the external program can
;; understand, and then filtering the search results into a format
;; that nnselect can understand.

;; The general flow is:

;; 1. The user calls one of `gnus-group-make-search-group' or
;; `gnus-group-make-permanent-search-group' (or a few other entry
;; points).  These functions prompt for a search query, and collect
;; the groups to search, then create an nnselect group, setting an
;; 'nnselect-specs group parameter where 'nnselect-function is
;; `gnus-search-run-query', and 'nnselect-args is the search query and
;; groups to search.

;; 2. `gnus-search-run-query' is called with 'nnselect-args.  It looks
;; at the groups to search, categorizes them by server, and for each
;; server finds the search engine to use.  It calls each engine's
;; `gnus-search-run-search' method with the query and groups passed as
;; arguments, and the results are collected and handed off to the
;; nnselect group.

;; For information on writing new search engines, see the Gnus manual.

;; TODO: Rewrite the query parser using syntax tables and
;; `parse-partial-sexp'.

;; TODO: Refactor IMAP search so we can move code that uses nnimap-*
;; functions out into nnimap.el.

;; TODO: Is there anything we can do about sorting results?

;; TODO: Provide for returning a result count.  This would probably
;; need a completely separate top-level command, since we wouldn't be
;; creating a group at all.

;;; Code:

(require 'gnus-group)
(require 'gnus-sum)
(require 'message)
(require 'gnus-util)
(require 'eieio)
(eval-when-compile (require 'cl-lib))
(autoload 'eieio-build-class-alist "eieio-opt")
(autoload 'nnmaildir-base-name-to-article-number "nnmaildir")

(defvar gnus-inhibit-demon)
(defvar gnus-english-month-names)

;;; Internal Variables:

;; When Gnus servers are implemented as objects or structs, give them
;; a `search-engine' slot and get rid of this variable.
(defvar gnus-search-engine-instance-alist nil
  "Mapping between servers and instantiated search engines.")

(defvar gnus-search-history ()
  "Internal history of Gnus searches.")

(defun gnus-search-shutdown ()
  (setq gnus-search-engine-instance-alist nil))

(gnus-add-shutdown #'gnus-search-shutdown 'gnus)

(define-error 'gnus-search-parse-error "Gnus search parsing error")

(define-error 'gnus-search-config-error "Gnus search configuration error")

;;; User Customizable Variables:

(defgroup gnus-search nil
  "Search groups in Gnus with assorted search engines."
  :group 'gnus)

(defcustom gnus-search-use-parsed-queries nil
  "When t, use Gnus' generalized search language.
The generalized search language is a search language that can be
used across all search engines that Gnus supports.  See the Gnus
manual for details.

If this option is set to nil, search queries will be passed
directly to the search engines without being parsed or
transformed."
  :version "28.1"
  :type 'boolean)

(define-obsolete-variable-alias 'nnir-ignored-newsgroups
  'gnus-search-ignored-newsgroups "28.1")

(defcustom gnus-search-ignored-newsgroups ""
  "A regexp to match newsgroups in the active file that should
be skipped when searching."
  :version "24.1"
  :type 'regexp)

(make-obsolete-variable
 'nnir-imap-default-search-key
 "specify imap search keys, or use parsed queries." "28.1")

;; Engine-specific configuration options.

(defcustom gnus-search-swish++-config-file
  (expand-file-name "~/Mail/swish++.conf")
  "Location of Swish++ configuration file.
This variable can also be set per-server."
  :type 'file)

(defcustom gnus-search-swish++-program "search"
  "Name of swish++ search executable.
This variable can also be set per-server."
  :type 'string)

(defcustom gnus-search-swish++-switches '()
  "A list of strings, to be given as additional arguments to swish++.
Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-swish++-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-swish++-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string))

(defcustom gnus-search-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by swish++
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type 'regexp)

(defcustom gnus-search-swish++-raw-queries-p nil
  "If t, all Swish++ engines will only accept raw search query strings."
  :type 'boolean
  :version "28.1")

(defcustom gnus-search-swish-e-config-file
  (expand-file-name "~/Mail/swish-e.conf")
  "Configuration file for swish-e.
This variable can also be set per-server."
  :type 'file
  :version "28.1")

(defcustom gnus-search-swish-e-program "search"
  "Name of swish-e search executable.
This variable can also be set per-server."
  :type 'string
  :version "28.1")

(defcustom gnus-search-swish-e-switches '()
  "A list of strings, to be given as additional arguments to swish-e.
Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-swish-e-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-swish-e-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string)
  :version "28.1")

(defcustom gnus-search-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by swish-e
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type 'regexp
  :version "28.1")

(defcustom gnus-search-swish-e-index-files '()
  "A list of index files to use with this Swish-e instance.
This variable can also be set per-server."
  :type '(repeat file)
  :version "28.1")

(defcustom gnus-search-swish-e-raw-queries-p nil
  "If t, all Swish-e engines will only accept raw search query strings."
  :type 'boolean
  :version "28.1")

;; Namazu engine, see <URL:http://www.namazu.org/>

(defcustom gnus-search-namazu-program "namazu"
  "Name of Namazu search executable.
This variable can also be set per-server."
  :type 'string
  :version "28.1")

(defcustom gnus-search-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "Index directory for Namazu.
This variable can also be set per-server."
  :type 'directory
  :version "28.1")

(defcustom gnus-search-namazu-switches '()
  "A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other switches
make any sense in this context.

Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-namazu-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-namazu-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string)
  :version "28.1")

(defcustom gnus-search-namazu-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by Namazu
in order to get a group name (albeit with / instead of .).

For example, suppose that Namazu returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq gnus-search-namazu-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
Gnus knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\".

This variable can also be set per-server."
  :type 'directory
  :version "28.1")

(defcustom gnus-search-namazu-raw-queries-p nil
  "If t, all Namazu engines will only accept raw search query strings."
  :type 'boolean
  :version "28.1")

(defcustom gnus-search-notmuch-program "notmuch"
  "Name of notmuch search executable.
This variable can also be set per-server."
  :type '(string)
  :version "28.1")

(defcustom gnus-search-notmuch-config-file
  (expand-file-name "~/.notmuch-config")
  "Configuration file for notmuch.
This variable can also be set per-server."
  :type 'file
  :version "28.1")

(defcustom gnus-search-notmuch-switches '()
  "A list of strings, to be given as additional arguments to notmuch.
Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-notmuch-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-notmuch-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string)
  :version "28.1")

(defcustom gnus-search-notmuch-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by notmuch
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type 'regexp
  :version "28.1")

(defcustom gnus-search-notmuch-raw-queries-p nil
  "If t, all Notmuch engines will only accept raw search query strings."
  :type 'boolean
  :version "28.1")

(defcustom gnus-search-imap-raw-queries-p nil
  "If t, all IMAP engines will only accept raw search query strings."
  :version "28.1"
  :type 'boolean)

(defcustom gnus-search-mairix-program "mairix"
  "Name of mairix search executable.
This variable can also be set per-server."
  :version "28.1"
  :type 'string)

(defcustom gnus-search-mairix-config-file
  (expand-file-name "~/.mairixrc")
  "Configuration file for mairix.
This variable can also be set per-server."
  :version "28.1"
  :type 'file)

(defcustom gnus-search-mairix-switches '()
  "A list of strings, to be given as additional arguments to mairix.
Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-mairix-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnu-search-mairix-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :version "28.1"
  :type '(repeat string))

(defcustom gnus-search-mairix-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by mairix
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :version "28.1"
  :type 'regexp)

(defcustom gnus-search-mairix-raw-queries-p nil
  "If t, all Mairix engines will only accept raw search query strings."
  :version "28.1"
  :type 'boolean)

;; Options for search language parsing.

(defcustom gnus-search-expandable-keys
  '("from" "subject" "to" "cc" "bcc" "body" "recipient" "date"
    "mark" "before" "after" "larger" "smaller" "attachment" "text"
    "since" "thread" "sender" "address" "tag" "size" "grep" "limit"
    "raw" "message-id" "id")
  "A list of strings representing expandable search keys.
\"Expandable\" simply means the key can be abbreviated while
typing in search queries, ie \"subject\" could be entered as
\"subj\" or even \"su\", though \"s\" is ambiguous between
\"subject\" and \"since\".

Ambiguous abbreviations will raise an error."
  :version "28.1"
  :type '(repeat string))

(defcustom gnus-search-date-keys
  '("date" "before" "after" "on" "senton" "sentbefore" "sentsince" "since")
  "A list of keywords whose value should be parsed as a date.
See the docstring of `gnus-search-parse-query' for information on
date parsing."
  :version "26.1"
  :type '(repeat string))

(defcustom gnus-search-contact-tables '()
  "A list of completion tables used to search for messages from contacts.
Each list element should be a table or collection suitable to be
returned by `completion-at-point-functions'.  That usually means
a list of strings, a hash table, or an alist."
  :version "28.1"
  :type '(repeat sexp))

;;; Search language

;; This "language" was generalized from the original IMAP search query
;; parsing routine.

(defun gnus-search-parse-query (string)
  "Turn STRING into an s-expression based query.
The resulting query structure is passed to the various search
backends, each of which adapts it as needed.

The search \"language\" is essentially a series of key:value
expressions.  Key is most often a mail header, but there are
other keys.  Value is a string, quoted if it contains spaces.
Key and value are separated by a colon, no space.  Expressions
are implicitly ANDed; the \"or\" keyword can be used to
OR.  \"not\" will negate the following expression, or keys can be
prefixed with a \"-\".  The \"near\" operator will work for
engines that understand it; other engines will convert it to
\"or\".  Parenthetical groups work as expected.

A key that matches the name of a mail header will search that
header.

Search keys can be expanded with TAB during entry, or left
abbreviated so long as they remain unambiguous, ie \"f\" will
search the \"from\" header.  \"s\" will raise an error.

Other keys:

\"address\" will search all sender and recipient headers.

\"recipient\" will search \"To\", \"Cc\", and \"Bcc\".

\"before\" will search messages sent before the specified
date (date specifications to come later).  Date is exclusive.

\"after\" (or its synonym \"since\") will search messages sent
after the specified date.  Date is inclusive.

\"mark\" will search messages that have some sort of mark.
Likely values include \"flag\", \"seen\", \"read\", \"replied\".
It's also possible to use Gnus' internal marks, ie \"mark:R\"
will be interpreted as mark:read.

\"tag\" will search tags -- right now that's translated to
\"keyword\" in IMAP, and left as \"tag\" for notmuch.  At some
point this should also be used to search marks in the Gnus
registry.

Other keys can be specified, provided that the search backends
know how to interpret them.

External contact-management packages can push completion tables
onto the list variable `gnus-search-contact-tables', to provide
auto-completion of contact names and addresses for keys like
\"from\" and \"to\".

Date values (any key in `gnus-search-date-keys') can be provided
in any format that `parse-time-string' can parse (note that this
can produce weird results).  Dates with missing bits will be
interpreted as the most recent occurrence thereof (i.e. \"march
03\" is the most recent March 3rd).  Lastly, relative
specifications such as 1d (one day ago) are understood.  This
also accepts w, m, and y.  m is assumed to be 30 days.

This function will accept pretty much anything as input.  Its
only job is to parse the query into a sexp, and pass that on --
it is the job of the search backends to make sense of the
structured query.  Malformed, unusable or invalid queries will
typically be silently ignored."
  (with-temp-buffer
    ;; Set up the parsing environment.
    (insert string)
    (goto-char (point-min))
    ;; Now, collect the output terms and return them.
    (let (out)
      (while (not (gnus-search-query-end-of-input))
	(push (gnus-search-query-next-expr) out))
      (reverse out))))

(defun gnus-search-query-next-expr (&optional count halt)
  "Return the next expression from the current buffer."
  (let ((term (gnus-search-query-next-term count))
	(next (gnus-search-query-peek-symbol)))
    ;; Deal with top-level expressions.  And, or, not, near...  What
    ;; else?  Notmuch also provides xor and adj.  It also provides a
    ;; "nearness" parameter for near and adj.
    (cond
     ;; Handle 'expr or expr'
     ((and (eq next 'or)
	   (null halt))
      (list 'or term (gnus-search-query-next-expr 2)))
     ;; Handle 'near operator.
     ((eq next 'near)
      (let ((near-next (gnus-search-query-next-expr 2)))
	(if (and (stringp term)
		 (stringp near-next))
	    (list 'near term near-next)
	  (signal 'gnus-search-parse-error
		  (list "\"Near\" keyword must appear between two plain strings.")))))
     ;; Anything else
     (t term))))

(defun gnus-search-query-next-term (&optional count)
  "Return the next TERM from the current buffer."
  (let ((term (gnus-search-query-next-symbol count)))
    ;; What sort of term is this?
    (cond
     ;; negated term
     ((eq term 'not) (list 'not (gnus-search-query-next-expr nil 'halt)))
     ;; generic term
     (t term))))

(defun gnus-search-query-peek-symbol ()
  "Return the next symbol from the current buffer, but don't consume it."
  (save-excursion
    (gnus-search-query-next-symbol)))

(defun gnus-search-query-next-symbol (&optional count)
  "Return the next symbol from the current buffer, or nil if we are
at the end of the buffer.  If supplied COUNT skips some symbols before
returning the one at the supplied position."
  (when (and (numberp count) (> count 1))
    (gnus-search-query-next-symbol (1- count)))
  (let ((case-fold-search t))
    ;; end of input stream?
    (unless (gnus-search-query-end-of-input)
      ;; No, return the next symbol from the stream.
      (cond
       ;; Negated expression -- return it and advance one char.
       ((looking-at "-") (forward-char 1) 'not)
       ;; List expression -- we parse the content and return this as a list.
       ((looking-at "(")
	(gnus-search-parse-query (gnus-search-query-return-string ")" t)))
       ;; Keyword input -- return a symbol version.
       ((looking-at "\\band\\b") (forward-char 3) 'and)
       ((looking-at "\\bor\\b")  (forward-char 2) 'or)
       ((looking-at "\\bnot\\b") (forward-char 3) 'not)
       ((looking-at "\\bnear\\b") (forward-char 4) 'near)
       ;; Plain string, no keyword
       ((looking-at "[\"/]?\\b[^:]+\\([[:blank:]]\\|\\'\\)")
	(gnus-search-query-return-string
	 (when (looking-at-p "[\"/]") t)))
       ;; Assume a K:V expression.
       (t (let ((key (gnus-search-query-expand-key
		      (buffer-substring
		       (point)
		       (progn
			 (re-search-forward ":" (point-at-eol) t)
			 (1- (point))))))
		(value (gnus-search-query-return-string
			(when (looking-at-p "[\"/]") t))))
	    (gnus-search-query-parse-kv key value)))))))

(defun gnus-search-query-parse-kv (key value)
  "Handle KEY and VALUE, parsing and expanding as necessary.
This may result in (key value) being turned into a larger query
structure.

In the simplest case, they are simply consed together.  String
KEY is converted to a symbol."
  (let () ;; return
    (cond
     ((member key gnus-search-date-keys)
      (when (string= "after" key)
	(setq key "since"))
      (setq value (gnus-search-query-parse-date value)))
     ((equal key "mark")
      (setq value (gnus-search-query-parse-mark value)))
     ((string= "message-id" key)
      (setq key "id")))
    (or nil ;; return
	(cons (intern key) value))))

(defun gnus-search-query-parse-date (value &optional rel-date)
  "Interpret VALUE as a date specification.
See the docstring of `gnus-search-parse-query' for details.

The result is a list of (dd mm yyyy); individual elements can be
nil.

If VALUE is a relative time, interpret it as relative to
REL-DATE, or (current-time) if REL-DATE is nil."
  ;; Time parsing doesn't seem to work with slashes.
  (let ((value (string-replace "/" "-" value))
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

(defun gnus-search-query-parse-mark (mark)
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

(defun gnus-search-query-expand-key (key)
  "Attempt to expand KEY to a full keyword.
Use `gnus-search-expandable-keys' as a completion table; return
KEY directly if it can't be completed.  Raise an error if KEY is
ambiguous, meaning that it is a prefix of multiple known
keywords.  This means that it's not possible to enter a custom
keyword that happens to be a prefix of a known keyword."
  (let ((comp (try-completion key gnus-search-expandable-keys)))
    (if (or (eql comp 't)		; Already a key.
	    (null comp))		; An unknown key.
	key
      (if (null (member comp gnus-search-expandable-keys))
	  ;; KEY is a prefix of multiple known keywords, and could not
	  ;; be completed to something unique.
	  (signal 'gnus-search-parse-error
		  (list (format "Ambiguous keyword: %s" key)))
	;; We completed to a unique known key.
	comp))))

(defun gnus-search-query-return-string (&optional delimited trim)
  "Return a string from the current buffer.
If DELIMITED is non-nil, assume the next character is a delimiter
character, and return everything between point and the next
occurrence of the delimiter, including the delimiters themselves.
If TRIM is non-nil, do not return the delimiters.  Otherwise,
return one word."
  ;; This function cannot handle nested delimiters, as it's not a
  ;; proper parser.  Ie, you cannot parse "to:bob or (from:bob or
  ;; (cc:bob or bcc:bob))".
  (let ((start (point))
	(delimiter (if (stringp delimited)
		       delimited
		     (when delimited
		       (char-to-string (char-after)))))
	end)
    (if delimiter
	(progn
	  (when trim
	    ;; Skip past first delimiter if we're trimming.
	    (forward-char 1))
	  (while (not end)
	    (unless (search-forward delimiter nil t (unless trim 2))
	      (signal 'gnus-search-parse-error
		      (list (format "Unmatched delimited input with %s in query" delimiter))))
	    (let ((here (point)))
	      (unless (equal (buffer-substring (- here 2) (- here 1)) "\\")
		(setq end (if trim (1- (point)) (point))
		      start (if trim (1+ start) start))))))
      (setq end (progn (re-search-forward "\\([[:blank:]]+\\|$\\)" (point-max) t)
		       (match-beginning 0))))
    (buffer-substring-no-properties start end)))

(defun gnus-search-query-end-of-input ()
  "Are we at the end of input?"
  (skip-chars-forward "[:blank:]")
  (looking-at "$"))

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

(defclass gnus-search-grep ()
  ((grep-program
    :initarg :grep-program
    :initform "grep"
    :type string
    :documentation "Grep executable to use for second-pass grep
    searches.")
   (grep-options
    :initarg :grep-options
    :initform nil
    :type list
    :documentation "Additional options, in the form of a list,
    passed to the second-pass grep search, when present."))
  :abstract t
  :documentation "An abstract mixin class that can be added to
  local-filesystem search engines, providing an additional grep:
  search key.  After the base engine returns a list of search
  results (as local filenames), an external grep process is used
  to further filter the results.")

(cl-defgeneric gnus-search-grep-search (engine artlist criteria)
  "Run a secondary grep search over a list of preliminary results.

ARTLIST is a list of (filename score) pairs, produced by one of
the other search engines.  CRITERIA is a grep-specific search
key.  This method uses an external grep program to further filter
the files in ARTLIST by that search key.")

(cl-defmethod gnus-search-grep-search ((engine gnus-search-grep)
				       artlist criteria)
  (with-slots (grep-program grep-options) engine
    (if (executable-find grep-program)
	;; Don't catch errors -- allow them to propagate.
	(let ((matched-files
	       (apply
		#'process-lines
		grep-program
		`("-l" ,@grep-options
		  "-e" ,(shell-quote-argument criteria)
		  ,@(mapcar #'car artlist)))))
	  (seq-filter (lambda (a) (member (car a) matched-files))
		      artlist))
      (nnheader-report 'search "invalid grep program: %s" grep-program))))

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
			 (generate-new-buffer " *gnus-search-")))
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
    :initform nil
    :type boolean
    :documentation
    "Can this search engine handle the MULTISEARCH capability?
    This slot is set automatically by the imap server, and cannot
    be set manually.  Currently unimplemented.")
   (fuzzy
    :initarg :fuzzy
    :initform nil
    :type boolean
    :documentation
    "Can this search engine handle the FUZZY search capability?
    This slot is set automatically by the imap server, and cannot
    be set manually.  Currently only partially implemented.")
   (raw-queries-p
    :initform (symbol-value 'gnus-search-imap-raw-queries-p)))
    :documentation
  "The base IMAP search engine, using an IMAP server's search capabilities.
This backend may be subclassed to handle particular IMAP servers'
quirks.")

(defclass gnus-search-find-grep (gnus-search-engine
				 gnus-search-process
				 gnus-search-grep)
  nil)

;;; The "indexed" search engine.

;; These are engines that use an external program, with indexes kept
;; on disk, to search messages usually kept in some local directory.
;; They have several slots in common, for instance program name or
;; configuration file.  Many of the subclasses also allow
;; distinguishing multiple databases or indexes.  These slots can be
;; set using a global default, or on a per-server basis.

(defclass gnus-search-indexed (gnus-search-engine
			       gnus-search-process
			       gnus-search-grep)
  ((program
    :initarg :program
    :type string
    :documentation
    "The executable used for indexing and searching.")
   (config-file
    :init-arg :config-file
    :type string
    :custom file
    :documentation "Location of the config file, if any.")
   (remove-prefix
    :initarg :remove-prefix
    :initform (concat (getenv "HOME") "/Mail/")
    :type string
    :documentation
    "The path to the directory where the indexed mails are
    kept.  This path is removed from the search results.")
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

(defclass gnus-search-swish-e (gnus-search-indexed)
  ((index-files
    :init-arg :index-files
    :initform (symbol-value 'gnus-search-swish-e-index-files)
    :type list)
   (program
    :initform (symbol-value 'gnus-search-swish-e-program))
   (remove-prefix
    :initform (symbol-value 'gnus-search-swish-e-remove-prefix))
   (switches
    :initform (symbol-value 'gnus-search-swish-e-switches))
   (raw-queries-p
    :initform (symbol-value 'gnus-search-swish-e-raw-queries-p))))

(defclass gnus-search-swish++ (gnus-search-indexed)
  ((program
    :initform (symbol-value 'gnus-search-swish++-program))
   (remove-prefix
    :initform (symbol-value 'gnus-search-swish++-remove-prefix))
   (switches
    :initform (symbol-value 'gnus-search-swish++-switches))
   (config-file
    :initform (symbol-value 'gnus-search-swish++-config-file))
   (raw-queries-p
    :initform (symbol-value 'gnus-search-swish++-raw-queries-p))))

(defclass gnus-search-mairix (gnus-search-indexed)
  ((program
    :initform (symbol-value 'gnus-search-mairix-program))
   (remove-prefix
    :initform (symbol-value 'gnus-search-mairix-remove-prefix))
   (switches
    :initform (symbol-value 'gnus-search-mairix-switches))
   (config-file
    :initform (symbol-value 'gnus-search-mairix-config-file))
   (raw-queries-p
    :initform (symbol-value 'gnus-search-mairix-raw-queries-p))))

(defclass gnus-search-namazu (gnus-search-indexed)
  ((index-directory
    :initarg :index-directory
    :initform (symbol-value 'gnus-search-namazu-index-directory)
    :type string
    :custom directory)
   (program
    :initform (symbol-value 'gnus-search-namazu-program))
   (remove-prefix
    :initform (symbol-value 'gnus-search-namazu-remove-prefix))
   (switches
    :initform (symbol-value 'gnus-search-namazu-switches))
   (raw-queries-p
    :initform (symbol-value 'gnus-search-namazu-raw-queries-p))))

(defclass gnus-search-notmuch (gnus-search-indexed)
  ((program
    :initform (symbol-value 'gnus-search-notmuch-program))
   (remove-prefix
    :initform (symbol-value 'gnus-search-notmuch-remove-prefix))
   (switches
    :initform (symbol-value 'gnus-search-notmuch-switches))
   (config-file
    :initform (symbol-value 'gnus-search-notmuch-config-file))
   (raw-queries-p
    :initform (symbol-value 'gnus-search-notmuch-raw-queries-p))))

(define-obsolete-variable-alias 'nnir-method-default-engines
  'gnus-search-default-engines "28.1")

(defcustom gnus-search-default-engines '((nnimap . gnus-search-imap))
  "Alist of default search engines keyed by server method."
  :version "26.1"
  :type `(repeat (cons (choice (const nnimap) (const nntp) (const nnspool)
			       (const nneething) (const nndir) (const nnmbox)
			       (const nnml) (const nnmh) (const nndraft)
			       (const nnfolder) (const nnmaildir))
		       (choice
			,@(mapcar
			   (lambda (el) (list 'const (intern (car el))))
			   (eieio-build-class-alist 'gnus-search-engine t))))))

;;; Transforming and running search queries.

(cl-defgeneric gnus-search-run-search (engine server query groups)
  "Run QUERY in GROUPS against SERVER, using search ENGINE.
Should return results as a vector of vectors.")

(cl-defgeneric gnus-search-transform (engine expression)
  "Transform sexp EXPRESSION into a string search query usable by ENGINE.
Responsible for handling and, or, and parenthetical expressions.")

(cl-defgeneric gnus-search-transform-expression (engine expression)
  "Transform a basic EXPRESSION into a string usable by ENGINE.")

(cl-defgeneric gnus-search-make-query-string (engine query-spec)
  "Extract the actual query string to use from QUERY-SPEC.")

;; Methods that are likely to be the same for all engines.

(cl-defmethod gnus-search-make-query-string ((engine gnus-search-engine)
					     query-spec)
  (let ((parsed-query (alist-get 'parsed-query query-spec))
	(raw-query (alist-get 'query query-spec)))
    (if (and gnus-search-use-parsed-queries
	     (null (alist-get 'raw query-spec))
	     (null (slot-value engine 'raw-queries-p))
	     parsed-query)
	(gnus-search-transform engine parsed-query)
      (if (listp raw-query)
	  ;; Some callers are sending this in as (query "query"), not
	  ;; as a cons cell?
	  (car raw-query)
	raw-query))))

(defsubst gnus-search-single-p (query)
  "Return t if QUERY is a search for a single message."
  (let ((q (alist-get 'parsed-query query)))
    (and (= (length q ) 1)
	 (consp (car-safe q))
	 (eq (caar q) 'id))))

(cl-defmethod gnus-search-transform ((engine gnus-search-engine)
				     (query list))
  (let (clauses)
    (mapc
     (lambda (item)
       (when-let ((expr (gnus-search-transform-expression engine item)))
	 (push expr clauses)))
     query)
    (mapconcat #'identity (reverse clauses) " ")))

;; Most search engines just pass through plain strings.
(cl-defmethod gnus-search-transform-expression ((_ gnus-search-engine)
						(expr string))
  expr)

;; Most search engines use implicit ANDs.
(cl-defmethod gnus-search-transform-expression ((_ gnus-search-engine)
						(_expr (eql 'and)))
  nil)

;; Most search engines use explicit infixed ORs.
(cl-defmethod gnus-search-transform-expression ((engine gnus-search-engine)
						(expr (head or)))
  (let ((left (gnus-search-transform-expression engine (nth 1 expr)))
	(right (gnus-search-transform-expression engine (nth 2 expr))))
    ;; Unhandled keywords return a nil; don't create an "or" expression
    ;; unless both sub-expressions are non-nil.
    (if (and left right)
	(format "%s or %s" left right)
      (or left right))))

;; Most search engines just use the string "not"
(cl-defmethod gnus-search-transform-expression ((engine gnus-search-engine)
						(expr (head not)))
  (let ((next (gnus-search-transform-expression engine (cadr expr))))
    (when next
     (format "not %s" next))))

;;; Search Engine Interfaces:

(autoload 'nnimap-change-group "nnimap")
(declare-function nnimap-buffer "nnimap" ())
(declare-function nnimap-command "nnimap" (&rest args))

(defvar gnus-search-imap-search-keys
  '(body cc bcc from header keyword larger smaller subject text to uid x-gm-raw
	 answered before deleted draft flagged on since recent seen sentbefore
	 senton sentsince unanswered undeleted undraft unflagged unkeyword
	 unseen all old new or not)
  "Known IMAP search keys.")

;; imap interface
(cl-defmethod gnus-search-run-search ((engine gnus-search-imap)
				      srv query groups)
  (save-excursion
    (let ((server (cadr (gnus-server-to-method srv)))
          (gnus-inhibit-demon t)
	  ;; We're using the message id to look for a single message.
	  (single-search (gnus-search-single-p query))
	  (grouplist (or groups (gnus-search-get-active srv)))
	  q-string artlist group)
      (message "Opening server %s" server)
      (gnus-open-server srv)
      ;; We should only be doing this once, in
      ;; `nnimap-open-connection', but it's too frustrating to try to
      ;; get to the server from the process buffer.
      (with-current-buffer (nnimap-buffer)
	(setf (slot-value engine 'literal-plus)
	      (when (nnimap-capability "LITERAL+") t))
	;; MULTISEARCH not yet implemented.
	(setf (slot-value engine 'multisearch)
	      (when (nnimap-capability "MULTISEARCH") t))
	;; FUZZY only partially supported: the command is sent to the
	;; server (and presumably acted upon), but we don't yet
	;; request a RELEVANCY score as part of the response.
	(setf (slot-value engine 'fuzzy)
	      (when (nnimap-capability "SEARCH=FUZZY") t)))

      (setq q-string
	    (gnus-search-make-query-string engine query))

      ;; A bit of backward-compatibility slash convenience: if the
      ;; query string doesn't start with any known IMAP search
      ;; keyword, assume it is a "TEXT" search.
      (unless (or (eql ?\( (aref q-string 0))
		  (and (string-match "\\`[^[:blank:]]+" q-string)
		       (memql (intern-soft (downcase
					    (match-string 0 q-string)))
			      gnus-search-imap-search-keys)))
	(setq q-string (concat "TEXT " q-string)))

      ;; If it's a thread query, make sure that all message-id
      ;; searches are also references searches.
      (when (alist-get 'thread query)
	(setq q-string
	      (replace-regexp-in-string
	       "HEADER Message-Id \\([^ )]+\\)"
	       "(OR HEADER Message-Id \\1 HEADER References \\1)"
	       q-string)))

      (while (and (setq group (pop grouplist))
		  (or (null single-search) (null artlist)))
	(when (nnimap-change-group
	       (gnus-group-short-name group) server)
	  (with-current-buffer (nnimap-buffer)
	    (message "Searching %s..." group)
	    (let ((result
		   (gnus-search-imap-search-command engine q-string)))
	      (when (car result)
		(setq artlist
		      (vconcat
		       (mapcar
			(lambda (artnum)
			  (let ((artn (string-to-number artnum)))
			    (when (> artn 0)
			      (vector group artn 100))))
			(cdr (assoc "SEARCH" (cdr result))))
		       artlist))))
	    (message "Searching %s...done" group))))
      (nreverse artlist))))

(cl-defmethod gnus-search-imap-search-command ((engine gnus-search-imap)
					       (query string))
  "Create the IMAP search command for QUERY.
Currently takes into account support for the LITERAL+ capability.
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

(cl-defmethod gnus-search-transform ((_ gnus-search-imap)
				     (_query null))
  "ALL")

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-imap)
						(expr string))
  (unless (string-match-p "\\`/.+/\\'" expr)
    ;; Also need to check for fuzzy here.  Or better, do some
    ;; refactoring of this stuff.
    (format "TEXT %s"
	    (gnus-search-imap-handle-string engine expr))))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-imap)
						(expr (head or)))
  (let ((left (gnus-search-transform-expression engine (nth 1 expr)))
	(right (gnus-search-transform-expression engine (nth 2 expr))))
    (if (and left right)
	(format "(OR %s %s)"
		left (format (if (eq 'or (car-safe (nth 2 expr)))
				 "(%s)" "%s")
			     right))
      (or left right))))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-imap)
						(expr (head near)))
  "Imap searches interpret \"near\" as \"or\"."
  (setcar expr 'or)
  (gnus-search-transform-expression engine expr))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-imap)
						(expr (head not)))
  "Transform IMAP NOT.
If the term to be negated is a flag, then use the appropriate UN*
boolean instead."
  (if (eql (caadr expr) 'mark)
      (if (string= (cdadr expr) "new")
	  "OLD"
	(format "UN%s" (gnus-search-imap-handle-flag (cdadr expr))))
    (format "NOT %s"
	    (gnus-search-transform-expression engine (cadr expr)))))

(cl-defmethod gnus-search-transform-expression ((_ gnus-search-imap)
						(expr (head mark)))
  (gnus-search-imap-handle-flag (cdr expr)))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-imap)
						(expr list))
  "Handle a search keyword for IMAP.
All IMAP search keywords that take a value are supported
directly.  Keywords that are boolean are supported through other
means (usually the \"mark\" keyword)."
  (let ((fuzzy-supported (slot-value engine 'fuzzy))
	(fuzzy ""))
    (cl-case (car expr)
      (date (setcar expr 'on))
      (tag (setcar expr 'keyword))
      (sender (setcar expr 'from))
      (attachment (setcar expr 'body)))
    ;; Allow sizes specified as KB or MB.
    (let ((case-fold-search t)
	  unit)
      (when (and (memq (car expr) '(larger smaller))
		 (string-match "\\(kb?\\|mb?\\)\\'" (cdr expr)))
	(setq unit (match-string 1 (cdr expr)))
	(setcdr expr
		(number-to-string
		 (* (string-to-number
		     (string-replace unit "" (cdr expr)))
		    (if (string-prefix-p "k" unit)
			1024
		      1048576))))))
    (cond
     ((consp (car expr))
      (format "(%s)" (gnus-search-transform engine expr)))
     ((eq (car expr) 'recipient)
      (gnus-search-transform
       engine (gnus-search-parse-query
	       (format
		"to:%s or cc:%s or bcc:%s"
		(cdr expr) (cdr expr) (cdr expr)))))
     ((eq (car expr) 'address)
      (gnus-search-transform
       engine (gnus-search-parse-query
	       (format
		"from:%s or to:%s or cc:%s or bcc:%s"
		(cdr expr) (cdr expr) (cdr expr) (cdr expr)))))
     ((memq (car expr) '(before since on sentbefore senton sentsince))
      ;; Ignore dates given as strings.
      (when (listp (cdr expr))
	(format "%s %s"
		(upcase (symbol-name (car expr)))
		(gnus-search-imap-handle-date engine (cdr expr)))))
     ((stringp (cdr expr))
      ;; If the search term starts or ends with "*", remove the
      ;; asterisk.  If the engine supports FUZZY, then additionally make
      ;; the search fuzzy.
      (when (string-match "\\`\\*\\|\\*\\'" (cdr expr))
	(setcdr expr (replace-regexp-in-string
		      "\\`\\*\\|\\*\\'" "" (cdr expr)))
	(when fuzzy-supported
	  (setq fuzzy "FUZZY ")))
      ;; If the search term is a regexp, drop the expression altogether.
      (unless (string-match-p "\\`/.+/\\'" (cdr expr))
	(cond
	 ((memq (car expr) gnus-search-imap-search-keys)
	  (format "%s%s %s"
		  fuzzy
		  (upcase (symbol-name (car expr)))
		  (gnus-search-imap-handle-string engine (cdr expr))))
	 ((eq (car expr) 'id)
	  (format "HEADER Message-ID \"%s\"" (cdr expr)))
	 ;; Treat what can't be handled as a HEADER search.  Probably a bad
	 ;; idea.
	 (t (format "%sHEADER %s %s"
		    fuzzy
		    (car expr)
		    (gnus-search-imap-handle-string engine (cdr expr))))))))))

(cl-defmethod gnus-search-imap-handle-date ((_engine gnus-search-imap)
					    (date list))
  "Turn DATE into a date string recognizable by IMAP.
While other search engines can interpret partially-qualified
dates such as a plain \"January\", IMAP requires an absolute
date.

DATE is a list of (dd mm yyyy), any element of which could be
nil (except that (dd nil yyyy) is not allowed).  Massage those
numbers into the most recent past occurrence of whichever date
elements are present."
  (pcase-let ((`(,nday ,nmonth ,nyear)
	       (seq-subseq (decode-time (current-time))
			   3 6))
	      (`(,dday ,dmonth ,dyear) date))
    (unless (and dday dmonth dyear)
      (unless dday (setq dday 1))
      (if dyear
	  ;; If we have a year, then leave everything else as is or set
	  ;; to 1.
	  (setq dmonth (or dmonth 1))
	(if dmonth
	    (setq dyear
		  (if (or (> dmonth nmonth)
			  (and (= dmonth nmonth)
			       (> dday nday)))
		      ;; If our day/month combo is ahead of "now",
		      ;; move the year back.
		      (1- nyear)
		    nyear))
	  (setq dmonth 1))))
    (format-time-string
     "%e-%b-%Y"
     (apply #'encode-time
	    (append '(0 0 0)
		    (list dday dmonth dyear))))))

(cl-defmethod gnus-search-imap-handle-string ((engine gnus-search-imap)
					      (str string))
  (with-slots (literal-plus) engine
    (if (multibyte-string-p str)
	;; If LITERAL+ is available, use it and encode string as
	;; UTF-8.
	(if literal-plus
	    (format "{%d+}\n%s"
		    (string-bytes str)
		    (encode-coding-string str 'utf-8))
	  ;; Otherwise, if the user hasn't already quoted the string,
	  ;; quote it for them.
	  (if (string-prefix-p "\"" str)
	      str
	    (format "\"%s\"" str)))
      str)))

(defun gnus-search-imap-handle-flag (flag)
  "Adjust string FLAG to help IMAP recognize it.
If it's one of the RFC3501 flags, make sure it's upcased.
Otherwise, if FLAG starts with a \"$\", treat as a KEYWORD
search.  Otherwise, drop the flag."
  (setq flag
	(pcase flag
	  ("flag" "flagged")
	  ("read" "seen")
	  ("replied" "answered")
	  (_ flag)))
  (cond
   ((member flag '("seen" "answered" "deleted" "draft" "flagged" "recent"))
    (upcase flag))
   ((string-prefix-p "$" flag)
    (format "KEYWORD %s" flag))
   ;; TODO: Provide a user option to treat *all* marks as a KEYWORDs?
   (t "")))

;;; Methods for the indexed search engines.

;; First, some common methods.

(cl-defgeneric gnus-search-indexed-parse-output (engine server query &optional groups)
  "Parse the results of ENGINE's QUERY against SERVER in GROUPS.
Locally-indexed search engines return results as a list of
filenames, sometimes with additional information.  Returns a list
of viable results, in the form of a list of [group article score]
vectors.")

(cl-defgeneric gnus-search-indexed-extract (engine)
  "Extract a single article result from the current buffer.
Returns a list of two values: a file name, and a relevancy score.
Advances point to the beginning of the next result.")

(cl-defmethod gnus-search-run-search ((engine gnus-search-indexed)
				      server query groups)
  "Run QUERY against SERVER using ENGINE.
This method is common to all indexed search engines.

Returns a list of [group article score] vectors."

  (save-excursion
    (let* ((qstring (gnus-search-make-query-string engine query))
	   (program (slot-value engine 'program))
	   (buffer (slot-value engine 'proc-buffer))
	   (cp-list (gnus-search-indexed-search-command
		     engine qstring query groups))
           proc exitstatus)
      (set-buffer buffer)
      (erase-buffer)

      (if groups
	  (message "Doing %s query on %s..." program groups)
	(message "Doing %s query..." program))
      (setq proc (apply #'start-process (format "search-%s" server)
			buffer program cp-list))
      (while (process-live-p proc)
	(accept-process-output proc))
      (setq exitstatus (process-exit-status proc))
      (if (zerop exitstatus)
	  ;; The search results have been put into the current buffer;
	  ;; `parse-output' finds them there and returns the article
	  ;; list.
	  (gnus-search-indexed-parse-output engine server query groups)
	(nnheader-report 'search "%s error: %s" program exitstatus)
	;; Failure reason is in this buffer, show it if the user
	;; wants it.
	(when (> gnus-verbose 6)
	  (display-buffer buffer))
	nil))))

(cl-defmethod gnus-search-indexed-parse-output ((engine gnus-search-indexed)
						server query &optional groups)
  (let ((prefix (or (slot-value engine 'remove-prefix)
                    ""))
        (groups (mapcar #'gnus-group-short-name groups))
	artlist article group)
    (goto-char (point-min))
    ;; Prep prefix, we want to at least be removing the root
    ;; filesystem separator.
    (when (stringp prefix)
      (setq prefix (file-name-as-directory
                    (expand-file-name prefix "/"))))
    (while (not (or (eobp)
                    (looking-at-p
                     "\\(?:[[:space:]\n]+\\)?Process .+ finished")))
      (pcase-let ((`(,f-name ,score) (gnus-search-indexed-extract engine)))
	(when (and f-name
                   (file-readable-p f-name)
		   (null (file-directory-p f-name)))
          (setq group
                (replace-regexp-in-string
	         "[/\\]" "."
	         (replace-regexp-in-string
	          "/?\\(cur\\|new\\|tmp\\)?/\\'" ""
	          (replace-regexp-in-string
	           "\\`\\." ""
	           (string-remove-prefix
                    prefix (file-name-directory f-name))
                   nil t)
	          nil t)
	         nil t))
          (setq article (file-name-nondirectory f-name)
                article
                ;; TODO: Provide a cleaner way of producing final
                ;; article numbers for the various backends.
                (if (string-match-p "\\`[[:digit:]]+\\'" article)
		    (string-to-number article)
		  (nnmaildir-base-name-to-article-number
		   (substring article 0 (string-search ":" article))
		   group (string-remove-prefix "nnmaildir:" server))))
          (when (and (numberp article)
                     (or (null groups)
                         (member group groups)))
	    (push (list f-name article group score)
                  artlist)))))
    ;; Are we running an additional grep query?
    (when-let ((grep-reg (alist-get 'grep query)))
      (setq artlist (gnus-search-grep-search engine artlist grep-reg)))
    ;; Munge into the list of vectors expected by nnselect.
    (mapcar (pcase-lambda (`(,_ ,article ,group ,score))
              (vector
               (gnus-group-full-name group server)
               article
               (if (numberp score)
		   score
		 (string-to-number score))))
            artlist)))

(cl-defmethod gnus-search-indexed-extract ((_engine gnus-search-indexed))
  "Base implementation treats the whole line as a filename, and
fudges a relevancy score of 100."
  (prog1
      (list (buffer-substring-no-properties (line-beginning-position)
					    (line-end-position))
	    100)
    (forward-line 1)))

;; Swish++

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-swish++)
						(expr (head near)))
  (format "%s near %s"
	  (gnus-search-transform-expression engine (nth 1 expr))
	  (gnus-search-transform-expression engine (nth 2 expr))))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-swish++)
						(expr list))
  (cond
   ((listp (car expr))
    (format "(%s)" (gnus-search-transform engine expr)))
   ;; Untested and likely wrong.
   ((and (stringp (cdr expr))
	 (string-prefix-p "(" (cdr expr)))
    (format "%s = %s" (car expr) (gnus-search-transform
				  engine
				  (gnus-search-parse-query (cdr expr)))))
   (t (format "%s = %s" (car expr) (cdr expr)))))

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-swish++)
						  (qstring string)
						  _query &optional _groups)
  (with-slots (config-file switches) engine
   `("--config-file" ,config-file
     ,@switches
     ,qstring
     )))

(cl-defmethod gnus-search-indexed-extract ((_engine gnus-search-swish++))
  (when (re-search-forward
         "\\(^[0-9]+\\) \\([^ ]+\\) [0-9]+ \\(.*\\)$" nil t)
    (list (match-string 2)
	  (match-string 1))))

;; Swish-e

;; I didn't do the query transformation for Swish-e, because the
;; program seems no longer to exist.

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-swish-e)
						  (qstring string)
						  _query &optional _groups)
  (with-slots (index-files switches) engine
    `("-f" ,@index-files
      ,@switches
      "-w"
      ,qstring
      )))

(cl-defmethod gnus-search-indexed-extract ((_engine gnus-search-swish-e))
  (when (re-search-forward
         "\\(^[0-9]+\\) \\([^ ]+\\) \"\\([^\"]+\\)\" [0-9]+$" nil t)
    (list (match-string 3)
          (match-string 1))))

;; Namazu interface

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-namazu)
						(expr list))
  (cond
   ((listp (car expr))
    (format "(%s)" (gnus-search-transform engine expr)))
   ((eql (car expr) 'body)
    (cadr expr))
   ;; I have no idea which fields namazu can handle.  Just do these
   ;; for now.
   ((memq (car expr) '(subject from to))
    (format "+%s:%s" (car expr) (cdr expr)))
   ((eql (car expr) 'address)
    (gnus-search-transform engine `((or (from . ,(cdr expr))
					(to . ,(cdr expr))))))
   ((eq (car expr) 'id)
    (format "+message-id:%s" (cdr expr)))
   (t (ignore-errors (cl-call-next-method)))))

;; I can't tell if this is actually necessary.
(cl-defmethod gnus-search-run-search :around ((_e gnus-search-namazu)
					      _server _query _groups)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LC_MESSAGES" "C")
    (cl-call-next-method)))

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-namazu)
						  (qstring string)
						  query &optional _groups)
  (let ((max (alist-get 'limit query)))
    (with-slots (switches index-directory) engine
      (append
       (list "-q"			; don't be verbose
	     "-a"			; show all matches
	     "-s") 			; use short format
       (when max (list (format "--max=%d" max)))
       switches
       (list qstring index-directory)))))

(cl-defmethod gnus-search-indexed-extract ((_engine gnus-search-namazu))
  "Extract a single message result for Namazu.
Namazu provides a little more information, for instance a score."

  (when (re-search-forward
	 "^\\([0-9,]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
	 nil t)
    (forward-line 1)
    (list (match-string 4)
	  (match-string 3))))

;;; Notmuch interface

(cl-defmethod gnus-search-transform ((_engine gnus-search-notmuch)
				     (_query null))
  "*")

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-notmuch)
						(expr (head near)))
  (format "%s near %s"
	  (gnus-search-transform-expression engine (nth 1 expr))
	  (gnus-search-transform-expression engine (nth 2 expr))))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-notmuch)
						(expr list))
  ;; Swap keywords as necessary.
  (cl-case (car expr)
    (sender (setcar expr 'from))
    ;; Notmuch's "to" is already equivalent to our "recipient".
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
       (format "(%s)" (gnus-search-transform engine expr)))
     ((eql (car expr) 'address)
      (gnus-search-transform engine `((or (from . ,(cdr expr))
					  (to . ,(cdr expr))))))
     ((eql (car expr) 'body)
      (cdr expr))
     ((memq (car expr) '(from to subject attachment mimetype tag id
			      thread folder path lastmod query property))
      ;; Notmuch requires message-id with no angle brackets.
      (when (eql (car expr) 'id)
	(setcdr
	 expr (replace-regexp-in-string "\\`<\\|>\\'" "" (cdr expr))))
      (format "%s:%s" (car expr)
	      (if (string-match "\\`\\*" (cdr expr))
		  ;; Notmuch can only handle trailing asterisk
		  ;; wildcards, so strip leading asterisks.
		  (replace-match "" nil nil (cdr expr))
		(cdr expr))))
     ((eq (car expr) 'date)
      (format "date:%s" (notmuch-date (cdr expr))))
     ((eq (car expr) 'before)
      (format "date:..%s" (notmuch-date (cdr expr))))
     ((eq (car expr) 'since)
      (format "date:%s.." (notmuch-date (cdr expr))))
     (t (ignore-errors (cl-call-next-method))))))

(cl-defmethod gnus-search-run-search :around ((engine gnus-search-notmuch)
					      server query groups)
  "Handle notmuch's thread-search routine."
  ;; Notmuch allows for searching threads, but only using its own
  ;; thread ids.  That means a thread search is a \"double-bounce\":
  ;; once to find the relevant thread ids, and again to find the
  ;; actual messages.  This method performs the first \"bounce\".
  (if (alist-get 'thread query)
      (with-slots (program proc-buffer) engine
	(let* ((qstring
		(gnus-search-make-query-string engine query))
	       (cp-list (gnus-search-indexed-search-command
			 engine qstring query groups))
	       thread-ids proc)
	  (set-buffer proc-buffer)
	  (erase-buffer)
	  (setq proc (apply #'start-process (format "search-%s" server)
			    proc-buffer program cp-list))
	  (while (process-live-p proc)
	    (accept-process-output proc))
	  (while (re-search-forward "^thread:\\([^ ]+\\)" (point-max) t)
	    (push (match-string 1) thread-ids))
	  (cl-call-next-method
	   engine server
	   ;; Completely replace the query with our new thread-based one.
	   (mapconcat (lambda (thrd) (concat "thread:" thrd))
		      thread-ids " or ")
	   nil)))
    (cl-call-next-method engine server query groups)))

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-notmuch)
						  (qstring string)
						  query &optional _groups)
  ;; Theoretically we could use the GROUPS parameter to pass a
  ;; --folder switch to notmuch, but I'm not confident of getting the
  ;; format right.
  (let ((limit (alist-get 'limit query))
	(thread (alist-get 'thread query)))
    (with-slots (switches config-file) engine
      `(,(format "--config=%s" config-file)
	"search"
	,(if thread
	     "--output=threads"
	   "--output=files")
	"--duplicate=1" ; I have found this necessary, I don't know why.
	,@switches
	,(if limit (format "--limit=%d" limit) "")
	,qstring
	))))

;;; Mairix interface

;; See the Gnus manual for why mairix searching is a bit weird.

(cl-defmethod gnus-search-transform ((engine gnus-search-mairix)
				     (query list))
  "Transform QUERY for a Mairix engine.
Because Mairix doesn't accept parenthesized expressions, nor
\"or\" statements between different keys, results may differ from
other engines.  We unpeel parenthesized expressions, and just
cross our fingers for the rest of it."
  (let (clauses)
    (mapc
     (lambda (item)
       (when-let ((expr (if (consp (car-safe item))
			    (gnus-search-transform engine item)
			  (gnus-search-transform-expression engine item))))
	 (push expr clauses)))
     query)
    (mapconcat #'identity (reverse clauses) " ")))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mairix)
						(expr (head not)))
  "Transform Mairix \"not\".
Mairix negation requires a \"~\" preceding string search terms,
and \"-\" before marks."
  (let ((next (gnus-search-transform-expression engine (cadr expr))))
    (string-replace
     ":"
     (if (eql (caadr expr) 'mark)
	 ":-"
       ":~")
     next)))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mairix)
						(expr (head or)))
  "Handle Mairix \"or\" statement.
Mairix only accepts \"or\" expressions on homogeneous keys.  We
cast \"or\" expressions on heterogeneous keys as \"and\", which
isn't quite right, but it's the best we can do.  For date keys,
only keep one of the terms."
  (let ((term1 (caadr expr))
	(term2 (caaddr expr))
	(val1 (gnus-search-transform-expression engine (nth 1 expr)))
	(val2 (gnus-search-transform-expression engine (nth 2 expr))))
    (cond
     ((or (listp term1) (listp term2))
      (concat val1 " " val2))
     ((and (member (symbol-name term1) gnus-search-date-keys)
	   (member (symbol-name term2) gnus-search-date-keys))
      (or val1 val2))
     ((eql term1 term2)
      (if (and val1 val2)
	  (format "%s/%s"
		  val1
		  (nth 1 (split-string val2 ":")))
	(or val1 val2)))
     (t (concat val1 " " val2)))))


(cl-defmethod gnus-search-transform-expression ((_ gnus-search-mairix)
						(expr (head mark)))
  (gnus-search-mairix-handle-mark (cdr expr)))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mairix)
						(expr list))
  (let ((key (cl-case (car expr)
	       (sender "f")
	       (from "f")
	       (to "t")
	       (cc "c")
	       (subject "s")
	       (id "m")
	       (body "b")
	       (address "a")
	       (recipient "tc")
	       (text "bs")
	       (attachment "n")
	       (t nil))))
    (cond
     ((consp (car expr))
      (gnus-search-transform engine expr))
     ((member (symbol-name (car expr)) gnus-search-date-keys)
      (gnus-search-mairix-handle-date expr))
     ((memq (car expr) '(size smaller larger))
      (gnus-search-mairix-handle-size expr))
     ;; Drop regular expressions.
     ((string-match-p "\\`/" (cdr expr))
      nil)
     ;; Turn parenthesized phrases into multiple word terms.  Again,
     ;; this isn't quite what the user is asking for, but better to
     ;; return false positives.
     ((and key (string-match-p "[[:blank:]]" (cdr expr)))
      (mapconcat
       (lambda (s) (format "%s:%s" key s))
       (split-string (gnus-search-mairix-treat-string
		      (cdr expr)))
       " "))
     (key (format "%s:%s" key
		  (gnus-search-mairix-treat-string
		   (cdr expr))))
     (t nil))))

(defun gnus-search-mairix-treat-string (str)
  "Treat string for wildcards.
Mairix accepts trailing wildcards, but not leading.  Also remove
double quotes."
  (replace-regexp-in-string
   "\\`\\*\\|\"" ""
   (replace-regexp-in-string "\\*\\'" "=" str)))

(defun gnus-search-mairix-handle-size (expr)
  "Format a mairix size search.
Assume \"size\" key is equal to \"larger\"."
  (format
   (if (eql (car expr) 'smaller)
       "z:-%s"
     "z:%s-")
   (cdr expr)))

(defun gnus-search-mairix-handle-mark (expr)
  "Format a mairix mark search."
  (let ((mark
	 (pcase (cdr expr)
	   ("flag" "f")
	   ("read" "s")
	   ("seen" "s")
	   ("replied" "r")
	   (_ nil))))
    (when mark
      (format "F:%s" mark))))

(defun gnus-search-mairix-handle-date (expr)
  (let ((str
	 (pcase (cdr expr)
	   (`(nil ,m nil)
	    (substring
	     (nth (1- m) gnus-english-month-names)
	     0 3))
	   (`(nil nil ,y)
	    (number-to-string y))
	   (`(,d ,m nil)
	    (format "%s%02d"
		    (substring
		     (nth (1- m) gnus-english-month-names)
		     0 3)
		    d))
	   (`(nil ,m ,y)
	    (format "%d%s"
		    y (substring
		       (nth (1- m) gnus-english-month-names)
		       0 3)))
	   (`(,d ,m ,y)
	    (format "%d%02d%02d" y m d)))))
    (format
     (pcase (car expr)
       ('date "d:%s")
       ('since "d:%s-")
       ('after "d:%s-")
       ('before "d:-%s"))
     str)))

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-mairix)
						  (qstring string)
						  query &optional _groups)
  (with-slots (switches config-file) engine
    (append `("--rcfile" ,config-file "-r")
	   switches
	   (when (alist-get 'thread query) (list "-t"))
	   (list qstring))))

;;; Find-grep interface

(cl-defmethod gnus-search-transform-expression ((_engine gnus-search-find-grep)
						(_ list))
  ;; Drop everything that isn't a plain string.
  nil)

(cl-defmethod gnus-search-run-search ((engine gnus-search-find-grep)
				      server query
				      &optional groups)
  "Run find and grep to obtain matching articles."
  (let* ((method (gnus-server-to-method server))
	 (sym (intern
	       (concat (symbol-name (car method)) "-directory")))
	 (directory (cadr (assoc sym (cddr method))))
	 (regexp (alist-get 'grep query))
	 (grep-options (slot-value engine 'grep-options))
	 (grouplist (or groups (gnus-search-get-active server)))
	 (buffer (slot-value engine 'proc-buffer)))
    (unless directory
      (signal 'gnus-search-config-error
	      (list (format-message
		     "No directory found in definition of server %s"
		     server))))
    (apply
     #'vconcat
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
					   (string-replace
					    "." "/"
					    group)))
				    group))))))
		     (unless group
		       (signal 'gnus-search-config-error
			       (list
				"Cannot locate directory for group")))
		     (save-excursion
		       (apply
			#'call-process "find" nil t
			"find" group "-maxdepth" "1" "-type" "f"
			"-name" "[0-9]*" "-exec"
			(slot-value engine 'grep-program)
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
					       (cl-subseq path 0 -1)
					       ".")))
			 (push
			  (vector (gnus-group-full-name group server) art 0)
			  artlist))
		       (forward-line 1)))
		   (message "Searching %s using find-grep...done"
			    (or group server))
		   artlist)))
	     grouplist))))

;;; Util Code:

(defun gnus-search-run-query (specs)
  "Invoke appropriate search engine function."
  ;; For now, run the searches synchronously.  At some point
  ;; multiple-server searches can each be run in their own thread,
  ;; allowing concurrent searches of multiple backends.  At present
  ;; this causes problems when searching more than one server that
  ;; uses `nntp-server-buffer', as their return values are written
  ;; interleaved into that buffer.  Anyway, that's the reason for the
  ;; `mapc'.
  (let* ((results [])
	 (prepared-query (gnus-search-prepare-query
			  (alist-get 'search-query-spec specs)))
	 (limit (alist-get 'limit prepared-query)))
    (mapc
     (pcase-lambda (`(,server . ,groups))
       (condition-case err
	   (let ((search-engine (gnus-search-server-to-engine server)))
	     (setq results
		   (vconcat
		    (gnus-search-run-search
		     search-engine server prepared-query groups)
		    results)))
	 (gnus-search-config-error
	  (if (< 1 (length (alist-get 'search-group-spec specs)))
	      (apply #'nnheader-message 4
		     "Search engine for %s improperly configured: %s"
		     server (cdr err))
	    (signal 'gnus-search-config-error err)))))
     (alist-get 'search-group-spec specs))
    ;; Some search engines do their own limiting, but some don't, so
    ;; do it again here.  This is bad because, if the user is
    ;; searching multiple groups, they would reasonably expect the
    ;; limiting to apply to the search results *after sorting*.  Doing
    ;; it this way is liable to, for instance, eliminate all results
    ;; from a later group entirely.
    (if limit
	(seq-subseq results 0 (min limit (length results)))
      results)))

(defun gnus-search-prepare-query (query-spec)
  "Accept a search query in raw format, and prepare it.
QUERY-SPEC is an alist produced by functions such as
`gnus-group-make-search-group', and contains at least a 'query
key, and possibly some meta keys.  This function extracts any
additional meta keys from the 'query string, and parses the
remaining string, then adds all that to the top-level spec."
  (let ((query (alist-get 'query query-spec))
	val)
    (when (stringp query)
      ;; Look for these meta keys:
      (while (string-match
	      "\\(thread\\|grep\\|limit\\|raw\\):\\([^ ]+\\)"
	      query)
	(setq val (match-string 2 query))
	(setf (alist-get (intern (match-string 1 query)) query-spec)
	      ;; This is stupid.
	      (cond
	       ((equal val "t"))
	       ((null (zerop (string-to-number val)))
		(string-to-number val))
	       (t val)))
	(setq query
	      (string-trim (replace-match "" t t query 0)))
	(setf (alist-get 'query query-spec) query)))
    (when (and gnus-search-use-parsed-queries
	       (null (alist-get 'raw query-spec)))
      (setf (alist-get 'parsed-query query-spec)
	    (gnus-search-parse-query query)))
    query-spec))

;; This should be done once at Gnus startup time, when the servers are
;; first opened, and the resulting engine instance attached to the
;; server.
(defun gnus-search-server-to-engine (srv)
  (let* ((method (gnus-server-to-method srv))
	 (engine-config (assoc 'gnus-search-engine (cddr method)))
	 (server (or (cdr-safe
		      (assoc-string srv gnus-search-engine-instance-alist t))
		     (nth 1 engine-config)
		     (cdr-safe (assoc (car method) gnus-search-default-engines))
		     (when-let ((old (assoc 'nnir-search-engine
					    (cddr method))))
		       (nnheader-message
			8 "\"nnir-search-engine\" is no longer a valid parameter")
		       (nth 1 old))))
	 inst)
    (setq server
	  (pcase server
	    ('notmuch 'gnus-search-notmuch)
	    ('namazu 'gnus-search-namazu)
	    ('find-grep 'gnus-search-find-grep)
	    ('imap 'gnus-search-imap)
	    (_ server))
	  inst
	  (cond
	   ((null server) nil)
	   ((eieio-object-p server)
	    server)
	   ((class-p server)
	    (make-instance server))
	   (t nil)))
    (if inst
	(unless (assoc-string srv gnus-search-engine-instance-alist t)
	  (when (cddr engine-config)
	    ;; We're not being completely backward-compatible here,
	    ;; because we're not checking for nnir-specific config
	    ;; options in the server definition.
	    (pcase-dolist (`(,key ,value) (cddr engine-config))
	      (condition-case nil
		  (setf (slot-value inst key) value)
		((invalid-slot-name invalid-slot-type)
		 (nnheader-report 'search
		  "Invalid search engine parameter: (%s %s)"
		  key value)))))
	  (push (cons srv inst) gnus-search-engine-instance-alist))
      (signal 'gnus-search-config-error
	      (list (format-message
		     "No search engine configured for %s" srv))))
    inst))

(declare-function gnus-registry-get-id-key "gnus-registry" (id key))

(defun gnus-search-thread (header)
  "Make an nnselect group based on the thread containing the article
header. The current server will be searched. If the registry is
installed, the server that the registry reports the current
article came from is also searched."
  (let* ((ids (cons (mail-header-id header)
		    (split-string
		     (or (mail-header-references header)
			 ""))))
	 (query
	  (list (cons 'query (mapconcat (lambda (i)
					  (format "id:%s" i))
					ids " or "))
		(cons 'thread t)))
	 (server
	  (list (list (gnus-method-to-server
		       (gnus-find-method-for-group gnus-newsgroup-name)))))
	 (registry-group (and
			  (bound-and-true-p gnus-registry-enabled)
			  (car (gnus-registry-get-id-key
				(mail-header-id header) 'group))))
	 (registry-server
	  (and registry-group
	       (gnus-method-to-server
		(gnus-find-method-for-group registry-group)))))
    (when registry-server
      (cl-pushnew (list registry-server) server :test #'equal))
    (gnus-group-make-search-group nil (list
				       (cons 'search-query-spec query)
				       (cons 'search-group-spec server)))
    (gnus-summary-goto-subject (gnus-id-to-article (mail-header-id header)))))

(defun gnus-search-get-active (srv)
  (let ((method (gnus-server-to-method srv))
	groups)
    (gnus-request-list method)
    (with-current-buffer nntp-server-buffer
      (let ((cur (current-buffer)))
	(goto-char (point-min))
	(unless (or (null gnus-search-ignored-newsgroups)
		    (string= gnus-search-ignored-newsgroups ""))
	  (delete-matching-lines gnus-search-ignored-newsgroups))
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

(defvar gnus-search-minibuffer-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km minibuffer-local-map)
    (define-key km (kbd "TAB") #'completion-at-point)
    km))

(defun gnus-search--complete-key-data ()
  "Potentially return completion data for a search key or value."
  (let* ((key-start (save-excursion
		      (or (re-search-backward " " (minibuffer-prompt-end) t)
			  (goto-char (minibuffer-prompt-end)))
		      (skip-chars-forward " -")
		      (point)))
	 (after-colon (save-excursion
			(when (re-search-backward ":" key-start t)
			  (1+ (point)))))
	 in-string)
    (if after-colon
	;; We're in the value part of a key:value pair, which we
	;; only handle in a contact-completion context.
	(when (and gnus-search-contact-tables
		   (save-excursion
		     (re-search-backward "\\<-?\\(\\w+\\):" key-start t)
		     (member (match-string 1)
			     '("from" "to" "cc"
			       "bcc" "recipient" "address"))))
	  (setq in-string (nth 3 (syntax-ppss)))
	  (list (if in-string (1+ after-colon) after-colon)
		(point) (apply #'completion-table-merge
			       gnus-search-contact-tables)
		:exit-function
		(lambda (str status)
		  ;; If the value contains spaces, make sure it's
		  ;; quoted.
		  (when (and (memql status '(exact finished))
			     (or (string-search " " str)
				 in-string))
		    (unless (looking-at-p "\\s\"")
		      (insert "\""))
		    ;; Unless we already have an opening quote...
		    (unless in-string
		      (save-excursion
			(goto-char after-colon)
			(insert "\"")))))))
      (list
       key-start (point) gnus-search-expandable-keys
       :exit-function (lambda (_s status)
			(when (memql status '(exact finished))
			  (insert ":")))))))

(defun gnus-search-make-spec (arg)
  (list (cons 'query
	      (minibuffer-with-setup-hook
		  (lambda ()
		    (add-hook 'completion-at-point-functions
			      #'gnus-search--complete-key-data
			      nil t))
		(read-from-minibuffer
		 "Query: " nil gnus-search-minibuffer-map
		 nil 'gnus-search-history)))
	(cons 'raw arg)))

(provide 'gnus-search)
;;; gnus-search.el ends here
