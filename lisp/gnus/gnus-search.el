;;; gnus-search.el --- Search facilities for Gnus    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defvar gnus-search-memo-query nil
  "Internal: stores current query.")

(defvar gnus-search-memo-server nil
  "Internal: stores current server.")

(defvar gnus-search-history ()
  "Internal history of Gnus searches.")

(define-error 'gnus-search-parse-error "Gnus search parsing error")

;;; User Customizable Variables:

(defgroup gnus-search nil
  "Search groups in Gnus with assorted search engines."
  :group 'gnus)

(defcustom gnus-search-use-parsed-queries t
  "When t, use Gnus' generalized search language.

The generalized search language is a sort of \"meta search\"
language that can be used across all search engines that Gnus
supports.  See the Gnus manual for details.

If this option is set to nil, search queries will be passed
directly to the search engines without being parsed or
transformed."
  :version "26.3"
  :type 'boolean
  :group 'gnus-search)

(defcustom gnus-search-ignored-newsgroups ""
  "A regexp to match newsgroups in the active file that should
  be skipped when searching."
  :version "24.1"
  :type 'regexp
  :group 'gnus-search)

;; Engine-specific configuration options.

(defcustom gnus-search-swish++-configuration-file
  (expand-file-name "~/Mail/swish++.conf")
  "Location of Swish++ configuration file.

This variable can also be set per-server."
  :type 'file
  :group 'gnus-search)

(defcustom gnus-search-swish++-program "search"
  "Name of swish++ search executable.

This variable can also be set per-server."
  :type 'string
  :group 'gnus-search)

(defcustom gnus-search-swish++-additional-switches '()
  "A list of strings, to be given as additional arguments to swish++.

Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-swish++-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-swish++-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string)
  :group 'gnus-search)

(defcustom gnus-search-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by swish++
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type 'regexp
  :group 'gnus-search)

(defcustom gnus-search-swish++-raw-queries-p nil
  "If t, all Swish++ engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'gnus-search)

(defcustom gnus-search-swish-e-configuration-file
  (expand-file-name "~/Mail/swish-e.conf")
  "Configuration file for swish-e.

This variable can also be set per-server."
  :type 'file
  :group 'gnus-search)

(defcustom gnus-search-swish-e-program "search"
  "Name of swish-e search executable.

This variable can also be set per-server."
  :type 'string
  :group 'gnus-search)

(defcustom gnus-search-swish-e-additional-switches '()
  "A list of strings, to be given as additional arguments to swish-e.

Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-swish-e-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-swish-e-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string)
  :group 'gnus-search)

(defcustom gnus-search-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by swish-e
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :type 'regexp
  :group 'gnus-search)

(defcustom gnus-search-swish-e-index-files '()
  "A list of index files to use with this Swish-e instance.

This variable can also be set per-server."
  :type '(repeat file)
  :group 'gnus-search)

(defcustom gnus-search-swish-e-raw-queries-p nil
  "If t, all Swish-e engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'gnus-search)

;; Namazu engine, see <URL:http://www.namazu.org/>

(defcustom gnus-search-namazu-program "namazu"
  "Name of Namazu search executable.

This variable can also be set per-server."
  :type 'string
  :group 'gnus-search)

(defcustom gnus-search-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "Index directory for Namazu.

This variable can also be set per-server."
  :type 'directory
  :group 'gnus-search)

(defcustom gnus-search-namazu-additional-switches '()
  "A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other switches
make any sense in this context.

Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-namazu-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-namazu-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :type '(repeat string)
  :group 'gnus-search)

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
  :group 'gnus-search)

(defcustom gnus-search-namazu-raw-queries-p nil
  "If t, all Namazu engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'gnus-search)

(defcustom gnus-search-notmuch-program "notmuch"
  "Name of notmuch search executable.

This variable can also be set per-server."
  :version "24.1"
  :type '(string)
  :group 'gnus-search)

(defcustom gnus-search-notmuch-configuration-file
  (expand-file-name "~/.notmuch-config")
  "Configuration file for notmuch.

This variable can also be set per-server."
  :type 'file
  :group 'gnus-search)

(defcustom gnus-search-notmuch-additional-switches '()
  "A list of strings, to be given as additional arguments to notmuch.

Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-notmuch-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnus-search-notmuch-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :version "24.1"
  :type '(repeat string)
  :group 'gnus-search)

(defcustom gnus-search-notmuch-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by notmuch
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :version "24.1"
  :type 'regexp
  :group 'gnus-search)

(defcustom gnus-search-notmuch-raw-queries-p nil
  "If t, all Notmuch engines will only accept raw search query
  strings."
  :type 'boolean
  :version "26.3"
  :group 'gnus-search)

(defcustom gnus-search-imap-raw-queries-p nil
  "If t, all IMAP engines will only accept raw search query
  strings."
  :version "26.3"
  :type 'boolean
  :group 'gnus-search)

(defcustom gnus-search-mairix-program "mairix"
  "Name of mairix search executable.

This variable can also be set per-server."
  :version "26.3"
  :type 'string
  :group 'gnus-search)

(defcustom gnus-search-mairix-configuration-file
  (expand-file-name "~/.mairixrc")
  "Configuration file for mairix.

This variable can also be set per-server."
  :version "26.3"
  :type 'file
  :group 'gnus-search)

(defcustom gnus-search-mairix-additional-switches '()
  "A list of strings, to be given as additional arguments to mairix.

Note that this should be a list.  I.e., do NOT use the following:
    (setq gnus-search-mairix-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq gnu-search-mairix-additional-switches \\='(\"-i\" \"-w\"))

This variable can also be set per-server."
  :version "26.3"
  :type '(repeat string)
  :group 'gnus-search)

(defcustom gnus-search-mairix-remove-prefix (concat (getenv "HOME") "/Mail/")
  "The prefix to remove from each file name returned by mairix
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable can also be set per-server."
  :version "26.3"
  :type 'regexp
  :group 'gnus-search)

(defcustom gnus-search-mairix-raw-queries-p nil
  "If t, all Mairix engines will only accept raw search query
  strings."
  :version "26.3"
  :type 'boolean
  :group 'gnus-search)

(defcustom gnus-search-imap-raw-queries-p nil
  "If t, all IMAP engines will only accept raw search query
  strings."
  :version "26.3"
  :type 'boolean
  :group 'gnus-search)

;; Options for search language parsing.

(defcustom gnus-search-expandable-keys
  '("from" "subject" "to" "cc" "bcc" "body" "recipient" "date"
    "mark" "contact" "contact-from" "contact-to" "before" "after"
    "larger" "smaller" "attachment" "text" "since" "thread"
    "sender" "address" "tag" "size")
  "A list of strings representing expandable search keys.

\"Expandable\" simply means the key can be abbreviated while
typing in search queries, ie \"subject\" could be entered as
\"subj\" or even \"su\", though \"s\" is ambigous between
\"subject\" and \"since\".

Keys can contain hyphens, in which case each section will be
expanded separately.  \"cont\" will expand to \"contact\", for
instance, while \"c-t\" will expand to \"contact-to\".

Ambiguous abbreviations will raise an error."
  :group 'gnus-search
  :version "26.1"
  :type '(repeat string))

(defcustom gnus-search-date-keys
  '("date" "before" "after" "on" "senton" "sentbefore" "sentsince" "since")
  "A list of keywords whose value should be parsed as a date.

See the docstring of `gnus-search-parse-query' for information on
date parsing."
  :group 'gnus-search
  :version "26.1"
  :type '(repeat string))

(defcustom gnus-search-contact-sources nil
  "A list of sources used to search for messages from contacts.

Each list element can be either a function, or an alist.
Functions should accept a search string, and return a list of
email addresses of matching contacts.  An alist should map single
strings to lists of mail addresses, usable as search keys in mail
headers."
  :group 'gnus-search
  :version "26.1"
  :type '(repeat (choice function
			 (alist
			  :key-type string
			  :value-type (repeat string)))))

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
are implictly ANDed; the \"or\" keyword can be used to
OR. \"not\" will negate the following expression, or keys can be
prefixed with a \"-\".  The \"near\" operator will work for
engines that understand it; other engines will convert it to
\"or\".  Parenthetical groups work as expected.

A key that matches the name of a mail header will search that
header.

Search keys can be abbreviated so long as they remain
unambiguous, ie \"f\" will search the \"from\" header. \"s\" will
raise an error.

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
\"keyword\" in IMAP, and left as \"tag\" for notmuch. At some
point this should also be used to search marks in the Gnus
registry.

\"contact\" will search messages to/from a contact.  Contact
management packages must push a function onto
`gnus-search-contact-sources', the docstring of which see, for
this to work.

\"contact-from\" does what you'd expect.

\"contact-to\" searches the same headers as \"recipient\".

Other keys can be specified, provided that the search backends
know how to interpret them.

Date values (any key in `gnus-search-date-keys') can be provided
in any format that `parse-time-string' can parse (note that this
can produce weird results).  Dates with missing bits will be
interpreted as the most recent occurance thereof (ie \"march 03\"
is the most recent March 3rd).  Lastly, relative specifications
such as 1d (one day ago) are understood.  This also accepts w, m,
and y.  m is assumed to be 30 days.

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
     ((and (eq next 'near))
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

In the simplest case, they are simply consed together.  KEY comes
in as a string, goes out as a symbol."
  (let (return)
    (cond
     ((member key gnus-search-date-keys)
      (when (string= "after" key)
	(setq key "since"))
      (setq value (gnus-search-query-parse-date value)))
     ((string-match-p "contact" key)
      (setq return (gnus-search-query-parse-contact key value)))
     ((equal key "address")
      (setq return `(or (sender . ,value) (recipient . ,value))))
     ((equal key "mark")
      (setq value (gnus-search-query-parse-mark value))))
    (or return
	(cons (intern key) value))))

(defun gnus-search-query-parse-date (value &optional rel-date)
  "Interpret VALUE as a date specification.

See the docstring of `gnus-search-parse-query' for details.

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

(defun gnus-search-query-parse-contact (key value)
  "Handle VALUE as the name of a contact.

Runs VALUE through the elements of
`gnus-search-contact-sources' until one of them returns a list
of email addresses.  Turns those addresses into an appropriate
chunk of query syntax."
  (let ((funcs (or (copy-sequence gnus-search-contact-sources)
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

(defun gnus-search-query-expand-key (key)
  "Attempt to expand KEY to a full keyword."
  (let ((bits (split-string key "-"))
	bit out-bits comp)
    (if (try-completion (car bits) gnus-search-expandable-keys)
	(progn
	  (while (setq bit (pop bits))
	    (setq comp (try-completion bit gnus-search-expandable-keys))
	    (if (stringp comp)
		(if (and (string= bit comp)
			 (null (member comp gnus-search-expandable-keys)))
		    (signal 'gnus-search-parse-error
			    (list (format "Ambiguous keyword: %s" key)))
		  (push comp out-bits))
	      (push bit out-bits)))
	  (mapconcat #'identity (reverse out-bits) "-"))
      key)))

;; (defun gnus-search-query-expand-key (key)
;;   "Attempt to expand (possibly abbreviated) KEY to a full keyword.

;; Can handle any non-ambiguous abbreviation, with hyphens as substring separator."
;;   (let* ((bits (split-string key "-"))
;; 	 (bit (pop bits))
;; 	 (comp (all-completions bit gnus-search-expandable-keys)))
;;     ;; Make a cl-labels recursive function, that accepts a rebuilt key and
;;     ;; results of `all-completions' back in as a COLLECTION argument.
;;     (if (= 1 (length comp))
;; 	(setq key (car comp))
;;       (when (setq comp (try-completion bit gnus-search-expandable-keys))
;; 	(if (and (string= bit comp)
;; 		 (null (member comp gnus-search-expandable-keys)))
;; 	    (error "Ambiguous keyword: %s" key)))
;;       (unless (eq t (try-completion key gnus-search-expandable-keys))))
;;     key))


(defun gnus-search-query-return-string (&optional delimited trim)
  "Return a string from the current buffer.

If DELIMITED is non-nil, assume the next character is a delimiter
character, and return everything between point and the next
occurance of the delimiter, including the delimiters themselves.
If TRIM is non-nil, do not return the delimiters. Otherwise,
return one word."
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
  (skip-chars-forward "[[:blank:]]")
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
			 (get-buffer-create
			  (generate-new-buffer-name " *gnus-search-"))))
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
    be set manually.  Currently only partially implemented."))
    :documentation
  "The base IMAP search engine, using an IMAP server's search capabilites.

This backend may be subclassed to handle particular IMAP servers'
quirks.")

(eieio-oset-default 'gnus-search-imap 'raw-queries-p
		    gnus-search-imap-raw-queries-p)

(defclass gnus-search-find-grep (gnus-search-engine
				 gnus-search-process
				 gnus-search-grep)
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

(defclass gnus-search-indexed (gnus-search-engine
			       gnus-search-process
			       gnus-search-grep)
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
		    gnus-search-swish-e-program)

(eieio-oset-default 'gnus-search-swish-e 'prefix
		    gnus-search-swish-e-remove-prefix)

(eieio-oset-default 'gnus-search-swish-e 'index-files
		    gnus-search-swish-e-index-files)

(eieio-oset-default 'gnus-search-swish-e 'switches
		    gnus-search-swish-e-additional-switches)

(eieio-oset-default 'gnus-search-swish-e 'raw-queries-p
		    gnus-search-swish-e-raw-queries-p)

(defclass gnus-search-swish++ (gnus-search-indexed)
  ((config-file
    :init-arg :config-file
    :type string)))

(eieio-oset-default 'gnus-search-swish++ 'program
		    gnus-search-swish++-program)

(eieio-oset-default 'gnus-search-swish++ 'prefix
		    gnus-search-swish++-remove-prefix)

(eieio-oset-default 'gnus-search-swish++ 'config-file
		    gnus-search-swish++-configuration-file)

(eieio-oset-default 'gnus-search-swish++ 'switches
		    gnus-search-swish++-additional-switches)

(eieio-oset-default 'gnus-search-swish++ 'raw-queries-p
		    gnus-search-swish++-raw-queries-p)

(defclass gnus-search-mairix (gnus-search-indexed)
  ((config-file
    :initarg :config-file
    :type string
    :custom file)))

(eieio-oset-default 'gnus-search-mairix 'program
		    gnus-search-mairix-program)

(eieio-oset-default 'gnus-search-mairix 'switches
		    gnus-search-mairix-additional-switches)

(eieio-oset-default 'gnus-search-mairix 'prefix
		    gnus-search-mairix-remove-prefix)

(eieio-oset-default 'gnus-search-mairix 'config-file
		    gnus-search-mairix-configuration-file)

(eieio-oset-default 'gnus-search-mairix 'raw-queries-p
		    gnus-search-mairix-raw-queries-p)

(defclass gnus-search-namazu (gnus-search-indexed)
  ((index-dir
    :initarg :index-dir
    :type string
    :custom directory)))

(eieio-oset-default 'gnus-search-namazu 'program
		    gnus-search-namazu-program)

(eieio-oset-default 'gnus-search-namazu 'index-dir
		    gnus-search-namazu-index-directory)

(eieio-oset-default 'gnus-search-namazu 'switches
		    gnus-search-namazu-additional-switches)

(eieio-oset-default 'gnus-search-namazu 'prefix
		    gnus-search-namazu-remove-prefix)

(eieio-oset-default 'gnus-search-namazu 'raw-queries-p
		    gnus-search-namazu-raw-queries-p)

(defclass gnus-search-notmuch (gnus-search-indexed)
  ((config-file
    :init-arg :config-file
    :type string)))

(eieio-oset-default 'gnus-search-notmuch 'program
		    gnus-search-notmuch-program)

(eieio-oset-default 'gnus-search-notmuch 'switches
		    gnus-search-notmuch-additional-switches)

(eieio-oset-default 'gnus-search-notmuch 'prefix
		    gnus-search-notmuch-remove-prefix)

(eieio-oset-default 'gnus-search-notmuch 'config-file
		    gnus-search-notmuch-configuration-file)

(eieio-oset-default 'gnus-search-notmuch 'raw-queries-p
		    gnus-search-notmuch-raw-queries-p)

(defcustom gnus-search-default-engines '((nnimap gnus-search-imap)
					 (nntp  gnus-search-gmane))
  "Alist of default search engines keyed by server method."
  :version "26.1"
  :group 'gnus-search
  :type `(repeat (list (choice (const nnimap) (const nntp) (const nnspool)
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
  (if (and gnus-search-use-parsed-queries
	   (null (alist-get 'raw query-spec))
	   (null (slot-value engine 'raw-queries-p)))
      (gnus-search-transform
       engine (alist-get 'parsed-query query-spec))
    (alist-get 'query query-spec)))

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
						(_expr (eql and)))
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

;; imap interface
(cl-defmethod gnus-search-run-search ((engine gnus-search-imap)
				      srv query groups)
  (save-excursion
    (let ((server (cadr (gnus-server-to-method srv)))
          (gnus-inhibit-demon t)
	  q-string)
      (message "Opening server %s" server)
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
			   (gnus-search-imap-search-command engine q-string)))
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

(cl-defmethod gnus-search-imap-search-command ((engine gnus-search-imap)
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
(defvar gnus-search-imap-search-keys
  '(body cc bcc from header keyword larger smaller subject text to uid)
  "Known IMAP search keys, excluding booleans and date keys.")

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
	(format "OR %s %s" left right)
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

   Search keyword.  All IMAP search keywords that take a value
   are supported directly.  Keywords that are boolean are
   supported through other means (usually the \"mark\" keyword)."
  (let ((fuzzy-supported (slot-value engine 'fuzzy))
	(fuzzy ""))
    (cl-case (car expr)
      (date (setcar expr 'on))
      (tag (setcar expr 'keyword))
      (sender (setcar expr 'from)))
    (cond
     ((consp (car expr))
      (format "(%s)" (gnus-search-transform engine expr)))
     ((eq (car expr) 'recipient)
      (gnus-search-transform
       engine (gnus-search-parse-query
	       (format
	       "to:%s or (cc:%s or bcc:%s)"
	       (cdr expr) (cdr expr) (cdr expr)))))
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
	  (format "HEADER Message-ID %s" (cdr expr)))
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

(cl-defmethod gnus-search-imap-handle-string ((engine gnus-search-imap)
					      (str string))
  (with-slots (literal-plus) engine
    ;; If string is non-ASCII...
    (if (null (= (length str)
		 (string-bytes str)))
	;; If LITERAL+ is available, use it and force UTF-8.
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

(cl-defgeneric gnus-search-indexed-parse-output (engine server &optional groups)
  "Parse the results of ENGINE's query against SERVER in GROUPS.

Locally-indexed search engines return results as a list of
filenames, sometimes with additional information.  Returns a list
of viable results, in the form of a list of [group article score]
vectors.")

(cl-defgeneric gnus-search-index-extract (engine)
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
	  (display-buffer buffer))))))

(cl-defmethod gnus-search-indexed-parse-output ((engine gnus-search-indexed)
						server query &optional groups)
  (let ((prefix (slot-value engine 'prefix))
	(group-regexp (when groups
			(regexp-opt
			 (mapcar
			  (lambda (x) (gnus-group-real-name x))
			  groups))))
	artlist vectors article group)
    (goto-char (point-min))
    (while (not (eobp))
      (pcase-let ((`(,f-name ,score) (gnus-search-indexed-extract engine)))
	(when (and (file-readable-p f-name)
		   (null (file-directory-p f-name))
		   (or (null groups)
		       (string-match-p group-regexp f-name)))
	  (push (list f-name score) artlist))))
    ;; Are we running an additional grep query?
    (when-let ((grep-reg (alist-get 'grep query)))
      (setq artlist (gnus-search-grep-search engine artlist grep-reg)))
    (pcase-dolist (`(,f-name ,score) artlist)
      (setq article (file-name-nondirectory f-name))
      ;; Remove prefix.
      (when (and prefix
		 (file-name-absolute-p prefix)
		 (string-match (concat "^"
				       (file-name-as-directory prefix))
			       f-name))
	(setq group (replace-match "" t t (file-name-directory f-name))))
      ;; Break the directory name down until it's something that
      ;; (probably) can be used as a group name.
      (setq group
	    (replace-regexp-in-string
	     "[/\\]" "."
	     (replace-regexp-in-string
	      "/?\\(cur\\|new\\|tmp\\)?/\\'" ""
	      (replace-regexp-in-string
	       "^[./\\]" ""
	       group nil t)
	      nil t)
	     nil t))

      (push (vector (gnus-group-full-name group server)
		    (if (string-match-p "\\`[[:digit:]]+\\'" article)
			(string-to-number article)
		      (nnmaildir-base-name-to-article-number
		       (substring article 0 (string-match ":" article))
		       group nil))
		    (if (numberp score)
			score
		      (string-to-number score)))
	    vectors))
    vectors))

(cl-defmethod gnus-search-indexed-extract ((engine gnus-search-indexed))
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

(cl-defmethod gnus-search-indexed-extract ((engine gnus-search-swish++))
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

(cl-defmethod gnus-search-indexed-extract ((engine gnus-search-swish-e))
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
    (with-slots (switches index-dir) engine
      (nconc
       (list "-q"			; don't be verbose
	     "-a"			; show all matches
	     "-s") 			; use short format
       (when max (list (format "--max=%d" max)))
       switches
       (list qstring index-dir)))))

(cl-defmethod gnus-search-indexed-extract ((engine gnus-search-namazu))
  "Extract a single message result for Namazu.

Namazu provides a little more information, for instance a score."

  (when (re-search-forward
	 "^\\([0-9,]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
	 nil t)
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
      (format "(%s)") (gnus-search-transform engine expr))
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

(cl-defmethod gnus-search-indexed-search-command ((engine gnus-search-notmuch)
						  (qstring string)
						  query &optional _groups)
  ;; Theoretically we could use the GROUPS parameter to pass a
  ;; --folder switch to notmuch, but I'm not confident of getting the
  ;; format right.
  (let ((limit (alist-get 'limit query)))
   (with-slots (switches config-file) engine
     `(,(format "--config=%s" config-file)
       "search"
       "--output=files"
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
    (replace-regexp-in-string
     ":"
     (if (eql (caadr expr) 'mark)
	 ":-"
       ":~")
     next)))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-mairix)
						(expr (head or)))
  "Handle Mairix \"or\" statement.

Mairix only accepts \"or\" expressions on homogenous keys.  We
cast \"or\" expressions on heterogenous keys as \"and\", which
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
    (nconc `("--rcfile" ,config-file "-r")
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
			(slot-value engine 'grep-program)
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

(declare-function mm-url-insert "mm-url" (url &optional follow-refresh))
(declare-function mm-url-encode-www-form-urlencoded "mm-url" (pairs))

;; gmane interface
(cl-defmethod gnus-search-run-search ((engine gnus-search-gmane)
				      srv query &optional groups)
  "Run a search against a gmane back-end server."
  (let* ((case-fold-search t)
	 (groupspec (mapconcat
		     (lambda (x)
		       (if (string-match-p "gmane" x)
			   (format "group:%s" (gnus-group-short-name x))
			 (error "Can't search non-gmane groups: %s" x)))
		     groups " "))
	 (buffer (slot-value engine 'proc-buffer))
	 (search (concat (gnus-search-make-query-string engine query)
			 " "
			 groupspec))
	 (gnus-inhibit-demon t)
	 artlist)
    (require 'mm-url)
    (with-current-buffer buffer
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

(cl-defmethod gnus-search-transform-expression ((_e gnus-search-gmane)
						(_expr (head near)))
  nil)

;; Can Gmane handle OR or NOT keywords?
(cl-defmethod gnus-search-transform-expression ((_e gnus-search-gmane)
						(_expr (head or)))
  nil)

(cl-defmethod gnus-search-transform-expression ((_e gnus-search-gmane)
						(_expr (head not)))
  nil)

(cl-defmethod gnus-search-transform-expression ((_e gnus-search-gmane)
						(expr list))
  "The only keyword value gmane can handle is author, ie from."
  (cond
   ((memq (car expr) '(from sender author address))
    (format "author:%s" (cdr expr)))
   ((eql (car expr) 'body)
    (cdr expr))))

;;; Util Code:

(defun gnus-search-run-query (specs)
  "Invoke appropriate search engine function."
  ;; For now, run the searches synchronously.  At some point each
  ;; search can be run in its own thread, allowing concurrent searches
  ;; of multiple backends.  At present this causes problems when
  ;; multiple IMAP servers are searched at the same time, apparently
  ;; because the `nntp-server-buffer' variable is getting clobbered,
  ;; or something.  Anyway, that's the reason for the `mapc'.
  (let ((results [])
	(prepared-query (gnus-search-prepare-query
			 (alist-get 'search-query-spec specs))))
    (mapc
     (lambda (x)
       (let* ((server (car x))
	      (search-engine (gnus-search-server-to-engine server))
	      (groups (cdr x)))
	 (setq results
	       (vconcat
		(gnus-search-run-search
		 search-engine server prepared-query groups)
		results))))
     (alist-get 'search-group-spec specs))
    results))

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
	      "\\(thread\\|grep\\|limit\\|raw\\|count\\):\\([^ ]+\\)"
	      query)
	(setq val (match-string 2 query))
	(setf (alist-get (intern (match-string 1 query)) query-spec)
	      ;; This is stupid.
	      (cond
	       ((eql val 't))
	       ((null (zerop (string-to-number val)))
		(string-to-number val))
	       (t val)))
	(setq query
	      (string-trim (replace-match "" t t query 0)))
	(setf (alist-get 'query query-spec) query)))
    (when gnus-search-use-parsed-queries
      (setf (alist-get 'parsed-query query-spec)
	    (gnus-search-parse-query query)))
    query-spec))

;; This should be done once at Gnus startup time, when the servers are
;; first opened, and the resulting engine instance attached to the
;; server.
(defun gnus-search-server-to-engine (server)
  (let* ((server
	  (or (assoc 'gnus-search-engine
		     (cddr (gnus-server-to-method server)))
	      (assoc (car (gnus-server-to-method server))
		     gnus-search-default-engines)))
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

(autoload 'nnimap-make-thread-query "nnimap")
(declare-function gnus-registry-get-id-key "gnus-registry" (id key))

(defun gnus-search-thread (header)
  "Make an nnselect group based on the thread containing the article
header. The current server will be searched. If the registry is
installed, the server that the registry reports the current
article came from is also searched."
  (let* ((query
	  (list (cons 'query (nnimap-make-thread-query header))))
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
				     (cons 'gnus-search-query-spec query)
				     (cons 'gnus-search-group-spec server)))
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

(autoload 'nnselect-categorize "nnselect" nil nil)
(autoload 'gnus-group-topic-name "gnus-topic" nil nil)
(defvar gnus-group-marked)
(defvar gnus-topic-alist)

(defun gnus-search-make-specs (nnir-extra-parms &optional specs)
  (let* ((group-spec
	  (or (cdr (assq 'search-group-spec specs))
	      (if (gnus-server-server-name)
		  (list (list (gnus-server-server-name)))
		(nnselect-categorize
		 (or gnus-group-marked
		     (if (gnus-group-group-name)
			 (list (gnus-group-group-name))
		       (cdr (assoc (gnus-group-topic-name) gnus-topic-alist))))
		 gnus-group-server))))
	 (query-spec
	  (or (cdr (assq 'search-query-spec specs))
	      (list (cons 'query
			  (read-string "Query: " nil 'gnus-search-history))
		    (cons 'no-parse arg)))))
    (list (cons 'search-query-spec query-spec)
	  (cons 'search-group-spec group-spec))))

;;; Interface with Gnus and nnselect

(declare-function gnus-registry-get-id-key "gnus-registry" (id key))
(declare-function gnus-group-topic-name "gnus-topic" ())

;; Temporary to make group creation easier

(defun gnus-group-make-permanent-search-group (&optional arg specs)
  (interactive "P")
  (gnus-group-make-search-group arg t specs))

(defun gnus-group-make-search-group (&optional arg perm specs)
  "Create an nnselect group based on a search.  Prompt for a
search query and determine the groups to search as follows: if
called from the *Server* buffer search all groups belonging to
the server on the current line; if called from the *Group* buffer
search any marked groups, or the group on the current line, or
all the groups under the current topic. Calling with a prefix-arg
means the search query will be passed raw to the . A
non-nil `specs' arg must be an alist with `search-query-spec' and
`search-group-spec' keys, and skips all prompting."
  (interactive "P")
  (let* ((group-spec
	  (or (cdr (assq 'search-group-spec specs))
	    (if (gnus-server-server-name)
		(list (list (gnus-server-server-name)))
	      (nnselect-categorize
	       (or gnus-group-marked
		   (if (gnus-group-group-name)
		       (list (gnus-group-group-name))
		     (cdr (assoc (gnus-group-topic-name) gnus-topic-alist))))
	       gnus-group-server))))
	 (query-spec
	  (or (cdr (assq 'search-query-spec specs))
	    (list (cons 'query
			(read-string "Query: " nil 'gnus-search-history))
		  (cons 'no-parse arg)))))
    (if perm
	(let ((name (read-string "Group name: " nil)))
	  (gnus-group-make-group
	   name
	   (list 'nnselect "nnselect")
	   nil
	   (list
	    (cons 'nnselect-specs
		  (list
		   (cons 'nnselect-function 'gnus-search-run-query)
		   (cons 'nnselect-args
			 (list (cons 'search-query-spec query-spec)
			       (cons 'search-group-spec group-spec))))))))
      (gnus-group-read-ephemeral-group
       (concat "nnselect-" (message-unique-id))
       (list 'nnselect "nnselect")
       nil
       (cons (current-buffer) gnus-current-window-configuration)
					;     nil
       nil nil
       (list
	(cons 'nnselect-specs
	      (list
	       (cons 'nnselect-function 'gnus-search-run-query)
	       (cons 'nnselect-args
		     (list (cons 'search-query-spec query-spec)
			   (cons 'search-group-spec group-spec)))))
	(cons 'nnselect-artlist nil))))))

(provide 'gnus-search)
;;; gnus-search.el ends here
