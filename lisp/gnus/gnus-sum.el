;;; gnus-sum.el --- summary mode commands for Gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-group)
(require 'gnus-spec)
(require 'gnus-range)
(require 'gnus-int)
(require 'gnus-undo)
(require 'gnus-util)
(require 'mm-decode)
;; Recursive :-(.
;; (require 'gnus-art)
(require 'nnoo)
(autoload 'gnus-summary-limit-include-cached "gnus-cache" nil t)
(autoload 'gnus-cache-write-active "gnus-cache")
(autoload 'mm-uu-dissect "mm-uu")

(defcustom gnus-kill-summary-on-exit t
  "*If non-nil, kill the summary buffer when you exit from it.
If nil, the summary will become a \"*Dead Summary*\" buffer, and
it will be killed sometime later."
  :group 'gnus-summary-exit
  :type 'boolean)

(defcustom gnus-fetch-old-headers nil
  "*Non-nil means that Gnus will try to build threads by grabbing old headers.
If an unread article in the group refers to an older, already read (or
just marked as read) article, the old article will not normally be
displayed in the Summary buffer.  If this variable is non-nil, Gnus
will attempt to grab the headers to the old articles, and thereby
build complete threads.  If it has the value `some', only enough
headers to connect otherwise loose threads will be displayed.  This
variable can also be a number.  In that case, no more than that number
of old headers will be fetched.  If it has the value `invisible', all
old headers will be fetched, but none will be displayed.

The server has to support NOV for any of this to work."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const some)
		 number
		 (sexp :menu-tag "other" t)))

(defcustom gnus-refer-thread-limit 200
  "*The number of old headers to fetch when doing \\<gnus-summary-mode-map>\\[gnus-summary-refer-thread].
If t, fetch all the available old headers."
  :group 'gnus-thread
  :type '(choice number
		 (sexp :menu-tag "other" t)))

(defcustom gnus-summary-make-false-root 'adopt
  "*nil means that Gnus won't gather loose threads.
If the root of a thread has expired or been read in a previous
session, the information necessary to build a complete thread has been
lost.  Instead of having many small sub-threads from this original thread
scattered all over the summary buffer, Gnus can gather them.

If non-nil, Gnus will try to gather all loose sub-threads from an
original thread into one large thread.

If this variable is non-nil, it should be one of `none', `adopt',
`dummy' or `empty'.

If this variable is `none', Gnus will not make a false root, but just
present the sub-threads after another.
If this variable is `dummy', Gnus will create a dummy root that will
have all the sub-threads as children.
If this variable is `adopt', Gnus will make one of the \"children\"
the parent and mark all the step-children as such.
If this variable is `empty', the \"children\" are printed with empty
subject fields.	 (Or rather, they will be printed with a string
given by the `gnus-summary-same-subject' variable.)"
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const none)
		 (const dummy)
		 (const adopt)
		 (const empty)))

(defcustom gnus-summary-gather-exclude-subject "^ *$\\|^(none)$"
  "*A regexp to match subjects to be excluded from loose thread gathering.
As loose thread gathering is done on subjects only, that means that
there can be many false gatherings performed.  By rooting out certain
common subjects, gathering might become saner."
  :group 'gnus-thread
  :type 'regexp)

(defcustom gnus-summary-gather-subject-limit nil
  "*Maximum length of subject comparisons when gathering loose threads.
Use nil to compare full subjects.  Setting this variable to a low
number will help gather threads that have been corrupted by
newsreaders chopping off subject lines, but it might also mean that
unrelated articles that have subject that happen to begin with the
same few characters will be incorrectly gathered.

If this variable is `fuzzy', Gnus will use a fuzzy algorithm when
comparing subjects."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const fuzzy)
		 (sexp :menu-tag "on" t)))

(defcustom gnus-simplify-subject-functions nil
  "List of functions taking a string argument that simplify subjects.
The functions are applied recursively.

Useful functions to put in this list include: `gnus-simplify-subject-re',
`gnus-simplify-subject-fuzzy' and `gnus-simplify-whitespace'."
  :group 'gnus-thread
  :type '(repeat function))

(defcustom gnus-simplify-ignored-prefixes nil
  "*Regexp, matches for which are removed from subject lines when simplifying fuzzily."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 regexp))

(defcustom gnus-build-sparse-threads nil
  "*If non-nil, fill in the gaps in threads.
If `some', only fill in the gaps that are needed to tie loose threads
together.  If `more', fill in all leaf nodes that Gnus can find.  If
non-nil and non-`some', fill in all gaps that Gnus manages to guess."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const some)
		 (const more)
		 (sexp :menu-tag "all" t)))

(defcustom gnus-summary-thread-gathering-function
  'gnus-gather-threads-by-subject
  "*Function used for gathering loose threads.
There are two pre-defined functions: `gnus-gather-threads-by-subject',
which only takes Subjects into consideration; and
`gnus-gather-threads-by-references', which compared the References
headers of the articles to find matches."
  :group 'gnus-thread
  :type '(radio (function-item gnus-gather-threads-by-subject)
		(function-item gnus-gather-threads-by-references)
		(function :tag "other")))

(defcustom gnus-summary-same-subject ""
  "*String indicating that the current article has the same subject as the previous.
This variable will only be used if the value of
`gnus-summary-make-false-root' is `empty'."
  :group 'gnus-summary-format
  :type 'string)

(defcustom gnus-summary-goto-unread t
  "*If t, many commands will go to the next unread article.
This applies to marking commands as well as other commands that
\"naturally\" select the next article, like, for instance, `SPC' at
the end of an article.

If nil, the marking commands do NOT go to the next unread article
(they go to the next article instead).  If `never', commands that
usually go to the next unread article, will go to the next article,
whether it is read or not."
  :group 'gnus-summary-marks
  :link '(custom-manual "(gnus)Setting Marks")
  :type '(choice (const :tag "off" nil)
		 (const never)
		 (sexp :menu-tag "on" t)))

(defcustom gnus-summary-default-score 0
  "*Default article score level.
All scores generated by the score files will be added to this score.
If this variable is nil, scoring will be disabled."
  :group 'gnus-score-default
  :type '(choice (const :tag "disable")
		 integer))

(defcustom gnus-summary-zcore-fuzz 0
  "*Fuzziness factor for the zcore in the summary buffer.
Articles with scores closer than this to `gnus-summary-default-score'
will not be marked."
  :group 'gnus-summary-format
  :type 'integer)

(defcustom gnus-simplify-subject-fuzzy-regexp nil
  "*Strings to be removed when doing fuzzy matches.
This can either be a regular expression or list of regular expressions
that will be removed from subject strings if fuzzy subject
simplification is selected."
  :group 'gnus-thread
  :type '(repeat regexp))

(defcustom gnus-show-threads t
  "*If non-nil, display threads in summary mode."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-hide-subtree nil
  "*If non-nil, hide all threads initially.
If threads are hidden, you have to run the command
`gnus-summary-show-thread' by hand or use `gnus-select-article-hook'
to expose hidden threads."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-hide-killed t
  "*If non-nil, hide killed threads automatically."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-ignore-subject t
  "*If non-nil, which is the default, ignore subjects and do all threading based on the Reference header.
If nil, articles that have different subjects from their parents will
start separate threads."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-operation-ignore-subject t
  "*If non-nil, subjects will be ignored when doing thread commands.
This affects commands like `gnus-summary-kill-thread' and
`gnus-summary-lower-thread'.

If this variable is nil, articles in the same thread with different
subjects will not be included in the operation in question.  If this
variable is `fuzzy', only articles that have subjects that are fuzzily
equal will be included."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const fuzzy)
		 (sexp :tag "on" t)))

(defcustom gnus-thread-indent-level 4
  "*Number that says how much each sub-thread should be indented."
  :group 'gnus-thread
  :type 'integer)

(defcustom gnus-auto-extend-newsgroup t
  "*If non-nil, extend newsgroup forward and backward when requested."
  :group 'gnus-summary-choose
  :type 'boolean)

(defcustom gnus-auto-select-first t
  "*If nil, don't select the first unread article when entering a group.
If this variable is `best', select the highest-scored unread article
in the group.  If t, select the first unread article.

This variable can also be a function to place point on a likely
subject line.  Useful values include `gnus-summary-first-unread-subject',
`gnus-summary-first-unread-article' and
`gnus-summary-best-unread-article'.

If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in
`gnus-select-group-hook'."
  :group 'gnus-group-select
  :type '(choice (const :tag "none" nil)
		 (const best)
		 (sexp :menu-tag "first" t)
		 (function-item gnus-summary-first-unread-subject)
		 (function-item gnus-summary-first-unread-article)
		 (function-item gnus-summary-best-unread-article)))

(defcustom gnus-auto-select-next t
  "*If non-nil, offer to go to the next group from the end of the previous.
If the value is t and the next newsgroup is empty, Gnus will exit
summary mode and go back to group mode.	 If the value is neither nil
nor t, Gnus will select the following unread newsgroup.	 In
particular, if the value is the symbol `quietly', the next unread
newsgroup will be selected without any confirmation, and if it is
`almost-quietly', the next group will be selected without any
confirmation if you are located on the last article in the group.
Finally, if this variable is `slightly-quietly', the `Z n' command
will go to the next group without confirmation."
  :group 'gnus-summary-maneuvering
  :type '(choice (const :tag "off" nil)
		 (const quietly)
		 (const almost-quietly)
		 (const slightly-quietly)
		 (sexp :menu-tag "on" t)))

(defcustom gnus-auto-select-same nil
  "*If non-nil, select the next article with the same subject.
If there are no more articles with the same subject, go to
the first unread article."
  :group 'gnus-summary-maneuvering
  :type 'boolean)

(defcustom gnus-summary-check-current nil
  "*If non-nil, consider the current article when moving.
The \"unread\" movement commands will stay on the same line if the
current article is unread."
  :group 'gnus-summary-maneuvering
  :type 'boolean)

(defcustom gnus-auto-center-summary t
  "*If non-nil, always center the current summary buffer.
In particular, if `vertical' do only vertical recentering.  If non-nil
and non-`vertical', do both horizontal and vertical recentering."
  :group 'gnus-summary-maneuvering
  :type '(choice (const :tag "none" nil)
		 (const vertical)
		 (integer :tag "height")
		 (sexp :menu-tag "both" t)))

(defcustom gnus-show-all-headers nil
  "*If non-nil, don't hide any headers."
  :group 'gnus-article-hiding
  :group 'gnus-article-headers
  :type 'boolean)

(defcustom gnus-summary-ignore-duplicates nil
  "*If non-nil, ignore articles with identical Message-ID headers."
  :group 'gnus-summary
  :type 'boolean)

(defcustom gnus-single-article-buffer t
  "*If non-nil, display all articles in the same buffer.
If nil, each group will get its own article buffer."
  :group 'gnus-article-various
  :type 'boolean)

(defcustom gnus-break-pages t
  "*If non-nil, do page breaking on articles.
The page delimiter is specified by the `gnus-page-delimiter'
variable."
  :group 'gnus-article-various
  :type 'boolean)

(defcustom gnus-move-split-methods nil
  "*Variable used to suggest where articles are to be moved to.
It uses the same syntax as the `gnus-split-methods' variable."
  :group 'gnus-summary-mail
  :type '(repeat (choice (list :value (fun) function)
			 (cons :value ("" "") regexp (repeat string))
			 (sexp :value nil))))

(defcustom gnus-unread-mark ?  ;Whitespace
  "*Mark used for unread articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-ticked-mark ?!
  "*Mark used for ticked articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-dormant-mark ??
  "*Mark used for dormant articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-del-mark ?r
  "*Mark used for del'd articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-read-mark ?R
  "*Mark used for read articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-expirable-mark ?E
  "*Mark used for expirable articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-killed-mark ?K
  "*Mark used for killed articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-souped-mark ?F
  "*Mark used for killed articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-kill-file-mark ?X
  "*Mark used for articles killed by kill files."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-low-score-mark ?Y
  "*Mark used for articles with a low score."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-catchup-mark ?C
  "*Mark used for articles that are caught up."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-replied-mark ?A
  "*Mark used for articles that have been replied to."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-cached-mark ?*
  "*Mark used for articles that are in the cache."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-saved-mark ?S
  "*Mark used for articles that have been saved to."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-ancient-mark ?O
  "*Mark used for ancient articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-sparse-mark ?Q
  "*Mark used for sparsely reffed articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-canceled-mark ?G
  "*Mark used for canceled articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-duplicate-mark ?M
  "*Mark used for duplicate articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-undownloaded-mark ?@
  "*Mark used for articles that weren't downloaded."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-downloadable-mark ?%
  "*Mark used for articles that are to be downloaded."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-unsendable-mark ?=
  "*Mark used for articles that won't be sent."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-score-over-mark ?+
  "*Score mark used for articles with high scores."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-score-below-mark ?-
  "*Score mark used for articles with low scores."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-empty-thread-mark ?  ;Whitespace
  "*There is no thread under the article."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-not-empty-thread-mark ?=
  "*There is a thread under the article."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-view-pseudo-asynchronously nil
  "*If non-nil, Gnus will view pseudo-articles asynchronously."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-auto-expirable-marks
  (list gnus-killed-mark gnus-del-mark gnus-catchup-mark
	gnus-low-score-mark gnus-ancient-mark gnus-read-mark
	gnus-souped-mark gnus-duplicate-mark)
  "*The list of marks converted into expiration if a group is auto-expirable."
  :version "21.1"
  :group 'gnus-summary
  :type '(repeat character))

(defcustom gnus-inhibit-user-auto-expire t
  "*If non-nil, user marking commands will not mark an article as expirable, even if the group has auto-expire turned on."
  :version "21.1"
  :group 'gnus-summary
  :type 'boolean)

(defcustom gnus-view-pseudos nil
  "*If `automatic', pseudo-articles will be viewed automatically.
If `not-confirm', pseudos will be viewed automatically, and the user
will not be asked to confirm the command."
  :group 'gnus-extract-view
  :type '(choice (const :tag "off" nil)
		 (const automatic)
		 (const not-confirm)))

(defcustom gnus-view-pseudos-separately t
  "*If non-nil, one pseudo-article will be created for each file to be viewed.
If nil, all files that use the same viewing command will be given as a
list of parameters to that command."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-insert-pseudo-articles t
  "*If non-nil, insert pseudo-articles when decoding articles."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-summary-dummy-line-format
  "  %(:                          :%) %S\n"
  "*The format specification for the dummy roots in the summary buffer.
It works along the same lines as a normal formatting string,
with some simple extensions.

%S  The subject"
  :group 'gnus-threading
  :type 'string)

(defcustom gnus-summary-mode-line-format "Gnus: %g [%A] %Z"
  "*The format specification for the summary mode line.
It works along the same lines as a normal formatting string,
with some simple extensions:

%G  Group name
%p  Unprefixed group name
%A  Current article number
%z  Current article score
%V  Gnus version
%U  Number of unread articles in the group
%e  Number of unselected articles in the group
%Z  A string with unread/unselected article counts
%g  Shortish group name
%S  Subject of the current article
%u  User-defined spec
%s  Current score file name
%d  Number of dormant articles
%r  Number of articles that have been marked as read in this session
%E  Number of articles expunged by the score files"
  :group 'gnus-summary-format
  :type 'string)

(defcustom gnus-list-identifiers nil
  "Regexp that matches list identifiers to be removed from subject.
This can also be a list of regexps."
  :version "21.1"
  :group 'gnus-summary-format
  :group 'gnus-article-hiding
  :type '(choice (const :tag "none" nil)
		 (regexp :value ".*")
		 (repeat :value (".*") regexp)))

(defcustom gnus-summary-mark-below 0
  "*Mark all articles with a score below this variable as read.
This variable is local to each summary buffer and usually set by the
score file."
  :group 'gnus-score-default
  :type 'integer)

(defcustom gnus-article-sort-functions '(gnus-article-sort-by-number)
  "*List of functions used for sorting articles in the summary buffer.
This variable is only used when not using a threaded display."
  :group 'gnus-summary-sort
  :type '(repeat (choice (function-item gnus-article-sort-by-number)
			 (function-item gnus-article-sort-by-author)
			 (function-item gnus-article-sort-by-subject)
			 (function-item gnus-article-sort-by-date)
			 (function-item gnus-article-sort-by-score)
			 (function :tag "other"))))

(defcustom gnus-thread-sort-functions '(gnus-thread-sort-by-number)
  "*List of functions used for sorting threads in the summary buffer.
By default, threads are sorted by article number.

Each function takes two threads and return non-nil if the first thread
should be sorted before the other.  If you use more than one function,
the primary sort function should be the last.  You should probably
always include `gnus-thread-sort-by-number' in the list of sorting
functions -- preferably first.

Ready-made functions include `gnus-thread-sort-by-number',
`gnus-thread-sort-by-author', `gnus-thread-sort-by-subject',
`gnus-thread-sort-by-date', `gnus-thread-sort-by-score' and
`gnus-thread-sort-by-total-score' (see `gnus-thread-score-function')."
  :group 'gnus-summary-sort
  :type '(repeat (choice (function-item gnus-thread-sort-by-number)
			 (function-item gnus-thread-sort-by-author)
			 (function-item gnus-thread-sort-by-subject)
			 (function-item gnus-thread-sort-by-date)
			 (function-item gnus-thread-sort-by-score)
			 (function-item gnus-thread-sort-by-total-score)
			 (function :tag "other"))))

(defcustom gnus-thread-score-function '+
  "*Function used for calculating the total score of a thread.

The function is called with the scores of the article and each
subthread and should then return the score of the thread.

Some functions you can use are `+', `max', or `min'."
  :group 'gnus-summary-sort
  :type 'function)

(defcustom gnus-summary-expunge-below nil
  "All articles that have a score less than this variable will be expunged.
This variable is local to the summary buffers."
  :group 'gnus-score-default
  :type '(choice (const :tag "off" nil)
		 integer))

(defcustom gnus-thread-expunge-below nil
  "All threads that have a total score less than this variable will be expunged.
See `gnus-thread-score-function' for en explanation of what a
\"thread score\" is.

This variable is local to the summary buffers."
  :group 'gnus-threading
  :group 'gnus-score-default
  :type '(choice (const :tag "off" nil)
		 integer))

(defcustom gnus-summary-mode-hook nil
  "*A hook for Gnus summary mode.
This hook is run before any variables are set in the summary buffer."
  :options '(turn-on-gnus-mailing-list-mode)
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-summary-menu-hook nil
  "*Hook run after the creation of the summary mode menu."
  :group 'gnus-summary-visual
  :type 'hook)

(defcustom gnus-summary-exit-hook nil
  "*A hook called on exit from the summary buffer.
It will be called with point in the group buffer."
  :group 'gnus-summary-exit
  :type 'hook)

(defcustom gnus-summary-prepare-hook nil
  "*A hook called after the summary buffer has been generated.
If you want to modify the summary buffer, you can use this hook."
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-summary-prepared-hook nil
  "*A hook called as the last thing after the summary buffer has been generated."
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-summary-generate-hook nil
  "*A hook run just before generating the summary buffer.
This hook is commonly used to customize threading variables and the
like."
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-select-group-hook nil
  "*A hook called when a newsgroup is selected.

If you'd like to simplify subjects like the
`gnus-summary-next-same-subject' command does, you can use the
following hook:

 (setq gnus-select-group-hook
      (list
	(lambda ()
	  (mapcar (lambda (header)
		     (mail-header-set-subject
		      header
		      (gnus-simplify-subject
		       (mail-header-subject header) 're-only)))
		  gnus-newsgroup-headers))))"
  :group 'gnus-group-select
  :type 'hook)

(defcustom gnus-select-article-hook nil
  "*A hook called when an article is selected."
  :group 'gnus-summary-choose
  :type 'hook)

(defcustom gnus-visual-mark-article-hook
  (list 'gnus-highlight-selected-summary)
  "*Hook run after selecting an article in the summary buffer.
It is meant to be used for highlighting the article in some way.  It
is not run if `gnus-visual' is nil."
  :group 'gnus-summary-visual
  :type 'hook)

(defcustom gnus-parse-headers-hook nil
  "*A hook called before parsing the headers."
  :group 'gnus-various
  :type 'hook)

(defcustom gnus-exit-group-hook nil
  "*A hook called when exiting summary mode.
This hook is not called from the non-updating exit commands like `Q'."
  :group 'gnus-various
  :type 'hook)

(defcustom gnus-summary-update-hook
  (list 'gnus-summary-highlight-line)
  "*A hook called when a summary line is changed.
The hook will not be called if `gnus-visual' is nil.

The default function `gnus-summary-highlight-line' will
highlight the line according to the `gnus-summary-highlight'
variable."
  :group 'gnus-summary-visual
  :type 'hook)

(defcustom gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read)
  "*A hook called when an article is selected for the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected."
  :group 'gnus-summary-choose
  :type 'hook)

(defcustom gnus-group-no-more-groups-hook nil
  "*A hook run when returning to group mode having no more (unread) groups."
  :group 'gnus-group-select
  :type 'hook)

(defcustom gnus-ps-print-hook nil
  "*A hook run before ps-printing something from Gnus."
  :group 'gnus-summary
  :type 'hook)

(defcustom gnus-summary-selected-face 'gnus-summary-selected-face
  "Face used for highlighting the current article in the summary buffer."
  :group 'gnus-summary-visual
  :type 'face)

(defcustom gnus-summary-highlight
  '(((= mark gnus-canceled-mark)
     . gnus-summary-cancelled-face)
    ((and (> score default)
	  (or (= mark gnus-dormant-mark)
	      (= mark gnus-ticked-mark)))
     . gnus-summary-high-ticked-face)
    ((and (< score default)
	  (or (= mark gnus-dormant-mark)
	      (= mark gnus-ticked-mark)))
     . gnus-summary-low-ticked-face)
    ((or (= mark gnus-dormant-mark)
	 (= mark gnus-ticked-mark))
     . gnus-summary-normal-ticked-face)
    ((and (> score default) (= mark gnus-ancient-mark))
     . gnus-summary-high-ancient-face)
    ((and (< score default) (= mark gnus-ancient-mark))
     . gnus-summary-low-ancient-face)
    ((= mark gnus-ancient-mark)
     . gnus-summary-normal-ancient-face)
    ((and (> score default) (= mark gnus-unread-mark))
     . gnus-summary-high-unread-face)
    ((and (< score default) (= mark gnus-unread-mark))
     . gnus-summary-low-unread-face)
    ((= mark gnus-unread-mark)
     . gnus-summary-normal-unread-face)
    ((and (> score default) (memq mark (list gnus-downloadable-mark
					     gnus-undownloaded-mark)))
     . gnus-summary-high-unread-face)
    ((and (< score default) (memq mark (list gnus-downloadable-mark
					     gnus-undownloaded-mark)))
     . gnus-summary-low-unread-face)
    ((memq mark (list gnus-downloadable-mark gnus-undownloaded-mark))
     . gnus-summary-normal-unread-face)
    ((> score default)
     . gnus-summary-high-read-face)
    ((< score default)
     . gnus-summary-low-read-face)
    (t
     . gnus-summary-normal-read-face))
  "*Controls the highlighting of summary buffer lines.

A list of (FORM . FACE) pairs.  When deciding how a a particular
summary line should be displayed, each form is evaluated.  The content
of the face field after the first true form is used.  You can change
how those summary lines are displayed, by editing the face field.

You can use the following variables in the FORM field.

score:   The articles score
default: The default article score.
below:   The score below which articles are automatically marked as read.
mark:    The articles mark."
  :group 'gnus-summary-visual
  :type '(repeat (cons (sexp :tag "Form" nil)
		       face)))

(defcustom gnus-alter-header-function nil
  "Function called to allow alteration of article header structures.
The function is called with one parameter, the article header vector,
which it may alter in any way.")

(defvar gnus-decode-encoded-word-function 'mail-decode-encoded-word-string
  "Variable that says which function should be used to decode a string with encoded words.")

(defcustom gnus-extra-headers nil
  "*Extra headers to parse."
  :version "21.1"
  :group 'gnus-summary
  :type '(repeat symbol))

(defcustom gnus-ignored-from-addresses
  (and user-mail-address (regexp-quote user-mail-address))
  "*Regexp of From headers that may be suppressed in favor of To headers."
  :version "21.1"
  :group 'gnus-summary
  :type 'regexp)

(defcustom gnus-group-charset-alist
  '(("^hk\\>\\|^tw\\>\\|\\<big5\\>" cn-big5)
    ("^cn\\>\\|\\<chinese\\>" cn-gb-2312)
    ("^fj\\>\\|^japan\\>" iso-2022-jp-2)
    ("^tnn\\>\\|^pin\\>\\|^sci.lang.japan" iso-2022-7bit)
    ("^relcom\\>" koi8-r)
    ("^fido7\\>" koi8-r)
    ("^\\(cz\\|hun\\|pl\\|sk\\|hr\\)\\>" iso-8859-2)
    ("^israel\\>" iso-8859-1)
    ("^han\\>" euc-kr)
    ("^alt.chinese.text.big5\\>" chinese-big5)
    ("^soc.culture.vietnamese\\>" vietnamese-viqr)
    ("^\\(comp\\|rec\\|alt\\|sci\\|soc\\|news\\|gnu\\|bofh\\)\\>" iso-8859-1)
    (".*" iso-8859-1))
  "Alist of regexps (to match group names) and default charsets to be used when reading."
  :type '(repeat (list (regexp :tag "Group")
		       (symbol :tag "Charset")))
  :group 'gnus-charset)

(defcustom gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown)
  "List of charsets that should be ignored.
When these charsets are used in the \"charset\" parameter, the
default charset will be used instead."
  :version "21.1"
  :type '(repeat symbol)
  :group 'gnus-charset)

(defcustom gnus-group-ignored-charsets-alist
  '(("alt\\.chinese\\.text" iso-8859-1))
  "Alist of regexps (to match group names) and charsets that should be ignored.
When these charsets are used in the \"charset\" parameter, the
default charset will be used instead."
  :type '(repeat (cons (regexp :tag "Group")
		       (repeat symbol)))
  :group 'gnus-charset)

(defcustom gnus-group-highlight-words-alist nil
  "Alist of group regexps and highlight regexps.
This variable uses the same syntax as `gnus-emphasis-alist'."
  :version "21.1"
  :type '(repeat (cons (regexp :tag "Group")
		       (repeat (list (regexp :tag "Highlight regexp")
				     (number :tag "Group for entire word" 0)
				     (number :tag "Group for displayed part" 0)
				     (symbol :tag "Face"
					     gnus-emphasis-highlight-words)))))
  :group 'gnus-summary-visual)

(defcustom gnus-summary-show-article-charset-alist
  nil
  "Alist of number and charset.
The article will be shown with the charset corresponding to the
numbered argument.
For example: ((1 . cn-gb-2312) (2 . big5))."
  :version "21.1"
  :type '(repeat (cons (number :tag "Argument" 1)
		       (symbol :tag "Charset")))
  :group 'gnus-charset)

(defcustom gnus-preserve-marks t
  "Whether marks are preserved when moving, copying and respooling messages."
  :version "21.1"
  :type 'boolean
  :group 'gnus-summary-marks)

(defcustom gnus-alter-articles-to-read-function nil
  "Function to be called to alter the list of articles to be selected."
  :type '(choice (const nil) function)
  :group 'gnus-summary)

(defcustom gnus-orphan-score nil
  "*All orphans get this score added.  Set in the score file."
  :group 'gnus-score-default
  :type '(choice (const nil)
		 integer))

(defcustom gnus-summary-save-parts-default-mime "image/.*"
  "*A regexp to match MIME parts when saving multiple parts of a message
with gnus-summary-save-parts (X m). This regexp will be used by default
when prompting the user for which type of files to save."
  :group 'gnus-summary
  :type 'regexp)


;;; Internal variables

(defvar gnus-article-mime-handles nil)
(defvar gnus-article-decoded-p nil)
(defvar gnus-scores-exclude-files nil)
(defvar gnus-page-broken nil)
(defvar gnus-inhibit-mime-unbuttonizing nil)

(defvar gnus-original-article nil)
(defvar gnus-article-internal-prepare-hook nil)
(defvar gnus-newsgroup-process-stack nil)

(defvar gnus-thread-indent-array nil)
(defvar gnus-thread-indent-array-level gnus-thread-indent-level)
(defvar gnus-sort-gathered-threads-function 'gnus-thread-sort-by-number
  "Function called to sort the articles within a thread after it has been gathered together.")

(defvar gnus-summary-save-parts-type-history nil)
(defvar gnus-summary-save-parts-last-directory nil)

;; Avoid highlighting in kill files.
(defvar gnus-summary-inhibit-highlight nil)
(defvar gnus-newsgroup-selected-overlay nil)
(defvar gnus-inhibit-limiting nil)
(defvar gnus-newsgroup-adaptive-score-file nil)
(defvar gnus-current-score-file nil)
(defvar gnus-current-move-group nil)
(defvar gnus-current-copy-group nil)
(defvar gnus-current-crosspost-group nil)

(defvar gnus-newsgroup-dependencies nil)
(defvar gnus-newsgroup-adaptive nil)
(defvar gnus-summary-display-article-function nil)
(defvar gnus-summary-highlight-line-function nil
  "Function called after highlighting a summary line.")

(defvar gnus-summary-line-format-alist
  `((?N ,(macroexpand '(mail-header-number gnus-tmp-header)) ?d)
    (?S ,(macroexpand '(mail-header-subject gnus-tmp-header)) ?s)
    (?s gnus-tmp-subject-or-nil ?s)
    (?n gnus-tmp-name ?s)
    (?A (car (cdr (funcall gnus-extract-address-components gnus-tmp-from)))
	?s)
    (?a (or (car (funcall gnus-extract-address-components gnus-tmp-from))
	    gnus-tmp-from) ?s)
    (?F gnus-tmp-from ?s)
    (?x ,(macroexpand '(mail-header-xref gnus-tmp-header)) ?s)
    (?D ,(macroexpand '(mail-header-date gnus-tmp-header)) ?s)
    (?d (gnus-dd-mmm (mail-header-date gnus-tmp-header)) ?s)
    (?o (gnus-date-iso8601 (mail-header-date gnus-tmp-header)) ?s)
    (?M ,(macroexpand '(mail-header-id gnus-tmp-header)) ?s)
    (?r ,(macroexpand '(mail-header-references gnus-tmp-header)) ?s)
    (?c (or (mail-header-chars gnus-tmp-header) 0) ?d)
    (?L gnus-tmp-lines ?d)
    (?I gnus-tmp-indentation ?s)
    (?T (if (= gnus-tmp-level 0) "" (make-string (frame-width) ? )) ?s)
    (?R gnus-tmp-replied ?c)
    (?\[ gnus-tmp-opening-bracket ?c)
    (?\] gnus-tmp-closing-bracket ?c)
    (?\> (make-string gnus-tmp-level ? ) ?s)
    (?\< (make-string (max 0 (- 20 gnus-tmp-level)) ? ) ?s)
    (?i gnus-tmp-score ?d)
    (?z gnus-tmp-score-char ?c)
    (?l (bbb-grouplens-score gnus-tmp-header) ?s)
    (?V (gnus-thread-total-score (and (boundp 'thread) (car thread))) ?d)
    (?U gnus-tmp-unread ?c)
    (?f (gnus-summary-from-or-to-or-newsgroups gnus-tmp-header) ?s)
    (?t (gnus-summary-number-of-articles-in-thread
	 (and (boundp 'thread) (car thread)) gnus-tmp-level)
	?d)
    (?e (gnus-summary-number-of-articles-in-thread
	 (and (boundp 'thread) (car thread)) gnus-tmp-level t)
	?c)
    (?u gnus-tmp-user-defined ?s)
    (?P (gnus-pick-line-number) ?d))
  "An alist of format specifications that can appear in summary lines.
These are paired with what variables they correspond with, along with
the type of the variable (string, integer, character, etc).")

(defvar gnus-summary-dummy-line-format-alist
  `((?S gnus-tmp-subject ?s)
    (?N gnus-tmp-number ?d)
    (?u gnus-tmp-user-defined ?s)))

(defvar gnus-summary-mode-line-format-alist
  `((?G gnus-tmp-group-name ?s)
    (?g (gnus-short-group-name gnus-tmp-group-name) ?s)
    (?p (gnus-group-real-name gnus-tmp-group-name) ?s)
    (?A gnus-tmp-article-number ?d)
    (?Z gnus-tmp-unread-and-unselected ?s)
    (?V gnus-version ?s)
    (?U gnus-tmp-unread-and-unticked ?d)
    (?S gnus-tmp-subject ?s)
    (?e gnus-tmp-unselected ?d)
    (?u gnus-tmp-user-defined ?s)
    (?d (length gnus-newsgroup-dormant) ?d)
    (?t (length gnus-newsgroup-marked) ?d)
    (?r (length gnus-newsgroup-reads) ?d)
    (?z (gnus-summary-article-score gnus-tmp-article-number) ?d)
    (?E gnus-newsgroup-expunged-tally ?d)
    (?s (gnus-current-score-file-nondirectory) ?s)))

(defvar gnus-last-search-regexp nil
  "Default regexp for article search command.")

(defvar gnus-last-shell-command nil
  "Default shell command on article.")

(defvar gnus-newsgroup-begin nil)
(defvar gnus-newsgroup-end nil)
(defvar gnus-newsgroup-last-rmail nil)
(defvar gnus-newsgroup-last-mail nil)
(defvar gnus-newsgroup-last-folder nil)
(defvar gnus-newsgroup-last-file nil)
(defvar gnus-newsgroup-auto-expire nil)
(defvar gnus-newsgroup-active nil)

(defvar gnus-newsgroup-data nil)
(defvar gnus-newsgroup-data-reverse nil)
(defvar gnus-newsgroup-limit nil)
(defvar gnus-newsgroup-limits nil)

(defvar gnus-newsgroup-unreads nil
  "List of unread articles in the current newsgroup.")

(defvar gnus-newsgroup-unselected nil
  "List of unselected unread articles in the current newsgroup.")

(defvar gnus-newsgroup-reads nil
  "Alist of read articles and article marks in the current newsgroup.")

(defvar gnus-newsgroup-expunged-tally nil)

(defvar gnus-newsgroup-marked nil
  "List of ticked articles in the current newsgroup (a subset of unread art).")

(defvar gnus-newsgroup-killed nil
  "List of ranges of articles that have been through the scoring process.")

(defvar gnus-newsgroup-cached nil
  "List of articles that come from the article cache.")

(defvar gnus-newsgroup-saved nil
  "List of articles that have been saved.")

(defvar gnus-newsgroup-kill-headers nil)

(defvar gnus-newsgroup-replied nil
  "List of articles that have been replied to in the current newsgroup.")

(defvar gnus-newsgroup-expirable nil
  "List of articles in the current newsgroup that can be expired.")

(defvar gnus-newsgroup-processable nil
  "List of articles in the current newsgroup that can be processed.")

(defvar gnus-newsgroup-downloadable nil
  "List of articles in the current newsgroup that can be processed.")

(defvar gnus-newsgroup-undownloaded nil
  "List of articles in the current newsgroup that haven't been downloaded..")

(defvar gnus-newsgroup-unsendable nil
  "List of articles in the current newsgroup that won't be sent.")

(defvar gnus-newsgroup-bookmarks nil
  "List of articles in the current newsgroup that have bookmarks.")

(defvar gnus-newsgroup-dormant nil
  "List of dormant articles in the current newsgroup.")

(defvar gnus-newsgroup-scored nil
  "List of scored articles in the current newsgroup.")

(defvar gnus-newsgroup-headers nil
  "List of article headers in the current newsgroup.")

(defvar gnus-newsgroup-threads nil)

(defvar gnus-newsgroup-prepared nil
  "Whether the current group has been prepared properly.")

(defvar gnus-newsgroup-ancient nil
  "List of `gnus-fetch-old-headers' articles in the current newsgroup.")

(defvar gnus-newsgroup-sparse nil)

(defvar gnus-current-article nil)
(defvar gnus-article-current nil)
(defvar gnus-current-headers nil)
(defvar gnus-have-all-headers nil)
(defvar gnus-last-article nil)
(defvar gnus-newsgroup-history nil)
(defvar gnus-newsgroup-charset nil)
(defvar gnus-newsgroup-ephemeral-charset nil)
(defvar gnus-newsgroup-ephemeral-ignored-charsets nil)

(defconst gnus-summary-local-variables
  '(gnus-newsgroup-name
    gnus-newsgroup-begin gnus-newsgroup-end
    gnus-newsgroup-last-rmail gnus-newsgroup-last-mail
    gnus-newsgroup-last-folder gnus-newsgroup-last-file
    gnus-newsgroup-auto-expire gnus-newsgroup-unreads
    gnus-newsgroup-unselected gnus-newsgroup-marked
    gnus-newsgroup-reads gnus-newsgroup-saved
    gnus-newsgroup-replied gnus-newsgroup-expirable
    gnus-newsgroup-processable gnus-newsgroup-killed
    gnus-newsgroup-downloadable gnus-newsgroup-undownloaded
    gnus-newsgroup-unsendable
    gnus-newsgroup-bookmarks gnus-newsgroup-dormant
    gnus-newsgroup-headers gnus-newsgroup-threads
    gnus-newsgroup-prepared gnus-summary-highlight-line-function
    gnus-current-article gnus-current-headers gnus-have-all-headers
    gnus-last-article gnus-article-internal-prepare-hook
    gnus-newsgroup-dependencies gnus-newsgroup-selected-overlay
    gnus-newsgroup-scored gnus-newsgroup-kill-headers
    gnus-thread-expunge-below
    gnus-score-alist gnus-current-score-file
    (gnus-summary-expunge-below . global)
    (gnus-summary-mark-below . global)
    (gnus-orphan-score . global)
    gnus-newsgroup-active gnus-scores-exclude-files
    gnus-newsgroup-history gnus-newsgroup-ancient
    gnus-newsgroup-sparse gnus-newsgroup-process-stack
    (gnus-newsgroup-adaptive . gnus-use-adaptive-scoring)
    gnus-newsgroup-adaptive-score-file (gnus-reffed-article-number . -1)
    (gnus-newsgroup-expunged-tally . 0)
    gnus-cache-removable-articles gnus-newsgroup-cached
    gnus-newsgroup-data gnus-newsgroup-data-reverse
    gnus-newsgroup-limit gnus-newsgroup-limits
    gnus-newsgroup-charset)
  "Variables that are buffer-local to the summary buffers.")

;; Byte-compiler warning.
(eval-when-compile (defvar gnus-article-mode-map))

;; MIME stuff.

(defvar gnus-decode-encoded-word-methods
  '(mail-decode-encoded-word-string)
  "List of methods used to decode encoded words.

This variable is a list of FUNCTION or (REGEXP . FUNCTION).  If item is
FUNCTION, FUNCTION will be apply to all newsgroups.  If item is a
(REGEXP . FUNCTION), FUNCTION will be only apply to thes newsgroups
whose names match REGEXP.

For example:
((\"chinese\" . gnus-decode-encoded-word-string-by-guess)
 mail-decode-encoded-word-string
 (\"chinese\" . rfc1843-decode-string))")

(defvar gnus-decode-encoded-word-methods-cache nil)

(defun gnus-multi-decode-encoded-word-string (string)
  "Apply the functions from `gnus-encoded-word-methods' that match."
  (unless (and gnus-decode-encoded-word-methods-cache
	       (eq gnus-newsgroup-name
		   (car gnus-decode-encoded-word-methods-cache)))
    (setq gnus-decode-encoded-word-methods-cache (list gnus-newsgroup-name))
    (mapcar (lambda (x)
	      (if (symbolp x)
		  (nconc gnus-decode-encoded-word-methods-cache (list x))
		(if (and gnus-newsgroup-name
			 (string-match (car x) gnus-newsgroup-name))
		    (nconc gnus-decode-encoded-word-methods-cache
			   (list (cdr x))))))
	  gnus-decode-encoded-word-methods))
  (let ((xlist gnus-decode-encoded-word-methods-cache))
    (pop xlist)
    (while xlist
      (setq string (funcall (pop xlist) string))))
  string)

;; Subject simplification.

(defun gnus-simplify-whitespace (str)
  "Remove excessive whitespace from STR."
  (let ((mystr str))
    ;; Multiple spaces.
    (while (string-match "[ \t][ \t]+" mystr)
      (setq mystr (concat (substring mystr 0 (match-beginning 0))
                          " "
                          (substring mystr (match-end 0)))))
    ;; Leading spaces.
    (when (string-match "^[ \t]+" mystr)
      (setq mystr (substring mystr (match-end 0))))
    ;; Trailing spaces.
    (when (string-match "[ \t]+$" mystr)
      (setq mystr (substring mystr 0 (match-beginning 0))))
    mystr))

(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines."
  (if (string-match "^[Rr][Ee]: *" subject)
      (substring subject (match-end 0))
    subject))

(defun gnus-simplify-subject (subject &optional re-only)
  "Remove `Re:' and words in parentheses.
If RE-ONLY is non-nil, strip leading `Re:'s only."
  (let ((case-fold-search t))		;Ignore case.
    ;; Remove `Re:', `Re^N:', `Re(n)', and `Re[n]:'.
    (when (string-match "\\`\\(re\\([[(^][0-9]+[])]?\\)?:[ \t]*\\)+" subject)
      (setq subject (substring subject (match-end 0))))
    ;; Remove uninteresting prefixes.
    (when (and (not re-only)
	       gnus-simplify-ignored-prefixes
	       (string-match gnus-simplify-ignored-prefixes subject))
      (setq subject (substring subject (match-end 0))))
    ;; Remove words in parentheses from end.
    (unless re-only
      (while (string-match "[ \t\n]*([^()]*)[ \t\n]*\\'" subject)
	(setq subject (substring subject 0 (match-beginning 0)))))
    ;; Return subject string.
    subject))

;; Remove any leading "re:"s, any trailing paren phrases, and simplify
;; all whitespace.
(defsubst gnus-simplify-buffer-fuzzy-step (regexp &optional newtext)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match (or newtext ""))))

(defun gnus-simplify-buffer-fuzzy ()
  "Simplify string in the buffer fuzzily.
The string in the accessible portion of the current buffer is simplified.
It is assumed to be a single-line subject.
Whitespace is generally cleaned up, and miscellaneous leading/trailing
matter is removed.  Additional things can be deleted by setting
`gnus-simplify-subject-fuzzy-regexp'."
  (let ((case-fold-search t)
	(modified-tick))
    (gnus-simplify-buffer-fuzzy-step "\t" " ")

    (while (not (eq modified-tick (buffer-modified-tick)))
      (setq modified-tick (buffer-modified-tick))
      (cond
       ((listp gnus-simplify-subject-fuzzy-regexp)
	(mapcar 'gnus-simplify-buffer-fuzzy-step
		gnus-simplify-subject-fuzzy-regexp))
       (gnus-simplify-subject-fuzzy-regexp
	(gnus-simplify-buffer-fuzzy-step gnus-simplify-subject-fuzzy-regexp)))
      (gnus-simplify-buffer-fuzzy-step "^ *\\[[-+?*!][-+?*!]\\] *")
      (gnus-simplify-buffer-fuzzy-step
       "^ *\\(re\\|fw\\|fwd\\)[[{(^0-9]*[])}]?[:;] *")
      (gnus-simplify-buffer-fuzzy-step "^[[].*:\\( .*\\)[]]$" "\\1"))

    (gnus-simplify-buffer-fuzzy-step " *[[{(][^()\n]*[]})] *$")
    (gnus-simplify-buffer-fuzzy-step "  +" " ")
    (gnus-simplify-buffer-fuzzy-step " $")
    (gnus-simplify-buffer-fuzzy-step "^ +")))

(defun gnus-simplify-subject-fuzzy (subject)
  "Simplify a subject string fuzzily.
See `gnus-simplify-buffer-fuzzy' for details."
  (save-excursion
    (gnus-set-work-buffer)
    (let ((case-fold-search t))
      ;; Remove uninteresting prefixes.
      (when (and gnus-simplify-ignored-prefixes
		 (string-match gnus-simplify-ignored-prefixes subject))
	(setq subject (substring subject (match-end 0))))
      (insert subject)
      (inline (gnus-simplify-buffer-fuzzy))
      (buffer-string))))

(defsubst gnus-simplify-subject-fully (subject)
  "Simplify a subject string according to gnus-summary-gather-subject-limit."
  (cond
   (gnus-simplify-subject-functions
    (gnus-map-function gnus-simplify-subject-functions subject))
   ((null gnus-summary-gather-subject-limit)
    (gnus-simplify-subject-re subject))
   ((eq gnus-summary-gather-subject-limit 'fuzzy)
    (gnus-simplify-subject-fuzzy subject))
   ((numberp gnus-summary-gather-subject-limit)
    (gnus-limit-string (gnus-simplify-subject-re subject)
		       gnus-summary-gather-subject-limit))
   (t
    subject)))

(defsubst gnus-subject-equal (s1 s2 &optional simple-first)
  "Check whether two subjects are equal.
If optional argument simple-first is t, first argument is already
simplified."
  (cond
   ((null simple-first)
    (equal (gnus-simplify-subject-fully s1)
	   (gnus-simplify-subject-fully s2)))
   (t
    (equal s1
	   (gnus-simplify-subject-fully s2)))))

(defun gnus-summary-bubble-group ()
  "Increase the score of the current group.
This is a handy function to add to `gnus-summary-exit-hook' to
increase the score of each group you read."
  (gnus-group-add-score gnus-newsgroup-name))


;;;
;;; Gnus summary mode
;;;

(put 'gnus-summary-mode 'mode-class 'special)

(defvar gnus-article-commands-menu)

(when t
  ;; Non-orthogonal keys

  (gnus-define-keys gnus-summary-mode-map
    " " gnus-summary-next-page
    "\177" gnus-summary-prev-page
    [delete] gnus-summary-prev-page
    [backspace] gnus-summary-prev-page
    "\r" gnus-summary-scroll-up
    "\M-\r" gnus-summary-scroll-down
    "n" gnus-summary-next-unread-article
    "p" gnus-summary-prev-unread-article
    "N" gnus-summary-next-article
    "P" gnus-summary-prev-article
    "\M-\C-n" gnus-summary-next-same-subject
    "\M-\C-p" gnus-summary-prev-same-subject
    "\M-n" gnus-summary-next-unread-subject
    "\M-p" gnus-summary-prev-unread-subject
    "." gnus-summary-first-unread-article
    "," gnus-summary-best-unread-article
    "\M-s" gnus-summary-search-article-forward
    "\M-r" gnus-summary-search-article-backward
    "<" gnus-summary-beginning-of-article
    ">" gnus-summary-end-of-article
    "j" gnus-summary-goto-article
    "^" gnus-summary-refer-parent-article
    "\M-^" gnus-summary-refer-article
    "u" gnus-summary-tick-article-forward
    "!" gnus-summary-tick-article-forward
    "U" gnus-summary-tick-article-backward
    "d" gnus-summary-mark-as-read-forward
    "D" gnus-summary-mark-as-read-backward
    "E" gnus-summary-mark-as-expirable
    "\M-u" gnus-summary-clear-mark-forward
    "\M-U" gnus-summary-clear-mark-backward
    "k" gnus-summary-kill-same-subject-and-select
    "\C-k" gnus-summary-kill-same-subject
    "\M-\C-k" gnus-summary-kill-thread
    "\M-\C-l" gnus-summary-lower-thread
    "e" gnus-summary-edit-article
    "#" gnus-summary-mark-as-processable
    "\M-#" gnus-summary-unmark-as-processable
    "\M-\C-t" gnus-summary-toggle-threads
    "\M-\C-s" gnus-summary-show-thread
    "\M-\C-h" gnus-summary-hide-thread
    "\M-\C-f" gnus-summary-next-thread
    "\M-\C-b" gnus-summary-prev-thread
    [(meta down)] gnus-summary-next-thread
    [(meta up)] gnus-summary-prev-thread
    "\M-\C-u" gnus-summary-up-thread
    "\M-\C-d" gnus-summary-down-thread
    "&" gnus-summary-execute-command
    "c" gnus-summary-catchup-and-exit
    "\C-w" gnus-summary-mark-region-as-read
    "\C-t" gnus-summary-toggle-truncation
    "?" gnus-summary-mark-as-dormant
    "\C-c\M-\C-s" gnus-summary-limit-include-expunged
    "\C-c\C-s\C-n" gnus-summary-sort-by-number
    "\C-c\C-s\C-l" gnus-summary-sort-by-lines
    "\C-c\C-s\C-c" gnus-summary-sort-by-chars
    "\C-c\C-s\C-a" gnus-summary-sort-by-author
    "\C-c\C-s\C-s" gnus-summary-sort-by-subject
    "\C-c\C-s\C-d" gnus-summary-sort-by-date
    "\C-c\C-s\C-i" gnus-summary-sort-by-score
    "=" gnus-summary-expand-window
    "\C-x\C-s" gnus-summary-reselect-current-group
    "\M-g" gnus-summary-rescan-group
    "w" gnus-summary-stop-page-breaking
    "\C-c\C-r" gnus-summary-caesar-message
    "f" gnus-summary-followup
    "F" gnus-summary-followup-with-original
    "C" gnus-summary-cancel-article
    "r" gnus-summary-reply
    "R" gnus-summary-reply-with-original
    "\C-c\C-f" gnus-summary-mail-forward
    "o" gnus-summary-save-article
    "\C-o" gnus-summary-save-article-mail
    "|" gnus-summary-pipe-output
    "\M-k" gnus-summary-edit-local-kill
    "\M-K" gnus-summary-edit-global-kill
    ;; "V" gnus-version
    "\C-c\C-d" gnus-summary-describe-group
    "q" gnus-summary-exit
    "Q" gnus-summary-exit-no-update
    "\C-c\C-i" gnus-info-find-node
    gnus-mouse-2 gnus-mouse-pick-article
    "m" gnus-summary-mail-other-window
    "a" gnus-summary-post-news
    "x" gnus-summary-limit-to-unread
    "s" gnus-summary-isearch-article
    "t" gnus-summary-toggle-header
    "g" gnus-summary-show-article
    "l" gnus-summary-goto-last-article
    "\C-c\C-v\C-v" gnus-uu-decode-uu-view
    "\C-d" gnus-summary-enter-digest-group
    "\M-\C-d" gnus-summary-read-document
    "\M-\C-e" gnus-summary-edit-parameters
    "\M-\C-a" gnus-summary-customize-parameters
    "\C-c\C-b" gnus-bug
    "*" gnus-cache-enter-article
    "\M-*" gnus-cache-remove-article
    "\M-&" gnus-summary-universal-argument
    "\C-l" gnus-recenter
    "I" gnus-summary-increase-score
    "L" gnus-summary-lower-score
    "\M-i" gnus-symbolic-argument
    "h" gnus-summary-select-article-buffer

    "b" gnus-article-view-part
    "\M-t" gnus-summary-toggle-display-buttonized

    "V" gnus-summary-score-map
    "X" gnus-uu-extract-map
    "S" gnus-summary-send-map)

  ;; Sort of orthogonal keymap
  (gnus-define-keys (gnus-summary-mark-map "M" gnus-summary-mode-map)
    "t" gnus-summary-tick-article-forward
    "!" gnus-summary-tick-article-forward
    "d" gnus-summary-mark-as-read-forward
    "r" gnus-summary-mark-as-read-forward
    "c" gnus-summary-clear-mark-forward
    " " gnus-summary-clear-mark-forward
    "e" gnus-summary-mark-as-expirable
    "x" gnus-summary-mark-as-expirable
    "?" gnus-summary-mark-as-dormant
    "b" gnus-summary-set-bookmark
    "B" gnus-summary-remove-bookmark
    "#" gnus-summary-mark-as-processable
    "\M-#" gnus-summary-unmark-as-processable
    "S" gnus-summary-limit-include-expunged
    "C" gnus-summary-catchup
    "H" gnus-summary-catchup-to-here
    "\C-c" gnus-summary-catchup-all
    "k" gnus-summary-kill-same-subject-and-select
    "K" gnus-summary-kill-same-subject
    "P" gnus-uu-mark-map)

  (gnus-define-keys (gnus-summary-mscore-map "V" gnus-summary-mark-map)
    "c" gnus-summary-clear-above
    "u" gnus-summary-tick-above
    "m" gnus-summary-mark-above
    "k" gnus-summary-kill-below)

  (gnus-define-keys (gnus-summary-limit-map "/" gnus-summary-mode-map)
    "/" gnus-summary-limit-to-subject
    "n" gnus-summary-limit-to-articles
    "w" gnus-summary-pop-limit
    "s" gnus-summary-limit-to-subject
    "a" gnus-summary-limit-to-author
    "u" gnus-summary-limit-to-unread
    "m" gnus-summary-limit-to-marks
    "M" gnus-summary-limit-exclude-marks
    "v" gnus-summary-limit-to-score
    "*" gnus-summary-limit-include-cached
    "D" gnus-summary-limit-include-dormant
    "T" gnus-summary-limit-include-thread
    "d" gnus-summary-limit-exclude-dormant
    "t" gnus-summary-limit-to-age
    "x" gnus-summary-limit-to-extra
    "E" gnus-summary-limit-include-expunged
    "c" gnus-summary-limit-exclude-childless-dormant
    "C" gnus-summary-limit-mark-excluded-as-read)

  (gnus-define-keys (gnus-summary-goto-map "G" gnus-summary-mode-map)
    "n" gnus-summary-next-unread-article
    "p" gnus-summary-prev-unread-article
    "N" gnus-summary-next-article
    "P" gnus-summary-prev-article
    "\C-n" gnus-summary-next-same-subject
    "\C-p" gnus-summary-prev-same-subject
    "\M-n" gnus-summary-next-unread-subject
    "\M-p" gnus-summary-prev-unread-subject
    "f" gnus-summary-first-unread-article
    "b" gnus-summary-best-unread-article
    "j" gnus-summary-goto-article
    "g" gnus-summary-goto-subject
    "l" gnus-summary-goto-last-article
    "o" gnus-summary-pop-article)

  (gnus-define-keys (gnus-summary-thread-map "T" gnus-summary-mode-map)
    "k" gnus-summary-kill-thread
    "l" gnus-summary-lower-thread
    "i" gnus-summary-raise-thread
    "T" gnus-summary-toggle-threads
    "t" gnus-summary-rethread-current
    "^" gnus-summary-reparent-thread
    "s" gnus-summary-show-thread
    "S" gnus-summary-show-all-threads
    "h" gnus-summary-hide-thread
    "H" gnus-summary-hide-all-threads
    "n" gnus-summary-next-thread
    "p" gnus-summary-prev-thread
    "u" gnus-summary-up-thread
    "o" gnus-summary-top-thread
    "d" gnus-summary-down-thread
    "#" gnus-uu-mark-thread
    "\M-#" gnus-uu-unmark-thread)

  (gnus-define-keys (gnus-summary-buffer-map "Y" gnus-summary-mode-map)
    "g" gnus-summary-prepare
    "c" gnus-summary-insert-cached-articles)

  (gnus-define-keys (gnus-summary-exit-map "Z" gnus-summary-mode-map)
    "c" gnus-summary-catchup-and-exit
    "C" gnus-summary-catchup-all-and-exit
    "E" gnus-summary-exit-no-update
    "Q" gnus-summary-exit
    "Z" gnus-summary-exit
    "n" gnus-summary-catchup-and-goto-next-group
    "R" gnus-summary-reselect-current-group
    "G" gnus-summary-rescan-group
    "N" gnus-summary-next-group
    "s" gnus-summary-save-newsrc
    "P" gnus-summary-prev-group)

  (gnus-define-keys (gnus-summary-article-map "A" gnus-summary-mode-map)
    " " gnus-summary-next-page
    "n" gnus-summary-next-page
    "\177" gnus-summary-prev-page
    [delete] gnus-summary-prev-page
    "p" gnus-summary-prev-page
    "\r" gnus-summary-scroll-up
    "\M-\r" gnus-summary-scroll-down
    "<" gnus-summary-beginning-of-article
    ">" gnus-summary-end-of-article
    "b" gnus-summary-beginning-of-article
    "e" gnus-summary-end-of-article
    "^" gnus-summary-refer-parent-article
    "r" gnus-summary-refer-parent-article
    "D" gnus-summary-enter-digest-group
    "R" gnus-summary-refer-references
    "T" gnus-summary-refer-thread
    "g" gnus-summary-show-article
    "s" gnus-summary-isearch-article
    "P" gnus-summary-print-article
    "t" gnus-article-babel)

  (gnus-define-keys (gnus-summary-wash-map "W" gnus-summary-mode-map)
    "b" gnus-article-add-buttons
    "B" gnus-article-add-buttons-to-head
    "o" gnus-article-treat-overstrike
    "e" gnus-article-emphasize
    "w" gnus-article-fill-cited-article
    "Q" gnus-article-fill-long-lines
    "C" gnus-article-capitalize-sentences
    "c" gnus-article-remove-cr
    "q" gnus-article-de-quoted-unreadable
    "6" gnus-article-de-base64-unreadable
    "Z" gnus-article-decode-HZ
    "h" gnus-article-wash-html
    "f" gnus-article-display-x-face
    "l" gnus-summary-stop-page-breaking
    "r" gnus-summary-caesar-message
    "t" gnus-summary-toggle-header
    "v" gnus-summary-verbose-headers
    "H" gnus-article-strip-headers-in-body
    "d" gnus-article-treat-dumbquotes)

  (gnus-define-keys (gnus-summary-wash-hide-map "W" gnus-summary-wash-map)
    "a" gnus-article-hide
    "h" gnus-article-hide-headers
    "b" gnus-article-hide-boring-headers
    "s" gnus-article-hide-signature
    "c" gnus-article-hide-citation
    "C" gnus-article-hide-citation-in-followups
    "l" gnus-article-hide-list-identifiers
    "p" gnus-article-hide-pgp
    "B" gnus-article-strip-banner
    "P" gnus-article-hide-pem
    "\C-c" gnus-article-hide-citation-maybe)

  (gnus-define-keys (gnus-summary-wash-highlight-map "H" gnus-summary-wash-map)
    "a" gnus-article-highlight
    "h" gnus-article-highlight-headers
    "c" gnus-article-highlight-citation
    "s" gnus-article-highlight-signature)

  (gnus-define-keys (gnus-summary-wash-mime-map "M" gnus-summary-wash-map)
    "w" gnus-article-decode-mime-words
    "c" gnus-article-decode-charset
    "v" gnus-mime-view-all-parts
    "b" gnus-article-view-part)

  (gnus-define-keys (gnus-summary-wash-time-map "T" gnus-summary-wash-map)
    "z" gnus-article-date-ut
    "u" gnus-article-date-ut
    "l" gnus-article-date-local
    "e" gnus-article-date-lapsed
    "o" gnus-article-date-original
    "i" gnus-article-date-iso8601
    "s" gnus-article-date-user)

  (gnus-define-keys (gnus-summary-wash-empty-map "E" gnus-summary-wash-map)
    "t" gnus-article-remove-trailing-blank-lines
    "l" gnus-article-strip-leading-blank-lines
    "m" gnus-article-strip-multiple-blank-lines
    "a" gnus-article-strip-blank-lines
    "A" gnus-article-strip-all-blank-lines
    "s" gnus-article-strip-leading-space
    "e" gnus-article-strip-trailing-space)

  (gnus-define-keys (gnus-summary-help-map "H" gnus-summary-mode-map)
    "v" gnus-version
    "f" gnus-summary-fetch-faq
    "d" gnus-summary-describe-group
    "h" gnus-summary-describe-briefly
    "i" gnus-info-find-node)

  (gnus-define-keys (gnus-summary-backend-map "B" gnus-summary-mode-map)
    "e" gnus-summary-expire-articles
    "\M-\C-e" gnus-summary-expire-articles-now
    "\177" gnus-summary-delete-article
    [delete] gnus-summary-delete-article
    [backspace] gnus-summary-delete-article
    "m" gnus-summary-move-article
    "r" gnus-summary-respool-article
    "w" gnus-summary-edit-article
    "c" gnus-summary-copy-article
    "B" gnus-summary-crosspost-article
    "q" gnus-summary-respool-query
    "t" gnus-summary-respool-trace
    "i" gnus-summary-import-article
    "p" gnus-summary-article-posted-p)

  (gnus-define-keys (gnus-summary-save-map "O" gnus-summary-mode-map)
    "o" gnus-summary-save-article
    "m" gnus-summary-save-article-mail
    "F" gnus-summary-write-article-file
    "r" gnus-summary-save-article-rmail
    "f" gnus-summary-save-article-file
    "b" gnus-summary-save-article-body-file
    "h" gnus-summary-save-article-folder
    "v" gnus-summary-save-article-vm
    "p" gnus-summary-pipe-output
    "s" gnus-soup-add-article)

  (gnus-define-keys (gnus-summary-mime-map "K" gnus-summary-mode-map)
    "b" gnus-summary-display-buttonized
    "m" gnus-summary-repair-multipart
    "v" gnus-article-view-part
    "o" gnus-article-save-part
    "c" gnus-article-copy-part
    "e" gnus-article-externalize-part
    "i" gnus-article-inline-part
    "|" gnus-article-pipe-part))

(defun gnus-summary-make-menu-bar ()
  (gnus-turn-off-edit-menu 'summary)

  (unless (boundp 'gnus-summary-misc-menu)

    (easy-menu-define
     gnus-summary-kill-menu gnus-summary-mode-map ""
     (cons
      "Score"
      (nconc
       (list
	["Enter score..." gnus-summary-score-entry t]
	["Customize" gnus-score-customize t])
       (gnus-make-score-map 'increase)
       (gnus-make-score-map 'lower)
       '(("Mark"
	  ["Kill below" gnus-summary-kill-below t]
	  ["Mark above" gnus-summary-mark-above t]
	  ["Tick above" gnus-summary-tick-above t]
	  ["Clear above" gnus-summary-clear-above t])
	 ["Current score" gnus-summary-current-score t]
	 ["Set score" gnus-summary-set-score t]
	 ["Switch current score file..." gnus-score-change-score-file t]
	 ["Set mark below..." gnus-score-set-mark-below t]
	 ["Set expunge below..." gnus-score-set-expunge-below t]
	 ["Edit current score file" gnus-score-edit-current-scores t]
	 ["Edit score file" gnus-score-edit-file t]
	 ["Trace score" gnus-score-find-trace t]
	 ["Find words" gnus-score-find-favourite-words t]
	 ["Rescore buffer" gnus-summary-rescore t]
	 ["Increase score..." gnus-summary-increase-score t]
	 ["Lower score..." gnus-summary-lower-score t]))))

    ;; Define both the Article menu in the summary buffer and the equivalent
    ;; Commands menu in the article buffer here for consistency.
    (let ((innards
           '(("Hide"
              ["All" gnus-article-hide t]
              ["Headers" gnus-article-hide-headers t]
              ["Signature" gnus-article-hide-signature t]
              ["Citation" gnus-article-hide-citation t]
	      ["List identifiers" gnus-article-hide-list-identifiers t]
              ["PGP" gnus-article-hide-pgp t]
	      ["Banner" gnus-article-strip-banner t]
              ["Boring headers" gnus-article-hide-boring-headers t])
             ("Highlight"
              ["All" gnus-article-highlight t]
              ["Headers" gnus-article-highlight-headers t]
              ["Signature" gnus-article-highlight-signature t]
              ["Citation" gnus-article-highlight-citation t])
	     ("MIME"
	      ["Words" gnus-article-decode-mime-words t]
	      ["Charset" gnus-article-decode-charset t]
	      ["QP" gnus-article-de-quoted-unreadable t]
	      ["Base64" gnus-article-de-base64-unreadable t]
	      ["View all" gnus-mime-view-all-parts t])
             ("Date"
              ["Local" gnus-article-date-local t]
              ["ISO8601" gnus-article-date-iso8601 t]
              ["UT" gnus-article-date-ut t]
              ["Original" gnus-article-date-original t]
              ["Lapsed" gnus-article-date-lapsed t]
              ["User-defined" gnus-article-date-user t])
             ("Washing"
              ("Remove Blanks"
               ["Leading" gnus-article-strip-leading-blank-lines t]
               ["Multiple" gnus-article-strip-multiple-blank-lines t]
               ["Trailing" gnus-article-remove-trailing-blank-lines t]
               ["All of the above" gnus-article-strip-blank-lines t]
               ["All" gnus-article-strip-all-blank-lines t]
               ["Leading space" gnus-article-strip-leading-space t]
	       ["Trailing space" gnus-article-strip-trailing-space t])
              ["Overstrike" gnus-article-treat-overstrike t]
              ["Dumb quotes" gnus-article-treat-dumbquotes t]
              ["Emphasis" gnus-article-emphasize t]
              ["Word wrap" gnus-article-fill-cited-article t]
	      ["Fill long lines" gnus-article-fill-long-lines t]
	      ["Capitalize sentences" gnus-article-capitalize-sentences t]
              ["CR" gnus-article-remove-cr t]
              ["Show X-Face" gnus-article-display-x-face t]
              ["Quoted-Printable" gnus-article-de-quoted-unreadable t]
              ["Base64" gnus-article-de-base64-unreadable t]
              ["Rot 13" gnus-summary-caesar-message
	       :help "\"Caesar rotate\" article by 13"]
              ["Unix pipe" gnus-summary-pipe-message t]
              ["Add buttons" gnus-article-add-buttons t]
              ["Add buttons to head" gnus-article-add-buttons-to-head t]
              ["Stop page breaking" gnus-summary-stop-page-breaking t]
              ["Verbose header" gnus-summary-verbose-headers t]
              ["Toggle header" gnus-summary-toggle-header t]
	      ["Html" gnus-article-wash-html t]
	      ["HZ" gnus-article-decode-HZ t])
             ("Output"
              ["Save in default format" gnus-summary-save-article
	       :help "Save article using default method"]
              ["Save in file" gnus-summary-save-article-file
	       :help "Save article in file"]
              ["Save in Unix mail format" gnus-summary-save-article-mail t]
              ["Save in MH folder" gnus-summary-save-article-folder t]
              ["Save in VM folder" gnus-summary-save-article-vm t]
              ["Save in RMAIL mbox" gnus-summary-save-article-rmail t]
              ["Save body in file" gnus-summary-save-article-body-file t]
              ["Pipe through a filter" gnus-summary-pipe-output t]
              ["Add to SOUP packet" gnus-soup-add-article t]
              ["Print" gnus-summary-print-article t])
             ("Backend"
              ["Respool article..." gnus-summary-respool-article t]
              ["Move article..." gnus-summary-move-article
               (gnus-check-backend-function
                'request-move-article gnus-newsgroup-name)]
              ["Copy article..." gnus-summary-copy-article t]
              ["Crosspost article..." gnus-summary-crosspost-article
               (gnus-check-backend-function
                'request-replace-article gnus-newsgroup-name)]
              ["Import file..." gnus-summary-import-article t]
              ["Check if posted" gnus-summary-article-posted-p t]
              ["Edit article" gnus-summary-edit-article
               (not (gnus-group-read-only-p))]
              ["Delete article" gnus-summary-delete-article
               (gnus-check-backend-function
                'request-expire-articles gnus-newsgroup-name)]
              ["Query respool" gnus-summary-respool-query t]
	      ["Trace respool" gnus-summary-respool-trace t]
              ["Delete expirable articles" gnus-summary-expire-articles-now
               (gnus-check-backend-function
                'request-expire-articles gnus-newsgroup-name)])
             ("Extract"
              ["Uudecode" gnus-uu-decode-uu
	       :help "Decode uuencoded article(s)"]
              ["Uudecode and save" gnus-uu-decode-uu-and-save t]
              ["Unshar" gnus-uu-decode-unshar t]
              ["Unshar and save" gnus-uu-decode-unshar-and-save t]
              ["Save" gnus-uu-decode-save t]
              ["Binhex" gnus-uu-decode-binhex t]
              ["Postscript" gnus-uu-decode-postscript t])
             ("Cache"
              ["Enter article" gnus-cache-enter-article t]
              ["Remove article" gnus-cache-remove-article t])
	     ["Translate" gnus-article-babel t]
             ["Select article buffer" gnus-summary-select-article-buffer t]
             ["Enter digest buffer" gnus-summary-enter-digest-group t]
             ["Isearch article..." gnus-summary-isearch-article t]
             ["Beginning of the article" gnus-summary-beginning-of-article t]
             ["End of the article" gnus-summary-end-of-article t]
             ["Fetch parent of article" gnus-summary-refer-parent-article t]
             ["Fetch referenced articles" gnus-summary-refer-references t]
             ["Fetch current thread" gnus-summary-refer-thread t]
             ["Fetch article with id..." gnus-summary-refer-article t]
             ["Redisplay" gnus-summary-show-article t])))
      (easy-menu-define
       gnus-summary-article-menu gnus-summary-mode-map ""
       (cons "Article" innards))

      (if (not (keymapp gnus-summary-article-menu))
	  (easy-menu-define
	    gnus-article-commands-menu gnus-article-mode-map ""
	    (cons "Commands" innards))
	;; in Emacs, don't share menu.
	(setq gnus-article-commands-menu 
	      (copy-keymap gnus-summary-article-menu))
	(define-key gnus-article-mode-map [menu-bar commands]
	  (cons "Commands" gnus-article-commands-menu))))

    (easy-menu-define
     gnus-summary-thread-menu gnus-summary-mode-map ""
     '("Threads"
       ["Toggle threading" gnus-summary-toggle-threads t]
       ["Hide threads" gnus-summary-hide-all-threads t]
       ["Show threads" gnus-summary-show-all-threads t]
       ["Hide thread" gnus-summary-hide-thread t]
       ["Show thread" gnus-summary-show-thread t]
       ["Go to next thread" gnus-summary-next-thread t]
       ["Go to previous thread" gnus-summary-prev-thread t]
       ["Go down thread" gnus-summary-down-thread t]
       ["Go up thread" gnus-summary-up-thread t]
       ["Top of thread" gnus-summary-top-thread t]
       ["Mark thread as read" gnus-summary-kill-thread t]
       ["Lower thread score" gnus-summary-lower-thread t]
       ["Raise thread score" gnus-summary-raise-thread t]
       ["Rethread current" gnus-summary-rethread-current t]))

    (easy-menu-define
     gnus-summary-post-menu gnus-summary-mode-map ""
     '("Post"
       ["Post an article" gnus-summary-post-news
	:help "Post an article"]
       ["Followup" gnus-summary-followup
	:help "Post followup to this article"]
       ["Followup and yank" gnus-summary-followup-with-original
	:help "Post followup to this article, quoting its contents"]
       ["Supersede article" gnus-summary-supersede-article t]
       ["Cancel article" gnus-summary-cancel-article
	:help "Cancel an article you posted"]
       ["Reply" gnus-summary-reply t]
       ["Reply and yank" gnus-summary-reply-with-original t]
       ["Wide reply" gnus-summary-wide-reply t]
       ["Wide reply and yank" gnus-summary-wide-reply-with-original
	:help "Mail a reply, quoting this article"]
       ["Mail forward" gnus-summary-mail-forward t]
       ["Post forward" gnus-summary-post-forward t]
       ["Digest and mail" gnus-uu-digest-mail-forward t]
       ["Digest and post" gnus-uu-digest-post-forward t]
       ["Resend message" gnus-summary-resend-message t]
       ["Send bounced mail" gnus-summary-resend-bounced-mail t]
       ["Send a mail" gnus-summary-mail-other-window t]
       ["Uuencode and post" gnus-uu-post-news
	:help "Post a uuencoded article"]
       ["Followup via news" gnus-summary-followup-to-mail t]
       ["Followup via news and yank"
	gnus-summary-followup-to-mail-with-original t]
       ;;("Draft"
       ;;["Send" gnus-summary-send-draft t]
       ;;["Send bounced" gnus-resend-bounced-mail t])
       ))

    (easy-menu-define
     gnus-summary-misc-menu gnus-summary-mode-map ""
     '("Misc"
       ("Mark Read"
	["Mark as read" gnus-summary-mark-as-read-forward t]
	["Mark same subject and select"
	 gnus-summary-kill-same-subject-and-select t]
	["Mark same subject" gnus-summary-kill-same-subject t]
	["Catchup" gnus-summary-catchup
	 :help "Mark unread articles in this group as read"]
	["Catchup all" gnus-summary-catchup-all t]
	["Catchup to here" gnus-summary-catchup-to-here t]
	["Catchup region" gnus-summary-mark-region-as-read t]
	["Mark excluded" gnus-summary-limit-mark-excluded-as-read t])
       ("Mark Various"
	["Tick" gnus-summary-tick-article-forward t]
	["Mark as dormant" gnus-summary-mark-as-dormant t]
	["Remove marks" gnus-summary-clear-mark-forward t]
	["Set expirable mark" gnus-summary-mark-as-expirable t]
	["Set bookmark" gnus-summary-set-bookmark t]
	["Remove bookmark" gnus-summary-remove-bookmark t])
       ("Mark Limit"
	["Marks..." gnus-summary-limit-to-marks t]
	["Subject..." gnus-summary-limit-to-subject t]
	["Author..." gnus-summary-limit-to-author t]
	["Age..." gnus-summary-limit-to-age t]
	["Extra..." gnus-summary-limit-to-extra t]
	["Score" gnus-summary-limit-to-score t]
	["Unread" gnus-summary-limit-to-unread t]
	["Non-dormant" gnus-summary-limit-exclude-dormant t]
	["Articles" gnus-summary-limit-to-articles t]
	["Pop limit" gnus-summary-pop-limit t]
	["Show dormant" gnus-summary-limit-include-dormant t]
	["Hide childless dormant"
	 gnus-summary-limit-exclude-childless-dormant t]
	;;["Hide thread" gnus-summary-limit-exclude-thread t]
	["Hide marked" gnus-summary-limit-exclude-marks t]
	["Show expunged" gnus-summary-show-all-expunged t])
       ("Process Mark"
	["Set mark" gnus-summary-mark-as-processable t]
	["Remove mark" gnus-summary-unmark-as-processable t]
	["Remove all marks" gnus-summary-unmark-all-processable t]
	["Mark above" gnus-uu-mark-over t]
	["Mark series" gnus-uu-mark-series t]
	["Mark region" gnus-uu-mark-region t]
	["Unmark region" gnus-uu-unmark-region t]
	["Mark by regexp..." gnus-uu-mark-by-regexp t]
        ["Unmark by regexp..." gnus-uu-unmark-by-regexp t]
	["Mark all" gnus-uu-mark-all t]
	["Mark buffer" gnus-uu-mark-buffer t]
	["Mark sparse" gnus-uu-mark-sparse t]
	["Mark thread" gnus-uu-mark-thread t]
	["Unmark thread" gnus-uu-unmark-thread t]
	("Process Mark Sets"
	 ["Kill" gnus-summary-kill-process-mark t]
	 ["Yank" gnus-summary-yank-process-mark
	  gnus-newsgroup-process-stack]
	 ["Save" gnus-summary-save-process-mark t]))
       ("Scroll article"
	["Page forward" gnus-summary-next-page
	 :help "Show next page of article"]
	["Page backward" gnus-summary-prev-page
	 :help "Show previous page of article"]
	["Line forward" gnus-summary-scroll-up t])
       ("Move"
	["Next unread article" gnus-summary-next-unread-article t]
	["Previous unread article" gnus-summary-prev-unread-article t]
	["Next article" gnus-summary-next-article t]
	["Previous article" gnus-summary-prev-article t]
	["Next unread subject" gnus-summary-next-unread-subject t]
	["Previous unread subject" gnus-summary-prev-unread-subject t]
	["Next article same subject" gnus-summary-next-same-subject t]
	["Previous article same subject" gnus-summary-prev-same-subject t]
	["First unread article" gnus-summary-first-unread-article t]
	["Best unread article" gnus-summary-best-unread-article t]
	["Go to subject number..." gnus-summary-goto-subject t]
	["Go to article number..." gnus-summary-goto-article t]
	["Go to the last article" gnus-summary-goto-last-article t]
	["Pop article off history" gnus-summary-pop-article t])
       ("Sort"
	["Sort by number" gnus-summary-sort-by-number t]
	["Sort by author" gnus-summary-sort-by-author t]
	["Sort by subject" gnus-summary-sort-by-subject t]
	["Sort by date" gnus-summary-sort-by-date t]
	["Sort by score" gnus-summary-sort-by-score t]
	["Sort by lines" gnus-summary-sort-by-lines t]
	["Sort by characters" gnus-summary-sort-by-chars t])
       ("Help"
	["Fetch group FAQ" gnus-summary-fetch-faq t]
	["Describe group" gnus-summary-describe-group t]
	["Read manual" gnus-info-find-node t])
       ("Modes"
	["Pick and read" gnus-pick-mode t]
	["Binary" gnus-binary-mode t])
       ("Regeneration"
	["Regenerate" gnus-summary-prepare t]
	["Insert cached articles" gnus-summary-insert-cached-articles t]
	["Toggle threading" gnus-summary-toggle-threads t])
       ["Filter articles..." gnus-summary-execute-command t]
       ["Run command on subjects..." gnus-summary-universal-argument t]
       ["Search articles forward..." gnus-summary-search-article-forward t]
       ["Search articles backward..." gnus-summary-search-article-backward t]
       ["Toggle line truncation" gnus-summary-toggle-truncation t]
       ["Expand window" gnus-summary-expand-window t]
       ["Expire expirable articles" gnus-summary-expire-articles
	(gnus-check-backend-function
	 'request-expire-articles gnus-newsgroup-name)]
       ["Edit local kill file" gnus-summary-edit-local-kill t]
       ["Edit main kill file" gnus-summary-edit-global-kill t]
       ["Edit group parameters" gnus-summary-edit-parameters t]
       ["Customize group parameters" gnus-summary-customize-parameters t]
       ["Send a bug report" gnus-bug t]
       ("Exit"
	["Catchup and exit" gnus-summary-catchup-and-exit
	 :help "Mark unread articles in this group as read, then exit"]
	["Catchup all and exit" gnus-summary-catchup-all-and-exit t]
	["Catchup and goto next" gnus-summary-catchup-and-goto-next-group t]
	["Exit group" gnus-summary-exit
	 :help "Exit current group, return to group selection mode"]
	["Exit group without updating" gnus-summary-exit-no-update t]
	["Exit and goto next group" gnus-summary-next-group t]
	["Exit and goto prev group" gnus-summary-prev-group t]
	["Reselect group" gnus-summary-reselect-current-group t]
	["Rescan group" gnus-summary-rescan-group t]
	["Update dribble" gnus-summary-save-newsrc t])))

    (gnus-run-hooks 'gnus-summary-menu-hook)))

(defvar gnus-summary-tool-bar-map nil)

;; Emacs 21 tool bar.  Should be no-op otherwise.
;; NB: A new function tool-bar-local-item-from-menu is added in Emacs
;; 21.2.50+.  Considering many users use Emacs 21, use
;; tool-bar-add-item-from-menu here.
(defun gnus-summary-make-tool-bar ()
  (if (and
       (condition-case nil (require 'tool-bar) (error nil))
       (fboundp 'tool-bar-add-item-from-menu)
       (default-value 'tool-bar-mode)
       (not gnus-summary-tool-bar-map))
      (setq gnus-summary-tool-bar-map
	    (let ((tool-bar-map (make-sparse-keymap)))
	      (tool-bar-add-item-from-menu
	       'gnus-summary-prev-unread "prev-ur" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-next-unread "next-ur" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-post-news "post" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-followup-with-original "fuwo" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-followup "followup" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-reply-with-original "reply-wo" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-reply "reply" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-caesar-message "rot13" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-uu-decode-uu "uu-decode" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-save-article-file "save-aif" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-save-article "save-art" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-uu-post-news "uu-post" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-catchup "catchup" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-catchup-and-exit "cu-exit" gnus-summary-mode-map)
	      (tool-bar-add-item-from-menu
	       'gnus-summary-exit "exit-summ" gnus-summary-mode-map)
	      tool-bar-map)))
  (if gnus-summary-tool-bar-map
      (set (make-local-variable 'tool-bar-map) gnus-summary-tool-bar-map)))

(defun gnus-score-set-default (var value)
  "A version of set that updates the GNU Emacs menu-bar."
  (set var value)
  ;; It is the message that forces the active status to be updated.
  (message ""))

(defun gnus-make-score-map (type)
  "Make a summary score map of type TYPE."
  (if t
      nil
    (let ((headers '(("author" "from" string)
		     ("subject" "subject" string)
		     ("article body" "body" string)
		     ("article head" "head" string)
		     ("xref" "xref" string)
		     ("extra header" "extra" string)
		     ("lines" "lines" number)
		     ("followups to author" "followup" string)))
	  (types '((number ("less than" <)
			   ("greater than" >)
			   ("equal" =))
		   (string ("substring" s)
			   ("exact string" e)
			   ("fuzzy string" f)
			   ("regexp" r))))
	  (perms '(("temporary" (current-time-string))
		   ("permanent" nil)
		   ("immediate" now)))
	  header)
      (list
       (apply
	'nconc
	(list
	 (if (eq type 'lower)
	     "Lower score"
	   "Increase score"))
	(let (outh)
	  (while headers
	    (setq header (car headers))
	    (setq outh
		  (cons
		   (apply
		    'nconc
		    (list (car header))
		    (let ((ts (cdr (assoc (nth 2 header) types)))
			  outt)
		      (while ts
			(setq outt
			      (cons
			       (apply
				'nconc
				(list (caar ts))
				(let ((ps perms)
				      outp)
				  (while ps
				    (setq outp
					  (cons
					   (vector
					    (caar ps)
					    (list
					     'gnus-summary-score-entry
					     (nth 1 header)
					     (if (or (string= (nth 1 header)
							      "head")
						     (string= (nth 1 header)
							      "body"))
						 ""
					       (list 'gnus-summary-header
						     (nth 1 header)))
					     (list 'quote (nth 1 (car ts)))
					     (list 'gnus-score-delta-default
						   nil)
					     (nth 1 (car ps))
					     t)
					    t)
					   outp))
				    (setq ps (cdr ps)))
				  (list (nreverse outp))))
			       outt))
			(setq ts (cdr ts)))
		      (list (nreverse outt))))
		   outh))
	    (setq headers (cdr headers)))
	  (list (nreverse outh))))))))



(defun gnus-summary-mode (&optional group)
  "Major mode for reading articles.

All normal editing commands are switched off.
\\<gnus-summary-mode-map>
Each line in this buffer represents one article.  To read an
article, you can, for instance, type `\\[gnus-summary-next-page]'.  To move forwards
and backwards while displaying articles, type `\\[gnus-summary-next-unread-article]' and `\\[gnus-summary-prev-unread-article]',
respectively.

You can also post articles and send mail from this buffer.  To
follow up an article, type `\\[gnus-summary-followup]'.	 To mail a reply to the author
of an article, type `\\[gnus-summary-reply]'.

There are approx. one gazillion commands you can execute in this
buffer; read the info pages for more information (`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-summary-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (when (gnus-visual-p 'summary-menu 'menu)
    (gnus-summary-make-menu-bar)
    (gnus-summary-make-tool-bar))
  (gnus-summary-make-local-variables)
  (gnus-make-thread-indent-array)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-summary-mode)
  (setq mode-name "Summary")
  (make-local-variable 'minor-mode-alist)
  (use-local-map gnus-summary-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t)		;Disable modification
  (setq truncate-lines t)
  (setq selective-display t)
  (setq selective-display-ellipses t)	;Display `...'
  (gnus-summary-set-display-table)
  (gnus-set-default-directory)
  (setq gnus-newsgroup-name group)
  (make-local-variable 'gnus-summary-line-format)
  (make-local-variable 'gnus-summary-line-format-spec)
  (make-local-variable 'gnus-summary-dummy-line-format)
  (make-local-variable 'gnus-summary-dummy-line-format-spec)
  (make-local-variable 'gnus-summary-mark-positions)
  (make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook 'gnus-set-global-variables nil t)
  (gnus-run-hooks 'gnus-summary-mode-hook)
  (mm-enable-multibyte-mule4)
  (gnus-update-format-specifications nil 'summary 'summary-mode 'summary-dummy)
  (gnus-update-summary-mark-positions))

(defun gnus-summary-make-local-variables ()
  "Make all the local summary buffer variables."
  (let (global)
    (dolist (local gnus-summary-local-variables)
      (if (consp local)
	  (progn
	    (if (eq (cdr local) 'global)
		;; Copy the global value of the variable.
		(setq global (symbol-value (car local)))
	      ;; Use the value from the list.
	      (setq global (eval (cdr local))))
	    (set (make-local-variable (car local)) global))
	;; Simple nil-valued local variable.
	(set (make-local-variable local) nil)))))

(defun gnus-summary-clear-local-variables ()
  (let ((locals gnus-summary-local-variables))
    (while locals
      (if (consp (car locals))
	  (and (vectorp (caar locals))
	       (set (caar locals) nil))
	(and (vectorp (car locals))
	     (set (car locals) nil)))
      (setq locals (cdr locals)))))

;; Summary data functions.

(defmacro gnus-data-number (data)
  `(car ,data))

(defmacro gnus-data-set-number (data number)
  `(setcar ,data ,number))

(defmacro gnus-data-mark (data)
  `(nth 1 ,data))

(defmacro gnus-data-set-mark (data mark)
  `(setcar (nthcdr 1 ,data) ,mark))

(defmacro gnus-data-pos (data)
  `(nth 2 ,data))

(defmacro gnus-data-set-pos (data pos)
  `(setcar (nthcdr 2 ,data) ,pos))

(defmacro gnus-data-header (data)
  `(nth 3 ,data))

(defmacro gnus-data-set-header (data header)
  `(setf (nth 3 ,data) ,header))

(defmacro gnus-data-level (data)
  `(nth 4 ,data))

(defmacro gnus-data-unread-p (data)
  `(= (nth 1 ,data) gnus-unread-mark))

(defmacro gnus-data-read-p (data)
  `(/= (nth 1 ,data) gnus-unread-mark))

(defmacro gnus-data-pseudo-p (data)
  `(consp (nth 3 ,data)))

(defmacro gnus-data-find (number)
  `(assq ,number gnus-newsgroup-data))

(defmacro gnus-data-find-list (number &optional data)
  `(let ((bdata ,(or data 'gnus-newsgroup-data)))
     (memq (assq ,number bdata)
	   bdata)))

(defmacro gnus-data-make (number mark pos header level)
  `(list ,number ,mark ,pos ,header ,level))

(defun gnus-data-enter (after-article number mark pos header level offset)
  (let ((data (gnus-data-find-list after-article)))
    (unless data
      (error "No such article: %d" after-article))
    (setcdr data (cons (gnus-data-make number mark pos header level)
		       (cdr data)))
    (setq gnus-newsgroup-data-reverse nil)
    (gnus-data-update-list (cddr data) offset)))

(defun gnus-data-enter-list (after-article list &optional offset)
  (when list
    (let ((data (and after-article (gnus-data-find-list after-article)))
	  (ilist list))
      (if (not (or data
		   after-article))
	  (let ((odata gnus-newsgroup-data))
	    (setq gnus-newsgroup-data (nconc list gnus-newsgroup-data))
	    (when offset
	      (gnus-data-update-list odata offset)))
	;; Find the last element in the list to be spliced into the main
	;; list.
	(while (cdr list)
	  (setq list (cdr list)))
	(if (not data)
	    (progn
	      (setcdr list gnus-newsgroup-data)
	      (setq gnus-newsgroup-data ilist)
	      (when offset
		(gnus-data-update-list (cdr list) offset)))
	  (setcdr list (cdr data))
	  (setcdr data ilist)
	  (when offset
	    (gnus-data-update-list (cdr list) offset))))
      (setq gnus-newsgroup-data-reverse nil))))

(defun gnus-data-remove (article &optional offset)
  (let ((data gnus-newsgroup-data))
    (if (= (gnus-data-number (car data)) article)
	(progn
	  (setq gnus-newsgroup-data (cdr gnus-newsgroup-data)
		gnus-newsgroup-data-reverse nil)
	  (when offset
	    (gnus-data-update-list gnus-newsgroup-data offset)))
      (while (cdr data)
	(when (= (gnus-data-number (cadr data)) article)
	  (setcdr data (cddr data))
	  (when offset
	    (gnus-data-update-list (cdr data) offset))
	  (setq data nil
		gnus-newsgroup-data-reverse nil))
	(setq data (cdr data))))))

(defmacro gnus-data-list (backward)
  `(if ,backward
       (or gnus-newsgroup-data-reverse
	   (setq gnus-newsgroup-data-reverse
		 (reverse gnus-newsgroup-data)))
     gnus-newsgroup-data))

(defun gnus-data-update-list (data offset)
  "Add OFFSET to the POS of all data entries in DATA."
  (setq gnus-newsgroup-data-reverse nil)
  (while data
    (setcar (nthcdr 2 (car data)) (+ offset (nth 2 (car data))))
    (setq data (cdr data))))

(defun gnus-summary-article-pseudo-p (article)
  "Say whether this article is a pseudo article or not."
  (not (vectorp (gnus-data-header (gnus-data-find article)))))

(defmacro gnus-summary-article-sparse-p (article)
  "Say whether this article is a sparse article or not."
  `(memq ,article gnus-newsgroup-sparse))

(defmacro gnus-summary-article-ancient-p (article)
  "Say whether this article is a sparse article or not."
  `(memq ,article gnus-newsgroup-ancient))

(defun gnus-article-parent-p (number)
  "Say whether this article is a parent or not."
  (let ((data (gnus-data-find-list number)))
    (and (cdr data)			; There has to be an article after...
	 (< (gnus-data-level (car data)) ; And it has to have a higher level.
	    (gnus-data-level (nth 1 data))))))

(defun gnus-article-children (number)
  "Return a list of all children to NUMBER."
  (let* ((data (gnus-data-find-list number))
	 (level (gnus-data-level (car data)))
	 children)
    (setq data (cdr data))
    (while (and data
		(= (gnus-data-level (car data)) (1+ level)))
      (push (gnus-data-number (car data)) children)
      (setq data (cdr data)))
    children))

(defmacro gnus-summary-skip-intangible ()
  "If the current article is intangible, then jump to a different article."
  '(let ((to (get-text-property (point) 'gnus-intangible)))
     (and to (gnus-summary-goto-subject to))))

(defmacro gnus-summary-article-intangible-p ()
  "Say whether this article is intangible or not."
  '(get-text-property (point) 'gnus-intangible))

(defun gnus-article-read-p (article)
  "Say whether ARTICLE is read or not."
  (not (or (memq article gnus-newsgroup-marked)
	   (memq article gnus-newsgroup-unreads)
	   (memq article gnus-newsgroup-unselected)
	   (memq article gnus-newsgroup-dormant))))

;; Some summary mode macros.

(defmacro gnus-summary-article-number ()
  "The article number of the article on the current line.
If there isn's an article number here, then we return the current
article number."
  '(progn
     (gnus-summary-skip-intangible)
     (or (get-text-property (point) 'gnus-number)
	 (gnus-summary-last-subject))))

(defmacro gnus-summary-article-header (&optional number)
  "Return the header of article NUMBER."
  `(gnus-data-header (gnus-data-find
		      ,(or number '(gnus-summary-article-number)))))

(defmacro gnus-summary-thread-level (&optional number)
  "Return the level of thread that starts with article NUMBER."
  `(if (and (eq gnus-summary-make-false-root 'dummy)
	    (get-text-property (point) 'gnus-intangible))
       0
     (gnus-data-level (gnus-data-find
		       ,(or number '(gnus-summary-article-number))))))

(defmacro gnus-summary-article-mark (&optional number)
  "Return the mark of article NUMBER."
  `(gnus-data-mark (gnus-data-find
		    ,(or number '(gnus-summary-article-number)))))

(defmacro gnus-summary-article-pos (&optional number)
  "Return the position of the line of article NUMBER."
  `(gnus-data-pos (gnus-data-find
		   ,(or number '(gnus-summary-article-number)))))

(defalias 'gnus-summary-subject-string 'gnus-summary-article-subject)
(defmacro gnus-summary-article-subject (&optional number)
  "Return current subject string or nil if nothing."
  `(let ((headers
	  ,(if number
	       `(gnus-data-header (assq ,number gnus-newsgroup-data))
	     '(gnus-data-header (assq (gnus-summary-article-number)
				      gnus-newsgroup-data)))))
     (and headers
	  (vectorp headers)
	  (mail-header-subject headers))))

(defmacro gnus-summary-article-score (&optional number)
  "Return current article score."
  `(or (cdr (assq ,(or number '(gnus-summary-article-number))
		  gnus-newsgroup-scored))
       gnus-summary-default-score 0))

(defun gnus-summary-article-children (&optional number)
  "Return a list of article numbers that are children of article NUMBER."
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))))
	 (level (gnus-data-level (car data)))
	 l children)
    (while (and (setq data (cdr data))
		(> (setq l (gnus-data-level (car data))) level))
      (and (= (1+ level) l)
	   (push (gnus-data-number (car data))
		 children)))
    (nreverse children)))

(defun gnus-summary-article-parent (&optional number)
  "Return the article number of the parent of article NUMBER."
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))
				    (gnus-data-list t)))
	 (level (gnus-data-level (car data))))
    (if (zerop level)
	()				; This is a root.
      ;; We search until we find an article with a level less than
      ;; this one.  That function has to be the parent.
      (while (and (setq data (cdr data))
		  (not (< (gnus-data-level (car data)) level))))
      (and data (gnus-data-number (car data))))))

(defun gnus-unread-mark-p (mark)
  "Say whether MARK is the unread mark."
  (= mark gnus-unread-mark))

(defun gnus-read-mark-p (mark)
  "Say whether MARK is one of the marks that mark as read.
This is all marks except unread, ticked, dormant, and expirable."
  (not (or (= mark gnus-unread-mark)
	   (= mark gnus-ticked-mark)
	   (= mark gnus-dormant-mark)
	   (= mark gnus-expirable-mark))))

(defmacro gnus-article-mark (number)
  "Return the MARK of article NUMBER.
This macro should only be used when computing the mark the \"first\"
time; i.e., when generating the summary lines.  After that,
`gnus-summary-article-mark' should be used to examine the
marks of articles."
  `(cond
    ((memq ,number gnus-newsgroup-unsendable) gnus-unsendable-mark)
    ((memq ,number gnus-newsgroup-undownloaded) gnus-undownloaded-mark)
    ((memq ,number gnus-newsgroup-downloadable) gnus-downloadable-mark)
    ((memq ,number gnus-newsgroup-unreads) gnus-unread-mark)
    ((memq ,number gnus-newsgroup-marked) gnus-ticked-mark)
    ((memq ,number gnus-newsgroup-dormant) gnus-dormant-mark)
    ((memq ,number gnus-newsgroup-expirable) gnus-expirable-mark)
    (t (or (cdr (assq ,number gnus-newsgroup-reads))
	   gnus-ancient-mark))))

;; Saving hidden threads.

(put 'gnus-save-hidden-threads 'lisp-indent-function 0)
(put 'gnus-save-hidden-threads 'edebug-form-spec '(body))

(defmacro gnus-save-hidden-threads (&rest forms)
  "Save hidden threads, eval FORMS, and restore the hidden threads."
  (let ((config (make-symbol "config")))
    `(let ((,config (gnus-hidden-threads-configuration)))
       (unwind-protect
	   (save-excursion
	     ,@forms)
	 (gnus-restore-hidden-threads-configuration ,config)))))

(defun gnus-data-compute-positions ()
  "Compute the positions of all articles."
  (setq gnus-newsgroup-data-reverse nil)
  (let ((data gnus-newsgroup-data))
    (save-excursion
      (gnus-save-hidden-threads
	(gnus-summary-show-all-threads)
	(goto-char (point-min))
	(while data
	  (while (get-text-property (point) 'gnus-intangible)
	    (forward-line 1))
	  (gnus-data-set-pos (car data) (+ (point) 3))
	  (setq data (cdr data))
	  (forward-line 1))))))

(defun gnus-hidden-threads-configuration ()
  "Return the current hidden threads configuration."
  (save-excursion
    (let (config)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(push (1- (point)) config))
      config)))

(defun gnus-restore-hidden-threads-configuration (config)
  "Restore hidden threads configuration from CONFIG."
  (save-excursion
    (let (point buffer-read-only)
      (while (setq point (pop config))
	(when (and (< point (point-max))
		   (goto-char point)
		   (eq (char-after) ?\n))
	  (subst-char-in-region point (1+ point) ?\n ?\r))))))

;; Various summary mode internalish functions.

(defun gnus-mouse-pick-article (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-summary-next-page nil t))

(defun gnus-summary-set-display-table ()
  "Change the display table.
Odd characters have a tendency to mess
up nicely formatted displays - we make all possible glyphs
display only a single character."

  ;; We start from the standard display table, if any.
  (let ((table (or (copy-sequence standard-display-table)
		   (make-display-table)))
	(i 32))
    ;; Nix out all the control chars...
    (while (>= (setq i (1- i)) 0)
      (aset table i [??]))
    ;; ... but not newline and cr, of course.  (cr is necessary for the
    ;; selective display).
    (aset table ?\n nil)
    (aset table ?\r nil)
    ;; We keep TAB as well.
    (aset table ?\t nil)
    ;; We nix out any glyphs over 126 that are not set already.
    (let ((i 256))
      (while (>= (setq i (1- i)) 127)
	;; Only modify if the entry is nil.
	(unless (aref table i)
	  (aset table i [??]))))
    (setq buffer-display-table table)))

(defun gnus-summary-setup-buffer (group)
  "Initialize summary buffer."
  (let ((buffer (concat "*Summary " group "*")))
    (if (get-buffer buffer)
	(progn
	  (set-buffer buffer)
	  (setq gnus-summary-buffer (current-buffer))
	  (not gnus-newsgroup-prepared))
      ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
      (setq gnus-summary-buffer (set-buffer (gnus-get-buffer-create buffer)))
      (gnus-summary-mode group)
      (when gnus-carpal
	(gnus-carpal-setup-buffer 'summary))
      (unless gnus-single-article-buffer
	(make-local-variable 'gnus-article-buffer)
	(make-local-variable 'gnus-article-current)
	(make-local-variable 'gnus-original-article-buffer))
      (setq gnus-newsgroup-name group)
      t)))

(defun gnus-set-global-variables ()
  "Set the global equivalents of the buffer-local variables.
They are set to the latest values they had.  These reflect the summary
buffer that was in action when the last article was fetched."
  (when (eq major-mode 'gnus-summary-mode)
    (setq gnus-summary-buffer (current-buffer))
    (let ((name gnus-newsgroup-name)
	  (marked gnus-newsgroup-marked)
	  (unread gnus-newsgroup-unreads)
	  (headers gnus-current-headers)
	  (data gnus-newsgroup-data)
	  (summary gnus-summary-buffer)
	  (article-buffer gnus-article-buffer)
	  (original gnus-original-article-buffer)
	  (gac gnus-article-current)
	  (reffed gnus-reffed-article-number)
	  (score-file gnus-current-score-file)
	  (default-charset gnus-newsgroup-charset))
      (save-excursion
	(set-buffer gnus-group-buffer)
	(setq gnus-newsgroup-name name
	      gnus-newsgroup-marked marked
	      gnus-newsgroup-unreads unread
	      gnus-current-headers headers
	      gnus-newsgroup-data data
	      gnus-article-current gac
	      gnus-summary-buffer summary
	      gnus-article-buffer article-buffer
	      gnus-original-article-buffer original
	      gnus-reffed-article-number reffed
	      gnus-current-score-file score-file
	      gnus-newsgroup-charset default-charset)
	;; The article buffer also has local variables.
	(when (gnus-buffer-live-p gnus-article-buffer)
	  (set-buffer gnus-article-buffer)
	  (setq gnus-summary-buffer summary))))))

(defun gnus-summary-article-unread-p (article)
  "Say whether ARTICLE is unread or not."
  (memq article gnus-newsgroup-unreads))

(defun gnus-summary-first-article-p (&optional article)
  "Return whether ARTICLE is the first article in the buffer."
  (if (not (setq article (or article (gnus-summary-article-number))))
      nil
    (eq article (caar gnus-newsgroup-data))))

(defun gnus-summary-last-article-p (&optional article)
  "Return whether ARTICLE is the last article in the buffer."
  (if (not (setq article (or article (gnus-summary-article-number))))
      ;; All non-existent numbers are the last article.  :-)
      t
    (not (cdr (gnus-data-find-list article)))))

(defun gnus-make-thread-indent-array ()
  (let ((n 200))
    (unless (and gnus-thread-indent-array
		 (= gnus-thread-indent-level gnus-thread-indent-array-level))
      (setq gnus-thread-indent-array (make-vector 201 "")
	    gnus-thread-indent-array-level gnus-thread-indent-level)
      (while (>= n 0)
	(aset gnus-thread-indent-array n
	      (make-string (* n gnus-thread-indent-level) ? ))
	(setq n (1- n))))))

(defun gnus-update-summary-mark-positions ()
  "Compute where the summary marks are to go."
  (save-excursion
    (when (gnus-buffer-exists-p gnus-summary-buffer)
      (set-buffer gnus-summary-buffer))
    (let ((gnus-replied-mark 129)
	  (gnus-score-below-mark 130)
	  (gnus-score-over-mark 130)
	  (gnus-download-mark 131)
	  (spec gnus-summary-line-format-spec)
	  gnus-visual pos)
      (save-excursion
	(gnus-set-work-buffer)
	(let ((gnus-summary-line-format-spec spec)
	      (gnus-newsgroup-downloadable '((0 . t))))
	  (gnus-summary-insert-line
	   [0 "" "" "" "" "" 0 0 "" nil]  0 nil 128 t nil "" nil 1)
	  (goto-char (point-min))
	  (setq pos (list (cons 'unread (and (search-forward "\200" nil t)
					     (- (point) 2)))))
	  (goto-char (point-min))
	  (push (cons 'replied (and (search-forward "\201" nil t)
				    (- (point) 2)))
		pos)
	  (goto-char (point-min))
	  (push (cons 'score (and (search-forward "\202" nil t) (- (point) 2)))
		pos)
	  (goto-char (point-min))
	  (push (cons 'download
		      (and (search-forward "\203" nil t) (- (point) 2)))
		pos)))
      (setq gnus-summary-mark-positions pos))))

(defun gnus-summary-insert-dummy-line (gnus-tmp-subject gnus-tmp-number)
  "Insert a dummy root in the summary buffer."
  (beginning-of-line)
  (gnus-add-text-properties
   (point) (progn (eval gnus-summary-dummy-line-format-spec) (point))
   (list 'gnus-number gnus-tmp-number 'gnus-intangible gnus-tmp-number)))

(defun gnus-summary-from-or-to-or-newsgroups (header)
  (let ((to (cdr (assq 'To (mail-header-extra header))))
	(newsgroups (cdr (assq 'Newsgroups (mail-header-extra header))))
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-excursion (set-buffer gnus-summary-buffer)
			 gnus-newsgroup-ignored-charsets)))
    (cond
     ((and to
	   gnus-ignored-from-addresses
	   (string-match gnus-ignored-from-addresses
			 (mail-header-from header)))
      (concat "-> "
	      (or (car (funcall gnus-extract-address-components
				(funcall
				 gnus-decode-encoded-word-function to)))
		  (funcall gnus-decode-encoded-word-function to))))
     ((and newsgroups
	   gnus-ignored-from-addresses
	   (string-match gnus-ignored-from-addresses
			 (mail-header-from header)))
      (concat "=> " newsgroups))
     (t
      (or (car (funcall gnus-extract-address-components
			(mail-header-from header)))
	  (mail-header-from header))))))

(defun gnus-summary-insert-line (gnus-tmp-header
				 gnus-tmp-level gnus-tmp-current
				 gnus-tmp-unread gnus-tmp-replied
				 gnus-tmp-expirable gnus-tmp-subject-or-nil
				 &optional gnus-tmp-dummy gnus-tmp-score
				 gnus-tmp-process)
  (let* ((gnus-tmp-indentation (aref gnus-thread-indent-array gnus-tmp-level))
	 (gnus-tmp-lines (mail-header-lines gnus-tmp-header))
	 (gnus-tmp-score (or gnus-tmp-score gnus-summary-default-score 0))
	 (gnus-tmp-score-char
	  (if (or (null gnus-summary-default-score)
		  (<= (abs (- gnus-tmp-score gnus-summary-default-score))
		      gnus-summary-zcore-fuzz))
	      ?  ;Whitespace
	    (if (< gnus-tmp-score gnus-summary-default-score)
		gnus-score-below-mark gnus-score-over-mark)))
	 (gnus-tmp-replied
	  (cond (gnus-tmp-process gnus-process-mark)
		((memq gnus-tmp-current gnus-newsgroup-cached)
		 gnus-cached-mark)
		(gnus-tmp-replied gnus-replied-mark)
		((memq gnus-tmp-current gnus-newsgroup-saved)
		 gnus-saved-mark)
		(t gnus-unread-mark)))
	 (gnus-tmp-from (mail-header-from gnus-tmp-header))
	 (gnus-tmp-name
	  (cond
	   ((string-match "<[^>]+> *$" gnus-tmp-from)
	    (let ((beg (match-beginning 0)))
	      (or (and (string-match "^\"[^\"]*\"" gnus-tmp-from)
		       (substring gnus-tmp-from (1+ (match-beginning 0))
				  (1- (match-end 0))))
		  (substring gnus-tmp-from 0 beg))))
	   ((string-match "(.+)" gnus-tmp-from)
	    (substring gnus-tmp-from
		       (1+ (match-beginning 0)) (1- (match-end 0))))
	   (t gnus-tmp-from)))
	 (gnus-tmp-subject (mail-header-subject gnus-tmp-header))
	 (gnus-tmp-number (mail-header-number gnus-tmp-header))
	 (gnus-tmp-opening-bracket (if gnus-tmp-dummy ?\< ?\[))
	 (gnus-tmp-closing-bracket (if gnus-tmp-dummy ?\> ?\]))
	 (buffer-read-only nil))
    (when (string= gnus-tmp-name "")
      (setq gnus-tmp-name gnus-tmp-from))
    (unless (numberp gnus-tmp-lines)
      (setq gnus-tmp-lines 0))
    (gnus-put-text-property
     (point)
     (progn (eval gnus-summary-line-format-spec) (point))
     'gnus-number gnus-tmp-number)
    (when (gnus-visual-p 'summary-highlight 'highlight)
      (forward-line -1)
      (gnus-run-hooks 'gnus-summary-update-hook)
      (forward-line 1))))

(defun gnus-summary-update-line (&optional dont-update)
  "Update summary line after change."
  (when (and gnus-summary-default-score
	     (not gnus-summary-inhibit-highlight))
    (let* ((gnus-summary-inhibit-highlight t) ; Prevent recursion.
	   (article (gnus-summary-article-number))
	   (score (gnus-summary-article-score article)))
      (unless dont-update
	(if (and gnus-summary-mark-below
		 (< (gnus-summary-article-score)
		    gnus-summary-mark-below))
	    ;; This article has a low score, so we mark it as read.
	    (when (memq article gnus-newsgroup-unreads)
	      (gnus-summary-mark-article-as-read gnus-low-score-mark))
	  (when (eq (gnus-summary-article-mark) gnus-low-score-mark)
	    ;; This article was previously marked as read on account
	    ;; of a low score, but now it has risen, so we mark it as
	    ;; unread.
	    (gnus-summary-mark-article-as-unread gnus-unread-mark)))
	(gnus-summary-update-mark
	 (if (or (null gnus-summary-default-score)
		 (<= (abs (- score gnus-summary-default-score))
		     gnus-summary-zcore-fuzz))
	     ?  ;Whitespace
	   (if (< score gnus-summary-default-score)
	       gnus-score-below-mark gnus-score-over-mark))
	 'score))
      ;; Do visual highlighting.
      (when (gnus-visual-p 'summary-highlight 'highlight)
	(gnus-run-hooks 'gnus-summary-update-hook)))))

(defvar gnus-tmp-new-adopts nil)

(defun gnus-summary-number-of-articles-in-thread (thread &optional level char)
  "Return the number of articles in THREAD.
This may be 0 in some cases -- if none of the articles in
the thread are to be displayed."
  (let* ((number
	  ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
	  (cond
	   ((not (listp thread))
	    1)
	   ((and (consp thread) (cdr thread))
	    (apply
	     '+ 1 (mapcar
		   'gnus-summary-number-of-articles-in-thread (cdr thread))))
	   ((null thread)
	    1)
	   ((memq (mail-header-number (car thread)) gnus-newsgroup-limit)
	    1)
	   (t 0))))
    (when (and level (zerop level) gnus-tmp-new-adopts)
      (incf number
	    (apply '+ (mapcar
		       'gnus-summary-number-of-articles-in-thread
		       gnus-tmp-new-adopts))))
    (if char
	(if (> number 1) gnus-not-empty-thread-mark
	  gnus-empty-thread-mark)
      number)))

(defun gnus-summary-set-local-parameters (group)
  "Go through the local params of GROUP and set all variable specs in that list."
  (let ((params (gnus-group-find-parameter group))
	elem)
    (while params
      (setq elem (car params)
	    params (cdr params))
      (and (consp elem)			; Has to be a cons.
	   (consp (cdr elem))		; The cdr has to be a list.
	   (symbolp (car elem))		; Has to be a symbol in there.
	   (not (memq (car elem) '(quit-config))) ; Ignore quit-config.
	   (ignore-errors		; So we set it.
	     (make-local-variable (car elem))
	     (set (car elem) (eval (nth 1 elem))))))))

(defun gnus-summary-read-group (group &optional show-all no-article
				      kill-buffer no-display backward
				      select-articles)
  "Start reading news in newsgroup GROUP.
If SHOW-ALL is non-nil, already read articles are also listed.
If NO-ARTICLE is non-nil, no article is selected initially.
If NO-DISPLAY, don't generate a summary buffer."
  (let (result)
    (while (and group
		(null (setq result
			    (let ((gnus-auto-select-next nil))
			      (or (gnus-summary-read-group-1
				   group show-all no-article
				   kill-buffer no-display
				   select-articles)
				  (setq show-all nil
					select-articles nil)))))
		(eq gnus-auto-select-next 'quietly))
      (set-buffer gnus-group-buffer)
      ;; The entry function called above goes to the next
      ;; group automatically, so we go two groups back
      ;; if we are searching for the previous group.
      (when backward
	(gnus-group-prev-unread-group 2))
      (if (not (equal group (gnus-group-group-name)))
	  (setq group (gnus-group-group-name))
	(setq group nil)))
    result))

(defun gnus-summary-read-group-1 (group show-all no-article
					kill-buffer no-display
					&optional select-articles)
  ;; Killed foreign groups can't be entered.
  (when (and (not (gnus-group-native-p group))
	     (not (gnus-gethash group gnus-newsrc-hashtb)))
    (error "Dead non-native groups can't be entered"))
  (gnus-message 5 "Retrieving newsgroup: %s..." group)
  (let* ((new-group (gnus-summary-setup-buffer group))
	 (quit-config (gnus-group-quit-config group))
	 (did-select (and new-group (gnus-select-newsgroup
				     group show-all select-articles))))
    (cond
     ;; This summary buffer exists already, so we just select it.
     ((not new-group)
      (gnus-set-global-variables)
      (when kill-buffer
	(gnus-kill-or-deaden-summary kill-buffer))
      (gnus-configure-windows 'summary 'force)
      (gnus-set-mode-line 'summary)
      (gnus-summary-position-point)
      (message "")
      t)
     ;; We couldn't select this group.
     ((null did-select)
      (when (and (eq major-mode 'gnus-summary-mode)
		 (not (equal (current-buffer) kill-buffer)))
	(kill-buffer (current-buffer))
	(if (not quit-config)
	    (progn
	      ;; Update the info -- marks might need to be removed,
	      ;; for instance.
	      (gnus-summary-update-info)
	      (set-buffer gnus-group-buffer)
	      (gnus-group-jump-to-group group)
	      (gnus-group-next-unread-group 1))
	  (gnus-handle-ephemeral-exit quit-config)))
      (gnus-message 3 "Can't select group")
      nil)
     ;; The user did a `C-g' while prompting for number of articles,
     ;; so we exit this group.
     ((eq did-select 'quit)
      (and (eq major-mode 'gnus-summary-mode)
	   (not (equal (current-buffer) kill-buffer))
	   (kill-buffer (current-buffer)))
      (when kill-buffer
	(gnus-kill-or-deaden-summary kill-buffer))
      (if (not quit-config)
	  (progn
	    (set-buffer gnus-group-buffer)
	    (gnus-group-jump-to-group group)
	    (gnus-group-next-unread-group 1)
	    (gnus-configure-windows 'group 'force))
	(gnus-handle-ephemeral-exit quit-config))
      ;; Finally signal the quit.
      (signal 'quit nil))
     ;; The group was successfully selected.
     (t
      (gnus-set-global-variables)
      ;; Save the active value in effect when the group was entered.
      (setq gnus-newsgroup-active
	    (gnus-copy-sequence
	     (gnus-active gnus-newsgroup-name)))
      ;; You can change the summary buffer in some way with this hook.
      (gnus-run-hooks 'gnus-select-group-hook)
      ;; Set any local variables in the group parameters.
      (gnus-summary-set-local-parameters gnus-newsgroup-name)
      (gnus-update-format-specifications
       nil 'summary 'summary-mode 'summary-dummy)
      (gnus-update-summary-mark-positions)
      ;; Do score processing.
      (when gnus-use-scoring
	(gnus-possibly-score-headers))
      ;; Check whether to fill in the gaps in the threads.
      (when gnus-build-sparse-threads
	(gnus-build-sparse-threads))
      ;; Find the initial limit.
      (if gnus-show-threads
	  (if show-all
	      (let ((gnus-newsgroup-dormant nil))
		(gnus-summary-initial-limit show-all))
	    (gnus-summary-initial-limit show-all))
	;; When untreaded, all articles are always shown.
	(setq gnus-newsgroup-limit
	      (mapcar
	       (lambda (header) (mail-header-number header))
	       gnus-newsgroup-headers)))
      ;; Generate the summary buffer.
      (unless no-display
	(gnus-summary-prepare))
      (when gnus-use-trees
	(gnus-tree-open group)
	(setq gnus-summary-highlight-line-function
	      'gnus-tree-highlight-article))
      ;; If the summary buffer is empty, but there are some low-scored
      ;; articles or some excluded dormants, we include these in the
      ;; buffer.
      (when (and (zerop (buffer-size))
		 (not no-display))
	(cond (gnus-newsgroup-dormant
	       (gnus-summary-limit-include-dormant))
	      ((and gnus-newsgroup-scored show-all)
	       (gnus-summary-limit-include-expunged t))))
      ;; Function `gnus-apply-kill-file' must be called in this hook.
      (gnus-run-hooks 'gnus-apply-kill-hook)
      (if (and (zerop (buffer-size))
	       (not no-display))
	  (progn
	    ;; This newsgroup is empty.
	    (gnus-summary-catchup-and-exit nil t)
	    (gnus-message 6 "No unread news")
	    (when kill-buffer
	      (gnus-kill-or-deaden-summary kill-buffer))
	    ;; Return nil from this function.
	    nil)
	;; Hide conversation thread subtrees.  We cannot do this in
	;; gnus-summary-prepare-hook since kill processing may not
	;; work with hidden articles.
	(and gnus-show-threads
	     gnus-thread-hide-subtree
	     (gnus-summary-hide-all-threads))
	(when kill-buffer
	  (gnus-kill-or-deaden-summary kill-buffer))
	;; Show first unread article if requested.
	(if (and (not no-article)
		 (not no-display)
		 gnus-newsgroup-unreads
		 gnus-auto-select-first)
	    (progn
	      (gnus-configure-windows 'summary)
	      (cond
	       ((eq gnus-auto-select-first 'best)
		(gnus-summary-best-unread-article))
	       ((eq gnus-auto-select-first t)
		(gnus-summary-first-unread-article))
	       ((gnus-functionp gnus-auto-select-first)
		(funcall gnus-auto-select-first))))
	  ;; Don't select any articles, just move point to the first
	  ;; article in the group.
	  (goto-char (point-min))
	  (gnus-summary-position-point)
	  (gnus-configure-windows 'summary 'force)
	  (gnus-set-mode-line 'summary))
	(when (get-buffer-window gnus-group-buffer t)
	  ;; Gotta use windows, because recenter does weird stuff if
	  ;; the current buffer ain't the displayed window.
	  (let ((owin (selected-window)))
	    (select-window (get-buffer-window gnus-group-buffer t))
	    (when (gnus-group-goto-group group)
	      (recenter))
	    (select-window owin)))
	;; Mark this buffer as "prepared".
	(setq gnus-newsgroup-prepared t)
	(gnus-run-hooks 'gnus-summary-prepared-hook)
	t)))))

(defun gnus-summary-prepare ()
  "Generate the summary buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (setq gnus-newsgroup-data nil
	  gnus-newsgroup-data-reverse nil)
    (gnus-run-hooks 'gnus-summary-generate-hook)
    ;; Generate the buffer, either with threads or without.
    (when gnus-newsgroup-headers
      (gnus-summary-prepare-threads
       (if gnus-show-threads
	   (gnus-sort-gathered-threads
	    (funcall gnus-summary-thread-gathering-function
		     (gnus-sort-threads
		      (gnus-cut-threads (gnus-make-threads)))))
	 ;; Unthreaded display.
	 (gnus-sort-articles gnus-newsgroup-headers))))
    (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
    ;; Call hooks for modifying summary buffer.
    (goto-char (point-min))
    (gnus-run-hooks 'gnus-summary-prepare-hook)))

(defsubst gnus-general-simplify-subject (subject)
  "Simply subject by the same rules as gnus-gather-threads-by-subject."
  (setq subject
	(cond
	 ;; Truncate the subject.
	 (gnus-simplify-subject-functions
	  (gnus-map-function gnus-simplify-subject-functions subject))
	 ((numberp gnus-summary-gather-subject-limit)
	  (setq subject (gnus-simplify-subject-re subject))
	  (if (> (length subject) gnus-summary-gather-subject-limit)
	      (substring subject 0 gnus-summary-gather-subject-limit)
	    subject))
	 ;; Fuzzily simplify it.
	 ((eq 'fuzzy gnus-summary-gather-subject-limit)
	  (gnus-simplify-subject-fuzzy subject))
	 ;; Just remove the leading "Re:".
	 (t
	  (gnus-simplify-subject-re subject))))

  (if (and gnus-summary-gather-exclude-subject
	   (string-match gnus-summary-gather-exclude-subject subject))
      nil				; This article shouldn't be gathered
    subject))

(defun gnus-summary-simplify-subject-query ()
  "Query where the respool algorithm would put this article."
  (interactive)
  (gnus-summary-select-article)
  (message (gnus-general-simplify-subject (gnus-summary-article-subject))))

(defun gnus-gather-threads-by-subject (threads)
  "Gather threads by looking at Subject headers."
  (if (not gnus-summary-make-false-root)
      threads
    (let ((hashtb (gnus-make-hashtable 1024))
	  (prev threads)
	  (result threads)
	  subject hthread whole-subject)
      (while threads
	(setq subject (gnus-general-simplify-subject
		       (setq whole-subject (mail-header-subject
					    (caar threads)))))
	(when subject
	  (if (setq hthread (gnus-gethash subject hashtb))
	      (progn
		;; We enter a dummy root into the thread, if we
		;; haven't done that already.
		(unless (stringp (caar hthread))
		  (setcar hthread (list whole-subject (car hthread))))
		;; We add this new gathered thread to this gathered
		;; thread.
		(setcdr (car hthread)
			(nconc (cdar hthread) (list (car threads))))
		;; Remove it from the list of threads.
		(setcdr prev (cdr threads))
		(setq threads prev))
	    ;; Enter this thread into the hash table.
	    (gnus-sethash subject threads hashtb)))
	(setq prev threads)
	(setq threads (cdr threads)))
      result)))

(defun gnus-gather-threads-by-references (threads)
  "Gather threads by looking at References headers."
  (let ((idhashtb (gnus-make-hashtable 1024))
	(thhashtb (gnus-make-hashtable 1024))
	(prev threads)
	(result threads)
	ids references id gthread gid entered ref)
    (while threads
      (when (setq references (mail-header-references (caar threads)))
	(setq id (mail-header-id (caar threads))
	      ids (gnus-split-references references)
	      entered nil)
	(while (setq ref (pop ids))
	  (setq ids (delete ref ids))
	  (if (not (setq gid (gnus-gethash ref idhashtb)))
	      (progn
		(gnus-sethash ref id idhashtb)
		(gnus-sethash id threads thhashtb))
	    (setq gthread (gnus-gethash gid thhashtb))
	    (unless entered
	      ;; We enter a dummy root into the thread, if we
	      ;; haven't done that already.
	      (unless (stringp (caar gthread))
		(setcar gthread (list (mail-header-subject (caar gthread))
				      (car gthread))))
	      ;; We add this new gathered thread to this gathered
	      ;; thread.
	      (setcdr (car gthread)
		      (nconc (cdar gthread) (list (car threads)))))
	    ;; Add it into the thread hash table.
	    (gnus-sethash id gthread thhashtb)
	    (setq entered t)
	    ;; Remove it from the list of threads.
	    (setcdr prev (cdr threads))
	    (setq threads prev))))
      (setq prev threads)
      (setq threads (cdr threads)))
    result))

(defun gnus-sort-gathered-threads (threads)
  "Sort subtreads inside each gathered thread by `gnus-sort-gathered-threads-function'."
  (let ((result threads))
    (while threads
      (when (stringp (caar threads))
	(setcdr (car threads)
		(sort (cdar threads) gnus-sort-gathered-threads-function)))
      (setq threads (cdr threads)))
    result))

(defun gnus-thread-loop-p (root thread)
  "Say whether ROOT is in THREAD."
  (let ((stack (list thread))
	(infloop 0)
	th)
    (while (setq thread (pop stack))
      (setq th (cdr thread))
      (while (and th
		  (not (eq (caar th) root)))
	(pop th))
      (if th
	  ;; We have found a loop.
	  (let (ref-dep)
	    (setcdr thread (delq (car th) (cdr thread)))
	    (if (boundp (setq ref-dep (intern "none"
					      gnus-newsgroup-dependencies)))
		(setcdr (symbol-value ref-dep)
			(nconc (cdr (symbol-value ref-dep))
			       (list (car th))))
	      (set ref-dep (list nil (car th))))
	    (setq infloop 1
		  stack nil))
	;; Push all the subthreads onto the stack.
	(push (cdr thread) stack)))
    infloop))

(defun gnus-make-threads ()
  "Go through the dependency hashtb and find the roots.	 Return all threads."
  (let (threads)
    (while (catch 'infloop
	     (mapatoms
	      (lambda (refs)
		;; Deal with self-referencing References loops.
		(when (and (car (symbol-value refs))
			   (not (zerop
				 (apply
				  '+
				  (mapcar
				   (lambda (thread)
				     (gnus-thread-loop-p
				      (car (symbol-value refs)) thread))
				   (cdr (symbol-value refs)))))))
		  (setq threads nil)
		  (throw 'infloop t))
		(unless (car (symbol-value refs))
		  ;; These threads do not refer back to any other articles,
		  ;; so they're roots.
		  (setq threads (append (cdr (symbol-value refs)) threads))))
	      gnus-newsgroup-dependencies)))
    threads))

;; Build the thread tree.
(defsubst gnus-dependencies-add-header (header dependencies force-new)
  "Enter HEADER into the DEPENDENCIES table if it is not already there.

If FORCE-NEW is not nil, enter HEADER into the DEPENDENCIES table even
if it was already present.

If `gnus-summary-ignore-duplicates' is nil then duplicate Message-IDs
will not be entered in the DEPENDENCIES table.  Otherwise duplicate
Message-IDs will be renamed be renamed to a unique Message-ID before
being entered.

Returns HEADER if it was entered in the DEPENDENCIES.  Returns nil otherwise."
  (let* ((id (mail-header-id header))
	 (id-dep (and id (intern id dependencies)))
	 ref ref-dep ref-header)
    ;; Enter this `header' in the `dependencies' table.
    (cond
     ((not id-dep)
      (setq header nil))
     ;; The first two cases do the normal part: enter a new `header'
     ;; in the `dependencies' table.
     ((not (boundp id-dep))
      (set id-dep (list header)))
     ((null (car (symbol-value id-dep)))
      (setcar (symbol-value id-dep) header))

     ;; From here the `header' was already present in the
     ;; `dependencies' table.
     (force-new
      ;; Overrides an existing entry;
      ;; just set the header part of the entry.
      (setcar (symbol-value id-dep) header))

     ;; Renames the existing `header' to a unique Message-ID.
     ((not gnus-summary-ignore-duplicates)
      ;; An article with this Message-ID has already been seen.
      ;; We rename the Message-ID.
      (set (setq id-dep (intern (setq id (nnmail-message-id)) dependencies))
	   (list header))
      (mail-header-set-id header id))

     ;; The last case ignores an existing entry, except it adds any
     ;; additional Xrefs (in case the two articles came from different
     ;; servers.
     ;; Also sets `header' to `nil' meaning that the `dependencies'
     ;; table was *not* modified.
     (t
      (mail-header-set-xref
       (car (symbol-value id-dep))
       (concat (or (mail-header-xref (car (symbol-value id-dep)))
		   "")
	       (or (mail-header-xref header) "")))
      (setq header nil)))

    (when header
      ;; First check if that we are not creating a References loop.
      (setq ref (gnus-parent-id (mail-header-references header)))
      (while (and ref
		  (setq ref-dep (intern-soft ref dependencies))
		  (boundp ref-dep)
		  (setq ref-header (car (symbol-value ref-dep))))
	(if (string= id ref)
	    ;; Yuk!  This is a reference loop.  Make the article be a
	    ;; root article.
	    (progn
	      (mail-header-set-references (car (symbol-value id-dep)) "none")
	      (setq ref nil))
	  (setq ref (gnus-parent-id (mail-header-references ref-header)))))
      (setq ref (gnus-parent-id (mail-header-references header)))
      (setq ref-dep (intern (or ref "none") dependencies))
      (if (boundp ref-dep)
	  (setcdr (symbol-value ref-dep)
		  (nconc (cdr (symbol-value ref-dep))
			 (list (symbol-value id-dep))))
	(set ref-dep (list nil (symbol-value id-dep)))))
    header))

(defun gnus-build-sparse-threads ()
  (let ((headers gnus-newsgroup-headers)
	(mail-parse-charset gnus-newsgroup-charset)
	(gnus-summary-ignore-duplicates t)
	header references generation relations
	subject child end new-child date)
    ;; First we create an alist of generations/relations, where
    ;; generations is how much we trust the relation, and the relation
    ;; is parent/child.
    (gnus-message 7 "Making sparse threads...")
    (save-excursion
      (nnheader-set-temp-buffer " *gnus sparse threads*")
      (while (setq header (pop headers))
	(when (and (setq references (mail-header-references header))
		   (not (string= references "")))
	  (insert references)
	  (setq child (mail-header-id header)
		subject (mail-header-subject header)
		date (mail-header-date header)
		generation 0)
	  (while (search-backward ">" nil t)
	    (setq end (1+ (point)))
	    (when (search-backward "<" nil t)
	      (setq new-child (buffer-substring (point) end))
	      (push (list (incf generation)
			  child (setq child new-child)
			  subject date)
		    relations)))
	  (when child
	    (push (list (1+ generation) child nil subject) relations))
	  (erase-buffer)))
      (kill-buffer (current-buffer)))
    ;; Sort over trustworthiness.
    (mapcar
     (lambda (relation)
       (when (gnus-dependencies-add-header
	      (make-full-mail-header
	       gnus-reffed-article-number
	       (nth 3 relation) "" (or (nth 4 relation) "")
	       (nth 1 relation)
	       (or (nth 2 relation) "") 0 0 "")
	      gnus-newsgroup-dependencies nil)
	 (push gnus-reffed-article-number gnus-newsgroup-limit)
	 (push gnus-reffed-article-number gnus-newsgroup-sparse)
	 (push (cons gnus-reffed-article-number gnus-sparse-mark)
	       gnus-newsgroup-reads)
	 (decf gnus-reffed-article-number)))
     (sort relations 'car-less-than-car))
    (gnus-message 7 "Making sparse threads...done")))

(defun gnus-build-old-threads ()
  ;; Look at all the articles that refer back to old articles, and
  ;; fetch the headers for the articles that aren't there.  This will
  ;; build complete threads - if the roots haven't been expired by the
  ;; server, that is.
  (let ((mail-parse-charset gnus-newsgroup-charset)
	id heads)
    (mapatoms
     (lambda (refs)
       (when (not (car (symbol-value refs)))
	 (setq heads (cdr (symbol-value refs)))
	 (while heads
	   (if (memq (mail-header-number (caar heads))
		     gnus-newsgroup-dormant)
	       (setq heads (cdr heads))
	     (setq id (symbol-name refs))
	     (while (and (setq id (gnus-build-get-header id))
			 (not (car (gnus-id-to-thread id)))))
	     (setq heads nil)))))
     gnus-newsgroup-dependencies)))

;; This function has to be called with point after the article number
;; on the beginning of the line.
(defsubst gnus-nov-parse-line (number dependencies &optional force-new)
  (let ((eol (gnus-point-at-eol))
	(buffer (current-buffer))
	header)

    ;; overview: [num subject from date id refs chars lines misc]
    (unwind-protect
	(progn
	  (narrow-to-region (point) eol)
	  (unless (eobp)
	    (forward-char))

	  (setq header
		(make-full-mail-header
		 number			; number
		 (funcall gnus-decode-encoded-word-function
			  (nnheader-nov-field))	; subject
		 (funcall gnus-decode-encoded-word-function
			  (nnheader-nov-field))	; from
		 (nnheader-nov-field)	; date
		 (nnheader-nov-read-message-id)	; id
		 (nnheader-nov-field)	; refs
		 (nnheader-nov-read-integer) ; chars
		 (nnheader-nov-read-integer) ; lines
		 (unless (eobp)
		   (if (looking-at "Xref: ")
		       (goto-char (match-end 0)))
		   (nnheader-nov-field)) ; Xref
		 (nnheader-nov-parse-extra)))) ; extra

      (widen))

    (when gnus-alter-header-function
      (funcall gnus-alter-header-function header))
    (gnus-dependencies-add-header header dependencies force-new)))

(defun gnus-build-get-header (id)
  "Look through the buffer of NOV lines and find the header to ID.
Enter this line into the dependencies hash table, and return
the id of the parent article (if any)."
  (let ((deps gnus-newsgroup-dependencies)
	found header)
    (prog1
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (let ((case-fold-search nil))
	    (goto-char (point-min))
	    (while (and (not found)
			(search-forward id nil t))
	      (beginning-of-line)
	      (setq found (looking-at
			   (format "^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t%s"
				   (regexp-quote id))))
	      (or found (beginning-of-line 2)))
	    (when found
	      (beginning-of-line)
	      (and
	       (setq header (gnus-nov-parse-line
			     (read (current-buffer)) deps))
	       (gnus-parent-id (mail-header-references header))))))
      (when header
	(let ((number (mail-header-number header)))
	  (push number gnus-newsgroup-limit)
	  (push header gnus-newsgroup-headers)
	  (if (memq number gnus-newsgroup-unselected)
	      (progn
		(push number gnus-newsgroup-unreads)
		(setq gnus-newsgroup-unselected
		      (delq number gnus-newsgroup-unselected)))
	    (push number gnus-newsgroup-ancient)))))))

(defun gnus-build-all-threads ()
  "Read all the headers."
  (let ((gnus-summary-ignore-duplicates t)
	(mail-parse-charset gnus-newsgroup-charset)
	(dependencies gnus-newsgroup-dependencies)
	header article)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (let ((case-fold-search nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (ignore-errors
	    (setq article (read (current-buffer))
		  header (gnus-nov-parse-line article dependencies)))
	  (when header
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (push header gnus-newsgroup-headers)
	      (if (memq (setq article (mail-header-number header))
			gnus-newsgroup-unselected)
		  (progn
		    (push article gnus-newsgroup-unreads)
		    (setq gnus-newsgroup-unselected
			  (delq article gnus-newsgroup-unselected)))
		(push article gnus-newsgroup-ancient)))
	    (forward-line 1)))))))

(defun gnus-summary-update-article-line (article header)
  "Update the line for ARTICLE using HEADERS."
  (let* ((id (mail-header-id header))
	 (thread (gnus-id-to-thread id)))
    (unless thread
      (error "Article in no thread"))
    ;; Update the thread.
    (setcar thread header)
    (gnus-summary-goto-subject article)
    (let* ((datal (gnus-data-find-list article))
	   (data (car datal))
	   (length (when (cdr datal)
		     (- (gnus-data-pos data)
			(gnus-data-pos (cadr datal)))))
	   (buffer-read-only nil)
	   (level (gnus-summary-thread-level)))
      (gnus-delete-line)
      (gnus-summary-insert-line
       header level nil (gnus-article-mark article)
       (memq article gnus-newsgroup-replied)
       (memq article gnus-newsgroup-expirable)
       ;; Only insert the Subject string when it's different
       ;; from the previous Subject string.
       (if (and
	    gnus-show-threads
	    (gnus-subject-equal
	     (condition-case ()
		 (mail-header-subject
		  (gnus-data-header
		   (cadr
		    (gnus-data-find-list
		     article
		     (gnus-data-list t)))))
	       ;; Error on the side of excessive subjects.
	       (error ""))
	     (mail-header-subject header)))
	   ""
	 (mail-header-subject header))
       nil (cdr (assq article gnus-newsgroup-scored))
       (memq article gnus-newsgroup-processable))
      (when length
	(gnus-data-update-list
	 (cdr datal) (- length (- (gnus-data-pos data) (point))))))))

(defun gnus-summary-update-article (article &optional iheader)
  "Update ARTICLE in the summary buffer."
  (set-buffer gnus-summary-buffer)
  (let* ((header (gnus-summary-article-header article))
	 (id (mail-header-id header))
	 (data (gnus-data-find article))
	 (thread (gnus-id-to-thread id))
	 (references (mail-header-references header))
	 (parent
	  (gnus-id-to-thread
	   (or (gnus-parent-id
		(when (and references
			   (not (equal "" references)))
		  references))
	       "none")))
	 (buffer-read-only nil)
	 (old (car thread)))
    (when thread
      (unless iheader
	(setcar thread nil)
	(when parent
	  (delq thread parent)))
      (if (gnus-summary-insert-subject id header)
	  ;; Set the (possibly) new article number in the data structure.
	  (gnus-data-set-number data (gnus-id-to-article id))
	(setcar thread old)
	nil))))

(defun gnus-rebuild-thread (id &optional line)
  "Rebuild the thread containing ID.
If LINE, insert the rebuilt thread starting on line LINE."
  (let ((buffer-read-only nil)
	old-pos current thread data)
    (if (not gnus-show-threads)
	(setq thread (list (car (gnus-id-to-thread id))))
      ;; Get the thread this article is part of.
      (setq thread (gnus-remove-thread id)))
    (setq old-pos (gnus-point-at-bol))
    (setq current (save-excursion
		    (and (re-search-backward "[\r\n]" nil t)
			 (gnus-summary-article-number))))
    ;; If this is a gathered thread, we have to go some re-gathering.
    (when (stringp (car thread))
      (let ((subject (car thread))
	    roots thr)
	(setq thread (cdr thread))
	(while thread
	  (unless (memq (setq thr (gnus-id-to-thread
				   (gnus-root-id
				    (mail-header-id (caar thread)))))
			roots)
	    (push thr roots))
	  (setq thread (cdr thread)))
	;; We now have all (unique) roots.
	(if (= (length roots) 1)
	    ;; All the loose roots are now one solid root.
	    (setq thread (car roots))
	  (setq thread (cons subject (gnus-sort-threads roots))))))
    (let (threads)
      ;; We then insert this thread into the summary buffer.
      (when line
	(goto-char (point-min))
	(forward-line (1- line)))
      (let (gnus-newsgroup-data gnus-newsgroup-threads)
	(if gnus-show-threads
	    (gnus-summary-prepare-threads (gnus-cut-threads (list thread)))
	  (gnus-summary-prepare-unthreaded thread))
	(setq data (nreverse gnus-newsgroup-data))
	(setq threads gnus-newsgroup-threads))
      ;; We splice the new data into the data structure.
      ;;!!! This is kinda bogus.  We assume that in LINE is non-nil,
      ;;!!! then we want to insert at the beginning of the buffer.
      ;;!!! That happens to be true with Gnus now, but that may
      ;;!!! change in the future.  Perhaps.
      (gnus-data-enter-list
       (if line nil current) data (- (point) old-pos))
      (setq gnus-newsgroup-threads
	    (nconc threads gnus-newsgroup-threads))
      (gnus-data-compute-positions))))

(defun gnus-number-to-header (number)
  "Return the header for article NUMBER."
  (let ((headers gnus-newsgroup-headers))
    (while (and headers
		(not (= number (mail-header-number (car headers)))))
      (pop headers))
    (when headers
      (car headers))))

(defun gnus-parent-headers (in-headers &optional generation)
  "Return the headers of the GENERATIONeth parent of HEADERS."
  (unless generation
    (setq generation 1))
  (let ((parent t)
	(headers in-headers)
	references)
    (while (and parent
		(not (zerop generation))
		(setq references (mail-header-references headers)))
      (setq headers (if (and references
			     (setq parent (gnus-parent-id references)))
			(car (gnus-id-to-thread parent))
		      nil))
      (decf generation))
    (and (not (eq headers in-headers))
	 headers)))

(defun gnus-id-to-thread (id)
  "Return the (sub-)thread where ID appears."
  (gnus-gethash id gnus-newsgroup-dependencies))

(defun gnus-id-to-article (id)
  "Return the article number of ID."
  (let ((thread (gnus-id-to-thread id)))
    (when (and thread
	       (car thread))
      (mail-header-number (car thread)))))

(defun gnus-id-to-header (id)
  "Return the article headers of ID."
  (car (gnus-id-to-thread id)))

(defun gnus-article-displayed-root-p (article)
  "Say whether ARTICLE is a root(ish) article."
  (let ((level (gnus-summary-thread-level article))
	(refs (mail-header-references  (gnus-summary-article-header article)))
	particle)
    (cond
     ((null level) nil)
     ((zerop level) t)
     ((null refs) t)
     ((null (gnus-parent-id refs)) t)
     ((and (= 1 level)
	   (null (setq particle (gnus-id-to-article
				 (gnus-parent-id refs))))
	   (null (gnus-summary-thread-level particle)))))))

(defun gnus-root-id (id)
  "Return the id of the root of the thread where ID appears."
  (let (last-id prev)
    (while (and id (setq prev (car (gnus-id-to-thread id))))
      (setq last-id id
	    id (gnus-parent-id (mail-header-references prev))))
    last-id))

(defun gnus-articles-in-thread (thread)
  "Return the list of articles in THREAD."
  (cons (mail-header-number (car thread))
	(apply 'nconc (mapcar 'gnus-articles-in-thread (cdr thread)))))

(defun gnus-remove-thread (id &optional dont-remove)
  "Remove the thread that has ID in it."
  (let (headers thread last-id)
    ;; First go up in this thread until we find the root.
    (setq last-id (gnus-root-id id)
	  headers (message-flatten-list (gnus-id-to-thread last-id)))
    ;; We have now found the real root of this thread.	It might have
    ;; been gathered into some loose thread, so we have to search
    ;; through the threads to find the thread we wanted.
    (let ((threads gnus-newsgroup-threads)
	  sub)
      (while threads
	(setq sub (car threads))
	(if (stringp (car sub))
	    ;; This is a gathered thread, so we look at the roots
	    ;; below it to find whether this article is in this
	    ;; gathered root.
	    (progn
	      (setq sub (cdr sub))
	      (while sub
		(when (member (caar sub) headers)
		  (setq thread (car threads)
			threads nil
			sub nil))
		(setq sub (cdr sub))))
	  ;; It's an ordinary thread, so we check it.
	  (when (eq (car sub) (car headers))
	    (setq thread sub
		  threads nil)))
	(setq threads (cdr threads)))
      ;; If this article is in no thread, then it's a root.
      (if thread
	  (unless dont-remove
	    (setq gnus-newsgroup-threads (delq thread gnus-newsgroup-threads)))
	(setq thread (gnus-id-to-thread last-id)))
      (when thread
	(prog1
	    thread			; We return this thread.
	  (unless dont-remove
	    (if (stringp (car thread))
		(progn
		  ;; If we use dummy roots, then we have to remove the
		  ;; dummy root as well.
		  (when (eq gnus-summary-make-false-root 'dummy)
		    ;; We go to the dummy root by going to
		    ;; the first sub-"thread", and then one line up.
		    (gnus-summary-goto-article
		     (mail-header-number (caadr thread)))
		    (forward-line -1)
		    (gnus-delete-line)
		    (gnus-data-compute-positions))
		  (setq thread (cdr thread))
		  (while thread
		    (gnus-remove-thread-1 (car thread))
		    (setq thread (cdr thread))))
	      (gnus-remove-thread-1 thread))))))))

(defun gnus-remove-thread-1 (thread)
  "Remove the thread THREAD recursively."
  (let ((number (mail-header-number (pop thread)))
	d)
    (setq thread (reverse thread))
    (while thread
      (gnus-remove-thread-1 (pop thread)))
    (when (setq d (gnus-data-find number))
      (goto-char (gnus-data-pos d))
      (gnus-summary-show-thread)
      (gnus-data-remove
       number
       (- (gnus-point-at-bol)
	  (prog1
	      (1+ (gnus-point-at-eol))
	    (gnus-delete-line)))))))

(defun gnus-sort-threads-1 (threads func)
  (sort (mapcar (lambda (thread)
		  (cons (car thread)
			(and (cdr thread)
			     (gnus-sort-threads-1 (cdr thread) func))))
		threads) func))

(defun gnus-sort-threads (threads)
  "Sort THREADS."
  (if (not gnus-thread-sort-functions)
      threads
    (gnus-message 8 "Sorting threads...")
    (prog1
	(gnus-sort-threads-1 
	 threads 
	 (gnus-make-sort-function gnus-thread-sort-functions))
      (gnus-message 8 "Sorting threads...done"))))

(defun gnus-sort-articles (articles)
  "Sort ARTICLES."
  (when gnus-article-sort-functions
    (gnus-message 7 "Sorting articles...")
    (prog1
	(setq gnus-newsgroup-headers
	      (sort articles (gnus-make-sort-function
			      gnus-article-sort-functions)))
      (gnus-message 7 "Sorting articles...done"))))

;; Written by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(defmacro gnus-thread-header (thread)
  "Return header of first article in THREAD.
Note that THREAD must never, ever be anything else than a variable -
using some other form will lead to serious barfage."
  (or (symbolp thread) (signal 'wrong-type-argument '(symbolp thread)))
  ;; (8% speedup to gnus-summary-prepare, just for fun :-)
  (list 'byte-code "\10\211:\203\17\0\211@;\203\16\0A@@\207"
	(vector thread) 2))

(defsubst gnus-article-sort-by-number (h1 h2)
  "Sort articles by article number."
  (< (mail-header-number h1)
     (mail-header-number h2)))

(defun gnus-thread-sort-by-number (h1 h2)
  "Sort threads by root article number."
  (gnus-article-sort-by-number
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-lines (h1 h2)
  "Sort articles by article Lines header."
  (< (mail-header-lines h1)
     (mail-header-lines h2)))

(defun gnus-thread-sort-by-lines (h1 h2)
  "Sort threads by root article Lines header."
  (gnus-article-sort-by-lines
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-chars (h1 h2)
  "Sort articles by octet length."
  (< (mail-header-chars h1)
     (mail-header-chars h2)))

(defun gnus-thread-sort-by-chars (h1 h2)
  "Sort threads by root article octet length."
  (gnus-article-sort-by-chars
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-author (h1 h2)
  "Sort articles by root author."
  (string-lessp
   (let ((extract (funcall
		   gnus-extract-address-components
		   (mail-header-from h1))))
     (or (car extract) (cadr extract) ""))
   (let ((extract (funcall
		   gnus-extract-address-components
		   (mail-header-from h2))))
     (or (car extract) (cadr extract) ""))))

(defun gnus-thread-sort-by-author (h1 h2)
  "Sort threads by root author."
  (gnus-article-sort-by-author
   (gnus-thread-header h1)  (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-subject (h1 h2)
  "Sort articles by root subject."
  (string-lessp
   (downcase (gnus-simplify-subject-re (mail-header-subject h1)))
   (downcase (gnus-simplify-subject-re (mail-header-subject h2)))))

(defun gnus-thread-sort-by-subject (h1 h2)
  "Sort threads by root subject."
  (gnus-article-sort-by-subject
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-date (h1 h2)
  "Sort articles by root article date."
  (time-less-p
   (gnus-date-get-time (mail-header-date h1))
   (gnus-date-get-time (mail-header-date h2))))

(defun gnus-thread-sort-by-date (h1 h2)
  "Sort threads by root article date."
  (gnus-article-sort-by-date
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-score (h1 h2)
  "Sort articles by root article score.
Unscored articles will be counted as having a score of zero."
  (> (or (cdr (assq (mail-header-number h1)
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)
     (or (cdr (assq (mail-header-number h2)
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)))

(defun gnus-thread-sort-by-score (h1 h2)
  "Sort threads by root article score."
  (gnus-article-sort-by-score
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defun gnus-thread-sort-by-total-score (h1 h2)
  "Sort threads by the sum of all scores in the thread.
Unscored articles will be counted as having a score of zero."
  (> (gnus-thread-total-score h1) (gnus-thread-total-score h2)))

(defun gnus-thread-total-score (thread)
  ;; This function find the total score of THREAD.
  (cond ((null thread)
	 0)
	((consp thread)
	 (if (stringp (car thread))
	     (apply gnus-thread-score-function 0
		    (mapcar 'gnus-thread-total-score-1 (cdr thread)))
	   (gnus-thread-total-score-1 thread)))
	(t
	 (gnus-thread-total-score-1 (list thread)))))

(defun gnus-thread-total-score-1 (root)
  ;; This function find the total score of the thread below ROOT.
  (setq root (car root))
  (apply gnus-thread-score-function
	 (or (append
	      (mapcar 'gnus-thread-total-score
		      (cdr (gnus-id-to-thread (mail-header-id root))))
	      (when (> (mail-header-number root) 0)
		(list (or (cdr (assq (mail-header-number root)
				     gnus-newsgroup-scored))
			  gnus-summary-default-score 0))))
	     (list gnus-summary-default-score)
	     '(0))))

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-tmp-prev-subject nil)
(defvar gnus-tmp-false-parent nil)
(defvar gnus-tmp-root-expunged nil)
(defvar gnus-tmp-dummy-line nil)

(eval-when-compile (defvar gnus-tmp-header))
(defun gnus-extra-header (type &optional header)
  "Return the extra header of TYPE."
  (or (cdr (assq type (mail-header-extra (or header gnus-tmp-header))))
      ""))

(defun gnus-summary-prepare-threads (threads)
  "Prepare summary buffer from THREADS and indentation LEVEL.
THREADS is either a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...])'
or a straight list of headers."
  (gnus-message 7 "Generating summary...")

  (setq gnus-newsgroup-threads threads)
  (beginning-of-line)

  (let ((gnus-tmp-level 0)
	(default-score (or gnus-summary-default-score 0))
	(gnus-visual-p (gnus-visual-p 'summary-highlight 'highlight))
	thread number subject stack state gnus-tmp-gathered beg-match
	new-roots gnus-tmp-new-adopts thread-end
	gnus-tmp-header gnus-tmp-unread
	gnus-tmp-replied gnus-tmp-subject-or-nil
	gnus-tmp-dummy gnus-tmp-indentation gnus-tmp-lines gnus-tmp-score
	gnus-tmp-score-char gnus-tmp-from gnus-tmp-name
	gnus-tmp-number gnus-tmp-opening-bracket gnus-tmp-closing-bracket)

    (setq gnus-tmp-prev-subject nil)

    (if (vectorp (car threads))
	;; If this is a straight (sic) list of headers, then a
	;; threaded summary display isn't required, so we just create
	;; an unthreaded one.
	(gnus-summary-prepare-unthreaded threads)

      ;; Do the threaded display.

      (while (or threads stack gnus-tmp-new-adopts new-roots)

	(if (and (= gnus-tmp-level 0)
		 (or (not stack)
		     (= (caar stack) 0))
		 (not gnus-tmp-false-parent)
		 (or gnus-tmp-new-adopts new-roots))
	    (if gnus-tmp-new-adopts
		(setq gnus-tmp-level (if gnus-tmp-root-expunged 0 1)
		      thread (list (car gnus-tmp-new-adopts))
		      gnus-tmp-header (caar thread)
		      gnus-tmp-new-adopts (cdr gnus-tmp-new-adopts))
	      (when new-roots
		(setq thread (list (car new-roots))
		      gnus-tmp-header (caar thread)
		      new-roots (cdr new-roots))))

	  (if threads
	      ;; If there are some threads, we do them before the
	      ;; threads on the stack.
	      (setq thread threads
		    gnus-tmp-header (caar thread))
	    ;; There were no current threads, so we pop something off
	    ;; the stack.
	    (setq state (car stack)
		  gnus-tmp-level (car state)
		  thread (cdr state)
		  stack (cdr stack)
		  gnus-tmp-header (caar thread))))

	(setq gnus-tmp-false-parent nil)
	(setq gnus-tmp-root-expunged nil)
	(setq thread-end nil)

	(if (stringp gnus-tmp-header)
	    ;; The header is a dummy root.
	    (cond
	     ((eq gnus-summary-make-false-root 'adopt)
	      ;; We let the first article adopt the rest.
	      (setq gnus-tmp-new-adopts (nconc gnus-tmp-new-adopts
					       (cddar thread)))
	      (setq gnus-tmp-gathered
		    (nconc (mapcar
			    (lambda (h) (mail-header-number (car h)))
			    (cddar thread))
			   gnus-tmp-gathered))
	      (setq thread (cons (list (caar thread)
				       (cadar thread))
				 (cdr thread)))
	      (setq gnus-tmp-level -1
		    gnus-tmp-false-parent t))
	     ((eq gnus-summary-make-false-root 'empty)
	      ;; We print adopted articles with empty subject fields.
	      (setq gnus-tmp-gathered
		    (nconc (mapcar
			    (lambda (h) (mail-header-number (car h)))
			    (cddar thread))
			   gnus-tmp-gathered))
	      (setq gnus-tmp-level -1))
	     ((eq gnus-summary-make-false-root 'dummy)
	      ;; We remember that we probably want to output a dummy
	      ;; root.
	      (setq gnus-tmp-dummy-line gnus-tmp-header)
	      (setq gnus-tmp-prev-subject gnus-tmp-header))
	     (t
	      ;; We do not make a root for the gathered
	      ;; sub-threads at all.
	      (setq gnus-tmp-level -1)))

	  (setq number (mail-header-number gnus-tmp-header)
		subject (mail-header-subject gnus-tmp-header))

	  (cond
	   ;; If the thread has changed subject, we might want to make
	   ;; this subthread into a root.
	   ((and (null gnus-thread-ignore-subject)
		 (not (zerop gnus-tmp-level))
		 gnus-tmp-prev-subject
		 (not (inline
			(gnus-subject-equal gnus-tmp-prev-subject subject))))
	    (setq new-roots (nconc new-roots (list (car thread)))
		  thread-end t
		  gnus-tmp-header nil))
	   ;; If the article lies outside the current limit,
	   ;; then we do not display it.
	   ((not (memq number gnus-newsgroup-limit))
	    (setq gnus-tmp-gathered
		  (nconc (mapcar
			  (lambda (h) (mail-header-number (car h)))
			  (cdar thread))
			 gnus-tmp-gathered))
	    (setq gnus-tmp-new-adopts (if (cdar thread)
					  (append gnus-tmp-new-adopts
						  (cdar thread))
					gnus-tmp-new-adopts)
		  thread-end t
		  gnus-tmp-header nil)
	    (when (zerop gnus-tmp-level)
	      (setq gnus-tmp-root-expunged t)))
	   ;; Perhaps this article is to be marked as read?
	   ((and gnus-summary-mark-below
		 (< (or (cdr (assq number gnus-newsgroup-scored))
			default-score)
		    gnus-summary-mark-below)
		 ;; Don't touch sparse articles.
		 (not (gnus-summary-article-sparse-p number))
		 (not (gnus-summary-article-ancient-p number)))
	    (setq gnus-newsgroup-unreads
		  (delq number gnus-newsgroup-unreads))
	    (if gnus-newsgroup-auto-expire
		(push number gnus-newsgroup-expirable)
	      (push (cons number gnus-low-score-mark)
		    gnus-newsgroup-reads))))

	  (when gnus-tmp-header
	    ;; We may have an old dummy line to output before this
	    ;; article.
	    (when (and gnus-tmp-dummy-line
		       (gnus-subject-equal
			gnus-tmp-dummy-line
			(mail-header-subject gnus-tmp-header)))
	      (gnus-summary-insert-dummy-line
	       gnus-tmp-dummy-line (mail-header-number gnus-tmp-header))
	      (setq gnus-tmp-dummy-line nil))

	    ;; Compute the mark.
	    (setq gnus-tmp-unread (gnus-article-mark number))

	    (push (gnus-data-make number gnus-tmp-unread (1+ (point))
				  gnus-tmp-header gnus-tmp-level)
		  gnus-newsgroup-data)

	    ;; Actually insert the line.
	    (setq
	     gnus-tmp-subject-or-nil
	     (cond
	      ((and gnus-thread-ignore-subject
		    gnus-tmp-prev-subject
		    (not (inline (gnus-subject-equal
				  gnus-tmp-prev-subject subject))))
	       subject)
	      ((zerop gnus-tmp-level)
	       (if (and (eq gnus-summary-make-false-root 'empty)
			(memq number gnus-tmp-gathered)
			gnus-tmp-prev-subject
			(inline (gnus-subject-equal
				 gnus-tmp-prev-subject subject)))
		   gnus-summary-same-subject
		 subject))
	      (t gnus-summary-same-subject)))
	    (if (and (eq gnus-summary-make-false-root 'adopt)
		     (= gnus-tmp-level 1)
		     (memq number gnus-tmp-gathered))
		(setq gnus-tmp-opening-bracket ?\<
		      gnus-tmp-closing-bracket ?\>)
	      (setq gnus-tmp-opening-bracket ?\[
		    gnus-tmp-closing-bracket ?\]))
	    (setq
	     gnus-tmp-indentation
	     (aref gnus-thread-indent-array gnus-tmp-level)
	     gnus-tmp-lines (mail-header-lines gnus-tmp-header)
	     gnus-tmp-score (or (cdr (assq number gnus-newsgroup-scored))
				gnus-summary-default-score 0)
	     gnus-tmp-score-char
	     (if (or (null gnus-summary-default-score)
		     (<= (abs (- gnus-tmp-score gnus-summary-default-score))
			 gnus-summary-zcore-fuzz))
		 ?  ;Whitespace
	       (if (< gnus-tmp-score gnus-summary-default-score)
		   gnus-score-below-mark gnus-score-over-mark))
	     gnus-tmp-replied
	     (cond ((memq number gnus-newsgroup-processable)
		    gnus-process-mark)
		   ((memq number gnus-newsgroup-cached)
		    gnus-cached-mark)
		   ((memq number gnus-newsgroup-replied)
		    gnus-replied-mark)
		   ((memq number gnus-newsgroup-saved)
		    gnus-saved-mark)
		   (t gnus-unread-mark))
	     gnus-tmp-from (mail-header-from gnus-tmp-header)
	     gnus-tmp-name
	     (cond
	      ((string-match "<[^>]+> *$" gnus-tmp-from)
	       (setq beg-match (match-beginning 0))
	       (or (and (string-match "^\"[^\"]*\"" gnus-tmp-from)
			(substring gnus-tmp-from (1+ (match-beginning 0))
				   (1- (match-end 0))))
		   (substring gnus-tmp-from 0 beg-match)))
	      ((string-match "(.+)" gnus-tmp-from)
	       (substring gnus-tmp-from
			  (1+ (match-beginning 0)) (1- (match-end 0))))
	      (t gnus-tmp-from)))
	    (when (string= gnus-tmp-name "")
	      (setq gnus-tmp-name gnus-tmp-from))
	    (unless (numberp gnus-tmp-lines)
	      (setq gnus-tmp-lines 0))
	    (gnus-put-text-property
	     (point)
	     (progn (eval gnus-summary-line-format-spec) (point))
	     'gnus-number number)
	    (when gnus-visual-p
	      (forward-line -1)
	      (gnus-run-hooks 'gnus-summary-update-hook)
	      (forward-line 1))

	    (setq gnus-tmp-prev-subject subject)))

	(when (nth 1 thread)
	  (push (cons (max 0 gnus-tmp-level) (nthcdr 1 thread)) stack))
	(incf gnus-tmp-level)
	(setq threads (if thread-end nil (cdar thread)))
	(unless threads
	  (setq gnus-tmp-level 0)))))
  (gnus-message 7 "Generating summary...done"))

(defun gnus-summary-prepare-unthreaded (headers)
  "Generate an unthreaded summary buffer based on HEADERS."
  (let (header number mark)

    (beginning-of-line)

    (while headers
      ;; We may have to root out some bad articles...
      (when (memq (setq number (mail-header-number
				(setq header (pop headers))))
		  gnus-newsgroup-limit)
	;; Mark article as read when it has a low score.
	(when (and gnus-summary-mark-below
		   (< (or (cdr (assq number gnus-newsgroup-scored))
			  gnus-summary-default-score 0)
		      gnus-summary-mark-below)
		   (not (gnus-summary-article-ancient-p number)))
	  (setq gnus-newsgroup-unreads
		(delq number gnus-newsgroup-unreads))
	  (if gnus-newsgroup-auto-expire
	      (push number gnus-newsgroup-expirable)
	    (push (cons number gnus-low-score-mark)
		  gnus-newsgroup-reads)))

	(setq mark (gnus-article-mark number))
	(push (gnus-data-make number mark (1+ (point)) header 0)
	      gnus-newsgroup-data)
	(gnus-summary-insert-line
	 header 0 number
	 mark (memq number gnus-newsgroup-replied)
	 (memq number gnus-newsgroup-expirable)
	 (mail-header-subject header) nil
	 (cdr (assq number gnus-newsgroup-scored))
	 (memq number gnus-newsgroup-processable))))))

(defun gnus-summary-remove-list-identifiers ()
  "Remove list identifiers in `gnus-list-identifiers' from articles in the current group."
  (let ((regexp (if (stringp gnus-list-identifiers)
		    gnus-list-identifiers
		  (mapconcat 'identity gnus-list-identifiers " *\\|"))))
    (dolist (header gnus-newsgroup-headers)
      (when (string-match (concat "\\(\\(\\(Re: +\\)?\\(" regexp 
				  " *\\)\\)+\\(Re: +\\)?\\)")
			  (mail-header-subject header))
	(mail-header-set-subject
	 header (concat (substring (mail-header-subject header)
				   0 (match-beginning 1))
			(or
			 (match-string 3 (mail-header-subject header))
			 (match-string 5 (mail-header-subject header)))
			(substring (mail-header-subject header)
				   (match-end 1))))))))

(defun gnus-select-newsgroup (group &optional read-all select-articles)
  "Select newsgroup GROUP.
If READ-ALL is non-nil, all articles in the group are selected.
If SELECT-ARTICLES, only select those articles from GROUP."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 ;;!!! Dirty hack; should be removed.
	 (gnus-summary-ignore-duplicates
 	  (if (eq (car (gnus-find-method-for-group group)) 'nnvirtual)
	      t
	    gnus-summary-ignore-duplicates))
	 (info (nth 2 entry))
	 articles fetched-articles cached)

    (unless (gnus-check-server
	     (set (make-local-variable 'gnus-current-select-method)
		  (gnus-find-method-for-group group)))
      (error "Couldn't open server"))

    (or (and entry (not (eq (car entry) t))) ; Either it's active...
	(gnus-activate-group group)	; Or we can activate it...
	(progn				; Or we bug out.
	  (when (equal major-mode 'gnus-summary-mode)
	    (kill-buffer (current-buffer)))
	  (error "Couldn't request group %s: %s"
		 group (gnus-status-message group))))

    (unless (gnus-request-group group t)
      (when (equal major-mode 'gnus-summary-mode)
	(kill-buffer (current-buffer)))
      (error "Couldn't request group %s: %s"
	     group (gnus-status-message group)))

    (setq gnus-newsgroup-name group)
    (setq gnus-newsgroup-unselected nil)
    (setq gnus-newsgroup-unreads (gnus-list-of-unread-articles group))
    (gnus-summary-setup-default-charset)

    ;; Adjust and set lists of article marks.
    (when info
      (gnus-adjust-marked-articles info))

    ;; Kludge to avoid having cached articles nixed out in virtual groups.
    (when (gnus-virtual-group-p group)
      (setq cached gnus-newsgroup-cached))

    (setq gnus-newsgroup-unreads
	  (gnus-set-difference
	   (gnus-set-difference gnus-newsgroup-unreads gnus-newsgroup-marked)
	   gnus-newsgroup-dormant))

    (setq gnus-newsgroup-processable nil)

    (gnus-update-read-articles group gnus-newsgroup-unreads)

    (if (setq articles select-articles)
	(setq gnus-newsgroup-unselected
	      (gnus-sorted-intersection
	       gnus-newsgroup-unreads
	       (gnus-sorted-complement gnus-newsgroup-unreads articles)))
      (setq articles (gnus-articles-to-read group read-all)))

    (cond
     ((null articles)
      ;;(gnus-message 3 "Couldn't select newsgroup -- no articles to display")
      'quit)
     ((eq articles 0) nil)
     (t
      ;; Init the dependencies hash table.
      (setq gnus-newsgroup-dependencies
	    (gnus-make-hashtable (length articles)))
      (gnus-set-global-variables)
      ;; Retrieve the headers and read them in.
      (gnus-message 5 "Fetching headers for %s..." gnus-newsgroup-name)
      (setq gnus-newsgroup-headers
	    (if (eq 'nov
		    (setq gnus-headers-retrieved-by
			  (gnus-retrieve-headers
			   articles gnus-newsgroup-name
			   ;; We might want to fetch old headers, but
			   ;; not if there is only 1 article.
			   (and (or (and
				     (not (eq gnus-fetch-old-headers 'some))
				     (not (numberp gnus-fetch-old-headers)))
				    (> (length articles) 1))
				gnus-fetch-old-headers))))
		(gnus-get-newsgroup-headers-xover
		 articles nil nil gnus-newsgroup-name t)
	      (gnus-get-newsgroup-headers)))
      (gnus-message 5 "Fetching headers for %s...done" gnus-newsgroup-name)

      ;; Kludge to avoid having cached articles nixed out in virtual groups.
      (when cached
	(setq gnus-newsgroup-cached cached))

      ;; Suppress duplicates?
      (when gnus-suppress-duplicates
	(gnus-dup-suppress-articles))

      ;; Set the initial limit.
      (setq gnus-newsgroup-limit (copy-sequence articles))
      ;; Remove canceled articles from the list of unread articles.
      (setq gnus-newsgroup-unreads
	    (gnus-set-sorted-intersection
	     gnus-newsgroup-unreads
	     (setq fetched-articles
		   (mapcar (lambda (headers) (mail-header-number headers))
			   gnus-newsgroup-headers))))
      ;; Removed marked articles that do not exist.
      (gnus-update-missing-marks
       (gnus-sorted-complement fetched-articles articles))
      ;; We might want to build some more threads first.
      (when (and gnus-fetch-old-headers
		 (eq gnus-headers-retrieved-by 'nov))
	(if (eq gnus-fetch-old-headers 'invisible)
	    (gnus-build-all-threads)
	  (gnus-build-old-threads)))
      ;; Let the Gnus agent mark articles as read.
      (when gnus-agent
	(gnus-agent-get-undownloaded-list))
      ;; Remove list identifiers from subject
      (when gnus-list-identifiers
	(gnus-summary-remove-list-identifiers))
      ;; Check whether auto-expire is to be done in this group.
      (setq gnus-newsgroup-auto-expire
	    (gnus-group-auto-expirable-p group))
      ;; Set up the article buffer now, if necessary.
      (unless gnus-single-article-buffer
	(gnus-article-setup-buffer))
      ;; First and last article in this newsgroup.
      (when gnus-newsgroup-headers
	(setq gnus-newsgroup-begin
	      (mail-header-number (car gnus-newsgroup-headers))
	      gnus-newsgroup-end
	      (mail-header-number
	       (gnus-last-element gnus-newsgroup-headers))))
      ;; GROUP is successfully selected.
      (or gnus-newsgroup-headers t)))))

(defun gnus-articles-to-read (group &optional read-all)
  "Find out what articles the user wants to read."
  (let* ((articles
	  ;; Select all articles if `read-all' is non-nil, or if there
	  ;; are no unread articles.
	  (if (or read-all
		  (and (zerop (length gnus-newsgroup-marked))
		       (zerop (length gnus-newsgroup-unreads)))
		  (eq (gnus-group-find-parameter group 'display)
		      'all))
	      (or
	       (gnus-uncompress-range (gnus-active group))
	       (gnus-cache-articles-in-group group))
	    (sort (append gnus-newsgroup-dormant gnus-newsgroup-marked
			  (copy-sequence gnus-newsgroup-unreads))
		  '<)))
	 (scored-list (gnus-killed-articles gnus-newsgroup-killed articles))
	 (scored (length scored-list))
	 (number (length articles))
	 (marked (+ (length gnus-newsgroup-marked)
		    (length gnus-newsgroup-dormant)))
	 (select
	  (cond
	   ((numberp read-all)
	    read-all)
	   (t
	    (condition-case ()
		(cond
		 ((and (or (<= scored marked) (= scored number))
		       (numberp gnus-large-newsgroup)
		       (> number gnus-large-newsgroup))
		  (let ((input
			 (read-string
			  (format
			   "How many articles from %s (default %d): "
			   (gnus-limit-string gnus-newsgroup-name 35)
			   number))))
		    (if (string-match "^[ \t]*$" input) number input)))
		 ((and (> scored marked) (< scored number)
		       (> (- scored number) 20))
		  (let ((input
			 (read-string
			  (format "%s %s (%d scored, %d total): "
				  "How many articles from"
				  group scored number))))
		    (if (string-match "^[ \t]*$" input)
			number input)))
		 (t number))
	      (quit
	       (message "Quit getting the articles to read")
	       nil))))))
    (setq select (if (stringp select) (string-to-number select) select))
    (if (or (null select) (zerop select))
	select
      (if (and (not (zerop scored)) (<= (abs select) scored))
	  (progn
	    (setq articles (sort scored-list '<))
	    (setq number (length articles)))
	(setq articles (copy-sequence articles)))

      (when (< (abs select) number)
	(if (< select 0)
	    ;; Select the N oldest articles.
	    (setcdr (nthcdr (1- (abs select)) articles) nil)
	  ;; Select the N most recent articles.
	  (setq articles (nthcdr (- number select) articles))))
      (setq gnus-newsgroup-unselected
	    (gnus-sorted-intersection
	     gnus-newsgroup-unreads
	     (gnus-sorted-complement gnus-newsgroup-unreads articles)))
      (when gnus-alter-articles-to-read-function
	(setq gnus-newsgroup-unreads
	      (sort 
	       (funcall gnus-alter-articles-to-read-function
			gnus-newsgroup-name gnus-newsgroup-unreads)
	       '<)))
      articles)))

(defun gnus-killed-articles (killed articles)
  (let (out)
    (while articles
      (when (inline (gnus-member-of-range (car articles) killed))
	(push (car articles) out))
      (setq articles (cdr articles)))
    out))

(defun gnus-uncompress-marks (marks)
  "Uncompress the mark ranges in MARKS."
  (let ((uncompressed '(score bookmark))
	out)
    (while marks
      (if (memq (caar marks) uncompressed)
	  (push (car marks) out)
	(push (cons (caar marks) (gnus-uncompress-range (cdar marks))) out))
      (setq marks (cdr marks)))
    out))

(defun gnus-adjust-marked-articles (info)
  "Set all article lists and remove all marks that are no longer valid."
  (let* ((marked-lists (gnus-info-marks info))
	 (active (gnus-active (gnus-info-group info)))
	 (min (car active))
	 (max (cdr active))
	 (types gnus-article-mark-lists)
	 (uncompressed '(score bookmark killed))
	 marks var articles article mark)

    (while marked-lists
      (setq marks (pop marked-lists))
      (set (setq var (intern (format "gnus-newsgroup-%s"
				     (car (rassq (setq mark (car marks))
						 types)))))
	   (if (memq (car marks) uncompressed) (cdr marks)
	     (gnus-uncompress-range (cdr marks))))

      (setq articles (symbol-value var))

      ;; All articles have to be subsets of the active articles.
      (cond
       ;; Adjust "simple" lists.
       ((memq mark '(tick dormant expire reply save))
	(while articles
	  (when (or (< (setq article (pop articles)) min) (> article max))
	    (set var (delq article (symbol-value var))))))
       ;; Adjust assocs.
       ((memq mark uncompressed)
	(when (not (listp (cdr (symbol-value var))))
	  (set var (list (symbol-value var))))
	(when (not (listp (cdr articles)))
	  (setq articles (list articles)))
	(while articles
	  (when (or (not (consp (setq article (pop articles))))
		    (< (car article) min)
		    (> (car article) max))
	    (set var (delq article (symbol-value var))))))))))

(defun gnus-update-missing-marks (missing)
  "Go through the list of MISSING articles and remove them from the mark lists."
  (when missing
    (let ((types gnus-article-mark-lists)
	  var m)
      ;; Go through all types.
      (while types
	(setq var (intern (format "gnus-newsgroup-%s" (car (pop types)))))
	(when (symbol-value var)
	  ;; This list has articles.  So we delete all missing articles
	  ;; from it.
	  (setq m missing)
	  (while m
	    (set var (delq (pop m) (symbol-value var)))))))))

(defun gnus-update-marks ()
  "Enter the various lists of marked articles into the newsgroup info list."
  (let ((types gnus-article-mark-lists)
	(info (gnus-get-info gnus-newsgroup-name))
	(uncompressed '(score bookmark killed))
	type list newmarked symbol delta-marks)
    (when info
      ;; Add all marks lists to the list of marks lists.
      (while (setq type (pop types))
	(setq list (symbol-value
		    (setq symbol
			  (intern (format "gnus-newsgroup-%s"
					  (car type))))))

	(when list
	  ;; Get rid of the entries of the articles that have the
	  ;; default score.
	  (when (and (eq (cdr type) 'score)
		     gnus-save-score
		     list)
	    (let* ((arts list)
		   (prev (cons nil list))
		   (all prev))
	      (while arts
		(if (or (not (consp (car arts)))
			(= (cdar arts) gnus-summary-default-score))
		    (setcdr prev (cdr arts))
		  (setq prev arts))
		(setq arts (cdr arts)))
	      (setq list (cdr all)))))

	(unless (memq (cdr type) uncompressed)
	  (setq list (gnus-compress-sequence (set symbol (sort list '<)) t)))
       
	(when (gnus-check-backend-function
	       'request-set-mark gnus-newsgroup-name)
	  ;; propagate flags to server, with the following exceptions:
	  ;; uncompressed:s are not proper flags (they are cons cells)
	  ;; cache is a internal gnus flag
	  ;; download are local to one gnus installation (well)
	  ;; unsend are for nndraft groups only
	  ;; xxx: generality of this?  this suits nnimap anyway
	  (unless (memq (cdr type) (append '(cache download unsend)
					   uncompressed))
	    (let* ((old (cdr (assq (cdr type) (gnus-info-marks info))))
		   (del (gnus-remove-from-range (gnus-copy-sequence old) list))
		   (add (gnus-remove-from-range
			 (gnus-copy-sequence list) old)))
	      (when add
		(push (list add 'add (list (cdr type))) delta-marks))
	      (when del
		(push (list del 'del (list (cdr type))) delta-marks)))))
	  
	(when list
	  (push (cons (cdr type) list) newmarked)))

      (when delta-marks
	(unless (gnus-check-group gnus-newsgroup-name)
	  (error "Can't open server for %s" gnus-newsgroup-name))
	(gnus-request-set-mark gnus-newsgroup-name delta-marks))
	  
      ;; Enter these new marks into the info of the group.
      (if (nthcdr 3 info)
	  (setcar (nthcdr 3 info) newmarked)
	;; Add the marks lists to the end of the info.
	(when newmarked
	  (setcdr (nthcdr 2 info) (list newmarked))))

      ;; Cut off the end of the info if there's nothing else there.
      (let ((i 5))
	(while (and (> i 2)
		    (not (nth i info)))
	  (when (nthcdr (decf i) info)
	    (setcdr (nthcdr i info) nil)))))))

(defun gnus-set-mode-line (where)
  "Set the mode line of the article or summary buffers.
If WHERE is `summary', the summary mode line format will be used."
  ;; Is this mode line one we keep updated?
  (when (and (memq where gnus-updated-mode-lines)
	     (symbol-value
	      (intern (format "gnus-%s-mode-line-format-spec" where))))
    (let (mode-string)
      (save-excursion
	;; We evaluate this in the summary buffer since these
	;; variables are buffer-local to that buffer.
	(set-buffer gnus-summary-buffer)
	;; We bind all these variables that are used in the `eval' form
	;; below.
	(let* ((mformat (symbol-value
			 (intern
			  (format "gnus-%s-mode-line-format-spec" where))))
	       (gnus-tmp-group-name (gnus-group-name-decode 
				     gnus-newsgroup-name
				     (gnus-group-name-charset 
				      nil
				      gnus-newsgroup-name)))
	       (gnus-tmp-article-number (or gnus-current-article 0))
	       (gnus-tmp-unread gnus-newsgroup-unreads)
	       (gnus-tmp-unread-and-unticked (length gnus-newsgroup-unreads))
	       (gnus-tmp-unselected (length gnus-newsgroup-unselected))
	       (gnus-tmp-unread-and-unselected
		(cond ((and (zerop gnus-tmp-unread-and-unticked)
			    (zerop gnus-tmp-unselected))
		       "")
		      ((zerop gnus-tmp-unselected)
		       (format "{%d more}" gnus-tmp-unread-and-unticked))
		      (t (format "{%d(+%d) more}"
				 gnus-tmp-unread-and-unticked
				 gnus-tmp-unselected))))
	       (gnus-tmp-subject
		(if (and gnus-current-headers
			 (vectorp gnus-current-headers))
		    (gnus-mode-string-quote
		     (mail-header-subject gnus-current-headers))
		  ""))
	       bufname-length max-len
	       gnus-tmp-header);; passed as argument to any user-format-funcs
	  (setq mode-string (eval mformat))
	  (setq bufname-length (if (string-match "%b" mode-string)
				   (- (length
				       (buffer-name
					(if (eq where 'summary)
					    nil
					  (get-buffer gnus-article-buffer))))
				      2)
				 0))
	  (setq max-len (max 4 (if gnus-mode-non-string-length
				   (- (window-width)
				      gnus-mode-non-string-length
				      bufname-length)
				 (length mode-string))))
	  ;; We might have to chop a bit of the string off...
	  (when (> (length mode-string) max-len)
	    (setq mode-string
		  (concat (truncate-string-to-width mode-string (- max-len 3))
			  "...")))
	  ;; Pad the mode string a bit.
	  (setq mode-string (format (format "%%-%ds" max-len) mode-string))))
      ;; Update the mode line.
      (setq mode-line-buffer-identification
	    (gnus-mode-line-buffer-identification (list mode-string)))
      (set-buffer-modified-p t))))

(defun gnus-create-xref-hashtb (from-newsgroup headers unreads)
  "Go through the HEADERS list and add all Xrefs to a hash table.
The resulting hash table is returned, or nil if no Xrefs were found."
  (let* ((virtual (gnus-virtual-group-p from-newsgroup))
	 (prefix (if virtual "" (gnus-group-real-prefix from-newsgroup)))
	 (xref-hashtb (gnus-make-hashtable))
	 start group entry number xrefs header)
    (while headers
      (setq header (pop headers))
      (when (and (setq xrefs (mail-header-xref header))
		 (not (memq (setq number (mail-header-number header))
			    unreads)))
	(setq start 0)
	(while (string-match "\\([^ ]+\\)[:/]\\([0-9]+\\)" xrefs start)
	  (setq start (match-end 0))
	  (setq group (if prefix
			  (concat prefix (substring xrefs (match-beginning 1)
						    (match-end 1)))
			(substring xrefs (match-beginning 1) (match-end 1))))
	  (setq number
		(string-to-int (substring xrefs (match-beginning 2)
					  (match-end 2))))
	  (if (setq entry (gnus-gethash group xref-hashtb))
	      (setcdr entry (cons number (cdr entry)))
	    (gnus-sethash group (cons number nil) xref-hashtb)))))
    (and start xref-hashtb)))

(defun gnus-mark-xrefs-as-read (from-newsgroup headers unreads)
  "Look through all the headers and mark the Xrefs as read."
  (let ((virtual (gnus-virtual-group-p from-newsgroup))
	name entry info xref-hashtb idlist method nth4)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (when (setq xref-hashtb
		  (gnus-create-xref-hashtb from-newsgroup headers unreads))
	(mapatoms
	 (lambda (group)
	   (unless (string= from-newsgroup (setq name (symbol-name group)))
	     (setq idlist (symbol-value group))
	     ;; Dead groups are not updated.
	     (and (prog1
		      (setq entry (gnus-gethash name gnus-newsrc-hashtb)
			    info (nth 2 entry))
		    (when (stringp (setq nth4 (gnus-info-method info)))
		      (setq nth4 (gnus-server-to-method nth4))))
		  ;; Only do the xrefs if the group has the same
		  ;; select method as the group we have just read.
		  (or (gnus-methods-equal-p
		       nth4 (gnus-find-method-for-group from-newsgroup))
		      virtual
		      (equal nth4 (setq method (gnus-find-method-for-group
						from-newsgroup)))
		      (and (equal (car nth4) (car method))
			   (equal (nth 1 nth4) (nth 1 method))))
		  gnus-use-cross-reference
		  (or (not (eq gnus-use-cross-reference t))
		      virtual
		      ;; Only do cross-references on subscribed
		      ;; groups, if that is what is wanted.
		      (<= (gnus-info-level info) gnus-level-subscribed))
		  (gnus-group-make-articles-read name idlist))))
	 xref-hashtb)))))

(defun gnus-compute-read-articles (group articles)
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (active (gnus-active group))
	 ninfo)
    (when entry
      ;; First peel off all invalid article numbers.
      (when active
	(let ((ids articles)
	      id first)
	  (while (setq id (pop ids))
	    (when (and first (> id (cdr active)))
	      ;; We'll end up in this situation in one particular
	      ;; obscure situation.  If you re-scan a group and get
	      ;; a new article that is cross-posted to a different
	      ;; group that has not been re-scanned, you might get
	      ;; crossposted article that has a higher number than
	      ;; Gnus believes possible.  So we re-activate this
	      ;; group as well.  This might mean doing the
	      ;; crossposting thingy will *increase* the number
	      ;; of articles in some groups.  Tsk, tsk.
	      (setq active (or (gnus-activate-group group) active)))
	    (when (or (> id (cdr active))
		      (< id (car active)))
	      (setq articles (delq id articles))))))
      ;; If the read list is nil, we init it.
      (if (and active
	       (null (gnus-info-read info))
	       (> (car active) 1))
	  (setq ninfo (cons 1 (1- (car active))))
	(setq ninfo (gnus-info-read info)))
      ;; Then we add the read articles to the range.
      (gnus-add-to-range
       ninfo (setq articles (sort articles '<))))))

(defun gnus-group-make-articles-read (group articles)
  "Update the info of GROUP to say that ARTICLES are read."
  (let* ((num 0)
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (active (gnus-active group))
	 range)
    (when entry
      (setq range (gnus-compute-read-articles group articles))
      (save-excursion
	(set-buffer gnus-group-buffer)
	(gnus-undo-register
	  `(progn
	     (gnus-info-set-marks ',info ',(gnus-info-marks info) t)
	     (gnus-info-set-read ',info ',(gnus-info-read info))
	     (gnus-get-unread-articles-in-group ',info (gnus-active ,group))
	     (gnus-group-update-group ,group t))))
      ;; Add the read articles to the range.
      (gnus-info-set-read info range)
      ;; Then we have to re-compute how many unread
      ;; articles there are in this group.
      (when active
	(cond
	 ((not range)
	  (setq num (- (1+ (cdr active)) (car active))))
	 ((not (listp (cdr range)))
	  (setq num (- (cdr active) (- (1+ (cdr range))
				       (car range)))))
	 (t
	  (while range
	    (if (numberp (car range))
		(setq num (1+ num))
	      (setq num (+ num (- (1+ (cdar range)) (caar range)))))
	    (setq range (cdr range)))
	  (setq num (- (cdr active) num))))
	;; Update the number of unread articles.
	(setcar entry num)
	;; Update the group buffer.
	(gnus-group-update-group group t)))))

(defvar gnus-newsgroup-none-id 0)

(defun gnus-get-newsgroup-headers (&optional dependencies force-new)
  (let ((cur nntp-server-buffer)
	(dependencies
	 (or dependencies
	     (save-excursion (set-buffer gnus-summary-buffer)
			     gnus-newsgroup-dependencies)))
	headers id end ref
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-excursion (condition-case nil
			     (set-buffer gnus-summary-buffer)
			   (error))
			 gnus-newsgroup-ignored-charsets)))
    (save-excursion
      (set-buffer nntp-server-buffer)
      ;; Translate all TAB characters into SPACE characters.
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (subst-char-in-region (point-min) (point-max) ?\r ?  t)
      (gnus-run-hooks 'gnus-parse-headers-hook)
      (let ((case-fold-search t)
	    in-reply-to header p lines chars)
	(goto-char (point-min))
	;; Search to the beginning of the next header.	Error messages
	;; do not begin with 2 or 3.
	(while (re-search-forward "^[23][0-9]+ " nil t)
	  (setq id nil
		ref nil)
	  ;; This implementation of this function, with nine
	  ;; search-forwards instead of the one re-search-forward and
	  ;; a case (which basically was the old function) is actually
	  ;; about twice as fast, even though it looks messier.	 You
	  ;; can't have everything, I guess.  Speed and elegance
	  ;; doesn't always go hand in hand.
	  (setq
	   header
	   (vector
	    ;; Number.
	    (prog1
		(read cur)
	      (end-of-line)
	      (setq p (point))
	      (narrow-to-region (point)
				(or (and (search-forward "\n.\n" nil t)
					 (- (point) 2))
				    (point))))
	    ;; Subject.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nsubject: " nil t)
		  (funcall gnus-decode-encoded-word-function
			   (nnheader-header-value))
		"(none)"))
	    ;; From.
	    (progn
	      (goto-char p)
	      (if (or (search-forward "\nfrom: " nil t)
		      (search-forward "\nfrom:" nil t))
		  (funcall gnus-decode-encoded-word-function
			   (nnheader-header-value))
		"(nobody)"))
	    ;; Date.
	    (progn
	      (goto-char p)
	      (if (search-forward "\ndate: " nil t)
		  (nnheader-header-value) ""))
	    ;; Message-ID.
	    (progn
	      (goto-char p)
	      (setq id (if (re-search-forward
			    "^message-id: *\\(<[^\n\t> ]+>\\)" nil t)
			   ;; We do it this way to make sure the Message-ID
			   ;; is (somewhat) syntactically valid.
			   (buffer-substring (match-beginning 1)
					     (match-end 1))
			 ;; If there was no message-id, we just fake one
			 ;; to make subsequent routines simpler.
			 (nnheader-generate-fake-message-id))))
	    ;; References.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nreferences: " nil t)
		  (progn
		    (setq end (point))
		    (prog1
			(nnheader-header-value)
		      (setq ref
			    (buffer-substring
			     (progn
			       (end-of-line)
			       (search-backward ">" end t)
			       (1+ (point)))
			     (progn
			       (search-backward "<" end t)
			       (point))))))
		;; Get the references from the in-reply-to header if there
		;; were no references and the in-reply-to header looks
		;; promising.
		(if (and (search-forward "\nin-reply-to: " nil t)
			 (setq in-reply-to (nnheader-header-value))
			 (string-match "<[^>]+>" in-reply-to))
		    (let (ref2)
		      (setq ref (substring in-reply-to (match-beginning 0)
					   (match-end 0)))
		      (while (string-match "<[^>]+>" in-reply-to (match-end 0))
			(setq ref2 (substring in-reply-to (match-beginning 0)
					      (match-end 0)))
			(when (> (length ref2) (length ref))
			  (setq ref ref2)))
		      ref)
		  (setq ref nil))))
	    ;; Chars.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nchars: " nil t)
		  (if (numberp (setq chars (ignore-errors (read cur))))
		      chars 0)
		0))
	    ;; Lines.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nlines: " nil t)
		  (if (numberp (setq lines (ignore-errors (read cur))))
		      lines 0)
		0))
	    ;; Xref.
	    (progn
	      (goto-char p)
	      (and (search-forward "\nxref: " nil t)
		   (nnheader-header-value)))
	    ;; Extra.
	    (when gnus-extra-headers
	      (let ((extra gnus-extra-headers)
		    out)
		(while extra
		  (goto-char p)
		  (when (search-forward
			 (concat "\n" (symbol-name (car extra)) ": ") nil t)
		    (push (cons (car extra) (nnheader-header-value))
			  out))
		  (pop extra))
		out))))
	  (when (equal id ref)
	    (setq ref nil))

	  (when gnus-alter-header-function
	    (funcall gnus-alter-header-function header)
	    (setq id (mail-header-id header)
		  ref (gnus-parent-id (mail-header-references header))))

	  (when (setq header
		      (gnus-dependencies-add-header
		       header dependencies force-new))
	    (push header headers))
	  (goto-char (point-max))
	  (widen))
	(nreverse headers)))))

;; Goes through the xover lines and returns a list of vectors
(defun gnus-get-newsgroup-headers-xover (sequence &optional
						  force-new dependencies
						  group also-fetch-heads)
  "Parse the news overview data in the server buffer.
Return a list of headers that match SEQUENCE (see
`nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets)
	(cur nntp-server-buffer)
	(dependencies (or dependencies gnus-newsgroup-dependencies))
	number headers header)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (subst-char-in-region (point-min) (point-max) ?\r ?  t)
      ;; Allow the user to mangle the headers before parsing them.
      (gnus-run-hooks 'gnus-parse-headers-hook)
      (goto-char (point-min))
      (while (not (eobp))
	(condition-case ()
	    (while (and sequence (not (eobp)))
	      (setq number (read cur))
	      (while (and sequence
			  (< (car sequence) number))
		(setq sequence (cdr sequence)))
	      (and sequence
		   (eq number (car sequence))
		   (progn
		     (setq sequence (cdr sequence))
		     (setq header (inline
				    (gnus-nov-parse-line
				     number dependencies force-new))))
		   (push header headers))
	      (forward-line 1))
	  (error
	   (gnus-error 4 "Strange nov line (%d)"
		       (count-lines (point-min) (point)))))
	(forward-line 1))
      ;; A common bug in inn is that if you have posted an article and
      ;; then retrieves the active file, it will answer correctly --
      ;; the new article is included.  However, a NOV entry for the
      ;; article may not have been generated yet, so this may fail.
      ;; We work around this problem by retrieving the last few
      ;; headers using HEAD.
      (if (or (not also-fetch-heads)
	      (not sequence))
	  ;; We (probably) got all the headers.
	  (nreverse headers)
	(let ((gnus-nov-is-evil t))
	  (nconc
	   (nreverse headers)
	   (when (gnus-retrieve-headers sequence group)
	     (gnus-get-newsgroup-headers))))))))

(defun gnus-article-get-xrefs ()
  "Fill in the Xref value in `gnus-current-headers', if necessary.
This is meant to be called in `gnus-article-internal-prepare-hook'."
  (let ((headers (save-excursion (set-buffer gnus-summary-buffer)
				 gnus-current-headers)))
    (or (not gnus-use-cross-reference)
	(not headers)
	(and (mail-header-xref headers)
	     (not (string= (mail-header-xref headers) "")))
	(let ((case-fold-search t)
	      xref)
	  (save-restriction
	    (nnheader-narrow-to-headers)
	    (goto-char (point-min))
	    (when (or (and (not (eobp))
			   (eq (downcase (char-after)) ?x)
			   (looking-at "Xref:"))
		      (search-forward "\nXref:" nil t))
	      (goto-char (1+ (match-end 0)))
	      (setq xref (buffer-substring (point)
					   (progn (end-of-line) (point))))
	      (mail-header-set-xref headers xref)))))))

(defun gnus-summary-insert-subject (id &optional old-header use-old-header)
  "Find article ID and insert the summary line for that article.
OLD-HEADER can either be a header or a line number to insert
the subject line on."
  (let* ((line (and (numberp old-header) old-header))
	 (old-header (and (vectorp old-header) old-header))
	 (header (cond ((and old-header use-old-header)
			old-header)
		       ((and (numberp id)
			     (gnus-number-to-header id))
			(gnus-number-to-header id))
		       (t
			(gnus-read-header id))))
	 (number (and (numberp id) id))
	 d)
    (when header
      ;; Rebuild the thread that this article is part of and go to the
      ;; article we have fetched.
      (when (and (not gnus-show-threads)
		 old-header)
	(when (and number
		   (setq d (gnus-data-find (mail-header-number old-header))))
	  (goto-char (gnus-data-pos d))
	  (gnus-data-remove
	   number
	   (- (gnus-point-at-bol)
	      (prog1
		  (1+ (gnus-point-at-eol))
		(gnus-delete-line))))))
      (when old-header
	(mail-header-set-number header (mail-header-number old-header)))
      (setq gnus-newsgroup-sparse
	    (delq (setq number (mail-header-number header))
		  gnus-newsgroup-sparse))
      (setq gnus-newsgroup-ancient (delq number gnus-newsgroup-ancient))
      (push number gnus-newsgroup-limit)
      (gnus-rebuild-thread (mail-header-id header) line)
      (gnus-summary-goto-subject number nil t))
    (when (and (numberp number)
	       (> number 0))
      ;; We have to update the boundaries even if we can't fetch the
      ;; article if ID is a number -- so that the next `P' or `N'
      ;; command will fetch the previous (or next) article even
      ;; if the one we tried to fetch this time has been canceled.
      (when (> number gnus-newsgroup-end)
	(setq gnus-newsgroup-end number))
      (when (< number gnus-newsgroup-begin)
	(setq gnus-newsgroup-begin number))
      (setq gnus-newsgroup-unselected
	    (delq number gnus-newsgroup-unselected)))
    ;; Report back a success?
    (and header (mail-header-number header))))

;;; Process/prefix in the summary buffer

(defun gnus-summary-work-articles (n)
  "Return a list of articles to be worked upon.
The prefix argument, the list of process marked articles, and the
current article will be taken into consideration."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (cond
     (n
      ;; A numerical prefix has been given.
      (setq n (prefix-numeric-value n))
      (let ((backward (< n 0))
	    (n (abs (prefix-numeric-value n)))
	    articles article)
	(save-excursion
	  (while
	      (and (> n 0)
		   (push (setq article (gnus-summary-article-number))
			 articles)
		   (if backward
		       (gnus-summary-find-prev nil article)
		     (gnus-summary-find-next nil article)))
	    (decf n)))
	(nreverse articles)))
     ((and (gnus-region-active-p) (mark))
      (message "region active")
      ;; Work on the region between point and mark.
      (let ((max (max (point) (mark)))
	    articles article)
	(save-excursion
	  (goto-char (min (min (point) (mark))))
	  (while
	      (and
	       (push (setq article (gnus-summary-article-number)) articles)
	       (gnus-summary-find-next nil article)
	       (< (point) max)))
	  (nreverse articles))))
     (gnus-newsgroup-processable
      ;; There are process-marked articles present.
      ;; Save current state.
      (gnus-summary-save-process-mark)
      ;; Return the list.
      (reverse gnus-newsgroup-processable))
     (t
      ;; Just return the current article.
      (list (gnus-summary-article-number))))))

(defmacro gnus-summary-iterate (arg &rest forms)
  "Iterate over the process/prefixed articles and do FORMS.
ARG is the interactive prefix given to the command.  FORMS will be
executed with point over the summary line of the articles."
  (let ((articles (make-symbol "gnus-summary-iterate-articles")))
    `(let ((,articles (gnus-summary-work-articles ,arg)))
       (while ,articles
	 (gnus-summary-goto-subject (car ,articles))
	 ,@forms
	 (pop ,articles)))))

(put 'gnus-summary-iterate 'lisp-indent-function 1)
(put 'gnus-summary-iterate 'edebug-form-spec '(form body))

(defun gnus-summary-save-process-mark ()
  "Push the current set of process marked articles on the stack."
  (interactive)
  (push (copy-sequence gnus-newsgroup-processable)
	gnus-newsgroup-process-stack))

(defun gnus-summary-kill-process-mark ()
  "Push the current set of process marked articles on the stack and unmark."
  (interactive)
  (gnus-summary-save-process-mark)
  (gnus-summary-unmark-all-processable))

(defun gnus-summary-yank-process-mark ()
  "Pop the last process mark state off the stack and restore it."
  (interactive)
  (unless gnus-newsgroup-process-stack
    (error "Empty mark stack"))
  (gnus-summary-process-mark-set (pop gnus-newsgroup-process-stack)))

(defun gnus-summary-process-mark-set (set)
  "Make SET into the current process marked articles."
  (gnus-summary-unmark-all-processable)
  (while set
    (gnus-summary-set-process-mark (pop set))))

;;; Searching and stuff

(defun gnus-summary-search-group (&optional backward use-level)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (when (gnus-group-search-forward
	   backward nil (if use-level (gnus-group-group-level) nil))
      (gnus-group-group-name))))

(defun gnus-summary-best-group (&optional exclude-group)
  "Find the name of the best unread group.
If EXCLUDE-GROUP, do not go to this group."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (save-excursion
      (gnus-group-best-unread-group exclude-group))))

(defun gnus-summary-find-next (&optional unread article backward undownloaded)
  (if backward (gnus-summary-find-prev)
    (let* ((dummy (gnus-summary-article-intangible-p))
	   (article (or article (gnus-summary-article-number)))
	   (arts (gnus-data-find-list article))
	   result)
      (when (and (not dummy)
		 (or (not gnus-summary-check-current)
		     (not unread)
		     (not (gnus-data-unread-p (car arts)))))
	(setq arts (cdr arts)))
      (when (setq result
		  (if unread
		      (progn
			(while arts
			  (when (or (and undownloaded
					 (eq gnus-undownloaded-mark
					     (gnus-data-mark (car arts))))
				    (gnus-data-unread-p (car arts)))
			    (setq result (car arts)
				  arts nil))
			  (setq arts (cdr arts)))
			result)
		    (car arts)))
	(goto-char (gnus-data-pos result))
	(gnus-data-number result)))))

(defun gnus-summary-find-prev (&optional unread article)
  (let* ((eobp (eobp))
	 (article (or article (gnus-summary-article-number)))
	 (arts (gnus-data-find-list article (gnus-data-list 'rev)))
	 result)
    (when (and (not eobp)
	       (or (not gnus-summary-check-current)
		   (not unread)
		   (not (gnus-data-unread-p (car arts)))))
      (setq arts (cdr arts)))
    (when (setq result
		(if unread
		    (progn
		      (while arts
			(when (gnus-data-unread-p (car arts))
			  (setq result (car arts)
				arts nil))
			(setq arts (cdr arts)))
		      result)
		  (car arts)))
      (goto-char (gnus-data-pos result))
      (gnus-data-number result))))

(defun gnus-summary-find-subject (subject &optional unread backward article)
  (let* ((simp-subject (gnus-simplify-subject-fully subject))
	 (article (or article (gnus-summary-article-number)))
	 (articles (gnus-data-list backward))
	 (arts (gnus-data-find-list article articles))
	 result)
    (when (or (not gnus-summary-check-current)
	      (not unread)
	      (not (gnus-data-unread-p (car arts))))
      (setq arts (cdr arts)))
    (while arts
      (and (or (not unread)
	       (gnus-data-unread-p (car arts)))
	   (vectorp (gnus-data-header (car arts)))
	   (gnus-subject-equal
	    simp-subject (mail-header-subject (gnus-data-header (car arts))) t)
	   (setq result (car arts)
		 arts nil))
      (setq arts (cdr arts)))
    (and result
	 (goto-char (gnus-data-pos result))
	 (gnus-data-number result))))

(defun gnus-summary-search-forward (&optional unread subject backward)
  "Search forward for an article.
If UNREAD, look for unread articles.  If SUBJECT, look for
articles with that subject.  If BACKWARD, search backward instead."
  (cond (subject (gnus-summary-find-subject subject unread backward))
	(backward (gnus-summary-find-prev unread))
	(t (gnus-summary-find-next unread))))

(defun gnus-recenter (&optional n)
  "Center point in window and redisplay frame.
Also do horizontal recentering."
  (interactive "P")
  (when (and gnus-auto-center-summary
	     (not (eq gnus-auto-center-summary 'vertical)))
    (gnus-horizontal-recenter))
  (recenter n))

(defun gnus-summary-recenter ()
  "Center point in the summary window.
If `gnus-auto-center-summary' is nil, or the article buffer isn't
displayed, no centering will be performed."
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.  Suggested by popovich@park.cs.columbia.edu.
  (interactive)
  (let* ((top (cond ((< (window-height) 4) 0)
		    ((< (window-height) 7) 1)
		    (t (if (numberp gnus-auto-center-summary)
			   gnus-auto-center-summary
			 2))))
	 (height (1- (window-height)))
	 (bottom (save-excursion (goto-char (point-max))
				 (forward-line (- height))
				 (point)))
	 (window (get-buffer-window (current-buffer))))
    ;; The user has to want it.
    (when gnus-auto-center-summary
      (when (get-buffer-window gnus-article-buffer)
	;; Only do recentering when the article buffer is displayed,
	;; Set the window start to either `bottom', which is the biggest
	;; possible valid number, or the second line from the top,
	;; whichever is the least.
	(let ((top-pos (save-excursion (forward-line (- top)) (point))))
	  (if (> bottom top-pos)
	      ;; Keep the second line from the top visible
	      (set-window-start window top-pos t)
	    ;; Try to keep the bottom line visible; if it's partially
	    ;; obscured, either scroll one more line to make it fully
	    ;; visible, or revert to using TOP-POS.
	    (save-excursion
	      (goto-char (point-max))
	      (forward-line -1)
	      (let ((last-line-start (point)))
		(goto-char bottom)
		(set-window-start window (point) t)
		(when (not (pos-visible-in-window-p last-line-start window))
		  (forward-line 1)
		  (set-window-start window (min (point) top-pos) t)))))))
      ;; Do horizontal recentering while we're at it.
      (when (and (get-buffer-window (current-buffer) t)
		 (not (eq gnus-auto-center-summary 'vertical)))
	(let ((selected (selected-window)))
	  (select-window (get-buffer-window (current-buffer) t))
	  (gnus-summary-position-point)
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-summary-jump-to-group (newsgroup)
  "Move point to NEWSGROUP in group mode buffer."
  ;; Keep update point of group mode buffer if visible.
  (if (eq (current-buffer) (get-buffer gnus-group-buffer))
      (save-window-excursion
	;; Take care of tree window mode.
	(when (get-buffer-window gnus-group-buffer)
	  (pop-to-buffer gnus-group-buffer))
	(gnus-group-jump-to-group newsgroup))
    (save-excursion
      ;; Take care of tree window mode.
      (if (get-buffer-window gnus-group-buffer)
	  (pop-to-buffer gnus-group-buffer)
	(set-buffer gnus-group-buffer))
      (gnus-group-jump-to-group newsgroup))))

;; This function returns a list of article numbers based on the
;; difference between the ranges of read articles in this group and
;; the range of active articles.
(defun gnus-list-of-unread-articles (group)
  (let* ((read (gnus-info-read (gnus-get-info group)))
	 (active (or (gnus-active group) (gnus-activate-group group)))
	 (last (cdr active))
	 first nlast unread)
    ;; If none are read, then all are unread.
    (if (not read)
	(setq first (car active))
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article.  Sounds logical, doesn't it?
      (if (and (not (listp (cdr read)))
	       (or (< (car read) (car active))
		   (progn (setq read (list read))
			  nil)))
	  (setq first (max (car active) (1+ (cdr read))))
	;; `read' is a list of ranges.
	(when (/= (setq nlast (or (and (numberp (car read)) (car read))
				  (caar read)))
		  1)
	  (setq first (car active)))
	(while read
	  (when first
	    (while (< first nlast)
	      (push first unread)
	      (setq first (1+ first))))
	  (setq first (1+ (if (atom (car read)) (car read) (cdar read))))
	  (setq nlast (if (atom (cadr read)) (cadr read) (caadr read)))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (while (<= first last)
      (push first unread)
      (setq first (1+ first)))
    ;; Return the list of unread articles.
    (delq 0 (nreverse unread))))

(defun gnus-list-of-read-articles (group)
  "Return a list of unread, unticked and non-dormant articles."
  (let* ((info (gnus-get-info group))
	 (marked (gnus-info-marks info))
	 (active (gnus-active group)))
    (and info active
	 (gnus-set-difference
	  (gnus-sorted-complement
	   (gnus-uncompress-range active)
	   (gnus-list-of-unread-articles group))
	  (append
	   (gnus-uncompress-range (cdr (assq 'dormant marked)))
	   (gnus-uncompress-range (cdr (assq 'tick marked))))))))

;; Various summary commands

(defun gnus-summary-select-article-buffer ()
  "Reconfigure windows to show article buffer."
  (interactive)
  (if (not (gnus-buffer-live-p gnus-article-buffer))
      (error "There is no article buffer for this summary buffer")
    (gnus-configure-windows 'article)
    (select-window (get-buffer-window gnus-article-buffer))))

(defun gnus-summary-universal-argument (arg)
  "Perform any operation on all articles that are process/prefixed."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles arg))
	func article)
    (if (eq
	 (setq
	  func
	  (key-binding
	   (read-key-sequence
	    (substitute-command-keys
	     "\\<gnus-summary-mode-map>\\[gnus-summary-universal-argument]"))))
	 'undefined)
	(gnus-error 1 "Undefined key")
      (save-excursion
	(while articles
	  (gnus-summary-goto-subject (setq article (pop articles)))
	  (let (gnus-newsgroup-processable)
	    (command-execute func))
	  (gnus-summary-remove-process-mark article)))))
  (gnus-summary-position-point))

(defun gnus-summary-toggle-truncation (&optional arg)
  "Toggle truncation of summary lines.
With arg, turn line truncation on iff arg is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-summary-reselect-current-group (&optional all rescan)
  "Exit and then reselect the current newsgroup.
The prefix argument ALL means to select all articles."
  (interactive "P")
  (when (gnus-ephemeral-group-p gnus-newsgroup-name)
    (error "Ephemeral groups can't be reselected"))
  (let ((current-subject (gnus-summary-article-number))
	(group gnus-newsgroup-name))
    (setq gnus-newsgroup-begin nil)
    (gnus-summary-exit)
    ;; We have to adjust the point of group mode buffer because
    ;; point was moved to the next unread newsgroup by exiting.
    (gnus-summary-jump-to-group group)
    (when rescan
      (save-excursion
	(gnus-group-get-new-news-this-group 1)))
    (gnus-group-read-group all t)
    (gnus-summary-goto-subject current-subject nil t)))

(defun gnus-summary-rescan-group (&optional all)
  "Exit the newsgroup, ask for new articles, and select the newsgroup."
  (interactive "P")
  (gnus-summary-reselect-current-group all t))

(defun gnus-summary-update-info (&optional non-destructive)
  (save-excursion
    (let ((group gnus-newsgroup-name))
      (when group
	(when gnus-newsgroup-kill-headers
	  (setq gnus-newsgroup-killed
		(gnus-compress-sequence
		 (nconc
		  (gnus-set-sorted-intersection
		   (gnus-uncompress-range gnus-newsgroup-killed)
		   (setq gnus-newsgroup-unselected
			 (sort gnus-newsgroup-unselected '<)))
		  (setq gnus-newsgroup-unreads
			(sort gnus-newsgroup-unreads '<)))
		 t)))
	(unless (listp (cdr gnus-newsgroup-killed))
	  (setq gnus-newsgroup-killed (list gnus-newsgroup-killed)))
	(let ((headers gnus-newsgroup-headers))
	  ;; Set the new ranges of read articles.
	  (save-excursion
	    (set-buffer gnus-group-buffer)
	    (gnus-undo-force-boundary))
	  (gnus-update-read-articles
	   group (append gnus-newsgroup-unreads gnus-newsgroup-unselected))
	  ;; Set the current article marks.
	  (let ((gnus-newsgroup-scored
		 (if (and (not gnus-save-score)
			  (not non-destructive))
		     nil
		   gnus-newsgroup-scored)))
	    (save-excursion
	      (gnus-update-marks)))
	  ;; Do the cross-ref thing.
	  (when gnus-use-cross-reference
	    (gnus-mark-xrefs-as-read group headers gnus-newsgroup-unreads))
	  ;; Do not switch windows but change the buffer to work.
	  (set-buffer gnus-group-buffer)
	  (unless (gnus-ephemeral-group-p group)
	    (gnus-group-update-group group)))))))

(defun gnus-summary-save-newsrc (&optional force)
  "Save the current number of read/marked articles in the dribble buffer.
The dribble buffer will then be saved.
If FORCE (the prefix), also save the .newsrc file(s)."
  (interactive "P")
  (gnus-summary-update-info t)
  (if force
      (gnus-save-newsrc-file)
    (gnus-dribble-save)))

(defun gnus-summary-exit (&optional temporary)
  "Exit reading current newsgroup, and then return to group selection mode.
`gnus-exit-group-hook' is called with no arguments if that value is non-nil."
  (interactive)
  (gnus-set-global-variables)
  (when (gnus-buffer-live-p gnus-article-buffer)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (mm-destroy-parts gnus-article-mime-handles)
      ;; Set it to nil for safety reason.
      (setq gnus-article-mime-handle-alist nil)
      (setq gnus-article-mime-handles nil)))
  (gnus-kill-save-kill-buffer)
  (gnus-async-halt-prefetch)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config gnus-newsgroup-name))
	 (mode major-mode)
         (group-point nil)
	 (buf (current-buffer)))
    (unless quit-config
      ;; Do adaptive scoring, and possibly save score files.
      (when gnus-newsgroup-adaptive
	(gnus-score-adaptive))
      (when gnus-use-scoring
	(gnus-score-save)))
    (gnus-run-hooks 'gnus-summary-prepare-exit-hook)
    ;; If we have several article buffers, we kill them at exit.
    (unless gnus-single-article-buffer
      (gnus-kill-buffer gnus-original-article-buffer)
      (setq gnus-article-current nil))
    (when gnus-use-cache
      (gnus-cache-possibly-remove-articles)
      (gnus-cache-save-buffers))
    (gnus-async-prefetch-remove-group group)
    (when gnus-suppress-duplicates
      (gnus-dup-enter-articles))
    (when gnus-use-trees
      (gnus-tree-close group))
    (when gnus-use-cache
      (gnus-cache-write-active))
    ;; Remove entries for this group.
    (nnmail-purge-split-history (gnus-group-real-name group))
    ;; Make all changes in this group permanent.
    (unless quit-config
      (gnus-run-hooks 'gnus-exit-group-hook)
      (gnus-summary-update-info))
    (gnus-close-group group)
    ;; Make sure where we were, and go to next newsgroup.
    (set-buffer gnus-group-buffer)
    (unless quit-config
      (gnus-group-jump-to-group group))
    (gnus-run-hooks 'gnus-summary-exit-hook)
    (unless (or quit-config
		;; If this group has disappeared from the summary
		;; buffer, don't skip forwards.
		(not (string= group (gnus-group-group-name))))
      (gnus-group-next-unread-group 1))
    (setq group-point (point))
    (if temporary
	nil				;Nothing to do.
      ;; If we have several article buffers, we kill them at exit.
      (unless gnus-single-article-buffer
	(gnus-kill-buffer gnus-article-buffer)
	(gnus-kill-buffer gnus-original-article-buffer)
	(setq gnus-article-current nil))
      (set-buffer buf)
      (if (not gnus-kill-summary-on-exit)
	  (gnus-deaden-summary)
	;; We set all buffer-local variables to nil.  It is unclear why
	;; this is needed, but if we don't, buffer-local variables are
	;; not garbage-collected, it seems.  This would the lead to en
	;; ever-growing Emacs.
	(gnus-summary-clear-local-variables)
	(when (get-buffer gnus-article-buffer)
	  (bury-buffer gnus-article-buffer))
	;; We clear the global counterparts of the buffer-local
	;; variables as well, just to be on the safe side.
	(set-buffer gnus-group-buffer)
	(gnus-summary-clear-local-variables)
	;; Return to group mode buffer.
	(when (eq mode 'gnus-summary-mode)
	  (gnus-kill-buffer buf)))
      (setq gnus-current-select-method gnus-select-method)
      (pop-to-buffer gnus-group-buffer)
      (if (not quit-config)
	  (progn
	    (goto-char group-point)
	    (gnus-configure-windows 'group 'force))
	(gnus-handle-ephemeral-exit quit-config))
      ;; Clear the current group name.
      (unless quit-config
	(setq gnus-newsgroup-name nil)))))

(defalias 'gnus-summary-quit 'gnus-summary-exit-no-update)
(defun gnus-summary-exit-no-update (&optional no-questions)
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config group)))
    (when (or no-questions
	      gnus-expert-user
	      (gnus-y-or-n-p "Discard changes to this group and exit? "))
      (gnus-async-halt-prefetch)
      (mapcar 'funcall
	      (delq 'gnus-summary-expire-articles
		    (copy-sequence gnus-summary-prepare-exit-hook)))
      (when (gnus-buffer-live-p gnus-article-buffer)
	(save-excursion
	  (set-buffer gnus-article-buffer)
	  (mm-destroy-parts gnus-article-mime-handles)
	  ;; Set it to nil for safety reason.
	  (setq gnus-article-mime-handle-alist nil)
	  (setq gnus-article-mime-handles nil)))
      ;; If we have several article buffers, we kill them at exit.
      (unless gnus-single-article-buffer
	(gnus-kill-buffer gnus-article-buffer)
	(gnus-kill-buffer gnus-original-article-buffer)
	(setq gnus-article-current nil))
      (if (not gnus-kill-summary-on-exit)
	  (gnus-deaden-summary)
	(gnus-close-group group)
	(gnus-summary-clear-local-variables)
	(set-buffer gnus-group-buffer)
	(gnus-summary-clear-local-variables)
	(when (get-buffer gnus-summary-buffer)
	  (kill-buffer gnus-summary-buffer)))
      (unless gnus-single-article-buffer
	(setq gnus-article-current nil))
      (when gnus-use-trees
	(gnus-tree-close group))
      (gnus-async-prefetch-remove-group group)
      (when (get-buffer gnus-article-buffer)
	(bury-buffer gnus-article-buffer))
      ;; Return to the group buffer.
      (gnus-configure-windows 'group 'force)
      ;; Clear the current group name.
      (setq gnus-newsgroup-name nil)
      (when (equal (gnus-group-group-name) group)
	(gnus-group-next-unread-group 1))
      (when quit-config
        (gnus-handle-ephemeral-exit quit-config)))))

(defun gnus-handle-ephemeral-exit (quit-config)
  "Handle movement when leaving an ephemeral group.
The state which existed when entering the ephemeral is reset."
  (if (not (buffer-name (car quit-config)))
      (gnus-configure-windows 'group 'force)
    (set-buffer (car quit-config))
    (cond ((eq major-mode 'gnus-summary-mode)
           (gnus-set-global-variables))
          ((eq major-mode 'gnus-article-mode)
           (save-excursion
             ;; The `gnus-summary-buffer' variable may point
             ;; to the old summary buffer when using a single
             ;; article buffer.
             (unless (gnus-buffer-live-p gnus-summary-buffer)
               (set-buffer gnus-group-buffer))
             (set-buffer gnus-summary-buffer)
             (gnus-set-global-variables))))
    (if (or (eq (cdr quit-config) 'article)
            (eq (cdr quit-config) 'pick))
        (progn
          ;; The current article may be from the ephemeral group
          ;; thus it is best that we reload this article
          (gnus-summary-show-article)
          (if (and (boundp 'gnus-pick-mode) (symbol-value 'gnus-pick-mode))
              (gnus-configure-windows 'pick 'force)
            (gnus-configure-windows (cdr quit-config) 'force)))
      (gnus-configure-windows (cdr quit-config) 'force))
    (when (eq major-mode 'gnus-summary-mode)
      (gnus-summary-next-subject 1 nil t)
      (gnus-summary-recenter)
      (gnus-summary-position-point))))

;;; Dead summaries.

(defvar gnus-dead-summary-mode-map nil)

(unless gnus-dead-summary-mode-map
  (setq gnus-dead-summary-mode-map (make-keymap))
  (suppress-keymap gnus-dead-summary-mode-map)
  (substitute-key-definition
   'undefined 'gnus-summary-wake-up-the-dead gnus-dead-summary-mode-map)
  (let ((keys '("\C-d" "\r" "\177" [delete])))
    (while keys
      (define-key gnus-dead-summary-mode-map
	(pop keys) 'gnus-summary-wake-up-the-dead))))

(defvar gnus-dead-summary-mode nil
  "Minor mode for Gnus summary buffers.")

(defun gnus-dead-summary-mode (&optional arg)
  "Minor mode for Gnus summary buffers."
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (make-local-variable 'gnus-dead-summary-mode)
    (setq gnus-dead-summary-mode
	  (if (null arg) (not gnus-dead-summary-mode)
	    (> (prefix-numeric-value arg) 0)))
    (when gnus-dead-summary-mode
      (gnus-add-minor-mode
       'gnus-dead-summary-mode " Dead" gnus-dead-summary-mode-map))))

(defun gnus-deaden-summary ()
  "Make the current summary buffer into a dead summary buffer."
  ;; Kill any previous dead summary buffer.
  (when (and gnus-dead-summary
	     (buffer-name gnus-dead-summary))
    (save-excursion
      (set-buffer gnus-dead-summary)
      (when gnus-dead-summary-mode
	(kill-buffer (current-buffer)))))
  ;; Make this the current dead summary.
  (setq gnus-dead-summary (current-buffer))
  (gnus-dead-summary-mode 1)
  (let ((name (buffer-name)))
    (when (string-match "Summary" name)
      (rename-buffer
       (concat (substring name 0 (match-beginning 0)) "Dead "
	       (substring name (match-beginning 0)))
       t)
      (bury-buffer))))

(defun gnus-kill-or-deaden-summary (buffer)
  "Kill or deaden the summary BUFFER."
  (save-excursion
    (when (and (buffer-name buffer)
	       (not gnus-single-article-buffer))
      (save-excursion
	(set-buffer buffer)
	(gnus-kill-buffer gnus-article-buffer)
	(gnus-kill-buffer gnus-original-article-buffer)))
    (cond (gnus-kill-summary-on-exit
	   (when (and gnus-use-trees
		      (gnus-buffer-exists-p buffer))
	     (save-excursion
	       (set-buffer buffer)
	       (gnus-tree-close gnus-newsgroup-name)))
	   (gnus-kill-buffer buffer))
	  ((gnus-buffer-exists-p buffer)
	   (save-excursion
	     (set-buffer buffer)
	     (gnus-deaden-summary))))))

(defun gnus-summary-wake-up-the-dead (&rest args)
  "Wake up the dead summary buffer."
  (interactive)
  (gnus-dead-summary-mode -1)
  (let ((name (buffer-name)))
    (when (string-match "Dead " name)
      (rename-buffer
       (concat (substring name 0 (match-beginning 0))
	       (substring name (match-end 0)))
       t)))
  (gnus-message 3 "This dead summary is now alive again"))

;; Suggested by Andrew Eskilsson <pi92ae@pt.hk-r.se>.
(defun gnus-summary-fetch-faq (&optional faq-dir)
  "Fetch the FAQ for the current group.
If FAQ-DIR (the prefix), prompt for a directory to search for the faq
in."
  (interactive
   (list
    (when current-prefix-arg
      (completing-read
       "Faq dir: " (and (listp gnus-group-faq-directory)
			(mapcar (lambda (file) (list file))
				gnus-group-faq-directory))))))
  (let (gnus-faq-buffer)
    (when (setq gnus-faq-buffer
		(gnus-group-fetch-faq gnus-newsgroup-name faq-dir))
      (gnus-configure-windows 'summary-faq))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-summary-describe-group (&optional force)
  "Describe the current newsgroup."
  (interactive "P")
  (gnus-group-describe-group force gnus-newsgroup-name))

(defun gnus-summary-describe-briefly ()
  "Describe summary mode commands briefly."
  (interactive)
  (gnus-message 6 (substitute-command-keys "\\<gnus-summary-mode-map>\\[gnus-summary-next-page]:Select  \\[gnus-summary-next-unread-article]:Forward  \\[gnus-summary-prev-unread-article]:Backward  \\[gnus-summary-exit]:Exit  \\[gnus-info-find-node]:Run Info	 \\[gnus-summary-describe-briefly]:This help")))

;; Walking around group mode buffer from summary mode.

(defun gnus-summary-next-group (&optional no-article target-group backward)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected
initially.  If NEXT-GROUP, go to this group.  If BACKWARD, go to
previous group instead."
  (interactive "P")
  ;; Stop pre-fetching.
  (gnus-async-halt-prefetch)
  (let ((current-group gnus-newsgroup-name)
	(current-buffer (current-buffer))
	entered)
    ;; First we semi-exit this group to update Xrefs and all variables.
    ;; We can't do a real exit, because the window conf must remain
    ;; the same in case the user is prompted for info, and we don't
    ;; want the window conf to change before that...
    (gnus-summary-exit t)
    (while (not entered)
      ;; Then we find what group we are supposed to enter.
      (set-buffer gnus-group-buffer)
      (gnus-group-jump-to-group current-group)
      (setq target-group
	    (or target-group
		(if (eq gnus-keep-same-level 'best)
		    (gnus-summary-best-group gnus-newsgroup-name)
		  (gnus-summary-search-group backward gnus-keep-same-level))))
      (if (not target-group)
	  ;; There are no further groups, so we return to the group
	  ;; buffer.
	  (progn
	    (gnus-message 5 "Returning to the group buffer")
	    (setq entered t)
	    (when (gnus-buffer-live-p current-buffer)
	      (set-buffer current-buffer)
	      (gnus-summary-exit))
	    (gnus-run-hooks 'gnus-group-no-more-groups-hook))
	;; We try to enter the target group.
	(gnus-group-jump-to-group target-group)
	(let ((unreads (gnus-group-group-unread)))
	  (if (and (or (eq t unreads)
		       (and unreads (not (zerop unreads))))
		   (gnus-summary-read-group
		    target-group nil no-article
		    (and (buffer-name current-buffer) current-buffer)
		    nil backward))
	      (setq entered t)
	    (setq current-group target-group
		  target-group nil)))))))

(defun gnus-summary-prev-group (&optional no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  (gnus-summary-next-group no-article nil t))

;; Walking around summary lines.

(defun gnus-summary-first-subject (&optional unread undownloaded)
  "Go to the first unread subject.
If UNREAD is non-nil, go to the first unread article.
Returns the article selected or nil if there are no unread articles."
  (interactive "P")
  (prog1
      (cond
       ;; Empty summary.
       ((null gnus-newsgroup-data)
	(gnus-message 3 "No articles in the group")
	nil)
       ;; Pick the first article.
       ((not unread)
	(goto-char (gnus-data-pos (car gnus-newsgroup-data)))
	(gnus-data-number (car gnus-newsgroup-data)))
       ;; No unread articles.
       ((null gnus-newsgroup-unreads)
	(gnus-message 3 "No more unread articles")
	nil)
       ;; Find the first unread article.
       (t
	(let ((data gnus-newsgroup-data))
	  (while (and data
		      (and (not (and undownloaded
				     (eq gnus-undownloaded-mark
					 (gnus-data-mark (car data)))))
			   (not (gnus-data-unread-p (car data)))))
	    (setq data (cdr data)))
	  (when data
	    (goto-char (gnus-data-pos (car data)))
	    (gnus-data-number (car data))))))
    (gnus-summary-position-point)))

(defun gnus-summary-next-subject (n &optional unread dont-display)
  "Go to next N'th summary line.
If N is negative, go to the previous N'th subject line.
If UNREAD is non-nil, only unread articles are selected.
The difference between N and the actual number of steps taken is
returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if backward
		    (gnus-summary-find-prev unread)
		  (gnus-summary-find-next unread)))
      (unless (zerop (setq n (1- n)))
	(gnus-summary-show-thread)))
    (when (/= 0 n)
      (gnus-message 7 "No more%s articles"
		    (if unread " unread" "")))
    (unless dont-display
      (gnus-summary-recenter)
      (gnus-summary-position-point))
    n))

(defun gnus-summary-next-unread-subject (n)
  "Go to next N'th unread summary line."
  (interactive "p")
  (gnus-summary-next-subject n t))

(defun gnus-summary-prev-subject (n &optional unread)
  "Go to previous N'th summary line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (gnus-summary-next-subject (- n) unread))

(defun gnus-summary-prev-unread-subject (n)
  "Go to previous N'th unread summary line."
  (interactive "p")
  (gnus-summary-next-subject (- n) t))

(defun gnus-summary-goto-subject (article &optional force silent)
  "Go the subject line of ARTICLE.
If FORCE, also allow jumping to articles not currently shown."
  (interactive "nArticle number: ")
  (let ((b (point))
	(data (gnus-data-find article)))
    ;; We read in the article if we have to.
    (and (not data)
	 force
	 (gnus-summary-insert-subject
	  article
	  (if (or (numberp force) (vectorp force)) force)
	  t)
	 (setq data (gnus-data-find article)))
    (goto-char b)
    (if (not data)
	(progn
	  (unless silent
	    (gnus-message 3 "Can't find article %d" article))
	  nil)
      (goto-char (gnus-data-pos data))
      (gnus-summary-position-point)
      article)))

;; Walking around summary lines with displaying articles.

(defun gnus-summary-expand-window (&optional arg)
  "Make the summary buffer take up the entire Emacs frame.
Given a prefix, will force an `article' buffer configuration."
  (interactive "P")
  (if arg
      (gnus-configure-windows 'article 'force)
    (gnus-configure-windows 'summary 'force)))

(defun gnus-summary-display-article (article &optional all-header)
  "Display ARTICLE in article buffer."
  (when (gnus-buffer-live-p gnus-article-buffer)
    (with-current-buffer gnus-article-buffer
      (mm-enable-multibyte-mule4)))
  (gnus-set-global-variables)
  (if (null article)
      nil
    (prog1
	(if gnus-summary-display-article-function
	    (funcall gnus-summary-display-article-function article all-header)
	  (gnus-article-prepare article all-header))
      (gnus-run-hooks 'gnus-select-article-hook)
      (when (and gnus-current-article
		 (not (zerop gnus-current-article)))
	(gnus-summary-goto-subject gnus-current-article))
      (gnus-summary-recenter)
      (when (and gnus-use-trees gnus-show-threads)
	(gnus-possibly-generate-tree article)
	(gnus-highlight-selected-tree article))
      ;; Successfully display article.
      (gnus-article-set-window-start
       (cdr (assq article gnus-newsgroup-bookmarks))))))

(defun gnus-summary-select-article (&optional all-headers force pseudo article)
  "Select the current article.
If ALL-HEADERS is non-nil, show all header fields.  If FORCE is
non-nil, the article will be re-fetched even if it already present in
the article buffer.  If PSEUDO is non-nil, pseudo-articles will also
be displayed."
  ;; Make sure we are in the summary buffer to work around bbdb bug.
  (unless (eq major-mode 'gnus-summary-mode)
    (set-buffer gnus-summary-buffer))
  (let ((article (or article (gnus-summary-article-number)))
	(all-headers (not (not all-headers))) ;Must be t or nil.
	gnus-summary-display-article-function)
    (and (not pseudo)
	 (gnus-summary-article-pseudo-p article)
	 (error "This is a pseudo-article"))
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (if (or (and gnus-single-article-buffer
		   (or (null gnus-current-article)
		       (null gnus-article-current)
		       (null (get-buffer gnus-article-buffer))
		       (not (eq article (cdr gnus-article-current)))
		       (not (equal (car gnus-article-current)
				   gnus-newsgroup-name))))
	      (and (not gnus-single-article-buffer)
		   (or (null gnus-current-article)
		       (not (eq gnus-current-article article))))
	      force)
	  ;; The requested article is different from the current article.
	  (progn
	    (gnus-summary-display-article article all-headers)
	    (when (gnus-buffer-live-p gnus-article-buffer)
	       (with-current-buffer gnus-article-buffer
		(if (not gnus-article-decoded-p) ;; a local variable
		    (mm-disable-multibyte-mule4))))
	    (when (or all-headers gnus-show-all-headers)
	      (gnus-article-show-all-headers))
	    (gnus-article-set-window-start
	     (cdr (assq article gnus-newsgroup-bookmarks)))
	    article)
	(when (or all-headers gnus-show-all-headers)
	  (gnus-article-show-all-headers))
	'old))))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Obsolete function."
  nil)

(defun gnus-summary-next-article (&optional unread subject backward push)
  "Select the next article.
If UNREAD, only unread articles are selected.
If SUBJECT, only articles with SUBJECT are selected.
If BACKWARD, the previous article is selected instead of the next."
  (interactive "P")
  (cond
   ;; Is there such an article?
   ((and (gnus-summary-search-forward unread subject backward)
	 (or (gnus-summary-display-article (gnus-summary-article-number))
	     (eq (gnus-summary-article-mark) gnus-canceled-mark)))
    (gnus-summary-position-point))
   ;; If not, we try the first unread, if that is wanted.
   ((and subject
	 gnus-auto-select-same
	 (gnus-summary-first-unread-article))
    (gnus-summary-position-point)
    (gnus-message 6 "Wrapped"))
   ;; Try to get next/previous article not displayed in this group.
   ((and gnus-auto-extend-newsgroup
	 (not unread) (not subject))
    (gnus-summary-goto-article
     (if backward (1- gnus-newsgroup-begin) (1+ gnus-newsgroup-end))
     nil (count-lines (point-min) (point))))
   ;; Go to next/previous group.
   (t
    (unless (gnus-ephemeral-group-p gnus-newsgroup-name)
      (gnus-summary-jump-to-group gnus-newsgroup-name))
    (let ((cmd last-command-char)
	  (point
	   (save-excursion
	     (set-buffer gnus-group-buffer)
	     (point)))
	  (group
	   (if (eq gnus-keep-same-level 'best)
	       (gnus-summary-best-group gnus-newsgroup-name)
	     (gnus-summary-search-group backward gnus-keep-same-level))))
      ;; For some reason, the group window gets selected.  We change
      ;; it back.
      (select-window (get-buffer-window (current-buffer)))
      ;; Select next unread newsgroup automagically.
      (cond
       ((or (not gnus-auto-select-next)
	    (not cmd))
	(gnus-message 7 "No more%s articles" (if unread " unread" "")))
       ((or (eq gnus-auto-select-next 'quietly)
	    (and (eq gnus-auto-select-next 'slightly-quietly)
		 push)
	    (and (eq gnus-auto-select-next 'almost-quietly)
		 (gnus-summary-last-article-p)))
	;; Select quietly.
	(if (gnus-ephemeral-group-p gnus-newsgroup-name)
	    (gnus-summary-exit)
	  (gnus-message 7 "No more%s articles (%s)..."
			(if unread " unread" "")
			(if group (concat "selecting " group)
			  "exiting"))
	  (gnus-summary-next-group nil group backward)))
       (t
	(when (gnus-key-press-event-p last-input-event)
	  (gnus-summary-walk-group-buffer
	   gnus-newsgroup-name cmd unread backward point))))))))

(defun gnus-summary-walk-group-buffer (from-group cmd unread backward start)
  (let ((keystrokes '((?\C-n (gnus-group-next-unread-group 1))
		      (?\C-p (gnus-group-prev-unread-group 1))))
	(cursor-in-echo-area t)
	keve key group ended)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (goto-char start)
      (setq group
	    (if (eq gnus-keep-same-level 'best)
		(gnus-summary-best-group gnus-newsgroup-name)
	      (gnus-summary-search-group backward gnus-keep-same-level))))
    (while (not ended)
      (gnus-message
       5 "No more%s articles%s" (if unread " unread" "")
       (if (and group
		(not (gnus-ephemeral-group-p gnus-newsgroup-name)))
	   (format " (Type %s for %s [%s])"
		   (single-key-description cmd) group
		   (car (gnus-gethash group gnus-newsrc-hashtb)))
	 (format " (Type %s to exit %s)"
		 (single-key-description cmd)
		 gnus-newsgroup-name)))
      ;; Confirm auto selection.
      (setq key (car (setq keve (gnus-read-event-char))))
      (setq ended t)
      (cond
       ((assq key keystrokes)
	(let ((obuf (current-buffer)))
	  (switch-to-buffer gnus-group-buffer)
	  (when group
	    (gnus-group-jump-to-group group))
	  (eval (cadr (assq key keystrokes)))
	  (setq group (gnus-group-group-name))
	  (switch-to-buffer obuf))
	(setq ended nil))
       ((equal key cmd)
	(if (or (not group)
		(gnus-ephemeral-group-p gnus-newsgroup-name))
	    (gnus-summary-exit)
	  (gnus-summary-next-group nil group backward)))
       (t
	(push (cdr keve) unread-command-events))))))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article
   (or (not (eq gnus-summary-goto-unread 'never))
       (gnus-summary-last-article-p (gnus-summary-article-number)))
   (and gnus-auto-select-same
	(gnus-summary-article-subject))))

(defun gnus-summary-prev-article (&optional unread subject)
  "Select the article after the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (gnus-summary-next-article unread subject t))

(defun gnus-summary-prev-unread-article ()
  "Select unread article before current one."
  (interactive)
  (gnus-summary-prev-article
   (or (not (eq gnus-summary-goto-unread 'never))
       (gnus-summary-first-article-p (gnus-summary-article-number)))
   (and gnus-auto-select-same
	(gnus-summary-article-subject))))

(defun gnus-summary-next-page (&optional lines circular)
  "Show next page of the selected article.
If at the end of the current article, select the next article.
LINES says how many lines should be scrolled up.

If CIRCULAR is non-nil, go to the start of the article instead of
selecting the next article when reaching the end of the current
article."
  (interactive "P")
  (setq gnus-summary-buffer (current-buffer))
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number))
	(article-window (get-buffer-window gnus-article-buffer t))
	endp)
    ;; If the buffer is empty, we have no article.
    (unless article
      (error "No article to select"))
    (gnus-configure-windows 'article)
    (if (eq (cdr (assq article gnus-newsgroup-reads)) gnus-canceled-mark)
	(if (and (eq gnus-summary-goto-unread 'never)
		 (not (gnus-summary-last-article-p article)))
	    (gnus-summary-next-article)
	  (gnus-summary-next-unread-article))
      (if (or (null gnus-current-article)
	      (null gnus-article-current)
	      (/= article (cdr gnus-article-current))
	      (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	  ;; Selected subject is different from current article's.
	  (gnus-summary-display-article article)
	(when article-window
	  (gnus-eval-in-buffer-window gnus-article-buffer
	    (setq endp (gnus-article-next-page lines)))
	  (when endp
	    (cond (circular
		   (gnus-summary-beginning-of-article))
		  (lines
		   (gnus-message 3 "End of message"))
		  ((null lines)
		   (if (and (eq gnus-summary-goto-unread 'never)
			    (not (gnus-summary-last-article-p article)))
		       (gnus-summary-next-article)
		     (gnus-summary-next-unread-article))))))))
    (gnus-summary-recenter)
    (gnus-summary-position-point)))

(defun gnus-summary-prev-page (&optional lines move)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down.
If MOVE, move to the previous unread article if point is at
the beginning of the buffer."
  (interactive "P")
  (let ((article (gnus-summary-article-number))
	(article-window (get-buffer-window gnus-article-buffer t))
	endp)
    (gnus-configure-windows 'article)
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-summary-recenter)
      (when article-window
	(gnus-eval-in-buffer-window gnus-article-buffer
	  (setq endp (gnus-article-prev-page lines)))
	(when (and move endp)
	  (cond (lines
		 (gnus-message 3 "Beginning of message"))
		((null lines)
		 (if (and (eq gnus-summary-goto-unread 'never)
			  (not (gnus-summary-first-article-p article)))
		     (gnus-summary-prev-article)
		   (gnus-summary-prev-unread-article))))))))
  (gnus-summary-position-point))

(defun gnus-summary-prev-page-or-article (&optional lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down.
If at the beginning of the article, go to the next article."
  (interactive "P")
  (gnus-summary-prev-page lines t))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (gnus-configure-windows 'article)
  (gnus-summary-show-thread)
  (when (eq (gnus-summary-select-article nil nil 'pseudo) 'old)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (cond ((> lines 0)
	     (when (gnus-article-next-page lines)
	       (gnus-message 3 "End of message")))
	    ((< lines 0)
	     (gnus-article-prev-page (- lines))))))
  (gnus-summary-recenter)
  (gnus-summary-position-point))

(defun gnus-summary-scroll-down (lines)
  "Scroll down (or up) one line current article.
Argument LINES specifies lines to be scrolled down (or up if negative)."
  (interactive "p")
  (gnus-summary-scroll-up (- lines)))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article nil (gnus-summary-article-subject)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article nil (gnus-summary-article-subject)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article t (gnus-summary-article-subject)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article t (gnus-summary-article-subject)))

(defun gnus-summary-first-unread-article ()
  "Select the first unread article.
Return nil if there are no unread articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject t)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject t)
	(gnus-summary-display-article (gnus-summary-article-number)))
    (gnus-summary-position-point)))

(defun gnus-summary-first-unread-subject ()
  "Place the point on the subject line of the first unread article.
Return nil if there are no unread articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject t)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject t))
    (gnus-summary-position-point)))

(defun gnus-summary-first-article ()
  "Select the first article.
Return nil if there are no articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject)
	(gnus-summary-display-article (gnus-summary-article-number)))
    (gnus-summary-position-point)))

(defun gnus-summary-best-unread-article ()
  "Select the unread article with the highest score."
  (interactive)
  (let ((best -1000000)
	(data gnus-newsgroup-data)
	article score)
    (while data
      (and (gnus-data-unread-p (car data))
	   (> (setq score
		    (gnus-summary-article-score (gnus-data-number (car data))))
	      best)
	   (setq best score
		 article (gnus-data-number (car data))))
      (setq data (cdr data)))
    (prog1
	(if article
	    (gnus-summary-goto-article article)
	  (error "No unread articles"))
      (gnus-summary-position-point))))

(defun gnus-summary-last-subject ()
  "Go to the last displayed subject line in the group."
  (let ((article (gnus-data-number (car (gnus-data-list t)))))
    (when article
      (gnus-summary-goto-subject article))))

(defun gnus-summary-goto-article (article &optional all-headers force)
  "Fetch ARTICLE (article number or Message-ID) and display it if it exists.
If ALL-HEADERS is non-nil, no header lines are hidden.
If FORCE, go to the article even if it isn't displayed.  If FORCE
is a number, it is the line the article is to be displayed on."
  (interactive
   (list
    (completing-read
     "Article number or Message-ID: "
     (mapcar (lambda (number) (list (int-to-string number)))
	     gnus-newsgroup-limit))
    current-prefix-arg
    t))
  (prog1
      (if (and (stringp article)
	       (string-match "@" article))
	  (gnus-summary-refer-article article)
	(when (stringp article)
	  (setq article (string-to-number article)))
	(if (gnus-summary-goto-subject article force)
	    (gnus-summary-display-article article all-headers)
	  (gnus-message 4 "Couldn't go to article %s" article) nil))
    (gnus-summary-position-point)))

(defun gnus-summary-goto-last-article ()
  "Go to the previously read article."
  (interactive)
  (prog1
      (when gnus-last-article
	(gnus-summary-goto-article gnus-last-article nil t))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-article (number)
  "Pop one article off the history and go to the previous.
NUMBER articles will be popped off."
  (interactive "p")
  (let (to)
    (setq gnus-newsgroup-history
	  (cdr (setq to (nthcdr number gnus-newsgroup-history))))
    (if to
	(gnus-summary-goto-article (car to) nil t)
      (error "Article history empty")))
  (gnus-summary-position-point))

;; Summary commands and functions for limiting the summary buffer.

(defun gnus-summary-limit-to-articles (n)
  "Limit the summary buffer to the next N articles.
If not given a prefix, use the process marked articles instead."
  (interactive "P")
  (prog1
      (let ((articles (gnus-summary-work-articles n)))
	(setq gnus-newsgroup-processable nil)
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-limit (&optional total)
  "Restore the previous limit.
If given a prefix, remove all limits."
  (interactive "P")
  (when total
    (setq gnus-newsgroup-limits
	  (list (mapcar (lambda (h) (mail-header-number h))
			gnus-newsgroup-headers))))
  (unless gnus-newsgroup-limits
    (error "No limit to pop"))
  (prog1
      (gnus-summary-limit nil 'pop)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-subject (subject &optional header not-matching)
  "Limit the summary buffer to articles that have subjects that match a regexp.
If NOT-MATCHING, excluding articles that have subjects that match a regexp."
  (interactive 
   (list (read-string (if current-prefix-arg
			  "Exclude subject (regexp): "
			"Limit to subject (regexp): ")) 
	 nil current-prefix-arg))
  (unless header
    (setq header "subject"))
  (when (not (equal "" subject))
    (prog1
	(let ((articles (gnus-summary-find-matching
			 (or header "subject") subject 'all nil nil 
			 not-matching)))
	  (unless articles
	    (error "Found no matches for \"%s\"" subject))
	  (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-to-author (from &optional not-matching)
  "Limit the summary buffer to articles that have authors that match a regexp.
If NOT-MATCHING, excluding articles that have authors that match a regexp."
  (interactive 
   (list (read-string (if current-prefix-arg
			  "Exclude author (regexp): "
			"Limit to author (regexp): ")) 
	 current-prefix-arg))
  (gnus-summary-limit-to-subject from "from" not-matching))

(defun gnus-summary-limit-to-age (age &optional younger-p)
  "Limit the summary buffer to articles that are older than (or equal) AGE days.
If YOUNGER-P (the prefix) is non-nil, limit the summary buffer to
articles that are younger than AGE days."
  (interactive
   (let ((younger current-prefix-arg)
	 (days-got nil)
	 days)
     (while (not days-got)
       (setq days (if younger
		      (read-string "Limit to articles within (in days): ")
		    (read-string "Limit to articles old than (in days): ")))
       (when (> (length days) 0)
	 (setq days (read days)))
       (if (numberp days)
	   (setq days-got t)
	 (message "Please enter a number.")
	 (sleep-for 1)))
     (list days younger)))
  (prog1
      (let ((data gnus-newsgroup-data)
	    (cutoff (days-to-time age))
	    articles d date is-younger)
	(while (setq d (pop data))
	  (when (and (vectorp (gnus-data-header d))
		     (setq date (mail-header-date (gnus-data-header d))))
	    (setq is-younger (time-less-p
			      (time-since (condition-case ()
					      (date-to-time date)
					    (error '(0 0))))
			      cutoff))
	    (when (if younger-p
		      is-younger
		    (not is-younger))
	      (push (gnus-data-number d) articles))))
	(gnus-summary-limit (nreverse articles)))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-extra (header regexp &optional not-matching)
  "Limit the summary buffer to articles that match an 'extra' header."
  (interactive
   (let ((header
	  (intern
	   (gnus-completing-read
	    (symbol-name (car gnus-extra-headers))
	    (if current-prefix-arg
		"Exclude extra header:"
	      "Limit extra header:")
	    (mapcar (lambda (x)
		      (cons (symbol-name x) x))
		    gnus-extra-headers)
	    nil
	    t))))
     (list header
	   (read-string (format "%s header %s (regexp): " 
				(if current-prefix-arg "Exclude" "Limit to")
				header))
	   current-prefix-arg)))
  (when (not (equal "" regexp))
    (prog1
	(let ((articles (gnus-summary-find-matching
			 (cons 'extra header) regexp 'all nil nil 
			 not-matching)))
	  (unless articles
	    (error "Found no matches for \"%s\"" regexp))
	  (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defalias 'gnus-summary-delete-marked-as-read 'gnus-summary-limit-to-unread)
(make-obsolete
 'gnus-summary-delete-marked-as-read 'gnus-summary-limit-to-unread)

(defun gnus-summary-limit-to-unread (&optional all)
  "Limit the summary buffer to articles that are not marked as read.
If ALL is non-nil, limit strictly to unread articles."
  (interactive "P")
  (if all
      (gnus-summary-limit-to-marks (char-to-string gnus-unread-mark))
    (gnus-summary-limit-to-marks
     ;; Concat all the marks that say that an article is read and have
     ;; those removed.
     (list gnus-del-mark gnus-read-mark gnus-ancient-mark
	   gnus-killed-mark gnus-kill-file-mark
	   gnus-low-score-mark gnus-expirable-mark
	   gnus-canceled-mark gnus-catchup-mark gnus-sparse-mark
	   gnus-duplicate-mark gnus-souped-mark)
     'reverse)))

(defalias 'gnus-summary-delete-marked-with 'gnus-summary-limit-exclude-marks)
(make-obsolete 'gnus-summary-delete-marked-with
	       'gnus-summary-limit-exclude-marks)

(defun gnus-summary-limit-exclude-marks (marks &optional reverse)
  "Exclude articles that are marked with MARKS (e.g. \"DK\").
If REVERSE, limit the summary buffer to articles that are marked
with MARKS.  MARKS can either be a string of marks or a list of marks.
Returns how many articles were removed."
  (interactive "sMarks: ")
  (gnus-summary-limit-to-marks marks t))

(defun gnus-summary-limit-to-marks (marks &optional reverse)
  "Limit the summary buffer to articles that are marked with MARKS (e.g. \"DK\").
If REVERSE (the prefix), limit the summary buffer to articles that are
not marked with MARKS.  MARKS can either be a string of marks or a
list of marks.
Returns how many articles were removed."
  (interactive "sMarks: \nP")
  (prog1
      (let ((data gnus-newsgroup-data)
	    (marks (if (listp marks) marks
		     (append marks nil))) ; Transform to list.
	    articles)
	(while data
	  (when (if reverse (not (memq (gnus-data-mark (car data)) marks))
		  (memq (gnus-data-mark (car data)) marks))
	    (push (gnus-data-number (car data)) articles))
	  (setq data (cdr data)))
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-score (&optional score)
  "Limit to articles with score at or above SCORE."
  (interactive "P")
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (let ((data gnus-newsgroup-data)
	articles)
    (while data
      (when (>= (gnus-summary-article-score (gnus-data-number (car data)))
		score)
	(push (gnus-data-number (car data)) articles))
      (setq data (cdr data)))
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))

(defun gnus-summary-limit-include-thread (id)
  "Display all the hidden articles that in the current thread."
  (interactive (list (mail-header-id (gnus-summary-article-header))))
  (let ((articles (gnus-articles-in-thread
		   (gnus-id-to-thread (gnus-root-id id)))))
    (prog1
	(gnus-summary-limit (nconc articles gnus-newsgroup-limit))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-include-dormant ()
  "Display all the hidden articles that are marked as dormant.
Note that this command only works on a subset of the articles currently
fetched for this group."
  (interactive)
  (unless gnus-newsgroup-dormant
    (error "There are no dormant articles in this group"))
  (prog1
      (gnus-summary-limit (append gnus-newsgroup-dormant gnus-newsgroup-limit))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-dormant ()
  "Hide all dormant articles."
  (interactive)
  (prog1
      (gnus-summary-limit-to-marks (list gnus-dormant-mark) 'reverse)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-childless-dormant ()
  "Hide all dormant articles that have no children."
  (interactive)
  (let ((data (gnus-data-list t))
	articles d children)
    ;; Find all articles that are either not dormant or have
    ;; children.
    (while (setq d (pop data))
      (when (or (not (= (gnus-data-mark d) gnus-dormant-mark))
		(and (setq children
			   (gnus-article-children (gnus-data-number d)))
		     (let (found)
		       (while children
			 (when (memq (car children) articles)
			   (setq children nil
				 found t))
			 (pop children))
		       found)))
	(push (gnus-data-number d) articles)))
    ;; Do the limiting.
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))

(defun gnus-summary-limit-mark-excluded-as-read (&optional all)
  "Mark all unread excluded articles as read.
If ALL, mark even excluded ticked and dormants as read."
  (interactive "P")
  (let ((articles (gnus-sorted-complement
		   (sort
		    (mapcar (lambda (h) (mail-header-number h))
			    gnus-newsgroup-headers)
		    '<)
		   (sort gnus-newsgroup-limit '<)))
	article)
    (setq gnus-newsgroup-unreads
	  (gnus-intersection gnus-newsgroup-unreads gnus-newsgroup-limit))
    (if all
	(setq gnus-newsgroup-dormant nil
	      gnus-newsgroup-marked nil
	      gnus-newsgroup-reads
	      (nconc
	       (mapcar (lambda (n) (cons n gnus-catchup-mark)) articles)
	       gnus-newsgroup-reads))
      (while (setq article (pop articles))
	(unless (or (memq article gnus-newsgroup-dormant)
		    (memq article gnus-newsgroup-marked))
	  (push (cons article gnus-catchup-mark) gnus-newsgroup-reads))))))

(defun gnus-summary-limit (articles &optional pop)
  (if pop
      ;; We pop the previous limit off the stack and use that.
      (setq articles (car gnus-newsgroup-limits)
	    gnus-newsgroup-limits (cdr gnus-newsgroup-limits))
    ;; We use the new limit, so we push the old limit on the stack.
    (push gnus-newsgroup-limit gnus-newsgroup-limits))
  ;; Set the limit.
  (setq gnus-newsgroup-limit articles)
  (let ((total (length gnus-newsgroup-data))
	(data (gnus-data-find-list (gnus-summary-article-number)))
	(gnus-summary-mark-below nil)	; Inhibit this.
	found)
    ;; This will do all the work of generating the new summary buffer
    ;; according to the new limit.
    (gnus-summary-prepare)
    ;; Hide any threads, possibly.
    (and gnus-show-threads
	 gnus-thread-hide-subtree
	 (gnus-summary-hide-all-threads))
    ;; Try to return to the article you were at, or one in the
    ;; neighborhood.
    (when data
      ;; We try to find some article after the current one.
      (while data
	(when (gnus-summary-goto-subject (gnus-data-number (car data)) nil t)
	  (setq data nil
		found t))
	(setq data (cdr data))))
    (unless found
      ;; If there is no data, that means that we were after the last
      ;; article.  The same goes when we can't find any articles
      ;; after the current one.
      (goto-char (point-max))
      (gnus-summary-find-prev))
    (gnus-set-mode-line 'summary)
    ;; We return how many articles were removed from the summary
    ;; buffer as a result of the new limit.
    (- total (length gnus-newsgroup-data))))

(defsubst gnus-invisible-cut-children (threads)
  (let ((num 0))
    (while threads
      (when (memq (mail-header-number (caar threads)) gnus-newsgroup-limit)
	(incf num))
      (pop threads))
    (< num 2)))

(defsubst gnus-cut-thread (thread)
  "Go forwards in the thread until we find an article that we want to display."
  (when (or (eq gnus-fetch-old-headers 'some)
	    (eq gnus-fetch-old-headers 'invisible)
	    (numberp gnus-fetch-old-headers)
	    (eq gnus-build-sparse-threads 'some)
	    (eq gnus-build-sparse-threads 'more))
    ;; Deal with old-fetched headers and sparse threads.
    (while (and
	    thread
	    (or
	     (gnus-summary-article-sparse-p (mail-header-number (car thread)))
	     (gnus-summary-article-ancient-p
	      (mail-header-number (car thread))))
	    (if (or (<= (length (cdr thread)) 1)
		    (eq gnus-fetch-old-headers 'invisible))
		(setq gnus-newsgroup-limit
		      (delq (mail-header-number (car thread))
			    gnus-newsgroup-limit)
		      thread (cadr thread))
	      (when (gnus-invisible-cut-children (cdr thread))
		(let ((th (cdr thread)))
		  (while th
		    (if (memq (mail-header-number (caar th))
			      gnus-newsgroup-limit)
			(setq thread (car th)
			      th nil)
		      (setq th (cdr th))))))))))
  thread)

(defun gnus-cut-threads (threads)
  "Cut off all uninteresting articles from the beginning of threads."
  (when (or (eq gnus-fetch-old-headers 'some)
	    (eq gnus-fetch-old-headers 'invisible)
	    (numberp gnus-fetch-old-headers)
	    (eq gnus-build-sparse-threads 'some)
	    (eq gnus-build-sparse-threads 'more))
    (let ((th threads))
      (while th
	(setcar th (gnus-cut-thread (car th)))
	(setq th (cdr th)))))
  ;; Remove nixed out threads.
  (delq nil threads))

(defun gnus-summary-initial-limit (&optional show-if-empty)
  "Figure out what the initial limit is supposed to be on group entry.
This entails weeding out unwanted dormants, low-scored articles,
fetch-old-headers verbiage, and so on."
  ;; Most groups have nothing to remove.
  (if (or gnus-inhibit-limiting
	  (and (null gnus-newsgroup-dormant)
	       (not (eq gnus-fetch-old-headers 'some))
	       (not (numberp gnus-fetch-old-headers))
	       (not (eq gnus-fetch-old-headers 'invisible))
	       (null gnus-summary-expunge-below)
	       (not (eq gnus-build-sparse-threads 'some))
	       (not (eq gnus-build-sparse-threads 'more))
	       (null gnus-thread-expunge-below)
	       (not gnus-use-nocem)))
      ()				; Do nothing.
    (push gnus-newsgroup-limit gnus-newsgroup-limits)
    (setq gnus-newsgroup-limit nil)
    (mapatoms
     (lambda (node)
       (unless (car (symbol-value node))
	 ;; These threads have no parents -- they are roots.
	 (let ((nodes (cdr (symbol-value node)))
	       thread)
	   (while nodes
	     (if (and gnus-thread-expunge-below
		      (< (gnus-thread-total-score (car nodes))
			 gnus-thread-expunge-below))
		 (gnus-expunge-thread (pop nodes))
	       (setq thread (pop nodes))
	       (gnus-summary-limit-children thread))))))
     gnus-newsgroup-dependencies)
    ;; If this limitation resulted in an empty group, we might
    ;; pop the previous limit and use it instead.
    (when (and (not gnus-newsgroup-limit)
	       show-if-empty)
      (setq gnus-newsgroup-limit (pop gnus-newsgroup-limits)))
    gnus-newsgroup-limit))

(defun gnus-summary-limit-children (thread)
  "Return 1 if this subthread is visible and 0 if it is not."
  ;; First we get the number of visible children to this thread.  This
  ;; is done by recursing down the thread using this function, so this
  ;; will really go down to a leaf article first, before slowly
  ;; working its way up towards the root.
  (when thread
    (let ((children
	   (if (cdr thread)
	       (apply '+ (mapcar 'gnus-summary-limit-children
				 (cdr thread)))
	     0))
	  (number (mail-header-number (car thread)))
	  score)
      (if (and
	   (not (memq number gnus-newsgroup-marked))
	   (or
	    ;; If this article is dormant and has absolutely no visible
	    ;; children, then this article isn't visible.
	    (and (memq number gnus-newsgroup-dormant)
		 (zerop children))
	    ;; If this is "fetch-old-headered" and there is no
	    ;; visible children, then we don't want this article.
	    (and (or (eq gnus-fetch-old-headers 'some)
		     (numberp gnus-fetch-old-headers))
		 (gnus-summary-article-ancient-p number)
		 (zerop children))
	    ;; If this is "fetch-old-headered" and `invisible', then
	    ;; we don't want this article.
	    (and (eq gnus-fetch-old-headers 'invisible)
		 (gnus-summary-article-ancient-p number))
	    ;; If this is a sparsely inserted article with no children,
	    ;; we don't want it.
	    (and (eq gnus-build-sparse-threads 'some)
		 (gnus-summary-article-sparse-p number)
		 (zerop children))
	    ;; If we use expunging, and this article is really
	    ;; low-scored, then we don't want this article.
	    (when (and gnus-summary-expunge-below
		       (< (setq score
				(or (cdr (assq number gnus-newsgroup-scored))
				    gnus-summary-default-score))
			  gnus-summary-expunge-below))
	      ;; We increase the expunge-tally here, but that has
	      ;; nothing to do with the limits, really.
	      (incf gnus-newsgroup-expunged-tally)
	      ;; We also mark as read here, if that's wanted.
	      (when (and gnus-summary-mark-below
			 (< score gnus-summary-mark-below))
		(setq gnus-newsgroup-unreads
		      (delq number gnus-newsgroup-unreads))
		(if gnus-newsgroup-auto-expire
		    (push number gnus-newsgroup-expirable)
		  (push (cons number gnus-low-score-mark)
			gnus-newsgroup-reads)))
	      t)
	    ;; Check NoCeM things.
	    (if (and gnus-use-nocem
		     (gnus-nocem-unwanted-article-p
		      (mail-header-id (car thread))))
		(progn
		  (setq gnus-newsgroup-unreads
			(delq number gnus-newsgroup-unreads))
		  t))))
	  ;; Nope, invisible article.
	  0
	;; Ok, this article is to be visible, so we add it to the limit
	;; and return 1.
	(push number gnus-newsgroup-limit)
	1))))

(defun gnus-expunge-thread (thread)
  "Mark all articles in THREAD as read."
  (let* ((number (mail-header-number (car thread))))
    (incf gnus-newsgroup-expunged-tally)
    ;; We also mark as read here, if that's wanted.
    (setq gnus-newsgroup-unreads
	  (delq number gnus-newsgroup-unreads))
    (if gnus-newsgroup-auto-expire
	(push number gnus-newsgroup-expirable)
      (push (cons number gnus-low-score-mark)
	    gnus-newsgroup-reads)))
  ;; Go recursively through all subthreads.
  (mapcar 'gnus-expunge-thread (cdr thread)))

;; Summary article oriented commands

(defun gnus-summary-refer-parent-article (n)
  "Refer parent article N times.
If N is negative, go to ancestor -N instead.
The difference between N and the number of articles fetched is returned."
  (interactive "p")
  (let ((skip 1)
	error header ref)
    (when (not (natnump n))
      (setq skip (abs n)
	    n 1))
    (while (and (> n 0)
		(not error))
      (setq header (gnus-summary-article-header))
      (if (and (eq (mail-header-number header)
		   (cdr gnus-article-current))
	       (equal gnus-newsgroup-name
		      (car gnus-article-current)))
	  ;; If we try to find the parent of the currently
	  ;; displayed article, then we take a look at the actual
	  ;; References header, since this is slightly more
	  ;; reliable than the References field we got from the
	  ;; server.
	  (save-excursion
	    (set-buffer gnus-original-article-buffer)
	    (nnheader-narrow-to-headers)
	    (unless (setq ref (message-fetch-field "references"))
	      (setq ref (message-fetch-field "in-reply-to")))
	    (widen))
	(setq ref
	      ;; It's not the current article, so we take a bet on
	      ;; the value we got from the server.
	      (mail-header-references header)))
      (if (and ref
	       (not (equal ref "")))
	  (unless (gnus-summary-refer-article (gnus-parent-id ref skip))
	    (gnus-message 1 "Couldn't find parent"))
	(gnus-message 1 "No references in article %d"
		      (gnus-summary-article-number))
	(setq error t))
      (decf n))
    (gnus-summary-position-point)
    n))

(defun gnus-summary-refer-references ()
  "Fetch all articles mentioned in the References header.
Return the number of articles fetched."
  (interactive)
  (let ((ref (mail-header-references (gnus-summary-article-header)))
	(current (gnus-summary-article-number))
	(n 0))
    (if (or (not ref)
	    (equal ref ""))
	(error "No References in the current article")
      ;; For each Message-ID in the References header...
      (while (string-match "<[^>]*>" ref)
	(incf n)
	;; ... fetch that article.
	(gnus-summary-refer-article
	 (prog1 (match-string 0 ref)
	   (setq ref (substring ref (match-end 0))))))
      (gnus-summary-goto-subject current)
      (gnus-summary-position-point)
      n)))

(defun gnus-summary-refer-thread (&optional limit)
  "Fetch all articles in the current thread.
If LIMIT (the numerical prefix), fetch that many old headers instead
of what's specified by the `gnus-refer-thread-limit' variable."
  (interactive "P")
  (let ((id (mail-header-id (gnus-summary-article-header)))
	(limit (if limit (prefix-numeric-value limit)
		 gnus-refer-thread-limit)))
    ;; We want to fetch LIMIT *old* headers, but we also have to
    ;; re-fetch all the headers in the current buffer, because many of
    ;; them may be undisplayed.  So we adjust LIMIT.
    (when (numberp limit)
      (incf limit (- gnus-newsgroup-end gnus-newsgroup-begin)))
    (unless (eq gnus-fetch-old-headers 'invisible)
      (gnus-message 5 "Fetching headers for %s..." gnus-newsgroup-name)
      ;; Retrieve the headers and read them in.
      (if (eq (gnus-retrieve-headers
	       (list gnus-newsgroup-end) gnus-newsgroup-name limit)
	      'nov)
	  (gnus-build-all-threads)
	(error "Can't fetch thread from backends that don't support NOV"))
      (gnus-message 5 "Fetching headers for %s...done" gnus-newsgroup-name))
    (gnus-summary-limit-include-thread id)))

(defun gnus-summary-refer-article (message-id)
  "Fetch an article specified by MESSAGE-ID."
  (interactive "sMessage-ID: ")
  (when (and (stringp message-id)
	     (not (zerop (length message-id))))
    ;; Construct the correct Message-ID if necessary.
    ;; Suggested by tale@pawl.rpi.edu.
    (unless (string-match "^<" message-id)
      (setq message-id (concat "<" message-id)))
    (unless (string-match ">$" message-id)
      (setq message-id (concat message-id ">")))
    (let* ((header (gnus-id-to-header message-id))
	   (sparse (and header
			(gnus-summary-article-sparse-p
			 (mail-header-number header))
			(memq (mail-header-number header)
			      gnus-newsgroup-limit)))
	   number)
      (cond
       ;; If the article is present in the buffer we just go to it.
       ((and header
	     (or (not (gnus-summary-article-sparse-p
		       (mail-header-number header)))
		 sparse))
	(prog1
	    (gnus-summary-goto-article
	     (mail-header-number header) nil t)
	  (when sparse
	    (gnus-summary-update-article (mail-header-number header)))))
       (t
	;; We fetch the article.
	(catch 'found
	  (dolist (gnus-override-method (gnus-refer-article-methods))
	    (gnus-check-server gnus-override-method)
	    ;; Fetch the header, and display the article.
	    (when (setq number (gnus-summary-insert-subject message-id))
	      (gnus-summary-select-article nil nil nil number)
	      (throw 'found t)))
	  (gnus-message 3 "Couldn't fetch article %s" message-id)))))))

(defun gnus-refer-article-methods ()
  "Return a list of referrable methods."
  (cond
   ;; No method, so we default to current and native.
   ((null gnus-refer-article-method)
    (list gnus-current-select-method gnus-select-method))
   ;; Current.
   ((eq 'current gnus-refer-article-method)
    (list gnus-current-select-method))
   ;; List of select methods.
   ((not (and (symbolp (car gnus-refer-article-method))
	      (assq (car gnus-refer-article-method) nnoo-definition-alist)))
    (let (out)
      (dolist (method gnus-refer-article-method)
	(push (if (eq 'current method)
		  gnus-current-select-method
		method)
	      out))
      (nreverse out)))
   ;; One single select method.
   (t
    (list gnus-refer-article-method))))

(defun gnus-summary-edit-parameters ()
  "Edit the group parameters of the current group."
  (interactive)
  (gnus-group-edit-group gnus-newsgroup-name 'params))

(defun gnus-summary-customize-parameters ()
  "Customize the group parameters of the current group."
  (interactive)
  (gnus-group-customize gnus-newsgroup-name))

(defun gnus-summary-enter-digest-group (&optional force)
  "Enter an nndoc group based on the current article.
If FORCE, force a digest interpretation.  If not, try
to guess what the document format is."
  (interactive "P")
  (let ((conf gnus-current-window-configuration))
    (save-excursion
      (gnus-summary-select-article))
    (setq gnus-current-window-configuration conf)
    (let* ((name (format "%s-%d"
			 (gnus-group-prefixed-name
			  gnus-newsgroup-name (list 'nndoc ""))
			 (save-excursion
			   (set-buffer gnus-summary-buffer)
			   gnus-current-article)))
	   (ogroup gnus-newsgroup-name)
	   (params (append (gnus-info-params (gnus-get-info ogroup))
			   (list (cons 'to-group ogroup))
			   (list (cons 'save-article-group ogroup))))
	   (case-fold-search t)
	   (buf (current-buffer))
	   dig to-address)
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	;; Have the digest group inherit the main mail address of
	;; the parent article.
	(when (setq to-address (or (message-fetch-field "reply-to")
				   (message-fetch-field "from")))
	  (setq params (append 
			(list (cons 'to-address 
				    (funcall gnus-decode-encoded-word-function
					     to-address))))))
	(setq dig (nnheader-set-temp-buffer " *gnus digest buffer*"))
	(insert-buffer-substring gnus-original-article-buffer)
	;; Remove lines that may lead nndoc to misinterpret the
	;; document type.
	(narrow-to-region
	 (goto-char (point-min))
	 (or (search-forward "\n\n" nil t) (point)))
	(goto-char (point-min))
	(delete-matching-lines "^Path:\\|^From ")
	(widen))
      (unwind-protect
          (if (let ((gnus-newsgroup-ephemeral-charset gnus-newsgroup-charset)
		    (gnus-newsgroup-ephemeral-ignored-charsets
		     gnus-newsgroup-ignored-charsets))
		(gnus-group-read-ephemeral-group
		 name `(nndoc ,name (nndoc-address ,(get-buffer dig))
			      (nndoc-article-type
			       ,(if force 'mbox 'guess))) t))
	      ;; Make all postings to this group go to the parent group.
              (nconc (gnus-info-params (gnus-get-info name))
                     params)
            ;; Couldn't select this doc group.
            (switch-to-buffer buf)
            (gnus-set-global-variables)
            (gnus-configure-windows 'summary)
            (gnus-message 3 "Article couldn't be entered?"))
	(kill-buffer dig)))))

(defun gnus-summary-read-document (n)
  "Open a new group based on the current article(s).
This will allow you to read digests and other similar
documents as newsgroups.
Obeys the standard process/prefix convention."
  (interactive "P")
  (let* ((articles (gnus-summary-work-articles n))
	 (ogroup gnus-newsgroup-name)
	 (params (append (gnus-info-params (gnus-get-info ogroup))
			 (list (cons 'to-group ogroup))))
	 article group egroup groups vgroup)
    (while (setq article (pop articles))
      (setq group (format "%s-%d" gnus-newsgroup-name article))
      (gnus-summary-remove-process-mark article)
      (when (gnus-summary-display-article article)
	(save-excursion
	  (with-temp-buffer
	    (insert-buffer-substring gnus-original-article-buffer)
	    ;; Remove some headers that may lead nndoc to make
	    ;; the wrong guess.
	    (message-narrow-to-head)
	    (goto-char (point-min))
	    (delete-matching-lines "^\\(Path\\):\\|^From ")
	    (widen)
	    (if (setq egroup
		      (gnus-group-read-ephemeral-group
		       group `(nndoc ,group (nndoc-address ,(current-buffer))
				     (nndoc-article-type guess))
		       t nil t))
		(progn
		  ;; Make all postings to this group go to the parent group.
		  (nconc (gnus-info-params (gnus-get-info egroup))
			 params)
		  (push egroup groups))
	      ;; Couldn't select this doc group.
	      (gnus-error 3 "Article couldn't be entered"))))))
    ;; Now we have selected all the documents.
    (cond
     ((not groups)
      (error "None of the articles could be interpreted as documents"))
     ((gnus-group-read-ephemeral-group
       (setq vgroup (format
		     "nnvirtual:%s-%s" gnus-newsgroup-name
		     (format-time-string "%Y%m%dT%H%M%S" (current-time))))
       `(nnvirtual ,vgroup (nnvirtual-component-groups ,groups))
       t
       (cons (current-buffer) 'summary)))
     (t
      (error "Couldn't select virtual nndoc group")))))

(defun gnus-summary-isearch-article (&optional regexp-p)
  "Do incremental search forward on the current article.
If REGEXP-P (the prefix) is non-nil, do regexp isearch."
  (interactive "P")
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (isearch-forward regexp-p))))

(defun gnus-summary-search-article-forward (regexp &optional backward)
  "Search for an article containing REGEXP forward.
If BACKWARD, search backward instead."
  (interactive
   (list (read-string
	  (format "Search article %s (regexp%s): "
		  (if current-prefix-arg "backward" "forward")
		  (if gnus-last-search-regexp
		      (concat ", default " gnus-last-search-regexp)
		    "")))
	 current-prefix-arg))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-summary-search-article regexp backward)
      (gnus-summary-show-thread)
    (error "Search failed: \"%s\"" regexp)))

(defun gnus-summary-search-article-backward (regexp)
  "Search for an article containing REGEXP backward."
  (interactive
   (list (read-string
	  (format "Search article backward (regexp%s): "
		  (if gnus-last-search-regexp
		      (concat ", default " gnus-last-search-regexp)
		    "")))))
  (gnus-summary-search-article-forward regexp 'backward))

(defun gnus-summary-search-article (regexp &optional backward)
  "Search for an article containing REGEXP.
Optional argument BACKWARD means do search for backward.
`gnus-select-article-hook' is not called during the search."
  ;; We have to require this here to make sure that the following
  ;; dynamic binding isn't shadowed by autoloading.
  (require 'gnus-async)
  (require 'gnus-art)
  (let ((gnus-select-article-hook nil)	;Disable hook.
	(gnus-article-prepare-hook nil)
	(gnus-mark-article-hook nil)	;Inhibit marking as read.
	(gnus-use-article-prefetch nil)
	(gnus-xmas-force-redisplay nil)	;Inhibit XEmacs redisplay.
	(gnus-use-trees nil)		;Inhibit updating tree buffer.
	(sum (current-buffer))
	(gnus-display-mime-function nil)
	(found nil)
	point)
    (gnus-save-hidden-threads
      (gnus-summary-select-article)
      (set-buffer gnus-article-buffer)
      (goto-char (window-point (get-buffer-window (current-buffer))))
      (when backward
	(forward-line -1))
      (while (not found)
	(gnus-message 7 "Searching article: %d..." (cdr gnus-article-current))
	(if (if backward
		(re-search-backward regexp nil t)
	      (re-search-forward regexp nil t))
	    ;; We found the regexp.
	    (progn
	      (setq found 'found)
	      (beginning-of-line)
	      (set-window-start
	       (get-buffer-window (current-buffer))
	       (point))
	      (forward-line 1)
	      (set-window-point
	       (get-buffer-window (current-buffer))
	       (point))
	      (set-buffer sum)
	      (setq point (point)))
	  ;; We didn't find it, so we go to the next article.
	  (set-buffer sum)
	  (setq found 'not)
	  (while (eq found 'not)
	    (if (not (if backward (gnus-summary-find-prev)
		       (gnus-summary-find-next)))
		;; No more articles.
		(setq found t)
	      ;; Select the next article and adjust point.
	      (unless (gnus-summary-article-sparse-p
		       (gnus-summary-article-number))
		(setq found nil)
		(gnus-summary-select-article)
		(set-buffer gnus-article-buffer)
		(widen)
		(goto-char (if backward (point-max) (point-min))))))))
      (gnus-message 7 ""))
    ;; Return whether we found the regexp.
    (when (eq found 'found)
      (goto-char point)
      (gnus-summary-show-thread)
      (gnus-summary-goto-subject gnus-current-article)
      (gnus-summary-position-point)
      t)))

(defun gnus-summary-find-matching (header regexp &optional backward unread
					  not-case-fold not-matching)
  "Return a list of all articles that match REGEXP on HEADER.
The search stars on the current article and goes forwards unless
BACKWARD is non-nil.  If BACKWARD is `all', do all articles.
If UNREAD is non-nil, only unread articles will
be taken into consideration.  If NOT-CASE-FOLD, case won't be folded
in the comparisons. If NOT-MATCHING, return a list of all articles that 
not match REGEXP on HEADER."
  (let ((case-fold-search (not not-case-fold))
	articles d func)
    (if (consp header)
	(if (eq (car header) 'extra)
	    (setq func
		  `(lambda (h)
		     (or (cdr (assq ',(cdr header) (mail-header-extra h)))
			 "")))
	  (error "%s is an invalid header" header))
      (unless (fboundp (intern (concat "mail-header-" header)))
	(error "%s is not a valid header" header))
      (setq func `(lambda (h) (,(intern (concat "mail-header-" header)) h))))
    (dolist (d (if (eq backward 'all)
		   gnus-newsgroup-data
		 (gnus-data-find-list
		  (gnus-summary-article-number)
		  (gnus-data-list backward))))
      (when (and (or (not unread)	; We want all articles...
		     (gnus-data-unread-p d)) ; Or just unreads.
		 (vectorp (gnus-data-header d)) ; It's not a pseudo.
		 (if not-matching
		     (not (string-match 
			   regexp
			   (funcall func (gnus-data-header d))))
		   (string-match regexp
				 (funcall func (gnus-data-header d)))))
	(push (gnus-data-number d) articles))) ; Success!
    (nreverse articles)))

(defun gnus-summary-execute-command (header regexp command &optional backward)
  "Search forward for an article whose HEADER matches REGEXP and execute COMMAND.
If HEADER is an empty string (or nil), the match is done on the entire
article.  If BACKWARD (the prefix) is non-nil, search backward instead."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read
	    "Header name: "
	    (mapcar (lambda (string) (list string))
		    '("Number" "Subject" "From" "Lines" "Date"
		      "Message-ID" "Xref" "References" "Body"))
	    nil 'require-match))
	 (read-string "Regexp: ")
	 (read-key-sequence "Command: ")
	 current-prefix-arg))
  (when (equal header "Body")
    (setq header ""))
  ;; Hidden thread subtrees must be searched as well.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (gnus-message 6 "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute header regexp
		    `(call-interactively ',(key-binding command))
		    backward)
      (gnus-message 6 "Executing %s...done" (key-description command)))))

(defun gnus-summary-beginning-of-article ()
  "Scroll the article back to the beginning."
  (interactive)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-min))
    (when gnus-page-broken
      (gnus-narrow-to-page))))

(defun gnus-summary-end-of-article ()
  "Scroll to the end of the article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-max))
    (recenter -3)
    (when gnus-page-broken
      (gnus-narrow-to-page))))

(defun gnus-summary-print-article (&optional filename n)
  "Generate and print a PostScript image of the N next (mail) articles.

If N is negative, print the N previous articles.  If N is nil and articles
have been marked with the process mark, print these instead.

If the optional first argument FILENAME is nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is a number, prompt the user for the name of the file
to save in."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (dolist (article (gnus-summary-work-articles n))
    (gnus-summary-select-article nil nil 'pseudo article)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (let ((buffer (generate-new-buffer " *print*")))
	(unwind-protect
	    (progn
	      (copy-to-buffer buffer (point-min) (point-max))
	      (set-buffer buffer)
	      (gnus-article-delete-invisible-text)
	      (let ((ps-left-header
		     (list
		      (concat "("
			      (mail-header-subject gnus-current-headers) ")")
		      (concat "("
			      (mail-header-from gnus-current-headers) ")")))
		    (ps-right-header
		     (list
		      "/pagenumberstring load"
		      (concat "("
			      (mail-header-date gnus-current-headers) ")"))))
		(gnus-run-hooks 'gnus-ps-print-hook)
		(save-excursion
		  (ps-spool-buffer-with-faces))))
	  (kill-buffer buffer))))
    (gnus-summary-remove-process-mark article))
  (ps-despool filename))

(defun gnus-summary-show-article (&optional arg)
  "Force re-fetching of the current article.
If ARG (the prefix) is a number, show the article with the charset
defined in `gnus-summary-show-article-charset-alist', or the charset
inputed.
If ARG (the prefix) is non-nil and not a number, show the raw article
without any article massaging functions being run."
  (interactive "P")
  (cond
   ((numberp arg)
    (let ((gnus-newsgroup-charset
	   (or (cdr (assq arg gnus-summary-show-article-charset-alist))
	       (read-coding-system "Charset: ")))
	  (gnus-newsgroup-ignored-charsets 'gnus-all))
      (gnus-summary-select-article nil 'force)))
   ((not arg)
    ;; Select the article the normal way.
    (gnus-summary-select-article nil 'force))
   (t
    ;; We have to require this here to make sure that the following
    ;; dynamic binding isn't shadowed by autoloading.
    (require 'gnus-async)
    (require 'gnus-art)
    ;; Bind the article treatment functions to nil.
    (let ((gnus-have-all-headers t)
	  gnus-article-prepare-hook
	  gnus-article-decode-hook
	  gnus-display-mime-function
	  gnus-break-pages)
      ;; Destroy any MIME parts.
      (when (gnus-buffer-live-p gnus-article-buffer)
	(save-excursion
	  (set-buffer gnus-article-buffer)
	  (mm-destroy-parts gnus-article-mime-handles)
	  ;; Set it to nil for safety reason.
	  (setq gnus-article-mime-handle-alist nil)
	  (setq gnus-article-mime-handles nil)))
      (gnus-summary-select-article nil 'force))))
  (gnus-summary-goto-subject gnus-current-article)
  (gnus-summary-position-point))

(defun gnus-summary-verbose-headers (&optional arg)
  "Toggle permanent full header display.
If ARG is a positive number, turn header display on.
If ARG is a negative number, turn header display off."
  (interactive "P")
  (setq gnus-show-all-headers
	(cond ((or (not (numberp arg))
		   (zerop arg))
	       (not gnus-show-all-headers))
	      ((natnump arg)
	       t)))
  (gnus-summary-show-article))

(defun gnus-summary-toggle-header (&optional arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let* ((buffer-read-only nil)
	     (inhibit-point-motion-hooks t)
	     hidden e)
	(setq hidden
	      (if (numberp arg)
		  (>= arg 0)
		(save-restriction
		  (article-narrow-to-head)
		  (gnus-article-hidden-text-p 'headers))))
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (delete-region (point-min) (1- (point))))
	(goto-char (point-min))
	(save-excursion
	  (set-buffer gnus-original-article-buffer)
	  (goto-char (point-min))
	  (setq e (1- (or (search-forward "\n\n" nil t) (point-max)))))
	(insert-buffer-substring gnus-original-article-buffer 1 e)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (article-decode-encoded-words)
	  (if  hidden
	      (let ((gnus-treat-hide-headers nil)
		    (gnus-treat-hide-boring-headers nil))
		(setq gnus-article-wash-types
		      (delq 'headers gnus-article-wash-types))
		(gnus-treat-article 'head))
	    (gnus-treat-article 'head)))
	(gnus-set-mode-line 'article)))))

(defun gnus-summary-show-all-headers ()
  "Make all header lines visible."
  (interactive)
  (gnus-article-show-all-headers))

(defun gnus-summary-caesar-message (&optional arg)
  "Caesar rotate the current article by 13.
The numerical prefix specifies how many places to rotate each letter
forward."
  (interactive "P")
  (gnus-summary-select-article)
  (let ((mail-header-separator ""))
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-restriction
	(widen)
	(let ((start (window-start))
	      buffer-read-only)
	  (message-caesar-buffer-body arg)
	  (set-window-start (get-buffer-window (current-buffer)) start))))))

(defun gnus-summary-stop-page-breaking ()
  "Stop page breaking in the current article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (when (gnus-visual-p 'page-marker)
      (let ((buffer-read-only nil))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next))
      (setq gnus-page-broken nil))))

(defun gnus-summary-move-article (&optional n to-newsgroup
					    select-method action)
  "Move the current article to a different newsgroup.
If N is a positive number, move the N next articles.
If N is a negative number, move the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to.
If SELECT-METHOD is non-nil, do not move to a specific newsgroup, but
re-spool using this method.

For this function to work, both the current newsgroup and the
newsgroup that you want to move to have to support the `request-move'
and `request-accept' functions.

ACTION can be either `move' (the default), `crosspost' or `copy'."
  (interactive "P")
  (unless action
    (setq action 'move))
  ;; Disable marking as read.
  (let (gnus-mark-article-hook)
    (save-window-excursion
      (gnus-summary-select-article)))
  ;; Check whether the source group supports the required functions.
  (cond ((and (eq action 'move)
	      (not (gnus-check-backend-function
		    'request-move-article gnus-newsgroup-name)))
	 (error "The current group does not support article moving"))
	((and (eq action 'crosspost)
	      (not (gnus-check-backend-function
		    'request-replace-article gnus-newsgroup-name)))
	 (error "The current group does not support article editing")))
  (let ((articles (gnus-summary-work-articles n))
	(prefix (if (gnus-check-backend-function
		    'request-move-article gnus-newsgroup-name)
		    (gnus-group-real-prefix gnus-newsgroup-name)
		  ""))
	(names '((move "Move" "Moving")
		 (copy "Copy" "Copying")
		 (crosspost "Crosspost" "Crossposting")))
	(copy-buf (save-excursion
		    (nnheader-set-temp-buffer " *copy article*")))
	art-group to-method new-xref article to-groups)
    (unless (assq action names)
      (error "Unknown action %s" action))
    ;; Read the newsgroup name.
    (when (and (not to-newsgroup)
	       (not select-method))
      (setq to-newsgroup
	    (gnus-read-move-group-name
	     (cadr (assq action names))
	     (symbol-value (intern (format "gnus-current-%s-group" action)))
	     articles prefix))
      (set (intern (format "gnus-current-%s-group" action)) to-newsgroup))
    (setq to-method (or select-method
			(gnus-server-to-method
			 (gnus-group-method to-newsgroup))))
    ;; Check the method we are to move this article to...
    (unless (gnus-check-backend-function
	     'request-accept-article (car to-method))
      (error "%s does not support article copying" (car to-method)))
    (unless (gnus-check-server to-method)
      (error "Can't open server %s" (car to-method)))
    (gnus-message 6 "%s to %s: %s..."
		  (caddr (assq action names))
		  (or (car select-method) to-newsgroup) articles)
    (while articles
      (setq article (pop articles))
      (setq
       art-group
       (cond
	;; Move the article.
	((eq action 'move)
	 ;; Remove this article from future suppression.
	 (gnus-dup-unsuppress-article article)
	 (gnus-request-move-article
	  article			; Article to move
	  gnus-newsgroup-name		; From newsgroup
	  (nth 1 (gnus-find-method-for-group
		  gnus-newsgroup-name)) ; Server
	  (list 'gnus-request-accept-article
		to-newsgroup (list 'quote select-method)
		(not articles) t)	; Accept form
	  (not articles)))		; Only save nov last time
	;; Copy the article.
	((eq action 'copy)
	 (save-excursion
	   (set-buffer copy-buf)
	   (when (gnus-request-article-this-buffer article gnus-newsgroup-name)
	     (gnus-request-accept-article
	      to-newsgroup select-method (not articles) t))))
	;; Crosspost the article.
	((eq action 'crosspost)
	 (let ((xref (message-tokenize-header
		      (mail-header-xref (gnus-summary-article-header article))
		      " ")))
	   (setq new-xref (concat (gnus-group-real-name gnus-newsgroup-name)
				  ":" article))
	   (unless xref
	     (setq xref (list (system-name))))
	   (setq new-xref
		 (concat
		  (mapconcat 'identity
			     (delete "Xref:" (delete new-xref xref))
			     " ")
		  " " new-xref))
	   (save-excursion
	     (set-buffer copy-buf)
	     ;; First put the article in the destination group.
	     (gnus-request-article-this-buffer article gnus-newsgroup-name)
	     (when (consp (setq art-group
				(gnus-request-accept-article
				 to-newsgroup select-method (not articles))))
	       (setq new-xref (concat new-xref " " (car art-group)
				      ":" (cdr art-group)))
	       ;; Now we have the new Xrefs header, so we insert
	       ;; it and replace the new article.
	       (nnheader-replace-header "Xref" new-xref)
	       (gnus-request-replace-article
		(cdr art-group) to-newsgroup (current-buffer))
	       art-group))))))
      (cond
       ((not art-group)
	(gnus-message 1 "Couldn't %s article %s: %s"
		      (cadr (assq action names)) article
		      (nnheader-get-report (car to-method))))
       ((eq art-group 'junk)
	(when (eq action 'move)
	  (gnus-summary-mark-article article gnus-canceled-mark)
	  (gnus-message 4 "Deleted article %s" article)))
       (t
	(let* ((pto-group (gnus-group-prefixed-name
			   (car art-group) to-method))
	       (entry
		(gnus-gethash pto-group gnus-newsrc-hashtb))
	       (info (nth 2 entry))
               (to-group (gnus-info-group info))
	       to-marks)
	  ;; Update the group that has been moved to.
	  (when (and info
		     (memq action '(move copy)))
	    (unless (member to-group to-groups)
	      (push to-group to-groups))

	    (unless (memq article gnus-newsgroup-unreads)
	      (push 'read to-marks)
	      (gnus-info-set-read
	       info (gnus-add-to-range (gnus-info-read info)
				       (list (cdr art-group)))))

	    ;; See whether the article is to be put in the cache.
	    (let ((marks gnus-article-mark-lists)
		  (to-article (cdr art-group)))

	      ;; Enter the article into the cache in the new group,
	      ;; if that is required.
	      (when gnus-use-cache
		(gnus-cache-possibly-enter-article
		 to-group to-article
		 (memq article gnus-newsgroup-marked)
		 (memq article gnus-newsgroup-dormant)
		 (memq article gnus-newsgroup-unreads)))

	      (when gnus-preserve-marks
		;; Copy any marks over to the new group.
		(when (and (equal to-group gnus-newsgroup-name)
			   (not (memq article gnus-newsgroup-unreads)))
		  ;; Mark this article as read in this group.
		  (push (cons to-article gnus-read-mark) gnus-newsgroup-reads)
		  (setcdr (gnus-active to-group) to-article)
		  (setcdr gnus-newsgroup-active to-article))

		(while marks
		  (when (memq article (symbol-value
				       (intern (format "gnus-newsgroup-%s"
						       (caar marks)))))
		    (push (cdar marks) to-marks)
		    ;; If the other group is the same as this group,
		    ;; then we have to add the mark to the list.
		    (when (equal to-group gnus-newsgroup-name)
		      (set (intern (format "gnus-newsgroup-%s" (caar marks)))
			   (cons to-article
				 (symbol-value
				  (intern (format "gnus-newsgroup-%s"
						  (caar marks)))))))
		    ;; Copy the marks to other group.
		    (gnus-add-marked-articles
		     to-group (cdar marks) (list to-article) info))
		  (setq marks (cdr marks)))

		(gnus-request-set-mark to-group (list (list (list to-article)
							    'set
							    to-marks))))

	      (gnus-dribble-enter
	       (concat "(gnus-group-set-info '"
		       (gnus-prin1-to-string (gnus-get-info to-group))
		       ")"))))

	  ;; Update the Xref header in this article to point to
	  ;; the new crossposted article we have just created.
	  (when (eq action 'crosspost)
	    (save-excursion
	      (set-buffer copy-buf)
	      (gnus-request-article-this-buffer article gnus-newsgroup-name)
	      (nnheader-replace-header "Xref" new-xref)
	      (gnus-request-replace-article
	       article gnus-newsgroup-name (current-buffer)))))

	;;;!!!Why is this necessary?
	(set-buffer gnus-summary-buffer)

	(gnus-summary-goto-subject article)
	(when (eq action 'move)
	  (gnus-summary-mark-article article gnus-canceled-mark))))
      (gnus-summary-remove-process-mark article))
    ;; Re-activate all groups that have been moved to.
    (while to-groups
      (save-excursion
	(set-buffer gnus-group-buffer)
	(when (gnus-group-goto-group (car to-groups) t)
	  (gnus-group-get-new-news-this-group 1 t))
	(pop to-groups)))

    (gnus-kill-buffer copy-buf)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)))

(defun gnus-summary-copy-article (&optional n to-newsgroup select-method)
  "Move the current article to a different newsgroup.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to.
If SELECT-METHOD is non-nil, do not move to a specific newsgroup, but
re-spool using this method."
  (interactive "P")
  (gnus-summary-move-article n to-newsgroup select-method 'copy))

(defun gnus-summary-crosspost-article (&optional n)
  "Crosspost the current article to some other group."
  (interactive "P")
  (gnus-summary-move-article n nil nil 'crosspost))

(defcustom gnus-summary-respool-default-method nil
  "Default method for respooling an article.
If nil, use to the current newsgroup method."
  :type '(choice (gnus-select-method :value (nnml ""))
		 (const nil))
  :group 'gnus-summary-mail)

(defun gnus-summary-respool-article (&optional n method)
  "Respool the current article.
The article will be squeezed through the mail spooling process again,
which means that it will be put in some mail newsgroup or other
depending on `nnmail-split-methods'.
If N is a positive number, respool the N next articles.
If N is a negative number, respool the N previous articles.
If N is nil and any articles have been marked with the process mark,
respool those articles instead.

Respooling can be done both from mail groups and \"real\" newsgroups.
In the former case, the articles in question will be moved from the
current group into whatever groups they are destined to.  In the
latter case, they will be copied into the relevant groups."
  (interactive
   (list current-prefix-arg
	 (let* ((methods (gnus-methods-using 'respool))
		(methname
		 (symbol-name (or gnus-summary-respool-default-method
				  (car (gnus-find-method-for-group
					gnus-newsgroup-name)))))
		(method
		 (gnus-completing-read
		  methname "What backend do you want to use when respooling?"
		  methods nil t nil 'gnus-mail-method-history))
		ms)
	   (cond
	    ((zerop (length (setq ms (gnus-servers-using-backend
				      (intern method)))))
	     (list (intern method) ""))
	    ((= 1 (length ms))
	     (car ms))
	    (t
	     (let ((ms-alist (mapcar (lambda (m) (cons (cadr m) m)) ms)))
	       (cdr (assoc (completing-read "Server name: " ms-alist nil t)
			   ms-alist))))))))
  (unless method
    (error "No method given for respooling"))
  (if (assoc (symbol-name
	      (car (gnus-find-method-for-group gnus-newsgroup-name)))
	     (gnus-methods-using 'respool))
      (gnus-summary-move-article n nil method)
    (gnus-summary-copy-article n nil method)))

(defun gnus-summary-import-article (file)
  "Import an arbitrary file into a mail newsgroup."
  (interactive "fImport file: ")
  (let ((group gnus-newsgroup-name)
	(now (current-time))
	atts lines)
    (unless (gnus-check-backend-function 'request-accept-article group)
      (error "%s does not support article importing" group))
    (or (file-readable-p file)
	(not (file-regular-p file))
	(error "Can't read %s" file))
    (save-excursion
      (set-buffer (gnus-get-buffer-create " *import file*"))
      (erase-buffer)
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (unless (nnheader-article-p)
	;; This doesn't look like an article, so we fudge some headers.
	(setq atts (file-attributes file)
	      lines (count-lines (point-min) (point-max)))
	(insert "From: " (read-string "From: ") "\n"
		"Subject: " (read-string "Subject: ") "\n"
		"Date: " (message-make-date (nth 5 atts))
		"\n"
		"Message-ID: " (message-make-message-id) "\n"
		"Lines: " (int-to-string lines) "\n"
		"Chars: " (int-to-string (nth 7 atts)) "\n\n"))
      (gnus-request-accept-article group nil t)
      (kill-buffer (current-buffer)))))

(defun gnus-summary-article-posted-p ()
  "Say whether the current (mail) article is available from news as well.
This will be the case if the article has both been mailed and posted."
  (interactive)
  (let ((id (mail-header-references (gnus-summary-article-header)))
	(gnus-override-method (car (gnus-refer-article-methods))))
    (if (gnus-request-head id "")
	(gnus-message 2 "The current message was found on %s"
		      gnus-override-method)
      (gnus-message 2 "The current message couldn't be found on %s"
		    gnus-override-method)
      nil)))

(defun gnus-summary-expire-articles (&optional now)
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (when (gnus-check-backend-function
	 'request-expire-articles gnus-newsgroup-name)
    ;; This backend supports expiry.
    (let* ((total (gnus-group-total-expirable-p gnus-newsgroup-name))
	   (expirable (if total
			  (progn
			    ;; We need to update the info for
			    ;; this group for `gnus-list-of-read-articles'
			    ;; to give us the right answer.
			    (gnus-run-hooks 'gnus-exit-group-hook)
			    (gnus-summary-update-info)
			    (gnus-list-of-read-articles gnus-newsgroup-name))
			(setq gnus-newsgroup-expirable
			      (sort gnus-newsgroup-expirable '<))))
	   (expiry-wait (if now 'immediate
			  (gnus-group-find-parameter
			   gnus-newsgroup-name 'expiry-wait)))
	   (nnmail-expiry-target
	    (or (gnus-group-find-parameter gnus-newsgroup-name 'expiry-target)
		nnmail-expiry-target))
	   es)
      (when expirable
	;; There are expirable articles in this group, so we run them
	;; through the expiry process.
	(gnus-message 6 "Expiring articles...")
	(unless (gnus-check-group gnus-newsgroup-name)
	  (error "Can't open server for %s" gnus-newsgroup-name))
	;; The list of articles that weren't expired is returned.
	(save-excursion
	  (if expiry-wait
	      (let ((nnmail-expiry-wait-function nil)
		    (nnmail-expiry-wait expiry-wait))
		(setq es (gnus-request-expire-articles
			  expirable gnus-newsgroup-name)))
	    (setq es (gnus-request-expire-articles
		      expirable gnus-newsgroup-name))))
	(unless total
	  (setq gnus-newsgroup-expirable es))
	;; We go through the old list of expirable, and mark all
	;; really expired articles as nonexistent.
	(unless (eq es expirable)	;If nothing was expired, we don't mark.
	  (let ((gnus-use-cache nil))
	    (while expirable
	      (unless (memq (car expirable) es)
		(when (gnus-data-find (car expirable))
		  (gnus-summary-mark-article
		   (car expirable) gnus-canceled-mark)))
	      (setq expirable (cdr expirable)))))
	(gnus-message 6 "Expiring articles...done")))))

(defun gnus-summary-expire-articles-now ()
  "Expunge all expirable articles in the current group.
This means that *all* articles that are marked as expirable will be
deleted forever, right now."
  (interactive)
  (or gnus-expert-user
      (gnus-yes-or-no-p
       "Are you really, really, really sure you want to delete all these messages? ")
      (error "Phew!"))
  (gnus-summary-expire-articles t))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-summary-delete-article (&optional n)
  "Delete the N next (mail) articles.
This command actually deletes articles.	 This is not a marking
command.  The article will disappear forever from your life, never to
return.
If N is negative, delete backwards.
If N is nil and articles have been marked with the process mark,
delete these instead."
  (interactive "P")
  (unless (gnus-check-backend-function 'request-expire-articles
				       gnus-newsgroup-name)
    (error "The current newsgroup does not support article deletion"))
  (unless (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
    (error "Couldn't open server"))
  ;; Compute the list of articles to delete.
  (let ((articles (sort (copy-sequence (gnus-summary-work-articles n)) '<))
	not-deleted)
    (if (and gnus-novice-user
	     (not (gnus-yes-or-no-p
		   (format "Do you really want to delete %s forever? "
			   (if (> (length articles) 1)
			       (format "these %s articles" (length articles))
			     "this article")))))
	()
      ;; Delete the articles.
      (setq not-deleted (gnus-request-expire-articles
			 articles gnus-newsgroup-name 'force))
      (while articles
	(gnus-summary-remove-process-mark (car articles))
	;; The backend might not have been able to delete the article
	;; after all.
	(unless (memq (car articles) not-deleted)
	  (gnus-summary-mark-article (car articles) gnus-canceled-mark))
	(setq articles (cdr articles)))
      (when not-deleted
	(gnus-message 4 "Couldn't delete articles %s" not-deleted)))
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    not-deleted))

(defun gnus-summary-edit-article (&optional arg)
  "Edit the current article.
This will have permanent effect only in mail groups.
If ARG is nil, edit the decoded articles.
If ARG is 1, edit the raw articles. 
If ARG is 2, edit the raw articles even in read-only groups.
Otherwise, allow editing of articles even in read-only
groups."
  (interactive "P")
  (let (force raw)
    (cond 
     ((null arg))
     ((eq arg 1) (setq raw t))
     ((eq arg 2) (setq raw t
		       force t))
     (t (setq force t)))
    (if (and raw (not force) (equal gnus-newsgroup-name "nndraft:drafts"))
	(error "Can't edit the raw article in group nndraft:drafts"))
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (let ((mail-parse-charset gnus-newsgroup-charset)
	    (mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets))
	(gnus-set-global-variables)
	(when (and (not force)
		   (gnus-group-read-only-p))
	  (error "The current newsgroup does not support article editing"))
	(gnus-summary-show-article t)
	(when (and (not raw) (gnus-buffer-live-p gnus-article-buffer))
	  (with-current-buffer gnus-article-buffer
	    (mm-enable-multibyte-mule4)))
	(if (equal gnus-newsgroup-name "nndraft:drafts")
	    (setq raw t))
	(gnus-article-edit-article
	 (if raw 'ignore 
	   #'(lambda () 
	       (let ((mbl mml-buffer-list))
		 (setq mml-buffer-list nil)
		 (mime-to-mml)
		 (make-local-hook 'kill-buffer-hook)
		 (let ((mml-buffer-list mml-buffer-list))
		   (setq mml-buffer-list mbl)
		   (make-local-variable 'mml-buffer-list))
		 (add-hook 'kill-buffer-hook 'mml-destroy-buffers t t))))
	 `(lambda (no-highlight)
	    (let ((mail-parse-charset ',gnus-newsgroup-charset)
		  (mail-parse-ignored-charsets 
		   ',gnus-newsgroup-ignored-charsets))
	      ,(if (not raw) '(progn 
				(mml-to-mime)
				(mml-destroy-buffers)
				(remove-hook 'kill-buffer-hook 
					     'mml-destroy-buffers t)
				(kill-local-variable 'mml-buffer-list)))
	      (gnus-summary-edit-article-done
	       ,(or (mail-header-references gnus-current-headers) "")
	       ,(gnus-group-read-only-p) 
	       ,gnus-summary-buffer no-highlight))))))))

(defalias 'gnus-summary-edit-article-postpone 'gnus-article-edit-exit)

(defun gnus-summary-edit-article-done (&optional references read-only buffer
						 no-highlight)
  "Make edits to the current article permanent."
  (interactive)
  ;; Replace the article.
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (if (and (not read-only)
	       (not (gnus-request-replace-article
		     (cdr gnus-article-current) (car gnus-article-current)
		     (current-buffer) t)))
	  (error "Couldn't replace article")
	;; Update the summary buffer.
	(if (and references
		 (equal (message-tokenize-header references " ")
			(message-tokenize-header
			 (or (message-fetch-field "references") "") " ")))
	    ;; We only have to update this line.
	    (save-excursion
	      (save-restriction
		(message-narrow-to-head)
		(let ((head (buffer-string))
		      header)
		  (with-temp-buffer
		    (insert (format "211 %d Article retrieved.\n"
				    (cdr gnus-article-current)))
		    (insert head)
		    (insert ".\n")
		    (let ((nntp-server-buffer (current-buffer)))
		      (setq header (car (gnus-get-newsgroup-headers
					 (save-excursion
					   (set-buffer gnus-summary-buffer)
					   gnus-newsgroup-dependencies)
					 t))))
		    (save-excursion
		      (set-buffer gnus-summary-buffer)
		      (gnus-data-set-header
		       (gnus-data-find (cdr gnus-article-current))
		       header)
		      (gnus-summary-update-article-line
		       (cdr gnus-article-current) header))))))
	  ;; Update threads.
	  (set-buffer (or buffer gnus-summary-buffer))
	  (gnus-summary-update-article (cdr gnus-article-current)))
	;; Prettify the article buffer again.
	(unless no-highlight
	  (save-excursion
	    (set-buffer gnus-article-buffer)
	    ;;;!!! Fix this -- article should be rehighlighted.
	    ;;;(gnus-run-hooks 'gnus-article-display-hook)
	    (set-buffer gnus-original-article-buffer)
	    (gnus-request-article
	     (cdr gnus-article-current)
	     (car gnus-article-current) (current-buffer))))
	;; Prettify the summary buffer line.
	(when (gnus-visual-p 'summary-highlight 'highlight)
	  (gnus-run-hooks 'gnus-visual-mark-article-hook))))))

(defun gnus-summary-edit-wash (key)
  "Perform editing command KEY in the article buffer."
  (interactive
   (list
    (progn
      (message "%s" (concat (this-command-keys) "- "))
      (read-char))))
  (message "")
  (gnus-summary-edit-article)
  (execute-kbd-macro (concat (this-command-keys) key))
  (gnus-article-edit-done))

;;; Respooling

(defun gnus-summary-respool-query (&optional silent trace)
  "Query where the respool algorithm would put this article."
  (interactive)
  (let (gnus-mark-article-hook)
    (gnus-summary-select-article)
    (save-excursion
      (set-buffer gnus-original-article-buffer)
      (save-restriction
	(message-narrow-to-head)
	(let ((groups (nnmail-article-group 'identity trace)))
	  (unless silent
	    (if groups
		(message "This message would go to %s"
			 (mapconcat 'car groups ", "))
	      (message "This message would go to no groups"))
	    groups))))))

(defun gnus-summary-respool-trace ()
  "Trace where the respool algorithm would put this article.
Display a buffer showing all fancy splitting patterns which matched."
  (interactive)
  (gnus-summary-respool-query nil t))

;; Summary marking commands.

(defun gnus-summary-kill-same-subject-and-select (&optional unmark)
  "Mark articles which has the same subject as read, and then select the next.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; Select next unread article.  If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-summary-next-article t (and gnus-auto-select-same
				      (gnus-summary-article-subject)))
    (gnus-message 7 "%d article%s marked as %s"
		  count (if (= count 1) " is" "s are")
		  (if unmark "unread" "read"))))

(defun gnus-summary-kill-same-subject (&optional unmark)
  "Mark articles which has the same subject as read.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; If marked as read, go to next unread subject.
    (when (null unmark)
      ;; Go to next unread subject.
      (gnus-summary-next-subject 1 t))
    (gnus-message 7 "%d articles are marked as %s"
		  count (if unmark "unread" "read"))))

(defun gnus-summary-mark-same-subject (subject &optional unmark)
  "Mark articles with same SUBJECT as read, and return marked number.
If optional argument UNMARK is positive, remove any kinds of marks.
If optional argument UNMARK is negative, mark articles as unread instead."
  (let ((count 1))
    (save-excursion
      (cond
       ((null unmark)			; Mark as read.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-read gnus-killed-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count))))
       ((> unmark 0)			; Tick.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-ticked-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count))))
       (t				; Mark as unread.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-unread-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count)))))
      (gnus-set-mode-line 'summary)
      ;; Return the number of marked articles.
      count)))

(defun gnus-summary-mark-as-processable (n &optional unmark)
  "Set the process mark on the next N articles.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the process mark instead.  The difference between N and the actual
number of articles marked is returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and
	    (> n 0)
	    (if unmark
		(gnus-summary-remove-process-mark
		 (gnus-summary-article-number))
	      (gnus-summary-set-process-mark (gnus-summary-article-number)))
	    (zerop (gnus-summary-next-subject (if backward -1 1) nil t)))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    n))

(defun gnus-summary-unmark-as-processable (n)
  "Remove the process mark from the next N articles.
If N is negative, unmark backward instead.  The difference between N and
the actual number of articles unmarked is returned."
  (interactive "p")
  (gnus-summary-mark-as-processable n t))

(defun gnus-summary-unmark-all-processable ()
  "Remove the process mark from all articles."
  (interactive)
  (save-excursion
    (while gnus-newsgroup-processable
      (gnus-summary-remove-process-mark (car gnus-newsgroup-processable))))
  (gnus-summary-position-point))

(defun gnus-summary-mark-as-expirable (n)
  "Mark N articles forward as expirable.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-expirable-mark))

(defun gnus-summary-mark-article-as-replied (article)
  "Mark ARTICLE replied and update the summary line."
  (push article gnus-newsgroup-replied)
  (let ((buffer-read-only nil))
    (when (gnus-summary-goto-subject article nil t)
      (gnus-summary-update-secondary-mark article))))

(defun gnus-summary-set-bookmark (article)
  "Set a bookmark in current article."
  (interactive (list (gnus-summary-article-number)))
  (when (or (not (get-buffer gnus-article-buffer))
	    (not gnus-current-article)
	    (not gnus-article-current)
	    (not (equal gnus-newsgroup-name (car gnus-article-current))))
    (error "No current article selected"))
  ;; Remove old bookmark, if one exists.
  (let ((old (assq article gnus-newsgroup-bookmarks)))
    (when old
      (setq gnus-newsgroup-bookmarks
	    (delq old gnus-newsgroup-bookmarks))))
  ;; Set the new bookmark, which is on the form
  ;; (article-number . line-number-in-body).
  (push
   (cons article
	 (save-excursion
	   (set-buffer gnus-article-buffer)
	   (count-lines
	    (min (point)
		 (save-excursion
		   (goto-char (point-min))
		   (search-forward "\n\n" nil t)
		   (point)))
	    (point))))
   gnus-newsgroup-bookmarks)
  (gnus-message 6 "A bookmark has been added to the current article."))

(defun gnus-summary-remove-bookmark (article)
  "Remove the bookmark from the current article."
  (interactive (list (gnus-summary-article-number)))
  ;; Remove old bookmark, if one exists.
  (let ((old (assq article gnus-newsgroup-bookmarks)))
    (if old
	(progn
	  (setq gnus-newsgroup-bookmarks
		(delq old gnus-newsgroup-bookmarks))
	  (gnus-message 6 "Removed bookmark."))
      (gnus-message 6 "No bookmark in current article."))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-mark-as-dormant (n)
  "Mark N articles forward as dormant.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-dormant-mark))

(defun gnus-summary-set-process-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable
	(cons article
	      (delq article gnus-newsgroup-processable)))
  (when (gnus-summary-goto-subject article)
    (gnus-summary-show-thread)
    (gnus-summary-goto-subject article)
    (gnus-summary-update-secondary-mark article)))

(defun gnus-summary-remove-process-mark (article)
  "Remove the process mark from ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (delq article gnus-newsgroup-processable))
  (when (gnus-summary-goto-subject article)
    (gnus-summary-show-thread)
    (gnus-summary-goto-subject article)
    (gnus-summary-update-secondary-mark article)))

(defun gnus-summary-set-saved-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (push article gnus-newsgroup-saved)
  (when (gnus-summary-goto-subject article)
    (gnus-summary-update-secondary-mark article)))

(defun gnus-summary-mark-forward (n &optional mark no-expire)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.  Mark with MARK, ?r by default.
The difference between N and the actual number of articles marked is
returned.
Iff NO-EXPIRE, auto-expiry will be inhibited."
  (interactive "p")
  (gnus-summary-show-thread)
  (let ((backward (< n 0))
	(gnus-summary-goto-unread
	 (and gnus-summary-goto-unread
	      (not (eq gnus-summary-goto-unread 'never))
	      (not (memq mark (list gnus-unread-mark
				    gnus-ticked-mark gnus-dormant-mark)))))
	(n (abs n))
	(mark (or mark gnus-del-mark)))
    (while (and (> n 0)
		(gnus-summary-mark-article nil mark no-expire)
		(zerop (gnus-summary-next-subject
			(if backward -1 1)
			(and gnus-summary-goto-unread
			     (not (eq gnus-summary-goto-unread 'never)))
			t)))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(defun gnus-summary-mark-article-as-read (mark)
  "Mark the current article quickly as read with MARK."
  (let ((article (gnus-summary-article-number)))
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (push (cons article mark) gnus-newsgroup-reads)
    ;; Possibly remove from cache, if that is used.
    (when gnus-use-cache
      (gnus-cache-enter-remove-article article))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    ;; Check for auto-expiry.
    (when (and gnus-newsgroup-auto-expire
	       (memq mark gnus-auto-expirable-marks))
      (setq mark gnus-expirable-mark)
      ;; Let the backend know about the mark change.
      (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
      (push article gnus-newsgroup-expirable))
    ;; Set the mark in the buffer.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article-as-unread (mark)
  "Mark the current article quickly as unread with MARK."
  (let* ((article (gnus-summary-article-number))
	 (old-mark (gnus-summary-article-mark article)))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    (if (eq mark old-mark)
	t
      (if (<= article 0)
	  (progn
	    (gnus-error 1 "Can't mark negative article numbers")
	    nil)
	(setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
	(setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
	(setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
	(setq gnus-newsgroup-reads (delq article gnus-newsgroup-reads))
	(cond ((= mark gnus-ticked-mark)
	       (push article gnus-newsgroup-marked))
	      ((= mark gnus-dormant-mark)
	       (push article gnus-newsgroup-dormant))
	      (t
	       (push article gnus-newsgroup-unreads)))
	(gnus-pull article gnus-newsgroup-reads)

	;; See whether the article is to be put in the cache.
	(and gnus-use-cache
	     (vectorp (gnus-summary-article-header article))
	     (save-excursion
	       (gnus-cache-possibly-enter-article
		gnus-newsgroup-name article
		(= mark gnus-ticked-mark)
		(= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

	;; Fix the mark.
	(gnus-summary-update-mark mark 'unread)
	t))))

(defun gnus-summary-mark-article (&optional article mark no-expire)
  "Mark ARTICLE with MARK.  MARK can be any character.
Four MARK strings are reserved: `? ' (unread), `?!' (ticked),
`??' (dormant) and `?E' (expirable).
If MARK is nil, then the default character `?r' is used.
If ARTICLE is nil, then the article on the current line will be
marked.
Iff NO-EXPIRE, auto-expiry will be inhibited."
  ;; The mark might be a string.
  (when (stringp mark)
    (setq mark (aref mark 0)))
  ;; If no mark is given, then we check auto-expiring.
  (when (null mark)
    (setq mark gnus-del-mark))
  (when (and (not no-expire)
	     gnus-newsgroup-auto-expire
 	     (memq mark gnus-auto-expirable-marks))
    (setq mark gnus-expirable-mark))
  (let ((article (or article (gnus-summary-article-number)))
	(old-mark (gnus-summary-article-mark article)))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    (if (eq mark old-mark)
	t
      (unless article
	(error "No article on current line"))
      (if (not (if (or (= mark gnus-unread-mark)
		       (= mark gnus-ticked-mark)
		       (= mark gnus-dormant-mark))
		   (gnus-mark-article-as-unread article mark)
		 (gnus-mark-article-as-read article mark)))
	  t
	;; See whether the article is to be put in the cache.
	(and gnus-use-cache
	     (not (= mark gnus-canceled-mark))
	     (vectorp (gnus-summary-article-header article))
	     (save-excursion
	       (gnus-cache-possibly-enter-article
		gnus-newsgroup-name article
		(= mark gnus-ticked-mark)
		(= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

	(when (gnus-summary-goto-subject article nil t)
	  (let ((buffer-read-only nil))
	    (gnus-summary-show-thread)
	    ;; Fix the mark.
	    (gnus-summary-update-mark mark 'unread)
	    t))))))

(defun gnus-summary-update-secondary-mark (article)
  "Update the secondary (read, process, cache) mark."
  (gnus-summary-update-mark
   (cond ((memq article gnus-newsgroup-processable)
	  gnus-process-mark)
	 ((memq article gnus-newsgroup-cached)
	  gnus-cached-mark)
	 ((memq article gnus-newsgroup-replied)
	  gnus-replied-mark)
	 ((memq article gnus-newsgroup-saved)
	  gnus-saved-mark)
	 (t gnus-unread-mark))
   'replied)
  (when (gnus-visual-p 'summary-highlight 'highlight)
    (gnus-run-hooks 'gnus-summary-update-hook))
  t)

(defun gnus-summary-update-mark (mark type)
  (let ((forward (cdr (assq type gnus-summary-mark-positions)))
        (buffer-read-only nil))
    (re-search-backward "[\n\r]" (gnus-point-at-bol) 'move-to-limit)
    (when forward
      (when (looking-at "\r")
	(incf forward))
      (when (<= (+ forward (point)) (point-max))
	;; Go to the right position on the line.
	(goto-char (+ forward (point)))
	;; Replace the old mark with the new mark.
	(subst-char-in-region (point) (1+ (point)) (char-after) mark)
	;; Optionally update the marks by some user rule.
	(when (eq type 'unread)
	  (gnus-data-set-mark
	   (gnus-data-find (gnus-summary-article-number)) mark)
	  (gnus-summary-update-line (eq mark gnus-unread-mark)))))))

(defun gnus-mark-article-as-read (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  ;; Make the article expirable.
  (let ((mark (or mark gnus-del-mark)))
    (if (= mark gnus-expirable-mark)
	(push article gnus-newsgroup-expirable)
      (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable)))
    ;; Remove from unread and marked lists.
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (push (cons article mark) gnus-newsgroup-reads)
    ;; Possibly remove from cache, if that is used.
    (when gnus-use-cache
      (gnus-cache-enter-remove-article article))
    t))

(defun gnus-mark-article-as-unread (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  (let ((mark (or mark gnus-ticked-mark)))
    (if (<= article 0)
	(progn
	  (gnus-error 1 "Can't mark negative article numbers")
	  nil)
      (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked)
	    gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant)
	    gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable)
	    gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))

      ;; Unsuppress duplicates?
      (when gnus-suppress-duplicates
	(gnus-dup-unsuppress-article article))

      (cond ((= mark gnus-ticked-mark)
	     (push article gnus-newsgroup-marked))
	    ((= mark gnus-dormant-mark)
	     (push article gnus-newsgroup-dormant))
	    (t
	     (push article gnus-newsgroup-unreads)))
      (gnus-pull article gnus-newsgroup-reads)
      t)))

(defalias 'gnus-summary-mark-as-unread-forward
  'gnus-summary-tick-article-forward)
(make-obsolete 'gnus-summary-mark-as-unread-forward
	       'gnus-summary-tick-article-forward)
(defun gnus-summary-tick-article-forward (n)
  "Tick N articles forwards.
If N is negative, tick backwards instead.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-ticked-mark))

(defalias 'gnus-summary-mark-as-unread-backward
  'gnus-summary-tick-article-backward)
(make-obsolete 'gnus-summary-mark-as-unread-backward
	       'gnus-summary-tick-article-backward)
(defun gnus-summary-tick-article-backward (n)
  "Tick N articles backwards.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-ticked-mark))

(defalias 'gnus-summary-mark-as-unread 'gnus-summary-tick-article)
(make-obsolete 'gnus-summary-mark-as-unread 'gnus-summary-tick-article)
(defun gnus-summary-tick-article (&optional article clear-mark)
  "Mark current article as unread.
Optional 1st argument ARTICLE specifies article number to be marked as unread.
Optional 2nd argument CLEAR-MARK remove any kinds of mark."
  (interactive)
  (gnus-summary-mark-article article (if clear-mark gnus-unread-mark
				       gnus-ticked-mark)))

(defun gnus-summary-mark-as-read-forward (n)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-del-mark gnus-inhibit-user-auto-expire))

(defun gnus-summary-mark-as-read-backward (n)
  "Mark the N articles as read backwards.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward
   (- n) gnus-del-mark gnus-inhibit-user-auto-expire))

(defun gnus-summary-mark-as-read (&optional article mark)
  "Mark current article as read.
ARTICLE specifies the article to be marked as read.
MARK specifies a string to be inserted at the beginning of the line."
  (gnus-summary-mark-article article mark))

(defun gnus-summary-clear-mark-forward (n)
  "Clear marks from N articles forward.
If N is negative, clear backward instead.
The difference between N and the number of marks cleared is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-unread-mark))

(defun gnus-summary-clear-mark-backward (n)
  "Clear marks from N articles backward.
The difference between N and the number of marks cleared is returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-unread-mark))

(defun gnus-summary-mark-unread-as-read ()
  "Intended to be used by `gnus-summary-mark-article-hook'."
  (when (memq gnus-current-article gnus-newsgroup-unreads)
    (gnus-summary-mark-article gnus-current-article gnus-read-mark)))

(defun gnus-summary-mark-read-and-unread-as-read ()
  "Intended to be used by `gnus-summary-mark-article-hook'."
  (let ((mark (gnus-summary-article-mark)))
    (when (or (gnus-unread-mark-p mark)
	      (gnus-read-mark-p mark))
      (gnus-summary-mark-article gnus-current-article gnus-read-mark))))

(defun gnus-summary-mark-region-as-read (point mark all)
  "Mark all unread articles between point and mark as read.
If given a prefix, mark all articles between point and mark as read,
even ticked and dormant ones."
  (interactive "r\nP")
  (save-excursion
    (let (article)
      (goto-char point)
      (beginning-of-line)
      (while (and
	      (< (point) mark)
	      (progn
		(when (or all
			  (memq (setq article (gnus-summary-article-number))
				gnus-newsgroup-unreads))
		  (gnus-summary-mark-article article gnus-del-mark))
		t)
	      (gnus-summary-find-next))))))

(defun gnus-summary-mark-below (score mark)
  "Mark articles with score less than SCORE with MARK."
  (interactive "P\ncMark: ")
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while
	(progn
	  (and (< (gnus-summary-article-score) score)
	       (gnus-summary-mark-article nil mark))
	  (gnus-summary-find-next)))))

(defun gnus-summary-kill-below (&optional score)
  "Mark articles with score below SCORE as read."
  (interactive "P")
  (gnus-summary-mark-below score gnus-killed-mark))

(defun gnus-summary-clear-above (&optional score)
  "Clear all marks from articles with score above SCORE."
  (interactive "P")
  (gnus-summary-mark-above score gnus-unread-mark))

(defun gnus-summary-tick-above (&optional score)
  "Tick all articles with score above SCORE."
  (interactive "P")
  (gnus-summary-mark-above score gnus-ticked-mark))

(defun gnus-summary-mark-above (score mark)
  "Mark articles with score over SCORE with MARK."
  (interactive "P\ncMark: ")
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while (and (progn
		  (when (> (gnus-summary-article-score) score)
		    (gnus-summary-mark-article nil mark))
		  t)
		(gnus-summary-find-next)))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defalias 'gnus-summary-show-all-expunged 'gnus-summary-limit-include-expunged)
(defun gnus-summary-limit-include-expunged (&optional no-error)
  "Display all the hidden articles that were expunged for low scores."
  (interactive)
  (let ((buffer-read-only nil))
    (let ((scored gnus-newsgroup-scored)
	  headers h)
      (while scored
	(unless (gnus-summary-goto-subject (caar scored))
	  (and (setq h (gnus-summary-article-header (caar scored)))
	       (< (cdar scored) gnus-summary-expunge-below)
	       (push h headers)))
	(setq scored (cdr scored)))
      (if (not headers)
	  (when (not no-error)
	    (error "No expunged articles hidden"))
	(goto-char (point-min))
	(gnus-summary-prepare-unthreaded (nreverse headers))
	(goto-char (point-min))
	(gnus-summary-position-point)
	t))))

(defun gnus-summary-catchup (&optional all quietly to-here not-mark)
  "Mark all unread articles in this newsgroup as read.
If prefix argument ALL is non-nil, ticked and dormant articles will
also be marked as read.
If QUIETLY is non-nil, no questions will be asked.
If TO-HERE is non-nil, it should be a point in the buffer.  All
articles before this point will be marked as read.
Note that this function will only catch up the unread article
in the current summary buffer limitation.
The number of articles marked as read is returned."
  (interactive "P")
  (prog1
      (save-excursion
	(when (or quietly
		  (not gnus-interactive-catchup) ;Without confirmation?
		  gnus-expert-user
		  (gnus-y-or-n-p
		   (if all
		       "Mark absolutely all articles as read? "
		     "Mark all unread articles as read? ")))
	  (if (and not-mark
		   (not gnus-newsgroup-adaptive)
		   (not gnus-newsgroup-auto-expire)
		   (not gnus-suppress-duplicates)
		   (or (not gnus-use-cache)
		       (eq gnus-use-cache 'passive)))
	      (progn
		(when all
		  (setq gnus-newsgroup-marked nil
			gnus-newsgroup-dormant nil))
		(setq gnus-newsgroup-unreads gnus-newsgroup-downloadable))
	    ;; We actually mark all articles as canceled, which we
	    ;; have to do when using auto-expiry or adaptive scoring.
	    (gnus-summary-show-all-threads)
	    (when (gnus-summary-first-subject (not all) t)
	      (while (and
		      (if to-here (< (point) to-here) t)
		      (gnus-summary-mark-article-as-read gnus-catchup-mark)
		      (gnus-summary-find-next (not all) nil nil t))))
	    (gnus-set-mode-line 'summary))
	  t))
    (gnus-summary-position-point)))

(defun gnus-summary-catchup-to-here (&optional all)
  "Mark all unticked articles before the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive "P")
  (save-excursion
    (gnus-save-hidden-threads
      (let ((beg (point)))
	;; We check that there are unread articles.
	(when (or all (gnus-summary-find-prev))
	  (gnus-summary-catchup all t beg)))))
  (gnus-summary-position-point))

(defun gnus-summary-catchup-all (&optional quietly)
  "Mark all articles in this newsgroup as read."
  (interactive "P")
  (gnus-summary-catchup t quietly))

(defun gnus-summary-catchup-and-exit (&optional all quietly)
  "Mark all unread articles in this group as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (when (gnus-summary-catchup all quietly nil 'fast)
    ;; Select next newsgroup or exit.
    (if (and (not (gnus-group-quit-config gnus-newsgroup-name))
	     (eq gnus-auto-select-next 'quietly))
	(gnus-summary-next-group nil)
      (gnus-summary-exit))))

(defun gnus-summary-catchup-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit."
  (interactive "P")
  (gnus-summary-catchup-and-exit t quietly))

(defun gnus-summary-catchup-and-goto-next-group (&optional all)
  "Mark all articles in this group as read and select the next group.
If given a prefix, mark all articles, unread as well as ticked, as
read."
  (interactive "P")
  (save-excursion
    (gnus-summary-catchup all))
  (gnus-summary-next-group))

;;;
;;; with article
;;;

(defmacro gnus-with-article (article &rest forms)
  "Select ARTICLE and perform FORMS in the original article buffer.
Then replace the article with the result."
  `(progn
     ;; We don't want the article to be marked as read.
     (let (gnus-mark-article-hook)
       (gnus-summary-select-article t t nil ,article))
     (set-buffer gnus-original-article-buffer)
     ,@forms
     (if (not (gnus-check-backend-function
	       'request-replace-article (car gnus-article-current)))
	 (gnus-message 5 "Read-only group; not replacing")
       (unless (gnus-request-replace-article
		,article (car gnus-article-current)
		(current-buffer) t)
	 (error "Couldn't replace article")))
     ;; The cache and backlog have to be flushed somewhat.
     (when gnus-keep-backlog
       (gnus-backlog-remove-article
	(car gnus-article-current) (cdr gnus-article-current)))
     (when gnus-use-cache
       (gnus-cache-update-article
	(car gnus-article-current) (cdr gnus-article-current)))))

(put 'gnus-with-article 'lisp-indent-function 1)
(put 'gnus-with-article 'edebug-form-spec '(form body))

;; Thread-based commands.

(defun gnus-summary-articles-in-thread (&optional article)
  "Return a list of all articles in the current thread.
If ARTICLE is non-nil, return all articles in the thread that starts
with that article."
  (let* ((article (or article (gnus-summary-article-number)))
	 (data (gnus-data-find-list article))
	 (top-level (gnus-data-level (car data)))
	 (top-subject
	  (cond ((null gnus-thread-operation-ignore-subject)
		 (gnus-simplify-subject-re
		  (mail-header-subject (gnus-data-header (car data)))))
		((eq gnus-thread-operation-ignore-subject 'fuzzy)
		 (gnus-simplify-subject-fuzzy
		  (mail-header-subject (gnus-data-header (car data)))))
		(t nil)))
	 (end-point (save-excursion
		      (if (gnus-summary-go-to-next-thread)
			  (point) (point-max))))
	 articles)
    (while (and data
		(< (gnus-data-pos (car data)) end-point))
      (when (or (not top-subject)
		(string= top-subject
			 (if (eq gnus-thread-operation-ignore-subject 'fuzzy)
			     (gnus-simplify-subject-fuzzy
			      (mail-header-subject
			       (gnus-data-header (car data))))
			   (gnus-simplify-subject-re
			    (mail-header-subject
			     (gnus-data-header (car data)))))))
	(push (gnus-data-number (car data)) articles))
      (unless (and (setq data (cdr data))
		   (> (gnus-data-level (car data)) top-level))
	(setq data nil)))
    ;; Return the list of articles.
    (nreverse articles)))

(defun gnus-summary-rethread-current ()
  "Rethread the thread the current article is part of."
  (interactive)
  (let* ((gnus-show-threads t)
	 (article (gnus-summary-article-number))
	 (id (mail-header-id (gnus-summary-article-header)))
	 (gnus-newsgroup-threads (list (gnus-id-to-thread (gnus-root-id id)))))
    (unless id
      (error "No article on the current line"))
    (gnus-rebuild-thread id)
    (gnus-summary-goto-subject article)))

(defun gnus-summary-reparent-thread ()
  "Make the current article child of the marked (or previous) article.

Note that the re-threading will only work if `gnus-thread-ignore-subject'
is non-nil or the Subject: of both articles are the same."
  (interactive)
  (unless (not (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing"))
  (unless (<= (length gnus-newsgroup-processable) 1)
    (error "No more than one article may be marked"))
  (save-window-excursion
    (let ((gnus-article-buffer " *reparent*")
	  (current-article (gnus-summary-article-number))
	  ;; First grab the marked article, otherwise one line up.
	  (parent-article (if (not (null gnus-newsgroup-processable))
			      (car gnus-newsgroup-processable)
			    (save-excursion
			      (if (eq (forward-line -1) 0)
				  (gnus-summary-article-number)
				(error "Beginning of summary buffer"))))))
      (unless (not (eq current-article parent-article))
	(error "An article may not be self-referential"))
      (let ((message-id (mail-header-id
			 (gnus-summary-article-header parent-article))))
	(unless (and message-id (not (equal message-id "")))
	  (error "No message-id in desired parent"))
	(gnus-with-article current-article
	  (save-restriction
	    (goto-char (point-min))
	    (message-narrow-to-head)
	    (if (re-search-forward "^References: " nil t)
		(progn
		  (re-search-forward "^[^ \t]" nil t)
		  (forward-line -1)
		  (end-of-line)
		  (insert " " message-id))
	      (insert "References: " message-id "\n"))))
	(set-buffer gnus-summary-buffer)
	(gnus-summary-unmark-all-processable)
	(gnus-summary-update-article current-article)
	(gnus-summary-rethread-current)
	(gnus-message 3 "Article %d is now the child of article %d"
		      current-article parent-article)))))

(defun gnus-summary-toggle-threads (&optional arg)
  "Toggle showing conversation threads.
If ARG is positive number, turn showing conversation threads on."
  (interactive "P")
  (let ((current (or (gnus-summary-article-number) gnus-newsgroup-end)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    (gnus-message 6 "Threading is now %s" (if gnus-show-threads "on" "off"))
    (gnus-summary-position-point)))

(defun gnus-summary-show-all-threads ()
  "Show all threads."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)))
  (gnus-summary-position-point))

(defun gnus-summary-show-thread ()
  "Show thread subtrees.
Returns nil if no thread was there to be shown."
  (interactive)
  (let ((buffer-read-only nil)
	(orig (point))
	;; first goto end then to beg, to have point at beg after let
	(end (progn (end-of-line) (point)))
	(beg (progn (beginning-of-line) (point))))
    (prog1
	;; Any hidden lines here?
	(search-forward "\r" end t)
      (subst-char-in-region beg end ?\^M ?\n t)
      (goto-char orig)
      (gnus-summary-position-point))))

(defun gnus-summary-hide-all-threads ()
  "Hide all thread subtrees."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (gnus-summary-hide-thread)
    (while (zerop (gnus-summary-next-thread 1 t))
      (gnus-summary-hide-thread)))
  (gnus-summary-position-point))

(defun gnus-summary-hide-thread ()
  "Hide thread subtrees.
Returns nil if no threads were there to be hidden."
  (interactive)
  (let ((buffer-read-only nil)
	(start (point))
	(article (gnus-summary-article-number)))
    (goto-char start)
    ;; Go forward until either the buffer ends or the subthread
    ;; ends.
    (when (and (not (eobp))
	       (or (zerop (gnus-summary-next-thread 1 t))
		   (goto-char (point-max))))
      (prog1
	  (if (and (> (point) start)
		   (search-backward "\n" start t))
	      (progn
		(subst-char-in-region start (point) ?\n ?\^M)
		(gnus-summary-goto-subject article))
	    (goto-char start)
	    nil)))))

(defun gnus-summary-go-to-next-thread (&optional previous)
  "Go to the same level (or less) next thread.
If PREVIOUS is non-nil, go to previous thread instead.
Return the article number moved to, or nil if moving was impossible."
  (let ((level (gnus-summary-thread-level))
	(way (if previous -1 1))
	(beg (point)))
    (forward-line way)
    (while (and (not (eobp))
		(< level (gnus-summary-thread-level)))
      (forward-line way))
    (if (eobp)
	(progn
	  (goto-char beg)
	  nil)
      (setq beg (point))
      (prog1
	  (gnus-summary-article-number)
	(goto-char beg)))))

(defun gnus-summary-next-thread (n &optional silent)
  "Go to the same level next N'th thread.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done.

If SILENT, don't output messages."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-summary-go-to-next-thread backward))
      (decf n))
    (unless silent
      (gnus-summary-position-point))
    (when (and (not silent) (/= 0 n))
      (gnus-message 7 "No more threads"))
    n))

(defun gnus-summary-prev-thread (n)
  "Go to the same level previous N'th thread.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-summary-next-thread (- n)))

(defun gnus-summary-go-down-thread ()
  "Go down one level in the current thread."
  (let ((children (gnus-summary-article-children)))
    (when children
      (gnus-summary-goto-subject (car children)))))

(defun gnus-summary-go-up-thread ()
  "Go up one level in the current thread."
  (let ((parent (gnus-summary-article-parent)))
    (when parent
      (gnus-summary-goto-subject parent))))

(defun gnus-summary-down-thread (n)
  "Go down thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (let ((up (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if up (gnus-summary-go-up-thread)
		  (gnus-summary-go-down-thread)))
      (setq n (1- n)))
    (gnus-summary-position-point)
    (when (/= 0 n)
      (gnus-message 7 "Can't go further"))
    n))

(defun gnus-summary-up-thread (n)
  "Go up thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-summary-down-thread (- n)))

(defun gnus-summary-top-thread ()
  "Go to the top of the thread."
  (interactive)
  (while (gnus-summary-go-up-thread))
  (gnus-summary-article-number))

(defun gnus-summary-kill-thread (&optional unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((articles (gnus-summary-articles-in-thread)))
    (save-excursion
      ;; Expand the thread.
      (gnus-summary-show-thread)
      ;; Mark all the articles.
      (while articles
	(gnus-summary-goto-subject (car articles))
	(cond ((null unmark)
	       (gnus-summary-mark-article-as-read gnus-killed-mark))
	      ((> unmark 0)
	       (gnus-summary-mark-article-as-unread gnus-unread-mark))
	      (t
	       (gnus-summary-mark-article-as-unread gnus-ticked-mark)))
	(setq articles (cdr articles))))
    ;; Hide killed subtrees.
    (and (null unmark)
	 gnus-thread-hide-killed
	 (gnus-summary-hide-thread))
    ;; If marked as read, go to next unread subject.
    (when (null unmark)
      ;; Go to next unread subject.
      (gnus-summary-next-subject 1 t)))
  (gnus-set-mode-line 'summary))

;; Summary sorting commands

(defun gnus-summary-sort-by-number (&optional reverse)
  "Sort the summary buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'number reverse))

(defun gnus-summary-sort-by-author (&optional reverse)
  "Sort the summary buffer by author name alphabetically.
If `case-fold-search' is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'author reverse))

(defun gnus-summary-sort-by-subject (&optional reverse)
  "Sort the summary buffer by subject alphabetically.  `Re:'s are ignored.
If `case-fold-search' is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'subject reverse))

(defun gnus-summary-sort-by-date (&optional reverse)
  "Sort the summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'date reverse))

(defun gnus-summary-sort-by-score (&optional reverse)
  "Sort the summary buffer by score.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'score reverse))

(defun gnus-summary-sort-by-lines (&optional reverse)
  "Sort the summary buffer by the number of lines.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'lines reverse))

(defun gnus-summary-sort-by-chars (&optional reverse)
  "Sort the summary buffer by article length.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'chars reverse))

(defun gnus-summary-sort (predicate reverse)
  "Sort summary buffer by PREDICATE.  REVERSE means reverse order."
  (let* ((thread (intern (format "gnus-thread-sort-by-%s" predicate)))
	 (article (intern (format "gnus-article-sort-by-%s" predicate)))
	 (gnus-thread-sort-functions
	  (if (not reverse)
	      thread
	    `(lambda (t1 t2)
	       (,thread t2 t1))))
	 (gnus-sort-gathered-threads-function
	  gnus-thread-sort-functions)
	 (gnus-article-sort-functions
	  (if (not reverse)
	      article
	    `(lambda (t1 t2)
	       (,article t2 t1))))
	 (buffer-read-only)
	 (gnus-summary-prepare-hook nil))
    ;; We do the sorting by regenerating the threads.
    (gnus-summary-prepare)
    ;; Hide subthreads if needed.
    (when (and gnus-show-threads gnus-thread-hide-subtree)
      (gnus-summary-hide-all-threads))))

;; Summary saving commands.

(defun gnus-summary-save-article (&optional n not-saved)
  "Save the current article using the default saver function.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead.
The variable `gnus-default-article-saver' specifies the saver function."
  (interactive "P")
  (let* ((articles (gnus-summary-work-articles n))
	 (save-buffer (save-excursion
			(nnheader-set-temp-buffer " *Gnus Save*")))
	 (num (length articles))
	 header file)
    (dolist (article articles)
      (setq header (gnus-summary-article-header article))
      (if (not (vectorp header))
	  ;; This is a pseudo-article.
	  (if (assq 'name header)
	      (gnus-copy-file (cdr (assq 'name header)))
	    (gnus-message 1 "Article %d is unsaveable" article))
	;; This is a real article.
	(save-window-excursion
	  (let ((gnus-display-mime-function nil)
		(gnus-article-prepare-hook nil))
	    (gnus-summary-select-article t nil nil article)))
	(save-excursion
	  (set-buffer save-buffer)
	  (erase-buffer)
	  (insert-buffer-substring gnus-original-article-buffer))
	(setq file (gnus-article-save save-buffer file num))
	(gnus-summary-remove-process-mark article)
	(unless not-saved
	  (gnus-summary-set-saved-mark article))))
    (gnus-kill-buffer save-buffer)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(defun gnus-summary-pipe-output (&optional arg)
  "Pipe the current article to a subprocess.
If N is a positive number, pipe the N next articles.
If N is a negative number, pipe the N previous articles.
If N is nil and any articles have been marked with the process mark,
pipe those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-pipe))
    (gnus-summary-save-article arg t))
  (gnus-configure-windows 'pipe))

(defun gnus-summary-save-article-mail (&optional arg)
  "Append the current article to an mail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-mail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-rmail (&optional arg)
  "Append the current article to an rmail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-rmail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-file (&optional arg)
  "Append the current article to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-write-article-file (&optional arg)
  "Write the current article to a file, deleting the previous file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-write-to-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-body-file (&optional arg)
  "Append the current article body to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-body-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-pipe-message (program)
  "Pipe the current article through PROGRAM."
  (interactive "sProgram: ")
  (gnus-summary-select-article)
  (let ((mail-header-separator ""))
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-restriction
        (widen)
        (let ((start (window-start))
              buffer-read-only)
          (message-pipe-buffer-body program)
          (set-window-start (get-buffer-window (current-buffer)) start))))))

(defun gnus-get-split-value (methods)
  "Return a value based on the split METHODS."
  (let (split-name method result match)
    (when methods
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(save-restriction
	  (nnheader-narrow-to-headers)
	  (while (and methods (not split-name))
	    (goto-char (point-min))
	    (setq method (pop methods))
	    (setq match (car method))
	    (when (cond
		   ((stringp match)
		    ;; Regular expression.
		    (ignore-errors
		      (re-search-forward match nil t)))
		   ((gnus-functionp match)
		    ;; Function.
		    (save-restriction
		      (widen)
		      (setq result (funcall match gnus-newsgroup-name))))
		   ((consp match)
		    ;; Form.
		    (save-restriction
		      (widen)
		      (setq result (eval match)))))
	      (setq split-name (cdr method))
	      (cond ((stringp result)
		     (push (expand-file-name
			    result gnus-article-save-directory)
			   split-name))
		    ((consp result)
		     (setq split-name (append result split-name)))))))))
    (nreverse split-name)))

(defun gnus-valid-move-group-p (group)
  (and (boundp group)
       (symbol-name group)
       (symbol-value group)
       (gnus-get-function (gnus-find-method-for-group
			   (symbol-name group)) 'request-accept-article t)))

(defun gnus-read-move-group-name (prompt default articles prefix)
  "Read a group name."
  (let* ((split-name (gnus-get-split-value gnus-move-split-methods))
	 (minibuffer-confirm-incomplete nil) ; XEmacs
	 (prom
	  (format "%s %s to:"
		  prompt
		  (if (> (length articles) 1)
		      (format "these %d articles" (length articles))
		    "this article")))
	 (to-newsgroup
	  (cond
	   ((null split-name)
	    (gnus-completing-read default prom
				  gnus-active-hashtb
				  'gnus-valid-move-group-p
				  nil prefix
				  'gnus-group-history))
	   ((= 1 (length split-name))
	    (gnus-completing-read (car split-name) prom
				  gnus-active-hashtb
				  'gnus-valid-move-group-p
				  nil nil
				  'gnus-group-history))
	   (t
	    (gnus-completing-read nil prom
				  (mapcar (lambda (el) (list el))
					  (nreverse split-name))
				  nil nil nil
				  'gnus-group-history))))
         (to-method (gnus-server-to-method (gnus-group-method to-newsgroup))))
    (when to-newsgroup
      (if (or (string= to-newsgroup "")
	      (string= to-newsgroup prefix))
	  (setq to-newsgroup default))
      (unless to-newsgroup
	(error "No group name entered"))
      (or (gnus-active to-newsgroup)
	  (gnus-activate-group to-newsgroup nil nil to-method)
	  (if (gnus-y-or-n-p (format "No such group: %s.  Create it? "
				     to-newsgroup))
	      (or (and (gnus-request-create-group to-newsgroup to-method)
		       (gnus-activate-group
			to-newsgroup nil nil to-method)
		       (gnus-subscribe-group to-newsgroup))
		  (error "Couldn't create group %s" to-newsgroup)))
	  (error "No such group: %s" to-newsgroup)))
    to-newsgroup))

(defun gnus-summary-save-parts (type dir n &optional reverse)
  "Save parts matching TYPE to DIR.
If REVERSE, save parts that do not match TYPE."
  (interactive
   (list (read-string "Save parts of type: " 
		      (or (car gnus-summary-save-parts-type-history)
			  gnus-summary-save-parts-default-mime)
		      'gnus-summary-save-parts-type-history)
	 (setq gnus-summary-save-parts-last-directory
	       (read-file-name "Save to directory: " 
			       gnus-summary-save-parts-last-directory
			       nil t))
	 current-prefix-arg))
  (gnus-summary-iterate n
    (let ((gnus-display-mime-function nil)
	  (gnus-inhibit-treatment t))
      (gnus-summary-select-article))
    (save-excursion
      (set-buffer gnus-article-buffer)
      (let ((handles (or gnus-article-mime-handles
			 (mm-dissect-buffer) (mm-uu-dissect))))
	(when handles
	  (gnus-summary-save-parts-1 type dir handles reverse)
	  (unless gnus-article-mime-handles ;; Don't destroy this case.
	    (mm-destroy-parts handles)))))))

(defun gnus-summary-save-parts-1 (type dir handle reverse)
  (if (stringp (car handle))
      (mapcar (lambda (h) (gnus-summary-save-parts-1 type dir h reverse))
	      (cdr handle))
    (when (if reverse
	      (not (string-match type (mm-handle-media-type handle)))
	    (string-match type (mm-handle-media-type handle)))
      (let ((file (expand-file-name
		   (file-name-nondirectory
		    (or
		     (mail-content-type-get
		      (mm-handle-disposition handle) 'filename)
		     (concat gnus-newsgroup-name
			     "." (number-to-string
				  (cdr gnus-article-current)))))
		   dir)))
	(unless (file-exists-p file)
	  (mm-save-part-to-file handle file))))))

;; Summary extract commands

(defun gnus-summary-insert-pseudos (pslist &optional not-view)
  (let ((buffer-read-only nil)
	(article (gnus-summary-article-number))
	after-article b e)
    (unless (gnus-summary-goto-subject article)
      (error "No such article: %d" article))
    (gnus-summary-position-point)
    ;; If all commands are to be bunched up on one line, we collect
    ;; them here.
    (unless gnus-view-pseudos-separately
      (let ((ps (setq pslist (sort pslist 'gnus-pseudos<)))
	    files action)
	(while ps
	  (setq action (cdr (assq 'action (car ps))))
	  (setq files (list (cdr (assq 'name (car ps)))))
	  (while (and ps (cdr ps)
		      (string= (or action "1")
			       (or (cdr (assq 'action (cadr ps))) "2")))
	    (push (cdr (assq 'name (cadr ps))) files)
	    (setcdr ps (cddr ps)))
	  (when files
	    (when (not (string-match "%s" action))
	      (push " " files))
	    (push " " files)
	    (when (assq 'execute (car ps))
	      (setcdr (assq 'execute (car ps))
		      (funcall (if (string-match "%s" action)
				   'format 'concat)
			       action
			       (mapconcat
				(lambda (f)
				  (if (equal f " ")
				      f
				    (mm-quote-arg f)))
				files " ")))))
	  (setq ps (cdr ps)))))
    (if (and gnus-view-pseudos (not not-view))
	(while pslist
	  (when (assq 'execute (car pslist))
	    (gnus-execute-command (cdr (assq 'execute (car pslist)))
				  (eq gnus-view-pseudos 'not-confirm)))
	  (setq pslist (cdr pslist)))
      (save-excursion
	(while pslist
	  (setq after-article (or (cdr (assq 'article (car pslist)))
				  (gnus-summary-article-number)))
	  (gnus-summary-goto-subject after-article)
	  (forward-line 1)
	  (setq b (point))
	  (insert "    " (file-name-nondirectory
			  (cdr (assq 'name (car pslist))))
		  ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	  (setq e (point))
	  (forward-line -1)		; back to `b'
	  (gnus-add-text-properties
	   b (1- e) (list 'gnus-number gnus-reffed-article-number
			  gnus-mouse-face-prop gnus-mouse-face))
	  (gnus-data-enter
	   after-article gnus-reffed-article-number
	   gnus-unread-mark b (car pslist) 0 (- e b))
	  (push gnus-reffed-article-number gnus-newsgroup-unreads)
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number))
	  (setq pslist (cdr pslist)))))))

(defun gnus-pseudos< (p1 p2)
  (let ((c1 (cdr (assq 'action p1)))
	(c2 (cdr (assq 'action p2))))
    (and c1 c2 (string< c1 c2))))

(defun gnus-request-pseudo-article (props)
  (cond ((assq 'execute props)
	 (gnus-execute-command (cdr (assq 'execute props)))))
  (let ((gnus-current-article (gnus-summary-article-number)))
    (gnus-run-hooks 'gnus-mark-article-hook)))

(defun gnus-execute-command (command &optional automatic)
  (save-excursion
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (setq buffer-read-only nil)
    (let ((command (if automatic command
		     (read-string "Command: " (cons command 0)))))
      (erase-buffer)
      (insert "$ " command "\n\n")
      (if gnus-view-pseudo-asynchronously
	  (start-process "gnus-execute" (current-buffer) shell-file-name
			 shell-command-switch command)
	(call-process shell-file-name nil t nil
		      shell-command-switch command)))))

;; Summary kill commands.

(defun gnus-summary-edit-global-kill (article)
  "Edit the \"global\" kill file."
  (interactive (list (gnus-summary-article-number)))
  (gnus-group-edit-global-kill article))

(defun gnus-summary-edit-local-kill ()
  "Edit a local kill file applied to the current newsgroup."
  (interactive)
  (setq gnus-current-headers (gnus-summary-article-header))
  (gnus-group-edit-local-kill
   (gnus-summary-article-number) gnus-newsgroup-name))

;;; Header reading.

(defun gnus-read-header (id &optional header)
  "Read the headers of article ID and enter them into the Gnus system."
  (let ((group gnus-newsgroup-name)
	(gnus-override-method
	 (or
	  gnus-override-method
	  (and (gnus-news-group-p gnus-newsgroup-name)
	       (car (gnus-refer-article-methods)))))
	where)
    ;; First we check to see whether the header in question is already
    ;; fetched.
    (if (stringp id)
	;; This is a Message-ID.
	(setq header (or header (gnus-id-to-header id)))
      ;; This is an article number.
      (setq header (or header (gnus-summary-article-header id))))
    (if (and header
	     (not (gnus-summary-article-sparse-p (mail-header-number header))))
	;; We have found the header.
	header
      ;; If this is a sparse article, we have to nix out its
      ;; previous entry in the thread hashtb.
      (when (and header
		 (gnus-summary-article-sparse-p (mail-header-number header)))
	(let* ((parent (gnus-parent-id (mail-header-references header)))
	       (thread (and parent (gnus-id-to-thread parent))))
	  (when thread
	    (delq (assq header thread) thread))))
      ;; We have to really fetch the header to this article.
      (save-excursion
	(set-buffer nntp-server-buffer)
	(when (setq where (gnus-request-head id group))
	  (nnheader-fold-continuation-lines)
	  (goto-char (point-max))
	  (insert ".\n")
	  (goto-char (point-min))
	  (insert "211 ")
	  (princ (cond
		  ((numberp id) id)
		  ((cdr where) (cdr where))
		  (header (mail-header-number header))
		  (t gnus-reffed-article-number))
		 (current-buffer))
	  (insert " Article retrieved.\n"))
	(if (or (not where)
		(not (setq header (car (gnus-get-newsgroup-headers nil t)))))
	    ()				; Malformed head.
	  (unless (gnus-summary-article-sparse-p (mail-header-number header))
	    (when (and (stringp id)
		       (not (string= (gnus-group-real-name group)
				     (car where))))
	      ;; If we fetched by Message-ID and the article came
	      ;; from a different group, we fudge some bogus article
	      ;; numbers for this article.
	      (mail-header-set-number header gnus-reffed-article-number))
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (decf gnus-reffed-article-number)
	      (gnus-remove-header (mail-header-number header))
	      (push header gnus-newsgroup-headers)
	      (setq gnus-current-headers header)
	      (push (mail-header-number header) gnus-newsgroup-limit)))
	  header)))))

(defun gnus-remove-header (number)
  "Remove header NUMBER from `gnus-newsgroup-headers'."
  (if (and gnus-newsgroup-headers
	   (= number (mail-header-number (car gnus-newsgroup-headers))))
      (pop gnus-newsgroup-headers)
    (let ((headers gnus-newsgroup-headers))
      (while (and (cdr headers)
		  (not (= number (mail-header-number (cadr headers)))))
	(pop headers))
      (when (cdr headers)
	(setcdr headers (cddr headers))))))

;;;
;;; summary highlights
;;;

(defun gnus-highlight-selected-summary ()
  "Highlight selected article in summary buffer."
  ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
  (when gnus-summary-selected-face
    (save-excursion
      (let* ((beg (progn (beginning-of-line) (point)))
	     (end (progn (end-of-line) (point)))
	     ;; Fix by Mike Dugan <dugan@bucrf16.bu.edu>.
	     (from (if (get-text-property beg gnus-mouse-face-prop)
		       beg
		     (or (next-single-property-change
			  beg gnus-mouse-face-prop nil end)
			 beg)))
	     (to
	      (if (= from end)
		  (- from 2)
		(or (next-single-property-change
		     from gnus-mouse-face-prop nil end)
		    end))))
	;; If no mouse-face prop on line we will have to = from = end,
	;; so we highlight the entire line instead.
	(when (= (+ to 2) from)
	  (setq from beg)
	  (setq to end))
	(if gnus-newsgroup-selected-overlay
	    ;; Move old overlay.
	    (gnus-move-overlay
	     gnus-newsgroup-selected-overlay from to (current-buffer))
	  ;; Create new overlay.
	  (gnus-overlay-put
	   (setq gnus-newsgroup-selected-overlay (gnus-make-overlay from to))
	   'face gnus-summary-selected-face))))))

;; New implementation by Christian Limpach <Christian.Limpach@nice.ch>.
(defun gnus-summary-highlight-line ()
  "Highlight current line according to `gnus-summary-highlight'."
  (let* ((list gnus-summary-highlight)
	 (p (point))
	 (end (progn (end-of-line) (point)))
	 ;; now find out where the line starts and leave point there.
	 (beg (progn (beginning-of-line) (point)))
	 (article (gnus-summary-article-number))
	 (score (or (cdr (assq (or article gnus-current-article)
			       gnus-newsgroup-scored))
		    gnus-summary-default-score 0))
	 (mark (or (gnus-summary-article-mark) gnus-unread-mark))
	 (inhibit-read-only t))
    ;; Eval the cars of the lists until we find a match.
    (let ((default gnus-summary-default-score))
      (while (and list
		  (not (eval (caar list))))
	(setq list (cdr list))))
    (let ((face (cdar list)))
      (unless (eq face (get-text-property beg 'face))
	(gnus-put-text-property-excluding-characters-with-faces
	 beg end 'face
	 (setq face (if (boundp face) (symbol-value face) face)))
	(when gnus-summary-highlight-line-function
	  (funcall gnus-summary-highlight-line-function article face))))
    (goto-char p)))

(defun gnus-update-read-articles (group unread &optional compute)
  "Update the list of read articles in GROUP."
  (let* ((active (or gnus-newsgroup-active (gnus-active group)))
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (prev 1)
	 (unread (sort (copy-sequence unread) '<))
	 read)
    (if (or (not info) (not active))
	;; There is no info on this group if it was, in fact,
	;; killed.  Gnus stores no information on killed groups, so
	;; there's nothing to be done.
	;; One could store the information somewhere temporarily,
	;; perhaps...  Hmmm...
	()
      ;; Remove any negative articles numbers.
      (while (and unread (< (car unread) 0))
	(setq unread (cdr unread)))
      ;; Remove any expired article numbers
      (while (and unread (< (car unread) (car active)))
	(setq unread (cdr unread)))
      ;; Compute the ranges of read articles by looking at the list of
      ;; unread articles.
      (while unread
	(when (/= (car unread) prev)
	  (push (if (= prev (1- (car unread))) prev
		  (cons prev (1- (car unread))))
		read))
	(setq prev (1+ (car unread)))
	(setq unread (cdr unread)))
      (when (<= prev (cdr active))
	(push (cons prev (cdr active)) read))
      (setq read (if (> (length read) 1) (nreverse read) read))
      (if compute
	  read
	(save-excursion
	  (let (setmarkundo)
	    ;; Propagate the read marks to the backend.
	    (when (gnus-check-backend-function 'request-set-mark group)
	      (let ((del (gnus-remove-from-range (gnus-info-read info) read))
		    (add (gnus-remove-from-range read (gnus-info-read info))))
		(when (or add del)
		  (unless (gnus-check-group group)
		    (error "Can't open server for %s" group))
		  (gnus-request-set-mark
		   group (delq nil (list (if add (list add 'add '(read)))
					 (if del (list del 'del '(read))))))
		  (setq setmarkundo
			`(gnus-request-set-mark
			  ,group
			  ',(delq nil (list
				       (if del (list del 'add '(read)))
				       (if add (list add 'del '(read))))))))))
	    (set-buffer gnus-group-buffer)
	    (gnus-undo-register
	      `(progn
		 (gnus-info-set-marks ',info ',(gnus-info-marks info) t)
		 (gnus-info-set-read ',info ',(gnus-info-read info))
		 (gnus-get-unread-articles-in-group ',info 
						    (gnus-active ,group))
		 (gnus-group-update-group ,group t)
		 ,setmarkundo))))
	;; Enter this list into the group info.
	(gnus-info-set-read info read)
	;; Set the number of unread articles in gnus-newsrc-hashtb.
	(gnus-get-unread-articles-in-group info (gnus-active group))
	t))))

(defun gnus-offer-save-summaries ()
  "Offer to save all active summary buffers."
  (save-excursion
    (let ((buflist (buffer-list))
	  buffers bufname)
      ;; Go through all buffers and find all summaries.
      (while buflist
	(and (setq bufname (buffer-name (car buflist)))
	     (string-match "Summary" bufname)
	     (save-excursion
	       (set-buffer bufname)
	       ;; We check that this is, indeed, a summary buffer.
	       (and (eq major-mode 'gnus-summary-mode)
		    ;; Also make sure this isn't bogus.
		    gnus-newsgroup-prepared
		    ;; Also make sure that this isn't a dead summary buffer.
		    (not gnus-dead-summary-mode)))
	     (push bufname buffers))
	(setq buflist (cdr buflist)))
      ;; Go through all these summary buffers and offer to save them.
      (when buffers
	(map-y-or-n-p
	 "Update summary buffer %s? "
	 (lambda (buf)
	   (switch-to-buffer buf)
	   (gnus-summary-exit))
	 buffers)))))

(defun gnus-summary-setup-default-charset ()
  "Setup newsgroup default charset."
  (if (equal gnus-newsgroup-name "nndraft:drafts")
      (setq gnus-newsgroup-charset nil)
    (let* ((name (and gnus-newsgroup-name
		      (gnus-group-real-name gnus-newsgroup-name)))
	   (ignored-charsets
	    (or gnus-newsgroup-ephemeral-ignored-charsets
		(append
		 (and gnus-newsgroup-name
		      (or (gnus-group-find-parameter gnus-newsgroup-name
						     'ignored-charsets t)
			  (let ((alist gnus-group-ignored-charsets-alist)
				elem (charsets nil))
			    (while (setq elem (pop alist))
			      (when (and name
					 (string-match (car elem) name))
				(setq alist nil
				      charsets (cdr elem))))
			    charsets)))
		 gnus-newsgroup-ignored-charsets))))
      (setq gnus-newsgroup-charset
	    (or gnus-newsgroup-ephemeral-charset
		(and gnus-newsgroup-name
		     (or (gnus-group-find-parameter gnus-newsgroup-name 'charset)
			 (let ((alist gnus-group-charset-alist)
			       elem charset)
			   (while (setq elem (pop alist))
			     (when (and name
					(string-match (car elem) name))
			       (setq alist nil
				     charset (cadr elem))))
			   charset)))
		gnus-default-charset))
      (set (make-local-variable 'gnus-newsgroup-ignored-charsets)
	   ignored-charsets))))

;;;
;;; Mime Commands
;;;

(defun gnus-summary-display-buttonized (&optional show-all-parts)
  "Display the current article buffer fully MIME-buttonized.
If SHOW-ALL-PARTS (the prefix) is non-nil, all multipart/* parts are
treated as multipart/mixed."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-unbuttonized-mime-types nil)
	(gnus-mime-display-multipart-as-mixed show-all-parts))
    (gnus-summary-show-article)))

(defun gnus-summary-repair-multipart (article)
  "Add a Content-Type header to a multipart article without one."
  (interactive (list (gnus-summary-article-number)))
  (gnus-with-article article
    (message-narrow-to-head)
    (goto-char (point-max))
    (widen)
    (when (search-forward "\n--" nil t)
      (let ((separator (buffer-substring (point) (gnus-point-at-eol))))
	(message-narrow-to-head)
	(message-remove-header "Mime-Version")
	(message-remove-header "Content-Type")
	(goto-char (point-max))
	(insert (format "Content-Type: multipart/mixed; boundary=\"%s\"\n"
			separator))
	(insert "Mime-Version: 1.0\n")
	(widen))))
  (let (gnus-mark-article-hook)
    (gnus-summary-select-article t t nil article)))

(defun gnus-summary-toggle-display-buttonized ()
  "Toggle the buttonizing of the article buffer."
  (interactive)
  (require 'gnus-art)
  (if (setq gnus-inhibit-mime-unbuttonizing
	    (not gnus-inhibit-mime-unbuttonizing))
      (let ((gnus-unbuttonized-mime-types nil))
	(gnus-summary-show-article))
    (gnus-summary-show-article)))

;;;
;;; Generic summary marking commands
;;;

(defvar gnus-summary-marking-alist
  '((read gnus-del-mark "d")
    (unread gnus-unread-mark "u")
    (ticked gnus-ticked-mark "!")
    (dormant gnus-dormant-mark "?")
    (expirable gnus-expirable-mark "e"))
  "An alist of names/marks/keystrokes.")

(defvar gnus-summary-generic-mark-map (make-sparse-keymap))
(defvar gnus-summary-mark-map)

(defun gnus-summary-make-all-marking-commands ()
  (define-key gnus-summary-mark-map "M" gnus-summary-generic-mark-map)
  (dolist (elem gnus-summary-marking-alist)
    (apply 'gnus-summary-make-marking-command elem)))

(defun gnus-summary-make-marking-command (name mark keystroke)
  (let ((map (make-sparse-keymap)))
    (define-key gnus-summary-generic-mark-map keystroke map)
    (dolist (lway `((next "next" next nil "n")
		    (next-unread "next unread" next t "N")
		    (prev "previous" prev nil "p")
		    (prev-unread "previous unread" prev t "P")
		    (nomove "" nil nil ,keystroke)))
      (let ((func (gnus-summary-make-marking-command-1
		   mark (car lway) lway name)))
	(setq func (eval func))
	(define-key map (nth 4 lway) func)))))
      
(defun gnus-summary-make-marking-command-1 (mark way lway name)
  `(defun ,(intern
	    (format "gnus-summary-put-mark-as-%s%s"
		    name (if (eq way 'nomove)
			     ""
			   (concat "-" (symbol-name way)))))
     (n)
     ,(format
       "Mark the current article as %s%s.
If N, the prefix, then repeat N times.
If N is negative, move in reverse order.
The difference between N and the actual number of articles marked is
returned."
       name (cadr lway))
     (interactive "p")
     (gnus-summary-generic-mark n ,mark ',(nth 2 lway) ,(nth 3 lway))))
    
(defun gnus-summary-generic-mark (n mark move unread)
  "Mark N articles with MARK."
  (unless (eq major-mode 'gnus-summary-mode)
    (error "This command can only be used in the summary buffer"))
  (gnus-summary-show-thread)
  (let ((nummove
	 (cond
	  ((eq move 'next) 1)
	  ((eq move 'prev) -1)
	  (t 0))))
    (if (zerop nummove)
	(setq n 1)
      (when (< n 0)
	(setq n (abs n)
	      nummove (* -1 nummove))))
    (while (and (> n 0)
		(gnus-summary-mark-article nil mark)
		(zerop (gnus-summary-next-subject nummove unread t)))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(gnus-summary-make-all-marking-commands)

(gnus-ems-redefine)

(provide 'gnus-sum)

(run-hooks 'gnus-sum-load-hook)

;;; gnus-sum.el ends here
