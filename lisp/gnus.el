;;; gnus.el --- a newsreader for GNU Emacs
;; Copyright (C) 1987,88,89,90,93,94,95,96 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

(eval '(run-hooks 'gnus-load-hook))

(require 'mail-utils)
(require 'timezone)
(require 'nnheader)
(require 'nnmail)
(require 'backquote)
(require 'nnoo)

(eval-when-compile (require 'cl))

(defvar gnus-directory (or (getenv "SAVEDIR") "~/News/")
  "*Directory variable from which all other Gnus file variables are derived.")

;; Site dependent variables.  These variables should be defined in
;; paths.el.

(defvar gnus-default-nntp-server nil
  "Specify a default NNTP server.
This variable should be defined in paths.el, and should never be set
by the user.
If you want to change servers, you should use `gnus-select-method'.
See the documentation to that variable.")

(defvar gnus-backup-default-subscribed-newsgroups
  '("news.announce.newusers" "news.groups.questions" "gnu.emacs.gnus")
  "Default default new newsgroups the first time Gnus is run.
Should be set in paths.el, and shouldn't be touched by the user.")

(defvar gnus-local-domain nil
  "Local domain name without a host name.
The DOMAINNAME environment variable is used instead if it is defined.
If the `system-name' function returns the full Internet name, there is
no need to set this variable.")

(defvar gnus-local-organization nil
  "String with a description of what organization (if any) the user belongs to.
The ORGANIZATION environment variable is used instead if it is defined.
If this variable contains a function, this function will be called
with the current newsgroup name as the argument.  The function should
return a string.

In any case, if the string (either in the variable, in the environment
variable, or returned by the function) is a file name, the contents of
this file will be used as the organization.")

;; Customization variables

;; Don't touch this variable.
(defvar gnus-nntp-service "nntp"
  "*NNTP service name (\"nntp\" or 119).
This is an obsolete variable, which is scarcely used.  If you use an
nntp server for your newsgroup and want to change the port number
used to 899, you would say something along these lines:

 (setq gnus-select-method '(nntp \"my.nntp.server\" (nntp-port-number 899)))")

(defvar gnus-nntpserver-file "/etc/nntpserver"
  "*A file with only the name of the nntp server in it.")

;; This function is used to check both the environment variable
;; NNTPSERVER and the /etc/nntpserver file to see whether one can find
;; an nntp server name default.
(defun gnus-getenv-nntpserver ()
  (or (getenv "NNTPSERVER")
      (and (file-readable-p gnus-nntpserver-file)
	   (save-excursion
	     (set-buffer (get-buffer-create " *gnus nntp*"))
	     (buffer-disable-undo (current-buffer))
	     (insert-file-contents gnus-nntpserver-file)
	     (let ((name (buffer-string)))
	       (prog1
		   (if (string-match "^[ \t\n]*$" name)
		       nil
		     name)
		 (kill-buffer (current-buffer))))))))

(defvar gnus-select-method
  (nconc
   (list 'nntp (or (condition-case ()
		       (gnus-getenv-nntpserver)
		     (error nil))
		   (if (and gnus-default-nntp-server
			    (not (string= gnus-default-nntp-server "")))
		       gnus-default-nntp-server)
		   (system-name)))
   (if (or (null gnus-nntp-service)
	   (equal gnus-nntp-service "nntp"))
       nil
     (list gnus-nntp-service)))
  "*Default method for selecting a newsgroup.
This variable should be a list, where the first element is how the
news is to be fetched, the second is the address.

For instance, if you want to get your news via NNTP from
\"flab.flab.edu\", you could say:

(setq gnus-select-method '(nntp \"flab.flab.edu\"))

If you want to use your local spool, say:

(setq gnus-select-method (list 'nnspool (system-name)))

If you use this variable, you must set `gnus-nntp-server' to nil.

There is a lot more to know about select methods and virtual servers -
see the manual for details.")

(defvar gnus-message-archive-method 
  `(nnfolder
    "archive"
    (nnfolder-directory ,(nnheader-concat message-directory "archive"))
    (nnfolder-active-file 
     ,(nnheader-concat message-directory "archive/active"))
    (nnfolder-get-new-mail nil)
    (nnfolder-inhibit-expiry t))
  "*Method used for archiving messages you've sent.
This should be a mail method.

It's probably not a very effective to change this variable once you've
run Gnus once.  After doing that, you must edit this server from the
server buffer.")

(defvar gnus-message-archive-group nil
  "*Name of the group in which to save the messages you've written.
This can either be a string, a list of strings; or an alist
of regexps/functions/forms to be evaluated to return a string (or a list
of strings).  The functions are called with the name of the current
group (or nil) as a parameter.

Normally the group names returned by this variable should be
unprefixed -- which implictly means \"store on the archive server\".
However, you may wish to store the message on some other server.  In
that case, just return a fully prefixed name of the group --
\"nnml+private:mail.misc\", for instance.")

(defvar gnus-refer-article-method nil
  "*Preferred method for fetching an article by Message-ID.
If you are reading news from the local spool (with nnspool), fetching
articles by Message-ID is painfully slow.  By setting this method to an
nntp method, you might get acceptable results.

The value of this variable must be a valid select method as discussed
in the documentation of `gnus-select-method'.")

(defvar gnus-secondary-select-methods nil
  "*A list of secondary methods that will be used for reading news.
This is a list where each element is a complete select method (see
`gnus-select-method').

If, for instance, you want to read your mail with the nnml backend,
you could set this variable:

(setq gnus-secondary-select-methods '((nnml \"\")))")

(defvar gnus-secondary-servers nil
  "*List of NNTP servers that the user can choose between interactively.
To make Gnus query you for a server, you have to give `gnus' a
non-numeric prefix - `C-u M-x gnus', in short.")

(defvar gnus-nntp-server nil
  "*The name of the host running the NNTP server.
This variable is semi-obsolete.	 Use the `gnus-select-method'
variable instead.")

(defvar gnus-startup-file "~/.newsrc"
  "*Your `.newsrc' file.
`.newsrc-SERVER' will be used instead if that exists.")

(defvar gnus-init-file "~/.gnus"
  "*Your Gnus elisp startup file.
If a file with the .el or .elc suffixes exist, it will be read
instead.")

(defvar gnus-group-faq-directory
  '("/ftp@mirrors.aol.com:/pub/rtfm/usenet/"
    "/ftp@sunsite.auc.dk:/pub/usenet/"
    "/ftp@sunsite.doc.ic.ac.uk:/pub/usenet/news-faqs/"
    "/ftp@src.doc.ic.ac.uk:/usenet/news-FAQS/"
    "/ftp@ftp.seas.gwu.edu:/pub/rtfm/"
    "/ftp@rtfm.mit.edu:/pub/usenet/"
    "/ftp@ftp.uni-paderborn.de:/pub/FAQ/"
    "/ftp@ftp.sunet.se:/pub/usenet/"
    "/ftp@nctuccca.edu.tw:/USENET/FAQ/"
    "/ftp@hwarang.postech.ac.kr:/pub/usenet/"
    "/ftp@ftp.hk.super.net:/mirror/faqs/")
  "*Directory where the group FAQs are stored.
This will most commonly be on a remote machine, and the file will be
fetched by ange-ftp.

This variable can also be a list of directories.  In that case, the
first element in the list will be used by default.  The others can
be used when being prompted for a site.

Note that Gnus uses an aol machine as the default directory.  If this
feels fundamentally unclean, just think of it as a way to finally get
something of value back from them.

If the default site is too slow, try one of these:

   North America: mirrors.aol.com		 /pub/rtfm/usenet
		  ftp.seas.gwu.edu		 /pub/rtfm
		  rtfm.mit.edu			 /pub/usenet
   Europe:	  ftp.uni-paderborn.de		 /pub/FAQ
                  src.doc.ic.ac.uk               /usenet/news-FAQS
		  ftp.sunet.se			 /pub/usenet
	          sunsite.auc.dk                 /pub/usenet
   Asia:	  nctuccca.edu.tw		 /USENET/FAQ
		  hwarang.postech.ac.kr		 /pub/usenet
		  ftp.hk.super.net		 /mirror/faqs")

(defvar gnus-group-archive-directory
  "/ftp@ftp.hpc.uh.edu:/pub/emacs/ding-list/"
  "*The address of the (ding) archives.")

(defvar gnus-group-recent-archive-directory
  "/ftp@ftp.hpc.uh.edu:/pub/emacs/ding-list-recent/"
  "*The address of the most recent (ding) articles.")

(defvar gnus-default-subscribed-newsgroups nil
  "*This variable lists what newsgroups should be subscribed the first time Gnus is used.
It should be a list of strings.
If it is `t', Gnus will not do anything special the first time it is
started; it'll just use the normal newsgroups subscription methods.")

(defvar gnus-use-cross-reference t
  "*Non-nil means that cross referenced articles will be marked as read.
If nil, ignore cross references.  If t, mark articles as read in
subscribed newsgroups.	If neither t nor nil, mark as read in all
newsgroups.")

(defvar gnus-single-article-buffer t
  "*If non-nil, display all articles in the same buffer.
If nil, each group will get its own article buffer.")

(defvar gnus-use-dribble-file t
  "*Non-nil means that Gnus will use a dribble file to store user updates.
If Emacs should crash without saving the .newsrc files, complete
information can be restored from the dribble file.")

(defvar gnus-dribble-directory nil
  "*The directory where dribble files will be saved.
If this variable is nil, the directory where the .newsrc files are
saved will be used.")

(defvar gnus-asynchronous nil
  "*If non-nil, Gnus will supply backends with data needed for async article fetching.")

(defvar gnus-kill-summary-on-exit t
  "*If non-nil, kill the summary buffer when you exit from it.
If nil, the summary will become a \"*Dead Summary*\" buffer, and
it will be killed sometime later.")

(defvar gnus-large-newsgroup 200
  "*The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than this value,
confirmation is required for selecting the newsgroup.")

;; Suggested by Andrew Eskilsson <pi92ae@lelle.pt.hk-r.se>.
(defvar gnus-no-groups-message "No news is horrible news"
  "*Message displayed by Gnus when no groups are available.")

(defvar gnus-use-long-file-name (not (memq system-type '(usg-unix-v xenix)))
  "*Non-nil means that the default name of a file to save articles in is the group name.
If it's nil, the directory form of the group name is used instead.

If this variable is a list, and the list contains the element
`not-score', long file names will not be used for score files; if it
contains the element `not-save', long file names will not be used for
saving; and if it contains the element `not-kill', long file names
will not be used for kill files.

Note that the default for this variable varies according to what system
type you're using.  On `usg-unix-v' and `xenix' this variable defaults
to nil while on all other systems it defaults to t.")

(defvar gnus-article-save-directory gnus-directory
  "*Name of the directory articles will be saved in (default \"~/News\").")

(defvar gnus-kill-files-directory gnus-directory
  "*Name of the directory where kill files will be stored (default \"~/News\").")

(defvar gnus-default-article-saver 'gnus-summary-save-in-rmail
  "*A function to save articles in your favorite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format).
* gnus-summary-save-in-vm (use VM's folder format).")

(defvar gnus-prompt-before-saving 'always
  "*This variable says how much prompting is to be done when saving articles.
If it is nil, no prompting will be done, and the articles will be
saved to the default files.  If this variable is `always', each and
every article that is saved will be preceded by a prompt, even when
saving large batches of articles.  If this variable is neither nil not
`always', there the user will be prompted once for a file name for
each invocation of the saving commands.")

(defvar gnus-rmail-save-name (function gnus-plain-save-name)
  "*A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-mail-save-name (function gnus-plain-save-name)
  "*A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-folder-save-name (function gnus-folder-save-name)
  "*A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER.")

(defvar gnus-file-save-name (function gnus-numeric-save-name)
  "*A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional
LAST-FILE.")

(defvar gnus-split-methods
  '((gnus-article-archive-name))
  "*Variable used to suggest where articles are to be saved.
For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))

This variable is an alist where the where the key is the match and the
value is a list of possible files to save in if the match is non-nil.

If the match is a string, it is used as a regexp match on the
article.  If the match is a symbol, that symbol will be funcalled
from the buffer of the article to be saved with the newsgroup as the
parameter.  If it is a list, it will be evaled in the same buffer.

If this form or function returns a string, this string will be used as
a possible file name; and if it returns a non-nil list, that list will
be used as possible file names.")

(defvar gnus-move-split-methods nil
  "*Variable used to suggest where articles are to be moved to.
It uses the same syntax as the `gnus-split-methods' variable.")

(defvar gnus-save-score nil
  "*If non-nil, save group scoring info.")

(defvar gnus-use-adaptive-scoring nil
  "*If non-nil, use some adaptive scoring scheme.")

(defvar gnus-use-cache 'passive
  "*If nil, Gnus will ignore the article cache.
If `passive', it will allow entering (and reading) articles
explicitly entered into the cache.  If anything else, use the
cache to the full extent of the law.")

(defvar gnus-use-trees nil
  "*If non-nil, display a thread tree buffer.")

(defvar gnus-use-grouplens nil
  "*If non-nil, use GroupLens ratings.")

(defvar gnus-keep-backlog nil
  "*If non-nil, Gnus will keep read articles for later re-retrieval.
If it is a number N, then Gnus will only keep the last N articles
read.  If it is neither nil nor a number, Gnus will keep all read
articles.  This is not a good idea.")

(defvar gnus-use-nocem nil
  "*If non-nil, Gnus will read NoCeM cancel messages.")

(defvar gnus-use-demon nil
  "If non-nil, Gnus might use some demons.")

(defvar gnus-use-scoring t
  "*If non-nil, enable scoring.")

(defvar gnus-use-picons nil
  "*If non-nil, display picons.")

(defvar gnus-fetch-old-headers nil
  "*Non-nil means that Gnus will try to build threads by grabbing old headers.
If an unread article in the group refers to an older, already read (or
just marked as read) article, the old article will not normally be
displayed in the Summary buffer.  If this variable is non-nil, Gnus
will attempt to grab the headers to the old articles, and thereby
build complete threads.	 If it has the value `some', only enough
headers to connect otherwise loose threads will be displayed.
This variable can also be a number.  In that case, no more than that
number of old headers will be fetched.

The server has to support NOV for any of this to work.")

;see gnus-cus.el
;(defvar gnus-visual t
;  "*If non-nil, will do various highlighting.
;If nil, no mouse highlights (or any other highlights) will be
;performed.  This might speed up Gnus some when generating large group
;and summary buffers.")

(defvar gnus-novice-user t
  "*Non-nil means that you are a usenet novice.
If non-nil, verbose messages may be displayed and confirmations may be
required.")

(defvar gnus-expert-user nil
  "*Non-nil means that you will never be asked for confirmation about anything.
And that means *anything*.")

(defvar gnus-verbose 7
  "*Integer that says how verbose Gnus should be.
The higher the number, the more messages Gnus will flash to say what
it's doing.  At zero, Gnus will be totally mute; at five, Gnus will
display most important messages; and at ten, Gnus will keep on
jabbering all the time.")

(defvar gnus-keep-same-level nil
  "*Non-nil means that the next newsgroup after the current will be on the same level.
When you type, for instance, `n' after reading the last article in the
current newsgroup, you will go to the next newsgroup.  If this variable
is nil, the next newsgroup will be the next from the group
buffer.
If this variable is non-nil, Gnus will either put you in the
next newsgroup with the same level, or, if no such newsgroup is
available, the next newsgroup with the lowest possible level higher
than the current level.
If this variable is `best', Gnus will make the next newsgroup the one
with the best level.")

(defvar gnus-summary-make-false-root 'adopt
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
given by the `gnus-summary-same-subject' variable.)")

(defvar gnus-summary-gather-exclude-subject "^ *$\\|^(none)$"
  "*A regexp to match subjects to be excluded from loose thread gathering.
As loose thread gathering is done on subjects only, that means that
there can be many false gatherings performed.  By rooting out certain
common subjects, gathering might become saner.")

(defvar gnus-summary-gather-subject-limit nil
  "*Maximum length of subject comparisons when gathering loose threads.
Use nil to compare full subjects.  Setting this variable to a low
number will help gather threads that have been corrupted by
newsreaders chopping off subject lines, but it might also mean that
unrelated articles that have subject that happen to begin with the
same few characters will be incorrectly gathered.

If this variable is `fuzzy', Gnus will use a fuzzy algorithm when
comparing subjects.")

(defvar gnus-simplify-ignored-prefixes nil
  "*Regexp, matches for which are removed from subject lines when simplifying.")

(defvar gnus-build-sparse-threads nil
  "*If non-nil, fill in the gaps in threads.
If `some', only fill in the gaps that are needed to tie loose threads
together.  If `more', fill in all leaf nodes that Gnus can find.  If
non-nil and non-`some', fill in all gaps that Gnus manages to guess.")

(defvar gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
  "Function used for gathering loose threads.
There are two pre-defined functions: `gnus-gather-threads-by-subject',
which only takes Subjects into consideration; and
`gnus-gather-threads-by-references', which compared the References
headers of the articles to find matches.")

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-summary-same-subject ""
  "*String indicating that the current article has the same subject as the previous.
This variable will only be used if the value of
`gnus-summary-make-false-root' is `empty'.")

(defvar gnus-summary-goto-unread t
  "*If non-nil, marking commands will go to the next unread article.
If `never', \\<gnus-summary-mode-map>\\[gnus-summary-next-page] will go to the next article,
whether it is read or not.")

(defvar gnus-group-goto-unread t
  "*If non-nil, movement commands will go to the next unread and subscribed group.")

(defvar gnus-goto-next-group-when-activating t
  "*If non-nil, the \\<gnus-group-mode-map>\\[gnus-group-get-new-news-this-group] command will advance point to the next group.")

(defvar gnus-check-new-newsgroups t
  "*Non-nil means that Gnus will add new newsgroups at startup.
If this variable is `ask-server', Gnus will ask the server for new
groups since the last time it checked.	This means that the killed list
is no longer necessary, so you could set `gnus-save-killed-list' to
nil.

A variant is to have this variable be a list of select methods.	 Gnus
will then use the `ask-server' method on all these select methods to
query for new groups from all those servers.

Eg.
  (setq gnus-check-new-newsgroups
	'((nntp \"some.server\") (nntp \"other.server\")))

If this variable is nil, then you have to tell Gnus explicitly to
check for new newsgroups with \\<gnus-group-mode-map>\\[gnus-find-new-newsgroups].")

(defvar gnus-check-bogus-newsgroups nil
  "*Non-nil means that Gnus will check and remove bogus newsgroup at startup.
If this variable is nil, then you have to tell Gnus explicitly to
check for bogus newsgroups with \\<gnus-group-mode-map>\\[gnus-group-check-bogus-groups].")

(defvar gnus-read-active-file t
  "*Non-nil means that Gnus will read the entire active file at startup.
If this variable is nil, Gnus will only know about the groups in your
`.newsrc' file.

If this variable is `some', Gnus will try to only read the relevant
parts of the active file from the server.  Not all servers support
this, and it might be quite slow with other servers, but this should
generally be faster than both the t and nil value.

If you set this variable to nil or `some', you probably still want to
be told about new newsgroups that arrive.  To do that, set
`gnus-check-new-newsgroups' to `ask-server'.  This may not work
properly with all servers.")

(defvar gnus-level-subscribed 5
  "*Groups with levels less than or equal to this variable are subscribed.")

(defvar gnus-level-unsubscribed 7
  "*Groups with levels less than or equal to this variable are unsubscribed.
Groups with levels less than `gnus-level-subscribed', which should be
less than this variable, are subscribed.")

(defvar gnus-level-zombie 8
  "*Groups with this level are zombie groups.")

(defvar gnus-level-killed 9
  "*Groups with this level are killed.")

(defvar gnus-level-default-subscribed 3
  "*New subscribed groups will be subscribed at this level.")

(defvar gnus-level-default-unsubscribed 6
  "*New unsubscribed groups will be unsubscribed at this level.")

(defvar gnus-activate-level (1+ gnus-level-subscribed)
  "*Groups higher than this level won't be activated on startup.
Setting this variable to something log might save lots of time when
you have many groups that you aren't interested in.")

(defvar gnus-activate-foreign-newsgroups 4
  "*If nil, Gnus will not check foreign newsgroups at startup.
If it is non-nil, it should be a number between one and nine.  Foreign
newsgroups that have a level lower or equal to this number will be
activated on startup.  For instance, if you want to active all
subscribed newsgroups, but not the rest, you'd set this variable to
`gnus-level-subscribed'.

If you subscribe to lots of newsgroups from different servers, startup
might take a while.  By setting this variable to nil, you'll save time,
but you won't be told how many unread articles there are in the
groups.")

(defvar gnus-save-newsrc-file t
  "*Non-nil means that Gnus will save the `.newsrc' file.
Gnus always saves its own startup file, which is called
\".newsrc.eld\".  The file called \".newsrc\" is in a format that can
be readily understood by other newsreaders.  If you don't plan on
using other newsreaders, set this variable to nil to save some time on
exit.")

(defvar gnus-save-killed-list t
  "*If non-nil, save the list of killed groups to the startup file.
If you set this variable to nil, you'll save both time (when starting
and quitting) and space (both memory and disk), but it will also mean
that Gnus has no record of which groups are new and which are old, so
the automatic new newsgroups subscription methods become meaningless.

You should always set `gnus-check-new-newsgroups' to `ask-server' or
nil if you set this variable to nil.")

(defvar gnus-interactive-catchup t
  "*If non-nil, require your confirmation when catching up a group.")

(defvar gnus-interactive-exit t
  "*If non-nil, require your confirmation when exiting Gnus.")

(defvar gnus-kill-killed t
  "*If non-nil, Gnus will apply kill files to already killed articles.
If it is nil, Gnus will never apply kill files to articles that have
already been through the scoring process, which might very well save lots
of time.")

(defvar gnus-extract-address-components 'gnus-extract-address-components
  "*Function for extracting address components from a From header.
Two pre-defined function exist: `gnus-extract-address-components',
which is the default, quite fast, and too simplistic solution, and
`mail-extract-address-components', which works much better, but is
slower.")

(defvar gnus-summary-default-score 0
  "*Default article score level.
If this variable is nil, scoring will be disabled.")

(defvar gnus-summary-zcore-fuzz 0
  "*Fuzziness factor for the zcore in the summary buffer.
Articles with scores closer than this to `gnus-summary-default-score'
will not be marked.")

(defvar gnus-simplify-subject-fuzzy-regexp nil
  "*Strings to be removed when doing fuzzy matches.
This can either be a regular expression or list of regular expressions
that will be removed from subject strings if fuzzy subject
simplification is selected.")

(defvar gnus-permanently-visible-groups nil
  "*Regexp to match groups that should always be listed in the group buffer.
This means that they will still be listed when there are no unread
articles in the groups.")

(defvar gnus-list-groups-with-ticked-articles t
  "*If non-nil, list groups that have only ticked articles.
If nil, only list groups that have unread articles.")

(defvar gnus-group-default-list-level gnus-level-subscribed
  "*Default listing level.
Ignored if `gnus-group-use-permanent-levels' is non-nil.")

(defvar gnus-group-use-permanent-levels nil
  "*If non-nil, once you set a level, Gnus will use this level.")

(defvar gnus-group-list-inactive-groups t
  "*If non-nil, inactive groups will be listed.")

(defvar gnus-show-mime nil
  "*If non-nil, do mime processing of articles.
The articles will simply be fed to the function given by
`gnus-show-mime-method'.")

(defvar gnus-strict-mime t
  "*If nil, MIME-decode even if there is no Mime-Version header in the article.")

(defvar gnus-show-mime-method 'metamail-buffer
  "*Function to process a MIME message.
The function is called from the article buffer.")

(defvar gnus-decode-encoded-word-method (lambda ())
  "*Function to decode a MIME encoded-words.
The function is called from the article buffer.")

(defvar gnus-show-threads t
  "*If non-nil, display threads in summary mode.")

(defvar gnus-thread-hide-subtree nil
  "*If non-nil, hide all threads initially.
If threads are hidden, you have to run the command
`gnus-summary-show-thread' by hand or use `gnus-select-article-hook'
to expose hidden threads.")

(defvar gnus-thread-hide-killed t
  "*If non-nil, hide killed threads automatically.")

(defvar gnus-thread-ignore-subject nil
  "*If non-nil, ignore subjects and do all threading based on the Reference header.
If nil, which is the default, articles that have different subjects
from their parents will start separate threads.")

(defvar gnus-thread-operation-ignore-subject t
  "*If non-nil, subjects will be ignored when doing thread commands.
This affects commands like `gnus-summary-kill-thread' and
`gnus-summary-lower-thread'.

If this variable is nil, articles in the same thread with different
subjects will not be included in the operation in question.  If this
variable is `fuzzy', only articles that have subjects that are fuzzily
equal will be included.")

(defvar gnus-thread-indent-level 4
  "*Number that says how much each sub-thread should be indented.")

(defvar gnus-ignored-newsgroups
  (purecopy (mapconcat 'identity
		       '("^to\\."	; not "real" groups
			 "^[0-9. \t]+ " ; all digits in name
			 "[][\"#'()]"	; bogus characters
			 )
		       "\\|"))
  "*A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively non-existent.")

(defvar gnus-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^Received:\\|^Mail-from:"
  "*All headers that match this regexp will be hidden.
This variable can also be a list of regexps of headers to be ignored.
If `gnus-visible-headers' is non-nil, this variable will be ignored.")

(defvar gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-"
  "*All headers that do not match this regexp will be hidden.
This variable can also be a list of regexp of headers to remain visible.
If this variable is non-nil, `gnus-ignored-headers' will be ignored.")

(defvar gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^To:"
    "^Cc:" "^Date:" "^Organization:")
  "*This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list.")

(defvar gnus-boring-article-headers
  '(empty followup-to reply-to)
  "*Headers that are only to be displayed if they have interesting data.
Possible values in this list are `empty', `newsgroups', `followup-to',
`reply-to', and `date'.")

(defvar gnus-show-all-headers nil
  "*If non-nil, don't hide any headers.")

(defvar gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving.")

(defvar gnus-saved-headers gnus-visible-headers
  "*Headers to keep if `gnus-save-all-headers' is nil.
If `gnus-save-all-headers' is non-nil, this variable will be ignored.
If that variable is nil, however, all headers that match this regexp
will be kept while the rest will be deleted before saving.")

(defvar gnus-inhibit-startup-message nil
  "*If non-nil, the startup message will not be displayed.")

(defvar gnus-signature-separator "^-- *$"
  "Regexp matching signature separator.")

(defvar gnus-signature-limit nil
  "Provide a limit to what is considered a signature.
If it is a number, no signature may not be longer (in characters) than
that number.  If it is a function, the function will be called without
any parameters, and if it returns nil, there is no signature in the
buffer.  If it is a string, it will be used as a regexp.  If it
matches, the text in question is not a signature.")

(defvar gnus-auto-extend-newsgroup t
  "*If non-nil, extend newsgroup forward and backward when requested.")

(defvar gnus-auto-select-first t
  "*If nil, don't select the first unread article when entering a group.
If this variable is `best', select the highest-scored unread article
in the group.  If neither nil nor `best', select the first unread
article.

If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in
`gnus-select-group-hook'.")

(defvar gnus-auto-select-next t
  "*If non-nil, offer to go to the next group from the end of the previous.
If the value is t and the next newsgroup is empty, Gnus will exit
summary mode and go back to group mode.	 If the value is neither nil
nor t, Gnus will select the following unread newsgroup.	 In
particular, if the value is the symbol `quietly', the next unread
newsgroup will be selected without any confirmation, and if it is
`almost-quietly', the next group will be selected without any
confirmation if you are located on the last article in the group.
Finally, if this variable is `slightly-quietly', the `Z n' command
will go to the next group without confirmation.")

(defvar gnus-auto-select-same nil
  "*If non-nil, select the next article with the same subject.")

(defvar gnus-summary-check-current nil
  "*If non-nil, consider the current article when moving.
The \"unread\" movement commands will stay on the same line if the
current article is unread.")

(defvar gnus-auto-center-summary t
  "*If non-nil, always center the current summary buffer.
In particular, if `vertical' do only vertical recentering.  If non-nil
and non-`vertical', do both horizontal and vertical recentering.")

(defvar gnus-break-pages t
  "*If non-nil, do page breaking on articles.
The page delimiter is specified by the `gnus-page-delimiter'
variable.")

(defvar gnus-page-delimiter "^\^L"
  "*Regexp describing what to use as article page delimiters.
The default value is \"^\^L\", which is a form linefeed at the
beginning of a line.")

(defvar gnus-use-full-window t
  "*If non-nil, use the entire Emacs screen.")

(defvar gnus-window-configuration nil
  "Obsolete variable.  See `gnus-buffer-configuration'.")

(defvar gnus-window-min-width 2
  "*Minimum width of Gnus buffers.")

(defvar gnus-window-min-height 1
  "*Minimum height of Gnus buffers.")

(defvar gnus-buffer-configuration
  '((group
     (vertical 1.0
	       (group 1.0 point)
	       (if gnus-carpal '(group-carpal 4))))
    (summary
     (vertical 1.0
	       (summary 1.0 point)
	       (if gnus-carpal '(summary-carpal 4))))
    (article
     (cond 
      (gnus-use-picons
       '(frame 1.0
	       (vertical 1.0
			 (summary 0.25 point)
			 (if gnus-carpal '(summary-carpal 4))
			 (article 1.0))
	       (vertical ((height . 5) (width . 15)
			  (user-position . t)
			  (left . -1) (top . 1))
			 (picons 1.0))))
      (gnus-use-trees
       '(vertical 1.0
		  (summary 0.25 point)
		  (tree 0.25)
		  (article 1.0)))
      (t
       '(vertical 1.0
		 (summary 0.25 point)
		 (if gnus-carpal '(summary-carpal 4))
		 (article 1.0)))))
    (server
     (vertical 1.0
	       (server 1.0 point)
	       (if gnus-carpal '(server-carpal 2))))
    (browse
     (vertical 1.0
	       (browse 1.0 point)
	       (if gnus-carpal '(browse-carpal 2))))
    (message
     (vertical 1.0
	       (message 1.0 point)))
    (pick
     (vertical 1.0
	       (article 1.0 point)))
    (info
     (vertical 1.0
	       (info 1.0 point)))
    (summary-faq
     (vertical 1.0
	       (summary 0.25)
	       (faq 1.0 point)))
    (edit-group
     (vertical 1.0
	       (group 0.5)
	       (edit-group 1.0 point)))
    (edit-server
     (vertical 1.0
	       (server 0.5)
	       (edit-server 1.0 point)))
    (edit-score
     (vertical 1.0
	       (summary 0.25)
	       (edit-score 1.0 point)))
    (post
     (vertical 1.0
	       (post 1.0 point)))
    (reply
     (vertical 1.0
	       (article-copy 0.5)
	       (message 1.0 point)))
    (forward
     (vertical 1.0
	       (message 1.0 point)))
    (reply-yank
     (vertical 1.0
	       (message 1.0 point)))
    (mail-bounce
     (vertical 1.0
	       (article 0.5)
	       (message 1.0 point)))
    (draft
     (vertical 1.0
	       (draft 1.0 point)))
    (pipe
     (vertical 1.0
	       (summary 0.25 point)
	       (if gnus-carpal '(summary-carpal 4))
	       ("*Shell Command Output*" 1.0)))
    (bug
     (vertical 1.0
	       ("*Gnus Help Bug*" 0.5)
	       ("*Gnus Bug*" 1.0 point)))
    (compose-bounce
     (vertical 1.0
	       (article 0.5)
	       (message 1.0 point))))
  "Window configuration for all possible Gnus buffers.
This variable is a list of lists.  Each of these lists has a NAME and
a RULE.	 The NAMEs are commonsense names like `group', which names a
rule used when displaying the group buffer; `summary', which names a
rule for what happens when you enter a group and do not display an
article buffer; and so on.  See the value of this variable for a
complete list of NAMEs.

Each RULE is a list of vectors.	 The first element in this vector is
the name of the buffer to be displayed; the second element is the
percentage of the screen this buffer is to occupy (a number in the
0.0-0.99 range); the optional third element is `point', which should
be present to denote which buffer point is to go to after making this
buffer configuration.")

(defvar gnus-window-to-buffer
  '((group . gnus-group-buffer)
    (summary . gnus-summary-buffer)
    (article . gnus-article-buffer)
    (server . gnus-server-buffer)
    (browse . "*Gnus Browse Server*")
    (edit-group . gnus-group-edit-buffer)
    (edit-server . gnus-server-edit-buffer)
    (group-carpal . gnus-carpal-group-buffer)
    (summary-carpal . gnus-carpal-summary-buffer)
    (server-carpal . gnus-carpal-server-buffer)
    (browse-carpal . gnus-carpal-browse-buffer)
    (edit-score . gnus-score-edit-buffer)
    (message . gnus-message-buffer)
    (mail . gnus-message-buffer)
    (post-news . gnus-message-buffer)
    (faq . gnus-faq-buffer)
    (picons . "*Picons*")
    (tree . gnus-tree-buffer)
    (info . gnus-info-buffer)
    (article-copy . gnus-article-copy)
    (draft . gnus-draft-buffer))
  "Mapping from short symbols to buffer names or buffer variables.")

(defvar gnus-carpal nil
  "*If non-nil, display clickable icons.")

(defvar gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
  "*Function called with a group name when new group is detected.
A few pre-made functions are supplied: `gnus-subscribe-randomly'
inserts new groups at the beginning of the list of groups;
`gnus-subscribe-alphabetically' inserts new groups in strict
alphabetic order; `gnus-subscribe-hierarchically' inserts new groups
in hierarchical newsgroup order; `gnus-subscribe-interactively' asks
for your decision; `gnus-subscribe-killed' kills all new groups;
`gnus-subscribe-zombies' will make all new groups into zombies.")

;; Suggested by a bug report by Hallvard B Furuseth.
;; <h.b.furuseth@usit.uio.no>.
(defvar gnus-subscribe-options-newsgroup-method
  (function gnus-subscribe-alphabetically)
  "*This function is called to subscribe newsgroups mentioned on \"options -n\" lines.
If, for instance, you want to subscribe to all newsgroups in the
\"no\" and \"alt\" hierarchies, you'd put the following in your
.newsrc file:

options -n no.all alt.all

Gnus will the subscribe all new newsgroups in these hierarchies with
the subscription method in this variable.")

(defvar gnus-subscribe-hierarchical-interactive nil
  "*If non-nil, Gnus will offer to subscribe hierarchically.
When a new hierarchy appears, Gnus will ask the user:

'alt.binaries': Do you want to subscribe to this hierarchy? ([d]ys):

If the user pressed `d', Gnus will descend the hierarchy, `y' will
subscribe to all newsgroups in the hierarchy and `s' will skip this
hierarchy in its entirety.")

(defvar gnus-group-sort-function 'gnus-group-sort-by-alphabet
  "*Function used for sorting the group buffer.
This function will be called with group info entries as the arguments
for the groups to be sorted.  Pre-made functions include
`gnus-group-sort-by-alphabet', `gnus-group-sort-by-unread',
`gnus-group-sort-by-level', `gnus-group-sort-by-score', and
`gnus-group-sort-by-rank'.

This variable can also be a list of sorting functions.	In that case,
the most significant sort function should be the last function in the
list.")

;; Mark variables suggested by Thomas Michanek
;; <Thomas.Michanek@telelogic.se>.
(defvar gnus-unread-mark ? 
  "*Mark used for unread articles.")
(defvar gnus-ticked-mark ?!
  "*Mark used for ticked articles.")
(defvar gnus-dormant-mark ??
  "*Mark used for dormant articles.")
(defvar gnus-del-mark ?r
  "*Mark used for del'd articles.")
(defvar gnus-read-mark ?R
  "*Mark used for read articles.")
(defvar gnus-expirable-mark ?E
  "*Mark used for expirable articles.")
(defvar gnus-killed-mark ?K
  "*Mark used for killed articles.")
(defvar gnus-souped-mark ?F
  "*Mark used for killed articles.")
(defvar gnus-kill-file-mark ?X
  "*Mark used for articles killed by kill files.")
(defvar gnus-low-score-mark ?Y
  "*Mark used for articles with a low score.")
(defvar gnus-catchup-mark ?C
  "*Mark used for articles that are caught up.")
(defvar gnus-replied-mark ?A
  "*Mark used for articles that have been replied to.")
(defvar gnus-cached-mark ?*
  "*Mark used for articles that are in the cache.")
(defvar gnus-saved-mark ?S
  "*Mark used for articles that have been saved to.")
(defvar gnus-process-mark ?#
  "*Process mark.")
(defvar gnus-ancient-mark ?O
  "*Mark used for ancient articles.")
(defvar gnus-sparse-mark ?Q
  "*Mark used for sparsely reffed articles.")
(defvar gnus-canceled-mark ?G
  "*Mark used for canceled articles.")
(defvar gnus-score-over-mark ?+
  "*Score mark used for articles with high scores.")
(defvar gnus-score-below-mark ?-
  "*Score mark used for articles with low scores.")
(defvar gnus-empty-thread-mark ? 
  "*There is no thread under the article.")
(defvar gnus-not-empty-thread-mark ?=
  "*There is a thread under the article.")

(defvar gnus-view-pseudo-asynchronously nil
  "*If non-nil, Gnus will view pseudo-articles asynchronously.")

(defvar gnus-view-pseudos nil
  "*If `automatic', pseudo-articles will be viewed automatically.
If `not-confirm', pseudos will be viewed automatically, and the user
will not be asked to confirm the command.")

(defvar gnus-view-pseudos-separately t
  "*If non-nil, one pseudo-article will be created for each file to be viewed.
If nil, all files that use the same viewing command will be given as a
list of parameters to that command.")

(defvar gnus-insert-pseudo-articles t
  "*If non-nil, insert pseudo-articles when decoding articles.")

(defvar gnus-group-line-format "%M%S%p%P%5y: %(%g%)%l\n"
  "*Format of group lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%M    Only marked articles (character, \"*\" or \" \")
%S    Whether the group is subscribed (character, \"U\", \"K\", \"Z\" or \" \")
%L    Level of subscribedness (integer)
%N    Number of unread articles (integer)
%I    Number of dormant articles (integer)
%i    Number of ticked and dormant (integer)
%T    Number of ticked articles (integer)
%R    Number of read articles (integer)
%t    Total number of articles (integer)
%y    Number of unread, unticked articles (integer)
%G    Group name (string)
%g    Qualified group name (string)
%D    Group description (string)
%s    Select method (string)
%o    Moderated group (char, \"m\")
%p    Process mark (char)
%O    Moderated group (string, \"(m)\" or \"\")
%P    Topic indentation (string)
%l    Whether there are GroupLens predictions for this group (string)
%n    Select from where (string)
%z    A string that look like `<%s:%n>' if a foreign select method is used
%u    User defined specifier.  The next character in the format string should
      be a letter.  Gnus will call the function gnus-user-format-function-X,
      where X is the letter following %u.  The function will be passed the
      current header as argument.  The function should return a string, which
      will be inserted into the buffer just like information from any other
      group specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face' when
the mouse point move inside the area.  There can only be one such area.

Note that this format specification is not always respected.  For
reasons of efficiency, when listing killed groups, this specification
is ignored altogether.	If the spec is changed considerably, your
output may end up looking strange when listing both alive and killed
groups.

If you use %o or %O, reading the active file will be slower and quite
a bit of extra memory will be used. %D will also worsen performance.
Also note that if you change the format specification to include any
of these specs, you must probably re-start Gnus to see them go into
effect.")

(defvar gnus-summary-line-format "%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n"
  "*The format specification of the lines in the summary buffer.

It works along the same lines as a normal formatting string,
with some simple extensions.

%N   Article number, left padded with spaces (string)
%S   Subject (string)
%s   Subject if it is at the root of a thread, and \"\" otherwise (string)
%n   Name of the poster (string)
%a   Extracted name of the poster (string)
%A   Extracted address of the poster (string)
%F   Contents of the From: header (string)
%x   Contents of the Xref: header (string)
%D   Date of the article (string)
%d   Date of the article (string) in DD-MMM format
%M   Message-id of the article (string)
%r   References of the article (string)
%c   Number of characters in the article (integer)
%L   Number of lines in the article (integer)
%I   Indentation based on thread level (a string of spaces)
%T   A string with two possible values: 80 spaces if the article
     is on thread level two or larger and 0 spaces on level one
%R   \"A\" if this article has been replied to, \" \" otherwise (character)
%U   Status of this article (character, \"R\", \"K\", \"-\" or \" \")
%[   Opening bracket (character, \"[\" or \"<\")
%]   Closing bracket (character, \"]\" or \">\")
%>   Spaces of length thread-level (string)
%<   Spaces of length (- 20 thread-level) (string)
%i   Article score (number)
%z   Article zcore (character)
%t   Number of articles under the current thread (number).
%e   Whether the thread is empty or not (character).
%l   GroupLens score (string).
%u   User defined specifier.  The next character in the format string should
     be a letter.  Gnus will call the function gnus-user-format-function-X,
     where X is the letter following %u.  The function will be passed the
     current header as argument.  The function should return a string, which
     will be inserted into the summary just like information from any other
     summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face'
when the mouse point is placed inside the area.	 There can only be one
such area.

The %U (status), %R (replied) and %z (zcore) specs have to be handled
with care.  For reasons of efficiency, Gnus will compute what column
these characters will end up in, and \"hard-code\" that.  This means that
it is illegal to have these specs after a variable-length spec.	 Well,
you might not be arrested, but your summary buffer will look strange,
which is bad enough.

The smart choice is to have these specs as for to the left as
possible.

This restriction may disappear in later versions of Gnus.")

(defvar gnus-summary-dummy-line-format
  "*  %(:                          :%) %S\n"
  "*The format specification for the dummy roots in the summary buffer.
It works along the same lines as a normal formatting string,
with some simple extensions.

%S  The subject")

(defvar gnus-summary-mode-line-format "Gnus: %%b [%A] %Z"
  "*The format specification for the summary mode line.
It works along the same lines as a normal formatting string,
with some simple extensions:

%G  Group name
%p  Unprefixed group name
%A  Current article number
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
%E  Number of articles expunged by the score files")

(defvar gnus-article-mode-line-format "Gnus: %%b %S"
  "*The format specification for the article mode line.
See `gnus-summary-mode-line-format' for a closer description.")

(defvar gnus-group-mode-line-format "Gnus: %%b {%M%:%S}"
  "*The format specification for the group mode line.
It works along the same lines as a normal formatting string,
with some simple extensions:

%S   The native news server.
%M   The native select method.
%:   \":\" if %S isn't \"\".")

(defvar gnus-valid-select-methods
  '(("nntp" post address prompt-address)
    ("nnspool" post address)
    ("nnvirtual" post-mail virtual prompt-address)
    ("nnmbox" mail respool address)
    ("nnml" mail respool address)
    ("nnmh" mail respool address)
    ("nndir" post-mail prompt-address address)
    ("nneething" none address prompt-address)
    ("nndoc" none address prompt-address)
    ("nnbabyl" mail address respool)
    ("nnkiboze" post virtual)
    ("nnsoup" post-mail address)
    ("nndraft" post-mail)
    ("nnfolder" mail respool address))
  "An alist of valid select methods.
The first element of each list lists should be a string with the name
of the select method.  The other elements may be be the category of
this method (ie. `post', `mail', `none' or whatever) or other
properties that this method has (like being respoolable).
If you implement a new select method, all you should have to change is
this variable.	I think.")

(defvar gnus-updated-mode-lines '(group article summary tree)
  "*List of buffers that should update their mode lines.
The list may contain the symbols `group', `article' and `summary'.  If
the corresponding symbol is present, Gnus will keep that mode line
updated with information that may be pertinent.
If this variable is nil, screen refresh may be quicker.")

;; Added by Keinonen Kari <kk85613@cs.tut.fi>.
(defvar gnus-mode-non-string-length nil
  "*Max length of mode-line non-string contents.
If this is nil, Gnus will take space as is needed, leaving the rest
of the modeline intact.")

;see gnus-cus.el
;(defvar gnus-mouse-face 'highlight
;  "*Face used for mouse highlighting in Gnus.
;No mouse highlights will be done if `gnus-visual' is nil.")

(defvar gnus-summary-mark-below 0
  "*Mark all articles with a score below this variable as read.
This variable is local to each summary buffer and usually set by the
score file.")

(defvar gnus-article-sort-functions '(gnus-article-sort-by-number)
  "*List of functions used for sorting articles in the summary buffer.
This variable is only used when not using a threaded display.")

(defvar gnus-thread-sort-functions '(gnus-thread-sort-by-number)
  "*List of functions used for sorting threads in the summary buffer.
By default, threads are sorted by article number.

Each function takes two threads and return non-nil if the first thread
should be sorted before the other.  If you use more than one function,
the primary sort function should be the last.  You should probably
always include `gnus-thread-sort-by-number' in the list of sorting
functions -- preferably first.

Ready-mady functions include `gnus-thread-sort-by-number',
`gnus-thread-sort-by-author', `gnus-thread-sort-by-subject',
`gnus-thread-sort-by-date', `gnus-thread-sort-by-score' and
`gnus-thread-sort-by-total-score' (see `gnus-thread-score-function').")

(defvar gnus-thread-score-function '+
  "*Function used for calculating the total score of a thread.

The function is called with the scores of the article and each
subthread and should then return the score of the thread.

Some functions you can use are `+', `max', or `min'.")

(defvar gnus-summary-expunge-below nil
  "All articles that have a score less than this variable will be expunged.")

(defvar gnus-thread-expunge-below nil
  "All threads that have a total score less than this variable will be expunged.
See `gnus-thread-score-function' for en explanation of what a
\"thread score\" is.")

(defvar gnus-auto-subscribed-groups
  "^nnml\\|^nnfolder\\|^nnmbox\\|^nnmh\\|^nnbabyl"
  "*All new groups that match this regexp will be subscribed automatically.
Note that this variable only deals with new groups.  It has no effect
whatsoever on old groups.

New groups that match this regexp will not be handled by
`gnus-subscribe-newsgroup-method'.  Instead, they will
be subscribed using `gnus-subscribe-options-newsgroup-method'.")

(defvar gnus-options-subscribe nil
  "*All new groups matching this regexp will be subscribed unconditionally.
Note that this variable deals only with new newsgroups.	 This variable
does not affect old newsgroups.

New groups that match this regexp will not be handled by
`gnus-subscribe-newsgroup-method'.  Instead, they will
be subscribed using `gnus-subscribe-options-newsgroup-method'.")

(defvar gnus-options-not-subscribe nil
  "*All new groups matching this regexp will be ignored.
Note that this variable deals only with new newsgroups.	 This variable
does not affect old (already subscribed) newsgroups.")

(defvar gnus-auto-expirable-newsgroups nil
  "*Groups in which to automatically mark read articles as expirable.
If non-nil, this should be a regexp that should match all groups in
which to perform auto-expiry.  This only makes sense for mail groups.")

(defvar gnus-total-expirable-newsgroups nil
  "*Groups in which to perform expiry of all read articles.
Use with extreme caution.  All groups that match this regexp will be
expiring - which means that all read articles will be deleted after
(say) one week.	 (This only goes for mail groups and the like, of
course.)")

(defvar gnus-group-uncollapsed-levels 1
  "Number of group name elements to leave alone when making a short group name.")

(defvar gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text.")

(defvar gnus-modtime-botch nil
  "*Non-nil means .newsrc should be deleted prior to save.  
Its use is due to the bogus appearance that .newsrc was modified on
disc.")

;; Hooks.

(defvar gnus-group-mode-hook nil
  "*A hook for Gnus group mode.")

(defvar gnus-summary-mode-hook nil
  "*A hook for Gnus summary mode.
This hook is run before any variables are set in the summary buffer.")

(defvar gnus-article-mode-hook nil
  "*A hook for Gnus article mode.")

(defvar gnus-summary-prepare-exit-hook nil
  "*A hook called when preparing to exit from the summary buffer.
It calls `gnus-summary-expire-articles' by default.")
(add-hook 'gnus-summary-prepare-exit-hook 'gnus-summary-expire-articles)

(defvar gnus-summary-exit-hook nil
  "*A hook called on exit from the summary buffer.")

(defvar gnus-group-catchup-group-hook nil
  "*A hook run when catching up a group from the group buffer.")

(defvar gnus-group-update-group-hook nil
  "*A hook called when updating group lines.")

(defvar gnus-open-server-hook nil
  "*A hook called just before opening connection to the news server.")

(defvar gnus-load-hook nil
  "*A hook run while Gnus is loaded.")

(defvar gnus-startup-hook nil
  "*A hook called at startup.
This hook is called after Gnus is connected to the NNTP server.")

(defvar gnus-get-new-news-hook nil
  "*A hook run just before Gnus checks for new news.")

(defvar gnus-after-getting-new-news-hook nil
  "*A hook run after Gnus checks for new news.")

(defvar gnus-group-prepare-function 'gnus-group-prepare-flat
  "*A function that is called to generate the group buffer.
The function is called with three arguments: The first is a number;
all group with a level less or equal to that number should be listed,
if the second is non-nil, empty groups should also be displayed.  If
the third is non-nil, it is a number.  No groups with a level lower
than this number should be displayed.

The only current function implemented is `gnus-group-prepare-flat'.")

(defvar gnus-group-prepare-hook nil
  "*A hook called after the group buffer has been generated.
If you want to modify the group buffer, you can use this hook.")

(defvar gnus-summary-prepare-hook nil
  "*A hook called after the summary buffer has been generated.
If you want to modify the summary buffer, you can use this hook.")

(defvar gnus-summary-generate-hook nil
  "*A hook run just before generating the summary buffer.
This hook is commonly used to customize threading variables and the
like.")

(defvar gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer.
If you want to run a special decoding program like nkf, use this hook.")

;(defvar gnus-article-display-hook nil
;  "*A hook called after the article is displayed in the article buffer.
;The hook is designed to change the contents of the article
;buffer.  Typical functions that this hook may contain are
;`gnus-article-hide-headers' (hide selected headers),
;`gnus-article-maybe-highlight' (perform fancy article highlighting),
;`gnus-article-hide-signature' (hide signature) and
;`gnus-article-treat-overstrike' (turn \"^H_\" into bold characters).")
;(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
;(add-hook 'gnus-article-display-hook 'gnus-article-treat-overstrike)
;(add-hook 'gnus-article-display-hook 'gnus-article-maybe-highlight)

(defvar gnus-article-x-face-too-ugly nil
  "Regexp matching posters whose face shouldn't be shown automatically.")

(defvar gnus-select-group-hook nil
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
		  gnus-newsgroup-headers))))")

(defvar gnus-select-article-hook nil
  "*A hook called when an article is selected.")

(defvar gnus-apply-kill-hook '(gnus-apply-kill-file)
  "*A hook called to apply kill files to a group.
This hook is intended to apply a kill file to the selected newsgroup.
The function `gnus-apply-kill-file' is called by default.

Since a general kill file is too heavy to use only for a few
newsgroups, I recommend you to use a lighter hook function.  For
example, if you'd like to apply a kill file to articles which contains
a string `rmgroup' in subject in newsgroup `control', you can use the
following hook:

 (setq gnus-apply-kill-hook
      (list
	(lambda ()
	  (cond ((string-match \"control\" gnus-newsgroup-name)
		 (gnus-kill \"Subject\" \"rmgroup\")
		 (gnus-expunge \"X\"))))))")

(defvar gnus-visual-mark-article-hook
  (list 'gnus-highlight-selected-summary)
  "*Hook run after selecting an article in the summary buffer.
It is meant to be used for highlighting the article in some way.  It
is not run if `gnus-visual' is nil.")

(defvar gnus-parse-headers-hook nil
  "*A hook called before parsing the headers.")
(add-hook 'gnus-parse-headers-hook 'gnus-decode-rfc1522)

(defvar gnus-exit-group-hook nil
  "*A hook called when exiting (not quitting) summary mode.")

(defvar gnus-suspend-gnus-hook nil
  "*A hook called when suspending (not exiting) Gnus.")

(defvar gnus-exit-gnus-hook nil
  "*A hook called when exiting Gnus.")

(defvar gnus-after-exiting-gnus-hook nil
  "*A hook called after exiting Gnus.")

(defvar gnus-save-newsrc-hook nil
  "*A hook called before saving any of the newsrc files.")

(defvar gnus-save-quick-newsrc-hook nil
  "*A hook called just before saving the quick newsrc file.
Can be used to turn version control on or off.")

(defvar gnus-save-standard-newsrc-hook nil
  "*A hook called just before saving the standard newsrc file.
Can be used to turn version control on or off.")

(defvar gnus-summary-update-hook
  (list 'gnus-summary-highlight-line)
  "*A hook called when a summary line is changed.
The hook will not be called if `gnus-visual' is nil.

The default function `gnus-summary-highlight-line' will
highlight the line according to the `gnus-summary-highlight'
variable.")

(defvar gnus-group-update-hook '(gnus-group-highlight-line)
  "*A hook called when a group line is changed.
The hook will not be called if `gnus-visual' is nil.

The default function `gnus-group-highlight-line' will
highlight the line according to the `gnus-group-highlight'
variable.")

(defvar gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read)
  "*A hook called when an article is selected for the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected.")

(defvar gnus-group-change-level-function nil
  "Function run when a group level is changed.
It is called with three parameters -- GROUP, LEVEL and OLDLEVEL.")

;; Remove any hilit infestation.
(add-hook 'gnus-startup-hook
	  (lambda ()
	    (remove-hook 'gnus-summary-prepare-hook
			 'hilit-rehighlight-buffer-quietly)
	    (remove-hook 'gnus-summary-prepare-hook 'hilit-install-line-hooks)
	    (setq gnus-mark-article-hook
		  '(gnus-summary-mark-read-and-unread-as-read))
	    (remove-hook 'gnus-article-prepare-hook
			 'hilit-rehighlight-buffer-quietly)))


;; Internal variables

(defvar gnus-tree-buffer "*Tree*"
  "Buffer where Gnus thread trees are displayed.")

;; Dummy variable.
(defvar gnus-use-generic-from nil)

(defvar gnus-thread-indent-array nil)
(defvar gnus-thread-indent-array-level gnus-thread-indent-level)

(defvar gnus-newsrc-file-version nil)

(defvar gnus-method-history nil)
;; Variable holding the user answers to all method prompts.

(defvar gnus-group-history nil)
;; Variable holding the user answers to all group prompts.

(defvar gnus-server-alist nil
  "List of available servers.")

(defvar gnus-group-indentation-function nil)

(defvar gnus-topic-indentation "") ;; Obsolete variable.

(defvar gnus-goto-missing-group-function nil)

(defvar gnus-override-subscribe-method nil)

(defvar gnus-group-goto-next-group-function nil
  "Function to override finding the next group after listing groups.")

(defconst gnus-article-mark-lists
  '((marked . tick) (replied . reply)
    (expirable . expire) (killed . killed)
    (bookmarks . bookmark) (dormant . dormant)
    (scored . score) (saved . save)
    (cached . cache)
    ))

;; Avoid highlighting in kill files.
(defvar gnus-summary-inhibit-highlight nil)
(defvar gnus-newsgroup-selected-overlay nil)

(defvar gnus-inhibit-hiding nil)
(defvar gnus-group-indentation "")
(defvar gnus-inhibit-limiting nil)
(defvar gnus-created-frames nil)

(defvar gnus-article-mode-map nil)
(defvar gnus-dribble-buffer nil)
(defvar gnus-headers-retrieved-by nil)
(defvar gnus-article-reply nil)
(defvar gnus-override-method nil)
(defvar gnus-article-check-size nil)

(defvar gnus-current-score-file nil)
(defvar gnus-newsgroup-adaptive-score-file nil)
(defvar gnus-scores-exclude-files nil)

(defvar gnus-opened-servers nil)

(defvar gnus-current-move-group nil)
(defvar gnus-current-copy-group nil)
(defvar gnus-current-crosspost-group nil)

(defvar gnus-newsgroup-dependencies nil)
(defvar gnus-newsgroup-async nil)
(defconst gnus-group-edit-buffer "*Gnus edit newsgroup*")

(defvar gnus-newsgroup-adaptive nil)

(defvar gnus-summary-display-table nil)
(defvar gnus-summary-display-article-function nil)

(defvar gnus-summary-highlight-line-function nil
  "Function called after highlighting a summary line.")

(defvar gnus-group-line-format-alist
  `((?M gnus-tmp-marked-mark ?c)
    (?S gnus-tmp-subscribed ?c)
    (?L gnus-tmp-level ?d)
    (?N (cond ((eq number t) "*" )
	      ((numberp number) 
	       (int-to-string
		(+ number
		   (gnus-range-length (cdr (assq 'dormant gnus-tmp-marked)))
		   (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))))))
	      (t number)) ?s)
    (?R gnus-tmp-number-of-read ?s)
    (?t gnus-tmp-number-total ?d)
    (?y gnus-tmp-number-of-unread ?s)
    (?I (gnus-range-length (cdr (assq 'dormant gnus-tmp-marked))) ?d)
    (?T (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))) ?d)
    (?i (+ (gnus-range-length (cdr (assq 'dormant gnus-tmp-marked)))
	   (gnus-range-length (cdr (assq 'tick gnus-tmp-marked)))) ?d)
    (?g gnus-tmp-group ?s)
    (?G gnus-tmp-qualified-group ?s)
    (?c (gnus-short-group-name gnus-tmp-group) ?s)
    (?D gnus-tmp-newsgroup-description ?s)
    (?o gnus-tmp-moderated ?c)
    (?O gnus-tmp-moderated-string ?s)
    (?p gnus-tmp-process-marked ?c)
    (?s gnus-tmp-news-server ?s)
    (?n gnus-tmp-news-method ?s)
    (?P gnus-group-indentation ?s)
    (?l gnus-tmp-grouplens ?s)
    (?z gnus-tmp-news-method-string ?s)
    (?u gnus-tmp-user-defined ?s)))

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
    (?t (gnus-summary-number-of-articles-in-thread
	 (and (boundp 'thread) (car thread)) gnus-tmp-level)
	?d)
    (?e (gnus-summary-number-of-articles-in-thread
	 (and (boundp 'thread) (car thread)) gnus-tmp-level t)
	?c)
    (?u gnus-tmp-user-defined ?s))
  "An alist of format specifications that can appear in summary lines,
and what variables they correspond with, along with the type of the
variable (string, integer, character, etc).")

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
    (?E gnus-newsgroup-expunged-tally ?d)
    (?s (gnus-current-score-file-nondirectory) ?s)))

(defvar gnus-article-mode-line-format-alist
  gnus-summary-mode-line-format-alist)

(defvar gnus-group-mode-line-format-alist
  `((?S gnus-tmp-news-server ?s)
    (?M gnus-tmp-news-method ?s)
    (?u gnus-tmp-user-defined ?s)
    (?: gnus-tmp-colon ?s)))

(defvar gnus-have-read-active-file nil)

(defconst gnus-maintainer
  "gnus-bug@ifi.uio.no (The Gnus Bugfixing Girls + Boys)"
  "The mail address of the Gnus maintainers.")

(defconst gnus-version-number "5.3"
  "Version number for this version of Gnus.")

(defconst gnus-version (format "Gnus v%s" gnus-version-number)
  "Version string for this version of Gnus.")

(defvar gnus-info-nodes
  '((gnus-group-mode "(gnus)The Group Buffer")
    (gnus-summary-mode "(gnus)The Summary Buffer")
    (gnus-article-mode "(gnus)The Article Buffer")
    (gnus-server-mode "(gnus)The Server Buffer")
    (gnus-browse-mode "(gnus)Browse Foreign Server")
    (gnus-tree-mode "(gnus)Tree Display")
    )
  "Alist of major modes and related Info nodes.")

(defvar gnus-group-buffer "*Group*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")
(defvar gnus-server-buffer "*Server*")

(defvar gnus-work-buffer " *gnus work*")

(defvar gnus-original-article-buffer " *Original Article*")
(defvar gnus-original-article nil)

(defvar gnus-buffer-list nil
  "Gnus buffers that should be killed on exit.")

(defvar gnus-slave nil
  "Whether this Gnus is a slave or not.")

(defvar gnus-variable-list
  '(gnus-newsrc-options gnus-newsrc-options-n
    gnus-newsrc-last-checked-date
    gnus-newsrc-alist gnus-server-alist
    gnus-killed-list gnus-zombie-list
    gnus-topic-topology gnus-topic-alist
    gnus-format-specs)
  "Gnus variables saved in the quick startup file.")

(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n nil
  "List of regexps representing groups to be subscribed/ignored unconditionally.")

(defvar gnus-newsrc-last-checked-date nil
  "Date Gnus last asked server for new newsgroups.")

(defvar gnus-topic-topology nil
  "The complete topic hierarchy.")

(defvar gnus-topic-alist nil
  "The complete topic-group alist.")

(defvar gnus-newsrc-alist nil
  "Assoc list of read articles.
gnus-newsrc-hashtb should be kept so that both hold the same information.")

(defvar gnus-newsrc-hashtb nil
  "Hashtable of gnus-newsrc-alist.")

(defvar gnus-killed-list nil
  "List of killed newsgroups.")

(defvar gnus-killed-hashtb nil
  "Hash table equivalent of gnus-killed-list.")

(defvar gnus-zombie-list nil
  "List of almost dead newsgroups.")

(defvar gnus-description-hashtb nil
  "Descriptions of newsgroups.")

(defvar gnus-list-of-killed-groups nil
  "List of newsgroups that have recently been killed by the user.")

(defvar gnus-active-hashtb nil
  "Hashtable of active articles.")

(defvar gnus-moderated-list nil
  "List of moderated newsgroups.")

(defvar gnus-group-marked nil)

(defvar gnus-current-startup-file nil
  "Startup file for the current host.")

(defvar gnus-last-search-regexp nil
  "Default regexp for article search command.")

(defvar gnus-last-shell-command nil
  "Default shell command on article.")

(defvar gnus-current-select-method nil
  "The current method for selecting a newsgroup.")

(defvar gnus-group-list-mode nil)

(defvar gnus-article-internal-prepare-hook nil)

(defvar gnus-newsgroup-name nil)
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
(defvar gnus-current-kill-article nil)

;; Save window configuration.
(defvar gnus-prev-winconf nil)

(defvar gnus-summary-mark-positions nil)
(defvar gnus-group-mark-positions nil)

(defvar gnus-reffed-article-number nil)

;;; Let the byte-compiler know that we know about this variable.
(defvar rmail-default-rmail-file)

(defvar gnus-cache-removable-articles nil)

(defvar gnus-dead-summary nil)

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
    gnus-newsgroup-bookmarks gnus-newsgroup-dormant
    gnus-newsgroup-headers gnus-newsgroup-threads
    gnus-newsgroup-prepared gnus-summary-highlight-line-function
    gnus-current-article gnus-current-headers gnus-have-all-headers
    gnus-last-article gnus-article-internal-prepare-hook
    gnus-newsgroup-dependencies gnus-newsgroup-selected-overlay
    gnus-newsgroup-scored gnus-newsgroup-kill-headers
    gnus-newsgroup-async gnus-thread-expunge-below
    gnus-score-alist gnus-current-score-file gnus-summary-expunge-below
    (gnus-summary-mark-below . global)
    gnus-newsgroup-active gnus-scores-exclude-files
    gnus-newsgroup-history gnus-newsgroup-ancient
    gnus-newsgroup-sparse
    (gnus-newsgroup-adaptive . gnus-use-adaptive-scoring)
    gnus-newsgroup-adaptive-score-file
    (gnus-newsgroup-expunged-tally . 0)
    gnus-cache-removable-articles gnus-newsgroup-cached
    gnus-newsgroup-data gnus-newsgroup-data-reverse
    gnus-newsgroup-limit gnus-newsgroup-limits)
  "Variables that are buffer-local to the summary buffers.")

(defconst gnus-bug-message
  "Sending a bug report to the Gnus Towers.
========================================

The buffer below is a mail buffer.  When you press `C-c C-c', it will
be sent to the Gnus Bug Exterminators.

At the bottom of the buffer you'll see lots of variable settings.
Please do not delete those.  They will tell the Bug People what your
environment is, so that it will be easier to locate the bugs.

If you have found a bug that makes Emacs go \"beep\", set
debug-on-error to t (`M-x set-variable RET debug-on-error RET t RET')
and include the backtrace in your bug report.

Please describe the bug in annoying, painstaking detail.

Thank you for your help in stamping out bugs.
")

;;; End of variables.

;; Define some autoload functions Gnus might use.
(eval-and-compile

  ;; This little mapcar goes through the list below and marks the
  ;; symbols in question as autoloaded functions.
  (mapcar
   (lambda (package)
     (let ((interactive (nth 1 (memq ':interactive package))))
       (mapcar
	(lambda (function)
	  (let (keymap)
	    (when (consp function)
	      (setq keymap (car (memq 'keymap function)))
	      (setq function (car function)))
	    (autoload function (car package) nil interactive keymap)))
	(if (eq (nth 1 package) ':interactive)
	    (cdddr package)
	  (cdr package)))))
   '(("metamail" metamail-buffer)
     ("info" Info-goto-node)
     ("hexl" hexl-hex-string-to-integer)
     ("pp" pp pp-to-string pp-eval-expression)
     ("mail-extr" mail-extract-address-components)
     ("nnmail" nnmail-split-fancy nnmail-article-group)
     ("nnvirtual" nnvirtual-catchup-group)
     ("timezone" timezone-make-date-arpa-standard timezone-fix-time
      timezone-make-sortable-date timezone-make-time-string)
     ("rmailout" rmail-output)
     ("rmail" rmail-insert-rmail-file-header rmail-count-new-messages
      rmail-show-message)
     ("gnus-soup" :interactive t
      gnus-group-brew-soup gnus-brew-soup gnus-soup-add-article
      gnus-soup-send-replies gnus-soup-save-areas gnus-soup-pack-packet)
     ("nnsoup" nnsoup-pack-replies)
     ("gnus-scomo" :interactive t gnus-score-mode)
     ("gnus-mh" gnus-mh-mail-setup gnus-summary-save-article-folder
      gnus-Folder-save-name gnus-folder-save-name)
     ("gnus-mh" :interactive t gnus-summary-save-in-folder)
     ("gnus-vis" gnus-group-make-menu-bar gnus-summary-make-menu-bar
      gnus-server-make-menu-bar gnus-article-make-menu-bar
      gnus-browse-make-menu-bar gnus-highlight-selected-summary
      gnus-summary-highlight-line gnus-carpal-setup-buffer
      gnus-group-highlight-line
      gnus-article-add-button gnus-insert-next-page-button
      gnus-insert-prev-page-button gnus-visual-turn-off-edit-menu)
     ("gnus-vis" :interactive t
      gnus-article-push-button gnus-article-press-button
      gnus-article-highlight gnus-article-highlight-some
      gnus-article-highlight-headers gnus-article-highlight-signature
      gnus-article-add-buttons gnus-article-add-buttons-to-head
      gnus-article-next-button gnus-article-prev-button)
     ("gnus-demon" gnus-demon-add-nocem gnus-demon-add-scanmail
      gnus-demon-add-disconnection gnus-demon-add-handler
      gnus-demon-remove-handler)
     ("gnus-demon" :interactive t
      gnus-demon-init gnus-demon-cancel)
     ("gnus-salt" gnus-highlight-selected-tree gnus-possibly-generate-tree
      gnus-tree-open gnus-tree-close)
     ("gnus-nocem" gnus-nocem-scan-groups gnus-nocem-close
      gnus-nocem-unwanted-article-p)
     ("gnus-srvr" gnus-enter-server-buffer gnus-server-set-info)
     ("gnus-srvr" gnus-browse-foreign-server)
     ("gnus-cite" :interactive t
      gnus-article-highlight-citation gnus-article-hide-citation-maybe
      gnus-article-hide-citation gnus-article-fill-cited-article
      gnus-article-hide-citation-in-followups)
     ("gnus-kill" gnus-kill gnus-apply-kill-file-internal
      gnus-kill-file-edit-file gnus-kill-file-raise-followups-to-author
      gnus-execute gnus-expunge)
     ("gnus-cache" gnus-cache-possibly-enter-article gnus-cache-save-buffers
      gnus-cache-possibly-remove-articles gnus-cache-request-article
      gnus-cache-retrieve-headers gnus-cache-possibly-alter-active
      gnus-cache-enter-remove-article gnus-cached-article-p
      gnus-cache-open gnus-cache-close gnus-cache-update-article)
     ("gnus-cache" :interactive t gnus-jog-cache gnus-cache-enter-article
      gnus-cache-remove-article)
     ("gnus-score" :interactive t
      gnus-summary-increase-score gnus-summary-lower-score
      gnus-score-flush-cache gnus-score-close
      gnus-score-raise-same-subject-and-select
      gnus-score-raise-same-subject gnus-score-default
      gnus-score-raise-thread gnus-score-lower-same-subject-and-select
      gnus-score-lower-same-subject gnus-score-lower-thread
      gnus-possibly-score-headers gnus-summary-raise-score 
      gnus-summary-set-score gnus-summary-current-score)
     ("gnus-score"
      (gnus-summary-score-map keymap) gnus-score-save gnus-score-headers
      gnus-current-score-file-nondirectory gnus-score-adaptive
      gnus-score-find-trace gnus-score-file-name)
     ("gnus-edit" :interactive t gnus-score-customize)
     ("gnus-topic" :interactive t gnus-topic-mode)
     ("gnus-topic" gnus-topic-remove-group)
     ("gnus-salt" :interactive t gnus-pick-mode gnus-binary-mode)
     ("gnus-uu" (gnus-uu-extract-map keymap) (gnus-uu-mark-map keymap))
     ("gnus-uu" :interactive t
      gnus-uu-digest-mail-forward gnus-uu-digest-post-forward
      gnus-uu-mark-series gnus-uu-mark-region gnus-uu-mark-buffer
      gnus-uu-mark-by-regexp gnus-uu-mark-all
      gnus-uu-mark-sparse gnus-uu-mark-thread gnus-uu-decode-uu
      gnus-uu-decode-uu-and-save gnus-uu-decode-unshar
      gnus-uu-decode-unshar-and-save gnus-uu-decode-save
      gnus-uu-decode-binhex gnus-uu-decode-uu-view
      gnus-uu-decode-uu-and-save-view gnus-uu-decode-unshar-view
      gnus-uu-decode-unshar-and-save-view gnus-uu-decode-save-view
      gnus-uu-decode-binhex-view)
     ("gnus-msg" (gnus-summary-send-map keymap)
      gnus-mail-yank-original gnus-mail-send-and-exit
      gnus-article-mail gnus-new-mail gnus-mail-reply)
     ("gnus-msg" :interactive t
      gnus-group-post-news gnus-group-mail gnus-summary-post-news
      gnus-summary-followup gnus-summary-followup-with-original
      gnus-summary-cancel-article gnus-summary-supersede-article
      gnus-post-news gnus-inews-news 
      gnus-summary-reply gnus-summary-reply-with-original
      gnus-summary-mail-forward gnus-summary-mail-other-window
      gnus-bug)
     ("gnus-picon" :interactive t gnus-article-display-picons
      gnus-group-display-picons gnus-picons-article-display-x-face
      gnus-picons-display-x-face)
     ("gnus-gl" bbb-login bbb-logout bbb-grouplens-group-p 
      gnus-grouplens-mode)
     ("smiley" :interactive t gnus-smiley-display)
     ("gnus-vm" gnus-vm-mail-setup)
     ("gnus-vm" :interactive t gnus-summary-save-in-vm
      gnus-summary-save-article-vm))))



;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
;; If you want the cursor to go somewhere else, set these two
;; functions in some startup hook to whatever you want.
(defalias 'gnus-summary-position-point 'gnus-goto-colon)
(defalias 'gnus-group-position-point 'gnus-goto-colon)

;;; Various macros and substs.

(defun gnus-header-from (header)
  (mail-header-from header))

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then return to the original window."
  (let ((tempvar (make-symbol "GnusStartBufferWindow"))
	(w (make-symbol "w"))
	(buf (make-symbol "buf")))
    `(let* ((,tempvar (selected-window))
	    (,buf ,buffer)
	    (,w (get-buffer-window ,buf 'visible)))
       (unwind-protect
	   (progn
	     (if ,w
		 (select-window ,w)
	       (pop-to-buffer ,buf))
	     ,@forms)
	 (select-window ,tempvar)))))

(put 'gnus-eval-in-buffer-window 'lisp-indent-function 1)
(put 'gnus-eval-in-buffer-window 'lisp-indent-hook 1)
(put 'gnus-eval-in-buffer-window 'edebug-form-spec '(form body))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  `(symbol-value (intern-soft ,string ,hashtable)))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(set (intern ,string ,hashtable) ,value))

(defmacro gnus-intern-safe (string hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(let ((symbol (intern ,string ,hashtable)))
     (or (boundp symbol)
	 (set symbol nil))
     symbol))

(defmacro gnus-group-unread (group)
  "Get the currently computed number of unread articles in GROUP."
  `(car (gnus-gethash ,group gnus-newsrc-hashtb)))

(defmacro gnus-group-entry (group)
  "Get the newsrc entry for GROUP."
  `(gnus-gethash ,group gnus-newsrc-hashtb))

(defmacro gnus-active (group)
  "Get active info on GROUP."
  `(gnus-gethash ,group gnus-active-hashtb))

(defmacro gnus-set-active (group active)
  "Set GROUP's active info."
  `(gnus-sethash ,group ,active gnus-active-hashtb))

;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;   function `substring' might cut on a middle of multi-octet
;;   character.
(defun gnus-truncate-string (str width)
  (substring str 0 width))

;; Added by Geoffrey T. Dairiki <dairiki@u.washington.edu>.  A safe way
;; to limit the length of a string.  This function is necessary since
;; `(substr "abc" 0 30)' pukes with "Args out of range".
(defsubst gnus-limit-string (str width)
  (if (> (length str) width)
      (substring str 0 width)
    str))

(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines."
  (if (string-match "^[Rr][Ee]: *" subject)
      (substring subject (match-end 0))
    subject))

(defsubst gnus-functionp (form)
  "Return non-nil if FORM is funcallable."
  (or (and (symbolp form) (fboundp form))
      (and (listp form) (eq (car form) 'lambda))))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))

(defmacro gnus-buffer-exists-p (buffer)
  `(let ((buffer ,buffer))
     (and buffer
	  (funcall (if (stringp buffer) 'get-buffer 'buffer-name)
		   buffer))))

(defmacro gnus-kill-buffer (buffer)
  `(let ((buf ,buffer))
     (if (gnus-buffer-exists-p buf)
	 (kill-buffer buf))))

(defsubst gnus-point-at-bol ()
  "Return point at the beginning of the line."
  (let ((p (point)))
    (beginning-of-line)
    (prog1
	(point)
      (goto-char p))))

(defsubst gnus-point-at-eol ()
  "Return point at the end of the line."
  (let ((p (point)))
    (end-of-line)
    (prog1
	(point)
      (goto-char p))))

(defun gnus-alive-p ()
  "Say whether Gnus is running or not."
  (and gnus-group-buffer
       (get-buffer gnus-group-buffer)))

(defun gnus-delete-first (elt list)
  "Delete by side effect the first occurrence of ELT as a member of LIST."
  (if (equal (car list) elt)
      (cdr list)
    (let ((total list))
      (while (and (cdr list)
		  (not (equal (cadr list) elt)))
	(setq list (cdr list)))
      (when (cdr list)
	(setcdr list (cddr list)))
      total)))

;; Delete the current line (and the next N lines.);
(defmacro gnus-delete-line (&optional n)
  `(delete-region (progn (beginning-of-line) (point))
		  (progn (forward-line ,(or n 1)) (point))))

;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
(defvar gnus-init-inhibit nil)
(defun gnus-read-init-file (&optional inhibit-next)
  (if gnus-init-inhibit
      (setq gnus-init-inhibit nil)
    (setq gnus-init-inhibit inhibit-next)
    (and gnus-init-file
	 (or (and (file-exists-p gnus-init-file)
		  ;; Don't try to load a directory.
		  (not (file-directory-p gnus-init-file)))
	     (file-exists-p (concat gnus-init-file ".el"))
	     (file-exists-p (concat gnus-init-file ".elc")))
	 (condition-case var
	     (load gnus-init-file nil t)
	   (error
	    (error "Error in %s: %s" gnus-init-file var))))))

;; Info access macros.

(defmacro gnus-info-group (info)
  `(nth 0 ,info))
(defmacro gnus-info-rank (info)
  `(nth 1 ,info))
(defmacro gnus-info-read (info)
  `(nth 2 ,info))
(defmacro gnus-info-marks (info)
  `(nth 3 ,info))
(defmacro gnus-info-method (info)
  `(nth 4 ,info))
(defmacro gnus-info-params (info)
  `(nth 5 ,info))

(defmacro gnus-info-level (info)
  `(let ((rank (gnus-info-rank ,info)))
     (if (consp rank)
	 (car rank)
       rank)))
(defmacro gnus-info-score (info)
  `(let ((rank (gnus-info-rank ,info)))
     (or (and (consp rank) (cdr rank)) 0)))

(defmacro gnus-info-set-group (info group)
  `(setcar ,info ,group))
(defmacro gnus-info-set-rank (info rank)
  `(setcar (nthcdr 1 ,info) ,rank))
(defmacro gnus-info-set-read (info read)
  `(setcar (nthcdr 2 ,info) ,read))
(defmacro gnus-info-set-marks (info marks)
  `(setcar (nthcdr 3 ,info) ,marks))
(defmacro gnus-info-set-method (info method)
  `(setcar (nthcdr 4 ,info) ,method))
(defmacro gnus-info-set-params (info params)
  `(setcar (nthcdr 5 ,info) ,params))

(defmacro gnus-info-set-level (info level)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcar (car rank) ,level)
       (setcar rank ,level))))
(defmacro gnus-info-set-score (info score)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcdr (car rank) ,score)
       (setcar rank (cons (car rank) ,score)))))

(defmacro gnus-get-info (group)
  `(nth 2 (gnus-gethash ,group gnus-newsrc-hashtb)))

(defun gnus-byte-code (func)
  "Return a form that can be `eval'ed based on FUNC."
  (let ((fval (symbol-function func)))
    (if (byte-code-function-p fval)
	(let ((flist (append fval nil)))
	  (setcar flist 'byte-code)
	  flist)
      (cons 'progn (cddr fval)))))

;; Find out whether the gnus-visual TYPE is wanted.
(defun gnus-visual-p (&optional type class)
  (and gnus-visual			; Has to be non-nil, at least.
       (if (not type)			; We don't care about type.
	   gnus-visual
	 (if (listp gnus-visual)	; It's a list, so we check it.
	     (or (memq type gnus-visual)
		 (memq class gnus-visual))
	   t))))

;;; Load the compatability functions.

(require 'gnus-cus)
(require 'gnus-ems)


;;;
;;; Shutdown
;;;

(defvar gnus-shutdown-alist nil)

(defun gnus-add-shutdown (function &rest symbols)
  "Run FUNCTION whenever one of SYMBOLS is shut down."
  (push (cons function symbols) gnus-shutdown-alist))

(defun gnus-shutdown (symbol)
  "Shut down everything that waits for SYMBOL."
  (let ((alist gnus-shutdown-alist)
	entry)
    (while (setq entry (pop alist))
      (when (memq symbol (cdr entry))
	(funcall (car entry))))))



;; Format specs.  The chunks below are the machine-generated forms
;; that are to be evaled as the result of the default format strings.
;; We write them in here to get them byte-compiled.  That way the
;; default actions will be quite fast, while still retaining the full
;; flexibility of the user-defined format specs.

;; First we have lots of dummy defvars to let the compiler know these
;; are really dynamic variables.

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)
(defvar gnus-tmp-subject)
(defvar gnus-tmp-marked)
(defvar gnus-tmp-marked-mark)
(defvar gnus-tmp-subscribed)
(defvar gnus-tmp-process-marked)
(defvar gnus-tmp-number-of-unread)
(defvar gnus-tmp-group-name)
(defvar gnus-tmp-group)
(defvar gnus-tmp-article-number)
(defvar gnus-tmp-unread-and-unselected)
(defvar gnus-tmp-news-method)
(defvar gnus-tmp-news-server)
(defvar gnus-tmp-article-number)
(defvar gnus-mouse-face)
(defvar gnus-mouse-face-prop)

(defun gnus-summary-line-format-spec ()
  (insert gnus-tmp-unread gnus-tmp-replied
	  gnus-tmp-score-char gnus-tmp-indentation)
  (gnus-put-text-property
   (point)
   (progn
     (insert
      gnus-tmp-opening-bracket
      (format "%4d: %-20s"
	      gnus-tmp-lines
	      (if (> (length gnus-tmp-name) 20)
		  (substring gnus-tmp-name 0 20)
		gnus-tmp-name))
      gnus-tmp-closing-bracket)
     (point))
   gnus-mouse-face-prop gnus-mouse-face)
  (insert " " gnus-tmp-subject-or-nil "\n"))

(defvar gnus-summary-line-format-spec
  (gnus-byte-code 'gnus-summary-line-format-spec))

(defun gnus-summary-dummy-line-format-spec ()
  (insert "*  ")
  (gnus-put-text-property
   (point)
   (progn
     (insert ":				 :")
     (point))
   gnus-mouse-face-prop gnus-mouse-face)
  (insert " " gnus-tmp-subject "\n"))

(defvar gnus-summary-dummy-line-format-spec
  (gnus-byte-code 'gnus-summary-dummy-line-format-spec))

(defun gnus-group-line-format-spec ()
  (insert gnus-tmp-marked-mark gnus-tmp-subscribed
	  gnus-tmp-process-marked
	  gnus-group-indentation
	  (format "%5s: " gnus-tmp-number-of-unread))
  (gnus-put-text-property
   (point)
   (progn
     (insert gnus-tmp-group "\n")
     (1- (point)))
   gnus-mouse-face-prop gnus-mouse-face))
(defvar gnus-group-line-format-spec
  (gnus-byte-code 'gnus-group-line-format-spec))

(defvar gnus-format-specs
  `((version . ,emacs-version)
    (group ,gnus-group-line-format ,gnus-group-line-format-spec)
    (summary-dummy ,gnus-summary-dummy-line-format
		   ,gnus-summary-dummy-line-format-spec)
    (summary ,gnus-summary-line-format ,gnus-summary-line-format-spec)))

(defvar gnus-article-mode-line-format-spec nil)
(defvar gnus-summary-mode-line-format-spec nil)
(defvar gnus-group-mode-line-format-spec nil)

;;; Phew.  All that gruft is over, fortunately.


;;;
;;; Gnus Utility Functions
;;;

(defun gnus-extract-address-components (from)
  (let (name address)
    ;; First find the address - the thing with the @ in it.  This may
    ;; not be accurate in mail addresses, but does the trick most of
    ;; the time in news messages.
    (if (string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" from)
	(setq address (substring from (match-beginning 0) (match-end 0))))
    ;; Then we check whether the "name <address>" format is used.
    (and address
	 ;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>
	 ;; Linear white space is not required.
	 (string-match (concat "[ \t]*<" (regexp-quote address) ">") from)
	 (and (setq name (substring from 0 (match-beginning 0)))
	      ;; Strip any quotes from the name.
	      (string-match "\".*\"" name)
	      (setq name (substring name 1 (1- (match-end 0))))))
    ;; If not, then "address (name)" is used.
    (or name
	(and (string-match "(.+)" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (1- (match-end 0)))))
	(and (string-match "()" from)
	     (setq name address))
	;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>.
	;; XOVER might not support folded From headers.
	(and (string-match "(.*" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (match-end 0)))))
    ;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (list (or name from) (or address from))))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-point-motion-hooks t))
	(nnheader-narrow-to-headers)
	(message-fetch-field field)))))

(defun gnus-goto-colon ()
  (beginning-of-line)
  (search-forward ":" (gnus-point-at-eol) t))

;;;###autoload
(defun gnus-update-format (var)
  "Update the format specification near point."
  (interactive
   (list
    (save-excursion
      (eval-defun nil)
      ;; Find the end of the current word.
      (re-search-forward "[ \t\n]" nil t)
      ;; Search backward.
      (when (re-search-backward "\\(gnus-[-a-z]+-line-format\\)" nil t)
	(match-string 1)))))
  (let* ((type (intern (progn (string-match "gnus-\\([-a-z]+\\)-line" var)
			      (match-string 1 var))))
	 (entry (assq type gnus-format-specs))
	 value spec)
    (when entry
      (setq gnus-format-specs (delq entry gnus-format-specs)))
    (set
     (intern (format "%s-spec" var))
     (gnus-parse-format (setq value (symbol-value (intern var)))
			(symbol-value (intern (format "%s-alist" var)))
			(not (string-match "mode" var))))
    (setq spec (symbol-value (intern (format "%s-spec" var))))
    (push (list type value spec) gnus-format-specs)

    (pop-to-buffer "*Gnus Format*")
    (erase-buffer)
    (lisp-interaction-mode)
    (insert (pp-to-string spec))))

(defun gnus-update-format-specifications (&optional force)
  "Update all (necessary) format specifications."
  ;; Make the indentation array.
  (gnus-make-thread-indent-array)

  ;; See whether all the stored info needs to be flushed.
  (when (or force
	    (not (equal emacs-version
			(cdr (assq 'version gnus-format-specs)))))
    (setq gnus-format-specs nil))

  ;; Go through all the formats and see whether they need updating.
  (let ((types '(summary summary-dummy group
			 summary-mode group-mode article-mode))
	new-format entry type val)
    (while (setq type (pop types))
      ;; Jump to the proper buffer to find out the value of
      ;; the variable, if possible.  (It may be buffer-local.)
      (save-excursion
	(let ((buffer (intern (format "gnus-%s-buffer" type)))
	      val)
	  (when (and (boundp buffer)
		     (setq val (symbol-value buffer))
		     (get-buffer val)
		     (buffer-name (get-buffer val)))
	    (set-buffer (get-buffer val)))
	  (setq new-format (symbol-value
			    (intern (format "gnus-%s-line-format" type))))))
      (setq entry (cdr (assq type gnus-format-specs)))
      (if (and entry
	       (equal (car entry) new-format))
	  ;; Use the old format.
	  (set (intern (format "gnus-%s-line-format-spec" type))
	       (cadr entry))
	;; This is a new format.
	(setq val
	      (if (not (stringp new-format))
		  ;; This is a function call or something.
		  new-format
		;; This is a "real" format.
		(gnus-parse-format
		 new-format
		 (symbol-value
		  (intern (format "gnus-%s-line-format-alist"
				  (if (eq type 'article-mode)
				      'summary-mode type))))
		 (not (string-match "mode$" (symbol-name type))))))
	;; Enter the new format spec into the list.
	(if entry
	    (progn
	      (setcar (cdr entry) val)
	      (setcar entry new-format))
	  (push (list type new-format val) gnus-format-specs))
	(set (intern (format "gnus-%s-line-format-spec" type)) val))))

  (unless (assq 'version gnus-format-specs)
    (push (cons 'version emacs-version) gnus-format-specs))

  (gnus-update-group-mark-positions)
  (gnus-update-summary-mark-positions))

(defun gnus-update-summary-mark-positions ()
  "Compute where the summary marks are to go."
  (save-excursion
    (when (and gnus-summary-buffer
	       (get-buffer gnus-summary-buffer)
	       (buffer-name (get-buffer gnus-summary-buffer)))
      (set-buffer gnus-summary-buffer))
    (let ((gnus-replied-mark 129)
	  (gnus-score-below-mark 130)
	  (gnus-score-over-mark 130)
	  (thread nil)
	  (gnus-visual nil)
	  (spec gnus-summary-line-format-spec)
	  pos)
      (save-excursion
	(gnus-set-work-buffer)
	(let ((gnus-summary-line-format-spec spec))
	  (gnus-summary-insert-line
	   [0 "" "" "" "" "" 0 0 ""]  0 nil 128 t nil "" nil 1)
	  (goto-char (point-min))
	  (setq pos (list (cons 'unread (and (search-forward "\200" nil t)
					     (- (point) 2)))))
	  (goto-char (point-min))
	  (push (cons 'replied (and (search-forward "\201" nil t) 
				    (- (point) 2)))
		pos)
	  (goto-char (point-min))
	  (push (cons 'score (and (search-forward "\202" nil t) (- (point) 2)))
		pos)))
      (setq gnus-summary-mark-positions pos))))

(defun gnus-update-group-mark-positions ()
  (save-excursion
    (let ((gnus-process-mark 128)
	  (gnus-group-marked '("dummy.group"))
	  (gnus-active-hashtb (make-vector 10 0)))
      (gnus-set-active "dummy.group" '(0 . 0))
      (gnus-set-work-buffer)
      (gnus-group-insert-group-line "dummy.group" 0 nil 0 nil)
      (goto-char (point-min))
      (setq gnus-group-mark-positions
	    (list (cons 'process (and (search-forward "\200" nil t)
				      (- (point) 2))))))))

(defvar gnus-mouse-face-0 'highlight)
(defvar gnus-mouse-face-1 'highlight)
(defvar gnus-mouse-face-2 'highlight)
(defvar gnus-mouse-face-3 'highlight)
(defvar gnus-mouse-face-4 'highlight)

(defun gnus-mouse-face-function (form type)
  `(gnus-put-text-property
    (point) (progn ,@form (point))
    gnus-mouse-face-prop
    ,(if (equal type 0)
	 'gnus-mouse-face
       `(quote ,(symbol-value (intern (format "gnus-mouse-face-%d" type)))))))

(defvar gnus-face-0 'bold)
(defvar gnus-face-1 'italic)
(defvar gnus-face-2 'bold-italic)
(defvar gnus-face-3 'bold)
(defvar gnus-face-4 'bold)

(defun gnus-face-face-function (form type)
  `(gnus-put-text-property
    (point) (progn ,@form (point))
    'face ',(symbol-value (intern (format "gnus-face-%d" type)))))

(defun gnus-max-width-function (el max-width)
  (or (numberp max-width) (signal 'wrong-type-argument '(numberp max-width)))
  (if (symbolp el)
      `(if (> (length ,el) ,max-width)
	   (substring ,el 0 ,max-width)
	 ,el)
    `(let ((val (eval ,el)))
       (if (numberp val)
	   (setq val (int-to-string val)))
       (if (> (length val) ,max-width)
	   (substring val 0 ,max-width)
	 val))))

(defun gnus-parse-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  (if (string-match
       "\\`\\(.*\\)%[0-9]?[{(]\\(.*\\)%[0-9]?[})]\\(.*\n?\\)\\'"
       format)
      (gnus-parse-complex-format format spec-alist)
    ;; This is a simple format.
    (gnus-parse-simple-format format spec-alist insert)))

(defun gnus-parse-complex-format (format spec-alist)
  (save-excursion
    (gnus-set-work-buffer)
    (insert format)
    (goto-char (point-min))
    (while (re-search-forward "\"" nil t)
      (replace-match "\\\"" nil t))
    (goto-char (point-min))
    (insert "(\"")
    (while (re-search-forward "%\\([0-9]+\\)?\\([{}()]\\)" nil t)
      (let ((number (if (match-beginning 1)
			(match-string 1) "0"))
	    (delim (aref (match-string 2) 0)))
	(if (or (= delim ?\() (= delim ?\{))
	    (replace-match (concat "\"(" (if (= delim ?\() "mouse" "face")
				   " " number " \""))
	  (replace-match "\")\""))))
    (goto-char (point-max))
    (insert "\")")
    (goto-char (point-min))
    (let ((form (read (current-buffer))))
      (cons 'progn (gnus-complex-form-to-spec form spec-alist)))))

(defun gnus-complex-form-to-spec (form spec-alist)
  (delq nil
	(mapcar
	 (lambda (sform)
	   (if (stringp sform)
	       (gnus-parse-simple-format sform spec-alist t)
	     (funcall (intern (format "gnus-%s-face-function" (car sform)))
		      (gnus-complex-form-to-spec (cddr sform) spec-alist)
		      (nth 1 sform))))
	 form)))

(defun gnus-parse-simple-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return a
  ;; string.
  (let ((max-width 0)
	spec flist fstring newspec elem beg result dontinsert)
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "%[-0-9]*\\(,[0-9]+\\)?\\([^0-9]\\)\\(.\\)?"
				nil t)
	(if (= (setq spec (string-to-char (match-string 2))) ?%)
	      (setq newspec "%"
		    beg (1+ (match-beginning 0)))
	  ;; First check if there are any specs that look anything like
	  ;; "%12,12A", ie. with a "max width specification".  These have
	  ;; to be treated specially.
	  (if (setq beg (match-beginning 1))
	      (setq max-width
		    (string-to-int
		     (buffer-substring
		      (1+ (match-beginning 1)) (match-end 1))))
	    (setq max-width 0)
	    (setq beg (match-beginning 2)))
	  ;; Find the specification from `spec-alist'.
	  (unless (setq elem (cdr (assq spec spec-alist)))
	    (setq elem '("*" ?s)))
	  ;; Treat user defined format specifiers specially.
	  (when (eq (car elem) 'gnus-tmp-user-defined)
	    (setq elem
		  (list
		   (list (intern (concat "gnus-user-format-function-"
					 (match-string 3)))
			 'gnus-tmp-header) ?s))
	    (delete-region (match-beginning 3) (match-end 3)))
	  (if (not (zerop max-width))
	      (let ((el (car elem)))
		(cond ((= (cadr elem) ?c)
		       (setq el (list 'char-to-string el)))
		      ((= (cadr elem) ?d)
		       (setq el (list 'int-to-string el))))
		(setq flist (cons (gnus-max-width-function el max-width)
				  flist))
		(setq newspec ?s))
	    (progn
	      (setq flist (cons (car elem) flist))
	      (setq newspec (cadr elem)))))
	;; Remove the old specification (and possibly a ",12" string).
	(delete-region beg (match-end 2))
	;; Insert the new specification.
	(goto-char beg)
	(insert newspec))
      (setq fstring (buffer-substring 1 (point-max))))
    ;; Do some postprocessing to increase efficiency.
    (setq
     result
     (cond
      ;; Emptyness.
      ((string= fstring "")
       nil)
      ;; Not a format string.
      ((not (string-match "%" fstring))
       (list fstring))
      ;; A format string with just a single string spec.
      ((string= fstring "%s")
       (list (car flist)))
      ;; A single character.
      ((string= fstring "%c")
       (list (car flist)))
      ;; A single number.
      ((string= fstring "%d")
       (setq dontinsert)
       (if insert
	   (list `(princ ,(car flist)))
	 (list `(int-to-string ,(car flist)))))
      ;; Just lots of chars and strings.
      ((string-match "\\`\\(%[cs]\\)+\\'" fstring)
       (nreverse flist))
      ;; A single string spec at the beginning of the spec.
      ((string-match "\\`%[sc][^%]+\\'" fstring)
       (list (car flist) (substring fstring 2)))
      ;; A single string spec in the middle of the spec.
      ((string-match "\\`\\([^%]+\\)%[sc]\\([^%]+\\)\\'" fstring)
       (list (match-string 1 fstring) (car flist) (match-string 2 fstring)))
      ;; A single string spec in the end of the spec.
      ((string-match "\\`\\([^%]+\\)%[sc]\\'" fstring)
       (list (match-string 1 fstring) (car flist)))
      ;; A more complex spec.
      (t
       (list (cons 'format (cons fstring (nreverse flist)))))))

    (if insert
	(when result
	  (if dontinsert
	      result
	    (cons 'insert result)))
      (cond ((stringp result)
	     result)
	    ((consp result)
	     (cons 'concat result))
	    (t "")))))

(defun gnus-eval-format (format &optional alist props)
  "Eval the format variable FORMAT, using ALIST.
If PROPS, insert the result."
  (let ((form (gnus-parse-format format alist props)))
    (if props
	(gnus-add-text-properties (point) (progn (eval form) (point)) props)
      (eval form))))

(defun gnus-remove-text-with-property (prop)
  "Delete all text in the current buffer with text property PROP."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (while (get-text-property (point) prop)
	(delete-char 1))
      (goto-char (next-single-property-change (point) prop nil (point-max))))))

(defun gnus-set-work-buffer ()
  (if (get-buffer gnus-work-buffer)
      (progn
	(set-buffer gnus-work-buffer)
	(erase-buffer))
    (set-buffer (get-buffer-create gnus-work-buffer))
    (kill-all-local-variables)
    (buffer-disable-undo (current-buffer))
    (gnus-add-current-to-buffer-list)))

;; Article file names when saving.

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.	Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-Plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/News.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   (gnus-capitalize-newsgroup newsgroup)
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       gnus-article-save-directory)))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       gnus-article-save-directory)))

;; For subscribing new newsgroup

(defun gnus-subscribe-hierarchical-interactive (groups)
  (let ((groups (sort groups 'string<))
	prefixes prefix start ans group starts)
    (while groups
      (setq prefixes (list "^"))
      (while (and groups prefixes)
	(while (not (string-match (car prefixes) (car groups)))
	  (setq prefixes (cdr prefixes)))
	(setq prefix (car prefixes))
	(setq start (1- (length prefix)))
	(if (and (string-match "[^\\.]\\." (car groups) start)
		 (cdr groups)
		 (setq prefix
		       (concat "^" (substring (car groups) 0 (match-end 0))))
		 (string-match prefix (cadr groups)))
	    (progn
	      (setq prefixes (cons prefix prefixes))
	      (message "Descend hierarchy %s? ([y]nsq): "
		       (substring prefix 1 (1- (length prefix))))
	      (while (not (memq (setq ans (read-char)) '(?y ?\n ?n ?s ?q)))
		(ding)
		(message "Descend hierarchy %s? ([y]nsq): "
			 (substring prefix 1 (1- (length prefix)))))
	      (cond ((= ans ?n)
		     (while (and groups
				 (string-match prefix
					       (setq group (car groups))))
		       (setq gnus-killed-list
			     (cons group gnus-killed-list))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?s)
		     (while (and groups
				 (string-match prefix
					       (setq group (car groups))))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (gnus-subscribe-alphabetically (car groups))
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?q)
		     (while groups
		       (setq group (car groups))
		       (setq gnus-killed-list (cons group gnus-killed-list))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups))))
		    (t nil)))
	  (message "Subscribe %s? ([n]yq)" (car groups))
	  (while (not (memq (setq ans (read-char)) '(?y ?\n ?q ?n)))
	    (ding)
	    (message "Subscribe %s? ([n]yq)" (car groups)))
	  (setq group (car groups))
	  (cond ((= ans ?y)
		 (gnus-subscribe-alphabetically (car groups))
		 (gnus-sethash group group gnus-killed-hashtb))
		((= ans ?q)
		 (while groups
		   (setq group (car groups))
		   (setq gnus-killed-list (cons group gnus-killed-list))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (setq groups (cdr groups))))
		(t
		 (setq gnus-killed-list (cons group gnus-killed-list))
		 (gnus-sethash group group gnus-killed-hashtb)))
	  (setq groups (cdr groups)))))))

(defun gnus-subscribe-randomly (newsgroup)
  "Subscribe new NEWSGROUP by making it the first newsgroup."
  (gnus-subscribe-newsgroup newsgroup))

(defun gnus-subscribe-alphabetically (newgroup)
  "Subscribe new NEWSGROUP and insert it in alphabetical order."
  (let ((groups (cdr gnus-newsrc-alist))
	before)
    (while (and (not before) groups)
      (if (string< newgroup (caar groups))
	  (setq before (caar groups))
	(setq groups (cdr groups))))
    (gnus-subscribe-newsgroup newgroup before)))

(defun gnus-subscribe-hierarchically (newgroup)
  "Subscribe new NEWSGROUP and insert it in hierarchical newsgroup order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (save-excursion
    (set-buffer (find-file-noselect gnus-current-startup-file))
    (let ((groupkey newgroup)
	  before)
      (while (and (not before) groupkey)
	(goto-char (point-min))
	(let ((groupkey-re
	       (concat "^\\(" (regexp-quote groupkey) ".*\\)[!:]")))
	  (while (and (re-search-forward groupkey-re nil t)
		      (progn
			(setq before (match-string 1))
			(string< before newgroup)))))
	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	(setq groupkey
	      (if (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		  (substring groupkey (match-beginning 1) (match-end 1)))))
      (gnus-subscribe-newsgroup newgroup before))
    (kill-buffer (current-buffer))))

(defun gnus-subscribe-interactively (group)
  "Subscribe the new GROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed.  If not,
it is killed."
  (if (gnus-y-or-n-p (format "Subscribe new newsgroup: %s " group))
      (gnus-subscribe-hierarchically group)
    (push group gnus-killed-list)))

(defun gnus-subscribe-zombies (group)
  "Make the new GROUP into a zombie group."
  (push group gnus-zombie-list))

(defun gnus-subscribe-killed (group)
  "Make the new GROUP a killed group."
  (push group gnus-killed-list))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If NEXT is non-nil, it is inserted before NEXT.	 Otherwise it is made
the first newsgroup."
  ;; We subscribe the group by changing its level to `subscribed'.
  (gnus-group-change-level
   newsgroup gnus-level-default-subscribed
   gnus-level-killed (gnus-gethash (or next "dummy.group") gnus-newsrc-hashtb))
  (gnus-message 5 "Subscribe newsgroup: %s" newsgroup))

;; For directories

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let ((newsgroup (gnus-newsgroup-savable-name newsgroup))
	(len (length newsgroup))
	idx)
    ;; If this is a foreign group, we don't want to translate the
    ;; entire name.
    (if (setq idx (string-match ":" newsgroup))
	(aset newsgroup idx ?/)
      (setq idx 0))
    ;; Replace all occurrences of `.' with `/'.
    (while (< idx len)
      (if (= (aref newsgroup idx) ?.)
	  (aset newsgroup idx ?/))
      (setq idx (1+ idx)))
    newsgroup))

(defun gnus-newsgroup-savable-name (group)
  ;; Replace any slashes in a group name (eg. an ange-ftp nndoc group)
  ;; with dots.
  (nnheader-replace-chars-in-string group ?/ ?.))

(defun gnus-make-directory (dir)
  "Make DIRECTORY recursively."
  ;; Why don't we use `(make-directory dir 'parents)'?  That's just one
  ;; of the many mysteries of the universe.
  (let* ((dir (expand-file-name dir default-directory))
	 dirs err)
    (if (string-match "/$" dir)
	(setq dir (substring dir 0 (match-beginning 0))))
    ;; First go down the path until we find a directory that exists.
    (while (not (file-exists-p dir))
      (setq dirs (cons dir dirs))
      (string-match "/[^/]+$" dir)
      (setq dir (substring dir 0 (match-beginning 0))))
    ;; Then create all the subdirs.
    (while (and dirs (not err))
      (condition-case ()
	  (make-directory (car dirs))
	(error (setq err t)))
      (setq dirs (cdr dirs)))
    ;; We return whether we were successful or not.
    (not dirs)))

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name."
  (and (not (zerop (length newsgroup)))
       (concat (char-to-string (upcase (aref newsgroup 0)))
	       (substring newsgroup 1))))

;; Various... things.

(defun gnus-simplify-subject (subject &optional re-only)
  "Remove `Re:' and words in parentheses.
If RE-ONLY is non-nil, strip leading `Re:'s only."
  (let ((case-fold-search t))		;Ignore case.
    ;; Remove `Re:', `Re^N:', `Re(n)', and `Re[n]:'.
    (when (string-match "\\`\\(re\\([[(^][0-9]+[])]?\\)?:[ \t]*\\)+" subject)
      (setq subject (substring subject (match-end 0))))
    ;; Remove uninteresting prefixes.
    (if (and (not re-only)
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
;; Written by Stainless Steel Rat <ratinox@ccs.neu.edu>.
(defun gnus-simplify-buffer-fuzzy ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (search-forward "\t" nil t)
      (replace-match " " t t))
    (goto-char (point-min))
    (re-search-forward "^ *\\(re\\|fwd\\)[[{(^0-9]*[])}]?[:;] *" nil t)
    (goto-char (match-beginning 0))
    (while (or
	    (looking-at "^ *\\(re\\|fwd\\)[[{(^0-9]*[])}]?[:;] *")
	    (looking-at "^[[].*: .*[]]$"))
      (goto-char (point-min))
      (while (re-search-forward "^ *\\(re\\|fwd\\)[[{(^0-9]*[])}]?[:;] *"
				nil t)
	(replace-match "" t t))
      (goto-char (point-min))
      (while (re-search-forward "^[[].*: .*[]]$" nil t)
	(goto-char (match-end 0))
	(delete-char -1)
	(delete-region
	 (progn (goto-char (match-beginning 0)))
	 (re-search-forward ":"))))
    (goto-char (point-min))
    (while (re-search-forward " *[[{(][^()\n]*[]})] *$" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (while (re-search-forward "  +" nil t)
      (replace-match " " t t))
    (goto-char (point-min))
    (while (re-search-forward " $" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (while (re-search-forward "^ +" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (when gnus-simplify-subject-fuzzy-regexp
      (if (listp gnus-simplify-subject-fuzzy-regexp)
	  (let ((list gnus-simplify-subject-fuzzy-regexp))
	    (while list
	      (goto-char (point-min))
	      (while (re-search-forward (car list) nil t)
		(replace-match "" t t))
	      (setq list (cdr list))))
	(while (re-search-forward gnus-simplify-subject-fuzzy-regexp nil t)
	  (replace-match "" t t))))))

(defun gnus-simplify-subject-fuzzy (subject)
  "Siplify a subject string fuzzily."
  (save-excursion
    (gnus-set-work-buffer)
    (let ((case-fold-search t))
      (insert subject)
      (inline (gnus-simplify-buffer-fuzzy))
      (buffer-string))))

;; Add the current buffer to the list of buffers to be killed on exit.
(defun gnus-add-current-to-buffer-list ()
  (or (memq (current-buffer) gnus-buffer-list)
      (setq gnus-buffer-list (cons (current-buffer) gnus-buffer-list))))

(defun gnus-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

(defun gnus-read-active-file-p ()
  "Say whether the active file has been read from `gnus-select-method'."
  (memq gnus-select-method gnus-have-read-active-file))

;;; General various misc type functions.

(defun gnus-clear-system ()
  "Clear all variables and buffers."
  ;; Clear Gnus variables.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  ;; Clear other internal variables.
  (setq gnus-list-of-killed-groups nil
	gnus-have-read-active-file nil
	gnus-newsrc-alist nil
	gnus-newsrc-hashtb nil
	gnus-killed-list nil
	gnus-zombie-list nil
	gnus-killed-hashtb nil
	gnus-active-hashtb nil
	gnus-moderated-list nil
	gnus-description-hashtb nil
	gnus-current-headers nil
	gnus-thread-indent-array nil
	gnus-newsgroup-headers nil
	gnus-newsgroup-name nil
	gnus-server-alist nil
	gnus-group-list-mode nil
	gnus-opened-servers nil
	gnus-group-mark-positions nil
	gnus-newsgroup-data nil
	gnus-newsgroup-unreads nil
	nnoo-state-alist nil
	gnus-current-select-method nil)
  (gnus-shutdown 'gnus)
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  ;; Clear the dribble buffer.
  (gnus-dribble-clear)
  ;; Kill global KILL file buffer.
  (when (get-file-buffer (gnus-newsgroup-kill-file nil))
    (kill-buffer (get-file-buffer (gnus-newsgroup-kill-file nil))))
  (gnus-kill-buffer nntp-server-buffer)
  ;; Kill Gnus buffers.
  (while gnus-buffer-list
    (gnus-kill-buffer (pop gnus-buffer-list)))
  ;; Remove Gnus frames.
  (gnus-kill-gnus-frames))

(defun gnus-kill-gnus-frames ()
  "Kill all frames Gnus has created."
  (while gnus-created-frames
    (when (frame-live-p (car gnus-created-frames))
      ;; We slap a condition-case around this `delete-frame' to ensure 
      ;; against errors if we try do delete the single frame that's left.
      (condition-case ()
	  (delete-frame (car gnus-created-frames))
	(error nil)))
    (pop gnus-created-frames)))

(defun gnus-windows-old-to-new (setting)
  ;; First we take care of the really, really old Gnus 3 actions.
  (when (symbolp setting)
    (setq setting
	  ;; Take care of ooold GNUS 3.x values.
	  (cond ((eq setting 'SelectArticle) 'article)
		((memq setting '(SelectSubject ExpandSubject)) 'summary)
		((memq setting '(SelectNewsgroup ExitNewsgroup)) 'group)
		(t setting))))
  (if (or (listp setting)
	  (not (and gnus-window-configuration
		    (memq setting '(group summary article)))))
      setting
    (let* ((setting (if (eq setting 'group)
			(if (assq 'newsgroup gnus-window-configuration)
			    'newsgroup
			  'newsgroups) setting))
	   (elem (cadr (assq setting gnus-window-configuration)))
	   (total (apply '+ elem))
	   (types '(group summary article))
	   (pbuf (if (eq setting 'newsgroups) 'group 'summary))
	   (i 0)
	   perc
	   out)
      (while (< i 3)
	(or (not (numberp (nth i elem)))
	    (zerop (nth i elem))
	    (progn
	      (setq perc (if (= i 2)
			     1.0
			   (/ (float (nth 0 elem)) total)))
	      (setq out (cons (if (eq pbuf (nth i types))
				  (list (nth i types) perc 'point)
				(list (nth i types) perc))
			      out))))
	(setq i (1+ i)))
      `(vertical 1.0 ,@(nreverse out)))))

;;;###autoload
(defun gnus-add-configuration (conf)
  "Add the window configuration CONF to `gnus-buffer-configuration'."
  (setq gnus-buffer-configuration
	(cons conf (delq (assq (car conf) gnus-buffer-configuration)
			 gnus-buffer-configuration))))

(defvar gnus-frame-list nil)

(defun gnus-configure-frame (split &optional window)
  "Split WINDOW according to SPLIT."
  (unless window
    (setq window (get-buffer-window (current-buffer))))
  (select-window window)
  ;; This might be an old-stylee buffer config.
  (when (vectorp split)
    (setq split (append split nil)))
  (when (or (consp (car split))
	    (vectorp (car split)))
    (push 1.0 split)
    (push 'vertical split))
  ;; The SPLIT might be something that is to be evaled to
  ;; return a new SPLIT.
  (while (and (not (assq (car split) gnus-window-to-buffer))
	      (gnus-functionp (car split)))
    (setq split (eval split)))
  (let* ((type (car split))
	 (subs (cddr split))
	 (len (if (eq type 'horizontal) (window-width) (window-height)))
	 (total 0)
	 (window-min-width (or gnus-window-min-width window-min-width))
	 (window-min-height (or gnus-window-min-height window-min-height))
	 s result new-win rest comp-subs size sub)
    (cond
     ;; Nothing to do here.
     ((null split))
     ;; Don't switch buffers.
     ((null type)
      (and (memq 'point split) window))
     ;; This is a buffer to be selected.
     ((not (memq type '(frame horizontal vertical)))
      (let ((buffer (cond ((stringp type) type)
			  (t (cdr (assq type gnus-window-to-buffer)))))
	    buf)
	(unless buffer
	  (error "Illegal buffer type: %s" type))
	(unless (setq buf (get-buffer (if (symbolp buffer)
					  (symbol-value buffer) buffer)))
	  (setq buf (get-buffer-create (if (symbolp buffer)
					   (symbol-value buffer) buffer))))
	(switch-to-buffer buf)
	;; We return the window if it has the `point' spec.
	(and (memq 'point split) window)))
     ;; This is a frame split.
     ((eq type 'frame)
      (unless gnus-frame-list
	(setq gnus-frame-list (list (window-frame
				     (get-buffer-window (current-buffer))))))
      (let ((i 0)
	    params frame fresult)
	(while (< i (length subs))
	  ;; Frame parameter is gotten from the sub-split.
	  (setq params (cadr (elt subs i)))
	  ;; It should be a list.
	  (unless (listp params)
	    (setq params nil))
	  ;; Create a new frame?
	  (unless (setq frame (elt gnus-frame-list i))
	    (nconc gnus-frame-list (list (setq frame (make-frame params))))
	    (push frame gnus-created-frames))
	  ;; Is the old frame still alive?
	  (unless (frame-live-p frame)
	    (setcar (nthcdr i gnus-frame-list)
		    (setq frame (make-frame params))))
	  ;; Select the frame in question and do more splits there.
	  (select-frame frame)
	  (setq fresult (or (gnus-configure-frame (elt subs i)) fresult))
	  (incf i))
	;; Select the frame that has the selected buffer.
	(when fresult
	  (select-frame (window-frame fresult)))))
     ;; This is a normal split.
     (t
      (when (> (length subs) 0)
	;; First we have to compute the sizes of all new windows.
	(while subs
	  (setq sub (append (pop subs) nil))
	  (while (and (not (assq (car sub) gnus-window-to-buffer))
		      (gnus-functionp (car sub)))
	    (setq sub (eval sub)))
	  (when sub
	    (push sub comp-subs)
	    (setq size (cadar comp-subs))
	    (cond ((equal size 1.0)
		   (setq rest (car comp-subs))
		   (setq s 0))
		  ((floatp size)
		   (setq s (floor (* size len))))
		  ((integerp size)
		   (setq s size))
		  (t
		   (error "Illegal size: %s" size)))
	    ;; Try to make sure that we are inside the safe limits.
	    (cond ((zerop s))
		  ((eq type 'horizontal)
		   (setq s (max s window-min-width)))
		  ((eq type 'vertical)
		   (setq s (max s window-min-height))))
	    (setcar (cdar comp-subs) s)
	    (incf total s)))
	;; Take care of the "1.0" spec.
	(if rest
	    (setcar (cdr rest) (- len total))
	  (error "No 1.0 specs in %s" split))
	;; The we do the actual splitting in a nice recursive
	;; fashion.
	(setq comp-subs (nreverse comp-subs))
	(while comp-subs
	  (if (null (cdr comp-subs))
	      (setq new-win window)
	    (setq new-win
		  (split-window window (cadar comp-subs)
				(eq type 'horizontal))))
	  (setq result (or (gnus-configure-frame
			    (car comp-subs) window) result))
	  (select-window new-win)
	  (setq window new-win)
	  (setq comp-subs (cdr comp-subs))))
      ;; Return the proper window, if any.
      (when result
	(select-window result))))))

(defvar gnus-frame-split-p nil)

(defun gnus-configure-windows (setting &optional force)
  (setq setting (gnus-windows-old-to-new setting))
  (let ((split (if (symbolp setting)
		   (cadr (assq setting gnus-buffer-configuration))
		 setting))
	all-visible)

    (setq gnus-frame-split-p nil)

    (unless split
      (error "No such setting: %s" setting))

    (if (and (setq all-visible (gnus-all-windows-visible-p split))
	     (not force))
	;; All the windows mentioned are already visible, so we just
	;; put point in the assigned buffer, and do not touch the
	;; winconf.
	(select-window all-visible)

      ;; Either remove all windows or just remove all Gnus windows.
      (let ((frame (selected-frame)))
	(unwind-protect
	    (if gnus-use-full-window
		;; We want to remove all other windows.
		(if (not gnus-frame-split-p)
		    ;; This is not a `frame' split, so we ignore the
		    ;; other frames.  
		    (delete-other-windows)
		  ;; This is a `frame' split, so we delete all windows
		  ;; on all frames.
		  (mapcar 
		   (lambda (frame)
		     (unless (eq (cdr (assq 'minibuffer
					    (frame-parameters frame)))
				 'only)
		       (select-frame frame)
		       (delete-other-windows)))
		   (frame-list)))
	      ;; Just remove some windows.
	      (gnus-remove-some-windows)
	      (switch-to-buffer nntp-server-buffer))
	  (select-frame frame)))

      (switch-to-buffer nntp-server-buffer)
      (gnus-configure-frame split (get-buffer-window (current-buffer))))))

(defun gnus-all-windows-visible-p (split)
  "Say whether all buffers in SPLIT are currently visible.
In particular, the value returned will be the window that
should have point."
  (let ((stack (list split))
	(all-visible t)
	type buffer win buf)
    (while (and (setq split (pop stack))
		all-visible)
      ;; Be backwards compatible.
      (when (vectorp split)
	(setq split (append split nil)))
      (when (or (consp (car split))
		(vectorp (car split)))
	(push 1.0 split)
	(push 'vertical split))
      ;; The SPLIT might be something that is to be evaled to
      ;; return a new SPLIT.
      (while (and (not (assq (car split) gnus-window-to-buffer))
		  (gnus-functionp (car split)))
	(setq split (eval split)))

      (setq type (elt split 0))
      (cond
       ;; Nothing here.
       ((null split) t)
       ;; A buffer.
       ((not (memq type '(horizontal vertical frame)))
	(setq buffer (cond ((stringp type) type)
			   (t (cdr (assq type gnus-window-to-buffer)))))
	(unless buffer
	  (error "Illegal buffer type: %s" type))
	(when (setq buf (get-buffer (if (symbolp buffer)
					(symbol-value buffer)
				      buffer)))
	  (setq win (get-buffer-window buf t)))
	(if win
	    (when (memq 'point split)
		(setq all-visible win))
	  (setq all-visible nil)))
       (t
	(when (eq type 'frame)
	  (setq gnus-frame-split-p t))
	(setq stack (append (cddr split) stack)))))
    (unless (eq all-visible t)
      all-visible)))

(defun gnus-window-top-edge (&optional window)
  (nth 1 (window-edges window)))

(defun gnus-remove-some-windows ()
  (let ((buffers gnus-window-to-buffer)
	buf bufs lowest-buf lowest)
    (save-excursion
      ;; Remove windows on all known Gnus buffers.
      (while buffers
	(setq buf (cdar buffers))
	(if (symbolp buf)
	    (setq buf (and (boundp buf) (symbol-value buf))))
	(and buf
	     (get-buffer-window buf)
	     (progn
	       (setq bufs (cons buf bufs))
	       (pop-to-buffer buf)
	       (if (or (not lowest)
		       (< (gnus-window-top-edge) lowest))
		   (progn
		     (setq lowest (gnus-window-top-edge))
		     (setq lowest-buf buf)))))
	(setq buffers (cdr buffers)))
      ;; Remove windows on *all* summary buffers.
      (walk-windows
       (lambda (win)
	 (let ((buf (window-buffer win)))
	   (if (string-match	"^\\*Summary" (buffer-name buf))
	       (progn
		 (setq bufs (cons buf bufs))
		 (pop-to-buffer buf)
		 (if (or (not lowest)
			 (< (gnus-window-top-edge) lowest))
		     (progn
		       (setq lowest-buf buf)
		       (setq lowest (gnus-window-top-edge)))))))))
      (and lowest-buf
	   (progn
	     (pop-to-buffer lowest-buf)
	     (switch-to-buffer nntp-server-buffer)))
      (while bufs
	(and (not (eq (car bufs) lowest-buf))
	     (delete-windows-on (car bufs)))
	(setq bufs (cdr bufs))))))

(defun gnus-version (&optional arg)
  "Version number of this version of Gnus.
If ARG, insert string at point."
  (interactive "P")
  (let ((methods gnus-valid-select-methods)
	(mess gnus-version)
	meth)
    ;; Go through all the legal select methods and add their version
    ;; numbers to the total version string.  Only the backends that are
    ;; currently in use will have their message numbers taken into
    ;; consideration.
    (while methods
      (setq meth (intern (concat (caar methods) "-version")))
      (and (boundp meth)
	   (stringp (symbol-value meth))
	   (setq mess (concat mess "; " (symbol-value meth))))
      (setq methods (cdr methods)))
    (if arg
	(insert (message mess))
      (message mess))))

(defun gnus-info-find-node ()
  "Find Info documentation of Gnus."
  (interactive)
  ;; Enlarge info window if needed.
  (let ((mode major-mode)
	gnus-info-buffer)
    (Info-goto-node (cadr (assq mode gnus-info-nodes)))
    (setq gnus-info-buffer (current-buffer))
    (gnus-configure-windows 'info)))

(defun gnus-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (- (gnus-day-number date1) (gnus-day-number date2)))

(defun gnus-day-number (date)
  (let ((dat (mapcar (lambda (s) (and s (string-to-int s)) )
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian
     (nth 1 dat) (nth 2 dat) (car dat))))

(defun gnus-encode-date (date)
  "Convert DATE to internal time."
  (let* ((parse (timezone-parse-date date))
	 (date (mapcar (lambda (d) (and d (string-to-int d))) parse))
	 (time (mapcar 'string-to-int (timezone-parse-time (aref parse 3)))))
    (encode-time (caddr time) (cadr time) (car time)
		 (caddr date) (cadr date) (car date) (nth 4 date))))

(defun gnus-time-minus (t1 t2)
  "Subtract two internal times."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun gnus-file-newer-than (file date)
  (let ((fdate (nth 5 (file-attributes file))))
    (or (> (car fdate) (car date))
	(and (= (car fdate) (car date))
	     (> (nth 1 fdate) (nth 1 date))))))

(defmacro gnus-local-set-keys (&rest plist)
  "Set the keys in PLIST in the current keymap."
  `(gnus-define-keys-1 (current-local-map) ',plist))

(defmacro gnus-define-keys (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  `(gnus-define-keys-1 (quote ,keymap) (quote ,plist)))

(put 'gnus-define-keys 'lisp-indent-function 1)
(put 'gnus-define-keys 'lisp-indent-hook 1)
(put 'gnus-define-keymap 'lisp-indent-function 1)
(put 'gnus-define-keymap 'lisp-indent-hook 1)

(defmacro gnus-define-keymap (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  `(gnus-define-keys-1 ,keymap (quote ,plist)))

(defun gnus-define-keys-1 (keymap plist)
  (when (null keymap)
    (error "Can't set keys in a null keymap"))
  (cond ((symbolp keymap)
	 (setq keymap (symbol-value keymap)))
	((keymapp keymap))
	((listp keymap)
	 (set (car keymap) nil)
	 (define-prefix-command (car keymap))
	 (define-key (symbol-value (caddr keymap)) (cadr keymap) (car keymap))
	 (setq keymap (symbol-value (car keymap)))))
  (let (key)
    (while plist
      (when (symbolp (setq key (pop plist)))
	(setq key (symbol-value key)))
      (define-key keymap key (pop plist)))))

(defun gnus-group-read-only-p (&optional group)
  "Check whether GROUP supports editing or not.
If GROUP is nil, `gnus-newsgroup-name' will be checked instead.	 Note
that that variable is buffer-local to the summary buffers."
  (let ((group (or group gnus-newsgroup-name)))
    (not (gnus-check-backend-function 'request-replace-article group))))

(defun gnus-group-total-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (or (memq 'total-expire params)
	(cdr (assq 'total-expire params)) ; (total-expire . t)
	(and gnus-total-expirable-newsgroups ; Check var.
	     (string-match gnus-total-expirable-newsgroups group)))))

(defun gnus-group-auto-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (or (memq 'auto-expire params)
	(cdr (assq 'auto-expire params)) ; (auto-expire . t)
	(and gnus-auto-expirable-newsgroups ; Check var.
	     (string-match gnus-auto-expirable-newsgroups group)))))

(defun gnus-virtual-group-p (group)
  "Say whether GROUP is virtual or not."
  (memq 'virtual (assoc (symbol-name (car (gnus-find-method-for-group group)))
			gnus-valid-select-methods)))

(defun gnus-news-group-p (group &optional article)
  "Return non-nil if GROUP (and ARTICLE) come from a news server."
  (or (gnus-member-of-valid 'post group) ; Ordinary news group.
      (and (gnus-member-of-valid 'post-mail group) ; Combined group.
	   (eq (gnus-request-type group article) 'news))))

(defsubst gnus-simplify-subject-fully (subject)
  "Simplify a subject string according to the user's wishes."
  (cond
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
  "Check whether two subjects are equal.  If optional argument
simple-first is t, first argument is already simplified."
  (cond
   ((null simple-first)
    (equal (gnus-simplify-subject-fully s1)
	   (gnus-simplify-subject-fully s2)))
   (t
    (equal s1
	   (gnus-simplify-subject-fully s2)))))

;; Returns a list of writable groups.
(defun gnus-writable-groups ()
  (let ((alist gnus-newsrc-alist)
	groups group)
    (while (setq group (car (pop alist)))
      (unless (gnus-group-read-only-p group)
	(push group groups)))
    (nreverse groups)))

(defun gnus-completing-read (default prompt &rest args)
  ;; Like `completing-read', except that DEFAULT is the default argument.
  (let* ((prompt (if default 
		     (concat prompt " (default " default ") ")
		   (concat prompt " ")))
	 (answer (apply 'completing-read prompt args)))
    (if (or (null answer) (zerop (length answer)))
	default
      answer)))

;; Two silly functions to ensure that all `y-or-n-p' questions clear
;; the echo area.
(defun gnus-y-or-n-p (prompt)
  (prog1
      (y-or-n-p prompt)
    (message "")))

(defun gnus-yes-or-no-p (prompt)
  (prog1
      (yes-or-no-p prompt)
    (message "")))

;; Check whether to use long file names.
(defun gnus-use-long-file-name (symbol)
  ;; The variable has to be set...
  (and gnus-use-long-file-name
       ;; If it isn't a list, then we return t.
       (or (not (listp gnus-use-long-file-name))
	   ;; If it is a list, and the list contains `symbol', we
	   ;; return nil.
	   (not (memq symbol gnus-use-long-file-name)))))

;; I suspect there's a better way, but I haven't taken the time to do
;; it yet. -erik selberg@cs.washington.edu
(defun gnus-dd-mmm (messy-date)
  "Return a string like DD-MMM from a big messy string"
  (let ((datevec (condition-case () (timezone-parse-date messy-date) 
		   (error nil))))
    (if (not datevec)
	"??-???"
      (format "%2s-%s"
	      (condition-case ()
		  ;; Make sure leading zeroes are stripped.
		  (number-to-string (string-to-number (aref datevec 2)))
		(error "??"))
	      (capitalize
	       (or (car
		    (nth (1- (string-to-number (aref datevec 1)))
			 timezone-months-assoc))
		   "???"))))))

(defun gnus-mode-string-quote (string)
  "Quote all \"%\" in STRING."
  (save-excursion
    (gnus-set-work-buffer)
    (insert string)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (insert "%"))
    (buffer-string)))

;; Make a hash table (default and minimum size is 255).
;; Optional argument HASHSIZE specifies the table size.
(defun gnus-make-hashtable (&optional hashsize)
  (make-vector (if hashsize (max (gnus-create-hash-size hashsize) 255) 255) 0))

;; Make a number that is suitable for hashing; bigger than MIN and one
;; less than 2^x.
(defun gnus-create-hash-size (min)
  (let ((i 1))
    (while (< i min)
      (setq i (* 2 i)))
    (1- i)))

;; Show message if message has a lower level than `gnus-verbose'.
;; Guideline for numbers:
;; 1 - error messages, 3 - non-serious error messages, 5 - messages
;; for things that take a long time, 7 - not very important messages
;; on stuff, 9 - messages inside loops.
(defun gnus-message (level &rest args)
  (if (<= level gnus-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

(defun gnus-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gnus-verbose'."
  (when (<= (floor level) gnus-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

;; Generate a unique new group name.
(defun gnus-generate-new-group-name (leaf)
  (let ((name leaf)
	(num 0))
    (while (gnus-gethash name gnus-newsrc-hashtb)
      (setq name (concat leaf "<" (int-to-string (setq num (1+ num))) ">")))
    name))

(defsubst gnus-hide-text (b e props)
  "Set text PROPS on the B to E region, extending `intangible' 1 past B."
  (gnus-add-text-properties b e props)
  (when (memq 'intangible props)
    (gnus-put-text-property (max (1- b) (point-min))
		       b 'intangible (cddr (memq 'intangible props)))))

(defsubst gnus-unhide-text (b e)
  "Remove hidden text properties from region between B and E."
  (remove-text-properties b e gnus-hidden-properties)
  (when (memq 'intangible gnus-hidden-properties)
    (gnus-put-text-property (max (1- b) (point-min))
			    b 'intangible nil)))

(defun gnus-hide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (gnus-hide-text b e (cons 'gnus-type (cons type gnus-hidden-properties))))

(defun gnus-parent-headers (headers &optional generation)
  "Return the headers of the GENERATIONeth parent of HEADERS."
  (unless generation 
    (setq generation 1))
  (let (references parent)
    (while (and headers (not (zerop generation)))
      (setq references (mail-header-references headers))
      (when (and references
		 (setq parent (gnus-parent-id references))
		 (setq headers (car (gnus-id-to-thread parent))))
	(decf generation)))
    headers))

(defun gnus-parent-id (references)
  "Return the last Message-ID in REFERENCES."
  (when (and references
	     (string-match "\\(<[^\n<>]+>\\)[ \t\n]*\\'" references))
    (substring references (match-beginning 1) (match-end 1))))

(defun gnus-split-references (references)
  "Return a list of Message-IDs in REFERENCES."
  (let ((beg 0)
	ids)
    (while (string-match "<[^>]+>" references beg)
      (push (substring references (match-beginning 0) (setq beg (match-end 0)))
	    ids))
    (nreverse ids)))

(defun gnus-buffer-live-p (buffer)
  "Say whether BUFFER is alive or not."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer))))

(defun gnus-ephemeral-group-p (group)
  "Say whether GROUP is ephemeral or not."
  (gnus-group-get-parameter group 'quit-config))

(defun gnus-group-quit-config (group)
  "Return the quit-config of GROUP."
  (gnus-group-get-parameter group 'quit-config))

(defun gnus-simplify-mode-line ()
  "Make mode lines a bit simpler."
  (setq mode-line-modified "-- ")
  (when (listp mode-line-format)
    (make-local-variable 'mode-line-format)
    (setq mode-line-format (copy-sequence mode-line-format))
    (when (equal (nth 3 mode-line-format) "   ")
      (setcar (nthcdr 3 mode-line-format) " "))))

;;; List and range functions

(defun gnus-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun gnus-copy-sequence (list)
  "Do a complete, total copy of a list."
  (if (and (consp list) (not (consp (cdr list))))
      (cons (car list) (cdr list))
    (mapcar (lambda (elem) (if (consp elem)
			       (if (consp (cdr elem))
				   (gnus-copy-sequence elem)
				 (cons (car elem) (cdr elem)))
			     elem))
	    list)))

(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1))

(defun gnus-sorted-complement (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <."
  (let (out)
    (if (or (null list1) (null list2))
	(or list1 list2)
      (while (and list1 list2)
	(cond ((= (car list1) (car list2))
	       (setq list1 (cdr list1)
		     list2 (cdr list2)))
	      ((< (car list1) (car list2))
	       (setq out (cons (car list1) out))
	       (setq list1 (cdr list1)))
	      (t
	       (setq out (cons (car list2) out))
	       (setq list2 (cdr list2)))))
      (nconc (nreverse out) (or list1 list2)))))

(defun gnus-intersection (list1 list2)
  (let ((result nil))
    (while list2
      (if (memq (car list2) list1)
	  (setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result))

(defun gnus-sorted-intersection (list1 list2)
  ;; LIST1 and LIST2 have to be sorted over <.
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (nreverse out)))

(defun gnus-set-sorted-intersection (list1 list2)
  ;; LIST1 and LIST2 have to be sorted over <.
  ;; This function modifies LIST1.
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setcdr prev (cdr list1))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (setcdr prev nil)
    (cdr top)))

(defun gnus-compress-sequence (numbers &optional always-list)
  "Convert list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges."
  (let* ((first (car numbers))
	 (last (car numbers))
	 result)
    (if (null numbers)
	nil
      (if (not (listp (cdr numbers)))
	  numbers
	(while numbers
	  (cond ((= last (car numbers)) nil) ;Omit duplicated number
		((= (1+ last) (car numbers)) ;Still in sequence
		 (setq last (car numbers)))
		(t			;End of one sequence
		 (setq result
		       (cons (if (= first last) first
			       (cons first last)) result))
		 (setq first (car numbers))
		 (setq last  (car numbers))))
	  (setq numbers (cdr numbers)))
	(if (and (not always-list) (null result))
	    (if (= first last) (list first) (cons first last))
	  (nreverse (cons (if (= first last) first (cons first last))
			  result)))))))

(defalias 'gnus-uncompress-sequence 'gnus-uncompress-range)
(defun gnus-uncompress-range (ranges)
  "Expand a list of ranges into a list of numbers.
RANGES is either a single range on the form `(num . num)' or a list of
these ranges."
  (let (first last result)
    (cond
     ((null ranges)
      nil)
     ((not (listp (cdr ranges)))
      (setq first (car ranges))
      (setq last (cdr ranges))
      (while (<= first last)
	(setq result (cons first result))
	(setq first (1+ first)))
      (nreverse result))
     (t
      (while ranges
	(if (atom (car ranges))
	    (if (numberp (car ranges))
		(setq result (cons (car ranges) result)))
	  (setq first (caar ranges))
	  (setq last  (cdar ranges))
	  (while (<= first last)
	    (setq result (cons first result))
	    (setq first (1+ first))))
	(setq ranges (cdr ranges)))
      (nreverse result)))))

(defun gnus-add-to-range (ranges list)
  "Return a list of ranges that has all articles from both RANGES and LIST.
Note: LIST has to be sorted over `<'."
  (if (not ranges)
      (gnus-compress-sequence list t)
    (setq list (copy-sequence list))
    (or (listp (cdr ranges))
	(setq ranges (list ranges)))
    (let ((out ranges)
	  ilist lowest highest temp)
      (while (and ranges list)
	(setq ilist list)
	(setq lowest (or (and (atom (car ranges)) (car ranges))
			 (caar ranges)))
	(while (and list (cdr list) (< (cadr list) lowest))
	  (setq list (cdr list)))
	(if (< (car ilist) lowest)
	    (progn
	      (setq temp list)
	      (setq list (cdr list))
	      (setcdr temp nil)
	      (setq out (nconc (gnus-compress-sequence ilist t) out))))
	(setq highest (or (and (atom (car ranges)) (car ranges))
			  (cdar ranges)))
	(while (and list (<= (car list) highest))
	  (setq list (cdr list)))
	(setq ranges (cdr ranges)))
      (if list
	  (setq out (nconc (gnus-compress-sequence list t) out)))
      (setq out (sort out (lambda (r1 r2)
			    (< (or (and (atom r1) r1) (car r1))
			       (or (and (atom r2) r2) (car r2))))))
      (setq ranges out)
      (while ranges
	(if (atom (car ranges))
	    (if (cdr ranges)
		(if (atom (cadr ranges))
		    (if (= (1+ (car ranges)) (cadr ranges))
			(progn
			  (setcar ranges (cons (car ranges)
					       (cadr ranges)))
			  (setcdr ranges (cddr ranges))))
		  (if (= (1+ (car ranges)) (caadr ranges))
		      (progn
			(setcar (cadr ranges) (car ranges))
			(setcar ranges (cadr ranges))
			(setcdr ranges (cddr ranges))))))
	  (if (cdr ranges)
	      (if (atom (cadr ranges))
		  (if (= (1+ (cdar ranges)) (cadr ranges))
		      (progn
			(setcdr (car ranges) (cadr ranges))
			(setcdr ranges (cddr ranges))))
		(if (= (1+ (cdar ranges)) (caadr ranges))
		    (progn
		      (setcdr (car ranges) (cdadr ranges))
		      (setcdr ranges (cddr ranges)))))))
	(setq ranges (cdr ranges)))
      out)))

(defun gnus-remove-from-range (ranges list)
  "Return a list of ranges that has all articles from LIST removed from RANGES.
Note: LIST has to be sorted over `<'."
  ;; !!! This function shouldn't look like this, but I've got a headache.
  (gnus-compress-sequence
   (gnus-sorted-complement
    (gnus-uncompress-range ranges) list)))

(defun gnus-member-of-range (number ranges)
  (if (not (listp (cdr ranges)))
      (and (>= number (car ranges))
	   (<= number (cdr ranges)))
    (let ((not-stop t))
      (while (and ranges
		  (if (numberp (car ranges))
		      (>= number (car ranges))
		    (>= number (caar ranges)))
		  not-stop)
	(if (if (numberp (car ranges))
		(= number (car ranges))
	      (and (>= number (caar ranges))
		   (<= number (cdar ranges))))
	    (setq not-stop nil))
	(setq ranges (cdr ranges)))
      (not not-stop))))

(defun gnus-range-length (range)
  "Return the length RANGE would have if uncompressed."
  (length (gnus-uncompress-range range)))

(defun gnus-sublist-p (list sublist)
  "Test whether all elements in SUBLIST are members of LIST."
  (let ((sublistp t))
    (while sublist
      (unless (memq (pop sublist) list)
	(setq sublistp nil
	      sublist nil)))
    sublistp))


;;;
;;; Gnus group mode
;;;

(defvar gnus-group-mode-map nil)
(put 'gnus-group-mode 'mode-class 'special)

(unless gnus-group-mode-map
  (setq gnus-group-mode-map (make-keymap))
  (suppress-keymap gnus-group-mode-map)

  (gnus-define-keys gnus-group-mode-map
    " " gnus-group-read-group
    "=" gnus-group-select-group
    "\r" gnus-group-select-group
    "\M-\r" gnus-group-quick-select-group
    "j" gnus-group-jump-to-group
    "n" gnus-group-next-unread-group
    "p" gnus-group-prev-unread-group
    "\177" gnus-group-prev-unread-group
    [delete] gnus-group-prev-unread-group
    "N" gnus-group-next-group
    "P" gnus-group-prev-group
    "\M-n" gnus-group-next-unread-group-same-level
    "\M-p" gnus-group-prev-unread-group-same-level
    "," gnus-group-best-unread-group
    "." gnus-group-first-unread-group
    "u" gnus-group-unsubscribe-current-group
    "U" gnus-group-unsubscribe-group
    "c" gnus-group-catchup-current
    "C" gnus-group-catchup-current-all
    "l" gnus-group-list-groups
    "L" gnus-group-list-all-groups
    "m" gnus-group-mail
    "g" gnus-group-get-new-news
    "\M-g" gnus-group-get-new-news-this-group
    "R" gnus-group-restart
    "r" gnus-group-read-init-file
    "B" gnus-group-browse-foreign-server
    "b" gnus-group-check-bogus-groups
    "F" gnus-find-new-newsgroups
    "\C-c\C-d" gnus-group-describe-group
    "\M-d" gnus-group-describe-all-groups
    "\C-c\C-a" gnus-group-apropos
    "\C-c\M-\C-a" gnus-group-description-apropos
    "a" gnus-group-post-news
    "\ek" gnus-group-edit-local-kill
    "\eK" gnus-group-edit-global-kill
    "\C-k" gnus-group-kill-group
    "\C-y" gnus-group-yank-group
    "\C-w" gnus-group-kill-region
    "\C-x\C-t" gnus-group-transpose-groups
    "\C-c\C-l" gnus-group-list-killed
    "\C-c\C-x" gnus-group-expire-articles
    "\C-c\M-\C-x" gnus-group-expire-all-groups
    "V" gnus-version
    "s" gnus-group-save-newsrc
    "z" gnus-group-suspend
;    "Z" gnus-group-clear-dribble
    "q" gnus-group-exit
    "Q" gnus-group-quit
    "?" gnus-group-describe-briefly
    "\C-c\C-i" gnus-info-find-node
    "\M-e" gnus-group-edit-group-method
    "^" gnus-group-enter-server-mode
    gnus-mouse-2 gnus-mouse-pick-group
    "<" beginning-of-buffer
    ">" end-of-buffer
    "\C-c\C-b" gnus-bug
    "\C-c\C-s" gnus-group-sort-groups
    "t" gnus-topic-mode
    "\C-c\M-g" gnus-activate-all-groups
    "\M-&" gnus-group-universal-argument
    "#" gnus-group-mark-group
    "\M-#" gnus-group-unmark-group)

  (gnus-define-keys (gnus-group-mark-map "M" gnus-group-mode-map)
    "m" gnus-group-mark-group
    "u" gnus-group-unmark-group
    "w" gnus-group-mark-region
    "m" gnus-group-mark-buffer
    "r" gnus-group-mark-regexp
    "U" gnus-group-unmark-all-groups)

  (gnus-define-keys (gnus-group-group-map "G" gnus-group-mode-map)
    "d" gnus-group-make-directory-group
    "h" gnus-group-make-help-group
    "a" gnus-group-make-archive-group
    "k" gnus-group-make-kiboze-group
    "m" gnus-group-make-group
    "E" gnus-group-edit-group
    "e" gnus-group-edit-group-method
    "p" gnus-group-edit-group-parameters
    "v" gnus-group-add-to-virtual
    "V" gnus-group-make-empty-virtual
    "D" gnus-group-enter-directory
    "f" gnus-group-make-doc-group
    "r" gnus-group-rename-group
    "\177" gnus-group-delete-group
    [delete] gnus-group-delete-group)

   (gnus-define-keys (gnus-group-soup-map "s" gnus-group-group-map)
     "b" gnus-group-brew-soup
     "w" gnus-soup-save-areas
     "s" gnus-soup-send-replies
     "p" gnus-soup-pack-packet
     "r" nnsoup-pack-replies)

   (gnus-define-keys (gnus-group-sort-map "S" gnus-group-group-map)
     "s" gnus-group-sort-groups
     "a" gnus-group-sort-groups-by-alphabet
     "u" gnus-group-sort-groups-by-unread
     "l" gnus-group-sort-groups-by-level
     "v" gnus-group-sort-groups-by-score
     "r" gnus-group-sort-groups-by-rank
     "m" gnus-group-sort-groups-by-method)

   (gnus-define-keys (gnus-group-list-map "A" gnus-group-mode-map)
     "k" gnus-group-list-killed
     "z" gnus-group-list-zombies
     "s" gnus-group-list-groups
     "u" gnus-group-list-all-groups
     "A" gnus-group-list-active
     "a" gnus-group-apropos
     "d" gnus-group-description-apropos
     "m" gnus-group-list-matching
     "M" gnus-group-list-all-matching
     "l" gnus-group-list-level)

   (gnus-define-keys (gnus-group-score-map "W" gnus-group-mode-map)
     "f" gnus-score-flush-cache)

   (gnus-define-keys (gnus-group-help-map "H" gnus-group-mode-map)
     "f" gnus-group-fetch-faq)

   (gnus-define-keys (gnus-group-sub-map "S" gnus-group-mode-map)
     "l" gnus-group-set-current-level
     "t" gnus-group-unsubscribe-current-group
     "s" gnus-group-unsubscribe-group
     "k" gnus-group-kill-group
     "y" gnus-group-yank-group
     "w" gnus-group-kill-region
     "\C-k" gnus-group-kill-level
     "z" gnus-group-kill-all-zombies))

(defun gnus-group-mode ()
  "Major mode for reading news.

All normal editing commands are switched off.
\\<gnus-group-mode-map>
The group buffer lists (some of) the groups available.	For instance,
`\\[gnus-group-list-groups]' will list all subscribed groups with unread articles, while `\\[gnus-group-list-zombies]'
lists all zombie groups.

Groups that are displayed can be entered with `\\[gnus-group-read-group]'.  To subscribe
to a group not displayed, type `\\[gnus-group-unsubscribe-group]'.

For more in-depth information on this mode, read the manual (`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-group-mode-map}"
  (interactive)
  (when (and menu-bar-mode
	     (gnus-visual-p 'group-menu 'menu))
    (gnus-group-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-group-mode)
  (setq mode-name "Group")
  (gnus-group-set-mode-line)
  (setq mode-line-process nil)
  (use-local-map gnus-group-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (gnus-make-local-hook 'post-command-hook)
  (gnus-add-hook 'post-command-hook 'gnus-clear-inboxes-moved nil t)
  (run-hooks 'gnus-group-mode-hook))

(defun gnus-clear-inboxes-moved ()
  (setq nnmail-moved-inboxes nil))

(defun gnus-mouse-pick-group (e)
  "Enter the group under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (gnus-group-read-group nil))

;; Look at LEVEL and find out what the level is really supposed to be.
;; If LEVEL is non-nil, LEVEL will be returned, if not, what happens
;; will depend on whether `gnus-group-use-permanent-levels' is used.
(defun gnus-group-default-level (&optional level number-or-nil)
  (cond
   (gnus-group-use-permanent-levels
    (or (setq gnus-group-use-permanent-levels
	      (or level (if (numberp gnus-group-use-permanent-levels)
			    gnus-group-use-permanent-levels
			  (or gnus-group-default-list-level
			      gnus-level-subscribed))))
	gnus-group-default-list-level gnus-level-subscribed))
   (number-or-nil
    level)
   (t
    (or level gnus-group-default-list-level gnus-level-subscribed))))

;;;###autoload
(defun gnus-slave-no-server (&optional arg)
  "Read network news as a slave, without connecting to local server"
  (interactive "P")
  (gnus-no-server arg t))

;;;###autoload
(defun gnus-no-server (&optional arg slave)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.	If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (let ((val (or arg (1- gnus-level-default-subscribed))))
    (gnus val t slave)
    (make-local-variable 'gnus-group-use-permanent-levels)
    (setq gnus-group-use-permanent-levels val)))

;;;###autoload
(defun gnus-slave (&optional arg)
  "Read news as a slave."
  (interactive "P")
  (gnus arg nil 'slave))

;;;###autoload
(defun gnus-other-frame (&optional arg)
  "Pop up a frame to read news."
  (interactive "P")
  (if (get-buffer gnus-group-buffer)
      (let ((pop-up-frames t))
	(gnus arg))
    (select-frame (make-frame))
    (gnus arg)))

;;;###autoload
(defun gnus (&optional arg dont-connect slave)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.	If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")

  (if (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-get-new-news))

    (gnus-clear-system)
    (nnheader-init-server-buffer)
    (gnus-read-init-file)
    (setq gnus-slave slave)

    (gnus-group-setup-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (if (not gnus-inhibit-startup-message)
	  (progn
	    (gnus-group-startup-message)
	    (sit-for 0))))

    (let ((level (and (numberp arg) (> arg 0) arg))
	  did-connect)
      (unwind-protect
	  (progn
	    (or dont-connect
		(setq did-connect
		      (gnus-start-news-server (and arg (not level))))))
	(if (and (not dont-connect)
		 (not did-connect))
	    (gnus-group-quit)
	  (run-hooks 'gnus-startup-hook)
	  ;; NNTP server is successfully open.

	  ;; Find the current startup file name.
	  (setq gnus-current-startup-file
		(gnus-make-newsrc-file gnus-startup-file))

	  ;; Read the dribble file.
	  (when (or gnus-slave gnus-use-dribble-file)
	    (gnus-dribble-read-file))

	  ;; Allow using GroupLens predictions.
	  (when gnus-use-grouplens
	    (bbb-login)
	    (add-hook 'gnus-summary-mode-hook 'gnus-grouplens-mode))

	  (gnus-summary-make-display-table)
	  ;; Do the actual startup.
	  (gnus-setup-news nil level dont-connect)
	  ;; Generate the group buffer.
	  (gnus-group-list-groups level)
	  (gnus-group-first-unread-group)
	  (gnus-configure-windows 'group)
	  (gnus-group-set-mode-line))))))

(defun gnus-unload ()
  "Unload all Gnus features."
  (interactive)
  (or (boundp 'load-history)
      (error "Sorry, `gnus-unload' is not implemented in this Emacs version."))
  (let ((history load-history)
	feature)
    (while history
      (and (string-match "^\\(gnus\\|nn\\)" (caar history))
	   (setq feature (cdr (assq 'provide (car history))))
	   (unload-feature feature 'force))
      (setq history (cdr history)))))

(defun gnus-compile ()
  "Byte-compile the user-defined format specs."
  (interactive)
  (let ((entries gnus-format-specs)
	entry gnus-tmp-func)
    (save-excursion
      (gnus-message 7 "Compiling format specs...")

      (while entries
	(setq entry (pop entries))
	(if (eq (car entry) 'version)
	    (setq gnus-format-specs (delq entry gnus-format-specs))
	  (when (and (listp (caddr entry))
		     (not (eq 'byte-code (caaddr entry))))
	    (fset 'gnus-tmp-func
		  `(lambda () ,(caddr entry)))
	    (byte-compile 'gnus-tmp-func)
	    (setcar (cddr entry) (gnus-byte-code 'gnus-tmp-func)))))

      (push (cons 'version emacs-version) gnus-format-specs)

      (gnus-message 7 "Compiling user specs...done"))))

(defun gnus-indent-rigidly (start end arg)
  "Indent rigidly using only spaces and no tabs."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (indent-rigidly start end arg)
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
	(replace-match "	" t t)))))

(defun gnus-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (insert
   (format "              %s
          _    ___ _             _
          _ ___ __ ___  __    _ ___
          __   _     ___    __  ___
              _           ___     _
             _  _ __             _
             ___   __            _
                   __           _
                    _      _   _
                   _      _    _
                      _  _    _
                  __  ___
                 _   _ _     _
                _   _
              _    _
             _    _
            _
          __

"
           ""))
  ;; And then hack it.
  (gnus-indent-rigidly (point-min) (point-max)
		       (/ (max (- (window-width) (or x 46)) 0) 2))
  (goto-char (point-min))
  (forward-line 1)
  (let* ((pheight (count-lines (point-min) (point-max)))
	 (wheight (window-height))
	 (rest (- wheight pheight)))
    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n)))
  ;; Fontify some.
  (goto-char (point-min))
  (and (search-forward "Praxis" nil t)
       (gnus-put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
  (goto-char (point-min))
  (let* ((mode-string (gnus-group-set-mode-line)))
    (setq mode-line-buffer-identification
	  (list (concat gnus-version (substring (car mode-string) 4))))
    (set-buffer-modified-p t)))

(defun gnus-group-setup-buffer ()
  (or (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-add-current-to-buffer-list)
	(gnus-group-mode)
	(and gnus-carpal (gnus-carpal-setup-buffer 'group)))))

(defun gnus-group-list-groups (&optional level unread lowest)
  "List newsgroups with level LEVEL or lower that have unread articles.
Default is all subscribed groups.
If argument UNREAD is non-nil, groups with no unread articles are also
listed."
  (interactive (list (if current-prefix-arg
			 (prefix-numeric-value current-prefix-arg)
		       (or
			(gnus-group-default-level nil t)
			gnus-group-default-list-level
			gnus-level-subscribed))))
  (or level
      (setq level (car gnus-group-list-mode)
	    unread (cdr gnus-group-list-mode)))
  (setq level (gnus-group-default-level level))
  (gnus-group-setup-buffer)		;May call from out of group buffer
  (gnus-update-format-specifications)
  (let ((case-fold-search nil)
	(props (text-properties-at (gnus-point-at-bol)))
	(group (gnus-group-group-name)))
    (set-buffer gnus-group-buffer)
    (funcall gnus-group-prepare-function level unread lowest)
    (if (zerop (buffer-size))
	(gnus-message 5 gnus-no-groups-message)
      (goto-char (point-max))
      (when (or (not gnus-group-goto-next-group-function)
		(not (funcall gnus-group-goto-next-group-function 
			      group props)))
	(if (not group)
	    ;; Go to the first group with unread articles.
	    (gnus-group-search-forward t)
	  ;; Find the right group to put point on.  If the current group
	  ;; has disappeared in the new listing, try to find the next
	  ;; one.	 If no next one can be found, just leave point at the
	  ;; first newsgroup in the buffer.
	  (if (not (gnus-goto-char
		    (text-property-any
		     (point-min) (point-max)
		     'gnus-group (gnus-intern-safe group gnus-active-hashtb))))
	      (let ((newsrc (cdddr (gnus-gethash group gnus-newsrc-hashtb))))
		(while (and newsrc
			    (not (gnus-goto-char
				  (text-property-any
				   (point-min) (point-max) 'gnus-group
				   (gnus-intern-safe
				    (caar newsrc) gnus-active-hashtb)))))
		  (setq newsrc (cdr newsrc)))
		(or newsrc (progn (goto-char (point-max))
				  (forward-line -1)))))))
      ;; Adjust cursor point.
      (gnus-group-position-point))))

(defun gnus-group-list-level (level &optional all)
  "List groups on LEVEL.
If ALL (the prefix), also list groups that have no unread articles."
  (interactive "nList groups on level: \nP")
  (gnus-group-list-groups level all level))

(defun gnus-group-prepare-flat (level &optional all lowest regexp)
  "List all newsgroups with unread articles of level LEVEL or lower.
If ALL is non-nil, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher.
If REGEXP, only list groups matching REGEXP."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(newsrc (cdr gnus-newsrc-alist))
	(lowest (or lowest 1))
	info clevel unread group params)
    (erase-buffer)
    (if (< lowest gnus-level-zombie)
	;; List living groups.
	(while newsrc
	  (setq info (car newsrc)
		group (gnus-info-group info)
		params (gnus-info-params info)
		newsrc (cdr newsrc)
		unread (car (gnus-gethash group gnus-newsrc-hashtb)))
	  (and unread			; This group might be bogus
	       (or (not regexp)
		   (string-match regexp group))
	       (<= (setq clevel (gnus-info-level info)) level)
	       (>= clevel lowest)
	       (or all			; We list all groups?
		   (if (eq unread t)	; Unactivated?
		       gnus-group-list-inactive-groups ; We list unactivated 
		     (> unread 0))	; We list groups with unread articles
		   (and gnus-list-groups-with-ticked-articles
			(cdr (assq 'tick (gnus-info-marks info))))
					; And groups with tickeds
		   ;; Check for permanent visibility.
		   (and gnus-permanently-visible-groups
			(string-match gnus-permanently-visible-groups
				      group))
		   (memq 'visible params)
		   (cdr (assq 'visible params)))
	       (gnus-group-insert-group-line
		group (gnus-info-level info)
		(gnus-info-marks info) unread (gnus-info-method info)))))

    ;; List dead groups.
    (and (>= level gnus-level-zombie) (<= lowest gnus-level-zombie)
	 (gnus-group-prepare-flat-list-dead
	  (setq gnus-zombie-list (sort gnus-zombie-list 'string<))
	  gnus-level-zombie ?Z
	  regexp))
    (and (>= level gnus-level-killed) (<= lowest gnus-level-killed)
	 (gnus-group-prepare-flat-list-dead
	  (setq gnus-killed-list (sort gnus-killed-list 'string<))
	  gnus-level-killed ?K regexp))

    (gnus-group-set-mode-line)
    (setq gnus-group-list-mode (cons level all))
    (run-hooks 'gnus-group-prepare-hook)))

(defun gnus-group-prepare-flat-list-dead (groups level mark regexp)
  ;; List zombies and killed lists somewhat faster, which was
  ;; suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.  It does
  ;; this by ignoring the group format specification altogether.
  (let (group)
    (if regexp
	;; This loop is used when listing groups that match some
	;; regexp.
	(while groups
	  (setq group (pop groups))
	  (when (string-match regexp group)
	    (gnus-add-text-properties
	     (point) (prog1 (1+ (point))
		       (insert " " mark "     *: " group "\n"))
	     (list 'gnus-group (gnus-intern-safe group gnus-active-hashtb)
		   'gnus-unread t
		   'gnus-level level))))
      ;; This loop is used when listing all groups.
      (while groups
	(gnus-add-text-properties
	 (point) (prog1 (1+ (point))
		   (insert " " mark "     *: "
			   (setq group (pop groups)) "\n"))
	 (list 'gnus-group (gnus-intern-safe group gnus-active-hashtb)
	       'gnus-unread t
	       'gnus-level level))))))

(defmacro gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  `(let ((gname ,group))
     (if (string-match ":[^:]+$" gname)
	 (substring gname (1+ (match-beginning 0)))
       gname)))

(defsubst gnus-server-add-address (method)
  (let ((method-name (symbol-name (car method))))
    (if (and (memq 'address (assoc method-name gnus-valid-select-methods))
	     (not (assq (intern (concat method-name "-address")) method)))
	(append method (list (list (intern (concat method-name "-address"))
				   (nth 1 method))))
      method)))

(defsubst gnus-server-get-method (group method)
  ;; Input either a server name, and extended server name, or a
  ;; select method, and return a select method.
  (cond ((stringp method)
	 (gnus-server-to-method method))
	((equal method gnus-select-method)
	 gnus-select-method)
	((and (stringp (car method)) group)
	 (gnus-server-extend-method group method))
	((and method (not group)
	      (equal (cadr method) ""))
	 method)
	(t
	 (gnus-server-add-address method))))

(defun gnus-server-to-method (server)
  "Map virtual server names to select methods."
  (or 
   ;; Is this a method, perhaps?
   (and server (listp server) server)
   ;; Perhaps this is the native server?
   (and (equal server "native") gnus-select-method)
   ;; It should be in the server alist.
   (cdr (assoc server gnus-server-alist))
   ;; If not, we look through all the opened server
   ;; to see whether we can find it there.
   (let ((opened gnus-opened-servers))
     (while (and opened
		 (not (equal server (format "%s:%s" (caaar opened)
					    (cadaar opened)))))
       (pop opened))
     (caar opened))))

(defmacro gnus-method-equal (ss1 ss2)
  "Say whether two servers are equal."
  `(let ((s1 ,ss1)
	 (s2 ,ss2))
     (or (equal s1 s2)
	 (and (= (length s1) (length s2))
	      (progn
		(while (and s1 (member (car s1) s2))
		  (setq s1 (cdr s1)))
		(null s1))))))

(defun gnus-server-equal (m1 m2)
  "Say whether two methods are equal."
  (let ((m1 (cond ((null m1) gnus-select-method)
		  ((stringp m1) (gnus-server-to-method m1))
		  (t m1)))
	(m2 (cond ((null m2) gnus-select-method)
		  ((stringp m2) (gnus-server-to-method m2))
		  (t m2))))
    (gnus-method-equal m1 m2)))

(defun gnus-servers-using-backend (backend)
  "Return a list of known servers using BACKEND."
  (let ((opened gnus-opened-servers)
	out)
    (while opened
      (when (eq backend (caaar opened))
	(push (caar opened) out))
      (pop opened))
    out))

(defun gnus-archive-server-wanted-p ()
  "Say whether the user wants to use the archive server."
  (cond 
   ((or (not gnus-message-archive-method)
	(not gnus-message-archive-group))
    nil)
   ((and gnus-message-archive-method gnus-message-archive-group)
    t)
   (t
    (let ((active (cadr (assq 'nnfolder-active-file
			      gnus-message-archive-method))))
      (and active
	   (file-exists-p active))))))

(defun gnus-group-prefixed-name (group method)
  "Return the whole name from GROUP and METHOD."
  (and (stringp method) (setq method (gnus-server-to-method method)))
  (concat (format "%s" (car method))
	  (if (and
	       (or (assoc (format "%s" (car method)) 
			  (gnus-methods-using 'address))
		   (gnus-server-equal method gnus-message-archive-method))
	       (nth 1 method)
	       (not (string= (nth 1 method) "")))
	      (concat "+" (nth 1 method)))
	  ":" group))

(defun gnus-group-real-prefix (group)
  "Return the prefix of the current group name."
  (if (string-match "^[^:]+:" group)
      (substring group 0 (match-end 0))
    ""))

(defun gnus-group-method (group)
  "Return the server or method used for selecting GROUP."
  (let ((prefix (gnus-group-real-prefix group)))
    (if (equal prefix "")
	gnus-select-method
      (let ((servers gnus-opened-servers)
	    (server "")
	    backend possible found)
	(if (string-match "^[^\\+]+\\+" prefix)
	    (setq backend (intern (substring prefix 0 (1- (match-end 0))))
		  server (substring prefix (match-end 0) (1- (length prefix))))
	  (setq backend (intern (substring prefix 0 (1- (length prefix))))))
	(while servers
	  (when (eq (caaar servers) backend)
	    (setq possible (caar servers))
	    (when (equal (cadaar servers) server)
	      (setq found (caar servers))))
	  (pop servers))
	(or (car (rassoc found gnus-server-alist))
	    found
	    (car (rassoc possible gnus-server-alist))
	    possible
	    (list backend server))))))

(defsubst gnus-secondary-method-p (method)
  "Return whether METHOD is a secondary select method."
  (let ((methods gnus-secondary-select-methods)
	(gmethod (gnus-server-get-method nil method)))
    (while (and methods
		(not (equal (gnus-server-get-method nil (car methods))
			    gmethod)))
      (setq methods (cdr methods)))
    methods))

(defun gnus-group-foreign-p (group)
  "Say whether a group is foreign or not."
  (and (not (gnus-group-native-p group))
       (not (gnus-group-secondary-p group))))

(defun gnus-group-native-p (group)
  "Say whether the group is native or not."
  (not (string-match ":" group)))

(defun gnus-group-secondary-p (group)
  "Say whether the group is secondary or not."
  (gnus-secondary-method-p (gnus-find-method-for-group group)))

(defun gnus-group-get-parameter (group &optional symbol)
  "Returns the group parameters for GROUP.
If SYMBOL, return the value of that symbol in the group parameters."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (if symbol
	(gnus-group-parameter-value params symbol)
      params)))

(defun gnus-group-parameter-value (params symbol)
  "Return the value of SYMBOL in group PARAMS."
  (or (car (memq symbol params))	; It's either a simple symbol
      (cdr (assq symbol params))))	; or a cons.

(defun gnus-group-add-parameter (group param)
  "Add parameter PARAM to GROUP."
  (let ((info (gnus-get-info group)))
    (if (not info)
	() ; This is a dead group.  We just ignore it.
      ;; Cons the new param to the old one and update.
      (gnus-group-set-info (cons param (gnus-info-params info))
			   group 'params))))

(defun gnus-group-set-parameter (group name value)
  "Set parameter NAME to VALUE in GROUP."
  (let ((info (gnus-get-info group)))
    (if (not info)
	() ; This is a dead group.  We just ignore it.
      (let ((old-params (gnus-info-params info))
	    (new-params (list (cons name value))))
	(while old-params
	  (if (or (not (listp (car old-params)))
		  (not (eq (caar old-params) name)))
	      (setq new-params (append new-params (list (car old-params)))))
	  (setq old-params (cdr old-params)))
	(gnus-group-set-info new-params group 'params)))))

(defun gnus-group-add-score (group &optional score)
  "Add SCORE to the GROUP score.
If SCORE is nil, add 1 to the score of GROUP."
  (let ((info (gnus-get-info group)))
    (when info
      (gnus-info-set-score info (+ (gnus-info-score info) (or score 1))))))

(defun gnus-summary-bubble-group ()
  "Increase the score of the current group.
This is a handy function to add to `gnus-summary-exit-hook' to
increase the score of each group you read."
  (gnus-group-add-score gnus-newsgroup-name))

(defun gnus-group-set-info (info &optional method-only-group part)
  (let* ((entry (gnus-gethash
		 (or method-only-group (gnus-info-group info))
		 gnus-newsrc-hashtb))
	 (part-info info)
	 (info (if method-only-group (nth 2 entry) info))
	 method)
    (when method-only-group
      (unless entry
	(error "Trying to change non-existent group %s" method-only-group))
      ;; We have received parts of the actual group info - either the
      ;; select method or the group parameters.	 We first check
      ;; whether we have to extend the info, and if so, do that.
      (let ((len (length info))
	    (total (if (eq part 'method) 5 6)))
	(when (< len total)
	  (setcdr (nthcdr (1- len) info)
		  (make-list (- total len) nil)))
	;; Then we enter the new info.
	(setcar (nthcdr (1- total) info) part-info)))
    (unless entry
      ;; This is a new group, so we just create it.
      (save-excursion
	(set-buffer gnus-group-buffer)
	(setq method (gnus-info-method info))
	(when (gnus-server-equal method "native")
	  (setq method nil))
	(save-excursion
	  (set-buffer gnus-group-buffer)
	  (if method
	      ;; It's a foreign group...
	      (gnus-group-make-group
	       (gnus-group-real-name (gnus-info-group info))
	       (if (stringp method) method
		 (prin1-to-string (car method)))
	       (and (consp method)
		    (nth 1 (gnus-info-method info))))
	    ;; It's a native group.
	    (gnus-group-make-group (gnus-info-group info))))
	(gnus-message 6 "Note: New group created")
	(setq entry
	      (gnus-gethash (gnus-group-prefixed-name
			     (gnus-group-real-name (gnus-info-group info))
			     (or (gnus-info-method info) gnus-select-method))
			    gnus-newsrc-hashtb))))
    ;; Whether it was a new group or not, we now have the entry, so we
    ;; can do the update.
    (if entry
	(progn
	  (setcar (nthcdr 2 entry) info)
	  (when (and (not (eq (car entry) t))
		     (gnus-active (gnus-info-group info)))
	    (setcar entry (length (gnus-list-of-unread-articles (car info))))))
      (error "No such group: %s" (gnus-info-group info)))))

(defun gnus-group-set-method-info (group select-method)
  (gnus-group-set-info select-method group 'method))

(defun gnus-group-set-params-info (group params)
  (gnus-group-set-info params group 'params))

(defun gnus-group-update-group-line ()
  "Update the current line in the group buffer."
  (let* ((buffer-read-only nil)
	 (group (gnus-group-group-name))
	 (entry (and group (gnus-gethash group gnus-newsrc-hashtb)))
	 gnus-group-indentation)
    (when group
      (and entry
	   (not (gnus-ephemeral-group-p group))
	   (gnus-dribble-enter
	    (concat "(gnus-group-set-info '"
		    (prin1-to-string (nth 2 entry)) ")")))
      (setq gnus-group-indentation (gnus-group-group-indentation))
      (gnus-delete-line)
      (gnus-group-insert-group-line-info group)
      (forward-line -1)
      (gnus-group-position-point))))

(defun gnus-group-insert-group-line-info (group)
  "Insert GROUP on the current line."
  (let ((entry (gnus-gethash group gnus-newsrc-hashtb))
	active info)
    (if entry
	(progn
	  ;; (Un)subscribed group.
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line
	   group (gnus-info-level info) (gnus-info-marks info)
	   (or (car entry) t) (gnus-info-method info)))
      ;; This group is dead.
      (gnus-group-insert-group-line
       group
       (if (member group gnus-zombie-list) gnus-level-zombie gnus-level-killed)
       nil
       (if (setq active (gnus-active group))
	   (- (1+ (cdr active)) (car active)) 0)
       nil))))

(defun gnus-group-insert-group-line (gnus-tmp-group gnus-tmp-level 
						    gnus-tmp-marked number
						    gnus-tmp-method)
  "Insert a group line in the group buffer."
  (let* ((gnus-tmp-active (gnus-active gnus-tmp-group))
	 (gnus-tmp-number-total
	  (if gnus-tmp-active
	      (1+ (- (cdr gnus-tmp-active) (car gnus-tmp-active)))
	    0))
	 (gnus-tmp-number-of-unread
	  (if (numberp number) (int-to-string (max 0 number))
	    "*"))
	 (gnus-tmp-number-of-read
	  (if (numberp number)
	      (int-to-string (max 0 (- gnus-tmp-number-total number)))
	    "*"))
	 (gnus-tmp-subscribed
	  (cond ((<= gnus-tmp-level gnus-level-subscribed) ? )
		((<= gnus-tmp-level gnus-level-unsubscribed) ?U)
		((= gnus-tmp-level gnus-level-zombie) ?Z)
		(t ?K)))
	 (gnus-tmp-qualified-group (gnus-group-real-name gnus-tmp-group))
	 (gnus-tmp-newsgroup-description
	  (if gnus-description-hashtb
	      (or (gnus-gethash gnus-tmp-group gnus-description-hashtb) "")
	    ""))
	 (gnus-tmp-moderated
	  (if (member gnus-tmp-group gnus-moderated-list) ?m ? ))
	 (gnus-tmp-moderated-string
	  (if (eq gnus-tmp-moderated ?m) "(m)" ""))
	 (gnus-tmp-method
	  (gnus-server-get-method gnus-tmp-group gnus-tmp-method))
	 (gnus-tmp-news-server (or (cadr gnus-tmp-method) ""))
	 (gnus-tmp-news-method (or (car gnus-tmp-method) ""))
	 (gnus-tmp-news-method-string
	  (if gnus-tmp-method
	      (format "(%s:%s)" (car gnus-tmp-method)
		      (cadr gnus-tmp-method)) ""))
	 (gnus-tmp-marked-mark
	  (if (and (numberp number)
		   (zerop number)
		   (cdr (assq 'tick gnus-tmp-marked)))
	      ?* ? ))
	 (gnus-tmp-process-marked
	  (if (member gnus-tmp-group gnus-group-marked)
	      gnus-process-mark ? ))
	 (gnus-tmp-grouplens
	  (or (and gnus-use-grouplens
		   (bbb-grouplens-group-p gnus-tmp-group))
	      ""))
	 (buffer-read-only nil)
	 header gnus-tmp-header)	; passed as parameter to user-funcs.
    (beginning-of-line)
    (gnus-add-text-properties
     (point)
     (prog1 (1+ (point))
       ;; Insert the text.
       (eval gnus-group-line-format-spec))
     `(gnus-group ,(gnus-intern-safe gnus-tmp-group gnus-active-hashtb)
       gnus-unread ,(if (numberp number)
			(string-to-int gnus-tmp-number-of-unread)
		      t)
       gnus-marked ,gnus-tmp-marked-mark
       gnus-indentation ,gnus-group-indentation
       gnus-level ,gnus-tmp-level))
    (when (inline (gnus-visual-p 'group-highlight 'highlight))
      (forward-line -1)
      (run-hooks 'gnus-group-update-hook)
      (forward-line))
    ;; Allow XEmacs to remove front-sticky text properties.
    (gnus-group-remove-excess-properties)))

(defun gnus-group-update-group (group &optional visible-only)
  "Update all lines where GROUP appear.
If VISIBLE-ONLY is non-nil, the group won't be displayed if it isn't
already."
  (save-excursion
    (set-buffer gnus-group-buffer)
    ;; The buffer may be narrowed.
    (save-restriction
      (widen)
      (let ((ident (gnus-intern-safe group gnus-active-hashtb))
	    (loc (point-min))
	    found buffer-read-only)
	;; Enter the current status into the dribble buffer.
	(let ((entry (gnus-gethash group gnus-newsrc-hashtb)))
	  (if (and entry (not (gnus-ephemeral-group-p group)))
	      (gnus-dribble-enter
	       (concat "(gnus-group-set-info '" (prin1-to-string (nth 2 entry))
		       ")"))))
	;; Find all group instances.  If topics are in use, each group
	;; may be listed in more than once.
	(while (setq loc (text-property-any
			  loc (point-max) 'gnus-group ident))
	  (setq found t)
	  (goto-char loc)
	  (let ((gnus-group-indentation (gnus-group-group-indentation)))
	    (gnus-delete-line)
	    (gnus-group-insert-group-line-info group)
	    (save-excursion
	      (forward-line -1)
	      (run-hooks 'gnus-group-update-group-hook)))
	  (setq loc (1+ loc)))
	(unless (or found visible-only)
	  ;; No such line in the buffer, find out where it's supposed to
	  ;; go, and insert it there (or at the end of the buffer).
	  (if gnus-goto-missing-group-function
	      (funcall gnus-goto-missing-group-function group)
	    (let ((entry (cddr (gnus-gethash group gnus-newsrc-hashtb))))
	      (while (and entry (car entry)
			  (not
			   (gnus-goto-char
			    (text-property-any
			     (point-min) (point-max)
			     'gnus-group (gnus-intern-safe
					  (caar entry) gnus-active-hashtb)))))
		(setq entry (cdr entry)))
	      (or entry (goto-char (point-max)))))
	  ;; Finally insert the line.
	  (let ((gnus-group-indentation (gnus-group-group-indentation)))
	    (gnus-group-insert-group-line-info group)
	    (save-excursion
	      (forward-line -1)
	      (run-hooks 'gnus-group-update-group-hook))))
	(gnus-group-set-mode-line)))))

(defun gnus-group-set-mode-line ()
  "Update the mode line in the group buffer."
  (when (memq 'group gnus-updated-mode-lines)
    ;; Yes, we want to keep this mode line updated.
    (save-excursion
      (set-buffer gnus-group-buffer)
      (let* ((gformat (or gnus-group-mode-line-format-spec
			  (setq gnus-group-mode-line-format-spec
				(gnus-parse-format
				 gnus-group-mode-line-format
				 gnus-group-mode-line-format-alist))))
	     (gnus-tmp-news-server (cadr gnus-select-method))
	     (gnus-tmp-news-method (car gnus-select-method))
	     (gnus-tmp-colon (if (equal gnus-tmp-news-server "") "" ":"))
	     (max-len 60)
	     gnus-tmp-header		;Dummy binding for user-defined formats
	     ;; Get the resulting string.
	     (modified 
	      (and gnus-dribble-buffer
		   (buffer-name gnus-dribble-buffer)
		   (buffer-modified-p gnus-dribble-buffer)
		   (save-excursion
		     (set-buffer gnus-dribble-buffer)
		     (not (zerop (buffer-size))))))
	     (mode-string (eval gformat)))
	;; Say whether the dribble buffer has been modified.
	(setq mode-line-modified
	      (if modified "---*- " "----- "))
	;; If the line is too long, we chop it off.
	(when (> (length mode-string) max-len)
	  (setq mode-string (substring mode-string 0 (- max-len 4))))
	(prog1
	    (setq mode-line-buffer-identification 
		  (gnus-mode-line-buffer-identification
		   (list mode-string)))
	  (set-buffer-modified-p modified))))))

(defun gnus-group-group-name ()
  "Get the name of the newsgroup on the current line."
  (let ((group (get-text-property (gnus-point-at-bol) 'gnus-group)))
    (and group (symbol-name group))))

(defun gnus-group-group-level ()
  "Get the level of the newsgroup on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-level))

(defun gnus-group-group-indentation ()
  "Get the indentation of the newsgroup on the current line."
  (or (get-text-property (gnus-point-at-bol) 'gnus-indentation)
      (and gnus-group-indentation-function
	   (funcall gnus-group-indentation-function))
      ""))

(defun gnus-group-group-unread ()
  "Get the number of unread articles of the newsgroup on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-unread))

(defun gnus-group-search-forward (&optional backward all level first-too)
  "Find the next newsgroup with unread articles.
If BACKWARD is non-nil, find the previous newsgroup instead.
If ALL is non-nil, just find any newsgroup.
If LEVEL is non-nil, find group with level LEVEL, or higher if no such
group exists.
If FIRST-TOO, the current line is also eligible as a target."
  (let ((way (if backward -1 1))
	(low gnus-level-killed)
	(beg (point))
	pos found lev)
    (if (and backward (progn (beginning-of-line)) (bobp))
	nil
      (or first-too (forward-line way))
      (while (and
	      (not (eobp))
	      (not (setq
		    found
		    (and (or all
			     (and
			      (let ((unread
				     (get-text-property (point) 'gnus-unread)))
				(and (numberp unread) (> unread 0)))
			      (setq lev (get-text-property (point)
							   'gnus-level))
			      (<= lev gnus-level-subscribed)))
			 (or (not level)
			     (and (setq lev (get-text-property (point)
							       'gnus-level))
				  (or (= lev level)
				      (and (< lev low)
					   (< level lev)
					   (progn
					     (setq low lev)
					     (setq pos (point))
					     nil))))))))
	      (zerop (forward-line way)))))
    (if found
	(progn (gnus-group-position-point) t)
      (goto-char (or pos beg))
      (and pos t))))

;;; Gnus group mode commands

;; Group marking.

(defun gnus-group-mark-group (n &optional unmark no-advance)
  "Mark the current group."
  (interactive "p")
  (let ((buffer-read-only nil)
	group)
    (while (and (> n 0)
		(not (eobp)))
      (when (setq group (gnus-group-group-name))
	;; Update the mark.
	(beginning-of-line)
	(forward-char
	 (or (cdr (assq 'process gnus-group-mark-positions)) 2))
	(delete-char 1)
	(if unmark
	    (progn
	      (insert " ")
	      (setq gnus-group-marked (delete group gnus-group-marked)))
	  (insert "#")
	  (setq gnus-group-marked
		(cons group (delete group gnus-group-marked)))))
      (or no-advance (gnus-group-next-group 1))
      (decf n))
    (gnus-summary-position-point)
    n))

(defun gnus-group-unmark-group (n)
  "Remove the mark from the current group."
  (interactive "p")
  (gnus-group-mark-group n 'unmark)
  (gnus-group-position-point))

(defun gnus-group-unmark-all-groups ()
  "Unmark all groups."
  (interactive)
  (let ((groups gnus-group-marked))
    (save-excursion
      (while groups
	(gnus-group-remove-mark (pop groups)))))
  (gnus-group-position-point))

(defun gnus-group-mark-region (unmark beg end)
  "Mark all groups between point and mark.
If UNMARK, remove the mark instead."
  (interactive "P\nr")
  (let ((num (count-lines beg end)))
    (save-excursion
      (goto-char beg)
      (- num (gnus-group-mark-group num unmark)))))

(defun gnus-group-mark-buffer (&optional unmark)
  "Mark all groups in the buffer.
If UNMARK, remove the mark instead."
  (interactive "P")
  (gnus-group-mark-region unmark (point-min) (point-max)))

(defun gnus-group-mark-regexp (regexp)
  "Mark all groups that match some regexp."
  (interactive "sMark (regexp): ")
  (let ((alist (cdr gnus-newsrc-alist))
	group)
    (while alist
      (when (string-match regexp (setq group (gnus-info-group (pop alist))))
	(gnus-group-set-mark group))))
  (gnus-group-position-point))

(defun gnus-group-remove-mark (group)
  "Remove the process mark from GROUP and move point there.
Return nil if the group isn't displayed."
  (if (gnus-group-goto-group group)
      (save-excursion
	(gnus-group-mark-group 1 'unmark t)
	t)
    (setq gnus-group-marked
	  (delete group gnus-group-marked))
    nil))

(defun gnus-group-set-mark (group)
  "Set the process mark on GROUP."
  (if (gnus-group-goto-group group) 
      (save-excursion
	(gnus-group-mark-group 1 nil t))
    (setq gnus-group-marked (cons group (delete group gnus-group-marked)))))

(defun gnus-group-universal-argument (arg &optional groups func)
  "Perform any command on all groups accoring to the process/prefix convention."
  (interactive "P")
  (let ((groups (or groups (gnus-group-process-prefix arg)))
	group func)
    (if (eq (setq func (or func
			   (key-binding
			    (read-key-sequence
			     (substitute-command-keys
			      "\\<gnus-group-mode-map>\\[gnus-group-universal-argument]")))))
	    'undefined)
	(gnus-error 1 "Undefined key")
      (while groups
	(gnus-group-remove-mark (setq group (pop groups)))
	(command-execute func))))
  (gnus-group-position-point))

(defun gnus-group-process-prefix (n)
  "Return a list of groups to work on.
Take into consideration N (the prefix) and the list of marked groups."
  (cond
   (n
    (setq n (prefix-numeric-value n))
    ;; There is a prefix, so we return a list of the N next
    ;; groups.
    (let ((way (if (< n 0) -1 1))
	  (n (abs n))
	  group groups)
      (save-excursion
	(while (and (> n 0)
		    (setq group (gnus-group-group-name)))
	  (setq groups (cons group groups))
	  (setq n (1- n))
	  (gnus-group-next-group way)))
      (nreverse groups)))
   ((and (boundp 'transient-mark-mode)
	 transient-mark-mode
	 (boundp 'mark-active)
	 mark-active)
    ;; Work on the region between point and mark.
    (let ((max (max (point) (mark)))
	  groups)
      (save-excursion
	(goto-char (min (point) (mark)))
	(while
	    (and
	     (push (gnus-group-group-name) groups)
	     (zerop (gnus-group-next-group 1))
	     (< (point) max)))
	(nreverse groups))))
   (gnus-group-marked
    ;; No prefix, but a list of marked articles.
    (reverse gnus-group-marked))
   (t
    ;; Neither marked articles or a prefix, so we return the
    ;; current group.
    (let ((group (gnus-group-group-name)))
      (and group (list group))))))

;; Selecting groups.

(defun gnus-group-read-group (&optional all no-article group)
  "Read news in this newsgroup.
If the prefix argument ALL is non-nil, already read articles become
readable.  IF ALL is a number, fetch this number of articles.  If the
optional argument NO-ARTICLE is non-nil, no article will be
auto-selected upon group entry.	 If GROUP is non-nil, fetch that
group."
  (interactive "P")
  (let ((group (or group (gnus-group-group-name)))
	number active marked entry)
    (or group (error "No group on current line"))
    (setq marked (nth 3 (nth 2 (setq entry (gnus-gethash
					    group gnus-newsrc-hashtb)))))
    ;; This group might be a dead group.  In that case we have to get
    ;; the number of unread articles from `gnus-active-hashtb'.
    (setq number
	  (cond ((numberp all) all)
		(entry (car entry))
		((setq active (gnus-active group))
		 (- (1+ (cdr active)) (car active)))))
    (gnus-summary-read-group
     group (or all (and (numberp number)
			(zerop (+ number (length (cdr (assq 'tick marked)))
				  (length (cdr (assq 'dormant marked)))))))
     no-article)))

(defun gnus-group-select-group (&optional all)
  "Select this newsgroup.
No article is selected automatically.
If ALL is non-nil, already read articles become readable.
If ALL is a number, fetch this number of articles."
  (interactive "P")
  (gnus-group-read-group all t))

(defun gnus-group-quick-select-group (&optional all)
  "Select the current group \"quickly\".
This means that no highlighting or scoring will be performed."
  (interactive "P")
  (let (gnus-visual
	gnus-score-find-score-files-function
	gnus-apply-kill-hook
	gnus-summary-expunge-below)
    (gnus-group-read-group all t)))

(defun gnus-group-visible-select-group (&optional all)
  "Select the current group without hiding any articles."
  (interactive "P")
  (let ((gnus-inhibit-limiting t))
    (gnus-group-read-group all t)))

;;;###autoload
(defun gnus-fetch-group (group)
  "Start Gnus if necessary and enter GROUP.
Returns whether the fetching was successful or not."
  (interactive "sGroup name: ")
  (or (get-buffer gnus-group-buffer)
      (gnus))
  (gnus-group-read-group nil nil group))

;; Enter a group that is not in the group buffer.  Non-nil is returned
;; if selection was successful.
(defun gnus-group-read-ephemeral-group
  (group method &optional activate quit-config)
  (let ((group (if (gnus-group-foreign-p group) group
		 (gnus-group-prefixed-name group method))))
    (gnus-sethash
     group
     `(t nil (,group ,gnus-level-default-subscribed nil nil ,method
		     ((quit-config . ,(if quit-config quit-config
					(cons (current-buffer) 'summary))))))
     gnus-newsrc-hashtb)
    (set-buffer gnus-group-buffer)
    (or (gnus-check-server method)
	(error "Unable to contact server: %s" (gnus-status-message method)))
    (if activate (or (gnus-request-group group)
		     (error "Couldn't request group")))
    (condition-case ()
	(gnus-group-read-group t t group)
      (error nil)
      (quit nil))))

(defun gnus-group-jump-to-group (group)
  "Jump to newsgroup GROUP."
  (interactive
   (list (completing-read
	  "Group: " gnus-active-hashtb nil
	  (gnus-read-active-file-p)
	  nil
	  'gnus-group-history)))

  (when (equal group "")
    (error "Empty group name"))

  (when (string-match "[\000-\032]" group)
    (error "Control characters in group: %s" group))

  (let ((b (text-property-any
	    (point-min) (point-max)
	    'gnus-group (gnus-intern-safe group gnus-active-hashtb))))
    (unless (gnus-ephemeral-group-p group)
      (if b
	  ;; Either go to the line in the group buffer...
	  (goto-char b)
	;; ... or insert the line.
	(or
	 (gnus-active group)
	 (gnus-activate-group group)
	 (error "%s error: %s" group (gnus-status-message group)))

	(gnus-group-update-group group)
	(goto-char (text-property-any
		    (point-min) (point-max)
		    'gnus-group (gnus-intern-safe group gnus-active-hashtb)))))
    ;; Adjust cursor point.
    (gnus-group-position-point)))

(defun gnus-group-goto-group (group)
  "Goto to newsgroup GROUP."
  (when group
    (let ((b (text-property-any (point-min) (point-max)
				'gnus-group (gnus-intern-safe
					     group gnus-active-hashtb))))
      (and b (goto-char b)))))

(defun gnus-group-next-group (n)
  "Go to next N'th newsgroup.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group n t))

(defun gnus-group-next-unread-group (n &optional all level)
  "Go to next N'th unread newsgroup.
If N is negative, search backward instead.
If ALL is non-nil, choose any newsgroup, unread or not.
If LEVEL is non-nil, choose the next group with level LEVEL, or, if no
such group can be found, the next group with a level higher than
LEVEL.
Returns the difference between N and the number of skips actually
made."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-group-search-forward
		 backward (or (not gnus-group-goto-unread) all) level))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more%s newsgroups%s" (if all "" " unread")
			       (if level " on this level or higher" "")))
    n))

(defun gnus-group-prev-group (n)
  "Go to previous N'th newsgroup.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t))

(defun gnus-group-prev-unread-group (n)
  "Go to previous N'th unread newsgroup.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n)))

(defun gnus-group-next-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group n t (gnus-group-group-level))
  (gnus-group-position-point))

(defun gnus-group-prev-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t (gnus-group-group-level))
  (gnus-group-position-point))

(defun gnus-group-best-unread-group (&optional exclude-group)
  "Go to the group with the highest level.
If EXCLUDE-GROUP, do not go to that group."
  (interactive)
  (goto-char (point-min))
  (let ((best 100000)
	unread best-point)
    (while (not (eobp))
      (setq unread (get-text-property (point) 'gnus-unread))
      (if (and (numberp unread) (> unread 0))
	  (progn
	    (if (and (get-text-property (point) 'gnus-level)
		     (< (get-text-property (point) 'gnus-level) best)
		     (or (not exclude-group)
			 (not (equal exclude-group (gnus-group-group-name)))))
		(progn
		  (setq best (get-text-property (point) 'gnus-level))
		  (setq best-point (point))))))
      (forward-line 1))
    (if best-point (goto-char best-point))
    (gnus-summary-position-point)
    (and best-point (gnus-group-group-name))))

(defun gnus-group-first-unread-group ()
  "Go to the first group with unread articles."
  (interactive)
  (prog1
      (let ((opoint (point))
	    unread)
	(goto-char (point-min))
	(if (or (eq (setq unread (gnus-group-group-unread)) t) ; Not active.
		(and (numberp unread)	; Not a topic.
		     (not (zerop unread))) ; Has unread articles.
		(zerop (gnus-group-next-unread-group 1))) ; Next unread group.
	    (point)			; Success.
	  (goto-char opoint)
	  nil))				; Not success.
    (gnus-group-position-point)))

(defun gnus-group-enter-server-mode ()
  "Jump to the server buffer."
  (interactive)
  (gnus-enter-server-buffer))

(defun gnus-group-make-group (name &optional method address)
  "Add a new newsgroup.
The user will be prompted for a NAME, for a select METHOD, and an
ADDRESS."
  (interactive
   (cons
    (read-string "Group name: ")
    (let ((method
	   (completing-read
	    "Method: " (append gnus-valid-select-methods gnus-server-alist)
	    nil t nil 'gnus-method-history)))
      (cond ((assoc method gnus-valid-select-methods)
	     (list method
		   (if (memq 'prompt-address
			     (assoc method gnus-valid-select-methods))
		       (read-string "Address: ")
		     "")))
	    ((assoc method gnus-server-alist)
	     (list method))
	    (t
	     (list method ""))))))

  (let* ((meth (and method (if address (list (intern method) address)
			     method)))
	 (nname (if method (gnus-group-prefixed-name name meth) name))
	 backend info)
    (when (gnus-gethash nname gnus-newsrc-hashtb)
      (error "Group %s already exists" nname))
    ;; Subscribe to the new group.
    (gnus-group-change-level
     (setq info (list t nname gnus-level-default-subscribed nil nil meth))
     gnus-level-default-subscribed gnus-level-killed
     (and (gnus-group-group-name)
	  (gnus-gethash (gnus-group-group-name)
			gnus-newsrc-hashtb))
     t)
    ;; Make it active.
    (gnus-set-active nname (cons 1 0))
    (or (gnus-ephemeral-group-p name)
	(gnus-dribble-enter
	 (concat "(gnus-group-set-info '" (prin1-to-string (cdr info)) ")")))
    ;; Insert the line.
    (gnus-group-insert-group-line-info nname)
    (forward-line -1)
    (gnus-group-position-point)

    ;; Load the backend and try to make the backend create
    ;; the group as well.
    (when (assoc (symbol-name (setq backend (car (gnus-server-get-method
						  nil meth))))
		 gnus-valid-select-methods)
      (require backend))
    (gnus-check-server meth)
    (and (gnus-check-backend-function 'request-create-group nname)
	 (gnus-request-create-group nname))
    t))

(defun gnus-group-delete-group (group &optional force)
  "Delete the current group.  Only meaningful with mail groups.
If FORCE (the prefix) is non-nil, all the articles in the group will
be deleted.  This is \"deleted\" as in \"removed forever from the face
of the Earth\".	 There is no undo.  The user will be prompted before
doing the deletion."
  (interactive
   (list (gnus-group-group-name)
	 current-prefix-arg))
  (or group (error "No group to rename"))
  (or (gnus-check-backend-function 'request-delete-group group)
      (error "This backend does not support group deletion"))
  (prog1
      (if (not (gnus-yes-or-no-p
		(format
		 "Do you really want to delete %s%s? "
		 group (if force " and all its contents" ""))))
	  () ; Whew!
	(gnus-message 6 "Deleting group %s..." group)
	(if (not (gnus-request-delete-group group force))
	    (gnus-error 3 "Couldn't delete group %s" group)
	  (gnus-message 6 "Deleting group %s...done" group)
	  (gnus-group-goto-group group)
	  (gnus-group-kill-group 1 t)
	  (gnus-sethash group nil gnus-active-hashtb)
	  t))
    (gnus-group-position-point)))

(defun gnus-group-rename-group (group new-name)
  (interactive
   (list
    (gnus-group-group-name)
    (progn
      (or (gnus-check-backend-function
	   'request-rename-group (gnus-group-group-name))
	  (error "This backend does not support renaming groups"))
      (read-string "New group name: "))))

  (or (gnus-check-backend-function 'request-rename-group group)
      (error "This backend does not support renaming groups"))

  (or group (error "No group to rename"))
  (and (string-match "^[ \t]*$" new-name)
       (error "Not a valid group name"))

  ;; We find the proper prefixed name.
  (setq new-name
	(gnus-group-prefixed-name
	 (gnus-group-real-name new-name)
	 (gnus-info-method (gnus-get-info group))))

  (gnus-message 6 "Renaming group %s to %s..." group new-name)
  (prog1
      (if (not (gnus-request-rename-group group new-name))
	  (gnus-error 3 "Couldn't rename group %s to %s" group new-name)
	;; We rename the group internally by killing it...
	(gnus-group-goto-group group)
	(gnus-group-kill-group)
	;; ... changing its name ...
	(setcar (cdar gnus-list-of-killed-groups) new-name)
	;; ... and then yanking it.  Magic!
	(gnus-group-yank-group)
	(gnus-set-active new-name (gnus-active group))
	(gnus-message 6 "Renaming group %s to %s...done" group new-name)
	new-name)
    (gnus-group-position-point)))

(defun gnus-group-edit-group (group &optional part)
  "Edit the group on the current line."
  (interactive (list (gnus-group-group-name)))
  (let* ((part (or part 'info))
	 (done-func `(lambda ()
		       "Exit editing mode and update the information."
		       (interactive)
		       (gnus-group-edit-group-done ',part ,group)))
	 (winconf (current-window-configuration))
	 info)
    (or group (error "No group on current line"))
    (or (setq info (gnus-get-info group))
	(error "Killed group; can't be edited"))
    (set-buffer (get-buffer-create gnus-group-edit-buffer))
    (gnus-configure-windows 'edit-group)
    (gnus-add-current-to-buffer-list)
    (emacs-lisp-mode)
    ;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (use-local-map (copy-keymap emacs-lisp-mode-map))
    (local-set-key "\C-c\C-c" done-func)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    (erase-buffer)
    (insert
     (cond
      ((eq part 'method)
       ";; Type `C-c C-c' after editing the select method.\n\n")
      ((eq part 'params)
       ";; Type `C-c C-c' after editing the group parameters.\n\n")
      ((eq part 'info)
       ";; Type `C-c C-c' after editing the group info.\n\n")))
    (insert
     (pp-to-string
      (cond ((eq part 'method)
	     (or (gnus-info-method info) "native"))
	    ((eq part 'params)
	     (gnus-info-params info))
	    (t info)))
     "\n")))

(defun gnus-group-edit-group-method (group)
  "Edit the select method of GROUP."
  (interactive (list (gnus-group-group-name)))
  (gnus-group-edit-group group 'method))

(defun gnus-group-edit-group-parameters (group)
  "Edit the group parameters of GROUP."
  (interactive (list (gnus-group-group-name)))
  (gnus-group-edit-group group 'params))

(defun gnus-group-edit-group-done (part group)
  "Get info from buffer, update variables and jump to the group buffer."
  (set-buffer (get-buffer-create gnus-group-edit-buffer))
  (goto-char (point-min))
  (let* ((form (read (current-buffer)))
	 (winconf gnus-prev-winconf)
	 (method (cond ((eq part 'info) (nth 4 form))
		       ((eq part 'method) form)
		       (t nil)))
	 (info (cond ((eq part 'info) form)
		     ((eq part 'method) (gnus-get-info group))
		     (t nil)))
	 (new-group (if info
		      (if (or (not method)
			      (gnus-server-equal
			       gnus-select-method method))
			  (gnus-group-real-name (car info))
			(gnus-group-prefixed-name
			 (gnus-group-real-name (car info)) method))
		      nil)))
    (when (and new-group
	       (not (equal new-group group)))
      (when (gnus-group-goto-group group)
	(gnus-group-kill-group 1))
      (gnus-activate-group new-group))
    ;; Set the info.
    (if (and info new-group)
	(progn
	  (setq info (gnus-copy-sequence info))
	  (setcar info new-group)
	  (unless (gnus-server-equal method "native")
	    (unless (nthcdr 3 info)
	      (nconc info (list nil nil)))
	    (unless (nthcdr 4 info)
	      (nconc info (list nil)))
	    (gnus-info-set-method info method))
	  (gnus-group-set-info info))
      (gnus-group-set-info form (or new-group group) part))
    (kill-buffer (current-buffer))
    (and winconf (set-window-configuration winconf))
    (set-buffer gnus-group-buffer)
    (gnus-group-update-group (or new-group group))
    (gnus-group-position-point)))

(defun gnus-group-make-help-group ()
  "Create the Gnus documentation group."
  (interactive)
  (let ((path load-path)
	(name (gnus-group-prefixed-name "gnus-help" '(nndoc "gnus-help")))
	file dir)
    (and (gnus-gethash name gnus-newsrc-hashtb)
	 (error "Documentation group already exists"))
    (while path
      (setq dir (file-name-as-directory (expand-file-name (pop path)))
	    file nil)
      (when (or (file-exists-p (setq file (concat dir "gnus-tut.txt")))
		(file-exists-p
		 (setq file (concat (file-name-directory
				     (directory-file-name dir))
				    "etc/gnus-tut.txt"))))
	(setq path nil)))
    (if (not file)
	(gnus-message 1 "Couldn't find doc group")
      (gnus-group-make-group
       (gnus-group-real-name name)
       (list 'nndoc "gnus-help"
	     (list 'nndoc-address file)
	     (list 'nndoc-article-type 'mbox)))))
  (gnus-group-position-point))

(defun gnus-group-make-doc-group (file type)
  "Create a group that uses a single file as the source."
  (interactive
   (list (read-file-name "File name: ")
	 (and current-prefix-arg 'ask)))
  (when (eq type 'ask)
    (let ((err "")
	  char found)
      (while (not found)
	(message
	 "%sFile type (mbox, babyl, digest, forward, mmfd, guess) [mbdfag]: "
	 err)
	(setq found (cond ((= (setq char (read-char)) ?m) 'mbox)
			  ((= char ?b) 'babyl)
			  ((= char ?d) 'digest)
			  ((= char ?f) 'forward)
			  ((= char ?a) 'mmfd)
			  (t (setq err (format "%c unknown. " char))
			     nil))))
      (setq type found)))
  (let* ((file (expand-file-name file))
	 (name (gnus-generate-new-group-name
		(gnus-group-prefixed-name
		 (file-name-nondirectory file) '(nndoc "")))))
    (gnus-group-make-group
     (gnus-group-real-name name)
     (list 'nndoc (file-name-nondirectory file)
	   (list 'nndoc-address file)
	   (list 'nndoc-article-type (or type 'guess))))))

(defun gnus-group-make-archive-group (&optional all)
  "Create the (ding) Gnus archive group of the most recent articles.
Given a prefix, create a full group."
  (interactive "P")
  (let ((group (gnus-group-prefixed-name
		(if all "ding.archives" "ding.recent") '(nndir ""))))
    (and (gnus-gethash group gnus-newsrc-hashtb)
	 (error "Archive group already exists"))
    (gnus-group-make-group
     (gnus-group-real-name group)
     (list 'nndir (if all "hpc" "edu")
	   (list 'nndir-directory
		 (if all gnus-group-archive-directory
		   gnus-group-recent-archive-directory))))))

(defun gnus-group-make-directory-group (dir)
  "Create an nndir group.
The user will be prompted for a directory.  The contents of this
directory will be used as a newsgroup.	The directory should contain
mail messages or news articles in files that have numeric names."
  (interactive
   (list (read-file-name "Create group from directory: ")))
  (or (file-exists-p dir) (error "No such directory"))
  (or (file-directory-p dir) (error "Not a directory"))
  (let ((ext "")
	(i 0)
	group)
    (while (or (not group) (gnus-gethash group gnus-newsrc-hashtb))
      (setq group
	    (gnus-group-prefixed-name
	     (concat (file-name-as-directory (directory-file-name dir))
		     ext)
	     '(nndir "")))
      (setq ext (format "<%d>" (setq i (1+ i)))))
    (gnus-group-make-group
     (gnus-group-real-name group)
     (list 'nndir group (list 'nndir-directory dir)))))

(defun gnus-group-make-kiboze-group (group address scores)
  "Create an nnkiboze group.
The user will be prompted for a name, a regexp to match groups, and
score file entries for articles to include in the group."
  (interactive
   (list
    (read-string "nnkiboze group name: ")
    (read-string "Source groups (regexp): ")
    (let ((headers (mapcar (lambda (group) (list group))
			   '("subject" "from" "number" "date" "message-id"
			     "references" "chars" "lines" "xref"
			     "followup" "all" "body" "head")))
	  scores header regexp regexps)
      (while (not (equal "" (setq header (completing-read
					  "Match on header: " headers nil t))))
	(setq regexps nil)
	(while (not (equal "" (setq regexp (read-string
					    (format "Match on %s (string): "
						    header)))))
	  (setq regexps (cons (list regexp nil nil 'r) regexps)))
	(setq scores (cons (cons header regexps) scores)))
      scores)))
  (gnus-group-make-group group "nnkiboze" address)
  (nnheader-temp-write (gnus-score-file-name (concat "nnkiboze:" group))
    (let (emacs-lisp-mode-hook)
      (pp scores (current-buffer)))))

(defun gnus-group-add-to-virtual (n vgroup)
  "Add the current group to a virtual group."
  (interactive
   (list current-prefix-arg
	 (completing-read "Add to virtual group: " gnus-newsrc-hashtb nil t
			  "nnvirtual:")))
  (or (eq (car (gnus-find-method-for-group vgroup)) 'nnvirtual)
      (error "%s is not an nnvirtual group" vgroup))
  (let* ((groups (gnus-group-process-prefix n))
	 (method (gnus-info-method (gnus-get-info vgroup))))
    (setcar (cdr method)
	    (concat
	     (nth 1 method) "\\|"
	     (mapconcat
	      (lambda (s)
		(gnus-group-remove-mark s)
		(concat "\\(^" (regexp-quote s) "$\\)"))
	      groups "\\|"))))
  (gnus-group-position-point))

(defun gnus-group-make-empty-virtual (group)
  "Create a new, fresh, empty virtual group."
  (interactive "sCreate new, empty virtual group: ")
  (let* ((method (list 'nnvirtual "^$"))
	 (pgroup (gnus-group-prefixed-name group method)))
    ;; Check whether it exists already.
    (and (gnus-gethash pgroup gnus-newsrc-hashtb)
	 (error "Group %s already exists." pgroup))
    ;; Subscribe the new group after the group on the current line.
    (gnus-subscribe-group pgroup (gnus-group-group-name) method)
    (gnus-group-update-group pgroup)
    (forward-line -1)
    (gnus-group-position-point)))

(defun gnus-group-enter-directory (dir)
  "Enter an ephemeral nneething group."
  (interactive "DDirectory to read: ")
  (let* ((method (list 'nneething dir))
	 (leaf (gnus-group-prefixed-name
		(file-name-nondirectory (directory-file-name dir))
		method))
	 (name (gnus-generate-new-group-name leaf)))
    (let ((nneething-read-only t))
      (or (gnus-group-read-ephemeral-group
	   name method t
	   (cons (current-buffer) (if (eq major-mode 'gnus-summary-mode)
				      'summary 'group)))
	  (error "Couldn't enter %s" dir)))))

;; Group sorting commands
;; Suggested by Joe Hildebrand <hildjj@idaho.fuentez.com>.

(defun gnus-group-sort-groups (func &optional reverse)
  "Sort the group buffer according to FUNC.
If REVERSE, reverse the sorting order."
  (interactive (list gnus-group-sort-function
		     current-prefix-arg))
  (let ((func (cond 
	       ((not (listp func)) func)
	       ((null func) func)
	       ((= 1 (length func)) (car func))
	       (t `(lambda (t1 t2)
		     ,(gnus-make-sort-function 
		       (reverse func)))))))
    ;; We peel off the dummy group from the alist.
    (when func
      (when (equal (car (gnus-info-group gnus-newsrc-alist)) "dummy.group")
	(pop gnus-newsrc-alist))
      ;; Do the sorting.
      (setq gnus-newsrc-alist
	    (sort gnus-newsrc-alist func))
      (when reverse
	(setq gnus-newsrc-alist (nreverse gnus-newsrc-alist)))
      ;; Regenerate the hash table.
      (gnus-make-hashtable-from-newsrc-alist)
      (gnus-group-list-groups))))

(defun gnus-group-sort-groups-by-alphabet (&optional reverse)
  "Sort the group buffer alphabetically by group name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-alphabet reverse))

(defun gnus-group-sort-groups-by-unread (&optional reverse)
  "Sort the group buffer by number of unread articles.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-unread reverse))

(defun gnus-group-sort-groups-by-level (&optional reverse)
  "Sort the group buffer by group level.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-level reverse))

(defun gnus-group-sort-groups-by-score (&optional reverse)
  "Sort the group buffer by group score.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-score reverse))

(defun gnus-group-sort-groups-by-rank (&optional reverse)
  "Sort the group buffer by group rank.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-rank reverse))

(defun gnus-group-sort-groups-by-method (&optional reverse)
  "Sort the group buffer alphabetically by backend name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-method reverse))

(defun gnus-group-sort-by-alphabet (info1 info2)
  "Sort alphabetically."
  (string< (gnus-info-group info1) (gnus-info-group info2)))

(defun gnus-group-sort-by-unread (info1 info2)
  "Sort by number of unread articles."
  (let ((n1 (car (gnus-gethash (gnus-info-group info1) gnus-newsrc-hashtb)))
	(n2 (car (gnus-gethash (gnus-info-group info2) gnus-newsrc-hashtb))))
    (< (or (and (numberp n1) n1) 0)
       (or (and (numberp n2) n2) 0))))

(defun gnus-group-sort-by-level (info1 info2)
  "Sort by level."
  (< (gnus-info-level info1) (gnus-info-level info2)))

(defun gnus-group-sort-by-method (info1 info2)
  "Sort alphabetically by backend name."
  (string< (symbol-name (car (gnus-find-method-for-group
			      (gnus-info-group info1) info1)))
	   (symbol-name (car (gnus-find-method-for-group
			      (gnus-info-group info2) info2)))))

(defun gnus-group-sort-by-score (info1 info2)
  "Sort by group score."
  (< (gnus-info-score info1) (gnus-info-score info2)))

(defun gnus-group-sort-by-rank (info1 info2)
  "Sort by level and score."
  (let ((level1 (gnus-info-level info1))
	(level2 (gnus-info-level info2)))
    (or (< level1 level2)
	(and (= level1 level2)
	     (> (gnus-info-score info1) (gnus-info-score info2))))))

;; Group catching up.

(defun gnus-group-clear-data (n)
  "Clear all marks and read ranges from the current group."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group info)
    (while (setq group (pop groups))
      (setq info (gnus-get-info group))
      (gnus-info-set-read info nil)
      (when (gnus-info-marks info)
	(gnus-info-set-marks info nil))
      (gnus-get-unread-articles-in-group info (gnus-active group) t)
      (when (gnus-group-goto-group group)
	(gnus-group-remove-mark group)
	(gnus-group-update-group-line)))))

(defun gnus-group-catchup-current (&optional n all)
  "Mark all articles not marked as unread in current newsgroup as read.
If prefix argument N is numeric, the ARG next newsgroups will be
caught up.  If ALL is non-nil, marked articles will also be marked as
read.  Cross references (Xref: header) of articles are ignored.
The difference between N and actual number of newsgroups that were
caught up is returned."
  (interactive "P")
  (unless (gnus-group-group-name)
    (error "No group on the current line"))
  (if (not (or (not gnus-interactive-catchup) ;Without confirmation?
	       gnus-expert-user
	       (gnus-y-or-n-p
		(if all
		    "Do you really want to mark all articles as read? "
		  "Mark all unread articles as read? "))))
      n
    (let ((groups (gnus-group-process-prefix n))
	  (ret 0))
      (while groups
	;; Virtual groups have to be given special treatment.
	(let ((method (gnus-find-method-for-group (car groups))))
	  (if (eq 'nnvirtual (car method))
	      (nnvirtual-catchup-group
	       (gnus-group-real-name (car groups)) (nth 1 method) all)))
	(gnus-group-remove-mark (car groups))
	(if (>= (gnus-group-group-level) gnus-level-zombie)
	    (gnus-message 2 "Dead groups can't be caught up")
	  (if (prog1
		  (gnus-group-goto-group (car groups))
		(gnus-group-catchup (car groups) all))
	      (gnus-group-update-group-line)
	    (setq ret (1+ ret))))
	(setq groups (cdr groups)))
      (gnus-group-next-unread-group 1)
      ret)))

(defun gnus-group-catchup-current-all (&optional n)
  "Mark all articles in current newsgroup as read.
Cross references (Xref: header) of articles are ignored."
  (interactive "P")
  (gnus-group-catchup-current n 'all))

(defun gnus-group-catchup (group &optional all)
  "Mark all articles in GROUP as read.
If ALL is non-nil, all articles are marked as read.
The return value is the number of articles that were marked as read,
or nil if no action could be taken."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (num (car entry)))
    ;; Do the updating only if the newsgroup isn't killed.
    (if (not (numberp (car entry)))
	(gnus-message 1 "Can't catch up; non-active group")
      ;; Do auto-expirable marks if that's required.
      (when (gnus-group-auto-expirable-p group)
	(gnus-add-marked-articles
	 group 'expire (gnus-list-of-unread-articles group))
	(when all
	  (let ((marks (nth 3 (nth 2 entry))))
	    (gnus-add-marked-articles
	     group 'expire (gnus-uncompress-range (cdr (assq 'tick marks))))
	    (gnus-add-marked-articles
	     group 'expire (gnus-uncompress-range (cdr (assq 'tick marks)))))))
      (when entry
	(gnus-update-read-articles group nil)
	;; Also nix out the lists of marks and dormants.
	(when all
	  (gnus-add-marked-articles group 'tick nil nil 'force)
	  (gnus-add-marked-articles group 'dormant nil nil 'force))
	(run-hooks 'gnus-group-catchup-group-hook)
	num))))

(defun gnus-group-expire-articles (&optional n)
  "Expire all expirable articles in the current newsgroup."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group)
    (unless groups
      (error "No groups to expire"))
    (while (setq group (pop groups))
      (gnus-group-remove-mark group)
      (when (gnus-check-backend-function 'request-expire-articles group)
	(gnus-message 6 "Expiring articles in %s..." group)
	(let* ((info (gnus-get-info group))
	       (expirable (if (gnus-group-total-expirable-p group)
			      (cons nil (gnus-list-of-read-articles group))
			    (assq 'expire (gnus-info-marks info))))
	       (expiry-wait (gnus-group-get-parameter group 'expiry-wait)))
	  (when expirable
	    (setcdr
	     expirable
	     (gnus-compress-sequence
	      (if expiry-wait
		  ;; We set the expiry variables to the groupp
		  ;; parameter. 
		  (let ((nnmail-expiry-wait-function nil)
			(nnmail-expiry-wait expiry-wait))
		    (gnus-request-expire-articles
		     (gnus-uncompress-sequence (cdr expirable)) group))
		;; Just expire using the normal expiry values.
		(gnus-request-expire-articles
		 (gnus-uncompress-sequence (cdr expirable)) group))))
	    (gnus-close-group group))
	  (gnus-message 6 "Expiring articles in %s...done" group)))
      (gnus-group-position-point))))

(defun gnus-group-expire-all-groups ()
  "Expire all expirable articles in all newsgroups."
  (interactive)
  (save-excursion
    (gnus-message 5 "Expiring...")
    (let ((gnus-group-marked (mapcar (lambda (info) (gnus-info-group info))
				     (cdr gnus-newsrc-alist))))
      (gnus-group-expire-articles nil)))
  (gnus-group-position-point)
  (gnus-message 5 "Expiring...done"))

(defun gnus-group-set-current-level (n level)
  "Set the level of the next N groups to LEVEL."
  (interactive
   (list
    current-prefix-arg
    (string-to-int
     (let ((s (read-string
	       (format "Level (default %s): "
		       (or (gnus-group-group-level) 
			   gnus-level-default-subscribed)))))
       (if (string-match "^\\s-*$" s)
	   (int-to-string (or (gnus-group-group-level) 
			      gnus-level-default-subscribed))
	 s)))))
  (or (and (>= level 1) (<= level gnus-level-killed))
      (error "Illegal level: %d" level))
  (let ((groups (gnus-group-process-prefix n))
	group)
    (while (setq group (pop groups))
      (gnus-group-remove-mark group)
      (gnus-message 6 "Changed level of %s from %d to %d"
		    group (or (gnus-group-group-level) gnus-level-killed)
		    level)
      (gnus-group-change-level
       group level (or (gnus-group-group-level) gnus-level-killed))
      (gnus-group-update-group-line)))
  (gnus-group-position-point))

(defun gnus-group-unsubscribe-current-group (&optional n)
  "Toggle subscription of the current group.
If given numerical prefix, toggle the N next groups."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (gnus-group-unsubscribe-group
       group (if (<= (gnus-group-group-level) gnus-level-subscribed)
		 gnus-level-default-unsubscribed
	       gnus-level-default-subscribed) t)
      (gnus-group-update-group-line))
    (gnus-group-next-group 1)))

(defun gnus-group-unsubscribe-group (group &optional level silent)
  "Toggle subscription to GROUP.
Killed newsgroups are subscribed.  If SILENT, don't try to update the
group line."
  (interactive
   (list (completing-read
	  "Group: " gnus-active-hashtb nil
	  (gnus-read-active-file-p)
	  nil 
	  'gnus-group-history)))
  (let ((newsrc (gnus-gethash group gnus-newsrc-hashtb)))
    (cond
     ((string-match "^[ \t]$" group)
      (error "Empty group name"))
     (newsrc
      ;; Toggle subscription flag.
      (gnus-group-change-level
       newsrc (if level level (if (<= (nth 1 (nth 2 newsrc))
				      gnus-level-subscribed)
				  (1+ gnus-level-subscribed)
				gnus-level-default-subscribed)))
      (unless silent
	(gnus-group-update-group group)))
     ((and (stringp group)
	   (or (not (gnus-read-active-file-p))
	       (gnus-active group)))
      ;; Add new newsgroup.
      (gnus-group-change-level
       group
       (if level level gnus-level-default-subscribed)
       (or (and (member group gnus-zombie-list)
		gnus-level-zombie)
	   gnus-level-killed)
       (and (gnus-group-group-name)
	    (gnus-gethash (gnus-group-group-name) gnus-newsrc-hashtb)))
      (unless silent
	(gnus-group-update-group group)))
     (t (error "No such newsgroup: %s" group)))
    (gnus-group-position-point)))

(defun gnus-group-transpose-groups (n)
  "Move the current newsgroup up N places.
If given a negative prefix, move down instead.	The difference between
N and the number of steps taken is returned."
  (interactive "p")
  (or (gnus-group-group-name)
      (error "No group on current line"))
  (gnus-group-kill-group 1)
  (prog1
      (forward-line (- n))
    (gnus-group-yank-group)
    (gnus-group-position-point)))

(defun gnus-group-kill-all-zombies ()
  "Kill all zombie newsgroups."
  (interactive)
  (setq gnus-killed-list (nconc gnus-zombie-list gnus-killed-list))
  (setq gnus-zombie-list nil)
  (gnus-group-list-groups))

(defun gnus-group-kill-region (begin end)
  "Kill newsgroups in current region (excluding current point).
The killed newsgroups can be yanked by using \\[gnus-group-yank-group]."
  (interactive "r")
  (let ((lines
	 ;; Count lines.
	 (save-excursion
	   (count-lines
	    (progn
	      (goto-char begin)
	      (beginning-of-line)
	      (point))
	    (progn
	      (goto-char end)
	      (beginning-of-line)
	      (point))))))
    (goto-char begin)
    (beginning-of-line)			;Important when LINES < 1
    (gnus-group-kill-group lines)))

(defun gnus-group-kill-group (&optional n discard)
  "Kill the next N groups.
The killed newsgroups can be yanked by using \\[gnus-group-yank-group].
However, only groups that were alive can be yanked; already killed
groups or zombie groups can't be yanked.
The return value is the name of the group that was killed, or a list
of groups killed."
  (interactive "P")
  (let ((buffer-read-only nil)
	(groups (gnus-group-process-prefix n))
	group entry level out)
    (if (< (length groups) 10)
	;; This is faster when there are few groups.
	(while groups
	  (push (setq group (pop groups)) out)
	  (gnus-group-remove-mark group)
	  (setq level (gnus-group-group-level))
	  (gnus-delete-line)
	  (when (and (not discard)
		     (setq entry (gnus-gethash group gnus-newsrc-hashtb)))
	    (push (cons (car entry) (nth 2 entry))
		  gnus-list-of-killed-groups))
	  (gnus-group-change-level
	   (if entry entry group) gnus-level-killed (if entry nil level)))
      ;; If there are lots and lots of groups to be killed, we use
      ;; this thing instead.
      (let (entry)
	(setq groups (nreverse groups))
	(while groups
	  (gnus-group-remove-mark (setq group (pop groups)))
	  (gnus-delete-line)
	  (push group gnus-killed-list)
	  (setq gnus-newsrc-alist
		(delq (assoc group gnus-newsrc-alist)
		      gnus-newsrc-alist))
	  (when gnus-group-change-level-function
	    (funcall gnus-group-change-level-function group 9 3))
	  (cond
	   ((setq entry (gnus-gethash group gnus-newsrc-hashtb))
	    (push (cons (car entry) (nth 2 entry))
		  gnus-list-of-killed-groups)
	    (setcdr (cdr entry) (cdddr entry)))
	   ((member group gnus-zombie-list)
	    (setq gnus-zombie-list (delete group gnus-zombie-list)))))
	(gnus-make-hashtable-from-newsrc-alist)))

    (gnus-group-position-point)
    (if (< (length out) 2) (car out) (nreverse out))))

(defun gnus-group-yank-group (&optional arg)
  "Yank the last newsgroups killed with \\[gnus-group-kill-group],
inserting it before the current newsgroup.  The numeric ARG specifies
how many newsgroups are to be yanked.  The name of the newsgroup yanked
is returned, or (if several groups are yanked) a list of yanked groups
is returned."
  (interactive "p")
  (setq arg (or arg 1))
  (let (info group prev out)
    (while (>= (decf arg) 0)
      (if (not (setq info (pop gnus-list-of-killed-groups)))
	  (error "No more newsgroups to yank"))
      (push (setq group (nth 1 info)) out)
      ;; Find which newsgroup to insert this one before - search
      ;; backward until something suitable is found.  If there are no
      ;; other newsgroups in this buffer, just make this newsgroup the
      ;; first newsgroup.
      (setq prev (gnus-group-group-name))
      (gnus-group-change-level
       info (gnus-info-level (cdr info)) gnus-level-killed
       (and prev (gnus-gethash prev gnus-newsrc-hashtb))
       t)
      (gnus-group-insert-group-line-info group))
    (forward-line -1)
    (gnus-group-position-point)
    (if (< (length out) 2) (car out) (nreverse out))))

(defun gnus-group-kill-level (level)
  "Kill all groups that is on a certain LEVEL."
  (interactive "nKill all groups on level: ")
  (cond
   ((= level gnus-level-zombie)
    (setq gnus-killed-list
	  (nconc gnus-zombie-list gnus-killed-list))
    (setq gnus-zombie-list nil))
   ((and (< level gnus-level-zombie)
	 (> level 0)
	 (or gnus-expert-user
	     (gnus-yes-or-no-p
	      (format
	       "Do you really want to kill all groups on level %d? "
	       level))))
    (let* ((prev gnus-newsrc-alist)
	   (alist (cdr prev)))
      (while alist
	(if (= (gnus-info-level (car alist)) level)
	    (progn
	      (push (gnus-info-group (car alist)) gnus-killed-list)
	      (setcdr prev (cdr alist)))
	  (setq prev alist))
	(setq alist (cdr alist)))
      (gnus-make-hashtable-from-newsrc-alist)
      (gnus-group-list-groups)))
   (t
    (error "Can't kill; illegal level: %d" level))))

(defun gnus-group-list-all-groups (&optional arg)
  "List all newsgroups with level ARG or lower.
Default is gnus-level-unsubscribed, which lists all subscribed and most
unsubscribed groups."
  (interactive "P")
  (gnus-group-list-groups (or arg gnus-level-unsubscribed) t))

;; Redefine this to list ALL killed groups if prefix arg used.
;; Rewritten by engstrom@src.honeywell.com (Eric Engstrom).
(defun gnus-group-list-killed (&optional arg)
  "List all killed newsgroups in the group buffer.
If ARG is non-nil, list ALL killed groups known to Gnus.  This may
entail asking the server for the groups."
  (interactive "P")
  ;; Find all possible killed newsgroups if arg.
  (when arg
    (gnus-get-killed-groups))
  (if (not gnus-killed-list)
      (gnus-message 6 "No killed groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function
	       gnus-level-killed t gnus-level-killed))
    (goto-char (point-min)))
  (gnus-group-position-point))

(defun gnus-group-list-zombies ()
  "List all zombie newsgroups in the group buffer."
  (interactive)
  (if (not gnus-zombie-list)
      (gnus-message 6 "No zombie groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function
	       gnus-level-zombie t gnus-level-zombie))
    (goto-char (point-min)))
  (gnus-group-position-point))

(defun gnus-group-list-active ()
  "List all groups that are available from the server(s)."
  (interactive)
  ;; First we make sure that we have really read the active file.
  (unless (gnus-read-active-file-p)
    (let ((gnus-read-active-file t))
      (gnus-read-active-file)))
  ;; Find all groups and sort them.
  (let ((groups
	 (sort
	  (let (list)
	    (mapatoms
	     (lambda (sym)
	       (and (boundp sym)
		    (symbol-value sym)
		    (setq list (cons (symbol-name sym) list))))
	     gnus-active-hashtb)
	    list)
	  'string<))
	(buffer-read-only nil))
    (erase-buffer)
    (while groups
      (gnus-group-insert-group-line-info (pop groups)))
    (goto-char (point-min))))

(defun gnus-activate-all-groups (level)
  "Activate absolutely all groups."
  (interactive (list 7))
  (let ((gnus-activate-level level)
	(gnus-activate-foreign-newsgroups level))
    (gnus-group-get-new-news)))

(defun gnus-group-get-new-news (&optional arg)
  "Get newly arrived articles.
If ARG is a number, it specifies which levels you are interested in
re-scanning.  If ARG is non-nil and not a number, this will force
\"hard\" re-reading of the active files from all servers."
  (interactive "P")
  (run-hooks 'gnus-get-new-news-hook)
  ;; We might read in new NoCeM messages here.
  (when (and gnus-use-nocem 
	     (null arg))
    (gnus-nocem-scan-groups))
  ;; If ARG is not a number, then we read the active file.
  (when (and arg (not (numberp arg)))
    (let ((gnus-read-active-file t))
      (gnus-read-active-file))
    (setq arg nil))

  (setq arg (gnus-group-default-level arg t))
  (if (and gnus-read-active-file (not arg))
      (progn
	(gnus-read-active-file)
	(gnus-get-unread-articles arg))
    (let ((gnus-read-active-file (if arg nil gnus-read-active-file)))
      (gnus-get-unread-articles arg)))
  (run-hooks 'gnus-after-getting-new-news-hook)
  (gnus-group-list-groups))

(defun gnus-group-get-new-news-this-group (&optional n)
  "Check for newly arrived news in the current group (and the N-1 next groups).
The difference between N and the number of newsgroup checked is returned.
If N is negative, this group and the N-1 previous groups will be checked."
  (interactive "P")
  (let* ((groups (gnus-group-process-prefix n))
	 (ret (if (numberp n) (- n (length groups)) 0))
	 (beg (unless n (point)))
	 group)
    (while (setq group (pop groups))
      (gnus-group-remove-mark group)
      (if (gnus-activate-group group 'scan)
	  (progn
	    (gnus-get-unread-articles-in-group
	     (gnus-get-info group) (gnus-active group) t)
	    (unless (gnus-virtual-group-p group)
	      (gnus-close-group group))
	    (gnus-group-update-group group))
	(if (eq (gnus-server-status (gnus-find-method-for-group group))
		'denied)
	    (gnus-error "Server denied access")
	  (gnus-error 3 "%s error: %s" group (gnus-status-message group)))))
    (when beg (goto-char beg))
    (when gnus-goto-next-group-when-activating
      (gnus-group-next-unread-group 1 t))
    (gnus-summary-position-point)
    ret))

(defun gnus-group-fetch-faq (group &optional faq-dir)
  "Fetch the FAQ for the current group."
  (interactive
   (list
    (and (gnus-group-group-name)
	 (gnus-group-real-name (gnus-group-group-name)))
    (cond (current-prefix-arg
	   (completing-read
	    "Faq dir: " (and (listp gnus-group-faq-directory)
			     (mapcar (lambda (file) (list file))
				     gnus-group-faq-directory)))))))
  (or faq-dir
      (setq faq-dir (if (listp gnus-group-faq-directory)
			(car gnus-group-faq-directory)
		      gnus-group-faq-directory)))
  (or group (error "No group name given"))
  (let ((file (concat (file-name-as-directory faq-dir)
		      (gnus-group-real-name group))))
    (if (not (file-exists-p file))
	(error "No such file: %s" file)
      (find-file file))))

(defun gnus-group-describe-group (force &optional group)
  "Display a description of the current newsgroup."
  (interactive (list current-prefix-arg (gnus-group-group-name)))
  (let* ((method (gnus-find-method-for-group group))
	 (mname (gnus-group-prefixed-name "" method))
	 desc)
    (when (and force
	       gnus-description-hashtb)
      (gnus-sethash mname nil gnus-description-hashtb))
    (or group (error "No group name given"))
    (and (or (and gnus-description-hashtb
		  ;; We check whether this group's method has been
		  ;; queried for a description file.
		  (gnus-gethash mname gnus-description-hashtb))
	     (setq desc (gnus-group-get-description group))
	     (gnus-read-descriptions-file method))
	 (gnus-message 1
	  (or desc (gnus-gethash group gnus-description-hashtb)
	      "No description available")))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-describe-all-groups (&optional force)
  "Pop up a buffer with descriptions of all newsgroups."
  (interactive "P")
  (and force (setq gnus-description-hashtb nil))
  (if (not (or gnus-description-hashtb
	       (gnus-read-all-descriptions-files)))
      (error "Couldn't request descriptions file"))
  (let ((buffer-read-only nil)
	b)
    (erase-buffer)
    (mapatoms
     (lambda (group)
       (setq b (point))
       (insert (format "      *: %-20s %s\n" (symbol-name group)
		       (symbol-value group)))
       (gnus-add-text-properties
	b (1+ b) (list 'gnus-group group
		       'gnus-unread t 'gnus-marked nil
		       'gnus-level (1+ gnus-level-subscribed))))
     gnus-description-hashtb)
    (goto-char (point-min))
    (gnus-group-position-point)))

;; Suggested by by Daniel Quinlan <quinlan@best.com>.
(defun gnus-group-apropos (regexp &optional search-description)
  "List all newsgroups that have names that match a regexp."
  (interactive "sGnus apropos (regexp): ")
  (let ((prev "")
	(obuf (current-buffer))
	groups des)
    ;; Go through all newsgroups that are known to Gnus.
    (mapatoms
     (lambda (group)
       (and (symbol-name group)
	    (string-match regexp (symbol-name group))
	    (setq groups (cons (symbol-name group) groups))))
     gnus-active-hashtb)
    ;; Also go through all descriptions that are known to Gnus.
    (when search-description
      (mapatoms
       (lambda (group)
	 (and (string-match regexp (symbol-value group))
	      (gnus-active (symbol-name group))
	      (setq groups (cons (symbol-name group) groups))))
       gnus-description-hashtb))
    (if (not groups)
	(gnus-message 3 "No groups matched \"%s\"." regexp)
      ;; Print out all the groups.
      (save-excursion
	(pop-to-buffer "*Gnus Help*")
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(setq groups (sort groups 'string<))
	(while groups
	  ;; Groups may be entered twice into the list of groups.
	  (if (not (string= (car groups) prev))
	      (progn
		(insert (setq prev (car groups)) "\n")
		(if (and gnus-description-hashtb
			 (setq des (gnus-gethash (car groups)
						 gnus-description-hashtb)))
		    (insert "  " des "\n"))))
	  (setq groups (cdr groups)))
	(goto-char (point-min))))
    (pop-to-buffer obuf)))

(defun gnus-group-description-apropos (regexp)
  "List all newsgroups that have names or descriptions that match a regexp."
  (interactive "sGnus description apropos (regexp): ")
  (if (not (or gnus-description-hashtb
	       (gnus-read-all-descriptions-files)))
      (error "Couldn't request descriptions file"))
  (gnus-group-apropos regexp t))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-list-matching (level regexp &optional all lowest)
  "List all groups with unread articles that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If ALL, also list groups with no unread articles.
If LOWEST, don't list groups with level lower than LOWEST.

This command may read the active file."
  (interactive "P\nsList newsgroups matching: ")
  ;; First make sure active file has been read.
  (when (and level
	     (> (prefix-numeric-value level) gnus-level-killed))
    (gnus-get-killed-groups))
  (gnus-group-prepare-flat (or level gnus-level-subscribed)
			   all (or lowest 1) regexp)
  (goto-char (point-min))
  (gnus-group-position-point))

(defun gnus-group-list-all-matching (level regexp &optional lowest)
  "List all groups that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If LOWEST, don't list groups with level lower than LOWEST."
  (interactive "P\nsList newsgroups matching: ")
  (gnus-group-list-matching (or level gnus-level-killed) regexp t lowest))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-group-save-newsrc (&optional force)
  "Save the Gnus startup files.
If FORCE, force saving whether it is necessary or not."
  (interactive "P")
  (gnus-save-newsrc-file force))

(defun gnus-group-restart (&optional arg)
  "Force Gnus to read the .newsrc file."
  (interactive "P")
  (when (gnus-yes-or-no-p
	 (format "Are you sure you want to read %s? "
		 gnus-current-startup-file))
    (gnus-save-newsrc-file)
    (gnus-setup-news 'force)
    (gnus-group-list-groups arg)))

(defun gnus-group-read-init-file ()
  "Read the Gnus elisp init file."
  (interactive)
  (gnus-read-init-file))

(defun gnus-group-check-bogus-groups (&optional silent)
  "Check bogus newsgroups.
If given a prefix, don't ask for confirmation before removing a bogus
group."
  (interactive "P")
  (gnus-check-bogus-newsgroups (and (not silent) (not gnus-expert-user)))
  (gnus-group-list-groups))

(defun gnus-group-edit-global-kill (&optional article group)
  "Edit the global kill file.
If GROUP, edit that local kill file instead."
  (interactive "P")
  (setq gnus-current-kill-article article)
  (gnus-kill-file-edit-file group)
  (gnus-message
   6
   (substitute-command-keys
    (format "Editing a %s kill file (Type \\[gnus-kill-file-exit] to exit)"
	    (if group "local" "global")))))

(defun gnus-group-edit-local-kill (article group)
  "Edit a local kill file."
  (interactive (list nil (gnus-group-group-name)))
  (gnus-group-edit-global-kill article group))

(defun gnus-group-force-update ()
  "Update `.newsrc' file."
  (interactive)
  (gnus-save-newsrc-file))

(defun gnus-group-suspend ()
  "Suspend the current Gnus session.
In fact, cleanup buffers except for group mode buffer.
The hook gnus-suspend-gnus-hook is called before actually suspending."
  (interactive)
  (run-hooks 'gnus-suspend-gnus-hook)
  ;; Kill Gnus buffers except for group mode buffer.
  (let* ((group-buf (get-buffer gnus-group-buffer))
	 ;; Do this on a separate list in case the user does a ^G before we finish
	 (gnus-buffer-list
	  (delete group-buf (delete gnus-dribble-buffer
				    (append gnus-buffer-list nil)))))
    (while gnus-buffer-list
      (gnus-kill-buffer (pop gnus-buffer-list)))
    (gnus-kill-gnus-frames)
    (when group-buf
      (setq gnus-buffer-list (list group-buf))
      (bury-buffer group-buf)
      (delete-windows-on group-buf t))))

(defun gnus-group-clear-dribble ()
  "Clear all information from the dribble buffer."
  (interactive)
  (gnus-dribble-clear)
  (gnus-message 7 "Cleared dribble buffer"))

(defun gnus-group-exit ()
  "Quit reading news after updating .newsrc.eld and .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (when 
      (or noninteractive		;For gnus-batch-kill
	  (not gnus-interactive-exit)	;Without confirmation
	  gnus-expert-user
	  (gnus-y-or-n-p "Are you sure you want to quit reading news? "))
    (run-hooks 'gnus-exit-gnus-hook)
    ;; Offer to save data from non-quitted summary buffers.
    (gnus-offer-save-summaries)
    ;; Save the newsrc file(s).
    (gnus-save-newsrc-file)
    ;; Kill-em-all.
    (gnus-close-backends)
    ;; Reset everything.
    (gnus-clear-system)
    ;; Allow the user to do things after cleaning up.
    (run-hooks 'gnus-after-exiting-gnus-hook)))

(defun gnus-close-backends ()
  ;; Send a close request to all backends that support such a request.
  (let ((methods gnus-valid-select-methods)
	func)
    (while methods
      (if (fboundp (setq func (intern (concat (caar methods)
					      "-request-close"))))
	  (funcall func))
      (setq methods (cdr methods)))))

(defun gnus-group-quit ()
  "Quit reading news without updating .newsrc.eld or .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (when (or noninteractive		;For gnus-batch-kill
	    (zerop (buffer-size))
	    (not (gnus-server-opened gnus-select-method))
	    gnus-expert-user
	    (not gnus-current-startup-file)
	    (gnus-yes-or-no-p
	     (format "Quit reading news without saving %s? "
		     (file-name-nondirectory gnus-current-startup-file))))
    (run-hooks 'gnus-exit-gnus-hook)
    (if gnus-use-full-window
	(delete-other-windows)
      (gnus-remove-some-windows))
    (gnus-dribble-save)
    (gnus-close-backends)
    (gnus-clear-system)
    ;; Allow the user to do things after cleaning up.
    (run-hooks 'gnus-after-exiting-gnus-hook)))

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
		    gnus-newsgroup-prepared))
	     (push bufname buffers))
	(setq buflist (cdr buflist)))
      ;; Go through all these summary buffers and offer to save them.
      (when buffers
	(map-y-or-n-p
	 "Update summary buffer %s? "
	 (lambda (buf) (set-buffer buf) (gnus-summary-exit))
	 buffers)))))

(defun gnus-group-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (gnus-message 7 (substitute-command-keys "\\<gnus-group-mode-map>\\[gnus-group-read-group]:Select  \\[gnus-group-next-unread-group]:Forward  \\[gnus-group-prev-unread-group]:Backward  \\[gnus-group-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-group-describe-briefly]:This help")))

(defun gnus-group-browse-foreign-server (method)
  "Browse a foreign news server.
If called interactively, this function will ask for a select method
 (nntp, nnspool, etc.) and a server address (eg. nntp.some.where).
If not, METHOD should be a list where the first element is the method
and the second element is the address."
  (interactive
   (list (let ((how (completing-read
		     "Which backend: "
		     (append gnus-valid-select-methods gnus-server-alist)
		     nil t (cons "nntp" 0) 'gnus-method-history)))
	   ;; We either got a backend name or a virtual server name.
	   ;; If the first, we also need an address.
	   (if (assoc how gnus-valid-select-methods)
	       (list (intern how)
		     ;; Suggested by mapjph@bath.ac.uk.
		     (completing-read
		      "Address: "
		      (mapcar (lambda (server) (list server))
			      gnus-secondary-servers)))
	     ;; We got a server name, so we find the method.
	     (gnus-server-to-method how)))))
  (gnus-browse-foreign-server method))


;;;
;;; Gnus summary mode
;;;

(defvar gnus-summary-mode-map nil)

(put 'gnus-summary-mode 'mode-class 'special)

(unless gnus-summary-mode-map
  (setq gnus-summary-mode-map (make-keymap))
  (suppress-keymap gnus-summary-mode-map)

  ;; Non-orthogonal keys

  (gnus-define-keys gnus-summary-mode-map
    " " gnus-summary-next-page
    "\177" gnus-summary-prev-page
    [delete] gnus-summary-prev-page
    "\r" gnus-summary-scroll-up
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
    "\M-\C-u" gnus-summary-up-thread
    "\M-\C-d" gnus-summary-down-thread
    "&" gnus-summary-execute-command
    "c" gnus-summary-catchup-and-exit
    "\C-w" gnus-summary-mark-region-as-read
    "\C-t" gnus-summary-toggle-truncation
    "?" gnus-summary-mark-as-dormant
    "\C-c\M-\C-s" gnus-summary-limit-include-expunged
    "\C-c\C-s\C-n" gnus-summary-sort-by-number
    "\C-c\C-s\C-a" gnus-summary-sort-by-author
    "\C-c\C-s\C-s" gnus-summary-sort-by-subject
    "\C-c\C-s\C-d" gnus-summary-sort-by-date
    "\C-c\C-s\C-i" gnus-summary-sort-by-score
    "=" gnus-summary-expand-window
    "\C-x\C-s" gnus-summary-reselect-current-group
    "\M-g" gnus-summary-rescan-group
    "w" gnus-summary-stop-page-breaking
    "\C-c\C-r" gnus-summary-caesar-message
    "\M-t" gnus-summary-toggle-mime
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
    "V" gnus-version
    "\C-c\C-d" gnus-summary-describe-group
    "q" gnus-summary-exit
    "Q" gnus-summary-exit-no-update
    "\C-c\C-i" gnus-info-find-node
    gnus-mouse-2 gnus-mouse-pick-article
    "m" gnus-summary-mail-other-window
    "a" gnus-summary-post-news
    "x" gnus-summary-limit-to-unread
    "s" gnus-summary-isearch-article
    "t" gnus-article-hide-headers
    "g" gnus-summary-show-article
    "l" gnus-summary-goto-last-article
    "\C-c\C-v\C-v" gnus-uu-decode-uu-view
    "\C-d" gnus-summary-enter-digest-group
    "\C-c\C-b" gnus-bug
    "*" gnus-cache-enter-article
    "\M-*" gnus-cache-remove-article
    "\M-&" gnus-summary-universal-argument
    "\C-l" gnus-recenter
    "I" gnus-summary-increase-score
    "L" gnus-summary-lower-score

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

  (gnus-define-keys (gnus-summary-mscore-map "V" gnus-summary-mode-map)
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
    "v" gnus-summary-limit-to-score
    "D" gnus-summary-limit-include-dormant
    "d" gnus-summary-limit-exclude-dormant
    ;;  "t" gnus-summary-limit-exclude-thread
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
    "p" gnus-summary-pop-article)

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
    "P" gnus-summary-prev-group)

  (gnus-define-keys (gnus-summary-article-map "A" gnus-summary-mode-map)
    " " gnus-summary-next-page
    "n" gnus-summary-next-page
    "\177" gnus-summary-prev-page
    [delete] gnus-summary-prev-page
    "p" gnus-summary-prev-page
    "\r" gnus-summary-scroll-up
    "<" gnus-summary-beginning-of-article
    ">" gnus-summary-end-of-article
    "b" gnus-summary-beginning-of-article
    "e" gnus-summary-end-of-article
    "^" gnus-summary-refer-parent-article
    "r" gnus-summary-refer-parent-article
    "R" gnus-summary-refer-references
    "g" gnus-summary-show-article
    "s" gnus-summary-isearch-article)

  (gnus-define-keys (gnus-summary-wash-map "W" gnus-summary-mode-map)
    "b" gnus-article-add-buttons
    "B" gnus-article-add-buttons-to-head
    "o" gnus-article-treat-overstrike
    ;;  "w" gnus-article-word-wrap
    "w" gnus-article-fill-cited-article
    "c" gnus-article-remove-cr
    "L" gnus-article-remove-trailing-blank-lines
    "q" gnus-article-de-quoted-unreadable
    "f" gnus-article-display-x-face
    "l" gnus-summary-stop-page-breaking
    "r" gnus-summary-caesar-message
    "t" gnus-article-hide-headers
    "v" gnus-summary-verbose-headers
    "m" gnus-summary-toggle-mime)

  (gnus-define-keys (gnus-summary-wash-hide-map "W" gnus-summary-wash-map)
    "a" gnus-article-hide
    "h" gnus-article-hide-headers
    "b" gnus-article-hide-boring-headers
    "s" gnus-article-hide-signature
    "c" gnus-article-hide-citation
    "p" gnus-article-hide-pgp
    "\C-c" gnus-article-hide-citation-maybe)

  (gnus-define-keys (gnus-summary-wash-highlight-map "H" gnus-summary-wash-map)
    "a" gnus-article-highlight
    "h" gnus-article-highlight-headers
    "c" gnus-article-highlight-citation
    "s" gnus-article-highlight-signature)

  (gnus-define-keys (gnus-summary-wash-time-map "T" gnus-summary-wash-map)
    "z" gnus-article-date-ut
    "u" gnus-article-date-ut
    "l" gnus-article-date-local
    "e" gnus-article-date-lapsed
    "o" gnus-article-date-original)

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
    "m" gnus-summary-move-article
    "r" gnus-summary-respool-article
    "w" gnus-summary-edit-article
    "c" gnus-summary-copy-article
    "B" gnus-summary-crosspost-article
    "q" gnus-summary-respool-query
    "i" gnus-summary-import-article)

  (gnus-define-keys (gnus-summary-save-map "O" gnus-summary-mode-map)
    "o" gnus-summary-save-article
    "m" gnus-summary-save-article-mail
    "r" gnus-summary-save-article-rmail
    "f" gnus-summary-save-article-file
    "b" gnus-summary-save-article-body-file
    "h" gnus-summary-save-article-folder
    "v" gnus-summary-save-article-vm
    "p" gnus-summary-pipe-output
    "s" gnus-soup-add-article)
  )



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
  (when (and menu-bar-mode
	     (gnus-visual-p 'summary-menu 'menu))
    (gnus-summary-make-menu-bar))
  (kill-all-local-variables)
  (gnus-summary-make-local-variables)
  (gnus-make-thread-indent-array)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-summary-mode)
  (setq mode-name "Summary")
  (make-local-variable 'minor-mode-alist)
  (use-local-map gnus-summary-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (setq truncate-lines t)
  (setq selective-display t)
  (setq selective-display-ellipses t)	;Display `...'
  (setq buffer-display-table gnus-summary-display-table)
  (setq gnus-newsgroup-name group)
  (make-local-variable 'gnus-summary-line-format)
  (make-local-variable 'gnus-summary-line-format-spec)
  (make-local-variable 'gnus-summary-mark-positions)
  (gnus-make-local-hook 'post-command-hook)
  (gnus-add-hook 'post-command-hook 'gnus-clear-inboxes-moved nil t)
  (run-hooks 'gnus-summary-mode-hook))

(defun gnus-summary-make-local-variables ()
  "Make all the local summary buffer variables."
  (let ((locals gnus-summary-local-variables)
	global local)
    (while (setq local (pop locals))
      (if (consp local)
	  (progn
	    (if (eq (cdr local) 'global)
		;; Copy the global value of the variable.
		(setq global (symbol-value (car local)))
	      ;; Use the value from the list.
	      (setq global (eval (cdr local))))
	    (make-local-variable (car local))
	    (set (car local) global))
	;; Simple nil-valued local variable.
	(make-local-variable local)
	(set local nil)))))

(defun gnus-summary-make-display-table ()
  ;; Change the display table.	Odd characters have a tendency to mess
  ;; up nicely formatted displays - we make all possible glyphs
  ;; display only a single character.

  ;; We start from the standard display table, if any.
  (setq gnus-summary-display-table
	(or (copy-sequence standard-display-table)
	    (make-display-table)))
  ;; Nix out all the control chars...
  (let ((i 32))
    (while (>= (setq i (1- i)) 0)
      (aset gnus-summary-display-table i [??])))
  ;; ... but not newline and cr, of course. (cr is necessary for the
  ;; selective display).
  (aset gnus-summary-display-table ?\n nil)
  (aset gnus-summary-display-table ?\r nil)
  ;; We nix out any glyphs over 126 that are not set already.
  (let ((i 256))
    (while (>= (setq i (1- i)) 127)
      ;; Only modify if the entry is nil.
      (or (aref gnus-summary-display-table i)
	  (aset gnus-summary-display-table i [??])))))

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

(defmacro gnus-data-level (data)
  `(nth 4 ,data))

(defmacro gnus-data-unread-p (data)
  `(= (nth 1 ,data) gnus-unread-mark))

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
    (or data (error "No such article: %d" after-article))
    (setcdr data (cons (gnus-data-make number mark pos header level)
		       (cdr data)))
    (setq gnus-newsgroup-data-reverse nil)
    (gnus-data-update-list (cddr data) offset)))

(defun gnus-data-enter-list (after-article list &optional offset)
  (when list
    (let ((data (and after-article (gnus-data-find-list after-article)))
	  (ilist list))
      (or data (not after-article) (error "No such article: %d" after-article))
      ;; Find the last element in the list to be spliced into the main
      ;; list.
      (while (cdr list)
	(setq list (cdr list)))
      (if (not data)
	  (progn
	    (setcdr list gnus-newsgroup-data)
	    (setq gnus-newsgroup-data ilist)
	    (and offset (gnus-data-update-list (cdr list) offset)))
	(setcdr list (cdr data))
	(setcdr data ilist)
	(and offset (gnus-data-update-list (cdr data) offset)))
      (setq gnus-newsgroup-data-reverse nil))))

(defun gnus-data-remove (article &optional offset)
  (let ((data gnus-newsgroup-data))
    (if (= (gnus-data-number (car data)) article)
	(setq gnus-newsgroup-data (cdr gnus-newsgroup-data)
	      gnus-newsgroup-data-reverse nil)
      (while (cdr data)
	(and (= (gnus-data-number (cadr data)) article)
	     (progn
	       (setcdr data (cddr data))
	       (and offset (gnus-data-update-list (cdr data) offset))
	       (setq data nil
		     gnus-newsgroup-data-reverse nil)))
	(setq data (cdr data))))))

(defmacro gnus-data-list (backward)
  `(if ,backward
       (or gnus-newsgroup-data-reverse
	   (setq gnus-newsgroup-data-reverse
		 (reverse gnus-newsgroup-data)))
     gnus-newsgroup-data))

(defun gnus-data-update-list (data offset)
  "Add OFFSET to the POS of all data entries in DATA."
  (while data
    (setcar (nthcdr 2 (car data)) (+ offset (nth 2 (car data))))
    (setq data (cdr data))))

(defun gnus-data-compute-positions ()
  "Compute the positions of all articles."
  (let ((data gnus-newsgroup-data)
	pos)
    (while data
      (when (setq pos (text-property-any
		       (point-min) (point-max)
		       'gnus-number (gnus-data-number (car data))))
	(gnus-data-set-pos (car data) (+ pos 3)))
      (setq data (cdr data)))))

(defun gnus-summary-article-pseudo-p (article)
  "Say whether this article is a pseudo article or not."
  (not (vectorp (gnus-data-header (gnus-data-find article)))))

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
  `(gnus-data-header (gnus-data-find
		      ,(or number '(gnus-summary-article-number)))))

(defmacro gnus-summary-thread-level (&optional number)
  `(if (and (eq gnus-summary-make-false-root 'dummy)
	    (get-text-property (point) 'gnus-intangible))
       0
     (gnus-data-level (gnus-data-find
		       ,(or number '(gnus-summary-article-number))))))

(defmacro gnus-summary-article-mark (&optional number)
  `(gnus-data-mark (gnus-data-find
		    ,(or number '(gnus-summary-article-number)))))

(defmacro gnus-summary-article-pos (&optional number)
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
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))))
	 (level (gnus-data-level (car data)))
	 l children)
    (while (and (setq data (cdr data))
		(> (setq l (gnus-data-level (car data))) level))
      (and (= (1+ level) l)
	   (setq children (cons (gnus-data-number (car data))
				children))))
    (nreverse children)))

(defun gnus-summary-article-parent (&optional number)
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))
				    (gnus-data-list t)))
	 (level (gnus-data-level (car data))))
    (if (zerop level)
	() ; This is a root.
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

;; Saving hidden threads.

(put 'gnus-save-hidden-threads 'lisp-indent-function 0)
(put 'gnus-save-hidden-threads 'lisp-indent-hook 0)
(put 'gnus-save-hidden-threads 'edebug-form-spec '(body))

(defmacro gnus-save-hidden-threads (&rest forms)
  "Save hidden threads, eval FORMS, and restore the hidden threads."
  (let ((config (make-symbol "config")))
    `(let ((,config (gnus-hidden-threads-configuration)))
       (unwind-protect
	   (progn
	     ,@forms)
	 (gnus-restore-hidden-threads-configuration ,config)))))

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
  (let (point buffer-read-only)
    (while (setq point (pop config))
      (when (and (< point (point-max))
		 (goto-char point)
		 (= (following-char) ?\n))
	(subst-char-in-region point (1+ point) ?\n ?\r)))))

;; Various summary mode internalish functions.

(defun gnus-mouse-pick-article (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-summary-next-page nil t))

(defun gnus-summary-setup-buffer (group)
  "Initialize summary buffer."
  (let ((buffer (concat "*Summary " group "*")))
    (if (get-buffer buffer)
	(progn
	  (set-buffer buffer)
	  (setq gnus-summary-buffer (current-buffer))
	  (not gnus-newsgroup-prepared))
      ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
      (setq gnus-summary-buffer (set-buffer (get-buffer-create buffer)))
      (gnus-add-current-to-buffer-list)
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
  ;; Set the global equivalents of the summary buffer-local variables
  ;; to the latest values they had.  These reflect the summary buffer
  ;; that was in action when the last article was fetched.
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
	  (score-file gnus-current-score-file))
      (save-excursion
	(set-buffer gnus-group-buffer)
	(setq gnus-newsgroup-name name)
	(setq gnus-newsgroup-marked marked)
	(setq gnus-newsgroup-unreads unread)
	(setq gnus-current-headers headers)
	(setq gnus-newsgroup-data data)
	(setq gnus-article-current gac)
	(setq gnus-summary-buffer summary)
	(setq gnus-article-buffer article-buffer)
	(setq gnus-original-article-buffer original)
	(setq gnus-current-score-file score-file)))))

(defun gnus-summary-last-article-p (&optional article)
  "Return whether ARTICLE is the last article in the buffer."
  (if (not (setq article (or article (gnus-summary-article-number))))
      t ; All non-existant numbers are the last article. :-)
    (not (cdr (gnus-data-find-list article)))))

(defun gnus-summary-insert-dummy-line (gnus-tmp-subject gnus-tmp-number)
  "Insert a dummy root in the summary buffer."
  (beginning-of-line)
  (gnus-add-text-properties
   (point) (progn (eval gnus-summary-dummy-line-format-spec) (point))
   (list 'gnus-number gnus-tmp-number 'gnus-intangible gnus-tmp-number)))

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

(defun gnus-summary-insert-line
  (gnus-tmp-header gnus-tmp-level gnus-tmp-current gnus-tmp-unread
		   gnus-tmp-replied gnus-tmp-expirable gnus-tmp-subject-or-nil
		   &optional gnus-tmp-dummy gnus-tmp-score gnus-tmp-process)
  (let* ((gnus-tmp-indentation (aref gnus-thread-indent-array gnus-tmp-level))
	 (gnus-tmp-lines (mail-header-lines gnus-tmp-header))
	 (gnus-tmp-score (or gnus-tmp-score gnus-summary-default-score 0))
	 (gnus-tmp-score-char
	  (if (or (null gnus-summary-default-score)
		  (<= (abs (- gnus-tmp-score gnus-summary-default-score))
		      gnus-summary-zcore-fuzz)) ? 
	    (if (< gnus-tmp-score gnus-summary-default-score)
		gnus-score-below-mark gnus-score-over-mark)))
	 (gnus-tmp-replied (cond (gnus-tmp-process gnus-process-mark)
				 ((memq gnus-tmp-current gnus-newsgroup-cached)
				  gnus-cached-mark)
				 (gnus-tmp-replied gnus-replied-mark)
				 ((memq gnus-tmp-current gnus-newsgroup-saved)
				  gnus-saved-mark)
				 (t gnus-unread-mark)))
	 (gnus-tmp-from (mail-header-from gnus-tmp-header))
	 (gnus-tmp-name
	  (cond
	   ((string-match "(.+)" gnus-tmp-from)
	    (substring gnus-tmp-from
		       (1+ (match-beginning 0)) (1- (match-end 0))))
	   ((string-match "<[^>]+> *$" gnus-tmp-from)
	    (let ((beg (match-beginning 0)))
	      (or (and (string-match "^\"[^\"]*\"" gnus-tmp-from)
		       (substring gnus-tmp-from (1+ (match-beginning 0))
				  (1- (match-end 0))))
		  (substring gnus-tmp-from 0 beg))))
	   (t gnus-tmp-from)))
	 (gnus-tmp-subject (mail-header-subject gnus-tmp-header))
	 (gnus-tmp-number (mail-header-number gnus-tmp-header))
	 (gnus-tmp-opening-bracket (if gnus-tmp-dummy ?\< ?\[))
	 (gnus-tmp-closing-bracket (if gnus-tmp-dummy ?\> ?\]))
	 (buffer-read-only nil))
    (when (string= gnus-tmp-name "")
      (setq gnus-tmp-name gnus-tmp-from))
    (or (numberp gnus-tmp-lines) (setq gnus-tmp-lines 0))
    (gnus-put-text-property
     (point)
     (progn (eval gnus-summary-line-format-spec) (point))
     'gnus-number gnus-tmp-number)
    (when (gnus-visual-p 'summary-highlight 'highlight)
      (forward-line -1)
      (run-hooks 'gnus-summary-update-hook)
      (forward-line 1))))

(defun gnus-summary-update-line (&optional dont-update)
  ;; Update summary line after change.
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
		     gnus-summary-zcore-fuzz)) ? 
	   (if (< score gnus-summary-default-score)
	       gnus-score-below-mark gnus-score-over-mark)) 'score))
      ;; Do visual highlighting.
      (when (gnus-visual-p 'summary-highlight 'highlight)
	(run-hooks 'gnus-summary-update-hook)))))

(defvar gnus-tmp-new-adopts nil)

(defun gnus-summary-number-of-articles-in-thread (thread &optional level char)
  ;; Sum up all elements (and sub-elements) in a list.
  (let* ((number
	  ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
	  (cond
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
  (let ((params (gnus-info-params (gnus-get-info group)))
	elem)
    (while params
      (setq elem (car params)
	    params (cdr params))
      (and (consp elem)			; Has to be a cons.
	   (consp (cdr elem))		; The cdr has to be a list.
	   (symbolp (car elem))		; Has to be a symbol in there.
	   (not (memq (car elem) 
		      '(quit-config to-address to-list to-group)))
	   (progn			; So we set it.
	     (make-local-variable (car elem))
	     (set (car elem) (eval (nth 1 elem))))))))

(defun gnus-summary-read-group (group &optional show-all no-article
				      kill-buffer no-display)
  "Start reading news in newsgroup GROUP.
If SHOW-ALL is non-nil, already read articles are also listed.
If NO-ARTICLE is non-nil, no article is selected initially.
If NO-DISPLAY, don't generate a summary buffer."
  (gnus-message 5 "Retrieving newsgroup: %s..." group)
  (let* ((new-group (gnus-summary-setup-buffer group))
	 (quit-config (gnus-group-quit-config group))
	 (did-select (and new-group (gnus-select-newsgroup group show-all))))
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
	      (set-buffer gnus-group-buffer)
	      (gnus-group-jump-to-group group)
	      (gnus-group-next-unread-group 1))
	  (if (not (buffer-name (car quit-config)))
	      (gnus-configure-windows 'group 'force)
	    (set-buffer (car quit-config))
	    (and (eq major-mode 'gnus-summary-mode)
		 (gnus-set-global-variables))
	    (gnus-configure-windows (cdr quit-config)))))
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
	(if (not (buffer-name (car quit-config)))
	    (gnus-configure-windows 'group 'force)
	  (set-buffer (car quit-config))
	  (and (eq major-mode 'gnus-summary-mode)
	       (gnus-set-global-variables))
	  (gnus-configure-windows (cdr quit-config))))
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
      (run-hooks 'gnus-select-group-hook)
      ;; Set any local variables in the group parameters.
      (gnus-summary-set-local-parameters gnus-newsgroup-name)
      (gnus-update-format-specifications)
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
	       (gnus-summary-limit-include-expunged))))
      ;; Function `gnus-apply-kill-file' must be called in this hook.
      (run-hooks 'gnus-apply-kill-hook)
      (if (and (zerop (buffer-size))
	       (not no-display))
	  (progn
	    ;; This newsgroup is empty.
	    (gnus-summary-catchup-and-exit nil t) ;Without confirmations.
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
	;; Show first unread article if requested.
	(if (and (not no-article)
		 (not no-display)
		 gnus-newsgroup-unreads
		 gnus-auto-select-first)
	    (unless (if (eq gnus-auto-select-first 'best)
			(gnus-summary-best-unread-article)
		      (gnus-summary-first-unread-article))
	      (gnus-configure-windows 'summary))
	  ;; Don't select any articles, just move point to the first
	  ;; article in the group.
	  (goto-char (point-min))
	  (gnus-summary-position-point)
	  (gnus-set-mode-line 'summary)
	  (gnus-configure-windows 'summary 'force))
	;; If we are in async mode, we send some info to the backend.
	(when gnus-newsgroup-async
	  (gnus-request-asynchronous gnus-newsgroup-name gnus-newsgroup-data))
	(when kill-buffer
	  (gnus-kill-or-deaden-summary kill-buffer))
	(when (get-buffer-window gnus-group-buffer t)
	  ;; Gotta use windows, because recenter does wierd stuff if
	  ;; the current buffer ain't the displayed window.
	  (let ((owin (selected-window)))
	    (select-window (get-buffer-window gnus-group-buffer t))
	    (when (gnus-group-goto-group group)
	      (recenter))
	    (select-window owin))))
      ;; Mark this buffer as "prepared".
      (setq gnus-newsgroup-prepared t)
      t))))

(defun gnus-summary-prepare ()
  "Generate the summary buffer."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (setq gnus-newsgroup-data nil
	  gnus-newsgroup-data-reverse nil)
    (run-hooks 'gnus-summary-generate-hook)
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
    (run-hooks 'gnus-summary-prepare-hook)))

(defun gnus-gather-threads-by-subject (threads)
  "Gather threads by looking at Subject headers."
  (if (not gnus-summary-make-false-root)
      threads
    (let ((hashtb (gnus-make-hashtable 1023))
	  (prev threads)
	  (result threads)
	  subject hthread whole-subject)
      (while threads
	(setq whole-subject (mail-header-subject (caar threads)))
	(setq subject
	      (cond
	       ;; Truncate the subject.
	       ((numberp gnus-summary-gather-subject-limit)
		(setq subject (gnus-simplify-subject-re whole-subject))
		(if (> (length subject) gnus-summary-gather-subject-limit)
		    (substring subject 0 gnus-summary-gather-subject-limit)
		  subject))
	       ;; Fuzzily simplify it.
	       ((eq 'fuzzy gnus-summary-gather-subject-limit)
		(gnus-simplify-subject-fuzzy whole-subject))
	       ;; Just remove the leading "Re:".
	       (t
		(gnus-simplify-subject-re whole-subject))))

	(if (and gnus-summary-gather-exclude-subject
		 (string-match gnus-summary-gather-exclude-subject
			       subject))
	    ()	       	; We don't want to do anything with this article.
	  ;; We simplify the subject before looking it up in the
	  ;; hash table.

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
  (let ((idhashtb (gnus-make-hashtable 1023))
	(thhashtb (gnus-make-hashtable 1023))
	(prev threads)
	(result threads)
	ids references id gthread gid entered)
    (while threads
      (when (setq references (mail-header-references (caar threads)))
	(setq id (mail-header-id (caar threads)))
	(setq ids (gnus-split-references references))
	(setq entered nil)
	(while ids
	  (if (not (setq gid (gnus-gethash (car ids) idhashtb)))
	      (progn
		(gnus-sethash (car ids) id idhashtb)
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
	    (setq threads prev))
	  (setq ids (cdr ids))))
      (setq prev threads)
      (setq threads (cdr threads)))
    result))

(defun gnus-sort-gathered-threads (threads)
  "Sort subtreads inside each gathered thread by article number."
  (let ((result threads))
    (while threads
      (when (stringp (caar threads))
	(setcdr (car threads)
		(sort (cdar threads) 'gnus-thread-sort-by-number)))
      (setq threads (cdr threads)))
    result))

(defun gnus-make-threads ()
  "Go through the dependency hashtb and find the roots.	 Return all threads."
  (let (threads)
    (mapatoms
     (lambda (refs)
       (unless (car (symbol-value refs))
	 ;; These threads do not refer back to any other articles,
	 ;; so they're roots.
	 (setq threads (append (cdr (symbol-value refs)) threads))))
     gnus-newsgroup-dependencies)
    threads))

(defun gnus-build-sparse-threads ()
  (let ((headers gnus-newsgroup-headers)
	(deps gnus-newsgroup-dependencies)
	header references generation relations 
	cthread subject child end pthread relation)
    ;; First we create an alist of generations/relations, where 
    ;; generations is how much we trust the ralation, and the relation
    ;; is parent/child.
    (gnus-message 7 "Making sparse threads...")
    (save-excursion
      (nnheader-set-temp-buffer " *gnus sparse threads*")
      (while (setq header (pop headers))
	(when (and (setq references (mail-header-references header))
		   (not (string= references "")))
	  (insert references)
	  (setq child (mail-header-id header)
		subject (mail-header-subject header))
	  (setq generation 0)
	  (while (search-backward ">" nil t)
	    (setq end (1+ (point)))
	    (when (search-backward "<" nil t)
	      (push (list (incf generation) 
			  child (setq child (buffer-substring (point) end))
			  subject)
		    relations)))
	  (push (list (1+ generation) child nil subject) relations)
	  (erase-buffer)))
      (kill-buffer (current-buffer)))
    ;; Sort over trustworthiness.
    (setq relations (sort relations (lambda (r1 r2) (< (car r1) (car r2)))))
    (while (setq relation (pop relations))
      (when (if (boundp (setq cthread (intern (cadr relation) deps)))
		(unless (car (symbol-value cthread))
		  ;; Make this article the parent of these threads.
		  (setcar (symbol-value cthread)
			  (vector gnus-reffed-article-number 
				  (cadddr relation) 
				  "" ""
				  (cadr relation) 
				  (or (caddr relation) "") 0 0 "")))
	      (set cthread (list (vector gnus-reffed-article-number
					 (cadddr relation) 
					 "" "" (cadr relation) 
					 (or (caddr relation) "") 0 0 ""))))
	(push gnus-reffed-article-number gnus-newsgroup-limit)
	(push gnus-reffed-article-number gnus-newsgroup-sparse)
	(push (cons gnus-reffed-article-number gnus-sparse-mark)
	      gnus-newsgroup-reads)
	(decf gnus-reffed-article-number)
	;; Make this new thread the child of its parent.
	(if (boundp (setq pthread (intern (or (caddr relation) "none") deps)))
	    (setcdr (symbol-value pthread)
		    (nconc (cdr (symbol-value pthread))
			   (list (symbol-value cthread))))
	  (set pthread (list nil (symbol-value cthread))))))
    (gnus-message 7 "Making sparse threads...done")))

(defun gnus-build-old-threads ()
  ;; Look at all the articles that refer back to old articles, and
  ;; fetch the headers for the articles that aren't there.  This will
  ;; build complete threads - if the roots haven't been expired by the
  ;; server, that is.
  (let (id heads)
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
			 (not (car (gnus-gethash
				    id gnus-newsgroup-dependencies)))))
	     (setq heads nil)))))
     gnus-newsgroup-dependencies)))

(defun gnus-build-get-header (id)
  ;; Look through the buffer of NOV lines and find the header to
  ;; ID.  Enter this line into the dependencies hash table, and return
  ;; the id of the parent article (if any).
  (let ((deps gnus-newsgroup-dependencies)
	found header)
    (prog1
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (while (and (not found) (search-forward id nil t))
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
	     (gnus-parent-id (mail-header-references header)))))
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

(defun gnus-summary-update-article (article &optional iheader)
  "Update ARTICLE in the summary buffer."
  (set-buffer gnus-summary-buffer)
  (let* ((header (or iheader (gnus-summary-article-header article)))
	 (id (mail-header-id header))
	 (data (gnus-data-find article))
	 (thread (gnus-id-to-thread id))
	 (references (mail-header-references header))
	 (parent
	  (gnus-id-to-thread
	   (or (gnus-parent-id 
		(if (and references
			 (not (equal "" references)))
		    references))
	       "none")))
	 (buffer-read-only nil)
	 (old (car thread))
	 (number (mail-header-number header))
	 pos)
    (when thread
      ;; !!! Should this be in or not?
      (unless iheader
	(setcar thread nil))
      (when parent
	(delq thread parent))
      (if (gnus-summary-insert-subject id header iheader)
	  ;; Set the (possibly) new article number in the data structure.
	  (gnus-data-set-number data (gnus-id-to-article id))
	(setcar thread old)
	nil))))

(defun gnus-rebuild-thread (id)
  "Rebuild the thread containing ID."
  (let ((buffer-read-only nil)
	current thread data)
    (if (not gnus-show-threads)
	(setq thread (list (car (gnus-id-to-thread id))))
      ;; Get the thread this article is part of.
      (setq thread (gnus-remove-thread id)))
    (setq current (save-excursion
		    (and (zerop (forward-line -1))
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
      (let (gnus-newsgroup-data gnus-newsgroup-threads)
	(gnus-summary-prepare-threads (gnus-cut-threads (list thread)))
	(setq data (nreverse gnus-newsgroup-data))
	(setq threads gnus-newsgroup-threads))
      ;; We splice the new data into the data structure.
      (gnus-data-enter-list current data)
      (gnus-data-compute-positions)
      (setq gnus-newsgroup-threads (nconc threads gnus-newsgroup-threads)))))

(defun gnus-number-to-header (number)
  "Return the header for article NUMBER."
  (let ((headers gnus-newsgroup-headers))
    (while (and headers
		(not (= number (mail-header-number (car headers)))))
      (pop headers))
    (when headers
      (car headers))))

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
    (while (and id (setq prev (car (gnus-gethash 
				    id gnus-newsgroup-dependencies))))
      (setq last-id id
	    id (gnus-parent-id (mail-header-references prev))))
    last-id))

(defun gnus-remove-thread (id &optional dont-remove)
  "Remove the thread that has ID in it."
  (let ((dep gnus-newsgroup-dependencies)
	headers thread last-id)
    ;; First go up in this thread until we find the root.
    (setq last-id (gnus-root-id id))
    (setq headers (list (car (gnus-id-to-thread last-id))
			(caadr (gnus-id-to-thread last-id))))
    ;; We have now found the real root of this thread.	It might have
    ;; been gathered into some loose thread, so we have to search
    ;; through the threads to find the thread we wanted.
    (let ((threads gnus-newsgroup-threads)
	  sub)
      (while threads
	(setq sub (car threads))
	(if (stringp (car sub))
	    ;; This is a gathered threads, so we look at the roots
	    ;; below it to find whether this article in in this
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
	(setq thread (gnus-gethash last-id dep)))
      (when thread
	(prog1
	    thread ; We return this thread.
	  (unless dont-remove
	    (if (stringp (car thread))
		(progn
		  ;; If we use dummy roots, then we have to remove the
		  ;; dummy root as well.
		  (when (eq gnus-summary-make-false-root 'dummy)
		    ;; Uhm.
		    )
		  (setq thread (cdr thread))
		  (while thread
		    (gnus-remove-thread-1 (car thread))
		    (setq thread (cdr thread))))
	      (gnus-remove-thread-1 thread))))))))

(defun gnus-remove-thread-1 (thread)
  "Remove the thread THREAD recursively."
  (let ((number (mail-header-number (car thread)))
	pos)
    (when (setq pos (text-property-any
		     (point-min) (point-max) 'gnus-number number))
      (goto-char pos)
      (gnus-delete-line)
      (gnus-data-remove number))
    (setq thread (cdr thread))
    (while thread
      (gnus-remove-thread-1 (pop thread)))))

(defun gnus-sort-threads (threads)
  "Sort THREADS."
  (if (not gnus-thread-sort-functions)
      threads
    (let ((func (if (= 1 (length gnus-thread-sort-functions))
		    (car gnus-thread-sort-functions)
		  `(lambda (t1 t2)
		     ,(gnus-make-sort-function 
		       (reverse gnus-thread-sort-functions))))))
      (gnus-message 7 "Sorting threads...")
      (prog1
	  (sort threads func)
	(gnus-message 7 "Sorting threads...done")))))

(defun gnus-sort-articles (articles)
  "Sort ARTICLES."
  (when gnus-article-sort-functions
    (let ((func (if (= 1 (length gnus-article-sort-functions))
		    (car gnus-article-sort-functions)
		  `(lambda (t1 t2)
		     ,(gnus-make-sort-function 
		       (reverse gnus-article-sort-functions))))))
      (gnus-message 7 "Sorting articles...")
      (prog1
	  (setq gnus-newsgroup-headers (sort articles func))
	(gnus-message 7 "Sorting articles...done")))))

(defun gnus-make-sort-function (funs)
  "Return a composite sort condition based on the functions in FUNC."
  (if (cdr funs)
      `(or (,(car funs) t1 t2)
	   (and (not (,(car funs) t2 t1))
		,(gnus-make-sort-function (cdr funs))))
    `(,(car funs) t1 t2)))
		 
;; Written by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(defmacro gnus-thread-header (thread)
  ;; Return header of first article in THREAD.
  ;; Note that THREAD must never, ever be anything else than a variable -
  ;; using some other form will lead to serious barfage.
  (or (symbolp thread) (signal 'wrong-type-argument '(symbolp thread)))
  ;; (8% speedup to gnus-summary-prepare, just for fun :-)
  (list 'byte-code "\10\211:\203\17\0\211@;\203\16\0A@@\207" ;
	(vector thread) 2))

(defsubst gnus-article-sort-by-number (h1 h2)
  "Sort articles by article number."
  (< (mail-header-number h1)
     (mail-header-number h2)))

(defun gnus-thread-sort-by-number (h1 h2)
  "Sort threads by root article number."
  (gnus-article-sort-by-number
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-author (h1 h2)
  "Sort articles by root author."
  (string-lessp
   (let ((extract (funcall
		   gnus-extract-address-components
		   (mail-header-from h1))))
     (or (car extract) (cdr extract)))
   (let ((extract (funcall
		   gnus-extract-address-components
		   (mail-header-from h2))))
     (or (car extract) (cdr extract)))))

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
  (string-lessp
   (inline (gnus-sortable-date (mail-header-date h1)))
   (inline (gnus-sortable-date (mail-header-date h2)))))

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
  ;;  This function find the total score of THREAD.
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
		      (cdr (gnus-gethash (mail-header-id root)
					 gnus-newsgroup-dependencies)))
		 (if (> (mail-header-number root) 0)
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
		 (not (setq gnus-tmp-dummy-line nil))
		 (or (not stack)
		     (= (caar stack) 0))
		 (not gnus-tmp-false-parent)
		 (or gnus-tmp-new-adopts new-roots))
	    (if gnus-tmp-new-adopts
		(setq gnus-tmp-level (if gnus-tmp-root-expunged 0 1)
		      thread (list (car gnus-tmp-new-adopts))
		      gnus-tmp-header (caar thread)
		      gnus-tmp-new-adopts (cdr gnus-tmp-new-adopts))
	      (if new-roots
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
	   ((and (not (memq number gnus-newsgroup-limit))
		 ;(not gnus-tmp-dummy-line)
		 )
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
		 (not (memq number gnus-newsgroup-sparse))
		 (not (memq number gnus-newsgroup-ancient)))
	    (setq gnus-newsgroup-unreads
		  (delq number gnus-newsgroup-unreads))
	    (if gnus-newsgroup-auto-expire
		(push number gnus-newsgroup-expirable)
	      (push (cons number gnus-low-score-mark)
		    gnus-newsgroup-reads))))

	  (when gnus-tmp-header
	    ;; We may have an old dummy line to output before this
	    ;; article.
	    (when gnus-tmp-dummy-line
	      (gnus-summary-insert-dummy-line
	       gnus-tmp-dummy-line (mail-header-number gnus-tmp-header))
	      (setq gnus-tmp-dummy-line nil))

	    ;; Compute the mark.
	    (setq
	     gnus-tmp-unread
	     (cond
	      ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
	      ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
	      ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
	      ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
	      (t (or (cdr (assq number gnus-newsgroup-reads))
		     gnus-ancient-mark))))

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
			 gnus-summary-zcore-fuzz)) ? 
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
	      ((string-match "(.+)" gnus-tmp-from)
	       (substring gnus-tmp-from
			  (1+ (match-beginning 0)) (1- (match-end 0))))
	      ((string-match "<[^>]+> *$" gnus-tmp-from)
	       (setq beg-match (match-beginning 0))
	       (or (and (string-match "^\"[^\"]*\"" gnus-tmp-from)
			(substring gnus-tmp-from (1+ (match-beginning 0))
				   (1- (match-end 0))))
		   (substring gnus-tmp-from 0 beg-match)))
	      (t gnus-tmp-from)))
	    (when (string= gnus-tmp-name "")
	      (setq gnus-tmp-name gnus-tmp-from))
	    (or (numberp gnus-tmp-lines) (setq gnus-tmp-lines 0))
	    (gnus-put-text-property
	     (point)
	     (progn (eval gnus-summary-line-format-spec) (point))
	     'gnus-number number)
	    (when gnus-visual-p
	      (forward-line -1)
	      (run-hooks 'gnus-summary-update-hook)
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
		   (not (memq number gnus-newsgroup-ancient)))
	  (setq gnus-newsgroup-unreads
		(delq number gnus-newsgroup-unreads))
	  (if gnus-newsgroup-auto-expire
	      (push number gnus-newsgroup-expirable)
	    (push (cons number gnus-low-score-mark)
		  gnus-newsgroup-reads)))

	(setq mark
	      (cond
	       ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
	       ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
	       ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
	       ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
	       (t (or (cdr (assq number gnus-newsgroup-reads))
		      gnus-ancient-mark))))
	(setq gnus-newsgroup-data
	      (cons (gnus-data-make number mark (1+ (point)) header 0)
		    gnus-newsgroup-data))
	(gnus-summary-insert-line
	 header 0 nil mark (memq number gnus-newsgroup-replied)
	 (memq number gnus-newsgroup-expirable)
	 (mail-header-subject header) nil
	 (cdr (assq number gnus-newsgroup-scored))
	 (memq number gnus-newsgroup-processable))))))

(defun gnus-select-newsgroup (group &optional read-all)
  "Select newsgroup GROUP.
If READ-ALL is non-nil, all articles in the group are selected."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 articles fetched-articles cached)

    (or (gnus-check-server
	 (setq gnus-current-select-method (gnus-find-method-for-group group)))
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

    (and gnus-asynchronous
	 (gnus-check-backend-function
	  'request-asynchronous gnus-newsgroup-name)
	 (setq gnus-newsgroup-async
	       (gnus-request-asynchronous gnus-newsgroup-name)))

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

    (setq articles (gnus-articles-to-read group read-all))

    (cond
     ((null articles)
      ;;(gnus-message 3 "Couldn't select newsgroup -- no articles to display")
      'quit)
     ((eq articles 0) nil)
     (t
      ;; Init the dependencies hash table.
      (setq gnus-newsgroup-dependencies
	    (gnus-make-hashtable (length articles)))
      ;; Retrieve the headers and read them in.
      (gnus-message 5 "Fetching headers for %s..." gnus-newsgroup-name)
      (setq gnus-newsgroup-headers
	    (if (eq 'nov
		    (setq gnus-headers-retrieved-by
			  (gnus-retrieve-headers
			   articles gnus-newsgroup-name
			   ;; We might want to fetch old headers, but
			   ;; not if there is only 1 article.
			   (and gnus-fetch-old-headers
				(or (and
				     (not (eq gnus-fetch-old-headers 'some))
				     (not (numberp gnus-fetch-old-headers)))
				    (> (length articles) 1))))))
		(gnus-get-newsgroup-headers-xover articles)
	      (gnus-get-newsgroup-headers)))
      (gnus-message 5 "Fetching headers for %s...done" gnus-newsgroup-name)

      ;; Kludge to avoid having cached articles nixed out in virtual groups.
      (when cached
	(setq gnus-newsgroup-cached cached))

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
      (and gnus-fetch-old-headers
	   (eq gnus-headers-retrieved-by 'nov)
	   (gnus-build-old-threads))
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
      (setq gnus-reffed-article-number -1)
      ;; GROUP is successfully selected.
      (or gnus-newsgroup-headers t)))))

(defun gnus-articles-to-read (group read-all)
  ;; Find out what articles the user wants to read.
  (let* ((articles
	  ;; Select all articles if `read-all' is non-nil, or if there
	  ;; are no unread articles.
	  (if (or read-all
		  (and (zerop (length gnus-newsgroup-marked))
		       (zerop (length gnus-newsgroup-unreads))))
	      (gnus-uncompress-range (gnus-active group))
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
			   gnus-newsgroup-name number))))
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
	      (quit nil))))))
    (setq select (if (stringp select) (string-to-number select) select))
    (if (or (null select) (zerop select))
	select
      (if (and (not (zerop scored)) (<= (abs select) scored))
	  (progn
	    (setq articles (sort scored-list '<))
	    (setq number (length articles)))
	(setq articles (copy-sequence articles)))

      (if (< (abs select) number)
	  (if (< select 0)
	      ;; Select the N oldest articles.
	      (setcdr (nthcdr (1- (abs select)) articles) nil)
	    ;; Select the N most recent articles.
	    (setq articles (nthcdr (- number select) articles))))
      (setq gnus-newsgroup-unselected
	    (gnus-sorted-intersection
	     gnus-newsgroup-unreads
	     (gnus-sorted-complement gnus-newsgroup-unreads articles)))
      articles)))

(defun gnus-killed-articles (killed articles)
  (let (out)
    (while articles
      (if (inline (gnus-member-of-range (car articles) killed))
	  (setq out (cons (car articles) out)))
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
  "Set all article lists and remove all marks that are no longer legal."
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
       ((memq mark '(tick dormant expirable reply save))
	(while articles
	  (when (or (< (setq article (pop articles)) min) (> article max))
	    (set var (delq article (symbol-value var))))))
       ;; Adjust assocs.
       ((memq mark uncompressed)
	(while articles
	  (when (or (not (consp (setq article (pop articles))))
		    (< (car article) min)
		    (> (car article) max))
	    (set var (delq article (symbol-value var))))))))))

(defun gnus-update-missing-marks (missing)
  "Go through the list of MISSING articles and remove them mark lists."
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
	type list newmarked symbol)
    (when info
      ;; Add all marks lists that are non-nil to the list of marks lists.
      (while types
	(setq type (pop types))
	(when (setq list (symbol-value
			  (setq symbol
				(intern (format "gnus-newsgroup-%s"
						(car type))))))
	  (push (cons (cdr type)
		      (if (memq (cdr type) uncompressed) list
			(gnus-compress-sequence 
			 (set symbol (sort list '<)) t)))
		newmarked)))

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

(defun gnus-add-marked-articles (group type articles &optional info force)
  ;; Add ARTICLES of TYPE to the info of GROUP.
  ;; If INFO is non-nil, use that info.	 If FORCE is non-nil, don't
  ;; add, but replace marked articles of TYPE with ARTICLES.
  (let ((info (or info (gnus-get-info group)))
	(uncompressed '(score bookmark killed))
	marked m)
    (or (not info)
	(and (not (setq marked (nthcdr 3 info)))
	     (or (null articles)
		 (setcdr (nthcdr 2 info)
			 (list (list (cons type (gnus-compress-sequence
						 articles t)))))))
	(and (not (setq m (assq type (car marked))))
	     (or (null articles)
		 (setcar marked
			 (cons (cons type (gnus-compress-sequence articles t) )
			       (car marked)))))
	(if force
	    (if (null articles)
		(setcar (nthcdr 3 info)
			(delq (assq type (car marked)) (car marked)))
	      (setcdr m (gnus-compress-sequence articles t)))
	  (setcdr m (gnus-compress-sequence
		     (sort (nconc (gnus-uncompress-range (cdr m))
				  (copy-sequence articles)) '<) t))))))

(defun gnus-set-mode-line (where)
  "This function sets the mode line of the article or summary buffers.
If WHERE is `summary', the summary mode line format will be used."
  ;; Is this mode line one we keep updated?
  (when (memq where gnus-updated-mode-lines)
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
	       (gnus-tmp-group-name gnus-newsgroup-name)
	       (gnus-tmp-article-number (or gnus-current-article 0))
	       (gnus-tmp-unread gnus-newsgroup-unreads)
	       (gnus-tmp-unread-and-unticked (length gnus-newsgroup-unreads))
	       (gnus-tmp-unselected (length gnus-newsgroup-unselected))
	       (gnus-tmp-unread-and-unselected
		(cond ((and (zerop gnus-tmp-unread-and-unticked)
			    (zerop gnus-tmp-unselected)) "")
		      ((zerop gnus-tmp-unselected)
		       (format "{%d more}" gnus-tmp-unread-and-unticked))
		      (t (format "{%d(+%d) more}"
				 gnus-tmp-unread-and-unticked
				 gnus-tmp-unselected))))
	       (gnus-tmp-subject
		(if (and gnus-current-headers
			 (vectorp gnus-current-headers))
		    (gnus-mode-string-quote
		     (mail-header-subject gnus-current-headers)) ""))
	       max-len
	       gnus-tmp-header);; passed as argument to any user-format-funcs
	  (setq mode-string (eval mformat))
	  (setq max-len (max 4 (if gnus-mode-non-string-length
				   (- (window-width)
				      gnus-mode-non-string-length)
				 (length mode-string))))
	  ;; We might have to chop a bit of the string off...
	  (when (> (length mode-string) max-len)
	    (setq mode-string
		  (concat (gnus-truncate-string mode-string (- max-len 3))
			  "...")))
	  ;; Pad the mode string a bit.
	  (setq mode-string (format (format "%%-%ds" max-len) mode-string))))
      ;; Update the mode line.
      (setq mode-line-buffer-identification 
	    (gnus-mode-line-buffer-identification
	     (list mode-string)))
      (set-buffer-modified-p t))))

(defun gnus-create-xref-hashtb (from-newsgroup headers unreads)
  "Go through the HEADERS list and add all Xrefs to a hash table.
The resulting hash table is returned, or nil if no Xrefs were found."
  (let* ((virtual (gnus-virtual-group-p from-newsgroup))
	 (prefix (if virtual "" (gnus-group-real-prefix from-newsgroup)))
	 (xref-hashtb (make-vector 63 0))
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
		    (if (stringp (setq nth4 (gnus-info-method info)))
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

(defun gnus-group-make-articles-read (group articles)
  (let* ((num 0)
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (active (gnus-active group))
	 range)
    ;; First peel off all illegal article numbers.
    (if active
	(let ((ids articles)
	      id first)
	  (while ids
	    (setq id (car ids))
	    (if (and first (> id (cdr active)))
		(progn
		  ;; We'll end up in this situation in one particular
		  ;; obscure situation.	 If you re-scan a group and get
		  ;; a new article that is cross-posted to a different
		  ;; group that has not been re-scanned, you might get
		  ;; crossposted article that has a higher number than
		  ;; Gnus believes possible.  So we re-activate this
		  ;; group as well.  This might mean doing the
		  ;; crossposting thingy will *increase* the number
		  ;; of articles in some groups.  Tsk, tsk.
		  (setq active (or (gnus-activate-group group) active))))
	    (if (or (> id (cdr active))
		    (< id (car active)))
		(setq articles (delq id articles)))
	    (setq ids (cdr ids)))))
    ;; If the read list is nil, we init it.
    (and active
	 (null (gnus-info-read info))
	 (> (car active) 1)
	 (gnus-info-set-read info (cons 1 (1- (car active)))))
    ;; Then we add the read articles to the range.
    (gnus-info-set-read
     info
     (setq range
	   (gnus-add-to-range
	    (gnus-info-read info) (setq articles (sort articles '<)))))
    ;; Then we have to re-compute how many unread
    ;; articles there are in this group.
    (if active
	(progn
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

(defun gnus-methods-equal-p (m1 m2)
  (let ((m1 (or m1 gnus-select-method))
	(m2 (or m2 gnus-select-method)))
    (or (equal m1 m2)
	(and (eq (car m1) (car m2))
	     (or (not (memq 'address (assoc (symbol-name (car m1))
					    gnus-valid-select-methods)))
		 (equal (nth 1 m1) (nth 1 m2)))))))

(defsubst gnus-header-value ()
  (buffer-substring (match-end 0) (gnus-point-at-eol)))

(defvar gnus-newsgroup-none-id 0)

(defun gnus-get-newsgroup-headers (&optional dependencies force-new)
  (let ((cur nntp-server-buffer)
	(dependencies
	 (or dependencies
	     (save-excursion (set-buffer gnus-summary-buffer)
			     gnus-newsgroup-dependencies)))
	headers id id-dep ref-dep end ref)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (run-hooks 'gnus-parse-headers-hook)
      (let ((case-fold-search t)
	    in-reply-to header p lines)
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
		  (gnus-header-value) "(none)"))
	    ;; From.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nfrom: " nil t)
		  (gnus-header-value) "(nobody)"))
	    ;; Date.
	    (progn
	      (goto-char p)
	      (if (search-forward "\ndate: " nil t)
		  (gnus-header-value) ""))
	    ;; Message-ID.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nmessage-id: " nil t)
		  (setq id (gnus-header-value))
		;; If there was no message-id, we just fake one to make
		;; subsequent routines simpler.
		(setq id (concat "none+"
				 (int-to-string
				  (setq gnus-newsgroup-none-id
					(1+ gnus-newsgroup-none-id)))))))
	    ;; References.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nreferences: " nil t)
		  (progn
		    (setq end (point))
		    (prog1
			(gnus-header-value)
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
			 (setq in-reply-to (gnus-header-value))
			 (string-match "<[^>]+>" in-reply-to))
		    (setq ref (substring in-reply-to (match-beginning 0)
					 (match-end 0)))
		  (setq ref ""))))
	    ;; Chars.
	    0
	    ;; Lines.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nlines: " nil t)
		  (if (numberp (setq lines (read cur)))
		      lines 0)
		0))
	    ;; Xref.
	    (progn
	      (goto-char p)
	      (and (search-forward "\nxref: " nil t)
		   (gnus-header-value)))))
	  ;; We do the threading while we read the headers.  The
	  ;; message-id and the last reference are both entered into
	  ;; the same hash table.  Some tippy-toeing around has to be
	  ;; done in case an article has arrived before the article
	  ;; which it refers to.
	  (if (boundp (setq id-dep (intern id dependencies)))
	      (if (and (car (symbol-value id-dep))
		       (not force-new))
		  ;; An article with this Message-ID has already
		  ;; been seen, so we ignore this one, except we add
		  ;; any additional Xrefs (in case the two articles
		  ;; came from different servers).
		  (progn
		    (mail-header-set-xref
		     (car (symbol-value id-dep))
		     (concat (or (mail-header-xref
				  (car (symbol-value id-dep))) "")
			     (or (mail-header-xref header) "")))
		    (setq header nil))
		(setcar (symbol-value id-dep) header))
	    (set id-dep (list header)))
	  (when header
	    (if (boundp (setq ref-dep (intern ref dependencies)))
		(setcdr (symbol-value ref-dep)
			(nconc (cdr (symbol-value ref-dep))
			       (list (symbol-value id-dep))))
	      (set ref-dep (list nil (symbol-value id-dep))))
	    (setq headers (cons header headers)))
	  (goto-char (point-max))
	  (widen))
	(nreverse headers)))))

;; The following macros and functions were written by Felix Lee
;; <flee@cse.psu.edu>.

(defmacro gnus-nov-read-integer ()
  '(prog1
       (if (= (following-char) ?\t)
	   0
	 (let ((num (condition-case nil (read buffer) (error nil))))
	   (if (numberp num) num 0)))
     (or (eobp) (forward-char 1))))

(defmacro gnus-nov-skip-field ()
  '(search-forward "\t" eol 'move))

(defmacro gnus-nov-field ()
  '(buffer-substring (point) (if (gnus-nov-skip-field) (1- (point)) eol)))

;; Goes through the xover lines and returns a list of vectors
(defun gnus-get-newsgroup-headers-xover (sequence &optional 
						  force-new dependencies)
  "Parse the news overview data in the server buffer, and return a
list of headers that match SEQUENCE (see `nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((cur nntp-server-buffer)
	(dependencies (or dependencies gnus-newsgroup-dependencies))
	number headers header)
    (save-excursion
      (set-buffer nntp-server-buffer)
      ;; Allow the user to mangle the headers before parsing them.
      (run-hooks 'gnus-parse-headers-hook)
      (goto-char (point-min))
      (while (and sequence (not (eobp)))
	(setq number (read cur))
	(while (and sequence (< (car sequence) number))
	  (setq sequence (cdr sequence)))
	(and sequence
	     (eq number (car sequence))
	     (progn
	       (setq sequence (cdr sequence))
	       (if (setq header
			 (inline (gnus-nov-parse-line
				  number dependencies force-new)))
		   (setq headers (cons header headers)))))
	(forward-line 1))
      (setq headers (nreverse headers)))
    headers))

;; This function has to be called with point after the article number
;; on the beginning of the line.
(defun gnus-nov-parse-line (number dependencies &optional force-new)
  (let ((none 0)
	(eol (gnus-point-at-eol))
	(buffer (current-buffer))
	header ref id id-dep ref-dep)

    ;; overview: [num subject from date id refs chars lines misc]
    (narrow-to-region (point) eol)
    (or (eobp) (forward-char))

    (condition-case nil
	(setq header
	      (vector
	       number			; number
	       (gnus-nov-field)		; subject
	       (gnus-nov-field)		; from
	       (gnus-nov-field)		; date
	       (setq id (or (gnus-nov-field)
			    (concat "none+"
				    (int-to-string
				     (setq none (1+ none)))))) ; id
	       (progn
		 (save-excursion
		   (let ((beg (point)))
		     (search-forward "\t" eol)
		     (if (search-backward ">" beg t)
			 (setq ref
			       (buffer-substring
				(1+ (point))
				(search-backward "<" beg t)))
		       (setq ref nil))))
		 (gnus-nov-field))	; refs
	       (gnus-nov-read-integer)	; chars
	       (gnus-nov-read-integer)	; lines
	       (if (= (following-char) ?\n)
		   nil
		 (gnus-nov-field))	; misc
	       ))
      (error (progn
	       (gnus-error 4 "Strange nov line")
	       (setq header nil)
	       (goto-char eol))))

    (widen)

    ;; We build the thread tree.
    (when header
      (if (boundp (setq id-dep (intern id dependencies)))
	  (if (and (car (symbol-value id-dep))
		   (not force-new))
	      ;; An article with this Message-ID has already been seen,
	      ;; so we ignore this one, except we add any additional
	      ;; Xrefs (in case the two articles came from different
	      ;; servers.
	      (progn
		(mail-header-set-xref
		 (car (symbol-value id-dep))
		 (concat (or (mail-header-xref
			      (car (symbol-value id-dep))) "")
			 (or (mail-header-xref header) "")))
		(setq header nil))
	    (setcar (symbol-value id-dep) header))
	(set id-dep (list header))))
    (when header
      (if (boundp (setq ref-dep (intern (or ref "none") dependencies)))
	  (setcdr (symbol-value ref-dep)
		  (nconc (cdr (symbol-value ref-dep))
			 (list (symbol-value id-dep))))
	(set ref-dep (list nil (symbol-value id-dep)))))
    header))

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
	    (if (or (and (eq (downcase (following-char)) ?x)
			 (looking-at "Xref:"))
		    (search-forward "\nXref:" nil t))
		(progn
		  (goto-char (1+ (match-end 0)))
		  (setq xref (buffer-substring (point)
					       (progn (end-of-line) (point))))
		  (mail-header-set-xref headers xref))))))))

(defun gnus-summary-insert-subject (id &optional old-header use-old-header)
  "Find article ID and insert the summary line for that article."
  (let ((header (if (and old-header use-old-header)
		    old-header (gnus-read-header id)))
	(number (and (numberp id) id))
	pos)
    (when header
      ;; Rebuild the thread that this article is part of and go to the
      ;; article we have fetched.
      (when (and (not gnus-show-threads)
		 old-header)
	(when (setq pos (text-property-any
			 (point-min) (point-max) 'gnus-number 
			 (mail-header-number old-header)))
	  (goto-char pos)
	  (gnus-delete-line)
	  (gnus-data-remove (mail-header-number old-header))))
      (when old-header
	(mail-header-set-number header (mail-header-number old-header)))
      (setq gnus-newsgroup-sparse
	    (delq (setq number (mail-header-number header)) 
		  gnus-newsgroup-sparse))
      (setq gnus-newsgroup-ancient (delq number gnus-newsgroup-ancient))
      (gnus-rebuild-thread (mail-header-id header))
      (gnus-summary-goto-subject number nil t))
    (when (and (numberp number)
	       (> number 0))
      ;; We have to update the boundaries even if we can't fetch the
      ;; article if ID is a number -- so that the next `P' or `N'
      ;; command will fetch the previous (or next) article even
      ;; if the one we tried to fetch this time has been canceled.
      (and (> number gnus-newsgroup-end)
	   (setq gnus-newsgroup-end number))
      (and (< number gnus-newsgroup-begin)
	   (setq gnus-newsgroup-begin number))
      (setq gnus-newsgroup-unselected
	    (delq number gnus-newsgroup-unselected)))
    ;; Report back a success?
    (and header (mail-header-number header))))

(defun gnus-summary-work-articles (n)
  "Return a list of articles to be worked upon.	 The prefix argument,
the list of process marked articles, and the current article will be
taken into consideration."
  (cond
   (n
    ;; A numerical prefix has been given.
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
   ((and (boundp 'transient-mark-mode)
	 transient-mark-mode
	 mark-active)
    ;; Work on the region between point and mark.
    (let ((max (max (point) (mark)))
	  articles article)
      (save-excursion
	(goto-char (min (point) (mark)))
	(while
	    (and
	     (push (setq article (gnus-summary-article-number)) articles)
	     (gnus-summary-find-next nil article)
	     (< (point) max)))
	(nreverse articles))))
   (gnus-newsgroup-processable
    ;; There are process-marked articles present.
    (reverse gnus-newsgroup-processable))
   (t
    ;; Just return the current article.
    (list (gnus-summary-article-number)))))

(defun gnus-summary-search-group (&optional backward use-level)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (if (gnus-group-search-forward
	 backward nil (if use-level (gnus-group-group-level) nil))
	(gnus-group-group-name))))

(defun gnus-summary-best-group (&optional exclude-group)
  "Find the name of the best unread group.
If EXCLUDE-GROUP, do not go to this group."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (save-excursion
      (gnus-group-best-unread-group exclude-group))))

(defun gnus-summary-find-next (&optional unread article backward)
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
			  (when (gnus-data-unread-p (car arts))
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
    (if (setq result
	      (if unread
		  (progn
		    (while arts
		      (and (gnus-data-unread-p (car arts))
			   (setq result (car arts)
				 arts nil))
		      (setq arts (cdr arts)))
		    result)
		(car arts)))
	(progn
	  (goto-char (gnus-data-pos result))
	  (gnus-data-number result)))))

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
  (let* ((top (cond ((< (window-height) 4) 0)
		    ((< (window-height) 7) 1)
		    (t 2)))
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
       (set-window-start
	window (min bottom (save-excursion 
			     (forward-line (- top)) (point)))))
      ;; Do horizontal recentering while we're at it.
      (when (and (get-buffer-window (current-buffer) t)
		 (not (eq gnus-auto-center-summary 'vertical)))
	(let ((selected (selected-window)))
	  (select-window (get-buffer-window (current-buffer) t))
	  (gnus-summary-position-point)
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-horizontal-recenter ()
  "Recenter the current buffer horizontally."
  (if (< (current-column) (/ (window-width) 2))
      (set-window-hscroll (get-buffer-window (current-buffer) t) 0)
    (let* ((orig (point))
	   (end (window-end (get-buffer-window (current-buffer) t)))
	   (max 0))
      ;; Find the longest line currently displayed in the window.
      (goto-char (window-start))
      (while (and (not (eobp)) 
		  (< (point) end))
	(end-of-line)
	(setq max (max max (current-column)))
	(forward-line 1))
      (goto-char orig)
      ;; Scroll horizontally to center (sort of) the point.
      (if (> max (window-width))
	  (set-window-hscroll 
	   (get-buffer-window (current-buffer) t)
	   (min (- (current-column) (/ (window-width) 3))
		(+ 2 (- max (window-width)))))
	(set-window-hscroll (get-buffer-window (current-buffer) t) 0))
      max)))

;; Function written by Stainless Steel Rat <ratinox@ccs.neu.edu>.
(defun gnus-short-group-name (group &optional levels)
  "Collapse GROUP name LEVELS."
  (let* ((name "") 
	 (foreign "")
	 (depth 0) 
	 (skip 1)
	 (levels (or levels
		     (progn
		       (while (string-match "\\." group skip)
			 (setq skip (match-end 0)
			       depth (+ depth 1)))
		       depth))))
    (if (string-match ":" group)
	(setq foreign (substring group 0 (match-end 0))
	      group (substring group (match-end 0))))
    (while group
      (if (and (string-match "\\." group)
	       (> levels (- gnus-group-uncollapsed-levels 1)))
	  (setq name (concat name (substring group 0 1))
		group (substring group (match-end 0))
		levels (- levels 1)
		name (concat name "."))
	(setq name (concat foreign name group)
	      group nil)))
    name))

(defun gnus-summary-jump-to-group (newsgroup)
  "Move point to NEWSGROUP in group mode buffer."
  ;; Keep update point of group mode buffer if visible.
  (if (eq (current-buffer) (get-buffer gnus-group-buffer))
      (save-window-excursion
	;; Take care of tree window mode.
	(if (get-buffer-window gnus-group-buffer)
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
	 (active (gnus-active group))
	 (last (cdr active))
	 first nlast unread)
    ;; If none are read, then all are unread.
    (if (not read)
	(setq first (car active))
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article.  Sounds logical, doesn't it?
      (if (not (listp (cdr read)))
	  (setq first (1+ (cdr read)))
	;; `read' is a list of ranges.
	(if (/= (setq nlast (or (and (numberp (car read)) (car read))
				(caar read))) 1)
	    (setq first 1))
	(while read
	  (if first
	      (while (< first nlast)
		(setq unread (cons first unread))
		(setq first (1+ first))))
	  (setq first (1+ (if (atom (car read)) (car read) (cdar read))))
	  (setq nlast (if (atom (cadr read)) (cadr read) (caadr read)))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (while (<= first last)
      (setq unread (cons first unread))
      (setq first (1+ first)))
    ;; Return the list of unread articles.
    (nreverse unread)))

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

(defun gnus-summary-universal-argument (arg)
  "Perform any operation on all articles that are process/prefixed."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles arg))
	func article)
    (if (eq
	 (setq
	  func
	  (key-binding
	   (read-key-sequence
	    (substitute-command-keys
	     "\\<gnus-summary-mode-map>\\[gnus-summary-universal-argument]"
	     ))))
	 'undefined)
	(gnus-error 1 "Undefined key")
      (save-excursion
	(while articles
	  (gnus-summary-goto-subject (setq article (pop articles)))
	  (command-execute func)
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
  (gnus-set-global-variables)
  (when (gnus-ephemeral-group-p gnus-newsgroup-name)
    (error "Ephemeral groups can't be reselected"))
  (let ((current-subject (gnus-summary-article-number))
	(group gnus-newsgroup-name))
    (setq gnus-newsgroup-begin nil)
    (gnus-summary-exit)
    ;; We have to adjust the point of group mode buffer because the
    ;; current point was moved to the next unread newsgroup by
    ;; exiting.
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

(defun gnus-summary-update-info ()
  (let* ((group gnus-newsgroup-name))
    (when gnus-newsgroup-kill-headers
      (setq gnus-newsgroup-killed
	    (gnus-compress-sequence
	     (nconc
	      (gnus-set-sorted-intersection
	       (gnus-uncompress-range gnus-newsgroup-killed)
	       (setq gnus-newsgroup-unselected
		     (sort gnus-newsgroup-unselected '<)))
	      (setq gnus-newsgroup-unreads
		    (sort gnus-newsgroup-unreads '<))) t)))
    (unless (listp (cdr gnus-newsgroup-killed))
      (setq gnus-newsgroup-killed (list gnus-newsgroup-killed)))
    (let ((headers gnus-newsgroup-headers))
      (run-hooks 'gnus-exit-group-hook)
      (unless gnus-save-score
	(setq gnus-newsgroup-scored nil))
      ;; Set the new ranges of read articles.
      (gnus-update-read-articles
       group (append gnus-newsgroup-unreads gnus-newsgroup-unselected))
      ;; Set the current article marks.
      (gnus-update-marks)
      ;; Do the cross-ref thing.
      (when gnus-use-cross-reference
	(gnus-mark-xrefs-as-read group headers gnus-newsgroup-unreads))
      ;; Do adaptive scoring, and possibly save score files.
      (when gnus-newsgroup-adaptive
	(gnus-score-adaptive))
      (when gnus-use-scoring
	(gnus-score-save))
      ;; Do not switch windows but change the buffer to work.
      (set-buffer gnus-group-buffer)
      (or (gnus-ephemeral-group-p gnus-newsgroup-name)
	  (gnus-group-update-group group)))))

(defun gnus-summary-exit (&optional temporary)
  "Exit reading current newsgroup, and then return to group selection mode.
gnus-exit-group-hook is called with no arguments if that value is non-nil."
  (interactive)
  (gnus-set-global-variables)
  (gnus-kill-save-kill-buffer)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config gnus-newsgroup-name))
	 (mode major-mode)
	 (buf (current-buffer)))
    (run-hooks 'gnus-summary-prepare-exit-hook)
    ;; If we have several article buffers, we kill them at exit.
    (unless gnus-single-article-buffer
      (gnus-kill-buffer gnus-original-article-buffer)
      (setq gnus-article-current nil))
    (when gnus-use-cache
      (gnus-cache-possibly-remove-articles)
      (gnus-cache-save-buffers))
    (when gnus-use-trees
      (gnus-tree-close group))
    ;; Make all changes in this group permanent.
    (unless quit-config
      (gnus-summary-update-info))
    (gnus-close-group group)
    ;; Make sure where I was, and go to next newsgroup.
    (set-buffer gnus-group-buffer)
    (unless quit-config
      (gnus-group-jump-to-group group))
    (run-hooks 'gnus-summary-exit-hook)
    (unless quit-config
      (gnus-group-next-unread-group 1))
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
	(gnus-configure-windows 'group 'force)
	(gnus-summary-clear-local-variables)
	;; Return to group mode buffer.
	(if (eq mode 'gnus-summary-mode)
	    (gnus-kill-buffer buf)))
      (setq gnus-current-select-method gnus-select-method)
      (pop-to-buffer gnus-group-buffer)
      ;; Clear the current group name.
      (if (not quit-config)
	  (progn
	    (gnus-group-jump-to-group group)
	    (gnus-group-next-unread-group 1)
	    (gnus-configure-windows 'group 'force))
	(if (not (buffer-name (car quit-config)))
	    (gnus-configure-windows 'group 'force)
	  (set-buffer (car quit-config))
	  (and (eq major-mode 'gnus-summary-mode)
	       (gnus-set-global-variables))
	  (gnus-configure-windows (cdr quit-config))))
      (unless quit-config
	(setq gnus-newsgroup-name nil)))))

(defalias 'gnus-summary-quit 'gnus-summary-exit-no-update)
(defun gnus-summary-exit-no-update (&optional no-questions)
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (gnus-set-global-variables)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config group)))
    (when (or no-questions
	      gnus-expert-user
	      (gnus-y-or-n-p "Do you really wanna quit reading this group? "))
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
      (when (get-buffer gnus-article-buffer)
	(bury-buffer gnus-article-buffer))
      ;; Return to the group buffer.
      (gnus-configure-windows 'group 'force)
      ;; Clear the current group name.
      (setq gnus-newsgroup-name nil)
      (when (equal (gnus-group-group-name) group)
	(gnus-group-next-unread-group 1))
      (when quit-config
	(if (not (buffer-name (car quit-config)))
	    (gnus-configure-windows 'group 'force)
	  (set-buffer (car quit-config))
	  (when (eq major-mode 'gnus-summary-mode)
	    (gnus-set-global-variables))
	  (gnus-configure-windows (cdr quit-config)))))))

;;; Dead summaries.

(defvar gnus-dead-summary-mode-map nil)

(if gnus-dead-summary-mode-map
    nil
  (setq gnus-dead-summary-mode-map (make-keymap))
  (suppress-keymap gnus-dead-summary-mode-map)
  (substitute-key-definition
   'undefined 'gnus-summary-wake-up-the-dead gnus-dead-summary-mode-map)
  (let ((keys '("\C-d" "\r" "\177")))
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
      (unless (assq 'gnus-dead-summary-mode minor-mode-alist)
	(push '(gnus-dead-summary-mode " Dead") minor-mode-alist))
      (unless (assq 'gnus-dead-summary-mode minor-mode-map-alist)
	(push (cons 'gnus-dead-summary-mode gnus-dead-summary-mode-map)
	      minor-mode-map-alist)))))

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
	       (substring name (match-beginning 0))) t))))

(defun gnus-kill-or-deaden-summary (buffer)
  "Kill or deaden the summary BUFFER."
  (when (and (buffer-name buffer)
	     (not gnus-single-article-buffer))
    (save-excursion
      (set-buffer buffer)
      (gnus-kill-buffer gnus-article-buffer)
      (gnus-kill-buffer gnus-original-article-buffer)))
  (cond (gnus-kill-summary-on-exit
	 (when (and gnus-use-trees
		    (and (get-buffer buffer)
			 (buffer-name (get-buffer buffer))))
	   (save-excursion
	     (set-buffer (get-buffer buffer))
	     (gnus-tree-close gnus-newsgroup-name)))
	 (gnus-kill-buffer buffer))
	((and (get-buffer buffer)
	      (buffer-name (get-buffer buffer)))
	 (save-excursion
	   (set-buffer buffer)
	   (gnus-deaden-summary)))))

(defun gnus-summary-wake-up-the-dead (&rest args)
  "Wake up the dead summary buffer."
  (interactive)
  (gnus-dead-summary-mode -1)
  (let ((name (buffer-name)))
    (when (string-match "Dead " name)
      (rename-buffer
       (concat (substring name 0 (match-beginning 0))
	       (substring name (match-end 0))) t)))
  (gnus-message 3 "This dead summary is now alive again"))

;; Suggested by Andrew Eskilsson <pi92ae@pt.hk-r.se>.
(defun gnus-summary-fetch-faq (&optional faq-dir)
  "Fetch the FAQ for the current group.
If FAQ-DIR (the prefix), prompt for a directory to search for the faq
in."
  (interactive
   (list
    (if current-prefix-arg
	(completing-read
	 "Faq dir: " (and (listp gnus-group-faq-directory)
			  gnus-group-faq-directory)))))
  (let (gnus-faq-buffer)
    (and (setq gnus-faq-buffer
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
  (gnus-message 6
		(substitute-command-keys "\\<gnus-summary-mode-map>\\[gnus-summary-next-page]:Select  \\[gnus-summary-next-unread-article]:Forward  \\[gnus-summary-prev-unread-article]:Backward  \\[gnus-summary-exit]:Exit  \\[gnus-info-find-node]:Run Info	 \\[gnus-summary-describe-briefly]:This help")))

;; Walking around group mode buffer from summary mode.

(defun gnus-summary-next-group (&optional no-article target-group backward)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected
initially.  If NEXT-GROUP, go to this group.  If BACKWARD, go to
previous group instead."
  (interactive "P")
  (gnus-set-global-variables)
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
	    (set-buffer current-buffer)
	    (gnus-summary-exit))
	;; We try to enter the target group.
	(gnus-group-jump-to-group target-group)
	(let ((unreads (gnus-group-group-unread)))
	  (if (and (or (eq t unreads)
		       (and unreads (not (zerop unreads))))
		   (gnus-summary-read-group
		    target-group nil no-article current-buffer))
	      (setq entered t)
	    (setq current-group target-group
		  target-group nil)))))))

(defun gnus-summary-prev-group (&optional no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  (gnus-summary-next-group no-article nil t))

;; Walking around summary lines.

(defun gnus-summary-first-subject (&optional unread)
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
		      (not (gnus-data-unread-p (car data))))
	    (setq data (cdr data)))
	  (if data
	      (progn
		(goto-char (gnus-data-pos (car data)))
		(gnus-data-number (car data)))))))
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
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more%s articles"
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
  (let ((b (point))
	(data (gnus-data-find article)))
    ;; We read in the article if we have to.
    (and (not data)
	 force
	 (gnus-summary-insert-subject article (and (vectorp force) force) t)
	 (setq data (gnus-data-find article)))
    (goto-char b)
    (if (not data)
	(progn
	  (unless silent
	    (gnus-message 3 "Can't find article %d" article))
	  nil)
      (goto-char (gnus-data-pos data))
      article)))

;; Walking around summary lines with displaying articles.

(defun gnus-summary-expand-window (&optional arg)
  "Make the summary buffer take up the entire Emacs frame.
Given a prefix, will force an `article' buffer configuration."
  (interactive "P")
  (gnus-set-global-variables)
  (if arg
      (gnus-configure-windows 'article 'force)
    (gnus-configure-windows 'summary 'force)))

(defun gnus-summary-display-article (article &optional all-header)
  "Display ARTICLE in article buffer."
  (gnus-set-global-variables)
  (if (null article)
      nil
    (prog1
	(if gnus-summary-display-article-function
	    (funcall gnus-summary-display-article-function article all-header)
	  (gnus-article-prepare article all-header))
      (run-hooks 'gnus-select-article-hook)
      (unless (zerop gnus-current-article)
	(gnus-summary-goto-subject gnus-current-article))
      (gnus-summary-recenter)
      (when gnus-use-trees
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
	(all-headers (not (not all-headers))) ;Must be T or NIL.
	gnus-summary-display-article-function
	did)
    (and (not pseudo)
	 (gnus-summary-article-pseudo-p article)
	 (error "This is a pseudo-article."))
    (prog1
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
	      (prog1
		  (gnus-summary-display-article article all-headers)
		(setq did article))
	    (if (or all-headers gnus-show-all-headers)
		(gnus-article-show-all-headers))
	    'old))
      (if did
	  (gnus-article-set-window-start
	   (cdr (assq article gnus-newsgroup-bookmarks)))))))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Obsolete function."
  nil)

(defun gnus-summary-next-article (&optional unread subject backward push)
  "Select the next article.
If UNREAD, only unread articles are selected.
If SUBJECT, only articles with SUBJECT are selected.
If BACKWARD, the previous article is selected instead of the next."
  (interactive "P")
  (gnus-set-global-variables)
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
     nil t))
   ;; Go to next/previous group.
   (t
    (or (gnus-ephemeral-group-p gnus-newsgroup-name)
	(gnus-summary-jump-to-group gnus-newsgroup-name))
    (let ((cmd last-command-char)
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
	(gnus-summary-walk-group-buffer
	 gnus-newsgroup-name cmd unread backward)))))))

(defun gnus-summary-walk-group-buffer (from-group cmd unread backward)
  (let ((keystrokes '((?\C-n (gnus-group-next-unread-group 1))
		      (?\C-p (gnus-group-prev-unread-group 1))))
	keve key group ended)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-summary-jump-to-group from-group)
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
	  (and group
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

(defun gnus-read-event-char ()
  "Get the next event."
  (let ((event (read-event)))
    (cons (and (numberp event) event) event)))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article t (and gnus-auto-select-same
				    (gnus-summary-article-subject))))

(defun gnus-summary-prev-article (&optional unread subject)
  "Select the article after the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (gnus-summary-next-article unread subject t))

(defun gnus-summary-prev-unread-article ()
  "Select unred article before current one."
  (interactive)
  (gnus-summary-prev-article t (and gnus-auto-select-same
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
	(endp nil))
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
	(gnus-eval-in-buffer-window gnus-article-buffer
	  (setq endp (gnus-article-next-page lines)))
	(if endp
	    (cond (circular
		   (gnus-summary-beginning-of-article))
		  (lines
		   (gnus-message 3 "End of message"))
		  ((null lines)
		   (if (and (eq gnus-summary-goto-unread 'never)
			    (not (gnus-summary-last-article-p article)))
		       (gnus-summary-next-article)
		     (gnus-summary-next-unread-article)))))))
    (gnus-summary-recenter)
    (gnus-summary-position-point)))

(defun gnus-summary-prev-page (&optional lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number)))
    (gnus-configure-windows 'article)
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-summary-recenter)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(gnus-article-prev-page lines))))
  (gnus-summary-position-point))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-configure-windows 'article)
  (gnus-summary-show-thread)
  (when (eq (gnus-summary-select-article nil nil 'pseudo) 'old)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (cond ((> lines 0)
	     (if (gnus-article-next-page lines)
		 (gnus-message 3 "End of message")))
	    ((< lines 0)
	     (gnus-article-prev-page (- lines))))))
  (gnus-summary-recenter)
  (gnus-summary-position-point))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-next-article nil (gnus-summary-article-subject)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-prev-article nil (gnus-summary-article-subject)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-next-article t (gnus-summary-article-subject)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-prev-article t (gnus-summary-article-subject)))

(defun gnus-summary-first-unread-article ()
  "Select the first unread article.
Return nil if there are no unread articles."
  (interactive)
  (gnus-set-global-variables)
  (prog1
      (if (gnus-summary-first-subject t)
	  (progn
	    (gnus-summary-show-thread)
	    (gnus-summary-first-subject t)
	    (gnus-summary-display-article (gnus-summary-article-number))))
    (gnus-summary-position-point)))

(defun gnus-summary-best-unread-article ()
  "Select the unread article with the highest score."
  (interactive)
  (gnus-set-global-variables)
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
  "Fetch ARTICLE and display it if it exists.
If ALL-HEADERS is non-nil, no header lines are hidden."
  (interactive
   (list
    (string-to-int
     (completing-read
      "Article number: "
      (mapcar (lambda (number) (list (int-to-string number)))
	      gnus-newsgroup-limit)))
    current-prefix-arg
    t))
  (prog1
      (if (gnus-summary-goto-subject article force)
	  (gnus-summary-display-article article all-headers)
	(gnus-message 4 "Couldn't go to article %s" article) nil)
    (gnus-summary-position-point)))

(defun gnus-summary-goto-last-article ()
  "Go to the previously read article."
  (interactive)
  (prog1
      (and gnus-last-article
	   (gnus-summary-goto-article gnus-last-article))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-article (number)
  "Pop one article off the history and go to the previous.
NUMBER articles will be popped off."
  (interactive "p")
  (let (to)
    (setq gnus-newsgroup-history
	  (cdr (setq to (nthcdr number gnus-newsgroup-history))))
    (if to
	(gnus-summary-goto-article (car to))
      (error "Article history empty")))
  (gnus-summary-position-point))

;; Summary commands and functions for limiting the summary buffer.

(defun gnus-summary-limit-to-articles (n)
  "Limit the summary buffer to the next N articles.
If not given a prefix, use the process marked articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (prog1
      (let ((articles (gnus-summary-work-articles n)))
	(setq gnus-newsgroup-processable nil)
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-limit (&optional total)
  "Restore the previous limit.
If given a prefix, remove all limits."
  (interactive "P")
  (gnus-set-global-variables)
  (when total 
    (setq gnus-newsgroup-limits
	  (list (mapcar (lambda (h) (mail-header-number h))
			gnus-newsgroup-headers))))
  (unless gnus-newsgroup-limits
    (error "No limit to pop"))
  (prog1
      (gnus-summary-limit nil 'pop)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-subject (subject &optional header)
  "Limit the summary buffer to articles that have subjects that match a regexp."
  (interactive "sRegexp: ")
  (unless header
    (setq header "subject"))
  (when (not (equal "" subject))
    (prog1
	(let ((articles (gnus-summary-find-matching
			 (or header "subject") subject 'all)))
	  (or articles (error "Found no matches for \"%s\"" subject))
	  (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-to-author (from)
  "Limit the summary buffer to articles that have authors that match a regexp."
  (interactive "sRegexp: ")
  (gnus-summary-limit-to-subject from "from"))

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
	   gnus-canceled-mark gnus-catchup-mark gnus-sparse-mark)
     'reverse)))

(defalias 'gnus-summary-delete-marked-with 'gnus-summary-limit-to-marks)
(make-obsolete 'gnus-summary-delete-marked-with 'gnus-summary-limit-to-marks)

(defun gnus-summary-limit-to-marks (marks &optional reverse)
  "Limit the summary buffer to articles that are marked with MARKS (e.g. \"DK\").
If REVERSE, limit the summary buffer to articles that are not marked
with MARKS.  MARKS can either be a string of marks or a list of marks.
Returns how many articles were removed."
  (interactive "sMarks: ")
  (gnus-set-global-variables)
  (prog1
      (let ((data gnus-newsgroup-data)
	    (marks (if (listp marks) marks
		     (append marks nil))) ; Transform to list.
	    articles)
	(while data
	  (and (if reverse (not (memq (gnus-data-mark (car data)) marks))
		 (memq (gnus-data-mark (car data)) marks))
	       (setq articles (cons (gnus-data-number (car data)) articles)))
	  (setq data (cdr data)))
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-score (&optional score)
  "Limit to articles with score at or above SCORE."
  (interactive "P")
  (gnus-set-global-variables)
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

(defun gnus-summary-limit-include-dormant ()
  "Display all the hidden articles that are marked as dormant."
  (interactive)
  (gnus-set-global-variables)
  (or gnus-newsgroup-dormant
      (error "There are no dormant articles in this group"))
  (prog1
      (gnus-summary-limit (append gnus-newsgroup-dormant gnus-newsgroup-limit))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-dormant ()
  "Hide all dormant articles."
  (interactive)
  (gnus-set-global-variables)
  (prog1
      (gnus-summary-limit-to-marks (list gnus-dormant-mark) 'reverse)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-childless-dormant ()
  "Hide all dormant articles that have no children."
  (interactive)
  (gnus-set-global-variables)
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
    (setq gnus-newsgroup-unreads nil)
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
    (setq gnus-newsgroup-limits
	  (cons gnus-newsgroup-limit gnus-newsgroup-limits)))
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
    (if data
	;; We try to find some article after the current one.
	(while data
	  (and (gnus-summary-goto-subject
		(gnus-data-number (car data)) nil t)
	       (setq data nil
		     found t))
	  (setq data (cdr data))))
    (or found
	;; If there is no data, that means that we were after the last
	;; article.  The same goes when we can't find any articles
	;; after the current one.
	(progn
	  (goto-char (point-max))
	  (gnus-summary-find-prev)))
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
	    (eq gnus-build-sparse-threads 'some)
	    (eq gnus-build-sparse-threads 'more))
    ;; Deal with old-fetched headers and sparse threads.
    (while (and
	    thread
	    (or
	     (memq (mail-header-number (car thread)) gnus-newsgroup-sparse)
	     (memq (mail-header-number (car thread)) gnus-newsgroup-ancient))
	    (or (<= (length (cdr thread)) 1)
		(gnus-invisible-cut-children (cdr thread))))
      (setq thread (cadr thread))))
  thread)

(defun gnus-cut-threads (threads)
  "Cut off all uninteresting articles from the beginning of threads."
  (when (or (eq gnus-fetch-old-headers 'some)
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
	       (null gnus-summary-expunge-below)
	       (not (eq gnus-build-sparse-threads 'some))
	       (not (eq gnus-build-sparse-threads 'more))
	       (null gnus-thread-expunge-below)
	       (not gnus-use-nocem)))
      () ; Do nothing.
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
      (if (or
	   ;; If this article is dormant and has absolutely no visible
	   ;; children, then this article isn't visible.
	   (and (memq number gnus-newsgroup-dormant)
		(= children 0))
	   ;; If this is "fetch-old-headered" and there is only one
	   ;; visible child (or less), then we don't want this article.
	   (and (eq gnus-fetch-old-headers 'some)
		(memq number gnus-newsgroup-ancient)
		(zerop children))
	   ;; If this is a sparsely inserted article with no children,
	   ;; we don't want it.
	   (and (eq gnus-build-sparse-threads 'some)
		(memq number gnus-newsgroup-sparse)
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
	   (and gnus-use-nocem
		(gnus-nocem-unwanted-article-p (mail-header-id (car thread)))))
	  ;; Nope, invisible article.
	  0
	;; Ok, this article is to be visible, so we add it to the limit
	;; and return 1.
	(setq gnus-newsgroup-limit (cons number gnus-newsgroup-limit))
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
The difference between N and the number of articles fetched is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (while
      (and
       (> n 0)
       (let* ((header (gnus-summary-article-header))
	      (ref
	       ;; If we try to find the parent of the currently
	       ;; displayed article, then we take a look at the actual
	       ;; References header, since this is slightly more
	       ;; reliable than the References field we got from the
	       ;; server.
	       (if (and (eq (mail-header-number header)
			    (cdr gnus-article-current))
			(equal gnus-newsgroup-name
			       (car gnus-article-current)))
		   (save-excursion
		     (set-buffer gnus-original-article-buffer)
		     (nnheader-narrow-to-headers)
		     (prog1
			 (message-fetch-field "references")
		       (widen)))
		 ;; It's not the current article, so we take a bet on
		 ;; the value we got from the server.
		 (mail-header-references header))))
	 (if (setq ref (or ref (mail-header-references header)))
	     (or (gnus-summary-refer-article (gnus-parent-id ref))
		 (gnus-message 1 "Couldn't find parent"))
	   (gnus-message 1 "No references in article %d"
			 (gnus-summary-article-number))
	   nil)))
    (setq n (1- n)))
  (gnus-summary-position-point)
  n)

(defun gnus-summary-refer-references ()
  "Fetch all articles mentioned in the References header.
Return how many articles were fetched."
  (interactive)
  (gnus-set-global-variables)
  (let ((ref (mail-header-references (gnus-summary-article-header)))
	(current (gnus-summary-article-number))
	(n 0))
    ;; For each Message-ID in the References header...
    (while (string-match "<[^>]*>" ref)
      (incf n)
      ;; ... fetch that article.
      (gnus-summary-refer-article
       (prog1 (match-string 0 ref)
	 (setq ref (substring ref (match-end 0))))))
    (gnus-summary-goto-subject current)
    (gnus-summary-position-point)
    n))

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
			(memq (mail-header-number header)
			      gnus-newsgroup-sparse))))
      (if header
	  (prog1
	      ;; The article is present in the buffer, to we just go to it.
	      (gnus-summary-goto-article 
	       (mail-header-number header) nil header)
	    (when sparse
	      (gnus-summary-update-article (mail-header-number header))))
	;; We fetch the article
	(let ((gnus-override-method 
	       (and (gnus-news-group-p gnus-newsgroup-name)
		    gnus-refer-article-method))
	      number)
	  ;; Start the special refer-article method, if necessary.
	  (when (and gnus-refer-article-method
		     (gnus-news-group-p gnus-newsgroup-name))
	    (gnus-check-server gnus-refer-article-method))
	  ;; Fetch the header, and display the article.
	  (if (setq number (gnus-summary-insert-subject message-id))
	      (gnus-summary-select-article nil nil nil number)
	    (gnus-message 3 "Couldn't fetch article %s" message-id)))))))

(defun gnus-summary-enter-digest-group (&optional force)
  "Enter a digest group based on the current article."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (let ((name (format "%s-%d"
		      (gnus-group-prefixed-name
		       gnus-newsgroup-name (list 'nndoc ""))
		      gnus-current-article))
	(ogroup gnus-newsgroup-name)
	(case-fold-search t)
	(buf (current-buffer))
	dig)
    (save-excursion
      (setq dig (nnheader-set-temp-buffer " *gnus digest buffer*"))
      (insert-buffer-substring gnus-original-article-buffer)
      (narrow-to-region
       (goto-char (point-min))
       (or (search-forward "\n\n" nil t) (point)))
      (goto-char (point-min))
      (delete-matching-lines "^\\(Path\\):\\|^From ")
      (widen))
    (unwind-protect
	(if (gnus-group-read-ephemeral-group
	     name `(nndoc ,name (nndoc-address
				 ,(get-buffer dig))
			  (nndoc-article-type ,(if force 'digest 'guess))) t)
	    ;; Make all postings to this group go to the parent group.
	    (nconc (gnus-info-params (gnus-get-info name))
		   (list (cons 'to-group ogroup)))
	  ;; Couldn't select this doc group.
	  (switch-to-buffer buf)
	  (gnus-set-global-variables)
	  (gnus-configure-windows 'summary)
	  (gnus-message 3 "Article couldn't be entered?"))
      (kill-buffer dig))))

(defun gnus-summary-isearch-article (&optional regexp-p)
  "Do incremental search forward on the current article.
If REGEXP-P (the prefix) is non-nil, do regexp isearch."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (goto-char (point-min))
    (isearch-forward regexp-p)))

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
  (gnus-set-global-variables)
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (unless (gnus-summary-search-article regexp backward)
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
  (let ((gnus-select-article-hook nil)	;Disable hook.
	(gnus-article-display-hook nil)
	(gnus-mark-article-hook nil)	;Inhibit marking as read.
	(re-search
	 (if backward
	     're-search-backward 're-search-forward))
	(sum (current-buffer))
	(found nil))
    (gnus-save-hidden-threads
      (gnus-summary-select-article)
      (set-buffer gnus-article-buffer)
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
	      (set-buffer sum))
	  ;; We didn't find it, so we go to the next article.
	  (set-buffer sum)
	  (if (not (if backward (gnus-summary-find-prev)
		     (gnus-summary-find-next)))
	      ;; No more articles.
	      (setq found t)
	    ;; Select the next article and adjust point.
	    (gnus-summary-select-article)
	    (set-buffer gnus-article-buffer)
	    (widen)
	    (goto-char (if backward (point-max) (point-min))))))
      (gnus-message 7 ""))
    ;; Return whether we found the regexp.
    (when (eq found 'found)
      (gnus-summary-show-thread)
      (gnus-summary-goto-subject gnus-current-article)
      (gnus-summary-position-point)
      t)))

(defun gnus-summary-find-matching (header regexp &optional backward unread
					  not-case-fold)
  "Return a list of all articles that match REGEXP on HEADER.
The search stars on the current article and goes forwards unless
BACKWARD is non-nil.  If BACKWARD is `all', do all articles.
If UNREAD is non-nil, only unread articles will
be taken into consideration.  If NOT-CASE-FOLD, case won't be folded
in the comparisons."
  (let ((data (if (eq backward 'all) gnus-newsgroup-data
		(gnus-data-find-list
		 (gnus-summary-article-number) (gnus-data-list backward))))
	(func `(lambda (h) (,(intern (concat "mail-header-" header)) h)))
	(case-fold-search (not not-case-fold))
	articles d)
    (or (fboundp (intern (concat "mail-header-" header)))
	(error "%s is not a valid header" header))
    (while data
      (setq d (car data))
      (and (or (not unread)		; We want all articles...
	       (gnus-data-unread-p d))	; Or just unreads.
	   (vectorp (gnus-data-header d)) ; It's not a pseudo.
	   (string-match regexp (funcall func (gnus-data-header d))) ; Match.
	   (setq articles (cons (gnus-data-number d) articles))) ; Success!
      (setq data (cdr data)))
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
  (gnus-set-global-variables)
  ;; Hidden thread subtrees must be searched as well.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (gnus-message 6 "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute header regexp
		    `(lambda () (call-interactively ',(key-binding command)))
		    backward)
      (gnus-message 6 "Executing %s...done" (key-description command)))))

(defun gnus-summary-beginning-of-article ()
  "Scroll the article back to the beginning."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-min))
    (and gnus-break-pages (gnus-narrow-to-page))))

(defun gnus-summary-end-of-article ()
  "Scroll to the end of the article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-max))
    (recenter -3)
    (and gnus-break-pages (gnus-narrow-to-page))))

(defun gnus-summary-show-article (&optional arg)
  "Force re-fetching of the current article.
If ARG (the prefix) is non-nil, show the raw article without any
article massaging functions being run."
  (interactive "P")
  (gnus-set-global-variables)
  (if (not arg)
      ;; Select the article the normal way.
      (gnus-summary-select-article nil 'force)
    ;; Bind the article treatment functions to nil.
    (let ((gnus-have-all-headers t)
	  gnus-article-display-hook
	  gnus-article-prepare-hook
	  gnus-break-pages
	  gnus-visual)
      (gnus-summary-select-article nil 'force)))
  (gnus-summary-goto-subject gnus-current-article)
;  (gnus-configure-windows 'article)
  (gnus-summary-position-point))

(defun gnus-summary-verbose-headers (&optional arg)
  "Toggle permanent full header display.
If ARG is a positive number, turn header display on.
If ARG is a negative number, turn header display off."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-toggle-header arg)
  (setq gnus-show-all-headers
	(cond ((or (not (numberp arg))
		   (zerop arg))
	       (not gnus-show-all-headers))
	      ((natnump arg)
	       t))))

(defun gnus-summary-toggle-header (&optional arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (gnus-set-global-variables)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let* ((buffer-read-only nil)
	   (inhibit-point-motion-hooks t)
	   (hidden (text-property-any
		    (goto-char (point-min)) (search-forward "\n\n")
		    'invisible t))
	   e)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(delete-region (point-min) (1- (point))))
      (goto-char (point-min))
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(goto-char (point-min))
	(setq e (1- (or (search-forward "\n\n" nil t) (point-max)))))
      (insert-buffer-substring gnus-original-article-buffer 1 e)
      (let ((gnus-inhibit-hiding t))
	(run-hooks 'gnus-article-display-hook))
      (if (or (not hidden) (and (numberp arg) (< arg 0)))
	  (gnus-article-hide-headers)))))

(defun gnus-summary-show-all-headers ()
  "Make all header lines visible."
  (interactive)
  (gnus-set-global-variables)
  (gnus-article-show-all-headers))

(defun gnus-summary-toggle-mime (&optional arg)
  "Toggle MIME processing.
If ARG is a positive number, turn MIME processing on."
  (interactive "P")
  (gnus-set-global-variables)
  (setq gnus-show-mime
	(if (null arg) (not gnus-show-mime)
	  (> (prefix-numeric-value arg) 0)))
  (gnus-summary-select-article t 'force))

(defun gnus-summary-caesar-message (&optional arg)
  "Caesar rotate the current article by 13.
The numerical prefix specifies how manu places to rotate each letter
forward."
  (interactive "P")
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)))

(defun gnus-summary-move-article (&optional n to-newsgroup select-method action)
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
and `request-accept' functions."
  (interactive "P")
  (unless action (setq action 'move))
  (gnus-set-global-variables)
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
	(prefix (gnus-group-real-prefix gnus-newsgroup-name))
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
			(gnus-group-name-to-method to-newsgroup)))
    ;; Check the method we are to move this article to...
    (or (gnus-check-backend-function 'request-accept-article (car to-method))
	(error "%s does not support article copying" (car to-method)))
    (or (gnus-check-server to-method)
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
	 (gnus-request-move-article
	  article			; Article to move
	  gnus-newsgroup-name		; From newsgrouo
	  (nth 1 (gnus-find-method-for-group
		  gnus-newsgroup-name)) ; Server
	  (list 'gnus-request-accept-article
		to-newsgroup (list 'quote select-method)
		(not articles))		; Accept form
	  (not articles)))		; Only save nov last time
	;; Copy the article.
	((eq action 'copy)
	 (save-excursion
	   (set-buffer copy-buf)
	   (gnus-request-article-this-buffer article gnus-newsgroup-name)
	   (gnus-request-accept-article
	    to-newsgroup select-method (not articles))))
	;; Crosspost the article.
	((eq action 'crosspost)
	 (let ((xref (mail-header-xref (gnus-summary-article-header article))))
	   (setq new-xref (concat gnus-newsgroup-name ":" article))
	   (if (and xref (not (string= xref "")))
	       (progn
		 (when (string-match "^Xref: " xref)
		   (setq xref (substring xref (match-end 0))))
		 (setq new-xref (concat xref " " new-xref)))
	     (setq new-xref (concat (system-name) " " new-xref)))
	   (save-excursion
	     (set-buffer copy-buf)
	     (gnus-request-article-this-buffer article gnus-newsgroup-name)
	     (nnheader-replace-header "xref" new-xref)
	     (gnus-request-accept-article
	      to-newsgroup select-method (not articles)))))))
      (if (not art-group)
	  (gnus-message 1 "Couldn't %s article %s"
			(cadr (assq action names)) article)
	(let* ((entry
		(or
		 (gnus-gethash (car art-group) gnus-newsrc-hashtb)
		 (gnus-gethash
		  (gnus-group-prefixed-name
		   (car art-group)
		   (or select-method 
		       (gnus-find-method-for-group to-newsgroup)))
		  gnus-newsrc-hashtb)))
	       (info (nth 2 entry))
	       (to-group (gnus-info-group info)))
	  ;; Update the group that has been moved to.
	  (when (and info
		     (memq action '(move copy)))
	    (unless (member to-group to-groups)
	      (push to-group to-groups))

	    (unless (memq article gnus-newsgroup-unreads)
	      (gnus-info-set-read
	       info (gnus-add-to-range (gnus-info-read info)
				       (list (cdr art-group)))))

	    ;; Copy any marks over to the new group.
	    (let ((marks gnus-article-mark-lists)
		  (to-article (cdr art-group)))

	      ;; See whether the article is to be put in the cache.
	      (when gnus-use-cache
		(gnus-cache-possibly-enter-article
		 to-group to-article
		 (let ((header (copy-sequence
				(gnus-summary-article-header article))))
		   (mail-header-set-number header to-article)
		   header)
		 (memq article gnus-newsgroup-marked)
		 (memq article gnus-newsgroup-dormant)
		 (memq article gnus-newsgroup-unreads)))

	      (while marks
		(when (memq article (symbol-value
				     (intern (format "gnus-newsgroup-%s"
						     (caar marks)))))
		  ;; If the other group is the same as this group,
		  ;; then we have to add the mark to the list.
		  (when (equal to-group gnus-newsgroup-name)
		    (set (intern (format "gnus-newsgroup-%s" (caar marks)))
			 (cons to-article
			       (symbol-value
				(intern (format "gnus-newsgroup-%s"
						(caar marks)))))))
		  ;; Copy mark to other group.
		  (gnus-add-marked-articles
		   to-group (cdar marks) (list to-article) info))
		(setq marks (cdr marks)))))

	  ;; Update the Xref header in this article to point to
	  ;; the new crossposted article we have just created.
	  (when (eq action 'crosspost)
	    (save-excursion
	      (set-buffer copy-buf)
	      (gnus-request-article-this-buffer article gnus-newsgroup-name)
	      (nnheader-replace-header
	       "xref" (concat new-xref " " (gnus-group-prefixed-name
					    (car art-group) to-method)
			      ":" (cdr art-group)))
	      (gnus-request-replace-article
	       article gnus-newsgroup-name (current-buffer)))))

	(gnus-summary-goto-subject article)
	(when (eq action 'move)
	  (gnus-summary-mark-article article gnus-canceled-mark)))
      (gnus-summary-remove-process-mark article))
    ;; Re-activate all groups that have been moved to.
    (while to-groups
      (gnus-activate-group (pop to-groups)))
    
    (gnus-kill-buffer copy-buf)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)))

(defun gnus-summary-copy-article (&optional n to-newsgroup select-method)
  "Move the current article to a different newsgroup.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to.
If SELECT-METHOD is non-nil, do not move to a specific newsgroup, but
re-spool using this method."
  (interactive "P")
  (gnus-summary-move-article n nil select-method 'copy))

(defun gnus-summary-crosspost-article (&optional n)
  "Crosspost the current article to some other group."
  (interactive "P")
  (gnus-summary-move-article n nil nil 'crosspost))

(defvar gnus-summary-respool-default-method nil
  "Default method for respooling an article.  
If nil, use to the current newsgroup method.")

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
		  methods nil t nil 'gnus-method-history))
		ms)
	   (cond
	    ((zerop (length (setq ms (gnus-servers-using-backend method))))
	     (list (intern method) ""))
	    ((= 1 (length ms))
	     (car ms))
	    (t
	     (cdr (completing-read 
		   "Server name: "
		   (mapcar (lambda (m) (cons (cadr m) m)) ms) nil t)))))))
  (gnus-set-global-variables)
  (unless method
    (error "No method given for respooling"))
  (if (assoc (symbol-name
	      (car (gnus-find-method-for-group gnus-newsgroup-name)))
	     (gnus-methods-using 'respool))
      (gnus-summary-move-article n nil method)
    (gnus-summary-copy-article n nil method)))

(defun gnus-summary-import-article (file)
  "Import a random file into a mail newsgroup."
  (interactive "fImport file: ")
  (gnus-set-global-variables)
  (let ((group gnus-newsgroup-name)
	(now (current-time))
	atts lines)
    (or (gnus-check-backend-function 'request-accept-article group)
	(error "%s does not support article importing" group))
    (or (file-readable-p file)
	(not (file-regular-p file))
	(error "Can't read %s" file))
    (save-excursion
      (set-buffer (get-buffer-create " *import file*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents file)
      (goto-char (point-min))
      (unless (nnheader-article-p)
	;; This doesn't look like an article, so we fudge some headers.
	(setq atts (file-attributes file)
	      lines (count-lines (point-min) (point-max)))
	(insert "From: " (read-string "From: ") "\n"
		"Subject: " (read-string "Subject: ") "\n"
		"Date: " (timezone-make-date-arpa-standard
			  (current-time-string (nth 5 atts))
			  (current-time-zone now)
			  (current-time-zone now)) "\n"
		"Message-ID: " (message-make-message-id) "\n"
		"Lines: " (int-to-string lines) "\n"
		"Chars: " (int-to-string (nth 7 atts)) "\n\n"))
      (gnus-request-accept-article group nil t)
      (kill-buffer (current-buffer)))))

(defun gnus-summary-expire-articles (&optional now)
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (gnus-set-global-variables)
  (when (gnus-check-backend-function
	 'request-expire-articles gnus-newsgroup-name)
    ;; This backend supports expiry.
    (let* ((total (gnus-group-total-expirable-p gnus-newsgroup-name))
	   (expirable (if total
			  (gnus-list-of-read-articles gnus-newsgroup-name)
			(setq gnus-newsgroup-expirable
			      (sort gnus-newsgroup-expirable '<))))
	   (expiry-wait (if now 'immediate
			  (gnus-group-get-parameter
			   gnus-newsgroup-name 'expiry-wait)))
	   es)
      (when expirable
	;; There are expirable articles in this group, so we run them
	;; through the expiry process.
	(gnus-message 6 "Expiring articles...")
	;; The list of articles that weren't expired is returned.
	(if expiry-wait
	    (let ((nnmail-expiry-wait-function nil)
		  (nnmail-expiry-wait expiry-wait))
	      (setq es (gnus-request-expire-articles
			expirable gnus-newsgroup-name)))
	  (setq es (gnus-request-expire-articles
		    expirable gnus-newsgroup-name)))
	(or total (setq gnus-newsgroup-expirable es))
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
  (gnus-set-global-variables)
  (or gnus-expert-user
      (gnus-y-or-n-p
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
  (gnus-set-global-variables)
  (or (gnus-check-backend-function 'request-expire-articles
				   gnus-newsgroup-name)
      (error "The current newsgroup does not support article deletion."))
  ;; Compute the list of articles to delete.
  (let ((articles (gnus-summary-work-articles n))
	not-deleted)
    (if (and gnus-novice-user
	     (not (gnus-y-or-n-p
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
	(or (memq (car articles) not-deleted)
	    (gnus-summary-mark-article (car articles) gnus-canceled-mark))
	(setq articles (cdr articles))))
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    not-deleted))

(defun gnus-summary-edit-article (&optional force)
  "Enter into a buffer and edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-set-global-variables)
    (when (and (not force)
	       (gnus-group-read-only-p))
      (error "The current newsgroup does not support article editing."))
    (gnus-summary-select-article t nil t)
    (gnus-configure-windows 'article)
    (select-window (get-buffer-window gnus-article-buffer))
    (gnus-message 6 "C-c C-c to end edits")
    (setq buffer-read-only nil)
    (text-mode)
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key "\C-c\C-c" 'gnus-summary-edit-article-done)
    (buffer-enable-undo)
    (widen)
    (goto-char (point-min))
    (search-forward "\n\n" nil t)))

(defun gnus-summary-edit-article-done ()
  "Make edits to the current article permanent."
  (interactive)
  (if (gnus-group-read-only-p)
      (progn
	(let ((beep (not (eq major-mode 'text-mode))))
	  (gnus-summary-edit-article-postpone)
	  (when beep
	    (gnus-error
	     3 "The current newsgroup does not support article editing."))))
    (let ((buf (format "%s" (buffer-string))))
      (erase-buffer)
      (insert buf)
      (if (not (gnus-request-replace-article
		(cdr gnus-article-current) (car gnus-article-current)
		(current-buffer)))
	  (error "Couldn't replace article.")
	(gnus-article-mode)
	(use-local-map gnus-article-mode-map)
	(setq buffer-read-only t)
	(buffer-disable-undo (current-buffer))
	(gnus-configure-windows 'summary)
	(gnus-summary-update-article (cdr gnus-article-current))
	(when gnus-use-cache
	  (gnus-cache-update-article 	
	   (car gnus-article-current) (cdr gnus-article-current)))
	(when gnus-keep-backlog
	  (gnus-backlog-remove-article 
	   (car gnus-article-current) (cdr gnus-article-current))))
      (save-excursion
	(when (get-buffer gnus-original-article-buffer)
	  (set-buffer gnus-original-article-buffer)
	  (setq gnus-original-article nil)))
      (setq gnus-article-current nil
	    gnus-current-article nil)
      (run-hooks 'gnus-article-display-hook)
      (and (gnus-visual-p 'summary-highlight 'highlight)
	   (run-hooks 'gnus-visual-mark-article-hook)))))

(defun gnus-summary-edit-article-postpone ()
  "Postpone changes to the current article."
  (interactive)
  (gnus-article-mode)
  (use-local-map gnus-article-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (gnus-configure-windows 'summary)
  (and (gnus-visual-p 'summary-highlight 'highlight)
       (run-hooks 'gnus-visual-mark-article-hook)))

(defun gnus-summary-respool-query ()
  "Query where the respool algorithm would put this article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (goto-char (point-min))
      (search-forward "\n\n")
      (narrow-to-region (point-min) (point))
      (pp-eval-expression
       (list 'quote (mapcar 'car (nnmail-article-group 'identity)))))))

;; Summary marking commands.

(defun gnus-summary-kill-same-subject-and-select (&optional unmark)
  "Mark articles which has the same subject as read, and then select the next.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (gnus-set-global-variables)
  (if unmark
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
  (gnus-set-global-variables)
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; If marked as read, go to next unread subject.
    (if (null unmark)
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
  (gnus-set-global-variables)
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
    (if (/= 0 n) (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    n))

(defun gnus-summary-unmark-as-processable (n)
  "Remove the process mark from the next N articles.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-mark-as-processable n t))

(defun gnus-summary-unmark-all-processable ()
  "Remove the process mark from all articles."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (while gnus-newsgroup-processable
      (gnus-summary-remove-process-mark (car gnus-newsgroup-processable))))
  (gnus-summary-position-point))

(defun gnus-summary-mark-as-expirable (n)
  "Mark N articles forward as expirable.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-mark-forward n gnus-expirable-mark))

(defun gnus-summary-mark-article-as-replied (article)
  "Mark ARTICLE replied and update the summary line."
  (setq gnus-newsgroup-replied (cons article gnus-newsgroup-replied))
  (let ((buffer-read-only nil))
    (when (gnus-summary-goto-subject article)
      (gnus-summary-update-secondary-mark article))))

(defun gnus-summary-set-bookmark (article)
  "Set a bookmark in current article."
  (interactive (list (gnus-summary-article-number)))
  (gnus-set-global-variables)
  (if (or (not (get-buffer gnus-article-buffer))
	  (not gnus-current-article)
	  (not gnus-article-current)
	  (not (equal gnus-newsgroup-name (car gnus-article-current))))
      (error "No current article selected"))
  ;; Remove old bookmark, if one exists.
  (let ((old (assq article gnus-newsgroup-bookmarks)))
    (if old (setq gnus-newsgroup-bookmarks
		  (delq old gnus-newsgroup-bookmarks))))
  ;; Set the new bookmark, which is on the form
  ;; (article-number . line-number-in-body).
  (setq gnus-newsgroup-bookmarks
	(cons
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
	 gnus-newsgroup-bookmarks))
  (gnus-message 6 "A bookmark has been added to the current article."))

(defun gnus-summary-remove-bookmark (article)
  "Remove the bookmark from the current article."
  (interactive (list (gnus-summary-article-number)))
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
  (gnus-summary-mark-forward n gnus-dormant-mark))

(defun gnus-summary-set-process-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable
	(cons article
	      (delq article gnus-newsgroup-processable)))
  (when (gnus-summary-goto-subject article)
    (gnus-summary-show-thread)
    (gnus-summary-update-secondary-mark article)))

(defun gnus-summary-remove-process-mark (article)
  "Remove the process mark from ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (delq article gnus-newsgroup-processable))
  (when (gnus-summary-goto-subject article)
    (gnus-summary-show-thread)
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
returned."
  (interactive "p")
  (gnus-set-global-variables)
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
    (if (/= 0 n) (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
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
    (setq gnus-newsgroup-reads
	  (cons (cons article mark) gnus-newsgroup-reads))
    ;; Possibly remove from cache, if that is used.
    (and gnus-use-cache (gnus-cache-enter-remove-article article))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    ;; Check for auto-expiry.
    (when (and gnus-newsgroup-auto-expire
	       (or (= mark gnus-killed-mark) (= mark gnus-del-mark)
		   (= mark gnus-catchup-mark) (= mark gnus-low-score-mark)
		   (= mark gnus-ancient-mark)
		   (= mark gnus-read-mark) (= mark gnus-souped-mark)))
      (setq mark gnus-expirable-mark)
      (push article gnus-newsgroup-expirable))
    ;; Set the mark in the buffer.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article-as-unread (mark)
  "Mark the current article quickly as unread with MARK."
  (let ((article (gnus-summary-article-number)))
    (if (< article 0)
	(gnus-error 1 "Unmarkable article")
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
      (setq gnus-newsgroup-reads
	    (delq (assq article gnus-newsgroup-reads)
		  gnus-newsgroup-reads))

      ;; See whether the article is to be put in the cache.
      (and gnus-use-cache
	   (vectorp (gnus-summary-article-header article))
	   (save-excursion
	     (gnus-cache-possibly-enter-article
	      gnus-newsgroup-name article
	      (gnus-summary-article-header article)
	      (= mark gnus-ticked-mark)
	      (= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

      ;; Fix the mark.
      (gnus-summary-update-mark mark 'unread))
    t))

(defun gnus-summary-mark-article (&optional article mark no-expire)
  "Mark ARTICLE with MARK.  MARK can be any character.
Four MARK strings are reserved: `? ' (unread), `?!' (ticked),
`??' (dormant) and `?E' (expirable).
If MARK is nil, then the default character `?D' is used.
If ARTICLE is nil, then the article on the current line will be
marked."
  ;; The mark might be a string.
  (and (stringp mark)
       (setq mark (aref mark 0)))
  ;; If no mark is given, then we check auto-expiring.
  (and (not no-expire)
       gnus-newsgroup-auto-expire
       (or (not mark)
	   (and (numberp mark)
		(or (= mark gnus-killed-mark) (= mark gnus-del-mark)
		    (= mark gnus-catchup-mark) (= mark gnus-low-score-mark)
		    (= mark gnus-read-mark) (= mark gnus-souped-mark))))
       (setq mark gnus-expirable-mark))
  (let* ((mark (or mark gnus-del-mark))
	 (article (or article (gnus-summary-article-number))))
    (or article (error "No article on current line"))
    (if (or (= mark gnus-unread-mark)
	    (= mark gnus-ticked-mark)
	    (= mark gnus-dormant-mark))
	(gnus-mark-article-as-unread article mark)
      (gnus-mark-article-as-read article mark))

    ;; See whether the article is to be put in the cache.
    (and gnus-use-cache
	 (not (= mark gnus-canceled-mark))
	 (vectorp (gnus-summary-article-header article))
	 (save-excursion
	   (gnus-cache-possibly-enter-article
	    gnus-newsgroup-name article
	    (gnus-summary-article-header article)
	    (= mark gnus-ticked-mark)
	    (= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

    (if (gnus-summary-goto-subject article nil t)
	(let ((buffer-read-only nil))
	  (gnus-summary-show-thread)
	  ;; Fix the mark.
	  (gnus-summary-update-mark mark 'unread)
	  t))))

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
    (run-hooks 'gnus-summary-update-hook))
  t)

(defun gnus-summary-update-mark (mark type)
  (beginning-of-line)
  (let ((forward (cdr (assq type gnus-summary-mark-positions)))
	(buffer-read-only nil))
    (when (and forward
	       (<= (+ forward (point)) (point-max)))
      ;; Go to the right position on the line.
      (goto-char (+ forward (point)))
      ;; Replace the old mark with the new mark.
      (subst-char-in-region (point) (1+ (point)) (following-char) mark)
      ;; Optionally update the marks by some user rule.
      (when (eq type 'unread)
	(gnus-data-set-mark
	 (gnus-data-find (gnus-summary-article-number)) mark)
	(gnus-summary-update-line (eq mark gnus-unread-mark))))))

(defun gnus-mark-article-as-read (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  ;; Make the article expirable.
  (let ((mark (or mark gnus-del-mark)))
    (if (= mark gnus-expirable-mark)
	(setq gnus-newsgroup-expirable (cons article gnus-newsgroup-expirable))
      (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable)))
    ;; Remove from unread and marked lists.
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (push (cons article mark) gnus-newsgroup-reads)
    ;; Possibly remove from cache, if that is used.
    (when gnus-use-cache
      (gnus-cache-enter-remove-article article))))

(defun gnus-mark-article-as-unread (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  (let ((mark (or mark gnus-ticked-mark)))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (cond ((= mark gnus-ticked-mark)
	   (push article gnus-newsgroup-marked))
	  ((= mark gnus-dormant-mark)
	   (push article gnus-newsgroup-dormant))
	  (t
	   (push article gnus-newsgroup-unreads)))
    (setq gnus-newsgroup-reads
	  (delq (assq article gnus-newsgroup-reads)
		gnus-newsgroup-reads))))

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
  (gnus-summary-mark-forward n gnus-del-mark t))

(defun gnus-summary-mark-as-read-backward (n)
  "Mark the N articles as read backwards.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-del-mark t))

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
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
  (gnus-summary-mark-below score gnus-killed-mark))

(defun gnus-summary-clear-above (&optional score)
  "Clear all marks from articles with score above SCORE."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-mark-above score gnus-unread-mark))

(defun gnus-summary-tick-above (&optional score)
  "Tick all articles with score above SCORE."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-mark-above score gnus-ticked-mark))

(defun gnus-summary-mark-above (score mark)
  "Mark articles with score over SCORE with MARK."
  (interactive "P\ncMark: ")
  (gnus-set-global-variables)
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while (and (progn
		  (if (> (gnus-summary-article-score) score)
		      (gnus-summary-mark-article nil mark))
		  t)
		(gnus-summary-find-next)))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defalias 'gnus-summary-show-all-expunged 'gnus-summary-limit-include-expunged)
(defun gnus-summary-limit-include-expunged ()
  "Display all the hidden articles that were expunged for low scores."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil))
    (let ((scored gnus-newsgroup-scored)
	  headers h)
      (while scored
	(or (gnus-summary-goto-subject (caar scored))
	    (and (setq h (gnus-summary-article-header (caar scored)))
		 (< (cdar scored) gnus-summary-expunge-below)
		 (setq headers (cons h headers))))
	(setq scored (cdr scored)))
      (or headers (error "No expunged articles hidden."))
      (goto-char (point-min))
      (gnus-summary-prepare-unthreaded (nreverse headers)))
    (goto-char (point-min))
    (gnus-summary-position-point)))

(defun gnus-summary-catchup (&optional all quietly to-here not-mark)
  "Mark all articles not marked as unread in this newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read.
If QUIETLY is non-nil, no questions will be asked.
If TO-HERE is non-nil, it should be a point in the buffer.  All
articles before this point will be marked as read.
The number of articles marked as read is returned."
  (interactive "P")
  (gnus-set-global-variables)
  (prog1
      (if (or quietly
	      (not gnus-interactive-catchup) ;Without confirmation?
	      gnus-expert-user
	      (gnus-y-or-n-p
	       (if all
		   "Mark absolutely all articles as read? "
		 "Mark all unread articles as read? ")))
	  (if (and not-mark
		   (not gnus-newsgroup-adaptive)
		   (not gnus-newsgroup-auto-expire))
	      (progn
		(when all
		  (setq gnus-newsgroup-marked nil
			gnus-newsgroup-dormant nil))
		(setq gnus-newsgroup-unreads nil))
	    ;; We actually mark all articles as canceled, which we
	    ;; have to do when using auto-expiry or adaptive scoring.
	    (gnus-summary-show-all-threads)
	    (if (gnus-summary-first-subject (not all))
		(while (and
			(if to-here (< (point) to-here) t)
			(gnus-summary-mark-article-as-read gnus-catchup-mark)
			(gnus-summary-find-next (not all)))))
	    (unless to-here
	      (setq gnus-newsgroup-unreads nil))
	    (gnus-set-mode-line 'summary)))
    (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
      (if (and (not to-here) (eq 'nnvirtual (car method)))
	  (nnvirtual-catchup-group
	   (gnus-group-real-name gnus-newsgroup-name) (nth 1 method) all)))
    (gnus-summary-position-point)))

(defun gnus-summary-catchup-to-here (&optional all)
  "Mark all unticked articles before the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive "P")
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
  (gnus-summary-catchup t quietly))

(defun gnus-summary-catchup-and-exit (&optional all quietly)
  "Mark all articles not marked as unread in this newsgroup as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-catchup all quietly nil 'fast)
  ;; Select next newsgroup or exit.
  (if (eq gnus-auto-select-next 'quietly)
      (gnus-summary-next-group nil)
    (gnus-summary-exit)))

(defun gnus-summary-catchup-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-catchup-and-exit t quietly))

;; Suggested by "Arne Eofsson" <arne@hodgkin.mbi.ucla.edu>.
(defun gnus-summary-catchup-and-goto-next-group (&optional all)
  "Mark all articles in this group as read and select the next group.
If given a prefix, mark all articles, unread as well as ticked, as
read."
  (interactive "P")
  (gnus-set-global-variables)
  (save-excursion
    (gnus-summary-catchup all))
  (gnus-summary-next-article t nil nil t))

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
  (gnus-set-global-variables)
  (let* ((gnus-show-threads t)
	 (article (gnus-summary-article-number))
	 (id (mail-header-id (gnus-summary-article-header)))
	 (gnus-newsgroup-threads (list (gnus-id-to-thread (gnus-root-id id)))))
    (unless id
      (error "No article on the current line"))
    (gnus-rebuild-thread id)
    (gnus-summary-goto-subject article)))

(defun gnus-summary-reparent-thread ()
  "Make current article child of the marked (or previous) article.

Note that the re-threading will only work if `gnus-thread-ignore-subject'
is non-nil or the Subject: of both articles are the same."
  (interactive)
  (or (not (gnus-group-read-only-p))
      (error "The current newsgroup does not support article editing."))
  (or (<= (length gnus-newsgroup-processable) 1)
      (error "No more than one article may be marked."))
  (save-window-excursion
    (let ((gnus-article-buffer " *reparent*")
	  (current-article (gnus-summary-article-number))
	  ; first grab the marked article, otherwise one line up.
	  (parent-article (if (not (null gnus-newsgroup-processable))
			      (car gnus-newsgroup-processable)
			    (save-excursion
			      (if (eq (forward-line -1) 0)
				  (gnus-summary-article-number)
				(error "Beginning of summary buffer."))))))
      (or (not (eq current-article parent-article))
	  (error "An article may not be self-referential."))
      (let ((message-id (mail-header-id 
			 (gnus-summary-article-header parent-article))))
	(or (and message-id (not (equal message-id "")))
	    (error "No message-id in desired parent."))
	(gnus-summary-select-article t t nil current-article)
	(set-buffer gnus-article-buffer)
	(setq buffer-read-only nil)
	(let ((buf (format "%s" (buffer-string))))
	  (erase-buffer)
	  (insert buf))
	(goto-char (point-min))
	(if (search-forward-regexp "^References: " nil t)
	    (insert message-id " " )
	  (insert "References: " message-id "\n"))
	(or (gnus-request-replace-article current-article
					  (car gnus-article-current)
					  gnus-article-buffer)
	    (error "Couldn't replace article."))
	(set-buffer gnus-summary-buffer)
	(gnus-summary-unmark-all-processable)
	(gnus-summary-rethread-current)
	(gnus-message 3 "Article %d is now the child of article %d."
		      current-article parent-article)))))

(defun gnus-summary-toggle-threads (&optional arg)
  "Toggle showing conversation threads.
If ARG is positive number, turn showing conversation threads on."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((current (or (gnus-summary-article-number) gnus-newsgroup-end)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    (gnus-summary-position-point)))

(defun gnus-summary-show-all-threads ()
  "Show all threads."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (let ((buffer-read-only nil))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)))
  (gnus-summary-position-point))

(defun gnus-summary-show-thread ()
  "Show thread subtrees.
Returns nil if no thread was there to be shown."
  (interactive)
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
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
	    nil)
	;;(gnus-summary-position-point)
	))))

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

(defun gnus-summary-go-to-next-thread-old (&optional previous)
  "Go to the same level (or less) next thread.
If PREVIOUS is non-nil, go to previous thread instead.
Return the article number moved to, or nil if moving was impossible."
  (if (and (eq gnus-summary-make-false-root 'dummy)
	   (gnus-summary-article-intangible-p))
      (let ((beg (point)))
	(while (and (zerop (forward-line 1))
		    (not (gnus-summary-article-intangible-p))
		    (not (zerop (save-excursion 
				  (gnus-summary-thread-level))))))
	(if (eobp)
	    (progn
	      (goto-char beg)
	      nil)
	  (point)))
    (let* ((level (gnus-summary-thread-level))
	   (article (gnus-summary-article-number))
	   (data (cdr (gnus-data-find-list article (gnus-data-list previous))))
	   oart)
      (while data
	(if (<= (gnus-data-level (car data)) level)
	    (setq oart (gnus-data-number (car data))
		  data nil)
	  (setq data (cdr data))))
      (and oart
	   (gnus-summary-goto-subject oart)))))

(defun gnus-summary-next-thread (n &optional silent)
  "Go to the same level next N'th thread.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done.

If SILENT, don't output messages."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(n (abs n))
	old dum int)
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
  (gnus-set-global-variables)
  (gnus-summary-next-thread (- n)))

(defun gnus-summary-go-down-thread ()
  "Go down one level in the current thread."
  (let ((children (gnus-summary-article-children)))
    (and children
	 (gnus-summary-goto-subject (car children)))))

(defun gnus-summary-go-up-thread ()
  "Go up one level in the current thread."
  (let ((parent (gnus-summary-article-parent)))
    (and parent
	 (gnus-summary-goto-subject parent))))

(defun gnus-summary-down-thread (n)
  "Go down thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((up (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if up (gnus-summary-go-up-thread)
		  (gnus-summary-go-down-thread)))
      (setq n (1- n)))
    (gnus-summary-position-point)
    (if (/= 0 n) (gnus-message 7 "Can't go further"))
    n))

(defun gnus-summary-up-thread (n)
  "Go up thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-down-thread (- n)))

(defun gnus-summary-top-thread ()
  "Go to the top of the thread."
  (interactive)
  (gnus-set-global-variables)
  (while (gnus-summary-go-up-thread))
  (gnus-summary-article-number))

(defun gnus-summary-kill-thread (&optional unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (gnus-set-global-variables)
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
    (if (null unmark)
	;; Go to next unread subject.
	(gnus-summary-next-subject 1 t)))
  (gnus-set-mode-line 'summary))

;; Summary sorting commands

(defun gnus-summary-sort-by-number (&optional reverse)
  "Sort summary buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'number reverse))

(defun gnus-summary-sort-by-author (&optional reverse)
  "Sort summary buffer by author name alphabetically.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'author reverse))

(defun gnus-summary-sort-by-subject (&optional reverse)
  "Sort summary buffer by subject alphabetically. `Re:'s are ignored.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'subject reverse))

(defun gnus-summary-sort-by-date (&optional reverse)
  "Sort summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'date reverse))

(defun gnus-summary-sort-by-score (&optional reverse)
  "Sort summary buffer by score.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'score reverse))

(defun gnus-summary-sort (predicate reverse)
  "Sort summary buffer by PREDICATE.  REVERSE means reverse order."
  (gnus-set-global-variables)
  (let* ((thread (intern (format "gnus-thread-sort-by-%s" predicate)))
	 (article (intern (format "gnus-article-sort-by-%s" predicate)))
	 (gnus-thread-sort-functions
	  (list
	   (if (not reverse)
	       thread
	     `(lambda (t1 t2)
		(,thread t2 t1)))))
	 (gnus-article-sort-functions
	  (list
	   (if (not reverse)
	       article
	     `(lambda (t1 t2)
		(,article t2 t1)))))
	 (buffer-read-only)
	 (gnus-summary-prepare-hook nil))
    ;; We do the sorting by regenerating the threads.
    (gnus-summary-prepare)
    ;; Hide subthreads if needed.
    (when (and gnus-show-threads gnus-thread-hide-subtree)
      (gnus-summary-hide-all-threads)))
  ;; If in async mode, we send some info to the backend.
  (when gnus-newsgroup-async
    (gnus-request-asynchronous
     gnus-newsgroup-name gnus-newsgroup-data)))

(defun gnus-sortable-date (date)
  "Make sortable string by string-lessp from DATE.
Timezone package is used."
  (condition-case ()
      (progn
	(setq date (inline (timezone-fix-time 
			    date nil 
			    (aref (inline (timezone-parse-date date)) 4))))
	(inline
	  (timezone-make-sortable-date
	   (aref date 0) (aref date 1) (aref date 2)
	   (inline
	     (timezone-make-time-string
	      (aref date 3) (aref date 4) (aref date 5))))))
    (error "")))
  
;; Summary saving commands.

(defun gnus-summary-save-article (&optional n not-saved)
  "Save the current article using the default saver function.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead.
The variable `gnus-default-article-saver' specifies the saver function."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n))
	(save-buffer (save-excursion 
		       (nnheader-set-temp-buffer " *Gnus Save*")))
	file header article)
    (while articles
      (setq header (gnus-summary-article-header
		    (setq article (pop articles))))
      (if (not (vectorp header))
	  ;; This is a pseudo-article.
	  (if (assq 'name header)
	      (gnus-copy-file (cdr (assq 'name header)))
	    (gnus-message 1 "Article %d is unsaveable" article))
	;; This is a real article.
	(save-window-excursion
	  (gnus-summary-select-article t nil nil article))
	(save-excursion
	  (set-buffer save-buffer)
	  (erase-buffer)
	  (insert-buffer-substring gnus-original-article-buffer))
	(unless gnus-save-all-headers
	  ;; Remove headers accoring to `gnus-saved-headers'.
	  (let ((gnus-visible-headers
		 (or gnus-saved-headers gnus-visible-headers))
		(gnus-article-buffer save-buffer))
	    (gnus-article-hide-headers 1 t)))
	(save-window-excursion
	  (if (not gnus-default-article-saver)
	      (error "No default saver is defined.")
	    ;; !!! Magic!  The saving functions all save
	    ;; `gnus-original-article-buffer' (or so they think),
	    ;; but we bind that variable to our save-buffer.
	    (set-buffer gnus-article-buffer)
	    (let ((gnus-original-article-buffer save-buffer))
	      (set-buffer gnus-summary-buffer)
	      (setq file (funcall
			  gnus-default-article-saver
			  (cond
			   ((not gnus-prompt-before-saving)
			    'default)
			   ((eq gnus-prompt-before-saving 'always)
			    nil)
			   (t file)))))))
	(gnus-summary-remove-process-mark article)
	(unless not-saved
	  (gnus-summary-set-saved-mark article))))
    (gnus-kill-buffer save-buffer)
    (gnus-summary-position-point)
    n))

(defun gnus-summary-pipe-output (&optional arg)
  "Pipe the current article to a subprocess.
If N is a positive number, pipe the N next articles.
If N is a negative number, pipe the N previous articles.
If N is nil and any articles have been marked with the process mark,
pipe those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
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
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-mail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-rmail (&optional arg)
  "Append the current article to an rmail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-rmail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-file (&optional arg)
  "Append the current article to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-body-file (&optional arg)
  "Append the current article body to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-body-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-get-split-value (methods)
  "Return a value based on the split METHODS."
  (let (split-name method result match)
    (when methods
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(save-restriction
	  (nnheader-narrow-to-headers)
	  (while methods
	    (goto-char (point-min))
	    (setq method (pop methods))
	    (setq match (car method))
	    (when (cond
		   ((stringp match)
		    ;; Regular expression.
		    (condition-case ()
			(re-search-forward match nil t)
		      (error nil)))
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
	      (setq split-name (append (cdr method) split-name))
	      (cond ((stringp result)
		     (push result split-name))
		    ((consp result)
		     (setq split-name (append result split-name)))))))))
    split-name))

(defun gnus-read-move-group-name (prompt default articles prefix)
  "Read a group name."
  (let* ((split-name (gnus-get-split-value gnus-move-split-methods))
	 (minibuffer-confirm-incomplete nil) ; XEmacs
	 group-map
	 (dum (mapatoms
	       (lambda (g) 
		 (and (boundp g)
		      (symbol-name g)
		      (memq 'respool
			    (assoc (symbol-name
				    (car (gnus-find-method-for-group
					  (symbol-name g))))
				   gnus-valid-select-methods))
		      (push (list (symbol-name g)) group-map)))
	       gnus-active-hashtb))
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
				  group-map nil nil prefix
				  'gnus-group-history))
	   ((= 1 (length split-name))
	    (gnus-completing-read (car split-name) prom group-map
				  nil nil nil
				  'gnus-group-history))
	   (t
	    (gnus-completing-read nil prom 
				  (mapcar (lambda (el) (list el))
					  (nreverse split-name))
				  nil nil nil
				  'gnus-group-history)))))
    (when to-newsgroup
      (if (or (string= to-newsgroup "")
	      (string= to-newsgroup prefix))
	  (setq to-newsgroup (or default "")))
      (or (gnus-active to-newsgroup)
	  (gnus-activate-group to-newsgroup)
	  (if (gnus-y-or-n-p (format "No such group: %s.  Create it? "
				     to-newsgroup))
	      (or (and (gnus-request-create-group 
			to-newsgroup (gnus-group-name-to-method to-newsgroup))
		       (gnus-activate-group to-newsgroup nil nil
					    (gnus-group-name-to-method
					     to-newsgroup)))
		  (error "Couldn't create group %s" to-newsgroup)))
	  (error "No such group: %s" to-newsgroup)))
    to-newsgroup))

(defun gnus-read-save-file-name (prompt default-name)
  (let* ((split-name (gnus-get-split-value gnus-split-methods))
	 (file
	  ;; Let the split methods have their say.
	  (cond
	   ;; No split name was found.
	   ((null split-name)
	    (read-file-name
	     (concat prompt " (default "
		     (file-name-nondirectory default-name) ") ")
	     (file-name-directory default-name)
	     default-name))
	   ;; A single split name was found
	   ((= 1 (length split-name))
 	    (let* ((name (car split-name))
 		   (dir (cond ((file-directory-p name)
 			       (file-name-as-directory name))
 			      ((file-exists-p name) name)
 			      (t gnus-article-save-directory))))
 	      (read-file-name
 	       (concat prompt " (default " name ") ")
 	       dir name)))
	   ;; A list of splits was found.
	   (t
	    (setq split-name (nreverse split-name))
	    (let (result)
	      (let ((file-name-history (nconc split-name file-name-history)))
		(setq result
		      (read-file-name
		       (concat prompt " (`M-p' for defaults) ")
		       gnus-article-save-directory
		       (car split-name))))
	      (car (push result file-name-history)))))))
    ;; If we have read a directory, we append the default file name.
    (when (file-directory-p file)
      (setq file (concat (file-name-as-directory file)
			 (file-name-nondirectory default-name))))
    ;; Possibly translate some charaters.
    (nnheader-translate-file-chars file)))

(defun gnus-article-archive-name (group)
  "Return the first instance of an \"Archive-name\" in the current buffer."
  (let ((case-fold-search t))
    (when (re-search-forward "archive-name: *\\([^ \n\t]+\\)[ \t]*$" nil t)
      (match-string 1))))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-rmail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-rmail)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name
		    "Save in rmail file:" default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (gnus-output-to-rmail filename))))
    ;; Remember the directory name to save articles
    (setq gnus-newsgroup-last-rmail filename)))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-mail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-mail)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name
		    "Save in Unix mail file:" default-name))))
    (setq filename
	  (expand-file-name filename
			    (and default-name
				 (file-name-directory default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (if (and (file-readable-p filename) (mail-file-babyl-p filename))
	      (gnus-output-to-rmail filename)
	    (let ((mail-use-rfc822 t))
	      (rmail-output filename 1 t t))))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-mail filename)))

(defun gnus-summary-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name
		    "Save in file:" default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-body-in-file (&optional filename)
  "Append this article body to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name
		    "Save body in file:" default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (and (search-forward "\n\n" nil t)
	       (narrow-to-region (point) (point-max)))
	  (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-in-pipe (&optional command)
  "Pipe this article to subprocess."
  (interactive)
  (gnus-set-global-variables)
  (setq command
	(cond ((eq command 'default)
	       gnus-last-shell-command)
	      (command command)
	      (t (read-string "Shell command on article: "
			      gnus-last-shell-command))))
  (if (string-equal command "")
      (setq command gnus-last-shell-command))
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max) command nil)))
  (setq gnus-last-shell-command command))

;; Summary extract commands

(defun gnus-summary-insert-pseudos (pslist &optional not-view)
  (let ((buffer-read-only nil)
	(article (gnus-summary-article-number))
	after-article b e)
    (or (gnus-summary-goto-subject article)
	(error (format "No such article: %d" article)))
    (gnus-summary-position-point)
    ;; If all commands are to be bunched up on one line, we collect
    ;; them here.
    (if gnus-view-pseudos-separately
	()
      (let ((ps (setq pslist (sort pslist 'gnus-pseudos<)))
	    files action)
	(while ps
	  (setq action (cdr (assq 'action (car ps))))
	  (setq files (list (cdr (assq 'name (car ps)))))
	  (while (and ps (cdr ps)
		      (string= (or action "1")
			       (or (cdr (assq 'action (cadr ps))) "2")))
	    (setq files (cons (cdr (assq 'name (cadr ps))) files))
	    (setcdr ps (cddr ps)))
	  (if (not files)
	      ()
	    (if (not (string-match "%s" action))
		(setq files (cons " " files)))
	    (setq files (cons " " files))
	    (and (assq 'execute (car ps))
		 (setcdr (assq 'execute (car ps))
			 (funcall (if (string-match "%s" action)
				      'format 'concat)
				  action
				  (mapconcat (lambda (f) f) files " ")))))
	  (setq ps (cdr ps)))))
    (if (and gnus-view-pseudos (not not-view))
	(while pslist
	  (and (assq 'execute (car pslist))
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
    (run-hooks 'gnus-mark-article-hook)))

(defun gnus-execute-command (command &optional automatic)
  (save-excursion
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (setq buffer-read-only nil)
    (let ((command (if automatic command (read-string "Command: " command)))
	  ;; Just binding this here doesn't help, because there might
	  ;; be output from the process after exiting the scope of 
	  ;; this `let'.
	  ;; (buffer-read-only nil)
	  )
      (erase-buffer)
      (insert "$ " command "\n\n")
      (if gnus-view-pseudo-asynchronously
	  (start-process "gnus-execute" nil shell-file-name
			 shell-command-switch command)
	(call-process shell-file-name nil t nil
		      shell-command-switch command)))))

(defun gnus-copy-file (file &optional to)
  "Copy FILE to TO."
  (interactive
   (list (read-file-name "Copy file: " default-directory)
	 (read-file-name "Copy file to: " default-directory)))
  (gnus-set-global-variables)
  (or to (setq to (read-file-name "Copy file to: " default-directory)))
  (and (file-directory-p to)
       (setq to (concat (file-name-as-directory to)
			(file-name-nondirectory file))))
  (copy-file file to))

;; Summary kill commands.

(defun gnus-summary-edit-global-kill (article)
  "Edit the \"global\" kill file."
  (interactive (list (gnus-summary-article-number)))
  (gnus-set-global-variables)
  (gnus-group-edit-global-kill article))

(defun gnus-summary-edit-local-kill ()
  "Edit a local kill file applied to the current newsgroup."
  (interactive)
  (gnus-set-global-variables)
  (setq gnus-current-headers (gnus-summary-article-header))
  (gnus-set-global-variables)
  (gnus-group-edit-local-kill
   (gnus-summary-article-number) gnus-newsgroup-name))


;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(if gnus-article-mode-map
    nil
  (setq gnus-article-mode-map (make-keymap))
  (suppress-keymap gnus-article-mode-map)

  (gnus-define-keys gnus-article-mode-map
    " " gnus-article-goto-next-page
    "\177" gnus-article-goto-prev-page
    [delete] gnus-article-goto-prev-page
    "\C-c^" gnus-article-refer-article
    "h" gnus-article-show-summary
    "s" gnus-article-show-summary
    "\C-c\C-m" gnus-article-mail
    "?" gnus-article-describe-briefly
    gnus-mouse-2 gnus-article-push-button
    "\r" gnus-article-press-button
    "\t" gnus-article-next-button
    "\M-\t" gnus-article-prev-button
    "<" beginning-of-buffer
    ">" end-of-buffer
    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug)

  (substitute-key-definition
   'undefined 'gnus-article-read-summary-keys gnus-article-mode-map))

(defun gnus-article-mode ()
  "Major mode for displaying an article.

All normal editing commands are switched off.

The following commands are available:

\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node"
  (interactive)
  (when (and menu-bar-mode
	     (gnus-visual-p 'article-menu 'menu))
    (gnus-article-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq mode-name "Article")
  (setq major-mode 'gnus-article-mode)
  (make-local-variable 'minor-mode-alist)
  (or (assq 'gnus-show-mime minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list 'gnus-show-mime " MIME") minor-mode-alist)))
  (use-local-map gnus-article-mode-map)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter gnus-page-delimiter)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize the article buffer."
  (let* ((name (if gnus-single-article-buffer "*Article*"
		 (concat "*Article " gnus-newsgroup-name "*")))
	 (original
	  (progn (string-match "\\*Article" name)
		 (concat " *Original Article"
			 (substring name (match-end 0))))))
    (setq gnus-article-buffer name)
    (setq gnus-original-article-buffer original)
    ;; This might be a variable local to the summary buffer.
    (unless gnus-single-article-buffer
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(setq gnus-article-buffer name)
	(setq gnus-original-article-buffer original)
	(gnus-set-global-variables))
      (make-local-variable 'gnus-summary-buffer))
    ;; Init original article buffer.
    (save-excursion
      (set-buffer (get-buffer-create gnus-original-article-buffer))
      (buffer-disable-undo (current-buffer))
      (setq major-mode 'gnus-original-article-mode)
      (make-local-variable 'gnus-original-article))
    (if (get-buffer name)
	(save-excursion
	  (set-buffer name)
	  (buffer-disable-undo (current-buffer))
	  (setq buffer-read-only t)
	  (gnus-add-current-to-buffer-list)
	  (or (eq major-mode 'gnus-article-mode)
	      (gnus-article-mode))
	  (current-buffer))
      (save-excursion
	(set-buffer (get-buffer-create name))
	(gnus-add-current-to-buffer-list)
	(gnus-article-mode)
	(current-buffer)))))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (set-window-start
   (get-buffer-window gnus-article-buffer t)
   (save-excursion
     (set-buffer gnus-article-buffer)
     (goto-char (point-min))
     (if (not line)
	 (point-min)
       (gnus-message 6 "Moved to bookmark")
       (search-forward "\n\n" nil t)
       (forward-line line)
       (point)))))

(defun gnus-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (when (fboundp 'overlay-lists)
    (let* ((overlayss (overlay-lists))
	   (buffer-read-only nil)
	   (overlays (nconc (car overlayss) (cdr overlayss))))
      (while overlays
	(delete-overlay (pop overlays))))))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer."
  (let (do-update-line)
    (prog1
	(save-excursion
	  (erase-buffer)
	  (gnus-kill-all-overlays)
	  (setq group (or group gnus-newsgroup-name))

	  ;; Open server if it has closed.
	  (gnus-check-server (gnus-find-method-for-group group))

	  ;; Using `gnus-request-article' directly will insert the article into
	  ;; `nntp-server-buffer' - so we'll save some time by not having to
	  ;; copy it from the server buffer into the article buffer.

	  ;; We only request an article by message-id when we do not have the
	  ;; headers for it, so we'll have to get those.
	  (when (stringp article)
	    (let ((gnus-override-method gnus-refer-article-method))
	      (gnus-read-header article)))

	  ;; If the article number is negative, that means that this article
	  ;; doesn't belong in this newsgroup (possibly), so we find its
	  ;; message-id and request it by id instead of number.
	  (when (and (numberp article)
		     gnus-summary-buffer
		     (get-buffer gnus-summary-buffer)
		     (buffer-name (get-buffer gnus-summary-buffer)))
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (let ((header (gnus-summary-article-header article)))
		(if (< article 0)
		    (cond 
		     ((memq article gnus-newsgroup-sparse)
		      ;; This is a sparse gap article.
		      (setq do-update-line article)
		      (setq article (mail-header-id header))
		      (let ((gnus-override-method gnus-refer-article-method))
			(gnus-read-header article))
		      (setq gnus-newsgroup-sparse
			    (delq article gnus-newsgroup-sparse)))
		     ((vectorp header)
		      ;; It's a real article.
		      (setq article (mail-header-id header)))
		     (t
		      ;; It is an extracted pseudo-article.
		      (setq article 'pseudo)
		      (gnus-request-pseudo-article header))))
		
		(let ((method (gnus-find-method-for-group 
			       gnus-newsgroup-name)))
		  (if (not (eq (car method) 'nneething))
		      ()
		    (let ((dir (concat (file-name-as-directory (nth 1 method))
				       (mail-header-subject header))))
		      (if (file-directory-p dir)
			  (progn
			    (setq article 'nneething)
			    (gnus-group-enter-directory dir)))))))))

	  (cond
	   ;; Refuse to select canceled articles.
	   ((and (numberp article)
		 gnus-summary-buffer
		 (get-buffer gnus-summary-buffer)
		 (buffer-name (get-buffer gnus-summary-buffer))
		 (eq (cdr (save-excursion
			    (set-buffer gnus-summary-buffer)
			    (assq article gnus-newsgroup-reads)))
		     gnus-canceled-mark))
	    nil)
	   ;; We first check `gnus-original-article-buffer'.
	   ((and (get-buffer gnus-original-article-buffer)
		 (numberp article)
		 (save-excursion
		   (set-buffer gnus-original-article-buffer)
		   (and (equal (car gnus-original-article) group)
			(eq (cdr gnus-original-article) article))))
	    (insert-buffer-substring gnus-original-article-buffer)
	    'article)
	   ;; Check the backlog.
	   ((and gnus-keep-backlog
		 (gnus-backlog-request-article group article (current-buffer)))
	    'article)
	   ;; Check the cache.
	   ((and gnus-use-cache
		 (numberp article)
		 (gnus-cache-request-article article group))
	    'article)
	   ;; Get the article and put into the article buffer.
	   ((or (stringp article) (numberp article))
	    (let ((gnus-override-method
		   (and (stringp article) gnus-refer-article-method))
		  (buffer-read-only nil))
	      (erase-buffer)
	      (gnus-kill-all-overlays)
	      (if (gnus-request-article article group (current-buffer))
		  (progn
		    (and gnus-keep-backlog
			 (numberp article)
			 (gnus-backlog-enter-article
			  group article (current-buffer)))
		    'article))))
	   ;; It was a pseudo.
	   (t article)))

      ;; Take the article from the original article buffer
      ;; and place it in the buffer it's supposed to be in.
      (when (and (get-buffer gnus-article-buffer)
		 ;;(numberp article)
		 (equal (buffer-name (current-buffer))
			(buffer-name (get-buffer gnus-article-buffer))))
	(save-excursion
	  (if (get-buffer gnus-original-article-buffer)
	      (set-buffer (get-buffer gnus-original-article-buffer))
	    (set-buffer (get-buffer-create gnus-original-article-buffer))
	    (buffer-disable-undo (current-buffer))
	    (setq major-mode 'gnus-original-article-mode)
	    (setq buffer-read-only t)
	    (gnus-add-current-to-buffer-list))
	  (let (buffer-read-only)
	    (erase-buffer)
	    (insert-buffer-substring gnus-article-buffer))
	  (setq gnus-original-article (cons group article))))
    
      ;; Update sparse articles.
      (when (and do-update-line
		 (or (numberp article)
		     (stringp article)))
	(let ((buf (current-buffer)))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-update-article do-update-line)
	  (gnus-summary-goto-subject do-update-line nil t)
	  (set-window-point (get-buffer-window (current-buffer) t)
			    (point))
	  (set-buffer buf))))))

(defun gnus-read-header (id &optional header)
  "Read the headers of article ID and enter them into the Gnus system."
  (let ((group gnus-newsgroup-name)
	(gnus-override-method 
	 (and (gnus-news-group-p gnus-newsgroup-name)
	      gnus-refer-article-method))	
	where)
    ;; First we check to see whether the header in question is already
    ;; fetched.
    (if (stringp id)
	;; This is a Message-ID.
	(setq header (or header (gnus-id-to-header id)))
      ;; This is an article number.
      (setq header (or header (gnus-summary-article-header id))))
    (if (and header
	     (not (memq (mail-header-number header) gnus-newsgroup-sparse)))
	;; We have found the header.
	header
      ;; We have to really fetch the header to this article.
      (when (setq where (gnus-request-head id group))
	(save-excursion
	  (set-buffer nntp-server-buffer)
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
	;(when (and header
	;	   (memq (mail-header-number header) gnus-newsgroup-sparse))
	;  (setcar (gnus-id-to-thread id) nil))
	(if (not (setq header (car (gnus-get-newsgroup-headers))))
	    ()				; Malformed head.
	  (unless (memq (mail-header-number header) gnus-newsgroup-sparse)
	    (if (and (stringp id)
		     (not (string= (gnus-group-real-name group)
				   (car where))))
		;; If we fetched by Message-ID and the article came
		;; from a different group, we fudge some bogus article
		;; numbers for this article.
		(mail-header-set-number header gnus-reffed-article-number))
	    (decf gnus-reffed-article-number)
	    (gnus-remove-header (mail-header-number header))
	    (push header gnus-newsgroup-headers)
	    (setq gnus-current-headers header)
	    (push (mail-header-number header) gnus-newsgroup-limit))
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

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (unless (eq major-mode 'gnus-summary-mode)
      (set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    ;; Make sure the connection to the server is alive.
    (unless (gnus-server-opened
	     (gnus-find-method-for-group gnus-newsgroup-name))
      (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
      (gnus-request-group gnus-newsgroup-name t))
    (let* ((article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	;; Deactivate active regions.
	(when (and (boundp 'transient-mark-mode)
		   transient-mark-mode)
	  (setq mark-active nil))
	(if (not (setq result (let ((buffer-read-only nil))
				(gnus-request-article-this-buffer
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (when (and (numberp article)
			 (not (memq article gnus-newsgroup-sparse)))
		(setq gnus-article-current
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(gnus-summary-mark-article article gnus-canceled-mark))
	      (unless (memq article gnus-newsgroup-sparse)
		(gnus-error
		 1 "No such article (may have expired or been canceled)")))
	  (if (or (eq result 'pseudo) (eq result 'nneething))
	      (progn
		(save-excursion
		  (set-buffer summary-buffer)
		  (setq gnus-last-article gnus-current-article
			gnus-newsgroup-history (cons gnus-current-article
						     gnus-newsgroup-history)
			gnus-current-article 0
			gnus-current-headers nil
			gnus-article-current nil)
		  (if (eq result 'nneething)
		      (gnus-configure-windows 'summary)
		    (gnus-configure-windows 'article))
		  (gnus-set-global-variables))
		(gnus-set-mode-line 'article))
	    ;; The result from the `request' was an actual article -
	    ;; or at least some text that is now displayed in the
	    ;; article buffer.
	    (if (and (numberp article)
		     (not (eq article gnus-current-article)))
		;; Seems like a new article has been selected.
		;; `gnus-current-article' must be an article number.
		(save-excursion
		  (set-buffer summary-buffer)
		  (setq gnus-last-article gnus-current-article
			gnus-newsgroup-history (cons gnus-current-article
						     gnus-newsgroup-history)
			gnus-current-article article
			gnus-current-headers
			(gnus-summary-article-header gnus-current-article)
			gnus-article-current
			(cons gnus-newsgroup-name gnus-current-article))
		  (unless (vectorp gnus-current-headers)
		    (setq gnus-current-headers nil))
		  (gnus-summary-show-thread)
		  (run-hooks 'gnus-mark-article-hook)
		  (gnus-set-mode-line 'summary)
		  (and (gnus-visual-p 'article-highlight 'highlight)
		       (run-hooks 'gnus-visual-mark-article-hook))
		  ;; Set the global newsgroup variables here.
		  ;; Suggested by Jim Sisolak
		  ;; <sisolak@trans4.neep.wisc.edu>.
		  (gnus-set-global-variables)
		  (setq gnus-have-all-headers
			(or all-headers gnus-show-all-headers))
		  (and gnus-use-cache
		       (vectorp (gnus-summary-article-header article))
		       (gnus-cache-possibly-enter-article
			group article
			(gnus-summary-article-header article)
			(memq article gnus-newsgroup-marked)
			(memq article gnus-newsgroup-dormant)
			(memq article gnus-newsgroup-unreads)))))
	    (when (or (numberp article)
		      (stringp article))
	      ;; Hooks for getting information from the article.
	      ;; This hook must be called before being narrowed.
	      (let (buffer-read-only)
		(run-hooks 'internal-hook)
		(run-hooks 'gnus-article-prepare-hook)
		;; Decode MIME message.
		(if gnus-show-mime
		    (if (or (not gnus-strict-mime)
			    (gnus-fetch-field "Mime-Version"))
			(funcall gnus-show-mime-method)
		      (funcall gnus-decode-encoded-word-method)))
		;; Perform the article display hooks.
		(run-hooks 'gnus-article-display-hook))
	      ;; Do page break.
	      (goto-char (point-min))
	      (and gnus-break-pages (gnus-narrow-to-page)))
	    (gnus-set-mode-line 'article)
	    (gnus-configure-windows 'article)
	    (goto-char (point-min))
	    t))))))

(defun gnus-article-show-all-headers ()
  "Show all article headers in article mode buffer."
  (save-excursion
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (gnus-unhide-text (point-min) (point-max)))))

(defun gnus-article-hide-headers-if-wanted ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatibility."
  (or (save-excursion (set-buffer gnus-summary-buffer) gnus-have-all-headers)
      gnus-inhibit-hiding
      (gnus-article-hide-headers)))

(defsubst gnus-article-header-rank ()
  "Give the rank of the string HEADER as given by `gnus-sorted-header-list'."
  (let ((list gnus-sorted-header-list)
	(i 0))
    (while list
      (when (looking-at (car list))
	(setq list nil))
      (setq list (cdr list))
      (incf i))
    i))

(defun gnus-article-hide-headers (&optional arg delete)
  "Toggle whether to hide unwanted headers and possibly sort them as well.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-hidden-arg))
  (if (gnus-article-check-hidden-text 'headers arg)
      ;; Show boring headers as well.
      (gnus-article-show-hidden-text 'boring-headers)
    ;; This function might be inhibited.
    (unless gnus-inhibit-hiding
      (save-excursion
	(set-buffer gnus-article-buffer)
	(save-restriction
	  (let ((buffer-read-only nil)
		(props (nconc (list 'gnus-type 'headers)
			      gnus-hidden-properties))
		(max (1+ (length gnus-sorted-header-list)))
		(ignored (when (not (stringp gnus-visible-headers))
			   (cond ((stringp gnus-ignored-headers)
				  gnus-ignored-headers)
				 ((listp gnus-ignored-headers)
				  (mapconcat 'identity gnus-ignored-headers
					     "\\|")))))
		(visible
		 (cond ((stringp gnus-visible-headers)
			gnus-visible-headers)
		       ((and gnus-visible-headers
			     (listp gnus-visible-headers))
			(mapconcat 'identity gnus-visible-headers "\\|"))))
		(inhibit-point-motion-hooks t)
		want-list beg)
	    ;; First we narrow to just the headers.
	    (widen)
	    (goto-char (point-min))
	    ;; Hide any "From " lines at the beginning of (mail) articles.
	    (while (looking-at "From ")
	      (forward-line 1))
	    (unless (bobp)
	      (if delete
		  (delete-region (point-min) (point))
		(gnus-hide-text (point-min) (point) props)))
	    ;; Then treat the rest of the header lines.
	    (narrow-to-region
	     (point)
	     (progn (search-forward "\n\n" nil t) (forward-line -1) (point)))
	    ;; Then we use the two regular expressions
	    ;; `gnus-ignored-headers' and `gnus-visible-headers' to
	    ;; select which header lines is to remain visible in the
	    ;; article buffer.
	    (goto-char (point-min))
	    (while (re-search-forward "^[^ \t]*:" nil t)
	      (beginning-of-line)
	      ;; We add the headers we want to keep to a list and delete
	      ;; them from the buffer.
	      (gnus-put-text-property 
	       (point) (1+ (point)) 'message-rank
	       (if (or (and visible (looking-at visible))
		       (and ignored
			    (not (looking-at ignored))))
		   (gnus-article-header-rank) 
		 (+ 2 max)))
	      (forward-line 1))
	    (message-sort-headers-1)
	    (when (setq beg (text-property-any 
			     (point-min) (point-max) 'message-rank (+ 2 max)))
	      ;; We make the unwanted headers invisible.
	      (if delete
		  (delete-region beg (point-max))
		;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>.
		(gnus-hide-text-type beg (point-max) 'headers))
	      ;; Work around XEmacs lossage.
	      (gnus-put-text-property (point-min) beg 'invisible nil))))))))

(defun gnus-article-hide-boring-headers (&optional arg)
  "Toggle hiding of headers that aren't very interesting.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-hidden-arg))
  (unless (gnus-article-check-hidden-text 'boring-headers arg)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (save-restriction
	(let ((buffer-read-only nil)
	      (list gnus-boring-article-headers)
	      (inhibit-point-motion-hooks t)
	      elem)
	  (nnheader-narrow-to-headers)
	  (while list
	    (setq elem (pop list))
	    (goto-char (point-min))
	    (cond
	     ;; Hide empty headers.
	     ((eq elem 'empty)
	      (while (re-search-forward "^[^:]+:[ \t]\n[^ \t]" nil t)
		(forward-line -1)
		(gnus-hide-text-type
		 (progn (beginning-of-line) (point))
		 (progn 
		   (end-of-line)
		   (if (re-search-forward "^[^ \t]" nil t)
		       (match-beginning 0)
		     (point-max)))
		 'boring-headers)))
	     ;; Hide boring Newsgroups header.
	     ((eq elem 'newsgroups)
	      (when (equal (message-fetch-field "newsgroups")
			   (gnus-group-real-name gnus-newsgroup-name))
		(gnus-article-hide-header "newsgroups")))
	     ((eq elem 'followup-to)
	      (when (equal (message-fetch-field "followup-to")
			   (message-fetch-field "newsgroups"))
		(gnus-article-hide-header "followup-to")))
	     ((eq elem 'reply-to)
	      (let ((from (message-fetch-field "from"))
		    (reply-to (message-fetch-field "reply-to")))
		(when (and
		       from reply-to
		       (equal 
			(nth 1 (funcall gnus-extract-address-components from))
			(nth 1 (funcall gnus-extract-address-components
					reply-to))))
		  (gnus-article-hide-header "reply-to"))))
	     ((eq elem 'date)
	      (let ((date (message-fetch-field "date")))
		(when (and date
			   (< (gnus-days-between date (current-time-string))
			      4))
		  (gnus-article-hide-header "date")))))))))))

(defun gnus-article-hide-header (header)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" header ":") nil t)
      (gnus-hide-text-type
       (progn (beginning-of-line) (point))
       (progn 
	 (end-of-line)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (match-beginning 0)
	   (point-max)))
       'boring-headers))))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-article-treat-overstrike ()
  "Translate overstrikes into bold text."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (while (search-forward "\b" nil t)
	(let ((next (following-char))
	      (previous (char-after (- (point) 2))))
	  (cond 
	   ((eq next previous)
	    (gnus-put-text-property (- (point) 2) (point) 'invisible t)
	    (gnus-put-text-property (point) (1+ (point)) 'face 'bold))
	   ((eq next ?_)
	    (gnus-put-text-property (1- (point)) (1+ (point)) 'invisible t)
	    (gnus-put-text-property
	     (- (point) 2) (1- (point)) 'face 'underline))
	   ((eq previous ?_)
	    (gnus-put-text-property (- (point) 2) (point) 'invisible t)
	    (gnus-put-text-property
	     (point) (1+ (point))	'face 'underline))))))))

(defun gnus-article-word-wrap ()
  "Format too long lines."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (end-of-line 1)
      (let ((paragraph-start "^[>|#:<;* ]*[ \t]*$")
	    (adaptive-fill-regexp "[ \t]*\\([|#:<;>*]+ *\\)?")
	    (adaptive-fill-mode t))
	(while (not (eobp))
	  (and (>= (current-column) (min fill-column (window-width)))
	       (/= (preceding-char) ?:)
	       (fill-paragraph nil))
	  (end-of-line 2))))))

(defun gnus-article-remove-cr ()
  "Remove carriage returns from an article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t)))))

(defun gnus-article-remove-trailing-blank-lines ()
  "Remove all trailing blank lines from the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (delete-region
       (point)
       (progn
	 (while (looking-at "^[ \t]*$")
	   (forward-line -1))
	 (forward-line 1)
	 (point))))))

(defun gnus-article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    ;; Delete the old process, if any.
    (when (process-status "gnus-x-face")
      (delete-process "gnus-x-face"))
    (let ((inhibit-point-motion-hooks t)
	  (case-fold-search nil)
	  from)
      (save-restriction
	(nnheader-narrow-to-headers)
	(setq from (message-fetch-field "from"))
	(goto-char (point-min))
	(when (and gnus-article-x-face-command
		   (or force
		       ;; Check whether this face is censored.
		       (not gnus-article-x-face-too-ugly)
		       (and gnus-article-x-face-too-ugly from
			    (not (string-match gnus-article-x-face-too-ugly
					       from))))
		   ;; Has to be present.
		   (re-search-forward "^X-Face: " nil t))
	  ;; We now have the area of the buffer where the X-Face is stored.
	  (let ((beg (point))
		(end (1- (re-search-forward "^\\($\\|[^ \t]\\)" nil t))))
	    ;; We display the face.
	    (if (symbolp gnus-article-x-face-command)
		;; The command is a lisp function, so we call it.
		(if (gnus-functionp gnus-article-x-face-command)
		    (funcall gnus-article-x-face-command beg end)
		  (error "%s is not a function" gnus-article-x-face-command))
	      ;; The command is a string, so we interpret the command
	      ;; as a, well, command, and fork it off.
	      (let ((process-connection-type nil))
		(process-kill-without-query
		 (start-process
		  "gnus-x-face" nil shell-file-name shell-command-switch
		  gnus-article-x-face-command))
		(process-send-region "gnus-x-face" beg end)
		(process-send-eof "gnus-x-face")))))))))

(defalias 'gnus-headers-decode-quoted-printable 'gnus-decode-rfc1522)
(defun gnus-decode-rfc1522 ()
  "Hack to remove QP encoding from headers."
  (let ((case-fold-search t)
	(inhibit-point-motion-hooks t)
	(buffer-read-only nil)
	string)
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (or (search-forward "\n\n" nil t) (point-max)))

      (while (re-search-forward 
	      "=\\?iso-8859-1\\?q\\?\\([^?\t\n]*\\)\\?=" nil t)
	(setq string (match-string 1))
	(narrow-to-region (match-beginning 0) (match-end 0))
	(delete-region (point-min) (point-max))
	(insert string)
	(gnus-mime-decode-quoted-printable (goto-char (point-min)) (point-max))
	(subst-char-in-region (point-min) (point-max) ?_ ? )
	(widen)
	(goto-char (point-min))))))

(defun gnus-article-de-quoted-unreadable (&optional force)
  "Do a naive translation of a quoted-printable-encoded article.
This is in no way, shape or form meant as a replacement for real MIME
processing, but is simply a stop-gap measure until MIME support is
written.
If FORCE, decode the article whether it is marked as quoted-printable
or not."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((case-fold-search t)
	  (buffer-read-only nil)
	  (type (gnus-fetch-field "content-transfer-encoding")))
      (gnus-decode-rfc1522)
      (when (or force
		(and type (string-match "quoted-printable" (downcase type))))
	(goto-char (point-min))
	(search-forward "\n\n" nil 'move)
	(gnus-mime-decode-quoted-printable (point) (point-max))))))

(defun gnus-mime-decode-quoted-printable (from to)
  "Decode Quoted-Printable in the region between FROM and TO."
  (interactive "r")
  (goto-char from)
  (while (search-forward "=" to t)
    (cond ((eq (following-char) ?\n)
	   (delete-char -1)
	   (delete-char 1))
	  ((looking-at "[0-9A-F][0-9A-F]")
	   (subst-char-in-region
	    (1- (point)) (point) ?=
	    (hexl-hex-string-to-integer
	     (buffer-substring (point) (+ 2 (point)))))
	   (delete-char 2))
	  ((looking-at "=")
	   (delete-char 1))
	  ((gnus-message 3 "Malformed MIME quoted-printable message")))))

(defun gnus-article-hide-pgp (&optional arg)
  "Toggle hiding of any PGP headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-hidden-arg))
  (unless (gnus-article-check-hidden-text 'pgp arg)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (let ((props (nconc (list 'gnus-type 'pgp) gnus-hidden-properties))
	    buffer-read-only beg end)
	(widen)
	(goto-char (point-min))
	;; Hide the "header".
	(and (search-forward "\n-----BEGIN PGP SIGNED MESSAGE-----\n" nil t)
	     (gnus-hide-text (match-beginning 0) (match-end 0) props))
	(setq beg (point))
	;; Hide the actual signature.
	(and (search-forward "\n-----BEGIN PGP SIGNATURE-----\n" nil t)
	     (setq end (1+ (match-beginning 0)))
	     (gnus-hide-text
	      end
	      (if (search-forward "\n-----END PGP SIGNATURE-----\n" nil t)
		  (match-end 0)
		;; Perhaps we shouldn't hide to the end of the buffer
		;; if there is no end to the signature?
		(point-max))
	      props))
	;; Hide "- " PGP quotation markers.
	(when (and beg end)
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (re-search-forward "^- " nil t)
	    (gnus-hide-text (match-beginning 0) (match-end 0) props))
	  (widen))))))

(defun gnus-article-hide-signature (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-hidden-arg))
  (unless (gnus-article-check-hidden-text 'signature arg)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (save-restriction
	(let ((buffer-read-only nil))
	  (when (gnus-narrow-to-signature)
	    (gnus-hide-text-type (point-min) (point-max) 'signature)))))))

(defun gnus-article-strip-leading-blank-lines ()
  "Remove all blank lines from the beginning of the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let (buffer-read-only)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(while (looking-at "[ \t]$")
	  (gnus-delete-line))))))

(defvar mime::preview/content-list)
(defvar mime::preview-content-info/point-min)
(defun gnus-narrow-to-signature ()
  "Narrow to the signature."
  (widen)
  (if (and (boundp 'mime::preview/content-list)
	   mime::preview/content-list)
      (let ((pcinfo (car (last mime::preview/content-list))))
	(condition-case ()
	    (narrow-to-region
	     (funcall (intern "mime::preview-content-info/point-min") pcinfo)
	     (point-max))
	  (error nil))))
  (goto-char (point-max))
  (when (re-search-backward gnus-signature-separator nil t)
    (forward-line 1)
    (when (or (null gnus-signature-limit)
	      (and (numberp gnus-signature-limit)
		   (< (- (point-max) (point)) gnus-signature-limit))
	      (and (gnus-functionp gnus-signature-limit)
		   (funcall gnus-signature-limit))
	      (and (stringp gnus-signature-limit)
		   (not (re-search-forward gnus-signature-limit nil t))))
      (narrow-to-region (point) (point-max))
      t)))

(defun gnus-hidden-arg ()
  "Return the current prefix arg as a number, or 0 if no prefix."
  (list (if current-prefix-arg
	    (prefix-numeric-value current-prefix-arg)
	  0)))

(defun gnus-article-check-hidden-text (type arg)
  "Return nil if hiding is necessary.
Arg can be nil or a number.  Nil and positive means hide, negative
means show, 0 means toggle."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((hide (gnus-article-hidden-text-p type)))
      (cond
       ((or (null arg)
	    (> arg 0))
	nil)
       ((< arg 0)
	(gnus-article-show-hidden-text type))
       (t
	(if (eq hide 'hidden)
	    (gnus-article-show-hidden-text type)
	  nil))))))

(defun gnus-article-hidden-text-p (type)
  "Say whether the current buffer contains hidden text of type TYPE."
  (let ((pos (text-property-any (point-min) (point-max) 'gnus-type type)))
    (when pos
      (if (get-text-property pos 'invisible)
	  'hidden
	'shown))))

(defun gnus-article-hide (&optional arg force)
  "Hide all the gruft in the current article.
This means that PGP stuff, signatures, cited text and (some)
headers will be hidden.
If given a prefix, show the hidden text instead."
  (interactive (list current-prefix-arg 'force))
  (gnus-article-hide-headers arg)
  (gnus-article-hide-pgp arg)
  (gnus-article-hide-citation-maybe arg force)
  (gnus-article-hide-signature arg))

(defun gnus-article-show-hidden-text (type &optional hide)
  "Show all hidden text of type TYPE.
If HIDE, hide the text instead."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (beg (point-min)))
      (while (gnus-goto-char (text-property-any
			      beg (point-max) 'gnus-type type))
	(setq beg (point))
	(forward-char)
	(if hide
	    (gnus-hide-text beg (point) gnus-hidden-properties)
	  (gnus-unhide-text beg (point)))
	(setq beg (point)))
      t)))

(defvar gnus-article-time-units
  `((year . ,(* 365.25 24 60 60))
    (week . ,(* 7 24 60 60))
    (day . ,(* 24 60 60))
    (hour . ,(* 60 60))
    (minute . 60)
    (second . 1))
  "Mapping from time units to seconds.")

(defun gnus-article-date-ut (&optional type highlight)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE."
  (interactive (list 'ut t))
  (let* ((header (or gnus-current-headers
		     (gnus-summary-article-header) ""))
	 (date (and (vectorp header) (mail-header-date header)))
	 (date-regexp "^Date: \\|^X-Sent: ")
	 (now (current-time))
	 (inhibit-point-motion-hooks t)
	 bface eface)
    (when (and date (not (string= date "")))
      (save-excursion
	(set-buffer gnus-article-buffer)
	(save-restriction
	  (nnheader-narrow-to-headers)
	  (let ((buffer-read-only nil))
	    ;; Delete any old Date headers.
	    (if (re-search-forward date-regexp nil t)
		(progn
		  (setq bface (get-text-property (gnus-point-at-bol) 'face)
			eface (get-text-property (1- (gnus-point-at-eol))
						 'face))
		  (message-remove-header date-regexp t)
		  (beginning-of-line))
	      (goto-char (point-max)))
	    (insert (gnus-make-date-line date type))
	    ;; Do highlighting.
	    (forward-line -1)
	    (when (and (gnus-visual-p 'article-highlight 'highlight)
		       (looking-at "\\([^:]+\\): *\\(.*\\)$"))
	      (gnus-put-text-property (match-beginning 1) (match-end 1)
				 'face bface)
	      (gnus-put-text-property (match-beginning 2) (match-end 2)
				 'face eface))))))))

(defun gnus-make-date-line (date type)
  "Return a DATE line of TYPE."
  (cond
   ;; Convert to the local timezone.  We have to slap a
   ;; `condition-case' round the calls to the timezone
   ;; functions since they aren't particularly resistant to
   ;; buggy dates.
   ((eq type 'local)
    (concat "Date: " (condition-case ()
			 (timezone-make-date-arpa-standard date)
		       (error date))
	    "\n"))
   ;; Convert to Universal Time.
   ((eq type 'ut)
    (concat "Date: "
	    (condition-case ()
		(timezone-make-date-arpa-standard date nil "UT")
	      (error date))
	    "\n"))
   ;; Get the original date from the article.
   ((eq type 'original)
    (concat "Date: " date "\n"))
   ;; Do an X-Sent lapsed format.
   ((eq type 'lapsed)
    ;; If the date is seriously mangled, the timezone
    ;; functions are liable to bug out, so we condition-case
    ;; the entire thing.
    (let* ((now (current-time))
	   (real-time
	    (condition-case ()
		(gnus-time-minus
		 (gnus-encode-date
		  (timezone-make-date-arpa-standard
		   (current-time-string now)
		   (current-time-zone now) "UT"))
		 (gnus-encode-date
		  (timezone-make-date-arpa-standard
		   date nil "UT")))
	      (error '(0 0))))
	   (real-sec (+ (* (float (car real-time)) 65536)
			(cadr real-time)))
	   (sec (abs real-sec))
	   num prev)
      (cond
       ((equal real-time '(0 0))
	"X-Sent: Unknown\n")
       ((zerop sec)
	"X-Sent: Now\n")
       (t
	(concat
	 "X-Sent: "
	 ;; This is a bit convoluted, but basically we go
	 ;; through the time units for years, weeks, etc,
	 ;; and divide things to see whether that results
	 ;; in positive answers.
	 (mapconcat
	  (lambda (unit)
	    (if (zerop (setq num (ffloor (/ sec (cdr unit)))))
		;; The (remaining) seconds are too few to
		;; be divided into this time unit.
		""
	      ;; It's big enough, so we output it.
	      (setq sec (- sec (* num (cdr unit))))
	      (prog1
		  (concat (if prev ", " "") (int-to-string
					     (floor num))
			  " " (symbol-name (car unit)) 
			  (if (> num 1) "s" ""))
		(setq prev t))))
	  gnus-article-time-units "")
	 ;; If dates are odd, then it might appear like the
	 ;; article was sent in the future.
	 (if (> real-sec 0)
	     " ago\n"
	   " in the future\n"))))))
   (t
    (error "Unknown conversion type: %s" type))))

(defun gnus-article-date-local (&optional highlight)
  "Convert the current article date to the local timezone."
  (interactive (list t))
  (gnus-article-date-ut 'local highlight))

(defun gnus-article-date-original (&optional highlight)
  "Convert the current article date to what it was originally.
This is only useful if you have used some other date conversion
function and want to see what the date was before converting."
  (interactive (list t))
  (gnus-article-date-ut 'original highlight))

(defun gnus-article-date-lapsed (&optional highlight)
  "Convert the current article date to time lapsed since it was sent."
  (interactive (list t))
  (gnus-article-date-ut 'lapsed highlight))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if `gnus-visual' is non-nil."
  (if (gnus-visual-p 'article-highlight 'highlight)
      (gnus-article-highlight-some)))

;;; Article savers.

(defun gnus-output-to-rmail (file-name)
  "Append the current article to an Rmail file named FILE-NAME."
  (require 'rmail)
  ;; Most of these codes are borrowed from rmailout.el.
  (setq file-name (expand-file-name file-name))
  (setq rmail-default-rmail-file file-name)
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-output*")))
    (save-excursion
      (or (get-file-buffer file-name)
	  (file-exists-p file-name)
	  (if (gnus-yes-or-no-p
	       (concat "\"" file-name "\" does not exist, create it? "))
	      (let ((file-buffer (create-file-buffer file-name)))
		(save-excursion
		  (set-buffer file-buffer)
		  (rmail-insert-rmail-file-header)
		  (let ((require-final-newline nil))
		    (write-region (point-min) (point-max) file-name t 1)))
		(kill-buffer file-buffer))
	    (error "Output file does not exist")))
      (set-buffer tmpbuf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      (gnus-convert-article-to-rmail)
      ;; Decide whether to append to a file or to an Emacs buffer.
      (let ((outbuf (get-file-buffer file-name)))
	(if (not outbuf)
	    (append-to-file (point-min) (point-max) file-name)
	  ;; File has been visited, in buffer OUTBUF.
	  (set-buffer outbuf)
	  (let ((buffer-read-only nil)
		(msg (and (boundp 'rmail-current-message)
			  (symbol-value 'rmail-current-message))))
	    ;; If MSG is non-nil, buffer is in RMAIL mode.
	    (if msg
		(progn (widen)
		       (narrow-to-region (point-max) (point-max))))
	    (insert-buffer-substring tmpbuf)
	    (if msg
		(progn
		  (goto-char (point-min))
		  (widen)
		  (search-backward "\^_")
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages t)
		  (rmail-show-message msg)))))))
    (kill-buffer tmpbuf)))

(defun gnus-output-to-file (file-name)
  "Append the current article to a file named FILE-NAME."
  (let ((artbuf (current-buffer)))
    (nnheader-temp-write nil
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (append-to-file (point-min) (point-max) file-name))))

(defun gnus-convert-article-to-rmail ()
  "Convert article in current buffer to Rmail message format."
  (let ((buffer-read-only nil))
    ;; Convert article directly into Babyl format.
    ;; Suggested by Rob Austein <sra@lcs.mit.edu>
    (goto-char (point-min))
    (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
    (while (search-forward "\n\^_" nil t) ;single char
      (replace-match "\n^_" t t))	;2 chars: "^" and "_"
    (goto-char (point-max))
    (insert "\^_")))

(defun gnus-narrow-to-page (&optional arg)
  "Narrow the article buffer to a page.
If given a numerical ARG, move forward ARG pages."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    (widen)
    (when (gnus-visual-p 'page-marker)
      (let ((buffer-read-only nil))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next)))
    (when
	(cond ((< arg 0)
	       (re-search-backward page-delimiter nil 'move (1+ (abs arg))))
	      ((> arg 0)
	       (re-search-forward page-delimiter nil 'move arg)))
      (goto-char (match-end 0)))
    (narrow-to-region
     (point)
     (if (re-search-forward page-delimiter nil 'move)
	 (match-beginning 0)
       (point)))
    (when (and (gnus-visual-p 'page-marker)
	       (not (= (point-min) 1)))
      (save-excursion
	(goto-char (point-min))
	(gnus-insert-prev-page-button)))
    (when (and (gnus-visual-p 'page-marker)
	       (not (= (1- (point-max)) (buffer-size))))
      (save-excursion
	(goto-char (point-max))
	(gnus-insert-next-page-button)))))

;; Article mode commands

(defun gnus-article-goto-next-page ()
  "Show the next page of the article."
  (interactive)
  (when (gnus-article-next-page)
    (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))))

(defun gnus-article-goto-prev-page ()
  "Show the next page of the article."
  (interactive)
  (if (bobp) (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))
    (gnus-article-prev-page nil)))

(defun gnus-article-next-page (&optional lines)
  "Show the next page of the current article.
If end of article, return non-nil.  Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "p")
  (move-to-window-line -1)
  ;; Fixed by enami@ptgd.sony.co.jp (enami tsugutomo)
  (if (save-excursion
	(end-of-line)
	(and (pos-visible-in-window-p)	;Not continuation line.
	     (eobp)))
      ;; Nothing in this page.
      (if (or (not gnus-break-pages)
	      (save-excursion
		(save-restriction
		  (widen) (forward-line 1) (eobp)))) ;Real end-of-buffer?
	  t				;Nothing more.
	(gnus-narrow-to-page 1)		;Go to next page.
	nil)
    ;; More in this page.
    (condition-case ()
	(scroll-up lines)
      (end-of-buffer
       ;; Long lines may cause an end-of-buffer error.
       (goto-char (point-max))))
    (move-to-window-line 0)
    nil))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "p")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1)	;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (prog1
	(condition-case ()
	    (scroll-down lines)
	  (error nil))
      (move-to-window-line 0))))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (let ((point (point)))
    (search-forward ">" nil t)		;Move point to end of "<....>".
    (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
	(let ((message-id (match-string 1)))
	  (goto-char point)
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-refer-article message-id))
      (goto-char (point))
      (error "No references around point"))))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-goto-subject gnus-current-article))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-goto-next-page]:Next page	 \\[gnus-article-goto-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

(defun gnus-article-summary-command ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let ((obuf (current-buffer))
	(owin (current-window-configuration))
	func)
    (switch-to-buffer gnus-summary-buffer 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)
    (set-buffer obuf)
    (set-window-configuration owin)
    (set-window-point (get-buffer-window (current-buffer)) (point))))

(defun gnus-article-summary-command-nosave ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let (func)
    (pop-to-buffer gnus-summary-buffer 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)))

(defun gnus-article-read-summary-keys (&optional arg key not-restore-window)
  "Read a summary buffer key sequence and execute it from the article buffer."
  (interactive "P")
  (let ((nosaves
	 '("q" "Q"  "c" "r" "R" "\C-c\C-f" "m"	"a" "f" "F"
	   "Zc" "ZC" "ZE" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP"
	   "=" "^" "\M-^" "|"))
	(nosave-but-article
	 '("A\r"))
	keys)
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (push (or key last-command-event) unread-command-events)
      (setq keys (read-key-sequence nil)))
    (message "")

    (if (or (member keys nosaves)
	    (member keys nosave-but-article))
	(let (func)
	  (save-window-excursion
	    (pop-to-buffer gnus-summary-buffer 'norecord)
	    (setq func (lookup-key (current-local-map) keys)))
	  (if (not func)
	      (ding)
	    (set-buffer gnus-summary-buffer)
	    (call-interactively func))
	  (when (member keys nosave-but-article)
	    (pop-to-buffer gnus-article-buffer 'norecord)))
      (let ((obuf (current-buffer))
	    (owin (current-window-configuration))
	    (opoint (point))
	    func in-buffer)
	(if not-restore-window
	    (pop-to-buffer gnus-summary-buffer 'norecord)
	  (switch-to-buffer gnus-summary-buffer 'norecord))
	(setq in-buffer (current-buffer))
	(if (setq func (lookup-key (current-local-map) keys))
	    (call-interactively func)
	  (ding))
	(when (eq in-buffer (current-buffer))
	  (set-buffer obuf)
	  (unless not-restore-window
	    (set-window-configuration owin))
	  (set-window-point (get-buffer-window (current-buffer)) opoint))))))


;;;
;;; Kill file handling.
;;;

;;;###autoload
(defalias 'gnus-batch-kill 'gnus-batch-score)
;;;###autoload
(defun gnus-batch-score ()
  "Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-score <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\".  If you would not like to
score the alt hierarchy, you'd say \"!alt.all\"."
  (interactive)
  (let* ((yes-and-no
	  (gnus-newsrc-parse-options
	   (apply (function concat)
		  (mapcar (lambda (g) (concat g " "))
			  command-line-args-left))))
	 (gnus-expert-user t)
	 (nnmail-spool-file nil)
	 (gnus-use-dribble-file nil)
	 (yes (car yes-and-no))
	 (no (cdr yes-and-no))
	 group newsrc entry
	 ;; Disable verbose message.
	 gnus-novice-user gnus-large-newsgroup)
    ;; Eat all arguments.
    (setq command-line-args-left nil)
    ;; Start Gnus.
    (gnus)
    ;; Apply kills to specified newsgroups in command line arguments.
    (setq newsrc (cdr gnus-newsrc-alist))
    (while newsrc
      (setq group (caar newsrc))
      (setq entry (gnus-gethash group gnus-newsrc-hashtb))
      (if (and (<= (nth 1 (car newsrc)) gnus-level-subscribed)
	       (and (car entry)
		    (or (eq (car entry) t)
			(not (zerop (car entry)))))
	       (if yes (string-match yes group) t)
	       (or (null no) (not (string-match no group))))
	  (progn
	    (gnus-summary-read-group group nil t nil t)
	    (and (eq (current-buffer) (get-buffer gnus-summary-buffer))
		 (gnus-summary-exit))))
      (setq newsrc (cdr newsrc)))
    ;; Exit Emacs.
    (switch-to-buffer gnus-group-buffer)
    (gnus-group-save-newsrc)))

(defun gnus-apply-kill-file ()
  "Apply a kill file to the current newsgroup.
Returns the number of articles marked as read."
  (if (or (file-exists-p (gnus-newsgroup-kill-file nil))
	  (file-exists-p (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (gnus-apply-kill-file-internal)
    0))

(defun gnus-kill-save-kill-buffer ()
  (let ((file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
    (when (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(and (buffer-modified-p) (save-buffer))
	(kill-buffer (current-buffer))))))

(defvar gnus-kill-file-name "KILL"
  "Suffix of the kill files.")

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a kill file name for NEWSGROUP.
If NEWSGROUP is nil, return the global kill file name instead."
  (cond 
   ;; The global KILL file is placed at top of the directory.
   ((or (null newsgroup)
	(string-equal newsgroup ""))
    (expand-file-name gnus-kill-file-name
		      gnus-kill-files-directory))
   ;; Append ".KILL" to newsgroup name.
   ((gnus-use-long-file-name 'not-kill)
    (expand-file-name (concat (gnus-newsgroup-savable-name newsgroup)
			      "." gnus-kill-file-name)
		      gnus-kill-files-directory))
   ;; Place "KILL" under the hierarchical directory.
   (t
    (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
			      "/" gnus-kill-file-name)
		      gnus-kill-files-directory))))


;;;
;;; Dribble file
;;;

(defvar gnus-dribble-ignore nil)
(defvar gnus-dribble-eval-file nil)

(defun gnus-dribble-file-name ()
  "Return the dribble file for the current .newsrc."
  (concat
   (if gnus-dribble-directory
       (concat (file-name-as-directory gnus-dribble-directory)
	       (file-name-nondirectory gnus-current-startup-file))
     gnus-current-startup-file)
   "-dribble"))

(defun gnus-dribble-enter (string)
  "Enter STRING into the dribble buffer."
  (if (and (not gnus-dribble-ignore)
	   gnus-dribble-buffer
	   (buffer-name gnus-dribble-buffer))
      (let ((obuf (current-buffer)))
	(set-buffer gnus-dribble-buffer)
	(insert string "\n")
	(set-window-point (get-buffer-window (current-buffer)) (point-max))
	(bury-buffer gnus-dribble-buffer)
	(set-buffer obuf))))

(defun gnus-dribble-read-file ()
  "Read the dribble file from disk."
  (let ((dribble-file (gnus-dribble-file-name)))
    (save-excursion
      (set-buffer (setq gnus-dribble-buffer
			(get-buffer-create
			 (file-name-nondirectory dribble-file))))
      (gnus-add-current-to-buffer-list)
      (erase-buffer)
      (setq buffer-file-name dribble-file)
      (auto-save-mode t)
      (buffer-disable-undo (current-buffer))
      (bury-buffer (current-buffer))
      (set-buffer-modified-p nil)
      (let ((auto (make-auto-save-file-name))
	    (gnus-dribble-ignore t)
	    modes)
	(when (or (file-exists-p auto) (file-exists-p dribble-file))
	  ;; Load whichever file is newest -- the auto save file
	  ;; or the "real" file.
	  (if (file-newer-than-file-p auto dribble-file)
	      (insert-file-contents auto)
	    (insert-file-contents dribble-file))
	  (unless (zerop (buffer-size))
	    (set-buffer-modified-p t))
	  ;; Set the file modes to reflect the .newsrc file modes.
	  (save-buffer)
	  (when (and (file-exists-p gnus-current-startup-file)
		     (setq modes (file-modes gnus-current-startup-file)))
	    (set-file-modes dribble-file modes))
	  ;; Possibly eval the file later.
	  (when (gnus-y-or-n-p
		 "Auto-save file exists.  Do you want to read it? ")
	    (setq gnus-dribble-eval-file t)))))))

(defun gnus-dribble-eval-file ()
  (when gnus-dribble-eval-file
    (setq gnus-dribble-eval-file nil)
    (save-excursion
      (let ((gnus-dribble-ignore t))
	(set-buffer gnus-dribble-buffer)
	(eval-buffer (current-buffer))))))

(defun gnus-dribble-delete-file ()
  (when (file-exists-p (gnus-dribble-file-name))
    (delete-file (gnus-dribble-file-name)))
  (when gnus-dribble-buffer
    (save-excursion
      (set-buffer gnus-dribble-buffer)
      (let ((auto (make-auto-save-file-name)))
	(if (file-exists-p auto)
	    (delete-file auto))
	(erase-buffer)
	(set-buffer-modified-p nil)))))

(defun gnus-dribble-save ()
  (when (and gnus-dribble-buffer
	     (buffer-name gnus-dribble-buffer))
    (save-excursion
      (set-buffer gnus-dribble-buffer)
      (save-buffer))))

(defun gnus-dribble-clear ()
  (when (gnus-buffer-exists-p gnus-dribble-buffer)
    (save-excursion
      (set-buffer gnus-dribble-buffer)
      (erase-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-saved-size (buffer-size)))))


;;;
;;; Server Communication
;;;

(defun gnus-start-news-server (&optional confirm)
  "Open a method for getting news.
If CONFIRM is non-nil, the user will be asked for an NNTP server."
  (let (how)
    (if gnus-current-select-method
	;; Stream is already opened.
	nil
      ;; Open NNTP server.
      (if (null gnus-nntp-service) (setq gnus-nntp-server nil))
      (if confirm
	  (progn
	    ;; Read server name with completion.
	    (setq gnus-nntp-server
		  (completing-read "NNTP server: "
				   (mapcar (lambda (server) (list server))
					   (cons (list gnus-nntp-server)
						 gnus-secondary-servers))
				   nil nil gnus-nntp-server))))

      (if (and gnus-nntp-server
	       (stringp gnus-nntp-server)
	       (not (string= gnus-nntp-server "")))
	  (setq gnus-select-method
		(cond ((or (string= gnus-nntp-server "")
			   (string= gnus-nntp-server "::"))
		       (list 'nnspool (system-name)))
		      ((string-match "^:" gnus-nntp-server)
		       (list 'nnmh gnus-nntp-server
			     (list 'nnmh-directory
				   (file-name-as-directory
				    (expand-file-name
				     (concat "~/" (substring
						   gnus-nntp-server 1)))))
			     (list 'nnmh-get-new-mail nil)))
		      (t
		       (list 'nntp gnus-nntp-server)))))

      (setq how (car gnus-select-method))
      (cond ((eq how 'nnspool)
	     (require 'nnspool)
	     (gnus-message 5 "Looking up local news spool..."))
	    ((eq how 'nnmh)
	     (require 'nnmh)
	     (gnus-message 5 "Looking up mh spool..."))
	    (t
	     (require 'nntp)))
      (setq gnus-current-select-method gnus-select-method)
      (run-hooks 'gnus-open-server-hook)
      (or
       ;; gnus-open-server-hook might have opened it
       (gnus-server-opened gnus-select-method)
       (gnus-open-server gnus-select-method)
       (gnus-y-or-n-p
	(format
	 "%s (%s) open error: '%s'.	Continue? "
	 (car gnus-select-method) (cadr gnus-select-method)
	 (gnus-status-message gnus-select-method)))
       (gnus-error 1 "Couldn't open server on %s"
		   (nth 1 gnus-select-method))))))

(defun gnus-check-group (group)
  "Try to make sure that the server where GROUP exists is alive."
  (let ((method (gnus-find-method-for-group group)))
    (or (gnus-server-opened method)
	(gnus-open-server method))))

(defun gnus-check-server (&optional method silent)
  "Check whether the connection to METHOD is down.
If METHOD is nil, use `gnus-select-method'.
If it is down, start it up (again)."
  (let ((method (or method gnus-select-method)))
    ;; Transform virtual server names into select methods.
    (when (stringp method)
      (setq method (gnus-server-to-method method)))
    (if (gnus-server-opened method)
	;; The stream is already opened.
	t
      ;; Open the server.
      (unless silent
	(gnus-message 5 "Opening %s server%s..." (car method)
		      (if (equal (nth 1 method) "") ""
			(format " on %s" (nth 1 method)))))
      (run-hooks 'gnus-open-server-hook)
      (prog1
	  (gnus-open-server method)
	(unless silent
	  (message ""))))))

(defun gnus-get-function (method function &optional noerror)
  "Return a function symbol based on METHOD and FUNCTION."
  ;; Translate server names into methods.
  (unless method
    (error "Attempted use of a nil select method"))
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((func (intern (format "%s-%s" (car method) function))))
    ;; If the functions isn't bound, we require the backend in
    ;; question.
    (unless (fboundp func)
      (require (car method))
      (when (and (not (fboundp func))
		 (not noerror))
	;; This backend doesn't implement this function.
	(error "No such function: %s" func)))
    func))


;;;
;;; Interface functions to the backends.
;;;

(defun gnus-open-server (method)
  "Open a connection to METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((elem (assoc method gnus-opened-servers)))
    ;; If this method was previously denied, we just return nil.
    (if (eq (nth 1 elem) 'denied)
	(progn
	  (gnus-message 1 "Denied server")
	  nil)
      ;; Open the server.
      (let ((result
	     (funcall (gnus-get-function method 'open-server)
		      (nth 1 method) (nthcdr 2 method))))
	;; If this hasn't been opened before, we add it to the list.
	(unless elem
	  (setq elem (list method nil)
		gnus-opened-servers (cons elem gnus-opened-servers)))
	;; Set the status of this server.
	(setcar (cdr elem) (if result 'ok 'denied))
	;; Return the result from the "open" call.
	result))))

(defun gnus-close-server (method)
  "Close the connection to METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'close-server) (nth 1 method)))

(defun gnus-request-list (method)
  "Request the active file from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-list) (nth 1 method)))

(defun gnus-request-list-newsgroups (method)
  "Request the newsgroups file from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-list-newsgroups) (nth 1 method)))

(defun gnus-request-newgroups (date method)
  "Request all new groups since DATE from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-newgroups)
	   date (nth 1 method)))

(defun gnus-server-opened (method)
  "Check whether a connection to METHOD has been opened."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'server-opened) (nth 1 method)))

(defun gnus-status-message (method)
  "Return the status message from METHOD.
If METHOD is a string, it is interpreted as a group name.   The method
this group uses will be queried."
  (let ((method (if (stringp method) (gnus-find-method-for-group method)
		  method)))
    (funcall (gnus-get-function method 'status-message) (nth 1 method))))

(defun gnus-request-group (group &optional dont-check method)
  "Request GROUP.  If DONT-CHECK, no information is required."
  (let ((method (or method (gnus-find-method-for-group group))))
    (when (stringp method)
      (setq method (gnus-server-to-method method)))
    (funcall (gnus-get-function method 'request-group)
	     (gnus-group-real-name group) (nth 1 method) dont-check)))

(defun gnus-request-asynchronous (group &optional articles)
  "Request that GROUP behave asynchronously.
ARTICLES is the `data' of the group."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-asynchronous)
	     (gnus-group-real-name group) (nth 1 method) articles)))

(defun gnus-list-active-group (group)
  "Request active information on GROUP."
  (let ((method (gnus-find-method-for-group group))
	(func 'list-active-group))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function method func)
	       (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-request-group-description (group)
  "Request a description of GROUP."
  (let ((method (gnus-find-method-for-group group))
	(func 'request-group-description))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function method func)
	       (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-close-group (group)
  "Request the GROUP be closed."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'close-group)
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-retrieve-headers (articles group &optional fetch-old)
  "Request headers for ARTICLES in GROUP.
If FETCH-OLD, retrieve all headers (or some subset thereof) in the group."
  (let ((method (gnus-find-method-for-group group)))
    (if (and gnus-use-cache (numberp (car articles)))
	(gnus-cache-retrieve-headers articles group fetch-old)
      (funcall (gnus-get-function method 'retrieve-headers)
	       articles (gnus-group-real-name group) (nth 1 method)
	       fetch-old))))

(defun gnus-retrieve-groups (groups method)
  "Request active information on GROUPS from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'retrieve-groups) groups (nth 1 method)))

(defun gnus-request-type (group &optional article)
  "Return the type (`post' or `mail') of GROUP (and ARTICLE)."
  (let ((method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function 'request-type (car method)))
	'unknown
      (funcall (gnus-get-function method 'request-type)
	       (gnus-group-real-name group) article))))

(defun gnus-request-update-mark (group article mark)
  "Return the type (`post' or `mail') of GROUP (and ARTICLE)."
  (let ((method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function 'request-update-mark (car method)))
	mark
      (funcall (gnus-get-function method 'request-update-mark)
	       (gnus-group-real-name group) article mark))))

(defun gnus-request-article (article group &optional buffer)
  "Request the ARTICLE in GROUP.
ARTICLE can either be an article number or an article Message-ID.
If BUFFER, insert the article in that group."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-article)
	     article (gnus-group-real-name group) (nth 1 method) buffer)))

(defun gnus-request-head (article group)
  "Request the head of ARTICLE in GROUP."
  (let* ((method (gnus-find-method-for-group group))
	 (head (gnus-get-function method 'request-head t)))
    (if (fboundp head)
	(funcall head article (gnus-group-real-name group) (nth 1 method))
      (let ((res (gnus-request-article article group)))
	(when res
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-min))
	    (when (search-forward "\n\n" nil t)
	      (delete-region (1- (point)) (point-max)))
	    (nnheader-fold-continuation-lines)))
	res))))

(defun gnus-request-body (article group)
  "Request the body of ARTICLE in GROUP."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-body)
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-post (method)
  "Post the current buffer using METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-post) (nth 1 method)))

(defun gnus-request-scan (group method)
  "Request a SCAN being performed in GROUP from METHOD.
If GROUP is nil, all groups on METHOD are scanned."
  (let ((method (if group (gnus-find-method-for-group group) method)))
    (funcall (gnus-get-function method 'request-scan)
	     (and group (gnus-group-real-name group)) (nth 1 method))))

(defsubst gnus-request-update-info (info method)
  "Request that METHOD update INFO."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (when (gnus-check-backend-function 'request-update-info (car method))
    (funcall (gnus-get-function method 'request-update-info)
	     (gnus-group-real-name (gnus-info-group info))
	     info (nth 1 method))))

(defun gnus-request-expire-articles (articles group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-expire-articles)
	     articles (gnus-group-real-name group) (nth 1 method)
	     force)))

(defun gnus-request-move-article
  (article group server accept-function &optional last)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-move-article)
	     article (gnus-group-real-name group)
	     (nth 1 method) accept-function last)))

(defun gnus-request-accept-article (group method &optional last)
  ;; Make sure there's a newline at the end of the article.
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (when (and (not method)
	     (stringp group))
    (setq method (gnus-group-name-to-method group)))
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (let ((func (car (or method (gnus-find-method-for-group group)))))
    (funcall (intern (format "%s-request-accept-article" func))
	     (if (stringp group) (gnus-group-real-name group) group)
	     (cadr method)
	     last)))

(defun gnus-request-replace-article (article group buffer)
  (let ((func (car (gnus-find-method-for-group group))))
    (funcall (intern (format "%s-request-replace-article" func))
	     article (gnus-group-real-name group) buffer)))

(defun gnus-request-associate-buffer (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-associate-buffer)
	     (gnus-group-real-name group))))

(defun gnus-request-restore-buffer (article group)
  "Request a new buffer restored to the state of ARTICLE."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-restore-buffer)
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-create-group (group &optional method)
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((method (or method (gnus-find-method-for-group group))))
    (funcall (gnus-get-function method 'request-create-group)
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-delete-group (group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-delete-group)
	     (gnus-group-real-name group) force (nth 1 method))))

(defun gnus-request-rename-group (group new-name)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-rename-group)
	     (gnus-group-real-name group)
	     (gnus-group-real-name new-name) (nth 1 method))))

(defun gnus-member-of-valid (symbol group)
  "Find out if GROUP has SYMBOL as part of its \"valid\" spec."
  (memq symbol (assoc
		(symbol-name (car (gnus-find-method-for-group group)))
		gnus-valid-select-methods)))

(defun gnus-method-option-p (method option)
  "Return non-nil if select METHOD has OPTION as a parameter."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (memq option (assoc (format "%s" (car method))
		      gnus-valid-select-methods)))

(defun gnus-server-extend-method (group method)
  ;; This function "extends" a virtual server.	If the server is
  ;; "hello", and the select method is ("hello" (my-var "something"))
  ;; in the group "alt.alt", this will result in a new virtual server
  ;; called "hello+alt.alt".
  (let ((entry
	 (gnus-copy-sequence
	  (if (equal (car method) "native") gnus-select-method
	    (cdr (assoc (car method) gnus-server-alist))))))
    (setcar (cdr entry) (concat (nth 1 entry) "+" group))
    (nconc entry (cdr method))))

(defun gnus-server-status (method)
  "Return the status of METHOD."
  (nth 1 (assoc method gnus-opened-servers)))

(defun gnus-group-name-to-method (group)
  "Return a select method suitable for GROUP."
  (if (string-match ":" group)
      (let ((server (substring group 0 (match-beginning 0))))
	(if (string-match "\\+" server)
	    (list (intern (substring server 0 (match-beginning 0)))
		  (substring server (match-end 0)))
	  (list (intern server) "")))
    gnus-select-method))

(defun gnus-find-method-for-group (group &optional info)
  "Find the select method that GROUP uses."
  (or gnus-override-method
      (and (not group)
	   gnus-select-method)
      (let ((info (or info (gnus-get-info group)))
	    method)
	(if (or (not info)
		(not (setq method (gnus-info-method info)))
		(equal method "native"))
	    gnus-select-method
	  (setq method
		(cond ((stringp method)
		       (gnus-server-to-method method))
		      ((stringp (car method))
		       (gnus-server-extend-method group method))
		      (t
		       method)))
	  (cond ((equal (cadr method) "")
		 method)
		((null (cadr method))
		 (list (car method) ""))
		(t
		 (gnus-server-add-address method)))))))

(defun gnus-check-backend-function (func group)
  "Check whether GROUP supports function FUNC."
  (let ((method (if (stringp group) (car (gnus-find-method-for-group group))
		  group)))
    (fboundp (intern (format "%s-%s" method func)))))

(defun gnus-methods-using (feature)
  "Find all methods that have FEATURE."
  (let ((valids gnus-valid-select-methods)
	outs)
    (while valids
      (if (memq feature (car valids))
	  (setq outs (cons (car valids) outs)))
      (setq valids (cdr valids)))
    outs))


;;;
;;; Active & Newsrc File Handling
;;;

(defun gnus-setup-news (&optional rawfile level dont-connect)
  "Setup news information.
If RAWFILE is non-nil, the .newsrc file will also be read.
If LEVEL is non-nil, the news will be set up at level LEVEL."
  (let ((init (not (and gnus-newsrc-alist gnus-active-hashtb (not rawfile)))))

    (when init 
      ;; Clear some variables to re-initialize news information.
      (setq gnus-newsrc-alist nil
	    gnus-active-hashtb nil)
      ;; Read the newsrc file and create `gnus-newsrc-hashtb'.
      (gnus-read-newsrc-file rawfile))

    (when (and (not (assoc "archive" gnus-server-alist))
	       (gnus-archive-server-wanted-p))
      (push (cons "archive" gnus-message-archive-method)
	    gnus-server-alist))

    ;; If we don't read the complete active file, we fill in the
    ;; hashtb here.
    (if (or (null gnus-read-active-file)
	    (eq gnus-read-active-file 'some))
	(gnus-update-active-hashtb-from-killed))

    ;; Read the active file and create `gnus-active-hashtb'.
    ;; If `gnus-read-active-file' is nil, then we just create an empty
    ;; hash table.  The partial filling out of the hash table will be
    ;; done in `gnus-get-unread-articles'.
    (and gnus-read-active-file
	 (not level)
	 (gnus-read-active-file))

    (or gnus-active-hashtb
	(setq gnus-active-hashtb (make-vector 4095 0)))

    ;; Initialize the cache.
    (when gnus-use-cache
      (gnus-cache-open))

    ;; Possibly eval the dribble file.
    (and init (or gnus-use-dribble-file gnus-slave) (gnus-dribble-eval-file))

    ;; Slave Gnusii should then clear the dribble buffer.
    (when (and init gnus-slave)
      (gnus-dribble-clear))

    (gnus-update-format-specifications)

    ;; See whether we need to read the description file.
    (if (and (string-match "%[-,0-9]*D" gnus-group-line-format)
	     (not gnus-description-hashtb)
	     (not dont-connect)
	     gnus-read-active-file)
	(gnus-read-all-descriptions-files))

    ;; Find new newsgroups and treat them.
    (if (and init gnus-check-new-newsgroups (not level)
	     (gnus-check-server gnus-select-method))
	(gnus-find-new-newsgroups))

    ;; We might read in new NoCeM messages here.
    (when (and gnus-use-nocem 
	       (not level)
	       (not dont-connect))
      (gnus-nocem-scan-groups))

    ;; Find the number of unread articles in each non-dead group.
    (let ((gnus-read-active-file (and (not level) gnus-read-active-file)))
      (gnus-get-unread-articles level))

    (if (and init gnus-check-bogus-newsgroups
	     gnus-read-active-file (not level)
	     (gnus-server-opened gnus-select-method))
	(gnus-check-bogus-newsgroups))))

(defun gnus-find-new-newsgroups (&optional arg)
  "Search for new newsgroups and add them.
Each new newsgroup will be treated with `gnus-subscribe-newsgroup-method.'
The `-n' option line from .newsrc is respected.
If ARG (the prefix), use the `ask-server' method to query
the server for new groups."
  (interactive "P")
  (let ((check (if (or (and arg (not (listp gnus-check-new-newsgroups)))
		       (null gnus-read-active-file)
		       (eq gnus-read-active-file 'some))
		   'ask-server gnus-check-new-newsgroups)))
    (unless (gnus-check-first-time-used)
      (if (or (consp check)
	      (eq check 'ask-server))
	  ;; Ask the server for new groups.
	  (gnus-ask-server-for-new-groups)
	;; Go through the active hashtb and look for new groups.
	(let ((groups 0)
	      group new-newsgroups)
	  (gnus-message 5 "Looking for new newsgroups...")
	  (unless gnus-have-read-active-file
	    (gnus-read-active-file))
	  (setq gnus-newsrc-last-checked-date (current-time-string))
	  (unless gnus-killed-hashtb
	    (gnus-make-hashtable-from-killed))
	  ;; Go though every newsgroup in `gnus-active-hashtb' and compare
	  ;; with `gnus-newsrc-hashtb' and `gnus-killed-hashtb'.
	  (mapatoms
	   (lambda (sym)
	     (if (or (null (setq group (symbol-name sym)))
		     (not (boundp sym))
		     (null (symbol-value sym))
		     (gnus-gethash group gnus-killed-hashtb)
		     (gnus-gethash group gnus-newsrc-hashtb))
		 ()
	       (let ((do-sub (gnus-matches-options-n group)))
		 (cond
		  ((eq do-sub 'subscribe)
		   (setq groups (1+ groups))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (funcall gnus-subscribe-options-newsgroup-method group))
		  ((eq do-sub 'ignore)
		   nil)
		  (t
		   (setq groups (1+ groups))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (if gnus-subscribe-hierarchical-interactive
		       (setq new-newsgroups (cons group new-newsgroups))
		     (funcall gnus-subscribe-newsgroup-method group)))))))
	   gnus-active-hashtb)
	  (when new-newsgroups
	    (gnus-subscribe-hierarchical-interactive new-newsgroups))
	  ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
	  (if (> groups 0)
	      (gnus-message 6 "%d new newsgroup%s arrived."
			    groups (if (> groups 1) "s have" " has"))
	    (gnus-message 6 "No new newsgroups.")))))))

(defun gnus-matches-options-n (group)
  ;; Returns `subscribe' if the group is to be unconditionally
  ;; subscribed, `ignore' if it is to be ignored, and nil if there is
  ;; no match for the group.

  ;; First we check the two user variables.
  (cond
   ((and gnus-options-subscribe
	 (string-match gnus-options-subscribe group))
    'subscribe)
   ((and gnus-auto-subscribed-groups
	 (string-match gnus-auto-subscribed-groups group))
    'subscribe)
   ((and gnus-options-not-subscribe
	 (string-match gnus-options-not-subscribe group))
    'ignore)
   ;; Then we go through the list that was retrieved from the .newsrc
   ;; file.  This list has elements on the form
   ;; `(REGEXP . {ignore,subscribe})'.  The first match found (the list
   ;; is in the reverse order of the options line) is returned.
   (t
    (let ((regs gnus-newsrc-options-n))
      (while (and regs
		  (not (string-match (caar regs) group)))
	(setq regs (cdr regs)))
      (and regs (cdar regs))))))

(defun gnus-ask-server-for-new-groups ()
  (let* ((date (or gnus-newsrc-last-checked-date (current-time-string)))
	 (methods (cons gnus-select-method
			(nconc
			 (when (gnus-archive-server-wanted-p)
			   (list "archive"))
			 (append
			  (and (consp gnus-check-new-newsgroups)
			       gnus-check-new-newsgroups)
			  gnus-secondary-select-methods))))
	 (groups 0)
	 (new-date (current-time-string))
	 group new-newsgroups got-new method hashtb
	 gnus-override-subscribe-method)
    ;; Go through both primary and secondary select methods and
    ;; request new newsgroups.
    (while (setq method (gnus-server-get-method nil (pop methods)))
      (setq new-newsgroups nil)
      (setq gnus-override-subscribe-method method)
      (when (and (gnus-check-server method)
		 (gnus-request-newgroups date method))
	(save-excursion
	  (setq got-new t)
	  (setq hashtb (gnus-make-hashtable 100))
	  (set-buffer nntp-server-buffer)
	  ;; Enter all the new groups into a hashtable.
	  (gnus-active-to-gnus-format method hashtb 'ignore))
	;; Now all new groups from `method' are in `hashtb'.
	(mapatoms
	 (lambda (group-sym)
	   (if (or (null (setq group (symbol-name group-sym)))
		   (not (boundp group-sym))
		   (null (symbol-value group-sym))
		   (gnus-gethash group gnus-newsrc-hashtb)
		   (member group gnus-zombie-list)
		   (member group gnus-killed-list))
	       ;; The group is already known.
	       ()
	     ;; Make this group active.
	     (when (symbol-value group-sym)
	       (gnus-set-active group (symbol-value group-sym)))
	     ;; Check whether we want it or not.
	     (let ((do-sub (gnus-matches-options-n group)))
	       (cond
		((eq do-sub 'subscribe)
		 (incf groups)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (funcall gnus-subscribe-options-newsgroup-method group))
		((eq do-sub 'ignore)
		 nil)
		(t
		 (incf groups)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (if gnus-subscribe-hierarchical-interactive
		     (push group new-newsgroups)
		   (funcall gnus-subscribe-newsgroup-method group)))))))
	 hashtb))
      (when new-newsgroups
	(gnus-subscribe-hierarchical-interactive new-newsgroups)))
    ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
    (when (> groups 0)
      (gnus-message 6 "%d new newsgroup%s arrived."
		    groups (if (> groups 1) "s have" " has")))
    (and got-new (setq gnus-newsrc-last-checked-date new-date))
    got-new))

(defun gnus-check-first-time-used ()
  (if (or (> (length gnus-newsrc-alist) 1)
	  (file-exists-p gnus-startup-file)
	  (file-exists-p (concat gnus-startup-file ".el"))
	  (file-exists-p (concat gnus-startup-file ".eld")))
      nil
    (gnus-message 6 "First time user; subscribing you to default groups")
    (unless (gnus-read-active-file-p)
      (gnus-read-active-file))
    (setq gnus-newsrc-last-checked-date (current-time-string))
    (let ((groups gnus-default-subscribed-newsgroups)
	  group)
      (if (eq groups t)
	  nil
	(setq groups (or groups gnus-backup-default-subscribed-newsgroups))
	(mapatoms
	 (lambda (sym)
	   (if (null (setq group (symbol-name sym)))
	       ()
	     (let ((do-sub (gnus-matches-options-n group)))
	       (cond
		((eq do-sub 'subscribe)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (funcall gnus-subscribe-options-newsgroup-method group))
		((eq do-sub 'ignore)
		 nil)
		(t
		 (setq gnus-killed-list (cons group gnus-killed-list)))))))
	 gnus-active-hashtb)
	(while groups
	  (if (gnus-active (car groups))
	      (gnus-group-change-level
	       (car groups) gnus-level-default-subscribed gnus-level-killed))
	  (setq groups (cdr groups)))
	(gnus-group-make-help-group)
	(and gnus-novice-user
	     (gnus-message 7 "`A k' to list killed groups"))))))

(defun gnus-subscribe-group (group previous &optional method)
  (gnus-group-change-level
   (if method
       (list t group gnus-level-default-subscribed nil nil method)
     group)
   gnus-level-default-subscribed gnus-level-killed previous t))

;; `gnus-group-change-level' is the fundamental function for changing
;; subscription levels of newsgroups.  This might mean just changing
;; from level 1 to 2, which is pretty trivial, from 2 to 6 or back
;; again, which subscribes/unsubscribes a group, which is equally
;; trivial.  Changing from 1-7 to 8-9 means that you kill a group, and
;; from 8-9 to 1-7 means that you remove the group from the list of
;; killed (or zombie) groups and add them to the (kinda) subscribed
;; groups.  And last but not least, moving from 8 to 9 and 9 to 8,
;; which is trivial.
;; ENTRY can either be a string (newsgroup name) or a list (if
;; FROMKILLED is t, it's a list on the format (NUM INFO-LIST),
;; otherwise it's a list in the format of the `gnus-newsrc-hashtb'
;; entries.
;; LEVEL is the new level of the group, OLDLEVEL is the old level and
;; PREVIOUS is the group (in hashtb entry format) to insert this group
;; after.
(defun gnus-group-change-level (entry level &optional oldlevel
				      previous fromkilled)
  (let (group info active num)
    ;; Glean what info we can from the arguments
    (if (consp entry)
	(if fromkilled (setq group (nth 1 entry))
	  (setq group (car (nth 2 entry))))
      (setq group entry))
    (if (and (stringp entry)
	     oldlevel
	     (< oldlevel gnus-level-zombie))
	(setq entry (gnus-gethash entry gnus-newsrc-hashtb)))
    (if (and (not oldlevel)
	     (consp entry))
	(setq oldlevel (gnus-info-level (nth 2 entry)))
      (setq oldlevel (or oldlevel 9)))
    (if (stringp previous)
	(setq previous (gnus-gethash previous gnus-newsrc-hashtb)))

    (if (and (>= oldlevel gnus-level-zombie)
	     (gnus-gethash group gnus-newsrc-hashtb))
	;; We are trying to subscribe a group that is already
	;; subscribed.
	()				; Do nothing.

      (or (gnus-ephemeral-group-p group)
	  (gnus-dribble-enter
	   (format "(gnus-group-change-level %S %S %S %S %S)"
		   group level oldlevel (car (nth 2 previous)) fromkilled)))

      ;; Then we remove the newgroup from any old structures, if needed.
      ;; If the group was killed, we remove it from the killed or zombie
      ;; list.  If not, and it is in fact going to be killed, we remove
      ;; it from the newsrc hash table and assoc.
      (cond
       ((>= oldlevel gnus-level-zombie)
	(if (= oldlevel gnus-level-zombie)
	    (setq gnus-zombie-list (delete group gnus-zombie-list))
	  (setq gnus-killed-list (delete group gnus-killed-list))))
       (t
	(if (and (>= level gnus-level-zombie)
		 entry)
	    (progn
	      (gnus-sethash (car (nth 2 entry)) nil gnus-newsrc-hashtb)
	      (if (nth 3 entry)
		  (setcdr (gnus-gethash (car (nth 3 entry))
					gnus-newsrc-hashtb)
			  (cdr entry)))
	      (setcdr (cdr entry) (cdddr entry))))))

      ;; Finally we enter (if needed) the list where it is supposed to
      ;; go, and change the subscription level.  If it is to be killed,
      ;; we enter it into the killed or zombie list.
      (cond 
       ((>= level gnus-level-zombie)
	;; Remove from the hash table.
	(gnus-sethash group nil gnus-newsrc-hashtb)
	;; We do not enter foreign groups into the list of dead
	;; groups.
	(unless (gnus-group-foreign-p group)
	  (if (= level gnus-level-zombie)
	      (setq gnus-zombie-list (cons group gnus-zombie-list))
	    (setq gnus-killed-list (cons group gnus-killed-list)))))
       (t
	;; If the list is to be entered into the newsrc assoc, and
	;; it was killed, we have to create an entry in the newsrc
	;; hashtb format and fix the pointers in the newsrc assoc.
	(if (< oldlevel gnus-level-zombie)
	    ;; It was alive, and it is going to stay alive, so we
	    ;; just change the level and don't change any pointers or
	    ;; hash table entries.
	    (setcar (cdaddr entry) level)
	  (if (listp entry)
	      (setq info (cdr entry)
		    num (car entry))
	    (setq active (gnus-active group))
	    (setq num
		  (if active (- (1+ (cdr active)) (car active)) t))
	    ;; Check whether the group is foreign.  If so, the
	    ;; foreign select method has to be entered into the
	    ;; info.
	    (let ((method (or gnus-override-subscribe-method
			      (gnus-group-method group))))
	      (if (eq method gnus-select-method)
		  (setq info (list group level nil))
		(setq info (list group level nil nil method)))))
	  (unless previous
	    (setq previous
		  (let ((p gnus-newsrc-alist))
		    (while (cddr p)
		      (setq p (cdr p)))
		    p)))
	  (setq entry (cons info (cddr previous)))
	  (if (cdr previous)
	      (progn
		(setcdr (cdr previous) entry)
		(gnus-sethash group (cons num (cdr previous))
			      gnus-newsrc-hashtb))
	    (setcdr previous entry)
	    (gnus-sethash group (cons num previous)
			  gnus-newsrc-hashtb))
	  (when (cdr entry)
	    (setcdr (gnus-gethash (caadr entry) gnus-newsrc-hashtb) entry)))))
      (when gnus-group-change-level-function
	(funcall gnus-group-change-level-function group level oldlevel)))))

(defun gnus-kill-newsgroup (newsgroup)
  "Obsolete function.  Kills a newsgroup."
  (gnus-group-change-level
   (gnus-gethash newsgroup gnus-newsrc-hashtb) gnus-level-killed))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Remove bogus newsgroups.
If CONFIRM is non-nil, the user has to confirm the deletion of every
newsgroup."
  (let ((newsrc (cdr gnus-newsrc-alist))
	bogus group entry info)
    (gnus-message 5 "Checking bogus newsgroups...")
    (unless (gnus-read-active-file-p)
      (gnus-read-active-file))
    (when (gnus-read-active-file-p)
      ;; Find all bogus newsgroup that are subscribed.
      (while newsrc
	(setq info (pop newsrc)
	      group (gnus-info-group info))
	(unless (or (gnus-active group)	; Active
		    (gnus-info-method info) ; Foreign
		    (and confirm
			 (not (gnus-y-or-n-p
			       (format "Remove bogus newsgroup: %s " group)))))
	  ;; Found a bogus newsgroup.
	  (push group bogus)))
      ;; Remove all bogus subscribed groups by first killing them, and
      ;; then removing them from the list of killed groups.
      (while bogus
	(when (setq entry (gnus-gethash (setq group (pop bogus))
					gnus-newsrc-hashtb))
	  (gnus-group-change-level entry gnus-level-killed)
	  (setq gnus-killed-list (delete group gnus-killed-list))))
      ;; Then we remove all bogus groups from the list of killed and
      ;; zombie groups.  They are are removed without confirmation.
      (let ((dead-lists '(gnus-killed-list gnus-zombie-list))
	    killed)
	(while dead-lists
	  (setq killed (symbol-value (car dead-lists)))
	  (while killed
	    (unless (gnus-active (setq group (pop killed)))
	      ;; The group is bogus.
	      ;; !!!Slow as hell.
	      (set (car dead-lists)
		   (delete group (symbol-value (car dead-lists))))))
	  (setq dead-lists (cdr dead-lists))))
      (gnus-message 5 "Checking bogus newsgroups...done"))))

(defun gnus-check-duplicate-killed-groups ()
  "Remove duplicates from the list of killed groups."
  (interactive)
  (let ((killed gnus-killed-list))
    (while killed
      (gnus-message 9 "%d" (length killed))
      (setcdr killed (delete (car killed) (cdr killed)))
      (setq killed (cdr killed)))))

;; We want to inline a function from gnus-cache, so we cheat here:
(eval-when-compile
  (provide 'gnus)
  (require 'gnus-cache))

(defun gnus-get-unread-articles-in-group (info active &optional update)
  (when active
    ;; Allow the backend to update the info in the group.
    (when (and update 
	       (gnus-request-update-info
		info (gnus-find-method-for-group (gnus-info-group info))))
      (gnus-activate-group (gnus-info-group info) nil t))
    (let* ((range (gnus-info-read info))
	   (num 0))
      ;; If a cache is present, we may have to alter the active info.
      (when (and gnus-use-cache info)
	(inline (gnus-cache-possibly-alter-active 
		 (gnus-info-group info) active)))
      ;; Modify the list of read articles according to what articles
      ;; are available; then tally the unread articles and add the
      ;; number to the group hash table entry.
      (cond
       ((zerop (cdr active))
	(setq num 0))
       ((not range)
	(setq num (- (1+ (cdr active)) (car active))))
       ((not (listp (cdr range)))
	;; Fix a single (num . num) range according to the
	;; active hash table.
	;; Fix by Carsten Bormann <cabo@Informatik.Uni-Bremen.DE>.
	(and (< (cdr range) (car active)) (setcdr range (1- (car active))))
	(and (> (cdr range) (cdr active)) (setcdr range (cdr active)))
	;; Compute number of unread articles.
	(setq num (max 0 (- (cdr active) (- (1+ (cdr range)) (car range))))))
       (t
	;; The read list is a list of ranges.  Fix them according to
	;; the active hash table.
	;; First peel off any elements that are below the lower
	;; active limit.
	(while (and (cdr range)
		    (>= (car active)
			(or (and (atom (cadr range)) (cadr range))
			    (caadr range))))
	  (if (numberp (car range))
	      (setcar range
		      (cons (car range)
			    (or (and (numberp (cadr range))
				     (cadr range))
				(cdadr range))))
	    (setcdr (car range)
		    (or (and (numberp (nth 1 range)) (nth 1 range))
			(cdadr range))))
	  (setcdr range (cddr range)))
	;; Adjust the first element to be the same as the lower limit.
	(if (and (not (atom (car range)))
		 (< (cdar range) (car active)))
	    (setcdr (car range) (1- (car active))))
	;; Then we want to peel off any elements that are higher
	;; than the upper active limit.
	(let ((srange range))
	  ;; Go past all legal elements.
	  (while (and (cdr srange)
		      (<= (or (and (atom (cadr srange))
				   (cadr srange))
			      (caadr srange)) (cdr active)))
	    (setq srange (cdr srange)))
	  (if (cdr srange)
	      ;; Nuke all remaining illegal elements.
	      (setcdr srange nil))

	  ;; Adjust the final element.
	  (if (and (not (atom (car srange)))
		   (> (cdar srange) (cdr active)))
	      (setcdr (car srange) (cdr active))))
	;; Compute the number of unread articles.
	(while range
	  (setq num (+ num (- (1+ (or (and (atom (car range)) (car range))
				      (cdar range)))
			      (or (and (atom (car range)) (car range))
				  (caar range)))))
	  (setq range (cdr range)))
	(setq num (max 0 (- (cdr active) num)))))
      ;; Set the number of unread articles.
      (when info
	(setcar (gnus-gethash (gnus-info-group info) gnus-newsrc-hashtb) num))
      num)))

;; Go though `gnus-newsrc-alist' and compare with `gnus-active-hashtb'
;; and compute how many unread articles there are in each group.
(defun gnus-get-unread-articles (&optional level)
  (let* ((newsrc (cdr gnus-newsrc-alist))
	 (level (or level gnus-activate-level (1+ gnus-level-subscribed)))
	 (foreign-level
	  (min
	   (cond ((and gnus-activate-foreign-newsgroups
		       (not (numberp gnus-activate-foreign-newsgroups)))
		  (1+ gnus-level-subscribed))
		 ((numberp gnus-activate-foreign-newsgroups)
		  gnus-activate-foreign-newsgroups)
		 (t 0))
	   level))
	 info group active method)
    (gnus-message 5 "Checking new news...")

    (while newsrc
      (setq active (gnus-active (setq group (gnus-info-group
					     (setq info (pop newsrc))))))

      ;; Check newsgroups.  If the user doesn't want to check them, or
      ;; they can't be checked (for instance, if the news server can't
      ;; be reached) we just set the number of unread articles in this
      ;; newsgroup to t.  This means that Gnus thinks that there are
      ;; unread articles, but it has no idea how many.
      (if (and (setq method (gnus-info-method info))
	       (not (gnus-server-equal
		     gnus-select-method
		     (setq method (gnus-server-get-method nil method))))
	       (not (gnus-secondary-method-p method)))
	  ;; These groups are foreign.  Check the level.
	  (when (<= (gnus-info-level info) foreign-level)
	    (setq active (gnus-activate-group group 'scan))
	    (unless (inline (gnus-virtual-group-p group))
	      (inline (gnus-close-group group)))
	    (when (fboundp (intern (concat (symbol-name (car method))
					   "-request-update-info")))
	      (inline (gnus-request-update-info info method))))
	;; These groups are native or secondary.
	(when (and (<= (gnus-info-level info) level)
		   (not gnus-read-active-file))
	  (setq active (gnus-activate-group group 'scan))
	  (inline (gnus-close-group group))))

      ;; Get the number of unread articles in the group.
      (if active
	  (inline (gnus-get-unread-articles-in-group info active))
	;; The group couldn't be reached, so we nix out the number of
	;; unread articles and stuff.
	(gnus-set-active group nil)
	(setcar (gnus-gethash group gnus-newsrc-hashtb) t)))

    (gnus-message 5 "Checking new news...done")))

;; Create a hash table out of the newsrc alist.  The `car's of the
;; alist elements are used as keys.
(defun gnus-make-hashtable-from-newsrc-alist ()
  (let ((alist gnus-newsrc-alist)
	(ohashtb gnus-newsrc-hashtb)
	prev)
    (setq gnus-newsrc-hashtb (gnus-make-hashtable (length alist)))
    (setq alist
	  (setq prev (setq gnus-newsrc-alist
			   (if (equal (caar gnus-newsrc-alist)
				      "dummy.group")
			       gnus-newsrc-alist
			     (cons (list "dummy.group" 0 nil) alist)))))
    (while alist
      (gnus-sethash
       (caar alist)
       (cons (and ohashtb (car (gnus-gethash (caar alist) ohashtb)))
	     prev)
       gnus-newsrc-hashtb)
      (setq prev alist
	    alist (cdr alist)))))

(defun gnus-make-hashtable-from-killed ()
  "Create a hash table from the killed and zombie lists."
  (let ((lists '(gnus-killed-list gnus-zombie-list))
	list)
    (setq gnus-killed-hashtb
	  (gnus-make-hashtable
	   (+ (length gnus-killed-list) (length gnus-zombie-list))))
    (while (setq list (pop lists))
      (setq list (symbol-value list))
      (while list
	(gnus-sethash (car list) (pop list) gnus-killed-hashtb)))))

(defun gnus-activate-group (group &optional scan dont-check method)
  ;; Check whether a group has been activated or not.
  ;; If SCAN, request a scan of that group as well.
  (let ((method (or method (gnus-find-method-for-group group)))
	active)
    (and (gnus-check-server method)
	 ;; We escape all bugs and quit here to make it possible to
	 ;; continue if a group is so out-there that it reports bugs
	 ;; and stuff.
	 (progn
	   (and scan
		(gnus-check-backend-function 'request-scan (car method))
		(gnus-request-scan group method))
	   t)
	 (condition-case ()
	     (gnus-request-group group dont-check method)
	;   (error nil)
	   (quit nil))
	 (save-excursion
	   (set-buffer nntp-server-buffer)
	   (goto-char (point-min))
	   ;; Parse the result we got from `gnus-request-group'.
	   (and (looking-at "[0-9]+ [0-9]+ \\([0-9]+\\) [0-9]+")
		(progn
		  (goto-char (match-beginning 1))
		  (gnus-set-active
		   group (setq active (cons (read (current-buffer))
					    (read (current-buffer)))))
		  ;; Return the new active info.
		  active))))))

(defun gnus-update-read-articles (group unread)
  "Update the list of read and ticked articles in GROUP using the
UNREAD and TICKED lists.
Note: UNSELECTED has to be sorted over `<'.
Returns whether the updating was successful."
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
	(if (/= (car unread) prev)
	    (setq read (cons (if (= prev (1- (car unread))) prev
			       (cons prev (1- (car unread)))) read)))
	(setq prev (1+ (car unread)))
	(setq unread (cdr unread)))
      (when (<= prev (cdr active))
	(setq read (cons (cons prev (cdr active)) read)))
      ;; Enter this list into the group info.
      (gnus-info-set-read
       info (if (> (length read) 1) (nreverse read) read))
      ;; Set the number of unread articles in gnus-newsrc-hashtb.
      (gnus-get-unread-articles-in-group info (gnus-active group))
      t)))

(defun gnus-make-articles-unread (group articles)
  "Mark ARTICLES in GROUP as unread."
  (let* ((info (nth 2 (or (gnus-gethash group gnus-newsrc-hashtb)
			  (gnus-gethash (gnus-group-real-name group)
					gnus-newsrc-hashtb))))
	 (ranges (gnus-info-read info))
	 news article)
    (while articles
      (when (gnus-member-of-range
	     (setq article (pop articles)) ranges)
	(setq news (cons article news))))
    (when news
      (gnus-info-set-read
       info (gnus-remove-from-range (gnus-info-read info) (nreverse news)))
      (gnus-group-update-group group t))))

;; Enter all dead groups into the hashtb.
(defun gnus-update-active-hashtb-from-killed ()
  (let ((hashtb (setq gnus-active-hashtb (make-vector 4095 0)))
	(lists (list gnus-killed-list gnus-zombie-list))
	killed)
    (while lists
      (setq killed (car lists))
      (while killed
	(gnus-sethash (car killed) nil hashtb)
	(setq killed (cdr killed)))
      (setq lists (cdr lists)))))

(defun gnus-get-killed-groups ()
  "Go through the active hashtb and all all unknown groups as killed."
  ;; First make sure active file has been read.
  (unless (gnus-read-active-file-p)
    (let ((gnus-read-active-file t))
      (gnus-read-active-file)))
  (or gnus-killed-hashtb (gnus-make-hashtable-from-killed))
  ;; Go through all newsgroups that are known to Gnus - enlarge kill list.
  (mapatoms
   (lambda (sym)
     (let ((groups 0)
	   (group (symbol-name sym)))
       (if (or (null group)
	       (gnus-gethash group gnus-killed-hashtb)
	       (gnus-gethash group gnus-newsrc-hashtb))
	   ()
	 (let ((do-sub (gnus-matches-options-n group)))
	   (if (or (eq do-sub 'subscribe) (eq do-sub 'ignore))
	       ()
	     (setq groups (1+ groups))
	     (setq gnus-killed-list
		   (cons group gnus-killed-list))
	     (gnus-sethash group group gnus-killed-hashtb))))))
   gnus-active-hashtb))

;; Get the active file(s) from the backend(s).
(defun gnus-read-active-file ()
  (gnus-group-set-mode-line)
  (let ((methods 
	 (append
	  (if (gnus-check-server gnus-select-method)
	      ;; The native server is available.
	      (cons gnus-select-method gnus-secondary-select-methods)
	    ;; The native server is down, so we just do the
	    ;; secondary ones.
	    gnus-secondary-select-methods)
	  ;; Also read from the archive server.
	  (when (gnus-archive-server-wanted-p)
	    (list "archive"))))
	list-type)
    (setq gnus-have-read-active-file nil)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (while methods
	(let* ((method (if (stringp (car methods))
			   (gnus-server-get-method nil (car methods))
			 (car methods)))
	       (where (nth 1 method))
	       (mesg (format "Reading active file%s via %s..."
			     (if (and where (not (zerop (length where))))
				 (concat " from " where) "")
			     (car method))))
	  (gnus-message 5 mesg)
	  (when (gnus-check-server method)
	    ;; Request that the backend scan its incoming messages.
	    (and (gnus-check-backend-function 'request-scan (car method))
		 (gnus-request-scan nil method))
	    (cond
	     ((and (eq gnus-read-active-file 'some)
		   (gnus-check-backend-function 'retrieve-groups (car method)))
	      (let ((newsrc (cdr gnus-newsrc-alist))
		    (gmethod (gnus-server-get-method nil method))
		    groups info)
		(while (setq info (pop newsrc))
		  (when (gnus-server-equal
			 (gnus-find-method-for-group 
			  (gnus-info-group info) info)
			 gmethod)
		    (push (gnus-group-real-name (gnus-info-group info)) 
			  groups)))
		(when groups
		  (gnus-check-server method)
		  (setq list-type (gnus-retrieve-groups groups method))
		  (cond
		   ((not list-type)
		    (gnus-error
		     1.2 "Cannot read partial active file from %s server."
		     (car method)))
		   ((eq list-type 'active)
		    (gnus-active-to-gnus-format method gnus-active-hashtb))
		   (t
		    (gnus-groups-to-gnus-format method gnus-active-hashtb))))))
	     (t
	      (if (not (gnus-request-list method))
		  (unless (equal method gnus-message-archive-method)
		    (gnus-error 1 "Cannot read active file from %s server."
				(car method)))
		(gnus-message 5 mesg)
		(gnus-active-to-gnus-format method gnus-active-hashtb)
		;; We mark this active file as read.
		(push method gnus-have-read-active-file)
		(gnus-message 5 "%sdone" mesg))))))
	(setq methods (cdr methods))))))

;; Read an active file and place the results in `gnus-active-hashtb'.
(defun gnus-active-to-gnus-format (&optional method hashtb ignore-errors)
  (unless method
    (setq method gnus-select-method))
  (let ((cur (current-buffer))
	(hashtb (or hashtb
		    (if (and gnus-active-hashtb
			     (not (equal method gnus-select-method)))
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (if (equal method gnus-select-method)
				(gnus-make-hashtable
				 (count-lines (point-min) (point-max)))
			      (gnus-make-hashtable 4096)))))))
    ;; Delete unnecessary lines.
    (goto-char (point-min))
    (while (search-forward "\nto." nil t)
      (delete-region (1+ (match-beginning 0))
		     (progn (forward-line 1) (point))))
    (or (string= gnus-ignored-newsgroups "")
	(progn
	  (goto-char (point-min))
	  (delete-matching-lines gnus-ignored-newsgroups)))
    ;; Make the group names readable as a lisp expression even if they
    ;; contain special characters.
    ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
    (goto-char (point-max))
    (while (re-search-backward "[][';?()#]" nil t)
      (insert ?\\))
    ;; If these are groups from a foreign select method, we insert the
    ;; group prefix in front of the group names.
    (and method (not (gnus-server-equal
		      (gnus-server-get-method nil method)
		      (gnus-server-get-method nil gnus-select-method)))
	 (let ((prefix (gnus-group-prefixed-name "" method)))
	   (goto-char (point-min))
	   (while (and (not (eobp))
		       (progn (insert prefix)
			      (zerop (forward-line 1)))))))
    ;; Store the active file in a hash table.
    (goto-char (point-min))
    (if (string-match "%[oO]" gnus-group-line-format)
	;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
	;; If we want information on moderated groups, we use this
	;; loop...
	(let* ((mod-hashtb (make-vector 7 0))
	       (m (intern "m" mod-hashtb))
	       group max min)
	  (while (not (eobp))
	    (condition-case nil
		(progn
		  (narrow-to-region (point) (gnus-point-at-eol))
		  (setq group (let ((obarray hashtb)) (read cur)))
		  (if (and (numberp (setq max (read cur)))
			   (numberp (setq min (read cur)))
			   (progn
			     (skip-chars-forward " \t")
			     (not
			      (or (= (following-char) ?=)
				  (= (following-char) ?x)
				  (= (following-char) ?j)))))
		      (set group (cons min max))
		    (set group nil))
		  ;; Enter moderated groups into a list.
		  (if (eq (let ((obarray mod-hashtb)) (read cur)) m)
		      (setq gnus-moderated-list
			    (cons (symbol-name group) gnus-moderated-list))))
	      (error
	       (and group
		    (symbolp group)
		    (set group nil))))
	    (widen)
	    (forward-line 1)))
      ;; And if we do not care about moderation, we use this loop,
      ;; which is faster.
      (let (group max min)
	(while (not (eobp))
	  (condition-case ()
	      (progn
		(narrow-to-region (point) (gnus-point-at-eol))
		;; group gets set to a symbol interned in the hash table
		;; (what a hack!!) - jwz
		(setq group (let ((obarray hashtb)) (read cur)))
		(if (and (numberp (setq max (read cur)))
			 (numberp (setq min (read cur)))
			 (progn
			   (skip-chars-forward " \t")
			   (not
			    (or (= (following-char) ?=)
				(= (following-char) ?x)
				(= (following-char) ?j)))))
		    (set group (cons min max))
		  (set group nil)))
	    (error
	     (progn
	       (and group
		    (symbolp group)
		    (set group nil))
	       (or ignore-errors
		   (gnus-message 3 "Warning - illegal active: %s"
				 (buffer-substring
				  (gnus-point-at-bol) (gnus-point-at-eol)))))))
	  (widen)
	  (forward-line 1))))))

(defun gnus-groups-to-gnus-format (method &optional hashtb)
  ;; Parse a "groups" active file.
  (let ((cur (current-buffer))
	(hashtb (or hashtb
		    (if (and method gnus-active-hashtb)
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (gnus-make-hashtable
			     (count-lines (point-min) (point-max)))))))
	(prefix (and method
		     (not (gnus-server-equal
			   (gnus-server-get-method nil method)
			   (gnus-server-get-method nil gnus-select-method)))
		     (gnus-group-prefixed-name "" method))))

    (goto-char (point-min))
    ;; We split this into to separate loops, one with the prefix
    ;; and one without to speed the reading up somewhat.
    (if prefix
	(let (min max opoint group)
	  (while (not (eobp))
	    (condition-case ()
		(progn
		  (read cur) (read cur)
		  (setq min (read cur)
			max (read cur)
			opoint (point))
		  (skip-chars-forward " \t")
		  (insert prefix)
		  (goto-char opoint)
		  (set (let ((obarray hashtb)) (read cur))
		       (cons min max)))
	      (error (and group (symbolp group) (set group nil))))
	    (forward-line 1)))
      (let (min max group)
	(while (not (eobp))
	  (condition-case ()
	      (if (= (following-char) ?2)
		  (progn
		    (read cur) (read cur)
		    (setq min (read cur)
			  max (read cur))
		    (set (setq group (let ((obarray hashtb)) (read cur)))
			 (cons min max))))
	    (error (and group (symbolp group) (set group nil))))
	  (forward-line 1))))))

(defun gnus-read-newsrc-file (&optional force)
  "Read startup file.
If FORCE is non-nil, the .newsrc file is read."
  ;; Reset variables that might be defined in the .newsrc.eld file.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el")))
    (save-excursion
      ;; We always load the .newsrc.eld file.  If always contains
      ;; much information that can not be gotten from the .newsrc
      ;; file (ticked articles, killed groups, foreign methods, etc.)
      (gnus-read-newsrc-el-file quick-file)

      (if (and (file-exists-p gnus-current-startup-file)
	       (or force
		   (and (file-newer-than-file-p newsrc-file quick-file)
			(file-newer-than-file-p newsrc-file
						(concat quick-file "d")))
		   (not gnus-newsrc-alist)))
	  ;; We read the .newsrc file.  Note that if there if a
	  ;; .newsrc.eld file exists, it has already been read, and
	  ;; the `gnus-newsrc-hashtb' has been created.  While reading
	  ;; the .newsrc file, Gnus will only use the information it
	  ;; can find there for changing the data already read -
	  ;; ie. reading the .newsrc file will not trash the data
	  ;; already read (except for read articles).
	  (save-excursion
	    (gnus-message 5 "Reading %s..." newsrc-file)
	    (set-buffer (find-file-noselect newsrc-file))
	    (buffer-disable-undo (current-buffer))
	    (gnus-newsrc-to-gnus-format)
	    (kill-buffer (current-buffer))
	    (gnus-message 5 "Reading %s...done" newsrc-file)))

      ;; Read any slave files.
      (unless gnus-slave
	(gnus-master-read-slave-newsrc))
      
      ;; Convert old to new.
      (gnus-convert-old-newsrc))))

(defun gnus-continuum-version (version)
  "Return VERSION as a floating point number."
  (when (or (string-match "^\\([^ ]+\\)? ?Gnus v?\\([0-9.]+\\)$" version)
	    (string-match "^\\(.?\\)gnus-\\([0-9.]+\\)$" version))
    (let* ((alpha (and (match-beginning 1) (match-string 1 version)))
	   (number (match-string 2 version))
	   major minor least)
      (string-match "\\([0-9]\\)\\.\\([0-9]+\\)\\.?\\([0-9]+\\)?" number)
      (setq major (string-to-number (match-string 1 number)))
      (setq minor (string-to-number (match-string 2 number)))
      (setq least (if (match-beginning 3)
		      (string-to-number (match-string 3 number))
		    0))
      (string-to-number
       (if (zerop major)
	   (format "%s00%02d%02d"
		   (cond 
		    ((member alpha '("(ding)" "d")) "4.99")
		    ((member alpha '("September" "s")) "5.01")
		    ((member alpha '("Red" "r")) "5.03"))
		   minor least)
	 (format "%d.%02d%02d" major minor least))))))

(defun gnus-convert-old-newsrc ()
  "Convert old newsrc into the new format, if needed."
  (let ((fcv (and gnus-newsrc-file-version
		  (gnus-continuum-version gnus-newsrc-file-version))))
    (cond
     ;; No .newsrc.eld file was loaded.
     ((null fcv) nil)
     ;; Gnus 5 .newsrc.eld was loaded.
     ((< fcv (gnus-continuum-version "September Gnus v0.1"))
      (gnus-convert-old-ticks)))))

(defun gnus-convert-old-ticks ()
  (let ((newsrc (cdr gnus-newsrc-alist))
	marks info dormant ticked)
    (while (setq info (pop newsrc))
      (when (setq marks (gnus-info-marks info))
	(setq dormant (cdr (assq 'dormant marks))
	      ticked (cdr (assq 'tick marks)))
	(when (or dormant ticked)
	  (gnus-info-set-read
	   info
	   (gnus-add-to-range
	    (gnus-info-read info)
	    (nconc (gnus-uncompress-range dormant)
		   (gnus-uncompress-range ticked)))))))))

(defun gnus-read-newsrc-el-file (file)
  (let ((ding-file (concat file "d")))
    ;; We always, always read the .eld file.
    (gnus-message 5 "Reading %s..." ding-file)
    (let (gnus-newsrc-assoc)
      (condition-case nil
	  (load ding-file t t t)
	(error
	 (gnus-error 1 "Error in %s" ding-file)))
      (when gnus-newsrc-assoc
	(setq gnus-newsrc-alist gnus-newsrc-assoc)))
    (gnus-make-hashtable-from-newsrc-alist)
    (when (file-newer-than-file-p file ding-file)
      ;; Old format quick file
      (gnus-message 5 "Reading %s..." file)
      ;; The .el file is newer than the .eld file, so we read that one
      ;; as well.
      (gnus-read-old-newsrc-el-file file))))

;; Parse the old-style quick startup file
(defun gnus-read-old-newsrc-el-file (file)
  (let (newsrc killed marked group m info)
    (prog1
	(let ((gnus-killed-assoc nil)
	      gnus-marked-assoc gnus-newsrc-alist gnus-newsrc-assoc)
	  (prog1
	      (condition-case nil
		  (load file t t t)
		(error nil))
	    (setq newsrc gnus-newsrc-assoc
		  killed gnus-killed-assoc
		  marked gnus-marked-assoc)))
      (setq gnus-newsrc-alist nil)
      (while (setq group (pop newsrc))
	(if (setq info (gnus-get-info (car group)))
	    (progn
	      (gnus-info-set-read info (cddr group))
	      (gnus-info-set-level
	       info (if (nth 1 group) gnus-level-default-subscribed
		      gnus-level-default-unsubscribed))
	      (setq gnus-newsrc-alist (cons info gnus-newsrc-alist)))
	  (push (setq info
		      (list (car group)
			    (if (nth 1 group) gnus-level-default-subscribed
			      gnus-level-default-unsubscribed)
			    (cddr group)))
		gnus-newsrc-alist))
	;; Copy marks into info.
	(when (setq m (assoc (car group) marked))
	  (unless (nthcdr 3 info)
	    (nconc info (list nil)))
	  (gnus-info-set-marks
	   info (list (cons 'tick (gnus-compress-sequence 
				   (sort (cdr m) '<) t))))))
      (setq newsrc killed)
      (while newsrc
	(setcar newsrc (caar newsrc))
	(setq newsrc (cdr newsrc)))
      (setq gnus-killed-list killed))
    ;; The .el file version of this variable does not begin with
    ;; "options", while the .eld version does, so we just add it if it
    ;; isn't there.
    (and
     gnus-newsrc-options
     (progn
       (and (not (string-match "^ *options" gnus-newsrc-options))
	    (setq gnus-newsrc-options (concat "options " gnus-newsrc-options)))
       (and (not (string-match "\n$" gnus-newsrc-options))
	    (setq gnus-newsrc-options (concat gnus-newsrc-options "\n")))
       ;; Finally, if we read some options lines, we parse them.
       (or (string= gnus-newsrc-options "")
	   (gnus-newsrc-parse-options gnus-newsrc-options))))

    (setq gnus-newsrc-alist (nreverse gnus-newsrc-alist))
    (gnus-make-hashtable-from-newsrc-alist)))

(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" (nth 1 gnus-select-method))))
    (if (or (file-exists-p real-file)
	    (file-exists-p (concat real-file ".el"))
	    (file-exists-p (concat real-file ".eld")))
	real-file file)))

(defun gnus-newsrc-to-gnus-format ()
  (setq gnus-newsrc-options "")
  (setq gnus-newsrc-options-n nil)

  (or gnus-active-hashtb
      (setq gnus-active-hashtb (make-vector 4095 0)))
  (let ((buf (current-buffer))
	(already-read (> (length gnus-newsrc-alist) 1))
	group subscribed options-symbol newsrc Options-symbol
	symbol reads num1)
    (goto-char (point-min))
    ;; We intern the symbol `options' in the active hashtb so that we
    ;; can `eq' against it later.
    (set (setq options-symbol (intern "options" gnus-active-hashtb)) nil)
    (set (setq Options-symbol (intern "Options" gnus-active-hashtb)) nil)

    (while (not (eobp))
      ;; We first read the first word on the line by narrowing and
      ;; then reading into `gnus-active-hashtb'.  Most groups will
      ;; already exist in that hashtb, so this will save some string
      ;; space.
      (narrow-to-region
       (point)
       (progn (skip-chars-forward "^ \t!:\n") (point)))
      (goto-char (point-min))
      (setq symbol
	    (and (/= (point-min) (point-max))
		 (let ((obarray gnus-active-hashtb)) (read buf))))
      (widen)
      ;; Now, the symbol we have read is either `options' or a group
      ;; name.  If it is an options line, we just add it to a string.
      (cond
       ((or (eq symbol options-symbol)
	    (eq symbol Options-symbol))
	(setq gnus-newsrc-options
	      ;; This concating is quite inefficient, but since our
	      ;; thorough studies show that approx 99.37% of all
	      ;; .newsrc files only contain a single options line, we
	      ;; don't give a damn, frankly, my dear.
	      (concat gnus-newsrc-options
		      (buffer-substring
		       (gnus-point-at-bol)
		       ;; Options may continue on the next line.
		       (or (and (re-search-forward "^[^ \t]" nil 'move)
				(progn (beginning-of-line) (point)))
			   (point)))))
	(forward-line -1))
       (symbol
	;; Group names can be just numbers.  
	(when (numberp symbol) 
	  (setq symbol (intern (int-to-string symbol) gnus-active-hashtb)))
	(or (boundp symbol) (set symbol nil))
	;; It was a group name.
	(setq subscribed (= (following-char) ?:)
	      group (symbol-name symbol)
	      reads nil)
	(if (eolp)
	    ;; If the line ends here, this is clearly a buggy line, so
	    ;; we put point a the beginning of line and let the cond
	    ;; below do the error handling.
	    (beginning-of-line)
	  ;; We skip to the beginning of the ranges.
	  (skip-chars-forward "!: \t"))
	;; We are now at the beginning of the list of read articles.
	;; We read them range by range.
	(while
	    (cond
	     ((looking-at "[0-9]+")
	      ;; We narrow and read a number instead of buffer-substring/
	      ;; string-to-int because it's faster.  narrow/widen is
	      ;; faster than save-restriction/narrow, and save-restriction
	      ;; produces a garbage object.
	      (setq num1 (progn
			   (narrow-to-region (match-beginning 0) (match-end 0))
			   (read buf)))
	      (widen)
	      ;; If the next character is a dash, then this is a range.
	      (if (= (following-char) ?-)
		  (progn
		    ;; We read the upper bound of the range.
		    (forward-char 1)
		    (if (not (looking-at "[0-9]+"))
			;; This is a buggy line, by we pretend that
			;; it's kinda OK.  Perhaps the user should be
			;; dinged?
			(setq reads (cons num1 reads))
		      (setq reads
			    (cons
			     (cons num1
				   (progn
				     (narrow-to-region (match-beginning 0)
						       (match-end 0))
				     (read buf)))
			     reads))
		      (widen)))
		;; It was just a simple number, so we add it to the
		;; list of ranges.
		(setq reads (cons num1 reads)))
	      ;; If the next char in ?\n, then we have reached the end
	      ;; of the line and return nil.
	      (/= (following-char) ?\n))
	     ((= (following-char) ?\n)
	      ;; End of line, so we end.
	      nil)
	     (t
	      ;; Not numbers and not eol, so this might be a buggy
	      ;; line...
	      (or (eobp)
		  ;; If it was eob instead of ?\n, we allow it.
		  (progn
		    ;; The line was buggy.
		    (setq group nil)
		    (gnus-error 3.1 "Mangled line: %s"
				(buffer-substring (gnus-point-at-bol)
						  (gnus-point-at-eol)))))
	      nil))
	  ;; Skip past ", ".  Spaces are illegal in these ranges, but
	  ;; we allow them, because it's a common mistake to put a
	  ;; space after the comma.
	  (skip-chars-forward ", "))

	;; We have already read .newsrc.eld, so we gently update the
	;; data in the hash table with the information we have just
	;; read.
	(when group
	  (let ((info (gnus-get-info group))
		level)
	    (if info
		;; There is an entry for this file in the alist.
		(progn
		  (gnus-info-set-read info (nreverse reads))
		  ;; We update the level very gently.  In fact, we
		  ;; only change it if there's been a status change
		  ;; from subscribed to unsubscribed, or vice versa.
		  (setq level (gnus-info-level info))
		  (cond ((and (<= level gnus-level-subscribed)
			      (not subscribed))
			 (setq level (if reads
					 gnus-level-default-unsubscribed
				       (1+ gnus-level-default-unsubscribed))))
			((and (> level gnus-level-subscribed) subscribed)
			 (setq level gnus-level-default-subscribed)))
		  (gnus-info-set-level info level))
	      ;; This is a new group.
	      (setq info (list group
			       (if subscribed
				   gnus-level-default-subscribed
				 (if reads
				     (1+ gnus-level-subscribed)
				   gnus-level-default-unsubscribed))
			       (nreverse reads))))
	    (setq newsrc (cons info newsrc))))))
      (forward-line 1))

    (setq newsrc (nreverse newsrc))

    (if (not already-read)
	()
      ;; We now have two newsrc lists - `newsrc', which is what we
      ;; have read from .newsrc, and `gnus-newsrc-alist', which is
      ;; what we've read from .newsrc.eld.  We have to merge these
      ;; lists.  We do this by "attaching" any (foreign) groups in the
      ;; gnus-newsrc-alist to the (native) group that precedes them.
      (let ((rc (cdr gnus-newsrc-alist))
	    (prev gnus-newsrc-alist)
	    entry mentry)
	(while rc
	  (or (null (nth 4 (car rc)))	; It's a native group.
	      (assoc (caar rc) newsrc) ; It's already in the alist.
	      (if (setq entry (assoc (caar prev) newsrc))
		  (setcdr (setq mentry (memq entry newsrc))
			  (cons (car rc) (cdr mentry)))
		(setq newsrc (cons (car rc) newsrc))))
	  (setq prev rc
		rc (cdr rc)))))

    (setq gnus-newsrc-alist newsrc)
    ;; We make the newsrc hashtb.
    (gnus-make-hashtable-from-newsrc-alist)

    ;; Finally, if we read some options lines, we parse them.
    (or (string= gnus-newsrc-options "")
	(gnus-newsrc-parse-options gnus-newsrc-options))))

;; Parse options lines to find "options -n !all rec.all" and stuff.
;; The return value will be a list on the form
;; ((regexp1 . ignore)
;;  (regexp2 . subscribe)...)
;; When handling new newsgroups, groups that match a `ignore' regexp
;; will be ignored, and groups that match a `subscribe' regexp will be
;; subscribed.  A line like
;; options -n !all rec.all
;; will lead to a list that looks like
;; (("^rec\\..+" . subscribe)
;;  ("^.+" . ignore))
;; So all "rec.*" groups will be subscribed, while all the other
;; groups will be ignored.  Note that "options -n !all rec.all" is very
;; different from "options -n rec.all !all".
(defun gnus-newsrc-parse-options (options)
  (let (out eol)
    (save-excursion
      (gnus-set-work-buffer)
      (insert (regexp-quote options))
      ;; First we treat all continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\n[ \t]+" nil t)
	(replace-match " " t t))
      ;; Then we transform all "all"s into ".+"s.
      (goto-char (point-min))
      (while (re-search-forward "\\ball\\b" nil t)
	(replace-match ".+" t t))
      (goto-char (point-min))
      ;; We remove all other options than the "-n" ones.
      (while (re-search-forward "[ \t]-[^n][^-]*" nil t)
	(replace-match " ")
	(forward-char -1))
      (goto-char (point-min))

      ;; We are only interested in "options -n" lines - we
      ;; ignore the other option lines.
      (while (re-search-forward "[ \t]-n" nil t)
	(setq eol
	      (or (save-excursion
		    (and (re-search-forward "[ \t]-n" (gnus-point-at-eol) t)
			 (- (point) 2)))
		  (gnus-point-at-eol)))
	;; Search for all "words"...
	(while (re-search-forward "[^ \t,\n]+" eol t)
	  (if (= (char-after (match-beginning 0)) ?!)
	      ;; If the word begins with a bang (!), this is a "not"
	      ;; spec.  We put this spec (minus the bang) and the
	      ;; symbol `ignore' into the list.
	      (setq out (cons (cons (concat
				     "^" (buffer-substring
					  (1+ (match-beginning 0))
					  (match-end 0)))
				    'ignore) out))
	    ;; There was no bang, so this is a "yes" spec.
	    (setq out (cons (cons (concat "^" (match-string 0))
				  'subscribe) out)))))

      (setq gnus-newsrc-options-n out))))

(defun gnus-save-newsrc-file (&optional force)
  "Save .newsrc file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-alist.
  (when (and (or gnus-newsrc-alist gnus-killed-list)
	     gnus-current-startup-file)
    (save-excursion
      (if (and (or gnus-use-dribble-file gnus-slave)
	       (not force)
	       (or (not gnus-dribble-buffer)
		   (not (buffer-name gnus-dribble-buffer))
		   (zerop (save-excursion
			    (set-buffer gnus-dribble-buffer)
			    (buffer-size)))))
	  (gnus-message 4 "(No changes need to be saved)")
	(run-hooks 'gnus-save-newsrc-hook)
	(if gnus-slave
	    (gnus-slave-save-newsrc)
	  ;; Save .newsrc.
	  (when gnus-save-newsrc-file
	    (gnus-message 5 "Saving %s..." gnus-current-startup-file)
	    (gnus-gnus-to-newsrc-format)
	    (gnus-message 5 "Saving %s...done" gnus-current-startup-file))
	  ;; Save .newsrc.eld.
	  (set-buffer (get-buffer-create " *Gnus-newsrc*"))
	  (make-local-variable 'version-control)
	  (setq version-control 'never)
	  (setq buffer-file-name
		(concat gnus-current-startup-file ".eld"))
	  (setq default-directory (file-name-directory buffer-file-name))
	  (gnus-add-current-to-buffer-list)
	  (buffer-disable-undo (current-buffer))
	  (erase-buffer)
	  (gnus-message 5 "Saving %s.eld..." gnus-current-startup-file)
	  (gnus-gnus-to-quick-newsrc-format)
	  (run-hooks 'gnus-save-quick-newsrc-hook)
	  (save-buffer)
	  (kill-buffer (current-buffer))
	  (gnus-message
	   5 "Saving %s.eld...done" gnus-current-startup-file))
	(gnus-dribble-delete-file)
	(gnus-group-set-mode-line)))))

(defun gnus-gnus-to-quick-newsrc-format ()
  "Insert Gnus variables such as gnus-newsrc-alist in lisp format."
  (insert ";; Gnus startup file.\n")
  (insert ";; Never delete this file - touch .newsrc instead to force Gnus\n")
  (insert ";; to read .newsrc.\n")
  (insert "(setq gnus-newsrc-file-version "
	  (prin1-to-string gnus-version) ")\n")
  (let ((variables
	 (if gnus-save-killed-list gnus-variable-list
	   ;; Remove the `gnus-killed-list' from the list of variables
	   ;; to be saved, if required.
	   (delq 'gnus-killed-list (copy-sequence gnus-variable-list))))
	;; Peel off the "dummy" group.
	(gnus-newsrc-alist (cdr gnus-newsrc-alist))
	variable)
    ;; Insert the variables into the file.
    (while variables
      (when (and (boundp (setq variable (pop variables)))
		 (symbol-value variable))
	(insert "(setq " (symbol-name variable) " '")
	(prin1 (symbol-value variable) (current-buffer))
	(insert ")\n")))))

(defun gnus-gnus-to-newsrc-format ()
  ;; Generate and save the .newsrc file.
  (save-excursion
    (set-buffer (create-file-buffer gnus-current-startup-file))
    (let ((newsrc (cdr gnus-newsrc-alist))
	  (standard-output (current-buffer))
	  info ranges range method)
      (setq buffer-file-name gnus-current-startup-file)
      (setq default-directory (file-name-directory buffer-file-name))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; Write options.
      (if gnus-newsrc-options (insert gnus-newsrc-options))
      ;; Write subscribed and unsubscribed.
      (while (setq info (pop newsrc))
	;; Don't write foreign groups to .newsrc.
	(when (or (null (setq method (gnus-info-method info)))
		  (equal method "native")
		  (gnus-server-equal method gnus-select-method))
	  (insert (gnus-info-group info)
		  (if (> (gnus-info-level info) gnus-level-subscribed)
		      "!" ":"))
	  (when (setq ranges (gnus-info-read info))
	    (insert " ")
	    (if (not (listp (cdr ranges)))
		(if (= (car ranges) (cdr ranges))
		    (princ (car ranges))
		  (princ (car ranges))
		  (insert "-")
		  (princ (cdr ranges)))
	      (while (setq range (pop ranges))
		(if (or (atom range) (= (car range) (cdr range)))
		    (princ (or (and (atom range) range) (car range)))
		  (princ (car range))
		  (insert "-")
		  (princ (cdr range)))
		(if ranges (insert ",")))))
	  (insert "\n")))
      (make-local-variable 'version-control)
      (setq version-control 'never)
      ;; It has been reported that sometime the modtime on the .newsrc
      ;; file seems to be off.  We really do want to overwrite it, so
      ;; we clear the modtime here before saving.  It's a bit odd,
      ;; though...
      ;; sometimes the modtime clear isn't sufficient.  most brute force:
      ;; delete the silly thing entirely first.  but this fails to provide
      ;; such niceties as .newsrc~ creation.
      (if gnus-modtime-botch
	  (delete-file gnus-startup-file)
	(clear-visited-file-modtime))
      (run-hooks 'gnus-save-standard-newsrc-hook)
      (save-buffer)
      (kill-buffer (current-buffer)))))


;;;
;;; Slave functions.
;;;

(defun gnus-slave-save-newsrc ()
  (save-excursion
    (set-buffer gnus-dribble-buffer)
    (let ((slave-name
	   (make-temp-name (concat gnus-current-startup-file "-slave-"))))
      (write-region (point-min) (point-max) slave-name nil 'nomesg))))

(defun gnus-master-read-slave-newsrc ()
  (let ((slave-files
	 (directory-files
	  (file-name-directory gnus-current-startup-file)
	  t (concat
	     "^" (regexp-quote
		  (concat
		   (file-name-nondirectory gnus-current-startup-file)
		   "-slave-")))
	  t))
	file)
    (if (not slave-files)
	()				; There are no slave files to read.
      (gnus-message 7 "Reading slave newsrcs...")
      (save-excursion
	(set-buffer (get-buffer-create " *gnus slave*"))
	(buffer-disable-undo (current-buffer))
	(setq slave-files
	      (sort (mapcar (lambda (file)
			      (list (nth 5 (file-attributes file)) file))
			    slave-files)
		    (lambda (f1 f2)
		      (or (< (caar f1) (caar f2))
			  (< (nth 1 (car f1)) (nth 1 (car f2)))))))
	(while slave-files
	  (erase-buffer)
	  (setq file (nth 1 (car slave-files)))
	  (insert-file-contents file)
	  (if (condition-case ()
		  (progn
		    (eval-buffer (current-buffer))
		    t)
		(error
		 (gnus-error 3.2 "Possible error in %s" file)
		 nil))
	      (or gnus-slave ; Slaves shouldn't delete these files.
		  (condition-case ()
		      (delete-file file)
		    (error nil))))
	  (setq slave-files (cdr slave-files))))
      (gnus-message 7 "Reading slave newsrcs...done"))))


;;;
;;; Group description.
;;;

(defun gnus-read-all-descriptions-files ()
  (let ((methods (cons gnus-select-method 
		       (nconc
			(when (gnus-archive-server-wanted-p)
			  (list "archive"))
			gnus-secondary-select-methods))))
    (while methods
      (gnus-read-descriptions-file (car methods))
      (setq methods (cdr methods)))
    t))

(defun gnus-read-descriptions-file (&optional method)
  (let ((method (or method gnus-select-method))
	group)
    (when (stringp method)
      (setq method (gnus-server-to-method method)))
    ;; We create the hashtable whether we manage to read the desc file
    ;; to avoid trying to re-read after a failed read.
    (or gnus-description-hashtb
	(setq gnus-description-hashtb
	      (gnus-make-hashtable (length gnus-active-hashtb))))
    ;; Mark this method's desc file as read.
    (gnus-sethash (gnus-group-prefixed-name "" method) "Has read"
		  gnus-description-hashtb)

    (gnus-message 5 "Reading descriptions file via %s..." (car method))
    (cond
     ((not (gnus-check-server method))
      (gnus-message 1 "Couldn't open server")
      nil)
     ((not (gnus-request-list-newsgroups method))
      (gnus-message 1 "Couldn't read newsgroups descriptions")
      nil)
     (t
      (save-excursion
	(save-restriction
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (when (or (search-forward "\n.\n" nil t)
		    (goto-char (point-max)))
	    (beginning-of-line)
	    (narrow-to-region (point-min) (point)))
	  ;; If these are groups from a foreign select method, we insert the
	  ;; group prefix in front of the group names.
	  (and method (not (gnus-server-equal
			    (gnus-server-get-method nil method)
			    (gnus-server-get-method nil gnus-select-method)))
	       (let ((prefix (gnus-group-prefixed-name "" method)))
		 (goto-char (point-min))
		 (while (and (not (eobp))
			     (progn (insert prefix)
				    (zerop (forward-line 1)))))))
	  (goto-char (point-min))
	  (while (not (eobp))
	    ;; If we get an error, we set group to 0, which is not a
	    ;; symbol...
	    (setq group
		  (condition-case ()
		      (let ((obarray gnus-description-hashtb))
			;; Group is set to a symbol interned in this
			;; hash table.
			(read nntp-server-buffer))
		    (error 0)))
	    (skip-chars-forward " \t")
	    ;; ...  which leads to this line being effectively ignored.
	    (and (symbolp group)
		 (set group (buffer-substring
			     (point) (progn (end-of-line) (point)))))
	    (forward-line 1))))
      (gnus-message 5 "Reading descriptions file...done")
      t))))

(defun gnus-group-get-description (group)
  "Get the description of a group by sending XGTITLE to the server."
  (when (gnus-request-group-description group)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (when (looking-at "[^ \t]+[ \t]+\\(.*\\)")
	(match-string 1)))))


;;;
;;; Buffering of read articles.
;;;

(defvar gnus-backlog-buffer " *Gnus Backlog*")
(defvar gnus-backlog-articles nil)
(defvar gnus-backlog-hashtb nil)

(defun gnus-backlog-buffer ()
  "Return the backlog buffer."
  (or (get-buffer gnus-backlog-buffer)
      (save-excursion
	(set-buffer (get-buffer-create gnus-backlog-buffer))
	(buffer-disable-undo (current-buffer))
	(setq buffer-read-only t)
	(gnus-add-current-to-buffer-list)
	(get-buffer gnus-backlog-buffer))))

(defun gnus-backlog-setup ()
  "Initialize backlog variables."
  (unless gnus-backlog-hashtb
    (setq gnus-backlog-hashtb (make-vector 1023 0))))

(gnus-add-shutdown 'gnus-backlog-shutdown 'gnus)

(defun gnus-backlog-shutdown ()
  "Clear all backlog variables and buffers."
  (when (get-buffer gnus-backlog-buffer)
    (kill-buffer gnus-backlog-buffer))
  (setq gnus-backlog-hashtb nil
	gnus-backlog-articles nil))

(defun gnus-backlog-enter-article (group number buffer)
  (gnus-backlog-setup)
  (let ((ident (intern (concat group ":" (int-to-string number))
		       gnus-backlog-hashtb))
	b)
    (if (memq ident gnus-backlog-articles)
	() ; It's already kept.
      ;; Remove the oldest article, if necessary.
      (and (numberp gnus-keep-backlog)
	   (>= (length gnus-backlog-articles) gnus-keep-backlog)
	   (gnus-backlog-remove-oldest-article))
      (setq gnus-backlog-articles (cons ident gnus-backlog-articles))
      ;; Insert the new article.
      (save-excursion
	(set-buffer (gnus-backlog-buffer))
	(let (buffer-read-only)
	  (goto-char (point-max))
	  (or (bolp) (insert "\n"))
	  (setq b (point))
	  (insert-buffer-substring buffer)
	  ;; Tag the beginning of the article with the ident.
	  (gnus-put-text-property b (1+ b) 'gnus-backlog ident))))))

(defun gnus-backlog-remove-oldest-article ()
  (save-excursion
    (set-buffer (gnus-backlog-buffer))
    (goto-char (point-min))
    (if (zerop (buffer-size))
	() ; The buffer is empty.
      (let ((ident (get-text-property (point) 'gnus-backlog))
	    buffer-read-only)
	;; Remove the ident from the list of articles.
	(when ident
	  (setq gnus-backlog-articles (delq ident gnus-backlog-articles)))
	;; Delete the article itself.
	(delete-region
	 (point) (next-single-property-change
		  (1+ (point)) 'gnus-backlog nil (point-max)))))))

(defun gnus-backlog-remove-article (group number)
  "Remove article NUMBER in GROUP from the backlog."
  (when (numberp number)
    (gnus-backlog-setup)
    (let ((ident (intern (concat group ":" (int-to-string number))
			 gnus-backlog-hashtb))
	  beg end)
      (when (memq ident gnus-backlog-articles)
	;; It was in the backlog.
	(save-excursion
	  (set-buffer (gnus-backlog-buffer))
	  (let (buffer-read-only)
	    (when (setq beg (text-property-any
			     (point-min) (point-max) 'gnus-backlog
			     ident))
	      ;; Find the end (i. e., the beginning of the next article).
	      (setq end
		    (next-single-property-change
		     (1+ beg) 'gnus-backlog (current-buffer) (point-max)))
	      (delete-region beg end)
	      ;; Return success.
	      t)))))))

(defun gnus-backlog-request-article (group number buffer)
  (when (numberp number)
    (gnus-backlog-setup)
    (let ((ident (intern (concat group ":" (int-to-string number))
			 gnus-backlog-hashtb))
	  beg end)
      (when (memq ident gnus-backlog-articles)
	;; It was in the backlog.
	(save-excursion
	  (set-buffer (gnus-backlog-buffer))
	  (if (not (setq beg (text-property-any
			      (point-min) (point-max) 'gnus-backlog
			      ident)))
	      ;; It wasn't in the backlog after all.
	      (ignore
	       (setq gnus-backlog-articles (delq ident gnus-backlog-articles)))
	    ;; Find the end (i. e., the beginning of the next article).
	    (setq end
		  (next-single-property-change
		   (1+ beg) 'gnus-backlog (current-buffer) (point-max)))))
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (insert-buffer-substring gnus-backlog-buffer beg end)
	  t)))))

;; Allow redefinition of Gnus functions.

(gnus-ems-redefine)

(provide 'gnus)

;;; gnus.el ends here
