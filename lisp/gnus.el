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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Although Gnus looks suspiciously like GNUS, it isn't quite the same
;; beast. Most internal structures have been changed. If you have
;; written packages that depend on any of the hash tables,
;; `gnus-newsrc-alist', `gnus-killed-assoc', marked lists, the .newsrc
;; buffer, or internal knowledge of the `nntp-header-' macros, or
;; dependence on the buffers having a certain format, your code will
;; fail.

;;; Code:

(eval '(run-hooks 'gnus-load-hook))

(require 'mail-utils)
(require 'timezone)
(require 'nnheader)

;; Site dependent variables. These variables should be defined in
;; paths.el.

(defvar gnus-default-nntp-server nil
  "Specify a default NNTP server.
This variable should be defined in paths.el, and should never be set
by the user.
If you want to change servers, you should use `gnus-select-method'.
See the documentation to that variable.")

(defconst gnus-backup-default-subscribed-newsgroups 
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
with the current newsgroup name as the argument. The function should
return a string.

In any case, if the string (either in the variable, in the environment
variable, or returned by the function) is a file name, the contents of
this file will be used as the organization.")

(defvar gnus-use-generic-from nil
  "If nil, the full host name will be the system name prepended to the domain name.
If this is a string, the full host name will be this string.
If this is non-nil, non-string, the domain name will be used as the
full host name.")

(defvar gnus-use-generic-path nil
  "If nil, use the NNTP server name in the Path header.
If stringp, use this; if non-nil, use no host name (user name only).")


;; Customization variables

;; Don't touch this variable.
(defvar gnus-nntp-service "nntp"
  "*NNTP service name (\"nntp\" or 119).
This is an obsolete variable, which is scarcely used. If you use an
nntp server for your newsgroup and want to change the port number
used to 899, you would say something along these lines:

 (setq gnus-select-method '(nntp \"my.nntp.server\" (nntp-port-number 899)))")

(defvar gnus-select-method 
  (nconc
   (list 'nntp (or (getenv "NNTPSERVER") 
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

;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defvar gnus-post-method nil
  "*Preferred method for posting USENET news.
If this variable is nil, Gnus will use the current method to decide
which method to use when posting.  If it is non-nil, it will override
the current method.  This method will not be used in mail groups and
the like, only in \"real\" newsgroups.

The value must be a valid method as discussed in the documentation of
`gnus-select-method'.")

(defvar gnus-refer-article-method nil
  "*Preferred method for fetching an article by Message-ID.
If you are reading news from the local spool (with nnspool), fetching
articles by Message-ID is painfully slow. By setting this method to an
nntp method, you might get acceptable results.

The value of this variable must be a valid select method as discussed
in the documentation of `gnus-select-method'")

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
This variable is semi-obsolete. Use the `gnus-select-method'
variable instead.")

(defvar gnus-startup-file "~/.newsrc"
  "*Your `.newsrc' file.
`.newsrc-SERVER' will be used instead if that exists.")

(defvar gnus-init-file "~/.gnus"
  "*Your Gnus elisp startup file.
If a file with the .el or .elc suffixes exist, it will be read
instead.") 

(defvar gnus-group-faq-directory
  "/ftp@mirrors.aol.com:/pub/rtfm/usenet/"
  "*Directory where the group FAQs are stored.
This will most commonly be on a remote machine, and the file will be
fetched by ange-ftp.

Note that Gnus uses an aol machine as the default directory.  If this
feels fundamentally unclean, just think of it as a way to finally get
something of value back from them.

If the default site is too slow, try one of these:

   North America: ftp.uu.net                     /usenet/news.answers
		  mirrors.aol.com                /pub/rtfm/usenet
		  ftp.seas.gwu.edu               /pub/rtfm
                  rtfm.mit.edu                   /pub/usenet/news.answers
   Europe:        ftp.uni-paderborn.de           /pub/FAQ
		  ftp.Germany.EU.net             /pub/newsarchive/news.answers
		  ftp.sunet.se                   /pub/usenet
   Asia:          nctuccca.edu.tw                /USENET/FAQ
		  hwarang.postech.ac.kr          /pub/usenet/news.answers
		  ftp.hk.super.net               /mirror/faqs")

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
subscribed newsgroups. If neither t nor nil, mark as read in all
newsgroups.") 

(defvar gnus-use-dribble-file t
  "*Non-nil means that Gnus will use a dribble file to store user updates.
If Emacs should crash without saving the .newsrc files, complete
information can be restored from the dribble file.")

(defvar gnus-asynchronous nil
  "*If non-nil, Gnus will supply backends with data needed for async article fetching.")

(defvar gnus-asynchronous-article-function nil
  "*Function for picking articles to pre-fetch, possibly.")

(defvar gnus-score-file-single-match-alist nil
  "*Alist mapping regexps to lists of score files.
Each element of this alist should be of the form
	(\"REGEXP\" [ \"SCORE-FILE-1\" ] [ \"SCORE-FILE-2\" ] ... )

If the name of a group is matched by REGEXP, the corresponding scorefiles
will be used for that group.
The first match found is used, subsequent matching entries are ignored (to
use multiple matches, see gnus-score-file-multiple-match-alist).

These score files are loaded in addition to any files returned by
gnus-score-find-score-files-function (which see).")

(defvar gnus-score-file-multiple-match-alist nil
  "*Alist mapping regexps to lists of score files.
Each element of this alist should be of the form
	(\"REGEXP\" [ \"SCORE-FILE-1\" ] [ \"SCORE-FILE-2\" ] ... )

If the name of a group is matched by REGEXP, the corresponding scorefiles
will be used for that group.
If multiple REGEXPs match a group, the score files corresponding to each
match will be used (for only one match to be used, see
gnus-score-file-single-match-alist).

These score files are loaded in addition to any files returned by
gnus-score-find-score-files-function (which see).")


(defvar gnus-score-file-suffix "SCORE"
  "*Suffix of the score files.")

(defvar gnus-adaptive-file-suffix "ADAPT"
  "*Suffix of the adaptive score files.")

(defvar gnus-score-find-score-files-function 'gnus-score-find-bnews
  "*Function used to find score files.
The function will be called with the group name as the argument, and
should return a list of score files to apply to that group.  The score
files do not actually have to exist.

Predefined values are:

gnus-score-find-single: Only apply the group's own score file.
gnus-score-find-hierarchical: Also apply score files from parent groups.
gnus-score-find-bnews: Apply score files whose names matches.

See the documentation to these functions for more information.

This variable can also be a list of functions to be called.  Each
function should either return a list of score files, or a list of
score alists.")

(defvar gnus-score-interactive-default-score 1000
  "*Scoring commands will raise/lower the score with this number as the default.")

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
will not be used for kill files.")

(defvar gnus-article-save-directory (or (getenv "SAVEDIR") "~/News/")
  "*Name of the directory articles will be saved in (default \"~/News\").
Initialized from the SAVEDIR environment variable.")

(defvar gnus-kill-files-directory (or (getenv "SAVEDIR") "~/News/")
  "*Name of the directory where kill files will be stored (default \"~/News\").
Initialized from the SAVEDIR environment variable.")

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

(defvar gnus-split-methods nil
  "*Variable used to suggest where articles are to be saved.
The syntax of this variable is the same as `nnmail-split-methods'.  

For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))")

(defvar gnus-save-score nil
  "*If non-nil, save group scoring info.")

(defvar gnus-use-adaptive-scoring nil
  "*If non-nil, use some adaptive scoring scheme.")

(defvar gnus-use-cache nil
  "*If non-nil, Gnus will cache (some) articles locally.")

(defvar gnus-use-scoring t
  "*If non-nil, enable scoring.")

(defvar gnus-fetch-old-headers nil
  "*Non-nil means that Gnus will try to build threads by grabbing old headers.
If an unread article in the group refers to an older, already read (or
just marked as read) article, the old article will not normally be
displayed in the Summary buffer.  If this variable is non-nil, Gnus
will attempt to grab the headers to the old articles, and thereby
build complete threads.  If it has the value `some', only enough
headers to connect otherwise loose threads will be displayed.

The server has to support XOVER for any of this to work.")

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
current newsgroup, you will go to the next newsgroup. If this variable
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
lost. Instead of having many small sub-threads from this original thread
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
subject fields.  (Or rather, they will be printed with a string
given by the `gnus-summary-same-subject' variable.)")

(defvar gnus-summary-gather-subject-limit nil
  "*Maximum length of subject comparisons when gathering loose threads.
Use nil to compare full subjects.  Setting this variable to a low
number will help gather threads that have been corrupted by
newsreaders chopping off subject lines, but it might also mean that
unrelated articles that have subject that happen to begin with the
same few characters will be incorrectly gathered.

If this variable is `fuzzy', Gnus will use a fuzzy algorithm when
comparing subjects.")

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-summary-same-subject ""
  "*String indicating that the current article has the same subject as the previous.
This variable will only be used if the value of
`gnus-summary-make-false-root' is `empty'.")

(defvar gnus-summary-goto-unread t
  "*If non-nil, marking commands will go to the next unread article.")

(defvar gnus-group-goto-unread t
  "*If non-nil, movement commands will go to the next unread and subscribed group.")

(defvar gnus-check-new-newsgroups t
  "*Non-nil means that Gnus will add new newsgroups at startup.
If this variable is `ask-server', Gnus will ask the server for new
groups since the last time it checked. This means that the killed list
is no longer necessary, so you could set `gnus-save-killed-list' to
nil. 

A variant is to have this variable be a list of select methods. Gnus
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

(defvar gnus-activate-foreign-newsgroups 4
  "*If nil, Gnus will not check foreign newsgroups at startup.
If it is non-nil, it should be a number between one and nine. Foreign
newsgroups that have a level lower or equal to this number will be
activated on startup. For instance, if you want to active all
subscribed newsgroups, but not the rest, you'd set this variable to 
`gnus-level-subscribed'.

If you subscribe to lots of newsgroups from different servers, startup
might take a while. By setting this variable to nil, you'll save time,
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
This will save both time (when starting and quitting) and space (both
memory and disk), but it will also mean that Gnus has no record of
which groups are new and which are old, so the automatic new
newsgroups subscription methods become meaningless. You should always
set `gnus-check-new-newsgroups' to `ask-server' or nil if you set this
variable to nil.")

(defvar gnus-interactive-catchup t
  "*If non-nil, require your confirmation when catching up a group.")

(defvar gnus-interactive-post t
  "*If non-nil, group name will be asked for when posting.")

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
  "*Regular expression that will be removed from subject strings if
fuzzy subject simplification is selected.")

(defvar gnus-group-default-list-level gnus-level-subscribed
  "*Default listing level. 
Ignored if `gnus-group-use-permanent-levels' is non-nil.")

(defvar gnus-group-use-permanent-levels nil
  "*If non-nil, once you set a level, Gnus will use this level.")

(defvar gnus-show-mime nil
  "*If non-nil, do mime processing of articles.
The articles will simply be fed to the function given by
`gnus-show-mime-method'.")

(defvar gnus-strict-mime t
  "*If nil, decode MIME header even if there is not Mime-Version field.")
 
(defvar gnus-show-mime-method (function metamail-buffer)
  "*Function to process a MIME message.
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

(defvar gnus-thread-indent-level 4
  "*Number that says how much each sub-thread should be indented.")

(defvar gnus-ignored-newsgroups 
  (purecopy (mapconcat 'identity
                       '("^to\\."       ; not "real" groups
                         "^[0-9. \t]+ " ; all digits in name
                         "[][\"#'()]"   ; bogus characters
                         )
                       "\\|"))
  "*A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively non-existent.")

(defvar gnus-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^Received:\\|^Mail-from:"
  "*All headers that match this regexp will be hidden.
If `gnus-visible-headers' is non-nil, this variable will be ignored.")

(defvar gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^Cc:"
  "*All headers that do not match this regexp will be hidden.
If this variable is non-nil, `gnus-ignored-headers' will be ignored.")

(defvar gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^To:" 
    "^Cc:" "^Date:" "^Organization:")
  "*This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list.")

(defvar gnus-show-all-headers nil
  "*If non-nil, don't hide any headers.")

(defvar gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving.")

(defvar gnus-inhibit-startup-message nil
  "*If non-nil, the startup message will not be displayed.")

(defvar gnus-signature-separator "^-- *$"
  "Regexp matching signature separator.")

(defvar gnus-auto-extend-newsgroup t
  "*If non-nil, extend newsgroup forward and backward when requested.")

(defvar gnus-auto-select-first t
  "*If non-nil, select the first unread article when entering a group.
If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in
`gnus-select-group-hook'.") 

(defvar gnus-auto-select-next t
  "*If non-nil, offer to go to the next group from the end of the previous.
If the value is t and the next newsgroup is empty, Gnus will exit
summary mode and go back to group mode.  If the value is neither nil
nor t, Gnus will select the following unread newsgroup.  In
particular, if the value is the symbol `quietly', the next unread
newsgroup will be selected without any confirmations.")

(defvar gnus-auto-select-same nil
  "*If non-nil, select the next article with the same subject.")

(defvar gnus-summary-check-current nil
  "*If non-nil, consider the current article when moving.
The \"unread\" movement commands will stay on the same line if the
current article is unread.")

(defvar gnus-auto-center-summary t
  "*If non-nil, always center the current summary buffer.")

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

(defvar gnus-buffer-configuration
  '((group ([group 1.0 point] 
	    (if gnus-carpal [group-carpal 4])))
    (summary ([summary 1.0 point]
	      (if gnus-carpal [summary-carpal 4])))
    (article ([summary 0.25 point] 
	      (if gnus-carpal [summary-carpal 4]) 
	      [article 1.0]))
    (pipe ([summary 0.25 point] 
	   (if gnus-carpal [summary-carpal 4]) 
	   [pipe 1.0]))
    (server ([server 1.0 point]
	     (if gnus-carpal [server-carpal 2])))
    (browse ([browse 1.0 point]
	     (if gnus-carpal [browse-carpal 2])))
    (group-mail ([mail 1.0 point]))
    (summary-mail ([mail 1.0 point]))
    (summary-reply ([article 0.5]
		    [mail 1.0 point]))
    (info ([nil 1.0 point]))
    (summary-faq ([summary 0.25]
		  [faq 1.0 point]))
    (edit-group ([group 0.5]
		 [edit-group 1.0 point]))
    (edit-server ([server 0.5]
		  [edit-server 1.0 point]))
    (edit-score ([summary 0.25]
		 [edit-score 1.0 point]))
    (post ([post 1.0 point]))
    (reply ([article 0.5]
	    [mail 1.0 point]))
    (mail-forward ([mail 1.0 point]))
    (post-forward ([post 1.0 point]))
    (reply-yank ([mail 1.0 point]))
    (followup ([article 0.5]
	       [post 1.0 point]))
    (followup-yank ([post 1.0 point])))
  "Window configuration for all possible Gnus buffers.
This variable is a list of lists.  Each of these lists has a NAME and
a RULE.  The NAMEs are common-sense names like `group', which names a
rule used when displaying the group buffer; `summary', which names a
rule for what happens when you enter a group and do not display an
article buffer; and so on.  See the value of this variable for a
complete list of NAMEs.

Each RULE is a list of vectors.  The first element in this vector is
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
    (pipe . "*Shell Command Output*")
    (edit-group . gnus-group-edit-buffer)
    (edit-server . gnus-server-edit-buffer)
    (group-carpal . gnus-carpal-group-buffer)
    (summary-carpal . gnus-carpal-summary-buffer)
    (server-carpal . gnus-carpal-server-buffer)
    (browse-carpal . gnus-carpal-browse-buffer)
    (edit-score . gnus-score-edit-buffer)
    (mail . gnus-mail-buffer)
    (post . gnus-post-news-buffer)
    (faq . gnus-faq-buffer))
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
for your decision.")

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
`gnus-group-sort-by-alphabet', `gnus-group-sort-by-unread' and
`gnus-group-sort-by-level'")

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
(defvar gnus-kill-file-mark ?X
  "*Mark used for articles killed by kill files.")
(defvar gnus-low-score-mark ?Y
  "*Mark used for articles with a low score.")
(defvar gnus-catchup-mark ?C
  "*Mark used for articles that are caught up.")
(defvar gnus-replied-mark ?A
  "*Mark used for articles that have been replied to.")
(defvar gnus-process-mark ?# 
  "*Process mark.")
(defvar gnus-ancient-mark ?O
  "*Mark used for ancient articles.")
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
(defvar gnus-dummy-mark ?Z
  "*This is a dummy article.")

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

(defvar gnus-group-line-format "%M%S%p%5y: %(%g%)\n"
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
%n    Select from where (string)
%z    A string that look like `<%s:%n>' if a foreign select method is used
%u    User defined specifier. The next character in the format string should
      be a letter.  Gnus will call the function gnus-user-format-function-X,
      where X is the letter following %u. The function will be passed the
      current header as argument. The function should return a string, which
      will be inserted into the buffer just like information from any other
      group specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face' when
the mouse point move inside the area.  There can only be one such area.

Note that this format specification is not always respected. For
reasons of efficiency, when listing killed groups, this specification
is ignored altogether. If the spec is changed considerably, your
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
%u   User defined specifier. The next character in the format string should
     be a letter.  Gnus will call the function gnus-user-format-function-X,
     where X is the letter following %u. The function will be passed the
     current header as argument. The function should return a string, which
     will be inserted into the summary just like information from any other
     summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face'
when the mouse point is placed inside the area.  There can only be one
such area.

The %U (status), %R (replied) and %z (zcore) specs have to be handled
with care. For reasons of efficiency, Gnus will compute what column
these characters will end up in, and \"hard-code\" that. This means that
it is illegal to have these specs after a variable-length spec. Well,
you might not be arrested, but your summary buffer will look strange,
which is bad enough.

The smart choice is to have these specs as for to the left as
possible. 

This restriction may disappear in later versions of Gnus.")

(defvar gnus-summary-dummy-line-format "*  :                          : %S\n"
  "*The format specification for the dummy roots in the summary buffer.
It works along the same lines as a normal formatting string,
with some simple extensions.

%S  The subject")

(defvar gnus-summary-mode-line-format "Gnus: %b [%A] %Z"
  "*The format specification for the summary mode line.")

(defvar gnus-article-mode-line-format "Gnus: %b %S"
  "*The format specification for the article mode line.")

(defvar gnus-group-mode-line-format "Gnus: %b {%M:%S}  "
  "*The format specification for the group mode line.")

(defvar gnus-valid-select-methods
  '(("nntp" post address prompt-address)
    ("nnspool" post)
    ("nnvirtual" none virtual prompt-address) 
    ("nnmbox" mail respool) 
    ("nnml" mail respool)
    ("nnmh" mail respool) 
    ("nndir" none prompt-address address)
    ("nneething" none prompt-address)
    ("nndigest" none) 
    ("nndoc" none prompt-address) 
    ("nnbabyl" mail respool) 
    ("nnkiboze" post virtual) 
    ;;("nnsoup" post)
    ("nnfolder" mail respool))
  "An alist of valid select methods.
The first element of each list lists should be a string with the name
of the select method. The other elements may be be the category of
this method (ie. `post', `mail', `none' or whatever) or other
properties that this method has (like being respoolable).
If you implement a new select method, all you should have to change is
this variable. I think.")

(defvar gnus-updated-mode-lines '(group article summary)
  "*List of buffers that should update their mode lines.
The list may contain the symbols `group', `article' and `summary'. If
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

(defvar gnus-summary-mark-below nil
  "*Mark all articles with a score below this variable as read.
This variable is local to each summary buffer and usually set by the
score file.")  

(defvar gnus-thread-sort-functions '(gnus-thread-sort-by-number)
  "*List of functions used for sorting threads in the summary buffer.
By default, threads are sorted by article number.

Each function takes two threads and return non-nil if the first thread
should be sorted before the other.  If you use more than one function,
the primary sort function should be the last.

Ready-mady functions include `gnus-thread-sort-by-number',
`gnus-thread-sort-by-author', `gnus-thread-sort-by-subject',
`gnus-thread-sort-by-date', `gnus-thread-sort-by-score' and
`gnus-thread-sort-by-total-score' (see `gnus-thread-score-function').")

(defvar gnus-thread-score-function '+
  "*Function used for calculating the total score of a thread.

The function is called with the scores of the article and each
subthread and should then return the score of the thread.

Some functions you can use are `+', `max', or `min'.")

(defvar gnus-options-subscribe nil
  "*All new groups matching this regexp will be subscribed unconditionally.
Note that this variable deals only with new newsgroups.  This variable
does not affect old newsgroups.")

(defvar gnus-options-not-subscribe nil
  "*All new groups matching this regexp will be ignored.
Note that this variable deals only with new newsgroups.  This variable
does not affect old (already subscribed) newsgroups.")

(defvar gnus-auto-expirable-newsgroups nil
  "*Groups in which to automatically mark read articles as expirable.
If non-nil, this should be a regexp that should match all groups in
which to perform auto-expiry.  This only makes sense for mail groups.")

(defvar gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text.")

(defvar gnus-modtime-botch nil
  "*Non-nil means .newsrc should be deleted prior to save.  Its use is
due to the bogus appearance that .newsrc was modified on disc.")

;; Hooks.

(defvar gnus-group-mode-hook nil
  "*A hook for Gnus group mode.")

(defvar gnus-summary-mode-hook nil
  "*A hook for Gnus summary mode.
This hook is run before any variables are set in the summary buffer.")

(defvar gnus-article-mode-hook nil
  "*A hook for Gnus article mode.")

(defun gnus-summary-prepare-exit-hook nil
  "*A hook called when preparing to exit from the summary buffer.
It calls `gnus-summary-expire-articles' by default.")
(add-hook 'gnus-summary-prepare-exit-hook 'gnus-summary-expire-articles)

(defun gnus-summary-exit-hook nil
  "*A hook called on exit from the summary buffer.")

(defvar gnus-open-server-hook nil
  "*A hook called just before opening connection to the news server.")

(defvar gnus-load-hook nil
  "*A hook run while Gnus is loaded.")

(defvar gnus-startup-hook nil
  "*A hook called at startup.
This hook is called after Gnus is connected to the NNTP server.")

(defvar gnus-get-new-news-hook nil
  "*A hook run just before Gnus checks for new news.")

(defvar gnus-group-prepare-function 'gnus-group-prepare-flat
  "*A function that is called to generate the group buffer.
The function is called with three arguments: The first is a number;
all group with a level less or equal to that number should be listed,
if the second is non-nil, empty groups should also be displayed. If
the third is non-nil, it is a number. No groups with a level lower
than this number should be displayed.

The only current function implemented is `gnus-group-prepare-flat'.")

(defvar gnus-group-prepare-hook nil
  "*A hook called after the group buffer has been generated.
If you want to modify the group buffer, you can use this hook.")

(defvar gnus-summary-prepare-hook nil
  "*A hook called after the summary buffer has been generated.
If you want to modify the summary buffer, you can use this hook.")

(defvar gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer.
If you want to run a special decoding program like nkf, use this hook.")

;(defvar gnus-article-display-hook nil
;  "*A hook called after the article is displayed in the article buffer.
;The hook is designed to change the contents of the article
;buffer. Typical functions that this hook may contain are
;`gnus-article-hide-headers' (hide selected headers),
;`gnus-article-maybe-highlight' (perform fancy article highlighting), 
;`gnus-article-hide-signature' (hide signature) and
;`gnus-article-treat-overstrike' (turn \"^H_\" into bold characters).")
;(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
;(add-hook 'gnus-article-display-hook 'gnus-article-treat-overstrike)
;(add-hook 'gnus-article-display-hook 'gnus-article-maybe-highlight)

(defvar gnus-article-x-face-command
  "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | xv -quit -"
  "String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously. The compressed face will be piped to this command.") 

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

(defvar gnus-select-article-hook
  '(gnus-summary-show-thread)
  "*A hook called when an article is selected.
The default hook shows conversation thread subtrees of the selected
article automatically using `gnus-summary-show-thread'.")

(defvar gnus-apply-kill-hook '(gnus-apply-kill-file)
  "*A hook called to apply kill files to a group.
This hook is intended to apply a kill file to the selected newsgroup.
The function `gnus-apply-kill-file' is called by default.

Since a general kill file is too heavy to use only for a few
newsgroups, I recommend you to use a lighter hook function. For
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

(defun gnus-parse-headers-hook nil
  "*A hook called before parsing the headers.")

(defvar gnus-exit-group-hook nil
  "*A hook called when exiting (not quitting) summary mode.")

(defvar gnus-suspend-gnus-hook nil
  "*A hook called when suspending (not exiting) Gnus.")

(defvar gnus-exit-gnus-hook nil
  "*A hook called when exiting Gnus.")

(defvar gnus-save-newsrc-hook nil
  "*A hook called when saving the newsrc file.")

(defvar gnus-summary-update-hook 
  (list 'gnus-summary-highlight-line)
  "*A hook called when a summary line is changed.
The hook will not be called if `gnus-visual' is nil.

The default function `gnus-summary-highlight-line' will
highlight the line according to the `gnus-summary-highlight'
variable.")

(defvar gnus-mark-article-hook (list 'gnus-summary-mark-unread-as-read)
  "*A hook called when an article is selected for the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected.")

;; Remove any hilit infestation.
(add-hook 'gnus-startup-hook
	  (lambda ()
	    (remove-hook 'gnus-summary-prepare-hook
			 'hilit-rehighlight-buffer-quietly)
	    (remove-hook 'gnus-summary-prepare-hook 'hilit-install-line-hooks)
	    (setq gnus-mark-article-hook '(gnus-summary-mark-unread-as-read))
	    (remove-hook 'gnus-article-prepare-hook
			 'hilit-rehighlight-buffer-quietly)))



;; Internal variables

;; Avoid highlighting in kill files.
(defvar gnus-summary-inhibit-highlight nil)
(defvar gnus-newsgroup-selected-overlay nil)

(defvar gnus-article-mode-map nil)
(defvar gnus-dribble-buffer nil)
(defvar gnus-headers-retrieved-by nil)
(defvar gnus-article-reply nil)
(defvar gnus-override-method nil)
(defvar gnus-article-check-size nil)

(defvar gnus-current-score-file nil)
(defvar gnus-internal-global-score-files nil)
(defvar gnus-score-file-list nil)
(defvar gnus-scores-exclude-files nil)

(defvar gnus-current-move-group nil)

(defvar gnus-newsgroup-dependencies nil)
(defvar gnus-newsgroup-threads nil)
(defvar gnus-newsgroup-async nil)
(defconst gnus-group-edit-buffer "*Gnus edit newsgroup*")

(defvar gnus-newsgroup-adaptive nil)

(defvar gnus-summary-display-table nil)

(defconst gnus-group-line-format-alist
  (list (list ?M 'marked ?c)
	(list ?S 'subscribed ?c)
	(list ?L 'level ?d)
	(list ?N 'number ?s)
	(list ?I 'number-of-dormant ?d)
	(list ?T 'number-of-ticked ?d)
	(list ?R 'number-of-read ?s)
	(list ?t 'number-total ?d)
	(list ?y 'number-of-unread-unticked ?s)
	(list ?i 'number-of-ticked-and-dormant ?d)
	(list ?g 'group ?s)
	(list ?G 'qualified-group ?s)
	(list ?D 'newsgroup-description ?s)
	(list ?o 'moderated ?c)
	(list ?O 'moderated-string ?s)
	(list ?p 'process-marked ?c)
	(list ?s 'news-server ?s)
	(list ?n 'news-method ?s)
	(list ?z 'news-method-string ?s)
	(list ?u 'user-defined ?s)))

(defconst gnus-summary-line-format-alist 
  (list (list ?N 'number ?d)
	(list ?S 'subject ?s)
	(list ?s 'subject-or-nil ?s)
	(list ?n 'name ?s)
	(list ?A '(car (cdr (funcall gnus-extract-address-components from)))
	      ?s)
	(list ?a '(or (car (funcall gnus-extract-address-components from)) 
		      from) ?s)
	(list ?F 'from ?s)
	(list ?x (macroexpand '(mail-header-xref header)) ?s)
	(list ?D (macroexpand '(mail-header-date header)) ?s)
  	(list ?d '(gnus-dd-mmm (mail-header-date header)) ?s)
	(list ?M (macroexpand '(mail-header-id header)) ?s)
	(list ?r (macroexpand '(mail-header-references header)) ?s)
	(list ?c '(or (mail-header-chars header) 0) ?d)
	(list ?L 'lines ?d)
	(list ?I 'indentation ?s)
	(list ?T '(if (= level 0) "" (make-string (frame-width) ? )) ?s)
	(list ?R 'replied ?c)
	(list ?\[ 'opening-bracket ?c)
	(list ?\] 'closing-bracket ?c)
	(list ?\> '(make-string level ? ) ?s)
	(list ?\< '(make-string (max 0 (- 20 level)) ? ) ?s)
	(list ?i 'score ?d)
	(list ?z 'score-char ?c)
	(list ?U 'unread ?c)
	(list ?t '(gnus-summary-number-of-articles-in-thread 
		   (and (boundp 'thread) (car thread)))
	      ?d)
	(list ?e '(gnus-summary-number-of-articles-in-thread 
		   (and (boundp 'thread) (car thread)) t)
	      ?c)
	(list ?u 'user-defined ?s))
  "An alist of format specifications that can appear in summary lines,
and what variables they correspond with, along with the type of the
variable (string, integer, character, etc).")

(defconst gnus-summary-dummy-line-format-alist
  (list (list ?S 'subject ?s)
	(list ?N 'number ?d)
	(list ?u 'user-defined ?s)))

(defconst gnus-summary-mode-line-format-alist 
  (list (list ?G 'group-name ?s)
	(list ?g '(gnus-short-group-name group-name) ?s)
	(list ?A 'article-number ?d)
	(list ?Z 'unread-and-unselected ?s)
	(list ?V 'gnus-version ?s)
	(list ?U 'unread ?d)
	(list ?S 'subject ?s)
	(list ?e 'unselected ?d)
	(list ?u 'user-defined ?s)
	(list ?b 'buffer-name ?s)
	(list ?s '(gnus-current-score-file-nondirectory) ?s)))

(defconst gnus-group-mode-line-format-alist 
  (list (list ?S 'news-server ?s)
	(list ?M 'news-method ?s)
	(list ?b '(buffer-name) ?s)
	(list ?u 'user-defined ?s)))

(defvar gnus-have-read-active-file nil)

(defconst gnus-maintainer
  "gnus-bug@ifi.uio.no (The Gnus Bugfixing Girls + Boys)"
  "The mail address of the Gnus maintainers.")

(defconst gnus-version "Gnus v5.1"
  "Version number for this version of Gnus.")

(defvar gnus-info-nodes
  '((gnus-group-mode		"(gnus)The Group Buffer")
    (gnus-summary-mode		"(gnus)The Summary Buffer")
    (gnus-article-mode		"(gnus)The Article Buffer"))
  "Assoc list of major modes and related Info nodes.")

(defvar gnus-group-buffer "*Group*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")
(defvar gnus-server-buffer "*Server*")

(defvar gnus-work-buffer " *gnus work*")

(defvar gnus-buffer-list nil
  "Gnus buffers that should be killed on exit.")

(defvar gnus-server-alist nil
  "List of available servers.")

(defvar gnus-variable-list
  '(gnus-newsrc-options gnus-newsrc-options-n
    gnus-newsrc-last-checked-date 
    gnus-newsrc-alist gnus-server-alist
    gnus-killed-list gnus-zombie-list)
  "Gnus variables saved in the quick startup file.")

(defvar gnus-overload-functions
  '((news-inews gnus-inews-news "rnewspost"))
  "Functions overloaded by gnus.
It is a list of `(original overload &optional file)'.")

(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n nil
  "List of regexps representing groups to be subscribed/ignored unconditionally.") 

(defvar gnus-newsrc-last-checked-date nil
  "Date Gnus last asked server for new newsgroups.")

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

(defvar gnus-newsgroup-unreads nil
  "List of unread articles in the current newsgroup.")

(defvar gnus-newsgroup-unselected nil
  "List of unselected unread articles in the current newsgroup.")

(defvar gnus-newsgroup-reads nil
  "Alist of read articles and article marks in the current newsgroup.")

(defvar gnus-newsgroup-marked nil
  "List of ticked articles in the current newsgroup (a subset of unread art).")

(defvar gnus-newsgroup-killed nil
  "List of ranges of articles that have been through the scoring process.")

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
(defvar gnus-newsgroup-headers-hashtb-by-number nil)

(defvar gnus-newsgroup-ancient nil
  "List of `gnus-fetch-old-headers' articles in the current newsgroup.")

(defvar gnus-current-article nil)
(defvar gnus-article-current nil)
(defvar gnus-current-headers nil)
(defvar gnus-have-all-headers nil)
(defvar gnus-last-article nil)
(defvar gnus-newsgroup-history nil)
(defvar gnus-current-kill-article nil)

;; Save window configuration.
(defvar gnus-prev-winconf nil)

;; Format specs
(defvar gnus-summary-line-format-spec nil)
(defvar gnus-summary-dummy-line-format-spec nil)
(defvar gnus-group-line-format-spec nil)
(defvar gnus-summary-mode-line-format-spec nil)
(defvar gnus-article-mode-line-format-spec nil)
(defvar gnus-group-mode-line-format-spec nil)
(defvar gnus-summary-mark-positions nil)
(defvar gnus-group-mark-positions nil)

(defvar gnus-summary-expunge-below nil)
(defvar gnus-reffed-article-number nil)

; Let the byte-compiler know that we know about this variable.
(defvar rmail-default-rmail-file)

(defvar gnus-cache-removable-articles nil)

(defconst gnus-summary-local-variables 
  '(gnus-newsgroup-name 
    gnus-newsgroup-begin gnus-newsgroup-end 
    gnus-newsgroup-last-rmail gnus-newsgroup-last-mail 
    gnus-newsgroup-last-folder gnus-newsgroup-last-file 
    gnus-newsgroup-auto-expire gnus-newsgroup-unreads 
    gnus-newsgroup-unselected gnus-newsgroup-marked
    gnus-newsgroup-reads
    gnus-newsgroup-replied gnus-newsgroup-expirable
    gnus-newsgroup-processable gnus-newsgroup-killed
    gnus-newsgroup-bookmarks gnus-newsgroup-dormant
    gnus-newsgroup-headers gnus-newsgroup-headers-hashtb-by-number
    gnus-current-article gnus-current-headers gnus-have-all-headers
    gnus-last-article gnus-article-internal-prepare-hook
    gnus-newsgroup-dependencies gnus-newsgroup-selected-overlay
    gnus-newsgroup-scored gnus-newsgroup-kill-headers
    gnus-newsgroup-threads gnus-newsgroup-async
    gnus-score-alist gnus-current-score-file gnus-summary-expunge-below 
    gnus-summary-mark-below gnus-newsgroup-active gnus-scores-exclude-files
    gnus-newsgroup-history gnus-newsgroup-ancient
    (gnus-newsgroup-adaptive . gnus-use-adaptive-scoring)
    gnus-cache-removable-articles)
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

  ;; Various 
  (autoload 'metamail-buffer "metamail")
  (autoload 'Info-goto-node "info")
  (autoload 'hexl-hex-string-to-integer "hexl")
  (autoload 'pp "pp")
  (autoload 'pp-to-string "pp")
  (autoload 'pp-eval-expression "pp")
  (autoload 'mail-extract-address-components "mail-extr")

  (autoload 'nnmail-split-fancy "nnmail")
  (autoload 'nnvirtual-catchup-group "nnvirtual")

  ;; timezone
  (autoload 'timezone-make-date-arpa-standard "timezone")
  (autoload 'timezone-fix-time "timezone")
  (autoload 'timezone-make-sortable-date "timezone")
  (autoload 'timezone-make-time-string "timezone")

  ;; rmail & friends
  (autoload 'mail-position-on-field "sendmail")
  (autoload 'mail-setup "sendmail")
  (autoload 'rmail-output "rmailout")
  (autoload 'news-mail-other-window "rnewspost")
  (autoload 'news-reply-yank-original "rnewspost")
  (autoload 'news-caesar-buffer-body "rnewspost")
  (autoload 'rmail-insert-rmail-file-header "rmail")
  (autoload 'rmail-count-new-messages "rmail")
  (autoload 'rmail-show-message "rmail")

  ;; gnus-soup
  ;;(autoload 'gnus-group-brew-soup "gnus-soup" nil t)
  ;;(autoload 'gnus-brew-soup "gnus-soup" nil t)
  ;;(autoload 'gnus-soup-add-article "gnus-soup" nil t)
  ;;(autoload 'gnus-soup-send-replies "gnus-soup" nil t)
  ;;(autoload 'gnus-soup-save-areas "gnus-soup" nil t)
  ;;(autoload 'gnus-soup-pack-packet "gnus-soup" nil t)
  ;;(autoload 'nnsoup-pack-replies "nnsoup" nil t)

  ;; gnus-mh
  (autoload 'gnus-mail-reply-using-mhe "gnus-mh")
  (autoload 'gnus-mail-forward-using-mhe "gnus-mh")
  (autoload 'gnus-mail-other-window-using-mhe "gnus-mh")
  (autoload 'gnus-summary-save-in-folder "gnus-mh" nil t)
  (autoload 'gnus-summary-save-article-folder "gnus-mh")
  (autoload 'gnus-Folder-save-name "gnus-mh")
  (autoload 'gnus-folder-save-name "gnus-mh")

  ;; gnus-vis misc
  (autoload 'gnus-group-make-menu-bar "gnus-vis")
  (autoload 'gnus-summary-make-menu-bar "gnus-vis")
  (autoload 'gnus-server-make-menu-bar "gnus-vis")
  (autoload 'gnus-article-make-menu-bar "gnus-vis")
  (autoload 'gnus-browse-make-menu-bar "gnus-vis")
  (autoload 'gnus-highlight-selected-summary "gnus-vis")
  (autoload 'gnus-summary-highlight-line "gnus-vis")
  (autoload 'gnus-carpal-setup-buffer "gnus-vis")

  ;; gnus-vis article
  (autoload 'gnus-article-push-button "gnus-vis" nil t)
  (autoload 'gnus-article-press-button "gnus-vis" nil t)
  (autoload 'gnus-article-highlight "gnus-vis" nil t)
  (autoload 'gnus-article-highlight-some "gnus-vis" nil t)
  (autoload 'gnus-article-hide "gnus-vis" nil t)
  (autoload 'gnus-article-hide-signature "gnus-vis" nil t)
  (autoload 'gnus-article-highlight-headers "gnus-vis" nil t)
  (autoload 'gnus-article-highlight-signature "gnus-vis" nil t)
  (autoload 'gnus-article-add-buttons "gnus-vis" nil t)
  (autoload 'gnus-article-next-button "gnus-vis" nil t)
  (autoload 'gnus-article-add-button "gnus-vis")

  ;; gnus-cite
  (autoload 'gnus-article-highlight-citation "gnus-cite" nil t)
  (autoload 'gnus-article-hide-citation-maybe "gnus-cite" nil t)
  (autoload 'gnus-article-hide-citation "gnus-cite" nil t)

  ;; gnus-kill
  (autoload 'gnus-kill "gnus-kill")
  (autoload 'gnus-apply-kill-file-internal "gnus-kill")
  (autoload 'gnus-kill-file-edit-file "gnus-kill")
  (autoload 'gnus-kill-file-raise-followups-to-author "gnus-kill")
  (autoload 'gnus-execute "gnus-kill")
  (autoload 'gnus-expunge "gnus-kill")

  ;; gnus-cache
  (autoload 'gnus-cache-possibly-enter-article "gnus-cache")
  (autoload 'gnus-cache-save-buffers "gnus-cache")
  (autoload 'gnus-cache-possibly-remove-articles "gnus-cache")
  (autoload 'gnus-cache-request-article "gnus-cache")
  (autoload 'gnus-cache-retrieve-headers "gnus-cache")
  (autoload 'gnus-cache-possibly-alter-active "gnus-cache")
  (autoload 'gnus-jog-cache "gnus-cache" nil t)
  (autoload 'gnus-cache-enter-remove-article "gnus-cache")

  ;; gnus-score
  (autoload 'gnus-summary-increase-score "gnus-score" nil t)
  (autoload 'gnus-summary-lower-score "gnus-score" nil t)
  (autoload 'gnus-summary-score-map "gnus-score" nil nil 'keymap)
  (autoload 'gnus-score-save "gnus-score")
  (autoload 'gnus-score-headers "gnus-score")
  (autoload 'gnus-current-score-file-nondirectory "gnus-score")
  (autoload 'gnus-score-adaptive "gnus-score")
  (autoload 'gnus-score-remove-lines-adaptive "gnus-score")
  (autoload 'gnus-score-find-trace "gnus-score")

  ;; gnus-edit
  (autoload 'gnus-score-customize "gnus-edit" nil t)

  ;; gnus-uu
  (autoload 'gnus-uu-extract-map "gnus-uu" nil nil 'keymap)
  (autoload 'gnus-uu-mark-map "gnus-uu" nil nil 'keymap)
  (autoload 'gnus-uu-digest-mail-forward "gnus-uu" nil t)
  (autoload 'gnus-uu-digest-post-forward "gnus-uu" nil t)
  (autoload 'gnus-uu-mark-series "gnus-uu" nil t)
  (autoload 'gnus-uu-mark-region "gnus-uu" nil t)
  (autoload 'gnus-uu-mark-by-regexp "gnus-uu" nil t)
  (autoload 'gnus-uu-mark-all "gnus-uu" nil t)
  (autoload 'gnus-uu-mark-sparse "gnus-uu" nil t)
  (autoload 'gnus-uu-mark-thread "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-uu "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-uu-and-save "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-unshar "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-unshar-and-save "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-save "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-binhex "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-uu-view "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-uu-and-save-view "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-unshar-view "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-unshar-and-save-view "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-save-view "gnus-uu" nil t)
  (autoload 'gnus-uu-decode-binhex-view "gnus-uu" nil t)

  ;; gnus-msg
  (autoload 'gnus-summary-send-map "gnus-msg" nil nil 'keymap)
  (autoload 'gnus-group-post-news "gnus-msg" nil t)
  (autoload 'gnus-group-mail "gnus-msg" nil t)
  (autoload 'gnus-summary-post-news "gnus-msg" nil t)
  (autoload 'gnus-summary-followup "gnus-msg" nil t)
  (autoload 'gnus-summary-followup-with-original "gnus-msg" nil t)
  (autoload 'gnus-summary-followup-and-reply "gnus-msg" nil t)
  (autoload 'gnus-summary-followup-and-reply-with-original "gnus-msg" nil t)
  (autoload 'gnus-summary-cancel-article "gnus-msg" nil t)
  (autoload 'gnus-summary-supersede-article "gnus-msg" nil t)
  (autoload 'gnus-post-news "gnus-msg" nil t)
  (autoload 'gnus-inews-news "gnus-msg" nil t)
  (autoload 'gnus-cancel-news "gnus-msg" nil t)
  (autoload 'gnus-summary-reply "gnus-msg" nil t)
  (autoload 'gnus-summary-reply-with-original "gnus-msg" nil t)
  (autoload 'gnus-summary-mail-forward "gnus-msg" nil t)
  (autoload 'gnus-summary-mail-other-window "gnus-msg" nil t)
  (autoload 'gnus-mail-reply-using-mail "gnus-msg")
  (autoload 'gnus-mail-yank-original "gnus-msg")
  (autoload 'gnus-mail-send-and-exit "gnus-msg")
  (autoload 'gnus-mail-forward-using-mail "gnus-msg")
  (autoload 'gnus-mail-other-window-using-mail "gnus-msg")
  (autoload 'gnus-article-mail "gnus-msg")
  (autoload 'gnus-bug "gnus-msg" nil t)

  ;; gnus-vm
  (autoload 'gnus-summary-save-in-vm "gnus-vm" nil t)
  (autoload 'gnus-summary-save-article-vm "gnus-vm" nil t)
  (autoload 'gnus-mail-forward-using-vm "gnus-vm")
  (autoload 'gnus-mail-reply-using-vm "gnus-vm")
  (autoload 'gnus-mail-other-window-using-vm "gnus-vm" nil t)
  (autoload 'gnus-yank-article "gnus-vm" nil t)

  )



;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
;; If you want the cursor to go somewhere else, set these two
;; functions in some startup hook to whatever you want.
(defalias 'gnus-summary-position-cursor 'gnus-goto-colon)
(defalias 'gnus-group-position-cursor 'gnus-goto-colon)

;;; Various macros and substs.

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then returns to original window."
  (` (let ((GnusStartBufferWindow (selected-window)))
       (unwind-protect
	   (progn
	     (pop-to-buffer (, buffer))
	     (,@ forms))
	 (select-window GnusStartBufferWindow)))))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  ;;(` (symbol-value (abbrev-symbol (, string) (, hashtable))))
  ;;(` (abbrev-expansion (, string) (, hashtable)))
  (` (symbol-value (intern-soft (, string) (, hashtable)))))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value. Arguments are STRING, VALUE, and HASHTABLE."
  ;; We cannot use define-abbrev since it only accepts string as value.
  ;; (set (intern string hashtable) value))
  (` (set (intern (, string) (, hashtable)) (, value))))

(defsubst gnus-buffer-substring (beg end)
  (buffer-substring (match-beginning beg) (match-end end)))

;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;   function `substring' might cut on a middle of multi-octet
;;   character.
(defun gnus-truncate-string (str width)
  (substring str 0 width))

;; Added by Geoffrey T. Dairiki <dairiki@u.washington.edu>. A safe way
;; to limit the length of a string. This function is necessary since
;; `(substr "abc" 0 30)' pukes with "Args out of range".
(defsubst gnus-limit-string (str width)
  (if (> (length str) width)
      (substring str 0 width)
    str))

(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines."
  (let ((case-fold-search t))
    (if (string-match "^re: *" subject)
	(substring subject (match-end 0))
      subject)))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))

(defmacro gnus-buffer-exists-p (buffer)
  (` (and (, buffer)
	  (funcall (if (stringp (, buffer)) 'get-buffer 'buffer-name)
		   (, buffer)))))

(defmacro gnus-kill-buffer (buffer)
  (` (if (gnus-buffer-exists-p (, buffer))
	 (kill-buffer (, buffer)))))

(defsubst gnus-point-at-bol ()
  "Return point at the beginning of line."
  (let ((p (point)))
    (beginning-of-line)
    (prog1
	(point)
      (goto-char p))))

(defsubst gnus-point-at-eol ()
  "Return point at the beginning of line."
  (let ((p (point)))
    (end-of-line)
    (prog1
	(point)
      (goto-char p))))

;; Delete the current line (and the next N lines.);
(defmacro gnus-delete-line (&optional n)
  (` (delete-region (progn (beginning-of-line) (point))
		    (progn (forward-line (, (or n 1))) (point)))))

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
	 (load gnus-init-file nil t))))

;;; Load the user startup file.
;; (eval '(gnus-read-init-file 'inhibit))

;;; Load the compatibility functions. 

(require 'gnus-cus)
(require 'gnus-ems)


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
      (let ((case-fold-search t))
	(gnus-narrow-to-headers)
	(mail-fetch-field field)))))

(defun gnus-goto-colon ()
  (beginning-of-line)
  (search-forward ":" (gnus-point-at-eol) t))

(defun gnus-narrow-to-headers ()
  (widen)
  (save-excursion
    (narrow-to-region
     (goto-char (point-min))
     (if (search-forward "\n\n" nil t)
	 (1- (point))
       (point-max)))))

(defvar gnus-old-specs nil)

(defun gnus-update-format-specifications ()
  (gnus-make-thread-indent-array)

  (let ((formats '(summary summary-dummy group 
			   summary-mode group-mode article-mode))
	old-format new-format)
    (while formats
      (setq new-format (symbol-value
			(intern (format "gnus-%s-line-format" (car formats)))))
      (or (and (setq old-format (cdr (assq (car formats) gnus-old-specs)))
	       (equal old-format new-format))
	  (set (intern (format "gnus-%s-line-format-spec" (car formats)))
	       (gnus-parse-format
		new-format
		(symbol-value 
		 (intern (format "gnus-%s-line-format-alist"
				 (if (eq (car formats) 'article-mode)
				     'summary-mode (car formats))))))))
      (setq gnus-old-specs (cons (cons (car formats) new-format)
				 (delq (car formats) gnus-old-specs)))
      (setq formats (cdr formats))))
      
  (gnus-update-group-mark-positions)
  (gnus-update-summary-mark-positions)

  (if (and (string-match "%D" gnus-group-line-format)
	   (not gnus-description-hashtb)
	   gnus-read-active-file)
      (gnus-read-all-descriptions-files)))

(defun gnus-update-summary-mark-positions ()
  (save-excursion
    (let ((gnus-replied-mark 129)
	  (gnus-score-below-mark 130)
	  (gnus-score-over-mark 130)
	  (thread nil)
	  pos)
      (gnus-set-work-buffer)
      (gnus-summary-insert-line 
       nil [0 "" "" "" "" "" 0 0 ""]  0 nil 128 t nil "" nil 1)
      (goto-char (point-min))
      (setq pos (list (cons 'unread (and (search-forward "\200" nil t)
					 (- (point) 2)))))
      (goto-char (point-min))
      (setq pos (cons (cons 'replied (and (search-forward "\201" nil t)
					  (- (point) 2))) pos))
      (goto-char (point-min))
      (setq pos (cons (cons 'score (and (search-forward "\202" nil t)
					(- (point) 2))) pos))
      (setq gnus-summary-mark-positions pos))))

(defun gnus-update-group-mark-positions ()
  (save-excursion
    (let ((gnus-process-mark 128)
	  (gnus-group-marked '("dummy.group")))
      (gnus-sethash "dummy.group" '(0 . 0) gnus-active-hashtb)
      (gnus-set-work-buffer)
      (gnus-group-insert-group-line nil "dummy.group" 0 nil 0 nil)
      (goto-char (point-min))
      (setq gnus-group-mark-positions
	    (list (cons 'process (and (search-forward "\200" nil t)
				      (- (point) 2))))))))

(defun gnus-mouse-face-function (form)
  (` (let ((string (, form)))
       (put-text-property 0 (length string) 'mouse-face gnus-mouse-face string)
       string)))

(defun gnus-max-width-function (el max-width)
  (or (numberp max-width) (signal 'wrong-type-argument '(numberp max-width)))
  (` (let* ((val (eval (, el)))
	    (valstr (if (numberp val)
			(int-to-string val) val)))
       (if (> (length valstr) (, max-width))
	   (substring valstr 0 (, max-width))
	 valstr))))

(defun gnus-parse-format (format spec-alist)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  (if (string-match "\\`\\(.*\\)%(\\(.*\\)%)\\(.*\n?\\)\\'" format)
      (if (and gnus-visual gnus-mouse-face)
	  (let ((pre (substring format (match-beginning 1) (match-end 1)))
		(button (substring format (match-beginning 2) (match-end 2)))
		(post (substring format (match-beginning 3) (match-end 3))))
	    (list 'concat
		  (gnus-parse-simple-format pre spec-alist)
		  (gnus-mouse-face-function 
		   (gnus-parse-simple-format button spec-alist))
		  (gnus-parse-simple-format post spec-alist)))
	(gnus-parse-simple-format
	 (concat (substring format (match-beginning 1) (match-end 1))
		 (substring format (match-beginning 2) (match-end 2))
		 (substring format (match-beginning 3) (match-end 3)))
	 spec-alist))
    (gnus-parse-simple-format format spec-alist)))

(defun gnus-parse-simple-format (format spec-alist)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string. The list will consist of the symbol `format', a format
  ;; specification string, and a list of forms depending on the
  ;; SPEC-ALIST.
  (let ((max-width 0)
	spec flist fstring newspec elem beg)
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "%[-0-9]*\\(,[0-9]+\\)?\\([^0-9]\\)\\(.\\)?" nil t)
	(setq spec (string-to-char (buffer-substring (match-beginning 2)
						     (match-end 2))))
	;; First check if there are any specs that look anything like
	;; "%12,12A", ie. with a "max width specification". These have
	;; to be treated specially.
	(if (setq beg (match-beginning 1))
	    (setq max-width 
		  (string-to-int 
		   (buffer-substring (1+ (match-beginning 1)) (match-end 1))))
	  (setq max-width 0)
	  (setq beg (match-beginning 2)))
	;; Find the specification from `spec-alist'.
	(if (not (setq elem (cdr (assq spec spec-alist))))
	    (setq elem '("*" ?s)))
	;; Treat user defined format specifiers specially
	(and (eq (car elem) 'user-defined)
	     (setq elem
		   (list 
		    (list (intern (concat "gnus-user-format-function-"
					  (buffer-substring
					   (match-beginning 3)
					   (match-end 3))))
			  'header)
		    ?s))
	     (delete-region (match-beginning 3) (match-end 3)))
	(if (not (zerop max-width))
	    (let ((el (car elem)))
	      (cond ((= (car (cdr elem)) ?c) 
		     (setq el (list 'char-to-string el)))
		    ((= (car (cdr elem)) ?d)
		     (numberp el) (setq el (list 'int-to-string el))))
	      (setq flist (cons (gnus-max-width-function el max-width)
				flist))
	      (setq newspec ?s))
	  (setq flist (cons (car elem) flist))
	  (setq newspec (car (cdr elem))))
	;; Remove the old specification (and possibly a ",12" string).
	(delete-region beg (match-end 2))
	;; Insert the new specification.
	(goto-char beg)
	(insert newspec))
      (setq fstring (buffer-substring 1 (point-max))))
    (cons 'format (cons fstring (nreverse flist)))))

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
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.  Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
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
       (or gnus-article-save-directory "~/News"))))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

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
		 (string-match prefix (car (cdr groups))))
	    (progn
	      (setq prefixes (cons prefix prefixes))
	      (message "Descend hierarchy %s? ([y]nsq): " 
		       (substring prefix 1 (1- (length prefix))))
	      (setq ans (read-char))
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
	  (setq ans (read-char))
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
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (let ((groups (cdr gnus-newsrc-alist))
	before)
    (while (and (not before) groups)
      (if (string< newgroup (car (car groups)))
	  (setq before (car (car groups)))
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
			(setq before (buffer-substring
				      (match-beginning 1) (match-end 1)))
			(string< before newgroup)))))
	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	(setq groupkey
	      (if (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		  (substring groupkey (match-beginning 1) (match-end 1)))))
      (gnus-subscribe-newsgroup newgroup before))))

(defun gnus-subscribe-interactively (newsgroup)
  "Subscribe new NEWSGROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed. If not,
it is killed."
  (if (gnus-y-or-n-p (format "Subscribe new newsgroup: %s " newsgroup))
      (gnus-subscribe-hierarchically newsgroup)
    (setq gnus-killed-list (cons newsgroup gnus-killed-list))))

(defun gnus-subscribe-zombies (newsgroup)
  "Make new NEWSGROUP a zombie group."
  (setq gnus-zombie-list (cons newsgroup gnus-zombie-list)))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If NEXT is non-nil, it is inserted before NEXT. Otherwise it is made
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
  (gnus-replace-chars-in-string group ?/ ?.))

(defun gnus-make-directory (dir)
  "Make DIRECTORY recursively."
  ;; Why don't we use `(make-directory dir 'parents)'? That's just one
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

;; Var

(defun gnus-simplify-subject (subject &optional re-only)
  "Remove `Re:' and words in parentheses.
If optional argument RE-ONLY is non-nil, strip `Re:' only."
  (let ((case-fold-search t))		;Ignore case.
    ;; Remove `Re:' and `Re^N:'.
    (if (string-match "^re:[ \t]*" subject)
	(setq subject (substring subject (match-end 0))))
    ;; Remove words in parentheses from end.
    (or re-only
	(while (string-match "[ \t\n]*([^()]*)[ \t\n]*\\'" subject)
	  (setq subject (substring subject 0 (match-beginning 0)))))
    ;; Return subject string.
    subject))

;; Remove any leading "re:"s, any trailing paren phrases, and simplify
;; all whitespace.
(defun gnus-simplify-subject-fuzzy (subject)
  (let ((case-fold-search t))
    (save-excursion
      (gnus-set-work-buffer)
      (insert subject)
      (inline (gnus-simplify-buffer-fuzzy))
      (buffer-string))))

(defun gnus-simplify-buffer-fuzzy ()
  (goto-char (point-min))
  ;; Fix by Stainless Steel Rat <ratinox@ccs.neu.edu>.
  (while (re-search-forward "^[ \t]*\\(re\\|fwd\\)[[{(^0-9]*[])}]?[:;][ \t]*"
			    nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "[ \t\n]*([^()]*)[ \t\n]*$" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+" nil t)
    (replace-match " " t t))
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]+" nil t)
    (replace-match "" t t))
  (if gnus-simplify-subject-fuzzy-regexp
      (while (re-search-forward gnus-simplify-subject-fuzzy-regexp nil t)
	(replace-match "" t t))))

;; Add the current buffer to the list of buffers to be killed on exit. 
(defun gnus-add-current-to-buffer-list ()
  (or (memq (current-buffer) gnus-buffer-list)
      (setq gnus-buffer-list (cons (current-buffer) gnus-buffer-list))))

(defun gnus-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

;; Functions accessing headers.
;; Functions are more convenient than macros in some cases.

(defun gnus-header-number (header)
  (mail-header-number header))

(defun gnus-header-subject (header)
  (mail-header-subject header))

(defun gnus-header-from (header)
  (mail-header-from header))

(defun gnus-header-xref (header)
  (mail-header-xref header))

(defun gnus-header-lines (header)
  (mail-header-lines header))

(defun gnus-header-date (header)
  (mail-header-date header))

(defun gnus-header-id (header)
  (mail-header-id header))

(defun gnus-header-message-id (header)
  (mail-header-id header))

(defun gnus-header-chars (header)
  (mail-header-chars header))

(defun gnus-header-references (header)
  (mail-header-references header))

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
	gnus-newsgroup-headers nil
	gnus-newsgroup-headers-hashtb-by-number nil
	gnus-newsgroup-name nil
	gnus-server-alist nil
	gnus-current-select-method nil)
  ;; Reset any score variables.
  (and (boundp 'gnus-score-cache)
       (set 'gnus-score-cache nil))
  (and (boundp 'gnus-internal-global-score-files)
       (set 'gnus-internal-global-score-files nil))
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  ;; Save any cache buffers.
  (and gnus-use-cache (gnus-cache-save-buffers))
  ;; Clear the dribble buffer.
  (gnus-dribble-clear)
  ;; Kill global KILL file buffer.
  (if (get-file-buffer (gnus-newsgroup-kill-file nil))
      (kill-buffer (get-file-buffer (gnus-newsgroup-kill-file nil))))
  (gnus-kill-buffer nntp-server-buffer)
  ;; Kill Gnus buffers.
  (while gnus-buffer-list
    (gnus-kill-buffer (car gnus-buffer-list))
    (setq gnus-buffer-list (cdr gnus-buffer-list))))

(defun gnus-windows-old-to-new (setting)
  ;; First we take care of the really, really old Gnus 3 actions.
  (if (symbolp setting)
      (setq setting 
	    (cond ((memq setting '(SelectArticle))
		   'article)
		  ((memq setting '(SelectSubject ExpandSubject))
		   'summary)
		  ((memq setting '(SelectNewsgroup ExitNewsgroup))
		   'group)
		  (t setting))))
  (if (or (listp setting)
	  (not (and gnus-window-configuration
		    (memq setting '(group summary article)))))
      setting
    (let* ((setting (if (eq setting 'group) 
			(if (assq 'newsgroup gnus-window-configuration)
			    'newsgroup
			  'newsgroups) setting))
	   (elem (car (cdr (assq setting gnus-window-configuration))))
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
	      (setq perc  (/ (* 1.0 (nth 0 elem)) total))
	      (setq out (cons (if (eq pbuf (nth i types))
				  (vector (nth i types) perc 'point)
				(vector (nth i types) perc))
			      out))))
	(setq i (1+ i)))
      (list (nreverse out)))))
	   
(defun gnus-add-configuration (conf)
  (setq gnus-buffer-configuration 
	(cons conf (delq (assq (car conf) gnus-buffer-configuration)
			 gnus-buffer-configuration))))

(defun gnus-configure-windows (setting &optional force)
  (setq setting (gnus-windows-old-to-new setting))
  (let ((r (if (symbolp setting)
	       (cdr (assq setting gnus-buffer-configuration))
	     setting))
	(in-buf (current-buffer))
	rule val w height hor ohor heights sub jump-buffer
	rel total to-buf all-visible)
    (or r (error "No such setting: %s" setting))

    (if (and (not force) (setq all-visible (gnus-all-windows-visible-p r)))
	;; All the windows mentioned are already visible, so we just
	;; put point in the assigned buffer, and do not touch the
	;; winconf. 
	(select-window (get-buffer-window all-visible t))
	 

      ;; Either remove all windows or just remove all Gnus windows.
      (if gnus-use-full-window
	  (delete-other-windows)
	(gnus-remove-some-windows)
	(switch-to-buffer nntp-server-buffer))

      (while r
	(setq hor (car r)
	      ohor nil)

	;; We have to do the (possible) horizontal splitting before the
	;; vertical. 
	(if (and (listp (car hor)) 
		 (eq (car (car hor)) 'horizontal))
	    (progn
	      (split-window 
	       nil
	       (if (integerp (nth 1 (car hor)))
		   (nth 1 (car hor))
		 (- (frame-width) (floor (* (frame-width) (nth 1 (car hor))))))
	       t)
	      (setq hor (cdr hor))))

	;; Go through the rules and eval the elements that are to be
	;; evalled.  
	(while hor
	  (if (setq val (if (vectorp (car hor)) (car hor) (eval (car hor))))
	      (progn
		;; Expand short buffer name.
		(setq w (aref val 0))
		(and (setq w (cdr (assq w gnus-window-to-buffer)))
		     (progn
		       (setq val (apply 'vector (mapcar 'identity val)))
		       (aset val 0 w)))
		(setq ohor (cons val ohor))))
	  (setq hor (cdr hor)))
	(setq rule (cons (nreverse ohor) rule))
	(setq r (cdr r)))
      (setq rule (nreverse rule))

      ;; We tally the window sizes.
      (setq total (window-height))
      (while rule
	(setq hor (car rule))
	(if (and (listp (car hor)) (eq (car (car hor)) 'horizontal))
	    (setq hor (cdr hor)))
	(setq sub 0)
	(while hor
	  (setq rel (aref (car hor) 1)
		heights (cons
			 (cond ((and (floatp rel) (= 1.0 rel))
				'x)
			       ((integerp rel)
				rel)
			       (t
				(max (floor (* total rel)) 4)))
			 heights)
		sub (+ sub (if (numberp (car heights)) (car heights) 0))
		hor (cdr hor)))
	(setq heights (nreverse heights)
	      hor (car rule))

	;; We then go through these heights and create windows for them.
	(while heights
	  (setq height (car heights)
		heights (cdr heights))
	  (and (eq height 'x)
	       (setq height (- total sub)))
	  (and heights
	       (split-window nil height))
	  (setq to-buf (aref (car hor) 0))
	  (switch-to-buffer 
	   (cond ((not to-buf)
		  in-buf)
		 ((symbolp to-buf)
		  (symbol-value (aref (car hor) 0)))
		 (t
		  (aref (car hor) 0))))
	  (and (> (length (car hor)) 2)
	       (eq (aref (car hor) 2) 'point)
	       (setq jump-buffer (current-buffer)))
	  (other-window 1)
	  (setq hor (cdr hor)))
      
	(setq rule (cdr rule)))

      ;; Finally, we pop to the buffer that's supposed to have point. 
      (or jump-buffer (error "Missing `point' in spec for %s" setting))

      (select-window (get-buffer-window jump-buffer t))
      (set-buffer jump-buffer))))

(defun gnus-all-windows-visible-p (rule)
  (let (invisible hor jump-buffer val buffer)
    ;; Go through the rules and eval the elements that are to be
    ;; evalled.  
    (while (and rule (not invisible))
      (setq hor (car rule)
	    rule (cdr rule))
      (while (and hor (not invisible))
	(if (setq val (if (vectorp (car hor)) 
			  (car hor)
			(if (not (eq (car (car hor)) 'horizontal))
			    (eval (car hor)))))
	    (progn
	      ;; Expand short buffer name.
	      (setq buffer (or (cdr (assq (aref val 0) gnus-window-to-buffer))
			       (aref val 0)))
	      (setq buffer (if (symbolp buffer) (symbol-value buffer)
			     buffer))
	      (and (> (length val) 2) (eq 'point (aref val 2))
		   (setq jump-buffer buffer))
	      (setq invisible (not (and buffer (get-buffer-window buffer))))))
	(setq hor (cdr hor))))
    (and (not invisible) jump-buffer)))

(defun gnus-window-top-edge (&optional window)
  (nth 1 (window-edges window)))

(defun gnus-remove-some-windows ()
  (let ((buffers gnus-window-to-buffer)
	buf bufs lowest-buf lowest)
    (save-excursion
      ;; Remove windows on all known Gnus buffers.
      (while buffers
	(setq buf (cdr (car buffers)))
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
      (let (wins)
	(walk-windows
	 (lambda (win)
	   (let ((buf (window-buffer win)))
	     (if (string-match  "^\\*Summary" (buffer-name buf))
		 (progn
		   (setq bufs (cons buf bufs))
		   (pop-to-buffer buf)
		   (if (or (not lowest)
			   (< (gnus-window-top-edge) lowest))
		       (progn
			 (setq lowest-buf buf)
			 (setq lowest (gnus-window-top-edge))))))))))
      (and lowest-buf 
	   (progn
	     (pop-to-buffer lowest-buf)
	     (switch-to-buffer nntp-server-buffer)))
      (while bufs
	(and (not (eq (car bufs) lowest-buf))
	     (delete-windows-on (car bufs)))
	(setq bufs (cdr bufs))))))
			  
(defun gnus-version ()
  "Version numbers of this version of Gnus."
  (interactive)
  (let ((methods gnus-valid-select-methods)
	(mess gnus-version)
	meth)
    ;; Go through all the legal select methods and add their version
    ;; numbers to the total version string. Only the backends that are
    ;; currently in use will have their message numbers taken into
    ;; consideration. 
    (while methods
      (setq meth (intern (concat (car (car methods)) "-version")))
      (and (boundp meth)
	   (stringp (symbol-value meth))
	   (setq mess (concat mess "; " (symbol-value meth))))
      (setq methods (cdr methods)))
    (gnus-message 2 mess)))

(defun gnus-info-find-node ()
  "Find Info documentation of Gnus."
  (interactive)
  ;; Enlarge info window if needed.
  (let ((mode major-mode))
    (gnus-configure-windows 'info)
    (Info-goto-node (car (cdr (assq mode gnus-info-nodes))))))

(defun gnus-overload-functions (&optional overloads)
  "Overload functions specified by optional argument OVERLOADS.
If nothing is specified, use the variable gnus-overload-functions."
  (let ((defs nil)
	(overloads (or overloads gnus-overload-functions)))
    (while overloads
      (setq defs (car overloads))
      (setq overloads (cdr overloads))
      ;; Load file before overloading function if necessary.  Make
      ;; sure we cannot use `require' always.
      (and (not (fboundp (car defs)))
	   (car (cdr (cdr defs)))
	   (load (car (cdr (cdr defs))) nil 'nomessage))
      (fset (car defs) (car (cdr defs))))))

(defun gnus-replace-chars-in-string (string &rest pairs)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0)
	sym to)
    (or (zerop (% (length pairs) 2)) 
	(error "Odd number of translation pairs"))
    (setplist 'sym pairs)
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (if (setq to (get 'sym (aref string idx)))
	  (aset string idx to))
      (setq idx (1+ idx)))
    string))

(defun gnus-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (- (gnus-day-number date1) (gnus-day-number date2)))

(defun gnus-day-number (date)
  (let ((dat (mapcar (lambda (s) (and s (string-to-int s)) )
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian 
     (nth 1 dat) (nth 2 dat) (car dat))))

;; Returns a floating point number that says how many seconds have
;; lapsed between Jan 1 12:00:00 1970 and DATE.
(defun gnus-seconds-since-epoch (date)
  (let* ((tdate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date date)))
	 (ttime (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-time
			 (aref (timezone-parse-date date) 3))))
	 (edate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date "Jan 1 12:00:00 1970")))
	 (tday (- (timezone-absolute-from-gregorian 
		   (nth 1 tdate) (nth 2 tdate) (nth 0 tdate))
		  (timezone-absolute-from-gregorian 
		   (nth 1 edate) (nth 2 edate) (nth 0 edate)))))
    (+ (nth 2 ttime)
       (* (nth 1 ttime) 60)
       (* 1.0 (nth 0 ttime) 60 60)
       (* 1.0 tday 60 60 24))))

(defun gnus-file-newer-than (file date)
  (let ((fdate (nth 5 (file-attributes file))))
    (or (> (car fdate) (car date))
	(and (= (car fdate) (car date))
	     (> (nth 1 fdate) (nth 1 date))))))

(defun gnus-group-read-only-p (&optional group)
  "Check whether GROUP supports editing or not.
If GROUP is nil, `gnus-newsgroup-name' will be checked instead.  Note
that that variable is buffer-local to the summary buffers."
  (let ((group (or group gnus-newsgroup-name)))
    (not (gnus-check-backend-function 'request-replace-article group))))

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
  (let ((datevec (timezone-parse-date messy-date)))
    (format "%2s-%s"
	    (or (aref datevec 2) "??")
	    (capitalize
	     (or (car 
		  (nth (1- (string-to-number (aref datevec 1)))
		       timezone-months-assoc))
		 "???")))))

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
;; Guide-line for numbers:
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

;; Generate a unique new group name.
(defun gnus-generate-new-group-name (leaf)
  (let ((name leaf)
	(num 0))
    (while (gnus-gethash name gnus-newsrc-hashtb)
      (setq name (concat leaf "<" (int-to-string (setq num (1+ num))) ">")))
    name))

(defun gnus-ephemeral-group-p (group)
  "Say whether GROUP is ephemeral or not."
  (assoc 'quit-config (gnus-find-method-for-group group)))

(defun gnus-group-quit-config (group)
  "Return the quit-config of GROUP."
  (nth 1 (assoc 'quit-config (gnus-find-method-for-group group))))

(defun gnus-simplify-mode-line ()
  "Make mode lines a bit simpler."
  (setq mode-line-modified "-- ")
  (if (listp mode-line-format)
      (progn
	(make-local-variable 'mode-line-format)
	(setq mode-line-format (copy-sequence mode-line-format))
	(and (equal (nth 3 mode-line-format) "   ")
	     (setcar (nthcdr 3 mode-line-format) "")))))

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
	  (setq first (car (car ranges)))
	  (setq last  (cdr (car ranges)))
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
			 (car (car ranges))))
	(while (and list (cdr list) (< (car (cdr list)) lowest))
	  (setq list (cdr list)))
	(if (< (car ilist) lowest)
	    (progn
	      (setq temp list)
	      (setq list (cdr list))
	      (setcdr temp nil)
	      (setq out (nconc (gnus-compress-sequence ilist t) out))))
	(setq highest (or (and (atom (car ranges)) (car ranges))
			  (cdr (car ranges))))
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
		(if (atom (car (cdr ranges)))
		    (if (= (1+ (car ranges)) (car (cdr ranges)))
			(progn
			  (setcar ranges (cons (car ranges) 
					       (car (cdr ranges))))
			  (setcdr ranges (cdr (cdr ranges)))))
		  (if (= (1+ (car ranges)) (car (car (cdr ranges))))
		      (progn
			(setcar (car (cdr ranges)) (car ranges))
			(setcar ranges (car (cdr ranges)))
			(setcdr ranges (cdr (cdr ranges)))))))
	  (if (cdr ranges)
	      (if (atom (car (cdr ranges)))
		  (if (= (1+ (cdr (car ranges))) (car (cdr ranges)))
		      (progn
			(setcdr (car ranges) (car (cdr ranges)))
			(setcdr ranges (cdr (cdr ranges)))))
		(if (= (1+ (cdr (car ranges))) (car (car (cdr ranges))))
		    (progn
		      (setcdr (car ranges) (cdr (car (cdr ranges))))
		      (setcdr ranges (cdr (cdr ranges))))))))
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
		    (>= number (car (car ranges))))
		  not-stop)
	(if (if (numberp (car ranges))
		(= number (car ranges))
	      (and (>= number (car (car ranges)))
		   (<= number (cdr (car ranges)))))
	    (setq not-stop nil))
	(setq ranges (cdr ranges)))
      (not not-stop))))


;;;
;;; Gnus group mode
;;;

(defvar gnus-group-mode-map nil)
(defvar gnus-group-group-map nil)
(defvar gnus-group-mark-map nil)
(defvar gnus-group-list-map nil)
(defvar gnus-group-help-map nil)
(defvar gnus-group-sub-map nil)
(put 'gnus-group-mode 'mode-class 'special)

(if gnus-group-mode-map
    nil
  (setq gnus-group-mode-map (make-keymap))
  (suppress-keymap gnus-group-mode-map)
  (define-key gnus-group-mode-map " " 'gnus-group-read-group)
  (define-key gnus-group-mode-map "=" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "\r" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "j" 'gnus-group-jump-to-group)
  (define-key gnus-group-mode-map "n" 'gnus-group-next-unread-group)
  (define-key gnus-group-mode-map "p" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "\177" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "N" 'gnus-group-next-group)
  (define-key gnus-group-mode-map "P" 'gnus-group-prev-group)
  (define-key gnus-group-mode-map
    "\M-n" 'gnus-group-next-unread-group-same-level)
  (define-key gnus-group-mode-map 
    "\M-p" 'gnus-group-prev-unread-group-same-level)
  (define-key gnus-group-mode-map "," 'gnus-group-best-unread-group)
  (define-key gnus-group-mode-map "." 'gnus-group-first-unread-group)
  (define-key gnus-group-mode-map "u" 'gnus-group-unsubscribe-current-group)
  (define-key gnus-group-mode-map "U" 'gnus-group-unsubscribe-group)
  (define-key gnus-group-mode-map "c" 'gnus-group-catchup-current)
  (define-key gnus-group-mode-map "C" 'gnus-group-catchup-current-all)
  (define-key gnus-group-mode-map "l" 'gnus-group-list-groups)
  (define-key gnus-group-mode-map "L" 'gnus-group-list-all-groups)
  (define-key gnus-group-mode-map "m" 'gnus-group-mail)
  (define-key gnus-group-mode-map "g" 'gnus-group-get-new-news)
  (define-key gnus-group-mode-map "\M-g" 'gnus-group-get-new-news-this-group)
  (define-key gnus-group-mode-map "R" 'gnus-group-restart)
  (define-key gnus-group-mode-map "r" 'gnus-group-read-init-file)
  (define-key gnus-group-mode-map "B" 'gnus-group-browse-foreign-server)
  (define-key gnus-group-mode-map "b" 'gnus-group-check-bogus-groups)
  (define-key gnus-group-mode-map "F" 'gnus-find-new-newsgroups)
  (define-key gnus-group-mode-map "\C-c\C-d" 'gnus-group-describe-group)
  (define-key gnus-group-mode-map "\M-d" 'gnus-group-describe-all-groups)
  (define-key gnus-group-mode-map "\C-c\C-a" 'gnus-group-apropos)
  (define-key gnus-group-mode-map "\C-c\M-\C-a" 'gnus-group-description-apropos)
  (define-key gnus-group-mode-map "a" 'gnus-group-post-news)
  (define-key gnus-group-mode-map "\ek" 'gnus-group-edit-local-kill)
  (define-key gnus-group-mode-map "\eK" 'gnus-group-edit-global-kill)
  (define-key gnus-group-mode-map "\C-k" 'gnus-group-kill-group)
  (define-key gnus-group-mode-map "\C-y" 'gnus-group-yank-group)
  (define-key gnus-group-mode-map "\C-w" 'gnus-group-kill-region)
  (define-key gnus-group-mode-map "\C-x\C-t" 'gnus-group-transpose-groups)
  (define-key gnus-group-mode-map "\C-c\C-l" 'gnus-group-list-killed)
  (define-key gnus-group-mode-map "\C-c\C-x" 'gnus-group-expire-articles)
  (define-key gnus-group-mode-map "\C-c\M-\C-x" 'gnus-group-expire-all-groups)
  (define-key gnus-group-mode-map "V" 'gnus-version)
  (define-key gnus-group-mode-map "s" 'gnus-group-save-newsrc)
  (define-key gnus-group-mode-map "z" 'gnus-group-suspend)
  (define-key gnus-group-mode-map "Z" 'gnus-group-clear-dribble)
  (define-key gnus-group-mode-map "q" 'gnus-group-exit)
  (define-key gnus-group-mode-map "Q" 'gnus-group-quit)
  (define-key gnus-group-mode-map "?" 'gnus-group-describe-briefly)
  (define-key gnus-group-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-group-mode-map "\M-e" 'gnus-group-edit-group-method)
  (define-key gnus-group-mode-map "^" 'gnus-group-enter-server-mode)
  (define-key gnus-group-mode-map gnus-mouse-2 'gnus-mouse-pick-group)
  (define-key gnus-group-mode-map "<" 'beginning-of-buffer)
  (define-key gnus-group-mode-map ">" 'end-of-buffer)
  (define-key gnus-group-mode-map "\C-c\C-b" 'gnus-bug)
  (define-key gnus-group-mode-map "\C-c\C-s" 'gnus-group-sort-groups)

  (define-key gnus-group-mode-map "#" 'gnus-group-mark-group)
  (define-key gnus-group-mode-map "\M-#" 'gnus-group-unmark-group)
  (define-prefix-command 'gnus-group-mark-map)
  (define-key gnus-group-mode-map "M" 'gnus-group-mark-map)
  (define-key gnus-group-mark-map "m" 'gnus-group-mark-group)
  (define-key gnus-group-mark-map "u" 'gnus-group-unmark-group)
  (define-key gnus-group-mark-map "w" 'gnus-group-mark-region)

  (define-prefix-command 'gnus-group-group-map)
  (define-key gnus-group-mode-map "G" 'gnus-group-group-map)
  (define-key gnus-group-group-map "d" 'gnus-group-make-directory-group)
  (define-key gnus-group-group-map "h" 'gnus-group-make-help-group)
  (define-key gnus-group-group-map "a" 'gnus-group-make-archive-group)
  (define-key gnus-group-group-map "k" 'gnus-group-make-kiboze-group)
  (define-key gnus-group-group-map "m" 'gnus-group-make-group)
  (define-key gnus-group-group-map "E" 'gnus-group-edit-group)
  (define-key gnus-group-group-map "e" 'gnus-group-edit-group-method)
  (define-key gnus-group-group-map "p" 'gnus-group-edit-group-parameters)
  (define-key gnus-group-group-map "v" 'gnus-group-add-to-virtual)
  (define-key gnus-group-group-map "V" 'gnus-group-make-empty-virtual)
  (define-key gnus-group-group-map "D" 'gnus-group-enter-directory)
  (define-key gnus-group-group-map "f" 'gnus-group-make-doc-group)
  ;;(define-key gnus-group-group-map "sb" 'gnus-group-brew-soup)
  ;;(define-key gnus-group-group-map "sw" 'gnus-soup-save-areas)
  ;;(define-key gnus-group-group-map "ss" 'gnus-soup-send-replies)
  ;;(define-key gnus-group-group-map "sp" 'gnus-soup-pack-packet)
  ;;(define-key gnus-group-group-map "sr" 'nnsoup-pack-replies)

  (define-prefix-command 'gnus-group-list-map)
  (define-key gnus-group-mode-map "A" 'gnus-group-list-map)
  (define-key gnus-group-list-map "k" 'gnus-group-list-killed)
  (define-key gnus-group-list-map "z" 'gnus-group-list-zombies)
  (define-key gnus-group-list-map "s" 'gnus-group-list-groups)
  (define-key gnus-group-list-map "u" 'gnus-group-list-all-groups)
  (define-key gnus-group-list-map "a" 'gnus-group-apropos)
  (define-key gnus-group-list-map "d" 'gnus-group-description-apropos)
  (define-key gnus-group-list-map "m" 'gnus-group-list-matching)
  (define-key gnus-group-list-map "M" 'gnus-group-list-all-matching)

  (define-prefix-command 'gnus-group-help-map)
  (define-key gnus-group-mode-map "H" 'gnus-group-help-map)
  (define-key gnus-group-help-map "f" 'gnus-group-fetch-faq)

  (define-prefix-command 'gnus-group-sub-map)
  (define-key gnus-group-mode-map "S" 'gnus-group-sub-map)
  (define-key gnus-group-sub-map "l" 'gnus-group-set-current-level)
  (define-key gnus-group-sub-map "t" 'gnus-group-unsubscribe-current-group)
  (define-key gnus-group-sub-map "s" 'gnus-group-unsubscribe-group)
  (define-key gnus-group-sub-map "k" 'gnus-group-kill-group)
  (define-key gnus-group-sub-map "y" 'gnus-group-yank-group)
  (define-key gnus-group-sub-map "w" 'gnus-group-kill-region)
  (define-key gnus-group-sub-map "z" 'gnus-group-kill-all-zombies))

(defun gnus-group-mode ()
  "Major mode for reading news.

All normal editing commands are switched off.
\\<gnus-group-mode-map>
The group buffer lists (some of) the groups available.  For instance,
`\\[gnus-group-list-groups]' will list all subscribed groups with unread articles, while `\\[gnus-group-list-zombies]'
lists all zombie groups. 

Groups that are displayed can be entered with `\\[gnus-group-read-group]'.  To subscribe 
to a group not displayed, type `\\[gnus-group-unsubscribe-group]'. 

For more in-depth information on this mode, read the manual (`\\[gnus-info-find-node]'). 

The following commands are available:

\\{gnus-group-mode-map}"
  (interactive)
  (if gnus-visual (gnus-group-make-menu-bar))
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
  (run-hooks 'gnus-group-mode-hook))

(defun gnus-mouse-pick-group (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-group-read-group nil))

;; Look at LEVEL and find out what the level is really supposed to be.
;; If LEVEL is non-nil, LEVEL will be returned, if not, what happens
;; will depend on whether `gnus-group-use-permanent-levels' is used.
(defun gnus-group-default-level (&optional level number-or-nil)
  (cond  
   (gnus-group-use-permanent-levels
    (setq gnus-group-default-list-level 
	  (or level gnus-group-default-list-level))
    (or gnus-group-default-list-level gnus-level-subscribed))
   (number-or-nil
    level)
   (t
    (or level gnus-group-default-list-level gnus-level-subscribed))))
  

(defvar gnus-tmp-prev-perm nil)

;;;###autoload
(defun gnus-no-server (&optional arg)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level. If ARG is nil, Gnus will be started at level 2. 
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (let ((perm
	 (cons gnus-group-use-permanent-levels gnus-group-default-list-level)))
    (setq gnus-tmp-prev-perm nil)
    (setq gnus-group-use-permanent-levels t)
    (gnus (or arg (1- gnus-level-default-subscribed)) t)
    (setq gnus-tmp-prev-perm perm)))

;;;###autoload
(defun read-news (&optional arg dont-connect)
  "Read network news.  This is an alias for the `gnus' command."
  (gnus arg dont-connect))

;;;###autoload
(defun gnus (&optional arg dont-connect)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level. If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")
  (if (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-get-new-news))

    (gnus-clear-system)

    (nnheader-init-server-buffer)
    ;; We do this if `gnus-no-server' has been run.
    (if gnus-tmp-prev-perm 
	(setq gnus-group-use-permanent-levels (car gnus-tmp-prev-perm)
	      gnus-group-default-list-level (cdr gnus-tmp-prev-perm)
	      gnus-tmp-prev-perm nil))
    (gnus-read-init-file)

    (gnus-group-setup-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (if (not gnus-inhibit-startup-message)
	  (progn
	    (gnus-group-startup-message)
	    (sit-for 0))))
    
    (let ((level (and arg (numberp arg) (> arg 0) arg))
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
	  (and gnus-use-dribble-file (gnus-dribble-read-file))

	  (gnus-summary-make-display-table)
	  (gnus-setup-news nil level)
	  (gnus-group-list-groups level)
	  (gnus-configure-windows 'group))))))

(defun gnus-unload ()
  "Unload all Gnus features."
  (interactive)
  (or (boundp 'load-history)
      (error "Sorry, `gnus-unload' is not implemented in this Emacs version."))
  (let ((history load-history)
	feature)
    (while history
      (and (string-match "^gnus" (car (car history)))
	   (setq feature (cdr (assq 'provide (car history))))
	   (unload-feature feature 'force))
      (setq history (cdr history)))))

(defun gnus-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (insert
   (format "
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


      Gnus * A newsreader for Emacsen
    A Praxis release * larsi@ifi.uio.no
" 
	   gnus-version))
  ;; And then hack it.
  ;; 18 is the longest line.
  (indent-rigidly (point-min) (point-max) 
		  (/ (max (- (window-width) (or x 46)) 0) 2))
  (goto-char (point-min))
  (let* ((pheight (count-lines (point-min) (point-max)))
	 (wheight (window-height))
	 (rest (- wheight  pheight)))
    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n)))
    
    

  ;; Fontify some.
  (goto-char (point-min))
  (search-forward "Praxis")
  (put-text-property (match-beginning 0) (match-end 0) 'face 'bold)
  (goto-char (point-min)))

(defun gnus-group-startup-message-old (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (insert
   (format "
     %s
           A newsreader 
      for GNU Emacs

        Based on GNUS 
             written by 
     Masanobu UMEDA

       A Praxis Release
      larsi@ifi.uio.no
" 
	   gnus-version))
  ;; And then hack it.
  ;; 18 is the longest line.
  (indent-rigidly (point-min) (point-max) 
		  (/ (max (- (window-width) (or x 28)) 0) 2))
  (goto-char (point-min))
  ;; +4 is fuzzy factor.
  (insert-char ?\n (/ (max (- (window-height) (or y 12)) 0) 2))

  ;; Fontify some.
  (goto-char (point-min))
  (search-forward "Praxis")
  (put-text-property (match-beginning 0) (match-end 0) 'face 'bold)
  (goto-char (point-min)))

(defun gnus-group-setup-buffer ()
  (or (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-add-current-to-buffer-list)
	(gnus-group-mode)
	(and gnus-carpal (gnus-carpal-setup-buffer 'group)))))

(defun gnus-group-list-groups (&optional level unread)
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
  (let ((case-fold-search nil)
	(group (gnus-group-group-name)))
    (funcall gnus-group-prepare-function level unread nil)
    (if (zerop (buffer-size))
	(gnus-message 5 gnus-no-groups-message)
      (goto-char (point-min))
      (if (not group)
	  ;; Go to the first group with unread articles.
	  (gnus-group-search-forward nil nil nil t)
	;; Find the right group to put point on. If the current group
	;; has disappeared in the new listing, try to find the next
	;; one. If no next one can be found, just leave point at the
	;; first newsgroup in the buffer.
	(if (not (gnus-goto-char
		  (text-property-any (point-min) (point-max) 
				     'gnus-group (intern group))))
	    (let ((newsrc (nthcdr 3 (gnus-gethash group gnus-newsrc-hashtb))))
	      (while (and newsrc
			  (not (gnus-goto-char 
				(text-property-any 
				 (point-min) (point-max) 'gnus-group 
				 (intern (car (car newsrc)))))))
		(setq newsrc (cdr newsrc)))
	      (or newsrc (progn (goto-char (point-max))
				(forward-line -1))))))
      ;; Adjust cursor point.
      (gnus-group-position-cursor))))

(defun gnus-group-prepare-flat (level &optional all lowest regexp) 
  "List all newsgroups with unread articles of level LEVEL or lower.
If ALL is non-nil, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher.
If REGEXP, only list groups matching REGEXP."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(newsrc (cdr gnus-newsrc-alist))
	(lowest (or lowest 1))
	info clevel unread group)
    (erase-buffer)
    (if (< lowest gnus-level-zombie)
	;; List living groups.
	(while newsrc
	  (setq info (car newsrc)
		group (car info)
		newsrc (cdr newsrc)
		unread (car (gnus-gethash group gnus-newsrc-hashtb)))
	  (and unread			; This group might be bogus
	       (or (not regexp)
		   (string-match regexp group))
	       (<= (setq clevel (car (cdr info))) level) 
	       (>= clevel lowest)
	       (or all			; We list all groups?
		   (eq unread t)	; We list unactivated groups
		   (> unread 0)		; We list groups with unread articles
		   (cdr (assq 'tick (nth 3 info)))) ; And ticked groups
	       (gnus-group-insert-group-line 
		nil group (car (cdr info)) (nth 3 info) unread (nth 4 info)))))

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
  ;; suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>. It does
  ;; this by ignoring the group format specification altogether.
  (let (group beg)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (if (or (not regexp)
	      (string-match regexp group))
	  (progn
	    (setq beg (point))
	    (insert (format " %c     *: %s\n" mark group))
	    (add-text-properties 
	     beg (1+ beg) 
	     (list 'gnus-group (intern group)
		   'gnus-unread t
		   'gnus-level level)))))))

(defun gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  (if (string-match ":[^:]+$" group)
      (substring group (1+ (match-beginning 0)))
    group))

(defun gnus-group-prefixed-name (group method)
  "Return the whole name from GROUP and METHOD."
  (and (stringp method) (setq method (gnus-server-to-method method)))
  (concat (format "%s" (car method))
	  (if (and 
	       (assoc (format "%s" (car method)) (gnus-methods-using 'address))
	       (not (string= (nth 1 method) "")))
	      (concat "+" (nth 1 method)))
	  ":" group))

(defun gnus-group-real-prefix (group)
  "Return the prefix of the current group name."
  (if (string-match "^[^:]+:" group)
      (substring group 0 (match-end 0))
    ""))

(defun gnus-group-method-name (group)
  "Return the method used for selecting GROUP."
  (let ((prefix (gnus-group-real-prefix group)))
    (if (equal prefix "")
	gnus-select-method
      (if (string-match "^[^\\+]+\\+" prefix)
	  (list (intern (substring prefix 0 (1- (match-end 0))))
		(substring prefix (match-end 0) (1- (length prefix))))
	(list (intern (substring prefix 0 (1- (length prefix)))) "")))))

(defun gnus-group-foreign-p (group)
  "Return nil if GROUP is native, non-nil if it is foreign."
  (string-match ":" group))

(defun gnus-group-set-info (info &optional method-only-group part)
  (let* ((entry (gnus-gethash
		 (or method-only-group (car info)) gnus-newsrc-hashtb))
	 (part-info info)
	 (info (if method-only-group (nth 2 entry) info)))
    (if (not method-only-group)
	()
      (or entry
	  (error "Trying to change non-existent group %s" method-only-group))
      ;; We have received parts of the actual group info - either the
      ;; select method or the group parameters.  We first check
      ;; whether we have to extend the info, and if so, do that.
      (let ((len (length info))
	    (total (if (eq part 'method) 5 6)))
	(and (< len total)
	     (setcdr (nthcdr (1- len) info)
		     (make-list (- total len) nil)))
	;; Then we enter the new info.
	(setcar (nthcdr (1- total) info) part-info)))
    ;; We uncompress some lists of marked articles.
    (let (marked)
      (if (not (setq marked (nth 3 info)))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (setcdr (car marked) 
		      (gnus-uncompress-range (cdr (car marked)))))
	  (setq marked (cdr marked)))))
    (if entry
	()
      ;; This is a new group, so we just create it.
      (save-excursion
	(set-buffer gnus-group-buffer)
	(if (nth 4 info)
	    ;; It's a foreign group...
	    (gnus-group-make-group 
	     (gnus-group-real-name (car info))
	     (prin1-to-string (car (nth 4 info)))
	     (nth 1 (nth 4 info)))
	  ;; It's a native group.
	  (gnus-group-make-group (car info)))
	(gnus-message 6 "Note: New group created")
	(setq entry 
	      (gnus-gethash (gnus-group-prefixed-name 
			     (gnus-group-real-name (car info))
			     (or (nth 4 info) gnus-select-method))
			    gnus-newsrc-hashtb))))
    ;; Whether it was a new group or not, we now have the entry, so we
    ;; can do the update.
    (if entry
	(progn
	  (setcar (nthcdr 2 entry) info)
	  (if (and (not (eq (car entry) t)) 
		   (gnus-gethash (car info) gnus-active-hashtb))
	      (let ((marked (nth 3 info)))
		(setcar entry 
			(max 0 (- (length (gnus-list-of-unread-articles 
					   (car info)))
				  (length (cdr (assq 'tick marked)))
				  (length (cdr (assq 'dormant marked)))))))))
      (error "No such group: %s" (car info)))))

(defun gnus-group-set-method-info (group select-method)
  (gnus-group-set-info select-method group 'method))

(defun gnus-group-set-params-info (group params)
  (gnus-group-set-info params group 'params))

(defun gnus-group-update-group-line ()
  "This function updates the current line in the newsgroup buffer and
moves the point to the colon."
  (let* ((buffer-read-only nil)
	 (group (gnus-group-group-name))
	 (entry (and group (gnus-gethash group gnus-newsrc-hashtb))))
    (if (and entry (not (gnus-ephemeral-group-p group)))
	(gnus-dribble-enter 
	 (concat "(gnus-group-set-info '" (prin1-to-string (nth 2 entry))
		 ")")))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (gnus-group-insert-group-line-info group)
    (forward-line -1)
    (gnus-group-position-cursor)))

(defun gnus-group-insert-group-line-info (group)
  (let ((entry (gnus-gethash group gnus-newsrc-hashtb)) 
	active info)
    (if entry
	(progn
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line 
	   nil group (nth 1 info) (nth 3 info) (car entry) (nth 4 info)))
      (setq active (gnus-gethash group gnus-active-hashtb))
      (gnus-group-insert-group-line 
       nil group 
       (if (member group gnus-zombie-list) gnus-level-zombie gnus-level-killed)
       nil (if active (- (1+ (cdr active)) (car active)) 0) nil))))

(defun gnus-group-insert-group-line (gformat group level marked number method)
  (let* ((gformat (or gformat gnus-group-line-format-spec))
	 (active (gnus-gethash group gnus-active-hashtb))
	 (number-total (if active (1+ (- (cdr active) (car active))) 0))
	 (number-of-dormant (length (cdr (assq 'dormant marked))))
	 (number-of-ticked (length (cdr (assq 'tick marked))))
	 (number-of-ticked-and-dormant
	  (+ number-of-ticked number-of-dormant))
	 (number-of-unread-unticked 
	  (if (numberp number) (int-to-string (max 0 number))
	    "*"))
	 (number-of-read
	  (if (numberp number)
	      (max 0 (- number-total number))
	    "*"))
	 (subscribed (cond ((<= level gnus-level-subscribed) ? )
			   ((<= level gnus-level-unsubscribed) ?U)
			   ((= level gnus-level-zombie) ?Z)
			   (t ?K)))
	 (qualified-group (gnus-group-real-name group))
	 (newsgroup-description 
	  (if gnus-description-hashtb
	      (or (gnus-gethash group gnus-description-hashtb) "")
	    ""))
	 (moderated (if (member group gnus-moderated-list) ?m ? ))
	 (moderated-string (if (eq moderated ?m) "(m)" ""))
	 (method (gnus-server-get-method group method))
	 (news-server (or (car (cdr method)) ""))
	 (news-method (or (car method) ""))
	 (news-method-string 
	  (if method (format "(%s:%s)" (car method) (car (cdr method))) ""))
	 (marked (if (and 
		      (numberp number) 
		      (zerop number)
		      (> number-of-ticked 0))
		     ?* ? ))
	 (number (if (eq number t) "*" (+ number number-of-dormant 
					  number-of-ticked)))
	 (process-marked (if (member group gnus-group-marked)
			     gnus-process-mark ? ))
	 (buffer-read-only nil)
	 header				; passed as parameter to user-funcs.
	 b)
    (beginning-of-line)
    (setq b (point))
    ;; Insert the text.
    (insert (eval gformat))

    (add-text-properties 
     b (1+ b) (list 'gnus-group (intern group)
		    'gnus-unread (if (numberp number)
				     (string-to-int number-of-unread-unticked)
				   t)
		    'gnus-marked marked
		    'gnus-level level))))

(defun gnus-group-update-group (group &optional visible-only)
  "Update newsgroup info of GROUP.
If VISIBLE-ONLY is non-nil, the group won't be displayed if it isn't already."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (let ((buffer-read-only nil)
	  visible)
      (let ((entry (gnus-gethash group gnus-newsrc-hashtb)))
	(if (and entry
		 (not (gnus-ephemeral-group-p group)))
	    (gnus-dribble-enter 
	     (concat "(gnus-group-set-info '" (prin1-to-string (nth 2 entry))
		     ")"))))
      ;; Buffer may be narrowed.
      (save-restriction
	(widen)
	;; Search a line to modify.  If the buffer is large, the search
	;; takes long time.  In most cases, current point is on the line
	;; we are looking for.  So, first of all, check current line. 
	(if (or (progn
		  (beginning-of-line)
		  (eq (get-text-property (point) 'gnus-group)
		      (intern group)))
		(progn
		  (gnus-goto-char 
		   (text-property-any 
		    (point-min) (point-max) 'gnus-group (intern group)))))
	    ;; GROUP is listed in current buffer. So, delete old line.
	    (progn
	      (setq visible t)
	      (beginning-of-line)
	      (delete-region (point) (progn (forward-line 1) (point))))
	  ;; No such line in the buffer, find out where it's supposed to
	  ;; go, and insert it there (or at the end of the buffer).
	  ;; Fix by Per Abrahamsen <amanda@iesd.auc.dk>.
	  (or visible-only
	      (let ((entry 
		     (cdr (cdr (gnus-gethash group gnus-newsrc-hashtb)))))
		(while (and entry
			    (car entry)
			    (not
			     (gnus-goto-char
			      (text-property-any
			       (point-min) (point-max) 
			       'gnus-group (intern (car (car entry)))))))
		  (setq entry (cdr entry)))
		(or entry (goto-char (point-max)))))))
      (if (or visible (not visible-only))
	  (gnus-group-insert-group-line-info group))
      (gnus-group-set-mode-line))))

(defun gnus-group-set-mode-line ()
  (if (memq 'group gnus-updated-mode-lines)
      (let* ((gformat (or gnus-group-mode-line-format-spec
			  (setq gnus-group-mode-line-format-spec
				(gnus-parse-format 
				 gnus-group-mode-line-format 
				 gnus-group-mode-line-format-alist))))
	     (news-server (car (cdr gnus-select-method)))
	     (news-method (car gnus-select-method))
	     (max-len 60)
	     header			;Dummy binding for user-defined specs.
	     (mode-string (eval gformat)))
	(setq mode-string (eval gformat))
	(if (> (length mode-string) max-len) 
	    (setq mode-string (substring mode-string 0 (- max-len 4))))
	(setq mode-line-buffer-identification mode-string)
	(set-buffer-modified-p t))))

(defun gnus-group-group-name ()
  "Get the name of the newsgroup on the current line."
  (let ((group (get-text-property (gnus-point-at-bol) 'gnus-group)))
    (and group (symbol-name group))))

(defun gnus-group-group-level ()
  "Get the level of the newsgroup on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-level))

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
				(or (eq unread t) (and unread (> unread 0))))
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
	(progn (gnus-group-position-cursor) t)
      (goto-char (or pos beg))
      (and pos t))))

;;; Gnus group mode commands

;; Group marking.

(defun gnus-group-mark-group (n &optional unmark no-advance)
  "Mark the current group."
  (interactive "p")
  (let ((buffer-read-only nil)
	group)
    (while 
	(and (> n 0) 
	     (setq group (gnus-group-group-name))
	     (progn
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
		       (cons group (delete group gnus-group-marked))))
	       t)
	     (or no-advance (zerop (gnus-group-next-group 1))))
      (setq n (1- n)))
    (gnus-summary-position-cursor)
    n))

(defun gnus-group-unmark-group (n)
  "Remove the mark from the current group."
  (interactive "p")
  (gnus-group-mark-group n 'unmark))

(defun gnus-group-mark-region (unmark beg end)
  "Mark all groups between point and mark.
If UNMARK, remove the mark instead."
  (interactive "P\nr")
  (let ((num (count-lines beg end)))
    (save-excursion
      (goto-char beg)
      (- num (gnus-group-mark-group num unmark)))))

(defun gnus-group-remove-mark (group)
  (and (gnus-group-goto-group group)
       (save-excursion
	 (gnus-group-mark-group 1 'unmark t))))

;; Return a list of groups to work on.  Take into consideration N (the
;; prefix) and the list of marked groups.
(defun gnus-group-process-prefix (n)
  (cond (n
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
	       (forward-line way)))
	   (nreverse groups)))
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
readable. If the optional argument NO-ARTICLE is non-nil, no article
will be auto-selected upon group entry."
  (interactive "P")
  (let ((group (or group (gnus-group-group-name)))
	number active marked entry)
    (or group (error "No group on current line"))
    (setq marked 
	  (nth 3 (nth 2 (setq entry (gnus-gethash group gnus-newsrc-hashtb)))))
    ;; This group might be a dead group. In that case we have to get
    ;; the number of unread articles from `gnus-active-hashtb'.
    (if entry
	(setq number (car entry))
      (if (setq active (gnus-gethash group gnus-active-hashtb))
	  (setq number (- (1+ (cdr active)) (car active)))))
    (gnus-summary-read-group 
     group (or all (and (numberp number) 
			(zerop (+ number (length (cdr (assq 'tick marked)))
				  (length (cdr (assq 'dormant marked)))))))
     no-article)))

(defun gnus-group-select-group (&optional all)
  "Select this newsgroup.
No article is selected automatically.
If argument ALL is non-nil, already read articles become readable."
  (interactive "P")
  (gnus-group-read-group all t))

(defun gnus-group-select-group-all ()
  "Select the current group and display all articles in it."
  (interactive)
  (gnus-group-select-group 'all))

;; Enter a group that is not in the group buffer. Non-nil is returned
;; if selection was successful.
(defun gnus-group-read-ephemeral-group 
  (group method &optional activate quit-config)
  (let ((group (if (gnus-group-foreign-p group) group
		 (gnus-group-prefixed-name group method))))
    (gnus-sethash 
     group
     (list t nil (list group gnus-level-default-subscribed nil nil 
		       (append method
			       (list
				(list 'quit-config 
				      (if quit-config quit-config
					(cons (current-buffer) 'summary)))))))
     gnus-newsrc-hashtb)
    (set-buffer gnus-group-buffer)
    (or (gnus-check-server method)
	(error "Unable to contact server: %s" (gnus-status-message method)))
    (if activate (or (gnus-request-group group)
		     (error "Couldn't request group")))
    (condition-case ()
	(gnus-group-read-group t t group)
      (error nil)
      (quit nil))
    (not (equal major-mode 'gnus-group-mode))))
  
(defun gnus-group-jump-to-group (group)
  "Jump to newsgroup GROUP."
  (interactive 
   (list (completing-read 
	  "Group: " gnus-active-hashtb nil 
	  (memq gnus-select-method gnus-have-read-active-file))))

  (if (equal group "")
      (error "Empty group name"))

  (let ((b (text-property-any 
	    (point-min) (point-max) 'gnus-group (intern group))))
    (if b
	;; Either go to the line in the group buffer...
	(goto-char b)
      ;; ... or insert the line.
      (or
       (gnus-gethash group gnus-active-hashtb)
       (gnus-activate-group group)
       (error "%s error: %s" group (gnus-status-message group)))

      (gnus-group-update-group group)
      (goto-char (text-property-any 
		  (point-min) (point-max) 'gnus-group (intern group)))))
  ;; Adjust cursor point.
  (gnus-group-position-cursor))

(defun gnus-group-goto-group (group)
  "Goto to newsgroup GROUP."
  (let ((b (text-property-any (point-min) (point-max) 
			      'gnus-group (intern group))))
    (and b (goto-char b))))

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
  (gnus-group-position-cursor))

(defun gnus-group-prev-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t (gnus-group-group-level))
  (gnus-group-position-cursor))

(defun gnus-group-best-unread-group (&optional exclude-group)
  "Go to the group with the highest level.
If EXCLUDE-GROUP, do not go to that group."
  (interactive)
  (goto-char (point-min))
  (let ((best 100000)
	unread best-point)
    (while (setq unread (get-text-property (point) 'gnus-unread))
      (if (and (numberp unread) (> unread 0))
	  (progn
	    (if (and (< (get-text-property (point) 'gnus-level) best)
		     (or (not exclude-group)
			 (not (equal exclude-group (gnus-group-group-name)))))
		(progn 
		  (setq best (get-text-property (point) 'gnus-level))
		  (setq best-point (point))))))
      (forward-line 1))
    (if best-point (goto-char best-point))
    (gnus-summary-position-cursor)
    (and best-point (gnus-group-group-name))))

(defun gnus-group-first-unread-group ()
  "Go to the first group with unread articles."
  (interactive)
  (prog1
      (let ((opoint (point))
	    unread)
	(goto-char (point-min))
	(if (or (eq (setq unread (gnus-group-group-unread)) t) ; Not active.
		(not (zerop unread))	; Has unread articles.
		(zerop (gnus-group-next-unread-group 1))) ; Next unread group.
	    (point)			; Success.
	  (goto-char opoint)
	  nil))				; Not success.
    (gnus-group-position-cursor)))

(defun gnus-group-enter-server-mode ()
  "Jump to the server buffer."
  (interactive)
  (gnus-server-setup-buffer)
  (gnus-configure-windows 'server)
  (gnus-server-prepare))

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
	    nil t)))
      (if (assoc method gnus-valid-select-methods)
	  (list method
		(if (memq 'prompt-address
			  (assoc method gnus-valid-select-methods))
		    (read-string "Address: ")
		  ""))
	(list method nil)))))
  
  (let* ((meth (and method (if address (list (intern method) address) method)))
	 (nname (if method (gnus-group-prefixed-name name meth) name))
	 info)
    (and (gnus-gethash nname gnus-newsrc-hashtb)
	 (error "Group %s already exists" nname))
    (gnus-group-change-level 
     (setq info (list t nname gnus-level-default-subscribed nil nil meth))
     gnus-level-default-subscribed gnus-level-killed 
     (and (gnus-group-group-name)
	  (gnus-gethash (gnus-group-group-name)
			gnus-newsrc-hashtb))
     t)
    (gnus-sethash nname (cons 1 0) gnus-active-hashtb)
    (or (gnus-ephemeral-group-p name)
	(gnus-dribble-enter 
	 (concat "(gnus-group-set-info '" (prin1-to-string (cdr info)) ")")))
    (gnus-group-insert-group-line-info nname)

    (if (assoc method gnus-valid-select-methods)
	(require (intern method)))
    (and (gnus-check-backend-function 'request-create-group nname)
	 (gnus-request-create-group nname))))

(defun gnus-group-edit-group (group &optional part)
  "Edit the group on the current line."
  (interactive (list (gnus-group-group-name)))
  (let ((done-func '(lambda () 
		      "Exit editing mode and update the information."
		      (interactive)
		      (gnus-group-edit-group-done 'part 'group)))
	(part (or part 'info))
	(winconf (current-window-configuration))
	info)
    (or group (error "No group on current line"))
    (or (setq info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
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
    ;; We modify the func to let it know what part it is editing.
    (setcar (cdr (nth 4 done-func)) (list 'quote part))
    (setcar (cdr (cdr (nth 4 done-func))) group)
    (erase-buffer)
    (insert
     (cond 
      ((eq part 'method)
       ";; Type `C-c C-c' after editing the select method.\n\n")
      ((eq part 'params)
       ";; Type `C-c C-c' after editing the group parameters.\n\n")
      ((eq part 'info)
       ";; Type `C-c C-c' after editing the group info.\n\n")))
    (let ((cinfo (gnus-copy-sequence info))
	  marked)
      (if (not (setq marked (nth 3 cinfo)))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (not (numberp (car (cdr (car marked)))))
	      (setcdr (car marked) 
		      (gnus-compress-sequence (sort (cdr (car marked)) '<) t)))
	  (setq marked (cdr marked))))
      (insert 
       (pp-to-string
	(cond ((eq part 'method)
	       (or (nth 4 info) "native"))
	      ((eq part 'params)
	       (nth 5 info))
	      (t
	       cinfo)))
       "\n"))))

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
  (let ((form (read (current-buffer)))
	(winconf gnus-prev-winconf))
    (if (eq part 'info) 
	(gnus-group-set-info form)
      (gnus-group-set-info form group part))
    (kill-buffer (current-buffer))
    (and winconf (set-window-configuration winconf))
    (set-buffer gnus-group-buffer)
    (gnus-group-update-group (gnus-group-group-name))
    (gnus-group-position-cursor)))

(defun gnus-group-make-help-group ()
  "Create the Gnus documentation group."
  (interactive)
  (let ((path (if installation-directory
		  (cons (concat installation-directory "etc/") load-path)
	        (cons data-directory load-path)))
	(name (gnus-group-prefixed-name "gnus-help" '(nndoc "gnus-help")))
	file)
    (and (gnus-gethash name gnus-newsrc-hashtb)
	 (error "Documentation group already exists"))
    (while (and path
		(not (file-exists-p 
		      (setq file (concat (file-name-as-directory (car path))
					 "gnus-tut.txt")))))
      (setq path (cdr path)))
    (if (not path)
	(message "Couldn't find doc group")
      (gnus-group-make-group 
       (gnus-group-real-name name)
       (list 'nndoc name
	     (list 'nndoc-address file)
	     (list 'nndoc-article-type 'mbox)))))
  (gnus-group-position-cursor))

(defun gnus-group-make-doc-group (file type)
  "Create a group that uses a single file as the source."
  (interactive 
   (list (read-file-name "File name: ") 
	 (let ((err "")
	       found char)
	   (while (not found)
	     (message "%sFile type (mbox, babyl, digest) [mbd]: " err)
	     (setq found (cond ((= (setq char (read-char)) ?m) 'mbox)
			       ((= char ?b) 'babyl)
			       ((= char ?d) 'digest)
			       (t (setq err (format "%c unknown. " char))
				  nil))))
	   found)))
  (let* ((file (expand-file-name file))
	 (name (gnus-generate-new-group-name
		(gnus-group-prefixed-name
		 (file-name-nondirectory file) '(nndoc "")))))
    (gnus-group-make-group 
     (gnus-group-real-name name)
     (list 'nndoc name
	   (list 'nndoc-address file)
	   (list 'nndoc-article-type type)))))

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
     "nndir" 
     (if all gnus-group-archive-directory 
       gnus-group-recent-archive-directory)))
  (gnus-group-position-cursor))

(defun gnus-group-make-directory-group (dir)
  "Create an nndir group.
The user will be prompted for a directory. The contents of this
directory will be used as a newsgroup. The directory should contain
mail messages or news articles in files that have numeric names."
  (interactive
   (list (read-file-name "Create group from directory: ")))
  (or (file-exists-p dir) (error "No such directory"))
  (or (file-directory-p dir) (error "Not a directory"))
  (gnus-group-make-group dir "nndir" dir)
  (gnus-group-position-cursor))

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
			     "references" "chars" "lines" "xref")))
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
  (save-excursion
    (gnus-set-work-buffer)
    (let (emacs-lisp-mode-hook)
      (pp scores (current-buffer)))
    (write-region (point-min) (point-max) 
		  (concat (or gnus-kill-files-directory "~/News")
			  "nnkiboze:" group "." gnus-score-file-suffix)))
  (gnus-group-position-cursor))

(defun gnus-group-add-to-virtual (n vgroup)
  "Add the current group to a virtual group."
  (interactive
   (list current-prefix-arg
	 (completing-read "Add to virtual group: " gnus-newsrc-hashtb nil t
			  "nnvirtual:")))
  (or (eq (car (gnus-find-method-for-group vgroup)) 'nnvirtual)
      (error "%s is not an nnvirtual group" vgroup))
  (let* ((groups (gnus-group-process-prefix n))
	 (method (nth 4 (nth 2 (gnus-gethash vgroup gnus-newsrc-hashtb)))))
    (setcar (cdr method)
	    (concat 
	     (nth 1 method) "\\|"
	     (mapconcat 
	      (lambda (s) 
		(gnus-group-remove-mark s)
		(concat "\\(^" (regexp-quote s) "$\\)"))
	      groups "\\|"))))
  (gnus-group-position-cursor))

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
    (gnus-group-position-cursor)))

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

(defun gnus-group-sort-groups ()
  "Sort the group buffer using `gnus-group-sort-function'."
  (interactive)
  (setq gnus-newsrc-alist 
	(sort (cdr gnus-newsrc-alist) gnus-group-sort-function))
  (gnus-make-hashtable-from-newsrc-alist)
  (gnus-group-list-groups))

(defun gnus-group-sort-by-alphabet (info1 info2)
  (string< (car info1) (car info2)))

(defun gnus-group-sort-by-unread (info1 info2)
  (let ((n1 (car (gnus-gethash (car info1) gnus-newsrc-hashtb)))
	(n2 (car (gnus-gethash (car info2) gnus-newsrc-hashtb))))
    (< (or (and (numberp n1) n1) 0)
       (or (and (numberp n2) n2) 0))))

(defun gnus-group-sort-by-level (info1 info2)
  (< (nth 1 info1) (nth 1 info2)))

;; Group catching up.

(defun gnus-group-catchup-current (&optional n all)
  "Mark all articles not marked as unread in current newsgroup as read.
If prefix argument N is numeric, the ARG next newsgroups will be
caught up. If ALL is non-nil, marked articles will also be marked as
read. Cross references (Xref: header) of articles are ignored.
The difference between N and actual number of newsgroups that were
caught up is returned."
  (interactive "P")
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
	(if (prog1
		(gnus-group-goto-group (car groups))
	      (gnus-group-catchup (car groups) all))
	    (gnus-group-update-group-line)
	  (setq ret (1+ ret)))
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
	 (num (car entry))
	 (marked (nth 3 (nth 2 entry))))
    (if (not (numberp (car entry)))
	(gnus-message 1 "Can't catch up; non-active group")
      ;; Do the updating only if the newsgroup isn't killed.
      (if (not entry)
	  ()
	(gnus-update-read-articles 
	 group (and (not all) (append (cdr (assq 'tick marked))
				      (cdr (assq 'dormant marked))))
	 nil (and (not all) (cdr (assq 'tick marked))))
	(and all 
	     (setq marked (nth 3 (nth 2 entry)))
	     (setcar (nthcdr 3 (nth 2 entry)) 
		     (delq (assq 'dormant marked) 
			   (nth 3 (nth 2 entry)))))))
    num))

(defun gnus-group-expire-articles (&optional n)
  "Expire all expirable articles in the current newsgroup."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group)
    (or groups (error "No groups to expire"))
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (if (not (gnus-check-backend-function 'request-expire-articles group))
	  ()
	(let* ((info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	       (expirable (if (memq 'total-expire (nth 5 info))
			      (cons nil (gnus-list-of-read-articles group))
			    (assq 'expire (nth 3 info)))))
	  (and expirable 
	       (setcdr expirable
		       (gnus-request-expire-articles 
			(cdr expirable) group))))))))

(defun gnus-group-expire-all-groups ()
  "Expire all expirable articles in all newsgroups."
  (interactive)
  (save-excursion
    (gnus-message 5 "Expiring...")
    (let ((gnus-group-marked (mapcar (lambda (info) (car info))
				     (cdr gnus-newsrc-alist))))
      (gnus-group-expire-articles nil)))
  (gnus-group-position-cursor)
  (gnus-message 5 "Expiring...done"))

(defun gnus-group-set-current-level (n level)
  "Set the level of the next N groups to LEVEL."
  (interactive "P\nnLevel: ")
  (or (and (>= level 1) (<= level gnus-level-killed))
      (error "Illegal level: %d" level))
  (let ((groups (gnus-group-process-prefix n))
	group)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (gnus-message 6 "Changed level of %s from %d to %d" 
		    group (gnus-group-group-level) level)
      (gnus-group-change-level group level
			       (gnus-group-group-level))
      (gnus-group-update-group-line)))
  (gnus-group-position-cursor))

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
	       gnus-level-default-subscribed))
      (gnus-group-update-group-line))
    (gnus-group-next-group 1)))

(defun gnus-group-unsubscribe-group (group &optional level)
  "Toggle subscribe from/to unsubscribe GROUP.
New newsgroup is added to .newsrc automatically."
  (interactive
   (list (completing-read
	  "Group: " gnus-active-hashtb nil 
	  (memq gnus-select-method gnus-have-read-active-file))))
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
      (gnus-group-update-group group))
     ((and (stringp group)
	   (or (not (memq gnus-select-method gnus-have-read-active-file))
	       (gnus-gethash group gnus-active-hashtb)))
      ;; Add new newsgroup.
      (gnus-group-change-level 
       group 
       (if level level gnus-level-default-subscribed) 
       (or (and (member group gnus-zombie-list) 
		gnus-level-zombie) 
	   gnus-level-killed)
       (and (gnus-group-group-name)
	    (gnus-gethash (gnus-group-group-name) gnus-newsrc-hashtb)))
      (gnus-group-update-group group))
     (t (error "No such newsgroup: %s" group)))
    (gnus-group-position-cursor)))

(defun gnus-group-transpose-groups (n)
  "Move the current newsgroup up N places.
If given a negative prefix, move down instead. The difference between
N and the number of steps taken is returned." 
  (interactive "p")
  (or (gnus-group-group-name)
      (error "No group on current line"))
  (gnus-group-kill-group 1)
  (prog1
      (forward-line (- n))
    (gnus-group-yank-group)
    (gnus-group-position-cursor)))

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

(defun gnus-group-kill-group (&optional n)
  "The the next N groups.
The killed newsgroups can be yanked by using \\[gnus-group-yank-group].
However, only groups that were alive can be yanked; already killed 
groups or zombie groups can't be yanked.
The return value is the name of the (last) group that was killed."
  (interactive "P")
  (let ((buffer-read-only nil)
	(groups (gnus-group-process-prefix n))
	group entry level)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (setq level (gnus-group-group-level))
      (gnus-delete-line)
      (if (setq entry (gnus-gethash group gnus-newsrc-hashtb))
	  (setq gnus-list-of-killed-groups 
		(cons (cons (car entry) (nth 2 entry)) 
		      gnus-list-of-killed-groups)))
      (gnus-group-change-level 
       (if entry entry group) gnus-level-killed (if entry nil level)))
    (gnus-group-position-cursor)
    group))

(defun gnus-group-yank-group (&optional arg)
  "Yank the last newsgroups killed with \\[gnus-group-kill-group],
inserting it before the current newsgroup.  The numeric ARG specifies
how many newsgroups are to be yanked.  The name of the (last)
newsgroup yanked is returned."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (let (info group prev)
    (while (>= (setq arg (1- arg)) 0)
      (if (not (setq info (car gnus-list-of-killed-groups)))
	  (error "No more newsgroups to yank"))
      (setq group (nth 2 info))
      ;; Find which newsgroup to insert this one before - search
      ;; backward until something suitable is found. If there are no
      ;; other newsgroups in this buffer, just make this newsgroup the
      ;; first newsgroup.
      (setq prev (gnus-group-group-name))
      (gnus-group-change-level 
       info (nth 2 info) gnus-level-killed 
       (and prev (gnus-gethash prev gnus-newsrc-hashtb))
       t)
      (gnus-group-insert-group-line-info (nth 1 info))
      (setq gnus-list-of-killed-groups 
	    (cdr gnus-list-of-killed-groups)))
    (forward-line -1)
    (gnus-group-position-cursor)
    group))
      
(defun gnus-group-list-all-groups (&optional arg)
  "List all newsgroups with level ARG or lower.
Default is gnus-level-unsubscribed, which lists all subscribed and most
unsubscribed groups."
  (interactive "P")
  (gnus-group-list-groups (or arg gnus-level-unsubscribed) t))

(defun gnus-group-list-killed ()
  "List all killed newsgroups in the group buffer."
  (interactive)
  (if (not gnus-killed-list)
      (gnus-message 6 "No killed groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function 
	       gnus-level-killed t gnus-level-killed))
    (goto-char (point-min)))
  (gnus-group-position-cursor))

(defun gnus-group-list-zombies ()
  "List all zombie newsgroups in the group buffer."
  (interactive)
  (if (not gnus-zombie-list)
      (gnus-message 6 "No zombie groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function
	       gnus-level-zombie t gnus-level-zombie))
    (goto-char (point-min)))
  (gnus-group-position-cursor))

(defun gnus-group-get-new-news (&optional arg)
  "Get newly arrived articles.
If ARG is non-nil, it should be a number between one and nine to
specify which levels you are interested in re-scanning."
  (interactive "P")
  (run-hooks 'gnus-get-new-news-hook)
  (setq arg (gnus-group-default-level arg t))
  (if (and gnus-read-active-file (not arg))
      (progn
	(gnus-read-active-file)
	(gnus-get-unread-articles (or arg (1+ gnus-level-subscribed))))
    (let ((gnus-read-active-file (if arg nil gnus-read-active-file)))
      (gnus-get-unread-articles (or arg (1+ gnus-level-subscribed)))))
  (gnus-group-list-groups))

(defun gnus-group-get-new-news-this-group (&optional n)
  "Check for newly arrived news in the current group (and the N-1 next groups).
The difference between N and the number of newsgroup checked is returned.
If N is negative, this group and the N-1 previous groups will be checked."
  (interactive "P")
  (let* ((groups (gnus-group-process-prefix n))
	 (ret (if (numberp n) (- n (length groups)) 0))
	 group)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (or (gnus-get-new-news-in-group group)
	  (progn 
	    (ding) 
	    (message "%s error: %s" group (gnus-status-message group))
	    (sit-for 2))))
    (gnus-group-next-unread-group 1 t)
    (gnus-summary-position-cursor)
    ret))

(defun gnus-get-new-news-in-group (group)
  (and group 
       (gnus-activate-group group)
       (progn
	 (gnus-get-unread-articles-in-group 
	  (nth 2 (gnus-gethash group gnus-newsrc-hashtb))
	  (gnus-gethash group gnus-active-hashtb))
	 (gnus-group-update-group-line)
	 t)))

(defun gnus-group-fetch-faq (group)
  "Fetch the FAQ for the current group."
  (interactive (list (gnus-group-real-name (gnus-group-group-name))))
  (or group (error "No group name given"))
  (let ((file (concat gnus-group-faq-directory (gnus-group-real-name group))))
    (if (not (file-exists-p file))
	(error "No such file: %s" file)
      (find-file file))))
  
(defun gnus-group-describe-group (force &optional group)
  "Display a description of the current newsgroup."
  (interactive (list current-prefix-arg (gnus-group-group-name)))
  (and force (setq gnus-description-hashtb nil))
  (let ((method (gnus-find-method-for-group group))
	desc)
    (or group (error "No group name given"))
    (and (or (and gnus-description-hashtb
		  ;; We check whether this group's method has been
		  ;; queried for a description file.  
		  (gnus-gethash 
		   (gnus-group-prefixed-name "" method) 
		   gnus-description-hashtb))
	     (setq desc (gnus-group-get-description group))
	     (gnus-read-descriptions-file method))
	 (message
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
       (add-text-properties 
	b (1+ b) (list 'gnus-group group
		       'gnus-unread t 'gnus-marked nil
		       'gnus-level (1+ gnus-level-subscribed))))
     gnus-description-hashtb)
    (goto-char (point-min))
    (gnus-group-position-cursor)))

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
    ;; Go through all descriptions that are known to Gnus. 
    (if search-description
	(mapatoms 
	 (lambda (group)
	   (and (string-match regexp (symbol-value group))
		(gnus-gethash (symbol-name group) gnus-active-hashtb)
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
If LOWEST, don't list groups with level lower than LOWEST."
  (interactive "P\nsList newsgroups matching: ")
  (gnus-group-prepare-flat (or level gnus-level-subscribed)
			   all (or lowest 1) regexp)
  (goto-char (point-min))
  (gnus-group-position-cursor))

(defun gnus-group-list-all-matching (level regexp &optional lowest) 
  "List all groups that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups. 
If LOWEST, don't list groups with level lower than LOWEST."
  (interactive "P\nsList newsgroups matching: ")
  (gnus-group-list-matching (or level gnus-level-killed) regexp t lowest))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-group-save-newsrc ()
  "Save the Gnus startup files."
  (interactive)
  (gnus-save-newsrc-file))

(defun gnus-group-restart (&optional arg)
  "Force Gnus to read the .newsrc file."
  (interactive "P")
  (gnus-save-newsrc-file)
  (gnus-setup-news 'force)
  (gnus-group-list-groups arg))

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
    "Editing a global kill file (Type \\[gnus-kill-file-exit] to exit)")))

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
  (let ((group-buf (get-buffer gnus-group-buffer)))
    ;; Do this on a separate list in case the user does a ^G before we finish
    (let ((gnus-buffer-list
	   (delq group-buf (delq gnus-dribble-buffer
				 (append gnus-buffer-list nil)))))
      (while gnus-buffer-list
	(gnus-kill-buffer (car gnus-buffer-list))
	(setq gnus-buffer-list (cdr gnus-buffer-list))))
    (if group-buf
	(progn
	  (setq gnus-buffer-list (list group-buf))
	  (bury-buffer group-buf)
	  (delete-windows-on group-buf t)))))

(defun gnus-group-clear-dribble ()
  "Clear all information from the dribble buffer."
  (interactive)
  (gnus-dribble-clear))

(defun gnus-group-exit ()
  "Quit reading news after updating .newsrc.eld and .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (not (gnus-server-opened gnus-select-method)) ;NNTP connection closed
	  (not gnus-interactive-exit)	;Without confirmation
	  gnus-expert-user
	  (gnus-y-or-n-p "Are you sure you want to quit reading news? "))
      (progn
	(run-hooks 'gnus-exit-gnus-hook)
	;; Offer to save data from non-quitted summary buffers.
	(gnus-offer-save-summaries)
	;; Save the newsrc file(s).
	(gnus-save-newsrc-file)
	;; Kill-em-all.
	(gnus-close-backends)
	;; Reset everything.
	(gnus-clear-system))))

(defun gnus-close-backends ()
  ;; Send a close request to all backends that support such a request. 
  (let ((methods gnus-valid-select-methods)
	func)
    (while methods
      (if (fboundp (setq func (intern (concat (car (car methods))
					      "-request-close"))))
	  (funcall func))
      (setq methods (cdr methods)))))

(defun gnus-group-quit ()
  "Quit reading news without updating .newsrc.eld or .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (zerop (buffer-size))
	  (not (gnus-server-opened gnus-select-method))
	  gnus-expert-user
	  (not gnus-current-startup-file)
	  (gnus-yes-or-no-p
	   (format "Quit reading news without saving %s? "
		   (file-name-nondirectory gnus-current-startup-file))))
      (progn
	(run-hooks 'gnus-exit-gnus-hook)
	(if gnus-use-full-window
	    (delete-other-windows)
	  (gnus-remove-some-windows))
	(gnus-dribble-save)
	(gnus-close-backends)
	(gnus-clear-system))))

(defun gnus-offer-save-summaries ()
  (save-excursion
    (let ((buflist (buffer-list)) 
	  buffers bufname)
      (while buflist
	(and (setq bufname (buffer-name (car buflist)))
	     (string-match "Summary" bufname)
	     (save-excursion
	       (set-buffer bufname)
	       ;; We check that this is, indeed, a summary buffer.
	       (eq major-mode 'gnus-summary-mode))
	     (setq buffers (cons bufname buffers)))
	(setq buflist (cdr buflist)))
      (and buffers
	   (map-y-or-n-p 
	    "Update summary buffer %s? "
	    (lambda (buf)
	      (set-buffer buf)
	      (gnus-summary-exit))
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
		     nil t "nntp")))
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
;;; Browse Server Mode
;;;

(defvar gnus-browse-mode-hook nil)
(defvar gnus-browse-mode-map nil)
(put 'gnus-browse-mode 'mode-class 'special)

(if gnus-browse-mode-map
    nil
  (setq gnus-browse-mode-map (make-keymap))
  (suppress-keymap gnus-browse-mode-map)
  (define-key gnus-browse-mode-map " " 'gnus-browse-read-group)
  (define-key gnus-browse-mode-map "=" 'gnus-browse-select-group)
  (define-key gnus-browse-mode-map "n" 'gnus-browse-next-group)
  (define-key gnus-browse-mode-map "p" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "\177" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "N" 'gnus-browse-next-group)
  (define-key gnus-browse-mode-map "P" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "\M-n" 'gnus-browse-next-group)
  (define-key gnus-browse-mode-map "\M-p" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "\r" 'gnus-browse-select-group)
  (define-key gnus-browse-mode-map "u" 'gnus-browse-unsubscribe-current-group)
  (define-key gnus-browse-mode-map "l" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "L" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "q" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "Q" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "\C-c\C-c" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "?" 'gnus-browse-describe-briefly)
  (define-key gnus-browse-mode-map "\C-c\C-i" 'gnus-info-find-node)
  )

(defvar gnus-browse-current-method nil)
(defvar gnus-browse-return-buffer nil)

(defvar gnus-browse-buffer "*Gnus Browse Server*")

(defun gnus-browse-foreign-server (method &optional return-buffer)
  (setq gnus-browse-current-method method)
  (setq gnus-browse-return-buffer return-buffer)
  (let ((gnus-select-method method)
	groups group)
    (gnus-message 5 "Connecting to %s..." (nth 1 method))
    (or (gnus-check-server method)
	(error "Unable to contact server: %s" (gnus-status-message method)))
    (or (gnus-request-list method)
	(error "Couldn't request list: %s" (gnus-status-message method)))
    (get-buffer-create gnus-browse-buffer)
    (gnus-add-current-to-buffer-list)
    (and gnus-carpal (gnus-carpal-setup-buffer 'browse))
    (gnus-configure-windows 'browse)
    (buffer-disable-undo (current-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer))
    (gnus-browse-mode)
    (setq mode-line-buffer-identification
	  (format
	   "Gnus  Browse Server {%s:%s}" (car method) (car (cdr method))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (let ((cur (current-buffer)))
	(goto-char (point-min))
	(or (string= gnus-ignored-newsgroups "")
	    (delete-matching-lines gnus-ignored-newsgroups))
	(while (re-search-forward 
		"\\(^[^ \t]+\\)[ \t]+[0-9]+[ \t]+[0-9]+" nil t)
	  (goto-char (match-end 1))
	  (setq groups (cons (cons (buffer-substring (match-beginning 1)
						     (match-end 1))
				   (max 0 (- (1+ (read cur)) (read cur))))
			     groups)))))
    (setq groups (sort groups 
		       (lambda (l1 l2)
			 (string< (car l1) (car l2)))))
    (let ((buffer-read-only nil))
      (while groups
	(setq group (car groups))
	(insert 
	 (format "K%7d: %s\n" (cdr group) (car group)))
	(setq groups (cdr groups))))
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (gnus-group-position-cursor)))

(defun gnus-browse-mode ()
  "Major mode for browsing a foreign server.

All normal editing commands are switched off.

\\<gnus-browse-mode-map>
The only things you can do in this buffer is

1) `\\[gnus-browse-unsubscribe-current-group]' to subscribe to a group.
The group will be inserted into the group buffer upon exit from this
buffer.  

2) `\\[gnus-browse-read-group]' to read a group ephemerally.

3) `\\[gnus-browse-exit]' to return to the group buffer."
  (interactive)
  (kill-all-local-variables)
  (if gnus-visual (gnus-browse-make-menu-bar))
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-browse-mode)
  (setq mode-name "Browse Server")
  (setq mode-line-process nil)
  (use-local-map gnus-browse-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gnus-browse-mode-hook))

(defun gnus-browse-read-group (&optional no-article)
  "Enter the group at the current line."
  (interactive)
  (let ((group (gnus-browse-group-name)))
    (or (gnus-group-read-ephemeral-group 
	 group gnus-browse-current-method nil
	 (cons (current-buffer) 'browse))
	(error "Couldn't enter %s" group))))

(defun gnus-browse-select-group ()
  "Select the current group."
  (interactive)
  (gnus-browse-read-group 'no))

(defun gnus-browse-next-group (n)
  "Go to the next group."
  (interactive "p")
  (prog1
      (forward-line n)
    (gnus-group-position-cursor)))

(defun gnus-browse-prev-group (n)
  "Go to the next group."
  (interactive "p")
  (gnus-browse-next-group (- n)))

(defun gnus-browse-unsubscribe-current-group (arg)
  "(Un)subscribe to the next ARG groups."
  (interactive "p")
  (and (eobp)
       (error "No group at current line."))
  (let ((ward (if (< arg 0) -1 1))
	(arg (abs arg)))
    (while (and (> arg 0)
		(not (eobp))
		(gnus-browse-unsubscribe-group)
		(zerop (gnus-browse-next-group ward)))
      (setq arg (1- arg)))
    (gnus-group-position-cursor)
    (if (/= 0 arg) (gnus-message 7 "No more newsgroups"))
    arg))

(defun gnus-browse-group-name ()
  (save-excursion
    (beginning-of-line)
    (if (not (re-search-forward ": \\(.*\\)$" (gnus-point-at-eol) t))
	()
      (gnus-group-prefixed-name 
       (buffer-substring (match-beginning 1) (match-end 1))
       gnus-browse-current-method))))
  
(defun gnus-browse-unsubscribe-group ()
  "Toggle subscription of the current group in the browse buffer."
  (let ((sub nil)
	(buffer-read-only nil)
	group)
    (save-excursion
      (beginning-of-line)
      ;; If this group it killed, then we want to subscribe it.
      (if (= (following-char) ?K) (setq sub t))
      (setq group (gnus-browse-group-name))
      (delete-char 1)
      (if sub
	  (progn
	    (gnus-group-change-level 
	     (list t group gnus-level-default-subscribed
		   nil nil gnus-browse-current-method) 
	     gnus-level-default-subscribed gnus-level-killed
	     (and (car (nth 1 gnus-newsrc-alist))
		  (gnus-gethash (car (nth 1 gnus-newsrc-alist))
				gnus-newsrc-hashtb))
	     t)
	    (insert ? ))
	(gnus-group-change-level 
	 group gnus-level-killed gnus-level-default-subscribed)
	(insert ?K)))
    t))

(defun gnus-browse-exit ()
  "Quit browsing and return to the group buffer."
  (interactive)
  (if (eq major-mode 'gnus-browse-mode)
      (kill-buffer (current-buffer)))
  (if gnus-browse-return-buffer
      (gnus-configure-windows 'server 'force)
    (gnus-configure-windows 'group 'force)
    (gnus-group-list-groups nil)))

(defun gnus-browse-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-browse-mode-map>\\[gnus-group-next-group]:Forward  \\[gnus-group-prev-group]:Backward  \\[gnus-browse-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-browse-describe-briefly]:This help")))
      

;;;
;;; Gnus summary mode
;;;

(defvar gnus-summary-mode-map nil)
(defvar gnus-summary-mark-map nil)
(defvar gnus-summary-mscore-map nil)
(defvar gnus-summary-article-map nil)
(defvar gnus-summary-thread-map nil)
(defvar gnus-summary-goto-map nil)
(defvar gnus-summary-exit-map nil)
(defvar gnus-summary-interest-map nil)
(defvar gnus-summary-sort-map nil)
(defvar gnus-summary-backend-map nil)
(defvar gnus-summary-save-map nil)
(defvar gnus-summary-wash-map nil)
(defvar gnus-summary-wash-hide-map nil)
(defvar gnus-summary-wash-highlight-map nil)
(defvar gnus-summary-wash-time-map nil)
(defvar gnus-summary-help-map nil)

(put 'gnus-summary-mode 'mode-class 'special)

(if gnus-summary-mode-map
    nil
  (setq gnus-summary-mode-map (make-keymap))
  (suppress-keymap gnus-summary-mode-map)

  ;; Non-orthogonal keys

  (define-key gnus-summary-mode-map " " 'gnus-summary-next-page)
  (define-key gnus-summary-mode-map "\177" 'gnus-summary-prev-page)
  (define-key gnus-summary-mode-map "\r" 'gnus-summary-scroll-up)
  (define-key gnus-summary-mode-map "n" 'gnus-summary-next-unread-article)
  (define-key gnus-summary-mode-map "p" 'gnus-summary-prev-unread-article)
  (define-key gnus-summary-mode-map "N" 'gnus-summary-next-article)
  (define-key gnus-summary-mode-map "P" 'gnus-summary-prev-article)
  (define-key gnus-summary-mode-map "\M-\C-n" 'gnus-summary-next-same-subject)
  (define-key gnus-summary-mode-map "\M-\C-p" 'gnus-summary-prev-same-subject)
  (define-key gnus-summary-mode-map "\M-n" 'gnus-summary-next-unread-subject)
  (define-key gnus-summary-mode-map "\M-p" 'gnus-summary-prev-unread-subject)
  (define-key gnus-summary-mode-map "." 'gnus-summary-first-unread-article)
  (define-key gnus-summary-mode-map "," 'gnus-summary-best-unread-article)
  (define-key gnus-summary-mode-map 
    "\M-s" 'gnus-summary-search-article-forward)
  (define-key gnus-summary-mode-map 
    "\M-r" 'gnus-summary-search-article-backward)
  (define-key gnus-summary-mode-map "<" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-mode-map ">" 'gnus-summary-end-of-article)
  (define-key gnus-summary-mode-map "j" 'gnus-summary-goto-subject)
  (define-key gnus-summary-mode-map "^" 'gnus-summary-refer-parent-article)
  (define-key gnus-summary-mode-map "\M-^" 'gnus-summary-refer-article)
  (define-key gnus-summary-mode-map "u" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mode-map "!" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mode-map "U" 'gnus-summary-tick-article-backward)
  (define-key gnus-summary-mode-map "d" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mode-map "D" 'gnus-summary-mark-as-read-backward)
  (define-key gnus-summary-mode-map "E" 'gnus-summary-mark-as-expirable)
  (define-key gnus-summary-mode-map "\M-u" 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mode-map "\M-U" 'gnus-summary-clear-mark-backward)
  (define-key gnus-summary-mode-map 
    "k" 'gnus-summary-kill-same-subject-and-select)
  (define-key gnus-summary-mode-map "\C-k" 'gnus-summary-kill-same-subject)
  (define-key gnus-summary-mode-map "\M-\C-k" 'gnus-summary-kill-thread)
  (define-key gnus-summary-mode-map "\M-\C-l" 'gnus-summary-lower-thread)
  (define-key gnus-summary-mode-map "e" 'gnus-summary-edit-article)
  (define-key gnus-summary-mode-map "#" 'gnus-summary-mark-as-processable)
  (define-key gnus-summary-mode-map "\M-#" 'gnus-summary-unmark-as-processable)
  (define-key gnus-summary-mode-map "\M-\C-t" 'gnus-summary-toggle-threads)
  (define-key gnus-summary-mode-map "\M-\C-s" 'gnus-summary-show-thread)
  (define-key gnus-summary-mode-map "\M-\C-h" 'gnus-summary-hide-thread)
  (define-key gnus-summary-mode-map "\M-\C-f" 'gnus-summary-next-thread)
  (define-key gnus-summary-mode-map "\M-\C-b" 'gnus-summary-prev-thread)
  (define-key gnus-summary-mode-map "\M-\C-u" 'gnus-summary-up-thread)
  (define-key gnus-summary-mode-map "\M-\C-d" 'gnus-summary-down-thread)
  (define-key gnus-summary-mode-map "&" 'gnus-summary-execute-command)
  (define-key gnus-summary-mode-map "c" 'gnus-summary-catchup-and-exit)
  (define-key gnus-summary-mode-map "\C-w" 'gnus-summary-mark-region-as-read)
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-truncation)
  (define-key gnus-summary-mode-map "?" 'gnus-summary-mark-as-dormant)
  (define-key gnus-summary-mode-map 
    "\C-c\M-\C-s" 'gnus-summary-show-all-expunged)
  (define-key gnus-summary-mode-map 
    "\C-c\C-s\C-n" 'gnus-summary-sort-by-number)
  (define-key gnus-summary-mode-map 
    "\C-c\C-s\C-a" 'gnus-summary-sort-by-author)
  (define-key gnus-summary-mode-map 
    "\C-c\C-s\C-s" 'gnus-summary-sort-by-subject)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-d" 'gnus-summary-sort-by-date)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-i" 'gnus-summary-sort-by-score)
  (define-key gnus-summary-mode-map "=" 'gnus-summary-expand-window)
  (define-key gnus-summary-mode-map 
    "\C-x\C-s" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-mode-map "\M-g" 'gnus-summary-rescan-group)
  (define-key gnus-summary-mode-map "w" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-mode-map "\C-c\C-r" 'gnus-summary-caesar-message)
  (define-key gnus-summary-mode-map "\M-t" 'gnus-summary-toggle-mime)
  (define-key gnus-summary-mode-map "f" 'gnus-summary-followup)
  (define-key gnus-summary-mode-map "F" 'gnus-summary-followup-with-original)
  (define-key gnus-summary-mode-map "C" 'gnus-summary-cancel-article)
  (define-key gnus-summary-mode-map "r" 'gnus-summary-reply)
  (define-key gnus-summary-mode-map "R" 'gnus-summary-reply-with-original)
  (define-key gnus-summary-mode-map "\C-c\C-f" 'gnus-summary-mail-forward)
  (define-key gnus-summary-mode-map "o" 'gnus-summary-save-article)
  (define-key gnus-summary-mode-map "\C-o" 'gnus-summary-save-article-mail)
  (define-key gnus-summary-mode-map "|" 'gnus-summary-pipe-output)
  (define-key gnus-summary-mode-map "\M-k" 'gnus-summary-edit-local-kill)
  (define-key gnus-summary-mode-map "\M-K" 'gnus-summary-edit-global-kill)
  (define-key gnus-summary-mode-map "V" 'gnus-version)
  (define-key gnus-summary-mode-map "\C-c\C-d" 'gnus-summary-describe-group)
  (define-key gnus-summary-mode-map "q" 'gnus-summary-exit)
  (define-key gnus-summary-mode-map "Q" 'gnus-summary-exit-no-update)
  (define-key gnus-summary-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-summary-mode-map gnus-mouse-2 'gnus-mouse-pick-article)
  (define-key gnus-summary-mode-map "m" 'gnus-summary-mail-other-window)
  (define-key gnus-summary-mode-map "a" 'gnus-summary-post-news)
  (define-key gnus-summary-mode-map 
    "x" 'gnus-summary-remove-lines-marked-as-read)
; (define-key gnus-summary-mode-map "X" 'gnus-summary-remove-lines-marked-with)
  (define-key gnus-summary-mode-map "s" 'gnus-summary-isearch-article)
  (define-key gnus-summary-mode-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map "g" 'gnus-summary-show-article)
;  (define-key gnus-summary-mode-map "?" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-mode-map "l" 'gnus-summary-goto-last-article)
  (define-key gnus-summary-mode-map "\C-c\C-v\C-v" 'gnus-uu-decode-uu-view)
  (define-key gnus-summary-mode-map "\C-d" 'gnus-summary-enter-digest-group)
  (define-key gnus-summary-mode-map "v" 'gnus-summary-verbose-headers)
  (define-key gnus-summary-mode-map "\C-c\C-b" 'gnus-bug)


  ;; Sort of orthogonal keymap
  (define-prefix-command 'gnus-summary-mark-map)
  (define-key gnus-summary-mode-map "M" 'gnus-summary-mark-map)
  (define-key gnus-summary-mark-map "t" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mark-map "!" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mark-map "d" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mark-map "r" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mark-map "c" 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mark-map " " 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mark-map "e" 'gnus-summary-mark-as-expirable)
  (define-key gnus-summary-mark-map "x" 'gnus-summary-mark-as-expirable)
  (define-key gnus-summary-mark-map "?" 'gnus-summary-mark-as-dormant)
  (define-key gnus-summary-mark-map "b" 'gnus-summary-set-bookmark)
  (define-key gnus-summary-mark-map "B" 'gnus-summary-remove-bookmark)
  (define-key gnus-summary-mark-map "#" 'gnus-summary-mark-as-processable)
  (define-key gnus-summary-mark-map "\M-#" 'gnus-summary-unmark-as-processable)
  (define-key gnus-summary-mark-map 
    "\M-r" 'gnus-summary-remove-lines-marked-as-read)
  (define-key gnus-summary-mark-map 
    "\M-\C-r" 'gnus-summary-remove-lines-marked-with)
  (define-key gnus-summary-mark-map "D" 'gnus-summary-show-all-dormant)
  (define-key gnus-summary-mark-map "\M-D" 'gnus-summary-hide-all-dormant)
  (define-key gnus-summary-mark-map "S" 'gnus-summary-show-all-expunged)
  (define-key gnus-summary-mark-map "C" 'gnus-summary-catchup)
  (define-key gnus-summary-mark-map "H" 'gnus-summary-catchup-to-here)
  (define-key gnus-summary-mark-map "\C-c" 'gnus-summary-catchup-all)
  (define-key gnus-summary-mark-map 
    "k" 'gnus-summary-kill-same-subject-and-select)
  (define-key gnus-summary-mark-map "K" 'gnus-summary-kill-same-subject)

  (define-prefix-command 'gnus-summary-mscore-map)
  (define-key gnus-summary-mark-map "V" 'gnus-summary-mscore-map)
  (define-key gnus-summary-mscore-map "c" 'gnus-summary-clear-above)
  (define-key gnus-summary-mscore-map "u" 'gnus-summary-tick-above)
  (define-key gnus-summary-mscore-map "m" 'gnus-summary-mark-above)
  (define-key gnus-summary-mscore-map "k" 'gnus-summary-kill-below)

  (define-key gnus-summary-mark-map "P" 'gnus-uu-mark-map)
  
  (define-key gnus-summary-mode-map "S" 'gnus-summary-send-map)
  
  (define-prefix-command 'gnus-summary-goto-map)
  (define-key gnus-summary-mode-map "G" 'gnus-summary-goto-map)
  (define-key gnus-summary-goto-map "n" 'gnus-summary-next-unread-article)
  (define-key gnus-summary-goto-map "p" 'gnus-summary-prev-unread-article)
  (define-key gnus-summary-goto-map "N" 'gnus-summary-next-article)
  (define-key gnus-summary-goto-map "P" 'gnus-summary-prev-article)
  (define-key gnus-summary-goto-map "\C-n" 'gnus-summary-next-same-subject)
  (define-key gnus-summary-goto-map "\C-p" 'gnus-summary-prev-same-subject)
  (define-key gnus-summary-goto-map "\M-n" 'gnus-summary-next-unread-subject)
  (define-key gnus-summary-goto-map "\M-p" 'gnus-summary-prev-unread-subject)
  (define-key gnus-summary-goto-map "f" 'gnus-summary-first-unread-article)
  (define-key gnus-summary-goto-map "b" 'gnus-summary-best-unread-article)
  (define-key gnus-summary-goto-map "g" 'gnus-summary-goto-subject)
  (define-key gnus-summary-goto-map "l" 'gnus-summary-goto-last-article)
  (define-key gnus-summary-goto-map "p" 'gnus-summary-pop-article)


  (define-prefix-command 'gnus-summary-thread-map)
  (define-key gnus-summary-mode-map "T" 'gnus-summary-thread-map)
  (define-key gnus-summary-thread-map "k" 'gnus-summary-kill-thread)
  (define-key gnus-summary-thread-map "l" 'gnus-summary-lower-thread)
  (define-key gnus-summary-thread-map "i" 'gnus-summary-raise-thread)
  (define-key gnus-summary-thread-map "T" 'gnus-summary-toggle-threads)
  (define-key gnus-summary-thread-map "s" 'gnus-summary-show-thread)
  (define-key gnus-summary-thread-map "S" 'gnus-summary-show-all-threads)
  (define-key gnus-summary-thread-map "h" 'gnus-summary-hide-thread)
  (define-key gnus-summary-thread-map "H" 'gnus-summary-hide-all-threads)
  (define-key gnus-summary-thread-map "n" 'gnus-summary-next-thread)
  (define-key gnus-summary-thread-map "p" 'gnus-summary-prev-thread)
  (define-key gnus-summary-thread-map "u" 'gnus-summary-up-thread)
  (define-key gnus-summary-thread-map "d" 'gnus-summary-down-thread)
  (define-key gnus-summary-thread-map "#" 'gnus-uu-mark-thread)

  
  (define-prefix-command 'gnus-summary-exit-map)
  (define-key gnus-summary-mode-map "Z" 'gnus-summary-exit-map)
  (define-key gnus-summary-exit-map "c" 'gnus-summary-catchup-and-exit)
  (define-key gnus-summary-exit-map "C" 'gnus-summary-catchup-all-and-exit)
  (define-key gnus-summary-exit-map "E" 'gnus-summary-exit-no-update)
  (define-key gnus-summary-exit-map "Q" 'gnus-summary-exit)
  (define-key gnus-summary-exit-map "Z" 'gnus-summary-exit)
  (define-key gnus-summary-exit-map 
    "n" 'gnus-summary-catchup-and-goto-next-group)
  (define-key gnus-summary-exit-map "R" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-exit-map "G" 'gnus-summary-rescan-group)
  (define-key gnus-summary-exit-map "N" 'gnus-summary-next-group)
  (define-key gnus-summary-exit-map "P" 'gnus-summary-prev-group)


  (define-prefix-command 'gnus-summary-article-map)
  (define-key gnus-summary-mode-map "A" 'gnus-summary-article-map)
  (define-key gnus-summary-article-map " " 'gnus-summary-next-page)
  (define-key gnus-summary-article-map "n" 'gnus-summary-next-page)
  (define-key gnus-summary-article-map "\177" 'gnus-summary-prev-page)
  (define-key gnus-summary-article-map "p" 'gnus-summary-prev-page)
  (define-key gnus-summary-article-map "\r" 'gnus-summary-scroll-up)
  (define-key gnus-summary-article-map "<" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-article-map ">" 'gnus-summary-end-of-article)
  (define-key gnus-summary-article-map "b" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-article-map "e" 'gnus-summary-end-of-article)
  (define-key gnus-summary-article-map "^" 'gnus-summary-refer-parent-article)
  (define-key gnus-summary-article-map "r" 'gnus-summary-refer-parent-article)
  (define-key gnus-summary-article-map "g" 'gnus-summary-show-article)
  (define-key gnus-summary-article-map "s" 'gnus-summary-isearch-article)



  (define-prefix-command 'gnus-summary-wash-map)
  (define-key gnus-summary-mode-map "W" 'gnus-summary-wash-map)

  (define-prefix-command 'gnus-summary-wash-hide-map)
  (define-key gnus-summary-wash-map "W" 'gnus-summary-wash-hide-map)
  (define-key gnus-summary-wash-hide-map "a" 'gnus-article-hide)
  (define-key gnus-summary-wash-hide-map "h" 'gnus-article-hide-headers)
  (define-key gnus-summary-wash-hide-map "s" 'gnus-article-hide-signature)
  (define-key gnus-summary-wash-hide-map "c" 'gnus-article-hide-citation)
  (define-key gnus-summary-wash-hide-map 
    "\C-c" 'gnus-article-hide-citation-maybe)

  (define-prefix-command 'gnus-summary-wash-highlight-map)
  (define-key gnus-summary-wash-map "H" 'gnus-summary-wash-highlight-map)
  (define-key gnus-summary-wash-highlight-map "a" 'gnus-article-highlight)
  (define-key gnus-summary-wash-highlight-map 
    "h" 'gnus-article-highlight-headers)
  (define-key gnus-summary-wash-highlight-map
    "c" 'gnus-article-highlight-citation)
  (define-key gnus-summary-wash-highlight-map
    "s" 'gnus-article-highlight-signature)

  (define-prefix-command 'gnus-summary-wash-time-map)
  (define-key gnus-summary-wash-map "T" 'gnus-summary-wash-time-map)
  (define-key gnus-summary-wash-time-map "z" 'gnus-article-date-ut)
  (define-key gnus-summary-wash-time-map "u" 'gnus-article-date-ut)
  (define-key gnus-summary-wash-time-map "l" 'gnus-article-date-local)
  (define-key gnus-summary-wash-time-map "e" 'gnus-article-date-lapsed)

  (define-key gnus-summary-wash-map "b" 'gnus-article-add-buttons)
  (define-key gnus-summary-wash-map "o" 'gnus-article-treat-overstrike)
  (define-key gnus-summary-wash-map "w" 'gnus-article-word-wrap)
  (define-key gnus-summary-wash-map "c" 'gnus-article-remove-cr)
  (define-key gnus-summary-wash-map "q" 'gnus-article-de-quoted-unreadable)
  (define-key gnus-summary-wash-map "f" 'gnus-article-display-x-face)
  (define-key gnus-summary-wash-map "l" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-wash-map "r" 'gnus-summary-caesar-message)
  (define-key gnus-summary-wash-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-wash-map "m" 'gnus-summary-toggle-mime)


  (define-prefix-command 'gnus-summary-help-map)
  (define-key gnus-summary-mode-map "H" 'gnus-summary-help-map)
  (define-key gnus-summary-help-map "v" 'gnus-version)
  (define-key gnus-summary-help-map "f" 'gnus-summary-fetch-faq)
  (define-key gnus-summary-help-map "d" 'gnus-summary-describe-group)
  (define-key gnus-summary-help-map "h" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-help-map "i" 'gnus-info-find-node)


  (define-prefix-command 'gnus-summary-backend-map)
  (define-key gnus-summary-mode-map "B" 'gnus-summary-backend-map)
  (define-key gnus-summary-backend-map "e" 'gnus-summary-expire-articles)
  (define-key gnus-summary-backend-map "\M-\C-e" 
    'gnus-summary-expire-articles-now)
  (define-key gnus-summary-backend-map "\177" 'gnus-summary-delete-article)
  (define-key gnus-summary-backend-map "m" 'gnus-summary-move-article)
  (define-key gnus-summary-backend-map "r" 'gnus-summary-respool-article)
  (define-key gnus-summary-backend-map "w" 'gnus-summary-edit-article)
  (define-key gnus-summary-backend-map "c" 'gnus-summary-copy-article)
  (define-key gnus-summary-backend-map "q" 'gnus-summary-fancy-query)
  (define-key gnus-summary-backend-map "i" 'gnus-summary-import-article)


  (define-prefix-command 'gnus-summary-save-map)
  (define-key gnus-summary-mode-map "O" 'gnus-summary-save-map)
  (define-key gnus-summary-save-map "o" 'gnus-summary-save-article)
  (define-key gnus-summary-save-map "m" 'gnus-summary-save-article-mail)
  (define-key gnus-summary-save-map "r" 'gnus-summary-save-article-rmail)
  (define-key gnus-summary-save-map "f" 'gnus-summary-save-article-file)
  (define-key gnus-summary-save-map "h" 'gnus-summary-save-article-folder)
  (define-key gnus-summary-save-map "v" 'gnus-summary-save-article-vm)
  (define-key gnus-summary-save-map "p" 'gnus-summary-pipe-output)
;  (define-key gnus-summary-save-map "s" 'gnus-soup-add-article)

  (define-key gnus-summary-mode-map "X" 'gnus-uu-extract-map)

  (define-key gnus-summary-mode-map "\M-&" 'gnus-summary-universal-argument)
;  (define-key gnus-summary-various-map "\C-s" 'gnus-summary-search-article-forward)
;  (define-key gnus-summary-various-map "\C-r" 'gnus-summary-search-article-backward)
;  (define-key gnus-summary-various-map "r" 'gnus-summary-refer-article)
;  (define-key gnus-summary-various-map "&" 'gnus-summary-execute-command)
;  (define-key gnus-summary-various-map "T" 'gnus-summary-toggle-truncation)
;  (define-key gnus-summary-various-map "e" 'gnus-summary-expand-window)
  (define-key gnus-summary-article-map "D" 'gnus-summary-enter-digest-group)
;  (define-key gnus-summary-various-map "k" 'gnus-summary-edit-local-kill)
;  (define-key gnus-summary-various-map "K" 'gnus-summary-edit-global-kill)

  (define-key gnus-summary-mode-map "V" 'gnus-summary-score-map)

;  (define-prefix-command 'gnus-summary-sort-map)
;  (define-key gnus-summary-various-map "s" 'gnus-summary-sort-map)
;  (define-key gnus-summary-sort-map "n" 'gnus-summary-sort-by-number)
;  (define-key gnus-summary-sort-map "a" 'gnus-summary-sort-by-author)
;  (define-key gnus-summary-sort-map "s" 'gnus-summary-sort-by-subject)
;  (define-key gnus-summary-sort-map "d" 'gnus-summary-sort-by-date)
;  (define-key gnus-summary-sort-map "i" 'gnus-summary-sort-by-score)

  (define-key gnus-summary-mode-map "I" 'gnus-summary-increase-score)
  (define-key gnus-summary-mode-map "L" 'gnus-summary-lower-score)
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
follow up an article, type `\\[gnus-summary-followup]'.  To mail a reply to the author 
of an article, type `\\[gnus-summary-reply]'.

There are approx. one gazillion commands you can execute in this 
buffer; read the info pages for more information (`\\[gnus-info-find-node]'). 

The following commands are available:

\\{gnus-summary-mode-map}"
  (interactive)
  (if gnus-visual (gnus-summary-make-menu-bar))
  (kill-all-local-variables)
  (let ((locals gnus-summary-local-variables))
    (while locals
      (if (consp (car locals))
	  (progn
	    (make-local-variable (car (car locals)))
	    (set (car (car locals)) (eval (cdr (car locals)))))
	(make-local-variable (car locals))
	(set (car locals) nil))
      (setq locals (cdr locals))))
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
  (run-hooks 'gnus-summary-mode-hook))

(defun gnus-summary-make-display-table ()
  ;; Change the display table.  Odd characters have a tendency to mess
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
	  (and (vectorp (car (car locals)))
	       (set (car (car locals)) nil))
	(and (vectorp (car locals))
	     (set (car locals) nil)))
      (setq locals (cdr locals)))))

;; Some summary mode macros.

;; Return a header specified by a NUMBER.
(defun gnus-get-header-by-number (number)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (or gnus-newsgroup-headers-hashtb-by-number
	(gnus-make-headers-hashtable-by-number))
    (gnus-gethash (int-to-string number)
		  gnus-newsgroup-headers-hashtb-by-number)))

;; Fast version of the function above.
(defmacro gnus-get-header-by-num (number)
  (` (gnus-gethash (int-to-string (, number)) 
		   gnus-newsgroup-headers-hashtb-by-number)))

(defmacro gnus-summary-search-forward (&optional unread subject backward)
  "Search for article forward.
If UNREAD is non-nil, only unread articles are selected.
If SUBJECT is non-nil, the article which has the same subject will be
searched for. 
If BACKWARD is non-nil, the search will be performed backwards instead."
  (` (gnus-summary-search-subject (, backward) (, unread) (, subject))))

(defmacro gnus-summary-search-backward (&optional unread subject)
  "Search for article backward.
If 1st optional argument UNREAD is non-nil, only unread article is selected.
If 2nd optional argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (` (gnus-summary-search-forward (, unread) (, subject) t)))

(defmacro gnus-summary-article-number (&optional number-or-nil)
  "The article number of the article on the current line.
If there isn's an article number here, then we return the current
article number."
  (if number-or-nil
      '(get-text-property (gnus-point-at-bol) 'gnus-number)
    '(or (get-text-property (gnus-point-at-bol) 'gnus-number) 
	 gnus-current-article)))

(defmacro gnus-summary-thread-level ()
  "The thread level of the article on the current line."
  '(or (get-text-property (gnus-point-at-bol) 'gnus-level)
       0))

(defmacro gnus-summary-article-mark ()
  "The mark on the current line."
  '(get-text-property (gnus-point-at-bol) 'gnus-mark))

(defun gnus-summary-subject-string ()
  "Return current subject string or nil if nothing."
  (let ((article (gnus-summary-article-number))
	header)
    (and article 
	 (setq header (gnus-get-header-by-num article))
	 (vectorp header)
	 (mail-header-subject header))))

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
	  (not gnus-newsgroup-begin))
      ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
      (setq gnus-summary-buffer (set-buffer (get-buffer-create buffer)))
      (gnus-add-current-to-buffer-list)
      (gnus-summary-mode group)
      (and gnus-carpal (gnus-carpal-setup-buffer 'summary))
      (setq gnus-newsgroup-name group)
      t)))

(defun gnus-set-global-variables ()
  ;; Set the global equivalents of the summary buffer-local variables
  ;; to the latest values they had. These reflect the summary buffer
  ;; that was in action when the last article was fetched.
  (if (eq major-mode 'gnus-summary-mode) 
      (progn
	(setq gnus-summary-buffer (current-buffer))
	(let ((name gnus-newsgroup-name)
	      (marked gnus-newsgroup-marked)
	      (unread gnus-newsgroup-unreads)
	      (headers gnus-current-headers)
	      (score-file gnus-current-score-file))
	  (save-excursion
	    (set-buffer gnus-group-buffer)
	    (setq gnus-newsgroup-name name)
	    (setq gnus-newsgroup-marked marked)
	    (setq gnus-newsgroup-unreads unread)
	    (setq gnus-current-headers headers)
	    (setq gnus-current-score-file score-file))))))

(defun gnus-summary-insert-dummy-line (sformat subject number)
  (if (not sformat) 
      (setq sformat gnus-summary-dummy-line-format-spec))
  (let (b)
    (beginning-of-line)
    (setq b (point))
    (insert (eval sformat))
    (add-text-properties
     b (1+ b)
     (list 'gnus-number number 
	   'gnus-mark gnus-dummy-mark
	   'gnus-level 0))))

(defvar gnus-thread-indent-array nil)
(defvar gnus-thread-indent-array-level gnus-thread-indent-level)
(defun gnus-make-thread-indent-array ()
  (let ((n 200))
    (if (and gnus-thread-indent-array
	     (= gnus-thread-indent-level gnus-thread-indent-array-level))
	nil
      (setq gnus-thread-indent-array (make-vector 201 "")
	    gnus-thread-indent-array-level gnus-thread-indent-level)
      (while (>= n 0)
	(aset gnus-thread-indent-array n
	      (make-string (* n gnus-thread-indent-level) ? ))
	(setq n (1- n))))))

(defun gnus-summary-insert-line 
  (sformat header level current unread replied expirable subject-or-nil
	   &optional dummy score process)
  (or sformat (setq sformat gnus-summary-line-format-spec))
  (let* ((indentation (aref gnus-thread-indent-array level))
	 (lines (mail-header-lines header))
	 (score (or score gnus-summary-default-score 0))
	 (score-char
	  (if (or (null gnus-summary-default-score)
		  (<= (abs (- score gnus-summary-default-score))
		      gnus-summary-zcore-fuzz)) ? 
	    (if (< score gnus-summary-default-score)
		gnus-score-below-mark gnus-score-over-mark)))
	 (replied (cond (process gnus-process-mark)
			(replied gnus-replied-mark)
			(t gnus-unread-mark)))
	 (from (mail-header-from header))
	 (name (cond 
		((string-match "(.+)" from)
		 (substring from (1+ (match-beginning 0)) (1- (match-end 0))))
		((string-match "<[^>]+> *$" from)
		 (let ((beg (match-beginning 0)))
		   (or (and (string-match "^\"[^\"]*\"" from)
			    (substring from (1+ (match-beginning 0))
				       (1- (match-end 0))))
		       (substring from 0 beg))))
		(t from)))
	 (subject (mail-header-subject header))
	 (number (mail-header-number header))
	 (opening-bracket (if dummy ?\< ?\[))
	 (closing-bracket (if dummy ?\> ?\]))
	 (buffer-read-only nil)
	 (b (progn (beginning-of-line) (point))))
    (or (numberp lines) (setq lines 0))
    (insert (eval sformat))
    (add-text-properties
     b (1+ b) (list 'gnus-number number 
		    'gnus-mark (or unread gnus-unread-mark)
		    'gnus-level level))))

(defun gnus-summary-update-line (&optional dont-update)
  ;; Update summary line after change.
  (or (not gnus-summary-default-score)
      gnus-summary-inhibit-highlight
      (let ((gnus-summary-inhibit-highlight t)
	    (article (gnus-summary-article-number)))
	(progn
	  (or dont-update
	      (if (and gnus-summary-mark-below
		       (< (gnus-summary-article-score)
			  gnus-summary-mark-below))
		  (and (not (memq article gnus-newsgroup-marked))
		       (not (memq article gnus-newsgroup-dormant))
		       (memq article gnus-newsgroup-unreads)
		       (gnus-summary-mark-article-as-read gnus-low-score-mark))
		(and (eq (gnus-summary-article-mark) gnus-low-score-mark)
		     (gnus-summary-mark-article-as-unread gnus-unread-mark))))
	  (and gnus-visual
	       (run-hooks 'gnus-summary-update-hook))))))

(defun gnus-summary-update-lines (&optional beg end)
  ;; Mark article as read (or not) by taking into account scores.
  (let ((beg (or beg (point-min)))
	(end (or end (point-max))))
    (if (or (not gnus-summary-default-score)
	    gnus-summary-inhibit-highlight)
	()
      (let ((gnus-summary-inhibit-highlight t)
	    article)
	(save-excursion
	  (set-buffer gnus-summary-buffer)
	  (goto-char beg)
	  (beginning-of-line)
	  (while (and (not (eobp)) (< (point) end))
	    (if (and gnus-summary-mark-below
		     (< (or (cdr (assq 
				  (setq article (get-text-property 
						 (point) 'gnus-number))
				  gnus-newsgroup-scored))
			    gnus-summary-default-score 0)
			gnus-summary-mark-below))
		;; We want to possibly mark it as read...
		(and (not (memq article gnus-newsgroup-marked))
		     (not (memq article gnus-newsgroup-dormant))
		     (memq article gnus-newsgroup-unreads)
		     (gnus-summary-mark-article-as-read gnus-low-score-mark))
	      ;; We want to possibly mark it as unread.
	      (and (eq (get-text-property (point) 'gnus-mark)
		       gnus-low-score-mark)
		   (gnus-summary-mark-article-as-unread gnus-unread-mark)))
	    ;; Do the visual highlights at the same time.
	    (and gnus-visual (run-hooks 'gnus-summary-update-hook))
	    (forward-line 1)))))))

(defvar gnus-tmp-gathered nil)

(defun gnus-summary-number-of-articles-in-thread (thread &optional char)
  ;; Sum up all elements (and sub-elements) in a list.
  (let* ((number
	  ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
	  (if (and (consp thread) (cdr thread))
	      (apply
	       '+ 1 (mapcar
		     'gnus-summary-number-of-articles-in-thread 
		     (cdr thread)))
	    1)))
    (if char 
	(if (> number 1) gnus-not-empty-thread-mark
	  gnus-empty-thread-mark)
      number)))

(defun gnus-summary-read-group 
  (group &optional show-all no-article kill-buffer)
  "Start reading news in newsgroup GROUP.
If SHOW-ALL is non-nil, already read articles are also listed.
If NO-ARTICLE is non-nil, no article is selected initially."
  (gnus-message 5 "Retrieving newsgroup: %s..." group)
  (let* ((new-group (gnus-summary-setup-buffer group))
	 (quit-config (gnus-group-quit-config group))
	 (did-select (and new-group (gnus-select-newsgroup group show-all))))
    (cond 
     ((not new-group)
      (gnus-set-global-variables)
      (gnus-kill-buffer kill-buffer)
      (gnus-configure-windows 'summary 'force)
      (gnus-set-mode-line 'summary)
      (gnus-summary-position-cursor)
      (message "")
      t)
     ((null did-select) 
      (and (eq major-mode 'gnus-summary-mode)
	   (not (equal (current-buffer) kill-buffer))
	   (progn
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
		 (gnus-configure-windows (cdr quit-config))))))
      (message "Can't select group")
      nil)
     ((eq did-select 'quit)
      (and (eq major-mode 'gnus-summary-mode)
	   (not (equal (current-buffer) kill-buffer))
	   (kill-buffer (current-buffer)))
      (gnus-kill-buffer kill-buffer)
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
      (signal 'quit nil))
     (t
      (gnus-set-global-variables)
      ;; Save the active value in effect when the group was entered.
      (setq gnus-newsgroup-active 
	    (gnus-copy-sequence
	     (gnus-gethash gnus-newsgroup-name gnus-active-hashtb)))
      ;; You can change the subjects in this hook.
      (run-hooks 'gnus-select-group-hook)
      ;; Do score processing.
      (and gnus-use-scoring (gnus-possibly-score-headers))
      (gnus-update-format-specifications)
      ;; Generate the summary buffer.
      (gnus-summary-prepare)
      (if (zerop (buffer-size))
	  (cond (gnus-newsgroup-dormant
		 (gnus-summary-show-all-dormant))
		((and gnus-newsgroup-scored show-all)
		 (gnus-summary-show-all-expunged))))
      ;; Function `gnus-apply-kill-file' must be called in this hook.
      (run-hooks 'gnus-apply-kill-hook)
      (if (zerop (buffer-size))
	  (progn
	    ;; This newsgroup is empty.
	    (gnus-summary-catchup-and-exit nil t) ;Without confirmations.
	    (gnus-message 6 "No unread news")
	    (gnus-kill-buffer kill-buffer)
	    nil)
	;;(save-excursion
	;;  (if kill-buffer
	;;      (let ((gnus-summary-buffer kill-buffer))
	;;	(gnus-configure-windows 'group))))
	;; Hide conversation thread subtrees.  We cannot do this in
	;; gnus-summary-prepare-hook since kill processing may not
	;; work with hidden articles.
	(and gnus-show-threads
	     gnus-thread-hide-subtree
	     (gnus-summary-hide-all-threads))
	;; Show first unread article if requested.
	(goto-char (point-min))
	(if (and (not no-article)
		 gnus-auto-select-first
		 (gnus-summary-first-unread-article))
	    ()
	  (gnus-configure-windows 'summary 'force))
	(gnus-set-mode-line 'summary)
	(gnus-summary-position-cursor)
	;; If in async mode, we send some info to the backend.
	(and gnus-newsgroup-async
	     (setq gnus-newsgroup-threads (nreverse gnus-newsgroup-threads))
	     (gnus-request-asynchronous 
	      gnus-newsgroup-name
	      (if (and gnus-asynchronous-article-function
		       (fboundp gnus-asynchronous-article-function))
		  (funcall gnus-asynchronous-article-function
			   gnus-newsgroup-threads)
		gnus-newsgroup-threads)))
	(gnus-kill-buffer kill-buffer)
	(if (not (get-buffer-window gnus-group-buffer))
	    ()
	  ;; gotta use windows, because recenter does weird stuff if
	  ;; the current buffer ain't the displayed window.
 	  (let ((owin (selected-window))) 
 	    (select-window (get-buffer-window gnus-group-buffer))
  	    (and (gnus-group-goto-group group)
  		 (recenter))
 	    (select-window owin))))
      t))))

(defun gnus-summary-prepare ()
  ;; Generate the summary buffer.
  (let ((buffer-read-only nil))
    (erase-buffer)
    (gnus-summary-prepare-threads 
     (if gnus-show-threads
	 (gnus-gather-threads 
	  (gnus-sort-threads 
	   (if (and gnus-summary-expunge-below
		    (not gnus-fetch-old-headers))
	       (gnus-make-threads-and-expunge)
	     (gnus-make-threads))))
       gnus-newsgroup-headers)
     'cull)
    (gnus-summary-update-lines)
    ;; Create the header hashtb.
    (gnus-make-headers-hashtable-by-number)
    ;; Call hooks for modifying summary buffer.
    ;; Suggested by sven@tde.LTH.Se (Sven Mattisson).
    (goto-char (point-min))
    (run-hooks 'gnus-summary-prepare-hook)))

(defun gnus-gather-threads (threads)
  "Gather threads that have lost their roots."
  (if (not gnus-summary-make-false-root)
      threads 
    (let ((hashtb (gnus-make-hashtable 1023))
	  (prev threads)
	  (result threads)
	  subject hthread whole-subject)
      (while threads
	(setq whole-subject 
	      (setq subject (mail-header-subject (car (car threads)))))
	(if gnus-summary-gather-subject-limit
	    (or (and (numberp gnus-summary-gather-subject-limit)
		     (> (length subject) gnus-summary-gather-subject-limit)
		     (setq subject
			   (substring subject 0 
				      gnus-summary-gather-subject-limit)))
		(and (eq 'fuzzy gnus-summary-gather-subject-limit)
		     (setq subject (gnus-simplify-subject-fuzzy subject))))
	  (setq subject (gnus-simplify-subject-re subject)))
	(if (setq hthread 
		  (gnus-gethash subject hashtb))
	    (progn
	      (or (stringp (car (car hthread)))
		  (setcar hthread (list whole-subject (car hthread))))
	      (setcdr (car hthread) (nconc (cdr (car hthread)) 
					   (list (car threads))))
	      (setcdr prev (cdr threads))
	      (setq threads prev))
	  (gnus-sethash subject threads hashtb))
	(setq prev threads)
	(setq threads (cdr threads)))
      result)))

(defun gnus-make-threads ()
  ;; This function takes the dependencies already made by 
  ;; `gnus-get-newsgroup-headers' and builds the trees. First we go
  ;; through the dependencies in the hash table and finds all the
  ;; roots. Roots do not refer back to any valid articles.
  (gnus-message 6 "Threading...")
  (let (roots new-roots)
    (and gnus-fetch-old-headers
	 (eq gnus-headers-retrieved-by 'nov)
	 (gnus-build-old-threads))
    (mapatoms
     (lambda (refs)
       (if (not (car (symbol-value refs)))
	   (setq roots (append (cdr (symbol-value refs)) roots))
	 ;; Ok, these refer back to valid articles, but if
	 ;; `gnus-thread-ignore-subject' is nil, we have to check that
	 ;; the root has the same subject as its children. The children
	 ;; that do not are made into roots and removed from the list
	 ;; of children. 
	 (or gnus-thread-ignore-subject
	     (let* ((prev (symbol-value refs))
		    (subject (gnus-simplify-subject-re 
			      (mail-header-subject (car prev))))
		    (headers (cdr prev)))
	       (while headers
		 (if (not (string= subject
				   (gnus-simplify-subject-re 
				    (mail-header-subject (car headers)))))
		     (progn
		       (setq new-roots (cons (car headers) new-roots))
		       (setcdr prev (cdr headers)))
		   (setq prev headers))
		 (setq headers (cdr headers)))))))
     gnus-newsgroup-dependencies)

    ;; We enter the new roots into the dependencies structure to
    ;; ensure that any possible later thread-regeneration will be
    ;; possible. 
    (let ((r new-roots))
      (while r
	(gnus-sethash (concat (mail-header-id (car r)) ".boo")
		      (list nil (car r)) gnus-newsgroup-dependencies)
	(setq r (cdr r))))

    (setq roots (nconc new-roots roots))

    (prog1
	(mapcar 'gnus-trim-thread
		(apply 'append
		       (mapcar 'gnus-cut-thread
			       (mapcar 'gnus-make-sub-thread roots))))
      (gnus-message 6 "Threading...done"))))

  
(defun gnus-make-threads-and-expunge ()
  ;; This function takes the dependencies already made by 
  ;; `gnus-get-newsgroup-headers' and builds the trees. First we go
  ;; through the dependencies in the hash table and finds all the
  ;; roots. Roots do not refer back to any valid articles.
  (gnus-message 6 "Threading...")
  (let ((default (or gnus-summary-default-score 0))
	(below gnus-summary-expunge-below)
	roots article new-roots)
    (and gnus-fetch-old-headers
	 (eq gnus-headers-retrieved-by 'nov)
	 (gnus-build-old-threads))
    (mapatoms
     (lambda (refs)
       (if (not (car (symbol-value refs)))
	   ;; These articles do not refer back to any other articles -
	   ;; they are roots.
	   (let ((headers (cdr (symbol-value refs))))
	     ;; We weed out the low-scored articles.
	     (while headers
	       (if (not (< (or (cdr (assq (mail-header-number (car headers))
					  gnus-newsgroup-scored)) default)
			   below))
		   ;; It is over.
		   (setq roots (cons (car headers) roots))
		 ;; It is below, so we mark it as read.
		 (setq gnus-newsgroup-unreads
		       (delq (mail-header-number (car headers))
			     gnus-newsgroup-unreads))
		 (setq gnus-newsgroup-reads 
		       (cons (cons (mail-header-number (car headers))
				   gnus-low-score-mark) 
			     gnus-newsgroup-reads)))
	       (setq headers (cdr headers))))
	 ;; Ok, these refer back to valid articles, but if
	 ;; `gnus-thread-ignore-subject' is nil, we have to check that
	 ;; the root has the same subject as its children. The children
	 ;; that do not are made into roots and removed from the list
	 ;; of children. 
	 (or gnus-thread-ignore-subject
	     (let* ((prev (symbol-value refs))
		    (subject (gnus-simplify-subject-re 
			      (mail-header-subject (car prev))))
		    (headers (cdr prev)))
	       (while headers
		 (if (not (string= subject
				   (gnus-simplify-subject-re 
				    (mail-header-subject (car headers)))))
		     (progn
		       (if (not (< (or (cdr (assq (mail-header-number
						   (car headers))
						  gnus-newsgroup-scored))
				       default) below))
			   (setq new-roots (cons (car headers) new-roots))
			 (setq gnus-newsgroup-unreads
			       (delq (mail-header-number (car headers))
				     gnus-newsgroup-unreads))
			 (setq gnus-newsgroup-reads
			       (cons (cons (mail-header-number (car headers)) 
					   gnus-low-score-mark) 
				     gnus-newsgroup-reads)))
		       (setcdr prev (cdr headers)))
		   (setq prev headers))
		 (setq headers (cdr headers)))))
	 ;; If this article is expunged, some of the children might be
	 ;; roots.  
	 (if (< (or (cdr (assq (mail-header-number (car (symbol-value refs)))
			       gnus-newsgroup-scored)) default)
		below)
	     (let* ((prev (symbol-value refs))
		    (headers (cdr prev)))
	       (while headers
		 (setq article (mail-header-number (car headers)))
		 (if (not (< (or (cdr (assq article gnus-newsgroup-scored))
				 default) below))
		     (progn (setq new-roots (cons (car headers) new-roots))
			    (setq prev headers))
		   (setq gnus-newsgroup-unreads 
			 (delq article gnus-newsgroup-unreads))
		   (setq gnus-newsgroup-reads 
			 (cons (cons article gnus-low-score-mark) 
			       gnus-newsgroup-reads))
		   (setcdr prev (cdr headers)))
		 (setq headers (cdr headers))))
	   ;; It was not expunged, but we look at expunged children.
	   (let* ((prev (symbol-value refs))
		  (headers (cdr prev))
		  article)
	     (while headers
	       (setq article (mail-header-number (car headers)))
	       (if (not (< (or (cdr (assq article gnus-newsgroup-scored))
			       default) below))
		   (setq prev headers)
		 (setq gnus-newsgroup-unreads 
		       (delq article gnus-newsgroup-unreads))
		 (setq gnus-newsgroup-reads 
		       (cons (cons article gnus-low-score-mark)
			     gnus-newsgroup-reads))
		 (setcdr prev (cdr headers)))
	       (setq headers (cdr headers)))))))
     gnus-newsgroup-dependencies)

    ;; We enter the new roots into the dependencies structure to
    ;; ensure that any possible later thread-regeneration will be
    ;; possible. 
    (let ((r new-roots))
      (while r
	(gnus-sethash (concat (mail-header-id (car r)) ".boo")
		      (list nil (car r)) gnus-newsgroup-dependencies)
	(setq r (cdr r))))

    (setq roots (nconc new-roots roots))

    (prog1
	(mapcar 'gnus-trim-thread
		(apply 'append
		       (mapcar 'gnus-cut-thread
			       (mapcar 'gnus-make-sub-thread roots))))
      (gnus-message 6 "Threading...done"))))

  
(defun gnus-cut-thread (thread)
  ;; Remove leaf dormant or ancient articles from THREAD.
  (let ((head (car thread))
	(tail (apply 'append (mapcar 'gnus-cut-thread (cdr thread)))))
    (if (and (null tail)
	     (let ((number (mail-header-number head)))
	       (or (memq number gnus-newsgroup-ancient)
		   (memq number gnus-newsgroup-dormant)
		   (and gnus-summary-expunge-below
			(eq gnus-fetch-old-headers 'some)
			(< (or (cdr (assq number gnus-newsgroup-scored))
			       gnus-summary-default-score 0)
			   gnus-summary-expunge-below)
			(progn
			  (setq gnus-newsgroup-unreads
				(delq number gnus-newsgroup-unreads))
			  (setq gnus-newsgroup-reads
				(cons (cons number gnus-low-score-mark)
				      gnus-newsgroup-reads))
			  t)))))
	nil
      (list (cons head tail)))))

(defun gnus-trim-thread (thread)
  ;; Remove root ancient articles with only one child from THREAD.
  (if (and (eq gnus-fetch-old-headers 'some)
	   (memq (mail-header-number (car thread)) gnus-newsgroup-ancient)
	   (= (length thread) 2))
      (gnus-trim-thread (nth 1 thread))
    thread))

(defun gnus-make-sub-thread (root)
  ;; This function makes a sub-tree for a node in the tree.
  (let ((children (reverse (cdr (gnus-gethash (downcase (mail-header-id root))
					      gnus-newsgroup-dependencies)))))
    (cons root (mapcar 'gnus-make-sub-thread children))))

(defun gnus-build-old-threads ()
  ;; Look at all the articles that refer back to old articles, and
  ;; fetch the headers for the articles that aren't there. This will
  ;; build complete threads - if the roots haven't been expired by the
  ;; server, that is.
  (let (id heads)
    (mapatoms
     (lambda (refs)
       (if (not (car (symbol-value refs)))
	   (progn
	     (setq heads (cdr (symbol-value refs)))
	     (while heads
	       (if (not (memq (mail-header-number (car heads))
			      gnus-newsgroup-dormant))
		   (progn
		     (setq id (symbol-name refs))
		     (while (and (setq id (gnus-build-get-header id))
				 (not (car (gnus-gethash 
					    id gnus-newsgroup-dependencies)))))
		     (setq heads nil))
		 (setq heads (cdr heads)))))))
     gnus-newsgroup-dependencies)))

(defun gnus-build-get-header (id)
  ;; Look through the buffer of NOV lines and find the header to
  ;; ID. Enter this line into the dependencies hash table, and return
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
	  (if found
	      (let (ref)
		(beginning-of-line)
		(and
		 (setq header (gnus-nov-parse-line 
			       (read (current-buffer)) deps))
		 (setq ref (mail-header-references header))
		 (string-match "\\(<[^>]+>\\) *$" ref)
		 (substring ref (match-beginning 1) (match-end 1))))))
      (and header
	   (setq gnus-newsgroup-headers (cons header gnus-newsgroup-headers)
		 gnus-newsgroup-ancient (cons (mail-header-number header)
					      gnus-newsgroup-ancient))))))

;; Re-build the thread containing ID.
(defun gnus-rebuild-thread (id)
  (let ((dep gnus-newsgroup-dependencies)
	(buffer-read-only nil)
	parent headers refs thread art)
    (while (and id (setq headers
			 (car (setq art (gnus-gethash (downcase id) dep)))))
      (setq parent art)
      (setq id (and (setq refs (mail-header-references headers))
		    (string-match "\\(<[^>]+>\\) *$" refs)
		    (substring refs (match-beginning 1) (match-end 1)))))
    (setq thread (gnus-make-sub-thread (car parent)))
    (gnus-rebuild-remove-articles thread)
    (let ((beg (point)))
      (gnus-summary-prepare-threads (list thread))
      (gnus-summary-update-lines beg (point)))))

;; Delete all lines in the summary buffer that correspond to articles
;; in this thread.
(defun gnus-rebuild-remove-articles (thread)
  (and (gnus-summary-goto-subject (mail-header-number (car thread)))
       (gnus-delete-line))
  (mapcar (lambda (th) (gnus-rebuild-remove-articles th)) (cdr thread)))

(defun gnus-sort-threads (threads)
  ;; Sort threads as specified in `gnus-thread-sort-functions'.
  (let ((fun gnus-thread-sort-functions))
    (while fun
      (gnus-message 6 "Sorting with %S..." fun)
      (setq threads (sort threads (car fun))
	    fun (cdr fun))))
  (if gnus-thread-sort-functions
      (gnus-message 6 "Sorting...done"))
  threads)

;; Written by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(defmacro gnus-thread-header (thread)
  ;; Return header of first article in THREAD.
  ;; Note that THREAD must never, evr be anything else than a variable -
  ;; using some other form will lead to serious barfage.
  (or (symbolp thread) (signal 'wrong-type-argument '(symbolp thread)))
  ;; (8% speedup to gnus-summary-prepare, just for fun :-)
  (list 'byte-code "\10\211:\203\17\0\211@;\203\16\0A@@\207" ; 
	(vector thread) 2))

(defun gnus-thread-sort-by-number (h1 h2)
  "Sort threads by root article number."
  (< (mail-header-number (gnus-thread-header h1))
     (mail-header-number (gnus-thread-header h2))))

(defun gnus-thread-sort-by-author (h1 h2)
  "Sort threads by root author."
  (string-lessp
   (let ((extract (funcall 
		   gnus-extract-address-components
		   (mail-header-from (gnus-thread-header h1)))))
     (or (car extract) (cdr extract)))
   (let ((extract (funcall
		   gnus-extract-address-components 
		   (mail-header-from (gnus-thread-header h2)))))
     (or (car extract) (cdr extract)))))

(defun gnus-thread-sort-by-subject (h1 h2)
  "Sort threads by root subject."
  (string-lessp
   (downcase (gnus-simplify-subject-re
	      (mail-header-subject (gnus-thread-header h1))))
   (downcase (gnus-simplify-subject-re 
	      (mail-header-subject (gnus-thread-header h2))))))

(defun gnus-thread-sort-by-date (h1 h2)
  "Sort threads by root article date."
  (string-lessp
   (gnus-sortable-date (mail-header-date (gnus-thread-header h1)))
   (gnus-sortable-date (mail-header-date (gnus-thread-header h2)))))

(defun gnus-thread-sort-by-score (h1 h2)
  "Sort threads by root article score.
Unscored articles will be counted as having a score of zero."
  (> (or (cdr (assq (mail-header-number (gnus-thread-header h1))
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)
     (or (cdr (assq (mail-header-number (gnus-thread-header h2))
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)))

(defun gnus-thread-sort-by-total-score (h1 h2)
  "Sort threads by the sum of all scores in the thread.
Unscored articles will be counted as having a score of zero."
  (> (gnus-thread-total-score h1) (gnus-thread-total-score h2)))

(defun gnus-thread-total-score (thread)
  ;;  This function find the total score of THREAD.
  (if (consp thread)
      (if (stringp (car thread))
	  (apply gnus-thread-score-function 0
		 (mapcar 'gnus-thread-total-score-1 (cdr thread)))
	(gnus-thread-total-score-1 thread))
    (gnus-thread-total-score-1 (list thread))))

(defun gnus-thread-total-score-1 (root)
  ;; This function find the total score of the thread below ROOT.
  (setq root (car root))
  (apply gnus-thread-score-function
	 (or (cdr (assq (mail-header-number root) gnus-newsgroup-scored))
	     gnus-summary-default-score 0)
	 (mapcar 'gnus-thread-total-score
		 (cdr (gnus-gethash (downcase (mail-header-id root))
				    gnus-newsgroup-dependencies)))))

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-tmp-prev-subject "")

(defun gnus-summary-prepare-threads (threads &optional cull)
  "Prepare summary buffer from THREADS and indentation LEVEL.  
THREADS is either a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...])'  
or a straight list of headers."
  (gnus-message 5 "Generating summary...")
  (let ((level 0)
	thread header number subject stack state gnus-tmp-gathered)
    (if (vectorp (car threads))
	;; If this is a straight (sic) list of headers, then a
	;; threaded summary display isn't required, so we just create
	;; an unthreaded one.
	(gnus-summary-prepare-unthreaded threads cull)

      ;; Do the threaded display.

      (while (or threads stack)
	
	(if threads
	    ;; If there are some threads, we do them before the
	    ;; threads on the stack.
	    (setq thread threads
		  header (car (car thread)))
	  ;; There were no current threads, so we pop something off
	  ;; the stack. 
	  (setq state (car stack)
		level (car state)
		thread (cdr state)
		stack (cdr stack)
		header (car (car thread))))

	(if (stringp header)
	    (progn
	      ;; The header is a dummy root.
	      (cond 
	       ((eq gnus-summary-make-false-root 'adopt)
		;; We let the first article adopt the rest.
		(let ((th (car (cdr (car thread)))))
		  (while (cdr th)
		    (setq th (cdr th)))
		  (setcdr th (cdr (cdr (car thread))))
		  (setq gnus-tmp-gathered 
			(nconc (mapcar
				(lambda (h) (mail-header-number (car h)))
				(cdr (cdr (car thread))))
			       gnus-tmp-gathered))
		  (setcdr (cdr (car thread)) nil))
		(setq level -1))
	       ((eq gnus-summary-make-false-root 'empty)
		;; We print adopted articles with empty subject fields.
		(setq gnus-tmp-gathered 
		      (nconc (mapcar
			      (lambda (h) (mail-header-number (car h)))
			      (cdr (cdr (car thread))))
			     gnus-tmp-gathered))
		(setq level -1))
	       ((eq gnus-summary-make-false-root 'dummy)
		;; We output a dummy root.
		(gnus-summary-insert-dummy-line 
		 nil header (mail-header-number
			     (car (car (cdr (car thread)))))))
	       (t
		;; We do not make a root for the gathered
		;; sub-threads at all.  
		(setq level -1))))
      
	  (setq number (mail-header-number header)
		subject (mail-header-subject header))

	  ;; Do the async thing.
	  (and gnus-newsgroup-async
	       (setq gnus-newsgroup-threads
		     (cons (cons number (mail-header-lines header)) 
			   gnus-newsgroup-threads)))

	  ;; We may have to root out some bad articles...
	  (and cull
	       (= level 0)
	       (cond ((and (memq (setq number (mail-header-number header))
				 gnus-newsgroup-dormant)
			   (null thread))
		      (setq header nil))
		     ((and gnus-summary-expunge-below
			   (< (or (cdr (assq number gnus-newsgroup-scored))
				  gnus-summary-default-score 0)
			      gnus-summary-expunge-below))
		      (setq header nil)
		      (setq gnus-newsgroup-unreads 
			    (delq number gnus-newsgroup-unreads))
		      (setq gnus-newsgroup-reads
			    (cons (cons number gnus-low-score-mark)
				  gnus-newsgroup-reads)))))
	  
	  (and
	   header
	   (progn
	     (inline
	       (gnus-summary-insert-line
		nil header level nil 
		(cond 
		 ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
		 ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
		 ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
		 ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
		 (t (or (cdr (assq number gnus-newsgroup-reads))
			gnus-ancient-mark)))
		(memq number gnus-newsgroup-replied)
		(memq number gnus-newsgroup-expirable)
		(cond
		 ((and gnus-thread-ignore-subject
		       (not (string= 
			     (gnus-simplify-subject-re gnus-tmp-prev-subject)
			     (gnus-simplify-subject-re subject))))
		  subject)
		 ((zerop level)
		  (if (and (eq gnus-summary-make-false-root 'empty)
			   (memq number gnus-tmp-gathered))
		      gnus-summary-same-subject
		    subject))
		 (t gnus-summary-same-subject))
		(and (eq gnus-summary-make-false-root 'adopt)
		     (memq number gnus-tmp-gathered))
		(cdr (assq number gnus-newsgroup-scored))
		(memq number gnus-newsgroup-processable))

	       (setq gnus-tmp-prev-subject subject)))))

	(if (nth 1 thread) 
	    (setq stack (cons (cons (max 0 level) (nthcdr 1 thread)) stack)))
	(setq level (1+ level))
	(setq threads (cdr (car thread))))))
  (gnus-message 5 "Generating summary...done"))



(defun gnus-summary-prepare-unthreaded (headers &optional cull)
  (let (header number)

    ;; Do the async thing, if that is required.
    (if gnus-newsgroup-async
	(setq gnus-newsgroup-threads
	      (mapcar (lambda (h) 
			(cons (mail-header-number h) (mail-header-lines h)))
		      headers)))

    (while headers
      (setq header (car headers)
	    headers (cdr headers)
	    number (mail-header-number header))

      ;; We may have to root out some bad articles...
      (cond 
       ((and cull
	     (memq (setq number (mail-header-number header))
		   gnus-newsgroup-dormant)))
       ((and cull gnus-summary-expunge-below
	     (< (or (cdr (assq number gnus-newsgroup-scored))
		    gnus-summary-default-score 0)
		gnus-summary-expunge-below))
	(setq gnus-newsgroup-unreads 
	      (delq number gnus-newsgroup-unreads))
	(setq gnus-newsgroup-reads
	      (cons (cons number gnus-low-score-mark)
		    gnus-newsgroup-reads)))
       (t
	(gnus-summary-insert-line
	 nil header 0 nil 
	 (cond ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
	       ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
	       ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
	       ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
	       (t (or (cdr (assq number gnus-newsgroup-reads))
		      gnus-ancient-mark)))
	 (memq number gnus-newsgroup-replied)
	 (memq number gnus-newsgroup-expirable)
	 (mail-header-subject header) nil
	 (cdr (assq number gnus-newsgroup-scored))
	 (memq number gnus-newsgroup-processable)))))))

(defun gnus-select-newsgroup (group &optional read-all)
  "Select newsgroup GROUP.
If READ-ALL is non-nil, all articles in the group are selected."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 articles)

    (or (gnus-check-server
	 (setq gnus-current-select-method (gnus-find-method-for-group group)))
	(error "Couldn't open server"))
    
    (or (and entry (not (eq (car entry) t))) ; Either it's active...
	(gnus-activate-group group) ; Or we can activate it...
	(progn ; Or we bug out.
	  (kill-buffer (current-buffer))
	  (error "Couldn't request group %s: %s" 
		 group (gnus-status-message group))))

    (setq gnus-newsgroup-name group)
    (setq gnus-newsgroup-unselected nil)
    (setq gnus-newsgroup-unreads (gnus-list-of-unread-articles group))

    (and gnus-asynchronous
	 (gnus-check-backend-function 
	  'request-asynchronous gnus-newsgroup-name)
	 (setq gnus-newsgroup-async
	       (gnus-request-asynchronous gnus-newsgroup-name)))

    (setq articles (gnus-articles-to-read group read-all))

    (cond 
     ((null articles) 
      (gnus-message 3 "Couldn't select newsgroup")
      'quit)
     ((eq articles 0) nil)
     (t
      ;; Init the dependencies hash table.
      (setq gnus-newsgroup-dependencies 
	    (gnus-make-hashtable (length articles)))
      ;; Retrieve the headers and read them in.
      (gnus-message 5 "Fetching headers...")
      (setq gnus-newsgroup-headers 
	    (if (eq 'nov (setq gnus-headers-retrieved-by
			       ;; This is a naughty hack. To get the
			       ;; retrieval of old headers to work, we
			       ;; set `nntp-nov-gap' to nil (locally),
			       ;; and then just retrieve the headers.
			       ;; Mucho magic.
			       (if gnus-fetch-old-headers
				   (let (nntp-nov-gap)
				     (gnus-retrieve-headers 
				      (if (not (eq 1 (car articles)))
					  (cons 1 articles)
					articles)
				      gnus-newsgroup-name))
				 (gnus-retrieve-headers 
				  articles gnus-newsgroup-name))))
		(progn
		  (gnus-get-newsgroup-headers-xover articles))
	      ;; If we were to fetch old headers, but the backend didn't
	      ;; support XOVER, then it is possible we fetched one article
	      ;; that we shouldn't have. If that's the case, we remove it.
	      (if (or (not gnus-fetch-old-headers)
		      (eq 1 (car articles)))
		  ()
		(save-excursion
		  (set-buffer nntp-server-buffer)
		  (goto-char (point-min))
		  (and 
		   (looking-at "[0-9]+[ \t]+1[ \t]") ; This is not a NOV line.
		   (delete-region	; So we delete this head.
		    (point) 
		    (search-forward "\n.\n" nil t)))))
	      (gnus-get-newsgroup-headers)))
      (gnus-message 5 "Fetching headers...done")      
      ;; Remove canceled articles from the list of unread articles.
      (setq gnus-newsgroup-unreads
	    (gnus-set-sorted-intersection 
	     gnus-newsgroup-unreads
	     (mapcar (lambda (headers) (mail-header-number headers))
		     gnus-newsgroup-headers)))
      ;; Adjust and set lists of article marks.
      (and info
	   (let (marked)
	     (gnus-adjust-marked-articles info)
	     (setq gnus-newsgroup-marked 
		   (copy-sequence
		    (cdr (assq 'tick (setq marked (nth 3 info))))))
	     (setq gnus-newsgroup-replied 
		   (copy-sequence (cdr (assq 'reply marked))))
	     (setq gnus-newsgroup-expirable
		   (copy-sequence (cdr (assq 'expire marked))))
	     (setq gnus-newsgroup-killed
		   (copy-sequence (cdr (assq 'killed marked))))
	     (setq gnus-newsgroup-bookmarks 
		   (copy-sequence (cdr (assq 'bookmark marked))))
	     (setq gnus-newsgroup-dormant 
		   (copy-sequence (cdr (assq 'dormant marked))))
	     (setq gnus-newsgroup-scored 
		   (copy-sequence (cdr (assq 'score marked))))
	     (setq gnus-newsgroup-processable nil)))
      ;; Check whether auto-expire is to be done in this group.
      (setq gnus-newsgroup-auto-expire
	    (or (and (stringp gnus-auto-expirable-newsgroups)
		     (string-match gnus-auto-expirable-newsgroups group))
		(memq 'auto-expire (nth 5 info))))
      ;; First and last article in this newsgroup.
      (and gnus-newsgroup-headers
	   (setq gnus-newsgroup-begin 
		 (mail-header-number (car gnus-newsgroup-headers)))
	   (setq gnus-newsgroup-end
		 (mail-header-number
		  (gnus-last-element gnus-newsgroup-headers))))
      (setq gnus-reffed-article-number -1)
      ;; GROUP is successfully selected.
      (or gnus-newsgroup-headers t)))))

(defun gnus-articles-to-read (group read-all)
  ;; Find out what articles the user wants to read.
  (let* ((articles
	  ;; Select all articles if `read-all' is non-nil, or if all the
	  ;; unread articles are dormant articles.
	  (if (or read-all
		  (= (length gnus-newsgroup-unreads) 
		     (length gnus-newsgroup-dormant)))
	      (gnus-uncompress-range 
	       (gnus-gethash group gnus-active-hashtb))
	    gnus-newsgroup-unreads))
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
		(cond ((and (or (<= scored marked)
				(= scored number))
			    (numberp gnus-large-newsgroup)
			    (> number gnus-large-newsgroup))
		       (let ((input
			      (read-string
			       (format
				"How many articles from %s (default %d): "
				gnus-newsgroup-name number))))
			 (if (string-match "^[ \t]*$" input)
			     number input)))
		      ((and (> scored marked) (< scored number))
		       (let ((input
			      (read-string
			       (format 
				"%s %s (%d scored, %d total): "
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

(defun gnus-adjust-marked-articles (info &optional active)
  "Remove all marked articles that are no longer legal."
  (let ((marked-lists (nth 3 info))
	(active (or active (gnus-gethash (car info) gnus-active-hashtb)))
	m prev)
    ;; There are many types of marked articles.
    (while marked-lists
      (setq m (cdr (setq prev (car marked-lists))))
      (cond ((or (eq 'tick (car prev)) (eq 'dormant (car prev)))
	     ;; Make sure that all ticked articles are a subset of the
	     ;; unread/unselected articles.
	     ;;(while m
	     ;;  (if (or (memq (car m) gnus-newsgroup-unreads)
	     ;;  (memq (car m) gnus-newsgroup-unselected))
	     ;; (setq prev m)
	     ;; (setcdr prev (cdr m)))
	     ;; (setq m (cdr m)))
	     )
	    ((eq 'score (car prev))
	     ;; Scored articles should be a subset of
	     ;; unread/unselected articles. 
	     (while m
	       (if (or (memq (car (car m)) gnus-newsgroup-unreads)
		       (memq (car (car m)) gnus-newsgroup-unreads))
		   (setq prev m)
		 (setcdr prev (cdr m)))
	       (setq m (cdr m))))
	    ((eq 'bookmark (car prev))
	     ;; Bookmarks should be a subset of active articles.
	     (while m
	       (if (< (car (car m)) (car active))
		   (setcdr prev (cdr m))
		 (setq prev m))
	       (setq m (cdr m))))
	    ((eq 'killed (car prev))
	     ;; Articles that have been through the kill process are
	     ;; to be a subset of active articles.
	     (while (and m (< (or (and (numberp (car m)) (car m))
				  (cdr (car m)))
			      (car active)))
	       (setcdr prev (cdr m))
	       (setq m (cdr m)))
	     (if (and m (< (or (and (numberp (car m)) (car m))
			       (car (car m)))
			   (car active))) 
		 (setcar (if (numberp (car m)) m (car m)) (car active))))
	    ((or (eq 'reply (car prev)) (eq 'expire (car prev)))
	     ;; The replied and expirable articles have to be articles
	     ;; that are active. 
	     (while m
	       (if (< (car m) (car active))
		   (setcdr prev (cdr m))
		 (setq prev m))
	       (setq m (cdr m)))))
      (setq marked-lists (cdr marked-lists)))
    ;; Remove all lists that are empty.
    (setq marked-lists (nth 3 info))
    (if marked-lists
	(progn
	  (while (= 1 (length (car marked-lists)))
	    (setq marked-lists (cdr marked-lists)))
	  (setq m (cdr (setq prev marked-lists)))
	  (while m
	    (if (= 1 (length (car m)))
		(setcdr prev (cdr m))
	      (setq prev m))
	    (setq m (cdr m)))
	  (setcar (nthcdr 3 info) marked-lists)))
    ;; Finally, if there are no marked lists at all left, and if there
    ;; are no elements after the lists in the info list, we just chop
    ;; the info list off before the marked lists.
    (and (null marked-lists) 
	 (not (nthcdr 4 info))
	 (setcdr (nthcdr 2 info) nil)))
  info)

(defun gnus-set-marked-articles 
  (info ticked replied expirable killed dormant bookmark score) 
  "Enter the various lists of marked articles into the newsgroup info list."
  (let (newmarked)
    (and ticked (setq newmarked (cons (cons 'tick ticked) nil)))
    (and replied (setq newmarked (cons (cons 'reply replied) newmarked)))
    (and expirable (setq newmarked (cons (cons 'expire expirable) 
					 newmarked)))
    (and killed (setq newmarked (cons (cons 'killed killed) newmarked)))
    (and dormant (setq newmarked (cons (cons 'dormant dormant) newmarked)))
    (and bookmark (setq newmarked (cons (cons 'bookmark bookmark) 
					newmarked)))
    (and score (setq newmarked (cons (cons 'score score) newmarked)))
    (if (nthcdr 3 info)
	(progn
	  (setcar (nthcdr 3 info) newmarked)
	  (and (not newmarked)
	       (not (nthcdr 4 info))
	       (setcdr (nthcdr 2 info) nil)))
      (if newmarked
	  (setcdr (nthcdr 2 info) (list newmarked))))))

(defun gnus-add-marked-articles (group type articles &optional info force)
  ;; Add ARTICLES of TYPE to the info of GROUP.
  ;; If INFO is non-nil, use that info. If FORCE is non-nil, don't
  ;; add, but replace marked articles of TYPE with ARTICLES.
  (let ((info (or info (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
	marked m)
    (or (not info)
	(and (not (setq marked (nthcdr 3 info)))
	     (setcdr (nthcdr 2 info) (list (list (cons type articles)))))
	(and (not (setq m (assq type (car marked))))
	     (setcar marked (cons (cons type articles) (car marked))))
	(if force
	    (setcdr m articles)
	  (nconc m articles)))))
	 
(defun gnus-set-mode-line (where)
  "This function sets the mode line of the article or summary buffers.
If WHERE is `summary', the summary mode line format will be used."
  (if (memq where gnus-updated-mode-lines)
      (let (mode-string)
	(save-excursion
	  (set-buffer gnus-summary-buffer)
	  (let* ((mformat (if (eq where 'article) 
			      gnus-article-mode-line-format-spec
			    gnus-summary-mode-line-format-spec))
		 (buffer-name (if (eq where 'article)
				  (buffer-name
				   (get-buffer gnus-article-buffer))
				(buffer-name)))
		 (group-name gnus-newsgroup-name)
		 (article-number (or gnus-current-article 0))
		 (unread (- (length gnus-newsgroup-unreads)
			    (length gnus-newsgroup-dormant)))
		 (unread-and-unticked 
		  (- unread (length gnus-newsgroup-marked)))
		 (unselected (length gnus-newsgroup-unselected))
		 (unread-and-unselected
		  (cond ((and (zerop unread-and-unticked)
			      (zerop unselected)) "")
			((zerop unselected) 
			 (format "{%d more}" unread-and-unticked))
			(t (format "{%d(+%d) more}"
				   unread-and-unticked unselected))))
		 (subject
		  (if gnus-current-headers
		      (mail-header-subject gnus-current-headers) ""))
		 (max-len (and gnus-mode-non-string-length
			       (- (frame-width) gnus-mode-non-string-length)))
		 header);; passed as argument to any user-format-funcs
	    (setq mode-string (eval mformat))
            (or (numberp max-len)
		(setq max-len (length mode-string)))
	    (if (< max-len 4) (setq max-len 4))
	    (if (> (length mode-string) max-len)
		;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
		;;  function `substring' might cut on a middle
		;;  of multi-octet character.
		(setq mode-string 
		      (concat (gnus-truncate-string mode-string (- max-len 3))
			      "...")))
	    (setq mode-string (format (format "%%-%ds" max-len)
				      mode-string))))
	(setq mode-line-buffer-identification mode-string)
	(set-buffer-modified-p t))))

(defun gnus-create-xref-hashtb (from-newsgroup headers unreads)
  "Go through the HEADERS list and add all Xrefs to a hash table.
The resulting hash table is returned, or nil if no Xrefs were found."
  (let* ((from-method (gnus-find-method-for-group from-newsgroup))
	 (prefix (if (and 
		      (gnus-group-foreign-p from-newsgroup)
		      (not (memq 'virtual 
				 (assoc (symbol-name (car from-method))
					gnus-valid-select-methods))))
		     (gnus-group-real-prefix from-newsgroup)))
	 (xref-hashtb (make-vector 63 0))
	 start group entry number xrefs header)
    (while headers
      (setq header (car headers))
      (if (and (setq xrefs (mail-header-xref header))
	       (not (memq (mail-header-number header) unreads)))
	  (progn
	    (setq start 0)
	    (while (string-match "\\([^ ]+\\):\\([0-9]+\\)" xrefs start)
	      (setq start (match-end 0))
	      (setq group (concat prefix (substring xrefs (match-beginning 1) 
						    (match-end 1))))
	      (setq number 
		    (string-to-int (substring xrefs (match-beginning 2) 
					      (match-end 2))))
	      (if (setq entry (gnus-gethash group xref-hashtb))
		  (setcdr entry (cons number (cdr entry)))
		(gnus-sethash group (cons number nil) xref-hashtb)))))
      (setq headers (cdr headers)))
    (if start xref-hashtb nil)))

(defun gnus-mark-xrefs-as-read (from-newsgroup headers unreads expirable)
  "Look through all the headers and mark the Xrefs as read."
  (let ((virtual (memq 'virtual 
		       (assoc (symbol-name (car (gnus-find-method-for-group 
						 from-newsgroup)))
			      gnus-valid-select-methods)))
	name entry info xref-hashtb idlist method
	nth4)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (if (setq xref-hashtb 
		(gnus-create-xref-hashtb from-newsgroup headers unreads))
	  (mapatoms 
	   (lambda (group)
	     (if (string= from-newsgroup (setq name (symbol-name group)))
		 ()
	       (setq idlist (symbol-value group))
	       ;; Dead groups are not updated.
	       (if (and (prog1 
			    (setq entry (gnus-gethash name gnus-newsrc-hashtb)
				  info (nth 2 entry))
			  (if (stringp (setq nth4 (nth 4 info)))
			      (setq nth4 (gnus-server-to-method nth4))))
			;; Only do the xrefs if the group has the same
			;; select method as the group we have just read.
			(or (gnus-methods-equal-p 
			     nth4 (gnus-find-method-for-group from-newsgroup))
			    virtual
			    (equal nth4 
				   (setq method (gnus-find-method-for-group 
						 from-newsgroup)))
			    (and (equal (car nth4) (car method))
				 (equal (nth 1 nth4) (nth 1 method))))
			gnus-use-cross-reference
			(or (not (eq gnus-use-cross-reference t))
			    virtual
			    ;; Only do cross-references on subscribed
			    ;; groups, if that is what is wanted.  
			    (<= (nth 1 info) gnus-level-subscribed)))
		   (gnus-group-make-articles-read name idlist expirable))))
	   xref-hashtb)))))

(defun gnus-group-make-articles-read (group articles expirable)
  (let* ((num 0)
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (active (gnus-gethash group gnus-active-hashtb))
	 exps expirable range)
    ;; First peel off all illegal article numbers.
    (if active
	(let ((ids articles)
	      (ticked (cdr (assq 'tick (nth 3 info))))
	      (dormant (cdr (assq 'dormant (nth 3 info))))
	      id first)
	  (setq exps nil)
	  (while ids
	    (setq id (car ids))
	    (if (and first (> id (cdr active)))
		(progn
		  ;; We'll end up in this situation in one particular
		  ;; obscure situation. If you re-scan a group and get
		  ;; a new article that is cross-posted to a different
		  ;; group that has not been re-scanned, you might get
		  ;; crossposted article that has a higher number than
		  ;; Gnus believes possible. So we re-activate this
		  ;; group as well. This might mean doing the
		  ;; crossposting thingy will *increase* the number
		  ;; of articles in some groups. Tsk, tsk.
		  (setq active (or (gnus-activate-group group) active))))
	    (if (or (> id (cdr active))
		    (< id (car active))
		    (memq id ticked)
		    (memq id dormant))
		(setq articles (delq id articles)))
	    (and (memq id expirable)
		 (setq exps (cons id exps)))
	    (setq ids (cdr ids)))))
    ;; Update expirable articles.
    (gnus-add-marked-articles nil 'expirable exps info)
    (and active
	 (null (nth 2 info))
	 (> (car active) 1)
	 (setcar (nthcdr 2 info) (cons 1 (1- (car active)))))
    (setcar (nthcdr 2 info)
	    (setq range
		  (gnus-add-to-range 
		   (nth 2 info) 
		   (setq articles (sort articles '<)))))
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
		(setq num (+ num (- (1+ (cdr (car range)))
				    (car (car range))))))
	      (setq range (cdr range)))
	    (setq num (- (cdr active) num))))
	  ;; Update the number of unread articles.
	  (setcar 
	   entry 
	   (max 0 (- num 
		     (length (cdr (assq 'tick (nth 3 info))))
		     (length 
		      (cdr (assq 'dormant (nth 3 info)))))))
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

(defun gnus-get-newsgroup-headers ()
  (setq gnus-article-internal-prepare-hook nil)
  (let ((cur nntp-server-buffer)
	(dependencies gnus-newsgroup-dependencies)
	headers id dep end ref)
    (save-excursion
      (set-buffer nntp-server-buffer)
      ;; Allow the user to mangle the headers before parsing them.
      (run-hooks 'gnus-parse-headers-hook)
      (goto-char (point-min))
      ;; Search to the beginning of the next header. Error messages
      ;; do not begin with 2 or 3.
      (while (re-search-forward "^[23][0-9]+ " nil t)
	(let ((header (make-vector 9 nil))
	      (case-fold-search t)
	      (p (point))
	      in-reply-to)
	  (setq id nil
		ref nil)
	  (mail-header-set-number header (read cur))
	  ;; This implementation of this function, with nine
	  ;; search-forwards instead of the one re-search-forward and
	  ;; a case (which basically was the old function) is actually
	  ;; about twice as fast, even though it looks messier. You
	  ;; can't have everything, I guess. Speed and elegance
	  ;; doesn't always come hand in hand.
	  (save-restriction
	    (narrow-to-region (point) (or (save-excursion 
					    (search-forward "\n.\n" nil t))
					  (point)))
	    (if (search-forward "\nfrom: " nil t)
		(mail-header-set-from header (gnus-header-value))
	      (mail-header-set-from header "(nobody)"))
	    (goto-char p)
	    (if (search-forward "\nsubject: " nil t)
		(mail-header-set-subject header (gnus-header-value))
	      (mail-header-set-subject header "(none)"))
	    (goto-char p)
	    (and (search-forward "\nxref: " nil t)
		 (mail-header-set-xref header (gnus-header-value)))
	    (goto-char p)
	    (or (numberp (and (search-forward "\nlines: " nil t)
			      (mail-header-set-lines header (read cur))))
		(mail-header-set-lines header 0))
	    (goto-char p)
	    (and (search-forward "\ndate: " nil t)
		 (mail-header-set-date header (gnus-header-value)))
	    (goto-char p)
	    (if (search-forward "\nmessage-id: " nil t)
		(mail-header-set-id header (setq id (gnus-header-value)))
	      ;; If there was no message-id, we just fake one to make
	      ;; subsequent routines simpler.
	      (mail-header-set-id 
	       header 
	       (setq id (concat "none+" 
				(int-to-string 
				 (setq gnus-newsgroup-none-id 
				       (1+ gnus-newsgroup-none-id)))))))
	    (goto-char p)
	    (if (search-forward "\nreferences: " nil t)
		(progn
		  (mail-header-set-references header (gnus-header-value))
		  (setq end (match-end 0))
		  (save-excursion
		    (setq ref 
			  (downcase
			   (buffer-substring
			    (progn 
			      (end-of-line)
			      (search-backward ">" end t)
			      (1+ (point)))
			    (progn
			      (search-backward "<" end t)
			      (point)))))))
	      ;; Get the references from the in-reply-to header if there
	      ;; ware no references and the in-reply-to header looks
	      ;; promising. 
	      (if (and (search-forward "\nin-reply-to: " nil t)
		       (setq in-reply-to (gnus-header-value))
		       (string-match "<[^>]+>" in-reply-to))
		  (progn
		    (mail-header-set-references 
		     header 
		     (setq ref (substring in-reply-to (match-beginning 0)
					  (match-end 0))))
		    (setq ref (downcase ref)))
		(setq ref "none")))
	    ;; We do some threading while we read the headers. The
	    ;; message-id and the last reference are both entered into
	    ;; the same hash table. Some tippy-toeing around has to be
	    ;; done in case an article has arrived before the article
	    ;; which it refers to.
	    (if (boundp (setq dep (intern (downcase id) dependencies)))
		(if (car (symbol-value dep))
		    ;; An article with this Message-ID has already
		    ;; been seen, so we ignore this one, except we add
		    ;; any additional Xrefs (in case the two articles
		    ;; came from different servers.
		    (progn
		      (mail-header-set-xref 
		       (car (symbol-value dep))
		       (concat (or (mail-header-xref 
				    (car (symbol-value dep))) "")
			       (or (mail-header-xref header) "")))
		      (setq header nil))
		  (setcar (symbol-value dep) header))
	      (set dep (list header)))
	    (if header
		(progn
		  (if (boundp (setq dep (intern ref dependencies)))
		      (setcdr (symbol-value dep) 
			      (cons header (cdr (symbol-value dep))))
		    (set dep (list nil header)))
		  (setq headers (cons header headers))))
	    (goto-char (point-max))))))
    (nreverse headers)))

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
(defun gnus-get-newsgroup-headers-xover (sequence)
  "Parse the news overview data in the server buffer, and return a
list of headers that match SEQUENCE (see `nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((cur nntp-server-buffer)
	(dependencies gnus-newsgroup-dependencies)
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
			 (inline (gnus-nov-parse-line number dependencies)))
		   (setq headers (cons header headers)))))
	(forward-line 1))
      (setq headers (nreverse headers)))
    headers))

;; This function has to be called with point after the article number
;; on the beginning of the line.
(defun gnus-nov-parse-line (number dependencies)
  (let ((none 0)
	(eol (gnus-point-at-eol)) 
	(buffer (current-buffer))
	header ref id dep)

    ;; overview: [num subject from date id refs chars lines misc]
    (narrow-to-region (point) eol)
    (or (eobp) (forward-char))

    (condition-case nil
	(setq header
	      (vector 
	       number			; number
	       (gnus-nov-field)      	; subject
	       (gnus-nov-field)      	; from
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
			       (downcase 
				(buffer-substring 
				 (1+ (point))
				 (progn
				   (search-backward "<" beg t)
				   (point)))))
		       (setq ref nil))))
		 (gnus-nov-field))	; refs
	       (gnus-nov-read-integer)	; chars
	       (gnus-nov-read-integer)	; lines
	       (if (= (following-char) ?\n)
		   nil
		 (gnus-nov-field))	; misc
	       ))
      (error (progn 
	       (ding)
	       (message "Strange nov line.")
	       (setq header nil)
	       (goto-char eol))))

    (widen)

    ;; We build the thread tree.
    (and header
	 (if (boundp (setq dep (intern (downcase id) dependencies)))
	     (if (car (symbol-value dep))
		 ;; An article with this Message-ID has already been seen,
		 ;; so we ignore this one, except we add any additional
		 ;; Xrefs (in case the two articles came from different
		 ;; servers.
		 (progn
		   (mail-header-set-xref 
		    (car (symbol-value dep))
		    (concat (or (mail-header-xref (car (symbol-value dep))) "")
			    (or (mail-header-xref header) "")))
		   (setq header nil))
	       (setcar (symbol-value dep) header))
	   (set dep (list header))))
    (if header
	(progn
	  (if (boundp (setq dep (intern (or ref "none") 
					dependencies)))
	      (setcdr (symbol-value dep) 
		      (cons header (cdr (symbol-value dep))))
	    (set dep (list nil header)))))
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
	    (gnus-narrow-to-headers)
	    (goto-char (point-min))
	    (if (or (and (eq (downcase (following-char)) ?x)
			 (looking-at "Xref:"))
		    (search-forward "\nXref:" nil t))
		(progn
		  (goto-char (1+ (match-end 0)))
		  (setq xref (buffer-substring (point) 
					       (progn (end-of-line) (point))))
		  (mail-header-set-xref headers xref))))))))

(defalias 'gnus-find-header-by-number 'gnus-get-header-by-number)
(make-obsolete 'gnus-find-header-by-number 'gnus-get-header-by-number)

(defun gnus-make-headers-hashtable-by-number ()
  "Make hashtable for the variable gnus-newsgroup-headers by number."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let ((headers gnus-newsgroup-headers)
	  header)
      (setq gnus-newsgroup-headers-hashtb-by-number
	    (gnus-make-hashtable (length headers)))
      (while headers
	(setq header (car headers))
	(gnus-sethash (int-to-string (mail-header-number header))
		      header gnus-newsgroup-headers-hashtb-by-number)
	(setq headers (cdr headers))))))

(defun gnus-more-header-backward ()
  "Find new header backward."
  (let ((first (car (gnus-gethash gnus-newsgroup-name gnus-active-hashtb)))
	(artnum gnus-newsgroup-begin)
	(header nil))
    (while (and (not header)
		(> artnum first))
      (setq artnum (1- artnum))
      (setq header (gnus-read-header artnum)))
    header))

(defun gnus-more-header-forward (&optional backward)
  "Find new header forward.
If BACKWARD, find new header backward instead."
  (if backward
      (gnus-more-header-backward)
    (let ((last (cdr (gnus-gethash gnus-newsgroup-name gnus-active-hashtb)))
	  (artnum gnus-newsgroup-end)
	  (header nil))
      (while (and (not header)
		  (< artnum last))
	(setq artnum (1+ artnum))
	(setq header (gnus-read-header artnum)))
      header)))

(defun gnus-extend-newsgroup (header &optional backward)
  "Extend newsgroup selection with HEADER.
Optional argument BACKWARD means extend toward backward."
  (if header
      (let ((artnum (mail-header-number header)))
	(setq gnus-newsgroup-headers
	      (if backward
		  (cons header gnus-newsgroup-headers)
		(nconc gnus-newsgroup-headers (list header))))
	(setq gnus-newsgroup-unselected
	      (delq artnum gnus-newsgroup-unselected))
	(setq gnus-newsgroup-begin (min gnus-newsgroup-begin artnum))
	(setq gnus-newsgroup-end (max gnus-newsgroup-end artnum)))))

(defun gnus-summary-work-articles (n)
  "Return a list of articles to be worked upon. The prefix argument,
the list of process marked articles, and the current article will be
taken into consideration."
  (let (articles)
    (if (and n (numberp n))
	(let ((backward (< n 0))
	      (n (abs n)))
	  (save-excursion
	    (while (and (> n 0)
			(setq articles (cons (gnus-summary-article-number) 
					     articles))
			(gnus-summary-search-forward nil nil backward))
	      (setq n (1- n))))
	  (sort articles (function <)))
      (or (reverse gnus-newsgroup-processable)
	  (list (gnus-summary-article-number))))))

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

(defun gnus-subject-equal (s1 s2)
  (cond
   ((null gnus-summary-gather-subject-limit)
    (equal (gnus-simplify-subject-re s1)
	   (gnus-simplify-subject-re s2)))
   ((eq gnus-summary-gather-subject-limit 'fuzzy)
    (equal (gnus-simplify-subject-fuzzy s1)
	   (gnus-simplify-subject-fuzzy s2)))
   ((numberp gnus-summary-gather-subject-limit)
    (equal (gnus-limit-string s1 gnus-summary-gather-subject-limit)
	   (gnus-limit-string s2 gnus-summary-gather-subject-limit)))
   (t
    (equal s1 s2))))
    
(defun gnus-summary-search-subject (&optional backward unread subject)
  "Search for article forward.
If BACKWARD is non-nil, search backward.
If UNREAD is non-nil, only unread articles are selected.
If SUBJECT is non-nil, the article which has the same subject will be
searched for." 
  (let ((func (if backward 'previous-single-property-change
		'next-single-property-change))
	(beg (point))
	(did t)
	pos psubject)
    (beginning-of-line)
    (and gnus-summary-check-current unread
	 (eq (get-text-property (point) 'gnus-mark) gnus-unread-mark)
	 (setq did nil))
    (if (not did)
	()
      (forward-char (if backward (if (bobp) 0 -1) (if (eobp) 0 1)))
      (while
	  (and 
	   (setq pos (funcall func (point) 'gnus-number))
	   (goto-char (if backward (1- pos) pos))
	   (setq did
		 (not (and
		       (or (not unread)
			   (eq (get-text-property (point) 'gnus-mark)
			       gnus-unread-mark))
		       (or (not subject)
			   (and (setq psubject 
				      (inline (gnus-summary-subject-string)))
				(inline 
				  (gnus-subject-equal subject psubject)))))))
	   (if backward (if (bobp) nil (forward-char -1) t)
	     (if (eobp) nil (forward-char 1) t)))))
    (if did
	(progn (goto-char beg) nil)
      (prog1
	  (get-text-property (point) 'gnus-number)
	(gnus-summary-show-thread)
	(gnus-summary-position-cursor)))))

(defun gnus-summary-pseudo-article ()
  "The thread level of the article on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-pseudo))

(defalias 'gnus-summary-score 'gnus-summary-article-score)
(make-obsolete 'gnus-summary-score 'gnus-summary-article-score)
(defun gnus-summary-article-score ()
  "Return current article score."
  (or (cdr (assq (gnus-summary-article-number) gnus-newsgroup-scored))
      gnus-summary-default-score 0))

(defun gnus-summary-recenter ()
  "Center point in the summary window.
If `gnus-auto-center-summary' is nil, or the article buffer isn't
displayed, no centering will be performed." 
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested. Suggested by popovich@park.cs.columbia.edu.
  (let* ((top (cond ((< (window-height) 4) 0)
		    ((< (window-height) 7) 1)
		    (t 2)))
	 (height (1- (window-height)))
	 (bottom (save-excursion (goto-char (point-max))
				 (forward-line (- height))
				 (point)))
	 (window (get-buffer-window (current-buffer))))
    (and 
     ;; The user has to want it,
     gnus-auto-center-summary 
     ;; the article buffer must be displayed,
     (get-buffer-window gnus-article-buffer)
     ;; Set the window start to either `bottom', which is the biggest
     ;; possible valid number, or the second line from the top,
     ;; whichever is the least.
     (set-window-start
      window (min bottom (save-excursion (forward-line (- top)) (point)))))))

;; Function written by Stainless Steel Rat <ratinox@ccs.neu.edu>.
(defun gnus-short-group-name (group &optional levels)
  "Collapse GROUP name LEVELS."
  (let* ((name "") (foreign "") (depth -1) (skip 1)
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
      (if (and (string-match "\\." group) (> levels 0))
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
  (let* ((read (nth 2 (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
	 (active (gnus-gethash group gnus-active-hashtb))
	 (last (cdr active))
	 first nlast unread)
    ;; If none are read, then all are unread. 
    (if (not read)
	(setq first (car active))
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article. Sounds logical, doesn't it?
      (if (not (listp (cdr read)))
	  (setq first (1+ (cdr read)))
	;; `read' is a list of ranges.
	(if (/= (setq nlast (or (and (numberp (car read)) (car read)) 
				(car (car read)))) 1)
	    (setq first 1))
	(while read
	  (if first 
	      (while (< first nlast)
		(setq unread (cons first unread))
		(setq first (1+ first))))
	  (setq first (1+ (if (atom (car read)) (car read) (cdr (car read)))))
	  (setq nlast (if (atom (car (cdr read))) 
			  (car (cdr read))
			(car (car (cdr read)))))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (while (<= first last)
      (setq unread (cons first unread))
      (setq first (1+ first)))
    ;; Return the list of unread articles.
    (nreverse unread)))

(defun gnus-list-of-read-articles (group)
  (let ((info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	(active (gnus-gethash group gnus-active-hashtb)))
    (and info active
	 (gnus-sorted-complement 
	  (gnus-uncompress-range active) 
	  (gnus-list-of-unread-articles group)))))

;; Various summary commands

(defun gnus-summary-universal-argument ()
  "Perform any operation on all articles marked with the process mark."
  (interactive)
  (gnus-set-global-variables)
  (let ((articles (reverse gnus-newsgroup-processable))
	func)
    (or articles (error "No articles marked"))
    (or (setq func (key-binding (read-key-sequence "C-c C-u")))
	(error "Undefined key"))
    (while articles
      (gnus-summary-goto-subject (car articles))
      (command-execute func)
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))))

(defun gnus-summary-toggle-truncation (&optional arg)
  "Toggle truncation of summary lines.
With arg, turn line truncation on iff arg is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-summary-reselect-current-group (&optional all)
  "Once exit and then reselect the current newsgroup.
The prefix argument ALL means to select all articles."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((current-subject (gnus-summary-article-number))
	(group gnus-newsgroup-name))
    (setq gnus-newsgroup-begin nil)
    (gnus-summary-exit t)
    ;; We have to adjust the point of group mode buffer because the
    ;; current point was moved to the next unread newsgroup by
    ;; exiting.
    (gnus-summary-jump-to-group group)
    (gnus-group-read-group all t)
    (gnus-summary-goto-subject current-subject)))

(defun gnus-summary-rescan-group (&optional all)
  "Exit the newsgroup, ask for new articles, and select the newsgroup."
  (interactive "P")
  (gnus-set-global-variables)
  ;; Fix by Ilja Weis <kult@uni-paderborn.de>.
  (let ((group gnus-newsgroup-name))
    (gnus-summary-exit)
    (gnus-summary-jump-to-group group)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-group-get-new-news-this-group 1))
    (gnus-summary-jump-to-group group)
    (gnus-group-read-group all)))

(defun gnus-summary-update-info ()
  (let* ((group gnus-newsgroup-name))
    (if gnus-newsgroup-kill-headers
	(setq gnus-newsgroup-killed
	      (gnus-compress-sequence
	       (nconc
		(gnus-set-sorted-intersection
		 (gnus-uncompress-range gnus-newsgroup-killed)
		 (setq gnus-newsgroup-unselected
		       (sort gnus-newsgroup-unselected '<)))
		(setq gnus-newsgroup-unreads
		      (sort gnus-newsgroup-unreads '<))) t)))
    (or (listp (cdr gnus-newsgroup-killed))
	(setq gnus-newsgroup-killed (list gnus-newsgroup-killed)))
    (let ((headers gnus-newsgroup-headers))
      (gnus-close-group group)
      (run-hooks 'gnus-exit-group-hook)
      (gnus-update-read-articles 
       group gnus-newsgroup-unreads gnus-newsgroup-unselected 
       gnus-newsgroup-marked
       t gnus-newsgroup-replied gnus-newsgroup-expirable
       gnus-newsgroup-killed gnus-newsgroup-dormant
       gnus-newsgroup-bookmarks 
       (and gnus-save-score gnus-newsgroup-scored))
      (and gnus-use-cross-reference
	   (gnus-mark-xrefs-as-read 
	    group headers gnus-newsgroup-unreads gnus-newsgroup-expirable))
      ;; Do adaptive scoring, and possibly save score files.
      (and gnus-newsgroup-adaptive
	   (gnus-score-adaptive))
      (and gnus-use-scoring 
	   (fboundp 'gnus-score-save)
	   (funcall 'gnus-score-save))
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
    ;; Make all changes in this group permanent.
    (gnus-summary-update-info)		
    (set-buffer buf)
    (and gnus-use-cache (gnus-cache-possibly-remove-articles))
    ;; Make sure where I was, and go to next newsgroup.
    (set-buffer gnus-group-buffer)
    (or quit-config
	(progn
	  (gnus-group-jump-to-group group)
	  (gnus-group-next-unread-group 1)))
    (if temporary
	nil				;Nothing to do.
      ;; We set all buffer-local variables to nil. It is unclear why
      ;; this is needed, but if we don't, buffer-local variables are
      ;; not garbage-collected, it seems. This would the lead to en
      ;; ever-growing Emacs.
      (set-buffer buf)
      (gnus-summary-clear-local-variables)
      ;; We clear the global counterparts of the buffer-local
      ;; variables as well, just to be on the safe side.
      (gnus-configure-windows 'group 'force)
      (gnus-summary-clear-local-variables)
      ;; Return to group mode buffer. 
      (if (eq mode 'gnus-summary-mode)
	  (gnus-kill-buffer buf))
      (if (get-buffer gnus-article-buffer)
	  (bury-buffer gnus-article-buffer))
      (setq gnus-current-select-method gnus-select-method)
      (pop-to-buffer gnus-group-buffer)
      (if (not quit-config)
	  (progn
	    (gnus-group-jump-to-group group)
	    (gnus-group-next-unread-group 1))
	(if (not (buffer-name (car quit-config)))
	    (gnus-configure-windows 'group 'force)
	  (set-buffer (car quit-config))
	  (and (eq major-mode 'gnus-summary-mode)
	       (gnus-set-global-variables))
	  (gnus-configure-windows (cdr quit-config))))
      (run-hooks 'gnus-summary-exit-hook))))

(defalias 'gnus-summary-quit 'gnus-summary-exit-no-update)
(defun gnus-summary-exit-no-update (&optional no-questions)
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (gnus-set-global-variables)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config group)))
    (if (or no-questions
	    gnus-expert-user
	    (gnus-y-or-n-p "Do you really wanna quit reading this group? "))
	(progn
	  (gnus-close-group group)
	  (gnus-summary-clear-local-variables)
	  (set-buffer gnus-group-buffer)
	  (gnus-summary-clear-local-variables)
	  ;; Return to group selection mode.
	  (gnus-configure-windows 'group 'force)
	  (if (get-buffer gnus-summary-buffer)
	      (kill-buffer gnus-summary-buffer))
	  (if (get-buffer gnus-article-buffer)
	      (bury-buffer gnus-article-buffer))
	  (if (equal (gnus-group-group-name) group)
	      (gnus-group-next-unread-group 1))
	  (if quit-config
	      (progn
		(if (not (buffer-name (car quit-config)))
		    (gnus-configure-windows 'group 'force)
		  (set-buffer (car quit-config))
		  (and (eq major-mode 'gnus-summary-mode)
		       (gnus-set-global-variables))
		  (gnus-configure-windows (cdr quit-config)))))))))

;; Suggested by Andrew Eskilsson <pi92ae@pt.hk-r.se>.
(defun gnus-summary-fetch-faq (group)
  "Fetch the FAQ for the current group."
  (interactive (list gnus-newsgroup-name))
  (let (gnus-faq-buffer)
    (and (setq gnus-faq-buffer (gnus-group-fetch-faq group))
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
		(substitute-command-keys "\\<gnus-summary-mode-map>\\[gnus-summary-next-page]:Select  \\[gnus-summary-next-unread-article]:Forward  \\[gnus-summary-prev-unread-article]:Backward  \\[gnus-summary-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-summary-describe-briefly]:This help")))

;; Walking around group mode buffer from summary mode.

(defun gnus-summary-next-group (&optional no-article target-group backward)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected
initially. If NEXT-GROUP, go to this group. If BACKWARD, go to
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

(defun gnus-summary-next-group-old (&optional no-article group backward)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially.
If BACKWARD, go to previous group instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((ingroup gnus-newsgroup-name)
	(sumbuf (current-buffer))
	num)
    (set-buffer gnus-group-buffer)
    (if (and group
	     (or (and (numberp (setq num (car (gnus-gethash
					       group gnus-newsrc-hashtb))))
		      (< num 1))
		 (null num)))
	(progn
	  (gnus-group-jump-to-group group)
	  (setq group nil))
      (gnus-group-jump-to-group ingroup))
    (gnus-summary-search-group backward)
    (let ((group (or group (gnus-summary-search-group backward))))
      (set-buffer sumbuf)
      (gnus-summary-exit t)		;Update all information.
      (if (null group)
	  (gnus-summary-exit-no-update t)
	(gnus-group-jump-to-group ingroup)
	(setq group (gnus-summary-search-group backward))
	(gnus-message 5 "Selecting %s..." group)
	(set-buffer gnus-group-buffer)
	;; We are now in group mode buffer.
	;; Make sure group mode buffer point is on GROUP.
	(gnus-group-jump-to-group group)
	(if (not (eq gnus-auto-select-next 'quietly))
	    (progn
	      (gnus-summary-read-group group nil no-article sumbuf)
	      (and (string= gnus-newsgroup-name ingroup)
		   (bufferp sumbuf) (buffer-name sumbuf)
		   (progn
		     (set-buffer (setq gnus-summary-buffer sumbuf))
		     (gnus-summary-exit-no-update t))))
	  (let ((prevgroup group))
	    (gnus-group-jump-to-group ingroup)
	    (setq group (gnus-summary-search-group backward))
	    (gnus-summary-read-group group nil no-article sumbuf)
	    (while (and (string= gnus-newsgroup-name ingroup)
			(bufferp sumbuf) 
			(buffer-name sumbuf)
			(not (string= prevgroup (gnus-group-group-name))))
	      (set-buffer gnus-group-buffer)
	      (gnus-summary-read-group 
	       (setq prevgroup (gnus-group-group-name)) 
	       nil no-article sumbuf))
	    (and (string= prevgroup (gnus-group-group-name))
		 ;; We have reached the final group in the group
		 ;; buffer.
		 (progn
		   (if (buffer-name sumbuf)
		       (progn
			 (set-buffer sumbuf)
			 (gnus-summary-exit)))))))))))

(defun gnus-summary-prev-group (&optional no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  (gnus-summary-next-group no-article nil t))

;; Walking around summary lines.

(defun gnus-summary-first-subject (&optional unread)
  "Go to the first unread subject.
If UNREAD is non-nil, go to the first unread article.
Returns nil if there are no unread articles."
  (interactive "P")
  (prog1
      (cond ((not unread)
	     (goto-char (point-min)))
	    ((gnus-goto-char 
	      (text-property-any 
	       (point-min) (point-max) 'gnus-mark gnus-unread-mark))
	     t)
	    (t 
	     ;; There are no unread articles.
	     (gnus-message 3 "No more unread articles")
	     nil))
    (gnus-summary-position-cursor)))

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
		(gnus-summary-search-forward unread nil backward))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more%s articles"
			       (if unread " unread" "")))
    (or dont-display
	(progn
	  (gnus-summary-recenter)
	  (gnus-summary-position-cursor)))
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

(defun gnus-summary-goto-subject (article)
  "Go the subject line of ARTICLE."
  (interactive
   (list
    (string-to-int
     (completing-read "Article number: "
		      (mapcar
		       (lambda (headers)
			 (list
			  (int-to-string (mail-header-number headers))))
		       gnus-newsgroup-headers)
		      nil 'require-match))))
  (or article (error "No article number"))
  (let ((b (point)))
    (if (not (gnus-goto-char (text-property-any (point-min) (point-max)
						'gnus-number article)))
	()
      (gnus-summary-show-thread)
      ;; Skip dummy articles. 
      (if (eq (gnus-summary-article-mark) gnus-dummy-mark)
	  (forward-line 1))
      (prog1
	  (if (not (eobp))
	      article
	    (goto-char b)
	    nil)
	(gnus-summary-position-cursor)))))

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
	(gnus-article-prepare article all-header)
      (gnus-summary-show-thread)
      (if (eq (gnus-summary-article-mark) gnus-dummy-mark)
	  (progn
	    (forward-line 1)
	    (gnus-summary-position-cursor)))
      (run-hooks 'gnus-select-article-hook)
      (gnus-summary-recenter)
      (gnus-summary-goto-subject article)
      ;; Successfully display article.
      (gnus-summary-update-line)
      (gnus-article-set-window-start 
       (cdr (assq article gnus-newsgroup-bookmarks)))
      t)))

(defun gnus-summary-select-article (&optional all-headers force pseudo article)
  "Select the current article.
If ALL-HEADERS is non-nil, show all header fields.  If FORCE is
non-nil, the article will be re-fetched even if it already present in
the article buffer.  If PSEUDO is non-nil, pseudo-articles will also
be displayed."
  (and (not pseudo) (gnus-summary-pseudo-article)
       (error "This is a pseudo-article."))
  (let ((article (or article (gnus-summary-article-number)))
	(all-headers (not (not all-headers))) ;Must be T or NIL.
	did) 
    (prog1
	(save-excursion
	  (set-buffer gnus-summary-buffer)
	  (if (or (null gnus-current-article)
		  (null gnus-article-current)
		  (null (get-buffer gnus-article-buffer))
		  (not (eq article (cdr gnus-article-current)))
		  (not (equal (car gnus-article-current) gnus-newsgroup-name))
		  force)
	      ;; The requested article is different from the current article.
	      (progn
		(gnus-summary-display-article article all-headers)
		(setq did article))
	    (if (or all-headers gnus-show-all-headers) 
		(gnus-article-show-all-headers))
	    nil))
      (if did 
	  (gnus-article-set-window-start 
	   (cdr (assq article gnus-newsgroup-bookmarks)))))))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Obsolete function."
  nil)

(defun gnus-summary-next-article (&optional unread subject backward)
  "Select the next article.
If UNREAD, only unread articles are selected.
If SUBJECT, only articles with SUBJECT are selected.
If BACKWARD, the previous article is selected instead of the next."
  (interactive "P")
  (gnus-set-global-variables)
  (let (header)
    (cond
     ;; Is there such an article?
     ((and (gnus-summary-search-forward unread subject backward)
	   (or (gnus-summary-display-article (gnus-summary-article-number))
	       (eq (gnus-summary-article-mark) gnus-canceled-mark)))
      (gnus-summary-position-cursor))
     ;; If not, we try the first unread, if that is wanted.
     ((and subject
	   gnus-auto-select-same
	   (or (gnus-summary-first-unread-article)
	       (eq (gnus-summary-article-mark) gnus-canceled-mark)))
      (gnus-summary-position-cursor)
      (gnus-message 6 "Wrapped"))
     ;; Try to get next/previous article not displayed in this group.
     ((and gnus-auto-extend-newsgroup
	   (not unread) (not subject)
	   (setq header (gnus-more-header-forward backward)))
      (gnus-extend-newsgroup header backward)
      (let ((buffer-read-only nil))
	(goto-char (if backward (point-min) (point-max)))
	(gnus-summary-prepare-threads (list header)))
      (gnus-summary-goto-article (if backward gnus-newsgroup-begin
				   gnus-newsgroup-end)))
     ;; Go to next/previous group.
     (t
      (or (gnus-ephemeral-group-p gnus-newsgroup-name)
	  (gnus-summary-jump-to-group gnus-newsgroup-name))
      (let ((cmd last-command-char)
	    (group 
	     (if (eq gnus-keep-same-level 'best) 
		 (gnus-summary-best-group gnus-newsgroup-name)
	       (gnus-summary-search-group backward gnus-keep-same-level))))
	;; For some reason, the group window gets selected. We change
	;; it back.  
	(select-window (get-buffer-window (current-buffer)))
	;; Keep just the event type of CMD.
					;(and (listp cmd) (setq cmd (car cmd)))
	;; Select next unread newsgroup automagically.
	(cond 
	 ((not gnus-auto-select-next)
	  (gnus-message 7 "No more%s articles" (if unread " unread" "")))
	 ((eq gnus-auto-select-next 'quietly)
	  ;; Select quietly.
	  (if (gnus-ephemeral-group-p gnus-newsgroup-name)
	      (gnus-summary-exit)
	    (gnus-message 7 "No more%s articles (%s)..."
			  (if unread " unread" "") 
			  (if group (concat "selecting " group)
			    "exiting"))
	    (gnus-summary-next-group nil group backward)))
	 (t
	  (let ((keystrokes '(?\C-n ?\C-p))
		key)
	    (while (or (null key) (memq key keystrokes))
	      (gnus-message 
	       7 "No more%s articles%s" (if unread " unread" "")
	       (if (and group 
			(not (gnus-ephemeral-group-p gnus-newsgroup-name)))
		   (format " (Type %s for %s [%s])"
			   (single-key-description cmd) group
			   (car (gnus-gethash group gnus-newsrc-hashtb)))
		 (format " (Type %s to exit %s)"
			 (single-key-description cmd)
			 gnus-newsgroup-name)))
	      ;; Confirm auto selection.
	      (let* ((event (read-char)))
		(setq key (if (listp event) (car event) event))
		(if (memq key keystrokes)
		    (let ((obuf (current-buffer)))
		      (switch-to-buffer gnus-group-buffer)
		      (and group
			   (gnus-group-jump-to-group group))
		      (condition-case ()
			  (execute-kbd-macro (char-to-string key))
			(error (ding) nil))
		      (setq group (gnus-group-group-name))
		      (switch-to-buffer obuf)))))
	    (if (equal key cmd)
		(if (or (not group)
			(gnus-ephemeral-group-p gnus-newsgroup-name))
		    (gnus-summary-exit)
		  (gnus-summary-next-group nil group backward))
	      (execute-kbd-macro (char-to-string key)))))))))))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string))))

(defun gnus-summary-prev-article (&optional unread subject)
  "Select the article after the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (gnus-summary-next-article unread subject t))

(defun gnus-summary-prev-unread-article ()
  "Select unred article before current one."
  (interactive)
  (gnus-summary-prev-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string))))

(defun gnus-summary-next-page (&optional lines circular)
  "Show next page of selected article.
If end of article, select next article.
Argument LINES specifies lines to be scrolled up.
If CIRCULAR is non-nil, go to the start of the article instead of 
instead of selecting the next article when reaching the end of the
current article." 
  (interactive "P")
  (setq gnus-summary-buffer (current-buffer))
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number))
	(endp nil))
    (gnus-configure-windows 'article)
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-eval-in-buffer-window
       gnus-article-buffer
       (setq endp (gnus-article-next-page lines)))
      (if endp
 	  (cond (circular
 		 (gnus-summary-beginning-of-article))
 		(lines
 		 (gnus-message 3 "End of message"))
 		((null lines)
 		 (gnus-summary-next-unread-article)))))
    (gnus-summary-recenter)
    (gnus-summary-position-cursor)))

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
  (gnus-summary-position-cursor))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-configure-windows 'article)
  (or (gnus-summary-select-article nil nil 'pseudo)
      (gnus-eval-in-buffer-window 
       gnus-article-buffer
       (cond ((> lines 0)
	      (if (gnus-article-next-page lines)
		  (gnus-message 3 "End of message")))
	     ((< lines 0)
	      (gnus-article-prev-page (- lines))))))
  (gnus-summary-recenter)
  (gnus-summary-position-cursor))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-next-article nil (gnus-summary-subject-string)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-prev-article nil (gnus-summary-subject-string)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-next-article t (gnus-summary-subject-string)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-prev-article t (gnus-summary-subject-string)))

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
    (gnus-summary-position-cursor)))

(defun gnus-summary-best-unread-article ()
  "Select the unread article with the highest score."
  (interactive)
  (gnus-set-global-variables)
  (let ((best -1000000)
	article score)
    (save-excursion
      (or (gnus-summary-first-subject t)
	  (error "No unread articles"))
      (while 
	  (and
	   (progn
	     (and (> (setq score (gnus-summary-article-score)) best)
		  (setq best score
			article (gnus-summary-article-number)))
	     t)
	   (gnus-summary-search-subject nil t))))
    (if (not article)
	(error "No unread articles")
      (gnus-summary-goto-article article))
    (gnus-summary-position-cursor)))

(defun gnus-summary-goto-article (article &optional all-headers)
  "Fetch ARTICLE and display it if it exists.
If ALL-HEADERS is non-nil, no header lines are hidden."
  (interactive
   (list
    (string-to-int
     (completing-read 
      "Article number: "
      (mapcar (lambda (headers) 
		(list (int-to-string (mail-header-number headers))))
	      gnus-newsgroup-headers) 
      nil 'require-match))))
  (prog1
      (and (gnus-summary-goto-subject article)
	   (gnus-summary-display-article article all-headers))
    (gnus-summary-position-cursor)))

(defun gnus-summary-goto-last-article ()
  "Go to the previously read article."
  (interactive)
  (prog1
      (and gnus-last-article
	   (gnus-summary-goto-article gnus-last-article))
    (gnus-summary-position-cursor)))

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
  (gnus-summary-position-cursor))

;; Summary article oriented commands

(defun gnus-summary-refer-parent-article (n)
  "Refer parent article N times.
The difference between N and the number of articles fetched is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (while 
      (and 
       (> n 0)
       (let ((ref (mail-header-references (gnus-get-header-by-num
					   (gnus-summary-article-number)))))
	 (if (and ref (not (equal ref ""))
		  (string-match "<[^<>]*>[ \t]*$" ref))
	     (gnus-summary-refer-article 
	      (substring ref (match-beginning 0) (match-end 0)))
	   (gnus-message 1 "No references in article %d"
			 (gnus-summary-article-number))
	   nil)))
    (setq n (1- n)))
  (gnus-summary-position-cursor)
  n)
    
(defun gnus-summary-refer-article (message-id)
  "Refer article specified by MESSAGE-ID.
NOTE: This command only works with newsgroups that use real or simulated NNTP."
  (interactive "sMessage-ID: ")
  (if (or (not (stringp message-id))
	  (zerop (length message-id)))
      ()
    ;; Construct the correct Message-ID if necessary.
    ;; Suggested by tale@pawl.rpi.edu.
    (or (string-match "^<" message-id)
	(setq message-id (concat "<" message-id)))
    (or (string-match ">$" message-id)
	(setq message-id (concat message-id ">")))
    (let ((header (car (gnus-gethash (downcase message-id)
				     gnus-newsgroup-dependencies))))
      (if header
	  (or (gnus-summary-goto-article (mail-header-number header))
	      ;; The header has been read, but the article had been
	      ;; expunged, so we insert it again.
	      (progn
		(gnus-summary-insert-line
		 nil header 0 nil gnus-read-mark nil nil
		 (mail-header-subject header))
		(forward-line -1)
		(mail-header-number header)))
	(let ((gnus-override-method gnus-refer-article-method)
	      (gnus-ancient-mark gnus-read-mark)
	      (tmp-point (window-start
			  (get-buffer-window gnus-article-buffer)))
	      number tmp-buf)
	  (and gnus-refer-article-method
	       (gnus-check-server gnus-refer-article-method))
	  ;; Save the old article buffer.
	  (save-excursion
	    (set-buffer (gnus-article-setup-buffer))
	    (gnus-kill-buffer " *temp Article*")
	    (setq tmp-buf (rename-buffer " *temp Article*")))
	  (prog1
	      (if (gnus-article-prepare 
		   message-id nil (gnus-read-header message-id))
		  (progn
		    (setq number (mail-header-number gnus-current-headers))
		    (gnus-rebuild-thread message-id)
		    (gnus-summary-goto-subject number)
		    (if (null gnus-use-full-window)
			(progn
			  (delete-windows-on tmp-buf)
			  (gnus-configure-windows 'article 'force)))
		    (gnus-summary-recenter)
		    (gnus-article-set-window-start 
		     (cdr (assq number gnus-newsgroup-bookmarks)))
		    (and gnus-visual
			 (run-hooks 'gnus-visual-mark-article-hook))
		    message-id)
		;; We restore the old article buffer.
		(save-excursion
		  (kill-buffer gnus-article-buffer)
		  (set-buffer tmp-buf)
		  (rename-buffer gnus-article-buffer)
		  (let ((buffer-read-only nil))
		    (and tmp-point
			 (set-window-start (get-buffer-window (current-buffer))
					   tmp-point)))))))))))

(defun gnus-summary-enter-digest-group ()
  "Enter a digest group based on the current article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  ;; We do not want a narrowed article.
  (gnus-summary-stop-page-breaking)
  (let ((name (format "%s-%d" 
		      (gnus-group-prefixed-name 
		       gnus-newsgroup-name (list 'nndoc "")) 
		      gnus-current-article))
	(ogroup gnus-newsgroup-name)
	(buf (current-buffer)))
    (if (gnus-group-read-ephemeral-group 
	 name (list 'nndoc name
		    (list 'nndoc-address (get-buffer gnus-article-buffer))
		    '(nndoc-article-type digest))
	 t)
	(setcdr (nthcdr 4 (nth 2 (gnus-gethash name gnus-newsrc-hashtb)))
		(list (list (cons 'to-group ogroup))))
      (switch-to-buffer buf)
      (gnus-set-global-variables)
      (gnus-configure-windows 'summary)
      (gnus-message 3 "Article not a digest?"))))

(defun gnus-summary-isearch-article ()
  "Do incremental search forward on current article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window 
   gnus-article-buffer (isearch-forward)))

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
  (if (gnus-summary-search-article regexp backward)
      (gnus-article-set-window-start 
       (cdr (assq (gnus-summary-article-number) gnus-newsgroup-bookmarks)))
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
gnus-select-article-hook is not called during the search."
  (let ((gnus-select-article-hook nil)	;Disable hook.
	(gnus-mark-article-hook nil)	;Inhibit marking as read.
	(re-search
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(found nil)
	(last nil))
    ;; Hidden thread subtrees must be searched for ,too.
    (gnus-summary-show-all-threads)
    (if (eobp) (forward-line -1))
    ;; First of all, search current article.
    ;; We don't want to read article again from NNTP server nor reset
    ;; current point.
    (gnus-summary-select-article)
    (gnus-message 9 "Searching article: %d..." gnus-current-article)
    (setq last gnus-current-article)
    (gnus-eval-in-buffer-window
     gnus-article-buffer
     (save-restriction
       (widen)
       ;; Begin search from current point.
       (setq found (funcall re-search regexp nil t))))
    ;; Then search next articles.
    (while (and (not found)
		(gnus-summary-display-article 
		 (gnus-summary-search-subject backward nil nil)))
      (gnus-message 9 "Searching article: %d..." gnus-current-article)
      (gnus-eval-in-buffer-window
       gnus-article-buffer
       (save-restriction
	 (widen)
	 (goto-char (if backward (point-max) (point-min)))
	 (setq found (funcall re-search regexp nil t)))))
    (message "")
    ;; Adjust article pointer.
    (or (eq last gnus-current-article)
	(setq gnus-last-article last))
    ;; Return T if found such article.
    found))

(defun gnus-summary-execute-command (header regexp command &optional backward)
  "Search forward for an article whose HEADER matches REGEXP and execute COMMAND.
If HEADER is an empty string (or nil), the match is done on the entire
article. If BACKWARD (the prefix) is non-nil, search backward instead."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read 
	    "Header name: "
	    (mapcar (lambda (string) (list string))
		    '("Number" "Subject" "From" "Lines" "Date"
		      "Message-ID" "Xref" "References"))
	    nil 'require-match))
	 (read-string "Regexp: ")
	 (read-key-sequence "Command: ")
	 current-prefix-arg))
  (gnus-set-global-variables)
  ;; Hidden thread subtrees must be searched as well.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (gnus-message 6 "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute header regexp
		    (` (lambda ()
			 (call-interactively '(, (key-binding command)))))
		    backward)
      (gnus-message 6 "Executing %s...done" (key-description command)))))

(defun gnus-summary-beginning-of-article ()
  "Scroll the article back to the beginning."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window
   gnus-article-buffer
   (widen)
   (goto-char (point-min))
   (and gnus-break-pages (gnus-narrow-to-page))))

(defun gnus-summary-end-of-article ()
  "Scroll to the end of the article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window 
   gnus-article-buffer
   (widen)
   (goto-char (point-max))
   (recenter -3)
   (and gnus-break-pages (gnus-narrow-to-page))))

(defun gnus-summary-show-article ()
  "Force re-fetching of the current article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article nil 'force)
  (gnus-configure-windows 'article)
  (gnus-summary-position-cursor))

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
    (let ((buffer-read-only nil))
      (if (numberp arg) 
	  (if (> arg 0) (remove-text-properties (point-min) (point-max) 
						gnus-hidden-properties)
	    (if (< arg 0) (run-hooks 'gnus-article-display-hook)))
	(if (text-property-any (point-min) (point-max) 'invisible t)
	    (remove-text-properties 
	     (point-min) (point-max) gnus-hidden-properties)
	  ;; We hide the headers. This song and dance act below is
	  ;; done because `gnus-have-all-headers' is buffer-local to
	  ;; the summary buffer, and we only want to temporarily
	  ;; change it in that buffer. Ugh.
	  (let ((have gnus-have-all-headers))
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (setq gnus-have-all-headers nil)
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(run-hooks 'gnus-article-display-hook))
	      (setq gnus-have-all-headers have)))))
      (set-window-point (get-buffer-window (current-buffer)) (point-min)))))

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
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       (let ((start (window-start)))
	 (news-caesar-buffer-body arg)
	 (set-window-start (get-buffer-window (current-buffer)) start))))))

(defun gnus-summary-stop-page-breaking ()
  "Stop page breaking in the current article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer (widen)))

;; Suggested by Brian Edmonds <bedmonds@prodigy.bc.ca>.

(defun gnus-summary-move-article (&optional n to-newsgroup select-method)
  "Move the current article to a different newsgroup.
If N is a positive number, move the N next articles.
If N is a negative number, move the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to. 
If SELECT-METHOD is symbol, do not move to a specific newsgroup, but
re-spool using this method.
For this function to work, both the current newsgroup and the
newsgroup that you want to move to have to support the `request-move'
and `request-accept' functions. (Ie. mail newsgroups at present.)"
  (interactive "P")
  (gnus-set-global-variables)
  (or (gnus-check-backend-function 'request-move-article gnus-newsgroup-name)
      (error "The current newsgroup does not support article moving"))
  (let ((articles (gnus-summary-work-articles n))
	(prefix (gnus-group-real-prefix gnus-newsgroup-name))
	art-group to-method sel-met)
    (if (and (not to-newsgroup) (not select-method))
	(setq to-newsgroup
	      (completing-read 
	       (format "Where do you want to move %s? %s"
		       (if (> (length articles) 1)
			   (format "these %d articles" (length articles))
			 "this article")
		       (if gnus-current-move-group
			   (format "(%s default) " gnus-current-move-group)
			 ""))
	       gnus-active-hashtb nil nil prefix)))
    (if to-newsgroup
        (progn
          (if (or (string= to-newsgroup "") (string= to-newsgroup prefix))
              (setq to-newsgroup (or gnus-current-move-group "")))
          (or (gnus-gethash to-newsgroup gnus-active-hashtb)
	      (gnus-activate-group to-newsgroup)
              (error "No such group: %s" to-newsgroup))
          (setq gnus-current-move-group to-newsgroup)))
    (setq to-method (if select-method (list select-method "")
		      (gnus-find-method-for-group to-newsgroup)))
    (or (gnus-check-backend-function 'request-accept-article (car to-method))
	(error "%s does not support article copying" (car to-method)))
    (or (gnus-check-server to-method)
	(error "Can't open server %s" (car to-method)))
    (gnus-message 6 "Moving to %s: %s..." 
		  (or select-method to-newsgroup) articles)
    (while articles
      (if (setq art-group
		(gnus-request-move-article 
		 (car articles)		; Article to move
		 gnus-newsgroup-name	; From newsgroup
		 (nth 1 (gnus-find-method-for-group 
			 gnus-newsgroup-name)) ; Server
		 (list 'gnus-request-accept-article 
		       (if select-method
			   (list 'quote select-method)
			 to-newsgroup)
		       (not (cdr articles))) ; Accept form
		 (not (cdr articles))))	; Only save nov last time
	  (let* ((buffer-read-only nil)
		 (entry 
		  (or
		   (gnus-gethash (car art-group) gnus-newsrc-hashtb)
		   (gnus-gethash 
		    (gnus-group-prefixed-name 
		     (car art-group) 
		     (if select-method (list select-method "")
		       (gnus-find-method-for-group to-newsgroup)))
		    gnus-newsrc-hashtb)))
		 (info (nth 2 entry))
		 (article (car articles)))
	    (gnus-summary-goto-subject article)
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 1) (point)))
	    ;; Update the group that has been moved to.
	    (if (not info)
		()			; This group does not exist yet.
	      (if (not (memq article gnus-newsgroup-unreads))
		  (setcar (cdr (cdr info))
			  (gnus-add-to-range (nth 2 info) 
					     (list (cdr art-group)))))
	      ;; Copy any marks over to the new group.
	      (let ((marks '((tick . gnus-newsgroup-marked)
			     (dormant . gnus-newsgroup-dormant)
			     (expire . gnus-newsgroup-expirable)
			     (bookmark . gnus-newsgroup-bookmarks)
			     (reply . gnus-newsgroup-replied)))
		    (to-article (cdr art-group)))
		(while marks
		  (if (memq article (symbol-value (cdr (car marks))))
		      (gnus-add-marked-articles 
		       (car info) (car (car marks)) (list to-article) info))
		  (setq marks (cdr marks)))))
	    ;; Update marks.
	    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
	    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
	    (setq gnus-newsgroup-dormant
		  (delq article gnus-newsgroup-dormant))
	    (setq gnus-newsgroup-reads
		  (cons (cons article gnus-canceled-mark)
			gnus-newsgroup-reads)))
	(gnus-message 1 "Couldn't move article %s" (car articles)))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))
    (gnus-set-mode-line 'summary)))

(defun gnus-summary-respool-article (&optional n respool-method)
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
  (interactive "P")
  (gnus-set-global-variables)
  (let ((respool-methods (gnus-methods-using 'respool))
	(methname 
	 (symbol-name (car (gnus-find-method-for-group gnus-newsgroup-name)))))
    (or respool-method
	(setq respool-method
	      (completing-read
	       "What method do you want to use when respooling? "
	       respool-methods nil t methname)))
    (or (string= respool-method "")
	(if (assoc (symbol-name
		    (car (gnus-find-method-for-group gnus-newsgroup-name)))
		   respool-methods)
	    (gnus-summary-move-article n nil (intern respool-method))
	  (gnus-summary-copy-article n nil (intern respool-method))))))

;; Suggested by gregj@unidata.com (Gregory J. Grubbs).
(defun gnus-summary-copy-article (&optional n to-newsgroup select-method)
  "Move the current article to a different newsgroup.
If N is a positive number, move the N next articles.
If N is a negative number, move the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to. 
If SELECT-METHOD is symbol, do not move to a specific newsgroup, but
re-spool using this method.
For this function to work, the newsgroup that you want to move to have
to support the `request-move' and `request-accept'
functions. (Ie. mail newsgroups at present.)"
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n))
	(copy-buf (get-buffer-create "*copy work*"))
	(prefix (gnus-group-real-prefix gnus-newsgroup-name))
	art-group to-method)
    (buffer-disable-undo copy-buf)
    (if (and (not to-newsgroup) (not select-method))
	(setq to-newsgroup
	      (completing-read 
	       (format "Where do you want to copy %s? %s"
		       (if (> (length articles) 1)
			   (format "these %d articles" (length articles))
			 "this article")
		       (if gnus-current-move-group
			   (format "(%s default) " gnus-current-move-group)
			 ""))
	       gnus-active-hashtb nil nil prefix)))
    (if to-newsgroup
        (progn
          (if (or (string= to-newsgroup "") (string= to-newsgroup prefix))
              (setq to-newsgroup (or gnus-current-move-group "")))
          (or (gnus-gethash to-newsgroup gnus-active-hashtb)
	      (gnus-activate-group to-newsgroup)
              (error "No such group: %s" to-newsgroup))
          (setq gnus-current-move-group to-newsgroup)))
    (setq to-method (if select-method (list select-method "")
		      (gnus-find-method-for-group to-newsgroup)))
    (or (gnus-check-backend-function 'request-accept-article (car to-method))
	(error "%s does not support article copying" (car to-method)))
    (or (gnus-check-server to-method)
	(error "Can't open server %s" (car to-method)))
    (while articles
      (gnus-message 6 "Copying to %s: %s..." 
		    (or select-method to-newsgroup) articles)
      (if (setq art-group
		(save-excursion
		  (set-buffer copy-buf)
		  (gnus-request-article-this-buffer
		   (car articles) gnus-newsgroup-name)
		  (gnus-request-accept-article
		   (if select-method (list 'quote select-method) to-newsgroup)
		   (not (cdr articles)))))
	  (let* ((entry 
		  (or
		   (gnus-gethash (car art-group) gnus-newsrc-hashtb)
		   (gnus-gethash 
		    (gnus-group-prefixed-name 
		     (car art-group) 
		     (if select-method (list select-method "")
		       (gnus-find-method-for-group to-newsgroup)))
		    gnus-newsrc-hashtb)))
		 (info (nth 2 entry))
		 (article (car articles)))
	    ;; We copy the info over to the new group.
	    (if (not info)
		()			; This group does not exist (yet).
	      (if (not (memq article gnus-newsgroup-unreads))
		  (setcar (cdr (cdr info))
			  (gnus-add-to-range (nth 2 info) 
					     (list (cdr art-group)))))
	      ;; Copy any marks over to the new group.
	      (let ((marks '((tick . gnus-newsgroup-marked)
			     (dormant . gnus-newsgroup-dormant)
			     (expire . gnus-newsgroup-expirable)
			     (bookmark . gnus-newsgroup-bookmarks)
			     (reply . gnus-newsgroup-replied)))
		    (to-article (cdr art-group)))
		(while marks
		  (if (memq article (symbol-value (cdr (car marks))))
		      (gnus-add-marked-articles 
		       (car info) (car (car marks)) (list to-article) info))
		  (setq marks (cdr marks))))))
	(gnus-message 1 "Couldn't copy article %s" (car articles)))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))
    (kill-buffer copy-buf)))

(defun gnus-summary-import-article (file)
  "Import a random file into a mail newsgroup."
  (interactive "fImport file: ")
  (let ((group gnus-newsgroup-name)
	atts)
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
      (if (nnheader-article-p)
	  ()
	(setq atts (file-attributes file))
	(insert "From: " (read-string "From: ") "\n"
		"Subject: " (read-string "Subject: ") "\n"
		"Date: " (current-time-string (nth 5 atts)) "\n"
		"Chars: " (int-to-string (nth 7 atts)) "\n\n"))
      (gnus-request-accept-article group t)
      (kill-buffer (current-buffer)))))

(defun gnus-summary-expire-articles ()
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (if (not (gnus-check-backend-function 
	    'request-expire-articles gnus-newsgroup-name))
      ()
    (let* ((info (nth 2 (gnus-gethash gnus-newsgroup-name 
				      gnus-newsrc-hashtb)))
	   (total (memq 'total-expire (nth 5 info)))
	   (expirable (if total
			  (gnus-list-of-read-articles gnus-newsgroup-name)
			(setq gnus-newsgroup-expirable
			      (sort gnus-newsgroup-expirable '<))))
	   es)
      (if (not expirable)
	  ()
	(gnus-message 6 "Expiring articles...")
	;; The list of articles that weren't expired is returned.
	(setq es (gnus-request-expire-articles expirable gnus-newsgroup-name))
	(or total (setq gnus-newsgroup-expirable es))
	;; We go through the old list of expirable, and mark all
	;; really expired articles as nonexistent.
	(or (eq es expirable)		;If nothing was expired, we don't mark.
	    (let ((gnus-use-cache nil))
	      (while expirable
		(or (memq (car expirable) es)
		    (gnus-summary-mark-article
		     (car expirable) gnus-canceled-mark))
		(setq expirable (cdr expirable)))))
	(gnus-message 6 "Expiring articles...done")))))

(defun gnus-summary-expire-articles-now ()
  "Expunge all expirable articles in the current group.
This means that *all* articles that are marked as expirable will be
deleted forever, right now."
  (interactive)
  (or gnus-expert-user
      (gnus-y-or-n-p
       "Are you really, really, really sure you want to expunge? ")
      (error "Phew!"))
  (let ((nnmail-expiry-wait -1)
	(nnmail-expiry-wait-function nil))
    (gnus-summary-expire-articles)))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-summary-delete-article (&optional n)
  "Delete the N next (mail) articles.
This command actually deletes articles. This is not a marking
command. The article will disappear forever from you life, never to
return. 
If N is negative, delete backwards.
If N is nil and articles have been marked with the process mark,
delete these instead."
  (interactive "P")
  (or (gnus-check-backend-function 'request-expire-articles 
				   gnus-newsgroup-name)
      (error "The current newsgroup does not support article deletion."))
  ;; Compute the list of articles to delete.
  (let ((articles (gnus-summary-work-articles n))
	not-deleted)
    (if (and gnus-novice-user
	     (not (gnus-y-or-n-p 
		   (format "Do you really want to delete %s forever? "
			   (if (> (length articles) 1) "these articles"
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
    (gnus-summary-position-cursor)
    (gnus-set-mode-line 'summary)
    not-deleted))

(defun gnus-summary-edit-article (&optional force)
  "Enter into a buffer and edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (or force
      (not (gnus-group-read-only-p))
      (error "The current newsgroup does not support article editing."))
  (gnus-summary-select-article t)
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
  (search-forward "\n\n" nil t))

(defun gnus-summary-edit-article-done ()
  "Make edits to the current article permanent."
  (interactive)
  (if (gnus-group-read-only-p)
      (progn
	(gnus-summary-edit-article-postpone)
	(message "The current newsgroup does not support article editing.")
	(ding))
    (let ((buf (buffer-substring-no-properties (point-min) (point-max))))
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
	(gnus-configure-windows 'summary))
      (and gnus-visual (run-hooks 'gnus-visual-mark-article-hook)))))

(defun gnus-summary-edit-article-postpone ()
  "Postpone changes to the current article."
  (interactive)
  (gnus-article-mode)
  (use-local-map gnus-article-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (gnus-configure-windows 'summary)
  (and gnus-visual (run-hooks 'gnus-visual-mark-article-hook)))

(defun gnus-summary-fancy-query ()
  "Query where the fancy respool algorithm would put this article."
  (interactive)
  (gnus-summary-select-article)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (goto-char (point-min))
      (search-forward "\n\n")
      (narrow-to-region (point-min) (point))
      (pp-eval-expression (list 'quote (nnmail-split-fancy))))))

;; Summary score commands.

;; Suggested by boubaker@cenatls.cena.dgac.fr.

(defun gnus-summary-raise-score (n)
  "Raise the score of the current article by N."
  (interactive "p")
  (gnus-summary-set-score (+ (gnus-summary-article-score) n)))

(defun gnus-summary-set-score (n)
  "Set the score of the current article to N."
  (interactive "p")
  ;; Skip dummy header line.
  (save-excursion
    (gnus-summary-show-thread)
    (if (eq (gnus-summary-article-mark) gnus-dummy-mark)
	(forward-line 1))
    (let ((buffer-read-only nil))
      ;; Set score.
      (gnus-summary-update-mark
       (if (= n (or gnus-summary-default-score 0)) ? 
	 (if (< n (or gnus-summary-default-score 0)) 
	     gnus-score-below-mark gnus-score-over-mark)) 'score))
    (let* ((article (gnus-summary-article-number))
	   (score (assq article gnus-newsgroup-scored)))
      (if score (setcdr score n)
	(setq gnus-newsgroup-scored 
	      (cons (cons article n) gnus-newsgroup-scored))))
    (gnus-summary-update-line)))

(defun gnus-summary-current-score ()
  "Return the score of the current article."
  (interactive)
  (message "%s" (gnus-summary-article-score)))

;; Summary marking commands.

(defun gnus-summary-raise-same-subject-and-select (score)
  "Raise articles which has the same subject with SCORE and select the next."
  (interactive "p")
  (let ((subject (gnus-summary-subject-string)))
    (gnus-summary-raise-score score)
    (while (gnus-summary-search-subject nil nil subject)
      (gnus-summary-raise-score score))
    (gnus-summary-next-article t)))

(defun gnus-summary-raise-same-subject (score)
  "Raise articles which has the same subject with SCORE."
  (interactive "p")
  (let ((subject (gnus-summary-subject-string)))
    (gnus-summary-raise-score score)
    (while (gnus-summary-search-subject nil nil subject)
      (gnus-summary-raise-score score))
    (gnus-summary-next-subject 1 t)))

(defun gnus-score-default (level)
  (if level (prefix-numeric-value level) 
    gnus-score-interactive-default-score))

(defun gnus-summary-raise-thread (&optional score)
  "Raise the score of the articles in the current thread with SCORE."
  (interactive "P")
  (setq score (gnus-score-default score))
  (let (e)
    (save-excursion
      (let ((level (gnus-summary-thread-level)))
	(gnus-summary-raise-score score)
	(while (and (zerop (gnus-summary-next-subject 1 nil t))
		    (> (gnus-summary-thread-level) level))
	  (gnus-summary-raise-score score))
	(setq e (point))))
    (let ((gnus-summary-check-current t))
      (or (zerop (gnus-summary-next-subject 1 t))
	  (goto-char e))))
  (gnus-summary-recenter)
  (gnus-summary-position-cursor)
  (gnus-set-mode-line 'summary))

(defun gnus-summary-lower-same-subject-and-select (score)
  "Raise articles which has the same subject with SCORE and select the next."
  (interactive "p")
  (gnus-summary-raise-same-subject-and-select (- score)))

(defun gnus-summary-lower-same-subject (score)
  "Raise articles which has the same subject with SCORE."
  (interactive "p")
  (gnus-summary-raise-same-subject (- score)))

(defun gnus-summary-lower-thread (&optional score)
  "Lower score of articles in the current thread with SCORE."
  (interactive "P")
  (gnus-summary-raise-thread (- (1- (gnus-score-default score)))))

(defun gnus-summary-kill-same-subject-and-select (&optional unmark)
  "Mark articles which has the same subject as read, and then select the next.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-subject-string) unmark)))
    ;; Select next unread article. If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-summary-next-article t (and gnus-auto-select-same
				      (gnus-summary-subject-string)))
    (gnus-message 7 "%d article%s marked as %s"
		  count (if (= count 1) " is" "s are")
		  (if unmark "unread" "read"))))

(defun gnus-summary-kill-same-subject (&optional unmark)
  "Mark articles which has the same subject as read. 
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-subject-string) unmark)))
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
		(gnus-summary-search-forward nil subject))
	  (setq count (1+ count))))
       ((> unmark 0)			; Tick.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-ticked-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-search-forward nil subject))
	  (setq count (1+ count))))
       (t				; Mark as unread.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-unread-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-search-forward nil subject))
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
    (if (/= 0 n) (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-cursor)
    n))

(defun gnus-summary-unmark-as-processable (n)
  "Remove the process mark from the next N articles.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-as-processable n t))

(defun gnus-summary-unmark-all-processable ()
  "Remove the process mark from all articles."
  (interactive)
  (save-excursion
    (while gnus-newsgroup-processable
      (gnus-summary-remove-process-mark (car gnus-newsgroup-processable))))
  (gnus-summary-position-cursor))

(defun gnus-summary-mark-as-expirable (n)
  "Mark N articles forward as expirable.
If N is negative, mark backward instead. The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-expirable-mark))

(defun gnus-summary-mark-article-as-replied (article)
  "Mark ARTICLE replied and update the summary line."
  (setq gnus-newsgroup-replied (cons article gnus-newsgroup-replied))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-update-mark gnus-replied-mark 'replied)
	  t))))

(defun gnus-summary-set-bookmark (article)
  "Set a bookmark in current article."
  (interactive (list (gnus-summary-article-number)))
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
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-show-thread)
	  (and (eq (gnus-summary-article-mark) gnus-dummy-mark)
	       (forward-line 1))
	  (gnus-summary-update-mark gnus-process-mark 'replied)
	  t))))

(defun gnus-summary-remove-process-mark (article)
  "Remove the process mark from ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (delq article gnus-newsgroup-processable))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-show-thread)
	  (and (eq (gnus-summary-article-mark) gnus-dummy-mark)
	       (forward-line 1))
	  (gnus-summary-update-mark ?  'replied)
	  (if (memq article gnus-newsgroup-replied) 
	      (gnus-summary-update-mark gnus-replied-mark 'replied))
	  t))))

(defun gnus-summary-mark-forward (n &optional mark no-expire)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.
Mark with MARK. If MARK is ? , ?! or ??, articles will be
marked as unread. 
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(gnus-summary-goto-unread
	 (and gnus-summary-goto-unread
	      (not (memq mark (list gnus-unread-mark
				    gnus-ticked-mark gnus-dormant-mark)))))
	(n (abs n))
	(mark (or mark gnus-del-mark)))
    (while (and (> n 0)
		(gnus-summary-mark-article nil mark no-expire)
		(zerop (gnus-summary-next-subject 
			(if backward -1 1) gnus-summary-goto-unread t)))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
    (gnus-summary-recenter)
    (gnus-summary-position-cursor)
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
    (and gnus-newsgroup-auto-expire 
	 (or (= mark gnus-killed-mark) (= mark gnus-del-mark)
	     (= mark gnus-catchup-mark) (= mark gnus-low-score-mark)
	     (= mark gnus-read-mark))
	 (progn
	   (setq mark gnus-expirable-mark)
	   (setq gnus-newsgroup-expirable 
		 (cons article gnus-newsgroup-expirable))))
    (while (eq (gnus-summary-article-mark) gnus-dummy-mark)
      (forward-line 1))
    ;; Fix the mark.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article-as-unread (mark)
  "Mark the current article quickly as unread with MARK."
  (let ((article (gnus-summary-article-number)))
    (or (memq article gnus-newsgroup-unreads)
	(setq gnus-newsgroup-unreads (cons article gnus-newsgroup-unreads)))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
    (setq gnus-newsgroup-reads
	  (delq (assq article gnus-newsgroup-reads)
		gnus-newsgroup-reads))
    (if (= mark gnus-ticked-mark)
	(setq gnus-newsgroup-marked (cons article gnus-newsgroup-marked)))
    (if (= mark gnus-dormant-mark)
	(setq gnus-newsgroup-dormant (cons article gnus-newsgroup-dormant)))

    ;; See whether the article is to be put in the cache.
    (and gnus-use-cache
	 (vectorp (gnus-get-header-by-num article))
	 (save-excursion
	   (gnus-cache-possibly-enter-article 
	    gnus-newsgroup-name article 
	    (gnus-get-header-by-num article)
	    (= mark gnus-ticked-mark)
	    (= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

    (while (eq (gnus-summary-article-mark) gnus-dummy-mark)
      (forward-line 1))
    ;; Fix the mark.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article (&optional article mark no-expire)
  "Mark ARTICLE with MARK.  MARK can be any character.
Four MARK strings are reserved: `? ' (unread), `?!' (ticked), `??'
(dormant) and `?E' (expirable).
If MARK is nil, then the default character `?D' is used.
If ARTICLE is nil, then the article on the current line will be
marked." 
  (and (stringp mark)
       (setq mark (aref mark 0)))
  ;; If no mark is given, then we check auto-expiring.
  (and (not no-expire)
       gnus-newsgroup-auto-expire 
       (or (not mark)
	   (and (numberp mark) 
		(or (= mark gnus-killed-mark) (= mark gnus-del-mark)
		    (= mark gnus-catchup-mark) (= mark gnus-low-score-mark)
		    (= mark gnus-read-mark))))
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
	 (vectorp (gnus-get-header-by-num article))
	 (save-excursion
	   (gnus-cache-possibly-enter-article 
	    gnus-newsgroup-name article 
	    (gnus-get-header-by-num article)
	    (= mark gnus-ticked-mark)
	    (= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

    (if (gnus-summary-goto-subject article)
	(let ((buffer-read-only nil))
	  (gnus-summary-show-thread)
	  (and (eq (gnus-summary-article-mark) gnus-dummy-mark)
	       (forward-line 1))
	  ;; Fix the mark.
	  (gnus-summary-update-mark mark 'unread)
	  t))))

(defun gnus-summary-update-mark (mark type)
  (beginning-of-line)
  (let ((forward (cdr (assq type gnus-summary-mark-positions)))
	(buffer-read-only nil)
	plist)
    (if (not forward)
	()
      (forward-char forward)
      (setq plist (text-properties-at (point)))
      (delete-char 1)
      (insert mark)
      (and plist (add-text-properties (1- (point)) (point) plist))
      (and (eq type 'unread)
	   (progn
	     (add-text-properties (1- (point)) (point) (list 'gnus-mark mark))
	     (gnus-summary-update-line (eq mark gnus-unread-mark)))))))
  
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
    (setq gnus-newsgroup-reads 
	  (cons (cons article mark) gnus-newsgroup-reads))
    ;; Possibly remove from cache, if that is used. 
    (and gnus-use-cache (gnus-cache-enter-remove-article article))))

(defun gnus-mark-article-as-unread (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  (let ((mark (or mark gnus-ticked-mark)))
    ;; Add to unread list.
    (or (memq article gnus-newsgroup-unreads)
	(setq gnus-newsgroup-unreads (cons article gnus-newsgroup-unreads)))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
    (setq gnus-newsgroup-reads
	  (delq (assq article gnus-newsgroup-reads)
		gnus-newsgroup-reads))
    (if (= mark gnus-ticked-mark)
	(setq gnus-newsgroup-marked (cons article gnus-newsgroup-marked)))
    (if (= mark gnus-dormant-mark)
	(setq gnus-newsgroup-dormant (cons article gnus-newsgroup-dormant)))))

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
  (or (memq gnus-current-article gnus-newsgroup-marked)
      (memq gnus-current-article gnus-newsgroup-dormant)
      (memq gnus-current-article gnus-newsgroup-expirable)
      (gnus-summary-mark-article gnus-current-article gnus-read-mark)))

(defun gnus-summary-mark-region-as-read (point mark all)
  "Mark all unread articles between point and mark as read.
If given a prefix, mark all articles between point and mark as read,
even ticked and dormant ones."
  (interactive "r\nP")
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (while (and 
	    (< (point) mark)
	    (progn
	      (and
	       (or all
		   (and
		    (not (memq (gnus-summary-article-number)
			       gnus-newsgroup-marked))
		    (not (memq (gnus-summary-article-number)
			       gnus-newsgroup-dormant))))
	       (gnus-summary-mark-article
		(gnus-summary-article-number) gnus-del-mark))
	      t)
	    (zerop (forward-line 1))))))

;; Fix by Per Abrahamsen <amanda@iesd.auc.dk>.
(defalias 'gnus-summary-delete-marked-as-read 
  'gnus-summary-remove-lines-marked-as-read)
(make-obsolete 'gnus-summary-delete-marked-as-read 
	       'gnus-summary-remove-lines-marked-as-read)
(defun gnus-summary-remove-lines-marked-as-read ()
  "Remove lines that are marked as read."
  (interactive)
  (gnus-summary-remove-lines-marked-with 
   (concat (mapconcat
	    (lambda (char) (char-to-string (symbol-value char)))
	    '(gnus-del-mark gnus-read-mark gnus-ancient-mark
			    gnus-killed-mark gnus-kill-file-mark
			    gnus-low-score-mark gnus-expirable-mark
			    gnus-canceled-mark gnus-catchup-mark)
	    ""))))

(defalias 'gnus-summary-delete-marked-with 
  'gnus-summary-remove-lines-marked-with)
(make-obsolete 'gnus-summary-delete-marked-with 
	       'gnus-summary-remove-lines-marked-with)
;; Rewrite by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-remove-lines-marked-with (marks)
  "Remove lines that are marked with MARKS (e.g. \"DK\")."
  (interactive "sMarks: ")
  ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
  (gnus-set-global-variables)
  (let ((buffer-read-only nil)
	(orig-article 
	 (let ((gnus-summary-check-current t))
	   (gnus-summary-search-forward t)
	   (gnus-summary-article-number)))
	(marks (concat "^[" marks "]")))
    (goto-char (point-min))
    (if gnus-newsgroup-adaptive
	(gnus-score-remove-lines-adaptive marks)
      (while (re-search-forward marks nil t)
	(gnus-delete-line)))
    ;; If we use dummy roots, we have to do an additional sweep over
    ;; the buffer.
    (if (not (eq gnus-summary-make-false-root 'dummy))
	()
      (goto-char (point-min))
      (setq marks (concat "^[" (char-to-string gnus-dummy-mark) "]"))
      (while (re-search-forward marks nil t)
	(if (gnus-subject-equal
	     (gnus-summary-subject-string)
	     (progn
	       (forward-line 1)
	       (gnus-summary-subject-string)))
	    ()
	  (forward-line -1)
	  (gnus-delete-line))))
    (or (zerop (buffer-size))
	(gnus-summary-goto-subject orig-article)
	(if (eobp)
	    (gnus-summary-prev-subject 1)
	  (gnus-summary-position-cursor)))))

(defun gnus-summary-expunge-below (&optional score)
  "Remove articles with score less than SCORE."
  (interactive "P")
  (gnus-set-global-variables)
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (let ((buffer-read-only nil)
	  beg)
      (while (not (eobp))
	(if (< (gnus-summary-article-score) score)
	    (progn
	      (setq beg (point))
	      (forward-line 1)
	      (delete-region beg (point)))
	  (forward-line 1)))
      ;; Adjust point.
      (or (zerop (buffer-size))
	  (if (eobp)
	      (gnus-summary-prev-subject 1)
	    (gnus-summary-position-cursor))))))

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
    (while (not (eobp))
      (and (< (gnus-summary-article-score) score)
	   (gnus-summary-mark-article nil mark))
      (forward-line 1))))

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
    (while (not (eobp))
      (if (> (gnus-summary-article-score) score)
	  (progn
	    (gnus-summary-mark-article nil mark)
	    (forward-line 1))
	(forward-line 1)))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.  
(defun gnus-summary-show-all-expunged ()
  "Display all the hidden articles that were expunged for low scores."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil))
    (let ((scored gnus-newsgroup-scored)
	  headers h)
      (while scored
	(or (gnus-summary-goto-subject (car (car scored)))
	    (and (setq h (gnus-get-header-by-num (car (car scored))))
		 (< (cdr (car scored)) gnus-summary-expunge-below)
		 (setq headers (cons h headers))))
	(setq scored (cdr scored)))
      (or headers (error "No expunged articles hidden."))
      (goto-char (point-min))
      (save-excursion 
	(gnus-summary-update-lines 
	 (point)
	 (progn
	   (gnus-summary-prepare-unthreaded (nreverse headers))
	   (point)))))
    (goto-char (point-min))
    (gnus-summary-position-cursor)))

(defun gnus-summary-show-all-dormant ()
  "Display all the hidden articles that are marked as dormant."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil))
    (let ((dormant gnus-newsgroup-dormant)
	  headers h)
      (while dormant
	(or (gnus-summary-goto-subject (car dormant))
	    (and (setq h (gnus-get-header-by-num (car dormant)))
		 (setq headers (cons h headers))))
	(setq dormant (cdr dormant)))
      (or headers (error "No dormant articles hidden."))
      (goto-char (point-min))
      (save-excursion 
	(gnus-summary-update-lines 
	 (point)
	 (progn
	   (gnus-summary-prepare-unthreaded (nreverse headers))
	   (point)))))
    (goto-char (point-min))
    (gnus-summary-position-cursor)))

(defun gnus-summary-hide-all-dormant ()
  "Hide all dormant articles."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-remove-lines-marked-with (char-to-string gnus-dormant-mark))
  (gnus-summary-position-cursor))

(defun gnus-summary-catchup (&optional all quietly to-here not-mark)
  "Mark all articles not marked as unread in this newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read.
If QUIETLY is non-nil, no questions will be asked.
If TO-HERE is non-nil, it should be a point in the buffer. All
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
		(and all (setq gnus-newsgroup-marked nil
			       gnus-newsgroup-dormant nil))
		(setq gnus-newsgroup-unreads 
		      (append gnus-newsgroup-marked gnus-newsgroup-dormant)))
	    ;; We actually mark all articles as canceled, which we
	    ;; have to do when using auto-expiry or adaptive scoring. 
	    (gnus-summary-show-all-threads)
	    (if (gnus-summary-first-subject (not all))
		(while (and 
			(if to-here (< (point) to-here) t)
			(gnus-summary-mark-article-as-read gnus-catchup-mark)
			(gnus-summary-search-subject nil (not all)))))
	    (or to-here
		(setq gnus-newsgroup-unreads
		      (append gnus-newsgroup-marked
			      gnus-newsgroup-dormant)))))
    (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
      (if (and (not to-here) (eq 'nnvirtual (car method)))
	  (nnvirtual-catchup-group
	   (gnus-group-real-name gnus-newsgroup-name) (nth 1 method) all)))
    (gnus-summary-position-cursor)))

(defun gnus-summary-catchup-to-here (&optional all)
  "Mark all unticked articles before the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive "P")
  (gnus-set-global-variables)
  (save-excursion
    (and (zerop (forward-line -1))
	 (progn
	   (end-of-line)
	   (gnus-summary-catchup all t (point))
	   (gnus-set-mode-line 'summary))))
  (gnus-summary-position-cursor))

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
  (if (and (eq gnus-auto-select-next 'quietly)
	   (not (gnus-ephemeral-group-p gnus-newsgroup-name)))
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
  (gnus-summary-catchup all)
  (gnus-summary-next-group))

;; Thread-based commands.

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
    (gnus-summary-position-cursor)))

(defun gnus-summary-show-all-threads ()
  "Show all threads."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (let ((buffer-read-only nil))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)))
  (gnus-summary-position-cursor))

(defun gnus-summary-show-thread ()
  "Show thread subtrees.
Returns nil if no thread was there to be shown."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil)
	(orig (prog1 (point) (gnus-summary-hide-thread)))
	;; first goto end then to beg, to have point at beg after let
	(end (progn (end-of-line) (point)))
	(beg (progn (beginning-of-line) (point))))
    (prog1
	;; Any hidden lines here?
	(search-forward "\r" end t)
      (subst-char-in-region beg end ?\^M ?\n t)
      (goto-char orig)
      (gnus-summary-position-cursor))))

(defun gnus-summary-hide-all-threads ()
  "Hide all thread subtrees."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (goto-char (point-min))
    (gnus-summary-hide-thread)
    (while (and (not (eobp)) (zerop (forward-line 1)))
      (gnus-summary-hide-thread)))
  (gnus-summary-position-cursor))

(defun gnus-summary-hide-thread ()
  "Hide thread subtrees.
Returns nil if no threads were there to be hidden."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil)
	(start (point))
	(level (gnus-summary-thread-level))
	(end (point)))
    ;; Go forward until either the buffer ends or the subthread
    ;; ends. 
    (if (eobp)
	()
      (while (and (zerop (forward-line 1))
		  (> (gnus-summary-thread-level) level))
	(setq end (point)))
      (prog1
	  (save-excursion
	    (goto-char end)
	    (search-backward "\n" start t))
	(subst-char-in-region start end ?\n ?\^M t)
	(forward-line -1)
	(gnus-summary-position-cursor)))))

(defun gnus-summary-go-to-next-thread (&optional previous)
  "Go to the same level (or less) next thread.
If PREVIOUS is non-nil, go to previous thread instead.
Return the article number moved to, or nil if moving was impossible."
  (let ((level (gnus-summary-thread-level))
	(article (gnus-summary-article-number)))
    (if previous 
	(while (and (zerop (forward-line -1))
		    (> (gnus-summary-thread-level) level)))
      (while (and (save-excursion
		    (forward-line 1)
		    (not (eobp)))
		  (zerop (forward-line 1))
		  (> (gnus-summary-thread-level) level))))
    (gnus-summary-recenter)
    (gnus-summary-position-cursor)
    (let ((oart (gnus-summary-article-number)))
      (and (/= oart article) oart))))

(defun gnus-summary-next-thread (n)
  "Go to the same level next N'th thread.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-summary-go-to-next-thread backward))
      (setq n (1- n)))
    (gnus-summary-position-cursor)
    (if (/= 0 n) (gnus-message 7 "No more threads"))
    n))

(defun gnus-summary-prev-thread (n)
  "Go to the same level previous N'th thread.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-next-thread (- n)))

(defun gnus-summary-go-down-thread (&optional same)
  "Go down one level in the current thread.
If SAME is non-nil, also move to articles of the same level."
  (let ((level (gnus-summary-thread-level))
	(start (point)))
    (if (and (zerop (forward-line 1))
	     (> (gnus-summary-thread-level) level))
	t
      (goto-char start)
      nil)))

(defun gnus-summary-go-up-thread ()
  "Go up one level in the current thread."
  (let ((level (gnus-summary-thread-level))
	(start (point)))
    (while (and (zerop (forward-line -1))
		(>= (gnus-summary-thread-level) level)))
    (if (>= (gnus-summary-thread-level) level)
	(progn
	  (goto-char start)
	  nil)
      t)))

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
    (gnus-summary-position-cursor)
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

(defun gnus-summary-kill-thread (&optional unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((killing t)
	(level (gnus-summary-thread-level)))
    (save-excursion
      ;; Expand the thread.
      (gnus-summary-show-thread)
      (while killing
	;; Mark the article...
	(cond ((null unmark) (gnus-summary-mark-article-as-read
			      gnus-killed-mark))
	      ((> unmark 0) (gnus-summary-mark-article-as-unread 
			     gnus-unread-mark))
	      (t (gnus-summary-mark-article-as-unread gnus-ticked-mark)))
	;; ...and go forward until either the buffer ends or the subtree
	;; ends. 
	(if (not (and (zerop (forward-line 1))
		      (> (gnus-summary-thread-level) level)))
	    (setq killing nil))))
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
  (gnus-set-global-variables)
  (gnus-summary-sort 
   ;; `gnus-summary-article-number' is a macro, and `sort-subr' wants
   ;; a function, so we wrap it.
   (cons (lambda () (gnus-summary-article-number))
	 'gnus-thread-sort-by-number) reverse))

(defun gnus-summary-sort-by-author (&optional reverse)
  "Sort summary buffer by author name alphabetically.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort
   (cons
    (lambda ()
      (let* ((header (gnus-get-header-by-num (gnus-summary-article-number)))
	     extract)
	(if (not (vectorp header))
	    ""
	  (setq extract (funcall gnus-extract-address-components
				 (mail-header-from header)))
	  (concat (or (car extract) (cdr extract))
		  "\r" (int-to-string (mail-header-number header))
		  "\r" (mail-header-subject header)))))
    'gnus-thread-sort-by-author)
   reverse))

(defun gnus-summary-sort-by-subject (&optional reverse)
  "Sort summary buffer by subject alphabetically. `Re:'s are ignored.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort
   (cons
    (lambda ()
      (let* ((header (gnus-get-header-by-num (gnus-summary-article-number)))
	     extract)
	(if (not (vectorp header))
	    ""
	  (setq extract (funcall gnus-extract-address-components
				 (mail-header-from header)))
	  (concat 
	   (downcase (gnus-simplify-subject (gnus-summary-subject-string) t))
	   "\r" (int-to-string (mail-header-number header))
	   "\r" (or (car extract) (cdr extract))))))
    'gnus-thread-sort-by-subject)
   reverse))

(defun gnus-summary-sort-by-date (&optional reverse)
  "Sort summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort
   (cons
    (lambda ()
      (gnus-sortable-date
       (mail-header-date 
	(gnus-get-header-by-num (gnus-summary-article-number)))))
    'gnus-thread-sort-by-date)
   reverse))

(defun gnus-summary-sort-by-score (&optional reverse)
  "Sort summary buffer by score.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort 
   (cons (lambda () (gnus-summary-article-score))
	 'gnus-thread-sort-by-score)
   (not reverse)))

(defvar gnus-summary-already-sorted nil)
(defun gnus-summary-sort (predicate reverse)
  ;; Sort summary buffer by PREDICATE.  REVERSE means reverse order. 
  (if gnus-summary-already-sorted
      ()
    (let (buffer-read-only)
      (if (not gnus-show-threads)
	  ;; We do untreaded sorting...
	  (progn
	    (goto-char (point-min))
	    (sort-subr reverse 'forward-line 'end-of-line (car predicate)))
	;; ... or we do threaded sorting.
	(let ((gnus-thread-sort-functions (list (cdr predicate)))
	      (gnus-summary-prepare-hook nil)
	      (gnus-summary-already-sorted nil))
	  ;; We do that by simply regenerating the threads.
	  (gnus-summary-prepare)
	  (and gnus-show-threads
	       gnus-thread-hide-subtree
	       (gnus-summary-hide-all-threads))
	  ;; If in async mode, we send some info to the backend.
	  (and gnus-newsgroup-async
	       (setq gnus-newsgroup-threads (nreverse gnus-newsgroup-threads))
	       (gnus-request-asynchronous 
		gnus-newsgroup-name
		(if (and gnus-asynchronous-article-function
			 (fboundp gnus-asynchronous-article-function))
		    (funcall gnus-asynchronous-article-function
			     gnus-newsgroup-threads)
		  gnus-newsgroup-threads))))))))

  
(defun gnus-sortable-date (date)
  "Make sortable string by string-lessp from DATE.
Timezone package is used."
  (let* ((date (timezone-fix-time date nil nil)) ;[Y M D H M S]
	 (year (aref date 0))
	 (month (aref date 1))
	 (day (aref date 2)))
    (timezone-make-sortable-date 
     year month day 
     (timezone-make-time-string
      (aref date 3) (aref date 4) (aref date 5)))))


;; Summary saving commands.

(defun gnus-summary-save-article (&optional n)
  "Save the current article using the default saver function.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead.
The variable `gnus-default-article-saver' specifies the saver function."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n)))
    (while articles
      (let ((header (gnus-get-header-by-num (car articles))))
	(if (vectorp header)
	    (progn
	      (save-window-excursion
		(gnus-summary-select-article t nil nil (car articles)))
	      (or gnus-save-all-headers
		  (gnus-article-hide-headers t))
	      ;; Remove any X-Gnus lines.
	      (save-excursion
		(save-restriction
		  (set-buffer gnus-article-buffer)
		  (let ((buffer-read-only nil))
		    (goto-char (point-min))
		    (narrow-to-region (point) (or (search-forward "\n\n" nil t)
						  (point-max)))
		    (while (re-search-forward "^X-Gnus" nil t)
		      (beginning-of-line)
		      (delete-region (point)
				     (progn (forward-line 1) (point))))
		    (widen))))
	      (save-window-excursion
		(if gnus-default-article-saver
		    (funcall gnus-default-article-saver)
		  (error "No default saver is defined."))))
	  (if (assq 'name header)
	      (gnus-copy-file (cdr (assq 'name header)))
	    (gnus-message 1 "Article %d is unsavable" (car articles)))))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))
    (gnus-summary-position-cursor)
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
    (gnus-summary-save-article arg))
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

(defun gnus-read-save-file-name (prompt default-name)
  (let ((methods gnus-split-methods)
	split-name)
    (if (not gnus-split-methods)
	()
      (save-excursion
	(set-buffer gnus-article-buffer)
	(gnus-narrow-to-headers)
	(while methods
	  (goto-char (point-min))
	  (and (condition-case () 
		   (re-search-forward (car (car methods)) nil t)
		 (error nil))
	       (setq split-name (cons (nth 1 (car methods)) split-name)))
	  (setq methods (cdr methods)))
	(widen)))
    (cond ((null split-name)
	   (read-file-name
	    (concat prompt " (default "
		    (file-name-nondirectory default-name) ") ")
	    (file-name-directory default-name)
	    default-name))
	  ((= 1 (length split-name))
	   (read-file-name
	    (concat prompt " (default " (car split-name) ") ")
	    gnus-article-save-directory
	    (concat gnus-article-save-directory (car split-name))))
	  (t
	   (setq split-name (mapcar (lambda (el) (list el))
				    (nreverse split-name)))
	   (let ((result (completing-read 
			  (concat prompt " ")
			  split-name nil nil)))
	     (concat gnus-article-save-directory
		     (if (string= result "")
			 (car (car split-name))
		       result)))))))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-rmail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-rmail)))
    (or filename
	(setq filename (gnus-read-save-file-name 
			"Save in rmail file:" default-name)))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (gnus-output-to-rmail filename))))
    ;; Remember the directory name to save articles
    (setq gnus-newsgroup-last-rmail filename)))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-mail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-mail)))
    (or filename
	(setq filename (gnus-read-save-file-name 
			"Save in Unix mail file:" default-name)))
    (setq filename
	  (expand-file-name filename
			    (and default-name
				 (file-name-directory default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (if (and (file-readable-p filename) (mail-file-babyl-p filename))
	     (gnus-output-to-rmail filename)
	   (rmail-output filename 1 t t)))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-mail filename)))

(defun gnus-summary-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (or filename
	(setq filename (gnus-read-save-file-name 
			"Save in file:" default-name)))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-in-pipe (&optional command)
  "Pipe this article to subprocess."
  (interactive)
  (gnus-set-global-variables)
  (let ((command (read-string "Shell command on article: "
			      gnus-last-shell-command)))
    (if (string-equal command "")
	(setq command gnus-last-shell-command))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       (shell-command-on-region (point-min) (point-max) command nil)))
    (setq gnus-last-shell-command command)))

;; Summary extract commands

(defun gnus-summary-insert-pseudos (pslist &optional not-view)
  (let ((buffer-read-only nil)
	(article (gnus-summary-article-number))
	b)
    (or (gnus-summary-goto-subject article)
	(error (format "No such article: %d" article)))
    (gnus-summary-position-cursor)
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
			       (or (cdr (assq 'action (car (cdr ps)))) "2")))
	    (setq files (cons (cdr (assq 'name (car (cdr ps)))) files))
	    (setcdr ps (cdr (cdr ps))))
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
	  (gnus-summary-goto-subject (or (cdr (assq 'article (car pslist)))
					 (gnus-summary-article-number)))
	  (forward-line 1)
	  (setq b (point))
	  (insert "          " (file-name-nondirectory 
				(cdr (assq 'name (car pslist))))
		  ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	  (add-text-properties 
	   b (1+ b) (list 'gnus-number gnus-reffed-article-number
			  'gnus-mark gnus-unread-mark 
			  'gnus-level 0
			  'gnus-pseudo (car pslist)))
	  (forward-line -1)
	  (gnus-sethash (int-to-string gnus-reffed-article-number)
			(car pslist) gnus-newsgroup-headers-hashtb-by-number)
	  (setq gnus-newsgroup-unreads
		(cons gnus-reffed-article-number gnus-newsgroup-unreads))
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
    (let ((command (if automatic command (read-string "Command: " command)))
	  (buffer-read-only nil))
      (erase-buffer)
      (insert "$ " command "\n\n")
      (if gnus-view-pseudo-asynchronously
	  (start-process "gnus-execute" nil "sh" "-c" command)
	(call-process "sh" nil t nil "-c" command)))))

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
  (setq gnus-current-headers 
	(gnus-gethash 
	 (int-to-string (gnus-summary-article-number))
	 gnus-newsgroup-headers-hashtb-by-number))
  (gnus-set-global-variables)
  (gnus-group-edit-local-kill 
   (gnus-summary-article-number) gnus-newsgroup-name))


;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(defvar gnus-bugaboo nil)

(if gnus-article-mode-map
    nil
  (setq gnus-article-mode-map (make-keymap))
  (suppress-keymap gnus-article-mode-map)
  (define-key gnus-article-mode-map " " 'gnus-article-next-page)
  (define-key gnus-article-mode-map "\177" 'gnus-article-prev-page)
  (define-key gnus-article-mode-map "\C-c^" 'gnus-article-refer-article)
  (define-key gnus-article-mode-map "h" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "s" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "\C-c\C-m" 'gnus-article-mail)
  (define-key gnus-article-mode-map "?" 'gnus-article-describe-briefly)
  (define-key gnus-article-mode-map gnus-mouse-2 'gnus-article-push-button)
  (define-key gnus-article-mode-map "\r" 'gnus-article-press-button)
  (define-key gnus-article-mode-map "\t" 'gnus-article-next-button)
  (define-key gnus-article-mode-map "\C-c\C-b" 'gnus-bug)
  
  ;; Duplicate almost all summary keystrokes in the article mode map.
  (let ((commands 
	 (list 
	  "p" "N" "P" "\M-\C-n" "\M-\C-p"
	  "\M-n" "\M-p" "." "," "\M-s" "\M-r" "<" ">" "j"
	  "u" "!" "U" "d" "D" "E" "\M-u" "\M-U" "k" "\C-k" "\M-\C-k"
	  "\M-\C-l" "e" "#" "\M-#" "\M-\C-t" "\M-\C-s" "\M-\C-h"
	  "\M-\C-f" "\M-\C-b" "\M-\C-u" "\M-\C-d" "&" "\C-w"
	  "\C-t" "?" "\C-c\M-\C-s" "\C-c\C-s\C-n" "\C-c\C-s\C-a"
	  "\C-c\C-s\C-s" "\C-c\C-s\C-d" "\C-c\C-s\C-i" "\C-x\C-s"
	  "\M-g" "w" "\C-c\C-r" "\M-t" "C"
	  "o" "\C-o" "|" "\M-k" "\M-K" "V" "\C-c\C-d"
	  "\C-c\C-i" "x" "X" "t" "g" "?" "l"
	  "\C-c\C-v\C-v" "\C-d" "v" 
;;	  "Mt" "M!" "Md" "Mr"
;;	  "Mc" "M " "Me" "Mx" "M?" "Mb" "MB" "M#" "M\M-#" "M\M-r"
;;	  "M\M-\C-r" "MD" "M\M-D" "MS" "MC" "MH" "M\C-c" "Mk" "MK"
;;	  "Ms" "Mc" "Mu" "Mm" "Mk" "Gn" "Gp" "GN" "GP" "G\C-n" "G\C-p"
;;	  "G\M-n" "G\M-p" "Gf" "Gb" "Gg" "Gl" "Gp" "Tk" "Tl" "Ti" "TT"
;;	  "Ts" "TS" "Th" "TH" "Tn" "Tp" "Tu" "Td" "T#" "A " "An" "A\177" "Ap"
;;	  "A\r" "A<" "A>" "Ab" "Ae" "A^" "Ar" "Aw" "Ac" "Ag" "At" "Am"
;;	  "As" "Wh" "Ws" "Wc" "Wo" "Ww" "Wd" "Wq" "Wf" "Wt" "W\C-t"
;;	  "WT" "WA" "Wa" "WH" "WC" "WS" "Wb" "Hv" "Hf" "Hd" "Hh" "Hi"
;;	  "Be" "B\177" "Bm" "Br" "Bw" "Bc" "Bq" "Bi" "Oo" "Om" "Or"
;;	  "Of" "Oh" "Ov" "Op" "Vu" "V\C-s" "V\C-r" "Vr" "V&" "VT" "Ve"
;;	  "VD" "Vk" "VK" "Vsn" "Vsa" "Vss" "Vsd" "Vsi"
	  )))
    (while (and gnus-bugaboo commands) ; disabled
      (define-key gnus-article-mode-map (car commands) 
	'gnus-article-summary-command)
      (setq commands (cdr commands))))

  (let ((commands (list "q" "Q"  "c" "r" "R" "\C-c\C-f" "m"  "a" "f" "F"
;;			"Zc" "ZC" "ZE" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP" 
			 "=" "n"  "^" "\M-^")))
    (while (and gnus-bugaboo commands) ; disabled
      (define-key gnus-article-mode-map (car commands) 
	'gnus-article-summary-command-nosave)
      (setq commands (cdr commands)))))


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
  (if gnus-visual (gnus-article-make-menu-bar))
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
  "Initialize article mode buffer."
  ;; Returns the article buffer.
  (if (get-buffer gnus-article-buffer)
      (save-excursion
	(set-buffer gnus-article-buffer)
	(buffer-disable-undo (current-buffer))
	(setq buffer-read-only t)
	(gnus-add-current-to-buffer-list)
	(or (eq major-mode 'gnus-article-mode)
	    (gnus-article-mode))
	(current-buffer))
    (save-excursion
      (set-buffer (get-buffer-create gnus-article-buffer))
      (gnus-add-current-to-buffer-list)
      (gnus-article-mode)
      (current-buffer))))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (set-window-start 
   (get-buffer-window gnus-article-buffer)
   (save-excursion
     (set-buffer gnus-article-buffer)
     (goto-char (point-min))
     (if (not line)
	 (point-min)
       (gnus-message 6 "Moved to bookmark")
       (search-forward "\n\n" nil t)
       (forward-line line)
       (point)))))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer."
  (setq group (or group gnus-newsgroup-name))

  ;; Open server if it has closed.
  (gnus-check-server (gnus-find-method-for-group group))

  ;; Using `gnus-request-article' directly will insert the article into
  ;; `nntp-server-buffer' - so we'll save some time by not having to
  ;; copy it from the server buffer into the article buffer.

  ;; We only request an article by message-id when we do not have the
  ;; headers for it, so we'll have to get those.
  (and (stringp article) 
       (let ((gnus-override-method gnus-refer-article-method))
	 (gnus-read-header article)))

  ;; If the article number is negative, that means that this article
  ;; doesn't belong in this newsgroup (possibly), so we find its
  ;; message-id and request it by id instead of number.
  (if (not (numberp article))
      ()
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (let ((header (gnus-get-header-by-num article)))
	(if (< article 0)
	    (if (vectorp header)
		;; It's a real article.
		(setq article (mail-header-id header))
	      ;; It is an extracted pseudo-article.
	      (setq article 'pseudo)
	      (gnus-request-pseudo-article header)))

	(let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
	  (if (not (eq (car method) 'nneething))
	      ()
	    (let ((dir (concat (file-name-as-directory (nth 1 method))
			       (mail-header-subject header))))
	      (if (file-directory-p dir)
		  (progn
		    (setq article 'nneething)
		    (gnus-group-enter-directory dir)))))))))

  ;; Check the cache.
  (if (and gnus-use-cache
	   (numberp article)
	   (gnus-cache-request-article article group))
      'article
    ;; Get the article and put into the article buffer.
    (if (or (stringp article) (numberp article))
	(progn
	  (erase-buffer)
	  ;; There may be some overlays that we have to kill...
	  (insert "i")
	  (let ((overlays (and (fboundp 'overlays-at)
			       (overlays-at (point-min)))))
	    (while overlays
	      (delete-overlay (car overlays))
	      (setq overlays (cdr overlays))))
	  (erase-buffer)	  
	  (let ((gnus-override-method 
		 (and (stringp article) gnus-refer-article-method)))
	    (and (gnus-request-article article group (current-buffer))
		 'article)))
      article)))

(defun gnus-read-header (id)
  "Read the headers of article ID and enter them into the Gnus system."
  (let (header)
    (if (not (setq header 
		   (car (if (let ((gnus-nov-is-evil t))
			      (gnus-retrieve-headers 
			       (list id) gnus-newsgroup-name))
			    (gnus-get-newsgroup-headers)))))
	nil
      (if (stringp id)
	  (mail-header-set-number header gnus-reffed-article-number))
      (setq gnus-newsgroup-headers (cons header gnus-newsgroup-headers))
      (gnus-sethash (int-to-string (mail-header-number header)) header
		    gnus-newsgroup-headers-hashtb-by-number)
      (if (stringp id)
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number)))
      (setq gnus-current-headers header)
      header)))

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (or (eq major-mode 'gnus-summary-mode)
	(set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    ;; Make sure the connection to the server is alive.
    (or (gnus-server-opened (gnus-find-method-for-group gnus-newsgroup-name))
	(progn
	  (gnus-check-server 
	   (gnus-find-method-for-group gnus-newsgroup-name))
	  (gnus-request-group gnus-newsgroup-name t)))
    (let* ((article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	(if (not (setq result (let ((buffer-read-only nil))
				(gnus-request-article-this-buffer 
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (if (not (numberp article))
		  ()
		(setq gnus-article-current 
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(gnus-summary-mark-article article gnus-canceled-mark))
	      (gnus-message 1 "No such article (may be canceled)")
	      (ding)
	      nil)
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
			(gnus-get-header-by-num gnus-current-article)
			gnus-article-current 
			(cons gnus-newsgroup-name gnus-current-article))
		  (gnus-summary-show-thread)
		  (run-hooks 'gnus-mark-article-hook)
		  (gnus-set-mode-line 'summary)
		  (and gnus-visual 
		       (run-hooks 'gnus-visual-mark-article-hook))
		  ;; Set the global newsgroup variables here.
		  ;; Suggested by Jim Sisolak
		  ;; <sisolak@trans4.neep.wisc.edu>.
		  (gnus-set-global-variables)
		  (setq gnus-have-all-headers 
			(or all-headers gnus-show-all-headers))
		  (and gnus-use-cache 
		       (vectorp (gnus-get-header-by-number article))
		       (gnus-cache-possibly-enter-article
			group article
			(gnus-get-header-by-number article)
			(memq article gnus-newsgroup-marked)
			(memq article gnus-newsgroup-dormant)
			(memq article gnus-newsgroup-unreads)))))
	    ;; Hooks for getting information from the article.
	    ;; This hook must be called before being narrowed.
	    (let (buffer-read-only)
	      (run-hooks 'internal-hook)
	      (run-hooks 'gnus-article-prepare-hook)
	      ;; Decode MIME message.
	      (if (and gnus-show-mime
		       (or (not gnus-strict-mime)
			   (gnus-fetch-field "Mime-Version")))
		  (funcall gnus-show-mime-method))
	      ;; Perform the article display hooks.
	      (run-hooks 'gnus-article-display-hook))
	    ;; Do page break.
	    (goto-char (point-min))
	    (and gnus-break-pages (gnus-narrow-to-page))
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
      (remove-text-properties (point-min) (point-max) 
			      gnus-hidden-properties))))

(defun gnus-article-hide-headers-if-wanted ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatibility."
  (or (save-excursion (set-buffer gnus-summary-buffer) gnus-have-all-headers)
      (gnus-article-hide-headers)))

(defun gnus-article-hide-headers (&optional delete)
  "Hide unwanted headers and possibly sort them as well."
  (interactive "P")
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((sorted gnus-sorted-header-list)
	    (buffer-read-only nil)
	    want-list beg want-l)
	;; First we narrow to just the headers.
	(widen)
	(goto-char (point-min))
	;; Hide any "From " lines at the beginning of (mail) articles. 
	(while (looking-at "From ")
	  (forward-line 1))
	(or (bobp) 
	    (add-text-properties (point-min) (point) gnus-hidden-properties))
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
	  (if (or (and (stringp gnus-visible-headers)
		       (looking-at gnus-visible-headers))
		  (and (not (stringp gnus-visible-headers))
		       (stringp gnus-ignored-headers)
		       (not (looking-at gnus-ignored-headers))))
	      (progn
		(setq beg (point))
		(forward-line 1)
		;; Be sure to get multi-line headers...
		(re-search-forward "^[^ \t]*:" nil t)
		(beginning-of-line)
		(setq want-list 
		      (cons (buffer-substring beg (point)) want-list))
		(delete-region beg (point))
		(goto-char beg))
	    (forward-line 1)))
	;; Next we perform the sorting by looking at
	;; `gnus-sorted-header-list'. 
	(goto-char (point-min))
	(while (and sorted want-list)
	  (setq want-l want-list)
	  (while (and want-l
		      (not (string-match (car sorted) (car want-l))))
	    (setq want-l (cdr want-l)))
	  (if want-l 
	      (progn
		(insert (car want-l))
		(setq want-list (delq (car want-l) want-list))))
	  (setq sorted (cdr sorted)))
	;; Any headers that were not matched by the sorted list we
	;; just tack on the end of the visible header list.
	(while want-list
	  (insert (car want-list))
	  (setq want-list (cdr want-list)))
	;; And finally we make the unwanted headers invisible.
	(if delete
	    (delete-region (point) (point-max))
	  ;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>.
	  (add-text-properties (point) (point-max) gnus-hidden-properties))))))

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
	  (cond ((eq next previous)
		 (put-text-property (- (point) 2) (point)
				    'invisible t)
		 (put-text-property (point) (1+ (point))
				    'face 'bold))
		((eq next ?_)
		 (put-text-property (1- (point)) (1+ (point))
				    'invisible t)
		 (put-text-property (1- (point)) (point)
				    'face 'underline))
		((eq previous ?_)
		 (put-text-property (- (point) 2) (point)
				    'invisible t)
		 (put-text-property (point) (1+ (point))
				    'face 'underline))))))))

(defun gnus-article-word-wrap ()
  "Format too long lines."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
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

(defun gnus-article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((inhibit-point-motion-hooks t)
	  (case-fold-search nil)
	  from)
      (save-restriction
	(goto-char (point-min))
	(search-forward "\n\n")
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(setq from (mail-fetch-field "from"))
	(if (not (and gnus-article-x-face-command
		      (or force
			  (not gnus-article-x-face-too-ugly)
			  (and gnus-article-x-face-too-ugly from
			       (not (string-match gnus-article-x-face-too-ugly
						  from))))
		      (progn
			(goto-char (point-min))
			(re-search-forward "^X-Face: " nil t))))
	    nil
	  (let ((beg (point))
		(end (1- (re-search-forward "^\\($\\|[^ \t]\\)" nil t))))
	    (if (symbolp gnus-article-x-face-command)
		(and (or (fboundp gnus-article-x-face-command)
			 (error "%s is not a function"
				gnus-article-x-face-command))
		     (funcall gnus-article-x-face-command beg end))
	      (call-process-region beg end "sh" nil 0 nil
				   "-c" gnus-article-x-face-command))))))))

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
      (if (or force (and type (string-match "quoted-printable" type)))
	  (progn
	    (goto-char (point-min))
	    (search-forward "\n\n" nil 'move)
	    (gnus-mime-decode-quoted-printable (point) (point-max)))))))

(defun gnus-mime-decode-quoted-printable (from to)
  ;; Decode quoted-printable from region between FROM and TO.
  (save-excursion
    (goto-char from)
    (while (search-forward "=" to t)
      (cond ((eq (following-char) ?\n)
	     (delete-char -1)
	     (delete-char 1))
	    ((looking-at "[0-9A-F][0-9A-F]")
	     (delete-char -1)
	     (insert (hexl-hex-string-to-integer
		      (buffer-substring (point) (+ 2 (point)))))
	     (delete-char 2))
	    ((looking-at "=")
	     (delete-char 1))
	    ((gnus-message 3 "Malformed MIME quoted-printable message"))))))

(defvar gnus-article-time-units
  (list (cons 'year (* 365.25 24 60 60))
	(cons 'week (* 7 24 60 60))
	(cons 'day (* 24 60 60))
	(cons 'hour (* 60 60))
	(cons 'minute 60)
	(cons 'second 1)))

(defun gnus-article-date-ut (&optional type)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE."
  (interactive (list 'ut))
  (let ((date (mail-header-date (or gnus-current-headers 
				    (gnus-get-header-by-number
				     (gnus-summary-article-number))"")))
	(date-regexp "^Date: \\|^X-Sent: "))
    (if (or (not date)
	    (string= date ""))
	()
      (save-excursion
	(set-buffer gnus-article-buffer)
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (if (and (re-search-forward date-regexp nil t)
		   (progn 
		     (beginning-of-line)
		     (looking-at date-regexp)))
	      (delete-region (gnus-point-at-bol)
			     (progn (end-of-line) (1+ (point))))
	    (goto-char (point-min))
	    (goto-char (- (search-forward "\n\n") 2)))
	  (insert
	   (cond 
	    ((eq type 'local)
	     (concat "Date: " (condition-case ()
				  (timezone-make-date-arpa-standard date)
				(error date))
		     "\n"))
	    ((eq type 'ut)
	     (concat "Date: "
		     (condition-case ()
			 (timezone-make-date-arpa-standard date nil "UT")
		       (error date))
		     "\n"))
	    ((eq type 'lapsed)
	     ;; If the date is seriously mangled, the timezone
	     ;; functions are liable to bug out, so we condition-case
	     ;; the entire thing.  
	     (let* ((real-sec (condition-case ()
				  (- (gnus-seconds-since-epoch 
				      (timezone-make-date-arpa-standard
				       (current-time-string) 
				       (current-time-zone) "UT"))
				     (gnus-seconds-since-epoch 
				      (timezone-make-date-arpa-standard 
				       date nil "UT")))
				(error 0)))
		    (sec (abs real-sec))
		    num prev)
	       (if (zerop sec)
		   "X-Sent: Now\n"
		 (concat
		  "X-Sent: "
		  (mapconcat 
		   (lambda (unit)
		     (if (zerop (setq num (ffloor (/ sec (cdr unit)))))
			 ""
		       (setq sec (- sec (* num (cdr unit))))
		       (prog1
			   (concat (if prev ", " "") (int-to-string 
						      (floor num))
				   " " (symbol-name (car unit))
				   (if (> num 1) "s" ""))
			 (setq prev t))))
		   gnus-article-time-units "")
		  (if (> real-sec 0)
		      " ago\n"
		    " in the future\n")))))
	    (t
	     (error "Unknown conversion type: %s" type)))))))))

(defun gnus-article-date-local ()
  "Convert the current article date to the local timezone."
  (interactive)
  (gnus-article-date-ut 'local))

(defun gnus-article-date-lapsed ()
  "Convert the current article date to time lapsed since it was sent."
  (interactive)
  (gnus-article-date-ut 'lapsed))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if `gnus-visual' is non-nil."
  (if gnus-visual (gnus-article-highlight-some)))

;; Article savers.

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
  (setq file-name (expand-file-name file-name))
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-output*")))
    (save-excursion
      (set-buffer tmpbuf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (append-to-file (point-min) (point-max) file-name))
    (kill-buffer tmpbuf)))

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
  "Make text outside current page invisible except for page delimiter.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (forward-page -1)			;Beginning of current page.
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (forward-page (1- arg))))
    ;; Find the end of the page.
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    ;; These are commented out.
    ;;    (if (save-excursion (beginning-of-line)
    ;;			(looking-at page-delimiter))
    ;;	(beginning-of-line))
    (narrow-to-region (point)
		      (progn
			;; Find the top of the page.
			(forward-page -1)
			;; If we found beginning of buffer, stay there.
			;; If extra text follows page delimiter on same line,
			;; include it.
			;; Otherwise, show text starting with following line.
			(if (and (eolp) (not (bobp)))
			    (forward-line 1))
			(point)))))

(defun gnus-gmt-to-local ()
  "Rewrite Date header described in GMT to local in current buffer.
Intended to be used with gnus-article-prepare-hook."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (goto-char (point-min))
      (if (re-search-forward "^Date:[ \t]\\(.*\\)$" nil t)
	  (let ((buffer-read-only nil)
		(date (buffer-substring-no-properties
		       (match-beginning 1) (match-end 1))))
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert
	     (timezone-make-date-arpa-standard 
	      date nil (current-time-zone))))))))


;; Article mode commands

(defun gnus-article-next-page (&optional lines)
  "Show next page of current article.
If end of article, return non-nil. Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "P")
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
    nil))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1)	;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (scroll-down lines)))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (search-forward ">" nil t)		;Move point to end of "<....>".
  (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
      (let ((message-id
	     (buffer-substring (match-beginning 1) (match-end 1))))
	(set-buffer gnus-summary-buffer)
	(gnus-summary-refer-article message-id))
    (error "No references around point")))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-goto-subject gnus-current-article))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-next-page]:Next page  \\[gnus-article-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

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


;; Basic ideas by emv@math.lsa.umich.edu (Edward Vielmetti)

;;;###autoload
(defalias 'gnus-batch-kill 'gnus-batch-score)
;;;###autoload
(defun gnus-batch-score ()
  "Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-score <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\". If you would not like to
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
      (setq group (car (car newsrc)))
      (setq entry (gnus-gethash group gnus-newsrc-hashtb))
      (if (and (<= (nth 1 (car newsrc)) gnus-level-subscribed)
	       (and (car entry)
		    (or (eq (car entry) t)
			(not (zerop (car entry)))))
	       (if yes (string-match yes group) t)
	       (or (null no) (not (string-match no group))))
	  (progn
	    (gnus-summary-read-group group nil t)
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
  (save-excursion
    (let ((file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (if (get-file-buffer file)
	  (progn
	    (set-buffer (get-file-buffer file))
	    (and (buffer-modified-p) (save-buffer))
	    (kill-buffer (current-buffer)))))))

(defvar gnus-kill-file-name "KILL"
  "Suffix of the kill files.")

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a kill file name for NEWSGROUP.
If NEWSGROUP is nil, return the global kill file name instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global KILL file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-kill-files-directory "~/News")))
	((gnus-use-long-file-name 'not-kill)
	 ;; Append ".KILL" to newsgroup name.
	 (expand-file-name (concat (gnus-newsgroup-savable-name newsgroup)
				   "." gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))))


;;;
;;; Dribble file
;;;

(defvar gnus-dribble-ignore nil)
(defvar gnus-dribble-eval-file nil)

(defun gnus-dribble-file-name ()
  (concat gnus-current-startup-file "-dribble"))

(defun gnus-dribble-enter (string)
  (if (and (not gnus-dribble-ignore)
	   gnus-dribble-buffer
	   (buffer-name gnus-dribble-buffer))
      (let ((obuf (current-buffer)))
	(set-buffer gnus-dribble-buffer)
	(insert string "\n")
	(set-window-point (get-buffer-window (current-buffer)) (point-max))
	(set-buffer obuf))))

(defun gnus-dribble-read-file ()
  (let ((dribble-file (gnus-dribble-file-name)))
    (save-excursion 
      (set-buffer (setq gnus-dribble-buffer 
			(get-buffer-create 
			 (file-name-nondirectory dribble-file))))
      (gnus-add-current-to-buffer-list)
      (erase-buffer)
      (set-visited-file-name dribble-file)
      (buffer-disable-undo (current-buffer))
      (bury-buffer (current-buffer))
      (set-buffer-modified-p nil)
      (let ((auto (make-auto-save-file-name))
	    (gnus-dribble-ignore t))
	(if (or (file-exists-p auto) (file-exists-p dribble-file))
	    (progn
	      (if (file-newer-than-file-p auto dribble-file)
		  (setq dribble-file auto))
	      (insert-file-contents dribble-file)
	      (if (not (zerop (buffer-size)))
		  (set-buffer-modified-p t))
	      (if (gnus-y-or-n-p 
		   "Auto-save file exists. Do you want to read it? ")
		  (setq gnus-dribble-eval-file t))))))))

(defun gnus-dribble-eval-file ()
  (if (not gnus-dribble-eval-file)
      ()
    (setq gnus-dribble-eval-file nil)
    (save-excursion
      (let ((gnus-dribble-ignore t))
	(set-buffer gnus-dribble-buffer)
	(eval-buffer (current-buffer))))))

(defun gnus-dribble-delete-file ()
  (if (file-exists-p (gnus-dribble-file-name))
      (delete-file (gnus-dribble-file-name)))
  (if gnus-dribble-buffer
      (save-excursion
	(set-buffer gnus-dribble-buffer)
	(let ((auto (make-auto-save-file-name)))
	  (if (file-exists-p auto)
	      (delete-file auto))
	  (erase-buffer)
	  (set-buffer-modified-p nil)))))

(defun gnus-dribble-save ()
  (if (and gnus-dribble-buffer
	   (buffer-name gnus-dribble-buffer))
      (save-excursion
	(set-buffer gnus-dribble-buffer)
	(save-buffer))))

(defun gnus-dribble-clear ()
  (save-excursion
    (if (gnus-buffer-exists-p gnus-dribble-buffer)
	(progn
	  (set-buffer gnus-dribble-buffer)
	  (erase-buffer)
	  (set-buffer-modified-p nil)
	  (setq buffer-saved-size (buffer-size))))))

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
	 "%s open error: '%s'. Continue? "
	 (nth 1 gnus-select-method)
	 (gnus-status-message gnus-select-method)))
       (progn
	 (gnus-message 1 "Couldn't open server on %s" 
		       (nth 1 gnus-select-method))
	 (ding)
	 nil)))))

(defun gnus-check-server (&optional method)
  "If the news server is down, start it up again."
  (let ((method (if method method gnus-select-method)))
    (and (stringp method)
	 (setq method (gnus-server-to-method method)))
    (if (gnus-server-opened method)
	;; Stream is already opened.
	t
      ;; Open server.
      (gnus-message 5 "Opening server %s on %s..." (car method) (nth 1 method))
      (run-hooks 'gnus-open-server-hook)
      (prog1
	  (gnus-open-server method)
	(message "")))))

(defun gnus-nntp-message (&optional message)
  "Check the status of the NNTP server.
If the status of the server is clear and MESSAGE is non-nil, MESSAGE
is returned insted of the status string."
  (let ((status (gnus-status-message (gnus-find-method-for-group 
				      gnus-newsgroup-name)))
	(message (or message "")))
    (if (and (stringp status) (> (length status) 0))
	status message)))

(defun gnus-get-function (method function)
  (and (stringp method)
       (setq method (gnus-server-to-method method)))
  (let ((func (intern (format "%s-%s" (car method) function))))
    (if (not (fboundp func)) 
	(progn
	  (require (car method))
	  (if (not (fboundp func)) 
	      (error "No such function: %s" func))))
    func))

;;; Interface functions to the backends.

(defun gnus-open-server (method)
  (funcall (gnus-get-function method 'open-server)
	   (nth 1 method) (nthcdr 2 method)))

(defun gnus-close-server (method)
  (funcall (gnus-get-function method 'close-server) (nth 1 method)))

(defun gnus-request-list (method)
  (funcall (gnus-get-function method 'request-list) (nth 1 method)))

(defun gnus-request-list-newsgroups (method)
  (funcall (gnus-get-function method 'request-list-newsgroups) (nth 1 method)))

(defun gnus-request-newgroups (date method)
  (funcall (gnus-get-function method 'request-newgroups) 
	   date (nth 1 method)))

(defun gnus-server-opened (method)
  (funcall (gnus-get-function method 'server-opened) (nth 1 method)))

(defun gnus-status-message (method)
  (let ((method (if (stringp method) (gnus-find-method-for-group method)
		  method)))
    (funcall (gnus-get-function method 'status-message) (nth 1 method))))

(defun gnus-request-group (group &optional dont-check)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-group) 
	     (gnus-group-real-name group) (nth 1 method) dont-check)))

(defun gnus-request-asynchronous (group &optional articles)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-asynchronous) 
	     (gnus-group-real-name group) (nth 1 method) articles)))

(defun gnus-list-active-group (group)
  (let ((method (gnus-find-method-for-group group))
	(func 'list-active-group))
    (and (gnus-check-backend-function func group)
	 (funcall (gnus-get-function method func) 
		  (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-request-group-description (group)
  (let ((method (gnus-find-method-for-group group))
	(func 'request-group-description))
    (and (gnus-check-backend-function func group)
	 (funcall (gnus-get-function method func) 
		  (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-close-group (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'close-group) 
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-retrieve-headers (articles group)
  (let ((method (gnus-find-method-for-group group)))
    (if (and gnus-use-cache (numberp (car articles)))
	(gnus-cache-retrieve-headers articles group)
      (funcall (gnus-get-function method 'retrieve-headers) 
	       articles (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-retrieve-groups (groups method)
  (funcall (gnus-get-function method 'retrieve-groups) groups (nth 1 method)))

(defun gnus-request-article (article group &optional buffer)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-article) 
	     article (gnus-group-real-name group) (nth 1 method) buffer)))

(defun gnus-request-head (article group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-head) 
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-body (article group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-body) 
	     article (gnus-group-real-name group) (nth 1 method))))

;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defun gnus-request-post-buffer (post group subject header artbuf
				      info follow-to respect-poster)
  (let* ((info (or info (and group (nth 2 (gnus-gethash 
					   group gnus-newsrc-hashtb)))))
	 (method
	  (if (and gnus-post-method
		   ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
		   (memq 'post (assoc
				(format "%s" (car (gnus-find-method-for-group
						   gnus-newsgroup-name)))
				gnus-valid-select-methods)))
	      gnus-post-method
	    (gnus-find-method-for-group gnus-newsgroup-name))))
    (or (gnus-check-server method)
	(error "Can't open server %s:%s" (car method) (nth 1 method)))
    (let ((mail-self-blind nil)
	  (mail-archive-file-name nil))
      (funcall (gnus-get-function method 'request-post-buffer) 
	       post group subject header artbuf info follow-to
	       respect-poster))))

(defun gnus-request-post (method &optional force)
  (and (stringp method)
       (setq method (gnus-server-to-method method)))
  (and (not force) gnus-post-method
       (memq 'post (assoc (format "%s" (car method))
 			  gnus-valid-select-methods))
       (setq method gnus-post-method))
  (funcall (gnus-get-function method 'request-post) 
	   (nth 1 method)))

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

(defun gnus-request-accept-article (group &optional last)
  (goto-char (point-max))
  (or (bolp) (insert "\n"))
  (let ((func (if (symbolp group) group
		(car (gnus-find-method-for-group group)))))
    (funcall (intern (format "%s-request-accept-article" func))
	     (if (stringp group) (gnus-group-real-name group) group)
	     last)))

(defun gnus-request-replace-article (article group buffer)
  (let ((func (car (gnus-find-method-for-group group))))
    (funcall (intern (format "%s-request-replace-article" func))
	     article (gnus-group-real-name group) buffer)))

(defun gnus-request-create-group (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-create-group) 
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-member-of-valid (symbol group)
  (memq symbol (assoc
		(format "%s" (car (gnus-find-method-for-group group)))
		gnus-valid-select-methods)))

(defun gnus-secondary-method-p (method)
  (let ((methods gnus-secondary-select-methods)
	(gmethod (gnus-server-get-method nil method)))
    (while (and methods
		(not (equal (gnus-server-get-method nil (car methods)) 
			    gmethod)))
      (setq methods (cdr methods)))
    methods))

(defun gnus-find-method-for-group (group &optional info)
  (or gnus-override-method
      (and (not group)
	   gnus-select-method)
      (let ((info (or info (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
	    method)
	(if (or (not info)
		(not (setq method (nth 4 info))))
	    (setq method gnus-select-method)
	  (setq method
		(cond ((stringp method)
		       (gnus-server-to-method method))
		      ((stringp (car method))
		       (gnus-server-extend-method group method))
		      (t
		       method))))
	(gnus-server-add-address method))))

(defun gnus-check-backend-function (func group)
  (let ((method (if (stringp group) (car (gnus-find-method-for-group group))
		  group)))
    (fboundp (intern (format "%s-%s" method func)))))

(defun gnus-methods-using (method)
  (let ((valids gnus-valid-select-methods)
	outs)
    (while valids
      (if (memq method (car valids)) 
	  (setq outs (cons (car valids) outs)))
      (setq valids (cdr valids)))
    outs))

;;; 
;;; Active & Newsrc File Handling
;;;

;; Newsrc related functions.
;; Gnus internal format of gnus-newsrc-alist:
;; (("alt.general" 3 (1 . 1))
;;  ("alt.misc"    3 ((1 . 10) (12 . 15)))
;;  ("alt.test"    7 (1 . 99) (45 57 93)) ...)
;; The first item is the group name; the second is the subscription
;; level; the third is either a range of a list of ranges of read
;; articles, the optional fourth element is a list of marked articles,
;; the optional fifth element is the select method.
;;
;; Gnus internal format of gnus-newsrc-hashtb:
;; (95 ("alt.general" 3 (1 . 1)) ("alt.misc" 3 ((1 . 10) (12 . 15))) ...)
;; This is the entry for "alt.misc". The first element is the number
;; of unread articles in "alt.misc". The cdr of this entry is the
;; element *before* "alt.misc" in gnus-newsrc-alist, which makes is
;; trivial to remove or add new elements into gnus-newsrc-alist
;; without scanning the entire list. So, to get the actual information
;; of "alt.misc", you'd say something like 
;; (nth 2 (gnus-gethash "alt.misc" gnus-newsrc-hashtb))
;;
;; Gnus internal format of gnus-active-hashtb:
;; ((1 . 1))
;;  (5 . 10))
;;  (67 . 99)) ...)
;; The only element in each entry in this hash table is a range of
;; (possibly) available articles. (Articles in this range may have
;; been expired or canceled.)
;;
;; Gnus internal format of gnus-killed-list and gnus-zombie-list:
;; ("alt.misc" "alt.test" "alt.general" ...)

(defun gnus-setup-news (&optional rawfile level)
  "Setup news information.
If RAWFILE is non-nil, the .newsrc file will also be read.
If LEVEL is non-nil, the news will be set up at level LEVEL."
  (let ((init (not (and gnus-newsrc-alist gnus-active-hashtb (not rawfile)))))
    ;; Clear some variables to re-initialize news information.
    (if init (setq gnus-newsrc-alist nil 
		   gnus-active-hashtb nil))

    ;; Read the newsrc file and create `gnus-newsrc-hashtb'.
    (if init (gnus-read-newsrc-file rawfile))

    ;; If we don't read the complete active file, we fill in the
    ;; hashtb here. 
    (if (or (null gnus-read-active-file)
	    (eq gnus-read-active-file 'some))
	(gnus-update-active-hashtb-from-killed))

    ;; Read the active file and create `gnus-active-hashtb'.
    ;; If `gnus-read-active-file' is nil, then we just create an empty
    ;; hash table. The partial filling out of the hash table will be
    ;; done in `gnus-get-unread-articles'.
    (and gnus-read-active-file 
	 (not level)
	 (gnus-read-active-file))

    (or gnus-active-hashtb
	(setq gnus-active-hashtb (make-vector 4095 0)))

    ;; Possibly eval the dribble file.
    (and init gnus-use-dribble-file (gnus-dribble-eval-file))

    (gnus-update-format-specifications)

    ;; Find new newsgroups and treat them.
    (if (and init gnus-check-new-newsgroups gnus-read-active-file (not level)
	     (gnus-check-server gnus-select-method))
	(gnus-find-new-newsgroups))

    ;; Find the number of unread articles in each non-dead group.
    (let ((gnus-read-active-file (and (not level) gnus-read-active-file)))
      (gnus-get-unread-articles (or level (1+ gnus-level-subscribed))))

    (if (and init gnus-check-bogus-newsgroups 
	     gnus-read-active-file (not level)
	     (gnus-server-opened gnus-select-method))
	(gnus-check-bogus-newsgroups))))

(defun gnus-find-new-newsgroups ()
  "Search for new newsgroups and add them.
Each new newsgroup will be treated with `gnus-subscribe-newsgroup-method.'
The `-n' option line from .newsrc is respected."
  (interactive)
  (or (gnus-check-first-time-used)
      (if (or (consp gnus-check-new-newsgroups)
	      (eq gnus-check-new-newsgroups 'ask-server))
	  (gnus-ask-server-for-new-groups)
	(let ((groups 0)
	      group new-newsgroups)
	  (gnus-message 5 "Looking for new newsgroups...")
	  (or gnus-have-read-active-file (gnus-read-active-file))
	  (setq gnus-newsrc-last-checked-date (current-time-string))
	  (if (not gnus-killed-hashtb) (gnus-make-hashtable-from-killed))
	  ;; Go though every newsgroup in `gnus-active-hashtb' and compare
	  ;; with `gnus-newsrc-hashtb' and `gnus-killed-hashtb'.
	  (mapatoms
	   (lambda (sym)
	     (if (or (null (setq group (symbol-name sym)))
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
	  (if new-newsgroups 
	      (gnus-subscribe-hierarchical-interactive new-newsgroups))
	  ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
	  (if (> groups 0)
	      (gnus-message 6 "%d new newsgroup%s arrived." 
			    groups (if (> groups 1) "s have" " has"))
	    (gnus-message 6 "No new newsgroups."))))))

(defun gnus-matches-options-n (group)
  ;; Returns `subscribe' if the group is to be unconditionally
  ;; subscribed, `ignore' if it is to be ignored, and nil if there is
  ;; no match for the group.

  ;; First we check the two user variables.
  (cond
   ((and gnus-options-subscribe
	 (string-match gnus-options-subscribe group))
    'subscribe)
   ((and gnus-options-not-subscribe
	 (string-match gnus-options-not-subscribe group))
    'ignore)
   ;; Then we go through the list that was retrieved from the .newsrc
   ;; file.  This list has elements on the form 
   ;; `(REGEXP . {ignore,subscribe})'. The first match found (the list
   ;; is in the reverse order of the options line) is returned.
   (t
    (let ((regs gnus-newsrc-options-n))
      (while (and regs
		  (not (string-match (car (car regs)) group)))
	(setq regs (cdr regs)))
      (and regs (cdr (car regs)))))))

(defun gnus-ask-server-for-new-groups ()
  (let* ((date (or gnus-newsrc-last-checked-date (current-time-string)))
	 (methods (cons gnus-select-method 
			(append
			 (and (consp gnus-check-new-newsgroups)
			      gnus-check-new-newsgroups)
			 gnus-secondary-select-methods)))
	 (groups 0)
	 (new-date (current-time-string))
	 (hashtb (gnus-make-hashtable 100))
	 group new-newsgroups got-new method)
    ;; Go through both primary and secondary select methods and
    ;; request new newsgroups.  
    (while methods
      (setq method (gnus-server-get-method nil (car methods)))
      (and (gnus-check-server method)
	   (gnus-request-newgroups date method)
	   (save-excursion
	     (setq got-new t)
	     (set-buffer nntp-server-buffer)
	     ;; Enter all the new groups in a hashtable.
	     (gnus-active-to-gnus-format method hashtb 'ignore)))
      (setq methods (cdr methods)))
    (and got-new (setq gnus-newsrc-last-checked-date new-date))
    ;; Now all new groups from all select methods are in `hashtb'.
    (mapatoms
     (lambda (group-sym)
       (setq group (symbol-name group-sym))
       (if (or (null group)
	       (null (symbol-value group-sym))
	       (gnus-gethash group gnus-newsrc-hashtb)
	       (member group gnus-zombie-list)
	       (member group gnus-killed-list))
	   ;; The group is already known.
	   ()
	 (and (symbol-value group-sym)
	      (gnus-sethash group (symbol-value group-sym) gnus-active-hashtb))
	 (let ((do-sub (gnus-matches-options-n group)))
	   (cond ((eq do-sub 'subscribe)
		  (setq groups (1+ groups))
		  (gnus-sethash group group gnus-killed-hashtb)
		  (funcall 
		   gnus-subscribe-options-newsgroup-method group))
		 ((eq do-sub 'ignore)
		  nil)
		 (t
		  (setq groups (1+ groups))
		  (gnus-sethash group group gnus-killed-hashtb)
		  (if gnus-subscribe-hierarchical-interactive
		      (setq new-newsgroups (cons group new-newsgroups))
		    (funcall gnus-subscribe-newsgroup-method group)))))))
     hashtb)
    (if new-newsgroups 
	(gnus-subscribe-hierarchical-interactive new-newsgroups))
    ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
    (if (> groups 0)
	(gnus-message 6 "%d new newsgroup%s arrived." 
		      groups (if (> groups 1) "s have" " has")))
    got-new))

(defun gnus-check-first-time-used ()
  (if (or (> (length gnus-newsrc-alist) 1)
	  (file-exists-p gnus-startup-file)
	  (file-exists-p (concat gnus-startup-file ".el"))
	  (file-exists-p (concat gnus-startup-file ".eld")))
      nil
    (gnus-message 6 "First time user; subscribing you to default groups")
    (or gnus-have-read-active-file (gnus-read-active-file))
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
	  (if (gnus-gethash (car groups) gnus-active-hashtb)
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
;; subscription levels of newsgroups. This might mean just changing
;; from level 1 to 2, which is pretty trivial, from 2 to 6 or back
;; again, which subscribes/unsubscribes a group, which is equally
;; trivial. Changing from 1-7 to 8-9 means that you kill a group, and
;; from 8-9 to 1-7 means that you remove the group from the list of
;; killed (or zombie) groups and add them to the (kinda) subscribed
;; groups. And last but not least, moving from 8 to 9 and 9 to 8,
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
	(setq oldlevel (car (cdr (nth 2 entry)))))
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
      ;; list. If not, and it is in fact going to be killed, we remove
      ;; it from the newsrc hash table and assoc.
      (cond ((>= oldlevel gnus-level-zombie)
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
		   (setcdr (cdr entry) (cdr (cdr (cdr entry))))))))

      ;; Finally we enter (if needed) the list where it is supposed to
      ;; go, and change the subscription level. If it is to be killed,
      ;; we enter it into the killed or zombie list.
      (cond ((>= level gnus-level-zombie)
	     ;; Remove from the hash table.
	     (gnus-sethash group nil gnus-newsrc-hashtb)
	     (or (gnus-group-foreign-p group)
		 ;; We do not enter foreign groups into the list of dead
		 ;; groups.  
		 (if (= level gnus-level-zombie)
		     (setq gnus-zombie-list (cons group gnus-zombie-list))
		   (setq gnus-killed-list (cons group gnus-killed-list)))))
	    (t
	     ;; If the list is to be entered into the newsrc assoc, and
	     ;; it was killed, we have to create an entry in the newsrc
	     ;; hashtb format and fix the pointers in the newsrc assoc.
	     (if (>= oldlevel gnus-level-zombie)
		 (progn
		   (if (listp entry)
		       (progn
			 (setq info (cdr entry))
			 (setq num (car entry)))
		     (setq active (gnus-gethash group gnus-active-hashtb))
		     (setq num 
			   (if active (- (1+ (cdr active)) (car active)) t))
		     ;; Check whether the group is foreign. If so, the
		     ;; foreign select method has to be entered into the
		     ;; info. 
		     (let ((method (gnus-group-method-name group)))
		       (if (eq method gnus-select-method)
			   (setq info (list group level nil))
			 (setq info (list group level nil nil method)))))
		   (or previous 
		       (setq previous 
			     (let ((p gnus-newsrc-alist))
			       (while (cdr (cdr p))
				 (setq p (cdr p)))
			       p)))
		   (setq entry (cons info (cdr (cdr previous))))
		   (if (cdr previous)
		       (progn
			 (setcdr (cdr previous) entry)
			 (gnus-sethash group (cons num (cdr previous)) 
				       gnus-newsrc-hashtb))
		     (setcdr previous entry)
		     (gnus-sethash group (cons num previous)
				   gnus-newsrc-hashtb))
		   (if (cdr entry)
		       (setcdr (gnus-gethash (car (car (cdr entry)))
					     gnus-newsrc-hashtb)
			       entry)))
	       ;; It was alive, and it is going to stay alive, so we
	       ;; just change the level and don't change any pointers or
	       ;; hash table entries.
	       (setcar (cdr (car (cdr (cdr entry)))) level)))))))

(defun gnus-kill-newsgroup (newsgroup)
  "Obsolete function. Kills a newsgroup."
  (gnus-group-change-level
   (gnus-gethash newsgroup gnus-newsrc-hashtb) gnus-level-killed))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Remove bogus newsgroups.
If CONFIRM is non-nil, the user has to confirm the deletion of every
newsgroup." 
  (let ((newsrc (cdr gnus-newsrc-alist))
	bogus group entry)
    (gnus-message 5 "Checking bogus newsgroups...")
    (or gnus-have-read-active-file (gnus-read-active-file))
    ;; Find all bogus newsgroup that are subscribed.
    (while newsrc
      (setq group (car (car newsrc)))
      (if (or (gnus-gethash group gnus-active-hashtb) ; Active
	      (nth 4 (car newsrc))	; Foreign
	      (and confirm
		   (not (gnus-y-or-n-p
			 (format "Remove bogus newsgroup: %s " group)))))
	  ;; Don't remove.
	  ()
	;; Found a bogus newsgroup.
	(setq bogus (cons group bogus)))
      (setq newsrc (cdr newsrc)))
    ;; Remove all bogus subscribed groups by first killing them, and
    ;; then removing them from the list of killed groups.
    (while bogus
      (and (setq entry (gnus-gethash (car bogus) gnus-newsrc-hashtb))
	   (progn
	     (gnus-group-change-level entry gnus-level-killed)
	     (setq gnus-killed-list (delete (car bogus) gnus-killed-list))))
      (setq bogus (cdr bogus)))
    ;; Then we remove all bogus groups from the list of killed and
    ;; zombie groups. They are are removed without confirmation.
    (let ((dead-lists '(gnus-killed-list gnus-zombie-list))
	  killed)
      (while dead-lists
	(setq killed (symbol-value (car dead-lists)))
	(while killed
	  (setq group (car killed))
	  (or (gnus-gethash group gnus-active-hashtb)
	      ;; The group is bogus.
	      (set (car dead-lists)
		   (delete group (symbol-value (car dead-lists)))))
	  (setq killed (cdr killed)))
	(setq dead-lists (cdr dead-lists))))
    (gnus-message 5 "Checking bogus newsgroups...done")))

(defun gnus-check-duplicate-killed-groups ()
  "Remove duplicates from the list of killed groups."
  (interactive)
  (let ((killed gnus-killed-list))
    (while killed
      (gnus-message 9 "%d" (length killed))
      (setcdr killed (delete (car killed) (cdr killed)))
      (setq killed (cdr killed)))))

;; Go though `gnus-newsrc-alist' and compare with `gnus-active-hashtb'
;; and compute how many unread articles there are in each group.
(defun gnus-get-unread-articles (&optional level) 
  (let* ((newsrc (cdr gnus-newsrc-alist))
	 (level (or level (1+ gnus-level-subscribed)))
	 (foreign-level
	  (min 
	   (cond ((and gnus-activate-foreign-newsgroups 
		       (not (numberp gnus-activate-foreign-newsgroups)))
		  (1+ gnus-level-subscribed))
		 ((numberp gnus-activate-foreign-newsgroups)
		  gnus-activate-foreign-newsgroups)
		 (t 0))
	   level))
	 info group active virtuals method)
    (gnus-message 5 "Checking new news...")

    (while newsrc
      (setq info (car newsrc)
	    group (car info)
	    active (gnus-gethash group gnus-active-hashtb))

      ;; Check newsgroups. If the user doesn't want to check them, or
      ;; they can't be checked (for instance, if the news server can't
      ;; be reached) we just set the number of unread articles in this
      ;; newsgroup to t. This means that Gnus thinks that there are
      ;; unread articles, but it has no idea how many.
      (if (and (setq method (nth 4 info))
	       (not (gnus-server-equal gnus-select-method
				       (gnus-server-get-method nil method)))
	       (not (gnus-secondary-method-p method)))
	  ;; These groups are foreign. Check the level.
	  (if (<= (nth 1 info) foreign-level)
	      (if (eq (car (if (stringp method) 
			       (gnus-server-to-method method)
			     (nth 4 info))) 'nnvirtual)
		  ;; We have to activate the virtual groups after all
		  ;; the others, so we just pop them on a list for
		  ;; now. 
		  (setq virtuals (cons info virtuals))
		(and (setq active (gnus-activate-group (car info)))
		     ;; Close the groups as we look at them!
		     (gnus-close-group group))))

	;; These groups are native or secondary. 
	(if (and (not gnus-read-active-file)
		 (<= (nth 1 info) level))
	    (progn
	      (or gnus-read-active-file (gnus-check-server method))
	      (setq active (gnus-activate-group (car info))))))
      
      (if active
	  (gnus-get-unread-articles-in-group info active)
	;; The group couldn't be reached, so we nix out the number of
	;; unread articles and stuff.
	(gnus-sethash group nil gnus-active-hashtb)
	(setcar (gnus-gethash group gnus-newsrc-hashtb) t))

      (setq newsrc (cdr newsrc)))

    ;; Activate the virtual groups. This has to be done after all the
    ;; other groups. 
    ;; !!! If one virtual group contains another virtual group, even
    ;; doing it this way might cause problems.
    (while virtuals
      (and (setq active (gnus-activate-group (car (car virtuals))))
	   (gnus-get-unread-articles-in-group (car virtuals) active))
      (setq virtuals (cdr virtuals)))

    (gnus-message 5 "Checking new news...done")))

;; Create a hash table out of the newsrc alist. The `car's of the
;; alist elements are used as keys.
(defun gnus-make-hashtable-from-newsrc-alist ()
  (let ((alist gnus-newsrc-alist)
	(ohashtb gnus-newsrc-hashtb)
	prev)
    (setq gnus-newsrc-hashtb (gnus-make-hashtable (length alist)))
    (setq alist 
	  (setq prev (setq gnus-newsrc-alist 
			   (if (equal (car (car gnus-newsrc-alist))
				      "dummy.group")
			       gnus-newsrc-alist
			     (cons (list "dummy.group" 0 nil) alist)))))
    (while alist
      (gnus-sethash (car (car alist)) 
		    (cons (and ohashtb (car (gnus-gethash 
					     (car (car alist)) ohashtb))) 
			  prev) gnus-newsrc-hashtb)
      (setq prev alist
	    alist (cdr alist)))))

(defun gnus-make-hashtable-from-killed ()
  "Create a hash table from the killed and zombie lists."
  (let ((lists '(gnus-killed-list gnus-zombie-list))
	list)
    (setq gnus-killed-hashtb 
	  (gnus-make-hashtable 
	   (+ (length gnus-killed-list) (length gnus-zombie-list))))
    (while lists
      (setq list (symbol-value (car lists)))
      (setq lists (cdr lists))
      (while list
	(gnus-sethash (car list) (car list) gnus-killed-hashtb)
	(setq list (cdr list))))))

(defun gnus-get-unread-articles-in-group (info active)
  (let* ((range (nth 2 info))
	 (num 0)
	 (marked (nth 3 info)))
    ;; If a cache is present, we may have to alter the active info.
    (and gnus-use-cache
	 (gnus-cache-possibly-alter-active (car info) active))
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
      ;; The read list is a list of ranges. Fix them according to
      ;; the active hash table.
      ;; First peel off any elements that are below the lower
      ;; active limit. 
      (while (and (cdr range) 
		  (>= (car active) 
		      (or (and (atom (car (cdr range))) (car (cdr range)))
			  (car (car (cdr range))))))
	(if (numberp (car range))
	    (setcar range 
		    (cons (car range) 
			  (or (and (numberp (car (cdr range)))
				   (car (cdr range))) 
			      (cdr (car (cdr range))))))
	  (setcdr (car range) 
		  (or (and (numberp (nth 1 range)) (nth 1 range))
		      (cdr (car (cdr range))))))
	(setcdr range (cdr (cdr range))))
      ;; Adjust the first element to be the same as the lower limit. 
      (if (and (not (atom (car range))) 
	       (< (cdr (car range)) (car active)))
	  (setcdr (car range) (1- (car active))))
      ;; Then we want to peel off any elements that are higher
      ;; than the upper active limit.  
      (let ((srange range))
	;; Go past all legal elements.
	(while (and (cdr srange) 
		    (<= (or (and (atom (car (cdr srange)))
				 (car (cdr srange)))
			    (car (car (cdr srange)))) (cdr active)))
	  (setq srange (cdr srange)))
	(if (cdr srange)
	    ;; Nuke all remaining illegal elements.
	    (setcdr srange nil))

	;; Adjust the final element.
	(if (and (not (atom (car srange)))
		 (> (cdr (car srange)) (cdr active)))
	    (setcdr (car srange) (cdr active))))
      ;; Compute the number of unread articles.
      (while range
	(setq num (+ num (- (1+ (or (and (atom (car range)) (car range))
				    (cdr (car range))))
			    (or (and (atom (car range)) (car range))
				(car (car range))))))
	(setq range (cdr range)))
      (setq num (max 0 (- (cdr active) num)))))
    (and info
	 (progn
	   (and (assq 'tick marked)
		(inline (gnus-remove-illegal-marked-articles
			 (assq 'tick marked) (nth 2 info))))
	   (and (assq 'dormant marked)
		(inline (gnus-remove-illegal-marked-articles
			 (assq 'dormant marked) (nth 2 info))))
	   (setcar
	    (gnus-gethash (car info) gnus-newsrc-hashtb) 
	    (setq num (max 0 (- num (length (cdr (assq 'tick marked)))
				(length (cdr (assq 'dormant marked)))))))))
    num))

(defun gnus-remove-illegal-marked-articles (marked ranges)
  (let ((m (cdr marked)))
    ;; Make sure that all ticked articles are a subset of the unread
    ;; articles. 
    (while m
      (if (gnus-member-of-range (car m) ranges)
	  (setcdr marked (cdr m))
	(setq marked m))
      (setq m (cdr m)))))

(defun gnus-activate-group (group)
  ;; Check whether a group has been activated or not.
  (let ((method (gnus-find-method-for-group group))
	active)
    (and (gnus-check-server method)
	 ;; We escape all bugs and quit here to make it possible to
	 ;; continue if a group is so out-there that it reports bugs
	 ;; and stuff.
	 (condition-case ()
	     (gnus-request-group group)
	   (error nil)
	   (quit nil))
	 (save-excursion
	   (set-buffer nntp-server-buffer)
	   (goto-char (point-min))
	   ;; Parse the result we got from `gnus-request-group'.
	   (and (looking-at "[0-9]+ [0-9]+ \\([0-9]+\\) [0-9]+")
		(progn
		  (goto-char (match-beginning 1))
		  (gnus-sethash 
		   group (setq active (cons (read (current-buffer))
					    (read (current-buffer))))
		   gnus-active-hashtb))
		;; Return the new active info.
		active)))))

(defun gnus-update-read-articles 
  (group unread unselected ticked &optional domarks replied expirable killed
	 dormant bookmark score)
  "Update the list of read and ticked articles in GROUP using the
UNREAD and TICKED lists.
Note: UNSELECTED has to be sorted over `<'.
Returns whether the updating was successful."
  (let* ((active (or gnus-newsgroup-active 
		     (gnus-gethash group gnus-active-hashtb)))
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (marked (nth 3 info))
	 (prev 1)
	 (unread (sort (copy-sequence unread) (function <)))
	 read)
    (if (or (not info) (not active))
	;; There is no info on this group if it was, in fact,
	;; killed. Gnus stores no information on killed groups, so
	;; there's nothing to be done. 
	;; One could store the information somewhere temporarily,
	;; perhaps... Hmmm... 
	()
      ;; Remove any negative articles numbers.
      (while (and unread (< (car unread) 0))
	(setq unread (cdr unread)))
      ;; Remove any expired article numbers
      (while (and unread (< (car unread) (car active)))
	(setq unread (cdr unread)))
      (while (and ticked (< (car ticked) (car active)))
	(setq ticked (cdr ticked)))
      (while (and dormant (< (car dormant) (car active)))
	(setq dormant (cdr dormant)))
      (setq unread (sort (append unselected unread) '<))
      ;; Weed out duplicates.
      (let ((un unread))
	(while (cdr un)
	  (if (eq (car un) (car (cdr un)))
	      (setcdr un (cdr (cdr un)))
	    (setq un (cdr un)))))
      ;; Compute the ranges of read articles by looking at the list of
      ;; unread articles.  
      (while unread
	(if (/= (car unread) prev)
	    (setq read (cons (if (= prev (1- (car unread))) prev
			       (cons prev (1- (car unread)))) read)))
	(setq prev (1+ (car unread)))
	(setq unread (cdr unread)))
      (if (<= prev (cdr active))
	  (setq read (cons (cons prev (cdr active)) read)))
      ;; Enter this list into the group info.
      (setcar (cdr (cdr info)) 
	      (if (> (length read) 1) (nreverse read) read))
      ;; Enter the list of ticked articles.
      (gnus-set-marked-articles 
       info ticked
       (if domarks replied (cdr (assq 'reply marked)))
       (if domarks expirable (cdr (assq 'expire marked)))
       (if domarks killed (cdr (assq 'killed marked)))
       (if domarks dormant (cdr (assq 'dormant marked)))
       (if domarks bookmark (cdr (assq 'bookmark marked)))
       (if domarks score (cdr (assq 'score marked))))
      ;; Set the number of unread articles in gnus-newsrc-hashtb.
      (gnus-get-unread-articles-in-group 
       info (gnus-gethash group gnus-active-hashtb))
      t)))

(defun gnus-make-articles-unread (group articles)
  "Mark ARTICLES in GROUP as unread."
  (let* ((info (nth 2 (or (gnus-gethash group gnus-newsrc-hashtb)
			  (gnus-gethash (gnus-group-real-name group)
					gnus-newsrc-hashtb))))
	 (ranges (nth 2 info))
	 news)
    (while articles
      (and (gnus-member-of-range (car articles) ranges)
	   (setq news (cons (car articles) news)))
      (setq articles (cdr articles)))
    (if (not news)
	()
      (setcar (nthcdr 2 info)
	      (gnus-remove-from-range (nth 2 info) (nreverse news)))
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

;; Get the active file(s) from the backend(s).
(defun gnus-read-active-file ()
  (gnus-group-set-mode-line)
  (let ((methods (if (gnus-check-server gnus-select-method)
		     ;; The native server is available.
		     (cons gnus-select-method gnus-secondary-select-methods)
		   ;; The native server is down, so we just do the
		   ;; secondary ones.   
		   gnus-secondary-select-methods))
	list-type)
    (setq gnus-have-read-active-file nil)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (while methods
	(let* ((method (gnus-server-get-method nil (car methods)))
	       (where (nth 1 method))
	       (mesg (format "Reading active file%s via %s..."
			     (if (and where (not (zerop (length where))))
				 (concat " from " where) "")
			     (car method))))
	  (gnus-message 5 mesg)
	  (if (not (gnus-check-server method))
	      ()
	    (cond 
	     ((and (eq gnus-read-active-file 'some)
		   (gnus-check-backend-function 'retrieve-groups (car method)))
	      (let ((newsrc (cdr gnus-newsrc-alist))
		    (gmethod (gnus-server-get-method nil method))
		    groups)
		(while newsrc
		  (and (gnus-server-equal 
			(gnus-find-method-for-group 
			 (car (car newsrc)) (car newsrc))
			gmethod)
		       (setq groups (cons (gnus-group-real-name 
					   (car (car newsrc))) groups)))
		  (setq newsrc (cdr newsrc)))
		(gnus-check-server method)
		(setq list-type (gnus-retrieve-groups groups method))
		(cond 
		 ((not list-type)
		  (gnus-message 
		   1 "Cannot read partial active file from %s server." 
		   (car method))
		  (ding)
		  (sit-for 2))
		 ((eq list-type 'active)
		  (gnus-active-to-gnus-format method gnus-active-hashtb))
		 (t
		  (gnus-groups-to-gnus-format method gnus-active-hashtb)))))
	     (t
	      (if (not (gnus-request-list method))
		  (progn
		    (gnus-message 1 "Cannot read active file from %s server." 
				  (car method))
		    (ding))
		(gnus-active-to-gnus-format method)
		;; We mark this active file as read.
		(setq gnus-have-read-active-file
		      (cons method gnus-have-read-active-file))
		(gnus-message 5 "%sdone" mesg))))))
	(setq methods (cdr methods))))))

;; Read an active file and place the results in `gnus-active-hashtb'.
(defun gnus-active-to-gnus-format (method &optional hashtb ignore-errors)
  (let ((cur (current-buffer))
	(hashtb (or hashtb 
		    (if (and gnus-active-hashtb 
			     (not (equal method gnus-select-method)))
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (if (equal method gnus-select-method)
				(gnus-make-hashtable 
				 (count-lines (point-min) (point-max)))
			      (gnus-make-hashtable 4096))))))
	(flag-hashtb (gnus-make-hashtable 60)))
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
      ;; We always load the .newsrc.eld file. If always contains
      ;; much information that can not be gotten from the .newsrc
      ;; file (ticked articles, killed groups, foreign methods, etc.)
      (gnus-read-newsrc-el-file quick-file)
 
      (if (or force
	      (and (file-newer-than-file-p newsrc-file quick-file)
		   (file-newer-than-file-p newsrc-file 
					   (concat quick-file "d")))
	      (not gnus-newsrc-alist))
	  ;; We read the .newsrc file. Note that if there if a
	  ;; .newsrc.eld file exists, it has already been read, and
	  ;; the `gnus-newsrc-hashtb' has been created. While reading
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
	    (gnus-message 5 "Reading %s...done" newsrc-file))))))

(defun gnus-read-newsrc-el-file (file)
  (let ((ding-file (concat file "d")))
    ;; We always, always read the .eld file.
    (gnus-message 5 "Reading %s..." ding-file)
    (let (gnus-newsrc-assoc)
      (condition-case nil
	  (load ding-file t t t)
	(error nil))
      (and gnus-newsrc-assoc (setq gnus-newsrc-alist gnus-newsrc-assoc)))
    (let ((inhibit-quit t))
      (gnus-uncompress-newsrc-alist))
    (gnus-make-hashtable-from-newsrc-alist)
    (if (not (file-newer-than-file-p file ding-file))
	()
      ;; Old format quick file
      (gnus-message 5 "Reading %s..." file)
      ;; The .el file is newer than the .eld file, so we read that one
      ;; as well. 
      (gnus-read-old-newsrc-el-file file))))

;; Parse the old-style quick startup file
(defun gnus-read-old-newsrc-el-file (file)
  (let (newsrc killed marked group m)
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
      (while newsrc
	(setq group (car newsrc))
	(let ((info (nth 2 (gnus-gethash (car group) gnus-newsrc-hashtb))))
	  (if info
	      (progn
		(setcar (nthcdr 2 info) (cdr (cdr group)))
		(setcar (cdr info)
			(if (nth 1 group) gnus-level-default-subscribed 
			  gnus-level-default-unsubscribed))
		(setq gnus-newsrc-alist (cons info gnus-newsrc-alist)))
	    (setq gnus-newsrc-alist
		  (cons 
		   (setq info
			 (list (car group)
			       (if (nth 1 group) gnus-level-default-subscribed
				 gnus-level-default-unsubscribed) 
			       (cdr (cdr group))))
		   gnus-newsrc-alist)))
	  (if (setq m (assoc (car group) marked))
	      (setcdr (cdr (cdr info))
		      (cons (list (cons 'tick (cdr m))) nil))))
	(setq newsrc (cdr newsrc)))
      (setq newsrc killed)
      (while newsrc
	(setcar newsrc (car (car newsrc)))
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

(defun gnus-uncompress-newsrc-alist ()
  ;; Uncompress all lists of marked articles in the newsrc assoc.
  (let ((newsrc gnus-newsrc-alist)
	marked)
    (while newsrc
      (if (not (setq marked (nth 3 (car newsrc))))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (setcdr (car marked) (gnus-uncompress-range (cdr (car marked)))))
	  (setq marked (cdr marked))))
      (setq newsrc (cdr newsrc)))))

(defun gnus-compress-newsrc-alist ()
  ;; Compress all lists of marked articles in the newsrc assoc.
  (let ((newsrc gnus-newsrc-alist)
	marked)
    (while newsrc
      (if (not (setq marked (nth 3 (car newsrc))))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (setcdr (car marked) 
		      (condition-case ()
			  (gnus-compress-sequence 
			   (sort (cdr (car marked)) '<) t)
			(error (cdr (car marked))))))
	  (setq marked (cdr marked))))
      (setq newsrc (cdr newsrc)))))

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
	      ;; string-to-int because it's faster. narrow/widen is
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
			;; it's kinda OK. Perhaps the user should be
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
		    (gnus-message 3 "Mangled line: %s" 
				  (buffer-substring (gnus-point-at-bol) 
						    (gnus-point-at-eol)))
		    (ding)
		    (sit-for 1)))
	      nil))
	  ;; Skip past ", ". Spaces are illegal in these ranges, but
	  ;; we allow them, because it's a common mistake to put a
	  ;; space after the comma.
	  (skip-chars-forward ", "))

	;; We have already read .newsrc.eld, so we gently update the
	;; data in the hash table with the information we have just
	;; read. 
	(if (not group)
	    ()
	  (let ((info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
		level)
	    (if info
		;; There is an entry for this file in the alist.
		(progn
		  (setcar (nthcdr 2 info) (nreverse reads))
		  ;; We update the level very gently.  In fact, we
		  ;; only change it if there's been a status change
		  ;; from subscribed to unsubscribed, or vice versa.
		  (setq level (nth 1 info))
		  (cond ((and (<= level gnus-level-subscribed)
			      (not subscribed))
			 (setq level (if reads
					 gnus-level-default-unsubscribed 
				       (1+ gnus-level-default-unsubscribed))))
			((and (> level gnus-level-subscribed) subscribed)
			 (setq level gnus-level-default-subscribed)))
		  (setcar (cdr info) level))
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
      ;; what we've read from .newsrc.eld. We have to merge these
      ;; lists. We do this by "attaching" any (foreign) groups in the
      ;; gnus-newsrc-alist to the (native) group that precedes them. 
      (let ((rc (cdr gnus-newsrc-alist))
	    (prev gnus-newsrc-alist)
	    entry mentry)
	(while rc
	  (or (null (nth 4 (car rc)))	; It's a native group.
	      (assoc (car (car rc)) newsrc) ; It's already in the alist.
	      (if (setq entry (assoc (car (car prev)) newsrc))
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
;; subscribed. A line like
;; options -n !all rec.all
;; will lead to a list that looks like
;; (("^rec\\..+" . subscribe) 
;;  ("^.+" . ignore))
;; So all "rec.*" groups will be subscribed, while all the other
;; groups will be ignored. Note that "options -n !all rec.all" is very
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
	      ;; spec. We put this spec (minus the bang) and the
	      ;; symbol `ignore' into the list.
	      (setq out (cons (cons (concat 
				     "^" (buffer-substring 
					  (1+ (match-beginning 0))
					  (match-end 0)))
				    'ignore) out))
	    ;; There was no bang, so this is a "yes" spec.
	    (setq out (cons (cons (concat 
				   "^" (buffer-substring (match-beginning 0)
							 (match-end 0)))
				  'subscribe) out)))))
    
      (setq gnus-newsrc-options-n out))))

	       
(defun gnus-save-newsrc-file ()
  "Save .newsrc file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-alist.
  (and (or gnus-newsrc-alist gnus-killed-list)
       gnus-current-startup-file
       (progn
	 (run-hooks 'gnus-save-newsrc-hook)
	 (save-excursion
	   (if (and gnus-use-dribble-file
		    (or (not gnus-dribble-buffer)
			(not (buffer-name gnus-dribble-buffer))
			(zerop (save-excursion
				 (set-buffer gnus-dribble-buffer)
				 (buffer-size)))))
	       (gnus-message 4 "(No changes need to be saved)")
	     (if gnus-save-newsrc-file
		 (progn
		   (gnus-message 5 "Saving %s..." gnus-current-startup-file)
		   ;; Make backup file of master newsrc.
		   (gnus-gnus-to-newsrc-format)
		   (gnus-message 5 "Saving %s...done"
				 gnus-current-startup-file)))
	     ;; Quickly loadable .newsrc.
	     (set-buffer (get-buffer-create " *Gnus-newsrc*"))
	     (make-local-variable 'version-control)
	     (setq version-control 'never)
	     (setq buffer-file-name (concat gnus-current-startup-file ".eld"))
	     (gnus-add-current-to-buffer-list)
	     (buffer-disable-undo (current-buffer))
	     (erase-buffer)
	     (gnus-message 5 "Saving %s.eld..." gnus-current-startup-file)
	     (gnus-gnus-to-quick-newsrc-format)
	     (save-buffer)
	     (kill-buffer (current-buffer))
	     (gnus-message 5 "Saving %s.eld...done" gnus-current-startup-file)
	     (gnus-dribble-delete-file))))))

(defun gnus-gnus-to-quick-newsrc-format ()
  "Insert Gnus variables such as gnus-newsrc-alist in lisp format."
  (insert ";; Gnus startup file.\n")
  (insert ";; Never delete this file - touch .newsrc instead to force Gnus\n")
  (insert ";; to read .newsrc.\n")
  (insert "(setq gnus-newsrc-file-version "
	  (prin1-to-string gnus-version) ")\n")
  (let ((variables gnus-variable-list)
	(inhibit-quit t)
	(gnus-newsrc-alist (cdr gnus-newsrc-alist))
	variable)
    ;; insert lisp expressions.
    (gnus-compress-newsrc-alist)
    (while variables
      (setq variable (car variables))
      (and (boundp variable)
	   (symbol-value variable)
	   (or gnus-save-killed-list (not (eq variable 'gnus-killed-list)))
	   (insert "(setq " (symbol-name variable) " '"
		   (prin1-to-string (symbol-value variable))
		   ")\n"))
      (setq variables (cdr variables)))
    (gnus-uncompress-newsrc-alist)))


(defun gnus-gnus-to-newsrc-format ()
  ;; Generate and save the .newsrc file.
  (let ((newsrc (cdr gnus-newsrc-alist))
	info ranges range)
    (save-excursion
      (set-buffer (create-file-buffer gnus-current-startup-file))
      (setq buffer-file-name gnus-current-startup-file)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; Write options.
      (if gnus-newsrc-options (insert gnus-newsrc-options))
      ;; Write subscribed and unsubscribed.
      (while newsrc
	(setq info (car newsrc))
	(if (not (nth 4 info))		;Don't write foreign groups to .newsrc.
	    (progn
	      (insert (car info) (if (> (nth 1 info) gnus-level-subscribed)
				     "!" ":"))
	      (if (setq ranges (nth 2 info))
		  (progn
		    (insert " ")
		    (if (not (listp (cdr ranges)))
			(if (= (car ranges) (cdr ranges))
			    (insert (int-to-string (car ranges)))
			  (insert (int-to-string (car ranges)) "-" 
				  (int-to-string (cdr ranges))))
		      (while ranges
			(setq range (car ranges)
			      ranges (cdr ranges))
			(if (or (atom range) (= (car range) (cdr range)))
			    (insert (int-to-string 
				     (or (and (atom range) range) 
					 (car range))))
			  (insert (int-to-string (car range)) "-"
				  (int-to-string (cdr range))))
			(if ranges (insert ","))))))
	      (insert "\n")))
	(setq newsrc (cdr newsrc)))
      (make-local-variable 'version-control)
      (setq version-control 'never)
      ;; It has been reported that sometime the modtime on the .newsrc
      ;; file seems to be off. We really do want to overwrite it, so
      ;; we clear the modtime here before saving. It's a bit odd,
      ;; though... 
      ;; sometimes the modtime clear isn't sufficient.  most brute force:
      ;; delete the silly thing entirely first.  but this fails to provide
      ;; such niceties as .newsrc~ creation.
      (if gnus-modtime-botch
	  (delete-file gnus-startup-file)
	(clear-visited-file-modtime))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun gnus-read-all-descriptions-files ()
  (let ((methods (cons gnus-select-method gnus-secondary-select-methods)))
    (while methods
      (gnus-read-descriptions-file (car methods))
      (setq methods (cdr methods)))
    t))

(defun gnus-read-descriptions-file (&optional method)
  (let ((method (or method gnus-select-method)))
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
      (let (group)
	(save-excursion
	  (save-restriction
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-min))
	    (if (or (search-forward "\n.\n" nil t)
		    (goto-char (point-max)))
		(progn
		  (beginning-of-line)
		  (narrow-to-region (point-min) (point))))
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
	      ;; ... which leads to this line being effectively ignored.
	      (and (symbolp group)
		   (set group (buffer-substring 
			       (point) (progn (end-of-line) (point)))))
	      (forward-line 1))))
	(gnus-message 5 "Reading descriptions file...done")
	t)))))

(defun gnus-group-get-description (group)
  ;; Get the description of a group by sending XGTITLE to the server.
  (and (gnus-request-group-description group)
       (save-excursion
	 (set-buffer nntp-server-buffer)
	 (goto-char (point-min))
	 (and (looking-at "[^ \t]+[ \t]+\\(.*\\)")
	      (buffer-substring (match-beginning 1) (match-end 1))))))

;;;
;;; Server
;;;

(defvar gnus-server-mode-hook nil
  "Hook run in `gnus-server-mode' buffers.")

(defconst gnus-server-line-format "     {%(%h:%w%)}\n"
  "Format of server lines.
It works along the same lines as a normal formatting string,
with some simple extensions.")

(defvar gnus-server-mode-line-format "Gnus  List of servers"
  "The format specification for the server mode line.")

(defconst gnus-server-line-format-alist
  (list (list ?h 'how ?s)
	(list ?n 'name ?s)
	(list ?w 'where ?s)
	))

(defconst gnus-server-mode-line-format-alist 
  (list (list ?S 'news-server ?s)
	(list ?M 'news-method ?s)
	(list ?u 'user-defined ?s)))

(defvar gnus-server-line-format-spec nil)
(defvar gnus-server-mode-line-format-spec nil)
(defvar gnus-server-killed-servers nil)

(defvar gnus-server-mode-map nil)
(put 'gnus-server-mode 'mode-class 'special)

(if gnus-server-mode-map
    nil
  (setq gnus-server-mode-map (make-sparse-keymap))
  (suppress-keymap gnus-server-mode-map)
  (define-key gnus-server-mode-map " " 'gnus-server-read-server)
  (define-key gnus-server-mode-map "\r" 'gnus-server-read-server)
  (define-key gnus-server-mode-map gnus-mouse-2 'gnus-server-pick-server)
  (define-key gnus-server-mode-map "q" 'gnus-server-exit)
  (define-key gnus-server-mode-map "l" 'gnus-server-list-servers)
  (define-key gnus-server-mode-map "k" 'gnus-server-kill-server)
  (define-key gnus-server-mode-map "y" 'gnus-server-yank-server)
  (define-key gnus-server-mode-map "c" 'gnus-server-copy-server)
  (define-key gnus-server-mode-map "a" 'gnus-server-add-server)
  (define-key gnus-server-mode-map "e" 'gnus-server-edit-server))

(defun gnus-server-mode ()
  "Major mode for listing and editing servers.

All normal editing commands are switched off.
\\<gnus-server-mode-map>

For more in-depth information on this mode, read the manual (`\\[gnus-info-find-node]'). 

The following commands are available:

\\{gnus-server-mode-map}"
  (interactive)
  (if gnus-visual (gnus-server-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-server-mode)
  (setq mode-name "Server")
					;  (gnus-group-set-mode-line)
  (setq mode-line-process nil)
  (use-local-map gnus-server-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gnus-server-mode-hook))

(defun gnus-server-insert-server-line (sformat name method)
  (let* ((sformat (or sformat gnus-server-line-format-spec))
	 (how (car method))
	 (where (nth 1 method))
	 b)
    (beginning-of-line)
    (setq b (point))
    ;; Insert the text.
    (insert (eval sformat))
    (add-text-properties b (1+ b) (list 'gnus-server (intern name)))))

(defun gnus-server-setup-buffer ()
  (if (get-buffer gnus-server-buffer)
      ()
    (save-excursion
      (set-buffer (get-buffer-create gnus-server-buffer))
      (gnus-server-mode)
      (and gnus-carpal (gnus-carpal-setup-buffer 'server)))))

(defun gnus-server-prepare ()
  (setq gnus-server-mode-line-format-spec 
	(gnus-parse-format gnus-server-mode-line-format 
			   gnus-server-mode-line-format-alist))
  (setq gnus-server-line-format-spec 
	(gnus-parse-format gnus-server-line-format 
			   gnus-server-line-format-alist))
  (let ((alist gnus-server-alist)
	(buffer-read-only nil))
    (erase-buffer)
    (while alist
      (gnus-server-insert-server-line nil (car (car alist)) (cdr (car alist)))
      (setq alist (cdr alist))))
  (goto-char (point-min))
  (gnus-server-position-cursor))

(defun gnus-server-server-name ()
  (let ((server (get-text-property (gnus-point-at-bol) 'gnus-server)))
    (and server (symbol-name server))))

(defalias 'gnus-server-position-cursor 'gnus-goto-colon)

(defconst gnus-server-edit-buffer "*Gnus edit server*")

(defun gnus-server-update-server (server)
  (save-excursion
    (set-buffer gnus-server-buffer)
    (let ((buffer-read-only nil)
	  (info (cdr (assoc server gnus-server-alist))))
      (gnus-dribble-enter 
       (concat "(gnus-server-set-info \"" server "\" '"
	       (prin1-to-string info) ")"))
      ;; Buffer may be narrowed.
      (save-restriction
	(widen)
	(if (gnus-server-goto-server server)
	    (delete-region (progn (beginning-of-line) (point))
			   (progn (forward-line 1) (point))))
	(let ((entry (assoc server gnus-server-alist)))
	  (gnus-server-insert-server-line nil (car entry) (cdr entry))
	  (gnus-server-position-cursor))))))

(defun gnus-server-set-info (server info)
  ;; Enter a select method into the virtual server alist.
  (gnus-dribble-enter 
   (concat "(gnus-server-set-info \"" server "\" '"
	   (prin1-to-string info) ")"))
  (let* ((server (nth 1 info))
	 (entry (assoc server gnus-server-alist)))
    (if entry (setcdr entry info)
      (setq gnus-server-alist
	    (nconc gnus-server-alist (list (cons server info)))))))

(defun gnus-server-to-method (server)
  ;; Map virtual server names to select methods.
  (or (and (equal server "native") gnus-select-method)
      (cdr (assoc server gnus-server-alist))))

(defun gnus-server-extend-method (group method)
  ;; This function "extends" a virtual server.  If the server is
  ;; "hello", and the select method is ("hello" (my-var "something")) 
  ;; in the group "alt.alt", this will result in a new virtual server
  ;; called "helly+alt.alt".
  (let ((entry
	 (gnus-copy-sequence 
	  (if (equal (car method) "native") gnus-select-method
	    (cdr (assoc (car method) gnus-server-alist))))))
    (setcar (cdr entry) (concat (nth 1 entry) "+" group))
    (nconc entry (cdr method))))

(defun gnus-server-get-method (group method)
  ;; Input either a server name, and extended server name, or a
  ;; select method, and return a select method. 
  (cond ((stringp method)
	 (gnus-server-to-method method))
	((and (stringp (car method)) group)
	 (gnus-server-extend-method group method))
	(t
	 (gnus-server-add-address method))))

(defun gnus-server-add-address (method)
  (let ((method-name (symbol-name (car method))))
    (if (and (memq 'address (assoc method-name gnus-valid-select-methods))
	     (not (assq (intern (concat method-name "-address")) method)))
	(append method (list (list (intern (concat method-name "-address"))
				   (nth 1 method))))
      method)))

(defun gnus-server-equal (s1 s2)
  (or (equal s1 s2)
      (and (= (length s1) (length s2))
	   (progn
	     (while (and s1 (member (car s1) s2))
	       (setq s1 (cdr s1)))
	     (null s1)))))

;;; Interactive server functions.

(defun gnus-server-kill-server (server)
  "Kill the server on the current line."
  (interactive (list (gnus-server-server-name)))
  (or (gnus-server-goto-server server)
      (if server (error "No such server: %s" server)
	(error "No server on the current line")))
  (gnus-dribble-enter "")
  (let ((buffer-read-only nil))
    (delete-region (progn (beginning-of-line) (point))
		   (progn (forward-line 1) (point))))
  (setq gnus-server-killed-servers 
	(cons (assoc server gnus-server-alist) gnus-server-killed-servers))
  (setq gnus-server-alist (delq (car gnus-server-killed-servers)
				gnus-server-alist))
  (gnus-server-position-cursor))

(defun gnus-server-yank-server ()
  "Yank the previously killed server."
  (interactive)
  (or gnus-server-killed-servers
      (error "No killed servers to be yanked"))
  (let ((alist gnus-server-alist)
	(server (gnus-server-server-name))
	(killed (car gnus-server-killed-servers)))
    (if (not server) 
	(setq gnus-server-alist (nconc gnus-server-alist (list killed)))
      (if (string= server (car (car gnus-server-alist)))
	  (setq gnus-server-alist (cons killed gnus-server-alist))
	(while (and (cdr alist)
		    (not (string= server (car (car (cdr alist))))))
	  (setq alist (cdr alist)))
	(setcdr alist (cons killed (cdr alist)))))
    (gnus-server-update-server (car killed))
    (setq gnus-server-killed-servers (cdr gnus-server-killed-servers))
    (gnus-server-position-cursor)))

(defun gnus-server-exit ()
  "Return to the group buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer gnus-group-buffer))

(defun gnus-server-list-servers ()
  "List all available servers."
  (interactive)
  (let ((cur (gnus-server-server-name)))
    (gnus-server-prepare)
    (if cur (gnus-server-goto-server cur)
      (goto-char (point-max))
      (forward-line -1))
    (gnus-server-position-cursor)))

(defun gnus-server-copy-server (from to)
  (interactive
   (list
    (or (gnus-server-server-name)
	(error "No server on the current line"))
    (read-string "Copy to: ")))
  (or from (error "No server on current line"))
  (or (and to (not (string= to ""))) (error "No name to copy to"))
  (and (assoc to gnus-server-alist) (error "%s already exists" to))
  (or (assoc from gnus-server-alist) 
      (error "%s: no such server" from))
  (let ((to-entry (gnus-copy-sequence (assoc from gnus-server-alist))))
    (setcar to-entry to)
    (setcar (nthcdr 2 to-entry) to)
    (setq gnus-server-killed-servers 
	  (cons to-entry gnus-server-killed-servers))
    (gnus-server-yank-server)))

(defun gnus-server-add-server (how where)
  (interactive 
   (list (intern (completing-read "Server method: "
				  gnus-valid-select-methods nil t))
	 (read-string "Server name: ")))
  (setq gnus-server-killed-servers 
	(cons (list where how where) gnus-server-killed-servers))
  (gnus-server-yank-server))

(defun gnus-server-goto-server (server)
  "Jump to a server line."
  (interactive
   (list (completing-read "Goto server: " gnus-server-alist nil t)))
  (let ((to (text-property-any (point-min) (point-max) 
			       'gnus-server (intern server))))
    (and to
	 (progn
	   (goto-char to) 
	   (gnus-server-position-cursor)))))

(defun gnus-server-edit-server (server)
  "Edit the server on the current line."
  (interactive (list (gnus-server-server-name)))
  (or server
      (error "No server on current line"))
  (let ((winconf (current-window-configuration)))
    (get-buffer-create gnus-server-edit-buffer)
    (gnus-configure-windows 'edit-server)
    (gnus-add-current-to-buffer-list)
    (emacs-lisp-mode)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    (use-local-map (copy-keymap (current-local-map)))
    (let ((done-func '(lambda () 
			"Exit editing mode and update the information."
			(interactive)
			(gnus-server-edit-server-done 'group))))
      (setcar (cdr (nth 4 done-func)) server)
      (local-set-key "\C-c\C-c" done-func))
    (erase-buffer)
    (insert ";; Type `C-c C-c' after you have edited the server.\n\n")
    (insert (pp-to-string (cdr (assoc server gnus-server-alist))))))

(defun gnus-server-edit-server-done (server)
  (interactive)
  (set-buffer (get-buffer-create gnus-server-edit-buffer))
  (goto-char (point-min))
  (let ((form (read (current-buffer)))
	(winconf gnus-prev-winconf))
    (gnus-server-set-info server form)
    (kill-buffer (current-buffer))
    (and winconf (set-window-configuration winconf))
    (set-buffer gnus-server-buffer)
    (gnus-server-update-server (gnus-server-server-name))
    (gnus-server-list-servers)
    (gnus-server-position-cursor)))

(defun gnus-server-read-server (server)
  "Browse a server."
  (interactive (list (gnus-server-server-name)))
  (gnus-browse-foreign-server (gnus-server-to-method server) (current-buffer)))

(defun gnus-mouse-pick-server (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-server-read-server (gnus-server-server-name)))

;;;
;;; entry points into gnus-score.el
;;;

;;; Finding score files. 

(defvar gnus-global-score-files nil
  "*List of global score files and directories.
Set this variable if you want to use people's score files.  One entry
for each score file or each score file directory.  Gnus will decide
by itself what score files are applicable to which group.

Say you want to use the single score file
\"/ftp.ifi.uio.no@ftp:/pub/larsi/ding/score/soc.motss.SCORE\" and all
score files in the \"/ftp.some-where:/pub/score\" directory.

 (setq gnus-global-score-files
       '(\"/ftp.ifi.uio.no:/pub/larsi/ding/score/soc.motss.SCORE\"
         \"/ftp.some-where:/pub/score\"))")

(defun gnus-score-score-files (group)
  "Return a list of all possible score files."
  ;; Search and set any global score files.
  (and gnus-global-score-files 
       (or gnus-internal-global-score-files
	   (gnus-score-search-global-directories gnus-global-score-files)))
  ;; Fix the kill-file dir variable.
  (setq gnus-kill-files-directory 
	(file-name-as-directory
	 (or gnus-kill-files-directory "~/News/")))
  ;; If we can't read it, there are no score files.
  (if (not (file-exists-p (expand-file-name gnus-kill-files-directory)))
      (setq gnus-score-file-list nil)
    (if (gnus-use-long-file-name 'not-score)
	;; We want long file names.
	(if (or (not gnus-score-file-list)
		(not (car gnus-score-file-list))
		(gnus-file-newer-than gnus-kill-files-directory
				      (car gnus-score-file-list)))
	    (setq gnus-score-file-list 
		  (cons (nth 5 (file-attributes gnus-kill-files-directory))
			(nreverse 
			 (directory-files 
			  gnus-kill-files-directory t 
			  (gnus-score-file-regexp))))))
      ;; We do not use long file names, so we have to do some
      ;; directory traversing.  
      (let ((mdir (length (expand-file-name gnus-kill-files-directory)))
  	    (suffixes (list gnus-score-file-suffix gnus-adaptive-file-suffix))
 	    dir files suffix)
  	(while suffixes
 	  (setq dir (expand-file-name
 		     (concat gnus-kill-files-directory
 			     (gnus-replace-chars-in-string group ?. ?/))))
	  (setq dir (gnus-replace-chars-in-string dir ?: ?/))
	  (setq suffix (car suffixes)
		suffixes (cdr suffixes))
	  (if (file-exists-p (concat dir "/" suffix))
	      (setq files (cons (concat dir "/" suffix) files)))
	  (while (>= (1+ (length dir)) mdir)
	    (and (file-exists-p (concat dir "/all/" suffix))
		 (setq files (cons (concat dir "/all/" suffix) files)))
	    (string-match "/[^/]*$" dir)
	    (setq dir (substring dir 0 (match-beginning 0)))))
	(setq gnus-score-file-list 
	      (cons nil (nreverse files)))))
    (cdr gnus-score-file-list)))

(defun gnus-score-file-regexp ()
  (concat "\\(" gnus-score-file-suffix 
	  "\\|" gnus-adaptive-file-suffix "\\)$"))
	
(defun gnus-score-find-bnews (group)
  "Return a list of score files for GROUP.
The score files are those files in the ~/News directory which matches
GROUP using BNews sys file syntax."
  (let* ((sfiles (append (gnus-score-score-files group)
			 gnus-internal-global-score-files))
	 (kill-dir (file-name-as-directory 
		    (expand-file-name gnus-kill-files-directory)))
	 (klen (length kill-dir))
	 ofiles not-match regexp)
    (save-excursion
      (set-buffer (get-buffer-create "*gnus score files*"))
      (buffer-disable-undo (current-buffer))
      ;; Go through all score file names and create regexp with them
      ;; as the source.  
      (while sfiles
	(erase-buffer)
	(insert (car sfiles))
	(goto-char (point-min))
	;; First remove the suffix itself.
	(re-search-forward (concat "." (gnus-score-file-regexp)))
	(replace-match "" t t) 
	(goto-char (point-min))
	(if (looking-at (regexp-quote kill-dir))
	    ;; If the file name was just "SCORE", `klen' is one character
	    ;; too much.
	    (delete-char (min (1- (point-max)) klen))
	  (goto-char (point-max))
	  (search-backward "/")
	  (delete-region (1+ (point)) (point-min)))
	;; If short file names were used, we have to translate slashes.
	(goto-char (point-min))
	(while (re-search-forward "[/:]" nil t)
	  (replace-match "." t t))
	;; Kludge to get rid of "nntp+" problems.
	(goto-char (point-min))
	(and (looking-at "nn[a-z]+\\+")
	     (progn
	       (search-forward "+")
	       (forward-char -1)
	       (insert "\\")))
	;; Translate ".all" to "[./].*";
	(while (search-forward ".all" nil t)
	  (replace-match "[./:].*" t t))
	(goto-char (point-min))
	;; Translate "all" to ".*".
	(while (search-forward "all" nil t)
	  (replace-match ".*" t t))
	(goto-char (point-min))
	;; Deal with "not."s.
	(if (looking-at "not.")
	    (progn
	      (setq not-match t)
	      (setq regexp (buffer-substring 5 (point-max))))
	  (setq regexp (buffer-substring 1 (point-max)))
	  (setq not-match nil))
	;; Finally - if this resulting regexp matches the group name,
	;; we add this score file to the list of score files
	;; applicable to this group.
	(if (or (and not-match
		     (not (string-match regexp group)))
		(and (not not-match)
		     (string-match regexp group)))
	    (setq ofiles (cons (car sfiles) ofiles)))
	(setq sfiles (cdr sfiles)))
      (kill-buffer (current-buffer))
      ;; Slight kludge here - the last score file returned should be
      ;; the local score file, whether it exists or not. This is so
      ;; that any score commands the user enters will go to the right
      ;; file, and not end up in some global score file.
      (let ((localscore
	     (expand-file-name
	      (if (gnus-use-long-file-name 'not-score)
		  (concat gnus-kill-files-directory group "." 
			  gnus-score-file-suffix)
		(concat gnus-kill-files-directory
			(gnus-replace-chars-in-string group ?. ?/ ?: ?/)
			"/" gnus-score-file-suffix)))))
	;; The local score file might already be there, but it's
	;; supposed to be the very last file, so we delete it from the
	;; list if it's already there, and add it to the head of the
	;; list. 
	(setq ofiles (cons localscore (delete localscore ofiles))))
      (nreverse ofiles))))

(defun gnus-score-find-single (group)
  "Return list containing the score file for GROUP."
  (list (gnus-score-file-name group gnus-adaptive-file-suffix)
	(gnus-score-file-name group)))

(defun gnus-score-find-hierarchical (group)
  "Return list of score files for GROUP.
This includes the score file for the group and all its parents."
  (let ((all (copy-sequence '(nil)))
	(start 0))
    (while (string-match "\\." group (1+ start))
      (setq start (match-beginning 0))
      (setq all (cons (substring group 0 start) all)))
    (setq all (cons group all))
    (nconc
     (mapcar (lambda (newsgroup)
	       (gnus-score-file-name newsgroup gnus-adaptive-file-suffix))
	     (setq all (nreverse all)))
     (mapcar 'gnus-score-file-name all))))

(defvar gnus-score-file-alist-cache nil)

(defun gnus-score-find-alist (group)
  "Return list of score files for GROUP.
The list is determined from the variable gnus-score-file-alist."
  (let ((alist gnus-score-file-multiple-match-alist)
	score-files)
    ;; if this group has been seen before, return the cached entry
    (if (setq score-files (assoc group gnus-score-file-alist-cache))
	(cdr score-files)		;ensures caching groups with no matches
      ;; handle the multiple match alist
      (while alist
	(and (string-match (car (car alist)) group)
	     (setq score-files
		   (nconc score-files (copy-sequence (cdr (car alist))))))
	(setq alist (cdr alist)))
      (setq alist gnus-score-file-single-match-alist)
      ;; handle the single match alist
      (while alist
	(and (string-match (car (car alist)) group)
	     ;; progn used just in case ("regexp") has no files
	     ;; and score-files is still nil. -sj
	     ;; this can be construed as a "stop searching here" feature :>
	     ;; and used to simplify regexps in the single-alist 
	     (progn
	       (setq score-files
		     (nconc score-files (copy-sequence (cdr (car alist)))))
	       (setq alist nil)))
	(setq alist (cdr alist)))
      ;; cache the score files
      (setq gnus-score-file-alist-cache
	    (cons (cons group score-files) gnus-score-file-alist-cache))
      score-files)))


(defun gnus-possibly-score-headers (&optional trace)
  (let ((func gnus-score-find-score-files-function)
	score-files)
    (and func (not (listp func))
	 (setq func (list func)))
    ;; Go through all the functions for finding score files (or actual
    ;; scores) and add them to a list.
    (setq score-files (gnus-score-find-alist gnus-newsgroup-name))
    (while func
      (and (symbolp (car func))
	   (fboundp (car func))
	   (setq score-files 
		 (nconc score-files (funcall (car func) gnus-newsgroup-name))))
      (setq func (cdr func)))
    (if score-files (gnus-score-headers score-files trace))))

(defun gnus-score-file-name (newsgroup &optional suffix)
  "Return the name of a score file for NEWSGROUP."
  (let ((suffix (or suffix gnus-score-file-suffix)))
    (cond 
     ((or (null newsgroup)
	  (string-equal newsgroup ""))
      ;; The global score file is placed at top of the directory.
      (expand-file-name 
       suffix (or gnus-kill-files-directory "~/News")))
     ((gnus-use-long-file-name 'not-score)
      ;; Append ".SCORE" to newsgroup name.
      (expand-file-name (concat (gnus-newsgroup-savable-name newsgroup)
				"." suffix)
			(or gnus-kill-files-directory "~/News")))
     (t
      ;; Place "SCORE" under the hierarchical directory.
      (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				"/" suffix)
			(or gnus-kill-files-directory "~/News"))))))

(defun gnus-score-search-global-directories (files)
  "Scan all global score directories for score files."
  ;; Set the variable `gnus-internal-global-score-files' to all
  ;; available global score files.
  (interactive (list gnus-global-score-files))
  (let (out)
    (while files
      (if (string-match "/$" (car files))
	  (setq out (nconc (directory-files 
			    (car files) t
			    (concat (gnus-score-file-regexp) "$"))))
	(setq out (cons (car files) out)))
      (setq files (cdr files)))
    (setq gnus-internal-global-score-files out)))

;; Allow redefinition of Gnus functions.

(gnus-ems-redefine)

(provide 'gnus)

;;; gnus.el ends here
