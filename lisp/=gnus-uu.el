;;; gnus-uu.el --- extract, view or save (uu)encoded files from gnus

;; Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@ifi.uio.no>
;; Created: 2 Oct 1993
;; Version: v2.8
;; Last Modified: 1994/06/01
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; All gnus-uu commands start with `C-c C-v'.
;;
;; Short user manual for this package:
;;
;; Type `C-c C-v C-v' to decode and view all articles of the current
;; series. The defaults should be reasonable for most systems.
;;
;; Type `C-c C-v C-i' to toggle interactive mode. When using
;; interactive mode, gnus-uu will which display a buffer that will let
;; you see the suggested commands to be executed.
;;
;; To post an uuencoded file, type `C-c C-v p', which will enter you
;; into a buffer analogous to the one you will get when typing `a'. Do
;; an `M-x describe-mode' in this buffer to get a description of what
;; this buffer lets you do.
;;
;; Read the documentation of the `gnus-uu' dummy function for a more
;; complete description of what this package does and how you can
;; customize it to fit your needs.
;; 
;;
;;
;; History
;;
;; v1.0: First version released Oct 2 1992.
;;
;; v1.1: Changed `C-c C-r' to `C-c C-e' and `C-c C-p' to `C-c C-k'.
;; Changed (setq gnus-exit-group-hook) to (add-hook).  Removed
;; checking for "Re:" for finding parts.
;;
;; v2.2: Fixed handling of currupted archives. Changed uudecoding to
;; an asynchronous process to avoid loading tons of data into emacs
;; buffers. No longer reads articles emacs already have aboard.  Fixed
;; a firmer support for shar files. Made regexp searches for files
;; more convenient. Added `C-c C-l' for editing uucode begin
;; lines. Added multi-system decoder entry point. Added interactive
;; view mode. Added function for decoding and saving all uuencoded
;; articles in the current newsgroup.
;;
;; v2.3: After suggestions I have changed all the gnus-uu key bindings
;; to avoid hogging all the user keys (C-c LETTER). Also added
;; (provide) and fixed some saving stuff. First posted version to
;; gnu.emacs.sources.
;;
;; v2.4: Fixed some more in the save-all category. Automatic fixing of
;; uucode "begin" lines: names on the form of "dir/file" are
;; translated into "dir-file". Added a function for fixing stripped
;; uucode articles. Added binhex save.
;;
;; v2.5: First version copyrighted by FSF. Changed lots of
;; documentation strings.
;;
;; v2.5.1: Added uuencode/posting code to post binary files. 
;;
;; v2.6: Thread support. gnus-uu is now able to decode uuencoded files
;; posted in threads. gnus-uu can also post in threads. I don't know
;; if this ability is of much use - I've never seen anyone post
;; uuencoded files in threads.
;;
;; v2.7: gnus-uu is now able to decode (and view/save) multiple
;; encoded files in one big gulp. Also added pseudo-mime support
;; (users can use metamail to view files), posting uuencoded/mime
;; files and various other bits and pieces.
;;
;; v2.7.1: New functions for decoding/saving threads bound to `C-c
;; C-v C-j'. Handy to save entire threads, not very useful for
;; decoding, as nobody posts encoded files in threads...
;;
;; v2.7.2: New functions for digesting and forwarding articles added
;; on the suggestion of Per Abrahamsen. Also added a function for
;; marking threads. 
;;
;; v2.8: Fixed saving original files in interactive mode. Fixed ask
;; before/save after view. Fixed setting up interactive buffers. Added
;; scanning and rescanning from interactive mode. Added the
;; `gnus-uu-ignore-file-by-name' and `...-by-type' variables to allow
;; users to sift files they don't want to view. At the suggestion of
;; boris@cs.rochester.edu, `C-c C-v C-h' has been undefined to allow
;; users to view list of binding beginning with `C-c C-v'. Fixed
;; viewing with `gnus-uu-asynchronous' set. The
;; "decode-and-save/view-all-articles" functions now accepts the
;; numeric prefix to delimit the maximum number of files to be
;; decoded.

;;; Code: 

(require 'gnus)
(require 'gnuspost)

;; Binding of keys to the gnus-uu functions.

(defvar gnus-uu-ctl-map nil)
(define-prefix-command 'gnus-uu-ctl-map)
(define-key gnus-summary-mode-map "\C-c\C-v" gnus-uu-ctl-map)

(define-key gnus-uu-ctl-map "\C-v" 'gnus-uu-decode-and-view)
(define-key gnus-uu-ctl-map "v" 'gnus-uu-decode-and-save)
(define-key gnus-uu-ctl-map "\C-s" 'gnus-uu-shar-and-view)
(define-key gnus-uu-ctl-map "s" 'gnus-uu-shar-and-save)
(define-key gnus-uu-ctl-map "\C-m" 'gnus-uu-multi-decode-and-view)
(define-key gnus-uu-ctl-map "m" 'gnus-uu-multi-decode-and-save)

(define-key gnus-uu-ctl-map "\C-b" 'gnus-uu-decode-and-show-in-buffer)

(define-key gnus-summary-mode-map "#" 'gnus-uu-mark-article)
(define-key gnus-summary-mode-map "\M-#" 'gnus-uu-unmark-article)
(define-key gnus-uu-ctl-map "\C-u" 'gnus-uu-unmark-all-articles)
(define-key gnus-uu-ctl-map "\C-r" 'gnus-uu-mark-by-regexp)
(define-key gnus-uu-ctl-map "r" 'gnus-uu-mark-by-regexp)
(define-key gnus-uu-ctl-map "t" 'gnus-uu-mark-thread)

(define-key gnus-uu-ctl-map "\M-\C-v" 'gnus-uu-marked-decode-and-view)
(define-key gnus-uu-ctl-map "\M-v" 'gnus-uu-marked-decode-and-save)
(define-key gnus-uu-ctl-map "\M-\C-s" 'gnus-uu-marked-shar-and-view)
(define-key gnus-uu-ctl-map "\M-s" 'gnus-uu-marked-shar-and-save)
(define-key gnus-uu-ctl-map "\M-\C-m" 'gnus-uu-marked-multi-decode-and-view)
(define-key gnus-uu-ctl-map "\M-m" 'gnus-uu-marked-multi-decode-and-save)

(define-key gnus-uu-ctl-map "f" 'gnus-uu-digest-and-forward)
(define-key gnus-uu-ctl-map "\M-f" 'gnus-uu-marked-digest-and-forward)

(define-key gnus-uu-ctl-map "\C-i" 'gnus-uu-toggle-interactive-view)
(define-key gnus-uu-ctl-map "\C-t" 'gnus-uu-toggle-any-variable)

(define-key gnus-uu-ctl-map "\C-l" 'gnus-uu-edit-begin-line)

(define-key gnus-uu-ctl-map "a" 'gnus-uu-decode-and-save-all-unread-articles)
(define-key gnus-uu-ctl-map "w" 'gnus-uu-decode-and-save-all-articles)
(define-key gnus-uu-ctl-map "\C-a" 'gnus-uu-decode-and-view-all-unread-articles)
(define-key gnus-uu-ctl-map "\C-w" 'gnus-uu-decode-and-view-all-articles)

(define-key gnus-uu-ctl-map "\C-j" 'gnus-uu-threaded-multi-decode-and-view)
(define-key gnus-uu-ctl-map "j" 'gnus-uu-threaded-multi-decode-and-save)

(define-key gnus-uu-ctl-map "p" 'gnus-uu-post-news)

;; Dummy function gnus-uu

(defun gnus-uu ()
  "gnus-uu is a package for uudecoding and viewing articles.


Keymap overview:

By default, all gnus-uu keystrokes begin with `C-c C-v'. 

There four decoding commands categories:
All commands for viewing are `C-c C-v C-LETTER'.
All commands for saving are `C-c C-v LETTER'.
All commands for marked viewing are `C-c C-v C-M-LETTER'.
All commands for marked saving are `C-c C-v M-LETTER'.

\\<gnus-summary-mode-map>\\[gnus-uu-decode-and-view]\tDecode and view articles
\\[gnus-uu-decode-and-save]\tDecode and save articles
\\[gnus-uu-shar-and-view]\tUnshar and view articles
\\[gnus-uu-shar-and-save]\tUnshar and save articles
\\[gnus-uu-multi-decode-and-view]\tChoose a decoding method, decode and view articles
\\[gnus-uu-multi-decode-and-save]\tChoose a decoding method, decode and save articles

\\[gnus-uu-threaded-multi-decode-and-view]\tDecode a thread and view
\\[gnus-uu-threaded-multi-decode-and-save]\tDecode a thread and save

\\[gnus-uu-decode-and-show-in-buffer]\tDecode the current article and view the result in a buffer
\\[gnus-uu-edit-begin-line]\tEdit the 'begin' line of an uuencoded article

\\[gnus-uu-decode-and-save-all-unread-articles]\tDecode and save all unread articles
\\[gnus-uu-decode-and-save-all-articles]\tDecode and save all articles
\\[gnus-uu-decode-and-view-all-unread-articles]\tDecode and view all unread articles
\\[gnus-uu-decode-and-view-all-articles]\tDecode and view all articles

\\[gnus-uu-digest-and-forward]\tDigest and forward a series of articles
\\[gnus-uu-marked-digest-and-forward]\tDigest and forward all marked articles

\\[gnus-uu-mark-article]\tMark the current article for decoding
\\[gnus-uu-unmark-article]\tUnmark the current article
\\[gnus-uu-unmark-all-articles]\tUnmark all articles
\\[gnus-uu-mark-by-regexp]\tMark articles for decoding by regexp
\\[gnus-uu-mark-thread]\tMark articles in this thread
\\[gnus-uu-marked-decode-and-view]\tDecode and view marked articles
\\[gnus-uu-marked-decode-and-save]\tDecode and save marked articles
\\[gnus-uu-marked-shar-and-view]\tUnshar and view marked articles
\\[gnus-uu-marked-shar-and-save]\tUnshar and save marked articles
\\[gnus-uu-marked-multi-decode-and-view]\tChoose decoding method, decode and view marked articles
\\[gnus-uu-marked-multi-decode-and-save]\tChoose decoding method, decode and save marked articles

\\[gnus-uu-toggle-asynchronous]\tToggle asynchronous viewing mode
\\[gnus-uu-toggle-query]\tToggle whether to ask before viewing a file
\\[gnus-uu-toggle-always-ask]\tToggle whether to ask to save a file after viewing
\\[gnus-uu-toggle-kill-carriage-return]\tToggle whether to strip trailing carriage returns
\\[gnus-uu-toggle-interactive-view]\tToggle whether to use interactive viewing mode
\\[gnus-uu-toggle-correct-stripped-articles]\tToggle whether to 'correct' articles
\\[gnus-uu-toggle-view-with-metamail]\tToggle whether to use metamail for viewing 
\\[gnus-uu-toggle-any-variable]\tToggle any of the things above

\\[gnus-uu-post-news]\tPost an uuencoded article

Function description:

`gnus-uu-decode-and-view' will try to find all articles in the same
series, uudecode them and view the resulting file(s).

gnus-uu guesses what articles are in the series according to the
following simplish rule: The subjects must be (nearly) identical,
except for the last two numbers of the line. (Spaces are largely
ignored, however.)

For example: If you choose a subject called 
  \"cat.gif (2/3)\"
gnus-uu will find all the articles that matches
  \"^cat.gif ([0-9]+/[0-9]+).*$\".  

Subjects that are nonstandard, like 
  \"cat.gif (2/3) Part 6 of a series\", 
will not be properly recognized by any of the automatic viewing
commands, and you have to mark the articles manually with '#'.

`gnus-uu-decode-and-save' will do the same as
`gnus-uu-decode-and-view', except that it will not display the
resulting file, but save it instead.

`gnus-uu-shar-and-view' and `gnus-uu-shar-and-save' are the \"shar\"
equivalents to the uudecode functions. Instead of feeding the articles
to uudecode, they are run through /bin/sh. Most shar files can be
viewed and/or saved with the normal uudecode commands, which is much
safer, as no foreign code is run.

Instead of having windows popping up automatically, it can be handy to
view files interactivly, especially when viewing archives. Use
`gnus-uu-toggle-interactive-mode' to toggle interactive mode.

`gnus-uu-mark-article' marks an article for later
decoding/unsharing/saving/viewing. The files will be decoded in the
sequence they were marked. To decode the files after you've marked the
articles you are interested in, type the corresponding key strokes as
the normal decoding commands, but put a `M-' in the last
keystroke. For instance, to perform a standard uudecode and view, you
would type `C-c C-v C-v'. To perform a marked uudecode and view, say
`C-v C-v M-C-v'. All the other view and save commands are handled the
same way; marked uudecode and save is then `C-c C-v M-v'.

`gnus-uu-unmark-article' will remove the mark from a previosly marked
article.

`gnus-uu-unmark-all-articles' will remove the mark from all marked
articles.

`gnus-uu-mark-by-regexp' will prompt for a regular expression and mark
all articles matching that regular expression.

`gnus-uu-mark-thread' will mark all articles downward in the current
thread.

There's an additional way to reach the decoding functions to make
future expansions easier: `gnus-uu-multi-decode-and-view' and the
corresponding save, marked view and marked save functions. You will be
prompted for a decoding method, like uudecode, shar, binhex or plain
save. Note that methods like binhex and save doesn't have view modes;
even if you issue a view command (`C-c C-v C-m' and \"binhex\"),
gnus-uu will just save the resulting binhex file.

`gnus-uu-decode-and-show-in-buffer' will decode the current article
and display the results in an emacs buffer. This might be useful if
there's jsut some text in the current article that has been uuencoded
by some perverse poster.

`gnus-uu-decode-and-save-all-articles' looks at all the articles in
the current newsgroup and tries to uudecode everything it can
find. The user will be prompted for a directory where the resulting
files (if any) will be
saved. `gnus-uu-decode-and-save-unread-articles' does only checks
unread articles. 

`gnus-uu-decode-and-view-all-articles' does the same as the function
above, only viewing files instead of saving them. 

`gnus-uu-edit-begin-line' lets you edit the begin line of an uuencoded
file in the current article. Useful to change a corrupted begin line.


When using the view commands, `gnus-uu-decode-and-view' for instance,
gnus-uu will (normally, see below) try to view the file according to
the rules given in `gnus-uu-default-view-rules' and
`gnus-uu-user-view-rules'. If it recognizes the file, it will display
it immediately. If the file is some sort of archive, gnus-uu will
attempt to unpack the archive and see if any of the files in the
archive can be viewed. For instance, if you have a gzipped tar file
\"pics.tar.gz\" containing the files \"pic1.jpg\" and \"pic2.gif\",
gnus-uu will uncompress and detar the main file, and then view the two
pictures. This unpacking process is recursive, so if the archive
contains archives of archives, it'll all be unpacked.

If the view command doesn't recognise the file type, or can't view it
because you don't have the viewer, or can't view *any* of the files in
the archive, the user will be asked if she wishes to have the file
saved somewhere. Note that if the decoded file is an archive, and
gnus-uu manages to view some of the files in the archive, it won't
tell the user that there were some files that were unviewable. Try
interactive view for a different approach.


Note that gnus-uu adds a function to `gnus-exit-group-hook' to clear
the list of marked articles and check for any generated files that
might have escaped deletion if the user typed `C-g' during viewing.


`gnus-uu-toggle-asynchronous' toggles the `gnus-uu-asynchronous'
variable.

`gnus-uu-toggle-query' toggles the `gnus-uu-ask-before-view'
variable.

`gnus-uu-toggle-always-ask' toggles the `gnus-uu-view-and-save'
variable.

`gnus-uu-toggle-kill-carriage-return' toggles the
`gnus-uu-kill-carriage-return' variable.

`gnus-uu-toggle-interactive-view' toggles interactive mode. If it is
turned on, gnus-uu won't view files immediately, but will give you a
buffer with the default commands and files and let you edit the
commands and execute them at leisure.

`gnus-uu-toggle-correct-stripped-articles' toggles whether to check
and correct uuencoded articles that may have had trailing spaces
stripped by mailers.

`gnus-uu-toggle-view-with-metamail' toggles whether to skip the
gnus-uu viewing methods and just guess at an content-type based on the
file name suffix and feed it to metamail.

`gnus-uu-toggle-any-variable' is an interface to the toggle commands
listed above.


Customization

   Rule Variables

   gnus-uu uses \"rule\" variables to decide how to view a file. All
   these variables are of the form
  
      (list '(regexp1 command2)
            '(regexp2 command2)
            ...)

   `gnus-uu-user-view-rules'
     This variable is consulted first when viewing files. If you wish
     to use, for instance, sox to convert an .au sound file, you could
     say something like:

       (setq gnus-uu-user-view-rules
         (list '(\"\\\\.au$\" \"sox %s -t .aiff > /dev/audio\")))

   `gnus-uu-user-view-rules-end'
     This variable is consulted if gnus-uu couldn't make any matches
     from the user and default view rules.

   `gnus-uu-user-interactive-view-rules'
     This is the variable used instead of `gnus-uu-user-view-rules'
     when in interactive mode.

   `gnus-uu-user-interactive-view-rules-end'
     This variable is used instead of `gnus-uu-user-view-rules-end'
     when in interactive mode.

   `gnus-uu-user-archive-rules`
     This variable can be used to say what comamnds should be used to
     unpack archives.

   
   Other Variables

   `gnus-uu-ignore-files-by-name'
     Files with name matching this regular expression won't be viewed.

   `gnus-uu-ignore-files-by-type'
     Files with a MIME type matching this variable won't be viewed.
     Note that gnus-uu tries to guess what type the file is based on
     the name. gnus-uu is not a MIME package, so this is slightly
     kludgy.

   `gnus-uu-tmp-dir'
     Where gnus-uu does its work.

   `gnus-uu-do-not-unpack-archives'
     Non-nil means that gnus-uu won't peek inside archives looking for
     files to dispay.

   `gnus-uu-view-and-save'
     Non-nil means that the user will always be asked to save a file
     after viewing it.

   `gnus-uu-asynchronous' 
     Non-nil means that files will be viewed asynchronously.  This can
     be useful if you're viewing long .mod files, for instance, which
     often takes several minutes. Note, however, that since gnus-uu
     doesn't ask, and if you are viewing an archive with lots of
     viewable files, you'll get them all up more or less at once,
     which can be confusing, to say the least. To get gnus-uu to ask
     you before viewing a file, set the `gnus-uu-ask-before-view' 
     variable.

   `gnus-uu-ask-before-view'
     Non-nil means that gnus-uu will ask you before viewing each file

   `gnus-uu-ignore-default-view-rules'
     Non-nil means that gnus-uu will ignore the default viewing rules.

   `gnus-uu-ignore-default-archive-rules'
     Non-nil means that gnus-uu will ignore the default archive
     unpacking commands.

   `gnus-uu-kill-carriage-return'
     Non-nil means that gnus-uu will strip all carriage returns from
     articles.

   `gnus-uu-unmark-articles-not-decoded'
     Non-nil means that gnus-uu will mark articles that were
     unsuccessfully decoded as unread.

   `gnus-uu-output-window-height'
     This variable says how tall the output buffer window is to be
     when using interactive view mode.

   `gnus-uu-correct-stripped-uucode'
     Non-nil means that gnus-uu will *try* to fix uuencoded files that
     have had traling spaces deleted.

   `gnus-uu-use-interactive-view'
     Non-nil means that gnus-uu will use interactive viewing mode.

   `gnus-uu-view-with-metamail'
     Non-nil means that gnus-uu will ignore the viewing commands
     defined by the rule variables and just fudge a MIME content type
     based on the file name. The result will be fed to metamail for
     viewing.

   `gnus-uu-save-in-digest'
     Non-nil means that gnus-uu, when asked to save without decoding,
     will save in digests.  If this variable is nil, gnus-uu will just
     save everything in a file without any embellishments. The
     digesting almost conforms to RFC1153 - no easy way to specify any
     meaningful volume and issue numbers were found, so I simply
     dropped them.

   `gnus-uu-post-include-before-composing'
     Non-nil means that gnus-uu will ask for a file to encode before
     you compose the article.  If this variable is t, you can either
     include an encoded file with \\<gnus-uu-post-reply-mode-map>\\[gnus-uu-post-insert-binary-in-article] or have one included for you when you 
     post the article.

   `gnus-uu-post-length'
     Maximum length of an article.  The encoded file will be split
     into how many articles it takes to post the entire file.

   `gnus-uu-post-threaded'
     Non-nil means that gnus-uu will post the encoded file in a
     thread.  This may not be smart, as no other decoder I have seen
     are able to follow threads when collecting uuencoded
     articles. (Well, I have seen one package that does that -
     gnus-uu, but somehow, I don't think that counts...) Default is
     nil.

   `gnus-uu-post-separate-description'
     Non-nil means that the description will be posted in a separate
     article.  The first article will typically be numbered (0/x). If
     this variable is nil, the description the user enters will be
     included at the beginning of the first article, which will be
     numbered (1/x). Default is t.
"
  (interactive)
  )

;; Default viewing action rules

(defvar gnus-uu-default-view-rules 
  (list 
   '("\\.\\(jpe?g\\|gif\\|tiff?\\|p[pgb]m\\|xwd\\|xbm\\|pcx\\)$" "xv")
   '("\\.tga$" "tgatoppm %s | xv -")
   '("\\.te?xt$\\|\\.doc$\\|read.*me" "xterm -e less")
   '("\\.\\(wav\\|aiff\\|hcom\\|u[blw]\\|s[bfw]\\|voc\\|smp\\)$" 
     "sox -v .5 %s -t .au -u - > /dev/audio")
   '("\\.au$" "cat %s > /dev/audio")
   '("\\.mod$" "str32")
   '("\\.ps$" "ghostview")
   '("\\.dvi$" "xdvi")
   '("\\.[1-6]$" "xterm -e man -l")
   '("\\.html$" "xmosaic")
   '("\\.mpe?g$" "mpeg_play")
   '("\\.\\(flc\\|fli\\|rle\\|iff\\|pfx\\|avi\\|sme\\|rpza\\|dl\\|qt\\|rsrc\\)$" "xanim")
   '("\\.\\(tar\\|arj\\|zip\\|zoo\\|arc\\|gz\\|Z\\|lzh\\|ar\\|lha\\)$" 
     "gnus-uu-archive"))

  "Default actions to be taken when the user asks to view a file.  
To change the behaviour, you can either edit this variable or set
`gnus-uu-user-view-rules' to something useful.

For example:

To make gnus-uu use 'xli' to display JPEG and GIF files, put the
following in your .emacs file

  (setq gnus-uu-user-view-rules (list '(\"jpg$\\\\|gif$\" \"xli\")))

Both these variables are lists of lists with two string elements. The
first string is a regular expression. If the file name matches this
regular expression, the command in the second string is executed with
the file as an argument.

If the command string contains \"%s\", the file name will be inserted
at that point in the command string. If there's no \"%s\" in the
command string, the file name will be appended to the command string
before executing.

There are several user variables to tailor the behaviour of gnus-uu to
your needs. First we have `gnus-uu-user-view-rules', which is the
variable gnus-uu first consults when trying to decide how to view a
file. If this variable contains no matches, gnus-uu examines the
default rule vaiable provided in this package. If gnus-uu finds no
match here, it uses `gnus-uu-user-view-rules-end' to try to make a
match.

Unless, of course, you are using the interactive view mode. Then
`gnus-uu-user-interactive-view-rules' and
`gnus-uu-user-interactive-view-rules-end' will be used instead.")

(defvar gnus-uu-user-view-rules nil 
  "Variable detailing what actions are to be taken to view a file.
See the documentation on the `gnus-uu-default-view-rules' variable for 
details.")

(defvar gnus-uu-user-view-rules-end nil
  "Variable saying what actions are to be taken if no rule matched the file name.
See the documentation on the `gnus-uu-default-view-rules' variable for 
details.")

(defvar gnus-uu-user-interactive-view-rules nil
  "Variable detailing what actions are to be taken to view a file when using interactive mode.
See the documentation on the `gnus-uu-default-view-rules' variable for 
details.")

(defvar gnus-uu-user-interactive-view-rules-end nil
  "Variable saying what actions are to be taken if no rule matched the file name when using interactive mode.
See the documentation on the `gnus-uu-default-view-rules' variable for 
details.")

(defvar gnus-uu-default-interactive-view-rules-begin
  (list
   '("\\.te?xt$\\|\\.doc$\\|read.*me\\|\\.c?$\\|\\.h$\\|\\.bat$\\|\\.asm$\\|makefile" "cat %s | sed s/\r//g")
   '("\\.pas$" "cat %s | sed s/\r//g")
   ))

(defvar gnus-uu-default-interactive-view-rules-end
  (list
   '(".*" "file")))

;; Default unpacking commands

(defvar gnus-uu-default-archive-rules 
  (list '("\\.tar$" "tar xf")
	'("\\.zip$" "unzip -o")
	'("\\.ar$" "ar x")
	'("\\.arj$" "unarj x")
	'("\\.zoo$" "zoo -e")
	'("\\.\\(lzh\\|lha\\)$" "lha x")
	'("\\.Z$" "uncompress")
	'("\\.gz$" "gunzip")
	'("\\.arc$" "arc -x"))
  )

(defvar gnus-uu-destructive-archivers 
  (list "uncompress" "gunzip"))

(defvar gnus-uu-user-archive-rules nil
  "A list that can be set to override the default archive unpacking commands.
To use, for instance, 'untar' to unpack tar files and 'zip -x' to
unpack zip files, say the following:
  (setq gnus-uu-user-archive-rules 
    (list '(\"\\\\.tar$\" \"untar\")
          '(\"\\\\.zip$\" \"zip -x\")))")

(defvar gnus-uu-ignore-files-by-name nil
  "A regular expression saying what files should not be viewed based on name.
If, for instance, you want gnus-uu to ignore all .au and .wav files, 
you could say something like

  (setq gnus-uu-ignore-files-by-name \"\\\\.au$\\\\|\\\\.wav$\")

Note that this variable can be used in conjunction with the
`gnus-uu-ignore-files-by-type' variable.")

(defvar gnus-uu-ignore-files-by-type nil
  "A regular expression saying what files that shouldn't be viewed, based on MIME file type.
If, for instance, you want gnus-uu to ignore all audio files and all mpegs, 
you could say something like

  (setq gnus-uu-ignore-files-by-type \"audio/\\\\|video/mpeg\")

Note that this variable can be used in conjunction with the
`gnus-uu-ignore-files-by-name' variable.")

;; Pseudo-MIME support

(defconst gnus-uu-ext-to-mime-list
  (list '("\\.gif$" "image/gif")
	'("\\.jpe?g$" "image/jpeg")
	'("\\.tiff?$" "image/tiff")
	'("\\.xwd$" "image/xwd")
	'("\\.pbm$" "image/pbm")
	'("\\.pgm$" "image/pgm")
	'("\\.ppm$" "image/ppm")
	'("\\.xbm$" "image/xbm")
	'("\\.pcx$" "image/pcx")
	'("\\.tga$" "image/tga")
	'("\\.ps$" "image/postscript")
	'("\\.fli$" "video/fli")
	'("\\.wav$" "audio/wav")
	'("\\.aiff$" "audio/aiff")
	'("\\.hcom$" "audio/hcom")
	'("\\.voc$" "audio/voc")
	'("\\.smp$" "audio/smp")
	'("\\.mod$" "audio/mod")
	'("\\.dvi$" "image/dvi")
	'("\\.mpe?g$" "video/mpeg")
	'("\\.au$" "audio/basic")
	'("\\.\\(te?xt\\|doc\\|c\\|h\\)$" "text/plain")
	'("\\.\\(c\\|h\\)$" "text/source")
	'("read.*me" "text/plain")
	'("\\.html$" "text/html")
	'("\\.bat$" "text/bat")
	'("\\.[1-6]$" "text/man")
	'("\\.flc$" "video/flc")
	'("\\.rle$" "video/rle")
	'("\\.pfx$" "video/pfx")
	'("\\.avi$" "video/avi")
	'("\\.sme$" "video/sme")
	'("\\.rpza$" "video/prza")
	'("\\.dl$" "video/dl")
	'("\\.qt$" "video/qt")
	'("\\.rsrc$" "video/rsrc")
	'("\\..*$" "unknown/unknown")))

;; Various variables users may set 

(defvar gnus-uu-tmp-dir "/tmp/" 
  "Variable saying where gnus-uu is to do its work.
Default is \"/tmp/\".")

(defvar gnus-uu-do-not-unpack-archives nil 
  "Non-nil means that gnus-uu won't peek inside archives looking for files to dispay. 
Default is nil.")

(defvar gnus-uu-view-and-save nil 
  "Non-nil means that the user will always be asked to save a file after viewing it.
If the variable is nil, the suer will only be asked to save if the
viewing is unsuccessful. Default is nil.")

(defvar gnus-uu-asynchronous nil
  "Non-nil means that files will be viewed asynchronously.
Default is nil.")

(defvar gnus-uu-ask-before-view nil
  "Non-nil means that gnus-uu will ask you before viewing each file. 
Especially useful when `gnus-uu-asynchronous' is set. Default is
nil.")

(defvar gnus-uu-ignore-default-view-rules nil
  "Non-nil means that gnus-uu will ignore the default viewing rules.
Only the user viewing rules will be consulted. Default is nil.")

(defvar gnus-uu-ignore-default-archive-rules nil 
  "Non-nil means that gnus-uu will ignore the default archive unpacking commands.  
Only the user unpacking commands will be consulted. Default is nil.")

(defvar gnus-uu-kill-carriage-return t
  "Non-nil means that gnus-uu will strip all carriage returns from articles.
Default is t.")

(defvar gnus-uu-view-with-metamail nil
  "Non-nil means that files will be viewed with metamail.
The gnus-uu viewing functions will be ignored and gnus-uu will try
to guess at a content-type based on file name suffixes. Default
it nil.")

(defvar gnus-uu-unmark-articles-not-decoded nil
  "Non-nil means that gnus-uu will mark articles that were unsuccessfully decoded as unread. 
Default is nil.")

(defvar gnus-uu-output-window-height 20 
  "This variable says how tall the output buffer window is to be when using interactive view mode. 
Change it at your convenience. Default is 20.")

(defvar gnus-uu-correct-stripped-uucode nil
  "Non-nil means that gnus-uu will *try* to fix uuencoded files that have had traling spaces deleted. 
Default is nil.")

(defvar gnus-uu-use-interactive-view nil
  "Non-nil means that gnus-uu will use interactive viewing mode.
Gnus-uu will create a special buffer where the user may choose
interactively which files to view and how. Default is nil.")

(defvar gnus-uu-save-in-digest nil
  "Non-nil means that gnus-uu, when asked to save without decoding, will save in digests.
If this variable is nil, gnus-uu will just save everything in a 
file without any embellishments. The digesting almost conforms to RFC1153 -
no easy way to specify any meaningful volume and issue numbers were found, 
so I simply dropped them.")


;; Internal variables

(defconst gnus-uu-begin-string "^begin[ \t]+[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defconst gnus-uu-end-string "^end[ \t]*$")

(defconst gnus-uu-body-line "^M")
(let ((i 61))
  (while (> (setq i (1- i)) 0)
    (setq gnus-uu-body-line (concat gnus-uu-body-line "[^a-z]")))
  (setq gnus-uu-body-line (concat gnus-uu-body-line ".?$")))

;"^M.............................................................?$"

(defconst gnus-uu-shar-begin-string "^#! */bin/sh")

(defvar gnus-uu-shar-file-name nil)
(defconst gnus-uu-shar-name-marker "begin [0-7][0-7][0-7][ \t]+\\(\\(\\w\\|\\.\\)*\\b\\)")
(defvar gnus-uu-shar-directory nil)

(defvar gnus-uu-file-name nil)
(defvar gnus-uu-list-of-files-decoded nil)
(defconst gnus-uu-uudecode-process nil)

(defvar gnus-uu-interactive-file-list nil)
(defvar gnus-uu-marked-article-list nil)
(defvar gnus-uu-generated-file-list nil)
(defvar gnus-uu-work-dir nil)

(defconst gnus-uu-interactive-buffer-name "*gnus-uu interactive*")
(defconst gnus-uu-output-buffer-name "*Gnus UU Output*")
(defconst gnus-uu-result-buffer "*Gnus UU Result Buffer*")

(defconst gnus-uu-highest-article-number 1)

;; Interactive functions

;; UUdecode and view

(defun gnus-uu-decode-and-view ()
  "UUdecodes and 'views' (if possible) the resulting file.
'Viewing' can be any action at all, as defined in the
`gnus-uu-file-action-list' variable. Running 'xv' on gifs and 'cat
>/dev/audio' on au files are popular actions. If the file can't be
viewed, the user is asked if she would like to save the file instead."
  (interactive) 
  (gnus-uu-decode-and-view-or-save t nil))

(defun gnus-uu-decode-and-save ()
  "Decodes and saves the resulting file."
  (interactive)
  (gnus-uu-decode-and-view-or-save nil nil))

(defun gnus-uu-marked-decode-and-view ()
  "Decodes and views articles marked.
The marked equivalent to `gnus-uu-decode-and-view'."
  (interactive)
  (gnus-uu-decode-and-view-or-save t t))

(defun gnus-uu-marked-decode-and-save ()
  "Decodes and saves articles marked.
The marked equivalent to `gnus-uu-decode-and-save'."
  (interactive)
  (gnus-uu-decode-and-view-or-save nil t))
      

;; Unshar and view

(defun gnus-uu-shar-and-view ()
  "Unshars and views articles.
The shar equivalent of `gnus-uu-decode-and-view'."
  (interactive)
  (gnus-uu-unshar-and-view-or-save t nil))

(defun gnus-uu-shar-and-save ()
  "Unshars and saves files.
The shar equivalent to `gnus-uu-decode-and-save'."
  (interactive)
  (gnus-uu-unshar-and-view-or-save nil nil))

(defun gnus-uu-marked-shar-and-view ()
  "Unshars and views articles marked.
The marked equivalent to `gnus-uu-shar-and-view'."
  (interactive)
  (gnus-uu-unshar-and-view-or-save t t))

(defun gnus-uu-marked-shar-and-save ()
  "Unshars and saves articles marked.
The marked equivalent to `gnus-uu-shar-and-save'."
  (interactive)
  (gnus-uu-unshar-and-view-or-save nil t))

;; Threaded decode

(defun gnus-uu-threaded-decode-and-view ()
  "Decodes and saves the resulting file."
  (interactive)
  (gnus-uu-threaded-decode-and-view-or-save t))

(defun gnus-uu-threaded-decode-and-save ()
  "Decodes and saves the resulting file."
  (interactive)
  (gnus-uu-threaded-decode-and-view-or-save nil))

(defun gnus-uu-threaded-multi-decode-and-view ()
  "Decodes and saves the resulting file."
  (interactive)
  (gnus-uu-threaded-multi-decode-and-view-or-save t))

(defun gnus-uu-threaded-multi-decode-and-save ()
  "Decodes and saves the resulting file."
  (interactive)
  (gnus-uu-threaded-multi-decode-and-view-or-save nil))

(defun gnus-uu-threaded-decode-and-view-or-save (&optional view)
  (gnus-uu-unmark-all-articles)
  (gnus-uu-mark-thread)
  (gnus-uu-decode-and-view-or-save view t))

(defun gnus-uu-threaded-multi-decode-and-view-or-save (view)
  (let (type)
    (message "Decode type: [u]udecode, (s)har, s(a)ve, (b)inhex: ")
    (setq type (read-char))
    (if (not (or (= type ?u) (= type ?s) (= type ?b) (= type ?a)))
	(error "No such decoding method '%c'" type))
    
    (gnus-uu-unmark-all-articles)
    (gnus-uu-mark-thread)

    (if (= type ?\r) (setq type ?u))
    (cond ((= type ?u) (gnus-uu-decode-and-view-or-save view t))
	  ((= type ?s) (gnus-uu-unshar-and-view-or-save view t))
	  ((= type ?b) (gnus-uu-binhex-and-save view t))
	  ((= type ?a) (gnus-uu-save-articles view t)))))
    

;; Toggle commands      

(defun gnus-uu-toggle-asynchronous ()
  "This function toggles asynchronous viewing."
  (interactive)
  (if (setq gnus-uu-asynchronous (not gnus-uu-asynchronous))
      (message "gnus-uu will now view files asynchronously")
    (message "gnus-uu will now view files synchronously")))

(defun gnus-uu-toggle-query ()
  "This function toggles whether to ask before viewing or not."
  (interactive)
  (if (setq gnus-uu-ask-before-view (not gnus-uu-ask-before-view))
      (message "gnus-uu will now ask before viewing")
    (message "gnus-uu will now view without asking first")))

(defun gnus-uu-toggle-always-ask ()
  "This function toggles whether to always ask to save a file after viewing."
  (interactive)
  (if (setq gnus-uu-view-and-save (not gnus-uu-view-and-save))
      (message "gnus-uu will now ask to save the file after viewing")
    (message "gnus-uu will now not ask to save after successful viewing")))

(defun gnus-uu-toggle-interactive-view ()
  "This function toggles whether to use interactive view."
  (interactive)
  (if (setq gnus-uu-use-interactive-view (not gnus-uu-use-interactive-view))
      (message "gnus-uu will now use interactive view")
    (message "gnus-uu will now use non-interactive view")))

(defun gnus-uu-toggle-unmark-undecoded ()
  "This function toggles whether to unmark articles not decoded."
  (interactive)
  (if (setq gnus-uu-unmark-articles-not-decoded 
	    (not gnus-uu-unmark-articles-not-decoded))
      (message "gnus-uu will now unmark articles not decoded")
    (message "gnus-uu will now not unmark articles not decoded")))

(defun gnus-uu-toggle-kill-carriage-return ()
  "This function toggles the stripping of carriage returns from the articles."
  (interactive)
  (if (setq gnus-uu-kill-carriage-return (not gnus-uu-kill-carriage-return))
      (message "gnus-uu will now strip carriage returns")
    (message "gnus-uu won't strip carriage returns")))

(defun gnus-uu-toggle-view-with-metamail ()
  "This function toggles whether to view files with metamail."
  (interactive)
  (if (setq gnus-uu-view-with-metamail (not gnus-uu-view-with-metamail))
      (message "gnus-uu will now view with metamail")
    (message "gnus-uu will now view with the gnus-uu viewing functions")))

(defun gnus-uu-toggle-correct-stripped-uucode ()
  "This function toggles whether to correct stripped uucode."
  (interactive)
  (if (setq gnus-uu-correct-stripped-uucode 
	    (not gnus-uu-correct-stripped-uucode))
      (message "gnus-uu will now correct stripped uucode")
    (message "gnus-uu won't check and correct stripped uucode")))

(defun gnus-uu-toggle-any-variable ()
  "This function ask what variable the user wants to toggle."
  (interactive)
  (let (rep)
    (message "(a)sync, (q)uery, (p)ask, (k)ill CR, (i)nteract, (u)nmark, (c)orrect, (m)eta")
    (setq rep (read-char))
    (if (= rep ?a)
	(gnus-uu-toggle-asynchronous))
    (if (= rep ?q)
	(gnus-uu-toggle-query))
    (if (= rep ?p)
	(gnus-uu-toggle-always-ask))
    (if (= rep ?k)
	(gnus-uu-toggle-kill-carriage-return))
    (if (= rep ?u)
	(gnus-uu-toggle-unmark-undecoded))
    (if (= rep ?c)
	(gnus-uu-toggle-correct-stripped-uucode))
    (if (= rep ?m)
	(gnus-uu-toggle-view-with-metamail))
    (if (= rep ?i)
	(gnus-uu-toggle-interactive-view))))


;; Misc interactive functions

(defun gnus-uu-decode-and-show-in-buffer ()
  "Uudecodes the current article and displays the result in a buffer.
Might be useful if someone has, for instance, some text uuencoded in
their sigs. (Stranger things have happened.)"
  (interactive)
  (gnus-uu-initialize)
  (let ((uu-buffer (get-buffer-create gnus-uu-output-buffer-name))
	file-name)
    (save-excursion
      (and 
       (gnus-summary-select-article)
       (gnus-uu-grab-articles (list gnus-current-article) 
			      'gnus-uu-uustrip-article-as)
       (setq file-name (concat gnus-uu-work-dir gnus-uu-file-name))
       (progn
	 (save-excursion
	   (set-buffer uu-buffer)
	   (erase-buffer)
	   (insert-file-contents file-name))
	 (set-window-buffer (get-buffer-window gnus-article-buffer) 
			    uu-buffer)
	 (message "Showing file %s in buffer" file-name)
	 (delete-file file-name))))))

(defun gnus-uu-edit-begin-line ()
  "Edit the begin line of the current article."
  (interactive)
  (let ((buffer-read-only nil)
	begin b)
    (save-excursion
      (gnus-summary-select-article)
      (set-buffer gnus-article-buffer)
      (goto-line 1)
      (if (not (re-search-forward "begin " nil t))
	  (error "No begin line in the current article")
	(beginning-of-line)
	(setq b (point))
	(end-of-line)
	(setq begin (buffer-substring b (point)))
	(setq begin (read-string "" begin))
	(setq buffer-read-only nil)
	(delete-region b (point))
	(insert-string begin)))))


;; Multi functions

(defun gnus-uu-multi-decode-and-view ()
  "Choose a method of decoding and then decode and view.
This function lets the user decide what method to use for decoding.
Other than that, it's equivalent to the other decode-and-view
functions."
  (interactive)
  (gnus-uu-multi-decode-and-view-or-save t nil))

(defun gnus-uu-multi-decode-and-save ()
  "Choose a method of decoding and then decode and save.
This function lets the user decide what method to use for decoding.
Other than that, it's equivalent to the other decode-and-save 
functions."
  (interactive)
  (gnus-uu-multi-decode-and-view-or-save nil nil))

(defun gnus-uu-marked-multi-decode-and-view ()
  "Choose a method of decoding and then decode and view the marked articles.
This function lets the user decide what method to use for decoding.
Other than that, it's equivalent to the other marked decode-and-view 
functions."
  (interactive)
  (gnus-uu-multi-decode-and-view-or-save t t))

(defun gnus-uu-marked-multi-decode-and-save ()
  "Choose a method of decoding and then decode and save the marked articles.
This function lets the user decide what method to use for decoding.
Other than that, it's equivalent to the other marked decode-and-save 
functions."
  (interactive)
  (gnus-uu-multi-decode-and-view-or-save t t))

(defun gnus-uu-multi-decode-and-view-or-save (view marked)
  (let (type)
    (message "[u]udecode, (s)har, s(a)ve, (b)inhex: ")
    (setq type (read-char))
    (if (= type ?\r) (setq type ?u))
    (cond ((= type ?u) (gnus-uu-decode-and-view-or-save view marked))
	  ((= type ?s) (gnus-uu-unshar-and-view-or-save view marked))
	  ((= type ?b) (gnus-uu-binhex-and-save view marked))
	  ((= type ?a) (gnus-uu-save-articles view marked))
	  (t (error "Unknown decode method '%c'." type)))))


;; "All articles" commands

(defconst gnus-uu-rest-of-articles nil)
(defvar gnus-uu-current-save-dir nil)

(defun gnus-uu-decode-and-view-all-articles (arg &optional unread)
  "Try to decode all articles and view the result.
ARG delimits the number of files to be decoded."
  (interactive "p")
  (if (not (setq gnus-uu-marked-article-list 
		 (nreverse (gnus-uu-get-list-of-articles 
			    "^." nil unread t))))
      (error "No%s articles to be decoded" (if unread " unread" "")))
  (gnus-uu-decode-and-view-or-save t t nil (if (> arg 1) arg nil)))

(defun gnus-uu-decode-and-view-all-unread-articles (arg)
  "Try to decode all unread articles and view the result.
ARG delimits the number of files to be decoded."
  (interactive "p")
  (gnus-uu-decode-and-view-all-articles arg t))

(defun gnus-uu-decode-and-save-all-unread-articles (arg)
  "Try to decode all unread articles and saves the result.
This function reads all unread articles in the current group and sees
whether it can uudecode the articles. The user will be prompted for an
directory to put the resulting (if any) files.
ARG delimits the number of files to be decoded."
  (interactive "p")
  (gnus-uu-decode-and-save-articles arg t t))

(defun gnus-uu-decode-and-save-all-articles (arg)
  "Try to decode all articles and saves the result.
Does the same as `gnus-uu-decode-and-save-all-unread-articles', except
that it grabs all articles visible, unread or not.
ARG delimits the number of files to be decoded."
  (interactive "p")
  (gnus-uu-decode-and-save-articles arg nil t))

(defun gnus-uu-decode-and-save-articles (arg &optional unread unmark)
  (let (dir)
    (if (not (setq gnus-uu-marked-article-list 
		   (nreverse (gnus-uu-get-list-of-articles 
			      "^." nil unread t))))
	(error "No%s articles to be decoded." (if unread " unread" ""))
      (setq dir (gnus-uu-read-directory "Where do you want the files? "))
      (gnus-uu-decode-and-view-or-save nil t dir (if (> arg 1) arg nil))
      (message "Saved."))))


;; Work functions

; All the interactive uudecode/view/save/marked functions are interfaces
; to this function, which does the rest.
(defun gnus-uu-decode-and-view-or-save (view marked &optional save-dir limit)
  (gnus-uu-initialize)
  (let (decoded)
    (save-excursion
      (if (gnus-uu-decode-and-strip nil marked limit)
	  (progn
	    (setq decoded t)
	    (if view 
		(gnus-uu-view-directory gnus-uu-work-dir 
					gnus-uu-use-interactive-view)
	      (gnus-uu-save-directory gnus-uu-work-dir save-dir save-dir)
	      (gnus-uu-check-for-generated-files)))))

    (gnus-uu-summary-next-subject)

    (if (and gnus-uu-use-interactive-view view decoded)
	(gnus-uu-do-interactive))

    (if (or (not view) (not gnus-uu-use-interactive-view) (not decoded))
	(gnus-uu-clean-up))))

; Unshars and views/saves marked/unmarked articles.
(defun gnus-uu-unshar-and-view-or-save (view marked)
  (gnus-uu-initialize)
  (let (tar-file files decoded)
    (save-excursion
      (setq gnus-uu-shar-directory 
	    (make-temp-name (concat gnus-uu-tmp-dir "gnusuush")))
      (make-directory gnus-uu-shar-directory)
      (gnus-uu-add-file gnus-uu-shar-directory)
      (if (gnus-uu-decode-and-strip t marked)
	  (progn
	    (setq decoded t)
	    (setq files (directory-files gnus-uu-shar-directory t))
	    (setq gnus-uu-generated-file-list
		  (append files gnus-uu-generated-file-list))
	    (if (> (length files) 3)
		(progn 
		  (setq tar-file 
			(concat
			 (make-temp-name (concat gnus-uu-tmp-dir "gnusuuar"))
			 ".tar"))
		  (gnus-uu-add-file tar-file)
		  (call-process 
		   "sh" nil 
		   (get-buffer-create gnus-uu-output-buffer-name) nil "-c" 
		   (format "cd %s ; tar cf %s * ; cd .. ; rm -r %s" 
			   gnus-uu-shar-directory tar-file
			   gnus-uu-shar-directory))
		  (if view
		      (gnus-uu-view-file tar-file)
		    (gnus-uu-save-file tar-file)))
	      (if view
		  (gnus-uu-view-file (elt files 2))
		(gnus-uu-save-file (elt files 2)))))))

    (gnus-uu-summary-next-subject)

    (if (and gnus-uu-use-interactive-view view decoded)
	(gnus-uu-do-interactive))

    (if (or (not gnus-uu-use-interactive-view) (not decoded))
	(gnus-uu-clean-up))))


;; Functions for saving and possibly digesting articles without
;; any decoding.

(defconst gnus-uu-saved-article-name nil)

; VIEW isn't used, but is here anyway, to provide similar interface to
; the other related functions.  If MARKED is non-nil, the list of
; marked articles is used.  If NO-SAVE is non-nil, the articles aren't
; actually saved in a permanent location, but the collecting is done
; and a temporary file with the result is returned.
(defun gnus-uu-save-articles (view marked &optional no-save)
  (let (list-of-articles)
    (save-excursion
      (gnus-uu-initialize)
      (if (not marked)
	  (setq list-of-articles (gnus-uu-get-list-of-articles))
	(setq list-of-articles (reverse gnus-uu-marked-article-list))
	(setq gnus-uu-marked-article-list nil))

      (if (not list-of-articles)
	  (error "No list of articles"))

      (setq gnus-uu-saved-article-name 
	    (concat gnus-uu-work-dir 
		    (if no-save
			gnus-newsgroup-name
		      (read-file-name "Enter file name: " gnus-newsgroup-name
				      gnus-newsgroup-name))))
      (gnus-uu-add-file gnus-uu-saved-article-name)
      (if (and (gnus-uu-grab-articles list-of-articles 'gnus-uu-save-article t)
	       (not no-save))
	  (gnus-uu-save-file gnus-uu-saved-article-name)
	gnus-uu-saved-article-name))))

; Function called by gnus-uu-grab-articles to treat each article.
(defun gnus-uu-save-article (buffer in-state)
  (if (not gnus-uu-save-in-digest)
      (save-excursion
	(set-buffer buffer)
	(write-region 1 (point-max) gnus-uu-saved-article-name t)
	(cond ((eq in-state 'first) (list gnus-uu-saved-article-name 'begin))
	      ((eq in-state 'first-and-last) (list gnus-uu-saved-article-name 'begin 'end))
	      ((eq in-state 'last) (list 'end))
	      (t (list 'middle))))
    (let (beg subj name headers headline sorthead body end-string state)
      (string-match "/\\([^/]*\\)$" gnus-uu-saved-article-name)
      (setq name (substring gnus-uu-saved-article-name (match-beginning 1)
			    (match-end 1)))
      (if (or (eq in-state 'first) 
	      (eq in-state 'first-and-last))
	  (progn 
	    (setq state (list 'begin))
	    (save-excursion (set-buffer (get-buffer-create "*gnus-uu-body*"))
			    (erase-buffer))
	    (save-excursion 
	      (set-buffer (get-buffer-create "*gnus-uu-pre*"))
	      (erase-buffer)
	      (insert (format 
		       "Date: %s\nFrom: %s\nSubject: %s Digest\n\nTopics:\n"
		       (current-time-string) name name))))
	(if (not (eq in-state 'end))
	    (setq state (list 'middle))))
      (save-excursion
	(set-buffer (get-buffer "*gnus-uu-body*"))
	(goto-char (setq beg (point-max)))
	(save-excursion
	  (save-restriction
	    (set-buffer buffer)
	    (goto-char 1)
	    (re-search-forward "\n\n")
	    (setq body (buffer-substring (1- (point)) (point-max)))
	    (narrow-to-region 1 (point))
	    (setq headers (list "Date:" "From:" "To:" "Cc:" "Subject:"
				"Message-ID:" "Keywords:" "Summary:"))
	    (while headers
	      (setq headline (car headers))
	      (setq headers (cdr headers))
	      (goto-char 1)
	      (if (re-search-forward (concat "^" headline ".*$") nil t)
		  (setq sorthead 
			(concat sorthead (buffer-substring 
					  (match-beginning 0)
					  (match-end 0)) "\n"))))
	    (widen)))
	(insert sorthead)(goto-char (point-max))
	(insert body)(goto-char (point-max))
	(insert (concat "\n" (make-string 30 ?-) "\n\n"))
	(goto-char beg)
	(if (re-search-forward "^Subject: \\(.*\\)$" nil t)
	    (progn
	      (setq subj (buffer-substring (match-beginning 1) (match-end 1)))
	      (save-excursion 
		(set-buffer (get-buffer "*gnus-uu-pre*"))
		(insert (format "   %s\n" subj))))))
      (if (or (eq in-state 'last)
	      (eq in-state 'first-and-last))
	  (progn
	    (save-excursion
	      (set-buffer (get-buffer "*gnus-uu-pre*"))
	      (insert (format "\n\n%s\n\n" (make-string 70 ?-)))
	      (write-region 1 (point-max) gnus-uu-saved-article-name))
	    (save-excursion
	      (set-buffer (get-buffer "*gnus-uu-body*"))
	      (goto-char (point-max))
	      (insert 
	       (concat (setq end-string (format "End of %s Digest" name)) 
		       "\n"))
	      (insert (concat (make-string (length end-string) ?*) "\n"))
	      (write-region 1 (point-max) gnus-uu-saved-article-name t))
	    (kill-buffer (get-buffer "*gnus-uu-pre*"))
	    (kill-buffer (get-buffer "*gnus-uu-body*"))
	    (setq state (cons 'end state))))
      (if (memq 'begin state)
	  (cons gnus-uu-saved-article-name state)
	state))))


;; Digest and forward articles

(autoload 'gnus-mail-forward-using-mail "gnusmail"
	  "Forward the current message to another user." t)
(autoload 'gnus-mail-forward-using-mhe "gnusmail"
	  "Forward the current message to another user." t)

(defun gnus-uu-digest-and-forward (&optional marked)
  "Digests and forwards all articles in this series."
  (interactive)
  (let ((gnus-uu-save-in-digest t)
	file buf)
    (setq file (gnus-uu-save-articles nil marked t))
    (switch-to-buffer (setq buf (get-buffer-create "*gnus-uu-forward*")))
    (erase-buffer)
    (delete-other-windows)
    (erase-buffer)
    (insert-file file)
    (goto-char 1)
    (bury-buffer buf)
    (funcall gnus-mail-forward-method)))

(defun gnus-uu-marked-digest-and-forward (&optional marked)
  "Digests and forwards all marked articles."
  (interactive)
  (gnus-uu-digest-and-forward t))


;; Binhex treatment - not very advanced. 

(defconst gnus-uu-binhex-body-line 
  "^[^:]...............................................................$")
(defconst gnus-uu-binhex-begin-line 
  "^:...............................................................$")
(defconst gnus-uu-binhex-end-line
  ":$")
(defvar gnus-uu-binhex-article-name nil)

; This just concatenates and strips stuff from binhexed articles.
; No actual unbinhexing takes place. VIEW is ignored.
(defun gnus-uu-binhex-and-save (view marked)
  (gnus-uu-initialize)
  (let (list-of-articles)
    (save-excursion
      (if (not marked)
	  (setq list-of-articles (gnus-uu-get-list-of-articles))
	(setq list-of-articles (reverse gnus-uu-marked-article-list))
	(setq gnus-uu-marked-article-list nil))
      (if (not list-of-articles)
	  (error "No list of articles"))

      (setq gnus-uu-binhex-article-name 
	    (concat gnus-uu-work-dir 
		    (read-file-name "Enter binhex file name: " 
				    gnus-newsgroup-name
				    gnus-newsgroup-name)))
      (gnus-uu-add-file gnus-uu-binhex-article-name)
      (if (gnus-uu-grab-articles list-of-articles 'gnus-uu-binhex-article t)
	  (gnus-uu-save-file gnus-uu-binhex-article-name))))
  (gnus-uu-check-for-generated-files)
  (gnus-uu-summary-next-subject))

(defun gnus-uu-binhex-article (buffer in-state)
  (let (state start-char)
    (save-excursion
      (set-buffer buffer)
      (widen)
      (goto-char 1)
      (if (not (re-search-forward gnus-uu-binhex-begin-line nil t))
	  (if (not (re-search-forward gnus-uu-binhex-body-line nil t))
	      (setq state (list 'wrong-type))))

      (if (memq 'wrong-type state)
	  ()
	(beginning-of-line)
	(setq start-char (point))
	(if (looking-at gnus-uu-binhex-begin-line)
	    (progn
	      (setq state (list 'begin))
	      (write-region 1 1 gnus-uu-binhex-article-name))
	  (setq state (list 'middle)))
	(goto-char (point-max))
	(re-search-backward (concat gnus-uu-binhex-body-line "\\|" 
				    gnus-uu-binhex-end-line) nil t)
	(if (looking-at gnus-uu-binhex-end-line)
	    (setq state (if (memq 'begin state)
			    (cons 'end state)
			  (list 'end))))
	(beginning-of-line)
	(forward-line 1)
	(if (file-exists-p gnus-uu-binhex-article-name)
	    (append-to-file start-char (point) gnus-uu-binhex-article-name))))
    (if (memq 'begin state)
	(cons gnus-uu-binhex-article-name state)
      state)))
      

;; Internal view commands

; This function takes two parameters. The first is name of the file to
; be viewed. `gnus-uu-view-file' will look for an action associated
; with the file type of the file. If it finds an appropriate action,
; the file will be attempted displayed.
; 
; The second parameter specifies if the user is to be asked whether to
; save the file if viewing is unsuccessful. t means "do not ask."
;
; Note that the file given will be deleted by this function, one way
; or another. If `gnus-uu-asynchronous' is set, it won't be deleted
; right away, but sometime later. If the user is offered to save the
; file, it'll be moved to wherever the user wants it.

; `gnus-uu-view-file' returns t if viewing is successful.

(defun gnus-uu-view-file (file &optional silent)
  (let (action did-view)
    (cond 
     ((not (setq action (gnus-uu-get-action file)))
      (if (and (not silent) (not gnus-uu-use-interactive-view))
	  (progn
	    (message "Couldn't find any rule for file '%s'" file)
	    (sleep-for 2)
	    (gnus-uu-ask-to-save-file file))))
     
     ((and gnus-uu-use-interactive-view 
	   (not (string= (or action "") "gnus-uu-archive")))
      (gnus-uu-enter-interactive-file (or action "") file))

     (gnus-uu-ask-before-view
      (if (y-or-n-p (format "Do you want to view %s? " file))
	  (setq did-view (gnus-uu-call-file-action file action)))
      (message ""))
     
     ((setq did-view (gnus-uu-call-file-action file action)))

     ((not silent)
      (gnus-uu-ask-to-save-file file)))

    (if (and (file-exists-p file) 
	     (not gnus-uu-use-interactive-view)
	     (or 
	      (not (and gnus-uu-asynchronous did-view))
	      (string= (or action  "") "gnus-uu-archive")))
	(delete-file file))

  did-view))

(defun gnus-uu-call-file-action (file action)
  (prog1
      (if gnus-uu-asynchronous
	  (gnus-uu-call-asynchronous file action)
	(gnus-uu-call-synchronous file action))
    (if gnus-uu-view-and-save
	(gnus-uu-ask-to-save-file file))))

(defun gnus-uu-ask-to-save-file (file)
  (if (y-or-n-p (format "Do you want to save the file %s? " file))
      (gnus-uu-save-file file))
  (message ""))

(defun gnus-uu-get-action (file-name)
  (let (action)
    (setq action 
	  (gnus-uu-choose-action 
	   file-name
	   (append 
	    (if (and gnus-uu-use-interactive-view 
		     gnus-uu-user-interactive-view-rules)
		gnus-uu-user-interactive-view-rules
	      gnus-uu-user-view-rules)
	    (if (or gnus-uu-ignore-default-view-rules 
		    (not gnus-uu-use-interactive-view))
		()
	      gnus-uu-default-interactive-view-rules-begin)
	    (if gnus-uu-ignore-default-view-rules 
		nil 
	      gnus-uu-default-view-rules)
	    (if gnus-uu-use-interactive-view
		(append gnus-uu-user-interactive-view-rules-end
			(if gnus-uu-ignore-default-view-rules
			    ()
			  gnus-uu-default-interactive-view-rules-end))
	      gnus-uu-user-view-rules-end))))
    (if (and (not (string= (or action "") "gnus-uu-archive")) 
	     gnus-uu-view-with-metamail)
	(if (setq action 
		  (gnus-uu-choose-action file-name gnus-uu-ext-to-mime-list))
	    (setq action (format "metamail -d -b -c \"%s\"" action))))
    action))

; `gnus-uu-call-synchronous' takes two parameters: The name of the
; file to be displayed and the command to display it with. Returns t
; on success and nil if the file couldn't be displayed.
(defun gnus-uu-call-synchronous (file-name action)
  (let (did-view command)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (erase-buffer)
      (setq command (gnus-uu-command action file-name))
      (message "Viewing with '%s'" command)
      (if (not (= 0 (call-process "sh" nil t nil "-c" command)))
	  (progn
	    (goto-char 1)
	    (while (re-search-forward "\n" nil t)
	      (replace-match " "))
	    (message (concat "Error: " (buffer-substring 1 (point-max))))
	    (sit-for 2))
	(message "")
	(setq did-view t)))
    did-view))

; `gnus-uu-call-asyncronous' takes two parameters: The name of the
; file to be displayed and the command to display it with. Since the
; view command is executed asynchronously, it's kinda hard to decide
; whether the command succeded or not, so this function always returns
; t. It also adds "; rm -f file-name" to the end of the execution
; string, so the file will be removed after viewing has ended.
(defun gnus-uu-call-asynchronous (file-name action)
  (let (command file tmp-file start)
    (while (string-match "/" file-name start)
      (setq start (1+ (match-beginning 0))))
    (setq file (substring file-name start))
    (setq tmp-file (concat gnus-uu-work-dir file))
    (if (string= tmp-file file-name)
	()
      (rename-file file-name tmp-file t)
      (setq file-name tmp-file))

    (setq command (gnus-uu-command action file-name))
    (setq command (format "%s ; rm -f %s" command file-name))
    (message "Viewing with %s" command)
    (start-process "gnus-uu-view" nil "sh" "-c" command)
    t))

; `gnus-uu-decode-and-strip' does all the main work. It finds out what
; articles to grab, grabs them, strips the result and decodes. If any
; of these operations fail, it returns nil, t otherwise.  If shar is
; t, it will pass this on to `gnus-uu-grab-articles', which will
; (probably) unshar the articles. If use-marked is non-nil, it won't
; try to find articles, but use the marked list.
(defun gnus-uu-decode-and-strip (&optional shar use-marked limit)
  (let (list-of-articles)
    (save-excursion

      (if use-marked
	  (if (not gnus-uu-marked-article-list)
	      (message "No articles marked")
	    (setq list-of-articles (reverse gnus-uu-marked-article-list))
	    (setq gnus-uu-marked-article-list nil))
	(setq list-of-articles (gnus-uu-get-list-of-articles)))
      
      (and list-of-articles
	   (gnus-uu-grab-articles 
	    list-of-articles 
	    (if shar 'gnus-uu-unshar-article 'gnus-uu-uustrip-article-as)
	    t limit)))))

; Takes a string and puts a \ in front of every special character;
; ignores any leading "version numbers" thingies that they use in the
; comp.binaries groups, and either replaces anything that looks like
; "2/3" with "[0-9]+/[0-9]+" or, if it can't find something like that,
; replaces the last two numbers with "[0-9]+". This, in my experience,
; should get most postings of a series."
(defun gnus-uu-reginize-string (string)
  (let ((count 2)
	(vernum "v[0-9]+[a-z][0-9]+:")
	reg beg)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (erase-buffer)
      (insert (regexp-quote string))
      (setq beg 1)

      (setq case-fold-search nil)
      (goto-char 1)
      (if (looking-at vernum)
	  (progn
	    (replace-match vernum t t)
	    (setq beg (length vernum))))

      (goto-char beg)
      (if (re-search-forward "[ \t]*[0-9]+/[0-9]+" nil t)
	  (replace-match " [0-9]+/[0-9]+")

	(goto-char beg)
	(if (re-search-forward "[0-9]+[ \t]*of[ \t]*[0-9]+" nil t)
	    (replace-match "[0-9]+ of [0-9]+")

	  (end-of-line)
	  (while (and (re-search-backward "[0-9]" nil t) (> count 0))
            (while (and 
		    (looking-at "[0-9]") 
		    (< 1 (goto-char (1- (point))))))
            (re-search-forward "[0-9]+" nil t)
	    (replace-match "[0-9]+")
	    (backward-char 5)
	    (setq count (1- count)))))

      (goto-char beg)
      (while (re-search-forward "[ \t]+" nil t)
	(replace-match "[ \t]*" t t))

      (buffer-substring 1 (point-max)))))

; Finds all articles that matches the regular expression given.
; Returns the resulting list. SUBJECT is the regular expression to be
; matched. If it is nil, the current article name will be used. If
; MARK-ARTICLES is non-nil, articles found are marked. If ONLY-UNREAD
; is non-nil, only unread articles are chose. If DO-NOT-TRANSLATE is
; non-nil, article names are not equialized before sorting.
(defun gnus-uu-get-list-of-articles (&optional subject mark-articles only-unread do-not-translate)
  (let (beg end reg-subject list-of-subjects list-of-numbers art-num)
    (save-excursion
      
; If the subject is not given, this function looks at the current subject
; and takes that.

      (if subject
	  (setq reg-subject subject)
	(end-of-line)
	(setq end (point))
	(beginning-of-line)
	(if (not (re-search-forward "\\] " end t))
	    (progn (message "No valid subject chosen") (sit-for 2))
	  (setq subject (buffer-substring (point) end))
	  (setq reg-subject 
		(concat "\\[.*\\] " (gnus-uu-reginize-string subject)))))

;      (message reg-subject)(sleep-for 2)
      
      (if reg-subject
	  (progn

; Collect all subjects matching reg-subject.

	    (let ((case-fold-search t))
	      (goto-char 1)
	      (while (re-search-forward reg-subject nil t)
		(beginning-of-line)
		(setq beg (point))
		(if (or (not only-unread) (looking-at " \\|-"))
		    (progn
		      (end-of-line)
		      (setq list-of-subjects (cons 
					      (buffer-substring beg (point))
					      list-of-subjects)))
		  (end-of-line))))

; Expand all numbers in all the subjects: (hi9 -> hi0009, etc).

	    (setq list-of-subjects (gnus-uu-expand-numbers 
				    list-of-subjects
				    (not do-not-translate)))

; Sort the subjects.

	    (setq list-of-subjects (sort list-of-subjects 'gnus-uu-string<))

; Get the article numbers from the sorted list of subjects.

	    (while list-of-subjects 
	      (setq art-num (gnus-uu-article-number (car list-of-subjects)))
	      (if mark-articles (gnus-summary-mark-as-read art-num ?#))
	      (setq list-of-numbers (cons art-num list-of-numbers))
	      (setq list-of-subjects (cdr list-of-subjects)))

	    (setq list-of-numbers (nreverse list-of-numbers))))

      list-of-numbers)))

; Takes a list of strings and "expands" all numbers in all the
; strings.  That is, this function makes all numbers equal length by
; prepending lots of zeroes before each number. This is to ease later
; sorting to find out what sequence the articles are supposed to be
; decoded in. Returns the list of expanded strings.
(defun gnus-uu-expand-numbers (string-list &optional translate)
  (let (string out-list pos num)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (while string-list
	(erase-buffer)
	(setq string (car string-list))
	(setq string-list (cdr string-list))
	(insert string)
	(goto-char 1)
	(while (re-search-forward "[ \t]+" nil t)
	  (replace-match " "))
	(goto-char 1)
	(if translate 
	    (while (re-search-forward "[A-Za-z]" nil t)
	      (replace-match "a" t t)))

	(goto-char 1)
	(if (not (search-forward "] " nil t))
	    ()
	  (while (re-search-forward "[0-9]+" nil t)
	    (replace-match  
	     (format "%06d" 
		     (string-to-int (buffer-substring 
				     (match-beginning 0) (match-end 0))))))
	  (setq string (buffer-substring 1 (point-max)))
	  (setq out-list (cons string out-list)))))
    out-list))

; Used in a sort for finding out what string is bigger, but ignoring
; everything before the subject part.
(defun gnus-uu-string< (string1 string2) 
  (string< (substring string1 (string-match "\\] " string1))
	   (substring string2 (string-match "\\] " string2))))


;; gnus-uu-grab-article
;
; This is the general multi-article treatment function.  It takes a
; list of articles to be grabbed and a function to apply to each
; article. It puts the result in `gnus-uu-result-buffer'.
;
; The function to be called should take two parameters.  The first
; parameter is the article buffer. The function should leave the
; result, if any, in this buffer. This result is then appended on to
; the `gnus-uu-result-buffer'. Most treatment functions will just
; generate files...
;
; The second parameter is the state of the list of articles, and can
; have four values: `first', `middle', `last' and `first-and-last'.
;
; The function should return a list. The list may contain the
; following symbols:
; `error' if an error occurred
; `begin' if the beginning of an encoded file has been received
;   If the list returned contains a `begin', the first element of
;   the list *must* be a string with the file name of the decoded
;   file.
; `end' if the the end of an encoded file has been received
; `middle' if the article was a body part of an encoded file
; `wrong-type' if the article was not a part of an encoded file
; `ok', which can be used everything is ok

(defvar gnus-uu-has-been-grabbed nil)

(defun gnus-uu-unmark-list-of-grabbed (&optional dont-unmark-last-article)
  (let (art)
    (if (not (and gnus-uu-has-been-grabbed
		  gnus-uu-unmark-articles-not-decoded))
	()
      (if dont-unmark-last-article
	  (progn
	    (setq art (car gnus-uu-has-been-grabbed))
	    (setq gnus-uu-has-been-grabbed (cdr gnus-uu-has-been-grabbed))))
      (while gnus-uu-has-been-grabbed
	(gnus-summary-mark-as-unread (car gnus-uu-has-been-grabbed) t)
	(setq gnus-uu-has-been-grabbed (cdr gnus-uu-has-been-grabbed)))
      (if dont-unmark-last-article
	  (setq gnus-uu-has-been-grabbed (list art))))))


; This function takes a list of articles and a function to apply to
; each article grabbed. The result of the function is appended on to
; `gnus-uu-result-buffer'.
; 
; This function returns a list of files decoded if the grabbing and
; the process-function has been successful and nil otherwise.
(defun gnus-uu-grab-articles (list-of-articles process-function &optional sloppy limit)
  (let ((result-buffer (get-buffer-create gnus-uu-result-buffer))
	(state 'first)
	(wrong-type t)
	has-been-begin has-been-end 
	article result-file result-files process-state)

    (save-excursion
      (set-buffer result-buffer)
      (erase-buffer))
    (setq gnus-uu-has-been-grabbed nil)

    (while (and list-of-articles 
		(not (memq 'error process-state))
		(or sloppy
		    (not (memq 'end process-state))))

      (setq article (car list-of-articles))
      (setq list-of-articles (cdr list-of-articles))
      (setq gnus-uu-has-been-grabbed (cons article gnus-uu-has-been-grabbed))

      (if (> article gnus-uu-highest-article-number) 
	  (setq gnus-uu-highest-article-number article))

      (if (eq list-of-articles ()) 
	  (if (eq state 'first)
	      (setq state 'first-and-last)
	    (setq state 'last)))

      (message "Getting article %d" article)
      (if (not (= (or gnus-current-article 0) article))
	  (gnus-summary-display-article article))
      (gnus-summary-mark-as-read article)

      (save-excursion (set-buffer gnus-article-buffer) (widen))

      (setq process-state (funcall process-function gnus-article-buffer state))

;      (message "process-state er %s" process-state)(sleep-for 3)

      (if (or (memq 'begin process-state)
	      (and (or (eq state 'first) (eq state 'first-and-last))
		   (memq 'ok process-state)))
	  (progn
	    (if has-been-begin
		(if (file-exists-p result-file) (delete-file result-file)))
	    (setq result-file (car process-state))
	    (setq has-been-begin t)
	    (setq has-been-end nil)))

      (if (memq 'end process-state)
	  (progn
	    (setq gnus-uu-has-been-grabbed nil)
	    (setq result-files (cons result-file result-files))
	    (setq has-been-end t)
	    (setq has-been-begin nil)
	    (if (and limit (= (length result-files) limit))
		(progn
		  (setq list-of-articles nil)
		  (setq gnus-uu-marked-article-list nil)))))

      (if (and (or (eq state 'last) (eq state 'first-and-last))
	       (not (memq 'end process-state)))
		(if (and result-file (file-exists-p result-file))
		    (delete-file result-file)))

      (setq result-file nil)

      (if (not (memq 'wrong-type process-state))
	  (setq wrong-type nil)
	(if gnus-uu-unmark-articles-not-decoded
	    (gnus-summary-mark-as-unread article t)))

      (if sloppy (setq wrong-type nil))

      (if (and (not has-been-begin)
	       (not sloppy)
	       (or (memq 'end process-state)
		   (memq 'middle process-state)))
	  (progn
	    (setq process-state (list 'error))
	    (message "No begin part at the beginning")
	    (sleep-for 2))
	(setq state 'middle)))

    (if result-files
	()
      (if (not has-been-begin)
	  (message "Wrong type file")
	(if (memq 'error process-state)
	    (setq result-files nil)
	  (if (not (or (memq 'ok process-state) 
		       (memq 'end process-state)))
	      (progn
		(message "End of articles reached before end of file")
		(setq result-files nil))
	    (gnus-uu-unmark-list-of-grabbed)))))
    (setq gnus-uu-list-of-files-decoded result-files)
    result-files))

(defun gnus-uu-uudecode-sentinel (process event)
  (delete-process (get-process process)))

; Uudecodes a file asynchronously.
(defun gnus-uu-uustrip-article-as (process-buffer in-state)
  (let ((state (list 'ok))
	(process-connection-type nil)
	start-char pst name-beg name-end)
    (save-excursion
      (set-buffer process-buffer)
      (let ((case-fold-search nil)
	    (buffer-read-only nil))

	(goto-char 1)

	(if gnus-uu-kill-carriage-return
	    (progn
	      (while (search-forward "\r" nil t)
		(delete-backward-char 1))
	      (goto-char 1)))

	(if (not (re-search-forward gnus-uu-begin-string nil t))
	    (if (not (re-search-forward gnus-uu-body-line nil t))
		(setq state (list 'wrong-type))))
     
	(if (memq 'wrong-type state)
	    ()
	  (beginning-of-line)
	  (setq start-char (point))

	  (if (looking-at gnus-uu-begin-string)
	      (progn 
		(setq name-end (match-end 1))

		; Replace any slashes and spaces in file names before decoding
		(goto-char (setq name-beg (match-beginning 1)))
		(while (re-search-forward "/" name-end t)
		  (replace-match ","))
		(goto-char name-beg)
		(while (re-search-forward " " name-end t)
		  (replace-match "_"))

		(setq gnus-uu-file-name (buffer-substring name-beg name-end))
		(and gnus-uu-uudecode-process
		     (setq pst (process-status 
				(or gnus-uu-uudecode-process "nevair")))
		     (if (or (eq pst 'stop) (eq pst 'run))
			 (progn
			   (delete-process gnus-uu-uudecode-process)
			   (gnus-uu-unmark-list-of-grabbed t))))
		(setq gnus-uu-uudecode-process
		      (start-process 
		       "*uudecode*" 
		       (get-buffer-create gnus-uu-output-buffer-name)
		       "sh" "-c" 
		       (format "cd %s ; uudecode" gnus-uu-work-dir)))
		(set-process-sentinel 
		 gnus-uu-uudecode-process 'gnus-uu-uudecode-sentinel)
		(setq state (list 'begin))
		(gnus-uu-add-file (concat gnus-uu-work-dir gnus-uu-file-name)))
	    (setq state (list 'middle)))
	
	  (goto-char (point-max))

	  (re-search-backward 
	   (concat gnus-uu-body-line "\\|" gnus-uu-end-string) nil t)
	  (beginning-of-line)

	  (if (looking-at gnus-uu-end-string)
	      (setq state (cons 'end state)))
	  (forward-line 1)

	  (and gnus-uu-uudecode-process
	       (setq pst (process-status 
			  (or gnus-uu-uudecode-process "nevair")))
	       (if (or (eq pst 'run) (eq pst 'stop))
		   (progn
		     (if gnus-uu-correct-stripped-uucode
			 (progn
			   (gnus-uu-check-correct-stripped-uucode 
			    start-char (point))
			   (goto-char (point-max))
			   (re-search-backward 
			    (concat gnus-uu-body-line "\\|" 
				    gnus-uu-end-string) 
			    nil t)
			   (forward-line 1)))
		     (condition-case err
			 (process-send-region gnus-uu-uudecode-process 
					      start-char (point))
		       (error 
			(progn 
			  (message "gnus-uu: Couldn't uudecode")
			  (sleep-for 2)
			  (setq state (list 'wrong-type))
			  (delete-process gnus-uu-uudecode-process))))
		     (if (memq 'end state)
			 (accept-process-output gnus-uu-uudecode-process)))
		 (setq state (list 'wrong-type))))
	  (if (not gnus-uu-uudecode-process)
	      (setq state (list 'wrong-type)))))

      (if (memq 'begin state)
	  (cons (concat gnus-uu-work-dir gnus-uu-file-name) state)
	state))))

; This function is used by `gnus-uu-grab-articles' to treat
; a shared article.
(defun gnus-uu-unshar-article (process-buffer in-state)
  (let ((state (list 'ok))
	start-char)
    (save-excursion
     (set-buffer process-buffer)
     (goto-char 1)
     (if (not (re-search-forward gnus-uu-shar-begin-string nil t))
	 (setq state (list 'wrong-type))
       (beginning-of-line)
       (setq start-char (point))
       (call-process-region 
	start-char (point-max) "sh" nil 
	(get-buffer-create gnus-uu-output-buffer-name) nil 
	"-c" (concat "cd " gnus-uu-shar-directory " ; sh"))))
    state))

; Returns the name of what the shar file is going to unpack.
(defun gnus-uu-find-name-in-shar ()
  (let ((oldpoint (point))
	res)
    (goto-char 1)
    (if (re-search-forward gnus-uu-shar-name-marker nil t)
	(setq res (buffer-substring (match-beginning 1) (match-end 1))))
    (goto-char oldpoint)
    res))

; Returns the article number of the given subject.
(defun gnus-uu-article-number (subject)
  (let (end)
    (string-match "[0-9]+[^0-9]" subject 1)
    (setq end (match-end 0))
    (string-to-int 
     (substring subject (string-match "[0-9]" subject 1) end)))) 
	      
; `gnus-uu-choose-action' chooses what action to perform given the name
; and `gnus-uu-file-action-list'.  Returns either nil if no action is
; found, or the name of the command to run if such a rule is found.
(defun gnus-uu-choose-action (file-name file-action-list &optional no-ignore)
  (let ((action-list (copy-sequence file-action-list))
	rule action)
    (and 
     (or no-ignore 
	 (and (not 
	       (and gnus-uu-ignore-files-by-name
		    (string-match gnus-uu-ignore-files-by-name file-name)))
	      (not 
	       (and gnus-uu-ignore-files-by-type
		    (string-match gnus-uu-ignore-files-by-type 
				  (or (gnus-uu-choose-action 
				       file-name gnus-uu-ext-to-mime-list t) 
				      ""))))))
     (while (not (or (eq action-list ()) action))
       (setq rule (car action-list))
       (setq action-list (cdr action-list))
       (if (string-match (car rule) file-name)
	   (setq action (car (cdr rule))))))
    action))

(defun gnus-uu-save-directory (from-dir &optional default-dir ignore-existing)
  (let (dir file-name command files file)
    (setq files (directory-files from-dir t))
    (if default-dir
	(setq dir default-dir)
      (setq dir (gnus-uu-read-directory 
		 (concat "Where do you want the file" 
			 (if (< 3 (length files)) "s" "") "? "))))

    (while files
      (setq file (car files))
      (setq files (cdr files))
      (string-match "/[^/]*$" file)
      (setq file-name (substring file (1+ (match-beginning 0))))
      (if (string-match "^\\.\\.?$" file-name)
	  ()
	(if (and (not ignore-existing) (file-exists-p (concat dir file-name)))
	    (setq file-name
		  (read-file-name "File exists. Enter a new name: " dir 
				  (concat dir file-name) nil file-name))
	  (setq file-name (concat dir file-name)))
	(rename-file file file-name t)))))

; Moves the file from the tmp directory to where the user wants it.
(defun gnus-uu-save-file (from-file-name &optional default-dir ignore-existing)
  (let (dir file-name command)
    (string-match "/[^/]*$" from-file-name)
    (setq file-name (substring from-file-name (1+ (match-beginning 0))))
    (if default-dir
	(setq dir default-dir)
      (setq dir (gnus-uu-read-directory "Where do you want the file? ")))
    (if (and (not ignore-existing) (file-exists-p (concat dir file-name)))
	(setq file-name
	      (read-file-name "File exist. Enter a new name: " dir 
			      (concat dir file-name) nil file-name))
      (setq file-name (concat dir file-name)))
    (rename-file from-file-name file-name t)))
    
(defun gnus-uu-read-directory (prompt &optional default)
  (let (dir ok create)
    (while (not ok)
      (setq ok t)
      (setq dir (if default default
		  (read-file-name prompt gnus-uu-current-save-dir 
				  gnus-uu-current-save-dir)))
      (while (string-match "/$" dir)
	(setq dir (substring dir 0 (match-beginning 0))))
      (if (file-exists-p dir)
	  (if (not (file-directory-p dir))
	      (progn
		(setq ok nil)
		(message "%s is a file" dir)
		(sit-for 2)))
	(setq create ?o)
	(while (not (or (= create ?y) (= create ?n)))
	  (message "%s: No such directory. Do you want to create it? (y/n)" 
		   dir)
	  (setq create (read-char)))
	(if (= create ?y) (make-directory dir))))
    (setq gnus-uu-current-save-dir (concat dir "/"))))

; Unpacks an archive and views all the files in it. Returns t if
; viewing one or more files is successful.
(defun gnus-uu-treat-archive (file-path)
  (let ((did-unpack t)
	action command files file file-name dir)
    (setq action (gnus-uu-choose-action 
		  file-path (append gnus-uu-user-archive-rules
				    (if gnus-uu-ignore-default-archive-rules
					nil
				      gnus-uu-default-archive-rules))))

    (if (not action) (error "No unpackers for the file %s" file-path))

    (string-match "/[^/]*$" file-path)
    (setq file-name (substring file-path (1+ (match-beginning 0))))
    (setq dir (substring file-path 0 (match-beginning 0)))

    (if (gnus-uu-string-in-list action gnus-uu-destructive-archivers)
	(copy-file file-path (concat file-path "~") t))

    (setq command (format "cd %s ; %s" dir (gnus-uu-command action file-path)))

    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (erase-buffer))

    (message "Unpacking: %s..." (gnus-uu-command action file-path))

    (if (= 0 (call-process "sh" nil 
			   (get-buffer-create gnus-uu-output-buffer-name)
			   nil "-c" command))
	(message "")
      (if (not gnus-uu-use-interactive-view)
	  (progn
	    (message "Error during unpacking of archive")
	    (sleep-for 2)))
      (setq did-unpack nil))

    (if (gnus-uu-string-in-list action gnus-uu-destructive-archivers)
	(rename-file (concat file-path "~") file-path t))

    did-unpack))

; Tries to view all the files in the given directory. Returns t if
; viewing one or more files is successful.
(defun gnus-uu-view-directory (dir &optional dont-delete-files not-top)
  (let ((first t)
	files file did-view ignore-files)
    (setq files (directory-files dir t "[^/][^\\.][^\\.]?$"))
    (gnus-uu-add-file files)
    (setq ignore-files files)
    
    (while (gnus-uu-unpack-archives 
	    files (if not-top (list ".")
		    (if first () ignore-files)))
      (setq first nil)
      (gnus-uu-add-file 
       (setq files (directory-files dir t "[^/][^\\.][^\\.]?$"))))

    (gnus-uu-add-file (directory-files dir t "[^/][^\\.][^\\.]?$"))
      
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (if (not (string= (or (gnus-uu-get-action file) "") "gnus-uu-archive"))
	  (progn
	    (set-file-modes file 448)
	    (if (file-directory-p file)
		(setq did-view (or (gnus-uu-view-directory file 
							   dont-delete-files 
							   t) 
				   did-view))
	      (setq did-view (or (gnus-uu-view-file file t) did-view)))))
      (if (and (not dont-delete-files) (not gnus-uu-asynchronous) 
	       (file-exists-p file))
	  (delete-file file)))

    (if (and (not gnus-uu-asynchronous) (not dont-delete-files))
	(if (string-match "/$" dir)
	    (delete-directory (substring dir 0 (match-beginning 0)))
	  (delete-directory dir)))
    did-view))

(defun gnus-uu-unpack-archives (files &optional ignore)
  (let (path did-unpack)
    (while files
      (setq path (car files))
      (setq files (cdr files))
      (if (not (gnus-uu-string-in-list path ignore))
	  (if (string= (or (gnus-uu-get-action 
			    (gnus-uu-name-from-path path)) "") 
		       "gnus-uu-archive")
	      (progn
		(if (and (not (setq did-unpack (gnus-uu-treat-archive path)))
			 gnus-uu-use-interactive-view)
		    (gnus-uu-enter-interactive-file 
		     "# error during unpacking of" path))
		(if ignore (delete-file path))))))
    did-unpack))


;; Manual marking

(defun gnus-uu-enter-mark-in-list ()
  (let (article	beg)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq article (gnus-uu-article-number 
		     (buffer-substring beg (point))))
      (message "Adding article %d to list" article)
      (setq gnus-uu-marked-article-list 
	    (cons article gnus-uu-marked-article-list))))) 

(defun gnus-uu-mark-article (&optional dont-move)
  "Marks the current article to be decoded later."
  (interactive)
  (gnus-uu-enter-mark-in-list)
  (gnus-summary-mark-as-read nil ?#)
  (gnus-summary-next-subject 1 nil))

(defun gnus-uu-unmark-article ()
  "Unmarks the current article."
  (interactive)
  (let ((in (copy-sequence gnus-uu-marked-article-list))
	out article beg found
	(old-point (point)))
    (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq article (gnus-uu-article-number (buffer-substring beg (point))))
      (message "Removing article %d" article)
      (while in 
	(if (not (= (car in) article))
	    (setq out (cons (car in) out))
	  (setq found t)
	  (message "Removing article %d" article))
	(setq in (cdr in)))
      (if (not found) (message "Not a marked article."))
      (setq gnus-uu-marked-article-list (reverse out))
      (gnus-summary-mark-as-unread nil t)
      (gnus-summary-next-subject 1 nil)))

(defun gnus-uu-unmark-all-articles ()
  "Removes the mark from all articles marked for decoding."
  (interactive)
  (while gnus-uu-marked-article-list
    (gnus-summary-goto-subject (car gnus-uu-marked-article-list))
    (gnus-summary-mark-as-unread nil t)
    (setq gnus-uu-marked-article-list (cdr gnus-uu-marked-article-list))))

(defun gnus-uu-mark-by-regexp ()
  "Asks for a regular expression and marks all articles that match."
  (interactive)
  (let (exp)
    (setq exp (read-from-minibuffer "Mark (regexp): "))
    (setq gnus-uu-marked-article-list 
	  (append gnus-uu-marked-article-list
		  (reverse (gnus-uu-get-list-of-articles exp t))))
    (message "")))
      
(defun gnus-uu-mark-thread ()
  "Marks all articles downwards in this thread."
  (interactive)
  (beginning-of-line)
  (let (level)
    (if (not (search-forward ":" nil t))
	()
      (setq level (current-column))
      (gnus-uu-enter-mark-in-list)
      (gnus-summary-mark-as-read nil ?#)
      (gnus-summary-search-forward)
      (while (< level (current-column))
	(gnus-uu-enter-mark-in-list)
	(gnus-summary-mark-as-read nil ?#)
	(gnus-summary-search-forward))
      (gnus-summary-search-backward))))


;; Various stuff

(defun gnus-uu-string-in-list (string list)
  (while (and list
	      (not (string= (car list) string))
	      (setq list (cdr list))))
  list)

(defun gnus-uu-name-from-path (path)
  (string-match "/[^/]*$" path)
  (substring path (1+ (match-beginning 0))))

(defun gnus-uu-directory-files (dir)
  (let (files out file)
    (setq files (directory-files dir t))
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (if (not (string-match "/\\.\\.?$" file))
	  (setq out (cons file out))))
    (setq out (reverse out))
    out))

(defun gnus-uu-check-correct-stripped-uucode (start end)
  (let (found beg length short)
    (if (not gnus-uu-correct-stripped-uucode)
	()
      (goto-char start)

      (if (re-search-forward " \\|`" end t)
	  (progn
	    (goto-char start)
	    (while (not (eobp))
	      (progn
		(if (looking-at "\n") (replace-match ""))
		(forward-line 1))))
	    
	(while (not (eobp))
	  (if (looking-at (concat gnus-uu-begin-string "\\|" 
				  gnus-uu-end-string))
	      ()
	    (if (not found)
		(progn
		  (beginning-of-line)
		  (setq beg (point))
		  (end-of-line)
		  (setq length (- (point) beg))))
	    (setq found t)
	    (beginning-of-line)
	    (setq beg (point))
	    (end-of-line)
	    (if (not (= length (- (point) beg)))
		(insert (make-string (- length (- (point) beg)) ? ))))
	  (forward-line 1))))))

(defun gnus-uu-initialize ()
  (setq gnus-uu-highest-article-number 1)
  (gnus-uu-check-for-generated-files)
  (setq gnus-uu-tmp-dir (expand-file-name gnus-uu-tmp-dir))
  (if (string-match "[^/]$" gnus-uu-tmp-dir) 
      (setq gnus-uu-tmp-dir (concat gnus-uu-tmp-dir "/")))
  (if (not (file-directory-p gnus-uu-tmp-dir))
      (error "Temp directory %s doesn't exist" gnus-uu-tmp-dir)
    (if (not (file-writable-p gnus-uu-tmp-dir))
	(error "Temp directory %s can't be written to" gnus-uu-tmp-dir)))
  (setq gnus-uu-work-dir 
	(concat gnus-uu-tmp-dir (make-temp-name "gnus")))
  (gnus-uu-add-file gnus-uu-work-dir)
  (if (not (file-directory-p gnus-uu-work-dir)) 
      (make-directory gnus-uu-work-dir))
  (setq gnus-uu-work-dir (concat gnus-uu-work-dir "/"))
  (setq gnus-uu-interactive-file-list nil))

; Kills the temporary uu buffers, kills any processes, etc.
(defun gnus-uu-clean-up ()
  (let (buf pst)
    (and gnus-uu-uudecode-process
	 (setq pst (process-status (or gnus-uu-uudecode-process "nevair")))
	 (if (or (eq pst 'stop) (eq pst 'run))
	     (delete-process gnus-uu-uudecode-process)))
    (and (not gnus-uu-asynchronous) 
	 (setq buf (get-buffer gnus-uu-output-buffer-name))
	 (kill-buffer buf))
    (and (setq buf (get-buffer gnus-uu-result-buffer))
	 (kill-buffer buf))))

; `gnus-uu-check-for-generated-files' deletes any generated files that
; hasn't been deleted, if, for instance, the user terminated decoding
; with `C-g'.
(defun gnus-uu-check-for-generated-files ()
  (let (file dirs)
    (while gnus-uu-generated-file-list
      (setq file (car gnus-uu-generated-file-list))
      (setq gnus-uu-generated-file-list (cdr gnus-uu-generated-file-list))
      (if (not (string-match "/\\.[\\.]?$" file))
	  (progn
	    (if (file-directory-p file)
		(setq dirs (cons file dirs))
	      (if (file-exists-p file)
		  (delete-file file))))))
    (setq dirs (nreverse dirs))
    (while dirs
      (setq file (car dirs))
      (setq dirs (cdr dirs))
      (if (file-directory-p file)
	  (if (string-match "/$" file)
	      (delete-directory (substring file 0 (match-beginning 0)))
	    (delete-directory file))))))

; Add a file (or a list of files) to be checked (and deleted if it/they
; still exists upon exiting the newsgroup).
(defun gnus-uu-add-file (file)
  (if (stringp file)
      (setq gnus-uu-generated-file-list 
	    (cons file gnus-uu-generated-file-list))
    (setq gnus-uu-generated-file-list 
	  (append file gnus-uu-generated-file-list))))

; Go to the next unread subject. If there is no further unread
; subjects, go to the last subject in the buffer.
(defun gnus-uu-summary-next-subject ()
  (let (opi)
    (if (not (gnus-summary-search-forward t))
	(progn
	  (goto-char 1)
	  (sit-for 0)
	  (gnus-summary-goto-subject gnus-uu-highest-article-number)))

    ; You may well find all this a bit puzzling - so do I, but I seem
    ; to have to do something like this to move to the next unread article,
    ; as `sit-for' seems to do some rather strange things here. Might
    ; be a bug in my head, probably.
    (setq opi (point))
    (sit-for 0)
    (goto-char opi)
    (gnus-summary-recenter)))

; Inputs an action and a file and returns a full command, putting
; ticks round the file name and escaping any ticks in the file name.
(defun gnus-uu-command (action file)
  (let ((ofile ""))
    (while (string-match "`\\|\"\\|\\$\\|\\\\" file)
      (progn
	(setq ofile
	      (concat ofile (substring file 0 (match-beginning 0)) "\\"
		      (substring file (match-beginning 0) (match-end 0))))
	(setq file (substring file (1+ (match-beginning 0))))))
    (setq ofile (concat "\"" ofile file "\""))
    (if (string-match "%s" action)
	(format action ofile)
      (concat action " " ofile))))


;; Initializing
(add-hook 'gnus-exit-group-hook
      '(lambda ()
	 (gnus-uu-clean-up)
	 (setq gnus-uu-marked-article-list nil)
	 (gnus-uu-check-for-generated-files)))


;; Interactive exec mode

(defvar gnus-uu-output-window nil)
(defvar gnus-uu-mode-hook nil)

(defvar gnus-uu-mode-map nil)
(if gnus-uu-mode-map
    ()
  (setq gnus-uu-mode-map (make-sparse-keymap))
  (define-key gnus-uu-mode-map "\C-c\C-x" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-c\C-v" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-m" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-c\C-c" 'gnus-uu-interactive-end)
  (define-key gnus-uu-mode-map "\C-c\C-z" 
    'gnus-uu-interactive-save-current-file)
  (define-key gnus-uu-mode-map "\C-c\C-s"
    'gnus-uu-interactive-save-current-file-silent)
  (define-key gnus-uu-mode-map "\C-c\C-w" 'gnus-uu-interactive-save-all-files)
  (define-key gnus-uu-mode-map "\C-c\C-o" 'gnus-uu-interactive-save-original-file)
  (define-key gnus-uu-mode-map "\C-c\C-r" 'gnus-uu-interactive-rescan-directory)
  (define-key gnus-uu-mode-map "\C-c\C-d" 'gnus-uu-interactive-scan-directory)
  )

(defun gnus-uu-interactive-set-up-windows ()
  (let (int-buf out-buf)
    (set-buffer 
     (setq int-buf (get-buffer-create gnus-uu-interactive-buffer-name)))
    (if (not (get-buffer-window int-buf))
	(switch-to-buffer-other-window int-buf))
    (pop-to-buffer int-buf)
    (setq out-buf (get-buffer-create gnus-uu-output-buffer-name))
    (if (not (get-buffer-window out-buf))
	(progn
	  (setq gnus-uu-output-window 
		(split-window nil (- (window-height) 
				     gnus-uu-output-window-height)))
	  (set-window-buffer gnus-uu-output-window out-buf)))))

(defun gnus-uu-do-interactive (&optional dont-do-windows)
  (if (not gnus-uu-interactive-file-list) 
      (gnus-uu-enter-interactive-file "#" ""))
  (if (not dont-do-windows) (gnus-uu-interactive-set-up-windows))
  (save-excursion 
    (set-buffer (get-buffer-create gnus-uu-output-buffer-name)) 
    (erase-buffer))
  (set-buffer (get-buffer-create gnus-uu-interactive-buffer-name))
  (goto-char 1)
  (forward-line 3)
  (run-hooks 'gnus-uu-mode-hook))

(defun gnus-uu-enter-interactive-file (action file)
  (let (command)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-interactive-buffer-name))
      (if (not gnus-uu-interactive-file-list)
	  (progn
	    (erase-buffer)
	    (gnus-uu-mode)
	    (insert 
	     "# Press return to execute a command.
# Press `C-c C-c' to exit interactive view.

")))   
      (setq gnus-uu-interactive-file-list
	    (cons file gnus-uu-interactive-file-list))
;      (if (string-match (concat "^" gnus-uu-work-dir) file)
;	  (setq file (substring file (match-end 0))))
      (setq command (gnus-uu-command action file))
      (goto-char (point-max))
      (insert (format "%s\n" command)))))

(defun gnus-uu-interactive-execute ()
  "Executes the command on the current line in interactive mode."
  (interactive)
  (let (beg out-buf command)
    (beginning-of-line)
    (setq beg (point))
    (end-of-line)
    (setq command (buffer-substring beg (point)))
    (setq out-buf (get-buffer-create gnus-uu-output-buffer-name))
    (save-excursion
      (set-buffer out-buf)
      (erase-buffer)
      (insert (format "$ %s \n\n" command)))
    (setq command (format "cd %s ; %s" gnus-uu-work-dir command))
    (message "Executing...")
    (if gnus-uu-asynchronous
	(start-process "gnus-uu-view" out-buf "sh" "-c" command)
      (call-process "sh" nil out-buf nil "-c" command)
      (message ""))
    (end-of-line)
    (if (= (forward-line 1) 1)
	(progn
	  (end-of-line)
	  (insert "\n")))
    (beginning-of-line)))

(defun gnus-uu-interactive-end ()
  "This function exits interactive view mode and returns to summary mode."
  (interactive)
  (let (buf)
    (delete-window gnus-uu-output-window)
    (gnus-uu-clean-up)
    (if (not gnus-uu-asynchronous) (gnus-uu-check-for-generated-files))
    (setq buf (get-buffer gnus-uu-interactive-buffer-name))
    (if gnus-article-buffer (switch-to-buffer gnus-article-buffer))
    (if buf (kill-buffer buf))
    (pop-to-buffer gnus-summary-buffer)))


(defun gnus-uu-interactive-scan-directory (dir)
  "Read any directory and view the files.
When used in interactive mode, the files and commands will be displayed,
as usual, in the interactive mode buffer."
  (interactive "DDirectory: ")
  (setq gnus-uu-interactive-file-list nil)
  (gnus-uu-view-directory dir gnus-uu-use-interactive-view)
  (gnus-uu-do-interactive t))
  
(defun gnus-uu-interactive-rescan-directory ()
  "Reread the directory and view the files.
When used in interactive mode, the files and commands will be displayed,
as usual, in the interactive mode buffer."
  (interactive)
  (gnus-uu-interactive-scan-directory gnus-uu-work-dir))

(defun gnus-uu-interactive-save-original-file ()
  "Saves the file from whence the file on the current line came from."
  (interactive)
  (let ((files gnus-uu-list-of-files-decoded)
	(filestr "")
	file did dir)
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (if (file-exists-p file)
	  (progn
	    (if (not did)
		(progn
		  (setq dir (gnus-uu-read-directory 
			     (format "Where do you want the file%s? " 
				     (if (> (length files) 1) "s" ""))))
		  (setq did t)))
	    (setq filestr (concat filestr (gnus-uu-name-from-path file) " "))
	    (gnus-uu-save-file file dir t)))
      (if did 
	  (message "Saved %s" filestr)
	(message "Already saved.")))))

(defun gnus-uu-interactive-save-current-file-silent ()
  "Saves the file referred to on the current line in the current directory."
  (interactive)
  (gnus-uu-interactive-save-current-file t))

(defun gnus-uu-interactive-save-current-file (&optional dont-ask silent)
  "Saves the file referred to on the current line."
  (interactive)
  (let (files beg line file)
    (setq files (copy-sequence gnus-uu-interactive-file-list))
    (beginning-of-line)
    (setq beg (point))
    (end-of-line)
    (setq line (buffer-substring beg (point)))
    (while (and files
		(not (string-match 
		      (concat "" (regexp-quote (setq file (car files))) "")
		      line)))
      (setq files (cdr files)))
    (beginning-of-line)
    (forward-line 1)
    (if (not files)
	(if (not silent)
	    (progn (message "Could not find file") (sit-for 2)))
      (gnus-uu-save-file file (if dont-ask gnus-uu-current-save-dir nil) silent)
      (delete-region beg (point)))))

(defun gnus-uu-interactive-save-all-files ()
  "Saves all files referred to in the interactive buffer."
  (interactive)
  (let (dir)
    (goto-char 1)
    (setq dir (gnus-uu-read-directory "Where do you want the files? "))
    (while (not (eobp))
      (gnus-uu-interactive-save-current-file t t))))

(defun gnus-uu-mode ()
  "Major mode for editing view commands in gnus-uu.

Commands:
\\<gnus-uu-mode-map>Return, C-c C-v, C-c C-x        Execute the current command
\\[gnus-uu-interactive-end]\tEnd interactive mode
\\[gnus-uu-interactive-save-current-file]\tSave the current file
\\[gnus-uu-interactive-save-current-file-silent]\tSave the current file without asking 
\twhere to put it
\\[gnus-uu-interactive-save-all-files]\tSave all files
\\[gnus-uu-interactive-save-original-file]\tSave the original file: If the files
\toriginated in an archive, the archive 
\tfile is saved.
\\[gnus-uu-interactive-rescan-directory]\tRescan the directory
\\[gnus-uu-interactive-scan-directory]\tScan any directory
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-uu-mode-map)   
  (setq mode-name "gnus-uu")         
  (setq major-mode 'gnus-uu-mode)    
)

  (define-key gnus-uu-mode-map "\C-c\C-x" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-c\C-v" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-m" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-c\C-c" 'gnus-uu-interactive-end)
  (define-key gnus-uu-mode-map "\C-cs" 
    'gnus-uu-interactive-save-current-file)
  (define-key gnus-uu-mode-map "\C-c\C-s"
    'gnus-uu-interactive-save-current-file-silent)
  (define-key gnus-uu-mode-map "\C-c\C-a" 'gnus-uu-interactive-save-all-files)
  (define-key gnus-uu-mode-map "\C-c\C-o" 'gnus-uu-interactive-save-original-file)


;; Major mode for posting encoded articles.

(require 'sendmail)
(require 'rnews)

; Any function that is to be used as and encoding method will take two
; parameters: PATH-NAME and FILE-NAME. (E.g. "/home/gaga/spiral.jpg"
; and "spiral.jpg", respectively.) The function should return nil if
; the encoding wasn't successful.
(defvar gnus-uu-post-encode-method 'gnus-uu-post-encode-uuencode
  "Function used for encoding binary files.
There are three functions supplied with gnus-uu for encoding files:
`gnus-uu-post-encode-uuencode', which does straight uuencoding;
`gnus-uu-post-encode-mime', which encodes with base64 and adds MIME 
headers; and `gnus-uu-post-encode-mime-uuencode', which encodes with 
uuencode and adds MIME headers.")

(defvar gnus-uu-post-include-before-composing nil
  "Non-nil means that gnus-uu will ask for a file to encode before you compose the article.
If this variable is t, you can either include an encoded file with
\\<gnus-uu-post-reply-mode-map>\\[gnus-uu-post-insert-binary-in-article] or have one included for you when you post the article.")

(defvar gnus-uu-post-length 990
  "Maximum length of an article.
The encoded file will be split into how many articles it takes to
post the entire file.")

(defvar gnus-uu-post-threaded nil
  "Non-nil means that gnus-uu will post the encoded file in a thread.
This may not be smart, as no other decoder I have seen are able to
follow threads when collecting uuencoded articles. (Well, I have seen
one package that does that - gnus-uu, but somehow, I don't think that 
counts...) Default is nil.")

(defvar gnus-uu-post-separate-description t
  "Non-nil means that the description will be posted in a separate article.
The first article will typically be numbered (0/x). If this variable
is nil, the description the user enters will be included at the 
beginning of the first article, which will be numbered (1/x). Default 
is t.")

(defconst gnus-uu-post-binary-separator "--binary follows this line--")
(defvar gnus-uu-post-message-id nil)
(defvar gnus-uu-post-inserted-file-name nil)
(defvar gnus-uu-winconf-post-news nil)

; The following map and mode was taken from rnewspost.el and edited
; somewhat.
(defvar gnus-uu-post-reply-mode-map () "Mode map used by gnus-uu-post-reply.")
(or gnus-uu-post-reply-mode-map
    (progn
      (setq gnus-uu-post-reply-mode-map (make-keymap))
      (define-key gnus-uu-post-reply-mode-map "\C-c?" 'describe-mode)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-f\C-d" 
	'news-reply-distribution)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-f\C-k" 
	'news-reply-keywords)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-f\C-n" 
	'news-reply-newsgroups)
      
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-f\C-f" 
	'news-reply-followup-to)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-f\C-s" 'mail-subject)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-f\C-a" 
	'gnus-uu-post-reply-summary)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-r" 
	'news-caesar-buffer-body)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-w" 'news-reply-signature)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-y" 
	'news-reply-yank-original)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-q" 
	'mail-fill-yanked-message)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-c" 
	'gnus-uu-post-news-inews)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-s" 
	'gnus-uu-post-news-inews)
      (define-key gnus-uu-post-reply-mode-map "\C-c\C-i" 
	'gnus-uu-post-insert-binary-in-article)
      ))

; This mode was taken from rnewspost.el and modified slightly.
(defun gnus-uu-post-reply-mode ()
  "Major mode for editing binary news to be posted on USENET.
First-time posters are asked to please read the articles in newsgroup:
                                                     news.announce.newusers .

Like news-reply-mode, which is like Text Mode, but with these
additional commands:

\\<gnus-uu-post-reply-mode-map>\\[gnus-uu-post-news-inews]  post the message.
C-c C-f	 move to a header field (and create it if there isn't):
	 C-c C-f C-n  move to Newsgroups:	C-c C-f C-s  move to Subj:
	 C-c C-f C-f  move to Followup-To:      C-c C-f C-k  move to Keywords:
	 C-c C-f C-d  move to Distribution:	C-c C-f C-a  move to Summary:
C-c C-y  news-reply-yank-original (insert current message, in NEWS).
C-c C-q  mail-fill-yanked-message (fill what was yanked).
C-c C-r  caesar rotate all letters by 13 places in the article's body (rot13).
\\[gnus-uu-post-insert-binary-in-article]  encode and include a file in this article.

This mode is almost identical to news-reply-mode, but has some
additional commands for treating encoded binary articles. In
particular, \\[gnus-uu-post-news-inews] will ask for a file to include, if
one hasn't been included already. It will post, first, the message
composed, and then it will post as many additional articles it takes
to post the entire encoded files.

   Relevant Variables

   `gnus-uu-post-encode-method' 
   There are three functions supplied with gnus-uu for encoding files:
   `gnus-uu-post-encode-uuencode', which does straight uuencoding;
   `gnus-uu-post-encode-mime', which encodes with base64 and adds MIME 
   headers; and `gnus-uu-post-encode-mime-uuencode', which encodes with 
   uuencode and adds MIME headers.
 
   `gnus-uu-post-include-before-composing'
   Non-nil means that gnus-uu will ask for a file to encode before you
   compose the article.  If this variable is t, you can either include
   an encoded file with `C-c C-i' or have one included for you when you 
   post the article.

   `gnus-uu-post-length'
   Maximum length of an article. The encoded file will be split into how 
   many articles it takes to post the entire file.

   `gnus-uu-post-separate-description'
   Non-nil means that the description will be posted in a separate 
   article. The first article will typically be numbered (0/x). If 
   this variable is nil, the description the user enters will be 
   included at the beginning of the first article, which will be 
   numbered (1/x). Default is t.

   `gnus-uu-post-threaded'
   Non-nil means that gnus-uu will post the encoded file in a thread.
   This may not be smart, as no other decoder I have seen are able to
   follow threads when collecting uuencoded articles. (Well, I have seen
   one package that does that - gnus-uu, but somehow, I don't think that 
   counts...) Default is nil.
"
  (interactive)
  ;; require...
  (or (fboundp 'mail-setup) (load "sendmail"))
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map gnus-uu-post-reply-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'gnus-uu-post-reply-mode)
  (setq mode-name "Gnus UU News")
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat mail-header-separator "$\\|"
				paragraph-start))
  (setq paragraph-separate (concat mail-header-separator "$\\|"
				   paragraph-separate))
  (run-hooks 'text-mode-hook 'gnus-uu-post-reply-mode-hook))

(defun gnus-uu-post-news ()
  "Compose an article and post an encoded file."
  (interactive)
  (setq gnus-uu-post-inserted-file-name nil)
  (setq gnus-uu-winconf-post-news (current-window-configuration))
  (let (news-reply-mode)
    (fset 'news-reply-mode 'gnus-uu-post-reply-mode)
    (gnus-summary-post-news)
    (if gnus-uu-post-include-before-composing
	(save-excursion (setq gnus-uu-post-inserted-file-name 
			      (gnus-uu-post-insert-binary))))))

(defun gnus-uu-post-insert-binary-in-article ()
  "Inserts an encoded file in the buffer.
The user will be asked for a file name."
  (interactive)
  (if (not (eq (current-buffer) (get-buffer gnus-post-news-buffer)))
      (error "Not in post-news buffer"))
  (save-excursion 
    (setq gnus-uu-post-inserted-file-name (gnus-uu-post-insert-binary))))

; Encodes with uuencode and substitutes all spaces with backticks.
(defun gnus-uu-post-encode-uuencode (path file-name)
  (if (gnus-uu-post-encode-file "uuencode" path file-name)
      (progn
	(goto-char 1)
	(forward-line 1)
	(while (re-search-forward " " nil t)
	  (replace-match "`"))
	t)))

; Encodes with uuencode and adds MIME headers.
(defun gnus-uu-post-encode-mime-uuencode (path file-name)
  (if (gnus-uu-post-encode-uuencode path file-name)
      (progn
	(gnus-uu-post-make-mime file-name "x-uue")
	t)))

; Encodes with base64 and adds MIME headers
(defun gnus-uu-post-encode-mime (path file-name)
  (if (gnus-uu-post-encode-file "mmencode" path file-name)
      (progn
	(gnus-uu-post-make-mime file-name "base64")
	t)))

; Adds MIME headers.
(defun gnus-uu-post-make-mime (file-name encoding)
  (goto-char 1)
  (insert (format "Content-Type: %s; name=\"%s\"\n" 
		  (gnus-uu-choose-action file-name gnus-uu-ext-to-mime-list) 
		  file-name))
  (insert (format "Content-Transfer-Encoding: %s\n\n" encoding))
  (save-restriction
    (set-buffer gnus-post-news-buffer)
    (goto-char 1)
    (re-search-forward mail-header-separator)
    (beginning-of-line)
    (forward-line -1)
    (narrow-to-region 1 (point))
    (or (mail-fetch-field "mime-version")
	(progn
	  (widen)
	  (insert "MIME-Version: 1.0\n")))
    (widen)))

; Encodes a file PATH with COMMAND, leaving the result in the
; current buffer.
(defun gnus-uu-post-encode-file (command path file-name)
  (= 0 (call-process "sh" nil t nil "-c" 
		     (format "%s %s %s" command path file-name))))

(defun gnus-uu-post-news-inews ()
  "Posts the composed news article and encoded file.
If no file has been included, the user will be asked for a file."
  (interactive)
  (if (not (eq (current-buffer) (get-buffer gnus-post-news-buffer)))
      (error "Not in post news buffer"))

  (let (file-name)

    (if gnus-uu-post-inserted-file-name
	(setq file-name gnus-uu-post-inserted-file-name)
      (setq file-name (gnus-uu-post-insert-binary)))
  
    (if gnus-uu-post-threaded
	(let ((gnus-required-headers 
	       (if (memq 'Message-ID gnus-required-headers)
		   gnus-required-headers
		 (cons 'Message-ID gnus-required-headers)))
	      gnus-inews-article-hook elem)

	  (setq gnus-inews-article-hook (if (listp gnus-inews-article-hook)
					    gnus-inews-article-hook
					  (list gnus-inews-article-hook)))
	  (setq gnus-inews-article-hook 
		(cons
		 '(lambda ()
		    (save-excursion
		      (goto-char 1)
		      (if (re-search-forward "^Message-ID: \\(.*\\)$" nil t)
			  (setq gnus-uu-post-message-id 
				(buffer-substring 
				 (match-beginning 1) (match-end 1)))
			(setq gnus-uu-post-message-id nil))))
		 gnus-inews-article-hook))
	  (gnus-uu-post-encoded file-name t))
      (gnus-uu-post-encoded file-name nil)))
  (setq gnus-uu-post-inserted-file-name nil)
  (and gnus-uu-winconf-post-news
       (set-window-configuration gnus-uu-winconf-post-news)))
      
; Asks for a file to encode, encodes it and inserts the result in
; the current buffer. Returns the file name the user gave.
(defun gnus-uu-post-insert-binary ()
  (let ((uuencode-buffer-name "*uuencode buffer*")
	file-path post-buf uubuf file-name)

    (setq file-path (read-file-name 
		     "What file do you want to encode? "))
    (if (not (file-exists-p file-path))
	(error "%s: No such file" file-path))

    (goto-char (point-max))
    (insert (format "\n%s\n" gnus-uu-post-binary-separator))
    
    (if (string-match "^~/" file-path)
	(setq file-path (concat "$HOME" (substring file-path 1))))
    (if (string-match "/[^/]*$" file-path)
	(setq file-name (substring file-path (1+ (match-beginning 0))))
      (setq file-name file-path))

    (unwind-protect
	(if (save-excursion
	      (set-buffer (setq uubuf 
				(get-buffer-create uuencode-buffer-name)))
	      (erase-buffer)
	      (funcall gnus-uu-post-encode-method file-path file-name))
	    (insert-buffer uubuf)
	  (error "Encoding unsuccessful"))
      (kill-buffer uubuf))
    file-name))

; Posts the article and all of the encoded file.
(defun gnus-uu-post-encoded (file-name &optional threaded)
  (let ((send-buffer-name "*uuencode send buffer*")
	(encoded-buffer-name "*encoded buffer*")
	(top-string "[ cut here %s (%s %d/%d) %s gnus-uu ]")
	(separator (concat mail-header-separator "\n\n"))
	file uubuf length parts header i end beg
	beg-line minlen buf post-buf whole-len beg-binary end-binary)

    (setq post-buf (current-buffer))

    (goto-char 1)
    (if (not (re-search-forward 
	      (if gnus-uu-post-separate-description 
		  gnus-uu-post-binary-separator 
		mail-header-separator) nil t))
	(error "Internal error: No binary/header separator"))
    (beginning-of-line)
    (forward-line 1)
    (setq beg-binary (point))
    (setq end-binary (point-max))

    (save-excursion 
      (set-buffer (setq uubuf (get-buffer-create encoded-buffer-name)))
      (erase-buffer)
      (insert-buffer-substring post-buf beg-binary end-binary)
      (goto-char 1)
      (setq length (count-lines 1 (point-max)))
      (setq parts (/ length gnus-uu-post-length))
      (if (not (< (% length gnus-uu-post-length) 4))
	  (setq parts (1+ parts))))

    (if gnus-uu-post-separate-description
	(forward-line -1))
    (kill-region (point) (point-max))

    (goto-char 1)
    (search-forward mail-header-separator nil t)
    (beginning-of-line)
    (setq header (buffer-substring 1 (point)))

    (goto-char 1)
    (if (not gnus-uu-post-separate-description)
	()
      (if (and (not threaded) (re-search-forward "^Subject: " nil t))
	  (progn
	    (end-of-line)
	    (insert (format " (0/%d)" parts))))
      (gnus-inews-news))

    (save-excursion
      (setq i 1)
      (setq beg 1)
      (while (not (> i parts))
	(set-buffer (get-buffer-create send-buffer-name))
	(erase-buffer)
	(insert header)
	(if (and threaded gnus-uu-post-message-id)
	    (insert (format "References: %s\n" gnus-uu-post-message-id)))
	(insert separator)
	(setq whole-len
	      (- 62 (length (format top-string "" file-name i parts ""))))
	(if (> 1 (setq minlen (/ whole-len 2)))
	    (setq minlen 1))
	(setq 
	 beg-line 
	 (format top-string
		 (make-string minlen ?-) 
		 file-name i parts
		 (make-string 
		  (if (= 0 (% whole-len 2)) (1- minlen) minlen) ?-)))

	(goto-char 1)
	(if (not (re-search-forward "^Subject: " nil t))
	    ()
	  (if (not threaded)
	      (progn
		(end-of-line)
		(insert (format " (%d/%d)" i parts)))
	    (if (or (and (= i 2) gnus-uu-post-separate-description)
		    (and (= i 1) (not gnus-uu-post-separate-description)))
		(replace-match "Subject: Re: "))))
		  
	(goto-char (point-max))
	(save-excursion
	  (set-buffer uubuf)
	  (goto-char beg)
	  (if (= i parts)
	      (goto-char (point-max))
	    (forward-line gnus-uu-post-length))
	  (if (and (= (1+ i) parts) (< (count-lines (point) (point-max)) 4))
	      (forward-line -4))
	  (setq end (point)))
	(insert-buffer-substring uubuf beg end)
	(insert beg-line)
	(insert "\n")
	(setq beg end)
	(setq i (1+ i))
	(goto-char 1)
	(re-search-forward mail-header-separator nil t)
	(beginning-of-line)
	(forward-line 2)
	(if (re-search-forward gnus-uu-post-binary-separator nil t)
	    (progn 
	      (replace-match "")
	      (forward-line 1)))
	(insert beg-line)
	(insert "\n")
	(gnus-inews-news)))

    (and (setq buf (get-buffer send-buffer-name))
	 (kill-buffer buf))
    (and (setq buf (get-buffer encoded-buffer-name))
	 (kill-buffer buf))

    (if (not gnus-uu-post-separate-description)
	(progn
	  (set-buffer-modified-p nil)
	  (and (fboundp 'bury-buffer) (bury-buffer))))))

(provide 'gnus-uu)

;; gnus-uu.el ends here
