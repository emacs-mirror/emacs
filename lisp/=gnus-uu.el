;;; gnus-uu.el --- extract, view or save (uu)encoded files from gnus

;; Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@ifi.uio.no>
;; Created: 2 Oct 1993
;; Version: v2.5.2
;; Last Modified: 1994/04/13
;; Keyword: news

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
;; Typing `C-c C-v C-v' (`gnus-uu-decode-and-view') in the summary
;; buffer will try to find all articles in the same series, uudecode
;; them and view the resulting file(s).
;;
;; gnus-uu guesses what articles are in the series according to the
;; following simple rule: The subjects must be identical, except for
;; the last two numbers of the line.
;;
;; For example: If you choose a subject called "cat.gif (2/3)" gnus-uu
;; will find all the articles that matches "^cat.gif
;; ([0-9]+/[0-9]+).*$".  Subjects that are nonstandard, like "cat.gif
;; (2/3) Part 6 of a series", will not be properly recognized by 'C-c
;; C-v C-v', and you have to mark the articles manually with '#'.
;;
;; Typing `C-c C-v v' (`gnus-uu-decode-and-save') will do the same as
;; `C-c C-v C-v', except that it will not display the resulting file,
;; but save it instead.
;;
;; Typing `C-c C-v s' (`gnus-uu-shar-and-save') does the same as `C-c
;; C-v v', and `C-c C-v C-s' (`gnus-uu-shar-and-view') does the same
;; as `C-c C-v C-v', except that they unshar files instead, i. e. run
;; them through /bin/sh. Most shar files can be viewed and/or saved
;; with the normal uudecode commands, which is much safer, as no
;; foreign code is run.
;;
;; `#' (`gnus-uu-mark-article') marks an article for later
;; decoding/unsharing/saving/viewing. The files will be decoded in the
;; sequence they were marked. To decode the files after you've marked
;; the articles you are interested in, type the corresponding key
;; strokes as the normal decoding commands, but put a `M-' in the last
;; keystroke. For instance, to perform a standard uudecode and view,
;; you would type `C-c C-v C-v'. To perform a marked uudecode and
;; view, say `C-v C-v M-C-v'. All the other view and save commands are
;; handled the same way; marked uudecode and save is then `C-c C-v
;; M-v'.
;;
;; `M-#' (`gnus-uu-unmark-article') will remove the mark from a
;; previosly marked article.
;;
;; `C-c C-v C-u' (`gnus-uu-unmark-all-articles') will remove the mark from
;; all marked articles.
;;
;; `C-c C-v C-r' (`gnus-uu-mark-by-regexp') will prompt for a regular
;; expression and mark (forward) all articles matching that regular
;; expression.
;;
;; There's an additional way to reach the decoding functions to make
;; future expansions easier: `C-c C-v C-m'
;; (`gnus-uu-multi-decode-and-view') and the corresponding save, marked
;; view and marked save keystrokes, `C-c C-v m', `C-c C-v M-C-m' and
;; `C-c C-v M-m' respectively. You will be prompted for decoding
;; method, like uudecode, shar, binhex or plain save. Note that
;; methods like binhex and save doesn't have view modes; even if you
;; issue a view command (`C-c C-v C-m' and "binhex"), gnus-uu will
;; just save the resulting binhex file.
;;
;; `C-c C-v C-b' (`gnus-uu-decode-and-show-in-buffer') will decode the
;; current article and display the results in an emacs buffer. This
;; might be useful if there's jsut some text in the current article
;; that has been uuencoded by some perverse poster.
;;
;; `C-c C-v a' (`gnus-uu-decode-and-save-all-articles') looks at all the
;; articles in the current newsgroup and tries to uudecode everything
;; it can find. The user will be prompted for a directory where the
;; resulting files (if any) will be stored. `C-c C-v M-a' only looks
;; at unread article. `C-c C-v w' does the same as `C-c C-v a', but
;; also marks as read all articles it has peeked through, even if they
;; weren't uuencoded articles. `C-c C-v M-w' is, as you might have
;; guessed, similar to `C-c C-v M-a'.
;;
;; `C-c C-v C-l' (`gnus-uu-edit-begin-line') lets you edit the begin
;; line of the current buffer. Useful to change an incorrect suffix or
;; an incorrect begin line.
;;
;;
;; When using the view commands, `C-c C-v C-v' for instance, gnus-uu
;; will (normally, see below) try to view the file according to the
;; rules given in `gnus-uu-default-view-rules' and
;; `gnus-uu-user-view-rules'. If it recognises the file, it will
;; display it immediately. If the file is some sort of archive,
;; gnus-uu will attempt to unpack the archive and see if any of the
;; files in the archive can be viewed. For instance, if you have a
;; gzipped tar file "pics.tar.gz" containing the files "pic1.jpg" and
;; "pic2.gif", gnus-uu will uncompress and detar the main file, and
;; then view the two pictures. This unpacking process is recursive, so
;; if the archive contains archives of archives, it'll all be
;; unpacked.
;;
;; If the view command doesn't recognise the file type, or can't view
;; it because you don't have the viewer, or can't view *any* of the
;; files in the archive, the user will be asked if she wishes to have
;; the file saved somewhere. Note that if the decoded file is an
;; archive, and gnus-uu manages to view some of the files in the
;; archive, it won't tell the user that there were some files that
;; were unviewable. See "Interactive view" for a different approach.
;;
;;
;; Note that gnus-uu adds a function to `gnus-exit-group-hook' to
;; clear the list of marked articles and check for any generated files
;; that might have escaped deletion if the user typed `C-g'.
;;
;;
;; `C-c C-v C-a' (`gnus-uu-toggle-asynchronous') toggles the
;; `gnus-uu-asynchronous' variable. See below for explanation.
;;
;; `C-c C-v C-q' (`gnus-uu-toggle-query') toggles the
;; `gnus-uu-ask-before-view' variable. See below for explanation.
;;
;; `C-c C-v C-p' (`gnus-uu-toggle-always-ask') toggles the
;; `gnus-uu-view-and-save' variable. See below for explanation.
;;
;; `C-c C-v C-k' (`gnus-uu-toggle-kill-carriage-return') toggles the
;; `gnus-uu-kill-carriage-return' variable. See below for explanation.
;;
;; `C-c C-v C-i' (`gnus-uu-toggle-interactive-view') toggles interactive
;; mode. If it is turned on, gnus-uu won't view files immediately but
;; give you a buffer with the default commands and files and lets you
;; edit the commands and execute them at leisure.
;;
;; `C-c C-v C-t' (`gnus-uu-toggle-any-variable') is an interface to the
;; five toggle commands listed above.
;;
;; `gnus-uu-toggle-correct-stripped-articles' toggles whether to check
;; and correct uuencoded articles that may have had trailing spaces
;; stripped by mailers.
;;
;;
;; Customization
;;
;; To load this file when starting gnus, put sumething like the
;; following in your .emacs file:
;;
;;   (setq gnus-group-mode-hook
;;      '(lambda () (load "gnus-uu")))
;;
;; To make gnus-uu use, for instance, "xli" to view JPEGs and GIFs,
;; put this in your .emacs file:
;;
;;   (setq gnus-uu-user-view-rules 
;;	 (list
;;	  '("jpg$\\|gif$" "xli") 
;;	  ))
;;
;; This variable is a list where each list item is a list containing
;; two strings. The first string is a regular expression. If the file
;; name is matched by this expression, the command given in the
;; second string is executed on this file. If the command contains
;; "%s", the file will be inserted there in the command string. Eg.
;; "giftoppm %s | xv -" will result in the file name being inserted at
;; the "%s".
;;
;; If you don't want to display certain file types, like if you
;; haven't got sound capabilities, you could put something like
;;
;;   (setq gnus-uu-user-view-rules 
;;	 (list
;;	  '("au$\\|voc$\\|wav$" nil) 
;;	  ))
;;
;; in your .emacs file.
;;
;; There's a similar variable called `gnus-uu-user-archive-rules'
;; which gives a list of unarcers to use when looking inside archives
;; for files to display.
;;
;; If you don't want gnus-uu to look inside archives for files to
;; display, say
;;
;;   (setq gnus-uu-do-not-unpack-archives t)
;;
;;
;; If you want gnus-uu to ask you if you want to save a file after
;; viewing, say
;;
;;   (setq gnus-uu-view-and-save t)
;;
;;
;; If you don't want to wait for the viewing command to finish before
;; returning to emacs, say
;;
;;   (setq gnus-uu-asynchronous t)
;;
;;
;; This can be useful if you're viewing long .mod files, for instance,
;; which often takes several minutes. Note, however, that since
;; gnus-uu doesn't ask, and if you are viewing an archive with lots of
;; viewable files, you'll get them all up more or less at once, which
;; can be confusing, to say the least. To get gnus-uu to ask you
;; before viewing a file, say
;;
;;   (setq gnus-uu-ask-before-view t)
;;
;; You can set this variable even if you're not using asynchronous
;; viewing, of course.
;;
;; If the articles has been posted by some numbscull with a PC (isn't
;; that a bit redundant, though?) and there's lots of carriage returns
;; everywhere, say
;;
;;   (setq gnus-uu-kill-carriage-return t)
;;
;; If you want gnus-uu to ignore the default file rules when viewing,
;; for instance if there's several file types that you can't view, set
;; `gnus-uu-ignore-default-view-rules' to t. There's a similar
;; variable to disable the default unarchive rule list,
;; `gnus-uu-ignore-default-archive-rules'.
;;
;; If you want a more interactive approach to file viewing, say
;;
;;   (setq gnus-uu-use-interactive-view t)
;;
;; If this variable is set, whenever you type `C-c C-v C-v' (or any of
;; the other view commands), gnus-uu will present you with a buffer
;; with the default actions and file names after decoding. You can
;; edit the command lines and execute them in a convenient fashion.
;; The output from the commands will be displayed in a small window at
;; the bottom of the emacs window. End interactive mode by typing `C-c
;; C-c' in the view window.
;;
;; If you want gnus-uu to unmark articles that you have asked to
;; decode, but can't be decoded (if, for instance, the articles aren't
;; uuencoded files or the posting is incomplete), say
;;
;;   (setq gnus-uu-unmark-articles-not-decoded t)
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
;;
;; Default keymap overview:
;; 
;; All commands start with `C-c C-v'. The difference is in the third
;; keystroke.  All view commands are `C-LETTER'. All save commands are
;; just `LETTER'. All marked commands are the same as the unmarked
;; commands, except that they have `M-' before in the last keystroke.
;;
;; `C-c C-v C-v'      gnus-uu-decode-and-view
;; `C-c C-v v'        gnus-uu-decode-and-save
;; `C-c C-v C-s'      gnus-uu-shar-and-view
;; `C-c C-v s'        gnus-uu-shar-and-save
;; `C-c C-v C-m'      gnus-uu-multi-decode-and-view
;; `C-c C-v m'        gnus-uu-multi-decode-and-save
;;
;; `C-c C-v C-b'      gnus-uu-decode-and-show-in-buffer
;; `C-c C-v C-l'      gnus-uu-edit-begin-line
;; `C-c C-v M-a'      gnus-uu-decode-and-save-all-unread-articles
;; `C-c C-v a'        gnus-uu-decode-and-save-all-articles
;; `C-c C-v M-w'      gnus-uu-decode-and-save-all-unread-articles-and-mark
;; `C-c C-v w'        gnus-uu-decode-and-save-all-articles-and-mark
;;
;; `#'                gnus-uu-mark-article
;; `M-#'              gnus-uu-unmark-article
;; `C-c C-v C-u'      gnus-uu-unmark-all-articles
;; `C-c C-v C-r'      gnus-uu-mark-by-regexp
;; `C-c C-v M-C-v'    gnus-uu-marked-decode-and-view
;; `C-c C-v M-v'      gnus-uu-marked-decode-and-save
;; `C-c C-v M-C-s'    gnus-uu-marked-shar-and-view
;; `C-c C-v M-s'      gnus-uu-marked-shar-and-save
;; `C-c C-v M-C-m'    gnus-uu-marked-multi-decode-and-view
;; `C-c C-v M-m'      gnus-uu-marked-multi-decode-and-save
;;
;; `C-c C-v C-a'      gnus-uu-toggle-asynchronous
;; `C-c C-v C-q'      gnus-uu-toggle-query
;; `C-c C-v C-p'      gnus-uu-toggle-always-ask
;; `C-c C-v C-k'      gnus-uu-toggle-kill-carriage-return
;; `C-c C-v C-i'      gnus-uu-toggle-interactive-view
;; `C-c C-v C-t'      gnus-uu-toggle-any-variable

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

(define-key gnus-uu-ctl-map "\M-\C-v" 'gnus-uu-marked-decode-and-view)
(define-key gnus-uu-ctl-map "\M-v" 'gnus-uu-marked-decode-and-save)
(define-key gnus-uu-ctl-map "\M-\C-s" 'gnus-uu-marked-shar-and-view)
(define-key gnus-uu-ctl-map "\M-s" 'gnus-uu-marked-shar-and-save)
(define-key gnus-uu-ctl-map "\M-\C-m" 'gnus-uu-marked-multi-decode-and-view)
(define-key gnus-uu-ctl-map "\M-m" 'gnus-uu-marked-multi-decode-and-save)

(define-key gnus-uu-ctl-map "\C-a" 'gnus-uu-toggle-asynchronous)
(define-key gnus-uu-ctl-map "\C-q" 'gnus-uu-toggle-query)
(define-key gnus-uu-ctl-map "\C-p" 'gnus-uu-toggle-always-ask)
(define-key gnus-uu-ctl-map "\C-k" 'gnus-uu-toggle-kill-carriage-return)
(define-key gnus-uu-ctl-map "\C-i" 'gnus-uu-toggle-interactive-view)
(define-key gnus-uu-ctl-map "\C-t" 'gnus-uu-toggle-any-variable)

(define-key gnus-uu-ctl-map "\C-l" 'gnus-uu-edit-begin-line)

(define-key gnus-uu-ctl-map "\M-a" 'gnus-uu-decode-and-save-all-unread-articles)
(define-key gnus-uu-ctl-map "a" 'gnus-uu-decode-and-save-all-articles)
(define-key gnus-uu-ctl-map "\M-w" 'gnus-uu-decode-and-save-all-unread-articles-and-mark)
(define-key gnus-uu-ctl-map "w" 'gnus-uu-decode-and-save-all-articles-and-mark)

;(load "rnewspost")
;(define-key news-reply-mode-map "\C-c\C-v" 'gnus-uu-uuencode-and-post)

;; Dummy function gnus-uu

(defun gnus-uu ()
  "gnus-uu is a package for uudecoding and viewing articles.

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

\\[gnus-uu-decode-and-show-in-buffer]\tDecode the current article and view the result in a buffer
\\[gnus-uu-edit-begin-line]\tEdit the 'begin' line of an uuencoded article

\\[gnus-uu-decode-and-save-all-unread-articles]\tDecode and save all unread articles
\\[gnus-uu-decode-and-save-all-articles]\tDecode and save all articles
\\[gnus-uu-decode-and-save-all-unread-articles-and-mark]\tDecode and save all unread articles and catch up
\\[gnus-uu-decode-and-save-all-articles-and-mark]\tDecode and save all articles and catch up

\\[gnus-uu-mark-article]\tMark the current article for decoding
\\[gnus-uu-unmark-article]\tUnmark the current article
\\[gnus-uu-unmark-all-articles]\tUnmark all articles
\\[gnus-uu-mark-by-regexp]\tMark articles for decoding by regexp
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
\\[gnus-uu-toggle-any-variable]\tToggle any of the things above

User configurable variables:

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

   `gnus-uu-tmp-dir'
     Where gnus-uu does its work.

   `gnus-uu-do-not-unpack-archives'
     Non-nil means that gnus-uu won't peek inside archives looking for
     files to dispay.

   `gnus-uu-view-and-save'
     Non-nil means that the user will always be asked to save a file
     after viewing it.

   `gnus-uu-asynchronous'
     Non-nil means that files will be viewed asynchronously.

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
     Non-nil means that gnus-uu will use interactive viewing mode."
  (interactive)
  )

;; Default viewing action rules

(defvar gnus-uu-default-view-rules 
  (list 
   '("\\.\\(jpe?g\\|gif\\|tiff?\\|p[pgb]m\\|xwd\\|xbm\\|pcx\\)$" "xv")
   '("\\.tga$" "tgatoppm %s | xv -")
   '("\\.te?xt$\\|\\.doc$\\|read.*me" "xterm -e less")
   '("\\.fli$" "xflick")
   '("\\.\\(wav\\|aiff\\|hcom\\|u[blw]\\|s[bfw]\\|voc\\|smp\\)$" 
     "sox -v .5 %s -t .au -u - > /dev/audio")
   '("\\.au$" "cat %s > /dev/audio")
   '("\\.mod$" "str32")
   '("\\.ps$" "ghostview")
   '("\\.dvi$" "xdvi")
   '("\\.1$" "xterm -e man -l")
   '("\\.html$" "xmosaic")
   '("\\.mpe?g$" "mpeg_play")
   '("\\.\\(tar\\|arj\\|zip\\|zoo\\|arc\\|gz\\|Z\\|lzh\\|ar\\)$" 
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
   '("\\.te?xt$\\|\\.doc$\\|read.*me\\|\\.c?$\\|\\.h$\\|\\.bat$\\|\\.asm$\\|makefile" "cat %s | sed s/
//g")
   '("\\.pas$" "cat %s | sed s/
//g")
   ))


;; Default unpacking commands

(defvar gnus-uu-default-archive-rules 
  (list '("\\.tar$" "tar xf")
	'("\\.zip$" "unzip")
	'("\\.ar$" "ar x")
	'("\\.arj$" "unarj x")
	'("\\.zoo$" "zoo -e")
	'("\\.lzh$" "lha x")
	'("\\.Z$" "uncompress")
	'("\\.gz$" "gunzip")
	'("\\.arc$" "arc -x"))
  )

(defvar gnus-uu-user-archive-rules nil
  "A list that can be set to override the default archive unpacking commands.
To use, for instance, 'untar' to unpack tar files and 'zip -x' to
unpack zip files, say the following:
  (setq gnus-uu-user-archive-rules 
    (list '(\"\\\\.tar$\" \"untar\")
          '(\"\\\\.zip$\" \"zip -x\")))"
  )


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


;; Internal variables

(defconst gnus-uu-begin-string "^begin[ \t]+[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defconst gnus-uu-end-string "^end[ \t]*$")
(defconst gnus-uu-body-line
"^M.............................................................?$")
(defconst gnus-uu-shar-begin-string "^#! */bin/sh")

(defvar gnus-uu-shar-file-name nil)
(defconst gnus-uu-shar-name-marker "begin [0-7][0-7][0-7][ \t]+\\(\\(\\w\\|\\.\\)*\\b\\)")
(defvar gnus-uu-shar-directory nil)

(defvar gnus-uu-file-name nil)
(defconst gnus-uu-uudecode-process nil)

(defvar gnus-uu-interactive-file-list nil)
(defvar gnus-uu-marked-article-list nil)
(defvar gnus-uu-generated-file-list nil)

(defconst gnus-uu-interactive-buffer-name "*gnus-uu interactive*")
(defconst gnus-uu-output-buffer-name "*Gnus UU Output*")
(defconst gnus-uu-result-buffer "*Gnus UU Result Buffer*")

(defconst gnus-uu-error-during-unarching nil)

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


;; Decode and show in buffer

(defun gnus-uu-decode-and-show-in-buffer ()
  "Uudecodes the current article and displays the result in a buffer.
Might be useful if someone has, for instance, some text uuencoded in
their sigs. (Stranger things have happened.)"
  (interactive)
  (let ((uu-buffer (get-buffer-create gnus-uu-output-buffer-name))
	list-of-articles file-name)
    (save-excursion
      (and 
       (setq list-of-articles (list gnus-current-article))
       (gnus-uu-grab-articles list-of-articles 'gnus-uu-uustrip-article-as)
       (setq file-name (gnus-uu-decode gnus-uu-tmp-dir))
       (progn
	 (save-excursion
	   (set-buffer uu-buffer)
	   (erase-buffer)
	   (insert-file-contents file-name))
	 (set-window-buffer (get-buffer-window gnus-article-buffer) 
			    uu-buffer)
	 (message (format "Showing file %s in buffer" file-name))
	 (delete-file file-name))))))


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
    (message "(a)sync, (q)uery, (p)ask, (k)ill CR, (i)nteractive, (u)nmark, (c)orrect")
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
    (if (= rep ?i)
	(gnus-uu-toggle-interactive-view))))


;; Edit line

(defun gnus-uu-edit-begin-line ()
  "Edit the begin line of the current article."
  (interactive)
  (let ((buffer-read-only nil)
	begin b)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (goto-line 1)
      (if (not (re-search-forward "begin " nil t))
	  (progn (message "No begin line in the current article") (sit-for 2))
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
  (let (decode-type)
    (message "(u)udecode, (s)har, s(a)ve, (b)inhex: ")
    (setq decode-type (read-char))
    (if (= decode-type ?
) (setq decode-type ?u))
    (if (= decode-type ?u)
	(gnus-uu-decode-and-view-or-save view marked)
      (if (= decode-type ?s)
	  (gnus-uu-unshar-and-view-or-save view marked)
	(if (= decode-type ?b)
	    (gnus-uu-binhex-and-save view marked)
	  (if (= decode-type ?a)
	      (gnus-uu-save-articles view marked)
	    (message (format "Unknown decode method '%c'." decode-type))
	    (sit-for 2)))))))


;; uuencode and post

(defconst gnus-uu-uuencode-post-length 90)

(defun gnus-uu-post ()
  (interactive)
  (let ((uuencode-buffer-name "*uuencode buffer*")
	(send-buffer-name "*uuencode send buffer*")
	(top-string "[ cut here %s (%s %d/%d) %s gnus-uu ]")
	file uubuf short-file length parts header i end beg
	beg-line minlen buf post-buf whole-len)
    (setq file (read-file-name 
		"What file do you want to uuencode and post? " "~/Unrd.jpg"))
    (if (not (file-exists-p file))
	(message "%s: No such file" file)
      (save-excursion
	(setq post-buf (current-buffer))
	(set-buffer (setq uubuf (get-buffer-create uuencode-buffer-name)))
	(erase-buffer)
	(if (string-match "^~/" file)
	    (setq file (concat "$HOME" (substring file 1))))
	(if (string-match "/[^/]*$" file)
	    (setq short-file (substring file (1+ (match-beginning 0))))
	  (setq short-file file))
	(call-process "sh" nil uubuf nil "-c" 
		      (format "uuencode %s %s" file short-file))
	(goto-char 1)
	(forward-line 1)
	(while (re-search-forward " " nil t)
	  (replace-match "`"))
	(setq length (count-lines 1 (point-max)))
	(setq parts (/ length gnus-uu-uuencode-post-length))
	(if (not (< (% length gnus-uu-uuencode-post-length) 4))
	    (setq parts (1+ parts)))
	(message "Det er %d parts" parts))
      (goto-char 1)
      (search-forward mail-header-separator nil t)
      (beginning-of-line)
      (forward-line 1)
      (setq header (buffer-substring 1 (point)))
      (goto-char 1)
      (if (re-search-forward "^Subject: " nil t)
	  (progn
	    (end-of-line)
	    (insert (format " (0/%d)" parts))))
;	(save-excursion 
;	  (set-buffer (get-buffer-create "*tull"))
;	  (erase-buffer)
;	  (goto-char (point-max))
;	  (insert-buffer send-buffer-name))
      (gnus-inews-news)

      (save-excursion
	(setq i 1)
	(setq beg 1)
	(while (not (> i parts))
	  (set-buffer (get-buffer-create send-buffer-name))
	  (erase-buffer)
	  (insert header)
	  (insert "\n")
	  (setq whole-len
		(- 62 (length (format top-string "" short-file i parts ""))))
	  (setq minlen (/ whole-len 2))
	  (setq 
	   beg-line 
	   (format top-string
	    (make-string minlen ?-) 
	    short-file i parts
	    (make-string (if (= 0 (% whole-len 2)) (1- minlen) minlen) ?-)))
	  (insert beg-line)
	  (insert "\n")
	  (goto-char 1)
	  (if (re-search-forward "^Subject: " nil t)
	      (progn
		(end-of-line)
		(insert (format " (%d/%d)" i parts))))
	  (goto-char (point-max))
	  (save-excursion
	    (set-buffer uubuf)
	    (goto-char beg)
	    (if (= i parts)
		(goto-char (point-max))
	      (forward-line gnus-uu-uuencode-post-length))
	    (setq end (point)))
	  (insert-buffer-substring uubuf beg end)
	  (insert beg-line)
	  (insert "\n")
	  (setq beg end)
	  (setq i (1+ i))
;	  (save-excursion 
;	    (set-buffer (get-buffer-create "*tull"))
;	    (goto-char (point-max))
;	    (insert-buffer send-buffer-name))
	  (gnus-inews-news)
	  ))
      (and (setq buf (get-buffer send-buffer-name))
	   (kill-buffer buf))
      (and (setq buf (get-buffer uuencode-buffer-name))
	   (kill-buffer buf)))))



;; Decode and all files

(defconst gnus-uu-rest-of-articles nil)
(defconst gnus-uu-do-sloppy-uudecode nil)
(defvar gnus-uu-current-save-dir nil)

(defun gnus-uu-decode-and-save-all-unread-articles ()
  "Try to decode all unread articles and saves the result.
This function reads all unread articles in the current group and sees
whether it can uudecode the articles. The user will be prompted for an
directory to put the resulting (if any) files."
  (interactive)
  (gnus-uu-decode-and-save-articles t t))

(defun gnus-uu-decode-and-save-all-articles ()
  "Try to decode all articles and saves the result.
Does the same as `gnus-uu-decode-and-save-all-unread-articles', except
that it grabs all articles visible, unread or not."
  (interactive)
  (gnus-uu-decode-and-save-articles nil t))

(defun gnus-uu-decode-and-save-all-unread-articles-and-mark ()
  "Try to decode all unread articles and saves the result and marks everything as read.
Does the same as `gnus-uu-decode-and-save-all-unread-articles', except that 
it marks everything as read, even if it couldn't decode the articles."
  (interactive)
  (gnus-uu-decode-and-save-articles t nil))

(defun gnus-uu-decode-and-save-all-articles-and-mark ()
  "Try to decode all articles and saves the result and marks everything as read.
Does the same as `gnus-uu-decode-and-save-all-articles', except that 
it marks everything as read, even if it couldn't decode the articles."
  (interactive)
  (gnus-uu-decode-and-save-articles nil nil))

(defun gnus-uu-decode-and-save-articles (&optional unread unmark)
  (let ((gnus-uu-unmark-articles-not-decoded unmark)
	(filest "")
	where dir did unmark saved-list)
    (setq gnus-uu-do-sloppy-uudecode t)
    (setq dir (gnus-uu-read-directory "Where do you want the files? "))
    (message "Grabbing...")
    (setq gnus-uu-rest-of-articles 
	  (gnus-uu-get-list-of-articles "^." nil unread))
    (setq gnus-uu-file-name nil)
    (while (and gnus-uu-rest-of-articles 
		(gnus-uu-grab-articles gnus-uu-rest-of-articles 
				       'gnus-uu-uustrip-article-as))
      (if gnus-uu-file-name
	  (progn
	    (setq saved-list (cons gnus-uu-file-name saved-list))
	    (rename-file (concat gnus-uu-tmp-dir gnus-uu-file-name) 
			 (concat dir gnus-uu-file-name) t)
	    (setq did t)
	    (setq gnus-uu-file-name nil))))
    (if (not did)
	()
      (while saved-list
	(setq filest (concat filest " " (car saved-list)))
	(setq saved-list (cdr saved-list)))
      (message "Saved%s" filest)))
  (setq gnus-uu-do-sloppy-uudecode nil))


;; Work functions

(defun gnus-uu-decode-and-view-or-save (view marked)
  (gnus-uu-initialize)
  (let (file decoded)
    (save-excursion
      (if (gnus-uu-decode-and-strip nil marked)
	  (progn
	    (setq decoded t)
	    (setq file (concat gnus-uu-tmp-dir gnus-uu-file-name))
	    (if view 
		(gnus-uu-view-file file)
	      (gnus-uu-save-file file)))))

    (gnus-uu-summary-next-subject)

    (if gnus-uu-error-during-unarching 
	(gnus-uu-clean-up)
      (if (and gnus-uu-use-interactive-view view decoded)
	  (gnus-uu-do-interactive)))

    (if (or (not gnus-uu-use-interactive-view) (not decoded))
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
		  (call-process "sh" nil 
				(get-buffer-create gnus-uu-output-buffer-name)
				nil "-c" 
				(format "cd %s ; tar cf %s * ; cd .. ; rm -r %s" 
					gnus-uu-shar-directory 
					tar-file
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


(defconst gnus-uu-saved-article-name nil)
(defun gnus-uu-save-articles (view marked)
  (let (list-of-articles)
    (save-excursion
      (if (not marked)
	  (setq list-of-articles (gnus-uu-get-list-of-articles))
	(setq list-of-articles (reverse gnus-uu-marked-article-list))
	(setq gnus-uu-marked-article-list nil))
      (if (not list-of-articles)
	  (progn
	    (message "No list of articles")
	    (sit-for 2))
	(setq gnus-uu-saved-article-name 
	      (concat gnus-uu-tmp-dir 
		      (read-file-name "Enter file name: " gnus-newsgroup-name
				      gnus-newsgroup-name)))
	(gnus-uu-add-file gnus-uu-saved-article-name)
	(if (gnus-uu-grab-articles list-of-articles 'gnus-uu-save-article)
	    (gnus-uu-save-file gnus-uu-saved-article-name))))))


(defun gnus-uu-save-article (buffer in-state)
  (save-excursion
    (set-buffer buffer)
    (call-process-region 
     1 (point-max) "sh" nil (get-buffer-create gnus-uu-output-buffer-name)
     nil "-c" (concat "cat >> " gnus-uu-saved-article-name)))
  'ok)


;; Binhex
(defconst gnus-uu-binhex-body-line 
  "^................................................................$")
(defconst gnus-uu-binhex-begin-line 
  "^:...............................................................$")
(defconst gnus-uu-binhex-end-line
  ":$")
(defvar gnus-uu-binhex-article-name nil)


(defun gnus-uu-binhex-and-save (view marked)
  (let (list-of-articles)
    (save-excursion
      (if (not marked)
	  (setq list-of-articles (gnus-uu-get-list-of-articles))
	(setq list-of-articles (reverse gnus-uu-marked-article-list))
	(setq gnus-uu-marked-article-list nil))
      (if (not list-of-articles)
	  (progn
	    (message "No list of articles")
	    (sit-for 2))
	(setq gnus-uu-binhex-article-name 
	      (concat gnus-uu-tmp-dir 
		      (read-file-name "Enter binhex file name: " 
				      gnus-newsgroup-name
				      gnus-newsgroup-name)))
	(gnus-uu-add-file gnus-uu-binhex-article-name)
	(if (gnus-uu-grab-articles list-of-articles 'gnus-uu-binhex-article)
	    (gnus-uu-save-file gnus-uu-binhex-article-name))))))


(defun gnus-uu-binhex-article (buffer in-state)
  (let ((state 'ok)
	start-char)
    (save-excursion
      (set-buffer buffer)
      (goto-char 1)
      (if (not (re-search-forward (concat gnus-uu-binhex-begin-line "\\|" 
					  gnus-uu-binhex-body-line) nil t))
	  (setq state 'wrong-type)
	(beginning-of-line)
	(setq start-char (point))
	(if (looking-at gnus-uu-binhex-begin-line)
	    (setq state 'begin)
	  (setq state 'middle))
	(goto-char (point-max))
	(re-search-backward (concat gnus-uu-binhex-body-line "\\|" 
				    gnus-uu-binhex-end-line) nil t)
	(if (looking-at gnus-uu-binhex-end-line)
	    (if (eq state 'begin)
		(setq state 'begin-and-end)
	      (setq state 'end)))
	(beginning-of-line)
	(forward-line 1)
	(append-to-file start-char (point) gnus-uu-binhex-article-name)))
    state))
      

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

(defun gnus-uu-view-file (file-name &optional dont-ask)
  (let (action did-view
	(didnt-want t)
	(do-view t))
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
	    (if (and gnus-uu-use-interactive-view
		     gnus-uu-user-interactive-view-rules-end)
		gnus-uu-user-interactive-view-rules-end
	       gnus-uu-user-view-rules-end))))

	   (if (and gnus-uu-use-interactive-view 
		    (not (string= (or action "") "gnus-uu-archive")))
	       (gnus-uu-enter-interactive-file (or action "") file-name)

	     (if action
		 (if (string= action "gnus-uu-archive") 
		     (setq did-view (gnus-uu-treat-archive file-name))
	  
		   (if gnus-uu-ask-before-view
		       (setq didnt-want 
			     (or (not 
				  (setq do-view
					(y-or-n-p 
					 (format "Do you want to view %s? " 
						 file-name))))
				 didnt-want)))

		   (if do-view
		       (setq did-view 
			     (if gnus-uu-asynchronous
				 (gnus-uu-call-asynchronous file-name action)
			       (gnus-uu-call-synchronous file-name action))))))

	     (if (and (not dont-ask) (not gnus-uu-use-interactive-view))
		 (progn
		   (if (and
			didnt-want
			(or (not action)
			    (and (string= action "gnus-uu-archive") 
				 (not did-view))))
		       (progn
			 (message 
			  (format "Could find no rule for %s" file-name))
			 (sit-for 2)))
		   (and (or (not did-view) gnus-uu-view-and-save)
			(y-or-n-p 
			 (format "Do you want to save the file %s? "
				 file-name))
			(gnus-uu-save-file file-name))))

	     (if (and (file-exists-p file-name) 
		      (not gnus-uu-use-interactive-view)
		      (or 
		       (not (and gnus-uu-asynchronous did-view))
		       (string= action "gnus-uu-archive")))
		 (delete-file file-name)))

	   did-view))


; `gnus-uu-call-synchronous' takes two parameters: The name of the
; file to be displayed and the command to display it with. Returns t
; on success and nil if the file couldn't be displayed.

(defun gnus-uu-call-synchronous (file-name action)
  (let (did-view command)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (erase-buffer)
      (setq command (gnus-uu-command action file-name))
      (message (format "Viewing with '%s'" command))
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
    (setq tmp-file (concat gnus-uu-tmp-dir file))
    (if (string= tmp-file file-name)
	()
      (rename-file file-name tmp-file t)
      (setq file-name tmp-file))

    (setq command (gnus-uu-command action file-name))
    (setq command (format "%s ; rm -f %s" command file-name))
    (message (format "Viewing with %s" command))
    (start-process "gnus-uu-view" nil "sh" "-c" command)
    t))


; `gnus-uu-decode-and-strip' does all the main work. It finds out what
; articles to grab, grabs them, strips the result and decodes. If any
; of these operations fail, it returns nil, t otherwise.  If shar is
; t, it will pass this on to `gnus-uu-grab-articles', which will
; (probably) unshar the articles. If use-marked is non-nil, it won't
; try to find articles, but use the marked list.

(defun gnus-uu-decode-and-strip (&optional shar use-marked)
  (let (list-of-articles)
    (save-excursion

      (if use-marked
	  (progn (if (eq gnus-uu-marked-article-list ())
		(message "No articles marked")
	      (setq list-of-articles (reverse gnus-uu-marked-article-list))
	      (gnus-uu-unmark-all-articles)))
	(setq list-of-articles (gnus-uu-get-list-of-articles)))
      
      (and list-of-articles
	   (gnus-uu-grab-articles list-of-articles 
				  (if shar 
				      'gnus-uu-unshar-article
				    'gnus-uu-uustrip-article-as))))))


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
; Returns the resulting list.

(defun gnus-uu-get-list-of-articles (&optional subject mark-articles only-unread)
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
	      (setq case-fold-search t)
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

	    (setq list-of-subjects (gnus-uu-expand-numbers list-of-subjects))

; Sort the subjects.

	    (setq list-of-subjects (sort list-of-subjects 'gnus-uu-string<))

; Get the article numbers from the sorted list of subjects.

	    (while list-of-subjects 
	      (setq art-num (gnus-uu-article-number (car list-of-subjects)))
	      (if mark-articles (gnus-summary-mark-as-read art-num ?#))
	      (setq list-of-numbers (cons art-num list-of-numbers))
	      (setq list-of-subjects (cdr list-of-subjects)))

	    (setq list-of-numbers (nreverse list-of-numbers))

	    (if (not list-of-numbers)
		(progn 
		  (message (concat "No subjects matched " subject))
		  (sit-for 2)))))

      list-of-numbers)))


; Takes a list of strings and "expands" all numbers in all the
; strings.  That is, this function makes all numbers equal length by
; prepending lots of zeroes before each number. This is to ease later
; sorting to find out what sequence the articles are supposed to be
; decoded in. Returns the list of expanded strings.

(defun gnus-uu-expand-numbers (string-list)
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
	(while (re-search-forward "[A-Za-z]" nil t)
	  (replace-match "a" t t))

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
; The function to be called should take two parameters.  The first is
; the buffer that has the article that should be treated. The function
; should leave the result in this buffer as well. This result is then
; appended on to the `gnus-uu-result-buffer'.
;
; The second parameter is the state of the list of articles, and can
; have three values: 'start, 'middle and 'end.
;
; The function can have several return values: 
; 'error if there was an error while treating.
; 'end if the last article has been sighted.
; 'begin-and-end if the article is both the beginning and
;   the end. All these three return values results in 
;   `gnus-uu-grab-articles' stopping traversing of the list
;   of articles.
; 'middle if the article is a "middle" article.
; 'ok if everything is ok.

(defvar gnus-uu-has-been-grabbed nil)

(defun gnus-uu-unmark-list-of-grabbed (&optional dont-unmark-last-article)
  (let (art)
    (if (or (not gnus-uu-has-been-grabbed) 
	    (not gnus-uu-unmark-articles-not-decoded))
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
; This function returns t if the grabbing and the process-function
; has been successful and nil otherwise."

(defun gnus-uu-grab-articles (list-of-articles process-function)
  (let ((result-buffer (get-buffer-create gnus-uu-result-buffer))
	(state 'first)
	(process-state 'ok)
	(result t)
	(wrong-type t)
	(has-been-begin nil)
	(article nil))

    (save-excursion
      (set-buffer result-buffer)
      (erase-buffer))
    (setq gnus-uu-has-been-grabbed nil)
    (while (and list-of-articles 
		(not (eq process-state 'end))
		(not (eq process-state 'begin-and-end))
		(not (eq process-state 'error)))
      (setq article (car list-of-articles))
      (setq list-of-articles (cdr list-of-articles))
      (setq gnus-uu-has-been-grabbed (cons article gnus-uu-has-been-grabbed))

      (if (eq list-of-articles ()) (setq state 'last))

      (message (format "Getting article %d" article))
      (if (not (= (or gnus-current-article 0) article))
	  (gnus-summary-display-article article))
      (gnus-summary-mark-as-read article)

      (save-excursion 
	(set-buffer gnus-article-buffer)
	(widen))

      (setq process-state (funcall process-function gnus-article-buffer state))

      (if (or (eq process-state 'begin) (eq process-state 'begin-and-end)
	      (eq process-state 'ok))
	  (setq has-been-begin t))

      (if (not (eq process-state 'wrong-type))
	  (setq wrong-type nil)
	(if gnus-uu-unmark-articles-not-decoded
	    (gnus-summary-mark-as-unread article t)))

      (if gnus-uu-do-sloppy-uudecode
	  (setq wrong-type nil))

      (if (and (not has-been-begin)
	       (not gnus-uu-do-sloppy-uudecode)
	       (or (eq process-state 'end)
		   (eq process-state 'middle)))
	  (progn
	    (setq process-state 'error)
	    (message "No begin part at the beginning")
	    (sit-for 2))
	(setq state 'middle)))

    (if (and (not has-been-begin) (not gnus-uu-do-sloppy-uudecode))
	(progn
	  (setq result nil)
	  (message "Wrong type file")
	  (sit-for 2))
      (if (eq process-state 'error)
	  (setq result nil)
	(if (not (or (eq process-state 'ok) 
		     (eq process-state 'end)
		     (eq process-state 'begin-and-end)))
	    (progn
	      (if (not gnus-uu-do-sloppy-uudecode)
		  (progn
		    (message "End of articles reached before end of file")
		    (sit-for 2)))
	      (gnus-uu-unmark-list-of-grabbed)
	      (setq result nil)))))
    (setq gnus-uu-rest-of-articles list-of-articles)
    result))


(defun gnus-uu-uudecode-sentinel (process event)
;  (message "Process '%s' has received event '%s'" process event)
;  (sit-for 2)
  (delete-process (get-process process)))


(defun gnus-uu-uustrip-article-as (process-buffer in-state)
  (let ((state 'ok)
	(process-connection-type nil)
	start-char pst name-beg name-end buf-state)
    (save-excursion
      (set-buffer process-buffer)
      (setq buf-state buffer-read-only)
      (setq buffer-read-only nil)

      (goto-char 1)

      (if gnus-uu-kill-carriage-return
	  (progn
	    (while (search-forward "
" nil t)
	      (delete-backward-char 1))
	    (goto-char 1)))

      (if (not (re-search-forward gnus-uu-begin-string nil t))
	  (if (not (re-search-forward gnus-uu-body-line nil t))
	    (setq state 'wrong-type)))
     
      (if (eq state 'wrong-type)
	  ()
	(beginning-of-line)
	(setq start-char (point))

	(if (looking-at gnus-uu-begin-string)
	    (progn 
	      (setq name-end (match-end 1))
	      (goto-char (setq name-beg (match-beginning 1)))
	      (while (re-search-forward "/" name-end t)
		(replace-match "-"))
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
		     (format "cd %s ; uudecode" gnus-uu-tmp-dir)))
	      (set-process-sentinel 
	       gnus-uu-uudecode-process 'gnus-uu-uudecode-sentinel)
	      (setq state 'begin)
	      (gnus-uu-add-file (concat gnus-uu-tmp-dir gnus-uu-file-name)))
	  (setq state 'middle))
	
	(goto-char (point-max))

	(re-search-backward 
	 (concat gnus-uu-body-line "\\|" gnus-uu-end-string) nil t)
	(beginning-of-line)

	(if (looking-at gnus-uu-end-string)
	    (if (eq state 'begin)
		(setq state 'begin-and-end)
	      (setq state 'end)))
	(forward-line 1)

;	(message "Ja: %s" state)(sit-for 0)(sleep-for 2)

	(and gnus-uu-uudecode-process
	     (setq pst (process-status (or gnus-uu-uudecode-process "nevair")))
	     (if (or (eq pst 'run) (eq pst 'stop))
		 (progn
		   (if gnus-uu-correct-stripped-uucode
		       (progn
			 (gnus-uu-check-correct-stripped-uucode 
			  start-char (point))
			 (goto-char (point-max))
			 (re-search-backward 
			  (concat gnus-uu-body-line "\\|" gnus-uu-end-string) 
			  nil t)
			 (forward-line 1)))
		   (condition-case err
		       (process-send-region gnus-uu-uudecode-process 
					    start-char (point))
		     (error 
		      (progn 
			(message "Her var en uuerror")
			(sleep-for 2)
			(setq state 'wrong-type)
			(delete-process gnus-uu-uudecode-process)))))
	       (setq state 'wrong-type)))
	(if (not gnus-uu-uudecode-process)
	    (setq state 'wrong-type)))

      (setq buffer-read-only buf-state))
    state))


; This function is used by `gnus-uu-grab-articles' to treat
; a shared article.

(defun gnus-uu-unshar-article (process-buffer in-state)
  (let ((state 'ok)
	start-char)
    (save-excursion
     (set-buffer process-buffer)
     (goto-char 1)
     (if (not (re-search-forward gnus-uu-shar-begin-string nil t))
	 (setq state 'wrong-type)
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
	      

; UUdecodes everything in the buffer and returns the name of the
; resulting file.

(defun gnus-uu-decode (directory)
  (let ((command (concat "cd " directory " ; uudecode"))
	file-name)
    (save-excursion
      (message "Uudecoding...")
      (set-buffer (get-buffer-create gnus-uu-result-buffer))
      (setq file-name (concat gnus-uu-tmp-dir gnus-uu-file-name))
      (gnus-uu-add-file file-name)
      (call-process-region 1 (point-max) "sh" nil t nil "-c" command)
      file-name)))


; `gnus-uu-choose-action' chooses what action to perform given the name
; and `gnus-uu-file-action-list'.  Returns either nil if no action is
; found, or the name of the command to run if such a rule is found.

(defun gnus-uu-choose-action (file-name file-action-list)
  (let ((action-list (copy-sequence file-action-list))
	rule action)
    (while (not (or (eq action-list ()) action))
      (setq rule (car action-list))
      (setq action-list (cdr action-list))
      (if (string-match (car rule) file-name)
	  (setq action (car (cdr rule)))))
    action))


; Moves the file from the tmp directory to where the user wants it.

(defun gnus-uu-save-file (from-file-name &optional default-dir ignore-existing)
  (let (dir file-name command)
    (string-match "/[^/]*$" from-file-name)
    (setq file-name (substring from-file-name (1+ (match-beginning 0))))
    (if default-dir
	(setq dir default-dir)
      (setq dir (gnus-uu-read-directory "Where do you want the file? ")))
    (if (and (not ignore-existing) (file-exists-p (concat dir file-name)))
	(progn
	  (message (concat "There already is a file called " file-name))
	  (sit-for 2)
	  (setq file-name
		(read-file-name "Give a new name: " dir (concat dir file-name)
			    nil file-name)))
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

(defun gnus-uu-treat-archive (file-name)
  (let ((arc-dir (make-temp-name 
		  (concat gnus-uu-tmp-dir "gnusuu")))
	action command files file did-view short-file-name)
    (setq action (gnus-uu-choose-action 
		  file-name (append gnus-uu-user-archive-rules
				    (if gnus-uu-ignore-default-archive-rules
					nil
				      gnus-uu-default-archive-rules))))
    (if (not action)
	(progn (message (format "No unpackers for the file %s" file-name))
	       (sit-for 2))
      (string-match "/[^/]*$" file-name)
      (setq short-file-name (substring file-name (1+ (match-beginning 0))))
      (setq command (format "%s %s %s ; cd %s ; %s " 
			    (if (or (string= action "uncompress")
				    (string= action "gunzip"))
				"cp"
			      "mv")
			    (gnus-uu-command "" file-name) arc-dir 
			    arc-dir 
			    (gnus-uu-command action short-file-name)))

      (make-directory arc-dir)
      (gnus-uu-add-file arc-dir)

      (save-excursion
	(set-buffer (get-buffer-create gnus-uu-output-buffer-name))
	(erase-buffer))

      (message (format "Unpacking with %s..." action))
      (sleep-for 1)

      (if (= 0 (call-process "sh" nil 
			     (get-buffer-create gnus-uu-output-buffer-name)
			     nil "-c" command))
	  (message "")
	(message "Error during unpacking of archive")
	(sit-for 0) (sleep-for 2)
	(setq gnus-uu-error-during-unarching t))

      (if (not (or (string= action "uncompress")
		   (string= action "gunzip")))
	  (call-process "sh" nil (get-buffer gnus-uu-output-buffer-name)
			nil "-c" (format "mv %s %s" 
					 (gnus-uu-command "" (concat arc-dir "/" short-file-name))
					 gnus-uu-tmp-dir)))
      (gnus-uu-add-file (concat gnus-uu-tmp-dir short-file-name))
    
      (setq did-view 
	    (or (gnus-uu-show-directory arc-dir gnus-uu-use-interactive-view) 
		did-view))

      (if (and (not gnus-uu-use-interactive-view) 
	       (file-directory-p arc-dir))
	  (delete-directory arc-dir)))

    did-view))


; Tries to view all the files in the given directory. Returns t if
; viewing one or more files is successful.

(defun gnus-uu-show-directory (dir &optional dont-delete-files)
  (let (files file did-view)
    (setq files (directory-files dir t))
    (setq gnus-uu-generated-file-list
	  (append files gnus-uu-generated-file-list))
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (if (and (not (string-match "/\\.$" file)) 
	       (not (string-match "/\\.\\.$" file)))
	  (progn
	    (set-file-modes file 448)
	    (if (file-directory-p file)
		(setq did-view (or (gnus-uu-show-directory file 
							   dont-delete-files) 
				   did-view))
	      (setq did-view (or (gnus-uu-view-file file t) did-view))
	      (if (and (not dont-delete-files) (file-exists-p file)) 
		       (delete-file file))))))
    (if (not dont-delete-files) (delete-directory dir))
    did-view))


;; Manual marking

(defun gnus-uu-enter-mark-in-list ()
  (let (article	beg)
    (beginning-of-line)
    (setq beg (point))
    (end-of-line)
    (setq article (gnus-uu-article-number 
		   (buffer-substring beg (point))))
    (message (format "Adding article %d to list" article))
    (setq gnus-uu-marked-article-list 
	  (cons article gnus-uu-marked-article-list)))) 

(defun gnus-uu-mark-article ()
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
      (message (format "Removing article %d" article))
      (while in 
	(if (not (= (car in) article))
	    (setq out (cons (car in) out))
	  (setq found t)
	  (message (format "Removing article %d" article)))
	(setq in (cdr in)))
      (if (not found) (message "Not a marked article."))
      (setq gnus-uu-marked-article-list (reverse out))
      (gnus-summary-mark-as-unread nil t)
      (gnus-summary-next-subject 1 nil)))
	

(defun gnus-uu-unmark-all-articles ()
  "Removes the mark from all articles marked for decoding."
  (interactive)
  (let ((articles (copy-sequence gnus-uu-marked-article-list)))
    (while articles
      (gnus-summary-goto-subject (car articles))
      (gnus-summary-mark-as-unread nil t)
      (setq articles (cdr articles)))
    (setq gnus-uu-marked-article-list ())))

(defun gnus-uu-mark-by-regexp ()
  "Asks for a regular expression and marks all articles that match."
  (interactive)
  (let (exp)
    (setq exp (read-from-minibuffer "Enter regular expression: "))
    (setq gnus-uu-marked-article-list 
	  (reverse (gnus-uu-get-list-of-articles exp t)))
    (message "")))
      

;; Various

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
  (setq gnus-uu-error-during-unarching nil)
  (if (not gnus-uu-use-interactive-view)
      ()
    (save-excursion
      (setq gnus-uu-interactive-file-list nil)
      (set-buffer (get-buffer-create gnus-uu-interactive-buffer-name))
      (erase-buffer)
      (gnus-uu-mode)
      (insert 
       "# Press return to execute a command.
# Press `C-c C-c' to exit interactive view.

"))))


; Kills the temporary uu buffers, kills any processes, etc.

(defun gnus-uu-clean-up ()
  (let (buf pst)
    (setq gnus-uu-do-sloppy-uudecode nil)
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
  (let (file)
    (while gnus-uu-generated-file-list
      (setq file (car gnus-uu-generated-file-list))
      (setq gnus-uu-generated-file-list (cdr gnus-uu-generated-file-list))
      (if (not (string-match "/\\.[\\.]?$" file))
	  (progn
	    (if (file-directory-p file)
		(delete-directory file)
	      (if (file-exists-p file)
		  (delete-file file))))))))


; Add a file to be checked (and deleted if it still exists upon
; exiting the newsgroup) to a list
(defun gnus-uu-add-file (file)
  (setq gnus-uu-generated-file-list 
	(cons file gnus-uu-generated-file-list)))


; Go to the next unread subject. If there is no further unread
; subjects, go to the last subject in the buffer.
(defun gnus-uu-summary-next-subject ()
  (if (not (gnus-summary-search-forward t))
      (progn
	(goto-char 1)
	(sit-for 0)
	(goto-char (point-max))
	(forward-line -1)
	(beginning-of-line)
	(search-forward ":" nil t)))
  (sit-for 0)
  (gnus-summary-recenter))

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

(defun gnus-uu-do-interactive ()
  (let (int-buffer out-buf)
    (set-buffer 
     (setq int-buffer (get-buffer gnus-uu-interactive-buffer-name)))
    (switch-to-buffer-other-window int-buffer)
    (pop-to-buffer int-buffer)
    (setq gnus-uu-output-window 
	  (split-window nil (- (window-height) gnus-uu-output-window-height)))
    (set-window-buffer gnus-uu-output-window
		       (setq out-buf 
			     (get-buffer-create gnus-uu-output-buffer-name)))
    (save-excursion (set-buffer out-buf) (erase-buffer))
    (goto-char 1)
    (forward-line 3)
    (run-hooks 'gnus-uu-mode-hook)))


(defun gnus-uu-enter-interactive-file (action file)
  (let (command)
    (save-excursion
      (setq gnus-uu-interactive-file-list
	    (cons file gnus-uu-interactive-file-list))
      (set-buffer (get-buffer gnus-uu-interactive-buffer-name))
      (setq command (gnus-uu-command action file))
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
    (message "Executing...")
    (if gnus-uu-asynchronous
	(start-process "gnus-uu-view" out-buf "sh" "-c" command)
      (call-process "sh" nil out-buf nil "-c" command)
      (message ""))
    (forward-line 1)
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


(if gnus-uu-mode-map
    ()
  (setq gnus-uu-mode-map (make-sparse-keymap))
  (define-key gnus-uu-mode-map "\C-c\C-x" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-c\C-v" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-m" 'gnus-uu-interactive-execute)
  (define-key gnus-uu-mode-map "\C-c\C-c" 'gnus-uu-interactive-end)
  (define-key gnus-uu-mode-map "\C-cs" 
    'gnus-uu-interactive-save-current-file)
  (define-key gnus-uu-mode-map "\C-c\C-s"
    'gnus-uu-interactive-save-current-file-silent)
  (define-key gnus-uu-mode-map "\C-c\C-w" 'gnus-uu-interactive-save-all-files)
  (define-key gnus-uu-mode-map "\C-c\C-o" 'gnus-uu-interactive-save-original-file))


(defun gnus-uu-interactive-save-original-file ()
  "Saves the file from whence the file on the current line came from."
  (interactive)
  (let (file)
    (if (file-exists-p 
	 (setq file (concat gnus-uu-tmp-dir
			    (or gnus-uu-file-name gnus-uu-shar-file-name))))
	(gnus-uu-save-file file)
      (message "Already saved."))))


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
\\[gnus-uu-interactive-end]                         End interactive mode
\\[gnus-uu-interactive-save-current-file]                           Save the current file
\\[gnus-uu-interactive-save-current-file-silent]                         Save the current file without asking 
                                where to put it
\\[gnus-uu-interactive-save-all-files]                         Save all files
\\[gnus-uu-interactive-save-original-file]                         Save the original file: If the files
                                originated in an archive, the archive 
                                file is saved.
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

(provide 'gnus-uu)

;; gnus-uu.el ends here
