;; tcl.el --- Tcl code editing commands for Emacs

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Maintainer: Tom Tromey <tromey@busco.lanl.gov>
;; Author: Tom Tromey <tromey@busco.lanl.gov>
;;    Chris Lindblad <cjl@lcs.mit.edu>
;; Keywords: languages tcl modes
;; Version: $Revision: 1.33 $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; HOW TO INSTALL:
;; Put the following forms in your .emacs to enable autoloading of Tcl
;; mode, and auto-recognition of ".tcl" files.
;;
;;   (autoload 'tcl-mode "tcl" "Tcl mode." t)
;;   (autoload 'inferior-tcl "tcl" "Run inferior Tcl process." t)
;;   (setq auto-mode-alist (append '(("\\.tcl$" . tcl-mode)) auto-mode-alist))
;;
;; If you plan to use the interface to the TclX help files, you must
;; set the variable tcl-help-directory-list to point to the topmost
;; directories containing the TclX help files.  Eg:
;;
;;   (setq tcl-help-directory-list '("/usr/local/lib/tclx/help"))
;;
;; Also you will want to add the following to your .emacs:
;;
;;   (autoload 'tcl-help-on-word "tcl" "Help on Tcl commands" t)
;;
;; FYI a *very* useful thing to do is nroff all the Tk man pages and
;; put them in a subdir of the help system.
;;

;;; Commentary:

;; LCD Archive Entry:
;; tcl|Tom Tromey|tromey@busco.lanl.gov|
;; Major mode for editing Tcl|
;; $Date: 1995/06/27 20:01:29 $|$Revision: 1.33 $|~/modes/tcl.el.Z|

;; CUSTOMIZATION NOTES:
;; * tcl-proc-list can be used to customize a list of things that
;; "define" other things.  Eg in my project I put "defvar" in this
;; list.
;; * tcl-typeword-list is similar, but uses font-lock-type-face.
;; * tcl-keyword-list is a list of keywords.  I've generally used this
;; for flow-control words.  Eg I add "unwind_protect" to this list.
;; * tcl-type-alist can be used to minimally customize indentation
;; according to context.

;; Change log:
;; $Log: tcl.el,v $
;; Revision 1.33  1995/06/27  20:01:29  tromey
;; (tcl-set-proc-regexp): Allow leading spaces.
;; (tcl-proc-list): Changes for itcl.
;; (tcl-typeword-list): Ditto.
;; (tcl-keyword-list): Ditto.
;;
;; Revision 1.32  1995/05/11  22:12:49  tromey
;; (tcl-type-alist): Include entry for "proc".
;;
;; Revision 1.31  1995/05/10  23:38:12  tromey
;; (tcl-add-fsf-menu): Use make-lucid-menu-keymap, not
;; "make-xemacs-menu-keymap".
;;
;; Revision 1.30  1995/05/10  18:22:21  tromey
;; Bug fix in menu code for XEmacs.
;;
;; Revision 1.29  1995/05/09  21:36:53  tromey
;; Changed "Lucid Emacs" to "XEmacs".
;; Tcl's popup menu now added to existing one, courtesy
;; dfarmer@evolving.com (Doug Farmer)
;;
;; Revision 1.28  1995/04/08  19:52:50  tromey
;; (tcl-outline-level): New function
;; (tcl-mode): Added outline-handling stuff.
;; From Jesper Pedersen <blackie@imada.ou.dk>
;;
;; Revision 1.27  1994/10/11  02:01:27  tromey
;; (tcl-mode): imenu-create-index-function made buffer local.
;;
;; Revision 1.26  1994/09/01  18:06:24  tromey
;; Added filename completion in inferior tcl mode
;;
;; Revision 1.25  1994/08/22  15:56:24  tromey
;; tcl-load-file default to current buffer.
;;
;; Revision 1.24  1994/08/21  20:33:05  tromey
;; Fixed bug in tcl-guess-application.
;;
;; Revision 1.23  1994/08/21  03:54:45  tromey
;; Keybindings don't overshadown comint bindings.
;;
;; Revision 1.22  1994/07/26  00:46:07  tromey
;; Emacs 18 changes from Carl Witty.
;;
;; Revision 1.21  1994/07/14  22:49:21  tromey
;; Added ";;;###autoload" comments where appropriate.
;;
; Revision 1.20  1994/06/05  16:57:22  tromey
; tcl-current-word does the right thing in inferior-tcl-mode.
;
; Revision 1.19  1994/06/03  21:09:19  tromey
; Another menu fix.
;
; Revision 1.18  1994/06/03  20:39:14  tromey
; Fixed menu bug.
;
; Revision 1.17  1994/06/03  00:47:15  tromey
; Fixed bug in bug-reporting code.
;
; Revision 1.16  1994/05/26  05:06:14  tromey
; Menu items now sensitive as appropriate.
;
; Revision 1.15  1994/05/22  20:38:11  tromey
; Added bug-report keybindings and menu entries.
;
; Revision 1.14  1994/05/22  20:18:28  tromey
; Even more compile stuff.
;
; Revision 1.13  1994/05/22  20:17:15  tromey
; Moved emacs version checking code to very beginning.
;
; Revision 1.12  1994/05/22  20:14:59  tromey
; Compile fixes.
;
; Revision 1.11  1994/05/22  20:12:44  tromey
; Fixed mark-defun for 19.23.
; More menu fixes.
;
; Revision 1.10  1994/05/22  20:02:03  tromey
; Fixed bug with M-;.
; Wrote bug-reporting code.
;
; Revision 1.9  1994/05/22  05:26:51  tromey
; Fixes for imenu.
;
; Revision 1.8  1994/05/22  03:38:07  tromey
; Fixed menu support.
;
; Revision 1.7  1994/05/03  01:23:42  tromey
; *** empty log message ***
;
; Revision 1.6  1994/04/23  16:23:36  tromey
; Wrote tcl-indent-for-comment
;
;;
;; 18-Mar-1994		Tom Tromey	Fourth beta release.
;;    Added {un,}comment-region to menu.  Idea from
;;    Mike Scheidler <c23mts@kocrsv01.delcoelect.com>
;; 17-Mar-1994		Tom Tromey	
;;    Fixed tcl-restart-with-file.  Bug fix attempt in
;;    tcl-internal-end-of-defun.
;; 16-Mar-1994		Tom Tromey	Third beta release
;;    Added support code for menu (from Tcl mode written by
;;    schmid@fb3-s7.math.TU-Berlin.DE (Gregor Schmid)).
;; 12-Mar-1994		Tom Tromey	
;;    Better documentation for inferior-tcl-buffer.  Wrote
;;    tcl-restart-with-file.  Wrote Lucid Emacs menu (but no
;;    code to install it).
;; 12-Mar-1994		Tom Tromey	
;;    Wrote tcl-guess-application.  Another stab at making
;;    tcl-omit-ws-regexp work.
;; 10-Mar-1994		Tom Tromey	Second beta release
;;    Last Modified: Thu Mar 10 01:24:25 1994 (Tom Tromey)
;;    Wrote perl-mode style line indentation command.
;;    Wrote more documentation.  Added tcl-continued-indent-level.
;;    Integrated help code.
;; 8-Mar-1994		Tom Tromey	
;;    Last Modified: Tue Mar  8 11:58:44 1994 (Tom Tromey)
;;    Bug fixes.
;; 6-Mar-1994		Tom Tromey	
;;    Last Modified: Sun Mar  6 18:55:41 1994 (Tom Tromey)
;;    Updated auto-newline support.
;; 6-Mar-1994		Tom Tromey	Beta release
;;    Last Modified: Sat Mar  5 17:24:32 1994 (Tom Tromey)
;;    Wrote tcl-hashify-buffer.  Other minor bug fixes.
;; 5-Mar-1994		Tom Tromey	
;;    Last Modified: Sat Mar  5 16:11:20 1994 (Tom Tromey)
;;    Wrote electric-hash code.
;; 3-Mar-1994		Tom Tromey	
;;    Last Modified: Thu Mar  3 02:53:40 1994 (Tom Tromey)
;;    Added code to handle auto-fill in comments.
;;    Added imenu support code.
;;    Cleaned up code.
;;    Better font-lock support.
;; 28-Feb-1994		Tom Tromey	
;;    Last Modified: Mon Feb 28 14:08:05 1994 (Tom Tromey)
;;    Made tcl-figure-type more easily configurable.
;; 28-Feb-1994		Tom Tromey	
;;    Last Modified: Mon Feb 28 01:02:58 1994 (Tom Tromey)
;;    Wrote inferior-tcl mode.
;; 16-Feb-1994		Tom Tromey	
;;    Last Modified: Wed Feb 16 17:05:19 1994 (Tom Tromey)
;;    Added support for font-lock-mode.
;; 29-Oct-1993		Tom Tromey	
;;    Last Modified: Sun Oct 24 17:39:14 1993 (Tom Tromey)
;;    Patches from Guido Bosch to make things work with Lucid Emacs.
;; 22-Oct-1993		Tom Tromey	
;;    Last Modified: Fri Oct 22 15:26:46 1993 (Tom Tromey)
;;    Made many characters have "_" syntax class; suggested by Guido
;;    Bosch <Guido.Bosch@loria.fr>.  Note that this includes the "$"
;;    character, which might be a change you'd notice.
;; 21-Oct-1993		Tom Tromey	
;;    Last Modified: Thu Oct 21 20:28:40 1993 (Tom Tromey)
;;    More fixes for tcl-omit-ws-regexp.
;; 20-Oct-1993		Tom Tromey	
;;    Started keeping history.  Fixed tcl-{beginning,end}-of-defun.
;;    Added some code to make things work with Emacs 18.

;; THANKS TO:
;; Guido Bosch <Guido.Bosch@loria.fr>
;; pgs1002@esc.cam.ac.uk (Dr P.G. Sjoerdsma)
;; Mike Scheidler <c23mts@kocrsv01.delcoelect.com>
;; Matt Newman <men@charney.colorado.edu>
;; rwhitby@research.canon.oz.au (Rod Whitby)
;; h9118101@hkuxa.hku.hk (Yip Chi Lap [Beta])
;; Pertti Tapio Kasanen <ptk@delta.hut.fi>
;; schmid@fb3-s7.math.TU-Berlin.DE (Gregor Schmid)
;; warsaw@nlm.nih.gov (Barry A. Warsaw)
;; Carl Witty <cwitty@ai.mit.edu>
;; T. V. Raman <raman@crl.dec.com>
;; Jesper Pedersen <blackie@imada.ou.dk>
;; dfarmer@evolving.com (Doug Farmer)

;; KNOWN BUGS:
;; * indent-region should skip blank lines.  (It does in v19, so I'm
;;   not motivated to fix it here).
;; * In Tcl "#" is not always a comment character.  This can confuse
;;   tcl.el in certain circumstances.  For now the only workaround is
;;   to enclose offending hash characters in quotes or precede it with
;;   a backslash.  Note that using braces won't work -- quotes change
;;   the syntax class of characters between them, while braces do not.
;;   The electric-# mode helps alleviate this problem somewhat.
;; * indent-tcl-exp is untested.
;; * Doesn't work under Emacs 18 yet.
;; * There's been a report that font-lock does strange things under
;;   Lucid Emacs 19.6.  For instance in "proc foobar", the space
;;   before "foobar" is highlighted.

;; TODO:
;; * make add-log-tcl-defun smarter.  should notice if we are in the
;;   middle of a defun, or between defuns.  should notice if point is
;;   on first line of defun (or maybe even in comments before defun).
;; * Allow continuation lines to be indented under the first argument
;;   of the preceeding line, like this:
;;      [list something \
;;            something-else]
;; * There is a request that indentation work like this:
;;        button .fred -label Fred \
;;                     -command {puts fred}
;; * Should have tcl-complete-symbol that queries the inferior process.
;; * Should have describe-symbol that works by sending the magic
;;   command to a tclX process.
;; * Need C-x C-e binding (tcl-eval-last-exp).
;; * Write indent-region function that is faster than indenting each
;;   line individually.
;; * tcl-figure-type should stop at "beginning of line" (only ws
;;   before point, and no "\" on previous line).  (see tcl-real-command-p).
;; * overrides some comint keybindings; fix.
;; * Trailing \ will eat blank lines.  Should deal with this.
;;   (this would help catch some potential bugs).
;; * Inferior should display in half the screen, not the whole screen.
;; * Indentation should deal with "switch".
;; * Consider writing code to find help files automatically (for
;;   common cases).
;; * `#' shouldn't insert `\#' when point is in string.



;;; Code:

;; I sure wish Emacs had a package that made it easy to extract this
;; sort of information.
(defconst tcl-using-emacs-19 (string-match "19\\." emacs-version)
  "Nil unless using Emacs 19 (XEmacs or FSF).")

;; FIXME this will break on Emacs 19.100.
(defconst tcl-using-emacs-19-23
  (string-match "19\\.\\(2[3-9]\\|[3-9][0-9]\\)" emacs-version)
  "Nil unless using Emacs 19-23 or later.")

(defconst tcl-using-xemacs-19 (string-match "XEmacs" emacs-version)
  "Nil unless using XEmacs).")

(require 'comint)

;; When compiling under GNU Emacs, load imenu during compilation.  If
;; you have 19.22 or earlier, comment this out, or get imenu.
(and (fboundp 'eval-when-compile)
     (eval-when-compile
       (if (and (string-match "19\\." emacs-version)
		(not (string-match "XEmacs" emacs-version)))
	   (require 'imenu))
       ()))

(defconst tcl-version "$Revision: 1.33 $")
(defconst tcl-maintainer "Tom Tromey <tromey@drip.colorado.edu>")

;;
;; User variables.
;;

(defvar tcl-indent-level 4
  "*Indentation of Tcl statements with respect to containing block.")

(defvar tcl-continued-indent-level 4
  "*Indentation of continuation line relative to first line of command.")

(defvar tcl-auto-newline nil
  "*Non-nil means automatically newline before and after braces
inserted in Tcl code.")

(defvar tcl-tab-always-indent t
  "*Control effect of TAB key.
If t (the default), always indent current line.
If nil and point is not in the indentation area at the beginning of
the line, a TAB is inserted.
Other values cause the first possible action from the following list
to take place:

  1. Move from beginning of line to correct indentation.
  2. Delete an empty comment.
  3. Move forward to start of comment, indenting if necessary.
  4. Move forward to end of line, indenting if necessary.
  5. Create an empty comment.
  6. Move backward to start of comment, indenting if necessary.")

(defvar tcl-use-hairy-comment-detector t
  "*If not `nil', the the more complicated, but slower, comment
detecting function is used.  This variable is only used in GNU Emacs
19 (the fast function is always used elsewhere).")

(defvar tcl-electric-hash-style 'smart
  "*Style of electric hash insertion to use.
Possible values are 'backslash, meaning that `\\' quoting should be
done; `quote, meaning that `\"' quoting should be done; 'smart,
meaning that the choice between 'backslash and 'quote should be
made depending on the number of hashes inserted; or nil, meaning that
no quoting should be done.  Any other value for this variable is
taken to mean 'smart.  The default is 'smart.")

(defvar tcl-help-directory-list nil
  "*List of topmost directories containing TclX help files")

(defvar tcl-use-smart-word-finder t
  "*If not nil, use a better way of finding the current word when
looking up help on a Tcl command.")

(defvar tcl-application "wish"
  "*Name of Tcl application to run in inferior Tcl mode.")

(defvar tcl-command-switches nil
  "*Switches to supply to `tcl-application'.")

(defvar tcl-prompt-regexp "^\\(% \\|\\)"
  "*If not nil, a regexp that will match the prompt in the inferior process.
If nil, the prompt is the name of the application with \">\" appended.

The default is \"^\\(% \\|\\)\", which will match the default primary
and secondary prompts for tclsh and wish.")

(defvar inferior-tcl-source-command "source %s\n"
  "*Format-string for building a Tcl command to load a file.
This format string should use `%s' to substitute a file name
and should result in a Tcl expression that will command the
inferior Tcl to load that file.  The filename will be appropriately
quoted for Tcl.")

;;
;; Keymaps, abbrevs, syntax tables.
;;

(defvar tcl-mode-abbrev-table nil
  "Abbrev table in use in Tcl-mode buffers.")
(if tcl-mode-abbrev-table
    ()
  (define-abbrev-table 'tcl-mode-abbrev-table ()))

(defvar tcl-mode-map ()
  "Keymap used in Tcl mode.")

(defvar tcl-mode-syntax-table nil
  "Syntax table in use in Tcl-mode buffers.")
(if tcl-mode-syntax-table
    ()
  (setq tcl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?%  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?@  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?&  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?*  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?+  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?-  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?.  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?:  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?!  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?$  "_" tcl-mode-syntax-table) ; FIXME use "'"?
  (modify-syntax-entry ?/  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?~  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?<  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?=  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?>  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?|  "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?\(  "()" tcl-mode-syntax-table)
  (modify-syntax-entry ?\)  ")(" tcl-mode-syntax-table)
  (modify-syntax-entry ?\;  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " tcl-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " tcl-mode-syntax-table)
  (modify-syntax-entry ?# "<   " tcl-mode-syntax-table))

(defvar inferior-tcl-mode-map nil
  "Keymap used in Inferior Tcl mode.")

;; XEmacs menu.
(defvar tcl-xemacs-menu
  '("Tcl"
    ["Beginning of function" tcl-beginning-of-defun t]
    ["End of function" tcl-end-of-defun t]
    ["Mark function" tcl-mark-defun t]
    ["Indent region" indent-region (tcl-mark)]
    ["Comment region" comment-region (tcl-mark)]
    ["Uncomment region" tcl-uncomment-region (tcl-mark)]
    "----"
    ["Show Tcl process buffer" inferior-tcl t]
    ["Send function to Tcl process" tcl-eval-defun
     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
    ["Send region to Tcl process" tcl-eval-region
     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
    ["Send file to Tcl process" tcl-load-file
     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
    ["Restart Tcl process with file" tcl-restart-with-file t]
    "----"
    ["Tcl help" tcl-help-on-word tcl-help-directory-list]
    ["Send bug report" tcl-submit-bug-report t])
  "XEmacs menu for Tcl mode.")

;; GNU Emacs does menus via keymaps.  Do it in a function in case we
;; later decide to add it to inferior Tcl mode as well.
(defun tcl-add-fsf-menu (map)
  (define-key map [menu-bar] (make-sparse-keymap))
  ;; This fails in Emacs 19.22 and earlier.
  (require 'lmenu)
  (let ((menu (make-lucid-menu-keymap "Tcl" (cdr tcl-xemacs-menu))))
    (define-key map [menu-bar tcl] (cons "Tcl" menu))
    ;; The following is intended to compute the key sequence
    ;; information for the menu.  It doesn't work.
    (x-popup-menu nil menu)))

(defun tcl-fill-mode-map ()
  (define-key tcl-mode-map "{" 'tcl-electric-char)
  (define-key tcl-mode-map "}" 'tcl-electric-brace)
  (define-key tcl-mode-map "[" 'tcl-electric-char)
  (define-key tcl-mode-map "]" 'tcl-electric-char)
  (define-key tcl-mode-map ";" 'tcl-electric-char)
  (define-key tcl-mode-map "#" 'tcl-electric-hash)
  ;; FIXME.
  (define-key tcl-mode-map "\e\C-a" 'tcl-beginning-of-defun)
  ;; FIXME.
  (define-key tcl-mode-map "\e\C-e" 'tcl-end-of-defun)
  ;; FIXME.
  (define-key tcl-mode-map "\e\C-h" 'tcl-mark-defun)
  (define-key tcl-mode-map "\e\C-q" 'indent-tcl-exp)
  (define-key tcl-mode-map "\177" 'backward-delete-char-untabify)
  (define-key tcl-mode-map "\t" 'tcl-indent-command)
  (define-key tcl-mode-map "\M-;" 'tcl-indent-for-comment)
  (define-key tcl-mode-map "\M-\C-x" 'tcl-eval-defun)
  (define-key tcl-mode-map "\C-c\C-b" 'tcl-submit-bug-report)
  (and (fboundp 'comment-region)
       (define-key tcl-mode-map "\C-c\C-c" 'comment-region))
  (define-key tcl-mode-map "\C-c\C-i" 'tcl-help-on-word)
  (define-key tcl-mode-map "\C-c\C-v" 'tcl-eval-defun)
  (define-key tcl-mode-map "\C-c\C-f" 'tcl-load-file)
  (define-key tcl-mode-map "\C-c\C-t" 'inferior-tcl)
  (define-key tcl-mode-map "\C-c\C-x" 'tcl-eval-region)
  (define-key tcl-mode-map "\C-c\C-s" 'switch-to-tcl)

  ;; Make menus.
  (if (and tcl-using-emacs-19 (not tcl-using-xemacs-19))
      (progn
	(tcl-add-fsf-menu tcl-mode-map))))

(defun tcl-fill-inferior-map ()
  (define-key inferior-tcl-mode-map "\t" 'comint-dynamic-complete)
  (define-key inferior-tcl-mode-map "\M-?"
    'comint-dynamic-list-filename-completions)
  (define-key inferior-tcl-mode-map "\e\C-a" 'tcl-beginning-of-defun)
  (define-key inferior-tcl-mode-map "\e\C-e" 'tcl-end-of-defun)
  (define-key inferior-tcl-mode-map "\177" 'backward-delete-char-untabify)
  (define-key inferior-tcl-mode-map "\M-\C-x" 'tcl-eval-defun)
  (define-key inferior-tcl-mode-map "\C-c\C-b" 'tcl-submit-bug-report)
  (define-key inferior-tcl-mode-map "\C-c\C-i" 'tcl-help-on-word)
  (define-key inferior-tcl-mode-map "\C-c\C-v" 'tcl-eval-defun)
  (define-key inferior-tcl-mode-map "\C-c\C-f" 'tcl-load-file)
  (define-key inferior-tcl-mode-map "\C-c\C-t" 'inferior-tcl)
  (define-key inferior-tcl-mode-map "\C-c\C-x" 'tcl-eval-region)
  (define-key inferior-tcl-mode-map "\C-c\C-s" 'switch-to-tcl))

(if tcl-mode-map
    ()
  (setq tcl-mode-map (make-sparse-keymap))
  (tcl-fill-mode-map))

(if inferior-tcl-mode-map
    ()
  ;; FIXME Use keymap inheritance here?  FIXME we override comint
  ;; keybindings here.  Maybe someone has a better set?
  (setq inferior-tcl-mode-map (copy-keymap comint-mode-map))
  (tcl-fill-inferior-map))


(defvar inferior-tcl-buffer nil
  "*The current inferior-tcl process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Tcl processes, you start the first up with
\\[inferior-tcl].  It will be in a buffer named `*inferior-tcl*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-tcl].  It will be in a new buffer,
named `*inferior-tcl*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Tcl processes -- like
`tcl-eval-defun' or `tcl-load-file' -- have to choose a process to
send to, when you have more than one Tcl process around.  This is
determined by the global variable `inferior-tcl-buffer'.  Suppose you
have three inferior Lisps running:
    Buffer              Process
    foo                 inferior-tcl
    bar                 inferior-tcl<2>
    *inferior-tcl*      inferior-tcl<3>
If you do a \\[tcl-eval-defun] command on some Lisp source code, what
process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-tcl*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-tcl-buffer'.
This process selection is performed by function `inferior-tcl-proc'.

Whenever \\[inferior-tcl] fires up a new process, it resets
`inferior-tcl-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you can change `inferior-tcl-buffer' to another process
buffer with \\[set-variable].")

;;
;; Hooks and other customization.
;;

(defvar tcl-mode-hook nil
  "Hook run on entry to Tcl mode.

Several functions exist which are useful to run from your
`tcl-mode-hook' (see each function's documentation for more
information):

  tcl-guess-application
    Guesses a default setting for `tcl-application' based on any
    \"#!\" line at the top of the file.
  tcl-hashify-buffer
    Quotes all \"#\" characters that don't correspond to actual
    Tcl comments.  (Useful when editing code not originally created
    with this mode).
  tcl-auto-fill-mode
    Auto-filling of Tcl comments.

Emacs 19 users can add functions to the hook with `add-hook':

   (add-hook 'tcl-mode-hook 'tcl-guess-application)

Emacs 18 users must use `setq':

   (setq tcl-mode-hook (cons 'tcl-guess-application tcl-mode-hook))")


(defvar inferior-tcl-mode-hook nil
  "Hook for customizing Inferior Tcl mode.")

(defvar tcl-proc-list
  '("proc" "method" "itcl_class")
  "List of commands whose first argument defines something.
This exists because some people (eg, me) use \"defvar\" et al.
Call `tcl-set-proc-regexp' and `tcl-set-font-lock-keywords'
after changing this list.")

(defvar tcl-proc-regexp nil
  "Regexp to use when matching proc headers.")

(defvar tcl-typeword-list
  '("global" "upvar" "inherit" "public" "protected" "common")
  "List of Tcl keywords denoting \"type\".  Used only for highlighting.
Call `tcl-set-font-lock-keywords' after changing this list.")

;; Generally I've picked control operators to be keywords.
(defvar tcl-keyword-list
  '("if" "then" "else" "elseif" "for" "foreach" "break" "continue" "while"
    "eval" "case" "in" "switch" "default" "exit" "error" "proc" "return"
    "uplevel" "constructor" "destructor" "itcl_class" "loop" "for_array_keys"
    "for_recursive_glob" "for_file")
  "List of Tcl keywords.  Used only for highlighting.
Default list includes some TclX keywords.
Call `tcl-set-font-lock-keywords' after changing this list.")

(defvar tcl-font-lock-keywords nil
  "Keywords to highlight for Tcl.  See variable `font-lock-keywords'.
This variable is generally set from `tcl-proc-regexp',
`tcl-typeword-list', and `tcl-keyword-list' by the function
`tcl-set-font-lock-keywords'.")

;; FIXME need some way to recognize variables because array refs look
;; like 2 sexps.
(defvar tcl-type-alist
  '(
    ("proc" nil tcl-expr tcl-commands)
    ("method" nil tcl-expr tcl-commands)
    ("expr" tcl-expr)
    ("catch" tcl-commands)
    ("if" tcl-expr "then" tcl-commands)
    ("elseif" tcl-expr "then" tcl-commands)
    ("elseif" tcl-expr tcl-commands)
    ("if" tcl-expr tcl-commands)
    ("while" tcl-expr tcl-commands)
    ("for" tcl-commands tcl-expr tcl-commands tcl-commands)
    ("foreach" nil nil tcl-commands)
    ("for_file" nil nil tcl-commands)
    ("for_array_keys" nil nil tcl-commands)
    ("for_recursive_glob" nil nil nil tcl-commands)
    ;; Loop handling is not perfect, because the third argument can be
    ;; either a command or an expr, and there is no real way to look
    ;; forward.
    ("loop" nil tcl-expr tcl-expr tcl-commands)
    ("loop" nil tcl-expr tcl-commands)
    )
  "Alist that controls indentation.
\(Actually, this really only controls what happens on continuation lines).
Each entry looks like `(KEYWORD TYPE ...)'.
Each type entry describes a sexp after the keyword, and can be one of:
* nil, meaning that this sexp has no particular type.
* tcl-expr, meaning that this sexp is an arithmetic expression.
* tcl-commands, meaning that this sexp holds Tcl commands.
* a string, which must exactly match the string at the corresponding
  position for a match to be made.

For example, the entry for the \"loop\" command is:

   (\"loop\" nil tcl-expr tcl-commands)

This means that the \"loop\" command has three arguments.  The first
argument is ignored (for indentation purposes).  The second argument
is a Tcl expression, and the last argument is Tcl commands.")

(defvar tcl-explain-indentation nil
  "If not `nil', debugging message will be printed during indentation.")



;;
;; Work around differences between various versions of Emacs.
;;

;; We use this because Lemacs 19.9 has what we need.
(defconst tcl-pps-has-arg-6
  (or tcl-using-emacs-19
      (and tcl-using-xemacs-19
	   (condition-case nil
	       (progn
		 (parse-partial-sexp (point) (point) nil nil nil t)
		 t)
	     (error nil))))
  "t if using an emacs which supports sixth (\"commentstop\") argument
to parse-partial-sexp.")

;; Its pretty bogus to have to do this, but there is no easier way to
;; say "match not syntax-1 and not syntax-2".  Too bad you can't put
;; \s in [...].  This sickness is used in Emacs 19 to match a defun
;; starter.  (It is used for this in v18 as well).
;;(defconst tcl-omit-ws-regexp
;;  (concat "^\\(\\s"
;;	  (mapconcat 'char-to-string "w_.()\"\\$'/" "\\|\\s")
;;	  "\\)\\S(*")
;;  "Regular expression that matches everything except space, comment
;;starter, and comment ender syntax codes.")

;; FIXME?  Instead of using the hairy regexp above, we just use a
;; simple one.
;;(defconst tcl-omit-ws-regexp "^[^] \t\n#}]\\S(*"
;;  "Regular expression used in locating function definitions.")

;; Here's another stab.  I think this one actually works.  Now the
;; problem seems to be that there is a bug in Emacs 19.22 where
;; end-of-defun doesn't really use the brace matching the one that
;; trails defun-prompt-regexp.
(defconst tcl-omit-ws-regexp "^[^ \t\n#}][^\n}]+}*[ \t]+")

(defun tcl-internal-beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (if (or (null arg) (= arg 0))
      (setq arg 1))
  (let (success)
    (while (progn
	     (setq arg (1- arg))
	     (and (>= arg 0)
		  (setq success
			(re-search-backward tcl-omit-ws-regexp nil 'move 1))))
      (while (and (looking-at "[]#}]")
		  (setq success
			(re-search-backward tcl-omit-ws-regexp nil 'move 1)))))
    (beginning-of-line)
    (not (null success))))

(defun tcl-internal-end-of-defun (&optional arg)
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let ((start (point)))
    ;; Was forward-char.  I think this works a little better.
    (forward-line)
    (tcl-beginning-of-defun)
    (while (> arg 0)
      (while (and (re-search-forward tcl-omit-ws-regexp nil 'move 1)
		  (progn (beginning-of-line) t)
		  (looking-at "[]#}]")
		  (progn (forward-line) t)))
      (let ((next-line (save-excursion 
			 (forward-line)
			 (point))))
	(while (< (point) next-line)
	  (forward-sexp)))
      (forward-line)
      (if (> (point) start) (setq arg (1- arg))))))

;; In Emacs 19, we can use begining-of-defun as long as we set up a
;; certain regexp.  In Emacs 18, we need our own function.
(fset 'tcl-beginning-of-defun
      (if tcl-using-emacs-19
	  'beginning-of-defun
	'tcl-internal-beginning-of-defun))

;; Ditto end-of-defun.
(fset 'tcl-end-of-defun
      (if tcl-using-emacs-19
	  'end-of-defun
	'tcl-internal-end-of-defun))

;; Internal mark-defun that is used for losing Emacsen.
(defun tcl-internal-mark-defun ()
  "Put mark at end of Tcl function, point at beginning."
  (interactive)
  (push-mark (point))
  (tcl-end-of-defun)
  (if tcl-using-emacs-19
      (push-mark (point) nil t)
    (push-mark (point)))
  (tcl-beginning-of-defun)
  (backward-paragraph))

;; In GNU Emacs 19-23 and later, mark-defun works as advertised.  I
;; don't know about XEmacs, so for now it and Emacs 18 just lose.
(fset 'tcl-mark-defun
      (if tcl-using-emacs-19-23
	  'mark-defun
	'tcl-internal-mark-defun))

;; In GNU Emacs 19, mark takes an additional "force" argument.  I
;; don't know about XEmacs, so I'm just assuming it is the same.
;; Emacs 18 doesn't have this argument.
(defun tcl-mark ()
  "Return mark, or nil if none."
  (if tcl-using-emacs-19
      (mark t)
    (mark)))



;;
;; Some helper functions.
;;

(defun tcl-set-proc-regexp ()
  "Set `tcl-proc-regexp' from variable `tcl-proc-list'."
  (setq tcl-proc-regexp (concat "^\\s-*\\("
				(mapconcat 'identity tcl-proc-list "\\|")
				"\\)[ \t]+")))

(defun tcl-set-font-lock-keywords ()
  "Set `tcl-font-lock-keywords'.
Uses variables `tcl-proc-regexp' and `tcl-keyword-list'."
  (setq tcl-font-lock-keywords
	(list
	 ;; Names of functions (and other "defining things").
	 (list (concat tcl-proc-regexp "\\([^ \t\n]+\\)")
	       2 'font-lock-function-name-face)

	 ;; Names of type-defining things.
	 (list (concat "\\(\\s-\\|^\\)\\("
		       ;; FIXME Use 'regexp-quote?
		       (mapconcat 'identity tcl-typeword-list "\\|")
		       "\\)\\(\\s-\\|$\\)")
	       2 'font-lock-type-face)

	 ;; Keywords.  Only recognized if surrounded by whitespace.
	 ;; FIXME consider using "not word or symbol", not
	 ;; "whitespace".
	 (cons (concat "\\(\\s-\\|^\\)\\("
		       ;; FIXME Use regexp-quote? 
		       (mapconcat 'identity tcl-keyword-list "\\|")
		       "\\)\\(\\s-\\|$\\)")
	       2)
	 )))

(if tcl-proc-regexp
    ()
  (tcl-set-proc-regexp))

(if tcl-font-lock-keywords
    ()
  (tcl-set-font-lock-keywords))



;;
;; The mode itself.
;;

;;;###autoload
(defun tcl-mode ()
  "Major mode for editing Tcl code.
Expression and list commands understand all Tcl brackets.
Tab indents for Tcl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
  tcl-indent-level
    Indentation of Tcl statements within surrounding block.
  tcl-continued-indent-level
    Indentation of continuation line relative to first line of command.

Variables controlling user interaction with mode (see variable
documentation for details):
  tcl-tab-always-indent
    Controls action of TAB key.
  tcl-auto-newline
    Non-nil means automatically newline before and after braces, brackets,
    and semicolons inserted in Tcl code.
  tcl-electric-hash-style
    Controls action of `#' key.
  tcl-use-hairy-comment-detector
    If t, use more complicated, but slower, comment detector.
    This variable is only used in GNU Emacs 19.

Turning on Tcl mode calls the value of the variable `tcl-mode-hook'
with no args, if that value is non-nil.  Read the documentation for
`tcl-mode-hook' to see what kinds of interesting hook functions
already exist.

Commands:
\\{tcl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tcl-mode-map)
  (setq major-mode 'tcl-mode)
  (setq mode-name "Tcl")
  (setq local-abbrev-table tcl-mode-abbrev-table)
  (set-syntax-table tcl-mode-syntax-table)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tcl-indent-line)
  ;; Tcl doesn't require a final newline.
  ;; (make-local-variable 'require-final-newline)
  ;; (setq require-final-newline t)

  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'outline-regexp)
  (setq outline-regexp "[^\n\^M]")
  (make-local-variable 'outline-level)
  (setq outline-level 'tcl-outline-level)

  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords tcl-font-lock-keywords)

  ;; The following only really makes sense under GNU Emacs 19.
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'tcl-imenu-create-index-function)
  (make-local-variable 'parse-sexp-ignore-comments)

  (if tcl-using-emacs-19
      (progn
	;; This can only be set to t in Emacs 19 and XEmacs.
	;; Emacs 18 and Epoch lose.
	(setq parse-sexp-ignore-comments t)
	;; XEmacs has defun-prompt-regexp, but I don't believe
	;; that it works for end-of-defun -- only for
	;; beginning-of-defun.
	(make-local-variable 'defun-prompt-regexp)
	(setq defun-prompt-regexp tcl-omit-ws-regexp)
	;; The following doesn't work in Lucid Emacs 19.6, but maybe
	;; it will appear in later versions.
	(make-local-variable 'add-log-current-defun-function)
	(setq add-log-current-defun-function 'add-log-tcl-defun))
    (setq parse-sexp-ignore-comments nil))

  ;; Put Tcl menu into menubar for XEmacs.  This happens
  ;; automatically for GNU Emacs.
  (if (and tcl-using-xemacs-19
	   current-menubar
	   (not (assoc "Tcl" current-menubar)))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "Tcl" tcl-xemacs-menu)))
  ;; Append Tcl menu to popup menu for XEmacs.
  (if (and tcl-using-xemacs-19 (boundp 'mode-popup-menu))
      (setq mode-popup-menu tcl-xemacs-menu))

  (run-hooks 'tcl-mode-hook))



;; This is used for braces, brackets, and semi (except for closing
;; braces, which are handled specially).
(defun tcl-electric-char (arg)
  "Insert character and correct line's indentation."
  (interactive "p")
  ;; Indent line first; this looks better if parens blink.
  (tcl-indent-line)
  (self-insert-command arg)
  (if (and tcl-auto-newline (= last-command-char ?\;))
      (progn
	(newline)
	(tcl-indent-line))))

;; This is used for closing braces.  If tcl-auto-newline is set, can
;; insert a newline both before and after the brace, depending on
;; context.  FIXME should this be configurable?  Does anyone use this?
(defun tcl-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "p")
  ;; If auto-newlining and there is stuff on the same line, insert a
  ;; newline first.
  (if tcl-auto-newline
      (progn
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (bolp))
	    ()
	  (tcl-indent-line)
	  (newline))
	;; In auto-newline case, must insert a newline after each
	;; brace.  So an explicit loop is needed.
	(while (> arg 0)
	  (insert last-command-char)
	  (tcl-indent-line)
	  (newline)
	  (setq arg (1- arg))))
    (self-insert-command arg))
  (tcl-indent-line))



(defun tcl-indent-command (&optional arg)
  "Indent current line as Tcl code, or in some cases insert a tab character.
If tcl-tab-always-indent is t (the default), always indent current line.
If tcl-tab-always-indent is nil and point is not in the indentation
area at the beginning of the line, a TAB is inserted.
Other values of tcl-tab-always-indent cause the first possible action
from the following list to take place:

  1. Move from beginning of line to correct indentation.
  2. Delete an empty comment.
  3. Move forward to start of comment, indenting if necessary.
  4. Move forward to end of line, indenting if necessary.
  5. Create an empty comment.
  6. Move backward to start of comment, indenting if necessary."
  (interactive "p")
  (cond
   ((not tcl-tab-always-indent)
    ;; Indent if in indentation area, otherwise insert TAB.
    (if (<= (current-column) (current-indentation))
	(tcl-indent-line)
      (self-insert-command arg)))
   ((eq tcl-tab-always-indent t)
    ;; Always indent.
    (tcl-indent-line))
   (t
    ;; "Perl-mode" style TAB command.
    (let* ((ipoint (point))
	   (eolpoint (progn
		       (end-of-line)
		       (point)))
	   (comment-p (tcl-in-comment)))
      (cond
       ((= ipoint (save-excursion
		    (beginning-of-line)
		    (point)))
	(beginning-of-line)
	(tcl-indent-line)
	;; If indenting didn't leave us in column 0, go to the
	;; indentation.  Otherwise leave point at end of line.  This
	;; is a hack.
	(if (= (point) (save-excursion
			 (beginning-of-line)
			 (point)))
	    (end-of-line)
	  (back-to-indentation)))
       ((and comment-p (looking-at "[ \t]*$"))
	;; Empty comment, so delete it.  We also delete any ";"
	;; characters at the end of the line.  I think this is
	;; friendlier, but I don't know how other people will feel.
	(backward-char)
	(skip-chars-backward " \t;")
	(delete-region (point) eolpoint))
       ((and comment-p (< ipoint (point)))
	;; Before comment, so skip to it.
	(tcl-indent-line)
	(indent-for-comment))
       ((/= ipoint eolpoint)
	;; Go to end of line (since we're not there yet).
	(goto-char eolpoint)
	(tcl-indent-line))
       ((not comment-p)
	(tcl-indent-line)
	(tcl-indent-for-comment))
       (t
	;; Go to start of comment.  We don't leave point where it is
	;; because we want to skip comment-start-skip.
	(tcl-indent-line)
	(indent-for-comment)))))))

(defun tcl-indent-line ()
  "Indent current line as Tcl code.
Return the amount the indentation changed by."
  (let ((indent (calculate-tcl-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((= (following-char) ?})
		  (setq indent (- indent tcl-indent-level)))
		 ((= (following-char) ?\])
		  (setq indent (- indent 1))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun tcl-figure-type ()
  "Determine type of sexp at point.
This is either 'tcl-expr, 'tcl-commands, or nil.  Puts point at start
of sexp that indicates types.

See documentation for variable `tcl-type-alist' for more information."
  (let ((count 0)
	result
	word-stack)
    (while (and (< count 5)
		(not result))
      (condition-case nil
	  (progn
	    ;; FIXME should use "tcl-backward-sexp", which would skip
	    ;; over entire variables, etc.
	    (backward-sexp)
	    (if (looking-at "[a-zA-Z_]+")
		(let ((list tcl-type-alist)
		      entry)
		  (setq word-stack (cons (current-word) word-stack))
		  (while (and list (not result))
		    (setq entry (car list))
		    (setq list (cdr list))
		    (let ((index 0))
		      (while (and entry (<= index count))
			;; Abort loop if string does not match word on
			;; stack.
			(and (stringp (car entry))
			     (not (string= (car entry)
					   (nth index word-stack)))
			     (setq entry nil))
			(setq entry (cdr entry))
			(setq index (1+ index)))
		      (and (> index count)
			   (not (stringp (car entry)))
			   (setq result (car entry)))
		      )))
	      (setq word-stack (cons nil word-stack))))
	(error nil))
      (setq count (1+ count)))
    (and tcl-explain-indentation
	 (message "Indentation type %s" result))
    result))

(defun calculate-tcl-indent (&optional parse-start)
  "Return appropriate indentation for current line as Tcl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let* ((indent-point (point))
	   (case-fold-search nil)
	   (continued-line 
	    (save-excursion
	      (if (bobp)
		  nil
		(backward-char)
		(= ?\\ (preceding-char)))))
	   (continued-indent-value (if continued-line
				       tcl-continued-indent-level
				     0))
	   state
	   containing-sexp
	   found-next-line)
      (if parse-start
	  (goto-char parse-start)
	(tcl-beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; Inside comment or string.  Return nil or t if should
	     ;; not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.
	     continued-indent-value)
	    (t
	     ;; Set expr-p if we are looking at the expression part of
	     ;; an "if", "expr", etc statement.  Set commands-p if we
	     ;; are looking at the body part of an if, while, etc
	     ;; statement.  FIXME Should check for "for" loops here.
	     (goto-char containing-sexp)
	     (let* ((sexpr-type (tcl-figure-type))
		    (expr-p (eq sexpr-type 'tcl-expr))
		    (commands-p (eq sexpr-type 'tcl-commands))
		    (expr-start (point)))
	       ;; Find the first statement in the block and indent
	       ;; like it.  The first statement in the block might be
	       ;; on the same line, so what we do is skip all
	       ;; "virtually blank" lines, looking for a non-blank
	       ;; one.  A line is virtually blank if it only contains
	       ;; a comment and whitespace.  FIXME continued comments
	       ;; aren't supported.  They are a wart on Tcl anyway.
	       ;; We do it this funky way because we want to know if
	       ;; we've found a statement on some line _after_ the
	       ;; line holding the sexp opener.
	       (goto-char containing-sexp)
	       (forward-char)
	       (if (and (< (point) indent-point)
			(looking-at "[ \t]*\\(#.*\\)?$"))
		   (progn
		     (forward-line)
		     (while (and (< (point) indent-point)
				 (looking-at "[ \t]*\\(#.*\\)?$"))
		       (setq found-next-line t)
		       (forward-line))))
	       (if (or continued-line
		       (/= (char-after containing-sexp) ?{)
		       expr-p)
		   (progn
		     ;; Line is continuation line, or the sexp opener
		     ;; is not a curly brace, or we are are looking at
		     ;; an `expr' expression (which must be split
		     ;; specially).  So indentation is column of first
		     ;; good spot after sexp opener (with some added
		     ;; in the continued-line case).  If there is no
		     ;; nonempty line before the indentation point, we
		     ;; use the column of the character after the sexp
		     ;; opener.
		     (if (>= (point) indent-point)
			 (progn
			   (goto-char containing-sexp)
			   (forward-char))
		       (skip-chars-forward " \t"))
		     (+ (current-column) continued-indent-value))
		 ;; After a curly brace, and not a continuation line.
		 ;; So take indentation from first good line after
		 ;; start of block, unless that line is on the same
		 ;; line as the opening brace.  In this case use the
		 ;; indentation of the opening brace's line, plus
		 ;; another indent step.  If we are in the body part
		 ;; of an "if" or "while" then the indentation is
		 ;; taken from the line holding the start of the
		 ;; statement.
		 (if (and (< (point) indent-point)
			  found-next-line)
		     (current-indentation)
		   (if commands-p
		       (goto-char expr-start)
		     (goto-char containing-sexp))
		   (+ (current-indentation) tcl-indent-level)))))))))



(defun indent-tcl-exp ()
  "Indent each line of the Tcl grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	outer-loop-done inner-loop-done state ostate
	this-indent last-sexp continued-line
	(next-depth 0)
	last-depth)
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (tcl-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack 
		      (or (car (cdr state))
			  (save-excursion
			    (forward-sexp -1)
			    (point)))))
	  (forward-line 1)
	  (setq continued-line 
		(save-excursion
		  (backward-char)
		  (= (preceding-char) ?\\)))
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		(setq this-indent (car indent-stack))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-tcl-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))
		(setq continued-line nil)))
	    (cond ((not (numberp this-indent)))
		  ((= (following-char) ?})
		   (setq this-indent (- this-indent tcl-indent-level)))
		  ((= (following-char) ?\])
		   (setq this-indent (- this-indent 1))))
	    ;; Put chosen indentation into effect.
	    (or (null this-indent)
		(= (current-column) 
		   (if continued-line 
		       (+ this-indent tcl-indent-level)
		     this-indent))
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to 
		   (if continued-line 
		       (+ this-indent tcl-indent-level)
		     this-indent)))))))))
  )



;;
;; Interfaces to other packages.
;;

(defun tcl-imenu-create-index-function ()
  "Generate alist of indices for imenu."
  (let ((re (concat tcl-proc-regexp "\\([^ \t\n{]+\\)"))
	alist)
    (imenu-progress-message 0)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (imenu-progress-message nil)
      ;; Position on start of proc name, not beginning of line.
      (setq alist (cons
		   (cons (buffer-substring (match-beginning 2) (match-end 2))
			 (match-beginning 2))
		   alist)))
    (imenu-progress-message 100)
    (nreverse alist)))

;; FIXME Definition of function is very ad-hoc.  Should use
;; tcl-beginning-of-defun.  Also has incestuous knowledge about the
;; format of tcl-proc-regexp.
(defun add-log-tcl-defun ()
  "Return name of Tcl function point is in, or nil."
  (save-excursion
    (if (re-search-backward
	 (concat tcl-proc-regexp "\\([^ \t\n{]+\\)") nil t)
	(buffer-substring (match-beginning 2)
			  (match-end 2)))))

(defun tcl-outline-level ()
  (save-excursion
    (skip-chars-forward " \t")
    (current-column)))



;;
;; Helper functions for inferior Tcl mode.
;;

;; This exists to let us delete the prompt when commands are sent
;; directly to the inferior Tcl.  See gud.el for an explanation of how
;; it all works (I took it from there).  This stuff doesn't really
;; work as well as I'd like it to.  But I don't believe there is
;; anything useful that can be done.
(defvar inferior-tcl-delete-prompt-marker nil)

(defun tcl-filter (proc string)
  (let ((inhibit-quit t))
    (save-excursion
      (set-buffer (process-buffer proc))
      (goto-char (process-mark proc))
      ;; Delete prompt if requested.
      (if (marker-buffer inferior-tcl-delete-prompt-marker)
	  (progn
	    (delete-region (point) inferior-tcl-delete-prompt-marker)
	    (set-marker inferior-tcl-delete-prompt-marker nil)))))
  (if tcl-using-emacs-19
      (comint-output-filter proc string)
    (funcall comint-output-filter string)))

(defun tcl-send-string (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (beginning-of-line)
    (if (looking-at comint-prompt-regexp)
	(set-marker inferior-tcl-delete-prompt-marker (point))))
  (comint-send-string proc string))

(defun tcl-send-region (proc start end)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (beginning-of-line)
    (if (looking-at comint-prompt-regexp)
	(set-marker inferior-tcl-delete-prompt-marker (point))))
  (comint-send-region proc start end))

(defun switch-to-tcl (eob-p)
  "Switch to inferior Tcl process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer inferior-tcl-buffer)
      (pop-to-buffer inferior-tcl-buffer)
    (error "No current inferior Tcl buffer"))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun inferior-tcl-proc ()
  "Return current inferior Tcl process.
See variable `inferior-tcl-buffer'."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-tcl-mode)
				      (current-buffer)
				    inferior-tcl-buffer))))
    (or proc
	(error "No Tcl process; see variable `inferior-tcl-buffer'"))))

(defun tcl-eval-region (start end &optional and-go)
  "Send the current region to the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive "r\nP")
  (let ((proc (inferior-tcl-proc)))
    (tcl-send-region proc start end)
    (tcl-send-string proc "\n")
    (if and-go (switch-to-tcl t))))

(defun tcl-eval-defun (&optional and-go)
  "Send the current defun to the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive "P")
  (save-excursion
    (tcl-end-of-defun)
    (let ((end (point)))
      (tcl-beginning-of-defun)
      (tcl-eval-region (point) end)))
  (if and-go (switch-to-tcl t)))



;;
;; Inferior Tcl mode itself.
;;

(defun inferior-tcl-mode ()
  "Major mode for interacting with Tcl interpreter.

A Tcl process can be started with M-x inferior-tcl.

Entry to this mode runs the hooks comint-mode-hook and
inferior-tcl-mode-hook, in that order.

You can send text to the inferior Tcl process from other buffers
containing Tcl source.

Variables controlling Inferior Tcl mode:
  tcl-application
    Name of program to run.
  tcl-command-switches
    Command line arguments to `tcl-application'.
  tcl-prompt-regexp
    Matches prompt.
  inferior-tcl-source-command
    Command to use to read Tcl file in running application.
  inferior-tcl-buffer
    The current inferior Tcl process buffer.  See variable
    documentation for details on multiple-process support.

The following commands are available:
\\{inferior-tcl-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp (or tcl-prompt-regexp
				 (concat "^"
					 (regexp-quote tcl-application)
					 ">")))
  (setq major-mode 'inferior-tcl-mode)
  (setq mode-name "Inferior Tcl")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-tcl-mode-map)
  (setq local-abbrev-table tcl-mode-abbrev-table)
  (set-syntax-table tcl-mode-syntax-table)
  (if tcl-using-emacs-19
      (progn
	(make-local-variable 'defun-prompt-regexp)
	(setq defun-prompt-regexp tcl-omit-ws-regexp)))
  (make-local-variable 'inferior-tcl-delete-prompt-marker)
  (setq inferior-tcl-delete-prompt-marker (make-marker))
  (set-process-filter (get-buffer-process (current-buffer)) 'tcl-filter)
  (run-hooks 'inferior-tcl-mode-hook))

;;;###autoload
(defun inferior-tcl (cmd)
  "Run inferior Tcl process.
Prefix arg means enter program name interactively.
See documentation for function `inferior-tcl-mode' for more information."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Run Tcl: " tcl-application)
	   tcl-application)))
  (if (not (comint-check-proc "*inferior-tcl*"))
      (progn
	(set-buffer (apply (function make-comint) "inferior-tcl" cmd nil
			   tcl-command-switches))
	(inferior-tcl-mode)))
  (make-local-variable 'tcl-application)
  (setq tcl-application cmd)
  (setq inferior-tcl-buffer "*inferior-tcl*")
  (switch-to-buffer "*inferior-tcl*"))

(and (fboundp 'defalias)
     (defalias 'run-tcl 'inferior-tcl))



;;
;; Auto-fill support.
;;

(defun tcl-real-command-p ()
  "Return nil if point is not at the beginning of a command.
A command is the first word on an otherwise empty line, or the
first word following a semicolon, opening brace, or opening bracket."
  (save-excursion
    (skip-chars-backward " \t")
    (cond
     ((bobp) t)
     ((bolp)
      (backward-char)
      ;; Note -- continued comments are not supported here.  I
      ;; consider those to be a wart on the language.
      (not (eq ?\\ (preceding-char))))
     (t
      (memq (preceding-char) '(?\; ?{ ?\[))))))

;; FIXME doesn't actually return t.  See last case.
(defun tcl-real-comment-p ()
  "Return t if point is just after the `#' beginning a real comment.
Does not check to see if previous char is actually `#'.
A real comment is either at the beginning of the buffer,
preceeded only by whitespace on the line, or has a preceeding
semicolon, opening brace, or opening bracket on the same line."
  (save-excursion
    (backward-char)
    (tcl-real-command-p)))

(defun tcl-hairy-scan-for-comment (state end always-stop)
  "Determine if point is in a comment.
Returns a list of the form `(FLAG . STATE)'.  STATE can be used
as input to future invocations.  FLAG is nil if not in comment,
t otherwise.  If in comment, leaves point at beginning of comment.
Only works in Emacs 19.  See also `tcl-simple-scan-for-comment', a
simpler version that is often right, and works in Emacs 18."
  (let ((bol (save-excursion
	       (goto-char end)
	       (beginning-of-line)
	       (point)))
	real-comment
	last-cstart)
    (while (and (not last-cstart) (< (point) end))
      (setq real-comment nil)		;In case we've looped around and it is
                                        ;set.
      (setq state (parse-partial-sexp (point) end nil nil state t))
      (if (nth 4 state)
	  (progn
	    ;; If ALWAYS-STOP is set, stop even if we don't have a
	    ;; real comment, or if the comment isn't on the same line
	    ;; as the end.
	    (if always-stop (setq last-cstart (point)))
	    ;; If we have a real comment, then set the comment
	    ;; starting point if we are on the same line as the ending
	    ;; location.
	    (setq real-comment (tcl-real-comment-p))
	    (if real-comment
		(progn
		  (and (> (point) bol) (setq last-cstart (point)))
		  ;; NOTE Emacs 19 has a misfeature whereby calling
		  ;; parse-partial-sexp with COMMENTSTOP set and with
		  ;; an initial list that says point is in a comment
		  ;; will cause an immediate return.  So we must skip
		  ;; over the comment ourselves.
		  (beginning-of-line 2)))
	    ;; Frob the state to make it look like we aren't in a
	    ;; comment.
	    (setcar (nthcdr 4 state) nil))))
    (and last-cstart
	 (goto-char last-cstart))
    (cons real-comment state)))

(defun tcl-hairy-in-comment ()
  "Return t if point is in a comment, and leave point at beginning
of comment."
  (let ((save (point)))
    (tcl-beginning-of-defun)
    (car (tcl-hairy-scan-for-comment nil save nil))))

(defun tcl-simple-in-comment ()
  "Return t if point is in comment, and leave point at beginning
of comment.  This is faster that `tcl-hairy-in-comment', but is
correct less often."
  (let ((save (point))
	comment)
    (beginning-of-line)
    (while (and (< (point) save) (not comment))
      (search-forward "#" save 'move)
      (setq comment (tcl-real-comment-p)))
    comment))

(defun tcl-in-comment ()
  "Return t if point is in comment, and leave point at beginning
of comment."
  (if (and tcl-pps-has-arg-6
	   tcl-use-hairy-comment-detector)
      (tcl-hairy-in-comment)
    (tcl-simple-in-comment)))

(defun tcl-do-auto-fill ()
  "Auto-fill function for Tcl mode.  Only auto-fills in a comment."
  (let (in-comment
	col)
    (save-excursion
      (setq in-comment (tcl-in-comment))
      (if in-comment
	  (setq col (1- (current-column)))))
    (if in-comment
	(progn
	  (do-auto-fill)
	  (save-excursion
	    (back-to-indentation)
	    (delete-region (point) (save-excursion
				     (beginning-of-line)
				     (point)))
	    (indent-to-column col))))))



;;
;; Help-related code.
;;

(defvar tcl-help-saved-dirs nil
  "Saved help directories.
If `tcl-help-directory-list' changes, this allows `tcl-help-on-word'
to update the alist.")

(defvar tcl-help-alist nil
  "Alist with command names as keys and filenames as values.")

(defun tcl-help-snarf-commands (dirlist)
  "Build alist of commands and filenames."
  (while dirlist
    (let ((files (directory-files (car dirlist) t)))
      (while files
	(if (and (file-directory-p (car files))
		 (not
		  (let ((fpart (file-name-nondirectory (car files))))
		    (or (equal fpart ".")
			(equal fpart "..")))))
	    (let ((matches (directory-files (car files) t)))
	      (while matches
		(or (file-directory-p (car matches))
		    (setq tcl-help-alist
			  (cons
			   (cons (file-name-nondirectory (car matches))
				 (car matches))
			   tcl-help-alist)))
		(setq matches (cdr matches)))))
	(setq files (cdr files))))
    (setq dirlist (cdr dirlist))))

(defun tcl-reread-help-files ()
  "Set up to re-read files, and then do it."
  (interactive)
  (message "Building Tcl help file index...")
  (setq tcl-help-saved-dirs tcl-help-directory-list)
  (setq tcl-help-alist nil)
  (tcl-help-snarf-commands tcl-help-directory-list)
  (message "Building Tcl help file index...done"))

(defun tcl-current-word (flag)
  "Return current command word, or nil.
If FLAG is nil, just uses `current-word'.
Otherwise scans backward for most likely Tcl command word."
  (if (and flag
	   (memq major-mode '(tcl-mode inferior-tcl-mode)))
      (condition-case nil
	  (save-excursion
	    ;; Look backward for first word actually in alist.
	    (if (bobp)
		()
	      (while (and (not (bobp))
			  (not (tcl-real-command-p)))
		(backward-sexp)))
	    (if (assoc (current-word) tcl-help-alist)
		(current-word)))
	(error nil))
    (current-word)))

;;;###autoload
(defun tcl-help-on-word (command &optional arg)
  "Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'."
  (interactive
   (list
    (progn
      (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
	  (tcl-reread-help-files))
      (let ((word (tcl-current-word
		   (if current-prefix-arg
		       (not tcl-use-smart-word-finder)
		     tcl-use-smart-word-finder))))
	(completing-read
	 (if (or (null word) (string= word ""))
	     "Help on Tcl command: "
	   (format "Help on Tcl command (default %s): " word))
	 tcl-help-alist nil t)))
    current-prefix-arg))
  (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
      (tcl-reread-help-files))
  (if (string= command "")
      (setq command (tcl-current-word
		     (if arg
			 (not tcl-use-smart-word-finder)
		       tcl-use-smart-word-finder))))
  (let* ((help (get-buffer-create "*Tcl help*"))
	 (cell (assoc command tcl-help-alist))
	 (file (and cell (cdr cell))))
    (set-buffer help)
    (delete-region (point-min) (point-max))
    (if file
	(progn
	  (insert "*** " command "\n\n")
	  (insert-file-contents file))
      (if (string= command "")
	  (insert "Magical Pig!")
	(insert "Tcl command " command " not in help\n")))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (display-buffer help)))



;;
;; Other interactive stuff.
;;

(defvar tcl-previous-dir/file nil
  "Record last directory and file used in loading.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `tcl-load-file' command.")

(defun tcl-load-file (file &optional and-go)
  "Load a Tcl file into the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive
   (list
    ;; car because comint-get-source returns a list holding the
    ;; filename.
    (car (comint-get-source "Load Tcl file: "
			    (or (and
				 (eq major-mode 'tcl-mode)
				 (buffer-file-name))
				tcl-previous-dir/file)
			    '(tcl-mode) t))
    current-prefix-arg))
  (comint-check-source file)
  (setq tcl-previous-dir/file (cons (file-name-directory file)
				    (file-name-nondirectory file)))
  (tcl-send-string (inferior-tcl-proc)
		   (format inferior-tcl-source-command (tcl-quote file)))
  (if and-go (switch-to-tcl t)))

(defun tcl-restart-with-file (file &optional and-go)
  "Restart inferior Tcl with file.
If an inferior Tcl process exists, it is killed first.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive
   (list
    (car (comint-get-source "Restart with Tcl file: "
			    (or (and
				 (eq major-mode 'tcl-mode)
				 (buffer-file-name))
				tcl-previous-dir/file)
			    '(tcl-mode) t))
    current-prefix-arg))
  (let* ((buf (if (eq major-mode 'inferior-tcl-mode)
		  (current-buffer)
		inferior-tcl-buffer))
	 (proc (and buf (get-process buf))))
    (cond
     ((not (and buf (get-buffer buf)))
      ;; I think this will be ok.
      (inferior-tcl tcl-application)
      (tcl-load-file file and-go))
     ((or
       (not (comint-check-proc buf))
       (yes-or-no-p
	"A Tcl process is running, are you sure you want to reset it? "))
      (save-excursion
	(comint-check-source file)
	(setq tcl-previous-dir/file (cons (file-name-directory file)
					  (file-name-nondirectory file)))
	(comint-exec (get-buffer-create buf)
		     (if proc
			 (process-name proc)
		       "inferior-tcl")
		     tcl-application file tcl-command-switches)
	(if and-go (switch-to-tcl t)))))))

;; FIXME I imagine you can do this under Emacs 18.  I just don't know
;; how.
(defun tcl-auto-fill-mode (&optional arg)
  "Like `auto-fill-mode', but controls filling of Tcl comments."
  (interactive "P")
  (and (not tcl-using-emacs-19)
       (error "You must use Emacs 19 to get this feature."))
  ;; Following code taken from "auto-fill-mode" (simple.el).
  (prog1
      (setq auto-fill-function
	    (if (if (null arg)
		    (not auto-fill-function)
		  (> (prefix-numeric-value arg) 0))
		'tcl-do-auto-fill
	      nil))
    ;; Update mode line.  FIXME I'd use force-mode-line-update, but I
    ;; don't know if it exists in v18.
    (set-buffer-modified-p (buffer-modified-p))))

(defun tcl-electric-hash (&optional count)
  "Insert a `#' and quote if it does not start a real comment.
Prefix arg is number of `#'s to insert.
See variable `tcl-electric-hash-style' for description of quoting
styles."
  (interactive "p")
  (or count (setq count 1))
  (if (> count 0)
      (let ((type
	     (if (eq tcl-electric-hash-style 'smart)
		 (if (> count 3)	; FIXME what is "smart"?
		     'quote
		   'backslash)
	       tcl-electric-hash-style))
	    comment)
	(if type
	    (progn
	      (save-excursion
		(insert "#")
		(setq comment (tcl-in-comment)))
	      (delete-char 1)
	      (and tcl-explain-indentation (message "comment: %s" comment))
	      (cond
	       ((eq type 'quote)
		(if (not comment)
		    (insert "\"")))
	       ((eq type 'backslash)
		;; The following will set count to 0, so the
		;; insert-char can still be run.
		(if (not comment)
		    (while (> count 0)
		      (insert "\\#")
		      (setq count (1- count)))))
	       (t nil))))
	(insert-char ?# count))))

(defun tcl-hashify-buffer ()
  "Quote all `#'s in current buffer that aren't Tcl comments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and tcl-pps-has-arg-6 tcl-use-hairy-comment-detector)
	(let (state
	      result)
	  (while (< (point) (point-max))
	    (setq result (tcl-hairy-scan-for-comment state (point-max) t))
	    (if (car result)
		(beginning-of-line 2)
	      (backward-char)
	      (if (eq ?# (following-char))
		  (insert "\\"))
	      (forward-char))
	    (setq state (cdr result))))
      (while (and (< (point) (point-max))
		  (search-forward "#" nil 'move))
	(if (tcl-real-comment-p)
	    (beginning-of-line 2)
	  ;; There's really no good way for the simple converter to
	  ;; work.  So we just quote # if it isn't already quoted.
	  ;; Bogus, but it works.
	  (backward-char)
	  (if (not (eq ?\\ (preceding-char)))
	      (insert "\\"))
	  (forward-char))))))

(defun tcl-indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment.
Is smart about syntax of Tcl comments.
Parts of this were taken from indent-for-comment (simple.el)."
  (interactive "*")
  (end-of-line)
  (or (tcl-in-comment)
      (progn
	;; Not in a comment, so we have to insert one.  Create an
	;; empty comment (since there isn't one on this line).  If
	;; line is not blank, make sure we insert a ";" first.
	(skip-chars-backward " \t")
	(let ((eolpoint (point)))
	  (beginning-of-line)
	  (if (/= (point) eolpoint)
	      (progn
		(goto-char eolpoint)
		(insert
		 (if (tcl-real-command-p) "" ";")
		 "# ")
		(backward-char))))))
  ;; Point is just after the "#" starting a comment.  Move it as
  ;; appropriate.
  (let* ((indent (if comment-indent-hook
		     (funcall comment-indent-hook)
		   (funcall comment-indent-function)))
	 (begpos (progn
		   (backward-char)
		   (point))))
    (if (/= begpos indent)
	(progn
	  (skip-chars-backward " \t" (save-excursion
				       (beginning-of-line)
				       (point)))
	  (delete-region (point) begpos)
	  (indent-to indent)))
    (looking-at comment-start-skip)	; Always true.
    (goto-char (match-end 0))
    ;; I don't like the effect of the next two.
    ;;(skip-chars-backward " \t" (match-beginning 0))
    ;;(skip-chars-backward "^ \t" (match-beginning 0))
    ))

;; The following was inspired by the Tcl editing mode written by
;; Gregor Schmid <schmid@fb3-s7.math.TU-Berlin.DE>.  His version also
;; attempts to snarf the command line options from the command line,
;; but I didn't think that would really be that helpful (doesn't seem
;; like it owould be right enough.  His version also looks for the
;; "#!/bin/csh ... exec" hack, but that seemed even less useful.
;; FIXME should make sure that the application mentioned actually
;; exists.
(defun tcl-guess-application ()
  "Attempt to guess Tcl application by looking at first line.
The first line is assumed to look like \"#!.../program ...\"."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "#![^ \t]*/\\([^ \t\n/]+\\)\\([ \t]\\|$\\)")
	(progn
	  (make-local-variable 'tcl-application)
	  (setq tcl-application (buffer-substring (match-beginning 1)
						  (match-end 1)))))))

;; This only exists to put on the menubar.  I couldn't figure out any
;; other way to do it.  FIXME should take "number of #-marks"
;; argument.
(defun tcl-uncomment-region (beg end)
  "Uncomment region."
  (interactive "r")
  (comment-region beg end -1))



;;
;; XEmacs menu support.
;; Taken from schmid@fb3-s7.math.TU-Berlin.DE (Gregor Schmid),
;; who wrote a different Tcl mode.
;; We also have support for menus in FSF.  We do this by
;; loading the XEmacs menu emulation code.
;;

(defun tcl-popup-menu (e)
  (interactive "@e")
  (and tcl-using-emacs-19
       (not tcl-using-xemacs-19)
       (if tcl-using-emacs-19-23
	   (require 'lmenu)
	 ;; CAVEATS:
	 ;; * lmenu.el provides 'menubar, which is bogus.
	 ;; * lmenu.el causes menubars to be turned on everywhere.
	 ;;   Doubly bogus!
	 ;; Both of these problems are fixed in Emacs 19.23.  People
	 ;; using an Emacs before that just suffer.
	 (require 'menubar "lmenu")))  ;; This is annoying
  ;; IMHO popup-menu should be autoloaded in FSF Emacs.  Oh well.
  (popup-menu tcl-xemacs-menu))



;;
;; Quoting and unquoting functions.
;;

;; This quoting is sufficient to protect eg a filename from any sort
;; of expansion or splitting.  Tcl quoting sure sucks.
(defun tcl-quote (string)
  "Quote STRING according to Tcl rules."
  (mapconcat (function (lambda (char)
			 (if (memq char '(?[ ?] ?{ ?} ?\\ ?\" ?$ ?  ?\;))
			     (concat "\\" (char-to-string char))
			   (char-to-string char))))
	     string ""))



;;
;; Bug reporting.
;;

(and (fboundp 'eval-when-compile)
     (eval-when-compile
       (require 'reporter)))

(defun tcl-submit-bug-report ()
  "Submit via mail a bug report on Tcl mode."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you really want to submit a bug report on Tcl mode? ")
   (reporter-submit-bug-report
    tcl-maintainer
    (concat "Tcl mode " tcl-version)
    '(tcl-indent-level
      tcl-continued-indent-level
      tcl-auto-newline
      tcl-tab-always-indent
      tcl-use-hairy-comment-detector
      tcl-electric-hash-style
      tcl-help-directory-list
      tcl-use-smart-word-finder
      tcl-application
      tcl-command-switches
      tcl-prompt-regexp
      inferior-tcl-source-command
      tcl-using-emacs-19
      tcl-using-emacs-19-23
      tcl-using-xemacs-19
      tcl-proc-list
      tcl-proc-regexp
      tcl-typeword-list
      tcl-keyword-list
      tcl-font-lock-keywords
      tcl-pps-has-arg-6))))



(provide 'tcl)

;;; tcl.el ends here
