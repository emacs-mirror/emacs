;;; mh-e.el --- GNU Emacs interface to the MH mail system

;; Copyright (C) 1985,86,87,88,90,92,93,94,95,97,2000,2001,2002 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Version: 6.1.1
;; Keywords: mail

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

;; How to Use:
;;   M-x mh-rmail to read mail.  Type C-h m there for a list of commands.
;;   C-u M-x mh-rmail to visit any folder.
;;   M-x mh-smail to send mail.  From within the mail reader, "m" works, too.

;; Your .emacs might benefit from these bindings:
;;   (global-set-key "\C-cr" 'mh-rmail)
;;   (global-set-key "\C-xm" 'mh-smail)
;;   (global-set-key "\C-x4m" 'mh-smail-other-window)

;; MH (Message Handler) is a powerful mail reader.

;; The MH newsgroup is comp.mail.mh; the mailing list is mh-users@ics.uci.edu
;; (send to mh-users-request to be added). See the monthly Frequently Asked
;; Questions posting there for information on getting MH and mh-e:
;;   http://www.faqs.org/faqs/mail/mh-faq/part1/preamble.html

;; N.B. MH must have been compiled with the MHE compiler flag or several
;; features necessary for mh-e will be missing from MH commands, specifically
;; the -build switch to repl and forw.

;; mh-e is an Emacs interface to the MH mail system.

;; mh-e is supported in GNU Emacs 20 and 21, with MH 6.8.4 and nmh 1.0.4.

;; Mailing Lists:
;;   mh-e-users@lists.sourceforge.net
;;   mh-e-announce@lists.sourceforge.net
;;   mh-e-devel@lists.sourceforge.net
;;
;;   Subscribe by sending a "subscribe" message to
;;   <list>-request@lists.sourceforge.net, or by using the web interface at
;;   https://sourceforge.net/mail/?group_id=13357

;; Bug Reports:
;;   https://sourceforge.net/tracker/?group_id=13357&atid=113357
;;   Include the output of M-x mh-version in any bug report.

;; Feature Requests:
;;   https://sourceforge.net/tracker/?atid=363357&group_id=13357&func=browse

;; Support:
;;   https://sourceforge.net/tracker/?group_id=13357&atid=213357

;;; Change Log:

;; Original version for Gosling emacs by Brian Reid, Stanford, 1982.
;; Modified by James Larus, BBN, July 1984 and UCB, 1984 & 1985.
;; Rewritten for GNU Emacs, James Larus 1985.  larus@ginger.berkeley.edu
;; Modified by Stephen Gildea 1988.  gildea@lcs.mit.edu
;; Maintenance picked up by Bill Wohler <wohler@newt.com> and the
;; SourceForge Crew <http://mh-e.sourceforge.net/>. 2001.

;; $Id: mh-e.el,v 1.99.1.1 2002/10/01 19:41:43 wohler Exp $

;;; Code:

(provide 'mh-e)
(require 'mh-utils)
(require 'gnus-util)
(require 'easymenu)
(if (save-match-data (string-match "XEmacs\\|Lucid" emacs-version))
    (require 'mh-xemacs-compat))
(eval-when-compile (require 'cl))

(defconst mh-version "6.1.1" "Version number of mh-e.")

;;; Initial Autoloads

(autoload 'Info-goto-node "info")


;;; Hooks:

(defgroup mh nil
  "Emacs interface to the MH mail system."
  :group 'mail)

(defgroup mh-hook nil
  "Hooks to mh-e mode."
  :prefix "mh-"
  :group 'mh)

(defcustom mh-folder-mode-hook nil
  "Invoked in MH-Folder mode on a new folder."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-inc-folder-hook nil
  "Invoked by \\<mh-folder-mode-map>`\\[mh-inc-folder]' after incorporating mail into a folder."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-folder-updated-hook nil
  "Invoked when the folder actions (such as moves and deletes) are performed.
Variables that are useful in this hook include `mh-delete-list' and
`mh-refile-list' which can be used to see which changes are being made to
current folder, `mh-current-folder'."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-show-hook nil
  "Invoked after \\<mh-folder-mode-map>`\\[mh-show]' shows a message."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-show-mode-hook nil
  "Invoked in MH-Show mode on each message."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-delete-msg-hook nil
  "Invoked after marking each message for deletion."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-refile-msg-hook nil
  "Invoked after marking each message for refiling."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-before-quit-hook nil
  "Invoked by \\<mh-folder-mode-map>`\\[mh-quit]' before quitting mh-e.
See also `mh-quit-hook'."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-quit-hook nil
  "Invoked after \\<mh-folder-mode-map>`\\[mh-quit]' quits mh-e.
See also `mh-before-quit-hook'."
  :type 'hook
  :group 'mh-hook)

(defcustom mh-unseen-updated-hook nil
  "Invoked after the unseen sequence has been updated.
The variable `mh-seen-list' can be used to obtain the list of messages which
will be removed from the unseen sequence."
  :type 'hook
  :group 'mh-hook)

;;; Personal preferences:

(defcustom mh-lpr-command-format "lpr -J '%s'"
  "*Format for Unix command that prints a message.
The string should be a Unix command line, with the string '%s' where
the job's name (folder and message number) should appear.  The formatted
message text is piped to this command when you type \\<mh-folder-mode-map>`\\[mh-print-msg]'."
  :type 'string
  :group 'mh)

(defcustom mh-scan-prog "scan"
  "*Program to run to generate one-line-per-message listing of a folder.
Normally \"scan\" or a file name linked to scan.  This file is searched
for relative to the mh-progs directory unless it is an absolute pathname."
  :type 'string
  :group 'mh)
(make-variable-buffer-local 'mh-scan-prog)

(defcustom mh-inc-prog "inc"
  "*Program to run to incorporate new mail into a folder.
Normally \"inc\".  This file is searched for relative to
the mh-progs directory unless it is an absolute pathname."
  :type 'string
  :group 'mh)

(defcustom mh-print-background nil
  "*Print messages in the background if non-nil.
WARNING: do not delete the messages until printing is finished;
otherwise, your output may be truncated."
  :type 'boolean
  :group 'mh)

(defcustom mh-recenter-summary-p nil
  "*Recenter summary window when the show window is toggled off if non-nil."
  :type 'boolean
  :group 'mh)

(defcustom mh-do-not-confirm nil
  "*Non-nil means do not prompt for confirmation.
Commands such as `mh-pack-folder' prompt to confirm whether to process
outstanding moves and deletes or not before continuing. A non-nil setting will
perform the action--which is usually desired but cannot be retracted--without
question."
  :type 'boolean
  :group 'mh)

(defcustom mh-store-default-directory nil
  "*Last directory used by \\[mh-store-msg]; default for next store.
A directory name string, or nil to use current directory."
  :type '(choice (const :tag "Current" nil)
		 directory)
  :group 'mh)

(defvar mh-note-deleted "D"
  "String whose first character is used to notate deleted messages.")

(defvar mh-note-refiled "^"
  "String whose first character is used to notate refiled messages.")

(defvar mh-note-cur "+"
  "String whose first character is used to notate the current message.")

(defvar mh-partial-folder-mode-line-annotation "select"
  "Annotation when displaying part of a folder.
The string is displayed after the folder's name.  NIL for no annotation.")

;;; Parameterize mh-e to work with different scan formats.  The defaults work
;;; with the standard MH scan listings, in which the first 4 characters on
;;; the line are the message number, followed by two places for notations.

(defcustom mh-scan-format-file t
  "Specifies the format file to pass to the scan program.
If t, the format string will be taken from the either `mh-scan-format-mh'
or `mh-scan-format-nmh' depending on whether MH or nmh is in use.
If nil, the default scan output will be used.

If you customize the scan format, you may need to modify a few variables
containing regexps that mh-e uses to identify specific portions of the output.
Use `M-x apropos RET mh-scan.*regexp' to obtain a list of these variables."
  :type '(choice (const :tag "Use mh-e scan format" t)
                 (const :tag "Use default scan format" nil)
                 (file  :tag "Specify a scan format file"))
  :group 'mh)

;; The following scan formats are passed to the scan program if the
;; setting of `mh-scan-format-file' above is nil.  They are identical
;; except the later one makes use of the nmh `decode' function to
;; decode RFC 2047 encodings.

(defvar mh-scan-format-mh
  (concat
   "%4(msg)"
   "%<(cur)+%| %>"
   "%<{replied}-"
   "%?(nonnull(comp{to}))%<(mymbox{to})t%>"
   "%?(nonnull(comp{cc}))%<(mymbox{cc})c%>"
   "%?(nonnull(comp{bcc}))%<(mymbox{bcc})b%>"
   "%?(nonnull(comp{newsgroups}))n%>"
   "%<(zero) %>"
   "%02(mon{date})/%02(mday{date})%<{date} %|*%>"
   "%<(mymbox{from})%<{to}To:%14(friendly{to})%>%>"
   "%<(zero)%17(friendly{from})%>  "
   "%{subject}%<{body}<<%{body}%>")
  "*Scan format string for MH, provided to the scan program via the -format arg.
This format is identical to the default except that additional hints for
fontification have been added to the sixth column.

The values of the sixth column, in priority order, are: `-' if the
message has been replied to, t if an address on the To: line matches
one of the mailboxes of the current user, `c' if the Cc: line matches,
`b' if the Bcc: line matches, and `n' if a non-empty Newsgroups: header
is present.")

(defvar mh-scan-format-nmh
  (concat
   "%4(msg)"
   "%<(cur)+%| %>"
   "%<{replied}-"
   "%?(nonnull(comp{to}))%<(mymbox{to})t%>"
   "%?(nonnull(comp{cc}))%<(mymbox{cc})c%>"
   "%?(nonnull(comp{bcc}))%<(mymbox{bcc})b%>"
   "%?(nonnull(comp{newsgroups}))n%>"
   "%<(zero) %>"
   "%02(mon{date})/%02(mday{date})%<{date} %|*%>"
   "%<(mymbox{from})%<{to}To:%14(decode(friendly{to}))%>%>"
   "%<(zero)%17(decode(friendly{from}))%>  "
   "%(decode{subject})%<{body}<<%{body}%>")
  "*Scan format string for nmh, provided to the scan program via the -format arg.
This format is identical to the default except that additional hints for
fontification have been added to the sixth column.

The values of the sixth column, in priority order, are: `-' if the
message has been replied to, t if an address on the To: line matches
one of the mailboxes of the current user, `c' if the Cc: line matches,
`b' if the Bcc: line matches, and `n' if a non-empty Newsgroups: header
is present.")

(defvar mh-scan-good-msg-regexp  "^\\(....\\)[^D^]"
  "Regexp specifying the scan lines that are 'good' messages.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least one parenthesized expression which matches the message number.")

(defvar mh-scan-deleted-msg-regexp "^\\(....\\)D"
  "Regexp matching scan lines of deleted messages.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least one parenthesized expression which matches the message number.")

(defvar mh-scan-refiled-msg-regexp  "^\\(....\\)\\^"
  "Regexp matching scan lines of refiled messages.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least one parenthesized expression which matches the message number.")

(defvar mh-scan-valid-regexp "^ *[0-9]"
  "Regexp matching scan lines for messages (not error messages).")

(defvar mh-scan-cur-msg-number-regexp "^\\(....\\+\\).*"
  "Regexp matching scan line for the current message.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least one parenthesized expression which matches the message number.
Don't disable this regexp as it's needed by non fontifying functions.")

(defvar mh-scan-cur-msg-regexp "^\\(....\\+DISABLED.*\\)"
  "Regexp matching scan line for the current message.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least one parenthesized expression which matches the whole line.
To enable this feature, remove the string DISABLED from the regexp.")

(defvar mh-scan-date-regexp "\\([0-9][0-9]/[0-9][0-9]\\)"
  "Regexp matching a valid date in scan lines.
The default `mh-folder-font-lock-keywords' expects this expression to contain
only one parenthesized expression which matches the date field
\(see `mh-scan-format-regexp').")

(defvar mh-scan-rcpt-regexp  "\\(To:\\)\\(..............\\)"
  "Regexp specifying the recipient in scan lines for messages we sent.
The default `mh-folder-font-lock-keywords' expects this expression to contain
two parenthesized expressions.  The first is expected to match the To:
that the default scan format file generates.  The second is expected to match
the recipient's name.")

(defvar mh-scan-body-regexp "\\(<<\\([^\n]+\\)?\\)"
  "Regexp matching the message body beginning displayed in scan lines.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least one parenthesized expression which matches the body text.")

(defvar mh-scan-subject-regexp
  "^...............................\\([Rr][Ee]:\\s-*\\)*\\([^<\n]*\\)"
  "*Regexp matching the subject string in MH folder mode.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least two parenthesized expressions. The first is expected to match the Re:
string, if any. The second is expected to match the subject line itself.")

(defvar mh-scan-format-regexp
  (concat "\\([bct]\\)" mh-scan-date-regexp " \\(..................\\)")
  "Regexp matching the output of scan using `mh-scan-format-mh' or `mh-scan-format-nmh'.
The default `mh-folder-font-lock-keywords' expects this expression to contain
at least three parenthesized expressions. The first should match the
fontification hint, the second is found in `mh-scan-date-regexp', and the
third should match the user name.")

(defvar mh-folder-followup-face 'mh-folder-followup-face
  "Face for highlighting Re: (followup) subject text in MH-Folder buffers.")
(defface mh-folder-followup-face
  '((((class color) (background light))
     (:foreground "blue3"))
    (((class color) (background dark))
     (:foreground "LightGoldenRod"))
    (t
     (:bold t)))
  "Face for highlighting Re: (followup) subject text in MH-Folder buffers."
  :group 'mh)
(defvar mh-folder-address-face 'mh-folder-address-face
  "Face for highlighting the address in MH-Folder buffers.")
(copy-face 'mh-folder-subject-face 'mh-folder-address-face)
(defvar mh-folder-scan-format-face 'mh-folder-scan-format-face
  "Face for highlighting `mh-scan-format-regexp' matches in MH-Folder buffers.")
(copy-face 'mh-folder-followup-face 'mh-folder-scan-format-face)

(defvar mh-folder-date-face 'mh-folder-date-face
  "Face for highlighting the date in MH-Folder buffers.")
(defface mh-folder-date-face
  '((((class color) (background light))
     (:foreground "snow4"))
    (((class color) (background dark))
     (:foreground "snow3"))
    (t
     (:bold t)))
  "Face for highlighting the date in MH-Folder buffers."
  :group 'mh)

(defvar mh-folder-msg-number-face 'mh-folder-msg-number-face
  "Face for highlighting the message number in MH-Folder buffers.")
(defface mh-folder-msg-number-face
  '((((class color) (background light))
     (:foreground "snow4"))
    (((class color) (background dark))
     (:foreground "snow3"))
    (t
     (:bold t)))
  "Face for highlighting the message number in MH-Folder buffers."
  :group 'mh)

(defvar mh-folder-deleted-face 'mh-folder-deleted-face
  "Face for highlighting deleted messages in MH-Folder buffers.")
(copy-face 'mh-folder-msg-number-face 'mh-folder-deleted-face)

(defvar mh-folder-cur-msg-face 'mh-folder-cur-msg-face
  "Face for the current message line in MH-Folder buffers.")
(defface mh-folder-cur-msg-face
  '((((type tty pc) (class color))
     (:background "LightGreen"))
    (((class color) (background light))
     (:background "LightGreen")         ;Use this for solid background colour
;;;  (:underline t)                     ;Use this for underlining
     )
    (((class color) (background dark))
     (:background "DarkOliveGreen4"))
    (t (:underline t)))
  "Face for the current message line in MH-Folder buffers."
  :group 'mh)

;;mh-folder-subject-face is defined in mh-utils since it's needed there
;;for mh-show-subject-face.

(eval-after-load "font-lock"
  '(progn
     (defvar mh-folder-refiled-face 'mh-folder-refiled-face
       "Face for highlighting refiled messages in MH-Folder buffers.")
     (copy-face 'font-lock-variable-name-face 'mh-folder-refiled-face)
     (defvar mh-folder-cur-msg-number-face 'mh-folder-cur-msg-number-face
       "Face for highlighting the current message in MH-Folder buffers.")
     (copy-face 'font-lock-keyword-face 'mh-folder-cur-msg-number-face)
     (defvar mh-folder-to-face 'mh-folder-to-face
       "Face for highlighting the To: string in MH-Folder buffers.")
     (copy-face 'font-lock-string-face 'mh-folder-to-face)
     (defvar mh-folder-body-face 'mh-folder-body-face
       "Face for highlighting body text in MH-Folder buffers.")
     (copy-face 'font-lock-string-face 'mh-folder-body-face)
     
     (defvar mh-folder-font-lock-keywords
       (list
        ;; Marked for deletion
        (list (concat mh-scan-deleted-msg-regexp ".*")
	      '(0 mh-folder-deleted-face))
        ;; Marked for refile
        (list (concat mh-scan-refiled-msg-regexp ".*")
	      '(0 mh-folder-refiled-face))
        ;;after subj
        (list mh-scan-body-regexp '(1 mh-folder-body-face nil t))
        '(mh-folder-font-lock-subject
          (1 mh-folder-followup-face append t)
          (2 mh-folder-subject-face append t))
        ;;current msg
        (list mh-scan-cur-msg-number-regexp
	      '(1 mh-folder-cur-msg-number-face))
        (list mh-scan-good-msg-regexp
	      '(1 mh-folder-msg-number-face))  ;; Msg number
        (list mh-scan-date-regexp '(1 mh-folder-date-face))       ;; Date
        (list mh-scan-rcpt-regexp
              '(1 mh-folder-to-face)                              ;; To:
              '(2 mh-folder-address-face))                        ;; address
        ;; scan font-lock name
        (list mh-scan-format-regexp
              '(1 mh-folder-date-face)
              '(3 mh-folder-scan-format-face))
        ;; Current message line
        (list mh-scan-cur-msg-regexp
	      '(1 mh-folder-cur-msg-face prepend t))
        ;; Unseen messages in bold
        '(mh-folder-font-lock-unseen (1 'bold append t))
        )
       "Regexp keywords used to fontify the MH-Folder buffer.")
     ))

(defun mh-folder-font-lock-subject (limit)
  "Return mh-e scan subject strings to font-lock between point and LIMIT."
  (if (not (re-search-forward mh-scan-subject-regexp limit t))
      nil
    (if (match-beginning 1)
        (set-match-data (list (match-beginning 1) (match-end 2)
                              (match-beginning 1) (match-end 2) nil nil))
      (set-match-data (list (match-beginning 2) (match-end 2)
                            nil nil (match-beginning 2) (match-end 2))))
    t))

;; Fontifify unseen mesages in bold. - Peter S Galbraith <psg@debian.org>
(defvar mh-folder-unseen-seq-name nil
  "Name of unseen sequence.
The default for this is provided by the function `mh-folder-unseen-seq-name'
On nmh systems.")

(defun mh-folder-unseen-seq-name ()
  "Provide name of unseen sequence from mhparam."
  (or mh-progs (mh-find-path))
  (save-excursion
    (let ((tmp-buffer (get-buffer-create mh-temp-buffer))
          (unseen-seq-name "unseen"))
      (set-buffer tmp-buffer)
      (unwind-protect
          (progn
            (call-process (expand-file-name "mhparam" mh-progs)
                          nil '(t t) nil "-component" "Unseen-Sequence")
            (goto-char (point-min))
            (if (re-search-forward "Unseen-Sequence: \\(.*\\)$" nil t)
                (setq unseen-seq-name (match-string 1))))
        (kill-buffer tmp-buffer))
      unseen-seq-name)))

(defun mh-folder-unseen-seq-list ()
  "Return a list of unseen message numbers for current folder."
  (if (not mh-folder-unseen-seq-name)
      (setq mh-folder-unseen-seq-name (mh-folder-unseen-seq-name)))
  (cond
   ((not mh-folder-unseen-seq-name)
    nil)
   (t
    (let ((folder mh-current-folder))
      (save-excursion
	(let ((tmp-buffer (get-buffer-create mh-temp-buffer)))
	  (set-buffer tmp-buffer)
	  (unwind-protect
	      (progn
		(call-process (expand-file-name "mark" mh-progs)
                              nil '(t t) nil 
                              folder "-seq" mh-folder-unseen-seq-name
			      "-list")
		(goto-char (point-min))
		(sort (mh-read-msg-list) '<))
	    (kill-buffer tmp-buffer))))))))

(defvar mh-folder-unseen-seq-cache nil
  "Internal cache variable used for font-lock in mh-e.
Should only be non-nil through font-lock stepping, and nil once font-lock
is done highlighting.")
(make-variable-buffer-local 'mh-folder-unseen-seq-cache)

(defun mh-folder-font-lock-unseen (limit)
  "Return unseen message lines to font-lock between point and LIMIT."
  (if (not mh-folder-unseen-seq-cache)
      (setq mh-folder-unseen-seq-cache (mh-folder-unseen-seq-list)))
  (let ((cur-msg (mh-get-msg-num nil)))
    (cond
     ((not mh-folder-unseen-seq-cache)
      nil)
     ((not cur-msg)                     ;Presumably at end of buffer
      (setq mh-folder-unseen-seq-cache nil)
      nil)
     ((member cur-msg mh-folder-unseen-seq-cache)
      (let ((bpoint (progn (beginning-of-line)(point)))
            (epoint (progn (forward-line 1)(point))))
        (if (<= limit (point))
            (setq  mh-folder-unseen-seq-cache nil))
        (set-match-data (list bpoint epoint bpoint epoint))
        t))
     (t
      ;; move forward one line at a time, checking each message number.
      (while (and
              (= 0 (forward-line 1))
              (> limit (point))
              (not (member (mh-get-msg-num nil) mh-folder-unseen-seq-cache))))
      ;; Examine how we must have exited the loop...
      (let ((cur-msg (mh-get-msg-num nil)))
        (cond
         ((or (not cur-msg)
              (<= limit (point))
              (not (member cur-msg mh-folder-unseen-seq-cache)))
          (setq mh-folder-unseen-seq-cache nil)
          nil)
         ((member cur-msg mh-folder-unseen-seq-cache)
          (let ((bpoint (progn (beginning-of-line)(point)))
                (epoint (progn (forward-line 1)(point))))
            (if (<= limit (point))
                (setq  mh-folder-unseen-seq-cache nil))
            (set-match-data (list bpoint epoint bpoint epoint))
            t))))))))
;; fontifify unseen mesages in bold. - end

;;; Internal variables:

(defvar mh-last-destination nil)	;Destination of last refile or write command.

(defvar mh-folder-mode-map (make-keymap)
  "Keymap for MH folders.")

(defvar mh-delete-list nil)		;List of msg numbers to delete.

(defvar mh-refile-list nil)		;List of folder names in mh-seq-list.

(defvar mh-next-direction 'forward)	;Direction to move to next message.

(defvar mh-narrowed-to-seq nil)		;Sequence display is narrowed to or nil if not narrowed.

(defvar mh-first-msg-num nil)		;Number of first msg in buffer.

(defvar mh-last-msg-num nil)		;Number of last msg in buffer.

(defvar mh-mode-line-annotation nil)	;Message range displayed in buffer.

;;; Macros and generic functions:

(defun mh-mapc (func list)
  (while list
    (funcall func (car list))
    (setq list (cdr list))))

(defun mh-scan-format ()
  "Generate arguments to the scan program to specify which format string should be used."
  (if (equal mh-scan-format-file t)
      (list "-format" (if mh-nmh-p
			  (list mh-scan-format-nmh)
			(list mh-scan-format-mh)))
    (if (not (equal mh-scan-format-file nil))
      (list "-form" mh-scan-format-file))))



;;; Entry points:

;;;###autoload
(defun mh-rmail (&optional arg)
  "Inc(orporate) new mail with MH.
Scan an MH folder if ARG is non-nil. This function is an entry point to mh-e,
the Emacs front end to the MH mail system."
  (interactive "P")
  (mh-find-path)
  (if arg
      (call-interactively 'mh-visit-folder)
      (mh-inc-folder)))

;;;###autoload
(defun mh-nmail (&optional arg)
  "Check for new mail in inbox folder.
Scan an MH folder if ARG is non-nil. This function is an entry point to mh-e,
the Emacs front end to the MH mail system."
  (interactive "P")
  (mh-find-path) ; init mh-inbox
  (if arg
      (call-interactively 'mh-visit-folder)
    (mh-visit-folder mh-inbox)))



;;; User executable mh-e commands:


(defun mh-delete-msg (msg-or-seq)
  "Mark the specified MSG-OR-SEQ for subsequent deletion and move to the next.

Default is the displayed message. If optional prefix argument is given then
prompt for the message sequence. If variable `transient-mark-mode' is non-nil
and the mark is active, then the selected region is marked for deletion."
  (interactive (list (cond
                      ((and (boundp 'transient-mark-mode)
                            transient-mark-mode mark-active)
                       (mh-region-to-sequence (region-beginning)(region-end))
                       'region)
                      (current-prefix-arg
                       (mh-read-seq-default "Delete" t))
                      (t
                       (mh-get-msg-num t)))))
  (mh-delete-msg-no-motion msg-or-seq)
  (mh-next-msg))


(defun mh-delete-msg-no-motion (msg-or-seq)
  "Mark the specified MSG-OR-SEQ for subsequent deletion.
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
			 (mh-read-seq-default "Delete" t)
			 (mh-get-msg-num t))))
  (if (numberp msg-or-seq)
      (mh-delete-a-msg msg-or-seq)
      (mh-map-to-seq-msgs 'mh-delete-a-msg msg-or-seq)))


(defun mh-execute-commands ()
  "Process outstanding delete and refile requests."
  (interactive)
  (if mh-narrowed-to-seq (mh-widen))
  (mh-process-commands mh-current-folder)
  (mh-set-scan-mode)
  (mh-goto-cur-msg)			; after mh-set-scan-mode for efficiency
  (mh-make-folder-mode-line)
  t)					; return t for [local-]write-file-hooks


(defun mh-first-msg ()
  "Move to the first message."
  (interactive)
  (goto-char (point-min))
  (while (and (not (eobp)) (not (looking-at mh-scan-valid-regexp)))
    (forward-line 1)))


(defun mh-header-display ()
  "Show the current message with all its headers.
Displays headers that might have been suppressed by setting the
variables `mh-clean-message-header' or `mhl-formfile', or by the fallback
behavior of scrolling uninteresting headers off the top of the window.
Type \"\\[mh-show]\" to show the message normally again."
  (interactive)
  (and (not mh-showing-with-headers)
       (or mhl-formfile mh-clean-message-header)
       (mh-invalidate-show-buffer))
  (let ((mhl-formfile nil)
	(mh-clean-message-header nil))
    (mh-show-msg nil)
    (mh-in-show-buffer (mh-show-buffer)
      (goto-char (point-min))
      (mh-recenter 0))
    (setq mh-showing-with-headers t)))


(defun mh-inc-folder (&optional maildrop-name)
  "Inc(orporate)s new mail into the Inbox folder.
Optional argument MAILDROP-NAME specifies an alternate maildrop from the
default. If the prefix argument is given, incorporates mail into the current
folder, otherwise uses the folder named by `mh-inbox'.
Runs `mh-inc-folder-hook' after incorporating new mail.
Do not call this function from outside mh-e; use \\[mh-rmail] instead."
  (interactive (list (if current-prefix-arg
			 (expand-file-name
			  (read-file-name "inc mail from file: "
					  mh-user-path)))))
  (let ((config (current-window-configuration)))
    (if (not maildrop-name)
	(cond ((not (get-buffer mh-inbox))
	       (mh-make-folder mh-inbox)
	       (setq mh-previous-window-config config))
	      ((not (eq (current-buffer) (get-buffer mh-inbox)))
	       (switch-to-buffer mh-inbox)
	       (setq mh-previous-window-config config)))))
  (mh-get-new-mail maildrop-name)
  (if mh-showing-mode (mh-show))
  (run-hooks 'mh-inc-folder-hook))


(defun mh-last-msg ()
  "Move to the last message."
  (interactive)
  (goto-char (point-max))
  (while (and (not (bobp)) (looking-at "^$"))
    (forward-line -1)))


(defun mh-next-undeleted-msg (&optional arg)
  "Move to the next undeleted message ARG in window."
  (interactive "p")
  (setq mh-next-direction 'forward)
  (forward-line 1)
  (cond ((re-search-forward mh-scan-good-msg-regexp nil 0 arg)
	 (beginning-of-line)
	 (mh-maybe-show))
	(t
	 (forward-line -1)
	 (if (get-buffer mh-show-buffer)
	     (delete-windows-on mh-show-buffer)))))


(defun mh-refile-msg (msg-or-seq folder)
  "Refile MSG-OR-SEQ (default: displayed message) into FOLDER.
If optional prefix argument provided, then prompt for message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is marked for refiling."
  (interactive
   (list (cond
          ((and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
           (mh-region-to-sequence (region-beginning)(region-end))
           'region)
          (current-prefix-arg
           (mh-read-seq-default "Refile" t))
          (t
           (mh-get-msg-num t)))
	 (intern
	  (mh-prompt-for-folder
	   "Destination"
	   (or (and mh-default-folder-for-message-function
		    (let ((refile-file (mh-msg-filename (mh-get-msg-num t))))
		      (save-excursion
			(set-buffer (get-buffer-create mh-temp-buffer))
			(erase-buffer)
			(insert-file-contents refile-file)
			(let ((buffer-file-name refile-file))
			  (funcall mh-default-folder-for-message-function)))))
	       (and (eq 'refile (car mh-last-destination))
		    (symbol-name (cdr mh-last-destination)))
	       "")
	   t))))
  (setq mh-last-destination (cons 'refile folder))
  (if (numberp msg-or-seq)
      (mh-refile-a-msg msg-or-seq folder)
      (mh-map-to-seq-msgs 'mh-refile-a-msg msg-or-seq folder))
  (mh-next-msg))


(defun mh-refile-or-write-again (message)
  "Re-execute the last refile or write command on the given MESSAGE.
Default is the displayed message.  Use the same folder or file as the
previous refile or write command."
  (interactive (list (mh-get-msg-num t)))
  (if (null mh-last-destination)
      (error "No previous refile or write"))
  (cond ((eq (car mh-last-destination) 'refile)
	 (mh-refile-a-msg message (cdr mh-last-destination))
	 (message "Destination folder: %s" (cdr mh-last-destination)))
	(t
	 (apply 'mh-write-msg-to-file message (cdr mh-last-destination))
	 (message "Destination: %s" (cdr mh-last-destination))))
  (mh-next-msg))

(defun mh-quit ()
  "Quit the current mh-e folder.
Start by running `mh-before-quit-hook'.  Restore the previous window
configuration, if one exists.  Finish by running `mh-quit-hook'."
  (interactive)
  (run-hooks 'mh-before-quit-hook)
  (mh-update-sequences)
  (mh-invalidate-show-buffer)
  (bury-buffer (current-buffer))
  (if (get-buffer mh-show-buffer)
      (bury-buffer mh-show-buffer))
  (if (get-buffer mh-temp-buffer)
      (kill-buffer mh-temp-buffer))
  (if (get-buffer mh-temp-folders-buffer)
      (kill-buffer mh-temp-folders-buffer))
  (if (get-buffer mh-temp-sequences-buffer)
      (kill-buffer mh-temp-sequences-buffer))
  (if mh-previous-window-config
      (set-window-configuration mh-previous-window-config))
  (run-hooks 'mh-quit-hook))

(defun mh-page-msg (&optional arg)
  "Page the displayed message forwards.
Scrolls ARG lines or a full screen if no argument is supplied. Show buffer
first if not displayed. Show the next undeleted message if looking at the
bottom of the current message."
  (interactive "P")
  (if mh-showing-mode
      (if mh-page-to-next-msg-p
	  (if (equal mh-next-direction 'backward)
	      (mh-previous-undeleted-msg)
	    (mh-next-undeleted-msg))
	(if (mh-in-show-buffer (mh-show-buffer)
	      (pos-visible-in-window-p (point-max)))
	    (progn
	      (message (format
			"End of message (Type %s to read %s undeleted message)"
			(single-key-description last-input-event)
			(if (equal mh-next-direction 'backward)
			    "previous"
			  "next")))
	      (setq mh-page-to-next-msg-p t))
	  (scroll-other-window arg)))
    (mh-show)))


(defun mh-previous-page (&optional arg)
  "Page the displayed message backwards.
Scrolls ARG lines or a full screen if no argument is supplied."
  (interactive "P")
  (mh-in-show-buffer (mh-show-buffer)
    (scroll-down arg)))


(defun mh-previous-undeleted-msg (&optional arg)
  "Move to the previous undeleted message ARG in window."
  (interactive "p")
  (setq mh-next-direction 'backward)
  (beginning-of-line)
  (cond ((re-search-backward mh-scan-good-msg-regexp nil 0 arg)
	 (mh-maybe-show))
	(t
	 (if (get-buffer mh-show-buffer)
	     (delete-windows-on mh-show-buffer)))))


(defun mh-rescan-folder (&optional range)
  "Rescan a folder after optionally processing the outstanding commands.
If optional prefix argument RANGE is provided, prompt for the range of
messages to display.  Otherwise show the entire folder."
  (interactive (list (if current-prefix-arg
			 (mh-read-msg-range "Range to scan [all]? ")
		       nil)))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder (or range "all")))


(defun mh-write-msg-to-file (msg file no-headers)
  "Append MSG to the end of a FILE.
If prefix argument NO-HEADERS is provided, write only the message body.
Otherwise send the entire message including the headers."
  (interactive
   (list (mh-get-msg-num t)
	 (let ((default-dir (if (eq 'write (car mh-last-destination))
				(file-name-directory (car (cdr mh-last-destination)))
			      default-directory)))
	   (read-file-name (format "Save message%s in file: "
				   (if current-prefix-arg " body" ""))
			   default-dir
			   (if (eq 'write (car mh-last-destination))
			       (car (cdr mh-last-destination))
			     (expand-file-name "mail.out" default-dir))))
	 current-prefix-arg))
  (let ((msg-file-to-output (mh-msg-filename msg))
	(output-file (mh-expand-file-name file)))
    (setq mh-last-destination (list 'write file (if no-headers 'no-headers)))
    (save-excursion
      (set-buffer (get-buffer-create mh-temp-buffer))
      (erase-buffer)
      (insert-file-contents msg-file-to-output)
      (goto-char (point-min))
      (if no-headers (search-forward "\n\n"))
      (append-to-file (point) (point-max) output-file))))


(defun mh-toggle-showing ()
  "Toggle the scanning mode/showing mode of displaying messages."
  (interactive)
  (if mh-showing-mode
      (mh-set-scan-mode)
    (mh-show)))


(defun mh-undo (msg-or-seq)
  "Undo the pending deletion or refile of the specified MSG-OR-SEQ.
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is unmarked."
  (interactive (list (cond
                      ((and (boundp 'transient-mark-mode)
                            transient-mark-mode mark-active)
                       (mh-region-to-sequence (region-beginning)(region-end))
                       'region)
                      (current-prefix-arg
                       (mh-read-seq-default "Undo" t))
                      (t
                       (mh-get-msg-num t)))))
  (cond ((numberp msg-or-seq)
	 (let ((original-position (point)))
	   (beginning-of-line)
	   (while (not (or (looking-at mh-scan-deleted-msg-regexp)
			   (looking-at mh-scan-refiled-msg-regexp)
			   (and (eq mh-next-direction 'forward) (bobp))
			   (and (eq mh-next-direction 'backward)
				(save-excursion (forward-line) (eobp)))))
	     (forward-line (if (eq mh-next-direction 'forward) -1 1)))
	   (if (or (looking-at mh-scan-deleted-msg-regexp)
		   (looking-at mh-scan-refiled-msg-regexp))
	       (progn
		 (mh-undo-msg (mh-get-msg-num t))
		 (mh-maybe-show))
	       (goto-char original-position)
	       (error "Nothing to undo"))))
	(t
	 (mh-map-to-seq-msgs 'mh-undo-msg msg-or-seq)))
  ;; update the mh-refile-list so mh-outstanding-commands-p will work
  (mh-mapc (function
	    (lambda (elt)
	      (if (not (mh-seq-to-msgs elt))
		  (setq mh-refile-list (delq elt mh-refile-list)))))
	   mh-refile-list)
  (if (not (mh-outstanding-commands-p))
      (mh-set-folder-modified-p nil)))


;;;###autoload
(defun mh-version ()
  "Display version information about mh-e and the MH mail handling system."
  (interactive)
  (mh-find-progs)
  (set-buffer (get-buffer-create mh-temp-buffer))
  (erase-buffer)
  ;; mh-e and Emacs versions.
  (insert "mh-e " mh-version "\n\n" (emacs-version) "\n\n")
  ;; MH version.
  (let ((help-start (point)))
    (condition-case err-data
	(mh-exec-cmd-output "inc" nil (if mh-nmh-p "-version" "-help"))
      (file-error (insert (mapconcat 'concat (cdr err-data) ": ") "\n")))
    (goto-char help-start)
    (if mh-nmh-p
	(search-forward "inc -- " nil t)
      (search-forward "version: " nil t))
    (delete-region help-start (point)))
  (goto-char (point-max))
  (insert "mh-progs:\t" mh-progs "\n"
	  "mh-lib:\t\t" mh-lib "\n"
	  "mh-lib-progs:\t" mh-lib-progs "\n\n")
  ;; Linux version.
  (condition-case ()
      (call-process "uname" nil t nil "-a")
    (file-error))
  (goto-char (point-min))
  (display-buffer mh-temp-buffer))


(defun mh-visit-folder (folder &optional range)
  "Visit FOLDER and display RANGE of messages.
Do not call this function from outside mh-e; see \\[mh-rmail] instead."
  (interactive (list (mh-prompt-for-folder "Visit" mh-inbox t)
		     (mh-read-msg-range "Range [all]? ")))
  (let ((config (current-window-configuration)))
    (mh-scan-folder folder (or range "all"))
    (setq mh-previous-window-config config))
  nil)


(defun mh-update-sequences ()
  "Update MH's Unseen sequence and current folder and message.
Flush mh-e's state out to MH.  The message at the cursor becomes current."
  (interactive)
  ;; mh-update-sequences is the opposite of mh-read-folder-sequences,
  ;; which updates mh-e's state from MH.
  (let ((folder-set (mh-update-unseen))
	(new-cur (mh-get-msg-num nil)))
    (if new-cur
	(let ((seq-entry (mh-find-seq 'cur)))
	  (mh-remove-cur-notation)
	  (setcdr seq-entry (list new-cur)) ;delete-seq-locally, add-msgs-to-seq
	  (mh-define-sequence 'cur (list new-cur))
	  (beginning-of-line)
	  (if (looking-at mh-scan-good-msg-regexp)
	      (mh-notate nil mh-note-cur mh-cmd-note)))
      (or folder-set
	  (save-excursion
            ;; psg - mh-current-folder is nil if mh-summary-height < 4 !
            ;;       So I added this sanity check.
            (if (stringp mh-current-folder)
                (mh-exec-cmd-quiet t "folder" mh-current-folder "-fast")
              (mh-exec-cmd-quiet t "folder" "-fast")))))))




;;; Support routines.

(defun mh-delete-a-msg (msg)
  ;; Delete the MESSAGE.
  (save-excursion
    (mh-goto-msg msg nil t)
    (if (looking-at mh-scan-refiled-msg-regexp)
	(error "Message %d is refiled.  Undo refile before deleting" msg))
    (if (looking-at mh-scan-deleted-msg-regexp)
	nil
	(mh-set-folder-modified-p t)
	(setq mh-delete-list (cons msg mh-delete-list))
	(mh-add-msgs-to-seq msg 'deleted t)
	(mh-notate msg mh-note-deleted mh-cmd-note)
	(run-hooks 'mh-delete-msg-hook))))

(defun mh-refile-a-msg (msg destination)
  ;; Refile MESSAGE in FOLDER.  FOLDER is a symbol, not a string.
  (save-excursion
    (mh-goto-msg msg nil t)
    (cond ((looking-at mh-scan-deleted-msg-regexp)
	   (error "Message %d is deleted.  Undo delete before moving" msg))
	  ((looking-at mh-scan-refiled-msg-regexp)
	   (if (y-or-n-p
		(format "Message %d already refiled.  Copy to %s as well? "
			msg destination))
	       (mh-exec-cmd "refile" (mh-get-msg-num t) "-link"
			    "-src" mh-current-folder
			    (symbol-name destination))
	       (message "Message not copied.")))
	  (t
	   (mh-set-folder-modified-p t)
	   (if (not (memq destination mh-refile-list))
	       (setq mh-refile-list (cons destination mh-refile-list)))
	   (if (not (memq msg (mh-seq-to-msgs destination)))
	       (mh-add-msgs-to-seq msg destination t))
	   (mh-notate msg mh-note-refiled mh-cmd-note)
	   (run-hooks 'mh-refile-msg-hook)))))


(defun mh-next-msg ()
  ;; Move backward or forward to the next undeleted message in the buffer.
  (if (eq mh-next-direction 'forward)
      (mh-next-undeleted-msg 1)
      (mh-previous-undeleted-msg 1)))


(defun mh-set-scan-mode ()
  ;; Display the scan listing buffer, but do not show a message.
  (if (get-buffer mh-show-buffer)
      (delete-windows-on mh-show-buffer))
  (mh-showing-mode 0)
  (force-mode-line-update)
  (if mh-recenter-summary-p
      (mh-recenter nil)))


(defun mh-undo-msg (msg)
  ;; Undo the deletion or refile of one MESSAGE.
  (cond ((memq msg mh-delete-list)
	 (setq mh-delete-list (delq msg mh-delete-list))
	 (mh-delete-msg-from-seq msg 'deleted t))
	(t
	 (mh-mapc (function (lambda (dest)
			      (mh-delete-msg-from-seq msg dest t)))
		  mh-refile-list)))
  (mh-notate msg ?  mh-cmd-note))




;;; The folder data abstraction.

(defun mh-make-folder (name)
  ;; Create and initialize a new mail folder called NAME and make it the
  ;; current folder.
  (switch-to-buffer name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)
  (mh-folder-mode)
  (mh-set-folder-modified-p nil)
  (setq buffer-file-name mh-folder-filename)
  (mh-make-folder-mode-line))


;;; Ensure new buffers won't get this mode if default-major-mode is nil.
(put 'mh-folder-mode 'mode-class 'special)

(define-derived-mode mh-folder-mode fundamental-mode "MH-Folder"
  "Major mh-e mode for \"editing\" an MH folder scan listing.\\<mh-folder-mode-map>

You can show the message the cursor is pointing to, and step through the
messages.  Messages can be marked for deletion or refiling into another
folder; these commands are executed all at once with a separate command.

A prefix argument (\\[universal-argument]) to delete, refile, list, or undo
applies the action to a message sequence. If `transient-mark-mode',
is non-nil, the action is applied to the region.

Options that control this mode can be changed with \\[customize-group];
specify the \"mh\" group. In particular, please see the `mh-scan-format-file'
option if you wish to modify scan's format.

When a folder is visited, the hook `mh-folder-mode-hook' is run.

\\{mh-folder-mode-map}"

  (make-local-variable 'font-lock-defaults)
  (setq	font-lock-defaults '(mh-folder-font-lock-keywords t))
  (mh-make-local-vars
   'mh-current-folder (buffer-name)	; Name of folder, a string
   'mh-show-buffer (format "show-%s" (buffer-name)) ; Buffer that displays msgs
   'mh-folder-filename			; e.g. "/usr/foobar/Mail/inbox/"
   (file-name-as-directory (mh-expand-file-name (buffer-name)))
   'mh-showing-mode nil			; Show message also?
   'mh-delete-list nil			; List of msgs nums to delete
   'mh-refile-list nil			; List of folder names in mh-seq-list
   'mh-seq-list nil			; Alist of (seq . msgs) nums
   'mh-seen-list nil			; List of displayed messages
   'mh-next-direction 'forward		; Direction to move to next message
   'mh-narrowed-to-seq nil		; Sequence display is narrowed to
   'mh-first-msg-num nil		; Number of first msg in buffer
   'mh-last-msg-num nil			; Number of last msg in buffer
   'mh-msg-count nil			; Number of msgs in buffer
   'mh-mode-line-annotation nil		; Indiction this is not the full folder
   'mh-previous-window-config nil)	; Previous window configuration
  (setq truncate-lines t)
  (auto-save-mode -1)
  (setq buffer-offer-save t)
  (if (boundp 'local-write-file-hooks)
      (setq local-write-file-hooks '(mh-execute-commands)) ;Emacs 19
    (make-local-variable 'write-file-hooks)
    (setq write-file-hooks '(mh-execute-commands))) ;Emacs 18
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'hl-line-mode) ; avoid pollution
  (if (fboundp 'hl-line-mode)
      (hl-line-mode 1))
  (setq revert-buffer-function 'mh-undo-folder)
  (or (assq 'mh-showing-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(mh-showing-mode " Show") minor-mode-alist)))
  (easy-menu-add mh-folder-sequence-menu)
  (easy-menu-add mh-folder-message-menu)
  (easy-menu-add mh-folder-folder-menu)
  (if (and (boundp 'tool-bar-mode) tool-bar-mode)
      (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map)))


(defun mh-make-local-vars (&rest pairs)
  ;; Take VARIABLE-VALUE pairs and make local variables initialized to the
  ;; value.
  (while pairs
    (set (make-local-variable (car pairs)) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))


(defun mh-scan-folder (folder range)
  ;; Scan the FOLDER over the RANGE.  Return in the folder's buffer.
  (cond ((null (get-buffer folder))
	 (mh-make-folder folder))
	(t
	 (mh-process-or-undo-commands folder)
	 (switch-to-buffer folder)))
  (mh-regenerate-headers range)
  (if (zerop (buffer-size))
	 (if (equal range "all")
	     (message "Folder %s is empty" folder)
	   (message "No messages in %s, range %s" folder range))
	 (mh-goto-cur-msg)))


(defun mh-regenerate-headers (range &optional update)
  ;; scan folder over range RANGE.
  ;; If UPDATE, append the scan lines, otherwise replace.
  (let ((folder mh-current-folder)
	scan-start)
    (message "Scanning %s..." folder)
    (with-mh-folder-updating (nil)
      (if update
	  (goto-char (point-max))
	(erase-buffer))
      (setq scan-start (point))
      (mh-exec-cmd-output mh-scan-prog nil
			  (mh-scan-format)
			  "-noclear" "-noheader"
			  "-width" (window-width)
			  folder range)
      (goto-char scan-start)
      (cond ((looking-at "scan: no messages in")
	     (keep-lines mh-scan-valid-regexp)) ; Flush random scan lines
	    ((looking-at "scan: bad message list ")
	     (keep-lines mh-scan-valid-regexp))
	    ((looking-at "scan: "))	; Keep error messages
	    (t
	     (keep-lines mh-scan-valid-regexp))) ; Flush random scan lines
      (setq mh-seq-list (mh-read-folder-sequences folder nil))
      (mh-notate-user-sequences)
      (or update
	  (setq mh-mode-line-annotation
		(if (equal range "all")
		    nil
		  mh-partial-folder-mode-line-annotation)))
      (mh-make-folder-mode-line))
    (message "Scanning %s...done" folder)))


(defun mh-get-new-mail (maildrop-name)
  ;; Read new mail from a maildrop into the current buffer.
  ;; Return in the current buffer.
  (let ((point-before-inc (point))
	(folder mh-current-folder)
	(new-mail-p nil))
    (with-mh-folder-updating (t)
      (if maildrop-name
	  (message "inc %s -file %s..." folder maildrop-name)
	(message "inc %s..." folder))
      (setq mh-next-direction 'forward)
      (goto-char (point-max))
      (let ((start-of-inc (point)))
	(if maildrop-name
	    ;; I think MH 5 used "-ms-file" instead of "-file",
	    ;; which would make inc'ing from maildrops fail.
	    (mh-exec-cmd-output mh-inc-prog nil folder
				(mh-scan-format)
				"-file" (expand-file-name maildrop-name)
				"-width" (window-width)
				"-truncate")
	    (mh-exec-cmd-output mh-inc-prog nil
				(mh-scan-format)
				"-width" (window-width)))
	(if maildrop-name
	    (message "inc %s -file %s...done" folder maildrop-name)
	  (message "inc %s...done" folder))
	(goto-char start-of-inc)
	(cond ((save-excursion
		 (re-search-forward "^inc: no mail" nil t))
	       (message "No new mail%s%s" (if maildrop-name " in " "")
			(if maildrop-name maildrop-name "")))
	      ((re-search-forward "^inc:" nil t) ; Error messages
	       (error "Error incorporating mail"))
	      (t
	       (mh-remove-cur-notation)
	       (setq new-mail-p t)))
	(keep-lines mh-scan-valid-regexp) ; Flush random scan lines
	(setq mh-seq-list (mh-read-folder-sequences folder t))
	(mh-notate-user-sequences)
	(if new-mail-p
	    (progn
	      (mh-make-folder-mode-line)
	      (mh-goto-cur-msg))
	    (goto-char point-before-inc))))))


(defun mh-make-folder-mode-line (&optional ignored)
  ;; Set the fields of the mode line for a folder buffer.
  ;; The optional argument is now obsolete.  It used to be used to pass
  ;; in what is now stored in the buffer-local variable
  ;; mh-mode-line-annotation.
  (save-excursion
    (mh-first-msg)
    (setq mh-first-msg-num (mh-get-msg-num nil))
    (mh-last-msg)
    (setq mh-last-msg-num (mh-get-msg-num nil))
    (setq mh-msg-count (if mh-first-msg-num
 			   (count-lines (point-min) (point-max))
 			 0))
    (setq mode-line-buffer-identification
	  (list (format "{%%b%s} %s msg%s"
			(if mh-mode-line-annotation
			    (format "/%s" mh-mode-line-annotation)
			  "")
 			(if (zerop mh-msg-count)
 			    "no"
 			  (format "%d" mh-msg-count))
			(if (zerop mh-msg-count)
			    "s"
 			  (cond ((> mh-msg-count 1)
 				 (format "s (%d-%d)" mh-first-msg-num
 					 mh-last-msg-num))
 				(mh-first-msg-num
 				 (format " (%d)" mh-first-msg-num))
 				(""))))))))

(defun mh-unmark-all-headers (remove-all-flags)
  ;; Remove all '+' flags from the headers, and if called with a non-nil
  ;; argument, remove all 'D', '^' and '%' flags too.
  ;; Optimized for speed (i.e., no regular expressions).
  (save-excursion
    (let ((case-fold-search nil)
	  (last-line (1- (point-max)))
	  char)
      (mh-first-msg)
      (while (<= (point) last-line)
	(forward-char mh-cmd-note)
	(setq char (following-char))
	(if (or (and remove-all-flags
		     (or (= char (aref mh-note-deleted 0))
			 (= char (aref mh-note-refiled 0))))
		(= char (aref mh-note-cur 0)))
	    (progn
	      (delete-char 1)
	      (insert " ")))
	(if remove-all-flags
	    (progn
	      (forward-char 1)
	      (if (= (following-char) (aref mh-note-seq 0))
		  (progn
		    (delete-char 1)
		    (insert " ")))))
	(forward-line)))))


(defun mh-remove-cur-notation ()
  ;; Remove old cur notation (cf mh-goto-cur-msg code).
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (save-excursion
      (and cur-msg
	   (mh-goto-msg cur-msg t t)
	   (looking-at mh-scan-cur-msg-number-regexp)
	   (mh-notate nil ?  mh-cmd-note)))))

(defun mh-goto-cur-msg ()
  ;; Position the cursor at the current message.
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (cond ((and cur-msg
		(mh-goto-msg cur-msg t t))
	   (mh-notate nil mh-note-cur mh-cmd-note)
	   (mh-recenter 0)
	   (mh-maybe-show cur-msg))
	  (t
	   (mh-last-msg)
	   (message "No current message")))))


(defun mh-process-or-undo-commands (folder)
  ;; If FOLDER has outstanding commands, then either process or discard them.
  ;; Called by functions like mh-sort-folder, so also invalidate show buffer.
  (set-buffer folder)
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm
	      (y-or-n-p
	       "Process outstanding deletes and refiles (or lose them)? "))
	  (mh-process-commands folder)
	(mh-undo-folder)))
  (mh-update-unseen)
  (mh-invalidate-show-buffer))


(defun mh-process-commands (folder)
  ;; Process outstanding commands for the folder FOLDER.
  (message "Processing deletes and refiles for %s..." folder)
  (set-buffer folder)
  (with-mh-folder-updating (nil)
    ;; Run the hook while the lists are still valid
    (run-hooks 'mh-folder-updated-hook)

    ;; Update the unseen sequence if it exists
    (mh-update-unseen)

    ;; Then refile messages
    (mh-mapc
     (function
      (lambda (dest)
	(let ((msgs (mh-seq-to-msgs dest)))
	  (cond (msgs
		 (apply 'mh-exec-cmd "refile"
			"-src" folder (symbol-name dest)
			(mh-coalesce-msg-list msgs))
		 (mh-delete-scan-msgs msgs))))))
     mh-refile-list)
    (setq mh-refile-list nil)

    ;; Now delete messages
    (cond (mh-delete-list
	   (apply 'mh-exec-cmd "rmm" folder
		  (mh-coalesce-msg-list mh-delete-list))
	   (mh-delete-scan-msgs mh-delete-list)
	   (setq mh-delete-list nil)))

    ;; Don't need to remove sequences since delete and refile do so.

    ;; Mark cur message
    (if (> (buffer-size) 0)
	(mh-define-sequence 'cur (list (or (mh-get-msg-num nil) "last"))))

    (and (buffer-file-name (get-buffer mh-show-buffer))
	 (not (file-exists-p (buffer-file-name (get-buffer mh-show-buffer))))
	 ;; If "inc" were to put a new msg in this file,
	 ;; we would not notice, so mark it invalid now.
	 (mh-invalidate-show-buffer))

    (setq mh-seq-list (mh-read-folder-sequences mh-current-folder nil))
    (mh-unmark-all-headers t)
    (mh-notate-user-sequences)
    (message "Processing deletes and refiles for %s...done" folder)))


(defun mh-update-unseen ()
  ;; Flush updates to the Unseen sequence out to MH.
  ;; Return non-NIL iff set the MH folder.
  (if mh-seen-list
      (let* ((unseen-seq (mh-find-seq mh-unseen-seq))
	     (unseen-msgs (mh-seq-msgs unseen-seq)))
	(if unseen-msgs
	    (progn
	      (mh-undefine-sequence mh-unseen-seq mh-seen-list)
	      (run-hooks 'mh-unseen-updated-hook)
	      (while mh-seen-list
		(setq unseen-msgs (delq (car mh-seen-list) unseen-msgs))
		(setq mh-seen-list (cdr mh-seen-list)))
	      (setcdr unseen-seq unseen-msgs)
	      t)			;since we set the folder
	  (setq mh-seen-list nil)))))


(defun mh-delete-scan-msgs (msgs)
  ;; Delete the scan listing lines for each of the msgs in the LIST.
  (save-excursion
    (while msgs
      (if (mh-goto-msg (car msgs) t t)
	  (mh-delete-line 1))
      (setq msgs (cdr msgs)))))


(defun mh-outstanding-commands-p ()
  ;; Returns non-nil if there are outstanding deletes or refiles.
  (or mh-delete-list mh-refile-list))


(defun mh-coalesce-msg-list (messages)
  ;; Give a list of MESSAGES, return a list of message number ranges.
  ;; Sort of the opposite of mh-read-msg-list, which expands ranges.
  ;; Message lists passed to MH programs go through this so
  ;; command line arguments won't exceed system limits.
  (let ((msgs (sort (copy-sequence messages) 'mh-greaterp))
	(range-high nil)
	(prev -1)
	(ranges nil))
    (while prev
      (if range-high
	  (if (or (not (numberp prev))
		  (not (equal (car msgs) (1- prev))))
	      (progn			;non-sequential, flush old range
		(if (eq prev range-high)
		    (setq ranges (cons range-high ranges))
		  (setq ranges (cons (format "%s-%s" prev range-high) ranges)))
		(setq range-high nil))))
      (or range-high
	  (setq range-high (car msgs))) ;start new or first range
      (setq prev (car msgs))
      (setq msgs (cdr msgs)))
    ranges))

(defun mh-greaterp (msg1 msg2)
  ;; Sort two message indicators.  Strings are "smaller" than numbers.
  ;; Legal values are things like "cur", "last", 1, and 1820.
  (if (numberp msg1)
	 (if (numberp msg2)
	     (> msg1 msg2)
	   t)
    (if (numberp msg2)
	nil
      (string-lessp msg2 msg1))))

(defun mh-lessp (msg1 msg2)
  (not (mh-greaterp msg1 msg2)))


;;; Basic sequence handling

(defun mh-delete-seq-locally (seq)
  ;; Remove mh-e's record of SEQUENCE.
  (let ((entry (mh-find-seq seq)))
    (setq mh-seq-list (delq entry mh-seq-list))))

(defun mh-read-folder-sequences (folder save-refiles)
  ;; Read and return the predefined sequences for a FOLDER.
  ;; If SAVE-REFILES is non-nil, then keep the sequences
  ;; that note messages to be refiled.
  (let ((seqs ()))
    (cond (save-refiles
	    (mh-mapc (function (lambda (seq) ; Save the refiling sequences
				 (if (mh-folder-name-p (mh-seq-name seq))
				     (setq seqs (cons seq seqs)))))
		     mh-seq-list)))
    (save-excursion
      (if (eq 0 (mh-exec-cmd-quiet nil "mark" folder "-list"))
	  (progn
	    ;; look for name in line of form "cur: 4" or "myseq (private): 23"
	    (while (re-search-forward "^[^: ]+" nil t)
	      (setq seqs (cons (mh-make-seq (intern (buffer-substring
						     (match-beginning 0)
						     (match-end 0)))
					    (mh-read-msg-list))
			       seqs)))
	    (delete-region (point-min) (point))))) ; avoid race with mh-process-daemon
    seqs))

(defun mh-read-msg-list ()
  ;; Return a list of message numbers from the current point to the end of
  ;; the line.  Expands ranges into set of individual numbers.
  (let ((msgs ())
	(end-of-line (save-excursion (end-of-line) (point)))
	num)
    (while (re-search-forward "[0-9]+" end-of-line t)
      (setq num (string-to-int (buffer-substring (match-beginning 0)
						 (match-end 0))))
      (cond ((looking-at "-")		; Message range
	     (forward-char 1)
	     (re-search-forward "[0-9]+" end-of-line t)
	     (let ((num2 (string-to-int (buffer-substring (match-beginning 0)
							  (match-end 0)))))
	       (if (< num2 num)
		   (error "Bad message range: %d-%d" num num2))
	       (while (<= num num2)
		 (setq msgs (cons num msgs))
		 (setq num (1+ num)))))
	    ((not (zerop num))		;"pick" outputs "0" to mean no match
	     (setq msgs (cons num msgs)))))
    msgs))

(defun mh-notate-user-sequences ()
  ;; Mark the scan listing of all messages in user-defined sequences.
  (let ((seqs mh-seq-list)
	name)
    (while seqs
      (setq name (mh-seq-name (car seqs)))
      (if (not (mh-internal-seq name))
	  (mh-notate-seq name mh-note-seq (1+ mh-cmd-note)))
      (setq seqs (cdr seqs)))))


(defun mh-internal-seq (name)
  ;; Return non-NIL if NAME is the name of an internal mh-e sequence.
  (or (memq name '(answered cur deleted forwarded printed))
      (eq name mh-unseen-seq)
      (eq name mh-previous-seq)
      (mh-folder-name-p name)))


(defun mh-delete-msg-from-seq (message sequence &optional internal-flag)
  "Delete MESSAGE from SEQUENCE.
MESSAGE defaults to displayed message. From Lisp, optional third arg
INTERNAL-FLAG non-nil means do not inform MH of the change."
  (interactive (list (mh-get-msg-num t)
		     (mh-read-seq-default "Delete from" t)
		     nil))
  (let ((entry (mh-find-seq sequence)))
    (cond (entry
	   (mh-notate-if-in-one-seq message ?  (1+ mh-cmd-note) sequence)
	   (if (not internal-flag)
	       (mh-undefine-sequence sequence (list message)))
	   (setcdr entry (delq message (mh-seq-msgs entry)))))))


(defun mh-undefine-sequence (seq msgs)
  ;; Remove from the SEQUENCE the list of MSGS.
  (mh-exec-cmd "mark" mh-current-folder "-delete"
	       "-sequence" (symbol-name seq)
	       (mh-coalesce-msg-list msgs)))


(defun mh-define-sequence (seq msgs)
  ;; Define the SEQUENCE to contain the list of MSGS.
  ;; Do not mark pseudo-sequences or empty sequences.
  ;; Signals an error if SEQUENCE is an illegal name.
  (if (and msgs
	   (not (mh-folder-name-p seq)))
      (save-excursion
	(mh-exec-cmd-error nil "mark" mh-current-folder "-add" "-zero"
			   "-sequence" (symbol-name seq)
			   (mh-coalesce-msg-list msgs)))))


(defun mh-map-over-seqs (func seq-list)
  ;; Apply the FUNCTION to each element in the list of SEQUENCES,
  ;; passing the sequence name and the list of messages as arguments.
  (while seq-list
    (funcall func (mh-seq-name (car seq-list)) (mh-seq-msgs (car seq-list)))
    (setq seq-list (cdr seq-list))))


(defun mh-notate-if-in-one-seq (msg notation offset seq)
  ;; If the MESSAGE is in only the SEQUENCE, then mark the scan listing of the
  ;; message with the CHARACTER at the given OFFSET from the beginning of the
  ;; listing line.
  (let ((in-seqs (mh-seq-containing-msg msg nil)))
    (if (and (eq seq (car in-seqs)) (null (cdr in-seqs)))
	(mh-notate msg notation offset))))


(defun mh-seq-containing-msg (msg &optional include-internal-p)
  ;; Return a list of the sequences containing MESSAGE.
  ;; If INCLUDE-INTERNAL-P non-nil, include mh-e internal sequences in list.
  (let ((l mh-seq-list)
	(seqs ()))
    (while l
      (and (memq msg (mh-seq-msgs (car l)))
	   (or include-internal-p
	       (not (mh-internal-seq (mh-seq-name (car l)))))
	   (setq seqs (cons (mh-seq-name (car l)) seqs)))
      (setq l (cdr l)))
    seqs))




;;; User prompting commands.


(defun mh-read-msg-range (prompt)
  ;; Read a list of blank-separated items.
  (let* ((buf (read-string prompt))
	 (buf-size (length buf))
	 (start 0)
	 (input ()))
    (while (< start buf-size)
      (let ((next (read-from-string buf start buf-size)))
	(setq input (cons (car next) input))
	(setq start (cdr next))))
    (nreverse input)))



;;; Build the folder-mode keymap:

(suppress-keymap mh-folder-mode-map)

;; Save the `b' binding for a future `back'. Maybe?
(gnus-define-keys  mh-folder-mode-map
  " "		mh-page-msg
  "!"		mh-refile-or-write-again
  ","		mh-header-display
  "."		mh-show			;alias
  ">"		mh-write-msg-to-file
  "E"		mh-extract-rejected-mail
  "\177"	mh-previous-page
  "\C-d"	mh-delete-msg-no-motion
  "\e<"		mh-first-msg
  "\e>"		mh-last-msg
  "\ed"		mh-redistribute
  "\r"		mh-show
  "^"		mh-refile-msg		;alias
  "c"		mh-copy-msg
  "d"		mh-delete-msg
  "e"		mh-edit-again
  "f"		mh-forward
  "g"		mh-goto-msg
  "i"		mh-inc-folder
  "k"  		mh-delete-subject-thread
  "l"		mh-print-msg
  "m"		mh-send			;alias
  "n"		mh-next-undeleted-msg
  "o"		mh-refile-msg
  "p"		mh-previous-undeleted-msg
  "q"		mh-quit
  "r"		mh-reply
  "s"		mh-send
  "t"		mh-toggle-showing
  "u"		mh-undo
  "x"		mh-execute-commands
  "|"		mh-pipe-msg)

(gnus-define-keys (mh-folder-map "F" mh-folder-mode-map)
  "S"		mh-sort-folder
  "f"		mh-visit-folder		;alias
  "k"		mh-kill-folder
  "l"		mh-list-folders
  "o"		mh-visit-folder		;alias
  "p"		mh-pack-folder
  "r"		mh-rescan-folder
  "s"		mh-search-folder
  "u"		mh-undo-folder
  "v"		mh-visit-folder)

(gnus-define-keys (mh-sequence-map "S" mh-folder-mode-map)
  "d"		mh-delete-msg-from-seq
  "k"		mh-delete-seq
  "l"		mh-list-sequences
  "n"		mh-narrow-to-seq
  "p"		mh-put-msg-in-seq
  "s"		mh-msg-is-in-seq
  "w"		mh-widen)

(gnus-define-keys (mh-thread-map "T" mh-folder-mode-map)
  "d"		mh-delete-subject-thread
  "k"		mh-delete-subject-thread
  "s"		mh-narrow-to-subject-thread
  "t"		mh-toggle-subject-thread
  "u"		mh-next-unseen-subject-thread)

(gnus-define-keys (mh-extract-map "X" mh-folder-mode-map)
  "s"		mh-store-msg		;shar
  "u"		mh-store-msg)		;uuencode

(gnus-define-keys (mh-digest-map "D" mh-folder-mode-map)
  " "		mh-page-digest
  "\177"	mh-page-digest-backwards
  "b"		mh-burst-digest)

(cond
 ((not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version))))
  (define-key mh-folder-mode-map [button2] 'mh-show-mouse))
 (t
  (define-key mh-folder-mode-map [mouse-2] 'mh-show-mouse)))

;; "C-c /" prefix is used in mh-folder-mode by pgp.el and mailcrypt

;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
;;; Menus for folder mode: folder, message, sequence (in that order)
;;; folder-mode "Sequence" menu
(easy-menu-define
  mh-folder-sequence-menu mh-folder-mode-map "Menu for mh-e folder-sequence."
  '("Sequence"
    ["Add Msg to Seq..."        mh-put-msg-in-seq (mh-get-msg-num nil)]
    ["List Seq's for Msg"       mh-msg-is-in-seq (mh-get-msg-num nil)]
    ["Delete Msg from Seq..."   mh-delete-msg-from-seq (mh-get-msg-num nil)]
    ["List Seq's in Folder..."  mh-list-sequences t]
    ["Delete Seq..."            mh-delete-seq t]
    ["Show Only Msgs in Seq..." mh-narrow-to-seq t]
    ["Show All Msgs in Folder"  mh-widen mh-narrowed-to-seq]
    "--"
    ["Toggle Subject Thread"    mh-toggle-subject-thread t]
    ["Narrow to Subject Thread"  mh-narrow-to-subject-thread t]
    ["Delete Rest of Subject Thread" mh-delete-subject-thread t]
    ["Next Unseen Subject Thread" mh-next-unseen-subject-thread t]
    "--"
    ["Push State Out to MH"     mh-update-sequences t]))

;;; folder-mode "Message" menu
(easy-menu-define
  mh-folder-message-menu mh-folder-mode-map "Menu for mh-e folder-message."
  '("Message"
    ["Show Msg"                 mh-show (mh-get-msg-num nil)]
    ["Next Msg"                 mh-next-undeleted-msg t]
    ["Previous Msg"             mh-previous-undeleted-msg t]
    ["Go to First Msg"          mh-first-msg t]
    ["Go to Last Msg"           mh-last-msg t]
    ["Go to Msg by Number..."   mh-goto-msg t]
    ["Delete Msg"               mh-delete-msg (mh-get-msg-num nil)]
    ["Refile Msg"               mh-refile-msg (mh-get-msg-num nil)]
    ["Undo Delete/Refile"       mh-undo t]
    ["Process Delete/Refile"    mh-execute-commands
     (or mh-refile-list mh-delete-list)]
    "--"
    ["Compose a New Msg"        mh-send t]
    ["Reply to Msg..."          mh-reply (mh-get-msg-num nil)]
    ["Forward Msg..."           mh-forward (mh-get-msg-num nil)]
    ["Redistribute Msg..."      mh-redistribute (mh-get-msg-num nil)]
    ["Edit Msg Again"           mh-edit-again (mh-get-msg-num nil)]
    ["Re-edit a Bounced Msg"    mh-extract-rejected-mail t]
    "--"
    ["Refile Msg in Folder..."  mh-refile-msg (mh-get-msg-num nil)]
    ["Copy Msg to Folder..."    mh-copy-msg (mh-get-msg-num nil)]
    ["Print Msg"                mh-print-msg (mh-get-msg-num nil)]
    ["Write Msg to File..."     mh-write-msg-to-file (mh-get-msg-num nil)]
    ["Pipe Msg to Command..."   mh-pipe-msg (mh-get-msg-num nil)]
    ["Unpack Uuencoded Msg..."  mh-store-msg (mh-get-msg-num nil)]
    ["Show Msg with Header"     mh-header-display (mh-get-msg-num nil)]
    ["Burst Digest Msg"         mh-burst-digest (mh-get-msg-num nil)]))

;;; folder-mode "Folder" menu
(easy-menu-define
  mh-folder-folder-menu mh-folder-mode-map  "Menu for mh-e folder."
  '("Folder"
    ["Incorporate New Mail"     mh-inc-folder t]
    ["Toggle Show/Folder"       mh-toggle-showing t]
    ["Execute Delete/Refile"    mh-execute-commands
     (or mh-refile-list mh-delete-list)]
    ["Rescan Folder"            mh-rescan-folder t]
    ["Pack Folder"              mh-pack-folder t]
    ["Sort Folder"              mh-sort-folder t]
    "--"
    ["Search a Folder..."       mh-search-folder t]
    ["Visit a Folder..."        mh-visit-folder t]
    ["List Folders"             mh-list-folders t]
    ["Quit MH-E"                mh-quit t]))


;;; Support for emacs21 toolbar using gnus/message.el icons (and code).
(eval-when-compile (defvar tool-bar-map))
(when (and (fboundp 'tool-bar-add-item)
           tool-bar-mode)
  (defvar mh-folder-tool-bar-map
    (let ((tool-bar-map (make-sparse-keymap)))
      (tool-bar-add-item "mail" 'mh-inc-folder 'mh-folder-inc-folder
                         :help "Incorporate new mail in Inbox")
      
      (tool-bar-add-item "left_arrow" 'mh-previous-undeleted-msg
                         'mh-folder-prev :help "Previous message")
      (tool-bar-add-item "page-down" 'mh-page-msg 'mh-folder-page
                         :help "Page this message")
      (tool-bar-add-item "right_arrow" 'mh-next-undeleted-msg 'mh-folder-next
                         :help "Next message")

      (tool-bar-add-item "close" 'mh-delete-msg  'mh-folder-delete
                         :help "Mark for deletion")
      (tool-bar-add-item "refile" 'mh-refile-msg  'mh-folder-refile
                         :help "Refile this message")
      (tool-bar-add-item "undo" 'mh-undo  'mh-folder-undo
                         :help "Undo this mark")
      (tool-bar-add-item "execute" 'mh-execute-commands  'mh-folder-exec
                         :help "Perform moves and deletes")

      (tool-bar-add-item "show" 'mh-toggle-showing 'mh-folder-toggle-show
                         :help "Toggle showing message")

      (tool-bar-add-item "mail/reply2" 'mh-reply 'mh-folder-reply
                         :help "Reply to this message")
      (tool-bar-add-item "mail_compose" 'mh-send 'mh-folder-compose
                         :help "Compose new message")

      (tool-bar-add-item "rescan" 'mh-rescan-folder  'mh-folder-rescan
                         :help "Rescan this folder")
      (tool-bar-add-item "repack" 'mh-pack-folder  'mh-folder-pack
                         :help "Repack this folder")

      (tool-bar-add-item "search" 'mh-search-folder  'mh-folder-search
                         :help "Search this folder")
      (tool-bar-add-item "fld_open" 'mh-visit-folder  'mh-folder-visit
                         :help "Visit other folder")

      (tool-bar-add-item "preferences" (lambda ()
                                         (interactive)
                                         (customize-group "mh"))
                         'mh-folder-customize
                         :help "mh-e preferences")
      (tool-bar-add-item "help" (lambda ()
                                  (interactive)
                                  (Info-goto-node "(mh-e)Top"))
                         'mh-folder-help :help "Help")
      tool-bar-map))

  (defvar mh-folder-seq-tool-bar-map
    (let ((tool-bar-map (copy-keymap mh-folder-tool-bar-map)))
      (tool-bar-add-item "widen" 'mh-widen 'mh-folder-widen
                         :help "Widen from this sequence")
      tool-bar-map)
    "Tool-bar to use when narrowed to a sequence in MH-Folder buffers.")
  )

;;;autoload the other mh-e parts

;;; mh-comp

(autoload 'mh-smail "mh-comp"
  "Compose and send mail with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.
See documentation of `\\[mh-send]' for more details on composing mail." t)

(autoload 'mh-smail-other-window "mh-comp"
  "Compose and send mail in other window with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.
See documentation of `\\[mh-send]' for more details on composing mail." t)

(autoload 'mh-edit-again "mh-comp"
  "Clean-up a draft or a message previously sent and make it resendable.
Default is the current message.
The variable mh-new-draft-cleaned-headers specifies the headers to remove.
See also documentation for `\\[mh-send]' function." t)

(autoload 'mh-extract-rejected-mail "mh-comp"
  "Extract a letter returned by the mail system and make it resendable.
Default is the current message.  The variable mh-new-draft-cleaned-headers
gives the headers to clean out of the original message.
See also documentation for `\\[mh-send]' function." t)

(autoload 'mh-forward "mh-comp"
  "Forward a message or message sequence.  Defaults to displayed message.
If optional prefix argument provided, then prompt for the message sequence.
See also documentation for `\\[mh-send]' function." t)

(autoload 'mh-redistribute "mh-comp"
  "Redistribute a letter.
Depending on how your copy of MH was compiled, you may need to change the
setting of the variable mh-redist-full-contents.  See its documentation." t)

(autoload 'mh-reply "mh-comp"
  "Reply to a MESSAGE (default: displayed message).
If optional prefix argument INCLUDEP provided, then include the message
in the reply using filter mhl.reply in your MH directory.
Prompts for type of addresses to reply to:
   from    sender only,
   to      sender and primary recipients,
   cc/all  sender and all recipients.
If the file named by `mh-repl-formfile' exists, it is used as a skeleton
for the reply.  See also documentation for `\\[mh-send]' function." t)

(autoload 'mh-send "mh-comp"
  "Compose and send a letter.
The file named by `mh-comp-formfile' will be used as the form.
Do not call this function from outside mh-e; use \\[mh-smail] instead.
The letter is composed in mh-letter-mode; see its documentation for more
details.  If `mh-compose-letter-function' is defined, it is called on the
draft and passed three arguments: to, subject, and cc." t)

(autoload 'mh-send-other-window "mh-comp"
  "Compose and send a letter in another window.
Do not call this function from outside mh-e;
use \\[mh-smail-other-window] instead.
See also documentation for `\\[mh-send]' function." t)

(autoload 'mh-letter-mode "mh-comp"
  "Mode for composing letters in mh-e.
For more details, type \\[describe-mode] while in MH-Letter mode." t)


;;; mh-funcs

(autoload 'mh-burst-digest "mh-funcs"
  "Burst apart the current message, which should be a digest.
The message is replaced by its table of contents and the messages from the
digest are inserted into the folder after that message." t)

(autoload 'mh-copy-msg "mh-funcs"
  "Copy to another FOLDER the specified MESSAGE(s) without deleting them.
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence." t)

(autoload 'mh-kill-folder "mh-funcs"
  "Remove the current folder." t)

(autoload 'mh-list-folders "mh-funcs"
  "List mail folders." t)

(autoload 'mh-pack-folder "mh-funcs"
  "Renumber the messages of a folder to be 1..n.
First, offer to execute any outstanding commands for the current folder.
If optional prefix argument provided, prompt for the range of messages
to display after packing.  Otherwise, show the entire folder." t)

(autoload 'mh-pipe-msg "mh-funcs"
  "Pipe the current message through the given shell COMMAND.
If INCLUDE-HEADERS (prefix argument) is provided, send the entire message.
Otherwise just send the message's body without the headers." t)

(autoload 'mh-page-digest "mh-funcs"
  "Advance displayed message to next digested message." t)

(autoload 'mh-page-digest-backwards "mh-funcs"
  "Back up displayed message to previous digested message." t)

(autoload 'mh-print-msg "mh-funcs"
  "Print MESSAGE(s) (default: displayed message) on printer.
If optional prefix argument provided, then prompt for the message sequence.
The variable mh-lpr-command-format is used to generate the print command.
The messages are formatted by mhl.  See the variable mhl-formfile." t)

(autoload 'mh-sort-folder "mh-funcs"
  "Sort the messages in the current folder by date.
Calls the MH program sortm to do the work.
The arguments in the list  mh-sortm-args  are passed to sortm
if this function is passed an argument." t)

(autoload 'mh-undo-folder "mh-funcs"
  "Undo all commands in current folder." t)

(autoload 'mh-store-msg "mh-funcs"
  "Store the file(s) contained in the current message into DIRECTORY.
The message can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
mh-store-default-directory  or the current directory." t)

(autoload 'mh-store-buffer "mh-funcs"
  "Store the file(s) contained in the current buffer into DIRECTORY.
The buffer can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
`mh-store-default-directory' or the current directory." t)


;;; mh-pick

(autoload 'mh-search-folder "mh-pick"
  "Search FOLDER for messages matching a pattern.
Add the messages found to the sequence named `search'." t)

;;; mh-seq

(autoload 'mh-region-to-sequence "mh-seq"
  "Define sequence 'region as the messages in selected region." t)
(autoload 'mh-delete-seq "mh-seq"
  "Delete the SEQUENCE." t)
(autoload 'mh-list-sequences "mh-seq"
  "List the sequences defined in FOLDER." t)
(autoload 'mh-msg-is-in-seq "mh-seq"
  "Display the sequences that contain MESSAGE (default: displayed message)." t)
(autoload 'mh-narrow-to-seq "mh-seq"
  "Restrict display of this folder to just messages in SEQUENCE
Use \\[mh-widen] to undo this command." t)
(autoload 'mh-put-msg-in-seq "mh-seq"
  "Add MESSAGE(s) (default: displayed message) to SEQUENCE.
If optional prefix argument provided, then prompt for the message sequence." t)
(autoload 'mh-widen "mh-seq"
  "Remove restrictions from current folder, thereby showing all messages." t)
(autoload 'mh-rename-seq "mh-seq"
  "Rename SEQUENCE to have NEW-NAME." t)
(autoload 'mh-narrow-to-subject-thread "mh-seq"
  "Narrow to a sequence containing all following messages with same subject."
  t)
(autoload 'mh-toggle-subject-thread "mh-seq"
  "Narrow to or widen from a sequence containing current subject sequence." t)
(autoload 'mh-delete-subject-thread "mh-seq"
  "Mark all following messages with same subject to be deleted." t)
(autoload 'mh-next-unseen-subject-thread "mh-seq"
  "Get the next unseen subject thread." t)


(dolist (mess '("^Cursor not pointing to message$"
		"^There is no other window$"))
  (add-to-list 'debug-ignored-errors mess))

;;; mh-e.el ends here
