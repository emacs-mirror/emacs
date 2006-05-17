;;; mh-e.el --- GNU Emacs interface to the MH mail system

;; Copyright (C) 1985, 1986, 1987, 1988,
;;  1990, 1992, 1993, 1994, 1995, 1997, 1999,
;;  2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Version: 7.85+cvs
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;; Questions posting there for information on getting MH and MH-E:
;;   http://www.faqs.org/faqs/mail/mh-faq/part1/preamble.html

;; N.B. MH must have been compiled with the MHE compiler flag or several
;; features necessary for MH-E will be missing from MH commands, specifically
;; the -build switch to repl and forw.

;; MH-E is an Emacs interface to the MH mail system.

;; MH-E is supported in GNU Emacs 20 and 21, with MH 6.8.4 and nmh 1.0.4.

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
;; Rewritten for GNU Emacs, James Larus, 1985.
;; Modified by Stephen Gildea, 1988.
;; Maintenance picked up by Bill Wohler and the
;; SourceForge Crew <http://mh-e.sourceforge.net/>, 2001.

;;; Code:

(provide 'mh-e)

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-utils)
(require 'mh-init)
(require 'mh-inc)
(require 'mh-seq)
(require 'gnus-util)
(require 'easymenu)

;; Shush the byte-compiler
(defvar font-lock-auto-fontify)
(defvar font-lock-defaults)

(defconst mh-version "7.85+cvs" "Version number of MH-E.")

(defvar mh-partial-folder-mode-line-annotation "select"
  "Annotation when displaying part of a folder.
The string is displayed after the folder's name.  nil for no annotation.")


;;; Scan Line Formats

;;; Parameterize MH-E to work with different scan formats.  The defaults work
;;; with the standard MH scan listings, in which the first 4 characters on
;;; the line are the message number, followed by two places for notations.

;; The following scan formats are passed to the scan program if the setting of
;; `mh-scan-format-file' is t. They are identical except the later one makes
;; use of the nmh `decode' function to decode RFC 2047 encodings. If you just
;; want to change the width of the msg number, use the `mh-set-cmd-note'
;; function.

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
fontification have been added to the fifth column (remember that in Emacs, the
first column is 0).

The values of the fifth column, in priority order, are: `-' if the message has
been replied to, t if an address on the To: line matches one of the
mailboxes of the current user, `c' if the Cc: line matches, `b' if the Bcc:
line matches, and `n' if a non-empty Newsgroups: header is present.")

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
  "*Scan format string for nmh.
This string is passed to the scan program via the -format arg.
This format is identical to the default except that additional hints for
fontification have been added to the fifth column (remember that in Emacs, the
first column is 0).

The values of the fifth column, in priority order, are: `-' if the message has
been replied to, t if an address on the To: field matches one of the
mailboxes of the current user, `c' if the Cc: field matches, `b' if the Bcc:
field matches, and `n' if a non-empty Newsgroups: field is present.")

(defvar mh-note-deleted ?D
  "Deleted messages are marked by this character.
See also `mh-scan-deleted-msg-regexp'.")

(defvar mh-note-refiled ?^
  "Refiled messages are marked by this character.
See also `mh-scan-refiled-msg-regexp'.")

(defvar mh-note-cur ?+
  "The current message (in MH) is marked by this character.
See also `mh-scan-cur-msg-number-regexp'.")

(defvar mh-scan-good-msg-regexp  "^\\( *[0-9]+\\)[^D^0-9]"
  "This regexp specifies the scan lines that are 'good' messages.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which matches the
message number as in the default of \"^\\\\( *[0-9]+\\\\)[^D^0-9]\".")

(defvar mh-scan-deleted-msg-regexp "^\\( *[0-9]+\\)D"
  "This regexp matches deleted messages.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which matches the
message number as in the default of \"^\\\\( *[0-9]+\\\\)D\".
See also `mh-note-deleted'.")

(defvar mh-scan-refiled-msg-regexp  "^\\( *[0-9]+\\)\\^"
  "This regexp matches refiled messages.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which matches the
message number as in the default of \"^\\\\( *[0-9]+\\\\)\\\\^\".
See also `mh-note-refiled'.")

(defvar mh-scan-valid-regexp "^ *[0-9]"
  "This regexp matches scan lines for messages (not error messages).")

(defvar mh-scan-cur-msg-number-regexp "^\\( *[0-9]+\\+\\).*"
  "This regexp matches the current message.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which matches the
message number as in the default of \"^\\\\( *[0-9]+\\\\+\\\\).*\". Don't
disable this regexp as it's needed by non-fontifying functions.
See also `mh-note-cur'.")

(defvar mh-scan-date-regexp "\\([0-9][0-9]/[0-9][0-9]\\)"
  "This regexp matches a valid date.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain only one parenthesized expression which matches the date
field as in the default of \"\\\\([0-9][0-9]/[0-9][0-9]\\\\)\"}.
See also `mh-scan-format-regexp'.")

(defvar mh-scan-rcpt-regexp  "\\(To:\\)\\(..............\\)"
  "This regexp specifies the recipient in messages you sent.
Note that the default setting of `mh-folder-font-lock-keywords'
expects this expression to contain two parenthesized expressions. The
first is expected to match the `To:' that the default scan format
file generates. The second is expected to match the recipient's name
as in the default of \"\\\\(To:\\\\)\\\\(..............\\\\)\".")

(defvar mh-scan-body-regexp "\\(<<\\([^\n]+\\)?\\)"
  "This regexp matches the message body fragment displayed in scan lines.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which matches the
body text as in the default of \"\\\\(<<\\\\([^\\n]+\\\\)?\\\\)\".")

(defvar mh-scan-subject-regexp
  "^ *[0-9]+........[ ]*...................\\([Rr][Ee]\\(\\[[0-9]+\\]\\)?:\\s-*\\)*\\([^<\n]*\\)"
  "This regexp matches the subject.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least three parenthesized expressions. The first is
expected to match the `Re:' string, if any. The second matches an optional
bracketed number after `Re:', such as in `Re[2]:' (and is thus a
sub-expression of the first expression) and the third is expected to match
the subject line itself as in the default of \"^ *[0-9]+........[ ]*...................\\\\([Rr][Ee]\\\\(\\\\\\=[[0-9]+\\\\]\\\\)?:\\\\s-*\\\\)*\\\\([^<\\n]*\\\\)\".")

(defvar mh-scan-format-regexp
  (concat "\\([bct]\\)" mh-scan-date-regexp " *\\(..................\\)")
  "This regexp matches the output of scan.
Note that the default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least three parenthesized expressions. The first
should match the fontification hint, the second is found in
`mh-scan-date-regexp', and the third should match the user name as in the
default of \"(concat \"\\\\([bct]\\\\)\" mh-scan-date-regexp
                     \"*\\\\(..................\\\\)\")\".")



(defvar mh-folder-font-lock-keywords
  (list
   ;; Folders when displaying index buffer
   (list "^\\+.*"
         '(0 mh-index-folder-face))
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
         '(1 mh-folder-msg-number-face)) ;; Msg number
   (list mh-scan-date-regexp '(1 mh-folder-date-face)) ;; Date
   (list mh-scan-rcpt-regexp
         '(1 mh-folder-to-face) ;; To:
         '(2 mh-folder-address-face)) ;; address
   ;; scan font-lock name
   (list mh-scan-format-regexp
         '(1 mh-folder-date-face)
         '(3 mh-folder-scan-format-face)))
  "Regexp keywords used to fontify the MH-Folder buffer.")

(defvar mh-scan-cmd-note-width 1
  "Number of columns consumed by the cmd-note field in `mh-scan-format'.
This column will have one of the values: ` ', `D', `^', `+' and where
` ' is the default value,
`D' is the `mh-note-deleted' character,
`^' is the `mh-note-refiled' character, and
`+' is the `mh-note-cur' character.")

(defvar mh-scan-destination-width 1
  "Number of columns consumed by the destination field in `mh-scan-format'.
This column will have one of ' ', '%', '-', 't', 'c', 'b', or `n' in it.
A ' ' blank space is the default character.
A '%' indicates that the message in in a named MH sequence.
A '-' indicates that the message has been annotated with a replied field.
A 't' indicates that the message contains mymbox in the To: field.
A 'c' indicates that the message contains mymbox in the Cc: field.
A 'b' indicates that the message contains mymbox in the Bcc: field.
A 'n' indicates that the message contains a Newsgroups: field.")

(defvar mh-scan-date-width 5
  "Number of columns consumed by the date field in `mh-scan-format'.
This column will typically be of the form mm/dd.")

(defvar mh-scan-date-flag-width 1
  "Number of columns consumed to flag (in)valid dates in `mh-scan-format'.
This column will have ` ' for valid and `*' for invalid or missing dates.")

(defvar mh-scan-from-mbox-width 17
  "Number of columns consumed with the \"From:\" line in `mh-scan-format'.
This column will have a friendly name or e-mail address of the
originator, or a \"To: address\" for outgoing e-mail messages.")

(defvar mh-scan-from-mbox-sep-width 2
  "Number of columns consumed by whitespace after from-mbox in `mh-scan-format'.
This column will only ever have spaces in it.")

(defvar mh-scan-field-from-start-offset
  (+ mh-scan-cmd-note-width
     mh-scan-destination-width
     mh-scan-date-width
     mh-scan-date-flag-width)
  "The offset from the `mh-cmd-note' to find the start of \"From:\" address.")

(defvar mh-scan-field-from-end-offset
  (+ mh-scan-field-from-start-offset mh-scan-from-mbox-width)
  "The offset from the `mh-cmd-note' to find the end of \"From:\" address.")

(defvar mh-scan-field-subject-start-offset
  (+ mh-scan-cmd-note-width
     mh-scan-destination-width
     mh-scan-date-width
     mh-scan-date-flag-width
     mh-scan-from-mbox-width
     mh-scan-from-mbox-sep-width)
  "The offset from the `mh-cmd-note' to find the start of the subject.")

(defun mh-folder-font-lock-subject (limit)
  "Return MH-E scan subject strings to font-lock between point and LIMIT."
  (if (not (re-search-forward mh-scan-subject-regexp limit t))
      nil
    (if (match-beginning 1)
        (set-match-data (list (match-beginning 1) (match-end 3)
                              (match-beginning 1) (match-end 3) nil nil))
      (set-match-data (list (match-beginning 3) (match-end 3)
                            nil nil (match-beginning 3) (match-end 3))))
    t))



;; Fontifify unseen mesages in bold.

(defmacro mh-generate-sequence-font-lock (seq prefix face)
  "Generate the appropriate code to fontify messages in SEQ.
PREFIX is used to generate unique names for the variables and functions
defined by the macro. So a different prefix should be provided for every
invocation.
FACE is the font-lock face used to display the matching scan lines."
  (let ((cache (intern (format "mh-folder-%s-seq-cache" prefix)))
        (func (intern (format "mh-folder-font-lock-%s" prefix))))
    `(progn
       (defvar ,cache nil
         "Internal cache variable used for font-lock in MH-E.
Should only be non-nil through font-lock stepping, and nil once font-lock
is done highlighting.")
       (make-variable-buffer-local ',cache)

       (defun ,func (limit)
         "Return unseen message lines to font-lock between point and LIMIT."
         (if (not ,cache) (setq ,cache (mh-seq-msgs (mh-find-seq ,seq))))
         (let ((cur-msg (mh-get-msg-num nil)))
           (cond ((not ,cache)
                  nil)
                 ((>= (point) limit)              ;Presumably at end of buffer
                  (setq ,cache nil)
                  nil)
                 ((member cur-msg ,cache)
                  (let ((bpoint (progn (beginning-of-line)(point)))
                        (epoint (progn (forward-line 1)(point))))
                    (if (<= limit (point)) (setq  ,cache nil))
                    (set-match-data (list bpoint epoint bpoint epoint))
                    t))
                 (t
                  ;; move forward one line at a time, checking each message
                  (while (and (= 0 (forward-line 1))
                              (> limit (point))
                              (not (member (mh-get-msg-num nil) ,cache))))
                  ;; Examine how we must have exited the loop...
                  (let ((cur-msg (mh-get-msg-num nil)))
                    (cond ((or (<= limit (point))
                               (not (member cur-msg ,cache)))
                           (setq ,cache nil)
                           nil)
                          ((member cur-msg ,cache)
                           (let ((bpoint (progn (beginning-of-line) (point)))
                                 (epoint (progn (forward-line 1) (point))))
                             (if (<= limit (point)) (setq ,cache nil))
                             (set-match-data
                              (list bpoint epoint bpoint epoint))
                             t))))))))

       (setq mh-folder-font-lock-keywords
             (append mh-folder-font-lock-keywords
                     (list (list ',func (list 1 '',face 'prepend t))))))))

(mh-generate-sequence-font-lock mh-unseen-seq unseen bold)
(mh-generate-sequence-font-lock mh-tick-seq tick mh-folder-tick)



;;; Internal variables:

(defvar mh-last-destination nil)        ;Destination of last refile or write
                                        ;command.
(defvar mh-last-destination-folder nil) ;Destination of last refile command.
(defvar mh-last-destination-write nil)  ;Destination of last write command.

(defvar mh-folder-mode-map (make-keymap)
  "Keymap for MH folders.")

(defvar mh-arrow-marker nil)            ;Marker for arrow display in fringe.

(defvar mh-delete-list nil)             ;List of msg numbers to delete.

(defvar mh-refile-list nil)             ;List of folder names in mh-seq-list.

(defvar mh-next-direction 'forward)     ;Direction to move to next message.

(defvar mh-view-ops ())                 ;Stack of ops that change the folder
                                        ;view (such as narrowing or threading).
(defvar mh-folder-view-stack ())        ;Stack of previous folder views.

(defvar mh-index-data nil)              ;Info about index search results
(defvar mh-index-previous-search nil)
(defvar mh-index-msg-checksum-map nil)
(defvar mh-index-checksum-origin-map nil)
(defvar mh-index-sequence-search-flag nil)

(defvar mh-first-msg-num nil)           ;Number of first msg in buffer.

(defvar mh-last-msg-num nil)            ;Number of last msg in buffer.

(defvar mh-mode-line-annotation nil)    ;Message range displayed in buffer.

(defvar mh-sequence-notation-history nil)
                                        ;Rememeber original notation that
                                        ;is overwritten by `mh-note-seq'.

(defvar mh-colors-available-flag nil)   ;Are colors available?

;;; Macros and generic functions:

(defun mh-mapc (function list)
  "Apply FUNCTION to each element of LIST for side effects only."
  (while list
    (funcall function (car list))
    (setq list (cdr list))))

(defun mh-scan-format ()
  "Return the output format argument for the scan program."
  (if (equal mh-scan-format-file t)
      (list "-format" (if (mh-variant-p 'nmh 'mu-mh)
                          (list (mh-update-scan-format
                                 mh-scan-format-nmh mh-cmd-note))
                        (list (mh-update-scan-format
                               mh-scan-format-mh mh-cmd-note))))
    (if (not (equal mh-scan-format-file nil))
        (list "-form" mh-scan-format-file))))



;;; Entry points:

;;;###autoload
(defun mh-rmail (&optional arg)
  "Inc(orporate) new mail with MH.
Scan an MH folder if ARG is non-nil. This function is an entry point to MH-E,
the Emacs interface to the MH mail system."
  (interactive "P")
  (mh-find-path)
  (if arg
      (call-interactively 'mh-visit-folder)
    (unless (get-buffer mh-inbox)
      (mh-visit-folder mh-inbox (symbol-name mh-unseen-seq)))
    (mh-inc-folder)))

;;;###autoload
(defun mh-nmail (&optional arg)
  "Check for new mail in inbox folder.
Scan an MH folder if ARG is non-nil. This function is an entry point to MH-E,
the Emacs interface to the MH mail system."
  (interactive "P")
  (mh-find-path)                        ; init mh-inbox
  (if arg
      (call-interactively 'mh-visit-folder)
    (mh-visit-folder mh-inbox)))



;;; User executable MH-E commands:

(defun mh-delete-msg (range)
  "Mark the specified RANGE for subsequent deletion and move to the next.
Default is the displayed message.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Delete")))
  (mh-delete-msg-no-motion range)
  (if (looking-at mh-scan-deleted-msg-regexp) (mh-next-msg)))

(defun mh-delete-msg-no-motion (range)
  "Mark the specified RANGE for subsequent deletion.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Delete")))
  (mh-iterate-on-range () range
    (mh-delete-a-msg nil)))

(defun mh-execute-commands ()
  "Process outstanding delete and refile requests."
  (interactive)
  (if mh-folder-view-stack (mh-widen t))
  (mh-process-commands mh-current-folder)
  (mh-set-scan-mode)
  (mh-goto-cur-msg)                    ; after mh-set-scan-mode for efficiency
  (mh-make-folder-mode-line)
  t)                                    ; return t for write-file-functions

(defun mh-first-msg ()
  "Move to the first message."
  (interactive)
  (goto-char (point-min))
  (while (and (not (eobp)) (not (looking-at mh-scan-valid-regexp)))
    (forward-line 1)))

(defun mh-header-display ()
  "Show the current message with all its headers.
Displays headers that might have been suppressed by setting the
variables `mh-clean-message-header-flag' or `mhl-formfile', or by the fallback
behavior of scrolling uninteresting headers off the top of the window.
Type \"\\[mh-show]\" to show the message normally again."
  (interactive)
  (and (not mh-showing-with-headers)
       (or mhl-formfile mh-clean-message-header-flag)
       (mh-invalidate-show-buffer))
  (let ((mh-decode-mime-flag nil)
        (mhl-formfile nil)
        (mh-clean-message-header-flag nil))
    (mh-show-msg nil)
    (mh-in-show-buffer (mh-show-buffer)
      (goto-char (point-min))
      (mh-recenter 0))
    (setq mh-showing-with-headers t)))

(defun mh-inc-folder (&optional maildrop-name folder)
  "Inc(orporate)s new mail into the Inbox folder.
Optional argument MAILDROP-NAME specifies an alternate maildrop from the
default. The optional argument FOLDER specifies where to incorporate mail
instead of the default named by `mh-inbox'.
The value of `mh-inc-folder-hook' is a list of functions to be called, with no
arguments, after incorporating new mail.
Do not call this function from outside MH-E; use \\[mh-rmail] instead."
  (interactive (list (if current-prefix-arg
                         (expand-file-name
                          (read-file-name "inc mail from file: "
                                          mh-user-path)))
                     (if current-prefix-arg
                         (mh-prompt-for-folder "inc mail into" mh-inbox t))))
  (if (not folder)
      (setq folder mh-inbox))
  (let ((threading-needed-flag nil))
    (let ((config (current-window-configuration)))
      (when (and mh-show-buffer (get-buffer mh-show-buffer))
        (delete-windows-on mh-show-buffer))
      (cond ((not (get-buffer folder))
             (mh-make-folder folder)
             (setq threading-needed-flag mh-show-threads-flag)
             (setq mh-previous-window-config config))
            ((not (eq (current-buffer) (get-buffer folder)))
             (switch-to-buffer folder)
             (setq mh-previous-window-config config))))
    (mh-get-new-mail maildrop-name)
    (when (and threading-needed-flag
               (save-excursion
                 (goto-char (point-min))
                 (or (null mh-large-folder)
                     (not (equal (forward-line (1+ mh-large-folder)) 0))
                     (and (message "Not threading since the number of messages exceeds `mh-large-folder'")
                          nil))))
      (mh-toggle-threads))
    (beginning-of-line)
    (if (and mh-showing-mode (looking-at mh-scan-valid-regexp)) (mh-show))
    (run-hooks 'mh-inc-folder-hook)))

(defun mh-last-msg ()
  "Move to the last message."
  (interactive)
  (goto-char (point-max))
  (while (and (not (bobp)) (not (looking-at mh-scan-valid-regexp)))
    (forward-line -1))
  (mh-recenter nil))

(defun mh-next-undeleted-msg (&optional arg wait-after-complaining-flag)
  "Move to the next undeleted message ARG in window.
If optional argument WAIT-AFTER-COMPLAINING-FLAG is non-nil and we are at the
last undeleted message then pause for a second after printing message."
  (interactive "p")
  (setq mh-next-direction 'forward)
  (forward-line 1)
  (cond ((re-search-forward mh-scan-good-msg-regexp nil t arg)
         (beginning-of-line)
         (mh-maybe-show))
        (t (forward-line -1)
           (message "No more undeleted messages")
           (if wait-after-complaining-flag (sit-for 1)))))

(defun mh-folder-from-address ()
  "Derive folder name from sender.

The name of the folder is derived as follows:

  a) The folder name associated with the first address found in the list
     `mh-default-folder-list' is used. Each element in this list contains a
     `Check Recipient' item. If this item is turned on, then the address is
     checked against the recipient instead of the sender. This is useful for
     mailing lists.

  b) An alias prefixed by `mh-default-folder-prefix' corresponding to the
     address is used. The prefix is used to prevent clutter in your mail
     directory.

Return nil if a folder name was not derived, or if the variable
`mh-default-folder-must-exist-flag' is t and the folder does not exist."
  ;; Loop for all entries in mh-default-folder-list
  (save-restriction
    (goto-char (point-min))
    (re-search-forward "\n\n" nil 'limit)
    (narrow-to-region (point-min) (point))
    (let ((to/cc (concat (or (message-fetch-field "to") "") ", "
                         (or (message-fetch-field "cc") "")))
          (from (or (message-fetch-field "from") ""))
          folder-name)
      (setq folder-name
            (loop for list in mh-default-folder-list
                  when (string-match (nth 0 list) (if (nth 2 list) to/cc from))
                  return (nth 1 list)
                  finally return nil))

      ;; Make sure a result from `mh-default-folder-list' begins with "+"
      ;; since 'mh-expand-file-name below depends on it
      (when (and folder-name (not (eq (aref folder-name 0) ?+)))
        (setq folder-name (concat "+" folder-name)))

      ;; If not, is there an alias for the address?
      (when (not folder-name)
        (let* ((from-header (mh-extract-from-header-value))
               (address (and from-header
                             (nth 1 (mail-extract-address-components
                                     from-header))))
               (alias (and address (mh-alias-address-to-alias address))))
          (when alias
            (setq folder-name
                  (and alias (concat "+" mh-default-folder-prefix alias))))))

      ;; If mh-default-folder-must-exist-flag set, check that folder exists.
      (if (and folder-name
               (or (not mh-default-folder-must-exist-flag)
                   (file-exists-p (mh-expand-file-name folder-name))))
          folder-name))))

(defun mh-prompt-for-refile-folder ()
  "Prompt the user for a folder in which the message should be filed.
The folder is returned as a string.

The default folder name is generated by the option
`mh-default-folder-for-message-function' if it is non-nil or
`mh-folder-from-address'."
  (mh-prompt-for-folder
   "Destination"
   (let ((refile-file (ignore-errors (mh-msg-filename (mh-get-msg-num t)))))
     (if (null refile-file) ""
       (save-excursion
         (set-buffer (get-buffer-create mh-temp-buffer))
         (erase-buffer)
         (insert-file-contents refile-file)
         (or (and mh-default-folder-for-message-function
                  (let ((buffer-file-name refile-file))
                    (funcall mh-default-folder-for-message-function)))
             (mh-folder-from-address)
             (and (eq 'refile (car mh-last-destination-folder))
                  (symbol-name (cdr mh-last-destination-folder)))
             ""))))
   t))

(defun mh-refile-msg (range folder &optional dont-update-last-destination-flag)
  "Refile RANGE into FOLDER.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

If optional argument DONT-UPDATE-LAST-DESTINATION-FLAG is non-nil then the
variables `mh-last-destination' and `mh-last-destination-folder' are not
updated."
  (interactive (list (mh-interactive-range "Refile")
                     (intern (mh-prompt-for-refile-folder))))
  (unless dont-update-last-destination-flag
    (setq mh-last-destination (cons 'refile folder)
          mh-last-destination-folder mh-last-destination))
  (mh-iterate-on-range () range
    (mh-refile-a-msg nil folder))
  (when (looking-at mh-scan-refiled-msg-regexp) (mh-next-msg)))

(defun mh-refile-or-write-again (range &optional interactive-flag)
  "Re-execute the last refile or write command on the given RANGE.
Default is the displayed message. Use the same folder or file as the previous
refile or write command.
If INTERACTIVE-FLAG is non-nil then the function was called interactively."
  (interactive (list (mh-interactive-range "Redo") t))
  (if (null mh-last-destination)
      (error "No previous refile or write"))
  (let (output)
    (setq output
          (cond ((eq (car mh-last-destination) 'refile)
                 (mh-refile-msg range (cdr mh-last-destination))
                 (format "Destination folder: %s" (cdr mh-last-destination)))
                (t
                 (mh-iterate-on-range msg range
                   (apply 'mh-write-msg-to-file msg (cdr mh-last-destination)))
                 (mh-next-msg interactive-flag)
                 (format "Destination: %s" (cdr mh-last-destination)))))
    (message "%s" output)))

(defun mh-quit ()
  "Quit the current MH-E folder.
Restore the previous window configuration, if one exists.
The value of `mh-before-quit-hook' is a list of functions to be called, with
no arguments, immediately upon entry to this function.
The value of `mh-quit-hook' is a list of functions to be called, with no
arguments, upon exit of this function.
MH-E working buffers (whose name begins with \" *mh-\" or \"*MH-E \") are
killed."
  (interactive)
  (run-hooks 'mh-before-quit-hook)
  (let ((show-buffer (get-buffer mh-show-buffer)))
    (when show-buffer
      (kill-buffer show-buffer)))
  (mh-update-sequences)
  (mh-destroy-postponed-handles)
  (bury-buffer (current-buffer))

  ;; Delete all MH-E temporary and working buffers.
  (dolist (buffer (buffer-list))
    (when (or (string-match "^ \\*mh-" (buffer-name buffer))
              (string-match "^\\*MH-E " (buffer-name buffer)))
      (kill-buffer buffer)))

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
      (if mh-page-to-next-msg-flag
          (if (equal mh-next-direction 'backward)
              (mh-previous-undeleted-msg)
            (mh-next-undeleted-msg))
        (if (mh-in-show-buffer (mh-show-buffer)
              (pos-visible-in-window-p (point-max)))
            (progn
              (message
               "End of message (Type %s to read %s undeleted message)"
               (single-key-description last-input-event)
               (if (equal mh-next-direction 'backward)
                   "previous"
                 "next"))
              (setq mh-page-to-next-msg-flag t))
          (scroll-other-window arg)))
    (mh-show)))

(defun mh-previous-page (&optional arg)
  "Page the displayed message backwards.
Scrolls ARG lines or a full screen if no argument is supplied."
  (interactive "P")
  (mh-in-show-buffer (mh-show-buffer)
    (scroll-down arg)))

(defun mh-previous-undeleted-msg (&optional arg wait-after-complaining-flag)
  "Move to the previous undeleted message ARG in window.
If optional argument WAIT-AFTER-COMPLAINING-FLAG is non-nil and we are at the
first undeleted message then pause for a second after printing message."
  (interactive "p")
  (setq mh-next-direction 'backward)
  (beginning-of-line)
  (cond ((re-search-backward mh-scan-good-msg-regexp nil t arg)
         (mh-maybe-show))
        (t (message "No previous undeleted message")
           (if wait-after-complaining-flag (sit-for 1)))))

(defun mh-previous-unread-msg (&optional count)
  "Move to previous unread message.
With optional argument COUNT, COUNT-1 unread messages before current message
are skipped."
  (interactive "p")
  (unless (> count 0)
    (error "The function mh-previous-unread-msg expects positive argument"))
  (setq count (1- count))
  (let ((unread-sequence (cdr (assoc mh-unseen-seq mh-seq-list)))
        (cur-msg (mh-get-msg-num nil)))
    (cond ((and (not cur-msg) (not (bobp))
                ;; If we are at the end of the buffer back up one line and go
                ;; to unread message after that.
                (progn
                  (forward-line -1)
                  (setq cur-msg (mh-get-msg-num nil)))
                nil))
          ((or (null unread-sequence) (not cur-msg))
           ;; No unread message or there aren't any messages in buffer...
           (message "No more unread messages"))
          ((progn
             ;; Skip count messages...
             (while (and unread-sequence (>= (car unread-sequence) cur-msg))
               (setq unread-sequence (cdr unread-sequence)))
             (while (> count 0)
               (setq unread-sequence (cdr unread-sequence))
               (setq count (1- count)))
             (not (car unread-sequence)))
           (message "No more unread messages"))
          (t (loop for msg in unread-sequence
                   when (mh-goto-msg msg t) return nil
                   finally (message "No more unread messages"))))))

(defun mh-goto-next-button (backward-flag &optional criterion)
  "Search for next button satisfying criterion.
If BACKWARD-FLAG is non-nil search backward in the buffer for a mime button. If
CRITERION is a function or a symbol which has a function binding then that
function must return non-nil at the button we stop."
  (unless (or (and (symbolp criterion) (fboundp criterion))
              (functionp criterion))
    (setq criterion (lambda (x) t)))
  ;; Move to the next button in the buffer satisfying criterion
  (goto-char (or (save-excursion
                   (beginning-of-line)
                   ;; Find point before current button
                   (let ((point-before-current-button
                          (save-excursion
                            (while (get-text-property (point) 'mh-data)
                              (unless (= (forward-line
                                          (if backward-flag 1 -1))
                                         0)
                                (if backward-flag
                                    (goto-char (point-min))
                                  (goto-char (point-max)))))
                            (point))))
                     ;; Skip over current button
                     (while (and (get-text-property (point) 'mh-data)
                                 (not (if backward-flag (bobp) (eobp))))
                       (forward-line (if backward-flag -1 1)))
                     ;; Stop at next MIME button if any exists.
                     (block loop
                       (while (/= (progn
                                    (unless (= (forward-line
                                                (if backward-flag -1 1))
                                               0)
                                      (if backward-flag
                                          (goto-char (point-max))
                                        (goto-char (point-min)))
                                      (beginning-of-line))
                                    (point))
                                  point-before-current-button)
                         (when (and (get-text-property (point) 'mh-data)
                                    (funcall criterion (point)))
                           (return-from loop (point))))
                       nil)))
                 (point))))

(defun mh-next-button (&optional backward-flag)
  "Go to the next MIME button.
Advance point to the next MIME button in the show buffer. If the end
of buffer is reached then the search wraps over to the start of the
buffer. With prefix argument, BACKWARD-FLAG the point will move to the
previous MIME button."
  (interactive (list current-prefix-arg))
  (unless mh-showing-mode
    (mh-show))
  (mh-in-show-buffer (mh-show-buffer)
    (mh-goto-next-button backward-flag)))

(defun mh-prev-button ()
  "Go to the prev MIME button.
Move point to the previous MIME button in the show buffer. If the beginning
of the buffer is reached then the search wraps over to the end of the
buffer."
  (interactive)
  (mh-next-button t))

(defun mh-folder-mime-action (part-index action include-security-flag)
  "Go to PART-INDEX and carry out ACTION.
If PART-INDEX is nil then go to the next part in the buffer. The search for
the next buffer wraps around if end of buffer is reached. If argument
INCLUDE-SECURITY-FLAG is non-nil then include security info buttons when
searching for a suitable parts."
  (unless mh-showing-mode
    (mh-show))
  (mh-in-show-buffer (mh-show-buffer)
    (let ((criterion
           (cond (part-index
                  (lambda (p)
                    (let ((part (get-text-property p 'mh-part)))
                      (and (integerp part) (= part part-index)))))
                 (t (lambda (p)
                      (if include-security-flag
                          (get-text-property p 'mh-data)
                        (integerp (get-text-property p 'mh-part)))))))
          (point (point)))
      (cond ((and (get-text-property point 'mh-part)
                  (or (null part-index)
                      (= (get-text-property point 'mh-part) part-index)))
             (funcall action))
            ((and (get-text-property point 'mh-data)
                  include-security-flag
                  (null part-index))
             (funcall action))
            (t
             (mh-goto-next-button nil criterion)
             (if (= (point) point)
                 (message "No matching MIME part found")
               (funcall action)))))))

(defun mh-folder-toggle-mime-part (part-index)
  "Toggle display of button.
If point in show buffer is at a button then that part is toggled.
If not at a button and PART-INDEX is non-nil point is moved to that part.
With nil PART-INDEX find the first button after point (search wraps around if
end of buffer is reached) and toggle it."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action part-index #'mh-press-button t))

(defun mh-folder-inline-mime-part (part-index)
  "Show the raw bytes of MIME part inline.
If point in show buffer is at a mime part then that part is inlined.
If not at a mime-part and PART-INDEX is non-nil point is moved to that part.
With nil PART-INDEX find the first button after point (search wraps around if
end of buffer is reached) and inline it."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action part-index #'mh-mime-inline-part nil))

(defun mh-folder-save-mime-part (part-index)
  "Save MIME part.
If point in show buffer is at a mime part then that part is saved.
If not at a mime-part and PART-INDEX is non-nil point is moved to that part.
With nil PART-INDEX find the first button after point (search wraps around if
end of buffer is reached) and save it."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action part-index #'mh-mime-save-part nil))

(defvar mh-thread-scan-line-map-stack)

(defun mh-reset-threads-and-narrowing ()
  "Reset all variables pertaining to threads and narrowing.
Also removes all content from the folder buffer."
  (setq mh-view-ops ())
  (setq mh-folder-view-stack ())
  (setq mh-thread-scan-line-map-stack ())
  (let ((buffer-read-only nil)) (erase-buffer)))

(defun mh-rescan-folder (&optional range dont-exec-pending)
  "Rescan a folder after optionally processing the outstanding commands.
If optional prefix argument RANGE is provided, prompt for the range of
messages to display. Otherwise show the entire folder.
If optional argument DONT-EXEC-PENDING is non-nil then pending deletes and
refiles aren't carried out."
  (interactive (list (if current-prefix-arg
                         (mh-read-range "Rescan" mh-current-folder t nil t
                                        mh-interpret-number-as-range-flag)
                       nil)))
  (setq mh-next-direction 'forward)
  (let ((threaded-flag (memq 'unthread mh-view-ops)))
    (mh-scan-folder mh-current-folder (or range "all") dont-exec-pending)
    (cond (threaded-flag (mh-toggle-threads))
          (mh-index-data (mh-index-insert-folder-headers)))))

(defun mh-write-msg-to-file (msg file no-headers)
  "Append MSG to the end of a FILE.
If prefix argument NO-HEADERS is provided, write only the message body.
Otherwise send the entire message including the headers."
  (interactive
   (list (mh-get-msg-num t)
         (let ((default-dir (if (eq 'write (car mh-last-destination-write))
                                (file-name-directory
                                 (car (cdr mh-last-destination-write)))
                              default-directory)))
           (read-file-name (format "Save message%s in file: "
                                   (if current-prefix-arg " body" ""))
                           default-dir
                           (if (eq 'write (car mh-last-destination-write))
                               (car (cdr mh-last-destination-write))
                             (expand-file-name "mail.out" default-dir))))
         current-prefix-arg))
  (let ((msg-file-to-output (mh-msg-filename msg))
        (output-file (mh-expand-file-name file)))
    (setq mh-last-destination (list 'write file (if no-headers 'no-headers))
          mh-last-destination-write mh-last-destination)
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

(defun mh-undo (range)
  "Undo the pending deletion or refile of the specified RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Undo")))
  (cond ((numberp range)
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
        (t (mh-iterate-on-range () range
             (mh-undo-msg nil))))
  (if (not (mh-outstanding-commands-p))
      (mh-set-folder-modified-p nil)))


(defun mh-folder-line-matches-show-buffer-p ()
  "Return t if the message under point in folder-mode is in the show buffer.
Return nil in any other circumstance (no message under point, no show buffer,
the message in the show buffer doesn't match."
  (and (eq major-mode 'mh-folder-mode)
       (mh-get-msg-num nil)
       mh-show-buffer
       (get-buffer mh-show-buffer)
       (buffer-file-name (get-buffer mh-show-buffer))
       (string-match ".*/\\([0-9]+\\)$"
                     (buffer-file-name (get-buffer mh-show-buffer)))
       (string-equal
        (match-string 1 (buffer-file-name (get-buffer mh-show-buffer)))
        (int-to-string (mh-get-msg-num nil)))))

(eval-when-compile (require 'gnus))

(defmacro mh-macro-expansion-time-gnus-version ()
  "Return Gnus version available at macro expansion time.
The macro evaluates the Gnus version at macro expansion time. If MH-E was
compiled then macro expansion happens at compile time."
  gnus-version)

(defun mh-run-time-gnus-version ()
  "Return Gnus version available at run time."
  (require 'gnus)
  gnus-version)

;;;###autoload
(defun mh-version ()
  "Display version information about MH-E and the MH mail handling system."
  (interactive)
  (set-buffer (get-buffer-create mh-info-buffer))
  (erase-buffer)
  ;; MH-E version.
  (insert "MH-E " mh-version "\n\n")
  ;; MH-E compilation details.
  (insert "MH-E compilation details:\n")
  (let* ((compiled-mhe (byte-code-function-p (symbol-function 'mh-version)))
         (gnus-compiled-version (if compiled-mhe
                                    (mh-macro-expansion-time-gnus-version)
                                  "N/A")))
    (insert " Byte compiled:\t\t" (if compiled-mhe "yes" "no") "\n"
            " Gnus (compile-time):\t" gnus-compiled-version "\n"
            " Gnus (run-time):\t" (mh-run-time-gnus-version) "\n\n"))
  ;; Emacs version.
  (insert (emacs-version) "\n\n")
  ;; MH version.
  (if mh-variant-in-use
      (insert mh-variant-in-use "\n"
              " mh-progs:\t" mh-progs "\n"
              " mh-lib:\t" mh-lib "\n"
              " mh-lib-progs:\t" mh-lib-progs "\n\n")
    (insert "No MH variant detected\n"))
  ;; Linux version.
  (condition-case ()
      (call-process "uname" nil t nil "-a")
    (file-error))
  (goto-char (point-min))
  (display-buffer mh-info-buffer))

(defun mh-parse-flist-output-line (line &optional current-folder)
  "Parse LINE to generate folder name, unseen messages and total messages.
If CURRENT-FOLDER is non-nil then it contains the current folder name and it is
used to avoid problems in corner cases involving folders whose names end with a
'+' character."
  (with-temp-buffer
    (insert line)
    (goto-char (point-max))
    (let (folder unseen total p)
      (when (search-backward " out of " (point-min) t)
        (setq total (read-from-string
                     (buffer-substring-no-properties
                      (match-end 0) (line-end-position))))
        (when (search-backward " in sequence " (point-min) t)
          (setq p (point))
          (when (search-backward " has " (point-min) t)
            (setq unseen (read-from-string (buffer-substring-no-properties
                                            (match-end 0) p)))
            (while (eq (char-after) ? )
              (backward-char))
            (setq folder (buffer-substring-no-properties
                          (point-min) (1+ (point))))
            (when (and (equal (aref folder (1- (length folder))) ?+)
                       (equal current-folder folder))
              (setq folder (substring folder 0 (1- (length folder)))))
            (values (format "+%s" folder) (car unseen) (car total))))))))

(defun mh-folder-size-folder (folder)
  "Find size of FOLDER using `folder'."
  (with-temp-buffer
    (let ((u (length (cdr (assoc mh-unseen-seq
                                 (mh-read-folder-sequences folder nil))))))
      (call-process (expand-file-name "folder" mh-progs) nil t nil
                    "-norecurse" folder)
      (goto-char (point-min))
      (if (re-search-forward " has \\([0-9]+\\) " nil t)
          (values (car (read-from-string (match-string 1))) u folder)
        (values 0 u folder)))))

(defun mh-folder-size-flist (folder)
  "Find size of FOLDER using `flist'."
  (with-temp-buffer
    (call-process (expand-file-name "flist" mh-progs) nil t nil "-showzero"
                  "-norecurse" folder "-sequence" (symbol-name mh-unseen-seq))
    (goto-char (point-min))
    (multiple-value-bind (folder unseen total)
        (mh-parse-flist-output-line
         (buffer-substring (point) (line-end-position)))
      (values total unseen folder))))

(defun mh-folder-size (folder)
  "Find size of FOLDER."
  (if mh-flists-present-flag
      (mh-folder-size-flist folder)
    (mh-folder-size-folder folder)))

(defun mh-visit-folder (folder &optional range index-data)
  "Visit FOLDER and display RANGE of messages.
Do not call this function from outside MH-E; see \\[mh-rmail] instead.

If RANGE is nil (the default if it is omitted when called non-interactively),
then all messages in FOLDER are displayed.

If an index buffer is being created then INDEX-DATA is used to initialize the
index buffer specific data structures.

A prefix argument will cause a prompt for the RANGE of messages
regardless of the size of the `mh-large-folder' variable."
  (interactive (let ((folder-name (mh-prompt-for-folder "Visit" mh-inbox t)))
                 (list folder-name
                       (mh-read-range "Scan" folder-name t nil
                                      current-prefix-arg
                                      mh-interpret-number-as-range-flag))))
  (let ((config (current-window-configuration))
        (current-buffer (current-buffer))
        (threaded-view-flag mh-show-threads-flag))
    (delete-other-windows)
    (save-excursion
      (when (get-buffer folder)
        (set-buffer folder)
        (setq threaded-view-flag (memq 'unthread mh-view-ops))))
    (when index-data
      (mh-make-folder folder)
      (setq mh-index-data (car index-data)
            mh-index-msg-checksum-map (make-hash-table :test #'equal)
            mh-index-checksum-origin-map (make-hash-table :test #'equal))
      (mh-index-update-maps folder (cadr index-data))
      (mh-index-create-sequences))
    (mh-scan-folder folder (or range "all"))
    (cond ((and threaded-view-flag
                (save-excursion
                  (goto-char (point-min))
                  (or (null mh-large-folder)
                      (not (equal (forward-line (1+ mh-large-folder)) 0))
                      (and (message "Not threading since the number of messages exceeds `mh-large-folder'")
                           nil))))
           (mh-toggle-threads))
          (mh-index-data
           (mh-index-insert-folder-headers)))
    (unless (eq current-buffer (current-buffer))
      (setq mh-previous-window-config config)))
  nil)


(defun mh-update-sequences ()
  "Update MH's Unseen-Sequence and current folder and message.
Flush MH-E's state out to MH. The message at the cursor becomes current."
  (interactive)
  ;; mh-update-sequences is the opposite of mh-read-folder-sequences,
  ;; which updates MH-E's state from MH.
  (let ((folder-set (mh-update-unseen))
        (new-cur (mh-get-msg-num nil)))
    (if new-cur
        (let ((seq-entry (mh-find-seq 'cur)))
          (mh-remove-cur-notation)
          (setcdr seq-entry
                  (list new-cur))       ;delete-seq-locally, add-msgs-to-seq
          (mh-define-sequence 'cur (list new-cur))
          (beginning-of-line)
          (if (looking-at mh-scan-good-msg-regexp)
              (mh-notate-cur)))
      (or folder-set
          (save-excursion
            ;; psg - mh-current-folder is nil if mh-summary-height < 4 !
            ;;       So I added this sanity check.
            (if (stringp mh-current-folder)
                (mh-exec-cmd-quiet t "folder" mh-current-folder "-fast")
              (mh-exec-cmd-quiet t "folder" "-fast")))))))



;;; Support routines.

(defun mh-delete-a-msg (msg)
  "Delete the MSG.
If MSG is nil then the message at point is deleted.

The value of `mh-delete-msg-hook' is a list of functions to be called, with no
arguments, after the message has been deleted."
  (save-excursion
    (if (numberp msg)
        (mh-goto-msg msg nil t)
      (beginning-of-line)
      (setq msg (mh-get-msg-num t)))
    (if (looking-at mh-scan-refiled-msg-regexp)
        (error "Message %d is refiled.  Undo refile before deleting" msg))
    (if (looking-at mh-scan-deleted-msg-regexp)
        nil
      (mh-set-folder-modified-p t)
      (setq mh-delete-list (cons msg mh-delete-list))
      (mh-notate nil mh-note-deleted mh-cmd-note)
      (run-hooks 'mh-delete-msg-hook))))

(defun mh-refile-a-msg (msg folder)
  "Refile MSG in FOLDER.
If MSG is nil then the message at point is refiled.

Folder is a symbol, not a string.
The value of `mh-refile-msg-hook' is a list of functions to be called, with no
arguments, after the message has been refiled."
  (save-excursion
    (if (numberp msg)
        (mh-goto-msg msg nil t)
      (beginning-of-line)
      (setq msg (mh-get-msg-num t)))
    (cond ((looking-at mh-scan-deleted-msg-regexp)
           (error "Message %d is deleted.  Undo delete before moving" msg))
          ((looking-at mh-scan-refiled-msg-regexp)
           (if (y-or-n-p
                (format "Message %d already refiled.  Copy to %s as well? "
                        msg folder))
               (mh-exec-cmd "refile" (mh-get-msg-num t) "-link"
                            "-src" mh-current-folder
                            (symbol-name folder))
             (message "Message not copied")))
          (t
           (mh-set-folder-modified-p t)
           (cond ((null (assoc folder mh-refile-list))
                  (push (list folder msg) mh-refile-list))
                 ((not (member msg (cdr (assoc folder mh-refile-list))))
                  (push msg (cdr (assoc folder mh-refile-list)))))
           (mh-notate nil mh-note-refiled mh-cmd-note)
           (run-hooks 'mh-refile-msg-hook)))))

(defun mh-next-msg (&optional wait-after-complaining-flag)
  "Move backward or forward to the next undeleted message in the buffer.
If optional argument WAIT-AFTER-COMPLAINING-FLAG is non-nil and we are at the
last message, then wait for a second after telling the user that there aren't
any more unread messages."
  (if (eq mh-next-direction 'forward)
      (mh-next-undeleted-msg 1 wait-after-complaining-flag)
    (mh-previous-undeleted-msg 1 wait-after-complaining-flag)))

(defun mh-next-unread-msg (&optional count)
  "Move to next unread message.
With optional argument COUNT, COUNT-1 unread messages are skipped."
  (interactive "p")
  (unless (> count 0)
    (error "The function mh-next-unread-msg expects positive argument"))
  (setq count (1- count))
  (let ((unread-sequence (reverse (cdr (assoc mh-unseen-seq mh-seq-list))))
        (cur-msg (mh-get-msg-num nil)))
    (cond ((and (not cur-msg) (not (bobp))
                ;; If we are at the end of the buffer back up one line and go
                ;; to unread message after that.
                (progn
                  (forward-line -1)
                  (setq cur-msg (mh-get-msg-num nil)))
                nil))
          ((or (null unread-sequence) (not cur-msg))
           ;; No unread message or there aren't any messages in buffer...
           (message "No more unread messages"))
          ((progn
             ;; Skip messages
             (while (and unread-sequence (>= cur-msg (car unread-sequence)))
               (setq unread-sequence (cdr unread-sequence)))
             (while (> count 0)
               (setq unread-sequence (cdr unread-sequence))
               (setq count (1- count)))
             (not (car unread-sequence)))
           (message "No more unread messages"))
          (t (loop for msg in unread-sequence
                   when (mh-goto-msg msg t) return nil
                   finally (message "No more unread messages"))))))

(defun mh-set-scan-mode ()
  "Display the scan listing buffer, but do not show a message."
  (if (get-buffer mh-show-buffer)
      (delete-windows-on mh-show-buffer))
  (mh-showing-mode 0)
  (force-mode-line-update)
  (if mh-recenter-summary-flag
      (mh-recenter nil)))

(defun mh-undo-msg (msg)
  "Undo the deletion or refile of one MSG.
If MSG is nil then act on the message at point"
  (save-excursion
    (if (numberp msg)
        (mh-goto-msg msg t t)
      (beginning-of-line)
      (setq msg (mh-get-msg-num t)))
    (cond ((memq msg mh-delete-list)
           (setq mh-delete-list (delq msg mh-delete-list)))
          (t
           (dolist (folder-msg-list mh-refile-list)
             (setf (cdr folder-msg-list) (remove msg (cdr folder-msg-list))))
           (setq mh-refile-list (loop for x in mh-refile-list
                                      unless (null (cdr x)) collect x))))
    (mh-notate nil ?  mh-cmd-note)))



;;; The folder data abstraction.

(defvar mh-index-data-file ".mhe_index"
  "MH-E specific file where index seach info is stored.")

(defun mh-make-folder (name)
  "Create a new mail folder called NAME.
Make it the current folder."
  (switch-to-buffer name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (if mh-adaptive-cmd-note-flag
      (mh-set-cmd-note (mh-message-number-width name)))
  (setq buffer-read-only t)
  (mh-folder-mode)
  (mh-set-folder-modified-p nil)
  (setq buffer-file-name mh-folder-filename)
  (when (and (not mh-index-data)
             (file-exists-p (concat buffer-file-name mh-index-data-file)))
    (mh-index-read-data))
  (mh-make-folder-mode-line))

;;; Ensure new buffers won't get this mode if default-major-mode is nil.
(put 'mh-folder-mode 'mode-class 'special)



;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
;;; Menus for folder mode: folder, message, sequence (in that order)
;;; folder-mode "Sequence" menu
(easy-menu-define
  mh-folder-sequence-menu mh-folder-mode-map "Menu for MH-E folder-sequence."
  '("Sequence"
    ["Add Message to Sequence..."       mh-put-msg-in-seq (mh-get-msg-num nil)]
    ["List Sequences for Message"       mh-msg-is-in-seq (mh-get-msg-num nil)]
    ["Delete Message from Sequence..."  mh-delete-msg-from-seq
     (mh-get-msg-num nil)]
    ["List Sequences in Folder..."      mh-list-sequences t]
    ["Delete Sequence..."               mh-delete-seq t]
    ["Narrow to Sequence..."            mh-narrow-to-seq t]
    ["Widen from Sequence"              mh-widen mh-folder-view-stack]
    "--"
    ["Narrow to Subject Sequence"       mh-narrow-to-subject t]
    ["Narrow to Tick Sequence"          mh-narrow-to-tick
     (and mh-tick-seq (mh-seq-msgs (mh-find-seq mh-tick-seq)))]
    ["Delete Rest of Same Subject"      mh-delete-subject t]
    ["Toggle Tick Mark"                 mh-toggle-tick t]
    "--"
    ["Push State Out to MH"             mh-update-sequences t]))

;;; folder-mode "Message" menu
(easy-menu-define
  mh-folder-message-menu mh-folder-mode-map "Menu for MH-E folder-message."
  '("Message"
    ["Show Message"                     mh-show (mh-get-msg-num nil)]
    ["Show Message with Header"         mh-header-display (mh-get-msg-num nil)]
    ["Next Message"                     mh-next-undeleted-msg t]
    ["Previous Message"                 mh-previous-undeleted-msg t]
    ["Go to First Message"              mh-first-msg t]
    ["Go to Last Message"               mh-last-msg t]
    ["Go to Message by Number..."       mh-goto-msg t]
    ["Modify Message"                   mh-modify t]
    ["Delete Message"                   mh-delete-msg (mh-get-msg-num nil)]
    ["Refile Message"                   mh-refile-msg (mh-get-msg-num nil)]
    ["Undo Delete/Refile"               mh-undo (mh-outstanding-commands-p)]
    ["Execute Delete/Refile"            mh-execute-commands
     (mh-outstanding-commands-p)]
    "--"
    ["Compose a New Message"            mh-send t]
    ["Reply to Message..."              mh-reply (mh-get-msg-num nil)]
    ["Forward Message..."               mh-forward (mh-get-msg-num nil)]
    ["Redistribute Message..."          mh-redistribute (mh-get-msg-num nil)]
    ["Edit Message Again"               mh-edit-again (mh-get-msg-num nil)]
    ["Re-edit a Bounced Message"        mh-extract-rejected-mail t]
    "--"
    ["Copy Message to Folder..."        mh-copy-msg (mh-get-msg-num nil)]
    ["Print Message"                    mh-print-msg (mh-get-msg-num nil)]
    ["Write Message to File..."         mh-write-msg-to-file
     (mh-get-msg-num nil)]
    ["Pipe Message to Command..."       mh-pipe-msg (mh-get-msg-num nil)]
    ["Unpack Uuencoded Message..."      mh-store-msg (mh-get-msg-num nil)]
    ["Burst Digest Message"             mh-burst-digest (mh-get-msg-num nil)]))

;;; folder-mode "Folder" menu
(easy-menu-define
  mh-folder-folder-menu mh-folder-mode-map  "Menu for MH-E folder."
  '("Folder"
    ["Incorporate New Mail"             mh-inc-folder t]
    ["Toggle Show/Folder"               mh-toggle-showing t]
    ["Execute Delete/Refile"            mh-execute-commands
     (mh-outstanding-commands-p)]
    ["Rescan Folder"                    mh-rescan-folder t]
    ["Thread Folder"                    mh-toggle-threads
     (not (memq 'unthread mh-view-ops))]
    ["Pack Folder"                      mh-pack-folder t]
    ["Sort Folder"                      mh-sort-folder t]
    "--"
    ["List Folders"                     mh-list-folders t]
    ["Visit a Folder..."                mh-visit-folder t]
    ["View New Messages"                mh-index-new-messages t]
    ["Search a Folder..."               mh-search-folder t]
    ["Indexed Search..."                mh-index-search t]
    "--"
    ["Quit MH-E"                        mh-quit t]))



(defmacro mh-remove-xemacs-horizontal-scrollbar ()
  "Get rid of the horizontal scrollbar that XEmacs insists on putting in."
  (when mh-xemacs-flag
    `(if (and (featurep 'scrollbar)
              (fboundp 'set-specifier))
         (set-specifier horizontal-scrollbar-visible-p nil
                        (cons (current-buffer) nil)))))

(defmacro mh-write-file-functions-compat ()
  "Return `write-file-functions' if it exists.
Otherwise return `local-write-file-hooks'. This macro exists purely for
compatibility. The former symbol is used in Emacs 21.4 onward while the latter
is used in previous versions and XEmacs."
  (if (boundp 'write-file-functions)
      ''write-file-functions            ;Emacs 21.4
    ''local-write-file-hooks))          ;<Emacs 21.4, XEmacs

;; Avoid compiler warnings in non-bleeding edge versions of Emacs.
(eval-when-compile
  (defvar tool-bar-mode)
  (defvar tool-bar-map)
  (defvar desktop-save-buffer))         ;Emacs 21.4

;; Register mh-folder-mode as supporting which-function-mode...
(load "which-func" t t)
(when (and (boundp 'which-func-modes)
           (not (member 'mh-folder-mode which-func-modes)))
  (push 'mh-folder-mode which-func-modes))

(defvar mh-folder-buttons-init-flag nil)

;; Autoload cookie needed by desktop.el
;;;###autoload
(define-derived-mode mh-folder-mode fundamental-mode "MH-Folder"
  "Major MH-E mode for \"editing\" an MH folder scan listing.\\<mh-folder-mode-map>

You can show the message the cursor is pointing to, and step through the
messages.  Messages can be marked for deletion or refiling into another
folder; these commands are executed all at once with a separate command.

Options that control this mode can be changed with \\[customize-group];
specify the \"mh\" group. In particular, please see the `mh-scan-format-file'
option if you wish to modify scan's format.

When a folder is visited, the hook `mh-folder-mode-hook' is run.

Ranges
======
Many commands that operate on individual messages, such as `mh-forward' or
`mh-refile-msg' take a RANGE argument. This argument can be used in several
ways.

If you provide the prefix argument (\\[universal-argument]) to these commands,
then you will be prompted for the message range. This can be any valid MH
range which can include messages, sequences, and the abbreviations (described
in the mh(1) man page):

<num1>-<num2>
    Indicates all messages in the range <num1> to <num2>, inclusive. The range
    must be nonempty.

`<num>:N'
`<num>:+N'
`<num>:-N'
    Up to N messages beginning with (or ending with) message num. Num may be
    any of the pre-defined symbols: first, prev, cur, next or last.

`first:N'
`prev:N'
`next:N'
`last:N'
    The first, previous, next or last messages, if they exist.

`all'
    All of the messages.

For example, a range that shows all of these things is `1 2 3 5-10 last:5
unseen'.

If the option `transient-mark-mode' is set to t and you set a region in the
MH-Folder buffer, then the MH-E command will perform the operation on all
messages in that region.

\\{mh-folder-mode-map}"
  (mh-do-in-gnu-emacs
   (unless mh-folder-buttons-init-flag
     (mh-tool-bar-folder-buttons-init)
     (setq mh-folder-buttons-init-flag t)))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mh-folder-font-lock-keywords t))
  (make-local-variable 'desktop-save-buffer)
  (setq desktop-save-buffer t)
  (mh-make-local-vars
   'mh-colors-available-flag (mh-colors-available-p)
                                        ; Do we have colors available
   'mh-current-folder (buffer-name)     ; Name of folder, a string
   'mh-show-buffer (format "show-%s" (buffer-name)) ; Buffer that displays msgs
   'mh-folder-filename                  ; e.g. "/usr/foobar/Mail/inbox/"
   (file-name-as-directory (mh-expand-file-name (buffer-name)))
   'mh-display-buttons-for-inline-parts-flag
   mh-display-buttons-for-inline-parts-flag ; Allow for display of buttons to
                                        ; be  toggled.
   'mh-arrow-marker (make-marker)       ; Marker where arrow is displayed
   'overlay-arrow-position nil          ; Allow for simultaneous display in
   'overlay-arrow-string ">"            ;  different MH-E buffers.
   'mh-showing-mode nil                 ; Show message also?
   'mh-delete-list nil                  ; List of msgs nums to delete
   'mh-refile-list nil                  ; List of folder names in mh-seq-list
   'mh-seq-list nil                     ; Alist of (seq . msgs) nums
   'mh-seen-list nil                    ; List of displayed messages
   'mh-next-direction 'forward          ; Direction to move to next message
   'mh-view-ops ()                      ; Stack that keeps track of the order
                                        ; in which narrowing/threading has been
                                        ; carried out.
   'mh-folder-view-stack ()             ; Stack of previous views of the
                                        ; folder.
   'mh-index-data nil                   ; If the folder was created by a call
                                        ; to mh-index-search this contains info
                                        ; about the search results.
   'mh-index-previous-search nil        ; Previous folder and search-regexp
   'mh-index-msg-checksum-map nil       ; msg -> checksum map
   'mh-index-checksum-origin-map nil    ; checksum -> ( orig-folder, orig-msg )
   'mh-index-sequence-search-flag nil   ; folder resulted from sequence search
   'mh-first-msg-num nil                ; Number of first msg in buffer
   'mh-last-msg-num nil                 ; Number of last msg in buffer
   'mh-msg-count nil                    ; Number of msgs in buffer
   'mh-mode-line-annotation nil         ; Indicates message range
   'mh-sequence-notation-history (make-hash-table)
                                        ; Remember what is overwritten by
                                        ; mh-note-seq.
   'imenu-create-index-function 'mh-index-create-imenu-index
                                        ; Setup imenu support
   'mh-previous-window-config nil)      ; Previous window configuration
  (mh-remove-xemacs-horizontal-scrollbar)
  (setq truncate-lines t)
  (auto-save-mode -1)
  (setq buffer-offer-save t)
  (mh-make-local-hook (mh-write-file-functions-compat))
  (add-hook (mh-write-file-functions-compat) 'mh-execute-commands nil t)
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'hl-line-mode)   ; avoid pollution
  (mh-funcall-if-exists hl-line-mode 1)
  (setq revert-buffer-function 'mh-undo-folder)
  (or (assq 'mh-showing-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(mh-showing-mode " Show") minor-mode-alist)))
  (easy-menu-add mh-folder-sequence-menu)
  (easy-menu-add mh-folder-message-menu)
  (easy-menu-add mh-folder-folder-menu)
  (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map)
  (mh-funcall-if-exists mh-toolbar-init :folder)
  (if (and mh-xemacs-flag
           font-lock-auto-fontify)
      (turn-on-font-lock)))             ; Force font-lock in XEmacs.

(defun mh-toggle-mime-buttons ()
  "Toggle display of buttons for inline MIME parts."
  (interactive)
  (setq mh-display-buttons-for-inline-parts-flag
        (not mh-display-buttons-for-inline-parts-flag))
  (mh-show nil t))

(defun mh-colors-available-p ()
  "Check if colors are available in the Emacs being used."
  (or mh-xemacs-flag
      (let ((color-cells
             (or (ignore-errors (mh-funcall-if-exists display-color-cells))
                 (ignore-errors (mh-funcall-if-exists
                                 x-display-color-cells)))))
        (and (numberp color-cells) (>= color-cells 8)))))

(defun mh-colors-in-use-p ()
  "Check if colors are being used in the folder buffer."
  (and mh-colors-available-flag font-lock-mode))

(defun mh-make-local-vars (&rest pairs)
  "Initialize local variables according to the variable-value PAIRS."

  (while pairs
    (set (make-local-variable (car pairs)) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))

(defun mh-restore-desktop-buffer (desktop-buffer-file-name
                                  desktop-buffer-name
                                  desktop-buffer-misc)
  "Restore an MH folder buffer specified in a desktop file.
When desktop creates a buffer, DESKTOP-BUFFER-FILE-NAME holds the file name to
visit, DESKTOP-BUFFER-NAME holds the desired buffer name, and
DESKTOP-BUFFER-MISC holds a list of miscellaneous info used by the
`desktop-buffer-handlers' functions."
  (mh-find-path)
  (mh-visit-folder desktop-buffer-name)
  (current-buffer))

;;; desktop-buffer-mode-handlers appeared in Emacs 22.
(if (fboundp 'desktop-buffer-mode-handlers)
    (add-to-list 'desktop-buffer-mode-handlers
                 '(mh-folder-mode . mh-restore-desktop-buffer)))

(defun mh-scan-folder (folder range &optional dont-exec-pending)
  "Scan the FOLDER over the RANGE.
If the optional argument DONT-EXEC-PENDING is non-nil then pending deletes and
refiles aren't carried out.
Return in the folder's buffer."
  (when (stringp range)
    (setq range (delete "" (split-string range "[ \t\n]"))))
  (cond ((null (get-buffer folder))
         (mh-make-folder folder))
        (t
         (unless dont-exec-pending
           (mh-process-or-undo-commands folder)
           (mh-reset-threads-and-narrowing))
         (switch-to-buffer folder)))
  (mh-regenerate-headers range)
  (if (zerop (buffer-size))
      (if (equal range "all")
          (message "Folder %s is empty" folder)
        (message "No messages in %s, range %s" folder range))
    (mh-goto-cur-msg))
  (when (mh-outstanding-commands-p)
    (mh-notate-deleted-and-refiled)))

(defun mh-set-cmd-note (width)
  "Set `mh-cmd-note' to WIDTH characters (minimum of 2).

If `mh-scan-format-file' specifies nil or a filename, then this function
will NOT update `mh-cmd-note'."
  ;; Add one to the width to always have whitespace in column zero.
  (setq width (max (1+ width) 2))
  (if (and (equal mh-scan-format-file t)
           (not (eq mh-cmd-note width)))
      (setq mh-cmd-note width))
  mh-cmd-note)

(defun mh-regenerate-headers (range &optional update)
  "Scan folder over range RANGE.
If UPDATE, append the scan lines, otherwise replace."
  (let ((folder mh-current-folder)
        (range (if (and range (atom range)) (list range) range))
        scan-start)
    (message "Scanning %s..." folder)
    (mh-remove-all-notation)
    (with-mh-folder-updating (nil)
      (if update
          (goto-char (point-max))
        (delete-region (point-min) (point-max))
        (if mh-adaptive-cmd-note-flag
            (mh-set-cmd-note (mh-message-number-width folder))))
      (setq scan-start (point))
      (apply #'mh-exec-cmd-output
             mh-scan-prog nil
             (mh-scan-format)
             "-noclear" "-noheader"
             "-width" (window-width)
             folder range)
      (goto-char scan-start)
      (cond ((looking-at "scan: no messages in")
             (keep-lines mh-scan-valid-regexp)) ; Flush random scan lines
            ((looking-at (if (mh-variant-p 'mu-mh)
                             "scan: message set .* does not exist"
                           "scan: bad message list "))
             (keep-lines mh-scan-valid-regexp))
            ((looking-at "scan: "))     ; Keep error messages
            (t
             (keep-lines mh-scan-valid-regexp))) ; Flush random scan lines
      (setq mh-seq-list (mh-read-folder-sequences folder nil))
      (mh-notate-user-sequences)
      (or update
          (setq mh-mode-line-annotation
                (if (equal range '("all"))
                    nil
                  mh-partial-folder-mode-line-annotation)))
      (mh-make-folder-mode-line))
    (message "Scanning %s...done" folder)))

(defun mh-generate-new-cmd-note (folder)
  "Fix the `mh-cmd-note' value for this FOLDER.

After doing an `mh-get-new-mail' operation in this FOLDER, at least
one line that looks like a truncated message number was found.

Remove the text added by the last `mh-inc' command. It should be the
messages cur-last. Call `mh-set-cmd-note' with the widest message number
in FOLDER.

Reformat the message number width on each line in the buffer and trim
the line length to fit in the window.

Rescan the FOLDER in the range cur-last in order to display the
messages that were removed earlier. They should all fit in the scan
line now with no message truncation."
  (save-excursion
    (let ((maxcol (1- (window-width)))
          (old-cmd-note mh-cmd-note)
          mh-cmd-note-fmt
          msgnum)
      ;; Nuke all of the lines just added by the last inc
      (delete-char (- (point-max) (point)))
      ;; Update the current buffer to reflect the new mh-cmd-note
      ;; value needed to display messages.
      (mh-set-cmd-note (mh-message-number-width folder))
      (setq mh-cmd-note-fmt (concat "%" (format "%d" mh-cmd-note) "d"))
      ;; Cleanup the messages that are in the buffer right now
      (goto-char (point-min))
      (cond ((memq 'unthread mh-view-ops)
             (mh-thread-add-spaces (- mh-cmd-note old-cmd-note)))
            (t (while (re-search-forward mh-scan-msg-number-regexp nil 0 1)
                 ;; reformat the number to fix in mh-cmd-note columns
                 (setq msgnum (string-to-number
                               (buffer-substring
                                (match-beginning 1) (match-end 1))))
                 (replace-match (format mh-cmd-note-fmt msgnum))
                 ;; trim the line to fix in the window
                 (end-of-line)
                 (let ((eol (point)))
                   (move-to-column maxcol)
                   (if (<= (point) eol)
                       (delete-char (- eol (point))))))))
      ;; now re-read the lost messages
      (goto-char (point-max))
      (prog1 (point)
        (mh-regenerate-headers "cur-last" t)))))

(defun mh-get-new-mail (maildrop-name)
  "Read new mail from MAILDROP-NAME into the current buffer.
Return in the current buffer."
  (let ((point-before-inc (point))
        (folder mh-current-folder)
        (new-mail-flag nil))
    (with-mh-folder-updating (t)
      (if maildrop-name
          (message "inc %s -file %s..." folder maildrop-name)
        (message "inc %s..." folder))
      (setq mh-next-direction 'forward)
      (goto-char (point-max))
      (mh-remove-cur-notation)
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
              ((and (when mh-folder-view-stack
                      (let ((saved-text (buffer-substring-no-properties
                                         start-of-inc (point-max))))
                        (delete-region start-of-inc (point-max))
                        (unwind-protect (mh-widen t)
                          (mh-remove-cur-notation)
                          (goto-char (point-max))
                          (setq start-of-inc (point))
                          (insert saved-text)
                          (goto-char start-of-inc))))
                    nil))
              ((re-search-forward "^inc:" nil t) ; Error messages
               (error "Error incorporating mail"))
              ((and
                (equal mh-scan-format-file t)
                mh-adaptive-cmd-note-flag
                ;; Have we reached an edge condition?
                (save-excursion
                  (re-search-forward mh-scan-msg-overflow-regexp nil 0 1))
                (setq start-of-inc (mh-generate-new-cmd-note folder))
                nil))
              (t
               (setq new-mail-flag t)))
        (keep-lines mh-scan-valid-regexp) ; Flush random scan lines
        (let* ((sequences (mh-read-folder-sequences folder t))
               (new-cur (assoc 'cur sequences))
               (new-unseen (assoc mh-unseen-seq sequences)))
          (unless (assoc 'cur mh-seq-list)
            (push (list 'cur) mh-seq-list))
          (unless (assoc mh-unseen-seq mh-seq-list)
            (push (list mh-unseen-seq) mh-seq-list))
          (setcdr (assoc 'cur mh-seq-list) (cdr new-cur))
          (setcdr (assoc mh-unseen-seq mh-seq-list) (cdr new-unseen)))
        (when (equal (point-max) start-of-inc)
          (mh-notate-cur))
        (if new-mail-flag
            (progn
              (mh-make-folder-mode-line)
              (when (mh-speed-flists-active-p)
                (mh-speed-flists t mh-current-folder))
              (when (memq 'unthread mh-view-ops)
                (mh-thread-inc folder start-of-inc))
              (mh-goto-cur-msg))
          (goto-char point-before-inc))
        (mh-notate-user-sequences (cons start-of-inc (point-max)))))))

(defun mh-make-folder-mode-line (&optional ignored)
  "Set the fields of the mode line for a folder buffer.
The optional argument is now obsolete and IGNORED. It used to be used to pass
in what is now stored in the buffer-local variable `mh-mode-line-annotation'."
  (save-excursion
    (save-window-excursion
      (mh-first-msg)
      (let ((new-first-msg-num (mh-get-msg-num nil)))
        (when (or (not (memq 'unthread mh-view-ops))
                  (null mh-first-msg-num)
                  (null new-first-msg-num)
                  (< new-first-msg-num mh-first-msg-num))
          (setq mh-first-msg-num new-first-msg-num)))
      (mh-last-msg)
      (let ((new-last-msg-num (mh-get-msg-num nil)))
        (when (or (not (memq 'unthread mh-view-ops))
                  (null mh-last-msg-num)
                  (null new-last-msg-num)
                  (> new-last-msg-num mh-last-msg-num))
          (setq mh-last-msg-num new-last-msg-num)))
      (setq mh-msg-count (if mh-first-msg-num
                             (count-lines (point-min) (point-max))
                           0))
      (setq mode-line-buffer-identification
            (list (format "    {%%b%s} %s msg%s"
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
                                  (""))))))
      (mh-logo-display))))

(defun mh-add-sequence-notation (msg internal-seq-flag)
  "Add sequence notation to the MSG on the current line.
If INTERNAL-SEQ-FLAG is non-nil, then refontify the scan line if font-lock is
turned on."
  (with-mh-folder-updating (t)
    (save-excursion
      (beginning-of-line)
      (if internal-seq-flag
          (progn
            ;; Change the buffer so that if transient-mark-mode is active
            ;; and there is an active region it will get deactivated as in
            ;; the case of user sequences.
            (mh-notate nil nil mh-cmd-note)
            (when font-lock-mode
              (font-lock-fontify-region (point) (line-end-position))))
        (forward-char (1+ mh-cmd-note))
        (let ((stack (gethash msg mh-sequence-notation-history)))
          (setf (gethash msg mh-sequence-notation-history)
                (cons (char-after) stack)))
        (mh-notate nil mh-note-seq (1+ mh-cmd-note))))))

(defun mh-remove-sequence-notation (msg internal-seq-flag &optional all)
  "Remove sequence notation from the MSG on the current line.
If INTERNAL-SEQ-FLAG is non-nil, then `font-lock' was used to highlight the
sequence. In that case, no notation needs to be removed. Otherwise the effect
of inserting `mh-note-seq' needs to be reversed.
If ALL is non-nil, then all sequence marks on the scan line are removed."
  (with-mh-folder-updating (t)
    ;; This takes care of internal sequences...
    (mh-notate nil nil mh-cmd-note)
    (unless internal-seq-flag
      ;; ... and this takes care of user sequences.
      (let ((stack (gethash msg mh-sequence-notation-history)))
        (while (and all (cdr stack))
          (setq stack (cdr stack)))
        (when stack
          (save-excursion
            (beginning-of-line)
            (forward-char (1+ mh-cmd-note))
            (delete-char 1)
            (insert (car stack))))
        (setf (gethash msg mh-sequence-notation-history) (cdr stack))))))

(defun mh-remove-cur-notation ()
  "Remove old cur notation."
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (save-excursion
      (when (and cur-msg
                 (mh-goto-msg cur-msg t t)
                 (looking-at mh-scan-cur-msg-number-regexp))
        (mh-notate nil ?  mh-cmd-note)
        (setq overlay-arrow-position nil)))))

(defun mh-remove-all-notation ()
  "Remove all notations on all scan lines that MH-E introduces."
  (save-excursion
    (setq overlay-arrow-position nil)
    (goto-char (point-min))
    (mh-iterate-on-range msg (cons (point-min) (point-max))
      (mh-notate nil ?  mh-cmd-note)
      (mh-remove-sequence-notation msg nil t))
    (clrhash mh-sequence-notation-history)))


(defun mh-goto-cur-msg (&optional minimal-changes-flag)
  "Position the cursor at the current message.
When optional argument MINIMAL-CHANGES-FLAG is non-nil, the function doesn't
recenter the folder buffer."
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (cond ((and cur-msg
                (mh-goto-msg cur-msg t t))
           (unless minimal-changes-flag
             (mh-notate-cur)
             (mh-recenter 0)
             (mh-maybe-show cur-msg)))
          (t
           (setq overlay-arrow-position nil)
           (message "No current message")))))

(defun mh-process-or-undo-commands (folder)
  "If FOLDER has outstanding commands, then either process or discard them.
Called by functions like `mh-sort-folder', so also invalidate show buffer."
  (set-buffer folder)
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm-flag
              (y-or-n-p
               "Process outstanding deletes and refiles? "))
          (mh-process-commands folder)
        (set-buffer folder)
        (mh-undo-folder)))
  (mh-update-unseen)
  (mh-invalidate-show-buffer))

(defun mh-process-commands (folder)
  "Process outstanding commands for FOLDER.
The value of `mh-folder-updated-hook' is a list of functions to be called,
with no arguments, before the commands are processed."
  (message "Processing deletes and refiles for %s..." folder)
  (set-buffer folder)
  (with-mh-folder-updating (nil)
    ;; Run the hook while the lists are still valid
    (run-hooks 'mh-folder-updated-hook)

    ;; Update the unseen sequence if it exists
    (mh-update-unseen)

    (let ((redraw-needed-flag mh-index-data)
          (folders-changed (list mh-current-folder))
          (seq-map (and mh-refile-list mh-refile-preserves-sequences-flag
                        (mh-create-sequence-map mh-seq-list)))
          (dest-map (and mh-refile-list mh-refile-preserves-sequences-flag
                         (make-hash-table))))
      ;; Remove invalid scan lines if we are in an index folder and then remove
      ;; the real messages
      (when mh-index-data
        (mh-index-delete-folder-headers)
        (setq folders-changed
              (append folders-changed (mh-index-execute-commands))))

      ;; Then refile messages
      (mh-mapc #'(lambda (folder-msg-list)
                   (let* ((dest-folder (symbol-name (car folder-msg-list)))
                          (last (car (mh-translate-range dest-folder "last")))
                          (msgs (cdr folder-msg-list)))
                     (push dest-folder folders-changed)
                     (setq redraw-needed-flag t)
                     (apply #'mh-exec-cmd
                            "refile" "-src" folder dest-folder
                            (mh-coalesce-msg-list msgs))
                     (mh-delete-scan-msgs msgs)
                     ;; Preserve sequences in destination folder...
                     (when mh-refile-preserves-sequences-flag
                       (clrhash dest-map)
                       (loop for i from (1+ (or last 0))
                             for msg in (sort (copy-sequence msgs) #'<)
                             do (loop for seq-name in (gethash msg seq-map)
                                      do (push i (gethash seq-name dest-map))))
                       (maphash
                        #'(lambda (seq msgs)
                            ;; Can't be run in the background, since the
                            ;; current folder is changed by mark this could
                            ;; lead to a race condition with the next refile.
                            (apply #'mh-exec-cmd "mark"
                                   "-sequence" (symbol-name seq) dest-folder
                                   "-add" (mapcar #'(lambda (x) (format "%s" x))
                                                  (mh-coalesce-msg-list msgs))))
                        dest-map))))
               mh-refile-list)
      (setq mh-refile-list ())

      ;; Now delete messages
      (cond (mh-delete-list
             (setq redraw-needed-flag t)
             (apply 'mh-exec-cmd "rmm" folder
                    (mh-coalesce-msg-list mh-delete-list))
             (mh-delete-scan-msgs mh-delete-list)
             (setq mh-delete-list nil)))

      ;; Don't need to remove sequences since delete and refile do so.
      ;; Mark cur message
      (if (> (buffer-size) 0)
          (mh-define-sequence 'cur (list (or (mh-get-msg-num nil) "last"))))

      ;; Redraw folder buffer if needed
      (when (and redraw-needed-flag)
        (when (mh-speed-flists-active-p)
          (apply #'mh-speed-flists t folders-changed))
        (cond ((memq 'unthread mh-view-ops) (mh-thread-inc folder (point-max)))
              (mh-index-data (mh-index-insert-folder-headers)))))

    (and (buffer-file-name (get-buffer mh-show-buffer))
         (not (file-exists-p (buffer-file-name (get-buffer mh-show-buffer))))
         ;; If "inc" were to put a new msg in this file,
         ;; we would not notice, so mark it invalid now.
         (mh-invalidate-show-buffer))

    (setq mh-seq-list (mh-read-folder-sequences mh-current-folder nil))
    (mh-remove-all-notation)
    (mh-notate-user-sequences)
    (message "Processing deletes and refiles for %s...done" folder)))

(defun mh-update-unseen ()
  "Synchronize the unseen sequence with MH.
Return non-nil iff the MH folder was set.
The value of `mh-unseen-updated-hook' is a list of functions to be called,
with no arguments, after the unseen sequence is updated."
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
              t)                        ;since we set the folder
          (setq mh-seen-list nil)))))

(defun mh-delete-scan-msgs (msgs)
  "Delete the scan listing lines for MSGS."
  (save-excursion
    (while msgs
      (when (mh-goto-msg (car msgs) t t)
        (when (memq 'unthread mh-view-ops)
          (mh-thread-forget-message (car msgs)))
        (mh-delete-line 1))
      (setq msgs (cdr msgs)))))

(defun mh-outstanding-commands-p ()
  "Return non-nil if there are outstanding deletes or refiles."
  (save-excursion
    (when (eq major-mode 'mh-show-mode)
      (set-buffer mh-show-folder-buffer))
    (or mh-delete-list mh-refile-list)))

(defun mh-coalesce-msg-list (messages)
  "Given a list of MESSAGES, return a list of message number ranges.
This is the inverse of `mh-read-msg-list', which expands ranges.
Message lists passed to MH programs should be processed by this function
to avoid exceeding system command line argument limits."
  (let ((msgs (sort (copy-sequence messages) 'mh-greaterp))
        (range-high nil)
        (prev -1)
        (ranges nil))
    (while prev
      (if range-high
          (if (or (not (numberp prev))
                  (not (equal (car msgs) (1- prev))))
              (progn                    ;non-sequential, flush old range
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
  "Return the greater of two message indicators MSG1 and MSG2.
Strings are \"smaller\" than numbers.
Valid values are things like \"cur\", \"last\", 1, and 1820."
  (if (numberp msg1)
      (if (numberp msg2)
          (> msg1 msg2)
        t)
    (if (numberp msg2)
        nil
      (string-lessp msg2 msg1))))

(defun mh-lessp (msg1 msg2)
  "Return the lesser of two message indicators MSG1 and MSG2.
Strings are \"smaller\" than numbers.
Valid values are things like \"cur\", \"last\", 1, and 1820."
  (not (mh-greaterp msg1 msg2)))



;;; Basic sequence handling

(defun mh-delete-seq-locally (seq)
  "Remove MH-E's record of SEQ."
  (let ((entry (mh-find-seq seq)))
    (setq mh-seq-list (delq entry mh-seq-list))))

(defun mh-read-folder-sequences (folder save-refiles)
  "Read and return the predefined sequences for a FOLDER.
If SAVE-REFILES is non-nil, then keep the sequences
that note messages to be refiled."
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
            (delete-region (point-min) (point))))) ; avoid race with
                                        ; mh-process-daemon
    seqs))

(defun mh-read-msg-list ()
  "Return a list of message numbers from point to the end of the line.
Expands ranges into set of individual numbers."
  (let ((msgs ())
        (end-of-line (save-excursion (end-of-line) (point)))
        num)
    (while (re-search-forward "[0-9]+" end-of-line t)
      (setq num (string-to-number (buffer-substring (match-beginning 0)
                                                    (match-end 0))))
      (cond ((looking-at "-")           ; Message range
             (forward-char 1)
             (re-search-forward "[0-9]+" end-of-line t)
             (let ((num2 (string-to-number
                          (buffer-substring (match-beginning 0)
                                            (match-end 0)))))
               (if (< num2 num)
                   (error "Bad message range: %d-%d" num num2))
               (while (<= num num2)
                 (setq msgs (cons num msgs))
                 (setq num (1+ num)))))
            ((not (zerop num))          ;"pick" outputs "0" to mean no match
             (setq msgs (cons num msgs)))))
    msgs))

(defun mh-notate-user-sequences (&optional range)
  "Mark user-defined sequences in the messages specified by RANGE.
The optional argument RANGE can be a message number, a list of message
numbers, a sequence, a region in a cons cell. If nil all messages are notated."
  (unless range
    (setq range (cons (point-min) (point-max))))
  (let ((seqs mh-seq-list)
        (msg-hash (make-hash-table)))
    (dolist (seq seqs)
      (dolist (msg (mh-seq-msgs seq))
        (push (car seq) (gethash msg msg-hash))))
    (mh-iterate-on-range msg range
      (loop for seq in (gethash msg msg-hash)
            do (mh-add-sequence-notation msg (mh-internal-seq seq))))))

(defvar mh-internal-seqs '(answered cur deleted forwarded printed))

(defun mh-internal-seq (name)
  "Return non-nil if NAME is the name of an internal MH-E sequence."
  (or (memq name mh-internal-seqs)
      (eq name mh-unseen-seq)
      (and (mh-colors-in-use-p) mh-tick-seq (eq name mh-tick-seq))
      (eq name mh-previous-seq)
      (mh-folder-name-p name)))

(defun mh-valid-seq-p (name)
  "Return non-nil if NAME is a valid MH sequence name."
  (and (symbolp name)
       (string-match "^[a-zA-Z][a-zA-Z0-9]*$" (symbol-name name))))

(defun mh-delete-msg-from-seq (range sequence &optional internal-flag)
  "Delete RANGE from SEQUENCE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

Optional third arg INTERNAL-FLAG non-nil means do not inform MH of the
change."
  (interactive (list (mh-interactive-range "Delete")
                     (mh-read-seq-default "Delete from" t)
                     nil))
  (let ((entry (mh-find-seq sequence))
        (user-sequence-flag (not (mh-internal-seq sequence)))
        (folders-changed (list mh-current-folder))
        (msg-list ()))
    (when entry
      (mh-iterate-on-range msg range
        (push msg msg-list)
        ;; Calling "mark" repeatedly takes too long. So we will pretend here
        ;; that we are just modifying an internal sequence...
        (when (memq msg (cdr entry))
          (mh-remove-sequence-notation msg (not user-sequence-flag)))
        (mh-delete-a-msg-from-seq msg sequence t))
      ;; ... and here we will "mark" all the messages at one go.
      (unless internal-flag (mh-undefine-sequence sequence msg-list))
      (when (and mh-index-data (not internal-flag))
        (setq folders-changed
              (append folders-changed
                      (mh-index-delete-from-sequence sequence msg-list))))
      (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
        (apply #'mh-speed-flists t folders-changed)))))

(defun mh-catchup (range)
  "Delete RANGE from the `mh-unseen-seq' sequence.

Check the document of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Catchup"
                                           (cons (point-min) (point-max)))))
  (mh-delete-msg-from-seq range mh-unseen-seq))

(defun mh-delete-a-msg-from-seq (msg sequence internal-flag)
  "Delete MSG from SEQUENCE.
If INTERNAL-FLAG is non-nil, then do not inform MH of the change."
  (let ((entry (mh-find-seq sequence)))
    (when (and entry (memq msg (mh-seq-msgs entry)))
      (if (not internal-flag)
          (mh-undefine-sequence sequence (list msg)))
      (setcdr entry (delq msg (mh-seq-msgs entry))))))

(defun mh-undefine-sequence (seq msgs)
  "Remove from the SEQ the list of MSGS."
  (when (and (mh-valid-seq-p seq) msgs)
    (apply #'mh-exec-cmd "mark" mh-current-folder "-delete"
           "-sequence" (symbol-name seq) (mh-coalesce-msg-list msgs))))

(defun mh-define-sequence (seq msgs)
  "Define the SEQ to contain the list of MSGS.
Do not mark pseudo-sequences or empty sequences.
Signals an error if SEQ is an invalid name."
  (if (and msgs
           (mh-valid-seq-p seq)
           (not (mh-folder-name-p seq)))
      (save-excursion
        (mh-exec-cmd-error nil "mark" mh-current-folder "-add" "-zero"
                           "-sequence" (symbol-name seq)
                           (mh-coalesce-msg-list msgs)))))

(defun mh-seq-containing-msg (msg &optional include-internal-flag)
  "Return a list of the sequences containing MSG.
If INCLUDE-INTERNAL-FLAG non-nil, include MH-E internal sequences in list."
  (let ((l mh-seq-list)
        (seqs ()))
    (while l
      (and (memq msg (mh-seq-msgs (car l)))
           (or include-internal-flag
               (not (mh-internal-seq (mh-seq-name (car l)))))
           (setq seqs (cons (mh-seq-name (car l)) seqs)))
      (setq l (cdr l)))
    seqs))



;;; Build the folder-mode keymap:

(suppress-keymap mh-folder-mode-map)

;; Use defalias to make sure the documented primary key bindings
;; appear in menu lists.
(defalias 'mh-alt-show 'mh-show)
(defalias 'mh-alt-refile-msg 'mh-refile-msg)
(defalias 'mh-alt-send 'mh-send)
(defalias 'mh-alt-visit-folder 'mh-visit-folder)

;; Save the `b' binding for a future `back'. Maybe?
(gnus-define-keys  mh-folder-mode-map
  " "           mh-page-msg
  "!"           mh-refile-or-write-again
  "'"           mh-toggle-tick
  ","           mh-header-display
  "."           mh-alt-show
  ";"           mh-toggle-mh-decode-mime-flag
  ">"           mh-write-msg-to-file
  "?"           mh-help
  "E"           mh-extract-rejected-mail
  "M"           mh-modify
  "\177"        mh-previous-page
  "\C-d"        mh-delete-msg-no-motion
  "\t"          mh-index-next-folder
  [backtab]     mh-index-previous-folder
  "\M-\t"       mh-index-previous-folder
  "\e<"         mh-first-msg
  "\e>"         mh-last-msg
  "\ed"         mh-redistribute
  "\r"          mh-show
  "^"           mh-alt-refile-msg
  "c"           mh-copy-msg
  "d"           mh-delete-msg
  "e"           mh-edit-again
  "f"           mh-forward
  "g"           mh-goto-msg
  "i"           mh-inc-folder
  "k"           mh-delete-subject-or-thread
  "m"           mh-alt-send
  "n"           mh-next-undeleted-msg
  "\M-n"        mh-next-unread-msg
  "o"           mh-refile-msg
  "p"           mh-previous-undeleted-msg
  "\M-p"        mh-previous-unread-msg
  "q"           mh-quit
  "r"           mh-reply
  "s"           mh-send
  "t"           mh-toggle-showing
  "u"           mh-undo
  "v"           mh-index-visit-folder
  "x"           mh-execute-commands
  "|"           mh-pipe-msg)

(gnus-define-keys (mh-folder-map "F" mh-folder-mode-map)
  "?"           mh-prefix-help
  "'"           mh-index-ticked-messages
  "S"           mh-sort-folder
  "c"           mh-catchup
  "f"           mh-alt-visit-folder
  "i"           mh-index-search
  "k"           mh-kill-folder
  "l"           mh-list-folders
  "n"           mh-index-new-messages
  "o"           mh-alt-visit-folder
  "p"           mh-pack-folder
  "q"           mh-index-sequenced-messages
  "r"           mh-rescan-folder
  "s"           mh-search-folder
  "u"           mh-undo-folder
  "v"           mh-visit-folder)

(define-key mh-folder-mode-map "I" mh-inc-spool-map)

(gnus-define-keys (mh-junk-map "J" mh-folder-mode-map)
  "?"           mh-prefix-help
  "b"           mh-junk-blacklist
  "w"           mh-junk-whitelist)

(gnus-define-keys (mh-ps-print-map "P" mh-folder-mode-map)
  "?"           mh-prefix-help
  "A"           mh-ps-print-toggle-mime
  "C"           mh-ps-print-toggle-color
  "F"           mh-ps-print-toggle-faces
  "M"           mh-ps-print-toggle-mime
  "f"           mh-ps-print-msg-file
  "l"		mh-print-msg
  "p"           mh-ps-print-msg
  "s"           mh-ps-print-msg-show)

(gnus-define-keys (mh-sequence-map "S" mh-folder-mode-map)
  "'"           mh-narrow-to-tick
  "?"           mh-prefix-help
  "d"           mh-delete-msg-from-seq
  "k"           mh-delete-seq
  "l"           mh-list-sequences
  "n"           mh-narrow-to-seq
  "p"           mh-put-msg-in-seq
  "s"           mh-msg-is-in-seq
  "w"           mh-widen)

(gnus-define-keys (mh-thread-map "T" mh-folder-mode-map)
  "?"           mh-prefix-help
  "u"           mh-thread-ancestor
  "p"           mh-thread-previous-sibling
  "n"           mh-thread-next-sibling
  "t"           mh-toggle-threads
  "d"           mh-thread-delete
  "o"           mh-thread-refile)

(gnus-define-keys (mh-limit-map "/" mh-folder-mode-map)
  "'"           mh-narrow-to-tick
  "?"           mh-prefix-help
  "c"           mh-narrow-to-cc
  "f"           mh-narrow-to-from
  "r"           mh-narrow-to-range
  "s"           mh-narrow-to-subject
  "t"           mh-narrow-to-to
  "w"           mh-widen)

(gnus-define-keys (mh-extract-map "X" mh-folder-mode-map)
  "?"           mh-prefix-help
  "s"           mh-store-msg            ;shar
  "u"           mh-store-msg)           ;uuencode

(gnus-define-keys (mh-digest-map "D" mh-folder-mode-map)
  " "           mh-page-digest
  "?"           mh-prefix-help
  "\177"        mh-page-digest-backwards
  "b"           mh-burst-digest)

(gnus-define-keys (mh-mime-map "K" mh-folder-mode-map)
  "?"           mh-prefix-help
  "a"           mh-mime-save-parts
  "e"           mh-display-with-external-viewer
  "i"           mh-folder-inline-mime-part
  "o"           mh-folder-save-mime-part
  "t"           mh-toggle-mime-buttons
  "v"           mh-folder-toggle-mime-part
  "\t"          mh-next-button
  [backtab]     mh-prev-button
  "\M-\t"       mh-prev-button)

(cond
 (mh-xemacs-flag
  (define-key mh-folder-mode-map [button2] 'mh-show-mouse))
 (t
  (define-key mh-folder-mode-map [mouse-2] 'mh-show-mouse)))

;; "C-c /" prefix is used in mh-folder-mode by pgp.el and mailcrypt



;;; Help Messages

;;; If you add a new prefix, add appropriate text to the nil key.
;;;
;;; In general, messages are grouped logically. Taking the main commands for
;;; example, the first line is "ways to view messages," the second line is
;;; "things you can do with messages", and the third is "composing" messages.
;;;
;;; When adding a new prefix, ensure that the help message contains "what" the
;;; prefix is for. For example, if the word "folder" were not present in the
;;; `F' entry, it would not be clear what these commands operated upon.
(defvar mh-help-messages
  '((nil "[i]nc, [.]show, [,]show all, [n]ext, [p]revious,\n"
         "[d]elete, [o]refile, e[x]ecute,\n"
         "[s]end, [r]eply,\n"
         "[;]toggle MIME decoding.\n"
         "Prefix characters:\n [F]older, [S]equence, [J]unk, MIME [K]eys,"
         "\n [T]hread, [/]limit, e[X]tract, [D]igest, [I]nc spools.")

    (?F "[l]ist; [v]isit folder;\n"
        "[n]ew messages; [']ticked messages; [s]earch; [i]ndexed search;\n"
        "[p]ack; [S]ort; [r]escan; [k]ill")
    (?P "PS [p]rint message; [l]non-PS print;\n"
        "PS Print [s]how window, message to [f]ile;\n"
        "Toggle printing of [M]IME parts, [C]olor, [F]aces")
    (?S "[p]ut message in sequence, [n]arrow, [']narrow to ticked, [w]iden,\n"
        "[s]equences, [l]ist,\n"
        "[d]elete message from sequence, [k]ill sequence")
    (?T "[t]oggle, [d]elete, [o]refile thread")
    (?/ "Limit to [c]c, [f]rom, [r]ange, [s]ubject, [t]o; [w]iden")
    (?X "un[s]har, [u]udecode message")
    (?D "[b]urst digest")
    (?K "[v]iew, [i]nline, [o]utput/save MIME part; save [a]ll parts; \n"
        "[TAB] next; [SHIFT-TAB] previous")
    (?J "[b]lacklist, [w]hitelist message"))
  "Key binding cheat sheet.

This is an associative array which is used to show the most common commands.
The key is a prefix char. The value is one or more strings which are
concatenated together and displayed in the minibuffer if ? is pressed after
the prefix character. The special key nil is used to display the
non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.")



(dolist (mess '("^Cursor not pointing to message$"
                "^There is no other window$"))
  (add-to-list 'debug-ignored-errors mess))

(provide 'mh-e)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: cce884de-bd37-4104-9963-e4439d5ed22b
;;; mh-e.el ends here
