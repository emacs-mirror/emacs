;;; mh-junk.el --- Interface to anti-spam measures

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>,
;;         Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords:  mail, spam

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

;; Spam handling in MH-E.

;;; Change Log:

;;; Code:

(require 'mh-e)

;; Interactive functions callable from the folder buffer
;;;###mh-autoload
(defun mh-junk-blacklist (msg-or-seq)
  "Blacklist MSG-OR-SEQ as spam.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is blacklisted.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence.

First the appropriate function is called depending on the value of
`mh-junk-choice'. Then if `mh-junk-mail-folder' is a string then the message is
refiled to that folder. If nil, the message is deleted.

To change the spam program being used, customize `mh-junk-program'. Directly
setting `mh-junk-choice' is not recommended.

The documentation for the following functions describes what setup is needed
for the different spam fighting programs:

  - `mh-bogofilter-blacklist'
  - `mh-spamprobe-blacklist'
  - `mh-spamassassin-blacklist'"
  (interactive (list (mh-interactive-msg-or-seq "Blacklist")))
  (let ((blacklist-func (nth 1 (assoc mh-junk-choice mh-junk-function-alist))))
    (unless blacklist-func
      (error "Customize `mh-junk-program' appropriately"))
    (let ((dest (cond ((null mh-junk-mail-folder) nil)
                      ((equal mh-junk-mail-folder "") "+")
                      ((eq (aref mh-junk-mail-folder 0) ?+)
                       mh-junk-mail-folder)
                      ((eq (aref mh-junk-mail-folder 0) ?@)
                       (concat mh-current-folder "/"
                               (substring mh-junk-mail-folder 1)))
                      (t (concat "+" mh-junk-mail-folder)))))
      (mh-iterate-on-msg-or-seq msg msg-or-seq
        (funcall (symbol-function blacklist-func) msg)
        (if dest
            (mh-refile-a-msg nil (intern dest))
          (mh-delete-a-msg nil)))
      (mh-next-msg))))

;;;###mh-autoload
(defun mh-junk-whitelist (msg-or-seq)
  "Whitelist MSG-OR-SEQ incorrectly classified as spam.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is whitelisted.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence.

First the appropriate function is called depending on the value of
`mh-junk-choice'. Then the message is refiled to `mh-inbox'.

To change the spam program being used, customize `mh-junk-program'. Directly
setting `mh-junk-choice' is not recommended."
  (interactive (list (mh-interactive-msg-or-seq "Whitelist")))
  (let ((whitelist-func (nth 2 (assoc mh-junk-choice mh-junk-function-alist))))
    (unless whitelist-func
      (error "Customize `mh-junk-program' appropriately"))
    (mh-iterate-on-msg-or-seq msg msg-or-seq
      (funcall (symbol-function whitelist-func) msg)
      (mh-refile-a-msg nil (intern mh-inbox)))
    (mh-next-msg)))



;; Bogofilter Interface

(defvar mh-bogofilter-executable (executable-find "bogofilter"))

(defun mh-bogofilter-blacklist (msg)
  "Classify MSG as spam.
Tell bogofilter that the message is spam.

Bogofilter is a Bayesian spam filtering program. Get it from your local
distribution or from:
   http://bogofilter.sourceforge.net/

You first need to teach bogofilter. This is done by running

   bogofilter -n < good-message

on every good message, and

   bogofilter -s < spam-message

on every spam message. Most Bayesian filters need 1000 to 5000 of each to
start doing a good job.

To use bogofilter, add the following .procmailrc recipes which you can also
find in the bogofilter man page:

   # Bogofilter
   :0fw
   | bogofilter -u -e -p

   :0
   * ^X-Bogosity: Yes, tests=bogofilter
   $SPAM

Bogofilter continues to feed the messages it classifies back into its
database. Occasionally it misses, and those messages need to be reclassified.
MH-E can do this for you. Use \\[mh-junk-blacklist] to reclassify messges in
your +inbox as spam, and \\[mh-junk-whitelist] to reclassify messages in your
spambox as good messages."
  (unless mh-bogofilter-executable
    (error "Couldn't find the bogofilter executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (call-process mh-bogofilter-executable msg-file 0 nil "-Ns")))

(defun mh-bogofilter-whitelist (msg)
  "Reinstate incorrectly filtered MSG.
Train bogofilter to think of the message as non-spam."
  (unless mh-bogofilter-executable
    (error "Couldn't find the bogofilter executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (call-process mh-bogofilter-executable msg-file 0 nil "-Sn")))



;; Spamprobe Interface

(defvar mh-spamprobe-executable (executable-find "spamprobe"))

(defun mh-spamprobe-blacklist (msg)
  "Classify MSG as spam.
Tell spamprobe that the message is spam.

Spamprobe is a Bayesian spam filtering program. More info about the program can
be found at:
   http://spamprobe.sourceforge.net

Here is a procmail recipe to stores incoming spam mail into the folder +spam
and good mail in /home/user/Mail/mdrop/mbox. This recipe is provided as an
example in the spamprobe man page.

   PATH=/bin:/usr/bin:/usr/local/bin
   DEFAULT=/home/user/Mail/mdrop/mbox
   SPAM=/home/user/Mail/spam/.

   # Spamprobe filtering
   :0
   SCORE=| spamprobe receive
   :0 wf
   | formail -I \"X-SpamProbe: $SCORE\"
   :0 a:
   *^X-SpamProbe: SPAM
   $SPAM

Occasionally some good mail gets misclassified as spam. You can use
\\[mh-junk-whitelist] to reclassify that as good mail."
  (unless mh-spamprobe-executable
    (error "Couldn't find the spamprobe executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (call-process mh-spamprobe-executable msg-file 0 nil "spam")))

(defun mh-spamprobe-whitelist (msg)
  "Reinstate incorrectly filtered MSG.
Train spamprobe to think of the message as non-spam."
  (unless mh-spamprobe-executable
    (error "Couldn't find the spamprobe executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (call-process mh-spamprobe-executable msg-file 0 nil "good")))



;; Spamassassin Interface

(defvar mh-spamassassin-executable (executable-find "spamassassin"))
(defvar mh-sa-learn-executable (executable-find "sa-learn"))

(defun mh-spamassassin-blacklist (msg)
  "Blacklist MSG.
This is done by sending the message to Razor and by appending the sender to
~/.spamassassin/user_prefs in a blacklist_from rule. If sa-learn is available,
the message is also recategorized as spam.

Spamassassin is an excellent spam filter. For more information, see:
  http://spamassassin.org/.

I ran \"spamassassin -t\" on every mail message in my archive and ran an
analysis in Gnumeric to find that the standard deviation of good mail
scored under 5 (coincidentally, the spamassassin default for \"spam\").

Furthermore, I observed that there weren't any messages with a score of 8
or more that were interesting, so I added a couple of points to be
conservative and send any message with a score of 10 or more down the
drain. You might want to use a score of 12 or 13 to be really conservative.
I have found that this really decreases the amount of junk to review.

Messages with a score of 5-9 are set aside for later review. The major
weakness of rules-based filters is a plethora of false positives\; I catch one
or two legitimate messages in here a week, so it is worthwhile to check.

You might choose to do this analysis yourself to pick a good score for
deleting spam sight unseen, or you might pick a score out of a hat, or you
might choose to be very conservative and not delete any messages at all.

Based upon this discussion, here is what the associated ~/.procmailrc
entries look like. These rules appear before my list filters so that spam
sent to mailing lists gets pruned too.

   #
   # Spam
   #
   :0fw
   | spamc

   # Anything with a spam level of 10 or more is junked immediately.
   :0:
   * ^X-Spam-Level: ..........
   /dev/null

   :0
   * ^X-Spam-Status: Yes
   $SPAM

If you don't use \"spamc\", use \"spamassassin -P -a\".

A handful of spam does find its way into +inbox. In this case, use
\\[mh-junk-blacklist] to add a \"blacklist_from\" line to
~/spamassassin/user_prefs, delete the message, and send the message to the
Razor, so that others might not see this spam.

Over time, you see some patterns in the blacklisted addresses and can
replace several lines with wildcards. For example, it is clear that High
Speed Media is the biggest bunch of jerks on the Net. Here are some of the
entries I have for them, and the list continues to grow.

   blacklist_from	*@*-hsm-*.com
   blacklist_from	*@*182*643*.com
   blacklist_from	*@*antarhsm*.com
   blacklist_from	*@*h*speed*
   blacklist_from	*@*hsm*182*.com
   blacklist_from	*@*hsm*643*.com
   blacklist_from	*@*hsmridi2983cslt227.com
   blacklist_from	*@*list*hsm*.com
   blacklist_from	*@h*s*media*
   blacklist_from	*@hsmdrct.com
   blacklist_from	*@hsmridi2983csltsite.com

The function `mh-spamassassin-identify-spammers' is provided that shows the
frequency counts of the host and domain names in your blacklist_from
entries. This can be helpful when editing the blacklist_from entries.

In versions of spamassassin (2.50 and on) that support a Bayesian classifier,
\\[mh-junk-blacklist] uses the sa-learn program to recategorize the message as
spam. Neither MH-E, nor spamassassin, rebuilds the database after adding
words, so you will need to run \"sa-learn --rebuild\" periodically. This can
be done by adding the following to your crontab:

  0 * * * *	sa-learn --rebuild > /dev/null 2>&1"
  (unless mh-spamassassin-executable
    (error "Couldn't find the spamassassin executable"))
  (let ((current-folder mh-current-folder)
        (msg-file (mh-msg-filename msg mh-current-folder))
        (sender))
    (save-excursion
      (message "Giving this message the Razor...")
      (mh-truncate-log-buffer)
      (call-process mh-spamassassin-executable msg-file mh-log-buffer nil
                    "--report" "--remove-from-whitelist")
      (when mh-sa-learn-executable
          (message "Recategorizing this message as spam...")
          (call-process mh-sa-learn-executable msg-file mh-log-buffer nil
                        "--single" "--spam" "--local --no-rebuild"))
      (message "Blacklisting address...")
      (set-buffer (get-buffer-create mh-temp-buffer))
      (erase-buffer)
      (call-process (expand-file-name mh-scan-prog mh-progs) nil t nil
                    (format "%s" msg) current-folder
                    "-format" "%<(mymbox{from})%|%(addr{from})%>")
      (goto-char (point-min))
      (if (search-forward-regexp "^\\(.+\\)$" nil t)
          (progn
            (setq sender (match-string 0))
            (mh-spamassassin-add-rule "blacklist_from" sender)
            (message "Blacklisting address...done"))
        (message "Blacklisting address...not done (from my address)")))))

(defun mh-spamassassin-whitelist (msg)
  "Whitelist MSG.
Add a whitelist_from rule to the ~/.spamassassin/user_prefs file. If sa-learn
is available, then the message is recategorized as ham."
  (unless mh-spamassassin-executable
    (error "Couldn't find the spamassassin executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder))
        (show-buffer (get-buffer mh-show-buffer))
        from)
    (save-excursion
      (set-buffer (get-buffer-create mh-temp-buffer))
      (erase-buffer)
      (message "Removing spamassassin markup from message...")
      (call-process mh-spamassassin-executable msg-file mh-temp-buffer nil
                    "--remove-markup")
      (if show-buffer
          (kill-buffer show-buffer))
      (write-file msg-file)
      (when mh-sa-learn-executable
        (message "Recategorizing this message as ham...")
        (call-process mh-sa-learn-executable msg-file mh-temp-buffer nil
                      "--single" "--ham" "--local --no-rebuild"))
      (message "Whitelisting address...")
      (setq from (car (ietf-drums-parse-address (mh-get-header-field "From:"))))
      (kill-buffer nil)
      (unless (equal from "")
        (mh-spamassassin-add-rule "whitelist_from" from))
      (message "Whitelisting address...done"))))

(defun mh-spamassassin-add-rule (rule body)
  "Add a new rule to ~/.spamassassin/user_prefs.
The name of the rule is RULE and its body is BODY."
  (save-window-excursion
    (let* ((line (format "%s\t%s\n" rule body))
           (case-fold-search t)
           (file (expand-file-name "~/.spamassassin/user_prefs"))
           (buffer-exists (find-buffer-visiting file)))
      (find-file file)
      (if (not (search-forward (format "\n%s" line) nil t))
          (progn
            (goto-char (point-max))
            (insert (if (bolp) "" "\n") line)
            (save-buffer)))
      (if (not buffer-exists)
          (kill-buffer nil)))))

(defun mh-spamassassin-identify-spammers ()
  "Identifies spammers who are repeat offenders.

For each blacklist_from entry from the last blank line of
~/.spamassassin/user_prefs to the end of the file, a list of host and domain
names along with their frequency counts is displayed. This information can be
used to replace multiple blacklist_from entries with a single wildcard entry
such as:

  blacklist_from *@*amazingoffersdirect2u.com"
  (interactive)
  (let* ((file (expand-file-name "~/.spamassassin/user_prefs"))
         (domains (make-hash-table :test 'equal)))
    (find-file file)
    ;; Only consider entries between last blank line and end of file.
    (goto-char (1- (point-max)))
    (search-backward-regexp "^$")
    ;; Perform frequency count.
    (save-excursion
      (while (search-forward-regexp "^blacklist_from\\s-*\\(.*\\)@\\(.*\\)$"
                                    nil t)
        (let ((host (match-string 2))
              value)
          ;; Remove top-level-domain from hostname.
          (setq host (cdr (reverse (split-string host "\\."))))
          ;; Add counts for each host and domain part.
          (while host
            (setq value (gethash (car host) domains))
            (puthash (car host) (1+ (if (not value) 0 value)) domains)
            (setq host (cdr host))))))

    ;; Output
    (delete-other-windows)
    (pop-to-buffer (get-buffer-create "*MH-E Spammer Frequencies*"))
    (erase-buffer)
    (maphash '(lambda (key value) ""
                (if (> value 2)
                    (insert (format "%s %s\n" key value))))
             domains)
    (sort-numeric-fields 2 (point-min) (point-max))
    (reverse-region (point-min) (point-max))
    (goto-char (point-min))))

(provide 'mh-junk)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 603335f1-77ff-4306-8828-5d3dad51abe1
;;; mh-junk.el ends here
