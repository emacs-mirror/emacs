;;; mh-junk.el --- MH-E interface to anti-spam measures  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2022 Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>,
;;         Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords:  mail, spam

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Spam handling in MH-E.

;;; Code:

(require 'mh-e)
(require 'mh-scan)

(autoload 'mail-header-parse-address "mail-parse")

;;;###mh-autoload
(defun mh-junk-blocklist (range)
  "Blocklist RANGE as spam.

This command trains the spam program in use (see the option
`mh-junk-program') with the content of RANGE and then handles the
message(s) as specified by the option `mh-junk-disposition'.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

For more information about using your particular spam fighting
program, see:

  - `mh-spamassassin-blocklist'
  - `mh-bogofilter-blocklist'
  - `mh-spamprobe-blocklist'"
  (interactive (list (mh-interactive-range "Blocklist")))
  (mh-iterate-on-range () range (mh-junk-blocklist-a-msg nil))
  (if (looking-at mh-scan-blocklisted-msg-regexp)
      (mh-next-msg)))

(defun mh-junk-blocklist-a-msg (message)
  "Blocklist MESSAGE.
If MESSAGE is nil then the message at point is blocklisted.
The hook `mh-blocklist-msg-hook' is called after you mark a message
for blocklisting."
  (save-excursion
    (if (numberp message)
        (mh-goto-msg message nil t)
      (beginning-of-line)
      (setq message (mh-get-msg-num t)))
    (cond ((looking-at mh-scan-refiled-msg-regexp)
           (error "Message %d is refiled; undo refile before blocklisting"
                  message))
          ((looking-at mh-scan-deleted-msg-regexp)
           (error "Message %d is deleted; undo delete before blocklisting"
                  message))
          ((looking-at mh-scan-allowlisted-msg-regexp)
           (error "Message %d is allowlisted; undo before blocklisting"
                  message))
          ((looking-at mh-scan-blocklisted-msg-regexp) nil)
          (t
           (mh-set-folder-modified-p t)
           (setq mh-blocklist (cons message mh-blocklist))
           (if (not (memq message mh-seen-list))
               (setq mh-seen-list (cons message mh-seen-list)))
           (mh-notate nil mh-note-blocklisted mh-cmd-note)
           (run-hooks 'mh-blocklist-msg-hook)))))

;;;###mh-autoload
(defun mh-junk-blocklist-disposition ()
  "Determines the fate of the selected spam."
  (cond ((null mh-junk-disposition) nil)
        ((equal mh-junk-disposition "") "+")
        ((eq (aref mh-junk-disposition 0) ?+)
         mh-junk-disposition)
        ((eq (aref mh-junk-disposition 0) ?@)
         (concat mh-current-folder "/"
                 (substring mh-junk-disposition 1)))
        (t (concat "+" mh-junk-disposition))))

;;;###mh-autoload
(defun mh-junk-process-blocklist (range)
  "Blocklist RANGE as spam.
This command trains the spam program in use (see the option
`mh-junk-program') with the content of RANGE and then handles the
message(s) as specified by the option `mh-junk-disposition'."
  (let ((blocklist-func (nth 1 (assoc mh-junk-choice mh-junk-function-alist))))
    (unless blocklist-func
      (error "Customize `mh-junk-program' appropriately"))
    (mh-iterate-on-range msg range
      (funcall (symbol-function blocklist-func) msg))))

;;;###mh-autoload
(defun mh-junk-whitelist (range)
  "Old name for `mh-junk-allowlist'; use \\[mh-junk-allowlist] instead."
  (interactive (list (mh-interactive-range "Allowlist")))
  ;; We do our own message here instead of using "declare obsolete"
  ;; in order to talk about keys instead of function names.  Also, it
  ;; lets us bind "J w" to this without the Emacs 29 compiler complaining.
  (when (not (get 'mh-junk-whitelist 'command-execute-obsolete-warned))
    (message "%s is an obsolete key (as of 28.1); use %s instead"
             (substitute-command-keys "\\[mh-junk-whitelist]")
             (substitute-command-keys "\\[mh-junk-allowlist]"))
    (put 'mh-junk-whitelist 'command-execute-obsolete-warned t))
  (mh-junk-allowlist range))

;;;###mh-autoload
(defun mh-junk-allowlist (range)
  "Allowlist RANGE as ham.

This command reclassifies the RANGE as ham if it has been incorrectly
classified as spam (see the option `mh-junk-program'). It then
refiles the message into the \"+inbox\" folder.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Allowlist")))
  (mh-iterate-on-range () range (mh-junk-allowlist-a-msg nil))
  (if (looking-at mh-scan-allowlisted-msg-regexp)
      (mh-next-msg)))

(defun mh-junk-allowlist-a-msg (message)
  "Allowlist MESSAGE.
If MESSAGE is nil then the message at point is allowlisted. The
hook `mh-allowlist-msg-hook' is called after you mark a message
for allowlisting."
  (save-excursion
    (if (numberp message)
        (mh-goto-msg message nil t)
      (beginning-of-line)
      (setq message (mh-get-msg-num t)))
    (cond ((looking-at mh-scan-refiled-msg-regexp)
           (error "Message %d is refiled; undo refile before allowlisting"
                  message))
          ((looking-at mh-scan-deleted-msg-regexp)
           (error "Message %d is deleted; undo delete before allowlisting"
                  message))
          ((looking-at mh-scan-blocklisted-msg-regexp)
           (error "Message %d is blocklisted; undo before allowlisting"
                  message))
          ((looking-at mh-scan-allowlisted-msg-regexp) nil)
          (t
           (mh-set-folder-modified-p t)
           (setq mh-allowlist (cons message mh-allowlist))
           (mh-notate nil mh-note-allowlisted mh-cmd-note)
           (run-hooks 'mh-allowlist-msg-hook)))))

;;;###mh-autoload
(defun mh-junk-process-allowlist (range)
  "Allowlist RANGE as ham.

This command reclassifies the RANGE as ham if it were incorrectly
classified as spam (see the option `mh-junk-program')."
  (let ((allowlist-func (nth 2 (assoc mh-junk-choice mh-junk-function-alist))))
    (unless allowlist-func
      (error "Customize `mh-junk-program' appropriately"))
    (mh-iterate-on-range msg range
      (funcall (symbol-function allowlist-func) msg))))



;; SpamAssassin Interface

(defvar mh-spamassassin-executable (executable-find "spamassassin"))
(defvar mh-sa-learn-executable (executable-find "sa-learn"))

;;;###mh-autoload
(defun mh-spamassassin-blocklist (msg)
  "Blocklist MSG with SpamAssassin.

SpamAssassin is one of the more popular spam filtering programs.
Get it from your local distribution or from the SpamAssassin web
site at URL `https://spamassassin.apache.org/'.

To use SpamAssassin, add the following recipes to
\".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with SpamAssassin.
    :0fw
    | spamc

    # Anything with a spam level of 10 or more is junked immediately.
    :0:
    * ^X-Spam-Level: ..........
    /dev/null

    :0:
    * ^X-Spam-Status: Yes
    spam/.

If you don't use \"spamc\", use \"spamassassin\".

Note that one of the recipes above throws away messages with a
score greater than or equal to 10. Here's how you can determine a
value that works best for you.

First, run \"spamassassin -t\" on every mail message in your
archive and use Gnumeric to verify that the average plus the
standard deviation of good mail is under 5, the SpamAssassin
default for \"spam\".

Using Gnumeric, sort the messages by score and view the messages
with the highest score. Determine the score which encompasses all
of your interesting messages and add a couple of points to be
conservative. Add that many dots to the \"X-Spam-Level:\" header
field above to send messages with that score down the drain.

In the example above, messages with a score of 5-9 are set aside
in the \"+spam\" folder for later review. The major weakness of
rules-based filters is a plethora of false positives so it is
worthwhile to check.

If SpamAssassin classifies a message incorrectly, or is unsure,
you can use the MH-E commands \\[mh-junk-blocklist] and
\\[mh-junk-allowlist].

The command \\[mh-junk-blocklist] adds a \"blacklist_from\" entry
to \"~/spamassassin/user_prefs\", deletes the message, and sends
the message to the Razor, so that others might not see this spam.
If the \"sa-learn\" command is available, the message is also
recategorized as spam.

The command \\[mh-junk-allowlist] adds a \"whitelist_from\" rule
to the \"~/.spamassassin/user_prefs\" file. If the \"sa-learn\"
command is available, the message is also recategorized as ham.

Over time, you'll observe that the same host or domain occurs
repeatedly in the \"blacklist_from\" entries, so you might think
that you could avoid future spam by blocklisting all mail from a
particular domain. The utility function
`mh-spamassassin-identify-spammers' helps you do precisely that.
This function displays a frequency count of the hosts and domains
in the \"blacklist_from\" entries from the last blank line in
\"~/.spamassassin/user_prefs\" to the end of the file. This
information can be used so that you can replace multiple
\"blacklist_from\" entries with a single wildcard entry such as:

    blacklist_from *@*amazingoffersdirect2u.com"
  (unless mh-spamassassin-executable
    (error "Unable to find the spamassassin executable"))
  (let ((current-folder mh-current-folder)
        (msg-file (mh-msg-filename msg mh-current-folder))
        (sender))
    (message "Reporting message %d as spam with spamassassin..." msg)
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-spamassassin-executable msg-file mh-junk-background nil
                    ;; -R removes from allowlist
                    "--report" "-R")
    (when mh-sa-learn-executable
      (message "Recategorizing message %d as spam with sa-learn..." msg)
      (mh-truncate-log-buffer)
      (call-process mh-sa-learn-executable msg-file mh-junk-background nil
                    "--spam" "--local" "--no-sync")))
    (message "Blocklisting sender of message %d..." msg)
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (call-process (expand-file-name mh-scan-prog mh-progs)
                    nil t nil
                    (format "%d" msg) current-folder
                    "-format" "%<(mymbox{from})%|%(addr{from})%>")
      (goto-char (point-min))
      (if (search-forward-regexp "^\\(.+\\)$" nil t)
          (progn
            (setq sender (match-string 0))
            (mh-spamassassin-add-rule "blacklist_from" sender)
            (message "Blocklisting sender of message %d...done" msg))
        (message "Blocklisting sender of message %d...not done (from my address)" msg)))))

;;;###mh-autoload
(defun mh-spamassassin-allowlist (msg)
  "Allowlist MSG with SpamAssassin.

The \\[mh-junk-allowlist] command adds a \"whitelist_from\" rule to
the \"~/.spamassassin/user_prefs\" file. If the \"sa-learn\" command
is available, the message is also recategorized as ham.

See `mh-spamassassin-blocklist' for more information."
  (unless mh-spamassassin-executable
    (error "Unable to find the spamassassin executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder))
        (show-buffer (get-buffer mh-show-buffer))
        from)
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (message "Removing spamassassin markup from message %d..." msg)
      (call-process mh-spamassassin-executable msg-file t nil
                    "--remove-markup")
      (if show-buffer
          (kill-buffer show-buffer))
      (write-file msg-file)
      (when mh-sa-learn-executable
        (message "Recategorizing message %d as ham with sa-learn..." msg)
        (mh-truncate-log-buffer)
        ;; Put call-process output in log buffer if we are saving it
        ;; (this happens if mh-junk-background is t).
        (with-current-buffer mh-log-buffer
          (call-process mh-sa-learn-executable msg-file mh-junk-background nil
                        "--ham" "--local" "--no-sync")))
      (message "Allowlisting sender of message %d..." msg)
      (setq from
            (car (mail-header-parse-address (mh-get-header-field "From:"))))
      (kill-buffer nil)
      (if (or (null from) (equal from ""))
          (message "Allowlisting sender of message %d...%s"
                   msg "not done (cannot identify sender)")
        (mh-spamassassin-add-rule "whitelist_from" from)
        (message "Allowlisting sender of message %d...done" msg)))))

(defun mh-spamassassin-add-rule (rule body)
  "Add a new rule to \"~/.spamassassin/user_prefs\".
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

;;;###mh-autoload
(defun mh-spamassassin-identify-spammers ()
  "Identify spammers who are repeat offenders.

This function displays a frequency count of the hosts and domains
in the \"blacklist_from\" entries from the last blank line in
\"~/.spamassassin/user_prefs\" to the end of the file. This
information can be used so that you can replace multiple
\"blacklist_from\" entries with a single wildcard entry such as:

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
            (setf (gethash (car host) domains) (1+ (if (not value) 0 value)))
            (setq host (cdr host))))))

    ;; Output
    (delete-other-windows)
    (pop-to-buffer (get-buffer-create "*MH-E Spammer Frequencies*"))
    (erase-buffer)
    (maphash (lambda (key value) ""
               (if (> value 2)
                   (insert (format "%s %s\n" key value))))
             domains)
    (sort-numeric-fields 2 (point-min) (point-max))
    (reverse-region (point-min) (point-max))
    (goto-char (point-min))))



;; Bogofilter Interface

(defvar mh-bogofilter-executable (executable-find "bogofilter"))

;;;###mh-autoload
(defun mh-bogofilter-blocklist (msg)
  "Blocklist MSG with bogofilter.

Bogofilter is a Bayesian spam filtering program. Get it from your
local distribution or from the bogofilter web site at URL
`http://bogofilter.sourceforge.net/'.

Bogofilter is taught by running:

    bogofilter -n < good-message

on every good message, and

    bogofilter -s < spam-message

on every spam message. This is called a full training; three other
training methods are described in the FAQ that is distributed with
bogofilter. Note that most Bayesian filters need 1000 to 5000 of each
type of message to start doing a good job.

To use bogofilter, add the following recipes to \".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with bogofilter.
    :0fw
    | bogofilter -3 -e -p

    :0:
    * ^X-Bogosity: Yes, tests=bogofilter
    spam/.

    :0:
    * ^X-Bogosity: Unsure, tests=bogofilter
    spam/unsure/.

If bogofilter classifies a message incorrectly, or is unsure, you can
use the MH-E commands \\[mh-junk-blocklist] and \\[mh-junk-allowlist]
to update bogofilter's training.

The \"Bogofilter FAQ\" suggests that you run the following
occasionally to shrink the database:

    bogoutil -d wordlist.db | bogoutil -l wordlist.db.new
    mv wordlist.db wordlist.db.prv
    mv wordlist.db.new wordlist.db

The \"Bogofilter tuning HOWTO\" describes how you can fine-tune Bogofilter."
  (unless mh-bogofilter-executable
    (error "Unable to find the bogofilter executable"))
  (message "Blocklisting message %d with bogofilter..." msg)
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-bogofilter-executable msg-file mh-junk-background
                    nil "-s")
      (message "Blocklisting message %d with bogofilter...done" msg))))

;;;###mh-autoload
(defun mh-bogofilter-allowlist (msg)
  "Allowlist MSG with bogofilter.

See `mh-bogofilter-blocklist' for more information."
  (unless mh-bogofilter-executable
    (error "Unable to find the bogofilter executable"))
  (message "Allowlisting message %d with bogofilter..." msg)
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-bogofilter-executable msg-file mh-junk-background
                    nil "-n")
      (message "Allowlisting message %d with bogofilter...done" msg))))



;; Spamprobe Interface

(defvar mh-spamprobe-executable (executable-find "spamprobe"))

;;;###mh-autoload
(defun mh-spamprobe-blocklist (msg)
  "Blocklist MSG with SpamProbe.

SpamProbe is a Bayesian spam filtering program. Get it from your
local distribution or from the SpamProbe web site at URL
`http://spamprobe.sourceforge.net'.

To use SpamProbe, add the following recipes to \".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with SpamProbe.
    :0
    SCORE=| spamprobe receive

    :0 wf
    | formail -I \"X-SpamProbe: $SCORE\"

    :0:
    *^X-SpamProbe: SPAM
    spam/.

If SpamProbe classifies a message incorrectly, you can use the
MH-E commands \\[mh-junk-blocklist] and \\[mh-junk-allowlist] to
update SpamProbe's training."
  (unless mh-spamprobe-executable
    (error "Unable to find the spamprobe executable"))
  (message "Blocklisting message %d with spamprobe..." msg)
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-spamprobe-executable msg-file mh-junk-background
                    nil "spam")
      (message "Blocklisting message %d with spamprobe...done" msg))))

;;;###mh-autoload
(defun mh-spamprobe-allowlist (msg)
  "Allowlist MSG with SpamProbe.

See `mh-spamprobe-blocklist' for more information."
  (unless mh-spamprobe-executable
    (error "Unable to find the spamprobe executable"))
  (message "Allowlisting message %d with spamprobe..." msg)
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-spamprobe-executable msg-file mh-junk-background
                    nil "good")
  (message "Allowlisting message %d with spamprobe...done" msg))))

(provide 'mh-junk)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-junk.el ends here
