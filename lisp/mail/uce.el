;;; uce.el --- facilitate reply to unsolicited commercial email

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: stanislav shalunov <shalunov@math.wisc.edu>
;; Created: 10 Dec 1996
;; Version: 1.0
;; Keywords: uce, unsolicited commercial email

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Code in this file provides semi-automatic means of replying to
;; UCE's you might get.  It works currently only with Rmail.  If you
;; would like to make it work with other mail readers, Rmail-specific
;; section is marked below.  If you want to play with code, would you
;; please grab the newest version from
;; http://math.wisc.edu/~shalunov/uce.el and let me know, if you would
;; like, about your changes so I can incorporate them.  I'd appreciate
;; it.

;; Function uce-reply-to-uce, if called when current message in RMAIL
;; buffer is a UCE, will setup *mail* buffer in the following way: it
;; scans full headers of message for 1) normal return address of
;; sender (From, Reply-To lines); and puts these addresses into To:
;; header, it also puts abuse@offenders.host address there 2) mailhub
;; that first saw this message; and puts address of its postmaster
;; into To: header 3) finally, it looks at Message-Id and adds
;; posmaster of that host to the list of addresses.

;; Then, we add "Errors-To: nobody@localhost" header, so that if some
;; of these addresses are not actually correct, we will never see
;; bounced mail.  Also, mail-self-blind and mail-archive-file-name
;; take no effect: the ideology is that we don't want to save junk or
;; replies to junk.

;; Then we put template into buffer (customizable message that
;; explains what has happened), customizable signature, and the
;; original message with full headers and envelope for postmasters.
;; Then buffer is left for editing.

;; The reason that function uce-reply-to-uce is Rmail dependant is
;; that we want full headers of the original message, nothing
;; stripped.  If we use normal means of inserting of the original
;; message into *mail* buffer headers like Received: (not really
;; headers, but envelope lines) will be stripped while they bear
;; valuable for us and postmasters information.  I do wish that there
;; would be some way to write this function in some portable way, but
;; I am not aware of any.

;;; Change log:

;; Dec 10, 1996 -- posted draft version to gnu.sources.emacs

;; Dec 11, 1996 -- fixed some typos, and Francesco Potorti`
;; <F.Potorti@cnuce.cnr.it> pointed out that my use of defvar was
;; weird, suggested fix, and added let form.

;; Dec 17, 1996 -- made scanning for host names little bit more clever
;; (obviously bogus stuff like localhost is now ignored).

;;; Setup:

;; put in your ~./emacs the following line:

;; (autoload 'uce-reply-to-uce "uce" "Reply to UCEs" t nil)

;; store this file (uce.el) somewhere in load-path and byte-compile it.

;;; Variables:

;; uce-message-text is template that will be inserted into buffer.  It
;; has reasonable default.  If you want to write some scarier one,
;; please do so and send it to me.  Please keep it polite.

;; uce-signature behaves just like mail-signature.  If nil, nothing is
;; inserted, if t, file ~/.signature is used, if a string, its
;; contents are inserted into buffer.

;; uce-uce-separator is line that separates your message from the UCE
;; that you enclose.

;; uce-subject-line will be used as subject of outgoing message.  If
;; nil, left blank.

;;; Code:

(require 'sendmail)
(require 'rmail)

(defvar uce-setup-hook nil
  "Hook to run after UCE rant message is composed.
This hook is run after mail-setup-hook, which is run as well.")

(defvar uce-message-text 
  "Recently, I have received an Unsolicited Commercial E-mail from you.
I do not like UCE's and I would like to inform you that sending
unsolicited messages to someone while he or she may have to pay for
reading your message may be illegal.  Anyway, it is highly annoying
and not welcome by anyone.  It is rude, after all.

If you think that this is a good way to advertise your products or
services you are mistaken.  Spamming will only make people hate you, not
buy from you.

If you have any list of people you send unsolicited commercial emails to, 
REMOVE me from such list immediately.  I suggest that you make this list 
just empty.

Note to the postmaster(s): I append the text of UCE in question to
this message, I would like to hear from you about action(s) taken.
This message has been sent to postmasters at the host that is
mentioned as original sender's host and to the postmaster whose host
was used as mail relay for this message.  If message was sent not by
your user, could you please compare time when this message was sent
(use time in Received: field of the envelope rather than Date: field)
with your sendmail logs and see what host was using your sendmail at
this moment of time.

Thank you."

  "This is the text that uce-reply-to-uce command will put in reply buffer.
Some of spamming programs in use will be set up to read all incoming
to spam address email, and will remove people who put the word `remove'
on beginning of some line from the spamming list.  So, when you set it
up, it might be a good idea to actually use this feature.

Value nil means insert no text by default, lets you type it in.")

(defvar uce-uce-separator
  "----- original unsolicited commercial email follows -----"
  "Line that will begin quoting of the UCE.
Value nil means use no separator.")

(defvar uce-signature mail-signature
"Text to put as your signature after the note to UCE sender.  
Value nil means none, t means insert ~/.signature file (if it happens
to exist), if this variable is a string this string will be inserted
as your signature.")

(defvar uce-default-headers
  "Errors-To: nobody@localhost\nPrecedence: bulk\n"
  "Additional headers to use when responding to a UCE with \\[uce-reply-to-uce].
These are mostly meant for headers that prevent delivery errors reporting.")

(defvar uce-subject-line
  "Spam alert: unsolicited commercial e-mail"
  "Subject of the message that will be sent in response to a UCE.")

(defun uce-reply-to-uce (&optional ignored)
  "Send reply to UCE in Rmail.
UCE stands for unsolicited commercial email.  Function will set up reply
buffer with default To: to the sender, his postmaster, his abuse@
address, and postmaster of the mail relay used."
  (interactive "P")
  (let ((to (mail-strip-quoted-names (mail-fetch-field "from" t)))
	(reply-to (mail-fetch-field "reply-to"))
	temp)
    ;; Initial setting of the list of recipients of our message; that's
    ;; what they are pretending to be (and in many cases, really are).
    (if to
	(setq to (format "%s" (mail-strip-quoted-names to)))
      (setq to ""))
    (if reply-to
	(setq to (format "%s, %s" to (mail-strip-quoted-names reply-to))))
    (let (first-at-sign end-of-hostname sender-host)
      (setq first-at-sign (string-match "@" to)
	    end-of-hostname (string-match "[ ,>]" to first-at-sign)
	    sender-host (substring to first-at-sign end-of-hostname))
      (if (string-match "\\." sender-host)
          (setq to (format "%s, postmaster%s, abuse%s" 
                           to sender-host sender-host))))
    (setq mail-send-actions nil)
    (setq mail-reply-buffer nil)
    ;; Begin of Rmail dependant section.
    (or (get-buffer "RMAIL")
	(error "No buffer RMAIL, cannot find UCE"))
    (switch-to-buffer "RMAIL")
    (save-excursion
      (save-restriction
	(widen)
	(rmail-maybe-set-message-counters)
	(copy-region-as-kill (rmail-msgbeg rmail-current-message) 
			     (rmail-msgend rmail-current-message))))
    (switch-to-buffer "*mail*")
    (erase-buffer)
    (setq temp (point))
    (yank)
    (goto-char temp)
    (forward-line 2)
    (while (looking-at "Summary-Line:\\|Mail-From:")
      (forward-line 1))
    (delete-region temp (point))
    ;; Now find the mail hub that first accepted this message.
    (while (or (looking-at "Received:")
               (looking-at " ")
               (looking-at "\t"))
      (forward-line 1))
    (while (or (looking-at " ")
               (looking-at "\t"))
      (forward-line -1))
    ;; Is this always good?  It's the only thing I saw when I checked
    ;; a few messages.
    (search-forward ": from ")
    (setq temp (point))
    (search-forward " ")
    (forward-char -1)
    ;; And add its postmaster to the list of addresses.
    (if (string-match "\\." (buffer-substring temp (point)))
        (setq to (format "%s, postmaster@%s" 
                         to (buffer-substring temp (point)))))
    ;; Also look at the message-id, it helps *very* often.
    (search-forward "\nMessage-Id: ")
    (search-forward "@")
    (setq temp (point))
    (search-forward ">")
    (forward-char -1)
    (if (string-match "\\." (buffer-substring temp (point)))
        (setq to (format "%s, postmaster@%s" 
                         to (buffer-substring temp (point)))))
    (search-forward "\n*** EOOH ***\n")
    (forward-line -1)
    (setq temp (point))
    (search-forward "\n\n" nil t)
    (delete-region temp (point))
    ;; End of Rmail dependent section.
    (auto-save-mode auto-save-default)
    (mail-mode)
    (goto-char (point-min))
    (insert "To: ")
    (save-excursion
      (if to
	  (let ((fill-prefix "\t")
		(address-start (point)))
	    (insert to "\n")
	    (fill-region-as-paragraph address-start (point)))
	(newline))
      (insert "Subject: " uce-subject-line "\n")
      (if uce-default-headers
	  (insert uce-default-headers))
      (if mail-default-headers
          (insert mail-default-headers))
      (if mail-default-reply-to
	  (insert "Reply-to: " mail-default-reply-to "\n"))
      (insert mail-header-separator "\n")
      ;; Insert all our text.  Then go back to the place where we started.
      (if to (setq to (point)))
      ;; Text of ranting.
      (if uce-message-text
	  (insert uce-message-text))
      ;; Signature.
      (cond ((eq uce-signature t)
	     (if (file-exists-p "~/.signature")
		 (progn
		   (insert "\n\n-- \n")
		   (insert-file "~/.signature")
		   ;; Function insert-file leaves point where it was,
		   ;; while we want to place signature in the ``middle''
		   ;; of the message.
		   (exchange-point-and-mark))))
	    (uce-signature
	     (insert "\n\n-- \n" uce-signature)))
      ;; And text of the original message.
      (if uce-uce-separator
	  (insert "\n\n" uce-uce-separator "\n"))
      ;; If message doesn't end with a newline, insert it.
      (goto-char (point-max))
      (or (bolp) (newline)))
    ;; And go back to the beginning of text.
    (if to (goto-char to))
    (or to (set-buffer-modified-p nil))
    ;; Run hooks before we leave buffer for editing.  Reasonable usage
    ;; might be to set up special key bindings, replace standart
    ;; functions in mail-mode, etc.
    (run-hooks 'mail-setup-hook 'uce-setup-hook)))
  
(defun uce-insert-ranting (&optional ignored)
  "Insert text of the usual reply to UCE into current buffer."
  (interactive "P")
  (insert uce-message-text))

(provide 'uce)

;;; uce.el ends here
