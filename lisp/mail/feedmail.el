;;; feedmail.el --- assist other email packages to massage outgoing messages
;;; This file is in the public domain.

;; This file is part of GNU Emacs.

;; Author: Bill Carpenter <bill@bubblegum.net>, <bill@carpenter.ORG>
;; Version: 8
;; Keywords: email, queue, mail, sendmail, message, spray, smtp, draft
;; X-URL: <URL:http://www.carpenter.org/feedmail/feedmail.html>

;;; Commentary:

;; A replacement for parts of Emacs' sendmail.el (specifically,
;; it's what handles your outgoing mail after you hit C-c C-c in mail
;; mode).  See below for a list of additional features, including the
;; ability to queue messages for later sending.  If you are using
;; fakemail as a subprocess, you can switch to feedmail and eliminate
;; the use of fakemail.  feedmail works with recent versions of
;; Emacs (mostly, but not exclusively, tested against 19.34 on
;; Win95; some testing on 20.x) and XEmacs (tested with 20.4 and
;; later betas).  It probably no longer works with Emacs 18,
;; though I haven't tried that in a long time.  Sorry, no manual yet
;; in this release.  Look for one with the next release.

;; As far as I'm concerned, anyone can do anything they want with
;; this specific piece of code.  No warranty or promise of support is
;; offered.  This code is hereby released into the public domain.

;; Thanks: My thanks to the many people who have sent me suggestions
;;    and fixes over time, as well as those who have tested many beta
;;    iterations.  Some are cited in comments in code fragments below,
;;    but that doesn't correlate well with the list of folks who have
;;    actually helped me along the way.

;; If you use feedmail, I invite you to send me some email about it.
;; I appreciate feedback about problems you find or suggestions for
;; improvements or added features (even though I can't predict when
;; I'll incorporate changes).  It's also OK with me if you send me a
;; note along the lines of "I use feedmail and find it useful" or "I
;; tried feedmail and didn't find it useful, so I stopped using it".
;;
;; It is most useful, when sending a bug report, if you tell me what
;; version of Emacs you are using, what version of feedmail you are
;; using, and what versions of other email-related elisp packages you
;; are using.  If in doubt about any of that, send the bug report
;; anyhow.
;;
;; =====
;; A NOTE TO THOSE WHO WOULD CHANGE THIS CODE...  Since it is PD,
;; you're within your rights to do whatever you want.  If you do
;; publish a new version with your changes in it, please (1) insert
;; lisp comments describing the changes, (2) insert lisp comments
;; that clearly delimit where your changes are, (3) email me a copy
;; (I can't always consistently follow the relevant usenet groups),
;; and (4) use a version number that is based on the version you're
;; changing along with something that indicates you changed it.  For
;; example,
;;
;;        (defconst feedmail-patch-level "123")
;;        (defconst feedmail-patch-level "123-XYZ-mods")
;;
;; The point of the last item, of course, is to try to minimize
;; confusion.  Odds are good that if your idea makes sense to me that
;; it will show up in some future version of feedmail, though it's
;; hard to say when releases will tumble out.
;; =====
;;
;; This file requires the mail-utils library.
;;
;; This file requires the smtpmail library if you use
;; feedmail-buffer-to-smtpmail.
;;
;; This file requires the custom library.  Unfortunately, there are
;; two incompatible versions of the custom library.  If you don't have
;; custom or you have the old version, this file will still load and
;; work properly.  If you don't know what custom is all about and want
;; to edit your user option elisp variables the old fashioned way,
;; just imagine that all the "defcustom" stuff you see below is really
;; "defvar", and ignore everthing else.  For info about custom, see
;; <URL:http://www.dina.kvl.dk/~abraham/custom/>.
;;
;; This code does in elisp a superset of the stuff that used to be done
;; by the separate program "fakemail" for processing outbound email.
;; In other words, it takes over after you hit "C-c C-c" in mail mode.
;; By appropriate setting of options, you can still use "fakemail",
;; or you can even revert to sendmail (which is not too popular
;; locally).  See the variables at the top of the elisp for how to
;; achieve these effects (there are more features than in this bullet
;; list, so trolling through the variable and function doc strings may
;; be worth your while):
;;
;;    --- you can park outgoing messages into a disk-based queue and
;;        stimulate sending them all later (handy for laptop users);
;;        there is also a queue for draft messages
;;
;;    --- you can get one last look at the prepped outbound message and
;;        be prompted for confirmation
;;
;;    --- removes Bcc:/Resent-Bcc: headers after getting address info
;;
;;    --- does smart filling of address headers
;;
;;    --- calls a routine to process Fcc: lines and removes them
;;
;;    --- empty headers are removed
;;
;;    --- can force From: or Sender: line
;;
;;    --- can generate a Message-Id: line
;;
;;    --- can generate a Date: line; the date can be the time the
;;        message was written or the time it is being sent
;;
;;    --- strips comments from address info (both "()" and "<>" are
;;        handled via a call to mail-strip-quoted-names); the
;;        comments are stripped in the simplified address list given
;;        to a subprocess, not in the headers in the mail itself
;;        (they are left unchanged, modulo smart filling)
;;
;;    --- error info is pumped into a normal buffer instead of the
;;        minibuffer
;;
;;    --- just before the optional prompt for confirmation, lets you
;;        run a hook on the prepped message and simplified address
;;        list
;;
;;    --- you can specify something other than /bin/mail for the
;;        subprocess
;;
;;    --- you can generate/modify an X-Mailer: message header
;;
;; After a long list of options below, you will find the function
;; feedmail-send-it. Hers's the best way to use the stuff in this
;; file:
;;
;; Save this file as feedmail.el somewhere on your elisp
;; loadpath; byte-compile it.  Put the following lines somewhere in
;; your ~/.emacs stuff:
;;
;;     (setq send-mail-function 'feedmail-send-it)
;;     (autoload 'feedmail-send-it "feedmail")
;;
;; If you plan to use the queue stuff, also use this:
;;
;;     (setq feedmail-enable-queue t)
;;     (autoload 'feedmail-run-the-queue "feedmail")
;;     (autoload 'feedmail-run-the-queue-no-prompts "feedmail")
;;     (setq auto-mode-alist (cons '("\\.fqm$" . mail-mode) auto-mode-alist))
;;
;; If you are using the desktop.el library to restore your sessions, you might
;; like to add the suffix ".fqm" to the list of non-saved things via the variable
;; desktop-files-not-to-save.
;;
;; If you are planning to call feedmail-queue-reminder from your .emacs or
;; something similar, you might need this:
;;
;;     (autoload 'feedmail-queue-reminder "feedmail")
;;
;; If you ever use rmail-resend and queue messages, you should do this:
;;
;;     (setq feedmail-queue-alternative-mail-header-separator "")
;;
;; If you want to automatically spell-check messages, but not when sending
;; them from the queue, you could do something like this:
;;
;;     (autoload 'feedmail-mail-send-hook-splitter "feedmail")
;;     (add-hook 'mail-send-hook 'feedmail-mail-send-hook-splitter)
;;     (add-hook 'feedmail-mail-send-hook 'ispell-message)
;;
;; If you are using message-mode to compose and send mail, feedmail will
;; probably work fine with that (someone else tested it and told me it worked).
;; Follow the directions above, but make these adjustments instead:
;;
;;     (setq message-send-mail-function 'feedmail-send-it)
;;     (add-hook 'message-mail-send-hook 'feedmail-mail-send-hook-splitter)
;;
;; I think the LCD is no longer being updated, but if it were, this
;; would be a proper LCD record.  There is an old version of
;; feedmail.el in the LCD archive.  It works but is missing a lot of
;; features.
;;
;; LCD record:
;; feedmail|Bill Carpenter|bill@bubblegum.net,bill@carpenter.ORG|Outbound mail queue handling|98-06-15|8|feedmail.el
;;
;; Change log:
;; original,      31 March 1991
;; patchlevel 1,   5 April 1991
;; patchlevel 2,  24 May   1991
;; 5-may-92  jwz	Conditionalized calling expand-mail-aliases, since that
;;			function doesn't exist in Lucid Emacs or when using
;;			mail-abbrevs.el.
;; patchlevel 3,   3 October 1996
;;         added queue stuff; still works in v18
;; patchlevel 4, issued by someone else
;; patchlevel 5, issued by someone else
;; patchlevel 6, not issued as far as I know
;; patchlevel 7,  20 May 1997
;;         abandon futile support of Emacs 18 (sorry if that hurts you)
;;         provide a Date: header by default
;;         provide a default for generating Message-Id: header contents
;;            and use it by default (slightly changed API)
;;         return value from feedmail-run-the-queue
;;         new wrapper function feedmail-run-the-queue-no-prompts
;;         user-mail-address as default for From:
;;         properly deal with Resent-{To,Cc,Bcc}
;;         Bcc and Resent-* now included in smart filling
;;         limited support for a "drafts" directory
;;         user-configurable default message action
;;         allow timeout for confirmation prompt (where available)
;;         move Fcc handling to as late as possible to get max
;;            header munging in the saved file
;;         work around sendmail.el's prompts when working from queue
;;         more reliably detect voluntary user bailouts
;;         offer to save modified buffers visiting queue files
;;         offer to delete old file copies of messages being queued
;;         offer to delete queue files when sending immediately
;;         queue filename convention preserves queue order
;;         default queue and draft directory names that work on VMS
;;         deduced address list now really a list, not a string (API change)
;;         no more address buffer
;;         when sending immediately, brief reminder of queue/draft counts
;;         copy trace of smtpmail stuff to feedmail error buffer on no-go
;;         more granularity on when to confirm sending
;;         pause a bit for errors while running queue
;;         try to clean up some pesky auto-save files from the
;;            queue/draft directories
;;         feedmail-force-expand-mail-aliases in case you can't figure
;;            any other way
;;         cleanup some of my sloppiness about case-fold-search (a strange
;;            variable)
;;         best effort following coding conventions from Emacs
;;            elisp manual appendix
;;         "customize" (see custom.el)
;;         when user selects "immediate send", clear action prompt since
;;            hooks may take a while to operate, and user may think the
;;            response didn't take
;;         fixes to the argument conventions for the
;;            feedmail-queue-runner-* functions; allows
;;            feedmail-run-the-queue[-no-prompts] to properly be called
;;            non-interactively
;;         eliminate reliance on directory-sep-char and feedmail-sep-thing
;;         tweak smart filling (reminded of comma problem by levitte@lp.se)
;;         option to control writing in text vs binary mode
;; patchlevel 8, 15 June 1998
;;         reliable re-editing of text-mode (vs binary) queued messages
;;         user option to keep Bcc: in Fcc: copy (keep by default)
;;         user option to delete body from Fcc: copy (keep by default)
;;         feedmail-deduce-bcc-where for envelope (API change for
;;           feedmail-deduce-address list)
;;         feedmail-queue-alternative-mail-header-separator
;;         at message action prompt, "I"/"S" bypass message confirmation prompt
;;         feedmail-mail-send-hook-splitter, feedmail-mail-send-hook,
;;           feedmail-mail-send-hook-queued
;;         user can supply stuff for message action prompt
;;         variable feedmail-queue-runner-confirm-global, function feedmail-run-the-queue-global-prompt
;;         bugfix: absolute argument to directory-files (tracked down for me
;;           by gray@austin.apc.slb.com (Douglas Gray Stephens)); relative
;;           pathnames can tickle stuff in ange-ftp remote directories
;;           (perhaps because feedmail is careless about its working
;;           directory)
;;         feedmail-deduce-envelope-from
;;         always supply envelope "from" (user-mail-address) to sendmail
;;         feedmail-message-id-suffix
;;         feedmail-queue-reminder, feedmail-queue-reminder-alist (after suggestions
;;           and/or code fragments from tonyl@Eng.Sun.COM (Tony Lam) and
;;           burge@newvision.com (Shane Burgess); bumped up the default value of
;;           feedmail-queue-chatty-sit-for since info is more complex sometimes
;;         feedmail-enable-spray (individual transmissions, crude mailmerge)
;;         blank Subject: no longer a special case; see feedmail-nuke-empty-headers
;;         fiddle-plexes data structure used lots of places; see feedmail-fiddle-plex-blurb
;;         feedmail-fiddle-plex-user-list
;;         feedmail-is-a-resend
;;         honor mail-from-style in constructing default for feedmail-from-line
;;         re-implement feedmail-from-line and feedmail-sender-line with
;;           fiddle-plexes; slightly modified semantics for feedmail-sender-line
;;         feedmail-queue-default-file-slug; tidy up some other slug details
;;         feedmail-queue-auto-file-nuke
;;         feedmail-queue-express-to-queue and feedmail-queue-express-to-draft
;;         strong versions of "q"ueue and "d"raft answers (always make a new file)
;;
;; todo (probably in patchlevel 9):
;;         write texinfo manual
;;         maybe partition into multiple files, including files of examples
;;
;;; Code:

(defconst feedmail-patch-level "8")


;; from <URL:http://www.dina.kvl.dk/~abraham/custom/>:
;; If you write software that must work without the new custom, you
;; can use this hack stolen from w3-cus.el:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(eval-when-compile (require 'smtpmail))
(autoload 'mail-do-fcc "sendmail")

(defgroup feedmail nil
  "Assist other email packages to massage outgoing messages."
  :link '(url-link "http://www.carpenter.org/feedmail/feedmail.html")
  :link '(emacs-commentary-link "feedmail")
  :group 'mail)

(defgroup feedmail-misc nil
  "Miscellaneous feedmail options that don't fit in other groups."
  :group 'feedmail)

(defgroup feedmail-headers nil
  "Options related to manipulating specific headers or types of headers."
  :group 'feedmail)

(defgroup feedmail-queue nil
  "Options related to queuing messages for later sending."
  :group 'feedmail)


(defcustom feedmail-confirm-outgoing nil
  "*If non-nil, give a y-or-n confirmation prompt before sending mail.
This is done after the message is completely prepped, and you'll be
looking at the top of the message in a buffer when you get the prompt.
If set to the symbol 'queued, give the confirmation prompt only while
running the queue (however, the prompt is always suppressed if you are
processing the queue via feedmail-run-the-queue-no-prompts).  If set
to the symbol 'immediate, give the confirmation prompt only when
sending immediately.  For any other non-nil value, prompt in both
cases.  You can give a timeout for the prompt; see variable
feedmail-confirm-outgoing-timeout."
  :group 'feedmail-misc
  :type 'boolean
  )


(defcustom feedmail-confirm-outgoing-timeout nil
  "*If non-nil, a timeout in seconds at the send confirmation prompt.
If a positive number, it's a timeout before sending.  If a negative
number, it's a timeout before not sending.  This will not work if your
version of Emacs doesn't include the function y-or-n-p-with-timeout
\(e.g., some versions of XEmacs\)."
  :group 'feedmail-misc
  :type '(choice (const nil) integer)
  )


(defcustom feedmail-nuke-bcc t
  "*If non-nil remove Bcc: lines from the message headers.
In any case, the Bcc: lines do participate in the composed address
list.  You may want to leave them in if you're using sendmail
\(see feedmail-buffer-eating-function\)."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-nuke-resent-bcc t
  "*If non-nil remove Resent-Bcc: lines from the message headers.
In any case, the Resent-Bcc: lines do participate in the composed
address list.  You may want to leave them in if you're using sendmail
\(see feedmail-buffer-eating-function\)."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-deduce-bcc-where nil
  "*Where Bcc:/Resent-Bcc: addresses should appear in the envelope list.
Addresses for the message envelope are deduced by examining
appropriate address headers in the message.  Generally, they will show
up in the list of deduced addresses in the order that the headers
happen to appear (duplicate addresses are eliminated in any case).
This variable can be set to the symbol 'first, in which case the
Bcc:/Resent-Bcc: addresses will appear at the beginning in the list;
or, it can be set to the symbol 'last, in which case they will appear
at the end of the list.

Why should you care?  Well, maybe you don't, and certainly the same
things could be accomplished by affecting the order of message headers
in the outgoing message.  Some people use Bcc: as a way of getting
their own \"come back\" copy of each message they send.  If Bcc:
addresses are not handled first, there can be substantial delays in
seeing the message again.  Some configurations of sendmail, for example,
seem to try to deliver to each addressee at least once, immediately
and serially, so slow SMTP conversations can add up to a delay.  There
is an option for either 'first or 'last because you might have a
delivery agent that processes the addresses backwards."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-fill-to-cc t
  "*If non-nil do smart filling of addressee header lines.
Smart filling means breaking long lines at appropriate points and
making continuation lines.  Despite the function name, it includes
To:, Cc:, Bcc: (and their Resent-* forms), as well as From: and
Reply-To: (though they seldom need it).  If nil, the lines are left
as-is.  The filling is done after mail address alias expansion."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-fill-to-cc-fill-column default-fill-column
  "*Fill column used by feedmail-fill-to-cc."
  :group 'feedmail-headers
  :type 'integer
  )


(defcustom feedmail-nuke-bcc-in-fcc nil
  "*If non-nil remove [Resent-]Bcc: lines in message copies saved via Fcc:.
This is independent of whether the Bcc: header lines are actually sent
with the message (see feedmail-nuke-bcc).  Though not implied in the name,
the same Fcc: treatment applies to both Bcc: and Resent-Bcc: lines."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-nuke-body-in-fcc nil
  "*If non-nil remove body of message in copies saved via Fcc:.
If an positive integer value, leave (up to) that many lines of the
beginning of the body intact.  The result is that the Fcc: copy will
consist only of the message headers, serving as a sort of an outgoing
message log."
  :group 'feedmail-headers
  :type '(choice (const nil) (const t) integer)
  )


(defcustom feedmail-force-expand-mail-aliases nil
  "*If non-nil, force the calling of `expand-mail-aliases'.
Normally, feedmail tries to figure out if you're using mailalias or
mailabbrevs and only calls `expand-mail-aliases' if it thinks you're
using the mailalias package.  This user option can be used to force
the issue since there are configurations which fool the figuring
out."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-nuke-empty-headers t
  "*If non-nil, remove header lines which have no contents.
A completely empty Subject: header is always removed, regardless of
the setting of this variable.  The only time you would want them left
in would be if you used some headers whose presence indicated
something rather than their contents.  This is rare in Internet email
but common in some proprietary systems."
  :group 'feedmail-headers
  :type 'boolean
  )

;; wjc sez:  I think the use of the Sender: line is pretty pointless,
;; but I left it in to be compatible with sendmail.el and because
;; maybe some distant mail system needs it.  Really, though, if you
;; want a sender line in your mail, just put one in there and don't
;; wait for feedmail to do it for you.  (Yes, I know all about
;; RFC-822 and RFC-1123, but are you *really* one of those cases
;; they're talking about?  I doubt it.)
(defcustom feedmail-sender-line nil
  "*If non-nil and the email has no Sender: header, use this value.
May be nil, in which case nothing in particular is done with respect
to Sender: lines.  By design, will not replace an existing Sender:
line, but you can achieve that with a fiddle-plex 'replace action.
NB: it makes no sense to use the value t since there is no sensible
default for Sender:.

If not nil, it may be a string, a fiddle-plex, or a function which
returns either nil, t, a string, or a fiddle-plex (or, in fact,
another function, but let's not be ridiculous).  If a string, it
should be just the contents of the header, not the name of the header
itself nor the trailing newline.  If a function, it will be called
with no arguments.  For an explanation of fiddle-plexes, see the
documentation for the variable feedmail-fiddle-plex-blurb.  In all
cases the name element of the fiddle-plex is ignored and is hardwired
by feedmail to either \"X-Sender\" or \"X-Resent-Sender\".

You can probably leave this nil, but if you feel like using it, a good
value would be a string of a fully-qualified domain name form of your
address.  For example, \"bill@bubblegum.net (WJCarpenter)\".  The Sender:
header is fiddled after the From: header is fiddled."
  :group 'feedmail-headers
  :type '(choice (const nil) string)
  )


(defcustom feedmail-force-binary-write t
  "*If non-nil, force writing file as binary (this applies to queues and Fcc:).
On systems where there is a difference between binary and text files,
feedmail will temporarily manipulate the values of `buffer-file-type'
and/or default-buffer-file-type to make the writing as binary.  If
nil, writing will be in text mode.  On systems where there is no
distinction or where it is controlled by other variables or other
means, this option has no effect."
  :group 'feedmail-misc
  :type 'boolean
  )


(defcustom feedmail-from-line t
  "*If non-nil and the email has no From: header, use this value.
May be t, in which case a default is computed (and you probably won't
be happy with it).  May be nil, in which case nothing in particular is
done with respect to From: lines.  By design, will not replace an
existing From: line, but you can achieve that with a fiddle-plex 'replace
action.

If neither nil nor t, it may be a string, a fiddle-plex, or a function
which returns either nil, t, a string, or a fiddle-plex (or, in fact,
another function, but let's not be ridiculous).  If a string, it
should be just the contents of the header, not the name of the header
itself nor the trailing newline.  If a function, it will be called
with no arguments.  For an explanation of fiddle-plexes, see the
documentation for the variable feedmail-fiddle-plex-blurb.  In all
cases the name element of the fiddle-plex is ignored and is hardwired
by feedmail to either \"X-From\" or \"X-Resent-From\".

A good value would be a string fully-qualified domain name form of
your address.  For example, \"bill@bubblegum.net (WJCarpenter)\".  The
default value of this variable uses the standard elisp variable
`user-mail-address' which should be set on every system but has a decent
chance of being wrong.  It also honors `mail-from-style'.  Better to set
this variable explicitly to the string you want or find some other way
to arrange for the message to get a From: line."
  :group 'feedmail-headers
  :type '(choice (const t) (const nil) string)
  )


(defcustom feedmail-deduce-envelope-from t
  "*If non-nil, deduce message envelope \"from\" from header From: or Sender:.
In other words, if there is a Sender: header in the message, temporarily
change the value of `user-mail-address' to be the same while the message
is being sent.  If there is no Sender: header, use the From: header,
if any.  Address values are taken from the actual message just before
it is sent, and the process is independent of the values of
feedmail-from-line and/or feedmail-sender-line.

There are many and good reasons for having the message header
From:/Sender: be different from the message envelope \"from\"
information.  However, for most people and for most circumstances, it
is usual for them to be the same (this is probably especially true for
the case where the user doesn't understand the difference between the
two in the first place).

The idea behind this feature is that you can have everything set up
some normal way for yourself.  If for some reason you want to send a
message with another From: line, you can just type it at the top of
the message, and feedmail will take care of \"fixing up\" the envelope
\"from\".  This only works for mail senders which make use of
`user-mail-address' as the envelope \"from\" value.  For some mail
senders (e.g., feedmail-buffer-to-bin-mail), there is no simple way to
influence what they will use as the envelope."
  :group 'feedmail-headers
  :type 'boolean
  )


(defcustom feedmail-x-mailer-line-user-appendage nil
  "*See feedmail-x-mailer-line."
  :group 'feedmail-headers
  :type '(choice (const nil) (const t) string)
  )


(defcustom feedmail-x-mailer-line t
  "*Control the form of an X-Mailer: header in an outgoing message.
Moderately useful for debugging, keeping track of your correspondents'
mailer preferences, or just wearing your MUA on your sleeve.  You
should probably know that some people are fairly emotional about the
presence of X-Mailer: lines in email.

If nil, nothing is done about X-Mailer:.

If t, an X-Mailer: header of a predetermined format is produced,
combining its efforts with any existing X-Mailer: header.  If you want
to take the default construct and just add a little blob of your own
at the end, define the variable feedmail-x-mailer-line-user-appendage
as that blob string.  A value of t is equivalent to using the function
feedmail-default-x-mailer-generator.

If neither nil nor t, it may be a string, a fiddle-plex, or a function
which returns either nil, t, a string, or a fiddle-plex (or, in fact,
another function, but let's not be ridiculous).  If a string, it
should be just the contents of the header, not the name of the header
itself nor the trailing newline.  If a function, it will be called
with no arguments.  For an explanation of fiddle-plexes, see the
documentation for the variable feedmail-fiddle-plex-blurb.  In all
cases the name element of the fiddle-plex is ignored and is hardwired
by feedmail to either \"X-Mailer\" or \"X-Resent-Mailer\"."
  :group 'feedmail-headers
  :type '(choice (const t) (const nil) string function)
  )


(defcustom feedmail-message-id-generator t
  "*Specifies the creation of a Message-Id: header field.

If nil, nothing is done about Message-Id:.

If t, a Message-Id: header of a predetermined format is produced, but
only if there is not already a Message-Id: in the message.  A value of
t is equivalent to using the function feedmail-default-message-id-generator.

If neither nil nor t, it may be a string, a fiddle-plex, or a function
which returns either nil, t, a string, or a fiddle-plex (or, in fact,
another function, but let's not be ridiculous).  If a string, it
should be just the contents of the header, not the name of the header
itself nor the trailing newline.  If a function, it will be called
with one argument: the possibly-nil name of the file associated with
the message buffer.  For an explanation of fiddle-plexes, see the
documentation for the variable feedmail-fiddle-plex-blurb.  In all
cases the name element of the fiddle-plex is ignored and is hardwired
by feedmail to either \"Message-Id\" or \"Resent-Message-Id\".

You should let feedmail generate a Message-Id: for you unless you are sure
that whatever you give your messages to will do it for you (e.g., most
configurations of sendmail).  Even if the latter case is true, it
probably won't hurt you to generate your own, and it will then show up
in the saved message if you use Fcc:."
  :group 'feedmail-headers
  :type '(choice (const nil) function)
  )


(defcustom feedmail-message-id-suffix nil
  "*If non-nil, used as a suffix for generating unique Message-Id: headers.
The function `feedmail-default-message-id-generator' creates its work based
on a formatted date-time string, a random number, and a domain-looking suffix.
You can control the suffix used by assigning a string value to this variable.
If you don't supply one, the value of the variable `user-mail-address' will be
used.  If the value of `feedmail-message-id-suffix' contains an \"@\" character,
the string will be used verbatim, else an \"@\" character will be prepended
automatically."
  :group 'feedmail-headers
  :type '(choice (const nil) string)
  )

;; this was suggested in various forms by several people; first was
;; Tony DeSimone in Oct 1992; sorry to be so tardy
(defcustom feedmail-date-generator t
  "*Specifies the creation of a Date: header field.

If nil, nothing is done about Date:.

If t, a Date: header of a predetermined format is produced, but only
if there is not already a Date: in the message.  A value of t is
equivalent to using the function feedmail-default-date-generator.

If neither nil nor t, it may be a string, a fiddle-plex, or a function
which returns either nil, t, a string, or a fiddle-plex (or, in fact,
another function, but let's not be ridiculous).  If a string, it
should be just the contents of the header, not the name of the header
itself nor the trailing newline.  If a function, it will be called
with one argument: the possibly-nil name of the file associated with
the message buffer.  For an explanation of fiddle-plexes, see the
documentation for the variable feedmail-fiddle-plex-blurb.  In all
cases the name element of the fiddle-plex is ignored and is hardwired
by feedmail to either \"Date\" or \"Resent-Date\".

If you decide to format your own date field, do us all a favor and know
what you're doing.  Study the relevant parts of RFC-822 and RFC-1123.
Don't make me come up there!

You should let feedmail generate a Date: for you unless you are sure
that whatever you give your messages to will do it for you (e.g., most
configurations of sendmail).  Even if the latter case is true, it
probably won't hurt you to generate your own, and it will then show up
in the saved message if you use Fcc:."
  :group 'feedmail-headers
  :type '(choice (const nil) function)
  )


(defcustom feedmail-fiddle-headers-upwardly t
  "*Non-nil means fiddled header fields should go at the top of the header.
Nil means insert them at the bottom.  This is mostly a novelty issue since
the standards define the ordering of header fields to be immaterial and it's
fairly likely that some MTA along the way will have its own idea of what the
order should be, regardless of what you specify."
  :group 'feedmail-header
  :type 'boolean
  )


(defcustom feedmail-fiddle-plex-user-list nil
  "If non-nil, should be a list of one or more fiddle-plexes.
Each element of the list can also be a function which returns a
fiddle-plex.

feedmail will use this list of fiddle-plexes to manipulate user-specified
message header fields.  It does this after it has completed all normal
message header field manipulation and before calling feedmail-last-chance-hook.

For an explanation of fiddle-plexes, see the documentation for the
variable feedmail-fiddle-plex-blurb.  In contrast to some other fiddle-plex
manipulation functions, in this context, it makes no sense to have an element
which is nil, t, or a simple string."
  :group 'feedmail-header
  :type '(repeat (choice function)
		 sexp) ; too complex to be described accurately
  )


(defcustom feedmail-enable-spray nil
  "If non-nil, transmit message separately to each addressee.
feedmail normally accumulates a list of addressees and passes the message
along with that list to a buffer-eating function which expects any number
of addressees.  If this variable is non-nil, however, feedmail will
repeatedly call the same buffer-eating function.  Each time, the list of
addressees will be just one item from the original list.  This only affects
the message envelope addresses and doesn't affect what appears in the
message headers except as noted.

Spray mode is usually pointless, and if you can't think of a good reason for
it, you should avoid it since it is inherently less efficient than normal
multiple delivery.  One reason to use it is to overcome mis-featured mail
transports which betray your trust by revealing Bcc: addressees in the
headers of a message.  Another use is to do a crude form of mailmerge, for
which see feedmail-spray-address-fiddle-plex-list.

If one of the calls to the buffer-eating function results in an error,
what happens next is carelessly defined, so beware."
  :group 'feedmail-spray
  :type 'boolean
  )

(defvar feedmail-spray-this-address nil
  "Do not set or change this variable.  See feedmail-spray-address-fiddle-plex-list.")

(defcustom feedmail-spray-address-fiddle-plex-list nil
  "User-supplied specification for a crude form of mailmerge capability.
When spraying is enabled, feedmail composes a list of envelope addresses.
In turn, feedmail-spray-this-address is temporarily set to each address
\(stripped of any comments and angle brackets\) and calls a function which
fiddles message headers according to this variable.  See the documentation for
`feedmail-fiddle-plex-blurb', for an overview of fiddle-plex data structures.

May be nil, in which case nothing in particular is done about message
headers for specific addresses.

May be t, in which case a \"To:\" header is added to the message with
the stripped address as the header contents.  The fiddle-plex operator
is 'supplement.

May be a string, in which case the string is assumed to be the name of
a message header field with the stripped address serving as the value.
The fiddle-plex operator is 'supplement.

May be a function, in which case it is called with no arguments and is
expected to return nil, t, a string, another function, or a fiddle-plex.
The result is used recursively.

May be a list of any combination of the foregoing and fiddle-plexes.  (A
value for this variable which consists of a single fiddle-plex must be
nested inside another list to avoid ambiguity.)  If a list, each item
is acted on in turn as described above.

For example,

  (setq feedmail-spray-address-fiddle-plex-list 'my-address-embellisher)

The idea of the example is that, during spray mode, as each message is
about to be transmitted to an individual address, the function will be
called and will consult feedmail-spray-this-address to find the
stripped envelope email address (no comments or angle brackets).  The
function should return an embellished form of the address.

The recipe for sending form letters is:  (1) create a message with all
addressees on Bcc: headers; (2) tell feedmail to remove Bcc: headers
before sending the message; (3) create a function which will embellish
stripped addresses, if desired; (4) define feedmail-spray-address-fiddle-plex-list
appropriately; (5) send the message with feedmail-enable-spray set
non-nil; (6) stand back and watch co-workers wonder at how efficient
you are at accomplishing inherently inefficient things."
  :group 'feedmail-spray
  :type 'sexp ; too complex to be described accurately
  )


(defcustom feedmail-enable-queue nil
  "*If non-nil, provide for stashing outgoing messages in a queue.
This is the master on/off switch for feedmail message queuing.
Queuing is quite handy for laptop-based users.  It's also handy if you
get a lot of mail and process it more or less sequentially.  For
example, you might change your mind about contents of a reply based on
a message you see a bit later.

There is a separate queue for draft messages, intended to prevent
you from accidentally sending incomplete messages.  The queues are
disk-based and intended for later transmission.  The messages are
queued in their raw state as they appear in the mail-mode buffer and
can be arbitrarily edited later, before sending, by visiting the
appropriate file in the queue directory (and setting the buffer to
mail-mode or whatever).  If you visit a file in the queue directory
and try to queue it again, it will just get saved in its existing file
name.  You can move a message from the draft to the main queue or vice
versa by pretending to send it and then selecting whichever queue
directory you want at the prompt.  The right thing will happen.

To transmit all the messages in the queue, invoke the command
feedmail-run-the-queue or feedmail-run-the-queue-no-prompts."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-queue-runner-confirm-global nil
  "*If non-nil, give a y-or-n confirmation prompt before running the queue.
Prompt even if the queue is about to be processed as a result of a call to
feedmail-run-the-queue-no-prompts.  This gives you a way to bail out
without having to answer no to the individual message prompts."
  :group 'feedmail-queue
  :type 'boolean)


;; I provided a default for VMS because someone asked for it (the
;; normal default doesn't work there), but, puh-lease!, it is a user
;; definable option, so if you don't like the default, change it to
;; whatever you want.  I am unable to directly test the VMS goop
;; provided here by levitte@lp.se (Richard Levitte - VMS Whacker).
(defcustom feedmail-queue-directory
  (if (memq system-type '(axp-vms vax-vms))
      (expand-file-name (concat (getenv "HOME") "[.MAIL.Q]"))
    (concat (getenv "HOME") "/mail/q"))
  "*Name of a directory where messages will be queued.
Directory will be created if necessary.  Should be a string that
doesn't end with a slash.  Default, except on VMS, is \"$HOME/mail/q\"."
  :group 'feedmail-queue
  :type 'string
  )


(defcustom feedmail-queue-draft-directory
  (if (memq system-type '(axp-vms vax-vms))
      (expand-file-name (concat (getenv "HOME") "[.MAIL.DRAFT]"))
    (concat (getenv "HOME") "/mail/draft"))
  "*Name of an directory where draft messages will be queued.
Directory will be created if necessary.  Should be a string that
doesn't end with a slash.  Default, except on VMS, is \"$HOME/mail/draft\"."
  :group 'feedmail-queue
  :type 'string
  )


(defcustom feedmail-ask-before-queue t
  "*If non-nil, feedmail will ask what you want to do with the message.
Default choices for the message action prompt will include sending it
immediately, putting it in the main queue, putting it in the draft
queue, or returning to the buffer to continue editing.  Only matters if
queuing is enabled.  If nil, the message is placed in the main queue
without a prompt."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-ask-before-queue-prompt "FQM: Message action (q, i, d, e, ?)? [%s]: "
  "*A string which will be used for the message action prompt.
If it contains a \"%s\", that will be replaced with the value of
feedmail-ask-before-queue-default."
  :group 'feedmail-queue
  :type 'string
  )


(defcustom feedmail-ask-before-queue-reprompt "FQM: Please type q, i, d, or e; or ? for help [%s]: "
  "*A string which will be used for repompting after invalid input.
If it contains a \"%s\", that will be replaced with the value of
feedmail-ask-before-queue-default."
  :group 'feedmail-queue
  :type 'string
  )


(defcustom feedmail-ask-before-queue-default "queue"
  "*Meaning if user hits return in response to the message action prompt.
Should be a character or a string; if a string, only the first
character is significant.  Useful values are those described in
the help for the message action prompt."
  :group 'feedmail-queue
  :type '(choice string integer)	;use integer to get char
  )


(defvar feedmail-prompt-before-queue-standard-alist
  '((?q . feedmail-message-action-queue)
    (?Q . feedmail-message-action-queue-strong)

    (?d . feedmail-message-action-draft)
    (?r . feedmail-message-action-draft)
    (?D . feedmail-message-action-draft-strong)
    (?R . feedmail-message-action-draft-strong)

    (?e . feedmail-message-action-edit)
    (?E . feedmail-message-action-edit)
    (?\C-g . feedmail-message-action-edit)
    (?n . feedmail-message-action-edit)
    (?N . feedmail-message-action-edit)

    (?i . feedmail-message-action-send)
    (?I . feedmail-message-action-send-strong)
    (?s . feedmail-message-action-send)
    (?S . feedmail-message-action-send-strong)

    (?* . feedmail-message-action-toggle-spray)

    (?\C-v . feedmail-message-action-help)
    (?? . feedmail-message-action-help))
  "An alist of choices for the message action prompt.
All of the values are function names, except help, which is a special
symbol that calls up help for the prompt (the help describes the
actions from the standard alist).  To customize your own choices,
define a similar alist called feedmail-prompt-before-queue-user-alist.
The actual alist used for message action will be the standard alist
overlaid with the user-alist.  To neutralize an item in the standard
alist without providing a replacement, define an appropriate element
in the user alist with a value of nil." )


(defcustom feedmail-prompt-before-queue-user-alist nil
  "See feedmail-prompt-before-queue-standard-alist."
  :group 'feedmail-queue
  :type '(repeat (cons character function))
  )


(defcustom feedmail-prompt-before-queue-help-supplement nil
  "User-provided supplementary help string for the message action prompt.
When the message action prompt is shown, the user can as for verbose help,
at which point a buffer pops up describing the meaning of possible
responses to the prompt.  Through various customizations (see, for
example, feedmail-prompt-before-queue-user-alist), the available responses
and the prompt itself can be changed.  If this variable is set to a string
value, that string is written to the help buffer after the standard info.
It may contain embedded line breaks.  It will be printed via princ."
  :group 'feedmail-queue
  :type '(choice (const nil) string)
  )


(defcustom feedmail-queue-reminder-alist
  '((after-immediate . feedmail-queue-reminder-brief)
    (after-queue . feedmail-queue-reminder-medium)
    (after-draft . feedmail-queue-reminder-medium)
    (after-run . feedmail-queue-reminder-brief)
    (on-demand . feedmail-run-the-queue-global-prompt))
  "See feedmail-queue-reminder."
  :group 'feedmail-queue
  :type '(repeat (cons (choice :tag "Event"
			       (const on-demand)
			       (const after-immediate)
			       (const after-queue)
			       (const after-draft)
			       (const after-run))
		       function))
  )


(defcustom feedmail-queue-chatty t
  "*If non-nil, blat a few status messages and such in the mini-buffer.
If nil, just do the work and don't pester people about what's going on.
In some cases, though, specific options inspire mini-buffer prompting.
That's not affected by this variable setting.  Also does not control
reporting of error/abnormal conditions."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-queue-chatty-sit-for 2
  "*Duration of pause after most queue-related messages.
After some messages are divulged, it is prudent to pause before
something else obliterates them.  This value controls the duration of
the pause."
  :group 'feedmail-queue
  :type 'integer
  )


(defcustom feedmail-queue-run-orderer nil
  "*If non-nil, name a function which will sort the queued messages.
The function is called during a running of the queue for sending, and
takes one argument, a list of the files in the queue directory.  It
may contain the names of non-message files, and it's okay to leave
them in the list when reordering it; they get skipped over later.
When nil, the default action processes the messages in normal sort
order by queued file name, which will typically result in the order
they were placed in the queue."
  :group 'feedmail-queue
  :type '(choice (const nil) function)
  )


(defcustom feedmail-queue-use-send-time-for-date nil
  "*If non-nil, use send time for the Date: header value.
This variable is used by the default date generating function,
feedmail-default-date-generator.  If nil, the default, the
last-modified timestamp of the queue file is used to create the
message Date: header; if there is no queue file, the current time is
used."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-queue-use-send-time-for-message-id nil
  "*If non-nil, use send time for the Message-Id: header value.
This variable is used by the default Message-Id: generating function,
feedmail-default-message-id-generator.  If nil, the default, the
last-modified timestamp of the queue file is used to create the
message Message-Id: header; if there is no queue file, the current time is
used."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-ask-for-queue-slug nil
  "*If non-nil, prompt user for part of the queue file name.
The file will automatically get the FQM suffix and an embedded
sequence number for uniqueness, so don't specify that.  feedmail will
get rid of all characters other than alphanumeric and hyphen in the
results.  If this variable is nil or if you just hit return in
response to the prompt, feedmail queuing will take care of things
properly.  At the prompt, completion is available if you want to see
what filenames are already in use, though, as noted, you will not be
typing a complete file name.  You probably don't want to be bothered
with this prompting since feedmail, by default, uses queue file names
based on the subjects of the messages."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-queue-slug-maker 'feedmail-queue-subject-slug-maker
  "*If non-nil, a function which creates part of the queued file name.
Takes a single argument giving the name of the directory into
which the message will be queued.  The returned string should be just
the non-directory filename part, without FQM suffix or uniquifying
sequence numbers.  The current buffer holds the raw message.  The
default function creates the slug based on the message subject, if
any."
  :group 'feedmail-queue
  :type '(choice (const nil) function)
  )


(defcustom feedmail-queue-default-file-slug t
  "*Indicates what to use for subject-less messages when forming a file name.
When feedmail queues a message, it creates a unique file name.  By default,
the file name is based in part on the subject of the message being queued.
If there is no subject, consult this variable.  See documentation for the
function feedmail-queue-subject-slug-maker.

If t, an innocuous default is used.

If a string, it is used directly.

If a function, it is called with no arguments from the buffer containing the raw
text of the message.  It must return a string (which may be empty).

If the symbol 'ask, you will be prompted for a string in the mini-buffer.
Filename completion is available so that you can inspect what's already been
used, but feedmail will do further manipulation on the string you return, so
it's not expected to be a complete filename."
  :group 'feedmail-queue
  :type '(choice (const :tag "Default" t) string function (const ask))
  )


(defcustom feedmail-queue-fqm-suffix ".fqm"
  "*The FQM suffix used to distinguish feedmail queued message files.
You probably want this to be a period followed by some letters and/or
digits.  The distinction is to be able to tell them from other random
files that happen to be in the feedmail-queue-directory or
feedmail-queue-draft-directory. By the way, FQM stands for feedmail
queued message."
  :group 'feedmail-queue
  :type 'string
  )


(defcustom feedmail-nuke-buffer-after-queue nil
  "*If non-nil, silently kill the buffer after a message is queued.
You might like that since a side-effect of queueing the message is
that its buffer name gets changed to the filename.  That means that
the buffer won't be reused for the next message you compose.  If you
are using VM for creating messages, you probably want to leave this
nil, since VM has its own options for managing the recycling of
message buffers."
  :group 'feedmail-queue
  :type 'boolean
  )


(defcustom feedmail-queue-auto-file-nuke nil
  "*If non-nil, automatically delete queue files when a message is sent.
Normally, feedmail will notice such files when you send a message in
immediate mode (i.e., not when you're running the queue) and will ask if
you want to delete them.  Since the answer is usually yes, setting this
variable to non-nil will tell feedmail to skip the prompt and just delete
the file without bothering you."
  :group 'feedmail-queue
  :type 'boolean
  )


;; defvars to make byte-compiler happy(er)
(defvar feedmail-error-buffer        nil)
(defvar feedmail-prepped-text-buffer nil)
(defvar feedmail-raw-text-buffer     nil)
(defvar feedmail-address-list        nil)


(defvar feedmail-queue-runner-is-active nil
  "*Non-nil means we're inside the logic of the queue-running loop.
That is, iterating over all messages in the queue to send them.  In
that case, the value is the name of the queued message file currently
being processed.  This can be used for differentiating customized code
for different scenarios.  Users shouldn't set or change this
variable, but may depend on its value as described here.")


(defun feedmail-mail-send-hook-splitter ()
  "Facilitate dividing mail-send-hook things into queued and immediate cases.
If you have mail-send-hook functions that should only be called for sending/
queueing messages or only be called for the sending of queued messages, this is
for you.  Add this function to mail-send-hook with something like this:

	(add-hook 'mail-send-hook 'feedmail-mail-send-hook-splitter)

Then add the functions you want called to either feedmail-mail-send-hook-queued
or feedmail-mail-send-hook, as apprpriate.  The distinction is that
feedmail-mail-send-hook will be called when you send mail from a composition
buffer (typically by typing C-c C-c), whether the message is sent immediately
or placed in the queue or drafts directory.  feedmail-mail-send-hook-queued is
called when messages are being sent from the queue directory, typically via a
call to feedmail-run-the-queue."
  (if feedmail-queue-runner-is-active
      (run-hooks 'feedmail-mail-send-hook-queued)
    (run-hooks 'feedmail-mail-send-hook))
  )


(defvar feedmail-mail-send-hook nil
  "*See documentation for feedmail-mail-send-hook-splitter.")


(defvar feedmail-mail-send-hook-queued nil
  "*See documentation for feedmail-mail-send-hook-splitter.")


(defun feedmail-confirm-addresses-hook-example ()
  "An example of a feedmail-last-chance-hook.
It shows the simple addresses and gets a confirmation.  Use as:
 (setq feedmail-last-chance-hook 'feedmail-confirm-addresses-hook-example)."
  (save-window-excursion
    (display-buffer (set-buffer (get-buffer-create " F-C-A-H-E")))
    (erase-buffer)
    (insert (mapconcat 'identity feedmail-address-list " "))
    (if (not (y-or-n-p "How do you like them apples? "))
	(error "FQM: Sending...gave up in last chance hook")
      )))


(defcustom feedmail-last-chance-hook nil
  "*User's last opportunity to modify the message on its way out.
It has already had all the header prepping from the standard package.
The next step after running the hook will be to push the buffer into a
subprocess that mails the mail.  The hook might be interested in
these: (1) feedmail-prepped-text-buffer contains the header and body
of the message, ready to go; (2) feedmail-address-list contains a list
of simplified recipients of addresses which are to be given to the
subprocess (the hook may change the list); (3) feedmail-error-buffer
is an empty buffer intended to soak up errors for display to the user.
If the hook allows interactive activity, the user should not send more
mail while in the hook since some of the internal buffers will be
reused and things will get confused."
  :group 'feedmail-misc
  :type 'hook
  )


(defcustom feedmail-before-fcc-hook nil
  "*User's last opportunity to modify the message before Fcc action.
It has already had all the header prepping from the standard package.
The next step after running the hook will be to save the message via
Fcc: processing. The hook might be interested in these: (1)
feedmail-prepped-text-buffer contains the header and body of the
message, ready to go; (2) feedmail-address-list contains a list of
simplified recipients of addressees to whom the message was sent (3)
feedmail-error-buffer is an empty buffer intended to soak up errors
for display to the user.  If the hook allows interactive activity, the
user should not send more mail while in the hook since some of the
internal buffers will be reused and things will get confused."
  :group 'feedmail-misc
  :type 'hook
  )

(defcustom feedmail-queue-runner-mode-setter
  '(lambda (&optional arg) (mail-mode))
  "*A function to set the proper mode of a message file.
Called when the message is read back out of the queue directory with a single
argument, the optional argument used in the call to
feedmail-run-the-queue or feedmail-run-the-queue-no-prompts.

Most people want `mail-mode', so the default value is an anonymous
function which is just a wrapper to ignore the supplied argument when
calling it, but here's your chance to have something different.
Called with funcall, not `call-interactively'."
  :group 'feedmail-queue
  :type 'function
  )


(defcustom feedmail-queue-alternative-mail-header-separator nil
  "*Alternative header demarcation for queued messages.
If you sometimes get alternative values for `mail-header-separator' in
queued messages, set the value of this variable to whatever it is.
For example, `rmail-resend' uses a `mail-header-separator' value of empty
string (\"\") when you send/queue a message.

When trying to send a queued message, if the value of this variable is
non-nil, feedmail will first try to send the message using the value
of `mail-header-separator'.  If it can't find that, it will temporarily
set `mail-header-separator' to the value of
feedmail-queue-alternative-mail-header-separator and try again."
  :group 'feedmail-queue
  :type '(choice (const nil) string)
  )


(defcustom feedmail-queue-runner-message-sender 'mail-send-and-exit
  "*Function to initiate sending a message file.
Called for each message read back out of the queue directory with a
single argument, the optional argument used in the call to
feedmail-run-the-queue or feedmail-run-the-queue-no-prompts.
Interactively, that argument will be the prefix argument.  Most people
want mail-send-and-exit (bound to C-c C-c in mail-mode), but here's
your chance to have something different.  Called with funcall, not
call-interactively."
  :group 'feedmail-queue
  :type 'function
  )


(defcustom feedmail-queue-runner-cleaner-upper
  '(lambda (fqm-file &optional arg)
     (delete-file fqm-file)
     (if (and arg feedmail-queue-chatty) (message "FQM: Nuked %s" fqm-file)))
  "*Function that will be called after a message has been sent.
Not called in the case of errors.  This function is called with two
arguments: the name of the message queue file for the message just sent,
and the optional argument used in the call to `feedmail-run-the-queue'
or `feedmail-run-the-queue-no-prompts' (prefix arg if interactive).
In any case, the affiliated buffer is killed elsewhere, so don't do that
inside this function.  Return value is ignored.

The default action is an anonymous function which gets rid of the file
from the queue directory.  With a non-nil second argument, a brief
message is give for each file deleted.  You could replace this
function, for example, to archive all of your sent messages someplace
\(though there are better ways to get that particular result\)."
  :group 'feedmail-queue
  :type 'function
  )


(defvar feedmail-is-a-resend nil
  "*Non-nil means the message is a Resend (in the RFC-822 sense).
This affects the composition of certain headers.  feedmail sets this
variable as soon as it starts prepping the message text buffer, so any
user-supplied functions can rely on it.  Users shouldn't set or change this
variable, but may depend on its value as described here.")


(defcustom feedmail-buffer-eating-function 'feedmail-buffer-to-binmail
  "*Function used to send the prepped buffer to a subprocess.
The function's three (mandatory) arguments are: (1) the buffer
containing the prepped message; (2) a buffer where errors should be
directed; and (3) a list containing the addresses individually as
strings.  Three popular choices for this are
feedmail-buffer-to-binmail, feedmail-buffer-to-smtpmail, and
feedmail-buffer-to-sendmail.  If you use the sendmail form, you
probably want to set feedmail-nuke-bcc and/or feedmail-nuke-resent-bcc
to nil.  If you use the binmail form, check the value of
feedmail-binmail-template."
  :group 'feedmail-misc
  :type 'function
  )


(defcustom feedmail-binmail-template (if mail-interactive "/bin/mail %s" "/bin/rmail %s")
  "*Command template for the subprocess which will get rid of the mail.
It can result in any command understandable by /bin/sh.  Might not
work at all in non-Unix environments.  The single '%s', if present,
gets replaced by the space-separated, simplified list of addressees.
Used in feedmail-buffer-to-binmail to form the shell command which
will receive the contents of the prepped buffer as stdin.  If you'd
like your errors to come back as mail instead of immediately in a
buffer, try /bin/rmail instead of /bin/mail (this can be accomplished
by keeping the default nil setting of `mail-interactive').  You might
also like to consult local mail experts for any other interesting
command line possibilities."
  :group 'feedmail-misc
  :type 'string
  )


;; feedmail-buffer-to-binmail, feedmail-buffer-to-sendmail, and
;; feedmail-buffer-to-smptmail are the only things provided for values
;; for the variable feedmail-buffer-eating-function.  It's pretty easy
;; to write your own, though.
(defun feedmail-buffer-to-binmail (prepped errors-to addr-listoid)
  "Function which actually calls /bin/mail as a subprocess.
Feeds the buffer to it."
  (set-buffer prepped)
  (apply
   'call-process-region
   (append (list (point-min) (point-max) "/bin/sh" nil errors-to nil "-c"
		 (format feedmail-binmail-template
			 (mapconcat 'identity addr-listoid " "))))))


(defun feedmail-buffer-to-sendmail (prepped errors-to addr-listoid)
  "Function which actually calls sendmail as a subprocess.
Feeds the buffer to it.  Probably has some flaws for Resent-* and other
complicated cases."
  (set-buffer prepped)
  (apply 'call-process-region
	 (append (list (point-min) (point-max)
		       (if (boundp 'sendmail-program) sendmail-program "/usr/lib/sendmail")
		       nil errors-to nil "-oi" "-t")
		 ;; provide envelope "from" to sendmail; results will vary
		 (list "-f" user-mail-address)
		 ;; These mean "report errors by mail" and "deliver in background".
		 (if (null mail-interactive) '("-oem" "-odb")))))

;; provided by jam@austin.asc.slb.com (James A. McLaughlin);
;; simplified by WJC after more feedmail development;
;; idea (but not implementation) of copying smtpmail trace buffer to
;; feedmail error buffer from:
;;   Mon 14-Oct-1996; Douglas Gray Stephens
;;   modified to insert error for displaying
(defun feedmail-buffer-to-smtpmail (prepped errors-to addr-listoid)
  "Function which actually calls `smtpmail-via-smtp' to send buffer as e-mail."
  ;; I'm not sure smtpmail.el is careful about the following
  ;; return value, but it also uses it internally, so I will fear
  ;; no evil.
  (require 'smtpmail)
  (if (not (smtpmail-via-smtp addr-listoid prepped))
      (progn
	(set-buffer errors-to)
	(insert "Send via smtpmail failed.  Probable SMTP protocol error.\n")
	(insert "Look for details below or in the *Messages* buffer.\n\n")
	(let ((case-fold-search t)
	      ;; don't be overconfident about the name of the trace buffer
	      (tracer (concat "trace.*smtp.*" (regexp-quote smtpmail-smtp-server))))
	  (mapcar
	   '(lambda (buffy)
	      (if (string-match tracer (buffer-name buffy))
		  (progn
		    (insert "SMTP Trace from " (buffer-name buffy) "\n---------------")
		    (insert-buffer buffy)
		    (insert "\n\n"))))
	   (buffer-list))))))


;; just a place to park a docstring
(defconst feedmail-fiddle-plex-blurb nil
  "A fiddle-plex is a concise way of specifying header field fiddling.
It is a list of up to 4 elements: NAME, VALUE, ACTION, FOLDING.  The element
VALUE can also be a list sometimes.

NAME is the name of the header field to be fiddled with.  Although case
doesn't matter in looking for headers, case of NAME is preserved when a header
is inserted via fiddling.  It shouldn't include the trailing colon.

VALUE is either nil, a simple string, a function returning nil or a string, or,
as described below for ACTION `combine', a list of up to three values.

ACTION describes the nature of the fiddling to be done.  Possibilities
for ACTION (default is `supplement'):

  `supplement'  Leave other like fields as-is, insert this one.

  `replace'     Delete other like fields, if any, and insert this one.

  `create'      Insert this one only if no like field exists.

  `combine'     Combine aggregate values of like fields with this one.
                In this case, VALUE has a special form.  It is a list
                of three items: VAL-PRE, VAL-LIKE, and VAL-POST.
                VAL-PRE and VAL-POST are strings or nil.  VAL-LIKE may
                be either a string or a function (it may also be nil,
                but there's not much point to that).

                Values of like header fields are aggregated, leading and
                trailing whitespace is removed, and embedded
                whitespace is left as-is.  If there are no like
                fields, or the aggregate value is an empty string,
                VAL-LIKE is not used.  Else, if VAL-LIKE is a function,
                it is called with two arguments: NAME and the
                aggregate like values.  Else, if VAL-LIKE is a string, it is
                used as a format string where a single \%s will be
                replaced by the aggregate values of like fields.

                VAL-PRE, the results of using VAL-LIKE, and VAL-POST
                are concatenated, and the result, if not nil and not
                an empty string, is used as the new value for the
                field.  Although this description sounds a bit
                complicated, the idea is to provide a mechanism for
                combining the old value with a new value in a flexible
                way.  For example, if you wanted to add a new value to
                an existing header field by adding a semi-colon and
                then starting the new value on a continuation line,
                you might specify this:

                 (nil \"%s;\\n\\t\" \"This is my new value\")

FOLDING can be nil, in which case VALUE is used as-is.  If FOLDING is
non-nil, feedmail \"smart filling\" is done on VALUE just before
insertion.")

;;;###autoload
(defun feedmail-send-it ()
  "Send the current mail buffer using the Feedmail package.
This is a suitable value for `send-mail-function'.  It can be used
with various lower-level mechanisms to provide features such as queueing."

  ;; avoid matching trouble over slash vs backslash by getting canonical
  (if feedmail-queue-directory
      (setq feedmail-queue-directory (expand-file-name feedmail-queue-directory)))
  (if feedmail-queue-draft-directory
      (setq feedmail-queue-draft-directory (expand-file-name feedmail-queue-draft-directory)))
  (if (not feedmail-enable-queue) (feedmail-send-it-immediately)
    ;; else, queuing is enabled, should we ask about it or just do it?
    (if feedmail-ask-before-queue
	(funcall (feedmail-queue-send-edit-prompt))
      (feedmail-dump-message-to-queue feedmail-queue-directory 'after-queue))))


(defun feedmail-message-action-send ()
  ;; hooks can make this take a while so clear the prompt
  (message "FQM: Immediate send...")
  (feedmail-send-it-immediately))


;; From a VM mailing list discussion and some suggestions from Samuel Mikes <smikes@alumni.hmc.edu>
(defun feedmail-queue-express-to-queue ()
  "*Send message directly to the queue, with a minimum of fuss and bother."
  (interactive)
  (let ((feedmail-enable-queue t)
	(feedmail-ask-before-queue nil)
	(feedmail-queue-reminder-alist nil)
	(feedmail-queue-chatty-sit-for 0))
    (feedmail-send-it)
    )
  )


(defun feedmail-queue-express-to-draft ()
  "*Send message directly to the draft queue, with a minimum of fuss and bother."
  (interactive)
  (let ((feedmail-queue-directory feedmail-queue-draft-directory))
    (feedmail-queue-express-to-queue)
    )
  )


(defun feedmail-message-action-send-strong ()
  (let ((feedmail-confirm-outgoing nil)) (feedmail-message-action-send)))


(defun feedmail-message-action-edit ()
  (error "FQM: Message not queued; returning to edit"))


(defun feedmail-message-action-draft ()
  (feedmail-dump-message-to-queue feedmail-queue-draft-directory 'after-draft))


(defun feedmail-message-action-draft-strong ()
  (let ((buffer-file-name nil))
    (feedmail-message-action-draft)))


(defun feedmail-message-action-queue ()
  (feedmail-dump-message-to-queue feedmail-queue-directory 'after-queue))


(defun feedmail-message-action-queue-strong ()
  (let ((buffer-file-name nil))
    (feedmail-message-action-queue)))


(defun feedmail-message-action-toggle-spray ()
  (let ((feedmail-enable-spray (not feedmail-enable-spray)))
    (if feedmail-enable-spray
	(message "FQM: For this message, spray toggled ON")
      (message "FQM: For this message, spray toggled OFF"))
    (sit-for 3)
    ;; recursion, but harmless
    (feedmail-send-it)))


(defun feedmail-message-action-help ()
  (let ((d-string " "))
    (if (stringp feedmail-ask-before-queue-default)
	(setq d-string feedmail-ask-before-queue-default)
      (setq d-string  (char-to-string feedmail-ask-before-queue-default)))
    (feedmail-queue-send-edit-prompt-help d-string)
    ;; recursive, but no worries (it goes deeper on user action)
    (feedmail-send-it)))


;;;###autoload
(defun feedmail-run-the-queue-no-prompts (&optional arg)
  "Like feedmail-run-the-queue, but suppress confirmation prompts."
  (interactive "p")
  (let ((feedmail-confirm-outgoing nil)) (feedmail-run-the-queue arg)))

;;;###autoload
(defun feedmail-run-the-queue-global-prompt (&optional arg)
  "Like feedmail-run-the-queue, but with a global confirmation prompt.
This is generally most useful if run non-interactively, since you can
bail out with an appropriate answer to the global confirmation prompt."
  (interactive "p")
  (let ((feedmail-queue-runner-confirm-global t)) (feedmail-run-the-queue arg)))

;;;###autoload
(defun feedmail-run-the-queue (&optional arg)
  "Visit each message in the feedmail queue directory and send it out.
Return value is a list of three things: number of messages sent, number of
messages skipped, and number of non-message things in the queue (commonly
backup file names and the like)."
  (interactive "p")
  ;; avoid matching trouble over slash vs backslash by getting canonical
  (if feedmail-queue-directory
      (setq feedmail-queue-directory (expand-file-name feedmail-queue-directory)))
  (if feedmail-queue-draft-directory
      (setq feedmail-queue-draft-directory (expand-file-name feedmail-queue-draft-directory)))
  (let* ((maybe-file)
	 (qlist (feedmail-look-at-queue-directory feedmail-queue-directory))
	 (dlist (feedmail-look-at-queue-directory feedmail-queue-draft-directory))
	 (q-cnt (nth 0 qlist))
	 (q-oth (nth 1 qlist))
	 (d-cnt (nth 0 dlist))
	 (d-oth (nth 1 dlist))
	 (messages-sent 0)
	 (messages-skipped 0)
	 (blobby-buffer)
	 (already-buffer)
	 (this-mhsep)
	 (do-the-run t)
	 (list-of-possible-fqms))
    (if (and (> q-cnt 0) feedmail-queue-runner-confirm-global)
	(setq do-the-run
	      (if (fboundp 'y-or-n-p-with-timeout)
		  (y-or-n-p-with-timeout (format "FQM: Draft: %dm+%d,  Queue: %dm+%d; run the queue? "
						 d-cnt d-oth q-cnt q-oth)
					 5 nil)
		(y-or-n-p (format "FQM: Draft: %dm+%d,  Queue: %dm+%d; run the queue? "
				  d-cnt d-oth q-cnt q-oth))
		)))
    (if (not do-the-run)
	(setq messages-skipped q-cnt)
      (save-window-excursion
	(setq list-of-possible-fqms (directory-files feedmail-queue-directory t))
	(if feedmail-queue-run-orderer
	    (setq list-of-possible-fqms (funcall feedmail-queue-run-orderer list-of-possible-fqms)))
	(mapcar
	 '(lambda (blobby)
	    (setq maybe-file (expand-file-name blobby feedmail-queue-directory))
	    (cond
	     ((file-directory-p maybe-file) nil) ; don't care about subdirs
	     ((feedmail-fqm-p blobby)
	      (setq blobby-buffer (generate-new-buffer (concat "FQM " blobby)))
	      (setq already-buffer
		    (if (fboundp 'find-buffer-visiting) ; missing from XEmacs
			(find-buffer-visiting maybe-file)
		      (get-file-buffer maybe-file)))
	      (if (and already-buffer (buffer-modified-p already-buffer))
		  (save-window-excursion
		    (display-buffer (set-buffer already-buffer))
		    (if (fboundp 'y-or-n-p-with-timeout)
			;; make a guess that the user just forgot to save
			(if (y-or-n-p-with-timeout (format "FQM: Visiting %s; save before send? " blobby) 10 t)
			    (save-buffer))
		      (if (y-or-n-p (format "FQM: Visiting %s; save before send? " blobby))
			  (save-buffer))
		      )))

	      (set-buffer blobby-buffer)
	      (setq buffer-offer-save nil)
	      (buffer-disable-undo blobby-buffer)
	      (insert-file-contents-literally maybe-file)
	      ;; work around text-vs-binary weirdness and also around rmail-resend's creative
	      ;; manipulation of mail-header-separator
	      ;;
	      ;; if we don't find the normal M-H-S, and the alternative is defined but also
	      ;; not found, try reading the file a different way
	      ;;
	      ;; if M-H-S not found and (a-M-H-S is nil or not found)
	      (if (and (not (feedmail-find-eoh t))
		       (or (not feedmail-queue-alternative-mail-header-separator)
			   (not
			    (let ((mail-header-separator feedmail-queue-alternative-mail-header-separator))
			      (feedmail-find-eoh t)))))
		  (let ((file-name-buffer-file-type-alist nil) (default-buffer-file-type nil))
		    (erase-buffer) (insert-file-contents maybe-file))
		)
	      ;; if M-H-S not found and (a-M-H-S is non-nil and is found)
	      ;; temporarily set M-H-S to the value of a-M-H-S
	      (if (and (not (feedmail-find-eoh t))
		       feedmail-queue-alternative-mail-header-separator
		       (let ((mail-header-separator feedmail-queue-alternative-mail-header-separator))
			 (feedmail-find-eoh t)))
		  (setq this-mhsep feedmail-queue-alternative-mail-header-separator)
		(setq this-mhsep mail-header-separator))
	      (funcall feedmail-queue-runner-mode-setter arg)
	      (condition-case nil	; don't give up the loop if user skips some
		  (let ((feedmail-enable-queue nil)
			(mail-header-separator this-mhsep)
			(feedmail-queue-runner-is-active maybe-file))
		    (funcall feedmail-queue-runner-message-sender arg)
		    (set-buffer blobby-buffer)
		    (if (buffer-modified-p) ; still modified, means wasn't sent
			(setq messages-skipped (1+ messages-skipped))
		      (setq messages-sent (1+ messages-sent))
		      (funcall feedmail-queue-runner-cleaner-upper maybe-file arg)
		      (if (and already-buffer (not (file-exists-p maybe-file)))
			  ;; we have gotten rid of the file associated with the
			  ;; buffer, so update the buffer's notion of that
			  (save-excursion
			    (set-buffer already-buffer)
			    (setq buffer-file-name nil)))))
		(error (setq messages-skipped (1+ messages-skipped))))
	      (kill-buffer blobby-buffer)
	      (if feedmail-queue-chatty
		  (progn
		    (message "FQM: %d to go, %d sent, %d skipped (%d other files ignored)"
			     (- q-cnt messages-sent messages-skipped)
			     messages-sent messages-skipped q-oth)
		    (sit-for feedmail-queue-chatty-sit-for))))))
	 list-of-possible-fqms)))
    (if feedmail-queue-chatty
	(progn
	  (message "FQM: %d sent, %d skipped (%d other files ignored)"
		   messages-sent messages-skipped q-oth)
	  (sit-for feedmail-queue-chatty-sit-for)
	  (feedmail-queue-reminder 'after-run)
	  (sit-for feedmail-queue-chatty-sit-for)))
    (list messages-sent messages-skipped q-oth)))


;;;###autoload
(defun feedmail-queue-reminder (&optional what-event)
  "Perform some kind of reminder activity about queued and draft messages.
Called with an optional symbol argument which says what kind of event
is triggering the reminder activity.  The default is 'on-demand, which
is what you typically would use if you were putting this in your emacs start-up
or mail hook code.  Other recognized values for WHAT-EVENT (these are passed
internally by feedmail):

   after-immediate      (a message has just been sent in immediate mode)
   after-queue          (a message has just been queued)
   after-draft          (a message has just been placed in the draft directory)
   after-run            (the queue has just been run, possibly sending messages)

WHAT-EVENT is used as a key into the table feedmail-queue-reminder-alist.  If
the associated value is a function, it is called without arguments and is expected
to perform the reminder activity.  You can supply your own reminder functions
by redefining feedmail-queue-reminder-alist.  If you don't want any reminders,
you can set feedmail-queue-reminder-alist to nil."
  (interactive "p")
  (let ((key (if (and what-event (symbolp what-event)) what-event 'on-demand)) entry reminder)
    (setq entry (assoc key feedmail-queue-reminder-alist))
    (setq reminder (cdr entry))
    (if (fboundp reminder) (funcall reminder)))
  )


(defun feedmail-queue-reminder-brief ()
  "Brief display of draft and queued message counts in modeline."
  (interactive)
  (let (q-cnt d-cnt q-lis d-lis)
    (setq q-lis (feedmail-look-at-queue-directory feedmail-queue-directory))
    (setq d-lis (feedmail-look-at-queue-directory feedmail-queue-draft-directory))
    (setq q-cnt (car q-lis))
    (setq d-cnt (car d-lis))
    (if (or (> q-cnt 0) (> d-cnt 0))
	(progn
	  (message "FQM: [D: %d,  Q: %d]" d-cnt q-cnt))))
  )


(defun feedmail-queue-reminder-medium ()
  "Verbose display of draft and queued message counts in modeline."
  (interactive)
  (let (q-cnt d-cnt q-oth d-oth q-lis d-lis)
    (setq q-lis (feedmail-look-at-queue-directory feedmail-queue-directory))
    (setq d-lis (feedmail-look-at-queue-directory feedmail-queue-draft-directory))
    (setq q-cnt (car q-lis))
    (setq d-cnt (car d-lis))
    (setq q-oth (nth 1 q-lis))
    (setq d-oth (nth 1 d-lis))
    (if (or (> q-cnt 0) (> d-cnt 0))
	(progn
	  (message "FQM: Draft: %dm+%d in \"%s\",  Queue: %dm+%d in \"%s\""
		   d-cnt d-oth (file-name-nondirectory feedmail-queue-draft-directory)
		   q-cnt q-oth (file-name-nondirectory feedmail-queue-directory)))))
  )


(defun feedmail-queue-send-edit-prompt ()
  "Ask whether to queue, send immediately, or return to editing a message."
  ;; Some implementation ideas here came from the userlock.el code
  (discard-input)
  (save-window-excursion
    (let ((answer) (d-char) (d-string " "))
      (if (stringp feedmail-ask-before-queue-default)
	  (progn
	    (setq d-char   (string-to-char feedmail-ask-before-queue-default))
	    (setq d-string feedmail-ask-before-queue-default))
	(setq d-string  (char-to-string feedmail-ask-before-queue-default))
	(setq d-char    feedmail-ask-before-queue-default)
	)
      (while (null answer)
	(message feedmail-ask-before-queue-prompt d-string)
	(let ((user-sez
	       (let ((inhibit-quit t) (cursor-in-echo-area t) (echo-keystrokes 0))
		 (read-char-exclusive))))
	  (if (= user-sez help-char)
	      (setq answer '(^ . feedmail-message-action-help))
	    (if (or (eq user-sez ?\C-m) (eq user-sez ?\C-j) (eq user-sez ?y))
		(setq user-sez d-char))
	    ;; these char-to-int things are because of some
	    ;; incomprensible difference between the two in
	    ;; byte-compiled stuff between Emacs and XEmacs
	    ;; (well, I'm sure someone could comprehend it,
	    ;; but I say 'uncle')
	    (setq answer (or (assoc user-sez feedmail-prompt-before-queue-user-alist)
			     (and (fboundp 'char-to-int)
				  (assoc (char-to-int user-sez) feedmail-prompt-before-queue-user-alist))
			     (assoc user-sez feedmail-prompt-before-queue-standard-alist)
			     (and (fboundp 'char-to-int)
				  (assoc (char-to-int user-sez) feedmail-prompt-before-queue-standard-alist))))
	    (if (or (null answer) (null (cdr answer)))
		(progn
		  (beep)
		  (message feedmail-ask-before-queue-reprompt d-string)
		  (sit-for 3)))
	    )))
      (cdr answer)
      )))

(defconst feedmail-p-h-b-n "*FQM Help*")

(defun feedmail-queue-send-edit-prompt-help (d-string)
  (let ((fqm-help (get-buffer feedmail-p-h-b-n)))
    (if (and fqm-help (get-buffer-window fqm-help 'visible))
	(feedmail-queue-send-edit-prompt-help-later fqm-help d-string)
      (feedmail-queue-send-edit-prompt-help-first d-string))))

(defun feedmail-queue-send-edit-prompt-help-later (fqm-help d-string)
  ;; scrolling fun
  (save-selected-window
    (let ((signal-error-on-buffer-boundary nil)
	  (fqm-window (display-buffer fqm-help)))
      (select-window fqm-window)
      (if (pos-visible-in-window-p (point-max) fqm-window)
	  (feedmail-queue-send-edit-prompt-help-first d-string)
	;;(goto-char (point-min))
	(scroll-up nil)
	))))

(defun feedmail-queue-send-edit-prompt-help-first (d-string)
  (with-output-to-temp-buffer feedmail-p-h-b-n
    (princ "You're dispatching a message and feedmail queuing is enabled.
Typing ? or C-v will normally scroll this help buffer.

Choices:
   q  QUEUE        for later sending (via feedmail-run-the-queue)
   Q  QUEUE!       like \"q\", but always make a new file
   i  IMMEDIATELY  send this (but not the other queued messages)
   I  IMMEDIATELY! like \"i\", but skip following confirmation prompt
   d  DRAFT        queue in the draft directory
   D  DRAFT!       like \"d\", but always make a new file
   e  EDIT         return to the message edit buffer (don't send or queue)
   *  SPRAY        toggle spray mode (individual message transmissions)

Synonyms:
   s  SEND         immediately (same as \"i\")
   S  SEND!        immediately (same as \"I\")
   r  ROUGH        draft (same as \"d\")
   R  ROUGH!       draft (same as \"D\")
   n  NOPE         didn't mean it (same as \"e\")
   y  YUP          do the default behavior (same as \"C-m\")

The user-configurable default is currently \"")
    (princ d-string)
    (princ "\".  For other possibilities,
see the variable feedmail-prompt-before-queue-user-alist.
")
    (and (stringp feedmail-prompt-before-queue-help-supplement)
	 (princ feedmail-prompt-before-queue-help-supplement))
    (save-excursion (set-buffer standard-output) (if (fboundp 'help-mode) (help-mode)))))

(defun feedmail-look-at-queue-directory (queue-directory)
  "Find out some things about a queue directory.
Result is a list containing a count of queued messages in the
directory, a count of other files in the directory, and a high water
mark for prefix sequence numbers.  Subdirectories are not included in
the counts."
  (let ((q-cnt 0) (q-oth 0) (high-water 0) (blobbet))
    ;; iterate, counting things we find along the way in the directory
    (if (file-directory-p queue-directory)
	(mapcar
	 '(lambda (blobby)
	    (cond
	     ((file-directory-p blobby) nil) ; don't care about subdirs
	     ((feedmail-fqm-p blobby)
	      (setq blobbet (file-name-nondirectory blobby))
	      (if (string-match "^[0-9][0-9][0-9]-" blobbet)
		  (let ((water-mark))
		    (setq water-mark (string-to-int (substring blobbet 0 3)))
		    (if (> water-mark high-water) (setq high-water water-mark))))
	      (setq q-cnt (1+ q-cnt)))
	     (t (setq q-oth (1+ q-oth)))
	     ))
	 (directory-files queue-directory t)))
    (list q-cnt q-oth high-water)))

(defun feedmail-tidy-up-slug (slug)
  "Utility for mapping out suspect characters in a potential filename."
  ;; even programmers deserve a break sometimes, so cover nil for them
  (if (null slug) (setq slug ""))
  ;; replace all non-alphanumerics with hyphen for safety
  (while (string-match "[^a-z0-9-]+" slug) (setq slug (replace-match "-" nil nil slug)))
  ;; collapse multiple hyphens to one
  (while (string-match "--+" slug) (setq slug (replace-match "-" nil nil slug)))
  ;; for tidyness, peel off leading hyphens
  (if (string-match "^-*" slug) (setq slug (replace-match "" nil nil slug)))
  ;; for tidyness, peel off trailing hyphens
  (if (string-match "-*$" slug) (setq slug (replace-match "" nil nil slug)))
  slug
  )

(defun feedmail-queue-subject-slug-maker (&optional queue-directory)
  "Create a name for storing the message in the queue.
Optional argument QUEUE-DIRECTORY specifies into which directory the
file will be placed.  The name is based on the Subject: header (if
there is one).  If there is no subject,
feedmail-queue-default-file-slug is consulted Special characters are
mapped to mostly alphanumerics for safety."
  (let ((eoh-marker) (case-fold-search t) (subject "") (s-point))
    (setq eoh-marker (feedmail-find-eoh))
    (goto-char (point-min))
    ;; get raw subject value (first line, anyhow)
    (if (re-search-forward "^Subject:" eoh-marker t)
	(progn (setq s-point (point))
	       (end-of-line)
	       (setq subject (buffer-substring s-point (point)))))
    (setq subject (feedmail-tidy-up-slug subject))
    (if (zerop (length subject))
	(setq subject
	      (cond
	       ((stringp feedmail-queue-default-file-slug) feedmail-queue-default-file-slug)
	       ((fboundp feedmail-queue-default-file-slug)
		(save-excursion (funcall feedmail-queue-default-file-slug)))
	       ((eq feedmail-queue-default-file-slug 'ask)
		(file-name-nondirectory
		 (read-file-name "FQM: Message filename slug? "
				 (file-name-as-directory queue-directory) subject nil subject)))
	       (t "no subject"))
	      ))
    ;; one more time, with feeling
    (feedmail-tidy-up-slug subject)
    ))


(defun feedmail-create-queue-filename (queue-directory)
  (let ((slug "wjc"))
    (cond
     (feedmail-queue-slug-maker
      (save-excursion (setq slug (funcall feedmail-queue-slug-maker queue-directory))))
     (feedmail-ask-for-queue-slug
      (setq slug (file-name-nondirectory
		  (read-file-name (concat "FQM: Message filename slug? [" slug "]? ")
				  (file-name-as-directory queue-directory) slug nil slug))))
     )
    (setq slug (feedmail-tidy-up-slug slug))
    (setq slug (format "%03d-%s" (1+ (nth 2 (feedmail-look-at-queue-directory queue-directory))) slug))
    (concat
     (expand-file-name slug queue-directory)
     feedmail-queue-fqm-suffix)
    ))


(defun feedmail-dump-message-to-queue (queue-directory what-event)
  (or (file-accessible-directory-p queue-directory)
      ;; progn to get nil result no matter what
      (progn (make-directory queue-directory t) nil)
      (file-accessible-directory-p queue-directory)
      (error (concat "FQM: Message not queued; trouble with directory " queue-directory)))
  (let ((filename)
	(is-fqm)
	(is-in-this-dir)
	(previous-buffer-file-name buffer-file-name))
    (if buffer-file-name
	(progn
	  (setq is-fqm (feedmail-fqm-p buffer-file-name))
	  (setq is-in-this-dir (string-equal
				(directory-file-name queue-directory)
				(directory-file-name (expand-file-name (file-name-directory buffer-file-name)))))))
    ;; if visiting a queued message, just save
    (if (and is-fqm is-in-this-dir)
	(setq filename buffer-file-name)
      (setq filename (feedmail-create-queue-filename queue-directory)))
    ;; make binary file on DOS/Win95/WinNT, etc
    (let ((buffer-file-type feedmail-force-binary-write)) (write-file filename))
    ;; convenient for moving from draft to q, for example
    (if (and previous-buffer-file-name (or (not is-fqm) (not is-in-this-dir))
	     (y-or-n-p (format "FQM: Was previously %s; delete that? " previous-buffer-file-name)))
	(delete-file previous-buffer-file-name))
    (if feedmail-nuke-buffer-after-queue
	(let ((a-s-file-name buffer-auto-save-file-name))
	  ;; be aggressive in nuking auto-save files
	  (and (kill-buffer (current-buffer))
	       delete-auto-save-files
	       (file-exists-p a-s-file-name)
	       (delete-file a-s-file-name))))
    (if feedmail-queue-chatty
	(progn (message (concat "FQM: Queued in " filename))
	       (sit-for feedmail-queue-chatty-sit-for)))
    (if feedmail-queue-chatty
	(progn
	  (feedmail-queue-reminder what-event)
	  (sit-for feedmail-queue-chatty-sit-for)))))


;; from a similar function in mail-utils.el
(defun feedmail-rfc822-time-zone (time)
  (let* ((sec (or (car (current-time-zone time)) 0))
	 (absmin (/ (abs sec) 60)))
    (format "%c%02d%02d" (if (< sec 0) ?- ?+) (/ absmin 60) (% absmin 60))))

(defun feedmail-rfc822-date (arg-time)
  (let ((time (if arg-time arg-time (current-time))))
    (concat
     (format-time-string "%a, %e %b %Y %T " time)
     (feedmail-rfc822-time-zone time)
     )))


(defun feedmail-send-it-immediately ()
  "Handle immediate sending, including during a queue run."
  (let* ((feedmail-error-buffer (get-buffer-create " *FQM Outgoing Email Errors*"))
	 (feedmail-prepped-text-buffer (get-buffer-create " *FQM Outgoing Email Text*"))
	 (feedmail-raw-text-buffer (current-buffer))
	 (feedmail-address-list)
	 (eoh-marker)
	 (bcc-holder)
	 (resent-bcc-holder)
	 (a-re-rtcb  "^Resent-\\(To\\|Cc\\|Bcc\\):")
	 (a-re-rtc   "^Resent-\\(To\\|Cc\\):")
	 (a-re-rb    "^Resent-Bcc:")
	 (a-re-dtcb  "^\\(To\\|Cc\\|Bcc\\):")
	 (a-re-dtc   "^\\(To\\|Cc\\):")
	 (a-re-db    "^Bcc:")
	 ;; to get a temporary changable copy
	 (mail-header-separator mail-header-separator)
	 )
    (unwind-protect
	(save-excursion
	  (set-buffer feedmail-error-buffer) (erase-buffer)
	  (set-buffer feedmail-prepped-text-buffer) (erase-buffer)

	  ;; jam contents of user-supplied mail buffer into our scratch buffer
	  (insert-buffer feedmail-raw-text-buffer)

	  ;; require one newline at the end.
	  (goto-char (point-max))
	  (or (= (preceding-char) ?\n) (insert ?\n))

	  (let ((case-fold-search nil))
	    ;; Change header-delimiter to be what mailers expect (empty line).
	    ;; leaves match data in place or signals error
	    (setq eoh-marker (feedmail-find-eoh))
	    (replace-match "\n")
	    (setq mail-header-separator ""))

	  ;; mail-aliases nil = mail-abbrevs.el
	  (if (or feedmail-force-expand-mail-aliases
		  (and (fboundp 'expand-mail-aliases) mail-aliases))
	      (expand-mail-aliases (point-min) eoh-marker))

	  ;; make it pretty
	  (if feedmail-fill-to-cc (feedmail-fill-to-cc-function eoh-marker))
	  ;; ignore any blank lines in the header
	  (goto-char (point-min))
	  (while (and (re-search-forward "\n\n\n*" eoh-marker t) (< (point) eoh-marker))
	    (replace-match "\n"))

	  (let ((case-fold-search t) (addr-regexp))
	    (goto-char (point-min))
	    ;; there are some RFC-822 combinations/cases missed here,
	    ;; but probably good enough and what users expect
	    ;;
	    ;; use resent-* stuff only if there is at least one non-empty one
	    (setq feedmail-is-a-resend
		  (re-search-forward
		   ;; header name, followed by optional whitespace, followed by
		   ;; non-whitespace, followed by anything, followed by newline;
		   ;; the idea is empty Resent-* headers are ignored
		   "^\\(Resent-To:\\|Resent-Cc:\\|Resent-Bcc:\\)\\s-*\\S-+.*$"
		   eoh-marker t))
	    ;; if we say so, gather the Bcc stuff before the main course
	    (if (eq feedmail-deduce-bcc-where 'first)
		(progn (if feedmail-is-a-resend (setq addr-regexp a-re-rb) (setq addr-regexp a-re-db))
		       (setq feedmail-address-list (feedmail-deduce-address-list feedmail-prepped-text-buffer (point-min) eoh-marker addr-regexp feedmail-address-list))))
	    ;; the main course
	    (if (or (eq feedmail-deduce-bcc-where 'first) (eq feedmail-deduce-bcc-where 'last))
		;; handled by first or last cases, so don't get Bcc stuff
		(progn (if feedmail-is-a-resend (setq addr-regexp a-re-rtc) (setq addr-regexp a-re-dtc))
		       (setq feedmail-address-list (feedmail-deduce-address-list feedmail-prepped-text-buffer (point-min) eoh-marker addr-regexp feedmail-address-list)))
	      ;; not handled by first or last cases, so also get Bcc stuff
	      (progn (if feedmail-is-a-resend (setq addr-regexp a-re-rtcb) (setq addr-regexp a-re-dtcb))
		     (setq feedmail-address-list (feedmail-deduce-address-list feedmail-prepped-text-buffer (point-min) eoh-marker addr-regexp feedmail-address-list))))
	    ;; if we say so, gather the Bcc stuff after the main course
	    (if (eq feedmail-deduce-bcc-where 'last)
		(progn (if feedmail-is-a-resend (setq addr-regexp a-re-rb) (setq addr-regexp a-re-db))
		       (setq feedmail-address-list (feedmail-deduce-address-list feedmail-prepped-text-buffer (point-min) eoh-marker addr-regexp feedmail-address-list))))
	    (if (not feedmail-address-list) (error "FQM: Sending...abandoned, no addressees"))
	    ;; not needed, but meets user expectations
	    (setq feedmail-address-list (nreverse feedmail-address-list))
	    ;; Find and handle any Bcc fields.
	    (setq bcc-holder (feedmail-accume-n-nuke-header eoh-marker "^Bcc:"))
	    (setq resent-bcc-holder (feedmail-accume-n-nuke-header eoh-marker "^Resent-Bcc:"))
	    (if (and bcc-holder (not feedmail-nuke-bcc))
		(progn (goto-char (point-min))
		       (insert bcc-holder)))
	    (if (and resent-bcc-holder (not feedmail-nuke-resent-bcc))
		(progn (goto-char (point-min))
		       (insert resent-bcc-holder)))
	    (goto-char (point-min))

	    ;; fiddle about, fiddle about, fiddle about....
	    (feedmail-fiddle-from)
	    (feedmail-fiddle-sender)
	    (feedmail-fiddle-x-mailer)
	    (feedmail-fiddle-message-id
	     (or feedmail-queue-runner-is-active (buffer-file-name feedmail-raw-text-buffer)))
	    (feedmail-fiddle-date
	     (or feedmail-queue-runner-is-active (buffer-file-name feedmail-raw-text-buffer)))
	    (feedmail-fiddle-list-of-fiddle-plexes feedmail-fiddle-plex-user-list)

	    ;; don't send out a blank headers of various sorts
	    ;; (this loses on continued line with a blank first line)
	    (goto-char (point-min))
	    (and feedmail-nuke-empty-headers ; hey, who's an empty-header?
		 (while (re-search-forward "^[A-Za-z0-9-]+:[ \t]*\n" eoh-marker t)
		   (replace-match ""))))

	  (run-hooks 'feedmail-last-chance-hook)

	  (let ((fcc (feedmail-accume-n-nuke-header eoh-marker "^Fcc:"))
		(also-file)
		(confirm (cond
			  ((eq feedmail-confirm-outgoing 'immediate)
			   (not feedmail-queue-runner-is-active))
			  ((eq feedmail-confirm-outgoing 'queued) feedmail-queue-runner-is-active)
			  (t feedmail-confirm-outgoing))))
	    (if (or (not confirm) (feedmail-one-last-look feedmail-prepped-text-buffer))
		(let ((user-mail-address (feedmail-envelope-deducer eoh-marker)))
		  (feedmail-give-it-to-buffer-eater)
		  (if (and (not feedmail-queue-runner-is-active) (setq also-file (buffer-file-name feedmail-raw-text-buffer)))
		      (progn		; if a file but not running the queue, offer to delete it
			(setq also-file (expand-file-name also-file))
			(if (or feedmail-queue-auto-file-nuke
				(y-or-n-p (format "FQM: Delete message file %s? " also-file)))
			    (save-excursion
			      ;; if we delete the affiliated file, get rid
			      ;; of the file name association and make sure we
			      ;; don't annoy people with a prompt on exit
			      (delete-file also-file)
			      (set-buffer feedmail-raw-text-buffer)
			      (setq buffer-offer-save nil)
			      (setq buffer-file-name nil)
			      )
			  )))
		  (goto-char (point-min))
		  ;; re-insert and handle any Fcc fields (and, optionally, any Bcc).
		  (if fcc (let ((default-buffer-file-type feedmail-force-binary-write))
			    (insert fcc)
			    (if (not feedmail-nuke-bcc-in-fcc)
				(progn (if bcc-holder (insert bcc-holder))
				       (if resent-bcc-holder (insert resent-bcc-holder))))

			    (run-hooks 'feedmail-before-fcc-hook)

			    (if feedmail-nuke-body-in-fcc
				(progn (goto-char eoh-marker)
				       (if (natnump feedmail-nuke-body-in-fcc)
					   (forward-line feedmail-nuke-body-in-fcc))
				       (delete-region (point) (point-max))
				       ))
			    (mail-do-fcc eoh-marker)
			    )))
	      (error "FQM: Sending...abandoned") ; user bailed out of one-last-look
	      )))			; unwind-protect body (save-excursion)

      ;; unwind-protect cleanup forms
      (kill-buffer feedmail-prepped-text-buffer)
      (set-buffer feedmail-error-buffer)
      (if (zerop (buffer-size)) (kill-buffer feedmail-error-buffer)
	(progn (display-buffer feedmail-error-buffer)
	       ;; read fast ... the meter is running
	       (if (and feedmail-queue-runner-is-active feedmail-queue-chatty)
		   (progn (message "FQM: Sending...failed") (ding t) (sit-for 3)))
	       (error "FQM: Sending...failed")))
      (set-buffer feedmail-raw-text-buffer))
    )					; let
  (if (and feedmail-queue-chatty (not feedmail-queue-runner-is-active))
      (progn
	(feedmail-queue-reminder 'after-immediate)
	(sit-for feedmail-queue-chatty-sit-for)))
  )


(defun feedmail-fiddle-header (name value &optional action folding)
  "Internal feedmail function for jamming fields into message header.
NAME, VALUE, ACTION, and FOLDING are the four elements of a
fiddle-plex, as described in the documentation for the variable
feedmail-fiddle-plex-blurb."
  (let ((case-fold-search t)
	(header-colon (concat (regexp-quote name) ":"))
	header-regexp eoh-marker has-like ag-like val-like that-point)
    (setq header-regexp (concat "^" header-colon))
    (setq eoh-marker (feedmail-find-eoh))
    (goto-char (point-min))
    (setq has-like (re-search-forward header-regexp eoh-marker t))

    (if (not action) (setq action 'supplement))
    (cond
     ((eq action 'supplement)
      ;; trim leading/trailing whitespace
      (if (string-match "\\`[ \t\n]+" value)
	  (setq value (substring value (match-end 0))))
      (if (string-match "[ \t\n]+\\'" value)
	  (setq value (substring value 0 (match-beginning 0))))
      (if (> (length value) 0)
	  (progn
	    (if feedmail-fiddle-headers-upwardly
		(goto-char (point-min))
	      (goto-char eoh-marker))
	    (setq that-point (point))
	    (insert name ": " value "\n")
	    (if folding (feedmail-fill-this-one that-point (point))))))

     ((eq action 'replace)
      (if has-like (feedmail-accume-n-nuke-header eoh-marker header-regexp))
      (feedmail-fiddle-header name value 'supplement folding))

     ((eq action 'create)
      (if (not has-like) (feedmail-fiddle-header name value 'supplement folding)))

     ((eq action 'combine)
      (setq val-like (nth 1 value))
      (setq ag-like (or (feedmail-accume-n-nuke-header eoh-marker header-regexp) ""))
      ;; get rid of initial header name from first instance (front of string)
      (if (string-match (concat header-regexp "[ \t\n]+") ag-like)
	  (setq ag-like (replace-match "" t t ag-like)))
      ;; get rid of embedded header names from subsequent instances
      (while (string-match (concat "\n" header-colon "[ \t\n]+") ag-like)
	(setq ag-like (replace-match "\n\t" t t ag-like)))
      ;; trim leading/trailing whitespace
      (if (string-match "\\`[ \t\n]+" ag-like)
	  (setq ag-like (substring ag-like (match-end 0))))
      (if (string-match "[ \t\n]+\\'" ag-like)
	  (setq ag-like (substring ag-like 0 (match-beginning 0))))
      ;; if ag-like is not nil and not an empty string, transform it via a function
      ;; call or format operation
      (if (> (length ag-like) 0)
	  (setq ag-like
		(cond
		 ((and (symbolp val-like) (fboundp val-like))
		  (funcall val-like name ag-like))
		 ((stringp val-like)
		  (format val-like ag-like))
		 (t nil))))
      (feedmail-fiddle-header name (concat (nth 0 value) ag-like (nth 2 value)) 'supplement folding)))
    ))

(defun feedmail-give-it-to-buffer-eater ()
  (save-excursion
    (if feedmail-enable-spray
	(mapcar
	 '(lambda (feedmail-spray-this-address)
	    (let ((spray-buffer (get-buffer-create " *FQM Outgoing Email Spray*")))
	      (save-excursion
		(set-buffer spray-buffer)
		(erase-buffer)
		;; not life's most efficient methodology, but spraying isn't
		;; an every-5-minutes event either
		(insert-buffer feedmail-prepped-text-buffer)
		;; There's a good case to me made that each separate transmission of
		;; a message in the spray should have a distinct Message-Id:.  There
		;; is also a less compelling argument in the other direction.  I think
		;; they technically should have distinct Message-Id:s, but I doubt that
		;; anyone cares, practically.  If someone complains about it, I'll add
		;; it.
		(feedmail-fiddle-list-of-spray-fiddle-plexes feedmail-spray-address-fiddle-plex-list)
		;; this (let ) is just in case some buffer eater
		;; is cheating and using the global variable name instead
		;; of its argument to find the buffer
		(let ((feedmail-prepped-text-buffer spray-buffer))
		  (funcall feedmail-buffer-eating-function
			   feedmail-prepped-text-buffer
			   feedmail-error-buffer
			   (list feedmail-spray-this-address))))
	      (kill-buffer spray-buffer)
	      ))
	 feedmail-address-list)
      (funcall feedmail-buffer-eating-function
	       feedmail-prepped-text-buffer
	       feedmail-error-buffer
	       feedmail-address-list))))


(defun feedmail-envelope-deducer (eoh-marker)
  "If feedmail-deduce-envelope-from is false, simply return `user-mail-address'.
Else, look for Sender: or From: (or Resent-*) and
return that value."
  (if (not feedmail-deduce-envelope-from)
      user-mail-address
    (let ((from-list))
      (setq from-list
	    (feedmail-deduce-address-list
	     (current-buffer) (point-min) eoh-marker (if feedmail-is-a-resend "^Resent-Sender:" "^Sender:")
	     from-list))
      (if (not from-list)
	  (setq from-list
		(feedmail-deduce-address-list
		 (current-buffer) (point-min) eoh-marker (if feedmail-is-a-resend "^Resent-From:" "^From:")
		 from-list)))
      (if (and from-list (car from-list)) (car from-list) user-mail-address))))


(defun feedmail-fiddle-from ()
  "Fiddle From:."
  ;; default is to fall off the end of the list and do nothing
  (cond
   ;; nil means do nothing
   ((eq nil feedmail-from-line) nil)
   ;; t is the same a using the default computation, so compute it and recurse
   ;; user-full-name suggested by kpc@ptolemy.arc.nasa.gov (=Kimball Collins)
   ;; improvement using user-mail-address suggested by
   ;;   gray@austin.apc.slb.com (Douglas Gray Stephens)
   ((eq t feedmail-from-line)
    (let ((feedmail-from-line
	   (let ((at-stuff
		  (if user-mail-address user-mail-address (concat (user-login-name) "@" (system-name)))))
	     (cond
	      ((eq mail-from-style nil) at-stuff)
	      ((eq mail-from-style 'parens) (concat at-stuff " (" (user-full-name) ")"))
	      ((eq mail-from-style 'angles) (concat "\"" (user-full-name) "\" <" at-stuff ">"))
	      ))))
      (feedmail-fiddle-from)))

   ;; if it's a string, simply make a fiddle-plex out of it and recurse
   ((stringp feedmail-from-line)
    (let ((feedmail-from-line (list "ignored" feedmail-from-line 'create)))
      (feedmail-fiddle-from)))

   ;; if it's a function, call it and recurse with the resulting value
   ((and (symbolp feedmail-from-line) (fboundp feedmail-from-line))
    (let ((feedmail-from-line (funcall feedmail-from-line)))
      (feedmail-fiddle-from)))

   ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
   ((listp feedmail-from-line)
    (feedmail-fiddle-header
     (if feedmail-is-a-resend "Resent-From" "From")
     (nth 1 feedmail-from-line)		; value
     (nth 2 feedmail-from-line)		; action
     (nth 3 feedmail-from-line)))))	; folding


(defun feedmail-fiddle-sender ()
  "Fiddle Sender:."
  ;; default is to fall off the end of the list and do nothing
  (cond
   ;; nil means do nothing
   ((eq nil feedmail-sender-line) nil)
   ;; t is not allowed, but handled it just to avoid bugs later
   ((eq t feedmail-sender-line) nil)

   ;; if it's a string, simply make a fiddle-plex out of it and recurse
   ((stringp feedmail-sender-line)
    (let ((feedmail-sender-line (list "ignored" feedmail-sender-line 'create)))
      (feedmail-fiddle-sender)))

   ;; if it's a function, call it and recurse with the resulting value
   ((and (symbolp feedmail-sender-line) (fboundp feedmail-sender-line))
    (let ((feedmail-sender-line (funcall feedmail-sender-line)))
      (feedmail-fiddle-sender)))

   ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
   ((listp feedmail-sender-line)
    (feedmail-fiddle-header
     (if feedmail-is-a-resend "Resent-Sender" "Sender")
     (nth 1 feedmail-sender-line)	; value
     (nth 2 feedmail-sender-line)	; action
     (nth 3 feedmail-sender-line)))))	; folding


(defun feedmail-default-date-generator (maybe-file)
  "Default function for generating Date: header contents."
  (let ((date-time))
    (if (and (not feedmail-queue-use-send-time-for-date) maybe-file)
	(setq date-time (nth 5 (file-attributes maybe-file))))
    (feedmail-rfc822-date date-time))
  )


(defun feedmail-fiddle-date (maybe-file)
  "Fiddle Date:.  See documentation of feedmail-date-generator."
  ;; default is to fall off the end of the list and do nothing
  (cond
   ;; nil means do nothing
   ((eq nil feedmail-date-generator) nil)
   ;; t is the same a using the function feedmail-default-date-generator, so let it and recurse
   ((eq t feedmail-date-generator)
    (let ((feedmail-date-generator (feedmail-default-date-generator maybe-file)))
      (feedmail-fiddle-date maybe-file)))

   ;; if it's a string, simply make a fiddle-plex out of it and recurse
   ((stringp feedmail-date-generator)
    (let ((feedmail-date-generator (list "ignored" feedmail-date-generator 'create)))
      (feedmail-fiddle-date maybe-file)))

   ;; if it's a function, call it and recurse with the resulting value
   ((and (symbolp feedmail-date-generator) (fboundp feedmail-date-generator))
    (let ((feedmail-date-generator (funcall feedmail-date-generator maybe-file)))
      (feedmail-fiddle-date maybe-file)))

   ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
   ((listp feedmail-date-generator)
    (feedmail-fiddle-header
     (if feedmail-is-a-resend "Resent-Date" "Date")
     (nth 1 feedmail-date-generator)	; value
     (nth 2 feedmail-date-generator)	; action
     (nth 3 feedmail-date-generator))))) ; folding


(defun feedmail-default-message-id-generator (maybe-file)
  "Default function for generating Message-Id: header contents.
Based on a date and a sort of random number for tie breaking.  Unless
feedmail-message-id-suffix is defined, uses `user-mail-address', so be
sure it's set."
  (let ((date-time)
	(end-stuff (if feedmail-message-id-suffix feedmail-message-id-suffix user-mail-address)))
    (if (string-match "^\\(.*\\)@" end-stuff)
	(setq end-stuff
	      (concat (if (equal (match-beginning 1) (match-end 1)) "" "-") end-stuff))
      (setq end-stuff (concat "@" end-stuff)))
    (if (and (not feedmail-queue-use-send-time-for-message-id) maybe-file)
	(setq date-time (nth 5 (file-attributes maybe-file))))
    (format "<%d-%s%s%s>"
	    (mod (random) 10000)
	    (format-time-string "%a%d%b%Y%H%M%S" date-time)
	    (feedmail-rfc822-time-zone date-time)
	    end-stuff))
  )

(defun feedmail-fiddle-message-id (maybe-file)
  "Fiddle Message-Id:.  See documentation of feedmail-message-id-generator."
  ;; default is to fall off the end of the list and do nothing
  (cond
   ;; nil means do nothing
   ((eq nil feedmail-message-id-generator) nil)
   ;; t is the same a using the function feedmail-default-message-id-generator, so let it and recurse
   ((eq t feedmail-message-id-generator)
    (let ((feedmail-message-id-generator (feedmail-default-message-id-generator maybe-file)))
      (feedmail-fiddle-message-id maybe-file)))

   ;; if it's a string, simply make a fiddle-plex out of it and recurse
   ((stringp feedmail-message-id-generator)
    (let ((feedmail-message-id-generator (list "ignored" feedmail-message-id-generator 'create)))
      (feedmail-fiddle-message-id maybe-file)))

   ;; if it's a function, call it and recurse with the resulting value
   ((and (symbolp feedmail-message-id-generator) (fboundp feedmail-message-id-generator))
    (let ((feedmail-message-id-generator (funcall feedmail-message-id-generator maybe-file)))
      (feedmail-fiddle-message-id maybe-file)))

   ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
   ((listp feedmail-message-id-generator)
    (feedmail-fiddle-header
     (if feedmail-is-a-resend "Resent-Message-Id" "Message-Id")
     (nth 1 feedmail-message-id-generator) ; value
     (nth 2 feedmail-message-id-generator) ; action
     (nth 3 feedmail-message-id-generator))))) ; folding


(defun feedmail-default-x-mailer-generator ()
  "Default function for generating X-Mailer: header contents."
  (concat
   (let ((case-fold-search t)) (if (string-match "emacs" emacs-version) "" "emacs "))
   emacs-version " (via feedmail " feedmail-patch-level
   (if feedmail-queue-runner-is-active " Q" " I")
   (if feedmail-enable-spray "S" "")
   (if feedmail-x-mailer-line-user-appendage ") " ")")
   feedmail-x-mailer-line-user-appendage))


(defun feedmail-fiddle-x-mailer ()
  "Fiddle X-Mailer:.  See documentation of feedmail-x-mailer-line."
  ;; default is to fall off the end of the list and do nothing
  (cond
   ;; t is the same a using the function feedmail-default-x-mailer-generator, so let it and recurse
   ((eq t feedmail-x-mailer-line)
    (let ((feedmail-x-mailer-line (feedmail-default-x-mailer-generator)))
      (feedmail-fiddle-x-mailer)))

   ;; if it's a string, simply make a fiddle-plex out of it and recurse
   ((stringp feedmail-x-mailer-line)
    (let ((feedmail-x-mailer-line (list "ignored" (list feedmail-x-mailer-line ";\n\t%s") 'combine)))
      (feedmail-fiddle-x-mailer)))

   ;; if it's a function, call it and recurse with the resulting value
   ((and (symbolp feedmail-x-mailer-line) (fboundp feedmail-x-mailer-line))
    (let ((feedmail-x-mailer-line (funcall feedmail-x-mailer-line)))
      (feedmail-fiddle-x-mailer)))

   ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
   ((listp feedmail-x-mailer-line)
    (feedmail-fiddle-header
     (if feedmail-is-a-resend "X-Resent-Mailer" "X-Mailer")
     (nth 1 feedmail-x-mailer-line)	; value
     (nth 2 feedmail-x-mailer-line)	; action
     (nth 3 feedmail-x-mailer-line)))))	; folding


(defun feedmail-fiddle-spray-address (addy-plex)
  "Fiddle header for single spray address.  Uses feedmail-spray-this-address."
  ;; default is to fall off the end of the list and do nothing
  (cond
   ;; nil means do nothing
   ((eq nil addy-plex) nil)
   ;; t means the same as using "To:" and unembellished addy
   ((eq t addy-plex)
    (let ((addy-plex (list "To" feedmail-spray-this-address)))
      (feedmail-fiddle-spray-address addy-plex)))

   ;; if it's a string, simply make a fiddle-plex out of it and recurse, assuming
   ;; the string names a header field (e.g., "To")
   ((stringp addy-plex)
    (let ((addy-plex (list addy-plex feedmail-spray-this-address)))
      (feedmail-fiddle-spray-address addy-plex)))

   ;; if it's a function, call it and recurse with the resulting value
   ((and (symbolp addy-plex) (fboundp addy-plex))
    (let ((addy-plex (funcall addy-plex)))
      (feedmail-fiddle-spray-address addy-plex)))

   ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
   ((listp addy-plex)
    (feedmail-fiddle-header
     (nth 0 addy-plex)			; name
     (nth 1 addy-plex)			; value
     (nth 2 addy-plex)			; action
     (nth 3 addy-plex)))))		; folding


(defun feedmail-fiddle-list-of-spray-fiddle-plexes (list-of-fiddle-plexes)
  "Fiddling based on a list of fiddle-plexes for spraying."
  ;; default is to fall off the end of the list and do nothing
  (let ((lofp list-of-fiddle-plexes) fp)
    (if (listp lofp)
	(while lofp
	  (setq fp (car lofp))
	  (setq lofp (cdr lofp))
	  (feedmail-fiddle-spray-address fp))
      (feedmail-fiddle-spray-address lofp))))


(defun feedmail-fiddle-list-of-fiddle-plexes (list-of-fiddle-plexes)
  "Fiddling based on a list of fiddle-plexes.  Values t, nil, and string are pointless."
  ;; default is to fall off the end of the list and do nothing
  (let ((lofp list-of-fiddle-plexes) fp)
    (while lofp
      (setq fp (car lofp))
      (setq lofp (cdr lofp))
      (cond

       ;; if it's a function, call it and recurse with the resulting value
       ((and (symbolp fp) (fboundp fp))
	(let ((lofp (list (funcall fp)))) (feedmail-fiddle-list-of-fiddle-plexes lofp)))

       ;; if it's a list, it must be a fiddle-plex -- so fiddle, man, fiddle
       ((listp fp)
	(feedmail-fiddle-header
	 (nth 0 fp)
	 (nth 1 fp)			; value
	 (nth 2 fp)			; action
	 (nth 3 fp)))))))		; folding


(defun feedmail-accume-n-nuke-header (header-end header-regexp)
  "Delete headers matching a regexp and their continuation lines.
There may be multiple such lines, and each may have arbitrarily
many continuation lines.  Return an accumulation of the deleted
headers, including the intervening newlines."
  (let ((case-fold-search t) (dropout))
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all matching lines
      (while (re-search-forward header-regexp header-end t)
	(forward-line 1)
	(setq dropout (concat dropout (buffer-substring (match-beginning 0) (point))))
	(delete-region (match-beginning 0) (point))
	;; get rid of any continuation lines
	(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
	  (forward-line 1)
	  (setq dropout (concat dropout (buffer-substring (match-beginning 0) (point))))
	  (replace-match ""))))
    (identity dropout)))

(defun feedmail-fill-to-cc-function (header-end)
  "Smart filling of address headers (don't be fooled by the name).
The filling tries to avoid splitting lines except at commas.  This
avoids, in particular, splitting within parenthesized comments in
addresses.  Headers filled include From:, Reply-To:, To:, Cc:, Bcc:,
Resent-To:, Resent-Cc:, and Resent-Bcc:."
  (let ((case-fold-search t)
	this-line
	this-line-end)
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all To:/Cc:, etc, lines
      (while
	  (re-search-forward
	   "^\\(From:\\|Reply-To:\\|To:\\|Cc:\\|Bcc:\\|Resent-To:\\|Resent-Cc:\\|Resent-Bcc:\\)"
	   header-end t)
	(setq this-line (match-beginning 0))
	;; replace 0 or more leading spaces with a single space
	(and (looking-at "[ \t]*") (replace-match " "))
	(forward-line 1)
	;; get any continuation lines
	(while (and (looking-at "[ \t]+") (< (point) header-end))
	  (forward-line 1))
	(setq this-line-end (point-marker))
	(save-excursion (feedmail-fill-this-one this-line this-line-end))
	))))


(defun feedmail-fill-this-one (this-line this-line-end)
  "In-place smart filling of the region bounded by the two arguments."
  (let ((fill-prefix "\t")
	(fill-column feedmail-fill-to-cc-fill-column))
    ;; The general idea is to break only on commas.  Collapse
    ;; multiple whitespace to a single blank; change
    ;; all the blanks to something unprintable; change the
    ;; commas to blanks; fill the region; change it back.
    (goto-char this-line)
    (while (re-search-forward "\\s-+" (1- this-line-end) t)
      (replace-match " "))

    (subst-char-in-region this-line this-line-end ?   2 t) ; blank->C-b
    (subst-char-in-region this-line this-line-end ?, ?  t) ; comma->blank

    (fill-region-as-paragraph this-line this-line-end)

    (subst-char-in-region this-line this-line-end ?  ?, t) ; comma<-blank
    (subst-char-in-region this-line this-line-end  2 ?  t) ; blank<-C-b

    ;; look out for missing commas before continuation lines
    (goto-char this-line)
    (while (re-search-forward "\\([^,]\\)\n\t[ ]*" this-line-end t)
      (replace-match "\\1,\n\t"))
    ))


(require 'mail-utils)			; pick up mail-strip-quoted-names
(defun feedmail-deduce-address-list (message-buffer header-start header-end addr-regexp address-list)
  "Get address list with all comments and other excitement trimmed.
Addresses are collected only from headers whose names match the fourth
argument Returns a list of strings.  Duplicate addresses will have
been weeded out."
  (let ((simple-address)
	(address-blob)
	(this-line)
	(this-line-end))
    (unwind-protect
	(save-excursion
	  (set-buffer (get-buffer-create " *FQM scratch*")) (erase-buffer)
	  (insert-buffer-substring message-buffer header-start header-end)
	  (goto-char (point-min))
	  (let ((case-fold-search t))
	    (while (re-search-forward addr-regexp (point-max) t)
	      (replace-match "")
	      (setq this-line (match-beginning 0))
	      (forward-line 1)
	      ;; get any continuation lines
	      (while (and (looking-at "^[ \t]+") (< (point) (point-max)))
		(forward-line 1))
	      (setq this-line-end (point-marker))
	      ;; only keep if we don't have it already
	      (setq address-blob
		    (mail-strip-quoted-names (buffer-substring this-line this-line-end)))
	      (while (string-match "\\([, \t\n\r]*\\)\\([^, \t\n\r]+\\)" address-blob)
		(setq simple-address (substring address-blob (match-beginning 2) (match-end 2)))
		(setq address-blob (replace-match "" t t address-blob))
		(if (not (member simple-address address-list))
		    (add-to-list 'address-list simple-address)))
	      ))
	  (kill-buffer nil)))
    (identity address-list)))


(defun feedmail-one-last-look (feedmail-prepped-text-buffer)
  "Offer the user one last chance to give it up."
  (save-excursion
    (save-window-excursion
      (switch-to-buffer feedmail-prepped-text-buffer)
      (if (and (fboundp 'y-or-n-p-with-timeout) (numberp feedmail-confirm-outgoing-timeout))
	  (y-or-n-p-with-timeout
	   "FQM: Send this email? "
	   (abs feedmail-confirm-outgoing-timeout)
	   (> feedmail-confirm-outgoing-timeout 0))
	(y-or-n-p "FQM: Send this email? "))
      )))

(defun feedmail-fqm-p (might-be)
  "Internal; does filename end with FQM suffix?"
  (string-match (concat (regexp-quote feedmail-queue-fqm-suffix) "$") might-be))


(defun feedmail-find-eoh (&optional noerror)
  "Internal; finds the end of message header fields, returns mark just before it"
  (save-excursion
    (goto-char (point-min))
    (when (or (re-search-forward (concat "^"
					 (regexp-quote mail-header-separator)
					 "\n")
				 nil noerror)
	      (and feedmail-queue-alternative-mail-header-separator
		   (re-search-forward
		    (concat "^"
			    (regexp-quote
			     feedmail-queue-alternative-mail-header-separator)
			    "\n")
		    nil noerror)))
      (forward-line -1)
      (point-marker))))

(provide 'feedmail)

;;; feedmail.el ends here
