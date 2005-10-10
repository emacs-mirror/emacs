;;; mh-customize.el --- MH-E customization

;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;; All of the defgroups, defcustoms, and deffaces in MH-E are found
;; here. This makes it possible to customize modules that aren't loaded
;; yet. It also makes it easier to organize the customization groups.

;; This file contains the following sections:
;;
;; 1. MH-E Customization Groups
;;
;;    These are the customization group definitions. Every group has a
;;    associated manual node. The ordering is alphabetical, except for the
;;    groups mh-faces and mh-hooks which are last .
;;
;; 2. MH-E Customization
;;
;;    These are the actual customization variables. There is a sub-section for
;;    each group in the MH-E Customization Groups section, in the same order,
;;    separated by page breaks. Within each section, variables are sorted
;;    alphabetically.
;;
;; 3. Hooks
;;
;;    All hooks must be placed in the mh-hook group; in addition, add the
;;    group associated with the manual node in which the hook is described.
;;    Since the mh-hook group appears near the end of this file, the hooks
;;    will appear at the end of these other groups.
;;
;; 4. Faces
;;
;;    Create a new face group if necessary; in this case, add the group
;;    associated with the manual node in which the faces are described to the
;;    faces' group definition. Since the face groups appear last, the face
;;    groups will appear at the end of these other groups.
;;
;;; Change Log:

;;; Code:

(provide 'mh-customize)

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-loaddefs)

(eval-and-compile
  (defvar mh-xemacs-flag (featurep 'xemacs)
    "Non-nil means the current Emacs is XEmacs."))

(when mh-xemacs-flag
  (require 'mh-xemacs))

;; XXX: Functions autoloaded from the following files are used to initialize
;;  customizable variables. They are require'd here, since otherwise the
;;  corresponding .elc would be loaded at compile time.
(eval-when-compile
  (require 'mh-init)
  (require 'mh-identity))

(defun mh-customize (&optional delete-other-windows-flag)
  "Customize MH-E variables.
If optional argument DELETE-OTHER-WINDOWS-FLAG is non-nil, other windows in
the frame are removed."
  (interactive "P")
  (customize-group 'mh-e)
  (when delete-other-windows-flag
    (delete-other-windows)))



;;; For compiler warnings...
(eval-when-compile
  (defvar mh-show-buffer)
  (defvar mh-show-folder-buffer))

;;; MH-E Customization Groups

(defgroup mh-e nil
  "Emacs interface to the MH mail system.
MH is the Rand Mail Handler. Other implementations include nmh and GNU
mailutils."
  :link '(custom-manual "(mh-e)Top")
  :group 'mail)

(defgroup mh-alias nil
  "Aliases."
  :link '(custom-manual "(mh-e)Aliases")
  :prefix "mh-alias-"
  :group 'mh-e)

(defgroup mh-folder nil
  "Organizing your mail with folders."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Organizing")
  :group 'mh-e)

(defgroup mh-folder-selection nil
  "Folder selection."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Folder Selection")
  :group 'mh-e)

(defgroup mh-identity nil
  "Identities."
  :link '(custom-manual "(mh-e)Identities")
  :prefix "mh-identity-"
  :group 'mh-e)

(defgroup mh-inc nil
  "Incorporating your mail."
  :prefix "mh-inc-"
  :link '(custom-manual "(mh-e)Incorporating Mail")
  :group 'mh-e)

(defgroup mh-index nil
  "Searching."
  :link '(custom-manual "(mh-e)Searching")
  :prefix "mh-index-"
  :group 'mh-e)

(defgroup mh-junk nil
  "Dealing with junk mail."
  :link '(custom-manual "(mh-e)Junk")
  :prefix "mh-junk-"
  :group 'mh-e)

(defgroup mh-letter nil
  "Editing a draft."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Editing Drafts")
  :group 'mh-e)

(defgroup mh-ranges nil
  "Ranges."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Ranges")
  :group 'mh-e)

(defgroup mh-scan-line-formats nil
  "Scan line formats."
  :link '(custom-manual "(mh-e)Scan Line Formats")
  :prefix "mh-"
  :group 'mh-e)

(defgroup mh-sending-mail nil
  "Sending mail."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Sending Mail")
  :group 'mh-e)

(defgroup mh-sequences nil
  "Sequences."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Sequences")
  :group 'mh-e)

(defgroup mh-show nil
  "Reading your mail."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Reading Mail")
  :group 'mh-e)

(defgroup mh-speed nil
  "The speedbar."
  :prefix "mh-speed-"
  :link '(custom-manual "(mh-e)Speedbar")
  :group 'mh-e)

(defgroup mh-toolbar nil
  "The toolbar"
  :link '(custom-manual "(mh-e)Toolbar")
  :prefix "mh-"
  :group 'mh-e)

(defgroup mh-faces nil
  "Faces used in MH-E."
  :link '(custom-manual "(mh-e)Top")
  :prefix "mh-"
  :group 'faces
  :group 'mh-e)

(defgroup mh-hooks nil
  "MH-E hooks."
  :link '(custom-manual "(mh-e)Top")
  :prefix "mh-"
  :group 'mh-e)

;;; Faces

(defgroup mh-folder-faces nil
  "Faces used in scan listing."
  :link '(custom-manual "(mh-e)Organizing")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-show)

(defgroup mh-index-faces nil
  "Faces used in searching."
  :link '(custom-manual "(mh-e)Searching")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-index)

(defgroup mh-letter-faces nil
  "Faces used in message drafts."
  :link '(custom-manual "(mh-e)Sending Mail")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-letter)

(defgroup mh-show-faces nil
  "Faces used in message display."
  :link '(custom-manual "(mh-e)Reading Mail")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-show)

(defgroup mh-speed-faces nil
  "Faces used in speedbar."
  :link '(custom-manual "(mh-e)Speedbar")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-speed)



;;; Emacs interface to the MH mail system (:group mh)
(eval-when (compile)
  (setq mh-variant 'none))

(defcustom mh-variant 'autodetect
  "*Specifies the variant used by MH-E.

The default setting of this option is `Auto-detect' which means that MH-E will
automatically choose the first of nmh, MH, or GNU mailutils that it finds in
the directories listed in `mh-path', `mh-sys-path', and `exec-path'. If, for
example, you have both nmh and mailutils installed and `mh-variant-in-use' was
initialized to nmh but you want to use mailutils, then you can set this option
to `mailutils'.

When this variable is changed, MH-E resets `mh-progs', `mh-lib',
`mh-lib-progs', `mh-flists-present-flag', and `mh-variant-in-use'
accordingly."
  :type `(radio
          (const :tag "Auto-detect" autodetect)
          ,@(mapcar (lambda (x) `(const ,(car x))) (mh-variants)))
  :set (lambda (symbol value)
         (set-default symbol value)     ;Done in mh-variant-set-variant!
         (mh-variant-set value))
  :group 'mh-e)



;;; Aliases (:group 'mh-alias)

(defcustom mh-alias-completion-ignore-case-flag t
  "*Non-nil means don't consider case significant in MH alias completion.
As MH ignores case in the aliases, so too does MH-E. However, you may turn
this option off to make case significant which can be used to segregate
completion of your aliases. You might use lowercase for mailing lists and
uppercase for people."
  :type 'boolean
  :group 'mh-alias)

(defcustom mh-alias-expand-aliases-flag nil
  "*Non-nil means to expand aliases entered in the minibuffer.
In other words, aliases entered in the minibuffer will be expanded to the full
address in the message draft. By default, this expansion is not performed."
  :type 'boolean
  :group 'mh-alias)

(defcustom mh-alias-flash-on-comma t
  "*Specify whether to flash address or warn on translation.
This option controls the behavior when a [comma] is pressed while entering
aliases or addresses. The default setting flashes the address associated with
an address in the minibuffer briefly, but does not display a warning if the
alias is not found."
  :type '(choice (const :tag "Flash but Don't Warn If No Alias" t)
                 (const :tag "Flash and Warn If No Alias" 1)
                 (const :tag "Don't Flash Nor Warn If No Alias" nil))
  :group 'mh-alias)

(defcustom mh-alias-insert-file nil
  "*Filename used to store a new MH-E alias.
The default setting of this option is `Use Aliasfile Profile Component'. This
option can also hold the name of a file or a list a file names. If this option
is set to a list of file names, or the `Aliasfile:' profile component contains
more than one file name, MH-E will prompt for one of them when MH-E adds an
alias."
  :type '(choice (const :tag "Use Aliasfile Profile Component" nil)
                 (file :tag "Alias File")
                 (repeat :tag "List of Alias Files" file))
  :group 'mh-alias)

(defcustom mh-alias-insertion-location 'sorted
  "Specifies where new aliases are entered in alias files.
This option is set to `Alphabetical' by default. If you organize your alias
file in other ways, then adding aliases to the `Top' or `Bottom' of your alias
file might be more appropriate."
  :type '(choice (const :tag "Alphabetical" sorted)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'mh-alias)

(defcustom mh-alias-local-users t
  "*If on, local users are added to alias completion.

Aliases are created from `/etc/passwd' entries with a user ID larger than
a magical number, typically 200. This can be a handy tool on a machine where
you and co-workers exchange messages. These aliases have the form
`local.first.last' if a real name is present in the password file.
Otherwise, the alias will have the form `local.login'.

If you're on a system with thousands of users you don't know, and the loading
of local aliases slows MH-E down noticeably, then turn this option off.

This option also takes a string which is executed to generate the password
file. For example, use \"ypcat passwd\" to obtain the NIS password file."
  :type '(choice (boolean) (string))
  :group 'mh-alias)

(defcustom mh-alias-local-users-prefix "local."
  "*String prepended to the real names of users from the password file.
This option can also be set to `Use Login'.

For example, consider the following password file entry:

    psg:x:1000:1000:Peter S Galbraith,,,:/home/psg:/bin/tcsh

The following settings of this option will produce the associated aliases:

    \"local.\"                  local.peter.galbraith
    \"\"                        peter.galbraith
    Use Login                   psg

This option has no effect if variable `mh-alias-local-users' is turned off."
  :type '(choice (const :tag "Use Login" nil)
                 (string))
  :group 'mh-alias)

(defcustom mh-alias-passwd-gecos-comma-separator-flag t
  "*Non-nil means the gecos field in the password file uses a comma separator.
In the example in `mh-alias-local-users-prefix', commas are used to separate
different values within the so-called gecos field. This is a fairly common
usage. However, in the rare case that the gecos field in your password file is
not separated by commas and whose contents may contain commas, you can turn
this option off."
  :type 'boolean
  :group 'mh-alias)



;;; Organizing Your Mail with Folders (:group 'mh-folder)

(defcustom mh-recenter-summary-flag nil
  "*Non-nil means to recenter the summary window.
If this option is turned on, recenter the summary window when the show window
is toggled off."
  :type 'boolean
  :group 'mh-folder)



;;; Folder Selection (:group 'mh-folder-selection)

(defcustom mh-default-folder-for-message-function nil
  "Function to select a default folder for refiling or `Fcc'.
The current buffer is set to the message being refiled with point at the start
of the message. This function should return the default folder as a string
with a leading `+' sign. It can also return nil so that the last folder name
is used as the default, or an empty string to suppress the default entirely."
  :type 'function
  :group 'mh-folder-selection)

(defcustom mh-default-folder-list nil
  "*List of addresses and folders.
The folder name associated with the first address found in this list is used
as the default for `mh-refile-msg' and similar functions. Each element in this
list contains a `Check Recipient' item. If this item is turned on, then the
address is checked against the recipient instead of the sender. This is useful
for mailing lists.

See `mh-prompt-for-refile-folder' and `mh-folder-from-address' for more
information."
  :type '(repeat (list (regexp :tag "Address")
                       (string :tag "Folder")
                       (boolean :tag "Check Recipient")))
  :group 'mh-folder-selection)

(defcustom mh-default-folder-must-exist-flag t
  "*Non-nil means guessed folder name must exist to be used.
If the derived folder does not exist, and this option is on, then the last
folder name used is suggested. This is useful if you get mail from various
people for whom you have an alias, but file them all in the same project
folder.

See `mh-prompt-for-refile-folder' and `mh-folder-from-address' for more
information."
  :type 'boolean
  :group 'mh-folder-selection)

(defcustom mh-default-folder-prefix ""
  "*Prefix used for folder names generated from aliases.
The prefix is used to prevent clutter in your mail directory.

See `mh-prompt-for-refile-folder' and `mh-folder-from-address' for more
information."
  :type 'string
  :group 'mh-folder-selection)



;;; Identities (:group 'mh-identity)

(defcustom mh-identity-list nil
  "*List of identities.

To customize this option, click on the `INS' button and enter a label such as
`Home' or `Work'. Then click on the `INS' button with the label `Add at least
one item below'. Then choose one of the items in the `Value Menu'.

You can specify an alternate `From:' header field using the `From Field' menu
item. You must include a valid email address. A standard format is `First Last
<login@@host.domain>'. If you use an initial with a period, then you must
quote your name as in `\"First I. Last\" <login@@host.domain>'. People usually
list the name of the company where they work using the `Organization Field'
menu item. Set any arbitrary header field and value in the `Other Field' menu
item. Unless the header field is a standard one, precede the name of your
field's label with `X-', as in `X-Fruit-of-the-Day:'. The value of
`Attribution Verb' overrides the setting of
`mh-extract-from-attribution-verb'. Set your signature with the `Signature'
menu item. You can specify the contents of `mh-signature-file-name', a file,
or a function. Specify a different key to sign or encrypt messages with the
`GPG Key ID' menu item.

You can select the identities you have added via the menu called `Identity' in
the MH-Letter buffer. You can also use \\[mh-insert-identity]. To clear the
fields and signature added by the identity, select the `None' identity.

The `Identity' menu contains two other items to save you from having to set
the identity on every message. The menu item `Set Default for Session' can be
used to set the default identity to the current identity until you exit Emacs.
The menu item `Save as Default' sets the option `mh-identity-default' to the
current identity setting. You can also customize the `mh-identity-default'
option in the usual fashion."
  :type '(repeat (list :tag ""
                       (string :tag "Label")
                       (repeat :tag "Add at least one item below"
                               (choice
                                (cons :tag "From Field"
                                      (const "From")
                                      (string :tag "Value"))
                                (cons :tag "Organization Field"
                                      (const "Organization")
                                      (string :tag "Value"))
                                (cons :tag "Other Field"
                                      (string :tag "Field")
                                      (string :tag "Value"))
                                (cons :tag "Attribution Verb"
                                      (const ":attribution-verb")
                                      (string :tag "Value"))
                                (cons :tag "Signature"
                                      (const :tag "Signature"
                                             ":signature")
                                      (choice
                                       (const :tag "mh-signature-file-name"
                                              nil)
                                       (file)
                                       (function)))
                                (cons :tag "GPG Key ID"
                                      (const :tag "GPG Key ID"
                                             ":pgg-default-user-id")
                                      (string :tag "Value"))))))
  :set 'mh-identity-list-set
  :group 'mh-identity)

(defcustom mh-auto-fields-list nil
  "List of recipients for which header lines are automatically inserted.

This option can be used to set the identity depending on the recipient. To
customize this option, click on the `INS' button and enter a regular
expression for the recipient's address. Click on the `INS' button with the
`Add at least one item below' label. Then choose one of the items in the
`Value Menu'.

The `Identity' menu item is used to select an identity from those configured
in `mh-identity-list'. All of the information for that identity will be added
if the recipient matches. The `Fcc Field' menu item is used to select a folder
that is used in the `Fcc:' header. When you send the message, MH will put a
copy of your message in this folder. The `Mail-Followup-To Field' menu item is
used to insert an `Mail-Followup-To:' header field with the recipients you
provide. If the recipient's mail user agent supports this header field (as nmh
does), then their replies will go to the addresses listed. This is useful if
their replies go both to the list and to you and you don't have a mechanism to
suppress duplicates. If you reply to someone not on the list, you must either
remove the `Mail-Followup-To:' field, or ensure the recipient is also listed
there so that he receives replies to your reply. Other header fields may be
added using the `Other Field' menu item.

These fields can only be added after the recipient is known. Once the header
contains one or more recipients, run the \\[mh-insert-auto-fields] command or
choose the `Identity -> Insert Auto Fields' menu item to insert these fields
manually. However, you can just send the message and the fields will be added
automatically. You are given a chance to see these fields and to confirm them
before the message is actually sent. You can do away with this confirmation by
turning off the option `mh-auto-fields-prompt-flag'.

You should avoid using the same header field in `mh-auto-fields-list' and
`mh-identity-list' definitions that may apply to the same message as the
result is undefined."
  :type `(repeat
          (list :tag ""
                (string :tag "Recipient")
                (repeat :tag "Add at least one item below"
                        (choice
                         (cons :tag "Identity"
                               (const ":identity")
                               ,(append
                                 '(radio)
                                 (mapcar
                                  (function (lambda (arg) `(const ,arg)))
                                  (mapcar 'car mh-identity-list))))
                         (cons :tag "Fcc Field"
                               (const "fcc")
                               (string :tag "Value"))
                         (cons :tag "Mail-Followup-To Field"
                               (const "Mail-Followup-To")
                               (string :tag "Value"))
                         (cons :tag "Other Field"
                                 (string :tag "Field")
                                 (string :tag "Value"))))))
  :group 'mh-identity)

(defcustom mh-auto-fields-prompt-flag t
  "*Non-nil means to prompt before sending if fields inserted.
See `mh-auto-fields-list'."
  :type 'boolean
  :group 'mh-identity)

(defcustom mh-identity-default nil
  "Default identity to use when `mh-letter-mode' is called.
See `mh-identity-list'."
  :type (append
         '(radio)
         (cons '(const :tag "None" nil)
               (mapcar (function (lambda (arg) `(const ,arg)))
                       (mapcar 'car mh-identity-list))))
  :group 'mh-identity)

(defcustom mh-identity-handlers
  '(("From" . mh-identity-handler-top)
    (":default" . mh-identity-handler-bottom)
    (":attribution-verb" . mh-identity-handler-attribution-verb)
    (":signature" . mh-identity-handler-signature)
    (":pgg-default-user-id" . mh-identity-handler-gpg-identity))
  "Handler functions for fields in `mh-identity-list'.

This option is used to change the way that fields, signatures, and
attributions in `mh-identity-list' are added. To customize
`mh-identity-handlers', replace the name of an existing handler function
associated with the field you want to change with the name of a function you
have written. You can also click on an `INS' button and insert a field of your
choice and the name of the function you have written to handle it.

The `Field' field can be any field that you've used in your
`mh-identity-list'. The special fields `:attribution-verb', `:signature', or
`:pgg-default-user-id' are used for the `mh-identity-list' choices
`Attribution Verb', `Signature', and `GPG Key ID' respectively.

The handler associated with the `:default' field is used when no other field
matches.

The handler functions are passed two or three arguments: the FIELD itself (for
example, `From'), or one of the special fields (for example, `:signature'),
and the ACTION `'remove' or `'add'. If the action is `'add', an additional
argument containing the VALUE for the field is given."
  :type '(repeat (cons (string :tag "Field") function))
  :group 'mh-identity)



;;; Incorporating Your Mail (:group 'mh-inc)

(defcustom mh-inc-prog "inc"
  "*Program to incorporate new mail into a folder.

This program generates a one-line summary for each of the new messages. Unless
it is an absolute pathname, the file is assumed to be in the `mh-progs'
directory. You may also link a file to `inc' that uses a different format.
You'll then need to modify several scan line format variables appropriately."
  :type 'string
  :group 'mh-inc)

(defcustom mh-inc-spool-list nil
  "*Alternate spool files.

You can use the `mh-inc-spool-list' variable to direct MH-E to retrieve mail
from arbitrary spool files other than your system mailbox, file it in folders
other than your `+inbox', and assign key bindings to incorporate this mail.

Suppose you are subscribed to the `mh-e-devel' mailing list and you use
`procmail' to filter this mail into `~/mail/mh-e' with the following recipe in
`.procmailrc':

    MAILDIR=$HOME/mail
    :0:
    * ^From mh-e-devel-admin@stop.mail-abuse.org
    mh-e

In order to incorporate `~/mail/mh-e' into `+mh-e' with an `I m'
\(`mh-inc-spool-mh-e'\) command, customize this option, and click on the `INS'
button. Enter a `Spool File' of `~/mail/mh-e', a `Folder' of `mh-e', and a
`Key Binding' of `m'.

You can use `xbuffy' to automate the incorporation of this mail using the
`gnudoit' command in the `gnuserv' package as follows:

    box ~/mail/mh-e
        title mh-e
        origMode
        polltime 10
        headertime 0
        command gnudoit -q '(mh-inc-spool-mh-e)'"
  :type '(repeat (list (file :tag "Spool File")
                       (string :tag "Folder")
                       (character :tag "Key Binding")))
  :set 'mh-inc-spool-list-set
  :group 'mh-inc)



;;; Searching (:group 'mh-index)

(defcustom mh-index-new-messages-folders t
  "Folders searched for the \"unseen\" sequence.
Set this option to \"Inbox\" to search the \"+inbox\" folder or \"All\" to
search all of the top level folders. Otherwise, list the folders that should
be searched with the \"Choose Folders\" menu item.

See also `mh-recursive-folders-flag'."
  :group 'mh-index
  :type '(choice (const :tag "Inbox" t)
                 (const :tag "All" nil)
                 (repeat :tag "Choose Folders" (string :tag "Folder"))))

(defcustom mh-index-program nil
  "Indexing program that MH-E shall use.
The default setting of this option is `Auto-detect' which means that MH-E will
automatically choose one of swish++, swish-e, mairix, namazu, pick and grep in
that order. If, for example, you have both swish++ and mairix installed and
you want to use mairix, then you can set this option to `mairix'.

More information about setting up an indexing program to use with MH-E can be
found in the documentation of `mh-index-search'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "swish++" swish++)
                 (const :tag "swish-e" swish)
                 (const :tag "mairix" mairix)
                 (const :tag "namazu" namazu)
                 (const :tag "pick" pick)
                 (const :tag "grep" grep))
  :group 'mh-index)

(defcustom mh-index-ticked-messages-folders t
  "Folders searched for `mh-tick-seq'.
Set this option to \"Inbox\" to search the \"+inbox\" folder or \"All\" to
search all of the top level folders. Otherwise, list the folders that should
be searched with the \"Choose Folders\" menu item.

See also `mh-recursive-folders-flag'."
  :group 'mh-index
  :type '(choice (const :tag "Inbox" t)
                 (const :tag "All" nil)
                 (repeat :tag "Choose Folders" (string :tag "Folder"))))



;;; Dealing with Junk Mail (:group 'mh-junk)

;; Spam fighting program chosen
(defvar mh-junk-choice nil)

;; Available spam filter interfaces
(defvar mh-junk-function-alist
  '((spamassassin mh-spamassassin-blacklist mh-spamassassin-whitelist)
    (bogofilter mh-bogofilter-blacklist mh-bogofilter-whitelist)
    (spamprobe mh-spamprobe-blacklist mh-spamprobe-whitelist))
  "Available choices of spam programs to use.
This is an alist. For each element there are functions that blacklist a message
as spam and whitelist a message incorrectly classified as spam.")

(defun mh-junk-choose (symbol value)
  "Choose spam program to use.
The function is always called with SYMBOL bound to `mh-junk-program' and VALUE
bound to the new value of `mh-junk-program'. The function sets the variable
`mh-junk-choice' in addition to `mh-junk-program'."
  (set symbol value)
  (setq mh-junk-choice
        (or value
            (loop for element in mh-junk-function-alist
                  until (executable-find (symbol-name (car element)))
                  finally return (car element)))))

;; User customizable variables
(defcustom mh-junk-background nil
  "If on, spam programs are run in background.
By default, the programs are run in the foreground, but this can be slow when
junking large numbers of messages. If you have enough memory or don't junk
that many messages at the same time, you might try turning on this option."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" 0))
  :group 'mh-junk)

(defcustom mh-junk-disposition nil
  "Disposition of junk mail."
  :type '(choice (const :tag "Delete Spam" nil)
                 (string :tag "Spam Folder"))
  :group 'mh-junk)

(defcustom mh-junk-program nil
  "Spam program that MH-E should use.

The default setting of this option is \"Auto-detect\" which means that MH-E
will automatically choose one of SpamAssassin, Bogofilter, or SpamProbe in
that order. If, for example, you have both SpamAssassin and Bogofilter
installed and you want to use BogoFilter, then you can set this option to
\"Bogofilter\"."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "SpamAssassin" spamassassin)
                 (const :tag "Bogofilter" bogofilter)
                 (const :tag "SpamProbe" spamprobe))
  :set 'mh-junk-choose
  :group 'mh-junk)



;;; Editing a Draft (:group 'mh-letter)

(defcustom mh-compose-insertion (if (locate-library "mml") 'gnus 'mhn)
  "Type of MIME message directives in messages.

By default, this option is set to `Gnus' if it is supported. This option can
also be set manually to `mhn' if mhn directives are preferred."
  :type '(choice (const :tag "Gnus" gnus)
                 (const :tag "mhn"  mhn))
  :group 'mh-letter)

(defcustom mh-compose-skipped-header-fields
  '("From" "Organization" "References" "In-Reply-To"
    "X-Face" "Face" "X-Image-URL" "X-Mailer")
  "List of header fields to skip over when navigating in draft."
  :type '(repeat (string :tag "Field"))
  :group 'mh-letter)

(defcustom mh-compose-space-does-completion-flag nil
  "*Non-nil means that <SPC> does completion in message header."
  :type 'boolean
  :group 'mh-letter)

(defcustom mh-delete-yanked-msg-window-flag nil
  "*Non-nil means delete any window displaying the message.
If this option is on, yanking the current message into a draft letter with
\\<mh-letter-mode-map>\\[mh-yank-cur-msg] deletes any windows displaying the
message."
  :type 'boolean
  :group 'mh-letter)

(defcustom mh-extract-from-attribution-verb "wrote:"
  "*Verb to use for attribution when a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg]."
  :type '(choice (const "wrote:")
                 (const "a écrit:")
                 (const "schrieb:")
                 (string :tag "Custom String"))
  :group 'mh-letter)

(defcustom mh-ins-buf-prefix "> "
  "*String to put before each non-blank line of a yanked or inserted message.
Used when the message is inserted into an outgoing letter
by \\<mh-letter-mode-map>\\[mh-insert-letter] or \\[mh-yank-cur-msg]."
  :type 'string
  :group 'mh-letter)

(defcustom mh-letter-complete-function 'ispell-complete-word
  "*Function to call when completing outside of address or folder fields.
By default, this is set to `ispell-complete-word'."
  :type '(choice function (const nil))
  :group 'mh-letter)

(defcustom mh-letter-fill-column 72
  "*Fill column to use in `mh-letter-mode'.
This is usually less than in other text modes because email messages get
quoted by some prefix (sometimes many times) when they are replied to,
and it's best to avoid quoted lines that span more than 80 columns."
  :type 'integer
  :group 'mh-letter)

(defcustom mh-mml-method-default (if mh-gnus-pgp-support-flag "pgpmime" "none")
  "Default method to use in security directives."
  :type '(choice (const :tag "PGP (MIME)" "pgpmime")
                 (const :tag "PGP" "pgp")
                 (const :tag "S/MIME" "smime")
                 (const :tag "None" "none"))
  :group 'mh-letter)

(defcustom mh-signature-file-name "~/.signature"
  "*Source of user's signature.

By default, the text of your signature is taken from the file `~/.signature'.
You can read from other files by changing this option. This file may contain a
vCard in which case an attachment is added with the vCard.

This option may also be a symbol, in which case that function is called. You
may not want a signature separator to be added for you; instead you may want
to insert one yourself. Variables that you may find useful to do this include
`mh-signature-separator' (when inserting a signature separator) and
`mh-signature-separator-regexp' (for finding said separator). The function
`mh-signature-separator-p', which reports t if the buffer contains a
separator, may be useful as well.

The signature is inserted into your message with the command
\\<mh-letter-mode-map>\\[mh-insert-signature] or with the `mh-identity-list'
option."
  :type 'file
  :group 'mh-letter)

(defcustom mh-signature-separator-flag t
  "*Non-nil means a signature separator should be inserted.
It is not recommended that you change this option since various mail user
agents, including MH-E, use the separator to present the signature
differently, and to suppress the signature when replying or yanking a letter
into a draft."
  :type 'boolean
  :group 'mh-letter)

(defcustom mh-x-face-file "~/.face"
  "*File containing face header field to insert in outgoing mail.

If the file starts with either of the strings `X-Face:', `Face:' or
`X-Image-URL:' then the contents are added to the message header verbatim.
Otherwise it is assumed that the file contains the value of the `X-Face:'
header field.

The `X-Face:' header field, which is a low-resolution, black and white image,
can be generated using the `compface' command, which can be obtained from
ftp://ftp.cs.indiana.edu/pub/faces/compface/compface.tar.Z. The \"Online
X-Face Convertor\" at http://www.dairiki.org/xface/ is a useful resource for
quick conversion of images into `X-Face:' header fields.

Use the `make-face' script (http://quimby.gnus.org/circus/face/make-face) to
convert a JPEG image to the higher resolution, color, `Face:' header field.

The URL of any image can be used for the `X-Image-URL:' field and no
processing of the image is required.

To prevent the setting of any of these header fields, either set
`mh-x-face-file' to nil, or simply ensure that the file defined by this option
doesn't exist."
  :type 'file
  :group 'mh-letter)

(defcustom mh-yank-from-start-of-msg 'attribution
  "*Controls which part of a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If t, include the entire message, with full headers.  This is historically
here for use with supercite, but is now deprecated in favor of the setting
`supercite' below.

If the symbol `body', then yank the message minus the header.

If the symbol `supercite', include the entire message, with full headers.
This also causes the invocation of `sc-cite-original' without the setting
of `mail-citation-hook', now deprecated practice.

If the symbol `autosupercite', do as for `supercite' automatically when
show buffer matches the message being replied-to.  When this option is used,
the -noformat switch is passed to the repl program to override a -filter or
-format switch.

If the symbol `attribution', then yank the message minus the header and add
a simple attribution line at the top.

If the symbol `autoattrib', do as for `attribution' automatically when show
buffer matches the message being replied-to.  You can make sure this is
always the case by setting `mh-reply-show-message-flag' to t (which is the
default) and optionally `mh-delete-yanked-msg-window-flag' to t as well such
that the show window is never displayed.  When the `autoattrib' option is
used, the -noformat switch is passed to the repl program to override a
-filter or -format switch.

If nil, yank only the portion of the message following the point.

If the show buffer has a region, this variable is ignored unless its value is
one of `attribution' or `autoattrib' in which case the attribution is added
to the yanked region."
  :type '(choice (const :tag "Body and Header" t)
                 (const :tag "Body" body)
                 (const :tag "Below Point" nil)
                 (const :tag "Invoke supercite" supercite)
                 (const :tag "Invoke supercite, Automatically" autosupercite)
                 (const :tag "Body With Attribution" attribution)
                 (const :tag "Body With Attribution, Automatically"
                        autoattrib))
  :group 'mh-letter)



;;; Ranges (:group 'mh-ranges)

(defcustom mh-interpret-number-as-range-flag t
  "Non-nil means interpret a number as a range.
If the variable is non-nil, and you use an integer, N, when asked for a
range to scan, then MH-E uses the range \"last:N\"."
  :type 'boolean
  :group 'mh-ranges)



;;; Scan Line Formats (:group 'mh-scan-line-formats)

(defcustom mh-adaptive-cmd-note-flag t
  "*Non-nil means that the message number width is determined dynamically.
This is done once when a folder is first opened by running scan on the last
message of the folder. The message number for the last message is extracted
and its width calculated. This width is used when calling `mh-set-cmd-note'.

If you prefer fixed-width message numbers, set this variable to nil and call
`mh-set-cmd-note' with the width specified by the scan format in
`mh-scan-format-file'. For example, the default width is 4, so you would use
\"(mh-set-cmd-note 4)\" if `mh-scan-format-file' were nil."
  :type 'boolean
  :group 'mh-scan-line-formats)

(defcustom mh-scan-format-file t
  "Specifies the format file to pass to the scan program.
If t, the format string will be taken from the either `mh-scan-format-mh'
or `mh-scan-format-nmh' depending on whether MH or nmh is in use.
If nil, the default scan output will be used.

If you customize the scan format, you may need to modify a few variables
containing regexps that MH-E uses to identify specific portions of the output.
Use `M-x apropos RET mh-scan.*regexp' to obtain a list of these variables. You
may also have to call `mh-set-cmd-note' with the width of your message
numbers. See also `mh-adaptive-cmd-note-flag'."
  :type '(choice (const :tag "Use MH-E scan Format" t)
                 (const :tag "Use Default scan Format" nil)
                 (file  :tag "Specify a scan Format File"))
  :group 'mh-scan-line-formats)

(defcustom mh-scan-prog "scan"
  "*Program to run to generate one-line-per-message listing of a folder.
Normally \"scan\" or a file name linked to scan.  This file is searched
for relative to the `mh-progs' directory unless it is an absolute pathname."
  :type 'string
  :group 'mh-scan-line-formats)
(make-variable-buffer-local 'mh-scan-prog)



;;; Sending Mail (:group 'mh-sending-mail)

(defcustom mh-compose-forward-as-mime-flag t
  "Non-nil means that messages are forwarded as a MIME part."
  :type 'boolean
  :group 'mh-sending-mail)

(defcustom mh-compose-letter-function nil
  "Invoked when setting up a letter draft.
It is passed three arguments: TO recipients, SUBJECT, and CC recipients."
  :type '(choice (const nil) function)
  :group 'mh-sending-mail)

(defcustom mh-compose-prompt-flag nil
  "*Non-nil means prompt for header fields when composing a new draft."
  :type 'boolean
  :group 'mh-sending-mail)

(defcustom mh-forward-subject-format "%s: %s"
  "*Format to generate the Subject: line contents for a forwarded message.
The two string arguments to the format are the sender of the original
message and the original subject line."
  :type 'string
  :group 'mh-sending-mail)

(defcustom mh-insert-x-mailer-flag t
  "*Non-nil means append an X-Mailer field to the header."
  :type 'boolean
  :group 'mh-sending-mail)

(defcustom mh-reply-default-reply-to nil
  "*Sets the person or persons to whom a reply will be sent.
If nil, prompt for recipient.  If non-nil, then \\<mh-folder-mode-map>`\\[mh-reply]' will use this
value and it should be one of \"from\", \"to\", \"cc\", or \"all\".
The values \"cc\" and \"all\" do the same thing."
  :type '(choice (const :tag "Prompt" nil)
                 (const "from") (const "to")
                 (const "cc") (const "all"))
  :group 'mh-sending-mail)

(defcustom mh-reply-show-message-flag t
  "*Non-nil means the show buffer is displayed using \\<mh-letter-mode-map>\\[mh-reply].

The setting of this variable determines whether the MH `show-buffer' is
displayed with the current message when using `mh-reply' without a prefix
argument.  Set it to nil if you already include the message automatically
in your draft using
 repl: -filter repl.filter
in your ~/.mh_profile file."
  :type 'boolean
  :group 'mh-sending-mail)



;;; Sequences (:group 'mh-sequences)

;;; If `mh-unpropagated-sequences' becomes a defcustom, add the following to
;;; the docstring: "Additional sequences that should not to be preserved can be
;;; specified by setting `mh-unpropagated-sequences' appropriately." XXX

(defcustom mh-refile-preserves-sequences-flag t
  "*Non-nil means that sequences are preserved when messages are refiled.
If this variable is non-nil and a message belonging to a sequence other than
cur or Previous-Sequence (see mh-profile 5) is refiled then it is put in the
same sequence in the destination folder."
  :type 'boolean
  :group 'mh-sequences)

(defcustom mh-tick-seq 'tick
  "The name of the MH sequence for ticked messages.
You would change this option if you already use the `tick' sequence for your
own use. You can also disable all of the ticking functions by choosing the
`Disable Ticking' item but there isn't much advantage to that."
  :type '(choice (const :tag "Disable Ticking" nil)
                 symbol)
  :group 'mh-sequences)

(defcustom mh-update-sequences-after-mh-show-flag t
  "*Non-nil means flush MH sequences to disk after message is shown.
Three sequences are maintained internally by MH-E and pushed out to MH when a
message is shown. They include the sequence specified by your
`Unseen-Sequence:' profile entry, `cur', and the sequence listed by
the `mh-tick-seq' option which is `tick' by default.
If you do not like this behavior, set this option to nil. You can then update
the state manually with the \\<mh-folder-mode-map>`\\[mh-execute-commands]', `\\[mh-quit]', or `\\[mh-update-sequences]' commands."
  :type 'boolean
  :group 'mh-sequences)



;;; Reading Your Mail (:group 'mh-show)

(defcustom mh-bury-show-buffer-flag t
  "*Non-nil means that the displayed show buffer for a folder is buried."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-clean-message-header-flag t
  "*Non-nil means remove extraneous header fields.
The header fields listed in the `mh-invisible-header-fields-default' option
are hidden, although you can check off any field that you would like to see.
Header fields that you would like to hide that aren't listed can be added to
the `mh-invisible-header-fields' option."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-decode-mime-flag (not (not (locate-library "mm-decode")))
  "*Non-nil means that Gnus is used to show MIME attachments with Gnus."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-display-buttons-for-alternatives-flag nil
  "*Non-nil means display buttons for all MIME alternatives.
Default behavior is to display only the preferred alternative. If this
variable is non-nil, then the preferred part is shown inline and buttons
are shown for each of the other alternatives."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-display-buttons-for-inline-parts-flag nil
  "*Non-nil means display buttons for all inline MIME parts.
If non-nil, buttons are displayed for all MIME parts. Inline parts start off
in displayed state but they can be hidden by clicking the button. If nil no
buttons are shown for inline parts."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-do-not-confirm-flag nil
  "*Non-nil means do not prompt for confirmation.
Commands such as `mh-pack-folder' prompt to confirm whether to process
outstanding moves and deletes or not before continuing. A non-nil setting will
perform the action--which is usually desired but cannot be retracted--without
question."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-fetch-x-image-url 'ask
  "*Control fetching of `X-Image-URL:' header field image.
If set to \"Always fetch\" (t), the image is always fetched. You probably want
to avoid this setting for privacy and DOS (denial of service) reasons. For
example, fetching a URL can tip off a spammer that you've read his email.
Someone may also flood your network and fill your disk drive by sending a
torrent of messages, each specifying a unique URL to a very large file.

If set to \"Ask before fetching\" ('ask), you are prompted before the image is
fetched. MH-E will remember your reply and will either use the already fetched
image the next time the same URL is encountered or silently skip it if you
didn't fetch it the first time. This is the default.

If set to \"Never fetch\" (nil), images are never fetched and only displayed
if they are already present in the cache.

The cache of images is found in the directory `.mhe-x-image-cache' within your
MH directory. To see how you can add your own face to the `From:' field, see
`mh-x-face-file'.

This setting only has effect if `mh-show-use-xface-flag' is non-nil."

  :type '(choice (const :tag "Always fetch" t)
                 (const :tag "Ask before fetching" ask)
                 (const :tag "Never fetch" nil))
  :group 'mh-show)

(defcustom mh-graphical-smileys-flag t
  "*Non-nil means graphical smileys are displayed.
Non-nil means that small graphics will be used in the show buffer instead of
patterns like :-), ;-) etc. The setting only has effect if
`mh-decode-mime-flag' is non-nil."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-graphical-emphasis-flag t
  "*Non-nil means graphical emphasis is displayed.
Non-nil means that _underline_ will be underlined, *bold* will appear in bold,
/italic/ will appear in italic etc. See `gnus-emphasis-alist' for the whole
list. The setting only has effect if `mh-decode-mime-flag' is non-nil."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-highlight-citation-p 'gnus
  "How to highlight citations in show buffers.
The gnus method uses a different color for each indentation."
  :type '(choice (const :tag "Use Gnus" gnus)
                 (const :tag "Use font-lock" font-lock)
                 (const :tag "Don't fontify" nil))
  :group 'mh-show)

;; Keep fields alphabetized. Mention source, if known.
(defvar mh-invisible-header-fields-internal
  '("Approved:"
    "Autoforwarded:"
    "Bestservhost:"
    "Cancel-Lock:"                      ; NNTP posts
    "Content-"                          ; RFC 2045
    "Delivered-To:"              ; Egroups/yahoogroups mailing list manager
    "Delivery-Date:"                    ; MH
    "Delivery:"
    "DomainKey-Signature:"              ;http://antispam.yahoo.com/domainkeys
    "Encoding:"
    "Envelope-to:"
    "Errors-To:"
    "Face:"                             ; Gnus Face header
    "Forwarded:"                        ; MH
    "From "                             ; sendmail
    "Importance:"                       ; MS Outlook
    "In-Reply-To:"                      ; MH
    "Lines:"
    "List-"                             ; Mailman mailing list manager
    "List-"                             ; Unknown mailing list managers
    "List-Subscribe:"                   ; Unknown mailing list managers
    "List-Unsubscribe:"                 ; Unknown mailing list managers
    "Mail-from:"                        ; MH
    "Mailing-List:"              ; Egroups/yahoogroups mailing list manager
    "Message-Id:"                       ; RFC 822
    "Mime-Version"                      ; RFC 2045
    "NNTP-"                             ; News
    "Old-Return-Path:"
    "Original-Encoded-Information-Types:"  ; X400
    "Original-Lines:"                   ; mail to news
    "Original-NNTP-"                    ; mail to news
    "Original-Newsgroups:"              ; mail to news
    "Original-Path:"                    ; mail to news
    "Original-Received:"                ; mail to news
    "Original-To:"                      ; mail to news
    "Original-X-"                       ; mail to news
    "Originator:"
    "P1-Content-Type:"                  ; X400
    "P1-Message-Id:"                    ; X400
    "P1-Recipient:"                     ; X400
    "Path:"
    "Precedence:"
    "Prev-Resent"                       ; MH
    "Priority:"
    "Received:"                         ; RFC 822
    "Received-SPF:"                     ; Gmail
    "References:"
    "Remailed-"                         ; MH
    "Replied:"                          ; MH
    "Resent"                            ; MH
    "Return-Path:"                      ; RFC 822
    "Sensitivity:"                      ; MS Outlook
    "Status:"                           ; sendmail
    "Thread-"
    "Ua-Content-Id:"                    ; X400
;;  "User-Agent:"                       ; Similar to X-Mailer, so display it.
    "Via:"                              ; MH
    "X-Abuse-Info:"
    "X-Abuse-and-DMCA-"
    "X-Accept-Language:"
    "X-Accept-Language:"                ; Netscape/Mozilla
    "X-Ack:"
    "X-Administrivia-To:"
    "X-AntiAbuse:"                      ; cPanel
    "X-Apparently-From:"                ; MS Outlook
    "X-Apparently-To:"           ; Egroups/yahoogroups mailing list manager
    "X-Authentication-Warning:"         ; sendmail
    "X-Beenthere:"                      ; Mailman mailing list manager
    "X-Bogosity:"                       ; bogofilter
    "X-Complaints-To:"
    "X-Cron-Env:"
    "X-DMCA"
    "X-Delivered"
    "X-ELNK-Trace:"                     ; Earthlink mailer
    "X-Envelope-Date:"                  ; GNU mailutils
    "X-Envelope-From:"
    "X-Envelope-Sender:"
    "X-Envelope-To:"
    "X-Evolution:"                      ; Evolution mail client
    "X-Face:"
    "X-Folder:"                         ; Spam
    "X-From-Line"
    "X-Gmail-"                          ; Gmail
    "X-Gnus-Mail-Source:"               ; gnus
    "X-Greylist:"                       ; milter-greylist-1.2.1
    "X-Habeas-SWE-1:"                   ; Spam
    "X-Habeas-SWE-2:"                   ; Spam
    "X-Habeas-SWE-3:"                   ; Spam
    "X-Habeas-SWE-4:"                   ; Spam
    "X-Habeas-SWE-5:"                   ; Spam
    "X-Habeas-SWE-6:"                   ; Spam
    "X-Habeas-SWE-7:"                   ; Spam
    "X-Habeas-SWE-8:"                   ; Spam
    "X-Habeas-SWE-9:"                   ; Spam
    "X-Image-URL:"                      ; URL equivalent of X-Face and Face
    "X-Info:"                           ; NTMail
    "X-Juno-"                           ; Juno
    "X-List-Host:"                      ; Unknown mailing list managers
    "X-List-Subscribe:"                 ; Unknown mailing list managers
    "X-List-Unsubscribe:"               ; Unknown mailing list managers
    "X-Listprocessor-"                  ; ListProc(tm) by CREN
    "X-Listserver:"                     ; Unknown mailing list managers
    "X-Loop:"                           ; Unknown mailing list managers
    "X-MHE-Checksum"                    ; Checksum added during index search
    "X-MIME-Autoconverted:"             ; sendmail
    "X-MIMETrack:"
    "X-Mms-"                            ; T-Mobile pictures
    "X-MS-"                             ; MS Outlook
    "X-MailScanner"                     ; ListProc(tm) by CREN
    "X-Mailing-List:"                   ; Unknown mailing list managers
    "X-Mailman-Version:"                ; Mailman mailing list manager
    "X-Majordomo:"                      ; Majordomo mailing list manager
    "X-Message-Id"
    "X-MessageWall-Score:"              ; Unknown mailing list manager, AUC TeX
    "X-MimeOLE:"                        ; MS Outlook
    "X-Mozilla-Status:"                 ; Netscape/Mozilla
    "X-Msmail-"                         ; MS Outlook
    "X-NAI-Spam-"                       ; Network Associates Inc. SpamKiller
    "X-News:"                           ; News
    "X-No-Archive:"
    "X-Notes-Item:"                     ; Lotus Notes Domino structured header
    "X-OperatingSystem:"
    ;;"X-Operator:"                     ; Similar to X-Mailer, so display it
    "X-Orcl-Content-Type:"
    "X-Original-Complaints-To:"
    "X-Original-Date:"                  ; SourceForge mailing list manager
    "X-Original-To:"
    "X-Original-Trace:"
    "X-OriginalArrivalTime:"            ; Hotmail
    "X-Originating-IP:"                 ; Hotmail
    "X-Postfilter:"
    "X-Priority:"                       ; MS Outlook
    "X-Qotd-"                           ; User added
    "X-RM"
    "X-Received-Date:"
    "X-Received:"
    "X-Request-"
    "X-SBClass:"                        ; Spam
    "X-SBNote:"                         ; Spam
    "X-SBPass:"                         ; Spam
    "X-SBRule:"                         ; Spam
    "X-SMTP-"
    "X-Scanned-By"
    "X-Sender:"
    "X-Server-Date:"
    "X-Server-Uuid:"
    "X-Sieve:"                          ; Sieve filtering
    "X-Source"
    "X-Spam-"                           ; Spamassassin
    "X-SpamBouncer:"                    ; Spam
    "X-Status"
    "X-Submissions-To:"
    "X-Telecom-Digest"
    "X-Trace:"
    "X-UID"
    "X-UIDL:"
    "X-USANET-"                         ; usa.net
    "X-UserInfo1:"
    "X-VSMLoop:"                        ; NTMail
    "X-Vms-To:"
    "X-WebTV-Signature:"
    "X-Wss-Id:"                         ; Worldtalk gateways
    "X-Yahoo"
    "X-eGroups-"                 ; Egroups/yahoogroups mailing list manager
    "X-pgp:"
    "X-submission-address:"
    "X400-"                             ; X400
    "Xref:")
  "List of default header fields that are not to be shown.
Do not alter this variable directly.  Instead, add entries from here that you
would like to be displayed in `mh-invisible-header-fields-default'
and add entries to hide in `mh-invisible-header-fields'.")

(defvar mh-invisible-header-fields-compiled nil
  "*Regexp matching lines in a message header that are not to be shown.
Do not alter this variable directly. Instead, customize
`mh-invisible-header-fields-default' checking for fields normally
hidden that you wish to display, and add extra entries to hide in
`mh-invisible-header-fields'.")

(defun mh-invisible-headers ()
  "Make or remake the variable `mh-invisible-header-fields-compiled'.
Done using `mh-invisible-header-fields-internal' as input, from which entries
from `mh-invisible-header-fields-default' are removed and entries
from `mh-invisible-header-fields' are added."
  (let ((fields mh-invisible-header-fields-internal))
    (when mh-invisible-header-fields-default
      ;; Remove entries from `mh-invisible-header-fields-default'
      (setq fields
            (loop for x in fields
                  unless (member x mh-invisible-header-fields-default)
                  collect x)))
    (when (and (boundp 'mh-invisible-header-fields)
               mh-invisible-header-fields)
      (dolist (x mh-invisible-header-fields)
        (unless (member x fields) (setq fields (cons x fields)))))
    (if fields
        (setq mh-invisible-header-fields-compiled
              (concat
               "^"
               ;; workaround for insufficient default
               (let ((max-specpdl-size 1000))
                 (regexp-opt fields t))))
      (setq mh-invisible-header-fields-compiled nil))))

(defcustom mh-invisible-header-fields-default nil
  "*List of hidden header fields.
The header fields listed in this option are hidden, although you can check off
any field that you would like to see. Header fields that you would like to
hide that aren't listed can be added to the `mh-invisible-header-fields'
option.

See also `mh-clean-message-header-flag'."
  :type `(set ,@(mapcar (lambda (x) `(const ,x))
                        mh-invisible-header-fields-internal))
  :set (lambda (symbol value)
         (set-default symbol value)
         (mh-invisible-headers))
  :group 'mh-show)

(defcustom mh-invisible-header-fields nil
  "*Additional header fields to hide.
Header fields that you would like to hide that aren't listed in
`mh-invisible-header-fields-default' can be added to this option with a couple
of caveats. Regular expressions are not allowed. Unique fields should have a
`:' suffix; otherwise, the element can be used to render invisible an entire
class of fields that start with the same prefix.

See also `mh-clean-message-header-flag'."

  :type '(repeat (string :tag "Header field"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (mh-invisible-headers))
  :group 'mh-show)

(defcustom mh-lpr-command-format "lpr -J '%s'"
  "*Format for Unix command that prints a message.
The string should be a Unix command line, with the string '%s' where
the job's name (folder and message number) should appear.  The formatted
message text is piped to this command when you type \\<mh-folder-mode-map>`\\[mh-print-msg]'."
  :type 'string
  :group 'mh-show)

(defcustom mh-max-inline-image-height nil
  "*Maximum inline image height if Content-Disposition is not present.
If nil, image will be displayed if its height is smaller than the height of
the window."
  :type '(choice (const nil) integer)
  :group 'mh-show)

(defcustom mh-max-inline-image-width nil
  "*Maximum inline image width if Content-Disposition is not present.
If nil, image will be displayed if its width is smaller than the width of the
window."
  :type '(choice (const nil) integer)
  :group 'mh-show)

(defcustom mh-mime-save-parts-default-directory t
  "Default directory to use for `mh-mime-save-parts'.
If nil, prompt and set for next time the command is used during same session.
If t, prompt always"
  :type '(choice (const :tag "Prompt the first time" nil)
                 (const :tag "Prompt always" t)
                 directory)
  :group 'mh-show)

(defcustom mh-print-background-flag nil
  "*Non-nil means messages should be printed in the background.
WARNING: do not delete the messages until printing is finished;
otherwise, your output may be truncated."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-recursive-folders-flag nil
  "*Non-nil means that commands which operate on folders do so recursively."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-show-maximum-size 0
  "*Maximum size of message (in bytes) to display automatically.
Provides an opportunity to skip over large messages which may be slow to load.
Use a value of 0 to display all messages automatically regardless of size."
  :type 'integer
  :group 'mh-show)

(defcustom mh-show-threads-flag nil
  "Non-nil means new folders start in threaded mode.
Threading large number of messages can be time consuming. So if the flag is
non-nil then threading will be done only if the number of messages being
threaded is less than `mh-large-folder'."
  :type 'boolean
  :group 'mh-show)

;; Use goto-addr if it was already loaded (which probably sets this
;; variable to t), or if this variable is otherwise set to t.
(defcustom mh-show-use-goto-addr-flag (and (boundp 'goto-address-highlight-p)
                                           goto-address-highlight-p)
  "*Non-nil means highlight URLs and email addresses.
The `goto-addr' module is used."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-show-use-xface-flag (>= emacs-major-version 21)
  "*Non-nil means display face images in `mh-show-mode'.

MH-E can display the content of `Face:', `X-Face:', and `X-Image-URL:' header
fields. If any of these fields occur in the header of your message, the
sender's face will appear in the `From:' header field. If more than one of
these fields appear, then the first field found in the order `Face:',
`X-Face:', and `X-Image-URL:' will be used. Note that versions of GNU Emacs
prior to 21.1 don't support the display of inline images, so face images are
not displayed in these versions.

The option `mh-show-use-xface-flag' is used to turn this feature on and off.
This feature will be turned on by default if your system supports it.

The first header field used, if present, is the Gnus-specific `Face:' field.
The `Face:' field appeared in GNU Emacs 21 and XEmacs. For more information,
see http://quimby.gnus.org/circus/face/. Next is the traditional `X-Face:'
header field. The display of this field requires the `uncompface' program
which can be obtained from
ftp://ftp.cs.indiana.edu/pub/faces/compface/compface.tar.Z. Recent versions of
XEmacs have internal support for `X-Face:' images. If your version of XEmacs
does not, then you'll need both `uncompface' and the x-face package which is
available at ftp://ftp.jpl.org/pub/elisp/.

Finally, MH-E will display images referenced by the `X-Image-URL:' header
field if neither the `Face:' nor the `X-Face:' fields are present. The display
of the images requires `wget' (available from
http://www.gnu.org/software/wget/wget.html), `fetch', or `curl' to fetch the
image and the `convert' program from the ImageMagick suite, available from
http://www.imagemagick.org/. Of the three header fields this is the most
efficient in terms of network usage since the image doesn't need to be
transmitted with every single mail.

The option `mh-fetch-x-image-url' controls the fetching of the `X-Image-URL:'
header field image."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-store-default-directory nil
  "*Last directory used by \\[mh-store-msg]; default for next store.
A directory name string, or nil to use current directory."
  :type '(choice (const :tag "Current" nil)
                 directory)
  :group 'mh-show)

(defcustom mh-summary-height nil
  "*Number of lines in MH-Folder window (including the mode line)."
  :type '(choice (const :tag "Automatic" nil)
                 (integer :tag "Fixed sized"))
  :group 'mh-show)

(defcustom mhl-formfile nil
  "*Name of format file to be used by mhl to show and print messages.
A value of t means use the default format file.
nil means don't use mhl to format messages when showing; mhl is still used,
with the default format file, to format messages when printing them.
The format used should specify a non-zero value for overflowoffset so
the message continues to conform to RFC 822 and MH-E can parse the headers."
  :type '(choice (const nil) (const t) string)
  :group 'mh-show)
(put 'mhl-formfile 'info-file "mh-e")



;;; The Speedbar (:group 'mh-speed)

(defcustom mh-large-folder 200
  "The number of messages that indicates a large folder.
If a folder is deemed to be large, that is the number of messages in it exceed
this value, then confirmation is needed when it is visited. Even when
`mh-show-threads-flag' is non-nil, the folder is not automatically threaded, if
it is large. If set to nil all folders are treated as if they are small."
  :type '(choice (const :tag "No limit") integer)
  :group 'mh-speed)

(defcustom mh-speed-flists-interval 60
  "Time between calls to flists in seconds.
If 0, flists is not called repeatedly."
  :type 'integer
  :group 'mh-speed)

(defcustom mh-speed-run-flists-flag t
  "Non-nil means flists is used.
If non-nil, flists is executed every `mh-speed-flists-interval' seconds to
update the display of the number of unseen and total messages in each folder.
If resources are limited, this can be set to nil and the speedbar display can
be updated manually with the \\[mh-speed-flists] command."
  :type 'boolean
  :group 'mh-speed)



;;; The Toolbar (:group 'mh-toolbar)

(defcustom mh-tool-bar-search-function 'mh-search-folder
  "*Function called by the tool-bar search button.
See `mh-search-folder' and `mh-index-search' for details."
  :type '(choice (const mh-search-folder)
                 (const mh-index-search)
                 (function :tag "Other function"))
  :group 'mh-toolbar)

;; Functions called from the tool bar
(defun mh-tool-bar-search (&optional arg)
  "Interactively call `mh-tool-bar-search-function'.
Optional argument ARG is not used."
  (interactive "P")
  (call-interactively mh-tool-bar-search-function))

(defun mh-tool-bar-customize ()
  "Call `mh-customize' from the toolbar."
  (interactive)
  (mh-customize t))

(defun mh-tool-bar-folder-help ()
  "Visit \"(mh-e)Top\"."
  (interactive)
  (info "(mh-e)Top")
  (delete-other-windows))

(defun mh-tool-bar-letter-help ()
  "Visit \"(mh-e)Draft Editing\"."
  (interactive)
  (info "(mh-e)Draft Editing")
  (delete-other-windows))

(defmacro mh-tool-bar-reply-generator (function recipient folder-buffer-flag)
  "Generate FUNCTION that replies to RECIPIENT.
If FOLDER-BUFFER-FLAG is nil then the function generated
When INCLUDE-FLAG is non-nil, include message body being replied to."
  `(defun ,function (&optional arg)
     ,(format "Reply to \"%s\".\nWhen ARG is non-nil include message in reply."
              recipient)
     (interactive "P")
     ,(if folder-buffer-flag nil '(set-buffer mh-show-folder-buffer))
     (mh-reply (mh-get-msg-num nil) ,recipient arg)))

(mh-tool-bar-reply-generator mh-tool-bar-reply-from "from" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-from "from" nil)
(mh-tool-bar-reply-generator mh-tool-bar-reply-to "to" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-to "to" nil)
(mh-tool-bar-reply-generator mh-tool-bar-reply-all "all" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-all "all" nil)

;; XEmacs has a couple of extra customizations...
(mh-do-in-xemacs
  (defcustom mh-xemacs-use-toolbar-flag (if (and (featurep 'toolbar)
                                                 (featurep 'xpm)
                                                 (device-on-window-system-p))
                                            t
                                          nil)
    "*If non-nil, use toolbar.

This will default to t if you are in an environment that supports
toolbars and xpm."
    :type 'boolean
    :group 'mh-toolbar)

  (defcustom mh-xemacs-toolbar-position (if mh-xemacs-use-toolbar-flag
                                            'default
                                          nil)
    "*Where to put the toolbar.

Valid non-nil values are \"default\", \"top\", \"bottom\", \"left\",
\"right\".  These match the four edges of the frame, with \"default\"
meaning \"use the same position as the default-toolbar\".

A nil value means do not use a toolbar.

If this variable is set to anything other than \"default\" and the
default-toolbar has a different positional setting from the value of
this variable, then two toolbars will be displayed.  The MH-E toolbar
and the default-toolbar."
    :type '(radio (const :tag "Same position as the \"default-toolbar\""
                         :value default)
                  (const :tag "Along the top edge of the frame"
                         :value top)
                  (const :tag "Along the bottom edge of the frame"
                         :value bottom)
                  (const :tag "Along the left edge of the frame"
                         :value left)
                  (const :tag "Along the right edge of the frame"
                         :value right)
                  (const :tag "Don't use a toolbar" nil))
    :group 'mh-toolbar))

(defmacro mh-tool-bar-define (defaults &rest buttons)
  "Define a tool bar for MH-E.
DEFAULTS is the list of buttons that are present by default. It is a list of
lists where the sublists are of the following form:

  (:KEYWORD FUNC1 FUNC2 FUNC3 ...)

Here :KEYWORD is one of :folder or :letter. If it is :folder then the default
buttons in the folder and show mode buffers are being specified. If it is
:letter then the default buttons in the letter mode are listed. FUNC1, FUNC2,
FUNC3, ... are the names of the functions that the buttons would execute.

Each element of BUTTONS is a list consisting of four mandatory items and one
optional item as follows:

  (FUNCTION MODES ICON DOC &optional ENABLE-EXPR)

where,

  FUNCTION is the name of the function that will be executed when the button
  is clicked.

  MODES is a list of symbols. List elements must be from `folder', `letter' and
  `sequence'. If `folder' is present then the button is available in the
  folder and show buffer. If the name of FUNCTION is of the form \"mh-foo\",
  where foo is some arbitrary string, then we check if the function
  `mh-show-foo' exists. If it exists then that function is used in the show
  buffer. Otherwise the original function `mh-foo' is used in the show buffer
  as well. Presence of `sequence' is handled similar to the above. The only
  difference is that the button is shown only when the folder is narrowed to a
  sequence. If `letter' is present in MODES, then the button is available
  during draft editing and runs FUNCTION when clicked.

  ICON is the icon that is drawn in the button.

  DOC is the documentation for the button. It is used in tool-tips and in
  providing other help to the user. GNU Emacs uses only the first line of the
  string. So the DOC should be formatted such that the first line is useful and
  complete without the rest of the string.

  Optional item ENABLE-EXPR is an arbitrary lisp expression. If it evaluates
  to nil, then the button is deactivated, otherwise it is active. If is in't
  present then the button is always active."
  ;; The following variable names have been carefully chosen to make code
  ;; generation easier. Modifying the names should be done carefully.
  (let (folder-buttons folder-docs folder-button-setter sequence-button-setter
                       show-buttons show-button-setter show-seq-button-setter
                       letter-buttons letter-docs letter-button-setter
                       folder-defaults letter-defaults
                       folder-vectors show-vectors letter-vectors)
    (dolist (x defaults)
      (cond ((eq (car x) :folder) (setq folder-defaults (cdr x)))
            ((eq (car x) :letter) (setq letter-defaults (cdr x)))))
    (dolist (button buttons)
      (unless (and (listp button)
                   (or (equal (length button) 4) (equal (length button) 5)))
        (error "Incorrect MH-E tool-bar button specification: %s" button))
      (let* ((name (nth 0 button))
             (name-str (symbol-name name))
             (icon (nth 2 button))
             (xemacs-icon (mh-do-in-xemacs
                            (cdr (assoc (intern icon) mh-xemacs-icon-map))))
             (full-doc (nth 3 button))
             (doc (if (string-match "\\(.*\\)\n" full-doc)
                      (match-string 1 full-doc)
                    full-doc))
             (enable-expr (or (nth 4 button) t))
             (modes (nth 1 button))
             functions show-sym)
        (when (memq 'letter modes) (setq functions `(:letter ,name)))
        (when (or (memq 'folder modes) (memq 'sequence modes))
          (setq functions
                (append `(,(if (memq 'folder modes) :folder :sequence) ,name)
                        functions))
          (setq show-sym
                (if (string-match "^mh-\\(.*\\)$" name-str)
                    (intern (concat "mh-show-" (match-string 1 name-str)))
                  name))
          (setq functions
                (append `(,(if (memq 'folder modes) :show :show-seq)
                          ,(if (fboundp show-sym) show-sym name))
                        functions)))
        (do ((functions functions (cddr functions)))
            ((null functions))
          (let* ((type (car functions))
                 (function (cadr functions))
                 (type1 (substring (symbol-name type) 1))
                 (vector-list (cond ((eq type :show) 'show-vectors)
                                    ((eq type :show-seq) 'show-vectors)
                                    ((eq type :letter) 'letter-vectors)
                                    (t 'folder-vectors)))
                 (list (cond ((eq type :letter) 'mh-tool-bar-letter-buttons)
                             (t 'mh-tool-bar-folder-buttons)))
                 (key (intern (concat "mh-" type1 "toolbar-" name-str)))
                 (setter (intern (concat type1 "-button-setter")))
                 (mbuttons (cond ((eq type :letter) 'letter-buttons)
                                 ((eq type :show) 'show-buttons)
                                 ((eq type :show-seq) 'show-buttons)
                                 (t 'folder-buttons)))
                 (docs (cond ((eq mbuttons 'letter-buttons) 'letter-docs)
                             ((eq mbuttons 'folder-buttons) 'folder-docs))))
            (add-to-list vector-list `[,xemacs-icon ,function t ,full-doc])
            (add-to-list
             setter `(when (member ',name ,list)
                       (mh-funcall-if-exists
                        tool-bar-add-item ,icon ',function ',key
                        :help ,doc :enable ',enable-expr)))
            (add-to-list mbuttons name)
            (if docs (add-to-list docs doc))))))
    (setq folder-buttons (nreverse folder-buttons)
          letter-buttons (nreverse letter-buttons)
          show-buttons (nreverse show-buttons)
          letter-docs (nreverse letter-docs)
          folder-docs (nreverse folder-docs)
          folder-vectors (nreverse folder-vectors)
          show-vectors (nreverse show-vectors)
          letter-vectors (nreverse letter-vectors))
    (dolist (x folder-defaults)
      (unless (memq x folder-buttons)
        (error "Folder defaults contains unknown button '%s'" x)))
    (dolist (x letter-defaults)
      (unless (memq x letter-buttons)
        (error "Letter defaults contains unknown button '%s'" x)))
    `(eval-when (compile load eval)
       (defvar mh-folder-tool-bar-map nil)
       (defvar mh-folder-seq-tool-bar-map nil)
       (defvar mh-show-tool-bar-map nil)
       (defvar mh-show-seq-tool-bar-map nil)
       (defvar mh-letter-tool-bar-map nil)
       ;; GNU Emacs tool bar specific code
       (mh-do-in-gnu-emacs
         ;; Custom setter functions
         (defun mh-tool-bar-folder-buttons-set (symbol value)
           "Construct toolbar for `mh-folder-mode' and `mh-show-mode'."
           (set-default symbol value)
           (setq mh-folder-tool-bar-map
                 (let ((tool-bar-map (make-sparse-keymap)))
                   ,@(nreverse folder-button-setter)
                   tool-bar-map))
           (setq mh-show-tool-bar-map
                 (let ((tool-bar-map (make-sparse-keymap)))
                   ,@(nreverse show-button-setter)
                   tool-bar-map))
           (setq mh-show-seq-tool-bar-map
                 (let ((tool-bar-map (copy-keymap mh-show-tool-bar-map)))
                   ,@(nreverse show-seq-button-setter)
                   tool-bar-map))
           (setq mh-folder-seq-tool-bar-map
                 (let ((tool-bar-map (copy-keymap mh-folder-tool-bar-map)))
                   ,@(nreverse sequence-button-setter)
                   tool-bar-map)))
         (defun mh-tool-bar-letter-buttons-set (symbol value)
           "Construct toolbar for `mh-letter-mode'."
           (set-default symbol value)
           (setq mh-letter-tool-bar-map
                 (let ((tool-bar-map (make-sparse-keymap)))
                   ,@(nreverse letter-button-setter)
                   tool-bar-map))))
       ;; XEmacs specific code
       (mh-do-in-xemacs
         (defvar mh-toolbar-folder-vector-map
           ',(loop for button in folder-buttons
                   for vector in folder-vectors
                   collect (cons button vector)))
         (defvar mh-toolbar-show-vector-map
           ',(loop for button in show-buttons
                   for vector in show-vectors
                   collect (cons button vector)))
         (defvar mh-toolbar-letter-vector-map
           ',(loop for button in letter-buttons
                   for vector in letter-vectors
                   collect (cons button vector)))
         (defvar mh-toolbar-folder-buttons nil)
         (defvar mh-toolbar-show-buttons nil)
         (defvar mh-toolbar-letter-buttons nil)
         ;; Custom setter functions
         (defun mh-tool-bar-letter-buttons-set (symbol value)
           (set-default symbol value)
           (when mh-xemacs-has-toolbar-flag
             (setq mh-toolbar-letter-buttons
                   (loop for b in value
                         collect (cdr (assoc b mh-toolbar-letter-vector-map))))))
         (defun mh-tool-bar-folder-buttons-set (symbol value)
           (set-default symbol value)
           (when mh-xemacs-has-toolbar-flag
             (setq mh-toolbar-folder-buttons
                   (loop for b in value
                         collect (cdr (assoc b mh-toolbar-folder-vector-map))))
             (setq mh-toolbar-show-buttons
                   (loop for b in value
                         collect (cdr (assoc b mh-toolbar-show-vector-map))))))
         ;; Initialize toolbar
         (defun mh-toolbar-init (mode)
           "Install toolbar in MODE."
           (let ((toolbar (cond ((eq mode :folder) mh-toolbar-folder-buttons)
                                ((eq mode :letter) mh-toolbar-letter-buttons)
                                ((eq mode :show) mh-toolbar-show-buttons)))
                 (height 37)
                 (width 40)
                 (buffer (current-buffer)))
             (when (and mh-xemacs-toolbar-position mh-xemacs-use-toolbar-flag
                        mh-xemacs-has-toolbar-flag)
               (cond
                ((eq mh-xemacs-toolbar-position 'top)
                 (set-specifier top-toolbar toolbar buffer)
                 (set-specifier top-toolbar-visible-p t)
                 (set-specifier top-toolbar-height height))
                ((eq mh-xemacs-toolbar-position 'bottom)
                 (set-specifier bottom-toolbar toolbar buffer)
                 (set-specifier bottom-toolbar-visible-p t)
                 (set-specifier bottom-toolbar-height height))
                ((eq mh-xemacs-toolbar-position 'left)
                 (set-specifier left-toolbar toolbar buffer)
                 (set-specifier left-toolbar-visible-p t)
                 (set-specifier left-toolbar-width width))
                ((eq mh-xemacs-toolbar-position 'right)
                 (set-specifier right-toolbar toolbar buffer)
                 (set-specifier right-toolbar-visible-p t)
                 (set-specifier right-toolbar-width width))
                (t (set-specifier default-toolbar toolbar buffer)))))))
       ;; Declare customizable toolbars
       (custom-declare-variable
        'mh-tool-bar-folder-buttons
        '(list ,@(mapcar (lambda (x) `(quote ,x)) folder-defaults))
        "Choose buttons to include in MH-E folder/show toolbar."
        :group 'mh-toolbar :set 'mh-tool-bar-folder-buttons-set
        :type '(set ,@(loop for x in folder-buttons
                            for y in folder-docs
                            collect `(const :tag ,y ,x))))
       (custom-declare-variable
        'mh-tool-bar-letter-buttons
        '(list ,@(mapcar (lambda (x) `(quote ,x)) letter-defaults))
        "Choose buttons to include in MH-E letter toolbar."
        :group 'mh-toolbar :set 'mh-tool-bar-letter-buttons-set
        :type '(set ,@(loop for x in letter-buttons
                            for y in letter-docs
                            collect `(const :tag ,y ,x)))))))

(mh-image-load-path)
(mh-tool-bar-define
    ((:folder mh-inc-folder mh-mime-save-parts mh-previous-undeleted-msg
              mh-page-msg  mh-next-undeleted-msg mh-delete-msg mh-refile-msg
              mh-undo mh-execute-commands mh-toggle-tick mh-reply
              mh-alias-grab-from-field mh-send mh-rescan-folder
              mh-tool-bar-search mh-visit-folder
              mh-tool-bar-customize mh-tool-bar-folder-help mh-widen)
     (:letter mh-send-letter mh-compose-insertion ispell-message save-buffer
              undo kill-region menu-bar-kill-ring-save yank mh-fully-kill-draft
              mh-tool-bar-customize mh-tool-bar-letter-help))
  ;; Folder/Show buffer buttons
  (mh-inc-folder (folder) "mail"
    "Incorporate new mail in Inbox
This button runs `mh-inc-folder' which drags any
new mail into your Inbox folder.")
  (mh-mime-save-parts (folder) "attach"
    "Save MIME parts from this message
This button runs `mh-mime-save-parts' which saves a message's
different parts into separate files.")
  (mh-previous-undeleted-msg (folder) "left_arrow"
    "Go to the previous undeleted message
This button runs `mh-previous-undeleted-msg'")
  (mh-page-msg (folder) "page-down"
    "Page the current message forwards\nThis button runs `mh-page-msg'")
  (mh-next-undeleted-msg (folder) "right_arrow"
    "Go to the next undeleted message\nThe button runs `mh-next-undeleted-msg'")
  (mh-delete-msg (folder) "close"
    "Mark this message for deletion\nThis button runs `mh-delete-msg'")
  (mh-refile-msg (folder) "mail/refile"
    "Refile this message\nThis button runs `mh-refile-msg'")
  (mh-undo (folder) "undo" "Undo last operation\nThis button runs `undo'"
    (mh-outstanding-commands-p))
  (mh-execute-commands (folder) "execute"
    "Perform moves and deletes\nThis button runs `mh-execute-commands'"
    (mh-outstanding-commands-p))
  (mh-toggle-tick (folder) "highlight"
    "Toggle tick mark\nThis button runs `mh-toggle-tick'")
  (mh-toggle-showing (folder) "show"
    "Toggle showing message\nThis button runs `mh-toggle-showing'")
  (mh-tool-bar-reply-from (folder) "mail/reply-from" "Reply to \"from\"")
  (mh-tool-bar-reply-to (folder) "mail/reply-to" "Reply to \"to\"")
  (mh-tool-bar-reply-all (folder) "mail/reply-all" "Reply to \"all\"")
  (mh-reply (folder) "mail/reply"
    "Reply to this message\nThis button runs `mh-reply'")
  (mh-alias-grab-from-field (folder) "mail/alias"
    "Grab From alias\nThis button runs `mh-alias-grab-from-field'"
    (and (mh-extract-from-header-value) (not (mh-alias-for-from-p))))
  (mh-send (folder) "mail_compose"
    "Compose new message\nThis button runs `mh-send'")
  (mh-rescan-folder (folder) "refresh"
    "Rescan this folder\nThis button runs `mh-rescan-folder'")
  (mh-pack-folder (folder) "mail/repack"
    "Repack this folder\nThis button runs `mh-pack-folder'")
  (mh-tool-bar-search (folder) "search"
    "Search\nThis button runs `mh-tool-bar-search-function'")
  (mh-visit-folder (folder) "fld_open"
    "Visit other folder\nThis button runs `mh-visit-folder'")
  ;; Letter buffer buttons
  (mh-send-letter (letter) "mail_send" "Send this letter")
  (mh-compose-insertion (letter) "attach" "Insert attachment")
  (ispell-message (letter) "spell" "Check spelling")
  (save-buffer (letter) "save" "Save current buffer to its file"
    (buffer-modified-p))
  (undo (letter) "undo" "Undo last operation")
  (kill-region (letter) "cut"
    "Cut (kill) text in region between mark and current position")
  (menu-bar-kill-ring-save (letter) "copy"
    "Copy text in region between mark and current position")
  (yank (letter) "paste" "Paste (yank) text cut or copied earlier")
  (mh-fully-kill-draft (letter) "close" "Kill this draft")
  ;; Common buttons
  (mh-tool-bar-customize (folder letter) "preferences" "MH-E Preferences")
  (mh-tool-bar-folder-help (folder) "help"
    "Help! (general help)\nThis button runs `info'")
  (mh-tool-bar-letter-help (letter) "help"
    "Help! (general help)\nThis button runs `info'")
  ;; Folder narrowed to sequence buttons
  (mh-widen (sequence) "widen"
    "Widen from the sequence\nThis button runs `mh-widen'"))



;;; Hooks (:group 'mh-hooks + group where hook described)

(defcustom mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), the text of the message is inserted
with `mh-ins-buf-prefix' prefixed to each line.

See also the variable `mh-yank-from-start-of-msg', which controls how
much of the message passed to the hook.

This hook was historically provided to set up supercite.  You may now leave
this nil and set up supercite by setting the variable
`mh-yank-from-start-of-msg' to 'supercite or, for more automatic insertion,
to 'autosupercite.

The hook 'trivial-cite is NOT part of Emacs.  It is provided from tc.el,
available here:
 http://shasta.cs.uiuc.edu/~lrclause/tc.html
If you use it, customize `mh-yank-from-start-of-msg' to
 \"Entire message with headers\"."
  :type 'hook
  :options '(trivial-cite)
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-alias-reloaded-hook nil
  "Invoked by `mh-alias-reload' after reloading aliases."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-alias)

(defcustom mh-before-quit-hook nil
  "Invoked by \\<mh-folder-mode-map>`\\[mh-quit]' before quitting MH-E.
See also `mh-quit-hook'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-before-send-letter-hook nil
  "Invoked at the beginning of the \\<mh-letter-mode-map>\\[mh-send-letter] command."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-delete-msg-hook nil
  "Invoked after marking each message for deletion."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-edit-mhn-hook nil
  "Invoked on the formatted letter by \\<mh-letter-mode-map>\\[mh-edit-mhn]."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-find-path-hook nil
  "Invoked by `mh-find-path' after reading the user's MH profile."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-folder-mode-hook nil
  "Invoked in `mh-folder-mode' on a new folder."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-folder-updated-hook nil
  "Invoked when the folder actions (such as moves and deletes) are performed.
Variables that are useful in this hook include `mh-delete-list' and
`mh-refile-list' which can be used to see which changes are being made to
current folder, `mh-current-folder'."
  :type 'hook
  :group 'mh-hooks)

(defcustom mh-forward-hook nil
  "Invoked on the forwarded letter by \\<mh-folder-mode-map>\\[mh-forward]."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-inc-folder-hook nil
  "Invoked by \\<mh-folder-mode-map>`\\[mh-inc-folder]' after incorporating mail into a folder."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-inc)

(defcustom mh-kill-folder-suppress-prompt-hook '(mh-index-p)
  "Invoked at the beginning of the  \\<mh-folder-mode-map>`\\[mh-kill-folder]' command.
This hook is a list of functions to be called, with no arguments, which should
return a value of non-nil if you should not be asked if you're sure that you
want to remove the folder. This is useful for folders that are easily
regenerated.

The default value of `mh-index-p' suppresses the prompt on folders generated
by an index search.

WARNING: Use this hook with care. If there is a bug in your hook which returns
t on +inbox and you hit \\<mh-folder-mode-map>`\\[mh-kill-folder]' by accident
in the +inbox buffer, you will not be happy."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-letter-insert-signature-hook nil
  "Invoked after signature has been inserted.
This hook may access the actual name of the file or the function used to
insert the signature with `mh-signature-file-name'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-letter-mode-hook nil
  "Invoked in `mh-letter-mode' on a new letter."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-sending-mail)

(defcustom mh-pick-mode-hook nil
  "Invoked upon entry to `mh-pick-mode'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-index)

(defcustom mh-quit-hook nil
  "Invoked after \\<mh-folder-mode-map>`\\[mh-quit]' quits MH-E.
See also `mh-before-quit-hook'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-refile-msg-hook nil
  "Invoked after marking each message for refiling."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-show-hook nil
  "Invoked after \\<mh-folder-mode-map>`\\[mh-show]' shows a message."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-show-mode-hook nil
  "Invoked upon entry to `mh-show-mode'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-unseen-updated-hook nil
  "Invoked after the unseen sequence has been updated.
The variable `mh-seen-list' can be used to obtain the list of messages which
will be removed from the unseen sequence."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)



;;; Faces (:group 'mh-*-faces + group where faces described)

;;; Faces Used in Scan Listing (:group 'mh-folder-faces)

(defvar mh-folder-body-face 'mh-folder-body
  "Face used to highlight body text in MH-Folder buffers.")
(defface mh-folder-body
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used to highlight body text in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-cur-msg-face 'mh-folder-cur-msg
  "Face used for the current message line in MH-Folder buffers.")
(defface mh-folder-cur-msg
  '((((type tty pc) (class color))
     (:background "LightGreen"))
    (((class color) (background light))
     (:background "LightGreen")         ;Use this for solid background colour
     ;;  (:underline t)                 ;Use this for underlining
     )
    (((class color) (background dark))
     (:background "DarkOliveGreen4"))
    (t (:underline t)))
  "Face used for the current message line in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-cur-msg-number-face 'mh-folder-cur-msg-number
  "Face used to highlight the current message in MH-Folder buffers.")
(defface mh-folder-cur-msg-number
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face used to highlight the current message in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-date-face 'mh-folder-date
  "Face used to highlight the date in MH-Folder buffers.")
(defface mh-folder-date
  '((((class color) (background light))
     (:foreground "snow4"))
    (((class color) (background dark))
     (:foreground "snow3"))
    (t
     (:bold t)))
  "Face used to highlight the date in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-followup-face 'mh-folder-followup
  "Face used to highlight Re: subject text in MH-Folder buffers.")
(defface mh-folder-followup
  '((((class color) (background light))
     (:foreground "blue3"))
    (((class color) (background dark))
     (:foreground "LightGoldenRod"))
    (t
     (:bold t)))
  "Face used to highlight Re: subject text in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-msg-number-face 'mh-folder-msg-number
  "Face used to highlight the message number in MH-Folder buffers.")
(defface mh-folder-msg-number
  '((((class color) (background light))
     (:foreground "snow4"))
    (((class color) (background dark))
     (:foreground "snow3"))
    (t
     (:bold t)))
  "Face used to highlight the message number in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-deleted-face 'mh-folder-deleted
  "Face used to highlight deleted messages in MH-Folder buffers.")
(copy-face 'mh-folder-msg-number 'mh-folder-deleted)

(defvar mh-folder-refiled-face 'mh-folder-refiled
  "Face used to highlight refiled messages in MH-Folder buffers.")
(defface mh-folder-refiled
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face used to highlight refiled messages in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-subject-face 'mh-folder-subject
  "Face used to highlight subject text in MH-Folder buffers.")
(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces "^mh-folder"))
(defface mh-folder-subject
  '((((class color) (background light))
     (:foreground "blue4"))
    (((class color) (background dark))
     (:foreground "yellow"))
    (t
     (:bold t)))
  "Face used to highlight subject text in MH-Folder buffers."
  :group 'mh-folder-faces)

(defface mh-folder-tick
  '((((class color) (background dark)) (:background "#dddf7e"))
    (((class color) (background light)) (:background "#dddf7e"))
    (t (:underline t)))
  "Face used to show ticked messages."
  :group 'mh-folder-faces)

(defvar mh-folder-address-face 'mh-folder-address
  "Face used to highlight the address in MH-Folder buffers.")
(copy-face 'mh-folder-subject 'mh-folder-address)

(defvar mh-folder-scan-format-face 'mh-folder-scan-format
  "Face used to highlight `mh-scan-format-regexp' matches in MH-Folder buffers.")
(copy-face 'mh-folder-followup 'mh-folder-scan-format)

(defvar mh-folder-to-face 'mh-folder-to
  "Face used to highlight the To: string in MH-Folder buffers.")
(defface mh-folder-to
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used to highlight the To: string in MH-Folder buffers."
  :group 'mh-folder-faces)



;;; Faces Used in Searching (:group 'mh-index-faces)

(defvar mh-index-folder-face 'mh-index-folder
  "Face used to highlight folders in MH-Index buffers.")
(defface mh-index-folder
  '((((class color) (background light))
     (:foreground "dark green" :bold t))
    (((class color) (background dark))
     (:foreground "indian red" :bold t))
    (t
     (:bold t)))
  "Face used to highlight folders in MH-Index buffers."
  :group 'mh-index-faces)



;;; Faces Used in Message Drafts (:group 'mh-letter-faces)

(defface mh-letter-header-field
  '((((class color) (background light))
     (:background "gray90"))
    (((class color) (background dark))
     (:background "gray10"))
    (t (:bold t)))
  "Face used to display header fields in draft buffers."
  :group 'mh-letter-faces)



;;; Faces Used in Message Display (:group 'mh-show-faces)

(defvar mh-show-cc-face 'mh-show-cc
  "Face used to highlight cc: header fields.")
(defface mh-show-cc
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face used to highlight cc: header fields."
  :group 'mh-show-faces)

(defvar mh-show-date-face 'mh-show-date
  "Face used to highlight the Date: header field.")
(defface mh-show-date
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "Gray90" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:bold t :underline t)))
  "Face used to highlight the Date: header field."
  :group 'mh-show-faces)

(defvar mh-show-header-face 'mh-show-header
  "Face used to deemphasize unspecified header fields.")
(defface mh-show-header
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used to deemphasize unspecified header fields."
  :group 'mh-show-faces)

(defvar mh-show-pgg-good-face 'mh-show-pgg-good
  "Face used to highlight a good PGG signature.")
(defface mh-show-pgg-good
  '((t (:bold t :foreground "LimeGreen")))
  "Face used to highlight a good PGG signature."
  :group 'mh-show-faces)

(defvar mh-show-pgg-unknown-face 'mh-show-pgg-unknown
  "Face used to highlight a PGG signature whose status is unknown.
This face is also used for a signature when the signer is untrusted.")
(defface mh-show-pgg-unknown
  '((t (:bold t :foreground "DarkGoldenrod2")))
  "Face used to highlight a PGG signature whose status is unknown.
This face is also used for a signature when the signer is untrusted."
  :group 'mh-show-faces)

(defvar mh-show-pgg-bad-face 'mh-show-pgg-bad
  "Face used to highlight a bad PGG signature.")
(defface mh-show-pgg-bad
  '((t (:bold t :foreground "DeepPink1")))
  "Face used to highlight a bad PGG signature."
  :group 'mh-show-faces)

(defface mh-show-signature
  '((t (:italic t)))
  "Face used to highlight the message signature."
  :group 'mh-show-faces)

(defvar mh-show-to-face 'mh-show-to
  "Face used to highlight the To: header field.")
(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces "^mh-show"))
(defface mh-show-to
  '((((class grayscale) (background light))
     (:foreground "DimGray" :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :underline t))
    (((class color) (background light)) (:foreground "SaddleBrown"))
    (((class color) (background dark))  (:foreground "burlywood"))
    (t (:underline t)))
  "Face used to highlight the To: header field."
  :group 'mh-show-faces)

(defvar mh-show-from-face 'mh-show-from
  "Face used to highlight the From: header field.")
(defface mh-show-from
  '((((class color) (background light))
     (:foreground "red3"))
    (((class color) (background dark))
     (:foreground "cyan"))
    (t
     (:bold t)))
  "Face used to highlight the From: header field."
  :group 'mh-show-faces)

(defface mh-show-xface
  '((t (:foreground "black" :background "white")))
  "Face used to display the X-Face image.
The background and foreground is used in the image."
  :group 'mh-show-faces)

(defvar mh-show-subject-face 'mh-show-subject
  "Face used to highlight the Subject: header field.")
(copy-face 'mh-folder-subject 'mh-show-subject)



;;; Faces Used in Speedbar (:group 'mh-speed-faces)

(defface mh-speedbar-folder
  '((((class color) (background light))
     (:foreground "blue4"))
    (((class color) (background dark))
     (:foreground "light blue")))
  "Face used for folders in the speedbar buffer."
  :group 'mh-speed-faces)

(defface mh-speedbar-selected-folder
  '((((class color) (background light))
     (:foreground "red1" :underline t))
    (((class color) (background dark))
     (:foreground "red1" :underline t))
    (t (:underline t)))
  "Face used for the current folder."
  :group 'mh-speed-faces)

(defface mh-speedbar-folder-with-unseen-messages
  '((t (:inherit mh-speedbar-folder :bold t)))
  "Face used for folders in the speedbar buffer which have unread messages."
  :group 'mh-speed-faces)

(defface mh-speedbar-selected-folder-with-unseen-messages
  '((t (:inherit mh-speedbar-selected-folder :bold t)))
  "Face used for the current folder when it has unread messages."
  :group 'mh-speed-faces)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 778d2a20-82e2-4276-be9d-309386776a68
;;; mh-customize.el ends here
