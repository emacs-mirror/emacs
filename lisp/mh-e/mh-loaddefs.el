;;; mh-loaddefs.el --- automatically extracted autoloads
;;
;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;; Author: Bill Wohler <wohler@newt.com>
;;; Keywords: mail
;;; Commentary:
;;; Change Log:
;;; Code:

;;;### (autoloads (mh-letter-complete mh-open-line mh-fully-kill-draft
;;;;;;  mh-yank-cur-msg mh-insert-letter mh-send-letter mh-check-whom
;;;;;;  mh-insert-signature mh-to-fcc mh-to-field mh-fill-paragraph-function
;;;;;;  mh-send-other-window mh-send mh-reply mh-redistribute mh-forward
;;;;;;  mh-extract-rejected-mail mh-edit-again) "mh-comp" "mh-comp.el"
;;;;;;  (16040 52697))
;;; Generated autoloads from mh-comp.el

(autoload (quote mh-edit-again) "mh-comp" "\
Clean up a draft or a message MSG previously sent and make it resendable.
Default is the current message.
The variable `mh-new-draft-cleaned-headers' specifies the headers to remove.
See also documentation for `\\[mh-send]' function." t nil)

(autoload (quote mh-extract-rejected-mail) "mh-comp" "\
Extract message MSG returned by the mail system and make it resendable.
Default is the current message.  The variable `mh-new-draft-cleaned-headers'
gives the headers to clean out of the original message.
See also documentation for `\\[mh-send]' function." t nil)

(autoload (quote mh-forward) "mh-comp" "\
Forward messages to the recipients TO and CC.
Use optional MSG-OR-SEQ argument to specify a message or sequence to forward.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is forwarded.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence.

See also documentation for `\\[mh-send]' function." t nil)

(autoload (quote mh-redistribute) "mh-comp" "\
Redistribute displayed message to recipients TO and CC.
Use optional argument MSG to redistribute another message.
Depending on how your copy of MH was compiled, you may need to change the
setting of the variable `mh-redist-full-contents'.  See its documentation." t nil)

(autoload (quote mh-reply) "mh-comp" "\
Reply to MESSAGE.
Default is the displayed message.
If the optional argument REPLY-TO is not given, prompts for type of addresses
to reply to:
   from    sender only,
   to      sender and primary recipients,
   cc/all  sender and all recipients.
If optional prefix argument INCLUDEP provided, then include the message
in the reply using filter `mhl.reply' in your MH directory.
If the file named by `mh-repl-formfile' exists, it is used as a skeleton
for the reply.  See also documentation for `\\[mh-send]' function." t nil)

(autoload (quote mh-send) "mh-comp" "\
Compose and send a letter.

Do not call this function from outside MH-E; use \\[mh-smail] instead.

The file named by `mh-comp-formfile' will be used as the form.
The letter is composed in `mh-letter-mode'; see its documentation for more
details.
If `mh-compose-letter-function' is defined, it is called on the draft and
passed three arguments: TO, CC, and SUBJECT." t nil)

(autoload (quote mh-send-other-window) "mh-comp" "\
Compose and send a letter in another window.

Do not call this function from outside MH-E; use \\[mh-smail-other-window]
instead.

The file named by `mh-comp-formfile' will be used as the form.
The letter is composed in `mh-letter-mode'; see its documentation for more
details.
If `mh-compose-letter-function' is defined, it is called on the draft and
passed three arguments: TO, CC, and SUBJECT." t nil)

(autoload (quote mh-fill-paragraph-function) "mh-comp" "\
Fill paragraph at or after point.
Prefix ARG means justify as well. This function enables `fill-paragraph' to
work better in MH-Letter mode." t nil)

(autoload (quote mh-to-field) "mh-comp" "\
Move point to the end of a specified header field.
The field is indicated by the previous keystroke (the last keystroke
of the command) according to the list in the variable `mh-to-field-choices'.
Create the field if it does not exist.  Set the mark to point before moving." t nil)

(autoload (quote mh-to-fcc) "mh-comp" "\
Insert an Fcc: FOLDER field in the current message.
Prompt for the field name with a completion list of the current folders." t nil)

(autoload (quote mh-insert-signature) "mh-comp" "\
Insert the file named by `mh-signature-file-name' at point.
The value of `mh-letter-insert-signature-hook' is a list of functions to be
called, with no arguments, before the signature is actually inserted." t nil)

(autoload (quote mh-check-whom) "mh-comp" "\
Verify recipients of the current letter, showing expansion of any aliases." t nil)

(autoload (quote mh-send-letter) "mh-comp" "\
Send the draft letter in the current buffer.
If optional prefix argument ARG is provided, monitor delivery.
The value of `mh-before-send-letter-hook' is a list of functions to be called,
with no arguments, before doing anything.
Run `\\[mh-edit-mhn]' if mhn directives are present; otherwise
run `\\[mh-mml-to-mime]' if mml directives are present.
Insert X-Mailer field if variable `mh-insert-x-mailer-flag' is set.
Insert X-Face field if the file specified by `mh-x-face-file' exists." t nil)

(autoload (quote mh-insert-letter) "mh-comp" "\
Insert a message into the current letter.
Removes the header fields according to the variable `mh-invisible-headers'.
Prefixes each non-blank line with `mh-ins-buf-prefix', unless
`mh-yank-from-start-of-msg' is set for supercite in which case supercite is
used to format the message.
Prompts for FOLDER and MESSAGE.  If prefix argument VERBATIM provided, do
not indent and do not delete headers.  Leaves the mark before the letter
and point after it." t nil)

(autoload (quote mh-yank-cur-msg) "mh-comp" "\
Insert the current message into the draft buffer.
Prefix each non-blank line in the message with the string in
`mh-ins-buf-prefix'.  If a region is set in the message's buffer, then
only the region will be inserted.  Otherwise, the entire message will
be inserted if `mh-yank-from-start-of-msg' is non-nil.  If this variable
is nil, the portion of the message following the point will be yanked.
If `mh-delete-yanked-msg-window-flag' is non-nil, any window displaying the
yanked message will be deleted." t nil)

(autoload (quote mh-fully-kill-draft) "mh-comp" "\
Kill the draft message file and the draft message buffer.
Use \\[kill-buffer] if you don't want to delete the draft message file." t nil)

(autoload (quote mh-open-line) "mh-comp" "\
Insert a newline and leave point after it.
In addition, insert newline and quoting characters before text after point.
This is useful in breaking up paragraphs in replies." t nil)

(autoload (quote mh-letter-complete) "mh-comp" "\
Perform completion on header field or word preceding point.
Alias completion is done within the mail header on selected fields and
by the function designated by `mh-letter-complete-function' elsewhere,
passing the prefix ARG if any." t nil)

;;;***

;;;### (autoloads (mh-customize) "mh-customize" "mh-customize.el"
;;;;;;  (16040 52697))
;;; Generated autoloads from mh-customize.el

(autoload (quote mh-customize) "mh-customize" "\
Customize MH-E variables.
With optional argument DELETE-OTHER-WINDOWS-FLAG, other windows in the frame
are removed." t nil)

;;;***

;;;### (autoloads (mh-goto-cur-msg mh-update-sequences mh-folder-line-matches-show-buffer-p)
;;;;;;  "mh-e" "mh-e.el" (16040 52698))
;;; Generated autoloads from mh-e.el

(autoload (quote mh-folder-line-matches-show-buffer-p) "mh-e" "\
Return t if the message under point in folder-mode is in the show buffer.
Return nil in any other circumstance (no message under point, no show buffer,
the message in the show buffer doesn't match." nil nil)

(autoload (quote mh-update-sequences) "mh-e" "\
Update MH's Unseen-Sequence and current folder and message.
Flush MH-E's state out to MH. The message at the cursor becomes current." t nil)

(autoload (quote mh-goto-cur-msg) "mh-e" "\
Position the cursor at the current message.
When optional argument MINIMAL-CHANGES-FLAG is non-nil, the function doesn't
recenter the folder buffer." nil nil)

;;;***

;;;### (autoloads (mh-prefix-help mh-help mh-ephem-message mh-store-buffer
;;;;;;  mh-store-msg mh-undo-folder mh-sort-folder mh-print-msg mh-page-digest-backwards
;;;;;;  mh-page-digest mh-pipe-msg mh-pack-folder mh-list-folders
;;;;;;  mh-kill-folder mh-copy-msg mh-burst-digest) "mh-funcs" "mh-funcs.el"
;;;;;;  (16040 52698))
;;; Generated autoloads from mh-funcs.el

(autoload (quote mh-burst-digest) "mh-funcs" "\
Burst apart the current message, which should be a digest.
The message is replaced by its table of contents and the messages from the
digest are inserted into the folder after that message." t nil)

(autoload (quote mh-copy-msg) "mh-funcs" "\
Copy the specified MSG-OR-SEQ to another FOLDER without deleting them.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is copied.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence." t nil)

(autoload (quote mh-kill-folder) "mh-funcs" "\
Remove the current folder and all included messages.
Removes all of the messages (files) within the specified current folder,
and then removes the folder (directory) itself." t nil)

(autoload (quote mh-list-folders) "mh-funcs" "\
List mail folders." t nil)

(autoload (quote mh-pack-folder) "mh-funcs" "\
Renumber the messages of a folder to be 1..n.
First, offer to execute any outstanding commands for the current folder. If
optional prefix argument provided, prompt for the RANGE of messages to display
after packing. Otherwise, show the entire folder." t nil)

(autoload (quote mh-pipe-msg) "mh-funcs" "\
Pipe the current message through the given shell COMMAND.
If INCLUDE-HEADERS (prefix argument) is provided, send the entire message.
Otherwise just send the message's body without the headers." t nil)

(autoload (quote mh-page-digest) "mh-funcs" "\
Advance displayed message to next digested message." t nil)

(autoload (quote mh-page-digest-backwards) "mh-funcs" "\
Back up displayed message to previous digested message." t nil)

(autoload (quote mh-print-msg) "mh-funcs" "\
Print MSG-OR-SEQ on printer.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is printed.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence.

The variable `mh-lpr-command-format' is used to generate the print command.
The messages are formatted by mhl. See the variable `mhl-formfile'." t nil)

(autoload (quote mh-sort-folder) "mh-funcs" "\
Sort the messages in the current folder by date.
Calls the MH program sortm to do the work.
The arguments in the list `mh-sortm-args' are passed to sortm if the optional
argument EXTRA-ARGS is given." t nil)

(autoload (quote mh-undo-folder) "mh-funcs" "\
Undo all pending deletes and refiles in current folder.
Argument IGNORE is deprecated." t nil)

(autoload (quote mh-store-msg) "mh-funcs" "\
Store the file(s) contained in the current message into DIRECTORY.
The message can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
`mh-store-default-directory' or the current directory." t nil)

(autoload (quote mh-store-buffer) "mh-funcs" "\
Store the file(s) contained in the current buffer into DIRECTORY.
The buffer can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
`mh-store-default-directory' or the current directory." t nil)

(autoload (quote mh-ephem-message) "mh-funcs" "\
Display STRING in the minibuffer momentarily." nil nil)

(autoload (quote mh-help) "mh-funcs" "\
Display cheat sheet for the MH-Folder commands in minibuffer." t nil)

(autoload (quote mh-prefix-help) "mh-funcs" "\
Display cheat sheet for the commands of the current prefix in minibuffer." t nil)

;;;***

;;;### (autoloads (mh-insert-identity mh-identity-list-set mh-identity-make-menu)
;;;;;;  "mh-identity" "mh-identity.el" (16040 52698))
;;; Generated autoloads from mh-identity.el

(autoload (quote mh-identity-make-menu) "mh-identity" "\
Build (or rebuild) the Identity menu (e.g. after the list is modified)." nil nil)

(autoload (quote mh-identity-list-set) "mh-identity" "\
Update the `mh-identity-list' variable, and rebuild the menu.
Sets the default for SYMBOL (e.g. `mh-identity-list') to VALUE (as set in
customization).  This is called after 'customize is used to alter
`mh-identity-list'." nil nil)

(autoload (quote mh-insert-identity) "mh-identity" "\
Insert proper fields for given IDENTITY.
Edit the `mh-identity-list' variable to define identity." t nil)

;;;***

;;;### (autoloads (mh-inc-spool-list-set) "mh-inc" "mh-inc.el" (16040
;;;;;;  52698))
;;; Generated autoloads from mh-inc.el

(autoload (quote mh-inc-spool-list-set) "mh-inc" "\
Set-default SYMBOL to VALUE to update the `mh-inc-spool-list' variable.
Also rebuilds the user commands.
This is called after 'customize is used to alter `mh-inc-spool-list'." nil nil)

;;;***

;;;### (autoloads (mh-index-choose mh-namazu-execute-search mh-swish++-execute-search
;;;;;;  mh-swish-execute-search mh-index-new-messages mh-glimpse-execute-search
;;;;;;  mh-index-execute-commands mh-index-update-unseen mh-index-visit-folder
;;;;;;  mh-index-delete-folder-headers mh-index-group-by-folder mh-index-insert-folder-headers
;;;;;;  mh-index-previous-folder mh-index-next-folder mh-index-parse-search-regexp
;;;;;;  mh-index-do-search mh-index-search mh-index-update-maps)
;;;;;;  "mh-index" "mh-index.el" (16040 52698))
;;; Generated autoloads from mh-index.el

(autoload (quote mh-index-update-maps) "mh-index" "\
Annotate all as yet unannotated messages in FOLDER with their MD5 hash.
As a side effect msg -> checksum map is updated. Optional argument ORIGIN-MAP
is a hashtable which maps each message in the index folder to the original
folder and message from whence it was copied. If present the
checksum -> (origin-folder, origin-index) map is updated too." nil nil)

(autoload (quote mh-index-search) "mh-index" "\
Perform an indexed search in an MH mail folder.
Use a prefix argument to repeat the search, as in REDO-SEARCH-FLAG below.

If REDO-SEARCH-FLAG is non-nil and the current folder buffer was generated by a
index search, then the search is repeated. Otherwise, FOLDER is searched with
SEARCH-REGEXP and the results are presented in an MH-E folder. If FOLDER is
\"+\" then mail in all folders are searched. Optional argument WINDOW-CONFIG
stores the window configuration that will be restored after the user quits the
folder containing the index search results. If optional argument UNSEEN-FLAG
is non-nil, then all the messages are marked as unseen.

Four indexing programs are supported; if none of these are present, then grep
is used. This function picks the first program that is available on your
system. If you would prefer to use a different program, set the customization
variable `mh-index-program' accordingly.

The documentation for the following functions describes how to generate the
index for each program:

    - `mh-swish++-execute-search'
    - `mh-swish-execute-search'
    - `mh-mairix-execute-search'
    - `mh-namazu-execute-search'
    - `mh-glimpse-execute-search'

If none of these programs are present then we use pick. If desired grep can be
used instead. Details about these methods can be found in:

    - `mh-pick-execute-search'
    - `mh-grep-execute-search'

This and related functions use an X-MHE-Checksum header to cache the MD5
checksum of a message. This means that already present X-MHE-Checksum headers
in the incoming email could result in messages not being found. The following
procmail recipe should avoid this:

  :0 wf
  | formail -R \"X-MHE-Checksum\" \"Old-X-MHE-Checksum\"

This has the effect of renaming already present X-MHE-Checksum headers." t nil)

(autoload (quote mh-index-do-search) "mh-index" "\
Construct appropriate regexp and call `mh-index-search'." t nil)

(autoload (quote mh-index-parse-search-regexp) "mh-index" "\
Construct parse tree for INPUT-STRING.
All occurrences of &, |, ! and ~ in INPUT-STRING are replaced by AND, OR and
NOT as appropriate. Then the resulting string is parsed." nil nil)

(autoload (quote mh-index-next-folder) "mh-index" "\
Jump to the next folder marker.
The function is only applicable to folders displaying index search results.
With non-nil optional argument BACKWARD-FLAG, jump to the previous group of
results." t nil)

(autoload (quote mh-index-previous-folder) "mh-index" "\
Jump to the previous folder marker." t nil)

(autoload (quote mh-index-insert-folder-headers) "mh-index" "\
Annotate the search results with original folder names." nil nil)

(autoload (quote mh-index-group-by-folder) "mh-index" "\
Partition the messages based on source folder.
Returns an alist with the the folder names in the car and the cdr being the
list of messages originally from that folder." nil nil)

(autoload (quote mh-index-delete-folder-headers) "mh-index" "\
Delete the folder headers." nil nil)

(autoload (quote mh-index-visit-folder) "mh-index" "\
Visit original folder from where the message at point was found." t nil)

(autoload (quote mh-index-update-unseen) "mh-index" "\
Remove counterpart of MSG in source folder from `mh-unseen-seq'.
Also `mh-update-unseen' is called in the original folder, if we have it open." nil nil)

(autoload (quote mh-index-execute-commands) "mh-index" "\
Delete/refile the actual messages.
The copies in the searched folder are then deleted/refiled to get the desired
result. Before deleting the messages we make sure that the message being
deleted is identical to the one that the user has marked in the index buffer." nil nil)

(autoload (quote mh-glimpse-execute-search) "mh-index" "\
Execute glimpse and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.glimpse. Then create the file
/home/user/Mail/.glimpse/.glimpse_exclude with the following contents:

    */.*
    */#*
    */,*
    */*~
    ^/home/user/Mail/.glimpse
    ^/home/user/Mail/mhe-index

If there are any directories you would like to ignore, append lines like the
following to .glimpse_exclude:

    ^/home/user/Mail/scripts

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The configuration file above assumes that the results are found in sub-folders
of `mh-index-folder' which is +mhe-index by default.

Use the following command line to generate the glimpse index. Run this
daily from cron:

    glimpseindex -H /home/user/Mail/.glimpse /home/user/Mail

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search." nil nil)

(autoload (quote mh-index-new-messages) "mh-index" "\
Display new messages.
All messages in the `mh-unseen-seq' sequence from FOLDERS are displayed.
By default the folders specified by `mh-index-new-messages-folders' are
searched. With a prefix argument, enter a space-separated list of folders, or
nothing to search all folders." t nil)

(autoload (quote mh-swish-execute-search) "mh-index" "\
Execute swish-e and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.swish. Then create the file
/home/user/Mail/.swish/config with the following contents:

    IndexDir /home/user/Mail
    IndexFile /home/user/Mail/.swish/index
    IndexName \"Mail Index\"
    IndexDescription \"Mail Index\"
    IndexPointer \"http://nowhere\"
    IndexAdmin \"nobody\"
    #MetaNames automatic
    IndexReport 3
    FollowSymLinks no
    UseStemming no
    IgnoreTotalWordCountWhenRanking yes
    WordCharacters abcdefghijklmnopqrstuvwxyz0123456789-
    BeginCharacters abcdefghijklmnopqrstuvwxyz
    EndCharacters abcdefghijklmnopqrstuvwxyz0123456789
    IgnoreLimit 50 1000
    IndexComments 0
    FileRules pathname contains /home/user/Mail/.swish
    FileRules pathname contains /home/user/Mail/mhe-index
    FileRules filename is index
    FileRules filename is \\..*
    FileRules filename is #.*
    FileRules filename is ,.*
    FileRules filename is .*~

If there are any directories you would like to ignore, append lines like the
following to config:

    FileRules pathname contains /home/user/Mail/scripts

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The configuration file above assumes that the results are found in sub-folders
of `mh-index-folder' which is +mhe-index by default.

Use the following command line to generate the swish index. Run this
daily from cron:

    swish-e -c /home/user/Mail/.swish/config

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search." nil nil)

(autoload (quote mh-swish++-execute-search) "mh-index" "\
Execute swish++ and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.swish++. Then create the file
/home/user/Mail/.swish++/swish++.conf with the following contents:

    IncludeMeta         Bcc Cc Comments Content-Description From Keywords
    IncludeMeta         Newsgroups Resent-To Subject To
    IncludeMeta         Message-Id References In-Reply-To
    IncludeFile         Mail    *
    IndexFile           /home/user/Mail/.swish++/swish++.index

Use the following command line to generate the swish index. Run this
daily from cron:

 find /home/user/Mail -path /home/user/Mail/mhe-index -prune \\
                   -o -path /home/user/Mail/.swish++ -prune \\
                   -o -name \"[0-9]*\" -print \\
    | index -c /home/user/Mail/.swish++/swish++.conf /home/user/Mail

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The command above assumes that the results are found in sub-folders of
`mh-index-folder' which is +mhe-index by default.

On some systems (Debian GNU/Linux, for example), use index++ instead of index.

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search." nil nil)

(autoload (quote mh-namazu-execute-search) "mh-index" "\
Execute namazu and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.namazu. Then create the file
/home/user/Mail/.namazu/mknmzrc with the following contents:

    package conf;  # Don't remove this line!
    $ADDRESS = 'user@localhost';
    $ALLOW_FILE = \"[0-9]*\";
    $EXCLUDE_PATH = \"^/home/user/Mail/(mhe-index|spam)\";

In the above example configuration, none of the mail files contained in the
directories /home/user/Mail/mhe-index and /home/user/Mail/spam are indexed.

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The configuration file above assumes that the results are found in sub-folders
of `mh-index-folder' which is +mhe-index by default.

Use the following command line to generate the namazu index. Run this
daily from cron:

   mknmz -f /home/user/Mail/.namazu/mknmzrc -O /home/user/Mail/.namazu \\
         /home/user/Mail

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search." nil nil)

(autoload (quote mh-index-choose) "mh-index" "\
Choose an indexing function.
The side-effects of this function are that the variables `mh-indexer',
`mh-index-execute-search-function', and `mh-index-next-result-function' are
set according to the first indexer in `mh-indexer-choices' present on the
system." nil nil)

;;;***

;;;### (autoloads (mh-junk-whitelist mh-junk-blacklist) "mh-junk"
;;;;;;  "mh-junk.el" (16040 52698))
;;; Generated autoloads from mh-junk.el

(autoload (quote mh-junk-blacklist) "mh-junk" "\
Blacklist MSG-OR-SEQ as spam.
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
  - `mh-spamassassin-blacklist'" t nil)

(autoload (quote mh-junk-whitelist) "mh-junk" "\
Whitelist MSG-OR-SEQ incorrectly classified as spam.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is whitelisted.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence.

First the appropriate function is called depending on the value of
`mh-junk-choice'. Then the message is refiled to `mh-inbox'.

To change the spam program being used, customize `mh-junk-program'. Directly
setting `mh-junk-choice' is not recommended." t nil)

;;;***

;;;### (autoloads (mh-mime-inline-part mh-mime-save-part mh-push-button
;;;;;;  mh-press-button mh-mime-display mh-decode-message-header
;;;;;;  mh-mime-save-parts mh-display-emphasis mh-display-smileys
;;;;;;  mh-add-missing-mime-version-header mh-destroy-postponed-handles
;;;;;;  mh-mime-cleanup mh-mml-directive-present-p mh-mml-secure-message-encrypt-pgpmime
;;;;;;  mh-mml-secure-message-sign-pgpmime mh-mml-attach-file mh-mml-forward-message
;;;;;;  mh-mml-to-mime mh-mhn-directive-present-p mh-revert-mhn-edit
;;;;;;  mh-edit-mhn mh-mhn-compose-forw mh-mhn-compose-external-compressed-tar
;;;;;;  mh-mhn-compose-anon-ftp mh-mhn-compose-insertion mh-compose-forward
;;;;;;  mh-compose-insertion) "mh-mime" "mh-mime.el" (16040 52699))
;;; Generated autoloads from mh-mime.el

(autoload (quote mh-compose-insertion) "mh-mime" "\
Add a directive to insert a MIME part from a file, using mhn or gnus.
If the variable `mh-compose-insertion' is set to 'mhn, then that will be used.
If it is set to 'gnus, then that will be used instead.
Optional argument INLINE means make it an inline attachment." t nil)

(autoload (quote mh-compose-forward) "mh-mime" "\
Add a MIME directive to forward a message, using mhn or gnus.
If the variable `mh-compose-insertion' is set to 'mhn, then that will be used.
If it is set to 'gnus, then that will be used instead.
Optional argument DESCRIPTION is a description of the attachment.
Optional argument FOLDER is the folder from which the forwarded message should
come.
Optional argument MESSAGE is the message to forward.
If any of the optional arguments are absent, they are prompted for." t nil)

(autoload (quote mh-mhn-compose-insertion) "mh-mime" "\
Add a directive to insert a MIME message part from a file.
This is the typical way to insert non-text parts in a message.

Arguments are FILENAME, which tells where to find the file, TYPE, the MIME
content type, DESCRIPTION, a line of text for the Content-Description field.
ATTRIBUTES is a comma separated list of name=value pairs that is appended to
the Content-Type field of the attachment.

See also \\[mh-edit-mhn]." t nil)

(autoload (quote mh-mhn-compose-anon-ftp) "mh-mime" "\
Add a directive for a MIME anonymous ftp external body part.
This directive tells MH to include a reference to a message/external-body part
retrievable by anonymous FTP.

Arguments are HOST and FILENAME, which tell where to find the file, TYPE, the
MIME content type, and DESCRIPTION, a line of text for the Content-description
header.

See also \\[mh-edit-mhn]." t nil)

(autoload (quote mh-mhn-compose-external-compressed-tar) "mh-mime" "\
Add a directive to include a MIME reference to a compressed tar file.
The file should be available via anonymous ftp. This directive tells MH to
include a reference to a message/external-body part.

Arguments are HOST and FILENAME, which tell where to find the file, and
DESCRIPTION, a line of text for the Content-description header.

See also \\[mh-edit-mhn]." t nil)

(autoload (quote mh-mhn-compose-forw) "mh-mime" "\
Add a forw directive to this message, to forward a message with MIME.
This directive tells MH to include the named messages in this one.

Arguments are DESCRIPTION, a line of text for the Content-description header,
and FOLDER and MESSAGES, which name the message(s) to be forwarded.

See also \\[mh-edit-mhn]." t nil)

(autoload (quote mh-edit-mhn) "mh-mime" "\
Format the current draft for MIME, expanding any mhn directives.

Process the current draft with the mhn program, which, using directives
already inserted in the draft, fills in all the MIME components and header
fields.

This step is performed automatically when sending the message, but this
function may be called manually before sending the draft as well.

The `\\[mh-revert-mhn-edit]' command undoes this command. The arguments in the
list `mh-mhn-args' are passed to mhn if this function is passed an optional
prefix argument EXTRA-ARGS.

For assistance with creating mhn directives to insert various types of
components in a message, see \\[mh-mhn-compose-insertion] (generic insertion
from a file), \\[mh-mhn-compose-anon-ftp] (external reference to file via
anonymous ftp), \\[mh-mhn-compose-external-compressed-tar] (reference to
compressed tar file via anonymous ftp), and \\[mh-mhn-compose-forw] (forward
message).

The value of `mh-edit-mhn-hook' is a list of functions to be called, with no
arguments, after performing the conversion.

The mhn program is part of MH version 6.8 or later." t nil)

(autoload (quote mh-revert-mhn-edit) "mh-mime" "\
Undo the effect of \\[mh-edit-mhn] by reverting to the backup file.
Optional non-nil argument NOCONFIRM means don't ask for confirmation." t nil)

(autoload (quote mh-mhn-directive-present-p) "mh-mime" "\
Check if the current buffer has text which might be a MHN directive." nil nil)

(autoload (quote mh-mml-to-mime) "mh-mime" "\
Compose MIME message from mml directives.
This step is performed automatically when sending the message, but this
function may be called manually before sending the draft as well." t nil)

(autoload (quote mh-mml-forward-message) "mh-mime" "\
Forward a message as attachment.
The function will prompt the user for a DESCRIPTION, a FOLDER and MESSAGE
number." nil nil)

(autoload (quote mh-mml-attach-file) "mh-mime" "\
Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[mh-send-letter]'.
Message disposition is \"inline\" or \"attachment\" and is prompted for if
DISPOSITION is nil.

This is basically `mml-attach-file' from gnus, modified such that a prefix
argument yields an `inline' disposition and Content-Type is determined
automatically." nil nil)

(autoload (quote mh-mml-secure-message-sign-pgpmime) "mh-mime" "\
Add directive to encrypt/sign the entire message." t nil)

(autoload (quote mh-mml-secure-message-encrypt-pgpmime) "mh-mime" "\
Add directive to encrypt and sign the entire message.
If called with a prefix argument DONTSIGN, only encrypt (do NOT sign)." t nil)

(autoload (quote mh-mml-directive-present-p) "mh-mime" "\
Check if the current buffer has text which may be an MML directive." nil nil)

(autoload (quote mh-mime-cleanup) "mh-mime" "\
Free the decoded MIME parts." nil nil)

(autoload (quote mh-destroy-postponed-handles) "mh-mime" "\
Free MIME data for externally displayed mime parts." nil nil)

(autoload (quote mh-add-missing-mime-version-header) "mh-mime" "\
Some mail programs don't put a MIME-Version header.
I have seen this only in spam, so maybe we shouldn't fix this ;-)" nil nil)

(autoload (quote mh-display-smileys) "mh-mime" "\
Function to display smileys." nil nil)

(autoload (quote mh-display-emphasis) "mh-mime" "\
Function to display graphical emphasis." nil nil)

(autoload (quote mh-mime-save-parts) "mh-mime" "\
Store the MIME parts of the current message.
If ARG, prompt for directory, else use that specified by the variable
`mh-mime-save-parts-default-directory'. These directories may be superseded by
mh_profile directives, since this function calls on mhstore or mhn to do the
actual storing." t nil)

(autoload (quote mh-decode-message-header) "mh-mime" "\
Decode RFC2047 encoded message header fields." nil nil)

(autoload (quote mh-mime-display) "mh-mime" "\
Display (and possibly decode) MIME handles.
Optional argument, PRE-DISSECTED-HANDLES is a list of MIME handles. If
present they are displayed otherwise the buffer is parsed and then
displayed." nil nil)

(autoload (quote mh-press-button) "mh-mime" "\
Press MIME button.
If the MIME part is visible then it is removed. Otherwise the part is
displayed." t nil)

(autoload (quote mh-push-button) "mh-mime" "\
Click MIME button for EVENT.
If the MIME part is visible then it is removed. Otherwise the part is
displayed. This function is called when the mouse is used to click the MIME
button." t nil)

(autoload (quote mh-mime-save-part) "mh-mime" "\
Save MIME part at point." t nil)

(autoload (quote mh-mime-inline-part) "mh-mime" "\
Toggle display of the raw MIME part." t nil)

;;;***

;;;### (autoloads (mh-do-search mh-pick-do-search mh-do-pick-search
;;;;;;  mh-search-folder) "mh-pick" "mh-pick.el" (16040 52699))
;;; Generated autoloads from mh-pick.el

(autoload (quote mh-search-folder) "mh-pick" "\
Search FOLDER for messages matching a pattern.
This function uses the MH command `pick' to do the work.
Add the messages found to the sequence named `search'.
Argument WINDOW-CONFIG is the current window configuration and is used when
the search folder is dismissed." t nil)

(autoload (quote mh-do-pick-search) "mh-pick" "\
Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in `mh-searching-folder'.
Add the messages found to the sequence named `search'.

This is a deprecated function and `mh-pick-do-search' should be used instead." t nil)

(autoload (quote mh-pick-do-search) "mh-pick" "\
Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in `mh-searching-folder'.
Add the messages found to the sequence named `search'." t nil)

(autoload (quote mh-do-search) "mh-pick" "\
Use the default searching function.
If \\[mh-search-folder] was used to create the search pattern then pick is used
to search the folder. Otherwise if \\[mh-index-search] was used then the
indexing program specified in `mh-index-program' is used." t nil)

;;;***

;;;### (autoloads (mh-narrow-to-tick mh-toggle-tick mh-notate-tick
;;;;;;  mh-thread-refile mh-thread-delete mh-thread-ancestor mh-thread-previous-sibling
;;;;;;  mh-thread-next-sibling mh-thread-forget-message mh-toggle-threads
;;;;;;  mh-thread-add-spaces mh-thread-inc mh-delete-subject-or-thread
;;;;;;  mh-delete-subject mh-narrow-to-subject mh-region-to-msg-list
;;;;;;  mh-interactive-msg-or-seq mh-msg-or-seq-to-msg-list mh-iterate-on-msg-or-seq
;;;;;;  mh-iterate-on-messages-in-region mh-add-to-sequence mh-notate-cur
;;;;;;  mh-notate-seq mh-map-to-seq-msgs mh-rename-seq mh-widen mh-put-msg-in-seq
;;;;;;  mh-narrow-to-seq mh-msg-is-in-seq mh-list-sequences mh-delete-seq)
;;;;;;  "mh-seq" "mh-seq.el" (16040 52700))
;;; Generated autoloads from mh-seq.el

(autoload (quote mh-delete-seq) "mh-seq" "\
Delete the SEQUENCE." t nil)

(autoload (quote mh-list-sequences) "mh-seq" "\
List the sequences defined in the folder being visited." t nil)

(autoload (quote mh-msg-is-in-seq) "mh-seq" "\
Display the sequences that contain MESSAGE.
Default is the displayed message." t nil)

(autoload (quote mh-narrow-to-seq) "mh-seq" "\
Restrict display of this folder to just messages in SEQUENCE.
Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

(autoload (quote mh-put-msg-in-seq) "mh-seq" "\
Add MSG-OR-SEQ to SEQUENCE.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is added to the sequence.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence." t nil)

(autoload (quote mh-widen) "mh-seq" "\
Remove restrictions from current folder, thereby showing all messages." t nil)

(autoload (quote mh-rename-seq) "mh-seq" "\
Rename SEQUENCE to have NEW-NAME." t nil)

(autoload (quote mh-map-to-seq-msgs) "mh-seq" "\
Invoke the FUNC at each message in the SEQ.
SEQ can either be a list of messages or a MH sequence. The remaining ARGS are
passed as arguments to FUNC." nil nil)

(autoload (quote mh-notate-seq) "mh-seq" "\
Mark the scan listing.
All messages in SEQ are marked with NOTATION at OFFSET from the beginning of
the line." nil nil)

(autoload (quote mh-notate-cur) "mh-seq" "\
Mark the MH sequence cur.
In addition to notating the current message with `mh-note-cur' the function
uses `overlay-arrow-position' to put a marker in the fringe." nil nil)

(autoload (quote mh-add-to-sequence) "mh-seq" "\
The sequence SEQ is augmented with the messages in MSGS." nil nil)

(autoload (quote mh-iterate-on-messages-in-region) "mh-seq" "\
Iterate over region.
VAR is bound to the message on the current line as we loop starting from BEGIN
till END. In each step BODY is executed.

If VAR is nil then the loop is executed without any binding." nil (quote macro))

(autoload (quote mh-iterate-on-msg-or-seq) "mh-seq" "\
Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over MSG-OR-SEQ, which can be a
message number, a list of message numbers, a sequence, or a region in a cons
cell. In each iteration, BODY is executed.

The parameter MSG-OR-SEQ is usually created with `mh-interactive-msg-or-seq'
in order to provide a uniform interface to MH-E functions." nil (quote macro))

(autoload (quote mh-msg-or-seq-to-msg-list) "mh-seq" "\
Return a list of messages for MSG-OR-SEQ.
MSG-OR-SEQ can be a message number, a list of message numbers, a sequence, or
a region in a cons cell." nil nil)

(autoload (quote mh-interactive-msg-or-seq) "mh-seq" "\
Return interactive specification for message, sequence, or region.
By convention, the name of this argument is msg-or-seq.

If variable `transient-mark-mode' is non-nil and the mark is active, then this
function returns a cons-cell of the region.
If optional prefix argument provided, then prompt for message sequence with
SEQUENCE-PROMPT and return sequence.
Otherwise, the message number at point is returned.

This function is usually used with `mh-iterate-on-msg-or-seq' in order to
provide a uniform interface to MH-E functions." nil nil)

(autoload (quote mh-region-to-msg-list) "mh-seq" "\
Return a list of messages within the region between BEGIN and END." nil nil)

(autoload (quote mh-narrow-to-subject) "mh-seq" "\
Narrow to a sequence containing all following messages with same subject." t nil)

(autoload (quote mh-delete-subject) "mh-seq" "\
Mark all following messages with same subject to be deleted.
This puts the messages in a sequence named subject.  You can undo the last
deletion marks using `mh-undo' with a prefix argument and then specifying the
subject sequence." t nil)

(autoload (quote mh-delete-subject-or-thread) "mh-seq" "\
Mark messages for deletion intelligently.
If the folder is threaded then `mh-thread-delete' is used to mark the current
message and all its descendants for deletion. Otherwise `mh-delete-subject' is
used to mark the current message and all messages following it with the same
subject for deletion." t nil)

(autoload (quote mh-thread-inc) "mh-seq" "\
Update thread tree for FOLDER.
All messages after START-POINT are added to the thread tree." nil nil)

(autoload (quote mh-thread-add-spaces) "mh-seq" "\
Add COUNT spaces to each scan line in `mh-thread-scan-line-map'." nil nil)

(autoload (quote mh-toggle-threads) "mh-seq" "\
Toggle threaded view of folder." t nil)

(autoload (quote mh-thread-forget-message) "mh-seq" "\
Forget the message INDEX from the threading tables." nil nil)

(autoload (quote mh-thread-next-sibling) "mh-seq" "\
Jump to next sibling.
With non-nil optional argument PREVIOUS-FLAG jump to the previous sibling." t nil)

(autoload (quote mh-thread-previous-sibling) "mh-seq" "\
Jump to previous sibling." t nil)

(autoload (quote mh-thread-ancestor) "mh-seq" "\
Jump to the ancestor of current message.
If optional argument THREAD-ROOT-FLAG is non-nil then jump to the root of the
thread tree the message belongs to." t nil)

(autoload (quote mh-thread-delete) "mh-seq" "\
Mark current message and all its children for subsequent deletion." t nil)

(autoload (quote mh-thread-refile) "mh-seq" "\
Mark current message and all its children for refiling to FOLDER." t nil)

(autoload (quote mh-notate-tick) "mh-seq" "\
Highlight current line if MSG is in TICKED-MSGS.
If optional argument IGNORE-NARROWING is non-nil then highlighting is carried
out even if folder is narrowed to `mh-tick-seq'." nil nil)

(autoload (quote mh-toggle-tick) "mh-seq" "\
Toggle tick mark of all messages in region BEGIN to END." t nil)

(autoload (quote mh-narrow-to-tick) "mh-seq" "\
Restrict display of this folder to just messages in `mh-tick-seq'.
Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

;;;***

;;;### (autoloads (mh-speed-add-folder mh-speed-invalidate-map mh-speed-flists
;;;;;;  mh-speed-view mh-speed-toggle mh-folder-speedbar-buttons)
;;;;;;  "mh-speed" "mh-speed.el" (16040 52700))
;;; Generated autoloads from mh-speed.el

(autoload (quote mh-folder-speedbar-buttons) "mh-speed" "\
Interface function to create MH-E speedbar buffer.
BUFFER is the MH-E buffer for which the speedbar buffer is to be created." nil nil)

(defalias (quote mh-show-speedbar-buttons) (quote mh-folder-speedbar-buttons))

(defalias (quote mh-letter-speedbar-buttons) (quote mh-folder-speedbar-buttons))

(autoload (quote mh-speed-toggle) "mh-speed" "\
Toggle the display of child folders.
The otional ARGS are ignored and there for compatibilty with speedbar." t nil)

(autoload (quote mh-speed-view) "mh-speed" "\
View folder on current line.
Optional ARGS are ignored." t nil)

(autoload (quote mh-speed-flists) "mh-speed" "\
Execute flists -recurse and update message counts.
If FORCE is non-nil the timer is reset. If FOLDER is non-nil then flists is run
only for that one folder." t nil)

(autoload (quote mh-speed-invalidate-map) "mh-speed" "\
Remove FOLDER from various optimization caches." t nil)

(autoload (quote mh-speed-add-folder) "mh-speed" "\
Add FOLDER since it is being created.
The function invalidates the latest ancestor that is present." nil nil)

;;;***

;;;### (autoloads (mh-get-msg-num mh-goto-address-find-address-at-point)
;;;;;;  "mh-utils" "mh-utils.el" (16040 52700))
;;; Generated autoloads from mh-utils.el

(autoload (quote mh-goto-address-find-address-at-point) "mh-utils" "\
Find e-mail address around or before point.
Then search backwards to beginning of line for the start of an e-mail
address.  If no e-mail address found, return nil." nil nil)

(autoload (quote mh-get-msg-num) "mh-utils" "\
Return the message number of the displayed message.
If the argument ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is
not pointing to a message." nil nil)

;;;***

;;;### (autoloads (mh-alias-add-address-under-point mh-alias-grab-from-field
;;;;;;  mh-alias-add-alias mh-alias-from-has-no-alias-p mh-alias-address-to-alias
;;;;;;  mh-alias-letter-expand-alias mh-alias-minibuffer-confirm-address
;;;;;;  mh-read-address mh-alias-reload) "mh-alias" "mh-alias.el"
;;;;;;  (16040 52696))
;;; Generated autoloads from mh-alias.el

(autoload (quote mh-alias-reload) "mh-alias" "\
Load MH aliases into `mh-alias-alist'." t nil)

(autoload (quote mh-read-address) "mh-alias" "\
Read an address from the minibuffer with PROMPT." nil nil)

(autoload (quote mh-alias-minibuffer-confirm-address) "mh-alias" "\
Display the alias expansion if `mh-alias-flash-on-comma' is non-nil." t nil)

(autoload (quote mh-alias-letter-expand-alias) "mh-alias" "\
Expand mail alias before point." nil nil)

(autoload (quote mh-alias-address-to-alias) "mh-alias" "\
Return the ADDRESS alias if defined, or nil." nil nil)

(autoload (quote mh-alias-from-has-no-alias-p) "mh-alias" "\
Return t is From has no current alias set.
In the exceptional situation where there isn't a From header in the message the
function returns nil." nil nil)

(autoload (quote mh-alias-add-alias) "mh-alias" "\
*Add ALIAS for ADDRESS in personal alias file.
Prompts for confirmation if the address already has an alias.
If the alias is already is use, `mh-alias-add-alias-to-file' will prompt." t nil)

(autoload (quote mh-alias-grab-from-field) "mh-alias" "\
*Add ALIAS for ADDRESS in personal alias file.
Prompts for confirmation if the alias is already in use or if the address
already has an alias." t nil)

(autoload (quote mh-alias-add-address-under-point) "mh-alias" "\
Insert an alias for email address under point." t nil)

;;;***

(provide 'mh-loaddefs)
;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:

;;; arch-tag: bc36a104-1edb-45d5-8aad-a85b45648378
;;; mh-loaddefs.el ends here
