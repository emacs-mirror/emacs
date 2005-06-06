;;; mh-loaddefs.el --- automatically extracted autoloads
;;
;;; Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
;;; Author: Bill Wohler <wohler@newt.com>
;;; Keywords: mail
;;; Commentary:
;;; Change Log:
;;; Code:

;;;### (autoloads (mh-letter-previous-header-field mh-letter-next-header-field-or-indent
;;;;;;  mh-beginning-of-word mh-complete-word mh-open-line mh-fully-kill-draft
;;;;;;  mh-yank-cur-msg mh-insert-letter mh-send-letter mh-insert-auto-fields
;;;;;;  mh-check-whom mh-insert-signature mh-to-fcc mh-to-field mh-fill-paragraph-function
;;;;;;  mh-get-header-field mh-send-other-window mh-send mh-reply
;;;;;;  mh-redistribute mh-forward mh-extract-rejected-mail mh-edit-again)
;;;;;;  "mh-comp" "mh-comp.el" (17048 51103))
;;; Generated autoloads from mh-comp.el

(autoload (quote mh-edit-again) "mh-comp" "\
Clean up a draft or a message MSG previously sent and make it resendable.
Default is the current message.
The variable `mh-new-draft-cleaned-headers' specifies the headers to remove.

See also `mh-send'." t nil)

(autoload (quote mh-extract-rejected-mail) "mh-comp" "\
Extract message MSG returned by the mail system and make it resendable.
Default is the current message.  The variable `mh-new-draft-cleaned-headers'
gives the headers to clean out of the original message.

See also `mh-send'." t nil)

(autoload (quote mh-forward) "mh-comp" "\
Forward messages to the recipients TO and CC.
Use optional RANGE argument to specify a message or sequence to forward.
Default is the displayed message.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

See also `mh-send'." t nil)

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
If the file named by `mh-repl-formfile' exists, it is used as a skeleton for
the reply. If REPLY-TO is cc or all and you're using either the nmh or GNU
mailutils variants and the file names by `mh-repl-group-formfile' exists, it
is used instead.

See also `mh-send'." t nil)

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

(autoload (quote mh-get-header-field) "mh-comp" "\
Find and return the body of FIELD in the mail header.
Returns the empty string if the field is not in the header of the
current buffer." nil nil)

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
Insert the signature specified by `mh-signature-file-name' or FILE at point.
A signature separator (`-- ') will be added if the signature block does not
contain one and `mh-signature-separator-flag' is on.
The value of `mh-letter-insert-signature-hook' is a list of functions to be
called, with no arguments, after the signature is inserted.
The signature can also be inserted with `mh-identity-list'." t nil)

(autoload (quote mh-check-whom) "mh-comp" "\
Verify recipients of the current letter, showing expansion of any aliases." t nil)

(autoload (quote mh-insert-auto-fields) "mh-comp" "\
Insert custom fields if To or Cc match `mh-auto-fields-list'.
Sets buffer-local `mh-insert-auto-fields-done-local' when done and inserted
something.  If NON-INTERACTIVE is non-nil, do not be verbose and only
attempt matches if `mh-insert-auto-fields-done-local' is nil.

An `identity' entry is skipped if one was already entered manually.

Return t if fields added; otherwise return nil." t nil)

(autoload (quote mh-send-letter) "mh-comp" "\
Send the draft letter in the current buffer.
If optional prefix argument ARG is provided, monitor delivery.
The value of `mh-before-send-letter-hook' is a list of functions to be called,
with no arguments, before doing anything.
Run `\\[mh-edit-mhn]' if mhn directives are present; otherwise
run `\\[mh-mml-to-mime]' if mml directives are present." t nil)

(autoload (quote mh-insert-letter) "mh-comp" "\
Insert a message into the current letter.
Removes the header fields according to the variable
`mh-invisible-header-fields-compiled'.
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

(autoload (quote mh-complete-word) "mh-comp" "\
Complete WORD at from CHOICES.
Any match found replaces the text from BEGIN to END." nil nil)

(autoload (quote mh-beginning-of-word) "mh-comp" "\
Return position of the N th word backwards." nil nil)

(autoload (quote mh-letter-next-header-field-or-indent) "mh-comp" "\
Move to next field or indent depending on point.
In the message header, go to the next field. Elsewhere call
`indent-relative' as usual with optional prefix ARG." t nil)

(autoload (quote mh-letter-previous-header-field) "mh-comp" "\
Cycle to the previous header field.
If we are at the first header field go to the start of the message body." t nil)

;;;***

;;;### (autoloads (mh-prefix-help mh-help mh-ephem-message mh-store-buffer
;;;;;;  mh-store-msg mh-undo-folder mh-sort-folder mh-page-digest-backwards
;;;;;;  mh-page-digest mh-pipe-msg mh-pack-folder mh-list-folders
;;;;;;  mh-kill-folder mh-copy-msg mh-burst-digest) "mh-funcs" "mh-funcs.el"
;;;;;;  (17048 47864))
;;; Generated autoloads from mh-funcs.el

(autoload (quote mh-burst-digest) "mh-funcs" "\
Burst apart the current message, which should be a digest.
The message is replaced by its table of contents and the messages from the
digest are inserted into the folder after that message." t nil)

(autoload (quote mh-copy-msg) "mh-funcs" "\
Copy the specified RANGE to another FOLDER without deleting them.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use." t nil)

(autoload (quote mh-kill-folder) "mh-funcs" "\
Remove the current folder and all included messages.
Removes all of the messages (files) within the specified current folder,
and then removes the folder (directory) itself.
The value of `mh-kill-folder-suppress-prompt-hook' is a list of functions to
be called, with no arguments, which should return a value of non-nil if
verification is not desired." t nil)

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

(autoload (quote mh-sort-folder) "mh-funcs" "\
Sort the messages in the current folder by date.
Calls the MH program sortm to do the work.
The arguments in the list `mh-sortm-args' are passed to sortm if the optional
argument EXTRA-ARGS is given." t nil)

(autoload (quote mh-undo-folder) "mh-funcs" "\
Undo all pending deletes and refiles in current folder." t nil)

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
Display cheat sheet for the MH-E commands." t nil)

(autoload (quote mh-prefix-help) "mh-funcs" "\
Display cheat sheet for the commands of the current prefix in minibuffer." t nil)

;;;***

;;;### (autoloads (mh-identity-handler-bottom mh-identity-handler-top
;;;;;;  mh-identity-insert-attribution-verb mh-identity-handler-attribution-verb
;;;;;;  mh-identity-handler-signature mh-identity-handler-gpg-identity
;;;;;;  mh-insert-identity mh-identity-list-set mh-identity-make-menu)
;;;;;;  "mh-identity" "mh-identity.el" (17044 63778))
;;; Generated autoloads from mh-identity.el

(autoload (quote mh-identity-make-menu) "mh-identity" "\
Build the Identity menu.
This should be called any time `mh-identity-list' or `mh-auto-fields-list'
change." nil nil)

(autoload (quote mh-identity-list-set) "mh-identity" "\
Update the `mh-identity-list' variable, and rebuild the menu.
Sets the default for SYMBOL (for example, `mh-identity-list') to VALUE (as set
in customization). This is called after 'customize is used to alter
`mh-identity-list'." nil nil)

(autoload (quote mh-insert-identity) "mh-identity" "\
Insert fields specified by given IDENTITY.
See `mh-identity-list'." t nil)

(autoload (quote mh-identity-handler-gpg-identity) "mh-identity" "\
Process header FIELD \":pgg-default-user-id\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
The buffer-local variable `mh-identity-pgg-default-user-id' is set to VALUE
when action 'add is selected." nil nil)

(autoload (quote mh-identity-handler-signature) "mh-identity" "\
Process header FIELD \":signature\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added." nil nil)

(autoload (quote mh-identity-handler-attribution-verb) "mh-identity" "\
Process header FIELD \":attribution-verb\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added." nil nil)

(autoload (quote mh-identity-insert-attribution-verb) "mh-identity" "\
Insert VALUE as attribution verb, setting up delimiting markers.
If VALUE is nil, use `mh-extract-from-attribution-verb'." nil nil)

(autoload (quote mh-identity-handler-top) "mh-identity" "\
Process header FIELD.
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
If the field wasn't present, it is added to the top of the header." nil nil)

(autoload (quote mh-identity-handler-bottom) "mh-identity" "\
Process header FIELD.
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
If the field wasn't present, it is added to the bottom of the header." nil nil)

;;;***

;;;### (autoloads (mh-inc-spool-list-set) "mh-inc" "mh-inc.el" (17048
;;;;;;  44143))
;;; Generated autoloads from mh-inc.el

(autoload (quote mh-inc-spool-list-set) "mh-inc" "\
Set-default SYMBOL to VALUE to update the `mh-inc-spool-list' variable.
Also rebuilds the user commands.
This is called after 'customize is used to alter `mh-inc-spool-list'." nil nil)

;;;***

;;;### (autoloads (mh-index-choose mh-namazu-execute-search mh-swish++-execute-search
;;;;;;  mh-swish-execute-search mh-index-ticked-messages mh-index-new-messages
;;;;;;  mh-index-sequenced-messages mh-index-delete-from-sequence
;;;;;;  mh-index-add-to-sequence mh-index-execute-commands mh-index-visit-folder
;;;;;;  mh-index-delete-folder-headers mh-index-group-by-folder mh-index-create-imenu-index
;;;;;;  mh-index-insert-folder-headers mh-index-previous-folder mh-index-next-folder
;;;;;;  mh-index-parse-search-regexp mh-index-do-search mh-index-p
;;;;;;  mh-index-read-data mh-index-search mh-index-create-sequences
;;;;;;  mh-create-sequence-map mh-index-update-maps) "mh-index" "mh-index.el"
;;;;;;  (17044 64025))
;;; Generated autoloads from mh-index.el

(autoload (quote mh-index-update-maps) "mh-index" "\
Annotate all as yet unannotated messages in FOLDER with their MD5 hash.
As a side effect msg -> checksum map is updated. Optional argument ORIGIN-MAP
is a hashtable which maps each message in the index folder to the original
folder and message from whence it was copied. If present the
checksum -> (origin-folder, origin-index) map is updated too." nil nil)

(autoload (quote mh-create-sequence-map) "mh-index" "\
Return a map from msg number to list of sequences in which it is present.
SEQ-LIST is an assoc list whose keys are sequence names and whose cdr is the
list of messages in that sequence." nil nil)

(autoload (quote mh-index-create-sequences) "mh-index" "\
Mirror sequences present in source folders in index folder." nil nil)

(autoload (quote mh-index-search) "mh-index" "\
Perform an indexed search in an MH mail folder.
Use a prefix argument to repeat the search.

Unlike regular searches, the prompt for the folder to search can be `all' to
search all folders; in addition, the search works recursively on the listed
folder. The search criteria are entered in an MH-Pick buffer as described in
`mh-search-folder'.

To perform the search, type \\<mh-pick-mode-map>\\[mh-do-search]. Another
difference from the regular searches is that because the search operates on
more than one folder, the messages that are found are put in a temporary
sub-folder of `+mhe-index' and are displayed in an MH-Folder buffer. This
buffer is special because it displays messages from multiple folders; each set
of messages from a given folder has a heading with the folder name.

In addition, the \\<mh-folder-mode-map>\\[mh-index-visit-folder] command can
be used to visit the folder of the message at point. Initially, only the
messages that matched the search criteria are displayed in the folder. While
the temporary buffer has its own set of message numbers, the actual messages
numbers are shown in the visited folder. Thus, the \\[mh-index-visit-folder]
command is useful to find the actual message number of an interesting message,
or to view surrounding messages with the \\[mh-rescan-folder] command.

Because this folder is temporary, you'll probably get in the habit of killing
it when you're done with \\[mh-kill-folder].

If you have run the \\[mh-search-folder] command, but change your mind while
entering the search criteria and actually want to run an indexed search, then
you can use the \\<mh-pick-mode-map>\\[mh-index-do-search] command in the
MH-Pick buffer.

The \\<mh-folder-mode-map>\\[mh-index-search] command runs the command defined
by the `mh-index-program' option. The default value is \"Auto-detect\" which
means that MH-E will automatically choose one of \"swish++\", \"swish-e\",
\"mairix\", \"namazu\", \"pick\" and \"grep\" in that order. If, for example,
you have both \"swish++\" and \"mairix\" installed and you want to use
\"mairix\", then you can set this option to \"mairix\".

                                *NOTE*

     The \"pick\" and \"grep\" commands do not perform a recursive search on
     the given folder.

This command uses an \"X-MHE-Checksum:\" header field to cache the MD5
checksum of a message. This means that if an incoming message already contains
an \"X-MHE-Checksum:\" field, that message might not be found by this command.
The following \"procmail\" recipe avoids this problem by renaming the existing
header field:

     :0 wf
     | formail -R \"X-MHE-Checksum\" \"X-Old-MHE-Checksum\"

The documentation for the following commands describe how to set up the
various indexing programs to use with MH-E. The \"pick\" and \"grep\" commands
do not require additional configuration.

    - `mh-swish++-execute-search'
    - `mh-swish-execute-search'
    - `mh-mairix-execute-search'
    - `mh-namazu-execute-search'
    - `mh-pick-execute-search'
    - `mh-grep-execute-search'

In a program, if REDO-SEARCH-FLAG is non-nil and the current folder buffer was
generated by a index search, then the search is repeated. Otherwise, FOLDER is
searched with SEARCH-REGEXP and the results are presented in an MH-E folder.
If FOLDER is \"+\" then mail in all folders are searched. Optional argument
WINDOW-CONFIG stores the window configuration that will be restored after the
user quits the folder containing the index search results." t nil)

(autoload (quote mh-index-read-data) "mh-index" "\
Read index data from file." nil nil)

(autoload (quote mh-index-p) "mh-index" "\
Non-nil means that this folder was generated by an index search." nil nil)

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

(autoload (quote mh-index-create-imenu-index) "mh-index" "\
Create alist of folder names and positions in index folder buffers." nil nil)

(autoload (quote mh-index-group-by-folder) "mh-index" "\
Partition the messages based on source folder.
Returns an alist with the the folder names in the car and the cdr being the
list of messages originally from that folder." nil nil)

(autoload (quote mh-index-delete-folder-headers) "mh-index" "\
Delete the folder headers." nil nil)

(autoload (quote mh-index-visit-folder) "mh-index" "\
Visit original folder from where the message at point was found." t nil)

(autoload (quote mh-index-execute-commands) "mh-index" "\
Delete/refile the actual messages.
The copies in the searched folder are then deleted/refiled to get the desired
result. Before deleting the messages we make sure that the message being
deleted is identical to the one that the user has marked in the index buffer." nil nil)

(autoload (quote mh-index-add-to-sequence) "mh-index" "\
Add to SEQ the messages in the list MSGS.
This function updates the source folder sequences. Also makes an attempt to
update the source folder buffer if we have it open." nil nil)

(autoload (quote mh-index-delete-from-sequence) "mh-index" "\
Delete from SEQ the messages in MSGS.
This function updates the source folder sequences. Also makes an attempt to
update the source folder buffer if present." nil nil)

(autoload (quote mh-index-sequenced-messages) "mh-index" "\
Display messages from FOLDERS in SEQUENCE.
All messages in the sequence you provide from the folders in
`mh-index-new-messages-folders' are listed. With a prefix argument, enter a
space-separated list of folders, or nothing to search all folders." t nil)

(autoload (quote mh-index-new-messages) "mh-index" "\
Display unseen messages.
If you use a program such as `procmail' to use `rcvstore' to file your
incoming mail automatically, you can display new, unseen, messages using this
command. All messages in the `unseen' sequence from the folders in
`mh-index-new-messages-folders' are listed. With a prefix argument, enter a
space-separated list of FOLDERS, or nothing to search all folders." t nil)

(autoload (quote mh-index-ticked-messages) "mh-index" "\
Display ticked messages.
All messages in `mh-tick-seq' from the folders in
`mh-index-ticked-messages-folders' are listed. With a prefix argument, enter a
space-separated list of FOLDERS, or nothing to search all folders." t nil)

(autoload (quote mh-swish-execute-search) "mh-index" "\
Execute swish-e and read the results.

In the examples below, replace \"/home/user/Mail\" with the path to your
MH directory.

First create the directory \"/home/user/Mail/.swish\". Then create the file
\"/home/user/Mail/.swish/config\" with the following contents:

     DefaultContents TXT*
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
     FileRules filename contains \\D
     FileRules pathname contains /home/user/Mail/.swish
     FileRules pathname contains /home/user/Mail/mhe-index

This configuration does not index the folders that hold the results of your
searches in \"+mhe-index\" since they tend to be ephemeral and the original
messages are indexed anyway.

If there are any directories you would like to ignore, append lines like the
following to \"config\":

     FileRules pathname contains /home/user/Mail/scripts

Use the following command line to generate the swish index. Run this daily
from cron:

         swish-e -c /home/user/Mail/.swish/config

In a program, FOLDER-PATH is the directory in which SEARCH-REGEXP is used to
search." nil nil)

(autoload (quote mh-swish++-execute-search) "mh-index" "\
Execute swish++ and read the results.

In the examples below, replace \"/home/user/Mail\" with the path to your MH
directory.

First create the directory \"/home/user/Mail/.swish++\". Then create the file
\"/home/user/Mail/.swish++/swish++.conf\" with the following contents:

     IncludeMeta         Bcc Cc Comments Content-Description From Keywords
     IncludeMeta         Newsgroups Resent-To Subject To
     IncludeMeta         Message-Id References In-Reply-To
     IncludeFile         Mail    *
     IndexFile           /home/user/Mail/.swish++/swish++.index

Use the following command line to generate the swish index. Run this daily
from cron:

     find /home/user/Mail -path /home/user/Mail/mhe-index -prune \\
                          -o -path /home/user/Mail/.swish++ -prune \\
                          -o -name \"[0-9]*\" -print \\
         | index -c /home/user/Mail/.swish++/swish++.conf -

This command does not index the folders that hold the results of your searches
in \"+mhe-index\" since they tend to be ephemeral and the original messages
are indexed anyway.

On some systems (Debian GNU/Linux, for example), use \"index++\" instead of
\"index\".

In a program, FOLDER-PATH is the directory in which SEARCH-REGEXP is used to
search." nil nil)

(autoload (quote mh-namazu-execute-search) "mh-index" "\
Execute namazu and read the results.

In the examples below, replace \"/home/user/Mail\" with the path to your MH
directory.

First create the directory \"/home/user/Mail/.namazu\". Then create the file
\"/home/user/Mail/.namazu/mknmzrc\" with the following contents:

     package conf;  # Don't remove this line!
     $ADDRESS = 'user@localhost';
     $ALLOW_FILE = \"[0-9]*\";
     $EXCLUDE_PATH = \"^/home/user/Mail/(mhe-index|spam)\";

This configuration does not index the folders that hold the results of your
searches in \"+mhe-index\" since they tend to be ephemeral and the original
messages are indexed anyway.

Use the following command line to generate the namazu index. Run this daily
from cron:

     mknmz -f /home/user/Mail/.namazu/mknmzrc -O /home/user/Mail/.namazu \\
              /home/user/Mail

In a program, FOLDER-PATH is the directory in which SEARCH-REGEXP is used to
search." nil nil)

(autoload (quote mh-index-choose) "mh-index" "\
Choose an indexing function.
The side-effects of this function are that the variables `mh-indexer',
`mh-index-execute-search-function', and `mh-index-next-result-function' are
set according to the first indexer in `mh-indexer-choices' present on the
system." nil nil)

;;;***

;;;### (autoloads (mh-variants mh-variant-p mh-variant-set) "mh-init"
;;;;;;  "mh-init.el" (17044 64253))
;;; Generated autoloads from mh-init.el

(autoload (quote mh-variant-set) "mh-init" "\
Set the MH variant to VARIANT.
Sets `mh-progs', `mh-lib', `mh-lib-progs' and `mh-flists-present-flag'.
If the VARIANT is `autodetect', then first try nmh, then MH and finally
GNU mailutils." t nil)

(autoload (quote mh-variant-p) "mh-init" "\
Return t if variant is any of VARIANTS.
Currently known variants are 'MH, 'nmh, and 'mu-mh." nil nil)

(autoload (quote mh-variants) "mh-init" "\
Return a list of installed variants of MH on the system.
This function looks for MH in `mh-sys-path', `mh-path' and
`exec-path'. The format of the list of variants that is returned is described
by the variable `mh-variants'." nil nil)

;;;***

;;;### (autoloads (mh-junk-whitelist mh-junk-blacklist) "mh-junk"
;;;;;;  "mh-junk.el" (17044 64253))
;;; Generated autoloads from mh-junk.el

(autoload (quote mh-junk-blacklist) "mh-junk" "\
Blacklist RANGE as spam.

This command trains the spam program in use (see the `mh-junk-program' option)
with the content of the range (see `mh-interactive-range') and then handles
the message(s) as specified by the `mh-junk-disposition' option.

For more information about using your particular spam fighting program, see:

  - `mh-spamassassin-blacklist'
  - `mh-bogofilter-blacklist'
  - `mh-spamprobe-blacklist'" t nil)

(autoload (quote mh-junk-whitelist) "mh-junk" "\
Whitelist RANGE as ham.

This command reclassifies a range of messages (see `mh-interactive-range') as
ham if it were incorrectly classified as spam. It then refiles the message
into the `+inbox' folder.

The `mh-junk-program' option specifies the spam program in use." t nil)

;;;***

;;;### (autoloads (mh-display-with-external-viewer mh-mime-inline-part
;;;;;;  mh-mime-save-part mh-push-button mh-press-button mh-mime-display
;;;;;;  mh-decode-message-header mh-toggle-mh-decode-mime-flag mh-mime-save-parts
;;;;;;  mh-display-emphasis mh-display-smileys mh-add-missing-mime-version-header
;;;;;;  mh-destroy-postponed-handles mh-mime-cleanup mh-mml-directive-present-p
;;;;;;  mh-mml-secure-message-signencrypt mh-mml-secure-message-encrypt
;;;;;;  mh-mml-secure-message-sign mh-mml-unsecure-message mh-mml-attach-file
;;;;;;  mh-mml-query-cryptographic-method mh-mml-forward-message
;;;;;;  mh-mml-to-mime mh-mhn-directive-present-p mh-revert-mhn-edit
;;;;;;  mh-edit-mhn mh-mhn-compose-forw mh-mhn-compose-external-type
;;;;;;  mh-mhn-compose-external-compressed-tar mh-mhn-compose-anon-ftp
;;;;;;  mh-mhn-compose-insertion mh-file-mime-type mh-have-file-command
;;;;;;  mh-compose-forward mh-compose-insertion) "mh-mime" "mh-mime.el"
;;;;;;  (17048 47895))
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

(autoload (quote mh-have-file-command) "mh-mime" "\
Return t if 'file' command is on the system.
'file -i' is used to get MIME type of composition insertion." nil nil)

(autoload (quote mh-file-mime-type) "mh-mime" "\
Return MIME type of FILENAME from file command.
Returns nil if file command not on system." nil nil)

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

(autoload (quote mh-mhn-compose-external-type) "mh-mime" "\
Add a directive to include a MIME reference to a remote file.
The file should be available via anonymous ftp. This directive tells MH to
include a reference to a message/external-body part.

Arguments are ACCESS-TYPE, HOST and FILENAME, which tell where to find the
file and TYPE which is the MIME Content-Type. Optional arguments include
DESCRIPTION, a line of text for the Content-description header, ATTRIBUTES,
EXTRA-PARAMS, and COMMENT.

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
Check if the text between BEGIN and END might be a MHN directive.
The optional argument BEGIN defaults to the beginning of the buffer, while END
defaults to the the end of the buffer." nil nil)

(autoload (quote mh-mml-to-mime) "mh-mime" "\
Compose MIME message from mml directives.
This step is performed automatically when sending the message, but this
function may be called manually before sending the draft as well." t nil)

(autoload (quote mh-mml-forward-message) "mh-mime" "\
Forward a message as attachment.
The function will prompt the user for a DESCRIPTION, a FOLDER and MESSAGE
number." nil nil)

(autoload (quote mh-mml-query-cryptographic-method) "mh-mime" "\
Read the cryptographic method to use." nil nil)

(autoload (quote mh-mml-attach-file) "mh-mime" "\
Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[mh-send-letter]'.
Message disposition is \"inline\" or \"attachment\" and is prompted for if
DISPOSITION is nil.

This is basically `mml-attach-file' from gnus, modified such that a prefix
argument yields an `inline' disposition and Content-Type is determined
automatically." nil nil)

(autoload (quote mh-mml-unsecure-message) "mh-mime" "\
Remove any secure message directives.
The IGNORE argument is not used." t nil)

(autoload (quote mh-mml-secure-message-sign) "mh-mime" "\
Add security directive to sign the entire message using METHOD." t nil)

(autoload (quote mh-mml-secure-message-encrypt) "mh-mime" "\
Add security directive to encrypt the entire message using METHOD." t nil)

(autoload (quote mh-mml-secure-message-signencrypt) "mh-mime" "\
Add security directive to encrypt and sign the entire message using METHOD." t nil)

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

(autoload (quote mh-toggle-mh-decode-mime-flag) "mh-mime" "\
Toggle whether MH-E should decode MIME or not." t nil)

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

(autoload (quote mh-display-with-external-viewer) "mh-mime" "\
View MIME PART-INDEX externally." t nil)

;;;***

;;;### (autoloads (mh-do-search mh-pick-do-search mh-search-folder)
;;;;;;  "mh-pick" "mh-pick.el" (17048 47905))
;;; Generated autoloads from mh-pick.el

(autoload (quote mh-search-folder) "mh-pick" "\
Search FOLDER for messages matching a pattern.

With this command, you can search a folder for messages to or from a
particular person or about a particular subject. In fact, you can also search
for messages containing selected strings in any arbitrary header field or any
string found within the messages.

You are first prompted for the name of the folder to search and then placed in
the following buffer in MH-Pick mode:

     From:
     To:
     Cc:
     Date:
     Subject:
     --------

Edit this template by entering your search criteria in an appropriate header
field that is already there, or create a new field yourself. If the string
you're looking for could be anywhere in a message, then place the string
underneath the row of dashes. The \\[mh-search-folder] command uses the MH
command \"pick\" to do the real work.

There are no semantics associated with the search criteria--they are simply
treated as strings. Case is ignored when all lowercase is used, and regular
expressions (a la \"ed\") are available. It is all right to specify several
search criteria. What happens then is that a logical _and_ of the various
fields is performed. If you prefer a logical _or_ operation, run
\\[mh-search-folder] multiple times.

As an example, let's say that we want to find messages from Ginnean about
horseback riding in the Kosciusko National Park (Australia) during January,
1994. Normally we would start with a broad search and narrow it down if
necessary to produce a manageable amount of data, but we'll cut to the chase
and create a fairly restrictive set of criteria as follows:

     From: ginnean
     To:
     Cc:
     Date: Jan 1994
     Subject: horse.*kosciusko
     --------

As with MH-Letter mode, MH-Pick provides commands like
\\<mh-pick-mode-map>\\[mh-to-field] to help you fill in the blanks.

To perform the search, type \\[mh-do-search]. The selected messages are placed
in the \"search\" sequence, which you can use later in forwarding, printing,
or narrowing your field of view. Subsequent searches are appended to the
\"search\" sequence. If, however, you wish to start with a clean slate, first
delete the \"search\" sequence.

If you're searching in a folder that is already displayed in an MH-Folder
buffer, only those messages contained in the buffer are used for the search.
Therefore, if you want to search in all messages, first kill the folder's
buffer with \\<mh-folder-mode-map>\\[kill-buffer] or scan the entire folder
with \\[mh-rescan-folder].

If you find that you do the same thing over and over when editing the search
template, you may wish to bind some shortcuts to keys. This can be done with
the variable `mh-pick-mode-hook', which is called when \\[mh-search-folder] is
run on a new pattern.

If you have run the \\[mh-index-search] command, but change your mind while
entering the search criteria and actually want to run a regular search, then
you can use the \\<mh-pick-mode-map>\\[mh-pick-do-search] command.

In a program, argument WINDOW-CONFIG is the current window configuration and
is used when the search folder is dismissed." t nil)

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

;;;### (autoloads (mh-print-msg mh-ps-print-toggle-mime mh-ps-print-toggle-color
;;;;;;  mh-ps-print-toggle-faces mh-ps-print-msg-show mh-ps-print-msg-file
;;;;;;  mh-ps-print-msg) "mh-print" "mh-print.el" (17044 64253))
;;; Generated autoloads from mh-print.el

(autoload (quote mh-ps-print-msg) "mh-print" "\
Print the messages in RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use." t nil)

(autoload (quote mh-ps-print-msg-file) "mh-print" "\
Print to FILE the messages in RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use." t nil)

(autoload (quote mh-ps-print-msg-show) "mh-print" "\
Print current show buffer to FILE." t nil)

(autoload (quote mh-ps-print-toggle-faces) "mh-print" "\
Toggle whether printing is done with faces or not." t nil)

(autoload (quote mh-ps-print-toggle-color) "mh-print" "\
Toggle whether color is used in printing messages." t nil)

(autoload (quote mh-ps-print-toggle-mime) "mh-print" "\
Cycle through available choices on how MIME parts should be printed.
The available settings are:
  1. Print only inline MIME parts.
  2. Print all MIME parts.
  3. Print no MIME parts." t nil)

(autoload (quote mh-print-msg) "mh-print" "\
Print RANGE on printer.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

The variable `mh-lpr-command-format' is used to generate the print command.
The messages are formatted by mhl. See the variable `mhl-formfile'." t nil)

;;;***

;;;### (autoloads (mh-narrow-to-tick mh-toggle-tick mh-thread-refile
;;;;;;  mh-thread-delete mh-thread-ancestor mh-thread-previous-sibling
;;;;;;  mh-thread-next-sibling mh-thread-forget-message mh-toggle-threads
;;;;;;  mh-thread-add-spaces mh-thread-update-scan-line-map mh-thread-inc
;;;;;;  mh-delete-subject-or-thread mh-delete-subject mh-narrow-to-range
;;;;;;  mh-narrow-to-to mh-narrow-to-cc mh-narrow-to-from mh-narrow-to-subject
;;;;;;  mh-interactive-range mh-range-to-msg-list mh-iterate-on-range
;;;;;;  mh-iterate-on-messages-in-region mh-add-to-sequence mh-notate-cur
;;;;;;  mh-rename-seq mh-translate-range mh-read-range mh-read-seq-default
;;;;;;  mh-notate-deleted-and-refiled mh-widen mh-put-msg-in-seq
;;;;;;  mh-narrow-to-seq mh-msg-is-in-seq mh-list-sequences mh-delete-seq)
;;;;;;  "mh-seq" "mh-seq.el" (17048 47921))
;;; Generated autoloads from mh-seq.el

(autoload (quote mh-delete-seq) "mh-seq" "\
Delete the SEQUENCE." t nil)

(autoload (quote mh-list-sequences) "mh-seq" "\
List the sequences defined in the folder being visited." t nil)

(autoload (quote mh-msg-is-in-seq) "mh-seq" "\
Display the sequences in which the current message appears.
Use a prefix argument to display the sequences in which another MESSAGE
appears." t nil)

(autoload (quote mh-narrow-to-seq) "mh-seq" "\
Restrict display of this folder to just messages in SEQUENCE.
Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

(autoload (quote mh-put-msg-in-seq) "mh-seq" "\
Add RANGE to SEQUENCE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use." t nil)

(autoload (quote mh-widen) "mh-seq" "\
Restore the previous limit.
If optional prefix argument ALL-FLAG is non-nil, remove all limits." t nil)

(autoload (quote mh-notate-deleted-and-refiled) "mh-seq" "\
Notate messages marked for deletion or refiling.
Messages to be deleted are given by `mh-delete-list' while messages to be
refiled are present in `mh-refile-list'." nil nil)

(autoload (quote mh-read-seq-default) "mh-seq" "\
Read and return sequence name with default narrowed or previous sequence.
PROMPT is the prompt to use when reading. If NOT-EMPTY is non-nil then a
non-empty sequence is read." nil nil)

(autoload (quote mh-read-range) "mh-seq" "\
Read a message range with PROMPT.

If FOLDER is non-nil then a range is read from that folder, otherwise use
`mh-current-folder'.

If DEFAULT is a string then use that as default range to return. If DEFAULT is
nil then ask user with default answer a range based on the sequences that seem
relevant. Finally if DEFAULT is t, try to avoid prompting the user. Unseen
messages, if present, are returned. If the folder has fewer than
`mh-large-folder' messages then \"all\" messages are returned. Finally as a
last resort prompt the user.

If EXPAND-FLAG is non-nil then a list of message numbers corresponding to the
input is returned. If this list is empty then an error is raised. If
EXPAND-FLAG is nil just return the input string. In this case we don't check
if the range is empty.

If ASK-FLAG is non-nil, then the user is always queried for a range of
messages. If ASK-FLAG is nil, then the function checks if the unseen sequence
is non-empty. If that is the case, `mh-unseen-seq', or the list of messages in
it depending on the value of EXPAND, is returned. Otherwise if the folder has
fewer than `mh-large-folder' messages then the list of messages corresponding
to \"all\" is returned. If neither of the above holds then as a last resort
the user is queried for a range of messages.

If NUMBER-AS-RANGE-FLAG is non-nil, then if a number, N is read as input, it
is interpreted as the range \"last:N\".

This function replaces the existing function `mh-read-msg-range'. Calls to:
  (mh-read-msg-range folder flag)
should be replaced with:
  (mh-read-range \"Suitable prompt\" folder t nil flag
                 mh-interpret-number-as-range-flag)" nil nil)

(autoload (quote mh-translate-range) "mh-seq" "\
In FOLDER, translate the string EXPR to a list of messages numbers." nil nil)

(autoload (quote mh-rename-seq) "mh-seq" "\
Rename SEQUENCE to have NEW-NAME." t nil)

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

(autoload (quote mh-iterate-on-range) "mh-seq" "\
Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over RANGE, which can be a
message number, a list of message numbers, a sequence, a region in a cons
cell, or a MH range (something like last:20) in a string. In each iteration,
BODY is executed.

The parameter RANGE is usually created with `mh-interactive-range'
in order to provide a uniform interface to MH-E functions." nil (quote macro))

(autoload (quote mh-range-to-msg-list) "mh-seq" "\
Return a list of messages for RANGE.
RANGE can be a message number, a list of message numbers, a sequence, or
a region in a cons cell." nil nil)

(autoload (quote mh-interactive-range) "mh-seq" "\
Return interactive specification for message, sequence, range or region.
By convention, the name of this argument is RANGE.

If variable `transient-mark-mode' is non-nil and the mark is active, then this
function returns a cons-cell of the region.

If optional prefix argument is provided, then prompt for message range with
RANGE-PROMPT. A list of messages in that range is returned.

If a MH range is given, say something like last:20, then a list containing
the messages in that range is returned.

If DEFAULT non-nil then it is returned.

Otherwise, the message number at point is returned.

This function is usually used with `mh-iterate-on-range' in order to provide
a uniform interface to MH-E functions." nil nil)

(autoload (quote mh-narrow-to-subject) "mh-seq" "\
Limit to messages with same subject.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

(autoload (quote mh-narrow-to-from) "mh-seq" "\
Limit to messages with the same `From:' field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

(autoload (quote mh-narrow-to-cc) "mh-seq" "\
Limit to messages with the same `Cc:' field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

(autoload (quote mh-narrow-to-to) "mh-seq" "\
Limit to messages with the same `To:' field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

(autoload (quote mh-narrow-to-range) "mh-seq" "\
Limit to messages in RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

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

(autoload (quote mh-thread-update-scan-line-map) "mh-seq" "\
In threaded view update `mh-thread-scan-line-map'.
MSG is the message being notated with NOTATION at OFFSET." nil nil)

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

(autoload (quote mh-toggle-tick) "mh-seq" "\
Toggle tick mark of all messages in RANGE." t nil)

(autoload (quote mh-narrow-to-tick) "mh-seq" "\
Limit to messages in `mh-tick-seq'.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command." t nil)

;;;***

;;;### (autoloads (mh-speed-add-folder mh-speed-invalidate-map mh-speed-flists
;;;;;;  mh-speed-view mh-speed-toggle mh-folder-speedbar-buttons)
;;;;;;  "mh-speed" "mh-speed.el" (17044 64253))
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
If FORCE is non-nil the timer is reset.

Any number of optional FOLDERS can be specified. If specified, flists is run
only for that one folder." t nil)

(autoload (quote mh-speed-invalidate-map) "mh-speed" "\
Remove FOLDER from various optimization caches." t nil)

(autoload (quote mh-speed-add-folder) "mh-speed" "\
Add FOLDER since it is being created.
The function invalidates the latest ancestor that is present." nil nil)

;;;***

;;;### (autoloads (mh-alias-apropos mh-alias-add-address-under-point
;;;;;;  mh-alias-grab-from-field mh-alias-add-alias mh-alias-for-from-p
;;;;;;  mh-alias-address-to-alias mh-alias-letter-expand-alias mh-alias-minibuffer-confirm-address
;;;;;;  mh-read-address mh-alias-reload-maybe mh-alias-reload) "mh-alias"
;;;;;;  "mh-alias.el" (17048 47789))
;;; Generated autoloads from mh-alias.el

(autoload (quote mh-alias-reload) "mh-alias" "\
Reload MH aliases.

Since aliases are updated frequently, MH-E will reload aliases automatically
whenever an alias lookup occurs if an alias source (a file listed in your
`Aliasfile:' profile component and your password file if variable
`mh-alias-local-users' is non-nil) has changed. However, you can reload your
aliases manually by calling this command directly.

The value of `mh-alias-reloaded-hook' is a list of functions to be called,
with no arguments, after the aliases have been loaded." t nil)

(autoload (quote mh-alias-reload-maybe) "mh-alias" "\
Load new MH aliases." nil nil)

(autoload (quote mh-read-address) "mh-alias" "\
Read an address from the minibuffer with PROMPT." nil nil)

(autoload (quote mh-alias-minibuffer-confirm-address) "mh-alias" "\
Display the alias expansion if `mh-alias-flash-on-comma' is non-nil." t nil)

(autoload (quote mh-alias-letter-expand-alias) "mh-alias" "\
Expand mail alias before point." nil nil)

(autoload (quote mh-alias-address-to-alias) "mh-alias" "\
Return the ADDRESS alias if defined, or nil." nil nil)

(autoload (quote mh-alias-for-from-p) "mh-alias" "\
Return t if sender's address has a corresponding alias." nil nil)

(autoload (quote mh-alias-add-alias) "mh-alias" "\
*Add ALIAS for ADDRESS in personal alias file.
This function prompts you for an alias and address. If the alias exists
already, you will have the choice of inserting the new alias before or after
the old alias. In the former case, this alias will be used when sending mail
to this alias. In the latter case, the alias serves as an additional folder
name hint when filing messages." t nil)

(autoload (quote mh-alias-grab-from-field) "mh-alias" "\
*Add alias for the sender of the current message." t nil)

(autoload (quote mh-alias-add-address-under-point) "mh-alias" "\
Insert an alias for address under point." t nil)

(autoload (quote mh-alias-apropos) "mh-alias" "\
Show all aliases or addresses that match REGEXP." t nil)

;;;***

(provide 'mh-loaddefs)
;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; arch-tag: bc36a104-1edb-45d5-8aad-a85b45648378
;;; mh-loaddefs.el ends here
