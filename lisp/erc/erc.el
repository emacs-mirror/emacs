;;; erc.el --- An Emacs Internet Relay Chat client  -*- lexical-binding:t -*-

;; Copyright (C) 1997-2026 Free Software Foundation, Inc.

;; Author: Alexander L. Belikoff <alexander@belikoff.net>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Contributors: Sergey Berezin (sergey.berezin@cs.cmu.edu),
;;               Mario Lang (mlang@delysid.org),
;;               Alex Schroeder (alex@gnu.org)
;;               Andreas Fuchs (afs@void.at)
;;               Gergely Nagy (algernon@midgard.debian.net)
;;               David Edmondson (dme@dme.org)
;;               Michael Olson (mwolson@gnu.org)
;;               Kelvin White (kwhite@gnu.org)
;; Version: 5.6.2-git
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.5"))
;; Keywords: IRC, chat, client, Internet
;; URL: https://www.gnu.org/software/emacs/erc.html

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; ERC is a powerful, modular, and extensible IRC client for Emacs.
;; For more information, visit the ERC page at
;; <https://www.gnu.org/software/emacs/erc.html>.

;; Configuration:

;; Use M-x customize-group RET erc RET to get an overview
;; of all the variables you can tweak.

;; Usage:

;; To connect to an IRC server, do
;;
;; M-x erc RET
;;
;; or
;;
;; M-x erc-tls RET
;;
;; to connect over TLS (encrypted).  Once you are connected to a
;; server, you can use C-h m or have a look at the ERC menu.

;;; Code:

(eval-and-compile (load "erc-loaddefs" 'noerror 'nomessage))

(require 'erc-networks)
(require 'erc-backend)
(require 'cl-lib)
(require 'format-spec)
(require 'auth-source)
(eval-when-compile (require 'subr-x))

(defconst erc-version "5.6.2-git"
  "This version of ERC.")

(defvar erc-official-location
  "https://www.gnu.org/software/emacs/erc.html (mailing list: emacs-erc@gnu.org)"
  "Location of the ERC client on the Internet.")

;; Map each :package-version to the associated Emacs version.
;; (This eliminates the need for explicit :version keywords on the
;; custom definitions.)
(add-to-list
 'customize-package-emacs-version-alist
 '(ERC ("5.2" . "22.1")
       ("5.3" . "23.1")
       ("5.4" . "28.1")
       ("5.4.1" . "29.1")
       ("5.5" . "29.1")
       ("5.6" . "30.1")
       ("5.6.1" . "31.1")
       ("5.6.2" . "31.1")))

(defgroup erc nil
  "Emacs Internet Relay Chat client."
  :link '(url-link "https://www.gnu.org/software/emacs/erc.html")
  :link '(custom-manual "(erc) Top")
  :prefix "erc-"
  :group 'applications)

(defgroup erc-buffers nil
  "Creating new ERC buffers."
  :group 'erc)

(defgroup erc-display nil
  "Settings controlling how various things are displayed.
See the customization group `erc-buffers' for display options
concerning buffers."
  :group 'erc)

(defgroup erc-mode-line-and-header nil
  "Displaying information in the mode-line and header."
  :group 'erc-display)

(defgroup erc-ignore nil
  "Ignoring certain messages."
  :group 'erc)

(defgroup erc-lurker nil
  "Hide specified message types sent by lurkers."
  :version "24.3"
  :group 'erc-ignore)

(defgroup erc-query nil
  "Using separate buffers for private discussions."
  :group 'erc)

(defgroup erc-quit-and-part nil
  "Quitting and parting channels."
  :group 'erc)

(defgroup erc-paranoia nil
  "Know what is sent and received; control the display of sensitive data."
  :group 'erc)

(defgroup erc-scripts nil
  "Running scripts at startup and with /LOAD."
  :group 'erc)

;; Add `custom-loads' features for group symbols missing from a
;; supported Emacs version, possibly because they belong to a new ERC
;; library.  These groups all share their library's feature name.
;;;###autoload(dolist (symbol '( erc-sasl erc-spelling ; 29
;;;###autoload                   erc-imenu erc-nicks)) ; 30
;;;###autoload  (custom-add-load symbol symbol))

(defvar erc-message-parsed) ; only known to this file

(defvar erc--msg-props nil
  "Hash table containing metadata properties for current message.
Provided by the insertion functions `erc-display-message' and
`erc-display-msg' while running their modification hooks.
Initialized when null for each visitation round from function
parameters and environmental factors, as well as the alist
`erc--msg-prop-overrides'.  Keys are symbols.  Values are opaque
objects, unless otherwise specified.  Items present after running
`erc-insert-post-hook' or `erc-send-post-hook' become text
properties added to the first character of an inserted message.
A given message therefore spans the interval extending from one
set of such properties to the newline before the next (or
`erc-insert-marker').  As of ERC 5.6, this forms the basis for
visiting and editing inserted messages.  Modules should align
their markers accordingly.  The following properties have meaning
as of ERC 5.6:

 - `erc--msg': a symbol, guaranteed present; possible values
    include `unknown', a fallback used by `erc-display-message'; a
    catalog key, such as `s401' or `finished'; an
   `erc-display-message' TYPE parameter, like `notice'

 - `erc--cmd': a message's associated IRC command, as read by
   `erc--get-eq-comparable-cmd'; currently either a symbol, like
   `PRIVMSG', or a number, like 5, which represents the numeric
    \"005\"; absent on \"local\" messages, such as simple warnings
    and help text, and on outgoing messages unless echoed back by
    the server (assuming future support)

 - `erc--spkr': a string, the non-case-mapped nick of the speaker as
    stored in the `nickname' slot of its `erc-server-users' item

 - `erc--ctcp': a CTCP command, like `ACTION'

 - `erc--ts': a timestamp, possibly provided by the server; as of
    5.6, a ticks/hertz pair on Emacs 29 and above, and a \"list\"
    type otherwise; managed by the `stamp' module

 - `erc--skip': list of symbols known to modules that indicate an
    intent to skip or simplify module-specific processing

 - `erc--ephemeral': a symbol prefixed by or matching a module
    name; indicates to other modules and members of modification
    hooks that the current message should not affect stateful
    operations, such as recording a channel's most recent speaker

 - `erc--hide': a symbol or list of symbols added as an `invisible'
    prop value to the entire message, starting *before* the preceding
    newline and ending before the trailing newline

This is an internal API, and the selection of related helper
utilities is fluid and provisional.  As of ERC 5.6, see the
functions `erc--check-msg-prop' and `erc--get-inserted-msg-prop'.")

(defvar erc--msg-prop-overrides nil
  "Alist of \"message properties\" for populating `erc--msg-props'.
These override any defaults normally shown to modification hooks
by `erc-display-msg' and `erc-display-message'.  Modules should
accommodate existing overrides when applicable.  Items toward the
front shadow any that follow.  Ignored when `erc--msg-props' is
already non-nil.")

;; Forward declarations
(declare-function decoded-time-period "time-date" (time))
(declare-function iso8601-parse-duration "iso8601" (string))
(declare-function word-at-point "thingatpt" (&optional no-properties))
(autoload 'word-at-point "thingatpt") ; for hl-nicks

(declare-function gnutls-negotiate "gnutls" (&rest rest))
(declare-function socks-open-network-stream "socks" (name buffer host service))
(declare-function url-host "url-parse" (cl-x))
(declare-function url-password "url-parse" (cl-x))
(declare-function url-portspec "url-parse" (cl-x))
(declare-function url-type "url-parse" (cl-x))
(declare-function url-user "url-parse" (cl-x))

;; tunable connection and authentication parameters

(defcustom erc-server nil
  "IRC server to use if one is not provided.
See function `erc-compute-server' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const :tag "None" nil)
                 (string :tag "Server")))

(defcustom erc-port nil
  "IRC port to use if not specified.

This can be either a string or a number."
  :group 'erc
  :type '(choice (const :tag "None" nil)
                 (integer :tag "Port number")
                 (string :tag "Port string")))

(defcustom erc-nick nil
  "Nickname to use if one is not provided.

This can be either a string, or a list of strings.
In the latter case, if the first nick in the list is already in use,
other nicks are tried in the list order.

See function `erc-compute-nick' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const :tag "None" nil)
                 (string :tag "Nickname")
                 (repeat (string :tag "Nickname"))))

(defcustom erc-nick-uniquifier "`"
  "The string to append to the nick if it is already in use."
  :group 'erc
  :type 'string)

(defcustom erc-try-new-nick-p t
  "Non-nil means attempt to connect with another nickname if nickname unavailable.
You can manually set another nickname with the /NICK command."
  :group 'erc
  :type 'boolean)

(defcustom erc-user-full-name nil
  "User full name.

This can be either a string or a function to call.

See function `erc-compute-full-name' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const :tag "No name" nil)
                 (string :tag "Name")
                 (function :tag "Get from function"))
  :set (lambda (sym val)
         (set sym (if (functionp val) (funcall val) val))))

(defcustom erc-rename-buffers t
  "Non-nil means rename buffers with network name, if available."
  :version "24.5"
  :group 'erc
  :type 'boolean)

;; For the sake of compatibility, an ID will be created on the user's
;; behalf when `erc-rename-buffers' is nil and one wasn't provided.
;; The name will simply be that of the buffer, usually SERVER:PORT.
;; This violates the policy of treating provided IDs as gospel, but
;; it'll have to do for now.

(make-obsolete-variable 'erc-rename-buffers
                        "old behavior when t now permanent" "29.1")

(defvar erc-password nil
  "Password to use when authenticating to an IRC server interactively.

This variable only exists for legacy reasons.  It's not customizable and
is limited to a single server password.  Users looking for similar
functionality should consider auth-source instead.  See Info
node `(auth) Top' and Info node `(erc) auth-source'.")

(make-obsolete-variable 'erc-password "use auth-source instead" "29.1")

(defcustom erc-user-mode "+i"
  ;; +i "Invisible".  Hides user from global /who and /names.
  "Initial user modes to be set after a connection is established."
  :group 'erc
  :type '(choice (const nil) string function)
  :version "28.1")


(defcustom erc-prompt-for-password t
  "Ask for a server password when invoking `erc-tls' interactively."
  :group 'erc
  :type 'boolean)

(defcustom erc-warn-about-blank-lines t
  "Warn the user if they attempt to send a blank line.
When non-nil, ERC signals a `user-error' upon encountering prompt
input containing empty or whitespace-only lines.  When nil, ERC
still inhibits sending but does so silently.  With the companion
option `erc-send-whitespace-lines' enabled, ERC sends pending
input and prints a message in the echo area indicating the amount
of padding and/or stripping applied, if any.  Setting this option
to nil suppresses such reporting."
  :group 'erc
  :type 'boolean)

(defcustom erc-send-whitespace-lines nil
  "If set to non-nil, send lines consisting of only whitespace."
  :group 'erc
  :type 'boolean)

(defcustom erc-inhibit-multiline-input nil
  "When non-nil, conditionally disallow input consisting of multiple lines.
Issue an error when the number of input lines submitted for
sending meets or exceeds this value.  The value t is synonymous
with a value of 2 and means disallow more than 1 line of input."
  :package-version '(ERC . "5.5")
  :group 'erc
  :type '(choice integer boolean))

(defcustom erc-ask-about-multiline-input nil
  "Whether to ask to ignore `erc-inhibit-multiline-input' when tripped."
  :package-version '(ERC . "5.5")
  :group 'erc
  :type 'boolean)

(defcustom erc-prompt-hidden ">"
  "Text to show in lieu of the prompt when hidden."
  :package-version '(ERC . "5.5")
  :group 'erc-display
  :type 'string)

(defcustom erc-hide-prompt t
  "If non-nil, hide input prompt upon disconnecting.
To unhide, type something in the input area.  Once revealed, a
prompt remains unhidden until the next disconnection.  Channel
prompts are unhidden upon rejoining.  For behavior concerning
query prompts, see `erc-unhide-query-prompt'.  Longtime ERC users
should note that this option was repurposed in ERC 5.5 because it
had lain dormant for years after being sidelined in 5.3 when its
only use in the interactive client was removed.  Before then, its
role was controlling whether `erc-command-indicator' would appear
alongside echoed slash-command lines."
  :package-version '(ERC . "5.5")
  :group 'erc-display
  :type '(choice (const :tag "Always hide prompt" t)
                 (set (const server)
                      (const query)
                      (const channel))))

(defcustom erc-unhide-query-prompt nil
  "When non-nil, always reveal query prompts upon reconnecting.
Otherwise, prompts in a connection's query buffers remain hidden
until the user types in the input area or a new message arrives
from the target."
  :package-version '(ERC . "5.5")
  :group 'erc-display
  ;; Extensions may one day offer a way to discover whether a target
  ;; is online.  When that happens, this can be expanded accordingly.
  :type 'boolean)

;; tunable GUI stuff

(defcustom erc-show-my-nick t
  "If non-nil, display one's own nickname when sending a message.

If non-nil, \"<nickname>\" will be shown.
If nil, only \"> \" will be shown."
  :group 'erc-display
  :type 'boolean)

(define-widget 'erc-message-type 'set
  "A set of standard IRC Message types."
  :args '((const "JOIN")
          (const "KICK")
          (const "NICK")
          (const "PART")
          (const "QUIT")
          (const "MODE")
          (const :tag "Away notices (RPL_AWAY 301)" "301")
          (const :tag "Self back notice (REP_UNAWAY 305)" "305")
          (const :tag "Self away notice (REP_NOWAWAY 306)" "306")
          (const :tag "Channel modes on join (RPL_CHANNELMODEIS 324)" "324")
          (const :tag "Channel creation time (RPL_CREATIONTIME 329)" "329")
          (const :tag "Channel no-topic on join (RPL_NOTOPIC 331)" "331")
          (const :tag "Channel topic on join (RPL_TOPIC 332)" "332")
          (const :tag "Topic author and time on join (RPL_TOPICWHOTIME 333)" "333")
          (const :tag "Invitation success notice (RPL_INVITING 341)" "341")
          (const :tag "Channel member names (353 RPL_NAMEREPLY)" "353")
          (repeat :inline t :tag "Others" (string :tag "IRC Message Type"))))

(defcustom erc-hide-list nil
  "A global list of IRC message types to hide.
A typical value would be \(\"JOIN\" \"PART\" \"QUIT\")."
  :group 'erc-ignore
  :type 'erc-message-type)

(defcustom erc-network-hide-list nil
  "A list of IRC networks to hide message types from.
A typical value would be \((\"Libera.Chat\" \"MODE\")
  \(\"OFTC\" \"JOIN\" \"QUIT\"))."
  :version "25.1"
  :group 'erc-ignore
  :type '(alist :key-type string :value-type erc-message-type
                :options ("Libera.Chat")))

(defcustom erc-channel-hide-list nil
  "A list of IRC channels to hide message types from.
A typical value would be \((\"#emacs\" \"QUIT\" \"JOIN\")
  \(\"#erc\" \"NICK\")."
  :version "25.1"
  :group 'erc-ignore
  :type '(alist :key-type string :value-type erc-message-type
                :options ("#emacs")))

(defcustom erc-disconnected-hook nil
  "Run this hook with arguments (NICK IP REASON) when disconnected.
This happens before automatic reconnection.  Note, that
`erc-server-QUIT-functions' might not be run when we disconnect,
simply because we do not necessarily receive the QUIT event."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-complete-functions nil
  "These functions get called when the user hits \\`TAB' in ERC.
Each function in turn is called until one returns non-nil to
indicate it has handled the input."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-join-hook nil
  "Hook run when we join a channel.
Hook functions are called without arguments, with the current
buffer set to the buffer of the new channel.

See also `erc-server-JOIN-functions', `erc-part-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-quit-hook nil
  "Hook run when processing a quit command directed at our nick.

The hook receives one argument, the current PROCESS.
See also `erc-server-QUIT-functions' and `erc-disconnected-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-part-hook nil
  "Hook run when processing a PART message directed at our nick.
Called in the server buffer with a single argument: the channel buffer
being parted.  For historical reasons, the buffer argument may be nil if
it's been killed or otherwise can't be found.  This typically happens
when the \"PART\" response being handled comes by way of a channel
buffer being killed, which, by default, tells `erc-part-channel-on-kill'
to emit a \"PART\".  Users needing to operate on a parted channel buffer
before it's killed in this manner should use `erc-kill-channel-hook' and
condition their code on `erc-killing-buffer-on-part-p' being nil."
  :group 'erc-hooks
  :type 'hook)

(defvar erc-killing-buffer-on-part-p nil
  "Non-nil when killing a target buffer while handling a \"PART\" response.
Useful for preventing the redundant execution of code designed to run
once when parting a channel.")

(defcustom erc-kick-hook nil
  "Hook run when processing a KICK message directed at our nick.

The hook receives one argument, the current BUFFER.
See also `erc-server-PART-functions' and `erc-part-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-nick-changed-functions nil
  "List of functions run when your nick was successfully changed.

Each function should accept two arguments, NEW-NICK and OLD-NICK."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-connect-pre-hook '(erc-initialize-log-marker)
  "Hook called just before `erc' calls `erc-connect'.
Functions are passed a buffer as the first argument."
  :group 'erc-hooks
  :type 'hook)

(defvaralias 'erc-channel-users 'erc-channel-members)
(defvar-local erc-channel-members nil
  "Hash table of members in the current channel or query buffer.
It associates nicknames with cons cells of the form
\(SERVER-USER . MEMBER-DATA), where SERVER-USER is a
`erc-server-user' object and MEMBER-DATA is a `erc-channel-user'
object.  Convenient abbreviations for these two components are
`susr' and `cusr', along with `cmem' for the pair.")

(defvar-local erc-server-users nil
  "Hash table of users on the current server.
It associates nicknames with `erc-server-user' struct instances.")

(defconst erc--casemapping-rfc1459-strict
  (let ((tbl (copy-sequence ascii-case-table))
        (cup (copy-sequence (char-table-extra-slot ascii-case-table 0))))
    (set-char-table-extra-slot tbl 0 cup)
    (set-char-table-extra-slot tbl 1 nil)
    (set-char-table-extra-slot tbl 2 nil)
    (pcase-dolist (`(,uc . ,lc) '((?\[ . ?\{) (?\] . ?\}) (?\\ . ?\|)))
      (aset tbl uc lc)
      (aset tbl lc lc)
      (aset cup uc uc))
    tbl))

(defconst erc--casemapping-rfc1459
  (let ((tbl (copy-sequence erc--casemapping-rfc1459-strict))
        (cup (copy-sequence (char-table-extra-slot
                             erc--casemapping-rfc1459-strict 0))))
    (set-char-table-extra-slot tbl 0 cup)
    (aset tbl ?~ ?^)
    (aset tbl ?^ ?^)
    (aset cup ?~ ?~)
    tbl))

(defun erc-add-server-user (nick user)
  "This function is for internal use only.

Adds USER with nickname NICK to the `erc-server-users' hash table."
  (erc-with-server-buffer
    (puthash (erc-downcase nick) user erc-server-users)))

(defvar erc--decouple-query-and-channel-membership-p nil
  "When non-nil, don't tether query participation to channel membership.
Specifically, add users to query tables when they speak, don't remove
them when they leave all channels, and allow removing the client's own
user from `erc-server-users'.  Note that enabling this compatibility
flag degrades the user experience and isn't guaranteed to correctly
restore the described historical behavior.")

(defvar erc--query-table-synced-predicate #'erc--query-participant-present-p
  "Predicate for whether a query buffer's member table dynamically updates.
By default, ERC flies half blind by managing membership based on shared
channels.  This rules out false positives but accepts the chance of
participants being on the server but absent from local tables.")

(defun erc--query-participant-present-p ()
  "Return non-nil if the query participant is present in the member table."
  (and (not erc--decouple-query-and-channel-membership-p)
       (erc-query-buffer-p) (erc-get-channel-member (erc-target))))

(defun erc--ensure-query-member (nick)
  "Populate membership table in query buffer for online NICK."
  (erc-with-buffer (nick)
    (when-let* (((not erc--decouple-query-and-channel-membership-p))
                ((zerop (hash-table-count erc-channel-users)))
                (user (erc-get-server-user nick)))
      (erc-update-current-channel-member nick nil t)
      (erc--unhide-prompt)
      t)))

(defun erc--ensure-query-members ()
  "Update membership tables in all query buffers.
Ensure targets with an entry in `erc-server-users' are present in
`erc-channel-members'."
  (erc-with-all-buffers-of-server erc-server-process #'erc-query-buffer-p
    (when-let* (((not erc--decouple-query-and-channel-membership-p))
                ((zerop (hash-table-count erc-channel-users)))
                (target (erc-target))
                ((erc-get-server-user target)))
      (erc-update-current-channel-member target nil t)
      (erc--unhide-prompt))
    erc-server-process))

(defun erc-remove-server-user (nick)
  "Remove NICK from the session's `erc-server-users' table."
  (erc-with-server-buffer
    (remhash (erc-downcase nick) erc-server-users)))

(defun erc-change-user-nickname (user new-nick)
  "This function is for internal use only.

Changes the nickname of USER to NEW-NICK in the
`erc-server-users' hash table.  The `erc-channel-users' lists of
other buffers are also changed."
  (let ((nick (erc-server-user-nickname user)))
    (setf (erc-server-user-nickname user) new-nick)
    (erc-with-server-buffer
      (remhash (erc-downcase nick) erc-server-users)
      (puthash (erc-downcase new-nick) user erc-server-users))
    (dolist (buf (erc-server-user-buffers user))
      (if (buffer-live-p buf)
          (with-current-buffer buf
            (let ((cdata (erc-get-channel-user nick)))
              (remhash (erc-downcase nick) erc-channel-users)
              (puthash (erc-downcase new-nick) cdata
                       erc-channel-users)))))))

(defvar erc--forget-server-user-function
  #'erc--forget-server-user-ignoring-queries
  "Function to conditionally remove a user from `erc-server-users'.
Called with a nick and its `erc-server-user' object.")

(defun erc--forget-server-user (nick user)
  "Remove NICK's USER from server table if they're not in any target buffers."
  (unless (erc-server-user-buffers user)
    (erc-remove-server-user nick)))

(defun erc--forget-server-user-ignoring-queries (nick user)
  "Remove NICK's USER from `erc-server-users' if they've parted all channels."
  (let ((buffers (erc-server-user-buffers user)))
    (when (or (null buffers)
              (and (not erc--decouple-query-and-channel-membership-p)
                   (cl-every #'erc-query-buffer-p buffers)))
      (when buffers
        (erc--remove-user-from-targets (erc-downcase nick) buffers))
      (erc-remove-server-user nick))))

(defun erc-remove-channel-user (nick)
  "Remove NICK from the current target buffer's `erc-channel-members'.
If this was their only target, also remove them from `erc-server-users'."
  (let ((channel-data (erc-get-channel-user nick)))
    (when channel-data
      (let ((user (car channel-data)))
        (setf (erc-server-user-buffers user)
              (delq (current-buffer)
                    (erc-server-user-buffers user)))
        (remhash (erc-downcase nick) erc-channel-users)
        (funcall erc--forget-server-user-function nick user)))))

(defun erc-remove-user (nick)
  "Remove NICK from the server and all relevant channels tables."
  (let ((user (erc-get-server-user nick)))
    (when user
      (erc--remove-user-from-targets (erc-downcase nick)
                                     (erc-server-user-buffers user))
      (erc-remove-server-user nick))))

(defun erc-remove-channel-users ()
  "Drain current buffer's `erc-channel-members' table.
Also remove members from the server table if this was their only buffer."
  (when (erc--target-channel-p erc--target)
    (setf (erc--target-channel-joined-p erc--target) nil))
  (when (and erc-server-connected
             (erc-server-process-alive)
             (hash-table-p erc-channel-users))
    (maphash (lambda (nick _cdata)
               (erc-remove-channel-user nick))
             erc-channel-users)
    (clrhash erc-channel-users)))

(defun erc--remove-channel-users-but (nick)
  "Drain channel users and remove from server, sparing NICK."
  (when-let* ((users (erc-with-server-buffer erc-server-users))
              (my-user (gethash (erc-downcase nick) users))
              (original-function erc--forget-server-user-function)
              (erc--forget-server-user-function
               (if erc--decouple-query-and-channel-membership-p
                   erc--forget-server-user-function
                 (lambda (nick user)
                   (unless (eq user my-user)
                     (funcall original-function nick user))))))
    (erc-remove-channel-users)))

(defvar erc-channel-user-signal-if-status-unknown nil
  "If non-nil ERC signals before setting an unadvertised status prefix.
But only in ERC buffers.  Otherwise, if nil, accessors like
`erc-channel-user-halfop' ignore such attempts and return nil.")

(defmacro erc--define-channel-user-status-compat-getter (name c d)
  "Define accessor with gv getter for historical `erc-channel-user' slot NAME.
Expect NAME to be a string, C to be its traditionally associated letter,
and D to be its fallback power-of-2 integer for non-ERC buffers.  Unlike
pre-ERC-5.6 accessors, do not bother generating a compiler macro for
inlining calls to these adapters."
  `(defun ,(intern (concat "erc-channel-user-" name)) (u)
     ,(concat
       "Get equivalent of pre-5.6 `" name "' slot for `erc-channel-user'."
       "\nUse a fallback value in non-ERC buffers.  Treat an unadvertised"
       "\nstatus according to `erc-channel-user-signal-if-status-unknown'.")
     (declare (gv-setter
               (lambda (v)
                 (macroexp-let2 nil v v
                   (,'\`(let* ((val (erc-channel-user-status ,',u))
                               (p (erc--parsed-prefix))
                               (n (if p (or (erc--get-prefix-flag ,c p) 0) ,d))
                               (nop (and p ,',v (zerop n))) ; unsupportedp
                               (rv (and (not nop) ,',v)))
                          (when (and nop
                                     erc-channel-user-signal-if-status-unknown)
                            (error "Unsupported status prefix: %c" ,c))
                          (unless nop
                            (setf (erc-channel-user-status ,',u)
                                  (if ,',v
                                      (logior val n)
                                    (logand val (lognot n)))))
                          rv))))))
     (let* ((p (erc--parsed-prefix))
            (n (if p (erc--get-prefix-flag ,c p) ,d)))
       (and n (= n (logand n (erc-channel-user-status u)))))))

(erc--define-channel-user-status-compat-getter "voice"  ?v 1)
(erc--define-channel-user-status-compat-getter "halfop" ?h 2)
(erc--define-channel-user-status-compat-getter "op"     ?o 4)
(erc--define-channel-user-status-compat-getter "admin"  ?a 8)
(erc--define-channel-user-status-compat-getter "owner"  ?q 16)

;; This is a generalized version of the compat-oriented getters above.
(defun erc--cusr-status-p (nick-or-cusr letter)
  "Return non-nil if NICK-OR-CUSR has channel membership status LETTER."
  (and-let* ((cusr (or (and (erc-channel-user-p nick-or-cusr) nick-or-cusr)
                       (cdr (erc-get-channel-member nick-or-cusr))))
             (n (erc--get-prefix-flag letter)))
    (= n (logand n (erc-channel-user-status cusr)))))

(defun erc--cusr-change-status (nick-or-cusr letter enablep &optional resetp)
  "Add or remove membership status associated with LETTER for NICK-OR-CUSR.
With RESETP, clear the user's status info completely.  If ENABLEP
is non-nil, add the status value associated with LETTER."
  (when-let* ((cusr (or (and (erc-channel-user-p nick-or-cusr) nick-or-cusr)
                        (cdr (erc-get-channel-member nick-or-cusr))))
              (n (erc--get-prefix-flag letter)))
    (cl-callf (lambda (v)
                (if resetp
                    (if enablep n 0)
                  (if enablep (logior v n) (logand v (lognot n)))))
        (erc-channel-user-status cusr))))

(defun erc-channel-user-owner-p (nick)
  "Return non-nil if NICK is an owner of the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
         (and cdata (cdr cdata)
              (erc-channel-user-owner (cdr cdata))))))

(defun erc-channel-user-admin-p (nick)
  "Return non-nil if NICK is an admin in the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
         (and cdata (cdr cdata)
              (erc-channel-user-admin (cdr cdata))))))

(defun erc-channel-user-op-p (nick)
  "Return non-nil if NICK is an operator in the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
         (and cdata (cdr cdata)
              (erc-channel-user-op (cdr cdata))))))

(defun erc-channel-user-halfop-p (nick)
  "Return non-nil if NICK is a half-operator in the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
         (and cdata (cdr cdata)
              (erc-channel-user-halfop (cdr cdata))))))

(defun erc-channel-user-voice-p (nick)
  "Return non-nil if NICK has voice in the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
         (and cdata (cdr cdata)
              (erc-channel-user-voice (cdr cdata))))))

(defun erc-get-channel-user-list ()
  "Return a list of users in the current channel.
Each element of the list is of the form (USER . CHANNEL-DATA),
where USER is an erc-server-user struct, and CHANNEL-DATA is
either nil or an erc-channel-user struct.

See also: `erc-sort-channel-users-by-activity'."
  (let (users)
    (if (hash-table-p erc-channel-users)
        (maphash (lambda (_nick cdata)
                   (setq users (cons cdata users)))
                 erc-channel-users))
    users))

(defun erc-get-server-nickname-list ()
  "Return a list of known nicknames on the current server."
  (erc-with-server-buffer
    (let (nicks)
      (when (hash-table-p erc-server-users)
        (maphash (lambda (_n user)
                   (setq nicks
                         (cons (erc-server-user-nickname user)
                               nicks)))
                 erc-server-users)
        nicks))))

(defun erc-get-channel-nickname-list ()
  "Return a list of known nicknames on the current channel."
  (let (nicks)
    (when (hash-table-p erc-channel-users)
      (maphash (lambda (_n cdata)
                 (setq nicks
                       (cons (erc-server-user-nickname (car cdata))
                             nicks)))
               erc-channel-users)
      nicks)))

(defun erc-get-server-nickname-alist ()
  "Return an alist of known nicknames on the current server."
  (erc-with-server-buffer
    (let (nicks)
      (when (hash-table-p erc-server-users)
        (maphash (lambda (_n user)
                   (setq nicks
                         (cons (cons (erc-server-user-nickname user) nil)
                               nicks)))
                 erc-server-users)
        nicks))))

(defun erc-get-channel-nickname-alist ()
  "Return an alist of known nicknames on the current channel."
  (let (nicks)
    (when (hash-table-p erc-channel-users)
      (maphash (lambda (_n cdata)
                 (setq nicks
                       (cons (cons (erc-server-user-nickname (car cdata)) nil)
                             nicks)))
               erc-channel-users)
      nicks)))

(defun erc-sort-channel-users-by-activity (list)
  "Sort LIST such that users which have spoken most recently are listed first.
LIST must be of the form (USER . CHANNEL-DATA).

See also: `erc-get-channel-user-list'."
  (sort list
        (lambda (x y)
          (when (and (cdr x) (cdr y))
            (let ((tx (erc-channel-user-last-message-time (cdr x)))
                  (ty (erc-channel-user-last-message-time (cdr y))))
              (and tx
                   (or (not ty)
                       (time-less-p ty tx))))))))

(defun erc-sort-channel-users-alphabetically (list)
  "Sort LIST so that users' nicknames are in alphabetical order.
LIST must be of the form (USER . CHANNEL-DATA).

See also: `erc-get-channel-user-list'."
  (sort list
        (lambda (x y)
          (when (and (cdr x) (cdr y))
            (let ((nickx (downcase (erc-server-user-nickname (car x))))
                  (nicky (downcase (erc-server-user-nickname (car y)))))
              (and nickx
                   (or (not nicky)
                       (string-lessp nickx nicky))))))))

(defvar-local erc-channel-topic nil
  "A topic string for the channel.  Should only be used in channel-buffers.")

(defvar-local erc-channel-modes nil
  "List of letters, as strings, representing channel modes.
For example, (\"i\" \"m\" \"s\").  Modes that take accompanying
parameters are not included.")

(defvar-local erc-insert-marker nil
  "The place where insertion of new text in erc buffers should happen.")

(defvar-local erc-input-marker nil
  "The marker where input should be inserted.")

(defun erc-string-no-properties (string)
  "Return a copy of STRING will all text-properties removed."
  (let ((newstring (copy-sequence string)))
    (set-text-properties 0 (length newstring) nil newstring)
    newstring))

(defcustom erc-prompt "ERC>"
  "Prompt used by ERC.  Trailing whitespace is not required."
  :group 'erc-display
  :type '(choice string
                 (function-item :tag "Interpret format specifiers"
                                erc-prompt-format)
                 function))

(defvar erc--prompt-format-face-example
  #("%p%m%a\u00b7%b>"
    0 2 (font-lock-face erc-my-nick-prefix-face)
    2 4 (font-lock-face font-lock-keyword-face)
    4 6 (font-lock-face erc-error-face)
    6 7 (font-lock-face shadow)
    7 9 (font-lock-face font-lock-constant-face)
    9 10 (font-lock-face shadow))
  "An example value for option `erc-prompt-format' with faces.")

(defcustom erc-prompt-format erc--prompt-format-face-example
  "Format string when `erc-prompt' is `erc-prompt-format'.
ERC recognizes these substitution specifiers:

 %a - away indicator
 %b - buffer name
 %t - channel or query target, server domain, or dialed address
 %S - target@network or buffer name
 %s - target@server or server
 %N - current network, like Libera.Chat
 %p - channel membership prefix, like @ or +
 %n - current nickname
 %c - channel modes with args for select modes
 %C - channel modes with all args
 %u - user modes
 %m - channel modes sans args in channels, user modes elsewhere
 %M - like %m but show nothing in query buffers

To pick your own colors, do something like:

  (setopt erc-prompt-format
          (concat
           (propertize \"%b\" \\='font-lock-face \\='erc-input-face)
           (propertize \"%a\" \\='font-lock-face \\='erc-error-face)))

Please remember that ERC ignores this option completely unless
the \"parent\" option `erc-prompt' is set to `erc-prompt-format'."
  :package-version '(ERC . "5.6")
  :group 'erc-display
  :type `(choice (const :tag "{Prefix}{Mode}{Away}{MIDDLE DOT}{Buffer}>"
                        ,erc--prompt-format-face-example)
                 string))

(defun erc-prompt-format ()
  "Make predefined `format-spec' substitutions.

See option `erc-prompt-format' and option `erc-prompt'."
  (format-spec erc-prompt-format
               (erc-compat--defer-format-spec-in-buffer
                (?C erc--channel-modes 3 ",")
                (?M erc--format-modes 'no-query-p)
                (?N erc-format-network)
                (?S erc-format-target-and/or-network)
                (?a erc--format-away-indicator)
                (?b buffer-name)
                (?c erc-format-channel-modes)
                (?m erc--format-modes)
                (?n erc-current-nick)
                (?p erc--format-channel-status-prefix)
                (?s erc-format-target-and/or-server)
                (?t erc-format-target)
                (?u erc--format-user-modes))
               'ignore-missing)) ; formerly `only-present'

(defun erc-prompt ()
  "Return the input prompt as a string.

See also the variable `erc-prompt'."
  (let ((prompt (if (functionp erc-prompt)
                    (funcall erc-prompt)
                  erc-prompt)))
    (if (> (length prompt) 0)
        (concat prompt " ")
      prompt)))

(defcustom erc-notice-prefix "*** "
  "Prefix for all notices."
  :group 'erc-display
  :type 'string)

(defcustom erc-notice-highlight-type 'all
  "Determines how to highlight notices.
See `erc-notice-prefix'.

The following values are allowed:

    `prefix' - highlight notice prefix only
    `all'    - highlight the entire notice

Any other value disables notice's highlighting altogether."
  :group 'erc-display
  :type '(choice (const :tag "highlight notice prefix only" prefix)
                 (const :tag "highlight the entire notice" all)
                 (const :tag "don't highlight notices at all" nil)))

(defcustom erc-echo-notice-hook nil
  "List of functions to call to echo a private notice.
Each function is called with four arguments, the string
to display, the parsed server message, the target buffer (or
nil), and the sender.  The functions are called in order, until a
function evaluates to non-nil.  These hooks are called after
those specified in `erc-echo-notice-always-hook'.

See also: `erc-echo-notice-always-hook',
`erc-echo-notice-in-default-buffer',
`erc-echo-notice-in-target-buffer',
`erc-echo-notice-in-minibuffer',
`erc-echo-notice-in-server-buffer',
`erc-echo-notice-in-active-non-server-buffer',
`erc-echo-notice-in-active-buffer',
`erc-echo-notice-in-user-buffers',
`erc-echo-notice-in-user-and-target-buffers',
`erc-echo-notice-in-first-user-buffer'."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-echo-notice-in-default-buffer
             erc-echo-notice-in-target-buffer
             erc-echo-notice-in-minibuffer
             erc-echo-notice-in-server-buffer
             erc-echo-notice-in-active-non-server-buffer
             erc-echo-notice-in-active-buffer
             erc-echo-notice-in-user-buffers
             erc-echo-notice-in-user-and-target-buffers
             erc-echo-notice-in-first-user-buffer))

(defcustom erc-echo-notice-always-hook
  '(erc-echo-notice-in-default-buffer)
  "List of functions to call to echo a private notice.
Each function is called with four arguments, the string
to display, the parsed server message, the target buffer (or
nil), and the sender.  The functions are called in order, and all
functions are called.  These hooks are called before those
specified in `erc-echo-notice-hook'.

See also: `erc-echo-notice-hook',
`erc-echo-notice-in-default-buffer',
`erc-echo-notice-in-target-buffer',
`erc-echo-notice-in-minibuffer',
`erc-echo-notice-in-server-buffer',
`erc-echo-notice-in-active-non-server-buffer',
`erc-echo-notice-in-active-buffer',
`erc-echo-notice-in-user-buffers',
`erc-echo-notice-in-user-and-target-buffers',
`erc-echo-notice-in-first-user-buffer'."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-echo-notice-in-default-buffer
             erc-echo-notice-in-target-buffer
             erc-echo-notice-in-minibuffer
             erc-echo-notice-in-server-buffer
             erc-echo-notice-in-active-non-server-buffer
             erc-echo-notice-in-active-buffer
             erc-echo-notice-in-user-buffers
             erc-echo-notice-in-user-and-target-buffers
             erc-echo-notice-in-first-user-buffer))

;; other tunable parameters

(defcustom erc-whowas-on-nosuchnick nil
  "If non-nil, do a whowas on a nick if no such nick."
  :group 'erc
  :type 'boolean)

(defcustom erc-verbose-server-ping nil
  "If non-nil, show every time you get a PING or PONG from the server."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-public-away-p nil
  "Let others know you are back when you are no longer marked away.
This happens in this form:
* <nick> is back (gone for <time>)

Many consider it impolite to do so automatically."
  :group 'erc
  :type 'boolean)

(defcustom erc-away-nickname nil
  "The nickname to take when you are marked as being away."
  :group 'erc
  :type '(choice (const nil)
                 string))

(defcustom erc-paranoid nil
  "If non-nil, then all incoming CTCP requests will be shown."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-disable-ctcp-replies nil
  "Disable replies to CTCP requests that require a reply.
If non-nil, then all incoming CTCP requests that normally require
an automatic reply (like VERSION or PING) will be ignored.  Good to
set if some hacker is trying to flood you away."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-anonymous-login t
  "Be paranoid, don't give away your machine name."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-prompt-for-channel-key nil
  "Prompt for channel key when using `erc-join-channel' interactively."
  :group 'erc
  :type 'boolean)

(defcustom erc-email-userid "user"
  "Use this as your email user ID."
  :group 'erc
  :type 'string)

(defcustom erc-system-name nil
  "Use this as the name of your system.
If nil, ERC will call function `system-name' to get this information."
  :group 'erc
  :type '(choice (const :tag "Default system name" nil)
                 string))

(defcustom erc-ignore-list nil
  "List of regexps matching user identifiers to ignore.

A user identifier has the form \"nick!login@host\".  If an
identifier matches, the message from the person will not be
processed."
  :group 'erc-ignore
  :type '(repeat regexp)
  :local t)

(defcustom erc-ignore-reply-list nil
  "List of regexps matching user identifiers to ignore completely.

This differs from `erc-ignore-list' in that it also ignores any
messages directed at the user.

A user identifier has the form \"nick!login@host\".

If an identifier matches, or a message is addressed to a nick
whose identifier matches, the message will not be processed.

CAVEAT: ERC doesn't know about the user and host of anyone who
was already in the channel when you joined, but never said
anything, so it won't be able to match the user and host of those
people.  You can update the ERC internal info using /WHO *."
  :group 'erc-ignore
  :type '(repeat regexp))

(defvar erc-flood-protect t
  "If non-nil, flood protection is enabled.
Flooding is sending too much information to the server in too
short of an interval, which may cause the server to terminate the
connection.

Note that older code conflated rate limiting and line splitting.
Starting in ERC 5.6, this option no longer influences the latter.

See `erc-server-flood-margin' for other flood-related parameters.")

;; Script parameters

(defcustom erc-startup-file-list
  (list (locate-user-emacs-file ".ercrc.el")
        (locate-user-emacs-file ".ercrc")
        "~/.ercrc.el" "~/.ercrc" ".ercrc.el" ".ercrc")
  "List of files to try for a startup script.
The first existent and readable one will get executed.

If the filename ends with `.el' it is presumed to be an Emacs Lisp
script and it gets (load)ed.  Otherwise it is treated as a bunch of
regular IRC commands."
  :group 'erc-scripts
  :type '(repeat file))

(defcustom erc-script-path nil
  "List of directories to look for a script in /load command.
The script is first searched in the current directory, then in each
directory in the list."
  :group 'erc-scripts
  :type '(repeat directory))

(defcustom erc-script-echo t
  "If non-nil, echo the IRC script commands locally."
  :group 'erc-scripts
  :type 'boolean)

(defvar-local erc-last-saved-position nil
  "A marker containing the position the current buffer was last saved at.")

(defcustom erc-kill-buffer-on-part nil
  "Kill the channel buffer on PART.
Nil by default because ERC can reuse buffers later re-joined."
  :group 'erc-quit-and-part
  :type 'boolean)

(defcustom erc-kill-queries-on-quit nil
  "Kill all query (also channel) buffers of this server on QUIT.
See the variable `erc-kill-buffer-on-part' for details."
  :group 'erc-quit-and-part
  :type 'boolean)

(defcustom erc-kill-server-buffer-on-quit nil
  "Kill the server buffer of the process on QUIT."
  :group 'erc-quit-and-part
  :type 'boolean)

(defcustom erc-quit-reason-various-alist nil
  "Alist of possible arguments to the /quit command.

Each element has the form:
  (REGEXP RESULT)

If REGEXP matches the argument to /quit, then its relevant RESULT
will be used.  RESULT may be either a string, or a function.  If
a function, it should return the quit message as a string.

If no elements match, then the empty string is used.

As an example:
  (setq erc-quit-reason-various-alist
      \\='((\"xmms\" dme:now-playing)
        (\"version\" erc-quit-reason-normal)
        (\"home\" \"Gone home !\")
        (\"^$\" \"Default Reason\")))
If the user types \"/quit home\", then \"Gone home !\" will be used
as the quit message."
  :group 'erc-quit-and-part
  :type '(repeat (list regexp (choice (string) (function)))))

(defcustom erc-part-reason-various-alist nil
  "Alist of possible arguments to the /part command.

Each element has the form:
  (REGEXP RESULT)

If REGEXP matches the argument to /part, then its relevant RESULT
will be used.  RESULT may be either a string, or a function.  If
a function, it should return the part message as a string.

If no elements match, then the empty string is used.

As an example:
  (setq erc-part-reason-various-alist
      \\='((\"xmms\" dme:now-playing)
        (\"version\" erc-part-reason-normal)
        (\"home\" \"Gone home !\")
        (\"^$\" \"Default Reason\")))
If the user types \"/part home\", then \"Gone home !\" will be used
as the part message."
  :group 'erc-quit-and-part
  :type '(repeat (list regexp (choice (string) (function)))))

(defcustom erc-quit-reason 'erc-quit-reason-normal
  "A function which returns the reason for quitting.

The function is passed a single argument, the string typed by the
user after \"/quit\"."
  :group 'erc-quit-and-part
  :type '(choice (const erc-quit-reason-normal)
                 (const erc-quit-reason-various)
                 (symbol)))

(defcustom erc-part-reason 'erc-part-reason-normal
  "A function which returns the reason for parting a channel.

The function is passed a single argument, the string typed by the
user after \"/PART\"."
  :group 'erc-quit-and-part
  :type '(choice (const erc-part-reason-normal)
                 (const erc-part-reason-various)
                 (symbol)))

(defvar erc-grab-buffer-name "*erc-grab*"
  "The name of the buffer created by `erc-grab-region'.")

;; variables available for IRC scripts

(defvar erc-user-information "ERC User"
  "USER_INFORMATION IRC variable.")

;; Hooks

(defgroup erc-hooks nil
  "Hooks for ERC.
Users of the interactive client should be aware that many of
these hooks have names predating the modern convention of
conveying abnormality via the \"-function\" suffix.  Users should
likewise be aware that built-in and third-party modules use these
hooks as well, and some of their variables may be buffer-local in
particular sessions and/or `let'-bound for spells."
  :group 'erc)

(defcustom erc-mode-hook nil
  "Hook run after `erc-mode' setup is finished.
Members should be robust enough to run in any order and not depend on
hook depth."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-timer-hook nil
  "Abnormal hook run after each response handler.
Called with a float returned from `erc-current-time'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-pre-hook nil
  "Hook called first when some text is inserted through `erc-display-line'.
It gets called with one argument, STRING.
To be able to modify the inserted text, use `erc-insert-modify-hook' instead.
Filtering functions can set `erc-insert-this' to nil to avoid
display of that particular string at all."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-pre-hook nil
  "Hook called first when some text is sent through `erc-send-current-line'.
It gets called with one argument, STRING.

To change the text that will be sent, set the variable `str' which is
used in `erc-send-current-line'.

To change the text inserted into the buffer without changing the text
that will be sent, use `erc-send-modify-hook' instead.

Filtering functions can set `erc-send-this' to nil to avoid sending of
that particular string at all and `erc-insert-this' to prevent
inserting that particular string into the buffer.

Note that it's useless to set `erc-send-this' to nil and
`erc-insert-this' to t.  ERC is sane enough to not insert the text
anyway."
  :group 'erc-hooks
  :type 'hook)
(make-obsolete-variable 'erc-send-pre-hook 'erc-pre-send-functions "27.1")

(defcustom erc-pre-send-functions nil
  "Special hook to possibly alter the string to send and insert.
ERC calls the member functions with one argument, an `erc-input'
struct instance to modify as needed.

The struct has five slots:

  `string': String to send, originally from prompt input.
  `insertp': Whether a string should be inserted in the buffer.
  `sendp': Whether `string' should be sent to the IRC server.
  `substxt': String to display (but not send) instead of `string'.
  `refoldp': Whether to re-split `string' per protocol limits.

This hook runs after protocol line splitting has taken place, so
the value of `string' comes \"pre-split\" according to the option
`erc-split-line-length'.  If you need ERC to refill the entire
payload before sending it, set the `refoldp' slot to a non-nil
value.  Note that this refilling is only a convenience, and
modules with special needs, such as preserving \"preformatted\"
text or encoding for subprotocol \"tunneling\", should handle
splitting manually and possibly also specify replacement text to
display via the `substxt' slot."
  :package-version '(ERC . "5.3")
  :group 'erc-hooks
  :type 'hook)

(define-obsolete-variable-alias 'erc--pre-send-split-functions
  'erc--input-review-functions "30.1")
(defvar erc--input-review-functions '(erc--split-lines
                                      erc--run-input-validation-checks
                                      erc--discard-trailing-multiline-nulls
                                      erc--inhibit-slash-cmd-insertion)
  "Special hook for reviewing and modifying prompt input.
ERC runs this before clearing the prompt and before running any
send-related hooks, such as `erc-pre-send-functions'.  Thus, it's
quite \"safe\" to bail out of this hook with a `user-error', if
necessary.  The hook's members are called with one argument, an
`erc--input-split' struct, which they can optionally modify.

The struct has five slots:

  `string': the original input as a read-only reference
  `insertp': same as in `erc-pre-send-functions'
  `sendp': same as in `erc-pre-send-functions'
  `refoldp': same as in `erc-pre-send-functions'
  `lines': a list of lines to be sent, each one a `string'
  `cmdp': whether to interpret input as a command, like /ignore

When `cmdp' is non-nil, all but the first line will be discarded.")

(defvar erc-insert-this t
  "Insert the text into the target buffer or not.
Functions on `erc-insert-pre-hook' can set this variable to nil
if they wish to avoid insertion of a particular string.")

(defvar erc-send-this t
  "Send the text to the target or not.
Functions on `erc-send-pre-hook' can set this variable to nil
if they wish to avoid sending of a particular string.")
(make-obsolete-variable 'erc-send-this 'erc-pre-send-functions "27.1")

(defcustom erc-insert-modify-hook ()
  "Insertion hook for functions that will change the text's appearance.
This hook is called just after `erc-insert-pre-hook' when the value
of `erc-insert-this' is t.

ERC runs this hook with the buffer narrowed to the bounds of the
inserted message plus a trailing newline.  Built-in modules place
their hook members in two depth ranges: the first between -80 and
-20 and the second between 20 and 80.  Use the functions
`erc-find-parsed-property' and `erc-get-parsed-vector' to locate
and extract the `erc-response' object for the inserted message."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-post-hook nil
  "This hook is called just after `erc-insert-modify-hook'.
At this point, all modifications from prior hook functions are done."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-truncate-buffer
             erc-make-read-only
             erc-save-buffer-in-logs))

(defcustom erc-insert-done-hook nil
  "This hook is called after inserting strings into the buffer.
This hook is not called from inside `save-excursion' and should
preserve point if needed."
  :group 'erc-hooks
  :version "27.1"
  :type 'hook)

(defcustom erc-send-modify-hook nil
  "Sending hook for functions that will change the text's appearance.
ERC runs this just after `erc-pre-send-functions' if its shared
`erc-input' object's `sendp' and `insertp' slots remain non-nil.
While this hook is run, narrowing is in effect and `current-buffer' is
the buffer where the text got inserted.

Note that no function in this hook can change the appearance of the
text that is sent.  Only changing the sent text's appearance on the
sending user's screen is possible.  One possible value to add here
is `erc-fill'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-post-hook nil
  "This hook is called just after `erc-send-modify-hook'.
At this point, all modifications from prior hook functions are done.
NOTE: The functions on this hook are called _before_ sending a command
to the server.

This function is called with narrowing, ala `erc-send-modify-hook'."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-make-read-only))

(defcustom erc-send-completed-hook
  (when (fboundp 'emacspeak-auditory-icon)
    (list (byte-compile
           (lambda (_str)
             (emacspeak-auditory-icon 'select-object)))))
  "Hook called after a message has been parsed by ERC.

The single argument to the functions is the unmodified string
which the local user typed."
  :group 'erc-hooks
  :type 'hook)
;; mode-specific tables

(defvar erc-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " syntax-table)
    (modify-syntax-entry ?\\ ".   " syntax-table)
    (modify-syntax-entry ?' "w   " syntax-table)
    ;; Make dabbrev-expand useful for nick names
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    syntax-table)
  "Syntax table used while in ERC mode.")

(defvar erc-mode-abbrev-table nil
  "Abbrev table used while in ERC mode.")
(define-abbrev-table 'erc-mode-abbrev-table ())

(defvar erc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'erc-send-current-line)
    (define-key map "\C-a" #'erc-bol)
    (define-key map [home] #'erc-bol)
    (define-key map "\C-c\C-a" #'erc-bol)
    (define-key map "\C-c\C-b" #'erc-switch-to-buffer)
    (define-key map "\C-c\C-d" #'erc-input-action)
    (define-key map "\C-c\C-e" #'erc-toggle-ctcp-autoresponse)
    (define-key map "\C-c\C-f" #'erc-toggle-flood-control)
    (define-key map "\C-c\C-i" #'erc-invite-only-mode)
    (define-key map "\C-c\C-j" #'erc-join-channel)
    (define-key map "\C-c\C-n" #'erc-channel-names)
    (define-key map "\C-c\C-o" #'erc-get-channel-mode-from-keypress)
    (define-key map "\C-c\C-p" #'erc-part-from-channel)
    (define-key map "\C-c\C-q" #'erc-quit-server)
    (define-key map "\C-c\C-r" #'erc-remove-text-properties-region)
    (define-key map "\C-c\C-t" #'erc-set-topic)
    (define-key map "\C-c\C-u" #'erc-kill-input)
    (define-key map "\C-c\C-x" #'erc-quit-server)
    (define-key map "\M-\t" #'ispell-complete-word)
    (define-key map "\t" #'erc-tab)

    ;; Suppress `font-lock-fontify-block' key binding since it
    ;; destroys face properties.
    (define-key map [remap font-lock-fontify-block] #'undefined)

    map)
  "ERC keymap.")

(defun erc--modify-local-map (mode &rest bindings)
  "Modify `erc-mode-map' on behalf of a global module.
Add or remove `key-valid-p' BINDINGS when toggling MODE."
  (declare (indent 1))
  (while (pcase-let* ((`(,key ,def . ,rest) bindings)
                      (existing (keymap-lookup erc-mode-map key)))
           (if mode
               (when (or (not existing) (eq existing #'undefined))
                 (keymap-set erc-mode-map key def))
             (when (eq existing def)
               (keymap-unset erc-mode-map key t)))
           (setq bindings rest))))

;; Faces

; Honestly, I have a horrible sense of color and the "defaults" below
; are supposed to be really bad. But colors ARE required in IRC to
; convey different parts of conversation. If you think you know better
; defaults - send them to me.

;; Now colors are a bit nicer, at least to my eyes.
;; You may still want to change them to better fit your background.-- S.B.

(defgroup erc-faces nil
  "Faces for ERC."
  :group 'erc)

;; FIXME faces should not end in "-face".
(defface erc-default-face '((t))
  "ERC default face."
  :group 'erc-faces)

(defface erc-nick-prefix-face '((t :inherit erc-nick-default-face :weight bold))
  "ERC face used for user mode prefix."
  :group 'erc-faces)

(defface erc-my-nick-prefix-face '((t :inherit erc-nick-default-face :weight bold))
  "ERC face used for my user mode prefix."
  :group 'erc-faces)

(defface erc-direct-msg-face '((t :foreground "IndianRed"))
  "ERC face used for messages you receive in the main erc buffer."
  :group 'erc-faces)

(defface erc-header-line
  '((t :inherit header-line))
  "ERC face used for the header line.

This will only be used if `erc-header-line-face-method' is non-nil."
  :group 'erc-faces)

(defface erc-input-face '((t :foreground "brown"))
  "ERC face used for your input."
  :group 'erc-faces)

(defface erc-prompt-face
  '((t :weight bold :foreground "Black" :background "lightBlue2"))
  "ERC face for the prompt."
  :group 'erc-faces)

(defface erc-notice-face
  '((default :weight bold)
    (((class color) (min-colors 88) (supports :weight semi-bold))
     :weight semi-bold :foreground "SlateBlue")
    (((class color) (min-colors 88)) :foreground "SlateBlue")
    (t :foreground "blue"))
  "ERC face for notices."
  :package-version '(ERC . "5.6")
  :group 'erc-faces)

(defface erc-action-face '((((supports :weight semi-bold)) :weight semi-bold)
                           (t :weight bold))
  "ERC face for actions generated by /ME."
  :package-version '(ERC . "5.6")
  :group 'erc-faces)

(defface erc-error-face '((t :foreground "red"))
  "ERC face for errors."
  :group 'erc-faces)

(defface erc-information '((t :inherit shadow))
  "Face for local administrative messages of low to moderate importance."
  :group 'erc-faces)

;; same default color as `erc-input-face'
(defface erc-my-nick-face '((t :weight bold :foreground "brown"))
  "ERC face for your current nickname in messages sent by you.
See also `erc-show-my-nick'."
  :group 'erc-faces)

(defface erc-nick-default-face '((t :weight bold))
  "ERC nickname default face."
  :group 'erc-faces)

(defface erc-nick-msg-face '((t :weight bold :foreground "IndianRed"))
  "ERC nickname face for private messages."
  :group 'erc-faces)

;; Debugging support

;; FIXME if this variable plays some role, indicate that here.
;; Otherwise, deprecate.
(defvar erc-debug-log-file (expand-file-name "ERC.debug")
  "Debug log file name.")

(defvar-local erc-dbuf nil)

;; See comments in `erc-scenarios-base-local-modules' explaining why
;; this is insufficient as a public interface.

(defvar erc--target-priors nil
  "Analogous to `erc--server-reconnecting' but for target buffers.
Bound to local variables from an existing (logical) session's
buffer during local-module setup and `erc-mode-hook' activation.")

(defmacro erc--restore-initialize-priors (mode &rest vars)
  "Restore local VARS for local minor MODE from a previous session."
  (declare (indent 1))
  (let ((priors (make-symbol "priors"))
        (initp (make-symbol "initp"))
        ;;
        forms)
    (while-let ((k (pop vars)))
      (push `(,k (if ,initp (alist-get ',k ,priors) ,(pop vars))) forms))
    `(let* ((,priors (or erc--server-reconnecting erc--target-priors))
            (,initp (and ,priors (alist-get ',mode ,priors))))
       (unless (local-variable-if-set-p ',mode)
         (error "Not a local minor mode var: %s" ',mode))
       (setq ,@(mapcan #'identity (nreverse forms))))))

(defun erc--target-from-string (string)
  "Construct an `erc--target' variant from STRING."
  (funcall (if (erc-channel-p string)
               (if (erc--valid-local-channel-p string)
                   #'make-erc--target-channel-local
                 #'make-erc--target-channel)
             #'make-erc--target)
           :string string :symbol (intern (erc-downcase string))))

(defun erc-once-with-server-event (event f)
  "Run function F the next time EVENT occurs in the `current-buffer'.

You should make sure that `current-buffer' is a server buffer.

This function temporarily adds a function to EVENT's hook to call F with
two arguments (`proc' and `parsed').  After F is called, the function is
removed from EVENT's hook.  F should return either nil
or t, where nil indicates that the other functions on EVENT's hook
should be run too, and t indicates that other functions should
not be run.

Please be sure to use this function in server-buffers.  In
channel-buffers it may not work at all, as it uses the LOCAL
argument of `add-hook' and `remove-hook' to ensure multiserver
capabilities."
  (unless (erc--server-buffer-p)
    (error
     "You should only run `erc-once-with-server-event' in a server buffer"))
  (let ((fun (make-symbol "fun"))
        (hook (erc-get-hook event)))
    (put fun 'erc-original-buffer (current-buffer))
    (fset fun (lambda (proc parsed)
                (with-current-buffer (get fun 'erc-original-buffer)
                  (remove-hook hook fun t))
                (fmakunbound fun)
                (funcall f proc parsed)))
    (add-hook hook fun -95 t)
    fun))

(defun erc--warn-once-before-connect (mode-var &rest args)
  "Display an \"error notice\" once.
Expect ARGS to be `erc-button--display-error-notice-with-keys'
compatible parameters, except without any leading buffers or processes.
If the current buffer has an `erc-server-process', print the notice
immediately.  Otherwise, if it's a server buffer without a process,
arrange to do so on `erc-connect-pre-hook'.  In non-ERC buffers, so long
as MODE-VAR belongs to a global module, try again at most once the next
time `erc-mode-hook' runs for any connection."
  (declare (indent 1))
  (cl-assert (stringp (car args)))
  (if (derived-mode-p 'erc-mode)
      (unless (or (erc-with-server-buffer ; needs `erc-server-process'
                    (apply #'erc-button--display-error-notice-with-keys
                           (current-buffer) args)
                    t)
                  erc--target) ; unlikely
        (let (hook)
          (setq hook
                (lambda (_)
                  (remove-hook 'erc-connect-pre-hook hook t)
                  (apply #'erc-button--display-error-notice-with-keys args)))
          (add-hook 'erc-connect-pre-hook hook nil t)))
    (when (custom-variable-p mode-var)
      (let (hook)
        (setq hook (lambda ()
                     (remove-hook 'erc-mode-hook hook)
                     (apply #'erc--warn-once-before-connect 'erc-fake args)))
        (add-hook 'erc-mode-hook hook)))))

(defun erc-server-buffer ()
  "Return the server buffer for the current buffer's process.
The buffer-local variable `erc-server-process' is used to find
the process buffer."
  (and (erc-server-buffer-live-p)
       (process-buffer erc-server-process)))

(defun erc-server-buffer-live-p ()
  "Return t if the server buffer has not been killed."
  (and (processp erc-server-process)
       (buffer-live-p (process-buffer erc-server-process))))

(define-obsolete-function-alias
  'erc-server-buffer-p 'erc-server-or-unjoined-channel-buffer-p "30.1")
(defun erc-server-or-unjoined-channel-buffer-p (&optional buffer)
  "Return non-nil if argument BUFFER is an ERC server buffer.
If BUFFER is nil, use the current buffer.  For historical
reasons, also return non-nil for channel buffers the client has
parted or been kicked from."
  (with-current-buffer (or buffer (current-buffer))
    (and (eq major-mode 'erc-mode)
         (null (erc-default-target)))))

(defun erc--server-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is an ERC server buffer.
Without BUFFER, use the current buffer."
  (if buffer
      (with-current-buffer buffer
        (and (eq major-mode 'erc-mode) (null erc--target)))
    (and (eq major-mode 'erc-mode) (null erc--target))))

(defun erc-open-server-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is an ERC server buffer with an open IRC process.

If BUFFER is nil, the current buffer is used."
  (and (erc--server-buffer-p buffer)
       (erc-server-process-alive buffer)))

(defun erc-query-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is an ERC query buffer.
If BUFFER is nil, use the current buffer."
  (and-let* ((target (if buffer
                         (progn (when (stringp buffer)
                                  (setq buffer (get-buffer buffer)))
                                (buffer-local-value 'erc--target buffer))
                       erc--target)))
    (not (erc--target-channel-p target))))

(defun erc-ison-p (nick)
  "Return non-nil if NICK is online."
  (interactive "sNick: ")
  (erc-with-server-buffer
    (let ((erc-online-p 'unknown))
      (erc-once-with-server-event
       303
       (lambda (_proc parsed)
         (let ((ison (split-string (aref parsed 3))))
           (setq erc-online-p (car (erc-member-ignore-case nick ison)))
           t)))
      (erc-server-send (format "ISON %s" nick))
      (while (eq erc-online-p 'unknown) (accept-process-output))
      (if (called-interactively-p 'interactive)
          (message "%s is %sonline"
                   (or erc-online-p nick)
                   (if erc-online-p "" "not "))
        erc-online-p))))



;; Last active buffer, to print server messages in the right place

(defvar-local erc-active-buffer nil
  "The current active buffer, the one where the user typed the last command.
Defaults to the server buffer, and should only be set in the
server buffer.")

(defun erc-active-buffer ()
  "Return the value of `erc-active-buffer' for the current server.
Defaults to the server buffer."
  (erc-with-server-buffer
    (if (buffer-live-p erc-active-buffer)
        erc-active-buffer
      (setq erc-active-buffer (current-buffer)))))

(defun erc-set-active-buffer (buffer)
  "Set the value of `erc-active-buffer' to BUFFER."
  (cond ((erc-server-buffer)
         (with-current-buffer (erc-server-buffer)
           (setq erc-active-buffer buffer)))
        (t (setq erc-active-buffer buffer))))

;; Mode activation routines

(define-derived-mode erc-mode fundamental-mode "ERC"
  "Major mode for Emacs IRC."
  :interactive nil
  (setq local-abbrev-table erc-mode-abbrev-table)
  (setq-local next-line-add-newlines nil)
  (setq line-move-ignore-invisible t)
  (setq-local paragraph-separate
              (concat "\C-l\\|\\(^" (regexp-quote (erc-prompt)) "\\)"))
  (setq-local paragraph-start
              (concat "\\(" (regexp-quote (erc-prompt)) "\\)"))
  (setq-local completion-ignore-case t)
  (add-hook 'post-command-hook #'erc-check-text-conversion nil t)
  (add-hook 'kill-buffer-hook #'erc-kill-buffer-function nil t)
  (add-hook 'completion-at-point-functions #'erc-complete-word-at-point nil t)
  (add-function :before (local 'erc--clear-function)
                #'erc--skip-past-headroom-on-clear '((depth . 30))))

;; activation

(defconst erc-default-server "irc.libera.chat"
  "IRC server to use if it cannot be detected otherwise.")

(defvar erc-default-port 6667
  "IRC port to use if it cannot be detected otherwise.")

(defconst erc-default-port-tls 6697
  "IRC port to use for encrypted connections if it cannot be \
detected otherwise.")

(defconst erc--buffer-display-choices
  `(choice (const :tag "Use value of `erc-buffer-display'" nil)
           (const :tag "Split window and select" window)
           (const :tag "Split window but don't select" window-noselect)
           (const :tag "New frame" frame)
           (const :tag "Don't display" bury)
           (const :tag "Use current window" buffer)
           (choice :tag "Defer to a display function"
                   (function-item display-buffer)
                   (function-item pop-to-buffer)
                   (function :tag "User-defined")))
  "Common choices for buffer-display options.")

(defvaralias 'erc-join-buffer 'erc-buffer-display)
(defcustom erc-buffer-display 'bury
  "How to display a newly created ERC buffer.
This determines ERC's baseline, \"catch-all\" buffer-display
behavior.  It takes a backseat to more specific options, like
`erc-interactive-display', `erc-auto-reconnect-display', and
`erc-receive-query-display'.

The available choices are:

  `window'          - in another window,
  `window-noselect' - in another window, but don't select that one,
  `frame'           - in another frame,
  `bury'            - bury it in a new buffer,
  `buffer'          - in place of the current buffer,
  DISPLAY-FUNCTION  - a `display-buffer'-like function

Here, DISPLAY-FUNCTION should accept a buffer and an ACTION of
the kind described by the Info node `(elisp) Choosing Window'.
At times, ERC may add hints about the calling context to the
ACTION's alist.  Keys are symbols such as user options, like
`erc-buffer-display', or module minor modes, like
`erc-autojoin-mode'.  Values are non-nil constants specific to
each.  For this particular option, possible values include the
symbols

  `JOIN', `PRIVMSG', `NOTICE', `erc', and `erc-tls'.

The first three signify IRC commands received from the server and
the rest entry-point commands responsible for the connection.
When dealing with the latter two, users may prefer to set this
option to `bury' and instead call DISPLAY-FUNCTION directly
on (server) buffers returned by these entry points because the
context leading to their creation is plainly obvious.  For
additional details, see the Info node `(erc) display-buffer'.

Note that when the selected window already shows the current
buffer, ERC pretends this option's value is `bury' unless the
variable `erc-skip-displaying-selected-window-buffer' is nil or
the value of this option is DISPLAY-FUNCTION."
  :package-version '(ERC . "5.5")
  :group 'erc-buffers
  :type (cons 'choice (nthcdr 2 erc--buffer-display-choices)))

(defvaralias 'erc-query-display 'erc-interactive-display)
(defcustom erc-interactive-display 'window
  "How to display buffers as a result of user interaction.
This affects commands like /QUERY and /JOIN when issued
interactively at the prompt.  It does not apply when calling a
handler for such a command, like `erc-cmd-JOIN', from Lisp code.
See `erc-buffer-display' for a full description of available
values.

When the value is a user-provided function, ERC may inject a hint
about the invocation context as an extra item in the \"action
alist\" included as part of the second argument.  The item's key
is the symbol `erc-interactive-display' and its value one of

  `/QUERY', `/JOIN', `/RECONNECT', `url', `erc', or `erc-tls'.

All are symbols indicating an inciting user action, such as the
issuance of a slash command, the clicking of a URL hyperlink, or
the invocation of an entry-point command.  See Info node `(erc)
display-buffer' for more."
  :package-version '(ERC . "5.6")
  :group 'erc-buffers
  :type erc--buffer-display-choices)

(defvaralias 'erc-reconnect-display 'erc-auto-reconnect-display)
(defcustom erc-auto-reconnect-display nil
  "How to display a channel buffer when automatically reconnecting.
ERC ignores this option when a user issues a /RECONNECT or
successfully reinvokes `erc-tls' with similar arguments to those
from the prior connection.  See `erc-buffer-display' for a
description of possible values.

When the value is function, ERC may inject a hint about the
calling context as an extra item in the alist making up the tail
of the second, \"action\" argument.  The item's key is the symbol
`erc-auto-reconnect-display' and its value something non-nil."
  :package-version '(ERC . "5.5")
  :group 'erc-buffers
  :type erc--buffer-display-choices)

(defcustom erc-auto-reconnect-display-timeout 10
  "Duration `erc-auto-reconnect-display' remains active.
The countdown starts on MOTD and is canceled early by any
\"slash\" command."
  :package-version '(ERC . "5.6")
  :type 'integer
  :group 'erc-buffers)

(defcustom erc-reconnect-display-server-buffers nil
  "Apply buffer-display options to server buffers when reconnecting.
By default, ERC does not consider `erc-auto-reconnect-display'
for server buffers when automatically reconnecting, nor does it
consider `erc-interactive-display' when users issue a /RECONNECT.
Enabling this tells ERC to always display server buffers
according to those options."
  :package-version '(ERC . "5.6")
  :type 'boolean
  :group 'erc-buffers)

(defcustom erc-frame-alist nil
  "Alist of frame parameters for creating erc frames.
A value of nil means to use `default-frame-alist'."
  :group 'erc-buffers
  :type '(repeat (cons :format "%v"
                       (symbol :tag "Parameter")
                       (sexp :tag "Value"))))

(defcustom erc-frame-dedicated-flag nil
  "Non-nil means the erc frames are dedicated to that buffer.
This only has effect when `erc-join-buffer' is set to `frame'."
  :group 'erc-buffers
  :type 'boolean)

(defcustom erc-reuse-frames t
  "Determines whether new frames are always created.
Non-nil means only create a frame for undisplayed buffers.  Nil
means always create a new frame.  Regardless of its value, ERC
ignores this option unless `erc-join-buffer' is `frame'.  And
like most options in the `erc-buffer' customize group, this has
no effect on server buffers while reconnecting because ERC always
buries those."
  :group 'erc-buffers
  :type 'boolean)

(defvar erc--fallback-channel-prefixes "#&"
  "Prefix chars for distinguishing channel targets when CHANTYPES is unknown.")

(defun erc-channel-p (target)
  "Return non-nil if TARGET is a valid channel name or a channel buffer."
  (cond ((stringp target)
         (and-let*
             (((not (string-empty-p target)))
              (value (let ((entry (erc--get-isupport-entry 'CHANTYPES)))
                       (if entry (cadr entry) erc--fallback-channel-prefixes)))
              ((erc--strpos (aref target 0) value)))))
        ((and-let* (((buffer-live-p target))
                    (target (buffer-local-value 'erc--target target))
                    ((erc--target-channel-p target)))))))

;; For the sake of compatibility, a historical quirk concerning this
;; option, when nil, has been preserved: all buffers are suffixed with
;; the original dialed host name, which is usually something like
;; irc.libera.chat.  Collisions are handled by adding a uniquifying
;; numeric suffix of the form <N>.  Note that channel reassociation
;; behavior involving this option (when nil) was inverted in 28.1 (ERC
;; 5.4 and 5.4.1).  This was regrettable and has since been undone.

(defcustom erc-reuse-buffers t
  "If nil, create new buffers on joining a channel/query.
If non-nil, a new buffer will only be created when you join
channels with same names on different servers, or have query buffers
open with nicks of the same name on different servers.  Otherwise,
the existing buffers will be reused."
  :group 'erc-buffers
  :type 'boolean)

(make-obsolete-variable 'erc-reuse-buffers
                        "old behavior when t now permanent" "29.1")

(defun erc-normalize-port (port)
  "Normalize known PORT specifications to an integer.
Expect PORT to be an integer, a string, or a symbol to coerce into a
standardized form for the express purpose of equality comparisons.  If
PORT is an IANA recognized service, return its numeric mapping.  Do the
same for a few traditional but nonstandard names.  Return nil in
pathological cases."
  ;; These were updated somewhat in 2022 to reflect modern standards
  ;; and practices.  See also:
  ;;
  ;; https://datatracker.ietf.org/doc/html/rfc7194#section-1
  ;; https://www.iana.org/assignments/service-names-port-numbers
  (cond
   ((symbolp port)
    (and port (erc-normalize-port (symbol-name port))))
   ((stringp port)
    (let ((port-nr (string-to-number port)))
      (cond
       ((> port-nr 0)
        port-nr)
       ((string-equal port "irc")
        194)
       ((string-equal port "ircs")
        994)
       ((string-equal port "ircu") 6665)
       ((string-equal port "ircu-2") 6666)
       ((string-equal port "ircu-3") 6667)
       ((string-equal port "ircu-4") 6668)
       ((string-equal port "ircu-5") 6669)
       ((string-equal port "ircd") ; nonstandard (irc-serv is 529)
        6667)
       ((string-equal port "ircs-u") 6697)
       ((string-equal port "ircd-dalnet")
        7000)
       ((string-empty-p port) nil)
       (t
        0))))
   ((numberp port)
    port)
   (t
    nil)))

(defun erc-port-equal (a b)
  "Check whether ports A and B are equal."
  (= (erc-normalize-port a) (erc-normalize-port b)))

(defun erc-generate-new-buffer-name (server port target &optional tgt-info id)
  "Determine the name of an ERC buffer.
When TGT-INFO is nil, assume this is a server buffer.  If ID is non-nil,
return ID as a string unless a buffer already exists with a live server
process, in which case signal an error.  When ID is nil, return a
temporary name based on SERVER and PORT to be replaced with the network
name when discovered (see `erc-networks--rename-server-buffer').  Allow
either SERVER or PORT (but not both) to be nil to accommodate oddball
`erc-server-connect-function's.

When TGT-INFO is non-nil, expect its string field to match the redundant
param TARGET (retained for compatibility).  Whenever possible, prefer
returning TGT-INFO's string unmodified.  But when a case-insensitive
collision prevents that, return target@ID when ID is non-nil or
target@network otherwise after renaming the conflicting buffer in the
same manner."
  (when target ; compat
    (setq tgt-info (erc--target-from-string target)))
  (if tgt-info
      (let* ((esid (and erc-networks--id
                        (erc-networks--id-symbol erc-networks--id)))
             (name (if esid
                       (erc-networks--reconcile-buffer-names tgt-info
                                                             erc-networks--id)
                     (erc--target-string tgt-info))))
        (if (and esid (with-suppressed-warnings ((obsolete erc-reuse-buffers))
                        erc-reuse-buffers))
            name
          (generate-new-buffer-name name)))
    (if (and (with-suppressed-warnings ((obsolete erc-reuse-buffers))
               erc-reuse-buffers)
             id)
        (let ((string (erc-networks--id-string (erc-networks--id-create id))))
          (when-let* ((buf (get-buffer string))
                      ((erc-server-process-alive buf)))
            (user-error  "Session with ID %S already exists" string))
          string)
      (generate-new-buffer-name (if (and server port)
                                    (if (with-suppressed-warnings
                                            ((obsolete erc-reuse-buffers))
                                          erc-reuse-buffers)
                                        (format "%s:%s" server port)
                                      (format "%s:%s/%s" server port server))
                                  (or server port))))))

(defun erc-get-buffer-create (server port target &optional tgt-info id)
  "Create a new buffer based on the arguments."
  (when target ; compat
    (setq tgt-info (erc--target-from-string target)))
  (if (and erc--server-reconnecting
           (not tgt-info)
           (with-suppressed-warnings ((obsolete erc-reuse-buffers))
             erc-reuse-buffers))
      (current-buffer)
    (get-buffer-create
     (erc-generate-new-buffer-name server port nil tgt-info id))))

(defun erc-member-ignore-case (string list)
  "Return non-nil if STRING is a member of LIST.

All strings are compared according to IRC protocol case rules, see
`erc-downcase'."
  (setq string (erc-downcase string))
  (catch 'result
    (while list
      (if (string= string (erc-downcase (car list)))
          (throw 'result list)
        (setq list (cdr list))))))

(defun erc-get-buffer (target &optional proc)
  "Return the buffer matching TARGET in the process PROC.
Without PROC, search all ERC buffers.  For historical reasons,
skip buffers for channels the client has \"PART\"ed or from which
it's been \"KICK\"ed.  Expect users to use a different function
for finding targets independent of \"JOIN\"edness."
  (let ((downcased-target (erc-downcase target)))
    (catch 'buffer
      (erc-buffer-filter
       (lambda ()
         (let ((current (erc-default-target)))
           (and (stringp current)
                (string-equal downcased-target (erc-downcase current))
                (throw 'buffer (current-buffer)))))
       proc))))

(defun erc--buffer-p (buf predicate proc)
  (with-current-buffer buf
    (and (derived-mode-p 'erc-mode)
	 (or (not proc)
	     (eq proc erc-server-process))
	 (funcall predicate)
	 buf)))

(defun erc-buffer-filter (predicate &optional proc)
  "Return a list of `erc-mode' buffers matching certain criteria.
Call PREDICATE without arguments in all ERC buffers or only those
belonging to a non-nil PROC.  Expect it to return non-nil in
buffers that should be included in the returned list.

PROC is either an `erc-server-process', identifying a certain
server connection, or nil which means all open connections."
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
               (when (buffer-live-p buf)
		 (erc--buffer-p buf predicate proc)))
             (buffer-list)))))

(defalias 'erc-buffer-do 'erc-buffer-filter
  "Call FUNCTION in all ERC buffers or only those for PROC.
Expect to be preferred over `erc-buffer-filter' in cases where
the return value goes unused.

\(fn FUNCTION &optional PROC)")

(defun erc-buffer-list (&optional predicate proc)
  "Return a list of ERC buffers.
PREDICATE is a function which executes with every buffer satisfying
the predicate.  If PREDICATE is passed as nil, return a list of all ERC
buffers.  If PROC is given, the buffers local variable `erc-server-process'
needs to match PROC."
  (erc-buffer-filter (or predicate #'always) proc))

(define-obsolete-function-alias 'erc-iswitchb #'erc-switch-to-buffer "25.1")
(defun erc--switch-to-buffer (&optional arg)
  (read-buffer "Switch to ERC buffer: "
	       (when (boundp 'erc-modified-channels-alist)
		 (buffer-name (caar (last erc-modified-channels-alist))))
	       t
	       ;; Only allow ERC buffers in the same session.
	       (let ((proc (unless arg erc-server-process)))
		 (lambda (bufname)
		   (let ((buf (if (consp bufname)
				  (cdr bufname) (get-buffer bufname))))
                     (and buf (erc--buffer-p buf (lambda () t) proc)))))))
(defun erc-switch-to-buffer (&optional arg)
  "Prompt for an ERC buffer to switch to.
When invoked with prefix argument, use all ERC buffers.  Without
prefix ARG, allow only buffers related to same session server.
If `erc-track-mode' is in enabled, put the last element of
`erc-modified-channels-alist' in front of the buffer list."
  (interactive "P")
  (switch-to-buffer (erc--switch-to-buffer arg)))
(defun erc-switch-to-buffer-other-window (&optional arg)
  "Prompt for an ERC buffer to switch to in another window.
When invoked with prefix argument, use all ERC buffers.  Without
prefix ARG, allow only buffers related to same session server.
If `erc-track-mode' is in enabled, put the last element of
`erc-modified-channels-alist' in front of the buffer list."
  (interactive "P")
  (switch-to-buffer-other-window (erc--switch-to-buffer arg)))

(defun erc-channel-list (proc)
  "Return a list of channel buffers.
PROC is the process for the server connection.  If PROC is nil, return
all channel buffers on all servers."
  (erc-buffer-filter
   (lambda ()
     (and (erc-default-target)
          (erc-channel-p (erc-default-target))))
   proc))

(defun erc-buffer-list-with-nick (nick proc)
  "Return buffers containing NICK in the `erc-channel-users' list."
  (with-current-buffer (process-buffer proc)
    (let ((user (gethash (erc-downcase nick) erc-server-users)))
      (if user
          (erc-server-user-buffers user)
        nil))))

(defun erc--query-list ()
  "Return all query buffers for the current connection."
  (erc-buffer-list #'erc-query-buffer-p erc-server-process))

;; Some local variables

;; TODO eventually deprecate this variable
;;
;; In the ancient, pre-CVS days (prior to June 2001), this list may
;; have been used for supporting the changing of a buffer's target on
;; the fly (mid-session).  Such usage, which allowed cons cells like
;; (QUERY . bob) to serve as the list's head, was either never fully
;; integrated or was partially clobbered prior to the introduction of
;; version control.  But vestiges remain (see `erc-dcc-chat-mode').
;; And despite appearances, no evidence has emerged that ERC ever
;; supported one-to-many target buffers.  If such a thing was aspired
;; to, it was never realized.
;;
;; New library code should use the `erc--target' struct instead.
;; Third-party code can continue to use this and `erc-default-target'.
(defvar-local erc-default-recipients nil
  "List of default recipients of the current buffer.")

(defvar-local erc-channel-user-limit nil
  "Limit of users per channel.")

(defvar-local erc-channel-key nil
  "Key needed to join channel.")

(defvar-local erc-invitation nil
  "Last invitation channel.")

(defvar-local erc-away nil
  "Non-nil indicates that we are away.

Use `erc-away-time' to access this if you might be in a channel
buffer rather than a server buffer.")

(defvar-local erc-channel-list nil
  "Server channel list.")

(defvar-local erc-bad-nick nil
  "Non-nil indicates that we got a `nick in use' error while connecting.")

(defvar-local erc-logged-in nil
  "Non-nil indicates that we are logged in.")

(defvar-local erc-default-nicks nil
  "The local copy of `erc-nick' - the list of nicks to choose from.")

(defvar-local erc-nick-change-attempt-count 0
  "Used to keep track of how many times an attempt at changing nick is made.")

(defun erc-migrate-modules (mods)
  "Migrate old names of ERC modules to new ones."
  ;; modify `transforms' to specify what needs to be changed
  ;; each item is in the format '(old . new)
  (delete-dups (mapcar #'erc--normalize-module-symbol mods)))

(defun erc--sort-modules (modules)
  "Return a copy of MODULES, deduped and led by sorted built-ins."
  (let (built-in third-party)
    (dolist (mod modules)
      (setq mod (erc--normalize-module-symbol mod))
      (cl-pushnew mod (if (get mod 'erc--module) built-in third-party)))
    `(,@(sort built-in #'string-lessp) ,@(nreverse third-party))))

(defcustom erc-modules '( autojoin button completion fill imenu irccontrols
                          list match menu move-to-prompt netsplit
                          networks readonly ring stamp track)
  "Modules to enable while connecting.
When modifying this option in Lisp code, use a Custom-friendly
facilitator, like `setopt', or call `erc-update-modules'
afterward.  This ensures a consistent ordering and disables
removed modules.  It also gives packages access to the hook
`erc-before-connect'."
  :get (lambda (sym)
         ;; replace outdated names with their newer equivalents
         (erc-migrate-modules (symbol-value sym)))
  ;; Expect every built-in module to have the symbol property
  ;; `erc--module' set to its canonical symbol (often itself).
  :initialize (lambda (symbol exp)
                ;; Use `cdddr' because (set :greedy t . ,entries)
                (dolist (entry (cdddr (get 'erc-modules 'custom-type)))
                  (when-let* (((eq (car entry) 'const))
                              (s (cadddr entry))) ; (const :tag "..." ,s)
                    (put s 'erc--module s)))
                (custom-initialize-reset symbol exp))
  :set (lambda (sym val)
         ;; disable modules which have just been removed
         (when (and (boundp 'erc-modules) erc-modules val)
           (dolist (module erc-modules)
             (unless (memq module val)
               (let ((f (intern-soft (format "erc-%s-mode" module))))
                 (when (and (fboundp f) (boundp f))
                   (when (symbol-value f)
                     (message "Disabling `erc-%s'" module)
                     (funcall f 0))
                   ;; Disable local module in all ERC buffers.
                   (unless (or (custom-variable-p f)
                               (not (fboundp 'erc-buffer-filter)))
                     (erc-buffer-filter (lambda ()
                                          (when (symbol-value f)
                                            (funcall f 0))
                                          (kill-local-variable f)))))))))
         ;; Calling `set-default-toplevel-value' complicates testing.
         (set sym (erc--sort-modules val))
         ;; Don't initialize modules on load, even though the rare
         ;; third-party module may need it.
         (when (fboundp 'erc-update-modules)
           (unless erc--inside-mode-toggle-p
             (erc-update-modules))))
  :type
  '(set
    :greedy t
    (const :tag "autoaway: Set away status automatically" autoaway)
    (const :tag "autojoin: Join channels automatically" autojoin)
    (const :tag "bufbar: Show ERC buffers in a side window" bufbar)
    (const :tag "button: Buttonize URLs, nicknames, and other text" button)
    (const :tag "capab: Mark unidentified users on servers supporting CAPAB"
           capab-identify)
    (const :tag "command-indicator: Echo command lines." command-indicator)
    (const :tag "completion: Complete nicknames and commands (programmable)"
           completion)
    (const :tag "dcc: Provide Direct Client-to-Client support" dcc)
    (const :tag "fill: Wrap long lines" fill)
    (const :tag "identd: Launch an identd server on port 8113" identd)
    (const :tag "imenu: A simple Imenu integration" imenu)
    (const :tag "irccontrols: Highlight or remove IRC control characters"
           irccontrols)
    (const :tag "keep-place: Leave point above un-viewed text" keep-place)
    (const :tag "list: List channels in a separate buffer" list)
    (const :tag "log: Save buffers in logs" log)
    (const :tag "match: Highlight pals, fools, and other keywords" match)
    (const :tag "menu: Display a menu in ERC buffers" menu)
    (const :tag "move-to-prompt: Move to the prompt when typing text"
           move-to-prompt)
    (const :tag "netsplit: Detect netsplits" netsplit)
    (const :tag "networks: Provide data about IRC networks" networks)
    (const :tag "nickbar: Show nicknames in a dynamic side window" nickbar)
    (const :tag "nicks: Uniquely colorize nicknames in target buffers" nicks)
    (const :tag "noncommands: Deprecated. See module `command-indicator'."
           noncommands)
    (const :tag "notifications: Desktop alerts on PRIVMSG or mentions"
           notifications)
    (const :tag
           "notify: Notify when the online status of certain users changes"
           notify)
    (const :tag "page: Process CTCP PAGE requests from IRC" page)
    (const :tag "readonly: Make displayed lines read-only" readonly)
    (const :tag "replace: Replace text in messages" replace)
    (const :tag "ring: Enable an input history" ring)
    (const :tag "sasl: Enable SASL authentication" sasl)
    (const :tag "scrolltobottom: Scroll to the bottom of the buffer"
           scrolltobottom)
    (const :tag "services: Identify to Nickserv (IRC Services) automatically"
           services)
    (const :tag "smiley: Convert smileys to pretty icons" smiley)
    (const :tag "sound: Play sounds when you receive CTCP SOUND requests"
           sound)
    (const :tag "spelling: Check spelling" spelling)
    (const :tag "stamp: Add timestamps to messages" stamp)
    (const :tag "track: Track channel activity in the mode-line" track)
    (const :tag "truncate: Truncate buffers to a certain size" truncate)
    (const :tag "unmorse: Translate morse code in messages" unmorse)
    (const :tag "xdcc: Act as an XDCC file-server" xdcc)
    (repeat :tag "Others" :inline t symbol))
  :package-version '(ERC . "5.6")
  :group 'erc)

(defun erc-update-modules ()
  "Enable minor mode for every module in `erc-modules'.
Except ignore all local modules, which were introduced in ERC 5.5."
  (erc--update-modules erc-modules)
  nil)

(defvar erc--aberrant-modules nil
  "Modules suspected of being improperly loaded.")

(defun erc--warn-about-aberrant-modules ()
  (when (and erc--aberrant-modules (not erc--target))
    (erc-button--display-error-notice-with-keys-and-warn
     "The following modules likely engage in unfavorable loading practices: "
     (mapconcat (lambda (s) (format "`%s'" s)) erc--aberrant-modules ", ")
     ". Please contact ERC with \\[erc-bug] if you believe this to be untrue."
     " See Info:\"(erc) Module Loading\" for more.")
    (setq erc--aberrant-modules nil)))

(defvar erc--requiring-module-mode-p nil
  "Non-nil while doing (require \\='erc-mymod) for `mymod' in `erc-modules'.
Used for inhibiting potentially recursive `erc-update-modules'
invocations by third-party packages.")

(defun erc--find-mode (sym)
  (setq sym (erc--normalize-module-symbol sym))
  (if-let* ((mode (intern-soft (concat "erc-" (symbol-name sym) "-mode")))
            ((and (fboundp mode)
                  (autoload-do-load (symbol-function mode) mode)))
            ((or (get sym 'erc--module)
                 (symbol-file mode)
                 (ignore (cl-pushnew sym erc--aberrant-modules)))))
      mode
    (and (or (and erc--requiring-module-mode-p
                  ;; Also likely non-nil: (eq sym (car features))
                  (cl-pushnew sym erc--aberrant-modules))
             (let ((erc--requiring-module-mode-p t))
               (require (or (get sym 'erc--feature)
                            (intern (concat "erc-" (symbol-name sym))))
                        nil 'noerror))
             (memq sym erc--aberrant-modules))
         (or mode (setq mode (intern-soft (concat "erc-" (symbol-name sym)
                                                  "-mode"))))
         (fboundp mode)
         mode)))

(defun erc--update-modules (modules)
  (let (local-modes)
    (dolist (module modules local-modes)
      (if-let* ((mode (erc--find-mode module)))
          (if (custom-variable-p mode)
              (funcall mode 1)
            (push mode local-modes))
        (error "`%s' is not a known ERC module" module)))))

(defvar erc--updating-modules-p nil
  "Non-nil when running `erc--update-modules' in `erc-open'.
This allows global modules with known or likely dependents (or
some other reason for activating after session initialization) to
conditionally run setup code traditionally reserved for
`erc-mode-hook' in the setup portion of their mode toggle.  Note
that being \"global\", they'll likely want to do so in all ERC
buffers and ensure the code is idempotent.  For example:

  (add-hook \\='erc-mode-hook #\\='erc-foo-setup-fn)
  (unless erc--updating-modules-p
    (erc-with-all-buffers-of-server nil
        (lambda () some-condition-p)
      (erc-foo-setup-fn)))

This means that when a dependent module is initializing and
realizes it's missing some required module \"foo\", it can
confidently call (erc-foo-mode 1) without having to learn
anything about the dependency's implementation.")

(defvar erc--setup-buffer-hook '(erc--warn-about-aberrant-modules)
  "Internal hook for module setup involving windows and frames.")

(defvar erc--display-context nil
  "Extra action alist items passed to `display-buffer'.
Non-nil when a user specifies a custom display action for certain
buffer-display options, like `erc-auto-reconnect-display'.  ERC
pairs the option's symbol with a context-dependent value and adds
the entry to the user-provided alist when calling `pop-to-buffer'
or `display-buffer'.")

(defvar erc-skip-displaying-selected-window-buffer t
  "Whether to forgo showing a buffer that's already being displayed.
But only in the selected window.  This is intended as a crutch
for non-user third-party code that might be slow to adopt the
`display-buffer' function variant available to all buffer-display
options starting in ERC 5.6.  Users with rare requirements, like
wanting to change the window buffer to something other than the
one being processed, should see the Info node `(erc)
display-buffer'.")
(make-obsolete 'erc-show-already-displayed-buffer
               "non-nil behavior to be made permanent" "30.1")

(defvar-local erc--display-buffer-overriding-action nil
  "The value of `display-buffer-overriding-action' when non-nil.
Influences the displaying of new or reassociated ERC buffers.
Reserved for use by built-in modules.")

(defun erc-setup-buffer (buffer)
  "Consults `erc-join-buffer' to find out how to display `BUFFER'."
  (pcase (if (zerop (erc-with-server-buffer
                      erc--server-last-reconnect-count))
             erc-join-buffer
           (or erc-auto-reconnect-display erc-join-buffer))
    ((and (pred functionp) disp-fn (let context erc--display-context))
     (unless (zerop erc--server-last-reconnect-count)
       (push '(erc-auto-reconnect-display . t) context))
     (funcall disp-fn buffer (cons nil context)))
    ((guard (and erc-skip-displaying-selected-window-buffer
                 (eq (window-buffer) buffer))))
    ('window
     (if (active-minibuffer-window)
         (display-buffer buffer)
       (switch-to-buffer-other-window buffer)))
    ('window-noselect
     (display-buffer buffer '(nil (inhibit-same-window . t))))
    ('bury
     nil)
    ('frame
     (when (or (not erc-reuse-frames)
               (not (get-buffer-window buffer t)))
       (let ((frame (make-frame (or erc-frame-alist
                                    default-frame-alist))))
         (raise-frame frame)
         (select-frame frame))
       (switch-to-buffer buffer)
       (when erc-frame-dedicated-flag
         (set-window-dedicated-p (selected-window) t))))
    (_
     (if (active-minibuffer-window)
         (display-buffer buffer)
       (switch-to-buffer buffer)))))

(defun erc--merge-local-modes (new-modes old-vars)
  "Return a cons of two lists, each containing local-module modes.
In the first, put modes to be enabled in a new ERC buffer by
calling their associated functions.  In the second, put modes to
be marked as disabled by setting their associated variables to
nil."
  (if old-vars
      (let ((out (list (reverse new-modes))))
        (pcase-dolist (`(,k . ,v) old-vars)
          (when (and (get k 'erc-module)
                     (string-prefix-p "erc-" (symbol-name k))
                     (string-suffix-p "-mode" (symbol-name k)))
            (if v
                (cl-pushnew k (car out))
              (setf (car out) (delq k (car out)))
              (cl-pushnew k (cdr out)))))
        (cons (nreverse (car out)) (nreverse (cdr out))))
    (list new-modes)))

;; This function doubles as a convenient helper for use in unit tests.
;; Prior to 5.6, its contents lived in `erc-open'.

(defun erc--initialize-markers (old-point continued-session)
  "Ensure prompt and its bounding markers have been initialized."
  ;; FIXME erase assertions after code review and additional testing.
  (setq erc-insert-marker (make-marker)
        erc-input-marker (make-marker))
  (if continued-session
      (progn
        ;; Trust existing markers.
        (set-marker erc-insert-marker
                    (alist-get 'erc-insert-marker continued-session))
        (set-marker erc-input-marker
                    (alist-get 'erc-input-marker continued-session))
        (set-marker-insertion-type erc-insert-marker t)
        (cl-assert (= (field-end erc-insert-marker) erc-input-marker))
        (goto-char old-point)
        (let ((erc--hidden-prompt-overlay
               (alist-get 'erc--hidden-prompt-overlay continued-session)))
          (erc--unhide-prompt)))
    (cl-assert (not (get-text-property (point) 'erc-prompt)))
    ;; In the original version from `erc-open', the snippet that
    ;; handled these newline insertions appeared twice close in
    ;; proximity, which was probably unintended.  Nevertheless, we
    ;; preserve the double newlines here for historical reasons.
    (insert "\n\n")
    (set-marker erc-insert-marker (point))
    (erc-display-prompt)
    (set-marker-insertion-type erc-insert-marker t)
    (cl-assert (= (point) (point-max)))))

(defun erc-open (&optional server port nick full-name
                           connect passwd _tgt-list channel process
                           client-certificate user id)
  "Return a new or reinitialized server or target buffer.
If CONNECT is non-nil, connect to SERVER and return its new or
reassociated buffer.  Otherwise, assume PROCESS is non-nil and belongs
to an active session, and return a new or refurbished target buffer for
CHANNEL, which may also be a query target (the parameter name remains
for historical reasons).  Pass SERVER, PORT, NICK, USER, FULL-NAME, and
PASSWD to `erc-determine-parameters' for preserving as session-local
variables.  Do something similar for CLIENT-CERTIFICATE and ID, which
should be as described by `erc-tls'.

Note that ERC ignores TGT-LIST and initializes `erc-default-recipients'
with CHANNEL as its only member.  Note also that this function has the
side effect of setting the current buffer to the one it returns.  Use
`with-current-buffer' or `save-excursion' to nullify this effect."
  (let* ((target (and channel (erc--target-from-string channel)))
         (buffer (erc-get-buffer-create server port nil target id))
         (old-buffer (current-buffer))
         (erc--target-priors (and target ; buf from prior session
                                  (buffer-local-value 'erc--target buffer)
                                  (buffer-local-variables buffer)))
         (old-recon-count erc-server-reconnect-count)
         (old-point nil)
         (delayed-modules nil)
         (continued-session (or erc--server-reconnecting
                                erc--target-priors
                                (and-let* (((not target))
                                           (m (buffer-local-value
                                               'erc-input-marker buffer))
                                           ((marker-position m)))
                                  (buffer-local-variables buffer)))))
    (when connect (run-hook-with-args 'erc-before-connect server port nick))
    (set-buffer buffer)
    (setq old-point (point))
    (setq delayed-modules
          (erc--merge-local-modes (let ((erc--updating-modules-p t))
                                    (erc--update-modules
                                     (erc--sort-modules erc-modules)))
                                  (or erc--server-reconnecting
                                      erc--target-priors)))

    (delay-mode-hooks (erc-mode))

    (setq erc-server-reconnect-count old-recon-count)

    (when (setq erc-server-connected (not connect))
      (setq erc-server-announced-name
            (buffer-local-value 'erc-server-announced-name old-buffer)))
    ;; connection parameters
    (setq erc-server-process process)
    ;; stack of default recipients
    (when channel (setq erc-default-recipients (list channel)))
    (when target
      (setq erc--target target
            erc-network (erc-network)))
    (setq erc-server-current-nick nil)
    ;; Initialize erc-server-users and erc-channel-users
    (if connect
        (progn ;; server buffer
          (setq erc-server-users
                (make-hash-table :test 'equal))
          (setq erc-channel-users nil))
      (progn ;; target buffer
        (setq erc-server-users nil)
        (setq erc-channel-users
              (make-hash-table :test 'equal))))
    (setq erc-channel-topic "")
    ;; limit on the number of users on the channel (mode +l)
    (setq erc-channel-user-limit nil)
    (setq erc-channel-key nil)
    ;; last active buffer, defaults to this one
    (erc-set-active-buffer buffer)
    ;; last invitation channel
    (setq erc-invitation nil)
    ;; Server channel list
    (setq erc-channel-list ())
    ;; login-time 'nick in use' error
    (setq erc-bad-nick nil)
    ;; whether we have logged in
    (setq erc-logged-in nil)
    ;; The local copy of `erc-nick' - the list of nicks to choose
    (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
    ;; client certificate (only useful if connecting over TLS)
    (setq erc-session-client-certificate client-certificate)
    (setq erc-networks--id
          (if connect
              (or (and erc--server-reconnecting
                       (alist-get 'erc-networks--id erc--server-reconnecting))
                  (and id (erc-networks--id-create id)))
            (buffer-local-value 'erc-networks--id old-buffer)))
    ;; debug output buffer
    (setq erc-dbuf
          (when erc-log-p
            (get-buffer-create (concat "*ERC-DEBUG: " server "*"))))

    (erc--initialize-markers old-point continued-session)
    (erc-determine-parameters server port nick full-name user passwd)
    (save-excursion (run-mode-hooks)
                    (dolist (mod (car delayed-modules))
                      (unless (and (boundp mod) (symbol-value mod))
                        (funcall mod +1)))
                    (dolist (var (cdr delayed-modules)) (set var nil)))

    ;; Saving log file on exit
    (run-hook-with-args 'erc-connect-pre-hook buffer)

    (if connect
        (erc-server-connect erc-session-server
                            (erc-string-to-port erc-session-port)
                            buffer
                            erc-session-client-certificate)
      (erc-update-mode-line))

    ;; Now display the buffer in a window as per user wishes.
    (when (eq buffer old-buffer) (cl-assert (and connect (not target))))
    (unless (and (not erc-reconnect-display-server-buffers)
                 (eq buffer old-buffer))
      (when erc-log-p
        ;; we can't log to debug buffer, it may not exist yet
        (message "erc: old buffer %s, switching to %s"
                 old-buffer buffer))
      (let ((display-buffer-overriding-action
             (or erc--display-buffer-overriding-action
                 display-buffer-overriding-action)))
        (erc-setup-buffer buffer)
        (run-hooks 'erc--setup-buffer-hook)))

    buffer))

(defun erc-initialize-log-marker (buffer)
  "Initialize the `erc-last-saved-position' marker to a sensible position.
BUFFER is the current buffer."
  ;; Note that in 5.6, `erc-input-marker' itself became a "sensible
  ;; position" when its insertion type changed to t.  However,
  ;; decrementing still makes sense for compatibility.
  (with-current-buffer buffer
    (unless (markerp erc-last-saved-position)
      (setq erc-last-saved-position (make-marker))
      (move-marker erc-last-saved-position
		   (1- (marker-position erc-insert-marker))))))

;; interactive startup

(defvar erc-server-history-list nil
  "IRC server interactive selection history list.")

(defvar erc-nick-history-list nil
  "Nickname interactive selection history list.")

(defun erc-already-logged-in (server port nick)
  "Return the buffers corresponding to a NICK on PORT of a session SERVER.
This is determined by looking for the appropriate buffer and checking
whether the connection is still alive.
If no buffer matches, return nil."
  (erc-buffer-list
   (lambda ()
     (and (erc-server-process-alive)
          (string= erc-session-server server)
          (erc-port-equal erc-session-port port)
          (erc-current-nick-p nick)))))

(defcustom erc-before-connect nil
  "Functions called before connecting to a server.
The functions in this variable gets executed before `erc'
actually invokes `erc-mode' with your input data.  The functions
in here get called with three parameters, SERVER, PORT and NICK."
  :group 'erc-hooks
  :type '(repeat function))

(defcustom erc-after-connect nil
  "Abnormal hook run upon establishing a logical IRC connection.
Runs on MOTD's end when `erc-server-connected' becomes non-nil.
ERC calls members with `erc-server-announced-name', falling back
to the 376/422 message's \"sender\", as well as the current nick,
as given by the 376/422 message's \"target\" parameter, which is
typically the same as that reported by `erc-current-nick'."
  :group 'erc-hooks
  :type '(repeat function))

(defun erc--ensure-url (input)
  (unless (string-match (rx bot "irc" (? "6") (? "s") "://") input)
    (when (and (string-match (rx (? (+ nonl) "@")
                                 (or (group (* (not "[")) ":" (* nonl))
                                     (+ nonl))
                                 ":" (+ (not (any ":]"))) eot)
                             input)
               (match-beginning 1))
      (setq input (concat "[" (substring input (match-beginning 1)) "]")))
    (setq input (concat "irc://" input)))
  input)

(defvar erc--prompt-for-server-function nil)

;;;###autoload
(defun erc-select-read-args ()
  "Prompt for connection parameters and return them in a plist.
By default, collect `:server', `:port', `:nickname', and
`:password'.  With a non-nil prefix argument, also prompt for
`:user' and `:full-name'.  Also return various environmental
properties needed by entry-point commands, like `erc-tls'."
  (let* ((input (let ((d (erc-compute-server)))
                  (if erc--prompt-for-server-function
                      (funcall erc--prompt-for-server-function)
                    (read-string (format-prompt "Server or URL" d)
                                 nil 'erc-server-history-list d))))
         ;; For legacy reasons, also accept a URL without a scheme.
         (url (url-generic-parse-url (erc--ensure-url input)))
         (server (url-host url))
         (sp (and (string-suffix-p "s" (url-type url)) erc-default-port-tls))
         (port (or (url-portspec url)
                   (erc-compute-port
                    (let ((d (erc-compute-port sp))) ; may be a string
                      (erc-string-to-port
                       (read-string (format-prompt "Port" d) nil nil d))))))
         ;; Trust the user not to connect twice accidentally.  We
         ;; can't use `erc-already-logged-in' to check for an existing
         ;; connection without modifying it to consider USER and PASS.
         (nick (or (url-user url)
                   (let ((d (erc-compute-nick)))
                     (read-string (format-prompt "Nickname" d)
                                  nil 'erc-nick-history-list d))))
         (user (and current-prefix-arg
                    (let ((d (erc-compute-user (url-user url))))
                      (read-string (format-prompt "User" d)
                                   nil nil d))))
         (full (and current-prefix-arg
                    (let ((d (erc-compute-full-name (url-user url))))
                      (read-string (format-prompt "Full name" d)
                                   nil nil d))))
         (passwd (let* ((p (with-suppressed-warnings ((obsolete erc-password))
                             (or (url-password url) erc-password)))
                        (m (if p
                               (format-prompt "Server password" p)
                             "Server password (optional): ")))
                   (if erc-prompt-for-password (read-passwd m nil p) p)))
         (opener (and (or sp (erc-port-equal port erc-default-port-tls)
                          (and (equal server erc-default-server)
                               (not (string-prefix-p "irc://" input))
                               (erc-port-equal port erc-default-port)
                               (y-or-n-p "Connect using TLS instead? ")
                               (setq port erc-default-port-tls)))
                      #'erc-open-tls-stream))
         env)
    (when erc-interactive-display
      (push `(erc-join-buffer . ,erc-interactive-display) env))
    (when erc--display-context
      (push `(erc--display-context . ,erc--display-context) env))
    (when opener
      (push `(erc-server-connect-function . ,opener) env))
    (when (and passwd (string= "" passwd))
      (setq passwd nil))
    `( :server ,server :port ,port :nick ,nick ,@(and user `(:user ,user))
       ,@(and passwd `(:password ,passwd)) ,@(and full `(:full-name ,full))
       ,@(and env `(--interactive-env-- ,env)))))

(defmacro erc--with-entrypoint-environment (env &rest body)
  "Run BODY with bindings from ENV alist."
  (declare (indent 1))
  (let ((syms (make-symbol "syms"))
        (vals (make-symbol "vals")))
    `(let (,syms ,vals)
       (pcase-dolist (`(,k . ,v) ,env) (push k ,syms) (push v ,vals))
       (cl-progv ,syms ,vals
         ,@body))))

;;;###autoload
(defun erc-server-select ()
  "Interactively connect to a server from `erc-server-alist'."
  (declare (obsolete erc-tls "30.1"))
  (interactive)
  (let ((erc--prompt-for-server-function #'erc-networks--server-select))
    (call-interactively #'erc)))

;;;###autoload
(cl-defun erc (&key (server (erc-compute-server))
                    (port   (erc-compute-port))
                    (nick   (erc-compute-nick))
                    (user   (erc-compute-user))
                    password
                    (full-name (erc-compute-full-name))
                    id
                    ;; Used by interactive form
                    ((--interactive-env-- --interactive-env--)))
  "Connect to an Internet Relay Chat SERVER on a non-TLS PORT.
Use NICK and USER, when non-nil, to inform the IRC commands of
the same name, possibly factoring in a non-nil FULL-NAME as well.
When PASSWORD is non-nil, also send an opening server password
via the \"PASS\" command.  Interactively, prompt for SERVER,
PORT, NICK, and PASSWORD, along with USER and FULL-NAME when
given a prefix argument.  Non-interactively, expect the rarely
needed ID parameter, when non-nil, to be a symbol or a string for
naming the server buffer and identifying the connection
unequivocally.  Once connected, return the server buffer.  (See
Info node `(erc) Connecting' for details about all mentioned
parameters.)

Together with `erc-tls', this command serves as the main entry
point for ERC, the powerful, modular, and extensible IRC client.
Non-interactively, both commands accept the following keyword
arguments, with their defaults supplied by the indicated
\"compute\" functions:

  :server    `erc-compute-server'
  :port      `erc-compute-port'
  :nick      `erc-compute-nick'
  :user      `erc-compute-user'
  :password   N/A
  :full-name `erc-compute-full-name'
  :id'        N/A

For example, when called in the following manner

   (erc :server \"irc.libera.chat\" :full-name \"J. Random Hacker\")

ERC assigns SERVER and FULL-NAME the associated keyword values
and defers to `erc-compute-port', `erc-compute-user', and
`erc-compute-nick' for those respective parameters.

\(fn &key SERVER PORT NICK USER PASSWORD FULL-NAME ID)"
  (interactive (let ((erc--display-context `((erc-interactive-display . erc)
                                             ,@erc--display-context)))
                 (erc-select-read-args)))
  (unless (assq 'erc--display-context --interactive-env--)
    (push '(erc--display-context . ((erc-buffer-display . erc)))
          --interactive-env--))
  (erc--with-entrypoint-environment --interactive-env--
    (erc-open server port nick full-name t password nil nil nil nil user id)))

;;;###autoload
(defalias 'erc-select #'erc)
(defalias 'erc-ssl #'erc-tls)

;;;###autoload
(cl-defun erc-tls (&key (server (erc-compute-server))
                        (port   (let ((erc-default-port erc-default-port-tls))
                                  (erc-compute-port)))
                        (nick   (erc-compute-nick))
                        (user   (erc-compute-user))
                        password
                        (full-name (erc-compute-full-name))
                        client-certificate
                        id
                        ;; Used by interactive form
                        ((--interactive-env-- --interactive-env--)))
  "Connect to an IRC server over a TLS-encrypted connection.
Interactively, prompt for SERVER, PORT, NICK, and PASSWORD, along
with USER and FULL-NAME when given a prefix argument.
Non-interactively, also accept a CLIENT-CERTIFICATE, which should
be a list containing the file name of the certificate's key
followed by that of the certificate itself.  Alternatively,
accept a value of t instead of a list, to tell ERC to query
`auth-source' for the certificate's details.

Example client certificate (CertFP) usage:

    (erc-tls :server \"irc.libera.chat\" :port 6697
             :client-certificate
             \\='(\"/home/bandali/my-key.pem\"
               \"/home/bandali/my-cert.pem\"))

See the alternative entry-point command `erc' as well as Info
node `(erc) Connecting' for a fuller description of the various
parameters, like ID.

\(fn &key SERVER PORT NICK USER PASSWORD FULL-NAME CLIENT-CERTIFICATE ID)"
  (interactive
   (let ((erc-default-port erc-default-port-tls)
         (erc--display-context `((erc-interactive-display . erc-tls)
                                 ,@erc--display-context)))
     (erc-select-read-args)))
  ;; Bind `erc-server-connect-function' to `erc-open-tls-stream'
  ;; around `erc-open' when a non-default value hasn't been specified
  ;; by the user or the interactive form.  And don't bother checking
  ;; for advice, indirect functions, autoloads, etc.
  (unless (or (assq 'erc-server-connect-function --interactive-env--)
              (not (eq erc-server-connect-function #'erc-open-network-stream)))
    (push '(erc-server-connect-function . erc-open-tls-stream)
          --interactive-env--))
  (unless (assq 'erc--display-context --interactive-env--)
    (push '(erc--display-context . ((erc-buffer-display . erc-tls)))
          --interactive-env--))
  (erc--with-entrypoint-environment --interactive-env--
    (erc-open server port nick full-name t password
              nil nil nil client-certificate user id)))

(defun erc-open-tls-stream (name buffer host port &rest parameters)
  "Open an TLS stream to an IRC server.
The process will be given the name NAME, its target buffer will
be BUFFER.  HOST and PORT specify the connection target.
PARAMETERS should be a sequence of keywords and values, per
`open-network-stream'."
  (let ((p (plist-put parameters :type 'tls))
        args)
    (unless (plist-member p :nowait)
      (setq p (plist-put p :nowait t)))
    (setq args `(,name ,buffer ,host ,port ,@p))
    (apply #'open-network-stream args)))

(defun erc-open-socks-tls-stream (name buffer host service &rest parameters)
  "Connect to an IRC server via SOCKS proxy over TLS.
Perform the duties required of an `erc-server-connect-function'
implementer, and return a network process.  Defer to the `socks'
and `gnutls' libraries to make the connection and handle TLS
negotiation.  Expect SERVICE to be a TLS port number and
PARAMETERS to be a possibly empty plist containing items like a
`:client-certificate' pair.  Pass NAME, BUFFER, and HOST directly
to `open-network-stream'.  Beyond that, operate as described in
Info node `(erc) SOCKS', and expect users to \"wrap\" this
function with `let'-bound credentials when necessary, as shown in
the example."
  (require 'gnutls)
  (require 'socks)
  (let ((proc (socks-open-network-stream name buffer host service))
        (cert-info (plist-get parameters :client-certificate)))
    (gnutls-negotiate :process proc
                      :hostname host
                      :keylist (and cert-info (list cert-info)))))

;;; Displaying error messages

(defun erc-error (&rest args)
  "Pass ARGS to `format', and display the result as an error message.
If `debug-on-error' is set to non-nil, then throw a real error with this
message instead, to make debugging easier."
  (if debug-on-error
      (apply #'error args)
    (apply #'message args)
    (beep)))

(defvar erc--warnings-buffer-name nil
  "Name of possibly existing alternate warnings buffer for unit tests.")

(defun erc--lwarn (type level format-string &rest args)
  "Issue a warning of TYPE and LEVEL with FORMAT-STRING and ARGS."
  (let ((message (substitute-command-keys
                  (apply #'format-message format-string args))))
    (display-warning type message level erc--warnings-buffer-name)))

;;; Debugging the protocol

(defvar erc-debug-irc-protocol-time-format "%FT%T.%6N%z "
  "Timestamp format string for protocol logger.")

(defconst erc-debug-irc-protocol-version "2"
  "Protocol log format version number.
This exists to help tooling track changes to the format.

In version 1, everything before and including the first double CRLF is
front matter, which must also be CRLF terminated.  Lines beginning with
three asterisks must be ignored as comments.  Other lines should be
interpreted as email-style headers.  Folding is not supported.  A second
double CRLF, if present, signals the end of a log.  Session resumption
is not supported.  Logger lines must adhere to the following format:
TIMESTAMP PEER-NAME FLOW-INDICATOR IRC-MESSAGE CRLF.  Outgoing messages
are indicated with a >> and incoming with a <<.

In version 2, certain outgoing passwords are replaced by a string
of ten question marks.")

(defvar erc-debug-irc-protocol nil
  "If non-nil, log all IRC protocol traffic to the buffer \"*erc-protocol*\".

The buffer is created if it doesn't exist.

NOTE: If this variable is non-nil, and you kill the only
visible \"*erc-protocol*\" buffer, it will be recreated shortly,
but you won't see it.

WARNING: Do not set this variable directly!  Instead, use the
function `erc-toggle-debug-irc-protocol' to toggle its value.")

(defvar erc--debug-irc-protocol-mask-secrets t
  "Whether to hide secrets in a debug log.
They are still visible on screen but are replaced by question
marks when yanked.")

(defun erc--mask-secrets (string)
  (when-let* ((eot (length string))
              (beg (text-property-any 0 eot 'erc-secret t string))
              (end (text-property-not-all beg eot 'erc-secret t string))
              (sec (substring string beg end)))
    (setq string (concat (substring string 0 beg)
                         (make-string 10 ??)
                         (substring string end eot)))
    (put-text-property beg (+ 10 beg) 'face 'erc-inverse-face string)
    (put-text-property beg (+ 10 beg) 'display sec string))
  string)

(defun erc-log-irc-protocol (string &optional outbound)
  "Append STRING to the buffer *erc-protocol*.

This only has any effect if `erc-debug-irc-protocol' is non-nil.

The buffer is created if it doesn't exist.

If OUTBOUND is non-nil, STRING is being sent to the IRC server and
appears in face `erc-input-face' in the buffer.  Lines must already
contain CRLF endings.  A peer is identified by the most precise label
available, starting with the session ID followed by the server-reported
hostname, and falling back to the dialed <server>:<port> pair.

When capturing logs for multiple peers and sorting them into buckets,
such inconsistent labeling may pose a problem until the MOTD is
received.  Setting a fixed `erc-networks--id' can serve as a
workaround."
  (when erc-debug-irc-protocol
    (let ((esid (if erc-networks--id
                    (erc-networks--id-string erc-networks--id)
                  (or erc-server-announced-name
                      (format "%s:%s" erc-session-server erc-session-port))))
          (ts (when erc-debug-irc-protocol-time-format
                (format-time-string erc-debug-irc-protocol-time-format))))
      (when (and outbound erc--debug-irc-protocol-mask-secrets)
        (setq string (erc--mask-secrets string)))
      (with-current-buffer (get-buffer-create "*erc-protocol*")
        (save-excursion
          (goto-char (point-max))
          (let ((buffer-undo-list t)
                (inhibit-read-only t))
            (insert (if outbound
                        (concat ts esid " >> " string)
                      ;; Cope with multi-line messages
                      (let ((lines (split-string string "[\r\n]+" t))
                            result)
                        (dolist (line lines)
                          (setq result (concat result ts esid
                                               " << " line "\r\n")))
                        result)))))
        (let ((orig-win (selected-window))
              (debug-buffer-window (get-buffer-window (current-buffer) t)))
          (when debug-buffer-window
            (select-window debug-buffer-window)
            (when (= 1 (count-lines (point) (point-max)))
              (goto-char (point-max))
              (recenter -1))
            (select-window orig-win)))))))

(defun erc-toggle-debug-irc-protocol (&optional arg)
  "Toggle the value of `erc-debug-irc-protocol'.

If ARG is non-nil, show the *erc-protocol* buffer."
  (interactive "P")
  (let* ((buf (get-buffer-create "*erc-protocol*")))
    (with-current-buffer buf
      (view-mode-enter)
      (when (null (current-local-map))
        (let ((inhibit-read-only t)
              (msg (list
                    (concat "Version: " erc-debug-irc-protocol-version)
                    (concat "ERC-Version: " erc-version)
                    (concat "Emacs-Version: " emacs-version)
                    (erc-make-notice
                     (concat "This buffer displays all IRC protocol "
                             "traffic exchanged with servers."))
                    (erc-make-notice "Kill it to disable logging.")
                    (erc-make-notice (substitute-command-keys
                                      "Press \\`t' to toggle.")))))
          (insert (string-join msg "\r\n")))
        (use-local-map (make-sparse-keymap))
        (local-set-key (kbd "t") 'erc-toggle-debug-irc-protocol))
      (add-hook 'kill-buffer-hook
                (lambda () (setq erc-debug-irc-protocol nil))
                nil 'local)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (if erc-debug-irc-protocol "\r\n" "")
                (erc-make-notice
                 (format "IRC protocol logging %s at %s"
                         (if erc-debug-irc-protocol "disabled" "enabled")
                         (current-time-string)))
                (if erc-debug-irc-protocol "\r\n" "\r\n\r\n"))))
    (setq erc-debug-irc-protocol (not erc-debug-irc-protocol))
    (if (and arg
             (not (get-buffer-window "*erc-protocol*" t)))
        (display-buffer buf t))
    (message "IRC protocol traffic logging %s (see buffer *erc-protocol*)."
             (if erc-debug-irc-protocol "enabled" "disabled"))))

;;; I/O interface

;; send interface

(defvar erc--send-action-function #'erc--send-action
  "Function to display and send an outgoing CTCP ACTION message.
Called with three arguments: the submitted input, the current
target, and an `erc-server-send' FORCE flag.")

(defun erc-send-action (tgt str &optional force)
  "Send CTCP ACTION information described by STR to TGT."
  (funcall erc--send-action-function tgt str force))

;; Sending and displaying are provided separately to afford modules
;; more flexibility, e.g., to forgo displaying on the way out when
;; expecting the server to echo messages back and/or to associate
;; outgoing messages with IDs generated for `erc--ephemeral'
;; placeholders.
(defun erc--send-action-perform-ctcp (target string force)
  "Send STRING to TARGET, possibly immediately, with FORCE."
  (erc-send-ctcp-message target (format "ACTION %s" string) force))

(defvar erc--use-language-catalog-for-ctcp-action-p nil
  "When non-nil, use `ACTION' entry from language catalog for /ME's.
Otherwise, use `ctcp-action' or `ctcp-action-input' from the
internal `-speaker' catalog.  This is an escape hatch to restore
pre-5.6 behavior for the `font-lock-face' property of incoming
and outgoing \"CTCP ACTION\" messages, whose pre-buttonized state
was a single interval of `erc-input-face' or `erc-action-face'.
Newer modules, like `fill-wrap' and `nicks', are incompatible with
this format style.  If you use this, please ask ERC to expose it
as a public variable via \\[erc-bug] or similar.")

(defun erc--send-action-display (string)
  "Display STRING as an outgoing \"CTCP ACTION\" message.
Propertize the message according to the compatibility flag
`erc--use-language-catalog-for-ctcp-action-p'."
  ;; Allow hooks acting on inserted PRIVMSG and NOTICES to process us.
  (let ((erc--msg-prop-overrides `((erc--ctcp . ACTION)
                                   ,@erc--msg-prop-overrides))
        (nick (erc-current-nick)))
    (if erc--use-language-catalog-for-ctcp-action-p
        (progn (erc--ensure-spkr-prop nick)
               (erc-display-message nil 'input (current-buffer) 'ACTION
                                    ?n (propertize nick 'erc--speaker nick)
                                    ?a string ?u "" ?h ""))
      (let ((erc-current-message-catalog erc--message-speaker-catalog))
        (erc-display-message nil nil (current-buffer) 'ctcp-action-input
                             ?p (erc-get-channel-membership-prefix nick)
                             ?n (erc--speakerize-nick nick) ?m string)))))

(defun erc--send-action (target string force)
  "Display STRING, then send to TARGET as a \"CTCP ACTION\" message."
  (erc--send-action-display string)
  (erc--send-action-perform-ctcp target string force))

;; Display interface

(defun erc--ensure-spkr-prop (nick &optional overrides)
  "Add NICK as `erc--spkr' to the current \"msg props\" environment.
Prefer `erc--msg-props' over `erc--msg-prop-overrides' when both
are available.  Also include any members of the alist OVERRIDES,
when present.  Assume NICK itself to be free of any text props,
and return it."
  (cond (erc--msg-props
         (puthash 'erc--spkr nick erc--msg-props)
         (dolist (entry overrides)
           (puthash (car entry) (cdr entry) erc--msg-props)))
        (erc--msg-prop-overrides
         (setq erc--msg-prop-overrides
               `((erc--spkr . ,nick) ,@overrides ,@erc--msg-prop-overrides))))
  nick)

(defun erc-string-invisible-p (string)
  "Check whether STRING is invisible or not.
I.e. any char in it has the `invisible' property set."
  (text-property-any 0 (length string) 'invisible t string))

(defcustom erc-remove-parsed-property t
  "Whether to remove the erc-parsed text property after displaying a message.

The default is to remove it, since it causes ERC to take up extra
memory.  If you have code that relies on this property, then set
this option to nil.

Note that this option is deprecated because a value of nil is
impractical in prolonged sessions with more than a few channels.
Use `erc-insert-post-hook' or similar and the helper function
`erc-find-parsed-property' and friends to stash the current
`erc-response' object as needed.  And instead of using this for
debugging purposes, try `erc-debug-irc-protocol'."
  :type 'boolean
  :group 'erc)
(make-obsolete-variable 'erc-remove-parsed-property
                        "impractical when non-nil" "30.1")

(define-inline erc--assert-input-bounds ()
  (inline-quote
   (progn (when (and (processp erc-server-process)
                     (eq (current-buffer) (process-buffer erc-server-process)))
            ;; It's believed that these only need syncing immediately
            ;; following the first two insertions in a server buffer.
            (set-marker (process-mark erc-server-process) erc-insert-marker))
          (cl-assert (< erc-insert-marker erc-input-marker))
          (cl-assert (= (field-end erc-insert-marker) erc-input-marker)))))

(defvar erc--merge-prop-behind-p nil
  "When non-nil, put merged prop(s) behind existing.")

(defvar erc--refresh-prompt-hook nil
  "Hook called after refreshing the prompt in the affected buffer.")

(defvar-local erc--inhibit-prompt-display-property-p nil
  "Tell `erc-prompt' related functions to avoid the `display' text prop.
Modules can enable this when needing to reserve the prompt's
display property for some other purpose, such as displaying it
elsewhere, abbreviating it, etc.")

(defconst erc--prompt-properties '( rear-nonsticky t
                                    erc-prompt t ; t or `hidden'
                                    field erc-prompt
                                    front-sticky t
                                    read-only t)
  "Mandatory text properties added to ERC's prompt.")

(defvar erc--refresh-prompt-continue-request nil
  "State flag for refreshing prompt in all buffers.
When the value is zero, functions assigned to the variable
`erc-prompt' can set this to run `erc--refresh-prompt-hook' (1)
or `erc--refresh-prompt' (2) in all buffers of the server.")

(defun erc--refresh-prompt-continue (&optional hooks-only-p)
  "Ask ERC to refresh the prompt in all buffers.
Functions assigned to `erc-prompt' can call this if needing to
recreate the prompt in other buffers as well.  With HOOKS-ONLY-P,
run `erc--refresh-prompt-hook' in other buffers instead of doing
a full refresh."
  (when (and erc--refresh-prompt-continue-request
             (zerop erc--refresh-prompt-continue-request))
    (setq erc--refresh-prompt-continue-request (if hooks-only-p 1 2))))

(defun erc--refresh-prompt ()
  "Re-render ERC's prompt when the option `erc-prompt' is a function."
  (erc--assert-input-bounds)
  (unless (erc--prompt-hidden-p)
    (let ((erc--refresh-prompt-continue-request
           (or erc--refresh-prompt-continue-request 0)))
      (when (functionp erc-prompt)
        (save-excursion
          (goto-char (1- erc-input-marker))
          ;; Avoid `erc-prompt' (the named function), which appends a
          ;; space, and `erc-display-prompt', which propertizes all
          ;; but that space.
          (let ((s (funcall erc-prompt))
                (p (point))
                (erc--merge-prop-behind-p t))
            (erc--merge-prop 0 (length s) 'font-lock-face 'erc-prompt-face s)
            (add-text-properties 0 (length s) erc--prompt-properties s)
            (insert s)
            (delete-region erc-insert-marker p))))
      (run-hooks 'erc--refresh-prompt-hook)
      (when-let* (((> erc--refresh-prompt-continue-request 0))
                  (n erc--refresh-prompt-continue-request)
                  (erc--refresh-prompt-continue-request -1)
                  (b (current-buffer)))
        (erc-with-all-buffers-of-server erc-server-process
            (lambda () (not (eq b (current-buffer))))
          (if (= n 1)
              (run-hooks 'erc--refresh-prompt-hook)
            (erc--refresh-prompt)))))))

(defun erc--check-msg-prop (prop &optional val)
  "Return PROP's value in `erc--msg-props' when populated.
If VAL is a list, return non-nil if PROP's value appears in VAL.  If VAL
is otherwise non-nil, return non-nil if VAL compares `eq' to the stored
value.  Otherwise, return the stored value."
  (and-let* ((erc--msg-props)
             (v (gethash prop erc--msg-props)))
    (if (consp val) (memq v val) (if val (eq v val) v))))

(defun erc--memq-msg-prop (prop needle)
  "Return non-nil if msg PROP's value is a list containing NEEDLE."
  (and-let* ((erc--msg-props)
             (haystack (gethash prop erc--msg-props))
             ((consp haystack)))
    (memq needle haystack)))

(defmacro erc--get-inserted-msg-beg-at (point at-start-p)
  (macroexp-let2* nil ((point point)
                       (at-start-p at-start-p))
    `(or (and ,at-start-p ,point)
         (let ((p (previous-single-property-change ,point 'erc--msg)))
           (cond
            ((and p (= p (1- ,point)) (get-text-property p 'erc--msg)) p)
            (p (1- p))
            ((and (null p)
                  (> ,point (point-min))
                  (get-text-property (1- point) 'erc--msg))
             (1- point)))))))

(defmacro erc--get-inserted-msg-end-at (point at-start-p)
  (macroexp-let2 nil point point
    `(1- (next-single-property-change (if ,at-start-p (1+ ,point) ,point)
                                      'erc--msg nil erc-insert-marker))))

(defun erc--get-inserted-msg-beg (&optional point)
  "Maybe return the start of message in an ERC buffer."
  (erc--get-inserted-msg-beg-at (or point (setq point (point)))
                                (get-text-property point 'erc--msg)))

(defun erc--get-inserted-msg-end (&optional point)
  "Return the end of message in an ERC buffer.
Include any trailing white space before final newline.  Expect
POINT to be less than `erc-insert-marker', and don't bother
considering `erc--insert-marker', for now."
  (erc--get-inserted-msg-end-at (or point (setq point (point)))
                                (get-text-property point 'erc--msg)))

(defun erc--get-inserted-msg-bounds (&optional point)
  "Return bounds of message at POINT in an ERC buffer when found.
Search from POINT, when non-nil, instead of `point'.  Return nil
if not found."
  (let ((at-start-p (get-text-property (or point (setq point (point)))
                                       'erc--msg)))
    (and-let* ((b (erc--get-inserted-msg-beg-at point at-start-p)))
      (cons b (erc--get-inserted-msg-end-at point at-start-p)))))

(defun erc--get-inserted-msg-prop (prop &optional point)
  "Return the value of text property PROP for some message at point."
  (and-let* ((stack-pos (erc--get-inserted-msg-beg (or point (point)))))
    (get-text-property stack-pos prop)))

;; FIXME improve this nascent "message splicing" facility to include a
;; means for modules to adjust inserted messages on either side of the
;; splice position as well as to modify the spliced-in message itself
;; before and after each insertion-related hook runs.  Also add a
;; counterpart to `erc--with-spliced-insertion' for deletions.
(defvar erc--insert-line-splice-function
  #'erc--insert-before-markers-transplanting-hidden
  "Function to handle in-place insertions away from prompt.
Modules that display \"stateful\" messages, where one message's content
depends on prior messages, should advise this locally as needed.")

(defmacro erc--with-spliced-insertion (marker-or-pos &rest body)
  "In BODY, ensure `erc-insert-line' inserts messages at MARKER-OR-POS.
If MARKER-OR-POS is a marker, let it advance normally (and permanently)
with each insertion.  Allow modules to influence insertion by binding
`erc--insert-line-function' to `erc--insert-line-splice-function' around
BODY.  Note that as of ERC 5.6, this macro cannot handle multiple
successive calls to `erc-insert-line' in BODY, such as when replaying
a history backlog."
  (declare (indent 1))
  (let ((marker (make-symbol "marker")))
    `(progn
       (cl-assert (or (= ,marker-or-pos (point-min))
                      (= ?\n (char-before ,marker-or-pos))))
       (cl-assert (null erc--insert-line-function))
       (let* ((,marker (and (not (markerp ,marker-or-pos))
                            (copy-marker ,marker-or-pos)))
              (erc--insert-marker (or ,marker ,marker-or-pos))
              (erc--insert-line-function erc--insert-line-splice-function))
         (prog1 (progn ,@body)
           (when ,marker (set-marker ,marker nil)))))))

(defun erc--traverse-inserted (beg end fn)
  "Visit messages between BEG and END and run FN in narrowed buffer.
If END is a marker, possibly update its position."
  (unless (markerp end)
    (setq end (set-marker (make-marker) (or end erc-insert-marker))))
  (unless (eq end erc-insert-marker)
    (set-marker end (min erc-insert-marker end)))
  (save-excursion
    (goto-char beg)
    (let ((b (if (get-text-property (point) 'erc--msg)
                 (point)
               (next-single-property-change (point) 'erc--msg nil end))))
      (while-let ((b)
                  ((< b end))
                  (e (next-single-property-change (1+ b) 'erc--msg nil end)))
        (save-restriction
          (narrow-to-region b e)
          (funcall fn))
        (setq b e))))
  (unless (eq end erc-insert-marker)
    (set-marker end nil)))

(defvar erc--insert-invisible-as-intangible-p nil
  "When non-nil, ensure certain invisible messages are also intangible.
That is, single out any message inserted via `erc-insert-line'
that lacks a trailing newline but has a t-valued `invisible'
property anywhere along its length, and ensure it's both
`invisible' t and `intangible' t throughout.  Note that this is
merely an escape hatch for accessing aberrant pre-5.6 behavior
that ERC considers a bug because it applies a practice described
as obsolete in the manual, and it does so heavy-handedly.  That
the old behavior only acted when the input lacked a trailing
newline was likely accidental but is ultimately incidental.  See
info node `(elisp) Special Properties' for specifics.  Beware
that this flag and the behavior it restores may disappear at any
time, so if you need them, please let ERC know with \\[erc-bug].")

(defvar erc--insert-line-function nil
  "When non-nil, an `insert'-like function for inserting messages.
Modules, like `fill-wrap', that leave a marker at the beginning of an
inserted message clearly want that marker to advance along with text
inserted at that position.  This can be addressed by binding this
variable to `insert-before-markers' around calls to `display-message'.")

(defvar erc--insert-marker nil
  "Internal override for `erc-insert-marker'.")

(define-obsolete-function-alias 'erc-display-line-1 'erc-insert-line "30.1")
(defun erc-insert-line (string buffer)
  "Insert STRING in an `erc-mode' BUFFER.
When STRING is nil, do nothing.  Otherwise, start off by running
`erc-insert-pre-hook' in BUFFER with `erc-insert-this' bound to
t.  If the latter remains non-nil afterward, insert STRING into
BUFFER, ensuring a trailing newline.  After that, narrow BUFFER
around STRING, along with its final line ending, and run
`erc-insert-modify' and `erc-insert-post-hook', respectively.  In
all cases, run `erc-insert-done-hook' unnarrowed before exiting,
and update positions in `buffer-undo-list'.

In general, expect to be called from a higher-level insertion
function, like `erc-display-message', especially when modules
should consider STRING as a candidate for formatting with
enhancements like indentation, fontification, timestamping, etc.
Otherwise, when called directly, allow built-in modules to ignore
STRING, which may make it appear incongruous in situ (unless
preformatted or anticipated by third-party members of the various
modification hooks)."
  (when string
    (with-current-buffer (or buffer (process-buffer erc-server-process))
      (let (insert-position)
        ;; Initialize ^ below to thwart rogue `erc-insert-pre-hook'
        ;; members that dare to modify the buffer's length.
        (let ((buffer-undo-list t)
              (inhibit-read-only t))
          (unless (string-suffix-p "\n" string)
            (setq string (concat string "\n"))
            (when (and erc--insert-invisible-as-intangible-p
                       (erc-string-invisible-p string))
              (erc-put-text-properties 0 (length string)
                                       '(invisible intangible) string)))
          (erc-log (concat "erc-display-message: " string
                           (format "(%S)" string) " in buffer "
                           (format "%s" buffer)))
          (setq erc-insert-this t)
          (run-hook-with-args 'erc-insert-pre-hook string)
          (setq insert-position (marker-position (or erc--insert-marker
                                                     erc-insert-marker)))
          (if (null erc-insert-this)
              ;; Leave erc-insert-this set to t as much as possible.  Fran
              ;; Litterio <franl> has seen erc-insert-this set to nil while
              ;; erc-send-pre-hook is running, which should never happen.  This
              ;; may cure it.
              (setq erc-insert-this t)
            (save-excursion ;; to restore point in the new buffer
              (save-restriction
                (widen)
                (goto-char insert-position)
                (if erc--insert-line-function
                    (funcall erc--insert-line-function string)
                  (insert string))
                (erc--assert-input-bounds)
                ;; run insertion hook, with point at restored location
                (save-restriction
                  (narrow-to-region insert-position (point))
                  (run-hooks 'erc-insert-modify-hook)
                  (run-hooks 'erc-insert-post-hook)
                  (when erc-remove-parsed-property
                    (remove-text-properties (point-min) (point-max)
                                            '(erc-parsed nil tags nil)))
                  (cl-assert (> (- (point-max) (point-min)) 1))
                  (let ((props (if erc--msg-props
                                   (erc--order-text-properties-from-hash
                                    erc--msg-props)
                                 '(erc--msg unknown))))
                    (add-text-properties (point-min) (1+ (point-min)) props)))
                (erc--refresh-prompt)))))
        (run-hooks 'erc-insert-done-hook)
        (erc-update-undo-list (- (or erc--insert-marker erc-insert-marker
                                     (point-max))
                                 insert-position))))))

(defun erc-update-undo-list (shift)
  ;; Translate buffer positions in buffer-undo-list by SHIFT.
  (unless (or (zerop shift) (atom buffer-undo-list))
    (let ((list buffer-undo-list) elt)
      (while list
        (setq elt (car list))
        (cond ((integerp elt)           ; POSITION
               (cl-incf (car list) shift))
              ((or (atom elt)           ; nil, EXTENT
                   ;; (eq t (car elt))  ; (t . TIME)
                   (markerp (car elt))) ; (MARKER . DISTANCE)
               nil)
              ((integerp (car elt))     ; (BEGIN . END)
               (cl-incf (car elt) shift)
               (cl-incf (cdr elt) shift))
              ((stringp (car elt))      ; (TEXT . POSITION)
               (cl-incf (cdr elt) (* (if (natnump (cdr elt)) 1 -1) shift)))
              ((null (car elt))         ; (nil PROPERTY VALUE BEG . END)
               (let ((cons (nthcdr 3 elt)))
                 (cl-incf (car cons) shift)
                 (cl-incf (cdr cons) shift))))
        (setq list (cdr list))))))

(defvar erc-valid-nick-regexp "[]a-zA-Z^[;\\`_{}|][]^[;\\`_{}|a-zA-Z0-9-]*"
  "Regexp which matches all valid characters in a IRC nickname.")

(defun erc-is-valid-nick-p (nick)
  "Check if NICK is a valid IRC nickname."
  (string-match (concat "\\`" erc-valid-nick-regexp "\\'") nick))

(defun erc--route-insertion (string buffer)
  "Insert STRING in BUFFER.
See `erc-display-message' for acceptable BUFFER types."
  (let (seen msg-props)
    (dolist (buf (cond
                  ((bufferp buffer) (list buffer))
                  ((consp buffer)
                   (setq msg-props erc--msg-props)
                   buffer)
                  ((processp buffer) (list (process-buffer buffer)))
                  ((eq 'all buffer)
                   ;; Hmm, or all of the same session server?
                   (erc-buffer-list nil erc-server-process))
                  ((and-let* (((eq 'active buffer))
                              (b (erc-active-buffer)))
                        (list b)))
                  ((erc-server-buffer-live-p)
                   (list (process-buffer erc-server-process)))
                  (t (list (current-buffer)))))
      (when (buffer-live-p buf)
        (when msg-props
          (setq erc--msg-props (copy-hash-table msg-props)))
        (erc-insert-line string buf)
        (setq seen t)))
    (unless (or seen (null buffer))
      (erc--route-insertion string nil))))

(defun erc-display-line (string &optional buffer)
  "Insert STRING in BUFFER as a plain \"local\" message.
Take pains to ensure modification hooks see messages created by
the old pattern (erc-display-line (erc-make-notice) my-buffer) as
being equivalent to a `erc-display-message' TYPE of `notice'."
  (let ((erc--msg-prop-overrides erc--msg-prop-overrides))
    (when (eq 'erc-notice-face (get-text-property 0 'font-lock-face string))
      (unless (assq 'erc--msg erc--msg-prop-overrides)
        (push '(erc--msg . notice) erc--msg-prop-overrides)))
    (erc-display-message nil nil buffer string)))

(defun erc--insert-admin-message (msg &rest args)
  "Print MSG with ARGS as a local notice.
Inhibit all stamps and buttonizing."
  (let ((erc--msg-prop-overrides `((erc--skip . (stamp track button))
                                   ,@erc--msg-prop-overrides)))
    (apply #'erc-display-message nil '(notice information)
           (current-buffer) msg args)))

(defvar erc--merge-text-properties-p nil
  "Non-nil when `erc-put-text-property' defers to `erc--merge-prop'.")

;; To save space, we could maintain a map of all readable property
;; values and optionally dispense archetypal constants in their place
;; in order to ensure all occurrences of some list (a b) across all
;; text-properties in all ERC buffers are actually the same object.
(defun erc--merge-prop (from to prop val &optional object cache-fn)
  "Combine existing PROP values with VAL between FROM and TO in OBJECT.
For spans where PROP is non-nil, cons VAL onto the existing
value, ensuring a proper list.  Otherwise, just set PROP to VAL.
When VAL is itself a list, prepend its members onto an existing
value.  Call CACHE-FN, when given, with the new value for prop.
It must return a suitable replacement or the same value.  See
also `erc-button-add-face'."
  (let ((old (get-text-property from prop object))
        (pos from)
        (end (next-single-property-change from prop object to))
        new)
    (while (< pos to)
      (setq new (if old
                    ;; Can't `nconc' without more info.
                    (if erc--merge-prop-behind-p
                        `(,@(ensure-list old) ,@(ensure-list val))
                      (if (listp val)
                          (append val (ensure-list old))
                        (cons val (ensure-list old))))
                  val))
      (when cache-fn
        (setq new (funcall cache-fn new)))
      (put-text-property pos end prop new object)
      (setq pos end
            old (get-text-property pos prop object)
            end (next-single-property-change pos prop object to)))))

(defun erc--remove-from-prop-value-list (from to prop val &optional object)
  "Remove VAL from text PROP value between FROM and TO.
If current value is VAL itself, remove the property entirely.
When VAL is a list, act as if this function were called
repeatedly with VAL set to each of VAL's members."
  (let ((old (get-text-property from prop object))
        (pos from)
        (end (next-single-property-change from prop object to))
        new)
    (while (< pos to)
      (when old
        (if (setq new (and (consp old) (if (consp val)
                                           (seq-difference old val)
                                         (remq val old))))
            (put-text-property pos end prop
                               (if (cdr new) new (car new)) object)
          (when (pcase val
                  ((pred consp) (or (consp old) (memq old val)))
                  (_ (if (consp old) (memq val old) (eq old val))))
            (remove-text-properties pos end (list prop nil) object))))
      (setq pos end
            old (get-text-property pos prop object)
            end (next-single-property-change pos prop object to)))))

(defun erc--reserve-important-text-props (beg end plist &optional object)
  "Record text-property pairs in PLIST as important between BEG and END.
Also mark the message being inserted as containing these important props
so modules performing destructive modifications can later restore them.
Expect to run in a narrowed buffer at message-insertion time."
  (when erc--msg-props
    (let ((existing (erc--check-msg-prop 'erc--important-prop-names)))
      (puthash 'erc--important-prop-names (cl-union existing (map-keys plist))
               erc--msg-props)))
  (erc--merge-prop beg end 'erc--important-props plist object))

(defun erc--restore-important-text-props (props &optional beg end)
  "Restore PROPS where recorded in the accessible portion of the buffer.
Expect to run in a narrowed buffer at message-insertion time.  Limit the
effect to the region between buffer positions BEG and END, when non-nil.

Callers should be aware that this function fails if the property
`erc--important-props' has an empty value almost anywhere along the
affected region.  Use the function `erc--remove-from-prop-value-list' to
ensure that props with empty values are excised completely."
  (when-let* ((registered (erc--check-msg-prop 'erc--important-prop-names))
              (present (seq-intersection props registered))
              (b (or beg (point-min)))
              (e (or end (point-max))))
    (while-let
        (((setq b (text-property-not-all b e 'erc--important-props nil)))
         (val (get-text-property b 'erc--important-props))
         (q (next-single-property-change b 'erc--important-props nil e)))
      (while-let ((k (pop val))
                  (v (pop val)))
        (when (memq k present)
          (put-text-property b q k v)))
      (setq b q))))

(defvar erc-legacy-invisible-bounds-p nil
  "Whether to hide trailing rather than preceding newlines.
Beginning in ERC 5.6, invisibility extends from a message's
preceding newline to its last non-newline character.")
(make-obsolete-variable 'erc-legacy-invisible-bounds-p
                        "decremented interval now permanent" "30.1")

(defun erc--insert-before-markers-transplanting-hidden (string)
  "Insert STRING before markers and migrate any `invisible' props.
Expect to be called with `point' at the start of an inserted message,
i.e., one with an `erc--msg' property.  Check the message prop header
for invisibility props advertised via `erc--hide'.  When found, remove
them from the previous newline, and add them to the newline suffixing
the inserted version of STRING."
  (let* ((after (and (not erc-legacy-invisible-bounds-p)
                     (get-text-property (point) 'erc--hide)))
         (before (and after (> (point) (point-min))
                      (get-text-property (1- (point)) 'invisible)))
         (a (and after (ensure-list after)))
         (b (and before (ensure-list before)))
         (new (and before (erc--solo (cl-intersection b a)))))
    (when new
      (erc--remove-from-prop-value-list (1- (point)) (point) 'invisible a))
    (prog1 (insert-before-markers string)
      (when new
        (erc--merge-prop (1- (point)) (point) 'invisible new)))))

(defun erc--hide-message (value)
  "Apply `invisible' text-property with VALUE to current message.
Expect to run in a narrowed buffer during message insertion.
Begin the invisible interval at the previous message's trailing
newline and end before the current message's.  If the preceding
message ends in a double newline or there is no previous message,
don't bother including the preceding newline.  Additionally,
record VALUE as part of the `erc--hide' property in the
\"msg-props\" header."
  (if erc-legacy-invisible-bounds-p
      ;; Before ERC 5.6, this also used to add an `intangible'
      ;; property, but the docs say it's now obsolete.
      (erc--merge-prop (point-min) (point-max) 'invisible value)
    (let ((old-hide (erc--check-msg-prop 'erc--hide))
          (beg (point-min))
          (end (point-max)))
      (puthash 'erc--hide (if old-hide
                              `(,value . ,(ensure-list old-hide))
                            value)
               erc--msg-props)
      (save-restriction
        (widen)
        (when (or (<= beg 4) (= ?\n (char-before (- beg 2))))
          (cl-incf beg))
        (erc--merge-prop (1- beg) (1- end) 'invisible value)))))

(defun erc--toggle-hidden (prop arg)
  "Toggle invisibility for spec member PROP.
Treat ARG in a manner similar to mode toggles defined by
`define-minor-mode'."
  (when arg
    (setq arg (prefix-numeric-value arg)))
  (if (memq prop (ensure-list buffer-invisibility-spec))
      (unless (natnump arg)
        (remove-from-invisibility-spec prop))
    (when (or (not arg) (natnump arg))
      (add-to-invisibility-spec prop))))

(defun erc--delete-inserted-message-naively (beg-or-point &optional end)
  "Remove message between BEG and END.
Do this without updating messages on either side even if their
appearance was somehow influenced by the newly absent message.
Expect BEG and END to match bounds as returned by the function
`erc--get-inserted-msg-bounds'.  Ensure all markers residing at
the start of the deleted message end up at the beginning of the
subsequent message."
  (let ((beg beg-or-point))
    (save-restriction
      (widen)
      (unless end
        (setq end (erc--get-inserted-msg-bounds beg-or-point)
              beg (pop end)))
      (with-silent-modifications
        (if erc-legacy-invisible-bounds-p
            (delete-region beg (1+ end))
          (save-excursion
            (goto-char beg)
            (insert-before-markers
             (substring (delete-and-extract-region (1- (point)) (1+ end))
                        -1))))))))

(defvar erc--ranked-properties
  '( erc--msg erc--spkr erc--ts erc--skip
     erc--cmd erc--hide erc--ctcp erc--ephemeral))

(defun erc--order-text-properties-from-hash (table)
  "Return a plist of text props from items in TABLE.
Ensure props in `erc--ranked-properties' appear last and in
reverse order so they end up sorted in buffer interval plists for
retrieval by `text-properties-at' and friends."
  (let (out)
    (dolist (k erc--ranked-properties)
      (when-let* ((v (gethash k table)))
        (remhash k table)
        (setq out (nconc (list k v) out))))
    (maphash (lambda (k v) (setq out (nconc (list k v) out))) table)
    out))

(defun erc-display-message-highlight (type string)
  "Highlight STRING according to TYPE, where erc-TYPE-face is an ERC face.

See also `erc-make-notice'."
  (cond ((eq type 'notice)
         (erc-make-notice string))
        (t
         (erc-put-text-property
          0 (length string)
          'font-lock-face
          (let* ((name (symbol-name type))
                 (symbol (or (intern-soft (concat "erc-" name "-face"))
                             (intern-soft (concat "erc-" name))
                             type)))
            (or (and (facep symbol) symbol) 'erc-default-face))
          string)
         string)))

(defvar erc-lurker-state nil
  "Track the time of the last PRIVMSG for each (server,nick) pair.

This is implemented as a hash of hashes, where the outer key is
the canonicalized server name (as returned by
`erc-canonicalize-server-name') and the outer value is a hash
table mapping nicks (as returned by `erc-lurker-maybe-trim') to
the times of their most recently received PRIVMSG on any channel
on the given server.")

(defcustom erc-lurker-trim-nicks t
  "If t, trim trailing `erc-lurker-ignore-chars' from nicks.

This causes e.g. nick and nick\\=` to be considered as the same
individual for activity tracking and lurkiness detection
purposes."
  :group 'erc-lurker
  :type 'boolean)

(defcustom erc-lurker-ignore-chars "`_"
  "Characters at the end of a nick to strip for activity tracking purposes.

See also `erc-lurker-trim-nicks'."
  :group 'erc-lurker
  :type 'string)

(defun erc-lurker-maybe-trim (nick)
  "Maybe trim trailing `erc-lurker-ignore-chars' from NICK.

Returns NICK unmodified unless `erc-lurker-trim-nicks' is
non-nil."
  (if erc-lurker-trim-nicks
      (string-trim-right
       nick (rx-to-string `(+ (in ,@(string-to-list erc-lurker-ignore-chars)))))
    nick))

(defcustom erc-lurker-hide-list nil
  "List of IRC type messages to hide when sent by lurkers.

A typical value would be \(\"JOIN\" \"PART\" \"QUIT\").
See also `erc-lurker-p' and `erc-hide-list'."
  :group 'erc-lurker
  :type 'erc-message-type)

(defcustom erc-lurker-threshold-time (* 60 60 24) ; 24h by default
  "Nicks from which no PRIVMSGs have been received within this
interval (in units of seconds) are considered lurkers by
`erc-lurker-p' and as a result their messages of types in
`erc-lurker-hide-list' will be hidden."
  :group 'erc-lurker
  :type 'integer)

(defun erc-lurker-initialize ()
  "Initialize ERC lurker tracking functionality.

This function adds `erc-lurker-update-status' to
`erc-insert-pre-hook' in order to record the time of each nick's
most recent PRIVMSG as well as initializing the state variable
storing this information."
  (setq erc-lurker-state (make-hash-table :test 'equal))
  (add-hook 'erc-insert-pre-hook #'erc-lurker-update-status))

(defun erc-lurker-cleanup ()
  "Remove all last PRIVMSG state older than `erc-lurker-threshold-time'.

This should be called regularly to avoid excessive resource
consumption for long-lived IRC or Emacs sessions."
  (maphash
   (lambda (server hash)
     (maphash
      (lambda (nick last-PRIVMSG-time)
        (when
	    (time-less-p erc-lurker-threshold-time
			 (time-since last-PRIVMSG-time))
          (remhash nick hash)))
      hash)
     (if (zerop (hash-table-count hash))
         (remhash server erc-lurker-state)))
   erc-lurker-state))

(defvar erc-lurker-cleanup-count 0
  "Internal counter variable for use with `erc-lurker-cleanup-interval'.")

(defvar erc-lurker-cleanup-interval 100
  "Frequency of cleaning up stale erc-lurker state.

`erc-lurker-update-status' calls `erc-lurker-cleanup' once for
every `erc-lurker-cleanup-interval' updates to
`erc-lurker-state'.  This is designed to limit the memory
consumption of lurker state during long Emacs sessions and/or ERC
sessions with large numbers of incoming PRIVMSGs.")

(defun erc-lurker-update-status (_message)
  "Update `erc-lurker-state' if necessary.

This function is called from `erc-insert-pre-hook'.  If the
current message is a PRIVMSG, update `erc-lurker-state' to
reflect the fact that its sender has issued a PRIVMSG at the
current time.  Otherwise, take no action.

This function depends on the fact that `erc-display-message'
dynamically binds `erc-message-parsed', which is used to check if
the current message is a PRIVMSG and to determine its sender.
See also `erc-lurker-trim-nicks' and `erc-lurker-ignore-chars'.

In order to limit memory consumption, this function also calls
`erc-lurker-cleanup' once every `erc-lurker-cleanup-interval'
updates of `erc-lurker-state'."
  (when (and (boundp 'erc-message-parsed)
             (erc-response-p erc-message-parsed))
    (let* ((command (erc-response.command erc-message-parsed))
           (sender
            (erc-lurker-maybe-trim
             (car (erc-parse-user
                   (erc-response.sender erc-message-parsed)))))
           (server
            (erc-canonicalize-server-name erc-server-announced-name)))
      (when (equal command "PRIVMSG")
        (when (>= (cl-incf erc-lurker-cleanup-count)
                  erc-lurker-cleanup-interval)
          (setq erc-lurker-cleanup-count 0)
          (erc-lurker-cleanup))
        (unless (gethash server erc-lurker-state)
          (puthash server (make-hash-table :test 'equal) erc-lurker-state))
        (puthash sender (current-time)
                 (gethash server erc-lurker-state))))))

(defun erc-lurker-p (nick)
  "Predicate indicating NICK's lurking status on the current server.

Lurking is the condition where NICK has issued no PRIVMSG on this
server within `erc-lurker-threshold-time'.  See also
`erc-lurker-trim-nicks' and `erc-lurker-ignore-chars'."
  (unless erc-lurker-state (erc-lurker-initialize))
  (let* ((server
          (erc-canonicalize-server-name erc-server-announced-name))
         (last-PRIVMSG-time
          (gethash (erc-lurker-maybe-trim nick)
                   (gethash server erc-lurker-state (make-hash-table)))))
    (or (null last-PRIVMSG-time)
	(time-less-p erc-lurker-threshold-time
		     (time-since last-PRIVMSG-time)))))

(defcustom erc-common-server-suffixes
  '(("openprojects.net\\'" . "OPN")
    ("freenode.net\\'" . "freenode")
    ("oftc.net\\'" . "OFTC"))
  "Alist of common server name suffixes.
This variable is used in mode-line display to save screen
real estate.  Set it to nil if you want to avoid changing
displayed hostnames."
  :group 'erc-mode-line-and-header
  :type 'alist)

(defun erc-canonicalize-server-name (server)
  "Return canonical network name for SERVER or `erc-server-announced-name'.
SERVER is matched against `erc-common-server-suffixes'."
  (when server
    (or (cdar (cl-remove-if-not
               (lambda (net) (string-match (car net) server))
               erc-common-server-suffixes))
        erc-server-announced-name)))

(defun erc-add-targets (scope target-list)
  (let ((targets
	 (mapcar (lambda (targets) (member scope targets)) target-list)))
    (cdr (apply #'append (delete nil targets)))))

(defun erc-hide-current-message-p (parsed)
  "Predicate indicating whether the parsed ERC response PARSED should be hidden.

Messages are always hidden if the message type of PARSED appears in
`erc-hide-list'.  Message types that appear in `erc-network-hide-list'
or `erc-channel-hide-list' are only hidden if the target matches
the network or channel in the list.  In addition, messages whose type
is a member of `erc-lurker-hide-list' are hidden if `erc-lurker-p'
returns non-nil."
  (let* ((command (erc-response.command parsed))
         (sender (car (erc-parse-user (erc-response.sender parsed))))
         (channel (car (erc-response.command-args parsed)))
         (network (or (and (erc-network) (erc-network-name))
		      (erc-shorten-server-name
		       (or erc-server-announced-name
			   erc-session-server))))
	 (current-hide-list
	  (when erc-network-hide-list
	    (erc-add-targets network erc-network-hide-list)))
	 (current-hide-list
	  (append current-hide-list
		  (when erc-channel-hide-list
		    (erc-add-targets channel erc-channel-hide-list)))))
    (or (member command erc-hide-list)
        (member command current-hide-list)
        (and (member command erc-lurker-hide-list) (erc-lurker-p sender)))))

(defun erc-display-message (parsed type buffer msg &rest args)
  "Display MSG in BUFFER.

Insert MSG or text derived from MSG into an ERC buffer, possibly
after applying formatting by way of either a `format-spec' known
to a message-catalog entry or a TYPE known to a specialized
string handler.  Additionally, derive metadata, faces, and other
text properties from the various overloaded parameters, such as
PARSED, when it's an `erc-response' object, and MSG, when it's a
key (symbol) for a \"message catalog\" entry.  Expect ARGS, when
applicable, to be `format-spec' args known to such an entry, and
TYPE, when non-nil, to be a symbol handled by
`erc-display-message-highlight' (necessarily accompanied by a
string MSG).  Expect BUFFER to be among the sort accepted by the
function `erc-display-line'.

When non-nil, expect BUFFER to be a live `erc-mode' buffer, a
list of such buffers, or the symbols `all' or `active'.  If
`all', insert STRING in all buffers for the current session.  If
`active', defer to the function `erc-active-buffer', which may
return the session's server buffer if the previously active
buffer has been killed.  If BUFFER is nil or a network process,
pretend it's set to the appropriate server buffer.  Otherwise,
use the current buffer.

When TYPE is a list of symbols, call handlers from left to right
without influencing how they behave when encountering existing
faces.  As of ERC 5.6, expect a TYPE of (notice error) to insert
MSG with `font-lock-face' as `erc-error-face' throughout.
However, when the list of symbols begins with t, tell compatible
handlers to compose rather than clobber faces.  For example,
expect a TYPE of (t notice error) to result in `font-lock-face'
being (erc-error-face erc-notice-face) throughout MSG when
`erc-notice-highlight-type' is left at its default, `all'.

As of ERC 5.6, assume third-party code will use this function
instead of lower-level ones, like `erc-insert-line', to insert
arbitrary informative messages as if sent by the server.  That
is, tell modules to treat a \"local\" message for which PARSED is
nil like any other server-sent message.  Finally, expect users to
treat the return value of this function as undefined even though
various default response handlers may appear to presume nil."
  (let* ((erc--msg-props
          (or erc--msg-props
              (let ((table (make-hash-table))
                    (cmd (and parsed (erc--get-eq-comparable-cmd
                                      (erc-response.command parsed)))))
                (puthash 'erc--msg
                         (cond ((and msg (symbolp msg)) msg)
                               (type (pcase type
                                       ((pred symbolp) type)
                                       ((pred listp)
                                        (intern (mapconcat #'prin1-to-string
                                                           type "-")))
                                       (_ 'unknown)))
                               (t 'unknown))
                         table)
                (when cmd
                  (puthash 'erc--cmd cmd table))
                (when erc--msg-prop-overrides
                  (pcase-dolist (`(,k . ,v) (reverse erc--msg-prop-overrides))
                    (when v (puthash k v table))))
                table)))
         (erc-message-parsed parsed)
         (string (if (symbolp msg) (apply #'erc-format-message msg args) msg)))
    (setq string
          (cond
           ((null type)
            string)
           ((listp type)
            (let ((erc--merge-text-properties-p
                   (and (eq (car type) t) (setq type (cdr type)))))
              (dolist (type type)
                (setq string (erc-display-message-highlight type string))))
            string)
           ((symbolp type)
            (erc-display-message-highlight type string))))

    (if (not (erc-response-p parsed))
        (erc--route-insertion string buffer)
      (unless (erc-hide-current-message-p parsed)
        (erc-put-text-property 0 (length string) 'erc-parsed parsed string)
	(when (erc-response.tags parsed)
	  (erc-put-text-property 0 (length string) 'tags (erc-response.tags parsed)
				 string))
        (erc--route-insertion string buffer)))))

(defun erc-message-type-member (position list)
  "Return non-nil if the erc-parsed text-property at POSITION is in LIST.

This function relies on the erc-parsed text-property being
present."
  (let ((prop-val (erc-get-parsed-vector position)))
    (and prop-val (member (erc-response.command prop-val) list))))

(defvar erc--called-as-input-p nil
  "Non-nil when a user types a \"/slash\" command.
Remains bound until `erc-cmd-SLASH' returns.")

(defvar erc--current-line-input-split nil
  "Current `erc--input-split' instance when processing user input.
This is for special cases in which a \"slash\" command needs
details about the input it's handling or needs to detect whether
it's been dispatched by `erc-send-current-line'.")

(defvar erc--allow-empty-outgoing-lines-p nil
  "Flag to opt out of last-minute padding of empty lines.
Useful to extensions, like `multiline', and for interop with
IRC-adjacent protocols.")

(defvar-local erc-send-input-line-function #'erc-send-input-line
  "Function for sending lines lacking a leading \"slash\" command.
When prompt input starts with a \"slash\" command, like \"/MSG\",
ERC calls a corresponding handler, like `erc-cmd-MSG'.  But
normal \"chat\" input also needs processing, for example, to
convert it into a proper IRC command.  ERC calls this variable's
value to perform that task, which, by default, simply involves
constructing a \"PRIVMSG\" with the current channel or query
partner as the target.  Some libraries, like `erc-dcc', use this
for other purposes.")

(defun erc-send-input-line (target line &optional force)
  "Send LINE to TARGET."
  (when-let* ((target)
              (cmem (erc-get-channel-member (erc-current-nick))))
    (setf (erc-channel-user-last-message-time (cdr cmem))
          (erc-compat--current-lisp-time)))
  (when (and (not erc--allow-empty-outgoing-lines-p) (string= line "\n"))
    (setq line " \n"))
  (erc-message "PRIVMSG" (concat target " " line) force))

(defun erc-get-arglist (fun)
  "Return the argument list of a function without the parens."
  (let ((arglist (format "%S" (help-function-arglist fun))))
    (if (string-match "\\`(\\(.*\\))\\'" arglist)
        (match-string 1 arglist)
      arglist)))

(defun erc-command-no-process-p (str)
  "Return non-nil if STR is an ERC command that can be run when the process
is not alive, nil otherwise."
  (let ((fun (erc-extract-command-from-line str)))
    (and fun
         (symbolp (car fun))
         (get (car fun) 'process-not-needed))))

(defun erc-command-name (cmd)
  "For CMD being the function name of a ERC command, something like
erc-cmd-FOO, this returns a string /FOO."
  (let ((command-name (symbol-name cmd)))
    (if (string-match "\\`erc-cmd-\\(.*\\)\\'" command-name)
        (concat "/" (match-string 1 command-name))
      command-name)))

(defun erc-process-input-line (line &optional force no-command)
  "Dispatch a slash-command or chat-input handler from user-input LINE.
If simplistic validation fails, print an error and return nil.
Otherwise, defer to an appropriate handler.  For \"slash\" commands,
like \"/JOIN\", expect a handler, like `erc-cmd-JOIN', to return non-nil
if LINE is fit for echoing as a command line when executing scripts.
For normal chat input, expect a handler to return non-nil if a message
was successfully processed as an outgoing \"PRIVMSG\".  If LINE is a
slash command, and ERC can't find a corresponding handler of the form
`erc-cmd-<COMMAND>', pass LINE to `erc-cmd-default', treating it as a
catch-all handler.  Otherwise, for normal chat input, pass LINE and the
boolean argument FORCE to `erc-send-input-line-function'.  With a
non-nil NO-COMMAND, always treat LINE as normal chat input rather than a
slash command."
  (let ((command-list (erc-extract-command-from-line line)))
    (if (and command-list
             (not no-command))
        (let* ((cmd  (nth 0 command-list))
               (args (nth 1 command-list))
               (erc--called-as-input-p t))
          (erc--server-last-reconnect-display-reset (erc-server-buffer))
          (condition-case nil
              (if (listp args)
                  (apply cmd args)
                (funcall cmd args))
            (wrong-number-of-arguments
             (erc-display-message nil 'error (current-buffer) 'incorrect-args
                                  ?c (erc-command-name cmd)
                                  ?u (or (erc-get-arglist cmd)
                                         "")
                                  ?d (format "%s\n"
                                             (or (documentation cmd) "")))
             nil)))
      (let ((r (erc-default-target)))
        (if r
            (funcall erc-send-input-line-function r line force)
          (erc-display-message nil '(notice error) (current-buffer) 'no-target)
          nil)))))

(defconst erc--shell-parse-regexp
  (rx (or (+ (not (any ?\s ?\t ?\n ?\\ ?\" ?' ?\;)))
          (: ?' (group (* (not ?'))) (? ?'))
          (: ?\" (group (* (or (not (any ?\" ?\\)) (: ?\\ nonl)))) (? ?\"))
          (: ?\\ (group (? (or nonl ?\n)))))))

(defun erc--split-string-shell-cmd (string)
  "Parse whitespace-separated arguments in STRING."
  ;; From `shell--parse-pcomplete-arguments' and friends.  Quirk:
  ;; backslash-escaped characters appearing within spans of double
  ;; quotes are unescaped.
  (with-temp-buffer
    (insert string)
    (let ((end (point))
          args)
      (goto-char (point-min))
      (while (and (skip-chars-forward " \t") (< (point) end))
        (let (arg)
          (while (looking-at erc--shell-parse-regexp)
            (goto-char (match-end 0))
            (cond ((match-beginning 3) ; backslash escape
                   (push (if (= (match-beginning 3) (match-end 3))
                             "\\"
                           (match-string 3))
                         arg))
                  ((match-beginning 2) ; double quote
                   (push (replace-regexp-in-string (rx ?\\ (group nonl))
                                                   "\\1" (match-string 2))
                         arg))
                  ((match-beginning 1) ; single quote
                   (push (match-string 1) arg))
                  (t (push (match-string 0) arg))))
          (push (string-join (nreverse arg)) args)))
      (nreverse args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Input commands handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc--connected-and-joined-p ()
  (and (erc--current-buffer-joined-p)
       erc-server-connected))

(defun erc-cmd-GMSG (line)
  "Send LINE to all channels on all networks you are on."
  (setq line (string-remove-prefix " " line))
  (erc-with-all-buffers-of-server nil
      #'erc--connected-and-joined-p
    (erc-send-message line)))
(put 'erc-cmd-GMSG 'do-not-parse-args t)

(defun erc-cmd-AMSG (line)
  "Send LINE to all channels of the current network.
Interactively, prompt for the line of text to send."
  (interactive "sSend to all channels on this network: ")
  (setq line (string-remove-prefix " " line))
  (erc-with-all-buffers-of-server erc-server-process
      #'erc--connected-and-joined-p
    (erc-send-message line)))
(put 'erc-cmd-AMSG 'do-not-parse-args t)

(defun erc-cmd-GME (line)
  "Send LINE as an action to all channels on all networks you are on."
  (erc-with-all-buffers-of-server nil
      #'erc--connected-and-joined-p
    (erc-cmd-ME line)))
(put 'erc-cmd-GME 'do-not-parse-args t)

(defun erc-cmd-AME (line)
  "Send LINE as an action to all channels on the current network."
  (erc-with-all-buffers-of-server erc-server-process
      #'erc--connected-and-joined-p
    (erc-cmd-ME line)))
(put 'erc-cmd-AME 'do-not-parse-args t)

(defun erc-cmd-SAY (line)
  "Send LINE to the current query or channel as a message, not a command.

Use this when you want to send a message with a leading `/'.  Note
that since multi-line messages are never a command, you don't
need this when pasting multiple lines of text."
  (if (string-match "^\\s-*$" line)
      nil
    (string-match "^ ?\\(.*\\)" line)
    (erc-send-message (match-string 1 line) nil)))
(put 'erc-cmd-SAY 'do-not-parse-args t)

(defun erc-cmd-SET (line)
  "Set the variable named by the first word in LINE to some VALUE.
VALUE is computed by evaluating the rest of LINE in Lisp."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.*\\)$" line)
    (let ((var (read (concat "erc-" (match-string 1 line))))
          (val (read (match-string 2 line))))
      (if (boundp var)
          (progn
            (set var (eval val t))
            (erc-display-message
             nil nil 'active (format "Set %S to %S" var val))
            t)
        (setq var (read (match-string 1 line)))
        (if (boundp var)
            (progn
              (set var (eval val t))
              (erc-display-message
               nil nil 'active (format "Set %S to %S" var val))
              t)
          (erc-display-message nil 'error 'active 'variable-not-bound)
          nil))))
   ((string-match "^\\s-*$" line)
    (erc-display-line
     (concat "Available user variables:\n"
             (apply
              #'concat
              (mapcar
               (lambda (var)
                 (let ((val (symbol-value var)))
                   (concat (format "%S:" var)
                           (if (consp val)
                               (concat "\n" (pp-to-string val))
                             (format " %S\n" val)))))
               (apropos-internal "^erc-" 'custom-variable-p))))
     (current-buffer))
    t)
   (t nil)))
(defalias 'erc-cmd-VAR #'erc-cmd-SET)
(defalias 'erc-cmd-VARIABLE #'erc-cmd-SET)
(put 'erc-cmd-SET 'do-not-parse-args t)
(put 'erc-cmd-SET 'process-not-needed t)

(defun erc-cmd-default (line)
  "Fallback command.

Commands for which no erc-cmd-xxx exists, are tunneled through
this function.  LINE is sent to the server verbatim, and
therefore has to contain the command itself as well."
  (erc-log (format "cmd: DEFAULT: %s" line))
  (erc-server-send (string-trim-right (substring line 1) "[\r\n]"))
  t)

(defvar erc--read-time-period-history nil)

(defun erc--read-time-period (prompt)
  "Read a time period on the \"2h\" format.
If there's no letter spec, the input is interpreted as a number of seconds.

If input is blank, this function returns nil.  Otherwise it
returns the time spec converted to a number of seconds."
  (erc--decode-time-period
   (string-trim (read-string prompt nil 'erc--read-time-period-history))))

(defun erc--decode-time-period (period)
  (progn ; unprogn on next major refactor
    (cond
     ;; Blank input.
     ((zerop (length period))
      nil)
     ;; All-number -- interpret as seconds.
     ((string-match-p "\\`[0-9]+\\'" period)
      (string-to-number period))
     ;; Parse as a time spec.
     (t
      (require 'time-date)
      (require 'iso8601)
      (let ((time (condition-case nil
                      (iso8601-parse-duration
                       (concat (cond
                                ((string-match-p "\\`P" (upcase period))
                                 ;; Somebody typed in a full ISO8601 period.
                                 (upcase period))
                                ((string-match-p "[YD]" (upcase period))
                                 ;; If we have a year/day element,
                                 ;; we have a full spec.
                                 "P")
                                (t
                                 ;; Otherwise it's just a sub-day spec.
                                 "PT"))
                               (upcase period)))
                    (wrong-type-argument nil))))
        (unless time
          (user-error "%s is not a valid time period" period))
        (decoded-time-period time))))))

(defun erc--format-time-period (secs)
  "Return a string with hour/minute/second labels for duration in SECS."
  (let* ((hours (floor secs 3600))
         (minutes (floor (mod secs 3600) 60))
         (seconds (mod secs 60)))
    (cond ((>= secs 3600) (format "%dh%dm%ds" hours minutes (floor seconds)))
          ((>= secs 60) (format "%dm%ds" minutes (floor seconds)))
          (t (format "%ds" (floor seconds))))))

(defun erc--get-ignore-timer-args (inst)
  ;; The `cl-struct' `pcase' pattern and `cl-struct-slot-value' emit
  ;; warnings when compiling because `timer' is un-`:named'.
  (when (and (timerp inst)
             (eq (aref inst (cl-struct-slot-offset 'timer 'function))
                 'erc--unignore-user))
    (aref inst (cl-struct-slot-offset 'timer 'args))))

(defun erc--find-ignore-timer (&rest args)
  "Find an existing ignore timer."
  (cl-find args timer-list :key #'erc--get-ignore-timer-args :test #'equal))

(defun erc-cmd-IGNORE (&optional user timespec)
  "Drop messages from senders, like nick!user@host, matching regexp USER.
With human-readable TIMESPEC, ignore messages from matched senders for
the specified duration, like \"20m\".  Without USER, list the contents
of `erc-ignore-list'."
  (if user
      (let ((quoted (regexp-quote user))
            (prompt "Add a timeout? (Blank for no, or a time spec like 2h): ")
            timeout msg)
        (when (and (not (string= user quoted))
                   (y-or-n-p (format "Use regexp-quoted form (%s) instead? "
                                     quoted)))
          (setq user quoted))
        (unless timespec
          (setq timespec
                (read-string prompt nil 'erc--read-time-period-history)))
        (setq timeout (erc--decode-time-period (string-trim timespec))
              msg (if timeout
                      (format "Now ignoring %s for %s" user
                              (erc--format-time-period timeout))
                    (format "Now ignoring %s" user)))
        (erc-with-server-buffer
          (when timeout
            (if-let* ((existing (erc--find-ignore-timer user (current-buffer))))
                (timer-set-time existing (timer-relative-time nil timeout))
              (run-at-time timeout nil #'erc--unignore-user user
                           (current-buffer))))
          (erc-display-message nil 'notice 'active msg)
          (cl-pushnew user erc-ignore-list :test #'equal)))
    (if (null (erc-with-server-buffer erc-ignore-list))
        (erc-display-message nil 'notice 'active "Ignore list is empty")
      (erc-display-message nil 'notice 'active "Ignore list:")
      (erc-with-server-buffer
        (let ((seen (copy-sequence erc-ignore-list)))
          (dolist (timer timer-list)
            (when-let* ((args (erc--get-ignore-timer-args timer))
                        ((eq (current-buffer) (nth 1 args)))
                        (user (car args))
                        (delta (- (timer-until timer (current-time))))
                        (duration (erc--format-time-period delta)))
              (setq seen (delete user seen))
              (erc-display-message nil 'notice 'active 'ignore-list
                                   ?p user ?s duration)))
          (dolist (pattern seen)
            (erc-display-message nil 'notice 'active pattern))))))
  t)

(defun erc-cmd-UNIGNORE (user)
  "Remove the first pattern in `erc-ignore-list' matching USER."
  (let ((ignored-nick (car (erc-with-server-buffer
                             (erc-member-ignore-case (regexp-quote user)
                                                     erc-ignore-list)))))
    (unless ignored-nick
      (if (setq ignored-nick (erc-ignored-user-p user))
          (unless (y-or-n-p (format "Remove this regexp (%s)? "
                                    ignored-nick))
            (setq ignored-nick nil))
        (erc-display-message nil 'notice 'active
                             (format "%s is not currently ignored!" user))))
    (when ignored-nick
      (erc--unignore-user ignored-nick (erc-server-buffer))))
  t)

(defun erc--unignore-user (user buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cl-assert (erc--server-buffer-p))
      (erc-display-message nil 'notice 'active
                           (format "No longer ignoring %s" user))
      (setq erc-ignore-list (delete user erc-ignore-list))
      (when-let* ((existing (erc--find-ignore-timer user buffer)))
        (cancel-timer existing)))))

(defvar erc--clear-function #'delete-region
  "Function to truncate buffer.
Called with two markers, LOWER and UPPER, indicating the bounds of the
interval to be excised.  LOWER <= UPPER <= `erc-insert-marker'.")

(defun erc--skip-past-headroom-on-clear (beg end)
  "Move marker BEG past the two newlines added by `erc--initialize-markers'."
  (when (and (not (buffer-narrowed-p)) (= beg (point-min)))
    (save-excursion
      (goto-char (point-min))
      (let ((pos (skip-chars-forward "\n" (if erc--called-as-input-p 2 3))))
        (set-marker beg (min (1+ pos) end erc-input-marker))))))

(defvar erc--inhibit-clear-p nil
  "When non-nil, ERC inhibits buffer truncation.")

(defun erc-cmd-CLEAR ()
  "Clear messages in current buffer after informing active modules.
Expect modules to perform housekeeping tasks to withstand the
disruption.  When called from Lisp code, only clear messages up
to but not including the one occupying the current line."
  (when erc--inhibit-clear-p
    (user-error "Truncation currently inhibited"))
  (with-silent-modifications
    (let ((end (copy-marker
                ;; Leave a final newline for compatibility, even though
                ;; it complicates `erc--clear-function' handling.
                (cond ((>= (point) erc-insert-marker)
                       (max (point-min) (1- erc-insert-marker)))
                      ((erc--get-inserted-msg-beg (point)))
                      ((pos-bol)))))
          (beg (point-min-marker)))
      (let ((erc--inhibit-clear-p t))
        (funcall erc--clear-function beg end))
      (set-marker beg nil)
      (set-marker end nil)))
  t)
(put 'erc-cmd-CLEAR 'process-not-needed t)

(defun erc-cmd-OPS ()
  "Show the ops in the current channel."
  (interactive)
  (let ((ops nil))
    (if erc-channel-users
        (maphash (lambda (_nick user-data)
                   (let ((cuser (cdr user-data)))
                     (if (and cuser
                              (erc-channel-user-op cuser))
                         (setq ops (cons (erc-server-user-nickname
                                          (car user-data))
                                         ops)))))
                 erc-channel-users))
    (setq ops (sort ops #'string-lessp))
    (if ops
        (erc-display-message
         nil 'notice (current-buffer) 'ops
         ?i (length ops) ?s (if (> (length ops) 1) "s" "")
         ?o (mapconcat #'identity ops " "))
      (erc-display-message nil 'notice (current-buffer) 'ops-none)))
  t)

(defun erc-cmd-COUNTRY (tld)
  "Display the country associated with the top level domain TLD."
  (require 'mail-extr)
  (let ((co (ignore-errors (what-domain tld))))
    (if co
        (erc-display-message
         nil 'notice 'active 'country ?c co ?d tld)
      (erc-display-message
       nil 'notice 'active 'country-unknown ?d tld))
    t))
(put 'erc-cmd-COUNTRY 'process-not-needed t)

(defun erc-cmd-AWAY (line)
  "Mark the user as being away, the reason being indicated by LINE.
If no reason is given, unset away status."
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (erc-log (format "cmd: AWAY: %s" reason))
      (erc-server-send
       (if (string= reason "")
           "AWAY"
         (concat "AWAY :" reason))))
    t))
(put 'erc-cmd-AWAY 'do-not-parse-args t)

(defun erc-cmd-GAWAY (line)
  "Mark the user as being away everywhere, the reason being indicated by LINE."
  ;; on all server buffers.
  (erc-with-all-buffers-of-server nil
    #'erc-open-server-buffer-p
    (erc-cmd-AWAY line)))
(put 'erc-cmd-GAWAY 'do-not-parse-args t)

(defun erc-cmd-CTCP (nick cmd &rest args)
  "Send a Client To Client Protocol message to NICK.

CMD is the CTCP command, possible values being ECHO, FINGER, CLIENTINFO, TIME,
VERSION and so on.  It is called with ARGS."
  (let ((str (concat cmd
                     (when args
                       (concat " " (mapconcat #'identity args " "))))))
    (erc-log (format "cmd: CTCP [%s]: [%s]" nick str))
    (erc-send-ctcp-message nick str)
    t))

(defun erc-cmd-HELP (&optional func &rest rest)
  "Popup help information.

If FUNC contains a valid function or variable, help about that
will be displayed.  If FUNC is empty, display an apropos about
ERC commands.  Otherwise, do `apropos' in the ERC namespace
\(\"erc-.*LINE\").

Examples:
To find out about erc and bbdb, do
  /help bbdb.*

For help about the WHOIS command, do:
  /help whois

For a list of user commands (/join /part, ...):
  /help."
  (if func
      (let* ((sym (or (let ((sym (intern-soft
                                  (concat "erc-cmd-" (upcase func)))))
                        (if (and sym (or (boundp sym) (fboundp sym)))
                            sym
                          nil))
                      (let ((sym (intern-soft func)))
                        (if (and sym (or (boundp sym) (fboundp sym)))
                            sym
                          nil))
                      (let ((sym (intern-soft (concat "erc-" func))))
                        (if (and sym (or (boundp sym) (fboundp sym)))
                            sym
                          nil)))))
        (if sym
            (cond
             ((get sym 'erc--cmd-help)
              (when (autoloadp (symbol-function sym))
                (autoload-do-load (symbol-function sym)))
              (apply (get sym 'erc--cmd-help) rest))
             ((boundp sym) (describe-variable sym))
             ((fboundp sym) (describe-function sym))
             (t nil))
          (apropos-command (concat "erc-.*" func) nil
                           (lambda (x)
                             (or (commandp x)
                                 (get x 'custom-type))))
          t))
    (apropos "erc-cmd-")
    (message "Type C-h m to get additional information about keybindings.")
    t))

(defalias 'erc-cmd-H #'erc-cmd-HELP)
(put 'erc-cmd-HELP 'process-not-needed t)

(defcustom erc-auth-source-server-function #'erc-auth-source-search
  "Function to query auth-source for a server password.
Called with a subset of keyword parameters known to
`auth-source-search' and relevant to an opening \"PASS\" command,
if any.  In return, ERC expects a string to send as the server
password, or nil, to skip the \"PASS\" command completely.  An
explicit `:password' argument to entry-point commands `erc' and
`erc-tls' also inhibits lookup, as does setting this option to
nil.  See Info node `(erc) auth-source' for details."
  :package-version '(ERC . "5.5")
  :group 'erc
  :type '(choice (function-item erc-auth-source-search)
                 (const nil)
                 function))

(defcustom erc-auth-source-join-function #'erc-auth-source-search
  "Function to query auth-source on joining a channel.
Called with a subset of keyword arguments known to
`auth-source-search' and relevant to joining a password-protected
channel.  In return, ERC expects a string to use as the channel
\"key\", or nil to just join the channel normally.  Setting the
option itself to nil tells ERC to always forgo consulting
auth-source for channel keys.  For more information, see Info
node `(erc) auth-source'."
  :package-version '(ERC . "5.5")
  :group 'erc
  :type '(choice (function-item erc-auth-source-search)
                 (const nil)
                 function))

(defun erc--auth-source-determine-params-defaults ()
  (let* ((net (and erc-networks--id
                   (erc-networks--id-string erc-networks--id)))
         (localp (and erc--target (erc--target-channel-local-p erc--target)))
         (hosts (if localp
                    (list erc-server-announced-name erc-session-server net)
                  (list net erc-server-announced-name erc-session-server)))
         (ports (list (cl-typecase erc-session-port
                        (integer (number-to-string erc-session-port))
                        (string (and (not (member erc-session-port
                                                  '("" "irc")))
                                     erc-session-port))
                        (t erc-session-port))
                      "irc")))
    (list (cons :host (delq nil hosts))
          (cons :port (delq nil ports))
          (cons :require '(:secret)))))

(defun erc--auth-source-determine-params-merge (&rest plist)
  "Return a plist of merged keyword args to pass to `auth-source-search'.
Combine items in PLIST with others derived from the current connection
context, but prioritize the former.  For keys not present in PLIST,
favor a network ID over an announced server unless `erc--target' is a
local channel.  And treat the dialed server address as a fallback for
the announced name in both cases."
  (let ((defaults (erc--auth-source-determine-params-defaults)))
    `(,@(cl-loop for (key value) on plist by #'cddr
                 for default = (assq key defaults)
                 do (when default (setq defaults (delq default defaults)))
                 append `(,key ,(delete-dups
                                 `(,@(if (consp value) value (list value))
                                   ,@(cdr default)))))
      ,@(cl-loop for (k . v) in defaults append (list k v)))))

(defun erc--auth-source-search (&rest defaults)
  "Ask auth-source for a secret and return it if found.
Use DEFAULTS as keyword arguments for querying auth-source and as a
guide for narrowing results.  Return a string if found or nil otherwise.
The ordering of DEFAULTS influences how results are filtered, as does
the ordering of the members of any individual composite values.  If
necessary, the former takes priority.  For example, if DEFAULTS were to
contain

  :host (\"foo\" \"bar\") :port (\"123\" \"456\")

the secret from an auth-source entry of host foo and port 456 would be
chosen over another of host bar and port 123.  However, if DEFAULTS
looked like

  :port (\"123\" \"456\") :host (\"foo\" \"bar\")

the opposite would be true.  In both cases, two entries with the same
host but different ports would result in the one with port 123 getting
the nod.  Much the same would happen for entries sharing only a port:
the one with host foo would win."
  (when-let*
      ((auth-source-backend-parser-functions
        (erc-compat--auth-source-backend-parser-functions))
       (priority (map-keys defaults))
       (test (lambda (a b)
               (catch 'done
                 (dolist (key priority)
                   (let* ((d (plist-get defaults key))
                          (defval (if (listp d) d (list d)))
                          ;; featurep 'seq via auth-source > json > map
                          (p (seq-position defval (plist-get a key)))
                          (q (seq-position defval (plist-get b key))))
                     (unless (eql p q)
                       (throw 'done (when p (or (not q) (< p q))))))))))
       (plist (copy-sequence defaults)))
    (unless (plist-get plist :max)
      (setq plist (plist-put plist :max 5000))) ; `auth-source-netrc-parse'
    (unless (plist-get defaults :require)
      (setq plist (plist-put plist :require '(:secret))))
    (when-let* ((sorted (sort (apply #'auth-source-search plist) test)))
      (plist-get (car sorted) :secret))))

(defun erc-auth-source-search (&rest plist)
  "Call `auth-source-search', possibly with keyword params in PLIST.
If the search signals an error before returning, `warn' the user
and ask whether to continue connecting anyway."
  ;; These exist as separate helpers in case folks should find them
  ;; useful.  If that's you, please request that they be exported.
  (condition-case err
      (apply #'erc--auth-source-search
             (apply #'erc--auth-source-determine-params-merge plist))
    (error
     (erc--lwarn '(erc auth-source) :error
                 "Problem querying `auth-source': %S. See %S for more."
                 (error-message-string err)
                 '(info "(erc) auth-source Troubleshooting"))
     (when (or noninteractive
               (not (y-or-n-p "Ignore auth-source error and continue? ")))
       (signal (car err) (cdr err))))))

(defun erc-server-join-channel (server channel &optional secret)
  "Join CHANNEL, optionally with SECRET.
Without SECRET, consult auth-source, possibly passing SERVER as the
`:host' query parameter."
  (unless (or secret (not erc-auth-source-join-function))
    (unless server
      (when (and erc-server-announced-name
                 (erc--valid-local-channel-p channel))
        (setq server erc-server-announced-name)))
    (setq secret (apply erc-auth-source-join-function
                        `(,@(and server (list :host server)) :user ,channel))))
  (erc-log (format "cmd: JOIN: %s" channel))
  (erc-server-send (concat "JOIN " channel
                           (and secret (concat " " (erc--unfun secret))))))

(defun erc--valid-local-channel-p (channel)
  "Non-nil when channel is server-local on a network that allows them."
  (and-let* (((eq ?& (aref channel 0)))
             (chan-types (erc--get-isupport-entry 'CHANTYPES 'single))
             ((string-search "&" chan-types)))))

(defun erc-cmd-JOIN (channel &optional key)
  "Join the channel given in CHANNEL, optionally with KEY.
If CHANNEL is specified as \"-invite\", join the channel to which you
were most recently invited.  See also `invitation'."
  (let (chnl)
    (if (string= (upcase channel) "-INVITE")
        (if erc-invitation
            (setq chnl erc-invitation)
          (erc-display-message nil 'error (current-buffer) 'no-invitation))
      (setq chnl (erc-ensure-channel-name channel)))
    (when chnl
      ;; Prevent double joining of same channel on same server.
      (if-let* ((existing (erc-get-buffer chnl erc-server-process))
                ((with-current-buffer existing
                   (erc-get-channel-user (erc-current-nick)))))
          (switch-to-buffer existing)
        (when-let* ; bind `erc-join-buffer' when /JOIN issued
            ((erc--called-as-input-p)
             (fn (lambda (proc parsed)
                   (when-let* ; `fn' wrapper already removed from hook
                       (((equal (car (erc-response.command-args parsed))
                                channel))
                        (sn (erc-extract-nick (erc-response.sender parsed)))
                        ((erc-nick-equal-p sn (erc-current-nick)))
                        (erc-join-buffer (or erc-interactive-display
                                             erc-join-buffer))
                        (erc--display-context `((erc-interactive-display
                                                 . /JOIN)
                                                ,@erc--display-context)))
                     (run-hook-with-args-until-success
                      'erc-server-JOIN-functions proc parsed)
                     t))))
          (erc-with-server-buffer
            (erc-once-with-server-event "JOIN" fn)))
        (erc-server-join-channel nil chnl key))))
  t)

(defalias 'erc-cmd-CHANNEL #'erc-cmd-JOIN)
(defalias 'erc-cmd-J #'erc-cmd-JOIN)

(defvar-local erc-channel-new-member-names nil
  "If non-nil, a names list is currently being received.

If non-nil, this variable is a hash-table that associates
received nicks with t.")

(defun erc-cmd-NAMES (&optional channel)
  "Display the users in CHANNEL.
If CHANNEL is not specified, display the users in the current channel.
This function clears the channel name list first, then sends the
command."
  (let ((tgt (or (and (erc-channel-p channel) channel)
                 (erc-default-target))))
    (if (and tgt (erc-channel-p tgt))
        (progn
          (erc-log (format "cmd: DEFAULT: NAMES %s" tgt))
          (erc-with-buffer
              (tgt)
            (erc-channel-begin-receiving-names))
          (erc-server-send (concat "NAMES " tgt)))
      (erc-display-message nil 'error (current-buffer) 'no-default-channel)))
  t)
(defalias 'erc-cmd-N #'erc-cmd-NAMES)

(defun erc-cmd-KICK (target &optional reason-or-nick &rest reasonwords)
  "Kick the user indicated in LINE from the current channel.
LINE has the format: \"#CHANNEL NICK REASON\" or \"NICK REASON\"."
  (let ((reasonstring (mapconcat #'identity reasonwords " ")))
    (if (string= "" reasonstring)
        (setq reasonstring (format "Kicked by %s" (erc-current-nick))))
    (if (erc-channel-p target)
        (let ((nick reason-or-nick))
          (erc-log (format "cmd: KICK: %s/%s: %s" nick target reasonstring))
          (erc-server-send (format "KICK %s %s :%s" target nick reasonstring)
                           nil target)
          t)
      (when target
        (let ((ch (erc-default-target)))
          (setq reasonstring (concat
                              (if reason-or-nick (concat reason-or-nick " "))
                              reasonstring))
          (if ch
              (progn
                (erc-log
                 (format "cmd: KICK: %s/%s: %s" target ch reasonstring))
                (erc-server-send
                 (format "KICK %s %s :%s" ch target reasonstring) nil ch))
            (erc-display-message nil 'error (current-buffer)
                                 'no-default-channel))
          t)))))

(defvar erc-script-args nil)

(defun erc-cmd-LOAD (line)
  "Load the script provided in the LINE.
If LINE continues beyond the file name, the rest of
it is put in a (local) variable `erc-script-args',
which can be used in Emacs Lisp scripts.

The optional FORCE argument is ignored here - you can't force loading
a script after exceeding the flood threshold."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(.*\\)$" line)
    (let* ((file-to-find (match-string 1 line))
           (erc-script-args (match-string 2 line))
           (file (erc-find-file file-to-find erc-script-path)))
      (erc-log (format "cmd: LOAD: %s" file-to-find))
      (cond
       ((not file)
        (erc-display-message nil 'error (current-buffer)
                             'cannot-find-file ?f file-to-find))
       ((not (file-readable-p file))
        (erc-display-message nil 'error (current-buffer)
                             'cannot-read-file ?f file))
       (t
        (message "Loading `%s'..." file)
        (erc-load-script file)
        (message "Loading `%s'...done" file))))
    t)
   (t nil)))

(defun erc-cmd-WHOIS (first &optional second)
  "Display whois information for the given user.

With one argument, FIRST is the nickname of the user to request
whois information for.

With two arguments, FIRST is the server, and SECOND is the user
nickname.

Specifying the server is useful for getting the time the user has
been idle for, when the user is connected to a different server
on the same IRC network.  (Only the server a user is connected to
knows how long the user has been idle for.)"
  (let ((send (if second
                  (format "WHOIS %s %s" first second)
                (format "WHOIS %s" first))))
    (erc-log (format "cmd: %s" send))
    (erc-server-send send)
    t))
(defalias 'erc-cmd-WI #'erc-cmd-WHOIS)

(defun erc-cmd-WII (nick)
  "Display whois information for NICK, including idle time.

This is a convenience function which calls `erc-cmd-WHOIS' with
the given NICK for both arguments.  Using NICK in place of the
server argument -- effectively delegating to the IRC network the
looking up of the server to which NICK is connected -- is not
standardized, but is widely supported across IRC networks.

See `erc-cmd-WHOIS' for more details."
  (erc-cmd-WHOIS nick nick))

(defun erc-cmd-WHOAMI ()
  "Display whois information about yourself."
  (erc-cmd-WHOIS (erc-current-nick))
  t)

(defun erc-cmd-IDLE (nick)
  "Show the length of time NICK has been idle."
  (let ((origbuf (current-buffer))
        symlist)
    (erc-with-server-buffer
      (push (cons (erc-once-with-server-event
                   311 (lambda (_proc parsed)
                         (string= nick
                                  (nth 1 (erc-response.command-args
                                          parsed)))))
                  'erc-server-311-functions)
            symlist)
      (push (cons (erc-once-with-server-event
                   312 (lambda (_proc parsed)
                         (string= nick
                                  (nth 1 (erc-response.command-args
                                          parsed)))))
                  'erc-server-312-functions)
            symlist)
      (push (cons (erc-once-with-server-event
                   318 (lambda (_proc parsed)
                         (string= nick
                                  (nth 1 (erc-response.command-args
                                          parsed)))))
                  'erc-server-318-functions)
            symlist)
      (push (cons (erc-once-with-server-event
                   319 (lambda (_proc parsed)
                         (string= nick
                                  (nth 1 (erc-response.command-args
                                          parsed)))))
                  'erc-server-319-functions)
            symlist)
      (push (cons (erc-once-with-server-event
                   320 (lambda (_proc parsed)
                         (string= nick
                                  (nth 1 (erc-response.command-args
                                          parsed)))))
                  'erc-server-320-functions)
            symlist)
      (push (cons (erc-once-with-server-event
                   330 (lambda (_proc parsed)
                         (string= nick
                                  (nth 1 (erc-response.command-args
                                          parsed)))))
                  'erc-server-330-functions)
            symlist)
      (push (cons (erc-once-with-server-event
                   317
                   (lambda (_proc parsed)
                     (let ((idleseconds
                            (string-to-number
                             (cl-third
                              (erc-response.command-args parsed)))))
                       (erc-display-message nil 'notice origbuf
                         (format "%s has been idle for %s."
                                 (erc-string-no-properties nick)
                                 (erc-seconds-to-string idleseconds)))
                       t)))
                  'erc-server-317-functions)
            symlist)

      ;; Send the WHOIS command.
      (erc-cmd-WHOIS nick)

      ;; Remove the uninterned symbols from the server hooks that did not run.
      (run-at-time 20 nil (lambda (buf symlist)
                            (with-current-buffer buf
                              (dolist (sym symlist)
                                (let ((hooksym (cdr sym))
                                      (funcsym (car sym)))
                                  (remove-hook hooksym funcsym t)))))
                   (current-buffer) symlist)))
  t)

(defun erc-cmd-DESCRIBE (line)
  "Pose some action to a certain user.
LINE has the format \"USER ACTION\"."
  (cond
   ((string-match
     "^\\s-*\\(\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((dst (match-string 1 line))
          (s (match-string 2 line)))
      (erc-log (format "cmd: DESCRIBE: [%s] %s" dst s))
      (erc-send-action dst s))
    t)
   (t nil)))
(put 'erc-cmd-DESCRIBE 'do-not-parse-args t)

(defun erc-cmd-ME (line)
  "Send LINE as an action."
  (cond
   ((string-match "^\\s-\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: ME: %s" s))
      (erc-send-action (erc-default-target) s))
    t)
   (t nil)))
(put 'erc-cmd-ME 'do-not-parse-args t)

(defun erc-cmd-ME\'S (line)
  "Do a /ME command, but add the string \" \\='s\" to the beginning."
  (erc-cmd-ME (concat " 's" line)))
(put 'erc-cmd-ME\'S 'do-not-parse-args t)

(defun erc-cmd-LASTLOG (line)
  "Show all lines in the current buffer matching the regexp LINE.

If a match spreads across multiple lines, all those lines are shown.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how.

If LINE contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
  (occur line)
  t)
(put 'erc-cmd-LASTLOG 'do-not-parse-args t)
(put 'erc-cmd-LASTLOG 'process-not-needed t)

(defvar erc--send-message-nested-function #'erc--send-message-nested
  "Function for inserting and sending slash-command generated text.
When a command like /SV or /SAY modifies or replaces command-line
input originally submitted at the prompt, `erc-send-message'
performs additional processing to ensure said input is fit for
inserting and sending given this \"nested\" meta context.  This
interface variable exists because modules extending fundamental
insertion and sending operations need a say in this processing as
well.")

(defun erc-send-message (line &optional force)
  "Send LINE to the current channel or user and display it.

See also `erc-message' and `erc-display-line'."
  (if (erc--input-split-p erc--current-line-input-split)
      (funcall erc--send-message-nested-function line force)
    (erc--send-message-external line force)))

(defun erc--send-message-external (line force)
  "Send a \"PRIVMSG\" to the default target with optional FORCE.
Expect caller to bind `erc-default-recipients' if needing to
specify a status-prefixed target."
  ;; Almost like an echoed message, but without the `erc--cmd'.
  (let* ((erc-current-message-catalog erc--message-speaker-catalog)
         (target (erc-default-target))
         (erc--msg-prop-overrides `((erc--tmp) ,@erc--msg-prop-overrides))
         ;; This util sets the `erc--spkr' property in ^.
         (trimmed (erc--statusmsg-target target))
         (stmsgindc (and trimmed (substring target 0 1)))
         (queryp (and erc--target (not (erc--target-channel-p erc--target))))
         (args (erc--determine-speaker-message-format-args
                (erc-current-nick) line queryp 'privmsgp 'inputp
                stmsgindc 'prefix)))
    (erc-message "PRIVMSG" (concat target " " line) force)
    (push (cons 'erc--msg (car args)) erc--msg-prop-overrides)
    (apply #'erc-display-message nil nil (current-buffer) args))
  ;; FIXME - treat multiline, run hooks, or remove me?
  ;; FIXME explain this ^ in more detail or remove.
  t)

(defun erc--send-message-nested (input-line force)
  "Process string INPUT-LINE almost as if it's normal chat input.
Expect INPUT-LINE to differ from the `string' slot of the calling
context's `erc--current-line-input-split' object because the
latter is likely a slash command invocation whose handler
generated INPUT-LINE.  Before inserting INPUT-LINE, split it and
run `erc-send-modify-hook' and `erc-send-post-hook' on each
actual outgoing line.  Forgo input validation because this isn't
interactive input, and skip `erc-send-completed-hook' because it
will run just before the outer `erc-send-current-line' call
returns."
  (let* ((erc-flood-protect (not force))
         (lines-obj (erc--make-input-split input-line)))
    (setf (erc--input-split-refoldp lines-obj) t
          (erc--input-split-cmdp lines-obj) nil)
    (erc--send-input-lines (erc--run-send-hooks lines-obj)))
  t)

;; FIXME if the user types /MODE<RET>, LINE becomes "\n", which
;; matches the pattern, so "\n" is sent to the server.  Perhaps
;; instead of `do-not-parse-args', this should just join &rest
;; arguments.
(defun erc-cmd-MODE (line)
  "Change or display the mode value of a channel or user.
The first word specifies the target.  The rest is the mode string
to send.

If only one word is given, display the mode of that target.

A list of valid mode strings for Libera.Chat may be found at
`https://libera.chat/guides/channelmodes' and
`https://libera.chat/guides/usermodes'."
  (cond
   ((string-match "^\\s-\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: MODE: %s" s))
      (erc-server-send (concat "MODE " s)))
    t)
   (t nil)))
(put 'erc-cmd-MODE 'do-not-parse-args t)

(defun erc-cmd-NOTICE (channel-or-user &rest message)
  "Send a notice to the channel or user given as the first word.
The rest is the message to send."
  (erc-message "NOTICE" (concat channel-or-user " "
                                (mapconcat #'identity message " "))))

(defun erc-cmd-MSG (line)
  "Send a message to the channel or user given as the first word in LINE.

The rest of LINE is the message to send."
  (erc-message "PRIVMSG" line))

(defalias 'erc-cmd-M #'erc-cmd-MSG)
(put 'erc-cmd-MSG 'do-not-parse-args t)

(defun erc-cmd-SQUERY (line)
  "Send a Service Query to the service given as the first word in LINE.

The rest of LINE is the message to send."
  (erc-message "SQUERY" line))
(put 'erc-cmd-SQUERY 'do-not-parse-args t)

(defun erc-cmd-NICK (nick)
  "Change current nickname to NICK."
  (erc-log (format "cmd: NICK: %s (erc-bad-nick: %S)" nick erc-bad-nick))
  (let ((nicklen (erc-with-server-buffer
                   (erc--get-isupport-entry 'NICKLEN 'single))))
    (and nicklen (> (length nick) (string-to-number nicklen))
         (erc-display-message
          nil 'notice 'active 'nick-too-long
          ?i (length nick) ?l nicklen)))
  (erc-server-send (format "NICK %s" nick))
  (cond (erc-bad-nick
         (erc-set-current-nick nick)
         (erc-update-mode-line)
         (setq erc-bad-nick nil)))
  t)

(defun erc-cmd-PART (line)
  "When LINE is an empty string, leave the current channel.
Otherwise leave the channel indicated by LINE."
  (cond
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\s-?\\(.*\\)$" line)
    (let* ((ch (match-string 1 line))
           (msg (match-string 2 line))
           (reason (funcall erc-part-reason (if (equal msg "") nil msg))))
      (erc-log (format "cmd: PART: %s: %s" ch reason))
      (erc-server-send (if (string= reason "")
                           (format "PART %s" ch)
                         (format "PART %s :%s" ch reason))
                       nil ch))
    t)
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let* ((ch (erc-default-target))
           (msg (match-string 1 line))
           (reason (funcall erc-part-reason (if (equal msg "") nil msg))))
      (if (and ch (erc-channel-p ch))
          (progn
            (erc-log (format "cmd: PART: %s: %s" ch reason))
            (erc-server-send (if (string= reason "")
                                 (format "PART %s" ch)
                               (format "PART %s :%s" ch reason))
                             nil ch))
        (erc-display-message nil '(notice error) (current-buffer) 'no-target)))
    t)
   (t nil)))
(put 'erc-cmd-PART 'do-not-parse-args t)

(defalias 'erc-cmd-LEAVE #'erc-cmd-PART)

(defun erc-cmd-PING (recipient)
  "Ping RECIPIENT."
  (let ((time (format-time-string "%s.%6N")))
    (erc-log (format "cmd: PING: %s" time))
    (erc-cmd-CTCP recipient "PING" time)))

(defun erc-cmd-QUOTE (line)
  "Send LINE directly to the server.
All the text given as argument is sent to the sever as unmodified,
just as you provided it.  Use this command with care!"
  (cond
   ((string-match "^ ?\\(.+\\)$" line)
    (erc-server-send (match-string 1 line)))
   (t nil)))
(put 'erc-cmd-QUOTE 'do-not-parse-args t)

(defun erc-cmd-QUERY (&optional user)
  "Open a query with USER.
Display the query buffer in accordance with `erc-interactive-display'."
  ;; FIXME: The doc string used to say at the end:
  ;; "If USER is omitted, close the current query buffer if one exists
  ;; - except this is broken now ;-)"
  ;; Does it make sense to have that functionality?  What's wrong with
  ;; `kill-buffer'?  If it makes sense, re-add it.  -- SK @ 2021-11-11
  (interactive
   (list (read-string "Start a query with: ")))
  (unless user
      ;; currently broken, evil hack to display help anyway
                                        ;(erc-delete-query))))
    (signal 'wrong-number-of-arguments '(erc-cmd-QUERY 0)))
  (let ((erc-join-buffer erc-interactive-display)
        (erc--display-context `((erc-interactive-display . /QUERY)
                                ,@erc--display-context)))
    (erc-with-server-buffer
      (if-let* ((buffer (erc-get-buffer user erc-server-process)))
          (prog1 buffer
            (erc-setup-buffer buffer))
        (prog1 (erc--open-target user) ; becomes current buffer
          (erc--ensure-query-member user))))))

(defalias 'erc-cmd-Q #'erc-cmd-QUERY)

(defun erc-quit/part-reason-default ()
  "Default quit/part message."
  (erc-version nil 'bold-erc))


(defun erc-quit-reason-normal (&optional s)
  "Normal quit message.

If S is non-nil, it will be used as the quit reason."
  (or s (erc-quit/part-reason-default)))

(defun erc-quit-reason-zippy (&optional s)
  "Zippy quit message.

If S is non-nil, it will be used as the quit reason."
  (or s (erc-quit/part-reason-default)))

(make-obsolete 'erc-quit-reason-zippy "it will be removed." "24.4")

(defun erc-quit-reason-various (s)
  "Choose a quit reason based on S (a string)."
  (let ((res (car (assoc-default (or s "")
                                 erc-quit-reason-various-alist 'string-match))))
    (cond
     ((functionp res) (funcall res))
     ((stringp res) res)
     (s s)
     (t (erc-quit/part-reason-default)))))

(defun erc-part-reason-normal (&optional s)
  "Normal part message.

If S is non-nil, it will be used as the part reason."
  (or s (erc-quit/part-reason-default)))

(defun erc-part-reason-zippy (&optional s)
  "Zippy part message.

If S is non-nil, it will be used as the quit reason."
  (or s (erc-quit/part-reason-default)))

(make-obsolete 'erc-part-reason-zippy "it will be removed." "24.4")

(defun erc-part-reason-various (s)
  "Choose a part reason based on S (a string)."
  (let ((res (car (assoc-default (or s "")
                                 erc-part-reason-various-alist 'string-match))))
    (cond
     ((functionp res) (funcall res))
     ((stringp res) res)
     (s s)
     (t (erc-quit/part-reason-default)))))

(defun erc-cmd-QUIT (reason)
  "Disconnect from the current server.
If REASON is omitted, display a default quit message, otherwise display
the message given by REASON."
  (unless reason
    (setq reason ""))
  (cond
   ((string-match "^\\s-*\\(.*\\)$" reason)
    (let* ((s (match-string 1 reason))
           (buffer (erc-server-buffer))
           (reason (funcall erc-quit-reason (if (equal s "") nil s)))
           server-proc)
      (with-current-buffer (if (and buffer
                                    (bufferp buffer))
                               buffer
                             (current-buffer))
        (erc-log (format "cmd: QUIT: %s" reason))
        (setq erc-server-quitting t)
        (erc-set-active-buffer (erc-server-buffer))
        (setq server-proc erc-server-process)
        (erc-server-send (format "QUIT :%s" reason)))
      (run-hook-with-args 'erc-quit-hook server-proc)
      (when erc-kill-queries-on-quit
        (erc-kill-query-buffers server-proc))
      ;; if the process has not been killed within 4 seconds, kill it
      (run-at-time 4 nil
                   (lambda (proc)
                     (when (and (processp proc)
                                (memq (process-status proc) '(run open)))
                       (delete-process proc)))
                   server-proc))
    t)
   (t nil)))

(defalias 'erc-cmd-BYE #'erc-cmd-QUIT)
(defalias 'erc-cmd-EXIT #'erc-cmd-QUIT)
(defalias 'erc-cmd-SIGNOFF #'erc-cmd-QUIT)
(put 'erc-cmd-QUIT 'do-not-parse-args t)
(put 'erc-cmd-QUIT 'process-not-needed t)

(defun erc-cmd-GQUIT (reason)
  "Disconnect from all servers at once with the same quit REASON."
  (erc-with-all-buffers-of-server nil #'erc-open-server-buffer-p
                                  (erc-cmd-QUIT reason))
  (when erc-kill-queries-on-quit
    ;; if the query buffers have not been killed within 4 seconds,
    ;; kill them
    (run-at-time
     4 nil
     #'erc-buffer-do (lambda () (when erc--target (kill-buffer)))))
  t)

(defalias 'erc-cmd-GQ #'erc-cmd-GQUIT)
(put 'erc-cmd-GQUIT 'do-not-parse-args t)
(put 'erc-cmd-GQUIT 'process-not-needed t)

(defun erc--cmd-reconnect ()
  (let ((buffer (erc-server-buffer))
        (erc-join-buffer erc-interactive-display)
        (erc--display-context `((erc-interactive-display . /RECONNECT)
                                ,@erc--display-context))
        (process nil))
    (unless (buffer-live-p buffer)
      (setq buffer (current-buffer)))
    (with-current-buffer buffer
      (when erc--server-reconnect-timer
        (erc--cancel-auto-reconnect-timer))
      (setq erc-server-quitting nil)
      (with-suppressed-warnings ((obsolete erc-server-reconnecting))
        (setq erc-server-reconnecting t))
      (setq erc-server-reconnect-count 0)
      (setq process (get-buffer-process (erc-server-buffer)))
      (when process
        (delete-process process))
      (erc-server-reconnect)
      (with-suppressed-warnings ((obsolete erc-server-reconnecting)
                                 (obsolete erc-reuse-buffers))
        (if erc-reuse-buffers
            (cl-assert (not erc-server-reconnecting))
          (setq erc-server-reconnecting nil)))))
  t)

(defun erc-cmd-RECONNECT (&rest args)
  "Try reconnecting to the current IRC server.
Alternatively, CANCEL a scheduled attempt for either the current
connection or, with -A, all applicable connections.

\(fn [CANCEL [-A]])"
  (pcase args
    (`("cancel" "-a") (erc-buffer-filter #'erc--cancel-auto-reconnect-timer))
    (`("cancel") (erc-with-server-buffer (erc--cancel-auto-reconnect-timer)))
    (_ (erc--cmd-reconnect))))

(put 'erc-cmd-RECONNECT 'process-not-needed t)

;; FIXME use less speculative error message or lose `condition-case',
;; since most connection failures don't signal anything.
(defun erc-cmd-SERVER (server)
  "Connect to SERVER, leaving existing connection intact."
  (erc-log (format "cmd: SERVER: %s" server))
  (condition-case nil
      (erc :server server :nick (erc-current-nick))
    (error
     (erc-error "Cannot find host: `%s'" server)))
  t)
(put 'erc-cmd-SERVER 'process-not-needed t)

(defun erc-cmd-SV ()
  "Say the current ERC and Emacs version into channel."
  (erc-send-message (format "I'm using ERC %s with GNU Emacs %s (%s%s)%s."
                            erc-version
                            emacs-version
                            system-configuration
                            (concat
                             (cond ((featurep 'motif)
                                    (defvar motif-version-string)
                                    (concat ", " (substring
                                                  motif-version-string 4)))
                                   ((featurep 'gtk)
                                    (defvar gtk-version-string)
                                    (concat ", GTK+ Version "
                                            gtk-version-string))
                                   ((featurep 'x-toolkit) ", X toolkit")
                                   (t ""))
                             (if (and (boundp 'x-toolkit-scroll-bars)
                                      (memq x-toolkit-scroll-bars
                                            '(xaw xaw3d)))
                                 (format ", %s scroll bars"
                                         (capitalize (symbol-name
                                                      x-toolkit-scroll-bars)))
                               "")
                             (if (featurep 'multi-tty) ", multi-tty" ""))
                            (if emacs-build-time
                                (concat " of " (format-time-string
                                                "%Y-%m-%d" emacs-build-time))
                              "")))
  t)

(defun erc-cmd-SM ()
  "Say the current ERC modes into channel."
  (erc-send-message (format "I'm using the following modules: %s!"
                            (erc-modes)))
  t)

(defun erc-cmd-DEOP (&rest people)
  "Remove the operator setting from user(s) given in PEOPLE."
  (when (> (length people) 0)
    (erc-server-send (concat "MODE " (erc-default-target)
                             " -"
                             (make-string (length people) ?o)
                             " "
                             (mapconcat #'identity people " ")))
    t))

(defun erc-cmd-OP (&rest people)
  "Add the operator setting to users(s) given in PEOPLE."
  (when (> (length people) 0)
    (erc-server-send (concat "MODE " (erc-default-target)
                             " +"
                             (make-string (length people) ?o)
                             " "
                             (mapconcat #'identity people " ")))
    t))

(defun erc-cmd-OPME ()
  "Ask ChanServ to op the current nick in the current channel.

This command assumes a ChanServ (channel service) available on
the IRC network which accepts an \"op\" command that takes the
channel name and the user's nick, and that the current nick is
allowed to become an operator in the current channel (typically
means that the user has a +o flag in the channel's access list)."
  (erc-message "PRIVMSG"
               (format "ChanServ op %s %s"
                       (erc-default-target)
                       (erc-current-nick))
               nil))

(defun erc-cmd-DEOPME ()
  "Deop the current nick in the current channel."
  (erc-cmd-DEOP (erc-current-nick)))

(defun erc-cmd-TIME (&optional line)
  "Request the current time and date from the current server."
  (cond
   ((and line (string-match "^\\s-*\\(.*\\)$" line))
    (let ((args (match-string 1 line)))
      (erc-log (format "cmd: TIME: %s" args))
      (erc-server-send (concat "TIME " args)))
    t)
   (t (erc-server-send "TIME"))))
(defalias 'erc-cmd-DATE #'erc-cmd-TIME)

(defun erc-cmd-MOTD (&optional target)
  "Ask server to send the current MOTD.
Some IRCds simply ignore TARGET."
  (letrec ((oneoff (lambda (proc parsed)
                     (with-current-buffer (erc-server-buffer)
                       (cl-assert (eq (current-buffer) (process-buffer proc)))
                       (remove-hook 'erc-server-402-functions h402 t)
                       (remove-hook 'erc-server-376-functions h376 t)
                       (remove-hook 'erc-server-422-functions h422 t))
                     (erc-server-MOTD proc parsed)
                     t))
           (h402 (erc-once-with-server-event 402 oneoff))
           (h376 (erc-once-with-server-event 376 oneoff))
           (h422 (erc-once-with-server-event 422 oneoff)))
    (erc-server-send (concat "MOTD" (and target " ") target))))

(defun erc-cmd-TOPIC (topic)
  "Set or request the topic for a channel.
LINE has the format: \"#CHANNEL TOPIC\", \"#CHANNEL\", \"TOPIC\"
or the empty string.

If no #CHANNEL is given, the default channel is used.  If TOPIC is
given, the channel topic is modified, otherwise the current topic will
be displayed."
  (cond
   ;; /topic #channel TOPIC
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\s-\\(.*\\)$" topic)
    (let ((ch (match-string 1 topic))
          (topic (match-string 2 topic)))
      ;; Ignore all-whitespace topics.
      (unless (equal (string-trim topic) "")
	(erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
	(erc-server-send (format "TOPIC %s :%s" ch topic) nil ch)))
    t)
   ;; /topic #channel
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)" topic)
    (let ((ch (match-string 1 topic)))
      (erc-server-send (format "TOPIC %s" ch) nil ch)
      t))
   ;; /topic
   ((string-match "^\\s-*$" topic)
    (let ((ch (erc-default-target)))
      (erc-server-send (format "TOPIC %s" ch) nil ch)
      t))
   ;; /topic TOPIC
   ((string-match "^\\s-*\\(.*\\)$" topic)
    (let ((ch (erc-default-target))
          (topic (match-string 1 topic)))
      (if (and ch (erc-channel-p ch))
          (progn
            (erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
            (erc-server-send (format "TOPIC %s :%s" ch topic) nil ch))
        (erc-display-message nil '(notice error) (current-buffer) 'no-target)))
    t)
   (t nil)))
(defalias 'erc-cmd-T #'erc-cmd-TOPIC)
(put 'erc-cmd-TOPIC 'do-not-parse-args t)

(defun erc-cmd-APPENDTOPIC (topic)
  "Append TOPIC to the current channel topic, separated by a space."
  (let ((oldtopic erc-channel-topic))
    ;; display help when given no arguments
    (when (string-match "^\\s-*$" topic)
      (signal 'wrong-number-of-arguments nil))
    ;; strip trailing ^O
    (when (string-match "\\(.*\\)\C-o" oldtopic)
      (erc-cmd-TOPIC (concat (match-string 1 oldtopic) topic)))))
(defalias 'erc-cmd-AT #'erc-cmd-APPENDTOPIC)
(put 'erc-cmd-APPENDTOPIC 'do-not-parse-args t)

(defun erc-cmd-CLEARTOPIC (&optional channel)
  "Clear the topic for a CHANNEL.
If CHANNEL is not specified, clear the topic for the default channel."
  (interactive "sClear topic of channel (RET is current channel): ")
  (let ((chnl (or (and (erc-channel-p channel) channel) (erc-default-target))))
    (when chnl
      (erc-server-send (format "TOPIC %s :" chnl))
      t)))

;;; Banlists

(defvar-local erc-channel-banlist nil
  "A list of bans seen for the current channel.
Entries are cons cells of the form (OP . MASK), where OP is the channel
operator who issued the ban.  Modules needing such a list should call
`erc-sync-banlist' once per session in the channel before accessing the
variable.  Interactive users need only issue a /BANLIST.  Note that
older versions of ERC relied on a deprecated convention involving a
property of the symbol `erc-channel-banlist' to indicate whether a ban
list had been received in full; this was found to be unreliable.")
(put 'erc-channel-banlist 'received-from-server nil)

(defvar-local erc--channel-banlist-synchronized-p nil
  "Whether the full channel ban list has been fetched since joining.")

(defun erc-sync-banlist (&optional done-fn)
  "Initialize syncing of current channel's `erc-channel-banlist'.
Arrange for it to remain synced for the rest of the IRC session.  When
DONE-FN is non-nil, call it with no args once fully updated. Expect it
to return non-nil, if necessary, to inhibit further processing."
  (unless (erc-channel-p (current-buffer))
    (error "Not a channel buffer"))
  (let ((channel (erc-target))
        (buffer (current-buffer))
        (hook (lambda (&rest r) (apply #'erc-banlist-store r) t)))
    (setq erc-channel-banlist nil)
    (erc-with-server-buffer
      (add-hook 'erc-server-367-functions hook -98 t)
      (erc-once-with-server-event
       368 (lambda (&rest _)
             (remove-hook 'erc-server-367-functions hook t)
             (with-current-buffer buffer
               (prog1 (if done-fn (funcall done-fn) t)
                 (setq erc--channel-banlist-synchronized-p t)))))
      (erc-server-send (format "MODE %s b" channel)))))

(defun erc--wrap-banlist-cmd (slashcmd)
  (lambda ()
    (put 'erc-channel-banlist 'received-from-server t)
    (unwind-protect (funcall slashcmd)
      (put 'erc-channel-banlist 'received-from-server nil))
    t))

(defvar erc-banlist-fill-padding 1.0
  "Scaling factor from 0 to 1 of free space between entries, if any.")

(cl-defgeneric erc--determine-fill-column-function ()
  fill-column)

(defun erc-cmd-BANLIST (&rest args)
  "Print the list of ban masks for the current channel.
When uninitialized or with option -f, resync `erc-channel-banlist'."
  (cond
   ((not (erc-channel-p (current-buffer)))
    (erc-display-message nil 'notice 'active "You're not on a channel\n"))
   ((or (equal args '("-f"))
        (and (not erc--channel-banlist-synchronized-p)
             (not (get 'erc-channel-banlist 'received-from-server))))
    (erc-sync-banlist (erc--wrap-banlist-cmd #'erc-cmd-BANLIST)))
   ((null erc-channel-banlist)
    (erc-display-message nil 'notice 'active
                         (format "No bans for channel: %s\n" (erc-target))))
   ((let ((max-width (erc--determine-fill-column-function))
          (lw 0) (rw 0) separator fmt)
      (dolist (entry erc-channel-banlist)
        (setq rw (max (length (car entry)) rw)
              lw (max (length (cdr entry)) lw)))
      (let ((maxw (* 1.0 (min max-width (+ rw lw)))))
        (when (< maxw (+ rw lw)) ; scale down when capped
          (cl-psetq rw (/ (* rw maxw) (* 1.0 (+ rw lw)))
                    lw (/ (* lw maxw) (* 1.0 (+ rw lw)))))
        (when-let* ((larger (max rw lw)) ; cap ratio at 3:1
                    (wavg (* maxw 0.75))
                    ((> larger wavg)))
          (setq rw (if (eql larger rw) wavg (- maxw wavg))
                lw (- maxw rw)))
        (cl-psetq rw (+ rw (* erc-banlist-fill-padding
                              (- (/ (* rw max-width) maxw) rw)))
                  lw (+ lw (* erc-banlist-fill-padding
                              (- (/ (* lw max-width) maxw) lw)))))
      (setq rw (truncate rw)
            lw (truncate lw))
      (cl-assert (<= (+ rw lw) max-width))
      (setq separator (make-string (+ rw lw 1) ?=)
            fmt (concat "%-" (number-to-string lw) "s "
                        "%" (number-to-string rw) "s"))
      (erc-display-message
       nil 'notice 'active
       (format "Ban list for channel: %s%s\n" (erc-target)
               (if erc--channel-banlist-synchronized-p " (cached)" "")))
      (erc-display-line separator 'active)
      (erc-display-line (format fmt "Ban Mask" "Banned By") 'active)
      (erc-display-line separator 'active)
      (dolist (entry erc-channel-banlist)
        (erc-display-line
         (format fmt (truncate-string-to-width (cdr entry) lw)
                 (truncate-string-to-width (car entry) rw))
         'active))
      (erc-display-message nil 'notice 'active "End of Ban list"))))
  (put 'erc-channel-banlist 'received-from-server nil)
  t)

(defalias 'erc-cmd-BL #'erc-cmd-BANLIST)

(defun erc-cmd-MASSUNBAN (&rest args)
  "Remove all bans in the current channel."
  (let ((chnl (erc-default-target)))
    (cond
     ((not (erc-channel-p chnl))
      (erc-display-message nil 'notice 'active "You're not on a channel\n"))
     ((or (equal args '("-f"))
          (and (not erc--channel-banlist-synchronized-p)
               (not (get 'erc-channel-banlist 'received-from-server))))
      (erc-sync-banlist (erc--wrap-banlist-cmd #'erc-cmd-MASSUNBAN)))
     (t (let ((bans (mapcar #'cdr erc-channel-banlist)))
          (when bans
            ;; Glob the bans into groups of three, and carry out the unban.
            ;; eg. /mode #foo -bbb a*!*@* b*!*@* c*!*@*
            (mapc
             (lambda (x)
               (erc-server-send
                (format "MODE %s -%s %s" (erc-default-target)
                        (make-string (length x) ?b)
                        (mapconcat #'identity x " "))))
             (erc-group-list bans 3))))))
    (put 'erc-channel-banlist 'received-from-server nil)
    t))

(defalias 'erc-cmd-MUB #'erc-cmd-MASSUNBAN)

;;;; End of IRC commands

(defun erc-ensure-channel-name (channel)
  "Return CHANNEL if it is a valid channel name.
Eventually add a # in front of it, if that turns it into a valid channel name."
  (if (erc-channel-p channel)
      channel
    (concat "#" channel)))

(defvar erc--own-property-names
  `( tags erc--speaker erc-parsed display ; core
     ;; `erc--msg-props'
     ,@erc--ranked-properties
     ;; `erc-display-prompt'
     rear-nonsticky erc-prompt field front-sticky read-only
     ;; stamp
     cursor-intangible cursor-sensor-functions isearch-open-invisible
     ;; match
     invisible intangible
     ;; button
     erc-callback erc-data mouse-face keymap
     ;; fill-wrap
     line-prefix wrap-prefix)
  "Props added by ERC that should not survive killing.
Among those left behind by default are `font-lock-face' and
`erc-secret'.")

(defun erc--remove-text-properties (string)
  "Remove text properties in STRING added by ERC.
Specifically, remove any that aren't members of
`erc--own-property-names'."
  (remove-list-of-text-properties 0 (length string)
                                  erc--own-property-names string)
  string)

(defun erc-grab-region (start end)
  "Copy the region between START and END in a recreatable format.

Converts all the IRC text properties in each line of the region
into control codes and writes them to a separate buffer.  The
resulting text may be used directly as a script to generate this
text again."
  (interactive "r")
  (erc-set-active-buffer (current-buffer))
  (save-excursion
    (let* ((cb (current-buffer))
           (buf (generate-new-buffer erc-grab-buffer-name))
           (region (buffer-substring start end))
           (lines (erc-split-multiline-safe region)))
      (set-buffer buf)
      (dolist (line lines)
        (insert (concat line "\n")))
      (set-buffer cb)
      (switch-to-buffer-other-window buf)))
  (message "erc-grab-region doesn't grab colors etc. anymore. If you use this, please tell the maintainers.")
  (ding))

(defun erc-display-prompt (&optional buffer pos prompt face)
  "Display PROMPT in BUFFER at position POS.
Display an ERC prompt in BUFFER.

If PROMPT is nil, one is constructed with the function `erc-prompt'.
If BUFFER is nil, the `current-buffer' is used.
If POS is nil, PROMPT will be displayed at `point'.
If FACE is non-nil, it will be used to propertize the prompt.  If it is nil,
`erc-prompt-face' will be used."
  (let* ((prompt (or prompt (erc-prompt)))
         (l (length prompt))
         (ob (current-buffer)))
    ;; We cannot use save-excursion because we move point, therefore
    ;; we resort to the ol' ob trick to restore this.
    (when (and buffer (bufferp buffer))
      (set-buffer buffer))

    ;; now save excursion again to store where point and mark are
    ;; in the current buffer
    (save-excursion
      (setq pos (or pos (point)))
      (goto-char pos)
      (when (> l 0)
        ;; Do not extend the text properties when typing at the end
        ;; of the prompt, but stuff typed in front of the prompt
        ;; shall remain part of the prompt.
        (setq prompt (apply #'propertize prompt erc--prompt-properties))
        (erc-put-text-property 0 (1- (length prompt))
                               'font-lock-face (or face 'erc-prompt-face)
                               prompt)
        (insert prompt))
      ;; Set the input marker
      (set-marker erc-input-marker (point)))

    ;; Now we are back at the old position.  If the prompt was
    ;; inserted here or before us, advance point by the length of
    ;; the prompt.
    (when (or (not pos) (<= (point) pos))
      (forward-char l))
    ;; Clear the undo buffer now, so the user can undo his stuff,
    ;; but not the stuff we did. Sneaky!
    (setq buffer-undo-list nil)
    (set-buffer ob)))

;; interactive operations

(defun erc-input-message ()
  "Read input from the minibuffer."
  (interactive)
  (let ((minibuffer-allow-text-properties t)
        (read-map minibuffer-local-map))
    (insert (read-from-minibuffer "Message: "
                                  (string last-command-event)
				  read-map))
    (erc-send-current-line)))

(defvar erc-action-history-list ()
  "History list for interactive action input.")

(defun erc-input-action ()
  "Interactively input a user action and send it to IRC."
  (interactive "")
  (erc-set-active-buffer (current-buffer))
  (let ((action (read-string "Action: " nil 'erc-action-history-list)))
    (if (not (string-match "^\\s-*$" action))
        (erc-send-action (erc-default-target) action))))

(defun erc-join-channel (channel &optional key)
  "Join CHANNEL.

If `point' is at the beginning of a channel name, use that as default."
  (interactive
   (list
    (let ((chnl (if (looking-at "\\([&#+!][^ \n]+\\)") (match-string 1) ""))
          (table (when (erc-server-buffer-live-p)
                   (set-buffer (process-buffer erc-server-process))
                   erc-channel-list)))
      (completing-read (format-prompt "Join channel" chnl)
                       table nil nil nil nil chnl))
    (when (or current-prefix-arg erc-prompt-for-channel-key)
      (read-string "Channel key (RET for none): "))))
  (erc-cmd-JOIN channel (when (>= (length key) 1) key)))

(defun erc-part-from-channel (reason)
  "Part from the current channel and prompt for a REASON."
  (interactive
    ;; FIXME: Has this ever worked?  We're in the interactive-spec, so the
    ;; argument `reason' can't be in scope yet!
    ;;(if (and (boundp 'reason) (stringp reason) (not (string= reason "")))
    ;;    reason
   (list
    (read-string (concat "Reason for leaving " (erc-default-target) ": "))))
  (erc-cmd-PART (concat (erc-default-target)" " reason)))

(defun erc-set-topic (topic)
  "Prompt for a TOPIC for the current channel."
  (interactive
   (list
    (read-string
     (concat "Set topic of " (erc-default-target) ": ")
     (when erc-channel-topic
       (let ((ss (split-string erc-channel-topic "\C-o")))
         (cons (apply #'concat (if (cdr ss) (butlast ss) ss))
               0))))))
  (let ((topic-list (split-string topic "\C-o"))) ; strip off the topic setter
    (erc-cmd-TOPIC (concat (erc-default-target) " " (car topic-list)))))

(defun erc-set-channel-limit (&optional limit)
  "Set a LIMIT for the current channel.  Remove limit if nil.
Prompt for one if called interactively."
  (interactive (list (read-string
                      (format "Limit for %s (RET to remove limit): "
                              (erc-default-target)))))
  (let ((tgt (erc-default-target)))
    (erc-server-send (if (and limit (>= (length limit) 1))
                         (format "MODE %s +l %s" tgt limit)
                       (format "MODE %s -l" tgt)))))

(defun erc-set-channel-key (&optional key)
  "Set a KEY for the current channel.  Remove key if nil.
Prompt for one if called interactively."
  (interactive (list (read-string
                      (format "Key for %s (RET to remove key): "
                              (erc-default-target)))))
  (let ((tgt (erc-default-target)))
    (erc-server-send (if (and key (>= (length key) 1))
                         (format "MODE %s +k %s" tgt key)
                       (format "MODE %s -k" tgt)))))

(defun erc-quit-server (reason)
  "Disconnect from current server after prompting for REASON.
`erc-quit-reason' works with this just like with `erc-cmd-QUIT'."
  (interactive (list (read-string
                      (format "Reason for quitting %s: "
                              (or erc-server-announced-name
                                  erc-session-server)))))
  (erc-cmd-QUIT reason))

;; Movement of point

(defun erc-bol ()
  "Move `point' to the beginning of the current line.

This places `point' just after the prompt, or at the beginning of the line."
  (interactive)
  (forward-line 0)
  (when (get-text-property (point) 'erc-prompt)
    (goto-char erc-input-marker))
  (point))

(defun erc-kill-input ()
  "Kill current input line using `erc-bol' followed by `kill-line'."
  (interactive)
  (when (and (erc-bol)
             (/= (point) (point-max))) ;; Prevent a (ding) and an error when
    ;; there's nothing to kill
    (if (boundp 'erc-input-ring-index)
        (setq erc-input-ring-index nil))
    (kill-line)))

(defvar erc--tab-functions nil
  "Functions to try when user hits \\`TAB' outside of input area.
Called with a numeric prefix arg.")

(defun erc-tab (arg)
  "Call `completion-at-point' when typing in the input area.
Otherwise call members of `erc--tab-functions' with a numeric
prefix ARG until one of them returns non-nil."
  (interactive "p")
  (if (>= (point) erc-input-marker)
      (completion-at-point)
    (run-hook-with-args-until-success 'erc--tab-functions arg)))

(defun erc-complete-word-at-point ()
  (run-hook-with-args-until-success 'erc-complete-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                        IRC SERVER INPUT HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; New Input parsing

; Stolen from ZenIRC. I just wanna test this code, so here is
; experiment area.

;; This shouldn't be a user option but remains so for compatibility.
(define-obsolete-variable-alias
  'erc-default-server-hook 'erc-default-server-functions "30.1")
(defcustom erc-default-server-functions '(erc-handle-unknown-server-response)
  "Abnormal hook for incoming messages without their own handlers.
See `define-erc-response-handler' for more."
  :package-version '(ERC . "5.6")
  :group 'erc-server-hooks
  :type 'hook)

(defun erc-default-server-handler (proc parsed)
  "Default server handler.

Displays PROC and PARSED appropriately using `erc-display-message'."
  (declare (obsolete erc-handle-unknown-server-response "29.1"))
  (erc-display-message
   parsed 'notice proc
   (mapconcat
    #'identity
    (let (res)
      (mapc (lambda (x)
              (if (stringp x)
                  (setq res (append res (list x)))))
            parsed)
      res)
    " ")))

(defvar erc-server-vectors
  '(["msgtype" "sender" "to" "arg1" "arg2" "arg3" "..."])
  "List of received server messages which ERC does not specifically handle.
See `erc-debug-missing-hooks'.")
;(make-variable-buffer-local 'erc-server-vectors)

(defun erc-debug-missing-hooks (_proc parsed)
  "Add PARSED server message ERC does not yet handle to `erc-server-vectors'.
These vectors can be helpful when adding new server message handlers to ERC.
See `erc-default-server-hook'."
  (setq erc-server-vectors `(,@erc-server-vectors ,parsed))
  nil)

(defun erc--open-target (target)
  "Open an ERC buffer on TARGET."
  (erc-open erc-session-server
            erc-session-port
            (erc-current-nick)
            erc-session-user-full-name
            nil
            nil
            (list target)
            target
            erc-server-process
            nil
            erc-session-username
            (erc-networks--id-given erc-networks--id)))

(defun erc-query (target server-buffer)
  "Open a query buffer on TARGET using SERVER-BUFFER.
To change how this query window is displayed, use `let' to bind
`erc-join-buffer' before calling this."
  (declare (obsolete "call `erc-open' in a live server buffer" "29.1"))
  (unless (buffer-live-p server-buffer)
    (error "Couldn't switch to server buffer"))
  (with-current-buffer server-buffer
    (erc--open-target target)))

(defvaralias 'erc-auto-query 'erc-receive-query-display)
(defcustom erc-receive-query-display 'window-noselect
  "If non-nil, create a query buffer each time you receive a private message.
If the buffer doesn't already exist, it is created.

This can be set to a symbol, to control how the new query window
should appear.  The default behavior is to display the buffer in
a new window but not to select it.  See the documentation for
`erc-buffer-display' for a description of available values.

Note that the legacy behavior of forgoing buffer creation
entirely when this option is nil requires setting the
compatibility flag `erc-receive-query-display-defer' to nil.  Use
`erc-ensure-target-buffer-on-privmsg' to achieve the same effect."
  :package-version '(ERC . "5.6")
  :group 'erc-buffers
  :group 'erc-query
  :type erc--buffer-display-choices)

(defvar erc-receive-query-display-defer t
  "How to interpret a null `erc-receive-query-display'.
When this variable is non-nil, ERC defers to `erc-buffer-display'
upon seeing a nil value for `erc-receive-query-display', much
like it does with other buffer-display options, like
`erc-interactive-display'.  Otherwise, when this option is nil,
ERC retains the legacy behavior of not creating a new query
buffer.")

(defvaralias 'erc-query-on-unjoined-chan-privmsg
  'erc-ensure-target-buffer-on-privmsg)
(defcustom erc-ensure-target-buffer-on-privmsg t
  "When non-nil, create a target buffer upon receiving a PRIVMSG.
This includes PRIVMSGs directed to channels.  If you are using an IRC
bouncer, such as dircproxy, to keep a log of channels when you are
disconnected, you should set this option to t.

For queries (direct messages), this option's non-nil meaning is
straightforward: if a buffer doesn't exist for the sender, create
one.  For channels, the use case is more niche and usually
involves receiving playback (via commands like ZNC's
\"PLAYBUFFER\") for channels to which your bouncer is joined but
from which you've \"detached\".

Note that this option was absent from ERC 5.5 because knowledge
of its intended role was \"unavailable\" during a major
refactoring involving buffer management.  The option has since
been restored in ERC 5.6 but now also affects queries in the
manner implied above, which was lost sometime before ERC 5.4."
  :package-version '(ERC . "5.6") ; revived
  :group 'erc-buffers
  :group 'erc-query
  :type '(choice boolean
                 (const :tag "Create pseudo queries for STATUSMSGs" status)))

(defcustom erc-format-query-as-channel-p t
  "If non-nil, format text from others in a query buffer like in a channel.
Otherwise format like a private message."
  :group 'erc-query
  :type 'boolean)

(defcustom erc-minibuffer-notice nil
  "If non-nil, print ERC notices for the user in the minibuffer.
Only happens when the session buffer isn't visible."
  :group 'erc-display
  :type 'boolean)

(defcustom erc-minibuffer-ignored nil
  "If non-nil, print a message in the minibuffer if we ignored something."
  :group 'erc-ignore
  :type 'boolean)

(defun erc-wash-quit-reason (reason nick login host)
  "Remove duplicate text from quit REASON.
Specifically in relation to NICK (user@host) information.  Returns REASON
unmodified if nothing can be removed.
E.g. \"Read error to Nick [user@some.host]: 110\" would be shortened to
\"Read error: 110\".  The same applies for \"Ping Timeout\"."
  (setq nick (regexp-quote nick)
        login (regexp-quote login)
        host (regexp-quote host))
  (or (when (string-match (concat "^\\(Read error\\) to "
                                  nick "\\[" host "\\]: "
                                  "\\(.+\\)$")
			  reason)
        (concat (match-string 1 reason) ": " (match-string 2 reason)))
      (when (string-match (concat "^\\(Ping timeout\\) for "
                                  nick "\\[" host "\\]$")
			  reason)
        (match-string 1 reason))
      reason))

(cl-defmethod erc--nickname-in-use-make-request (_nick temp)
  "Request nickname TEMP in place of rejected NICK."
  (erc-cmd-NICK temp))

(defun erc-nickname-in-use (nick reason)
  "If NICK is unavailable, tell the user the REASON.

See also `erc-display-error-notice'."
  (if (or (not erc-try-new-nick-p)
          ;; how many default-nicks are left + one more try...
          (eq erc-nick-change-attempt-count
              (if (consp erc-nick)
                  (+ (length erc-nick) 1)
                1)))
      (erc-display-error-notice
       nil
       (format "Nickname %s is %s, try another." nick reason))
    (setq erc-nick-change-attempt-count (+ erc-nick-change-attempt-count 1))
    (let ((newnick (nth 1 erc-default-nicks))
          (nicklen (erc-with-server-buffer
                     (erc--get-isupport-entry 'NICKLEN 'single))))
      (setq erc-bad-nick t)
      ;; try to use a different nick
      (if erc-default-nicks
          (setq erc-default-nicks (cdr erc-default-nicks)))
      (if (not newnick)
          (setq newnick (concat (truncate-string-to-width
                                 nick
                                 (if (and erc-server-connected nicklen)
                                     (- (string-to-number nicklen)
                                        (length erc-nick-uniquifier))
                                   ;; rfc2812 max nick length = 9
                                   ;; we must assume this is the
                                   ;; server's setting if we haven't
                                   ;; established a connection yet
                                   (- 9 (length erc-nick-uniquifier))))
				erc-nick-uniquifier)))
      (erc--nickname-in-use-make-request nick newnick)
      (erc-display-error-notice
       nil
       (format "Nickname %s is %s, trying %s"
               nick reason newnick)))))

;;; Server messages

;; FIXME remove on next major version release.  This group is all but
;; unused because most `erc-server-FOO-functions' are plain variables
;; and not user options as implied by this doc string.
(defgroup erc-server-hooks nil
  "Server event callbacks.
Every server event - like numeric replies - has its own hook.
Those hooks are all called using `run-hook-with-args-until-success'.
They receive as first argument the process object from where the event
originated from,
and as second argument the event parsed as a vector."
  :group 'erc-hooks)

(defun erc-display-server-message (_proc parsed)
  "Display the message sent by the server as a notice."
  (erc-display-message
   parsed 'notice 'active (erc-response.contents parsed)))

(defun erc-auto-query (proc parsed)
  ;; FIXME: This needs more documentation, unless it's not a user function --
  ;; Lawrence 2004-01-08
  "Put this on `erc-server-PRIVMSG-functions'."
  (when erc-auto-query
    (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
           (target (car (erc-response.command-args parsed)))
           (msg (erc-response.contents parsed))
           (query  (if (not erc-query-on-unjoined-chan-privmsg)
                       nick
                     (if (erc-current-nick-p target)
                         nick
                       target))))
      (and (not (erc-ignored-user-p (erc-response.sender parsed)))
           (or erc-query-on-unjoined-chan-privmsg
               (string= target (erc-current-nick)))
           (not (erc-get-buffer query proc))
           (not (erc-is-message-ctcp-and-not-action-p msg))
           (let ((erc-query-display erc-auto-query))
             (erc-cmd-QUERY query))
           nil))))

(make-obsolete 'erc-auto-query "try erc-cmd-QUERY instead" "29.1")

(defun erc-is-message-ctcp-p (message)
  "Check if MESSAGE is a CTCP message or not."
  (string-match "^\C-a\\([^\C-a]*\\)\C-a?$" message))

(defun erc-is-message-ctcp-and-not-action-p (message)
  "Check if MESSAGE is a CTCP message or not."
  (and (erc-is-message-ctcp-p message)
       (not (string-match "^\C-aACTION.*\C-a$" message))))

(defun erc--get-speaker-bounds ()
  "Return the bounds of `erc--speaker' text property when present.
Assume buffer is narrowed to the confines of an inserted message."
  (and-let* (((erc--check-msg-prop 'erc--spkr))
             (beg (text-property-not-all (point-min) (point-max)
                                         'erc--speaker nil)))
    (cons beg (next-single-property-change beg 'erc--speaker))))

(defvar erc--cmem-from-nick-function #'erc--cmem-get-existing
  "Function maybe returning a \"channel member\" cons from a nick.
Must return either nil or a cons of an `erc-server-user' and an
`erc-channel-user' (see `erc-channel-users') for use in
formatting a user's nick prior to insertion.  Called in the
appropriate target buffer with the downcased nick, the parsed
NUH, and the current `erc-response' object.")

(defun erc--cmem-get-existing (downcased _nuh _parsed)
  (and erc-channel-users (gethash downcased erc-channel-users)))

(defun erc-format-privmessage (nick msg privp msgp)
  "Format a PRIVMSG in an insertable fashion.

Note that as of version 5.6, the default client no longer calls this
function.  It instead defers to the `format-spec'-based message-catalog
system to handle all message formatting.  Anyone needing to influence
such formatting should describe their use case via \\[erc-bug] or
similar.  Please do this instead of resorting to things like modifying
formatting templates to remove speaker brackets (because many modules
rely on their presence, and cleaner ways exist)."
  (let* ((mark-s (if msgp (if privp "*" "<") "-"))
         (mark-e (if msgp (if privp "*" ">") "-"))
         (str    (format "%s%s%s %s" mark-s nick mark-e msg))
         (nick-face (if privp 'erc-nick-msg-face 'erc-nick-default-face))
         (nick-prefix-face (get-text-property 0 'font-lock-face nick))
         (prefix-len (or (and nick-prefix-face (text-property-not-all
                                                0 (length nick) 'font-lock-face
                                                nick-prefix-face nick))
                         0))
         (msg-face (if privp 'erc-direct-msg-face 'erc-default-face)))
    (erc--ensure-spkr-prop nick)
    ;; add text properties to text before the nick, the nick and after the nick
    (erc-put-text-property 0 (length mark-s) 'font-lock-face msg-face str)
    (erc-put-text-properties (+ (length mark-s) prefix-len)
                             (+ (length mark-s) (length nick))
                             '(font-lock-face erc--speaker) str
                             (list nick-face
                                   (substring-no-properties nick prefix-len)))
    (erc-put-text-property (+ (length mark-s) (length nick)) (length str)
                           'font-lock-face msg-face str)
    str))

;; The format strings in the following `-speaker' catalog shouldn't
;; contain any non-protocol words, so they make sense in any language.
;; Note that the following definitions generally avoid `propertize'
;; because it reverses the order of the text properties it's given.
(defvar erc--message-speaker-statusmsg
  #("(%p%n%s) %m"
    0 1 (font-lock-face erc-default-face)
    1 3 (font-lock-face erc-nick-prefix-face)
    3 5 (font-lock-face erc-nick-default-face)
    5 7 (font-lock-face erc-notice-face)
    7 11 (font-lock-face erc-default-face))
  "Message template for in-channel status messages.")

(defvar erc--message-speaker-statusmsg-input
  #("(%p%n%s) %m"
    0 1 (font-lock-face erc-default-face)
    1 3 (font-lock-face erc-my-nick-prefix-face)
    3 5 (font-lock-face erc-my-nick-face)
    5 7 (font-lock-face erc-notice-face)
    7 8 (font-lock-face erc-default-face)
    8 11 (font-lock-face erc-input-face))
  "Message template for echoed status messages.")

(defvar erc--message-speaker-input-chan-privmsg
  #("<%p%n> %m"
    0 1 (font-lock-face erc-default-face)
    1 3 (font-lock-face erc-my-nick-prefix-face)
    3 5 (font-lock-face erc-my-nick-face)
    5 7 (font-lock-face erc-default-face)
    7 9 (font-lock-face erc-input-face))
  "Message template for prompt input or echoed PRIVMSG from own nick.")

(defvar erc--message-speaker-input-query-privmsg
  #("*%n* %m"
    0 1 (font-lock-face erc-direct-msg-face)
    1 3 (font-lock-face erc-my-nick-face)
    3 5 (font-lock-face erc-direct-msg-face)
    5 7 (font-lock-face erc-input-face))
  "Message template for prompt input or echoed PRIVMSG query from own nick.")

(defvar erc--message-speaker-input-query-notice
  #("-%n- %m"
    0 1 (font-lock-face erc-direct-msg-face)
    1 3 (font-lock-face erc-my-nick-face)
    3 5 (font-lock-face erc-direct-msg-face)
    5 7 (font-lock-face erc-input-face))
  "Message template for echoed or spoofed query NOTICE from own nick.")

(defvar erc--message-speaker-input-chan-notice
  #("-%p%n- %m"
    0 1 (font-lock-face erc-default-face)
    1 3 (font-lock-face erc-my-nick-prefix-face)
    3 5 (font-lock-face erc-my-nick-face)
    5 7 (font-lock-face erc-default-face)
    7 9 (font-lock-face erc-input-face))
  "Message template for prompt input or echoed NOTICE from own nick.")

(defvar erc--message-speaker-chan-privmsg
  #("<%p%n> %m"
    0 1 (font-lock-face erc-default-face)
    1 3 (font-lock-face erc-nick-prefix-face)
    3 5 (font-lock-face erc-nick-default-face)
    5 9 (font-lock-face erc-default-face))
  "Message template for a PRIVMSG in a channel.")

(defvar erc--message-speaker-query-privmsg
  #("*%n* %m"
    0 1 (font-lock-face erc-direct-msg-face)
    1 3 (font-lock-face erc-nick-msg-face)
    3 7 (font-lock-face erc-direct-msg-face))
  "Message template for a PRIVMSG in query buffer.")

(defvar erc--message-speaker-chan-notice
  #("-%p%n- %m"
    0 1 (font-lock-face erc-default-face)
    1 3 (font-lock-face erc-nick-prefix-face)
    3 5 (font-lock-face erc-nick-default-face)
    5 9 (font-lock-face erc-default-face))
  "Message template for a NOTICE in a channel.")

(defvar erc--message-speaker-query-notice
  #("-%n- %m"
    0 1 (font-lock-face erc-direct-msg-face)
    1 3 (font-lock-face erc-nick-msg-face)
    3 7 (font-lock-face erc-direct-msg-face))
  "Message template for a NOTICE in a query buffer.")

(defvar erc--message-speaker-ctcp-action
  #("* %p%n %m"
    0 2 (font-lock-face erc-action-face)
    2 4 (font-lock-face (erc-nick-prefix-face erc-action-face))
    4 9 (font-lock-face erc-action-face))
  "Message template for a CTCP ACTION from another user.")

(defvar erc--message-speaker-ctcp-action-input
  (let ((base '(erc-input-face erc-action-face))) ; shared
    (concat (propertize "* " 'font-lock-face base)
            (propertize "%p" 'font-lock-face `(erc-my-nick-prefix-face ,@base))
            (propertize "%n" 'font-lock-face `(erc-my-nick-face ,@base))
            (propertize " %m" 'font-lock-face base)))
  "Message template for a CTCP ACTION from current client.")

(defvar erc--message-speaker-ctcp-action-statusmsg
  #("* (%p%n%s) %m"
    0 3 (font-lock-face erc-action-face)
    3 5 (font-lock-face (erc-nick-prefix-face erc-action-face))
    5 7 (font-lock-face erc-action-face)
    7 9 (font-lock-face (erc-notice-face erc-action-face))
    9 13 (font-lock-face erc-action-face))
  "Template for a CTCP ACTION status message from another chan op.")

(defvar erc--message-speaker-ctcp-action-statusmsg-input
  (let ((base '(erc-input-face erc-action-face))) ; shared
    (concat (propertize "* (" 'font-lock-face base)
            (propertize "%p" 'font-lock-face `(erc-my-nick-prefix-face ,@base))
            (propertize "%n" 'font-lock-face `(erc-my-nick-face ,@base))
            (propertize "%s" 'font-lock-face `(erc-notice-face ,@base))
            (propertize ") %m" 'font-lock-face base)))
  "Template for a CTCP ACTION status message from current client.")

(defun erc--speakerize-nick (nick &optional disp)
  "Return propertized NICK with canonical NICK in `erc--speaker'.
Return propertized DISP instead if given.  As a side effect, pair NICK
with `erc--spkr' in the \"msg prop\" environment for any imminent
`erc-display-message' invocations, and include any overrides defined in
`erc--message-speaker-catalog'.  Expect NICK (but not necessarily DISP)
to be absent of any existing text properties."
  (when-let* ((erc-server-process)
              (cusr (erc-get-server-user nick)))
    (setq nick (erc-server-user-nickname cusr)))
  (erc--ensure-spkr-prop nick (get erc--message-speaker-catalog
                                   'erc--msg-prop-overrides))
  (propertize (or disp nick) 'erc--speaker nick))

(defun erc--determine-speaker-message-format-args
    (nick message queryp privmsgp inputp &optional statusmsg prefix disp-nick)
  "Return a list consisting of a \"speaker\"-template key and spec args.
Consider the three flags QUERYP, PRIVMSGP, and INPUTP, as well as
the possibly null STATUSMSG string.  (Combined, these describe
the context of a newly arrived \"PRIVMSG\" or, when PRIVMSGP is
nil, a \"NOTICE\").  Interpret QUERYP to mean that MESSAGE is
directed at the ERC client itself (a direct message), and INPUTP
to mean MESSAGE is an outgoing or echoed message originating from
or meant to simulate prompt input.  Interpret a non-nil STATUSMSG
to mean MESSAGE should be formatted as a special channel message
intended for privileged members of the same or greater status.

After deciding on the template key for the current \"speaker\"
catalog, use the remaining arguments, possibly along with
STATUSMSG, to construct the appropriate spec-args plist forming
the returned list's tail.  In this plist, pair the char ?n with
NICK, the nickname of the speaker and ?m with MESSAGE, the
message body.  When non-nil, assume DISP-NICK to be a possibly
phony display name to take the place of NICK for ?n.  When PREFIX
is non-nil, look up NICK's channel-membership status, possibly
using PREFIX itself if it's an `erc-channel-user' object, which
it must be when called outside of a channel buffer.  Pair the
result with the ?p specifier.  When STATUSMSG is non-nil, pair it
with the ?s specifier.  Ensure unused spec values are the empty
string rather than nil."
  (when prefix
    (setq prefix (erc-get-channel-membership-prefix
                  (if (erc-channel-user-p prefix) prefix nick))))
  (when (and queryp erc--target erc-format-query-as-channel-p
             (not (erc--target-channel-p erc--target)))
    (setq queryp nil))
  (list (cond (statusmsg (if inputp 'statusmsg-input 'statusmsg))
              (privmsgp (if queryp
                            (if inputp 'input-query-privmsg 'query-privmsg)
                          (if inputp 'input-chan-privmsg 'chan-privmsg)))
              (t (if queryp
                     (if inputp 'input-query-notice 'query-notice)
                   (if inputp 'input-chan-notice 'chan-notice))))
        ?p (or prefix "") ?n (erc--speakerize-nick nick disp-nick)
        ?s (or statusmsg "") ?m message))

(defcustom erc-show-speaker-membership-status nil
  "Whether to prefix speakers with their channel status.
For example, when this option is non-nil and some nick \"Alice\"
has operator status in the current channel, ERC displays their
leading \"speaker\" label as <@Alice> instead of <Alice>."
  :package-version '(ERC . "5.6")
  :group 'erc-display
  :type 'boolean)

(define-obsolete-variable-alias 'erc-format-nick-function
  'erc-speaker-from-channel-member-function "30.1")
(defcustom erc-speaker-from-channel-member-function
  #'erc-determine-speaker-from-user
  "Function to determine a message's displayed \"speaker\" label.
Called with an `erc-server-user' object and an `erc-channel-user'
object, both possibly nil.  Use this option to do things like
provide localized display names.  To ask ERC to prepend
channel-membership \"status\" prefixes, like \"@\", to the
returned name, see `erc-show-speaker-membership-status'."
  :package-version '(ERC . "5.6")
  :group 'erc-display
  :type '(choice (function-item erc-determine-speaker-from-user) function))

(define-obsolete-function-alias 'erc-format-nick
  #'erc-determine-speaker-from-user "30.1")
(defun erc-determine-speaker-from-user (&optional user _channel-data)
  "Return nickname slot of `erc-server-user' USER, when non-nil."
  (when user (erc-server-user-nickname user)))

(define-obsolete-function-alias 'erc-get-user-mode-prefix
  #'erc-get-channel-membership-prefix "30.1")
(defun erc-get-channel-membership-prefix (nick-or-cusr)
  "Return channel membership prefix for NICK-OR-CUSR as a string.
Ensure returned string has a `help-echo' text property with the
corresponding verbose membership type, like \"voice\", as its
value.  Expect NICK-OR-CUSR to be an `erc-channel-user' object or
a string nickname, not necessarily downcased.  When called in a
logically connected ERC buffer, use advertised prefix mappings.
For compatibility reasons, don't error when NICK-OR-CUSR is null,
but return nil instead of the empty string.  Otherwise, always
return a possibly empty string."
  (when nick-or-cusr
    (when (stringp nick-or-cusr)
      (setq nick-or-cusr (and erc-channel-members
                              (cdr (erc-get-channel-member nick-or-cusr)))))
    (cond
     ((null nick-or-cusr) "")
     ;; Special-case most common value.
     ((zerop (erc-channel-user-status nick-or-cusr)) "")
     ;; For compatibility, first check whether a parsed prefix exists.
     ((and-let* ((pfx-obj (erc--parsed-prefix)))
        (catch 'done
          (pcase-dolist (`(,letter . ,pfx)
                         (erc--parsed-prefix-alist pfx-obj))
            (when (erc--cusr-status-p nick-or-cusr letter)
              (throw 'done
                     (pcase letter
                       (?q (propertize (string pfx) 'help-echo "owner"))
                       (?a (propertize (string pfx) 'help-echo "admin"))
                       (?o (propertize (string pfx) 'help-echo "operator"))
                       (?h (propertize (string pfx) 'help-echo "half-op"))
                       (?v (propertize (string pfx) 'help-echo "voice"))
                       (_ (string pfx))))))
          "")))
     (t
      (cond ((erc-channel-user-owner nick-or-cusr)
             (propertize "~" 'help-echo "owner"))
            ((erc-channel-user-admin nick-or-cusr)
             (propertize "&" 'help-echo "admin"))
            ((erc-channel-user-op nick-or-cusr)
             (propertize "@" 'help-echo "operator"))
            ((erc-channel-user-halfop nick-or-cusr)
             (propertize "%" 'help-echo "half-op"))
            ((erc-channel-user-voice nick-or-cusr)
             (propertize "+" 'help-echo "voice"))
            (t ""))))))

(defun erc-format-@nick (&optional user channel-data)
  "Format the nickname of USER showing if USER has a voice, is an
operator, half-op, admin or owner.  Owners have \"~\", admins have
\"&\", operators have \"@\" and users with voice have \"+\" as a
prefix.  Use CHANNEL-DATA to determine op and voice status."
  (declare (obsolete "see option `erc-show-speaker-membership-status'" "30.1"))
  (when user
    (let ((nick (erc-server-user-nickname user)))
      (if (not erc--speaker-status-prefix-wanted-p)
          (prog1 nick
            (setq erc--speaker-status-prefix-wanted-p 'erc-format-@nick))
        (concat (propertize
                 (erc-get-channel-membership-prefix channel-data)
                 'font-lock-face 'erc-nick-prefix-face)
                nick)))))

(defun erc-format-my-nick ()
  "Return the beginning of this user's message, correctly propertized."
  (if erc-show-my-nick
      (let* ((open "<")
             (close "> ")
             (nick (erc-current-nick))
             (mode (erc-get-channel-membership-prefix nick)))
        (erc--ensure-spkr-prop nick)
        (concat
         (propertize open 'font-lock-face 'erc-default-face)
         (propertize mode 'font-lock-face 'erc-my-nick-prefix-face)
         (propertize nick 'erc--speaker nick 'font-lock-face 'erc-my-nick-face)
         (propertize close 'font-lock-face 'erc-default-face)))
    (let ((prefix "> "))
      (propertize prefix 'font-lock-face 'erc-default-face))))

(defun erc--format-speaker-input-message (message)
  "Assemble outgoing MESSAGE entered at the prompt for insertion.
Intend \"input\" to refer to interactive prompt input as well as
the group of associated message-format templates from the
\"speaker\" catalog.  Format the speaker portion in a manner
similar to that performed by `erc-format-my-nick', but use either
`erc--message-speaker-input-chan-privmsg' or
`erc--message-speaker-input-query-privmsg' as a formatting
template, with MESSAGE being the actual message body.  Return a
copy with possibly shared text-property values."
  (if-let* ((erc-show-my-nick)
            (nick (erc-current-nick))
            (pfx (erc-get-channel-membership-prefix nick))
            (erc-current-message-catalog erc--message-speaker-catalog)
            (key (if (or erc-format-query-as-channel-p
                         (erc--target-channel-p erc--target))
                     'input-chan-privmsg
                   'input-query-privmsg)))
      (progn
        (cond (erc--msg-props (puthash 'erc--msg key erc--msg-props))
              (erc--msg-prop-overrides (push (cons 'erc--msg key)
                                             erc--msg-prop-overrides)))
        (erc-format-message key ?p pfx ?n (erc--speakerize-nick nick)
                            ?m message))
    (propertize (concat "> " message) 'font-lock-face 'erc-input-face)))

(defun erc-echo-notice-in-default-buffer (s parsed buffer _sender)
  "Echo a private notice in the default buffer, namely the
target buffer specified by BUFFER, or there is no target buffer,
the server buffer.  This function is designed to be added to
either `erc-echo-notice-hook' or `erc-echo-notice-always-hook',
and always returns t."
  (erc-display-message parsed nil buffer s)
  t)

(defun erc-echo-notice-in-target-buffer (s parsed buffer _sender)
  "Echo a private notice in BUFFER, if BUFFER is non-nil.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
returns non-nil if BUFFER is non-nil."
  (if buffer
      (progn (erc-display-message parsed nil buffer s) t)
    nil))

(defun erc-echo-notice-in-minibuffer (s _parsed _buffer _sender)
  "Echo a private notice in the minibuffer.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
always returns t."
  (message "%s" (concat "NOTICE: " s))
  t)

(defun erc-echo-notice-in-server-buffer (s parsed _buffer _sender)
  "Echo a private notice in the server buffer.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
always returns t."
  (erc-display-message parsed nil nil s)
  t)

(defun erc-echo-notice-in-active-non-server-buffer (s parsed _buffer _sender)
  "Echo a private notice in the active buffer if the active
buffer is not the server buffer.  This function is designed to be
added to either `erc-echo-notice-hook' or
`erc-echo-notice-always-hook', and returns non-nil if the active
buffer is not the server buffer."
  (if (not (eq (erc-server-buffer) (erc-active-buffer)))
      (progn (erc-display-message parsed nil 'active s) t)
    nil))

(defun erc-echo-notice-in-active-buffer (s parsed _buffer _sender)
  "Echo a private notice in the active buffer.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
always returns t."
  (erc-display-message parsed nil 'active s)
  t)

(defun erc-echo-notice-in-user-buffers (s parsed _buffer sender)
  "Echo a private notice in all of the buffers for which SENDER is a member.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
returns non-nil if there is at least one buffer for which the
sender is a member.

See also: `erc-echo-notice-in-first-user-buffer',
`erc-buffer-list-with-nick'."
  (let ((buffers (erc-buffer-list-with-nick sender erc-server-process)))
    (if buffers
        (progn (erc-display-message parsed nil buffers s) t)
      nil)))

(defun erc-echo-notice-in-user-and-target-buffers (s parsed buffer sender)
  "Echo a private notice in BUFFER and in all buffers for which SENDER is a member.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
returns non-nil if there is at least one buffer for which the
sender is a member or the default target.

See also: `erc-echo-notice-in-user-buffers',
`erc-buffer-list-with-nick'."
  (let ((buffers (erc-buffer-list-with-nick sender erc-server-process)))
    (unless (memq buffer buffers) (push buffer buffers))
    (if buffers                         ;FIXME: How could it be nil?
        (progn (erc-display-message parsed nil buffers s) t)
      nil)))

(defun erc-echo-notice-in-first-user-buffer (s parsed _buffer sender)
  "Echo a private notice in one of the buffers for which SENDER is a member.
This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
returns non-nil if there is at least one buffer for which the
sender is a member.

See also: `erc-echo-notice-in-user-buffers',
`erc-buffer-list-with-nick'."
  (let ((buffers (erc-buffer-list-with-nick sender erc-server-process)))
    (if buffers
        (progn (erc-display-message parsed nil (car buffers) s) t)
      nil)))

;;; Ban manipulation

(defun erc-banlist-store (proc parsed)
  "Record ban entries for a channel."
  (pcase-let ((`(,channel ,mask ,whoset)
               (cdr (erc-response.command-args parsed))))
    ;; Determine to which buffer the message corresponds
    (let ((buffer (erc-get-buffer channel proc)))
      (with-current-buffer buffer
        (unless (member (cons whoset mask) erc-channel-banlist)
          (setq erc-channel-banlist (cons (cons whoset mask)
                                          erc-channel-banlist))))))
  nil)

;; This was a default member of `erc-server-368-functions' (nee -hook)
;; between January and June of 2003 (but not as part of any release).
(defun erc-banlist-finished (proc parsed)
  "Record that we have received the banlist."
  (declare (obsolete "uses obsolete and likely faulty logic" "31.1"))
  (let* ((channel (nth 1 (erc-response.command-args parsed)))
         (buffer (erc-get-buffer channel proc)))
    (with-current-buffer buffer
      (put 'erc-channel-banlist 'received-from-server t)))
  t)                                    ; suppress the 'end of banlist' message

(defun erc--banlist-update (statep mask)
  "Add or remove a mask from `erc-channel-banlist'."
  (if statep
      (let ((whoset (erc-response.sender erc--parsed-response)))
        (cl-pushnew (cons whoset mask) erc-channel-banlist :test #'equal))
    (let ((upcased (upcase mask)))
      (setq erc-channel-banlist
            (cl-delete-if (lambda (y) (equal (upcase (cdr y)) upcased))
                          erc-channel-banlist)))))

(defun erc-banlist-update (proc parsed)
  "Check MODE commands for bans and update the banlist appropriately."
  ;; FIXME: Possibly incorrect. -- Lawrence 2004-05-11
  (declare (obsolete "continual syncing via `erc--banlist-update'" "31.1"))
  (let* ((tgt (car (erc-response.command-args parsed)))
         (mode (erc-response.contents parsed))
         (whoset (erc-response.sender parsed))
         (buffer (erc-get-buffer tgt proc)))
    (when buffer
      (with-current-buffer buffer
        (cond ((not (get 'erc-channel-banlist 'received-from-server)) nil)
              ((string-match "^\\([+-]\\)b" mode)
               ;; This is a ban
               (cond
                ((string-match "^-" mode)
                 ;; Remove the unbanned masks from the ban list
                 (setq erc-channel-banlist
                       (cl-delete-if
                        (lambda (y)
                          (member (upcase (cdr y))
                                  (mapcar #'upcase
                                          (cdr (split-string mode)))))
                        erc-channel-banlist)))
                ((string-match "^\\+" mode)
                 ;; Add the banned mask(s) to the ban list
                 (mapc
                  (lambda (mask)
                    (unless (member (cons whoset mask) erc-channel-banlist)
                      (setq erc-channel-banlist
                            (cons (cons whoset mask) erc-channel-banlist))))
                  (cdr (split-string mode))))))))))
  nil)

;; used for the banlist cmds
(defun erc-group-list (list n)
  "Group LIST into sublists of length N."
  (cond ((null list) nil)
        ((null (nthcdr n list)) (list list))
        (t (cons (cl-subseq list 0 n) (erc-group-list (nthcdr n list) n)))))


;;; MOTD numreplies

(defun erc-handle-login ()
  "Handle the logging in process of connection."
  (unless erc-logged-in
    (setq erc-logged-in t)
    (message "Logging in as `%s'... done" (erc-current-nick))
    ;; execute a startup script
    (let ((f (erc-select-startup-file)))
      (when f
        (erc-load-script f)))))

(defun erc-connection-established (proc parsed)
  "Set user mode and run `erc-after-connect' hook in server buffer."
  (with-current-buffer (process-buffer proc)
    (unless erc-server-connected ; only once per session
      (let ((server (or erc-server-announced-name
                        (erc-response.sender parsed)))
            (nick (car (erc-response.command-args parsed)))
            (buffer (process-buffer proc)))
        (setq erc-server-connected t)
        (setq erc--server-last-reconnect-count erc-server-reconnect-count
              erc-server-reconnect-count 0)
        (setq erc--server-reconnect-display-timer
              (run-at-time erc-auto-reconnect-display-timeout nil
                           #'erc--server-last-reconnect-display-reset
                           (current-buffer)))
        (add-hook 'erc-disconnected-hook
                  #'erc--server-last-reconnect-on-disconnect nil t)
        (erc-update-mode-line)
        (erc-set-initial-user-mode nick buffer)
        (erc-server-setup-periodical-ping buffer)
        (when erc-unhide-query-prompt
          (erc-with-all-buffers-of-server erc-server-process nil
            (when (and erc--target (not (erc--target-channel-p erc--target)))
              (erc--unhide-prompt))))
        (run-hook-with-args 'erc-after-connect server nick)))))

(defun erc-set-initial-user-mode (nick buffer)
  "If `erc-user-mode' is non-nil for NICK, set the user modes.
The server buffer is given by BUFFER."
  (with-current-buffer buffer
    (when erc-user-mode
      (let* ((mode (if (functionp erc-user-mode)
                       (funcall erc-user-mode)
                     erc-user-mode))
             (groups (erc--parse-user-modes mode (erc--user-modes) t))
             (superfluous (last groups 2))
             (redundant-want (car superfluous))
             (redundant-drop (cadr superfluous)))
        (when redundant-want
          (erc-display-message nil 'notice buffer 'user-mode-redundant-add
                               ?m (apply #'string redundant-want)))
        (when redundant-drop
          (erc-display-message nil 'notice buffer 'user-mode-redundant-drop
                               ?m (apply #'string redundant-drop)))
        (when (stringp mode)
          (erc-log (format "changing mode for %s to %s" nick mode))
          (erc-server-send (format "MODE %s %s" nick mode)))))))

(defun erc-display-error-notice (parsed string)
  "Display STRING as an error notice.

See also `erc-display-message'."
  (erc-display-message
   parsed '(notice error) 'active string))

(defun erc-process-ctcp-query (proc parsed nick login host)
  ;; FIXME: This needs a proper docstring -- Lawrence 2004-01-08
  "Process a CTCP query."
  (let ((queries (delete "" (split-string (erc-response.contents parsed)
                                          "\C-a"))))
    (if (> (length queries) 4)
        (erc-display-message
         parsed (list 'notice 'error) proc 'ctcp-too-many)
      (if (= 0 (length queries))
          (erc-display-message
           parsed (list 'notice 'error) proc
           'ctcp-empty ?n nick)
        (while queries
          (let* ((type (upcase (car (split-string (car queries)))))
                 (hook (intern-soft (concat "erc-ctcp-query-" type "-hook")))
                 (erc--msg-prop-overrides `((erc--ctcp . ,(intern type))
                                            ,@erc--msg-prop-overrides)))
            (if (and hook (boundp hook))
                (if (string-equal type "ACTION")
                    (run-hook-with-args-until-success
                     hook proc parsed nick login host
                     (car (erc-response.command-args parsed))
                     (car queries))
                  (when erc-paranoid
                    (if (erc-current-nick-p
                         (car (erc-response.command-args parsed)))
                        (erc-display-message
                         parsed 'error 'active 'ctcp-request
                         ?n nick ?u login ?h host ?r (car queries))
                      (erc-display-message
                       parsed 'error 'active 'ctcp-request-to
                       ?n nick ?u login ?h host ?r (car queries)
                       ?t (car (erc-response.command-args parsed)))))
                  (run-hook-with-args-until-success
                   hook proc nick login host
                   (car (erc-response.command-args parsed))
                   (car queries)))
              (erc-display-message
               parsed (list 'notice 'error) proc
               'undefined-ctcp)))
          (setq queries (cdr queries)))))))

(defvar erc-ctcp-query-ACTION-hook '(erc-ctcp-query-ACTION))

(defun erc-ctcp-query-ACTION (proc parsed nick login host to msg)
  "Respond to a CTCP ACTION query."
  (when (string-match "^ACTION\\s-\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg))
          (buf (or (erc-get-buffer to proc)
                   (erc-get-buffer nick proc)
                   (process-buffer proc)))
          (selfp (erc-current-nick-p nick)))
      (if erc--use-language-catalog-for-ctcp-action-p
          (progn
            (erc--ensure-spkr-prop nick)
            (setq nick (propertize nick 'erc--speaker nick))
            (erc-display-message parsed (if selfp 'input 'action) buf
                                 'ACTION ?n nick ?u login ?h host ?a s))
        (let* ((obj (and (erc--ctcp-response-p parsed) parsed))
               (buffer (and obj (erc--ctcp-response-buffer obj)))
               (stsmsg (and obj (erc--ctcp-response-statusmsg obj)))
               (prefix (and obj (erc--ctcp-response-prefix obj)))
               (dispnm (and obj (erc--ctcp-response-dispname obj)))
               (erc-current-message-catalog erc--message-speaker-catalog))
          (erc-display-message
           parsed nil (or buffer buf)
           (if selfp
               (if stsmsg 'ctcp-action-statusmsg-input 'ctcp-action-input)
             (if stsmsg 'ctcp-action-statusmsg 'ctcp-action))
           ?s (or stsmsg to)
           ?p (or (and (erc-channel-user-p prefix)
                       (erc-get-channel-membership-prefix prefix))
                  "")
           ?n (erc--speakerize-nick nick dispnm)
           ?m s))))))

(defvar erc-ctcp-query-CLIENTINFO-hook '(erc-ctcp-query-CLIENTINFO))

(defun erc-ctcp-query-CLIENTINFO (_proc nick _login _host _to msg)
  "Respond to a CTCP CLIENTINFO query."
  (when (string-match "^CLIENTINFO\\(\\s-*\\|\\s-+.*\\)$" msg)
    (let ((s (erc-client-info (erc-trim-string (match-string 1 msg)))))
      (unless erc-disable-ctcp-replies
        (erc-send-ctcp-notice nick (format "CLIENTINFO %s" s)))))
  nil)

(defvar erc-ctcp-query-ECHO-hook '(erc-ctcp-query-ECHO))
(defun erc-ctcp-query-ECHO (_proc nick _login _host _to msg)
  "Respond to a CTCP ECHO query."
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg)))
      (unless erc-disable-ctcp-replies
        (erc-send-ctcp-notice nick (format "ECHO %s" s)))))
  nil)

(defvar erc-ctcp-query-FINGER-hook '(erc-ctcp-query-FINGER))
(defun erc-ctcp-query-FINGER (_proc nick _login _host _to _msg)
  "Respond to a CTCP FINGER query."
  (unless erc-disable-ctcp-replies
    (let ((s (if erc-anonymous-login
                 (format "FINGER I'm %s." (erc-current-nick))
               (format "FINGER %s (%s@%s)."
                       (user-full-name)
                       (user-login-name)
                       (system-name))))
          (ns (erc-time-diff erc-server-last-sent-time nil)))
      (when (> ns 0)
        (setq s (concat s " Idle for " (erc-sec-to-time ns))))
      (erc-send-ctcp-notice nick s)))
  nil)

(defvar erc-ctcp-query-PING-hook '(erc-ctcp-query-PING))
(defun erc-ctcp-query-PING (_proc nick _login _host _to msg)
  "Respond to a CTCP PING query."
  (when (string-match "^PING\\s-+\\(.*\\)" msg)
    (unless erc-disable-ctcp-replies
      (let ((arg (match-string 1 msg)))
        (erc-send-ctcp-notice nick (format "PING %s" arg)))))
  nil)

(defvar erc-ctcp-query-TIME-hook '(erc-ctcp-query-TIME))
(defun erc-ctcp-query-TIME (_proc nick _login _host _to _msg)
  "Respond to a CTCP TIME query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice nick (format "TIME %s" (current-time-string))))
  nil)

(defvar erc-ctcp-query-USERINFO-hook '(erc-ctcp-query-USERINFO))
(defun erc-ctcp-query-USERINFO (_proc nick _login _host _to _msg)
  "Respond to a CTCP USERINFO query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice nick (format "USERINFO %s" erc-user-information)))
  nil)

(defvar erc-ctcp-query-VERSION-hook '(erc-ctcp-query-VERSION))
(defun erc-ctcp-query-VERSION (_proc nick _login _host _to _msg)
  "Respond to a CTCP VERSION query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice
     nick (format
           "VERSION %s (\C-b%s\C-b)"
           (erc-version nil 'bold-erc)
           erc-official-location)))
  nil)

(defun erc-process-ctcp-reply (proc parsed nick login host msg)
  "Process MSG as a CTCP reply."
  (let* ((type (car (split-string msg)))
         (hook (intern-soft (concat "erc-ctcp-reply-" type "-hook")))
         ;; Help `erc-display-message' by ensuring subsequent
         ;; insertions retain the necessary props.
         (cmd (erc--get-eq-comparable-cmd (erc-response.command parsed)))
         (erc--msg-prop-overrides `((erc--ctcp . ,(and hook (intern type)))
                                    (erc--cmd . ,cmd)
                                    ,@erc--msg-prop-overrides)))
    (if (and hook (boundp hook))
        (run-hook-with-args-until-success
         hook proc nick login host
         (car (erc-response.command-args parsed)) msg)
      (erc-display-message
       parsed 'notice 'active
       'CTCP-UNKNOWN ?n nick ?u login ?h host ?m msg))))

(defvar erc-ctcp-reply-ECHO-hook '(erc-ctcp-reply-ECHO))
(defun erc-ctcp-reply-ECHO (_proc nick _login _host _to msg)
  "Handle a CTCP ECHO reply."
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil '(notice action) 'active
       'CTCP-ECHO ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-CLIENTINFO-hook '(erc-ctcp-reply-CLIENTINFO))
(defun erc-ctcp-reply-CLIENTINFO (_proc nick _login _host _to msg)
  "Handle a CTCP CLIENTINFO reply."
  (when (string-match "^CLIENTINFO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-CLIENTINFO ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-FINGER-hook '(erc-ctcp-reply-FINGER))
(defun erc-ctcp-reply-FINGER (_proc nick _login _host _to msg)
  "Handle a CTCP FINGER reply."
  (when (string-match "^FINGER\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-FINGER ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-PING-hook '(erc-ctcp-reply-PING))
(defun erc-ctcp-reply-PING (_proc nick _login _host _to msg)
  "Handle a CTCP PING reply."
  (if (not (string-match "^PING\\s-+\\([0-9.]+\\)" msg))
      nil
    (let ((time (match-string 1 msg)))
      (condition-case nil
          (let ((delta (erc-time-diff (string-to-number time) nil)))
            (erc-display-message
             nil 'notice 'active
             'CTCP-PING ?n nick
             ?t (erc-sec-to-time delta)))
        (range-error
         (erc-display-message
          nil 'error 'active
          'bad-ping-response ?n nick ?t time))))))

(defvar erc-ctcp-reply-TIME-hook '(erc-ctcp-reply-TIME))
(defun erc-ctcp-reply-TIME (_proc nick _login _host _to msg)
  "Handle a CTCP TIME reply."
  (when (string-match "^TIME\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-TIME ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-VERSION-hook '(erc-ctcp-reply-VERSION))
(defun erc-ctcp-reply-VERSION (_proc nick _login _host _to msg)
  "Handle a CTCP VERSION reply."
  (when (string-match "^VERSION\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-VERSION ?n nick ?m message)))
  nil)

(defun erc-process-away (proc away-p)
  "Toggle the away status of the user depending on the value of AWAY-P.

If nil, set the user as away.
If non-nil, return from being away."
  (let ((sessionbuf (process-buffer proc)))
    (when sessionbuf
      (with-current-buffer sessionbuf
        (when erc-away-nickname
          (erc-log (format "erc-process-away: away-nick: %s, away-p: %s"
                           erc-away-nickname away-p))
          (erc-cmd-NICK (if away-p
                            erc-away-nickname
                          erc-nick)))
        (cond
         (away-p
          (setq erc-away (current-time)))
         (t
          (let ((away-time erc-away))
            ;; away must be set to NIL BEFORE sending anything to prevent
            ;; an infinite recursion
            (setq erc-away nil)
            (with-current-buffer (erc-active-buffer)
              (when erc-public-away-p
                (erc-send-action
                 (erc-default-target)
                 (if away-time
                     (format "is back (gone for %s)"
                             (erc-sec-to-time (erc-time-diff away-time nil)))
                   "is back")))))))))
    (erc-update-mode-line)))

;;;; List of channel members handling

(defun erc-channel-begin-receiving-names ()
  "Internal function.

Used when a channel names list is about to be received.  Should
be called with the current buffer set to the channel buffer.

See also `erc-channel-end-receiving-names'."
  (setq erc-channel-new-member-names (make-hash-table :test #'equal)))

(defun erc-channel-end-receiving-names ()
  "Internal function.

Used to fix `erc-channel-users' after a channel names list has been
received.  Should be called with the current buffer set to the
channel buffer.

See also `erc-channel-begin-receiving-names'."
  (maphash (lambda (nick _user)
             (if (null (gethash nick erc-channel-new-member-names))
                 (erc-remove-channel-user nick)))
           erc-channel-users)
  (setq erc-channel-new-member-names nil))

(defun erc-parse-prefix ()
  "Return an alist of valid prefix character types and their representations.
For example, if the current ISUPPORT \"PREFIX\" is \"(ov)@+\",
return an alist `equal' to ((?v . ?+) (?o . ?@)).  For historical
reasons, ensure the ordering of the returned alist is opposite
that of the advertised parameter."
  (let* ((str (or (erc--get-isupport-entry 'PREFIX t) "(qaohv)~&@%+"))
         (i 0)
         (j (string-search ")" str))
         collected)
    (when j
      (while-let ((u (aref str (cl-incf i)))
                  ((not (=  ?\) u))))
        (push (cons u (aref str (cl-incf j))) collected)))
    collected))

(defvar-local erc--parsed-prefix nil
  "Possibly stale `erc--parsed-prefix' struct instance for the server.
Use the \"getter\" function of the same name to obtain the current
value.")

(defun erc--parsed-prefix ()
  "Return possibly cached `erc--parsed-prefix' object for the server.
Ensure the returned value describes the most recent \"PREFIX\"
parameter advertised by the current server, with the original
ordering intact.  If no such parameter has yet arrived, return a
stand-in from the fallback value \"(qaohv)~&@%+\"."
  (erc--with-isupport-data PREFIX erc--parsed-prefix
    (let ((alist (erc-parse-prefix)))
      (make-erc--parsed-prefix
       :key key
       :letters (apply #'string (map-keys alist))
       :statuses (apply #'string (map-values alist))
       :alist (nreverse alist)))))

(defun erc--get-prefix-flag (char &optional parsed-prefix from-prefix-p)
  "Return numeric rank for CHAR or nil if unknown or unsupported.
For example, given letters \"qaohv\" return 1 for ?v, 2 for ?h,
and 4 for ?o, etc.  If given, expect PARSED-PREFIX to be a
`erc--parsed-prefix' object.  With FROM-PREFIX-P, expect CHAR to
be a prefix instead."
  (and-let* ((obj (or parsed-prefix (erc--parsed-prefix)))
             (pos (erc--strpos char (if from-prefix-p
                                        (erc--parsed-prefix-statuses obj)
                                      (erc--parsed-prefix-letters obj)))))
    (ash 1 pos)))

(defun erc--init-cusr-fallback-status (voice halfop op admin owner)
  "Return channel-membership based on traditional status semantics.
Massage boolean switches VOICE, HALFOP, OP, ADMIN, and OWNER into
an internal numeric value suitable for the `status' slot of a new
`erc-channel-user' object."
  (let ((pfx (erc--parsed-prefix)))
    (+ (if voice  (if pfx (or (erc--get-prefix-flag ?v pfx) 0)  1) 0)
       (if halfop (if pfx (or (erc--get-prefix-flag ?h pfx) 0)  2) 0)
       (if op     (if pfx (or (erc--get-prefix-flag ?o pfx) 0)  4) 0)
       (if admin  (if pfx (or (erc--get-prefix-flag ?a pfx) 0)  8) 0)
       (if owner  (if pfx (or (erc--get-prefix-flag ?q pfx) 0) 16) 0))))

(defun erc--compute-cusr-fallback-status (current v h o a q)
  "Return current channel membership after toggling V H O A Q as requested.
Assume `erc--parsed-prefix' is non-nil in the current buffer.
Expect status switches V, H, O, A, Q, when non-nil, to be the
symbol `on' or `off'.  Return an internal numeric value suitable
for the `status' slot of an `erc-channel-user' object."
  (let (on off)
    (when v (push (or (erc--get-prefix-flag ?v) 0) (if (eq v 'on) on off)))
    (when h (push (or (erc--get-prefix-flag ?h) 0) (if (eq h 'on) on off)))
    (when o (push (or (erc--get-prefix-flag ?o) 0) (if (eq o 'on) on off)))
    (when a (push (or (erc--get-prefix-flag ?a) 0) (if (eq a 'on) on off)))
    (when q (push (or (erc--get-prefix-flag ?q) 0) (if (eq q 'on) on off)))
    (when on (setq current (apply #'logior current on)))
    (when off (setq current (apply #'logand current (mapcar #'lognot off)))))
  current)

(defcustom erc-channel-members-changed-hook nil
  "This hook is called every time the variable `channel-members' changes.
The buffer where the change happened is current while this hook is called."
  :group 'erc-hooks
  :type 'hook)

(defun erc--partition-prefixed-names (name)
  "From NAME, return a list of (STATUS NICK LOGIN HOST).
Expect NAME to be a prefixed name, like @bob."
  (unless (string-empty-p name)
    (let* ((status (erc--get-prefix-flag (aref name 0) nil 'from-prefix-p))
           (nick (if status (substring name 1) name)))
      (unless (string-empty-p nick)
        (list status nick nil nil)))))

(defun erc-channel-receive-names (names-string)
  "Update `erc-channel-members' from NAMES-STRING.
Expect NAMES-STRING to resemble the trailing argument of a 353
RPL_NAMREPLY.  Call internal handlers for parsing individual
names, whose expected composition may differ depending on enabled
extensions."
  (let ((names (delete "" (split-string names-string)))
        (erc-channel-members-changed-hook nil))
    (dolist (name names)
      (when-let* ((args (erc--partition-prefixed-names name)))
        (pcase-let* ((`(,status ,nick ,login ,host) args)
                     (cmem (erc-get-channel-user nick)))
          (progn
	    ;; If we didn't issue the NAMES request (consider two clients
	    ;; talking to an IRC proxy), `erc-channel-begin-receiving-names'
	    ;; will not have been called, so we have to do it here.
	    (unless erc-channel-new-member-names
	      (erc-channel-begin-receiving-names))
            (puthash (erc-downcase nick) t erc-channel-new-member-names)
            (if cmem
                (erc--update-current-channel-member cmem status nil
                                                    nick host login)
              (erc--create-current-channel-member nick status nil
                                                  nick host login)))))))
  (run-hooks 'erc-channel-members-changed-hook))

(defun erc-update-user-nick (nick &optional new-nick
                                  host login full-name info)
  "Update the stored user information for the user with nickname NICK.

See also: `erc-update-user'."
  (erc-update-user (erc-get-server-user nick) new-nick
                   host login full-name info))

(defun erc-update-user (user &optional new-nick
                             host login full-name info)
  "Update user info for USER.
USER must be an erc-server-user struct.  Any of NEW-NICK, HOST,
LOGIN, FULL-NAME, INFO which are non-nil and not equal to the
existing values for USER are used to replace the stored values in
USER.

If, and only if, a change is made,
`erc-channel-members-changed-hook' is run for each channel for
which USER is a member, and t is returned."
  (let (changed)
    (when user
      (when (and new-nick
                 (not (equal (erc-server-user-nickname user)
                             new-nick)))
        (setq changed t)
        (erc-change-user-nickname user new-nick))
      (when (and host
                 (not (equal (erc-server-user-host user) host)))
        (setq changed t)
        (setf (erc-server-user-host user) host))
      (when (and login
                 (not (equal (erc-server-user-login user) login)))
        (setq changed t)
        (setf (erc-server-user-login user) login))
      (when (and full-name
                 (not (equal (erc-server-user-full-name user)
                             full-name)))
        (setq changed t)
        (setf (erc-server-user-full-name user) full-name))
      (when (and info
                 (not (equal (erc-server-user-info user) info)))
        (setq changed t)
        (setf (erc-server-user-info user) info))
      (if changed
          (dolist (buf (erc-server-user-buffers user))
            (if (buffer-live-p buf)
                (with-current-buffer buf
                  (run-hooks 'erc-channel-members-changed-hook))))))
    changed))

(defun erc--create-current-channel-member
    (nick status timep &optional new-nick host login full-name info)
  "Add an `erc-channel-member' entry for NICK.
Create a new `erc-server-users' entry if necessary, and ensure
`erc-channel-members-changed-hook' runs exactly once, regardless.
Pass STATUS to the `erc-channel-user' constructor.  With TIMEP,
assume NICK has just spoken, and initialize `last-message-time'.
Pass NEW-NICK, HOST, LOGIN, FULL-NAME, and INFO to
`erc-update-user' if a server user exists and otherwise to the
`erc-server-user' constructor."
  (cl-assert (null (erc-get-channel-member nick)))
  (let* ((user-changed-p nil)
         (down (erc-downcase nick))
         (user (gethash down (erc-with-server-buffer erc-server-users))))
    (if user
        (progn
          (cl-pushnew (current-buffer) (erc-server-user-buffers user))
          ;; Update *after* ^ so hook has chance to run.
          (setf user-changed-p (erc-update-user user new-nick host login
                                                full-name info)))
      (erc-add-server-user nick
                           (setq user (make-erc-server-user
                                       :nickname (or new-nick nick)
                                       :host host
                                       :full-name full-name
                                       :login login
                                       :info nil
                                       :buffers (list (current-buffer))))))
    (let ((cusr (erc-channel-user--make
                 :status (or status 0)
                 :last-message-time (and timep
                                         (erc-compat--current-lisp-time)))))
      (puthash down (cons user cusr) erc-channel-users))
    ;; An existing `cusr' was changed or a new one was added, and
    ;; `user' was not updated, though possibly just created (since
    ;; `erc-update-user' runs this same hook in all a user's buffers).
    (unless user-changed-p
      (run-hooks 'erc-channel-members-changed-hook))
    t))

(defun erc--update-current-channel-member (cmem status timep &rest user-args)
  "Update existing `erc-channel-member' entry.
Set the `status' slot of the entry's `erc-channel-user' side to
STATUS and, with TIMEP, update its `last-message-time'.  When
actual changes are made, run `erc-channel-members-changed-hook',
and return non-nil."
  (cl-assert cmem)
  (let ((cusr (cdr cmem))
        (user (car cmem))
        cusr-changed-p user-changed-p)
    (when (and status (/= status (erc-channel-user-status cusr)))
      (setf (erc-channel-user-status cusr) status
            cusr-changed-p t))
    (when timep
      (setf (erc-channel-user-last-message-time cusr)
            (erc-compat--current-lisp-time)))
    ;; Ensure `erc-channel-members-changed-hook' runs on change.
    (cl-assert (memq (current-buffer) (erc-server-user-buffers user)))
    (setq user-changed-p (apply #'erc-update-user user user-args))
    ;; An existing `cusr' was changed or a new one was added, and
    ;; `user' was not updated, though possibly just created (since
    ;; `erc-update-user' runs this same hook in all a user's buffers).
    (when (and cusr-changed-p (null user-changed-p))
      (run-hooks 'erc-channel-members-changed-hook))
    (erc-log (format "update-member: user = %S, cusr = %S" user cusr))
    (or cusr-changed-p user-changed-p)))

(defun erc-update-current-channel-member
    (nick new-nick &optional addp voice halfop op admin owner host login
          full-name info update-message-time)
  "Update or create entry for NICK in current `erc-channel-members' table.
With ADDP, ensure an entry exists.  When an entry does exist or
when ADDP is non-nil and an `erc-server-users' entry already
exists, call `erc-update-user' with NEW-NICK, HOST, LOGIN,
FULL-NAME, and INFO.  Expect any non-nil membership
status switches among VOICE, HALFOP, OP, ADMIN, and OWNER to be
the symbol `on' or `off' when needing to influence a new or
existing `erc-channel-user' object's `status' slot.  Likewise,
when UPDATE-MESSAGE-TIME is non-nil, update or initialize the
`last-message-time' slot to the `current-time'.  If changes occur,
including creation, run `erc-channel-members-changed-hook'.
Return non-nil when meaningful changes, including creation, have
occurred.

Without ADDP, do nothing unless a `erc-channel-members' entry
exists.  When it doesn't, assume the sender is a non-joined
entity, like the server itself or a historical speaker, or assume
the prior buffer for the channel was killed without parting."
(let* ((cmem (erc-get-channel-member nick))
       (status (and (or voice halfop op admin owner)
                    (if cmem
                        (erc--compute-cusr-fallback-status
                         (erc-channel-user-status (cdr cmem))
                         voice halfop op admin owner)
                      (erc--init-cusr-fallback-status
                       (and voice  (eq voice  'on))
                       (and halfop (eq halfop 'on))
                       (and op     (eq op     'on))
                       (and admin  (eq admin  'on))
                       (and owner  (eq owner  'on)))))))
  (if cmem
      (erc--update-current-channel-member cmem status update-message-time
                                          new-nick host login
                                          full-name info)
    (when addp
      (erc--create-current-channel-member nick status update-message-time
                                          new-nick host login
                                          full-name info)))))

(defun erc-update-channel-member (channel nick new-nick
                                          &optional add voice halfop op admin owner host login
                                          full-name info update-message-time)
  "Update user and channel for user with nickname NICK in channel CHANNEL.

See also: `erc-update-current-channel-member'."
  (erc-with-buffer
      (channel)
    (erc-update-current-channel-member nick new-nick add voice halfop op admin owner host
                                       login full-name info
                                       update-message-time)))

(defun erc-remove-current-channel-member (nick)
  "Remove NICK from current channel membership list.
Runs `erc-channel-members-changed-hook'."
  (let ((channel-data (erc-get-channel-user nick)))
    (when channel-data
      (erc-remove-channel-user nick)
      (run-hooks 'erc-channel-members-changed-hook))))

(defun erc-remove-channel-member (channel nick)
  "Remove NICK from CHANNEL's membership list.

See also `erc-remove-current-channel-member'."
  (erc-with-buffer
      (channel)
    (erc-remove-current-channel-member nick)))

(defun erc-update-channel-topic (channel topic &optional modify)
  "Find a buffer for CHANNEL and set the TOPIC for it.

If optional MODIFY is `append' or `prepend', then append or prepend the
TOPIC string to the current topic."
  (erc-with-buffer (channel)
    (cond ((eq modify 'append)
           (setq erc-channel-topic (concat erc-channel-topic topic)))
          ((eq modify 'prepend)
           (setq erc-channel-topic (concat topic erc-channel-topic)))
          (t (setq erc-channel-topic topic)))
    (erc-update-mode-line-buffer (current-buffer))))

(defun erc-set-modes (tgt mode-string)
  "Set the modes for the TGT provided as MODE-STRING."
  (declare (obsolete "see comment atop `erc--update-modes'" "30.1"))
  (let* ((modes (with-suppressed-warnings ((obsolete erc-parse-modes))
                  (erc-parse-modes mode-string)))
         (add-modes (nth 0 modes))
         ;; list of triples: (mode-char 'on/'off argument)
         (arg-modes (nth 2 modes)))
    (cond ((erc-channel-p tgt); channel modes
           (let ((buf (and erc-server-process
                           (erc-get-buffer tgt erc-server-process))))
             (when buf
               (with-current-buffer buf
                 (setq erc-channel-modes add-modes)
                 (setq erc-channel-user-limit nil)
                 (setq erc-channel-key nil)
                 (while arg-modes
                   (let ((mode (nth 0 (car arg-modes)))
                         (onoff (nth 1 (car arg-modes)))
                         (arg (nth 2 (car arg-modes))))
                     (cond ((string-match "^[Ll]" mode)
                            (erc-update-channel-limit tgt onoff arg))
                           ((string-match "^[Kk]" mode)
                            (erc-update-channel-key tgt onoff arg))
                           (t nil)))
                   (setq arg-modes (cdr arg-modes)))
                 (erc-update-mode-line-buffer buf)))))
          ;; we do not keep our nick's modes yet
          ;;(t (setq erc-user-modes add-modes))
          )
    ))

(defun erc-sort-strings (list-of-strings)
  "Sort LIST-OF-STRINGS in lexicographic order.

Side-effect free."
  (sort (copy-sequence list-of-strings) #'string<))

(defun erc-parse-modes (mode-string)
  "Parse MODE-STRING into a list.

Returns a list of three elements:

  (ADD-MODES REMOVE-MODES ARG-MODES).

The add-modes and remove-modes are lists of single-character strings
for modes without parameters to add and remove respectively.  The
arg-modes is a list of triples of the form:

  (MODE-CHAR ON/OFF ARGUMENT)."
  (declare (obsolete "see comment atop `erc--update-modes'" "30.1"))
  (if (string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*$\\|$\\)" mode-string)
      (let ((chars (mapcar #'char-to-string (match-string 1 mode-string)))
            ;; arguments in channel modes
            (args-str (match-string 2 mode-string))
            (args nil)
            (add-modes nil)
            (remove-modes nil)
            (arg-modes nil); list of triples: (mode-char 'on/'off argument)
            (add-p t))
        ;; make the argument list
        (while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)" args-str)
          (setq args (cons (match-string 1 args-str) args))
          (setq args-str (match-string 2 args-str)))
        (setq args (nreverse args))
        ;; collect what modes changed, and match them with arguments
        (while chars
          (cond ((string= (car chars) "+") (setq add-p t))
                ((string= (car chars) "-") (setq add-p nil))
                ((string-match "^[qaovhbQAOVHB]" (car chars))
                 (setq arg-modes (cons (list (car chars)
                                             (if add-p 'on 'off)
                                             (if args (car args) nil))
                                       arg-modes))
                 (if args (setq args (cdr args))))
                ((string-match "^[LlKk]" (car chars))
                 (setq arg-modes (cons (list (car chars)
                                             (if add-p 'on 'off)
                                             (if (and add-p args)
                                                 (car args) nil))
                                       arg-modes))
                 (if (and add-p args) (setq args (cdr args))))
                (add-p (setq add-modes (cons (car chars) add-modes)))
                (t (setq remove-modes (cons (car chars) remove-modes))))
          (setq chars (cdr chars)))
        (setq add-modes (nreverse add-modes))
        (setq remove-modes (nreverse remove-modes))
        (setq arg-modes (nreverse arg-modes))
        (list add-modes remove-modes arg-modes))
    nil))

(defun erc-update-modes (tgt mode-string &optional _nick _host _login)
  "Update the mode information for TGT, provided as MODE-STRING.
Optional arguments: NICK, HOST and LOGIN - the attributes of the
person who changed the modes."
  (declare (obsolete "see comment atop `erc--update-modes'" "30.1"))
  ;; FIXME: neither of nick, host, and login are used!
  (let* ((modes (with-suppressed-warnings ((obsolete erc-parse-modes))
                  (erc-parse-modes mode-string)))
         (add-modes (nth 0 modes))
         (remove-modes (nth 1 modes))
         ;; list of triples: (mode-char 'on/'off argument)
         (arg-modes (nth 2 modes)))
    ;; now parse the modes changes and do the updates
    (cond ((erc-channel-p tgt); channel modes
           (let ((buf (and erc-server-process
                           (erc-get-buffer tgt erc-server-process))))
             (when buf
               ;; FIXME! This used to have an original buffer
               ;; variable, but it never switched back to the original
               ;; buffer. Is this wanted behavior?
               (set-buffer buf)
               (if (not (boundp 'erc-channel-modes))
                   (setq erc-channel-modes nil))
               (while remove-modes
                 (setq erc-channel-modes (delete (car remove-modes)
                                                 erc-channel-modes)
                       remove-modes (cdr remove-modes)))
               (while add-modes
                 (setq erc-channel-modes (cons (car add-modes)
                                               erc-channel-modes)
                       add-modes (cdr add-modes)))
               (setq erc-channel-modes (erc-sort-strings erc-channel-modes))
               (while arg-modes
                 (let ((mode (nth 0 (car arg-modes)))
                       (onoff (nth 1 (car arg-modes)))
                       (arg (nth 2 (car arg-modes))))
                   (cond ((string-match "^[Vv]" mode)
                          (erc-update-channel-member tgt arg arg nil onoff))
                         ((string-match "^[hH]" mode)
                          (erc-update-channel-member tgt arg arg nil nil onoff))
                         ((string-match "^[oO]" mode)
                          (erc-update-channel-member tgt arg arg nil nil nil onoff))
                         ((string-match "^[aA]" mode)
                          (erc-update-channel-member tgt arg arg nil nil nil nil onoff))
                         ((string-match "^[qQ]" mode)
                          (erc-update-channel-member tgt arg arg nil nil nil nil nil onoff))
                         ((string-match "^[Ll]" mode)
                          (erc-update-channel-limit tgt onoff arg))
                         ((string-match "^[Kk]" mode)
                          (erc-update-channel-key tgt onoff arg))
                         (t nil)); only ops are tracked now
                   (setq arg-modes (cdr arg-modes))))
               (erc-update-mode-line buf))))
          ;; nick modes - ignored at this point
          (t nil))))

(defvar-local erc--channel-modes nil
  "When non-nil, a hash table of current channel modes.
Keys are characters.  Values are either a string, for types A-C,
or t, for type D.")

(defvar-local erc--channel-mode-types nil
  "Possibly stale `erc--channel-mode-types' instance for the server.
Use the getter of the same name to retrieve the current value.")

(defvar-local erc--mode-line-mode-string nil
  "Computed mode-line or header-line component for user/channel modes.")

(defvar erc--mode-line-chanmodes-arg-len 10
  "Max length at which to truncate channel-mode args in header line.")

(defun erc--channel-mode-types ()
  "Return variable `erc--channel-mode-types', possibly initializing it."
  (erc--with-isupport-data CHANMODES erc--channel-mode-types
    (let ((types (or key '(nil "Kk" "Ll" nil)))
          (ct (make-char-table 'erc--channel-mode-types))
          (type ?a))
      (dolist (cs types)
        (seq-doseq (c cs)
          (aset ct c type))
        (cl-incf type))
      (make-erc--channel-mode-types :key key
                                    :fallbackp (null key)
                                    :table ct))))

(defun erc--process-channel-modes (string args &optional status-letters)
  "Parse channel \"MODE\" changes and call unary letter handlers.
Update `erc-channel-modes' and `erc--channel-modes'.  With
STATUS-LETTERS, also update channel membership prefixes.  Expect
STRING to be the second argument from an incoming \"MODE\"
command and ARGS to be the remaining arguments, which should
complement relevant letters in STRING."
  (cl-assert (erc--target-channel-p erc--target))
  (let* ((obj (erc--channel-mode-types))
         (table (erc--channel-mode-types-table obj))
         (fallbackp (erc--channel-mode-types-fallbackp obj))
         (+p t))
    (seq-doseq (c string)
      (cond ((= ?+ c) (setq +p t))
            ((= ?- c) (setq +p nil))
            ((and status-letters (string-search (string c) status-letters))
             (erc--cusr-change-status (pop args) c +p))
            ((and-let* ((group (or (aref table c) (and fallbackp ?d))))
               (erc--handle-channel-mode group c +p
                                         (and (/= group ?d)
                                              (or (/= group ?c) +p)
                                              (pop args)))
               t))
            ((not fallbackp)
             (erc-display-message nil '(notice error) (erc-server-buffer)
                                  'channel-mode-unknown ?c (string c)))))
    (setq erc-channel-modes (sort erc-channel-modes #'string<))
    (setq erc--mode-line-mode-string
          (concat "+" (erc--channel-modes erc--mode-line-chanmodes-arg-len)))
    (erc-update-mode-line (current-buffer))))

(defvar-local erc--user-modes nil
  "Sorted list of current user \"MODE\" letters.
Analogous to `erc-channel-modes' but chars rather than strings.")

(defun erc--user-modes (&optional as-type)
  "Return user \"MODE\" letters in a form described by AS-TYPE.
When AS-TYPE is the symbol `strings' (plural), return a list of
strings.  When it's `string' (singular), return the same list
concatenated into a single string.  When AS-TYPE is nil, return a
list of chars."
  (let ((modes (or erc--user-modes (erc-with-server-buffer erc--user-modes))))
    (pcase as-type
      ('strings (mapcar #'char-to-string modes))
      ('string (apply #'string modes))
      (_ modes))))

(defun erc--channel-modes (&optional as-type sep)
  "Return channel \"MODE\" settings in a form described by AS-TYPE.
When AS-TYPE is the symbol `strings' (plural), return letter keys
as a list of sorted string.  When it's `string' (singular),
return keys as a single string.  When it's a number N, return a
single string consisting of the concatenated and sorted keys
followed by a space and then their corresponding args, each
truncated to N chars max.  ERC joins these args together with
SEP, which defaults to a single space.  Otherwise, return a
sorted alist of letter and arg pairs.  In all cases that include
values, respect `erc-show-channel-key-p' and optionally omit the
secret key associated with the letter k."
  (and-let* ((modes erc--channel-modes)
             (tobj (erc--channel-mode-types))
             (types (erc--channel-mode-types-table tobj)))
    (let (out)
      (maphash (lambda (k v)
                 (unless (eq ?a (aref types k))
                   (push (cons k
                               (and (not (eq t v))
                                    (not (and (eq k ?k)
                                              (not (bound-and-true-p
                                                    erc-show-channel-key-p))))
                                    v))
                         out)))
               modes)
      (setq out (cl-sort out #'< :key #'car))
      (pcase as-type
        ('strings (mapcar (lambda (o) (char-to-string (car o))) out))
        ('string (apply #'string (mapcar #'car out)))
        ((and (pred natnump) c)
         (let (keys vals)
           (pcase-dolist (`(,k . ,v) out)
             (when v
               (push (if (> (length v) c)
                         (with-memoization
                             (gethash (list c k v)
                                      (erc--channel-mode-types-shortargs tobj))
                           (truncate-string-to-width v c 0 nil t))
                       v)
                     vals))
             (push k keys))
           (concat (apply #'string (nreverse keys)) (and vals " ")
                   (string-join (nreverse vals) (or sep " ")))))
        (_ out)))))

(defun erc--parse-user-modes (string &optional current extrap)
  "Return lists of chars from STRING to add to and drop from CURRENT.
Expect STRING to be a so-called \"modestring\", the second
parameter of a \"MODE\" command, here containing only valid
user-mode letters.  Expect CURRENT to be a list of chars
resembling those found in `erc--user-modes'.  With EXTRAP, return
two additional lists of chars: those that would be added were
they not already present in CURRENT and those that would be
dropped were they not already absent."
  (let ((addp t)
        ;;
        redundant-add redundant-drop adding dropping)
    (seq-doseq (c string)
      (pcase c
        (?+ (setq addp t))
        (?- (setq addp nil))
        (_ (push c (let ((hasp (and current (memq c current))))
                     (if addp
                         (if hasp redundant-add adding)
                       (if hasp dropping redundant-drop)))))))
    (if extrap
        (list (nreverse adding) (nreverse dropping)
              (nreverse redundant-add) (nreverse redundant-drop))
      (list (nreverse adding) (nreverse dropping)))))

(defun erc--update-user-modes (string)
  "Update `erc--user-modes' from \"MODE\" STRING.
Return its value, a list of characters sorted by character code."
  (prog1
      (setq erc--user-modes
            (pcase-let ((`(,adding ,dropping)
                         (erc--parse-user-modes string erc--user-modes)))
              (sort (seq-difference (nconc erc--user-modes adding) dropping)
                    #'<)))
    (setq erc--mode-line-mode-string
          (concat "+" (erc--user-modes 'string)))))

(defun erc--update-channel-modes (string &rest args)
  "Update `erc-channel-modes' and call individual mode handlers.
Also update membership prefixes, as needed.  Expect STRING to be
a \"modestring\" and ARGS to match mode-specific parameters."
  (let ((status-letters (or (erc-with-server-buffer
                              (erc--parsed-prefix-letters
                               (erc--parsed-prefix)))
                            "qaovhbQAOVHB")))
    (erc--process-channel-modes string args status-letters)))

;; XXX this comment is referenced elsewhere (grep before deleting).
;;
;; The function `erc-update-modes' was deprecated in ERC 5.6 with no
;; immediate public replacement.  Third parties needing such a thing
;; are encouraged to write to emacs-erc@gnu.org with ideas for a
;; mode-handler API, possibly one incorporating letter-specific
;; handlers, like `erc--handle-channel-mode' (below), which only
;; handles mode types A-C.
(defun erc--update-modes (raw-args)
  "Handle user or channel \"MODE\" update from server.
Expect RAW-ARGS be a list consisting of a \"modestring\" followed
by mode-specific arguments."
  (if (and erc--target (erc--target-channel-p erc--target))
      (apply #'erc--update-channel-modes raw-args)
    (erc--update-user-modes (car raw-args))))

(defun erc--init-channel-modes (channel raw-args)
  "Set CHANNEL modes from RAW-ARGS.
Expect RAW-ARGS to be a \"modestring\" without any status-prefix
chars, followed by applicable arguments."
  (erc-with-buffer (channel)
    (erc--process-channel-modes (car raw-args) (cdr raw-args))))

(cl-defgeneric erc--handle-channel-mode (type letter state arg)
  "Handle a STATE change for mode LETTER of TYPE with ARG.
Expect to be called in the affected target buffer.  Expect TYPE
to be a character, like ?a, representing an advertised
\"CHANMODES\" group.  Expect LETTER to also be a character, and
expect STATE to be a boolean and ARGUMENT either a string or nil."
  (erc-log (format "Channel-mode %c (type %s, arg %S) %s"
                   letter type arg (if state 'enabled 'disabled))))

(cl-defmethod erc--handle-channel-mode :before (type c state arg)
  "Record STATE change for mode letter C.
When STATE is non-nil, add or update C's mapping in
`erc--channel-modes', associating it with ARG if C takes a
parameter and t otherwise.  When STATE is nil, forget the
mapping.  For type A, add up update a permanent mapping for C,
associating it with an integer indicating a running total of
STATE changes since joining the channel.  In most cases, this
won't match the number known to the server."
  (unless erc--channel-modes
    (cl-assert (erc--target-channel-p erc--target))
    (setq erc--channel-modes (make-hash-table)))
  (if (= type ?a)
      (cl-callf (lambda (s) (+ (or s 0) (if state +1 -1)))
          (gethash c erc--channel-modes))
    (if state
        (puthash c (or arg t) erc--channel-modes)
      (remhash c erc--channel-modes))))

(cl-defmethod erc--handle-channel-mode :before ((_ (eql ?d)) c state _)
  "Update `erc-channel-modes' for any character C of nullary type D.
Remember when STATE is non-nil and forget otherwise."
  (setq erc-channel-modes
        (if state
            (cl-pushnew (char-to-string c) erc-channel-modes :test #'equal)
          (delete (char-to-string c) erc-channel-modes))))

;; We could specialize on type A, but that may be too brittle.
(cl-defmethod erc--handle-channel-mode (_ (_ (eql ?b)) state arg)
  "Update `erc-channel-banlist' when synchronized."
  (when erc--channel-banlist-synchronized-p (erc--banlist-update state arg)))

;; We could specialize on type C, but that may be too brittle.
(cl-defmethod erc--handle-channel-mode (_ (_ (eql ?l)) state arg)
  "Update channel user limit, remembering ARG when STATE is non-nil."
  (erc-update-channel-limit (erc--target-string erc--target)
                            (if state 'on 'off)
                            arg))

;; We could specialize on type B, but that may be too brittle.
(cl-defmethod erc--handle-channel-mode (_ (_ (eql ?k)) state arg)
  "Update channel key, remembering ARG when state is non-nil."
  ;; Mimic old parsing behavior in which an ARG of "*" was discarded
  ;; even though `erc-update-channel-limit' checks STATE first.
  (erc-update-channel-key (erc--target-string erc--target)
                          (if state 'on 'off)
                          (if (equal arg "*") nil arg)))

(defun erc-update-channel-limit (channel onoff n)
  "Update CHANNEL's user limit to N.
Expect ONOFF to be `on' when the mode is being enabled and `off'
otherwise.  And because this mode is of \"type C\", expect N to
be non-nil only when enabling."
  (if (or (not (eq onoff 'on))
          (and (stringp n) (string-match "^[0-9]+$" n)))
      (erc-with-buffer
          (channel)
        (cond ((eq onoff 'on) (setq erc-channel-user-limit (string-to-number n)))
              (t (setq erc-channel-user-limit nil))))))

(defun erc-update-channel-key (channel onoff key)
  "Update CHANNEL's key to KEY if ONOFF is `on' or to nil if it's `off'."
  (erc-with-buffer
      (channel)
    (cond ((eq onoff 'on) (setq erc-channel-key key))
          (t (setq erc-channel-key nil)))))

(defun erc-handle-user-status-change (type nlh &optional l)
  "Handle changes in any user's status.

So far, only nick change is handled.

Generally, the TYPE argument is a symbol describing the change type, NLH is
a list containing the original nickname, login name and hostname for the user,
and L is a list containing additional TYPE-specific arguments.

So far the following TYPE/L pairs are supported:

       Event                    TYPE                   L

    nickname change            `nick'                (NEW-NICK)"
  (erc-log (format "user-change: type: %S  nlh: %S  l: %S" type nlh l))
  (cond
   ;; nickname change
   ((equal type 'nick)
    t)
   (t
    nil)))

(defun erc-highlight-notice (s)
  "Highlight notice message S and return it.
See also variable `erc-notice-highlight-type'."
  (cond
   ((eq erc-notice-highlight-type 'prefix)
    (erc-put-text-property 0 (length erc-notice-prefix)
                           'font-lock-face 'erc-notice-face s)
    s)
   ((eq erc-notice-highlight-type 'all)
    (erc-put-text-property 0 (length s) 'font-lock-face 'erc-notice-face s)
    s)
   (t s)))

(defun erc-make-notice (message)
  "Notify the user of MESSAGE."
  (when erc-minibuffer-notice
    (message "%s" message))
  (erc-highlight-notice (concat erc-notice-prefix message)))

(defun erc-highlight-error (s)
  "Highlight error message S and return it."
  (erc-put-text-property 0 (length s) 'font-lock-face 'erc-error-face s)
  s)

(defun erc-put-text-property (start end property value &optional object)
  "Set text-property for an object (usually a string).
START and END define the characters covered.
PROPERTY is the text-property set, usually the symbol `face'.
VALUE is the value for the text-property, usually a face symbol such as
the face `bold' or `erc-pal-face'.
OBJECT is a string which will be modified and returned.
OBJECT is modified without being copied first.

You can redefine or `defadvice' this function in order to add
EmacsSpeak support."
  (if erc--merge-text-properties-p
      (erc--merge-prop start end property value object)
    (put-text-property start end property value object)))

(defalias 'erc-list 'ensure-list)

(defconst erc--parse-user-regexp-pedantic
  (rx bot (? (? (group (+ (not (any "!@\r\n"))))) "!")
      (? (? (group (+ nonl))) "@")
      (? (group (+ nonl))) eot))

(defconst erc--parse-user-regexp-legacy
  "^\\([^!\n]*\\)!\\([^@\n]*\\)@\\(.*\\)$")

(defvar erc--parse-user-regexp erc--parse-user-regexp-legacy)

(defun erc-parse-user (string)
  "Parse STRING as a user specification (nick!login@host).

Return a list of the three separate tokens."
  (cond
   ((string-match erc--parse-user-regexp string)
    (list (match-string 1 string)
          (match-string 2 string)
          (match-string 3 string)))
   ;; Some bogus bouncers send Nick!(null), try to live with that.
   ((string-match "^\\([^!\n]*\\)!\\(.*\\)$" string)
    (list (match-string 1 string)
          ""
          (match-string 2 string)))
   (t
    (list string "" ""))))

(defun erc--parse-nuh (string)
  "Match STRING against `erc--parse-user-regexp-pedantic'.
Return nil or matching groups representing nick, login, and host,
any of which may be nil.  Expect STRING not to contain leading
prefix chars.  Return an empty nick component to indicate further
processing is required based on context.  Interpret a lone token
lacking delimiters or one with only a leading \"!\" as a host.

See associated unit test for precise behavior."
  (when (string-match erc--parse-user-regexp-pedantic string)
    (list (match-string 1 string)
          (match-string 2 string)
          (match-string 3 string))))

(defun erc--shuffle-nuh-nickward (nick login host)
  "Interpret results of `erc--parse-nuh', promoting loners to nicks."
  (cond (nick (cl-assert (null login)) (list nick login host))
        ((and (null login) host) (list host nil nil))
        ((and login (null host)) (list login nil nil))))

(defun erc-extract-nick (string)
  "Return the nick corresponding to a user specification STRING.

See also `erc-parse-user'."
  (car (erc-parse-user string)))

(defun erc-put-text-properties (start end properties
                                      &optional object value-list)
  "Set text-properties for OBJECT.

START and END describe positions in OBJECT.
If VALUE-LIST is nil, set each property in PROPERTIES to t, else set
each property to the corresponding value in VALUE-LIST."
  (unless value-list
    (setq value-list (mapcar (lambda (_x) t)
                             properties)))
  (while (and properties value-list)
    (erc-put-text-property
     start end (pop properties) (pop value-list) object)))

;;; Input area handling:

(defun erc-beg-of-input-line ()
  "Return the value of `point' at the beginning of the input line.

Specifically, return the position of `erc-insert-marker'."
  (or (and (boundp 'erc-insert-marker)
           (markerp erc-insert-marker))
      (error "erc-insert-marker has no value, please report a bug"))
  (marker-position erc-insert-marker))

(defun erc-end-of-input-line ()
  "Return the value of `point' at the end of the input line."
  (point-max))

(defvar erc-last-input-time 0
  "Time of last successful call to `erc-send-current-line'.
If that function has never been called, the value is 0.")

(defcustom erc-accidental-paste-threshold-seconds 0.2
  "Minimum time, in seconds, before sending new lines via IRC.
If the value is a number, `erc-send-current-line' signals an error
if its previous invocation was fewer than this many seconds ago.
If the value is nil, `erc-send-current-line' always considers any
submitted line to be intentional.

This option mainly prevents text accidentally entered into Emacs
from being sent to the server.  Offending sources include
terminal multiplexers, desktop-automation scripts, and anything
capable of rapidly submitting successive lines of prompt input.
For example, if you could somehow manage to type \"one \\`RET'
two \\`RET' three \\`RET'\" at the prompt in less than
`erc-accidental-paste-threshold-seconds', ERC would send \"one\"
to the server, leave \"two\" at the prompt, and insert \"three\"
into an \"overflow\" buffer.  See `erc-inhibit-multiline-input'
and `erc-warn-about-blank-lines' for suppression involving input
yanked from the clipboard or the kill ring, which is a related
but separate concern.

Users of terminal multiplexers, in particular, should look into
support for \"bracketed pasting\", provided on the Emacs side by
libraries like `xterm' (and usually enabled by default).  When
everything's working smoothly, Emacs transparently arranges for
pasted text to appear on the kill ring, regardless of any
read-only warnings you may encounter.  And when point is in the
prompt area, ERC automatically yanks that text for previewing but
holds off on submitting it, for obvious reasons."
  :group 'erc
  :version "26.1"
  :type '(choice number (other :tag "disabled" nil)))

(defvar erc--input-line-delim-regexp (rx (| (: (? ?\r) ?\n) ?\r)))

(defvar erc-command-regexp "^/\\([A-Za-z']+\\)\\(\\s-+.*\\|\\s-*\\)$"
  "Regular expression used for matching commands in ERC.")

(defun erc--check-prompt-input-for-excess-lines (_ lines)
  "Return non-nil when trying to send too many LINES."
  (when erc-inhibit-multiline-input
    (let ((max (if (eq erc-inhibit-multiline-input t)
                   2
                 erc-inhibit-multiline-input))
          (seen 0)
          last msg)
      (while (and lines (setq last (pop lines)) (< (cl-incf seen) max)))
      (when (= seen max)
        (push last lines)
        (setq msg
              (format "-- exceeded by %d (%d chars)"
                      (length lines)
                      (apply #'+ (mapcar #'length lines))))
        (unless (and erc-ask-about-multiline-input
                     (y-or-n-p (concat "Send input " msg "?")))
          (concat "Too many lines " msg))))))

(defun erc--check-prompt-input-for-something (string _)
  (when (string-empty-p string)
    (if erc-warn-about-blank-lines
        "Blank line - ignoring..."
      'invalid)))

(defun erc--count-blank-lines (lines)
  "Report on the number of whitespace-only and empty LINES.
Return a list of (BLANKS TO-PAD TO-STRIP).  Expect caller to know
that BLANKS includes non-empty whitespace-only lines and that no
padding or stripping has yet occurred."
  (let ((real 0) (total 0) (pad 0) (strip 0))
    (dolist (line lines)
      (if (string-match (rx bot (* (in " \t\f")) eot) line)
          (progn
            (cl-incf total)
            (if (zerop (match-end 0))
                (cl-incf strip)
              (cl-incf pad strip)
              (setq strip 0)))
        (cl-incf real)
        (unless (zerop strip)
          (cl-incf pad strip)
          (setq strip 0))))
    (when (and (zerop real) (not (zerop total)) (= total (+ pad strip)))
      (cl-incf strip (1- pad))
      (setq pad 1))
    (list total pad strip)))

(defvar erc--check-prompt-explanation nil
  "List of strings to print if no validator returns non-nil.")

(defun erc--check-prompt-input-for-multiline-blanks (_ lines)
  "Return non-nil when multiline prompt input has blank LINES.
Consider newlines to be intervening delimiters, meaning the empty
\"logical\" line between a trailing newline and `eob' constitutes
a separate message."
  (pcase-let ((`(,total ,pad ,strip)(erc--count-blank-lines lines)))
    (cond ((zerop total) nil)
          ((and erc-warn-about-blank-lines erc-send-whitespace-lines)
           (let (msg args)
             (unless (zerop strip)
               (push "stripping (%d)" msg)
               (push strip args))
             (unless (zerop pad)
               (when msg
                 (push "and" msg))
               (push "padding (%d)" msg)
               (push pad args))
             (when msg
               (push "blank" msg)
               (push (if (> (apply #'+ args) 1) "lines" "line") msg))
             (when msg
               (setf msg (nreverse msg)
                     (car msg) (capitalize (car msg))))
             (when msg
               (push (apply #'format (string-join msg " ") (nreverse args))
                     erc--check-prompt-explanation)
               nil)))
          (erc-warn-about-blank-lines
           (concat (if (= total 1)
                       (if (zerop strip) "Blank" "Trailing")
                     (if (= total strip)
                         (format "%d trailing" strip)
                       (format "%d blank" total)))
                   (and (> total 1) (/= total strip) (not (zerop strip))
                        (format " (%d trailing)" strip))
                   (if (= total 1) " line" " lines")
                   " detected (see `erc-send-whitespace-lines')"))
          (erc-send-whitespace-lines nil)
          (t 'invalid))))

(defun erc--check-prompt-input-for-point-in-bounds (_ _)
  "Return non-nil when point is before prompt."
  (when (< (point) (erc-beg-of-input-line))
    "Point is not in the input area"))

;; Originally, `erc-send-current-line' inhibited sends whenever a
;; server buffer was missing.  In 2007, this was narrowed to
;; occurrences involving process-dependent commands.  However, the
;; accompanying error message, which was identical to that emitted by
;; `erc-server-send', "ERC: No process running", was always inaccurate
;; because a server buffer can be alive and its process dead.
(defun erc--check-prompt-input-for-running-process (string _)
  "Return non-nil if STRING is a slash command missing a process.
Also do so when the server buffer has been killed."
  ;; Even if the server buffer has been killed, the user should still
  ;; be able to /reconnect and recall previous commands.
  (and (not (erc-command-no-process-p string))
       (or (and (not (erc-server-buffer-live-p)) "Server buffer missing")
           (and (not (erc-server-process-alive)) "Process not running"))))

(defun erc--check-prompt-input-for-multiline-command (line lines)
  "Return non-nil when non-blank lines follow a command line."
  (when (and (cdr lines)
             (string-match erc-command-regexp line)
             (seq-drop-while #'string-empty-p (reverse (cdr lines))))
    "Excess input after command line"))

(defvar erc--check-prompt-input-functions
  '(erc--check-prompt-input-for-point-in-bounds
    erc--check-prompt-input-for-something
    erc--check-prompt-input-for-multiline-command
    erc--check-prompt-input-for-multiline-blanks
    erc--check-prompt-input-for-running-process
    erc--check-prompt-input-for-excess-lines)
  "Validators for user input typed at prompt.
Called with two arguments: the current input submitted by the
user, as a string, along with the same input as a list of
strings.  If any member function returns non-nil, ERC abandons
processing and leaves pending input untouched in the prompt area.
When the returned value is a string, ERC passes it to
`user-error'.  Any other non-nil value tells ERC to abort
silently.  If all members return nil, and the variable
`erc--check-prompt-explanation' is a nonempty list of strings,
ERC prints them as a single message joined by newlines.")

(defun erc--run-input-validation-checks (state)
  "Run input checkers from STATE, an `erc--input-split' object."
  (let* ((erc--check-prompt-explanation nil)
         (msg (run-hook-with-args-until-success
               'erc--check-prompt-input-functions
               (erc--input-split-string state)
               (erc--input-split-lines state))))
    (cond ((stringp msg) (user-error msg))
          (msg (push msg (erc--input-split-abortp state)))
          (erc--check-prompt-explanation
           (message "%s" (string-join (nreverse erc--check-prompt-explanation)
                                      "\n"))))))

(defun erc--inhibit-slash-cmd-insertion (state)
  "Don't insert STATE object's message if it's a \"slash\" command."
  (when (erc--input-split-cmdp state)
    (setf (erc--input-split-insertp state) nil)))

(defun erc--make-input-split (string)
  (make-erc--input-split
   :string string
   :insertp erc-insert-this
   :sendp erc-send-this
   :lines (split-string string erc--input-line-delim-regexp)
   :cmdp (string-match erc-command-regexp string)))

(defun erc-send-current-line ()
  "Parse current line and send it to IRC."
  (interactive)
  (let ((now (current-time)))
    (if (or (not erc-accidental-paste-threshold-seconds)
            (time-less-p erc-accidental-paste-threshold-seconds
			 (time-subtract now erc-last-input-time)))
        (save-restriction
          ;; If there's an abbrev at the end of the line, expand it.
          (when (and abbrev-mode
                     (eolp))
            (expand-abbrev))
          (widen)
          (let* ((str (erc-user-input))
                 (state (erc--make-input-split str)))
            (run-hook-with-args 'erc--input-review-functions state)
            (when-let* (((not (erc--input-split-abortp state)))
                        (inhibit-read-only t)
                        (erc--current-line-input-split state)
                        (old-buf (current-buffer)))
              (progn ; unprogn this during next major surgery
                (erc-set-active-buffer (current-buffer))
                ;; Kill the input and the prompt
                (delete-region erc-input-marker (erc-end-of-input-line))
                (unwind-protect
                    (erc--send-input-lines (erc--run-send-hooks state))
                  ;; Fix the buffer if the command didn't kill it
                  (when (buffer-live-p old-buf)
                    (with-current-buffer old-buf
                      (setq buffer-undo-list nil)
                      ;; `set-buffer-modified-p' used to do this here.
                      (force-mode-line-update))))

                ;; Only when last hook has been run...
                (run-hook-with-args 'erc-send-completed-hook str)))
            (setq erc-last-input-time now)))
      (switch-to-buffer "*ERC Accidental Paste Overflow*")
      (lwarn 'erc :warning
             "You seem to have accidentally pasted some text!"))))

(defun erc-user-input ()
  "Return the input of the user in the current buffer."
  (buffer-substring-no-properties
   erc-input-marker
   (erc-end-of-input-line)))

(defun erc--discard-trailing-multiline-nulls (state)
  "Remove trailing empty lines from STATE, an `erc--input-split' object.
When all lines are empty, remove all but the first."
  (when (erc--input-split-lines state)
    (let ((reversed (nreverse (erc--input-split-lines state))))
      (while (and (cdr reversed) (string-empty-p (car reversed)))
        (setq reversed (cdr reversed)))
      (setf (erc--input-split-lines state) (nreverse reversed)))))

(defun erc--split-lines (state)
  "Partition non-command input into lines of protocol-compliant length."
  ;; Prior to ERC 5.6, line splitting used to be predicated on
  ;; `erc-flood-protect' being non-nil.
  (unless (or (zerop erc-split-line-length) (erc--input-split-cmdp state))
    (setf (erc--input-split-lines state)
          (mapcan #'erc--split-line (erc--input-split-lines state)))))

(defun erc--run-send-hooks (lines-obj)
  "Run send-related hooks that operate on the entire prompt input.
Sequester some of the back and forth involved in honoring old
interfaces, such as the reconstituting and re-splitting of
multiline input.  Optionally readjust lines to protocol length
limits and pad empty ones, knowing full well that additional
processing may still corrupt messages before they reach the send
queue.  Expect LINES-OBJ to be an `erc--input-split' object."
  (progn ; FIXME remove `progn' after code review.
    (with-suppressed-warnings ((lexical str) (obsolete erc-send-this))
      (defvar str) ; see note in string `erc-send-input'.
      (let* ((str (string-join (erc--input-split-lines lines-obj) "\n"))
             (erc-send-this (erc--input-split-sendp lines-obj))
             (erc-insert-this (erc--input-split-insertp lines-obj))
             (state (progn
                      ;; This may change `str' and `erc-*-this'.
                      (run-hook-with-args 'erc-send-pre-hook str)
                      (make-erc-input
                       :string str
                       :insertp erc-insert-this
                       :sendp erc-send-this
                       :substxt (erc--input-split-substxt lines-obj)
                       :refoldp (erc--input-split-refoldp lines-obj)))))
        (run-hook-with-args 'erc-pre-send-functions state)
        (setf (erc--input-split-sendp lines-obj) (erc-input-sendp state)
              (erc--input-split-insertp lines-obj) (erc-input-insertp state)
              (erc--input-split-substxt lines-obj) (erc-input-substxt state)
              (erc--input-split-refoldp lines-obj) (erc-input-refoldp state)
              ;; See note in test of same name re trailing newlines.
              (erc--input-split-lines lines-obj)
              (let ((lines (split-string (erc-input-string state)
                                         erc--input-line-delim-regexp)))
                (if erc--allow-empty-outgoing-lines-p
                    lines
                  (cl-nsubst " " "" lines :test #'equal))))
        (when (erc--input-split-refoldp lines-obj)
          (erc--split-lines lines-obj)))))
  (when (and (erc--input-split-cmdp lines-obj)
             (cdr (erc--input-split-lines lines-obj)))
    (user-error "Multiline command detected" ))
  lines-obj)

(defun erc--send-input-lines (lines-obj)
  "Send lines in `erc--input-split-lines' object LINES-OBJ."
  (when (erc--input-split-sendp lines-obj)
    (let ((insertp (erc--input-split-insertp lines-obj))
          (substxt (erc--input-split-substxt lines-obj)))
      (when (and insertp substxt)
        (setq insertp nil)
        (if (functionp substxt)
            (apply substxt (erc--input-split-lines lines-obj))
          (erc-display-msg substxt)))
      (dolist (line (erc--input-split-lines lines-obj))
        (when insertp
          (erc-display-msg line))
        (erc-process-input-line (concat line "\n")
                                (null erc-flood-protect)
                                (not (erc--input-split-cmdp lines-obj)))))))

(defun erc-send-input (input &optional skip-ws-chk)
  "Treat INPUT as typed in by the user.
It is assumed that the input and the prompt is already deleted.
Return non-nil only if we actually send anything."
  ;; Handle different kinds of inputs
  (if (and (not skip-ws-chk)
           (erc--check-prompt-input-for-multiline-blanks
            input (split-string input erc--input-line-delim-regexp)))
      (when erc-warn-about-blank-lines
        (message "Blank line - ignoring...") ; compat
        (beep))
    ;; This dynamic variable is used by `erc-send-pre-hook'.  It's
    ;; obsolete, and when it's finally removed, this binding should
    ;; also be removed.
    (with-suppressed-warnings ((lexical str))
      (defvar str))
    (let ((str input)
          (erc-insert-this t)
	  (erc-send-this t)
	  state)
      ;; The calling convention of `erc-send-pre-hook' is that it
      ;; should change the dynamic variable `str' or set
      ;; `erc-send-this' to nil.  This has now been deprecated:
      ;; Instead `erc-pre-send-functions' is used as a filter to do
      ;; allow both changing and suppressing the string.
      (run-hook-with-args 'erc-send-pre-hook input)
      (setq state (make-erc-input :string str ;May be != from `input' now!
				  :insertp erc-insert-this
				  :sendp erc-send-this))
      (run-hook-with-args 'erc-pre-send-functions state)
      (when (and (erc-input-sendp state)
                 erc-send-this)
        (if-let* ((first (split-string (erc-input-string state)
                                       erc--input-line-delim-regexp))
                  (split (mapcan #'erc--split-line first))
                  (lines (nreverse (seq-drop-while #'string-empty-p
                                                   (nreverse split))))
                  ((string-match erc-command-regexp (car lines))))
            (progn
              ;; Asking users what to do here might make more sense.
              (cl-assert (not (cdr lines)))
              ;; The `force' arg (here t) is ignored for command lines.
              (erc-process-input-line (concat (car lines) "\n") t nil))
          (progn ; temporarily preserve indentation
            (dolist (line lines)
              (progn ; temporarily preserve indentation
                (when (erc-input-insertp state)
                  (erc-display-msg line))
                (erc-process-input-line (concat line "\n")
                                        (null erc-flood-protect) t))))
          t)))))

(defun erc-display-msg (line)
  "Insert LINE into current buffer and run \"send\" hooks.
Treat LINE as input submitted interactively at the prompt, such
as outgoing chat messages and echoed slash commands."
  (when erc-insert-this
    (save-excursion
      (erc--assert-input-bounds)
      (let ((insert-position (marker-position (goto-char erc-insert-marker)))
            (erc--msg-props (or erc--msg-props
                                (let ((ovs (seq-filter
                                            #'cdr erc--msg-prop-overrides)))
                                  (map-into `((erc--msg . msg) ,@(reverse ovs))
                                            'hash-table)))))
        (insert (erc--format-speaker-input-message line) "\n")
        (save-restriction
          (narrow-to-region insert-position (point))
          (run-hooks 'erc-send-modify-hook)
          (run-hooks 'erc-send-post-hook)
          (cl-assert (> (- (point-max) (point-min)) 1))
          (add-text-properties (point-min) (1+ (point-min))
                               (erc--order-text-properties-from-hash
                                erc--msg-props)))
        (erc--refresh-prompt)))))

(defun erc-command-symbol (command)
  "Return the ERC command symbol for COMMAND if it exists and is bound."
  (let ((cmd (intern-soft (format "erc-cmd-%s" (upcase command)))))
    (when (fboundp cmd) cmd)))

(defun erc-extract-command-from-line (line)
  "Extract a \"slash command\" and its args from a prompt-input LINE.
If LINE doesn't start with a slash command, return nil.  If it
does, meaning the pattern `erc-command-regexp' matches, return a
list of the form (COMMAND ARGS), where COMMAND is either a symbol
for a known handler function or `erc-cmd-default' if unknown.
When COMMAND has the symbol property `do-not-parse-args', return
a string in place of ARGS: that is, either LINE itself, when LINE
consists of only whitespace, or LINE stripped of any trailing
whitespace, including a final newline.  When COMMAND lacks the
symbol property `do-not-parse-args', return a possibly empty list
of non-whitespace tokens.  Do not perform any shell-style parsing
of quoted or escaped substrings."
  (when (string-match erc-command-regexp line)
    (let* ((cmd (erc-command-symbol (match-string 1 line)))
           ;; note: return is nil, we apply this simply for side effects
           (_canon-defun (while (and cmd (symbolp (symbol-function cmd)))
                           (setq cmd (symbol-function cmd))))
           (cmd-fun (or cmd #'erc-cmd-default))
           (arg (if cmd
                    (if (get cmd-fun 'do-not-parse-args)
                        (format "%s" (match-string 2 line))
                      (delete "" (split-string (erc-trim-string
                                                (match-string 2 line)) " ")))
                  line)))
      (list cmd-fun arg))))

(defun erc-split-multiline-safe (string)
  "Split STRING, containing multiple lines and return them in a list.
Do it only for STRING as the complete input, do not carry unfinished
strings over to the next call."
  (let ((l ())
        (i0 0)
        (doit t))
    (while doit
      (let ((i (string-match "\r?\n" string i0))
            (s (substring string i0)))
        (cond (i (setq l (cons (substring string i0 i) l))
                 (setq i0 (match-end 0)))
              ((> (length s) 0)
               (setq l (cons s l))(setq doit nil))
              (t (setq doit nil)))))
    (nreverse l)))

;; nick handling

(defun erc-set-current-nick (nick)
  "Set the current nickname to NICK."
  (with-current-buffer (if (buffer-live-p (erc-server-buffer))
                           (erc-server-buffer)
                         (current-buffer))
    (unless (equal erc-server-current-nick nick)
      (setq erc-server-current-nick nick)
      ;; This seems sensible but may well be superfluous.  Should
      ;; really prove that it's actually needed via test scenario.
      (when erc-server-connected
        (erc-networks--id-reload erc-networks--id)))
    nick))

(defun erc-current-nick ()
  "Return the current nickname."
  (with-current-buffer (if (buffer-live-p (erc-server-buffer))
                           (erc-server-buffer)
                         (current-buffer))
    erc-server-current-nick))

(defun erc-current-nick-p (nick)
  "Return non-nil if NICK is the current nickname."
  (erc-nick-equal-p nick (erc-current-nick)))

(defun erc-nick-equal-p (nick1 nick2)
  "Return non-nil if NICK1 and NICK2 are the same.

This matches strings according to the IRC protocol's case convention.

See also `erc-downcase'."
  (string= (erc-downcase nick1)
           (erc-downcase nick2)))

;; default target handling

(defun erc--current-buffer-joined-p ()
  "Return non-nil if the current buffer is a channel and is joined."
  (and (erc--target-channel-p erc--target)
       (erc--target-channel-joined-p erc--target)
       t))

(defun erc-default-target ()
  "Return the current channel or query target, if any.
For historical reasons, return nil in channel buffers if not
currently joined."
  (let ((tgt (car erc-default-recipients)))
    (cond
     ((not tgt) nil)
     ((listp tgt) (cdr tgt))
     (t tgt))))

(defun erc-add-default-channel (channel)
  "Add CHANNEL to the default channel list."
  (declare (obsolete "use `erc-cmd-JOIN' or similar instead" "29.1"))
  (let ((chl (downcase channel)))
    (setq erc-default-recipients
          (cons chl erc-default-recipients))))

(defun erc-delete-default-channel (channel &optional buffer)
  "Delete CHANNEL from the default channel list."
  (declare (obsolete "use `erc-cmd-PART' or similar instead" "29.1"))
  (with-current-buffer (if (and buffer
                                (bufferp buffer))
                           buffer
                         (current-buffer))
    (setq erc-default-recipients (delete (downcase channel)
                                         erc-default-recipients))))

(defun erc-add-query (nickname)
  "Add QUERY'd NICKNAME to the default channel list.

The previous default target of QUERY type gets removed."
  (declare (obsolete "use `erc-cmd-QUERY' or similar instead" "29.1"))
  (let ((d1 (car erc-default-recipients))
        (d2 (cdr erc-default-recipients))
        (qt (cons 'QUERY (downcase nickname))))
    (setq erc-default-recipients (cons qt (if (and (listp d1)
                                                   (eq (car d1) 'QUERY))
                                              d2
                                            erc-default-recipients)))))

(defun erc-delete-query ()
  "Delete the topmost target if it is a QUERY."
  (declare (obsolete "use one query buffer per target instead" "29.1"))
  (let ((d1 (car erc-default-recipients))
        (d2 (cdr erc-default-recipients)))
    (if (and (listp d1)
             (eq (car d1) 'QUERY))
        (setq erc-default-recipients d2)
      (error "Current target is not a QUERY"))))

;; FIXME move all ignore-related functionality to its own module,
;; required and enabled by default (until some major version change).
(defun erc-ignored-user-p (spec)
  "Return non-nil if SPEC matches something in `erc-ignore-list'.

Takes a full SPEC of a user in the form \"nick!login@host\", and
matches against all the regexp's in `erc-ignore-list'.  If any
match, returns that regexp."
  (catch 'found
    (dolist (ignored (erc-with-server-buffer erc-ignore-list))
      (if (string-match ignored spec)
          (throw 'found ignored)))))

(defun erc-ignored-reply-p (msg tgt proc)
  ;; FIXME: this docstring needs fixing -- Lawrence 2004-01-08
  "Return non-nil if MSG matches something in `erc-ignore-reply-list'.

Takes a message MSG to a channel and returns non-nil if the addressed
user matches any regexp in `erc-ignore-reply-list'."
  (let ((target-nick (erc-message-target msg)))
    (if (not target-nick)
        nil
      (erc-with-buffer (tgt proc)
        (let ((user (erc-get-server-user target-nick)))
          (when user
            (erc-list-match erc-ignore-reply-list
                            (erc-user-spec user))))))))

(defun erc-message-target (msg)
  "Return the addressed target in MSG.

The addressed target is the string before the first colon in MSG."
  (if (string-match "^\\([^: \n]*\\):" msg)
      (match-string 1 msg)
    nil))

(defun erc-user-spec (user)
  "Create a nick!user@host spec from a user struct."
  (let ((nick (erc-server-user-nickname user))
        (host (erc-server-user-host user))
        (login (erc-server-user-login user)))
    (concat (or nick "")
            "!"
            (or login "")
            "@"
            (or host ""))))

(defun erc-list-match (lst str)
  "Return non-nil if any regexp in LST matches STR."
  (and lst (string-match (string-join lst "\\|") str)))

;; other "toggles"

(defun erc-toggle-ctcp-autoresponse (&optional arg)
  "Toggle automatic CTCP replies (like VERSION and PING).

If ARG is positive, turns CTCP replies on.

If ARG is non-nil and not positive, turns CTCP replies off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
         (setq erc-disable-ctcp-replies t))
        (arg (setq erc-disable-ctcp-replies nil))
        (t (setq erc-disable-ctcp-replies (not erc-disable-ctcp-replies))))
  (message "ERC CTCP replies are %s" (if erc-disable-ctcp-replies "OFF" "ON")))

(defun erc-toggle-flood-control (&optional arg)
  "Toggle use of flood control on sent messages.

If ARG is positive, use flood control.
If ARG is non-nil and not positive, do not use flood control.

See `erc-server-flood-margin' for an explanation of the available
flood control parameters."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
         (setq erc-flood-protect t))
        (arg (setq erc-flood-protect nil))
        (t (setq erc-flood-protect (not erc-flood-protect))))
  (message "ERC flood control is %s"
           (cond (erc-flood-protect "ON")
                 (t "OFF"))))

;; Some useful channel and nick commands for fast key bindings

(defun erc-invite-only-mode (&optional arg)
  "Turn on the invite only mode (+i) for the current channel.

If ARG is non-nil, turn this mode off (-i).

This command is sent even if excess flood is detected."
  (interactive "P")
  (erc-set-active-buffer (current-buffer))
  (let ((tgt (erc-default-target)))
    (if (or (not tgt) (not (erc-channel-p tgt)))
        (erc-display-message nil 'error (current-buffer) 'no-target)
      (erc-load-irc-script-lines
       (list (concat "/mode " tgt (if arg " -i" " +i")))
       t))))

(defun erc-get-channel-mode-from-keypress (key)
  "Read a key sequence and call the corresponding channel mode function.
After doing C-c C-o, type in a channel mode letter.

C-g means quit.
RET lets you type more than one mode at a time.
If \"l\" is pressed, `erc-set-channel-limit' gets called.
If \"k\" is pressed, `erc-set-channel-key' gets called.
Anything else will be sent to `erc-toggle-channel-mode'."
  (interactive "kChannel mode (RET to set more than one): ")
  (cond ((equal key "\C-g")
         (keyboard-quit))
        ((equal key "\C-m")
         (erc-insert-mode-command))
        ((equal key "l")
         (call-interactively 'erc-set-channel-limit))
        ((equal key "k")
         (call-interactively 'erc-set-channel-key))
        (t (erc-toggle-channel-mode key))))

(defun erc-toggle-channel-mode (mode &optional channel)
  "Toggle channel MODE.

If CHANNEL is non-nil, toggle MODE for that channel, otherwise use
`erc-default-target'."
  (interactive "P")
  (erc-set-active-buffer (current-buffer))
  (let ((tgt (or channel (erc-default-target))))
    (if (or (null tgt) (null (erc-channel-p tgt)))
        (erc-display-message nil 'error 'active 'no-target)
      (let* ((active (member mode erc-channel-modes))
             (newstate (if active "OFF" "ON")))
        (erc-log (format "%s: Toggle mode %s %s" tgt mode newstate))
        (message "Toggle channel mode %s %s" mode newstate)
        (erc-server-send (format "MODE %s %s%s"
                                 tgt (if active "-" "+") mode))))))

(defun erc-insert-mode-command ()
  "Insert the line \"/mode <current target> \" at `point'."
  (interactive)
  (let ((tgt (erc-default-target)))
    (if tgt (insert (concat "/mode " tgt " "))
      (erc-display-message nil 'error (current-buffer) 'no-target))))

(defun erc-channel-names ()
  "Run \"/names #channel\" in the current channel."
  (interactive)
  (erc-set-active-buffer (current-buffer))
  (let ((tgt (erc-default-target)))
    (if tgt (erc-load-irc-script-lines (list (concat "/names " tgt)))
      (erc-display-message nil 'error (current-buffer) 'no-target))))

(defun erc-remove-text-properties-region (start end &optional object)
  "Clears the region (START,END) in OBJECT from all colors, etc."
  (interactive "r")
  (save-excursion
    (let ((inhibit-read-only t))
      (set-text-properties start end nil object))))
(put 'erc-remove-text-properties-region 'disabled t)

;; script execution and startup

(defun erc-find-file (file &optional path)
  "Search for a FILE in the filesystem.
First the `default-directory' is searched for FILE, then any directories
specified in the list PATH.

If FILE is found, return the path to it."
  (let ((filepath file))
    (if (file-readable-p filepath) filepath
      (while (and path
                  (progn (setq filepath (expand-file-name file (car path)))
                         (not (file-readable-p filepath))))
        (setq path (cdr path)))
      (if path filepath nil))))

(defun erc-select-startup-file ()
  "Select an ERC startup file.
See also `erc-startup-file-list'."
  (catch 'found
    (dolist (f erc-startup-file-list)
      (setq f (convert-standard-filename f))
      (when (file-readable-p f)
        (throw 'found f)))))

(defun erc-find-script-file (file)
  "Search for FILE in `default-directory', and any in `erc-script-path'."
  (erc-find-file file erc-script-path))

(defun erc-load-script (file)
  "Load a script from FILE.

FILE must be the full name, it is not searched in the
`erc-script-path'.  If the filename ends with `.el', then load it
as an Emacs Lisp program.  Otherwise, treat it as a regular IRC
script."
  (erc-log (concat "erc-load-script: " file))
  (cond
   ((string-match "\\.el\\'" file)
    (load file))
   (t
    (erc-load-irc-script file))))

(defun erc-process-script-line (line &optional args)
  "Process an IRC script LINE.

Does script-specific substitutions (script arguments, current nick,
server, etc.) in LINE and returns it.

Substitutions are: %C and %c = current target (channel or nick),
%S %s = current server, %N %n = my current nick, and %x is x verbatim,
where x is any other character;
$* = the entire argument string, $1 = the first argument, $2 = the second,
and so on."
  (if (not args) (setq args ""))
  (let* ((arg-esc-regexp "\\(\\$\\(\\*\\|[1-9][0-9]*\\)\\)\\([^0-9]\\|$\\)")
         (percent-regexp "\\(%.\\)")
         (esc-regexp (concat arg-esc-regexp "\\|" percent-regexp))
         (tgt (erc-default-target))
         (server (and (boundp 'erc-session-server) erc-session-server))
         (nick (erc-current-nick))
         (res "")
         (tmp nil)
         (arg-list nil)
         (arg-num 0))
    (if (not tgt) (setq tgt ""))
    (if (not server) (setq server ""))
    (if (not nick) (setq nick ""))
    ;; First, compute the argument list
    (setq tmp args)
    (while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)" tmp)
      (setq arg-list (cons (match-string 1 tmp) arg-list))
      (setq tmp (match-string 2 tmp)))
    (setq arg-list (nreverse arg-list))
    (setq arg-num (length arg-list))
    ;; now do the substitution
    (setq tmp (string-match esc-regexp line))
    (while tmp
      ;;(message "beginning of while: tmp=%S" tmp)
      (let* ((hd (substring line 0 tmp))
             (esc "")
             (subst "")
             (tail (substring line tmp)))
        (cond ((string-match (concat "^" arg-esc-regexp) tail)
               (setq esc (match-string 1 tail))
               (setq tail (substring tail (match-end 1))))
              ((string-match (concat "^" percent-regexp) tail)
               (setq esc (match-string 1 tail))
               (setq tail (substring tail (match-end 1)))))
        ;;(message "hd=%S, esc=%S, tail=%S, arg-num=%S" hd esc tail arg-num)
        (setq res (concat res hd))
        (setq subst
              (cond ((string= esc "") "")
                    ((string-match "^\\$\\*$" esc) args)
                    ((string-match "^\\$\\([0-9]+\\)$" esc)
                     (let ((n (string-to-number (match-string 1 esc))))
                       (message "n = %S, integerp(n)=%S" n (integerp n))
                       (if (<= n arg-num) (nth (1- n) arg-list) "")))
                    ((string-match "^%[Cc]$" esc) tgt)
                    ((string-match "^%[Ss]$" esc) server)
                    ((string-match "^%[Nn]$" esc) nick)
                    ((string-match "^%\\(.\\)$" esc) (match-string 1 esc))
                    (t (erc-log (format "Bad escape sequence in %s: %S\n"
                                        'erc-process-script-line esc))
                       (message "BUG IN ERC: esc=%S" esc)
                       "")))
        (setq line tail)
        (setq tmp (string-match esc-regexp line))
        (setq res (concat res subst))
        ;;(message "end of while: line=%S, res=%S, tmp=%S" line res tmp)
        ))
    (setq res (concat res line))
    res))

(defun erc-load-irc-script (file &optional force)
  "Load an IRC script from FILE."
  (erc-log (concat "erc-load-script: " file))
  (let ((str (with-temp-buffer
               (insert-file-contents file)
               (buffer-string))))
    (erc-load-irc-script-lines (erc-split-multiline-safe str) force)))

;; authentication

(defun erc--unfun (maybe-fn)
  "Return MAYBE-FN or whatever it returns."
  (let ((s (if (functionp maybe-fn) (funcall maybe-fn) maybe-fn)))
    (when (and erc-debug-irc-protocol
               erc--debug-irc-protocol-mask-secrets
               (stringp s))
      (put-text-property 0 (length s) 'erc-secret t s))
    s))

(defun erc-login ()
  "Perform user authentication at the IRC server."
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                   (erc-current-nick)
                   (user-login-name)
                   (or erc-system-name (system-name))
                   erc-session-server
                   erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (concat "PASS :" (erc--unfun erc-session-password)))
    (message "Logging in without password"))
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
           ;; hacked - S.B.
           erc-session-username
           "0" "*"
           erc-session-user-full-name))
  (erc-update-mode-line))

;; connection properties' heuristics

(defun erc-determine-parameters (&optional server port nick name user passwd)
  "Determine the connection and authentication parameters.
Sets the buffer local variables:

- `erc-session-connector'
- `erc-session-server'
- `erc-session-port'
- `erc-session-user-full-name'
- `erc-session-username'
- `erc-session-password'
- `erc-server-current-nick'"
  (setq erc-session-connector erc-server-connect-function
        erc-session-server (erc-compute-server server)
        erc-session-port (erc-compute-port port)
        erc-session-user-full-name (erc-compute-full-name name)
        erc-session-username (erc-compute-user user)
        erc-session-password (erc--compute-server-password passwd nick))
  (erc-set-current-nick (erc-compute-nick nick)))

(defun erc-compute-server (&optional server)
  "Return an IRC server name.

This tries a progressively greater number of default methods until a
non-nil value is found.

- SERVER (the argument passed to this function)
- The `erc-server' option
- The value of the IRCSERVER environment variable
- The `erc-default-server' variable"
  (or server
      erc-server
      (getenv "IRCSERVER")
      erc-default-server))

(defun erc-compute-user (&optional user)
  "Return a suitable value for the session user name."
  (or user (if erc-anonymous-login erc-email-userid (user-login-name))))

(defun erc-compute-nick (&optional nick)
  "Return user's IRC nick.

This tries a progressively greater number of default methods until a
non-nil value is found.

- NICK (the argument passed to this function)
- The `erc-nick' option
- The value of the IRCNICK environment variable
- The result from the `user-login-name' function"
  (or nick
      (if (consp erc-nick) (car erc-nick) erc-nick)
      (getenv "IRCNICK")
      (user-login-name)))

(defun erc--compute-server-password (password nick)
  "Maybe provide a PASSWORD argument for the IRC \"PASS\" command.
When `erc-auth-source-server-function' is non-nil, call it with NICK for
the user field and use whatever it returns as the server password."
  (or password (and erc-auth-source-server-function
                    (not erc--server-reconnecting)
                    (not erc--target)
                    (funcall erc-auth-source-server-function :user nick))))

(defun erc-compute-full-name (&optional full-name)
  "Return user's full name.

This tries a progressively greater number of default methods until a
non-nil value is found.

- FULL-NAME (the argument passed to this function)
- The `erc-user-full-name' option
- The value of the IRCNAME environment variable
- The result from the `user-full-name' function"
  (or full-name
      erc-user-full-name
      (getenv "IRCNAME")
      (if erc-anonymous-login "unknown" nil)
      (user-full-name)))

(defun erc-compute-port (&optional port)
  "Return a port for an IRC server.

This tries a progressively greater number of default methods until a
non-nil value is found.

- PORT (the argument passed to this function)
- The `erc-port' option
- The `erc-default-port' variable

Note that between ERC 5.5 and 5.6.1, this function filtered its result
through `erc-normalize-port', which introduced regrettable surprises,
such as unwelcome, possibly null, type conversions."
  (or (and port (not (equal "" port)) port) erc-port erc-default-port))

;; time routines

(define-obsolete-function-alias 'erc-string-to-emacs-time #'string-to-number
  "27.1")

(defalias 'erc-emacs-time-to-erc-time #'float-time)
(defalias 'erc-current-time #'float-time)

(defun erc-time-diff (t1 t2)
  "Return the absolute value of the difference in seconds between T1 and T2."
  (abs (float-time (time-subtract t1 t2))))

(defun erc-time-gt (t1 t2)
  "Check whether T1 > T2."
  (declare (obsolete time-less-p "27.1"))
  (time-less-p t2 t1))

(defun erc-sec-to-time (ns)
  "Convert NS to a time string HH:MM.SS."
  (setq ns (truncate ns))
  (format "%02d:%02d.%02d"
          (/ ns 3600)
          (/ (% ns 3600) 60)
          (% ns 60)))

(defun erc-seconds-to-string (seconds)
  "Convert a number of SECONDS into an English phrase."
  (let (days hours minutes format-args output)
    (setq days          (/ seconds 86400)
          seconds       (% seconds 86400)
          hours         (/ seconds 3600)
          seconds       (% seconds 3600)
          minutes       (/ seconds 60)
          seconds       (% seconds 60)
          format-args   (if (> days 0)
                            `("%d days, %d hours, %d minutes, %d seconds"
                              ,days ,hours ,minutes ,seconds)
                          (if (> hours 0)
                              `("%d hours, %d minutes, %d seconds"
                                ,hours ,minutes ,seconds)
                            (if (> minutes 0)
                                `("%d minutes, %d seconds" ,minutes ,seconds)
                              `("%d seconds" ,seconds))))
          output        (apply #'format format-args))
    ;; Change all "1 units" to "1 unit".
    (while (string-match "\\([^0-9]\\|^\\)1 \\S-+\\(s\\)" output)
      (setq output (replace-match "" nil nil output 2)))
    output))


;; info

(defconst erc-clientinfo-alist
  '(("ACTION" . "is used to inform about one's current activity")
    ("CLIENTINFO" . "gives help on CTCP commands supported by client")
    ("ECHO" . "echoes its arguments back")
    ("FINGER" . "shows user's name, location, and idle time")
    ("PING" . "measures delay between peers")
    ("TIME" . "shows client-side time")
    ("USERINFO" . "shows information provided by a user")
    ("VERSION" . "shows client type and version"))
  "Alist of CTCP CLIENTINFO for ERC commands.")

(defun erc-client-info (s)
  "Return CTCP CLIENTINFO on command S.
If S is nil or an empty string then return general CLIENTINFO."
  (if (or (not s) (string= s ""))
      (concat
       (apply #'concat
              (mapcar (lambda (e)
                        (concat (car e) " "))
                      erc-clientinfo-alist))
       ": use CLIENTINFO <COMMAND> to get more specific information")
    (let ((h (assoc (upcase s) erc-clientinfo-alist)))
      (if h
          (concat s " " (cdr h))
        (concat s ": unknown command")))))

;; Hook functions

;; FIXME rename this to something like `erc-ensure-directory-writable'.
;; Functions suffixed with "-p" probably shouldn't have side effects.
(defun erc-directory-writable-p (dir)
  "Determine whether DIR is a writable directory.
If it doesn't exist, create it."
  (unless (file-attributes dir) (make-directory dir))
  (or (file-accessible-directory-p dir) (error "Cannot access %s" dir)))

;; FIXME make function obsolete or alias to something less confusing.
(defun erc-kill-query-buffers (process)
  "Kill all target buffers of PROCESS, including channel buffers.
Do nothing if PROCESS is not a process object."
  ;; here, we only want to match the channel buffers, to avoid
  ;; "selecting killed buffers" b0rkage.
  (when (processp process)
    (erc-with-all-buffers-of-server process (lambda () erc--target)
      (kill-buffer (current-buffer)))))

(defun erc-nick-at-point ()
  "Give information about the nickname at `point'.

If called interactively, give a human readable message in the
minibuffer.  If called programmatically, return the corresponding
entry of `channel-members'."
  (interactive)
  (require 'thingatpt)
  (let* ((word (word-at-point))
         (channel-data (erc-get-channel-user word))
         (cuser (cdr channel-data))
         (user (if channel-data
                   (car channel-data)
                 (erc-get-server-user word)))
         host login full-name nick voice halfop op admin owner)
    (when user
      (setq nick (erc-server-user-nickname user)
            host (erc-server-user-host user)
            login (erc-server-user-login user)
            full-name (erc-server-user-full-name user))
      (if cuser
          (setq voice (erc-channel-user-voice cuser)
                halfop (erc-channel-user-halfop cuser)
                op (erc-channel-user-op cuser)
                admin (erc-channel-user-admin cuser)
                owner (erc-channel-user-owner cuser))))
    (if (called-interactively-p 'interactive)
        (message "%s is %s@%s%s%s"
                 nick login host
                 (if full-name (format " (%s)" full-name) "")
                 (if (or voice halfop op admin owner)
                     (format " and is +%s%s%s%s%s on %s"
                             (if voice "v" "")
                             (if halfop "h" "")
                             (if op "o" "")
                             (if admin "a" "")
                             (if owner "q" "")
                             (erc-default-target))
                   ""))
      user)))

(defun erc-away-time ()
  "Return non-nil if the current ERC process is set away.

In particular, the time that we were set away is returned.
See `current-time' for details on the time format."
  (erc-with-server-buffer erc-away))

;; Mode line handling

(defcustom erc-mode-line-format "%S %a"
  "A string to be formatted and shown in the mode-line in `erc-mode'.

The string is formatted using `format-spec' and the result is set as the value
of `mode-line-buffer-identification'.

The following characters are replaced:
%a: String indicating away status or \"\" if you are not away
%l: The estimated lag time to the server
%m: The modes of the channel
%n: The current nick name
%N: The name of the network
%o: The topic of the channel
%p: The session port
%t: The name of the target (channel, nickname, or servername:port)
%s: In the server-buffer, this gets filled with the value of
    `erc-server-announced-name', in a channel, the value of
    (erc-default-target) also get concatenated.
%S: In the server-buffer, this gets filled with the value of
    `erc-network', in a channel, the value of (erc-default-target)
    also get concatenated."
  :group 'erc-mode-line-and-header
  :type 'string)

(defcustom erc-header-line-format "%n on %t (%m,%l) %o"
  "A string to be formatted and shown in the header-line in `erc-mode'.

Set this to nil if you do not want the header line to be
displayed.

See `erc-mode-line-format' for which characters are can be used."
  :group 'erc-mode-line-and-header
  :set (lambda (sym val)
         (set sym val)
         (when (fboundp 'erc-update-mode-line)
           (erc-update-mode-line nil)))
  :type '(choice (const :tag "Disabled" nil)
                 string))

;; This should optionally support the built-in `tab-bar'.
(defcustom erc-header-line-uses-tabbar-p nil
  "Use `tabbar-mode' integration instead of the header line.
This concerns a historical integration with the external library
`tabbar' <https://www.emacswiki.org/emacs/tabbar.el>, which
shouldn't be confused with the built-in `tab-bar' described in
Info node `(emacs) Tab Bars'."
  :group 'erc-mode-line-and-header
  :type 'boolean)

(defcustom erc-header-line-uses-help-echo-p t
  "Show header line in echo area or as a tooltip
when point moves to the header line."
  :group 'erc-mode-line-and-header
  :type 'boolean)

(defcustom erc-header-line-face-method nil
  "Determine what method to use when colorizing the header line text.

If nil, don't colorize the header text.
If given a function, call it and use the resulting face name.
Otherwise, use the `erc-header-line' face."
  :group 'erc-mode-line-and-header
  :type '(choice (const :tag "Don't colorize" nil)
                 (const :tag "Use the erc-header-line face" t)
                 (function :tag "Call a function")))

(defcustom erc-show-channel-key-p t
  "Show the channel key in the header line."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-mode-line-away-status-format
  "(AWAY since %a %b %d %H:%M) "
  "When you're away on a server, this is shown in the mode line.
This should be a string with substitution variables recognized by
`format-time-string'."
  :group 'erc-mode-line-and-header
  :type 'string)

(defun erc-shorten-server-name (server)
  "Shorten SERVER name according to `erc-common-server-suffixes'."
  (if (stringp server)
      (with-temp-buffer
        (insert server)
        (let ((alist erc-common-server-suffixes))
          (while alist
            (goto-char (point-min))
            (if (re-search-forward (caar alist) nil t)
                (replace-match (cdar alist)))
            (setq alist (cdr alist))))
        (buffer-string))))

(defun erc-format-target ()
  "Return the name of the target (channel or nickname or servername:port)."
  (let ((target (erc-default-target)))
    (or target
        (concat (erc-shorten-server-name
                 (or erc-server-announced-name
                     erc-session-server))
                ":" (erc-port-to-string erc-session-port)))))

(defun erc-format-target-and/or-server ()
  "Return the server name or the current target and server name combined."
  (let ((server-name (erc-shorten-server-name
                      (or erc-server-announced-name
                          erc-session-server))))
    (cond ((erc-default-target)
           (concat (erc-string-no-properties (erc-default-target))
                   "@" server-name))
          (server-name server-name)
          (t (buffer-name (current-buffer))))))

(defun erc-format-network ()
  "Return the name of the network we are currently on."
  (erc-network-name))

(defun erc-format-target-and/or-network ()
  "Return the network or the current target and network combined.
If the name of the network is not available, then use the
shortened server name instead."
  (if-let* ((erc--target)
            (name (if erc-networks--id
                      (erc-networks--id-string erc-networks--id)
                    (erc-shorten-server-name (or erc-server-announced-name
                                                 erc-session-server)))))
      (concat (erc--target-string erc--target) "@" name)
    (buffer-name)))

(defun erc-format-away-status ()
  "Return a formatted `erc-mode-line-away-status-format' if `erc-away' is non-nil."
  (let ((a (erc-away-time)))
    (if a
        (format-time-string erc-mode-line-away-status-format a)
      "")))

(defvar-local erc--away-indicator nil
  "Cons containing an away indicator for the connection.")

(defvar erc-away-status-indicator "A"
  "String shown by various formatting facilities to indicate away status.
Currently only used by the option `erc-prompt-format'.")

(defun erc--format-away-indicator ()
  "Return char with `display' property of `erc--away-indicator'."
  (and-let* ((indicator (erc-with-server-buffer
                          (or erc--away-indicator
                              (setq erc--away-indicator (list "")))))
             (newcar (if (erc-away-time) erc-away-status-indicator "")))
    ;; Inform other buffers of the change when necessary.
    (let ((dispp (not erc--inhibit-prompt-display-property-p)))
      (unless (eq newcar (car indicator))
        (erc--refresh-prompt-continue (and dispp 'hooks-only-p))
        (setcar indicator newcar))
      (if dispp
          (propertize "(away?)" 'display indicator)
        newcar))))

(defvar-local erc--user-modes-indicator nil
  "Cons containing connection-wide indicator for user modes.")

;; If adding more of these functions, should factor out commonalities.
;; As of ERC 5.6, this is identical to the away variant aside from
;; the var names and `eq', which isn't important.
(defun erc--format-user-modes ()
  "Return server's user modes as a string."
  (and-let* ((indicator (erc-with-server-buffer
                          (or erc--user-modes-indicator
                              (setq erc--user-modes-indicator (list "")))))
             (newcar (erc--user-modes 'string)))
    (let ((dispp (not erc--inhibit-prompt-display-property-p)))
      (unless (string= newcar (car indicator))
        (erc--refresh-prompt-continue (and dispp 'hooks-only-p))
        (setcar indicator newcar))
      (if dispp
          (propertize "(user-modes?)" 'display indicator)
        newcar))))

(defun erc--format-channel-status-prefix ()
  "Return the current channel membership prefix."
  (and (erc--target-channel-p erc--target)
       (erc-get-channel-membership-prefix (erc-current-nick))))

(defun erc--format-modes (&optional no-query-p)
  "Return a string of channel modes in channels and user modes elsewhere.
With NO-QUERY-P, return nil instead of user modes in query
buffers.  Also return nil when mode information is unavailable."
  (cond ((erc--target-channel-p erc--target)
         (erc--channel-modes 'string))
        ((not (and erc--target no-query-p))
         (erc--format-user-modes))))

(defun erc-format-channel-modes ()
  "Return the current channel's modes."
  (concat (apply #'concat
                 "+" erc-channel-modes)
          (cond ((and erc-channel-user-limit erc-channel-key)
                 (if erc-show-channel-key-p
                     (format "lk %.0f %s" erc-channel-user-limit
                             erc-channel-key)
                   (format "kl %.0f" erc-channel-user-limit)))
                (erc-channel-user-limit
                 ;; Emacs has no bignums
                 (format "l %.0f" erc-channel-user-limit))
                (erc-channel-key
                 (if erc-show-channel-key-p
                     (format "k %s" erc-channel-key)
                   "k"))
                (t nil))))

(defun erc-format-lag-time ()
  "Return the estimated lag time to server, `erc-server-lag'."
  (let ((lag (erc-with-server-buffer erc-server-lag)))
    (cond (lag (format "lag:%.0f" lag))
          (t ""))))

;; TODO when ERC drops Emacs 28, replace the expressions in the format
;; spec below with functions.
(defun erc-update-mode-line-buffer (buffer)
  "Update the mode line in a single ERC buffer BUFFER."
  (with-current-buffer buffer
    (let ((spec `((?a . ,(erc-format-away-status))
                  (?l . ,(erc-format-lag-time))
                  (?m . ,(or erc--mode-line-mode-string ""))
                  (?n . ,(or (erc-current-nick) ""))
                  (?N . ,(erc-format-network))
                  (?o . ,(or (erc-controls-strip erc-channel-topic) ""))
                  (?p . ,(erc-port-to-string erc-session-port))
                  (?s . ,(erc-format-target-and/or-server))
                  (?S . ,(erc-format-target-and/or-network))
                  (?t . ,(erc-format-target))))
          (process-status (cond ((erc-server-process-alive buffer)
                                 (unless erc-server-connected
                                   ": connecting"))
                                ((erc-with-server-buffer
                                   erc--server-reconnect-timer)
                                 erc--mode-line-process-reconnecting)
                                (t
                                 ": CLOSED")))
          (face (cond ((eq erc-header-line-face-method nil)
                       nil)
                      ((functionp erc-header-line-face-method)
                       (funcall erc-header-line-face-method))
                      (t
                       'erc-header-line))))
      (setq mode-line-buffer-identification
            (list (format-spec erc-mode-line-format spec)))
      (setq mode-line-process process-status)
      (let ((header (if erc-header-line-format
                        (format-spec erc-header-line-format spec)
                      nil)))
        (cond (erc-header-line-uses-tabbar-p
               (when (boundp 'tabbar--local-hlf)
                 (setq-local tabbar--local-hlf header-line-format))
               (kill-local-variable 'header-line-format))
              ((null header)
               (setq header-line-format nil))
              (erc-header-line-uses-help-echo-p
               (let ((help-echo (with-temp-buffer
                                  (insert header)
                                  (fill-region (point-min) (point-max))
                                  (buffer-string))))
                 (setq header-line-format
                       (string-replace
                        "%"
                        "%%"
                        (if face
                            (propertize header 'help-echo help-echo 'face face)
                          (propertize header 'help-echo help-echo))))))
              (t (setq header-line-format
                       (if face
                           (propertize header 'face face)
                         header))))))
    (force-mode-line-update)))

(defun erc-update-mode-line (&optional buffer)
  "Update the mode line in BUFFER.

If BUFFER is nil, update the mode line in all ERC buffers."
  (if (and buffer (bufferp buffer))
      (erc-update-mode-line-buffer buffer)
    (dolist (buf (erc-buffer-list))
      (when (buffer-live-p buf)
        (erc-update-mode-line-buffer buf)))))

;; Miscellaneous

(defun erc-bug (subject)
  "Send a bug report to the Emacs bug tracker and ERC mailing list."
  (interactive "sBug Subject: ")
  (report-emacs-bug
   (format "ERC %s: %s" erc-version subject))
  (save-excursion
    (if (and (>= emacs-major-version 30)
             (search-backward "X-Debbugs-CC: " nil t)
             (goto-char (pos-eol))
             (eq (char-before) ?\s))
        (insert "emacs-erc@gnu.org")
      (goto-char (point-min))
      (insert "X-Debbugs-CC: emacs-erc@gnu.org\n"))))

(defconst erc--news-url
  "https://git.savannah.gnu.org/cgit/emacs.git/plain/etc/ERC-NEWS")

(defvar erc--news-temp-file nil)

(defun erc-news (arg)
  "Show ERC news in a manner similar to `view-emacs-news'.
With ARG, download and display the latest revision, which may
contain more up-to-date information, even for older versions."
  (interactive "P")
  (find-file
   (or (and erc--news-temp-file
            (time-less-p (current-time) (car erc--news-temp-file))
            (not (and arg (y-or-n-p (format "Re-fetch? "))))
            (cdr erc--news-temp-file))
       (and arg
            (with-current-buffer (url-retrieve-synchronously erc--news-url)
              (goto-char (point-min))
              (search-forward "200 OK" (pos-eol))
              (search-forward "\n\n")
              (delete-region (point-min) (point))
              ;; May warn about file having changed on disk (unless
              ;; `query-about-changed-file' is nil on 28+).
              (let ((tempfile (or (cdr erc--news-temp-file)
                                  (make-temp-file "erc-news."))))
                (write-region (point-min) (point-max) tempfile)
                (kill-buffer)
                (cdr (setq erc--news-temp-file
                           (cons (time-add (current-time) (* 60 60 12))
                                 tempfile))))))
       (and-let* ((file (or (eval-when-compile (macroexp-file-name))
                            (locate-library "erc")))
                  (dir (file-name-directory file))
                  (adjacent (expand-file-name "ERC-NEWS" dir))
                  ((file-exists-p adjacent)))
         adjacent)
       (expand-file-name "ERC-NEWS" data-directory)))
  (when (fboundp 'emacs-news-view-mode)
    (emacs-news-view-mode))
  (goto-char (point-min))
  (let ((v (mapcar #'number-to-string
                   (seq-take-while #'natnump (version-to-list erc-version)))))
    (while (and v (not (search-forward (concat "\014\n* Changes in ERC "
                                               (string-join v "."))
                                       nil t)))
      (setq v (butlast v))))
  (beginning-of-line))

(defun erc-port-to-string (p)
  "Convert port P to a string.
P may be an integer or a service name."
  (if (integerp p)
      (int-to-string p)
    p))

(defun erc-string-to-port (s)
  "Convert string S to either an integer port number or a service name."
  (if (numberp s)
      s
    (let ((n (string-to-number s)))
      (if (= n 0)
          s
        n))))

(defun erc-version (&optional here bold-erc)
  "Show the version number of ERC in the minibuffer.
If optional argument HERE is non-nil, insert version number at point.
If optional argument BOLD-ERC is non-nil, display \"ERC\" as bold."
  (interactive "P")
  (let ((version-string
         (format "%s %s (IRC client for GNU Emacs %s)"
                 (if bold-erc
                     "\C-bERC\C-b"
                   "ERC")
                 erc-version
                 emacs-version)))
    (if here
        (insert version-string)
      (if (called-interactively-p 'interactive)
          (message "%s" version-string)
        version-string))))

(defun erc-modes (&optional here)
  "Show the active ERC modes in the minibuffer.
If optional argument HERE is non-nil, insert version number at point."
  (interactive "P")
  (let ((string
         (mapconcat #'identity
                    (let (modes (case-fold-search nil))
                      (dolist (var (apropos-internal "^erc-.*mode$"))
                        (when (and (boundp var)
                                   (get var 'erc-module)
                                   (symbol-value var))
                          (setq modes (cons (concat "`" (symbol-name var) "'")
                                            modes))))
                      (sort modes #'string<))
                    ", ")))
    (if here
        (insert string)
      (if (called-interactively-p 'interactive)
          (message "%s" string)
        string))))

(defun erc-trim-string (s)
  "Trim leading and trailing spaces off S."
  (cond
   ((not (stringp s)) nil)
   ((string-match "^\\s-*$" s)
    "")
   ((string-match "^\\s-*\\(.*\\S-\\)\\s-*$" s)
    (match-string 1 s))
   (t
    s)))

(defun erc-arrange-session-in-multiple-windows ()
  "Open a window for every non-server buffer related to `erc-session-server'.

All windows are opened in the current frame."
  (interactive)
  (unless erc-server-process
    (error "No erc-server-process found in current buffer"))
  (let ((bufs (erc-buffer-list nil erc-server-process)))
    (when bufs
      (delete-other-windows)
      (switch-to-buffer (car bufs))
      (setq bufs (cdr bufs))
      (while bufs
        (split-window)
        (other-window 1)
        (switch-to-buffer (car bufs))
        (setq bufs (cdr bufs))
        (balance-windows)))))

(defun erc-popup-input-buffer ()
  "Provide an input buffer."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name "*ERC input*"))
        (mode (intern
               (completing-read
                "Mode: "
                (mapcar (lambda (e)
                          (list (symbol-name e)))
                        (apropos-internal "-mode\\'" 'commandp))
                nil t))))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region (point) (point))
    (shrink-window-if-larger-than-buffer)))

;;; Message catalog

(define-inline erc--make-message-variable-name (catalog key softp)
  "Return variable name conforming to ERC's message-catalog interface.
Given a CATALOG symbol `mycat' and format-string KEY `mykey',
also a symbol, return the symbol `erc-message-mycat-mykey'.  With
SOFTP, only do so when defined as a variable."
  (inline-quote
   (let* ((catname (symbol-name ,catalog))
          (prefix (if (eq ?- (aref catname 0)) "erc--message" "erc-message-"))
          (name (concat prefix catname "-" (symbol-name ,key))))
     (if ,softp
         (and-let* ((s (intern-soft name)) ((boundp s))) s)
       (intern name)))))

(defun erc-make-message-variable-name (catalog entry)
  "Create a variable name corresponding to CATALOG's ENTRY."
  (erc--make-message-variable-name catalog entry nil))

(defun erc-define-catalog-entry (catalog entry format-spec)
  "Set CATALOG's ENTRY to FORMAT-SPEC."
  (declare (obsolete "define manually using `defvar' instead" "30.1"))
  (set (erc-make-message-variable-name catalog entry)
       format-spec))

(defun erc-define-catalog (catalog entries)
  "Define a CATALOG according to ENTRIES."
  (declare (obsolete erc-define-message-format-catalog "30.1"))
  (with-suppressed-warnings ((obsolete erc-define-catalog-entry))
    (dolist (entry entries)
      (erc-define-catalog-entry catalog (car entry) (cdr entry)))))

(erc--define-catalog english
  ((bad-ping-response . "Unexpected PING response from %n (time %t)")
   (bad-syntax . "Error occurred - incorrect usage?\n%c %u\n%d")
   (incorrect-args . "Incorrect arguments. Usage:\n%c %u\n%d")
   (cannot-find-file . "Cannot find file %f")
   (cannot-read-file . "Cannot read file %f")
   (channel-mode-unknown . "Unknown channel mode: %c")
   (connect . "Connecting to %S:%p... ")
   (country . "%c")
   (country-unknown . "%d: No such domain")
   (ctcp-empty . "Illegal empty CTCP query received from %n. Ignoring.")
   (ctcp-request . "==> CTCP request from %n (%u@%h): %r")
   (ctcp-request-to . "==> CTCP request from %n (%u@%h) to %t: %r")
   (ctcp-too-many . "Too many CTCP queries in single message. Ignoring")
   (flood-ctcp-off . "FLOOD PROTECTION: Automatic CTCP responses turned off.")
   (flood-strict-mode
    . "FLOOD PROTECTION: Switched to Strict Flood Control mode.")
   (disconnected
    . "\n\n*** Connection failed!  Re-establishing connection...\n")
   (disconnected-noreconnect
    . "\n\n*** Connection failed!  Not re-establishing connection.\n")
   (ignore-list . "%-8p %s")
   (reconnecting . "Reconnecting in %ms: attempt %i/%n ...")
   (reconnect-canceled . "Canceled %u reconnect timer with %cs to go...")
   (recon-probe-hung-up . "Server answered but hung up. Delaying by %ts...")
   (recon-probe-nobody-home . "Nobody home...")
   (finished . "\n\n*** ERC finished ***\n")
   (terminated . "\n\n*** ERC terminated: %e\n")
   (login . "Logging in as `%n'...")
   (graft . "Grafting buffer `%n' onto `%o'...") ; {new} onto {old}
   (nick-in-use . "%n is in use. Choose new nickname: ")
   (nick-too-long
    . "WARNING: Nick length (%i) exceeds max NICKLEN(%l) defined by server")
   (no-default-channel . "No default channel")
   (no-invitation . "You've got no invitation")
   (no-target . "No target")
   (ops . "%i operator%s: %o")
   (ops-none . "No operators in this channel.")
   (undefined-ctcp . "Undefined CTCP query received. Silently ignored")
   (user-mode-redundant-add
    . "Already have user mode(s): %m. Requesting again anyway.")
   (user-mode-redundant-drop
    . "Already without user mode(s): %m. Requesting removal anyway.")
   (variable-not-bound . "Variable not bound!")
   (ACTION . "* %n %a")
   (CTCP-CLIENTINFO . "Client info for %n: %m")
   (CTCP-ECHO . "Echo %n: %m")
   (CTCP-FINGER . "Finger info for %n: %m")
   (CTCP-PING . "Ping time to %n is %t")
   (CTCP-TIME . "Time by %n is %m")
   (CTCP-UNKNOWN . "Unknown CTCP message from %n (%u@%h): %m")
   (CTCP-VERSION . "Version for %n is %m")
   (ERROR  . "==> ERROR from %s: %c\n")
   (INVITE . "%n (%u@%h) invites you to channel %c")
   (JOIN   . "%n (%u@%h) has joined channel %c")
   (JOIN-you . "You have joined channel %c")
   (KICK . "%n (%u@%h) has kicked %k off channel %c: %r")
   (KICK-you . "You have been kicked off channel %c by %n (%u@%h): %r")
   (KICK-by-you . "You have kicked %k off channel %c: %r")
   (MODE   . "%n (%u@%h) has changed mode for %t to %m")
   (MODE-nick . "%n has changed mode for %t to %m")
   (NICK   . "%n (%u@%h) is now known as %N")
   (NICK-you . "Your new nickname is %N")
   (PART   . #'erc-message-english-PART)
   (PING   . "PING from server (last: %s sec. ago)")
   (PONG   . "PONG from %h (%i second%s)")
   (QUIT   . "%n (%u@%h) has quit: %r")
   (TOPIC  . "%n (%u@%h) has set the topic for %c: \"%T\"")
   (WALLOPS . "Wallops from %n: %m")
   (s004   . "%s %v %U %C")
   (s221   . "User modes for %n: %m")
   (s252   . "%i operator(s) online")
   (s253   . "%i unknown connection(s)")
   (s254   . "%i channels formed")
   (s275   . "%n %m")
   (s301   . "%n is AWAY: %r")
   (s303   . "Is online: %n")
   (s305   . "%m")
   (s306   . "%m")
   (s307   . "%n %m")
   (s311   . "%n is %f (%u@%h)")
   (s312   . "%n is/was on server %s (%c)")
   (s313   . "%n is an IRC operator")
   (s314   . "%n was %f (%u@%h)")
   (s317   . "%n has been idle for %i")
   (s317-on-since . "%n has been idle for %i, on since %t")
   (s319   . "%n is on channel(s): %c")
   (s320   . "%n is an identified user")
   (s321   . "Channel  Users  Topic")
   (s322   . "%c [%u] %t")
   (s324   . "%c modes: %m")
   (s328   . "%c URL: %u")
   (s329   . "%c was created on %t")
   (s330   . "%n %a %i")
   (s331   . "No topic is set for %c")
   (s332   . "Topic for %c: %T")
   (s333   . "%c: topic set by %n, %t")
   (s341   . "Inviting %n to channel %c")
   (s352   . "%-11c %-10n %-4a %u@%h (%f)")
   (s352-you . "%n %a %u@%h (%f)")
   (s353   . "Users on %c: %u")
   (s367   . "Ban for %b on %c")
   (s367-set-by . "Ban for %b on %c set by %s on %t")
   (s368   . "Banlist of %c ends.")
   (s379   . "%c: Forwarded to %f")
   (s391   . "The time at %s is %t")
   (s396   . "Your visible host has changed to %s")
   (s401   . "%n: No such nick/channel")
   (s402   . "%c: No such server")
   (s403   . "%c: No such channel")
   (s404   . "%c: Cannot send to channel")
   (s405   . "%c: You have joined too many channels")
   (s406   . "%n: There was no such nickname")
   (s412   . "No text to send")
   (s421   . "%c: Unknown command")
   (s431   . "No nickname given")
   (s432   . "%n is an erroneous nickname")
   (s442   . "%c: You're not on that channel")
   (s445   . "SUMMON has been disabled")
   (s446   . "USERS has been disabled")
   (s451   . "You have not registered")
   (s461   . "%c: not enough parameters")
   (s462   . "Unauthorized command (already registered)")
   (s463   . "Your host isn't among the privileged")
   (s464   . "Password incorrect")
   (s465   . "You are banned from this server")
   (s471   . "Max occupancy for channel %c exceeded: %s")
   (s473   . "Channel %c is invitation only")
   (s474   . "You can't join %c because you're banned (+b)")
   (s475   . "You must specify the correct channel key (+k) to join %c")
   (s481   . "Permission Denied - You're not an IRC operator")
   (s482   . "You need to be a channel operator of %c to do that")
   (s483   . "You can't kill a server!")
   (s484   . "Your connection is restricted!")
   (s485   . "You're not the original channel operator")
   (s491   . "No O-lines for your host")
   (s501   . "Unknown MODE flag")
   (s502   . "You can't change modes for other users")
   (s671   . "%n %a")))

(defun erc-message-english-PART (&rest args)
  "Format a proper PART message.

This function is an example on what could be done with formatting
functions."
  (let ((nick (cadr (memq ?n args)))
        (user (cadr (memq ?u args)))
        (host (cadr (memq ?h args)))
        (channel (cadr (memq ?c args)))
        (reason (cadr (memq ?r args))))
    (if (string= nick (erc-current-nick))
        (format "You have left channel %s" channel)
      (format "%s (%s@%s) has left channel %s%s"
              nick user host channel
              (if (not (string= reason ""))
                  (format ": %s"
                          (string-replace "%" "%%" reason))
                "")))))

(defun erc-retrieve-catalog-entry (key &optional catalog)
  "Retrieve `format-spec' entry for symbol KEY in CATALOG.
Without symbol CATALOG, use `erc-current-message-catalog'.  If
lookup fails, try the latter's `default-toplevel-value' if it's
not the same as CATALOG.  Failing that, try the `english' catalog
if yet untried."
  (unless catalog (setq catalog erc-current-message-catalog))
  (symbol-value
   (or (erc--make-message-variable-name catalog key 'softp)
       (let ((parent catalog)
             last)
         (while (and (setq parent (get parent 'erc--base-format-catalog))
                     (not (setq last (erc--make-message-variable-name
                                      parent key 'softp)))))
         last)
       (let ((default (default-toplevel-value 'erc-current-message-catalog)))
         (or (and (not (eq default catalog))
                  (erc--make-message-variable-name default key 'softp))
             (and (not (memq 'english (list default catalog)))
                  (erc--make-message-variable-name 'english key 'softp)))))))

(defun erc-format-message (msg &rest args)
  "Format MSG according to ARGS.

See also `format-spec'."
  (unless (cl-evenp (length args))
    (error "Obscure usage of this function appeared"))
  (let ((entry (erc-retrieve-catalog-entry msg)))
    (when (not entry)
      (error "No format spec for message %s" msg))
    (when (functionp entry)
      (setq entry (apply entry args)))
    (format-spec entry (apply #'format-spec-make args) 'ignore)))

;;; Various hook functions

(defcustom erc-kill-server-hook '(erc-kill-server
                                  erc-networks-shrink-ids-and-buffer-names)
  "Invoked whenever a live server buffer is killed via `kill-buffer'."
  :package-version '(ERC . "5.5")
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-kill-channel-hook
  '(erc-part-channel-on-kill
    erc-networks-shrink-ids-and-buffer-names
    erc-networks-rename-surviving-target-buffer)
  "Invoked whenever a channel-buffer is killed via `kill-buffer'."
  :package-version '(ERC . "5.6")
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-kill-buffer-hook
  '(erc-networks-shrink-ids-and-buffer-names
    erc-networks-rename-surviving-target-buffer)
  "Hook run whenever a query buffer is killed.

See also `kill-buffer'."
  :package-version '(ERC . "5.5")
  :group 'erc-hooks
  :type 'hook)

;; FIXME alias and deprecate current *-function suffixed name.
(defun erc-kill-buffer-function ()
  "Function to call when an ERC buffer is killed.
This function should be on `kill-buffer-hook'.
When the current buffer is in `erc-mode', this function will run
one of the following hooks:
`erc-kill-server-hook' if the server buffer was killed,
`erc-kill-channel-hook' if a channel buffer was killed,
or `erc-kill-buffer-hook' if any other buffer."
  (when (eq major-mode 'erc-mode)
    (when-let* ((erc--target)
                (nick (erc-current-nick)))
      (erc--remove-channel-users-but nick))
    (cond
     ((eq (erc-server-buffer) (current-buffer))
      (run-hooks 'erc-kill-server-hook))
     ((erc--target-channel-p erc--target)
      (run-hooks 'erc-kill-channel-hook))
     (t
      (run-hooks 'erc-kill-buffer-hook)))))

(declare-function set-text-conversion-style "textconv.c")

(defun erc-check-text-conversion ()
  "Check if point is within the ERC prompt and toggle text conversion.
If `text-conversion-style' is not `action' if point is within the
prompt or nil otherwise, set it to such a value, so as to
guarantee that the input method functions properly for the
purpose of typing within the ERC prompt."
  (when (and (eq major-mode 'erc-mode)
             (fboundp 'set-text-conversion-style))
    (defvar text-conversion-style) ; avoid free variable warning on <=29
    (if (>= (point) (erc-beg-of-input-line))
        (unless (eq text-conversion-style 'action)
          (set-text-conversion-style 'action))
      (unless (not text-conversion-style)
        (set-text-conversion-style nil)))))

(defun erc-kill-server ()
  "Sends a QUIT command to the server when the server buffer is killed.
This function should be on `erc-kill-server-hook'."
  (when (erc-server-process-alive)
    (setq erc-server-quitting t)
    (erc-server-send (format "QUIT :%s" (funcall erc-quit-reason nil)))))

(define-obsolete-function-alias 'erc-kill-channel #'erc-part-channel-on-kill
  "30.1")
(defun erc-part-channel-on-kill ()
  "Send a \"PART\" when killing a channel buffer."
  (when (and (not erc-killing-buffer-on-part-p)
             (not erc-networks--target-transplant-in-progress-p)
             (erc-server-process-alive))
    (let ((tgt (erc-default-target)))
      (if tgt
         (erc-server-send (format "PART %s :%s" tgt
                                  (funcall erc-part-reason nil))
                          nil tgt)))))

;;; Dealing with `erc-parsed'

(defun erc-find-parsed-property ()
  "Find the next occurrence of the `erc-parsed' text property."
  (text-property-not-all (point-min) (point-max) 'erc-parsed nil))

(defun erc-restore-text-properties ()
  "Ensure the `erc-parsed' and `tags' props cover the entire message."
  (when-let* ((parsed-posn (erc-find-parsed-property))
              (found (erc-get-parsed-vector parsed-posn)))
    (put-text-property (point-min) (point-max) 'erc-parsed found)
    (when-let* ((tags (get-text-property parsed-posn 'tags)))
      (put-text-property (point-min) (point-max) 'tags tags))))

(defun erc-get-parsed-vector (point)
  "Return the whole parsed vector on POINT."
  (get-text-property point 'erc-parsed))

(defun erc-get-parsed-vector-nick (vect)
  "Return nickname in the parsed vector VECT."
  (let* ((untreated-nick (and vect (erc-response.sender vect)))
         (maybe-nick (when untreated-nick
                       (car (split-string untreated-nick "!")))))
    (when (and (not (null maybe-nick))
               (erc-is-valid-nick-p maybe-nick))
      untreated-nick)))

(defun erc-get-parsed-vector-type (vect)
  "Return message type in the parsed vector VECT."
  (and vect
       (erc-response.command vect)))

(defun erc--get-eq-comparable-cmd (command)
  "Return a symbol or a fixnum representing a message's COMMAND.
See also `erc-message-type'."
  ;; IRC numerics are three-digit numbers, possibly with leading 0s.
  ;; To invert: (if (numberp o) (format "%03d" o) (symbol-name o))
  (if-let* ((n (string-to-number command)) ((zerop n))) (intern command) n))

;; Teach url.el how to open irc:// URLs with ERC.
;; To activate, customize `url-irc-function' to `url-irc-erc'.

(defcustom erc-url-connect-function nil
  "When non-nil, a function used to connect to an IRC URL.
Called with a string meant to represent a URL scheme, like
\"ircs\", followed by any number of keyword arguments recognized
by `erc' and `erc-tls'."
  :group 'erc
  :package-version '(ERC . "5.5")
  :type '(choice (const nil) function))

(defun erc--url-default-connect-function (scheme &rest plist)
  (let* ((ircsp (if scheme
                    (string-suffix-p "s" scheme)
                  (or (eql 6697 (plist-get plist :port))
                      (yes-or-no-p "Connect using TLS? "))))
         (erc-server (plist-get plist :server))
         (erc-default-port (if ircsp erc-default-port-tls erc-default-port))
         (erc-port (erc-compute-port (plist-get plist :port)))
         (erc-nick (or (plist-get plist :nick) erc-nick))
         (erc-password (plist-get plist :password))
         (args (erc-select-read-args)))
    (unless ircsp
      (setq ircsp (eql 6697 erc-port)))
    (apply (if ircsp #'erc-tls #'erc) args)))

;;;###autoload
(defun erc-handle-irc-url (host port channel nick password &optional scheme)
  "Use ERC to IRC on HOST:PORT in CHANNEL.
If ERC is already connected to HOST:PORT, simply /join CHANNEL.
Otherwise, connect to HOST:PORT as NICK and /join CHANNEL.

Beginning with ERC 5.5, new connections require human intervention.
Customize `erc-url-connect-function' to override this."
  (when (eql port 0) (setq port nil))
  (let* ((net (erc-networks--determine host))
         (erc--display-context `((erc-interactive-display . url)
                                 ,@erc--display-context))
         (server-buffer
          ;; Viable matches may slip through the cracks for unknown
          ;; networks.  Additional passes could likely improve things.
          (car (erc-buffer-filter
                (lambda ()
                  (and (not erc--target)
                       (erc-server-process-alive)
                       ;; Always trust a matched network.
                       (or (and net (eq net (erc-network)))
                           (and (string-equal erc-session-server host)
                                ;; Ports only matter when dialed hosts
                                ;; match and we have sufficient info.
                                (or (null port)
                                    (erc-port-equal erc-session-port
                                                    port)))))))))
         key deferred)
    (unless server-buffer
      (setq deferred t
            server-buffer (apply (or erc-url-connect-function
                                     #'erc--url-default-connect-function)
                                 scheme
                                 :server host
                                 `(,@(and port (list :port port))
                                   ,@(and nick (list :nick nick))
                                   ,@(and password `(:password ,password))))))
    (when channel
      ;; These aren't percent-decoded by default
      (when (string-prefix-p "%" channel)
        (setq channel (url-unhex-string channel)))
      (cl-multiple-value-setq (channel key) (split-string channel "[?]"))
      (if deferred
          ;; Alternatively, we could make this a defmethod, so when
          ;; autojoin is loaded, it can do its own thing.  Also, as
          ;; with `erc-once-with-server-event', it's fine to set local
          ;; hooks here because they're killed when reconnecting.
          (with-current-buffer server-buffer
            (letrec ((f (lambda (&rest _)
                          (remove-hook 'erc-after-connect f t)
                          (erc-cmd-JOIN channel key))))
              (add-hook 'erc-after-connect f nil t)))
        (with-current-buffer server-buffer
          (erc-cmd-JOIN channel key))))))

(provide 'erc)

;;; erc.el ends here
