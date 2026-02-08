;;; erc-backend.el --- Backend network communication for ERC  -*- lexical-binding:t -*-

;; Copyright (C) 2004-2026 Free Software Foundation, Inc.

;; Author: Lawrence Mitchell <wence@gmx.li>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Created: 2004-05-7
;; Keywords: comm, IRC, chat, client, internet

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

;; This file defines backend network communication handlers for ERC.
;;
;; How things work:
;;
;; You define a new handler with `define-erc-response-handler'.  This
;; defines a function, a corresponding hook variable, and populates a
;; global hash table `erc-server-responses' with a map from response
;; to hook variable.  See the function documentation for more
;; information.
;;
;; Upon receiving a line from the server, `erc-parse-server-response'
;; is called on it.
;;
;; A line generally looks like:
;;
;; LINE := ':' SENDER ' ' COMMAND ' ' (COMMAND-ARGS ' ')* ':' CONTENTS
;; SENDER := Not ':' | ' '
;; COMMAND := Not ':' | ' '
;; COMMAND-ARGS := Not ':' | ' '
;;
;; This gets parsed and stuffed into an `erc-response' struct.  You
;; can access the fields of the struct with:
;;
;; COMMAND --- `erc-response.command'
;; COMMAND-ARGS --- `erc-response.command-args'
;; CONTENTS --- `erc-response.contents'
;; SENDER --- `erc-response.sender'
;; LINE --- `erc-response.unparsed'
;; TAGS --- `erc-response.tags'
;;
;; WARNING, WARNING!!
;; It's probably not a good idea to destructively modify the list
;; of command-args in your handlers, since other functions down the
;; line may well need to access the arguments too.
;;
;; That is, unless you're /absolutely/ sure that your handler doesn't
;; invoke some other function that needs to use COMMAND-ARGS, don't do
;; something like
;;
;; (while (erc-response.command-args parsed)
;;   (let ((a (pop (erc-response.command-args parsed))))
;;     ...))
;;
;; The parsed response is handed over to
;; `erc-handle-parsed-server-response', which checks whether it should
;; carry out duplicate suppression, and then runs `erc-call-hooks'.
;; `erc-call-hooks' retrieves the relevant hook variable from
;; `erc-server-responses' and runs it.
;;
;; Most handlers then destructure the parsed response in some way
;; (depending on what the handler is, the arguments have different
;; meanings), and generally display something, usually using
;; `erc-display-message'.

;;; TODO:

;; o Generalize the display-line code so that we can use it to
;;   display the stuff we send, as well as the stuff we receive.
;;   Then, move all display-related code into another backend-like
;;   file, erc-display.el, say.
;;
;; o Clean up the handlers using new display code (has to be written
;;   first).

;;; History:

;; 2004/05/10 -- Handler bodies taken out of erc.el and ported to new
;;               interface.

;; 2005-08-13 -- Moved sending commands from erc.el.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'erc-common)

(defvar erc--display-context)
(defvar erc--msg-prop-overrides)
(defvar erc--target)
(defvar erc-channel-list)
(defvar erc-channel-members)
(defvar erc-default-nicks)
(defvar erc-default-recipients)
(defvar erc-ensure-target-buffer-on-privmsg)
(defvar erc-hide-prompt)
(defvar erc-input-marker)
(defvar erc-insert-marker)
(defvar erc-join-buffer)
(defvar erc-networks--id)
(defvar erc-nick)
(defvar erc-nick-change-attempt-count)
(defvar erc-verbose-server-ping)

(declare-function erc--ensure-query-member "erc" (name))
(declare-function erc--ensure-query-members "erc" ())
(declare-function erc--init-channel-modes "erc" (channel raw-args))
(declare-function erc--open-target "erc" (target))
(declare-function erc--parse-nuh "erc" (string))
(declare-function erc--query-list "erc" ())
(declare-function erc--remove-channel-users-but "erc" (nick))
(declare-function erc--target-from-string "erc" (string))
(declare-function erc--update-modes "erc" (raw-args))
(declare-function erc-active-buffer "erc" nil)
(declare-function erc-add-default-channel "erc" (channel))
(declare-function erc-banlist-update "erc" (proc parsed))
(declare-function erc-buffer-filter "erc" (predicate &optional proc))
(declare-function erc-buffer-list-with-nick "erc" (nick proc))
(declare-function erc-channel-begin-receiving-names "erc" nil)
(declare-function erc-channel-end-receiving-names "erc" nil)
(declare-function erc-channel-p "erc" (channel))
(declare-function erc-channel-receive-names "erc" (names-string))
(declare-function erc-cmd-JOIN "erc" (channel &optional key))
(declare-function erc-connection-established "erc" (proc parsed))
(declare-function erc-current-nick "erc" nil)
(declare-function erc-current-nick-p "erc" (nick))
(declare-function erc-current-time "erc" (&optional specified-time))
(declare-function erc-default-target "erc" nil)
(declare-function erc-delete-default-channel "erc" (channel &optional buffer))
(declare-function erc-display-error-notice "erc" (parsed string))
(declare-function erc-display-server-message "erc" (_proc parsed))
(declare-function erc-emacs-time-to-erc-time "erc" (&optional specified-time))
(declare-function erc-format-message "erc" (msg &rest args))
(declare-function erc-get-buffer "erc" (target &optional proc))
(declare-function erc-handle-login "erc" nil)
(declare-function erc-handle-user-status-change "erc" (type nlh &optional l))
(declare-function erc-ignored-reply-p "erc" (msg tgt proc))
(declare-function erc-ignored-user-p "erc" (spec))
(declare-function erc-is-message-ctcp-and-not-action-p "erc" (message))
(declare-function erc-is-message-ctcp-p "erc" (message))
(declare-function erc-log-irc-protocol "erc" (string &optional outbound))
(declare-function erc-login "erc" nil)
(declare-function erc-network "erc-networks" nil)
(declare-function erc-networks--id-given "erc-networks" (arg &rest args))
(declare-function erc-networks--id-reload "erc-networks" (arg &rest args))
(declare-function erc-nickname-in-use "erc" (nick reason))
(declare-function erc-parse-user "erc" (string))
(declare-function erc-process-away "erc" (proc away-p))
(declare-function erc-process-ctcp-query "erc" (proc parsed nick login host))
(declare-function erc-remove-channel-member "erc" (channel nick))
(declare-function erc-remove-channel-users "erc" nil)
(declare-function erc-remove-user "erc" (nick))
(declare-function erc-sec-to-time "erc" (ns))
(declare-function erc-server-buffer "erc" nil)
(declare-function erc-set-active-buffer "erc" (buffer))
(declare-function erc-set-current-nick "erc" (nick))
(declare-function erc-time-diff "erc" (t1 t2))
(declare-function erc-trim-string "erc" (s))
(declare-function erc-update-mode-line "erc" (&optional buffer))
(declare-function erc-update-mode-line-buffer "erc" (buffer))
(declare-function erc-wash-quit-reason "erc" (reason nick login host))

(declare-function erc--determine-speaker-message-format-args "erc"
                  (nick target message queryp privmsgp statusmsgp inputp
                        &optional prefix disp-nick))
(declare-function erc-display-message "erc"
                  (parsed type buffer msg &rest args))
(declare-function erc-get-buffer-create "erc"
                  (server port target &optional tgt-info id))
(declare-function erc-process-ctcp-reply "erc"
                  (proc parsed nick login host msg))
(declare-function erc-update-channel-topic "erc"
                  (channel topic &optional modify))
(declare-function erc-update-user-nick "erc"
                  (nick &optional new-nick host login full-name info))
(declare-function erc-open "erc"
                  (&optional server port nick full-name connect passwd tgt-list
                             channel process client-certificate user id))
(declare-function erc-update-channel-member "erc"
                  (channel nick new-nick
                           &optional add voice halfop op admin owner host
                           login full-name info update-message-time))

;;;; Variables and options

(defvar-local erc-session-password nil
  "The password used for the current session.
This should be a string or a function returning a string.")

(defvar erc-server-responses (make-hash-table :test #'equal)
  "Hash table mapping server responses to their handler hooks.")

;;; User data

(defvar-local erc-server-current-nick nil
  "Nickname on the current server.
Use `erc-current-nick' to access this.")

(defvar-local erc-session-user-full-name nil
  "Real name used for the current session.
Sent as the last argument to the USER command.")

(defvar-local erc-session-username nil
  "Username used for the current session.
Sent as the first argument of the USER command.")

;;; Server attributes

(defvar-local erc-server-process nil
  "The process object of the corresponding server connection.")

(defvar-local erc-session-server nil
  "The server name used to connect to for this session.")

(defvar-local erc-session-connector nil
  "The function used to connect to this session (nil for the default).")

(defvar-local erc-session-port nil
  "The port used to connect to.")

(defvar-local erc-session-client-certificate nil
  "TLS client certificate used when connecting over TLS.
If non-nil, should either be a list where the first element is
the certificate key file name, and the second element is the
certificate file name itself, or t, which means that
`auth-source' will be queried for the key and the certificate.")

(defvar-local erc-server-announced-name nil
  "The name the server announced to use.")

(defvar-local erc-server-version nil
  "The name and version of the server's ircd.")

(defvar-local erc-server-parameters nil
  "Alist listing the supported server parameters.

This is only set if the server sends 005 messages saying what is
supported on the server.

Entries are of the form:
  (PARAMETER . VALUE)
or
  (PARAMETER) if no value is provided.

where PARAMETER is a string and VALUE is a string or nil.  For
compatibility, a raw parameter of the form \"FOO=\" becomes
(\"FOO\" . \"\") even though it's equivalent to the preferred
canonical form \"FOO\" and its Lisp representation (\"FOO\").

Some examples of possible parameters sent by servers:
CHANMODES=b,k,l,imnpst - list of supported channel modes
CHANNELLEN=50 - maximum length of channel names
CHANTYPES=#&!+ - supported channel prefixes
CHARMAPPING=rfc1459 - character mapping used for nickname and channels
KICKLEN=160 - maximum allowed kick message length
MAXBANS=30 - maximum number of bans per channel
MAXCHANNELS=10 - maximum number of channels allowed to join
NETWORK=EFnet -  the network identifier
NICKLEN=9 - maximum allowed length of nicknames
PREFIX=(ov)@+ - list of channel modes and the user prefixes if user has mode
RFC2812 - server supports RFC 2812 features
SILENCE=10 - supports the SILENCE command, maximum allowed number of entries
TOPICLEN=160 - maximum allowed topic length
WALLCHOPS - supports sending messages to all operators in a channel")

(defvar-local erc--isupport-params nil
  "Hash map of \"ISUPPORT\" params.
Keys are symbols.  Values are lists of zero or more strings with hex
escapes removed.  ERC normalizes incoming parameters of the form
\"FOO=\" to (FOO).")

;;; Server and connection state

(defvar erc-server-ping-timer-alist nil
  "Mapping of server buffers to their specific ping timer.")

(defvar-local erc-server-connected nil
  "Non-nil if the current buffer belongs to an active IRC connection.
To determine whether an underlying transport is connected, use the
function `erc-server-process-alive' instead.")

(defvar-local erc-server-reconnect-count 0
  "Number of times we have failed to reconnect to the current server.")

(defvar-local erc--server-reconnect-display-timer nil
  "Timer that resets `erc--server-last-reconnect-count' to zero.
Becomes non-nil in all server buffers when an IRC connection is
first \"established\" and carries out its duties
`erc-auto-reconnect-display-timeout' seconds later.")

(defvar-local erc--server-last-reconnect-count 0
  "Snapshot of reconnect count when the connection was established.")

(defvar-local erc--server-reconnect-timer nil
  "Auto-reconnect timer for a network context.")

(defvar-local erc-server-quitting nil
  "Non-nil if the user requests a quit.")

(defvar-local erc-server-reconnecting nil
  "Non-nil if the user requests an explicit reconnect, and the
current IRC process is still alive.")
(make-obsolete-variable 'erc-server-reconnecting
                        "see `erc--server-reconnecting'" "29.1")

(defvar erc--server-reconnecting nil
  "An alist of buffer-local vars and their values when reconnecting.
This is for the benefit of local modules and `erc-mode-hook'
members so they can access buffer-local data from the previous
session when reconnecting.  Once `erc-reuse-buffers' is retired
and fully removed, modules can switch to leveraging the
`permanent-local' property instead.")

(defvar erc--server-post-connect-hook '(erc-networks--warn-on-connect)
  "Functions to run when a network connection is successfully opened.
Though internal, this complements `erc-connect-pre-hook' in that
it bookends the process rather than the logical connection, which
is the domain of `erc-before-connect' and `erc-after-connect'.
Note that unlike `erc-connect-pre-hook', this only runs in server
buffers, and it does so immediately before the first protocol
exchange.")

(defvar-local erc-server-timed-out nil
  "Non-nil if the IRC server failed to respond to a ping.")

(defvar-local erc-server-banned nil
  "Non-nil if the user is denied access because of a server ban.")

(defvar-local erc-server-error-occurred nil
  "Non-nil if the user triggers some server error.")

(defvar-local erc-server-lines-sent nil
  "Line counter.")

(defvar-local erc-server-last-peers nil
  "Last peers used, both sender and receiver.
Those are used for /MSG destination shortcuts.")

(defvar-local erc-server-last-sent-time nil
  "Time the message was sent.
This is useful for flood protection.")

(defvar-local erc-server-last-ping-time nil
  "Time the last ping was sent.
This is useful for flood protection.")

(defvar-local erc-server-last-received-time nil
  "Time the last message was received from the server.
This is useful for detecting hung connections.")

(defvar-local erc-server-lag nil
  "Calculated server lag time in seconds.
This variable is only set in a server buffer.")

(defvar-local erc-server-filter-data nil
  "The data that arrived from the server but has not been processed yet.")

(defvar-local erc-server-duplicates (make-hash-table :test 'equal)
  "Internal variable used to track duplicate messages.")

;; From Circe
(defvar-local erc-server-processing-p nil
  "Non-nil when we're currently processing a message.

When ERC receives a private message, it sets up a new buffer for
this query.  These in turn, though, do start flyspell.  This
involves starting an external process, in which case Emacs will
wait - and when it waits, it does accept other stuff from, say,
network exceptions.  So, if someone sends you two messages
quickly after each other, ispell is started for the first, but
might take long enough for the second message to be processed
first.")

(defvar-local erc-server-flood-last-message 0
  "When we sent the last message.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm.")

(defvar-local erc-server-flood-queue nil
  "The queue of messages waiting to be sent to the server.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm.")

(defvar-local erc-server-flood-timer nil
  "The timer to resume sending.")

;;; IRC protocol and misc options

(defgroup erc-server nil
  "Parameters for dealing with IRC servers."
  :group 'erc)

(defcustom erc-server-auto-reconnect t
  "Non-nil means that ERC will attempt to reestablish broken connections.

Reconnection will happen automatically for any unexpected disconnection."
  :type 'boolean)

(defcustom erc-server-reconnect-attempts 2
  "Number of times that ERC will attempt to reestablish a broken connection.
If t, always attempt to reconnect.

This only has an effect if `erc-server-auto-reconnect' is non-nil."
  :type '(choice (const :tag "Always reconnect" t)
                 integer))

(defcustom erc-server-reconnect-timeout 1
  "Number of seconds to wait between successive reconnect attempts.
If this value is too low, servers may reject your initial nick
request upon reconnecting because they haven't yet noticed that
your previous connection is dead.  If this happens, try setting
this value to 120 or greater and/or exploring the option
`erc-services-regain-alist', which may provide a more proactive
means of handling this situation on some servers."
  :type 'number)

(defcustom erc-server-reconnect-function 'erc-server-prefer-check-reconnect
  "Function called by the reconnect timer to create a new connection.
Called with a server buffer as its only argument.  Potential uses
include exponential backoff and probing for connectivity prior to
dialing.  Use `erc-schedule-reconnect' to instead try again later
and optionally alter the attempts tally."
  :package-version '(ERC . "5.6.1")
  :type '(choice (function-item erc-server-delayed-reconnect)
                 (function-item erc-server-delayed-check-reconnect)
                 (function-item erc-server-prefer-check-reconnect)
                 function))

(defcustom erc-split-line-length 440
  "The maximum length of a single message.
ERC normally splits chat input submitted at its prompt into
multiple messages when the initial size exceeds this value in
bytes.  Modules can tell ERC to forgo splitting entirely by
setting this to zero locally or, preferably, by binding it around
a remapped `erc-send-current-line' command.

IRC allows for lines up to 512 bytes.  Two of them are CR LF.
And a typical message looks like this:

  :nicky!uhuser@host212223.dialin.fnordisp.net PRIVMSG #lazybastards :Hello!

You can limit here the maximum length of the \"Hello!\" part.
Good luck."
  :type 'integer)

(defcustom erc-coding-system-precedence '(utf-8 undecided)
  "List of coding systems to be preferred when receiving a string from the server.
This will only be consulted if the coding system in
`erc-server-coding-system' is `undecided'."
  :version "24.1"
  :type '(repeat coding-system))

(defcustom erc-server-coding-system (if (and (coding-system-p 'undecided)
                                             (coding-system-p 'utf-8))
                                        '(utf-8 . undecided)
                                      nil)
  "The default coding system for incoming and outgoing text.
This is either a coding system, a cons, a function, or nil.

If a cons, the encoding system for outgoing text is in the car
and the decoding system for incoming text is in the cdr.  The most
interesting use for this is to put `undecided' in the cdr.  This
means that `erc-coding-system-precedence' will be consulted, and the
first match there will be used.

If a function, it is called with the argument `target' and should
return a coding system or a cons as described above.

If you need to send non-ASCII text to people not using a client that
does decoding on its own, you must tell ERC what encoding to use.
Emacs cannot guess it, since it does not know what the people on the
other end of the line are using."
  :type '(choice (const :tag "None" nil)
                 coding-system
                 (cons (coding-system :tag "encoding" :value utf-8)
                       (coding-system :tag "decoding" :value undecided))
                 function))

(defcustom erc-encoding-coding-alist nil
  "Alist of target regexp and coding-system pairs to use.
This overrides `erc-server-coding-system' depending on the
current target as returned by `erc-default-target'.

Example: If you know that the channel #linux-ru uses the coding-system
`cyrillic-koi8', then add (\"#linux-ru\" . cyrillic-koi8) to the
alist."
  :type '(repeat (cons (regexp :tag "Target")
                       coding-system)))

(defcustom erc-server-connect-function #'erc-open-network-stream
  "Function used to initiate a connection.
It should take same arguments as `open-network-stream' does."
  :type 'function)

(defcustom erc-server-prevent-duplicates '("301")
  "Either nil or a list of strings.
Each string is a IRC message type, like PRIVMSG or NOTICE.
All Message types in that list of subjected to duplicate prevention."
  :type '(repeat string))

(defcustom erc-server-duplicate-timeout 60
  "The time allowed in seconds between duplicate messages.

If two identical messages arrive within this value of one another, the second
isn't displayed."
  :type 'integer)

(defcustom erc-server-timestamp-format "%Y-%m-%d %T"
  "Timestamp format used with server response messages.
This string is processed using `format-time-string'."
  :version "24.3"
  :type 'string)

;;; Flood-related

;; Most of this is courtesy of Jorgen Schaefer and Circe
;; (https://www.nongnu.org/circe)

(defcustom erc-server-flood-margin 10
  "A margin on how much excess data we send.
The flood protection algorithm of ERC works like the one
detailed in RFC 2813, section 5.8 \"Flood control of clients\".

  * If `erc-server-flood-last-message' is less than the current
    time, set it equal.
  * While `erc-server-flood-last-message' is less than
    `erc-server-flood-margin' seconds ahead of the current
    time, send a message, and increase
    `erc-server-flood-last-message' by
    `erc-server-flood-penalty' for each message."
  :type 'integer)

(defcustom erc-server-flood-penalty 3
  "How much we penalize a message.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm."
  :type 'integer)

;; Ping handling

(defcustom erc-server-send-ping-interval 30
  "Interval of sending pings to the server, in seconds.
If this is set to nil, pinging the server is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom erc-server-send-ping-timeout 120
  "If the time between ping and response is greater than this, reconnect.
The time is in seconds.

This must be greater than or equal to the value for
`erc-server-send-ping-interval'.

If this is set to nil, never try to reconnect."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defvar-local erc-server-ping-handler nil
  "The periodic server ping timer.")

;;;; Helper functions

(defvar erc--reject-unbreakable-lines nil
  "Signal an error when a line exceeds `erc-split-line-length'.
Sending such lines and hoping for the best is no longer supported
in ERC 5.6.  This internal var exists as a possibly temporary
escape hatch for inhibiting their transmission.")

(defun erc--split-line (longline)
  (let* ((coding (erc-coding-system-for-target nil))
         (original-window-buf (window-buffer (selected-window)))
         out)
    (when (consp coding)
      (setq coding (car coding)))
    (setq coding (coding-system-change-eol-conversion coding 'unix))
    (with-temp-buffer
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) (current-buffer))
            (insert longline)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((upper (filepos-to-bufferpos erc-split-line-length
                                                 'exact coding)))
                (goto-char (or upper (point-max)))
                (unless (eobp)
                  (skip-chars-backward "^ \t"))
                (when (bobp)
                  (when erc--reject-unbreakable-lines
                    (user-error
                     (substitute-command-keys
                      (concat "Unbreakable line encountered (Recover input"
                              " with \\[erc-previous-command])"))))
                  (goto-char upper))
                (when-let* ((cmp (find-composition (point) (1+ (point)))))
                  (if (= (car cmp) (point-min))
                      (goto-char (nth 1 cmp))
                    (goto-char (car cmp)))))
              (when (= (point-min) (point))
                (goto-char (point-max)))
              (push (buffer-substring-no-properties (point-min) (point)) out)
              (delete-region (point-min) (point)))
            (or (nreverse out) (list "")))
        (set-window-buffer (selected-window) original-window-buf)))))

;; From Circe
(defun erc-split-line (longline)
  "Return a list of lines which are not too long for IRC.
The length is specified in `erc-split-line-length'.

Currently this is called by `erc-send-input'."
  (let* ((coding (erc-coding-system-for-target nil))
         (charset (if (consp coding) (car coding) coding)))
    (with-temp-buffer
      (insert longline)
      ;; The line lengths are in octets, not characters (because these
      ;; are server protocol limits), so we have to first make the
      ;; text into bytes, then fold the bytes on "word" boundaries,
      ;; and then make the bytes into text again.
      (encode-coding-region (point-min) (point-max) charset)
      (let ((fill-column erc-split-line-length))
        (fill-region (point-min) (point-max)
                     nil t))
      (decode-coding-region (point-min) (point-max) charset)
      (split-string (buffer-string) "\n"))))

(defun erc-forward-word ()
  "Move forward one word, ignoring any subword settings.
If no `subword-mode' is active, then this is (forward-word)."
  (skip-syntax-forward "^w")
  (> (skip-syntax-forward "w") 0))

(defun erc-word-at-arg-p (pos)
  "Report whether the char after a given POS has word syntax.
If POS is out of range, the value is nil."
  (let ((c (char-after pos)))
    (if c
        (eq ?w (char-syntax c))
      nil)))

(defun erc-bounds-of-word-at-point ()
  "Return the bounds of word at point, or nil if we're not at a word.
If no `subword-mode' is active, then this is
\(bounds-of-thing-at-point \\='word)."
  (if (or (erc-word-at-arg-p (point))
          (erc-word-at-arg-p (1- (point))))
      (save-excursion
        (let* ((start (progn (skip-syntax-backward "w") (point)))
               (end   (progn (skip-syntax-forward  "w") (point))))
          (cons start end)))
    nil))

;; Used by CTCP functions
(defun erc-upcase-first-word (str)
  "Upcase the first word in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (upcase-region (point) (progn (erc-forward-word) (point)))
    (buffer-string)))

(defun erc-server-setup-periodical-ping (buffer)
  "Set up a timer to periodically ping the current server.
The current buffer is given by BUFFER."
  (with-current-buffer buffer
    (when erc-server-ping-handler
      (cancel-timer erc-server-ping-handler))
    (when erc-server-send-ping-interval
      (setq erc-server-ping-handler (run-with-timer
                                     4 erc-server-send-ping-interval
                                     #'erc-server-send-ping
                                     buffer))

      ;; I check the timer alist for an existing timer. If one exists,
      ;; I get rid of it
      (let ((timer-tuple (assq buffer erc-server-ping-timer-alist)))
        (if timer-tuple
            ;; this buffer already has a timer. Cancel it and set the new one
            (progn
              (cancel-timer (cdr timer-tuple))
              (setf (cdr (assq buffer erc-server-ping-timer-alist)) erc-server-ping-handler))

          ;; no existing timer for this buffer. Add new one
          (add-to-list 'erc-server-ping-timer-alist
                       (cons buffer erc-server-ping-handler)))))))

(defun erc-server-process-alive (&optional buffer)
  "Return non-nil when BUFFER has an `erc-server-process' open or running."
  (with-current-buffer (or buffer (current-buffer))
    (and erc-server-process
         (processp erc-server-process)
         (memq (process-status erc-server-process) '(run open)))))

;;;; Connecting to a server
(defun erc-open-network-stream (name buffer host service &rest parameters)
  "Like `open-network-stream', but does non-blocking IO."
  (let ((p (plist-put parameters :nowait t)))
    (apply #'open-network-stream name buffer host service p)))

(cl-defmethod erc--register-connection ()
  "Perform opening IRC protocol exchange with server."
  (run-hooks 'erc--server-post-connect-hook)
  (erc-login))

(defvar erc--server-post-dial-function
  #'erc--server-propagate-failed-connection
  "Function called one second after creating a server process.
Called with the newly created process just before the opening IRC
protocol exchange.")

(defun erc--server-propagate-failed-connection (process)
  "Ensure the PROCESS sentinel runs at least once on early failure.
Act as a watchdog timer to force `erc-process-sentinel' and its
finalizers, like `erc-disconnected-hook', to run when PROCESS has
a status of `failed' after one second.  But only do so when its
error data is something ERC recognizes.  Print an explanation to
the server buffer in any case."
  (when (eq (process-status process) 'failed)
    (erc-display-message
     nil '(notice error) (process-buffer process)
     (format "Process exit status: %S" (process-exit-status process)))
    (pcase (process-exit-status process)
      (111
       (erc-process-sentinel process "failed with code 111\n"))
      (`(file-error . ,_)
       (erc-process-sentinel process "failed with code -523\n"))
      ((rx "tls" (+ nonl) "failed")
       (erc-process-sentinel process "failed with code -525\n")))))

(defvar erc--server-connect-dumb-ipv6-regexp
  ;; Not for validation (gives false positives).
  (rx bot "[" (group (+ (any xdigit digit ":.")) (? "%" (+ alnum))) "]" eot))

(defun erc-server-connect (server port buffer &optional client-certificate)
  "Perform the connection and login using the specified SERVER and PORT.
We will store server variables in the buffer given by BUFFER.
CLIENT-CERTIFICATE may optionally be used to specify a TLS client
certificate to use for authentication when connecting over
TLS (see `erc-session-client-certificate' for more details)."
  (when (string-match erc--server-connect-dumb-ipv6-regexp server)
    (setq server (match-string 1 server)))
  (let ((msg (erc-format-message 'connect ?S server ?p port)) process
        (args `(,(format "erc-%s-%s" server port) nil ,server ,port)))
    (when client-certificate
      (setq args `(,@args :client-certificate ,client-certificate)))
    (message "%s" msg)
    (setq process (apply erc-server-connect-function args))
    (unless (processp process)
      (error "Connection attempt failed"))
    ;; Misc server variables
    (with-current-buffer buffer
      (setq erc-server-filter-data nil)
      (setq erc-server-process process)
      (setq erc-server-quitting nil)
      (setq erc-server-reconnecting nil
            erc--server-reconnect-timer nil)
      (setq erc-server-timed-out nil)
      (setq erc-server-banned nil)
      (setq erc-server-error-occurred nil)
      (let ((time (erc-current-time)))
        (setq erc-server-last-sent-time time)
        (setq erc-server-last-ping-time time)
        (setq erc-server-last-received-time time))
      (setq erc-server-lines-sent 0)
      ;; last peers (sender and receiver)
      (setq erc-server-last-peers (cons nil nil)))
    ;; we do our own encoding and decoding
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system process 'raw-text))
    ;; process handlers
    (set-process-sentinel process #'erc-process-sentinel)
    (set-process-filter process #'erc-server-filter-function)
    (set-process-buffer process buffer)
    (erc-log "\n\n\n********************************************\n")
    (message "%s" (erc-format-message
                   'login ?n
                   (with-current-buffer buffer (erc-current-nick))))
    ;; wait with script loading until we receive a confirmation (first
    ;; MOTD line)
    (if (eq (process-status process) 'connect)
        ;; waiting for a non-blocking connect - keep the user informed
        (let ((erc--msg-prop-overrides `((erc--skip . (stamp))
                                         ,@erc--msg-prop-overrides)))
          (erc-display-message nil nil buffer "Opening connection..\n")
          (run-at-time 1 nil erc--server-post-dial-function process))
      (message "%s...done" msg)
      (erc--register-connection))))

(defun erc-server-reconnect ()
  "Reestablish the current IRC connection.
Make sure you are in an ERC buffer when running this."
  (let ((buffer (erc-server-buffer)))
    (unless (buffer-live-p buffer)
      (if (eq major-mode 'erc-mode)
          (setq buffer (current-buffer))
        (error "Reconnect must be run from an ERC buffer")))
    (with-current-buffer buffer
      (erc-update-mode-line)
      (erc-set-active-buffer (current-buffer))
      (setq erc-server-last-sent-time 0)
      (setq erc-server-lines-sent 0)
      (let ((erc-server-connect-function (or erc-session-connector
                                             #'erc-open-network-stream))
            (erc--server-reconnecting (buffer-local-variables)))
        (erc-open erc-session-server erc-session-port erc-server-current-nick
                  erc-session-user-full-name t erc-session-password
                  nil nil nil erc-session-client-certificate
                  erc-session-username
                  (erc-networks--id-given erc-networks--id))
        (defvar erc-reuse-buffers)
        (unless (with-suppressed-warnings ((obsolete erc-reuse-buffers))
                  erc-reuse-buffers)
          (cl-assert (not (eq buffer (current-buffer)))))))))

(defun erc-server-delayed-reconnect (buffer)
  (if (buffer-live-p buffer)
    (with-current-buffer buffer
      (erc-server-reconnect))))

(defun erc--server-reconnect-opened (buffer process)
  "Reconnect session for server BUFFER using open PROCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((orig erc-session-connector)
             (erc-session-connector
              (lambda (&rest _)
                (setq erc-session-connector orig)
                process)))
        (erc-server-reconnect)))))

(defvar-local erc--server-reconnect-timeout nil)

;; These variables exist for use in unit tests.
(defvar erc--server-reconnect-timeout-check 10)
(defvar erc--server-reconnect-timeout-scale-function
  #'erc--server-reconnect-timeout-double)

(defun erc--server-reconnect-timeout-double (existing)
  "Double EXISTING timeout, but cap it at 5 minutes."
  (min 300 (* existing 2)))

(defun erc--recon-probe-reschedule (proc)
  "Print a message saying PROC's intended peer can't be reached.
Then call `erc-schedule-reconnect'."
  (let ((buffer (or (and-let* ((proc)
                               (buffer (process-buffer proc))
                               ((buffer-live-p buffer))
                               (buffer)))
                    (current-buffer))))
    (with-current-buffer buffer
      (let ((erc-server-reconnect-timeout
             (or erc--server-reconnect-timeout
                 erc-server-reconnect-timeout)))
        (when (and proc (not (eq proc erc-server-process)))
          (set-process-sentinel proc #'ignore)
          (delete-process proc))
        (erc-display-message nil '(notice error) buffer
                             'recon-probe-nobody-home)
        (erc-schedule-reconnect buffer 0)))))

(defvar erc-server-delayed-check-reconnect-reuse-process-p t
  "Whether to reuse a successful probe as the session process.")

(defun erc--recon-probe-sentinel (proc event)
  "Send a \"PING\" to PROC's peer on an \"open\" EVENT.
Otherwise, try connecting from scratch again after timeout."
  (pcase event
    ("open\n"
     (set-process-sentinel proc #'ignore)
     ;; This has been observed to possibly raise a `file-error'.
     (if erc-server-delayed-check-reconnect-reuse-process-p
         (run-at-time nil nil #'erc--server-reconnect-opened
                      (process-buffer proc) proc)
       (run-at-time nil nil #'delete-process proc)
       (run-at-time nil nil #'erc-server-delayed-reconnect
                    (process-buffer proc))))
    ((or "connection broken by remote peer\n" (rx bot "failed"))
     (run-at-time nil nil #'erc--recon-probe-reschedule proc))))

(defun erc--recon-probe-check (proc expire)
  "Restart reconnect probe if PROC has failed or EXPIRE time has passed.
Otherwise, if PROC's buffer is live and its status is `connect', arrange
for running again in 1 second."
  (let* ((buffer (process-buffer proc))
         ;;
         status)
    (cond ((not (buffer-live-p buffer)))
          ((time-less-p expire (current-time))
           ;; TODO convert into proper catalog message for i18n.
           (erc-display-message nil 'error buffer "Timed out while dialing...")
           (erc--recon-probe-reschedule proc))
          ((eq (setq status (process-status proc)) 'failed)
           (erc--recon-probe-reschedule proc))
          ((eq status 'connect)
           (run-at-time 1 nil #'erc--recon-probe-check proc expire)))))

;; This probing strategy may appear to hang at various junctures.  It's
;; assumed that when *Messages* contains "Waiting for socket ..."  or
;; similar, progress will be made eventually.
(defun erc-server-delayed-check-reconnect (buffer)
  "Wait for internet connectivity before trying to reconnect.
Use server BUFFER's cached session info to reestablish the logical
connection at the IRC protocol level.  Do this by probing for any
response to a PING, including a hang up, before (possibly) dialing again
and commencing with \"connection registration\".  Make no distinction
between configuration issues and the absence of service in printed
feedback.  For example, expect users of proxy-based connectors, like
`erc-open-socks-tls-stream', to ensure their setup works before choosing
this function as their reconnector."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq erc--server-reconnect-timeout
            (funcall erc--server-reconnect-timeout-scale-function
                     (or erc--server-reconnect-timeout
                         erc-server-reconnect-timeout)))
      (condition-case _
          (let* ((cert erc-session-client-certificate)
                 (server (if (string-match erc--server-connect-dumb-ipv6-regexp
                                           erc-session-server)
                             (match-string 1 erc-session-server)
                           erc-session-server))
                 (name (if erc-server-delayed-check-reconnect-reuse-process-p
                           (format "erc-%s-%s" server erc-session-port)
                         "*erc-connectivity-check*"))
                 (proc (apply erc-session-connector name
                              nil server erc-session-port
                              (and cert (list :client-certificate cert))))
                 (status (process-status proc)))
            (set-process-buffer proc buffer)
            (set-process-filter proc #'ignore)
            (if (not (eq status 'connect)) ; :nowait is nil
                (erc--recon-probe-sentinel proc (if (eq status 'open)
                                                    "open\n"
                                                  "failed"))
              (run-at-time 1 nil #'erc--recon-probe-check proc
                           (time-add erc--server-reconnect-timeout-check
                                     (current-time)))
              (set-process-sentinel proc #'erc--recon-probe-sentinel)))
        ;; E.g., "make client process failed" "Connection refused".
        (file-error (erc--recon-probe-reschedule nil))
        ;; C-g during blocking connect, like with the SOCKS connector.
        (quit (erc--cancel-auto-reconnect-timer))))))

(defun erc-server-prefer-check-reconnect (buffer)
  "Defer to another reconnector based on BUFFER's `erc-session-connector'.
Prefer `erc-server-delayed-check-reconnect' if the connector is known to
be \"check-aware\".  Otherwise, use `erc-server-delayed-reconnect'."
  (if (memq (buffer-local-value 'erc-session-connector buffer)
            '(erc-open-tls-stream
              erc-open-network-stream
              erc-open-socks-tls-stream))
      (erc-server-delayed-check-reconnect buffer)
    (erc-server-delayed-reconnect buffer)))

(defun erc-server-filter-function (process string)
  "The process filter for the ERC server."
  (with-current-buffer (process-buffer process)
    (setq erc-server-last-received-time (erc-current-time))
    ;; If you think this is written in a weird way - please refer to the
    ;; docstring of `erc-server-processing-p'
    (if erc-server-processing-p
        (setq erc-server-filter-data
              (if erc-server-filter-data
                  (concat erc-server-filter-data string)
                string))
      ;; This will be true even if another process is spawned!
      (let ((erc-server-processing-p t))
        (setq erc-server-filter-data (if erc-server-filter-data
                                           (concat erc-server-filter-data
                                                   string)
                                         string))
        (while (and erc-server-filter-data
                    (string-match "[\n\r]+" erc-server-filter-data))
          (let ((line (substring erc-server-filter-data
                                 0 (match-beginning 0))))
            (setq erc-server-filter-data
                  (if (= (match-end 0)
                         (length erc-server-filter-data))
                      nil
                    (substring erc-server-filter-data
                               (match-end 0))))
            (erc-log-irc-protocol line nil)
            (erc-parse-server-response process line)))))))

(defun erc--server-reconnect-p (event)
  "Return non-nil when ERC should attempt to reconnect.
EVENT is the message received from the closed connection process."
  (and erc-server-auto-reconnect
       (not erc-server-banned)
       ;; make sure we don't infinitely try to reconnect, unless the
       ;; user wants that
       (or (eq erc-server-reconnect-attempts t)
           (and (integerp erc-server-reconnect-attempts)
                (< erc-server-reconnect-count
                   erc-server-reconnect-attempts)))
       (or erc-server-timed-out
           (not (string-match "^deleted" event)))
       ;; open-network-stream-nowait error for connection refused
       (if (string-match "^failed with code 111" event) 'nonblocking t)))

(defun erc-server-reconnect-p (event)
  "Return non-nil if ERC should attempt to reconnect automatically.
EVENT is the message received from the closed connection process."
  (declare (obsolete "see `erc--server-reconnect-p'" "29.1"))
  (or (with-suppressed-warnings ((obsolete erc-server-reconnecting))
        erc-server-reconnecting)
      (erc--server-reconnect-p event)))

(defun erc--server-last-reconnect-on-disconnect (&rest _)
  (remove-hook 'erc-disconnected-hook
               #'erc--server-last-reconnect-on-disconnect t)
  (erc--server-last-reconnect-display-reset (current-buffer)))

(defun erc--server-last-reconnect-display-reset (buffer)
  "Deactivate `erc-auto-reconnect-display'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when erc--server-reconnect-display-timer
        (cancel-timer erc--server-reconnect-display-timer)
        (remove-hook 'erc-disconnected-hook
                     #'erc--server-last-reconnect-display-reset t)
        (setq erc--server-reconnect-display-timer nil
              erc--server-last-reconnect-count 0)))))

(defconst erc--mode-line-process-reconnecting
  '(:eval (erc-with-server-buffer
            (and erc--server-reconnect-timer
                 (format ": reconnecting in %.1fs"
                         (- (timer-until erc--server-reconnect-timer
                                         (current-time)))))))
  "Mode-line construct showing seconds until next reconnect attempt.
Move point around to refresh.")

(defun erc--cancel-auto-reconnect-timer ()
  (when erc--server-reconnect-timer
    (cancel-timer erc--server-reconnect-timer)
    (erc-display-message nil 'notice nil 'reconnect-canceled
                         ?u (buffer-name)
                         ?c (- (timer-until erc--server-reconnect-timer
                                            (current-time))))
    (setq erc--server-reconnect-timer nil)
    (erc-update-mode-line)))

(defun erc-schedule-reconnect (buffer &optional incr)
  "Create and return a reconnect timer for BUFFER.
When `erc-server-reconnect-attempts' is a number, increment
`erc-server-reconnect-count' by INCR unconditionally."
  (let ((count (and (integerp erc-server-reconnect-attempts)
                    (- erc-server-reconnect-attempts
                       (cl-incf erc-server-reconnect-count (or incr 1)))))
        (proc (buffer-local-value 'erc-server-process buffer)))
    (erc-display-message nil '(notice error) buffer 'reconnecting
                         ?m erc-server-reconnect-timeout
                         ?i (if count erc-server-reconnect-count "N")
                         ?n (if count erc-server-reconnect-attempts "A"))
    (set-process-sentinel proc #'ignore)
    (delete-process proc)
    (erc-update-mode-line)
    (setq erc-server-reconnecting nil
          erc--server-reconnect-timer
          (run-at-time erc-server-reconnect-timeout nil
                       erc-server-reconnect-function buffer))))

(defun erc-process-sentinel-2 (event buffer)
  "Called when `erc-process-sentinel-1' has detected an unexpected disconnect."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((reconnect-p (erc--server-reconnect-p event)) message)
        (setq message (if reconnect-p 'disconnected 'disconnected-noreconnect))
        (erc-display-message nil 'error (current-buffer) message)
        (if (not reconnect-p)
            ;; terminate, do not reconnect
            (progn
              (setq erc--server-reconnect-timer nil)
              (erc-display-message nil 'error (current-buffer)
                                   'terminated ?e event)
              (set-buffer-modified-p nil))
          ;; reconnect
          (erc-schedule-reconnect buffer))))
    (erc-update-mode-line)))

(defun erc-process-sentinel-1 (event buffer)
  "Called when `erc-process-sentinel' has decided that we're disconnecting.
Determine whether user has quit or whether erc has been terminated.
Conditionally try to reconnect and take appropriate action."
  (with-current-buffer buffer
    (if erc-server-quitting
        ;; normal quit
        (progn
          (erc-display-message nil 'error (current-buffer) 'finished)
          ;; Update mode line indicators
          (erc-update-mode-line)
          ;; Kill server buffer if user wants it
          (set-buffer-modified-p nil)
          (defvar erc-kill-server-buffer-on-quit)
          (when erc-kill-server-buffer-on-quit
            (kill-buffer (current-buffer))))
      ;; unexpected disconnect
      (erc-process-sentinel-2 event buffer))))

(defvar-local erc--hidden-prompt-overlay nil
  "Overlay for hiding the prompt when disconnected.")

(cl-defmethod erc--reveal-prompt ()
  (when erc--hidden-prompt-overlay
    (delete-overlay erc--hidden-prompt-overlay)
    (setq erc--hidden-prompt-overlay nil)))

(cl-defmethod erc--conceal-prompt ()
  (when-let* (((null erc--hidden-prompt-overlay))
              (ov (make-overlay erc-insert-marker (1- erc-input-marker)
                                nil 'front-advance)))
    (defvar erc-prompt-hidden)
    (overlay-put ov 'display erc-prompt-hidden)
    (setq erc--hidden-prompt-overlay ov)))

(defun erc--prompt-hidden-p ()
  (and (marker-position erc-insert-marker)
       (eq (get-text-property erc-insert-marker 'erc-prompt) 'hidden)))

(defun erc--unhide-prompt ()
  (remove-hook 'pre-command-hook #'erc--unhide-prompt-on-self-insert t)
  (when (and (marker-position erc-insert-marker)
             (marker-position erc-input-marker))
    (with-silent-modifications
      (put-text-property erc-insert-marker (1- erc-input-marker) 'erc-prompt t)
      (erc--reveal-prompt)
      (run-hooks 'erc--refresh-prompt-hook))))

(defun erc--unhide-prompt-on-self-insert ()
  (when (and (eq this-command #'self-insert-command)
             (or (eobp) (= (point) erc-input-marker)))
    (erc--unhide-prompt)))

(defun erc--hide-prompt (proc)
  "Hide prompt in all buffers of server.
Change value of property `erc-prompt' from t to `hidden'."
  (erc-with-all-buffers-of-server proc nil
    (when (and erc-hide-prompt
               (or (eq erc-hide-prompt t)
                   (memq (if erc--target
                             (if (erc--target-channel-p erc--target)
                                 'channel
                               'query)
                           'server)
                         erc-hide-prompt))
               (marker-position erc-insert-marker)
               (marker-position erc-input-marker)
               (get-text-property erc-insert-marker 'erc-prompt))
      (with-silent-modifications
        (put-text-property erc-insert-marker (1- erc-input-marker)
                           'erc-prompt 'hidden)
        (erc--conceal-prompt)
        (run-hooks 'erc--refresh-prompt-hook))
      (add-hook 'pre-command-hook #'erc--unhide-prompt-on-self-insert 80 t))))

(defun erc-process-sentinel (cproc event)
  "Sentinel function for ERC process."
  (let ((buf (process-buffer cproc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (erc-log (format
                  "SENTINEL: proc: %S    status: %S  event: %S (quitting: %S)"
                  cproc (process-status cproc) event erc-server-quitting))
        (if (string-match "^open" event)
            ;; newly opened connection (no wait)
            (erc--register-connection)
          ;; assume event is 'failed
          (erc-with-all-buffers-of-server cproc nil
            (setq erc-server-connected nil))
          (when erc-server-ping-handler
            (progn (cancel-timer erc-server-ping-handler)
                   (setq erc-server-ping-handler nil)))
          (run-hook-with-args 'erc-disconnected-hook
                              (erc-current-nick) (system-name) "")
          (erc-with-all-buffers-of-server cproc (lambda () erc-channel-members)
            (when (erc--target-channel-p erc--target)
              (setf (erc--target-channel-joined-p erc--target) nil))
            (clrhash erc-channel-members))
          ;; Hide the prompt
          (erc--hide-prompt cproc)
          ;; Decide what to do with the buffer
          ;; Restart if disconnected
          (erc-process-sentinel-1 event buf))))))

;;;; Sending messages

(defun erc-coding-system-for-target (target)
  "Return the coding system or cons cell appropriate for TARGET.
This is determined via `erc-encoding-coding-alist' or
`erc-server-coding-system'."
  (unless target (setq target (erc-default-target)))
  (or (when target
        (let ((case-fold-search t))
          (catch 'match
            (dolist (pat erc-encoding-coding-alist)
              (when (string-match (car pat) target)
                (throw 'match (cdr pat)))))))
      (and (functionp erc-server-coding-system)
           (funcall erc-server-coding-system target))
      erc-server-coding-system))

(defun erc-decode-string-from-target (str target)
  "Decode STR as appropriate for TARGET.
This is indicated by `erc-encoding-coding-alist', defaulting to the
value of `erc-server-coding-system'."
  (unless (stringp str)
    (setq str ""))
  (let ((coding (erc-coding-system-for-target target)))
    (when (consp coding)
      (setq coding (cdr coding)))
    (when (eq coding 'undecided)
      (let ((codings (detect-coding-string str))
            (precedence erc-coding-system-precedence))
        (while (and precedence
                    (not (memq (car precedence) codings)))
          (pop precedence))
        (when precedence
          (setq coding (car precedence)))))
    (decode-coding-string str coding t)))

;; proposed name, not used by anything yet
(defun erc-send-line (text display-fn)
  "Send TEXT to the current server.  Wrapping and flood control apply.
Use DISPLAY-FN to show the results."
  (mapc (lambda (line)
          (erc-server-send line)
          (funcall display-fn))
        (erc-split-line text)))

;; From Circe, with modifications
(defun erc-server-send (string &optional force target)
  "Send STRING to the current server.
When FORCE is non-nil, bypass flood protection so that STRING is
sent directly without modifying the queue.  When FORCE is the
symbol `no-penalty', exempt this round from accumulating a
timeout penalty and schedule it to run ASAP instead of blocking.

If TARGET is specified, look up encoding information for that
channel in `erc-encoding-coding-alist' or
`erc-server-coding-system'.

See `erc-server-flood-margin' for an explanation of the flood
protection algorithm."
  (erc--server-send string force target))

(cl-defmethod erc--server-send (string force target)
  "Encode and send STRING to `erc-server-process'.
Expect STRING, FORCE, and TARGET to originate from `erc-server-send'."
  (erc-log (concat "erc-server-send: " string "(" (buffer-name) ")"))
  (setq erc-server-last-sent-time (erc-current-time))
  (let ((encoding (erc-coding-system-for-target target)))
    (when (consp encoding)
      (setq encoding (car encoding)))
    (if (erc-server-process-alive)
        (erc-with-server-buffer
          (let ((str (concat string "\r\n")))
            (if force
                (progn
                  (unless (eq force 'no-penalty)
                    (cl-incf erc-server-flood-last-message
                             erc-server-flood-penalty))
                  (erc-log-irc-protocol str 'outbound)
                  (condition-case nil
                      (progn
                        ;; Set encoding just before sending the string
                        (when (fboundp 'set-process-coding-system)
                          (set-process-coding-system erc-server-process
                                                     'raw-text encoding))
                        (if (and (eq force 'no-penalty))
                            (run-at-time nil nil #'process-send-string
                                         erc-server-process str)
                          (process-send-string erc-server-process str)))
                    ;; See `erc-server-send-queue' for full
                    ;; explanation of why we need this condition-case
                    (error nil)))
              (setq erc-server-flood-queue
                    (append erc-server-flood-queue
                            (list (cons str encoding))))
              (run-at-time nil nil #'erc-server-send-queue (current-buffer))))
          t)
      (message "ERC: No process running")
      nil)))

(defun erc-server-send-ping (buf)
  "Send a ping to the IRC server buffer in BUF.
Additionally, detect whether the IRC process has hung."
  (if (and (buffer-live-p buf)
           (with-current-buffer buf
             erc-server-last-received-time))
      (with-current-buffer buf
        (if (and erc-server-send-ping-timeout
                 (time-less-p
                  erc-server-send-ping-timeout
                  (time-since erc-server-last-received-time)))
            (progn
              ;; if the process is hung, kill it
              (setq erc-server-timed-out t)
              (delete-process erc-server-process))
          (erc-server-send (format "PING %.0f" (erc-current-time)))))
    ;; remove timer if the server buffer has been killed
    (let ((timer (assq buf erc-server-ping-timer-alist)))
      (when timer
        (cancel-timer (cdr timer))
        (setcdr timer nil)))))

;; From Circe
(defun erc-server-send-queue (buffer)
  "Send messages in `erc-server-flood-queue'.
See `erc-server-flood-margin' for an explanation of the flood
protection algorithm."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((now (current-time)))
        (when erc-server-flood-timer
          (cancel-timer erc-server-flood-timer)
          (setq erc-server-flood-timer nil))
        (when (time-less-p erc-server-flood-last-message now)
          (setq erc-server-flood-last-message (erc-emacs-time-to-erc-time now)))
        (while (and erc-server-flood-queue
                    (time-less-p erc-server-flood-last-message
                                 (time-add now erc-server-flood-margin)))
          (let ((msg (caar erc-server-flood-queue))
                (encoding (cdar erc-server-flood-queue)))
            (setq erc-server-flood-queue (cdr erc-server-flood-queue)
                  erc-server-flood-last-message
                  (+ erc-server-flood-last-message
                     erc-server-flood-penalty))
            (erc-log-irc-protocol msg 'outbound)
            (erc-log (concat "erc-server-send-queue: "
                             msg "(" (buffer-name buffer) ")"))
            (when (erc-server-process-alive)
              (condition-case nil
                  ;; Set encoding just before sending the string
                  (progn
                    (when (fboundp 'set-process-coding-system)
                      (set-process-coding-system erc-server-process
                                                 'raw-text encoding))
                    (process-send-string erc-server-process msg))
                ;; Sometimes the send can occur while the process is
                ;; being killed, which results in a weird SIGPIPE error.
                ;; Catch this and ignore it.
                (error nil)))))
        (when erc-server-flood-queue
          (setq erc-server-flood-timer
                (run-at-time (+ 0.2 erc-server-flood-penalty)
                             nil #'erc-server-send-queue buffer)))))))

(defun erc-message (message-command line &optional force)
  "Send LINE, possibly expanding a target specifier beforehand.
Expect MESSAGE-COMMAND to be an IRC command with a single
positional target parameter followed by a trailing parameter.

If the target is \",\", the last person you've got a message from will
be used.  If the target is \".\", the last person you've sent a message
to will be used."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\) ?\\(.*\\)" line)
    (let* ((tgt (match-string 1 line))
           (s (match-string 2 line))
           (server-buffer (erc-server-buffer))
           (peers (buffer-local-value 'erc-server-last-peers server-buffer)))
      (erc-log (format "cmd: MSG(%s): [%s] %s" message-command tgt s))
      (cond
       ((string= tgt ",")
        (setq tgt (car peers)))
       ((string= tgt ".")
        (setq tgt (cdr peers))))
      (cond
       (tgt
        (with-current-buffer server-buffer
          (setq erc-server-last-peers (cons (car peers) tgt)))
        (erc-server-send (format "%s %s :%s" message-command tgt s)
                         force))
       (t
        (erc-display-message nil 'error (current-buffer) 'no-target))))
    t)
   (t nil)))

;;; CTCP

(defun erc-send-ctcp-message (tgt l &optional force)
  "Send CTCP message L to TGT.

If TGT is nil the message is not sent.
The command must contain neither a prefix nor a trailing `\\n'.

See also `erc-server-send'."
  (let ((l (erc-upcase-first-word l)))
    (cond
     (tgt
      (erc-log (format "erc-send-CTCP-message: [%s] %s" tgt l))
      (erc-server-send (format "PRIVMSG %s :\C-a%s\C-a" tgt l)
                       force)))))

(defun erc-send-ctcp-notice (tgt l &optional force)
  "Send CTCP notice L to TGT.

If TGT is nil the message is not sent.
The command must contain neither a prefix nor a trailing `\\n'.

See also `erc-server-send'."
  (let ((l (erc-upcase-first-word l)))
    (cond
     (tgt
      (erc-log (format "erc-send-CTCP-notice: [%s] %s" tgt l))
      (erc-server-send (format "NOTICE %s :\C-a%s\C-a" tgt l)
                       force)))))

;;;; Handling responses

(defcustom erc-tags-format 'overridable
  "Shape of the `tags' alist in `erc-response' objects.
When set to `legacy', pre-5.5 parsing behavior takes effect for
the tags portion of every message.  The resulting alist contains
conses of the form (STRING . LIST), in which LIST is comprised of
at most one, possibly empty string.  When set to nil, ERC only
parses tags if an active module defines an implementation.  It
otherwise ignores them.  In such cases, each alist element is a
cons of a symbol and an optional, nonempty string.

With the default value of `overridable', ERC behaves as it does
with `legacy' except that it emits a warning whenever first
encountering a message containing tags in a given Emacs session.
But it only does so when a module implementing overriding,
non-legacy behavior isn't already active in the current network
context.

Note that future bundled modules providing IRCv3 functionality
will not be compatible with the legacy format.  User code should
eventually transition to expecting this \"5.5+ variant\" and set
this option to nil."
  :package-version '(ERC . "5.5")
  :type '(choice (const nil)
                 (const legacy)
                 (const overridable)))

(defun erc-parse-tags (string)
  "Parse IRCv3 tags list in STRING to a (tag . value) alist."
  (erc--parse-message-tags string))

(defun erc--parse-tags (string)
  (let ((tags)
        (tag-strings (split-string string ";")))
    (dolist (tag-string tag-strings tags)
      (let ((pair (split-string tag-string "=")))
        (push (if (consp pair)
                  pair
                `(,pair))
              tags)))))

;; A benefit of this function being internal is not having to define a
;; separate method just to ensure an `erc-tags-format' value of
;; `legacy' always wins.  A downside is that module code must take
;; care to preserve that promise manually.

(cl-defgeneric erc--parse-message-tags (string)
  "Parse STRING into an alist of (TAG . VALUE) conses.
Expect TAG to be a symbol and VALUE nil or a nonempty string.
Don't split composite raw-input values containing commas;
instead, leave them as a single string."
  (when erc-tags-format
    (unless (or (eq erc-tags-format 'legacy)
                (get 'erc-parse-tags 'erc-v3-warned-p))
      (put 'erc-parse-tags 'erc-v3-warned-p t)
      (display-warning
       'erc
       (concat
        "Legacy ERC tags behavior is currently in effect, but other modules,"
        " including those bundled with ERC, may override this in future"
        " releases.  See `erc-tags-format' for more info.")))
    (erc--parse-tags string)))

(defun erc-parse-server-response (proc string)
  "Parse and act upon a complete line from an IRC server.
PROC is the process (connection) from which STRING was received.
PROCs `process-buffer' is `current-buffer' when this function is called."
  (unless (string= string "") ;; Ignore empty strings
    (save-match-data
      (let* ((tag-list (when (eq (aref string 0) ?@)
                         (substring string 1
                                    (string-search " " string))))
             (msg (make-erc-response :unparsed string :tags
                                     (when tag-list
                                       (erc--parse-message-tags tag-list))))
             (string (if tag-list
                         (substring string (+ 1 (string-search " " string)))
                       string))
             (posn (if (eq (aref string 0) ?:)
                       (string-search " " string)
                     0)))

        (setf (erc-response.sender msg)
              (if (eq posn 0)
                  erc-session-server
                (substring string 1 posn)))

        (setf (erc-response.command msg)
              (let* ((bposn (string-match "[^ \n]" string posn))
                     (eposn (string-search " " string bposn)))
                (setq posn (and eposn
                                (string-match "[^ \n]" string eposn)))
                (substring string bposn eposn)))

        (while (and posn
                    (not (eq (aref string posn) ?:)))
          (push (let* ((bposn posn)
                       (eposn (string-search " " string bposn)))
                  (setq posn (and eposn
                                  (string-match "[^ \n]" string eposn)))
                  (substring string bposn eposn))
                (erc-response.command-args msg)))
        (when posn
      (let ((str (substring string (1+ posn))))
        (push str (erc-response.command-args msg))))

    (setf (erc-response.contents msg)
          (car (erc-response.command-args msg)))

    (setf (erc-response.command-args msg)
          (nreverse (erc-response.command-args msg)))

    (erc-decode-parsed-server-response msg)

    (erc-handle-parsed-server-response proc msg)))))

(defun erc-decode-parsed-server-response (parsed-response)
  "Decode a pre-parsed PARSED-RESPONSE before it can be handled.

If there is a channel name in `erc-response.command-args', decode
`erc-response' according to this channel name and
`erc-encoding-coding-alist', or use `erc-server-coding-system'
for decoding."
  (let ((args (erc-response.command-args parsed-response))
        (decode-target nil)
        (decoded-args ()))
    ;; FIXME this should stop after the first match.
    (dolist (arg args nil)
      (when (string-match "^[#&].*" arg)
        (setq decode-target arg)))
    (when (stringp decode-target)
      ;; FIXME `decode-target' should be passed as TARGET.
      (setq decode-target (erc-decode-string-from-target decode-target nil)))
    (setf (erc-response.unparsed parsed-response)
          (erc-decode-string-from-target
           (erc-response.unparsed parsed-response)
           decode-target))
    (setf (erc-response.sender parsed-response)
          (erc-decode-string-from-target
           (erc-response.sender parsed-response)
           decode-target))
    (setf (erc-response.command parsed-response)
          (erc-decode-string-from-target
           (erc-response.command parsed-response)
           decode-target))
    (dolist (arg (nreverse args) nil)
      (push (erc-decode-string-from-target arg decode-target)
            decoded-args))
    (setf (erc-response.command-args parsed-response) decoded-args)
    (setf (erc-response.contents parsed-response)
          (erc-decode-string-from-target
           (erc-response.contents parsed-response)
           decode-target))))

(defun erc-handle-parsed-server-response (process parsed-response)
  "Handle a pre-parsed PARSED-RESPONSE from PROCESS.

Hands off to helper functions via `erc-call-hooks'."
  (if (member (erc-response.command parsed-response)
              erc-server-prevent-duplicates)
      (let ((m (erc-response.unparsed parsed-response)))
        ;; duplicate suppression
        (if (time-less-p (or (gethash m erc-server-duplicates) 0)
                         (time-since erc-server-duplicate-timeout))
            (erc-call-hooks process parsed-response))
        (puthash m (erc-current-time) erc-server-duplicates))
    ;; Hand off to the relevant handler.
    (erc-call-hooks process parsed-response)))

(defun erc-get-hook (command)
  "Return the hook variable associated with COMMAND.

See also `erc-server-responses'."
  (gethash (format (if (numberp command) "%03i" "%s") command)
           erc-server-responses))

(defvar erc--parsed-response nil)

(defun erc-call-hooks (process message)
  "Call hooks associated with MESSAGE in PROCESS.

Finds hooks by looking in the `erc-server-responses' hash table."
  (let ((erc--parsed-response message)
        (erc--msg-prop-overrides erc--msg-prop-overrides)
        (hook (or (erc-get-hook (erc-response.command message))
                  'erc-default-server-functions)))
    (run-hook-with-args-until-success hook process message)
    ;; Some handlers, like `erc-cmd-JOIN', open new targets without
    ;; saving excursion, and `erc-open' sets the current buffer.
    (erc-with-server-buffer
      (run-hook-with-args 'erc-timer-hook (erc-current-time)))))

(defun erc-handle-unknown-server-response (proc parsed)
  "Display unknown server response's message."
  (let ((line (concat (erc-response.sender parsed)
                      " "
                      (erc-response.command parsed)
                      " "
                      (mapconcat #'identity (erc-response.command-args parsed)
                                 " "))))
    (erc-display-message parsed 'notice proc line)))


(cl-defmacro define-erc-response-handler ((name &rest aliases)
                                          &optional extra-fn-doc extra-var-doc
                                          &rest fn-body)
  "Define an ERC handler hook/function pair.
NAME is the response name as sent by the server (see the IRC RFC for
meanings).

This creates:
 - a hook variable `erc-server-NAME-functions' initialized to
   `erc-server-NAME'.
 - a function `erc-server-NAME' with body FN-BODY.

\(Note that here, NAME merely refers to the parameter NAME rather than
an actual IRC response or server-sent command.)

If ALIASES is non-nil, each alias in ALIASES is `defalias'ed to
`erc-server-NAME'.
Alias hook variables are created as `erc-server-ALIAS-functions' and
initialized to the same default value as `erc-server-NAME-functions'.

ERC uses FN-BODY as the body of the default response handler
`erc-server-NAME', which handles all incoming IRC \"NAME\" responses,
unless overridden (see below).  ERC calls the function with two
arguments, PROC and PARSED, whose symbols (lowercase) are bound to the
current `erc-server-process' and `erc-response' instance within FN-BODY.
Implementers should take care not to shadow them inadvertently.  In all
cases, FN-BODY should return nil to allow third parties to run code
after `erc-server-NAME' returns.  For historical reasons, ERC does not
currently enforce this, however future versions very well may.

If EXTRA-FN-DOC is non-nil, it is inserted at the beginning of the
defined function's docstring.

If EXTRA-VAR-DOC is non-nil, it is inserted at the beginning of the
defined variable's docstring.

As an example:

  (define-erc-response-handler (311 WHOIS WI)
    \"Some non-generic function documentation.\"
    \"Some non-generic variable documentation.\"
    (do-stuff-with-whois proc parsed))

Would expand to:

  (prog2
      (defvar erc-server-311-functions \\='erc-server-311
        \"Some non-generic variable documentation.

  Hook called upon receiving a 311 server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.
  See also `erc-server-311'.\")

      (defun erc-server-311 (proc parsed)
        \"Some non-generic function documentation.

  Handler for a 311 server response.
  PROC is the server process which returned the response.
  PARSED is the actual response as an `erc-response' struct.
  If you want to add responses don't modify this function, but rather
  add things to `erc-server-311-functions' instead.\"
        (do-stuff-with-whois proc parsed))

    (puthash \"311\" \\='erc-server-311-functions erc-server-responses)
    (puthash \"WHOIS\" \\='erc-server-WHOIS-functions erc-server-responses)
    (puthash \"WI\" \\='erc-server-WI-functions erc-server-responses)

    (defalias \\='erc-server-WHOIS \\='erc-server-311)
    (defvar erc-server-WHOIS-functions \\='erc-server-311
      \"Some non-generic variable documentation.

  Hook called upon receiving a WHOIS server response.

  Each function is called with two arguments, the process associated
  with the response and the parsed response.  If the function returns
  non-nil, stop processing the hook.  Otherwise, continue.

  See also `erc-server-311'.\")

    (defalias \\='erc-server-WI \\='erc-server-311)
    (defvar erc-server-WI-functions \\='erc-server-311
      \"Some non-generic variable documentation.

  Hook called upon receiving a WI server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.  If the function returns
  non-nil, stop processing the hook.  Otherwise, continue.

  See also `erc-server-311'.\"))

  Note that while all ALIASES share the same handler function, each gets
  its own distinct hook variable.  The default value of these variables
  may be a list or a function.  Robust code should handle both.

\(fn (NAME &rest ALIASES) &optional EXTRA-FN-DOC EXTRA-VAR-DOC &rest FN-BODY)"
  (declare (debug (&define [&name "erc-response-handler@"
                                  ;; No `def-edebug-elem-spec' in 27.
                                  ([&or integerp symbolp]
                                   &rest [&or integerp symbolp])]
                           &optional sexp sexp def-body))
           (doc-string 2)
           (indent defun))
  (if (numberp name) (setq name (intern (format "%03i" name))))
  (setq aliases (mapcar (lambda (a)
                          (if (numberp a)
                              (format "%03i" a)
                            a))
                        aliases))
  (let* ((hook-name (intern (format "erc-server-%s-functions" name)))
         (fn-name (intern (format "erc-server-%s" name)))
         (hook-doc (format "\
%sHook called upon receiving a %%s server response.
Each function is called with two arguments, the process associated
with the response and the parsed response.  If the function returns
non-nil, stop processing the hook.  Otherwise, continue.

See also `%s'."
                           (if extra-var-doc
                               (concat extra-var-doc "\n\n")
                             "")
                           fn-name))
         (fn-doc (format "\
%sHandler for a %s server response.
PROC is the server process which returned the response.
PARSED is the actual response as an `erc-response' struct.
If you want to add responses don't modify this function, but rather
add things to `%s' instead."
                         (if extra-fn-doc
                             (concat extra-fn-doc "\n\n")
                           "")
                         name hook-name))
         (fn-alternates
          (cl-loop for alias in aliases
                   collect (intern (format "erc-server-%s" alias))))
         (var-alternates
          (cl-loop for alias in aliases
                   collect (intern (format "erc-server-%s-functions" alias)))))
    `(prog2
         ;; Normal hook variable.  The variable may already have a
         ;; value at this point, so I default to nil, and (add-hook)
         ;; unconditionally
         (defvar ,hook-name nil ,(format hook-doc name))
         (add-hook ',hook-name #',fn-name)
         ;; Handler function
         (defun ,fn-name (proc parsed)
           ,fn-doc
           (ignore proc parsed)
           ,@fn-body)

       ;; Make find-function and find-variable find them
       (put ',fn-name 'definition-name ',name)
       (put ',hook-name 'definition-name ',name)

       ;; Hash table map of responses to hook variables
       ,@(cl-loop for response in (cons name aliases)
                  for var in (cons hook-name var-alternates)
                  collect `(puthash ,(format "%s" response) ',var
                                    erc-server-responses))
       ;; Alternates.
       ;; Functions are defaliased, hook variables are defvared so we
       ;; can add hooks to one alias, but not another.
       ,@(cl-loop for fn in fn-alternates
                  for var in var-alternates
                  for a in aliases
                  nconc (list `(defalias ',fn #',fn-name)
                              `(defvar ,var #',fn-name ,(format hook-doc a))
                              `(put ',var 'definition-name ',hook-name))))))

(define-erc-response-handler (ERROR)
  "Handle an ERROR command from the server." nil
  (setq erc-server-error-occurred t)
  (erc-display-message
   parsed 'error nil 'ERROR
   ?s (erc-response.sender parsed) ?c (erc-response.contents parsed)))

(define-erc-response-handler (INVITE)
  "Handle invitation messages."
  nil
  (let ((target (car (erc-response.command-args parsed)))
        (chnl (erc-response.contents parsed)))
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      (defvar erc-invitation)
      (setq erc-invitation chnl)
      (when (string= target (erc-current-nick))
        (erc-display-message
         parsed 'notice 'active
         'INVITE ?n nick ?u login ?h host ?c chnl)))))

(cl-defmethod erc--server-determine-join-display-context (_channel alist)
  "Determine `erc--display-context' for JOINs."
  (if (assq 'erc-buffer-display alist)
      alist
    `((erc-buffer-display . JOIN) ,@alist)))

(define-erc-response-handler (JOIN)
  "Handle join messages."
  nil
  (let ((chnl (erc-response.contents parsed))
        (buffer nil))
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      ;; strip the stupid combined JOIN facility (IRC 2.9)
      (if (string-match "^\\(.*\\)\^g.*$" chnl)
          (setq chnl (match-string 1 chnl)))
      (save-excursion
        (let ((args (cond
                     ;; If I have joined a channel
                     ((erc-current-nick-p nick)
                      (let ((erc--display-context
                             (erc--server-determine-join-display-context
                              chnl erc--display-context)))
                        (setq buffer (erc--open-target chnl)))
                      (when buffer
                        (set-buffer buffer)
                        (with-suppressed-warnings
                            ((obsolete erc-add-default-channel))
                          (erc-add-default-channel chnl))
                        (setf (erc--target-channel-joined-p erc--target) t)
                        (erc-server-send (format "MODE %s" chnl)))
                      (erc-with-buffer (chnl proc)
                        (erc-channel-begin-receiving-names))
                      (erc-update-mode-line)
                      (run-hooks 'erc-join-hook)
                      (list 'JOIN-you ?c chnl))
                     (t
                      (setq buffer (erc-get-buffer chnl proc))
                      (list 'JOIN ?n nick ?u login ?h host ?c chnl)))))
          (when buffer (set-buffer buffer))
          (erc-update-channel-member chnl nick nick t nil nil nil nil nil host login)
          (unless (erc-current-nick-p nick)
            (erc--ensure-query-member nick))
          ;; on join, we want to stay in the new channel buffer
          ;;(set-buffer ob)
          (apply #'erc-display-message parsed 'notice buffer args))))))

(define-erc-response-handler (KICK)
  "Handle kick messages received from the server." nil
  (let* ((ch (nth 0 (erc-response.command-args parsed)))
         (tgt (nth 1 (erc-response.command-args parsed)))
         (reason (erc-trim-string (erc-response.contents parsed)))
         (buffer (erc-get-buffer ch proc)))
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      (cond
       ((string= tgt (erc-current-nick))
        (erc-display-message
         parsed 'notice buffer
         'KICK-you ?n nick ?u login ?h host ?c ch ?r reason)
        (run-hook-with-args 'erc-kick-hook buffer)
        (erc-with-buffer
            (buffer)
          (erc--remove-channel-users-but tgt))
        (with-suppressed-warnings ((obsolete erc-delete-default-channel))
          (erc-delete-default-channel ch buffer))
        (erc-update-mode-line buffer))
       ((string= nick (erc-current-nick))
        (erc-display-message
         parsed 'notice buffer
         'KICK-by-you ?k tgt ?c ch ?r reason)
        (erc-remove-channel-member buffer tgt))
       (t (erc-display-message
           parsed 'notice buffer
           'KICK ?k tgt ?n nick ?u login ?h host ?c ch ?r reason)
          (erc-remove-channel-member buffer tgt)))))
  nil)

(define-erc-response-handler (MODE)
  "Handle server mode changes." nil
  (let ((tgt (car (erc-response.command-args parsed)))
        (mode (mapconcat #'identity (cdr (erc-response.command-args parsed))
                         " ")))
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      (erc-log (format "MODE: %s -> %s: %s" nick tgt mode))
      ;; dirty hack
      (let ((buf (cond ((erc-channel-p tgt)
                        (erc-get-buffer tgt proc))
                       ((string= tgt (erc-current-nick)) nil)
                       ((erc-active-buffer) (erc-active-buffer))
                       (t (erc-get-buffer tgt)))))
        (with-current-buffer (or buf
                                 (current-buffer))
          (erc--update-modes (cdr (erc-response.command-args parsed))))
          (if (or (string= login "") (string= host ""))
              (erc-display-message parsed 'notice buf
                                   'MODE-nick ?n nick
                                   ?t tgt ?m mode)
            (erc-display-message parsed 'notice buf
                                 'MODE ?n nick ?u login
                                 ?h host ?t tgt ?m mode)))))
  nil)

(defun erc--wrangle-query-buffers-on-nick-change (old new)
  "Create or reuse a query buffer for NEW nick after considering OLD nick.
Return a list of buffers in which to announce the change."
  ;; Note that `new-buffer' may be older than `old-buffer', e.g., if
  ;; the query target is switching to a previously used nick.
  (let ((new-buffer (erc-get-buffer new erc-server-process))
        (old-buffer (erc-get-buffer old erc-server-process))
        (selfp (erc-current-nick-p old)) ; e.g., for note taking, etc.
        buffers)
    (when new-buffer
      (push new-buffer buffers))
    (when old-buffer
      (push old-buffer buffers)
      ;; Ensure the new nick is absent from the old query.
      (unless selfp
        (erc-remove-channel-member old-buffer old))
      (when (or selfp (null new-buffer))
        (let ((target (erc--target-from-string new))
              (id (erc-networks--id-given erc-networks--id)))
          (with-current-buffer old-buffer
            (setq erc-default-recipients (cons new
                                               (cdr erc-default-recipients))
                  erc--target target))
          (setq new-buffer (erc-get-buffer-create erc-session-server
                                                  erc-session-port
                                                  nil target id)))))
    (when new-buffer
      (with-current-buffer new-buffer
        (erc-update-mode-line)))
    buffers))

(define-erc-response-handler (NICK)
  "Handle nick change messages." nil
  (let ((nn (erc-response.contents parsed))
        bufs)
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      (setq bufs (erc-buffer-list-with-nick nick proc))
      (erc-log (format "NICK: %s -> %s" nick nn))
      ;; if we had a query with this user, make sure future messages will be
      ;; sent to the correct nick. also add to bufs, since the user will want
      ;; to see the nick change in the query, and if it's a newly begun query,
      ;; erc-channel-users won't contain it
      ;;
      ;; Possibly still relevant: bug#12002
      (dolist (buf (erc--wrangle-query-buffers-on-nick-change nick nn))
        (cl-pushnew buf bufs))
      (erc-update-user-nick nick nn host login)
      (cond
       ((string= nick (erc-current-nick))
        (cl-pushnew (erc-server-buffer) bufs)
        ;; Show message in all query buffers.
        (setq bufs (append (erc--query-list) bufs))
        (erc-set-current-nick nn)
        ;; Rename session, possibly rename server buf and all targets
        (when erc-server-connected
          (erc-networks--id-reload erc-networks--id proc parsed))
        (erc-update-mode-line)
        (setq erc-nick-change-attempt-count 0)
        (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
        (erc-display-message
         parsed 'notice bufs
         'NICK-you ?n nick ?N nn)
        (run-hook-with-args 'erc-nick-changed-functions nn nick))
       (t
        (when erc-server-connected
          (erc-networks--id-reload erc-networks--id proc parsed)
          (erc--ensure-query-member nn))
        (erc-handle-user-status-change 'nick (list nick login host) (list nn))
        (erc-display-message parsed 'notice bufs 'NICK ?n nick
                             ?u login ?h host ?N nn))))))

(define-erc-response-handler (PART)
  "Handle part messages." nil
  (let* ((chnl (car (erc-response.command-args parsed)))
         (reason (erc-trim-string (erc-response.contents parsed)))
         (buffer (erc-get-buffer chnl proc)))
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      ;; When `buffer' is nil, `erc-remove-channel-member' and
      ;; `erc-remove-channel-users' do almost nothing, and the message
      ;; is displayed in the server buffer.
      (erc-display-message parsed 'notice buffer
                           'PART ?n nick ?u login
                           ?h host ?c chnl ?r (or reason ""))
      (cond
       ((string= nick (erc-current-nick))
        (run-hook-with-args 'erc-part-hook buffer)
        (erc-with-buffer
            (buffer)
          (erc--remove-channel-users-but nick))
        (with-suppressed-warnings ((obsolete erc-delete-default-channel))
          (erc-delete-default-channel chnl buffer))
        (erc-update-mode-line buffer)
        (defvar erc-kill-buffer-on-part)
        (when (and erc-kill-buffer-on-part buffer)
          (defvar erc-killing-buffer-on-part-p)
          (let ((erc-killing-buffer-on-part-p t))
            (kill-buffer buffer))))
       (t (erc-remove-channel-member buffer nick)))))
  nil)

(define-erc-response-handler (PING)
  "Handle ping messages." nil
  (let ((pinger (car (erc-response.command-args parsed))))
    (erc-log (format "PING: %s" pinger))
    ;; ping response to the server MUST be forced, or you can lose big
    (erc-server-send (format "PONG :%s" pinger) 'no-penalty)
    (when erc-verbose-server-ping
      (erc-display-message
       parsed 'error proc
       'PING ?s (erc-time-diff erc-server-last-ping-time (erc-current-time))))
    (setq erc-server-last-ping-time (erc-current-time)))
  nil)

(define-erc-response-handler (PONG)
  "Handle pong messages." nil
  (let ((time (string-to-number (erc-response.contents parsed))))
    (when (> time 0)
      (setq erc-server-lag (erc-time-diff time nil))
      (when erc-verbose-server-ping
        (erc-display-message
         parsed 'notice proc 'PONG
         ?h (car (erc-response.command-args parsed)) ?i erc-server-lag
         ?s (if (/= erc-server-lag 1) "s" "")))
      (erc-update-mode-line))))

(defun erc--statusmsg-target (target)
  "Return actual target from given TARGET if it has a leading prefix char."
  (and-let* ((erc-ensure-target-buffer-on-privmsg)
             ((not (eq erc-ensure-target-buffer-on-privmsg 'status)))
             ((not (erc-channel-p target)))
             (chars (erc--get-isupport-entry 'STATUSMSG 'single))
             ((string-search (string (aref target 0)) chars))
             (trimmed (substring target 1))
             ((erc-channel-p trimmed)))
    trimmed))

;; Moved to this file from erc.el in ERC 5.6.
(defvar-local erc-current-message-catalog 'english
  "Current language or context catalog for formatting inserted messages.
See `erc-format-message'.")

;; This variable can be made public if the current design proves
;; sufficient.
(defvar erc--message-speaker-catalog '-speaker
  "The \"speaker\" catalog symbol used to format PRIVMSGs and NOTICEs.

This symbol defines a \"catalog\" of variables and functions
whose names reflect their membership via a corresponding CATALOG
component, as in \"erc-message-CATALOG-KEY\".  Here, KEY refers
to a common set of interface members (variables or functions),
that an implementer must define:

- `statusmsg' and `statusmsg-input': PRIVMSGs whose target is a
   status-prefixed channel; the latter is the \"echoed\" version

- `chan-privmsg', `query-privmsg', `chan-notice', `query-notice':
   standard chat messages traditionally prefixed by a <nickname>
   indicating the message's \"speaker\"

- `input-chan-privmsg', `input-query-privmsg', `input-query-notice',
  `input-chan-notice': \"echoed\" versions of the above

- `ctcp-action', `ctcp-action-input', `ctcp-action-statusmsg',
  `ctcp-action-statusmsg-input': \"CTCP ACTION\" versions of the
   above

The other part of this interface is the per-key collection of
`format-spec' parameters members must support.  For simplicity,
this catalog currently defines a common set for all keys, some of
which may be assigned the empty string when not applicable:

  %n - nickname
  %m - message body
  %p - nickname's status prefix (when applicable)
  %s - current target's STATUSMSG prefix (when applicable)

As an added means of communicating with various modules, if this
catalog's symbol has the property `erc--msg-prop-overrides',
consumers calling `erc-display-message' will see the value added
to the `erc--msg-props' \"environment\" in modification hooks,
like `erc-insert-modify-hook'.")

(defvar erc--speaker-status-prefix-wanted-p (gensym "erc-")
  "Sentinel to detect whether `erc-format-@nick' has just run.")

(define-erc-response-handler (PRIVMSG NOTICE)
  "Handle private messages, including messages in channels." nil
  (let ((sender-spec (erc-response.sender parsed))
        (cmd (erc-response.command parsed))
        (tgt (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (defvar erc-minibuffer-ignored)
    (defvar erc-ignore-list)
    (defvar erc-ignore-reply-list)
    (if (or (and erc-ignore-list (erc-ignored-user-p sender-spec))
            (and erc-ignore-reply-list (erc-ignored-reply-p msg tgt proc)))
        (when erc-minibuffer-ignored
          (message "Ignored %s from %s to %s" cmd sender-spec tgt))
      (let* ((sndr (erc-parse-user sender-spec))
             (nick (nth 0 sndr))
             (login (nth 1 sndr))
             (host (nth 2 sndr))
             (msgp (string= cmd "PRIVMSG"))
             (noticep (string= cmd "NOTICE"))
             ;; S.B. downcase *both* tgt and current nick
             (medown (erc-downcase (erc-current-nick)))
             (inputp (string= medown (erc-downcase nick)))
             (privp (string= (erc-downcase tgt) medown))
             (erc--display-context `((erc-buffer-display . ,(intern cmd))
                                     ,@erc--display-context))
             (erc--msg-prop-overrides `((erc--tmp) ,@erc--msg-prop-overrides))
             (erc--speaker-status-prefix-wanted-p nil)
             (erc-current-message-catalog erc--message-speaker-catalog)
             ;;
             finalize buffer statusmsg cmem-prefix fnick)
        (setq buffer (erc-get-buffer (if privp nick tgt) proc))
        ;; Even worth checking for empty target here? (invalid anyway)
        (unless (or buffer noticep (string-empty-p tgt) (eq ?$ (aref tgt 0))
                    (erc-is-message-ctcp-and-not-action-p msg))
          (defvar erc-receive-query-display)
          (defvar erc-receive-query-display-defer)
          (if privp
              (when-let* ((erc-join-buffer
                           (or (and (not erc-receive-query-display-defer)
                                    erc-receive-query-display)
                               (and erc-ensure-target-buffer-on-privmsg
                                    (or erc-receive-query-display
                                        erc-join-buffer)))))
                (push `(erc-receive-query-display . ,(intern cmd))
                      erc--display-context)
                (setq buffer (erc--open-target nick)))
            (cond
             ;; Target is a channel and contains leading @+ chars.
             ((and-let* ((trimmed(erc--statusmsg-target tgt)))
                (setq buffer (erc-get-buffer trimmed proc)
                      statusmsg (and buffer (substring tgt 0 1)))))
             ;; A channel buffer has been killed but is still joined.
             (erc-ensure-target-buffer-on-privmsg
              (setq buffer (erc--open-target tgt))))))
        (when buffer
          (with-current-buffer buffer
            (when privp
              (erc--unhide-prompt)
              ;; Remove untracked query partners after display.
              (defvar erc--decouple-query-and-channel-membership-p)
              (unless (or erc--decouple-query-and-channel-membership-p
                          (erc--get-server-user nick))
                (setq finalize (lambda ()
                                 (erc-remove-channel-member buffer nick)))))
            (erc-update-channel-member (if privp nick tgt) nick nick
                                       privp nil nil nil nil nil host login nil nil t)
            (defvar erc--cmem-from-nick-function)
            (defvar erc-format-nick-function)
            (defvar erc-show-speaker-membership-status)
            (defvar erc-speaker-from-channel-member-function)
            (let ((cdata (funcall erc--cmem-from-nick-function
                                  (erc-downcase nick) sndr parsed)))
              (setq fnick (funcall erc-speaker-from-channel-member-function
                                   (car cdata) (cdr cdata))
                    cmem-prefix (and (or erc--speaker-status-prefix-wanted-p
                                         erc-show-speaker-membership-status
                                         inputp)
                                     (cdr cdata))))))
        (if (erc-is-message-ctcp-p msg)
            (if noticep
                (erc-process-ctcp-reply proc parsed nick login host
                                        (match-string 1 msg))
              (setq parsed (erc--ctcp-response-from-parsed
                            :parsed parsed :buffer buffer :statusmsg statusmsg
                            :prefix cmem-prefix :dispname fnick))
              (erc-process-ctcp-query proc parsed nick login host))
          (setq erc-server-last-peers (cons nick (cdr erc-server-last-peers)))
          (with-current-buffer (or buffer (current-buffer))
            ;; Re-bind in case either buffer has a local value.
            (let ((erc-current-message-catalog erc--message-speaker-catalog)
                  (msg-args (erc--determine-speaker-message-format-args
                             nick msg privp msgp inputp statusmsg
                             cmem-prefix fnick)))
              (if (or msgp (not privp))
                  ;; This is a PRIVMSG or a NOTICE to a channel.
                  (apply #'erc-display-message parsed nil buffer msg-args)
                ;; This is a NOTICE directed at the client's current nick.
                (push (cons 'erc--msg (car msg-args)) erc--msg-prop-overrides)
                (let ((fmtmsg (apply #'erc-format-message msg-args)))
                  (run-hook-with-args 'erc-echo-notice-always-hook
                                      fmtmsg parsed buffer nick)
                  (run-hook-with-args-until-success
                   'erc-echo-notice-hook fmtmsg parsed buffer nick)))))
          (when finalize (funcall finalize)))
        nil))))

(define-erc-response-handler (QUIT)
  "Another user has quit IRC." nil
  (let ((reason (erc-response.contents parsed))
        (erc--msg-prop-overrides erc--msg-prop-overrides)
        bufs)
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      (setq bufs (erc-buffer-list-with-nick nick proc))
      (when (erc-current-nick-p nick)
        (setq bufs (append (erc--query-list) bufs))
        (push '(erc--skip . (track)) erc--msg-prop-overrides))
      (setq reason (erc-wash-quit-reason reason nick login host))
      (erc-display-message parsed 'notice bufs
                           'QUIT ?n nick ?u login
                           ?h host ?r reason)
      (erc-remove-user nick)))
  nil)

(define-erc-response-handler (TOPIC)
  "The channel topic has changed." nil
  (let* ((ch (car (erc-response.command-args parsed)))
         (topic (erc-trim-string (erc-response.contents parsed)))
         (time (format-time-string erc-server-timestamp-format)))
    (pcase-let ((`(,nick ,login ,host)
                 (erc-parse-user (erc-response.sender parsed))))
      (erc-update-channel-member ch nick nick nil nil nil nil nil nil host login)
      (erc-update-channel-topic ch (format "%s\C-o (%s, %s)" topic nick time))
      (erc-display-message parsed 'notice (erc-get-buffer ch proc)
                           'TOPIC ?n nick ?u login ?h host
                           ?c ch ?T topic))))

(define-erc-response-handler (WALLOPS)
  "Display a WALLOPS message." nil
  (let ((message (erc-response.contents parsed)))
    (pcase-let ((`(,nick ,_login ,_host)
                 (erc-parse-user (erc-response.sender parsed))))
      (erc-display-message
       parsed 'notice nil
       'WALLOPS ?n nick ?m message))))

(define-erc-response-handler (001)
  "Set `erc-server-current-nick' to reflect server settings.
Then display the welcome message."
  nil
  (erc-set-current-nick (car (erc-response.command-args parsed)))
  (erc-update-mode-line)                ; needed here?
  (setq erc-nick-change-attempt-count 0)
  (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
  (erc-display-message
   parsed 'notice 'active (erc-response.contents parsed)))

(define-erc-response-handler (MOTD 002 003 371 372 374 375)
  "Display the server's message of the day." nil
  (erc-handle-login)
  (erc-display-message
   parsed 'notice (if erc-server-connected 'active proc)
   (erc-response.contents parsed)))

(define-erc-response-handler (376 422)
  "End of MOTD/MOTD is missing." nil
  (erc-server-MOTD proc parsed)
  (erc-connection-established proc parsed))

(define-erc-response-handler (004)
  "Display the server's identification." nil
  (pcase-let ((`(,server-name ,server-version)
               (cdr (erc-response.command-args parsed))))
    (setq erc-server-version server-version)
    (setq erc-server-announced-name server-name)
    (erc-update-mode-line-buffer (process-buffer proc))
    (erc-display-message
     parsed 'notice proc
     's004 ?s server-name ?v server-version
     ?U (nth 3 (erc-response.command-args parsed))
     ?C (nth 4 (erc-response.command-args parsed)))))

(defun erc--parse-isupport-value (value)
  "Return list of unescaped components from an \"ISUPPORT\" VALUE."
  ;; https://tools.ietf.org/html/draft-brocklesby-irc-isupport-03#section-2
  ;;
  ;; > The server SHOULD send "X", not "X="; this is the normalized form.
  ;;
  (let (case-fold-search)
    (mapcar
     (lambda (v)
       (let ((start 0)
             m
             c)
         (while (and (< start (length v))
                     (string-match "[\\]x[0-9A-F][0-9A-F]" v start))
           (setq m (substring v (+ 2 (match-beginning 0)) (match-end 0))
                 c (string-to-number m 16))
           ;; In practice, this range is too liberal.  The only
           ;; escapes we're likely to see are ?\\, ?=, and ?\s.
           (if (<= ?\s c ?~)
               (setq v (concat (substring v 0 (match-beginning 0))
                               (string c)
                               (substring v (match-end 0)))
                     start (- (match-end 0) 3))
             (setq start (match-end 0))))
         v))
     (if (string-search "," value)
         (split-string value ",")
       (list value)))))

(defun erc--get-isupport-entry (key &optional single)
  "Return an item for \"ISUPPORT\" token KEY, a symbol.
When a lookup fails return nil.  Otherwise return a list whose
CAR is KEY and whose CDR is zero or more strings.  With SINGLE,
just return the first value, if any.  The latter is potentially
ambiguous and only useful for tokens supporting a single
primitive value."
  (if-let* ((table (or erc--isupport-params
                       (erc-with-server-buffer erc--isupport-params)))
            (value (with-memoization (gethash key table)
                     (when-let* ((v (assoc (symbol-name key)
                                           (or erc-server-parameters
                                               (erc-with-server-buffer
                                                erc-server-parameters)))))
                       (if-let* ((val (cdr v))
                                 ((not (string-empty-p val))))
                           (erc--parse-isupport-value val)
                         '--empty--)))))
      (pcase value
        ('--empty-- (unless single (list key)))
        (`(,head . ,_) (if single head (cons key value))))
    (when table
      (remhash key table))))

;; While it's better to depend on interfaces than specific types,
;; using `cl-struct-slot-value' or similar to extract a known slot at
;; runtime would incur a small "ducktyping" tax, which should probably
;; be avoided when running hundreds of times per incoming message.
;; Instead of separate keys per data type, we could increment a
;; counter whenever a new 005 arrives.
(defmacro erc--with-isupport-data (param var &rest body)
  "Return structured data stored in VAR for \"ISUPPORT\" PARAM.
Expect VAR's value to be an instance of `erc--isupport-data'.  If
VAR is uninitialized or stale, evaluate BODY and assign the
result to VAR."
  (declare (indent defun))
  `(erc-with-server-buffer
     (pcase-let (((,@(list '\` (list param  '\, 'key)))
                  (erc--get-isupport-entry ',param)))
       (or (and ,var (eq key (erc--isupport-data-key ,var)) ,var)
           (setq ,var (progn ,@body))))))

(define-erc-response-handler (005)
  "Set the variable `erc-server-parameters' and display the received message.

According to RFC 2812, suggests alternate servers on the network.
Many servers, however, use this code to show which parameters they have set,
for example, the network identifier, maximum allowed topic length, whether
certain commands are accepted and more.  See documentation for
`erc-server-parameters' for more information on the parameters sent.

A server may send more than one 005 message."
  nil
  (unless erc--isupport-params
    (setq erc--isupport-params (make-hash-table)))
  (let* ((args (cdr (erc-response.command-args parsed)))
         (line (string-join args " ")))
    (while args
      (let ((section (pop args))
            key
            value
            negated)
        (when (string-match
               (rx bot (| (: (group (+ (any "A-Z"))) "=" (group (* nonl)))
                          (: (? (group "-")) (group (+ (any "A-Z")))))
                   eot)
               section)
          (setq key (or (match-string 1 section) (match-string 4 section))
                value (match-string 2 section)
                negated (and (match-string 3 section) '-))
          (setf (alist-get key erc-server-parameters '- 'remove #'equal)
                (or value negated))
          (remhash (intern key) erc--isupport-params))))
    (erc-display-message parsed 'notice proc line)
    nil))

(define-erc-response-handler (221)
  "Display the current user modes." nil
  (let* ((nick (car (erc-response.command-args parsed)))
         (modes (mapconcat #'identity
                           (cdr (erc-response.command-args parsed)) " ")))
    (erc--update-modes (cdr (erc-response.command-args parsed)))
    (erc-display-message parsed 'notice 'active 's221 ?n nick ?m modes)))

(define-erc-response-handler (252)
  "Display the number of IRC operators online." nil
  (erc-display-message parsed 'notice 'active 's252
                       ?i (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (253)
  "Display the number of unknown connections." nil
  (erc-display-message parsed 'notice 'active 's253
                       ?i (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (254)
  "Display the number of channels formed." nil
  (erc-display-message parsed 'notice 'active 's254
                       ?i (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (250 251 255 256 257 258 259 265 266 377 378)
  "Generic display of server messages as notices.

See `erc-display-server-message'." nil
  (erc-display-server-message proc parsed))

(define-erc-response-handler (263) "RPL_TRYAGAIN." nil
  (erc-handle-unknown-server-response proc parsed))

(define-erc-response-handler (275)
  "Display secure connection message." nil
  (pcase-let ((`(,nick ,_user ,_message)
               (cdr (erc-response.command-args parsed))))
    (erc-display-message
     parsed 'notice 'active 's275
     ?n nick
     ?m (mapconcat #'identity (cddr (erc-response.command-args parsed))
                   " "))))

(define-erc-response-handler (290)
  "Handle dancer-ircd CAPAB messages." nil nil)

(define-erc-response-handler (301)
  "AWAY notice." nil
  (erc-display-message parsed 'notice 'active 's301
                       ?n (cadr (erc-response.command-args parsed))
                       ?r (erc-response.contents parsed)))

(define-erc-response-handler (303)
  "ISON reply" nil
  (erc-display-message parsed 'notice 'active 's303
                       ?n (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (305)
  "Return from AWAYness." nil
  (erc-process-away proc nil)
  (erc-display-message parsed 'notice 'active
                       's305 ?m (erc-response.contents parsed)))

(define-erc-response-handler (306)
  "Set AWAYness." nil
  (erc-process-away proc t)
  (erc-display-message parsed 'notice 'active
                       's306 ?m (erc-response.contents parsed)))

(define-erc-response-handler (307)
  "Display nick-identified message." nil
  (pcase-let ((`(,nick ,_user ,_message)
               (cdr (erc-response.command-args parsed))))
    (erc-display-message
     parsed 'notice 'active 's307
     ?n nick
     ?m (mapconcat #'identity (cddr (erc-response.command-args parsed))
                   " "))))

(define-erc-response-handler (311 314)
  "WHOIS/WHOWAS notices." nil
  (let ((fname (erc-response.contents parsed))
        (catalog-entry (intern (format "s%s" (erc-response.command parsed)))))
    (pcase-let ((`(,nick ,user ,host)
                 (cdr (erc-response.command-args parsed))))
      (erc-update-user-nick nick nick host user fname)
      (erc-display-message
       parsed 'notice 'active catalog-entry
       ?n nick ?f fname ?u user ?h host))))

(define-erc-response-handler (312)
  "Server name response in WHOIS." nil
  (pcase-let ((`(,nick ,server-host)
              (cdr (erc-response.command-args parsed))))
    (erc-display-message
     parsed 'notice 'active 's312
     ?n nick ?s server-host ?c (erc-response.contents parsed))))

(define-erc-response-handler (313)
  "IRC Operator response in WHOIS." nil
  (erc-display-message
   parsed 'notice 'active 's313
   ?n (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (315 318 323 369)
  ;; 315 - End of WHO
  ;; 318 - End of WHOIS list
  ;; 323 - End of channel LIST
  ;; 369 - End of WHOWAS
  "End of WHO/WHOIS/LIST/WHOWAS notices." nil
  (ignore proc parsed))

(define-erc-response-handler (317)
  "IDLE notice." nil
  (pcase-let ((`(,nick ,seconds-idle ,on-since ,time)
               (cdr (erc-response.command-args parsed))))
    (setq time (when on-since
                 (format-time-string erc-server-timestamp-format
                                     (string-to-number on-since))))
    (erc-update-user-nick nick nick nil nil nil
                          (and time (format "on since %s" time)))
    (if time
        (erc-display-message
         parsed 'notice 'active 's317-on-since
         ?n nick ?i (erc-sec-to-time (string-to-number seconds-idle)) ?t time)
      (erc-display-message
       parsed 'notice 'active 's317
       ?n nick ?i (erc-sec-to-time (string-to-number seconds-idle))))))

(define-erc-response-handler (319)
  "Channel names in WHOIS response." nil
  (erc-display-message
   parsed 'notice 'active 's319
   ?n (cadr (erc-response.command-args parsed))
   ?c (erc-response.contents parsed)))

(define-erc-response-handler (320)
  "Identified user in WHOIS." nil
  (erc-display-message
   parsed 'notice 'active 's320
   ?n (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (321)
  "LIST header." nil
  (setq erc-channel-list nil))

(defun erc-server-321-message (proc parsed)
  "Display a message for the 321 event."
  (erc-display-message parsed 'notice proc 's321)
  nil)
(add-hook 'erc-server-321-functions #'erc-server-321-message t)

(define-erc-response-handler (322)
  "LIST notice." nil
  (let ((topic (erc-response.contents parsed)))
    (pcase-let ((`(,channel ,_num-users)
                 (cdr (erc-response.command-args parsed))))
      (add-to-list 'erc-channel-list (list channel))
      (erc-update-channel-topic channel topic))))

(defun erc-server-322-message (proc parsed)
  "Display a message for the 322 event."
  (let ((topic (erc-response.contents parsed)))
    (pcase-let ((`(,channel ,num-users)
                 (cdr (erc-response.command-args parsed))))
      (erc-display-message
       parsed 'notice proc 's322
       ?c channel ?u num-users ?t (or topic "")))))
(add-hook 'erc-server-322-functions #'erc-server-322-message t)

(define-erc-response-handler (324)
  "Channel or nick modes." nil
  (let ((channel (cadr (erc-response.command-args parsed)))
        (modes (mapconcat #'identity (cddr (erc-response.command-args parsed))
                          " ")))
    (erc--init-channel-modes channel (cddr (erc-response.command-args parsed)))
    (erc-display-message
     parsed 'notice (erc-get-buffer channel proc)
     's324 ?c channel ?m modes)))

(define-erc-response-handler (328)
  "Channel URL." nil
  (let ((channel (cadr (erc-response.command-args parsed)))
        (url (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's328 ?c channel ?u url)))

(define-erc-response-handler (329)
  "Channel creation date." nil
  (let ((channel (cadr (erc-response.command-args parsed)))
        (time (string-to-number
               (nth 2 (erc-response.command-args parsed)))))
    (erc-display-message
     parsed 'notice (erc-get-buffer channel proc)
     's329 ?c channel ?t (format-time-string erc-server-timestamp-format
                                             time))))

(define-erc-response-handler (330)
  "Nick is authed as (on Quakenet network)." nil
  ;; FIXME: I don't know what the magic numbers mean.  Mummy, make
  ;; the magic numbers go away.
  ;; No seriously, I have no clue about the format of this command,
  ;; and don't sit on Quakenet, so can't test.  Originally we had:
  ;; nick == (aref parsed 3)
  ;; authaccount == (aref parsed 4)
  ;; authmsg == (aref parsed 5)
  ;; The guesses below are, well, just that. -- Lawrence 2004/05/10
  (let ((nick (cadr (erc-response.command-args parsed)))
        (authaccount (nth 2 (erc-response.command-args parsed)))
        (authmsg (erc-response.contents parsed)))
    (erc-display-message parsed 'notice 'active 's330
                         ?n nick ?a authmsg ?i authaccount)))

(define-erc-response-handler (331)
  "No topic set for channel." nil
  (let ((channel (cadr (erc-response.command-args parsed))))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's331 ?c channel)))

(define-erc-response-handler (332)
  "TOPIC notice." nil
  (let ((channel (cadr (erc-response.command-args parsed)))
        (topic (erc-response.contents parsed)))
    (erc-update-channel-topic channel topic)
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's332 ?c channel ?T topic)))

(define-erc-response-handler (333)
  "Who set the topic, and when." nil
  (pcase-let ((`(,channel ,nick ,time)
               (cdr (erc-response.command-args parsed))))
    (setq time (format-time-string erc-server-timestamp-format
                                   (string-to-number time)))
    (erc-update-channel-topic channel
                              (format "\C-o (%s, %s)" nick time)
                              'append)
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's333 ?c channel ?n nick ?t time)))

(define-erc-response-handler (341)
  "Let user know when an INVITE attempt has been sent successfully."
  nil
  (pcase-let ((`(,nick ,channel)
               (cdr (erc-response.command-args parsed))))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's341 ?n nick ?c channel)))

(defun erc--extract-352-full-name (contents)
  "Return full name from 352 trailing param, discarding hop count."
  (pcase contents
    ((rx (: bot (+ (any "0-9")) " ") (let full-name (group (* nonl))) eot)
     full-name)
    (_ contents)))

(define-erc-response-handler (352)
  "RPL_WHOREPLY response." nil
  (pcase-let*
      ((`(,_ ,channel ,user ,host ,_server ,nick ,flags, hop-real)
        (erc-response.command-args parsed))
       (full-name (erc--extract-352-full-name hop-real))
       (selfp (string= channel "*"))
       (template (if selfp 's352-you 's352)))
    (if selfp
        (erc-update-user-nick nick nick host user full-name)
      (erc-update-channel-member channel nick nick nil nil nil nil nil nil
                                 host user full-name))
    (erc-display-message parsed 'notice 'active template
                         ?c channel ?n nick ?a flags
                         ?u user ?h host ?f full-name)))

(define-erc-response-handler (353)
  "NAMES notice." nil
  (let ((channel (nth 2 (erc-response.command-args parsed)))
        (users (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (or (erc-get-buffer channel proc)
                                            'active)
                         's353 ?c channel ?u users)
    (erc-with-buffer (channel proc)
      (erc-channel-receive-names users))))

(define-erc-response-handler (366)
  "End of NAMES." nil
  (erc-with-buffer ((cadr (erc-response.command-args parsed)) proc)
    (erc-channel-end-receiving-names))
  (erc--ensure-query-members)
  nil)

(define-erc-response-handler (367)
  "Channel ban list entries." nil
  (pcase-let ((`(,channel ,banmask ,setter ,time)
               (cdr (erc-response.command-args parsed))))
    ;; setter and time are not standard
    (if setter
        (erc-display-message parsed 'notice 'active 's367-set-by
                             ?c channel
                             ?b banmask
                             ?s setter
                             ?t (or time ""))
      (erc-display-message parsed 'notice 'active 's367
                           ?c channel
                           ?b banmask))))

(define-erc-response-handler (368)
  "End of channel ban list." nil
  (let ((channel (cadr (erc-response.command-args parsed))))
    (erc-display-message parsed 'notice 'active 's368
                         ?c channel)))

(define-erc-response-handler (379)
  "Forwarding to another channel." nil
  ;; FIXME: Yet more magic numbers in original code, I'm guessing this
  ;; command takes two arguments, and doesn't have any "contents". --
  ;; Lawrence 2004/05/10
  (pcase-let ((`(,from ,to)
               (cdr (erc-response.command-args parsed))))
    (erc-display-message parsed 'notice 'active
                         's379 ?c from ?f to)))

(define-erc-response-handler (391)
  "Server's time string." nil
  (erc-display-message
   parsed 'notice 'active
   's391 ?s (cadr (erc-response.command-args parsed))
   ?t (nth 2 (erc-response.command-args parsed))))

;; https://defs.ircdocs.horse/defs/numerics.html#rpl-visiblehost-396
;; As of ERC 5.6, if the client hasn't yet joined any channels,
;; there's a good chance a server user for the current nick simply
;; doesn't exist (and there's not enough info in this reply to create
;; one).  To fix this, ERC could WHO itself on 372 or similar if it
;; hasn't yet received a 900.
(define-erc-response-handler (396)
  "RPL_VISIBLEHOST or RPL_YOURDISPLAYHOST or RPL_HOSTHIDDEN." nil
  (pcase-let* ((userhost (cadr (erc-response.command-args parsed)))
               ;; Behavior blindly copied from event_hosthidden in irssi 1.4.
               (rejectrx (rx (| (: bot (in ?@ ?: ?-)) (in ?* ?? ?! ?# ?& ?\s)
                                (: ?- eot))))
               (`(,_ ,user ,host) (and (not (string-match rejectrx userhost))
                                       (erc--parse-nuh userhost))))
    (when host
      (erc-update-user-nick (erc-current-nick) nil host user)
      (erc-display-message parsed 'notice 'active 's396 ?s userhost))))

(define-erc-response-handler (401)
  "No such nick/channel." nil
  (let ((nick/channel (cadr (erc-response.command-args parsed))))
    (defvar erc-whowas-on-nosuchnick)
    (when erc-whowas-on-nosuchnick
      (erc-log (format "cmd: WHOWAS: %s" nick/channel))
      (erc-server-send (format "WHOWAS %s 1" nick/channel)))
    (erc-display-message parsed '(notice error) 'active
                         's401 ?n nick/channel)
    (unless (erc-channel-p nick/channel)
      (erc-remove-user nick/channel))))

(define-erc-response-handler (402)
  "No such server." nil
  (erc-display-message parsed '(notice error) 'active
                       's402 ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (403)
  "No such channel." nil
  (erc-display-message parsed '(notice error) 'active
                       's403 ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (404)
  "Cannot send to channel." nil
  (erc-display-message parsed '(notice error) 'active
                       's404 ?c (cadr (erc-response.command-args parsed))))


(define-erc-response-handler (405)
  "Can't join that many channels." nil
  (erc-display-message parsed '(notice error) 'active
                       's405 ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (406)
  "No such nick." nil
  (erc-display-message parsed '(notice error) 'active
                       's406 ?n (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (412)
  "No text to send." nil
  (erc-display-message parsed '(notice error) 'active 's412))

(define-erc-response-handler (421)
  "Unknown command." nil
  (erc-display-message parsed '(notice error) 'active 's421
                       ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (432)
  "Bad nick." nil
  (erc-display-message parsed '(notice error) 'active 's432
                       ?n (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (433)
  "Login-time \"nick in use\"." nil
  (when erc-server-connected
    (erc-networks--id-reload erc-networks--id proc parsed))
  (erc-nickname-in-use (cadr (erc-response.command-args parsed))
                       "already in use"))

(define-erc-response-handler (437)
  "Nick temporarily unavailable (on IRCnet)." nil
  (let ((nick/channel (cadr (erc-response.command-args parsed))))
    (unless (erc-channel-p nick/channel)
      (erc-nickname-in-use nick/channel "temporarily unavailable"))))

(define-erc-response-handler (442)
  "Not on channel." nil
  (erc-display-message parsed '(notice error) 'active 's442
                       ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (461)
  "Not enough parameters for command." nil
  (erc-display-message parsed '(notice error)  'active 's461
                       ?c (cadr (erc-response.command-args parsed))
                       ?m (erc-response.contents parsed)))

(define-erc-response-handler (465)
  "You are banned from this server." nil
  (setq erc-server-banned t)
  ;; show the server's message, as a reason might be provided
  (erc-display-error-notice
   parsed
   (erc-response.contents parsed)))

(define-erc-response-handler (471)
  "ERR_CHANNELISFULL: channel full." nil
  (erc-display-message parsed '(notice error) nil 's471
                       ?c (cadr (erc-response.command-args parsed))
                       ?s (erc-response.contents parsed)))

(define-erc-response-handler (473)
  "ERR_INVITEONLYCHAN: channel invitation only." nil
  (erc-display-message parsed '(notice error) nil 's473
                       ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (474)
  "Banned from channel errors." nil
  (erc-display-message parsed '(notice error) nil
                       (intern (format "s%s"
                                       (erc-response.command parsed)))
                       ?c (cadr (erc-response.command-args parsed))))

(define-erc-response-handler (475)
  "Channel key needed." nil
  (erc-display-message parsed '(notice error) nil 's475
                       ?c (cadr (erc-response.command-args parsed)))
  (defvar erc-prompt-for-channel-key)
  (defvar erc--called-as-input-p)
  (when erc-prompt-for-channel-key
    (let ((channel (cadr (erc-response.command-args parsed)))
          (erc--called-as-input-p t)
          (key (read-from-minibuffer
                (format "Channel %s is mode +k.  Enter key (RET to cancel): "
                        (cadr (erc-response.command-args parsed))))))
      (when (and key (> (length key) 0))
          (erc-cmd-JOIN channel key)))))

(define-erc-response-handler (477)
  "Channel doesn't support modes." nil
  (let ((channel (cadr (erc-response.command-args parsed)))
        (message (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         (format "%s: %s" channel message))))

(define-erc-response-handler (482)
  "You need to be a channel operator to do that." nil
  (let ((channel (cadr (erc-response.command-args parsed)))
        (message (erc-response.contents parsed)))
    (erc-display-message parsed '(notice error) 'active 's482
                         ?c channel ?m message)))

(define-erc-response-handler (671)
  "Secure connection response in WHOIS." nil
  (let ((nick (cadr (erc-response.command-args parsed)))
        (securemsg (erc-response.contents parsed)))
    (erc-display-message parsed 'notice 'active 's671
                         ?n nick ?a securemsg)))

(define-erc-response-handler (900)
  "Handle a \"RPL_LOGGEDIN\" server command.
Some servers don't consider this SASL-specific but rather just an
indication of a server-side state change from logged-out to
logged-in." nil
  ;; Whenever ERC starts caring about user accounts, it should record
  ;; the session as being logged here.
  (erc-display-message parsed 'notice proc (erc-response.contents parsed)))

(define-erc-response-handler (431 445 446 451 462 463 464 481 483 484 485
                                  491 501 502)
  ;; 431 - No nickname given
  ;; 445 - SUMMON has been disabled
  ;; 446 - USERS has been disabled
  ;; 451 - You have not registered
  ;; 462 - Unauthorized command (already registered)
  ;; 463 - Your host isn't among the privileged
  ;; 464 - Password incorrect
  ;; 481 - Need IRCop privileges
  ;; 483 - You can't kill a server!
  ;; 484 - Your connection is restricted!
  ;; 485 - You're not the original channel operator
  ;; 491 - No O-lines for your host
  ;; 501 - Unknown MODE flag
  ;; 502 - Cannot change mode for other users
  "Generic display of server error messages.

See `erc-display-error-notice'." nil
  (erc-display-error-notice
   parsed
   (intern (format "s%s" (erc-response.command parsed)))))

;; FIXME: These are yet to be implemented, they're just stubs for now
;; -- Lawrence 2004/05/12

;; response numbers left here for reference

;; (define-erc-response-handler (323 364 365 381 382 392 393 394 395
;;                               200 201 202 203 204 205 206 208 209 211 212 213
;;                               214 215 216 217 218 219 241 242 243 244 249 261
;;                               262 302 342 351 407 409 411 413 414 415
;;                               423 424 436 441 443 444 467 472 KILL)
;;   nil nil
;;   (ignore proc parsed))

(provide 'erc-backend)

;;; erc-backend.el ends here
