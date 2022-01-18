;;; rcirc.el --- default, simple IRC client          -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2022 Free Software Foundation, Inc.

;; Author: Ryan Yeske <rcyeske@gmail.com>
;; Maintainers: Ryan Yeske <rcyeske@gmail.com>,
;;		Leo Liu <sdl.web@gmail.com>,
;;              Philip Kaludercic <philipk@posteo.net>
;; Keywords: comm

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

;; Internet Relay Chat (IRC) is a form of instant communication over
;; the Internet.  It is mainly designed for group (many-to-many)
;; communication in discussion forums called channels, but also allows
;; one-to-one communication.

;; Rcirc has simple defaults and clear and consistent behavior.
;; Message arrival timestamps, activity notification on the mode line,
;; message filling, nick completion, and keepalive pings are all
;; enabled by default, but can easily be adjusted or turned off.  Each
;; discussion takes place in its own buffer and there is a single
;; server buffer per connection.

;; Open a new irc connection with:
;; M-x irc RET

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'time-date)
(require 'auth-source)
(require 'parse-time)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'rx))

(defconst rcirc-id-string (concat "rcirc on GNU Emacs " emacs-version))

(defgroup rcirc nil
  "Simple IRC client."
  :version "22.1"
  :prefix "rcirc-"
  :link '(custom-manual "(rcirc)")
  :group 'applications)

(defcustom rcirc-server-alist
  (if (gnutls-available-p)
      '(("irc.libera.chat" :channels ("#emacs" "#rcirc")
         :port 6697 :encryption tls))
    '(("irc.libera.chat" :channels ("#emacs" "#rcirc"))))
  "An alist of IRC connections to establish when running `rcirc'.
Each element looks like (SERVER-NAME PARAMETERS).

SERVER-NAME is a string describing the server to connect
to.

The optional PARAMETERS come in pairs PARAMETER VALUE.

The following parameters are recognized:

`:nick'

VALUE must be a string.  If absent, `rcirc-default-nick' is used
for this connection.

`:port'

VALUE must be a number or string.  If absent,
`rcirc-default-port' is used.

`:user-name'

VALUE must be a string.  If absent, `rcirc-default-user-name' is
used.

`:password'

VALUE must be a string.  If absent, no PASS command will be sent
to the server.

`:full-name'

VALUE must be a string.  If absent, `rcirc-default-full-name' is
used.

`:channels'

VALUE must be a list of strings describing which channels to join
when connecting to this server.  If absent, no channels will be
connected to automatically.

`:encryption'

VALUE must be `plain' (the default) for unencrypted connections, or `tls'
for connections using SSL/TLS.

`:server-alias'

VALUE must be a string that will be used instead of the server
name for display purposes.  If absent, the real server name will
be displayed instead."
  :type '(alist :key-type string
		:value-type (plist :options
                                   ((:nick string)
                                    (:port integer)
                                    (:user-name string)
                                    (:password string)
                                    (:full-name string)
                                    (:channels (repeat string))
                                    (:encryption (choice (const tls)
                                                         (const plain)))
                                    (:server-alias string))))
  :version "28.1")

(defcustom rcirc-default-port 6667
  "The default port to connect to."
  :type 'integer)

(defcustom rcirc-default-nick (user-login-name)
  "Your nick."
  :type 'string)

(defcustom rcirc-default-user-name "user"
  "Your user name sent to the server when connecting."
  :version "24.1"                       ; changed default
  :type 'string)

(defcustom rcirc-default-full-name "unknown"
  "The full name sent to the server when connecting."
  :version "24.1"                       ; changed default
  :type 'string)

(defcustom rcirc-default-part-reason rcirc-id-string
  "The default reason to send when parting from a channel.
Used when no reason is explicitly given."
  :type 'string)

(defcustom rcirc-default-quit-reason rcirc-id-string
  "The default reason to send when quitting a server.
Used when no reason is explicitly given."
  :type 'string)

(defcustom rcirc-fill-flag t
  "Non-nil means line-wrap messages printed in channel buffers."
  :type 'boolean)

(defcustom rcirc-fill-column nil
  "Column beyond which automatic line-wrapping should happen.
If nil, use value of `fill-column'.
If a function (e.g., `frame-text-width' or `window-text-width'),
call it to compute the number of columns."
  :risky t                              ; can get funcalled
  :type '(choice (const :tag "Value of `fill-column'" nil)
		 (integer :tag "Number of columns")
                 (function :tag "Function returning the number of columns")))

(defcustom rcirc-fill-prefix nil
  "Text to insert before filled lines.
If nil, calculate the prefix dynamically to line up text
underneath each nick."
  :type '(choice (const :tag "Dynamic" nil)
		 (string :tag "Prefix text")))

(defcustom rcirc-url-max-length nil
  "Maximum number of characters in displayed URLs.
If nil, no maximum is applied."
  :version "27.1"
  :type '(choice (const :tag "No maximum" nil)
                 (integer :tag "Number of characters")))

(defvar-local rcirc-ignore-buffer-activity-flag nil
  "Non-nil means ignore activity in this buffer.")

(defvar-local rcirc-low-priority-flag nil
  "Non-nil means activity in this buffer is considered low priority.")

(defvar-local rcirc-pending-requests '()
  "List of pending requests.
See `rcirc-omit-unless-requested'.")

(defcustom rcirc-omit-unless-requested '()
  "List of commands to only be requested if preceded by a command.
For example, if \"TOPIC\" is added to this list, TOPIC commands
will only be displayed if `rcirc-cmd-TOPIC' was previously
invoked.  Commands will only be hidden if `rcirc-omit-mode' is
enabled."
  :version "28.1"
  :type '(repeat string))

(defcustom rcirc-omit-responses
  '("JOIN" "PART" "QUIT" "NICK")
  "Responses which will be hidden when `rcirc-omit-mode' is enabled."
  :type '(repeat string))

(defvar-local rcirc-prompt-start-marker nil
  "Marker indicating the beginning of the message prompt.")

(define-minor-mode rcirc-omit-mode
  "Toggle the hiding of \"uninteresting\" lines.

Uninteresting lines are those whose responses are listed in
`rcirc-omit-responses'."
  :lighter " Omit"
  (if rcirc-omit-mode
      (add-to-invisibility-spec '(rcirc-omit . nil))
    (remove-from-invisibility-spec '(rcirc-omit . nil)))
  (dolist (window (get-buffer-window-list (current-buffer)))
    (with-selected-window window
      (recenter (when (> (point) rcirc-prompt-start-marker) -1)))))

(defcustom rcirc-time-format "%H:%M "
  "Describes how timestamps are printed.
Used as the first arg to `format-time-string'."
  :type 'string)

(defcustom rcirc-input-ring-size 1024
  "Size of input history ring."
  :type 'integer)

(defcustom rcirc-read-only-flag t
  "Non-nil means make text in IRC buffers read-only."
  :type 'boolean)

(defcustom rcirc-buffer-maximum-lines nil
  "The maximum size in lines for rcirc buffers.
Channel buffers are truncated from the top to be no greater than this
number.  If zero or nil, no truncating is done."
  :type '(choice (const :tag "No truncation" nil)
                 (integer :tag "Number of lines")))

(defcustom rcirc-scroll-show-maximum-output t
  "Non-nil means scroll to keep the point at the bottom of the window."
  :type 'boolean)

(defcustom rcirc-authinfo nil
  "List of authentication passwords.
Each element of the list is a list with a SERVER-REGEXP string
and a method symbol followed by method specific arguments.

The valid METHOD symbols are `nickserv', `chanserv' and
`bitlbee'.

The ARGUMENTS for each METHOD symbol are:
  `nickserv': NICK PASSWORD [NICKSERV-NICK]
  `chanserv': NICK CHANNEL PASSWORD
  `bitlbee': NICK PASSWORD
  `quakenet': ACCOUNT PASSWORD
  `sasl': NICK PASSWORD

Examples:
 ((\"Libera.Chat\" nickserv \"bob\" \"p455w0rd\")
  (\"Libera.Chat\" chanserv \"bob\" \"#bobland\" \"passwd99\")
  (\"bitlbee\" bitlbee \"robert\" \"sekrit\")
  (\"dal.net\" nickserv \"bob\" \"sekrit\" \"NickServ@services.dal.net\")
  (\"quakenet.org\" quakenet \"bobby\" \"sekrit\")
  (\"oftc\" sasl \"bob\" \"hunter2\"))"
  :type '(alist :key-type (regexp :tag "Server")
		:value-type (choice (list :tag "NickServ"
					  (const nickserv)
					  (string :tag "Nick")
					  (string :tag "Password"))
				    (list :tag "ChanServ"
					  (const chanserv)
					  (string :tag "Nick")
					  (string :tag "Channel")
					  (string :tag "Password"))
				    (list :tag "BitlBee"
					  (const bitlbee)
					  (string :tag "Nick")
					  (string :tag "Password"))
                                    (list :tag "QuakeNet"
                                          (const quakenet)
                                          (string :tag "Account")
                                          (string :tag "Password"))
                                    (list :tag "SASL"
                                          (const sasl)
                                          (string :tag "Nick")
                                          (string :tag "Password")))))

(defcustom rcirc-auto-authenticate-flag t
  "Non-nil means automatically send authentication string to server.
See also `rcirc-authinfo'."
  :type 'boolean)

(defcustom rcirc-authenticate-before-join t
  "Non-nil means authenticate to services before joining channels.
Currently only works with NickServ on some networks."
  :version "24.1"
  :type 'boolean)

(defcustom rcirc-prompt "> "
  "Prompt string to use in IRC buffers.

The following replacements are made:
%n is your nick.
%s is the server.
%t is the buffer target, a channel or a user.

Setting this alone will not affect the prompt; use either
\\[execute-extended-command] customize or also call
`rcirc-update-prompt'."
  :type 'string
  :set #'rcirc-set-changed
  :initialize 'custom-initialize-default)

(defcustom rcirc-keywords nil
  "List of keywords to highlight in message text."
  :type '(repeat string))

(defcustom rcirc-ignore-list ()
  "List of ignored nicks.
Use /ignore to list them, use /ignore NICK to add or remove a nick."
  :type '(repeat string))

(defvar rcirc-ignore-list-automatic ()
  "List of ignored nicks added to `rcirc-ignore-list' because of renaming.
When an ignored person renames, their nick is added to both lists.
Nicks will be removed from the automatic list on follow-up renamings or
parts.")

(defcustom rcirc-bright-nicks nil
  "List of nicks to be emphasized.
See `rcirc-bright-nick' face."
  :type '(repeat string))

(defcustom rcirc-dim-nicks nil
  "List of nicks to be deemphasized.
See `rcirc-dim-nick' face."
  :type '(repeat string))

(define-obsolete-variable-alias 'rcirc-print-hooks
  'rcirc-print-functions "24.3")
(defcustom rcirc-print-functions nil
  "Hook run after text is printed.
Called with 5 arguments, PROCESS, SENDER, RESPONSE, TARGET and TEXT."
  :type 'hook)

(defvar rcirc-authenticated-hook nil
  "Hook run after successfully authenticated.
Functions in this hook are called with a single argument PROCESS.")

(defcustom rcirc-always-use-server-buffer-flag nil
  "Non-nil means messages without a channel target will go to the server buffer."
  :type 'boolean)

(defcustom rcirc-decode-coding-system 'utf-8
  "Coding system used to decode incoming irc messages.
Set to `undecided' if you want the encoding of the incoming
messages autodetected."
  :type 'coding-system)

(defcustom rcirc-encode-coding-system 'utf-8
  "Coding system used to encode outgoing irc messages."
  :type 'coding-system)

(defcustom rcirc-coding-system-alist nil
  "Alist to decide a coding system to use for a channel I/O operation.
The format is ((PATTERN . VAL) ...).
PATTERN is either a string or a cons of strings.
If PATTERN is a string, it is used to match a target.
If PATTERN is a cons of strings, the car part is used to match a
target, and the cdr part is used to match a server.
VAL is either a coding system or a cons of coding systems.
If VAL is a coding system, it is used for both decoding and encoding
messages.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding."
  :type '(alist :key-type (choice (regexp :tag "Channel Regexp")
					  (cons (regexp :tag "Channel Regexp")
						(regexp :tag "Server Regexp")))
		:value-type (choice coding-system
				    (cons (coding-system :tag "Decode")
                                          (coding-system :tag "Encode")))))

(defcustom rcirc-multiline-major-mode 'fundamental-mode
  "Major-mode function to use in multiline edit buffers."
  :type 'function)

(defcustom rcirc-nick-completion-format "%s: "
  "Format string to use in nick completions.

The format string is only used when completing at the beginning
of a line.  The string is passed as the first argument to
`format' with the nickname as the second argument."
  :version "24.1"
  :type 'string)

(defcustom rcirc-kill-channel-buffers nil
  "When non-nil, kill channel buffers when the server buffer is killed.
Only the channel buffers associated with the server in question
will be killed."
  :version "24.3"
  :type 'boolean)

(defcustom rcirc-nick-filter #'identity
  "Function applied to nicknames before displaying."
  :version "28.1"
  :type 'function)

(defcustom rcirc-channel-filter #'identity
  "Function applied to channels before displaying."
  :version "28.1"
  :type 'function)

(defcustom rcirc-track-ignore-server-buffer-flag nil
  "Non-nil means activities in the server buffer are not traced."
  :version "28.1"
  :type 'boolean)

(defcustom rcirc-display-server-buffer t
  "Non-nil means the server buffer should be shown on connecting."
  :version "28.1"
  :type 'boolean)

(defvar-local rcirc-nick nil
  "The nickname used for the current connection.")

(defvar-local rcirc-prompt-end-marker nil
  "Marker indicating the end of the message prompt.")

(defvar-local rcirc-nick-table nil
  "Hash table mapping nicks to channels.")

(defvar-local rcirc-recent-quit-alist nil
  "Alist of nicks that have recently quit or parted the channel.")

(defvar rcirc-nick-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (mapc (lambda (c) (modify-syntax-entry c "w" table))
          "[]\\`_^{|}-")
    (modify-syntax-entry ?' "_" table)
    table)
  "Syntax table which includes all nick characters as word constituents.")

(defvar-local rcirc-buffer-alist nil
  "Alist of (TARGET . BUFFER) pairs.")

(defvar rcirc-activity nil
  "List of buffers with unviewed activity.")

(defvar rcirc-activity-string ""
  "String displayed in mode line representing `rcirc-activity'.")
(put 'rcirc-activity-string 'risky-local-variable t)

(defvar-local rcirc-server-buffer nil
  "The server buffer associated with this channel buffer.")

(defvar-local rcirc-server-parameters nil
  "List of parameters received from the server.")

(defvar-local rcirc-target nil
  "The channel or user associated with this buffer.")

(defvar-local rcirc-urls nil
  "List of URLs seen in the current buffer and their start positions.")
(put 'rcirc-urls 'permanent-local t)

(defvar rcirc-timeout-seconds 600
  "Kill connection after this many seconds if there is no activity.")


(defvar-local rcirc-startup-channels nil
  "List of channel names to join after authenticating.")

(defvar rcirc-server-name-history nil
  "History variable for \\[rcirc] call.")

(defvar rcirc-server-port-history nil
  "History variable for \\[rcirc] call.")

(defvar rcirc-nick-name-history nil
  "History variable for \\[rcirc] call.")

(defvar rcirc-user-name-history nil
  "History variable for \\[rcirc] call.")

;;;###autoload
(defun rcirc (arg)
  "Connect to all servers in `rcirc-server-alist'.

Do not connect to a server if it is already connected.

If ARG is non-nil, instead prompt for connection parameters."
  (interactive "P")
  (if arg
      (let* ((server (completing-read "IRC Server: "
				      rcirc-server-alist
				      nil nil
				      (caar rcirc-server-alist)
				      'rcirc-server-name-history))
	     (server-plist (cdr (assoc-string server rcirc-server-alist)))
	     (port (read-string "IRC Port: "
				(number-to-string
				 (or (plist-get server-plist :port)
				     rcirc-default-port))
				'rcirc-server-port-history))
	     (nick (read-string "IRC Nick: "
				(or (plist-get server-plist :nick)
				    rcirc-default-nick)
				'rcirc-nick-name-history))
	     (user-name (read-string "IRC Username: "
                                     (or (plist-get server-plist :user-name)
                                         rcirc-default-user-name)
                                     'rcirc-user-name-history))
	     (password (read-passwd "IRC Password: " nil
                                    (plist-get server-plist :password)))
	     (channels (split-string
			(read-string "IRC Channels: "
				     (mapconcat 'identity
						(plist-get server-plist
							   :channels)
						" "))
			"[, ]+" t))
             (encryption (rcirc-prompt-for-encryption server-plist))
             (process (rcirc-connect server port nick user-name
		                     rcirc-default-full-name
		                     channels password encryption)))
	(when rcirc-display-server-buffer
          (pop-to-buffer-same-window (process-buffer process))))
    ;; connect to servers in `rcirc-server-alist'
    (let (connected-servers)
      (dolist (c rcirc-server-alist)
	(let ((server (car c))
	      (nick (or (plist-get (cdr c) :nick) rcirc-default-nick))
	      (port (or (plist-get (cdr c) :port) rcirc-default-port))
	      (user-name (or (plist-get (cdr c) :user-name)
			     rcirc-default-user-name))
	      (full-name (or (plist-get (cdr c) :full-name)
			     rcirc-default-full-name))
	      (channels (plist-get (cdr c) :channels))
              (password (plist-get (cdr c) :password))
              (encryption (plist-get (cdr c) :encryption))
              (server-alias (plist-get (cdr c) :server-alias))
              contact)
          (when-let (((not password))
                     (auth (auth-source-search :host server
                                               :user user-name
                                               :port port))
                     (fn (plist-get (car auth) :secret)))
            (setq password (funcall fn)))
	  (when server
	    (let (connected)
	      (dolist (p (rcirc-process-list))
		(when (string= (or server-alias server) (process-name p))
		  (setq connected p)))
	      (if (not connected)
		  (condition-case nil
		      (let ((process (rcirc-connect server port nick user-name
                                                    full-name channels password encryption
                                                    server-alias)))
                        (when rcirc-display-server-buffer
                          (pop-to-buffer-same-window (process-buffer process))))
		    (quit (message "Quit connecting to %s"
                                   (or server-alias server))))
		(with-current-buffer (process-buffer connected)
                  (setq contact (process-contact
                                 (get-buffer-process (current-buffer)) :name))
                  (setq connected-servers
                        (cons (if (stringp contact)
                                  contact (or server-alias server))
                              connected-servers))))))))
      (when connected-servers
	(message "Already connected to %s"
		 (if (cdr connected-servers)
		     (concat (mapconcat 'identity (butlast connected-servers) ", ")
			     ", and "
			     (car (last connected-servers)))
		   (car connected-servers)))))))

;;;###autoload
(defalias 'irc 'rcirc)


(defvar-local rcirc-process-output nil
  "Partial message response.")
(defvar-local rcirc-topic nil
  "Topic of the current channel.")
(defvar rcirc-keepalive-timer nil
  "Timer for sending KEEPALIVE message.")
(defvar-local rcirc-last-server-message-time nil
  "Timestamp for the last server response.")
(defvar-local rcirc-server nil
  "Server provided by server.")
(defvar-local rcirc-server-name nil
  "Server name given by 001 response.")
(defvar-local rcirc-timeout-timer nil
  "Timer for determining a network timeout.")
(defvar-local rcirc-user-authenticated nil
  "Flag indicating if the user is authenticated.")
(defvar-local rcirc-user-disconnect nil
  "Flag indicating if the connection was broken.")
(defvar-local rcirc-connecting nil
  "Flag indicating if the connection is being established.")
(defvar-local rcirc-connection-info nil
  "Information about the current connection.
If defined, it is a list of this form (SERVER PORT NICK USER-NAME
FULL-NAME STARTUP-CHANNELS PASSWORD ENCRYPTION SERVER-ALIAS).
See `rcirc-connect' for more details on these variables.")
(defvar-local rcirc-process nil
  "Network process for the current connection.")
(defvar-local rcirc-last-connect-time nil
  "The last time the buffer was connected.")

;;; IRCv3 capability negotiation (https://ircv3.net/specs/extensions/capability-negotiation)
(defvar rcirc-implemented-capabilities
  '("message-tags"                      ;https://ircv3.net/specs/extensions/message-tags
    "server-time"                       ;https://ircv3.net/specs/extensions/server-time
    "batch"                             ;https://ircv3.net/specs/extensions/batch
    "message-ids"                       ;https://ircv3.net/specs/extensions/message-ids
    "invite-notify"                     ;https://ircv3.net/specs/extensions/invite-notify
    "sasl"                              ;https://ircv3.net/specs/extensions/sasl-3.1
    "multi-prefix"                      ;https://ircv3.net/specs/extensions/multi-prefix
    "standard-replies"                  ;https://ircv3.net/specs/extensions/standard-replies
    ;; The following capabilities should be implemented as soon as
    ;; their specifications are undrafted:
    ;;
    ;; "reply"                             ;https://ircv3.net/specs/client-tags/reply
    ;; "react"                             ;https://ircv3.net/specs/client-tags/react
    ;; "multiline"                         ;https://ircv3.net/specs/extensions/multiline
    ;; "chathistory"                       ;https://ircv3.net/specs/extensions/chathistory
    ;; "channel-rename"                    ;https://ircv3.net/specs/extensions/channel-rename
    )
  "A list of capabilities that rcirc supports.")
(defvar-local rcirc-requested-capabilities nil
  "A list of capabilities that client has requested.")
(defvar-local rcirc-acked-capabilities nil
  "A list of capabilities that the server supports.")
(defvar-local rcirc-finished-sasl t
  "Check whether SASL authentication has completed.")

(defun rcirc-get-server-method (server)
  "Return authentication method for SERVER."
  (catch 'method
    (dolist (i rcirc-authinfo)
      (let ((server-i (car i))
	    (method (cadr i)))
	(when (string-match server-i server)
          (throw 'method method))))))

(defun rcirc-get-server-password (server)
  "Return password for SERVER."
  (catch 'pass
    (dolist (i rcirc-authinfo)
      (let ((server-i (car i))
	    (args (cdddr i)))
	(when (string-match server-i server)
          (throw 'pass (car args)))))))

;;;###autoload
(defun rcirc-connect (server &optional port nick user-name
                             full-name startup-channels password encryption
                             server-alias)
  "Connect to SERVER.
The arguments PORT, NICK, USER-NAME, FULL-NAME, PASSWORD,
ENCRYPTION, SERVER-ALIAS are interpreted as in
`rcirc-server-alist'.  STARTUP-CHANNELS is a list of channels
that are joined after authentication."
  (save-excursion
    (message "Connecting to %s..." (or server-alias server))
    (let* ((inhibit-eol-conversion)
           (port-number (if port
                            (if (stringp port)
                                (string-to-number port)
                              port)
                          rcirc-default-port))
           (nick (or nick rcirc-default-nick))
           (user-name (or user-name rcirc-default-user-name))
           (full-name (or full-name rcirc-default-full-name))
           (startup-channels startup-channels)

           process)

      ;; Ensure any previous process is killed
      (when-let ((old-process (get-process (or server-alias server))))
        (set-process-sentinel old-process #'ignore)
        (delete-process process))

      ;; Set up process
      (setq process (open-network-stream
                     (or server-alias server) nil server port-number
                     :type (or encryption 'plain)
                     :nowait t))
      (set-process-coding-system process 'raw-text 'raw-text)
      (with-current-buffer (get-buffer-create (rcirc-generate-new-buffer-name process nil))
        (set-process-buffer process (current-buffer))
        (unless (eq major-mode 'rcirc-mode)
          (rcirc-mode process nil))
        (set-process-sentinel process #'rcirc-sentinel)
        (set-process-filter process #'rcirc-filter)

        (setq rcirc-connection-info
              (list server port nick user-name full-name startup-channels
                    password encryption server-alias))
        (setq rcirc-process process)
        (setq rcirc-server server)
        (setq rcirc-server-name (or server-alias server)) ; Update when we get 001 response.
        (setq rcirc-nick-table (make-hash-table :test 'equal))
        (setq rcirc-nick nick)
        (setq rcirc-startup-channels startup-channels)
        (setq rcirc-last-server-message-time (current-time))
        (setq rcirc-last-connect-time (current-time))

        ;; Check if the immediate process state
        (sit-for .1)
        (cond
         ((eq (process-status process) 'failed)
          (setq mode-line-process ":disconnected")
          (setq rcirc-connecting nil))
         ((eq (process-status process) 'connect)
          (setq mode-line-process ":connecting")
          (setq rcirc-connecting t)))

        (add-hook 'auto-save-hook #'rcirc-log-write)

        ;; return process object
        process))))

(defmacro with-rcirc-process-buffer (process &rest body)
  "Evaluate BODY in the buffer of PROCESS."
  (declare (indent 1) (debug t))
  `(with-current-buffer (process-buffer ,process)
     ,@body))

(defmacro with-rcirc-server-buffer (&rest body)
  "Evaluate BODY in the server buffer of the current channel."
  (declare (indent 0) (debug t))
  `(if (buffer-live-p rcirc-server-buffer)
       (with-current-buffer rcirc-server-buffer
         ,@body)
     (user-error "Server buffer was killed")))

(define-obsolete-function-alias 'rcirc-float-time 'float-time "26.1")

(defun rcirc-prompt-for-encryption (server-plist)
  "Prompt the user for the encryption method to use.
SERVER-PLIST is the property list for the server."
  (if (or (eq (plist-get server-plist :encryption) 'plain)
          (yes-or-no-p "Encrypt connection?"))
      'tls 'plain))

(defun rcirc-keepalive ()
  "Send keep alive pings to active rcirc processes.
Kill processes that have not received a server message since the
last ping."
  (if (rcirc-process-list)
      (mapc (lambda (process)
	      (with-rcirc-process-buffer process
		(when (not rcirc-connecting)
                  (rcirc-send-ctcp process
                                   rcirc-nick
                                   (format "KEEPALIVE %f"
                                           (float-time))))))
            (rcirc-process-list))
    ;; no processes, clean up timer
    (when (timerp rcirc-keepalive-timer)
      (cancel-timer rcirc-keepalive-timer))
    (setq rcirc-keepalive-timer nil)))

(defun rcirc-handler-ctcp-KEEPALIVE (process _target _sender message)
  "Uptime header in PROCESS buffer.
MESSAGE should contain a timestamp, indicating when the KEEPALIVE
message was generated."
  (with-rcirc-process-buffer process
    (setq header-line-format
	  (format "%f" (float-time
			(time-since (string-to-number message)))))))

(defvar rcirc-debug-buffer "*rcirc debug*"
  "Buffer name for debugging messages.")
(defvar rcirc-debug-flag nil
  "Non-nil means write information to `rcirc-debug-buffer'.")
(defun rcirc-debug (process text)
  "Add an entry to the debug log including PROCESS and TEXT.
Debug text is appended to `rcirc-debug-buffer' if `rcirc-debug-flag'
is non-nil.

For convenience, the read-only state of the debug buffer is ignored.
When the point is at the end of the visible portion of the buffer, it
is moved to after the text inserted.  Otherwise the point is not moved."
  (when rcirc-debug-flag
    (with-current-buffer (get-buffer-create rcirc-debug-buffer)
      (let ((old (point-marker)))
        (set-marker-insertion-type old t)
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (terpri (current-buffer) t)
          (insert "["
                  (format-time-string "%FT%T ") (process-name process)
                  "] "
                  text))
        (goto-char old)))))

(define-obsolete-variable-alias 'rcirc-sentinel-hooks
  'rcirc-sentinel-functions "24.3")
(defvar rcirc-sentinel-functions nil
  "Hook functions called when the process sentinel is called.
Functions are called with PROCESS and SENTINEL arguments.")

(defcustom rcirc-reconnect-delay 0
  "The minimum interval in seconds between reconnect attempts.
When 0, do not auto-reconnect."
  :version "25.1"
  :type 'integer)

(defcustom rcirc-reconnect-attempts 3
  "Number of times a reconnection should be attempted."
  :version "28.1"
  :type 'integer)

(defvar-local rcirc-failed-attempts 0
  "Number of times reconnecting has failed.")

(defvar-local rcirc-reconnection-timer nil
  "Timer used for reconnecting.")

(defun rcirc-reconnect (process &optional quiet)
  "Attempt to reconnect connection to PROCESS.
If QUIET is non-nil, no not emit a message."
  (with-rcirc-process-buffer process
    (catch 'exit
      (if (rcirc--connection-open-p process)
          (throw 'exit (or quiet (message "Server process is alive")))
        (delete-process process))
      (let ((conn-info rcirc-connection-info))
	(setf (nth 5 conn-info)
	      (cl-remove-if-not #'rcirc-channel-p
				(mapcar #'car rcirc-buffer-alist)))
        (dolist (buffer (mapcar #'cdr rcirc-buffer-alist))
	  (when (buffer-live-p buffer)
            (with-current-buffer buffer
	      (setq mode-line-process ":connecting"))))
	(let ((nprocess (apply #'rcirc-connect conn-info)))
          (when (and (< rcirc-failed-attempts rcirc-reconnect-attempts)
                     (eq (process-status nprocess) 'failed))
            (setq rcirc-failed-attempts (1+ rcirc-failed-attempts))
            (rcirc-print nprocess "*rcirc*" "ERROR" nil
		         (format "Failed to reconnect (%d/%d)..."
                                 rcirc-failed-attempts
                                 rcirc-reconnect-attempts))
            (setq rcirc-reconnection-timer
                  (run-at-time rcirc-reconnect-delay nil
                               #'rcirc-reconnect process t))))))))

(defun rcirc-sentinel (process sentinel)
  "Called on a change of the state of PROCESS.
SENTINEL describes the change in form of a string."
  (let ((status (process-status process)))
    (rcirc-debug process (format "SENTINEL: %S %S\n" process sentinel))
    (with-rcirc-process-buffer process
      (cond
       ((eq status 'open)
        (let* ((server (nth 0 rcirc-connection-info))
               (user-name (nth 3 rcirc-connection-info))
               (full-name (nth 4 rcirc-connection-info))
               (password (nth 6 rcirc-connection-info))
               (server-alias (nth 8 rcirc-connection-info))
               (use-sasl (eq (rcirc-get-server-method server) 'sasl)))

          ;; Prepare SASL authentication
          (when use-sasl
            (rcirc-send-string process "CAP REQ sasl")
            (setq-local rcirc-finished-sasl nil))

          ;; Capability negotiation
          (dolist (cap rcirc-implemented-capabilities)
            (rcirc-send-string process "CAP" "REQ" : cap)
            (push cap rcirc-requested-capabilities))

          ;; Identify user
          (unless (zerop (length password))
            (rcirc-send-string process "PASS" password))
          (rcirc-send-string process "NICK" rcirc-nick)
          (rcirc-send-string process "USER" user-name "0" "*" : full-name)

          ;; Setup sasl, and initiate authentication.
          (when (and rcirc-auto-authenticate-flag
                     use-sasl)
            (rcirc-send-string process "AUTHENTICATE" "PLAIN"))

          ;; Setup ping timer if necessary
          (unless rcirc-keepalive-timer
            (setq rcirc-keepalive-timer
                  (run-at-time 0 (/ rcirc-timeout-seconds 2) #'rcirc-keepalive)))

          ;; Reset previous reconnection attempts
          (setq rcirc-failed-attempts 0)
          (when rcirc-reconnection-timer
            (cancel-timer rcirc-reconnection-timer)
            (setq rcirc-reconnection-timer nil))

          (message "Connecting to %s...done" (or server-alias server))
          (dolist (buffer (cons nil (mapcar 'cdr rcirc-buffer-alist)))
	    (with-current-buffer (or buffer (current-buffer))
	      (setq mode-line-process nil)))))
       ((eq status 'closed)
        (let ((now (current-time)))
          (with-rcirc-process-buffer process
            (when (and (< 0 rcirc-reconnect-delay)
                       (time-less-p rcirc-reconnect-delay
				    (time-subtract now rcirc-last-connect-time)))
              (setq rcirc-last-connect-time now)
              (rcirc-reconnect process)))))
       ((eq status 'failed)
        (dolist (buffer (cons nil (mapcar 'cdr rcirc-buffer-alist)))
	  (with-current-buffer (or buffer (current-buffer))
	    (rcirc-print process "*rcirc*" "ERROR" rcirc-target
		         (format "%s: %s (%S)"
			         (process-name process)
			         sentinel
			         (process-status process))
                         (not rcirc-target))
	    (rcirc-disconnect-buffer)))))
      (run-hook-with-args 'rcirc-sentinel-functions process sentinel))))

(defun rcirc-disconnect-buffer (&optional buffer)
  "Disconnect BUFFER.
If BUFFER is nil, default to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    ;; set rcirc-target to nil for each channel so cleanup
    ;; doesn't happen when we reconnect
    (setq rcirc-target nil)
    (setq mode-line-process ":disconnected")))

(defun rcirc-process-list ()
  "Return a list of rcirc processes."
  (let (ps)
    (mapc (lambda (p)
            (when (buffer-live-p (process-buffer p))
              (with-rcirc-process-buffer p
                (when (eq major-mode 'rcirc-mode)
                  (setq ps (cons p ps))))))
          (process-list))
    ps))

(define-obsolete-variable-alias 'rcirc-receive-message-hooks
  'rcirc-receive-message-functions "24.3")
(defvar rcirc-receive-message-functions nil
  "Hook functions run when a message is received from server.
Function is called with PROCESS, COMMAND, SENDER, ARGS and LINE.")
(defun rcirc-filter (process output)
  "Called when PROCESS receives OUTPUT."
  (rcirc-debug process output)
  (rcirc-reschedule-timeout process)
  (with-rcirc-process-buffer process
    (setq rcirc-last-server-message-time (current-time))
    (setq rcirc-process-output (concat rcirc-process-output output))
    (when (= ?\n (aref rcirc-process-output
                       (1- (length rcirc-process-output))))
      (let ((lines (split-string rcirc-process-output "[\n\r]" t)))
        (setq rcirc-process-output nil)
        (dolist (line lines)
          (rcirc-process-server-response process line))))))

(defun rcirc-reschedule-timeout (process)
  "Update timeout indicator for PROCESS."
  (with-rcirc-process-buffer process
    (when (not rcirc-connecting)
      (with-rcirc-process-buffer process
	(when rcirc-timeout-timer (cancel-timer rcirc-timeout-timer))
	(setq rcirc-timeout-timer (run-at-time rcirc-timeout-seconds nil
					       'delete-process
					       process))))))

(defvar rcirc-trap-errors-flag t
  "Non-nil means Lisp errors are degraded to error messages.")
(defun rcirc-process-server-response (process text)
  "Parse TEXT as received from PROCESS."
  (if rcirc-trap-errors-flag
      (condition-case err
          (rcirc-process-server-response-1 process text)
        (error
         (rcirc-print process "*rcirc*" "ERROR" nil
                      (format "\"%s\" %s" text err) t)))
    (rcirc-process-server-response-1 process text)))

(defconst rcirc-process-regexp
  (rx-let ((message-tag ; message tags as specified in
                        ; https://ircv3.net/specs/extensions/message-tags
            (: (? "+")
               (? (+ (or alnum "-")) (+ "." (+ (or alnum "-"))) "/")
               (+ (any alnum "-"))
               (? "="
                  (* (not (any 0 ?\n ?\r ?\; ?\s)))))))
    (rx line-start
        (optional "@" (group message-tag (* ";" message-tag)) (+ space))
        ;; See https://tools.ietf.org/html/rfc2812#section-2.3.1.
        ;; We're a bit more accepting than the RFC: We allow any non-space
        ;; characters in the command name, multiple spaces between
        ;; arguments, and allow the last argument to omit the leading ":",
        ;; even if there are less than 15 arguments.
        (optional
         (group ":" (group (one-or-more (not (any " ")))) " "))
        (group (one-or-more (not (any " "))))))
  "Regular expression used for parsing server response.")

(defconst rcirc-tag-regexp
  (rx bos
      (group
       (? "+")
       (? (+ (or alnum "-")) (+ "." (+ (or alnum "-"))) "/")
       (+ (any alnum "-")))
      (? "=" (group (* (not (any 0 ?\n ?\r ?\; ?\s)))))
      eos)
  "Regular expression used for destructing a tag.")

(defvar rcirc-message-tags nil
  "Alist of parsed message tags.")

(defvar rcirc-supported-batch-types
  '()
  "List of recognized batch types.
Each element has the form (TYPE HANDLE), where TYPE is a string
and HANDLE is either the symbol `immediate' or `deferred'.
Messages in an immediate batch are handled just like regular
messages, while deferred messages are stored in
`rcirc-batch-messages'.")

(defvar-local rcirc-batch-attributes nil
  "Alist mapping batch IDs to parameters.")

(defvar-local rcirc-batched-messages nil
  "Alist mapping batch IDs to deferred messages.
Note that the messages are stored in reverse order.")

(defsubst rcirc-get-tag (key &optional default)
  "Return tag value for KEY or DEFAULT."
  (alist-get key rcirc-message-tags default nil #'string=))

(defun rcirc-process-server-response-1 (process text)
  "Parse TEXT as received from PROCESS."
  (if (string-match rcirc-process-regexp text)
      (let* ((rcirc-message-tags
              (append
               (and-let* ((tag-data (match-string 1 text)))
                 (save-match-data
                   (mapcar
                    (lambda (tag)
                      (unless (string-match rcirc-tag-regexp tag)
                        ;; This should not happen, unless there is
                        ;; a mismatch between this regular
                        ;; expression and `rcirc-process-regexp'.
                        (error "Malformed tag %S" tag))
                      (cons (match-string 1 tag)
                            (replace-regexp-in-string
                             (rx (* ?\\ ?\\) ?\\ (any ?: ?s ?\\ ?r ?n))
                             (lambda (rep)
                               (concat (substring rep 0 -2)
                                       (cl-case (aref rep (1- (length rep)))
                                         (?:  ";")
                                         (?s  " ")
                                         (?\\ "\\\\")
                                         (?r  "\r")
                                         (?n  "\n"))))
                             (match-string 2 tag))))
                    (split-string tag-data ";"))))
               rcirc-message-tags))
             (user (match-string 3 text))
	     (sender (rcirc-user-nick user))
             (cmd (match-string 4 text))
             (cmd-end (match-end 4))
             (args nil)
             (handler (intern-soft (concat "rcirc-handler-" cmd))))
        (cl-loop with i = cmd-end
                 repeat 14
                 while (eql i (string-match " +\\([^: ][^ ]*\\)" text i))
                 do (progn (push (match-string 1 text) args)
                           (setq i (match-end 0)))
                 finally
                 (progn (if (eql i (string-match " +:?" text i))
                            (push (substring text (match-end 0)) args)
                          (cl-assert (= i (length text))))
                        (cl-callf nreverse args)))
        (cond ((and-let* ((batch-id (rcirc-get-tag "batch"))
                          (type (cadr (assoc batch-id rcirc-batch-attributes)))
                          (attr (assoc type rcirc-supported-batch-types))
                          ((eq (cadr attr) 'deferred)))
                 ;; handle deferred batch messages later
                 (push (list cmd process sender args text rcirc-message-tags)
                       (alist-get batch-id rcirc-batched-messages
                                  nil nil #'string=))
                 t))
              ((not (fboundp handler))
               (rcirc-handler-generic process cmd sender args text))
              ((funcall handler process sender args text)))
        (run-hook-with-args 'rcirc-receive-message-functions
                            process cmd sender args text))
    (message "UNHANDLED: %s" text)))

(defvar rcirc-responses-no-activity '("305" "306")
  "Responses that don't trigger activity in the mode-line indicator.")

(defun rcirc-handler-generic (process response sender args _text)
  "Generic server response handler.
This handler is called, when no more specific handler could be
found.  PROCESS, SENDER and RESPONSE are passed on to
`rcirc-print'.  ARGS are concatenated into a single string and
used as the message body."
  (rcirc-print process sender response nil
               (mapconcat 'identity (cdr args) " ")
	       (not (member response rcirc-responses-no-activity))))

(defun rcirc--connection-open-p (process)
  "Check if PROCESS is open or running."
  (memq (process-status process) '(run open)))

(defun rcirc-send-string (process &rest parts)
  "Send PROCESS a PARTS plus a newline.
PARTS may contain a `:' symbol, to designate that the next string
is the message, that should be prefixed by a colon.  If the last
element in PARTS is a list, append it to PARTS."
  (let ((last (car (last parts))))
    (when (listp last)
      (setf parts (append (butlast parts) last))))
  (when-let (message (memq : parts))
    (cl-check-type (cadr message) string)
    (setf (cadr message) (concat ":" (cadr message))
          parts (remq : parts)))
  (let ((string (concat (encode-coding-string
                         (mapconcat #'identity parts " ")
                         rcirc-encode-coding-system)
                        "\n")))
    (unless (rcirc--connection-open-p process)
      (error "Network connection to %s is not open"
             (process-name process)))
    (rcirc-debug process string)
    (process-send-string process string)))

(defun rcirc-send-privmsg (process target string)
  "Send TARGET the message in STRING via PROCESS."
  (cl-check-type target string)
  (rcirc-send-string process "PRIVMSG" target : string))

(defun rcirc-ctcp-wrap (&rest args)
  "Join ARGS into a string wrapped by ASCII 1 charterers."
  (concat "\C-a" (string-join (delq nil args) " ") "\C-a"))

(defun rcirc-send-ctcp (process target request &optional args)
  "Send TARGET a REQUEST via PROCESS."
  (rcirc-send-privmsg process target (rcirc-ctcp-wrap request args)))

(defun rcirc-buffer-process (&optional buffer)
  "Return the process associated with channel BUFFER.
With no argument or nil as argument, use the current buffer."
  (let ((buffer (or buffer (and (buffer-live-p rcirc-server-buffer)
				rcirc-server-buffer))))
    (if buffer
        (buffer-local-value 'rcirc-process buffer)
      rcirc-process)))

(defun rcirc-server-name (process)
  "Return PROCESS server name, given by the 001 response."
  (with-rcirc-process-buffer process
    (or rcirc-server-name
	(warn "server name for process %S unknown" process))))

(defun rcirc-nick (process)
  "Return PROCESS nick."
  (with-rcirc-process-buffer process
    (or rcirc-nick rcirc-default-nick)))

(defun rcirc-buffer-nick (&optional buffer)
  "Return the nick associated with BUFFER.
With no argument or nil as argument, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (with-rcirc-server-buffer
      (or rcirc-nick rcirc-default-nick))))

(defvar rcirc-max-message-length 420
  "Messages longer than this value will be split.")

(defun rcirc-split-message (message)
  "Split MESSAGE into chunks within `rcirc-max-message-length'."
  ;; `rcirc-encode-coding-system' can have buffer-local value.
  (let ((encoding rcirc-encode-coding-system))
    (with-temp-buffer
      (insert message)
      (goto-char (point-min))
      (let (result)
	(while (not (eobp))
	  (goto-char (or (byte-to-position rcirc-max-message-length)
			 (point-max)))
	  ;; max message length is 512 including CRLF
	  (while (and (not (bobp))
		      (> (length (encode-coding-region
				  (point-min) (point) encoding t))
			 rcirc-max-message-length))
	    (forward-char -1))
	  (push (delete-and-extract-region (point-min) (point)) result))
	(nreverse result)))))

(defun rcirc-send-message (process target message &optional noticep silent)
  "Send TARGET associated with PROCESS a privmsg with text MESSAGE.
If NOTICEP is non-nil, send a notice instead of privmsg.
If SILENT is non-nil, do not print the message in any irc buffer."
  (let ((response (if noticep "NOTICE" "PRIVMSG")))
    (rcirc-get-buffer-create process target)
    (dolist (msg (rcirc-split-message message))
      (rcirc-send-string process response target : msg)
      (unless silent
	(rcirc-print process (rcirc-nick process) response target msg)))))

(defvar-local rcirc-input-ring nil
  "Ring object for input.")

(defvar-local rcirc-input-ring-index 0
  "Current position in the input ring.")

(defun rcirc-prev-input-string (arg)
  "Move ARG elements ahead in the input ring."
  (ring-ref rcirc-input-ring (+ rcirc-input-ring-index arg)))

(defun rcirc-insert-prev-input ()
  "Insert previous element in input ring."
  (interactive)
  (when (<= rcirc-prompt-end-marker (point))
    (delete-region rcirc-prompt-end-marker (point-max))
    (insert (rcirc-prev-input-string 0))
    (setq rcirc-input-ring-index (1+ rcirc-input-ring-index))))

(defun rcirc-insert-next-input ()
  "Insert next element in input ring."
  (interactive)
  (when (<= rcirc-prompt-end-marker (point))
    (delete-region rcirc-prompt-end-marker (point-max))
    (setq rcirc-input-ring-index (1- rcirc-input-ring-index))
    (insert (rcirc-prev-input-string -1))))

(defvar rcirc-server-commands
  '("/admin"   "/away"   "/connect" "/die"      "/error"   "/info"
    "/invite"  "/ison"   "/join"    "/kick"     "/kill"    "/links"
    "/list"    "/lusers" "/mode"    "/motd"     "/names"   "/nick"
    "/notice"  "/oper"   "/part"    "/pass"     "/ping"    "/pong"
    "/privmsg" "/quit"   "/rehash"  "/restart"  "/service" "/servlist"
    "/server"  "/squery" "/squit"   "/stats"    "/summon"  "/time"
    "/topic"   "/trace"  "/user"    "/userhost" "/users"   "/version"
    "/wallops" "/who"    "/whois"   "/whowas")
  "A list of user commands by IRC server.
The value defaults to RFCs 1459 and 2812.")

;; /me and /ctcp are not defined by `defun-rcirc-command'.
(defvar rcirc-client-commands '("/me" "/ctcp")
  "A list of user commands defined by IRC client rcirc.
The list is updated automatically by `defun-rcirc-command'.")

(defun rcirc-completion-at-point ()
  "Function used for `completion-at-point-functions' in `rcirc-mode'."
  (and (rcirc-looking-at-input)
       (let* ((beg (save-excursion
                     ;; On some networks it is common to message or
                     ;; mention someone using @nick instead of just
                     ;; nick.
		     (if (re-search-backward "[[:space:]@]" rcirc-prompt-end-marker t)
			 (1+ (point))
		       rcirc-prompt-end-marker)))
	      (table (cond
                      ;; No completion before the prompt
                      ((< beg rcirc-prompt-end-marker) nil)
                      ;; Only complete nicks mid-message
                      ((> beg rcirc-prompt-end-marker)
                       (mapcar rcirc-nick-filter
                               (rcirc-channel-nicks
                                (rcirc-buffer-process)
				rcirc-target)))
                      ;; Complete commands at the beginning of the
                      ;; message, when the first character is a dash
                      ((eq (char-after beg) ?/)
                       (mapcar
                        (lambda (cmd) (concat cmd " "))
                        (nconc (sort (copy-sequence rcirc-client-commands)
				     'string-lessp)
			       (sort (copy-sequence rcirc-server-commands)
				     'string-lessp))))
                      ;; Complete usernames right after the prompt by
                      ;; appending a colon after the name
                      ((mapcar
                        (lambda (str) (concat (funcall rcirc-nick-filter str) ": "))
                        (rcirc-channel-nicks (rcirc-buffer-process)
					     rcirc-target))))))
	 (list beg (point)
               (lambda (str pred action)
                 (if (eq action 'metadata)
                     '(metadata (cycle-sort-function . identity))
                   (complete-with-action action table str pred)))))))

(defun rcirc-set-decode-coding-system (coding-system)
  "Set the decode CODING-SYSTEM used in this channel."
  (interactive "zCoding system for incoming messages: ")
  (setq-local rcirc-decode-coding-system coding-system))

(define-obsolete-function-alias
  'rcirc-set-decode-coding-system
  'set-rcirc-decode-coding-system
  "28.1")

(defun rcirc-set-encode-coding-system (coding-system)
  "Set the encode CODING-SYSTEM used in this channel."
  (interactive "zCoding system for outgoing messages: ")
  (setq-local rcirc-encode-coding-system coding-system))

(define-obsolete-function-alias
  'rcirc-set-encode-coding-system
  'set-rcirc-encode-coding-system
  "28.1")

(defvar rcirc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'rcirc-send-input)
    (define-key map (kbd "M-p") 'rcirc-insert-prev-input)
    (define-key map (kbd "M-n") 'rcirc-insert-next-input)
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-c C-b") 'rcirc-browse-url)
    (define-key map (kbd "C-c C-c") 'rcirc-edit-multiline)
    (define-key map (kbd "C-c C-j") 'rcirc-cmd-join)
    (define-key map (kbd "C-c C-k") 'rcirc-cmd-kick)
    (define-key map (kbd "C-c C-l") 'rcirc-toggle-low-priority)
    (define-key map (kbd "C-c C-d") 'rcirc-cmd-mode)
    (define-key map (kbd "C-c C-m") 'rcirc-cmd-msg)
    (define-key map (kbd "C-c C-r") 'rcirc-cmd-nick) ; rename
    (define-key map (kbd "C-c C-o") 'rcirc-omit-mode)
    (define-key map (kbd "C-c C-p") 'rcirc-cmd-part)
    (define-key map (kbd "C-c C-q") 'rcirc-cmd-query)
    (define-key map (kbd "C-c C-t") 'rcirc-cmd-topic)
    (define-key map (kbd "C-c C-n") 'rcirc-cmd-names)
    (define-key map (kbd "C-c C-w") 'rcirc-cmd-whois)
    (define-key map (kbd "C-c C-x") 'rcirc-cmd-quit)
    (define-key map (kbd "C-c TAB") ; C-i
      'rcirc-toggle-ignore-buffer-activity)
    (define-key map (kbd "C-c C-s") 'rcirc-switch-to-server-buffer)
    (define-key map (kbd "C-c C-a") 'rcirc-jump-to-first-unread-line)
    map)
  "Keymap for rcirc mode.")

(defvar-local rcirc-short-buffer-name nil
  "Generated abbreviation to use to indicate buffer activity.")

(defvar rcirc-mode-hook nil
  "Hook run when setting up rcirc buffer.")

(defvar-local rcirc-last-post-time nil
  "Timestamp indicating last user action.")

(defvar rcirc-log-alist nil
  "Alist of lines to log to disk when `rcirc-log-flag' is non-nil.
Each element looks like (FILENAME . TEXT).")

(defvar-local rcirc-current-line 0
  "The current number of responses printed in this channel.
This number is independent of the number of lines in the buffer.")

(defun rcirc-mode (process target)
  "Initialize an IRC buffer for writing with TARGET.
PROCESS is the process object used for communication.

\\{rcirc-mode-map}"
  ;; FIXME: Use define-derived-mode.
  (kill-all-local-variables)
  (use-local-map rcirc-mode-map)
  (setq mode-name "rcirc")
  (setq major-mode 'rcirc-mode)
  (setq mode-line-process nil)

  (setq rcirc-input-ring
	      ;; If rcirc-input-ring is already a ring with desired
	      ;; size do not re-initialize.
	      (if (and (ring-p rcirc-input-ring)
		       (= (ring-size rcirc-input-ring)
			  rcirc-input-ring-size))
		  rcirc-input-ring
		(make-ring rcirc-input-ring-size)))
  (setq rcirc-server-buffer (process-buffer process))
  (setq rcirc-target target)
  (setq rcirc-last-post-time (current-time))
  (setq-local fill-paragraph-function 'rcirc-fill-paragraph)
  (setq rcirc-current-line 0)

  (use-hard-newlines t)

  ;; setup for omitting responses
  (setq buffer-invisibility-spec '())
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table 4
			  (let ((glyph (make-glyph-code
					?. 'font-lock-keyword-face)))
			    (make-vector 3 glyph)))

  (dolist (i rcirc-coding-system-alist)
    (let ((chan (if (consp (car i)) (caar i) (car i)))
	  (serv (if (consp (car i)) (cdar i) "")))
      (when (and (string-match chan (or target ""))
		 (string-match serv (rcirc-server-name process)))
	(setq-local rcirc-decode-coding-system
		    (if (consp (cdr i)) (cadr i) (cdr i)))
        (setq-local rcirc-encode-coding-system
		    (if (consp (cdr i)) (cddr i) (cdr i))))))

  ;; setup the prompt and markers
  (setq rcirc-prompt-start-marker (point-max-marker))
  (setq rcirc-prompt-end-marker (point-max-marker))
  (rcirc-update-prompt)
  (goto-char rcirc-prompt-end-marker)

  (setq-local overlay-arrow-position (make-marker))

  ;; if the user changes the major mode or kills the buffer, there is
  ;; cleanup work to do
  (add-hook 'change-major-mode-hook 'rcirc-change-major-mode-hook nil t)
  (add-hook 'kill-buffer-hook 'rcirc-kill-buffer-hook nil t)

  ;; add to buffer list, and update buffer abbrevs
  (when target				; skip server buffer
    (let ((buffer (current-buffer)))
      (with-rcirc-process-buffer process
        (push (cons target buffer) rcirc-buffer-alist)))
    (rcirc-update-short-buffer-names))

  (add-hook 'completion-at-point-functions
            'rcirc-completion-at-point nil 'local)
  (setq-local completion-cycle-threshold t)

  (run-mode-hooks 'rcirc-mode-hook))

(defun rcirc-update-prompt (&optional all)
  "Reset the prompt string in the current buffer.

If ALL is non-nil, update prompts in all IRC buffers."
  (if all
      (mapc (lambda (process)
	      (mapc (lambda (buffer)
		      (with-current-buffer buffer
			(rcirc-update-prompt)))
		    (with-rcirc-process-buffer process
		      (mapcar 'cdr rcirc-buffer-alist))))
	    (rcirc-process-list))
    (let ((inhibit-read-only t)
	  (prompt (or rcirc-prompt "")))
      (mapc (lambda (rep)
	      (setq prompt
		    (replace-regexp-in-string (car rep) (cdr rep) prompt)))
	    (list (cons "%n" (rcirc-buffer-nick))
		  (cons "%s" (with-rcirc-server-buffer rcirc-server-name))
		  (cons "%t" (or rcirc-target ""))))
      (save-excursion
	(delete-region rcirc-prompt-start-marker rcirc-prompt-end-marker)
	(goto-char rcirc-prompt-start-marker)
	(let ((start (point)))
	  (insert-before-markers prompt)
	  (set-marker rcirc-prompt-start-marker start)
	  (when (not (zerop (- rcirc-prompt-end-marker
			       rcirc-prompt-start-marker)))
	    (add-text-properties rcirc-prompt-start-marker
				 rcirc-prompt-end-marker
				 (list 'face 'rcirc-prompt
				       'read-only t 'field t
				       'front-sticky t 'rear-nonsticky t))))))))

(defun rcirc-set-changed (option value)
  "Set OPTION to VALUE and update after a customization change."
  (set-default option value)
  (cond ((eq option 'rcirc-prompt)
	 (rcirc-update-prompt 'all))
	(t
	 (error "Bad option %s" option))))

(defun rcirc-channel-p (target)
  "Return t if TARGET is a channel name."
  (and target
       (not (zerop (length target)))
       (or (eq (aref target 0) ?#)
           (eq (aref target 0) ?&))))

(defcustom rcirc-log-directory (locate-user-emacs-file "rcirc-log")
  "Directory to keep IRC logfiles."
  :type 'directory
  :version "28.1")

(defcustom rcirc-log-flag nil
  "Non-nil means log IRC activity to disk.
Logfiles are kept in `rcirc-log-directory'."
  :type 'boolean)

(defun rcirc-kill-buffer-hook ()
  "Part the channel when killing an rcirc buffer.

If `rcirc-kill-channel-buffers' is non-nil and the killed buffer
is a server buffer, kills all of the channel buffers associated
with it."
  (when (eq major-mode 'rcirc-mode)
    (when (and rcirc-log-flag
               rcirc-log-directory)
      (rcirc-log-write))
    (rcirc-clean-up-buffer "Killed buffer")
    (when-let ((process (get-buffer-process (current-buffer))))
      (delete-process process))
    (when (and rcirc-buffer-alist ;; it's a server buffer
               rcirc-kill-channel-buffers)
      (dolist (channel rcirc-buffer-alist)
	(kill-buffer (cdr channel))))))

(defun rcirc-change-major-mode-hook ()
  "Part the channel when changing the major mode."
  (rcirc-clean-up-buffer "Changed major mode"))

(defun rcirc-clean-up-buffer (reason)
  "Clean up current buffer and part with REASON."
  (let ((buffer (current-buffer)))
    (rcirc-clear-activity buffer)
    (when (and (rcirc-buffer-process)
	       (rcirc--connection-open-p (rcirc-buffer-process)))
      (with-rcirc-server-buffer
       (setq rcirc-buffer-alist
	     (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
	  (rcirc-send-string (rcirc-buffer-process)
                             "PART" rcirc-target : reason)
	(when rcirc-target
	  (rcirc-remove-nick-channel (rcirc-buffer-process)
				     (rcirc-buffer-nick)
				     rcirc-target))))
    (setq rcirc-target nil)))

(defun rcirc-generate-new-buffer-name (process target)
  "Return a buffer name based on PROCESS and TARGET.
This is used for the initial name given to IRC buffers."
  (substring-no-properties
   (if target
       (concat target "@" (process-name process))
     (concat "*" (process-name process) "*"))))

(defun rcirc-get-buffer (process target &optional server)
  "Return the buffer associated with the PROCESS and TARGET.

If optional argument SERVER is non-nil, return the server buffer
if there is no existing buffer for TARGET, otherwise return nil."
  (with-rcirc-process-buffer process
    (if (null target)
	(current-buffer)
      (let ((buffer (cdr (assoc-string target rcirc-buffer-alist t))))
	(or buffer (when server (current-buffer)))))))

(defun rcirc-get-buffer-create (process target)
  "Return the buffer associated with the PROCESS and TARGET.
Create the buffer if it doesn't exist."
  (let ((buffer (rcirc-get-buffer process target)))
    (if (and buffer (buffer-live-p buffer))
	(with-current-buffer buffer
	  (when (not rcirc-target)
 	    (setq rcirc-target target))
	  buffer)
      ;; create the buffer
      (with-rcirc-process-buffer process
	(let ((new-buffer (get-buffer-create
			   (rcirc-generate-new-buffer-name process target))))
	  (with-current-buffer new-buffer
            (unless (eq major-mode 'rcirc-mode)
	      (rcirc-mode process target))
            (setq mode-line-process nil))
	  (rcirc-put-nick-channel process (rcirc-nick process) target
				  rcirc-current-line)
	  new-buffer)))))

(defun rcirc-send-input ()
  "Send input to target associated with the current buffer."
  (interactive)
  (if (< (point) rcirc-prompt-end-marker)
      ;; copy the line down to the input area
      (progn
	(forward-line 0)
	(let ((start (if (eq (point) (point-min))
			 (point)
		       (if (get-text-property (1- (point)) 'hard)
			   (point)
			 (previous-single-property-change (point) 'hard))))
	      (end (next-single-property-change (1+ (point)) 'hard)))
	  (goto-char (point-max))
	  (insert (replace-regexp-in-string
		   "\n\\s-+" " "
		   (buffer-substring-no-properties start end)))))
    ;; process input
    (goto-char (point-max))
    (when (not (equal 0 (- (point) rcirc-prompt-end-marker)))
      ;; delete a trailing newline
      (when (eq (point) (point-at-bol))
	(delete-char -1))
      (let ((input (buffer-substring-no-properties
		    rcirc-prompt-end-marker (point))))
	(dolist (line (split-string input "\n"))
	  (rcirc-process-input-line line))
	;; add to input-ring
	(save-excursion
	  (ring-insert rcirc-input-ring input)
	  (setq rcirc-input-ring-index 0))))))

(defun rcirc-fill-paragraph (&optional justify)
  "Implementation for `fill-paragraph-function'.
The argument JUSTIFY is passed on to `fill-region'."
  (interactive "P")
  (when (> (point) rcirc-prompt-end-marker)
    (save-restriction
      (narrow-to-region rcirc-prompt-end-marker (point-max))
      (let ((fill-column rcirc-max-message-length))
	(fill-region (point-min) (point-max) justify)))))

(defun rcirc-process-input-line (line)
  "Process LINE as a message or a command."
  (if (string-match "^/\\([^/ ][^ ]*\\) ?\\(.*\\)$" line)
      (rcirc-process-command (match-string 1 line)
			     (match-string 2 line)
			     line)
    (rcirc-process-message line)))

(defun rcirc-process-message (line)
  "Process LINE as a message to be sent."
  (when (and (null rcirc-target)
             (string-match
              (rx bos (group (+? nonl)) "@" (+ nonl) eos)
              (buffer-name)))
    (setq rcirc-target (match-string 1 (buffer-name))))
  (if (not rcirc-target)
      (message "Not joined (no target)")
    (delete-region rcirc-prompt-end-marker (point))
    (rcirc-send-message (rcirc-buffer-process) rcirc-target line)
    (setq rcirc-last-post-time (current-time))))

(defun rcirc-process-command (command args line)
  "Process COMMAND with arguments ARGS.
LINE is the raw input, from which COMMAND and ARGS was
extracted."
  (let ((fun (intern-soft (concat "rcirc-cmd-" command)))
	(process (rcirc-buffer-process)))
    (newline)
    (with-current-buffer (current-buffer)
      (delete-region rcirc-prompt-end-marker (point))
      (if (string= command "me")
	  (rcirc-print process (rcirc-buffer-nick)
		       "ACTION" rcirc-target args)
	(rcirc-print process (rcirc-buffer-nick)
		     "COMMAND" rcirc-target line))
      (set-marker rcirc-prompt-end-marker (point))
      (if (fboundp fun)
	  (funcall fun args process rcirc-target)
	(rcirc-send-string process command : args)))))

(defvar-local rcirc-parent-buffer nil
  "Message buffer that requested a multiline buffer.")
(put 'rcirc-parent-buffer 'permanent-local t)

(defvar rcirc-window-configuration nil
  "Window configuration before creating multiline buffer.")

(defun rcirc-edit-multiline ()
  "Move current edit to a dedicated buffer."
  (interactive)
  (let ((pos (1+ (- (point) rcirc-prompt-end-marker))))
    (goto-char (point-max))
    (let ((text (buffer-substring-no-properties rcirc-prompt-end-marker
						(point)))
          (parent (buffer-name)))
      (delete-region rcirc-prompt-end-marker (point))
      (setq rcirc-window-configuration (current-window-configuration))
      (pop-to-buffer (concat "*multiline " parent "*"))
      (funcall rcirc-multiline-major-mode)
      (rcirc-multiline-minor-mode 1)
      (setq rcirc-parent-buffer parent)
      (insert text)
      (and (> pos 0) (goto-char pos))
      (message "Type C-c C-c to return text to %s, or C-c C-k to cancel" parent))))

(defvar rcirc-multiline-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'rcirc-multiline-minor-submit)
    (define-key map (kbd "C-x C-s") 'rcirc-multiline-minor-submit)
    (define-key map (kbd "C-c C-k") 'rcirc-multiline-minor-cancel)
    (define-key map (kbd "ESC ESC ESC") 'rcirc-multiline-minor-cancel)
    map)
  "Keymap for multiline mode in rcirc.")

(define-minor-mode rcirc-multiline-minor-mode
  "Minor mode for editing multiple lines in rcirc."
  :lighter " rcirc-mline"
  :global nil
  (setq fill-column rcirc-max-message-length))

(defun rcirc-multiline-minor-submit ()
  "Send the text in buffer back to parent buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (let ((text (buffer-substring (point-min) (point-max)))
        (buffer (current-buffer))
        (pos (point)))
    (set-buffer rcirc-parent-buffer)
    (goto-char (point-max))
    (insert text)
    (kill-buffer buffer)
    (set-window-configuration rcirc-window-configuration)
    (goto-char (+ rcirc-prompt-end-marker (1- pos)))))

(defun rcirc-multiline-minor-cancel ()
  "Cancel the multiline edit."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration rcirc-window-configuration))

(defun rcirc-any-buffer (process)
  "Return a buffer for PROCESS, either the one selected or the process buffer."
  (if rcirc-always-use-server-buffer-flag
      (process-buffer process)
    (let ((buffer (window-buffer)))
      (if (and buffer
	       (with-current-buffer buffer
		 (and (eq major-mode 'rcirc-mode)
		      (eq (rcirc-buffer-process) process))))
	  buffer
	(process-buffer process)))))

(defcustom rcirc-response-formats
  '(("PRIVMSG" . "<%N> %m")
    ("NOTICE"  . "-%N- %m")
    ("ACTION"  . "[%N %m]")
    ("COMMAND" . "%m")
    ("ERROR"   . "%fw!!! %m")
    ("FAIL"   . "(%fwFAIL%f-) %m")
    ("WARN"   . "(%fwWARN%f-) %m")
    ("NOTE"   . "(%fwNOTE%f-) %m")
    (t         . "%fp*** %fs%n %r %m"))
  "An alist of formats used for printing responses.
The format is looked up using the response-type as a key;
if no match is found, the default entry (with a key of t) is used.

The entry's value part should be a string, which is inserted with
the of the following escape sequences replaced by the described values:

  %m        The message text
  %n        The sender's nick
  %N        The sender's nick (with face `rcirc-my-nick' or `rcirc-other-nick')
  %r        The response-type
  %t        The target
  %fw       Following text uses the face `font-lock-warning-face'
  %fp       Following text uses the face `rcirc-server-prefix'
  %fs       Following text uses the face `rcirc-server'
  %f[FACE]  Following text uses the face FACE
  %f-       Following text uses the default face
  %%        A literal `%' character"
  :type '(alist :key-type (choice (string :tag "Type")
				  (const :tag "Default" t))
                :value-type string))

(defun rcirc-format-response-string (process sender response target text)
  "Return a formatted response string from SENDER, incorporating TEXT.
The specific formatting used is found by looking up RESPONSE in
`rcirc-response-formats'.  PROCESS is the process object used for
communication."
  (with-temp-buffer
    (insert (or (cdr (assoc response rcirc-response-formats))
		(cdr (assq t rcirc-response-formats))))
    (goto-char (point-min))
    (let ((start (point-min))
	  (sender (if (or (not sender)
			  (string= (rcirc-server-name process) sender))
		      ""
		    (funcall rcirc-nick-filter sender)))
	  face)
      (while (re-search-forward "%\\(\\(f\\(.\\)\\)\\|\\(.\\)\\)" nil t)
	(rcirc-add-face start (match-beginning 0) face)
	(setq start (match-beginning 0))
	(replace-match
	 (cl-case (aref (match-string 1) 0)
	    (?f (setq face
		      (cl-case (string-to-char (match-string 3))
			(?w 'font-lock-warning-face)
			(?p 'rcirc-server-prefix)
			(?s 'rcirc-server)
			(t nil)))
		"")
	    (?n sender)
	    (?N (let ((my-nick (rcirc-nick process)))
		  (save-match-data
		    (with-syntax-table rcirc-nick-syntax-table
		      (rcirc-facify sender
				    (cond ((string= sender my-nick)
					   'rcirc-my-nick)
					  ((and rcirc-bright-nicks
						(string-match
						 (regexp-opt rcirc-bright-nicks
							     'words)
						 sender))
					   'rcirc-bright-nick)
					  ((and rcirc-dim-nicks
						(string-match
						 (regexp-opt rcirc-dim-nicks
							     'words)
						 sender))
					   'rcirc-dim-nick)
					  (t
					   'rcirc-other-nick)))))))
	    (?m (propertize text 'rcirc-text text))
	    (?r response)
	    (?t (or target ""))
	    (t (concat "UNKNOWN CODE:" (match-string 0))))
	 t t nil 0)
	(rcirc-add-face (match-beginning 0) (match-end 0) face))
      (rcirc-add-face start (match-beginning 0) face))
      (buffer-substring (point-min) (point-max))))

(defun rcirc-target-buffer (process sender response target _text)
  "Return a buffer to print the server response from SENDER.
PROCESS is the process object for the current connection."
  (cl-assert (not (bufferp target)))
  (with-rcirc-process-buffer process
    (cond ((not target)
	   (rcirc-any-buffer process))
	  ((not (rcirc-channel-p target))
	   ;; message from another user
	   (if (or (string= response "PRIVMSG")
		   (string= response "ACTION"))
	       (rcirc-get-buffer-create process (if (string= sender rcirc-nick)
						    target
						  sender))
	     (rcirc-get-buffer process target t)))
	  ((or (rcirc-get-buffer process target)
	       (rcirc-any-buffer process))))))

(defvar-local rcirc-last-sender nil)
(defvar-local rcirc-activity-types nil
  "List of symbols designating kinds of activities in a buffer.")

(defcustom rcirc-omit-threshold 100
  "Lines since last activity from a nick before `rcirc-omit-responses' are omitted."
  :type 'integer)

(defcustom rcirc-log-process-buffers nil
  "Non-nil if rcirc process buffers should be logged to disk."
  :type 'boolean
  :version "24.1")

(defun rcirc-last-quit-line (process nick target)
  "Return the line number where NICK left TARGET.
Returns nil if the information is not recorded.
PROCESS is the process object for the current connection."
  (let ((chanbuf (rcirc-get-buffer process target)))
    (when chanbuf
      (cdr (assoc-string nick (buffer-local-value
                               'rcirc-recent-quit-alist
                               chanbuf))))))

(defun rcirc-last-line (process nick target)
  "Return the line from the last activity from NICK in TARGET.
PROCESS is the process object for the current connection."
  (let ((line (or (cdr (assoc-string target
				     (gethash nick (with-rcirc-server-buffer
						     rcirc-nick-table)) t))
		  (rcirc-last-quit-line process nick target))))
    (if line
	line
      ;;(message "line is nil for %s in %s" nick target)
      nil)))

(defun rcirc-elapsed-lines (process nick target)
  "Return the number of lines since activity from NICK in TARGET.
PROCESS is the process object for the current connection."
  (let ((last-activity-line (rcirc-last-line process nick target)))
    (when (and last-activity-line
	       (> last-activity-line 0))
      (- rcirc-current-line last-activity-line))))

(defvar rcirc-markup-text-functions
  '(rcirc-markup-attributes
    rcirc-color-attributes
    rcirc-remove-markup-codes
    rcirc-markup-my-nick
    rcirc-markup-urls
    rcirc-markup-keywords
    rcirc-markup-bright-nicks)
  "List of functions used to manipulate text before it is printed.

Each function takes two arguments, SENDER, and RESPONSE.  The
buffer is narrowed with the text to be printed and the point is
at the beginning of the `rcirc-text' propertized text.")

(defun rcirc-print (process sender response target text &optional activity)
  "Print TEXT in the buffer associated with TARGET.
Format based on SENDER and RESPONSE.  If ACTIVITY is non-nil,
record activity.  PROCESS is the process object for the current
connection."
  (or text (setq text ""))
  (unless (and (or (member sender rcirc-ignore-list)
		   (member (with-syntax-table rcirc-nick-syntax-table
			     (when (string-match "^\\([^/]\\w*\\)[:,]" text)
			       (match-string 1 text)))
			   rcirc-ignore-list))
	       ;; do not ignore if we sent the message
 	       (not (string= sender (rcirc-nick process))))
    (let* ((buffer (rcirc-target-buffer process sender response target text))
           (time (if-let ((time (rcirc-get-tag "time")))
                     (parse-iso8601-time-string time)
                   (current-time)))
	   (inhibit-read-only t))
      (with-current-buffer buffer
	(let ((moving (= (point) rcirc-prompt-end-marker))
	      (old-point (point-marker)))

	  (setq text (decode-coding-string text rcirc-decode-coding-system))
	  (unless (string= sender (rcirc-nick process))
	    ;; mark the line with overlay arrow
	    (unless (or (marker-position overlay-arrow-position)
			(get-buffer-window (current-buffer))
			(member response rcirc-omit-responses))
	      (set-marker overlay-arrow-position
			  (marker-position rcirc-prompt-start-marker))))

	  ;; temporarily set the marker insertion-type because
	  ;; insert-before-markers results in hidden text in new buffers
	  (goto-char rcirc-prompt-start-marker)
          (catch 'exit
            (while (not (bobp))
              (goto-char (or (previous-single-property-change (point) 'hard)
                             (point-min)))
              (when (let ((then (get-text-property (point) 'rcirc-time)))
                      (and then (not (time-less-p time then))))
                (next-single-property-change (point) 'hard)
                (forward-char 1)
                (throw 'exit nil))))
	  (set-marker-insertion-type rcirc-prompt-start-marker t)
	  (set-marker-insertion-type rcirc-prompt-end-marker t)

          ;; run markup functions
          (cl-assert (bolp))
          (save-excursion
            (save-restriction
              (narrow-to-region (point) (point))
              (insert (propertize (rcirc-format-response-string process sender response
                                                              nil text)
                                'rcirc-msgid (rcirc-get-tag "msgid"))
		      (propertize "\n" 'hard t))

              ;; squeeze spaces out of text before rcirc-text
              (fill-region (point-min) (point-max))

              (goto-char (or (next-single-property-change (point-min) 'rcirc-text)
			       (point)))
		(when (rcirc-buffer-process)
		  (save-excursion (rcirc-markup-timestamp sender response))
		  (dolist (fn rcirc-markup-text-functions)
		    (save-excursion (funcall fn sender response)))
		  (when rcirc-fill-flag
		    (save-excursion (rcirc-markup-fill sender response))))

		(when rcirc-read-only-flag
		  (add-text-properties (point-min) (point-max)
                                     '(read-only t front-sticky t)))

              (add-text-properties (point-min) (point-max)
                                   (list 'rcirc-time time))

              ;; make text omittable
	      (let ((last-activity-lines (rcirc-elapsed-lines process sender target)))
		(if (and (not (string= (rcirc-nick process) sender))
                         (or (member response rcirc-omit-responses)
                             (and (member response rcirc-omit-unless-requested)
                                  (if (member response rcirc-pending-requests)
                                      (ignore (setq rcirc-pending-requests
                                                    (delete response rcirc-pending-requests)))
                                    t)))
                         (or (member response rcirc-omit-unless-requested)
                             (not last-activity-lines)
                             (< rcirc-omit-threshold last-activity-lines)))
                  (put-text-property (point-min) (point-max)
				       'invisible 'rcirc-omit)
		  ;; otherwise increment the line count
		  (setq rcirc-current-line (1+ rcirc-current-line))))))

	  (set-marker-insertion-type rcirc-prompt-start-marker nil)
	  (set-marker-insertion-type rcirc-prompt-end-marker nil)

	  ;; truncate buffer if it is very long
	  (save-excursion
	    (when (and rcirc-buffer-maximum-lines
		       (> rcirc-buffer-maximum-lines 0)
		       (= (forward-line (- rcirc-buffer-maximum-lines)) 0))
	      (delete-region (point-min) (point))))

	  ;; set the window point for buffers show in windows
	  (walk-windows (lambda (w)
			  (when (and (not (eq (selected-window) w))
				     (eq (current-buffer)
					 (window-buffer w))
				     (>= (window-point w)
					 rcirc-prompt-end-marker))
			    (set-window-point w (point-max))))
			nil t)

	  ;; restore the point
	  (goto-char (if moving rcirc-prompt-end-marker old-point)))

	  ;; keep window on bottom line if it was already there
	  (when rcirc-scroll-show-maximum-output
	    (let ((window (get-buffer-window)))
	      (when window
		(with-selected-window window
		  (when (eq major-mode 'rcirc-mode)
		    (when (<= (- (window-height)
				 (count-screen-lines (window-point)
						     (window-start))
				 1)
			      0)
		      (recenter -1)))))))

	  ;; flush undo (can we do something smarter here?)
	  (buffer-disable-undo)
	  (buffer-enable-undo)

        ;; record mode line activity
        (when (and activity
                   (not rcirc-ignore-buffer-activity-flag)
                   (not (and rcirc-dim-nicks sender
                             (string-match (regexp-opt rcirc-dim-nicks) sender)
                             (rcirc-channel-p target))))
            (rcirc-record-activity (current-buffer)
                                   (when (not (rcirc-channel-p rcirc-target))
                                     'nick)))

        (when (and rcirc-log-flag
                   (or target
                       rcirc-log-process-buffers))
          (rcirc-log process sender response target text))

        (sit-for 0)			; displayed text before hook
        (run-hook-with-args 'rcirc-print-functions
                            process sender response target text)))))

(defun rcirc-generate-log-filename (process target)
  "Return filename for log file based on PROCESS and TARGET."
  (if target
      (rcirc-generate-new-buffer-name process target)
    (process-name process)))

(defcustom rcirc-log-filename-function 'rcirc-generate-log-filename
  "A function to generate the filename used by rcirc's logging facility.

It is called with two arguments, PROCESS and TARGET (see
`rcirc-generate-new-buffer-name' for their meaning), and should
return the filename, or nil if no logging is desired for this
session.

If the returned filename is absolute (`file-name-absolute-p'
returns t), then it is used as-is, otherwise the resulting file
is put into `rcirc-log-directory'.

The filename is then cleaned using `convert-standard-filename' to
guarantee valid filenames for the current OS."
  :type 'function)

(defun rcirc-log (process sender response target text)
  "Record TEXT from SENDER to TARGET to be logged.
The message is logged in `rcirc-log', and is later written to
disk.  PROCESS is the process object for the current connection."
  (let ((filename (funcall rcirc-log-filename-function process target))
        (time (and-let* ((time (rcirc-get-tag "time")))
                (parse-iso8601-time-string time))))
    (unless (null filename)
      (let ((cell (assoc-string filename rcirc-log-alist))
	    (line (concat (format-time-string rcirc-time-format time)
			  (substring-no-properties
			   (rcirc-format-response-string process sender
							 response target text))
			  "\n")))
	(if cell
	    (setcdr cell (concat (cdr cell) line))
	  (setq rcirc-log-alist
		(cons (cons filename line) rcirc-log-alist)))))))

(defun rcirc-log-write ()
  "Flush `rcirc-log-alist' data to disk.

Log data is written to `rcirc-log-directory', except for
log-files with absolute names (see `rcirc-log-filename-function')."
  (dolist (cell rcirc-log-alist)
    (let ((filename (convert-standard-filename
                     (expand-file-name (car cell)
                                       rcirc-log-directory)))
	  (coding-system-for-write 'utf-8))
      (make-directory (file-name-directory filename) t)
      (with-temp-buffer
	(insert (cdr cell))
	(write-region (point-min) (point-max) filename t 'quiet))))
  (setq rcirc-log-alist nil))

(defun rcirc-view-log-file ()
  "View logfile corresponding to the current buffer."
  (interactive)
  (find-file-other-window
   (expand-file-name (funcall rcirc-log-filename-function
			      (rcirc-buffer-process) rcirc-target)
		     rcirc-log-directory)))

(defun rcirc-join-channels (process channels)
  "Join CHANNELS.
PROCESS is the process object for the current connection."
  (save-window-excursion
    (dolist (channel channels)
      (with-rcirc-process-buffer process
	(rcirc-cmd-join channel process)))))

;;; nick management
(defvar rcirc-nick-prefix-chars '(?~ ?& ?@ ?% ?+)
  "List of junk characters to strip from nick prefixes.")

(defun rcirc-user-nick (user)
  "Return the nick from USER.  Remove any non-nick junk."
  (save-match-data
    (if (string-match (concat "^[" rcirc-nick-prefix-chars
			      "]*\\([^! ]+\\)!?")
                      (or user ""))
	(match-string 1 user)
      user)))

(defun rcirc-nick-channels (process nick)
  "Return list of channels for NICK.
PROCESS is the process object for the current connection."
  (with-rcirc-process-buffer process
    (mapcar (lambda (x) (car x))
	    (gethash nick rcirc-nick-table))))

(defun rcirc-put-nick-channel (process nick channel &optional line)
  "Add CHANNEL to list associated with NICK.
Update the associated linestamp if LINE is non-nil.

If the record doesn't exist, and LINE is nil, set the linestamp
to zero.  PROCESS is the process object for the current connection."
  (let ((nick (rcirc-user-nick nick)))
    (with-rcirc-process-buffer process
      (let* ((chans (gethash nick rcirc-nick-table))
	     (record (assoc-string channel chans t)))
	(if record
	    (when line (setcdr record line))
	  (puthash nick (cons (cons channel (or line 0))
			      chans)
		   rcirc-nick-table))))))

(defun rcirc-nick-remove (process nick)
  "Remove NICK from table.
PROCESS is the process object for the current connection."
  (with-rcirc-process-buffer process
    (remhash nick rcirc-nick-table)))

(defun rcirc-remove-nick-channel (process nick channel)
  "Remove the CHANNEL from list associated with NICK.
PROCESS is the process object for the current connection."
  (with-rcirc-process-buffer process
    (let* ((chans (gethash nick rcirc-nick-table))
           (newchans
	    ;; instead of assoc-string-delete-all:
	    (let ((record (assoc-string channel chans t)))
	      (when record
		(setcar record 'delete)
		(assq-delete-all 'delete chans)))))
      (if newchans
          (puthash nick newchans rcirc-nick-table)
        (remhash nick rcirc-nick-table)))))

(defun rcirc-channel-nicks (process target)
  "Return the list of nicks associated with TARGET sorted by last activity.
PROCESS is the process object for the current connection."
  (when target
    (if (rcirc-channel-p target)
	(with-rcirc-process-buffer process
	  (let (nicks)
	    (maphash
	     (lambda (k v)
	       (let ((record (assoc-string target v t)))
		 (if record
		     (setq nicks (cons (cons k (cdr record)) nicks)))))
	     rcirc-nick-table)
	    (mapcar (lambda (x) (car x))
		    (sort nicks (lambda (x y)
				  (let ((lx (or (cdr x) 0))
					(ly (or (cdr y) 0)))
				    (< ly lx)))))))
      (list target))))

(defun rcirc-ignore-update-automatic (nick)
  "Check if NICK is in `rcirc-ignore-list-automatic'.
If so, remove from `rcirc-ignore-list'.  PROCESS is the process
object for the current connection."
  (when (member nick rcirc-ignore-list-automatic)
      (setq rcirc-ignore-list-automatic
	    (delete nick rcirc-ignore-list-automatic)
	    rcirc-ignore-list
	    (delete nick rcirc-ignore-list))))

(defun rcirc-nickname< (s1 s2)
  "Return non-nil if IRC nickname S1 is less than S2, and nil otherwise.
Operator nicknames (@) are considered less than voiced
nicknames (+).  Any other nicknames are greater than voiced
nicknames.  The comparison is case-insensitive."
  (setq s1 (downcase s1)
        s2 (downcase s2))
  (let* ((s1-op (eq ?@ (string-to-char s1)))
         (s2-op (eq ?@ (string-to-char s2))))
    (if s1-op
        (if s2-op
            (string< (substring s1 1) (substring s2 1))
          t)
      (if s2-op
          nil
        (string< s1 s2)))))

(defun rcirc-sort-nicknames-join (input sep)
  "Return a string of sorted nicknames.
INPUT is a string containing nicknames separated by SEP.
This function does not alter the INPUT string."
  (let* ((parts (split-string input sep t))
         (sorted (sort parts 'rcirc-nickname<)))
    (mapconcat rcirc-nick-filter sorted sep)))

;;; activity tracking
(defvar rcirc-track-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-@") 'rcirc-next-active-buffer)
    (define-key map (kbd "C-c C-SPC") 'rcirc-next-active-buffer)
    map)
  "Keymap for rcirc track minor mode.")

(defcustom rcirc-track-abbrevate-flag t
  "Non-nil means `rcirc-track-minor-mode' should abbreviate names."
  :version "28.1"
  :type 'boolean)

;;;###autoload
(define-minor-mode rcirc-track-minor-mode
  "Global minor mode for tracking activity in rcirc buffers."
  :global t
  (or global-mode-string (setq global-mode-string '("")))
  ;; toggle the mode-line channel indicator
  (if rcirc-track-minor-mode
      (progn
	(and (not (memq 'rcirc-activity-string global-mode-string))
	     (setq global-mode-string
		   (append global-mode-string '(rcirc-activity-string))))
	(add-hook 'window-configuration-change-hook
		  'rcirc-window-configuration-change))
    (setq global-mode-string
	  (delete 'rcirc-activity-string global-mode-string))
    (remove-hook 'window-configuration-change-hook
		 'rcirc-window-configuration-change)))

(add-to-list 'minor-mode-alist '(rcirc-ignore-buffer-activity-flag " Ignore"))
(add-to-list 'minor-mode-alist '(rcirc-low-priority-flag " LowPri"))

(defun rcirc-toggle-ignore-buffer-activity ()
  "Toggle the value of `rcirc-ignore-buffer-activity-flag'."
  (interactive)
  (setq rcirc-ignore-buffer-activity-flag
	(not rcirc-ignore-buffer-activity-flag))
  (message (if rcirc-ignore-buffer-activity-flag
	       "Ignore activity in this buffer"
	     "Notice activity in this buffer"))
  (force-mode-line-update))

(defun rcirc-toggle-low-priority ()
  "Toggle the value of `rcirc-low-priority-flag'."
  (interactive)
  (setq rcirc-low-priority-flag
	(not rcirc-low-priority-flag))
  (message (if rcirc-low-priority-flag
	       "Activity in this buffer is low priority"
	     "Activity in this buffer is normal priority"))
  (force-mode-line-update))

(defun rcirc-switch-to-server-buffer ()
  "Switch to the server buffer associated with current channel buffer."
  (interactive)
  (switch-to-buffer (with-rcirc-server-buffer (current-buffer))))

(defun rcirc-jump-to-first-unread-line ()
  "Move the point to the first unread line in this buffer."
  (interactive)
  (if (marker-position overlay-arrow-position)
      (goto-char overlay-arrow-position)
    (message "No unread messages")))

(defun rcirc-bury-buffers ()
  "Bury all RCIRC buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (eq 'rcirc-mode (buffer-local-value 'major-mode buf))
      (bury-buffer buf)         ; buffers not shown
      (quit-windows-on buf))))  ; buffers shown in a window

(defun rcirc-next-active-buffer (arg)
  "Switch to the next rcirc buffer with activity.
With prefix ARG, go to the next low priority buffer with activity."
  (interactive "P")
  (let* ((pair (rcirc-split-activity rcirc-activity))
	 (lopri (car pair))
	 (hipri (cdr pair)))
    (if (or (and (not arg) hipri)
	    (and arg lopri))
	(progn
	  (switch-to-buffer (car (if arg lopri hipri)))
	  (when (> (point) rcirc-prompt-start-marker)
	    (recenter -1)))
      (rcirc-bury-buffers)
      (message "No IRC activity.%s"
               (if lopri
                   (concat
                    "  Type C-u " (key-description (this-command-keys))
                    " for low priority activity.")
                 ""))))
  (rcirc-update-activity-string))

(define-obsolete-variable-alias 'rcirc-activity-hooks
  'rcirc-activity-functions "24.3")
(defvar rcirc-activity-functions nil
  "Hook to be run when there is channel activity.

Functions are called with a single argument, the buffer with the
activity.  Only run if the buffer is not visible and
`rcirc-ignore-buffer-activity-flag' is non-nil.")

(defun rcirc-record-activity (buffer &optional type)
  "Record BUFFER activity with TYPE."
  (with-current-buffer buffer
    (let ((old-activity rcirc-activity)
	  (old-types rcirc-activity-types))
      (when (and (not (get-buffer-window (current-buffer) t))
                 (not (and rcirc-track-ignore-server-buffer-flag
                           (eq rcirc-server-buffer (current-buffer)))))
	(setq rcirc-activity
	      (sort (if (memq (current-buffer) rcirc-activity) rcirc-activity
                      (cons (current-buffer) rcirc-activity))
		    (lambda (b1 b2)
		      (let ((t1 (buffer-local-value 'rcirc-last-post-time b1))
			    (t2 (buffer-local-value 'rcirc-last-post-time b2)))
			(time-less-p t2 t1)))))
	(cl-pushnew type rcirc-activity-types)
	(unless (and (equal rcirc-activity old-activity)
		     (member type old-types))
	  (rcirc-update-activity-string)))))
  (run-hook-with-args 'rcirc-activity-functions buffer))

(defun rcirc-clear-activity (buffer)
  "Clear the BUFFER activity."
  (setq rcirc-activity (remove buffer rcirc-activity))
  (with-current-buffer buffer
    (setq rcirc-activity-types nil)))

(defun rcirc-clear-unread (buffer)
  "Erase the last read message arrow from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-marker overlay-arrow-position nil))))

(defun rcirc-split-activity (activity)
  "Return a cons cell with ACTIVITY split into (lopri . hipri)."
  (let (lopri hipri)
    (dolist (buf activity)
      (with-current-buffer buf
	(if (and rcirc-low-priority-flag
		 (not (member 'nick rcirc-activity-types)))
	    (push buf lopri)
	  (push buf hipri))))
    (cons (nreverse lopri) (nreverse hipri))))

(defvar rcirc-update-activity-string-hook nil
  "Hook run whenever the activity string is updated.")

(defun rcirc-update-activity-string ()
  "Update mode-line string."
  (let* ((pair (rcirc-split-activity rcirc-activity))
	 (lopri (car pair))
	 (hipri (cdr pair)))
    (setq rcirc-activity-string
	  (cond ((or hipri lopri)
		 (concat (and hipri "[")
			 (rcirc-activity-string hipri)
			 (and hipri lopri ",")
			 (and lopri
			      (concat "("
				      (rcirc-activity-string lopri)
				      ")"))
			 (and hipri "]")))
		((not (null (rcirc-process-list)))
		 "[]")
		(t "[]")))
    (run-hooks 'rcirc-update-activity-string-hook)
    (force-mode-line-update t)))

(defun rcirc-activity-string (buffers)
  "Generate activity string for all BUFFERS."
  (mapconcat (lambda (b)
	       (let ((s (substring-no-properties (rcirc-short-buffer-name b))))
		 (with-current-buffer b
		   (dolist (type rcirc-activity-types)
                     (rcirc-facify s (cl-case type
				       (nick 'rcirc-track-nick)
				       (keyword 'rcirc-track-keyword)))))
                 (let ((map (make-mode-line-mouse-map
                             'mouse-1
                             (lambda ()
                               (interactive)
                               (pop-to-buffer b)))))
                   (propertize s
                               'mouse-face 'mode-line-highlight
                               'local-map map))))
	     buffers ","))

(defun rcirc-short-buffer-name (buffer)
  "Return a short name for BUFFER to use in the mode line indicator."
  (with-current-buffer buffer
    (funcall rcirc-channel-filter
             (replace-regexp-in-string
              "@.*?\\'" ""
              (or (and rcirc-track-abbrevate-flag
                       rcirc-short-buffer-name)
                  (buffer-name))))))

(defun rcirc-visible-buffers ()
  "Return a list of the visible buffers that are in `rcirc-mode'."
  (let (acc)
    (walk-windows (lambda (w)
		    (with-current-buffer (window-buffer w)
		      (when (eq major-mode 'rcirc-mode)
			(push (current-buffer) acc)))))
    acc))

(defvar rcirc-visible-buffers nil
  "List of visible IRC buffers.")

(defun rcirc-window-configuration-change ()
  "Clear activity and overlay arrows, unless minibuffer is active."
  (unless (minibuffer-window-active-p (minibuffer-window))
    (rcirc-window-configuration-change-1)))

(defun rcirc-window-configuration-change-1 ()
  "Clear activity and overlay arrows."
  (let* ((old-activity rcirc-activity)
	 (hidden-buffers rcirc-visible-buffers))

    (setq rcirc-visible-buffers (rcirc-visible-buffers))

    (dolist (vbuf rcirc-visible-buffers)
      (setq hidden-buffers (delq vbuf hidden-buffers))
      ;; clear activity for all visible buffers
      (rcirc-clear-activity vbuf))

    ;; clear unread arrow from recently hidden buffers
    (dolist (hbuf hidden-buffers)
      (rcirc-clear-unread hbuf))

    ;; remove any killed buffers from list
    (setq rcirc-activity
	  (delq nil (mapcar (lambda (buf) (when (buffer-live-p buf) buf))
			    rcirc-activity)))
    ;; update the mode-line string
    (unless (equal old-activity rcirc-activity)
      (rcirc-update-activity-string))))


;;; buffer name abbreviation
(defun rcirc-update-short-buffer-names ()
  "Update variable `rcirc-short-buffer-name' for IRC buffers."
  (let ((bufalist
	 (apply 'append (mapcar (lambda (process)
				  (with-rcirc-process-buffer process
				    rcirc-buffer-alist))
				(rcirc-process-list)))))
    (dolist (i (rcirc-abbreviate bufalist))
      (when (buffer-live-p (cdr i))
	(with-current-buffer (cdr i)
	  (setq rcirc-short-buffer-name (car i)))))))

(defun rcirc-abbreviate (pairs)
  "Generate alist of abbreviated buffer names to buffers.
PAIRS is the concatenated value of all `rcirc-buffer-alist'
values, from each process."
  (apply 'append (mapcar 'rcirc-rebuild-tree (rcirc-make-trees pairs))))

(defun rcirc-rebuild-tree (tree)
  "Merge prefix TREE into alist of unique prefixes to buffers."
  (let ((ch (char-to-string (car tree)))
        acc)
    (dolist (x (cdr tree))
      (if (listp x)
	  (setq acc (append acc
			   (mapcar (lambda (y)
				     (cons (concat ch (car y))
					   (cdr y)))
				   (rcirc-rebuild-tree x))))
	(setq acc (cons (cons ch x) acc))))
    acc))

(defun rcirc-make-trees (pairs)
  "Generate tree prefix tree of buffer names.
PAIRS is a list of (TARGET . BUFFER) entries.  The resulting tree
is a list of (CHAR . CHILDREN) cons-cells, where CHAR is the
leading character and CHILDREN is either BUFFER when a unique
prefix could be found or another tree if it shares the same
prefix with another element in PAIRS."
  (let (alist)
    (mapc (lambda (pair)
	    (if (consp pair)
		(let* ((str (car pair))
		       (data (cdr pair))
		       (char (unless (zerop (length str))
			       (aref str 0)))
		       (rest (unless (zerop (length str))
			       (substring str 1)))
		       (part (if char (assq char alist))))
		  (if part
		      ;; existing partition
		      (setcdr part (cons (cons rest data) (cdr part)))
		    ;; new partition
		    (setq alist (cons (if char
					  (list char (cons rest data))
					data)
				      alist))))
	      (setq alist (cons pair alist))))
	  pairs)
    ;; recurse into cdrs of alist
    (mapc (lambda (x)
	    (when (and (listp x) (listp (cadr x)))
	      (setcdr x (if (> (length (cdr x)) 1)
			    (rcirc-make-trees (cdr x))
			  (setcdr x (list (cdadr x)))))))
	  alist)))

;;; /commands these are called with 3 args: PROCESS, TARGET, which is
;; the current buffer/channel/user, and ARGS, which is a string
;; containing the text following the /cmd.

(defmacro rcirc-define-command (command arguments &rest body)
  "Define a new client COMMAND in BODY that takes ARGUMENTS.
ARGUMENTS may designate optional arguments using a single
`&optional' symbol.  Just like `defun', a string at the beginning
of BODY is interpreted as the documentation string.  Following
that, an interactive form can specified."
  (declare (debug (symbolp (&rest symbolp) def-body))
           (indent defun))
  (cl-check-type command symbol)
  (cl-check-type arguments list)
  (let* ((fn-name (intern (concat "rcirc-cmd-" (symbol-name command))))
         (total (length (remq '&optional arguments)))
         (required (- (length arguments) (length (memq '&optional arguments))))
         (optional (- total required))
         (regexp (with-temp-buffer
                   (insert "\\`")
                   (when arguments
                     (dotimes (_ (1- (length arguments)))
                       (insert "\\(?:\\(.+?\\)[[:space:]]+"))
                     (dotimes (i (1- (length arguments)))
                       (if (< i optional)
                           (insert "\\)?")
                         (insert "\\)"))))
                   (insert "\\(.*?\\)")
                   (insert "[[:space:]]*\\'")
                   (buffer-string)))
         (argument (make-symbol "arglist"))
         documentation
         interactive-spec)
    (when (stringp (car body))
      (setq documentation (pop body)))
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive-spec (cadr (pop body))))
    `(progn
       (defun ,fn-name (,argument &optional process target)
         ,(concat documentation
                  "\n\nNote: If PROCESS or TARGET are nil, the values given"
		  "\nby `rcirc-buffer-process' and `rcirc-target' will be used.")
         (interactive (list ,interactive-spec))
         (unless (if (listp ,argument)
                     (<= ,required (length ,argument) ,total)
                   (string-match ,regexp ,argument))
           (user-error "Malformed input (%s): %S" ',command ,argument))
         (push ,(upcase (symbol-name command)) rcirc-pending-requests)
         (let ((process (or process (rcirc-buffer-process)))
	       (target (or target rcirc-target)))
           (ignore target process)
           (let (,@(cl-loop
                    for i from 0 for arg in (delq '&optional arguments)
                    collect `(,arg (if (listp ,argument)
                                       (nth ,i ,argument)
                                     (match-string ,(1+ i) ,argument)))))
             ,@body)))
       (add-to-list 'rcirc-client-commands ,(concat "/" (symbol-name command))))))

(define-obsolete-function-alias
  'defun-rcirc-command
  'rcirc-define-command
  "28.1")

(rcirc-define-command msg (chan-or-nick message)
  "Send MESSAGE to CHAN-OR-NICK."
  (interactive (list (completing-read "Message nick: "
                                      (with-rcirc-server-buffer
					rcirc-nick-table))
                     (read-string "Message: ")))
  (rcirc-send-message process chan-or-nick message))

(rcirc-define-command query (nick)
  "Open a private chat buffer to NICK."
  (interactive (list (completing-read "Query nick: "
                                      (with-rcirc-server-buffer
                                        rcirc-nick-table))))
  (let ((existing-buffer (rcirc-get-buffer process nick)))
    (switch-to-buffer (or existing-buffer
			  (rcirc-get-buffer-create process nick)))
    (when (not existing-buffer)
      (rcirc-cmd-whois nick))))

(rcirc-define-command join (channels)
  "Join CHANNELS.
CHANNELS is a comma- or space-separated string of channel names."
  (interactive "sJoin channels: ")
  (let* ((split-channels (split-string channels "[ ,]" t))
         (buffers (mapcar (lambda (ch)
                            (rcirc-get-buffer-create process ch))
                          split-channels))
         (channels (mapconcat 'identity split-channels ",")))
    (rcirc-send-string process "JOIN" channels)
    (when (not (eq (selected-window) (minibuffer-window)))
      (dolist (b buffers) ;; order the new channel buffers in the buffer list
        (switch-to-buffer b)))))

(rcirc-define-command invite (nick channel)
  "Invite NICK to CHANNEL."
  (interactive (list
                (completing-read "Invite nick: "
				 (with-rcirc-server-buffer rcirc-nick-table))
                (read-string "Channel: ")))
  (rcirc-send-string process "INVITE" nick channel))

(rcirc-define-command part (&optional channel reason)
  "Part CHANNEL.
CHANNEL should be a string of the form \"#CHANNEL-NAME REASON\".
If omitted, CHANNEL-NAME defaults to TARGET, and REASON defaults
to `rcirc-default-part-reason'."
  (interactive "sPart channel: \nsReason: ")
  (rcirc-send-string process "PART" (or channel target)
                     : (or reason rcirc-default-part-reason)))

(rcirc-define-command quit (&optional reason)
  "Send a quit message to server with REASON."
  (interactive "sQuit reason: ")
  (rcirc-send-string process "QUIT"
                     : (or reason rcirc-default-quit-reason)))

(rcirc-define-command reconnect ()
  "Reconnect to current server."
  (interactive "i")
  (setq rcirc-failed-attempts 0)
  (rcirc-reconnect process))

(rcirc-define-command nick (nick)
  "Change nick to NICK."
  (interactive (list (read-string "New nick: ")))
  (rcirc-send-string process "NICK" nick))

(rcirc-define-command names (&optional channel)
  "Display list of names in CHANNEL or in current channel if CHANNEL is nil.
If called interactively, prompt for a channel when prefix arg is supplied."
  (interactive (list (and current-prefix-arg
                          (read-string "List names in channel: "))))
  (rcirc-send-string process "NAMES" (or channel target)))

(rcirc-define-command topic (topic)
  "List TOPIC for the TARGET channel.
With a prefix arg, prompt for new topic."
  (interactive (list (and current-prefix-arg
                          (read-string "List names in channel: "))))
  (if (> (length topic) 0)
      (rcirc-send-string process "TOPIC" target : topic)
    (rcirc-send-string process "TOPIC" target)))

(rcirc-define-command whois (nick)
  "Request information from server about NICK."
  (interactive (list (completing-read
                      "Whois: "
                      (with-rcirc-server-buffer rcirc-nick-table))))
  (rcirc-send-string process "WHOIS" nick))

(rcirc-define-command mode (nick-or-chan mode)
  "Set NICK-OR-CHAN mode to MODE."
  (interactive (list (read-string "Mode nick or channel: ")
                     (read-string "Mode: ")))
  (rcirc-send-string process "MODE" nick-or-chan mode))

(rcirc-define-command list (channels)
  "Request information on CHANNELS from server."
  (interactive "sList Channels: ")
  (rcirc-send-string process "LIST" channels))

(rcirc-define-command oper (args)
  "Send operator command to server."
  (interactive "sOper args: ")
  (rcirc-send-string process "OPER" args))

(rcirc-define-command quote (message)
  "Send MESSAGE literally to server."
  (interactive "sServer message: ")
  (rcirc-send-string process message))

(rcirc-define-command kick (nick reason)
  "Kick NICK from current channel."
  (interactive (list
                (completing-read "Kick nick: "
                                 (rcirc-channel-nicks
				  (rcirc-buffer-process)
				  rcirc-target))
                (read-from-minibuffer "Kick reason: ")))
  (rcirc-send-string process "KICK" target nick : reason))

(defun rcirc-cmd-ctcp (args &optional process _target)
  "Handle ARGS as a CTCP command.
PROCESS is the process object for the current connection."
  (if (string-match "^\\([^ ]+\\)\\s-+\\(.+\\)$" args)
      (let* ((target (match-string 1 args))
             (request (upcase (match-string 2 args)))
             (function (intern-soft (concat "rcirc-ctcp-sender-" request))))
        (if (fboundp function) ;; use special function if available
            (funcall function process target request)
          (rcirc-send-ctcp process target request)))
    (rcirc-print process (rcirc-nick process) "ERROR" nil
                 "usage: /ctcp NICK REQUEST")))

(defun rcirc-ctcp-sender-PING (process target _request)
  "Send a CTCP PING message to TARGET.
PROCESS is the process object for the current connection."
  (let ((timestamp (format-time-string "%s")))
    (rcirc-send-ctcp process target "PING" timestamp)))

(defun rcirc-cmd-me (args process target)
  "Send an action message ARGS to TARGET.
PROCESS is the process object for the current connection."
  (when target (rcirc-send-ctcp process target "ACTION" args)))

(defun rcirc-add-or-remove (set &rest elements)
  "Toggle membership of ELEMENTS in SET."
  (dolist (elt elements)
    (if (and elt (not (string= "" elt)))
	(setq set (if (member-ignore-case elt set)
		      (delete elt set)
		    (cons elt set)))))
  set)


(rcirc-define-command ignore (nick)
  "Manage the ignore list.
Ignore NICK, unignore NICK if already ignored, or list ignored
nicks when no NICK is given.  When listing ignored nicks, the
ones added to the list automatically are marked with an asterisk."
  (interactive "sToggle ignoring of nick: ")
  (setq rcirc-ignore-list
	(apply #'rcirc-add-or-remove rcirc-ignore-list
	       (split-string nick nil t)))
  (rcirc-print process nil "IGNORE" target
	       (mapconcat
		(lambda (nick)
		  (concat nick
			  (if (member nick rcirc-ignore-list-automatic)
			      "*" "")))
		rcirc-ignore-list " ")))

(rcirc-define-command bright (nick)
  "Manage the bright nick list."
  (interactive "sToggle emphasis of nick: ")
  (setq rcirc-bright-nicks
	(apply #'rcirc-add-or-remove rcirc-bright-nicks
	       (split-string nick nil t)))
  (rcirc-print process nil "BRIGHT" target
	       (mapconcat 'identity rcirc-bright-nicks " ")))

(rcirc-define-command dim (nick)
  "Manage the dim nick list."
  (interactive "sToggle deemphasis of nick: ")
  (setq rcirc-dim-nicks
	(apply #'rcirc-add-or-remove rcirc-dim-nicks
	       (split-string nick nil t)))
  (rcirc-print process nil "DIM" target
	       (mapconcat 'identity rcirc-dim-nicks " ")))

(rcirc-define-command keyword (keyword)
  "Manage the keyword list.
Mark KEYWORD, unmark KEYWORD if already marked, or list marked
keywords when no KEYWORD is given."
  (interactive "sToggle highlighting of keyword: ")
  (setq rcirc-keywords
	(apply #'rcirc-add-or-remove rcirc-keywords
	       (split-string keyword nil t)))
  (rcirc-print process nil "KEYWORD" target
	       (mapconcat 'identity rcirc-keywords " ")))


(defun rcirc-add-face (start end name &optional object)
  "Add face NAME to the face text property of the text from START to END."
  (when name
    (let ((pos start)
	  next prop)
      (while (< pos end)
	(setq prop (get-text-property pos 'font-lock-face object)
	      next (next-single-property-change pos 'font-lock-face object end))
	(unless (member name (get-text-property pos 'font-lock-face object))
	  (add-text-properties pos next
			       (list 'font-lock-face (cons name prop)) object))
	(setq pos next)))))

(defun rcirc-facify (string face)
  "Return a copy of STRING with FACE property added."
  (let ((string (or string "")))
    (rcirc-add-face 0 (length string) face string)
    string))

(defvar rcirc-url-regexp
  (eval-when-compile
    (require 'browse-url)
    browse-url-button-regexp)
  "Regexp matching URLs.  Set to nil to disable URL features in rcirc.")

;; cf cl-remove-if-not
(defun rcirc-condition-filter (condp lst)
  "Remove all items not satisfying condition CONDP in list LST.
CONDP is a function that takes a list element as argument and returns
non-nil if that element should be included.  Returns a new list."
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun rcirc-browse-url (&optional arg)
  "Prompt for URL to browse based on URLs in buffer before point.

If ARG is given, opens the URL in a new browser window."
  (interactive "P")
  (let* ((point (point))
         (filtered (rcirc-condition-filter
                    (lambda (x) (>= point (cdr x)))
                    rcirc-urls))
         (completions (mapcar (lambda (x) (car x)) filtered))
         (defaults (mapcar (lambda (x) (car x)) filtered)))
    (browse-url (completing-read "Rcirc browse-url: "
                                 completions nil nil (car defaults) nil defaults)
                arg)))

(defun rcirc-markup-timestamp (_sender _response)
  "Insert a timestamp."
  (goto-char (point-min))
  (let ((time (and-let* ((time (rcirc-get-tag "time")))
                (parse-iso8601-time-string time))))
    (insert (rcirc-facify (format-time-string rcirc-time-format time)
			  'rcirc-timestamp))))

(defun rcirc-markup-attributes (_sender _response)
  "Highlight IRC markup, indicated by ASCII control codes."
  (while (re-search-forward
          (rx (group (or #x02 #x1d #x1f #x1e #x11))
              (*? nonl)
              (group (or (backref 1) (+ #x0f) eol)))
          nil t)
    (rcirc-add-face (match-beginning 0) (match-end 0)
                    (cl-case (char-after (match-beginning 0))
                      (#x02 'bold)
                      (#x1d 'italic)
                      (#x1f 'underline)
                      (#x1e '(:strike-through t))
                      (#x11 'rcirc-monospace-text)))
    (goto-char (1+ (match-beginning 0)))))

(defconst rcirc-color-codes
  ;; Taken from https://modern.ircdocs.horse/formatting.html
  ["white" "black" "blue" "green" "red" "brown" "magenta"
   "orange" "yellow" "light green" "cyan" "light cyan"
   "light blue" "pink" "grey" "light grey"
   "#470000" "#472100" "#474700" "#324700" "#004700" "#00472c"
   "#004747" "#002747" "#000047" "#2e0047" "#470047" "#47002a"
   "#740000" "#743a00" "#747400" "#517400" "#007400" "#007449"
   "#007474" "#004074" "#000074" "#4b0074" "#740074" "#740045"
   "#b50000" "#b56300" "#b5b500" "#7db500" "#00b500" "#00b571"
   "#00b5b5" "#0063b5" "#0000b5" "#7500b5" "#b500b5" "#b5006b"
   "#ff0000" "#ff8c00" "#ffff00" "#b2ff00" "#00ff00" "#00ffa0"
   "#00ffff" "#008cff" "#0000ff" "#a500ff" "#ff00ff" "#ff0098"
   "#ff5959" "#ffb459" "#ffff71" "#cfff60" "#6fff6f" "#65ffc9"
   "#6dffff" "#59b4ff" "#5959ff" "#c459ff" "#ff66ff" "#ff59bc"
   "#ff9c9c" "#ffd39c" "#ffff9c" "#e2ff9c" "#9cff9c" "#9cffdb"
   "#9cffff" "#9cd3ff" "#9c9cff" "#dc9cff" "#ff9cff" "#ff94d3"
   "#000000" "#131313" "#282828" "#363636" "#4d4d4d" "#656565"
   "#818181" "#9f9f9f" "#bcbcbc" "#e2e2e2" "#ffffff"]
  "Vector of colors for each IRC color code.")

(defun rcirc-color-attributes (_sender _response)
  "Highlight IRC color-codes, indicated by ASCII control codes."
  (while (re-search-forward
          (rx #x03
              (? (group (= 2 digit)) (? "," (group (= 2 digit))))
              (*? nonl)
              (or #x03 #x0f eol))
          nil t)
    (let (foreground background)
      (when-let ((fg-raw (match-string 1))
                 (fg (string-to-number fg-raw))
                 ((<= 0 fg (1- (length rcirc-color-codes)))))
        (setq foreground (aref rcirc-color-codes fg)))
      (when-let ((bg-raw (match-string 2))
                 (bg (string-to-number bg-raw))
                 ((<= 0 bg (1- (length rcirc-color-codes)))))
        (setq background (aref rcirc-color-codes bg)))
      (rcirc-add-face (match-beginning 0) (match-end 0)
                           `(face (:foreground
                                   ,foreground
                                   :background
                                   ,background))))))

(defun rcirc-remove-markup-codes (_sender _response)
  "Remove ASCII control codes used to designate markup."
  (while (re-search-forward
          (rx (or #x02 #x1d #x1f #x1e #x11 #x0f
                  (: #x03 (? (= 2 digit) (? "," (= 2 digit))))))
          nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(defun rcirc-markup-my-nick (_sender response)
  "Highlight the users nick.
If RESPONSE indicates that the nick was mentioned in a message,
highlight the entire line and record the activity."
  (with-syntax-table rcirc-nick-syntax-table
    (while (re-search-forward (concat "\\b"
				      (regexp-quote (rcirc-nick
						     (rcirc-buffer-process)))
				      "\\b")
			      nil t)
      (rcirc-add-face (match-beginning 0) (match-end 0)
		      'rcirc-nick-in-message)
      (when (string= response "PRIVMSG")
	(rcirc-add-face (point-min) (point-max)
			'rcirc-nick-in-message-full-line)
	(rcirc-record-activity (current-buffer) 'nick)))))

(defun rcirc-markup-urls (_sender _response)
  "Highlight and activate URLs."
  (while (and rcirc-url-regexp ; nil means disable URL catching.
              (re-search-forward rcirc-url-regexp nil t))
    (let* ((start (match-beginning 0))
           (url   (buffer-substring-no-properties start (point))))
      (when rcirc-url-max-length
        ;; Replace match with truncated URL.
        (delete-region start (point))
        (insert (url-truncate-url-for-viewing url rcirc-url-max-length)))
      ;; Add a button for the URL.  Note that we use `make-text-button',
      ;; rather than `make-button', as text-buttons are much faster in
      ;; large buffers.
      (make-text-button start (point)
			'face 'rcirc-url
			'follow-link t
			'rcirc-url url
			'action (lambda (button)
				  (browse-url-button-open-url
                                   (button-get button 'rcirc-url))))
      ;; Record the URL if it is not already the latest stored URL.
      (unless (string= url (caar rcirc-urls))
        (push (cons url start) rcirc-urls)))))

(defun rcirc-markup-keywords (sender response)
  "Highlight keywords as specified by `rcirc-keywords'.
Keywords are only highlighted in messages (as indicated by
RESPONSE) when they were not written by the user (as indicated by
SENDER)."
  (when (and (string= response "PRIVMSG")
	     (not (string= sender (rcirc-nick (rcirc-buffer-process)))))
    (let* ((target (or rcirc-target ""))
	   (keywords (delq nil (mapcar (lambda (keyword)
					 (when (not (string-match keyword
								  target))
					   keyword))
				       rcirc-keywords))))
      (when keywords
	(while (re-search-forward (regexp-opt keywords 'words) nil t)
	  (rcirc-add-face (match-beginning 0) (match-end 0) 'rcirc-keyword)
	  (rcirc-record-activity (current-buffer) 'keyword))))))

(defun rcirc-markup-bright-nicks (_sender response)
  "Highlight nicks brightly as specified by `rcirc-bright-nicks'.
This highlighting only takes place in name lists (as indicated by
RESPONSE)."
  (when (and rcirc-bright-nicks
	     (string= response "NAMES"))
    (with-syntax-table rcirc-nick-syntax-table
      (while (re-search-forward (regexp-opt rcirc-bright-nicks 'words) nil t)
	(rcirc-add-face (match-beginning 0) (match-end 0)
			'rcirc-bright-nick)))))

(defun rcirc-markup-fill (_sender response)
  "Fill messages as configured by `rcirc-fill-column'.
MOTD messages are not filled (as indicated by RESPONSE)."
  (when (not (string= response "372")) 	; /motd
    (let ((fill-prefix
	   (or rcirc-fill-prefix
	       (make-string (- (point) (line-beginning-position)) ?\s)))
	  (fill-column (- (cond ((null rcirc-fill-column) fill-column)
                                ((functionp rcirc-fill-column)
				 (funcall rcirc-fill-column))
				(t rcirc-fill-column))
			  ;; make sure ... doesn't cause line wrapping
			  3)))
      (fill-region (point) (point-max) nil t))))

;;; handlers
;; these are called with the server PROCESS, the SENDER, which is a
;; server or a user, depending on the command, the ARGS, which is a
;; list of strings, and the TEXT, which is the original server text,
;; verbatim
(defun rcirc-handler-001 (process sender args _text)
  "Handle welcome message.
SENDER and ARGS are used to initialize the current connection.
PROCESS is the process object for the current connection."
  (rcirc-handler-generic process "001" sender args nil)
  (with-rcirc-process-buffer process
    (setq rcirc-connecting nil)
    (rcirc-reschedule-timeout process)
    (setq rcirc-server-name sender)
    (setq rcirc-nick (car args))
    (rcirc-update-prompt)
    (if (and rcirc-auto-authenticate-flag
             ;; We have to ensure that there's an authentication
             ;; entry for that server.  Otherwise,
             ;; there's no point in calling authenticate.
             (let (auth-required)
               (dolist (s rcirc-authinfo auth-required)
                 (when (string-match (car s) rcirc-server)
                   (setq auth-required t)))))
        (if rcirc-authenticate-before-join
            (progn
	      (add-hook 'rcirc-authenticated-hook 'rcirc-join-channels-post-auth t t)
              (rcirc-authenticate))
          (rcirc-authenticate)
          (rcirc-join-channels process rcirc-startup-channels))
      (rcirc-join-channels process rcirc-startup-channels))))

(defun rcirc-join-channels-post-auth (process)
  "Join `rcirc-startup-channels' after authenticating.
PROCESS is the process object for the current connection."
  (with-rcirc-process-buffer process
    (rcirc-join-channels process rcirc-startup-channels)))

(defun rcirc-handler-PRIVMSG (process sender args text)
  "Handle a (private) message from SENDER.
ARGS should have the form (TARGET MESSAGE).  TEXT is the verbatim
message as received from the server.  PROCESS is the process
object for the current connection."
  (rcirc-check-auth-status process sender args text)
  (let ((target (if (rcirc-channel-p (car args))
                    (car args)
                  sender))
        (message (or (cadr args) "")))
    (if (string-match "^\C-a\\(.*\\)\C-a$" message)
        (rcirc-handler-CTCP process target sender (match-string 1 message))
      (rcirc-print process sender "PRIVMSG" target message t))
    ;; update nick linestamp
    (with-current-buffer (rcirc-get-buffer process target t)
      (rcirc-put-nick-channel process sender target rcirc-current-line))))

(defun rcirc-handler-NOTICE (process sender args text)
  "Handle a notice message from SENDER.
ARGS should have the form (TARGET MESSAGE).
TEXT is the verbatim message as received from the server.
PROCESS is the process object for the current connection."
  (rcirc-check-auth-status process sender args text)
  (let ((target (car args))
        (message (cadr args)))
    (if (string-match "^\C-a\\(.*\\)\C-a$" message)
        (rcirc-handler-CTCP-response process target sender
				     (match-string 1 message))
      (rcirc-print process sender "NOTICE"
		   (cond ((rcirc-channel-p target)
			  target)
                         ;; -ChanServ- [#gnu] Welcome...
			 ((string-match "\\[\\(#[^] ]+\\)\\]" message)
			  (match-string 1 message))
			 (sender
			  (if (string= sender (rcirc-server-name process))
			      nil	; server notice
			    sender)))
                 message t))))

(defun rcirc-check-auth-status (process sender args _text)
  "Check if the user just authenticated.
If authenticated, runs `rcirc-authenticated-hook' with PROCESS as
the only argument.  ARGS should have the form (TARGET MESSAGE).
SENDER is used the determine the authentication method.  PROCESS
is the process object for the current connection."
  (with-rcirc-process-buffer process
    (when (and (not rcirc-user-authenticated)
               rcirc-authenticate-before-join
               rcirc-auto-authenticate-flag)
      (let ((target (car args))
            (message (cadr args)))
        (when (or
               (and ;; nickserv
                (string= sender "NickServ")
                (string= target rcirc-nick)
                (cl-member
                 message
                 (list
                  (format "You are now identified for \C-b%s\C-b." rcirc-nick)
                  (format "You are successfully identified as \C-b%s\C-b."
                          rcirc-nick)
                  "Password accepted - you are now recognized.")
                 ;; The nick may have a different case, so match
                 ;; case-insensitively (Bug#39345).
                 :test #'cl-equalp))
               (and ;; quakenet
                (string= sender "Q")
                (string= target rcirc-nick)
                (string-match "\\`You are now logged in as .+\\.\\'" message)))
          (setq rcirc-user-authenticated t)
          (run-hook-with-args 'rcirc-authenticated-hook process)
          (remove-hook 'rcirc-authenticated-hook 'rcirc-join-channels-post-auth t))))))

(defun rcirc-handler-WALLOPS (process sender args _text)
  "Handle WALLOPS message from SENDER.
ARGS should have the form (MESSAGE).
PROCESS is the process object for the current
connection."
  (rcirc-print process sender "WALLOPS" sender (car args) t))

(defun rcirc-handler-JOIN (process sender args _text)
  "Handle JOIN message from SENDER.
ARGS should have the form (CHANNEL).
PROCESS is the process object for the current
connection."
  (let ((channel (car args)))
    (with-current-buffer (rcirc-get-buffer-create process channel)
      ;; when recently rejoining, restore the linestamp
      (rcirc-put-nick-channel process sender channel
			      (let ((last-activity-lines
				     (rcirc-elapsed-lines process sender channel)))
				(when (and last-activity-lines
					   (< last-activity-lines rcirc-omit-threshold))
                                  (rcirc-last-line process sender channel))))
      ;; reset mode-line-process in case joining a channel with an
      ;; already open buffer (after getting kicked e.g.)
      (setq mode-line-process nil))

    (rcirc-print process sender "JOIN" (funcall rcirc-channel-filter channel) "")

    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) sender)
      (rcirc-print process sender "JOIN" sender (funcall rcirc-channel-filter channel)))))

;; PART and KICK are handled the same way
(defun rcirc-handler-PART-or-KICK (process _response channel _sender nick _args)
  "Remove NICK from CHANNEL.
PROCESS is the process object for the current connection."
  (rcirc-ignore-update-automatic nick)
  (if (not (string= nick (rcirc-nick process)))
      ;; this is someone else leaving
      (progn
	(rcirc-maybe-remember-nick-quit process nick channel)
	(rcirc-remove-nick-channel process nick channel))
    ;; this is us leaving
    (mapc (lambda (n)
	    (rcirc-remove-nick-channel process n channel))
	  (rcirc-channel-nicks process channel))

    ;; if the buffer is still around, make it inactive
    (let ((buffer (rcirc-get-buffer process channel)))
      (when buffer
	(rcirc-disconnect-buffer buffer)))))

(defun rcirc-handler-PART (process sender args _text)
  "Handle PART message from SENDER.
ARGS should have the form (CHANNEL REASON).
PROCESS is the process object for the current connection."
  (let* ((channel (car args))
	 (reason (cadr args))
	 (message (concat channel " " reason)))
    (rcirc-print process sender "PART" (funcall rcirc-channel-filter channel) message)
    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) sender)
      (rcirc-print process sender "PART" (funcall rcirc-channel-filter channel) message))

    (rcirc-handler-PART-or-KICK process "PART" channel sender sender reason)))

(defun rcirc-handler-KICK (process sender args _text)
  "Handle PART message from SENDER.
ARGS should have the form (CHANNEL NICK REASON).
PROCESS is the process object for the current connection."
  (let* ((channel (car args))
	 (nick (cadr args))
	 (reason (nth 2 args))
	 (message (concat nick " " channel " " reason)))
    (rcirc-print process sender "KICK" (funcall rcirc-channel-filter channel) message t)
    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) nick)
      (rcirc-print process sender "KICK" nick message))

    (rcirc-handler-PART-or-KICK process "KICK" channel sender nick reason)))

(defun rcirc-maybe-remember-nick-quit (process nick channel)
  "Remember NICK as leaving CHANNEL if they recently spoke.
PROCESS is the process object for the current connection."
  (let ((elapsed-lines (rcirc-elapsed-lines process nick channel)))
    (when (and elapsed-lines
	       (< elapsed-lines rcirc-omit-threshold))
      (let ((buffer (rcirc-get-buffer process channel)))
	(when buffer
	  (with-current-buffer buffer
	    (let ((record (assoc-string nick rcirc-recent-quit-alist t))
		  (line (rcirc-last-line process nick channel)))
	      (if record
		  (setcdr record line)
		(setq rcirc-recent-quit-alist
		      (cons (cons nick line)
			    rcirc-recent-quit-alist))))))))))

(defun rcirc-handler-QUIT (process sender args _text)
  "Handle QUIT message from SENDER.
PROCESS is the process object for the current connection."
  (rcirc-ignore-update-automatic sender)
  (mapc (lambda (channel)
	  ;; broadcast quit message each channel
	  (rcirc-print process sender "QUIT" (funcall rcirc-channel-filter channel) (apply 'concat args))
	  ;; record nick in quit table if they recently spoke
	  (rcirc-maybe-remember-nick-quit process sender channel))
	(rcirc-nick-channels process sender))
  (rcirc-nick-remove process sender))

(defun rcirc-handler-NICK (process sender args _text)
  "Handle NICK message from SENDER.
ARGS should have the form (NEW-NICK).
PROCESS is the process object for the current connection."
  (let* ((old-nick sender)
         (new-nick (car args))
         (channels (rcirc-nick-channels process old-nick)))
    ;; update list of ignored nicks
    (rcirc-ignore-update-automatic old-nick)
    (when (member old-nick rcirc-ignore-list)
      (add-to-list 'rcirc-ignore-list new-nick)
      (add-to-list 'rcirc-ignore-list-automatic new-nick))
    ;; print message to nick's channels
    (dolist (target channels)
      (rcirc-print process sender "NICK" target new-nick))
    ;; update chat buffer, if it exists
    (when-let ((chat-buffer (rcirc-get-buffer process old-nick)))
      (with-current-buffer chat-buffer
	(rcirc-print process sender "NICK" old-nick new-nick)
	(setq rcirc-target new-nick)
	(rename-buffer (rcirc-generate-new-buffer-name process new-nick)))
      (setf rcirc-buffer-alist
            (cons (cons new-nick chat-buffer)
                  (delq (assoc-string old-nick rcirc-buffer-alist t)
                        rcirc-buffer-alist))))
    ;; remove old nick and add new one
    (with-rcirc-process-buffer process
      (let ((v (gethash old-nick rcirc-nick-table)))
        (remhash old-nick rcirc-nick-table)
        (puthash new-nick v rcirc-nick-table))
      ;; if this is our nick...
      (when (string= old-nick rcirc-nick)
        (setq rcirc-nick new-nick)
	(rcirc-update-prompt t)
        ;; reauthenticate
        (when rcirc-auto-authenticate-flag (rcirc-authenticate))))))

(defun rcirc-handler-PING (process _sender args _text)
  "Respond to a PING with a PONG.
ARGS should have the form (MESSAGE).  MESSAGE is relayed back to
the server.  PROCESS is the process object for the current
connection."
  (rcirc-send-string process "PONG" : (car args)))

(defun rcirc-handler-PONG (_process _sender _args _text)
  "Ignore all incoming PONG messages.")

(defun rcirc-handler-TOPIC (process sender args _text)
  "Note the topic change from SENDER.
PROCESS is the process object for the current connection."
  (let ((topic (cadr args)))
    (rcirc-print process sender "TOPIC" (car args) topic)
    (with-current-buffer (rcirc-get-buffer process (car args))
      (setq rcirc-topic topic))))

(defvar rcirc-nick-away-alist nil
  "Alist from nicks to away messages.")

(defun rcirc-handler-301 (process _sender args text)
  "Handle away messages (RPL_AWAY).
ARGS should have the form (NICK AWAY-MESSAGE).
PROCESS is the process object for the current connection."
  (let* ((nick (cadr args))
	 (rec (assoc-string nick rcirc-nick-away-alist))
	 (away-message (nth 2 args)))
    (when (or (not rec)
	      (not (string= (cdr rec) away-message)))
      ;; away message has changed
      (rcirc-handler-generic process "AWAY" nick (cdr args) text)
      (if rec
	  (setcdr rec away-message)
	(setq rcirc-nick-away-alist (cons (cons nick away-message)
					  rcirc-nick-away-alist))))))

(defun rcirc-handler-317 (process sender args _text)
  "Handle idle messages from SENDER (RPL_WHOISIDLE).
ARGS should have the form (NICK IDLE-SECS SIGNON-TIME).
PROCESS is the process object for the current connection."
  (let* ((nick (nth 1 args))
         (idle-secs (string-to-number (nth 2 args)))
         (idle-string (format-seconds "%yy %dd %hh %mm %z%ss" idle-secs))
	 (signon-time (string-to-number (nth 3 args)))
         (signon-string (format-time-string "%c" signon-time))
         (message (format "%s idle for %s, signed on %s"
                          nick idle-string signon-string)))
    (rcirc-print process sender "317" nil message t)))

(defun rcirc-handler-332 (process _sender args _text)
  "Update topic when notified by server (RPL_TOPIC).
ARGS should have the form (CHANNEL TOPIC).
PROCESS is the process object for the current connection."
  (let ((buffer (or (rcirc-get-buffer process (cadr args))
		    (rcirc-get-temp-buffer-create process (cadr args)))))
    (with-current-buffer buffer
      (setq rcirc-topic (nth 2 args)))))

(defun rcirc-handler-333 (process sender args _text)
  "Update when and who set the current topic.
ARGS has the form (CHANNEL SETTER TIME).  SENDER is passed on to
`rcirc-print'.  PROCESS is the process object for the current
connection.  This is a non-standard extension, not specified in
RFC1459."
  (let ((buffer (or (rcirc-get-buffer process (cadr args))
		    (rcirc-get-temp-buffer-create process (cadr args)))))
    (with-current-buffer buffer
      (let ((setter (nth 2 args))
	    (time (current-time-string
		   (string-to-number (cadddr args)))))
	(rcirc-print process sender "TOPIC" (cadr args)
		     (format "%s (%s on %s)" rcirc-topic setter time))))))

(defun rcirc-handler-477 (process sender args _text)
  "Notify user that CHANNEL does not support modes (ERR_NOCHANMODES).
ARGS has the form (CHANNEL MESSAGE).  SENDER is passed on to
`rcirc-print'.  PROCESS is the process object for the current
connection."
  (rcirc-print process sender "477" (cadr args) (nth 2 args)))

(defun rcirc-handler-MODE (process sender args _text)
  "Handle MODE messages.
ARGS should have the form (TARGET . MESSAGE-LIST).
SENDER is passed on to `rcirc-print'.
PROCESS is the process object for the current connection."
  (let ((target (car args))
        (msg (mapconcat 'identity (cdr args) " ")))
    (rcirc-print process sender "MODE"
                 (if (string= target (rcirc-nick process))
                     nil
                   target)
                 msg)

    ;; print in private chat buffers if they exist
    (mapc (lambda (nick)
	    (when (rcirc-get-buffer process nick)
	      (rcirc-print process sender "MODE" nick msg)))
	  (cddr args))))

(defun rcirc-get-temp-buffer-create (process channel)
  "Return a buffer based on PROCESS and CHANNEL."
  (let ((tmpnam (concat " " (downcase channel) "TMP" (process-name process))))
    (get-buffer-create tmpnam)))

(defun rcirc-handler-353 (process _sender args _text)
  "Start handling list of users (RPL_NAMREPLY).
ARGS should have the form (TYPE CHANNEL . NICK-LIST).
PROCESS is the process object for the current connection."
  (let ((channel (nth 2 args))
	(names (or (nth 3 args) "")))
    (mapc (lambda (nick)
            (rcirc-put-nick-channel process nick channel))
          (split-string names " " t))
    ;; create a temporary buffer to insert the names into
    ;; rcirc-handler-366 (RPL_ENDOFNAMES) will handle it
    (with-current-buffer (rcirc-get-temp-buffer-create process channel)
      (goto-char (point-max))
      (insert (car (last args)) " "))))

(defun rcirc-handler-366 (process sender args _text)
  "Handle end of user list (RPL_ENDOFNAMES).
SENDER is passed on to `rcirc-print'.
PROCESS is the process object for the current connection."
  (let* ((channel (cadr args))
         (buffer (rcirc-get-temp-buffer-create process channel)))
    (with-current-buffer buffer
      (rcirc-print process sender "NAMES" channel
                   (let ((content (buffer-substring (point-min) (point-max))))
		     (rcirc-sort-nicknames-join content " "))))
    (kill-buffer buffer)))

(defun rcirc-handler-433 (process sender args text)
  "Warn user that nick is used (ERR_NICKNAMEINUSE).
ARGS should have the form (NICK CHANNEL WARNING).
SENDER is passed on to `rcirc-handler-generic'.
PROCESS is the process object for the current connection."
  (rcirc-handler-generic process "433" sender args text)
  (with-rcirc-process-buffer process
    (let* ((length (string-to-number
                    (or (rcirc-server-parameter-value 'nicklen)
                        "16"))))
      (rcirc-cmd-nick (rcirc--make-new-nick (cadr args) length) nil process))))

(defun rcirc--make-new-nick (nick length)
  "Attempt to create a unused nickname out of NICK.
A new nick may at most be LENGTH characters long.  If we already
have some ` chars at the end, then shorten the non-` bit of the
name."
  (when (= (length nick) length)
    (setq nick (replace-regexp-in-string "[^`]\\(`+\\)\\'" "\\1" nick)))
  (concat
   (if (>= (length nick) length)
       (substring nick 0 (1- length))
     nick)
   "`"))

(defun rcirc-handler-005 (process sender args text)
  "Register supported server features (RPL_ISUPPORT).
ARGS should be a list of string feature parameters, either of the
form \"PARAMETER\" to enable a feature, \"PARAMETER=VALUE\" to
configure a specific option or \"-PARAMETER\" to disable a
previously specified feature.  SENDER is passed on to
`rcirc-handler-generic'.  PROCESS is the process object for the
current connection.  Note that this is not the behaviour as
specified in RFC2812, where 005 stood for RPL_BOUNCE."
  (rcirc-handler-generic process "005" sender args text)
  (with-rcirc-process-buffer process
    (setq rcirc-server-parameters (append rcirc-server-parameters args))))

(defun rcirc-authenticate ()
  "Send authentication to process associated with current buffer.
Passwords are stored in `rcirc-authinfo' (which see)."
  (interactive)
  (with-rcirc-server-buffer
    (dolist (i rcirc-authinfo)
      (let ((process (rcirc-buffer-process))
	    (server (car i))
	    (nick (nth 2 i))
	    (method (cadr i))
	    (args (cdddr i)))
	(when (and (string-match server rcirc-server))
          (if (and (memq method '(nickserv chanserv bitlbee))
                   (string-match nick rcirc-nick))
              ;; the following methods rely on the user's nickname.
              (cl-case method
                (nickserv
                 (rcirc-send-privmsg
                  process
                  (or (cadr args) "NickServ")
                  (concat "IDENTIFY " (car args))))
                (chanserv
                 (rcirc-send-privmsg
                  process
                  "ChanServ"
                  (format "IDENTIFY %s %s" (car args) (cadr args))))
                (bitlbee
                 (rcirc-send-privmsg
                  process
                  "&bitlbee"
                  (concat "IDENTIFY " (car args))))
                (sasl nil))
            ;; quakenet authentication doesn't rely on the user's nickname.
            ;; the variable `nick' here represents the Q account name.
            (when (eq method 'quakenet)
              (rcirc-send-privmsg
               process
               "Q@CServe.quakenet.org"
               (format "AUTH %s %s" nick (car args))))))))))

(defun rcirc-handler-INVITE (process sender args _text)
  "Notify user of an invitation from SENDER.
ARGS should have the form (TARGET CHANNEL).  PROCESS is the
process object for the current connection."
  (let ((self (buffer-local-value 'rcirc-nick rcirc-process))
        (target (car args))
        (chan (cadr args)))
    ;; `rcirc-channel-filter' is not used here because joining
    ;; requires an unfiltered name.
    (if (string= target self)
        (rcirc-print process sender "INVITE" nil
                     (format "%s invited you to %s"
                             sender chan)
                     t)
      (rcirc-print process sender "INVITE" chan
                   (format "%s invited %s"
                           sender target)
                   t))))

(defun rcirc-handler-ERROR (process sender args _text)
  "Print a error message.
SENDER and ARGS (in concatenated form) are passed on to
`rcirc-print'.  PROCESS is the process object for the current
connection."
  (rcirc-print process sender "ERROR" nil (mapconcat 'identity args " ")))

(defun rcirc-handler-CTCP (process target sender text)
  "Handle Client-To-Client-Protocol message TEXT.
The message is addressed from SENDER to TARGET.  Attempt to find
an appropriate handler, by invoicing the function
`rcirc-handler-ctcp-REQUEST', where REQUEST is the message type
as extracted from TEXT.  If no handler was found, an error
message will be printed.  PROCESS is the process object for the
current connection."
  (if (string-match "^\\([^ ]+\\) *\\(.*\\)$" text)
      (let* ((request (upcase (match-string 1 text)))
             (args (match-string 2 text))
             (handler (intern-soft (concat "rcirc-handler-ctcp-" request))))
        (if (not (fboundp handler))
            (rcirc-print process sender "ERROR" target
                         (format "%s sent unsupported ctcp: %s" sender text)
			 t)
          (funcall handler process target sender args)
          (unless (or (string= request "ACTION")
		      (string= request "KEEPALIVE"))
              (rcirc-print process sender "CTCP" target
			   (format "%s" text) t))))))

(defun rcirc-handler-ctcp-VERSION (process _target sender _message)
  "Handle a CTCP VERSION message from SENDER.
PROCESS is the process object for the current connection."
  (rcirc-send-string process "NOTICE" sender :
                     (rcirc-ctcp-wrap "VERSION" rcirc-id-string)))

(defun rcirc-handler-ctcp-ACTION (process target sender message)
  "Handle a CTCP ACTION MESSAGE from SENDER to TARGET.
PROCESS is the process object for the current connection."
  (rcirc-print process sender "ACTION" target message t))

(defun rcirc-handler-ctcp-TIME (process _target sender _message)
  "Respond to CTCP TIME message from SENDER.
PROCESS is the process object for the current connection."
  (rcirc-send-string process "NOTICE" sender :
                     (rcirc-ctcp-wrap "TIME" (current-time-string))))

(defun rcirc-handler-CTCP-response (process _target sender message)
  "Handle CTCP response MESSAGE from SENDER.
PROCESS is the process object for the current connection."
  (rcirc-print process sender "CTCP" nil message t))


(defun rcirc-handler-CAP (process _sender args _text)
  "Handle capability negotiation messages.
ARGS should have the form (USER SUBCOMMAND . ARGUMENTS).  PROCESS
is the process object for the current connection."
  (with-rcirc-process-buffer process
    (let ((subcmd (cadr args)))
      (dolist (cap (cddr args))
        (cond ((string= subcmd "ACK")
               (push (intern (downcase cap)) rcirc-acked-capabilities)
               (setq rcirc-requested-capabilities
                     (delete cap rcirc-requested-capabilities)))
              ((string= subcmd "NAK")
               (setq rcirc-requested-capabilities
                     (delete cap rcirc-requested-capabilities))))))
    (when (and (null rcirc-requested-capabilities) rcirc-finished-sasl)
      ;; All requested capabilities have been responded to
      (rcirc-send-string process "CAP" "END"))))

(defun rcirc-handler-TAGMSG (process sender _args _text)
  "Handle a empty tag message from SENDER.
PROCESS is the process object for the current connection."
  (dolist (tag rcirc-message-tags)
    (when-let ((handler (intern-soft (concat "rcirc-tag-handler-" (car tag))))
               ((fboundp handler)))
      (funcall handler process sender (cdr tag)))))

(defun rcirc-handler-BATCH (process _sender args _text)
  "Open or close a batch.
ARGS should have the form (tag type . parameters) when starting a
batch, or (tag) when closing a batch.  PROCESS is the process
object for the current connection."
  (with-rcirc-process-buffer process
    (let ((type (cadr args))
          (id (substring (car args) 1)))
      (cond
       ((= (aref (car args) 0) ?+)      ;start a new batch
        (when (assoc id rcirc-batch-attributes)
          (error "Starting batch with already used ID"))
        (setf (alist-get id rcirc-batch-attributes nil nil #'string=)
              (cons type (cddr args))))
       ((= (aref (car args) 0) ?-)      ;close a batch
        (unless (assoc id rcirc-batch-attributes)
          (error "Closing a unknown batch"))
        (let ((type (car (alist-get id rcirc-batch-attributes
                                    nil nil #'string=))))
          (when (eq (car (alist-get type rcirc-supported-batch-types
                                    nil nil #'string=))
                    'deferred)
            (let ((messages (alist-get id rcirc-batched-messages
                                       nil nil #'string=))
                  (bhandler (intern-soft (concat "rcirc-batch-handler-" type))))
              (if (fboundp bhandler)
                  (funcall bhandler process id (nreverse messages))
                (dolist (message (nreverse messages))
                  (let ((cmd (nth 0 message))
                        (process (nth 1 message))
                        (sender (nth 2 message))
                        (args (nth 3 message))
                        (text (nth 4 message))
                        (rcirc-message-tags (nth 5 message)))
                    (if-let (handler (intern-soft (concat "rcirc-handler-" cmd)))
                        (funcall handler process sender args text)
                      (rcirc-handler-generic process cmd sender args text))))))))
        (setq rcirc-batch-attributes
              (delq (assoc id rcirc-batch-attributes)
                    rcirc-batch-attributes)
              rcirc-batched-messages
              (delq (assoc id rcirc-batched-messages)
                    rcirc-batched-messages)))))))

(defun rcirc-handler-AUTHENTICATE (process _cmd _args _text)
  "Respond to authentication request.
PROCESS is the process object for the current connection."
  (rcirc-send-string
   process
   "AUTHENTICATE"
   (base64-encode-string
    ;; use connection user-name
    (concat "\0" (nth 3 rcirc-connection-info)
            "\0" (rcirc-get-server-password rcirc-server)))))

(defun rcirc-handler-900 (process sender args _text)
  "Respond to a successful authentication response.
SENDER is passed on to `rcirc-handler-generic'.  PROCESS is the
process object for the current connection."
  (rcirc-handler-generic process "900" sender args nil)
  (when (not rcirc-finished-sasl)
    (setq-local rcirc-finished-sasl t)
    (rcirc-send-string process "CAP" "END"))
  (rcirc-join-channels-post-auth process))

(defun rcirc-handler-FAIL (process _sender args _text)
  "Display a FAIL message, as indicated by ARGS.
PROCESS is the process object for the current connection."
  (rcirc-print process nil "FAIL" nil
               (mapconcat #'identity args " ")
               t))

(defun rcirc-handler-WARN (process _sender args _text)
  "Display a WARN message, as indicated by ARGS.
PROCESS is the process object for the current connection."
  (rcirc-print process nil "WARN" nil
               (mapconcat #'identity args " ")
               t))

(defun rcirc-handler-NOTE (process _sender args _text)
  "Display a NOTE message, as indicated by ARGS.
PROCESS is the process object for the current connection."
  (rcirc-print process nil "NOTE" nil
               (mapconcat #'identity args " ")
               t))


(defgroup rcirc-faces nil
  "Faces for rcirc."
  :group 'rcirc
  :group 'faces)

(defface rcirc-monospace-text
  '((t :family "Monospace"))
  "Face used for monospace text in messages.")

(defface rcirc-my-nick			; font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Blue")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :inverse-video t :weight bold))
  "Rcirc face for my messages.")

(defface rcirc-other-nick	     ; font-lock-variable-name-face
  '((((class grayscale) (background light))
     :foreground "Gray90" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "DimGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 88) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 8)) :foreground "yellow" :weight light)
    (t :weight bold :slant italic))
  "Rcirc face for other users' messages.")

(defface rcirc-bright-nick
  '((((class grayscale) (background light))
     :foreground "LightGray" :weight bold :underline t)
    (((class grayscale) (background dark))
     :foreground "Gray50" :weight bold :underline t)
    (((class color) (min-colors 88) (background light)) :foreground "CadetBlue")
    (((class color) (min-colors 88) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 16) (background light)) :foreground "CadetBlue")
    (((class color) (min-colors 16) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 8)) :foreground "magenta")
    (t :weight bold :underline t))
  "Rcirc face for nicks matched by `rcirc-bright-nicks'.")

(defface rcirc-dim-nick
  '((t :inherit default))
  "Rcirc face for nicks in `rcirc-dim-nicks'.")

(defface rcirc-server			; font-lock-comment-face
  '((((class grayscale) (background light))
     :foreground "DimGray" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "LightGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light))
     :foreground "Firebrick")
    (((class color) (min-colors 88) (background dark))
     :foreground "chocolate1")
    (((class color) (min-colors 16) (background light))
     :foreground "red")
    (((class color) (min-colors 16) (background dark))
     :foreground "red1")
    (((class color) (min-colors 8) (background light)))
    (((class color) (min-colors 8) (background dark)))
    (t :weight bold :slant italic))
  "Rcirc face for server messages.")

(defface rcirc-server-prefix	 ; font-lock-comment-delimiter-face
  '((default :inherit rcirc-server)
    (((class grayscale)))
    (((class color) (min-colors 16)))
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "red1"))
  "Rcirc face for server prefixes.")

(defface rcirc-timestamp
  '((t :inherit default))
  "Rcirc face for timestamps.")

(defface rcirc-nick-in-message		; font-lock-keyword-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark)) :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "Purple")
    (((class color) (min-colors 88) (background dark))  :foreground "Cyan1")
    (((class color) (min-colors 16) (background light)) :foreground "Purple")
    (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
    (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
    (t :weight bold))
  "Rcirc face for instances of your nick within messages.")

(defface rcirc-nick-in-message-full-line '((t :weight bold))
  "Rcirc face for emphasizing the entire message when your nick is mentioned.")

(defface rcirc-prompt			; comint-highlight-prompt
  '((((min-colors 88) (background dark)) :foreground "cyan1")
    (((background dark)) :foreground "cyan")
    (t :foreground "dark blue"))
  "Rcirc face for prompts.")

(defface rcirc-track-nick
  '((((type tty)) :inherit default)
    (t :inverse-video t))
  "Rcirc face used in the mode-line when your nick is mentioned.")

(defface rcirc-track-keyword '((t :weight bold))
  "Rcirc face used in the mode-line when keywords are mentioned.")

(defface rcirc-url '((t :weight bold))
  "Rcirc face used to highlight urls.")

(defface rcirc-keyword '((t :inherit highlight))
  "Rcirc face used to highlight keywords.")


;; When using M-x flyspell-mode, only check words after the prompt
(put 'rcirc-mode 'flyspell-mode-predicate 'rcirc-looking-at-input)
(defun rcirc-looking-at-input ()
  "Return non-nil if point is past the input marker."
  (>= (point) rcirc-prompt-end-marker))


(defun rcirc-server-parameter-value (parameter)
  "Traverse `rcirc-server-parameters' for PARAMETER."
  (cl-loop for elem in rcirc-server-parameters
           for setting = (split-string elem "=")
           when (and (= (length setting) 2)
                     (string-equal (downcase (car setting)) parameter))
           return (cadr setting)))

(provide 'rcirc)

;;; rcirc.el ends here
