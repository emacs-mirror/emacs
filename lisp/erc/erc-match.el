;;; erc-match.el --- Highlight messages matching certain regexps  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2026 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm
;; URL: https://www.emacswiki.org/emacs/ErcMatch

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

;; This file includes stuff to work with pattern matching in ERC.  If
;; you were used to customizing erc-fools, erc-keywords, erc-pals,
;; erc-dangerous-hosts and the like, this file contains these
;; customizable variables.

;; Usage:
;; Put (erc-match-mode 1) into your init file.

;;; Code:

(require 'erc)

;; Customization:

(defgroup erc-match nil
  "Keyword and Friend/Foe/... recognition.
Group containing all things concerning pattern matching in ERC
messages."
  :group 'erc)

;;;###autoload(autoload 'erc-match-mode "erc-match")
(define-erc-module match nil
  "This mode checks whether messages match certain patterns.  If so,
they are hidden or highlighted.  This is controlled via the variables
`erc-pals', `erc-fools', `erc-keywords', `erc-dangerous-hosts', and
`erc-current-nick-highlight-type'.  For all these highlighting types,
you can decide whether the entire message or only the sending nick is
highlighted."
  ((add-hook 'erc-insert-modify-hook #'erc-match-message 50)
   (add-hook 'erc-mode-hook #'erc-match--setup)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-match--setup))
   (add-hook 'erc-insert-post-hook #'erc-match--on-insert-post 50)
   (erc--modify-local-map t "C-c C-k" #'erc-go-to-log-matches-buffer))
  ((remove-hook 'erc-insert-modify-hook #'erc-match-message)
   (remove-hook 'erc-insert-post-hook #'erc-match--on-insert-post)
   (remove-hook 'erc-mode-hook #'erc-match--setup)
   (erc-buffer-do #'erc-match--setup)
   (erc--modify-local-map nil "C-c C-k" #'erc-go-to-log-matches-buffer)))

;; This caches the result of applying `regexp-opt' analogs to the
;; regexp-based user options, mainly for history playback bursts.
(defvar erc-match--opt-pat-cache nil
  "Alist of (COMPUTE-FN . PAIRS) where PAIRS is an alist of (IN . OUT).")

(defun erc-match--opt-pat-custom-set (sym val &optional _)
  "Assign VAL to SYM via `set'."
  (when erc-match--opt-pat-cache
    (setq erc-match--opt-pat-cache nil))
  (set sym val))


;; Remaining customizations

(defcustom erc-pals nil
  "List of pals on IRC."
  :set #'erc-match--opt-pat-custom-set
  :type '(repeat regexp))

(defcustom erc-fools nil
  "List of fools on IRC."
  :set #'erc-match--opt-pat-custom-set
  :type '(repeat regexp))

(defcustom erc-keywords nil
  "List of keywords to highlight in all incoming messages.
Each entry in the list is either a regexp, or a cons cell with the
regexp in the car and the face to use in the cdr.  If no face is
specified, `erc-keyword-face' is used."
  :set #'erc-match--opt-pat-custom-set
  :type '(repeat (choice regexp
			 (list regexp face))))

(defcustom erc-dangerous-hosts nil
  "List of regexps for hosts to highlight.
Useful to mark nicks from dangerous hosts."
  :set #'erc-match--opt-pat-custom-set
  :type '(repeat regexp))

(defcustom erc-current-nick-highlight-type 'keyword
  "Determine how to highlight text in which your current nickname appears
\(does not apply to text sent by you).

The following values are allowed:

 nil               - do not highlight the message at all
 `keyword'         - highlight all instances of current nickname in message
 `nick'            - highlight the nick of the user who typed your nickname
 `nick-or-keyword' - highlight the nick of the user who typed your nickname,
                     or all instances of the current nickname if there was
                     no sending user
 `message'         - highlight the entire message where current nickname occurs
 `all'             - highlight the entire message (including the nick) where
                     current nickname occurs

Any other value disables highlighting of current nickname altogether."
  :type '(choice (const nil)
		 (const nick)
		 (const keyword)
		 (const nick-or-keyword)
                 (const message)
		 (const all)))

(defcustom erc-pal-highlight-type 'nick
  "Determines how to highlight messages by pals.
See `erc-pals'.

The following values are allowed:

    nil       - do not highlight the message at all
    `nick'    - highlight pal's nickname only
    \\+`message' - highlight the full message body from a matching pal
    `all'     - highlight the entire message (including the nick)
                from pal

    `nick-or-mention' - highlight a matching speaker or all matching
                        mentions as quasi keywords

A value of `nick' only highlights a matching sender's nick in the
bracketed speaker portion of the message.  A value of \\+`message'
basically highlights its complement: the message-body alone, after the
speaker tag.  A value of `nick-or-mention' works like `nick' but also
matches \"mentions,\" which `erc-fool-highlight-type' explains in its
doc string.  All values for this option require a matching sender to be
an actual user on the network \(or a bot/service) as opposed to a host
name, such as that of the server itself \(e.g. \"irc.gnu.org\").  When
patterns from other user-based categories \(namely, \\+`fool' and
\\+`dangerous-host') also match, the behavior is undefined.  However, in
ERC 5.6, `erc-dangerous-host-face' is known to clobber `erc-fool-face',
which in turn clobbers `erc-pal-face'.  \(Other effects, such as
\\+`fool'-related invisibility may not survive such collisions.)"
  :type '(choice (const nil)
		 (const nick)
                 (const nick-or-mention)
                 (const message)
		 (const all)))

(defcustom erc-fool-highlight-type 'nick
  "Determines how to highlight messages by fools.
Unlike with the \\+`pal' and \\+`dangerous-host' categories, ERC doesn't
only attempt to match associated patterns (here, from `erc-fools')
against a message's sender, it also checks for matches in traditional
IRC-style \"mentions\" in which a speaker addresses a USER directly:

  <speaker> USER: hi.
  <speaker> USER, hi.

See `erc-pal-highlight-type' for a summary of possible values and
additional details common to categories like \\+`fool' that normally
match against a message's sender."
  :type '(choice (const nil)
		 (const nick)
                 (const nick-or-mention)
                 (const message)
		 (const all)))

(defcustom erc-keyword-highlight-type 'keyword
  "Determines how to highlight messages containing keywords.
See variable `erc-keywords'.

The following values are allowed:

    `keyword' - highlight keyword only
    `message' - highlight the entire message containing keyword
    `all'     - highlight the entire message (including the nick)
                containing keyword

Any other value disables keyword highlighting altogether."
  :type '(choice (const nil)
		 (const keyword)
                 (const message)
		 (const all)))

(defcustom erc-dangerous-host-highlight-type 'nick
  "Determines how to highlight messages by nicks from dangerous-hosts.
Use option `erc-dangerous-hosts' to specify patterns.  See
`erc-pal-highlight-type' for a summary of possible values as well as
additional details common to categories like \\+`dangerous-host' that
normally match against a message's sender."
  :type '(choice (const nil)
		 (const nick)
                 (const nick-or-mention)
                 (const message)
		 (const all)))


(defcustom erc-log-matches-types-alist '((keyword . "ERC Keywords"))
  "Alist telling ERC where to log which match types.
Valid match type keys are:
- keyword
- pal
- dangerous-host
- fool
- current-nick

The other element of each cons pair in this list is the buffer name to
use for the logged message."
  :type '(repeat (cons (choice :tag "Key"
			       (const keyword)
			       (const pal)
			       (const dangerous-host)
			       (const fool)
			       (const current-nick))
		       (string :tag "Buffer name"))))

(defcustom erc-log-matches-flag 'away
  "Flag specifying when matched message logging should happen.
When nil, don't log any matched messages.
When t, log messages.
When `away', log messages only when away."
  :type '(choice (const nil)
		 (const away)
		 (const t)))

(defcustom erc-log-match-format "%t<%n:%c> %m"
  "Format for matched Messages.
This variable specifies how messages in the corresponding log buffers
will be formatted.  The various format specs are:

%t Timestamp (uses `erc-timestamp-format' if non-nil or \"[%Y-%m-%d %H:%M] \")
%n Nickname of sender
%u Nickname!user@host of sender
%c Channel in which this was received
%m Message"
  :type 'string)

(defcustom erc-beep-match-types '(current-nick)
  "Types of matches to beep for when a match occurs.
The function `erc-beep-on-match' needs to be added to `erc-text-matched-hook'
for beeping to work."
  :type '(choice (repeat :tag "Beep on match" (choice
					       (const current-nick)
					       (const keyword)
					       (const pal)
					       (const dangerous-host)
					       (const fool)))
		 (const :tag "Don't beep" nil)))

(defcustom erc-text-matched-hook '(erc-log-matches)
  "Abnormal hook for visiting text matching a predefined \"type\".
ERC calls members with the arguments (MATCH-TYPE NUH MESSAGE), where
MATCH-TYPE is a \"category\" symbol, one of `current-nick', `keyword',
`pal', `dangerous-host', and `fool'; and NUH is an `erc-response'
sender, like \"bob!~bob@example.org\" or an IRC command prefixed with
the string \"Server:\", as in \"Server:353\".  MESSAGE is the current
incarnation of the just-inserted message minus a leading speaker, like
\"<bob> \".  For traditional reasons, MESSAGE always includes a leading
`erc-notice-prefix' and a trailing newline."
  :options '(erc-log-matches erc-hide-fools erc-beep-on-match)
  :type 'hook)

(defcustom erc-match-exclude-server-buffer nil
  "If true, don't perform match on the server buffer.
This is useful for excluding all the things like MOTDs from the
server and other miscellaneous functions."
  :version "24.3"
  :type 'boolean)

(defcustom erc-match-quote-when-adding 'ask
  "Whether to `regexp-quote' when adding to a match list interactively.
When the value is a boolean, the opposite behavior will be made
available via universal argument."
  :package-version '(ERC . "5.5")
  :type '(choice (const ask)
                 (const t)
                 (const nil)))

(defcustom erc-match-functions '(erc-match-opt-pal
                                 erc-match-opt-fool
                                 erc-match-opt-dangerous-host
                                 erc-match-opt-keyword
                                 erc-match-opt-current-nick)
  "Type constructors for \\+`match' processing.
See the struct `erc-match' as well as Info node `(erc) Match API' for
details."
  :package-version '(ERC . "5.7")
  :type '(hook :options (erc-match-opt-pal
                         erc-match-opt-fool
                         erc-match-opt-dangerous-host
                         erc-match-opt-keyword
                         erc-match-opt-current-nick)))


;; Internal variables:

;; This is exactly the same as erc-button-syntax-table.  Should we
;; just put it in erc.el
(defvar erc-match-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\[ "w" table)
    (modify-syntax-entry ?\] "w" table)
    (modify-syntax-entry ?\{ "w" table)
    (modify-syntax-entry ?\} "w" table)
    (modify-syntax-entry ?` "w" table)
    (modify-syntax-entry ?^ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?| "w" table)
    (modify-syntax-entry ?\\ "w" table)
    table)
  "Syntax table used when highlighting messages.
This syntax table should make all the valid nick characters word
constituents.")

;; Faces:

(defface erc-current-nick-face '((t :weight bold :foreground "DarkTurquoise"))
  "ERC face for occurrences of your current nickname."
  :group 'erc-faces)

(defface erc-dangerous-host-face '((t :foreground "red"))
  "ERC face for people on dangerous hosts.
See `erc-dangerous-hosts'."
  :group 'erc-faces)

(defface erc-pal-face '((t :weight bold :foreground "Magenta"))
  "ERC face for your pals.
See `erc-pals'."
  :group 'erc-faces)

(defface erc-fool-face '((t :foreground "dim gray"))
  "ERC face for fools on the channel.
See `erc-fools'."
  :group 'erc-faces)

(defface erc-keyword-face '((t :weight bold :foreground "pale green"))
  "ERC face for your keywords.
Note that this is the default face to use if
`erc-keywords' does not specify another."
  :group 'erc-faces)

;; Functions:

(defun erc-add-entry-to-list (list prompt &optional completions alt)
  "Add an entry interactively to a list.
LIST must be passed as a symbol
The query happens using PROMPT.
Completion is performed on the optional alist COMPLETIONS."
  (when erc-match--opt-pat-cache
    (setq erc-match--opt-pat-cache nil))
  (let ((entry (completing-read
		prompt
		completions
		(lambda (x)
                  (not (erc-member-ignore-case (car x) (symbol-value list))))))
        quoted)
    (setq quoted (regexp-quote entry))
    (when (pcase erc-match-quote-when-adding
            ('ask (unless (string= quoted entry)
                    (y-or-n-p
                     (format "Use regexp-quoted form (%s) instead? " quoted))))
            ('t (not alt))
            ('nil alt))
      (setq entry quoted))
    (if (erc-member-ignore-case entry (symbol-value list))
	(error "\"%s\" is already on the list" entry)
      (set list (cons entry (symbol-value list))))))

(defun erc-remove-entry-from-list (list prompt)
  "Remove an entry interactively from a list.
LIST must be passed as a symbol.
The elements of LIST can be strings, or cons cells where the
car is the string."
  (when erc-match--opt-pat-cache
    (setq erc-match--opt-pat-cache nil))
  (let* ((alist (mapcar (lambda (x)
			  (if (listp x)
			      x
			    (list x)))
			(symbol-value list)))
	 (entry (completing-read
		 prompt
		 alist
		 nil
		 t)))
    (if (erc-member-ignore-case entry (symbol-value list))
	;; plain string
	(set list (delete entry (symbol-value list)))
      ;; cons cell
      (set list (delete (assoc entry (symbol-value list))
			(symbol-value list))))))

;;;###autoload
(defun erc-add-pal (&optional arg)
  "Add pal interactively to `erc-pals'."
  (interactive "P")
  (erc-add-entry-to-list 'erc-pals "Add pal: "
                         (erc-get-server-nickname-alist) arg))

;;;###autoload
(defun erc-delete-pal ()
  "Delete pal interactively to `erc-pals'."
  (interactive)
  (erc-remove-entry-from-list 'erc-pals "Delete pal: "))

;;;###autoload
(defun erc-add-fool (&optional arg)
  "Add fool interactively to `erc-fools'."
  (interactive "P")
  (erc-add-entry-to-list 'erc-fools "Add fool: "
                         (erc-get-server-nickname-alist) arg))

;;;###autoload
(defun erc-delete-fool ()
  "Delete fool interactively to `erc-fools'."
  (interactive)
  (erc-remove-entry-from-list 'erc-fools "Delete fool: "))

;;;###autoload
(defun erc-add-keyword (&optional arg)
  "Add keyword interactively to `erc-keywords'."
  (interactive "P")
  (erc-add-entry-to-list 'erc-keywords "Add keyword: " nil arg))

;;;###autoload
(defun erc-delete-keyword ()
  "Delete keyword interactively to `erc-keywords'."
  (interactive)
  (erc-remove-entry-from-list 'erc-keywords "Delete keyword: "))

;;;###autoload
(defun erc-add-dangerous-host (&optional arg)
  "Add dangerous-host interactively to `erc-dangerous-hosts'."
  (interactive "P")
  (erc-add-entry-to-list 'erc-dangerous-hosts "Add dangerous-host: " nil arg))

;;;###autoload
(defun erc-delete-dangerous-host ()
  "Delete dangerous-host interactively to `erc-dangerous-hosts'."
  (interactive)
  (erc-remove-entry-from-list 'erc-dangerous-hosts "Delete dangerous-host: "))

(defun erc-match-current-nick-p (_nickuserhost msg)
  "Check whether the current nickname is in MSG.
NICKUSERHOST will be ignored."
  (with-syntax-table erc-match-syntax-table
    (and msg
	 (string-match (concat "\\b"
			       (regexp-quote (erc-current-nick))
			       "\\b")
		       msg))))

(defun erc-match-pal-p (nickuserhost _msg)
  "Check whether NICKUSERHOST is in `erc-pals'.
MSG will be ignored."
  (and nickuserhost erc-pals
       (erc-list-match erc-pals nickuserhost)))

(defun erc-match-fool-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-fools' or MSG is directed at a fool."
  (and msg nickuserhost erc-fools
       (or (erc-list-match erc-fools nickuserhost)
	   (erc-match-directed-at-fool-p msg))))

(defun erc-match-keyword-p (_nickuserhost msg)
  "Check whether any keyword of `erc-keywords' matches for MSG.
NICKUSERHOST will be ignored."
  (and msg erc-keywords
       (erc-list-match
	(mapcar (lambda (x)
		  (if (listp x)
		      (car x)
		    x))
		erc-keywords)
	msg)))

(defun erc-match-dangerous-host-p (nickuserhost _msg)
  "Check whether NICKUSERHOST is in `erc-dangerous-hosts'.
MSG will be ignored."
  (and nickuserhost erc-dangerous-hosts
       (erc-list-match erc-dangerous-hosts nickuserhost)))

(defun erc-match-directed-at-fool-p (msg)
  "Check whether MSG is directed at a fool.
In order to do this, every entry in `erc-fools' will be used.
In any of the following situations, MSG is directed at an entry FOOL:

- MSG starts with \"FOOL: \" or \"FOO, \"
- MSG contains \", FOOL.\" (actually, \"\\s. FOOL\\s.\")"
  (let ((fools-beg (mapcar (lambda (entry)
				 (concat "^" entry "[:,] "))
			   erc-fools))
	(fools-end (mapcar (lambda (entry)
				 (concat "\\s. " entry "\\s."))
			       erc-fools)))
    (or (erc-list-match fools-beg msg)
	(erc-list-match fools-end msg))))

(cl-defstruct (erc-match (:constructor erc-match))
  "Base type for text and user matching performed by the \\+`match' module.
Users wishing to perform custom matching should add a constructor that
returns an instance of this type to the hook `erc-match-functions'.  If
the `:predicate' slot's predicate returns non-nil after being called
with its own instance in the narrowed single-message buffer, ERC calls
the `:handler' slot's function with the same instance and with the match
data still intact.  More details in Info node `(erc) Match API'."
  ( predicate (error "Keyword `:predicate' missing") :type function
    :documentation "Called in narrowed buffer with own instance.")
  ( spkr-beg nil :type (or null natnum)
    :documentation "Position of the beginning of speaker's nick, if known.")
  ( spkr-end nil :type (or null natnum)
    :documentation "Position of the end of speaker's nick, if known.")
  ( body-beg (error "Keyword `:body-beg' missing") :type marker
    :documentation "Marker residing at the beginning of the message body.")
  ( sender (error "Keyword `:sender' missing") :type string
    :documentation "The sender's n!u@h.")
  ( nick nil :type (or null string)
    :documentation "The sender's nick if they're a user and not the server.")
  ( command (error "Keyword `:command' missing") :type (or symbol natnum)
    :documentation "Protocol command or numeric, like `PRIVMSG' or 353.")
  ( handler #'ignore :type function
    :documentation "Called on `:predicate' match with own instance.")
  ( newlinep nil :type boolean
    :documentation "Whether narrowed buffer includes trailing newline."))

(cl-defstruct (erc-match-traditional
               (:constructor erc-match-traditional)
               (:include erc-match
                         (handler #'erc-match-highlight)
                         (newlinep t)))
  "Match type for user-option based on \"categories\" and \"parts\".
The `:category' slot exists for the benefit of `erc-text-matched-hook',
which receives its value as a second parameter (the hook only runs when
the slot is non-nil).  For compatibility, the narrowed buffer includes a
trailing newline."
  ( category (error "Keyword `:category' missing") :type symbol
    :documentation "Traditional \\+`match' \"category\", like `pal'.")
  ( face 'erc-default-face :type face
    :documentation "Face to highlight the matched portion with.")
  ( part nil :type symbol
    :documentation "Symbol for the portion of the message to highlight.")
  ( data nil :type list
    :documentation "User-specified patterns or other type-specific data."))

(cl-defstruct (erc-match-opt-current-nick
               (:include erc-match-traditional
                         (category 'current-nick)
                         (predicate #'erc-match--current-nick-p)
                         (part erc-current-nick-highlight-type)
                         (face 'erc-current-nick-face)
                         (data (list (concat "\\b"
                                             (regexp-quote (erc-current-nick))
                                             "\\b"))))
               (:constructor erc-match-opt-current-nick))
  "An options-based type for the `current-nick' category.")

(cl-defstruct (erc-match-opt-keyword
               (:include erc-match-traditional
                         (category 'keyword)
                         (predicate #'erc-match--keyword-p)
                         (part erc-keyword-highlight-type)
                         (face 'erc-keyword-face)
                         (data erc-keywords))
               (:constructor erc-match-opt-keyword))
  "An options-based type for the `keyword' category.")

(cl-defstruct (erc-match-user (:include erc-match-traditional)
                              (:constructor erc-match-user))
  "An `erc-match' that's only processed when `:nick' is non-nil.")

(cl-defstruct (erc-match-opt-fool
               (:include erc-match-user
                         (category 'fool)
                         (predicate #'erc-match--user-nuh-or-mention-p)
                         (part erc-fool-highlight-type)
                         (face 'erc-fool-face)
                         (data erc-fools))
               (:constructor erc-match-opt-fool))
  "An options-based type for the `fool' category.")

(cl-defstruct (erc-match-opt-pal
               (:include erc-match-user
                         (category 'pal)
                         (predicate #'erc-match--user-nuh-or-mention-p)
                         (part erc-pal-highlight-type)
                         (face 'erc-pal-face)
                         (data erc-pals))
               (:constructor erc-match-opt-pal))
  "An options-based type for the `pal' category.")

(cl-defstruct (erc-match-opt-dangerous-host
               (:include erc-match-user
                         (category 'dangerous-host)
                         (predicate #'erc-match--user-nuh-or-mention-p)
                         (part erc-dangerous-host-highlight-type)
                         (face 'erc-dangerous-host-face)
                         (data erc-dangerous-hosts))
               (:constructor erc-match-opt-dangerous-host))
  "An options-based type for the `dangerous-host' category.")

(defun erc-match--opt-pat-get (compute-fn input)
  "Retrieve cached results for computing INPUT with COMPUTE-FN."
  (with-memoization (alist-get input (alist-get compute-fn
                                                erc-match--opt-pat-cache nil t)
                               nil t)
    (funcall compute-fn input)))

(defun erc-match--opt-pat-make (patterns)
  "Act like `regexp-opt' but for regexp PATTERNS, not fixed strings."
  (string-join patterns "\\|"))

(defun erc-match--opt-pat-make-kw (patterns)
  (mapconcat (lambda (w) (or (car-safe w) w)) patterns "\\|"))

(defun erc-match--opt-pat-make-addr-beg (patterns)
  (concat "\\<\\(" (erc-match--opt-pat-make patterns) "\\)[:,] "))

(defun erc-match--opt-pat-make-addr-end (patterns)
  (concat "\\s. \\(" (erc-match--opt-pat-make patterns) "\\)\\s."))

(defun erc-match--current-nick-p (match)
  (re-search-forward (car (erc-match-traditional-data match)) nil t))

(defun erc-match--keyword-p (match)
  "Return non-nil if the pattern given for MATCH's user option matches."
  (and-let* ((patterns (erc-match-traditional-data match)))
    (goto-char (erc-match-body-beg match))
    (re-search-forward (erc-match--opt-pat-get #'erc-match--opt-pat-make-kw
                                               patterns)
                       nil t)))

(defun erc-match--user-nuh-or-mention-p (match)
  "Return non-nil on matching \"NUH\" for MATCH object.
Also do so on mentions if the category is `fool' or the corresponding
\"part\" option is `nick-or-mention'."
  (and-let* ((patterns (erc-match-traditional-data match)))
    (or (string-match (erc-match--opt-pat-get #'erc-match--opt-pat-make
                                              patterns)
                      (erc-match-sender match))
        (and (or (eq (erc-match-traditional-category match) 'fool)
                 (eq (erc-match-traditional-part match) 'nick-or-mention))
             ;; Mimic `erc-match-directed-at-fool-p', but search
             ;; the narrowed buffer instead of a string argument.
             (goto-char (erc-match-body-beg match))
             (or (looking-at (erc-match--opt-pat-get
                              #'erc-match--opt-pat-make-addr-beg
                              patterns))
                 (search-forward-regexp
                  (erc-match--opt-pat-get #'erc-match--opt-pat-make-addr-end
                                          patterns)
                  nil t))))))

(cl-defgeneric erc-match-highlight-by-part (match part)
  "Highlight PART of narrowed buffer for `erc-match' object MATCH.")

(cl-defmethod erc-match-highlight-by-part ((match erc-match-traditional)
                                           (_ (eql nick)))
  "Highlight MATCH's nick in the bracketed speaker portion of the message."
  (when (erc-match-spkr-beg match)
    (erc-put-text-property (erc-match-spkr-beg match)
                           (erc-match-spkr-end match)
                           'font-lock-face
                           (erc-match-traditional-face match))))

(cl-defmethod erc-match-highlight-by-part ((match erc-match-traditional)
                                           (_ (eql message)))
  "Highlight MATCH's message body, not including the leading speaker tag."
  (erc-put-text-property (erc-match-body-beg match) (point-max)
                         'font-lock-face (erc-match-traditional-face match)))

(cl-defmethod erc-match-highlight-by-part ((match erc-match-traditional)
                                           (_ (eql all)))
  "Highlight MATCH's whole message, including the speaker tag."
  (erc-put-text-property (point-min) (point-max)
                         'font-lock-face (erc-match-traditional-face match)))

(cl-defmethod erc-match-highlight-by-part ((match erc-match-traditional)
                                           (_ (eql keyword)))
  "Highlight all occurrences of all keyword patterns for MATCH."
  (dolist (pat (erc-match-traditional-data match))
    (let ((regex (if (consp pat) (car pat) pat))
          (face (if (consp pat) (cdr pat) (erc-match-traditional-face match))))
      (goto-char (erc-match-body-beg match))
      (while (re-search-forward regex nil t)
        (erc-put-text-property (match-beginning 0) (match-end 0)
                               'font-lock-face face)))))

(cl-defmethod erc-match-highlight-by-part ((match erc-match-traditional)
                                           (_ (eql nick-or-keyword)))
  "Highlight MATCH's speaker-tag nick if applicable, otherwise all mentions."
  (if (erc-match-spkr-end match)
      (erc-put-text-property (erc-match-spkr-beg match)
                             (erc-match-spkr-end match)
                             'font-lock-face
                             (erc-match-traditional-face match))
    (erc-match-highlight-by-part match 'keyword)))

(cl-defmethod erc-match-highlight-by-part ((match erc-match-traditional)
                                           (_ (eql nick-or-mention)))
  "Highlight MATCH's speaker tag nick of matching users or all mentions."
  (cl-letf (((erc-match-body-beg match)
             (or (erc-match-spkr-beg match) (point-min))))
    (erc-match-highlight-by-part match 'keyword)))

(defvar erc-match-highlight-matched nil
  "Matched `erc-match' instance in `erc-text-matched-hook'.")

(defvar erc-match--instances nil
  "Alist mapping constructors to successful `erc-match' instances.")

(defun erc-match-highlight (match)
  "Dispatch `erc-match-highlight-by-part' on MATCH's `:part' slot.
Run `erc-text-matched-hook' when MATCH's `category' slot is non-nil."
  (unless (erc-match-traditional-p match)
    (signal 'wrong-type-argument (list 'erc-match-traditional match)))
  (cl-assert (erc-match-newlinep match))
  (erc-match-highlight-by-part match (erc-match-traditional-part match))
  (when (erc-match-traditional-category match)
    (let ((user-nuh (and (erc-match-nick match) (erc-match-sender match)))
          (erc-match-highlight-matched match))
      (run-hook-with-args 'erc-text-matched-hook
                          (erc-match-traditional-category match)
                          (or user-nuh (format "Server:%s"
                                               (erc-match-command match)))
                          ;; For compatibility, include a leading "*** ".
                          (buffer-substring (if user-nuh
                                                (erc-match-body-beg match)
                                              (point-min))
                                            (point-max))))))

(defun erc-match-get-message-body (match)
  "Return the message body for MATCH in the current narrowed buffer."
  (with-restriction (point-min)
      (if (erc-match-newlinep match) (point-max) (1+ (point-max)))
    (buffer-substring (erc-match-body-beg match) (1- (point-max)))))

(defun erc-match-get-match (constructor)
  "Return successful `erc-match' instance for CONSTRUCTOR, if any.
Expect to be called only from `erc-match' :predicate and :handler
functions as well as `erc-text-matched-hook' members."
  (alist-get constructor erc-match--instances))

(defun erc-match--run-match (constructor spkr-beg spkr-end body-beg
                                         nick sender command)
  "Run `erc-match' handler if its predicate returns non-nil.
Call CONSTRUCTOR with SPKR-BEG, SPKR-END, BODY-BEG, NICK SENDER, and
COMMAND to create the `erc-match' instance."
  (when-let* ((instance (funcall constructor
                                 :spkr-beg spkr-beg
                                 :spkr-end spkr-end
                                 :body-beg body-beg
                                 :nick nick
                                 :sender sender
                                 :command command))
              (_ (or nick (not (erc-match-user-p instance))))
              (_ (goto-char (point-min)))
              (_ (funcall (erc-match-predicate instance) instance)))
    (if (erc-match-newlinep instance)
        (funcall (erc-match-handler instance) instance)
      (with-restriction (point-min) (1- (point-max))
        (funcall (erc-match-handler instance) instance)))
    (push (cons constructor instance) erc-match--instances)
    nil))

(defun erc-match--message ()
  "Run `erc-match-functions' against contents of narrowed buffer."
  (goto-char (point-min))
  (let* ((response erc--parsed-response)
         (user-nuh (and response (erc-get-parsed-vector-nick response)))
         ;; Nick of sender's NUH if they are not the server itself.
         (nick (and user-nuh (or (erc--check-msg-prop 'erc--spkr)
                                 (erc-extract-nick user-nuh))))
         (unknownp (erc--check-msg-prop 'erc--msg 'unknown))
         (spkr-end (and (not unknownp) (erc--get-speaker-bounds)))
         (spkr-beg (and spkr-end (pop spkr-end)))
         (body-beg (save-excursion
                     (unless unknownp
                       (when-let* ((fn (erc--check-msg-prop 'erc--pfx)))
                         (funcall fn)))
                     (point-marker)))
         (command (erc--check-msg-prop 'erc--cmd))
         (erc-match--instances ()))
    (with-syntax-table erc-match-syntax-table
      (run-hook-wrapped 'erc-match-functions #'erc-match--run-match
                        spkr-beg spkr-end body-beg nick
                        (erc-response.sender response) command))))

(defvar erc-match-use-legacy-logic-p nil
  "When non-nil, use the non-`erc-match' variant of `erc-match-message'.")
(make-obsolete 'erc-match-use-legacy-logic-p
               "non-nil behavior mostly replicated bug for bug" "32.1")

(defun erc-match-message ()
  "Run handlers for matched patterns in the narrowed buffer."
  (if (or erc-match-use-legacy-logic-p (null erc--parsed-response))
      (erc-match--message-legacy)
    (unless (or (and erc-match-exclude-server-buffer (erc--server-buffer-p))
                (null (erc--check-msg-prop 'erc--cmd))
                (erc--memq-msg-prop 'erc--skip 'match))
      (erc-match--message))))

(defun erc-match--message-legacy ()
  "Mark certain keywords in a region.
Use this defun with `erc-insert-modify-hook'."
  ;; This needs some refactoring.
  (goto-char (point-min))
  (let* ((to-match-nick-dep '("pal" "fool" "dangerous-host"))
	 (to-match-nick-indep '("keyword" "current-nick"))
	 (vector (erc-get-parsed-vector (point-min)))
	 (nickuserhost (erc-get-parsed-vector-nick vector))
	 (nickname (and nickuserhost
			(nth 0 (erc-parse-user nickuserhost))))
	 ;; (old-pt (point))
	 (nick-beg (and nickname
			(re-search-forward (regexp-quote nickname)
					   (point-max) t)
			(match-beginning 0)))
	 (nick-end (when nick-beg
		     (match-end 0)))
         (message-beg (if (and nick-end
                               (<= (+ 2 nick-end) (point-max)))
                          ;; Message starts 2 characters after the
                          ;; nick except for CTCP ACTION messages.
                          ;; Nick surrounded by angle brackets only in
                          ;; normal messages.
                          (+ nick-end
                             (if (eq ?> (char-after nick-end))
                                 2
                               1))
                        (point-min)))
         (message (buffer-substring message-beg (point-max))))
    (when (and vector
	       (not (and erc-match-exclude-server-buffer
                         ;; FIXME replace with `erc--server-buffer-p'
                         ;; or explain why that's unwise.
                         (erc-server-or-unjoined-channel-buffer-p))))
      (mapc
       (lambda (match-type)
	 (goto-char (point-min))
	 (let* ((match-prefix (concat "erc-" match-type))
		(match-pred (intern (concat "erc-match-" match-type "-p")))
		(match-htype (symbol-value (intern (concat match-prefix
						           "-highlight-type"))))
		(match-regex (if (string= match-type "current-nick")
				 (regexp-quote (erc-current-nick))
			       (symbol-value
			        (intern (concat match-prefix "s")))))
		(match-face (intern (concat match-prefix "-face"))))
	   (when (funcall match-pred nickuserhost message)
	     (cond
	      ;; Highlight the nick of the message
	      ((and (eq match-htype 'nick)
		    nick-end)
	       (erc-put-text-property
		nick-beg nick-end
		'font-lock-face match-face (current-buffer)))
	      ;; Highlight the nick of the message, or the current
	      ;; nick if there's no nick in the message (e.g. /NAMES
	      ;; output)
	      ((and (string= match-type "current-nick")
		    (eq match-htype 'nick-or-keyword))
	       (if nick-end
		   (erc-put-text-property
		    nick-beg nick-end
		    'font-lock-face match-face (current-buffer))
		 (goto-char (+ 2 (or nick-end
				     (point-min))))
		 (while (re-search-forward match-regex nil t)
		   (erc-put-text-property (match-beginning 0) (match-end 0)
					  'font-lock-face match-face))))
              ;; Highlight the whole message (not including the nick)
              ((eq match-htype 'message)
               (erc-put-text-property
                message-beg (point-max)
                'font-lock-face match-face (current-buffer)))
	      ;; Highlight the whole message (including the nick)
	      ((eq match-htype 'all)
	       (erc-put-text-property
		(point-min) (point-max)
		'font-lock-face match-face (current-buffer)))
	      ;; Highlight all occurrences of the word to be
	      ;; highlighted.
	      ((and (string= match-type "keyword")
		    (eq match-htype 'keyword))
	       (mapc (lambda (elt)
		       (let ((regex elt)
			     (face match-face))
			 (when (consp regex)
			   (setq regex (car elt)
				 face (cdr elt)))
			 (goto-char (+ 2 (or nick-end
					     (point-min))))
			 (while (re-search-forward regex nil t)
			   (erc-put-text-property
			    (match-beginning 0) (match-end 0)
			    'font-lock-face face))))
		     match-regex))
	      ;; Highlight all occurrences of our nick.
	      ((and (string= match-type "current-nick")
		    (eq match-htype 'keyword))
	       (goto-char (+ 2 (or nick-end
				   (point-min))))
	       (while (re-search-forward match-regex nil t)
		 (erc-put-text-property (match-beginning 0) (match-end 0)
					'font-lock-face match-face)))
	      ;; Else twiddle your thumbs.
	      (t nil))
	     (run-hook-with-args
	      'erc-text-matched-hook
	      (intern match-type)
	      (or nickuserhost
		  (concat "Server:" (erc-get-parsed-vector-type vector)))
	      message))))
       (if nickuserhost
	   (append to-match-nick-dep to-match-nick-indep)
	 to-match-nick-indep)))))

(defun erc-log-matches (match-type nickuserhost message)
  "Log matches in a separate buffer, determined by MATCH-TYPE.
The behavior of this function is controlled by the variables
`erc-log-matches-types-alist' and `erc-log-matches-flag'.
Specify the match types which should be logged in the former,
and deactivate/activate match logging in the latter.
See `erc-log-match-format'."
  (when-let*
      ((erc-log-matches-flag)
       (_ (or (eq erc-log-matches-flag t) (erc-away-time)))
       (match-buffer-name (cdr (assq match-type erc-log-matches-types-alist)))
       (line (format-spec
              erc-log-match-format
              (erc-compat--defer-format-spec-in-buffer
               (?n . (or (erc--check-msg-prop 'erc--spkr)
                         (erc-extract-nick nickuserhost)))
               (?t . (format-time-string
                      (or (bound-and-true-p erc-timestamp-format)
                          "[%Y-%m-%d %H:%M] ")))
               (?c erc-default-target)
               (?m . message)
               (?u . nickuserhost)))))
    (with-current-buffer (erc-log-matches-make-buffer match-buffer-name)
      (with-silent-modifications
        (goto-char (point-max))
        (insert line)))))

(defun erc-log-matches-make-buffer (name)
  "Create or get a log-matches buffer named NAME and return it."
  (let* ((buffer-already (get-buffer name))
	 (buffer (or buffer-already
		     (get-buffer-create name))))
    (with-current-buffer buffer
      (unless buffer-already
	(insert " == Type \"q\" to dismiss messages ==\n")
	(view-mode-enter nil (lambda (buffer)
			       (when (y-or-n-p "Discard messages? ")
				 (kill-buffer buffer)))))
      buffer)))

(defun erc-log-matches-come-back (_proc _parsed)
  "Display a notice that messages were logged while away."
  (when (and (erc-away-time)
	     (eq erc-log-matches-flag 'away))
    (mapc
     (lambda (match-type)
       (let ((buffer (get-buffer (cdr match-type)))
	     (buffer-name (cdr match-type)))
	 (when buffer
	   (let* ((last-msg-time (erc-emacs-time-to-erc-time
				  (with-current-buffer buffer
				    (get-text-property (1- (point-max))
						       'timestamp))))
		  (away-time (erc-emacs-time-to-erc-time (erc-away-time))))
	     (when (and away-time last-msg-time
			(time-less-p away-time last-msg-time))
	       (erc-display-message
		nil 'notice 'active
		(format "You have logged messages waiting in \"%s\"."
			buffer-name))
	       (erc-display-message
		nil 'notice 'active
		(format "Type \"C-c C-k %s RET\" to view them."
			buffer-name)))))))
     erc-log-matches-types-alist))
  nil)

; This handler must be run _before_ erc-process-away is.
(add-hook 'erc-server-305-functions #'erc-log-matches-come-back nil)

(defun erc-go-to-log-matches-buffer ()
  "Interactively open an erc-log-matches buffer."
  (interactive)
  (let ((buffer-name (completing-read "Switch to ERC Log buffer: "
				      (mapcar (lambda (x)
						(cons (cdr x) t))
					      erc-log-matches-types-alist)
				      (lambda (buffer-cons)
					(get-buffer (car buffer-cons))))))
    (switch-to-buffer buffer-name)))

(defun erc-hide-fools (match-type _nickuserhost _message)
  "Hide comments from designated fools."
  (when (and erc--msg-props (eq match-type 'fool))
    (puthash 'erc--invisible 'erc-match-fool erc--msg-props)))

;; FIXME remove, make public, or only add locally.
;;
;; ERC modules typically don't add internal functions to public hooks
;; globally.  However, ERC 5.6 will likely include a general
;; (internal) facility for adding invisible props, which will obviate
;; the need for this function.  IOW, leaving this internal for now is
;; an attempt to avoid the hassle of the deprecation process.
(defun erc-match--on-insert-post ()
  "Hide messages marked with the `erc--invisible' prop."
  (when (erc--check-msg-prop 'erc--invisible 'erc-match-fool)
    (remhash 'erc--invisible erc--msg-props)
    (erc--hide-message 'match-fools)))

(defun erc-beep-on-match (match-type _nickuserhost _message)
  "Beep when text matches.
This function is meant to be called from `erc-text-matched-hook'."
  (when (member match-type erc-beep-match-types)
    (beep)))

(defun erc-match--setup ()
  "Add an `erc-match' property to the local spec."
  ;; Hopefully, this will be extended to do the same for other
  ;; invisible properties managed by this module.
  (if erc-match-mode
      (erc-match-toggle-hidden-fools +1)
    (erc-match-toggle-hidden-fools -1)))

(defun erc-match-toggle-hidden-fools (arg)
  "Toggle fool visibility.
Expect the function `erc-hide-fools' or similar to be present in
`erc-text-matched-hook'."
  (interactive "P")
  (erc--toggle-hidden 'match-fools arg))

(provide 'erc-match)

;;; erc-match.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
