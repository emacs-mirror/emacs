;;; allout.el --- extensive outline mode for use alone and with other modes

;; Copyright (C) 1992, 1993, 1994, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Ken Manheimer <ken dot manheimer at gmail dot com>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail dot com>
;; Created: Dec 1991 - first release to usenet
;; Keywords: outlines wp languages

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

;; Allout outline mode provides extensive outline formatting and
;; and manipulation beyond standard emacs outline mode.  It provides
;; for structured editing of outlines, as well as navigation and
;; exposure.  It also provides for syntax-sensitive text like
;; programming languages.  (For an example, see the allout code
;; itself, which is organized in ;; an outline framework.)
;;
;; Some features:
;;
;;  - classic outline-mode topic-oriented navigation and exposure adjustment
;;  - topic-oriented editing including coherent topic and subtopic
;;    creation, promotion, demotion, cut/paste across depths, etc
;;  - incremental search with dynamic exposure and reconcealment of text
;;  - customizable bullet format enbles programming-language specific
;;    outlining, for ultimate code-folding editing.  (allout code itself is
;;    formatted as an outline - do ESC-x eval-current-buffer in allout.el
;;    to try it out.)
;;  - configurable per-file initial exposure settings
;;  - symmetric-key and key-pair topic encryption, plus reliable key
;;    verification and user-supplied hint maintenance.  (see
;;    allout-toggle-current-subtree-encryption docstring.)
;;  - automatic topic-number maintenance
;;  - "hot-spot" operation, for single-keystroke maneuvering and
;;    exposure control (see the allout-mode docstring)
;;  - easy rendering of exposed portions into numbered, latex, indented, etc
;;    outline styles
;;
;; and more.
;;
;; The outline menubar additions provide quick reference to many of
;; the features, and see the docstring of the variable `allout-init'
;; for instructions on priming your emacs session for automatic
;; activation of allout-mode.
;;
;; See the docstring of the variables `allout-layout' and
;; `allout-auto-activation' for details on automatic activation of
;; `allout-mode' as a minor mode.  (It has changed since allout
;; 3.x, for those of you that depend on the old method.)
;;
;; Note - the lines beginning with `;;;_' are outline topic headers.
;;        Just `ESC-x eval-current-buffer' to give it a whirl.

;; ken manheimer (ken dot manheimer at gmail dot com)

;;; Code:

;;;_* Provide
;(provide 'outline)
(provide 'allout)

;;;_* Dependency autoloads
(eval-when-compile 'cl)                 ; otherwise, flet compilation fouls
(autoload 'crypt-encrypt-buffer "crypt++")
(setq-default crypt-encryption-type 'gpg)

(autoload 'mc-encrypt "mailcrypt"
  "*Encrypt the current buffer")
(autoload 'mc-activate-passwd "mailcrypt"
  "Activate the passphrase matching ID, using PROMPT for a prompt.
Return the passphrase.  If PROMPT is nil, only return value if cached.")
(autoload 'mc-gpg-process-region "mc-gpg")
(autoload 'mc-dectivate-passwd "mailcrypt"
  "*Deactivate the passphrase cache.")

;;;_* USER CUSTOMIZATION VARIABLES:
(defgroup allout nil
  "Extensive outline mode for use alone and with other modes."
  :prefix "allout-"
  :group 'outlines)

;;;_ + Layout, Mode, and Topic Header Configuration

;;;_  = allout-auto-activation
(defcustom allout-auto-activation nil
  "*Regulates auto-activation modality of allout outlines - see `allout-init'.

Setq-default by `allout-init' to regulate whether or not allout
outline mode is automatically activated when the buffer-specific
variable `allout-layout' is non-nil, and whether or not the layout
dictated by `allout-layout' should be imposed on mode activation.

With value t, auto-mode-activation and auto-layout are enabled.
\(This also depends on `allout-find-file-hook' being installed in
`find-file-hook', which is also done by `allout-init'.)

With value `ask', auto-mode-activation is enabled, and endorsement for
performing auto-layout is asked of the user each time.

With value `activate', only auto-mode-activation is enabled,
auto-layout is not.

With value nil, neither auto-mode-activation nor auto-layout are
enabled.

See the docstring for `allout-init' for the proper interface to
this variable."
  :type '(choice (const :tag "On" t)
                (const :tag "Ask about layout" "ask")
                (const :tag "Mode only" "activate")
                (const :tag "Off" nil))
  :group 'allout)
;;;_  = allout-layout
(defvar allout-layout nil
  "*Layout specification and provisional mode trigger for allout outlines.

Buffer-specific.

A list value specifies a default layout for the current buffer, to be
applied upon activation of `allout-mode'.  Any non-nil value will
automatically trigger `allout-mode' \(provided `allout-init' has been called
to enable this behavior).

See the docstring for `allout-init' for details on setting up for
auto-mode-activation, and for `allout-expose-topic' for the format of
the layout specification.

You can associate a particular outline layout with a file by setting
this var via the file's local variables.  For example, the following
lines at the bottom of an Emacs Lisp file:

;;;Local variables:
;;;allout-layout: \(0 : -1 -1 0)
;;;End:

will, modulo the above-mentioned conditions, cause the mode to be
activated when the file is visited, followed by the equivalent of
`\(allout-expose-topic 0 : -1 -1 0)'.  \(This is the layout used for
the allout.el, itself.)

Also, allout's mode-specific provisions will make topic prefixes default
to the comment-start string, if any, of the language of the file.  This
is modulo the setting of `allout-use-mode-specific-leader', which see.")
(make-variable-buffer-local 'allout-layout)
;;;_  = allout-show-bodies
(defcustom allout-show-bodies nil
  "*If non-nil, show entire body when exposing a topic, rather than
just the header."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-show-bodies)

;;;_  = allout-header-prefix
(defcustom allout-header-prefix "."
  "*Leading string which helps distinguish topic headers.

Outline topic header lines are identified by a leading topic
header prefix, which mostly have the value of this var at their front.
\(Level 1 topics are exceptions.  They consist of only a single
character, which is typically set to the `allout-primary-bullet'.  Many
outlines start at level 2 to avoid this discrepancy."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-header-prefix)
;;;_  = allout-primary-bullet
(defcustom allout-primary-bullet "*"
  "Bullet used for top-level outline topics.

Outline topic header lines are identified by a leading topic header
prefix, which is concluded by bullets that includes the value of this
var and the respective allout-*-bullets-string vars.

The value of an asterisk (`*') provides for backwards compatibility
with the original Emacs outline mode.  See `allout-plain-bullets-string'
and `allout-distinctive-bullets-string' for the range of available
bullets."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-primary-bullet)
;;;_  = allout-plain-bullets-string
(defcustom allout-plain-bullets-string ".,"
  "*The bullets normally used in outline topic prefixes.

See `allout-distinctive-bullets-string' for the other kind of
bullets.

DO NOT include the close-square-bracket, `]', as a bullet.

Outline mode has to be reactivated in order for changes to the value
of this var to take effect."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-plain-bullets-string)
;;;_  = allout-distinctive-bullets-string
(defcustom allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^"
  "*Persistent outline header bullets used to distinguish special topics.

These bullets are used to distinguish topics from the run-of-the-mill
ones.  They are not used in the standard topic headers created by
the topic-opening, shifting, and rebulleting \(eg, on topic shift,
topic paste, blanket rebulleting) routines, but are offered among the
choices for rebulleting.  They are not altered by the above automatic
rebulleting, so they can be used to characterize topics, eg:

 `?' question topics
 `\(' parenthetic comment \(with a matching close paren inside)
 `[' meta-note \(with a matching close ] inside)
 `\"' a quotation
 `=' value settings
 `~' \"more or less\"
 `^' see above

 ... for example.  (`#' typically has a special meaning to the software,
according to the value of `allout-numbered-bullet'.)

See `allout-plain-bullets-string' for the selection of
alternating bullets.

You must run `set-allout-regexp' in order for outline mode to
reconcile to changes of this value.

DO NOT include the close-square-bracket, `]', on either of the bullet
strings."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-distinctive-bullets-string)

;;;_  = allout-use-mode-specific-leader
(defcustom allout-use-mode-specific-leader t
  "*When non-nil, use mode-specific topic-header prefixes.

Allout outline mode will use the mode-specific `allout-mode-leaders'
and/or comment-start string, if any, to lead the topic prefix string,
so topic headers look like comments in the programming language.

String values are used as they stand.

Value t means to first check for assoc value in `allout-mode-leaders'
alist, then use comment-start string, if any, then use default \(`.').
\(See note about use of comment-start strings, below.)

Set to the symbol for either of `allout-mode-leaders' or
`comment-start' to use only one of them, respectively.

Value nil means to always use the default \(`.').

comment-start strings that do not end in spaces are tripled, and an
`_' underscore is tacked on the end, to distinguish them from regular
comment strings.  comment-start strings that do end in spaces are not
tripled, but an underscore is substituted for the space. [This
presumes that the space is for appearance, not comment syntax.  You
can use `allout-mode-leaders' to override this behavior, when
incorrect.]"
  :type '(choice (const t) (const nil) string
		 (const allout-mode-leaders)
		 (const comment-start))
  :group 'allout)
;;;_  = allout-mode-leaders
(defvar allout-mode-leaders '()
  "Specific allout-prefix leading strings per major modes.

Entries will be used instead or in lieu of mode-specific
comment-start strings.  See also `allout-use-mode-specific-leader'.

If you're constructing a string that will comment-out outline
structuring so it can be included in program code, append an extra
character, like an \"_\" underscore, to distinguish the lead string
from regular comments that start at bol.")

;;;_  = allout-old-style-prefixes
(defcustom allout-old-style-prefixes nil
  "*When non-nil, use only old-and-crusty `outline-mode' `*' topic prefixes.

Non-nil restricts the topic creation and modification
functions to asterix-padded prefixes, so they look exactly
like the original Emacs-outline style prefixes.

Whatever the setting of this variable, both old and new style prefixes
are always respected by the topic maneuvering functions."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-old-style-prefixes)
;;;_  = allout-stylish-prefixes - alternating bullets
(defcustom allout-stylish-prefixes t
  "*Do fancy stuff with topic prefix bullets according to level, etc.

Non-nil enables topic creation, modification, and repositioning
functions to vary the topic bullet char (the char that marks the topic
depth) just preceding the start of the topic text) according to level.
Otherwise, only asterisks (`*') and distinctive bullets are used.

This is how an outline can look (but sans indentation) with stylish
prefixes:

    * Top level
    .* A topic
    . + One level 3 subtopic
    .  . One level 4 subtopic
    .  . A second 4 subtopic
    . + Another level 3 subtopic
    .  #1 A numbered level 4 subtopic
    .  #2 Another
    .  ! Another level 4 subtopic with a different distinctive bullet
    .  #4 And another numbered level 4 subtopic

This would be an outline with stylish prefixes inhibited (but the
numbered and other distinctive bullets retained):

    * Top level
    .* A topic
    . * One level 3 subtopic
    .  * One level 4 subtopic
    .  * A second 4 subtopic
    . * Another level 3 subtopic
    .  #1 A numbered level 4 subtopic
    .  #2 Another
    .  ! Another level 4 subtopic with a different distinctive bullet
    .  #4 And another numbered level 4 subtopic

Stylish and constant prefixes (as well as old-style prefixes) are
always respected by the topic maneuvering functions, regardless of
this variable setting.

The setting of this var is not relevant when `allout-old-style-prefixes'
is non-nil."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-stylish-prefixes)

;;;_  = allout-numbered-bullet
(defcustom allout-numbered-bullet "#"
  "*String designating bullet of topics that have auto-numbering; nil for none.

Topics having this bullet have automatic maintenance of a sibling
sequence-number tacked on, just after the bullet.  Conventionally set
to \"#\", you can set it to a bullet of your choice.  A nil value
disables numbering maintenance."
  :type '(choice (const nil) string)
  :group 'allout)
(make-variable-buffer-local 'allout-numbered-bullet)
;;;_  = allout-file-xref-bullet
(defcustom allout-file-xref-bullet "@"
  "*Bullet signifying file cross-references, for `allout-resolve-xref'.

Set this var to the bullet you want to use for file cross-references."
  :type '(choice (const nil) string)
  :group 'allout)
;;;_  = allout-presentation-padding
(defcustom allout-presentation-padding 2
  "*Presentation-format white-space padding factor, for greater indent."
  :type 'integer
  :group 'allout)

(make-variable-buffer-local 'allout-presentation-padding)

;;;_  = allout-abbreviate-flattened-numbering
(defcustom allout-abbreviate-flattened-numbering nil
  "*If non-nil, `allout-flatten-exposed-to-buffer' abbreviates topic
numbers to minimal amount with some context.  Otherwise, entire
numbers are always used."
  :type 'boolean
  :group 'allout)

;;;_ + LaTeX formatting
;;;_  - allout-number-pages
(defcustom allout-number-pages nil
  "*Non-nil turns on page numbering for LaTeX formatting of an outline."
  :type 'boolean
  :group 'allout)
;;;_  - allout-label-style
(defcustom allout-label-style "\\large\\bf"
  "*Font and size of labels for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-head-line-style
(defcustom allout-head-line-style "\\large\\sl "
  "*Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-body-line-style
(defcustom allout-body-line-style " "
  "*Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-title-style
(defcustom allout-title-style "\\Large\\bf"
  "*Font and size of titles for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-title
(defcustom allout-title '(or buffer-file-name (buffer-name))
  "*Expression to be evaluated to determine the title for LaTeX
formatted copy."
  :type 'sexp
  :group 'allout)
;;;_  - allout-line-skip
(defcustom allout-line-skip ".05cm"
  "*Space between lines for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-indent
(defcustom allout-indent ".3cm"
  "*LaTeX formatted depth-indent spacing."
  :type 'string
  :group 'allout)

;;;_ + Topic encryption
;;;_  = allout-topic-encryption-bullet
(defcustom allout-topic-encryption-bullet "~"
  "*Bullet signifying encryption of the entry's body."
  :type '(choice (const nil) string)
  :group 'allout)
;;;_  = allout-default-encryption-scheme
(defcustom allout-default-encryption-scheme 'mc-scheme-gpg
  "*Default allout outline topic encryption mode.

See mailcrypt variable `mc-schemes' and mailcrypt docs for encryption schemes."
  :type 'symbol
  :group 'allout)
;;;_  = allout-key-verifier-handling
(defcustom allout-key-verifier-handling 'situate
  "*Dictate outline encryption key verifier handling.

The key verifier is string associated with a file that is encrypted with
the file's current symmetric encryption key.  It is used, if present, to
confirm that the key entered by the user is the same as the established
one, or explicitly presenting the user with the choice to go with a
new key when a difference is encountered.

The range of values are:

 situate - include key verifier string as text in the file's local-vars
           section
 transient - establish the value as a variable in the file's buffer, but
             don't preserve it as a file variable.
 disabled - don't establish or do verification.

See the docstring for the `allout-enable-file-variable-adjustment'
variable for details about allout ajustment of file variables."
  :type '(choice (const situate)
                 (const transient)
                 (const disabled))
  :group 'allout)
(make-variable-buffer-local 'allout-key-verifier-handling)
;;;_  = allout-key-hint-handling
(defcustom allout-key-hint-handling 'always
  "*Dictate outline encryption key reminder handling:

 always - always show reminder when prompting
 needed - show reminder on key entry failure
 manage - never present reminder, but still manage a file-var entry for it
 disabled - don't even manage the file variable entry

See the docstring for the `allout-enable-file-variable-adjustment'
variable for details about allout ajustment of file variables."
  :type '(choice (const always)
                 (const needed)
                 (const manage)
                 (const disabled))
  :group 'allout)
(make-variable-buffer-local 'allout-key-hint-handling)
;;;_  = allout-encrypt-unencrypted-on-saves
(defcustom allout-encrypt-unencrypted-on-saves 'except-current
  "*When saving, should topics pending encryption be encrypted?

The idea is to prevent file-system exposure of any un-encrypted stuff, and
mostly covers both deliberate file writes and auto-saves.

 - Yes: encrypt all topics pending encryption, even if it's the one
        currently being edited.  \(In that case, the currently edited topic
        will be automatically decrypted before any user interaction, so they
        can continue editing but the copy on the file system will be
        encrypted.)
        Auto-saves will use the \"All except current topic\" mode if this
        one is selected, to avoid practical difficulties - see below.
 - All except current topic: skip the topic currently being edited, even if
       it's pending encryption.  This may expose the current topic on the
       file sytem, but avoids the nuisance of prompts for the encryption
       key in the middle of editing for, eg, autosaves.
       This mode is used for auto-saves for both this option and \"Yes\".
 - No: leave it to the user to encrypt any unencrypted topics.

For practical reasons, auto-saves always use the 'except-current policy
when auto-encryption is enabled.  \(Otherwise, spurious key prompts and
unavoidable timing collisions are too disruptive.)  If security for a file
requires that even the current topic is never auto-saved in the clear,
disable auto-saves for that file."

  :type '(choice (const :tag "Yes" t)
                 (const :tag "All except current topic" except-current)
                 (const :tag "No" nil))
  :group 'allout)
(make-variable-buffer-local 'allout-encrypt-unencrypted-on-saves)

;;;_ + Miscellaneous customization

;;;_  = allout-command-prefix
(defcustom allout-command-prefix "\C-c"
  "*Key sequence to be used as prefix for outline mode command key bindings."
  :type 'string
  :group 'allout)

;;;_  = allout-keybindings-list
;;; You have to reactivate allout-mode - `(allout-mode t)' - to
;;; institute changes to this var.
(defvar allout-keybindings-list ()
  "*List of `allout-mode' key / function bindings, for `allout-mode-map'.

String or vector key will be prefaced with `allout-command-prefix',
unless optional third, non-nil element is present.")
(setq allout-keybindings-list
      '(
                                        ; Motion commands:
        ("\C-n" allout-next-visible-heading)
        ("\C-p" allout-previous-visible-heading)
        ("\C-u" allout-up-current-level)
        ("\C-f" allout-forward-current-level)
        ("\C-b" allout-backward-current-level)
        ("\C-a" allout-beginning-of-current-entry)
        ("\C-e" allout-end-of-entry)
                                        ; Exposure commands:
        ("\C-i" allout-show-children)
        ("\C-s" allout-show-current-subtree)
        ("\C-h" allout-hide-current-subtree)
        ("h" allout-hide-current-subtree)
        ("\C-o" allout-show-current-entry)
        ("!" allout-show-all)
        ("x" allout-toggle-current-subtree-encryption)
                                        ; Alteration commands:
        (" " allout-open-sibtopic)
        ("." allout-open-subtopic)
        ("," allout-open-supertopic)
        ("'" allout-shift-in)
        (">" allout-shift-in)
        ("<" allout-shift-out)
        ("\C-m" allout-rebullet-topic)
        ("*" allout-rebullet-current-heading)
        ("#" allout-number-siblings)
        ("\C-k" allout-kill-line t)
        ("\C-y" allout-yank t)
        ("\M-y" allout-yank-pop t)
        ("\C-k" allout-kill-topic)
                                        ; Miscellaneous commands:
	;([?\C-\ ] allout-mark-topic)
        ("@" allout-resolve-xref)
        ("=c" allout-copy-exposed-to-buffer)
        ("=i" allout-indented-exposed-to-buffer)
	("=t" allout-latexify-exposed)
	("=p" allout-flatten-exposed-to-buffer)))

;;;_  = allout-isearch-dynamic-expose
(defcustom allout-isearch-dynamic-expose t
  "*Non-nil enable dynamic exposure of hidden incremental-search
targets as they're encountered."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-isearch-dynamic-expose)

;;;_  = allout-use-hanging-indents
(defcustom allout-use-hanging-indents t
  "*If non-nil, topic body text auto-indent defaults to indent of the header.
Ie, it is indented to be just past the header prefix.  This is
relevant mostly for use with indented-text-mode, or other situations
where auto-fill occurs.

\[This feature no longer depends in any way on the `filladapt.el'
lisp-archive package.\]"
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-use-hanging-indents)

;;;_  = allout-reindent-bodies
(defcustom allout-reindent-bodies (if allout-use-hanging-indents
				    'text)
  "*Non-nil enables auto-adjust of topic body hanging indent with depth shifts.

When active, topic body lines that are indented even with or beyond
their topic header are reindented to correspond with depth shifts of
the header.

A value of t enables reindent in non-programming-code buffers, ie
those that do not have the variable `comment-start' set.  A value of
`force' enables reindent whether or not `comment-start' is set."
  :type '(choice (const nil) (const t) (const text) (const force))
  :group 'allout)

(make-variable-buffer-local 'allout-reindent-bodies)

;;;_  = allout-enable-file-variable-adjustment
(defcustom allout-enable-file-variable-adjustment t
  "*If non-nil, some allout outline actions can edit Emacs file variables text.

This can range from changes to existing entries, addition of new ones,
and creation of a new local variables section when necessary.

Emacs file variables adjustments are also inhibited if `enable-local-variables'
is nil.

Operations potentially causing edits include allout encryption routines.
See the docstring for `allout-toggle-current-subtree-encryption' for
details."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-enable-file-variable-adjustment)

;;;_* CODE - no user customizations below.

;;;_ #1 Internal Outline Formatting and Configuration
;;;_  : Version
;;;_   = allout-version
(defvar allout-version
  (let ((rcs-rev "$Revision$"))
    (condition-case err
	(save-match-data
	  (string-match "Revision: \\([0-9]+\\.[0-9]+\\)" rcs-rev)
	  (substring rcs-rev (match-beginning 1) (match-end 1)))
      ('error rcs-rev)))
  "Revision number of currently loaded outline package.  \(allout.el)")
;;;_   > allout-version
(defun allout-version (&optional here)
  "Return string describing the loaded outline version."
  (interactive "P")
  (let ((msg (concat "Allout Outline Mode v " allout-version)))
    (if here (insert msg))
    (message "%s" msg)
    msg))
;;;_  : Topic header format
;;;_   = allout-regexp
(defvar allout-regexp ""
  "*Regular expression to match the beginning of a heading line.

Any line whose beginning matches this regexp is considered a
heading.  This var is set according to the user configuration vars
by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-regexp)
;;;_   = allout-bullets-string
(defvar allout-bullets-string ""
  "A string dictating the valid set of outline topic bullets.

This var should *not* be set by the user - it is set by `set-allout-regexp',
and is produced from the elements of `allout-plain-bullets-string'
and `allout-distinctive-bullets-string'.")
(make-variable-buffer-local 'allout-bullets-string)
;;;_   = allout-bullets-string-len
(defvar allout-bullets-string-len 0
  "Length of current buffers' `allout-plain-bullets-string'.")
(make-variable-buffer-local 'allout-bullets-string-len)
;;;_   = allout-line-boundary-regexp
(defvar allout-line-boundary-regexp ()
  "`allout-regexp' with outline style beginning-of-line anchor.

\(Ie, C-j, *or* C-m, for prefixes of hidden topics).  This is properly
set when `allout-regexp' is produced by `set-allout-regexp', so
that (match-beginning 2) and (match-end 2) delimit the prefix.")
(make-variable-buffer-local 'allout-line-boundary-regexp)
;;;_   = allout-bob-regexp
(defvar allout-bob-regexp ()
  "Like `allout-line-boundary-regexp', for headers at beginning of buffer.
\(match-beginning 2) and \(match-end 2) delimit the prefix.")
(make-variable-buffer-local 'allout-bob-regexp)
;;;_   = allout-header-subtraction
(defvar allout-header-subtraction (1- (length allout-header-prefix))
  "Allout-header prefix length to subtract when computing topic depth.")
(make-variable-buffer-local 'allout-header-subtraction)
;;;_   = allout-plain-bullets-string-len
(defvar allout-plain-bullets-string-len (length allout-plain-bullets-string)
  "Length of `allout-plain-bullets-string', updated by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-plain-bullets-string-len)


;;;_   X allout-reset-header-lead (header-lead)
(defun allout-reset-header-lead (header-lead)
  "*Reset the leading string used to identify topic headers."
  (interactive "sNew lead string: ")
  (setq allout-header-prefix header-lead)
  (setq allout-header-subtraction (1- (length allout-header-prefix)))
  (set-allout-regexp))
;;;_   X allout-lead-with-comment-string (header-lead)
(defun allout-lead-with-comment-string (&optional header-lead)
  "*Set the topic-header leading string to specified string.

Useful when for encapsulating outline structure in programming
language comments.  Returns the leading string."

  (interactive "P")
  (if (not (stringp header-lead))
      (setq header-lead (read-string
                         "String prefix for topic headers: ")))
  (setq allout-reindent-bodies nil)
  (allout-reset-header-lead header-lead)
  header-lead)
;;;_   > allout-infer-header-lead ()
(defun allout-infer-header-lead ()
  "Determine appropriate `allout-header-prefix'.

Works according to settings of:

       `comment-start'
       `allout-header-prefix' (default)
       `allout-use-mode-specific-leader'
and    `allout-mode-leaders'.

Apply this via \(re)activation of `allout-mode', rather than
invoking it directly."
  (let* ((use-leader (and (boundp 'allout-use-mode-specific-leader)
			  (if (or (stringp allout-use-mode-specific-leader)
				  (memq allout-use-mode-specific-leader
					'(allout-mode-leaders
					  comment-start
					  t)))
			      allout-use-mode-specific-leader
			    ;; Oops - garbled value, equate with effect of 't:
			    t)))
	 (leader
	  (cond
	   ((not use-leader) nil)
	   ;; Use the explicitly designated leader:
	   ((stringp use-leader) use-leader)
	   (t (or (and (memq use-leader '(t allout-mode-leaders))
		       ;; Get it from outline mode leaders?
		       (cdr (assq major-mode allout-mode-leaders)))
		  ;; ... didn't get from allout-mode-leaders...
		  (and (memq use-leader '(t comment-start))
		       comment-start
		       ;; Use comment-start, maybe tripled, and with
		       ;; underscore:
		       (concat
			(if (string= " "
				     (substring comment-start
						(1- (length comment-start))))
			    ;; Use comment-start, sans trailing space:
			    (substring comment-start 0 -1)
			  (concat comment-start comment-start comment-start))
			;; ... and append underscore, whichever:
			"_")))))))
    (if (not leader)
	nil
      (if (string= leader allout-header-prefix)
	  nil				; no change, nothing to do.
	(setq allout-header-prefix leader)
	allout-header-prefix))))
;;;_   > allout-infer-body-reindent ()
(defun allout-infer-body-reindent ()
  "Determine proper setting for `allout-reindent-bodies'.

Depends on default setting of `allout-reindent-bodies' \(which see)
and presence of setting for `comment-start', to tell whether the
file is programming code."
  (if (and allout-reindent-bodies
	   comment-start
	   (not (eq 'force allout-reindent-bodies)))
      (setq allout-reindent-bodies nil)))
;;;_   > set-allout-regexp ()
(defun set-allout-regexp ()
  "Generate proper topic-header regexp form for outline functions.

Works with respect to `allout-plain-bullets-string' and
`allout-distinctive-bullets-string'."

  (interactive)
  ;; Derive allout-bullets-string from user configured components:
  (setq allout-bullets-string "")
  (let ((strings (list 'allout-plain-bullets-string
                       'allout-distinctive-bullets-string
                       'allout-primary-bullet))
        cur-string
        cur-len
        cur-char
        cur-char-string
        index
        new-string)
    (while strings
      (setq new-string "") (setq index 0)
      (setq cur-len (length (setq cur-string (symbol-value (car strings)))))
      (while (< index cur-len)
        (setq cur-char (aref cur-string index))
        (setq allout-bullets-string
              (concat allout-bullets-string
                      (cond
                                        ; Single dash would denote a
                                        ; sequence, repeated denotes
                                        ; a dash:
                       ((eq cur-char ?-) "--")
                                        ; literal close-square-bracket
                                        ; doesn't work right in the
                                        ; expr, exclude it:
                       ((eq cur-char ?\]) "")
                       (t (regexp-quote  (char-to-string cur-char))))))
        (setq index (1+ index)))
      (setq strings (cdr strings)))
    )
  ;; Derive next for repeated use in allout-pending-bullet:
  (setq allout-plain-bullets-string-len (length allout-plain-bullets-string))
  (setq allout-header-subtraction (1- (length allout-header-prefix)))
  ;; Produce the new allout-regexp:
  (setq allout-regexp (concat "\\(\\"
                               allout-header-prefix
                               "[ \t]*["
                               allout-bullets-string
                               "]\\)\\|\\"
                               allout-primary-bullet
                               "+\\|\^l"))
  (setq allout-line-boundary-regexp
        (concat "\\([\n\r]\\)\\(" allout-regexp "\\)"))
  (setq allout-bob-regexp
        (concat "\\(\\`\\)\\(" allout-regexp "\\)"))
  )
;;;_  : Key bindings
;;;_   = allout-mode-map
(defvar allout-mode-map nil "Keybindings for (allout) outline minor mode.")
;;;_   > produce-allout-mode-map (keymap-alist &optional base-map)
(defun produce-allout-mode-map (keymap-list &optional base-map)
  "Produce keymap for use as allout-mode-map, from KEYMAP-LIST.

Built on top of optional BASE-MAP, or empty sparse map if none specified.
See doc string for allout-keybindings-list for format of binding list."
  (let ((map (or base-map (make-sparse-keymap)))
	(pref (list allout-command-prefix)))
    (mapcar (function
	     (lambda (cell)
	       (let ((add-pref (null (cdr (cdr cell))))
		     (key-suff (list (car cell))))
		 (apply 'define-key
			(list map
			      (apply 'concat (if add-pref
						 (append pref key-suff)
					       key-suff))
			      (car (cdr cell)))))))
	    keymap-list)
    map))
;;;_   = allout-prior-bindings - being deprecated.
(defvar allout-prior-bindings nil
  "Variable for use in V18, with allout-added-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_   = allout-added-bindings - being deprecated
(defvar allout-added-bindings nil
  "Variable for use in V18, with allout-prior-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_  : Menu bar
(defvar allout-mode-exposure-menu)
(defvar allout-mode-editing-menu)
(defvar allout-mode-navigation-menu)
(defvar allout-mode-misc-menu)
(defun produce-allout-mode-menubar-entries ()
  (require 'easymenu)
  (easy-menu-define allout-mode-exposure-menu
		    allout-mode-map
		    "Allout outline exposure menu."
		    '("Exposure"
		      ["Show Entry" allout-show-current-entry t]
		      ["Show Children" allout-show-children t]
		      ["Show Subtree" allout-show-current-subtree t]
		      ["Hide Subtree" allout-hide-current-subtree t]
		      ["Hide Leaves" allout-hide-current-leaves t]
		      "----"
		      ["Show All" allout-show-all t]))
  (easy-menu-define allout-mode-editing-menu
		    allout-mode-map
		    "Allout outline editing menu."
		    '("Headings"
		      ["Open Sibling" allout-open-sibtopic t]
		      ["Open Subtopic" allout-open-subtopic t]
		      ["Open Supertopic" allout-open-supertopic t]
		      "----"
		      ["Shift Topic In" allout-shift-in t]
		      ["Shift Topic Out" allout-shift-out t]
		      ["Rebullet Topic" allout-rebullet-topic t]
		      ["Rebullet Heading" allout-rebullet-current-heading t]
		      ["Number Siblings" allout-number-siblings t]
		      "----"
                      ["Toggle Topic Encryption"
                       allout-toggle-current-subtree-encryption
                       (> (allout-current-depth) 1)]))
  (easy-menu-define allout-mode-navigation-menu
		    allout-mode-map
		    "Allout outline navigation menu."
		    '("Navigation"
		      ["Next Visible Heading" allout-next-visible-heading t]
		      ["Previous Visible Heading"
		       allout-previous-visible-heading t]
		      "----"
		      ["Up Level" allout-up-current-level t]
		      ["Forward Current Level" allout-forward-current-level t]
		      ["Backward Current Level"
		       allout-backward-current-level t]
		      "----"
		      ["Beginning of Entry"
		       allout-beginning-of-current-entry t]
		      ["End of Entry" allout-end-of-entry t]
		      ["End of Subtree" allout-end-of-current-subtree t]))
  (easy-menu-define allout-mode-misc-menu
		    allout-mode-map
		    "Allout outlines miscellaneous bindings."
		    '("Misc"
		      ["Version" allout-version t]
		      "----"
		      ["Duplicate Exposed" allout-copy-exposed-to-buffer t]
		      ["Duplicate Exposed, numbered"
		       allout-flatten-exposed-to-buffer t]
		      ["Duplicate Exposed, indented"
		       allout-indented-exposed-to-buffer t]
		      "----"
		      ["Set Header Lead" allout-reset-header-lead t]
		      ["Set New Exposure" allout-expose-topic t])))
;;;_  : Mode-Specific Variable Maintenance Utilities
;;;_   = allout-mode-prior-settings
(defvar allout-mode-prior-settings nil
  "Internal `allout-mode' use; settings to be resumed on mode deactivation.")
(make-variable-buffer-local 'allout-mode-prior-settings)
;;;_   > allout-resumptions (name &optional value)
(defun allout-resumptions (name &optional value)

  "Registers or resumes settings over `allout-mode' activation/deactivation.

First arg is NAME of variable affected.  Optional second arg is list
containing allout-mode-specific VALUE to be imposed on named
variable, and to be registered.  (It's a list so you can specify
registrations of null values.)  If no value is specified, the
registered value is returned (encapsulated in the list, so the caller
can distinguish nil vs no value), and the registration is popped
from the list."

  (let ((on-list (assq name allout-mode-prior-settings))
        prior-capsule                   ; By `capsule' i mean a list
                                        ; containing a value, so we can
                                        ; distinguish nil from no value.
        )

    (if value

        ;; Registering:
        (progn
          (if on-list
              nil 	; Already preserved prior value - don't mess with it.
            ;; Register the old value, or nil if previously unbound:
            (setq allout-mode-prior-settings
                  (cons (list name
                              (if (boundp name) (list (symbol-value name))))
                        allout-mode-prior-settings)))
                                        ; And impose the new value, locally:
	  (progn (make-local-variable name)
		 (set name (car value))))

      ;; Relinquishing:
      (if (not on-list)

          ;; Oops, not registered - leave it be:
          nil

        ;; Some registration:
                                        ; reestablish it:
        (setq prior-capsule (car (cdr on-list)))
        (if prior-capsule
            (set name (car prior-capsule)) ; Some prior value - reestablish it.
          (makunbound name))		; Previously unbound - demolish var.
                                        ; Remove registration:
        (let (rebuild)
          (while allout-mode-prior-settings
            (if (not (eq (car allout-mode-prior-settings)
                         on-list))
                (setq rebuild
                      (cons (car allout-mode-prior-settings)
                            rebuild)))
            (setq allout-mode-prior-settings
                  (cdr allout-mode-prior-settings)))
          (setq allout-mode-prior-settings rebuild)))))
  )
;;;_  : Mode-specific incidentals
;;;_   = allout-pre-was-isearching nil
(defvar allout-pre-was-isearching nil
  "Cue for isearch-dynamic-exposure mechanism, implemented in
allout-pre- and -post-command-hooks.")
(make-variable-buffer-local 'allout-pre-was-isearching)
;;;_   = allout-isearch-prior-pos nil
(defvar allout-isearch-prior-pos nil
  "Cue for isearch-dynamic-exposure tracking, used by
`allout-isearch-expose'.")
(make-variable-buffer-local 'allout-isearch-prior-pos)
;;;_   = allout-isearch-did-quit
(defvar allout-isearch-did-quit nil
  "Distinguishes isearch conclusion and cancellation.

Maintained by allout-isearch-abort \(which is wrapped around the real
isearch-abort), and monitored by allout-isearch-expose for action.")
(make-variable-buffer-local 'allout-isearch-did-quit)
;;;_   > allout-unprotected (expr)
(defmacro allout-unprotected (expr)
  "Enable internal outline operations to alter read-only text."
  `(let ((was-inhibit-r-o inhibit-read-only))
     (unwind-protect
         (progn
           (setq inhibit-read-only t)
           ,expr)
       (setq inhibit-read-only was-inhibit-r-o)
       )
     )
  )
;;;_   = allout-undo-aggregation
(defvar allout-undo-aggregation 30
  "Amount of successive self-insert actions to bunch together per undo.

This is purely a kludge variable, regulating the compensation for a bug in
the way that `before-change-functions' and undo interact.")
(make-variable-buffer-local 'allout-undo-aggregation)
;;;_   = file-var-bug hack
(defvar allout-v18/19-file-var-hack nil
  "Horrible hack used to prevent invalid multiple triggering of outline
mode from prop-line file-var activation.  Used by `allout-mode' function
to track repeats.")
;;;_   = allout-file-key-verifier-string
(defvar allout-file-key-verifier-string nil
  "Name for use as a file variable for verifying encryption key across
sessions.")
(make-variable-buffer-local 'allout-file-key-verifier-string)
;;;_   = allout-encryption-scheme
(defvar allout-encryption-scheme nil
  "*Allout outline topic encryption scheme pending for the current buffer.

Intended as a file-specific (buffer local) setting, it defaults to the
value of allout-default-encryption-scheme if nil.")
(make-variable-buffer-local 'allout-encryption-scheme)
;;;_   = allout-key-verifier-string
(defvar allout-key-verifier-string nil
  "Setting used to test solicited encryption keys against that already
associated with a file.

It consists of an encrypted random string useful only to verify that a key
entered by the user is effective for decryption.  The key itself is \*not*
recorded in the file anywhere, and the encrypted contents are random binary
characters to avoid exposing greater susceptibility to search attacks.

The verifier string is retained as an Emacs file variable, as well as in
the emacs buffer state, if file variable adjustments are enabled.  See
`allout-enable-file-variable-adjustment' for details about that.")
(make-variable-buffer-local 'allout-key-verifier-string)
(setq-default allout-key-verifier-string nil)
;;;_   = allout-key-hint-string
(defvar allout-key-hint-string ""
  "Variable used to retain a reminder string for a file's encryption key.

See the description of `allout-key-hint-handling' for details about how
the reminder is deployed.

The hint is retained as an Emacs file variable, as well as in the emacs buffer
state, if file variable adjustments are enabled.  See
`allout-enable-file-variable-adjustment' for details about that.")
(make-variable-buffer-local 'allout-key-hint-string)
(setq-default allout-key-hint-string "")
;;;_   = allout-after-save-decrypt
(defvar allout-after-save-decrypt nil
  "Internal variable, is nil or has the value of two points:

 - the location of a topic to be decrypted after saving is done
 - where to situate the cursor after the decryption is performed

This is used to decrypt the topic that was currently being edited, if it
was encrypted automatically as part of a file write or autosave.")
(make-variable-buffer-local 'allout-after-save-decrypt)
;;;_   > allout-write-file-hook-handler ()
(defun allout-write-file-hook-handler ()
  "Implement `allout-encrypt-unencrypted-on-saves' policy for file writes."

  (if (or (not (boundp 'allout-encrypt-unencrypted-on-saves))
          (not allout-encrypt-unencrypted-on-saves))
      nil
    (let ((except-mark (and (equal allout-encrypt-unencrypted-on-saves
                                   'except-current)
                            (point-marker))))
      (if (save-excursion (goto-char (point-min))
                          (allout-next-topic-pending-encryption except-mark))
          (progn
            (message "auto-encrypting pending topics")
            (sit-for 2)
            (condition-case failure
                (setq allout-after-save-decrypt
                      (allout-encrypt-decrypted except-mark))
              (error (progn
                       (message
                        "allout-write-file-hook-handler suppressing error %s"
                        failure)
                       (sit-for 2))))))
      ))
    nil)
;;;_   > allout-auto-save-hook-handler ()
(defun allout-auto-save-hook-handler ()
  "Implement `allout-encrypt-unencrypted-on-saves' policy for auto saves."

  (if  allout-encrypt-unencrypted-on-saves
      ;; Always implement 'except-current policy when enabled.
      (let ((allout-encrypt-unencrypted-on-saves 'except-current))
        (allout-write-file-hook-handler))))
;;;_   > allout-after-saves-handler ()
(defun allout-after-saves-handler ()
  "Decrypt topic encrypted for save, if it's currently being edited.

Ie, if it was pending encryption and contained the point in its body before
the save.

We use values stored in `allout-after-save-decrypt' to locate the topic
and the place for the cursor after the decryption is done."
  (if (not (and (allout-mode-p)
                (boundp 'allout-after-save-decrypt)
                allout-after-save-decrypt))
      t
    (goto-char (car allout-after-save-decrypt))
    (let ((was-modified (buffer-modified-p)))
      (allout-toggle-current-subtree-encryption)
      (if (not was-modified)
          (set-buffer-modified-p nil)))
    (goto-char (cadr allout-after-save-decrypt))
    (setq allout-after-save-decrypt nil))
  )

;;;_ #2 Mode activation
;;;_  = allout-mode
(defvar allout-mode () "Allout outline mode minor-mode flag.")
(make-variable-buffer-local 'allout-mode)
;;;_  > allout-mode-p ()
(defmacro allout-mode-p ()
  "Return t if `allout-mode' is active in current buffer."
  'allout-mode)
;;;_  = allout-explicitly-deactivated
(defvar allout-explicitly-deactivated nil
  "If t, `allout-mode's last deactivation was deliberate.
So `allout-post-command-business' should not reactivate it...")
(make-variable-buffer-local 'allout-explicitly-deactivated)
;;;_  > allout-init (&optional mode)
(defun allout-init (&optional mode)
  "Prime `allout-mode' to enable/disable auto-activation, wrt `allout-layout'.

MODE is one of the following symbols:

 - nil \(or no argument) deactivate auto-activation/layout;
 - `activate', enable auto-activation only;
 - `ask', enable auto-activation, and enable auto-layout but with
   confirmation for layout operation solicited from user each time;
 - `report', just report and return the current auto-activation state;
 - anything else \(eg, t) for auto-activation and auto-layout, without
   any confirmation check.

Use this function to setup your Emacs session for automatic activation
of allout outline mode, contingent to the buffer-specific setting of
the `allout-layout' variable.  (See `allout-layout' and
`allout-expose-topic' docstrings for more details on auto layout).

`allout-init' works by setting up (or removing) the `allout-mode'
find-file-hook, and giving `allout-auto-activation' a suitable
setting.

To prime your Emacs session for full auto-outline operation, include
the following two lines in your Emacs init file:

\(require 'allout)
\(allout-init t)"

  (interactive)
  (if (interactive-p)
      (progn
	(setq mode
	      (completing-read
	       (concat "Select outline auto setup mode "
		       "(empty for report, ? for options) ")
	       '(("nil")("full")("activate")("deactivate")
		 ("ask") ("report") (""))
	       nil
	       t))
	(if (string= mode "")
	    (setq mode 'report)
	  (setq mode (intern-soft mode)))))
  (let
      ;; convenience aliases, for consistent ref to respective vars:
      ((hook 'allout-find-file-hook)
       (curr-mode 'allout-auto-activation))

    (cond ((not mode)
	   (setq find-file-hooks (delq hook find-file-hooks))
	   (if (interactive-p)
	       (message "Allout outline mode auto-activation inhibited.")))
	  ((eq mode 'report)
	   (if (not (memq hook find-file-hooks))
	       (allout-init nil)
	     ;; Just punt and use the reports from each of the modes:
	     (allout-init (symbol-value curr-mode))))
	  (t (add-hook 'find-file-hooks hook)
	     (set curr-mode		; `set', not `setq'!
		  (cond ((eq mode 'activate)
			 (message
			  "Outline mode auto-activation enabled.")
			 'activate)
			((eq mode 'report)
			 ;; Return the current mode setting:
			 (allout-init mode))
			((eq mode 'ask)
			 (message
			  (concat "Outline mode auto-activation and "
				  "-layout \(upon confirmation) enabled."))
			 'ask)
			((message
			  "Outline mode auto-activation and -layout enabled.")
			 'full)))))))

;;;_  > allout-setup-menubar ()
(defun allout-setup-menubar ()
  "Populate the current buffer's menubar with `allout-mode' stuff."
  (let ((menus (list allout-mode-exposure-menu
		     allout-mode-editing-menu
		     allout-mode-navigation-menu
		     allout-mode-misc-menu))
	cur)
    (while menus
      (setq cur (car menus)
	    menus (cdr menus))
      (easy-menu-add cur))))
;;;_  > allout-mode (&optional toggle)
;;;_   : Defun:
(defun allout-mode (&optional toggle)
;;;_    . Doc string:
  "Toggle minor mode for controlling exposure and editing of text outlines.

Optional arg forces mode to re-initialize iff arg is positive num or
symbol.  Allout outline mode always runs as a minor mode.

Allout outline mode provides extensive outline oriented formatting and
manipulation.  It enables structural editing of outlines, as well as
navigation and exposure.  It also is specifically aimed at
accommodating syntax-sensitive text like programming languages.  \(For
an example, see the allout code itself, which is organized as an allout
outline.)

In addition to outline navigation and exposure, allout includes:

 - topic-oriented repositioning, promotion/demotion, cut, and paste
 - integral outline exposure-layout
 - incremental search with dynamic exposure and reconcealment of hidden text
 - automatic topic-number maintenance
 - easy topic encryption and decryption
 - \"Hot-spot\" operation, for single-keystroke maneuvering and
    exposure control.  \(See the allout-mode docstring.)

and many other features.

Below is a description of the bindings, and then explanation of
special `allout-mode' features and terminology.  See also the outline
menubar additions for quick reference to many of the features, and see
the docstring of the function `allout-init' for instructions on
priming your emacs session for automatic activation of `allout-mode'.


The bindings are dictated by the `allout-keybindings-list' and
`allout-command-prefix' variables.

	Navigation:				   Exposure Control:
	----------                                 ----------------
C-c C-n allout-next-visible-heading     | C-c C-h allout-hide-current-subtree
C-c C-p allout-previous-visible-heading | C-c C-i allout-show-children
C-c C-u allout-up-current-level         | C-c C-s allout-show-current-subtree
C-c C-f allout-forward-current-level    | C-c C-o allout-show-current-entry
C-c C-b allout-backward-current-level   | ^U C-c C-s allout-show-all
C-c C-e allout-end-of-entry             |	   allout-hide-current-leaves
C-c C-a allout-beginning-of-current-entry, alternately, goes to hot-spot

	Topic Header Production:
	-----------------------
C-c<SP>	allout-open-sibtopic	Create a new sibling after current topic.
C-c .	allout-open-subtopic	... an offspring of current topic.
C-c ,	allout-open-supertopic	... a sibling of the current topic's parent.

	Topic Level and Prefix Adjustment:
	---------------------------------
C-c >	allout-shift-in	Shift current topic and all offspring deeper.
C-c <	allout-shift-out	... less deep.
C-c<CR>	allout-rebullet-topic	Reconcile bullets of topic and its offspring
				- distinctive bullets are not changed, others
				  alternated according to nesting depth.
C-c b	allout-rebullet-current-heading Prompt for alternate bullet for
					 current topic.
C-c #	allout-number-siblings	Number bullets of topic and siblings - the
				offspring are not affected.  With repeat
				count, revoke numbering.

	Topic-oriented Killing and Yanking:
	----------------------------------
C-c C-k	allout-kill-topic	Kill current topic, including offspring.
C-k	allout-kill-line	Like kill-line, but reconciles numbering, etc.
C-y	allout-yank		Yank, adjusting depth of yanked topic to
				depth of heading if yanking into bare topic
				heading (ie, prefix sans text).
M-y	allout-yank-pop	Is to allout-yank as yank-pop is to yank

	Misc commands:
	-------------
M-x outlineify-sticky		Activate outline mode for current buffer,
				and establish a default file-var setting
				for `allout-layout'.
C-c C-SPC allout-mark-topic
C-c = c	allout-copy-exposed-to-buffer
				Duplicate outline, sans concealed text, to
				buffer with name derived from derived from that
				of current buffer - \"*BUFFERNAME exposed*\".
C-c = p	allout-flatten-exposed-to-buffer
				Like above 'copy-exposed', but convert topic
				prefixes to section.subsection... numeric
				format.
ESC ESC (allout-init t)	Setup Emacs session for outline mode
				auto-activation.

                  Encrypted Entries

Outline mode supports easily togglable gpg encryption of topics, with
niceities like support for symmetric and key-pair modes, key timeout, key
consistency checking, user-provided hinting for symmetric key mode, and
auto-encryption of topics pending encryption on save.  The aim is to enable
reliable topic privacy while preventing accidents like neglected
encryption, encryption with a mistaken key, forgetting which key was used,
and other practical pitfalls.

See the `allout-toggle-current-subtree-encryption' function and
`allout-encrypt-unencrypted-on-saves' customization variable for details.

		 HOT-SPOT Operation

Hot-spot operation provides a means for easy, single-keystroke outline
navigation and exposure control.

\\<allout-mode-map>
When the text cursor is positioned directly on the bullet character of
a topic, regular characters (a to z) invoke the commands of the
corresponding allout-mode keymap control chars.  For example, \"f\"
would invoke the command typically bound to \"C-c C-f\"
\(\\[allout-forward-current-level] `allout-forward-current-level').

Thus, by positioning the cursor on a topic bullet, you can execute
the outline navigation and manipulation commands with a single
keystroke.  Non-literal chars never get this special translation, so
you can use them to get away from the hot-spot, and back to normal
operation.

Note that the command `allout-beginning-of-current-entry' \(\\[allout-beginning-of-current-entry]\)
will move to the hot-spot when the cursor is already located at the
beginning of the current entry, so you can simply hit \\[allout-beginning-of-current-entry]
twice in a row to get to the hot-spot.

			    Terminology

Topic hierarchy constituents - TOPICS and SUBTOPICS:

TOPIC:	A basic, coherent component of an Emacs outline.  It can
	contain other topics, and it can be subsumed by other topics,
CURRENT topic:
	The visible topic most immediately containing the cursor.
DEPTH:	The degree of nesting of a topic; it increases with
	containment.  Also called the:
LEVEL:	The same as DEPTH.

ANCESTORS:
	The topics that contain a topic.
PARENT:	A topic's immediate ancestor.  It has a depth one less than
	the topic.
OFFSPRING:
	The topics contained by a topic;
SUBTOPIC:
	An immediate offspring of a topic;
CHILDREN:
	The immediate offspring of a topic.
SIBLINGS:
	Topics having the same parent and depth.

Topic text constituents:

HEADER:	The first line of a topic, include the topic PREFIX and header
	text.
PREFIX: The leading text of a topic which distinguishes it from normal
        text.  It has a strict form, which consists of a prefix-lead
        string, padding, and a bullet.  The bullet may be followed by a
        number, indicating the ordinal number of the topic among its
        siblings, a space, and then the header text.

	The relative length of the PREFIX determines the nesting depth
	of the topic.
PREFIX-LEAD:
	The string at the beginning of a topic prefix, normally a `.'.
	It can be customized by changing the setting of
	`allout-header-prefix' and then reinitializing `allout-mode'.

	By setting the prefix-lead to the comment-string of a
	programming language, you can embed outline structuring in
	program code without interfering with the language processing
	of that code.  See `allout-use-mode-specific-leader'
	docstring for more detail.
PREFIX-PADDING:
	Spaces or asterisks which separate the prefix-lead and the
	bullet, according to the depth of the topic.
BULLET: A character at the end of the topic prefix, it must be one of
	the characters listed on `allout-plain-bullets-string' or
        `allout-distinctive-bullets-string'.  (See the documentation
        for these variables for more details.)  The default choice of
	bullet when generating varies in a cycle with the depth of the
	topic.
ENTRY:	The text contained in a topic before any offspring.
BODY:	Same as ENTRY.


EXPOSURE:
 	The state of a topic which determines the on-screen visibility
	of its offspring and contained text.
CONCEALED:
	Topics and entry text whose display is inhibited.  Contiguous
	units of concealed text is represented by `...' ellipses.
	(Ref the `selective-display' var.)

	Concealed topics are effectively collapsed within an ancestor.
CLOSED:	A topic whose immediate offspring and body-text is concealed.
OPEN:	A topic that is not closed, though its offspring or body may be."
;;;_    . Code
  (interactive "P")

  (let* ((active (and (not (equal major-mode 'outline))
		     (allout-mode-p)))
				       ; Massage universal-arg `toggle' val:
	 (toggle (and toggle
		     (or (and (listp toggle)(car toggle))
			 toggle)))
				       ; Activation specifically demanded?
	 (explicit-activation (or
			      ;;
			      (and toggle
				   (or (symbolp toggle)
				       (and (natnump toggle)
					    (not (zerop toggle)))))))
	 ;; allout-mode already called once during this complex command?
	 (same-complex-command (eq allout-v18/19-file-var-hack
				  (car command-history)))
	 do-layout
	 )

				       ; See comments below re v19.18,.19 bug.
    (setq allout-v18/19-file-var-hack (car command-history))

    (cond

     ;; Provision for v19.18, 19.19 bug -
     ;; Emacs v 19.18, 19.19 file-var code invokes prop-line-designated
     ;; modes twice when file is visited.  We have to avoid toggling mode
     ;; off on second invocation, so we detect it as best we can, and
     ;; skip everything.
     ((and same-complex-command		; Still in same complex command
                                        ; as last time `allout-mode' invoked.
	  active			; Already activated.
	  (not explicit-activation)	; Prop-line file-vars don't have args.
	  (string-match "^19.1[89]"	; Bug only known to be in v19.18 and
			emacs-version)); 19.19.
      t)

     ;; Deactivation:
     ((and (not explicit-activation)
	  (or active toggle))
				       ; Activation not explicitly
				       ; requested, and either in
				       ; active state or *de*activation
				       ; specifically requested:
      (setq allout-explicitly-deactivated t)
      (if (string-match "^18\." emacs-version)
				       ; Revoke those keys that remain
				       ; as we set them:
	  (let ((curr-loc (current-local-map)))
	   (mapcar (function
		    (lambda (cell)
		      (if (eq (lookup-key curr-loc (car cell))
			      (car (cdr cell)))
			  (define-key curr-loc (car cell)
			    (assq (car cell) allout-prior-bindings)))))
		   allout-added-bindings)
	   (allout-resumptions 'allout-added-bindings)
	   (allout-resumptions 'allout-prior-bindings)))

      (if allout-old-style-prefixes
	  (progn
	   (allout-resumptions 'allout-primary-bullet)
	   (allout-resumptions 'allout-old-style-prefixes)))
      (allout-resumptions 'selective-display)
      (if (and (boundp 'before-change-functions) before-change-functions)
	  (allout-resumptions 'before-change-functions))
      (setq local-write-file-hooks
	   (delq 'allout-write-file-hook-handler
		 local-write-file-hooks))
      (setq auto-save-hook
	   (delq 'allout-auto-save-hook-handler
		 auto-save-hook))
      (allout-resumptions 'paragraph-start)
      (allout-resumptions 'paragraph-separate)
      (allout-resumptions (if (string-match "^18" emacs-version)
			      'auto-fill-hook
			    'auto-fill-function))
      (allout-resumptions 'allout-former-auto-filler)
      (setq allout-mode nil))

     ;; Activation:
     ((not active)
      (setq allout-explicitly-deactivated nil)
      (if allout-old-style-prefixes
	  (progn			; Inhibit all the fancy formatting:
	   (allout-resumptions 'allout-primary-bullet '("*"))
	   (allout-resumptions 'allout-old-style-prefixes '(()))))

      (allout-infer-header-lead)
      (allout-infer-body-reindent)

      (set-allout-regexp)

				       ; Produce map from current version
				       ; of allout-keybindings-list:
      (if (boundp 'minor-mode-map-alist)

	  (progn			; V19, and maybe lucid and
				       ; epoch, minor-mode key bindings:
	   (setq allout-mode-map
		 (produce-allout-mode-map allout-keybindings-list))
	   (produce-allout-mode-menubar-entries)
	   (fset 'allout-mode-map allout-mode-map)
				       ; Include on minor-mode-map-alist,
				       ; if not already there:
	   (if (not (member '(allout-mode . allout-mode-map)
			    minor-mode-map-alist))
	       (setq minor-mode-map-alist
		     (cons '(allout-mode . allout-mode-map)
			   minor-mode-map-alist))))

				       ; V18 minor-mode key bindings:
				       ; Stash record of added bindings
				       ; for later revocation:
	(allout-resumptions 'allout-added-bindings
			    (list allout-keybindings-list))
	(allout-resumptions 'allout-prior-bindings
			    (list (current-local-map)))
				       ; and add them:
	(use-local-map (produce-allout-mode-map allout-keybindings-list
						(current-local-map)))
	)

				       ; selective-display is the
				       ; emacs conditional exposure
				       ; mechanism:
      (allout-resumptions 'selective-display '(t))
      (add-hook 'pre-command-hook 'allout-pre-command-business)
      (add-hook 'post-command-hook 'allout-post-command-business)
      (add-hook 'local-write-file-hooks 'allout-write-file-hook-handler)
      (make-variable-buffer-local 'auto-save-hook)
      (add-hook 'auto-save-hook 'allout-auto-save-hook-handler)
				       ; Custom auto-fill func, to support
				       ; respect for topic headline,
				       ; hanging-indents, etc:
      (let* ((fill-func-var (if (string-match "^18" emacs-version)
			       'auto-fill-hook
			     'auto-fill-function))
	    (fill-func (symbol-value fill-func-var)))
	;; Register prevailing fill func for use by allout-auto-fill:
	(allout-resumptions 'allout-former-auto-filler (list fill-func))
	;; Register allout-auto-fill to be used if filling is active:
	(allout-resumptions fill-func-var '(allout-auto-fill)))
      ;; Paragraphs are broken by topic headlines.
      (make-local-variable 'paragraph-start)
      (allout-resumptions 'paragraph-start
			  (list (concat paragraph-start "\\|^\\("
					allout-regexp "\\)")))
      (make-local-variable 'paragraph-separate)
      (allout-resumptions 'paragraph-separate
			  (list (concat paragraph-separate "\\|^\\("
					allout-regexp "\\)")))

      (or (assq 'allout-mode minor-mode-alist)
	  (setq minor-mode-alist
	       (cons '(allout-mode " Allout") minor-mode-alist)))

      (allout-setup-menubar)

      (if allout-layout
	  (setq do-layout t))

      (if (and allout-isearch-dynamic-expose
	       (not (fboundp 'allout-real-isearch-abort)))
	  (allout-enwrap-isearch))

      (run-hooks 'allout-mode-hook)
      (setq allout-mode t))

     ;; Reactivation:
     ((setq do-layout t)
      (allout-infer-body-reindent))
     )					; cond

    (if (and do-layout
	     allout-auto-activation
	     (listp allout-layout)
	     (and (not (eq allout-auto-activation 'activate))
		  (if (eq allout-auto-activation 'ask)
		      (if (y-or-n-p (format "Expose %s with layout '%s'? "
					    (buffer-name)
					    allout-layout))
			  t
			(message "Skipped %s layout." (buffer-name))
			nil)
		    t)))
	(save-excursion
	  (message "Adjusting '%s' exposure..." (buffer-name))
	  (goto-char 0)
	  (allout-this-or-next-heading)
	  (condition-case err
	      (progn
		(apply 'allout-expose-topic (list allout-layout))
		(message "Adjusting '%s' exposure... done." (buffer-name)))
	    ;; Problem applying exposure - notify user, but don't
	    ;; interrupt, eg, file visit:
	    (error (message "%s" (car (cdr err)))
		   (sit-for 1)))))
    allout-mode
    )					; let*
  )  					; defun
;;;_  > allout-minor-mode
(defalias 'allout-minor-mode 'allout-mode)

;;;_ #3 Internal Position State-Tracking - "allout-recent-*" funcs
;;; All the basic outline functions that directly do string matches to
;;; evaluate heading prefix location set the variables
;;; `allout-recent-prefix-beginning'  and `allout-recent-prefix-end'
;;; when successful.  Functions starting with `allout-recent-' all
;;; use this state, providing the means to avoid redundant searches
;;; for just-established data.  This optimization can provide
;;; significant speed improvement, but it must be employed carefully.
;;;_  = allout-recent-prefix-beginning
(defvar allout-recent-prefix-beginning 0
  "Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-prefix-beginning)
;;;_  = allout-recent-prefix-end
(defvar allout-recent-prefix-end 0
  "Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-prefix-end)
;;;_  = allout-recent-end-of-subtree
(defvar allout-recent-end-of-subtree 0
  "Buffer point last returned by `allout-end-of-current-subtree'.")
(make-variable-buffer-local 'allout-recent-end-of-subtree)
;;;_  > allout-prefix-data (beg end)
(defmacro allout-prefix-data (beg end)
  "Register allout-prefix state data - BEGINNING and END of prefix.

For reference by `allout-recent' funcs.  Returns BEGINNING."
  `(setq allout-recent-prefix-end ,end
         allout-recent-prefix-beginning ,beg))
;;;_  > allout-recent-depth ()
(defmacro allout-recent-depth ()
  "Return depth of last heading encountered by an outline maneuvering function.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth."

  '(max 1 (- allout-recent-prefix-end
	     allout-recent-prefix-beginning
	     allout-header-subtraction)))
;;;_  > allout-recent-prefix ()
(defmacro allout-recent-prefix ()
  "Like `allout-recent-depth', but returns text of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth."
  '(buffer-substring allout-recent-prefix-beginning
		     allout-recent-prefix-end))
;;;_  > allout-recent-bullet ()
(defmacro allout-recent-bullet ()
  "Like allout-recent-prefix, but returns bullet of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth of the most recently matched topic."
  '(buffer-substring (1- allout-recent-prefix-end)
		     allout-recent-prefix-end))

;;;_ #4 Navigation

;;;_  - Position Assessment
;;;_   : Location Predicates
;;;_    > allout-on-current-heading-p ()
(defun allout-on-current-heading-p ()
  "Return non-nil if point is on current visible topics' header line.

Actually, returns prefix beginning point."
  (save-excursion
    (beginning-of-line)
    (and (looking-at allout-regexp)
	 (allout-prefix-data (match-beginning 0) (match-end 0)))))
;;;_    > allout-on-heading-p ()
(defalias 'allout-on-heading-p 'allout-on-current-heading-p)
;;;_    > allout-e-o-prefix-p ()
(defun allout-e-o-prefix-p ()
  "True if point is located where current topic prefix ends, heading begins."
  (and (save-excursion (beginning-of-line)
		       (looking-at allout-regexp))
       (= (point)(save-excursion (allout-end-of-prefix)(point)))))
;;;_    > allout-hidden-p ()
(defmacro allout-hidden-p ()
  "True if point is in hidden text."
  '(save-excursion
     (and (re-search-backward "[\n\r]" () t)
	  (= ?\r (following-char)))))
;;;_    > allout-visible-p ()
(defmacro allout-visible-p ()
  "True if point is not in hidden text."
  (interactive)
  '(not (allout-hidden-p)))
;;;_   : Location attributes
;;;_    > allout-depth ()
(defsubst allout-depth ()
  "Like `allout-current-depth', but respects hidden as well as visible topics."
  (save-excursion
    (if (allout-goto-prefix)
	(allout-recent-depth)
      (progn
	;; Oops, no prefix, zero prefix data:
	(allout-prefix-data (point)(point))
	;; ... and return 0:
	0))))
;;;_    > allout-current-depth ()
(defmacro allout-current-depth ()
  "Return nesting depth of visible topic most immediately containing point."
  '(save-excursion
     (if (allout-back-to-current-heading)
	 (max 1
	      (- allout-recent-prefix-end
		 allout-recent-prefix-beginning
		 allout-header-subtraction))
       0)))
;;;_    > allout-get-current-prefix ()
(defun allout-get-current-prefix ()
  "Topic prefix of the current topic."
  (save-excursion
    (if (allout-goto-prefix)
	(allout-recent-prefix))))
;;;_    > allout-get-bullet ()
(defun allout-get-bullet ()
  "Return bullet of containing topic (visible or not)."
  (save-excursion
    (and (allout-goto-prefix)
	 (allout-recent-bullet))))
;;;_    > allout-current-bullet ()
(defun allout-current-bullet ()
  "Return bullet of current (visible) topic heading, or none if none found."
  (condition-case err
      (save-excursion
	(allout-back-to-current-heading)
	(buffer-substring (- allout-recent-prefix-end 1)
			  allout-recent-prefix-end))
    ;; Quick and dirty provision, ostensibly for missing bullet:
    ('args-out-of-range nil))
  )
;;;_    > allout-get-prefix-bullet (prefix)
(defun allout-get-prefix-bullet (prefix)
  "Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match allout-regexp prefix)
      (substring prefix (1- (match-end 0)) (match-end 0))))
;;;_    > allout-sibling-index (&optional depth)
(defun allout-sibling-index (&optional depth)
  "Item number of this prospective topic among its siblings.

If optional arg DEPTH is greater than current depth, then we're
opening a new level, and return 0.

If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (not depth) (= depth (allout-depth)))
           (let ((index 1))
             (while (allout-previous-sibling (allout-recent-depth) nil)
	       (setq index (1+ index)))
             index))
          ((< depth (allout-recent-depth))
           (allout-ascend-to-depth depth)
           (allout-sibling-index))
          (0))))
;;;_    > allout-topic-flat-index ()
(defun allout-topic-flat-index ()
  "Return a list indicating point's numeric section.subsect.subsubsect...
Outermost is first."
  (let* ((depth (allout-depth))
	 (next-index (allout-sibling-index depth))
	 (rev-sibls nil))
    (while (> next-index 0)
      (setq rev-sibls (cons next-index rev-sibls))
      (setq depth (1- depth))
      (setq next-index (allout-sibling-index depth)))
    rev-sibls)
  )

;;;_  - Navigation macros
;;;_   > allout-next-heading ()
(defsubst allout-next-heading ()
  "Move to the heading for the topic \(possibly invisible) before this one.

Returns the location of the heading, or nil if none found."

  (if (and (bobp) (not (eobp)))
       (forward-char 1))

  (if (re-search-forward allout-line-boundary-regexp nil 0)
      (allout-prefix-data		; Got valid location state - set vars:
       (goto-char (or (match-beginning 2)
		      allout-recent-prefix-beginning))
       (or (match-end 2) allout-recent-prefix-end))))
;;;_   : allout-this-or-next-heading
(defun allout-this-or-next-heading ()
  "Position cursor on current or next heading."
  ;; A throwaway non-macro that is defined after allout-next-heading
  ;; and usable by allout-mode.
  (if (not (allout-goto-prefix)) (allout-next-heading)))
;;;_   > allout-previous-heading ()
(defmacro allout-previous-heading ()
  "Move to the prior \(possibly invisible) heading line.

Return the location of the beginning of the heading, or nil if not found."

  '(if (bobp)
       nil
     (allout-goto-prefix)
     (if
	 ;; searches are unbounded and return nil if failed:
	 (or (re-search-backward allout-line-boundary-regexp nil 0)
	     (looking-at allout-bob-regexp))
	 (progn				; Got valid location state - set vars:
	   (allout-prefix-data
	    (goto-char (or (match-beginning 2)
			   allout-recent-prefix-beginning))
	    (or (match-end 2) allout-recent-prefix-end))))))

;;;_  - Subtree Charting
;;;_   " These routines either produce or assess charts, which are
;;; nested lists of the locations of topics within a subtree.
;;;
;;; Use of charts enables efficient navigation of subtrees, by
;;; requiring only a single regexp-search based traversal, to scope
;;; out the subtopic locations.  The chart then serves as the basis
;;; for assessment or adjustment of the subtree, without redundant
;;; traversal of the structure.

;;;_   > allout-chart-subtree (&optional levels orig-depth prev-depth)
(defun allout-chart-subtree (&optional levels orig-depth prev-depth)
  "Produce a location \"chart\" of subtopics of the containing topic.

Optional argument LEVELS specifies the depth \(relative to start
depth) for the chart.  Subsequent optional args are not for public
use.

Point is left at the end of the subtree.

Charts are used to capture outline structure, so that outline-altering
routines need assess the structure only once, and then use the chart
for their elaborate manipulations.

Topics are entered in the chart so the last one is at the car.
The entry for each topic consists of an integer indicating the point
at the beginning of the topic.  Charts for offspring consists of a
list containing, recursively, the charts for the respective subtopics.
The chart for a topics' offspring precedes the entry for the topic
itself.

The other function parameters are for internal recursion, and should
not be specified by external callers.  ORIG-DEPTH is depth of topic at
starting point, and PREV-DEPTH is depth of prior topic."

  (let ((original (not orig-depth))	; `orig-depth' set only in recursion.
	chart curr-depth)

    (if original			; Just starting?
					; Register initial settings and
					; position to first offspring:
	(progn (setq orig-depth (allout-depth))
	       (or prev-depth (setq prev-depth (1+ orig-depth)))
	       (allout-next-heading)))

    ;; Loop over the current levels' siblings.  Besides being more
    ;; efficient than tail-recursing over a level, it avoids exceeding
    ;; the typically quite constrained Emacs max-lisp-eval-depth.
    ;;
    ;; Probably would speed things up to implement loop-based stack
    ;; operation rather than recursing for lower levels.  Bah.

    (while (and (not (eobp))
					; Still within original topic?
		(< orig-depth (setq curr-depth (allout-recent-depth)))
		(cond ((= prev-depth curr-depth)
		       ;; Register this one and move on:
		       (setq chart (cons (point) chart))
		       (if (and levels (<= levels 1))
			   ;; At depth limit - skip sublevels:
			   (or (allout-next-sibling curr-depth)
			       ;; or no more siblings - proceed to
			       ;; next heading at lesser depth:
			       (while (and (<= curr-depth
					       (allout-recent-depth))
					   (allout-next-heading))))
			 (allout-next-heading)))

		      ((and (< prev-depth curr-depth)
			    (or (not levels)
				(> levels 0)))
		       ;; Recurse on deeper level of curr topic:
		       (setq chart
			     (cons (allout-chart-subtree (and levels
							       (1- levels))
							  orig-depth
							  curr-depth)
				   chart))
		       ;; ... then continue with this one.
		       )

		      ;; ... else nil if we've ascended back to prev-depth.

		      )))

    (if original			; We're at the last sibling on
					; the original level.  Position
					; to the end of it:
	(progn (and (not (eobp)) (forward-char -1))
	       (and (memq (preceding-char) '(?\n ?\r))
		    (memq (aref (buffer-substring (max 1 (- (point) 3))
						  (point))
				1)
			  '(?\n ?\r))
		    (forward-char -1))
	       (setq allout-recent-end-of-subtree (point))))

    chart				; (nreverse chart) not necessary,
					; and maybe not preferable.
    ))
;;;_   > allout-chart-siblings (&optional start end)
(defun allout-chart-siblings (&optional start end)
  "Produce a list of locations of this and succeeding sibling topics.
Effectively a top-level chart of siblings.  See `allout-chart-subtree'
for an explanation of charts."
  (save-excursion
    (if (allout-goto-prefix)
	(let ((chart (list (point))))
	  (while (allout-next-sibling)
	    (setq chart (cons (point) chart)))
	  (if chart (setq chart (nreverse chart)))))))
;;;_   > allout-chart-to-reveal (chart depth)
(defun allout-chart-to-reveal (chart depth)

  "Return a flat list of hidden points in subtree CHART, up to DEPTH.

Note that point can be left at any of the points on chart, or at the
start point."

  (let (result here)
    (while (and (or (eq depth t) (> depth 0))
		chart)
      (setq here (car chart))
      (if (listp here)
	  (let ((further (allout-chart-to-reveal here (or (eq depth t)
							   (1- depth)))))
	    ;; We're on the start of a subtree - recurse with it, if there's
	    ;; more depth to go:
	    (if further (setq result (append further result)))
	    (setq chart (cdr chart)))
	(goto-char here)
	(if (= (preceding-char) ?\r)
	    (setq result (cons here result)))
	(setq chart (cdr chart))))
    result))
;;;_   X allout-chart-spec (chart spec &optional exposing)
;; (defun allout-chart-spec (chart spec &optional exposing)
;;   "Not yet \(if ever) implemented.

;; Produce exposure directives given topic/subtree CHART and an exposure SPEC.

;; Exposure spec indicates the locations to be exposed and the prescribed
;; exposure status.  Optional arg EXPOSING is an integer, with 0
;; indicating pending concealment, anything higher indicating depth to
;; which subtopic headers should be exposed, and negative numbers
;; indicating (negative of) the depth to which subtopic headers and
;; bodies should be exposed.

;; The produced list can have two types of entries.  Bare numbers
;; indicate points in the buffer where topic headers that should be
;; exposed reside.

;;  - bare negative numbers indicates that the topic starting at the
;;    point which is the negative of the number should be opened,
;;    including their entries.
;;  - bare positive values indicate that this topic header should be
;;    opened.
;;  - Lists signify the beginning and end points of regions that should
;;    be flagged, and the flag to employ.  (For concealment: `\(\?r\)', and
;;    exposure:"
;;   (while spec
;;     (cond ((listp spec)
;; 	   )
;; 	  )
;;     (setq spec (cdr spec)))
;;   )

;;;_  - Within Topic
;;;_   > allout-goto-prefix ()
(defun allout-goto-prefix ()
  "Put point at beginning of immediately containing outline topic.

Goes to most immediate subsequent topic if none immediately containing.

Not sensitive to topic visibility.

Returns the point at the beginning of the prefix, or nil if none."

  (let (done)
    (while (and (not done)
		(re-search-backward "[\n\r]" nil 1))
      (forward-char 1)
      (if (looking-at allout-regexp)
	  (setq done (allout-prefix-data (match-beginning 0)
					  (match-end 0)))
	(forward-char -1)))
    (if (bobp)
	(cond ((looking-at allout-regexp)
	       (allout-prefix-data (match-beginning 0)(match-end 0)))
	      ((allout-next-heading))
	      (done))
      done)))
;;;_   > allout-end-of-prefix ()
(defun allout-end-of-prefix (&optional ignore-decorations)
  "Position cursor at beginning of header text.

If optional IGNORE-DECORATIONS is non-nil, put just after bullet,
otherwise skip white space between bullet and ensuing text."

  (if (not (allout-goto-prefix))
      nil
    (let ((match-data (match-data)))
      (goto-char (match-end 0))
      (if ignore-decorations
	  t
	(while (looking-at "[0-9]") (forward-char 1))
	(if (and (not (eolp)) (looking-at "\\s-")) (forward-char 1)))
      (store-match-data match-data))
    ;; Reestablish where we are:
    (allout-current-depth)))
;;;_   > allout-current-bullet-pos ()
(defun allout-current-bullet-pos ()
  "Return position of current \(visible) topic's bullet."

 (if (not (allout-current-depth))
      nil
   (1- (match-end 0))))
;;;_   > allout-back-to-current-heading ()
(defun allout-back-to-current-heading ()
  "Move to heading line of current topic, or beginning if already on the line."

  (beginning-of-line)
  (prog1 (or (allout-on-current-heading-p)
             (and (re-search-backward (concat "^\\(" allout-regexp "\\)")
                                      nil
                                      'move)
                  (allout-prefix-data (match-beginning 1)(match-end 1))))
    (if (interactive-p) (allout-end-of-prefix))))
;;;_   > allout-back-to-heading ()
(defalias 'allout-back-to-heading 'allout-back-to-current-heading)
;;;_   > allout-pre-next-preface ()
(defun allout-pre-next-preface ()
  "Skip forward to just before the next heading line.

Returns that character position."

  (if (re-search-forward allout-line-boundary-regexp nil 'move)
      (prog1 (goto-char (match-beginning 0))
             (allout-prefix-data (match-beginning 2)(match-end 2)))))
;;;_   > allout-end-of-subtree (&optional current)
(defun allout-end-of-subtree (&optional current)
  "Put point at the end of the last leaf in the containing topic.

If optional CURRENT is true (default false), then put point at the end of
the containing visible topic.

Returns the value of point."
  (interactive "P")
  (if current
      (allout-back-to-current-heading)
    (allout-goto-prefix))
  (let ((level (allout-recent-depth)))
    (allout-next-heading)
    (while (and (not (eobp))
                (> (allout-recent-depth) level))
      (allout-next-heading))
    (and (not (eobp)) (forward-char -1))
    (and (memq (preceding-char) '(?\n ?\r))
         (memq (aref (buffer-substring (max 1 (- (point) 3)) (point)) 1)
               '(?\n ?\r))
         (forward-char -1))
    (setq allout-recent-end-of-subtree (point))))
;;;_   > allout-end-of-current-subtree ()
(defun allout-end-of-current-subtree ()
  "Put point at end of last leaf in currently visible containing topic.

Returns the value of point."
  (interactive)
  (allout-end-of-subtree t))
;;;_   > allout-beginning-of-current-entry ()
(defun allout-beginning-of-current-entry ()
  "When not already there, position point at beginning of current topic header.

If already there, move cursor to bullet for hot-spot operation.
\(See `allout-mode' doc string for details on hot-spot operation.)"
  (interactive)
  (let ((start-point (point)))
    (allout-end-of-prefix)
    (if (and (interactive-p)
	     (= (point) start-point))
	(goto-char (allout-current-bullet-pos)))))
;;;_   > allout-end-of-entry ()
(defun allout-end-of-entry ()
  "Position the point at the end of the current topics' entry."
  (interactive)
  (prog1 (allout-pre-next-preface)
    (if (and (not (bobp))(looking-at "^$"))
        (forward-char -1))))
;;;_   > allout-end-of-current-heading ()
(defun allout-end-of-current-heading ()
  (interactive)
  (allout-beginning-of-current-entry)
  (re-search-forward "[\n\r]" nil t)
  (forward-char -1))
(defalias 'allout-end-of-heading 'allout-end-of-current-heading)
;;;_   > allout-get-body-text ()
(defun allout-get-body-text ()
  "Return the unmangled body text of the topic immediately containing point."
  (save-excursion
    (allout-end-of-prefix)
    (if (not (re-search-forward "[\n\r]" nil t))
        nil
      (backward-char 1)
      (let ((pre-body (point)))
        (if (not pre-body)
            nil
          (allout-end-of-entry)
          (if (not (= pre-body (point)))
              (buffer-substring-no-properties (1+ pre-body) (point))))
        )
      )
    )
  )

;;;_  - Depth-wise
;;;_   > allout-ascend-to-depth (depth)
(defun allout-ascend-to-depth (depth)
  "Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (allout-depth)))
      (let ((last-good (point)))
        (while (and (< depth (allout-depth))
                    (setq last-good (point))
                    (allout-beginning-of-level)
                    (allout-previous-heading)))
        (if (= (allout-recent-depth) depth)
            (progn (goto-char allout-recent-prefix-beginning)
                   depth)
          (goto-char last-good)
          nil))
    (if (interactive-p) (allout-end-of-prefix))))
;;;_   > allout-ascend ()
(defun allout-ascend ()
  "Ascend one level, returning t if successful, nil if not."
  (prog1
      (if (allout-beginning-of-level)
	  (allout-previous-heading))
    (if (interactive-p) (allout-end-of-prefix))))
;;;_   > allout-descend-to-depth (depth)
(defun allout-descend-to-depth (depth)
  "Descend to depth DEPTH within current topic.

Returning depth if successful, nil if not."
  (let ((start-point (point))
        (start-depth (allout-depth)))
    (while
        (and (> (allout-depth) 0)
             (not (= depth (allout-recent-depth))) ; ... not there yet
             (allout-next-heading)     ; ... go further
             (< start-depth (allout-recent-depth)))) ; ... still in topic
    (if (and (> (allout-depth) 0)
             (= (allout-recent-depth) depth))
        depth
      (goto-char start-point)
      nil))
  )
;;;_   > allout-up-current-level (arg &optional dont-complain)
(defun allout-up-current-level (arg &optional dont-complain)
  "Move out ARG levels from current visible topic.

Positions on heading line of containing topic.  Error if unable to
ascend that far, or nil if unable to ascend but optional arg
DONT-COMPLAIN is non-nil."
  (interactive "p")
  (allout-back-to-current-heading)
  (let ((present-level (allout-recent-depth))
	(last-good (point))
	failed
	return)
    ;; Loop for iterating arg:
    (while (and (> (allout-recent-depth) 1)
                (> arg 0)
                (not (bobp))
		(not failed))
      (setq last-good (point))
      ;; Loop for going back over current or greater depth:
      (while (and (not (< (allout-recent-depth) present-level))
		  (or (allout-previous-visible-heading 1)
		      (not (setq failed present-level)))))
      (setq present-level (allout-current-depth))
      (setq arg (- arg 1)))
    (if (or failed
	    (> arg 0))
	(progn (goto-char last-good)
	       (if (interactive-p) (allout-end-of-prefix))
	       (if (not dont-complain)
		   (error "Can't ascend past outermost level")
		 (if (interactive-p) (allout-end-of-prefix))
		 nil))
      (if (interactive-p) (allout-end-of-prefix))
      allout-recent-prefix-beginning)))

;;;_  - Linear
;;;_   > allout-next-sibling (&optional depth backward)
(defun allout-next-sibling (&optional depth backward)
  "Like `allout-forward-current-level', but respects invisible topics.

Traverse at optional DEPTH, or current depth if none specified.

Go backward if optional arg BACKWARD is non-nil.

Return depth if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-depth (or depth (allout-depth)))
          (start-point (point))
	  last-depth)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (allout-previous-heading)
                    (allout-next-heading))
                  (> (setq last-depth (allout-recent-depth)) start-depth)))
      (if (and (not (eobp))
               (and (> (or last-depth (allout-depth)) 0)
                    (= (allout-recent-depth) start-depth)))
          allout-recent-prefix-beginning
        (goto-char start-point)
	(if depth (allout-depth) start-depth)
        nil))))
;;;_   > allout-previous-sibling (&optional depth backward)
(defun allout-previous-sibling (&optional depth backward)
  "Like `allout-forward-current-level' backwards, respecting invisible topics.

Optional DEPTH specifies depth to traverse, default current depth.

Optional BACKWARD reverses direction.

Return depth if successful, nil otherwise."
  (allout-next-sibling depth (not backward))
  )
;;;_   > allout-snug-back ()
(defun allout-snug-back ()
  "Position cursor at end of previous topic.

Presumes point is at the start of a topic prefix."
 (if (or (bobp) (eobp))
     nil
   (forward-char -1))
 (if (or (bobp) (not (memq (preceding-char) '(?\n ?\r))))
     nil
   (forward-char -1)
   (if (or (bobp) (not (memq (preceding-char) '(?\n ?\r))))
       (forward-char -1)))
 (point))
;;;_   > allout-beginning-of-level ()
(defun allout-beginning-of-level ()
  "Go back to the first sibling at this level, visible or not."
  (allout-end-of-level 'backward))
;;;_   > allout-end-of-level (&optional backward)
(defun allout-end-of-level (&optional backward)
  "Go to the last sibling at this level, visible or not."

  (let ((depth (allout-depth)))
    (while (allout-previous-sibling depth nil))
    (prog1 (allout-recent-depth)
      (if (interactive-p) (allout-end-of-prefix)))))
;;;_   > allout-next-visible-heading (arg)
(defun allout-next-visible-heading (arg)
  "Move to the next ARG'th visible heading line, backward if arg is negative.

Move as far as possible in indicated direction \(beginning or end of
buffer) if headings are exhausted."

  (interactive "p")
  (let* ((backward (if (< arg 0) (setq arg (* -1 arg))))
	 (step (if backward -1 1))
	 (start-point (point))
	 prev got)

    (while (> arg 0)			; limit condition
      (while (and (not (if backward (bobp)(eobp))) ; boundary condition
		  ;; Move, skipping over all those concealed lines:
		  (< -1 (forward-line step))
		  (not (setq got (looking-at allout-regexp)))))
      ;; Register this got, it may be the last:
      (if got (setq prev got))
      (setq arg (1- arg)))
    (cond (got				; Last move was to a prefix:
	   (allout-prefix-data (match-beginning 0) (match-end 0))
	   (allout-end-of-prefix))
	  (prev				; Last move wasn't, but prev was:
	   (allout-prefix-data (match-beginning 0) (match-end 0)))
	  ((not backward) (end-of-line) nil))))
;;;_   > allout-previous-visible-heading (arg)
(defun allout-previous-visible-heading (arg)
  "Move to the previous heading line.

With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that `allout-regexp'
matches)."
  (interactive "p")
  (allout-next-visible-heading (- arg)))
;;;_   > allout-forward-current-level (arg)
(defun allout-forward-current-level (arg)
  "Position point at the next heading of the same level.

Takes optional repeat-count, goes backward if count is negative.

Returns resulting position, else nil if none found."
  (interactive "p")
  (let ((start-depth (allout-current-depth))
	(start-point (point))
	(start-arg arg)
	(backward (> 0 arg))
	last-depth
	(last-good (point))
	at-boundary)
    (if (= 0 start-depth)
	(error "No siblings, not in a topic..."))
    (if backward (setq arg (* -1 arg)))
    (while (not (or (zerop arg)
		    at-boundary))
      (while (and (not (if backward (bobp) (eobp)))
		  (if backward (allout-previous-visible-heading 1)
		    (allout-next-visible-heading 1))
		  (> (setq last-depth (allout-recent-depth)) start-depth)))
      (if (and last-depth (= last-depth start-depth)
	       (not (if backward (bobp) (eobp))))
	  (setq last-good (point)
		arg (1- arg))
	(setq at-boundary t)))
    (if (and (not (eobp))
	     (= arg 0)
	     (and (> (or last-depth (allout-depth)) 0)
		  (= (allout-recent-depth) start-depth)))
	allout-recent-prefix-beginning
      (goto-char last-good)
      (if (not (interactive-p))
	  nil
	(allout-end-of-prefix)
	(error "Hit %s level %d topic, traversed %d of %d requested"
	       (if backward "first" "last")
	       (allout-recent-depth)
	       (- (abs start-arg) arg)
	       (abs start-arg))))))
;;;_   > allout-backward-current-level (arg)
(defun allout-backward-current-level (arg)
  "Inverse of `allout-forward-current-level'."
  (interactive "p")
  (if (interactive-p)
      (let ((current-prefix-arg (* -1 arg)))
	(call-interactively 'allout-forward-current-level))
    (allout-forward-current-level (* -1 arg))))

;;;_ #5 Alteration

;;;_  - Fundamental
;;;_   = allout-post-goto-bullet
(defvar allout-post-goto-bullet nil
  "Outline internal var, for `allout-pre-command-business' hot-spot operation.

When set, tells post-processing to reposition on topic bullet, and
then unset it.  Set by `allout-pre-command-business' when implementing
hot-spot operation, where literal characters typed over a topic bullet
are mapped to the command of the corresponding control-key on the
`allout-mode-map'.")
(make-variable-buffer-local 'allout-post-goto-bullet)
;;;_   > allout-post-command-business ()
(defun allout-post-command-business ()
  "Outline `post-command-hook' function.

- Implement (and clear) `allout-post-goto-bullet', for hot-spot
  outline commands.

- Decrypt topic currently being edited if it was encrypted for a save.

- Massage buffer-undo-list so successive, standard character self-inserts are
  aggregated.  This kludge compensates for lack of undo bunching when
  before-change-functions is used."

					; Apply any external change func:
  (if (not (allout-mode-p))		; In allout-mode.
      nil
    (if allout-isearch-dynamic-expose
	(allout-isearch-rectification))
    ;; Undo bunching business:
    (if (and (listp buffer-undo-list)	; Undo history being kept.
	     (equal this-command 'self-insert-command)
	     (equal last-command 'self-insert-command))
	(let* ((prev-stuff (cdr buffer-undo-list))
	       (before-prev-stuff (cdr (cdr prev-stuff)))
	       cur-cell cur-from cur-to
	       prev-cell prev-from prev-to)
	  (if (and before-prev-stuff	; Goes back far enough to bother,
		   (not (car prev-stuff)) ; and break before current,
		   (not (car before-prev-stuff)) ; !and break before prev!
		   (setq prev-cell (car (cdr prev-stuff))) ; contents now,
		   (setq cur-cell (car buffer-undo-list)) ; contents prev.

		   ;; cur contents denote a single char insertion:
		   (numberp (setq cur-from (car cur-cell)))
		   (numberp (setq cur-to (cdr cur-cell)))
		   (= 1 (- cur-to cur-from))

		   ;; prev contents denote fewer than aggregate-limit
		   ;; insertions:
		   (numberp (setq prev-from (car prev-cell)))
		   (numberp (setq prev-to (cdr prev-cell)))
					; Below threshold:
		   (> allout-undo-aggregation (- prev-to prev-from)))
	      (setq buffer-undo-list
		    (cons (cons prev-from cur-to)
			  (cdr (cdr (cdr buffer-undo-list))))))))

    (if (and (boundp 'allout-after-save-decrypt)
             allout-after-save-decrypt)
        (allout-after-saves-handler))

    ;; Implement -post-goto-bullet, if set: (must be after undo business)
    (if (and allout-post-goto-bullet
	     (allout-current-bullet-pos))
	(progn (goto-char (allout-current-bullet-pos))
	       (setq allout-post-goto-bullet nil)))
    ))
;;;_   > allout-pre-command-business ()
(defun allout-pre-command-business ()
  "Outline `pre-command-hook' function for outline buffers.
Implements special behavior when cursor is on bullet character.

When the cursor is on the bullet character, self-insert characters are
reinterpreted as the corresponding control-character in the
`allout-mode-map'.  The `allout-mode' `post-command-hook' insures that
the cursor which has moved as a result of such reinterpretation is
positioned on the bullet character of the destination topic.

The upshot is that you can get easy, single (ie, unmodified) key
outline maneuvering operations by positioning the cursor on the bullet
char.  When in this mode you can use regular cursor-positioning
command/keystrokes to relocate the cursor off of a bullet character to
return to regular interpretation of self-insert characters."

  (if (not (allout-mode-p))
      ;; Shouldn't be invoked if not in allout-mode, but just in case:
      nil
    ;; Register isearch status:
    (if (and (boundp  'isearch-mode) isearch-mode)
	(setq allout-pre-was-isearching t)
      (setq allout-pre-was-isearching nil))
    ;; Hot-spot navigation provisions:
    (if (and (eq this-command 'self-insert-command)
	     (eq (point)(allout-current-bullet-pos)))
	(let* ((this-key-num (cond
			      ((numberp last-command-char)
			       last-command-char)
			      ;; Only xemacs has characterp.
			      ((and (fboundp 'characterp)
				    (characterp last-command-char))
			       (char-to-int last-command-char))
			      (t 0)))
	       mapped-binding)
	  (if (zerop this-key-num)
	      nil
					; Map upper-register literals
					; to lower register:
	    (if (<= 96 this-key-num)
		(setq this-key-num (- this-key-num 32)))
					; Check if we have a literal:
	    (if (and (<= 64 this-key-num)
		     (>= 96 this-key-num))
		(setq mapped-binding
		      (lookup-key 'allout-mode-map
				  (concat allout-command-prefix
					  (char-to-string (- this-key-num
							     64))))))
	    (if mapped-binding
		(setq allout-post-goto-bullet t
		      this-command mapped-binding)))))))
;;;_   > allout-find-file-hook ()
(defun allout-find-file-hook ()
  "Activate `allout-mode' when `allout-auto-activation', `allout-layout' non-nil.

See `allout-init' for setup instructions."
  (if (and allout-auto-activation
	   (not (allout-mode-p))
	   allout-layout)
      (allout-mode t)))
;;;_   > allout-isearch-rectification
(defun allout-isearch-rectification ()
  "Rectify outline exposure before, during, or after isearch.

Called as part of `allout-post-command-business'."

  (let ((isearching (and (boundp 'isearch-mode) isearch-mode)))
    (cond ((and isearching (not allout-pre-was-isearching))
	   (allout-isearch-expose 'start))
	  ((and isearching allout-pre-was-isearching)
	   (allout-isearch-expose 'continue))
	  ((and (not isearching) allout-pre-was-isearching)
	   (allout-isearch-expose 'final))
	  ;; Not and wasn't isearching:
	  (t (setq allout-isearch-prior-pos nil)
	     (setq allout-isearch-did-quit nil)))))
;;;_   = allout-isearch-was-font-lock
(defvar allout-isearch-was-font-lock
  (and (boundp 'font-lock-mode) font-lock-mode))
;;;_   > allout-isearch-expose (mode)
(defun allout-isearch-expose (mode)
  "MODE is either 'clear, 'start, 'continue, or 'final."
  ;; allout-isearch-prior-pos encodes exposure status of prior pos:
  ;; (pos was-vis header-pos end-pos)
  ;; pos	- point of concern
  ;; was-vis	- t, else 'topic if entire topic was exposed, 'entry otherwise
  ;; Do reclosure or prior pos, as necessary:
  (if (eq mode 'start)
      (setq allout-isearch-was-font-lock (and (boundp 'font-lock-mode)
                                               font-lock-mode)
	    font-lock-mode nil)
    (if (eq mode 'final)
	(setq font-lock-mode allout-isearch-was-font-lock))
    (if (and allout-isearch-prior-pos
	     (listp allout-isearch-prior-pos))
	;; Conceal prior peek:
	(allout-flag-region (car (cdr allout-isearch-prior-pos))
			     (car (cdr (cdr allout-isearch-prior-pos)))
			     ?\r)))
  (if (allout-visible-p)
      (setq allout-isearch-prior-pos nil)
    (if (not (eq mode 'final))
	(setq allout-isearch-prior-pos (cons (point) (allout-show-entry)))
      (if allout-isearch-did-quit
	  nil
	(setq allout-isearch-prior-pos nil)
	(allout-show-children))))
  (setq allout-isearch-did-quit nil))
;;;_   > allout-enwrap-isearch ()
(defun allout-enwrap-isearch ()
  "Impose `allout-mode' isearch-abort wrapper for dynamic exposure in isearch.

The function checks to ensure that the rebinding is done only once."

  (add-hook 'isearch-mode-end-hook 'allout-isearch-rectification)
  (if (fboundp 'allout-real-isearch-abort)
      ;;
      nil
                                        ; Ensure load of isearch-mode:
    (if (or (and (fboundp 'isearch-mode)
                 (fboundp 'isearch-abort))
            (condition-case error
                (load-library "isearch-mode")
              ('file-error (message
			    "Skipping isearch-mode provisions - %s '%s'"
			    (car (cdr error))
			    (car (cdr (cdr error))))
			   (sit-for 1)
			   ;; Inhibit subsequent tries and return nil:
			   (setq allout-isearch-dynamic-expose nil))))
        ;; Isearch-mode loaded, encapsulate specific entry points for
        ;; outline dynamic-exposure business:
        (progn
	  ;; stash crucial isearch-mode funcs under known, private
	  ;; names, then register wrapper functions under the old
	  ;; names, in their stead:
          (fset 'allout-real-isearch-abort (symbol-function 'isearch-abort))
          (fset 'isearch-abort 'allout-isearch-abort)))))
;;;_   > allout-isearch-abort ()
(defun allout-isearch-abort ()
  "Wrapper for allout-real-isearch-abort \(which see), to register
actual quits."
  (interactive)
  (setq allout-isearch-did-quit nil)
  (condition-case what
      (allout-real-isearch-abort)
    ('quit (setq allout-isearch-did-quit t)
	  (signal 'quit nil))))

;;; Prevent unnecessary font-lock while isearching!
(defvar isearch-was-font-locking nil)
(defun isearch-inhibit-font-lock ()
  "Inhibit `font-lock' while isearching - for use on `isearch-mode-hook'."
  (if (and (allout-mode-p) (boundp 'font-lock-mode) font-lock-mode)
      (setq isearch-was-font-locking t
	    font-lock-mode nil)))
(add-hook 'isearch-mode-hook 'isearch-inhibit-font-lock)
(defun isearch-reenable-font-lock ()
  "Reenable font-lock after isearching - for use on `isearch-mode-end-hook'."
  (if (and (boundp 'font-lock-mode) font-lock-mode)
      (if (and (allout-mode-p) isearch-was-font-locking)
	  (setq isearch-was-font-locking nil
		font-lock-mode t))))
(add-hook 'isearch-mode-end-hook 'isearch-reenable-font-lock)

;;;_  - Topic Format Assessment
;;;_   > allout-solicit-alternate-bullet (depth &optional current-bullet)
(defun allout-solicit-alternate-bullet (depth &optional current-bullet)

  "Prompt for and return a bullet char as an alternative to the current one.

Offer one suitable for current depth DEPTH as default."

  (let* ((default-bullet (or (and (stringp current-bullet) current-bullet)
                             (allout-bullet-for-depth depth)))
	 (sans-escapes (regexp-sans-escapes allout-bullets-string))
	 choice)
    (save-excursion
      (goto-char (allout-current-bullet-pos))
      (setq choice (solicit-char-in-string
                    (format "Select bullet: %s ('%s' default): "
                            sans-escapes
                            default-bullet)
                    sans-escapes
                    t)))
    (message "")
    (if (string= choice "") default-bullet choice))
  )
;;;_   > allout-distinctive-bullet (bullet)
(defun allout-distinctive-bullet (bullet)
  "True if BULLET is one of those on `allout-distinctive-bullets-string'."
  (string-match (regexp-quote bullet) allout-distinctive-bullets-string))
;;;_   > allout-numbered-type-prefix (&optional prefix)
(defun allout-numbered-type-prefix (&optional prefix)
  "True if current header prefix bullet is numbered bullet."
  (and allout-numbered-bullet
        (string= allout-numbered-bullet
                 (if prefix
                     (allout-get-prefix-bullet prefix)
                   (allout-get-bullet)))))
;;;_   > allout-encrypted-type-prefix (&optional prefix)
(defun allout-encrypted-type-prefix (&optional prefix)
  "True if current header prefix bullet is for an encrypted entry \(body)."
  (and allout-topic-encryption-bullet
        (string= allout-topic-encryption-bullet
                 (if prefix
                     (allout-get-prefix-bullet prefix)
                   (allout-get-bullet)))))
;;;_   > allout-bullet-for-depth (&optional depth)
(defun allout-bullet-for-depth (&optional depth)
  "Return outline topic bullet suited to optional DEPTH, or current depth."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if allout-stylish-prefixes
      (char-to-string (aref allout-plain-bullets-string
                            (% (max 0 (- depth 2))
                               allout-plain-bullets-string-len)))
    allout-primary-bullet)
  )

;;;_  - Topic Production
;;;_   > allout-make-topic-prefix (&optional prior-bullet
(defun allout-make-topic-prefix (&optional prior-bullet
                                            new
                                            depth
                                            solicit
                                            number-control
                                            index)
  ;; Depth null means use current depth, non-null means we're either
  ;; opening a new topic after current topic, lower or higher, or we're
  ;; changing level of current topic.
  ;; Solicit dominates specified bullet-char.
;;;_    . Doc string:
  "Generate a topic prefix suitable for optional arg DEPTH, or current depth.

All the arguments are optional.

PRIOR-BULLET indicates the bullet of the prefix being changed, or
nil if none.  This bullet may be preserved (other options
notwithstanding) if it is on the `allout-distinctive-bullets-string',
for instance.

Second arg NEW indicates that a new topic is being opened after the
topic at point, if non-nil.  Default bullet for new topics, eg, may
be set (contingent to other args) to numbered bullets if previous
sibling is one.  The implication otherwise is that the current topic
is being adjusted - shifted or rebulleted - and we don't consider
bullet or previous sibling.

Third arg DEPTH forces the topic prefix to that depth, regardless of
the current topics' depth.

If SOLICIT is non-nil, then the choice of bullet is solicited from
user.  If it's a character, then that character is offered as the
default, otherwise the one suited to the context \(according to
distinction or depth) is offered.  \(This overrides other options,
including, eg, a distinctive PRIOR-BULLET.)  If non-nil, then the
context-specific bullet is used.

Fifth arg, NUMBER-CONTROL, matters only if `allout-numbered-bullet'
is non-nil *and* soliciting was not explicitly invoked.  Then
NUMBER-CONTROL non-nil forces prefix to either numbered or
denumbered format, depending on the value of the sixth arg, INDEX.

\(Note that NUMBER-CONTROL does *not* apply to level 1 topics.  Sorry...)

If NUMBER-CONTROL is non-nil and sixth arg INDEX is non-nil then
the prefix of the topic is forced to be numbered.  Non-nil
NUMBER-CONTROL and nil INDEX forces non-numbered format on the
bullet.  Non-nil NUMBER-CONTROL and non-nil, non-number INDEX means
that the index for the numbered prefix will be derived, by counting
siblings back to start of level.  If INDEX is a number, then that
number is used as the index for the numbered prefix (allowing, eg,
sequential renumbering to not require this function counting back the
index for each successive sibling)."
;;;_    . Code:
  ;; The options are ordered in likely frequence of use, most common
  ;; highest, least lowest.  Ie, more likely to be doing prefix
  ;; adjustments than soliciting, and yet more than numbering.
  ;; Current prefix is least dominant, but most likely to be commonly
  ;; specified...

  (let* (body
         numbering
         denumbering
         (depth (or depth (allout-depth)))
         (header-lead allout-header-prefix)
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation - level 1:
           ((<= depth 1) (setq header-lead "") allout-primary-bullet)
                                        ; Simple, too: all asterisks:
           (allout-old-style-prefixes
            ;; Cheat - make body the whole thing, null out header-lead and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char allout-primary-bullet)))
            (setq header-lead "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   solicit)
            (let* ((got (allout-solicit-alternate-bullet depth solicit)))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and allout-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got allout-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and allout-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                allout-numbered-bullet
              (if (and prior-bullet
                       (not (string= allout-numbered-bullet
                                     prior-bullet)))
                  prior-bullet
                (allout-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (allout-depth))
                 allout-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (allout-depth))
                              (allout-ascend-to-depth depth))
                          (allout-get-bullet))))
                   (if (and sibling-bullet
                            (string= allout-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (allout-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and allout-numbered-bullet
                               (string= prior-bullet allout-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((allout-bullet-for-depth depth)))))

    (concat header-lead
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (allout-sibling-index depth)))
                                   ((allout-sibling-index))))))
    )
  )
;;;_   > allout-open-topic (relative-depth &optional before use_recent_bullet)
(defun allout-open-topic (relative-depth &optional before use_recent_bullet)
  "Open a new topic at depth DEPTH.

New topic is situated after current one, unless optional flag BEFORE
is non-nil, or unless current line is complete empty (not even
whitespace), in which case open is done on current line.

If USE_RECENT_BULLET is true, offer to use the bullet of the prior sibling.

Nuances:

- Creation of new topics is with respect to the visible topic
  containing the cursor, regardless of intervening concealed ones.

- New headers are generally created after/before the body of a
  topic.  However, they are created right at cursor location if the
  cursor is on a blank line, even if that breaks the current topic
  body.  This is intentional, to provide a simple means for
  deliberately dividing topic bodies.

- Double spacing of topic lists is preserved.  Also, the first
  level two topic is created double-spaced (and so would be
  subsequent siblings, if that's left intact).  Otherwise,
  single-spacing is used.

- Creation of sibling or nested topics is with respect to the topic
  you're starting from, even when creating backwards.  This way you
  can easily create a sibling in front of the current topic without
  having to go to its preceding sibling, and then open forward
  from there."

  (let* ((depth (+ (allout-current-depth) relative-depth))
         (opening-on-blank (if (looking-at "^\$")
                               (not (setq before nil))))
         ;; bunch o vars set while computing ref-topic
         opening-numbered
         opening-encrypted
         ref-depth
         ref-bullet
         (ref-topic (save-excursion
                      (cond ((< relative-depth 0)
                             (allout-ascend-to-depth depth))
                            ((>= relative-depth 1) nil)
                            (t (allout-back-to-current-heading)))
                      (setq ref-depth (allout-recent-depth))
                      (setq ref-bullet
                            (if (> allout-recent-prefix-end 1)
                                (allout-recent-bullet)
                              ""))
                      (setq opening-numbered
                            (save-excursion
                              (and allout-numbered-bullet
                                   (or (<= relative-depth 0)
                                       (allout-descend-to-depth depth))
                                   (if (allout-numbered-type-prefix)
                                       allout-numbered-bullet))))
                      (setq opening-encrypted
                            (save-excursion
                              (and allout-topic-encryption-bullet
                                   (or (<= relative-depth 0)
                                       (allout-descend-to-depth depth))
                                   (if (allout-numbered-type-prefix)
                                       allout-numbered-bullet))))
                      (point)))
         dbl-space
         doing-beginning)

    (if (not opening-on-blank)
                                        ; Positioning and vertical
                                        ; padding - only if not
                                        ; opening-on-blank:
        (progn
          (goto-char ref-topic)
          (setq dbl-space               ; Determine double space action:
                (or (and (<= relative-depth 0)	; not descending;
                         (save-excursion
                           ;; at b-o-b or preceded by a blank line?
                           (or (> 0 (forward-line -1))
                               (looking-at "^\\s-*$")
			       (bobp)))
                         (save-excursion
                           ;; succeeded by a blank line?
                           (allout-end-of-current-subtree)
                           (bolp)))
                    (and (= ref-depth 1)
                         (or before
                             (= depth 1)
                             (save-excursion
                               ;; Don't already have following
                               ;; vertical padding:
                               (not (allout-pre-next-preface)))))))

                                        ; Position to prior heading,
                                        ; if inserting backwards, and
					; not going outwards:
          (if (and before (>= relative-depth 0))
	      (progn (allout-back-to-current-heading)
                            (setq doing-beginning (bobp))
                            (if (not (bobp))
                                (allout-previous-heading)))
	    (if (and before (bobp))
		(allout-unprotected (allout-open-line-not-read-only))))

          (if (<= relative-depth 0)
              ;; Not going inwards, don't snug up:
              (if doing-beginning
                  (allout-unprotected
                   (if (not dbl-space)
                       (allout-open-line-not-read-only)
                     (allout-open-line-not-read-only)
                     (allout-open-line-not-read-only)))
		(if before
		    (progn (end-of-line)
			   (allout-pre-next-preface)
			   (while (= ?\r (following-char))
                             (forward-char 1))
			   (if (not (looking-at "^$"))
			       (allout-unprotected
                                (allout-open-line-not-read-only))))
		  (allout-end-of-current-subtree)))
            ;; Going inwards - double-space if first offspring is,
            ;; otherwise snug up.
            (end-of-line)		; So we skip any concealed progeny.
            (allout-pre-next-preface)
            (if (bolp)
                ;; Blank lines between current header body and next
                ;; header - get to last substantive (non-white-space)
                ;; line in body:
                (re-search-backward "[^ \t\n]" nil t))
            (if (save-excursion
                  (allout-next-heading)
                  (if (> (allout-recent-depth) ref-depth)
                      ;; This is an offspring.
                      (progn (forward-line -1)
                             (looking-at "^\\s-*$"))))
                (progn (forward-line 1)
                       (allout-unprotected
                        (allout-open-line-not-read-only))
                       (forward-line 1)))
            (end-of-line))
          ;;(if doing-beginning (goto-char doing-beginning))
          (if (not (bobp))
              ;; We insert a newline char rather than using open-line to
              ;; avoid rear-stickiness inheritence of read-only property.
              (progn (if (and (not (> depth ref-depth))
                              (not before))
                         (allout-unprotected
                          (allout-open-line-not-read-only))
		       (if (> depth ref-depth)
                         (allout-unprotected
                          (allout-open-line-not-read-only))
			 (if dbl-space
                             (allout-unprotected
                              (allout-open-line-not-read-only))
			   (if (not before)
			       (allout-unprotected (newline 1))))))
                     (if dbl-space
			 (allout-unprotected (newline 1)))
                     (if (and (not (eobp))
                              (not (bolp)))
                         (forward-char 1))))
          ))
    (insert (concat (allout-make-topic-prefix opening-numbered
                                              t
                                              depth)
                           " "))

    ;;(if doing-beginning (save-excursion (newline (if dbl-space 2 1))))


    (allout-rebullet-heading (and use_recent_bullet         ;;; solicit
                                   ref-bullet)
                              depth			     ;;; depth
                              nil 			     ;;; number-control
                              nil			     ;;; index
                              t)
    (end-of-line)
    )
  )
;;;_    . open-topic contingencies
;;;_     ; base topic - one from which open was issued
;;;_      , beginning char
;;;_      , amount of space before will be used, unless opening in place
;;;_      , end char will be used, unless opening before (and it still may)
;;;_     ; absolute depth of new topic
;;;_     ! insert in place - overrides most stuff
;;;_     ; relative depth of new re base
;;;_     ; before or after base topic
;;;_     ; spacing around topic, if any, prior to new topic and at same depth
;;;_     ; buffer boundaries - special provisions for beginning and end ob
;;;_     ; level 1 topics have special provisions also - double space.
;;;_     ; location of new topic
;;;_   > allout-open-line-not-read-only ()
(defun allout-open-line-not-read-only ()
  "Open line and remove inherited read-only text prop from new char, if any."
  (open-line 1)
  (if (plist-get (text-properties-at (point)) 'read-only)
      (allout-unprotected
       (remove-text-properties (point) (+ 1 (point)) '(read-only nil)))))
;;;_   > allout-open-subtopic (arg)
(defun allout-open-subtopic (arg)
  "Open new topic header at deeper level than the current one.

Negative universal arg means to open deeper, but place the new topic
prior to the current one."
  (interactive "p")
  (allout-open-topic 1 (> 0 arg) (< 1 arg)))
;;;_   > allout-open-sibtopic (arg)
(defun allout-open-sibtopic (arg)
  "Open new topic header at same level as the current one.

Positive universal arg means to use the bullet of the prior sibling.

Negative universal arg means to place the new topic prior to the current
one."
  (interactive "p")
  (allout-open-topic 0 (> 0 arg) (not (= 1 arg))))
;;;_   > allout-open-supertopic (arg)
(defun allout-open-supertopic (arg)
  "Open new topic header at shallower level than the current one.

Negative universal arg means to open shallower, but place the new
topic prior to the current one."

  (interactive "p")
  (allout-open-topic -1 (> 0 arg) (< 1 arg)))

;;;_  - Outline Alteration
;;;_   : Topic Modification
;;;_    = allout-former-auto-filler
(defvar allout-former-auto-filler nil
  "Name of modal fill function being wrapped by `allout-auto-fill'.")
;;;_    > allout-auto-fill ()
(defun allout-auto-fill ()
  "`allout-mode' autofill function.

Maintains outline hanging topic indentation if
`allout-use-hanging-indents' is set."
  (let ((fill-prefix (if allout-use-hanging-indents
                         ;; Check for topic header indentation:
                         (save-excursion
                           (beginning-of-line)
                           (if (looking-at allout-regexp)
                               ;; ... construct indentation to account for
                               ;; length of topic prefix:
                               (make-string (progn (allout-end-of-prefix)
                                                   (current-column))
                                            ?\ ))))))
    (if (or allout-former-auto-filler allout-use-hanging-indents)
        (do-auto-fill))))
;;;_    > allout-reindent-body (old-depth new-depth &optional number)
(defun allout-reindent-body (old-depth new-depth &optional number)
  "Reindent body lines which were indented at OLD-DEPTH to NEW-DEPTH.

Optional arg NUMBER indicates numbering is being added, and it must
be accommodated.

Note that refill of indented paragraphs is not done."

  (save-excursion
    (allout-end-of-prefix)
    (let* ((new-margin (current-column))
	   excess old-indent-begin old-indent-end
	   curr-ind
	   ;; We want the column where the header-prefix text started
	   ;; *before* the prefix was changed, so we infer it relative
	   ;; to the new margin and the shift in depth:
	   (old-margin (+ old-depth (- new-margin new-depth))))

      ;; Process lines up to (but excluding) next topic header:
      (allout-unprotected
       (save-match-data
         (while
	     (and (re-search-forward "[\n\r]\\(\\s-*\\)"
				     nil
				     t)
		  ;; Register the indent data, before we reset the
		  ;; match data with a subsequent `looking-at':
		  (setq old-indent-begin (match-beginning 1)
			old-indent-end (match-end 1))
		  (not (looking-at allout-regexp)))
	   (if (> 0 (setq excess (- (- old-indent-end old-indent-begin)
                                    old-margin)))
	       ;; Text starts left of old margin - don't adjust:
	       nil
	     ;; Text was hanging at or right of old left margin -
	     ;; reindent it, preserving its existing indentation
	     ;; beyond the old margin:
	     (delete-region old-indent-begin old-indent-end)
             (indent-to (+ new-margin excess (current-column))))))))))
;;;_    > allout-rebullet-current-heading (arg)
(defun allout-rebullet-current-heading (arg)
  "Solicit new bullet for current visible heading."
  (interactive "p")
  (let ((initial-col (current-column))
	(on-bullet (eq (point)(allout-current-bullet-pos)))
	(backwards (if (< arg 0)
		       (setq arg (* arg -1)))))
    (while (> arg 0)
      (save-excursion (allout-back-to-current-heading)
		      (allout-end-of-prefix)
		      (allout-rebullet-heading t	;;; solicit
						nil	;;; depth
						nil	;;; number-control
						nil	;;; index
						t))	;;; do-successors
      (setq arg (1- arg))
      (if (<= arg 0)
	  nil
	(setq initial-col nil)		; Override positioning back to init col
	(if (not backwards)
	    (allout-next-visible-heading 1)
	  (allout-goto-prefix)
	  (allout-next-visible-heading -1))))
    (message "Done.")
    (cond (on-bullet (goto-char (allout-current-bullet-pos)))
	  (initial-col (move-to-column initial-col)))))
;;;_    > allout-rebullet-heading (&optional solicit ...)
(defun allout-rebullet-heading (&optional solicit
                                           new-depth
                                           number-control
                                           index
                                           do-successors)

  "Adjust bullet of current topic prefix.

All args are optional.

If SOLICIT is non-nil, then the choice of bullet is solicited from
user.  If it's a character, then that character is offered as the
default, otherwise the one suited to the context \(according to
distinction or depth) is offered.  If non-nil, then the
context-specific bullet is just used.

Second arg DEPTH forces the topic prefix to that depth, regardless
of the topic's current depth.

Third arg NUMBER-CONTROL can force the prefix to or away from
numbered form.  It has effect only if `allout-numbered-bullet' is
non-nil and soliciting was not explicitly invoked (via first arg).
Its effect, numbering or denumbering, then depends on the setting
of the forth arg, INDEX.

If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
prefix of the topic is forced to be non-numbered.  Null index and
non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
INDEX is a number, then that number is used for the numbered
prefix.  Non-nil and non-number means that the index for the
numbered prefix will be derived by allout-make-topic-prefix.

Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
siblings.

Cf vars `allout-stylish-prefixes', `allout-old-style-prefixes',
and `allout-numbered-bullet', which all affect the behavior of
this function."

  (let* ((current-depth (allout-depth))
         (new-depth (or new-depth current-depth))
         (mb allout-recent-prefix-beginning)
         (me allout-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (allout-make-topic-prefix current-bullet
                                                nil
                                                new-depth
                                                solicit
                                                number-control
                                                index)))

    ;; Is new one is identical to old?
    (if (and (= current-depth new-depth)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
	;; Nothing to do:
        t

      ;; New prefix probably different from old:
					; get rid of old one:
      (allout-unprotected (delete-region mb me))
      (goto-char mb)
					; Dispense with number if
					; numbered-bullet prefix:
      (if (and allout-numbered-bullet
               (string= allout-numbered-bullet current-bullet)
               (looking-at "[0-9]+"))
	  (allout-unprotected
	   (delete-region (match-beginning 0)(match-end 0))))

					; Put in new prefix:
      (allout-unprotected (insert new-prefix))

      ;; Reindent the body if elected, margin changed, and not encrypted body:
      (if (and allout-reindent-bodies
	       (not (= new-depth current-depth))
               (not (allout-encrypted-topic-p)))
	  (allout-reindent-body current-depth new-depth))

      ;; Recursively rectify successive siblings of orig topic if
      ;; caller elected for it:
      (if do-successors
	  (save-excursion
	    (while (allout-next-sibling new-depth nil)
	      (setq index
		    (cond ((numberp index) (1+ index))
			  ((not number-control)  (allout-sibling-index))))
	      (if (allout-numbered-type-prefix)
		  (allout-rebullet-heading nil		;;; solicit
					    new-depth	;;; new-depth
					    number-control;;; number-control
					    index	;;; index
					    nil)))))	;;;(dont!)do-successors
      )	; (if (and (= current-depth new-depth)...))
    ) ; let* ((current-depth (allout-depth))...)
  ) ; defun
;;;_    > allout-rebullet-topic (arg)
(defun allout-rebullet-topic (arg)
  "Rebullet the visible topic containing point and all contained subtopics.

Descends into invisible as well as visible topics, however.

With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column))
        (was-eol (eolp)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (allout-back-to-current-heading)
      (if (<= (+ (allout-recent-depth) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (allout-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (max 0 (+ start-col arg)))))
;;;_     > allout-rebullet-topic-grunt (&optional relative-depth ...)
(defun allout-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors)
  "Like `allout-rebullet-topic', but on nearest containing topic
\(visible or not).

See `allout-rebullet-heading' for rebulleting behavior.

All arguments are optional.

First arg RELATIVE-DEPTH means to shift the depth of the entire
topic that amount.

The rest of the args are for internal recursive use by the function
itself.  The are STARTING-DEPTH, STARTING-POINT, and INDEX."

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (allout-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (allout-sibling-index))))
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1"))	;;; ====>

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one:
           (allout-rebullet-heading nil		;;; solicit
                                     (+ starting-depth	;;; starting-depth
                                        relative-depth)
                                     nil		;;; number
                                     index		;;; index
                                     ;; Every contained topic will get hit,
                                     ;; and we have to get to outside ones
                                     ;; deliberately:
                                     nil)		;;; do-successors
           ;; ... and work on subsequent ones which are at greater depth:
           (setq index 0)
           (allout-next-heading)
           (while (and (not (eobp))
                       (< starting-depth (allout-recent-depth)))
             (setq index (1+ index))
             (allout-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                           (1+ starting-depth);;;starting-depth
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-depth new-depth)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (allout-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                         new-depth	  ;;; starting-depth
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= (allout-recent-depth) starting-depth)
                           (= (allout-recent-depth) (+ starting-depth
                                                        relative-depth)))))
              (allout-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (allout-rebullet-heading nil nil nil nil t)))))
    )
  )
;;;_    > allout-renumber-to-depth (&optional depth)
(defun allout-renumber-to-depth (&optional depth)
  "Renumber siblings at current depth.

Affects superior topics if optional arg DEPTH is less than current depth.

Returns final depth."

  ;; Proceed by level, processing subsequent siblings on each,
  ;; ascending until we get shallower than the start depth:

  (let ((ascender (allout-depth))
	was-eobp)
    (while (and (not (eobp))
		(allout-depth)
                (>= (allout-recent-depth) depth)
                (>= ascender depth))
                                        ; Skip over all topics at
                                        ; lesser depths, which can not
                                        ; have been disturbed:
      (while (and (not (setq was-eobp (eobp)))
		  (> (allout-recent-depth) ascender))
        (allout-next-heading))
                                        ; Prime ascender for ascension:
      (setq ascender (1- (allout-recent-depth)))
      (if (>= (allout-recent-depth) depth)
          (allout-rebullet-heading nil	;;; solicit
                                    nil	;;; depth
                                    nil	;;; number-control
                                    nil	;;; index
                                    t)) ;;; do-successors
      (if was-eobp (goto-char (point-max)))))
  (allout-recent-depth))
;;;_    > allout-number-siblings (&optional denumber)
(defun allout-number-siblings (&optional denumber)
  "Assign numbered topic prefix to this topic and its siblings.

With universal argument, denumber - assign default bullet to this
topic and its siblings.

With repeated universal argument (`^U^U'), solicit bullet for each
rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (allout-back-to-current-heading)
    (allout-beginning-of-level)
    (let ((depth (allout-recent-depth))
	  (index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (allout-rebullet-heading use-bullet		;;; solicit
                                  depth			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (allout-next-sibling depth nil))))))
;;;_    > allout-shift-in (arg)
(defun allout-shift-in (arg)
  "Increase depth of current heading and any topics collapsed within it.

We disallow shifts that would result in the topic having a depth more than
one level greater than the immediately previous topic, to avoid containment
discontinuity.  The first topic in the file can be adjusted to any positive
depth, however."
  (interactive "p")
  (if (> arg 0)
      (save-excursion
        (allout-back-to-current-heading)
        (if (not (bobp))
            (let* ((current-depth (allout-recent-depth))
                   (start-point (point))
                   (predecessor-depth (progn
                                        (forward-char -1)
                                        (allout-goto-prefix)
                                        (if (< (point) start-point)
                                            (allout-recent-depth)
                                          0))))
              (if (and (> predecessor-depth 0)
                       (> (+ current-depth arg)
                          (1+ predecessor-depth)))
                  (error (concat "May not shift deeper than offspring depth"
                                 " of previous topic")))))))
  (allout-rebullet-topic arg))
;;;_    > allout-shift-out (arg)
(defun allout-shift-out (arg)
  "Decrease depth of current heading and any topics collapsed within it.

We disallow shifts that would result in the topic having a depth more than
one level greater than the immediately previous topic, to avoid containment
discontinuity.  The first topic in the file can be adjusted to any positive
depth, however."
  (interactive "p")
  (if (< arg 0)
      (allout-shift-in (* arg -1)))
  (allout-rebullet-topic (* arg -1)))
;;;_   : Surgery (kill-ring) functions with special provisions for outlines:
;;;_    > allout-kill-line (&optional arg)
(defun allout-kill-line (&optional arg)
  "Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")

  (let ((start-point (point))
        (leading-kill-ring-entry (car kill-ring))
        binding)

    (condition-case err

        (if (not (and (allout-mode-p)        ; active outline mode,
                      allout-numbered-bullet ; numbers may need adjustment,
                      (bolp)                  ; may be clipping topic head,
                      (looking-at allout-regexp))) ; are clipping topic head.
            ;; Above conditions do not obtain - just do a regular kill:
            (kill-line arg)
          ;; Ah, have to watch out for adjustments:
          (let* ((depth (allout-depth))
                 (start-point (point))
                 binding)
                                        ; Do the kill, presenting option
                                        ; for read-only text:
            (kill-line arg)
                                        ; Provide some feedback:
          (sit-for 0)
          (save-excursion
                                        ; Start with the topic
                                        ; following killed line:
            (if (not (looking-at allout-regexp))
                (allout-next-heading))
            (allout-renumber-to-depth depth))))
      ;; condition case handler:
      (text-read-only
       (goto-char start-point)
       (setq binding (where-is-internal 'allout-kill-topic nil t))
       (cond ((not binding) (setq binding ""))
             ((arrayp binding)
              (setq binding (mapconcat 'key-description (list binding) ", ")))
             (t (setq binding (format "%s" binding))))
       ;; ensure prior kill-ring leader is properly restored:
       (if (eq leading-kill-ring-entry (cadr kill-ring))
           ;; Aborted kill got pushed on front - ditch it:
           (pop kill-ring)
         ;; Aborted kill got appended to prior - resurrect prior:
         (setcar kill-ring leading-kill-ring-entry))
       ;; make last-command skip this failed command, so kill-appending
       ;; conditions track:
       (setq this-command last-command)
       (error (concat "read-only text hit - use %s allout-kill-topic to"
                      " discard collapsed stuff")
              binding)))
    )
  )
;;;_    > allout-kill-topic ()
(defun allout-kill-topic ()
  "Kill topic together with subtopics.

Leaves primary topic's trailing vertical whitespace, if any."

  ;; Some finagling is done to make complex topic kills appear faster
  ;; than they actually are.  A redisplay is performed immediately
  ;; after the region is disposed of, though the renumbering process
  ;; has yet to be performed.  This means that there may appear to be
  ;; a lag *after* the kill has been performed.

  (interactive)
  (let* ((beg (prog1 (allout-back-to-current-heading)(beginning-of-line)))
         (depth (allout-recent-depth)))
    (allout-end-of-current-subtree)
    (if (not (eobp))
	(if (or (not (looking-at "^$"))
		;; A blank line - cut it with this topic *unless* this
		;; is the last topic at this level, in which case
		;; we'll leave the blank line as part of the
		;; containing topic:
		(save-excursion
		  (and (allout-next-heading)
		       (>= (allout-recent-depth) depth))))
	    (forward-char 1)))

    (allout-unprotected (kill-region beg (point)))
    (sit-for 0)
    (save-excursion
      (allout-renumber-to-depth depth))))
;;;_    > allout-yank-processing ()
(defun allout-yank-processing (&optional arg)

  "Incidental outline-specific business to be done just after text yanks.

Does depth adjustment of yanked topics, when:

1 the stuff being yanked starts with a valid outline header prefix, and
2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

Also, adjusts numbering of subsequent siblings when appropriate.

Depth adjustment alters the depth of all the topics being yanked
the amount it takes to make the first topic have the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, are left exactly like normal, non-allout-specific yanks."

  (interactive "*P")
					; Get to beginning, leaving
					; region around subject:
  (if (< (my-mark-marker t) (point))
      (exchange-point-and-mark))
  (let* ((subj-beg (point))
	 (subj-end (my-mark-marker t))
	 ;; 'resituate' if yanking an entire topic into topic header:
	 (resituate (and (allout-e-o-prefix-p)
			 (looking-at (concat "\\(" allout-regexp "\\)"))
			 (allout-prefix-data (match-beginning 1)
					      (match-end 1))))
	 ;; `rectify-numbering' if resituating (where several topics may
	 ;; be resituating) or yanking a topic into a topic slot (bol):
	 (rectify-numbering (or resituate
				(and (bolp) (looking-at allout-regexp)))))
    (if resituate
                                        ; The yanked stuff is a topic:
	(let* ((prefix-len (- (match-end 1) subj-beg))
	       (subj-depth (allout-recent-depth))
	       (prefix-bullet (allout-recent-bullet))
	       (adjust-to-depth
		;; Nil if adjustment unnecessary, otherwise depth to which
		;; adjustment should be made:
		(save-excursion
		  (and (goto-char subj-end)
		       (eolp)
		       (goto-char subj-beg)
		       (and (looking-at allout-regexp)
			    (progn
			      (beginning-of-line)
			      (not (= (point) subj-beg)))
			    (looking-at allout-regexp)
			    (allout-prefix-data (match-beginning 0)
						 (match-end 0)))
		       (allout-recent-depth))))
	       done
	       (more t))
	  (setq rectify-numbering allout-numbered-bullet)
	  (if adjust-to-depth
                                        ; Do the adjustment:
	      (progn
		(message "... yanking") (sit-for 0)
		(save-restriction
		  (narrow-to-region subj-beg subj-end)
                                        ; Trim off excessive blank
                                        ; line at end, if any:
		  (goto-char (point-max))
		  (if (looking-at "^$")
		      (allout-unprotected (delete-char -1)))
                                        ; Work backwards, with each
                                        ; shallowest level,
                                        ; successively excluding the
                                        ; last processed topic from
                                        ; the narrow region:
		  (while more
		    (allout-back-to-current-heading)
                                        ; go as high as we can in each bunch:
		    (while (allout-ascend-to-depth (1- (allout-depth))))
		    (save-excursion
		      (allout-rebullet-topic-grunt (- adjust-to-depth
						       subj-depth))
		      (allout-depth))
		    (if (setq more (not (bobp)))
			(progn (widen)
			       (forward-char -1)
			       (narrow-to-region subj-beg (point))))))
		(message "")
		;; Preserve new bullet if it's a distinctive one, otherwise
		;; use old one:
		(if (string-match (regexp-quote prefix-bullet)
				  allout-distinctive-bullets-string)
                                        ; Delete from bullet of old to
                                        ; before bullet of new:
		    (progn
		      (beginning-of-line)
		      (delete-region (point) subj-beg)
		      (set-marker (my-mark-marker t) subj-end)
		      (goto-char subj-beg)
		      (allout-end-of-prefix))
                                        ; Delete base subj prefix,
                                        ; leaving old one:
		  (delete-region (point) (+ (point)
					    prefix-len
					    (- adjust-to-depth subj-depth)))
                                        ; and delete residual subj
                                        ; prefix digits and space:
		  (while (looking-at "[0-9]") (delete-char 1))
		  (if (looking-at " ") (delete-char 1))))
	    (exchange-point-and-mark))))
    (if rectify-numbering
	(progn
	  (save-excursion
                                        ; Give some preliminary feedback:
	    (message "... reconciling numbers") (sit-for 0)
                                        ; ... and renumber, in case necessary:
	    (goto-char subj-beg)
	    (if (allout-goto-prefix)
		(allout-rebullet-heading nil	;;; solicit
					  (allout-depth) ;;; depth
					  nil	;;; number-control
					  nil	;;; index
					  t))
	    (message ""))))
    (if (not resituate)
      (exchange-point-and-mark))))
;;;_    > allout-yank (&optional arg)
(defun allout-yank (&optional arg)
  "`allout-mode' yank, with depth and numbering adjustment of yanked topics.

Non-topic yanks work no differently than normal yanks.

If a topic is being yanked into a bare topic prefix, the depth of the
yanked topic is adjusted to the depth of the topic prefix.

  1 we're yanking in an `allout-mode' buffer
  2 the stuff being yanked starts with a valid outline header prefix, and
  3 it is being yanked at the end of a line which consists of only a valid
    topic prefix.

If these conditions hold then the depth of the yanked topics are all
adjusted the amount it takes to make the first one at the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, (ones that don't qualify for adjustment) are handled
exactly like normal yanks.

Numbering of yanked topics, and the successive siblings at the depth
into which they're being yanked, is adjusted.

`allout-yank-pop' works with `allout-yank' just like normal `yank-pop'
works with normal `yank' in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (yank arg)
  (if (allout-mode-p)
      (allout-yank-processing)))
;;;_    > allout-yank-pop (&optional arg)
(defun allout-yank-pop (&optional arg)
  "Yank-pop like `allout-yank' when popping to bare outline prefixes.

Adapts level of popped topics to level of fresh prefix.

Note - prefix changes to distinctive bullets will stick, if followed
by pops to non-distinctive yanks.  Bug..."

  (interactive "*p")
  (setq this-command 'yank)
  (yank-pop arg)
  (if (allout-mode-p)
      (allout-yank-processing)))

;;;_  - Specialty bullet functions
;;;_   : File Cross references
;;;_    > allout-resolve-xref ()
(defun allout-resolve-xref ()
  "Pop to file associated with current heading, if it has an xref bullet.

\(Works according to setting of `allout-file-xref-bullet')."
  (interactive)
  (if (not allout-file-xref-bullet)
      (error
       "Outline cross references disabled - no `allout-file-xref-bullet'")
    (if (not (string= (allout-current-bullet) allout-file-xref-bullet))
        (error "Current heading lacks cross-reference bullet `%s'"
               allout-file-xref-bullet)
      (let (file-name)
        (save-excursion
          (let* ((text-start allout-recent-prefix-end)
                 (heading-end (progn (end-of-line) (point))))
            (goto-char text-start)
            (setq file-name
                  (if (re-search-forward "\\s-\\(\\S-*\\)" heading-end t)
                      (buffer-substring (match-beginning 1) (match-end 1))))))
        (setq file-name
              (if (not (= (aref file-name 0) ?:))
                  (expand-file-name file-name)
                                        ; A registry-files ref, strip the `:'
                                        ; and try to follow it:
                (let ((reg-ref (reference-registered-file
                                (substring file-name 1) nil t)))
                  (if reg-ref (car (cdr reg-ref))))))
        (if (or (file-exists-p file-name)
                (if (file-writable-p file-name)
                    (y-or-n-p (format "%s not there, create one? "
                                      file-name))
                  (error "%s not found and can't be created" file-name)))
            (condition-case failure
                (find-file-other-window file-name)
              ('error failure))
          (error "%s not found" file-name))
        )
      )
    )
  )

;;;_ #6 Exposure Control

;;;_  - Fundamental
;;;_   > allout-flag-region (from to flag)
(defun allout-flag-region (from to flag)
  "Hide or show lines from FROM to TO, via Emacs selective-display FLAG char.
Ie, text following flag C-m \(carriage-return) is hidden until the
next C-j (newline) char.

Returns the endpoint of the region."
  ;; "OFR-" prefixes to avoid collisions with vars in code calling the macro.
  ;; ie, elisp macro vars are not 'hygenic', so distinct names are necessary.
  (let ((was-inhibit-r-o inhibit-read-only)
        (was-undo-list buffer-undo-list)
        (was-modified (buffer-modified-p))
        trans)
    (unwind-protect
     (save-excursion
       (setq inhibit-read-only t)
       (setq buffer-undo-list t)
       (if (> from to)
           (setq trans from from to to trans))
       (subst-char-in-region from to
                             (if (= flag ?\n) ?\r ?\n)
                             flag t)
       ;; adjust character read-protection on all the affected lines.
       ;; we handle the region line-by-line.
       (goto-char to)
       (end-of-line)
       (setq to (min (+ 2 (point)) (point-max)))
       (goto-char from)
       (beginning-of-line)
       (while (< (point) to)
         ;; handle from start of exposed to beginning of hidden, or eol:
         (remove-text-properties (point)
                                 (progn (if (re-search-forward "[\r\n]"
                                                               nil t)
                                            (forward-char -1))
                                        (point))
                                 '(read-only nil))
         ;; handle from start of hidden, if any, to eol:
         (if (and (not (eobp)) (= (char-after (point)) ?\r))
             (put-text-property (point) (progn (end-of-line) (point))
                                'read-only t))
         ;; Handle the end-of-line to beginning of next line:
         (if (not (eobp))
             (progn (forward-char 1)
                    (remove-text-properties (1- (point)) (point)
                                            '(read-only nil)))))
       )
     (if (not was-modified)
         (set-buffer-modified-p nil))
     (setq inhibit-read-only was-inhibit-r-o)
     (setq buffer-undo-list was-undo-list)
     )
    )
  )
;;;_   > allout-flag-current-subtree (flag)
(defun allout-flag-current-subtree (flag)
  "Hide or show subtree of currently-visible topic.

See `allout-flag-region' for more details."

  (save-excursion
    (allout-back-to-current-heading)
    (let ((from (point))
          (to (progn (allout-end-of-current-subtree) (1- (point)))))
      (allout-flag-region from to flag))))

;;;_  - Topic-specific
;;;_   > allout-show-entry ()
(defun allout-show-entry ()
  "Like `allout-show-current-entry', reveals entries nested in hidden topics.

This is a way to give restricted peek at a concealed locality without the
expense of exposing its context, but can leave the outline with aberrant
exposure.  `allout-hide-current-entry-completely' or `allout-show-offshoot'
should be used after the peek to rectify the exposure."

  (interactive)
  (save-excursion
    (let ((at (point))
	  beg end)
      (allout-goto-prefix)
      (setq beg (if (= (preceding-char) ?\r) (1- (point)) (point)))
      (re-search-forward "[\n\r]" nil t)
      (setq end (1- (if (< at (point))
			;; We're on topic head line - show only it:
			(point)
		      ;; or we're in body - include it:
		      (max beg (or (allout-pre-next-preface) (point))))))
      (allout-flag-region beg end ?\n)
      (list beg end))))
;;;_   > allout-show-children (&optional level strict)
(defun allout-show-children (&optional level strict)

  "If point is visible, show all direct subheadings of this heading.

Otherwise, do `allout-show-to-offshoot', and then show subheadings.

Optional LEVEL specifies how many levels below the current level
should be shown, or all levels if t.  Default is 1.

Optional STRICT means don't resort to -show-to-offshoot, no matter
what.  This is basically so -show-to-offshoot, which is called by
this function, can employ the pure offspring-revealing capabilities of
it.

Returns point at end of subtree that was opened, if any.  (May get a
point of non-opened subtree?)"

  (interactive "p")
  (let (max-pos)
    (if (and (not strict)
	     (allout-hidden-p))

	(progn (allout-show-to-offshoot) ; Point's concealed, open to
					  ; expose it.
	       ;; Then recurse, but with "strict" set so we don't
	       ;; infinite regress:
	       (setq max-pos (allout-show-children level t)))

      (save-excursion
	(save-restriction
	  (let* ((start-pt (point))
		 (chart (allout-chart-subtree (or level 1)))
		 (to-reveal (allout-chart-to-reveal chart (or level 1))))
	    (goto-char start-pt)
	    (if (and strict (= (preceding-char) ?\r))
		;; Concealed root would already have been taken care of,
		;; unless strict was set.
		(progn
		  (allout-flag-region (point) (allout-snug-back) ?\n)
		  (if allout-show-bodies
		      (progn (goto-char (car to-reveal))
			     (allout-show-current-entry)))))
	    (while to-reveal
	      (goto-char (car to-reveal))
	      (allout-flag-region (point) (allout-snug-back) ?\n)
	      (if allout-show-bodies
		  (progn (goto-char (car to-reveal))
			 (allout-show-current-entry)))
	      (setq to-reveal (cdr to-reveal)))))))))
;;;_   > allout-hide-point-reconcile ()
(defun allout-hide-reconcile ()
  "Like `allout-hide-current-entry'; hides completely if within hidden region.

Specifically intended for aberrant exposure states, like entries that were
exposed by `allout-show-entry' but are within otherwise concealed regions."
  (interactive)
  (save-excursion
    (allout-goto-prefix)
    (allout-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (allout-pre-next-preface)
                                (if (= ?\r (following-char))
                                    (point)
                                  (1- (point))))
                         ?\r)))
;;;_   > allout-show-to-offshoot ()
(defun allout-show-to-offshoot ()
  "Like `allout-show-entry', but reveals all concealed ancestors, as well.

As with `allout-hide-current-entry-completely', useful for rectifying
aberrant exposure states produced by `allout-show-entry'."

  (interactive)
  (save-excursion
    (let ((orig-pt (point))
	  (orig-pref (allout-goto-prefix))
	  (last-at (point))
	  bag-it)
      (while (or bag-it (= (preceding-char) ?\r))
	(beginning-of-line)
	(if (= last-at (setq last-at (point)))
	    ;; Oops, we're not making any progress!  Show the current
	    ;; topic completely, and bag this try.
	    (progn (beginning-of-line)
		   (allout-show-current-subtree)
		   (goto-char orig-pt)
		   (setq bag-it t)
		   (beep)
		   (message "%s: %s"
			    "allout-show-to-offshoot: "
			    "Aberrant nesting encountered.")))
	(allout-show-children)
	(goto-char orig-pref))
      (goto-char orig-pt)))
  (if (allout-hidden-p)
      (allout-show-entry)))
;;;_   > allout-hide-current-entry ()
(defun allout-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (allout-back-to-current-heading)
  (save-excursion
   (allout-flag-region (point)
                        (progn (allout-end-of-entry) (point))
                        ?\r)))
;;;_   > allout-show-current-entry (&optional arg)
(defun allout-show-current-entry (&optional arg)

  "Show body following current heading, or hide the entry if repeat count."

  (interactive "P")
  (if arg
      (allout-hide-current-entry)
    (save-excursion
      (allout-flag-region (point)
			   (progn (allout-end-of-entry) (point))
			   ?\n)
      )))
;;;_   > allout-hide-current-entry-completely ()
; ... allout-hide-current-entry-completely also for isearch dynamic exposure:
(defun allout-hide-current-entry-completely ()
  "Like `allout-hide-current-entry', but conceal topic completely.

Specifically intended for aberrant exposure states, like entries that were
exposed by `allout-show-entry' but are within otherwise concealed regions."
  (interactive)
  (save-excursion
    (allout-goto-prefix)
    (allout-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (allout-pre-next-preface)
                                (if (= ?\r (following-char))
                                    (point)
                                  (1- (point))))
                         ?\r)))
;;;_   > allout-show-current-subtree (&optional arg)
(defun allout-show-current-subtree (&optional arg)
  "Show everything within the current topic.  With a repeat-count,
expose this topic and its siblings."
  (interactive "P")
  (save-excursion
    (if (<= (allout-current-depth) 0)
	;; Outside any topics - try to get to the first:
	(if (not (allout-next-heading))
	    (error "No topics")
	  ;; got to first, outermost topic - set to expose it and siblings:
	  (message "Above outermost topic - exposing all.")
	  (allout-flag-region (point-min)(point-max) ?\n))
      (if (not arg)
	  (allout-flag-current-subtree ?\n)
	(allout-beginning-of-level)
	(allout-expose-topic '(* :))))))
;;;_   > allout-hide-current-subtree (&optional just-close)
(defun allout-hide-current-subtree (&optional just-close)
  "Close the current topic, or containing topic if this one is already closed.

If this topic is closed and it's a top level topic, close this topic
and its siblings.

If optional arg JUST-CLOSE is non-nil, do not treat the parent or
siblings, even if the target topic is already closed."

  (interactive)
  (let ((from (point))
	(orig-eol (progn (end-of-line)
			 (if (not (allout-goto-prefix))
			     (error "No topics found")
			   (end-of-line)(point)))))
    (allout-flag-current-subtree ?\r)
    (goto-char from)
    (if (and (= orig-eol (progn (goto-char orig-eol)
				(end-of-line)
				(point)))
	     (not just-close)
             ;; Structure didn't change - try hiding current level:
	     (goto-char from)
	     (if (allout-up-current-level 1 t)
		 t
	       (goto-char 0)
	       (let ((msg
		      "Top-level topic already closed - closing siblings..."))
		 (message msg)
		 (allout-expose-topic '(0 :))
		 (message (concat msg "  Done.")))
	       nil)
	     (/= (allout-recent-depth) 0))
	(allout-hide-current-subtree))
      (goto-char from)))
;;;_   > allout-show-current-branches ()
(defun allout-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (beginning-of-line)
  (allout-show-children t))
;;;_   > allout-hide-current-leaves ()
(defun allout-hide-current-leaves ()
  "Hide the bodies of the current topic and all its offspring."
  (interactive)
  (allout-back-to-current-heading)
  (allout-hide-region-body (point) (progn (allout-end-of-current-subtree)
                                           (point))))

;;;_  - Region and beyond
;;;_   > allout-show-all ()
(defun allout-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (message "Exposing entire buffer...")
  (allout-flag-region (point-min) (point-max) ?\n)
  (message "Exposing entire buffer...  Done."))
;;;_   > allout-hide-bodies ()
(defun allout-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (allout-hide-region-body (point-min) (point-max)))
;;;_   > allout-hide-region-body (start end)
(defun allout-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(allout-flag-region (point)
                             (progn (allout-pre-next-preface) (point)) ?\r)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\r][\n\r]")
		 2 1)))))))

;;;_   > allout-expose-topic (spec)
(defun allout-expose-topic (spec)
  "Apply exposure specs to successive outline topic items.

Use the more convenient frontend, `allout-new-exposure', if you don't
need evaluation of the arguments, or even better, the `allout-layout'
variable-keyed mode-activation/auto-exposure feature of allout outline
mode.  See the respective documentation strings for more details.

Cursor is left at start position.

SPEC is either a number or a list.

Successive specs on a list are applied to successive sibling topics.

A simple spec \(either a number, one of a few symbols, or the null
list) dictates the exposure for the corresponding topic.

Non-null lists recursively designate exposure specs for respective
subtopics of the current topic.

The `:' repeat spec is used to specify exposure for any number of
successive siblings, up to the trailing ones for which there are
explicit specs following the `:'.

Simple (numeric and null-list) specs are interpreted as follows:

 Numbers indicate the relative depth to open the corresponding topic.
     - negative numbers force the topic to be closed before opening to the
       absolute value of the number, so all siblings are open only to
       that level.
     - positive numbers open to the relative depth indicated by the
       number, but do not force already opened subtopics to be closed.
     - 0 means to close topic - hide all offspring.
  :  - `repeat'
       apply prior element to all siblings at current level, *up to*
       those siblings that would be covered by specs following the `:'
       on the list.  Ie, apply to all topics at level but the last
       ones.  \(Only first of multiple colons at same level is
       respected - subsequent ones are discarded.)
  *  - completely opens the topic, including bodies.
  +  - shows all the sub headers, but not the bodies
  -  - exposes the body of the corresponding topic.

Examples:
\(allout-expose-topic '(-1 : 0))
	Close this and all following topics at current level, exposing
	only their immediate children, but close down the last topic
	at this current level completely.
\(allout-expose-topic '(-1 () : 1 0))
	Close current topic so only the immediate subtopics are shown;
	show the children in the second to last topic, and completely
	close the last one.
\(allout-expose-topic '(-2 : -1 *))
        Expose children and grandchildren of all topics at current
	level except the last two; expose children of the second to
	last and completely open the last one."

  (interactive "xExposure spec: ")
  (if (not (listp spec))
      nil
    (let ((depth (allout-depth))
	  (max-pos 0)
	  prev-elem curr-elem
	  stay done
	  snug-back
	  )
      (while spec
	(setq prev-elem curr-elem
	      curr-elem (car spec)
	      spec (cdr spec))
	(cond				; Do current element:
	 ((null curr-elem) nil)
	 ((symbolp curr-elem)
	  (cond ((eq curr-elem '*) (allout-show-current-subtree)
		 (if (> allout-recent-end-of-subtree max-pos)
		     (setq max-pos allout-recent-end-of-subtree)))
		((eq curr-elem '+) (allout-show-current-branches)
		 (if (> allout-recent-end-of-subtree max-pos)
		     (setq max-pos allout-recent-end-of-subtree)))
		((eq curr-elem '-) (allout-show-current-entry))
		((eq curr-elem ':)
		 (setq stay t)
		 ;; Expand the `repeat' spec to an explicit version,
		 ;; w.r.t. remaining siblings:
		 (let ((residue	   ; = # of sibs not covered by remaining spec
			;; Dang - could be nice to make use of the chart, sigh:
			(- (length (allout-chart-siblings))
			   (length spec))))
		   (if (< 0 residue)
		       ;; Some residue - cover it with prev-elem:
		       (setq spec (append (make-list residue prev-elem)
					  spec)))))))
	 ((numberp curr-elem)
	  (if (and (>= 0 curr-elem) (allout-visible-p))
	      (save-excursion (allout-hide-current-subtree t)
			      (if (> 0 curr-elem)
				  nil
				(if (> allout-recent-end-of-subtree max-pos)
				    (setq max-pos
					  allout-recent-end-of-subtree)))))
	  (if (> (abs curr-elem) 0)
	      (progn (allout-show-children (abs curr-elem))
		     (if (> allout-recent-end-of-subtree max-pos)
			 (setq max-pos allout-recent-end-of-subtree)))))
	  ((listp curr-elem)
	   (if (allout-descend-to-depth (1+ depth))
	       (let ((got (allout-expose-topic curr-elem)))
		 (if (and got (> got max-pos)) (setq max-pos got))))))
	(cond (stay (setq stay nil))
	      ((listp (car spec)) nil)
	      ((> max-pos (point))
	       ;; Capitalize on max-pos state to get us nearer next sibling:
	       (progn (goto-char (min (point-max) max-pos))
		      (allout-next-heading)))
	      ((allout-next-sibling depth))))
      max-pos)))
;;;_   > allout-old-expose-topic (spec &rest followers)
(defun allout-old-expose-topic (spec &rest followers)

  "Deprecated.  Use `allout-expose-topic' \(with different schema
format) instead.

Dictate wholesale exposure scheme for current topic, according to SPEC.

SPEC is either a number or a list.  Optional successive args
dictate exposure for subsequent siblings of current topic.

A simple spec (either a number, a special symbol, or the null list)
dictates the overall exposure for a topic.  Non null lists are
composite specs whose first element dictates the overall exposure for
a topic, with the subsequent elements in the list interpreted as specs
that dictate the exposure for the successive offspring of the topic.

Simple (numeric and null-list) specs are interpreted as follows:

 - Numbers indicate the relative depth to open the corresponding topic:
  - negative numbers force the topic to be close before opening to the
    absolute value of the number.
  - positive numbers just open to the relative depth indicated by the number.
  - 0 just closes
 - `*' completely opens the topic, including bodies.
 - `+' shows all the sub headers, but not the bodies
 - `-' exposes the body and immediate offspring of the corresponding topic.

If the spec is a list, the first element must be a number, which
dictates the exposure depth of the topic as a whole.  Subsequent
elements of the list are nested SPECs, dictating the specific exposure
for the corresponding offspring of the topic.

Optional FOLLOWERS arguments dictate exposure for succeeding siblings."

  (interactive "xExposure spec: ")
  (let ((depth (allout-current-depth))
	done
	max-pos)
    (cond ((null spec) nil)
	  ((symbolp spec)
	   (if (eq spec '*) (allout-show-current-subtree))
	   (if (eq spec '+) (allout-show-current-branches))
	   (if (eq spec '-) (allout-show-current-entry)))
	  ((numberp spec)
	   (if (>= 0 spec)
	       (save-excursion (allout-hide-current-subtree t)
			       (end-of-line)
			       (if (or (not max-pos)
				       (> (point) max-pos))
				   (setq max-pos (point)))
			       (if (> 0 spec)
				   (setq spec (* -1 spec)))))
	   (if (> spec 0)
	     (allout-show-children spec)))
	  ((listp spec)
	   ;(let ((got (allout-old-expose-topic (car spec))))
	   ;  (if (and got (or (not max-pos) (> got max-pos)))
	   ;	 (setq max-pos got)))
	   (let ((new-depth  (+ (allout-current-depth) 1))
		 got)
	     (setq max-pos (allout-old-expose-topic (car spec)))
	     (setq spec (cdr spec))
	     (if (and spec
		      (allout-descend-to-depth new-depth)
		      (not (allout-hidden-p)))
		 (progn (setq got (apply 'allout-old-expose-topic spec))
			(if (and got (or (not max-pos) (> got max-pos)))
			    (setq max-pos got)))))))
    (while (and followers
		(progn (if (and max-pos (< (point) max-pos))
			   (progn (goto-char max-pos)
				  (setq max-pos nil)))
		       (end-of-line)
		       (allout-next-sibling depth)))
      (allout-old-expose-topic (car followers))
      (setq followers (cdr followers)))
    max-pos))
;;;_   > allout-new-exposure '()
(defmacro allout-new-exposure (&rest spec)
  "Literal frontend for `allout-expose-topic', doesn't evaluate arguments.
Some arguments that would need to be quoted in `allout-expose-topic'
need not be quoted in `allout-new-exposure'.

Cursor is left at start position.

Use this instead of obsolete `allout-exposure'.

Examples:
\(allout-new-exposure (-1 () () () 1) 0)
	Close current topic at current level so only the immediate
	subtopics are shown, except also show the children of the
	third subtopic; and close the next topic at the current level.
\(allout-new-exposure : -1 0)
	Close all topics at current level to expose only their
	immediate children, except for the last topic at the current
	level, in which even its immediate children are hidden.
\(allout-new-exposure -2 : -1 *)
        Expose children and grandchildren of first topic at current
	level, and expose children of subsequent topics at current
	level *except* for the last, which should be opened completely."
  (list 'save-excursion
	'(if (not (or (allout-goto-prefix)
		      (allout-next-heading)))
	     (error "allout-new-exposure: Can't find any outline topics"))
	(list 'allout-expose-topic (list 'quote spec))))

;;;_ #7 Systematic outline presentation - copying, printing, flattening

;;;_  - Mapping and processing of topics
;;;_   ( See also Subtree Charting, in Navigation code.)
;;;_   > allout-stringify-flat-index (flat-index)
(defun allout-stringify-flat-index (flat-index &optional context)
  "Convert list representing section/subsection/... to document string.

Optional arg CONTEXT indicates interior levels to include."
  (let ((delim ".")
	result
	numstr
	(context-depth (or (and context 2) 1)))
    ;; Take care of the explicit context:
    (while (> context-depth 0)
      (setq numstr (int-to-string (car flat-index))
	    flat-index (cdr flat-index)
	    result (if flat-index
		       (cons delim (cons numstr result))
		       (cons numstr result))
	    context-depth (if flat-index (1- context-depth) 0)))
    (setq delim " ")
    ;; Take care of the indentation:
    (if flat-index
	(progn
	  (while flat-index
	    (setq result
		  (cons delim
			(cons (make-string
			       (1+ (truncate (if (zerop (car flat-index))
						 1
					       (log10 (car flat-index)))))
			       ? )
			      result)))
	    (setq flat-index (cdr flat-index)))
	  ;; Dispose of single extra delim:
	  (setq result (cdr result))))
    (apply 'concat result)))
;;;_   > allout-stringify-flat-index-plain (flat-index)
(defun allout-stringify-flat-index-plain (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result)
	(while flat-index
	  (setq result (cons (int-to-string (car flat-index))
			     (if result
				 (cons delim result))))
	  (setq flat-index (cdr flat-index)))
    (apply 'concat result)))
;;;_   > allout-stringify-flat-index-indented (flat-index)
(defun allout-stringify-flat-index-indented (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result
	numstr)
    ;; Take care of the explicit context:
    (setq numstr (int-to-string (car flat-index))
	  flat-index (cdr flat-index)
	  result (if flat-index
		     (cons delim (cons numstr result))
		   (cons numstr result)))
    (setq delim " ")
    ;; Take care of the indentation:
    (if flat-index
	(progn
	  (while flat-index
	    (setq result
		  (cons delim
			(cons (make-string
			       (1+ (truncate (if (zerop (car flat-index))
						 1
					       (log10 (car flat-index)))))
			       ? )
			      result)))
	    (setq flat-index (cdr flat-index)))
	  ;; Dispose of single extra delim:
	  (setq result (cdr result))))
    (apply 'concat result)))
;;;_   > allout-listify-exposed (&optional start end format)
(defun allout-listify-exposed (&optional start end format)

  "Produce a list representing exposed topics in current region.

This list can then be used by `allout-process-exposed' to manipulate
the subject region.

Optional START and END indicate bounds of region.

optional arg, FORMAT, designates an alternate presentation form for
the prefix:

 list - Present prefix as numeric section.subsection..., starting with
	section indicated by the list, innermost nesting first.
 `indent' \(symbol) -  Convert header prefixes to all white space,
		       except for distinctive bullets.

The elements of the list produced are lists that represents a topic
header and body.  The elements of that list are:

 - a number representing the depth of the topic,
 - a string representing the header-prefix, including trailing whitespace and
   bullet.
 - a string representing the bullet character,
 - and a series of strings, each containing one line of the exposed
   portion of the topic entry."

  (interactive "r")
  (save-excursion
    (let*
	;; state vars:
	(strings prefix pad result depth new-depth out gone-out bullet beg
		 next done)

      (goto-char start)
      (beginning-of-line)
      ;; Goto initial topic, and register preceeding stuff, if any:
      (if (> (allout-goto-prefix) start)
	  ;; First topic follows beginning point - register preliminary stuff:
	  (setq result (list (list 0 "" nil
				   (buffer-substring start (1- (point)))))))
      (while (and (not done)
		  (not (eobp))		; Loop until we've covered the region.
		  (not (> (point) end)))
	(setq depth (allout-recent-depth) 	; Current topics depth,
	      bullet (allout-recent-bullet)	; ... bullet,
	      prefix (allout-recent-prefix)
	      beg (progn (allout-end-of-prefix t) (point))) ; and beginning.
	(setq done			; The boundary for the current topic:
	      (not (allout-next-visible-heading 1)))
	(setq new-depth (allout-recent-depth))
	(setq gone-out out
	      out (< new-depth depth))
	(beginning-of-line)
	(setq next (point))
	(goto-char beg)
	(setq strings nil)
	(while (> next (point))		; Get all the exposed text in
	  (setq strings
		(cons (buffer-substring
		       beg
					;To hidden text or end of line:
		       (progn
			 (search-forward "\r"
					 (save-excursion (end-of-line)
							 (point))
					 1)
			 (if (= (preceding-char) ?\r)
			     (1- (point))
			   (point))))
		      strings))
	  (if (< (point) next)		; Resume from after hid text, if any.
	      (forward-line 1))
	  (setq beg (point)))
	;; Accumulate list for this topic:
	(setq strings (nreverse strings))
	(setq result
	      (cons
	       (if format
		   (let ((special (if (string-match
				       (regexp-quote bullet)
				       allout-distinctive-bullets-string)
				      bullet)))
		     (cond ((listp format)
			    (list depth
				  (if allout-abbreviate-flattened-numbering
				      (allout-stringify-flat-index format
								    gone-out)
				      (allout-stringify-flat-index-plain
				       format))
				  strings
				  special))
			   ((eq format 'indent)
			    (if special
				(list depth
				      (concat (make-string (1+ depth) ? )
					      (substring prefix -1))
				      strings)
			      (list depth
				    (make-string depth ? )
				    strings)))
			   (t (error "allout-listify-exposed: %s %s"
				     "invalid format" format))))
		 (list depth prefix strings))
		    result))
	;; Reasses format, if any:
	(if (and format (listp format))
	    (cond ((= new-depth depth)
		   (setq format (cons (1+ (car format))
					  (cdr format))))
		  ((> new-depth depth)	; descending - assume by 1:
		   (setq format (cons 1 format)))
		  (t
					; Pop the residue:
		   (while (< new-depth depth)
		       (setq format (cdr format))
		       (setq depth (1- depth)))
					; And increment the current one:
		     (setq format
			   (cons (1+ (or (car format)
					 -1))
				 (cdr format)))))))
      ;; Put the list with first at front, to last at back:
      (nreverse result))))
;;;_   > my-region-active-p ()
(defmacro my-region-active-p ()
  (if (fboundp 'region-active-p)
      '(region-active-p)
    'mark-active))
;;;_   > allout-process-exposed (&optional func from to frombuf
;;;					    tobuf format)
(defun allout-process-exposed (&optional func from to frombuf tobuf
					  format &optional start-num)
  "Map function on exposed parts of current topic; results to another buffer.

All args are options; default values itemized below.

Apply FUNCTION to exposed portions FROM position TO position in buffer
FROMBUF to buffer TOBUF.  Sixth optional arg, FORMAT, designates an
alternate presentation form:

 `flat' - Present prefix as numeric section.subsection..., starting with
	 section indicated by the start-num, innermost nesting first.
 X`flat-indented' - Prefix is like `flat' for first topic at each
 X		   level, but subsequent topics have only leaf topic
 X		   number, padded with blanks to line up with first.
 `indent' \(symbol) -  Convert header prefixes to all white space,
		       except for distinctive bullets.

Defaults:
  FUNCTION:	`allout-insert-listified'
  FROM:		region start, if region active, else start of buffer
  TO:		region end, if region active, else end of buffer
  FROMBUF:	current buffer
  TOBUF:	buffer name derived: \"*current-buffer-name exposed*\"
  FORMAT:	nil"

					; Resolve arguments,
					; defaulting if necessary:
  (if (not func) (setq func 'allout-insert-listified))
  (if (not (and from to))
      (if (my-region-active-p)
	  (setq from (region-beginning) to (region-end))
	(setq from (point-min) to (point-max))))
  (if frombuf
      (if (not (bufferp frombuf))
	  ;; Specified but not a buffer - get it:
	  (let ((got (get-buffer frombuf)))
	    (if (not got)
		(error (concat "allout-process-exposed: source buffer "
			       frombuf
			       " not found."))
	      (setq frombuf got))))
    ;; not specified - default it:
    (setq frombuf (current-buffer)))
  (if tobuf
      (if (not (bufferp tobuf))
	  (setq tobuf (get-buffer-create tobuf)))
    ;; not specified - default it:
    (setq tobuf (concat "*" (buffer-name frombuf) " exposed*")))
  (if (listp format)
      (nreverse format))

  (let* ((listified
	  (progn (set-buffer frombuf)
		 (allout-listify-exposed from to format))))
    (set-buffer tobuf)
    (mapcar func listified)
    (pop-to-buffer tobuf)))

;;;_  - Copy exposed
;;;_   > allout-insert-listified (listified)
(defun allout-insert-listified (listified)
  "Insert contents of listified outline portion in current buffer.

LISTIFIED is a list representing each topic header and body:

 \`(depth prefix text)'

or \`(depth prefix text bullet-plus)'

If `bullet-plus' is specified, it is inserted just after the entire prefix."
  (setq listified (cdr listified))
  (let ((prefix (prog1
		    (car listified)
		  (setq listified (cdr listified))))
	(text (prog1
		  (car listified)
		(setq listified (cdr listified))))
	(bullet-plus (car listified)))
    (insert prefix)
    (if bullet-plus (insert (concat " " bullet-plus)))
    (while text
      (insert (car text))
      (if (setq text (cdr text))
	  (insert-string "\n")))
    (insert "\n")))
;;;_   > allout-copy-exposed-to-buffer (&optional arg tobuf format)
(defun allout-copy-exposed-to-buffer (&optional arg tobuf format)
  "Duplicate exposed portions of current outline to another buffer.

Other buffer has current buffers name with \" exposed\" appended to it.

With repeat count, copy the exposed parts of only the current topic.

Optional second arg TOBUF is target buffer name.

Optional third arg FORMAT, if non-nil, symbolically designates an
alternate presentation format for the outline:

 `flat'   - Convert topic header prefixes to numeric
	    section.subsection... identifiers.
 `indent' - Convert header prefixes to all white space, except for
	    distinctive bullets.
 `indent-flat' - The best of both - only the first of each level has
		 the full path, the rest have only the section number
		 of the leaf, preceded by the right amount of indentation."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf (get-buffer-create (concat "*" (buffer-name) " exposed*"))))
  (let* ((start-pt (point))
	 (beg (if arg (allout-back-to-current-heading) (point-min)))
	 (end (if arg (allout-end-of-current-subtree) (point-max)))
	 (buf (current-buffer))
	 (start-list ()))
    (if (eq format 'flat)
	(setq format (if arg (save-excursion
				   (goto-char beg)
				   (allout-topic-flat-index))
			   '(1))))
    (save-excursion (set-buffer tobuf)(erase-buffer))
    (allout-process-exposed 'allout-insert-listified
			     beg
			     end
			     (current-buffer)
			     tobuf
			     format start-list)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))
;;;_   > allout-flatten-exposed-to-buffer (&optional arg tobuf)
(defun allout-flatten-exposed-to-buffer (&optional arg tobuf)
  "Present numeric outline of outline's exposed portions in another buffer.

The resulting outline is not compatible with outline mode - use
`allout-copy-exposed-to-buffer' if you want that.

Use `allout-indented-exposed-to-buffer' for indented presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffer's name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (allout-copy-exposed-to-buffer arg tobuf 'flat))
;;;_   > allout-indented-exposed-to-buffer (&optional arg tobuf)
(defun allout-indented-exposed-to-buffer (&optional arg tobuf)
  "Present indented outline of outline's exposed portions in another buffer.

The resulting outline is not compatible with outline mode - use
`allout-copy-exposed-to-buffer' if you want that.

Use `allout-flatten-exposed-to-buffer' for numeric sectional presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffer's name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (allout-copy-exposed-to-buffer arg tobuf 'indent))

;;;_  - LaTeX formatting
;;;_   > allout-latex-verb-quote (string &optional flow)
(defun allout-latex-verb-quote (string &optional flow)
  "Return copy of STRING for literal reproduction across LaTeX processing.
Expresses the original characters \(including carriage returns) of the
string across LaTeX processing."
  (mapconcat (function
	      (lambda (char)
		(cond ((memq char '(?\\ ?$ ?% ?# ?& ?{ ?} ?_ ?^ ?- ?*))
		       (concat "\\char" (number-to-string char) "{}"))
		      ((= char ?\n) "\\\\")
		      (t (char-to-string char)))))
	     string
	     ""))
;;;_   > allout-latex-verbatim-quote-curr-line ()
(defun allout-latex-verbatim-quote-curr-line ()
  "Express line for exact \(literal) representation across LaTeX processing.

Adjust line contents so it is unaltered \(from the original line)
across LaTeX processing, within the context of a `verbatim'
environment.  Leaves point at the end of the line."
  (beginning-of-line)
  (let ((beg (point))
	(end (progn (end-of-line)(point))))
    (goto-char beg)
    (while (re-search-forward "\\\\"
	    ;;"\\\\\\|\\{\\|\\}\\|\\_\\|\\$\\|\\\"\\|\\&\\|\\^\\|\\-\\|\\*\\|#"
			      end	; bounded by end-of-line
			      1)	; no matches, move to end & return nil
      (goto-char (match-beginning 0))
      (insert "\\")
      (setq end (1+ end))
      (goto-char (1+ (match-end 0))))))
;;;_   > allout-insert-latex-header (buffer)
(defun allout-insert-latex-header (buffer)
  "Insert initial LaTeX commands at point in BUFFER."
  ;; Much of this is being derived from the stuff in appendix of E in
  ;; the TeXBook, pg 421.
  (set-buffer buffer)
  (let ((doc-style (format "\n\\documentstyle{%s}\n"
			   "report"))
	(page-numbering (if allout-number-pages
			    "\\pagestyle{empty}\n"
			  ""))
	(linesdef (concat "\\def\\beginlines{"
			  "\\par\\begingroup\\nobreak\\medskip"
			  "\\parindent=0pt\n"
			  " \\kern1pt\\nobreak \\obeylines \\obeyspaces "
			  "\\everypar{\\strut}}\n"
			  "\\def\\endlines{"
			  "\\kern1pt\\endgroup\\medbreak\\noindent}\n"))
	(titlecmd (format "\\newcommand{\\titlecmd}[1]{{%s #1}}\n"
			  allout-title-style))
	(labelcmd (format "\\newcommand{\\labelcmd}[1]{{%s #1}}\n"
			  allout-label-style))
	(headlinecmd (format "\\newcommand{\\headlinecmd}[1]{{%s #1}}\n"
			     allout-head-line-style))
	(bodylinecmd (format "\\newcommand{\\bodylinecmd}[1]{{%s #1}}\n"
			     allout-body-line-style))
	(setlength (format "%s%s%s%s"
			   "\\newlength{\\stepsize}\n"
			   "\\setlength{\\stepsize}{"
			   allout-indent
			   "}\n"))
	(oneheadline (format "%s%s%s%s%s%s%s"
			     "\\newcommand{\\OneHeadLine}[3]{%\n"
			     "\\noindent%\n"
			     "\\hspace*{#2\\stepsize}%\n"
			     "\\labelcmd{#1}\\hspace*{.2cm}"
			     "\\headlinecmd{#3}\\\\["
			     allout-line-skip
			     "]\n}\n"))
	(onebodyline (format "%s%s%s%s%s%s"
			       "\\newcommand{\\OneBodyLine}[2]{%\n"
			       "\\noindent%\n"
			       "\\hspace*{#1\\stepsize}%\n"
			       "\\bodylinecmd{#2}\\\\["
			       allout-line-skip
			       "]\n}\n"))
	(begindoc "\\begin{document}\n\\begin{center}\n")
	(title (format "%s%s%s%s"
		       "\\titlecmd{"
		       (allout-latex-verb-quote (if allout-title
						(condition-case err
						    (eval allout-title)
						  ('error "<unnamed buffer>"))
					      "Unnamed Outline"))
		       "}\n"
		       "\\end{center}\n\n"))
	(hsize "\\hsize = 7.5 true in\n")
	(hoffset "\\hoffset = -1.5 true in\n")
	(vspace "\\vspace{.1cm}\n\n"))
    (insert (concat doc-style
		    page-numbering
		    titlecmd
		    labelcmd
		    headlinecmd
		    bodylinecmd
		    setlength
		    oneheadline
		    onebodyline
		    begindoc
		    title
		    hsize
		    hoffset
		    vspace)
	    )))
;;;_   > allout-insert-latex-trailer (buffer)
(defun allout-insert-latex-trailer (buffer)
  "Insert concluding LaTeX commands at point in BUFFER."
  (set-buffer buffer)
  (insert "\n\\end{document}\n"))
;;;_   > allout-latexify-one-item (depth prefix bullet text)
(defun allout-latexify-one-item (depth prefix bullet text)
  "Insert LaTeX commands for formatting one outline item.

Args are the topics numeric DEPTH, the header PREFIX lead string, the
BULLET string, and a list of TEXT strings for the body."
  (let* ((head-line (if text (car text)))
	 (body-lines (cdr text))
	 (curr-line)
	 body-content bop)
					; Do the head line:
    (insert (concat "\\OneHeadLine{\\verb\1 "
                    (allout-latex-verb-quote bullet)
                    "\1}{"
                    depth
                    "}{\\verb\1 "
                    (if head-line
                        (allout-latex-verb-quote head-line)
                      "")
                    "\1}\n"))
    (if (not body-lines)
	nil
      ;;(insert "\\beginlines\n")
      (insert "\\begin{verbatim}\n")
      (while body-lines
	(setq curr-line (car body-lines))
	(if (and (not body-content)
		 (not (string-match "^\\s-*$" curr-line)))
	    (setq body-content t))
					; Mangle any occurrences of
					; "\end{verbatim}" in text,
					; it's special:
	(if (and body-content
		 (setq bop (string-match "\\end{verbatim}" curr-line)))
	    (setq curr-line (concat (substring curr-line 0 bop)
				    ">"
				    (substring curr-line bop))))
	;;(insert "|" (car body-lines) "|")
	(insert curr-line)
	(allout-latex-verbatim-quote-curr-line)
	(insert "\n")
	(setq body-lines (cdr body-lines)))
      (if body-content
	  (setq body-content nil)
	(forward-char -1)
	(insert "\\ ")
	(forward-char 1))
      ;;(insert "\\endlines\n")
      (insert "\\end{verbatim}\n")
      )))
;;;_   > allout-latexify-exposed (arg &optional tobuf)
(defun allout-latexify-exposed (arg &optional tobuf)
  "Format current topics exposed portions to TOBUF for LaTeX processing.
TOBUF defaults to a buffer named the same as the current buffer, but
with \"*\" prepended and \" latex-formed*\" appended.

With repeat count, copy the exposed portions of entire buffer."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf
	    (get-buffer-create (concat "*" (buffer-name) " latexified*"))))
  (let* ((start-pt (point))
	 (beg (if arg (point-min) (allout-back-to-current-heading)))
	 (end (if arg (point-max) (allout-end-of-current-subtree)))
	 (buf (current-buffer)))
    (set-buffer tobuf)
    (erase-buffer)
    (allout-insert-latex-header tobuf)
    (goto-char (point-max))
    (allout-process-exposed 'allout-latexify-one-item
			     beg
			     end
			     buf
			     tobuf)
    (goto-char (point-max))
    (allout-insert-latex-trailer tobuf)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))

;;;_ #8 Encryption
;;;_  > allout-toggle-current-subtree-encryption (&optional fetch-key)
(defun allout-toggle-current-subtree-encryption (&optional fetch-key)
  "Encrypt clear text or decrypt encoded contents of a topic.

Contents includes body and subtopics.

Currently only GnuPG encryption is supported.

\**NOTE WELL** that the encrypted text must be ascii-armored.  For gnupg
encryption, include the option ``armor'' in your ~/.gnupg/gpg.conf file.

Both symmetric-key and key-pair encryption is implemented.  Symmetric is
the default, use a single \(x4) universal argument for keypair mode.

Encrypted topic's bullet is set to a `~' to signal that the contents of the
topic \(body and subtopics, but not heading) is pending encryption or
encrypted.  An `*' asterisk immediately after the bullet signals that the
body is encrypted, its absence means it's meant to be encrypted but is not
- it's \"disclosed\".  When a file with disclosed topics is saved, the user
prompted for an ok to \(symmetric-key) encrypt the disclosed topics.  NOTE
WELL that you must explicitly \(re)encrypt key-pair encrypted topics if you
want them to continue to be in key-pair mode.

Level-1 topics, with prefix consisting solely of an `*' asterisk, cannot be
encrypted.  If you want to encrypt the contents of a top-level topic, use
\\[allout-shift-in] to increase its depth.

Failed transformation does not change the an entry being encrypted -
instead, the key is re-solicited and the transformation is retried.
\\[keyboard-quit] to abort.

Decryption does symmetric or key-pair key mode depending on how the text
was encrypted.  The encryption key is solicited if not currently available
from the key cache from a recent prior encryption action.

Optional FETCH-KEY universal argument is used for two purposes - to provoke
key-pair instead of symmetric encryption, or to provoke clearing of the key
cache so keys are freshly fetched.

 - Without any universal arguments, then the appropriate key for the is
   obtained from the cache, if available, else from the user.

 - If FETCH-KEY is the result of one universal argument - ie, equal to 4 -
   then key-pair encryption is used.

 - With repeated universal argument - equal to 16 - then the key cache is
   cleared before any encryption transformations, to force prompting of the
   user for the key.

The solicited key is retained for reuse in a buffer-specific cache for some
set period of time \(default, 60 seconds), after which the string is
nulled.  `mailcrypt' provides the key caching functionality.  You can
adjust the key cache timeout by ajdusting the setting of the elisp variable
`mc-passwd-timeout'.

If the file previously had no associated key, or had a different key than
specified, the user is prompted to repeat the new one for corroboration.  A
random string encrypted by the new key is set on the buffer-specific
variable `allout-key-verifier-string', for confirmation of the key when
next obtained, before encrypting or decrypting anything with it.  This
helps avoid mistakenly shifting between keys.

If allout customization var `allout-key-verifier-handling' is non-nil, an
entry for `allout-key-verifier-string' and its value is added to an Emacs
'local variables' section at the end of the file, which is created if
necessary.  That setting is for retention of the key verifier across emacs
sessions.

Similarly, `allout-key-hint-string' stores a user-provided reminder about
their key, and `allout-key-hint-handling' specifies when the hint is
presented, or if key hints are disabled.  If enabled \(see the
`allout-key-hint-handling' docstring for details), the hint string is
stored in the local-variables section of the file, and solicited whenever
the key is changed."

;;; This routine handles allout-specific business, dispatching
;;; encryption-specific business to allout-encrypt-string.

  (interactive "P")
  (save-excursion
    (allout-end-of-prefix t)

    (if (= (allout-recent-depth) 1)
        (error (concat "Cannot encrypt or decrypt level 1 topics -"
                       " shift it in to make it encryptable")))

    (if (and fetch-key
             (not (equal fetch-key '(4))))
        (mc-deactivate-passwd))

    (let* ((allout-buffer (current-buffer))
           ;; Asses location:
           (after-bullet-pos (point))
           (was-encrypted
            (progn (if (= (point-max) after-bullet-pos)
                       (error "no body to encrypt"))
                   (looking-at "\\*")))
           (was-collapsed (if (not (re-search-forward "[\n\r]" nil t))
                              nil
                            (backward-char 1)
                            (looking-at "\r")))
           (subtree-beg (1+ (point)))
           (subtree-end (allout-end-of-subtree))
           (subject-text (buffer-substring-no-properties subtree-beg
                                                         subtree-end))
           (subtree-end-char (char-after (1- subtree-end)))
           (subtree-trailling-char (char-after subtree-end))
           (place-holder (if (or (string= "" subject-text)
                                 (string= "\n" subject-text))
                             (error "No topic contents to %scrypt"
                                    (if was-encrypted "de" "en"))))
           ;; Assess key parameters:
           (key-type (or
                      ;; detect the type by which it is already encrypted
                      (and was-encrypted
                           (allout-encrypted-text-type subject-text))
                      (and (member fetch-key '(4 (4)))
                           (yes-or-no-p "Use key-pair encryption instead? ")
                           'keypair)
                      'symmetric))
           (fetch-key (and fetch-key (not (member fetch-key '(16 (16))))))
           result-text)

      (setq result-text
            (allout-encrypt-string subject-text was-encrypted
                                    (current-buffer) key-type fetch-key))

       ;; Replace the subtree with the processed product.
      (allout-unprotected
       (progn
         (set-buffer allout-buffer)
         (delete-region subtree-beg subtree-end)
         (insert result-text)
         (if was-collapsed
             (allout-flag-region subtree-beg (1- (point)) ?\r))
         ;; adjust trailling-blank-lines to preserve topic spacing:
         (if (not was-encrypted)
             (if (and (member subtree-end-char '(?\r ?\n))
                      (member subtree-trailling-char '(?\r ?\n)))
                 (insert subtree-trailling-char)))
         ;; Ensure that the item has an encrypted-entry bullet:
         (if (not (string= (buffer-substring-no-properties
                            (1- after-bullet-pos) after-bullet-pos)
                           allout-topic-encryption-bullet))
             (progn (goto-char (1- after-bullet-pos))
                    (delete-char 1)
                    (insert allout-topic-encryption-bullet)))
         (if was-encrypted
             ;; Remove the is-encrypted bullet qualifier:
             (progn (goto-char after-bullet-pos)
                    (delete-char 1))
           ;; Add the is-encrypted bullet qualifier:
           (goto-char after-bullet-pos)
           (insert "*"))
         )
       )
      )
    )
  )
;;;_  > allout-encrypt-string (text decrypt allout-buffer key-type rekey
;;;                                  &optional retried verifying)
(defun allout-encrypt-string (text decrypt allout-buffer key-type rekey
                                    &optional retried verifying)
  "Encrypt or decrypt a string TEXT using KEY.

If optional DECRYPT is true (default false), then decrypt instead of
encrypt.

Optional REKEY (default false) provokes clearing of the key cache to force
fresh prompting for the key.

Optional RETRIED is for internal use - conveys the number of failed keys have
been solicited in sequence leading to this current call.

Optional VERIFYING is for internal use, signifying processing of text
solely for verification of the cached key.

Returns the resulting string, or nil if the transformation fails."

  ;; Ensure that we have an alternate handle on the real mc-activate-passwd:
  (if (not (fboundp 'real-mc-activate-passwd))
      ;; Force loads of the primary mailcrypt packages, so flet below holds.
      (progn (require 'mailcrypt)
             (load "mc-toplev")
             (fset 'real-mc-activate-passwd
                   (symbol-function 'mc-activate-passwd))))

  (if (and rekey (not verifying)) (mc-deactivate-passwd))

  (catch 'encryption-failed
    (save-excursion

      (let* ((mc-default-scheme (or allout-encryption-scheme
                                    allout-default-encryption-scheme))
             (id (format "%s-%s" key-type
                         (or (buffer-file-name allout-buffer)
                             (buffer-name allout-buffer))))
             (cached (real-mc-activate-passwd id nil))
             (comment "Processed by allout driving mailcrypt")
             key work-buffer result result-text encryption-process-status)

        (unwind-protect

            ;; Interject our mc-activate-passwd wrapper:
            (flet ((mc-activate-passwd (id &optional prompt)
                                       (allout-mc-activate-passwd id prompt)))

              (setq work-buffer
                    (set-buffer (allout-encryption-produce-work-buffer text)))

              (cond

               ;; symmetric:
               ((equal key-type 'symmetric)
                (setq key (if verifying
                              (real-mc-activate-passwd id nil)
                            (allout-mc-activate-passwd id)))
                (setq encryption-process-status
                      (crypt-encrypt-buffer key decrypt))
                (if (zerop encryption-process-status)
                    t
                  (if verifying
                      (throw 'encryption-failed nil)
                    (mc-deactivate-passwd)
                    (error "Symmetric-key encryption failed (%s) - wrong key?"
                           encryption-process-status))))

               ;; encrypt 'keypair:
               ((not decrypt)
                (condition-case result
                    (mailcrypt-encrypt 1)
                  (error (mc-deactivate-passwd)
                         (error "encryption failed: %s"
                                (cadr result)))))

               ;; decrypt 'keypair:
               (t (condition-case result
                      (mc-decrypt)
                    (error (mc-deactivate-passwd)
                           (error "decryption failed: %s"
                                  (cadr result))))))

              (setq result-text (if (or (equal key-type 'keypair)
                                        (not decrypt))
                                    (buffer-substring 1 (1- (point-max)))
                                  (buffer-string)))
              ;; validate result - non-empty
              (cond ((not result-text)
                     (if verifying
                         nil
                       ;; Transformation was fruitless - retry with new key.
                       (mc-deactivate-passwd)
                       (allout-encrypt-string text allout-buffer decrypt nil
                                               (if retried (1+ retried) 1)
                                               verifying)))

                    ;; Barf if encryption yields extraordinary control chars:
                    ((and (not decrypt)
                          (string-match "[\C-a\C-k\C-o-\C-z\C-@]" result-text))
                     (error (concat "encryption produced unusable"
                                    " non-armored text - reconfigure!")))

                    ;; valid result and just verifying or non-symmetric:
                    ((or verifying (not (equal key-type 'symmetric)))
                     result-text)

                    ;; valid result and regular symmetric - situate validator:
                    (t
                     ;; valid result and verifier needs to be situated in
                     ;; allout-buffer:
                     (set-buffer allout-buffer)
                     (if (and (or rekey (not cached))
                              (not (allout-verify-key key allout-buffer)))
                         (allout-situate-encryption-key-verifier key id))
                     result-text)
                    )
              )

          ;; unwind-protect emergence:
          (if work-buffer
              (kill-buffer work-buffer))
          )
        )
      )
    )
  )
;;;_  > allout-mc-activate-passwd (id &optional prompt)
(defun allout-mc-activate-passwd (id &optional prompt)
  "Substituted for mc-activate-passwd during allout outline encryption.

We add key-verification to vanilla mc-activate-passwd.

We depend in some cases on values of the following allout-encrypt-string
internal or prevailing variables:
  - key-type - 'symmetric or 'keypair
  - id - id associated with current key in key cache
  - allout-buffer - where subject text resides
  - retried - number of current attempts to obtain this key
  - rekey - user asked to present a new key - needs to be confirmed"

;;  - if we're doing non-symmetric key, just do normal mc-activate-passwd
;;  - otherwise, if we are have a cached version of the key, then assume
;;    it's verified and return it
;;  - otherwise, prompt for a key, and:
;;    - if we have a key verifier \(a string value which should decrypt
;;      against a symmetric key), validate against the verifier
;;      - if successful, return the verified key
;;      - if unsuccessful:
;;        - offer to use the new key
;;          - if accepted, do confirm process
;;          - if refused, try again until we get a correctly spelled one or the
;;            user quits
;;    - if no key verifier, resolicit the key to get corroboration and return
;;      the corroborated key if spelled identically, or error if not.

  (if (not (equal key-type 'symmetric))
      ;; do regular mc-activate-passwd on non-symmetric key
      (real-mc-activate-passwd id prompt)

    ;; Symmetric hereon:

    (save-excursion
      (set-buffer allout-buffer)
      (let* ((hint (if (and (not (string= allout-key-hint-string ""))
                            (or (equal allout-key-hint-handling 'always)
                                (and (equal allout-key-hint-handling 'needed)
                                     retried)))
                       (format " [%s]" allout-key-hint-string)
                     ""))
             (retry-message (if retried (format " (%s retry)" retried) ""))
             (prompt-sans-hint (format "'%s' symmetric key%s: "
                                       (buffer-name allout-buffer)
                                       retry-message))
             (full-prompt (format "'%s' symmetric key%s%s: "
                                  (buffer-name allout-buffer)
                                  hint retry-message))
             (prompt full-prompt)
             (verifier-string (allout-get-encryption-key-verifier))
             ;; force retention of cached passwords for five minutes while
             ;; we're in this particular routine:
             (mc-passwd-timeout 300)
             (cached (real-mc-activate-passwd id nil))
             (got (or cached (real-mc-activate-passwd id full-prompt)))
             confirmation)

        (if (not got)
            nil

          ;; Duplicate our handle on the key so it's not clobbered by
          ;; deactivate-passwd memory clearing:
          (setq got (format "%s" got))

          (cond (verifier-string
                 (if (and (not (allout-encrypt-string
                                verifier-string 'decrypt allout-buffer
                                'symmetric nil 0 'verifying))
                          (if (yes-or-no-p
                               (concat "Key differs from established"
                                       " - use new one instead? "))
                              ;; deactivate password for subsequent
                              ;; confirmation:
                              (progn (mc-deactivate-passwd)
                                     (setq prompt prompt-sans-hint)
                                     nil)
                            t))
                     (progn (mc-deactivate-passwd)
                            (error "Wrong key."))))
                ;; Force confirmation by repetition for new key:
                ((or rekey (not cached)) (mc-deactivate-passwd))))
        ;; we have a key and it's either verified and cached.
        ;; confirmation vs new input - doing mc-activate-passwd will do the
        ;; right thing, in either case:
        (setq confirmation
              (real-mc-activate-passwd id (concat prompt
                                                  " ... confirm spelling: ")))
        (prog1
            (if (equal got confirmation)
                confirmation
              (if (yes-or-no-p (concat "spelling of original and"
                                       " confirmation differ - retry? "))
                  (progn (setq retried (if retried (1+ retried) 1))
                         (mc-deactivate-passwd)
                         ;; recurse to this routine:
                         (mc-activate-passwd id prompt-sans-hint))
                (mc-deactivate-passwd)
                (error "Confirmation failed.")))
          ;; reduce opportunity for memory cherry-picking by zeroing duplicate:
          (dotimes (i (length got))
            (aset got i 0))
          )
        )
      )
    )
  )
;;;_  > allout-encryption-produce-work-buffer (text)
(defun allout-encryption-produce-work-buffer (text)
  "Establish a new buffer filled with TEXT, for outline encrypion processing.

TEXT is massaged so outline collapsing, if any, is removed."
  (let ((work-buffer (generate-new-buffer " *allout encryption*")))
    (save-excursion
      (set-buffer work-buffer)
      (insert (subst-char-in-string ?\r ?\n text)))
    work-buffer))
;;;_  > allout-encrypted-topic-p ()
(defun allout-encrypted-topic-p ()
  "True if the current topic is encryptable and encrypted."
  (save-excursion
    (allout-end-of-prefix t)
    (and (string= (buffer-substring-no-properties (1- (point)) (point))
                  allout-topic-encryption-bullet)
         (looking-at "\\*"))
    )
  )
;;;_  > allout-encrypted-text-type (text)
;;; XXX gpg-specific, not generic!
(defun allout-encrypted-text-type (text)
  "For gpg encrypted text, return 'symmetric or 'keypair."

  ;; Ensure mc-gpg-path has a value:
  (if (not (boundp 'mc-gpg-path))
      (load-library "mc-gpg"))

  (save-excursion
    (let* ((work-buffer (set-buffer
                         (allout-encryption-produce-work-buffer text)))
           (result (mc-gpg-process-region (point-min) (point-max)
                                          nil mc-gpg-path
                                          '("--batch" "--decrypt")
                                          'mc-gpg-decrypt-parser
                                          work-buffer nil)))
      (cond ((equal (nth 0 result) 'symmetric)
             'symmetric)
            ((equal (nth 0 result) t)
             'keypair)
            (t (error "Unrecognized/unsupported encryption type %S"
                      (nth 0 result))))
      )
    )
  )
;;;_  > allout-create-encryption-key-verifier (key id)
(defun allout-create-encryption-key-verifier (key id)
  "Encrypt a random message for later validation of symmetric key."
  ;; use 20 random ascii characters, across the entire ascii range.
  (random t)
  (let ((spew (make-string 20 ?\0)))
    (dotimes (i (length spew))
      (aset spew i (1+ (random 254))))
    (allout-encrypt-string spew nil nil 'symmetric nil nil t))
  )
;;;_  > allout-situate-encryption-key-verifier (key id)
(defun allout-situate-encryption-key-verifier (key id)
  "Establish key verifier string on file variable.

We also prompt for and situate a new reminder, if reminders are enabled.

We massage the string to simplify programmatic adjustment.  File variable
is `allout-file-key-verifier-string'."
  (let ((verifier-string
         ;; Collapse to a single line and enclose in string quotes:
         (subst-char-in-string ?\n ?\C-a
                               (allout-create-encryption-key-verifier
                                key id)))
        (reminder (if (not (equal allout-key-hint-handling 'disabled))
                      (read-from-minibuffer
                       "Key hint to jog your memory next time: "
                       allout-key-hint-string))))
    (setq allout-key-verifier-string verifier-string)
    (allout-adjust-file-variable "allout-key-verifier-string"
                                  verifier-string)
    (cond ((equal allout-key-hint-handling 'disabled)
           nil)
          ((not (string= reminder allout-key-hint-string))
           (setq allout-key-hint-string reminder)
           (allout-adjust-file-variable "allout-key-hint-string"
                                         reminder)))
    )
  )
;;;_  > allout-get-encryption-key-verifier ()
(defun allout-get-encryption-key-verifier ()
  "Return the text of the encrypt key verifier, unmassaged, or nil if none.

Derived from value of `allout-file-key-verifier-string'."

  (let ((verifier-string (and (boundp 'allout-key-verifier-string)
                              allout-key-verifier-string)))
    (if verifier-string
        ;; Return it uncollapsed
        (subst-char-in-string ?\C-a ?\n verifier-string)
      nil)
   )
  )
;;;_  > allout-verify-key (key)
(defun allout-verify-key (key allout-buffer)
  "True if key successfully decrypts key verifier, nil otherwise.

\"Otherwise\" includes absence of key verifier."
  (save-excursion
    (set-buffer allout-buffer)
    (and (boundp 'allout-key-verifier-string)
         allout-key-verifier-string
         (allout-encrypt-string (allout-get-encryption-key-verifier)
                                 'decrypt allout-buffer 'symmetric
                                 nil nil 'verifying)
         t)))
;;;_  > allout-next-topic-pending-encryption (&optional except-mark)
(defun allout-next-topic-pending-encryption (&optional except-mark)
  "Return the point of the next topic pending encryption, or nil if none.

EXCEPT-MARK identifies a point whose containing topics should be excluded
from encryption.  This supports 'except-current mode of
`allout-encrypt-unencrypted-on-saves'.

Such a topic has the allout-topic-encryption-bullet without an
immediately following '*' that would mark the topic as being encrypted.  It
must also have content."
  (let (done got content-beg)
    (while (not done)

      (if (not (re-search-forward
                (format "\\(\\`\\|[\n\r]\\)%s *%s[^*]"
                        (regexp-quote allout-header-prefix)
                        (regexp-quote allout-topic-encryption-bullet))
                nil t))
          (setq got nil
                done t)
        (goto-char (setq got (match-beginning 0)))
        (if (looking-at "[\n\r]")
            (forward-char 1))
        (setq got (point)))

      (cond ((not got)
             (setq done t))

            ((not (re-search-forward "[\n\r]"))
             (setq got nil
                   done t))

            ((eobp)
             (setq got nil
                   done t))

            (t
             (setq content-beg (point))
             (backward-char 1)
             (allout-end-of-subtree)
             (if (or (<= (point) content-beg)
                     (and except-mark
                          (<= content-beg except-mark)
                          (>= (point) except-mark)))
                 ;; Continue looking
                 (setq got nil)
               ;; Got it!
               (setq done t)))
            )
      )
    (if got
        (goto-char got))
    )
  )
;;;_  > allout-encrypt-decrypted (&optional except-mark)
(defun allout-encrypt-decrypted (&optional except-mark)
  "Encrypt topics pending encryption except those containing exemption point.

EXCEPT-MARK identifies a point whose containing topics should be excluded
from encryption.  This supports 'except-current mode of
`allout-encrypt-unencrypted-on-saves'.

If a topic that is currently being edited was encrypted, we return a list
containing the location of the topic and the location of the cursor just
before the topic was encrypted.  This can be used, eg, to decrypt the topic
and exactly resituate the cursor if this is being done as part of a file
save.  See `allout-encrypt-unencrypted-on-saves' for more info."

  (interactive "p")
  (save-excursion
    (let ((current-mark (point-marker))
          was-modified
          bo-subtree
          editing-topic editing-point)
      (goto-char (point-min))
      (while (allout-next-topic-pending-encryption except-mark)
        (setq was-modified (buffer-modified-p))
        (if (save-excursion
              (and (boundp 'allout-encrypt-unencrypted-on-saves)
                   allout-encrypt-unencrypted-on-saves
                   (setq bo-subtree (re-search-forward "[\n\r]"))
                   ;; Not collapsed:
                   (string= (match-string 0) "\n")
                   (>= current-mark (point))
                   (allout-end-of-current-subtree)
                   (<= current-mark (point))))
            (setq editing-topic (point)
                  ;; we had to wait for this 'til now so prior topics are
                  ;; encrypted, any relevant text shifts are in place:
                  editing-point (marker-position current-mark)))
        (allout-toggle-current-subtree-encryption)
        (if (not was-modified)
            (set-buffer-modified-p nil))
        )
      (if (not was-modified)
         (set-buffer-modified-p nil))
      (if editing-topic (list editing-topic editing-point))
      )
    )
  )

;;;_ #9 miscellaneous
;;;_  > allout-mark-topic ()
(defun allout-mark-topic ()
  "Put the region around topic currently containing point."
  (interactive)
  (beginning-of-line)
  (allout-goto-prefix)
  (push-mark (point))
  (allout-end-of-current-subtree)
  (exchange-point-and-mark))
;;;_  > outlineify-sticky ()
;; outlinify-sticky is correct spelling; provide this alias for sticklers:
(defalias 'outlinify-sticky 'outlineify-sticky)
(defun outlineify-sticky (&optional arg)
  "Activate outline mode and establish file var so it is started subsequently.

See doc-string for `allout-layout' and `allout-init' for details on
setup for auto-startup."

  (interactive "P")

  (allout-mode t)

  (save-excursion
    (goto-char (point-min))
    (if (looking-at allout-regexp)
	t
      (allout-open-topic 2)
      (insert (concat "Dummy outline topic header - see"
                      "`allout-mode' docstring: `^Hm'."))
      (allout-adjust-file-variable
       "allout-layout" (format "%s" (or allout-layout '(-1 : 0)))))))
;;;_  > allout-file-vars-section-data ()
(defun allout-file-vars-section-data ()
  "Return data identifying the file-vars section, or nil if none.

Returns list `(beginning-point prefix-string suffix-string)'."
  ;; minimally gleaned from emacs 21.4 files.el hack-local-variables function.
  (let (beg prefix suffix)
    (save-excursion
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
      (if (let ((case-fold-search t))
	    (not (search-forward "Local Variables:" nil t)))
          nil
        (setq beg (- (point) 16))
        (setq suffix (buffer-substring-no-properties
                      (point)
                      (progn (if (re-search-forward "[\n\r]" nil t)
                                 (forward-char -1))
                             (point))))
        (setq prefix (buffer-substring-no-properties
                      (progn (if (re-search-backward "[\n\r]" nil t)
                                 (forward-char 1))
                             (point))
                      beg))
        (list beg prefix suffix))
      )
    )
  )
;;;_  > allout-adjust-file-variable (varname value)
(defun allout-adjust-file-variable (varname value)
  "Adjust the setting of an emacs file variable named VARNAME to VALUE.

This activity is inhibited if either `enable-local-variables'
`allout-enable-file-variable-adjustment' are nil.

When enabled, an entry for the variable is created if not already present,
or changed if established with a different value.  The section for the file
variables, itself, is created if not already present.  When created, the
section lines \(including the section line) exist as second-level topics in
a top-level topic at the end of the file.

enable-local-variables must be true for any of this to happen."
  (if (not (and enable-local-variables
                allout-enable-file-variable-adjustment))
      nil
    (save-excursion
      (let ((section-data (allout-file-vars-section-data))
            beg prefix suffix)
        (if section-data
            (setq beg (car section-data)
                  prefix (cadr section-data)
                  suffix (car (cddr section-data)))
          ;; create the section
          (goto-char (point-max))
          (open-line 1)
          (allout-open-topic 0)
          (end-of-line)
          (insert "Local emacs vars.\n")
          (allout-open-topic 1)
          (setq beg (point)
                suffix ""
                prefix (buffer-substring-no-properties (progn
                                                         (beginning-of-line)
                                                         (point))
                                                       beg))
          (goto-char beg)
          (insert "Local variables:\n")
          (allout-open-topic 0)
          (insert "End:\n")
          )
        ;; look for existing entry or create one, leaving point for insertion
        ;; of new value:
        (goto-char beg)
        (allout-show-to-offshoot)
        (if (search-forward (concat "\n" prefix varname ":") nil t)
            (let* ((value-beg (point))
                   (line-end (progn (if (re-search-forward "[\n\r]" nil t)
                                        (forward-char -1))
                                    (point)))
                   (value-end (- line-end (length suffix))))
              (if (> value-end value-beg)
                  (delete-region value-beg value-end)))
          (end-of-line)
          (open-line 1)
          (forward-line 1)
          (insert (concat prefix varname ":")))
        (insert (format " %S%s" value suffix))
        )
      )
    )
  )
;;;_  > solicit-char-in-string (prompt string &optional do-defaulting)
(defun solicit-char-in-string (prompt string &optional do-defaulting)
  "Solicit (with first arg PROMPT) choice of a character from string STRING.

Optional arg DO-DEFAULTING indicates to accept empty input (CR)."

  (let ((new-prompt prompt)
        got)

    (while (not got)
      (message "%s" new-prompt)

      ;; We do our own reading here, so we can circumvent, eg, special
      ;; treatment for `?' character.  (Oughta use minibuffer keymap instead.)
      (setq got
            (char-to-string (let ((cursor-in-echo-area nil)) (read-char))))

      (setq got
	    (cond ((string-match (regexp-quote got) string) got)
		  ((and do-defaulting (string= got "\r"))
		   ;; Return empty string to default:
		   "")
		  ((string= got "\C-g") (signal 'quit nil))
		  (t
		   (setq new-prompt (concat prompt
					    got
					    " ...pick from: "
					    string
					    ""))
		   nil))))
      ;; got something out of loop - return it:
      got)
  )
;;;_  > regexp-sans-escapes (string)
(defun regexp-sans-escapes (regexp &optional successive-backslashes)
  "Return a copy of REGEXP with all character escapes stripped out.

Representations of actual backslashes - '\\\\\\\\' - are left as a
single backslash.

Optional arg SUCCESSIVE-BACKSLASHES is used internally for recursion."

  (if (string= regexp "")
      ""
    ;; Set successive-backslashes to number if current char is
    ;; backslash, or else to nil:
    (setq successive-backslashes
	  (if (= (aref regexp 0) ?\\)
	      (if successive-backslashes (1+ successive-backslashes) 1)
	    nil))
    (if (or (not successive-backslashes) (= 2 successive-backslashes))
	;; Include first char:
	(concat (substring regexp 0 1)
		(regexp-sans-escapes (substring regexp 1)))
      ;; Exclude first char, but maintain count:
      (regexp-sans-escapes (substring regexp 1) successive-backslashes))))
;;;_  - add-hook definition for divergent emacsen
;;;_   > add-hook (hook function &optional append)
(if (not (fboundp 'add-hook))
    (defun add-hook (hook function &optional append)
      "Add to the value of HOOK the function FUNCTION unless already present.
\(It becomes the first hook on the list unless optional APPEND is non-nil, in
which case it becomes the last).  HOOK should be a symbol, and FUNCTION may be
any valid function.  HOOK's value should be a list of functions, not a single
function.  If HOOK is void, it is first set to nil."
      (or (boundp hook) (set hook nil))
      (or (if (consp function)
	      ;; Clever way to tell whether a given lambda-expression
	      ;; is equal to anything in the hook.
	      (let ((tail (assoc (cdr function) (symbol-value hook))))
		(equal function tail))
	    (memq function (symbol-value hook)))
	  (set hook
	       (if append
		   (nconc (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))))
;;;_  > subst-char-in-string if necessary
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
            (newstr (if inplace string (copy-sequence string))))
        (while (> i 0)
          (setq i (1- i))
          (if (eq (aref newstr i) fromchar)
              (aset newstr i tochar)))
        newstr)))

;;;_  : my-mark-marker to accommodate divergent emacsen:
(defun my-mark-marker (&optional force buffer)
  "Accommodate the different signature for `mark-marker' across Emacsen.

XEmacs takes two optional args, while mainline GNU Emacs does not,
so pass them along when appropriate."
  (if (string-match " XEmacs " emacs-version)
      (mark-marker force buffer)
    (mark-marker)))

;;;_ #10 Under development
;;;_  > allout-bullet-isearch (&optional bullet)
(defun allout-bullet-isearch (&optional bullet)
  "Isearch \(regexp) for topic with bullet BULLET."
  (interactive)
  (if (not bullet)
      (setq bullet (solicit-char-in-string
		    "ISearch for topic with bullet: "
		    (regexp-sans-escapes allout-bullets-string))))

  (let ((isearch-regexp t)
	(isearch-string (concat "^"
				allout-header-prefix
				"[ \t]*"
				bullet)))
    (isearch-repeat 'forward)
    (isearch-mode t)))
;;;_  ? Re hooking up with isearch - use isearch-op-fun rather than
;;;	wrapping the isearch functions.

;;;_* Local emacs vars.
;;; The following `allout-layout' local variable setting:
;;;  - closes all topics from the first topic to just before the third-to-last,
;;;  - shows the children of the third to last (config vars)
;;;  - and the second to last (code section),
;;;  - and closes the last topic (this local-variables section).
;;;Local variables:
;;;allout-layout: (0 : -1 -1 0)
;;;End:

;;; arch-tag: cf38fbc3-c044-450f-8bff-afed8ba5681c
;;; allout.el ends here
